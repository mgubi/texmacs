
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : source-files-test.scm
;; DESCRIPTION : source files are saved as they were loaded
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A source file (C++, Python, Scheme...) is loaded as a document of lines
;; and saved from it. Its bytes are kept: tabs (which TeXmacs expanded to
;; spaces) and the other control characters, its line ends (LF, CR LF, CR),
;; whether it ends by a newline, its encoding (UTF-8, or ISO-8859-1 when it
;; is not valid UTF-8), the characters which Cork writes otherwise (a
;; backquote, three dots).

(texmacs-module (check source-files-test)
  (:use (check check-lib)))

(define (bytes->string . l)
  (list->string (map integer->char l)))

(define tab (bytes->string 9))
(define cr (bytes->string 13))

(define dir (url-append (url-temp-dir) "source-files-test"))

(define (round-trip name text)
  ;; @text saved as the file @name, loaded in a buffer, which is saved as
  ;; another file: the text of that file
  (let* ((u (url-append dir name))
         (v (url-append dir (string-append "out-" name)))
         (fm (url-format u)))
    (string-save text u)
    (when (buffer-exists? u) (buffer-close u))
    (buffer-load u)
    (buffer-export u v fm)
    (buffer-close u)
    (string-load v)))

(define (body-of name text)
  ;; the lines of @text, imported as the file @name
  (let* ((u (url-append dir name)))
    (string-save text u)
    (with doc (tree->stree (tree-import u (url-format u)))
      (with b (list-find (cdr doc) (lambda (x) (func? x 'body)))
        (and b (cadr b))))))

(define (test-round-trips)
  (check-group "round trips")
  (for (x (list
           ;; tabs and a form feed
           (string-append "int" tab "x;\n" tab "if (a)" tab "{}\n"
                          (bytes->string 12) "\n")
           ;; CR LF, no newline at the end, CR alone
           (string-append "a" cr "\nb" cr "\n")
           "a\nb"
           (string-append "a" cr "b" cr)
           (string-append "a" cr "\nb\nc" cr "d\n")
           ;; what Cork writes otherwise; beyond U+FFFF; ISO-8859-1
           (string-append "s = `x` + y...;\n" (bytes->string 240 159 152 128)
                          "\n")
           (string-append "caf" (bytes->string 233) "\n")
           (string-append (bytes->string 239 187 191) "x\n")
           ""
           "\n"))
    (check= (round-trip "a.cpp" x) x))
  ;; also as Python and as Scheme
  (with x (string-append "def f():\n" tab "return 1\n")
    (check= (round-trip "a.py" x) x))
  (with x (string-append "(define (f)\n" tab "1)\n")
    (check= (round-trip "a.scm" x) x))
  ;; shell scripts (#734): the #! line, a tab and the last newline are kept
  (with x (string-append "#!/bin/bash\n# c\nfor f in *.txt; do\n" tab
                         "echo \"$f\" | wc -l\ndone\n")
    (for (name '("a.sh" "a.bash" "a.zsh"))
      (check= (round-trip name x) x))
    (check= (round-trip "b.sh" (substring x 0 (- (string-length x) 1)))
            (substring x 0 (- (string-length x) 1)))))

(define (test-shell-files)
  (check-group "shell scripts")
  ;; a .sh file is a source file of the language shell (#734), as .lua is lua
  (check= (map url-format (map (lambda (n) (url-append dir n))
                               '("a.sh" "a.bash" "a.zsh" "a.lua")))
          '("shell" "shell" "shell" "lua"))
  (in-file "s.sh" "#!/bin/sh\necho hi\n"
    (lambda ()
      (check= (get-init "prog-language") "shell")
      (check= (get-init "mode") "prog")
      (check= (get-style-list) '("code")))))

(define (test-lines)
  (check-group "the lines of a source file")
  ;; a tab is the character <#9> (byte 9 is a macron in Cork)
  (check= (body-of "b.cpp" (string-append tab "x;\n"))
          '(document "<#9>x;" ""))
  ;; a CR in a file whose other lines end by LF stays in its line
  (check= (body-of "c.cpp" (string-append "a" cr "b\nc\n"))
          '(document "a<#D>b" "c" ""))
  ;; the line ends and the encoding are in the initial environment
  (let* ((u (url-append dir "d.cpp")))
    (string-save (string-append "caf" (bytes->string 233) cr "\n") u)
    (with doc (tree->stree (tree-import u "cpp"))
      (with init (list-find (cdr doc) (lambda (x) (func? x 'initial)))
        (check-true (in? '(associate "source-line-ends" "dos")
                         (cdadr init)))
        (check-true (in? '(associate "source-encoding" "iso-8859-1")
                         (cdadr init)))))))

(define (in-file name text proc)
  ;; @proc called in a buffer of @text, the file @name
  (let* ((u (url-append dir name))
         (old (current-buffer)))
    (string-save text u)
    (when (buffer-exists? u) (buffer-close u))
    (load-buffer u)
    (proc)
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))))

(define (test-indentation)
  (check-group "indentation")
  ;; (loaded here, not with this module: check-master loads all the tests
  ;; in each suite, and the keys of programs must not be there for the
  ;; tests of mathematics, see kbd-menu-test.scm)
  (use-modules (prog prog-edit))
  ;; a file indented by tabs: tabs at stops of 8 columns (the preference)
  (in-file "e.cpp" (string-append "int f () {\n" tab "return 1;\n}\n")
    (lambda ()
      (check= (get-init "source-indent") "tabs")
      (check= (program-tab-width) 8)
      (check-true (program-indent-tabs?))
      (check= (string-get-indent "<#9>  x") 10)
      (check= (string-get-indent "  <#9>x") 8)
      (check= (string-indent-end "<#9>  x") 6)
      (check= (string-column "a<#9>b" 5) 8)
      (check= (string-set-indent "  x" 12) "<#9>    x")
      (check= (string-set-indent "<#9><#9>x" 0) "x")))
  ;; (one level of indentation is a tab there: Return after a line of
  ;; Python which ends by a colon adds one, which needs the mode of the
  ;; cursor, which is not known here: it was checked in a window)
  (in-file "g.py" (string-append "def f():\n" tab "y\n")
    (lambda () (check= (program-indent-step) 8)))
  ;; a file indented by spaces
  (in-file "f.cpp" "int f () {\n    return 1;\n}\n"
    (lambda ()
      (check= (get-init "source-indent") "spaces")
      (check-false (program-indent-tabs?))
      (check= (string-set-indent "<#9>x" 4) "    x"))))

(tm-define (source-files-test-failures)
  (check-suite "source-files")
  (system-mkdir dir)
  (test-round-trips)
  (test-shell-files)
  (test-lines)
  (test-indentation)
  (check-end))
