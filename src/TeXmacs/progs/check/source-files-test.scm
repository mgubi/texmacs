
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
    (check= (round-trip "a.scm" x) x)))

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

(tm-define (source-files-test-failures)
  (check-suite "source-files")
  (system-mkdir dir)
  (test-round-trips)
  (test-lines)
  (check-end))
