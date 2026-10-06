
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : source-links.scm
;; DESCRIPTION : links from the documentation to the source files
;; COPYRIGHT   : (C) 2026  The TeXmacs team
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The developer documentation refers to source files with
;;   <source-link|shown text|path[:line]>
;; where path is relative to the src directory of the TeXmacs repository
;; (the directory with src, TeXmacs and plugins).  Clicking the link calls
;; open-source-link, which opens the file with the tool of the preference
;; "developer:source editor": "texmacs" or a command line in which %f is
;; replaced by the file and %l by the line.

(texmacs-module (doc source-links))

(define-preferences
  ("developer:source editor" "texmacs" noop)
  ("developer:source directory" "" noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding the source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (source-links-root)
  (:synopsis "The src directory of the TeXmacs repository, or #f")
  (let* ((pref (get-preference "developer:source directory"))
         (conf (url->system (unix->url "$TEXMACS_SOURCE_PATH"))))
    (cond ((!= pref "") (system->url pref))
          ((and (!= conf "") (url-directory? (system->url conf)))
           (system->url conf))
          (else #f))))

(define (source-link-split s)
  ;; "dir/file.cpp:123" -> ("dir/file.cpp" 123)
  (with i (string-search-backwards ":" (string-length s) s)
    (with n (and (> i 0) (string->number (substring s (+ i 1)
                                                    (string-length s))))
      (if (and n (integer? n) (> n 0))
          (list (substring s 0 i) n)
          (list s #f)))))

(tm-define (source-link-file path)
  (:synopsis "The url of the source file @path, or #f")
  (let* ((root (source-links-root))
         (u1 (and root (url-append root (unix->url path))))
         (u2 (and (string-starts? path "TeXmacs/")
                  (url-append (unix->url "$TEXMACS_PATH")
                              (unix->url (string-drop path 8))))))
    (cond ((and u1 (url-exists? u1)) u1)
          ((and u2 (url-exists? (url-concretize u2))) (url-concretize u2))
          (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening the source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define source-editors
  ;; menu name, command (#f for TeXmacs)
  `(("TeXmacs" "texmacs")
    ("System default" "default")
    ("Visual Studio Code" "code -g %f:%l")
    ("Emacs" "emacsclient -n +%l %f")
    ("Xcode" "xed -l %l %f")
    ("Sublime Text" "subl %f:%l")
    ("Zed" "zed %f:%l")))

(define (shell-quote s)
  (if (or (os-mingw?) (os-win32?))
      (string-append "\"" s "\"")
      (string-append "'" (string-replace s "'" "'\\''") "'")))

(define (source-command template file line)
  (let* ((s1 (string-replace template "%f" (shell-quote file)))
         (s2 (string-replace s1 "%l" (number->string (or line 1)))))
    s2))

(define (open-in-texmacs u line)
  (load-buffer u)
  (when line
    (delayed
      (:idle 100)
      (go-to-line (- line 1)))))

(define (open-in-system u)
  (with s (shell-quote (url->system u))
    (cond ((or (os-mingw?) (os-win32?)) (shell (string-append "start \"\" " s)))
          ((os-macos?) (shell (string-append "open " s)))
          (else (shell (string-append "xdg-open " s " &"))))))

(define (open-in-tool template u line)
  (with cmd (source-command template (url->system u) line)
    (if (or (os-mingw?) (os-win32?))
        (shell (string-append "start \"\" " cmd))
        (shell (string-append cmd " &")))))

(tm-define (open-source-link path)
  (:synopsis "Open the source file @path, optionally ending with :line")
  (:secure #t)
  (let* ((s (if (tree? path) (tree->string path) path))
         (l (source-link-split s))
         (u (source-link-file (car l)))
         (line (cadr l))
         (tool (get-preference "developer:source editor")))
    (cond ((not u)
           (set-message `(concat "Source file " (verbatim ,(car l))
                                 " not found; set the source directory in"
                                 " Developer " (math "\\rightarrow")
                                 " Open source links with")
                        "Open source file"))
          ((== tool "texmacs") (open-in-texmacs u line))
          ((== tool "default") (open-in-system u))
          (else (open-in-tool tool u line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (source-editor-test cmd)
  (== (get-preference "developer:source editor") cmd))

(define (set-source-editor cmd)
  (set-preference "developer:source editor" cmd))

(define (source-editor-custom?)
  (not (assoc-ref (map reverse source-editors)
                  (get-preference "developer:source editor"))))

(tm-define (interactive-source-editor)
  (:interactive #t)
  (interactive (lambda (cmd) (when (!= cmd "") (set-source-editor cmd)))
    (list "Command (%f file, %l line)" "string"
          (get-preference "developer:source editor"))))

(tm-define (interactive-source-directory)
  (:interactive #t)
  (choose-file (lambda (u)
                 (set-preference "developer:source directory"
                                 (url->system u)))
               "Source directory (the src directory of the repository)"
               "directory"))

(menu-bind source-links-menu
  (for (e source-editors)
    ((check (eval (car e)) "v" (source-editor-test (cadr e)))
     (set-source-editor (cadr e))))
  ((check "Other..." "v" (source-editor-custom?))
   (interactive-source-editor))
  ---
  ("Source directory..." (interactive-source-directory))
  (when (!= (get-preference "developer:source directory") "")
    ("Use the configured directory"
     (set-preference "developer:source directory" ""))))
