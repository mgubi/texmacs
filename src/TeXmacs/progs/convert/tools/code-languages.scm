;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-languages.scm
;; DESCRIPTION : the languages of the blocks of code, for the converters
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Markdown and docx/odt converters write the language of a block of
;; code (after the fence of Markdown, as the language of a pre block) and
;; read it back. This table is the one place where the languages are
;; listed: add a language here and both converters know it.

(texmacs-module (convert tools code-languages))

;; (tag name other-names...): name is written, all names are read
(define code-languages
  '((cpp-code "cpp" "c" "c++" "cc" "h" "hpp")
    (python-code "python" "py" "python3")
    (scm-code "scheme" "scm" "lisp" "guile")
    (shell-code "sh" "bash" "shell" "zsh" "console")
    (java-code "java")
    (javascript-code "javascript" "js")
    (json-code "json")
    (julia-code "julia")
    (r-code "r")
    (scala-code "scala")
    (fortran-code "fortran")
    (octave-code "octave" "matlab")
    (scilab-code "scilab")
    (dot-code "dot")
    (mmx-code "mathemagix" "mmx")
    (go-code "go" "golang")
    (lean-code "lean" "lean4")
    (lua-code "lua")
    (rust-code "rust" "rs")))

;; the blocks of code without a language
(define plain-code-tags '(verbatim-code pseudo-code render-code code))

(define-public (code-block-tags)
  ;; the tags of the blocks of code, those with a language first
  (append (map car code-languages) plain-code-tags))

(define-public (code-language-name tag)
  ;; the name of the language of the code tag, "" for code without a
  ;; language, #f when tag is not code
  (let ((l (assoc tag code-languages)))
    (cond (l (cadr l))
          ((memq tag plain-code-tags) "")
          (else #f))))

(define-public (code-language-tag name)
  ;; the code tag of the language name (in lower case), or #f
  (let ((l (list-find code-languages (lambda (l) (member name (cdr l))))))
    (and l (car l))))
