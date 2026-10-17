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
;; read it back. The languages and their names come from the list of the
;; program languages (kernel/texmacs/tm-program-languages.scm).

(texmacs-module (convert tools code-languages))

;; (tag name other-names...): name is written, all names are read
(define code-languages
  (list-filter
   (map (lambda (lan)
          (let ((tag (program-language-block-tag lan))
                (names (program-language-names lan)))
            (and tag (pair? names) (cons tag names))))
        (program-languages))
   identity))

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
