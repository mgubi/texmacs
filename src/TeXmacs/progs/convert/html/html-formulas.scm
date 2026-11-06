
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : html-formulas.scm
;; DESCRIPTION : how the HTML export writes mathematical formulas
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html html-formulas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The ways of writing formulas, in the order in which the converter looks at
;; their preferences, with the defaults of the converter (see init-html)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define html-formula-preferences
  '(("static" "texmacs->html:static-math" "off")
    ("mathjax" "texmacs->html:mathjax" "off")
    ("mathml" "texmacs->html:mathml" "off")
    ("images" "texmacs->html:images" "on")))

(define (html-formula-on? p)
  ;; any value other than "on" is off for the converter
  (if (cpp-has-preference? (cadr p))
      (== (get-preference (cadr p)) "on")
      (== (caddr p) "on")))

(tm-define (html-formulas)
  (:synopsis "How the HTML export writes formulas")
  (let loop ((l html-formula-preferences))
    (cond ((null? l) "html")
          ((html-formula-on? (car l)) (caar l))
          (else (loop (cdr l))))))

(tm-define (html-formulas? mode)
  (== (html-formulas) mode))

(tm-define (html-formulas-set mode)
  (:synopsis "Choose how the HTML export writes formulas")
  (:check-mark "*" html-formulas?)
  (for-each (lambda (p) (set-boolean-preference (cadr p) (== (car p) mode)))
            html-formula-preferences))
