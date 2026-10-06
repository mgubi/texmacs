
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-markup-examples.scm
;; DESCRIPTION : helpers of the examples of the GUI through markup
;; COPYRIGHT   : (C) 2026  The TeXmacs team
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The examples of the developer documentation (doc/devel/source/gui-markup)
;; load this module with <use-module|(doc gui-markup-examples)>. The commands
;; of the markup are evaluated with secure-eval, which only allows secure
;; functions: these show in the footer what an element did.

(texmacs-module (doc gui-markup-examples))

(define (example-string x)
  (cond ((string? x) x)
        ((tree? x) (example-string (tree->stree x)))
        ((symbol? x) (symbol->string x))
        ((and (pair? x) (== (car x) 'tuple))
         (string-recompose (map example-string (cdr x)) ", "))
        (else (object->string x))))

(tm-define (gui-message . args)
  (:synopsis "Show @args in the footer")
  (:secure #t)
  (set-message (apply string-append (map example-string args))
               "GUI through markup"))

(tm-define (gui-show-event type x y label)
  (:synopsis "Show in the footer the event @type received by a relay box")
  (:secure #t)
  ;; NOTE: #f lets the editor handle the event as usual
  (set-message (string-append (example-string label) ": "
                              (example-string type))
               "GUI through markup")
  #f)
