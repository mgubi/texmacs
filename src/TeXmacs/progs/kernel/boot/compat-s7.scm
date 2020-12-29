
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : compat.scm
;; DESCRIPTION : for compatability
;; COPYRIGHT   : (C) 2003  David Allouche, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot compat-s7))

;;; certain Scheme versions do not define 'filter'
(if (not (defined? 'filter))
    (define-public (filter pred? l)
      (apply append (map (lambda (x) (if (pred? x) (list x) (list))) l))))

;; curried define
(define base-define define)
(define-public-macro (curried-define head . body)
    (if (pair? head)
      `(curried-define ,(car head) (lambda ,(cdr head) ,@body))
      `(base-define ,head ,@body)))

(define-public (noop) (lambda () #f))


