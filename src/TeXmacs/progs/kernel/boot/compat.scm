
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

(texmacs-module (kernel boot compat))

(define cout-port
  (make-soft-port
   (vector (lambda (c) (win32-display (char->string c)))
	   (lambda (s) (win32-display s))
	   (lambda () (noop))
	   (lambda () #\?)
	   (lambda () (noop)))
   "w"))

(if (os-win32?)
    (begin
      (set-current-output-port cout-port)
      (set-current-error-port cout-port)))

;;; FIXME: maybe we can remove this code?
;;; make eval from guile>=1.6.0 backwards compatible
(catch 'wrong-number-of-args
       (lambda () (eval 1))
       (lambda arg
	 (let ((default-eval eval))
	   (set! eval (lambda (form . env)
			(cond ((null? form) (list))
			      ((null? env) (primitive-eval form))
			      (else (default-eval form (car env)))))))))

;;; for old-style initialization files
(define-public (exec-file . args)
  (noop))

;;; certain Guile versions do not define 'filter'
(provide-public (filter pred? l)
   (apply append (map (lambda (x) (if (pred? x) (list x) (list))) l)))

;;; Guile 2/3 print inexact numbers with as many digits as are needed to
;;; read them back exactly (0.1 + 0.2 = 0.30000000000000004), while Guile
;;; 1.8 printed at most 15 significant digits (0.3), which is what the
;;; exports to HTML, LaTeX, etc. expect. number->string keeps the Guile
;;; 1.8 behaviour (display and write of numbers do not).
(cond-expand
  (guile-2
    (let ((guile-number->string number->string))
      (define (exponent a e)
        ;; the exponent of a > 0 in base 10, starting from a guess e
        (cond ((>= a (expt 10 (+ e 1))) (exponent a (+ e 1)))
              ((< a (expt 10 e)) (exponent a (- e 1)))
              (else e)))
      (define (round-15 x)
        (let* ((a (abs (inexact->exact x)))
               (e (exponent a (inexact->exact
                               (floor (/ (log (abs x)) (log 10))))))
               (scale (expt 10 (- 14 e)))
               (m (round (* (inexact->exact x) scale))))
          (exact->inexact (/ m scale))))
      (set! number->string
            (lambda (x . radix)
              (if (and (null? radix) (real? x) (inexact? x)
                       (not (zero? x)) (not (nan? x)) (not (inf? x)))
                  (guile-number->string (round-15 x))
                  (apply guile-number->string x radix))))))
  (else (noop)))

;;; Guile 2/3 do not keep the source of procedures (procedure-source
;;; returns #f). tm-define, tagged-lambda and lazy-define store the source
;;; of the procedures they make in the tm-source property: procedure-source
;;; returns it.
(cond-expand
  (guile-2
    (let ((guile-procedure-source procedure-source))
      (set! procedure-source
            (lambda (p)
              (or (and (procedure? p) (procedure-property p 'tm-source))
                  (guile-procedure-source p))))))
  (else (noop)))
