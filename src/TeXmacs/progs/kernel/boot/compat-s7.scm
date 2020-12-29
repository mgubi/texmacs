
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
      `(,curried-define ,(car head) (lambda ,(cdr head) ,@body))
      `(,base-define ,head ,@body)))
(varlet *texmacs-module* 'define curried-define)

(define-public (noop . l) (if #f #f #f))

(define-public (acons key datum alist) (cons (cons key datum) alist))

(define-public (symbol-append . l)
   (string->symbol (apply string-append (map symbol->string l))))

(define-public (map-in-order . l) (apply map l))

(define-public lazy-catch catch)

(define-public (last-pair lis)
;;  (check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))


(define-public (seed->random-state seed) (random-state seed))


(define-public (copy-tree tree)
  (let loop ((tree tree))
    (if (pair? tree)
        (cons (loop (car tree)) (loop (cdr tree)))
        tree)))


(define-public (assoc-set! l what val)
  (let ((b (assoc what l)))
    (if b (set! (cdr b) val) (set! l (cons (cons what val) l)))
    l))

;;FIXME: assoc-set! is tricky to use, maybe just get rid in the code
(define-public (assoc-set! l what val)
  (let ((b (assoc what l)))
    (if b (set! (cdr b) val) (set! l (cons (cons what val) l)))
    l))

(define-public (assoc-ref l what)
  (let ((b (assoc what l)))
    (if b (cdr b) #f)))

(define-public (sort l op) (sort! (copy l) op))

(define-public (force-output) (flush-output-port *stdout*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME

;getlogin
;string-split
; char-set-adjoin
;make-record-type
;string-index

(define-public (getpid) 1)
(define-public (access? . l) #f)
(define-public R_OK #f)

(define-public (current-time) 100)

(define *default-bound* (- (expt 2 29) 3))

(define (%string-hash s ch-conv bound)
  (let ((hash 31)
    (len (string-length s)))
    (do ((index 0 (+ index 1)))
      ((>= index len) (modulo hash bound))
      (set! hash (modulo (+ (* 37 hash)
                (char->integer (ch-conv (string-ref s index))))
             *default-bound*)))))

(define (string-hash s . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (%string-hash s (lambda (x) x) bound)))

(define (string-ci-hash s . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (%string-hash s char-downcase bound)))

(define (symbol-hash s . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (%string-hash (symbol->string s) (lambda (x) x) bound)))

(define (vector-hash v bound)
  (let ((hashvalue 571)
    (len (vector-length v)))
    (do ((index 0 (+ index 1)))
      ((>= index len) (modulo hashvalue bound))
      (set! hashvalue (modulo (+ (* 257 hashvalue) (hash (vector-ref v index)))
                  *default-bound*)))))

(define-public (hash obj . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (cond ((integer? obj) (modulo obj bound))
      ((string? obj) (string-hash obj bound))
      ((symbol? obj) (symbol-hash obj bound))
      ((real? obj) (modulo (+ (numerator obj) (denominator obj)) bound))
      ((number? obj)
       (modulo (+ (hash (real-part obj)) (* 3 (hash (imag-part obj))))
           bound))
      ((char? obj) (modulo (char->integer obj) bound))
      ((vector? obj) (vector-hash obj bound))
      ((pair? obj) (modulo (+ (hash (car obj)) (* 3 (hash (cdr obj))))
                   bound))
      ((null? obj) 0)
      ((not obj) 0)
      ((procedure? obj) (error "hash: procedures cannot be hashed" obj))
      (else 1))))

