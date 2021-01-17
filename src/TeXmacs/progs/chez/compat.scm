
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chez/compat.scm
;; DESCRIPTION : compatability layer for Chez
;; COPYRIGHT   : (C) 2021 Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (chez compat))

;;; certain Scheme versions do not define 'filter'
#;(if (not (defined? 'filter))
    (define-public (filter pred? l)
      (apply append (map (lambda (x) (if (pred? x) (list x) (list))) l))))


;(define primitive-string->symbol string->symbol)
;(define-public (string->symbol s) (if (string-null? s) '() (primitive-string->symbol s)))

(define-public (defined? s) (or (top-level-bound? s) (top-level-syntax? s (scheme-environment))))

(define-public-macro (1+ n) `(+ ,n 1))
(define-public-macro (1- n) `(- ,n 1))

(define-public (first x) (car x))
(define-public (second x) (cadr x))
(define-public (third x) (caddr x))
(define-public (fourth x) (cadddr x))
(define-public (fifth x) (cadddr (cdr x)))
(define-public (sixth x) (cadddr (cddr x)))

(define-public (object->string obj)
 (call-with-string-output-port (lambda (p) (write obj p))))
 
(define-public (procedure-source . args)  '(lambda () (noop))) ;FIXME

(define-public (delq x l)
  (if (pair? l) (if (eq? x (car l)) (delq x (cdr l)) (cons (car l) (delq x (cdr l)))) '()))

(define-public (acons key datum alist) (cons (cons key datum) alist))

(define-public (symbol-append . l)
   (string->symbol (apply string-append (map symbol->string l))))

(define-public (map-in-order f ls . more)
    (if (null? more)
        (let map1 ([ls ls])
          (if (null? ls)
              '()
              (cons (f (car ls))
                    (map1 (cdr ls)))))
        (let map-more ([ls ls] [more more])
          (if (null? ls)
              '()
              (cons
                (apply f (car ls) (map car more))
                (map-more (cdr ls) (map cdr more)))))))

;FIXME this below is not really correct
(define-public (catch thunk handler)
  (with-exception-handler handler thunk))
  
(define-public lazy-catch catch)

#;(define-public (last-pair lis)
;;  (check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))


(define-public (seed->random-state seed) (random-seed seed))

#;(define-public (list-copy lst)
  (copy lst)) ;; S7 has generic functions. copy do a shallow copy
  
(define-public (copy-tree tree)
  (let loop ((tree tree))
    (if (pair? tree)
        (cons (loop (car tree)) (loop (cdr tree)))
        tree)))


;;FIXME: assoc-set! is tricky to use, maybe just get rid in the code
(define-public (assoc-set! l what val)
  (let ((b (assoc what l)))
    (if b (set-cdr! b val) (set! l (cons (cons what val) l)))
    l))

(define-public (assoc-ref l what)
  (let ((b (assoc what l)))
    (if b (cdr b) #f)))

#;(define-public (sort l op) (sort! (copy l) op))

(define-public (force-output) (flush-output-port *stdout*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (string-null? s) (equal? (length s) 0))

#;(define-public (append! . ls) (apply append ls))


(define-public (char-position ch s start)
  (let ((l (string-length s)))
    (let h ((x start))
      (if (< x l)
            (if (eq? (string-ref s x) ch) x (h (+ x 1)))
            #f))))

(define-public (string-split s ch)
  (let ((len (string-length s)))
    (let f ((start 0) (acc '()))
      (if (< start len)
        (let ((end (+ (or (char-position ch s start) (- len 1)) 1)))
           (f end (cons (substring s start end)  acc)))
        (reverse acc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guile-style records

;(define tmtable-type (make-record-type "tmtable" '(nrows ncols cells formats)))
;(define tmtable-record (record-constructor tmtable-type))
;(tm-define tmtable? (record-predicate tmtable-type))
;(tm-define tmtable-nrows (record-accessor tmtable-type 'nrows))
;(tm-define tmtable-ncols (record-accessor tmtable-type 'ncols))
;(tm-define tmtable-cells (record-accessor tmtable-type 'cells))
;(define tmtable-formats (record-accessor tmtable-type 'formats))

#;(define-public-macro (make-record-type type fields)
  `(define-record-type ,(string->symbol type) ,fields))

#;(define-public-macro (record-constructor rec-type)
  (eval `(lambda ,(rec-type 'fields)
     (inlet 'type ,(rec-type 'type) ,@(map (lambda (f) (values (list 'quote f) f)) (rec-type 'fields))))))
 
#;(define-public-macro (record-accessor rec-type field)
  `(lambda (rec) (rec ,field)))

#;(define-public (record-predicate rec-type)
  (lambda (rec) (eq? (rec 'type) (rec-type 'type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hashing (from the SRFI reference implementation)

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
      ((procedure? obj) 245) ;(error "hash: procedures cannot be hashed" obj))
      (else 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (while test . body)      ; while loop with predefined break and continue
  `(call/cc
    (lambda (break)
      (let continue ()
    (if (let () ,test)
        (begin
          (let () ,@body)
          (continue))
        (break))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string search and charsets

; we implement char-sets via predicates

(define-public (char-set-adjoin cs . l)
   (lambda (ch) (or (cs ch) (memq ch l))))
   
(define-public (char-set-complement cs)
  (lambda (ch) (not (cs ch))))

(define-public (char-set:whitespace ch)
  (memq ch '(#\space #\tab #\newline)))
  
; string-index and string-rindex accepts char-sets

(define-public (string-index str cs)
 (let ((chr (if (char? cs) (lambda (c) (char=? c cs)) cs)))
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (chr (string-ref str pos)))
       (and (< pos len) pos)))))

(define-public (string-rindex str cs)
 (let ((chr (if (char? cs) (lambda (c) (char=? c cs)) cs)))
  (do ((pos (+ -1 (string-length str)) (+ -1 pos)))
      ((or (negative? pos) (chr (string-ref str pos)))
       (and (not (negative? pos)) pos)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO/FIXME

; redefine (error ...) to match guile usage
; https://www.gnu.org/software/guile/manual/html_node/Error-Reporting.html


