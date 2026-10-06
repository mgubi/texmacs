
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : compat-s7.scm
;; DESCRIPTION : compatability layer for S7
;; COPYRIGHT   : (C) 2021 Massimiliano Gubinelli
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

;; Guile's curried define, (define ((f a) b) ...), is s7's own define: the
;; vendored s7 is patched for it (src/Scheme/S7/patches/0003-curried-define).


;(define primitive-string->symbol string->symbol)
;(define-public (string->symbol s) (if (string-null? s) '() (primitive-string->symbol s)))

(define-public-macro (1+ n) `(+ ,n 1))
(define-public-macro (1- n) `(- ,n 1))
(define-public (noop . args) (and (pair? args) (car args)))

(define-public (delq x l)
  (if (pair? l) (if (eq? x (car l)) (delq x (cdr l)) (cons (car l) (delq x (cdr l)))) ()))

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

;; Guile's *random-state* is the default state used by 'random';
;; setting it reseeds s7's default random state
(varlet (rootlet) '*random-state* (*s7* 'default-random-state))
(set! (setter '*random-state* (rootlet))
      (lambda (sym val) (set! (*s7* 'default-random-state) val) val))

(define-public (list-copy lst)
  (copy lst)) ;; S7 has generic functions. copy do a shallow copy
  
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

;; as Guile's: the first entry of key what (equal?) goes, the list is
;; changed in place and returned (use the result: the first entry may go)
(define-public (assoc-remove! l what)
  (cond ((null? l) l)
        ((and (pair? (car l)) (equal? (caar l) what)) (cdr l))
        (else
         (let loop ((prev l))
           (cond ((null? (cdr prev)) l)
                 ((and (pair? (cadr prev)) (equal? (caadr prev) what))
                  (set-cdr! prev (cddr prev))
                  l)
                 (else (loop (cdr prev))))))))

(define-public (sort l op) (sort! (copy l) op))
;; Guile's module-ref: a TeXmacs module is an s7 environment, in which the
;; module's own bindings are looked up (used by tests which call functions a
;; module does not export)
(define-public (module-ref module sym) (let-ref module sym))
;; Guile's closure?: a procedure written in Scheme, which s7 recognizes by
;; its source (the source of a C function is the empty list)
(define-public (closure? f) (and (procedure? f) (pair? (procedure-source f))))
;; Guile's procedure-documentation: #f for a procedure without documentation
(define-public (procedure-documentation f)
  (let ((d (documentation f)))
    (and (string? d) (not (string-null? d)) d)))
;; Guile's debug options: s7 has no backtrace option to toggle (the debug
;; menu shows the option as off)
(define-public (debug-options . args) '())
(define-public (debug-enable . opts) (noop))
(define-public (debug-disable . opts) (noop))
;; Guile's procedure-property, for the arity only: a list of the required
;; and the optional arguments and whether there is a rest argument (s7's
;; arity is a pair of the minimum and the maximum, very large with a rest)
(define-public (procedure-property f key)
  (and (== key 'arity) (procedure? f)
       (let* ((a (arity f)) (rest? (>= (cdr a) 536870912)))
         (list (car a) (if rest? 0 (- (cdr a) (car a))) rest?))))

;; SRFI-13 string functions which Guile provides and s7 does not
;; (character arguments may be a char, a predicate or a char-set)

(define (char-matcher x)
  (if (char? x) (lambda (c) (char=? c x)) x))

(define-public (string-prefix? s1 s2)
  (let ((n1 (string-length s1)))
    (and (<= n1 (string-length s2)) (string=? s1 (substring s2 0 n1)))))

(define-public (string-suffix? s1 s2)
  (let ((n1 (string-length s1)) (n2 (string-length s2)))
    (and (<= n1 n2) (string=? s1 (substring s2 (- n2 n1) n2)))))

(define-public (string-count s x . range)
  (let ((m (char-matcher x))
        (start (if (pair? range) (car range) 0))
        (end (if (and (pair? range) (pair? (cdr range)))
                 (cadr range) (string-length s))))
    (do ((i start (+ i 1))
         (n 0 (if (m (string-ref s i)) (+ n 1) n)))
        ((>= i end) n))))

(define-public (string-skip s x . range)
  (let ((m (char-matcher x))
        (start (if (pair? range) (car range) 0))
        (end (if (and (pair? range) (pair? (cdr range)))
                 (cadr range) (string-length s))))
    (do ((i start (+ i 1)))
        ((or (>= i end) (not (m (string-ref s i))))
         (and (< i end) i)))))

(define-public (string-trim-right s . opt)
  (let ((m (if (pair? opt) (char-matcher (car opt)) char-whitespace?)))
    (do ((end (string-length s) (- end 1)))
        ((or (= end 0) (not (m (string-ref s (- end 1)))))
         (substring s 0 end)))))

;; Guile's stable-sort (a merge sort on lists, also accepting vectors)
(define-public (stable-sort seq less?)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((less? (car b) (car a)) (cons (car b) (merge a (cdr b))))
          (else (cons (car a) (merge (cdr a) b)))))
  (define (msort l n)
    (if (<= n 1)
        (if (= n 1) (list (car l)) '())
        (let ((h (quotient n 2)))
          (merge (msort l h) (msort (list-tail l h) (- n h))))))
  (if (vector? seq)
      (list->vector (stable-sort (vector->list seq) less?))
      (msort seq (length seq))))

;; Guile's hash-map->list; iterating over an s7 hash table gives (key . value)
(define-public (hash-map->list proc h)
  (map (lambda (entry) (proc (car entry) (cdr entry))) h))

(define-public (force-output) (flush-output-port *stdout*))

;; Guile's pretty-print (ice-9 pretty-print), which s7 lacks (its own is in
;; the library write.scm, not loaded): obj as write gives it, on one line
;; when it fits in 79 columns, else a list or a vector with one element per
;; line, indented under the first; then a newline. The text reads back as
;; obj. The keyword options of Guile are accepted and ignored.
(define (pp-written x)
  (call-with-output-string (lambda (p) (write x p))))

(define (pp-indent n port)
  (newline port)
  (display (make-string n #\space) port))

(define (pp-object x col port)
  (let ((s (pp-written x)))
    (cond ((<= (+ col (string-length s)) 79) (display s port))
          ((pair? x)
           (display "(" port)
           (pp-object (car x) (+ col 1) port)
           (let loop ((l (cdr x)))
             (cond ((null? l) (display ")" port))
                   ((pair? l)
                    (pp-indent (+ col 1) port)
                    (pp-object (car l) (+ col 1) port)
                    (loop (cdr l)))
                   (else
                    (pp-indent (+ col 1) port)
                    (display ". " port)
                    (pp-object l (+ col 3) port)
                    (display ")" port)))))
          ((and (vector? x) (> (vector-length x) 0))
           (display "#(" port)
           (pp-object (vector-ref x 0) (+ col 2) port)
           (do ((i 1 (+ i 1))) ((= i (vector-length x)))
             (pp-indent (+ col 2) port)
             (pp-object (vector-ref x i) (+ col 2) port))
           (display ")" port))
          (else (display s port)))))

(define-public (pretty-print obj . opts)
  (let ((port (if (and (pair? opts) (output-port? (car opts)))
                  (car opts)
                  (current-output-port))))
    (pp-object obj 0 port)
    (newline port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (string-null? s) (equal? (length s) 0))

(define-public (append! . ls) (apply append ls))

(define-public (string-split str ch)
  ;; as Guile's: the pieces between the occurrences of ch, so that there is
  ;; always one more piece than occurrences ("" gives (""), "a," ("a" ""))
  (let ((len (string-length str)))
    (let loop ((i (- len 1)) (end len) (acc '()))
      (cond ((< i 0) (cons (substring str 0 end) acc))
            ((char=? (string-ref str i) ch)
             (loop (- i 1) i (cons (substring str (+ i 1) end) acc)))
            (else (loop (- i 1) end acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guile-style records

;(define tmtable-type (make-record-type "tmtable" '(nrows ncols cells formats)))
;(define tmtable-record (record-constructor tmtable-type))
;(tm-define tmtable? (record-predicate tmtable-type))
;(tm-define tmtable-nrows (record-accessor tmtable-type 'nrows))
;(tm-define tmtable-ncols (record-accessor tmtable-type 'ncols))
;(tm-define tmtable-cells (record-accessor tmtable-type 'cells))
;(define tmtable-formats (record-accessor tmtable-type 'formats))

(define-public (make-record-type type fields)
  (inlet 'type type 'fields fields))

(define-public (record-constructor rec-type)
  (eval `(lambda ,(rec-type 'fields)
     (inlet 'type ,(rec-type 'type) ,@(map (lambda (f) (values (list 'quote f) f)) (rec-type 'fields))))))
 
(define-public-macro (record-accessor rec-type field)
  `(lambda (rec) (rec ,field)))

(define-public (record-predicate rec-type)
  (lambda (rec) (eq? (rec 'type) (rec-type 'type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From S7/r7rs.scm

;; delay and force: ugh
;;   this implementation is based on the r7rs spec

(define-public (make-promise done? proc)
  (list (cons done? proc)))

(define-public-macro (delay-force expr)
  `(make-promise #f (lambda () ,expr)))

(define-public-macro (delay expr) ; "delay" is taken damn it
  (list 'delay-force (list 'make-promise #t expr)))

;; a promise is ((done? . value)) if done? and ((done? . thunk)) otherwise
(define-public (force promise)
  (if (caar promise)
      (cdar promise)
      (let ((promise* ((cdar promise))))
        (if (not (caar promise))
            (begin
              (set-car! (car promise) (caar promise*))
              (set-cdr! (car promise) (cdar promise*))))
        (force promise))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hashing (use S7 internal hash)

(define *default-bound* (- (expt 2 29) 3))

(define-public (hash obj . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (modulo (hash-code obj) bound))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (while test . body)      ; while loop with predefined break and continue
  `(call-with-exit
    (lambda (break)
      (let continue ()
    (if (let () ,test)
        (begin
          (let () ,@body)
          (continue))
        (break))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string search and charsets

; Char-sets are hash tables from characters to #t.  Hash tables are
; applicable, so (cs ch) works as a membership test, like for the predicates
; which are also accepted wherever a char-set is expected.  We avoid closures
; on purpose: s7 (at least up to 11.9) can mis-apply a closure called from a
; loop after that loop was run with a closure of a different shape.
; s7 characters are bytes, so every char-set is a subset of 256 characters.

(define (char-set-from-predicate pred)
  (let ((cs (make-hash-table 256)))
    (do ((i 0 (+ i 1))) ((= i 256) cs)
      (let ((ch (integer->char i)))
        (if (pred ch) (hash-table-set! cs ch #t))))))

(define (->char-set cs)
  (if (hash-table? cs) cs (char-set-from-predicate cs)))

(define-public (char-set . l)
  (let ((cs (make-hash-table 256)))
    (for-each (lambda (ch) (hash-table-set! cs ch #t)) l)
    cs))

(define-public (string->char-set s)
  (apply char-set (string->list s)))

(define-public (char-set-adjoin cs . l)
  (let ((r (copy (->char-set cs))))
    (for-each (lambda (ch) (hash-table-set! r ch #t)) l)
    r))

(define-public (char-set-complement cs)
  (let ((cs (->char-set cs)) (r (make-hash-table 256)))
    (do ((i 0 (+ i 1))) ((= i 256) r)
      (let ((ch (integer->char i)))
        (if (not (hash-table-ref cs ch)) (hash-table-set! r ch #t))))))

(define-public (char-set-intersection cs . l)
  (let ((r (copy (->char-set cs))) (l (map ->char-set l)))
    (for-each (lambda (entry)
                (let ((ch (car entry)))
                  (if (not (let loop ((l l))
                             (or (null? l)
                                 (and (hash-table-ref (car l) ch)
                                      (loop (cdr l))))))
                      (hash-table-set! r ch #f))))
              (copy r))
    r))

(define-public (char-set-union . l)
  (let ((r (make-hash-table 256)))
    (for-each (lambda (cs)
                (for-each (lambda (entry) (hash-table-set! r (car entry) #t))
                          (->char-set cs)))
              l)
    r))

(define-public (char-set-contains? cs ch)
  (if (hash-table? cs) (hash-table-ref cs ch) (and (cs ch) #t)))

(define-public (char-set-size cs)
  (hash-table-entries (->char-set cs)))

(define-public char-set:whitespace (char-set #\space #\tab #\newline))
(define-public char-set:lower-case (char-set-from-predicate char-lower-case?))
(define-public char-set:upper-case (char-set-from-predicate char-upper-case?))
(define-public char-set:digit (char-set-from-predicate char-numeric?))

; string-index and string-rindex accept a character, a char-set or a
; predicate, and, as in Guile (SRFI-13), an optional start and end

(define-public (string-index str cs . range)
 (let ((chr (if (char? cs) (lambda (c) (char=? c cs)) cs))
       (start (if (pair? range) (car range) 0))
       (end (if (and (pair? range) (pair? (cdr range))) (cadr range)
                (string-length str))))
  (do ((pos start (+ 1 pos)))
      ((or (>= pos end) (chr (string-ref str pos)))
       (and (< pos end) pos)))))

(define-public (string-rindex str cs . range)
 (let ((chr (if (char? cs) (lambda (c) (char=? c cs)) cs))
       (start (if (pair? range) (car range) 0))
       (end (if (and (pair? range) (pair? (cdr range))) (cadr range)
                (string-length str))))
  (do ((pos (+ -1 end) (+ -1 pos)))
      ((or (< pos start) (chr (string-ref str pos)))
       (and (>= pos start) pos)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s7 does not have iota, let's provide it

(define-public (iota n)
   (let loop ((count (1- n)) (result '()))
     (if (< count 0) result
         (loop (1- count) (cons count result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO/FIXME

; redefine (error ...) to match guile usage
; https://www.gnu.org/software/guile/manual/html_node/Error-Reporting.html

