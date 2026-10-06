
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : compat-femtolisp.scm
;; DESCRIPTION : compatibility layer for femtolisp (the Guile functions)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot compat-femtolisp))

;;; for old-style initialization files
(define-public (exec-file . args)
  (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash tables (Guile), on the tables of femtolisp (equal? keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (make-hash-table . size) (table))
(define-public (hash-table? x) (table? x))
(define-public (hash-ref h key . default)
  (get h key (if (pair? default) (car default) #f)))
(define-public (hash-set! h key value) (put! h key value) value)
(define-public (hash-get-handle h key)
  (and (has? h key) (cons key (get h key))))
(define-public (hash-create-handle! h key init)
  (if (not (has? h key)) (put! h key init))
  (cons key (get h key)))
(define-public (hash-remove! h key)
  (and (has? h key)
       (let ((v (get h key)))
         (del! h key)
         (cons key v))))
(define-public (hash-fold proc init h)
  (table.foldl proc init h))
(define-public (hash-for-each proc h)
  (table.foldl (lambda (k v acc) (proc k v) acc) #t h))
(define-public (hash-map->list proc h)
  (table.foldl (lambda (k v acc) (cons (proc k v) acc)) '() h))
(define-public (hash-count pred h)
  (table.foldl (lambda (k v acc) (if (pred k v) (+ acc 1) acc)) 0 h))
(define-public (hash-clear! h)
  (for-each (lambda (k) (del! h k)) (table.keys h)))
(define-public hashq-ref hash-ref)
(define-public hashq-set! hash-set!)
(define-public hashq-remove! hash-remove!)
(define-public hashq-get-handle hash-get-handle)
(define-public hashv-ref hash-ref)
(define-public hashv-set! hash-set!)
(define-public hashv-remove! hash-remove!)

(define *default-bound* (- (expt 2 29) 3))
(define-public (hash obj . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (modulo (abs (%fl-hash obj)) bound)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Association lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (assoc-ref l what)
  (let ((b (assoc what l)))
    (if b (cdr b) #f)))
(define-public (assq-ref l what)
  (let ((b (assq what l)))
    (if b (cdr b) #f)))
(define-public (assv-ref l what)
  (let ((b (assv what l)))
    (if b (cdr b) #f)))

(define-public (assoc-set! l what val)
  (let ((b (assoc what l)))
    (if b (begin (set-cdr! b val) l) (cons (cons what val) l))))
(define-public assq-set! assoc-set!)
(define-public assv-set! assoc-set!)

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
(define-public assq-remove! assoc-remove!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists (SRFI-1 and Guile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (map-in-order . l) (apply map l))

;; (TeXmacs has an editor command fold, which replaces the SRFI-1 fold, as in
;; Guile where fold comes from (srfi srfi-1): the functions below use fold*)
(define (fold* kons knil l)
  (let loop ((l l) (acc knil))
    (if (null? l) acc (loop (cdr l) (kons (car l) acc)))))

(define-public (fold kons knil l . ls)
  (if (null? ls)
      (let loop ((l l) (acc knil))
        (if (null? l) acc (loop (cdr l) (kons (car l) acc))))
      (let loop ((ls (cons l ls)) (acc knil))
        (if (any null? ls) acc
            (loop (map cdr ls) (apply kons (append (map car ls) (list acc))))))))

(define-public (fold-right kons knil l . ls)
  (if (null? ls)
      (let loop ((l (reverse l)) (acc knil))
        (if (null? l) acc (loop (cdr l) (kons (car l) acc))))
      (let loop ((ls (map reverse (cons l ls))) (acc knil))
        (if (any null? ls) acc
            (loop (map cdr ls) (apply kons (append (map car ls) (list acc))))))))

(define-public (fold-left f init l . ls)
  (if (null? ls)
      (let loop ((l l) (acc init))
        (if (null? l) acc (loop (cdr l) (f acc (car l)))))
      (let loop ((ls (cons l ls)) (acc init))
        (if (any null? ls) acc
            (loop (map cdr ls) (apply f acc (map car ls)))))))

(define-public (reduce f ridentity l)
  (if (null? l) ridentity (fold* f (car l) (cdr l))))
(define-public (reduce-right f ridentity l)
  (if (null? l) ridentity
      (let loop ((l (reverse l)))
        (if (null? (cdr l)) (car l) (f (car l) (loop (cdr l)))))))

(define-public (append-map f . ls) (apply append (apply map f ls)))
(define-public (filter-map f . ls) (filter identity (apply map f ls)))
(define-public (find pred l)
  (let loop ((l l))
    (cond ((null? l) #f) ((pred (car l)) (car l)) (else (loop (cdr l))))))
(define-public (find-tail pred l)
  (let loop ((l l))
    (cond ((null? l) #f) ((pred (car l)) l) (else (loop (cdr l))))))
(define-public (partition pred l)
  (let loop ((l l) (in '()) (out '()))
    (cond ((null? l) (values (reverse! in) (reverse! out)))
          ((pred (car l)) (loop (cdr l) (cons (car l) in) out))
          (else (loop (cdr l) in (cons (car l) out))))))
(define-public (span pred l)
  (let loop ((l l) (acc '()))
    (if (and (pair? l) (pred (car l)))
        (loop (cdr l) (cons (car l) acc))
        (values (reverse! acc) l))))
(define-public (break pred l) (span (lambda (x) (not (pred x))) l))
(define-public (take l k) (list-head l k))
(define-public (drop l k) (list-tail l k))
(define-public (take-while pred l)
  (let loop ((l l) (acc '()))
    (if (and (pair? l) (pred (car l)))
        (loop (cdr l) (cons (car l) acc))
        (reverse! acc))))
(define-public (drop-while pred l)
  (if (and (pair? l) (pred (car l))) (drop-while pred (cdr l)) l))
(define-public (take-right l k) (list-tail l (- (length l) k)))
(define-public (drop-right l k) (list-head l (- (length l) k)))
(define-public (first l) (car l))
(define-public (second l) (cadr l))
(define-public (third l) (caddr l))
(define-public (fourth l) (cadddr l))
(define-public (fifth l) (car (cddddr l)))
(define-public (list-tabulate n f) (map f (iota n)))
(define-public (concatenate ls) (apply append ls))
(define-public (append-reverse rev tail) (append (reverse rev) tail))
(define-public (lset-adjoin = l . elts)
  (fold* (lambda (x acc) (if (member x acc) acc (append acc (list x)))) l elts))
(define-public (lset-union = . ls)
  (fold* (lambda (l acc) (apply lset-adjoin = acc l)) '() ls))
(define-public (lset-intersection = l . ls)
  (filter (lambda (x) (every (lambda (m) (member x m)) ls)) l))
(define-public (lset-difference = l . ls)
  (filter (lambda (x) (not (any (lambda (m) (member x m)) ls))) l))
(define-public (append-reverse! rev tail) (append (reverse rev) tail))
(define-public (last-pair* l) (last-pair l))

(define-public (copy-tree tree)
  (if (pair? tree)
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
      tree))

;; Guile's sort, sort! and stable-sort (a merge sort on lists, also accepting
;; vectors)
(define-public (stable-sort seq less?)
  (define (merge a b)
    (let loop ((a a) (b b) (acc '()))
      (cond ((null? a) (append-reverse! acc b))
            ((null? b) (append-reverse! acc a))
            ((less? (car b) (car a)) (loop a (cdr b) (cons (car b) acc)))
            (else (loop (cdr a) b (cons (car a) acc))))))
  (define (msort l n)
    (if (<= n 1)
        (if (= n 1) (list (car l)) '())
        (let ((h (quotient n 2)))
          (merge (msort l h) (msort (list-tail l h) (- n h))))))
  (if (vector? seq)
      (list->vector (stable-sort (vector->list seq) less?))
      (msort seq (length seq))))
(define-public sort stable-sort)
(define-public sort! stable-sort)
(define-public stable-sort! stable-sort)
(define-public (sorted? l less?)
  (or (null? l) (null? (cdr l))
      (and (not (less? (cadr l) (car l))) (sorted? (cdr l) less?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings (SRFI-13 and Guile); the characters may be a char, a predicate
;; or a char-set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-matcher x)
  (if (char? x) (lambda (c) (char=? c x)) (->char-predicate x)))

(define (range-start range) (if (pair? range) (car range) 0))
(define (range-end s range)
  (if (and (pair? range) (pair? (cdr range))) (cadr range) (string-length s)))

(define-public (string-prefix? s1 s2 . opt)
  (let ((n1 (string-length s1)))
    (and (<= n1 (string-length s2)) (string=? s1 (substring s2 0 n1)))))
(define-public (string-suffix? s1 s2 . opt)
  (let ((n1 (string-length s1)) (n2 (string-length s2)))
    (and (<= n1 n2) (string=? s1 (substring s2 (- n2 n1) n2)))))
(define-public (string-prefix-ci? s1 s2)
  (string-prefix? (string-downcase s1) (string-downcase s2)))
(define-public (string-suffix-ci? s1 s2)
  (string-suffix? (string-downcase s1) (string-downcase s2)))

(define-public (string-index str cs . range)
  (let ((start (range-start range)) (end (range-end str range)))
    (if (char? cs)
        (%string-index-of str cs start end)
        (let ((m (char-matcher cs)))
          (do ((pos start (+ 1 pos)))
              ((or (>= pos end) (m (string-ref str pos)))
               (and (< pos end) pos)))))))

(define-public (string-rindex str cs . range)
  (let ((m (char-matcher cs))
        (start (range-start range)) (end (range-end str range)))
    (do ((pos (- end 1) (- pos 1)))
        ((or (< pos start) (m (string-ref str pos)))
         (and (>= pos start) pos)))))
(define-public string-index-right string-rindex)

(define-public (string-skip s x . range)
  (let ((m (char-matcher x)) (start (range-start range)) (end (range-end s range)))
    (do ((i start (+ i 1)))
        ((or (>= i end) (not (m (string-ref s i))))
         (and (< i end) i)))))
(define-public (string-skip-right s x . range)
  (let ((m (char-matcher x)) (start (range-start range)) (end (range-end s range)))
    (do ((i (- end 1) (- i 1)))
        ((or (< i start) (not (m (string-ref s i))))
         (and (>= i start) i)))))

(define-public (string-count s x . range)
  (let ((m (char-matcher x)) (start (range-start range)) (end (range-end s range)))
    (do ((i start (+ i 1))
         (n 0 (if (m (string-ref s i)) (+ n 1) n)))
        ((>= i end) n))))

(define-public (string-contains s1 s2 . range)
  (%string-search s1 s2 (range-start range)))
(define-public (string-contains-ci s1 s2)
  (%string-search (string-downcase s1) (string-downcase s2) 0))

;; MIT Scheme: (string-search-forward pattern string start)
(define-public (string-search-forward pattern s start)
  (%string-search s pattern start))
(define-public (string-search-backward pattern s end)
  (let ((n (string-length pattern)))
    (let loop ((i (- end n)))
      (cond ((< i 0) #f)
            ((string=? (substring s i (+ i n)) pattern) (+ i n))
            (else (loop (- i 1)))))))
(define-public (string-search-all pattern s)
  (let loop ((i 0) (acc '()))
    (let ((j (%string-search s pattern i)))
      (if j (loop (+ j 1) (cons j acc)) (reverse! acc)))))

(define (whitespace? c) (char-whitespace? c))
(define-public (string-trim s . opt)
  (let ((m (if (pair? opt) (char-matcher (car opt)) whitespace?)))
    (let ((i (or (string-skip s m) (string-length s))))
      (substring s i (string-length s)))))
(define-public (string-trim-right s . opt)
  (let ((m (if (pair? opt) (char-matcher (car opt)) whitespace?)))
    (let ((i (string-skip-right s m)))
      (substring s 0 (if i (+ i 1) 0)))))
(define-public (string-trim-both s . opt)
  (apply string-trim (apply string-trim-right s opt) opt))

(define-public (string-pad s n . opt)
  (let ((c (if (pair? opt) (car opt) #\space)) (l (string-length s)))
    (if (>= l n) (substring s (- l n) l)
        (string-append (make-string (- n l) c) s))))
(define-public (string-pad-right s n . opt)
  (let ((c (if (pair? opt) (car opt) #\space)) (l (string-length s)))
    (if (>= l n) (substring s 0 n)
        (string-append s (make-string (- n l) c)))))

(define-public (string-take s n) (substring s 0 n))
(define-public (string-drop s n) (substring s n (string-length s)))
(define-public (string-take-right s n)
  (substring s (- (string-length s) n) (string-length s)))
(define-public (string-drop-right s n)
  (substring s 0 (- (string-length s) n)))
(define-public (string-reverse s) (list->string (reverse (string->list s))))
(define-public (string-concatenate l) (apply string-append l))
(define-public (string-tabulate f n) (list->string (map f (iota n))))
(define-public (string-every pred s)
  (let ((m (char-matcher pred)))
    (every m (string->list s))))
(define-public (string-any pred s)
  (let ((m (char-matcher pred)))
    (any m (string->list s))))
(define-public (string-filter pred s)
  (let ((m (char-matcher pred)))
    (list->string (filter m (string->list s)))))
(define-public (string-delete pred s)
  (let ((m (char-matcher pred)))
    (list->string (filter (lambda (c) (not (m c))) (string->list s)))))
(define-public (string-map f s) (list->string (map f (string->list s))))
(define-public (string-fold kons knil s)
  (fold* kons knil (string->list s)))

;; as Guile's: the pieces between the occurrences of ch, so that there is
;; always one more piece than occurrences ("" gives (""), "a," ("a" ""))
(define-public (string-split str ch)
  (let ((len (string-length str)))
    (let loop ((i (- len 1)) (end len) (acc '()))
      (cond ((< i 0) (cons (substring str 0 end) acc))
            ((char=? (string-ref str i) ch)
             (loop (- i 1) i (cons (substring str (+ i 1) end) acc)))
            (else (loop (- i 1) end acc))))))

(define-public (string-join l . opt)
  (let ((sep (if (pair? opt) (car opt) " "))
        (grammar (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) 'infix)))
    (cond ((null? l) "")
          ((eq? grammar 'prefix)
           (apply string-append (append-map (lambda (s) (list sep s)) l)))
          ((eq? grammar 'suffix)
           (apply string-append (append-map (lambda (s) (list s sep)) l)))
          (else
           (apply string-append
                  (cons (car l) (append-map (lambda (s) (list sep s))
                                            (cdr l))))))))

;; (string-replace s what by) is a function of the C++ glue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Char-sets: vectors of 256 booleans (characters are bytes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-char-set) (vector 'char-set (make-vector 256 #f)))
(define-public (char-set? x)
  (and (vector? x) (= (vector-length x) 2) (eq? (vector-ref x 0) 'char-set)))
(define (cs-bits cs) (vector-ref cs 1))

(define (char-set-from-predicate pred)
  (let ((cs (make-char-set)))
    (do ((i 0 (+ i 1))) ((= i 256) cs)
      (if (pred (integer->char i)) (vector-set! (cs-bits cs) i #t)))))

(define (->char-set x)
  (cond ((char-set? x) x)
        ((char? x) (char-set x))
        ((string? x) (string->char-set x))
        (else (char-set-from-predicate x))))

(define (->char-predicate x)
  (if (char-set? x)
      (let ((bits (cs-bits x)))
        (lambda (c) (vector-ref bits (char->integer c))))
      x))

(define-public (char-set . l)
  (let ((cs (make-char-set)))
    (for-each (lambda (c) (vector-set! (cs-bits cs) (char->integer c) #t)) l)
    cs))
(define-public (string->char-set s) (apply char-set (string->list s)))
(define-public (list->char-set l) (apply char-set l))
(define-public (char-set-contains? cs c)
  ((->char-predicate (->char-set cs)) c))
(define-public (char-set-adjoin cs . l)
  (let ((r (make-char-set)) (src (cs-bits (->char-set cs))))
    (do ((i 0 (+ i 1))) ((= i 256))
      (vector-set! (cs-bits r) i (vector-ref src i)))
    (for-each (lambda (c) (vector-set! (cs-bits r) (char->integer c) #t)) l)
    r))
(define-public (char-set-complement cs)
  (let ((p (->char-predicate (->char-set cs))))
    (char-set-from-predicate (lambda (c) (not (p c))))))
(define-public (char-set-union . l)
  (let ((ps (map (lambda (cs) (->char-predicate (->char-set cs))) l)))
    (char-set-from-predicate (lambda (c) (any (lambda (p) (p c)) ps)))))
(define-public (char-set-intersection cs . l)
  (let ((ps (map (lambda (cs) (->char-predicate (->char-set cs))) (cons cs l))))
    (char-set-from-predicate (lambda (c) (every (lambda (p) (p c)) ps)))))
(define-public (char-set-difference cs . l)
  (let ((p (->char-predicate (->char-set cs)))
        (ps (map (lambda (cs) (->char-predicate (->char-set cs))) l)))
    (char-set-from-predicate
     (lambda (c) (and (p c) (not (any (lambda (q) (q c)) ps)))))))
(define-public (char-set-size cs)
  (let ((bits (cs-bits (->char-set cs))))
    (do ((i 0 (+ i 1)) (n 0 (if (vector-ref bits i) (+ n 1) n)))
        ((= i 256) n))))
(define-public (char-set->list cs)
  (let ((bits (cs-bits (->char-set cs))))
    (filter identity
            (map (lambda (i) (and (vector-ref bits i) (integer->char i)))
                 (iota 256)))))

(define-public char-set:whitespace (char-set-from-predicate char-whitespace?))
(define-public char-set:lower-case (char-set-from-predicate char-lower-case?))
(define-public char-set:upper-case (char-set-from-predicate char-upper-case?))
(define-public char-set:letter (char-set-from-predicate char-alphabetic?))
(define-public char-set:digit (char-set-from-predicate char-numeric?))
(define-public char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))
(define-public char-set:punctuation
  (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}"))
(define-public char-set:full (char-set-from-predicate (lambda (c) #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of symbols, procedures and objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define property-table (table))
(define-public (object-property obj key)
  (let ((p (get property-table obj #f)))
    (and p (let ((b (assq key p))) (and b (cdr b))))))
(define-public (set-object-property! obj key val)
  (let ((p (get property-table obj '())))
    (put! property-table obj
          (cons (cons key val) (filter (lambda (b) (not (eq? (car b) key))) p)))
    val))
(define-public symbol-property object-property)
(define-public set-symbol-property! set-object-property!)
(define-public (source-property obj key) #f)

;; the source of a procedure, kept by the compiler (*keep-source*)
(define-public (procedure-source f) (function-source f))
(define-public (procedure-documentation f) #f)
(define-public (procedure-name f)
  (and (procedure? f)
       (let ((n (or (%builtin-name f)
                    (and (function? f)
                         (trycatch (function:name f) (lambda (e) #f))))))
         (and (symbol? n) (not (eq? n 'lambda)) n))))
(define-public (closure? f) (function? f))
;; Guile's procedure-property, for the arity only: a list of the required
;; and the optional arguments and whether there is a rest argument, from the
;; lambda list of the source (no required argument and a rest for builtins)
(define (lambda-list-arity l)
  (let loop ((l l) (req 0) (opt 0))
    (cond ((null? l) (list req opt #f))
          ((symbol? l) (list req opt #t))
          ((pair? (car l)) (loop (cdr l) req (+ opt 1)))
          (else (loop (cdr l) (+ req 1) opt)))))
(define-public (procedure-arity f)
  (let ((src (function-source f)))
    (if (and (pair? src) (pair? (cdr src)))
        (lambda-list-arity (cadr src))
        (list 0 0 #t))))
(define-public (procedure-property f key)
  (and (eq? key 'arity) (procedure? f) (procedure-arity f)))

;; Guile's debug options
(define-public (debug-options . args) '())
(define-public (debug-enable . opts) (noop))
(define-public (debug-disable . opts) (noop))
(define-public (debug-set! . opts) (noop))
(define-public (read-set! . opts) (noop))
(define-public (read-enable . opts) (noop))

;; Guile's display-error, used by format-err (debug.scm) for the errors of
;; remote services: "subr: message", the message formatted with args
(define-public (display-error frame port subr message args rest)
  (when subr (display subr port) (display ": " port))
  (display (apply format #f message (if (list? args) args '())) port)
  (newline port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatted output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guile's simple-format: ~A ~S ~% ~~ (and ~a ~s)
(define-public (format dest msg . args)
  (cond ((not dest) (call-with-output-string
                     (lambda (p) (%simple-format p msg args))))
        ((eq? dest #t) (display (call-with-output-string
                                 (lambda (p) (%simple-format p msg args)))))
        (else (%simple-format dest msg args))))
(define-public simple-format format)

;; Guile's pretty-print (ice-9 pretty-print): obj as write gives it, on one
;; line when it fits in 79 columns, else a list or a vector with one element
;; per line, indented under the first; then a newline
(define (pp-indent n port)
  (newline port)
  (display (make-string n #\space) port))

(define (pp-object x col port)
  (let ((s (object->string x)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Records (Guile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a record type is #(record-type name fields), a record #(record type vals...)
(define-public (make-record-type name fields . opt)
  (vector 'record-type name fields))
(define (record-field-index type field)
  (+ 2 (list-index (lambda (f) (eq? f field)) (vector-ref type 2))))
(define-public (record-constructor type . fields)
  (let ((n (length (vector-ref type 2))))
    (lambda vals (apply vector 'record type vals))))
(define-public (record-predicate type)
  (lambda (x) (and (vector? x) (> (vector-length x) 1)
                   (eq? (vector-ref x 0) 'record) (eq? (vector-ref x 1) type))))
(define-public (record-accessor type field)
  (let ((i (record-field-index type field)))
    (lambda (r) (vector-ref r i))))
(define-public (record-modifier type field)
  (let ((i (record-field-index type field)))
    (lambda (r v) (vector-set! r i v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guile's while, with break and continue
(define-public-macro (while test . body)
  (let ((loop (gensym)) (k (gensym)))
    `(call-with-exit
      (lambda (break)
        (let ,loop ()
          (if ,test
              (begin
                (call-with-exit (lambda (continue) ,@body))
                (,loop))
              (break #f)))))))

(define-public (seed->random-state seed) seed)
(define-public *random-state* #f)
(define-public (random-state? x) #t)

(define-public (current-filename) #f)
(define-public (gc) (%gc))
