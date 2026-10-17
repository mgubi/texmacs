
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-tables.scm
;; DESCRIPTION : the parse table of a tree-sitter grammar
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The LR(1) construction of tree-sitter (item.rs, item_set_builder.rs and
;; build_parse_table.rs).
;;
;; An item is a vector #(variable production step inherited? key) where the
;; production -1 is the augmented start production, and key an integer which
;; identifies the item as tree-sitter compares them: two items of the same
;; variable at the same step are the same when their productions agree on the
;; precedence and associativity at the dot, the aliases and fields of the
;; steps before the dot and everything after the dot (and also on the symbols
;; before the dot, when inherited? says that one of them is a hidden child
;; with fields).
;;
;; An item set is a sorted list of entries #(item lookaheads reserved), with
;; lookahead sets interned as integers.
;;
;; The parse states are vectors, see make-state. Actions are lists:
;;
;;   (accept) (shift state repetition?) (shift-extra) (recover)
;;   (reduce symbol child-count dynamic-precedence production-id)
;;
;; and the entries of the non-terminals are (goto state) or (shift-extra).

(texmacs-module (treesit ts-tables)
  (:use (treesit ts-base) (treesit ts-grammar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ordered maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maps from integers which remember the order of insertion, like IndexMap
(define-public (make-omap) (vector (make-ahash-table) (make-gvec)))
(define-public (om-ref m k) (ahash-ref (vector-ref m 0) k))
(define-public (om-has? m k) (and (ahash-ref (vector-ref m 0) k) #t))
(define-public (om-set! m k v)
  (unless (ahash-ref (vector-ref m 0) k) (gvec-push! (vector-ref m 1) k))
  (ahash-set! (vector-ref m 0) k v))
(define-public (om-keys m) (gvec->list (vector-ref m 1)))
(define-public (om-length m) (gvec-length (vector-ref m 1)))
(define-public (om-for-each fun m)
  (for-each (lambda (k) (fun k (om-ref m k))) (om-keys m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Precedences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compare-precedence g left left-symbols right right-symbols)
  (define (matches? entry prec symbols)
    (if (eq? (car entry) 'name)
        (and (string? prec) (string=? (cdr entry) prec))
        (ts-any (lambda (s) (equal? (g-variable-name g (sym-index s)) (cdr entry)))
                symbols)))
  (cond ((and (integer? left) (integer? right) (or (not (= left 0)) (not (= right 0))))
         (ts-cmp left right))
        ((and (integer? left) (not right) (not (= left 0))) (ts-cmp left 0))
        ((and (not left) (integer? right) (not (= right 0))) (ts-cmp 0 right))
        (else
         (let loop ((lists (g-ref g 'precedences)))
           (if (null? lists) 0
               (let scan ((l (car lists)) (saw-left #f) (saw-right #f))
                 (cond ((null? l) (loop (cdr lists)))
                       ((matches? (car l) left left-symbols)
                        (if saw-right -1 (scan (cdr l) #t saw-right)))
                       ((matches? (car l) right right-symbols)
                        (if saw-left 1 (scan (cdr l) saw-left #t)))
                       (else (scan (cdr l) saw-left saw-right)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define start-production
  (vector (vector (vector (non-terminal 0) #f #f #f #f 65535)) 0 #f))

;; An item is #(var prod step inherited? key production step prev-step
;; sort-key): the production, the step at the dot and the one before (or #f)
;; are those of the grammar, computed once; the sort key orders as item<?.
;; Items are made by make-lr-item only, shared and never modified.

(define-public (item-production g item) (vector-ref item 5))

(define-public (item-var item) (vector-ref item 0))
(define-public (item-prod item) (vector-ref item 1))
(define-public (item-step-index item) (vector-ref item 2))
(define-public (item-inherited? item) (vector-ref item 3))
(define-public (item-key item) (vector-ref item 4))
(define-public (item-augmented? item) (< (vector-ref item 0) 0))

(define-public (item-step g item) (vector-ref item 6))

(define-public (item-symbol g item)
  (let ((s (vector-ref item 6))) (and s (step-symbol s))))

(define-public (item-done? g item) (not (vector-ref item 6)))

(define (item-prev-step g item) (vector-ref item 7))

(define-public (item-precedence g item)
  (let ((s (item-prev-step g item))) (and s (step-prec s))))

(define-public (item-associativity g item)
  (let ((s (item-prev-step g item))) (and s (step-assoc s))))

;; the keys of the items

(define (content-key p dot with-symbols?)
  (let* ((steps (vector->list (prod-steps p)))
         (prev (and (> dot 0) (list-ref steps (- dot 1)))))
    (list (prod-dynamic p) (prod-eof? p) (length steps)
          (and prev (step-prec prev)) (and prev (step-assoc prev)) dot
          (ts-map (lambda (s) (list (step-alias s) (step-field s)
                                    (and with-symbols? (step-symbol s))))
                  (ts-take steps dot))
          (list-tail steps dot))))

(define (make-key-table) (vector (make-ahash-table) (make-ahash-table) (make-ahash-table)))

(define (item-key-of g keys var prod step inherited?)
  ;; an integer identifying the item, memoized by the production and step
  (let* ((memo (vector-ref keys 1))
         (k (+ (* (+ (* (+ (* (+ prod 1) 1024) step) 2) (if inherited? 1 0)) 1048576) (+ var 1)))
         (old (ahash-ref memo k)))
    (or old
        (let* ((p (if (< prod 0) start-production (g-production g prod)))
               (content (list var step inherited? (content-key p step inherited?)))
               (table (vector-ref keys 0))
               (id (or (ahash-ref table content)
                       (let ((id (ahash-size table)))
                         (ahash-set! table content id)
                         id))))
          (ahash-set! memo k id)
          id))))

(define item-key-limit 2097152)

(define (make-lr-item g keys var prod step inherited?)
  ;; the item, shared with the earlier ones of the same fields
  (let* ((memo (vector-ref keys 2))
         (k (+ (* (+ (* (+ (* (+ prod 1) 1024) step) 2) (if inherited? 1 0)) 1048576) (+ var 1))))
    (or (ahash-ref memo k)
        (let* ((key (item-key-of g keys var prod step inherited?))
               (p (if (< prod 0) start-production (g-production g prod)))
               (n (prod-length p))
               (item (vector var prod step inherited? key p
                             (and (< step n) (prod-step p step))
                             (and (> step 0) (prod-step p (- step 1)))
                             (+ (* (+ (* step item-key-limit) var 1) item-key-limit) key))))
          (when (or (>= key item-key-limit) (>= (+ var 1) item-key-limit))
            (ts-error "too many items for the sort keys"))
          (ahash-set! memo k item)
          item))))

(define (item-successor g keys item)
  (make-lr-item g keys (item-var item) (item-prod item) (+ (item-step-index item) 1)
             (item-inherited? item)))

(define (item-substitute g keys item prod)
  (make-lr-item g keys (item-var item) prod (item-step-index item) (item-inherited? item)))

(define (item-with-inherited g keys item)
  (make-lr-item g keys (item-var item) (item-prod item) (item-step-index item) #t))

(define (item<? a b)
  ;; the order of tree-sitter: step, then variable, then the key
  (< (vector-ref a 8) (vector-ref b 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookahead sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (make-la-pool)
  (let ((pool (vector (make-gvec) (make-ahash-table) (make-ahash-table))))
    (la-intern pool '())
    pool))

(define-public (la-get pool id) (gvec-ref (vector-ref pool 0) id))

(define-public (la-intern pool set)
  (or (ahash-ref (vector-ref pool 1) set)
      (let ((id (gvec-push! (vector-ref pool 0) set)))
        (ahash-set! (vector-ref pool 1) set id)
        id)))

(define-public (la-union pool a b)
  (cond ((or (= a b) (= b 0)) a)
        ((= a 0) b)
        (else
         (let* ((key (if (< a b) (+ (* a 16777216) b) (+ (* b 16777216) a)))
                (old (ahash-ref (vector-ref pool 2) key)))
           (or old
               (let ((id (la-intern pool (ts-union (la-get pool a) (la-get pool b)))))
                 (ahash-set! (vector-ref pool 2) key id)
                 id))))))

(define-public (la-insert pool a s)
  (la-intern pool (ts-insert s (la-get pool a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Item sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; while they are built, item sets are hash tables from the keys of the items
;; to entries #(item lookaheads reserved)

(define (make-item-set) (make-ahash-table))

(define (item-set-insert! set item)
  ;; the entry of item, created with empty lookaheads if needed
  (let ((k (item-key item)))
    (or (ahash-ref set k)
        (let ((e (vector item 0 0)))
          (ahash-set! set k e)
          e))))

(define-public (item-set-entries set)
  ;; the entries, sorted
  (ts-sort (map cdr (ahash-table->list set))
           (lambda (a b) (item<? (vector-ref a 0) (vector-ref b 0)))))

;; Item sets are identified by vectors of integers (the key of an item
;; determines its variable), interned in a table from a hash of the vector
;; to the alist of the vectors with that hash and their ids: smaller and
;; faster to hash than lists of lists.

(define (entries-signature entries)
  ;; what identifies an item set: key, lookaheads and reserved of each item
  (let ((v (make-vector (* 3 (length entries)) 0)))
    (let loop ((l entries) (i 0))
      (if (null? l) v
          (let ((e (car l)))
            (vector-set! v i (item-key (vector-ref e 0)))
            (vector-set! v (+ i 1) (vector-ref e 1))
            (vector-set! v (+ i 2) (vector-ref e 2))
            (loop (cdr l) (+ i 3)))))))

(define (entries-core entries)
  ;; what identifies the core of an item set: the keys of its items
  (list->vector (ts-map (lambda (e) (item-key (vector-ref e 0))) entries)))

(define (int-vector-hash v)
  (let loop ((i 0) (h (vector-length v)))
    (if (= i (vector-length v)) h
        (loop (+ i 1) (modulo (+ (* h 31) (vector-ref v i)) 1000000007)))))

(define (make-vector-table) (vector (make-ahash-table) 0))

(define (vector-table-ref t v)
  ;; the id of v, or #f
  (let ((bucket (ahash-ref (vector-ref t 0) (int-vector-hash v))))
    (and bucket (let ((x (assoc v bucket))) (and x (cdr x))))))

(define (vector-table-intern! t v)
  ;; the id of v, a new one (0, 1, ...) if v was not there
  (let* ((h (int-vector-hash v))
         (bucket (or (ahash-ref (vector-ref t 0) h) '()))
         (x (assoc v bucket)))
    (if x (cdr x)
        (let ((id (vector-ref t 1)))
          (ahash-set! (vector-ref t 0) h (cons (cons v id) bucket))
          (vector-set! t 1 (+ id 1))
          id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First and last sets, closures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (make-item-set-builder g keys)
  ;; a hash table with the first and last sets of the symbols and the
  ;; additions of the closures
  (let* ((b (make-ahash-table))
         (first (make-ahash-table))
         (last (make-ahash-table))
         (reserved-first (make-ahash-table))
         (pool (make-la-pool))
         (n-terminals (vector-length (g-ref g 'lexical)))
         (n-externals (length (g-ref g 'external-tokens)))
         (n-vars (g-variable-count g))
         (additions (make-vector n-vars '()))
         (word (g-ref g 'word))
         (inline (g-ref g 'inline)))
    (define (prods-of v)
      (let ((r (g-var-prods g v)))
        (let loop ((i (- (cdr r) 1)) (acc '()))
          (if (< i (car r)) acc (loop (- i 1) (cons i acc))))))
    (do ((i 0 (+ i 1))) ((= i n-terminals))
      (ahash-set! first (terminal i) (list (terminal i)))
      (ahash-set! last (terminal i) (list (terminal i)))
      (ahash-set! reserved-first (terminal i) 0))
    (do ((i 0 (+ i 1))) ((= i n-externals))
      (ahash-set! first (external i) (list (external i)))
      (ahash-set! last (external i) (list (external i)))
      (ahash-set! reserved-first (external i) 0))
    (do ((i 0 (+ i 1))) ((= i n-vars))
      (let ((fs '()) (rf 0) (ls '()))
        (let ((processed (make-ahash-table)))
          (let loop ((stack (list i)))
            (when (pair? stack)
              (let ((todo (cdr stack)))
                (for-each
                 (lambda (pid)
                   (let ((p (g-production g pid)))
                     (when (> (prod-length p) 0)
                       (let* ((step (prod-step p 0)) (s (step-symbol step)))
                         (if (or (terminal? s) (external? s))
                             (set! fs (ts-insert s fs))
                             (unless (ahash-ref processed s)
                               (ahash-set! processed s #t)
                               (set! todo (cons (sym-index s) todo))))
                         (set! rf (max rf (step-reserved step)))))))
                 (prods-of (car stack)))
                (loop todo)))))
        (let ((processed (make-ahash-table)))
          (let loop ((stack (list i)))
            (when (pair? stack)
              (let ((todo (cdr stack)))
                (for-each
                 (lambda (pid)
                   (let ((p (g-production g pid)))
                     (when (> (prod-length p) 0)
                       (let ((s (step-symbol (prod-step p (- (prod-length p) 1)))))
                         (if (or (terminal? s) (external? s))
                             (set! ls (ts-insert s ls))
                             (unless (ahash-ref processed s)
                               (ahash-set! processed s #t)
                               (set! todo (cons (sym-index s) todo))))))))
                 (prods-of (car stack)))
                (loop todo)))))
        (ahash-set! first (non-terminal i) fs)
        (ahash-set! reserved-first (non-terminal i) rf)
        (ahash-set! last (non-terminal i) ls)))
    ;; the additions of each non-terminal
    (do ((i 0 (+ i 1))) ((= i n-vars))
      (let ((infos (make-omap)))   ; variable -> #(lookaheads reserved propagates?)
        (let loop ((stack (list (list i '() 0 #t))))
          (when (pair? stack)
            (let* ((top (car stack))
                   (v (car top)) (las (cadr top)) (rid (caddr top)) (prop (cadddr top))
                   (info (or (om-ref infos v)
                             (let ((x (vector '() 0 #f))) (om-set! infos v x) x)))
                   (new-las (ts-union (vector-ref info 0) las))
                   (did (or (not (equal? new-las (vector-ref info 0)))
                            (> rid (vector-ref info 1))
                            (and prop (not (vector-ref info 2)))))
                   (rest (cdr stack)))
              (vector-set! info 0 new-las)
              (when (> rid (vector-ref info 1)) (vector-set! info 1 rid))
              (when prop (vector-set! info 2 #t))
              (if (not did) (loop rest)
                  (begin
                    (for-each
                     (lambda (pid)
                       (let ((p (g-production g pid)))
                         (when (and (> (prod-length p) 0)
                                    (non-terminal? (step-symbol (prod-step p 0))))
                           (let ((s (sym-index (step-symbol (prod-step p 0)))))
                             (set! rest
                                   (cons (cond ((> (prod-length p) 1)
                                                (let ((next (step-symbol (prod-step p 1))))
                                                  (list s (ahash-ref first next)
                                                        (ahash-ref reserved-first next) #f)))
                                               ((prod-eof? p) (list s (list sym-end) 0 #f))
                                               (else (list s las rid prop)))
                                         rest))))))
                     (prods-of v))
                    (loop rest))))))
        (let ((result '()))
          (om-for-each
           (lambda (v info)
             (unless (member (non-terminal v) inline)
               (let* ((base (vector (la-intern pool (vector-ref info 0))
                                    (vector-ref info 1)
                                    (vector-ref info 2)
                                    (and word (ts-contains? (vector-ref info 0) word) #t)))
                      (eof-info (vector (la-intern pool (list sym-end)) 0 #f #f)))
                 (for-each
                  (lambda (pid)
                    (let ((ids (g-inlined g pid 0)))
                      (for-each
                       (lambda (id)
                         (let ((a (cons (make-lr-item g keys v id 0 #f)
                                        (if (prod-eof? (g-production g id)) eof-info base))))
                           (unless (member a result) (set! result (cons a result)))))
                       (or ids (list pid)))))
                  (prods-of v)))))
           infos)
          (vector-set! additions i (reverse result)))))
    (ahash-set! b 'first first)
    (ahash-set! b 'last last)
    (ahash-set! b 'reserved-first reserved-first)
    (ahash-set! b 'pool pool)
    (ahash-set! b 'additions additions)
    (ahash-set! b 'first-ids (make-ahash-table))
    b))

(define-public (first-set b s) (ahash-ref (ahash-ref b 'first) s))
(define-public (last-set b s) (ahash-ref (ahash-ref b 'last) s))
(define-public (reserved-first b s) (ahash-ref (ahash-ref b 'reserved-first) s))

(define (first-set-id b s)
  (let ((ids (ahash-ref b 'first-ids)))
    (or (ahash-ref ids s)
        (let ((id (la-intern (ahash-ref b 'pool) (first-set b s))))
          (ahash-set! ids s id)
          id))))

(define (transitive-closure g keys b kernel)
  (let ((set (make-item-set))
        (pool (ahash-ref b 'pool))
        (word (g-ref g 'word)))
    (define (add-item! item las reserved)
      (let ((s (item-symbol g item)))
        (when (and s (non-terminal? s))
          (let* ((next (let ((p (item-production g item)) (i (+ (item-step-index item) 1)))
                         (and (< i (prod-length p)) (step-symbol (prod-step p i)))))
                 (following (if next (first-set-id b next) las))
                 (following-reserved (if next (reserved-first b next) reserved)))
            (for-each
             (lambda (a)
               (let ((e (item-set-insert! set (car a)))
                     (info (cdr a)))
                 (vector-set! e 1 (la-union pool (vector-ref e 1) (vector-ref info 0)))
                 (when (vector-ref info 3)
                   (vector-set! e 2 (max (vector-ref e 2) (vector-ref info 1))))
                 (when (vector-ref info 2)
                   (vector-set! e 1 (la-union pool (vector-ref e 1) following))
                   (when (and word (ts-contains? (la-get pool following) word))
                     (vector-set! e 2 (max (vector-ref e 2) following-reserved))))))
             (vector-ref (ahash-ref b 'additions) (sym-index s))))))
      (let ((e (item-set-insert! set item)))
        (vector-set! e 1 (la-union pool (vector-ref e 1) las))
        (vector-set! e 2 (max (vector-ref e 2) reserved))))
    (for-each
     (lambda (entry)
       (let* ((item (vector-ref entry 0))
              (ids (and (>= (item-prod item) 0)
                        (g-inlined g (item-prod item) (item-step-index item)))))
         (if ids
             (for-each (lambda (id)
                         (add-item! (item-substitute g keys item id)
                                    (vector-ref entry 1) (vector-ref entry 2)))
                       ids)
             (add-item! item (vector-ref entry 1) (vector-ref entry 2)))))
     kernel)
    (item-set-entries set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fields of the variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (child-type g step)
  ;; (alias . named?) or the symbol
  (or (step-alias step)
      (ahash-ref (g-ref g 'default-aliases) (step-symbol step))
      (step-symbol step)))

(define-public (child-type-kind g t)
  (if (pair? t)
      (if (cdr t) vt-named vt-anonymous)
      (cond ((member t (g-ref g 'supertypes)) vt-named)
            ((member t (g-ref g 'inline)) vt-hidden)
            ((non-terminal? t) (g-variable-kind g (sym-index t)))
            ((terminal? t) (lex-kind* g (sym-index t)))
            ((external? t) (vector-ref (list-ref (g-ref g 'external-tokens) (sym-index t)) 1))
            (else vt-hidden))))

(define (lex-kind* g i) (vector-ref (vector-ref (g-ref g 'lexical) i) 1))

(define-public (compute-variable-fields! g)
  ;; for each variable, the sorted names of its visible fields (also the
  ;; fields inherited from hidden children)
  (let* ((n (g-variable-count g))
         (fields (make-vector n '()))     ; alist field -> child types
         (children (make-vector n '())))  ; child types
    (define (visible? t) (vt-visible? (child-type-kind g t)))
    (define (add-types l types)
      (let loop ((types types) (l l))
        (if (null? types) l (loop (cdr types) (if (member (car types) l) l (cons (car types) l))))))
    (let loop ()
      (let ((changed #f))
        (do ((i 0 (+ i 1))) ((= i n))
          (let ((r (g-var-prods g i)))
            (do ((pid (car r) (+ pid 1))) ((= pid (cdr r)))
              (let ((p (g-production g pid)))
                (do ((k 0 (+ k 1))) ((= k (prod-length p)))
                  (let* ((step (prod-step p k))
                         (s (step-symbol step))
                         (t (child-type g step))
                         (hidden (and (not (visible? t))
                                      (not (member s (g-ref g 'supertypes)))
                                      (non-terminal? s))))
                    (define (extend-children! types)
                      (let ((new (add-types (vector-ref children i) types)))
                        (unless (= (length new) (length (vector-ref children i)))
                          (set! changed #t)
                          (vector-set! children i new))))
                    (define (extend-field! f types)
                      (let* ((old (or (assoc f (vector-ref fields i)) (cons f '())))
                             (new (add-types (cdr old) types)))
                        (unless (and (assoc f (vector-ref fields i))
                                     (= (length new) (length (cdr old))))
                          (set! changed #t)
                          (vector-set! fields i
                                       (cons (cons f new)
                                             (ts-filter (lambda (x) (not (equal? (car x) f)))
                                                        (vector-ref fields i)))))))
                    (extend-children! (list t))
                    (when (step-field step)
                      (extend-field! (step-field step) (list t))
                      (when hidden
                        (extend-field! (step-field step) (vector-ref children (sym-index s)))))
                    (when hidden
                      (for-each (lambda (f) (extend-field! (car f) (cdr f)))
                                (vector-ref fields (sym-index s)))
                      (extend-children! (vector-ref children (sym-index s))))))))))
        (when changed (loop))))
    (g-set! g 'variable-children
            (list->vector (ts-map (lambda (l) (ts-filter visible? l)) (vector->list children))))
    (g-set! g 'variable-fields
            (list->vector
             (ts-map (lambda (fl)
                       (ts-sort (map car (ts-filter (lambda (f) (ts-any visible? (cdr f))) fl))
                                string<?))
                     (vector->list fields))))))

(define-public (variable-fields g i) (vector-ref (g-ref g 'variable-fields) i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #(id terminal-entries nonterminal-entries reserved-words lex-state
;;   external-lex-state core eof-gated?) where the terminal entries are an
;; omap symbol -> #(actions reusable?) and the non-terminal entries an omap
;; symbol -> goto action
(define-public (make-state id core)
  (vector id (make-omap) (make-omap) '() 0 0 core #f))

(define-public (state-id s) (vector-ref s 0))
(define-public (state-terminals s) (vector-ref s 1))
(define-public (state-nonterminals s) (vector-ref s 2))
(define-public (state-reserved s) (vector-ref s 3))
(define-public (state-lex s) (vector-ref s 4))
(define-public (state-external-lex s) (vector-ref s 5))
(define-public (state-core s) (vector-ref s 6))
(define-public (state-eof-gated? s) (vector-ref s 7))

(define-public (entry-actions e) (vector-ref e 0))
(define-public (entry-reusable? e) (vector-ref e 1))

(define (state-entry! s symbol)
  (or (om-ref (state-terminals s) symbol)
      (let ((e (vector '() #t)))
        (om-set! (state-terminals s) symbol e)
        e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the parse table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (symbol-name g s)
  (cond ((or (= s sym-end) (= s sym-eont)) "EOF")
        ((external? s) (vector-ref (list-ref (g-ref g 'external-tokens) (sym-index s)) 0))
        ((non-terminal? s) (g-variable-name g (sym-index s)))
        (else (let ((v (vector-ref (g-ref g 'lexical) (sym-index s))))
                (if (= (vector-ref v 1) vt-named) (vector-ref v 0)
                    (string-append "'" (vector-ref v 0) "'"))))))

(define-public (build-parse-table g)
  ;; the parse table (a hash table) and the item set builder
  (let* ((keys (make-key-table))
         (b (make-item-set-builder g keys))
         (pool (ahash-ref b 'pool))
         (states (make-gvec))
         (kernels (make-gvec))
         (preceding (make-gvec))
         (state-ids (make-vector-table))
         (core-ids (make-vector-table))
         (queue '())
         (queue-tail '())
         (non-terminal-extra-states '())
         (production-infos (make-gvec))
         (production-info-ids (make-ahash-table))
         (max-aliased-length 1)
         (conflicts (g-ref g 'conflicts))
         (word (g-ref g 'word)))
    (compute-variable-fields! g)
    (define (enqueue! x)
      (let ((cell (list x)))
        (if (null? queue) (set! queue cell) (set-cdr! queue-tail cell))
        (set! queue-tail cell)))
    (define (add-state! preceding-symbols aux-symbols entries)
      ;; preceding-symbols are in reverse order, sharing their tails
      (let* ((sig (entries-signature entries))
             (old (vector-table-ref state-ids sig)))
        (or old
            (let* ((core-id (vector-table-intern! core-ids (entries-core entries)))
                   (id (gvec-length states)))
              (gvec-push! states (make-state id core-id))
              (gvec-push! kernels entries)
              (gvec-push! preceding preceding-symbols)
              (vector-table-intern! state-ids sig)
              (enqueue! (cons id aux-symbols))
              id))))
    (define (production-id item)
      (or (ahash-ref production-info-ids (item-prod item))
          (let* ((p (item-production g item))
                 (aliases '())
                 (fields '()))
            (do ((i 0 (+ i 1))) ((= i (prod-length p)))
              (let ((step (prod-step p i)))
                (set! aliases (cons (step-alias step) aliases))
                (when (step-field step)
                  (set! fields (add-field fields (step-field step) (cons i #f))))
                (when (and (non-terminal? (step-symbol step))
                           (not (vt-visible? (g-variable-kind g (sym-index (step-symbol step))))))
                  (for-each (lambda (f) (set! fields (add-field fields f (cons i #t))))
                            (variable-fields g (sym-index (step-symbol step)))))))
            (let* ((aliases (let trim ((l aliases)) (if (and (pair? l) (not (car l))) (trim (cdr l)) l)))
                   (info (cons (reverse aliases)
                               (ts-sort fields (lambda (a b) (string<? (car a) (car b))))))
                   (id (or (gvec-index production-infos info)
                           (gvec-push! production-infos info))))
              (when (> (prod-length p) max-aliased-length)
                (set! max-aliased-length (prod-length p)))
              (ahash-set! production-info-ids (item-prod item) id)
              id))))
    (define (add-field fields f loc)
      (let ((h (assoc f fields)))
        (if h
            (begin (set-cdr! h (append (cdr h) (list loc))) fields)
            (append fields (list (list f loc))))))
    (define (auxiliary-node-info entries symbol)
      (cons symbol
            (let loop ((l entries) (acc '()))
              (if (null? l) (reverse acc)
                  (let ((item (vector-ref (car l) 0)))
                    (loop (cdr l)
                          (if (and (not (item-augmented? item))
                                   (eqv? (item-symbol g item) symbol)
                                   (not (= (g-variable-kind g (item-var item)) vt-auxiliary)))
                              (cons (non-terminal (item-var item)) acc)
                              acc)))))))
    (define (handle-conflict! state entries preceding-symbols aux-symbols lookahead rinfo)
      (let* ((entry (om-ref (state-terminals state) lookahead))
             (considered-assoc #f)
             (shift-precs '())
             (conflicting '()))
        (for-each
         (lambda (e)
           (let* ((item (vector-ref e 0))
                  (step (item-step g item)))
             (if step
                 (when (and (> (item-step-index item) 0)
                            (ts-contains? (first-set b (step-symbol step)) lookahead))
                   (unless (item-augmented? item)
                     (set! conflicting (ts-adjoin item conflicting)))
                   (let ((p (cons (item-precedence g item) (non-terminal (item-var item)))))
                     (unless (member p shift-precs) (set! shift-precs (append shift-precs (list p))))))
                 (when (and (ts-contains? (la-get pool (vector-ref e 1)) lookahead)
                            (not (item-augmented? item)))
                   (set! conflicting (ts-adjoin item conflicting))))))
         entries)
        (let* ((actions (entry-actions entry))
               (last-action (ts-last actions)))
          (if (and (eq? (car last-action) 'shift)
                   (let ((v (item-var (car conflicting))))
                     (and (= (g-variable-kind g v) vt-auxiliary)
                          (ts-every (lambda (i) (= (item-var i) v)) conflicting))))
              ;; the ambiguity of a repetition
              (vector-set! entry 0 (append (ts-take actions (- (length actions) 1))
                                           (list (list 'shift (cadr last-action) #t))))
              (begin
                (when (eq? (car last-action) 'shift)
                  (let ((less #f) (equal #f) (more #f))
                    (for-each (lambda (p)
                                (case (compare-precedence g (car p) (list (cdr p))
                                                          (vector-ref rinfo 0) (vector-ref rinfo 1))
                                  ((1) (set! more #t))
                                  ((-1) (set! less #t))
                                  (else (set! equal #t))))
                              shift-precs)
                    (define (right-only?)
                      (and (not (vector-ref rinfo 2)) (not (vector-ref rinfo 4)) (vector-ref rinfo 3)))
                    (define (keep-last!) (vector-set! entry 0 (list (ts-last (entry-actions entry)))))
                    (define (pop!)
                      (vector-set! entry 0 (ts-take (entry-actions entry)
                                                    (- (length (entry-actions entry)) 1)))
                      (set! conflicting (ts-filter (lambda (i) (item-done? g i)) conflicting)))
                    (cond ((and more (not less)) (keep-last!))
                          ((and less (not more))
                           (if (and equal (right-only?)) (keep-last!) (pop!)))
                          ((and (not less) (not more))
                           (set! considered-assoc #t)
                           (cond ((and (vector-ref rinfo 2) (not (vector-ref rinfo 4))
                                       (not (vector-ref rinfo 3)))
                                  (pop!))
                                 ((right-only?) (keep-last!)))))))
                (unless (= (length (entry-actions entry)) 1)
                  (let ((actual (ts-dedup
                                 (ts-sort
                                  (apply append
                                         (ts-map (lambda (item)
                                                   (let ((s (non-terminal (item-var item))))
                                                     (if (= (g-variable-kind g (item-var item)) vt-auxiliary)
                                                         (let ((info (let find ((l (reverse aux-symbols)))
                                                                       (cond ((null? l) #f)
                                                                             ((= (car (car l)) s) (car l))
                                                                             (else (find (cdr l)))))))
                                                           (if info (cdr info) '()))
                                                         (list s))))
                                                 conflicting))
                                  <))))
                    (if (member actual conflicts)
                        (g-set! g 'used-conflicts (cons actual (or (g-ref g 'used-conflicts) '())))
                        (ts-error (string-append
                                   "unresolved conflict for symbol sequence: "
                                   (apply string-append
                                          (ts-map (lambda (s) (string-append (symbol-name g s) " "))
                                                  (reverse preceding-symbols)))
                                   "* " (symbol-name g lookahead)
                                   "; add a conflict for these rules: "
                                   (apply string-append
                                          (ts-map (lambda (s) (string-append (symbol-name g s) " "))
                                                  actual))))))))))))
    (define (add-actions! state-id aux-symbols entries)
      (let* ((state (gvec-ref states state-id))
             (preceding-symbols (gvec-ref preceding state-id))
             (terminal-successors (make-ahash-table))
             (non-terminal-successors (make-ahash-table))
             (conflicted '())
             (reduction-infos (make-ahash-table))
             (aux-info (make-ahash-table))
             (aux-symbols (reverse aux-symbols)))
        (for-each
         (lambda (e)
           (let* ((item (vector-ref e 0))
                  (las (vector-ref e 1))
                  (reserved (vector-ref e 2))
                  (next (item-symbol g item)))
             (if next
                 (let ((successor (item-successor g keys item))
                       (table terminal-successors))
                   (when (non-terminal? next)
                     (set! table non-terminal-successors)
                     (when (= (g-variable-kind g (sym-index next)) vt-auxiliary)
                       (set! aux-symbols
                             (cons (or (ahash-ref aux-info next)
                                       (let ((x (auxiliary-node-info entries next)))
                                         (ahash-set! aux-info next x)
                                         x))
                                   aux-symbols)))
                     (when (and (not (vt-visible? (g-variable-kind g (sym-index next))))
                                (pair? (variable-fields g (sym-index next))))
                       (set! successor (item-with-inherited g keys successor))))
                   (let* ((set (or (ahash-ref table next)
                                   (let ((s (make-item-set))) (ahash-set! table next s) s)))
                          (se (item-set-insert! set successor)))
                     (vector-set! se 1 (la-union pool (vector-ref se 1) las))
                     (vector-set! se 2 (max (vector-ref se 2) reserved))))
                 (let* ((symbol (non-terminal (max 0 (item-var item))))
                        (p (item-production g item))
                        (action (if (item-augmented? item)
                                    (list 'accept)
                                    (list 'reduce symbol (item-step-index item)
                                          (prod-dynamic p) (production-id item))))
                        (prec (item-precedence g item))
                        (assoc (item-associativity g item)))
                   (when (prod-eof? p) (vector-set! state 7 #t))
                   (for-each
                    (lambda (la)
                      (unless (and (prod-eof? p) (not (= la sym-end)))
                        (let* ((te (state-entry! state la))
                               (ri (or (ahash-ref reduction-infos la)
                                       (let ((x (vector #f '() #f #f #f)))
                                         (ahash-set! reduction-infos la x)
                                         x)))
                               (go #t))
                          (if (null? (entry-actions te))
                              (vector-set! te 0 (list action))
                              (case (compare-precedence g prec (list symbol)
                                                        (vector-ref ri 0) (vector-ref ri 1))
                                ((1)
                                 (vector-set! te 0 (list action))
                                 (set! conflicted (ts-remove la conflicted))
                                 (vector-set! ri 0 #f) (vector-set! ri 1 '())
                                 (vector-set! ri 2 #f) (vector-set! ri 3 #f) (vector-set! ri 4 #f))
                                ((0)
                                 (unless (member action (entry-actions te))
                                   (vector-set! te 0 (append (entry-actions te) (list action)))
                                   (set! conflicted (ts-insert la conflicted))))
                                (else (set! go #f))))
                          (when go
                            (vector-set! ri 0 prec)
                            (vector-set! ri 1 (ts-insert symbol (vector-ref ri 1)))
                            (case assoc
                              ((left) (vector-set! ri 2 #t))
                              ((right) (vector-set! ri 3 #t))
                              (else (vector-set! ri 4 #t)))))))
                    (la-get pool las))))))
         entries)
        (set! aux-symbols (let dedup ((l (reverse aux-symbols)) (acc '()))
                            (cond ((null? l) (reverse acc))
                                  ((and (pair? acc) (equal? (car l) (car acc))) (dedup (cdr l) acc))
                                  (else (dedup (cdr l) (cons (car l) acc))))))
        (for-each
         (lambda (kv)
           (let* ((symbol (car kv))
                  (next-id (add-state! (cons symbol preceding-symbols) aux-symbols
                                       (item-set-entries (cdr kv))))
                  (te (state-entry! state symbol)))
             (when (pair? (entry-actions te)) (set! conflicted (ts-insert symbol conflicted)))
             (vector-set! te 0 (append (entry-actions te) (list (list 'shift next-id #f))))))
         (ts-sort (ahash-table->list terminal-successors) (lambda (a b) (< (car a) (car b)))))
        (for-each
         (lambda (kv)
           (let ((next-id (add-state! (cons (car kv) preceding-symbols) aux-symbols
                                      (item-set-entries (cdr kv)))))
             (om-set! (state-nonterminals state) (car kv) (list 'goto next-id))))
         (ts-sort (ahash-table->list non-terminal-successors) (lambda (a b) (< (car a) (car b)))))
        (for-each (lambda (la)
                    (handle-conflict! state entries preceding-symbols aux-symbols la
                                      (ahash-ref reduction-infos la)))
                  conflicted)
        (if (om-has? (state-terminals state) sym-eont)
            (when (> (om-length (state-terminals state)) 1)
              (ts-error "extra rules must have unambiguous endings"))
            (begin
              (for-each (lambda (te)
                          (unless (om-has? (state-terminals state) (car te))
                            (om-set! (state-terminals state) (car te)
                                     (vector (list (list 'shift (cdr te) #f)) #t))))
                        non-terminal-extra-states)
              (for-each (lambda (extra)
                          (if (non-terminal? extra)
                              (om-set! (state-nonterminals state) extra (list 'shift-extra))
                              (unless (om-has? (state-terminals state) extra)
                                (om-set! (state-terminals state) extra
                                         (vector (list (list 'shift-extra)) #t)))))
                        (g-ref g 'extra-symbols))))
        (when word
          (let ((rid (let loop ((l entries) (best #f))
                       (if (null? l) best
                           (let* ((e (car l))
                                  (step (item-step g (vector-ref e 0)))
                                  (v (cond (step (and (= (step-symbol step) word) (step-reserved step)))
                                           ((ts-contains? (la-get pool (vector-ref e 1)) word)
                                            (vector-ref e 2))
                                           (else #f))))
                             (loop (cdr l) (if (and v (or (not best) (> v best))) v best)))))))
            (when rid
              (vector-set! state 3 (vector-ref (g-ref g 'reserved-word-sets) rid)))))))
    ;; the empty production info, the error state and the start state
    (gvec-push! production-infos (cons '() '()))
    (add-state! '() '() '())
    (add-state! '() '()
                (list (vector (make-lr-item g keys -1 -1 0 #f) (la-intern pool (list sym-end)) 0)))
    ;; the states of the non-terminal extras
    (let ((by-first (make-ahash-table)))
      (for-each
       (lambda (extra)
         (when (non-terminal? extra)
           (let ((r (g-var-prods g (sym-index extra))))
             (do ((pid (car r) (+ pid 1))) ((= pid (cdr r)))
               (let* ((first (step-symbol (prod-step (g-production g pid) 0)))
                      (set (or (ahash-ref by-first first)
                               (let ((s (make-item-set))) (ahash-set! by-first first s) s)))
                      (e (item-set-insert! set (make-lr-item g keys (sym-index extra) pid 1 #f))))
                 (vector-set! e 1 (la-insert pool (vector-ref e 1) sym-eont)))))))
       (g-ref g 'extra-symbols))
      (for-each
       (lambda (kv)
         (when (non-terminal? (car kv))
           (ts-error "non-terminal used in a non-terminal extra rule" (symbol-name g (car kv))))
         (let ((id (add-state! '() '() (item-set-entries (cdr kv)))))
           (set! non-terminal-extra-states
                 (append non-terminal-extra-states (list (cons (car kv) id))))))
       (ts-sort (ahash-table->list by-first) (lambda (a b) (< (car a) (car b))))))
    (let loop ()
      (when (pair? queue)
        (let ((e (car queue)))
          (set! queue (cdr queue))
          (add-actions! (car e) (cdr e)
                        (transitive-closure g keys b (gvec-ref kernels (car e))))
          (gvec-set! kernels (car e) #f)
          (loop))))
    (let ((t (make-ahash-table)))
      (ahash-set! t 'states states)
      (ahash-set! t 'production-infos (gvec->vector production-infos))
      (ahash-set! t 'max-aliased-production-length max-aliased-length)
      (ahash-set! t 'symbols '())
      (ahash-set! t 'external-lex-states '())
      (ahash-set! t 'builder b)
      t)))
