;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lists-test.scm
;; DESCRIPTION : tests of lists, abbreviations and iterators
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tests cover
;;
;;   - kernel/library/list.scm: constructors, selectors, sublists, folds,
;;     filtering and partitioning, search and replace, set operations and
;;     association lists;
;;   - kernel/boot/abbrevs.scm: the predicates and helpers (==, nnull?,
;;     in?, keyword conversions...) and the macros (with, when, for, ..);
;;   - kernel/library/iterator.scm: the lazy iterators.
;;
;; The helpers of abbrevs.scm which need an editor (root?, leaf?, go-to,
;; choose-file, the selection predicates) are left to editor tests.

(texmacs-module (check lists-test)
  (:use (check check-lib) (kernel library iterator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors and selectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cons*, rcons and friends build lists at either end; the set-...! macros
;; update a variable in place.
(define (test-constructors)
  (check-group "constructors")
  (check= (cons* 1 2 '(3 4)) '(1 2 3 4))
  (check= (cons* 1 2) '(1 . 2))
  (check= (cons* 'a) 'a)
  (check= (cons* '()) '())
  (check= (rcons '(a b) 'c) '(a b c))
  (check= (rcons '() 'a) '(a))
  (check= (rcons '(a) '(b)) '(a (b)))
  (check= (rcons* '(a) 'b 'c) '(a b c))
  (check= (rcons* '(a)) '(a))
  (check= (rcons* '() 1 2) '(1 2))
  (check= (let ((l '(b))) (set-cons! l 'a) l) '(a b))
  (check= (let ((l '())) (set-cons! l 'a) l) '(a))
  (check= (let ((l '(a))) (set-rcons! l 'b) l) '(a b))
  (check= (let ((l '())) (set-rcons! l 'a) (set-rcons! l 'b) l) '(a b))
  (check= (list-concatenate '((a b) () (c) (d e))) '(a b c d e))
  (check= (list-concatenate '()) '())
  (check= (list-intersperse '(a b c) 'x) '(a x b x c))
  (check= (list-intersperse '(a) 'x) '(a))
  (check= (list-intersperse '() 'x) '()))

;; first..tenth, and the selectors at the end of a list (cAr, cDr...).
(define (test-selectors)
  (check-group "selectors")
  (let ((l '(1 2 3 4 5 6 7 8 9 10 11)))
    (check= (first l) 1)
    (check= (second l) 2)
    (check= (third l) 3)
    (check= (fourth l) 4)
    (check= (fifth l) 5)
    (check= (sixth l) 6)
    (check= (seventh l) 7)
    (check= (eighth l) 8)
    (check= (ninth l) 9)
    (check= (tenth l) 10))
  (check= (first '(a)) 'a)
  (check-error (first '()) #t)
  (check-error (tenth '(1 2 3)) #t)
  (check= (cAr '(a b c)) 'c)
  (check= (cAr '(a)) 'a)
  (check= (cAr '(a b . c)) 'b)
  (check= (cDr '(a b c)) '(a b))
  (check= (cDr '(a)) '())
  (check-error (cDr '()) #t)
  (check= (cADr '(a b c)) 'b)
  (check-error (cADr '(a)) #t)
  (check= (cDDr '(a b c)) '(a))
  (check= (cDDr '(a b)) '())
  ;; cDDDr removes three elements, although its docstring says two
  (check= (cDDDr '(a b c d)) '(a))
  (check= (cDDDr '(a b c)) '())
  (check= (cDdr '(a b c d)) '(b c))
  (check= (cDdr '(a b)) '())
  (check= (cDddr '(a b c d e)) '(c d))
  (check= (cDddr '(a b c)) '())
  (check= (cdAr '(a b (c d e))) '(d e))
  (check= (last '(a b c)) 'c)
  (check= (but-last '(a b c)) '(a b))
  (check= (receive (a d) (car+cdr '(x y z)) (list a d)) '(x (y z)))
  (check= (receive (a d) (car+cdr '(x . y)) (list a d)) '(x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sublists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The bounds are those of list-head and list-tail: out of range is an error.
(define (test-sublists)
  (check-group "sublists")
  (check= (list-take '(a b c d) 2) '(a b))
  (check= (list-take '(a b) 0) '())
  (check= (list-take '(a b) 2) '(a b))
  (check-error (list-take '(a b) 3) #t)
  (check= (list-drop '(a b c d) 2) '(c d))
  (check= (list-drop '(a b) 2) '())
  (check-error (list-drop '(a b) 3) #t)
  (check= (list-take-right '(a b c d) 1) '(d))
  (check= (list-take-right '(a b c d) 0) '())
  (check= (list-take-right '(a b c d) 4) '(a b c d))
  (check-error (list-take-right '(a b) 3) #t)
  (check= (list-drop-right '(a b c d) 1) '(a b c))
  (check= (list-drop-right '(a b c d) 4) '())
  (check= (list-drop-right '(a b) 0) '(a b))
  (check-error (list-drop-right '(a b) 3) #t)
  (check= (sublist '(a b c d) 1 3) '(b c))
  (check= (sublist '(a b c d) 0 4) '(a b c d))
  (check= (sublist '(a b c d) 2 2) '())
  (check= (sublist '() 0 0) '())
  (check-error (sublist '(a b) 1 3) #t)
  (check-error (sublist '(a b) 3 3) #t)
  (check= (list-delete '(a b a c a) 'a) '(b c))
  (check= (list-delete '(a b) 'z) '(a b))
  (check= (list-delete '() 'a) '())
  (check= (list-delete '((1) 2 (1)) '(1)) '(2)))

;; A right shift by n moves the last n elements to the front.
(define (test-circulate)
  (check-group "circulating lists")
  (check= (list-circulate-right '(a b c d) 1) '(d a b c))
  (check= (list-circulate-right '(a b c d) 2) '(c d a b))
  (check= (list-circulate-right '(a b c d) 0) '(a b c d))
  (check= (list-circulate-right '(a b c d) 4) '(a b c d))
  (check= (list-circulate-right '(a) 1) '(a))
  (check-error (list-circulate-right '(a b) 3) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folds and maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The folds follow SRFI-1: with several lists they stop at the shortest,
;; and kons receives the elements first and the accumulator last.
(define (test-folds)
  (check-group "folds")
  (check= (list-fold cons '() '(1 2 3)) '(3 2 1))
  (check= (list-fold + 0 '(1 2 3)) 6)
  (check= (list-fold cons 'z '()) 'z)
  (check= (list-fold (lambda (a b acc) (cons (+ a b) acc)) '() '(1 2) '(10 20 30))
          '(22 11))
  (check= (list-fold (lambda (a b acc) (cons a acc)) 'z '() '(1 2)) 'z)
  (check= (list-fold-right cons '() '(1 2 3)) '(1 2 3))
  (check= (list-fold-right list 'z '(1 2)) '(1 (2 z)))
  (check= (list-fold-right cons 'z '()) 'z)
  (check= (list-fold-right (lambda (a b acc) (cons (list a b) acc)) '()
                           '(1 2 3) '(x y))
          '((1 x) (2 y)))
  (check= (pair-fold cons '() '(a b c)) '((c) (b c) (a b c)))
  (check= (pair-fold cons 'z '()) 'z)
  (check= (pair-fold (lambda (p q acc) (cons (append p q) acc)) '()
                     '(a b) '(x y z))
          '((b y z) (a b x y z)))
  (check= (pair-fold-right cons '() '(a b c)) '((a b c) (b c) (c)))
  (check= (pair-fold-right cons 'z '()) 'z)
  (check= (pair-fold-right (lambda (p q acc) (cons (list (car p) (car q)) acc))
                           '() '(a b c) '(x y))
          '((a x) (b y)))
  (check= (append-map (lambda (x) (list x x)) '(1 2)) '(1 1 2 2))
  (check= (append-map (lambda (x) '()) '(1 2)) '())
  (check= (append-map list '()) '())
  (check= (append-map list '(1 2) '(a b)) '(1 a 2 b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtering and partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note the argument order of this library: the list comes first, the
;; predicate second (except for forall?, exists?, list-any and list-every).
(define (test-filtering)
  (check-group "filtering")
  (check= (list-filter '(1 2 3 4) even?) '(2 4))
  (check= (list-filter '(1 3) even?) '())
  (check= (list-filter '() even?) '())
  (check= (filter-map (lambda (x) (and (even? x) (* x x))) '(1 2 3 4)) '(4 16))
  (check= (filter-map (lambda (x y) (and (< x y) (+ x y))) '(1 5 2) '(3 4 7))
          '(4 9))
  (check= (filter-map identity '()) '())
  (check= (receive (in out) (list-partition '(1 2 3 4 5) odd?) (list in out))
          '((1 3 5) (2 4)))
  (check= (receive (in out) (list-partition '() odd?) (list in out)) '(() ()))
  (check= (receive (in out) (list-partition '(2 4) odd?) (list in out))
          '(() (2 4)))
  (check= (receive (h t) (list-break '(1 2 3 4) (lambda (x) (> x 2))) (list h t))
          '((1 2) (3 4)))
  (check= (receive (h t) (list-break '(1 2) (lambda (x) (> x 5))) (list h t))
          '((1 2) ()))
  (check= (receive (h t) (list-break '(7 1) (lambda (x) (> x 5))) (list h t))
          '(() (7 1)))
  (check= (receive (h t) (list-break '() odd?) (list h t)) '(() ()))
  (check= (receive (h t) (list-span '(1 3 4 5) odd?) (list h t))
          '((1 3) (4 5)))
  (check= (receive (h t) (list-span '(1 3) odd?) (list h t)) '((1 3) ()))
  (check= (list-drop-while '(1 3 4 5) odd?) '(4 5))
  (check= (list-drop-while '(1 3) odd?) '())
  (check= (list-drop-while '(2 3) odd?) '(2 3))
  (check= (list-drop-while '() odd?) '()))

;; list-scatter splits at separators, which are dropped or, with keep?,
;; kept at the start of the following piece.
(define (test-scatter)
  (check-group "scatter")
  (let ((sep? (lambda (x) (== x '/))))
    (check= (list-scatter '(a / b c / d) sep? #f) '((a) (b c) (d)))
    (check= (list-scatter '(a / b c / d) sep? #t) '((a) (/ b c) (/ d)))
    (check= (list-scatter '(a b) sep? #f) '((a b)))
    (check= (list-scatter '() sep? #f) '(()))
    (check= (list-scatter '(/) sep? #f) '(() ()))
    (check= (list-scatter '(a /) sep? #f) '((a) ()))
    (check= (list-scatter '(a /) sep? #t) '((a) (/)))
    (check= (list-scatter '(/ /) sep? #f) '(() () ()))))

;; Conversions between flat lists and association lists, and the
;; quantifiers forall? and exists?.
(define (test-assoc-conversions)
  (check-group "assoc conversions and quantifiers")
  (check= (list->assoc '(a 1 b 2)) '((a . 1) (b . 2)))
  (check= (list->assoc '(a 1 b)) '((a . 1)))
  (check= (list->assoc '()) '())
  (check= (list->assoc '(a)) '())
  (check= (assoc->list '((a . 1) (b . 2))) '(a 1 b 2))
  (check= (assoc->list '((a 1 2))) '(a (1 2)))
  (check= (assoc->list '()) '())
  (check= (list->assoc (assoc->list '((x . 1) (y . 2)))) '((x . 1) (y . 2)))
  (check-true (forall? even? '(2 4)))
  (check-false (forall? even? '(2 3)))
  (check-true (forall? even? '()))
  (check-false (exists? even? '(1 3)))
  (check-false (exists? even? '()))
  (check-true (exists? even? '(1 2)))
  ;; exists? returns the value of the predicate, like list-any
  (check= (exists? (lambda (x) (and (> x 1) (* 10 x))) '(1 2 3)) 20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-search)
  (check-group "search")
  (check= (list-find '(1 2 3 4) even?) 2)
  (check-false (list-find '(1 3) even?))
  (check-false (list-find '() even?))
  ;; a #f element which matches cannot be told from a failure
  (check-false (list-find '(1 #f) not))
  (check= (list-find-index '(a b c) (lambda (x) (== x 'c))) 2)
  (check= (list-find-index '(a b c) symbol?) 0)
  (check-false (list-find-index '(a b) number?))
  (check-false (list-find-index '() number?))
  ;; list-any and list-every take the predicate first, and return the
  ;; value of the predicate
  (check-true (list-any even? '(1 2 3)))
  (check-false (list-any even? '(1 3)))
  (check-false (list-any even? '()))
  (check= (list-any (lambda (x) (and (> x 1) (* 10 x))) '(1 2 3)) 20)
  (check-true (list-any < '(5 1) '(2 3)))
  (check-false (list-any < '(5 4) '(2 3 9)))
  (check-true (list-any > '(1 5) '(2 3 4)))
  (check-false (list-any < '() '(1)))
  (check-true (list-every even? '(2 4)))
  (check-false (list-every even? '(2 3 4)))
  (check-true (list-every even? '()))
  (check= (list-every (lambda (x) (* 10 x)) '(1 2 3)) 30)
  (check-true (list-every < '(1 2) '(2 3 0)))
  (check-false (list-every < '(1 5) '(2 3)))
  (check-true (list-every < '() '(1))))

(define (test-replace)
  (check-group "replace and common parts")
  (check-true (list-starts? '(a b c) '(a b)))
  (check-true (list-starts? '(a b) '(a b)))
  (check-true (list-starts? '(a b) '()))
  (check-true (list-starts? '() '()))
  (check-false (list-starts? '(a) '(a b)))
  (check-false (list-starts? '(a c) '(a b)))
  (check-true (list-starts? '((1) b) '((1))))
  (check= (list-replace '(a b c a b) '(a b) '(x)) '(x c x))
  (check= (list-replace '(a b c) '(b) '(y z)) '(a y z c))
  (check= (list-replace '(a b c) '(b) '()) '(a c))
  (check= (list-replace '(a b c) '(d) '(x)) '(a b c))
  (check= (list-replace '() '(a) '(x)) '())
  (check= (list-replace '(a a a) '(a a) '(b)) '(b a))
  ;; NOTE: an empty @what never stops (each step replaces it again), so it
  ;; is not tested
  (check= (list-common-left '(a b c) '(a b d)) 2)
  (check= (list-common-left '(a b) '(a b c)) 2)
  (check= (list-common-left '(x) '(y)) 0)
  (check= (list-common-left '() '(a)) 0)
  (check= (list-common-right '(x b c) '(y b c)) 2)
  (check= (list-common-right '(a b) '(a c)) 0)
  (check= (list-common-right '() '()) 0)
  (check= (list-common '(a b c) '(a b d)) '(a b))
  (check= (list-common '(a) '(b)) '())
  (check= (list-common '() '(a)) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The set operations keep the order of their first argument and compare
;; with equal? (through hash tables); the order of ahash-set->list is
;; unspecified, so it is checked up to permutation.
(define (test-sets)
  (check-group "set operations")
  (check= (list-remove '(a b a c) 'a) '(b c))
  (check= (list-remove '() 'a) '())
  (check= (list-remove '((1) 2) '(1)) '(2))
  (check= (list-remove-duplicates '(a b a c b)) '(a b c))
  (check= (list-remove-duplicates '()) '())
  (check= (list-remove-duplicates '("x" "x" (1) (1))) '("x" (1)))
  (check-true (list-permutation? (ahash-set->list (list->ahash-set '(a b c)))
                                 '(a b c)))
  (check= (ahash-set->list (list->ahash-set '())) '())
  (check= (length (ahash-set->list (list->ahash-set '(a a b)))) 2)
  (check-true (ahash-ref (list->ahash-set '(a b)) 'b))
  (check-false (ahash-ref (list->ahash-set '(a b)) 'c))
  (check= (list-intersection '(a b c d) '(d b)) '(b d))
  (check= (list-intersection '(a b) '()) '())
  (check= (list-intersection '() '(a)) '())
  (check= (list-intersection '(a a b) '(a)) '(a a))
  (check= (list-difference '(a b c d) '(d b)) '(a c))
  (check= (list-difference '(a b) '()) '(a b))
  (check= (list-difference '() '(a)) '())
  (check= (list-difference '("s" (1)) '((1))) '("s"))
  (check= (list-union '(a b) '(b c) '(a d)) '(a b c d))
  (check= (list-union '(a a)) '(a))
  (check= (list-union) '())
  (check= (list-union '() '()) '())
  (check-true (list-permutation? '(a b c) '(c a b)))
  (check-true (list-permutation? '() '()))
  (check-false (list-permutation? '(a b) '(a)))
  (check-false (list-permutation? '(a) '(a b)))
  ;; multiplicities are ignored: these are permutations as sets
  (check-true (list-permutation? '(a a b) '(b a))))

;; list-or returns the deciding element, like or; list-and returns #f or
;; #t (the empty tail gives #t), not the last element as and would.
(define (test-booleans)
  (check-group "boolean lists")
  (check-true (list-and '()))
  (check= (list-and '(1 2)) #t)
  (check-false (list-and '(1 #f 2)))
  (check-false (list-or '()))
  (check= (list-or '(#f 3 4)) 3)
  (check-false (list-or '(#f #f)))
  (check-true (list-length=2? '(a b)))
  (check-false (list-length=2? '(a)))
  (check-false (list-length=2? '(a b c)))
  (check-false (list-length=2? '(a . b)))
  (check-false (list-length=2? '(a b . c)))
  (check-false (list-length=2? '()))
  (check-false (list-length=2? 'a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Association lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-assoc)
  (check-group "association lists")
  (let ((l '((a . 1) (b . 2) (a . 3))))
    ;; the first binding of a key wins, or the last one for the * variant
    (check= (assoc-remove-duplicates l) '((a . 1) (b . 2)))
    (check= (assoc-remove-duplicates* l) '((b . 2) (a . 3))))
  (check= (assoc-remove-duplicates '()) '())
  (check= (assoc-remove-duplicates* '()) '())
  ;; entries of l1 whose key is not bound in l2
  (check= (assoc-difference '((a . 1) (b . 2) (c . 3)) '((b . 9)))
          '((a . 1) (c . 3)))
  (check= (assoc-difference '((a . 1)) '()) '((a . 1)))
  (check= (assoc-difference '() '((a . 1))) '())
  ;; entries of l2 which are new or changed with respect to l1
  (check= (assoc-delta '((a . 1) (b . 2)) '((a . 1) (b . 3) (c . 4)))
          '((b . 3) (c . 4)))
  (check= (assoc-delta '((a . 1)) '((a . 1))) '())
  (check= (assoc-delta '() '((a . 1))) '((a . 1)))
  ;; entries of l1 whose key is not in the list of keys l2
  (check= (assoc-exclude '((a . 1) (b . 2) (c . 3)) '(a c)) '((b . 2)))
  (check= (assoc-exclude '((a . 1)) '()) '((a . 1)))
  (check= (assoc-exclude '() '(a)) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbreviations: predicates and small helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-negated-predicates)
  (check-group "negated predicates")
  (check-true (== '(a "b" 1) (list 'a "b" 1)))
  (check-false (== 1 2))
  (check-true (!= 1 2))
  (check-false (!= "a" "a"))
  (check-true (nsymbol? "a"))
  (check-false (nsymbol? 'a))
  (check-true (nstring? 'a))
  (check-false (nstring? "a"))
  (check-true (nnull? '(a)))
  (check-false (nnull? '()))
  (check-true (npair? '()))
  (check-false (npair? '(a . b)))
  (check-true (nlist? '(a . b)))
  (check-false (nlist? '()))
  (check= (nnot 5) #t)
  (check= (nnot #f) #f)
  (check= (let ((b #f)) (toggle! b) b) #t)
  (check= (let ((b 3)) (toggle! b) b) #f)
  (check= (safe-car '(a b)) 'a)
  (check-false (safe-car '()))
  (check-false (safe-car 'a))
  (check= (safe-cdr '(a b)) '(b))
  (check-false (safe-cdr '())))

;; list-1? and list>0? only look at the first pairs, the others need a
;; proper list.
(define (test-length-predicates)
  (check-group "length predicates")
  (check-true (list-1? '(a)))
  (check-false (list-1? '()))
  (check-false (list-1? '(a b)))
  (check-false (list-1? 'a))
  (check-true (nlist-1? '()))
  (check-false (nlist-1? '(a)))
  (check-true (list-2? '(a b)))
  (check-false (list-2? '(a b . c)))
  (check-false (list-2? '(a)))
  (check-true (nlist-2? '(a b c)))
  (check-true (list-3? '(a b c)))
  (check-false (list-3? '(a b)))
  (check-true (nlist-3? 'x))
  (check-true (list-4? '(a b c d)))
  (check-false (list-4? '(a b c)))
  (check-false (nlist-4? '(1 2 3 4)))
  (check-true (list>0? '(a)))
  (check-false (list>0? '()))
  (check-false (list>0? '(a . b)))
  (check-true (nlist>0? '()))
  (check-true (list>1? '(a b)))
  (check-false (list>1? '(a)))
  (check-false (list>1? '(a b . c)))
  (check-true (nlist>1? '(a))))

(define (test-membership)
  (check-group "membership")
  (check-true (in? 'b '(a b c)))
  (check= (in? 'b '(a b c)) #t)
  (check-false (in? 'd '(a b c)))
  (check-false (in? 'a '()))
  (check-true (in? '(1) '((1) 2)))
  (check-true (nin? 'd '(a b)))
  (check-false (nin? 'a '(a b)))
  (check= (cons-new 'a '(b c)) '(a b c))
  (check= (cons-new 'b '(b c)) '(b c))
  (check= (cons-new 'a '()) '(a)))

(define (test-helpers)
  (check-group "helpers")
  (check-true (always?))
  (check-true (always? 1 2))
  (check-false (never?))
  (check-false (never? #t))
  (check-true (true? #f))
  (check-false (false? #t))
  (check= (identity '(a)) '(a))
  (check= (map identity '(1 2)) '(1 2))
  (check-true ((negate even?) 3))
  (check-false ((negate even?) 2))
  (check-true ((negate <) 2 1))
  (check-false ((negate null?) '()))
  (check= (begin (ignore 1 2 3) 'done) 'done)
  (check= (keyword->string :foo) "foo")
  (check= (string->keyword "foo") :foo)
  (check= (keyword->string (string->keyword "a-b")) "a-b")
  (check= (number->keyword 3) :%3)
  (check= (keyword->number :%3) 3)
  (check= (keyword->number (number->keyword 42)) 42)
  (check= (sourcify 3) 3)
  (check= (sourcify "s") "s"))

;; save-object writes a value to a file, which load-object reads back; a
;; file which cannot be read gives the empty list.
(define (test-objects)
  (check-group "save-object and load-object")
  (let ((u (url-temp))
        (v '(a "b" (1 2.5) #t)))
    (check= (begin (save-object u v) (load-object u)) v)
    (check= (begin (save-object u '()) (load-object u)) '())
    (check= (begin (save-object u 7) (load-object u)) 7)
    (system-remove u)
    (check-false (url-exists? u))
    (check= (load-object u) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbreviations: programming constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define with-global-test-var 'outer)
(define (with-global-test-get) with-global-test-var)

(define (test-binding-macros)
  (check-group "binding macros")
  (check= (when (> 2 1) 'a 'b) 'b)
  (check= (let ((n 0)) (when #f (set! n 1)) n) 0)
  (check= (unless #f 'a 'b) 'b)
  (check= (let ((n 0)) (unless #t (set! n 1)) n) 0)
  (check= (with x 3 (* x x)) 9)
  (check= (with x 1 (with x (+ x 1) x)) 2)
  ;; with a list of variables, with destructures its value
  (check= (with (a b) '(1 2) (+ a b)) 3)
  (check= (with (a . r) '(1 2 3) (list a r)) '(1 (2 3)))
  (check= (with () '() 'empty) 'empty)
  (check-error (with (a b) '(1) a) #t)
  (check= (with-define (sq x) (* x x) (sq 5)) 25)
  (check= (with-define (k) 7 (+ (k) 1)) 8)
  ;; with-global sets a global variable during the body only
  (check= (with-global with-global-test-var 'inner (with-global-test-get))
          'inner)
  (check= (with-global-test-get) 'outer)
  (check= (and-with x (assq 'b '((a . 1) (b . 2))) (cdr x)) 2)
  (check-false (and-with x (assq 'c '((a . 1))) (cdr x)))
  (check= (let ((n 0)) (list (with-result (+ n 1) (set! n 10)) n)) '(1 10)))

;; .. excludes its end and ... includes it.
(define (test-ranges)
  (check-group "ranges")
  (check= (.. 0 3) '(0 1 2))
  (check= (.. 3 3) '())
  (check= (.. 5 3) '())
  (check= (.. 0 10 3) '(0 3 6 9))
  (check= (.. 0 9 3) '(0 3 6))
  (check= (.. -2 1) '(-2 -1 0))
  (check= (... 0 3) '(0 1 2 3))
  (check= (... 3 3) '(3))
  (check= (... 4 3) '())
  (check= (... 0 9 3) '(0 3 6 9)))

;; The forms of for: over a list, over a range, with a step (positive or
;; negative), and with a step and a comparison.
(define (test-loops)
  (check-group "loops")
  (let ((acc '()))
    (for (x '(a b c)) (set! acc (cons x acc)))
    (check= (reverse acc) '(a b c)))
  (let ((n 0))
    (for (x '()) (set! n (+ n 1)))
    (check= n 0))
  (let ((acc '()))
    (for (i 0 4) (set! acc (cons i acc)))
    (check= (reverse acc) '(0 1 2 3)))
  (let ((n 0))
    (for (i 5 5) (set! n (+ n 1)))
    (check= n 0))
  (let ((acc '()))
    (for (i 0 10 3) (set! acc (cons i acc)))
    (check= (reverse acc) '(0 3 6 9)))
  (let ((acc '()))
    (for (i 5 0 -2) (set! acc (cons i acc)))
    (check= (reverse acc) '(5 3 1)))
  (let ((acc '()))
    (for (i 0 6 2 <=) (set! acc (cons i acc)))
    (check= (reverse acc) '(0 2 4 6)))
  (let ((acc '()))
    (for (i 1 100 10 (lambda (i end) (< (* i i) end)))
      (set! acc (cons i acc)))
    (check= (reverse acc) '(1)))
  (let ((n 0))
    (repeat 4 (set! n (+ n 1)))
    (check= n 4))
  (let ((n 0))
    (repeat 0 (set! n (+ n 1)))
    (check= n 0))
  (let ((n 1))
    (twice (set! n (* n 3)))
    (check= n 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An iterator is #f when empty, or a thunk which gives the pair of the
;; current value and the next iterator.
(define (test-iterators)
  (check-group "iterators")
  (check= (iterator->list (range 0 4)) '(0 1 2 3))
  (check-false (range 3 3))
  (check-false (range 4 3))
  (check= (iterator->list (range 3 3)) '())
  (check= (iterator->list (list->iterator '(a b c))) '(a b c))
  (check-false (list->iterator '()))
  (check= (iterator->list #f) '())
  (check= (iterator-value (range 5 9)) 5)
  (check= (iterator-value (iterator-next (range 5 9))) 6)
  (check-false (iterator-value #f))
  (check-false (iterator-next #f))
  (check-false (iterator-next (range 0 1)))
  (check= (iterator->list (iterator-append (range 0 2) #f (list->iterator '(a))))
          '(0 1 a))
  (check-false (iterator-append))
  (check-false (iterator-append #f #f))
  (check= (iterator->list (iterator-append (range 0 2))) '(0 1))
  ;; iterators are lazy: an infinite one can be read from
  (let ((from (lambda (n)
                (let loop ((n n))
                  (lambda () (cons n (loop (+ n 1))))))))
    (check= (iterator-value (iterator-next (iterator-next (from 7)))) 9))
  (let ((it (list->iterator '(a b))))
    (check= (list (iterator-read! it) (iterator-read! it) (iterator-read! it)
                  it)
            '(a b #f #f)))
  (let ((acc '()))
    (iterator-apply (range 0 3) (lambda (x) (set! acc (cons x acc))))
    (check= acc '(2 1 0)))
  (let ((n 0))
    (iterator-apply #f (lambda (x) (set! n 1)))
    (check= n 0))
  (let ((acc '()))
    (for-in (x (list->iterator '(1 2 3))) (set! acc (cons (* 10 x) acc)))
    (check= acc '(30 20 10))))

;; iterator-filter and extract skip the values which do not match.
(define (test-iterator-filter)
  (check-group "iterator filter")
  (check-false (iterator-filter (range 1 4) (lambda (x) (> x 5))))
  (check-false (iterator-filter #f even?))
  (check= (iterator->list (iterator-filter (range 1 2) odd?)) '(1))
  (check= (iterator-value (iterator-filter (range 1 9) even?)) 2)
  (check= (iterator-value (extract x (range 1 9) (> x 4))) 5)
  ;; FIXME: iterator-filter only skips the values before the first match
  ;; and returns the rest of the iterator unfiltered, so that
  ;; (iterator->list (iterator-filter (range 0 6) even?)) is (0 1 2 3 4 5)
  ;; instead of (0 2 4), and likewise for extract; not checked
  (check= (iterator-value (iterator-filter (list->iterator '(a 1 b)) number?))
          1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lists-test-failures)
  (check-suite "lists")
  (test-constructors)
  (test-selectors)
  (test-sublists)
  (test-circulate)
  (test-folds)
  (test-filtering)
  (test-scatter)
  (test-assoc-conversions)
  (test-search)
  (test-replace)
  (test-sets)
  (test-booleans)
  (test-assoc)
  (test-negated-predicates)
  (test-length-predicates)
  (test-membership)
  (test-helpers)
  (test-objects)
  (test-binding-macros)
  (test-ranges)
  (test-loops)
  (test-iterators)
  (test-iterator-filter)
  (check-end))
