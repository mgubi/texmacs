;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : trees-test.scm
;; DESCRIPTION : tests of trees and content
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The document API of TeXmacs on the Scheme side:
;;
;;   - kernel/library/tree.scm and utils/library/tree.scm: access to and
;;     modification of trees (tree-ref, tree-set!, tree-insert!, ...);
;;   - kernel/library/content.scm: the tm- functions, which accept a
;;     string, a tree or a Scheme tree alike;
;;   - kernel/library/patch.scm: modifications and patches.
;;
;; All checks work on detached trees made with stree->tree: no buffer is
;; open. Functions which need the editor (the cursor, the focus, the
;; selection, tree-start, tree-go-to, tree-set-diff, ...) are left out.
;; Trees are compared through tree->stree.
;;
;; Detached trees have two properties which the checks rely on:
;;
;;   - a tree is a reference to a shared node, so that a change made in
;;     place through a child (tree-set!, tree-insert!, tree-remove!,
;;     tree-assign-node!) is seen through its parent;
;;   - a detached tree has no position: tree->path, tree-up, tree-index
;;     give #f, and tree-assign only rebinds the reference which it is
;;     given (it cannot reach the parent), so that tree-assign! and the
;;     one argument tree-set! change the variable and not the parent.

(texmacs-module (check trees-test)
  (:use (check check-lib)))

(define (st t) (tree->stree t))
(define (sts l) (map tree->stree l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction and conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stree->tree and tree->stree are inverse to each other; the various
;; constructors agree; the predicates on trees are false on strings.
(define (test-conversion)
  (check-group "conversion")
  (with s '(document (concat "a" (frac "x" (sqrt "y"))) "" (with "color" "red" "b"))
    (check= (tree->stree (stree->tree s)) s))
  (check= (tree->stree (stree->tree "abc")) "abc")
  (check= (st (tree 'frac "a" "b")) '(frac "a" "b"))
  (check= (st (tree "abc")) "abc")
  (check= (st (tm->tree '(em "x"))) '(em "x"))
  (check= (st (tm->tree "x")) "x")
  (check= (st (string->tree "xy")) "xy")
  (check= (tree->string (stree->tree "xy")) "xy")
  (check= (tree->symbol (stree->tree "abc")) 'abc)
  (check-true (tree? (stree->tree '(em "x"))))
  (check-false (tree? '(em "x")))
  (check-true (atomic-tree? (stree->tree "a")))
  (check-false (atomic-tree? "a"))
  (check-false (atomic-tree? (stree->tree '(em "a"))))
  (check-true (compound-tree? (stree->tree '(em "a"))))
  (check-false (compound-tree? '(em "a")))
  (check-true (stree? '(a "b" (c))))
  (check-true (stree? "x"))
  (check-false (stree? '(a b)))
  (check-false (stree? '("a" "b")))
  (check-false (stree? 3)))

;; numbers stored in atomic trees
(define (test-numbers)
  (check-group "numbers")
  (check= (tree-number? (stree->tree "12.5")) 12.5)
  (check-false (tree-number? (stree->tree "abc")))
  (check-true (tree-integer? (stree->tree "12")))
  (check-false (tree-integer? (stree->tree "12.5")))
  (check-false (tree-integer? (stree->tree '(em "1"))))
  (check= (tree->number (stree->tree "-3")) -3)
  (check= (tree->number (stree->tree '(frac "a" "b"))) 0))

;; trees with the same content are equal?, but distinct nodes are not
;; tree-eq?, and tree-copy makes a distinct node
(define (test-identity)
  (check-group "identity")
  (let* ((t (stree->tree '(concat "a" (frac "x" "y"))))
         (u (stree->tree '(concat "a" (frac "x" "y"))))
         (c (tree-copy t)))
    (check-true (equal? t u))
    (check-true (== t u))
    (check-false (tree-eq? t u))
    (check-true (tree-eq? (tree-ref t 1) (tree-ref t 1)))
    (check-true (tree-eq? t t))
    (check-false (tree-eq? t c))
    (check= (st c) (st t))
    ;; a copy does not share its nodes with the original
    (tree-set! (tree-ref c 1) 0 "z")
    (check= (st t) '(concat "a" (frac "x" "y")))
    (check= (st c) '(concat "a" (frac "z" "y")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the basic accessors of the glue on a concrete tree
(define (test-access)
  (check-group "access")
  (with t (stree->tree '(concat "ab" (frac "x" (sqrt "y")) "cd"))
    (check= (tree-label t) 'concat)
    (check= (tree-arity t) 3)
    (check= (sts (tree-children t)) '("ab" (frac "x" (sqrt "y")) "cd"))
    (check= (st (tree-child-ref t 2)) "cd")
    (check-true (tree-atomic? (tree-ref t 0)))
    (check-true (tree-compound? (tree-ref t 1)))
    (check= (tree-label (tree-ref t 1 1)) 'sqrt)
    (check= (st (subtree t '(1 1 0))) "y")
    (check= (st (subtree t '())) (st t))
    (check= (map (lambda (x) (if (tree? x) (st x) x)) (tree->list t))
            '(concat "ab" (frac "x" (sqrt "y")) "cd"))
    (check= (car (tree-explode t)) 'concat)
    (check= (sts (cdr (tree-explode t))) (sts (tree-children t)))
    (check= (tree-explode (stree->tree "ab")) "ab")))

;; tree-ref with integers, :first, :last and labels, and its #f results
(define (test-tree-ref)
  (check-group "tree-ref")
  (with t (stree->tree '(concat "ab" (frac "x" (sqrt "y")) "cd"))
    (check-true (tree-eq? (tree-ref t) t))
    (check= (st (tree-ref t 0)) "ab")
    (check= (st (tree-ref t 1 1 0)) "y")
    (check= (st (tree-ref t :first)) "ab")
    (check= (st (tree-ref t :last)) "cd")
    (check= (st (tree-ref t 1 :last)) '(sqrt "y"))
    (check= (st (tree-ref t 'frac)) '(frac "x" (sqrt "y")))
    (check= (st (tree-ref t 'frac 1 0)) "y")
    (check= (st (tree-ref t 'frac 'sqrt 0)) "y")
    (check-false (tree-ref t 3))
    (check-false (tree-ref t -1))
    (check-false (tree-ref t 0 0))
    (check-false (tree-ref t 'nosuch))
    (check-false (tree-ref t 'nosuch 0))
    (check-false (tree-ref (tree-ref t 0) :last))
    (check-false (tree-ref "abc" 0))
    (check-false (tree-ref '(frac "a" "b") 0))))

;; tree-is?, tree-in? and tree-func? look at the labels of subtrees
(define (test-predicates)
  (check-group "predicates")
  (with t (stree->tree '(concat "a" (frac "x" (sqrt "y"))))
    (check-true (tree-is? t 'concat))
    (check-true (tree-is? t 1 'frac))
    (check-true (tree-is? t 1 1 'sqrt))
    (check-false (tree-is? t 1 'sqrt))
    (check-false (tree-is? t 5 'frac))
    (check-true (tree-is? t 0 'string))
    (check-true (tree-in? t '(document concat)))
    (check-true (tree-in? t 1 '(frac tfrac)))
    (check-false (tree-in? t 1 '(sqrt)))
    (check-true (tree-func? (tree-ref t 1) 'frac))
    (check-true (tree-func? (tree-ref t 1) 'frac 2))
    (check-false (tree-func? (tree-ref t 1) 'frac 3))
    (check-false (tree-func? (tree-ref t 0) 'string))
    (check-false (tree-func? '(frac "a" "b") 'frac))))

;; functions of the glue which build new trees out of old ones
(define (test-building)
  (check-group "building")
  (with t (stree->tree '(concat "a" "b" "c"))
    (check= (st (tree-range t 1 3)) '(concat "b" "c"))
    (check= (st (tree-range t 0 0)) '(concat))
    (check= (st (tree-append t (stree->tree '(concat "d"))))
            '(concat "a" "b" "c" "d"))
    (check= (st (tree-child-insert t 1 "x")) '(concat "a" "x" "b" "c"))
    (check= (st (tree-child-insert t 3 "x")) '(concat "a" "b" "c" "x"))
    ;; none of them changes its argument
    (check= (st t) '(concat "a" "b" "c"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching and mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-search returns the matching subtrees in document order, and
;; tree-search-indices their relative paths, in the same order
(define (test-search)
  (check-group "search")
  (let* ((t (stree->tree '(concat (em "a") (frac (em "b") (em (em "c"))))))
         (em? (lambda (x) (tree-is? x 'em)))
         (found (tree-search t em?))
         (ids (tree-search-indices t em?)))
    (check= (sts found) '((em "a") (em "b") (em (em "c")) (em "c")))
    (check= ids '((0) (1 0) (1 1) (1 1 0)))
    (check= (length found) (length ids))
    ;; each index leads to the subtree which was found
    (check= (map (lambda (f i) (tree-eq? f (subtree t i))) found ids)
            '(#t #t #t #t))
    (check= (sts (tree-search t tree-atomic?)) '("a" "b" "c"))
    (check= (sts (tree-search t (lambda (x) #f))) '())
    (check= (tree-search-indices t (lambda (x) (tree-is? x 'concat))) '(()))
    (check= (sts (tree-search (stree->tree "x") tree-atomic?)) '("x"))))

;; tree-map-children and tree-map-accessible-children build a new tree
(define (test-map)
  (check-group "map")
  (let* ((t (stree->tree '(frac "a" "b")))
         (bang (lambda (x) (string-append (tree->string x) "!"))))
    (check= (st (tree-map-children bang t)) '(frac "a!" "b!"))
    (check= (st t) '(frac "a" "b"))
    (check= (st (tree-map-accessible-children (lambda (x) "X") t))
            '(frac "X" "X"))
    ;; the argument of a label is not accessible
    (check= (st (tree-map-accessible-children (lambda (x) "X")
                                              (stree->tree '(label "a"))))
            '(label "a"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a change in place through a child is seen through its parent, and
;; through every other reference to the same node
(define (test-sharing)
  (check-group "sharing")
  (let* ((t (stree->tree '(concat "ab" (frac "x" "y") "cd")))
         (f (tree-ref t 1))
         (g (tree-ref t 'frac)))
    (tree-set! f 0 "z")
    (check= (st t) '(concat "ab" (frac "z" "y") "cd"))
    (check= (st g) '(frac "z" "y"))
    (tree-set t 1 1 "w")
    (check= (st f) '(frac "z" "w"))
    (tree-set! t 'frac :first "u")
    (check= (st f) '(frac "u" "w"))
    (tree-set! t :last "CD")
    (check= (st t) '(concat "ab" (frac "u" "w") "CD"))
    (tree-assign-node! f 'tfrac)
    (check= (st t) '(concat "ab" (tfrac "u" "w") "CD"))
    (tree-insert! f 1 '("v"))
    (check= (st t) '(concat "ab" (tfrac "u" "v" "w") "CD"))
    (tree-remove! f 0 1)
    (check= (st t) '(concat "ab" (tfrac "v" "w") "CD"))
    ;; strings are changed in place as well
    (tree-insert! (tree-ref t 0) 2 "c")
    (check= (st t) '(concat "abc" (tfrac "v" "w") "CD"))
    (tree-remove! (tree-ref t 0) 0 1)
    (check= (st t) '(concat "bc" (tfrac "v" "w") "CD"))
    ;; the list of children shares its nodes with the tree
    (tree-set! (cadr (tree-children t)) 1 "W")
    (check= (st t) '(concat "bc" (tfrac "v" "W") "CD"))))

;; tree-assign! and the one argument tree-set! on a detached tree rebind
;; their variable to the new value; the old node and its parent are kept
(define (test-assign)
  (check-group "assign")
  (let* ((t (stree->tree '(concat "a" (strong "b"))))
         (r (tree-ref t 1))
         (old r))
    (check= (st (tree-assign! r '(em "c"))) '(em "c"))
    (check= (st r) '(em "c"))
    (check= (st old) '(strong "b"))
    (check= (st t) '(concat "a" (strong "b")))
    (tree-set! r "d")
    (check= (st r) "d")
    (check= (st t) '(concat "a" (strong "b")))
    (check= (st (tree-assign (tree-ref t 0) '(em "x"))) '(em "x"))
    (check= (st t) '(concat "a" (strong "b")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifications in place
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insertion and removal keep the order of the children and change the
;; arity by the number of children inserted or removed
(define (test-insert-remove)
  (check-group "insert and remove")
  (with t (stree->tree '(concat "a" "b" "c"))
    (tree-insert! t 1 '("x" "y"))
    (check= (tree-arity t) 5)
    (check= (st t) '(concat "a" "x" "y" "b" "c"))
    (tree-insert! t 5 '("z"))
    (check= (st t) '(concat "a" "x" "y" "b" "c" "z"))
    (tree-insert! t 0 '((em "0")))
    (check= (st t) '(concat (em "0") "a" "x" "y" "b" "c" "z"))
    (tree-insert! t 3 '())
    (check= (tree-arity t) 7)
    (tree-remove! t 0 1)
    (check= (st t) '(concat "a" "x" "y" "b" "c" "z"))
    (tree-remove! t 1 2)
    (check= (tree-arity t) 4)
    (check= (st t) '(concat "a" "b" "c" "z"))
    (tree-remove! t 3 1)
    (check= (st t) '(concat "a" "b" "c"))
    (tree-remove! t 1 0)
    (check= (st t) '(concat "a" "b" "c")))
  ;; inserting and removing in a string
  (with s (stree->tree "hello")
    (tree-insert! s 5 " world")
    (check= (st s) "hello world")
    (tree-insert! s 0 ">")
    (check= (st s) ">hello world")
    (tree-remove! s 0 1)
    (check= (st s) "hello world")
    (tree-remove! s 5 6)
    (check= (st s) "hello"))
  ;; the inserted content is copied
  (let* ((t (stree->tree '(concat "a")))
         (x (stree->tree '(em "x"))))
    (tree-insert! t 1 (list x))
    (tree-set! x 0 "y")
    (check= (st t) '(concat "a" (em "x"))))
  (check-error (tree-insert (stree->tree '(concat "a")) 0 3) 'texmacs-error))

;; splitting and joining children, and changing the label
(define (test-split-join)
  (check-group "split and join")
  (with t (stree->tree '(concat "abcd" "ef"))
    (tree-split! t 0 2)
    (check= (st t) '(concat "ab" "cd" "ef"))
    (tree-join! t 1)
    (check= (st t) '(concat "ab" "cdef"))
    (tree-join! t 0)
    (check= (st t) '(concat "abcdef"))
    (tree-assign-node! t 'document)
    (check= (st t) '(document "abcdef"))
    (check= (tree-arity t) 1))
  (with t (stree->tree '(document (concat "a" "b" "c") "d"))
    (tree-split! t 0 1)
    (check= (st t) '(document (concat "a") (concat "b" "c") "d"))
    (tree-join! t 0)
    (check= (st t) '(document (concat "a" "b" "c") "d"))))

;; tree-insert-node! puts its tree as a child of a new node, and
;; tree-remove-node! keeps one child of the node
(define (test-nodes)
  (check-group "nodes")
  (let* ((t (stree->tree '(frac "a" "b")))
         (old t))
    (tree-insert-node! t 1 '(sqrt "u"))
    (check= (st t) '(sqrt "u" (frac "a" "b")))
    (check= (st old) '(frac "a" "b"))
    (tree-remove-node! t 1)
    (check= (st t) '(frac "a" "b"))
    (tree-insert-node! t 0 '(em))
    (check= (st t) '(em (frac "a" "b")))
    (tree-remove-node! t 0)
    (tree-remove-node! t 0)
    (check= (st t) "a")))

;; tree-set with a path, and its errors where there is no subtree
(define (test-tree-set)
  (check-group "tree-set")
  (with t (stree->tree '(concat "a" (frac "x" (sqrt "y"))))
    (tree-set t 1 1 0 "Y")
    (check= (st t) '(concat "a" (frac "x" (sqrt "Y"))))
    (tree-set t 1 1 '(em "z"))
    (check= (st t) '(concat "a" (frac "x" (em "z"))))
    (tree-set t 'frac :last 0 "w")
    (check= (st t) '(concat "a" (frac "x" (em "w"))))
    (check-error (tree-set t 7 "x") 'texmacs-error)
    (check-error (tree-set t 0 0 "x") 'texmacs-error)
    (check-error (tree-set t 'nosuch "x") 'texmacs-error)
    (check-error (tree-set (tree-ref t 0) :last "x") 'texmacs-error)
    (check-error (tree-set '(concat "a") 0 "x") 'texmacs-error)
    ;; tree-set-diff needs a tree in a document
    (check-error (tree-set-diff (tree-ref t 1) "zz") 'texmacs-error)
    (check= (st t) '(concat "a" (frac "x" (em "w"))))))

;; tree-replace with labels or predicates changes the tree in place
(define (test-replace)
  (check-group "replace")
  (with t (stree->tree '(concat (em "a") "b" (frac (em "c") "d")))
    (tree-replace t 'em 'strong)
    (check= (st t) '(concat (strong "a") "b" (frac (strong "c") "d")))
    (tree-replace t (lambda (u) (tree-is? u 'frac))
                  (lambda (u) (tree-assign-node! u 'tfrac)))
    (check= (st t) '(concat (strong "a") "b" (tfrac (strong "c") "d")))
    (tree-replace t 'nosuch 'em)
    (check= (st t) '(concat (strong "a") "b" (tfrac (strong "c") "d")))))
;; tree-replace with content (neither a label nor a procedure) uses
;; tree-assign, which does not reach the parent of a detached tree: it is
;; only meaningful in a document and not checked here.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positions of detached trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a detached tree and its subtrees have no position: the inverse path
;; ends with -5 (DETACHED), and the navigation functions give #f
(define (test-detached)
  (check-group "detached")
  (let* ((t (stree->tree '(concat "a" (frac "x" (sqrt "y")))))
         (s (tree-ref t 1 1)))
    (check= (cAr (tree-ip t)) -5)
    (check= (cAr (tree-ip s)) -5)
    (check-false (tree-active? t))
    (check-false (tree-active? s))
    (check-false (tree-is-buffer? t))
    (check-false (tree-get-path s))
    (check-false (tree-get-path "abc"))
    (check-false (tree->path s))
    (check-false (tree->path t 1))
    (check-false (tree->path t 1 0))
    (check-false (tree-up s))
    (check-false (tree-up s 2))
    (check-false (tree-outer s))
    (check-false (tree-index s))
    (check-false (tree-inside? s t))
    ;; tree-search-upwards can only find the tree itself
    (check-true (tree-eq? (tree-search-upwards s 'sqrt) s))
    (check-true (tree-eq? (tree-search-upwards s '(em sqrt)) s))
    (check-false (tree-search-upwards s 'frac))
    (check-false (tree-search-upwards s (lambda (x) (tree-is? x 'concat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Content: the tm- functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define frac-st '(frac "a" (sqrt "b")))
(define (frac-forms) (list frac-st (stree->tree frac-st)))
(define (atom-forms) (list "abc" (stree->tree "abc")))

;; each tm- function gives the same answer for a Scheme tree and a tree,
;; and for a string and an atomic tree
(define (test-content-same)
  (check-group "content: same answers")
  (check= (map tm-atomic? (frac-forms)) '(#f #f))
  (check= (map tm-atomic? (atom-forms)) '(#t #t))
  (check= (map tm-compound? (frac-forms)) '(#t #t))
  (check= (map tm-compound? (atom-forms)) '(#f #f))
  (check= (map tm-arity (frac-forms)) '(2 2))
  ;; FIXME: tm-arity gives 0 for a string but the length of the string
  ;; for an atomic tree: (tm-arity (stree->tree "abc")) is 3.
  (check= (map tm-length (frac-forms)) '(2 2))
  (check= (map tm-length (atom-forms)) '(3 3))
  (check= (map tm->string (atom-forms)) '("abc" "abc"))
  (check= (map tm->string (frac-forms)) '(#f #f))
  (check= (map tm->stree (frac-forms)) (list frac-st frac-st))
  (check= (map tm->stree (atom-forms)) '("abc" "abc"))
  (check= (map tm-label (frac-forms)) '(frac frac))
  (check= (map tm-car (frac-forms)) '(frac frac))
  (check= (tm-label (stree->tree "abc")) 'string)
  (check= (map (lambda (x) (map tm->stree (tm-children x))) (frac-forms))
          '(("a" (sqrt "b")) ("a" (sqrt "b"))))
  ;; FIXME: tm-children and tm->list on an atomic tree crash TeXmacs
  ;; (segmentation fault in tree-children), where they raise an error on
  ;; a string.
  (check= (map (lambda (x) (map tm->stree (tm-cdr x))) (frac-forms))
          '(("a" (sqrt "b")) ("a" (sqrt "b"))))
  (check= (map (lambda (x) (tm->stree (tm->list x))) (frac-forms))
          (list frac-st frac-st))
  (check= (map (lambda (x) (tm->stree (tm-range x 1 2))) (frac-forms))
          '((frac (sqrt "b")) (frac (sqrt "b"))))
  (check= (map (lambda (x) (tm-range x 1 3)) (atom-forms)) '("bc" "bc"))
  (check= (map (cut tm-func? <> 'frac) (frac-forms)) '(#t #t))
  (check= (map (cut tm-func? <> 'frac 2) (frac-forms)) '(#t #t))
  (check= (map (cut tm-func? <> 'frac 3) (frac-forms)) '(#f #f))
  (check= (map (cut tm-func? <> 'sqrt) (frac-forms)) '(#f #f))
  (check= (map (cut tm-func? <> 'string) (atom-forms)) '(#f #f))
  (check= (map (cut tm-is? <> 'frac) (frac-forms)) '(#t #t))
  (check= (map (cut tm-is? <> 'sqrt) (frac-forms)) '(#f #f))
  (check= (map (cut tm-is? <> 'abc) (atom-forms)) '(#f #f))
  (check= (map (cut tm-in? <> '(sqrt frac)) (frac-forms)) '(#t #t))
  (check= (map (cut tm-in? <> '(sqrt)) (frac-forms)) '(#f #f))
  (check= (map (cut tm-in? <> '(string)) (atom-forms)) '(#f #f)))

;; tm-equal? compares content whatever its representation
(define (test-content-equal)
  (check-group "content: equality")
  (let ((s frac-st)
        (t1 (stree->tree frac-st))
        (t2 (stree->tree frac-st)))
    (check-true (tm-equal? s t1))
    (check-true (tm-equal? t1 s))
    (check-true (tm-equal? t1 t2))
    (check-true (tm-equal? s frac-st))
    (check-true (tm-equal? "abc" (stree->tree "abc")))
    (check-true (tm-equal? (stree->tree "abc") "abc"))
    ;; a tree mixed with Scheme content
    (check-true (tm-equal? (list 'frac (stree->tree "a") '(sqrt "b")) t1))
    (check-false (tm-equal? t1 '(frac "a" (sqrt "c"))))
    (check-false (tm-equal? t1 '(frac "a")))
    (check-false (tm-equal? "abc" (stree->tree "abd")))
    (check-false (tm-equal? "abc" t1))))

;; tm-find returns the first match, in its own representation, and
;; tm-search all of them in document order
(define (test-content-search)
  (check-group "content: search")
  (let* ((s '(concat (em "a") (frac (em "b") "c")))
         (t (stree->tree s))
         (frac? (cut tm-is? <> 'frac)))
    (check= (tm-find s frac?) '(frac (em "b") "c"))
    (check= (tm->stree (tm-find t frac?)) '(frac (em "b") "c"))
    (check-true (tree? (tm-find t frac?)))
    (check-false (tm-find s (cut tm-is? <> 'sqrt)))
    (check-false (tm-find "abc" (lambda (x) #f)))
    (check= (tm-find "abc" string?) "abc")
    (check= (tm-find-tag s 'em) '(em "a"))
    (check= (tm->stree (tm-find-tag t 'em)) '(em "a"))
    (check= (tm-search-tag s 'em) '((em "a") (em "b")))
    (check= (map tm->stree (tm-search-tag t 'em)) '((em "a") (em "b")))
    (check= (tm-search s string?) '("a" "b" "c"))
    (check= (map tm->stree (tm-search t tm-atomic?)) '("a" "b" "c"))
    (check= (tm-search s (lambda (x) #f)) '())))

;; tm-replace builds new Scheme content and leaves its argument alone
(define (test-content-replace)
  (check-group "content: replace")
  (let* ((s '(concat "a" (em "a") "b"))
         (t (stree->tree s)))
    (check= (tm-replace s "a" "x") '(concat "x" (em "x") "b"))
    (check= (tm->stree (tm-replace t "a" "x")) '(concat "x" (em "x") "b"))
    (check= (st t) s)
    (check= (tm-replace s '(em "a") '(strong "a"))
            '(concat "a" (strong "a") "b"))
    (check= (tm-replace s (cut tm-is? <> 'em)
                        (lambda (x) `(strong ,@(tm-children x))))
            '(concat "a" (strong "a") "b"))
    (check= (tm-replace s (cut tm-is? <> 'em) "E") '(concat "a" "E" "b"))
    (check= (tm-replace s "zz" "x") s)
    (check= (tm-replace "a" "a" "b") "b")))

;; lengths such as "1.5cm" in strings and atomic trees
(define (test-lengths)
  (check-group "lengths")
  (check= (tm-make-length 2.5 "cm") "2.5cm")
  (check= (tm-make-length 3 "pt") "3pt")
  (check= (tm-length-unit-search "12pt" 0) 2)
  (check= (tm-length-unit-search "50%" 0) 2)
  (check-false (tm-length-unit-search "12" 0))
  (check-false (tm-length-unit-search 12 0))
  (check-true (tm-length? "1.5cm"))
  (check-true (tm-length? "-2%"))
  (check-true (tm-length? "0.5par"))
  (check-true (tm-length? (stree->tree "3em")))
  (check-false (tm-length? "cm"))
  (check-false (tm-length? "1.5"))
  (check-false (tm-length? "2Cm"))
  (check-false (tm-length? "2c3m"))
  (check-false (tm-length? (stree->tree '(em "1cm"))))
  (check= (tm-length-value "1.5cm") 1.5)
  (check= (tm-length-value (stree->tree "3em")) 3)
  (check= (tm-length-unit "1.5cm") "cm")
  (check= (tm-length-unit (stree->tree "3em")) "em")
  (check-false (tm-length-unit "12"))
  (check-false (tm-length-unit (stree->tree '(em "a"))))
  (with l (tm-make-length 4 "fn")
    (check= (list (tm-length-value l) (tm-length-unit l)) '(4 "fn"))))

;; collections of associations, as in the initial environment of a
;; document
(define (test-collections)
  (check-group "collections")
  (let ((c '(collection (associate "a" "1") (associate "b" "2"))))
    (check= (associate->binding '(associate "k" "v")) '("k" . "v"))
    (check-false (associate->binding '(associate "k")))
    (check= (binding->associate '("k" . "v")) '(associate "k" "v"))
    (check= (assoc->collection '(("a" . "1"))) '(collection (associate "a" "1")))
    (check= (assoc->collection '()) '(collection))
    (check= (collection->assoc c) '(("a" . "1") ("b" . "2")))
    (check= (collection->assoc (stree->tree c)) '(("a" . "1") ("b" . "2")))
    (check= (collection->assoc '(collection (associate "a" "1") "junk"))
            '(("a" . "1")))
    (check-false (collection->assoc '(tuple "a")))
    (check= (collection->assoc (assoc->collection '(("x" . "y"))))
            '(("x" . "y")))
    (check= (collection-ref c "b") "2")
    (check= (collection-ref (stree->tree c) "a") "1")
    (check-false (collection-ref c "z"))
    (check-false (collection-ref '(tuple) "a"))
    (check= (collection-ref (collection-set c "b" "3") "b") "3")
    (check= (collection-ref (collection-set c "z" "9") "z") "9")
    (check= (collection-ref (collection-set c "z" "9") "a") "1")
    (check= (collection-append '(collection (associate "a" "1"))
                               '(collection (associate "a" "3")
                                            (associate "b" "2")))
            '(collection (associate "a" "3") (associate "b" "2")))
    (check= (collection-delta c '(collection (associate "a" "1")
                                             (associate "b" "3")))
            '(collection (associate "b" "3")))
    (check= (collection-exclude c '("a")) '(collection (associate "b" "2")))
    (check-false (collection-exclude '(tuple) '("a")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the constructor by kind, and the Scheme form (kind path tree), where
;; the position arguments are appended to the path
(define (test-modification-forms)
  (check-group "modification forms")
  (check= (modification->scheme (modification 'assign '(1) '(em "x")))
          '(assign (1) (em "x")))
  (check= (modification->scheme (modification "assign" '(1) "x"))
          '(assign (1) "x"))
  (check= (modification->scheme (modification 'insert '(1) 0 "x"))
          '(insert (1 0) "x"))
  (check= (modification->scheme (modification 'remove '(0) 1 2))
          '(remove (0 1 2) ""))
  (check= (modification->scheme (modification 'split '(0) 1 2))
          '(split (0 1 2) ""))
  (check= (modification->scheme (modification 'join '() 0)) '(join (0) ""))
  (check= (modification->scheme (modification 'assign-node '() 'concat))
          '(assign-node () (concat)))
  (check= (modification->scheme (modification 'insert-node '() 1 '(frac "u")))
          '(insert-node (1) (frac "u")))
  (check= (modification->scheme (modification 'remove-node '() 0))
          '(remove-node (0) ""))
  (check= (modification-type (modification "remove" '(0) 1 2)) 'remove)
  (check= (modification-kind (modification 'insert '() 0 "x")) "insert")
  (check= (modification-path (modification 'insert '(2) 0 "x")) '(2 0))
  (check= (st (modification-tree (modification 'assign '() '(em "x"))))
          '(em "x"))
  (check-error (modification 'bogus '()) 'texmacs-error)
  (with m '(assign (0) (frac "a" "b"))
    (check= (modification->scheme (scheme->modification m)) m))
  (with m '(insert (0 1) "xy")
    (check= (modification->scheme (scheme->modification m)) m)))

;; applying modifications: modification-apply makes a new tree,
;; modification-apply! changes the tree in place
(define (test-modification-apply)
  (check-group "modification apply")
  (let* ((d (stree->tree '(document "a" "b")))
         (m (modification 'insert '(0) 1 "XY")))
    (check-true (modification-applicable? d m))
    (check= (st (modification-apply d m)) '(document "aXY" "b"))
    (check= (st d) '(document "a" "b"))
    (check= (modification->scheme (modification-invert m d))
            '(remove (0 1 2) ""))
    (check= (st (modification-apply (modification-apply d m)
                                    (modification-invert m d)))
            '(document "a" "b"))
    (check-false (modification-applicable? d (modification 'remove '(5) 0 1)))
    (check-false (modification-applicable?
                  (stree->tree '(concat "ab" "c"))
                  (modification 'split '(0) 1 0))))
  ;; FIXME: modification-apply of a modification which is not applicable,
  ;; such as (modification 'split '(0) 1 0) on (concat "ab" "c"), crashes
  ;; TeXmacs (segmentation fault in clean_split).
  (with t (stree->tree '(concat "ab" "c"))
    (check= (st (modification-apply t (modification 'assign '(1) "z")))
            '(concat "ab" "z"))
    (check= (st (modification-apply t (modification 'remove '(0) 0 1)))
            '(concat "b" "c"))
    (check= (st (modification-apply t (modification 'split '() 0 1)))
            '(concat "a" "b" "c"))
    (check= (st (modification-apply t (modification 'join '() 0)))
            '(concat "abc"))
    (check= (st (modification-apply t (modification 'assign-node '() 'document)))
            '(document "ab" "c"))
    (check= (st (modification-apply t (modification 'insert-node '() 0
                                                    '(frac "u"))))
            '(frac (concat "ab" "c") "u"))
    (check= (st (modification-apply t (modification 'remove-node '() 1))) "c")
    (check= (st t) '(concat "ab" "c")))
  ;; every modification followed by its inverse gives back the tree
  (with t (stree->tree '(concat "ab" "cd" (em "e")))
    (for (m (list (modification 'assign '(2) "z")
                  (modification 'insert '() 1 '(concat "x" "y"))
                  (modification 'remove '() 0 2)
                  (modification 'split '() 0 1)
                  (modification 'join '() 0)
                  (modification 'assign-node '() 'document)
                  (modification 'insert-node '() 1 '(frac "u"))
                  (modification 'remove-node '(2) 0)))
      (check-true (modification-applicable? t m))
      (check= (st (modification-apply (modification-apply t m)
                                      (modification-invert m t)))
              '(concat "ab" "cd" (em "e")))))
  (let* ((t (stree->tree '(concat "ab" "c")))
         (r t))
    (modification-apply! t (modification 'assign '(1) "Z"))
    (check= (st t) '(concat "ab" "Z"))
    (check= (st r) '(concat "ab" "Z"))))

;; two concurrent modifications of the same tree
(define (test-modification-push)
  (check-group "modification push")
  (let* ((d (stree->tree '(document "abc")))
         (m1 (modification 'insert '(0) 0 "X"))
         (m2 (modification 'insert '(0) 3 "Y")))
    (check-true (modification-can-push? m1 m2 d))
    (check= (modification->scheme (modification-push m1 m2 d))
            '(insert (0 0) "X"))
    (check= (modification->scheme (modification-co-push m1 m2 d))
            '(insert (0 4) "Y"))
    ;; both orders give the same document
    (check= (st (modification-apply (modification-apply d m2)
                                    (modification-push m1 m2 d)))
            '(document "XabcY"))
    (check= (st (modification-apply (modification-apply d m1)
                                    (modification-co-push m1 m2 d)))
            '(document "XabcY"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insert-patch pos s)
  (patch-pair (modification 'insert '(0) pos s)
              (modification 'remove '(0) pos (string-length s))))

;; the kinds of patches and their Scheme forms
(define (test-patch-forms)
  (check-group "patch forms")
  (let* ((pa (insert-patch 0 "X"))
         (pb (insert-patch 3 "Y"))
         (pc (patch-append pa pb))
         (br (patch-branch (list pa pb))))
    (check-true (patch-pair? pa))
    (check-false (patch-compound? pa))
    (check-true (patch-compound? pc))
    (check-true (patch-branch? br))
    (check-false (patch-birth? pc))
    (check= (patch-arity pc) 2)
    (check= (patch->scheme pa)
            '(pair (insert (0 0) "X") (remove (0 0 1) "")))
    (check= (modification->scheme (patch-direct pa)) '(insert (0 0) "X"))
    (check= (modification->scheme (patch-inverse pa)) '(remove (0 0 1) ""))
    (check= (map patch->scheme (patch-children pc))
            (list (patch->scheme pa) (patch->scheme pb)))
    (check= (patch->scheme pc)
            `(compound ,(patch->scheme pa) ,(patch->scheme pb)))
    (check= (patch->scheme br)
            `(branch ,(patch->scheme pa) ,(patch->scheme pb)))
    (check= (patch->scheme (patch-append)) '(compound))
    (check-true (patch-birth? (patch-birth 3.0 #t)))
    (check= (patch->scheme (patch-birth 3.0 #t)) '(birth #t 3.0))
    (check= (patch-get-author (patch-author 2.0 pa)) 2.0)
    (check-true (patch-author? (patch-author 2.0 pa)))
    ;; Scheme forms come back unchanged
    (for (p (list pa pc br))
      (check= (patch->scheme (scheme->patch (patch->scheme p)))
              (patch->scheme p)))
    (check-false (scheme->patch '(nosuch)))))
;; FIXME: the Scheme forms of birth and author patches do not come back:
;; patch->scheme writes (birth BIRTH? AUTHOR) but scheme->patch calls
;; (patch-birth BIRTH? AUTHOR) whose arguments are (author birth?), which
;; raises wrong-type-arg; scheme->patch builds an author patch with
;; patch-birth instead of patch-author; and patch->scheme leaves the
;; child of an author patch as a patch, not as its Scheme form.

;; applying, inverting and composing patches
(define (test-patch-apply)
  (check-group "patch apply")
  (let* ((doc (stree->tree '(document "abc")))
         (pa (insert-patch 0 "X"))
         (pb (insert-patch 3 "Y"))
         (pc (patch-append pa pb)))
    (check-true (patch-applicable? pa doc))
    (check-false (patch-applicable? (insert-patch 9 "Z") doc))
    (check= (st (patch-apply doc pa)) '(document "Xabc"))
    (check= (st doc) '(document "abc"))
    (check= (patch->scheme (patch-invert pa doc))
            '(pair (remove (0 0 1) "") (insert (0 0) "X")))
    (check= (st (patch-apply (patch-apply doc pa) (patch-invert pa doc)))
            '(document "abc"))
    ;; a compound patch applies its children in order
    (check= (st (patch-apply doc pc)) '(document "XabYc"))
    (check= (st (patch-apply (patch-apply doc pa) pb))
            (st (patch-apply doc pc)))
    (check= (st (patch-apply (patch-apply doc pc) (patch-invert pc doc)))
            '(document "abc"))
    (check-true (patch-equivalent? pc pc doc))
    (check-false (patch-equivalent? pc (patch-append pb pa) doc))
    (check-true (patch-strong-equivalent? pc pc doc))
    (check-false (patch-equivalent? (insert-patch 9 "Z") pa doc))
    (let ((d doc) (r doc))
      (patch-apply! d pa)
      (check= (st d) '(document "Xabc"))
      (check= (st r) '(document "Xabc")))))

;; two concurrent patches of the same tree
(define (test-patch-push)
  (check-group "patch push")
  (let* ((doc (stree->tree '(document "abc")))
         (pa (insert-patch 0 "X"))
         (pb (insert-patch 3 "Y")))
    (check-true (patch-can-push? pa pb doc))
    (check= (patch->scheme (patch-push pa pb doc))
            '(pair (insert (0 0) "X") (remove (0 0 1) "")))
    (check= (patch->scheme (patch-co-push pa pb doc))
            '(pair (insert (0 4) "Y") (remove (0 4 1) "")))
    (check= (st (patch-apply (patch-apply doc pb) (patch-push pa pb doc)))
            '(document "XabcY"))
    (check= (st (patch-apply (patch-apply doc pa) (patch-co-push pa pb doc)))
            '(document "XabcY"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (trees-test-failures)
  (check-suite "trees")
  (test-conversion)
  (test-numbers)
  (test-identity)
  (test-access)
  (test-tree-ref)
  (test-predicates)
  (test-building)
  (test-search)
  (test-map)
  (test-sharing)
  (test-assign)
  (test-insert-remove)
  (test-split-join)
  (test-nodes)
  (test-tree-set)
  (test-replace)
  (test-detached)
  (test-content-same)
  (test-content-equal)
  (test-content-search)
  (test-content-replace)
  (test-lengths)
  (test-collections)
  (test-modification-forms)
  (test-modification-apply)
  (test-modification-push)
  (test-patch-forms)
  (test-patch-apply)
  (test-patch-push)
  (check-end))
