;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-edit-test.scm
;; DESCRIPTION : tests of the editing of mathematics, without a window
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite edits formulas in a buffer the way a user does, through the
;; commands of the Insert menu for mathematics (math-menu.scm), the keyboard
;; shortcuts of math-kbd.scm and the structured editing routines of
;; math-edit.scm and of the editor (Edit/Modify/edit_math.cpp,
;; edit_delete.cpp), and checks the exact document trees and cursor paths.
;; It also checks the correction routines of formulas (manual-correct,
;; math-correct-all, Data/Tree/tree_correct.cpp), the bracket upgraders
;; and the grammar of std-math (packrat-correct?).
;;
;; As in editing-test.scm, each step is wrapped like a key press of the
;; event loop (edit-step), switch-to-buffer* does not make a second view,
;; and paths are relative to the buffer. The cursor movements through boxes
;; (go-left, go-up, structured-up/down inside a fraction...) need a window
;; and are left out; the structured movements which follow the tree are
;; checked.
;;
;; Keys are pressed with key-press, which goes through the keyboard
;; combinations of the editor ("<" then "=" gives <leqslant>, "var" is the
;; tab key). The prefixes math, math:left, math:small... depend on the look
;; and feel (cmd is A- or M-), so that the shortcuts behind them are called
;; directly; the F5, F6, F7 and S-F5 prefixes are the same everywhere and
;; are pressed. Pressing a modifier key such as M-f does not return without
;; a window and is avoided.
;;
;; Preferences are read, never set. With the defaults, brackets are matched
;; ("automatic brackets" is "mathematics") and large ("use large brackets"
;; is "on"), and semantic editing is off ("semantic correctness"), so that
;; the wrappers of math-sem-edit.scm, which need it on, are not reached;
;; the grammar they use is checked through packrat-correct? instead.

(texmacs-module (check math-edit-test)
  (:use (check check-lib)
        (math math-edit)
        (math math-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a key press or a menu action
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define-macro (edit . body)
  `(edit-step (lambda () ,@body)))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (at . l)
  ;; the absolute path of @l in the current buffer
  (append (buffer-path) l))

(define (cursor)
  (list-tail (cursor-path) (length (buffer-path))))

(define (bt . l)
  ;; the subtree at the path @l of the body
  (apply tree-ref (cons (buffer-tree) l)))

(define (with-math-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, then close the buffer
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-start)
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define-macro (in-buffer doc . body)
  `(with-math-body ,doc (lambda () ,@body)))

(define (typed . keys)
  ;; the formula after typing @keys behind x in (math "x")
  (let ((old (current-buffer))
        (u (new-buffer))
        (r #f))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree '(document (math "x"))))
    (update-forced)
    (set! r (check-run
             (lambda ()
               (edit (go-to (at 0 0 1)))
               (for-each (lambda (k) (edit (key-press k))) keys)
               (tree->stree (bt 0)))))
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))
    r))

(define (correct x)
  (tree->stree (manual-correct (stree->tree x))))

(define (grammar-ok? type x)
  (packrat-correct? "std-math" type (stree->tree x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inline formulas, displayed equations and equation arrays are made from
;; text; the mode becomes math inside them. Inside a formula, $ leaves it
;; and enter goes to a new paragraph after it (kbd-enter in math-edit.scm).
(define (test-enter-math)
  (check-group "enter math")
  (in-buffer '(document "")
    (check-false (in-math?))
    (edit (make 'math))
    (check= (body) '(document (math "")))
    (check= (cursor) '(0 0 0))
    (check-true (in-math?))
    (check= (get-env "mode") "math")
    (edit (insert "a"))
    ;; $ inside a formula goes to its end (math-make-math)
    (edit (math-make-math))
    (check= (body) '(document (math "a")))
    (check= (cursor) '(0 1))
    (check-false (in-math?))
    ;; $ in text starts a new formula
    (edit (insert " b") (key-press "$"))
    (check= (body) '(document (concat (math "a") " b" (math ""))))
    (check= (cursor) '(0 2 0 0))
    (check-true (in-math?)))
  (in-buffer '(document (math "x"))
    ;; text inside a formula
    (edit (go-to (at 0 0 1)) (make 'text))
    (check= (body) '(document (math (concat "x" (text "")))))
    (check= (cursor) '(0 0 1 0 0))
    (check= (get-env "mode") "text")
    (check-false (in-math?))
    (check-true (in-text?)))
  (in-buffer '(document "")
    (edit (make-equation*))
    (check= (body) '(document (equation* "")))
    (check= (get-env "mode") "math")
    (edit (insert "x=1") (kbd-return))
    ;; enter leaves the equation for a new paragraph
    (check= (body) '(document (equation* "x=1") ""))
    (check= (cursor) '(1 0)))
  (in-buffer '(document "")
    (edit (make-equation))
    (check= (body) '(document (equation "")))
    (check= (get-env "mode") "math"))
  (in-buffer '(document "")
    (edit (make-eqnarray*))
    (check= (body) '(document (eqnarray* "")))
    (check= (get-env "mode") "math"))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (key-press "return"))
    (check= (body) '(document (math "x") ""))
    (check= (cursor) '(1 0)))
  (in-buffer '(document (equation (document "x")))
    (edit (go-to (at 0 0 0 1)) (kbd-enter (bt 0) #f))
    (check= (body) '(document (equation (document "x")) ""))
    (check= (cursor) '(1 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline and displayed formulas, equation arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; variant-equation and variant-formula (the focus menu of a formula) turn
;; an inline formula into a displayed one and back, moving the punctuation
;; after the formula into it; equation->eqnarray splits an equation at its
;; relations and eqnarray->equation joins the rows again.
(define (test-formula-variants)
  (check-group "formula variants")
  (in-buffer '(document "Let" (math "x+y") "then")
    (edit (go-to (at 1 0 1)) (variant-equation (bt 1)))
    (check= (body) '(document "Let" (equation* (document "x+y")) "then"))
    (check= (cursor) '(1 0 0 1))
    ;; back: the formula joins the paragraphs around it
    (edit (variant-formula (bt 1)))
    (check= (body) '(document (concat "Let " (math "x+y") " then")))
    (check= (cursor) '(0 1 0 1)))
  ;; a formula inside a paragraph: the paragraph is split around it,
  ;; without leaving concats of a single child
  (in-buffer '(document (concat "Let " (math "x+y") ", so"))
    (edit (go-to (at 0 1 0 1)) (variant-equation (bt 0 1)))
    (check= (body) '(document "Let" (equation* (document "x+y,")) "so"))
    (check= (cursor) '(1 0 0 1))
    (edit (variant-formula (bt 1)))
    (check= (body) '(document (concat "Let " (math "x+y") ", so")))
    (check= (cursor) '(0 1 0 1)))
  (in-buffer '(document (equation* (document "a")))
    (edit (go-to (at 0 0 0 1)) (variant-formula (bt 0)))
    (check= (body) '(document (math "a")))
    (check= (cursor) '(0 0 1)))
  ;; an equation of several lines stays displayed
  (in-buffer '(document (equation* (document "a" "b")))
    (edit (go-to (at 0 0 0 1)) (variant-formula (bt 0)))
    (check= (body) '(document (equation* (document "a" "b")))))
  ;; variant-circulate: a formula of mode math becomes an equation
  (in-buffer '(document (with "mode" "math" "x+y"))
    (edit (variant-circulate (bt 0) #t))
    (check= (body) '(document (equation* (document "x+y")))))
  (in-buffer '(document (equation* (document "a=b+c")))
    (edit (equation->eqnarray (bt 0)))
    (check= (body) '(document (eqnarray* (document (tformat (table
                       (row (cell "a") (cell "=") (cell "b+c"))))))))
    (check= (cursor) '(0 0 0 0 0 2 0 3))
    (edit (eqnarray->equation (bt 0)))
    (check= (body) '(document (equation* "a=b+c")))
    (check= (cursor) '(0 0 5)))
  ;; one row for each relation after the first
  (in-buffer '(document (equation* (document "a=b<leq>c")))
    (edit (equation->eqnarray (bt 0)))
    (check= (body) '(document (eqnarray* (document (tformat (table
                       (row (cell "a") (cell "=") (cell "b"))
                       (row (cell "") (cell "<leq>") (cell "c")))))))))
  ;; no relation: nothing to align
  (in-buffer '(document (equation* (document "a+b")))
    (edit (equation->eqnarray (bt 0)))
    (check= (body) '(document (equation* (document "a+b")))))
  ;; a label goes to the last row with an equation number, and back
  (in-buffer '(document (equation* (document (concat "a=b" (label "e1")))))
    (edit (equation->eqnarray (bt 0)))
    (check= (body) '(document (eqnarray* (document (tformat (table
                       (row (cell "a") (cell "=")
                            (cell (concat "b" (eq-number) (label "e1"))))))))))
    (edit (eqnarray->equation (bt 0)))
    (check= (body) '(document (equation (concat (label "e1") "a=b")))))
  (check= (focus-tag-name 'math) "Inline formula")
  (check= (focus-tag-name 'equation*) "Displayed formula")
  (check= (focus-tag-name 'eqnarray*) "Equations")
  (check= (focus-variants-of (stree->tree '(math "x"))) '(formula equation))
  (check= (focus-variants-of (stree->tree '(eqnarray* ""))) '(eqnarray*))
  (check= (standard-options 'math)
          '(:recurse "number-long-article" "math-check")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-fraction puts the cursor in the numerator, or makes a selection the
;; numerator and puts the cursor in the denominator; the kinds of fractions
;; of the menu are variants of each other.
(define (test-fractions)
  (check-group "fractions")
  (in-buffer '(document (math ""))
    (edit (go-to (at 0 0 0)) (make-fraction))
    (check= (body) '(document (math (frac "" ""))))
    (check= (cursor) '(0 0 0 0))
    (edit (insert "1") (make 'frac))
    (check= (body) '(document (math (frac (concat "1" (frac "" "")) ""))))
    (check= (cursor) '(0 0 0 1 0 0))
    (check= (get-env "mode") "math"))
  (in-buffer '(document (math "abc"))
    (edit (selection-set (at 0 0 1) (at 0 0 3)) (make-fraction))
    (check= (body) '(document (math (concat "a" (frac "bc" "")))))
    (check= (cursor) '(0 0 1 1 0)))
  (in-buffer '(document (math "abc"))
    (edit (go-to (at 0 0 3)) (make 'tfrac))
    (check= (body) '(document (math (concat "abc" (tfrac "" "")))))
    (edit (make 'dfrac) (make 'frac*) (make 'cfrac))
    (check= (body) '(document (math (concat "abc" (tfrac (dfrac (frac*
                                       (cfrac "" "") "") "") "")))))
    (check= (cursor) '(0 0 1 0 0 0 0 0)))
  (in-buffer '(document (math (frac "a" "b")))
    (check= (focus-variants-of (bt 0 0)) '(frac tfrac dfrac frac* cfrac))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (tfrac "a" "b"))))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (dfrac "a" "b"))))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (frac* "a" "b"))))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (cfrac "a" "b"))))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (frac "a" "b"))))
    (edit (variant-circulate (bt 0 0) #f))
    (check= (body) '(document (math (cfrac "a" "b")))))
  ;; the keys for a half and a quarter
  (check= (typed "onehalf") '(math (concat "x" (frac "1" "2")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-script makes the four scripts; _ and ^ are the keys of the right
;; ones; a superscript cannot start in an empty subscript; a script which
;; is alone can be turned into the other one (variant-circulate) or be
;; completed by the other one (structured-insert-up and -down).
(define (test-scripts)
  (check-group "scripts")
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make-script #f #t))
    (check= (body) '(document (math (concat "x" (rsub "")))))
    (check= (cursor) '(0 0 1 0 0))
    ;; nothing in an empty script
    (edit (make-script #t #t))
    (check= (body) '(document (math (concat "x" (rsub "")))))
    (check= (cursor) '(0 0 1 0 0))
    (edit (insert "i"))
    (check-true (script-context? (bt 0 0 1)))
    (check-false (script-context? (bt 0 0)))
    (check-true (script-only-script? (bt 0 0 1))))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make-script #f #f))
    (check= (body) '(document (math (concat "x" (lsub "")))))
    (edit (go-to (at 0 0 0 1)) (make-script #t #f))
    (check= (body) '(document (math (concat "x" (lsup "") (lsub "")))))
    (check= (cursor) '(0 0 1 0 0)))
  ;; the selection becomes the script
  (in-buffer '(document (math "xab"))
    (edit (selection-set (at 0 0 1) (at 0 0 3)) (make-script #t #t))
    (check= (body) '(document (math (concat "x" (rsup "ab")))))
    (check= (cursor) '(0 0 1 1)))
  ;; the keys
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (key-press "_"))
    (check= (body) '(document (math (concat "x" (rsub "")))))
    (edit (key-press "a") (key-press "^") (key-press "2"))
    (check= (body) '(document (math (concat "x" (rsub (concat "a" (rsup "2")))))))
    (check= (cursor) '(0 0 1 0 1 0 1)))
  (check= (typed "_" "tab") '(math "x_"))
  (check= (typed "^" "tab") '(math "x^"))
  (check= (typed "twosuperior") '(math (concat "x" (rsup "2"))))
  (in-buffer '(document (math (concat "x" (rsub "i"))))
    (edit (variant-circulate (bt 0 0 1) #t))
    (check= (body) '(document (math (concat "x" (rsup "i")))))
    (edit (variant-circulate (bt 0 0 1) #t))
    (check= (body) '(document (math (concat "x" (rsub "i")))))
    (edit (variant-set (bt 0 0 1) 'rsup))
    (check= (body) '(document (math (concat "x" (rsup "i")))))
    (edit (variant-set (bt 0 0 1) 'rsub))
    ;; a superscript after the subscript
    (edit (go-to (at 0 0 1 0 1)) (structured-insert-up))
    (check= (body) '(document (math (concat "x" (rsub "i") (rsup "")))))
    (check= (cursor) '(0 0 2 0 0))
    (edit (insert "n"))
    (check-false (script-only-script? (bt 0 0 1)))
    ;; the scripts of a pair are not turned into each other
    (edit (variant-circulate (bt 0 0 1) #t))
    (check= (body) '(document (math (concat "x" (rsub "i") (rsup "n"))))))
  (in-buffer '(document (math (concat "x" (rsup "n"))))
    ;; up from a superscript, down from a subscript: nothing
    (edit (go-to (at 0 0 1 0 1)) (structured-insert-up))
    (check= (body) '(document (math (concat "x" (rsup "n")))))
    (edit (go-to (at 0 0 1 0 1)) (structured-insert-down))
    (check= (body) '(document (math (concat "x" (rsup "n") (rsub "")))))
    (check= (cursor) '(0 0 2 0 0)))
  (in-buffer '(document (math (concat (lsub "i") "x")))
    (edit (variant-circulate (bt 0 0 0) #t))
    (check= (body) '(document (math (concat (lsup "i") "x"))))
    (edit (go-to (at 0 0 0 0 1)) (structured-insert-down))
    (check= (body) '(document (math (concat (lsup "i") (lsub "") "x"))))
    (check= (cursor) '(0 0 1 0 0)))
  (check= (focus-variants-of (stree->tree '(rsub "i"))) '(rsub rsup))
  (check= (focus-variants-of (stree->tree '(lsup "i"))) '(lsub lsup))
  (check-true (focus-can-insert-remove? (stree->tree '(rsup "i")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Square roots and n-th roots; sqrt-toggle (the "Multiple root" toggle of
;; the focus bar) adds and removes the index.
(define (test-roots)
  (check-group "roots")
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make-sqrt))
    (check= (body) '(document (math (concat "x" (sqrt "")))))
    (check= (cursor) '(0 0 1 0 0))
    (edit (insert "2") (sqrt-toggle (bt 0 0 1)))
    (check= (body) '(document (math (concat "x" (sqrt "2" "")))))
    (check= (cursor) '(0 0 1 1 0))
    (edit (insert "3"))
    (check= (body) '(document (math (concat "x" (sqrt "2" "3")))))
    (edit (sqrt-toggle (bt 0 0 1)))
    (check= (body) '(document (math (concat "x" (sqrt "2")))))
    ;; on something else than a root: nothing
    (edit (sqrt-toggle (bt 0 0)))
    (check= (body) '(document (math (concat "x" (sqrt "2"))))))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make-var-sqrt))
    (check= (body) '(document (math (concat "x" (sqrt "" "")))))
    (check= (cursor) '(0 0 1 0 0)))
  (in-buffer '(document (math "xy"))
    (edit (selection-set (at 0 0 1) (at 0 0 2)) (make-sqrt))
    (check= (body) '(document (math (concat "x" (sqrt "y")))))
    (check= (cursor) '(0 0 1 1)))
  ;; an n-th root of the selection: the cursor goes to the index
  (in-buffer '(document (math "xy"))
    (edit (selection-set (at 0 0 1) (at 0 0 2)) (make-var-sqrt))
    (check= (body) '(document (math (concat "x" (sqrt "y" "")))))
    (check= (cursor) '(0 0 1 1 0)))
  (check-false (focus-can-insert-remove? (stree->tree '(sqrt "2")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; math-big-operator inserts a big tag; the S-F5 prefix (math:symbol) gives
;; the sums and integrals; big-around is the form of a big operator with
;; its body, between which variant-circulate moves.
(define (test-big-operators)
  (check-group "big operators")
  (in-buffer '(document (math ""))
    (edit (go-to (at 0 0 0)) (math-big-operator "sum"))
    (check= (body) '(document (math (big "sum"))))
    (check= (cursor) '(0 0 1))
    (edit (make-script #f #t) (insert "i"))
    (check= (body) '(document (math (concat (big "sum") (rsub "i"))))))
  (check= (typed "S-F5" "S") '(math (concat "x" (big "sum"))))
  (check= (typed "S-F5" "P") '(math (concat "x" (big "prod"))))
  (check= (typed "S-F5" "I") '(math (concat "x" (big "int"))))
  (check= (typed "S-F5" "I" "I") '(math (concat "x" (big "iint"))))
  (check= (typed "S-F5" "O") '(math (concat "x" (big "oint"))))
  (check= (typed "S-F5" "U") '(math (concat "x" (big "cup"))))
  (in-buffer '(document (math (big-around "<sum>" "x")))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (big-around "<prod>" "x"))))
    (edit (variant-circulate (bt 0 0) #f))
    (edit (variant-circulate (bt 0 0) #f))
    (check= (body) '(document (math (big-around "<ointlim>" "x")))))
  ;; the first operator follows the last one
  (in-buffer '(document (math (big-around "<int>" "x")))
    (edit (variant-circulate (bt 0 0) #f))
    (check= (body) '(document (math (big-around "<interleave>" "x")))))
  ;; backspace after a big operator removes it
  (in-buffer '(document (math (concat "x" (big "sum") "y")))
    (edit (go-to (at 0 0 1 1)) (kbd-backspace))
    (check= (body) '(document (math "xy")))
    (check= (cursor) '(0 0 1)))
  ;; with matching brackets, a big-around is entered from the right and
  ;; left from the start of its body
  (in-buffer '(document (math (concat "x" (big-around "<sum>" "y"))))
    (edit (go-to (at 0 0 1 1)) (kbd-backspace))
    (check= (body) '(document (math (concat "x" (big-around "<sum>" "y")))))
    (check= (cursor) '(0 0 1 1 1))
    (edit (go-to (at 0 0 1 1 0)) (kbd-backspace))
    (check= (body) '(document (math (concat "x" (big-around "<sum>" "y")))))
    (check= (cursor) '(0 0 0 1)))
  (check= (tree->stree (tree-upgrade-big (stree->tree
                         '(concat (big "sum") (rsub "i") "x" (big ".")))))
          '(big-around "<sum>" (concat (rsub "i") "x")))
  (check= (tree->stree (tree-downgrade-big (stree->tree
                         '(big-around "<sum>" (concat (rsub "i") "x")))))
          '(concat (big "sum") (rsub "i") "x"))
  (check= (tree->stree (tree-downgrade-big (stree->tree
                         '(big-around "<sum>" "x"))))
          '(concat (big "sum") "x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With matching brackets, an opening bracket inserts a pair (around, or
;; around* for large brackets) with the cursor inside, the closing bracket
;; jumps over the closing one, and a missing bracket (<nobracket>) is
;; filled in by the bracket which is typed next to it.
(define (test-brackets)
  (check-group "brackets")
  (check= (get-preference "automatic brackets") "mathematics")
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (math-bracket-open "(" ")" #f))
    (check= (body) '(document (math (concat "x" (around "(" "" ")")))))
    (check= (cursor) '(0 0 1 1 0))
    (edit (insert "y") (math-bracket-close ")" "(" #f))
    (check= (body) '(document (math (concat "x" (around "(" "y" ")")))))
    (check= (cursor) '(0 0 1 1))
    (edit (math-bracket-open "[" "]" #t))
    (check= (body) '(document (math (concat "x" (around "(" "y" ")")
                                        (around* "[" "" "]")))))
    (check= (cursor) '(0 0 2 1 0))
    (edit (math-bracket-close "]" "[" #t))
    (check= (cursor) '(0 0 2 1))
    ;; a closing bracket right after a pair changes its closing bracket
    (edit (math-bracket-close ")" "(" #f))
    (check= (body) '(document (math (concat "x" (around "(" "y" ")")
                                        (around* "[" "" ")"))))))
  ;; the keys: large brackets by default
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (key-press "("))
    (check= (body) '(document (math (concat "x" (around* "(" "" ")")))))
    (edit (key-press "a") (key-press ")"))
    (check= (body) '(document (math (concat "x" (around* "(" "a" ")")))))
    (check= (cursor) '(0 0 1 1))
    (edit (key-press "[") (key-press "]") (key-press "{") (key-press "}"))
    (check= (body) '(document (math (concat "x" (around* "(" "a" ")")
                                        (around* "[" "" "]")
                                        (around* "{" "" "}")))))
    ;; | opens a pair, the second | closes it
    (edit (key-press "|") (key-press "z") (key-press "|"))
    (check= (tree->stree (bt 0 0 4)) '(around* "|" "z" "|"))
    (check= (cursor) '(0 0 4 1)))
  (check= (typed "|" "|") '(math (concat "x" (around* "<||>" "" "<||>"))))
  ;; around a selection
  (in-buffer '(document (math "abc"))
    (edit (selection-set (at 0 0 1) (at 0 0 2)) (math-bracket-open "(" ")" #f))
    (check= (body) '(document (math (concat "a" (around "(" "b" ")") "c"))))
    (check= (cursor) '(0 0 1 1 1)))
  ;; a closing bracket which matches nothing is not inserted
  (in-buffer '(document (math "abc"))
    (edit (go-to (at 0 0 3)) (math-bracket-close ")" "(" #f))
    (check= (body) '(document (math "abc")))
    (check= (cursor) '(0 0 3)))
  ;; filling a missing opening bracket
  (in-buffer '(document (math (concat "a" (around "<nobracket>" "b" ")") "c")))
    (edit (go-to (at 0 0 1 1 0)) (math-bracket-open "[" "]" #f))
    (check= (body) '(document (math (concat "a" (around "[" "b" ")") "c"))))
    (check= (cursor) '(0 0 1 1 0)))
  ;; filling a missing closing bracket
  (in-buffer '(document (math (concat "a" (around "(" "b" "<nobracket>") "c")))
    (edit (go-to (at 0 0 1 1 1)) (math-bracket-close "]" "[" #f))
    (check= (body) '(document (math (concat "a" (around "(" "b" "]") "c"))))
    (check= (cursor) '(0 0 1 1)))
  ;; an opening bracket in a missing closing one, as in ]a,b[
  (in-buffer '(document (math (concat "a" (around "(" "b" "<nobracket>") "c")))
    (edit (go-to (at 0 0 1 1 1)) (math-bracket-open "(" ")" #f))
    (check= (body) '(document (math (concat "a" (around "(" "b" "(") "c")))))
  ;; | closes a pair opened with <langle>
  (in-buffer '(document (math (concat "a" (around "<langle>" "b" "<rangle>"))))
    (edit (go-to (at 0 0 1 1 1)) (math-bracket-open "|" "|" #f))
    (check= (body) '(document (math (concat "a" (around "<langle>" "b" "|")))))
    (check= (cursor) '(0 0 1 1)))
  ;; a closing bracket typed further than the missing one: the pair is
  ;; extended up to the cursor
  (in-buffer '(document (math (concat "a" (around "(" "b" "<nobracket>") "cd")))
    (edit (go-to (at 0 0 2 1)) (math-bracket-close ")" "(" #f))
    (check= (body) '(document (math (concat "a" (around "(" "bc" ")") "d"))))
    (check= (cursor) '(0 0 1 1)))
  (in-buffer '(document (math (concat "ab" (around "<nobracket>" "c" ")"))))
    (edit (go-to (at 0 0 0 1)) (math-bracket-open "(" ")" #f))
    (check= (body) '(document (math (concat "a" (around "(" "bc" ")")))))
    (check= (cursor) '(0 0 1 1 0)))
  ;; separators
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (math-separator "|" #f))
    (check= (body) '(document (math "x|")))
    (edit (math-separator "|" #t))
    (check= (body) '(document (math (concat "x|" (mid "|"))))))
  ;; a separator given as a symbol keeps its brackets when small; a large
  ;; one takes the name without them, as tree-downgrade-brackets writes it
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (math-separator "<||>" #f))
    (check= (body) '(document (math "x<||>")))
    (edit (math-separator "<||>" #t))
    (check= (body) '(document (math (concat "x<||>" (mid "||")))))))

;; The shape and size of brackets: variant-circulate goes through the
;; brackets of the same kind, geometry-vertical makes them larger or
;; smaller (with an explicit size in left and right), geometry-default
;; goes back to the default size, alternate-toggle switches between small
;; and large brackets.
(define (test-bracket-shapes)
  (check-group "bracket shapes")
  (in-buffer '(document (math (around "(" "b" ")")))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (around "[" "b" "]"))))
    (edit (variant-circulate (bt 0 0) #f))
    (edit (variant-circulate (bt 0 0) #f))
    (check= (body) '(document (math (around "<llbracket>" "b" "<rrbracket>"))))
    (edit (geometry-vertical (bt 0 0) #f))
    (check= (body) '(document (math (around (left "<llbracket>" "1") "b"
                                            (right "<rrbracket>" "1")))))
    (edit (geometry-vertical (bt 0 0) #f))
    (check= (body) '(document (math (around (left "<llbracket>" "2") "b"
                                            (right "<rrbracket>" "2")))))
    (edit (geometry-vertical (bt 0 0) #t))
    (edit (geometry-vertical (bt 0 0) #t))
    ;; back at size 0: plain brackets again
    (check= (body) '(document (math (around "<llbracket>" "b" "<rrbracket>"))))
    (edit (geometry-vertical (bt 0 0) #t))
    (check= (body) '(document (math (around (left "<llbracket>" "-1") "b"
                                            (right "<rrbracket>" "-1")))))
    (edit (geometry-default (bt 0 0)))
    (check= (body) '(document (math (around "<llbracket>" "b" "<rrbracket>"))))
    (edit (alternate-toggle (bt 0 0)))
    (check= (body) '(document (math (around* "<llbracket>" "b" "<rrbracket>"))))
    (edit (alternate-toggle (bt 0 0)))
    (check= (body) '(document (math (around "<llbracket>" "b" "<rrbracket>")))))
  (in-buffer '(document (math (around "<langle>" "b" "<rangle>")))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (around "|" "b" "|")))))
  ;; left, mid and right brackets, without a size or with one
  (in-buffer '(document (math (concat (left "(") "b" (right ")"))))
    (edit (variant-circulate (bt 0 0 0) #t))
    (check= (body) '(document (math (concat (left "[") "b" (right ")")))))
    (edit (variant-circulate (bt 0 0 2) #f))
    (check= (body) '(document (math (concat (left "[") "b" (right "<rrbracket>"))))))
  (in-buffer '(document (math (around* "(" (concat "a" (mid "|") "b") ")")))
    (edit (variant-circulate (bt 0 0 1 1) #t))
    (check= (body) '(document (math (around* "(" (concat "a" (mid "<||>") "b")
                                              ")")))))
  (in-buffer '(document (math (concat (left "(" "1") "b" (right ")" "1"))))
    (edit (variant-circulate (bt 0 0 0) #t))
    (check= (body) '(document (math (concat (left "[" "1") "b" (right ")" "1")))))
    (edit (geometry-vertical (bt 0 0 0) #f))
    (check= (body) '(document (math (concat (left "[" "2") "b" (right ")" "1"))))))
  (in-buffer '(document (math (around* "(" (concat "a" (mid "|" "1") "b") ")")))
    (edit (variant-circulate (bt 0 0 1 1) #t))
    (check= (body) '(document (math (around* "(" (concat "a" (mid "<||>" "1") "b")
                                              ")")))))
  (check= (focus-tag-name 'around) "Around")
  (check= (focus-tag-name 'around*) "Around")
  (check= (focus-variants-of (stree->tree '(around "(" "" ")"))) '(around))
  (check= (standard-options 'around) '("math-brackets"))
  (check-true (focus-has-preferences? (stree->tree '(around "(" "" ")"))))
  ;; the brackets of a formula as the corrector sees them
  (check= (tree->stree (tree-upgrade-brackets (stree->tree "(a+b)") "math"))
          '(around "(" "a+b" ")"))
  (check= (tree->stree (tree-upgrade-brackets (stree->tree "(a+[b)") "math"))
          '(concat "(a+" (around "[" "b" ")")))
  (check= (tree->stree (tree-upgrade-brackets
                        (stree->tree '(concat (left "langle") "a" (mid "||")
                                              "b" (right "rangle")))
                        "math"))
          '(around* "<langle>" (concat "a" (mid "||") "b") "<rangle>"))
  (check= (tree->stree (tree-downgrade-brackets
                        (stree->tree '(around "(" "a+b" ")")) #t #f))
          "(a+b)")
  (check= (tree->stree (tree-downgrade-brackets
                        (stree->tree '(around* "<langle>" "a" "<rangle>"))
                        #t #f))
          '(concat (left "langle") "a" (right "rangle"))))

;; Backspace and delete over brackets: removing one bracket of a pair
;; leaves a missing bracket, removing the other one too removes the pair
;; and keeps its content, an empty pair goes at once.
(define (test-bracket-deletion)
  (check-group "bracket deletion")
  (in-buffer '(document (math (concat "a" (around "(" "b" ")") "c")))
    (edit (go-to (at 0 0 1 1)) (kbd-backspace))
    (check= (body) '(document (math (concat "a" (around "(" "b" "<nobracket>")
                                            "c"))))
    (check= (cursor) '(0 0 1 1 1))
    (edit (kbd-backspace))
    (check= (body) '(document (math (concat "a" (around "(" "" "<nobracket>")
                                            "c"))))
    (check= (cursor) '(0 0 1 1 0)))
  (in-buffer '(document (math (concat "a" (around "(" "b" ")") "c")))
    (edit (go-to (at 0 0 1 1 0)) (kbd-backspace))
    (check= (body) '(document (math (concat "a" (around "<nobracket>" "b" ")")
                                            "c"))))
    (check= (cursor) '(0 0 0 1))
    ;; the second bracket: the pair is gone
    (edit (go-to (at 0 0 1 1 1)) (kbd-delete))
    (check= (body) '(document (math "abc")))
    (check= (cursor) '(0 0 2)))
  (in-buffer '(document (math (concat "a" (around "(" "" ")") "c")))
    (edit (go-to (at 0 0 1 1 0)) (kbd-backspace))
    (check= (body) '(document (math "ac")))
    (check= (cursor) '(0 0 1)))
  (in-buffer '(document (math (concat "a" (around "(" "b" ")") "c")))
    (edit (go-to (at 0 0 1 0)) (kbd-delete))
    (check= (body) '(document (math (concat "a" (around "<nobracket>" "b" ")")
                                            "c"))))
    (check= (cursor) '(0 0 1 1 0)))
  ;; remove-structure-upwards keeps the content
  (in-buffer '(document (math (concat "x" (around "(" "a+b" ")") "y")))
    (edit (go-to (at 0 0 1 1 1)) (remove-structure-upwards))
    (check= (body) '(document (math "xa+by")))
    (check= (cursor) '(0 0 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ' makes a prime, a second one is added to the same prime; backspace
;; removes one prime at a time; tab after ' gives the other primes.
(define (test-primes)
  (check-group "primes")
  (in-buffer '(document (math "f"))
    (edit (go-to (at 0 0 1)) (key-press "'"))
    (check= (body) '(document (math (concat "f" (rprime "'")))))
    (check= (cursor) '(0 0 1 1))
    (edit (key-press "'"))
    (check= (body) '(document (math (concat "f" (rprime "''")))))
    (check= (cursor) '(0 0 1 1))
    (edit (kbd-backspace))
    (check= (body) '(document (math (concat "f" (rprime "'")))))
    (edit (kbd-backspace))
    (check= (body) '(document (math "f")))
    (check= (cursor) '(0 0 1))
    (edit (make-rprime "<dag>"))
    (check= (body) '(document (math (concat "f" (rprime "<dag>")))))
    (edit (go-to (at 0 0 0 0)) (make-lprime "`"))
    (check= (body) '(document (math (concat (lprime "`") "f" (rprime "<dag>")))))
    (check= (cursor) '(0 0 0 1)))
  (check= (typed "\"") '(math (concat "x" (rprime "''"))))
  (check= (typed "'" "tab") '(math (concat "x" (rprime "`"))))
  (check= (typed "'" "tab" "tab") '(math (concat "x" (rprime "<asterisk>"))))
  (check= (typed "`") '(math (concat "x" (lprime "`"))))
  (check= (typed "S-F7" "'") '(math (concat "x" (rprime "<dag>"))))
  ;; delete before a prime removes it
  (in-buffer '(document (math (concat "x" (rprime "'") "y")))
    (edit (go-to (at 0 0 0 1)) (kbd-delete))
    (check= (body) '(document (math "xy")))
    (check= (cursor) '(0 0 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wide accents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-wide and make-wide-under put an accent above or below; the accents
;; of a family are variants of each other (math-edit.scm, wide-list-1..5),
;; and alternate-toggle puts an accent below or above.
(define (test-wide)
  (check-group "wide accents")
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make-wide "^"))
    (check= (body) '(document (math (concat "x" (wide "" "^")))))
    (check= (cursor) '(0 0 1 0 0))
    (edit (insert "a") (go-to (at 0 0 1 1)) (make-wide "~") (insert "b"))
    (check= (body) '(document (math (concat "x" (wide "a" "^") (wide "b" "~")))))
    (edit (go-to (at 0 0 2 1)) (make-wide "<bar>") (insert "c"))
    (check= (tree->stree (bt 0 0 3)) '(wide "c" "<bar>"))
    (edit (go-to (at 0 0 3 1)) (make-wide-under "<wide-bar>") (insert "d"))
    (check= (tree->stree (bt 0 0 4)) '(wide* "d" "<wide-bar>"))
    (check= (cursor) '(0 0 4 0 1)))
  (in-buffer '(document (math "xab"))
    (edit (selection-set (at 0 0 1) (at 0 0 3)) (make-wide "^"))
    (check= (body) '(document (math (concat "x" (wide "ab" "^")))))
    (check= (cursor) '(0 0 1 1)))
  (in-buffer '(document (math (wide "a" "^")))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (wide "a" "<bar>"))))
    (edit (variant-circulate (bt 0 0) #f))
    (edit (variant-circulate (bt 0 0) #f))
    (check= (body) '(document (math (wide "a" "~"))))
    (edit (variant-circulate (bt 0 0) #f))
    (check= (body) '(document (math (wide "a" "<invbreve>"))))
    (edit (alternate-toggle (bt 0 0)))
    (check= (body) '(document (math (wide* "a" "<invbreve>"))))
    (edit (alternate-toggle (bt 0 0)))
    (check= (body) '(document (math (wide "a" "<invbreve>")))))
  (in-buffer '(document (math (wide "a" "<acute>")))
    (edit (variant-circulate (bt 0 0) #f))
    (check= (body) '(document (math (wide "a" "<abovering>")))))
  (in-buffer '(document (math (wide "a" "<wide-overbrace>")))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (wide "a" "<wide-underbrace*>")))))
  (in-buffer '(document (math (wide "a" "<wide-bar>")))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (wide "a" "<wide-varrightarrow>")))))
  ;; an accent of no family stays
  (in-buffer '(document (math (wide "a" "<bind>")))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (wide "a" "<bind>")))))
  ;; backspace in an empty accent removes it
  (in-buffer '(document (math (concat "x" (wide "" "^") "y")))
    (edit (go-to (at 0 0 1 0 0)) (kbd-backspace))
    (check= (body) '(document (math "xy")))
    (check= (cursor) '(0 0 1)))
  ;; at the start of a non empty one, the cursor leaves it
  (in-buffer '(document (math (concat "x" (wide "a" "^") "y")))
    (edit (go-to (at 0 0 1 0 0)) (kbd-backspace))
    (check= (body) '(document (math (concat "x" (wide "a" "^") "y"))))
    (check= (cursor) '(0 0 0 1))
    (edit (go-to (at 0 0 1 0 0)) (remove-structure-upwards))
    (check= (body) '(document (math "xay"))))
  (check= (focus-tag-name 'wide) "Wide")
  (check= (focus-variants-of (stree->tree '(wide* "a" "^"))) '(wide)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Negations, trees, scripts above and below, long arrows and rigid
;; groups, as the Insert menu makes them.
(define (test-other-structures)
  (check-group "other structures")
  (in-buffer '(document (math ""))
    (edit (go-to (at 0 0 0)) (make-neg))
    (check= (body) '(document (math (neg ""))))
    (check= (cursor) '(0 0 0 0))
    (edit (insert "="))
    (edit (go-to (at 0 0 1)) (make-tree))
    (check= (body) '(document (math (concat (neg "=") (tree "" "")))))
    (check= (cursor) '(0 0 1 0 0))
    (edit (go-to (at 0 0 1 1)) (make-above))
    (check= (tree->stree (bt 0 0 2)) '(above "" ""))
    (check= (cursor) '(0 0 2 0 0))
    (edit (go-to (at 0 0 2 1)) (make-below))
    (check= (tree->stree (bt 0 0 3)) '(below "" ""))
    (check= (cursor) '(0 0 3 0 0))
    (edit (go-to (at 0 0 3 1)) (make-long-arrow "<rightarrow>"))
    (check= (tree->stree (bt 0 0 4)) '(long-arrow "<rubber-rightarrow>" ""))
    (check= (cursor) '(0 0 4 1 0))
    (edit (go-to (at 0 0 4 1)) (make-long-arrow* "<leftarrow>"))
    (check= (tree->stree (bt 0 0 5)) '(long-arrow "<rubber-leftarrow>" "" ""))
    (check= (cursor) '(0 0 5 2 0))
    ;; not a symbol: no arrow
    (edit (go-to (at 0 0 5 1)) (make-long-arrow "x"))
    (check= (tm-arity (bt 0 0)) 6)
    (edit (make-rigid))
    (check= (tree->stree (bt 0 0 6)) '(rigid ""))
    (check= (cursor) '(0 0 6 0 0)))
  ;; a branch after the last one of a tree
  (in-buffer '(document (math (concat "x" (tree "r" "a") "y")))
    (edit (go-to (at 0 0 1 1 1)) (structured-insert-right))
    (check= (body) '(document (math (concat "x" (tree "r" "a" "") "y"))))
    (check= (cursor) '(0 0 1 2 0))
    ;; backspace in the empty branch removes it
    (edit (kbd-backspace))
    (check= (body) '(document (math (concat "x" (tree "r" "a") "y"))))
    (check= (cursor) '(0 0 1 1 1)))
  ;; a tree reduced to its root becomes the root
  (in-buffer '(document (math (concat "x" (tree "r" "") "y")))
    (edit (go-to (at 0 0 1 1 0)) (kbd-backspace))
    (check= (body) '(document (math "xry")))
    (check= (cursor) '(0 0 2)))
  (in-buffer '(document (math (concat "x" (long-arrow "<rubber-rightarrow>" "")
                                      "y")))
    (edit (go-to (at 0 0 1 1 0)) (kbd-backspace))
    (check= (body) '(document (math "xy")))
    (check= (cursor) '(0 0 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tables of mathematics (Insert > Table and math t): a matrix starts
;; with one cell; structured-insert-right and -down add a column and a row,
;; and the structured movements go from cell to cell.
(define (test-matrices)
  (check-group "matrices")
  (in-buffer '(document (math ""))
    (edit (go-to (at 0 0 0)) (make 'matrix))
    (check= (body) '(document (math (matrix (tformat (table (row (cell ""))))))))
    (check= (cursor) '(0 0 0 0 0 0 0 0))
    (check= (get-env "mode") "math")
    (edit (insert "a") (structured-insert-right))
    (check= (body) '(document (math (matrix (tformat (table
                                (row (cell "a") (cell ""))))))))
    (check= (cursor) '(0 0 0 0 0 1 0 0))
    (edit (insert "b") (structured-insert-down))
    (check= (body) '(document (math (matrix (tformat (table
                                (row (cell "a") (cell "b"))
                                (row (cell "") (cell ""))))))))
    (check= (cursor) '(0 0 0 0 1 1 0 0))
    (edit (table-insert-column #t))
    (check= (body) '(document (math (matrix (tformat (table
                                (row (cell "a") (cell "b") (cell ""))
                                (row (cell "") (cell "") (cell ""))))))))
    (check= (cursor) '(0 0 0 0 1 2 0 0))
    (edit (structured-left))
    (check= (cursor) '(0 0 0 0 1 1 0 0))
    (edit (structured-up))
    (check= (cursor) '(0 0 0 0 0 1 0 1))
    (edit (structured-right))
    (check= (cursor) '(0 0 0 0 0 2 0 0))
    (edit (structured-remove-right))
    (check= (body) '(document (math (matrix (tformat (table
                                (row (cell "a") (cell "b"))
                                (row (cell "") (cell "")))))))))
  (in-buffer '(document (math ""))
    (edit (go-to (at 0 0 0)) (make 'bmatrix))
    (check= (body) '(document (math (bmatrix (tformat (table (row (cell ""))))))))
    (edit (variant-circulate (bt 0 0) #t))
    (check= (body) '(document (math (Bmatrix (tformat (table (row (cell "")))))))))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make 'det))
    (check= (body) '(document (math (concat "x" (det (tformat (table
                                                   (row (cell ""))))))))))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make 'choice))
    (check= (tree->stree (bt 0 0 1)) '(choice (tformat (table (row (cell "")))))))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (make 'stack))
    (check= (tree->stree (bt 0 0 1)) '(stack (tformat (table (row (cell ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Backspace after a structure enters it, backspace at the start of a
;; non empty structure leaves it, backspace in an empty structure removes
;; it; remove-structure-upwards removes the structure around the cursor
;; and keeps the argument the cursor is in.
(define (test-remove)
  (check-group "remove")
  (in-buffer '(document (math (concat "x" (frac "a" "b") "y")))
    (edit (go-to (at 0 0 1 1)) (kbd-backspace))
    (check= (body) '(document (math (concat "x" (frac "a" "b") "y"))))
    (check= (cursor) '(0 0 1 1 1))
    (edit (kbd-backspace))
    (check= (body) '(document (math (concat "x" (frac "a" "") "y"))))
    (check= (cursor) '(0 0 1 1 0))
    ;; backspace from the empty denominator goes to the numerator
    (edit (kbd-backspace))
    (check= (body) '(document (math (concat "x" (frac "a" "") "y"))))
    (check= (cursor) '(0 0 1 0 1)))
  (in-buffer '(document (math (concat "x" (frac "a" "b") "y")))
    (edit (go-to (at 0 0 1 0)) (kbd-delete))
    (check= (body) '(document (math (concat "x" (frac "a" "b") "y"))))
    (check= (cursor) '(0 0 1 0 0))
    (edit (kbd-backspace))
    (check= (cursor) '(0 0 0 1)))
  (in-buffer '(document (math (concat "x" (frac "" "") "y")))
    (edit (go-to (at 0 0 1 0 0)) (kbd-backspace))
    (check= (body) '(document (math "xy")))
    (check= (cursor) '(0 0 1)))
  (in-buffer '(document (math (concat "x" (rsub "i") "y")))
    (edit (go-to (at 0 0 1 1)) (kbd-backspace))
    (check= (cursor) '(0 0 1 0 1))
    (edit (kbd-backspace))
    (check= (body) '(document (math (concat "x" (rsub "") "y"))))
    (edit (kbd-backspace))
    (check= (body) '(document (math "xy")))
    (check= (cursor) '(0 0 1)))
  (in-buffer '(document (math (concat "x" (rsub "i") "y")))
    (edit (go-to (at 0 0 1 0 0)) (kbd-backspace))
    (check= (body) '(document (math (concat "x" (rsub "i") "y"))))
    (check= (cursor) '(0 0 0 1)))
  (in-buffer '(document (math (concat "x" (sqrt "") "y")))
    (edit (go-to (at 0 0 1 0 0)) (kbd-backspace))
    (check= (body) '(document (math "xy"))))
  (in-buffer '(document (math (concat "x" (sqrt "2") "y")))
    (edit (go-to (at 0 0 1 0 0)) (kbd-backspace))
    (check= (body) '(document (math (concat "x" (sqrt "2") "y"))))
    (check= (cursor) '(0 0 0 1)))
  (in-buffer '(document (math (concat "x" (neg "=") "y")))
    (edit (go-to (at 0 0 1 1)) (kbd-backspace))
    (check= (cursor) '(0 0 1 0 1)))
  (in-buffer '(document (math (concat "x" (frac "a" "b") "y")))
    (edit (go-to (at 0 0 1 1 0)) (remove-structure-upwards))
    (check= (body) '(document (math "xby")))
    (check= (cursor) '(0 0 1)))
  (in-buffer '(document (math (concat "x" (rsup "2") "y")))
    (edit (go-to (at 0 0 1 0 1)) (remove-structure-upwards))
    (check= (body) '(document (math "x2y")))
    (check= (cursor) '(0 0 2)))
  ;; a selection over a structure
  (in-buffer '(document (math (concat "x" (frac "a" "b") "y")))
    (edit (selection-set (at 0 0 0 1) (at 0 0 2 0)) (kbd-backspace))
    (check= (body) '(document (math "xy")))
    (check= (cursor) '(0 0 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The focus is the innermost structure; structured-exit leaves it; the
;; traverse commands and go-to-next-tag go through the arguments of the
;; structures of the same kind.
(define (test-navigation)
  (check-group "navigation")
  (in-buffer '(document (math (concat "x" (frac "a" "b") "y" (frac "c" "d"))))
    (edit (go-to (at 0 0 1 0 1)))
    (check= (tree->stree (focus-tree)) '(frac "a" "b"))
    (check-true (cursor-inside? (bt 0 0 1)))
    (edit (structured-exit-right))
    (check= (cursor) '(0 0 1 1))
    (edit (go-to (at 0 0 1 0 1)) (structured-exit-left))
    (check= (cursor) '(0 0 0 1))
    (edit (go-to (at 0 0 1 0 1)) (traverse-next))
    (check= (cursor) '(0 0 1 1 0))
    (edit (traverse-previous))
    (check= (cursor) '(0 0 1 0 1))
    (edit (traverse-last))
    (check= (cursor) '(0 0 3 1 0))
    (edit (traverse-first))
    (check= (cursor) '(0 0 1 0 1))
    (edit (go-to (at 0 0 1 1 1)) (go-to-next-tag 'frac))
    (check= (cursor) '(0 0 3 0 0))
    (edit (go-to-previous-tag 'frac))
    (check= (cursor) '(0 0 1 1 1))
    (edit (go-end-of 'frac))
    (check= (cursor) '(0 0 1 1))
    (edit (go-to (at 0 0 3 0 1)) (go-start-of 'frac))
    (check= (cursor) '(0 0 2 1))
    (edit (go-end-of 'math))
    (check= (cursor) '(0 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Symbols typed as combinations of keys (math-kbd.scm and the symbols of
;; the keyboard), Greek letters with tab, and the F5, F6, F7 and S-F6
;; prefixes for letters and symbols.
(define (test-keyboard)
  (check-group "keyboard")
  (check= (typed "<") '(math "x<less>"))
  (check= (typed "<" "=") '(math "x<leqslant>"))
  (check= (typed "<" "=" "tab") '(math "x<leq>"))
  (check= (typed ">" "=") '(math "x<geqslant>"))
  (check= (typed "<" "<") '(math "x<ll>"))
  (check= (typed "<" "tab") '(math "x<in>"))
  (check= (typed "<" "tab" "tab") '(math "x<subset>"))
  (check= (typed "-" ">") '(math "x<rightarrow>"))
  (check= (typed "<" "-") '(math "x<leftarrow>"))
  (check= (typed "=" ">") '(math "x<Rightarrow>"))
  (check= (typed "+" "-") '(math "x<pm>"))
  (check= (typed "=" "/") '(math "x<neq>"))
  (check= (typed "=" "tab") '(math "x<asymp>"))
  (check= (typed "+" "=") '(math "x<plusassign>"))
  (check= (typed "-" "-") '(math "x<longminus>"))
  (check= (typed "=" "=") '(math "x<longequal>"))
  (check= (typed "," ",") '(math "x,<ldots>,"))
  (check= (typed "." ".") '(math "x<ldots>"))
  (check= (typed "." "tab") '(math "x<point>"))
  (check= (typed "@") '(math "x<circ>"))
  (check= (typed "~") '(math "x<sim>"))
  (check= (typed "*") '(math "x*"))
  (check= (typed "a" "tab") '(math "x<alpha>"))
  (check= (typed "p" "tab") '(math "x<pi>"))
  (check= (typed "e" "tab") '(math "x<varepsilon>"))
  (check= (typed "S-F7" "a") '(math "x<alpha>"))
  (check= (typed "F7" "R") '(math "x<cal-R>"))
  (check= (typed "S-F6" "R") '(math "x<bbb-R>"))
  (check= (typed "plusminus") '(math "x<pm>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spaces and symbol types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With the default "math spacebar", a space is only typed where it can
;; mean something (after a symbol, not after an operator), a second one
;; becomes a space of 1em, a third one a wider space; a space before an
;; infix operator or a separator is removed when the operator is typed.
(define (test-spaces)
  (check-group "spaces")
  (check= (get-preference "math spacebar") "default")
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (kbd-space-bar (buffer-tree) #f))
    (check= (body) '(document (math "x ")))
    (edit (kbd-space-bar (buffer-tree) #f))
    (check= (body) '(document (math (concat "x" (space "1em")))))
    (edit (kbd-space-bar (buffer-tree) #f))
    (check= (body) '(document (math (concat "x" (space "2em"))))))
  (in-buffer '(document (math "x+"))
    (edit (go-to (at 0 0 2)) (kbd-space-bar (buffer-tree) #f))
    (check= (body) '(document (math "x+"))))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (key-press "space"))
    (check= (body) '(document (math "x ")))
    (edit (key-press "+"))
    (check= (body) '(document (math "x+")))
    (edit (key-press "space") (key-press "y"))
    (check= (body) '(document (math "x+y"))))
  (in-buffer '(document (math "x"))
    (edit (go-to (at 0 0 1)) (key-press "space") (key-press ","))
    (check= (body) '(document (math "x,"))))
  (check-true (allow-space-after? "x"))
  (check-false (allow-space-after? "+"))
  (check-false (allow-space-after? "("))
  (check-false (allow-space-after? ","))
  (check-false (allow-space-after? (stree->tree '(big "sum"))))
  (check-true (allow-space-after? (stree->tree '(frac "a" "b"))))
  (check-false (allow-space-after? #f))
  (check= (math-symbol-type "x") "symbol")
  (check= (math-symbol-type "+") "prefix-infix")
  (check= (math-symbol-type "<leq>") "infix")
  (check= (math-symbol-type "(") "opening-bracket")
  (check= (math-symbol-type ")") "closing-bracket")
  (check= (math-symbol-type "|") "middle-bracket")
  (check= (math-symbol-type ",") "separator")
  (check= (math-symbol-type "!") "postfix")
  (check= (math-symbol-type "<neg>") "prefix")
  (check= (math-symbol-group "<forall>") "Quantifier-symbol")
  (check= (math-symbol-group "<leq>") "Relation-nolim-symbol")
  (check= (math-symbol-group "+") "Plus-visible-symbol"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Correction of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; manual-correct is what Edit > Correct all does to the formulas: brackets
;; become around tags, negations and := become their symbols (homoglyphs),
;; superfluous spaces and multiplications go, missing multiplications are
;; inserted, punctuation leaves the formula, nested scripts are simplified.
;; Text outside formulas is not changed.
(define (test-correct)
  (check-group "correct")
  (check= (correct '(math (concat "a" (neg "=") "b"))) '(math "a<neq>b"))
  (check= (correct '(math (concat "x" (neg "<in>") "A"))) '(math "x<nin>A"))
  (check= (correct '(math (concat "x" (neg "<leq>") "y"))) '(math "x<nleq>y"))
  (check= (correct '(math "a:=b")) '(math "a<assign>b"))
  (check= (correct '(math "A\\B")) '(math "A<setminus>B"))
  (check= (correct '(math "a<minus>b")) '(math "a-b"))
  (check= (correct '(math "2x")) '(math "2*x"))
  (check= (correct '(math "a+ b")) '(math "a+b"))
  (check= (correct '(math "a* b")) '(math "a*b"))
  (check= (correct '(math "a*+b")) '(math "a+b"))
  (check= (correct '(math "f(x)")) '(math (concat "f" (around "(" "x" ")"))))
  (check= (correct '(math "(a+b)")) '(math (around "(" "a+b" ")")))
  (check= (correct '(math "[a,b)")) '(math (around "[" "a,b" ")")))
  (check= (correct '(math (concat (left "(") "a" (right ")"))))
          '(math (around* "(" "a" ")")))
  (check= (correct '(math "x+y.")) '(concat (math "x+y") "."))
  (check= (correct '(math "x,")) '(concat (math "x") ","))
  (check= (correct '(math ".")) ".")
  (check= (correct '(math (rsub "i"))) '(rsub (math "i")))
  (check= (correct '(math (concat "x" (rsub (rsub "i")))))
          '(math (concat "x" (rsub "i"))))
  (check= (correct '(math (concat "x" (rsup (rprime "'")))))
          '(math (concat "x" (rprime "'"))))
  (check= (correct '(math (concat "x" (rsub (text "max")))))
          '(math (concat "x" (rsub "max"))))
  (check= (correct '(math (concat "x" (sqrt "2" "")))) '(math (concat "x" (sqrt "2"))))
  (check= (correct '(math (concat "x" (rsub ""))))
          '(math (concat "x" (rsub "<nosymbol>"))))
  (check= (correct '(math (concat (big "sum") (rsub "i") "x" (big "."))))
          '(math (concat (big "sum") (rsub "i") "x")))
  (check= (correct '(concat "a" (neg "=") "b")) '(concat "a" (neg "=") "b"))
  (check= (correct '(math (concat "x" (with "mode" "text" "a b"))))
          '(math (concat "x" (with "mode" "text" "a b"))))
  (check= (tree->stree (invisible-correct-superfluous (stree->tree '(math "a +b"))))
          '(math "a+b"))
  (check= (tree->stree (invisible-correct-missing (stree->tree '(math "2x")) -1))
          '(math "2*x"))
  (check= (tree->stree (invisible-correct-missing (stree->tree '(math "ab")) 1))
          '(math "ab"))
  ;; in a buffer: all formulas, or the selected subtree
  (in-buffer '(document (concat "a " (math (concat "x" (neg "=") "y"))
                                " and " (math "x+y.")))
    (edit (math-correct-all))
    (check= (body) '(document (concat "a " (math "x<neq>y") " and "
                                      (math "x+y") "."))))
  (in-buffer '(document (concat (math (concat "x" (neg "=") "y")) " and "
                                (math (concat "x" (neg "=") "y"))))
    (edit (selection-set (at 0 0 0 0) (at 0 2 0 1)))
    (check= (tree->stree (selection-tree))
            '(concat (math (concat "x" (neg "=") "y")) " and "
                     (math (concat "x" (neg "=") "y"))))
    (edit (math-correct-all))
    (check= (body) '(document (concat (math "x<neq>y") " and " (math "x<neq>y"))))))

;; The grammar of formulas (std-math), which semantic editing checks after
;; each change (math-correct? in math-sem-edit.scm).
(define (test-grammar)
  (check-group "grammar")
  (check-true (grammar-ok? "Main" "a+b"))
  (check-false (grammar-ok? "Main" "a+"))
  (check-true (grammar-ok? "Main" "a=b"))
  (check-true (grammar-ok? "Main" "a<leq>b<less>c"))
  (check-true (grammar-ok? "Main" "2*x"))
  (check-true (grammar-ok? "Main" "<forall>x,x=x"))
  (check-true (grammar-ok? "Main" '(concat "a" (rsub "i"))))
  (check-true (grammar-ok? "Main" '(frac "1" "2")))
  (check-true (grammar-ok? "Main" '(around "(" "a+b" ")")))
  (check-false (grammar-ok? "Main" '(around "(" "a+" ")")))
  (check-true (grammar-ok? "Strict" "a,b"))
  (check-false (grammar-ok? "Main" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each step of math editing is undone and redone as a whole.
(define (test-undo)
  (check-group "undo")
  ;; the formula is typed, the body set with the buffer is not undone
  (in-buffer '(document "")
    (edit (make 'math) (insert "x"))
    (edit (make-fraction))
    (edit (insert "1"))
    (edit (go-to (at 0 0 1 1)) (key-press "^"))
    (check= (body) '(document (math (concat "x" (frac "1" "") (rsup "")))))
    (edit (undo 0))
    (check= (body) '(document (math (concat "x" (frac "1" "")))))
    (edit (undo 0))
    (check= (body) '(document (math (concat "x" (frac "" "")))))
    (edit (undo 0))
    (check= (body) '(document (math "x")))
    (edit (redo 0))
    (check= (body) '(document (math (concat "x" (frac "" "")))))
    (edit (redo 0))
    (edit (redo 0))
    (check= (body) '(document (math (concat "x" (frac "1" "") (rsup "")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-group thunk)
  ;; an error in a group counts as one failure, the next groups still run
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (math-edit-test-failures)
  (check-suite "math-edit")
  (for-each run-group
            (list test-enter-math test-formula-variants test-fractions
                  test-scripts test-roots test-big-operators test-brackets
                  test-bracket-shapes test-bracket-deletion test-primes
                  test-wide test-other-structures test-matrices test-remove
                  test-navigation test-keyboard test-spaces test-correct
                  test-grammar test-undo))
  (check-end))
