;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : typeset-test.scm
;; DESCRIPTION : tests of the typesetter, without a window
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The typesetter as Scheme sees it, through three windows:
;;
;;   - texmacs-expand evaluates a TeXmacs expression in the environment at
;;     the start of the current buffer (Typeset/Env/env_exec.cpp), and
;;     restores the environment afterwards, so that an assign does not
;;     leak; texmacs-exec evaluates in the editor's environment as it is
;;     and is not used here for that reason;
;;   - the box-info primitive typesets its argument as a line and returns
;;     the extents of the box in tmpt (Typeset/Concat/concater.cpp,
;;     box_info): 'w' and 'h' are the logical width and height, 'l' 'r' the
;;     left and right sides; a par-block inside it is broken into lines at
;;     the par-width, so that its height counts the lines;
;;   - after update-forced, a buffer is typeset: get-env-tree-at gives the
;;     environment at a path, get-reference the numbers of the labels,
;;     get-page-count the number of pages, and tree-bounding-rectangle the
;;     rectangle of a subtree on the canvas (y grows upwards, the pages are
;;     stacked downwards).
;;
;; Font metrics differ from one installation to another, so that box sizes
;; are compared with each other (wider, taller, a multiple of a line) and
;; exact values are only checked where fonts do not enter: units, explicit
;; spaces, moves and resizes, numbers.
;;
;; Things which need a window are left out: the cursor position in pixels
;; (get-cursor-x and get-cursor-y are 0 without a view on the screen) and
;; the cursor movements through boxes. par-width is a page parameter (the
;; text width of the document, Env/env_semantics.cpp, update_page_pars),
;; not a paragraph one: a paragraph is narrowed with par-left and
;; par-right, or set at a width of its own in a par-block.

(texmacs-module (check typeset-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ev t)
  ;; the value of the TeXmacs expression @t, as a Scheme tree
  (tree->stree (texmacs-expand t)))

(define (box t what)
  ;; the extents @what (letters of box-info) of the box of @t, in tmpt
  (with r (ev `(box-info ,t ,what))
    (if (and (pair? r) (== (car r) 'tuple))
        (map string->number (cdr r))
        r)))

(define (width t) (car (box t "w")))
(define (height t) (car (box t "h")))
(define (math t) `(with "mode" "math" ,t))

(define (block w t)
  ;; a paragraph @t broken at the width @w
  `(with "par-width" ,w (par-block (document ,t))))

(define (lines w t)
  ;; the number of lines of the paragraph @t at the width @w
  (let ((h (height (block w t)))
        (h1 (height (block w "x"))))
    (inexact->exact (round (/ h h1)))))

(define (tmpt len) (length-decode len))

(define (near? x y tol) (<= (abs (- x y)) tol))

(define lorem
  (string-append
   "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do "
   "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim "
   "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
   "aliquip ex ea commodo consequat."))

(define (at . l)
  ;; the absolute path of @l in the current buffer
  (append (buffer-path) l))

(define (rect . l)
  ;; the rectangle (x1 y1 x2 y2) of the subtree at @l of the current buffer
  (tree-bounding-rectangle (path->tree (apply at l))))

(define (rect-x1 r) (car r))
(define (rect-y1 r) (cadr r))
(define (rect-x2 r) (caddr r))
(define (rect-y2 r) (cadddr r))
(define (rect-mid r) (/ (+ (rect-x1 r) (rect-x2 r)) 2))

(define (env-at var . l)
  (tree->stree (get-env-tree-at var (apply at l))))

(define (with-typeset-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, typeset, then close it
  (let* ((old (current-buffer))
         (u (new-buffer)))
    ;; new-buffer shows the buffer already (see editing-test.scm)
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (with-exact-environments thunk)
  ;; get-env-tree-at with the full evaluation of the document up to the
  ;; path, instead of the fast approximation which ignores the counters
  (let ((fast? (== (get-preference "fast environments") "on")))
    (set-fast-environments #f)
    (with r (check-run thunk)
      (set-fast-environments fast?)
      r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Arithmetic, strings and tuples of the style language, and the number
;; formats of the counters.
(define (test-arithmetic)
  (check-group "arithmetic")
  (check= (ev '(plus "1" "2")) "3")
  (check= (ev '(plus "1" "2" "3")) "6")
  (check= (ev '(minus "1" "4")) "-3")
  (check= (ev '(times "3" "4")) "12")
  (check= (ev '(over "1" "4")) "0.25")
  (check= (ev '(div "17" "5")) "3")
  (check= (ev '(mod "17" "5")) "2")
  (check= (ev '(minimum "3" "10" "2")) "2")
  (check= (ev '(maximum "3" "10" "2")) "10")
  (check= (ev '(plus (times "2" "3") (over "8" "2"))) "10")
  (check= (ev '(merge "ab" "cd")) "abcd")
  (check= (ev '(length "abcd")) "4")
  (check= (ev '(range "abcdef" "1" "3")) "bc")
  (check= (ev '(length (tuple "a" "b"))) "2")
  (check= (ev '(look-up (tuple "a" "b" "c") "1")) "b")
  (check= (ev '(is-tuple (tuple "a"))) "true")
  (check= (ev '(is-tuple "a")) "false")
  (check= (ev '(number "12" "arabic")) "12")
  (check= (ev '(number "3" "roman")) "iii")
  (check= (ev '(number "4" "Roman")) "IV")
  (check= (ev '(number "2" "alpha")) "b")
  (check= (ev '(number "3" "Alpha")) "C"))

;; Booleans and the conditionals if and case.
(define (test-conditionals)
  (check-group "conditionals")
  (check= (ev '(equal "a" "a")) "true")
  (check= (ev '(equal "a" "b")) "false")
  (check= (ev '(unequal "a" "b")) "true")
  (check= (ev '(less "3" "10")) "true")
  (check= (ev '(greater "3" "10")) "false")
  (check= (ev '(not "true")) "false")
  (check= (ev '(and "true" "false")) "false")
  (check= (ev '(or "true" "false")) "true")
  (check= (ev '(if (equal "1" "1") "a" "b")) "a")
  (check= (ev '(if (equal "1" "2") "a" "b")) "b")
  (check= (ev '(if (less "2" "1") "a")) "")
  (check= (ev '(case (equal "1" "2") "a" (equal "1" "1") "b" "c")) "b")
  (check= (ev '(case (equal "1" "2") "a" "c")) "c"))

;; Macros: arguments are substituted, an xmacro reaches its arguments by
;; number, with binds a variable or a macro for its body (and stays in the
;; result, with the quoted value), an assign is seen by what follows it and
;; is forgotten after the evaluation, value and provides look up the
;; environment of the document (the generic style).
(define (test-macros)
  (check-group "macros")
  (check= (ev '(compound (macro "x" (concat (arg "x") (arg "x"))) "ab"))
          "abab")
  (check= (ev '(with "foo" (macro "a" "b" (concat (arg "b") (arg "a")))
                 (foo "1" "2")))
          '(with "foo" (quote (macro "a" "b" (concat (arg "b") (arg "a"))))
             "21"))
  (check= (ev '(with "foo" (xmacro "a" (arg "a" "1")) (foo "1" "2")))
          '(with "foo" (quote (xmacro "a" (arg "a" "1"))) "2"))
  (check= (ev '(with "foo" (macro "a" (if (equal (arg "a") "") "e" "f"))
                 (concat (foo "") (foo "z"))))
          '(with "foo" (quote (macro "a" (if (equal (arg "a") "") "e" "f")))
             "ef"))
  (check= (ev '(with "x" "3" (value "x"))) '(with "x" "3" "3"))
  (check= (ev '(with "x" "3" (plus (value "x") "1"))) '(with "x" "3" "4"))
  (check= (ev '(concat (assign "foo" (macro "x" (concat "<" (arg "x") ">")))
                       (foo "y")))
          '(concat (assign "foo" (macro "x" (concat "<" (arg "x") ">"))) "<y>"))
  (check= (ev '(provides "foo")) "false")
  (check= (ev '(provides "section")) "true")
  (check= (ev '(provides "no-such-macro")) "false")
  (check= (ev '(value "font-base-size")) "10")
  (check= (ev '(value "mode")) "text")
  (check= (ev '(with "mode" "math" (value "mode"))) '(with "mode" "math" "math")))

;; Lengths: the units are fixed ratios of the inch (Env/env_length.cpp),
;; the operators on lengths give tmlen values which compare equal when
;; they are the same length, the font units scale with the font size, and
;; the length functions of the editor agree.
(define (test-lengths)
  (check-group "lengths")
  (check= (ev '(over "1cm" "1mm")) "10")
  (check= (ev '(over "1in" "1cm")) "2.54")
  (check= (ev '(over "1pc" "1pt")) "12")
  (check= (ev '(over "1bp" "1pt")) "1.00375")
  (check= (ev '(over "1dd" "1mm")) "0.376")
  (check= (ev '(over "1tmpt" "1tmpt")) "1")
  (check= (ev '(equal "1cm" "10mm")) "true")
  (check= (ev '(less "1mm" "1cm")) "true")
  (check= (ev '(greater "1mm" "1cm")) "false")
  (check= (ev '(equal (times "2" "1cm") (plus "1cm" "1cm"))) "true")
  (check= (ev '(equal (times "2" "1cm") (times "1cm" "2"))) "true")
  ;; the results of the operators are tmlen trees, which equal compares
  ;; as lengths only with each other, and which over divides
  (check= (ev '(equal (plus "1cm" "1cm") "2cm")) "false")
  (check= (ev '(over (minimum "1cm" "1mm") "1mm")) "1")
  (check= (ev '(over (maximum "1cm" "1mm") "1mm")) "10")
  (check-true (near? (string->number (ev '(over (minus "1cm" "1mm") "1mm")))
                     9.0 0.001))
  (check= (ev '(over (plus "1cm" "1mm") "1mm")) "11")
  (check-true (pair? (ev '(plus "1cm" "1mm"))))
  (check= (car (ev '(plus "1cm" "1mm"))) 'tmlen)
  ;; the paragraph of the default a4 page is 15cm wide
  (check= (ev '(over "1par" "1cm")) "15")
  ;; the font units follow the font size
  (let ((fn10 (string->number (ev '(over "1fn" "1pt"))))
        (fn20 (string->number
               (cadddr (ev '(with "font-base-size" "20" (over "1fn" "1pt")))))))
    (check-true (near? fn10 10.0 1.0))
    (check-true (near? fn20 (* 2 fn10) 0.01)))
  (check= (tmpt "1tmpt") 1)
  (check= (tmpt "10mm") (tmpt "1cm"))
  (check-true (near? (tmpt "1in") (* 2.54 (tmpt "1cm")) 2))
  (check-true (near? (tmpt "72.27pt") (tmpt "1in") 2))
  (check-true (length? "1cm"))
  (check-true (length? "2.5fn"))
  (check-false (length? "abc"))
  (check= (length-add "1cm" "1mm") "1.1cm")
  (check-true (near? (length-divide (length-sub "1cm" "1mm") "1mm") 9.0 0.001))
  (check= (length-max "1cm" "1mm") "1cm")
  (check= (length-min "1cm" "1mm") "1mm")
  (check= (length-mult 3.0 "1cm") "3cm")
  (check-true (near? (length-divide "1cm" "1mm") 10.0 0.001)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The environment of a typeset document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-env-tree-at sees the variables set by with and by the environments
;; around a path; the counters (section-nr, equation-nr) are only seen
;; when the environment is evaluated exactly, the fast approximation
;; evaluates the with and the macros on the way to the path only.
;; get-env-tree-at sees the variables set by with and by the environments
;; around a path; the counters (section-nr, equation-nr) are only seen
;; when the environment is evaluated exactly, the fast approximation
;; evaluates the with and the macros on the way to the path only. The
;; environment at a path is computed once and kept until the document
;; changes, whichever way it was computed, so that the exact environments
;; are asked for first.
(define (test-environment)
  (check-group "environment")
  (with-typeset-body
   '(document (section "A") (section "B") "x"
              (with "par-mode" "center" "font-shape" "italic"
                (document "centered"))
              (equation "a") (equation "b") "y")
   (lambda ()
     (with-exact-environments
      (lambda ()
        (check= (env-at "section-nr" 0 0 0) "1")
        (check= (env-at "section-nr" 1 0 0) "2")
        (check= (env-at "section-nr" 2 0) "2")
        (check= (env-at "par-mode" 3 4 0 0) "center")
        (check= (env-at "mode" 4 0 0) "math")
        (check= (env-at "math-display" 4 0 0) "true")
        (check= (env-at "equation-nr" 4 0 0) "1")
        (check= (env-at "equation-nr" 6 0) "2")))
     ;; the language package of the locale (buffer-set-default-style), none
     ;; for English
     (with lan (get-preference "language")
       (check= (get-style-list)
               (if (== lan "english") '("generic") (list "generic" lan))))
     (check= (get-init "font-base-size") "10")
     (check= (env-at "mode" 2 1) "text")
     (check= (env-at "par-mode" 3 4 0 1) "center")
     (check= (env-at "font-shape" 3 4 0 1) "italic")
     (check= (env-at "par-mode" 6 1) "justify")
     (check= (env-at "font-shape" 6 1) "right")
     (check= (env-at "mode" 5 0 1) "math")
     ;; the fast environments do not count
     (check= (env-at "section-nr" 2 1) "0"))))

;; The labels get the numbers of the sections and equations when the
;; buffer is typeset; a subsection is numbered within its section.
(define (test-numbering)
  (check-group "numbering")
  (with-typeset-body
   '(document (section (concat "A" (label "s1")))
              (section (concat "B" (label "s2")))
              (equation (concat "x" (label "e1")))
              (equation (concat "y" (label "e2")))
              (subsection (concat "C" (label "s3")))
              (equation* (document "z"))
              (equation (concat "w" (label "e3"))))
   (lambda ()
     (let ((ref (lambda (l) (tree->stree (get-reference l)))))
       (check= (ref "s1") '(tuple "1" "1"))
       (check= (ref "s2") '(tuple "2" "1"))
       (check= (ref "e1") '(tuple "1" "1"))
       (check= (ref "e2") '(tuple "2" "1"))
       ;; equation* is not numbered
       (check= (ref "e3") '(tuple "3" "1"))
       (check= (ev (cadr (ref "s3"))) "2.1")
       (check-true (in? "s3" (list-references)))
       (check-true (in? "e3" (list-references)))
       (check-false (in? "e4" (list-references)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Boxes of text: more text is wider, a larger font is larger, explicit
;; spaces and moves give exact widths, a resize sets the width exactly,
;; and the queries of box-info agree with each other.
(define (test-text-boxes)
  (check-group "text boxes")
  (check-true (< (width "ab") (width "abc")))
  (check-true (< (width "abc") (width "abcabc")))
  (check-true (> (height "ab") 0))
  (let ((w10 (width "abc"))
        (w20 (width '(with "font-base-size" "20" "abc")))
        (h10 (height "abc"))
        (h20 (height '(with "font-base-size" "20" "abc"))))
    ;; not exactly twice: a font may have a design for the larger size
    (check-true (near? w20 (* 2 w10) (* 0.2 w10)))
    (check-true (near? h20 (* 2 h10) (* 0.2 h10))))
  (let ((w1 (width '(concat "a" (hspace "1cm") "b")))
        (w2 (width '(concat "a" (hspace "2cm") "b")))
        (w12 (width '(concat "a" (hspace "1cm") (hspace "2cm") "b")))
        (w3 (width '(concat "a" (hspace "3cm") "b"))))
    (check-true (near? (- w2 w1) (tmpt "1cm") 2))
    (check-true (near? w12 w3 2)))
  (check= (box '(move "ab" "1cm" "") "l") (list (tmpt "1cm")))
  (check= (box '(move "ab" "1cm" "") "r")
          (list (+ (tmpt "1cm") (width "ab"))))
  (check= (box "ab" "l") '(0))
  (check= (width '(resize "ab" "" "" "5cm" "")) (tmpt "5cm"))
  (check= (box "ab" "wh") (list (width "ab") (height "ab")))
  (let ((b (box "ab" "lbrt")))
    (check= (- (caddr b) (car b)) (width "ab"))
    (check= (- (cadddr b) (cadr b)) (height "ab")))
  (check= (ev '(box-info "ab" "w."))
          (string-append (number->string (width "ab")) "tmpt"))
  (let ((b (box "o" "LRWBTH")))
    (check= (- (cadr b) (car b)) (caddr b))
    (check= (- (list-ref b 4) (list-ref b 3)) (list-ref b 5))
    (check-true (> (caddr b) 0)))
  ;; a graphics has the default geometry of 1par by 0.6par
  (let ((b (box '(graphics) "wh")))
    (check= (car b) (tmpt "1par"))
    (check-true (near? (cadr b) (* 0.6 (tmpt "1par")) 2))))

;; Boxes of mathematics: a fraction is taller than its numerator and
;; denominator, a root is wider and taller than its radicand, a script is
;; smaller than the same letter in the main line and raises the line, a
;; big operator is taller than a letter.
(define (test-math-boxes)
  (check-group "math boxes")
  (let ((x (box (math "x") "wh"))
        (f (box (math '(frac "x" "y")) "wh"))
        (r (box (math '(sqrt "x")) "wh"))
        (s (box (math '(big-around "<sum>" "x")) "wh")))
    (check-true (> (cadr f) (cadr x)))
    (check-true (> (cadr f) (height (math "y"))))
    (check-true (> (car r) (car x)))
    (check-true (> (cadr r) (cadr x)))
    (check-true (> (cadr s) (cadr x))))
  (let ((sub (- (width (math '(concat "a" (rsub "x")))) (width (math "a"))))
        (sup (- (width (math '(concat "a" (rsup "x")))) (width (math "a")))))
    (check-true (> sub 0))
    (check-true (< sub (width (math "x"))))
    (check-true (< sup (width (math "x")))))
  (check-true (> (height (math '(concat "a" (rsup "x")))) (height (math "a"))))
  (check-true (> (height (math '(frac (frac "a" "b") "c")))
                 (height (math '(frac "a" "c")))))
  (check-true (< (width (math '(frac "a" "b")))
                 (width (math '(frac "aaa" "b"))))))

;; Tables: a table is at least as wide as its widest cells in each column
;; and grows with its rows; in a typeset buffer, the alignment of a cell
;; puts its content against the left or right side, or in the middle, of
;; the column, whose width is set by its widest cell.
(define (test-tables)
  (check-group "tables")
  (let ((t1 (box '(tabular (table (row (cell "a") (cell "bbbb")))) "wh"))
        (t2 (box '(tabular (table (row (cell "a") (cell "bbbb"))
                                  (row (cell "ccccc") (cell "d")))) "wh")))
    (check-true (>= (car t2) (+ (width "ccccc") (width "bbbb"))))
    (check-true (> (car t2) (car t1)))
    (check-true (near? (cadr t2) (* 2 (cadr t1)) 2)))
  (with-typeset-body
   '(document
     (tabular (tformat (cwith "1" "1" "1" "1" "cell-halign" "r")
                       (cwith "2" "2" "1" "1" "cell-halign" "l")
                       (cwith "3" "3" "1" "1" "cell-halign" "c")
                       (table (row (cell "a")) (row (cell "b"))
                              (row (cell "c")) (row (cell "wwwwwwwwww")))))
     "z")
   (lambda ()
     (let ((ra (rect 0 0 3 0 0 0))
           (rb (rect 0 0 3 1 0 0))
           (rc (rect 0 0 3 2 0 0))
           (rw (rect 0 0 3 3 0 0)))
       (check-true (near? (rect-x2 ra) (rect-x2 rw) 2))
       (check-true (near? (rect-x1 rb) (rect-x1 rw) 2))
       (check-true (near? (rect-mid rc) (rect-mid rw) 2))
       (check-true (> (rect-x1 rc) (rect-x1 rb)))
       (check-true (> (rect-x1 ra) (rect-x1 rc)))
       ;; the rows go downwards
       (check-true (< (rect-y2 rb) (rect-y1 ra)))
       (check-true (< (rect-y2 rw) (rect-y1 rc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A par-block is as wide as the paragraph width and as tall as its lines:
;; the lines are evenly spaced, a narrower paragraph has more lines, and
;; par-left and par-right narrow it the same way as a smaller width.
(define (test-line-breaking)
  (check-group "line breaking")
  (check= (width (block "3cm" "x")) (tmpt "3cm"))
  (check= (width (block "6cm" lorem)) (tmpt "6cm"))
  (let ((h1 (height (block "15cm" "x")))
        (h2 (height (block "15cm" '(concat "x" (new-line) "x"))))
        (h4 (height (block "15cm" '(concat "x" (new-line) "x" (new-line)
                                           "x" (new-line) "x")))))
    (check= h2 (* 2 h1))
    (check= h4 (* 4 h1)))
  (check= (lines "15cm" "short") 1)
  (check= (lines "15cm" '(concat "a" (new-line) "b" (new-line) "c")) 3)
  (let ((l15 (lines "15cm" lorem))
        (l6 (lines "6cm" lorem))
        (l3 (lines "3cm" lorem)))
    (check-true (> l15 1))
    (check-true (> l6 l15))
    (check-true (> l3 l6))
    ;; the lines are full: the text, whose spaces may shrink a little,
    ;; is at least as long as all lines but the last one
    (check-true (>= (width lorem) (* (- l6 1) (tmpt "6cm"))))
    (check-true (>= (width lorem) (* (- l15 1) (tmpt "15cm")))))
  (check= (lines "6cm" `(with "par-left" "1.5cm" "par-right" "1.5cm"
                          (document ,lorem)))
          (lines "3cm" lorem))
  ;; two paragraphs are two lines at least, separated by par-par-sep
  (check-true (> (height (block "6cm" '(document "a" "b")))
                 (* 2 (height (block "6cm" "a"))))))

;; A word longer than the line is hyphenated: it takes several lines in
;; English, and one line which sticks out in a language without
;; hyphenation patterns (verbatim).
(define (test-hyphenation)
  (check-group "hyphenation")
  (check-true (> (width "incomprehensibilities") (tmpt "1.5cm")))
  (check-true (> (lines "1.5cm" "incomprehensibilities") 1))
  (check= (lines "1.5cm" '(with "language" "verbatim"
                            "incomprehensibilities"))
          1)
  (check= (height (block "1.5cm" '(with "language" "verbatim"
                                    "incomprehensibilities")))
          (height (block "1.5cm" "x"))))

;; In a typeset buffer the paragraphs go downwards, a centered line is in
;; the middle of the text width and a line set to the right ends where a
;; full line ends; par-left indents the paragraph and par-right makes it
;; taller.
(define (test-paragraphs)
  (check-group "paragraphs")
  (with-typeset-body
   `(document "short"
              (with "par-mode" "center" (document "short"))
              (with "par-mode" "right" (document "short"))
              ,lorem
              (with "par-left" "1cm" "par-right" "12cm" (document ,lorem))
              (with "par-left" "5cm" (document "short")))
   (lambda ()
     (let ((left (rect 0)) (center (rect 1)) (right (rect 2))
           (full (rect 3)) (narrow (rect 4)) (indented (rect 5)))
       (check-true (< (rect-y2 center) (rect-y1 left)))
       (check-true (< (rect-y2 right) (rect-y1 center)))
       (check-true (< (rect-y2 narrow) (rect-y1 full)))
       (check= (rect-x1 left) (rect-x1 full))
       (check-true (< (rect-x1 left) (rect-x1 center)))
       (check-true (< (rect-x1 center) (rect-x1 right)))
       (check-true (near? (rect-mid center) (rect-mid full) 2))
       (check-true (near? (rect-x2 right) (rect-x2 full) 2))
       (check-true (near? (- (rect-x2 left) (rect-x1 left))
                          (- (rect-x2 right) (rect-x1 right)) 2))
       (check-true (> (rect-x1 indented) (rect-x1 left)))
       (check-true (> (- (rect-y2 narrow) (rect-y1 narrow))
                      (* 3 (- (rect-y2 full) (rect-y1 full)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A new-page starts a page; the pages are stacked one page height (with
;; its decorations) apart; a long text fills several pages, and the total
;; height is the sum of the pages.
(define (test-pages)
  (check-group "pages")
  (with-typeset-body
   '(document "a")
   (lambda ()
     (check= (get-page-count) 1)
     (check= (cpp-nr-pages) 1)
     (check-true (near? (get-total-height #f) (get-page-height #f) 1))
     (check-true (> (get-page-height #t) (get-page-height #f)))
     (check-true (>= (get-page-width #t) (get-page-width #f)))
     ;; an a4 page is taller than wide
     (check-true (> (get-page-height #f) (get-page-width #f)))))
  (with-typeset-body
   '(document "a" (new-page) "b" (new-page) "c")
   (lambda ()
     (check= (get-page-count) 3)
     (check= (cpp-nr-pages) 3)
     (check-true (near? (get-total-height #f) (* 3 (get-page-height #f)) 3))
     (let ((ra (rect 0)) (rb (rect 2)) (rc (rect 4)))
       (check-true (near? (- (rect-y1 ra) (rect-y1 rb))
                          (get-page-height #t) 2))
       (check-true (near? (- (rect-y1 rb) (rect-y1 rc))
                          (get-page-height #t) 2))
       (check= (rect-x1 ra) (rect-x1 rb)))))
  (let ((pages (lambda (n)
                 (let ((r 0))
                   (with-typeset-body
                    `(document ,@(map (lambda (i) lorem) (iota n)))
                    (lambda () (set! r (get-page-count))))
                   r))))
    (let ((p1 (pages 1)) (p20 (pages 20)) (p60 (pages 60)))
      (check= p1 1)
      (check-true (> p20 1))
      (check-true (> p60 p20))
      ;; three times the text takes about three times the pages
      (check-true (<= p60 (* 3 p20)))
      (check-true (>= p60 (* 3 (- p20 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-buffer thunk)
  ;; the evaluations take the environment of a fresh generic buffer
  (with-typeset-body '(document "") thunk))

(tm-define (typeset-test-failures)
  (:synopsis "Run the tests of the typesetter and return the number of failures")
  (check-suite "typeset")
  (in-buffer
   (lambda ()
     (test-arithmetic)
     (test-conditionals)
     (test-macros)
     (test-lengths)
     (test-text-boxes)
     (test-math-boxes)
     (test-line-breaking)
     (test-hyphenation)))
  (test-environment)
  (test-numbering)
  (test-tables)
  (test-paragraphs)
  (test-pages)
  (check-end))
