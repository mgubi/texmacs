
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : opentype-typeset-test.scm
;; DESCRIPTION : tests of the typesetting of formulas in OpenType math fonts
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The placement rules of the MATH table, as far as they can be seen without
;; a window (see typeset-test.scm for the two means which are used here):
;;
;;   - the box-info primitive gives the extents of a formula in tmpt: 'l'
;;     'b' 'r' 't' are the sides of the logical box, 'w' 'h' its width and
;;     height;
;;   - after update-forced, tree-bounding-rectangle gives the rectangle of
;;     a subtree of the buffer, which goes through the correspondence
;;     between the positions in the tree and those in the boxes.
;;
;; The formulas are typeset in Latin Modern Math and in Fira Math, which are
;; shipped with TeXmacs. Extents are compared with each other; no value of
;; a font is written down here.

(texmacs-module (check opentype-typeset-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lm "Latin Modern Math")
(define fira "Fira Math")

(define (ev t)
  (tree->stree (texmacs-expand t)))

(define (numbers r)
  ;; the numbers of the tuple which box-info returns, also when the
  ;; evaluation leaves it inside the environments around it
  (cond ((and (pair? r) (== (car r) 'tuple)) (map string->number (cdr r)))
        ((pair? r) (numbers (cAr r)))
        (else '())))

(define (mbox font t what)
  ;; the extents @what of the formula @t in the math font @font
  (numbers (ev `(with "font" ,font "mode" "math" (box-info ,t ,what)))))

(define (top font t) (car (mbox font t "t")))
(define (left font t) (car (mbox font t "l")))
(define (right font t) (car (mbox font t "r")))

(define (near? x y tol) (<= (abs (- x y)) tol))

(define (at . l) (append (buffer-path) l))
(define (rect . l)
  (tree-bounding-rectangle (path->tree (apply at l))))
(define (rect-x1 r) (car r))
(define (rect-y1 r) (cadr r))
(define (rect-x2 r) (caddr r))
(define (rect-y2 r) (cadddr r))
(define (rect-w r) (- (rect-x2 r) (rect-x1 r)))
(define (rect-h r) (- (rect-y2 r) (rect-y1 r)))
(define (rect-inside? r s)
  (and (>= (rect-x1 r) (rect-x1 s)) (<= (rect-x2 r) (rect-x2 s))
       (>= (rect-y1 r) (rect-y1 s)) (<= (rect-y2 r) (rect-y2 s))))

(define (with-typeset-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, typeset, then close it
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fractions in display style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A display style fraction keeps its shifts when the line which holds it is
;; stretched by kerning or by glyph expansion: the paragraph of unbreakable
;; words below has such lines, and is as high as with both switched off.

(define long-words
  (string-append
   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "
   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "
   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxx."))

(define (dfrac-block font . opts)
  (car (numbers (ev `(with "font" ,font
                       (box-info
                        (with "par-width" "12cm" ,@opts
                          (par-block
                           (document
                            (concat "Q" (with "mode" "math"
                                          "math-display" "true"
                                          (frac "a" "b"))
                                    " " ,long-words))))
                        "h"))))))

(define (test-display-fraction font)
  (check-group (string-append "display fraction, " font))
  (let ((stretched (dfrac-block font))
        (rigid (dfrac-block font "par-expansion" "0" "par-contraction" "0"
                            "par-kerning-stretch" "0"
                            "par-kerning-reduce" "0")))
    (check-true (> stretched 0))
    (check-true (near? stretched rigid (/ rigid 100)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x2 '(concat "x" (rsup "2")))
(define xi2 '(concat "x" (rsub "i") (rsup "2")))
(define xA2 '(concat "x" (rsub "A") (rsup "2")))

(define (test-script-pair)
  (check-group "subscript and superscript")
  ;; a subscript never lowers the superscript: when the gap is too small
  ;; the subscript goes down
  (check= (top lm xi2) (top lm x2))
  (check= (top lm xA2) (top lm x2))
  ;; in Fira Math superscriptBottomMaxWithSubscript is above the standard
  ;; shift, and a pair which is too close goes up as a whole
  (check-true (> (top fira xi2) (top fira x2))))

(define (test-numbers)
  (check-group "scripts of numbers")
  ;; a number takes its scripts as its last digit does
  (check= (top lm '(concat "10" (rsup "3")))
          (top lm '(concat "0" (rsup "3"))))
  (check= (top lm '(concat "10" (rsup "3")))
          (top lm '(concat "x" (rsup "3")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Radicals with a degree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-radical-degree font)
  (check-group (string-append "radical with a degree, " font))
  ;; the box starts at its origin, and the space before the degree is part
  ;; of its width
  (check= (left font '(sqrt "x" "3")) 0)
  (check-true (>= (right font '(sqrt "x" "3")) (right font '(sqrt "x"))))
  (check-true (> (right font '(sqrt "x" "123"))
                 (right font '(sqrt "x" "3")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulas inside text, positions in substituted glyphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define delimited
  '(concat (around* "(" (frac "a" "b") ")") (rsub "i") (rsup "2")))

(define (buffer-doc font)
  `(document
     (with "font" ,font
       (document
         (math (concat "a" (wide "i" "^") "b" (neg "=") "c"
                       (neg "<subset>") "d"))
         (concat "Q" (math ,delimited))
         (equation* (with "math-display" "false" ,delimited))))))

(define (test-inline font)
  (check-group (string-append "scripts in a formula inside text, " font))
  ;; scripts on a base which is not a glyph are placed in the same way in
  ;; a formula inside text and in an equation at text style
  (let ((inline (rect 0 2 1 1 0))
        (eq (rect 0 2 2 0 2)))
    (check-true (> (rect-h inline) 0))
    (check= (rect-h inline) (rect-h eq))
    (check= (rect-w inline) (rect-w eq))))

(define (test-substituted font)
  (check-group (string-append "positions in substituted glyphs, " font))
  ;; the dotless i under an accent and the precomposed negations are other
  ;; strings than those of the tree: the positions of the tree still
  ;; delimit the glyph
  (let ((accent (rect 0 2 0 0 1))
        (letter (rect 0 2 0 0 1 0))
        (neg1 (rect 0 2 0 0 3))
        (rel1 (rect 0 2 0 0 3 0))
        (neg2 (rect 0 2 0 0 5))
        (rel2 (rect 0 2 0 0 5 0))
        (last (rect 0 2 0 0 6)))
    (check-true (> (rect-w letter) 0))
    (check-true (rect-inside? letter accent))
    (check-true (> (rect-w rel1) 0))
    (check= rel1 neg1)
    (check-true (> (rect-w rel2) 0))
    (check= rel2 neg2)
    (check-true (<= (rect-x2 neg1) (rect-x1 neg2)))
    (check-true (<= (rect-x2 neg2) (rect-x1 last)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accents over a single letter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The accents are the combining accents of the math font, which all sit at
;; about the same height over a letter, whichever font has the spacing
;; accents: Lete Sans Math has neither a spacing circumflex nor a spacing
;; tilde, and its hat and its tilde were the boxes of a missing glyph.
(define (test-narrow-accents font)
  (check-group (string-append "accents over a letter, " font))
  (let* ((x (top font "x"))
         (accs (list "^" "~" "<bar>" "<check>" "<breve>" "<dot>" "<acute>"))
         (tops (map (lambda (a) (top font `(wide "x" ,a))) accs))
         (lo (apply min tops))
         (hi (apply max tops)))
    (check-true (> x 0))
    (check-true (> lo x))
    (check-true (< (- hi lo) (* 0.5 x)))
    (check-true (< hi (* 2 x)))
    ;; and they add no width to the letter
    (check= (right font '(wide "x" "^")) (right font "x"))
    (check= (right font '(wide "x" "~")) (right font "x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (opentype-typeset-test-failures)
  (:synopsis "Run the tests of OpenType math typesetting and return the number of failures")
  (check-suite "opentype-typeset")
  (with-typeset-body '(document "")
    (lambda ()
      (test-display-fraction lm)
      (test-display-fraction fira)
      (test-script-pair)
      (test-numbers)
      (test-radical-degree lm)
      (test-radical-degree fira)
      (test-narrow-accents lm)
      (test-narrow-accents fira)
      (test-narrow-accents "Lete Sans Math")
      (test-narrow-accents "Stix Two Math")))
  (for (font (list lm fira))
    (with-typeset-body (buffer-doc font)
      (lambda ()
        (test-inline font)
        (test-substituted font))))
  (check-end))
