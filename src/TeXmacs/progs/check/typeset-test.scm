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

;; A paragraph which is added changes the counters: what comes after it is
;; numbered again, and what comes before is not.
(define (test-renumbering)
  (check-group "renumbering")
  (with-typeset-body
   '(document (section (concat "A" (label "s1")))
              (equation (concat "x" (label "e1")))
              (section (concat "B" (label "s2")))
              (equation (concat "y" (label "e2")))
              (concat "see " (reference "e2")))
   (lambda ()
     (let ((nr (lambda (l) (cadr (tree->stree (get-reference l))))))
       (check= (nr "s2") "2")
       (check= (nr "e2") "2")
       (tree-insert (buffer-tree) 2
                    '((equation (concat "n" (label "e0")))))
       (update-forced)
       (check= (nr "e1") "1")
       (check= (nr "e0") "2")
       (check= (nr "e2") "3")
       (check= (nr "s2") "2")
       (tree-insert (buffer-tree) 0 '((section (concat "N" (label "s0")))))
       (update-forced)
       (check= (nr "s0") "1")
       (check= (nr "s1") "2")
       (check= (nr "s2") "3")
       (check= (nr "e2") "3")
       (tree-remove (buffer-tree) 0 1)
       (update-forced)
       (check= (nr "s1") "1")
       (check= (nr "s2") "2")))))

;; The entries of the table of contents and of the index put a label where
;; they stand, auto-n. The number is that of the tag which makes the label
;; (Typeset/Env/env_exec.cpp, exec_auto_id): it follows the order of the
;; document when this one is loaded or updated, and a tag which is added
;; in between takes a new number, so that the other labels keep their
;; names until the next update.
(define (test-automatic-labels)
  (check-group "automatic labels")
  (with-typeset-body
   '(document (section "A")
              (concat "a" (index "first"))
              (section "B")
              (concat "b" (index "second"))
              (section "C"))
   (lambda ()
     (let ((nr (lambda (l) (cadr (tree->stree (get-reference l)))))
           (autos (lambda ()
                    (list-filter (list-references)
                                 (lambda (l) (string-starts? l "auto-"))))))
       ;; in the order of the document
       (check= (length (autos)) 5)
       (check= (nr "auto-1") "1")
       (check= (nr "auto-3") "2")
       (check= (nr "auto-5") "3")
       ;; a new section before B: B and C keep the names of their labels
       (tree-insert (buffer-tree) 2 '((section "N")))
       (update-forced)
       (check= (nr "auto-1") "1")
       (check= (nr "auto-6") "2")
       (check= (nr "auto-3") "3")
       (check= (nr "auto-5") "4")
       ;; typing in a section does not change them either
       (tree-set (tree-ref (buffer-tree) 3 0) "B again")
       (update-forced)
       (check= (nr "auto-3") "3")
       ;; an update numbers them in the order of the document again
       (renumber-auto-labels)
       (update-forced)
       (check= (nr "auto-1") "1")
       (check= (nr "auto-3") "2")
       (check= (nr "auto-4") "3")
       (check= (nr "auto-6") "4")
       ;; and so does a second update, which has nothing to do
       (renumber-auto-labels)
       (update-forced)
       (check= (nr "auto-4") "3")
       (check= (nr "auto-6") "4")))))

;; A tag makes its k-th label from the names which it and the tags around
;; it gave last: a tag inside it which makes a label of its own does not
;; make it start again.
(define (test-labels-of-nested-tags)
  (check-group "labels of nested tags")
  (with-typeset-body
   '(document (assign "wrap" (macro "body" (concat (index "start") (arg "body")
                                                   (index "end"))))
              (concat "a " (wrap (concat "x" (index "mid") "y")) " b")
              (concat "c " (wrap "z") " d"))
   (lambda ()
     (let* ((autos (list-filter (list-references)
                                (lambda (l) (string-starts? l "auto-")))))
       (check= (length autos) 5)
       (check= (length (list-remove-duplicates autos)) 5)))))

;; A paragraph is not typeset again when the variables which changed are
;; not among those it read (Typeset/Bridge/bridge.cpp). Below a tag which
;; binds a variable to a value computed from one which changed, the
;; paragraphs read the bound variable only, and are typeset again.
(define (test-reuse-below-bindings)
  (check-group "reuse below bindings")
  (let ((lefts (lambda (n)
                 (map (lambda (i)
                        (car (tree-bounding-rectangle
                              (tree-ref (buffer-tree) 1 0 i))))
                      (.. 0 n)))))
    (with-typeset-body
     '(document (assign "quote-left-indentation" "2tab")
                (quote-env (document "aaa" "bbb" "ccc" "ddd" "eee"))
                "tail")
     (lambda ()
       (with before (lefts 5)
         (check= (length (list-remove-duplicates before)) 1)
         (tree-set (tree-ref (buffer-tree) 0 1) "8tab")
         (update-forced)
         (with after (lefts 5)
           (check= (length (list-remove-duplicates after)) 1)
           (check-true (> (car after) (car before)))))))
    (with-typeset-body
     '(document (assign "ornament-hpadding" "1spc")
                (ornament (document "aaa" "bbb" "ccc"))
                "tail")
     (lambda ()
       (with before (lefts 3)
         (check= (length (list-remove-duplicates before)) 1)
         (tree-set (tree-ref (buffer-tree) 0 1) "3cm")
         (update-forced)
         (with after (lefts 3)
           (check= (length (list-remove-duplicates after)) 1)
           (check-true (> (car after) (car before)))))))))

;; The page breaks which were found are used again when the page items
;; keep their sizes (Typeset/Page/new_breaker.cpp). The breaker moves a
;; footnote on two columns of a title on one column to the next item on
;; two columns: the pager needs this done when the search is skipped too.
(define (test-page-breaks-kept)
  (check-group "page breaks kept")
  (with-typeset-body
   '(document (with "par-columns" "1"
                (document (concat "Title"
                            (footnote (with "par-columns" "2"
                                        (document "the note"))))))
              "body one body one body one body one body one"
              "body two")
   (lambda ()
     (init-env "page-medium" "paper")
     (init-env "par-columns" "2")
     ;; (the initial environment is in use after an update of the buffer)
     (update-current-buffer)
     (update-forced)
     (with top (lambda () (cadddr (tree-bounding-rectangle
                                   (tree-ref (buffer-tree) 2))))
       (with before (top)
         ;; edits which keep the heights of the lines
         (tree-set (tree-ref (buffer-tree) 2) "body 2wo")
         (update-forced)
         (check= (tree->stree (tree-ref (buffer-tree) 2)) "body 2wo")
         (check= (top) before)
         (tree-set (tree-ref (buffer-tree) 2) "body tvo")
         (update-forced)
         (check= (tree->stree (tree-ref (buffer-tree) 2)) "body tvo")
         (check= (top) before))))))

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

;; The pager keeps the pages of the last pass (Typeset/Page/make_pages.cpp):
;; a page break with nothing to say at it, pages which go away and come
;; back, and a paragraph at the same place as after a typesetting of the
;; whole document.
(define (test-pages-kept)
  (check-group "pages kept")
  (with-typeset-body
   `(document ,@(map (lambda (i)
                       `(concat (if-page-break "t" (document)) ,lorem))
                     (iota 40)))
   (lambda ()
     (init-env "page-type" "a5")
     (update-current-buffer)
     (update-forced)
     (with n (get-page-count)
       (check-true (> n 1))
       (tree-set (tree-ref (buffer-tree) 0) "changed")
       (update-forced)
       (check-true (> (get-page-count) 1)))))
  (with-typeset-body
   `(document ,@(map (lambda (i) lorem) (iota 60)))
   (lambda ()
     (let* ((n (get-page-count))
            (r (rect 20)))
       (check-true (> n 2))
       ;; a letter in a paragraph: the other pages are kept
       (tree-set (tree-ref (buffer-tree) 30) (string-append lorem " x"))
       (update-forced)
       (check= (get-page-count) n)
       (check= (rect 20) r)
       ;; the last pages go away and come back
       (tree-remove (buffer-tree) 30 30)
       (update-forced)
       (with m (get-page-count)
         (check-true (< m n))
         ;; (the breaks of a shorter document may be others)
         (tree-insert (buffer-tree) 30 (map (lambda (i) lorem) (iota 30)))
         (update-forced)
         (check= (get-page-count) n)
         (check= (rect 20) r)
         ;; as after a typesetting of the whole document
         (with r2 (rect 50)
           (update-current-buffer)
           (update-forced)
           (check= (rect 50) r2)
           (check= (get-page-count) n)))))))

;; A document whose typesetting fails is replaced by an empty one, and the
;; typesetting goes on, in it and in the other buffers (the bridges which
;; were recording what they read when it failed do not record any more).
(define (test-typesetting-failure)
  (check-group "typesetting failure")
  (with-typeset-body
   '(document "first" "second" (anim-compose) "third")
   (lambda ()
     (check= (tree->stree (buffer-tree)) '(document ""))))
  (with-typeset-body
   '(document "one" "two" "three")
   (lambda ()
     (tree-set (tree-ref (buffer-tree) 1) "two again")
     (update-forced)
     (check= (tree->stree (buffer-tree)) '(document "one" "two again" "three"))
     (check-true (> (rect-x2 (rect 1)) (rect-x1 (rect 1)))))))

;; A length in a unit made by a macro of the document: when the macro
;; changes, what the environment derived from the length changes too (the
;; limit of the fractions in a line), also in paragraphs which read none of
;; the variables which changed.
(define (test-derived-state)
  (check-group "derived state")
  (with-typeset-body
   '(document (assign "myu-length" (macro "0.1mm"))
              (assign "math-frac-limit" "5myu")
              (concat "x " (math (frac "aaaaaaaaaa" "b")) " y")
              "after")
   (lambda ()
     (tree-set (tree-ref (buffer-tree) 0 1 0) "10cm")
     (update-forced)
     (let* ((r2 (rect 2)) (r3 (rect 3)))
       (update-current-buffer)
       (update-forced)
       (check= r2 (rect 2))
       (check= r3 (rect 3))))))

;; The labels are numbered again in the order of the document once, after
;; a section was added before the others, and not a second time.
(define (test-renumbering-labels)
  (check-group "labels numbered again")
  (with-typeset-body
   '(document (section "A") (section "B"))
   (lambda ()
     (tree-insert (buffer-tree) 0 '((section "N")))
     (update-forced)
     (check-true (renumber-auto-labels))
     (update-forced)
     (check-false (renumber-auto-labels))
     (check= (cadr (tree->stree (get-reference "auto-1"))) "1")
     (check= (cadr (tree->stree (get-reference "auto-3"))) "3"))))
;; Blocks of paragraphs which did not change are replayed after the first
;; one (Typeset/Bridge/bridge_document.cpp). A paragraph without a line (a
;; hidden one) only passes on the space after it: when that space changes,
;; the paragraphs after it move, also when it is the first of a block.
(define (places-from-scratch body n)
  ;; the rectangles of the @n paragraphs of @body, typeset in a buffer of
  ;; its own
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (tree-copy body))
    (update-forced)
    (with r (map (lambda (i) (rect i)) (iota n))
      (buffer-close u)
      (switch-to-buffer* old)
      r)))

(define (test-block-replay)
  (check-group "block replay")
  (with-typeset-body
   `(document ,@(map (lambda (i)
                       (if (odd? i)
                           '(with "par-par-sep" "0fn" (hidden "h"))
                           (string-append "paragraph number "
                                          (number->string i))))
                     (iota 240)))
   (lambda ()
     (let* ((n 240)
            (places (lambda () (map (lambda (i) (rect i)) (iota n))))
            (bad 0))
       ;; an edit elsewhere, so that every block was recorded and replayed
       (tree-set (tree-ref (buffer-tree) 0) "first")
       (update-forced)
       (for (i (iota n))
         (when (odd? i)
           (tree-set (tree-ref (buffer-tree) i 1) "5fn")
           (update-forced)
           (when (!= (places) (places-from-scratch (buffer-tree) n))
             (set! bad (+ bad 1)))
           (tree-set (tree-ref (buffer-tree) i 1) "0fn")
           (update-forced)))
       (check= bad 0)))))

;; After an update the automatic labels are named as by the counter: auto-1
;; to auto-n in the order in which they are made, also when a tag makes two
;; labels, when the index makes one of its own, and after a section was
;; removed (which gives no new number, but leaves one unused).
(define (auto-names)
  (sort (list-filter (list-references)
                     (lambda (l) (string-starts? l "auto-")))
        (lambda (x y)
          (< (string->number (substring x 5 (string-length x)))
             (string->number (substring y 5 (string-length y)))))))

(define (auto-range n)
  (map (lambda (i) (string-append "auto-" (number->string (+ i 1))))
       (iota n)))

(define (test-label-names)
  (check-group "names of the labels")
  (with-typeset-body
   '(document (assign "two" (macro "x" (concat (index (arg "x"))
                                               (index (arg "x")))))
              (section "A")
              (concat "a" (two "first"))
              (section "B")
              (concat "b" (index "second"))
              (section "C")
              (the-index "idx" (document "")))
   (lambda ()
     (renumber-auto-labels)
     (generate-all-aux)
     (update-forced)
     (with names (auto-names)
       (check= names (auto-range (length names)))
       (check-true (>= (length names) 7)))
     ;; a section removed: an update numbers the labels again
     (tree-remove (buffer-tree) 3 2)
     (update-forced)
     (check-true (renumber-auto-labels))
     (generate-all-aux)
     (update-forced)
     (with names (auto-names)
       (check= names (auto-range (length names))))
     (check= (cadr (tree->stree (get-reference "auto-1"))) "1")
     ;; and nothing to do the next time
     (check-false (renumber-auto-labels)))))

;; A binding which is the same as in the previous pass: below it, the
;; variable has the value of the previous pass, whatever it had outside;
;; a variable which a with binds and which was not there before is there
;; after it (with no value), and is not there any more when the with goes.
;; After each edit the places of the paragraphs are those of a typesetting
;; from scratch.
(define (same-as-scratch?)
  (let* ((n (tree-arity (buffer-tree)))
         (inc (map (lambda (i) (rect i)) (iota n))))
    (== inc (scratch-places (buffer-tree) n))))

(define (test-bindings-against-scratch)
  (check-group "bindings against scratch")
  (with-typeset-body
   '(document (assign "x" "short")
              (assign "y" "1")
              (with "x" "BOUND-BOUND-BOUND-BOUND"
                (document "hello" (concat "val: " (value "x")) "tail"))
              "end")
   (lambda ()
     (tree-set (tree-ref (buffer-tree) 0 1) "a-very-very-very-long-value")
     (tree-set (tree-ref (buffer-tree) 1 1) "2")
     (tree-set (tree-ref (buffer-tree) 2 2 0) '(assign "x" "short"))
     (update-forced)
     (check-true (same-as-scratch?))))
  (for (replace? '(#t #f))
    (with-typeset-body
     '(document (assign "xb" "a-much-longer-value")
                (with "xc" "mid-val"
                  (document "t" (assign "xc" "q") (assign "xb" "q")))
                "plain"
                (concat "v: " (value "xc") "."))
     (lambda ()
       (if replace?
           (tree-set (tree-ref (buffer-tree) 1) "replaced")
           (tree-remove (buffer-tree) 1 1))
       (update-forced)
       (check-true (same-as-scratch?)))))
  ;; a paragraph which binds a variable and then gives it a value: when it
  ;; goes, the value is not there any more
  (with-typeset-body
   '(document (with "v" "B" "x")
              (concat (with "v" "B" "y") (assign "v" "CCCCCCCCCCCCCCCCCCCC"))
              "filler"
              (concat "val: " (value "v") ".")
              "end")
   (lambda ()
     (tree-remove (buffer-tree) 1 1)
     (update-forced)
     (check-true (same-as-scratch?))))
  ;; the same as the with above, with a cell
  (with-typeset-body
   '(document (assign "xb" "a-much-longer-value")
              (concat (tabular (tformat (cwith "1" "1" "1" "1" "xc" "mid-val")
                                        (table (row (cell "t")))))
                      (assign "xb" "q"))
              "plain"
              (concat "v: " (value "xc") "."))
   (lambda ()
     (tree-set (tree-ref (buffer-tree) 1) "replaced")
     (update-forced)
     (check-true (same-as-scratch?))))
  ;; the name of a theorem moved by a macro of the document: the first
  ;; paragraph of the theorem has it in its first line
  (with-typeset-body
   '(document (assign "theorem-name"
                      (macro "name" (move (strong (arg "name")) "0pt" "0pt")))
              "intro"
              (theorem (document "First paragraph of the theorem body."
                                 "Second paragraph."))
              "end")
   (lambda ()
     (tree-set (tree-ref (buffer-tree) 0 1 1 2) "4ex")
     (update-forced)
     (check-true (same-as-scratch?))))
  ;; a variable which is assigned below a binding of it: its value before
  ;; is the one outside, not the bound one (not with the classic cache,
  ;; TEXMACS_TYPESET_READS=off, which gets it wrong as TeXmacs always did)
  (when (!= (getenv "TEXMACS_TYPESET_READS") "off")
  (with-typeset-body
   `(document ,@(map (lambda (i)
                       (cond ((== i 1) '(assign "foo" "1cm"))
                             ((== i 5) '(with "par-left" "0cm"
                                          (document
                                           (with "foo" "3cm"
                                             (document (assign "foo" "2cm")
                                                       "inner"))
                                           "x" (assign "foo" "3cm"))))
                             ((== i 20) '(concat "a" (vspace (value "foo")) "b"))
                             (else (string-append "paragraph "
                                                  (number->string i)))))
                     (.. 0 30)))
   (lambda ()
     (update-forced)
     (tree-set (tree-ref (buffer-tree) 20) '(concat "c" (vspace (value "foo")) "d"))
     (update-forced)
     (check-true (same-as-scratch?))))))

;; The labels of the parts of a document are named after the part, and
;; numbered from 1 in each part, as the counter does (set-part).
(define (test-labels-of-parts)
  (check-group "labels of parts")
  (with-typeset-body
   '(document (table-of-contents "toc" (document ""))
              (show-part "p1" (document (section "A") "a" (section "B") "b")
                         "")
              (show-part "p2" (document (section "C") "c" (section "D") "d")
                         ""))
   (lambda ()
     (renumber-auto-labels)
     (generate-all-aux)
     (update-forced)
     (check= (sort (list-filter (list-references)
                                (lambda (l) (string-starts? l "auto.")))
                   string<?)
             '("auto.p1-1" "auto.p1-2" "auto.p2-1" "auto.p2-2"))
     ;; nothing to do the next time
     (check-false (renumber-auto-labels))))
  ;; a tag with two labels at the end of a part and at the start of the
  ;; next one: the sequences of the parts both start at 1
  (with-typeset-body
   '(document (assign "two" (macro "x" (concat (index (arg "x"))
                                               (index (concat (arg "x") "2")))))
              (set-part "A" (document (concat "a " (two "foo")) "text a"))
              (set-part "B" (document (concat "b " (two "bar")) "text b"))
              "end")
   (lambda ()
     (update-forced)
     (check= (sort (list-filter (list-references)
                                (lambda (l) (string-starts? l "auto.")))
                   string<?)
             '("auto.A-1" "auto.A-2" "auto.B-1" "auto.B-2")))))

;; A tag which is removed and put back by an undo gives its labels the
;; names they had (the undo puts back a copy with their numbers).
(define (test-labels-after-undo)
  (check-group "labels after undo")
  (with-typeset-body
   '(document (section "A") "a" (section "B") "b" (section "C") "c")
   (lambda ()
     (renumber-auto-labels)
     (update-forced)
     (start-editing)
     (tree-remove (buffer-tree) 2 2)
     (end-editing)
     (update-forced)
     (undo 0)
     (update-forced)
     (check= (auto-names) '("auto-1" "auto-2" "auto-3")))))

;; Paragraphs removed one after the other in a document with numbered
;; equations: the paragraphs are kept in blocks (bridge_document.cpp), and
;; the removal of the first paragraph of a block changes the range of the
;; block before it. The numbers which the later references find must be
;; those of a typesetting from scratch (not their pages, which are only
;; found again by a complete typesetting: determine_page_references).
(define (equation-label i) (string-append "eq" (number->string i)))

(define (equation-references labels)
  (map (lambda (lab) (cadr (tree->stree (get-reference lab)))) labels))

(define (test-references-after-removals)
  (check-group "references after removals")
  (let* ((n 150)
         (par (lambda (i)
                (if (and (> i 0) (< i (- n 1)) (== (modulo i 3) 1))
                    `(equation (document (concat "x" (label ,(equation-label i)))))
                    (string-append "paragraph " (number->string i)))))
         (labels (map equation-label
                      (list-filter (.. 100 (- n 1))
                                   (lambda (i) (== (modulo i 3) 1))))))
    (with-typeset-body
     `(document ,@(map par (.. 0 n)))
     (lambda ()
       (update-forced)
       (let ((bad 0))
         (for (k (.. 0 30))
           ;; the paragraph of text after the 20th one: those before it
           ;; are removed one by one
           (tree-remove (buffer-tree) 20 1)
           (update-forced)
           (update-forced)
           (let* ((inc (equation-references labels))
                  (body (tree-copy (buffer-tree)))
                  (old (current-buffer))
                  (u (new-buffer)))
             (switch-to-buffer* u)
             (buffer-set-body u body)
             (update-forced)
             (update-forced)
             (with scr (equation-references labels)
               (buffer-close u)
               (switch-to-buffer* old)
               (when (!= inc scr) (set! bad (+ bad 1))))))
         (check= bad 0))))))

;; Random edits of a document, each followed by a typesetting which uses
;; again what it can, against a typesetting from scratch in a buffer of its
;; own: the places of all the paragraphs must be the same. The document
;; holds what the reuse of the typesetter has to see: sections and numbered
;; equations (counters), references to earlier equations, index entries
;; (automatic labels), footnotes, hidden paragraphs which only pass on a
;; space, and a length in a unit made by a macro of the document. When the
;; check modes are on (TEXMACS_TYPESET_READS=check, TEXMACS_TYPESET_CACHES=
;; check...), none of them may have found a difference either.
(define rng-state 1)
(define (rng n)
  (set! rng-state (modulo (+ (* rng-state 1103515245) 12345) 2147483648))
  (modulo (quotient rng-state 65536) n))

(define (random-paragraph i)
  (with s (number->string i)
    (case (rng 7)
      ((0) `(section ,(string-append "Section " s)))
      ((1) `(concat ,(string-append "Text " s " ") (index ,(string-append "key " s))))
      ((2) `(equation (concat "x" (label ,(string-append "e" s)))))
      ((3) `(with "par-par-sep" "0fn" (hidden "h")))
      ((4) `(concat "a" (footnote ,(string-append "note " s)) " b"))
      ((5) `(concat "f " (math (frac "aaaaaaaaaa" "b"))))
      (else (string-append "Some text, number " s ".")))))

(define (scratch-places body n)
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (tree-copy body))
    (update-forced)
    (with r (map (lambda (i) (rect i)) (iota n))
      (buffer-close u)
      (switch-to-buffer* old)
      r)))

(define (test-incremental-against-scratch)
  (check-group "incremental against scratch")
  (set! rng-state 4242)
  (with-typeset-body
   `(document (assign "myu-length" (macro "0.1mm"))
              (assign "math-frac-limit" "5myu")
              ,@(map random-paragraph (iota 50)))
   (lambda ()
     (let ((bad 0) (next 50))
       (for (step (iota 40))
         (let* ((t (buffer-tree)) (n (tree-arity t))
                (op (if (== (modulo step 5) 4) 2 (rng 5))))
           (cond ((== op 0)
                  ;; a new paragraph
                  (tree-insert t (+ 2 (rng (- n 2)))
                               (list (random-paragraph next)))
                  (set! next (+ next 1)))
                 ((and (== op 1) (> n 10))
                  ;; a paragraph removed (not an equation: references to
                  ;; it would keep their number until a complete pass)
                  (with i (+ 2 (rng (- n 2)))
                    (when (not (tree-is? (tree-ref t i) 'equation))
                      (tree-remove t i 1))))
                 ((== op 2)
                  ;; the unit of the length
                  (tree-set (tree-ref t 0 1 0)
                            (if (== (tree->string (tree-ref t 0 1 0)) "10cm")
                                "0.1mm" "10cm")))
                 ((== op 3)
                  ;; a reference to an earlier equation, at the end
                  (let loop ((i (- n 1)))
                    (cond ((< i 2) (noop))
                          ((tree-is? (tree-ref t i) 'equation)
                           (with lab (tree->stree (tree-ref t i 0 1 0))
                             (tree-insert t n
                                          (list `(concat "see " (reference ,lab))))))
                          (else (loop (- i 1))))))
                 (else
                  ;; a word added to a paragraph of text
                  (with i (+ 2 (rng (- n 2)))
                    (when (tree-atomic? (tree-ref t i))
                      (tree-set (tree-ref t i)
                                (string-append (tree->string (tree-ref t i))
                                               " more"))))))
           (update-forced)
           (let* ((m (tree-arity (buffer-tree)))
                  (inc (map (lambda (i) (rect i)) (iota m))))
             (when (!= inc (scratch-places (buffer-tree) m))
               (set! bad (+ bad 1))))))
       (check= bad 0)
       (check= (typeset-check-mismatches) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random documents against scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Random documents made of what the audits of the incremental typesetting
;; found wrong (bindings and assignments of a variable, variables which are
;; not there, theorems with a name moved by a macro, labels and references,
;; parts, footnotes, columns, a Scheme routine, hidden paragraphs, tables,
;; nested documents), random edits of them (insertions, removals, copies,
;; moves, removals put back as an undo does), and after each edit the
;; places of all the paragraphs (also
;; in the nested documents) and the numbers of the references, against a
;; typesetting from scratch. (typeset-fuzz seed edits size medium) returns
;; the number of edits after which they differ, for long runs: the suite
;; runs a few small ones.

(define fz-state 1)
(define (fz-rnd n)
  (set! fz-state (modulo (+ (* fz-state 1103515245) 12345) 2147483648))
  (modulo (quotient fz-state 65536) n))
(define (fz-pick l) (list-ref l (fz-rnd (length l))))
(define fz-labels 0)
(define (fz-label)
  (set! fz-labels (+ fz-labels 1))
  (string-append "fz" (number->string fz-labels)))
(define (fz-text)
  (fz-pick '("alpha beta gamma" "a much longer paragraph of text which goes on"
             "x" "short words here" "delta epsilon zeta eta theta iota kappa")))
(define (fz-length) (fz-pick '("1cm" "2cm" "3mm" "0.5fn" "1fn")))

(define (fz-leaf depth)
  (case (fz-rnd (if (> depth 0) 15 16))
    ((0) (fz-text))
    ((1) `(assign "fa" ,(fz-length)))
    ((2) `(with "fa" ,(fz-length) (document (assign "fa" ,(fz-length)) "inner")))
    ((3) `(concat "v " (vspace (value "fa")) "x"))
    ((4) `(concat "u " (value "fu") "."))
    ((5) `(with "fu" "z" (document "w")))
    ((6) `(equation (document (concat "x=y" (label ,(fz-label))))))
    ((7) `(concat "see " (reference ,(string-append "fz" (number->string
                                                         (+ 1 (fz-rnd (max 1 fz-labels))))))))
    ((8) `(theorem (document ,(fz-text) ,(fz-text))))
    ((9) `(concat "n" (footnote "a note") " " ,(fz-text)))
    ((10) `(with "par-columns" "2" (document ,(fz-text) ,(fz-text) ,(fz-text))))
    ((11) `(concat (date) " d"))
    ((12) `(with "par-par-sep" "0fn" (hidden "h")))
    ((13) `(tabular (tformat (cwith "1" "1" "1" "1" "fb" "v")
                             (table (row (cell "c") (cell (value "fb")))))))
    ((14) `(concat "i " (index ,(fz-text)) " " ,(fz-text)))
    (else `(with "fb" ,(fz-length)
             (document ,(fz-leaf (+ depth 1)) ,(fz-leaf (+ depth 1))
                       ,(fz-leaf (+ depth 1)))))))

(define (fz-document size)
  `(document (assign "theorem-name"
                     (macro "name" (move (strong (arg "name")) "0pt" "0pt")))
             (assign "fa" "1cm")
             ,@(map (lambda (i) (fz-leaf 0)) (iota size))))

(define (fz-body c)
  ;; the document inside a paragraph, if any
  (cond ((and (tree-compound? c) (tree-is? c 'theorem)) (tree-ref c 0))
        ((and (tree-compound? c) (tree-is? c 'with)
              (tree-is? (tree-ref c (- (tree-arity c) 1)) 'document))
         (tree-ref c (- (tree-arity c) 1)))
        (else #f)))

(define (fz-documents t)
  (let loop ((i 0) (acc (list t)))
    (if (>= i (tree-arity t)) acc
        (with b (fz-body (tree-ref t i))
          (loop (+ i 1) (if b (append (fz-documents b) acc) acc))))))

(define (fz-places t)
  (apply append
         (map (lambda (i)
                (with c (tree-ref t i)
                  (cons (tree-bounding-rectangle c)
                        (with b (fz-body c) (if b (fz-places b) '())))))
              (iota (tree-arity t)))))

(define (fz-labels-in t)
  (cond ((tree-atomic? t) '())
        ((and (tree-is? t 'label) (tree-atomic? (tree-ref t 0)))
         (list (tree->string (tree-ref t 0))))
        (else (apply append (map fz-labels-in (tree-children t))))))

(define (fz-snapshot)
  (list (fz-places (buffer-tree))
        (map (lambda (l)
               (with r (tree->stree (get-reference l))
                 (if (and (pair? r) (pair? (cdr r))) (cadr r) r)))
             (fz-labels-in (buffer-tree)))))

(define (fz-typeset)
  (update-forced)
  (update-forced))

(define (fz-scratch body medium)
  (let* ((old (current-buffer)) (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (tree-copy body))
    (init-env "page-medium" medium)
    (update-current-buffer)
    (fz-typeset)
    (with r (fz-snapshot)
      (buffer-close u)
      (switch-to-buffer* old)
      r)))

(define (fz-edit)
  (let* ((ds (fz-documents (buffer-tree)))
         (d (fz-pick ds))
         (n (tree-arity d))
         (pos (if (<= n 2) (min 1 n) (+ 1 (fz-rnd (- n 1))))))
    (case (fz-rnd 10)
      ((0 1) (tree-insert d pos (list (fz-leaf 1))))
      ((2) (when (> n 3) (tree-remove d pos 1)))
      ((3) (when (> n 5) (tree-remove d pos (min 3 (- n pos)))))
      ((4) (when (< pos n) (tree-set (tree-ref d pos) (fz-leaf 1))))
      ((5) ;; removed and put back (as an undo does)
       (when (< pos n)
         (with s (tree->stree (tree-ref d pos))
           (tree-remove d pos 1)
           (fz-typeset)
           (tree-insert d pos (list (stree->tree s))))))
      ((6) ;; replaced by an identical copy (a new bridge)
       (when (< pos n) (tree-assign (tree-ref d pos) (tree-copy (tree-ref d pos)))))
      ((7) ;; moved
       (when (and (< pos n) (> n 3))
         (with s (tree->stree (tree-ref d pos))
           (tree-remove d pos 1)
           (tree-insert d (+ 1 (fz-rnd (- n 2))) (list (stree->tree s))))))
      ((8) ;; the name of the theorems moved
       (tree-set (tree-ref (buffer-tree) 0 1 1 2) (fz-pick '("0pt" "2ex" "4ex"))))
      (else ;; the value of the variable assigned at the start
       (tree-set (tree-ref (buffer-tree) 1 1) (fz-length))))))

(tm-define (typeset-fuzz seed edits size medium)
  (:synopsis "Random edits against scratch; the number of edits which differ")
  (set! fz-state seed)
  (set! fz-labels 0)
  (let* ((old (current-buffer)) (u (new-buffer)) (bad 0))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree (fz-document size)))
    (init-env "page-medium" medium)
    (update-current-buffer)
    (fz-typeset)
    (for (k (iota edits))
      (fz-edit)
      (fz-typeset)
      (when (!= (fz-snapshot) (fz-scratch (buffer-tree) medium))
        (set! bad (+ bad 1))))
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))
    bad))

(define (test-random-documents)
  (check-group "random documents against scratch")
  ;; (not with the classic cache, TEXMACS_TYPESET_READS=off, whose old
  ;; mistakes these documents find)
  (when (!= (getenv "TEXMACS_TYPESET_READS") "off")
    (check= (typeset-fuzz 17 12 30 "paper") 0)
    (check= (typeset-fuzz 29 12 30 "papyrus") 0)
    (check= (typeset-check-mismatches) 0)))

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
  (test-renumbering)
  (test-automatic-labels)
  (test-labels-of-nested-tags)
  (test-reuse-below-bindings)
  (test-page-breaks-kept)
  (test-tables)
  (test-paragraphs)
  (test-pages)
  (test-pages-kept)
  (test-typesetting-failure)
  (test-derived-state)
  (test-renumbering-labels)
  (test-label-names)
  (test-bindings-against-scratch)
  (test-labels-of-parts)
  (test-labels-after-undo)
  (test-incremental-against-scratch)
  (test-random-documents)
  (test-block-replay)
  (test-references-after-removals)
  (check-end))
