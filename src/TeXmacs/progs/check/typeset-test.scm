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
(define (rect-width r) (- (rect-x2 r) (rect-x1 r)))

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

(define (with-scratch-buffer thunk)
  ;; (@thunk u) in a new buffer u, which is closed afterwards, also when an
  ;; error goes through
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (dynamic-wind
      (lambda () (noop))
      (lambda ()
        (switch-to-buffer* u)
        (thunk u))
      (lambda ()
        (buffer-close u)
        (when (buffer-exists? old) (switch-to-buffer* old))))))

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

;; A label which is removed: the references to it show "?" after the
;; update which removes it, Update > All in the order of 2.1.4 (complete
;; typesetting, without renumbering the automatic labels). The table of
;; the references is cleaned at the end of a complete pass, and one more
;; pass typesets what read the removed label (#417).
(define (test-removed-label)
  (check-group "removed label")
  (with-typeset-body
   '(document (section (concat "A" (label "s1")))
              (concat "See " (reference "s2")
                      (if (equal (get-binding "s2") "2") (vspace "5cm") ""))
              "After."
              (section (concat "B" (label "s2")))
              "Text.")
   (lambda ()
     (let* ((update (lambda ()
                      (update-forced)
                      (generate-all-aux)
                      (update-current-buffer)
                      (update-forced)))
            (nr (lambda (l) (cadr (tree->stree (get-reference l)))))
            ;; the height of the paragraph of the reference, with its space
            (height (lambda () (- (rect-y2 (rect 0)) (rect-y2 (rect 2))))))
       (update)
       (update)
       (check= (nr "s2") "2")
       (with h (height)
         (tree-set (tree-ref (buffer-tree) 3) '(section "B"))
         (update)
         (check= (tree->stree (get-reference "s2")) '(uninit))
         (check= (tree->stree (texmacs-expand '(get-binding "s2"))) '(uninit))
         (check-true (< (height) h))
         ;; and back
         (tree-set (tree-ref (buffer-tree) 3)
                   '(section (concat "B" (label "s2"))))
         (update)
         (check= (nr "s2") "2")
         (check= (height) h))))))

;; The pass which a removed label asks for is a complete one: a label which
;; only disappears because of it (here dep, made when s2 is 2) is removed
;; too, and the references to it show "?" (#408: a pass which uses bridges
;; again does not clean the tables). (typeset-redone-count counts the
;; bridges typeset again with the reuse by inputs: it stays 0 with
;; TEXMACS_TYPESET_READS=off.)
(define (test-removed-label-chain)
  (check-group "removed label, chain")
  (with-typeset-body
   '(document (section (concat "A" (label "s1")))
              (concat "X" (if (equal (get-binding "s2") "2") (label "dep") ""))
              (concat "See dep: " (reference "dep"))
              (section (concat "B" (label "s2")))
              "Text.")
   (lambda ()
     (let* ((update (lambda ()
                      (update-forced)
                      (generate-all-aux)
                      (update-current-buffer)
                      (update-forced))))
       (update)
       (update)
       (check-true (in? "dep" (list-references)))
       (tree-set (tree-ref (buffer-tree) 3) '(section "B"))
       (update)
       (check-false (in? "s2" (list-references)))
       (check-false (in? "dep" (list-references)))
       (check= (tree->stree (get-reference "dep")) '(uninit))))))

;; Labels removed, put back, added and renumbered by edits, with the reuse
;; by inputs: the pass of an edit is not a complete one (unlike the updates
;; of test-removed-label above, which make complete passes), and the
;; references to these labels, also those before them, show after it what
;; a typesetting from scratch shows (#571): "?" for a removed label, not
;; its old number until Update All. One more pass typesets again what reads
;; them, the other paragraphs are used again. (Not with the classic cache,
;; TEXMACS_TYPESET_READS=off, which keeps the entry of a removed label until
;; a complete typesetting.)
(define (rebinding-state)
  ;; the references of the paragraph "see ..." and their widths (not their
  ;; pages, which only a complete pass finds: determine_page_references)
  (list (map (lambda (l)
               (with r (tree->stree (get-reference l))
                 (if (and (pair? r) (pair? (cdr r))) (cadr r) r)))
             '("a" "b" "c" "z"))
        (map (lambda (i) (rect-width (rect 1 i))) '(1 3 5))))

(define (rebinding-eq l)
  `(equation (document (concat "x=y" (label ,l)))))

(define (rebinding-costs thunk)
  ;; the passes and the bridges typeset again by (@thunk) and a typesetting
  (let ((p0 (typeset-pass-count))
        (r0 (typeset-redone-count)))
    (thunk)
    (update-forced)
    (list (- (typeset-pass-count) p0) (- (typeset-redone-count) r0))))

(define (test-removed-label-by-edit)
  (check-group "labels removed or added by an edit")
  (when (!= (getenv "TEXMACS_TYPESET_READS") "off")
   (with-typeset-body
    `(document "start"
               (concat "see " (reference "c") " and " (reference "b")
                       " and " (reference "z"))
               (equation (document "u=v"))
               ,(rebinding-eq "a")
               ,(rebinding-eq "c")
               (section (concat "S" (label "b")))
               ,@(map (lambda (i) (string-append "Paragraph " (number->string i)))
                      (iota 20)))
    (lambda ()
      (update-current-buffer)
      (update-forced)
      (update-forced)
      (check= (rebinding-state) (from-scratch rebinding-state))
      ;; (the check mode typesets again every bridge which is used again)
      (let* ((check? (== (getenv "TEXMACS_TYPESET_READS") "check"))
             ;; a paragraph without labels removed: one pass
             (plain (rebinding-costs
                     (lambda () (tree-remove (buffer-tree) 7 1))))
             ;; an equation without a label removed: the labels after it
             ;; are renumbered, and one more pass typesets again the
             ;; paragraph of the references, which is before them
             (renumbered (rebinding-costs
                          (lambda () (tree-remove (buffer-tree) 2 1))))
             ;; an equation with a label removed: the same, its label goes
             (removed (rebinding-costs
                       (lambda () (tree-remove (buffer-tree) 2 1)))))
        (check= (car plain) 1)
        (check= (car renumbered) 2)
        (check= (car removed) 2)
        ;; (a few bridges more at most: those of the references)
        (when (not check?)
          (check-true (<= (cadr removed) (+ (cadr renumbered) 3)))))
      (check= (tree->stree (get-reference "a")) '(uninit))
      (check= (rebinding-state) (from-scratch rebinding-state))
      ;; the last equation removed, then put back (as an undo does)
      (tree-remove (buffer-tree) 2 1)
      (update-forced)
      (check= (rebinding-state) (from-scratch rebinding-state))
      (tree-insert (buffer-tree) 2 (list (rebinding-eq "c")))
      (update-forced)
      (check= (cadr (tree->stree (get-reference "c"))) "1")
      (check= (rebinding-state) (from-scratch rebinding-state))
      ;; an equation inserted before it: renumbered
      (tree-insert (buffer-tree) 2 (list (rebinding-eq "a")))
      (update-forced)
      (check= (cadr (tree->stree (get-reference "c"))) "2")
      (check= (rebinding-state) (from-scratch rebinding-state))
      ;; a new label, referred to before it
      (tree-insert (buffer-tree) 5 (list (rebinding-eq "z")))
      (update-forced)
      (check= (cadr (tree->stree (get-reference "z"))) "3")
      (check= (rebinding-state) (from-scratch rebinding-state))
      ;; the label of a section removed from it, then put back
      (tree-set (tree-ref (buffer-tree) 4) '(section "S"))
      (update-forced)
      (check= (tree->stree (get-reference "b")) '(uninit))
      (check= (rebinding-state) (from-scratch rebinding-state))
      (tree-set (tree-ref (buffer-tree) 4) '(section (concat "S" (label "b"))))
      (update-forced)
      (check= (cadr (tree->stree (get-reference "b"))) "1")
      ;; a key bound twice: when one of the bindings is removed, the
      ;; other one stays
      (tree-insert (buffer-tree) 6 '((concat "T" (set-binding "b" "X"))))
      (update-forced)
      (check= (cadr (tree->stree (get-reference "b"))) "X")
      (tree-remove (buffer-tree) 6 1)
      (update-forced)
      (check= (cadr (tree->stree (get-reference "b"))) "1")
      (check= (rebinding-state) (from-scratch rebinding-state))
      (check= (typeset-check-mismatches) 0)))
   ;; a label which only a running header makes (the pager's bindings are
   ;; compared too), removed from it
   (with-typeset-body
    `(document (concat "see " (reference "h"))
               (set-header (concat "Head" (label "h")))
               ,@(map (lambda (i) (string-append "Paragraph " (number->string i)))
                      (iota 5)))
    (lambda ()
      (init-env "page-medium" "paper")
      (update-current-buffer)
      (update-forced)
      (update-forced)
      (check-true (in? "h" (list-references)))
      (tree-set (tree-ref (buffer-tree) 1) '(set-header "Head"))
      (update-forced)
      (check= (tree->stree (get-reference "h")) '(uninit))
      (with w (lambda () (rect-width (rect 0 1)))
        (check= (w) (from-scratch w)))
      (check= (typeset-check-mismatches) 0)))))

;; A file holds the table of its references as it was saved: entries which no
;; label of the body defines any more (old names of automatic labels, labels
;; removed since) are cleaned by the first complete pass, and are no reason
;; for another pass unless a reference to one of them was typeset (#408: a
;; chapter of a book with some hundreds of them was typeset twice from
;; scratch).
(define (stale-file name body refs)
  (tree-export
   (stree->tree
    `(document (TeXmacs "2.1")
               (style (tuple "article"))
               (body (document ,@body))
               (references
                (collection
                 ,@(map (lambda (r) `(associate ,r (tuple "7" "3"))) refs)))))
   (incl-file name) "texmacs"))

(define (stale-typeset name)
  ;; the passes and the bridges typeset again by the first typesetting of
  ;; the file, and the references of the buffer afterwards
  (let* ((p0 (typeset-pass-count))
         (r0 (typeset-redone-count))
         (u (incl-file name)))
    (load-buffer u)
    (switch-to-buffer* u)
    (update-forced)
    (list (- (typeset-pass-count) p0) (- (typeset-redone-count) r0)
          (list-references))))

(define (test-stale-references)
  (check-group "stale references")
  (let ((old (current-buffer))
        (before (buffer-list))
        (mismatches (typeset-check-mismatches))
        (body '((section (concat "A" (label "s1")))
                (concat "See " (reference "s1") ".")
                "Text.")))
    (incl-clear-dir)
    (system-mkdir (system->url incl-dir))
    (with r (check-run
             (lambda ()
               ;; no stale entry: the reference
               (stale-file "clean.tm" body '())
               (with c (stale-typeset "clean.tm")
                 (check= (car c) 1)
                 (check-true (in? "s1" (caddr c)))
                 ;; entries which nothing reads: one pass, as without them,
                 ;; and the table is cleaned
                 (stale-file "stale.tm" body
                             (list "old-1" "old-2" "auto-9" "s0"))
                 (with s (stale-typeset "stale.tm")
                   (check= (car s) 1)
                   (check= (cadr s) (cadr c))
                   (check= (caddr s) (caddr c))))
               ;; a reference to an entry which is not a label of the body
               ;; shows the old value in the first pass, and "?" in the
               ;; next (#417)
               (stale-file "read.tm"
                           '((concat "See " (reference "old-1") ".")
                             "Text.")
                           (list "old-1" "old-2"))
               (with s (stale-typeset "read.tm")
                 (check= (car s) 2)
                 (check-false (in? "old-1" (caddr s))))))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before)
          (buffer-pretend-saved b)
          (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer* old))
      (incl-clear-dir)
      (check= (typeset-check-mismatches) mismatches))))

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
         (check= (top) before)
         (tree-set (tree-ref (buffer-tree) 2) "body tvo")
         (update-forced)
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
                 (width (math '(frac "aaa" "b")))))
  ;; a script given as the argument of a macro is placed as the same script
  ;; written out: its box is narrowed back by the italic corrections by
  ;; which it was widened, although the markers of width zero around the
  ;; argument are then inside it (with an OpenType math font, the subscript
  ;; was further away)
  (let ((termes (lambda (t) `(with "font" "termes" "math-font" "math-termes"
                               ,(math t)))))
    (check= (width (termes '(with "f" (macro "c" (concat (around* "(" "x" ")")
                                                         (rsub (arg "c"))))
                              (concat (f "p") "q"))))
            (width (termes '(concat (around* "(" "x" ")") (rsub "p") "q"))))
    ;; (fractions do not read the extents of their children again after
    ;; positioning them: this only checks that they are unchanged)
    (check= (width (termes '(with "f" (macro "c" (frac (arg "c") "2"))
                              (concat (f "p") "q"))))
            (width (termes '(concat (frac "p" "2") "q"))))))

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
       (check-true (< (rect-y2 rw) (rect-y1 rc))))))
  ;; a table whose width is in par takes it from the paragraph where it is
  ;; laid out (a cell whose lines are broken, the body of a frame), also when
  ;; the width is given by a value (<value|figure-width> of the figures),
  ;; with the value where the table is, set locally or a macro argument:
  ;; its cell aligned on the right ends at the same place
  (let* ((inner (lambda (w)
                  `(tformat (twith "table-width" ,w) (twith "table-hmode" "exact")
                            (cwith "1" "1" "1" "1" "cell-halign" "r")
                            (table (row (cell "a"))))))
         (in-cell (lambda (x)
                    `(document
                       (assign "w" "1par")
                       (assign "mt" (macro "x" ,(inner '(arg "x"))))
                       (assign "ml" (macro "x" ,(inner "0.5par")))
                       (tformat (twith "table-width" "1par")
                                (twith "table-hmode" "exact")
                                (cwith "1" "1" "1" "1" "cell-hyphen" "t")
                                (cwith "1" "1" "1" "-1" "cell-hpart" "1")
                                (table (row (cell (document ,x))
                                            (cell "b")))))))
         (in-frame (lambda (w)
                     `(document (assign "w" "1par")
                                (framed (document ,(inner w))))))
         (pos-of (lambda (acc doc . p)
                   (let ((x #f))
                     (with-typeset-body doc
                       (lambda () (set! x (acc (apply rect p)))))
                     x))))
    (check= (pos-of rect-x1 (in-cell (inner '(value "w"))) 3 4 0 0 0 0 3 0 0 0)
            (pos-of rect-x1 (in-cell (inner "1par")) 3 4 0 0 0 0 3 0 0 0))
    (check= (pos-of rect-x1 (in-frame '(value "w")) 1 0 0 3 0 0 0)
            (pos-of rect-x1 (in-frame "1par") 1 0 0 3 0 0 0))
    (check= (pos-of rect-x1 (in-cell `(with "w" "0.5par" ,(inner '(value "w"))))
                    3 4 0 0 0 0 2 3 0 0 0)
            (pos-of rect-x1 (in-cell (inner "0.5par")) 3 4 0 0 0 0 3 0 0 0))
    (check= (pos-of rect-x2 (in-cell '(mt "0.5par")) 3 4 0 0 0 0)
            (pos-of rect-x2 (in-cell '(ml "0.5par")) 3 4 0 0 0 0)))
  ;; what a hidden part assigns in a cell whose lines are broken (the
  ;; abstract of tmarticle, a hide-preamble in it) holds for the paragraphs
  ;; after it, as when the assignment is not hidden: the hidden part was
  ;; typeset only when the cell was broken into lines, after them, and the
  ;; macro was undefined there
  (let* ((in-cell (lambda (x)
                    `(document
                       (tformat (cwith "1" "1" "1" "1" "cell-hyphen" "t")
                                (table (row (cell (document ,x
                                                            (concat "a" (ww))))))))))
         (def '(assign "ww" (macro "wwwwwwwwww")))
         (end-of (lambda (doc)
                   (let ((x #f))
                     (with-typeset-body doc
                       (lambda () (set! x (rect-x2 (rect 0 1 0 0 0 1)))))
                     x))))
    (check= (end-of (in-cell `(hidden ,def)))
            (end-of (in-cell def))))
  ;; the hidden part, typeset where it occurs, is in the record of the
  ;; paragraph around the table: the paragraph used again writes the
  ;; bindings of the hidden part again (its labels), and when the label is
  ;; removed from it, the binding goes (drop_bindings). The assignment holds
  ;; after the table too, as when it is not hidden
  (let* ((doc `(document
                 (section (concat "S" (label "s0")))
                 (concat "see " (reference "hx"))
                 (tformat (cwith "1" "1" "1" "1" "cell-hyphen" "t")
                          (table (row (cell (document
                            (hidden (concat "H" (label "hx")))
                            (hidden (assign "ww" (macro "wwwwwwwwww")))
                            (concat "a" (ww)))))))
                 (concat "a" (ww))
                 "P1" "P2"))
         (measure (lambda ()
                    (list (tree->stree (get-reference "hx"))
                          (rect-width (rect 3))
                          (rect-x2 (rect 2 1 0 0 0 2))))))
    (with-typeset-body doc
      (lambda ()
        (update-current-buffer)
        (update-forced)
        (update-forced)
        (check= (measure) (from-scratch measure))
        (check= (rect-width (rect 3)) (rect-width (rect 2 1 0 0 0 2)))
        (tree-set (tree-ref (buffer-tree) 5) "P2 changed")
        (update-forced)
        (check= (cadr (tree->stree (get-reference "hx"))) "1")
        (check= (measure) (from-scratch measure))
        (tree-set (path->tree (at 2 1 0 0 0 0)) '(hidden "H"))
        (update-forced)
        ;; (the classic cache keeps a removed label until a complete pass)
        (when (!= (getenv "TEXMACS_TYPESET_READS") "off")
          (check= (tree->stree (get-reference "hx")) '(uninit))
          (check= (measure) (from-scratch measure)))
        (check= (typeset-check-mismatches) 0)))))

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

;; Markers: the boundaries of a macro, of its arguments and of a with are
;; places for the cursor (concater_rep::marker), which do not change the
;; layout: markup given by a macro or inside a with is typeset as the same
;; markup written out. A script after them takes the base which the markup
;; written out gives it; their height does not count. The cursor at the
;; start and at the end of a with is next to its content: the space after an
;; operator goes after the markers which end a macro, inside a fraction as in
;; a paragraph. At the end of a line, the space before markers does not
;; count, also before those of the macros of a style; a table cell counts the
;; spaces of its content as the lines of its paragraph do. The italic
;; correction after an italic glyph stays inside the macro which ends with it.
(define (test-markers)
  (check-group "markers")
  (let ((same (lambda (t1 t2)
                (check= (box t1 "lbrtLBRT") (box t2 "lbrtLBRT"))))
        (with-same-as (lambda (t) `(with "same-as" (macro "x" (arg "x")) ,t))))
    (same '(concat "T" (with "color" "brown" (rsub "E")))
          '(concat "T" (rsub "E")))
    (same (math '(concat "T" (with "color" "brown" (rsub "E"))))
          (math '(concat "T" (rsub "E"))))
    (same (math (with-same-as '(concat (same-as "C") (rsub "N") "+1")))
          (math '(concat "C" (rsub "N") "+1")))
    (same (with-same-as '(tabular (table (row (cell (same-as (rsup "o")))))))
          '(tabular (table (row (cell (rsup "o")))))))
  (with-typeset-body
   `(document
     ,(math '(frac (concat "x+" (with "color" "red" "a") "+b") "c"))
     ,(math '(concat "x+" (with "color" "red" "a") "+b")))
   (lambda ()
     (for (l '((0 2 0 1) (1 2 1)))
       (let ((w (apply rect l))
             (a (apply rect (append l '(2)))))
         (check= (rect-x1 w) (rect-x1 a))
         (check= (rect-x2 w) (rect-x2 a))))))
  (with-typeset-body
   '(document
     (with "ff" (macro "a" (arg "a"))
       (with "gg" (macro (with "mode" "math" (concat "x" (ff "+"))))
         (with "par-mode" "right"
           (document (gg)
                     (with "mode" "math" (concat "x" (ff "+")))
                     (with "mode" "math" "x+"))))))
   (lambda ()
     (with x1 (lambda (i) (rect-x1 (rect 0 2 2 2 i)))
       (check= (x1 0) (x1 2))
       (check= (x1 1) (x1 2)))))
  (with-typeset-body
   (with content (math '(concat "x+" (with "color" "red" (concat))))
     `(document
       (tabular (tformat (cwith "1" "1" "1" "1" "cell-hyphen" "t")
                         (table (row (cell (document ,content))))))
       (tabular (table (row (cell ,content))))))
   (lambda ()
     (with w (lambda (r) (- (rect-x2 r) (rect-x1 r)))
       (check= (w (rect 0)) (w (rect 1)))
       (check= (w (rect 0 0 1 0 0 0 0)) (w (rect 1 0 0 0 0))))))
  (with-typeset-body
   '(document (concat (em "f") ")"))
   (lambda ()
     (check-true (> (rect-x2 (rect 0 0)) (rect-x2 (rect 0 0 0)))))))

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

;; A long document outside paper mode (as a source file): its lines are one
;; item made of chunks of them, and a letter changes one chunk; the lines
;; keep their places, and are where a typesetting of the whole document
;; puts them.
(define (test-long-document-without-pages)
  (check-group "long document without pages")
  (with-typeset-body
   `(document ,@(map (lambda (i) (string-append "line " (number->string i)))
                     (iota 1500)))
   (lambda ()
     (init-env "page-medium" "automatic")
     (update-current-buffer)
     (update-forced)
     (let ((r10 (rect 10)) (r700 (rect 700)) (r1399 (rect 1399))
           (r1400 (rect 1400)))
       (check-true (> (rect-y1 r10) (rect-y1 r700)))
       (check-true (> (rect-y1 r700) (rect-y1 r1400)))
       ;; a letter in the middle: the lines keep their places
       (tree-set (tree-ref (buffer-tree) 700) "line 700 x")
       (update-forced)
       (check= (rect 10) r10)
       (check= (rect 1400) r1400)
       (check= (rect-y1 (rect 700)) (rect-y1 r700))
       (check-true (> (rect-x2 (rect 700)) (rect-x2 r700)))
       ;; a line removed: those after it go up by one line (the line at
       ;; 1400 takes the place of the one at 1399)
       (tree-remove (buffer-tree) 600 1)
       (update-forced)
       (check= (rect 10) r10)
       (check= (rect-y1 (rect 1399)) (rect-y1 r1399))
       ;; as after a typesetting of the whole document
       (let ((a (rect 5)) (b (rect 699)) (c (rect 1398)))
         (update-current-buffer)
         (update-forced)
         (check= (rect 5) a)
         (check= (rect 699) b)
         (check= (rect 1398) c))))))

;; A document whose typesetting fails is replaced by an empty one, and the
;; typesetting goes on, in it and in the other buffers (the bridges which
;; were recording what they read when it failed do not record any more).
;; The text is not lost silently (#704): a copy of the document is saved
;; in the system folder of the home, and saving the buffer over its file
;; is asked first.
(define (test-typesetting-failure)
  (check-group "typesetting failure")
  (let* ((dir (url->url "$TEXMACS_HOME_PATH/system/recovered"))
         (files (lambda () (map url->system (url-read-directory dir "*.tm"))))
         (before (files)))
    (with-typeset-body
     '(document "first" "second" (anim-compose) "third")
     (lambda ()
       (check= (tree->stree (buffer-tree)) '(document ""))
       (check-true (typeset-failed-buffer? (current-buffer)))
       ;; the copy of the document, made before it was replaced
       (let ((new (filter (lambda (f) (not (member f before))) (files))))
         (check-true (pair? new))
         (when (pair? new)
           (let ((text (string-load (car new))))
             (check-true (string-contains? text "second"))
             (check-true (string-contains? text "anim-compose"))
             (check-true (string-contains? text "third")))))))
    ;; the flag which makes Save ask first (tm-files.scm) lives as long as
    ;; the buffer (Save asks through the prompt of the footer, which a
    ;; script cannot answer: it is not run here)
    (let* ((file (url->url "$TEXMACS_HOME_PATH/system/typeset-failure-test.tm"))
           (text (string-append
                  "<TeXmacs|2.1.5>\n\n<style|<tuple|generic|1.0>>\n\n"
                  "<\\body>\n  one\n\n  <anim-compose>\n\n  keep this\n"
                  "</body>\n\n<\\initial>\n  <\\collection>\n  </collection>"
                  "\n</initial>\n")))
      (string-save text (url->system file))
      (load-buffer file)
      (switch-to-buffer* file)
      (update-forced)
      (check= (tree->stree (buffer-tree)) '(document ""))
      (check-true (typeset-failed-buffer? file))
      (check-true (string-contains? (string-load (url->system file)) "keep this"))
      (buffer-close file)
      (check-false (typeset-failed-buffer? file))
      (system-remove file)))
  (with-typeset-body
   '(document "one" "two" "three")
   (lambda ()
     (tree-set (tree-ref (buffer-tree) 1) "two again")
     (update-forced)
     (check= (tree->stree (buffer-tree)) '(document "one" "two again" "three"))
     (check-true (> (rect-x2 (rect 1)) (rect-x1 (rect 1)))))))

;; A document with nothing to show on a page, such as one made of hidden
;; content only, is typeset as an empty page and keeps its tree: it
;; used to throw "empty composite box" (papyrus: "unexpected situation") and
;; to be replaced by an empty document (#704).
(define (test-hidden-documents)
  (check-group "documents without visible content")
  (let ((typeset-as-it-is (lambda (doc medium)
    (with-typeset-body
     doc
     (lambda ()
       (init-env "page-medium" medium)
       (update-forced)
       (check= (tree->stree (buffer-tree)) doc)
       (check= (get-page-count) 1)
       ;; a visible line added, then removed again
       (tree-insert (buffer-tree) (length (cdr doc)) '("visible"))
       (update-forced)
       (check= (tree->stree (buffer-tree)) (append doc '("visible")))
       (check-true (> (rect-x2 (rect (length (cdr doc))))
                      (rect-x1 (rect (length (cdr doc))))))
       (tree-remove (buffer-tree) (length (cdr doc)) 1)
       (update-forced)
       (check= (tree->stree (buffer-tree)) doc)
       ;; a key typed at the start: no loop, the letter is in the document
       ;; (the cursor is in the hidden content when it is all there is)
       (go-start)
       (insert "q")
       (update-forced)
       (check-true (string-contains? (object->string (tree->stree (buffer-tree)))
                                     "q")))))))
  (for-each
   (lambda (medium)
     (for-each
      (lambda (doc) (typeset-as-it-is doc medium))
      '((document (hidden "secret text"))
        (document (concat (hidden "b")))
        (document (hidden "a") (hidden "b"))
        (document (with "color" "red" (hidden "x")))
        (document (shown "x"))
        (document (ignore "x"))
        (document (freeze "x"))
        (document (concat "" (hidden "b"))))))
   '("paper" "papyrus" "automatic")))
;; a document without paragraph holds an empty one (a key typed in it
  ;; looped), in all the media
  (for-each
   (lambda (medium)
     (with-typeset-body
      '(document)
      (lambda ()
        (init-env "page-medium" medium)
        (update-forced)
        (check= (tree->stree (buffer-tree)) '(document ""))
        (go-start)
        (insert "q")
        (update-forced)
        (check= (tree->stree (buffer-tree)) '(document "q")))))
   '("paper" "papyrus" "automatic"))
  ;; the text of the first repro of #704: all of it replaced
  (with-typeset-body
   '(document (concat "a1x" (hidden "b")))
   (lambda ()
     (tree-set (tree-ref (buffer-tree) 0 0) "")
     (update-forced)
     (check= (tree->stree (buffer-tree)) '(document (concat "" (hidden "b"))))
     ;; and undone
     (tree-set (tree-ref (buffer-tree) 0 0) "a1x")
     (update-forced)
     (check= (tree->stree (buffer-tree)) '(document (concat "a1x" (hidden "b")))))))

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
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (tree-copy body))
     (update-forced)
     (map (lambda (i) (rect i)) (iota n)))))

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

;; When the labels are numbered again (Document > Update > All), only the
;; bridges which make labels are typeset again, since they read the
;; variable auto-epoch which the update changes (exec_auto_id in
;; Typeset/Env/env_exec.cpp), and the others are used again (#408). The
;; document is then the one of a typesetting from scratch: the same labels
;; with the same values, the same auxiliary data, the same tables made from
;; them in the document, the paragraphs at the same places.
(define table-tags
  '(table-of-contents the-index the-glossary list-of-figures bibliography))

(define (labels-tables)
  ;; the tables which generate-all-aux made in the document
  (with t (buffer-tree)
    (list-filter (map (lambda (i) (tree->stree (tree-ref t i)))
                      (iota (tree-arity t)))
                 (lambda (x) (and (pair? x) (in? (car x) table-tags))))))

(define (labels-state)
  (list (map (lambda (l) (list l (tree->stree (get-reference l))))
             (auto-names))
        (map (lambda (a) (list a (tree->stree (get-auxiliary a))))
             (sort (list-auxiliaries) string<?))
        (labels-tables)
        ;; (the places from the first paragraph: the margins of a new
        ;; buffer and of a loaded file may differ)
        (let* ((l (map (lambda (i) (rect i)) (iota (tree-arity (buffer-tree)))))
               (x (rect-x1 (car l))) (y (rect-y1 (car l))))
          (map (lambda (r)
                 (list (- (rect-x1 r) x) (- (rect-y1 r) y)
                       (- (rect-x2 r) x) (- (rect-y2 r) y)))
               l))))

(define (update-all-labels)
  ;; Document > Update > All, without the inclusions-gc
  (renumber-auto-labels)
  (generate-all-aux)
  (update-current-buffer)
  (update-forced))

(define (update-all-kept)
  ;; Document > Update > All as it is now: the layout is made from scratch
  ;; first (inclusions-gc), the labels are numbered and the tables made
  ;; from it, and the end only types again what the tables changed
  ;; (update-current-buffer-keep: a complete pass, which uses the bridges)
  (inclusions-gc)
  (renumber-auto-labels)
  (generate-all-aux)
  (update-current-buffer-keep)
  (update-forced))

(define (labels-state-from-scratch body)
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (tree-copy body))
     (update-forced)
     (update-all-labels)
     (update-all-labels)
     (labels-state))))

(define (test-renumbering-against-scratch)
  (check-group "labels numbered again, against scratch")
  (with-typeset-body
   '(document (table-of-contents "toc" (document ""))
              (assign "two" (macro "x" (concat (index (arg "x"))
                                               (index (arg "x")))))
              (section "A")
              (concat "a" (two "first"))
              (section "B")
              (concat "b" (index "second"))
              (section "C")
              "c"
              (the-index "idx" (document "")))
   (lambda ()
     (update-all-labels)
     (update-all-labels)
     (check= (labels-state) (labels-state-from-scratch (buffer-tree)))
     ;; sections and an index entry added before the others: their labels
     ;; take new numbers, until the update
     (tree-insert (buffer-tree) 4 '((section "N") (concat "n" (index "new"))))
     (update-forced)
     (tree-insert (buffer-tree) 2 '((section "M")))
     (update-forced)
     (check-true (renumber-auto-labels))
     (generate-all-aux)
     (update-current-buffer)
     (update-forced)
     (with names (auto-names)
       (check= names (auto-range (length names))))
     (check= (labels-state) (labels-state-from-scratch (buffer-tree)))
     ;; nothing to do the next time
     (check-false (renumber-auto-labels))
     (update-forced)
     (check= (labels-state) (labels-state-from-scratch (buffer-tree)))
     ;; a section removed
     (tree-remove (buffer-tree) 2 1)
     (update-forced)
     (update-all-labels)
     (with names (auto-names)
       (check= names (auto-range (length names))))
     (check= (labels-state) (labels-state-from-scratch (buffer-tree))))))

;; The update of everything keeps the layout of the renumbering pass
;; (#408): inclusions-gc makes everything typeset from scratch, once, the
;; renumbering pass is that typesetting, and the pass which follows the
;; regeneration of the tables uses the bridges again (it is a complete pass,
;; so that the page numbers of the references and the missing references are
;; those of a typesetting from scratch). The result is the one of a buffer
;; updated from scratch, whatever the order of the edits.
(define (test-update-all-kept)
  (check-group "update all keeps the layout")
  (with-typeset-body
   '(document (table-of-contents "toc" (document ""))
              (assign "two" (macro "x" (concat (index (arg "x"))
                                               (index (arg "x")))))
              (assign "mywrite" (macro "x" (write "mine" (tuple (arg "x")))))
              (section "A")
              (concat "a" (two "first") (label "la"))
              (concat "w" (mywrite "w1") (reference "lb") (pageref "lb"))
              (section "B")
              (concat "b" (index "second") (label "lb"))
              (section "C")
              (concat "c" (mywrite "w2") (reference "la"))
              (the-index "idx" (document "")))
   (lambda ()
     (update-all-kept)
     (update-all-kept)
     (check= (labels-state) (labels-state-from-scratch (buffer-tree)))
     ;; inclusions-gc changes no environment when no file changed (the
     ;; classic cache types it all again, and keeps that for the next pass)
     (when (!= (getenv "TEXMACS_TYPESET_READS") "off")
       (inclusions-gc)
       (check-false (notified-change? 8))
       (renumber-auto-labels)
       (check-false (notified-change? 8))
       (generate-all-aux)
       (update-current-buffer-keep)
       (update-forced)
       (check= (labels-state) (labels-state-from-scratch (buffer-tree))))
     ;; sections, an index entry, a write and a label before the others
     (tree-insert (buffer-tree) 4 '((section "N")
                                    (concat "n" (index "new") (label "ln")
                                            (mywrite "w3"))))
     (update-forced)
     (tree-insert (buffer-tree) 2 '((section "M")))
     (update-forced)
     (update-all-kept)
     (with names (auto-names)
       (check= names (auto-range (length names))))
     (check= (labels-state) (labels-state-from-scratch (buffer-tree)))
     (check= (tree->stree (get-auxiliary "mine"))
             '(document (tuple "w3") (tuple "w1") (tuple "w2")))
     ;; a section and a label removed
     (tree-remove (buffer-tree) 4 2)
     (update-forced)
     (update-all-kept)
     (check= (labels-state) (labels-state-from-scratch (buffer-tree)))
     (update-all-kept)
     (check= (labels-state) (labels-state-from-scratch (buffer-tree))))))

;; A binding which is the same as in the previous pass: below it, the
;; variable has the value of the previous pass, whatever it had outside;
;; a variable which a with binds and which was not there before is there
;; after it (with no value), and is not there any more when the with goes.
;; After each edit the places of the paragraphs are those of a typesetting
;; from scratch.
(define (same-as-scratch?)
  (let* ((n (tree-arity (buffer-tree)))
         (inc (map (lambda (i) (rect i)) (iota n))))
    (== inc (places-from-scratch (buffer-tree) n))))

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

;; The loci and links in the parts of a surround (the left part of a
;; footnote...) stay registered when its body is edited: the parts are
;; kept, and so is what they declared.
;; The parts of a surround below a paragraph (which records no inputs of its
;; own) read what the paragraph read before them: the inputs of the parts are
;; those they read, also those which the paragraph had read already.
(define (surround-parts-body lookup?)
  (if lookup?
      '(document
        (set-binding "lab" "1cm")
        (assign "sel" "1")
        (with "w" (if (equal (value "sel") "1") (get-binding "lab") "1cm")
              "sel" "0"
          (surround (concat "L" (hspace (get-binding "lab")) "R") ""
                    (document "body")))
        "after")
      '(document
        (assign "y" "1cm")
        (assign "sel" "1")
        (with "w" (if (equal (value "sel") "1") (value "y") "1cm") "sel" "0"
          (surround (concat "L" (hspace (value "y")) "R") ""
                    (document "body")))
        "after")))

(define (surround-parts-x)
  (rect-x1 (rect 2 4 2 0)))

(define (from-scratch measure)
  ;; (@measure) in a new buffer holding a copy of the current document,
  ;; typeset from scratch
  (with body (tree-copy (buffer-tree))
    (with-scratch-buffer
     (lambda (u)
       (buffer-set-body u body)
       (update-current-buffer)
       (update-forced) (update-forced)
       (measure)))))

(define (test-surround-parts-inputs)
  (check-group "inputs of the parts of a surround below a paragraph")
  ;; (also with the classic cache, TEXMACS_TYPESET_READS=off, which did
  ;; not typeset again what read a binding whose value changed, #576)
  (for (lookup? '(#f #t))
    (with-scratch-buffer
     (lambda (u)
       (buffer-set-body u (stree->tree (surround-parts-body lookup?)))
       (update-current-buffer)
       (update-forced) (update-forced)
       (tree-set (tree-ref (buffer-tree) 1 1) "2")
       (update-forced) (update-forced)
       (tree-set (tree-ref (buffer-tree) 0 1) "5cm")
       (update-forced) (update-forced)
       (check= (surround-parts-x) (from-scratch surround-parts-x))
       (check= (typeset-check-mismatches) 0)))))

(define (passes-of thunk)
  ;; the number of typesetting passes which @thunk makes
  (with p0 (typeset-pass-count)
    (thunk)
    (- (typeset-pass-count) p0)))

;; A paragraph which reads a binding is typeset again when an edit gives
;; the binding another value, also with the classic cache
;; (TEXMACS_TYPESET_READS=off), which does it by one complete pass (#576).
;; A key which two bindings write (a label defined twice) is not a reason
;; for it when the value is one which they wrote, nor are the missing
;; references, and an edit which changes no binding makes one pass.
(define (test-paragraph-reads-binding)
  (check-group "a paragraph which reads a binding changed by an edit")
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (stree->tree
                         '(document (set-binding "lab" "1cm")
                                    (concat "L" (hspace (get-binding "lab"))
                                            "R" (reference "nowhere"))
                                    "after")))
     (update-current-buffer)
     (update-forced) (update-forced)
     (with x (lambda () (rect-x1 (rect 1 2)))
       (check-true (<= (passes-of
                        (lambda ()
                          (tree-set (tree-ref (buffer-tree) 0 1) "5cm")
                          (update-forced)))
                       2))
       (update-forced)
       (check= (x) (from-scratch x))
       (check= (passes-of
                (lambda ()
                  (tree-set (tree-ref (buffer-tree) 2) "after x")
                  (update-forced)))
               1))
     (check= (typeset-check-mismatches) 0)))
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (stree->tree
                         '(document (theorem (document (concat "One"
                                                               (label "dup"))))
                                    (theorem (document (concat "Two"
                                                               (label "dup"))))
                                    (concat "see " (reference "dup"))
                                    "after")))
     (update-current-buffer)
     (update-forced) (update-forced)
     (check= (passes-of
              (lambda ()
                (for (v '("Onex" "Onexx" "Onexxx"))
                  (tree-set (tree-ref (buffer-tree) 0 0 0 0) v)
                  (update-forced))))
             3)
     (with x (lambda () (rect-x2 (rect 2)))
       (check= (x) (from-scratch x)))
     (check= (typeset-check-mismatches) 0)))
  ;; the last of two set-binding of one key edited: its new value is not
  ;; one which the two wrote (the values of the complete pass are copies,
  ;; not the strings of the document)
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (stree->tree
                         '(document (set-binding "dup" "a")
                                    (set-binding "dup" "WWWW")
                                    (concat "see " (get-binding "dup") "after")
                                    "filler")))
     (update-current-buffer)
     (update-forced) (update-forced)
     (with x (lambda () (rect-x1 (rect 2 2)))
       (tree-set (tree-ref (buffer-tree) 1 1) "WWWWWWWW")
       (update-forced) (update-forced)
       (check= (x) (from-scratch x))
       (with s (tree-ref (buffer-tree) 1 1)
         (go-to (append (tree->path s) (list (string-length (tree->string s)))))
         (insert "WWWW"))
       (update-forced) (update-forced)
       (check= (x) (from-scratch x)))
     (check= (typeset-check-mismatches) 0)))
  ;; a label defined twice, one definition removed, the other one
  ;; renumbered: its new value is not one which the two wrote
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (stree->tree
                         '(document (concat "see " (reference "dup") "after")
                                    (theorem (document (concat "One"
                                                               (label "dup"))))
                                    (theorem (document (concat "Two"
                                                               (label "dup"))))
                                    "filler")))
     (update-current-buffer)
     (update-forced) (update-forced)
     (tree-remove! (buffer-tree) 2 1)
     (update-forced) (update-forced)
     (tree-insert! (buffer-tree) 1
                   (map (lambda (i) (stree->tree '(theorem (document "P"))))
                        (.. 0 9)))
     (update-forced) (update-forced)
     (with x (lambda () (rect-x1 (rect 0 2)))
       (check= (x) (from-scratch x)))
     (check= (typeset-check-mismatches) 0))))

(define (test-loci-of-surround-parts)
  (check-group "loci of surround parts")
  (with-typeset-body
   '(document "intro"
              (surround (concat (locus (id "sur-locus") "L")
                                (locus (id "sur-link")
                                       (link "hyperlink" (id "sur-link")
                                             (url "#somewhere"))
                                       "H"))
                        ""
                        (document "first paragraph" "second paragraph"))
              "end")
   (lambda ()
     (update-forced)
     (check= (length (id->trees "sur-locus")) 1)
     (check= (length (id->trees "sur-link")) 1)
     (tree-set (tree-ref (buffer-tree) 1 2 0) "first paragraph changed")
     (update-forced)
     (check= (length (id->trees "sur-locus")) 1)
     (check= (length (id->trees "sur-link")) 1)
     (tree-insert (tree-ref (buffer-tree) 1 2) 1 '("inserted"))
     (update-forced)
     (check= (length (id->trees "sur-locus")) 1)
     (check= (length (id->trees "sur-link")) 1))))

;; The bindings which a paragraph or a running header writes (set-binding,
;; label) are written again when it is used again, in the same order: the
;; last one wins, as in a typesetting from scratch. A running header which
;; shows a title typesets its label again, after the body.
(define (references labels)
  (map (lambda (l) (tree->stree (get-reference l))) labels))

(define (references-from-scratch labels)
  (let* ((st (get-style-tree))
         (body (tree-copy (buffer-tree))))
    (with-scratch-buffer
     (lambda (u)
       (set-style-tree st)
       (buffer-set-body u body)
       (init-env "page-medium" "paper")
       (update-current-buffer)
       (update-forced)
       (update-forced)
       (references labels)))))

(define (with-paper-style style thunk)
  (lambda ()
    (set-main-style style)
    (init-env "page-medium" "paper")
    (update-current-buffer)
    (update-forced)
    (update-forced)
    (thunk)
    (update-forced)
    (update-forced)))

(define (test-bindings-written-again)
  (check-group "bindings written again")
  ;; two writers of one key: a theorem inserted before them makes the
  ;; first one again, the second one is used again
  (with-typeset-body
   '(document (theorem (document (concat "Theorem one" (label "x"))))
              "some text"
              (concat "a later binding" (set-binding "x" "LATER"))
              (concat "see " (reference "x")))
   (lambda ()
     ((with-paper-style "article"
        (lambda ()
          (tree-insert (buffer-tree) 0 '((theorem (document "Before")))))))
     (check= (references '("x")) (references-from-scratch '("x")))))
  ;; a section inserted before a labelled section: the running header
  ;; binds the label again, after the index entry
  (with-typeset-body
   '(document (section (concat "Gamma" (label "s3")))
              (concat "see " (reference "s3"))
              (index "Gamma"))
   (lambda ()
     ((with-paper-style "article"
        (lambda ()
          (tree-insert (buffer-tree) 0 '((section "Zero"))))))
     (check= (references '("s3")) (references-from-scratch '("s3")))))
  ;; a chapter typeset again, its running header kept
  (with-typeset-body
   `(document (chapter (concat "Intro" (label "ch")))
              (theorem (document (concat "A theorem" (label "th"))))
              ,@(map (lambda (i)
                       (string-append "A paragraph long enough to fill two "
                                      "lines of the page with words, words "
                                      "and more words, so that the chapter "
                                      "needs a second page."))
                     (iota 30))
              (concat (reference "ch") (reference "th")))
   (lambda ()
     ((with-paper-style "book"
        (lambda ()
          (tree-assign (tree-ref (buffer-tree) 0)
                       (tree-copy (tree-ref (buffer-tree) 0))))))
     (check= (references '("ch" "th"))
             (references-from-scratch '("ch" "th"))))))

;; A tag which is removed and put back by an undo gives its labels the
;; names they had (the undo puts back a copy with their numbers:
;; copy_for_apply in archiver.cpp).
(define (test-labels-after-undo)
  (check-group "labels after undo")
  (with-typeset-body
   '(document (section "A") "a" (section "B") "b" (section "C") "c")
   (lambda ()
     (renumber-auto-labels)
     (update-forced)
     (dynamic-wind start-editing
                   (lambda () (tree-remove (buffer-tree) 2 2))
                   end-editing)
     (update-forced)
     (undo 0)
     (update-forced)
     (check= (auto-names) '("auto-1" "auto-2" "auto-3"))
     ;; and again after a redo and an undo (the history keeps the numbers)
     (redo 0)
     (update-forced)
     (undo 0)
     (update-forced)
     (check= (auto-names) '("auto-1" "auto-2" "auto-3"))
     ;; the same when only the node of the tag is removed (its text is
     ;; kept, as by a backspace at the start of the title)
     (dynamic-wind start-editing
                   (lambda () (tree-remove-node (tree-ref (buffer-tree) 2) 0))
                   end-editing)
     (update-forced)
     (undo 0)
     (update-forced)
     (check= (auto-names) '("auto-1" "auto-2" "auto-3")))))

;; A tag which is inserted, removed by an undo and put back by a redo
;; gives its labels the names it had (the redo puts back the tree which the
;; undo removed, with their numbers: apply_for_undo in archiver.cpp), each
;; time.
(define (test-labels-after-redo)
  (check-group "labels after redo")
  (with-typeset-body
   '(document (section "A") "a" (section "B") "b")
   (lambda ()
     (renumber-auto-labels)
     (update-forced)
     (dynamic-wind start-editing
                   (lambda () (tree-insert (buffer-tree) 2
                                           '((section "D") "d")))
                   end-editing)
     (update-forced)
     (check= (auto-names) '("auto-1" "auto-2" "auto-3"))
     ;; (the labels of a removed tag stay in the list of references, so
     ;; the names are compared after each redo)
     (with names (lambda ()
                   (map (lambda (k)
                          (undo 0) (update-forced)
                          (redo 0) (update-forced)
                          (auto-names))
                        '(1 2 3)))
       (check= (names)
               '(("auto-1" "auto-2" "auto-3")
                 ("auto-1" "auto-2" "auto-3")
                 ("auto-1" "auto-2" "auto-3"))))
     ;; the same for a tag inserted and changed in the same step (the
     ;; undo changes the tag before it removes it)
     (dynamic-wind start-editing
                   (lambda ()
                     (tree-insert (buffer-tree) 4 '((section "E") "e"))
                     (tree-assign (tree-ref (buffer-tree) 4 0) "F"))
                   end-editing)
     (update-forced)
     (let ((n1 (auto-names)))
       (undo 0) (update-forced)
       (redo 0) (update-forced)
       (undo 0) (update-forced)
       (redo 0) (update-forced)
       (check= (tree->stree (tree-ref (buffer-tree) 4)) '(section "F"))
       (check= (auto-names) n1))
     ;; the same for a tag put around a text (insert_node)
     (dynamic-wind start-editing
                   (lambda () (tree-insert-node (tree-ref (buffer-tree) 1)
                                                0 '(section)))
                   end-editing)
     (update-forced)
     (let ((n1 (auto-names)))
       (undo 0) (update-forced)
       (redo 0) (update-forced)
       (undo 0) (update-forced)
       (redo 0) (update-forced)
       (check= (auto-names) n1)))))

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
                  (scr (with-scratch-buffer
                        (lambda (u)
                          (buffer-set-body u body)
                          (update-forced)
                          (update-forced)
                          (equation-references labels)))))
             (when (!= inc scr) (set! bad (+ bad 1)))))
         (check= bad 0))))))

;; A variable assigned below a binding of it, and again afterwards, in
;; the same block of paragraphs: the changes of the block are those of
;; its paragraphs (its value before is the one outside the binding)
(define (test-assignments-in-blocks)
  (check-group "assignments in blocks")
  (for (off '(10 30 50 70))
    (with-typeset-body
     `(document
       ,@(map (lambda (i)
                (cond ((== i 1) '(assign "foo" "1cm"))
                      ((== i off) '(with "foo" "3cm"
                                     (document (assign "foo" "2cm") "inner")))
                      ((== i (+ off 3)) '(assign "foo" "3cm"))
                      ((== i 150) '(concat "a" (vspace (value "foo")) "b"))
                      (else (string-append "paragraph " (number->string i)))))
              (.. 0 160)))
     (lambda ()
       (update-forced)
       (tree-set (tree-ref (buffer-tree) 150)
                 '(concat "c" (vspace (value "foo")) "d"))
       (update-forced)
       (check-true (same-as-scratch?))))))

;; A paragraph whose inputs are not all known (a date: the clock)
;; in a block: when its neighbour is edited, the block is typeset again
;; and the paragraph is used again through the fingerprint; the block must
;; not take its inputs for all of them, or a change of what it reads is
;; not seen.
(define (paper-places body)
  ;; the places of the paragraphs of a typesetting of body from scratch,
  ;; on paper
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (tree-copy body))
     (init-env "page-medium" "paper")
     (update-current-buffer)
     (update-forced)
     (update-forced)
     (map (lambda (i) (rect i)) (iota (tree-arity (buffer-tree)))))))

(define (unknown-inputs-stale? off)
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body
      u (stree->tree
         `(document
           ,@(map (lambda (i)
                    (cond ((== i 1) '(assign "foo" "1cm"))
                          ;; (the year: the same in both typesettings)
                          ((== i off) '(with "zz" "1"
                                         (document (concat (date "yyyy") " "
                                                           (vspace (value "foo"))
                                                           "x"))))
                          (else (string-append "paragraph "
                                               (number->string i)))))
                  (.. 0 120)))))
     (init-env "page-medium" "paper")
     (update-current-buffer)
     (update-forced)
     (update-forced)
     (tree-set (tree-ref (buffer-tree) (+ off 1)) "edited paragraph")
     (update-forced)
     (update-forced)
     (tree-set (tree-ref (buffer-tree) 1) '(assign "foo" "4cm"))
     (update-forced)
     (update-forced)
     (let* ((n (tree-arity (buffer-tree)))
            (inc (map (lambda (i) (rect i)) (iota n))))
       (!= inc (paper-places (buffer-tree)))))))

(define (test-unknown-inputs-in-blocks)
  (check-group "unknown inputs in blocks")
  ;; (where the blocks begin depends on the bridges made before)
  (check= (length (list-filter (.. 15 35) unknown-inputs-stale?)) 0))

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
                  ;; a paragraph removed (also an equation: the references
                  ;; to it are undefined at once, #571)
                  (tree-remove t (+ 2 (rng (- n 2))) 1))
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
             (when (!= inc (places-from-scratch (buffer-tree) m))
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
    ((11) `(concat (date "yyyy") " d"))
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
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (tree-copy body))
     (init-env "page-medium" medium)
     (update-current-buffer)
     (fz-typeset)
     (fz-snapshot))))

(define (fz-edit)
  (let* ((ds (fz-documents (buffer-tree)))
         (d (fz-pick ds))
         (n (tree-arity d))
         ;; (the first two paragraphs of the root are the assignments which
         ;; the edits of the cases 8 and 9 go through)
         (lo (if (== (tree->path d) (tree->path (buffer-tree))) 2 1))
         (pos (if (<= n (+ lo 1)) (min lo n) (+ lo (fz-rnd (- n lo))))))
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
           (tree-insert d (+ lo (fz-rnd (- n lo 1))) (list (stree->tree s))))))
      ((8) ;; the name of the theorems moved
       (tree-set (tree-ref (buffer-tree) 0 1 1 2) (fz-pick '("0pt" "2ex" "4ex"))))
      (else ;; the value of the variable assigned at the start
       (tree-set (tree-ref (buffer-tree) 1 1) (fz-length))))))

(tm-define (typeset-fuzz seed edits size medium)
  (:synopsis "Random edits against scratch; the number of edits which differ")
  (set! fz-state seed)
  (set! fz-labels 0)
  (let* ((old (current-buffer)) (u (new-buffer)) (bad 0))
    (dynamic-wind
      (lambda () (noop))
      (lambda ()
        (switch-to-buffer* u)
        (buffer-set-body u (stree->tree (fz-document size)))
        (init-env "page-medium" medium)
        (update-current-buffer)
        (fz-typeset)
        (for (k (iota edits))
          (fz-edit)
          (fz-typeset)
          (when (!= (fz-snapshot) (fz-scratch (buffer-tree) medium))
            (set! bad (+ bad 1)))))
      (lambda ()
        (buffer-close u)
        (when (buffer-exists? old) (switch-to-buffer old))))
    bad))

(define (test-random-documents)
  (check-group "random documents against scratch")
  ;; (not with the classic cache, TEXMACS_TYPESET_READS=off, whose old
  ;; mistakes these documents find)
  (when (!= (getenv "TEXMACS_TYPESET_READS") "off")
    (check= (typeset-fuzz 17 12 30 "paper") 0)
    (check= (typeset-fuzz 29 12 30 "papyrus") 0)
    (check= (typeset-check-mismatches) 0)))

;; The page breaker sets to 0 the penalty of the last line before a change
;; of the number of columns (breaker_prepare_items): on copies of the
;; lines, not on those which the bridges keep, which the check mode compares
;; with the lines typeset again (here the last line of the columns, which
;; may not be broken from the equation which follows, #573)
(define (test-lines-changed-by-breaker)
  (check-group "lines changed by the page breaker")
  (for (after '((equation (document "x=y"))
                (equation* (document "x=y"))
                (concat "plain")))
    (with-scratch-buffer
     (lambda (u)
       (with m0 (typeset-check-mismatches)
         (buffer-set-body u (stree->tree
                             `(document (with "par-columns" "2"
                                          (document "a" "b" "c"))
                                        ,after)))
         (init-env "page-medium" "paper")
         (update-current-buffer)
         (update-forced) (update-forced)
         (update-forced) (update-forced)
         (tree-insert (buffer-tree) 2 (list (stree->tree "d")))
         (update-forced) (update-forced)
         (check= (typeset-check-mismatches) m0))))))

;; A zoom typesets again only what reads the size of a pixel (px lengths,
;; the borders of ornaments, graphics: edit_env_rep::get_pixel), and uses the
;; rest again (edit_typeset_rep::set_zoom). After each zoom the places of the
;; paragraphs must be those of a typesetting from scratch at that zoom, and
;; the check modes may not find a difference. (apply-changes) does what the
;; event loop does after a zoom of the window.
(define (zoom-body)
  `(document
    ,@(apply append
        (map (lambda (k)
               (let ((lab (string-append "zl" (number->string k))))
                 `((section ,(string-append "Part " (number->string k)))
                   (concat "a space of " (hspace ,(string-append (number->string (+ k 2)) "px")) " pixels")
                   (with "ornament-border" "1px" "ornament-hpadding" "3px"
                     (ornamented (document "ornamented")))
                   (equation (document (concat "x=" ,(number->string k) (label ,lab))))
                   (concat "see " (reference ,lab) " and " (hlink "a link" "https://example.org"))
                   (with "gr-geometry" (tuple "geometry" "3cm" "1cm")
                     (graphics (spline (point "0" "0") (point "1" "1") (point "2" "0"))))
                   (with "par-left" "12px" (document "indented in pixels"))
                   "a paragraph of text which does not depend on the zoom")))
             (iota 6)))))

(define (zoom-to z)
  (set-window-zoom-factor z)
  (apply-changes)
  (update-forced))

(define (zoom-places)
  (map (lambda (i) (rect i)) (iota (tree-arity (buffer-tree)))))

(define (zoom-places-from-scratch body z inits)
  (with-scratch-buffer
   (lambda (u)
     (buffer-set-body u (tree-copy body))
     (init-env "page-medium" "paper")
     (for (i inits) (init-env (car i) (cadr i)))
     (set-window-zoom-factor z)
     (apply-changes)
     (update-current-buffer)
     (update-forced)
     (update-forced)
     (zoom-places))))

(define (zoom-check body inits zooms)
  ;; @body with the initial environment @inits zoomed to each of @zooms
  (with-typeset-body
   body
   (lambda ()
     (init-env "page-medium" "paper")
     (for (i inits) (init-env (car i) (cadr i)))
     (update-current-buffer)
     (update-forced)
     (let ((old (get-window-zoom-factor))
           (mismatches (typeset-check-mismatches)))
       (dynamic-wind
         (lambda () (noop))
         (lambda ()
           ;; (a zoom other than 1, which a window takes)
           (set-window-zoom-factor 1.2)
           (apply-changes)
           ;; (the window rounds the zoom to its pixels: 1.1985)
           (if (> (abs (- (get-window-zoom-factor) 1.2)) 0.01)
               ;; (without a window, nothing is zoomed)
               (display "  (no window: the zoom is not tested)\n")
               (begin
                 ;; (the window took it: the document must have it too)
                 (check-true (< (abs (- (string->number
                                         (get-init "zoom-factor"))
                                        (get-window-zoom-factor)))
                                0.001))
                 (for (z zooms)
                   (zoom-to z)
                   (check= (zoom-places)
                           (zoom-places-from-scratch (buffer-tree) z inits)))
                 (check= (typeset-check-mismatches) mismatches))))
         (lambda ()
           (set-window-zoom-factor old)
           (apply-changes)))))))

;; A length of the derived state in pixels (math-frac-limit: whether a
;; fraction is wide) makes the zoom factor an input of the derived state,
;; which no bridge reads itself: every bridge is then used again only at the
;; same zoom (env_zoom_derived). This one goes last, since what the state
;; was derived from is known for the rest of the session.
(define (zoom-derived-body)
  `(document
    ,@(map (lambda (k)
             `(concat "text " (math (frac ,(apply string-append
                                                  (map (lambda (i) "abcd")
                                                       (iota (+ k 1))))
                                          "b"))
                      " more"))
           (iota 12))))

(define (test-zoom)
  (check-group "zoom")
  (zoom-check (zoom-body) '() '(1.2 1.44 1.2 1.0))
  (zoom-check (zoom-derived-body) '(("math-frac-limit" "100px"))
              '(1.0 2.0 1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Included files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A master which includes files: the included documents come from the
;; cache of the inclusions (load_inclusion, Texmacs/Data/new_buffer.cpp),
;; the same tree at each pass, and their typesetting is kept from one pass
;; to the next (Typeset/Bridge/bridge_rewrite.cpp); what changes around
;; them is seen through the inputs of their paragraphs. After each change,
;; the rectangles of the paragraphs of the master (those of an include are
;; found through the paths of the boxes of the included document) are those
;; of a typesetting of the same master from scratch.
(define incl-dir
  (string-append (url->system (url-temp-dir)) "/typeset-test"))

(define (incl-file name)
  (system->url (string-append incl-dir "/" name)))

(define (incl-clear-dir)
  (with d (system->url incl-dir)
    (when (url-exists? d)
      (for (u (url-read-directory d "*"))
        (system-remove u))
      (system-rmdir d))))

(define (incl-write name body)
  (tree-export (stree->tree `(document (TeXmacs "2.1")
                                       (style (tuple "article"))
                                       (body (document ,@body))))
               (incl-file name) "texmacs"))

(define (incl-update)
  ;; Document > Update > All, without the inclusions-gc
  (update-forced)
  (generate-all-aux)
  (update-current-buffer)
  (update-forced))

(define (incl-places)
  (map (lambda (i) (rect i)) (iota (tree-arity (buffer-tree)))))

(define (incl-height i)
  (with r (rect i) (- (rect-y2 r) (rect-y1 r))))

(define (incl-num l)
  (tree->stree (texmacs-expand `(get-binding ,l))))

(define incl-fresh-count 0)

(define (incl-fresh-places)
  ;; the rectangles of the paragraphs of the current master, typeset from
  ;; scratch in a buffer of its own (a new file each time, so that each
  ;; typesetting starts from a new buffer)
  (set! incl-fresh-count (+ incl-fresh-count 1))
  (let* ((old (current-buffer))
         (name (string-append "fresh-" (number->string incl-fresh-count)
                              ".tm"))
         (u (incl-file name)))
    (incl-write name (cdr (tree->stree (buffer-tree))))
    (dynamic-wind
      (lambda () (noop))
      (lambda ()
        (load-buffer u)
        (switch-to-buffer* u)
        (incl-update)
        (incl-update)
        (incl-places))
      (lambda ()
        (when (buffer-exists? u) (buffer-close u))
        (switch-to-buffer* old)))))

(define (incl-same-as-fresh?)
  (== (incl-places) (incl-fresh-places)))

(define (incl-groups)
  (incl-write "one.tm"
              '((section (concat "One" (label "s1")))
                (concat "a" (foo) "b")
                ;; the number of the section of two.tm, as a height
                (concat "See " (reference "s2")
                        (vspace (merge (get-binding "s2") "cm")))
                "The end of one."))
  (incl-write "two.tm"
              '((section (concat "Two" (label "s2")))
                "Text of two."))
  ;; (without the label of two.tm: the references to it show "?" in the
  ;; master once two.tm is replaced by three.tm, #417)
  (incl-write "three.tm"
              '((section "Three")
                "Text of three."
                (concat "More" (vspace "2cm"))
                "The end of three."))
  (incl-write "master.tm"
              '((assign "foo" (macro (vspace "1cm")))
                (include "one.tm")
                (include "two.tm")
                "The end."))
  (with m (incl-file "master.tm")
    (load-buffer m)
    (switch-to-buffer* m)
    (incl-update)
    (incl-update)
    (check= (incl-num "s1") "1")
    (check= (incl-num "s2") "2")
    (check-true (incl-same-as-fresh?))
    (with t (buffer-tree)
      ;; a macro of the master, used in one.tm, is redefined
      (with h (incl-height 1)
        (tree-set (tree-ref t 0) '(assign "foo" (macro (vspace "3cm"))))
        (update-forced)
        (check-true (> (incl-height 1) h))
        (check-true (incl-same-as-fresh?)))
      ;; a section before the includes: the number of the label of two.tm
      ;; which one.tm references changes
      (with h (incl-height 1)
        (tree-insert t 1 (list (stree->tree '(section "Zero"))))
        (incl-update)
        (check= (incl-num "s1") "2")
        (check= (incl-num "s2") "3")
        (check-true (> (incl-height 2) h))
        (check-true (incl-same-as-fresh?)))
      ;; a paragraph before the includes: the includes are further down
      (with r (rect 2)
        (tree-insert t 1 (list (stree->tree "An inserted paragraph.")))
        (update-forced)
        (check-true (< (rect-y2 (rect 3)) (rect-y2 r)))
        (check-true (incl-same-as-fresh?)))
      ;; two.tm moved before one.tm
      (tree-remove t 4 1)
      (tree-insert t 3 (list (stree->tree '(include "two.tm"))))
      (incl-update)
      (check= (tree->stree (tree-ref t 3)) '(include "two.tm"))
      (check-true (> (rect-y2 (rect 3)) (rect-y2 (rect 4))))
      (check= (incl-num "s2") "2")
      (check= (incl-num "s1") "3")
      (check-true (incl-same-as-fresh?))
      ;; and back
      (tree-remove t 3 1)
      (tree-insert t 4 (list (stree->tree '(include "two.tm"))))
      (incl-update)
      (check= (incl-num "s1") "2")
      (check= (incl-num "s2") "3")
      (check-true (incl-same-as-fresh?))
      ;; the name of an included file changed, and back
      (with h (incl-height 4)
        (tree-set (tree-ref t 4 0) "three.tm")
        (incl-update)
        (check= (incl-num "s2") '(uninit))
        (check= (tree->stree (get-reference "s2")) '(uninit))
        (check-true (> (incl-height 4) h))
        (check-true (incl-same-as-fresh?))
        (tree-set (tree-ref t 4 0) "two.tm")
        (incl-update)
        (check= (incl-num "s2") "3")
        (check= (incl-height 4) h)
        (check-true (incl-same-as-fresh?))))
    ;; two.tm edited in a buffer of its own and saved: the master shows
    ;; it after Update > All (which empties the cache of the inclusions)
    (with h (incl-height 4)
      (with u (incl-file "two.tm")
        (load-buffer u)
        (switch-to-buffer* u)
        (update-forced)
        ;; (as an action of the user: the buffer is modified)
        (archive-state)
        (start-editing)
        (tree-insert (buffer-tree) 2
                     (list (stree->tree '(concat "New" (vspace "2cm")))))
        (end-editing)
        (check-true (buffer-modified? u))
        (save-buffer)
        (check-false (buffer-modified? u))
        (switch-to-buffer* m)
        (buffer-close u))
      (inclusions-gc)
      (incl-update)
      (check= (tree->stree (tree-load-inclusion (incl-file "two.tm")))
              '(document (section (concat "Two" (label "s2")))
                         "Text of two."
                         (concat "New" (vspace "2cm"))))
      (check-true (> (incl-height 4) h))
      (check-true (incl-same-as-fresh?)))
    (buffer-pretend-saved m)
    (buffer-close m)))

(define (incl-extern)
  ;; another rewriting, which gives a new tree at each pass: its body is
  ;; made again when what it reads changes
  (with-typeset-body
   '(document (assign "len" "1cm")
              (extern "(lambda (x) `(concat \"a\" (vspace ,(tree->string x))))"
                      (value "len"))
              "After.")
   (lambda ()
     (with r (rect 2)
       (tree-set (tree-ref (buffer-tree) 0 1) "3cm")
       (update-forced)
       (check-true (< (rect-y2 (rect 2)) (rect-y2 r)))
       (check= (map (lambda (i) (rect i)) (iota 3))
               (places-from-scratch (buffer-tree) 3))))))

(define (incl-labels-fresh)
  ;; the labels, tables and places of the current master, updated in a
  ;; buffer of its own
  (set! incl-fresh-count (+ incl-fresh-count 1))
  (let* ((old (current-buffer))
         (name (string-append "fresh-" (number->string incl-fresh-count)
                              ".tm"))
         (u (incl-file name)))
    (incl-write name (cdr (tree->stree (buffer-tree))))
    (dynamic-wind
      (lambda () (noop))
      (lambda ()
        (load-buffer u)
        (switch-to-buffer* u)
        (update-forced)
        (update-all-labels)
        (update-all-labels)
        (labels-state))
      (lambda ()
        (when (buffer-exists? u) (buffer-close u))
        (switch-to-buffer* old)))))

(define (incl-renumbering)
  ;; the labels of the sections and of the index entries of the included
  ;; files, numbered again in a master (#408): only the bridges which make
  ;; labels are typeset again, and the includes are used again
  (incl-write "four.tm"
              '((section "Four")
                (concat "a" (index "four"))
                (section "Four again")))
  (incl-write "five.tm"
              '((section "Five")
                (concat "b" (index "five"))))
  (incl-write "labels.tm"
              '((table-of-contents "toc" (document ""))
                (include "four.tm")
                (section "Between")
                (include "five.tm")
                (the-index "idx" (document ""))))
  (with m (incl-file "labels.tm")
    (load-buffer m)
    (switch-to-buffer* m)
    (update-forced)
    (update-all-labels)
    (update-all-labels)
    (with names (auto-names)
      (check= names (auto-range (length names)))
      (check-true (>= (length names) 7)))
    (check= (labels-state) (incl-labels-fresh))
    ;; sections of the master before and between the includes
    (tree-insert (buffer-tree) 1 '((section "Zero")))
    (update-forced)
    (tree-insert (buffer-tree) 4 '((concat "z" (index "zero"))))
    (update-forced)
    (check-true (renumber-auto-labels))
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (with names (auto-names)
      (check= names (auto-range (length names))))
    (check= (labels-state) (incl-labels-fresh))
    ;; Update All with the layout kept, after a section was added to the
    ;; master before the includes
    (tree-insert (buffer-tree) 1 '((section "One")))
    (update-forced)
    (update-all-kept)
    (update-all-kept)
    (check= (labels-state) (incl-labels-fresh))
    (buffer-pretend-saved m)
    (buffer-close m)))

(define incl-bib
  (string-append
   "@article{knuth, author={Knuth, Donald E.}, title={The Art}, journal={J}, year=1968}\n"
   "@book{lamport, author={Lamport, Leslie}, title={LaTeX}, publisher={AW}, year=1986}\n"
   "@misc{doe, author={Doe, J.}, title={Misc}, year=2000}\n"))

(define (incl-table tag)
  ;; the table @tag made in the document, as a string
  (with l (list-filter (labels-tables) (lambda (x) (== (car x) tag)))
    (if (null? l) "" (object->string (car l)))))

(define (incl-state)
  ;; the labels, tables and places of the current master, and those of a
  ;; typesetting from scratch (the cursor at the start: a cursor in the
  ;; bibliography shows its file in an input field of the focus toolbar,
  ;; whose completer crashes TeXmacs when it exits without a window)
  (go-start)
  (list (labels-state) (incl-labels-fresh)))

(define (incl-renumbering-writes)
  ;; the entries of the tables written by paragraphs which make no label (a
  ;; citation, a nocite, a write of the user): when the labels are numbered
  ;; again, these paragraphs are used again and write their entries again
  ;; (the <write|key|value> of their bindings, edit_env_rep::write_bindings
  ;; in Typeset/env.hpp), so that the tables made from the renumbering pass
  ;; hold them (#408)
  (string-save incl-bib (incl-file "refs.bib"))
  (incl-write "writes.tm"
              '((table-of-contents "toc" (document ""))
                (assign "myw" (macro "x" (write "mine" (tuple (arg "x")))))
                (section "A")
                (concat "a" (index "first"))
                (concat "x" (cite "knuth"))
                (concat "y" (nocite "lamport"))
                (concat "w" (myw "w1"))
                (section "B")
                (concat "z" (cite "doe") (myw "w2"))
                (the-index "idx" (document ""))
                (bibliography "bib" "tm-plain" "refs" (document ""))))
  (with m (incl-file "writes.tm")
    (load-buffer m)
    (switch-to-buffer* m)
    (update-forced)
    (update-all-labels)
    (update-all-labels)
    (with st (incl-state) (check= (car st) (cadr st)))
    ;; a section and an index entry before the others: the labels are
    ;; numbered again, the paragraphs which write without a label are used
    ;; again
    (tree-insert (buffer-tree) 2 '((section "Zero")))
    (update-forced)
    (tree-insert (buffer-tree) 4 '((concat "n" (index "new"))))
    (update-forced)
    (check-true (renumber-auto-labels))
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (with names (auto-names)
      (check= names (auto-range (length names))))
    ;; (the typesetting from scratch is updated as well, its bridges used
    ;; again: the entries are checked by themselves)
    (with bib (incl-table 'bibliography)
      (check-true (string-occurs? "The art" bib))
      (check-true (string-occurs? "LaTeX" bib))
      (check-true (string-occurs? "Misc" bib)))
    (check= (tree->stree (get-auxiliary "mine"))
            '(document (tuple "w1") (tuple "w2")))
    (with st (incl-state) (check= (car st) (cadr st)))
    (buffer-pretend-saved m)
    (buffer-close m)))

(define (incl-cite-first)
  ;; citations before a bibliography which the first Update All generates:
  ;; the pass which uses the bridges again must give them their numbers
  ;; (not "?") although the bridges of the citations are used again and
  ;; look nothing up (#408)
  (string-save incl-bib (incl-file "refs.bib"))
  (incl-write "citefirst.tm"
              '((section "S")
                (concat "Cite " (cite "knuth") " and " (cite "lamport") ".")
                (concat "Forward " (reference "later") ".")
                (bibliography "bib" "tm-plain" "refs" (document ""))))
  (with m (incl-file "citefirst.tm")
    (load-buffer m)
    (switch-to-buffer* m)
    (update-forced)
    (update-all-kept)
    (go-start)
    (with st (incl-state) (check= (car st) (cadr st)))
    (check= (cadr (tree->stree (get-reference "bib-knuth"))) "1")
    (buffer-pretend-saved m)
    (buffer-close m)))

(define (incl-ifref)
  ;; a lookup of a missing label under warn-missing false (tmdoc's if-ref)
  ;; is not told as missing: Update All makes its two passes, no more, also
  ;; when the bridge is used again (#408)
  (incl-write "ifref.tm"
              '((section "S")
                (concat "Test: "
                        (with "warn-missing" "false"
                              (if (unequal (get-binding "nolab") (uninit))
                                  "yes" "no"))
                        " end.")))
  (with m (incl-file "ifref.tm")
    (load-buffer m)
    (switch-to-buffer* m)
    (update-forced)
    (update-all-kept)
    (go-start)
    (with p0 (typeset-pass-count)
      (update-all-kept)
      (update-all-kept)
      (check= (- (typeset-pass-count) p0)
              4))
    (buffer-pretend-saved m)
    (buffer-close m)))

(define (incl-saved-include)
  ;; an included file changed and saved from its own buffer, then one
  ;; Update All of the master: saving resets the inclusion (reset_inclusion
  ;; in Texmacs/Data/new_buffer.cpp), and the include is typeset again
  ;; although nothing in the master changed and the old content of the file
  ;; made no label (the included document is an input of the bridge of the
  ;; include: rewrite_include in Typeset/Env/env_exec.cpp), so that its
  ;; section, index entry and citation are in the tables (#408)
  (string-save incl-bib (incl-file "refs.bib"))
  (incl-write "plain.tm" '((concat "plain text " (cite "knuth"))))
  (incl-write "saved.tm"
              '((table-of-contents "toc" (document ""))
                (section "First")
                (include "plain.tm")
                (the-index "idx" (document ""))
                (bibliography "bib" "tm-plain" "refs" (document ""))))
  (let ((m (incl-file "saved.tm"))
        (p (incl-file "plain.tm")))
    (load-buffer m)
    (switch-to-buffer* m)
    (update-forced)
    (update-all-labels)
    (update-all-labels)
    (with st (incl-state) (check= (car st) (cadr st)))
    (load-buffer p)
    (switch-to-buffer* p)
    (update-forced)
    (with t (buffer-tree)
      (tree-insert t (tree-arity t)
                   '((section "Added")
                     (concat "new " (index "added") (cite "lamport")))))
    (update-forced)
    (buffer-save p)
    (switch-to-buffer* m)
    (update-forced)
    (update-all-labels)
    (check-true (string-occurs? "Added" (incl-table 'table-of-contents)))
    (check-true (string-occurs? "added" (incl-table 'the-index)))
    (check-true (string-occurs? "LaTeX" (incl-table 'bibliography)))
    (with names (auto-names)
      (check= names (auto-range (length names))))
    (with st (incl-state) (check= (car st) (cadr st)))
    (buffer-close p)
    (buffer-pretend-saved m)
    (buffer-close m)))

;; The clean-up steps of Update All keep the layout (#425): when no included
;; file and no picture changed, the renumbering pass after inclusions-gc and
;; picture-gc uses the bridges again; when an included file changed (on
;; disk, or saved from its own buffer), only its include is typeset again;
;; the result is the one of a typesetting from scratch.
(define gc-changed 0)
(define (incl-gc-keeps)
  (incl-write "gc-a.tm"
              (append '((section "Alpha") (concat "a" (index "alpha")))
                      (map (lambda (i) (string-append "Text of alpha " (number->string i)))
                           (iota 40))))
  (incl-write "gc-b.tm"
              (append '((section "Beta") (concat "b" (index "beta")))
                      (map (lambda (i) (string-append "Text of beta " (number->string i)))
                           (iota 40))))
  (incl-write "gc-m.tm"
              (append '((table-of-contents "toc" (document ""))
                        (section "Intro"))
                      (map (lambda (i) (string-append "Master text " (number->string i)))
                           (iota 40))
                      '((include "gc-a.tm") (section "Middle") (include "gc-b.tm")
                        (the-index "idx" (document "")))))
  (with m (incl-file "gc-m.tm")
    (load-buffer m)
    (switch-to-buffer* m)
    (let* ((c0 (typeset-redone-count))
           (dummy (update-forced))
           (all (- (typeset-redone-count) c0)))
      (update-all-kept)
      (update-all-kept)
      (with c1 (typeset-redone-count)
        (update-all-kept)
        (with n (- (typeset-redone-count) c1)
          (check= (labels-state) (incl-labels-fresh))
          ;; (the classic cache types it all again, and the check mode too)
          (when (not (member (getenv "TEXMACS_TYPESET_READS") (list "off" "check")))
            (check-true (< (* 2 n) all)))))
      ;; an included file edited on disk: the master shows it, and what is
      ;; typeset again is that include and what follows from the renumbering
      (incl-write "gc-b.tm"
                  (append '((section "Beta") (concat "b" (index "beta"))
                            (section "Gamma") (concat "g" (index "gamma")))
                          (map (lambda (i) (string-append "Text of beta " (number->string i)))
                               (iota 40))))
      (with c2 (typeset-redone-count)
        (update-all-kept)
        (set! gc-changed (- (typeset-redone-count) c2))
        (update-all-kept)
        (check-true (string-occurs? "Gamma" (incl-table 'table-of-contents)))
        (check-true (string-occurs? "gamma" (incl-table 'the-index)))
        (check= (labels-state) (incl-labels-fresh))
        (when (not (member (getenv "TEXMACS_TYPESET_READS") (list "off" "check")))
          (check-true (< gc-changed all))))
      ;; and back
      (incl-write "gc-b.tm"
                  (append '((section "Beta") (concat "b" (index "beta")))
                          (map (lambda (i) (string-append "Text of beta " (number->string i)))
                               (iota 40))))
      (update-all-kept)
      (update-all-kept)
      (check-false (string-occurs? "Gamma" (incl-table 'table-of-contents)))
      (check= (labels-state) (incl-labels-fresh)))
    (buffer-pretend-saved m)
    (buffer-close m)))

;; More of Update All with the layout kept (#425): a nested include edited
;; on disk, a missing include which appears, a picture whose size changes
;; on disk before picture-gc.
(define (incl-gc-eps name w h)
  (string-save
   (string-append "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 0 0 "
                  (number->string w) " " (number->string h)
                  "\n0 0 moveto " (number->string w) " 0 lineto "
                  (number->string w) " " (number->string h)
                  " lineto 0 " (number->string h)
                  " lineto closepath fill\nshowpage\n")
   (incl-file name)))

(define (incl-gc-more)
  (incl-gc-eps "gc-pic.eps" 100 50)
  (incl-write "gc-n2.tm" '((section "Inner") (concat "i" (index "inner"))))
  (incl-write "gc-n1.tm" '((section "Outer") (include "gc-n2.tm")))
  (incl-write "gc-m2.tm"
              '((table-of-contents "toc" (document ""))
                (section "Top")
                (include "gc-n1.tm")
                (include "gc-late.tm")
                (concat "pic " (image "gc-pic.eps" "" "" "" ""))
                "End."
                (the-index "idx" (document ""))))
  (with m (incl-file "gc-m2.tm")
    (load-buffer m)
    (switch-to-buffer* m)
    (update-forced)
    (update-all-kept)
    (update-all-kept)
    (check= (labels-state) (incl-labels-fresh))
    ;; the nested include edited on disk
    (incl-write "gc-n2.tm" '((section "Inner") (concat "i" (index "inner"))
                             (section "Deeper") (concat "d" (index "deeper"))))
    (update-all-kept)
    (check-true (string-occurs? "Deeper" (incl-table 'table-of-contents)))
    (check-true (string-occurs? "deeper" (incl-table 'the-index)))
    (check= (labels-state) (incl-labels-fresh))
    ;; the missing include appears
    (check-false (string-occurs? "Late" (incl-table 'table-of-contents)))
    (incl-write "gc-late.tm" '((section "Late") (concat "l" (index "late"))))
    (update-all-kept)
    (check-true (string-occurs? "Late" (incl-table 'table-of-contents)))
    (check-true (string-occurs? "late" (incl-table 'the-index)))
    (check= (labels-state) (incl-labels-fresh))
    ;; the picture gets bigger on disk, then picture-gc
    (update-forced)
    (let* ((n (tree-arity (buffer-tree)))
           (r (rect (- n 3))))
      (incl-gc-eps "gc-pic.eps" 200 150)
      (picture-gc)
      (update-forced)
      (update-all-kept)
      (check-true (> (- (rect-y2 (rect (- n 3))) (rect-y1 (rect (- n 3))))
                     (- (rect-y2 r) (rect-y1 r))))
      ;; (and nothing changes when the file does not)
      (with r2 (rect (- n 3))
        (picture-gc)
        (update-forced)
        (check= (rect (- n 3)) r2)))
    (buffer-pretend-saved m)
    (buffer-close m)))

;; The files which Update All reads again are skipped when they were not
;; touched (#425): a file replaced with its old time and size is still read.
(define (incl-sh . l)
  (system (apply string-append l)))

(define (incl-gc-times)
  (incl-write "gc-t.tm" '((section "Alpha") (concat "a" (index "alpha"))))
  (incl-write "gc-tm.tm"
              '((table-of-contents "toc" (document ""))
                (section "Top")
                (include "gc-t.tm")
                (the-index "idx" (document ""))))
  (let ((t (url->system (incl-file "gc-t.tm")))
        (r (url->system (incl-file "gc-t.ref"))))
    (when (not (os-win32?))
      ;; the file is old, and the master reads it
      (incl-sh "touch -t 200001010000 '" t "'")
      (with m (incl-file "gc-tm.tm")
        (load-buffer m)
        (switch-to-buffer* m)
        (update-forced)
        (update-all-kept)
        (check-true (string-occurs? "Alpha" (incl-table 'table-of-contents)))
        ;; nothing changed: the file is not read again, nothing is typeset
        (let ((c (typeset-redone-count)))
          (inclusions-gc)
          (update-forced)
          (when (not (member (getenv "TEXMACS_TYPESET_READS")
                             (list "off" "check")))
            (check= (typeset-redone-count) c)))
        ;; replaced by a file of the same size and the same time
        (incl-sh "cp -p '" t "' '" r "'")
        (incl-write "gc-t.tm" '((section "Omega") (concat "a" (index "alpha"))))
        (incl-sh "touch -r '" r "' '" t "'")
        (update-all-kept)
        (check-true (string-occurs? "Omega" (incl-table 'table-of-contents)))
        (check-false (string-occurs? "Alpha" (incl-table 'table-of-contents)))
        (check= (labels-state) (incl-labels-fresh))
        (buffer-pretend-saved m)
        (buffer-close m)))))

;; A chapter opened through its master (Project menu: the master loaded, then
;; the chapter, which names it as its project), and a theorem inserted and
;; removed at its start: the numbers of the chapter change, those of the
;; master are seen, and what is typeset again is the chapter, as when it is
;; loaded alone, not the book (#408).
(define (incl-book name project body)
  (tree-export (stree->tree `(document (TeXmacs "2.1")
                                       ,@(if project `((project ,project)) '())
                                       (style (tuple "book"))
                                       (body (document ,@body))))
               (incl-file name) "texmacs"))

(define (incl-chapter-edit)
  ;; a theorem inserted after the title of the current chapter, then
  ;; removed: the pieces typeset again by each, and the numbers of t1
  (let* ((r0 (typeset-redone-count))
         (dummy (begin (tree-insert! (buffer-tree) 1
                                     (list (stree->tree '(theorem (document "New.")))))
                       (update-forced)))
         (ins (- (typeset-redone-count) r0))
         (num (incl-num "t1"))
         (r1 (typeset-redone-count)))
    (tree-remove! (buffer-tree) 1 1)
    (update-forced)
    (list ins (- (typeset-redone-count) r1) num (incl-num "t1"))))

(define (incl-through-master)
  (incl-book "pc1.tm" "pm.tm"
             (append '((chapter "One")
                       (theorem (document (concat (label "t1") "First.")))
                       (concat "See " (reference "t5") "."))
                     (map (lambda (i) (string-append "Text of one " (number->string i)))
                          (iota 10))))
  (incl-book "pc2.tm" "pm.tm"
             (cons '(chapter "Two")
                   (apply append
                          (map (lambda (i)
                                 (with l (string-append "t" (number->string (+ i 2)))
                                   (list `(theorem (document (concat (label ,l) "Claim.")))
                                         '(concat "Back to " (reference "t1") ".")
                                         (string-append "Text of two " (number->string i)))))
                               (iota 30)))))
  (incl-book "pm.tm" #f '((include "pc1.tm") (include "pc2.tm")))
  (let ((m (incl-file "pm.tm"))
        (c (incl-file "pc1.tm"))
        (full 0))
    (with r0 (typeset-redone-count)
      (load-buffer m)
      (switch-to-buffer* m)
      (update-forced)
      (set! full (- (typeset-redone-count) r0)))
    (update-all-kept)
    (update-all-kept)
    (check= (incl-num "t5") "2.4")
    (load-buffer c)
    (switch-to-buffer* c)
    (update-forced)
    (update-forced)
    ;; the numbers of the master, in the chapter
    (check= (incl-num "t5") "2.4")
    (check= (incl-num "t1") "1.1")
    (with via (incl-chapter-edit)
      (check= (cddr via) '("1.2" "1.1"))
      (buffer-pretend-saved c)
      (buffer-close c)
      (buffer-pretend-saved m)
      (buffer-close m)
      ;; the chapter loaded alone
      (load-buffer c)
      (switch-to-buffer* c)
      (update-forced)
      (update-forced)
      (with direct (incl-chapter-edit)
        (check= (cddr direct) '("1.2" "1.1"))
        ;; (the second chapter is ten times as big as the first; the
        ;; classic cache counts no pieces typeset again)
        (when (!= (getenv "TEXMACS_TYPESET_READS") "off")
          (check-true (< (* 4 (car via)) full))
          (check-true (< (* 4 (cadr via)) full))
          (check-true (<= (car via) (+ (car direct) 10)))
          (check-true (<= (cadr via) (+ (cadr direct) 10)))))
      (buffer-pretend-saved c)
      (buffer-close c)
      ;; the chapter loaded alone has its master too, as a hidden buffer
      (when (buffer-exists? m)
        (buffer-pretend-saved m)
        (buffer-close m)))))

(define (test-included-files)
  (check-group "included files")
  (let ((old (current-buffer))
        (before (buffer-list))
        (mismatches (typeset-check-mismatches)))
    (incl-clear-dir)
    (system-mkdir (system->url incl-dir))
    (incl-extern)
    (for (f (list incl-renumbering incl-renumbering-writes
                  incl-cite-first incl-ifref incl-saved-include incl-gc-keeps
                  incl-gc-more incl-gc-times incl-through-master))
      (with r (check-run f)
        (when (and (pair? r) (== (car r) 'error))
          (check-report #f "the group" (object->string r)))))
    (with r (check-run incl-groups)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before)
          (buffer-pretend-saved b)
          (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer* old))
      (incl-clear-dir)
      ;; (in the check modes, the bridges used again made what a
      ;; typesetting again makes)
      (check= (typeset-check-mismatches) mismatches))))

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
  (test-removed-label)
  (test-removed-label-chain)
  (test-removed-label-by-edit)
  (test-stale-references)
  (test-automatic-labels)
  (test-labels-of-nested-tags)
  (test-reuse-below-bindings)
  (test-page-breaks-kept)
  (test-tables)
  (test-paragraphs)
  (test-markers)
  (test-pages)
  (test-pages-kept)
  (test-long-document-without-pages)
  (test-typesetting-failure)
  (test-hidden-documents)
  (test-derived-state)
  (test-renumbering-labels)
  (test-label-names)
  (test-renumbering-against-scratch)
  (test-update-all-kept)
  (test-bindings-against-scratch)
  (test-labels-of-parts)
  (test-loci-of-surround-parts)
  (test-surround-parts-inputs)
  (test-paragraph-reads-binding)
  (test-bindings-written-again)
  (test-labels-after-undo)
  (test-labels-after-redo)
  (test-incremental-against-scratch)
  (test-random-documents)
  (test-lines-changed-by-breaker)
  (test-block-replay)
  (test-references-after-removals)
  (test-assignments-in-blocks)
  (test-unknown-inputs-in-blocks)
  (test-zoom)
  (test-included-files)
  (check-end))
