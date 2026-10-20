
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : style-rewrite-test.scm
;; DESCRIPTION : tests of the experimental style rewriter
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The style rewriter (src/src/Style, see src/docs/pure-style.md) exists only
;; in builds configured with --enable-experimental, so this suite is not
;; listed in check-master.scm; run it with
;;
;;   tests/scheme/check.sh TeXmacs/progs/check/style-rewrite-test.scm
;;
;; In other builds it reports that the rewriter is missing and fails.
;;
;; The tests cover
;;
;;   - macro expansion and the primitives ported to the rewriter, on small
;;     documents whose rewritten form is known;
;;   - the memorization: the numbers of evaluations after loading, after an
;;     edit and without a change (style-rewrite-statistics);
;;   - pages of the manual: the evaluation errors left in the result are
;;     compared with the ones known, so that a new error and a fixed one both
;;     show up (update the lists below when the rewriter improves);
;;   - the whole documentation: no page may leave an error in its result;
;;   - the caches of the typesetter, which the rewriter must not disturb.

(texmacs-module (check style-rewrite-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-scratch-body body thunk)
  ;; (thunk) in a new buffer with the given body, closed afterwards
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (dynamic-wind
      (lambda () (noop))
      (lambda ()
        (switch-to-buffer* u)
        (buffer-set-body u (stree->tree body))
        (thunk))
      (lambda ()
        (buffer-close u)
        (when (buffer-exists? old) (switch-to-buffer* old))))))

(define (rewrite body)
  ;; the rewritten body as a scheme tree
  (with-scratch-body body (lambda () (tree->stree (style-rewrite-buffer)))))

(define (unmark t)
  ;; t without the expand-as which keep the boundaries of the macro calls
  ;; of the document (see test-boundaries)
  (cond ((and (pair? t) (== (car t) 'expand-as) (= (length t) 3))
         (unmark (caddr t)))
        ((pair? t) (cons (car t) (map unmark (cdr t))))
        (else t)))

(define (rewrite-last body)
  ;; the rewriting of the last paragraph of a document (without the
  ;; boundaries of the macro calls)
  (with r (rewrite body)
    (and (pair? r) (unmark (cAr r)))))

(define (stree-errors t)
  ;; the messages of the error markup in a scheme tree, sorted, each once
  (define (collect t acc)
    (cond ((and (pair? t) (== (car t) 'error) (pair? (cdr t))
                (string? (cadr t)))
           (cons (cadr t) acc))
          ((pair? t)
           (let loop ((l (cdr t)) (acc acc))
             (if (pair? l) (loop (cdr l) (collect (car l) acc)) acc)))
          (else acc)))
  (sort (list-remove-duplicates (collect t '())) string<=?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A macro call becomes its body with the arguments substituted (the
;; alternative expansion); the assignment itself disappears; with stays.
(define (test-macros)
  (check-group "macros")
  (check= (rewrite-last '(document (assign "foo" (macro "x" (concat "[" (arg "x") "]")))
                                   (foo "world")))
          '(concat "[" "world" "]"))
  (check= (rewrite-last '(document (assign "twice" (macro "x" (concat (arg "x") (arg "x"))))
                                   (twice "ab")))
          '(concat "ab" "ab"))
  (check= (rewrite-last '(document (assign "v" "3") (value "v")))
          "3")
  (check= (rewrite-last '(document (with "font-series" "bold" "b")))
          '(with "font-series" "bold" "b"))
  ;; the argument of a macro used inside another macro
  (check= (rewrite-last '(document (assign "in" (macro "y" (concat "<" (arg "y") ">")))
                                   (assign "out" (macro "x" (in (arg "x"))))
                                   (out "z")))
          '(concat "<" "z" ">")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (value-of expr)
  (rewrite-last `(document ,expr)))

(define (test-primitives)
  (check-group "primitives")
  (check= (value-of '(plus "1" "2")) "3")
  (check= (value-of '(minimum "3" "4" "2")) "2")
  (check= (value-of '(maximum "3" "4" "2")) "4")
  (check= (value-of '(mod (minus "5" "1") (minimum "3" "4"))) "1")
  ;; or and and as in the typesetter: values, not only booleans
  (check= (value-of '(or "false" "x" "y")) "x")
  (check= (value-of '(or "false" "false")) "false")
  (check= (value-of '(and "true" "y")) "y")
  (check= (value-of '(and "false" "y")) "false")
  (check= (value-of '(rgb-access "red" "r")) "255")
  (check= (value-of '(rgb-color "255" "0" "0")) "#FF0000")
  (check= (value-of '(equal (merge "a" "b") "ab")) "true")
  ;; the lengths of resize may use the extents of the box: left as they are
  (check= (value-of '(resize "x" "" "" (minus "1r" "1pt") ""))
          '(resize "x" "" "" (minus "1r" "1pt") ""))
  ;; occurs-inside sees the argument as given
  (check= (rewrite-last '(document (assign "f" (macro "x" (occurs-inside "a" (arg "x"))))
                                   (f (concat "b" "a"))))
          "true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partial evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What only the typesetter knows (box-info, lengths in units of a box) is a
;; residual; a computation on a residual stays as it is, with its arguments
;; evaluated, so that it holds no value, arg or macro of the document.
(define (test-partial-evaluation)
  (check-group "partial evaluation")
  (check= (value-of '(plus "1r" "1pt")) '(plus "1r" "1pt"))
  (check= (value-of '(plus "2" "3")) "5")
  (check= (rewrite-last '(document (assign "v" "2pt")
                                   (resize "x" "" "" (minus "1r" (value "v")) "")))
          '(resize "x" "" "" (minus "1r" "2pt") ""))
  ;; the argument of a macro inside a residual is substituted
  (check= (rewrite-last '(document (assign "f" (macro "x" (move "y" (plus "1w" (arg "x")) "")))
                                   (f "3pt")))
          '(move "y" (plus "1w" "3pt") ""))
  (check= (value-of '(look-up (box-info "x" "wh") "1"))
          '(look-up (box-info "x" "wh") "1"))
  ;; the graphics of the conventions page: no error computed from box-info,
  ;; and the values which hold a residual are referred to, not copied (the
  ;; typesetter computes box-info where dims is bound)
  (with r (rewrite-last
           '(document
             (with "dims" (box-info "x" "wh")
               (with "ww" (look-up (value "dims") "0")
                     "hh" (look-up (value "dims") "1")
                 (tuple (plus (value "ww") "1pt")
                        (times (value "hh") "2"))))))
    (check= (stree-errors r) '())
    (check= (cAr (cAr r))
            '(tuple (plus (value "ww") "1pt") (times (value "hh") "2")))
    (check= (list-head r 3) '(with "dims" (box-info "x" "wh"))))
  ;; an assignment stays (the typesetter reads variables of its own, and
  ;; the assignment keeps its place for the cursor), with a value which is
  ;; not evaluated again
  (check= (rewrite '(document (assign "page-odd-header" (concat "a" (quote (page-the-page))))
                              (assign "m" (macro "x"))
                              "b"))
          '(document (assign "page-odd-header" (quote (concat "a" (page-the-page))))
                     (assign "m" (quote (macro "x")))
                     "b"))
  ;; the label tables are filled by the typesetter: set-binding stays,
  ;; get-binding is a residual
  (check= (value-of '(set-binding "lab" "3")) '(set-binding "lab" "3"))
  (check= (value-of '(get-binding "lab")) '(get-binding "lab"))
  ;; inactive markup is shown by the typesetter: the tags stay, the
  ;; source unevaluated
  (check= (value-of '(inactive* (assign "x" (plus "1" "2"))))
          '(inactive* (assign "x" (plus "1" "2"))))
  ;; if* is a phantom: typeset invisibly, it keeps its body
  (check= (value-of '(if* "false" "body")) '(if* "false" "body"))
  ;; conditionals on a residual keep their branches, evaluated
  (check= (rewrite-last '(document (assign "a" "A")
                                   (if (less "1r" "2pt") (value "a") "b")))
          '(if (less "1r" "2pt") "A" "b"))
  (check= (value-of '(if (less "1" "2") "yes" "no")) "yes")
  ;; lengths in units are converted by the typesetter, where they are used
  (check= (value-of '(if (less "1pt" "2pt") "yes" "no"))
          '(if (less "1pt" "2pt") "yes" "no"))
  (check= (rewrite-last '(document (assign "h" (macro "3fn"))
                                   (plus "0cm" (h))))
          '(plus "0cm" "3fn"))
  (check= (value-of '(fn-length)) '(fn-length))
  (check= (value-of '(or "false" (equal "1h" "0pt") "z"))
          '(or (equal "1h" "0pt") "z"))
  ;; the automatic labels are numbered by the typesetter (the-auto)
  (check= (value-of '(merge "auto" "-" (auto-id "last")))
          '(merge "auto" "-" (auto-id "last")))
  ;; a box length is only a residual in a computation on lengths
  (check= (value-of '(merge "1" "r")) "1r")
  (check= (value-of '(length "1r")) "2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boundaries of macro calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The expansion of a macro call of the document is kept in an expand-as,
;; whose boundaries the typesetter marks (the cursor goes before and after
;; the tag); not in a value, on which the rewriter computes.
(define (test-boundaries)
  (check-group "boundaries")
  (check= (cAr (rewrite '(document (assign "foo" (macro "x" (concat "[" (arg "x") "]")))
                                   (foo "world"))))
          '(expand-as "foo" (concat "[" "world" "]")))
  (check= (cAr (rewrite '(document (assign "two" (macro "2"))
                                   (plus (two) "1"))))
          "3")
  ;; (a call inside a macro body is not in the document)
  (check= (cAr (rewrite '(document (assign "in" (macro "y" (concat "<" (arg "y") ">")))
                                   (assign "out" (macro "x" (in (arg "x"))))
                                   (out "z"))))
          '(expand-as "out" (concat "<" "z" ">"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup and values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The typesetter shows quote and quote-value in the markup (as their
;; source), while exec gives the quoted tree: the rewriter evaluates the
;; markup and the values apart (evaluate_markup, evaluate_result).
(define (test-markup-and-values)
  (check-group "markup and values")
  ;; in the markup, kept
  (check= (rewrite-last '(document (quote (strong "x"))))
          '(quote (strong "x")))
  (check= (rewrite-last '(document (assign "q" (macro (quote "y"))) (q)))
          '(quote "y"))
  ;; the result of an extern in the markup (map, std-utils-dtd)
  (check= (rewrite-last '(document (map (macro "x" (em (arg "x")))
                                        (tuple "1" "2"))))
          '(quote (tuple (compound (macro "x" (em (arg "x"))) "1")
                         (compound (macro "x" (em (arg "x"))) "2"))))
  ;; in a value, the quoted tree
  (check= (rewrite-last '(document (assign "v" (concat "a" (quote "b")))
                                   (value "v")))
          '(concat "a" "b"))
  (check= (rewrite-last '(document (if (equal (quote "b") "b") "yes" "no")))
          "yes")
  ;; the typesetter typesets the body of an ornament with margins of its
  ;; own (bridge_ornamented_rep): there they stay references
  (check= (rewrite-last '(document
                          (with "par-left" "2cm"
                            (document
                              (ornament (with "par-left" (plus (value "par-left") "1fn") "x"))
                              (with "par-left" (plus (value "par-left") "1fn") "y")))))
          '(with "par-left" "2cm"
             (document
               (ornament (with "par-left" (plus (value "par-left") "1fn") "x"))
               (with "par-left" (plus "2cm" "1fn") "y")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memorization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; style-rewrite-statistics: (evaluations memorized table-size) of the last
;; evaluation of the document.
(define (test-memorization)
  (check-group "memorization")
  (with-scratch-body
   '(document (assign "foo" (macro "x" (concat "[" (arg "x") "]")))
              (concat "a " (foo "b"))
              (concat "c " (foo "d"))
              "e")
   (lambda ()
     (style-rewrite-buffer)
     (with s (style-rewrite-statistics)
       (check-true (> (car s) 5))
       (check= (cadr s) 0))
     ;; nothing changed: the document is found at once
     (style-rewrite-buffer)
     (check= (list-head (style-rewrite-statistics) 2) '(1 1))
     ;; an edit in the last paragraph: the path to it is evaluated again,
     ;; its siblings are found in the memorizers
     (tree-set (buffer-tree) 3 "f")
     (style-rewrite-buffer)
     (with s (style-rewrite-statistics)
       (check-true (<= (car s) 4))
       (check-true (>= (cadr s) 2)))
     (check= (cAr (tree->stree (style-rewrite-buffer))) "f")))
  ;; an edit in a list or in a table with a caption: the paragraphs after
  ;; them are found in the memorizers, the environment after the edited
  ;; tag being the same as before (the parts of the macro bodies which hold
  ;; arguments and the cell formats are interned, see intern_tree)
  (let ((after (map (lambda (i) `(concat (strong ,(number->string i)) " p"))
                    (iota 30))))
    (for (body (list `(itemize (document (concat (item) "a")))
                     `(big-table (tabular (tformat (table (row (cell "a")))))
                                 "caption")))
      (with-scratch-body
       `(document ,body ,@after)
       (lambda ()
         (style-rewrite-buffer)
         (let ((first (car (style-rewrite-statistics)))
               (u (tree-ref (buffer-tree) 0)))
           (tree-set u (stree->tree
                        (if (tree-is? u 'itemize)
                            `(itemize (document (concat (item) "ab")))
                            `(big-table (tabular (tformat (table (row (cell "ab")))))
                                        "caption"))))
           (style-rewrite-buffer)
           ;; (the tag itself is evaluated again; before, the 30 paragraphs
           ;; too, with a new environment)
           (with s (style-rewrite-statistics)
             (check-true (< (* 2 (car s)) first))
             (check-true (>= (cadr s) 30)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pages of the manual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (page errors): the errors left by the rewriter (none since auto-id is
;; left to the typesetter, October 2026; see src/docs/pure-style.md)
(define manual-pages
  '(("doc/about/about.en.tm" ())
    ("doc/about/welcome/welcome.en.tm" ())
    ("doc/devel/style/style.en.tm" ())
    ("doc/main/start/man-conventions.en.tm" ())))

(define (rewrite-file name)
  ;; the errors of the rewriting of a file of the distribution and the time
  (let* ((u (string->url (string-append "$TEXMACS_PATH/" name)))
         (old (current-buffer)))
    (load-buffer u)
    (let* ((start (texmacs-time))
           (r (tree->stree (style-rewrite-buffer)))
           (time (- (texmacs-time) start))
           (stats (style-rewrite-statistics)))
      (display* "  " name ": " (car stats) " evaluations, " time " ms\n")
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer* old))
      (stree-errors r))))

(define (test-manual)
  (check-group "manual")
  (for (p manual-pages)
    (check= (rewrite-file (car p)) (cadr p))))

;; Every English page of the documentation (about 700, 25 seconds): none
;; may leave an error in its result. A new primitive of the typesetter, or a
;; changed one, shows up here first.
(define (doc-pages dir)
  (append-map
   (lambda (u)
     (with name (url->string (url-tail u))
       (cond ((in? name '("." "..")) '())
             ((url-directory? u) (doc-pages u))
             ((string-ends? name ".en.tm") (list u))
             (else '()))))
   (url-read-directory dir "*")))

(define (test-documentation)
  (check-group "documentation")
  (let* ((pages (sort (doc-pages (url-expand "$TEXMACS_PATH/doc"))
                      (lambda (u v) (string<=? (url->string u) (url->string v)))))
         (old (current-buffer))
         (failed '()))
    (for (u pages)
      (load-buffer u)
      (with errs (stree-errors (tree->stree (style-rewrite-buffer)))
        (when (nnull? errs)
          (set! failed (cons (list (url->string u) errs) failed))))
      (buffer-close u))
    (when (buffer-exists? old) (switch-to-buffer* old))
    (display* "  " (length pages) " pages\n")
    (check-true (> (length pages) 600))
    (check= (reverse failed) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The caches of the typesetter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; environment_update (style-environment-update) prepares the typesetter's
;; environment and writes base-file-name, cur-file-name and secure into it
;; outside a typesetting pass, as apply_changes does with a window. The
;; caches of the typesetter (PR #333, #250) must not be disturbed by it: in
;; their check modes, each reused result is made again and compared.
;; Run with
;;
;;   TEXMACS_TYPESET_READS=check TEXMACS_TYPESET_CACHES=check \
;;   TEXMACS_PAGE_REUSE=check TEXMACS_PAGE_BREAK_FAST=check \
;;   TEXMACS_PAGE_BREAK_CACHE=check tests/scheme/check.sh \
;;   TeXmacs/progs/check/style-rewrite-test.scm
;;
;; (without them the mismatches are not counted, and the group only checks
;; that the steps run).
(define (typeset-check-mode?)
  (== (getenv "TEXMACS_TYPESET_READS") "check"))

(define (test-typesetter-caches)
  (check-group "typesetter caches")
  (when (not (typeset-check-mode?))
    (display* "  (not in the check modes of the typesetter: "
              "mismatches not counted)\n"))
  (let* ((u (string->url "$TEXMACS_PATH/doc/about/welcome/welcome.en.tm"))
         (old (current-buffer))
         (before (typeset-check-mismatches)))
    (load-buffer u)
    (update-forced)
    (style-rewrite-buffer)
    (for (i (iota 6))
      (style-environment-update)
      (tree-insert (buffer-tree) 1 (list (string-append "added " (number->string i))))
      (update-forced)
      (style-rewrite-buffer))
    (style-environment-update)
    (update-forced)
    (check= (- (typeset-check-mismatches) before) 0)
    ;; the rewriter follows the edits too
    (check= (list-ref (tree->stree (style-rewrite-buffer)) 2) "added 5")
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer* old))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (style-rewrite-test-failures)
  (check-suite "style-rewrite")
  (if (not (defined? 'style-rewrite-buffer))
      (begin
        (check-group "build")
        (check-true (defined? 'style-rewrite-buffer)))
      (begin
        (test-macros)
        (test-primitives)
        (test-partial-evaluation)
        (test-boundaries)
        (test-markup-and-values)
        (test-memorization)
        (test-manual)
        (test-documentation)
        (test-typesetter-caches)))
  (check-end))
