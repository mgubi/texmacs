
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-fold.scm
;; DESCRIPTION : folds of program code
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A fold hides lines of program code: of a source file, of a code block, of a
;; session input (a document of lines). It is a state of the view, kept on the
;; hidden lines (code-set-line-fold-state, System/Language/code_lines.cpp), not
;; markup: the document, its export, its printing and the editing do not see
;; it, and a line which is modified forgets it. A shown line before hidden
;; lines ends by a mark (Typeset/Bridge/bridge.cpp).
;;
;; The blocks which can be folded come from providers (code-features.scm):
;;   fold-ranges     (procedure) the blocks around the cursor
;;   fold-all-ranges (procedure) all the blocks of the code at the cursor
;; as lists of (first last): the first line stays shown, the lines after it up
;; to last are hidden (tree-sitter: the nodes of several lines, without the
;; line which closes them; a server: its folding ranges).
;;
;; The cursor does not stay in a hidden line (code-cursor-in-fold, called by
;; Edit/Interface/edit_cursor.cpp): moving right from the end of the first
;; line of a fold, or left from the start of the line after it, goes over
;; the fold (the arrows; up and down go over the lines which have no room by
;; themselves); arriving otherwise (a search, a jump to a definition...)
;; opens the folds which hide the line.

(texmacs-module (code code-fold)
  (:use (code code-features)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The hidden lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The state of a line is the number of the folds which hide it: folds nest.
;; A fold starts at a line when the next line is hidden by more folds;
;; folding a block around a fold makes its lines deeper, unfolding it keeps
;; the folds inside (as the folds of tree-sitter, progs/treesit/ts-edit.scm).

(tm-define (code-fold-depth root row)
  (:synopsis "The number of the folds which hide the line @row of @root")
  (if (and (tree-is? root 'document) (>= row 0) (< row (tree-arity root)))
      (code-line-fold-state (tree-ref root row))
      0))

(tm-define (code-hidden? root row)
  (:synopsis "Is the line @row of the lines @root hidden by a fold?")
  (> (code-fold-depth root row) 0))

(define (update-line line)
  ;; (the line is typeset again; while the document is modified, as when
  ;; the cursor is moved into a fold by an edit, its path is not known yet)
  (with p (tree->path line)
    (if p (update-path p)
        (delayed (with p (tree->path line) (when p (update-path p)))))))

;; the deepest folds which the lines keep (CODE_FOLD_MAX of
;; System/Language/code_lines.hpp)
(define code-fold-max 9)

(define mark-refresh? #f)

(define (refresh-mark line)
  ;; the first line of a fold gets or loses its mark (...), after its end:
  ;; it is typeset again, and the window is drawn again, once for many
  ;; folds (the part drawn again after a change is that of the line as it
  ;; was, without the mark)
  (update-line line)
  (when (not mark-refresh?)
    (set! mark-refresh? #t)
    (delayed
      (:idle 1)
      (set! mark-refresh? #f)
      (refresh-window))))

(define (set-depth! root row d)
  (let ((line (tree-ref root row)))
    (when (!= (code-fold-depth root row) d)
      (code-set-line-fold-state line (max d 0))
      (update-line line))))

(define (fold-starts? root row)
  (> (code-fold-depth root (+ row 1)) (code-fold-depth root row)))

(tm-define (code-fold-lines root first last)
  (:synopsis "Hides the lines after @first up to @last of @root")
  ;; (not when a fold starts already at the first line, nor deeper than
  ;; the folds which the lines can keep); whether there was something to
  ;; hide
  (and (tree-is? root 'document) (< first last) (< last (tree-arity root))
       (not (fold-starts? root first))
       (< (apply max (map (lambda (r) (code-fold-depth root r))
                          (.. (+ first 1) (+ last 1))))
          code-fold-max)
       (begin
         (for (r (.. (+ first 1) (+ last 1)))
           (set-depth! root r (+ (code-fold-depth root r) 1)))
         (refresh-mark (tree-ref root first))
         #t)))

(tm-define (code-unfold-lines root row)
  (:synopsis "Shows the lines of the fold which starts at the line @row of @root")
  ;; (the folds inside it stay); whether there was one
  (and (fold-starts? root row)
       (let ((d0 (code-fold-depth root row)))
         (let loop ((r (+ row 1)))
           (when (> (code-fold-depth root r) d0)
             (set-depth! root r (- (code-fold-depth root r) 1))
             (loop (+ r 1))))
         (refresh-mark (tree-ref root row))
         #t)))

(define (unfold-leading! root)
  ;; the hidden lines at the start, which no shown line comes before (the
  ;; first line of their fold was deleted), are shown by one level;
  ;; whether there were some
  (and (tree-is? root 'document) (> (tree-arity root) 0)
       (> (code-fold-depth root 0) 0)
       (let loop ((r 0))
         (when (and (< r (tree-arity root)) (> (code-fold-depth root r) 0))
           (set-depth! root r (- (code-fold-depth root r) 1))
           (loop (+ r 1)))
         #t)))

(tm-define (code-folds root)
  (:synopsis "The folds of the lines @root, as (first last), outer ones first")
  (if (not (tree-is? root 'document)) '()
      (let ((n (tree-arity root)))
        (let loop ((r 0) (acc '()))
          (if (>= (+ r 1) n) (reverse acc)
              (loop (+ r 1)
                    (if (fold-starts? root r)
                        (let ((d0 (code-fold-depth root r)))
                          (let end ((e (+ r 1)))
                            (if (and (< (+ e 1) n)
                                     (> (code-fold-depth root (+ e 1)) d0))
                                (end (+ e 1))
                                (cons (list r e) acc))))
                        acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The code at the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lines-at p)
  ;; (root row column) of the place @p in a line of a document of lines
  (and (pair? p) (pair? (cdr p))
       (let* ((up (cDr p))
              (root (and (pair? up) (path->tree (cDr up)))))
         (and root (tree-is? root 'document)
              (list root (cAr up) (cAr p))))))

(tm-define (code-cursor-lines)
  (:synopsis "(root row column) of the cursor in its lines of code, or #f")
  (lines-at (cursor-path)))

(define (line-length root row)
  (with t (tree-ref root row)
    (if (tree-atomic? t) (string-length (tree->string t)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (innermost ranges row)
  ;; the innermost of @ranges which hide something around @row
  (with in (list-filter ranges (lambda (r) (and (<= (car r) row)
                                                 (<= row (cadr r))
                                                 (< (car r) (cadr r)))))
    (and (nnull? in)
         (car (sort in (lambda (x y)
                         (or (> (car x) (car y))
                             (and (== (car x) (car y))
                                  (< (cadr x) (cadr y))))))))))

(tm-define (code-fold)
  (:synopsis "Folds the block at the cursor")
  (let ((c (code-cursor-lines)))
    (if (not c) (set-message "Not in program code" "")
        (let* ((root (car c)) (row (cadr c))
               (ranges (or (code-ask 'fold-ranges) '()))
               ;; (those which are not folded yet)
               (open (list-filter ranges
                                  (lambda (r) (not (fold-starts? root (car r))))))
               (r (innermost open row)))
          (if (not (and r (code-fold-lines root (car r) (cadr r))))
              (set-message "No block to fold here" "")
              ;; (the cursor does not stay in a hidden line)
              (when (> row (car r))
                (go-to (rcons (tree->path (tree-ref root (car r)))
                              (line-length root (car r))))))))))

(tm-define (code-unfold)
  (:synopsis "Unfolds the block whose first line is that of the cursor")
  (let ((c (code-cursor-lines)))
    (when (not (and c (code-unfold-lines (car c) (cadr c))))
      (set-message "No folded block here" ""))))

(tm-define (code-fold-all)
  (:synopsis "Folds all the blocks of the code at the cursor")
  (let ((c (code-cursor-lines))
        (ranges (code-ask 'fold-all-ranges)))
    (if (not (and c ranges)) (set-message "No block to fold" "")
        (let* ((root (car c))
               ;; (the outer ones first: the inner ones are then hidden; of
               ;; two with the same first line, the larger one)
               (l (sort ranges (lambda (x y)
                                 (or (< (car x) (car y))
                                     (and (== (car x) (car y))
                                          (> (cadr x) (cadr y))))))))
          (for (r l) (code-fold-lines root (car r) (cadr r)))
          (when (code-hidden? root (cadr c))
            ;; (the cursor goes to the first line of its fold)
            (let loop ((k (cadr c)))
              (if (code-hidden? root k) (loop (- k 1))
                  (go-to (rcons (tree->path (tree-ref root k))
                                (line-length root k))))))))))

(tm-define (code-unfold-all)
  (:synopsis "Unfolds all the blocks of the code at the cursor")
  (with c (code-cursor-lines)
    (when c
      (let loop ()
        (with fs (code-folds (car c))
          (when (nnull? fs)
            (for (f fs) (code-unfold-lines (car c) (car f)))
            (loop))))
      (let again () (when (unfold-leading! (car c)) (again))))))

(tm-define (code-can-fold?)
  (:synopsis "Can the blocks of the code at the cursor be folded?")
  (and (code-cursor-lines) (code-available? 'fold-ranges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fold-of root row)
  ;; the first line of the outermost fold which hides @row, and its last
  ;; line
  (let ((first (let loop ((k row)) (if (code-hidden? root k) (loop (- k 1)) k)))
        (last (let loop ((k row)) (if (code-hidden? root (+ k 1)) (loop (+ k 1)) k))))
    (list first last)))

(define (unfold-around! root row)
  ;; the folds which hide @row open, from the innermost
  (when (code-hidden? root row)
    (let find ((r (- row 1)))
      (cond ((< r 0)
             (when (unfold-leading! root) (unfold-around! root row)))
            ((< (code-fold-depth root r) (code-fold-depth root row))
             (code-unfold-lines root r)
             (unfold-around! root row))
            (else (find (- r 1)))))))

(tm-define (code-cursor-in-fold old new)
  (:synopsis "The cursor comes from @old to @new, in a hidden line")
  ;; (called by edit_cursor.cpp; @old is the last place of the cursor
  ;; outside of the folds)
  (let ((o (lines-at old))
        (n (lines-at new)))
    (when n
      (let* ((root (car n)) (row (cadr n))
             (f (fold-of root row)) (first (car f)) (last (cadr f))
             (same? (and o (tree-eq? (car o) root))))
        (cond ((and same? (== (cadr o) first)
                    (== (caddr o) (line-length root first))
                    (== row (+ first 1)) (== (caddr n) 0)
                    (< (+ last 1) (tree-arity root)))
               ;; (right: the start of the line after the fold)
               (with p (rcons (tree->path (tree-ref root (+ last 1))) 0)
                 (delayed (go-to p))))
              ((and same? (== (cadr o) (+ last 1)) (== (caddr o) 0)
                    (== row last) (== (caddr n) (line-length root last)))
               ;; (left: the end of the first line of the fold)
               (with p (rcons (tree->path (tree-ref root first))
                              (line-length root first))
                 (delayed (go-to p))))
              (else
               ;; (the folds which hide it open, from the innermost)
               (unfold-around! root row)))))))
