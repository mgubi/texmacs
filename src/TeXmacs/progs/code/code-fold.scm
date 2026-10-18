
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
;; markup: the document, its export and the editing do not see it, and a line
;; which is modified forgets it. The hidden lines after a shown line are a
;; fold, which the shown line ends by a mark (Typeset/Bridge/bridge.cpp).
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

(tm-define (code-hidden? root row)
  (:synopsis "Is the line @row of the lines @root hidden by a fold?")
  (and (tree-is? root 'document) (>= row 0) (< row (tree-arity root))
       (== (code-line-fold-state (tree-ref root row)) 1)))

(define (update-line line)
  ;; (the line is typeset again; while the document is modified, as when
  ;; the cursor is moved into a fold by an edit, its path is not known yet)
  (with p (tree->path line)
    (if p (update-path p)
        (delayed (with p (tree->path line) (when p (update-path p)))))))

(define (set-hidden! root row on)
  (let ((line (tree-ref root row)))
    (when (!= (code-hidden? root row) on)
      (code-set-line-fold-state line (if on 1 0))
      (update-line line))))

(tm-define (code-fold-lines root first last)
  (:synopsis "Hides the lines after @first up to @last of @root")
  ;; (not when the first line is hidden by another fold, nor when it is
  ;; already that of a fold); whether there was something to hide
  (and (tree-is? root 'document) (< first last) (< last (tree-arity root))
       (not (code-hidden? root first))
       (not (code-hidden? root (+ first 1)))
       (begin
         (for (r (.. (+ first 1) (+ last 1))) (set-hidden! root r #t))
         (update-line (tree-ref root first))   ; its mark
         #t)))

(tm-define (code-unfold-lines root row)
  (:synopsis "Shows the lines of the fold after the line @row of @root")
  ;; whether there was one
  (and (not (code-hidden? root row)) (code-hidden? root (+ row 1))
       (begin
         (let loop ((r (+ row 1)))
           (when (code-hidden? root r)
             (set-hidden! root r #f)
             (loop (+ r 1))))
         (update-line (tree-ref root row))
         #t)))

(tm-define (code-folds root)
  (:synopsis "The folds of the lines @root, as (first last)")
  (if (not (tree-is? root 'document)) '()
      (let loop ((r 0) (acc '()))
        (cond ((>= r (tree-arity root)) (reverse acc))
              ((code-hidden? root r)
               (let end ((e r))
                 (if (code-hidden? root (+ e 1)) (end (+ e 1))
                     (loop (+ e 1) (cons (list (- r 1) e) acc)))))
              (else (loop (+ r 1) acc))))))

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
                                  (lambda (r) (not (code-hidden? root
                                                                 (+ (car r) 1))))))
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
               ;; (the outer ones first: the inner ones are then hidden)
               (l (sort ranges (lambda (x y) (< (car x) (car y))))))
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
      (for (f (code-folds (car c)))
        (code-unfold-lines (car c) (car f))))))

(tm-define (code-can-fold?)
  (:synopsis "Can the blocks of the code at the cursor be folded?")
  (and (code-cursor-lines) (code-available? 'fold-ranges)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fold-of root row)
  ;; the first line of the fold which hides @row, and its last line
  (let ((first (let loop ((k row)) (if (code-hidden? root k) (loop (- k 1)) k)))
        (last (let loop ((k row)) (if (code-hidden? root (+ k 1)) (loop (+ k 1)) k))))
    (list first last)))

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
               ;; (the fold which hides it opens, with those in it)
               (code-unfold-lines root first)))))))
