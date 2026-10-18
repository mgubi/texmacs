;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-fold.scm
;; DESCRIPTION : folding the blocks of source files (folding ranges)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A fold changes what is shown, not the document: its lines stay in the
;; buffer (saved, sent to the server, searched, undone as usual), but the
;; typesetter does not show them (lsp-fold-line, C++: insert_paragraph skips
;; them), and the first line of the fold ends with a marker. The blocks are
;; those of the server (textDocument/foldingRange): std ( folds the block at
;; the cursor, std ) unfolds it, the menu folds or unfolds all of them.
;;
;; A fold is kept as the tree of its first line and the trees of the lines
;; which it hides, with their texts: after each change of the buffer, a fold
;; whose lines are no longer where they were is dropped, and the lines whose
;; texts have changed are registered again (the typesetter shows a line
;; whose text is not the one which was folded). The cursor does not stay in
;; a hidden line: moving right from the first line of a fold goes after it,
;; moving left from the line after it goes to the end of its first line, and
;; a fold where the cursor comes otherwise (a search, a definition) opens.

(texmacs-module (lsp lsp-fold)
  (:use (lsp lsp-base)
        (lsp lsp-client)
        (lsp lsp-documents)
        (lsp lsp-edit)))

;; the marker at the end of the first line of a fold (Cork)
(define fold-marker " <#2026> ")

;; buffer name -> its folds, each (head hidden labels head-label)
(define folds (make-ahash-table))

(define (buffer-key) (url->string (current-buffer)))
(define (current-folds) (or (ahash-ref folds (buffer-key)) '()))

(define (fold-head f) (car f))
(define (fold-hidden f) (cadr f))

(define (body) (buffer-get-body (current-buffer)))

(define (fold-index f b)
  ;; the line of the first line of @f in the body @b, or #f if the fold is
  ;; no longer there (its lines moved, or were removed)
  (let ((h (fold-head f)))
    (and (tree-is? b 'document)
         (tree-up h) (tree-eq? (tree-up h) b)
         (with i (tree-index h)
           (and (< (+ i (length (fold-hidden f))) (tree-arity b))
                (let loop ((l (fold-hidden f)) (k (+ i 1)))
                  (cond ((null? l) i)
                        ((tree-eq? (car l) (tree-ref b k)) (loop (cdr l) (+ k 1)))
                        (else #f))))))))

(define (make-fold b start end)
  (let* ((head (tree-ref b start))
         (hidden (map (lambda (k) (tree-ref b k)) (.. (+ start 1) (+ end 1)))))
    (list head hidden (map tree->string hidden) (tree->string head))))

(define (show-lines b from to)
  ;; the lines from to to are typeset again
  (if (> (- to from) 2000) (update-current-buffer)
      (with p (buffer-path)
        (for (k (.. from (min (+ to 1) (tree-arity b))))
          (update-path (append p (list k)))))))

(define (register! fs)
  ;; the folds @fs are given to the typesetter
  (for (f fs)
    (for (t (fold-hidden f)) (lsp-fold-line t #t))
    (lsp-fold-marker (fold-head f) fold-marker)))

(define (unregister! f)
  (for (t (fold-hidden f)) (lsp-fold-line t #f))
  (lsp-fold-marker (fold-head f) ""))

(define (set-folds! fs)
  ;; the folds of the current buffer are @fs: those which go are shown
  ;; again, those which come are hidden (a line in two folds stays hidden)
  (let ((old (current-folds))
        (b (body)))
    (for (f old) (unregister! f))
    (register! fs)
    (if (null? fs) (ahash-remove! folds (buffer-key))
        (ahash-set! folds (buffer-key) fs))
    (for (f (append old fs))
      (with i (fold-index f b)
        (when i (show-lines b i (+ i (length (fold-hidden f)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The blocks of the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-folding-ranges-of r)
  (:synopsis "The FoldingRanges @r as (start end), the blocks of lines")
  ;; (those of one line, which hide nothing, are left out)
  (list-filter
   (map (lambda (x)
          (list (or (lsp-number-value (lsp-ref x "startLine")) 0)
                (or (lsp-number-value (lsp-ref x "endLine")) 0)))
        (lsp-items r))
   (lambda (x) (> (cadr x) (car x)))))

(define (folding-ranges)
  ;; the blocks of the current buffer, or #f (with a message)
  (let ((c (lsp-buffer-client*))
        (uri (lsp-buffer-uri)))
    (cond ((not (and c uri))
           (set-message "No language server for this buffer" "") #f)
          ((lsp-null? (lsp-ref (lsp-client-capabilities c)
                               "foldingRangeProvider"))
           (set-message "The language server cannot fold" "") #f)
          (else
           ;; (the server must have the text: the changes which wait)
           (lsp-sync-pending)
           (with a (lsp-request-wait c "textDocument/foldingRange"
                                     (lsp-object "textDocument"
                                                 (lsp-object "uri" uri))
                                     5000)
             (if (== (car a) 'result) (lsp-folding-ranges-of (cadr a))
                 (begin (set-message "The language server does not answer" "")
                        #f)))))))

(tm-define (lsp-innermost-range ranges line)
  (:synopsis "The innermost of the blocks @ranges which contain @line")
  (with in (list-filter ranges (lambda (r) (and (<= (car r) line)
                                                (<= line (cadr r)))))
    (and (nnull? in)
         (car (sort in (lambda (x y)
                         (or (> (car x) (car y))
                             (and (== (car x) (car y))
                                  (< (cadr x) (cadr y))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cursor-lo)
  (let* ((p (cursor-path))
         (b (buffer-path)))
    (and (list-starts? p b)
         (with r (list-tail p (length b))
           (and (= (length r) 2) r)))))

(define (folded-at? b start)
  (list-or (map (lambda (f) (== (fold-index f b) start)) (current-folds))))

(define (end-of-line b i)
  (append (buffer-path) (list i (string-length (tree->string (tree-ref b i))))))

(tm-define (lsp-fold-block start end)
  (:synopsis "Folds the lines @start to @end: @start stays, with a marker")
  (let ((b (body))
        (lo (cursor-lo)))
    (when (and (tree-is? b 'document) (< start end) (< end (tree-arity b))
               (not (folded-at? b start)))
      (set-folds! (cons (make-fold b start end) (current-folds)))
      ;; (the cursor does not stay in a hidden line)
      (when (and lo (> (car lo) start) (<= (car lo) end))
        (go-to-path (end-of-line b start))))))

(tm-define (lsp-fold)
  (:synopsis "Folds the block at the cursor")
  (let ((lo (cursor-lo))
        (ranges (folding-ranges)))
    (when (and lo ranges)
      (let* ((b (body))
             ;; (the blocks which are not folded yet)
             (open (list-filter ranges
                                (lambda (r) (not (folded-at? b (car r))))))
             (r (lsp-innermost-range open (car lo))))
        (if r (lsp-fold-block (car r) (cadr r))
            (set-message "No block to fold here" ""))))))

(define (fold-at b line)
  ;; the innermost fold whose first line or block contains @line
  (let* ((in (list-filter
              (current-folds)
              (lambda (f)
                (with i (fold-index f b)
                  (and i (<= i line)
                       (<= line (+ i (length (fold-hidden f))))))))))
    (and (nnull? in)
         (car (sort in (lambda (x y) (> (fold-index x b) (fold-index y b))))))))

(tm-define (lsp-unfold)
  (:synopsis "Unfolds the block at the cursor")
  (let* ((lo (cursor-lo))
         (b (body))
         (f (and lo (fold-at b (car lo)))))
    (if (not f) (set-message "No folded block here" "")
        (set-folds! (list-filter (current-folds)
                                 (lambda (g) (not (eq? g f))))))))

(tm-define (lsp-fold-all)
  (:synopsis "Folds all the blocks of the buffer")
  (with ranges (folding-ranges)
    (when ranges
      (let* ((b (body))
             (new (map (lambda (r) (make-fold b (car r) (cadr r)))
                       (list-filter ranges
                                    (lambda (r)
                                      (and (< (cadr r) (tree-arity b))
                                           (not (folded-at? b (car r)))))))))
        (set-folds! (append new (current-folds)))
        (with lo (cursor-lo)
          (when (and lo (lsp-line-folded? (tree-ref b (car lo))))
            (with f (fold-at b (car lo))
              (when f (go-to-path (end-of-line b (fold-index f b)))))))))))

(tm-define (lsp-unfold-all)
  (:synopsis "Unfolds all the blocks of the buffer")
  (set-folds! '()))

(tm-define (lsp-folds)
  (:synopsis "The folds of the current buffer, as (start end)")
  (with b (body)
    (sort (list-filter (map (lambda (f)
                              (with i (fold-index f b)
                                (and i (list i (+ i (length (fold-hidden f)))))))
                            (current-folds))
                       identity)
          (lambda (x y) (< (car x) (car y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The folds and the edits, the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (refresh-fold f b)
  ;; @f after a change of the buffer: #f if it is no longer there, else @f
  ;; with the texts of its lines now (those which changed are registered
  ;; again and typeset again)
  (with i (fold-index f b)
    (if (not i) (begin (unregister! f) #f)
        (let ((labels (map tree->string (fold-hidden f)))
              (head-label (tree->string (fold-head f))))
          (when (!= head-label (cadddr f))
            (lsp-fold-marker (fold-head f) fold-marker)
            (show-lines b i i))
          (when (!= labels (caddr f))
            (for (t (fold-hidden f)) (lsp-fold-line t #t))
            (show-lines b i (+ i (length labels))))
          (list (fold-head f) (fold-hidden f) labels head-label)))))

(tm-define (notify-buffer-changed name)
  (former name)
  (with fs (ahash-ref folds name)
    (when (and fs (== name (buffer-key)))
      (let* ((b (body))
             (new (list-filter (map (lambda (f) (refresh-fold f b)) fs)
                               identity)))
        (when (!= (length new) (length fs))
          ;; (a fold which went: its lines are shown)
          (update-current-buffer))
        (if (null? new) (ahash-remove! folds name)
            (ahash-set! folds name new))))))

(define last-lo #f)
(define cursor-fix-scheduled? #f)

(define (fix-cursor)
  ;; after a move of the cursor: in a hidden line, it goes over the fold, or
  ;; the fold opens; elsewhere, its place is kept (the place it comes from)
  (set! cursor-fix-scheduled? #f)
  (let* ((lo (cursor-lo))
         (b (body))
         (f (and lo (tree-is? b 'document) (< (car lo) (tree-arity b))
                 (lsp-line-folded? (tree-ref b (car lo)))
                 (fold-at b (car lo)))))
    (if (not f) (set! last-lo lo)
        (let* ((i (fold-index f b))
               (end (+ i (length (fold-hidden f)))))
          (cond ((and last-lo (== (car last-lo) i)
                      (== (cadr last-lo)
                          (string-length (tree->string (tree-ref b i))))
                      (< (+ end 1) (tree-arity b)))
                 ;; (from the end of its first line, forwards: after it)
                 (go-to-path (append (buffer-path) (list (+ end 1) 0))))
                ((and last-lo (== (car last-lo) (+ end 1)) (== (cadr last-lo) 0))
                 ;; (from the start of the line after it, backwards: the
                 ;; end of its first line)
                 (go-to-path (end-of-line b i)))
                (else
                 (set-folds! (list-filter (current-folds)
                                          (lambda (g) (not (eq? g f)))))))))))

(define (schedule-fix)
  (when (not cursor-fix-scheduled?)
    (set! cursor-fix-scheduled? #t)
    (delayed (fix-cursor))))

(tm-define (notify-cursor-moved status)
  ;; NOTE: no mode predicates (see lsp-edit). The cursor is looked at once
  ;; the move is done (the path of the cursor is not yet the new one here)
  (former status)
  (when (nnull? (current-folds)) (schedule-fix)))

(tm-define (lsp-cursor-in-fold)
  ;; (called by the editor when the cursor is in a line which it does not
  ;; show: notify-cursor-moved is not, the cursor has no box)
  (schedule-fix))

(tm-define (notify-buffer-closed name)
  ;; (the lines of a closed buffer are forgotten)
  (with fs (ahash-ref folds name)
    (when fs
      (for (f fs) (unregister! f))
      (ahash-remove! folds name)))
  (former name))

(tm-define (lsp-turn-off)
  ;; (the folds are those of the servers: all of them open)
  (former)
  (for (name (map car (ahash-table->list folds)))
    (with u (string->url name)
      (when (buffer-exists? u)
        (with-buffer u (lsp-unfold-all))))))
