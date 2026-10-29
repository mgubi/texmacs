
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : process.scm
;; DESCRIPTION : running asynchroneous processes inside TeXmacs
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library process)
  (:use (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial number and active process management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define process-serial 0)

(define process-active (make-ahash-table))

(tm-define (process-active? serial)
  (not (not (ahash-ref process-active serial))))

(tm-define (process-running? type)
  (in? type (append-map cdr (ahash-table->list process-active))))

(tm-define (make-process-end . types)
  (set! process-serial (+ process-serial 1))
  (with serial process-serial
    ;;(display* "Launch process " serial "\n")
    (ahash-set! process-active serial types)
    (lambda ()
      ;;(display* "Terminate process " serial "\n")
      (ahash-remove! process-active serial))))

(tm-define (process-deactivate type)
  (for (serial (map car (ahash-table->list process-active)))
    (when (in? type (ahash-ref process-active serial))
      ;;(display* "Interrupt process " serial "\n")
      (ahash-remove! process-active serial))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operating on document snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ((make-process-docrange serial fun tp1 tp2 next))
  (let* ((t1 (tree-pointer->tree tp1))
         (t2 (tree-pointer->tree tp2))
         (p1 (and t1 (tree->path t1)))
         (p2 (and t2 (tree->path t2)))
         (l1 (and p1 (cAr p1)))
         (l2 (and p2 (cAr p2))))
    ;;(display* "process " p1 " -- " p2 "\n")
    (cond ((not (and p1 p2 (== (cDr p1) (cDr p2))))
           (next))
          ((== p1 p2)
           (if (ahash-ref process-active serial)
               (fun serial tp1 (tm->stree t1) next)
               (begin
                 (tree-pointer-detach tp1)
                 (next))))
          ((< l1 l2)
           (let* ((mid1 (quotient (+ l1 l2) 2))
                  (mid2 (+ mid1 1))
                  (t1* (tm-ref t1 :up mid1))
                  (t2* (tm-ref t1 :up mid2))
                  (tp1* (if (== mid1 l1) tp1 (tree->tree-pointer t1*)))
                  (tp2* (if (== mid2 l2) tp2 (tree->tree-pointer t2*)))
                  (proc2 (make-process-docrange serial fun tp2* tp2 next))
                  (proc1 (make-process-docrange serial fun tp1 tp1* proc2)))
             (proc1)))
          (else (next)))))

(tm-define (make-process-document serial fun t next)
  (when (tree->path t)
    (cond ((tm-func? t 'with)
           (make-process-document serial fun (tm-ref t :last) next))
          ((tm-func? t 'document)
           (let* ((tp1 (tree->tree-pointer (tm-ref t 0)))
                  (tp2 (if (== (tm-arity t) 1) tp1
                           (tree->tree-pointer (tm-ref t :last)))))
             (make-process-docrange serial fun tp1 tp2 next)))
          (else
            (with tp (tree->tree-pointer t)
              (make-process-docrange serial fun tp tp next))))))

(tm-define (make-process-documents serial fun ts next)
  (if (null? ts) next
      (with tail (make-process-documents serial fun (cdr ts) next)
        (make-process-document serial fun (car ts) tail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating top-level processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ((make-process-clean next))
  (with-innermost t 'identity
    (tree-remove-node! t 0)
    (tree-correct-upwards t))
  (next))

(tm-define (make-process fun . types)
  (with end (apply make-process-end types)
    (cond ((not (selection-active-any?))
           (make-process-document process-serial fun (buffer-tree) end))
          ((selection-active-small?)
           (with sel (selection-tree)
             (clipboard-cut "dummy")
             (insert-go-to `(identity ,sel) (cons 0 (path-start sel '())))
             (and-with t (tree-innermost 'identity)
               (with clean (make-process-clean end)
                 (make-process-document process-serial fun t clean)))))
          (else
            (with sels (selection-trees)
              (selection-cancel)
              (make-process-documents process-serial fun sels end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting an answer in place of the tree which was processed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The answer of AI or LanguageTool for a tree @t (a paragraph, or the
;; identity of make-process around a selection) is a document when the
;; engine wrapped it in <p>, and is never nested in the document (#526).

(define (answer-normalize u)
  ;; @u without the identity of make-process, which an engine may keep
  ;; around its paragraphs, and without the empty paragraphs at its end (as
  ;; ai_post, ai.cpp, for Correct and Translate); #f for empty paragraphs
  (let* ((u (tm->tree u))
         (u (if (and (tm-func? u 'identity 1)
                     (tm-func? (tm-ref u 0) 'document))
                (tm-ref u 0) u))
         (empty? (lambda (x) (tm-equal? x ""))))
    (cond ((not (tm-func? u 'document)) u)
          ((list-every empty? (tm-children u)) #f)
          (else
            (let drop ((l (reverse (tm-children u))))
              (if (empty? (car l)) (drop (cdr l))
                  (tm->tree `(document ,@(reverse l)))))))))

(define (merge-strings l)
  ;; the strees @l without empty strings nor two strings in a row
  (cond ((null? l) l)
        ((== (car l) "") (merge-strings (cdr l)))
        ((and (string? (car l)) (pair? (cdr l)) (string? (cadr l)))
         (merge-strings (cons (string-append (car l) (cadr l)) (cddr l))))
        (else (cons (car l) (merge-strings (cdr l))))))

(define (make-paragraph l)
  ;; the paragraph made of the trees or strees @l (concats are flattened)
  (let* ((items (lambda (x)
                  (with s (tm->stree x)
                    (if (func? s 'concat) (cdr s) (list s)))))
         (m (merge-strings (append-map items l))))
    (stree->tree (cond ((null? m) "")
                       ((null? (cdr m)) (car m))
                       (else `(concat ,@m))))))

(define (children-from t i n)
  ;; the @n children of @t from the @i-th one
  (map (lambda (k) (tree-ref t k)) (.. i (+ i n))))

(define (tree-set-in! t p u)
  ;; @t, a child of @p, set to @u; when @p is a concat which the correction
  ;; leaves with one child (an identity replaced by a string), that child
  (tree-set! t (tree-copy u))
  (tree-correct-upwards t)
  (if (and p (tree-is? p 'concat) (== (tree-arity p) 1))
      (begin
        (tree-set! p (tree-copy (tree-ref p 0)))
        (list p))
      (list t)))

(define (splice-paragraphs t doc u)
  ;; the paragraphs of @u in place of @t, a paragraph of @doc
  (let* ((i (tree-index t))
         (l (map tree-copy (tm-children u))))
    (tree-set! t (car l))
    (when (nnull? (cdr l))
      (tree-insert! doc (+ i 1) (cdr l)))
    (with r (children-from doc i (length l))
      (for-each tree-correct-upwards r)
      r)))

(define (split-paragraph t u)
  ;; the paragraphs of @u (at least two) in place of @t, a child of a concat
  ;; paragraph of a document, which is split around them:
  ;; (concat left... u0), u1, ..., (concat u(n-1) right...)
  (let* ((par (tree-up t))
         (doc (tree-up par))
         (j (tree-index par))
         (k (tree-index t))
         (items (tree-children par))
         (left (list-head items k))
         (right (list-tail items (+ k 1)))
         (ps (tm-children u))
         (head (make-paragraph (rcons left (car ps))))
         (middle (map tree-copy (cDr (cdr ps))))
         (tail (make-paragraph (cons (cAr ps) right)))
         (new (append (list head) middle (list tail))))
    (tree-set! par head)
    (tree-insert! doc (+ j 1) (cdr new))
    (children-from doc j (length new))))

(define (join-paragraphs u)
  ;; the paragraphs of @u in one, separated by spaces
  (make-paragraph (list-intersperse (tm-children u) " ")))

(tm-define (tree-replace-paragraph! t u)
  (:synopsis "Replace the paragraph @t by @u, which may be a document")
  ;; a document in place of a paragraph is spliced into its document; in
  ;; place of a part of a paragraph, one paragraph is unwrapped, and several
  ;; split the paragraph, or, where a paragraph cannot be split (inside em,
  ;; math, a section title, a cell without a document...), are joined.
  ;; Nothing is done for an answer of empty paragraphs. Neither the cursor
  ;; nor the selection are used. Returns the trees which hold @u.
  (let* ((u (answer-normalize u))
         (p (tree-up t))
         (pp (and p (tree-up p))))
    (cond ((not u) (list))
          ((not (tm-func? u 'document))
           (tree-set-in! t p u))
          ((and p (tree-is? p 'document))
           (splice-paragraphs t p u))
          ((== (tm-arity u) 1)
           (tree-set-in! t p (tm-ref u 0)))
          ((and (tree-is? p 'concat) pp (tree-is? pp 'document))
           (split-paragraph t u))
          (else
            (tree-set-in! t p (join-paragraphs u))))))
