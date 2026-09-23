
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-merge.scm
;; DESCRIPTION : three way merging of documents
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a common ancestor 'base' of two versions 'ours' and 'theirs' of
;; a document, (merge-versions base ours theirs) returns a document which
;; contains all changes made on only one side, while the changes made on
;; both sides are marked up with version-both (old = ours, new = theirs),
;; as in the output of compare-versions.  The merge is structured: it
;; descends into paragraphs, concatenations (word by word) and other tags.

(texmacs-module (version version-merge)
  (:use (version version-compare)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Longest common subsequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (common-prefix v1 v2)
  (let loop ((i 0))
    (if (and (< i (vector-length v1)) (< i (vector-length v2))
             (== (vector-ref v1 i) (vector-ref v2 i)))
        (loop (+ i 1))
        i)))

(define (common-suffix v1 v2 start)
  (let loop ((i 0))
    (let ((i1 (- (vector-length v1) i 1))
          (i2 (- (vector-length v2) i 1)))
      (if (and (>= i1 start) (>= i2 start)
               (== (vector-ref v1 i1) (vector-ref v2 i2)))
          (loop (+ i 1))
          i))))

(define (lcs-middle v1 v2 s1 e1 s2 e2 match)
  ;; Dynamic programming on the ranges [s1, e1) and [s2, e2)
  (let* ((n1 (- e1 s1))
         (n2 (- e2 s2))
         (w (+ n2 1))
         (tab (make-vector (* (+ n1 1) w) 0)))
    (define (ref i j) (vector-ref tab (+ (* i w) j)))
    (do ((i (- n1 1) (- i 1))) ((< i 0))
      (do ((j (- n2 1) (- j 1))) ((< j 0))
        (vector-set! tab (+ (* i w) j)
                     (if (== (vector-ref v1 (+ s1 i)) (vector-ref v2 (+ s2 j)))
                         (+ (ref (+ i 1) (+ j 1)) 1)
                         (max (ref (+ i 1) j) (ref i (+ j 1)))))))
    (let loop ((i 0) (j 0))
      (when (and (< i n1) (< j n2))
        (cond ((== (vector-ref v1 (+ s1 i)) (vector-ref v2 (+ s2 j)))
               (vector-set! match (+ s1 i) (+ s2 j))
               (loop (+ i 1) (+ j 1)))
              ((>= (ref (+ i 1) j) (ref i (+ j 1)))
               (loop (+ i 1) j))
              (else (loop i (+ j 1))))))))

(define max-lcs-size 4000000)

(define (lcs-match v1 v2)
  ;; Vector which associates to each index of v1 the matching index of v2
  ;; in a longest common subsequence, or #f
  (let* ((n1 (vector-length v1))
         (n2 (vector-length v2))
         (match (make-vector n1 #f))
         (p (common-prefix v1 v2))
         (s (common-suffix v1 v2 p)))
    (do ((i 0 (+ i 1))) ((>= i p))
      (vector-set! match i i))
    (do ((i 0 (+ i 1))) ((>= i s))
      (vector-set! match (- n1 i 1) (- n2 i 1)))
    (when (<= (* (- n1 p s) (- n2 p s)) max-lcs-size)
      ;; NOTE: for very large changes, only the common ends are matched
      (lcs-middle v1 v2 p (- n1 s) p (- n2 s) match))
    match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three way merging of lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (subvector->list v start end)
  (let loop ((i (- end 1)) (acc '()))
    (if (< i start) acc (loop (- i 1) (cons (vector-ref v i) acc)))))

(define (merge-chunk tag o a b)
  ;; Merge the unstable chunks o, a and b (lists of children of tag)
  (cond ((== a b) a)
        ((== o a) b)
        ((== o b) a)
        ((and (== (length o) (length a)) (== (length o) (length b))
              (or (!= tag 'concat) (> (length o) 1)
                  (and (pair? (car a)) (pair? (car b)))))
         ;; Replacements of elements: merge them one by one
         ;; NOTE: single words of a concatenation are not merged any further
         (map merge-versions o a b))
        (else
          (with d (compare-versions (version-normalize (cons tag a))
                                    (version-normalize (cons tag b)))
            (if (tm-is? d tag) (cdr d) (list d))))))

(tm-define (merge-versions-list tag o a b)
  (:synopsis "Three way merge of the lists of children @o, @a and @b of @tag")
  ;; Classical diff3: walk along the elements of o which are matched in
  ;; both a and b; the chunks in between are merged by merge-chunk.
  (let* ((vo (list->vector o))
         (va (list->vector a))
         (vb (list->vector b))
         (ma (lcs-match vo va))
         (mb (lcs-match vo vb))
         (no (vector-length vo)))
    (let loop ((io 0) (ia 0) (ib 0) (j 0) (acc '()))
      (cond ((>= j no)
             (reverse
              (append (reverse (merge-chunk tag
                                            (subvector->list vo io no)
                                            (subvector->list va ia
                                                             (vector-length va))
                                            (subvector->list vb ib
                                                             (vector-length vb))))
                      acc)))
            ((and (vector-ref ma j) (vector-ref mb j)
                  (>= (vector-ref ma j) ia) (>= (vector-ref mb j) ib))
             (let* ((ja (vector-ref ma j))
                    (jb (vector-ref mb j))
                    (chunk (merge-chunk tag
                                        (subvector->list vo io j)
                                        (subvector->list va ia ja)
                                        (subvector->list vb ib jb))))
               (loop (+ j 1) (+ ja 1) (+ jb 1) (+ j 1)
                     (cons (vector-ref vo j)
                           (append (reverse chunk) acc)))))
            (else (loop io ia ib (+ j 1) acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three way merging of trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (as-concat t)
  (version-denormalize (if (tm-is? t 'concat) t `(concat ,t))))

(define (textual? t)
  (or (string? t) (tm-is? t 'concat)))

(define (same-shape? o a b)
  (and (pair? o) (pair? a) (pair? b)
       (== (car o) (car a)) (== (car o) (car b))
       (== (length o) (length a)) (== (length o) (length b))
       (nin? (car o) '(graphics table tformat))))

(tm-define (merge-versions o a b)
  (:synopsis "Three way merge of @a and @b with common ancestor @o")
  (cond ((== a b) a)
        ((== o a) b)
        ((== o b) a)
        ((and (tm-is? o 'document) (tm-is? a 'document) (tm-is? b 'document))
         (version-normalize
          (cons 'document (merge-versions-list 'document
                                               (cdr o) (cdr a) (cdr b)))))
        ((and (textual? o) (textual? a) (textual? b))
         (version-normalize
          (cons 'concat (merge-versions-list 'concat
                                             (cdr (as-concat o))
                                             (cdr (as-concat a))
                                             (cdr (as-concat b))))))
        ((same-shape? o a b)
         (cons (car o) (map merge-versions (cdr o) (cdr a) (cdr b))))
        (else (compare-versions a b))))

(tm-define (merge-conflicts t)
  (:synopsis "Number of conflicts which remain in the merged document @t")
  (cond ((npair? t) 0)
        ((tm-in? t '(version-both version-old version-new)) 1)
        (else (apply + (map merge-conflicts (cdr t))))))
