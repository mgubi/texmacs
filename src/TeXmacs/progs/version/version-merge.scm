
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

(tm-define (version-match l1 l2)
  (:synopsis "Longest common subsequence of the lists @l1 and @l2")
  ;; Returns a vector which associates to each index of l1 the index
  ;; of the matching element in l2, or #f
  (lcs-match (list->vector l1) (list->vector l2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Three way merging of lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (subvector->list v start end)
  (let loop ((i (- end 1)) (acc '()))
    (if (< i start) acc (loop (- i 1) (cons (vector-ref v i) acc)))))

;; Each side changes a chunk of the ancestor by hunks (start end ins side):
;; the elements of the ancestor from start to end (excluded) are replaced
;; by the elements ins; for a pure insertion, start = end.  The hunks of
;; the two sides which overlap form groups, which are merged as a whole.

(define (hunk-start h) (car h))
(define (hunk-end h) (cadr h))
(define (hunk-ins h) (caddr h))
(define (hunk-side h) (cadddr h))
(define (hunk-insertion? h) (== (hunk-start h) (hunk-end h)))

(define (chunk-hunks o x side)
  ;; The hunks by which @side changes the chunk @o of the ancestor into @x
  (let* ((vo (list->vector o))
         (vx (list->vector x))
         (m (lcs-match vo vx))
         (n (vector-length vo))
         (nx (vector-length vx)))
    ;; s: the first element of the ancestor after the last kept one
    (let loop ((i 0) (j 0) (s 0) (acc '()))
      (if (or (>= i n) (vector-ref m i))
          (let* ((k (if (>= i n) nx (vector-ref m i)))
                 (ins (subvector->list vx j k))
                 (acc* (if (and (== s i) (null? ins)) acc
                           (cons (list s i ins side) acc))))
            (if (>= i n)
                (reverse acc*)
                (loop (+ i 1) (+ k 1) (+ i 1) acc*)))
          (loop (+ i 1) j s acc)))))

(define (hunk<? h1 h2)
  (or (< (hunk-start h1) (hunk-start h2))
      (and (== (hunk-start h1) (hunk-start h2))
           (< (hunk-end h1) (hunk-end h2)))))

(define (merge-hunk-lists l1 l2)
  (let loop ((l1 l1) (l2 l2) (acc '()))
    (cond ((null? l1) (append (reverse acc) l2))
          ((null? l2) (append (reverse acc) l1))
          ((hunk<? (car l2) (car l1)) (loop l1 (cdr l2) (cons (car l2) acc)))
          (else (loop (cdr l1) l2 (cons (car l1) acc))))))

(define (prefix? l1 l2)
  (and (<= (length l1) (length l2)) (== l1 (sublist l2 0 (length l1)))))

(define (suffix? l1 l2)
  (and (<= (length l1) (length l2))
       (== l1 (sublist l2 (- (length l2) (length l1)) (length l2)))))

(define (same-insertion? x y)
  ;; Is the pure insertion @x also made by the hunk @y, next to it?
  (and (hunk-insertion? x) (nnull? (hunk-ins x))
       (or (and (== (hunk-start y) (hunk-start x))
                (prefix? (hunk-ins x) (hunk-ins y)))
           (and (== (hunk-end y) (hunk-start x))
                (suffix? (hunk-ins x) (hunk-ins y))))))

(define (hunk-joins? h g ge)
  ;; Does the hunk @h overlap the group @g which ends at @ge?  Hunks which
  ;; only touch are independent (an insertion goes before a change which
  ;; starts at the same place, after one which ends there), except two
  ;; insertions at the same place, whose order is unclear, and insertions
  ;; of the same elements by the two sides
  (or (< (hunk-start h) ge)
      (and (== (hunk-start h) ge)
           (nnot (list-find g (lambda (m)
                                (and (!= (hunk-side m) (hunk-side h))
                                     (or (and (hunk-insertion? h)
                                              (hunk-insertion? m)
                                              (== (hunk-start m) ge))
                                         (same-insertion? h m)
                                         (same-insertion? m h)
                                         ;; elements inserted on both sides
                                         ;; must not be duplicated
                                         (and (== (hunk-end m) ge)
                                              (nnot (list-find
                                                     (hunk-ins h)
                                                     (cut in? <>
                                                          (hunk-ins m)))))))))))))

(define (side-span vo s e hunks)
  ;; The elements of the ancestor from @s to @e, with the @hunks applied
  (let loop ((i s) (l hunks) (acc '()))
    (if (null? l)
        (append (reverse acc) (subvector->list vo i e))
        (with h (car l)
          (loop (hunk-end h) (cdr l)
                (append (reverse (hunk-ins h))
                        (reverse (subvector->list vo i (hunk-start h)))
                        acc))))))

(define (contains? h1 h2)
  (and (<= (hunk-start h1) (hunk-start h2))
       (>= (hunk-end h1) (hunk-end h2))))

(define (same-kind? tag o a b)
  ;; Can @o, @a and @b be merged in turn?  They have the same tag, or are
  ;; paragraphs of text (but not words of a concatenation)
  (or (and (pair? o) (pair? a) (pair? b)
           (== (car o) (car a)) (== (car o) (car b)))
      (and (!= tag 'concat) (textual? o) (textual? a) (textual? b))))

(define (tree-words t)
  (cond ((string? t) (list-filter (string-decompose t " ") (cut != <> "")))
        ((pair? t) (apply append (map tree-words (cdr t))))
        (else '())))

(define (word-table t)
  (with h (make-ahash-table)
    (for-each (lambda (w) (ahash-set! h w #t)) (tree-words t))
    h))

(define (word-tables vo)
  ;; The words of the elements of @vo, computed when needed
  (cons vo (make-vector (vector-length vo) #f)))

(define (word-table-ref tabs j)
  (or (vector-ref (cdr tabs) j)
      (with h (word-table (vector-ref (car tabs) j))
        (vector-set! (cdr tabs) j h)
        h)))

(define (similarity zw tabs j)
  ;; The number of the words @zw (of a new element) which are words of the
  ;; j-th element of the ancestor
  (with h (word-table-ref tabs j)
    (length (list-filter zw (cut ahash-ref h <>)))))

(define (closest? z i s e tabs)
  ;; Is @z closer to the i-th element of the ancestor than to the other
  ;; ones from @s to @e?
  (let* ((zw (tree-words z))
         (k (similarity zw tabs i)))
    (let loop ((j s))
      (cond ((>= j e) #t)
            ((and (!= j i) (>= (similarity zw tabs j) k)) #f)
            (else (loop (+ j 1)))))))

(define (paired-changes tag x y vo)
  ;; Do the hunks @x and @y both replace the first elements of their ranges
  ;; one by one, the rest being removed?  The new elements must have the
  ;; tags of the old ones and be closer to them than to the other removed
  ;; elements (#676)
  (let* ((s (hunk-start x))
         (l (length (hunk-ins x)))
         (e (max (hunk-end x) (hunk-end y)))
         (tabs (word-tables vo)))
    (and (== s (hunk-start y))
         (> l 0)
         (== l (length (hunk-ins y)))
         (== l (min (- (hunk-end x) s) (- (hunk-end y) s)))
         (let loop ((i 0) (lx (hunk-ins x)) (ly (hunk-ins y)))
           (or (>= i l)
               (and (same-kind? tag (vector-ref vo (+ s i)) (car lx) (car ly))
                    (or (== e (+ s 1))
                        (and (closest? (car lx) (+ s i) s e tabs)
                             (closest? (car ly) (+ s i) s e tabs)))
                    (loop (+ i 1) (cdr lx) (cdr ly))))))))

(define (positional? tag o a b)
  ;; Can the chunks @o, @a and @b (of the same length) be merged element
  ;; by element?  Each element is either unchanged on a side, or replaced
  ;; by one of the same kind which is closer to it than to the others
  ;; (a paragraph removed and another inserted are not a change, #676)
  (let* ((vo (list->vector o))
         (n (vector-length vo))
         (tabs (word-tables vo))
         (ok? (lambda (i oi z)
                (or (== z oi)
                    (and (same-kind? tag oi z z)
                         (or (== n 1) (closest? z i 0 n tabs)))))))
    (let loop ((i 0) (o o) (a a) (b b))
      (or (null? o)
          (and (ok? i (car o) (car a))
               (ok? i (car o) (car b))
               (loop (+ i 1) (cdr o) (cdr a) (cdr b)))))))

(define (merge-group tag vo gs ge g)
  ;; Merge a group of overlapping hunks, from @gs to @ge
  (let* ((ha (list-filter g (lambda (h) (== (hunk-side h) 'a))))
         (hb (list-filter g (lambda (h) (== (hunk-side h) 'b))))
         (sa (side-span vo gs ge ha))
         (sb (side-span vo gs ge hb))
         (one? (and (== (length ha) 1) (== (length hb) 1)))
         (x (and one? (car ha)))
         (y (and one? (car hb))))
    (cond ((null? hb) sa)
          ((null? ha) sb)
          ((== sa sb) sa)
          ((and one? (same-insertion? x y)) sb)
          ((and one? (same-insertion? y x)) sa)
          ((and one? (== (hunk-ins x) (hunk-ins y)) (contains? x y))
           ;; the same change, and more removals on one side
           sa)
          ((and one? (== (hunk-ins x) (hunk-ins y)) (contains? y x))
           sb)
          ((and one? (paired-changes tag x y vo))
           ;; both sides changed the same elements (of the same kind) and
           ;; one of them removed more: the elements are merged in turn
           (with l (length (hunk-ins x))
             (map merge-versions
                  (subvector->list vo (hunk-start x) (+ (hunk-start x) l))
                  (hunk-ins x) (hunk-ins y))))
          (else
            ;; a conflict, shown as the differences between ours and theirs
            (with d (compare-versions (version-normalize (cons tag sa))
                                      (version-normalize (cons tag sb)))
              (if (tm-is? d tag) (cdr d) (list d)))))))

(define (merge-hunks tag o ha hb)
  (let* ((vo (list->vector o))
         (n (vector-length vo)))
    (let loop ((l (merge-hunk-lists ha hb)) (pos 0) (acc '()))
      (if (null? l)
          (append (reverse acc) (subvector->list vo pos n))
          (let group ((g (list (car l))) (ge (hunk-end (car l))) (r (cdr l)))
            (if (and (pair? r) (hunk-joins? (car r) g ge))
                (group (cons (car r) g) (max ge (hunk-end (car r))) (cdr r))
                (let* ((gs (hunk-start (car l)))
                       (m (merge-group tag vo gs ge (reverse g))))
                  (loop r ge (append (reverse m)
                                     (reverse (subvector->list vo pos gs))
                                     acc)))))))))

(define (merge-chunk tag o a b)
  ;; Merge the unstable chunks o, a and b (lists of children of tag)
  (cond ((== a b) a)
        ((== o a) b)
        ((== o b) a)
        ((and (== (length o) (length a)) (== (length o) (length b))
              (positional? tag o a b))
         ;; Replacements of elements: merge them one by one
         ;; NOTE: single words of a concatenation are not merged any further
         (map merge-versions o a b))
        (else
          ;; The changes of each side, against the ancestor (#676)
          (merge-hunks tag o (chunk-hunks o a 'a) (chunk-hunks o b 'b)))))

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
       (nin? (car o) '(graphics))))

(define (merge-tformat o a b)
  ;; Tables are merged cell by cell; the formatting is taken from the side
  ;; which changed it, and is never merged (version markup would break it)
  (let* ((fo (cDr (cdr o)))
         (fa (cDr (cdr a)))
         (fb (cDr (cdr b)))
         (f (cond ((== fa fb) fa) ((== fo fa) fb) ((== fo fb) fa) (else #f))))
    (if (not f)
        (compare-versions a b)
        `(tformat ,@f ,(merge-versions (cAr o) (cAr a) (cAr b))))))

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
        ((and (tm-is? o 'tformat) (tm-is? a 'tformat) (tm-is? b 'tformat))
         (merge-tformat o a b))
        ((same-shape? o a b)
         (cons (car o) (map merge-versions (cdr o) (cdr a) (cdr b))))
        (else (compare-versions a b))))

(tm-define (merge-conflicts t)
  (:synopsis "Number of conflicts which remain in the merged document @t")
  (cond ((npair? t) 0)
        ((tm-in? t '(version-both version-old version-new)) 1)
        (else (apply + (map merge-conflicts (cdr t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bodies of documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (document-body doc)
  (:synopsis "The body of the complete document @doc (an stree), or #f")
  (and (tm-is? doc 'document)
       (and-with b (list-find (cdr doc) (cut tm-func? <> 'body 1))
         (cadr b))))

(tm-define (document-set-body doc body)
  (:synopsis "The complete document @doc with its body replaced by @body")
  (cons 'document
        (map (lambda (x) (if (tm-func? x 'body 1) `(body ,body) x))
             (cdr doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete documents: the body, the style and the initial environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-value o a b)
  ;; Three way merge of values (#f: absent): (value . conflict?)
  (cond ((== a b) (cons a #f))
        ((== o a) (cons b #f))
        ((== o b) (cons a #f))
        (else (cons a #t))))

(define (document-part doc tag)
  (list-find (cdr doc) (cut tm-is? <> tag)))

(define (style-list doc)
  ;; The style and the packages of @doc, as a list of strings
  (with s (document-part doc 'style)
    (cond ((not (and s (pair? (cdr s)))) '())
          ((string? (cadr s)) (list (cadr s)))
          ((tm-is? (cadr s) 'tuple) (list-filter (cdadr s) string?))
          (else '()))))

(define (merge-styles o a b)
  ;; The main style is merged as a value, the packages as a set:
  ;; (styles . conflicts), the conflicts being '() or '("style")
  ;; NOTE: our order of the packages is kept, theirs are added at the end
  (let* ((main (merge-value (and (pair? o) (car o))
                            (and (pair? a) (car a))
                            (and (pair? b) (car b))))
         (po (if (pair? o) (cdr o) '()))
         (pa (if (pair? a) (cdr a) '()))
         (pb (if (pair? b) (cdr b) '()))
         (removed (list-filter po (lambda (x) (or (nin? x pa) (nin? x pb)))))
         (added (list-filter pb (lambda (x) (and (nin? x po) (nin? x pa)))))
         (packs (list-filter (append pa added)
                             (lambda (x) (and (nin? x removed)
                                              (!= x (car main)))))))
    (cons (if (car main) (cons (car main) packs) packs)
          (if (cdr main) '("style") '()))))

(define (initial-list doc)
  ;; The initial environment of @doc, as a list of (key value)
  (with i (document-part doc 'initial)
    (if (and i (tm-func? i 'initial 1) (tm-is? (cadr i) 'collection))
        (map cdr (list-filter (cdadr i) (cut tm-func? <> 'associate 2)))
        '())))

(define (merge-initials o a b)
  ;; Key by key: (environment . conflicting keys)
  (let* ((keys (list-remove-duplicates (map car (append a b))))
         (val (lambda (l k) (and-with x (assoc k l) (cadr x))))
         (l (map (lambda (k) (cons k (merge-value (val o k) (val a k)
                                                  (val b k))))
                 keys)))
    (cons (map (lambda (x) (list (car x) (cadr x)))
               (list-filter l cadr))
          (map car (list-filter l cddr)))))

(define (document-set-part doc tag new after?)
  ;; Replace the part @tag of @doc by @new (#f: remove it); a new part is
  ;; put before the body, or after it if @after?
  (cond ((document-part doc tag)
         (cons 'document
               (apply append
                      (map (lambda (x)
                             (cond ((not (tm-is? x tag)) (list x))
                                   (new (list new))
                                   (else '())))
                           (cdr doc)))))
        ((not new) doc)
        (else
          (cons 'document
                (apply append
                       (map (lambda (x)
                              (cond ((not (tm-func? x 'body 1)) (list x))
                                    (after? (list x new))
                                    (else (list new x))))
                            (cdr doc)))))))

(tm-define (merge-documents o a b)
  (:synopsis "Three way merge of the complete documents @a and @b")
  ;; @o, @a, @b: strees of complete documents, @o being the common ancestor.
  ;; The result is (merged-document conflicts kept): the number of
  ;; conflicts, and the parts of the preamble for which ours was kept
  ;; ("style" for the main style, the keys of the initial environment).
  ;; The body is merged as by merge-versions, the style packages as a set,
  ;; and the initial environment key by key; a conflicting change of the
  ;; preamble keeps our version (@a) and counts as one conflict.  The other
  ;; parts (references, auxiliary data...) are ours.
  (let* ((body (merge-versions (document-body o) (document-body a)
                               (document-body b)))
         (sa (style-list a))
         (st (merge-styles (style-list o) sa (style-list b)))
         (ia (initial-list a))
         (in (merge-initials (initial-list o) ia (initial-list b)))
         (doc (document-set-body a body))
         (doc (if (== (car st) sa) doc
                  (document-set-part
                   doc 'style
                   (list 'style (if (== (length (car st)) 1)
                                    (car (car st))
                                    (cons 'tuple (car st))))
                   #f)))
         (doc (if (== (car in) ia) doc
                  (document-set-part
                   doc 'initial
                   (and (nnull? (car in))
                        `(initial (collection
                                    ,@(map (cut cons 'associate <>)
                                           (car in)))))
                   #t))))
    (with kept (append (cdr st) (cdr in))
      (list doc (+ (merge-conflicts body) (length kept)) kept))))
