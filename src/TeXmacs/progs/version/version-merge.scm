
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

;; The chunks between two elements kept by both sides are merged following
;; the rules of src/docs/git-features.md, section 5a (#676).  The principle:
;; a silent wrong merge is much worse than a conflict, so that a chunk only
;; merges cleanly when each change can be attributed with certainty.

(define (chunk-conflict tag a b)
  ;; A conflict, shown as the differences between ours and theirs
  (with d (compare-versions (version-normalize (cons tag a))
                            (version-normalize (cons tag b)))
    (if (tm-is? d tag) (cdr d) (list d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When two elements are the same paragraph: the words which they share
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stop-words
  (with h (make-ahash-table)
    (for-each
     (lambda (w) (ahash-set! h w #t))
     '("a" "about" "after" "all" "also" "an" "and" "any" "are" "as" "at" "be"
       "because" "been" "before" "between" "both" "but" "by" "can" "could"
       "do" "does" "each" "for" "from" "had" "has" "have" "he" "hence" "her"
       "here" "his" "how" "however" "i" "if" "in" "into" "is" "it" "its"
       "may" "more" "most" "must" "no" "not" "now" "of" "on" "one" "only"
       "or" "other" "our" "over" "same" "she" "should" "since" "so" "some"
       "such" "than" "that" "the" "their" "them" "then" "there" "these"
       "they" "this" "those" "thus" "to" "two" "under" "very" "was" "we"
       "were" "what" "when" "where" "which" "while" "who" "why" "will"
       "with" "would" "you" "your"
       ;; the most frequent ones of a few other languages
       "au" "aux" "avec" "ce" "cette" "dans" "de" "des" "du" "elle" "en"
       "est" "et" "il" "la" "le" "les" "leur" "mais" "nous" "ou" "par" "pas"
       "pour" "que" "qui" "sont" "sur" "un" "une" "vous"
       "che" "con" "del" "della" "di" "e" "gli" "lo" "per" "si" "una"
       "das" "dem" "den" "der" "die" "ein" "eine" "ist" "mit" "nicht" "und"
       "von" "el" "los" "las" "y"))
    h))

(define min-word-length 4)

(define punctuation
  ;; The bytes which are not part of a word at its ends
  (with v (make-vector 256 #f)
    (for-each (lambda (c) (vector-set! v (char->integer c) #t))
              (string->list ".,;:!?()[]{}'\"`"))
    v))

(define (punctuation? s i)
  (with c (char->integer (string-ref s i))
    (and (< c 256) (vector-ref punctuation c))))

(define (word-key* w)
  (let* ((n (string-length w))
         (s (let loop ((i 0))
              (if (and (< i n) (punctuation? w i)) (loop (+ i 1)) i)))
         (e (let loop ((i n))
              (if (and (> i s) (punctuation? w (- i 1))) (loop (- i 1)) i))))
    (locase-all (if (and (== s 0) (== e n)) w (substring w s e)))))

;; The tables of a merge: the keys of the words seen, and when a key has
;; last been seen, so that no table has to be made for each element
(define key-table (make-ahash-table))
(define seen-table (make-ahash-table))
(define seen-stamp 0)

(define (reset-key-tables)
  (set! key-table (make-ahash-table))
  (set! seen-table (make-ahash-table)))

(define (word-key w)
  ;; The word @w in lower case, without the punctuation at its ends
  (or (ahash-ref key-table w)
      (with k (word-key* w)
        (ahash-set! key-table w k)
        k)))

(define (for-each-word t f)
  (cond ((string? t)
         (for-each (lambda (w) (f (word-key w))) (string-decompose t " ")))
        ((pair? t) (for-each (lambda (x) (for-each-word x f)) (cdr t)))))

(define (tree-keys-if t pred?)
  ;; The words of @t which satisfy @pred?, each one once
  (set! seen-stamp (+ seen-stamp 1))
  (let ((stamp seen-stamp) (acc '()))
    (for-each-word t
      (lambda (k)
        (when (and (pred? k) (!= (ahash-ref seen-table k) stamp)
                   (not (ahash-ref stop-words k)))
          (ahash-set! seen-table k stamp)
          (set! acc (cons k acc)))))
    acc))

(define (tree-keys t)
  ;; The real words of @t, each one once: neither short nor stop words.  A
  ;; text without any real word (such as "Let x+y be.") has only short ones,
  ;; which then count, for this text and for those compared with it
  (with real (tree-keys-if t (lambda (k) (>= (string-length k) min-word-length)))
    (if (null? real) (all-keys t) real)))

(define (all-keys t)
  (tree-keys-if t (lambda (k) (!= k ""))))

(define (short-keys? keys)
  (and (pair? keys) (< (string-length (car keys)) min-word-length)))

(define (same-kind? o z)
  ;; Two elements of the same kind: the same tag, or both are text
  (or (and (textual? o) (textual? z))
      (and (pair? o) (pair? z) (== (car o) (car z)))))

(define (keys-table keys)
  (with h (make-ahash-table)
    (for-each (lambda (w) (ahash-set! h w #t)) keys)
    h))

(define (shared-count keys h)
  (let loop ((l keys) (c 0))
    (cond ((null? l) c)
          ((ahash-ref h (car l)) (loop (cdr l) (+ c 1)))
          (else (loop (cdr l) c)))))

(define (enough-shared? c n1 n2)
  ;; At least half of the words of the smaller set are shared
  (with n (min n1 n2)
    (and (> n 0) (>= (* 2 c) n))))

(define (near-identical? x y)
  ;; Are the elements @x and @y the same paragraph?
  (or (== x y)
      (and (same-kind? x y)
           (let* ((kx0 (tree-keys x))
                  (ky0 (tree-keys y))
                  (short? (or (short-keys? kx0) (short-keys? ky0)))
                  (kx (if short? (all-keys x) kx0))
                  (ky (if short? (all-keys y) ky0)))
             (enough-shared? (shared-count kx (keys-table ky))
                             (length kx) (length ky))))))

;; The words of the elements are indexed by packets of this many elements,
;; made when needed: a new element is compared with the elements near its
;; place (at least half a packet on each side), which keeps the pairing
;; linear.  A paragraph which comes from further away is not recognized.
(define pairing-packet 32)

(define (make-pairing v s e)
  ;; For the elements of the vector @v from @s to @e, a function which tells
  ;; to which one a new element @z belongs: (pairing z i) gives 'paired when
  ;; @z surely is the element number @i changed, an index j when it rather
  ;; belongs to the element number j, and #f otherwise.  With i = #f, it
  ;; compares @z with all the elements, and gives one which is similar or #f.
  (let* ((n (- e s))
         (sizes (make-vector n 0))
         (shorts (make-vector n #f))
         (counts (make-vector n 0))
         (packets (make-vector (+ (quotient n pairing-packet) 1) #f)))
    (define (packet p)
      ;; word -> elements of the packet number @p which have this word
      (or (vector-ref packets p)
          (with h (make-ahash-table)
            (do ((k (* p pairing-packet) (+ k 1)))
                ((or (>= k n) (>= k (* (+ p 1) pairing-packet))))
              (with keys (tree-keys (vector-ref v (+ s k)))
                (vector-set! sizes k (length keys))
                (vector-set! shorts k (short-keys? keys))
                (for-each (lambda (w)
                            (ahash-set! h w (cons k (or (ahash-ref h w) '()))))
                          keys)))
            (vector-set! packets p h)
            h)))
    (define (packets-near own)
      (if (not own)
          (let loop ((p (quotient (- n 1) pairing-packet)) (acc '()))
            (if (< p 0) acc (loop (- p 1) (cons p acc))))
          (let* ((p (quotient own pairing-packet))
                 (r (remainder own pairing-packet))
                 (q (if (< r (quotient pairing-packet 2)) (- p 1) (+ p 1))))
            (if (and (>= q 0) (< (* q pairing-packet) n)) (list p q) (list p)))))
    (lambda (z i)
      (let* ((own (and i (- i s)))
             (tabs (map packet (packets-near own)))
             (keys (if (and own (vector-ref shorts own))
                       (all-keys z)
                       (tree-keys z)))
             (nz (length keys))
             (touched '()))
        (for-each
         (lambda (w)
           (for-each
            (lambda (h)
              (for-each
               (lambda (k)
                 (when (== (vector-ref counts k) 0)
                   (set! touched (cons k touched)))
                 (vector-set! counts k (+ (vector-ref counts k) 1)))
               (or (ahash-ref h w) '())))
            tabs))
         keys)
        (let* ((c (if own (vector-ref counts own) 0))
               (sim? (and own
                          (enough-shared? c nz (vector-ref sizes own))
                          (same-kind? (vector-ref v i) z))))
          (let loop ((l touched) (top 0) (best #f) (bestc 0))
            (if (pair? l)
                (let* ((k (car l))
                       (ck (vector-ref counts k))
                       (ok? (and (!= k own)
                                 (enough-shared? ck nz (vector-ref sizes k))
                                 (same-kind? (vector-ref v (+ s k)) z))))
                  (vector-set! counts k 0)
                  (cond ((== k own) (loop (cdr l) top best bestc))
                        ((and ok? (> ck bestc)) (loop (cdr l) (max top ck) k ck))
                        (else (loop (cdr l) (max top ck) best bestc))))
                (cond ((and sim? (> c top)) 'paired)
                      ((and best (or (not sim?) (> bestc c))) (+ s best))
                      (else #f)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The changes of each side, against the ancestor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A side changes the ancestor by hunks (start end ins side): the elements
;; from start to end (excluded) are replaced by the elements ins; for a pure
;; insertion, start = end.

(define (hunk-start h) (car h))
(define (hunk-end h) (cadr h))
(define (hunk-ins h) (caddr h))
(define (hunk-side h) (cadddr h))
(define (hunk-insertion? h) (== (hunk-start h) (hunk-end h)))
(define (hunk-size h) (- (hunk-end h) (hunk-start h)))
(define (hunk-one-for-one? h) (== (hunk-size h) (length (hunk-ins h))))

(define (chunk-hunks vx m io eo ix ex side)
  ;; The hunks of the chunk from @io to @eo of the ancestor, which is the
  ;; chunk from @ix to @ex of the version @vx, @m being the common
  ;; subsequence of the ancestor and of this version
  (let loop ((i io) (j ix) (s io) (acc '()))
    (if (or (>= i eo) (vector-ref m i))
        (let* ((k (if (>= i eo) ex (vector-ref m i)))
               (ins (subvector->list vx j k))
               (acc* (if (and (== s i) (null? ins)) acc
                         (cons (list s i ins side) acc))))
          (if (>= i eo)
              (reverse acc*)
              (loop (+ i 1) (+ k 1) (+ i 1) acc*)))
        (loop (+ i 1) j s acc))))

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

(define (hunk-uncertain? vo h)
  ;; Other hunks may give the same version: an element next to the hunk is
  ;; also among its elements (repeated paragraphs), or the hunk brings an
  ;; element of the ancestor (a paragraph moved)
  (let* ((s (hunk-start h))
         (e (hunk-end h))
         (in-hunk? (lambda (x)
                     (or (in? x (hunk-ins h))
                         (in? x (subvector->list vo s e))))))
    (or (and (> s 0) (in-hunk? (vector-ref vo (- s 1))))
        (and (< e (vector-length vo)) (in-hunk? (vector-ref vo e)))
        (with old (vector->list vo)
          (nnot (list-find (hunk-ins h) (cut in? <> old)))))))

(define (certain-edit? vo r i ins)
  ;; Does the hunk @r, next to the insertion of the elements @ins by the
  ;; other side, surely edit its element number @i?  This places @ins.
  (and (hunk-one-for-one? r)
       (let* ((s (hunk-start r))
              (z (list-ref (hunk-ins r) (- i s)))
              (pairing (make-pairing vo s (hunk-end r))))
         (and (== (pairing z i) 'paired)
              (not (list-find ins (cut near-identical? <> z)))))))

(define (touching-joins? vo h m ge)
  ;; The hunks @h and @m of different sides touch at @ge, @m coming first.
  ;; Two replacements stay apart: each one is anchored by the elements kept
  ;; by the other side.  An insertion goes before or after a replacement
  ;; only if the latter surely edits the element next to it.
  (let* ((hi? (hunk-insertion? h))
         (mi? (hunk-insertion? m)))
    (cond ((and hi? mi?) #t)
          ((or (hunk-uncertain? vo h) (hunk-uncertain? vo m)) #t)
          (hi? (not (certain-edit? vo m (- ge 1) (hunk-ins h))))
          (mi? (not (certain-edit? vo h ge (hunk-ins m))))
          (else #f))))

(define (hunk-joins? vo h g ge)
  ;; Does the hunk @h go with the group @g which ends at @ge?
  (or (< (hunk-start h) ge)
      (and (== (hunk-start h) ge)
           (nnot (list-find g (lambda (m)
                                (and (!= (hunk-side m) (hunk-side h))
                                     (== (hunk-end m) ge)
                                     (touching-joins? vo h m ge))))))))

(define (hunk-groups vo l)
  ;; Groups the hunks of the two sides which have to be merged together
  (let loop ((l l) (acc '()))
    (if (null? l)
        (reverse acc)
        (let group ((g (list (car l))) (ge (hunk-end (car l))) (r (cdr l)))
          (if (and (pair? r) (hunk-joins? vo (car r) g ge))
              (group (cons (car r) g) (max ge (hunk-end (car r))) (cdr r))
              (loop r (cons (reverse g) acc)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replacements of the same elements by both sides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-positions tag vo s e sa sb)
  ;; Both sides replace the elements from @s to @e one by one, with the
  ;; elements @sa and @sb: they are merged element by element, and a conflict
  ;; stays local.  A new element closer to another old one shows that the
  ;; positions do not correspond: all elements in between then conflict.
  (let* ((n (- e s))
         (va (list->vector sa))
         (vb (list->vector sb))
         (pairing (make-pairing vo s e))
         (state (make-vector n 'take))
         (far (make-vector n #f)))
    (define (mark-far! k j)
      ;; @k and @j (an index in @vo) do not correspond
      (let* ((j* (- j s))
             (lo (min k j*))
             (hi (max k j*))
             (x (vector-ref far lo)))
        (vector-set! far lo (if x (max x hi) hi))))
    (define (check-neighbour k)
      ;; An element changed by one side only, next to a conflict
      (when (and (>= k 0) (< k n) (== (vector-ref state k) 'take))
        (let* ((oi (vector-ref vo (+ s k)))
               (x (vector-ref va k))
               (y (vector-ref vb k))
               (z (if (== x oi) y x)))
          (when (!= z oi)
            (with st (pairing z (+ s k))
              (when (integer? st) (mark-far! k st)))))))
    (do ((k 0 (+ k 1))) ((>= k n))
      (let* ((oi (vector-ref vo (+ s k)))
             (x (vector-ref va k))
             (y (vector-ref vb k)))
        (when (not (or (== x oi) (== y oi) (== x y)))
          (let* ((sx (pairing x (+ s k)))
                 (sy (pairing y (+ s k))))
            (when (integer? sx) (mark-far! k sx))
            (when (integer? sy) (mark-far! k sy))
            (vector-set! state k
                         (if (and (== sx 'paired) (== sy 'paired))
                             'merge 'conflict))))))
    (do ((k 0 (+ k 1))) ((>= k n))
      (when (== (vector-ref state k) 'conflict)
        (check-neighbour (- k 1))
        (check-neighbour (+ k 1))))
    (let loop ((k 0) (acc '()))
      (cond ((>= k n) (reverse acc))
            ((vector-ref far k)
             ;; the positions up to the last one reached do not correspond
             (let run ((i k) (hi (vector-ref far k)))
               (if (< i hi)
                   (run (+ i 1) (max hi (or (vector-ref far (+ i 1)) 0)))
                   (loop (+ hi 1)
                         (append (reverse
                                  (chunk-conflict tag
                                                  (subvector->list va k (+ hi 1))
                                                  (subvector->list vb k (+ hi 1))))
                                 acc)))))
            (else
              (let* ((oi (vector-ref vo (+ s k)))
                     (x (vector-ref va k))
                     (y (vector-ref vb k)))
                (loop (+ k 1)
                      (with st (vector-ref state k)
                        (cond ((== st 'merge)
                               (cons (merge-versions oi x y) acc))
                              ((== st 'conflict)
                               (append (reverse (chunk-conflict tag (list x)
                                                                (list y)))
                                       acc))
                              ((== x oi) (cons y acc))
                              (else (cons x acc)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Words which touch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (range-strings? v s e)
  (or (>= s e)
      (and (string? (vector-ref v s)) (range-strings? v (+ s 1) e))))

(define (range-changed? vo s e vx ss ee)
  (or (!= (- e s) (- ee ss))
      (let loop ((i s) (j ss))
        (and (< i e)
             (or (!= (vector-ref vo i) (vector-ref vx j))
                 (loop (+ i 1) (+ j 1)))))))

(define (next-stable ma mb j n)
  (cond ((>= j n) n)
        ((and (vector-ref ma j) (vector-ref mb j)) j)
        (else (next-stable ma mb (+ j 1) n))))

(define (words-touch? vo va vb ma mb io j ia ja ib jb)
  ;; Is the element @j only the space between words changed by a side
  ;; before it and by the other side after it?  A change next to markup
  ;; is not concerned (Let <math> be: the formula and the word are apart).
  (and (== (vector-ref vo j) " ")
       (let* ((no (vector-length vo))
              (j2 (next-stable ma mb (+ j 1) no))
              (ja2 (if (< j2 no) (vector-ref ma j2) (vector-length va)))
              (jb2 (if (< j2 no) (vector-ref mb j2) (vector-length vb)))
              (j1 (+ j 1)) (ja1 (+ ja 1)) (jb1 (+ jb 1))
              (la? (range-changed? vo io j va ia ja))
              (lb? (range-changed? vo io j vb ib jb))
              (ra? (range-changed? vo j1 j2 va ja1 ja2))
              (rb? (range-changed? vo j1 j2 vb jb1 jb2)))
         (and (or (and la? rb?) (and lb? ra?))
              (range-strings? vo io j2)
              (range-strings? va ia ja2)
              (range-strings? vb ib jb2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Merging a chunk of blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-elements vo h)
  ;; The elements of the hunk @h which are not edits of an old element
  (if (hunk-one-for-one? h)
      (let* ((s (hunk-start h))
             (pairing (make-pairing vo s (hunk-end h))))
        (let loop ((i s) (l (hunk-ins h)) (acc '()))
          (cond ((null? l) (reverse acc))
                ((== (pairing (car l) i) 'paired) (loop (+ i 1) (cdr l) acc))
                (else (loop (+ i 1) (cdr l) (cons (car l) acc))))))
      (hunk-ins h)))

(define (near-identical-lists? l1 l2)
  ;; Is an element of @l1 the same paragraph as an element of @l2?
  (and (pair? l1) (pair? l2)
       (let* ((v (list->vector l1))
              (pairing (make-pairing v 0 (vector-length v))))
         (nnot (list-find l2 (lambda (z) (pairing z #f)))))))

(define (replacements-overlap? g)
  ;; Are the hunks @g replacements one by one, the next one always beginning
  ;; before the end of the previous ones?
  (let loop ((l (cdr g)) (e (hunk-end (car g))))
    (cond ((not (hunk-one-for-one? (car g))) #f)
          ((null? l) #t)
          ((not (hunk-one-for-one? (car l))) #f)
          ((>= (hunk-start (car l)) e) #f)
          (else (loop (cdr l) (max e (hunk-end (car l))))))))

(define (merge-blocks tag vo va vb ma mb io eo ia ea ib eb a b)
  ;; Merge the unstable chunk from @io to @eo of the ancestor, which has been
  ;; changed by both sides into the chunks @a and @b
  (reset-key-tables)
  (let* ((ha (chunk-hunks va ma io eo ia ea 'a))
         (hb (chunk-hunks vb mb io eo ib eb 'b))
         (groups (hunk-groups vo (merge-hunk-lists ha hb)))
         (new-a '())
         (new-b '()))
    (define (merge-group g)
      ;; The merge of a group of hunks, from @gs to @ge
      (let* ((gs (hunk-start (car g)))
             (ge (apply max (map hunk-end g)))
             (ga (list-filter g (lambda (h) (== (hunk-side h) 'a))))
             (gb (list-filter g (lambda (h) (== (hunk-side h) 'b)))))
        (cond ((null? gb)
               (set! new-a (append (new-elements vo (car ga)) new-a))
               (hunk-ins (car ga)))
              ((null? ga)
               (set! new-b (append (new-elements vo (car gb)) new-b))
               (hunk-ins (car gb)))
              (else
                (let* ((sa (side-span vo gs ge ga))
                       (sb (side-span vo gs ge gb)))
                  (cond ((== sa sb) sa)
                        ((replacements-overlap? g)
                         (merge-positions tag vo gs ge sa sb))
                        (else (chunk-conflict tag sa sb))))))))
    (let loop ((l groups) (pos io) (acc '()))
      (if (null? l)
          (if (near-identical-lists? new-a new-b)
              ;; the same paragraph, added by both sides, would be there twice
              (chunk-conflict tag a b)
              (append (reverse acc) (subvector->list vo pos eo)))
          (let* ((g (car l))
                 (gs (hunk-start (car g)))
                 (ge (apply max (map hunk-end g)))
                 (m (merge-group g)))
            (loop (cdr l) ge
                  (append (reverse m)
                          (reverse (subvector->list vo pos gs))
                          acc)))))))

(define (merge-chunk tag vo va vb ma mb io eo ia ea ib eb)
  ;; Merge the unstable chunks from @io to @eo of @vo, from @ia to @ea of @va
  ;; and from @ib to @eb of @vb (children of @tag)
  (let* ((o (subvector->list vo io eo))
         (a (subvector->list va ia ea))
         (b (subvector->list vb ib eb)))
    (cond ((== a b) a)
          ((== o a) b)
          ((== o b) a)
          ((== tag 'concat)
           ;; Changes of both sides without a word in between conflict, even
           ;; if each one is a replacement of a word: only one element with
           ;; markup is merged further
           (if (and (== (length o) 1) (== (length a) 1) (== (length b) 1)
                    (pair? (car a)) (pair? (car b)))
               (map merge-versions o a b)
               (chunk-conflict tag a b)))
          (else (merge-blocks tag vo va vb ma mb io eo ia ea ib eb a b)))))

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
              (append (reverse (merge-chunk tag vo va vb ma mb
                                            io no ia (vector-length va)
                                            ib (vector-length vb)))
                      acc)))
            ((and (vector-ref ma j) (vector-ref mb j)
                  (>= (vector-ref ma j) ia) (>= (vector-ref mb j) ib)
                  (not (and (== tag 'concat)
                            (words-touch? vo va vb ma mb io j ia (vector-ref ma j)
                                          ib (vector-ref mb j)))))
             (let* ((ja (vector-ref ma j))
                    (jb (vector-ref mb j))
                    (chunk (merge-chunk tag vo va vb ma mb
                                        io j ia ja ib jb)))
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
  ;; The style and the packages of @doc, as a list, or #f when @doc has no
  ;; style part.  The packages which are not names (macros...) are kept as
  ;; they are, and compared as a whole (#676)
  (with s (document-part doc 'style)
    (cond ((not s) #f)
          ((not (pair? (cdr s))) '())
          ((tm-is? (cadr s) 'tuple) (cdadr s))
          (else (list (cadr s))))))

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
    (cond ((car main)
           (cons (cons (car main) packs) (if (cdr main) '("style") '())))
          ((null? packs) (cons '() (if (cdr main) '("style") '())))
          ;; NOTE: the main style never comes from the packages
          (else (cons a '("style"))))))

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
         ;; a document without a style part is unchanged against the base
         (so (or (style-list o) '()))
         (sa (or (style-list a) so))
         (st (merge-styles so sa (or (style-list b) so)))
         (ia (initial-list a))
         (in (merge-initials (initial-list o) ia (initial-list b)))
         (doc (document-set-body a body))
         (doc (if (== (car st) sa) doc
                  (document-set-part
                   doc 'style
                   (list 'style (if (and (== (length (car st)) 1)
                                         (string? (car (car st))))
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
