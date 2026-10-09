
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-cite.scm
;; DESCRIPTION : citations of several items for the CSL processor
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A citation is a list of cites. A cite is an association list with the
;; key id and, optionally,
;;
;;   locator, label     "99" and "page", "chapter"...
;;   prefix, suffix     rich text around the cite
;;   suppress-author    the cite without its names: (1984)
;;   author-only        the names alone: Knuth
;;   position           first, subsequent, ibid or ibid-with-locator
;;   near-note, first-note
;;
;; The cites are sorted, the cites of the same authors are brought together
;; and collapsed as the style asks: "Doe 2000, 2001", "Doe 2000a, b",
;; "[1-3]".

(texmacs-module (csl csl-cite)
  (:use (csl csl-utils) (csl csl-style) (csl csl-data) (csl csl-render)
        (csl csl-process)))

(define en-dash (utf8->cork "–"))

;; a rendered cite is a vector #(cite text author)
(define (r-cite r) (vector-ref r 0))
(define (r-text r) (vector-ref r 1))
(define (r-author r) (vector-ref r 2))

(define (plain? cite)
  ;; can the cite be merged with its neighbours?
  (not (or (csl-cite-ref cite 'locator) (csl-cite-ref cite 'prefix)
           (csl-cite-ref cite 'suffix) (csl-cite-ref cite 'author-only))))

(define (family-name name)
  (let* ((part (lambda (k) (csl-name-ref name k)))
         (ndp (part 'non-dropping-particle)))
    (or (part 'literal)
        (if (and ndp (part 'family)) (rt-cat ndp " " (part 'family))
            (part 'family)))))

(define (default-author proc id)
  ;; the names of an item whose cite shows none, as in a numeric style
  (let* ((item (csl-processor-item proc id))
         (locale (csl-processor-locale proc))
         (names (or (and item
                         (list-find (map (cut csl-item-ref item <>)
                                         '("author" "editor" "translator"))
                                    (lambda (l) (and (pair? l)
                                                     (pair? (car l))))))
                    '()))
         (l (map family-name names)))
    (cond ((null? l) #f)
          ((null? (cdr l)) (car l))
          ((null? (cddr l))
           (rt-cat (car l) " " (or (csl-term locale "and" #f #f) "and") " "
                   (cadr l)))
          (else (rt-cat (car l) " "
                        (or (csl-term locale "et-al" #f #f) "et al."))))))

(define (cite-author proc cite p)
  ;; @p is the result of csl-render-cite
  (if (rt-empty? (cdr p)) (default-author proc (csl-cite-ref cite 'id))
      (cdr p)))

(define (render proc cite)
  (let* ((p (csl-render-cite proc cite))
         (text (if (csl-cite-ref cite 'author-only) (cite-author proc cite p)
                   (car p))))
    (vector cite text (rt->string (cdr p)))))

(define (with-affixes r text)
  (rt-cat (csl-cite-ref (r-cite r) 'prefix) text
          (csl-cite-ref (r-cite r) 'suffix)))

(define (shown r)
  (with-affixes r (r-text r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ranges of numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cite-number proc r)
  (and (plain? (r-cite r))
       (csl-processor-number proc (csl-cite-ref (r-cite r) 'id))))

(define (number-runs proc l)
  ;; the rendered cites @l as a list of runs of consecutive numbers
  (let loop ((l l) (run '()) (r '()))
    (define (flush) (if (null? run) r (cons (reverse run) r)))
    (cond ((null? l) (reverse (flush)))
          ((and (nnull? run) (cite-number proc (car l))
                (cite-number proc (car run))
                (== (cite-number proc (car l))
                    (+ (cite-number proc (car run)) 1)))
           (loop (cdr l) (cons (car l) run) r))
          (else (loop (cdr l) (list (car l)) (flush))))))

(define (collapse-numbers proc l delim)
  (rt-join
   (append-map
    (lambda (run)
      (if (>= (length run) 3)
          (list (rt-cat (shown (car run)) en-dash (shown (cAr run))))
          (map shown run)))
    (number-runs proc l))
   delim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups of cites with the same names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (name-groups l)
  ;; the cites with the same names join the first of them
  (let loop ((l l) (r '()))
    (if (null? l) (reverse (map reverse r))
        (let* ((a (r-author (car l)))
               (g (and (!= a "")
                       (not (csl-cite-ref (r-cite (car l)) 'author-only))
                       (list-find r (lambda (g) (== (r-author (car g)) a))))))
          (if g
              (loop (cdr l) (map (lambda (x) (if (eq? x g) (cons (car l) x) x))
                                 r))
              (loop (cdr l) (cons (list (car l)) r)))))))

(define (without-author proc r)
  (car (csl-render-cite proc (cons '(suppress-author . #t) (r-cite r)))))

(define (without-suffix proc r)
  (rt->string
   (car (csl-render-cite proc (cons* '(suppress-author . #t)
                                     '(no-year-suffix . #t) (r-cite r))))))

(define (suffix-of proc r)
  (and (plain? (r-cite r))
       (csl-processor-year-suffix proc (csl-cite-ref (r-cite r) 'id))))

(define (suffix-runs l)
  ;; runs of consecutive suffixes: ("a" "b" "c" "e") -> (("a" "b" "c") ("e"))
  (let loop ((l l) (run '()) (r '()))
    (define (flush) (if (null? run) r (cons (reverse run) r)))
    (define (next? a b)
      (and (== (string-length a) 1) (== (string-length b) 1)
           (== (char->integer (string-ref b 0))
               (+ (char->integer (string-ref a 0)) 1))))
    (cond ((null? l) (reverse (flush)))
          ((and (nnull? run) (next? (car run) (car l)))
           (loop (cdr l) (cons (car l) run) r))
          (else (loop (cdr l) (list (car l)) (flush))))))

(define (collapse-years proc g how group-delim suffix-delim)
  ;; the group @g of cites with the same names: the names once, then the
  ;; years; with @how, the years once, then the suffixes
  ;; @suffixes holds the suffixes of the cites of one year, the last one
  ;; first; the cite of the first of them is the last of @parts
  (let loop ((l (cdr g)) (parts (list (shown (car g))))
             (base (and how (suffix-of proc (car g))
                        (without-suffix proc (car g))))
             (suffixes (if (and how (suffix-of proc (car g)))
                           (list (suffix-of proc (car g))) '())))
    (define (flush)
      (if (or (null? suffixes) (null? (cdr suffixes))) parts
          (let* ((all (reverse suffixes))
                 (ranged? (== how "year-suffix-ranged"))
                 (runs (if ranged? (suffix-runs all) (map list all)))
                 (first (car runs))
                 (range? (>= (length first) 3))
                 (rest (if range? (cdr runs)
                           (append (map list (cdr first)) (cdr runs)))))
            (cons (rt-join
                   (cons (if range?
                             (rt-cat (car parts) en-dash (cAr first))
                             (car parts))
                         (map (lambda (run)
                                (if (>= (length run) 3)
                                    (rt-cat (car run) en-dash (cAr run))
                                    (rt-join run suffix-delim)))
                              rest))
                   suffix-delim)
                  (cdr parts)))))
    (cond ((null? l) (rt-join (reverse (flush)) group-delim))
          ((and how base (suffix-of proc (car l))
                (== (without-suffix proc (car l)) base))
           (loop (cdr l) parts base (cons (suffix-of proc (car l)) suffixes)))
          (else
            (with new (and how (suffix-of proc (car l)))
              (loop (cdr l)
                    (cons (with-affixes (car l)
                                        (without-author proc (car l)))
                          (flush))
                    (and new (without-suffix proc (car l)))
                    (if new (list new) '())))))))

(define (collapse-groups proc l area layout)
  (let* ((collapse (csl-attr area 'collapse))
         (delim (csl-attr layout 'delimiter ""))
         (group-delim (csl-attr area 'cite-group-delimiter ", "))
         (suffix-delim (csl-attr area 'year-suffix-delimiter delim))
         (after (csl-attr area 'after-collapse-delimiter delim))
         (how (and (in? collapse '("year-suffix" "year-suffix-ranged"))
                   collapse))
         (groups (name-groups l)))
    (let loop ((gs groups) (r #f) (last-collapsed? #f))
      (if (null? gs) r
          (let* ((g (car gs))
                 (plain? (list-and (map (lambda (c) (plain? (r-cite c))) g)))
                 (collapsed? (and collapse (> (length g) 1)))
                 (x (if collapsed?
                        (collapse-years proc g how
                                        (if plain? group-delim after)
                                        suffix-delim)
                        (rt-join (map shown g) delim))))
            (loop (cdr gs)
                  (rt-cat r (and r (if last-collapsed? after delim)) x)
                  collapsed?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (csl-citation proc cites . opt)
  (:synopsis "The rich text for the citation of the list @cites")
  ;; with the optional argument #t, without the affixes of the style
  (let* ((style (csl-processor-ref proc 'style))
         (area (csl-style-ref style 'citation))
         (layout (csl-child area 'layout))
         (collapse (csl-attr area 'collapse))
         (delim (csl-attr layout 'delimiter ""))
         (l (map (cut render proc <>) (csl-sort-cites proc cites)))
         (ctx (csl-make-context style (csl-processor-locale proc)
                                (csl-make-item "" "") 'citation))
         (bare? (or (and (nnull? opt) (car opt))
                    (and (nnull? cites)
                         (list-and (map (lambda (c)
                                          (csl-cite-ref c 'author-only))
                                        cites))))))
    (with x (cond ((== collapse "citation-number")
                   (collapse-numbers proc l delim))
                  ((or (in? collapse '("year" "year-suffix"
                                       "year-suffix-ranged"))
                       (csl-attr area 'cite-group-delimiter))
                   (collapse-groups proc l area layout))
                  (else (rt-join (map shown l) delim)))
      (cond ((and (nnull? l) (rt-empty? x))
             "[CSL STYLE ERROR: reference with no printed form.]")
            ;; names in the text come without the brackets of the style
            (bare? x)
            (else (csl-finish-layout layout ctx x))))))

(define (author-groups proc cites)
  ;; the consecutive cites with the same names: ((author cite ...) ...)
  (let loop ((l cites) (r '()))
    (if (null? l) (reverse (map (lambda (g) (cons (car g) (reverse (cdr g))))
                                r))
        (let* ((a (cite-author proc (car l) (csl-render-cite proc (car l))))
               (key (rt->string a)))
          (if (and (nnull? r) (!= key "") (== (rt->string (caar r)) key))
              (loop (cdr l) (cons (cons* (caar r) (car l) (cdar r)) (cdr r)))
              (loop (cdr l) (cons (list a (car l)) r)))))))

(tm-define (csl-textual-parts proc cites)
  (:synopsis "The authors and the rest of the citation of @cites in a text")
  ;; a list of (authors . citation without the authors)
  (map (lambda (g)
         (cons (car g)
               (csl-citation proc
                             (map (lambda (c)
                                    (cons '(suppress-author . #t) c))
                                  (cdr g)))))
       (author-groups proc cites)))

(tm-define (csl-textual-citation proc cites)
  (:synopsis "The citation of @cites as part of a sentence: Knuth (1984)")
  (rt-join (map (lambda (p) (rt-join (list (car p) (cdr p)) " "))
                (csl-textual-parts proc cites))
           ", "))
