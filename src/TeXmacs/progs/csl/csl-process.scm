
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-process.scm
;; DESCRIPTION : the CSL processor: sorting, bibliographies and citations
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A processor is a hash table with the entries
;;
;;   style, locale
;;   items     a hash table from identifiers to items
;;   order     the identifiers of the items, in the order of the bibliography
;;   numbers   a hash table from identifiers to citation numbers
;;
;; A cite is an association list with the key id and, optionally, locator,
;; label, prefix, suffix, suppress-author, author-only, position and
;; near-note.

(texmacs-module (csl csl-process)
  (:use (csl csl-utils) (csl csl-style) (csl csl-data) (csl csl-render)
        (csl csl-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sort-string s)
  ;; the punctuation is ignored
  (apply string-append
         (list-filter (csl-chars (csl-locase (tmstring-unaccent-all s)))
                      (lambda (c) (or (csl-letter? c) (csl-digit? c)
                                      (== c " "))))))

(define (sort-key-value proc key-node ctx)
  (ahash-set! ctx 'sort? #t)
  (ahash-set! ctx 'sort-names
              (list-filter (csl-attrs key-node)
                           (lambda (p) (in? (car p) '(names-min
                                                      names-use-first
                                                      names-use-last)))))
  (with x (if (csl-attr key-node 'macro)
              (csl-render-macro (csl-attr key-node 'macro) ctx)
              (csl-render-variable (csl-attr key-node 'variable "") ctx))
    (ahash-set! ctx 'sort? #f)
    (with s (rt->string x)
      (and (!= s "") (sort-string s)))))

(define (item-context proc item mode)
  (let* ((ctx (csl-make-context (ahash-ref proc 'style)
                                (ahash-ref proc 'locale) item mode))
         (id (csl-item-id item))
         (nr (ahash-ref (ahash-ref proc 'numbers) id)))
    (ahash-set! ctx 'number (and nr (number->string nr)))
    (ahash-set! ctx 'year-suffix
                (ahash-ref (ahash-ref proc 'year-suffixes) id))
    (ahash-set! ctx 'explicit-year-suffix
                (ahash-ref proc 'explicit-year-suffix))
    ctx))

(define (sort-keys proc area)
  (with s (and area (csl-child area 'sort))
    (if s (csl-children-named s 'key) '())))

(define (compare-keys a b keys)
  ;; is the list of values @a before @b?
  (cond ((null? keys) #f)
        ((== (car a) (car b)) (compare-keys (cdr a) (cdr b) (cdr keys)))
        ((not (car a)) #f)
        ((not (car b)) #t)
        ((csl-attr? (car keys) 'sort "descending")
         (if (string<? (car b) (car a)) #t
             (if (string<? (car a) (car b)) #f
                 (compare-keys (cdr a) (cdr b) (cdr keys)))))
        (else
          (if (string<? (car a) (car b)) #t
              (if (string<? (car b) (car a)) #f
                  (compare-keys (cdr a) (cdr b) (cdr keys)))))))

(define (sort-by-keys proc l keys mode item-of setup)
  ;; sort the list @l, whose elements have the items (item-of x)
  (if (null? keys) l
      (let* ((with-keys
              (map (lambda (x)
                     (with ctx (item-context proc (item-of x) mode)
                       (setup ctx x)
                       (cons (map (lambda (k) (sort-key-value proc k ctx))
                                  keys)
                             x)))
                   l))
             (sorted (csl-sort with-keys
                               (lambda (a b)
                                 (compare-keys (car a) (car b) keys)))))
        (map cdr sorted))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (node-mentions? node var)
  ;; does some cs:text below @node render the variable @var?
  (or (and (== (csl-name node) 'text) (csl-attr? node 'variable var))
      (list-or (map (lambda (c) (and (pair? c) (node-mentions? c var)))
                    (csl-children node)))))

(define (set-numbers! proc ids)
  (with numbers (make-ahash-table)
    (for-each (lambda (id i) (ahash-set! numbers id (+ i 1)))
              ids (iota (length ids)))
    (ahash-set! proc 'numbers numbers)))

(tm-define (csl-make-processor style lang items)
  (:synopsis "A processor for @items, in the order in which they are cited")
  (let* ((proc (make-ahash-table))
         (table (make-ahash-table))
         (ids (map csl-item-id items))
         (bib (csl-style-ref style 'bibliography)))
    (for (item items) (ahash-set! table (csl-item-id item) item))
    (ahash-set! proc 'style style)
    (ahash-set! proc 'locale (csl-locale style lang))
    (ahash-set! proc 'items table)
    (ahash-set! proc 'year-suffixes (make-ahash-table))
    (ahash-set! proc 'explicit-year-suffix
                (node-mentions? (csl-style-ref style 'root) "year-suffix"))
    (ahash-set! proc 'numeric?
                (node-mentions? (csl-style-ref style 'citation)
                                "citation-number"))
    (set-numbers! proc ids)
    (with sorted (sort-by-keys proc items (sort-keys proc bib) 'bibliography
                               identity (lambda (ctx x) (noop)))
      (ahash-set! proc 'order (map csl-item-id sorted))
      (set-numbers! proc (ahash-ref proc 'order)))
    proc))

(tm-define (csl-processor-ref proc key)
  (ahash-ref proc key))

(tm-define (csl-processor-item proc id)
  (ahash-ref (ahash-ref proc 'items) id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibliographies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bibliography-entry proc item)
  ;; returns (id number label body); label is the first field of the entry
  ;; when the style aligns the following ones, or else #f
  (let* ((style (ahash-ref proc 'style))
         (bib (csl-style-ref style 'bibliography))
         (layout (csl-child bib 'layout))
         (ctx (item-context proc item 'bibliography))
         (align (csl-attr bib 'second-field-align))
         (children (csl-children layout))
         (id (csl-item-id item)))
    (if (and align (nnull? children))
        (let* ((first (csl-render-layout (list 'layout '() (car children))
                                         ctx))
               (rest (csl-render-layout (cons* 'layout '() (cdr children))
                                        ctx)))
          (list id (ahash-ref ctx 'number)
                (csl-finish-layout
                 (list 'layout (list-filter (csl-attrs layout)
                                            (lambda (p) (== (car p) 'prefix))))
                 ctx first)
                (csl-finish-layout
                 (list 'layout (list-filter (csl-attrs layout)
                                            (lambda (p) (!= (car p) 'prefix))))
                 ctx rest)))
        (list id (ahash-ref ctx 'number) #f
              (csl-finish-layout layout ctx
                                 (csl-render-layout layout ctx))))))

(tm-define (csl-bibliography proc)
  (:synopsis "The entries (id number label body) of the bibliography")
  (with bib (csl-style-ref (ahash-ref proc 'style) 'bibliography)
    (if (not (and bib (csl-child bib 'layout))) '()
        (map (lambda (id)
               (bibliography-entry proc (csl-processor-item proc id)))
             (ahash-ref proc 'order)))))

(tm-define (csl-bibliography-option proc key . default)
  (with bib (csl-style-ref (ahash-ref proc 'style) 'bibliography)
    (if bib (apply csl-attr (cons* bib key default))
        (and (nnull? default) (car default)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cite-ref cite key)
  (with p (assq key cite)
    (and p (cdr p))))

(define (setup-cite ctx cite)
  (ahash-set! ctx 'locator (cite-ref cite 'locator))
  (ahash-set! ctx 'label (or (cite-ref cite 'label) "page"))
  (ahash-set! ctx 'position (or (cite-ref cite 'position) 'first))
  (ahash-set! ctx 'near-note (cite-ref cite 'near-note))
  (ahash-set! ctx 'first-note (cite-ref cite 'first-note)))

(define (render-cite proc cite layout)
  (let* ((item (csl-processor-item proc (cite-ref cite 'id)))
         (locale (ahash-ref proc 'locale)))
    (if (not item) (rt-fmt '((font-weight . "bold")) (cite-ref cite 'id))
        (with ctx (item-context proc item 'citation)
          (setup-cite ctx cite)
          (rt-cat (cite-ref cite 'prefix)
                  (csl-render-layout layout ctx)
                  (cite-ref cite 'suffix))))))

(tm-define (csl-citation proc cites)
  (:synopsis "The rich text for the citation of the list @cites")
  (let* ((style (ahash-ref proc 'style))
         (area (csl-style-ref style 'citation))
         (layout (csl-child area 'layout))
         (known (list-filter cites
                             (lambda (c)
                               (csl-processor-item proc (cite-ref c 'id)))))
         (unknown (list-filter cites
                               (lambda (c)
                                 (not (csl-processor-item
                                       proc (cite-ref c 'id))))))
         (sorted (sort-by-keys
                  proc known (sort-keys proc area) 'citation
                  (lambda (c) (csl-processor-item proc (cite-ref c 'id)))
                  setup-cite))
         (ctx (csl-make-context style (ahash-ref proc 'locale)
                                (csl-make-item "" "") 'citation)))
    (with l (map (cut render-cite proc <> layout) (append sorted unknown))
      (if (and (nnull? l) (list-and (map rt-empty? l)))
          "[CSL STYLE ERROR: reference with no printed form.]"
          (csl-finish-layout layout ctx
                             (rt-join l (csl-attr layout 'delimiter "")))))))

(tm-define (csl-cite-text proc id)
  (:synopsis "The citation of the item @id alone, without the affixes")
  (let* ((area (csl-style-ref (ahash-ref proc 'style) 'citation))
         (layout (csl-child area 'layout)))
    (render-cite proc (list (cons 'id id)) layout)))

(tm-define (csl-processor-locale proc)
  (ahash-ref proc 'locale))
