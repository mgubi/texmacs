
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
    (ahash-set! ctx 'disambiguate (state-ref proc id 'flag #f))
    (when (== mode 'citation)
      (ahash-set! ctx 'names-add (state-ref proc id 'names-add 0))
      (ahash-set! ctx 'given-levels (state-ref proc id 'levels '()))
      (ahash-set! ctx 'global-levels (ahash-ref proc 'global-levels))
      (ahash-set! ctx 'primary-only? (ahash-ref proc 'primary-only?)))
    ctx))

(define (state-ref proc id key default)
  ;; the state of the disambiguation of the item @id
  (let* ((st (ahash-ref (ahash-ref proc 'states) id))
         (p (and st (assq key st))))
    (if p (cdr p) default)))

(define (state-set! proc id key val)
  (let* ((states (ahash-ref proc 'states))
         (st (or (ahash-ref states id) '())))
    (ahash-set! states id
                (cons (cons key val)
                      (list-filter st (lambda (p) (!= (car p) key)))))))

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
    (ahash-set! proc 'states (make-ahash-table))
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
    (disambiguate! proc)
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
         (ctx (with ctx (item-context proc item 'bibliography)
                ;; subsequent-author-substitute, for the rule complete-all
                (when (in? (csl-attr bib 'subsequent-author-substitute-rule
                                     "complete-all")
                           '("complete-all" "complete-each"))
                  (ahash-set! ctx 'author-substitute
                              (csl-attr bib 'subsequent-author-substitute))
                  (ahash-set! ctx 'author-before
                              (ahash-ref proc 'author-before)))
                ctx))
         (remember (lambda (r)
                     (ahash-set! proc 'author-before
                                 (rt->string (ahash-ref ctx 'author-text)))
                     r))
         (align (csl-attr bib 'second-field-align))
         (children (csl-children layout))
         (id (csl-item-id item)))
    (if (and align (nnull? children))
        (let* ((first (csl-render-layout (list 'layout '() (car children))
                                         ctx))
               (rest (csl-render-layout (cons* 'layout '() (cdr children))
                                        ctx)))
          (remember
           (list id (ahash-ref ctx 'number)
                 (csl-finish-layout
                  (list 'layout (list-filter (csl-attrs layout)
                                             (lambda (p)
                                               (== (car p) 'prefix))))
                  ctx first)
                 (csl-finish-layout
                  (list 'layout (list-filter (csl-attrs layout)
                                             (lambda (p)
                                               (!= (car p) 'prefix))))
                  ctx rest))))
        (with x (csl-render-layout layout ctx)
          (remember
           (list id (ahash-ref ctx 'number) #f
                 (and (not (rt-empty? x))
                      (csl-finish-layout layout ctx x))))))))

(tm-define (csl-bibliography proc)
  (:synopsis "The entries (id number label body) of the bibliography")
  (with bib (csl-style-ref (ahash-ref proc 'style) 'bibliography)
    (if (not (and bib (csl-child bib 'layout))) '()
        (begin
          (ahash-set! proc 'author-before #f)
          ;; an entry with nothing to print is left out
          (list-filter
           (map (lambda (id)
                  (bibliography-entry proc (csl-processor-item proc id)))
                (ahash-ref proc 'order))
           (lambda (e) (or (caddr e) (cadddr e))))))))

(tm-define (csl-bibliography-option proc key . default)
  (with bib (csl-style-ref (ahash-ref proc 'style) 'bibliography)
    (if bib (apply csl-attr (cons* bib key default))
        (and (nnull? default) (car default)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One cite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (csl-cite-ref cite key)
  (with p (assq key cite)
    (and p (cdr p))))

(define (setup-cite ctx cite)
  (ahash-set! ctx 'locator (csl-cite-ref cite 'locator))
  (ahash-set! ctx 'label (or (csl-cite-ref cite 'label) "page"))
  (ahash-set! ctx 'position (or (csl-cite-ref cite 'position) 'first))
  (ahash-set! ctx 'near-note (csl-cite-ref cite 'near-note))
  (ahash-set! ctx 'first-note (csl-cite-ref cite 'first-note))
  (ahash-set! ctx 'suppress-author (csl-cite-ref cite 'suppress-author))
  (when (csl-cite-ref cite 'no-year-suffix)
    (ahash-set! ctx 'year-suffix #f)))

(tm-define (csl-render-cite proc cite)
  (:synopsis "Render one cite: (text . author), without prefix and suffix")
  ;; the author is the rich text of the first names of the cite
  (let* ((item (csl-processor-item proc (csl-cite-ref cite 'id)))
         (area (csl-style-ref (ahash-ref proc 'style) 'citation))
         (layout (csl-child area 'layout)))
    (if (not item)
        (cons (rt-fmt '((font-weight . "bold")) (csl-cite-ref cite 'id)) #f)
        (with ctx (item-context proc item 'citation)
          (setup-cite ctx cite)
          (with x (csl-render-layout layout ctx)
            (cons x (ahash-ref ctx 'author-text)))))))

(tm-define (csl-cite-text proc id)
  (:synopsis "The citation of the item @id alone, without the affixes")
  (car (csl-render-cite proc (list (cons 'id id)))))

(tm-define (csl-sort-cites proc cites)
  (:synopsis "The cites in the order which the style asks for")
  (let* ((area (csl-style-ref (ahash-ref proc 'style) 'citation))
         (known? (lambda (c) (csl-processor-item proc (csl-cite-ref c 'id)))))
    (append
     (sort-by-keys proc (list-filter cites known?) (sort-keys proc area)
                   'citation
                   (lambda (c) (csl-processor-item proc (csl-cite-ref c 'id)))
                   setup-cite)
     (list-filter cites (lambda (c) (not (known? c)))))))

(tm-define (csl-processor-number proc id)
  (ahash-ref (ahash-ref proc 'numbers) id))

(tm-define (csl-processor-year-suffix proc id)
  (ahash-ref (ahash-ref proc 'year-suffixes) id))

(tm-define (csl-processor-locale proc)
  (ahash-ref proc 'locale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disambiguation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Items whose cites read the same are told apart, in this order: by more
;; names and by initials or given names (name after name, kept only when
;; they help), by the condition disambiguate="true" of cs:choose, and by a
;; suffix of the year.

(define (cite-key proc id)
  (rt->string (csl-cite-text proc id)))

(define (classes proc ids)
  ;; the lists of the items of @ids whose cites read the same
  (let loop ((l ids) (r '()))
    (if (null? l) (reverse (map (lambda (c) (reverse (cdr c))) r))
        (let* ((k (cite-key proc (car l)))
               (c (assoc k r)))
          (if c (begin (set-cdr! c (cons (car l) (cdr c))) (loop (cdr l) r))
              (loop (cdr l) (cons (list k (car l)) r)))))))

(define (ambiguous proc ids)
  (list-filter (classes proc ids) (lambda (c) (> (length c) 1))))

(define (item-names proc id)
  ;; the names which the cite of the item can show
  (with item (csl-processor-item proc id)
    (or (list-find (map (cut csl-item-ref item <>)
                        '("author" "editor" "translator"))
                   (lambda (l) (and (pair? l) (pair? (car l)))))
        '())))

(define (snapshot proc ids)
  (map (lambda (id) (cons id (or (ahash-ref (ahash-ref proc 'states) id)
                                 '())))
       ids))

(define (restore! proc snap)
  (for (p snap) (ahash-set! (ahash-ref proc 'states) (car p) (cdr p))))

(define (set-level! proc id pos level)
  (with l (state-ref proc id 'levels '())
    (state-set! proc id 'levels
                (cons (cons pos level)
                      (list-filter l (lambda (p) (!= (car p) pos)))))))

(define (refine! proc ids names? givens?)
  ;; try to tell apart the items @ids, whose cites read the same; returns
  ;; whether some of them could be
  (let* ((snap (snapshot proc ids))
         (longest (apply max (map (lambda (id) (length (item-names proc id)))
                                  ids)))
         (again (lambda (cls)
                  (for (c cls)
                    (when (> (length c) 1) (refine! proc c names? givens?)))
                  #t)))
    (or
     ;; the initials, then the given names, of one name
     (and givens?
          (let loop ((pos 0) (level 1))
            (cond ((>= pos longest) #f)
                  ((> level 2) (loop (+ pos 1) 1))
                  (else
                    (for (id ids) (set-level! proc id pos level))
                    (with cls (classes proc ids)
                      (if (> (length cls) 1)
                          (again cls)
                          (begin
                            (restore! proc snap)
                            (loop pos (+ level 1)))))))))
     ;; one more name
     (and names?
          (list-or (map (lambda (id)
                          (> (length (item-names proc id))
                             (state-ref proc id 'names-add 0)))
                        ids))
          (< (state-ref proc (car ids) 'names-add 0) longest)
          (begin
            (for (id ids)
              (state-set! proc id 'names-add
                          (+ (state-ref proc id 'names-add 0) 1)))
            (with cls (classes proc ids)
              (cond ((> (length cls) 1) (again cls))
                    ((refine! proc ids names? givens?) #t)
                    (else (restore! proc snap) #f))))))))

(define (person-initials name iw)
  (with given (rt->string (csl-name-ref name 'given))
    (csl-initialize given (or iw "") #t #t)))

(define (set-global-levels! proc rule)
  ;; the persons with the same family name are told apart everywhere
  (let* ((table (make-ahash-table))
         (families (make-ahash-table))
         (primary? (string-starts? rule "primary-name"))
         (cap (if (string-ends? rule "with-initials") 1 2))
         (area (csl-style-ref (ahash-ref proc 'style) 'citation))
         (iw (or (and-with n (first-name-node area)
                   (csl-attr n 'initialize-with))
                 (csl-attr area 'initialize-with)
                 (csl-style-option (ahash-ref proc 'style)
                                   'initialize-with))))
    (for (id (ahash-ref proc 'order))
      (with names (item-names proc id)
        (for (name (if (and primary? (nnull? names)) (list (car names))
                       names))
          (let* ((fam (string-append
                       (rt->string (csl-name-ref name 'non-dropping-particle))
                       " " (rt->string (csl-name-ref name 'family))))
                 (old (or (ahash-ref families fam) '())))
            (when (and (!= fam "")
                       (not (list-find old
                                       (lambda (n) (== (csl-name-key n)
                                                       (csl-name-key name))))))
              (ahash-set! families fam (cons name old)))))))
    (for (p (ahash-table->list families))
      (when (> (length (cdr p)) 1)
        (for (name (cdr p))
          (let* ((others (list-filter (cdr p) (lambda (n) (!= n name))))
                 (ini (person-initials name iw))
                 (clash? (list-or (map (lambda (n)
                                         (== (person-initials n iw) ini))
                                       others))))
            (ahash-set! table (csl-name-key name)
                        (cond ((and iw (not clash?)) 1)
                              ((== cap 1) 0)
                              (else 2)))))))
    (ahash-set! proc 'global-levels table)
    (ahash-set! proc 'primary-only? primary?)))

(define (first-name-node node)
  (cond ((not (pair? node)) #f)
        ((== (csl-name node) 'name) node)
        (else (let loop ((l (csl-children node)))
                (cond ((null? l) #f)
                      ((and (pair? (car l)) (first-name-node (car l)))
                       (first-name-node (car l)))
                      (else (loop (cdr l))))))))

(define (suffix-letters i)
  ;; a, b, ... z, aa, ab, ...
  (let loop ((i i) (r ""))
    (with s (string-append (string (integer->char (+ 97 (modulo i 26)))) r)
      (if (< i 26) s (loop (- (quotient i 26) 1) s)))))

(define (node-has-attr? node key)
  (or (csl-attr node key)
      (list-or (map (lambda (c) (and (pair? c) (node-has-attr? c key)))
                    (csl-children node)))))

(define (disambiguate! proc)
  (let* ((style (ahash-ref proc 'style))
         (area (csl-style-ref style 'citation))
         (on? (lambda (key) (csl-attr? area key "true")))
         (ids (ahash-ref proc 'order))
         (rule (csl-attr area 'givenname-disambiguation-rule "by-cite"))
         (label? (node-mentions? area "citation-label")))
    (when (and (on? 'disambiguate-add-givenname) (!= rule "by-cite"))
      (set-global-levels! proc rule))
    (when (or (on? 'disambiguate-add-names)
              (on? 'disambiguate-add-givenname))
      (for (c (ambiguous proc ids))
        (refine! proc c (on? 'disambiguate-add-names)
                 (and (on? 'disambiguate-add-givenname)
                      (== rule "by-cite")))))
    (when (node-has-attr? (csl-style-ref style 'root) 'disambiguate)
      (for (c (ambiguous proc ids))
        (for (id c) (state-set! proc id 'flag #t))))
    (when (or (on? 'disambiguate-add-year-suffix) label?)
      (for (c (ambiguous proc ids))
        (for-each (lambda (id i)
                    (ahash-set! (ahash-ref proc 'year-suffixes) id
                                (suffix-letters i)))
                  c (iota (length c)))))))
