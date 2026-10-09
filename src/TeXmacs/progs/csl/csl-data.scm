
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-data.scm
;; DESCRIPTION : the references of the CSL processor
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An item is a hash table from the names of the CSL variables (strings) to
;;
;;   - rich text for the standard and the number variables;
;;   - a list of names for the name variables; a name is an association
;;     list with the keys family, given, suffix, dropping-particle,
;;     non-dropping-particle, literal (rich text) and comma-suffix,
;;     static-ordering (booleans);
;;   - a date (date start end season circa literal) for the date variables,
;;     where start and end are lists (year month day) of numbers or #f.
;;
;; Items are made from the entries of a BibTeX file, as the parser of
;; TeXmacs gives them, or from CSL-JSON.

(texmacs-module (csl csl-data)
  (:use (csl csl-utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define name-variables
  '("author" "chair" "collection-editor" "compiler" "composer"
    "container-author" "contributor" "curator" "director" "editor"
    "editorial-director" "editor-translator" "executive-producer" "guest"
    "host" "illustrator" "interviewer" "narrator" "organizer"
    "original-author" "performer" "producer" "recipient" "reviewed-author"
    "script-writer" "series-creator" "translator"))

(define date-variables
  '("accessed" "available-date" "container" "event-date" "issued"
    "original-date" "submitted"))

(define number-variables
  '("chapter-number" "citation-number" "collection-number" "edition"
    "first-reference-note-number" "issue" "locator" "number"
    "number-of-pages" "number-of-volumes" "page" "page-first" "part-number"
    "printing-number" "section" "supplement-number" "version" "volume"))

(tm-define (csl-name-variable? var) (in? var name-variables))
(tm-define (csl-date-variable? var) (in? var date-variables))
(tm-define (csl-number-variable? var) (in? var number-variables))

(tm-define (csl-make-item id type)
  (with item (make-ahash-table)
    (ahash-set! item "id" id)
    (ahash-set! item "type" type)
    item))

(tm-define (csl-item-ref item var)
  (ahash-ref item var))

(tm-define (csl-item-set! item var val)
  (ahash-set! item var val))

(tm-define (csl-item-id item) (ahash-ref item "id"))
(tm-define (csl-item-type item) (ahash-ref item "type"))

(tm-define (csl-name-ref name key)
  (with p (assq key name)
    (and p (cdr p))))

(tm-define (csl-make-date start end season circa literal)
  (list 'date start end season circa literal))

(tm-define (csl-date-start d) (list-ref d 1))
(tm-define (csl-date-end d) (list-ref d 2))
(tm-define (csl-date-season d) (list-ref d 3))
(tm-define (csl-date-circa? d) (list-ref d 4))
(tm-define (csl-date-literal d) (list-ref d 5))

(define (label-part name n)
  (let* ((s (rt->string (or (csl-name-ref name 'family)
                            (csl-name-ref name 'literal) "")))
         (l (list-filter (csl-chars s) csl-letter?)))
    (apply string-append (list-head l (min n (length l))))))

(tm-define (csl-citation-label item)
  (:synopsis "A label for @item made from the names and the year")
  (let* ((names (or (list-find (map (cut ahash-ref item <>)
                                    '("author" "editor" "translator"))
                               (lambda (l) (and (pair? l) (pair? (car l)))))
                    '()))
         (n (length names))
         (d (ahash-ref item "issued"))
         (y (and (func? d 'date) (csl-date-start d) (car (csl-date-start d))))
         (year (if y (csl-pad (modulo (abs y) 100) 2) "")))
    (string-append
     (cond ((== n 0) (label-part `((family . ,(rt->string
                                               (or (ahash-ref item "title")
                                                   ""))))
                                 4))
           ((== n 1) (label-part (car names) 4))
           ((<= n 3) (apply string-append
                            (map (cut label-part <> 2) names)))
           (else (apply string-append
                        (map (cut label-part <> 1) (list-head names 4)))))
     year)))

(tm-define (csl-item-english? item default-lang)
  (:synopsis "Is the item @item in English?")
  (with lang (ahash-ref item "language")
    (string-starts? (csl-locase (if (string? lang) lang default-lang)) "en")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (normalize-parts l)
  ;; (y m d) with numbers or #f; the months 13-16 and 21-24 are seasons
  (let* ((nr (lambda (x) (cond ((number? x) x)
                               ((and (string? x) (string->number x))
                                (string->number x))
                               (else #f))))
         (y (and (nnull? l) (nr (car l))))
         (m (and (> (length l) 1) (nr (cadr l))))
         (d (and (> (length l) 2) (nr (caddr l)))))
    (list y (and m (> m 0) m) (and d (> d 0) d))))

(define (with-season parts season)
  ;; a season is written as a month from 21 to 24
  (let* ((y (car parts)) (m (cadr parts)) (d (caddr parts)))
    (cond ((and m (>= m 13) (<= m 16)) (list y (+ m 8) #f))
          ((and (not m) season (>= season 1) (<= season 4))
           (list y (+ season 20) #f))
          (else parts))))

(define (make-date-from-parts l1 l2 season circa literal)
  ;; an end without a year is an open range
  (let* ((start (and l1 (normalize-parts l1)))
         (end (and l2 (normalize-parts l2))))
    (csl-make-date (and start (with-season start season))
                   (and end (if (and (car end) (!= (car end) 0))
                                (with-season end #f)
                                (list #f #f #f)))
                   #f circa literal)))

(define (iso-parts s)
  ;; "2020-05-12" -> ("2020" "05" "12"), also with a leading minus
  (let* ((neg? (string-starts? s "-"))
         (l (string-tokenize-by-char (if neg? (string-drop s 1) s) #\-)))
    (and (nnull? l) (csl-string-number? (car l))
         (list-and (map csl-string-number? l))
         (if neg? (cons (string-append "-" (car l)) (cdr l)) l))))

(tm-define (csl-parse-date s)
  (:synopsis "The date written as @s: ISO dates and ranges, or a literal")
  (let* ((s* (string-trim-both s))
         (l (map string-trim-both (string-tokenize-by-char s* #\/)))
         (p1 (and (nnull? l) (iso-parts (car l))))
         (p2 (and (== (length l) 2) (iso-parts (cadr l)))))
    (cond ((== s* "") #f)
          ((and p1 (or (== (length l) 1) p2))
           (make-date-from-parts p1 p2 #f #f #f))
          (else (csl-make-date #f #f #f #f s*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (particle-word? w)
  (and (!= w "") (csl-lower? w) (csl-letter? (csl-first-char w))))

(define (split-leading-particles family)
  ;; "van der Hoeven" -> ("van der" . "Hoeven")
  (with l (csl-split family)
    (let loop ((l l) (p '()))
      (cond ((or (null? l) (null? (cdr l)) (not (particle-word? (car l))))
             (cons (string-recompose (reverse p) " ")
                   (string-recompose l " ")))
            (else (loop (cdr l) (cons (car l) p)))))))

(define (split-trailing-particles given)
  ;; "Ludwig van" -> ("Ludwig" . "van")
  (with l (reverse (csl-split given))
    (let loop ((l l) (p '()))
      (cond ((or (null? l) (null? (cdr l)) (not (particle-word? (car l))))
             (cons (string-recompose (reverse l) " ")
                   (string-recompose p " ")))
            (else (loop (cdr l) (cons (car l) p)))))))

(define (set-part name key val)
  (if (or (not val) (== val "")) name
      (cons (cons key val)
            (list-filter name (lambda (p) (!= (car p) key))))))

(tm-define (csl-parse-particles name)
  (:synopsis "Find the particles and the suffix inside the parts of @name")
  (let* ((family (csl-name-ref name 'family))
         (given (csl-name-ref name 'given)))
    (when (and (string? family) (> (string-length family) 1)
               (string-starts? family "\"") (string-ends? family "\""))
      (set! name (set-part name 'family
                           (substring family 1
                                      (- (string-length family) 1))))
      (set! family #f))
    (when (and (string? family) (not (csl-name-ref name
                                                   'non-dropping-particle)))
      (with p (split-leading-particles family)
        (when (!= (car p) "")
          (set! name (set-part (set-part name 'family (cdr p))
                               'non-dropping-particle (car p))))))
    (when (and (string? given) (not (csl-name-ref name 'suffix)))
      (with i (string-search-forwards ", " 0 given)
        (when (>= i 0)
          (with suffix (string-drop given (+ i 2))
            (set! given (substring given 0 i))
            (when (string-starts? suffix "!")
              (set! suffix (string-trim-both (string-drop suffix 1)))
              (set! name (set-part name 'comma-suffix #t)))
            (set! name (set-part (set-part name 'given given)
                                 'suffix suffix))))))
    (when (and (string? given) (not (csl-name-ref name 'dropping-particle)))
      (with p (split-trailing-particles given)
        (when (!= (cdr p) "")
          (set! name (set-part (set-part name 'given (car p))
                               'dropping-particle (cdr p))))))
    name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items from CSL-JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (json-attrs t)
  ;; (attr k v ...) -> ((k . v) ...)
  (if (not (func? t 'attr)) '()
      (let loop ((l (cdr t)) (r '()))
        (if (or (null? l) (null? (cdr l))) (reverse r)
            (loop (cddr l) (cons (cons (car l) (cadr l)) r))))))

(define (json-list t)
  (if (func? t 'tuple) (cdr t) '()))

(define markup-tags
  '(("<i>" open (font-style . "italic")) ("</i>" close)
    ("<b>" open (font-weight . "bold")) ("</b>" close)
    ("<sup>" open (vertical-align . "sup")) ("</sup>" close)
    ("<sub>" open (vertical-align . "sub")) ("</sub>" close)
    ("<sc>" open (font-variant . "small-caps")) ("</sc>" close)
    ("<span style=\"font-variant:small-caps;\">" open
     (font-variant . "small-caps"))
    ("<span style=\"font-variant: small-caps;\">" open
     (font-variant . "small-caps"))
    ("<span class=\"nocase\">" open nocase)
    ("</span>" close)))

(define (tag-at s i)
  (list-find markup-tags
             (lambda (t)
               (let* ((tag (car t))
                      (n (string-length tag)))
                 (and (<= (+ i n) (string-length s))
                      (== (substring s i (+ i n)) tag))))))

(define (markup-tokens s)
  ;; the UTF-8 string @s as a list of strings and tags
  (let loop ((i 0) (start 0) (r '()))
    (define (flush)
      (if (== i start) r (cons (utf8->cork (substring s start i)) r)))
    (cond ((>= i (string-length s)) (reverse (flush)))
          ((and (== (string-ref s i) #\<) (tag-at s i))
           (let* ((t (tag-at s i))
                  (j (+ i (string-length (car t)))))
             (with r* (cons (cdr t) (flush))
               (loop j j r*))))
          (else (loop (+ i 1) start r)))))

(define (build-markup toks)
  ;; returns (rich texts . remaining tokens after the closing tag)
  (let loop ((l toks) (r '()))
    (cond ((null? l) (cons (reverse r) '()))
          ((string? (car l)) (loop (cdr l) (cons (car l) r)))
          ((== (caar l) 'close) (cons (reverse r) (cdr l)))
          (else
            (let* ((what (cadar l))
                   (sub (build-markup (cdr l)))
                   (node (if (== what 'nocase)
                             (cons 'nocase (car sub))
                             (cons* 'fmt (list what) (car sub)))))
              (loop (cdr sub) (cons node r)))))))

(tm-define (csl-parse-markup s)
  (:synopsis "The rich text of the UTF-8 string @s with HTML like markup")
  (if (not (string-index s #\<)) (utf8->cork s)
      (rt-cat* (car (build-markup (markup-tokens s))))))

(define (json-name t)
  (let* ((a (json-attrs t))
         (get (lambda (k) (with p (assoc k a)
                            (and p (string? (cdr p)) (!= (cdr p) "")
                                 (utf8->cork (cdr p))))))
         (flag (lambda (k) (with p (assoc k a)
                             (and p (in? (cdr p) '("true" "1"))))))
         (name '()))
    (for (k '("family" "given" "suffix" "dropping-particle"
              "non-dropping-particle" "literal"))
      (set! name (set-part name (string->symbol k) (get k))))
    (when (flag "comma-suffix") (set! name (set-part name 'comma-suffix #t)))
    (when (flag "static-ordering")
      (set! name (set-part name 'static-ordering #t)))
    (if (or (flag "isInstitution") (csl-name-ref name 'literal)) name
        (csl-parse-particles name))))

(define (json-date t)
  (if (string? t) (csl-parse-date (utf8->cork t))
      (let* ((a (json-attrs t))
             (get (lambda (k) (with p (assoc k a) (and p (cdr p)))))
             (parts (map json-list (json-list (or (get "date-parts")
                                                  '(tuple)))))
             (season (and (get "season") (string? (get "season"))
                          (string->number (get "season"))))
             (circa (and (get "circa") (nin? (get "circa")
                                              '("" "0" "false"))))
             (literal (and (string? (get "literal")) (!= (get "literal") "")
                           (utf8->cork (get "literal")))))
        (cond ((and (null? parts) (string? (get "raw")))
               (csl-parse-date (utf8->cork (get "raw"))))
              ((and (null? parts) (not literal)) #f)
              (else
                (make-date-from-parts
                 (and (nnull? parts) (car parts))
                 (and (> (length parts) 1) (cadr parts))
                 season circa literal))))))

(tm-define (csl-json->item t)
  (:synopsis "The item for the CSL-JSON object @t, a tree of json->tree")
  (let* ((a (json-attrs t))
         (get (lambda (k) (with p (assoc k a) (and p (cdr p)))))
         (item (csl-make-item (if (string? (get "id")) (utf8->cork (get "id"))
                                  "")
                              (if (string? (get "type")) (get "type") ""))))
    (for (p a)
      (let* ((k (car p)) (v (cdr p)))
        (cond ((in? k '("id" "type")) (noop))
              ((csl-name-variable? k)
               (with l (map json-name (json-list v))
                 (when (nnull? l) (ahash-set! item k l))))
              ((csl-date-variable? k)
               (with d (json-date v)
                 (when d (ahash-set! item k d))))
              ((string? v)
               (when (!= v "")
                 (ahash-set! item k (csl-parse-markup v))))
              (else (noop)))))
    item))

(tm-define (csl-json->items s)
  (:synopsis "The items of the CSL-JSON document in the string @s")
  (with t (tree->stree (json->tree s))
    (map csl-json->item
         (cond ((func? t 'tuple) (cdr t))
               ((func? t 'attr) (list t))
               (else '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items from BibTeX entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bibtex-types
  '(("article" . "article-journal") ("book" . "book")
    ("booklet" . "pamphlet") ("collection" . "book")
    ("conference" . "paper-conference") ("dataset" . "dataset")
    ("electronic" . "webpage") ("inbook" . "chapter")
    ("incollection" . "chapter") ("inproceedings" . "paper-conference")
    ("manual" . "book") ("mastersthesis" . "thesis") ("misc" . "document")
    ("online" . "webpage") ("patent" . "patent")
    ("periodical" . "periodical") ("phdthesis" . "thesis")
    ("proceedings" . "book") ("report" . "report")
    ("software" . "software") ("techreport" . "report")
    ("thesis" . "thesis") ("unpublished" . "manuscript")
    ("www" . "webpage")))

(define bibtex-fields
  ;; the fields which go to one variable whatever the type
  '(("title" . "title") ("journal" . "container-title")
    ("journaltitle" . "container-title") ("booktitle" . "container-title")
    ("publisher" . "publisher") ("address" . "publisher-place")
    ("location" . "publisher-place") ("volume" . "volume")
    ("series" . "collection-title") ("edition" . "edition")
    ("note" . "note") ("doi" . "DOI") ("url" . "URL") ("isbn" . "ISBN")
    ("issn" . "ISSN") ("pmid" . "PMID") ("pmcid" . "PMCID")
    ("language" . "language") ("langid" . "language")
    ("abstract" . "abstract") ("type" . "genre")
    ("chapter" . "chapter-number") ("annote" . "annote")
    ("keywords" . "keyword") ("shorttitle" . "title-short")
    ("shortjournal" . "container-title-short") ("issue" . "issue")
    ("pagetotal" . "number-of-pages") ("volumes" . "number-of-volumes")
    ("version" . "version") ("eventtitle" . "event-title")
    ("venue" . "event-place")))

(define month-names
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11)
    ("dec" . 12)))

(define edition-words
  '(("first" . "1") ("second" . "2") ("third" . "3") ("fourth" . "4")
    ("fifth" . "5") ("sixth" . "6") ("seventh" . "7") ("eighth" . "8")
    ("ninth" . "9") ("tenth" . "10")))

(define (tree->rt t)
  ;; the value of a field as rich text
  (cond ((string? t) t)
        ((not (pair? t)) "")
        ((in? (car t) '(concat document)) (rt-cat* (map tree->rt (cdr t))))
        ((== (car t) 'keepcase) (cons 'nocase (map tree->rt (cdr t))))
        ((and (== (car t) 'with) (== (length t) 4)
              (== (cadr t) "font-shape") (== (caddr t) "italic"))
         (rt-fmt '((font-style . "italic")) (tree->rt (cadddr t))))
        ((and (== (car t) 'with) (== (length t) 4)
              (== (cadr t) "font-series") (== (caddr t) "bold"))
         (rt-fmt '((font-weight . "bold")) (tree->rt (cadddr t))))
        ((and (== (car t) 'with) (== (length t) 4)
              (== (cadr t) "font-shape") (== (caddr t) "small-caps"))
         (rt-fmt '((font-variant . "small-caps")) (tree->rt (cadddr t))))
        ((and (== (car t) 'em) (== (length t) 2))
         (rt-fmt '((font-style . "italic")) (tree->rt (cadr t))))
        ((and (== (car t) 'strong) (== (length t) 2))
         (rt-fmt '((font-weight . "bold")) (tree->rt (cadr t))))
        ((and (in? (car t) '(slink verbatim href)) (nnull? (cdr t)))
         (tree->rt (cadr t)))
        (else (list 'raw t))))

(define (tree->text t)
  (string-trim-both (rt->string (tree->rt t))))

(define (sentence-word w first?)
  ;; the words with only an initial capital lose it
  (if (and (not first?) (== w (csl-upcase-first (csl-locase w)))) (csl-locase w)
      w))

(define (sentence-string s first?)
  (let loop ((l (csl-chars s)) (word '()) (first? first?) (r '()))
    (define (flush)
      (if (null? word) r
          (cons (sentence-word (apply string-append (reverse word)) first?)
                r)))
    (cond ((null? l) (apply string-append (reverse (flush))))
          ((csl-letter? (car l)) (loop (cdr l) (cons (car l) word) first? r))
          (else (loop (cdr l) '()
                      (if (null? word) (or first? (in? (car l) '(":" "?" "!")))
                          (in? (car l) '(":" "?" "!")))
                      (cons (car l) (flush)))))))

(define (sentence-case x)
  ;; BibTeX titles are capitalized, CSL wants them as sentences
  (with first? #t
    (rt-map (lambda (s)
              (with r (sentence-string s first?)
                (when (!= (string-trim-both s) "") (set! first? #f))
                r))
            x)))

(define (bib-name->name t)
  (let* ((part (lambda (i) (with x (and (> (length t) i) (list-ref t i))
                             (and x (!= x "") (tree->text x)))))
         (first (part 1)) (von (part 2)) (last (part 3)) (jr (part 4)))
    (cond ((and (not first) (not von) (not jr))
           (if last (list (cons 'literal last)) '()))
          (else
            (set-part
             (set-part
              (set-part (set-part '() 'family last) 'given first)
              'non-dropping-particle von)
             'suffix jr)))))

(define (bib-names->names t)
  ;; returns (names . others?)
  (let* ((l (if (func? t 'bib-names) (cdr t) '()))
         (names (list-filter (map bib-name->name
                                  (list-filter l (cut func? <> 'bib-name)))
                             nnull?))
         (other? (lambda (n) (in? (csl-name-ref n 'literal)
                                  '("others" "et al." "et al")))))
    (cons (list-filter names (lambda (n) (not (other? n))))
          (list-or (map other? names)))))

(define (bib-pages->page t)
  (cond ((func? t 'bib-pages)
         (string-recompose (list-filter (map tree->text (cdr t))
                                        (lambda (x) (!= x "")))
                           "-"))
        (else
          (with s (tree->text t)
            (string-replace (string-replace s "\x15" "-") "--" "-")))))

(define (parse-month s)
  (let* ((low (csl-locase (string-trim-both s)))
         (p (and (>= (string-length low) 3)
                 (assoc (substring low 0 3) month-names))))
    (cond ((string->number low) (string->number low))
          (p (cdr p))
          (else #f))))

(define (bib-date year month day)
  (let* ((y (and year (string-trim-both year)))
         (m (and month (parse-month month)))
         (d (and day (string->number (string-trim-both day)))))
    (cond ((or (not y) (== y "")) #f)
          ((csl-string-number? y)
           (csl-make-date (list (string->number y) m (and m d)) #f #f #f #f))
          (else (csl-parse-date y)))))

(tm-define (csl-bib-entry->item e)
  (:synopsis "The item for the BibTeX entry @e, (bib-entry type key fields)")
  (let* ((type (csl-locase (cadr e)))
         (key (caddr e))
         (fields (if (and (> (length e) 3) (pair? (cadddr e)))
                     (list-filter (cdr (cadddr e)) (cut func? <> 'bib-field 2))
                     '()))
         (get (lambda (k) (with f (list-find fields
                                             (lambda (f) (== (cadr f) k)))
                            (and f (caddr f)))))
         (text (lambda (k) (and-with v (get k)
                             (with s (tree->text v)
                               (and (!= s "") s)))))
         (csl-type (with p (assoc type bibtex-types)
                     (if p (cdr p) "document")))
         (item (csl-make-item key csl-type))
         (put! (lambda (var val)
                 (when (and val (or (func? val 'date) (not (rt-empty? val)))
                            (not (ahash-ref item var)))
                   (ahash-set! item var val)))))
    (ahash-set! item "bibtex-type" type)
    ;; names
    (for (k '("author" "editor" "translator"))
      (and-with v (get k)
        (with p (bib-names->names v)
          (when (nnull? (car p)) (ahash-set! item k (car p)))
          (when (cdr p) (ahash-set! item (string-append k ":others") #t)))))
    ;; dates
    (put! "issued" (or (and-with d (text "date") (csl-parse-date d))
                       (bib-date (text "year") (text "month") (text "day"))))
    (and-with d (text "urldate") (put! "accessed" (csl-parse-date d)))
    ;; titles, as sentences
    (and-with v (get "title")
      (with sub (get "subtitle")
        (put! "title" (sentence-case
                       (if sub (rt-cat (tree->rt v) ": " (tree->rt sub))
                           (tree->rt v))))))
    (when (in? type '("incollection" "inproceedings" "conference" "inbook"))
      (and-with v (get "booktitle")
        (put! "container-title" (sentence-case (tree->rt v)))))
    ;; fields which depend on the type
    (and-with v (get "pages") (put! "page" (bib-pages->page v)))
    (and-with v (text "number")
      (put! (cond ((in? type '("article" "periodical")) "issue")
                  ((in? type '("techreport" "report" "manual" "patent"
                               "misc" "unpublished")) "number")
                  (else "collection-number"))
            v))
    (and-with v (text "edition")
      (with p (assoc (csl-locase v) edition-words)
        (put! "edition" (if p (cdr p) v))))
    (when (== type "mastersthesis") (put! "genre" "Master's thesis"))
    (when (== type "phdthesis") (put! "genre" "PhD thesis"))
    (and-with v (get "school") (put! "publisher" (tree->rt v)))
    (and-with v (get "institution") (put! "publisher" (tree->rt v)))
    (and-with v (get "publisher") (put! "publisher" (tree->rt v)))
    (and-with v (get "organization") (put! "publisher" (tree->rt v)))
    (and-with v (text "howpublished")
      (if (or (string-starts? v "http://") (string-starts? v "https://"))
          (put! "URL" v)
          (put! "publisher" (tree->rt (get "howpublished")))))
    (when (and (text "eprint") (not (text "url"))
               (in? (csl-locase (or (text "archiveprefix")
                                    (text "eprinttype") ""))
                    '("arxiv")))
      (put! "URL" (string-append "https://arxiv.org/abs/" (text "eprint"))))
    ;; the other fields
    (for (p bibtex-fields)
      (and-with v (get (car p))
        (if (in? (cdr p) '("DOI" "URL" "ISBN" "ISSN" "PMID" "PMCID"
                           "language" "volume" "issue" "chapter-number"
                           "number-of-pages" "number-of-volumes" "version"))
            (put! (cdr p) (tree->text v))
            (put! (cdr p) (tree->rt v)))))
    item))

(tm-define (csl-bib->items t)
  (:synopsis "The items for the entries of the BibTeX document @t")
  (map csl-bib-entry->item
       (list-filter (if (func? t 'document) (cdr t) t)
                    (lambda (e) (and (func? e 'bib-entry)
                                     (>= (length e) 3)
                                     (string? (cadr e))
                                     (string? (caddr e)))))))
