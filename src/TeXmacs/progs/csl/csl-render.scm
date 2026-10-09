
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-render.scm
;; DESCRIPTION : evaluation of the rendering elements of a CSL style
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The rendering elements (cs:text, cs:number, cs:label, cs:date, cs:names,
;; cs:group, cs:choose) are evaluated for one item in a context, a hash
;; table with the entries
;;
;;   style, locale, item
;;   mode         citation or bibliography
;;   area         the node cs:citation or cs:bibliography
;;   sort?        whether a sort key is computed
;;   sort-names   the options names-min... of the sort key
;;   number       the number of the item in the bibliography (a string)
;;   year-suffix  the suffix which disambiguates the year, or #f
;;   names-add    the number of names added by the disambiguation
;;   disambiguate whether the condition disambiguate="true" holds
;;   locator, label, position, near-note   the data of the cite
;;   suppressed   the variables which a cs:substitute has used
;;   seen, filled the state of the enclosing cs:group: whether a variable
;;                was called, and whether one was not empty

(texmacs-module (csl csl-render)
  (:use (csl csl-utils) (csl csl-style) (csl csl-data) (csl csl-names)))

(define en-dash (utf8->cork "–"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (csl-make-context style locale item mode)
  (with ctx (make-ahash-table)
    (ahash-set! ctx 'style style)
    (ahash-set! ctx 'locale locale)
    (ahash-set! ctx 'item item)
    (ahash-set! ctx 'mode mode)
    (ahash-set! ctx 'area (csl-style-ref style mode))
    (ahash-set! ctx 'suppressed '())
    (ahash-set! ctx 'names-add 0)
    (ahash-set! ctx 'position 'first)
    ctx))

(define (ctx-ref ctx key) (ahash-ref ctx key))
(define (ctx-set! ctx key val) (ahash-set! ctx key val))

(define (mark! ctx filled?)
  (ctx-set! ctx 'seen #t)
  (when filled? (ctx-set! ctx 'filled #t)))

(define (inherited-option ctx key)
  ;; an option of cs:citation or cs:bibliography, or else of cs:style
  (or (and (ctx-ref ctx 'area) (csl-attr (ctx-ref ctx 'area) key))
      (csl-style-option (ctx-ref ctx 'style) key)))

(define (english? ctx)
  (csl-item-english? (ctx-ref ctx 'item)
                     (or (csl-style-ref (ctx-ref ctx 'style) 'default-locale)
                         "en-US")))

(define (term ctx name form plural?)
  (csl-term (ctx-ref ctx 'locale) name form plural?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes common to the rendering elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (finish node ctx x)
  ;; strip-periods, text-case, formatting, quotes, affixes and display
  (if (or (rt-empty? x) (ctx-ref ctx 'sort?)) x
      (let* ((x1 (if (csl-attr? node 'strip-periods "true")
                     (rt-strip-periods x) x))
             (tc (csl-attr node 'text-case))
             (x2 (if tc (rt-text-case x1 tc (english? ctx)) x1))
             (fmt (append (csl-node-formatting node)
                          (if (csl-attr? node 'quotes "true")
                              '((quotes . "true")) '())))
             (x3 (rt-fmt fmt x2))
             (x4 (rt-affix (csl-attr node 'prefix) x3
                           (csl-attr node 'suffix)))
             (display (csl-attr node 'display)))
        (if display (rt-fmt (list (cons 'display display)) x4) x4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (numeric-token? s)
  ;; letters, digits, letters: 12, 2b, D2
  (let loop ((l (csl-chars s)) (state 0))
    (cond ((null? l) (> state 0))
          ((csl-digit? (car l)) (and (<= state 1) (loop (cdr l) 1)))
          ((csl-letter? (car l))
           (loop (cdr l) (if (== state 0) 0 2)))
          (else #f))))

(tm-define (csl-number-tokens s)
  (:synopsis "The numbers and the separators of @s, or #f if not numeric")
  ;; "1 - 3, 5" -> ("1" "-" "3" "," "5")
  (let loop ((l (csl-chars s)) (word '()) (r '()))
    (define (flush)
      (if (null? word) r (cons (apply string-append (reverse word)) r)))
    (cond ((null? l)
           (with toks (reverse (flush))
             (and (nnull? toks)
                  (let check ((l toks) (number? #t))
                    (cond ((null? l) (not number?))
                          (number? (and (numeric-token? (car l))
                                        (check (cdr l) #f)))
                          (else (and (in? (car l) '("-" "," "&"))
                                     (check (cdr l) #t)))))
                  toks)))
          ((== (car l) " ") (loop (cdr l) '() (flush)))
          ((in? (car l) (list "-" en-dash))
           (loop (cdr l) '() (cons "-" (flush))))
          ((in? (car l) '("," "&")) (loop (cdr l) '() (cons (car l) (flush))))
          (else (loop (cdr l) (cons (car l) word) r)))))

(tm-define (csl-numeric? x)
  (and (not (rt-empty? x))
       (if (csl-number-tokens (rt->string x)) #t #f)))

(define (format-number-tokens toks f)
  (apply string-append
         (map (lambda (t)
                (cond ((== t "-") en-dash)
                      ((== t ",") ", ")
                      ((== t "&") " & ")
                      (else (f t))))
              toks)))

(define (ordinal ctx var s long?)
  (with n (string->number s)
    (if (not n) s
        (let* ((locale (ctx-ref ctx 'locale))
               (gender (csl-term-gender locale var))
               (long (and long? (>= n 1) (<= n 10)
                          (term ctx (string-append "long-ordinal-"
                                                   (csl-pad n 2))
                                #f #f))))
          (or long
              (string-append s (csl-ordinal-suffix locale n gender)))))))

(define (format-number ctx var s form)
  (with toks (csl-number-tokens s)
    (cond ((not toks) s)
          ((== form "ordinal")
           (format-number-tokens toks (lambda (t) (ordinal ctx var t #f))))
          ((== form "long-ordinal")
           (format-number-tokens toks (lambda (t) (ordinal ctx var t #t))))
          ((== form "roman")
           (format-number-tokens
            toks (lambda (t)
                   (with n (string->number t)
                     (if (and n (> n 0) (< n 4000)) (csl-roman n) t)))))
          (else (format-number-tokens toks identity)))))

(define (expand-page a b)
  ;; 321, 28 -> 328
  (if (< (string-length b) (string-length a))
      (string-append (substring a 0 (- (string-length a) (string-length b)))
                     b)
      b))

(define (minimal-page a b keep)
  ;; the digits of @b which differ from those of @a, at least @keep
  (if (!= (string-length a) (string-length b)) b
      (let loop ((i 0))
        (cond ((>= i (- (string-length b) keep)) (string-drop b i))
              ((!= (string-ref a i) (string-ref b i)) (string-drop b i))
              (else (loop (+ i 1)))))))

(define (page-range format a b)
  (let* ((b* (expand-page a b))
         (n (string->number a))
         (len (string-length a)))
    (cond ((or (not format) (not (csl-string-number? a))
               (not (csl-string-number? b)))
           b)
          ((== format "expanded") b*)
          ((== format "minimal") (minimal-page a b* 1))
          ((== format "minimal-two") (minimal-page a b* 2))
          ((in? format '("chicago" "chicago-15" "chicago-16"))
           (cond ((or (< n 100) (== (modulo n 100) 0)) b*)
                 ((and (== len 4) (== (string-length b*) 4)
                       (<= (string-length (minimal-page a b* 1)) 4)
                       (>= (string-length (minimal-page a b* 1)) 3))
                  b*)
                 ((< (modulo n 100) 10) (minimal-page a b* 1))
                 (else (minimal-page a b* 2))))
          (else b))))

(define (format-pages ctx s)
  (let* ((toks (csl-number-tokens s))
         (format (csl-style-option (ctx-ref ctx 'style) 'page-range-format))
         (delim (or (term ctx "page-range-delimiter" #f #f) en-dash)))
    (if (not toks) s
        (let loop ((l toks) (r '()))
          (cond ((null? l) (apply string-append (reverse r)))
                ((and (>= (length l) 3) (== (cadr l) "-"))
                 (loop (cdddr l)
                       (cons* (page-range format (car l) (caddr l)) delim
                              (car l) r)))
                ((== (car l) ",") (loop (cdr l) (cons ", " r)))
                ((== (car l) "&") (loop (cdr l) (cons " & " r)))
                ((== (car l) "-") (loop (cdr l) (cons delim r)))
                (else (loop (cdr l) (cons (car l) r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable-value ctx var)
  ;; the value of a standard or number variable, as rich text
  (cond ((in? var (ctx-ref ctx 'suppressed)) #f)
        ((== var "locator") (ctx-ref ctx 'locator))
        ((== var "citation-number") (ctx-ref ctx 'number))
        ((== var "year-suffix") (ctx-ref ctx 'year-suffix))
        ((== var "first-reference-note-number") (ctx-ref ctx 'first-note))
        ((== var "citation-label")
         (string-append
          (rt->string (or (csl-item-ref (ctx-ref ctx 'item) var)
                          (csl-citation-label (ctx-ref ctx 'item))))
          (begin
            (when (ctx-ref ctx 'year-suffix)
              (ctx-set! ctx 'year-suffix-done #t))
            (or (ctx-ref ctx 'year-suffix) ""))))
        ((== var "page-first")
         (or (csl-item-ref (ctx-ref ctx 'item) var)
             (and-with p (csl-item-ref (ctx-ref ctx 'item) "page")
               (and-with toks (csl-number-tokens (rt->string p))
                 (car toks)))))
        (else
          (with v (csl-item-ref (ctx-ref ctx 'item) var)
            (and v (not (and (pair? v) (in? (car v) '(date))))
                 (not (and (pair? v) (pair? (car v))))
                 (not (== v #t))
                 v)))))

(define (variable-defined? ctx var)
  (cond ((in? var '("locator" "citation-number" "year-suffix" "page-first"
                    "first-reference-note-number" "citation-label"))
         (not (rt-empty? (variable-value ctx var))))
        (else
          (with v (csl-item-ref (ctx-ref ctx 'item) var)
            (cond ((not v) #f)
                  ((string? v) (!= v ""))
                  ((null? v) #f)
                  (else #t))))))

(define (text-variable ctx var form)
  (let* ((short (and (== form "short")
                     (variable-value ctx (string-append var "-short"))))
         (v (if (rt-empty? short) (variable-value ctx var) short)))
    (cond ((rt-empty? v) #f)
          ((and (in? var '("page" "locator")) (string? v)
                (or (== var "page") (== (ctx-ref ctx 'label) "page")))
           (format-pages ctx v))
          (else v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cs:text, cs:number, cs:label
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (render-text node ctx)
  (cond ((csl-attr node 'variable)
         (with v (text-variable ctx (csl-attr node 'variable)
                                (csl-attr node 'form "long"))
           (mark! ctx (not (rt-empty? v)))
           (when (== (csl-attr node 'variable) "year-suffix")
             (ctx-set! ctx 'year-suffix-done #t))
           (finish node ctx v)))
        ((csl-attr node 'macro)
         (with m (csl-style-macro (ctx-ref ctx 'style) (csl-attr node 'macro))
           (finish node ctx (and m (render-children (csl-children m) ctx
                                                    #f)))))
        ((csl-attr node 'term)
         (finish node ctx (term ctx (csl-attr node 'term)
                                (csl-attr node 'form "long")
                                (csl-attr? node 'plural "true"))))
        ((csl-attr node 'value) (finish node ctx (csl-attr node 'value)))
        (else #f)))

(define (render-number node ctx)
  (let* ((var (csl-attr node 'variable ""))
         (v (variable-value ctx var))
         (s (and (not (rt-empty? v)) (rt->string v))))
    (mark! ctx (not (rt-empty? v)))
    (cond ((rt-empty? v) #f)
          ((csl-number-tokens s)
           (finish node ctx (format-number ctx var s
                                           (csl-attr node 'form "numeric"))))
          (else (finish node ctx v)))))

(define (plural-number? var s)
  (with toks (csl-number-tokens s)
    (cond ((not toks) #f)
          ((in? var '("number-of-pages" "number-of-volumes"))
           (with n (string->number (car toks))
             (and n (> n 1))))
          (else (> (length toks) 1)))))

(define (render-label node ctx)
  (let* ((var (csl-attr node 'variable ""))
         (v (variable-value ctx var))
         (name (if (== var "locator") (or (ctx-ref ctx 'label) "page") var))
         (rule (csl-attr node 'plural "contextual"))
         (plural? (cond ((== rule "always") #t)
                        ((== rule "never") #f)
                        (else (and (not (rt-empty? v))
                                   (plural-number? var (rt->string v)))))))
    (if (rt-empty? v) #f
        (finish node ctx (term ctx name (csl-attr node 'form "long")
                               plural?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cs:date
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-attrs base over)
  ;; the attributes @over replace those of @base
  (append over (list-filter base (lambda (p) (not (assq (car p) over))))))

(define (date-part-nodes node ctx)
  ;; the nodes cs:date-part which apply, and the delimiter between them
  (with form (csl-attr node 'form)
    (if (not form)
        (cons (csl-children-named node 'date-part)
              (csl-attr node 'delimiter ""))
        (let* ((loc (csl-locale-date (ctx-ref ctx 'locale) form))
               (own (csl-children-named node 'date-part))
               (which (csl-attr node 'date-parts "year-month-day"))
               (keep (cond ((== which "year") '("year"))
                           ((== which "year-month") '("year" "month"))
                           (else '("year" "month" "day"))))
               (parts (if loc (csl-children-named loc 'date-part) '())))
          (cons
           (map (lambda (p)
                  (with o (list-find own (cut csl-attr? <> 'name
                                              (csl-attr p 'name)))
                    (if o (cons* 'date-part
                                 (merge-attrs (csl-attrs p) (csl-attrs o))
                                 '())
                        p)))
                (list-filter parts (lambda (p) (in? (csl-attr p 'name)
                                                    keep))))
           (if loc (csl-attr loc 'delimiter "") ""))))))

(define (render-year ctx part y)
  (let* ((form (csl-attr part 'form "long"))
         (abs-y (abs y))
         (s (if (== form "short")
                (csl-pad (modulo abs-y 100) 2)
                (number->string abs-y)))
         (era (cond ((< y 0) (or (term ctx "bc" #f #f) ""))
                    ((< y 1000) (or (term ctx "ad" #f #f) ""))
                    (else "")))
         (suffix (and (not (ctx-ref ctx 'explicit-year-suffix))
                      (ctx-ref ctx 'year-suffix))))
    (when suffix (ctx-set! ctx 'year-suffix-done #t))
    (string-append s era (or suffix ""))))

(define (render-month ctx part m season)
  (with form (csl-attr part 'form "long")
    (cond ((not m) #f)
          ((> m 20)
           (term ctx (string-append "season-" (csl-pad (- m 20) 2)) #f #f))
          ((== form "numeric") (number->string m))
          ((== form "numeric-leading-zeros") (csl-pad m 2))
          (else (term ctx (string-append "month-" (csl-pad m 2)) form #f)))))

(define (render-day ctx part d)
  (with form (csl-attr part 'form "numeric")
    (cond ((not d) #f)
          ((== form "numeric-leading-zeros") (csl-pad d 2))
          ((and (== form "ordinal")
                (or (== d 1)
                    (!= (csl-locale-option (ctx-ref ctx 'locale)
                                           'limit-day-ordinals-to-day-1
                                           "false")
                        "true")))
           (string-append (number->string d)
                          (csl-ordinal-suffix
                           (ctx-ref ctx 'locale) d
                           (csl-term-gender (ctx-ref ctx 'locale)
                                            "month-01"))))
          (else (number->string d)))))

(define (render-date-part ctx part parts season affixes?)
  ;; @affixes? is a pair which tells whether the prefix and the suffix
  ;; are rendered
  (let* ((name (csl-attr part 'name))
         (y (car parts)) (m (cadr parts)) (d (caddr parts))
         (x (cond ((== name "year") (and y (render-year ctx part y)))
                  ((== name "month") (render-month ctx part m season))
                  ((== name "day") (and m (<= m 12) (render-day ctx part d)))
                  (else #f))))
    (if (rt-empty? x) #f
        (let* ((x1 (if (csl-attr? part 'strip-periods "true")
                       (rt-strip-periods x) x))
               (tc (csl-attr part 'text-case))
               (x2 (if tc (rt-text-case x1 tc (english? ctx)) x1))
               (x3 (rt-fmt (csl-node-formatting part) x2)))
          (rt-cat (and (car affixes?) (csl-attr part 'prefix))
                  x3
                  (and (cdr affixes?) (csl-attr part 'suffix)))))))

(define (render-date-parts ctx nodes delim parts season)
  (rt-join (map (lambda (p) (render-date-part ctx p parts season
                                              (cons #t #t)))
                nodes)
           delim))

(define (part-differs? name d1 d2)
  (cond ((== name "year") (!= (car d1) (car d2)))
        ((== name "month") (or (!= (car d1) (car d2))
                               (!= (cadr d1) (cadr d2))))
        (else (!= d1 d2))))

(define (render-date-range ctx nodes delim d1 d2 season)
  ;; the parts which differ are rendered for both dates, around the
  ;; delimiter of the range, between the parts which are shared
  (let* ((differs (map (lambda (p) (part-differs? (csl-attr p 'name) d1 d2))
                       nodes))
         (idx (list-filter (iota (length nodes))
                           (lambda (i) (list-ref differs i)))))
    (if (null? idx)
        (render-date-parts ctx nodes delim d1 season)
        (let* ((lo (car idx)) (hi (cAr idx))
               (before (list-head nodes lo))
               (block (sublist nodes lo (+ hi 1)))
               (after (list-tail nodes (+ hi 1)))
               (largest (or (list-find block (cut csl-attr? <> 'name "year"))
                            (list-find block (cut csl-attr? <> 'name
                                                  "month"))
                            (car block)))
               (range-delim (csl-attr largest 'range-delimiter en-dash))
               (n (length block))
               (side (lambda (parts end?)
                       (rt-join
                        (map (lambda (p i)
                               (render-date-part
                                ctx p parts season
                                (cons (or (not end?) (> i 0))
                                      (or end? (< i (- n 1))))))
                             block (iota n))
                        delim))))
          (rt-join (list (render-date-parts ctx before delim d1 season)
                         (rt-cat (side d1 #f) range-delim (side d2 #t))
                         (render-date-parts ctx after delim d1 season))
                   delim)))))

(define (date-sort-key nodes d)
  (let* ((names (map (cut csl-attr <> 'name) nodes))
         (parts (or (csl-date-start d) '(#f #f #f)))
         (y (or (car parts) 0)))
    (string-append
     (if (< y 0) "0" "1")
     (csl-pad (if (< y 0) (+ 10000 y) y) 5)
     (csl-pad (if (in? "month" names) (or (cadr parts) 0) 0) 2)
     (csl-pad (if (in? "day" names) (or (caddr parts) 0) 0) 2))))

(define (render-date node ctx)
  (let* ((var (csl-attr node 'variable ""))
         (d (and (not (in? var (ctx-ref ctx 'suppressed)))
                 (csl-item-ref (ctx-ref ctx 'item) var))))
    (cond ((not (func? d 'date)) (mark! ctx #f) #f)
          ((ctx-ref ctx 'sort?)
           (mark! ctx #t)
           (date-sort-key (car (date-part-nodes node ctx)) d))
          ((not (csl-date-start d))
           (mark! ctx (if (csl-date-literal d) #t #f))
           (finish node ctx (csl-date-literal d)))
          (else
            (let* ((spec (date-part-nodes node ctx))
                   (nodes (car spec))
                   (delim (cdr spec))
                   (x (cond
                        ((and (csl-date-end d) (not (car (csl-date-end d))))
                         (rt-cat (render-date-parts ctx nodes delim
                                                    (csl-date-start d) #f)
                                 en-dash))
                        ((csl-date-end d)
                          (render-date-range ctx nodes delim
                                             (csl-date-start d)
                                             (csl-date-end d)
                                             (csl-date-season d)))
                        (else
                          (render-date-parts ctx nodes delim
                                             (csl-date-start d)
                                             (csl-date-season d))))))
              (mark! ctx (not (rt-empty? x)))
              (finish node ctx x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cs:names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-name-node '(name ()))

(define (name-option ctx name-node)
  ;; the procedure which gives the options of a list of names
  (lambda (key default)
    (let* ((sort (and (ctx-ref ctx 'sort?) (or (ctx-ref ctx 'sort-names)
                                                '())))
           (over (and sort
                      (cond ((== key 'et-al-min) (assq 'names-min sort))
                            ((== key 'et-al-use-first)
                             (assq 'names-use-first sort))
                            ((== key 'et-al-use-last)
                             (assq 'names-use-last sort))
                            (else #f))))
           (inherited (cond ((== key 'form) 'name-form)
                            ((== key 'delimiter) 'name-delimiter)
                            ((in? key '(prefix suffix)) #f)
                            (else key))))
      (cond (over (cdr over))
            ((and sort (== key 'name-as-sort-order)) "all")
            ((and sort (in? key '(et-al-subsequent-min
                                  et-al-subsequent-use-first)))
             default)
            (else (or (csl-attr name-node key)
                      (and inherited (inherited-option ctx inherited))
                      default))))))

(define (names-settings ctx name-node et-al-node var)
  (let* ((opt (name-option ctx name-node))
         (and-opt (opt 'and #f))
         (et-al-term (if et-al-node (csl-attr et-al-node 'term "et-al")
                         "et-al"))
         (et-al (term ctx et-al-term #f #f)))
    (list (cons 'parts (csl-children-named name-node 'name-part))
          (cons 'formatting (csl-node-formatting name-node))
          (cons 'et-al (if et-al-node
                           (rt-fmt (csl-node-formatting et-al-node) et-al)
                           et-al))
          (cons 'and (cond ((== and-opt "text") (term ctx "and" #f #f))
                           ((== and-opt "symbol") "&")
                           (else #f)))
          (cons 'demote
                (csl-style-option (ctx-ref ctx 'style)
                                  'demote-non-dropping-particle
                                  "display-and-sort"))
          (cons 'hyphen?
                (!= (csl-style-option (ctx-ref ctx 'style)
                                      'initialize-with-hyphen "true")
                    "false"))
          (cons 'subsequent? (and (== (ctx-ref ctx 'mode) 'citation)
                                  (!= (ctx-ref ctx 'position) 'first)))
          (cons 'others? (csl-item-ref (ctx-ref ctx 'item)
                                       (string-append var ":others")))
          (cons 'more (ctx-ref ctx 'names-add))
          (cons 'english? (english? ctx)))))

(define (names-label ctx label-node var count)
  (let* ((rule (csl-attr label-node 'plural "contextual"))
         (plural? (cond ((== rule "always") #t)
                        ((== rule "never") #f)
                        (else (> count 1)))))
    (finish label-node ctx
            (term ctx var (csl-attr label-node 'form "long") plural?))))

(define (names-sort-key ctx names opt)
  (let* ((demote (csl-style-option (ctx-ref ctx 'style)
                                   'demote-non-dropping-particle
                                   "display-and-sort"))
         (shown (csl-names-shown names opt '())))
    (if (== (opt 'form "long") "count")
        (csl-pad (car shown) 5)
        (string-recompose
         (map (cut csl-name-sort-key <> demote)
              (list-head names (car shown)))
         "  "))))

(define (render-one-names ctx var names name-node et-al-node label-node
                          label-first? label-var)
  (let* ((opt (name-option ctx name-node))
         (settings (names-settings ctx name-node et-al-node var)))
    (if (ctx-ref ctx 'sort?)
        (names-sort-key ctx names opt)
        (let* ((x (csl-format-names names opt settings))
               (count? (== (opt 'form "long") "count"))
               (label (and label-node (not count?)
                           (names-label ctx label-node label-var
                                        (length names)))))
          (if label-first? (rt-cat label x) (rt-cat x label))))))

(define (render-names node ctx . inherit)
  ;; @inherit holds the node cs:names whose children a cs:names inside
  ;; cs:substitute uses when it has none
  (let* ((own? (or (csl-child node 'name) (csl-child node 'label)
                   (csl-child node 'et-al) (null? inherit)))
         (src (if own? node (car inherit)))
         (name-node (or (csl-child src 'name) empty-name-node))
         (et-al-node (csl-child src 'et-al))
         (label-node (csl-child src 'label))
         (label-first? (and label-node
                            (let loop ((l (csl-children src)))
                              (cond ((null? l) #f)
                                    ((== (caar l) 'label) #t)
                                    ((== (caar l) 'name) #f)
                                    (else (loop (cdr l)))))))
         (item (ctx-ref ctx 'item))
         (vars (list-filter (csl-split (csl-attr node 'variable ""))
                            (lambda (v) (nin? v (ctx-ref ctx 'suppressed)))))
         (value (lambda (v) (with l (csl-item-ref item v)
                              (if (and (pair? l) (pair? (car l))) l '()))))
         (same? (and (in? "editor" vars) (in? "translator" vars)
                     (nnull? (value "editor"))
                     (== (value "editor") (value "translator"))))
         (vars* (if same? (list-filter vars (cut != <> "translator")) vars))
         (filled (list-filter vars* (lambda (v) (nnull? (value v)))))
         (delim (or (csl-attr node 'delimiter)
                    (inherited-option ctx 'names-delimiter)
                    (if (ctx-ref ctx 'sort?) "  " "")))
         (parts (map (lambda (v)
                       (render-one-names
                        ctx v (value v) name-node et-al-node label-node
                        label-first?
                        (if (and same? (== v "editor")) "editortranslator"
                            v)))
                     filled))
         (count? (== ((name-option ctx name-node) 'form "long") "count"))
         (x (if (and count? (nnull? parts))
                (with n (apply + (map (lambda (p) (or (string->number p) 0))
                                      parts))
                  (if (ctx-ref ctx 'sort?) (csl-pad n 5) (number->string n)))
                (rt-join parts delim))))
    (cond ((not (rt-empty? x))
           (mark! ctx #t)
           (finish (if own? node src) ctx x))
          (else
            (mark! ctx #f)
            (with sub (csl-child node 'substitute)
              (and sub (render-substitute sub node ctx)))))))

(define (render-substitute sub parent ctx)
  (let loop ((l (csl-children sub)))
    (if (null? l) #f
        (with x (if (== (caar l) 'names)
                    (render-names (car l) ctx parent)
                    (render (car l) ctx))
          (cond ((rt-empty? x) (loop (cdr l)))
                (else
                  (with vars (csl-split (csl-attr (car l) 'variable ""))
                    (ctx-set! ctx 'suppressed
                              (append vars (ctx-ref ctx 'suppressed))))
                  (ctx-set! ctx 'substituted #t)
                  (finish parent ctx x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cs:choose
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (position-test ctx val)
  (let* ((pos (ctx-ref ctx 'position))
         (cite? (== (ctx-ref ctx 'mode) 'citation)))
    (and cite?
         (cond ((== val "first") (== pos 'first))
               ((== val "subsequent") (!= pos 'first))
               ((== val "ibid") (in? pos '(ibid ibid-with-locator)))
               ((== val "ibid-with-locator") (== pos 'ibid-with-locator))
               ((== val "near-note") (if (ctx-ref ctx 'near-note) #t #f))
               (else #f)))))

(define (condition-tests node ctx)
  ;; the results of all the tests of a cs:if
  (let* ((item (ctx-ref ctx 'item))
         (tests (lambda (key f)
                  (map f (csl-split (csl-attr node key ""))))))
    (append
     (tests 'type (lambda (v) (== (csl-item-type item) v)))
     (tests 'variable (lambda (v) (variable-defined? ctx v)))
     (tests 'is-numeric
            (lambda (v) (csl-numeric? (variable-value ctx v))))
     (tests 'is-uncertain-date
            (lambda (v) (with d (csl-item-ref item v)
                          (and (func? d 'date) (csl-date-circa? d) #t))))
     (tests 'locator
            (lambda (v) (and (not (rt-empty? (ctx-ref ctx 'locator)))
                             (== (or (ctx-ref ctx 'label) "page") v))))
     (tests 'position (lambda (v) (position-test ctx v)))
     (tests 'disambiguate
            (lambda (v)
              (ctx-set! ctx 'disambiguate-seen #t)
              (== (if (ctx-ref ctx 'disambiguate) "true" "false") v))))))

(define (condition-holds? node ctx)
  (let* ((l (condition-tests node ctx))
         (match (csl-attr node 'match "all")))
    (cond ((== match "any") (list-or l))
          ((== match "none") (not (list-or l)))
          (else (list-and l)))))

(define (render-choose node ctx)
  ;; the rendered children of the branch which applies: the delimiter of
  ;; the enclosing element separates them
  (let loop ((l (csl-children node)))
    (cond ((null? l) '())
          ((or (== (caar l) 'else) (condition-holds? (car l) ctx))
           (render-list (csl-children (car l)) ctx))
          (else (loop (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cs:group and the dispatcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (render-group node ctx)
  (let* ((seen (ctx-ref ctx 'seen))
         (filled (ctx-ref ctx 'filled)))
    (ctx-set! ctx 'seen #f)
    (ctx-set! ctx 'filled #f)
    (let* ((x (render-children (csl-children node) ctx
                               (csl-attr node 'delimiter)))
           (seen* (ctx-ref ctx 'seen))
           (filled* (ctx-ref ctx 'filled)))
      (ctx-set! ctx 'seen (or seen seen*))
      (ctx-set! ctx 'filled (or filled filled*))
      (if (and seen* (not filled*)) #f
          (finish node ctx x)))))

(define (render node ctx)
  (with name (csl-name node)
    (cond ((== name 'text) (render-text node ctx))
          ((== name 'number) (render-number node ctx))
          ((== name 'label) (render-label node ctx))
          ((== name 'date) (render-date node ctx))
          ((== name 'names) (render-names node ctx))
          ((== name 'group) (render-group node ctx))
          ((== name 'choose) (rt-cat* (render-choose node ctx)))
          (else #f))))

(define (render-list l ctx)
  (append-map (lambda (node)
                (if (== (csl-name node) 'choose)
                    (render-choose node ctx)
                    (list (render node ctx))))
              l))

(define (render-children l ctx delim)
  (rt-join (render-list l ctx)
           (if (ctx-ref ctx 'sort?) (or delim " ") delim)))

(tm-define (csl-render-layout layout ctx)
  (:synopsis "Render the children of the node cs:layout for the item of @ctx")
  (ctx-set! ctx 'seen #f)
  (ctx-set! ctx 'filled #f)
  (ctx-set! ctx 'suppressed '())
  (render-children (csl-children layout) ctx #f))

(tm-define (csl-render-macro name ctx)
  (:synopsis "Render the macro @name for the item of @ctx")
  (ctx-set! ctx 'seen #f)
  (ctx-set! ctx 'filled #f)
  (ctx-set! ctx 'suppressed '())
  (with m (csl-style-macro (ctx-ref ctx 'style) name)
    (and m (render-children (csl-children m) ctx #f))))

(tm-define (csl-render-variable var ctx)
  (:synopsis "The sort key for the variable @var of the item of @ctx")
  (let* ((item (ctx-ref ctx 'item))
         (v (csl-item-ref item var)))
    (cond ((csl-name-variable? var)
           (if (and (pair? v) (pair? (car v)))
               (names-sort-key ctx v (name-option ctx empty-name-node))
               #f))
          ((func? v 'date)
           (date-sort-key '((date-part ((name . "year")))
                            (date-part ((name . "month")))
                            (date-part ((name . "day"))))
                          v))
          ((csl-number-variable? var)
           (with s (rt->string (variable-value ctx var))
             (cond ((== s "") #f)
                   ((csl-string-number? s) (csl-pad s 10))
                   (else s))))
          (else (with s (rt->string (variable-value ctx var))
                  (and (!= s "") s))))))

(tm-define (csl-finish-layout layout ctx x)
  (:synopsis "Apply the formatting and the affixes of cs:layout to @x")
  (finish layout ctx x))
