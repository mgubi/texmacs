
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-utils.scm
;; DESCRIPTION : helpers of the CSL processor: nodes, rich text, strings
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The processor of the Citation Style Language (CSL 1.0.2) works on three
;; kinds of data:
;;
;;   - nodes: the elements of a style or of a locale, (name attrs . children)
;;     where attrs is an association list of symbols and strings and where
;;     the text inside an element is kept only for terms;
;;   - items: the references, hash tables from the names of the CSL
;;     variables to rich text, lists of names or dates (csl-data.scm);
;;   - rich text: what is rendered, see below.
;;
;; All strings are in the TeXmacs encoding (Cork with <entities>).
;;
;; Rich text is #f or "" (nothing), a string, or one of
;;
;;   (cat x ...)          a concatenation
;;   (fmt alist x ...)    formatted text; the keys of alist are font-style,
;;                        font-variant, font-weight, text-decoration,
;;                        vertical-align, quotes and display
;;   (nocase x ...)       text whose case is never changed
;;   (raw t)              a TeXmacs tree taken as it is (a formula...)

(texmacs-module (csl csl-utils))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (local-name s)
  ;; cs:name -> name
  (with i (string-index s #\:)
    (if i (substring s (+ i 1) (string-length s)) s)))

(define (text-element? name)
  (in? name '(term single multiple title id)))

(define (blank? s)
  (== (string-trim-both s) ""))

(define (clean-attr a)
  (cons (car a) (utf8->cork (if (pair? (cdr a)) (cadr a) ""))))

(define (clean-children name l)
  (cond ((null? l) '())
        ((string? (car l))
         (if (and (text-element? name) (not (and (blank? (car l))
                                                 (nnull? (cdr l)))))
             (cons (utf8->cork (car l)) (clean-children name (cdr l)))
             (clean-children name (cdr l))))
        ((and (pair? (car l)) (symbol? (caar l))
              (not (in? (caar l) '(*PI* *COMMENT* *TOP* @))))
         (cons (csl-clean (car l)) (clean-children name (cdr l))))
        (else (clean-children name (cdr l)))))

(tm-define (csl-clean x)
  (:synopsis "Convert the SXML element @x into a node")
  (let* ((name (string->symbol (local-name (symbol->string (car x)))))
         (attrs? (and (nnull? (cdr x)) (pair? (cadr x)) (== (caadr x) '@)))
         (attrs (if attrs? (map clean-attr (cdadr x)) '()))
         (body (if attrs? (cddr x) (cdr x))))
    (cons* name attrs (clean-children name body))))

(tm-define (csl-parse s)
  (:synopsis "The root node of the XML document in the string @s, or #f")
  (with x (parse-xml s)
    (and (pair? x)
         (with l (list-filter (cdr x)
                              (lambda (e) (and (pair? e) (symbol? (car e))
                                               (not (in? (car e)
                                                         '(*PI* *COMMENT*
                                                           *DOCTYPE*))))))
           (and (nnull? l) (csl-clean (car l)))))))

(tm-define (csl-name x) (car x))
(tm-define (csl-attrs x) (cadr x))
(tm-define (csl-children x) (cddr x))

(tm-define (csl-attr x key . default)
  (with p (assq key (cadr x))
    (cond (p (cdr p))
          ((null? default) #f)
          (else (car default)))))

(tm-define (csl-attr? x key val)
  (== (csl-attr x key) val))

(tm-define (csl-child x name)
  (:synopsis "The first child called @name of the node @x, or #f")
  (list-find (cddr x) (lambda (c) (and (pair? c) (== (car c) name)))))

(tm-define (csl-children-named x name)
  (list-filter (cddr x) (lambda (c) (and (pair? c) (== (car c) name)))))

(tm-define (csl-text x)
  (:synopsis "The text inside the node @x")
  (apply string-append (list-filter (cddr x) string?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (csl-split s)
  (:synopsis "The words of @s, which are separated by blanks")
  (list-filter (string-tokenize-by-char s #\space)
               (lambda (x) (!= x ""))))

(tm-define (csl-chars s)
  (:synopsis "The characters of the TeXmacs string @s, as strings")
  (tmstring->list s))

(tm-define (csl-upcase s) (tmstring-upcase-all s))
(tm-define (csl-locase s) (tmstring-locase-all s))

(tm-define (csl-upcase-first s)
  (with l (csl-chars s)
    (if (null? l) s
        (apply string-append (csl-upcase (car l)) (cdr l)))))

(tm-define (csl-letter? c)
  ;; @c is one character of csl-chars
  (!= (csl-upcase c) (csl-locase c)))

(tm-define (csl-digit? c)
  (and (== (string-length c) 1) (char-numeric? (string-ref c 0))))

(tm-define (csl-upper? s)
  (:synopsis "Does @s hold letters, none of them in lower case?")
  (and (== s (csl-upcase s)) (!= s (csl-locase s))))

(tm-define (csl-lower? s)
  (:synopsis "Does @s hold no letter in upper case?")
  (== s (csl-locase s)))

(tm-define (csl-string-number? s)
  (and (string? s) (!= s "")
       (list-and (map char-numeric? (string->list s)))))

(tm-define (csl-pad n width)
  (with s (if (number? n) (number->string n) n)
    (if (< (string-length s) width)
        (string-append (make-string (- width (string-length s)) #\0) s)
        s)))

(tm-define (csl-roman n)
  (:synopsis "The number @n in lower case roman numerals")
  (let loop ((n n)
             (l '((1000 . "m") (900 . "cm") (500 . "d") (400 . "cd")
                  (100 . "c") (90 . "xc") (50 . "l") (40 . "xl")
                  (10 . "x") (9 . "ix") (5 . "v") (4 . "iv") (1 . "i")))
             (r ""))
    (cond ((or (<= n 0) (null? l)) r)
          ((>= n (caar l)) (loop (- n (caar l)) l (string-append r (cdar l))))
          (else (loop n (cdr l) r)))))

(tm-define (csl-last-char s)
  (with l (csl-chars s)
    (if (null? l) "" (cAr l))))

(tm-define (csl-first-char s)
  (with l (csl-chars s)
    (if (null? l) "" (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-sorted a b before?)
  (cond ((null? a) b)
        ((null? b) a)
        ((before? (car b) (car a))
         (cons (car b) (merge-sorted a (cdr b) before?)))
        (else (cons (car a) (merge-sorted (cdr a) b before?)))))

(tm-define (csl-sort l before?)
  (:synopsis "The list @l sorted by @before?, equal elements kept in order")
  ;; a merge sort written in Scheme: the same on all Scheme systems
  (with n (length l)
    (if (< n 2) l
        (with h (quotient n 2)
          (merge-sorted (csl-sort (list-head l h) before?)
                        (csl-sort (list-tail l h) before?)
                        before?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rich text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (rt-empty? x)
  (cond ((not x) #t)
        ((string? x) (== x ""))
        ((null? x) #t)
        ((== (car x) 'raw) #f)
        ((== (car x) 'fmt) (list-and (map rt-empty? (cddr x))))
        (else (list-and (map rt-empty? (cdr x))))))

(tm-define (rt-cat . l)
  (:synopsis "The concatenation of the rich texts @l")
  (with r (list-filter l (lambda (x) (not (rt-empty? x))))
    (cond ((null? r) #f)
          ((null? (cdr r)) (car r))
          (else (cons 'cat r)))))

(tm-define (rt-cat* l)
  (apply rt-cat l))

(tm-define (rt-join l delim)
  (:synopsis "The non empty rich texts of @l, separated by @delim")
  (with r (list-filter l (lambda (x) (not (rt-empty? x))))
    (cond ((null? r) #f)
          ((null? (cdr r)) (car r))
          ((rt-empty? delim) (cons 'cat r))
          (else (cons 'cat (list-intersperse r delim))))))

(tm-define (rt-fmt alist x)
  (:synopsis "The rich text @x with the formatting @alist")
  (cond ((rt-empty? x) #f)
        ((null? alist) x)
        (else (list 'fmt alist x))))

(tm-define (rt-affix prefix x suffix)
  (if (rt-empty? x) #f
      (rt-cat prefix x suffix)))

(define (raw->string t)
  (cond ((string? t) t)
        ((and (pair? t) (in? (car t) '(concat document with math em strong
                                       keepcase rsup rsub)))
         (apply string-append (map raw->string (cdr t))))
        ((and (pair? t) (== (car t) 'with) (nnull? (cdr t)))
         (raw->string (cAr t)))
        (else "")))

(tm-define (rt->string x)
  (:synopsis "The text of the rich text @x, without any formatting")
  (cond ((not x) "")
        ((string? x) x)
        ((null? x) "")
        ((== (car x) 'raw) (raw->string (cadr x)))
        ((== (car x) 'fmt) (apply string-append (map rt->string (cddr x))))
        (else (apply string-append (map rt->string (cdr x))))))

(tm-define (rt-map f x)
  (:synopsis "Apply @f to the strings of @x whose case may be changed")
  (cond ((not x) x)
        ((string? x) (f x))
        ((null? x) x)
        ((in? (car x) '(raw nocase)) x)
        ((== (car x) 'fmt) (cons* 'fmt (cadr x) (map (cut rt-map f <>)
                                                    (cddr x))))
        (else (cons (car x) (map (cut rt-map f <>) (cdr x))))))

(tm-define (rt-map-all f x)
  (:synopsis "Apply @f to all the strings of @x")
  (cond ((not x) x)
        ((string? x) (f x))
        ((null? x) x)
        ((== (car x) 'raw) x)
        ((== (car x) 'fmt) (cons* 'fmt (cadr x) (map (cut rt-map-all f <>)
                                                    (cddr x))))
        (else (cons (car x) (map (cut rt-map-all f <>) (cdr x))))))

(tm-define (rt-last-char x)
  (csl-last-char (rt->string x)))

(tm-define (rt-first-char x)
  (csl-first-char (rt->string x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes of case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define title-stop-words
  '("a" "an" "and" "as" "at" "but" "by" "down" "for" "from" "in" "into"
    "nor" "of" "on" "onto" "or" "over" "so" "the" "till" "to" "up" "via"
    "with" "yet"))

(define (split-words l word acc)
  ;; the characters @l as a list of words and separators; a separator is
  ;; a one element list
  (define (flush)
    (if (null? word) acc
        (cons (apply string-append (reverse word)) acc)))
  (cond ((null? l) (reverse (flush)))
        ((or (csl-letter? (car l)) (csl-digit? (car l)) (== (car l) "'")
             (== (car l) "\x27"))
         (split-words (cdr l) (cons (car l) word) acc))
        (else (split-words (cdr l) '() (cons (list (car l)) (flush))))))

(define (capitalize-word w)
  (if (csl-lower? w) (csl-upcase-first w) w))

(define (title-word w upper? first? last? after-colon?)
  (let* ((w* (if upper? (csl-upcase-first (csl-locase w)) w))
         (low (csl-locase w)))
    (cond ((and (in? low title-stop-words) (not first?) (not last?)
                (not after-colon?))
           (if (or upper? (csl-lower? w)) low w))
          (else (capitalize-word w*)))))

(define (title-case-string s upper? first? last?)
  ;; returns the new string; @first? and @last? tell whether the string
  ;; starts and ends the text
  (let* ((toks (split-words (csl-chars s) '() '()))
         (nwords (length (list-filter toks string?))))
    (let loop ((l toks) (i 0) (colon? #f) (prev "") (r '()))
      (cond ((null? l) (apply string-append (reverse r)))
            ((pair? (car l))
             (with c (caar l)
               (loop (cdr l) i
                     (or (in? c '(":" "?" "!")) (and colon? (== c " ")))
                     c (cons c r))))
            (else
              (with w (title-word (car l) upper?
                                  (and first? (== i 0))
                                  (and last? (== i (- nwords 1)))
                                  colon?)
                (loop (cdr l) (+ i 1) #f "" (cons w r))))))))

(define (count-strings x)
  (cond ((string? x) (if (== x "") 0 1))
        ((not (pair? x)) 0)
        ((in? (car x) '(raw nocase)) 1)
        ((== (car x) 'fmt) (apply + (map count-strings (cddr x))))
        (else (apply + (map count-strings (cdr x))))))

(define rt-glued? #f)

(define (rt-map-indexed f x)
  ;; as rt-map, but @f also gets the index of the string among the leaves;
  ;; rt-glued? tells whether the string continues the word before it
  (with i 0
    (define (walk x)
      (cond ((string? x)
             (if (== x "") x
                 (with r (f x i)
                   (set! i (+ i 1))
                   (set! rt-glued? (with c (csl-last-char x)
                                     (or (csl-letter? c) (csl-digit? c))))
                   r)))
            ((not (pair? x)) x)
            ((in? (car x) '(raw nocase))
             (set! i (+ i 1))
             (set! rt-glued? (with c (rt-last-char x)
                               (or (== (car x) 'raw) (csl-letter? c)
                                   (csl-digit? c))))
             x)
            ((== (car x) 'fmt) (cons* 'fmt (cadr x) (map walk (cddr x))))
            (else (cons (car x) (map walk (cdr x))))))
    (set! rt-glued? #f)
    (walk x)))

(define (keep-first-word f s)
  ;; apply @f to @s without the letters which continue a word
  (let loop ((l (csl-chars s)) (r '()))
    (if (and (nnull? l) (or (csl-letter? (car l)) (csl-digit? (car l))))
        (loop (cdr l) (cons (car l) r))
        (string-append (apply string-append (reverse r))
                       (if (null? l) "" (f (apply string-append l)))))))

(tm-define (rt-text-case x how english?)
  (:synopsis "Apply the CSL text-case @how to the rich text @x")
  (cond ((rt-empty? x) x)
        ((== how "lowercase") (rt-map csl-locase x))
        ((== how "uppercase") (rt-map csl-upcase x))
        ((== how "capitalize-first")
         (rt-map-indexed (lambda (s i) (if (== i 0) (capitalize-first s) s))
                         x))
        ((== how "capitalize-all")
         (rt-map (lambda (s)
                   (apply string-append
                          (map (lambda (t) (if (pair? t) (car t)
                                               (capitalize-word t)))
                               (split-words (csl-chars s) '() '()))))
                 x))
        ((== how "sentence")
         (with upper? (csl-upper? (rt->string x))
           (rt-map-indexed
            (lambda (s i)
              (with t (if upper? (csl-locase s) s)
                (if (== i 0) (capitalize-first t) t)))
            x)))
        ((== how "title")
         (if (not english?) x
             (let* ((upper? (csl-upper? (rt->string x)))
                    (n (count-strings x)))
               (rt-map-indexed
                (lambda (s i)
                  (if rt-glued?
                      (keep-first-word
                       (lambda (t)
                         (title-case-string t upper? #f (== i (- n 1))))
                       s)
                      (title-case-string s upper? (== i 0) (== i (- n 1)))))
                x))))
        (else x)))

(define (capitalize-first s)
  ;; the first word in upper case when it is in lower case
  (with toks (split-words (csl-chars s) '() '())
    (let loop ((l toks) (r '()))
      (cond ((null? l) s)
            ((pair? (car l)) (loop (cdr l) (cons (caar l) r)))
            (else (apply string-append
                         (append (reverse r)
                                 (list (capitalize-word (car l)))
                                 (map (lambda (t) (if (pair? t) (car t) t))
                                      (cdr l)))))))))

(tm-define (rt-strip-periods x)
  (rt-map-all (lambda (s) (string-replace s "." "")) x))
