
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-output.scm
;; DESCRIPTION : the rich text of the CSL processor as TeXmacs trees or HTML
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rich text is first made flat, as a list of events
;;
;;   (text s)       a string
;;   (raw t)        a TeXmacs tree
;;   (open k v)     the start of text with the formatting k = v
;;   (close k v)    its end
;;   (quote s)      a closing quote
;;
;; on which the quotes are resolved, the formatting inside the same
;; formatting is switched off, the punctuation moves inside the quotes if
;; the locale asks for it and double punctuation is removed. The events
;; are then written as a TeXmacs tree, or as the HTML which the test suite
;; of CSL expects.

(texmacs-module (csl csl-output)
  (:use (csl csl-utils) (csl csl-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flattening
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flip k v active)
  ;; formatting inside the same formatting switches it off
  (with a (assq k active)
    (if (and a (== (cdr a) v) (in? k '(font-style font-weight font-variant)))
        "normal" v)))

(define (flatten x locale depth active)
  ;; returns the events in order
  (cond ((not x) '())
        ((string? x) (if (== x "") '() (list (list 'text x))))
        ((null? x) '())
        ((== (car x) 'raw) (list x))
        ((== (car x) 'fmt)
         (let* ((alist (cadr x))
                (quotes? (assq 'quotes alist))
                (keys (list-filter alist (lambda (p) (!= (car p) 'quotes))))
                (keys* (map (lambda (p) (cons (car p)
                                              (flip (car p) (cdr p) active)))
                            keys))
                (active* (append keys* active))
                (inner (append-map
                        (lambda (y) (flatten y locale
                                             (if quotes? (+ depth 1) depth)
                                             active*))
                        (cddr x)))
                (odd? (== (modulo depth 2) 0))
                (quoted
                 (if (not quotes?) inner
                     (append
                      (list (list 'text
                                  (or (csl-term locale
                                                (if odd? "open-quote"
                                                    "open-inner-quote")
                                                #f #f)
                                      "\"")))
                      inner
                      (list (list 'quote
                                  (or (csl-term locale
                                                (if odd? "close-quote"
                                                    "close-inner-quote")
                                                #f #f)
                                      "\"")))))))
           (append (map (lambda (p) (list 'open (car p) (cdr p))) keys*)
                   quoted
                   (map (lambda (p) (list 'close (car p) (cdr p)))
                        (reverse keys*)))))
        (else (append-map (lambda (y) (flatten y locale depth active))
                          (cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Punctuation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-text l)
  ;; the first event of @l which holds text, behind the ends of formatting
  (cond ((null? l) #f)
        ((== (caar l) 'text) (car l))
        ((== (caar l) 'close) (next-text (cdr l)))
        (else #f)))

(define (drop-first-char-of-next-text l)
  (cond ((null? l) l)
        ((== (caar l) 'text)
         (with s (apply string-append (cdr (csl-chars (cadar l))))
           (if (== s "") (cdr l) (cons (list 'text s) (cdr l)))))
        (else (cons (car l) (drop-first-char-of-next-text (cdr l))))))

(define (move-punctuation l)
  ;; a comma or a period after a closing quote goes before it
  (let loop ((l l) (r '()) (last ""))
    (cond ((null? l) (reverse r))
          ((== (caar l) 'quote)
           (let* ((t (next-text (cdr l)))
                  (c (if t (csl-first-char (cadr t)) "")))
             (if (in? c '("," "."))
                 (loop (drop-first-char-of-next-text (cdr l))
                       (cons (car l)
                             (if (and (== c ".") (in? last '("." "!" "?")))
                                 r
                                 (cons (list 'text c) r)))
                       last)
                 (loop (cdr l) (cons (car l) r) last))))
          ((== (caar l) 'text)
           (loop (cdr l) (cons (car l) r) (csl-last-char (cadar l))))
          (else (loop (cdr l) (cons (car l) r) last)))))

(define (clash? last c)
  ;; is the character @c dropped after the character @last?
  (cond ((== c ".") (in? last '("." "!" "?")))
        ((== c ",") (in? last '("," ";")))
        ((== c ";") (in? last '(";")))
        ((== c ":") (in? last '(":")))
        ((== c " ") (in? last '(" ")))
        (else #f)))

(define (collapse-punctuation l)
  (let loop ((l l) (r '()) (last ""))
    (cond ((null? l) (reverse r))
          ((== (caar l) 'text)
           (let* ((s (cadar l))
                  (chars (csl-chars s))
                  (s* (if (and (nnull? chars) (clash? last (car chars)))
                          (apply string-append (cdr chars))
                          s)))
             (if (== s* "")
                 (loop (cdr l) r last)
                 (loop (cdr l) (cons (list 'text s*) r)
                       (csl-last-char s*)))))
          ((== (caar l) 'quote)
           (loop (cdr l) (cons (list 'text (cadar l)) r) last))
          ((== (caar l) 'raw) (loop (cdr l) (cons (car l) r) ""))
          (else (loop (cdr l) (cons (car l) r) last)))))

(tm-define (csl-events x locale)
  (:synopsis "The events for the rich text @x")
  (let* ((l (flatten x locale 0 '()))
         (in-quote? (== (csl-locale-option locale 'punctuation-in-quote
                                           "false")
                        "true")))
    (collapse-punctuation (if in-quote? (move-punctuation l) l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML, as in the test suite of CSL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (html-escape s)
  (string-replace (string-replace (string-replace s "&" "&#38;")
                                  "<" "&#60;")
                  ">" "&#62;"))

(define (html-tag k v open?)
  (let* ((span (lambda (style)
                 (if open? (string-append "<span style=\"" style "\">")
                     "</span>")))
         (tag (lambda (name)
                (if open? (string-append "<" name ">")
                    (string-append "</" name ">"))))
         (div (lambda (class)
                (if open? (string-append "<div class=\"" class "\">")
                    "</div>"))))
    (cond ((and (== k 'font-style) (== v "italic")) (tag "i"))
          ((and (== k 'font-style) (== v "oblique")) (tag "em"))
          ((== k 'font-style) (span "font-style:normal;"))
          ((and (== k 'font-weight) (== v "bold")) (tag "b"))
          ((and (== k 'font-weight) (== v "light")) (span "font-weight:lighter;"))
          ((== k 'font-weight) (span "font-weight:normal;"))
          ((and (== k 'font-variant) (== v "small-caps"))
           (span "font-variant:small-caps;"))
          ((== k 'font-variant) (span "font-variant:normal;"))
          ((and (== k 'text-decoration) (== v "underline"))
           (span "text-decoration:underline;"))
          ((== k 'text-decoration) (span "text-decoration:none;"))
          ((and (== k 'vertical-align) (== v "sup")) (tag "sup"))
          ((and (== k 'vertical-align) (== v "sub")) (tag "sub"))
          ((== k 'vertical-align) (span "baseline"))
          ((== k 'display) (div (string-append "csl-" v)))
          (else ""))))

(define (html-text s)
  ;; line breaks are not characters of the TeXmacs encoding
  (string-recompose
   (map (lambda (x)
          (html-escape (string-replace (cork->utf8 x) "'" "’")))
        (string-tokenize-by-char s #\newline))
   "\n"))

(tm-define (csl->html x locale)
  (:synopsis "The rich text @x as HTML, in UTF-8")
  (apply string-append
         (map (lambda (e)
                (cond ((== (car e) 'text) (html-text (cadr e)))
                      ((== (car e) 'open) (html-tag (cadr e) (caddr e) #t))
                      ((== (car e) 'close) (html-tag (cadr e) (caddr e) #f))
                      ((== (car e) 'raw)
                       (html-text (rt->string e)))
                      (else "")))
              (csl-events x locale))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-wrap k v body)
  (cond ((and (== k 'font-style) (== v "italic"))
         `(with "font-shape" "italic" ,body))
        ((and (== k 'font-style) (== v "oblique"))
         `(with "font-shape" "slanted" ,body))
        ((== k 'font-style) `(with "font-shape" "right" ,body))
        ((and (== k 'font-weight) (== v "bold"))
         `(with "font-series" "bold" ,body))
        ((and (== k 'font-weight) (== v "light"))
         `(with "font-series" "light" ,body))
        ((== k 'font-weight) `(with "font-series" "medium" ,body))
        ((and (== k 'font-variant) (== v "small-caps"))
         `(with "font-shape" "small-caps" ,body))
        ((== k 'font-variant) `(with "font-shape" "right" ,body))
        ((and (== k 'text-decoration) (== v "underline")) `(underline ,body))
        ((and (== k 'vertical-align) (== v "sup")) `(rsup ,body))
        ((and (== k 'vertical-align) (== v "sub")) `(rsub ,body))
        (else body)))

(define (tm-concat l)
  ;; the concatenation of the trees @l, adjacent strings merged
  (let loop ((l l) (r '()))
    (cond ((null? l)
           (with r* (reverse r)
             (cond ((null? r*) "")
                   ((null? (cdr r*)) (car r*))
                   (else (cons 'concat r*)))))
          ((and (string? (car l)) (nnull? r) (string? (car r)))
           (loop (cdr l) (cons (string-append (car r) (car l)) (cdr r))))
          ((func? (car l) 'concat) (loop (append (cdar l) (cdr l)) r))
          ((== (car l) "") (loop (cdr l) r))
          (else (loop (cdr l) (cons (car l) r))))))

(define (build-tree l)
  ;; returns (trees . remaining events after the closing event)
  (let loop ((l l) (r '()))
    (cond ((null? l) (cons (reverse r) '()))
          ((== (caar l) 'text) (loop (cdr l) (cons (cadar l) r)))
          ((== (caar l) 'raw) (loop (cdr l) (cons (cadar l) r)))
          ((== (caar l) 'close) (cons (reverse r) (cdr l)))
          ((== (caar l) 'open)
           (let* ((sub (build-tree (cdr l)))
                  (body (tm-concat (car sub))))
             (loop (cdr sub)
                   (if (== body "") r
                       (cons (tm-wrap (cadar l) (caddar l) body) r)))))
          (else (loop (cdr l) r)))))

(tm-define (csl->texmacs x locale)
  (:synopsis "The rich text @x as a TeXmacs tree")
  (tm-concat (car (build-tree (csl-events x locale)))))
