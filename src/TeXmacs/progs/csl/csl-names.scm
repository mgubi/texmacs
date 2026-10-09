
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-names.scm
;; DESCRIPTION : formatting of lists of names for the CSL processor
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The options of a list of names are given by a procedure (opt key default)
;; which looks into the attributes of cs:name and into the options
;; inherited from cs:citation, cs:bibliography and cs:style. The other
;; settings come in an association list:
;;
;;   parts       the nodes cs:name-part
;;   formatting  the formatting of cs:name, an association list
;;   et-al       the rich text for "et al."
;;   and         the rich text for "and", or #f
;;   demote      the option demote-non-dropping-particle
;;   hyphen?     the option initialize-with-hyphen
;;   subsequent? whether the options et-al-subsequent-* apply
;;   others?     whether the list is known to be incomplete
;;   more        the number of names to show beyond et-al-use-first
;;   english?    whether title case applies

(texmacs-module (csl csl-names)
  (:use (csl csl-utils) (csl csl-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (given-tokens given)
  ;; "Jean-Paul A.B." -> ("Jean" "-" "Paul" " " "A." "B.")
  (let loop ((l (csl-chars given)) (word '()) (r '()))
    (define (flush)
      (if (null? word) r (cons (apply string-append (reverse word)) r)))
    (cond ((null? l) (reverse (flush)))
          ((== (car l) " ")
           (with r* (flush)
             (loop (cdr l) '()
                   (if (or (null? r*) (in? (car r*) '(" " "-"))) r*
                       (cons " " r*)))))
          ((== (car l) "-") (loop (cdr l) '() (cons "-" (flush))))
          ((== (car l) ".")
           (with r* (if (null? word) r
                        (cons (apply string-append (reverse (cons "." word)))
                              r))
             (loop (cdr l) '() r*)))
          (else (loop (cdr l) (cons (car l) word) r)))))

(define (initial-token? t)
  ;; "A." or "A": something which is already an initial
  (let* ((s (if (string-ends? t ".") (string-drop-right t 1) t))
         (l (csl-chars s)))
    (and (nnull? l)
         (or (string-ends? t ".")
             (and (null? (cdr l)) (csl-upper? s))))))

(define (token-initial t)
  (let* ((s (if (string-ends? t ".") (string-drop-right t 1) t))
         (l (csl-chars s)))
    (cond ((null? l) "")
          ;; several capitals written together: "JP"
          ((and (csl-upper? s) (string-ends? t ".")) s)
          (else (csl-upcase (car l))))))

(tm-define (csl-initialize given iw initialize? hyphen?)
  (:synopsis "The given names @given with initials followed by @iw")
  (let* ((toks (given-tokens given))
         (iw-trim (string-trim-right iw)))
    (let loop ((l toks) (r '()))
      (cond ((null? l)
             (string-trim-both (apply string-append (reverse r))))
            ((== (car l) " ")
             ;; the blank is kept after a full name
             (loop (cdr l)
                   (if (and (nnull? r) (not (string-ends? (car r) iw))
                            (not (string-ends? (car r) " ")))
                       (cons " " r) r)))
            ((== (car l) "-")
             (with r* (if (and (nnull? r) (string-ends? (car r) iw)
                               (!= iw iw-trim))
                          (cons iw-trim
                                (cons (string-drop-right
                                       (car r) (string-length iw))
                                      (cdr r)))
                          r)
               (loop (cdr l) (if hyphen? (cons "-" r*) r*))))
            ((and (csl-lower? (car l)) (not (initial-token? (car l))))
             ;; a particle inside the given names
             (loop (cdr l) (cons (string-append (car l) " ") r)))
            ((or initialize? (initial-token? (car l)))
             (loop (cdr l)
                   (cons (string-append (token-initial (car l)) iw) r)))
            (else (loop (cdr l) (cons (car l) r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apostrophe (utf8->cork "’"))
(define ellipsis (utf8->cork "…"))

(define (join-particle particle x)
  ;; no blank after d' or al-
  (cond ((rt-empty? particle) x)
        ((rt-empty? x) particle)
        ((in? (rt-last-char particle) (list "'" "-" apostrophe))
         (rt-cat particle x))
        (else (rt-cat particle " " x))))

(define (spaced . l)
  (rt-join l " "))

(define (part-node settings which)
  (list-find (or (assq-ref* settings 'parts) '())
             (lambda (p) (csl-attr? p 'name which))))

(define (assq-ref* l key)
  (with p (assq key l)
    (and p (cdr p))))

(define formatting-keys
  '(font-style font-variant font-weight text-decoration vertical-align))

(tm-define (csl-node-formatting node)
  (:synopsis "The formatting attributes of @node, an association list")
  (list-filter (csl-attrs node) (lambda (p) (in? (car p) formatting-keys))))

(define (format-part settings which x)
  ;; apply the cs:name-part @which to the rich text @x
  (with node (part-node settings which)
    (if (or (not node) (rt-empty? x)) x
        (let* ((tc (csl-attr node 'text-case))
               (x1 (if tc (rt-text-case x tc (assq-ref* settings 'english?))
                       x))
               (x2 (rt-fmt (csl-node-formatting node) x1)))
          (rt-affix (csl-attr node 'prefix) x2 (csl-attr node 'suffix))))))

(define (name-given name opt settings)
  (let* ((given (csl-name-ref name 'given))
         (iw (opt 'initialize-with #f)))
    (cond ((not given) #f)
          ((and iw (string? given))
           (csl-initialize given iw (!= (opt 'initialize "true") "false")
                           (assq-ref* settings 'hyphen?)))
          (else given))))

(define (format-name name opt settings inverted?)
  (let* ((literal (csl-name-ref name 'literal))
         (family (csl-name-ref name 'family))
         (given (name-given name opt settings))
         (dp (csl-name-ref name 'dropping-particle))
         (ndp (csl-name-ref name 'non-dropping-particle))
         (suffix (csl-name-ref name 'suffix))
         (form (opt 'form "long"))
         (sep (opt 'sort-separator ", "))
         (demote? (== (assq-ref* settings 'demote) "display-and-sort"))
         (fam (lambda (x) (format-part settings "family" x)))
         (giv (lambda (x) (format-part settings "given" x))))
    (cond (literal (fam literal))
          ((not family) (giv given))
          ((== form "short") (fam (join-particle ndp family)))
          ((csl-name-ref name 'static-ordering)
           (spaced (fam family) (giv given)))
          (inverted?
           (rt-join
            (list (if demote? (fam family) (fam (join-particle ndp family)))
                  (if demote? (giv (spaced given dp ndp))
                      (giv (spaced given dp)))
                  suffix)
            sep))
          (else
            (with main (spaced (giv given)
                               (join-particle
                                (giv dp) (fam (join-particle ndp family))))
              (cond ((rt-empty? suffix) main)
                    ((csl-name-ref name 'comma-suffix)
                     (rt-cat main ", " suffix))
                    (else (rt-cat main " " suffix))))))))

(tm-define (csl-name-sort-key name demote)
  (:synopsis "The text by which the name @name is sorted")
  (let* ((text (lambda (k) (rt->string (csl-name-ref name k))))
         (literal (text 'literal))
         (parts (if (== demote "never")
                    (list (text 'non-dropping-particle) (text 'family)
                          (text 'dropping-particle) (text 'given)
                          (text 'suffix))
                    (list (text 'family) (text 'dropping-particle)
                          (text 'non-dropping-particle) (text 'given)
                          (text 'suffix)))))
    (if (!= literal "") literal
        (string-recompose (list-filter parts (lambda (x) (!= x ""))) " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists of names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opt-number opt key)
  (with v (opt key #f)
    (and v (string->number v))))

(tm-define (csl-names-shown names opt settings)
  (:synopsis "How many of @names are shown, and whether et al. follows")
  ;; returns (count . et-al?)
  (let* ((n (length names))
         (sub? (assq-ref* settings 'subsequent?))
         (min (or (and sub? (opt-number opt 'et-al-subsequent-min))
                  (opt-number opt 'et-al-min)))
         (use (or (and sub? (opt-number opt 'et-al-subsequent-use-first))
                  (opt-number opt 'et-al-use-first)))
         (more (or (assq-ref* settings 'more) 0)))
    (cond ((and min use (>= n min) (< (+ use more) n))
           (cons (+ use more) #t))
          (else (cons n (assq-ref* settings 'others?))))))

(define (use-delimiter? rule count inverted-before?)
  ;; for delimiter-precedes-last and delimiter-precedes-et-al
  (cond ((== rule "always") #t)
        ((== rule "never") #f)
        ((== rule "after-inverted-name") inverted-before?)
        (else (> count 2))))

(tm-define (csl-format-names names opt settings)
  (:synopsis "The rich text for the list of names @names")
  (let* ((n (length names))
         (shown (csl-names-shown names opt settings))
         (count (car shown))
         (et-al? (cdr shown))
         (order (opt 'name-as-sort-order #f))
         (inverted? (lambda (i)
                      (and (or (== order "all")
                               (and (== order "first") (== i 0)))
                           (with name (list-ref names i)
                             (and (csl-name-ref name 'family)
                                  (csl-name-ref name 'given)
                                  (not (csl-name-ref name 'literal))
                                  (not (csl-name-ref name
                                                     'static-ordering)))))))
         (delim (opt 'delimiter ", "))
         (fmt (or (assq-ref* settings 'formatting) '()))
         (one (lambda (name i)
                (rt-affix (opt 'prefix #f)
                          (rt-fmt fmt (format-name name opt settings
                                                   (inverted? i)))
                          (opt 'suffix #f))))
         (first (map one (list-head names count) (iota count)))
         (and-text (assq-ref* settings 'and))
         (last? (and et-al? (== (opt 'et-al-use-last "false") "true")
                     (>= n (+ count 2)))))
    (cond ((== (opt 'form "long") "count") (number->string count))
          ((null? first) #f)
          (last?
           (rt-cat (rt-join first delim) delim ellipsis " "
                   (one (cAr names) (- n 1))))
          (et-al?
           (with delim? (use-delimiter?
                         (opt 'delimiter-precedes-et-al "contextual")
                         (+ count 1) (inverted? (- count 1)))
             (rt-cat (rt-join first delim)
                     (if (rt-empty? (assq-ref* settings 'et-al)) #f
                         (rt-cat (if delim? delim " ")
                                 (assq-ref* settings 'et-al))))))
          ((or (not and-text) (== count 1)) (rt-join first delim))
          (else
            (with delim? (use-delimiter?
                          (opt 'delimiter-precedes-last "contextual")
                          count (inverted? (- count 2)))
              (rt-cat (rt-join (cDr first) delim)
                      (if delim? delim " ")
                      and-text " "
                      (cAr first)))))))
