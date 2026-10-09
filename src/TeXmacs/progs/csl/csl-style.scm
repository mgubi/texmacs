
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-style.scm
;; DESCRIPTION : loading of CSL styles and locales, terms
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A style is a hash table with the entries
;;
;;   name            the name under which it was loaded
;;   root            the node cs:style
;;   class           "in-text" or "note"
;;   default-locale  the locale of the style, or #f
;;   macros          a hash table from names to the nodes cs:macro
;;   citation        the node cs:citation
;;   bibliography    the node cs:bibliography, or #f
;;   locales         the nodes cs:locale of the style
;;   title           the title of the style
;;
;; A locale is a hash table with the entries
;;
;;   lang            the code of the locale, such as "en-US"
;;   terms           a hash table from "name|form" to (single . multiple)
;;   genders         a hash table from the names of terms to genders
;;   ordinals        the nodes of the terms ordinal and ordinal-NN
;;   dates           a hash table from "text" and "numeric" to nodes cs:date
;;   options         the attributes of cs:style-options

(texmacs-module (csl csl-style)
  (:use (csl csl-utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define csl-extra-path '())

(tm-define (csl-directories sub)
  (:synopsis "The directories where the files of the kind @sub are sought")
  (append (map (lambda (d) (url-append d sub)) csl-extra-path)
          (list (url-append "$TEXMACS_HOME_PATH/csl" sub)
                (url-append "$TEXMACS_PATH/misc/csl" sub))))

(define (find-file sub name)
  (let loop ((l (csl-directories sub)))
    (cond ((null? l) #f)
          ((url-exists? (url-append (car l) name)) (url-append (car l) name))
          (else (loop (cdr l))))))

(define (load-xml u)
  (and u (url-exists? u)
       (csl-parse (string-load u))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define style-cache (make-ahash-table))

(define (parent-name root)
  ;; the name of the parent of a dependent style
  (and-with info (csl-child root 'info)
    (and-with link (list-find (csl-children-named info 'link)
                              (cut csl-attr? <> 'rel "independent-parent"))
      (and-with href (csl-attr link 'href)
        (with l (string-tokenize-by-char href #\/)
          (and (nnull? l) (cAr l)))))))

(define (make-style name root)
  (let* ((style (make-ahash-table))
         (macros (make-ahash-table))
         (info (csl-child root 'info))
         (title (and info (csl-child info 'title))))
    (for (m (csl-children-named root 'macro))
      (ahash-set! macros (csl-attr m 'name "") m))
    (ahash-set! style 'name name)
    (ahash-set! style 'root root)
    (ahash-set! style 'class (csl-attr root 'class "in-text"))
    (ahash-set! style 'default-locale (csl-attr root 'default-locale))
    (ahash-set! style 'macros macros)
    (ahash-set! style 'citation (csl-child root 'citation))
    (ahash-set! style 'bibliography (csl-child root 'bibliography))
    (ahash-set! style 'locales (csl-children-named root 'locale))
    (ahash-set! style 'title (if title (csl-text title) name))
    style))

(tm-define (csl-style-from-string name s)
  (:synopsis "The style defined by the XML in @s, or #f")
  (and-with root (csl-parse s)
    (and (== (csl-name root) 'style)
         (if (csl-child root 'citation)
             (make-style name root)
             ;; a dependent style: the parent with the locale of the child
             (and-with parent (and-with p (parent-name root)
                                (csl-load-style p))
               (with style (make-ahash-table)
                 (for (p (ahash-table->list parent))
                   (ahash-set! style (car p) (cdr p)))
                 (ahash-set! style 'name name)
                 (when (csl-attr root 'default-locale)
                   (ahash-set! style 'default-locale
                               (csl-attr root 'default-locale)))
                 style))))))

(tm-define (csl-style-file name)
  (:synopsis "The file of the style @name, or #f")
  (find-file "styles" (string-append name ".csl")))

(tm-define (csl-load-style name)
  (:synopsis "The style @name, or #f")
  (or (ahash-ref style-cache name)
      (and-with u (csl-style-file name)
        (and-with style (csl-style-from-string name (string-load u))
          (ahash-set! style-cache name style)
          style))))

(tm-define (csl-forget-styles)
  (set! style-cache (make-ahash-table))
  (set! locale-files (make-ahash-table))
  (set! locale-cache (make-ahash-table)))

(tm-define (csl-style-ref style key)
  (ahash-ref style key))

(tm-define (csl-style-macro style name)
  (ahash-ref (ahash-ref style 'macros) name))

(tm-define (csl-style-option style key . default)
  (:synopsis "The global option @key of @style")
  (apply csl-attr (cons* (ahash-ref style 'root) key default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define locale-files (make-ahash-table))
(define locale-cache (make-ahash-table))

(define primary-dialects
  '(("af" . "af-ZA") ("ar" . "ar") ("bg" . "bg-BG") ("ca" . "ca-AD")
    ("cs" . "cs-CZ") ("cy" . "cy-GB") ("da" . "da-DK") ("de" . "de-DE")
    ("el" . "el-GR") ("en" . "en-US") ("es" . "es-ES") ("et" . "et-EE")
    ("eu" . "eu") ("fa" . "fa-IR") ("fi" . "fi-FI") ("fr" . "fr-FR")
    ("he" . "he-IL") ("hr" . "hr-HR") ("hu" . "hu-HU") ("id" . "id-ID")
    ("is" . "is-IS") ("it" . "it-IT") ("ja" . "ja-JP") ("km" . "km-KH")
    ("ko" . "ko-KR") ("la" . "la") ("lt" . "lt-LT") ("lv" . "lv-LV")
    ("mn" . "mn-MN") ("nb" . "nb-NO") ("nl" . "nl-NL") ("nn" . "nn-NO")
    ("pl" . "pl-PL") ("pt" . "pt-PT") ("ro" . "ro-RO") ("ru" . "ru-RU")
    ("sk" . "sk-SK") ("sl" . "sl-SI") ("sr" . "sr-RS") ("sv" . "sv-SE")
    ("th" . "th-TH") ("tr" . "tr-TR") ("uk" . "uk-UA") ("vi" . "vi-VN")
    ("zh" . "zh-CN")))

(define texmacs-languages
  '(("british" . "en-GB") ("bulgarian" . "bg-BG") ("chinese" . "zh-CN")
    ("croatian" . "hr-HR") ("czech" . "cs-CZ") ("danish" . "da-DK")
    ("dutch" . "nl-NL") ("english" . "en-US") ("finnish" . "fi-FI")
    ("french" . "fr-FR") ("german" . "de-DE") ("greek" . "el-GR")
    ("hungarian" . "hu-HU") ("italian" . "it-IT") ("japanese" . "ja-JP")
    ("korean" . "ko-KR") ("polish" . "pl-PL") ("portuguese" . "pt-PT")
    ("romanian" . "ro-RO") ("russian" . "ru-RU") ("slovak" . "sk-SK")
    ("slovene" . "sl-SI") ("spanish" . "es-ES") ("swedish" . "sv-SE")
    ("taiwanese" . "zh-TW") ("ukrainian" . "uk-UA")))

(tm-define (csl-language->locale lan)
  (:synopsis "The code of the CSL locale for the TeXmacs language @lan")
  (with p (assoc lan texmacs-languages)
    (if p (cdr p) "en-US")))

(define (lang-prefix lang)
  (with i (string-index lang #\-)
    (if i (substring lang 0 i) lang)))

(define (locale-file lang)
  ;; the root node of the file of the locale @lang, or #f
  (when (not (ahash-ref locale-files lang))
    (ahash-set! locale-files lang
                (or (load-xml (find-file "locales"
                                         (string-append "locales-" lang
                                                        ".xml")))
                    'none)))
  (with r (ahash-ref locale-files lang)
    (and (!= r 'none) r)))

(define (locale-sources style lang)
  ;; the nodes cs:locale which apply to @lang, the most specific first
  (let* ((own (if style (ahash-ref style 'locales) '()))
         (pre (lang-prefix lang))
         (dialect (with p (assoc pre primary-dialects)
                    (if p (cdr p) lang)))
         (xml-lang (lambda (x) (csl-attr x 'xml:lang))))
    (list-filter
     (append (list-filter own (lambda (x) (== (xml-lang x) lang)))
             (if (== pre lang) '()
                 (list-filter own (lambda (x) (== (xml-lang x) pre))))
             (list-filter own (lambda (x) (not (xml-lang x))))
             (list (locale-file lang))
             (if (== dialect lang) '() (list (locale-file dialect)))
             (if (in? "en-US" (list lang dialect)) '()
                 (list (locale-file "en-US"))))
     identity)))

(define (ordinal-term? name)
  (string-starts? name "ordinal"))

(define (add-terms! locale src ordinals?)
  (let* ((terms (ahash-ref locale 'terms))
         (genders (ahash-ref locale 'genders)))
    (for (group (csl-children-named src 'terms))
      (for (term (csl-children-named group 'term))
        (let* ((name (csl-attr term 'name ""))
               (form (csl-attr term 'form "long"))
               (key (string-append name "|" form))
               (single (csl-child term 'single))
               (multiple (csl-child term 'multiple))
               (val (if (or single multiple)
                        (cons (if single (csl-text single) "")
                              (if multiple (csl-text multiple) ""))
                        (cons (csl-text term) (csl-text term)))))
          (cond ((ordinal-term? name)
                 (when ordinals?
                   (ahash-set! locale 'ordinals
                               (cons term (ahash-ref locale 'ordinals)))))
                ((not (ahash-ref terms key))
                 (ahash-set! terms key val)))
          (when (and (csl-attr term 'gender) (not (ahash-ref genders name)))
            (ahash-set! genders name (csl-attr term 'gender))))))))

(define (has-ordinals? src)
  (list-or (map (lambda (group)
                  (list-or (map (lambda (term)
                                  (ordinal-term? (csl-attr term 'name "")))
                                (csl-children-named group 'term))))
                (csl-children-named src 'terms))))

(define (make-locale style lang)
  (let* ((locale (make-ahash-table))
         (dates (make-ahash-table))
         (ordinals-done? #f))
    (ahash-set! locale 'lang lang)
    (ahash-set! locale 'terms (make-ahash-table))
    (ahash-set! locale 'genders (make-ahash-table))
    (ahash-set! locale 'ordinals '())
    (ahash-set! locale 'dates dates)
    (ahash-set! locale 'options '())
    (for (src (locale-sources style lang))
      (with ord? (and (not ordinals-done?) (has-ordinals? src))
        (add-terms! locale src ord?)
        (when ord? (set! ordinals-done? #t)))
      (for (d (csl-children-named src 'date))
        (with form (csl-attr d 'form "text")
          (when (not (ahash-ref dates form))
            (ahash-set! dates form d))))
      (for (o (csl-children-named src 'style-options))
        (ahash-set! locale 'options
                    (append (ahash-ref locale 'options) (csl-attrs o)))))
    locale))

(tm-define (csl-locale style lang)
  (:synopsis "The locale @lang as seen by @style")
  (let* ((lang* (or lang (and style (ahash-ref style 'default-locale))
                    "en-US"))
         (key (string-append (if style (ahash-ref style 'name) "") "|"
                             lang*)))
    (or (ahash-ref locale-cache key)
        (with locale (make-locale style lang*)
          (ahash-set! locale-cache key locale)
          locale))))

(tm-define (csl-locale-lang locale)
  (ahash-ref locale 'lang))

(tm-define (csl-locale-option locale key . default)
  (with p (assq key (ahash-ref locale 'options))
    (cond (p (cdr p))
          ((null? default) #f)
          (else (car default)))))

(tm-define (csl-locale-date locale form)
  (ahash-ref (ahash-ref locale 'dates) form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (form-fallback form)
  (cond ((== form "verb-short") "verb")
        ((== form "symbol") "short")
        ((== form "long") #f)
        (else "long")))

(tm-define (csl-term locale name form plural?)
  (:synopsis "The term @name of @locale, or #f when it is not defined")
  (let loop ((form (or form "long")))
    (with val (ahash-ref (ahash-ref locale 'terms)
                         (string-append name "|" form))
      (cond (val (if plural? (cdr val) (car val)))
            ((form-fallback form) (loop (form-fallback form)))
            (else #f)))))

(tm-define (csl-term-gender locale name)
  (ahash-ref (ahash-ref locale 'genders) name))

(define (ordinal-matches? term n gender)
  (let* ((name (csl-attr term 'name ""))
         (nr (and (> (string-length name) 8)
                  (string->number (substring name 8 (string-length name)))))
         (match (csl-attr term 'match
                          (if (and nr (< nr 10)) "last-digit"
                              "last-two-digits")))
         (gf (csl-attr term 'gender-form)))
    (and nr
         (== gf gender)
         (cond ((== match "whole-number") (== n nr))
               ((== match "last-two-digits") (== (modulo n 100) nr))
               (else (== (modulo n 10) nr))))))

(define (ordinal-rank term)
  ;; the more specific terms first
  (let* ((name (csl-attr term 'name ""))
         (nr (string->number (substring name 8 (string-length name))))
         (match (csl-attr term 'match
                          (if (and nr (< nr 10)) "last-digit"
                              "last-two-digits"))))
    (cond ((== match "whole-number") 0)
          ((== match "last-two-digits") 1)
          (else 2))))

(tm-define (csl-ordinal-suffix locale n gender)
  (:synopsis "The suffix of the ordinal of the number @n")
  (let* ((all (ahash-ref locale 'ordinals))
         (numbered (list-filter all (lambda (t)
                                      (!= (csl-attr t 'name) "ordinal"))))
         (find (lambda (g)
                 (with l (list-filter numbered
                                      (cut ordinal-matches? <> n g))
                   (and (nnull? l)
                        (car (csl-sort l (lambda (a b) (< (ordinal-rank a)
                                                          (ordinal-rank b)))))))))
         (generic (lambda (g)
                    (list-find all (lambda (t)
                                     (and (== (csl-attr t 'name) "ordinal")
                                          (== (csl-attr t 'gender-form)
                                              g))))))
         (term (or (and gender (find gender)) (find #f)
                   (and gender (generic gender)) (generic #f))))
    (if term (csl-text term) "")))
