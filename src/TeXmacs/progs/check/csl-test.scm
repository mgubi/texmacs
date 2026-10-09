
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-test.scm
;; DESCRIPTION : tests of the processor of the Citation Style Language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The CSL processor (progs/csl):
;;
;;   - the helpers: changes of case, initials, numbers and page ranges;
;;   - the items made from BibTeX entries and from CSL-JSON;
;;   - the rendering elements, with a small style written here;
;;   - the styles and the locales which come with TeXmacs: the
;;     bibliographies of three references in IEEE, APA, Nature and AMS,
;;     which were compared with those of pandoc --citeproc;
;;   - the bibliography of a document whose style is csl-NAME.
;;
;; The fixtures of the test suite of CSL are not distributed with TeXmacs:
;; csl-run-fixtures of (csl csl-fixtures) runs them from a checkout.

(texmacs-module (check csl-test)
  (:use (check check-lib)
        (convert bibtex init-bibtex)
        (csl csl-utils) (csl csl-style) (csl csl-data) (csl csl-names)
        (csl csl-render) (csl csl-process) (csl csl-cite) (csl csl-output) (csl csl-bib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (u s) (utf8->cork s))

(define sample-bib
  (string-append
   "@article{knuth84, author={Donald E. Knuth}, title={Literate Programming},"
   " journal={The Computer Journal}, year=1984, volume=27, number=2,"
   " pages={97--111}, doi={10.1093/comjnl/27.2.97}}\n"
   "@book{knuth86, author={Knuth, Donald E.}, title={The {\\TeX}book},"
   " publisher={Addison-Wesley}, address={Reading, MA}, year=1986}\n"
   "@article{knuth86b, author={Knuth, Donald E. and van der Hoeven, Joris},"
   " title={On $L^2$ estimates}, journal={J. Math.}, year=1986, volume=1,"
   " pages={1--2}}\n"))

(define (bib-parse s)
  ;; the entries of the .bib string @s, as generate_bibliography gets them
  (with d (convert s "bibtex-document" "bibtex-stree")
    (with b (assoc 'body (cdadr d))
      (cadr b))))

(define (sample-items) (csl-bib->items (bib-parse sample-bib)))

(define (item-of key)
  (list-find (sample-items) (lambda (i) (== (csl-item-id i) key))))

(define (flat x)
  ;; the text of a TeXmacs tree
  (cond ((string? x) x)
        ((or (func? x 'bibitem*) (func? x 'label) (func? x 'set-binding)) "")
        ((func? x 'with) (flat (cAr x)))
        ((func? x 'TeX) "TeX")
        ((func? x 'rsup) (string-append "^" (flat (cadr x))))
        ((pair? x) (apply string-append (map flat (cdr x))))
        (else "")))

(define (first-of tag x)
  (cond ((func? x tag) x)
        ((pair? x) (or (first-of tag (car x)) (first-of tag (cdr x))))
        (else #f)))

(define (bib-of style)
  ;; the bibliography of the sample in @style: a list of (text key entry)
  (let* ((r (csl-bib-process "bib" style (bib-parse sample-bib)
                             '("knuth86b" "knuth84" "knuth86") "en-US"))
         (l (first-of 'bib-list r)))
    (if (not l) r
        (map (lambda (e)
               (list (flat (cadr (first-of 'bibitem* e)))
                     (string-drop (cadr (first-of 'label e)) 4)
                     (flat e)))
             (cdr (caddr l))))))

(define small-style
  (string-append
   "<style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"in-text\""
   " version=\"1.0\" demote-non-dropping-particle=\"never\">"
   "<info><title>Small</title><id>small</id></info>"
   "<macro name=\"author\"><names variable=\"author\">"
   "<name and=\"text\" initialize-with=\". \" name-as-sort-order=\"first\"/>"
   "<substitute><text variable=\"title\"/></substitute></names></macro>"
   "<citation et-al-min=\"3\" et-al-use-first=\"1\">"
   "<layout prefix=\"(\" suffix=\")\" delimiter=\"; \">"
   "<group delimiter=\", \"><names variable=\"author\">"
   "<name form=\"short\" and=\"symbol\"/></names>"
   "<date variable=\"issued\"><date-part name=\"year\"/></date>"
   "<group><label variable=\"locator\" form=\"short\" suffix=\" \"/>"
   "<text variable=\"locator\"/></group></group></layout></citation>"
   "<bibliography><sort><key macro=\"author\"/>"
   "<key variable=\"issued\" sort=\"descending\"/></sort>"
   "<layout suffix=\".\"><group delimiter=\". \">"
   "<text macro=\"author\"/>"
   "<date variable=\"issued\" form=\"text\" date-parts=\"year-month\"/>"
   "<text variable=\"title\" font-style=\"italic\" text-case=\"title\"/>"
   "<group delimiter=\" \"><text variable=\"container-title\" quotes=\"true\"/>"
   "<group><text variable=\"volume\"/>"
   "<text variable=\"issue\" prefix=\"(\" suffix=\")\"/></group></group>"
   "<group delimiter=\" \"><label variable=\"page\" form=\"short\"/>"
   "<text variable=\"page\"/></group>"
   "<choose><if type=\"book\" match=\"any\">"
   "<text variable=\"publisher\"/><text variable=\"publisher-place\"/>"
   "</if></choose>"
   "</group></layout></bibliography></style>"))

(define (small-proc)
  (csl-make-processor (csl-style-from-string "small" small-style) "en-US"
                      (sample-items)))

(define (small-html x)
  (csl->html x (csl-processor-locale (small-proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers of the processor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-utils)
  (check-group "text case")
  (check= (rt-text-case "the art of computer programming" "title" #t)
          "The Art of Computer Programming")
  (check= (rt-text-case "a tale: of two cities" "title" #t)
          "A Tale: Of Two Cities")
  (check= (rt-text-case "the art of programming" "title" #f)
          "the art of programming")
  (check= (rt-text-case "THE ART" "sentence" #t) "The art")
  (check= (rt-text-case "the iPhone and NASA" "capitalize-first" #t)
          "The iPhone and NASA")
  (check= (rt-text-case "ab cd" "capitalize-all" #t) "Ab Cd")
  (check= (rt-text-case (u "école") "uppercase" #t) (u "ÉCOLE"))
  ;; protected text and formulas keep their case
  (check= (rt-text-case '(cat "on " (nocase "pH") " and " (raw (math "x")))
                        "uppercase" #t)
          '(cat "ON " (nocase "pH") " AND " (raw (math "x"))))
  (check-group "initials")
  (check= (csl-initialize "Donald Ervin" ". " #t #t) "D. E.")
  (check= (csl-initialize "Donald Ervin" "." #t #t) "D.E.")
  (check= (csl-initialize "Donald Ervin" "" #t #t) "DE")
  (check= (csl-initialize "Jean-Paul" "." #t #t) "J.-P.")
  (check= (csl-initialize "Jean-Paul" "." #t #f) "J.P.")
  (check= (csl-initialize "D. E." "." #t #t) "D.E.")
  (check= (csl-initialize "Donald E." ". " #f #t) "Donald E.")
  (check-group "numbers")
  (check= (csl-number-tokens "12") '("12"))
  (check= (csl-number-tokens "1 - 3, 5") '("1" "-" "3" "," "5"))
  (check= (csl-number-tokens "2nd edition") #f)
  (check-true (csl-numeric? "2b"))
  (check-false (csl-numeric? "second"))
  (check= (csl-roman 1984) "mcmlxxxiv")
  (check= (csl-pad 7 2) "07")
  (check-group "sorting")
  (check= (csl-sort '(3 1 2) <) '(1 2 3))
  ;; equal elements stay in order
  (check= (csl-sort '((1 . a) (0 . b) (1 . c) (0 . d))
                    (lambda (x y) (< (car x) (car y))))
          '((0 . b) (0 . d) (1 . a) (1 . c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-items)
  (check-group "items from BibTeX")
  (with i (item-of "knuth84")
    (check= (csl-item-type i) "article-journal")
    ;; titles become sentences, the names of journals do not change
    (check= (csl-item-ref i "title") "Literate programming")
    (check= (csl-item-ref i "container-title") "The Computer Journal")
    (check= (csl-item-ref i "issue") "2")
    (check= (csl-item-ref i "page") "97-111")
    (check= (csl-item-ref i "DOI") "10.1093/comjnl/27.2.97")
    (check= (csl-date-start (csl-item-ref i "issued")) '(1984 #f #f))
    (check= (csl-name-ref (car (csl-item-ref i "author")) 'family) "Knuth")
    (check= (csl-name-ref (car (csl-item-ref i "author")) 'given)
            "Donald E."))
  (with i (item-of "knuth86b")
    (with n (cadr (csl-item-ref i "author"))
      (check= (csl-name-ref n 'family) "Hoeven")
      (check= (csl-name-ref n 'non-dropping-particle) "van der"))
    ;; a formula stays a tree
    (check-true (if (first-of 'raw (csl-item-ref i "title")) #t #f)))
  (with i (item-of "knuth86")
    (check= (csl-item-type i) "book")
    (check= (csl-item-ref i "publisher-place") "Reading, MA"))
  (with i (car (csl-bib->items
                (bib-parse
                 (string-append
                  "@phdthesis{t, author={Doe, Jane and others},"
                  " title={A Thesis}, school={MIT}, year=2001, month=mar,"
                  " edition={Second}}"))))
    (check= (csl-item-type i) "thesis")
    (check= (csl-item-ref i "publisher") "MIT")
    (check= (csl-item-ref i "genre") "PhD thesis")
    (check= (csl-item-ref i "edition") "2")
    (check= (csl-date-start (csl-item-ref i "issued")) '(2001 3 #f))
    ;; "and others" is not a name
    (check= (length (csl-item-ref i "author")) 1)
    (check-true (csl-item-ref i "author:others")))
  (check-group "items from CSL-JSON")
  (with i (car (csl-json->items
                (string-append
                 "[{\"id\":\"a\",\"type\":\"book\",\"title\":\"On <i>x</i>\","
                 "\"author\":[{\"family\":\"van Gogh\",\"given\":\"Vincent\"}],"
                 "\"issued\":{\"date-parts\":[[2000,5,3],[2001]]}}]")))
    (check= (csl-item-id i) "a")
    (check= (csl-item-ref i "title")
            '(cat "On " (fmt ((font-style . "italic")) "x")))
    (check= (csl-name-ref (car (csl-item-ref i "author")) 'family) "Gogh")
    (check= (csl-name-ref (car (csl-item-ref i "author"))
                          'non-dropping-particle)
            "van")
    (check= (csl-date-start (csl-item-ref i "issued")) '(2000 5 3))
    (check= (csl-date-end (csl-item-ref i "issued")) '(2001 #f #f)))
  (check-group "dates")
  (check= (csl-date-start (csl-parse-date "2020-05-12")) '(2020 5 12))
  (check= (csl-date-end (csl-parse-date "2020-05/2021-01")) '(2021 1 #f))
  (check= (csl-date-literal (csl-parse-date "in press")) "in press"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-rendering)
  (check-group "citations")
  (with p (small-proc)
    (check= (small-html (csl-citation p '(((id . "knuth84")))))
            "(Knuth, 1984)")
    (check= (small-html (csl-citation p '(((id . "knuth86b")))))
            "(Knuth &#38; van der Hoeven, 1986)")
    ;; a locator with its label, in the plural for a range
    (check= (small-html (csl-citation p '(((id . "knuth84")
                                          (locator . "99")))))
            "(Knuth, 1984, p. 99)")
    (check= (small-html (csl-citation p '(((id . "knuth84")
                                          (locator . "99-101")
                                          (label . "page"))
                                         ((id . "knuth86")
                                          (prefix . "see ")))))
            (cork->utf8 (u "(Knuth, 1984, pp. 99–101; see Knuth, 1986)")))
    ;; an unknown reference shows its key
    (check= (small-html (csl-citation p '(((id . "nobody")))))
            "(<b>nobody</b>)"))
  (check-group "bibliography")
  (let* ((p (small-proc))
         (l (csl-bibliography p)))
    ;; sorted by author, then by decreasing date
    (check= (map car l) '("knuth86" "knuth84" "knuth86b"))
    (check= (map cadr l) '("1" "2" "3"))
    ;; the period after the initials is not doubled
    (check= (small-html (cadddr (cadr l)))
            (cork->utf8
             (u (string-append
                 "Knuth, D. E. 1984. <i>Literate Programming</i>. "
                 "“The Computer Journal” 27(2). pp. 97–111."))))
    (check= (small-html (cadddr (caddr l)))
            (cork->utf8
             (u (string-append
                 "Knuth, D. E. and J. van der Hoeven. 1986. <i>On "
                 "L2 Estimates</i>. “J. Math.” 1. pp. 1–2.")))))
  (check-group "TeXmacs trees")
  (with locale (csl-locale #f "en-US")
    (check= (csl->texmacs '(cat "a " (fmt ((font-style . "italic")) "b")
                                " " (raw (math "x")))
                          locale)
            '(concat "a " (with "font-shape" "italic" "b") " " (math "x")))
    ;; italics inside italics are upright
    (check= (csl->texmacs
             '(fmt ((font-style . "italic"))
                   (cat "a " (fmt ((font-style . "italic")) "b")))
             locale)
            '(with "font-shape" "italic"
               (concat "a " (with "font-shape" "right" "b"))))
    ;; American quotes take the period inside
    (check= (csl->texmacs '(cat (fmt ((quotes . "true")) "a") ".") locale)
            (u "“a.”"))
    ;; no double period
    (check= (csl->texmacs '(cat "Inc." ".") locale) "Inc.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disambiguation and collapsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (author-date-style options)
  ;; a style "Doe 2000" with the attributes @options of cs:citation
  (string-append
   "<style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"in-text\""
   " version=\"1.0\"><info><title>T</title><id>t</id></info>"
   "<citation " options ">"
   "<layout prefix=\"(\" suffix=\")\" delimiter=\"; \">"
   "<group delimiter=\" \"><names variable=\"author\">"
   "<name form=\"short\" and=\"symbol\" initialize-with=\". \"/></names>"
   "<date variable=\"issued\"><date-part name=\"year\"/></date>"
   "<text variable=\"locator\" prefix=\"p. \"/></group>"
   "</layout></citation>"
   "<bibliography><sort><key variable=\"title\"/></sort><layout>"
   "<group delimiter=\" \"><names variable=\"author\"/>"
   "<date variable=\"issued\"><date-part name=\"year\"/></date>"
   "<text variable=\"title\"/></group></layout></bibliography></style>"))

(define people-json
  (string-append
   "[{\"id\":\"a\",\"type\":\"book\",\"title\":\"A\",\"author\":"
   "[{\"family\":\"Doe\",\"given\":\"John\"}],"
   "\"issued\":{\"date-parts\":[[2000]]}},"
   "{\"id\":\"b\",\"type\":\"book\",\"title\":\"B\",\"author\":"
   "[{\"family\":\"Doe\",\"given\":\"John\"}],"
   "\"issued\":{\"date-parts\":[[2000]]}},"
   "{\"id\":\"c\",\"type\":\"book\",\"title\":\"C\",\"author\":"
   "[{\"family\":\"Doe\",\"given\":\"John\"}],"
   "\"issued\":{\"date-parts\":[[2001]]}},"
   "{\"id\":\"d\",\"type\":\"book\",\"title\":\"D\",\"author\":"
   "[{\"family\":\"Doe\",\"given\":\"Alice\"}],"
   "\"issued\":{\"date-parts\":[[2002]]}},"
   "{\"id\":\"e\",\"type\":\"book\",\"title\":\"E\",\"author\":"
   "[{\"family\":\"Roe\",\"given\":\"R.\"},"
   "{\"family\":\"Poe\",\"given\":\"P.\"},"
   "{\"family\":\"Low\",\"given\":\"L.\"}],"
   "\"issued\":{\"date-parts\":[[1990]]}},"
   "{\"id\":\"f\",\"type\":\"book\",\"title\":\"F\",\"author\":"
   "[{\"family\":\"Roe\",\"given\":\"R.\"},"
   "{\"family\":\"Moe\",\"given\":\"M.\"},"
   "{\"family\":\"Low\",\"given\":\"L.\"}],"
   "\"issued\":{\"date-parts\":[[1990]]}}]"))

(define (people-cite options . ids)
  ;; the citation of the items @ids in the style with @options
  (let* ((p (csl-make-processor
             (csl-style-from-string "t" (author-date-style options))
             "en-US" (csl-json->items people-json))))
    (csl->html (csl-citation p (map (lambda (id) (list (cons 'id id))) ids))
               (csl-processor-locale p))))

(define (test-disambiguation)
  (check-group "disambiguation")
  ;; nothing asked: the cites stay ambiguous
  (check= (people-cite "" "a" "b") "(Doe 2000; Doe 2000)")
  ;; a suffix of the year, in the order of the bibliography
  (check= (people-cite "disambiguate-add-year-suffix=\"true\"" "b" "a" "c")
          "(Doe 2000b; Doe 2000a; Doe 2001)")
  ;; one more name, only as far as needed
  (check= (people-cite (string-append "et-al-min=\"3\" et-al-use-first=\"1\""
                                      " disambiguate-add-names=\"true\"")
                       "e" "f")
          "(Roe, Poe, et al. 1990; Roe, Moe, et al. 1990)")
  (check= (people-cite "et-al-min=\"3\" et-al-use-first=\"1\"" "e" "f")
          "(Roe et al. 1990; Roe et al. 1990)")
  ;; the initials of the persons with the same family name
  (check= (people-cite (string-append
                        "disambiguate-add-givenname=\"true\" "
                        "givenname-disambiguation-rule=\"all-names\"")
                       "c" "d")
          "(J. Doe 2001; A. Doe 2002)")
  (check-group "collapsing")
  (check= (people-cite (string-append "disambiguate-add-year-suffix=\"true\""
                                      " collapse=\"year\"")
                       "a" "b" "c" "e")
          "(Doe 2000a, 2000b, 2001; Roe, Poe, &#38; Low 1990)")
  (check= (people-cite (string-append "disambiguate-add-year-suffix=\"true\""
                                      " collapse=\"year-suffix\""
                                      " year-suffix-delimiter=\",\"")
                       "a" "b" "c")
          "(Doe 2000a,b, 2001)")
  ;; a cite with a locator is not merged
  (with p (csl-make-processor
           (csl-style-from-string
            "t" (author-date-style "collapse=\"year\""))
           "en-US" (csl-json->items people-json))
    (check= (csl->html (csl-citation p '(((id . "c") (locator . "5"))
                                         ((id . "d"))))
                       (csl-processor-locale p))
            "(Doe 2001 p. 5; 2002)")
    (check-group "forms of a citation")
    (check= (csl->html (csl-citation p '(((id . "c") (suppress-author . #t))))
                       (csl-processor-locale p))
            "(2001)")
    (check= (csl->html (csl-citation p '(((id . "c") (author-only . #t))))
                       (csl-processor-locale p))
            "Doe")
    (check= (csl->html (csl-textual-citation p '(((id . "c")) ((id . "d"))
                                                 ((id . "e"))))
                       (csl-processor-locale p))
            "Doe (2001, 2002), Roe, Poe, &#38; Low (1990)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The styles which come with TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-styles)
  (check-group "installed styles")
  (with l (csl-available-styles)
    (check-true (in? "csl-apa" l))
    (check-true (in? "csl-ieee" l))
    ;; all of them load, with a bibliography
    (check= (list-filter l (lambda (s)
                             (with st (csl-load-style (string-drop s 4))
                               (not (and st
                                         (csl-style-ref st 'bibliography))))))
            '()))
  (check-false (csl-load-style "no-such-style"))
  (check-true (csl-style-name? "csl-apa"))
  (check-false (csl-style-name? "tm-plain"))
  (check-group "locales")
  (check= (csl-term (csl-locale #f "en-US") "and" #f #f) "and")
  (check= (csl-term (csl-locale #f "fr-FR") "and" #f #f) "et")
  (check= (csl-term (csl-locale #f "de-DE") "editor" "short" #t) "Hrsg.")
  (check= (csl-term (csl-locale #f "en-US") "page" "short" #t) "pp.")
  ;; a locale which is not installed falls back to en-US
  (check= (csl-term (csl-locale #f "xx-XX") "and" #f #f) "and")
  (check= (csl-ordinal-suffix (csl-locale #f "en-US") 2 #f) "nd")
  (check= (csl-ordinal-suffix (csl-locale #f "en-US") 12 #f) "th")
  (check= (csl-language->locale "french") "fr-FR")
  (check-group "IEEE")
  (with l (bib-of "csl-ieee")
    ;; in the order of the citations
    (check= (map car l) '("1" "2" "3"))
    (check= (map cadr l) '("knuth86b" "knuth84" "knuth86"))
    (check= (caddr (car l))
            (u (string-append "D. E. Knuth and J. van der Hoeven, “On L^2 "
                              "estimates,” J. Math., vol. 1, pp. 1–2, 1986.")))
    (check= (caddr (cadr l))
            (u (string-append
                "D. E. Knuth, “Literate programming,” The Computer Journal, "
                "vol. 27, no. 2, pp. 97–111, 1984, "
                "doi: 10.1093/comjnl/27.2.97.")))
    (check= (caddr (caddr l))
            "D. E. Knuth, The TeXbook. Reading, MA: Addison-Wesley, 1986."))
  ;; the labels look as the style wants them
  (check= (cadr (first-of 'macro (csl-bib-process "bib" "csl-ieee"
                                                  (bib-parse sample-bib))))
          "body")
  (check= (caddr (first-of 'macro (csl-bib-process "bib" "csl-ieee"
                                                   (bib-parse sample-bib))))
          '(concat "[" (arg "body") "]" " "))
  (check-group "APA")
  (with l (bib-of "csl-apa")
    (check= (map cadr l) '("knuth84" "knuth86" "knuth86b"))
    (check= (map car l) '("Knuth, 1984" "Knuth, 1986"
                          "Knuth & van der Hoeven, 1986"))
    (check= (caddr (car l))
            (u (string-append
                "Knuth, D. E. (1984). Literate programming. The Computer "
                "Journal, 27(2), 97–111. "
                "https://doi.org/10.1093/comjnl/27.2.97")))
    (check= (caddr (cadr l))
            "Knuth, D. E. (1986). The TeXbook. Addison-Wesley."))
  (check-group "Nature")
  (with l (bib-of "csl-nature")
    (check= (caddr (cadr l))
            (u (string-append "Knuth, D. E. Literate programming. The "
                              "Computer Journal 27, 97–111 (1984)."))))
  (check-group "AMS, with labels")
  (with l (bib-of "csl-american-mathematical-society-label")
    (check= (map car l) '("KnHo86" "Knut84" "Knut86"))
    (check= (caddr (caddr l))
            (string-append "Knuth, Donald E., The TeXbook, Addison-Wesley, "
                           "Reading, MA, 1986.")))
  (check-group "errors")
  (check= (csl-bib-process "bib" "csl-no-such-style" (bib-parse sample-bib))
          "Error: CSL style no-such-style not found"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The bibliography of a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define csl-dir
  (string-append (url->system (url-temp-dir)) "/csl-tmp"))

(define (tmp-file name)
  (system->url (string-append csl-dir "/" name)))

(define (remove-tmp-dir)
  (with d (system->url csl-dir)
    (when (url-exists? d)
      (for (f (url-read-directory d "*")) (system-remove f))
      (system-remove d))))

(define (doc-tm body)
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n" body
                 "</body>\n"))

(define (generated-bibliography name text)
  ;; the bibliography after two rounds of Document > Update > All: the
  ;; citations are known to the bibliography from the second round on
  (let* ((f (tmp-file name))
         (old (current-buffer)))
    (string-save (doc-tm text) f)
    (load-buffer f)
    (switch-to-buffer* f)
    (update-forced)
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (with r (first-of 'bibliography (tree->stree (buffer-get-body f)))
      (buffer-pretend-saved f)
      (buffer-close f)
      (when (buffer-exists? old) (switch-to-buffer old))
      r)))

(define (bindings b)
  ;; the citations which a generated bibliography carries: (name . text)
  (let loop ((x b))
    (cond ((and (func? x 'set-binding 2) (func? (caddr x) 'quote 1))
           (list (cons (cadr x) (flat* (cadr (caddr x))))))
          ((func? x 'set-binding 2) (list (cons (cadr x) (flat* (caddr x)))))
          ((pair? x) (append-map loop (cdr x)))
          (else '()))))

(define (flat* x)
  ;; as flat, with the footnotes between braces
  (cond ((or (func? x 'footnote 1) (func? x 'cite-note 1))
         (string-append "{" (flat* (cadr x)) "}"))
        ((string? x) x)
        ((func? x 'with) (flat* (cAr x)))
        ((func? x 'TeX) "TeX")
        ((pair? x) (apply string-append (map flat* (cdr x))))
        (else "")))

(define (test-document-citations)
  (check-group "document citations")
  (string-save sample-bib (tmp-file "refs.bib"))
  (with l (bindings
           (generated-bibliography
            "apa.tm"
            (string-append
             "  See <cite|knuth86b>, <cite|knuth84|knuth86>,"
             " <cite-detail|knuth84|pp. 99-101>, <cite-textual|knuth86b>,"
             " <cite-author|knuth84> in <cite-year|knuth84>"
             " and <cite-detail|knuth86|for example>.\n\n"
             "  <\\bibliography|bib|csl-apa|refs>\n  </bibliography>\n")))
    (check= (assoc-ref l "bib-csl") "true")
    (check= (assoc-ref l "bib-cite-p:knuth86b,")
            "(Knuth & van der Hoeven, 1986)")
    ;; the cites of one author are collapsed
    (check= (assoc-ref l "bib-cite-p:knuth84,knuth86,") "(Knuth, 1984, 1986)")
    (check= (assoc-ref l "bib-cite-p:knuth84@pp. 99-101,")
            (u "(Knuth, 1984, pp. 99–101)"))
    (check= (assoc-ref l "bib-cite-t:knuth86b,")
            "Knuth & van der Hoeven (1986)")
    (check= (assoc-ref l "bib-cite-a:knuth84,") "Knuth")
    (check= (assoc-ref l "bib-cite-y:knuth84,") "1984")
    ;; details which are no locator follow the cite
    (check= (assoc-ref l "bib-cite-p:knuth86@for example,")
            "(Knuth, 1986, for example)"))
  (with l (bindings
           (generated-bibliography
            "ieee2.tm"
            (string-append
             "  See <cite|knuth86b>, <cite|knuth84|knuth86>"
             " and <cite-textual|knuth86b>.\n\n"
             "  <\\bibliography|bib|csl-ieee|refs>\n  </bibliography>\n")))
    (check= (assoc-ref l "bib-cite-p:knuth86b,") "[1]")
    (check= (assoc-ref l "bib-cite-p:knuth84,knuth86,") "[2], [3]")
    ;; a numeric style has no names of its own
    (check= (assoc-ref l "bib-cite-t:knuth86b,")
            "Knuth and van der Hoeven [1]"))
  (with l (bindings
           (generated-bibliography
            "notes.tm"
            (string-append
             "  One<cite|knuth86>, two<cite|knuth84>, three<cite|knuth86>"
             " and <cite-textual|knuth84>.\n\n"
             "  <\\bibliography|bib|csl-chicago-notes-bibliography|refs>\n"
             "  </bibliography>\n")))
    ;; a note style: the macros put the citation in a footnote
    (check= (assoc-ref l "bib-csl") "note")
    (check= (assoc-ref l "bib-cite-p:knuth86,")
            "Donald E. Knuth, The TeXbook (Addison-Wesley, 1986).")
    ;; the third citation is a later one of the same book
    (check= (assoc-ref l "bib-cite-p:knuth86,#3") "Knuth, The TeXbook.")
    (check= (assoc-ref l "bib-cite-p:knuth84,#2") #f)
    ;; in the text, the names stay there and the rest goes to a footnote;
    ;; the fourth citation is a later one of the article
    (check= (assoc-ref l "bib-cite-t:knuth84,") "Donald E. Knuth")
    (check= (assoc-ref l "bib-cite-t:knuth84,#4") "Knuth")
    (check= (assoc-ref l "bib-cite-n:knuth84,#4")
            (u "“Literate Programming.”"))))

(define (test-document)
  (check-group "document bibliography")
  (string-save sample-bib (tmp-file "refs.bib"))
  (with b (generated-bibliography
           "ieee.tm"
           (string-append "  See <cite|knuth86b> and <cite|knuth84>.\n\n"
                          "  <\\bibliography|bib|csl-ieee|refs>\n"
                          "  </bibliography>\n"))
    (check= (list-head b 4) '(bibliography "bib" "csl-ieee" "refs"))
    ;; only the cited entries, in the order of the citations
    (check= (map (lambda (e) (list (cadr (first-of 'bibitem* e))
                                   (cadr (first-of 'label e))))
                 (cdr (caddr (first-of 'bib-list b))))
            '(("1" "bib-knuth86b") ("2" "bib-knuth84"))))
  ;; a style next to the document is found
  (string-save small-style (tmp-file "small.csl"))
  (with b (generated-bibliography
           "small.tm"
           (string-append "  See <cite|knuth86>.\n\n"
                          "  <\\bibliography|bib|csl-small|refs>\n"
                          "  </bibliography>\n"))
    (check= (flat (first-of 'bib-list b))
            (string-append "Knuth, D. E. 1986. The TeXbook. "
                           "Addison-Wesley. Reading, MA."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (csl-test-failures)
  (check-suite "csl")
  (test-utils)
  (test-items)
  (test-rendering)
  (test-disambiguation)
  (test-styles)
  (remove-tmp-dir)
  (system-mkdir (system->url csl-dir))
  (for (test (list test-document test-document-citations))
    (with r (check-run test)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))))
  (remove-tmp-dir)
  (check-end))
