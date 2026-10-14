
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

(define (style-of body)
  ;; the style with the elements @body after cs:info
  (csl-style-from-string
   "t" (string-append
        "<style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"in-text\""
        " version=\"1.0\"><info><title>T</title><id>t</id></info>"
        body "</style>")))

(define (rendered style lang json)
  ;; the citation of all the items of @json and the entries of the
  ;; bibliography in @style, as HTML
  (let* ((items (csl-json->items json))
         (p (csl-make-processor style lang items))
         (locale (csl-processor-locale p)))
    (cons (csl->html (csl-citation p (map (lambda (i)
                                            (list (cons 'id (csl-item-id i))))
                                          items))
                     locale)
          (map (lambda (e) (csl->html (rt-cat (caddr e) (cadddr e)) locale))
               (csl-bibliography p)))))

(define (bib-item s)
  ;; the item for the BibTeX entry in the string @s
  (car (csl-bib->items (bib-parse s))))

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
  ;; a text in capitals is not touched, as the words in capitals
  (check= (rt-text-case "THE ART OF WAR" "title" #t) "THE ART OF WAR")
  (check= (rt-text-case "a history of the UK" "title" #t)
          "A History of the UK")
  (check= (rt-text-case "THE ART" "sentence" #t) "The art")
  (check= (rt-text-case "the iPhone and NASA" "capitalize-first" #t)
          "The iPhone and NASA")
  (check= (rt-text-case "ab cd" "capitalize-all" #t) "Ab Cd")
  (check= (rt-text-case (u "école") "uppercase" #t) (u "ÉCOLE"))
  ;; protected text and formulas keep their case
  (check= (rt-text-case '(cat "on " (nocase "pH") " and " (raw (math "x")))
                        "uppercase" #t)
          '(cat "ON " (nocase "pH") " AND " (raw (math "x"))))
  ;; the text of a tree: the variables of a with are no text
  (check= (rt->string '(cat "a " (raw (with "font-shape" "italic" "b"))))
          "a b")
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
          '((0 . b) (0 . d) (1 . a) (1 . c)))
  ;; a list as long as a large database
  (with l (iota 20000)
    (check= (csl-sort (reverse l) <) l))
  ;; a range as TeXmacs writes it
  (check= (csl-number-tokens "12--15") '("12" "-" "15")))

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
  (with i (bib-item (string-append "@phdthesis{t, author={and others},"
                                   " title={T}, type={Habilitation thesis}}"))
    (check-false (csl-item-ref i "author"))
    ;; the field type replaces the name of the kind of thesis
    (check= (csl-item-ref i "genre") "Habilitation thesis"))
  ;; identifiers and addresses are not LaTeX
  (with i (bib-item (string-append
                     "@misc{s, doi={10.1007/978-3-540-30108-0_4},"
                     " url={https://example.org/~doe/my_paper?a=1&b=2}}"))
    (check= (csl-item-ref i "DOI") "10.1007/978-3-540-30108-0_4")
    (check= (csl-item-ref i "URL") "https://example.org/~doe/my_paper?a=1&b=2"))
  ;; an empty field gives no page, other pages are kept as they are written
  (check-false (csl-item-ref (bib-item "@misc{s, title={T}, pages={}}")
                             "page"))
  (check= (csl-item-ref (bib-item "@misc{s, title={T}, pages={e1234}}") "page")
          "e1234")
  (check= (csl-item-ref (bib-item "@misc{s, title={T}, pages={12, 15--17}}")
                        "page")
          "12, 15-17")
  (check-group "sentence case")
  (let* ((title (lambda (fields)
                  (csl-item-ref (bib-item (string-append "@book{b, " fields
                                                         "}"))
                                "title"))))
    ;; single letters are kept
    (check= (title "title={What I Learned About Vitamin A}")
            "What I learned about vitamin A")
    ;; protected text is a word like the other ones
    (check= (title "title={{NASA} Reports On Mars}")
            '(cat (nocase "NASA") " reports on mars"))
    (check= (title "title={A Tale: The Two Cities}") "A tale: The two cities")
    ;; the titles in another language are not changed
    (check= (title "title={Die Kunst Der Fuge}, language={german}")
            "Die Kunst Der Fuge")
    (check= (title "title={The Art Of War}, langid={american}")
            "The art of war"))
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
  ;; a tag which is not known has no effect, a stray end tag neither
  (check= (rt->string (csl-parse-markup "A <span class=\"x\">B</span> C"))
          "A B C")
  (check= (rt->string (csl-parse-markup "A</span> <i>B</i> C")) "A B C")
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
;; Particular cases of the rendering elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (book id fields)
  ;; the CSL-JSON of a book with the other @fields
  (string-append "{\"id\":\"" id "\",\"type\":\"book\","  fields "}"))

(define (books . l)
  (string-append "[" (string-recompose l ",") "]"))

(define doe "\"author\":[{\"family\":\"Doe\",\"given\":\"J.\"}]")

(define two-knuth
  (books (book "a" (string-append "\"title\":\"T1\",\"author\":[{\"family\":"
                                  "\"Knuth\",\"given\":\"D.\"}],"
                                  "\"issued\":{\"date-parts\":[[1984]]}"))
         (book "b" (string-append "\"title\":\"T2\",\"author\":[{\"family\":"
                                  "\"Knuth\",\"given\":\"D.\"}],"
                                  "\"issued\":{\"date-parts\":[[1984]]}"))))

(define twelve
  ;; the books k1 to k12, of the volumes 12 to 1
  (apply books
         (map (lambda (i)
                (book (string-append "k" (number->string (+ i 1)))
                      (string-append "\"title\":\"T" (number->string (+ i 1))
                                     "\",\"volume\":\""
                                     (number->string (- 12 i)) "\"")))
              (iota 12))))

(define (many-authors id given n)
  ;; a book of @n authors; the given name of the fifth one is @given
  (book id (string-append
            "\"title\":\"T\",\"issued\":{\"date-parts\":[[2012]]},"
            "\"author\":["
            (string-recompose
             (map (lambda (i)
                    (string-append "{\"family\":\"F" (number->string i)
                                   "\",\"given\":\"" (if (== i 4) given "G")
                                   "\"}"))
                  (iota n))
             ",")
            "]")))

(define (test-elements)
  (check-group "substitution")
  ;; names inside cs:choose inherit cs:name and the label, and the editors
  ;; which stand for the authors are not printed again
  (check= (rendered (csl-load-style "chicago-author-date") "en-US"
                    (books (book "o" (string-append
                                      "\"title\":\"T\",\"editor\":[{\"family\":"
                                      "\"Doe\",\"given\":\"J.\"},{\"family\":"
                                      "\"Roe\",\"given\":\"K.\"}],"
                                      "\"issued\":{\"date-parts\":[[1999]]}"))))
          '("(Doe and Roe 1999)" "Doe, J., and K. Roe, eds. 1999. <i>T</i>."))
  ;; a title which a macro substitutes is not printed again
  (check= (rendered (csl-load-style "apa") "en-US"
                    (books (book "t" "\"title\":\"Y\"")))
          '("(<i>Y</i>, n.d.)" "<i>Y</i>. (n.d.)."))
  ;; the affixes and the formatting of cs:names apply once to the names
  ;; which are substituted
  (check= (cdr (rendered
                (style-of
                 (string-append
                  "<citation><layout><text variable=\"title\"/></layout>"
                  "</citation><bibliography><layout>"
                  "<names variable=\"author\" prefix=\"[\" suffix=\"] \""
                  " font-style=\"italic\"><name/><substitute>"
                  "<names variable=\"editor\"/></substitute></names>"
                  "<text variable=\"title\"/></layout></bibliography>"))
                "en-US"
                (books (book "a" (string-append
                                  "\"title\":\"T1\",\"editor\":[{\"family\":"
                                  "\"Ed\",\"given\":\"E.\"}]")))))
          '("[<i>E. Ed</i>] T1"))
  (check-group "groups, dates and macros")
  ;; no date: the group with the suffix of the year is not left out
  (check= (rendered (csl-load-style "apa") "en-US"
                    (books (book "e" (string-append "\"title\":\"Y\"," doe))))
          '("(Doe, n.d.)" "Doe, J. (n.d.). <i>Y</i>."))
  ;; a range of years: the months and the days have no range
  (check= (cdr (rendered
                (csl-load-style "apa") "en-US"
                (books (book "r" (string-append
                                  "\"title\":\"T2\"," doe ",\"issued\":"
                                  "{\"date-parts\":[[1999],[2001]]}")))))
          (list (cork->utf8 (u "Doe, J. (1999–2001). <i>T2</i>."))))
  ;; a macro which calls itself ends
  (check= (rendered
           (style-of
            (string-append
             "<macro name=\"m\"><text variable=\"title\"/><text macro=\"m\"/>"
             "</macro><citation><layout><text macro=\"m\"/></layout>"
             "</citation>"))
           "en-US" (books (book "t" "\"title\":\"Y\"")))
          '("Y"))
  ;; a style needs the layout of the citations
  (check-false (style-of "<citation/>"))
  (check-false (csl-style-ref
                (style-of "<citation><layout/></citation><bibliography/>")
                'bibliography))
  (check-group "labels and numbers")
  (let* ((label (lambda (attrs)
                  (string-append
                   "<macro name=\"l\"><text variable=\"citation-label\"/>"
                   "</macro><citation><layout prefix=\"[\" suffix=\"]\""
                   " delimiter=\", \"><text macro=\"l\"/></layout></citation>"
                   "<bibliography" attrs "><layout>"
                   "<text variable=\"citation-label\" prefix=\"[\""
                   " suffix=\"] \"/><names variable=\"author\"><name/></names>"
                   "<date variable=\"issued\" prefix=\" (\" suffix=\")\">"
                   "<date-part name=\"year\"/></date></layout>"
                   "</bibliography>"))))
    ;; equal labels get a suffix, also when a macro prints them; the year
    ;; does not repeat it after an aligned label
    (check= (rendered (style-of (label " second-field-align=\"flush\""))
                      "en-US" two-knuth)
            '("[Knut84a, Knut84b]" "[Knut84a] D. Knuth (1984)"
              "[Knut84b] D. Knuth (1984)")))
  (let* ((style (style-of
                 (string-append
                  "<macro name=\"n\"><text variable=\"citation-number\"/>"
                  "</macro><macro name=\"v\"><number variable=\"volume\"/>"
                  "</macro><citation collapse=\"citation-number\"><sort>"
                  "<key macro=\"n\"/></sort><layout prefix=\"[\" suffix=\"]\""
                  " delimiter=\",\"><text macro=\"n\"/></layout></citation>"
                  "<bibliography><sort><key macro=\"v\"/></sort><layout>"
                  "<text variable=\"volume\"/></layout></bibliography>")))
         (p (csl-make-processor style "en-US" (csl-json->items twelve))))
    ;; the numbers of the citations are seen through a macro
    (check-true (csl-processor-ref p 'numeric?))
    ;; the keys of a macro sort numbers as numbers: the volume 9 comes
    ;; before the volume 10
    (check= (map (lambda (e) (rt->string (cadddr e))) (csl-bibliography p))
            (map number->string (map (lambda (i) (+ i 1)) (iota 12))))
    (check= (csl->html (csl-citation p (map (lambda (id) (list (cons 'id id)))
                                            '("k12" "k2" "k10" "k4" "k11")))
                       (csl-processor-locale p))
            (cork->utf8 (u "[1–3,9,11]"))))
  ;; of two items with the same identifier, the first one is kept
  (check= (cdr (rendered (csl-load-style "ieee") "en-US"
                         (books (book "a" "\"title\":\"First\"")
                                (book "a" "\"title\":\"Second\"")
                                (book "b" "\"title\":\"Third\""))))
          '("[1]<i>First</i>. " "[2]<i>Third</i>. "))
  (check-group "names alone")
  (let* ((p (csl-make-processor (csl-load-style "ieee") "en-US"
                                (csl-json->items
                                 (books (book "t" "\"title\":\"Y\"")))))
         (author (lambda (id)
                   (csl->html (csl-citation p `(((id . ,id)
                                                 (author-only . #t))))
                              (csl-processor-locale p)))))
    ;; a reference which is not known shows its key, a work without names
    ;; its title
    (check= (author "nobody") "<b>nobody</b>")
    (check= (author "t") "Y"))
  (check-group "languages")
  (let* ((rose (lambda (more)
                 (books (book "a" (string-append
                                   "\"title\":\"la vie en rose\"," more doe
                                   ",\"issued\":{\"date-parts\":[[2020]]}")))))
         (bib (lambda (lang more)
                (cadr (rendered (csl-load-style "chicago-author-date") lang
                                (rose more))))))
    ;; title case is for English: the locale in use, or else the language
    ;; of the item
    (check= (bib "en-US" "") "Doe, J. 2020. <i>La Vie En Rose</i>.")
    (check= (bib "fr-FR" "") "Doe, J. 2020. <i>la vie en rose</i>.")
    (check= (bib "fr-FR" "\"language\":\"en\",")
            "Doe, J. 2020. <i>La Vie En Rose</i>.")
    (check= (bib "en-US" "\"language\":\"fr\",")
            "Doe, J. 2020. <i>la vie en rose</i>."))
  ;; two styles of the same name keep their own terms
  (check= (map (lambda (word)
                 (car (rendered
                       (style-of
                        (string-append
                         "<locale><terms><term name=\"and\">" word "</term>"
                         "</terms></locale><citation><layout>"
                         "<names variable=\"author\"><name form=\"short\""
                         " and=\"text\"/></names></layout></citation>"))
                       "en-US"
                       (books (book "x" (string-append
                                         "\"author\":[{\"family\":\"A\"},"
                                         "{\"family\":\"B\"}]"))))))
               '("UND" "ET"))
          '("A UND B" "A ET B"))
  ;; the long ordinals follow the gender of the variable
  (check= (cdr (rendered
                (style-of
                 (string-append
                  "<citation><layout><text variable=\"title\"/></layout>"
                  "</citation><bibliography><layout>"
                  "<number variable=\"edition\" form=\"long-ordinal\"/>"
                  "<text value=\" \"/>"
                  "<number variable=\"volume\" form=\"long-ordinal\"/>"
                  "</layout></bibliography>"))
                "it-IT" (books (book "a" "\"edition\":\"1\",\"volume\":\"1\""))))
          '("prima primo"))
  (check-group "long lists of names")
  (let* ((style (style-of
                 (string-append
                  "<citation et-al-min=\"3\" et-al-use-first=\"1\""
                  " disambiguate-add-names=\"true\""
                  " disambiguate-add-givenname=\"true\""
                  " disambiguate-add-year-suffix=\"true\">"
                  "<layout prefix=\"(\" suffix=\")\" delimiter=\"; \">"
                  "<names variable=\"author\"><name form=\"short\""
                  " initialize-with=\". \"/></names>"
                  "<date variable=\"issued\" prefix=\" \">"
                  "<date-part name=\"year\"/></date></layout></citation>"))))
    ;; names are added up to the first one which differs, which gets its
    ;; initial
    (check= (rendered style "en-US" (books (many-authors "x" "Xavier" 6)
                                           (many-authors "y" "Yves" 6)))
            (list (string-append "(F0, F1, F2, F3, X. F4, et al. 2012; "
                                 "F0, F1, F2, F3, Y. F4, et al. 2012)")))
    ;; the same sixty authors: only the year can tell the works apart
    (check= (rendered style "en-US" (books (many-authors "x" "G" 60)
                                           (many-authors "y" "G" 60)))
            '("(F0 et al. 2012a; F0 et al. 2012b)")))
  (check-group "shapes and spaces")
  (with locale (csl-locale #f "fr-FR")
    ;; small capitals in italics are one shape
    (check= (csl->texmacs
             '(fmt ((font-style . "italic"))
                   (cat "a " (fmt ((font-variant . "small-caps")) "b")))
             locale)
            '(with "font-shape" "italic"
               (concat "a " (with "font-shape" "italic-small-caps" "b"))))
    ;; the no-break spaces of the French quotes
    (check= (csl->texmacs '(fmt ((quotes . "true")) "a") locale)
            `(concat ,(u "«") (nbsp) "a" (nbsp) ,(u "»")))))

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
  (set! csl-extra-path '())
  (for (d (list (system->url (string-append csl-dir "/styles"))
                (system->url csl-dir)))
    (when (url-exists? d)
      (for (f (url-read-directory d "*"))
        (when (not (url-directory? f)) (system-remove f)))
      (system-remove d))))

(define (doc-tm body . style)
  (string-append "<TeXmacs|2.1>\n\n<style|"
                 (if (null? style) "generic" (car style))
                 ">\n\n<\\body>\n" body "</body>\n"))

(define (generated-with name text style thunk)
  ;; the body after two rounds of Document > Update > All: the citations
  ;; are known to the bibliography from the second round on; @thunk is
  ;; called in the buffer, at its end, and the result is (body . value)
  (let* ((f (tmp-file name))
         (old (current-buffer)))
    (string-save (doc-tm text style) f)
    (load-buffer f)
    (switch-to-buffer* f)
    (update-forced)
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (go-end)
    (let* ((v (thunk))
           (r (tree->stree (buffer-get-body f))))
      (buffer-pretend-saved f)
      (buffer-close f)
      (when (buffer-exists? old) (switch-to-buffer old))
      (cons r v))))

(define (generated-body name text . opt)
  ;; the body of generated-with; with the optional arguments style and
  ;; markup, the markup as it is evaluated in the document instead
  (if (null? opt) (car (generated-with name text "generic" (lambda () #f)))
      (cdr (generated-with name text (car opt)
                           (lambda ()
                             (tree->stree (texmacs-exec (cadr opt))))))))

(define (expanded x)
  ;; the stree @x as the macros expand it at the cursor
  (tree->stree (texmacs-exec* x)))

(define (generated-bibliography name text)
  (first-of 'bibliography (generated-body name text)))

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
        ((or (func? x 'assign) (func? x 'write)) "")
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
             " <cite-author|knuth84> in <cite-year|knuth84>,"
             " <cite-detail|knuth84|pp. 99--101>, <cite-detail|knuth86|12--15>"
             " and <cite-detail|knuth86|for example>.\n\n"
             "  <\\bibliography|bib|csl-apa|refs>\n  </bibliography>\n")))
    (check= (assoc-ref l "bib-csl") "true")
    (check= (assoc-ref l "bib-cite-p:knuth86b,")
            "(Knuth & van der Hoeven, 1986)")
    ;; the cites of one author are collapsed
    (check= (assoc-ref l "bib-cite-p:knuth84,knuth86,") "(Knuth, 1984, 1986)")
    (check= (assoc-ref l "bib-cite-p:knuth84{pp. 99-101,")
            (u "(Knuth, 1984, pp. 99–101)"))
    (check= (assoc-ref l "bib-cite-t:knuth86b,")
            "Knuth & van der Hoeven (1986)")
    (check= (assoc-ref l "bib-cite-a:knuth84,") "Knuth")
    (check= (assoc-ref l "bib-cite-y:knuth84,") "1984")
    ;; the names of the bindings keep the "--" of the details
    (check= (assoc-ref l "bib-cite-p:knuth84{pp. 99--101,")
            (u "(Knuth, 1984, pp. 99–101)"))
    (check= (assoc-ref l "bib-cite-p:knuth86{12--15,")
            (u "(Knuth, 1986, pp. 12–15)"))
    ;; details which are no locator follow the cite
    (check= (assoc-ref l "bib-cite-p:knuth86{for example,")
            "(Knuth, 1986, for example)"))
  (with l (bindings
           (generated-bibliography
            "details.tm"
            (string-append
             "  See <cite-detail|knuth84|<em|passim>>, <cite|knuth84@x>,"
             " <cite-detail|knuth84|x>, <cite-detail|knuth84|Lemma2>,"
             " <cite-detail|knuth84|12a> and <cite-author|nobody>.\n\n"
             "  <\\bibliography|bib|csl-apa|refs>\n  </bibliography>\n")))
    ;; details which are not a plain text: the citation has a binding of
    ;; its own, with the number of the citation
    (check= (assoc-ref l "bib-cite-p:knuth84{*,#1") "(Knuth, 1984, passim)")
    ;; a key with @ is not the key of a citation with details
    (check= (assoc-ref l "bib-cite-p:knuth84@x,") "(knuth84@x)")
    (check= (assoc-ref l "bib-cite-p:knuth84{x,") "(Knuth, 1984, x)")
    ;; a word with a digit is no page
    (check= (assoc-ref l "bib-cite-p:knuth84{Lemma2,") "(Knuth, 1984, Lemma2)")
    (check= (assoc-ref l "bib-cite-p:knuth84{12a,") "(Knuth, 1984, p. 12a)")
    ;; the names of a reference which is not known
    (check= (assoc-ref l "bib-cite-a:nobody,") "nobody"))
  ;; a bibliography without any entry keeps the citations
  (with l (bindings
           (generated-bibliography
            "none.tm"
            (string-append
             "  See <cite|nobody>.\n\n"
             "  <\\bibliography|bib|csl-apa|refs>\n  </bibliography>\n")))
    (check= (assoc-ref l "bib-csl") "true")
    (check= (assoc-ref l "bib-cite-p:nobody,") "(nobody)"))
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
            (u "“Literate Programming.”")))
  ;; two bibliographies: the citations are numbered in the document, and
  ;; each bibliography knows the numbers of its own
  (with l (bindings
           (generated-body
            "two.tm"
            (string-append
             "  One<cite|knuth84>, two<with|bib-prefix|b2|<cite|knuth86>>,"
             " three<with|bib-prefix|b2|<cite|knuth86>>"
             " and four<cite|knuth84>.\n\n"
             "  <\\bibliography|bib|csl-chicago-notes-bibliography|refs>\n"
             "  </bibliography>\n\n"
             "  <\\with|bib-prefix|b2>\n"
             "    <\\bibliography|b2|csl-chicago-notes-bibliography|refs>\n"
             "    </bibliography>\n"
             "  </with>\n")))
    (check= (assoc-ref l "b2-cite-p:knuth86,#3") "Knuth, The TeXbook.")
    (check= (assoc-ref l "bib-cite-p:knuth84,#4")
            (u "Knuth, “Literate Programming.”"))
    (check= (assoc-ref l "bib-cite-p:knuth84,#3") #f))
  ;; the package cite-author-year finds the citations with details
  (with x (generated-body
           "author-year.tm"
           (string-append
            "  See <cite-detail|knuth84|pp. 99-101>.\n\n"
            "  <\\bibliography|bib|csl-apa|refs>\n  </bibliography>\n")
           "<tuple|generic|cite-author-year>"
           '(cite-detail "knuth84" "pp. 99-101"))
    (check= (flat* (if (func? x 'concat) (cAr x) x))
            (u "(Knuth, 1984, pp. 99–101)"))))

(define (save-style name text)
  (string-save text (system->url (string-append csl-dir "/styles/" name
                                                ".csl"))))

(define (dependent-style parent)
  (string-append
   "<style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"in-text\""
   " version=\"1.0\"><info><title>D</title><id>d</id>"
   "<link href=\"http://www.zotero.org/styles/" parent "\""
   " rel=\"independent-parent\"/></info></style>"))

(define (test-style-errors)
  (check-group "errors of styles")
  (system-mkdir (system->url (string-append csl-dir "/styles")))
  (set! csl-extra-path (list (system->url csl-dir)))
  (let* ((bib (bib-parse sample-bib))
         (processed (lambda (style t)
                      ;; the result, or raised when an error comes through
                      (catch #t (lambda () (csl-bib-process "bib" style t))
                        (lambda args 'raised))))
         (error? (lambda (name)
                   (with r (processed (string-append "csl-" name) bib)
                     (and (string? r)
                          (string-starts? r (string-append "Error: CSL style "
                                                           name)))))))
    ;; an error while the style is processed gives a message
    (with r (processed "csl-apa" 42)
      (check-true (and (string? r)
                       (string-starts? r "Error: CSL style apa: "))))
    ;; a count of names which is no count is ignored
    (save-style "negative"
                (string-replace (author-date-style
                                 "et-al-min=\"1\" et-al-use-first=\"-1\"")
                                "<names variable=\"author\"/>"
                                (string-append "<names variable=\"author\">"
                                               "<name et-al-min=\"1\""
                                               " et-al-use-first=\"-1\"/>"
                                               "</names>")))
    (with b (first-of 'bib-list (processed "csl-negative" bib))
      (check-true (and b (string-starts? (flat b) "Donald E. Knuth 1984"))))
    ;; a dependent style needs an independent parent
    (save-style "self" (dependent-style "self"))
    (save-style "one" (dependent-style "two"))
    (save-style "two" (dependent-style "one"))
    (save-style "child" (dependent-style "apa"))
    (check-true (error? "self"))
    (check-true (error? "one"))
    (check-false (error? "child"))
    (check= (csl-style-ref (csl-load-style "child") 'title)
            (csl-style-ref (csl-load-style "apa") 'title))
    ;; a style is read again when its file changes
    (save-style "edited" small-style)
    (with old (csl-style-ref (csl-load-style "edited") 'title)
      (let* ((u (system->url (string-append csl-dir "/styles/edited.csl")))
             (date (url-last-modified u))
             (start (texmacs-time)))
        (while (and (== (url-last-modified u) date)
                    (< (- (texmacs-time) start) 5000))
          (save-style "edited" (string-replace small-style "<title>Small"
                                               "<title>Edited")))
        (check= old "Small")
        (check= (csl-style-ref (csl-load-style "edited") 'title) "Edited"))))
  (set! csl-extra-path '()))

(define (test-scripts)
  (check-group "scripts without case")
  ;; the names in CJK sort after the Latin ones, by their code points
  (let* ((json (string-append
                "[{\"id\":\"w\",\"type\":\"book\",\"title\":\"A\","
                "\"author\":[{\"literal\":\"\\u738B\"}]},"
                "{\"id\":\"l\",\"type\":\"book\",\"title\":\"B\","
                "\"author\":[{\"literal\":\"\\u674E\"}]},"
                "{\"id\":\"a\",\"type\":\"book\",\"title\":\"C\","
                "\"author\":[{\"literal\":\"\\u5B89\"}]},"
                "{\"id\":\"z\",\"type\":\"book\",\"title\":\"D\","
                "\"author\":[{\"family\":\"Zed\",\"given\":\"A.\"}],"
                "\"issued\":{\"date-parts\":[[2000]]}}]"))
         (items (csl-json->items json))
         (p (csl-make-processor (csl-style-from-string "small" small-style)
                                "en-US" items)))
    (check= (map car (csl-bibliography p)) '("z" "a" "l" "w"))
    ;; and they make labels
    (check= (csl-citation-label (car items)) "<#738B>")))

(define three-bib
  (string-append
   sample-bib
   "@article{three, author={Alpha, Ann and Beta, Bob and Gamma, Carl},"
   " title={Three}, journal={J. Math.}, year=2000, volume=2,"
   " pages={3--4}}\n"))

(define (test-document-macros)
  (check-group "citation macros")
  (string-save three-bib (tmp-file "refs3.bib"))
  ;; cite-detail of the package cite-author-year has the signature of the
  ;; standard one; cite* and cite-textual* show all the names. The third
  ;; citation is the second cite-detail: its cite-add is one too
  (with r (generated-with
           "natbib.tm"
           (string-append
            "  See <cite-detail|knuth84|pp. 99-101>,"
            " <cite-detail|knuth84|<em|passim>>, <cite|three>,"
            " <cite*|three>, <cite-textual|three> and"
            " <cite-textual*|three>.\n\n"
            "  <\\bibliography|bib|csl-apa|refs3>\n  </bibliography>\n")
           "<tuple|generic|cite-author-year>"
           (lambda ()
             (list (flat* (expanded '(cite-detail "knuth84" "pp. 99-101")))
                   (flat* (expanded '(with "cite-nr" "2"
                                       (cite-detail "knuth84"
                                                    (em "passim")))))
                   (flat* (expanded '(cite "three")))
                   (flat* (expanded '(cite* "three")))
                   (flat* (expanded '(cite-textual* "three"))))))
    (with l (bindings (first-of 'bibliography (car r)))
      (check= (assoc-ref l "bib-cite-p:three,") "(Alpha et al., 2000)")
      (check= (assoc-ref l "bib-cite-p*:three,")
              "(Alpha, Beta, & Gamma, 2000)")
      (check= (assoc-ref l "bib-cite-t:three,") "Alpha et al. (2000)")
      (check= (assoc-ref l "bib-cite-t*:three,")
              "Alpha, Beta, & Gamma (2000)"))
    (check= (cdr r)
            (list (u "(Knuth, 1984, pp. 99–101)")
                  "(Knuth, 1984, passim)"
                  "(Alpha et al., 2000)"
                  "(Alpha, Beta, & Gamma, 2000)"
                  "Alpha, Beta, & Gamma (2000)")))
  ;; in a note style, a textual citation inside a footnote is the names,
  ;; a comma and the rest
  (string-save sample-bib (tmp-file "refs.bib"))
  (with r (generated-with
           "inline.tm"
           (string-append
            "  One<cite|knuth86>, two <cite-textual|knuth86>.\n\n"
            "  <\\bibliography|bib|csl-chicago-notes-bibliography|refs>\n"
            "  </bibliography>\n")
           "generic"
           (lambda ()
             (flat* (expanded '(with "cite-note" (value "cite-inline")
                                     "cite-note-nr" "2"
                                     (cite-textual "knuth86"))))))
    (check= (cdr r)
            "Donald E. Knuth, The TeXbook (Addison-Wesley, 1986)."))
  ;; the preview of the bibliography dialog: the entries without the
  ;; citations of the document
  (with r (generated-with
           "preview.tm"
           (string-append
            "  See <cite|knuth86b> and <cite|knuth84>.\n\n"
            "  <\\bibliography|bib|csl-apa|refs>\n  </bibliography>\n")
           "generic"
           (lambda ()
             (csl-bib-process "bib" "csl-apa" (bib-parse sample-bib)
                              '() #f #f)))
    (check= (length (bindings (first-of 'bibliography (car r)))) 3)
    (check= (bindings (cdr r)) '())
    (check= (length (cdr (caddr (first-of 'bib-list (cdr r))))) 3)))

(define note-style
  ;; a note style which shows the positions of its cites
  (string-append
   "<style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"note\""
   " version=\"1.0\"><info><title>Notes</title><id>notes</id></info>"
   "<citation near-note-distance=\"1\"><layout delimiter=\"; \">"
   "<group delimiter=\" \"><text variable=\"title\"/>"
   "<choose><if position=\"near-note\"><text value=\"near\"/></if>"
   "<else><text value=\"far\"/></else></choose>"
   "<text variable=\"first-reference-note-number\" prefix=\"n\"/>"
   "</group></layout></citation>"
   "<bibliography><layout><text variable=\"title\"/></layout>"
   "</bibliography></style>"))

(define (test-document-notes)
  (check-group "note numbers")
  (string-save sample-bib (tmp-file "refs.bib"))
  (string-save note-style (tmp-file "notes.csl"))
  ;; the positions count the footnotes, not the citations: the first two
  ;; citations are in the first footnote, the third one makes the second
  (with l (bindings
           (generated-bibliography
            "positions.tm"
            (string-append
             "  A<footnote|x <cite|knuth86> y <cite|knuth84>.>"
             " B<cite|knuth86> C<cite|knuth84>.\n\n"
             "  <\\bibliography|bib|csl-notes|refs>\n"
             "  </bibliography>\n")))
    (check= (assoc-ref l "bib-cite-p:knuth86,") "The TeXbook far")
    (check= (assoc-ref l "bib-cite-p:knuth86,#3") "The TeXbook near n1")
    (check= (assoc-ref l "bib-cite-p:knuth84,#4")
            "Literate programming far n1")))

(define (test-style-files)
  (check-group "style files")
  (when (not (url-exists? (system->url (string-append csl-dir "/styles"))))
    (system-mkdir (system->url (string-append csl-dir "/styles"))))
  (set! csl-extra-path (list (system->url csl-dir)))
  ;; a style which is changed within the same second is read again too
  (let* ((style (lambda (suffix)
                  (string-replace small-style "<layout suffix=\".\">"
                                  (string-append "<layout suffix=\"" suffix
                                                 "\">"))))
         (shown (lambda ()
                  (with r (bib-of "csl-changed")
                    (and (pair? r) (caddr (car r)))))))
    (save-style "changed" (style "."))
    (with s (shown)
      (check-true (and s (string-ends? s "Reading, MA."))))
    (save-style "changed" (style " (end)."))
    (with s (shown)
      (check-true (and s (string-ends? s "Reading, MA (end)."))))
    ;; the styles are listed without the processor
    (check-true (in? "csl-changed" (csl-available-styles))))
  (set! csl-extra-path '()))

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
  (test-elements)
  (test-styles)
  (remove-tmp-dir)
  (system-mkdir (system->url csl-dir))
  (for (test (list test-document test-document-citations test-style-errors
                   test-scripts test-document-macros test-document-notes
                   test-style-files))
    (with r (check-run test)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))))
  (remove-tmp-dir)
  (check-end))
