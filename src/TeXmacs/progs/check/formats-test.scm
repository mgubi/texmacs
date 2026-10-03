;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : formats-test.scm
;; DESCRIPTION : tests of the conversion to and from the document formats
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The converters of convert/ other than LaTeX, used as the user does:
;; through convert, texmacs->generic and generic->texmacs with the format
;; names declared by define-format and converter (kernel/texmacs/tm-convert),
;; and not through the internal functions of each converter (htmltm-test,
;; tmhtml-test, tmmltm-test and xmltm-test test those).
;;
;;   - the TeXmacs formats: .tm (texmacs-document, texmacs-snippet), the
;;     Scheme format (stm-document, stm-snippet) and the XML format
;;     (tmml-document, tmml-snippet), which must give back the same tree;
;;   - HTML: export and import, and the round trip TeXmacs -> HTML ->
;;     TeXmacs, with what does not come back;
;;   - verbatim (plain text) and the source code formats;
;;   - Markdown: TeXmacs has no Markdown converter, which is checked;
;;   - the registry of formats and converters.
;;
;; Strings of TeXmacs trees are Cork-encoded: "\xe9" is e acute. The
;; converter options (texmacs->html:css, texmacs->verbatim:wrap, ...) are
;; preferences: they are read, never set, and a check which needs another
;; value passes it to convert as an option, which takes precedence over the
;; preference for this call only.
;;
;; The HTML export of TeXmacs works on a document whose style macros have
;; been expanded by the editor (exec_html in edit_typeset.cpp, which needs a
;; buffer): convert alone only knows the primitives and the tags of the
;; tmhtml tables, so that the checks give it expanded markup (section-title,
;; (with "mode" "math" ...), tformat) and check that the macros which need
;; the editor (section, math, tabular) give nothing.

(texmacs-module (check formats-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (st x) (if (tree? x) (tree->stree x) x))

(define (export s fm . opts)
  ;; the TeXmacs stree @s converted to the format @fm (a string)
  (apply convert (cons* (stree->tree s) "texmacs-tree" fm opts)))

(define (import s fm . opts)
  ;; the string @s of the format @fm converted to a TeXmacs stree
  (st (apply convert (cons* s fm "texmacs-tree" opts))))

(define (round-trip s fm . opts)
  (st (convert (apply export (cons* s fm opts)) fm "texmacs-tree")))

(define (tmfile body)
  `(document (TeXmacs ,(texmacs-version)) (style "generic") (body ,body)))

(define (contains? s what)
  (and (string? s) (string-occurs? what s)))

(define (squash s)
  ;; The HTML serializer indents and breaks lines: whitespace runs become
  ;; one space and the whitespace next to a tag is removed, so that
  ;; "<p>\n  a\n</p>" gives "<p>a</p>" (and "</var> + <var>" "</var>+<var>").
  (let loop ((l (string->list s)) (acc '()) (space? #f))
    (cond ((null? l)
           (list->string (reverse acc)))
          ((char-whitespace? (car l))
           (loop (cdr l) acc #t))
          (else
           (let* ((c (car l))
                  (sep? (and space? (nnull? acc)
                             (not (== (car acc) #\>)) (not (== c #\<)))))
             (loop (cdr l) (cons c (if sep? (cons #\space acc) acc)) #f))))))

(define (html s . opts)
  (squash (apply export (cons* s "html-snippet" opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The table of documents of the TeXmacs formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inline content (a string or an inline tag) and block content (a
;; document), in the normal form of TeXmacs trees: no concat of two
;; strings, "<" written "<less>" in a string.
(define inline-samples
  (list "plain"
        "Caf\xe9 \xe0 la cr\xe8me, na\xefve"
        "a <less> b <gtr> c & d \"q\" 'x' \\backslash"
        "two \\\\ backslashes and a quote \\\""
        "<alpha> literal and <#3B1>"
        "tab|bar"
        "\x01\x1f\x7f\x80\xff"
        "  leading and trailing  "
        '(concat "x" (em "y") "z")
        '(with "font-series" "bold" "color" "red" "b")
        '(math (concat "x" (rsup "2") "+" (frac "a" (sqrt "b")) "<leq><alpha>"))
        '(concat (label "sec:a") (reference "sec:a")
                 (hlink "t" "http://x.org/?a=1&b=2"))
        '(verbatim "x  y")
        '(concat "line1" (next-line) "line2")
        '(concat "a" (space "1em") "b")
        '(image "f.png" "1cm" "" "" "")
        '(foo "a" "b")
        '(tabular (tformat (cwith "1" "1" "1" "1" "cell-halign" "c")
                           (table (row (cell "a") (cell "b"))
                                  (row (cell "c") (cell "d")))))))

(define block-samples
  (list '(document "p1" "p2")
        '(document "a" "" "b")
        '(document "")
        '(document "p1" (itemize (document (concat (item) "a")
                                           (concat (item) "b"
                                             (enumerate
                                               (document (concat (item) "c")))))))
        '(document (section "S<less>") "x")
        '(document (equation* (document (concat "a=b" (label "eq1")))))
        '(document (theorem (document "Statement" "Second")))
        '(document (concat "a" (strong (document "x" "y"))))
        '(document (with "color" "red" (document "b" "c")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The .tm format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The serialization escapes <, >, | and \ with a backslash, writes a
;; space which would be lost as "\ " and an empty paragraph as "\;"; the
;; parser undoes it.
(define (test-tm-serialize)
  (check-group "tm snippet serialization")
  (check= (export "plain" "texmacs-snippet") "plain")
  (check= (export "a<less>b" "texmacs-snippet") "a\\<less\\>b")
  (check= (export "a|b\\c" "texmacs-snippet") "a\\|b\\\\c")
  ;; only a space at the start or before another space needs the escape
  (check= (export "  x " "texmacs-snippet") "\\ \\ x ")
  (check= (export "x  y" "texmacs-snippet") "x \\ y")
  (check= (export "\x01\x1f" "texmacs-snippet") "\\A\\_")
  (check= (export "Caf\xe9" "texmacs-snippet") "Caf\xe9")
  (check= (export '(document "") "texmacs-snippet") "\\;")
  (check= (export '(concat "x" (em "y") (frac "a" "b")) "texmacs-snippet")
          "x<em|y><frac|a|b>")
  (check= (export '(document "a" (with "color" "red" (document "b" "c")))
                  "texmacs-snippet")
          "a\n\n<\\with|color|red>\n  b\n\n  c\n</with>")
  (check= (export '(verbatim "x  y") "texmacs-snippet") "<verbatim|x \\ y>")
  (check= (texmacs->generic (stree->tree '(em "y")) "texmacs-snippet")
          "<em|y>"))

;; A snippet is parsed as a document: inline content comes back in a
;; document of one paragraph.
(define (test-tm-parse)
  (check-group "tm snippet parsing")
  (check= (import "a\\<less\\>b\\|c\\\\d" "texmacs-snippet")
          '(document "a<less>b|c\\d"))
  (check= (import "<frac|a|b>" "texmacs-snippet") '(document (frac "a" "b")))
  (check= (import "<\\itemize>\n  <item>a\n</itemize>" "texmacs-snippet")
          '(document (itemize (document (concat (item) "a")))))
  (check= (import "a\n\nb" "texmacs-snippet") '(document "a" "b"))
  (check= (import "" "texmacs-snippet") "")
  (check= (st (generic->texmacs "x<em|y>" "texmacs-snippet"))
          '(document (concat "x" (em "y"))))
  ;; the parser is lenient: an unfinished tag is closed
  (check= (import "<a|" "texmacs-snippet") '(document (a))))

(define (test-tm-round-trip)
  (check-group "tm snippet round trip")
  (for-each (lambda (s)
              (check= (round-trip s "texmacs-snippet") `(document ,s)))
            inline-samples)
  (for-each (lambda (s)
              (check= (round-trip s "texmacs-snippet") s))
            block-samples))

;; A .tm document needs its header <TeXmacs|version>: parse-texmacs refuses
;; a body alone. A style which is a tuple of one style is written as the
;; style itself.
(define (test-tm-document)
  (check-group "tm document")
  (for-each (lambda (s)
              (check= (round-trip (tmfile s) "texmacs-document") (tmfile s)))
            block-samples)
  (with s (export (tmfile '(document "x")) "texmacs-document")
    (check-true (string-starts? s (string-append "<TeXmacs|" (texmacs-version)
                                                 ">")))
    (check-true (contains? s "<style|generic>"))
    (check-true (contains? s "<\\body>\n  x\n</body>")))
  (check= (round-trip '(document (TeXmacs "2.1") (style (tuple "generic"))
                                 (body (document "x")))
                      "texmacs-document")
          '(document (TeXmacs "2.1") (style "generic") (body (document "x"))))
  (check= (round-trip `(document (TeXmacs "2.1")
                                 (style (tuple "article" "british"))
                                 (body (document "x"))
                                 (initial (collection
                                           (associate "page-medium" "paper"))))
                      "texmacs-document")
          `(document (TeXmacs "2.1") (style (tuple "article" "british"))
                     (body (document "x"))
                     (initial (collection (associate "page-medium" "paper")))))
  (check= (import "x" "texmacs-document") '(error "bad format or data"))
  (check= (st (generic->texmacs "x" "texmacs-document"))
          '(error "bad format or data")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Scheme format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stm is the TeXmacs tree written as a Scheme expression, and gives back
;; every tree, inline or block, unchanged.
(define (test-stm)
  (check-group "stm")
  (check= (export '(frac "a" (sqrt "b")) "stm-snippet")
          "(frac \"a\" (sqrt \"b\"))")
  (check= (export "Caf\xe9" "stm-snippet") "\"Caf\xe9\"")
  (check= (export '(document "a" "") "stm-snippet") "(document \"a\" \"\")")
  (check= (import "(frac \"a\" \"b\")" "stm-snippet") '(frac "a" "b"))
  (check= (import "\"x\"" "stm-snippet") "x")
  (check= (import "(concat \"say \\\"q\\\"\" \"a\\\\b\")" "stm-snippet")
          '(concat "say \"q\"" "a\\b"))
  (check= (import "(document (TeXmacs \"2.1\") (body (document \"x\")))"
                  "stm-document")
          '(document (TeXmacs "2.1") (body (document "x"))))
  (for-each (lambda (s) (check= (round-trip s "stm-snippet") s))
            (append inline-samples block-samples
                    (list "" '(concat) '(document))))
  (for-each (lambda (s)
              (check= (round-trip (tmfile s) "stm-document") (tmfile s)))
            block-samples)
  (check-true (string-starts? (export (tmfile '(document "x")) "stm-document")
                              "(document (TeXmacs "))
  ;; a document of the Scheme format must be a TeXmacs file
  (check= (st (generic->texmacs "(document \"x\")" "stm-document"))
          '(error "bad format or data")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The XML format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TMML writes a tag as an element, several arguments as tm-arg elements,
;; paragraphs as tm-par elements, a symbol <x> as <tm-sym>x</tm-sym>, and
;; the text in UTF-8 with the XML entities.
(define (test-tmml-serialize)
  (check-group "tmml serialization")
  (check= (export "x" "tmml-snippet") "x")
  (check= (export '(frac "a" "b") "tmml-snippet")
          "<frac><tm-arg>a</tm-arg><tm-arg>b</tm-arg></frac>")
  (check= (export '(with "color" "red" "x") "tmml-snippet")
          "<with color=\"red\">x</with>")
  (check= (export "\xe9<alpha>&<less>" "tmml-snippet")
          "\xc3\xa9\xce\xb1&amp;<tm-sym>less</tm-sym>")
  (check= (export '(em "  x  ") "tmml-snippet")
          "<em xml:space=\"preserve\">  x  </em>")
  (check= (export '(equation* (document "a")) "tmml-snippet")
          "<equation_42_>\n  <tm-par>\n    a\n  </tm-par>\n</equation_42_>")
  (with s (export (tmfile '(document "x")) "tmml-document")
    (check-true (string-starts? s "<?xml version=\"1.0\"?>"))
    (check-true (contains? s "<TeXmacs version=\""))
    (check-true (contains? s "<style>generic</style>"))))

(define (test-tmml-parse)
  (check-group "tmml parsing")
  (check= (import "<frac><tm-arg>a</tm-arg><tm-arg>b</tm-arg></frac>"
                  "tmml-snippet")
          '(frac "a" "b"))
  (check= (import "caf\xc3\xa9 &amp; &lt; <tm-sym>alpha</tm-sym>"
                  "tmml-snippet")
          "caf\xe9 & <less> <alpha>")
  (check= (import "<em>x</em>" "tmml-snippet") '(em "x"))
  ;; without xml:space="preserve", the spaces at the ends are removed
  (check= (import "<em>  x  </em>" "tmml-snippet") '(em "x"))
  (check= (import "<em xml:space=\"preserve\">  x  </em>" "tmml-snippet")
          '(em "  x  ")))

;; Every document comes back through tmml-document, and the inline content
;; through tmml-snippet; the snippets lose some information, below.
(define (test-tmml-round-trip)
  (check-group "tmml round trip")
  (for-each (lambda (s)
              (check= (round-trip (tmfile s) "tmml-document") (tmfile s)))
            (append block-samples
                    (map (lambda (s) `(document ,s)) inline-samples)))
  (for-each (lambda (s) (check= (round-trip s "tmml-snippet") s))
            (list-filter inline-samples
                         (lambda (s) (!= s "  leading and trailing  "))))
  ;; the snippet writes no xml:space attribute for a string which is not in
  ;; an element, so that its spaces at the ends are lost
  (check= (round-trip "  leading and trailing  " "tmml-snippet")
          "leading and trailing")
  ;; a document inside a tag comes back
  (check= (round-trip '(equation* (document "a" "b")) "tmml-snippet")
          '(equation* (document "a" "b")))
  ;; a snippet of several paragraphs comes back as a document
  (check= (round-trip '(document "p1" "p2") "tmml-snippet")
          '(document "p1" "p2"))
  (check= (round-trip '(document "p1" (section "S") "p2") "tmml-snippet")
          '(document "p1" (section "S") "p2")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Snippets, compared after squash: the export writes the Cork characters
;; as HTML entities, the paragraphs as p, the lists as ul/ol/li, the
;; headings (section-title, ... after the expansion) as h1-h6.
(define (test-html-export)
  (check-group "html export")
  (check= (html "plain") "plain")
  (check= (html "Caf\xe9 \xe0 na\xefve") "Caf&eacute; &agrave; na&iuml;ve")
  (check= (html "a <less> b & \"q\"") "a &lt; b &amp; &quot;q&quot;")
  (check= (html "<alpha><beta>") "&alpha;&beta;")
  (check= (html "\\back") "\\back")
  (check= (html '(document "a" "b")) "<p>a</p><p>b</p>")
  (check= (html '(concat (em "a") (strong "b") (code* "c")))
          "<em>a</em><strong>b</strong><code>c</code>")
  (check= (html '(verbatim "v")) "<tt class=\"verbatim\">v</tt>")
  (check= (html '(with "font-series" "bold" "b")) "<b>b</b>")
  (check= (html '(with "font-shape" "italic" "i")) "<i>i</i>")
  (check= (html '(with "color" "red" "r")) "<font color=\"red\">r</font>")
  (check= (html '(hlink "text" "http://x.org/?a=1&b=2"))
          "<a href=\"http://x.org/?a=1&amp;b=2\">text</a>")
  (check= (html '(label "l1")) "<a id=\"l1\"></a>")
  (check= (html '(image "pic.png" "" "" "" ""))
          "<img class=\"image\" src=\"pic.png\"></img>")
  (check= (html '(document (section-title "T") "x")) "<h2>T</h2><p>x</p>")
  (check= (html '(chapter-title "C")) "<h1>C</h1>")
  (check= (html '(subsection-title "S")) "<h3>S</h3>")
  (check= (html '(itemize (document (concat (item) "a") (concat (item) "b"))))
          "<ul><li><p>a</p></li><li><p>b</p></li></ul>")
  (check= (html '(enumerate (document (concat (item) "a"))))
          "<ol><li><p>a</p></li></ol>")
  (check= (html '(tformat (table (row (cell "a") (cell "b")))))
          (string-append "<table style=\"display: inline-table; "
                         "vertical-align: middle\"><tbody><tr><td>a</td>"
                         "<td>b</td></tr></tbody></table>"))
  (check= (html '(with "mode" "math" (concat "x" (rsup "2"))))
          "<var>x</var><sup>2</sup>")
  (check= (html '(with "mode" "math" "x+y")) "<var>x</var>+<var>y</var>")
  (check= (html '(concat "a" (next-line) "b")) "a<br />b")
  (check= (html '(hrule)) "<hr></hr>")
  (check= (html '(nbsp)) "&nbsp;")
  ;; the code environments keep their spaces and line breaks
  (check= (export '(code (document "x" "  y")) "html-snippet")
          "<pre class=\"verbatim\" xml:space=\"preserve\">\nx\n  y</pre>")
  ;; the style macros need the expansion by the editor (see the top)
  (check= (html '(section "Title")) "")
  (check= (html '(math (frac "a" "b"))) "")
  (check= (html '(tabular (tformat (table (row (cell "a"))))))
          ""))

;; The options of the HTML export, given to convert for one call.
(define (test-html-options)
  (check-group "html options")
  (with fr '(with "mode" "math" (frac "a" "b"))
    (check= (html fr (cons "texmacs->html:mathml" "on"))
            (string-append "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
                           "<mfrac><mi>a</mi><mi>b</mi></mfrac></math>"))
    (check= (html fr (cons "texmacs->html:mathjax" "on")) "\\(\\frac{a}{b}\\)")
    (check-true (string-starts? (html fr (cons "texmacs->html:mathml" "off")
                                      (cons "texmacs->html:mathjax" "off"))
                                "<table class=\"fraction\">")))
  ;; with css, a font size is a style; without, the size of HTML 3
  (with big '(with "font-size" "2" "x")
    (check= (html big (cons "texmacs->html:css" "on"))
            "<font style=\"font-size: 200%\">x</font>")
    (check= (html big (cons "texmacs->html:css" "off"))
            "<font size=\"+4\">x</font>"))
  ;; the options of the converter are the preferences of the same names
  (with opts (std-converter-options "texmacs-stree" "html-stree")
    (check= (sort (map car opts) string<?)
            '("texmacs->html:css" "texmacs->html:css-stylesheet"
              "texmacs->html:images" "texmacs->html:mathjax"
              "texmacs->html:mathml"))
    (for-each (lambda (p)
                (check= (cons (car p) (get-preference (car p))) p))
              opts)))

;; A document: an XHTML file with a head, whose style sheet depends on
;; texmacs->html:css.
(define (test-html-document)
  (check-group "html document")
  (let* ((doc (tmfile '(document "Caf\xe9" (with "font-shape" "italic" "i"))))
         (on (export doc "html-document" (cons "texmacs->html:css" "on")))
         (off (export doc "html-document" (cons "texmacs->html:css" "off"))))
    (check-true (string-starts? on "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
    (check-true (contains? on "<!DOCTYPE html"))
    (check-true (contains? on "<html xmlns=\"http://www.w3.org/1999/xhtml\""))
    (check-true (contains? on "<title>No title</title>"))
    (check-true (contains? on "<meta charset=\"utf-8\""))
    (check-true (contains? (squash on) "<body><p>Caf&eacute;</p><p><i>i</i></p></body>"))
    (check-true (contains? on "</html>"))
    ;; the style sheet is written with and without css
    (check-true (contains? on "<style type=\"text/css\">"))
    (check-true (contains? off "<style type=\"text/css\">"))
    (check-true (contains? (squash off) "<p>Caf&eacute;</p>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-html-import)
  (check-group "html import")
  (check= (import "<p>Hello <em>world</em></p>" "html-snippet")
          '(concat "Hello " (em "world")))
  (check= (import "<p>a</p><p>b</p>" "html-snippet") '(document "a" "b"))
  (check= (import "<h1>T</h1><p>x</p>" "html-snippet")
          '(document (chapter* "T") "x"))
  (check= (import "<h2>T</h2>" "html-snippet") '(section* "T"))
  (check= (import "<h3>T</h3>" "html-snippet") '(subsection* "T"))
  (check= (import "<ul><li>a</li><li>b</li></ul>" "html-snippet")
          '(itemize (document (concat (item) "a") (concat (item) "b"))))
  (check= (import "<ol><li>a</li></ol>" "html-snippet")
          '(enumerate (concat (item) "a")))
  (check= (import "<a href=\"http://x.org\">t</a>" "html-snippet")
          '(hlink "t" "http://x.org"))
  (check= (import "<img src=\"pic.png\"/>" "html-snippet")
          '(image "pic.png" "0.6383w" "" "" ""))
  (check= (import "caf&eacute; &lt;&amp;&gt; &#233; &alpha;" "html-snippet")
          "caf\xe9 <less>&<gtr> \xe9 <alpha>")
  (check= (import "caf\xc3\xa9" "html-snippet") "caf\xe9")
  (check= (import "<b>b</b><i>i</i><strong>s</strong><code>c</code>"
                  "html-snippet")
          '(concat (with "font-series" "bold" "b")
                   (with "font-shape" "italic" "i")
                   (strong "s") (code* "c")))
  (check= (import "<table><tr><td>a</td><td>b</td></tr></table>"
                  "html-snippet")
          '(tabular (tformat (twith "table-hmode" "min")
                             (twith "table-width" "1par")
                             (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                             (table (row (cell "a") (cell "b"))))))
  (check= (import "<pre>a\n  b</pre>" "html-snippet")
          '(code (document "a" "  b")))
  (check= (import "a<br/>b" "html-snippet") '(concat "a" (next-line) "b"))
  ;; a document gets the style browser
  (check= (import "<html><body><p>x</p></body></html>" "html-document")
          '(document (body "x") (style "browser")))
  (check= (import "<html><head><title>T</title></head><body><p>a</p><p>b</p></body></html>"
                  "html-document")
          '(document (body (document "a" "b")) (style "browser"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML round trip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TeXmacs -> HTML -> TeXmacs gives back these constructs ...
(define html-same
  (list "plain" "Caf\xe9 na\xefve" "a <less> b & c <gtr> d" "<alpha><beta>"
        "\\back" '(em "x") '(strong "x") '(concat "a" (em "b") "c")
        '(code* "c") '(with "font-series" "bold" "b")
        '(with "font-shape" "italic" "i") '(with "color" "red" "r")
        '(hlink "t" "http://x.org/?a=1&b=2") '(label "l")
        '(concat (label "l") "x") '(document "a" "b")
        '(itemize (document (concat (item) "a") (concat (item) "b")))
        '(enumerate (document (concat (item) "a") (concat (item) "b")))
        '(hrule) '(concat "a" (next-line) "b") '(code (document "x" "  y"))))

;; ... and changes these ones (the expected result is the second element).
;; HTML has no tags for the TeXmacs ones: the headings come back unnumbered,
;; the mathematics as text, the tables with the attributes of HTML tables,
;; the underlining and the striking through are lost; the import makes
;; TeXmacs quotes of the straight quotes.
(define html-changed
  '(("say \"q\"" "say ``q\"")
    ((verbatim "v") (with "font-family" "tt" "v"))
    ((tt "t") (with "font-family" "tt" "t"))
    ((image "pic.png" "" "" "" "") (image "pic.png" "0.6383w" "" "" ""))
    ((document (section-title "T") "x") (document (section* "T") "x"))
    ((chapter-title "C") (chapter* "C"))
    ((tformat (table (row (cell "a") (cell "b"))))
     (tabular (tformat (twith "table-hmode" "min") (twith "table-width" "1par")
                       (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                       (table (row (cell "a") (cell "b"))))))
    ((with "mode" "math" (concat "x" (rsup "2"))) (concat (var "x") (rsup "2")))
    ((with "mode" "math" "x+y") (concat (var "x") " + " (var "y")))
    ((equation* (document "x=1"))
     (with "par-mode" "center" (concat (var "x") " = 1")))
    ((underline "u") "u")
    ((strike-through "s") "s")
    ((nbsp) (concat "" (nbsp) ""))))

(define (test-html-round-trip)
  (check-group "html round trip")
  (for-each (lambda (s) (check= (round-trip s "html-snippet") s)) html-same)
  (for-each (lambda (p) (check= (round-trip (car p) "html-snippet") (cadr p)))
            html-changed)
  ;; a document comes back with the style browser
  (check= (round-trip (tmfile '(document "a" (em "b"))) "html-document")
          '(document (body (document "a" (em "b"))) (style "browser")))
  ;; a description list is a dl of dt and dd, without a p around each
  ;; pair, and comes back without an empty paragraph
  (with dl '(description (document (concat (item* "k") "v")
                                   (concat (item* "l") "w")))
    (check= (html dl) "<dl><dt>k</dt><dd><p>v</p></dd><dt>l</dt><dd><p>w</p></dd></dl>")
    (check= (round-trip dl "html-snippet")
            '(description (document (concat (item* "k") "v")
                                    (concat (item* "l") "w")))))
  (check= (round-trip '(description (document (concat (item* "k") "v")))
                      "html-snippet")
          '(description (concat (item* "k") "v"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TeXmacs has no Markdown converter, neither in convert/ nor in the
;; plugins: the format is unknown and a .md file is of no known format.
(define (test-markdown)
  (check-group "markdown")
  (check-false (format? "markdown"))
  (check-false (format? "md"))
  (check= (format-from-suffix "md") "generic")
  (check= (converter-search "texmacs-tree" "markdown-document") #f)
  (check= (convert (stree->tree "x") "texmacs-tree" "markdown-snippet") #f)
  (check= (texmacs->generic (stree->tree "x") "markdown-document")
          "Error: bad format or data"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The export writes one line per paragraph (an empty line between the
;; paragraphs when wrapping, which also breaks the lines after 78
;; characters), the text of the tags, the mathematics in a linear notation
;; and the tables as aligned columns. The encoding "auto" is UTF-8.
(define (test-verbatim-export)
  (check-group "verbatim export")
  (check= (export '(document "a" "b") "verbatim-snippet") "a\nb")
  (check= (export '(document "a" "" "b") "verbatim-snippet") "a\n\nb")
  (check= (export '(document "a" "b") "verbatim-document") "a\nb")
  (check= (export '(document "a" "b") "verbatim-document"
                  (cons "texmacs->verbatim:wrap" "on"))
          "a\n\nb")
  (with long (apply string-append (make-list 20 "word "))
    (check= (export (string-drop-right long 1) "verbatim-snippet"
                    (cons "texmacs->verbatim:wrap" "on"))
            (string-append (string-drop-right (apply string-append
                                                     (make-list 15 "word "))
                                              1)
                           "\nword word word word word"))
    (check= (export (string-drop-right long 1) "verbatim-snippet")
            (string-drop-right long 1)))
  (check= (export '(concat "x" (em "y") (frac "1" "2")) "verbatim-snippet")
          "xy1/2")
  (check= (export '(with "mode" "math"
                     (concat "x" (rsup "2") (rsub "i") (sqrt "y")
                             (frac "a+b" "c")))
                  "verbatim-snippet")
          "x^2_isqrt(y)(a+b)/c")
  (check= (export '(itemize (document (concat (item) "a") (concat (item) "b")))
                  "verbatim-snippet")
          "a\nb")
  (check= (export '(concat "a" (space "1em") "b") "verbatim-snippet") "a b")
  (check= (export '(tformat (table (row (cell "a") (cell "bb"))
                                   (row (cell "ccc") (cell "d"))))
                  "verbatim-snippet")
          "a   bb\nccc d\n")
  ;; a line break of TeXmacs is not a line break of the text
  (check= (export '(concat "a" (next-line) "b") "verbatim-snippet") "ab")
  (check= (export "<alpha><less><gtr>\xe9" "verbatim-snippet") "\xce\xb1<>\xc3\xa9")
  (check= (export "<alpha><less><gtr>\xe9" "verbatim-snippet"
                  (cons "texmacs->verbatim:encoding" "utf-8"))
          "\xce\xb1<>\xc3\xa9")
  (check= (export "<alpha><less><gtr>\xe9" "verbatim-snippet"
                  (cons "texmacs->verbatim:encoding" "cork"))
          "<alpha><less><gtr>\xe9")
  ;; Latin-1 has no alpha, which is left out
  (check= (export "<alpha><less><gtr>\xe9" "verbatim-snippet"
                  (cons "texmacs->verbatim:encoding" "iso-8859-1"))
          "<>\xe9")
  ;; a document of TeXmacs gives its body
  (check= (export (tmfile '(document "x" "y")) "verbatim-document") "x\ny"))

;; The import makes a paragraph of each line, keeps the empty lines as
;; empty paragraphs, expands the tabs to the next multiple of 8 and gives
;; a document the verbatim style (tt font, language verbatim). With
;; wrapping, the lines of a paragraph are joined.
(define (test-verbatim-import)
  (check-group "verbatim import")
  (check= (import "a\nb" "verbatim-snippet") '(document "a" "b"))
  (check= (import "a\n\nb" "verbatim-snippet") '(document "a" "" "b"))
  (check= (import "a\n" "verbatim-snippet") '(document "a" ""))
  (check= (import "a\r\nb" "verbatim-snippet") '(document "a" "b"))
  (check= (import "a\rb" "verbatim-snippet") '(document "a" "b"))
  (check= (import "x" "verbatim-snippet") "x")
  (check= (import "" "verbatim-snippet") "")
  (check= (import "a<b" "verbatim-snippet") "a<less>b")
  (check= (import "a\\b" "verbatim-snippet") "a\\b")
  (check= (import "  x\ty" "verbatim-snippet") "  x     y")
  (check= (import "caf\xc3\xa9" "verbatim-snippet") "caf\xe9")
  (check= (import "caf\xe9" "verbatim-snippet"
                  (cons "verbatim->texmacs:encoding" "iso-8859-1"))
          "caf\xe9")
  (check= (import "a\nb\n\nc" "verbatim-snippet"
                  (cons "verbatim->texmacs:wrap" "on"))
          '(document "a b" "c"))
  (check= (import "a\nb" "verbatim-document")
          '(document (body (document "a" "b"))
                     (initial (collection (associate "language" "verbatim")
                                          (associate "font-family" "tt")
                                          (associate "par-first" "0cm"))))))

;; Plain text gives back the paragraphs of plain strings; the markup is
;; lost.
(define (test-verbatim-round-trip)
  (check-group "verbatim round trip")
  (for-each (lambda (s) (check= (round-trip s "verbatim-snippet") s))
            (list "plain" "Caf\xe9 na\xefve" "a <less> b & \"q\" \\x"
                  "<alpha><beta>" '(document "a" "b") '(document "a" "" "b")
                  '(document "  indented" "x")))
  (check= (round-trip '(document (em "a") (strong "b")) "verbatim-snippet")
          '(document "a" "b"))
  (check= (round-trip '(concat "a" (next-line) "b") "verbatim-snippet") "ab"))

;; The source code formats are verbatim with the encoding SourceCode.
(define (test-code)
  (check-group "source code")
  (check= (export '(document "x=1" "y") "code-snippet") "x=1\ny")
  (check= (import "x=1\ny" "code-snippet") '(document "x=1" "y"))
  (check= (export '(document "(define x 1)") "scheme-snippet") "(define x 1)")
  (check= (import "(define x 1)\n(f x)" "scheme-snippet")
          '(document "(define x 1)" "(f x)"))
  (check= (round-trip '(document "(f \"a\")" "  (g)") "scheme-snippet")
          '(document "(f \"a\")" "  (g)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The X-file formats of define-format save to and load from a temporary
;; file, removed afterwards.
(define (test-files)
  (check-group "files")
  (let* ((doc (tmfile '(document "Caf\xe9" (em "x"))))
         (u (export doc "texmacs-file")))
    (check-true (url? u))
    (check-true (and (url? u) (url-exists? u)))
    (check= (and (url? u) (st (convert u "texmacs-file" "texmacs-tree"))) doc)
    (check= (and (url? u) (convert u "texmacs-file" "verbatim-document"))
            "Caf\xc3\xa9\nx")
    (check= (and (url? u) (import (convert u "texmacs-file" "stm-document")
                                  "stm-document"))
            doc)
    (when (url? u) (system-remove u)))
  (check= (file-format (string->url "a.tm")) "texmacs-file")
  (check= (file-format (string->url "a.HTML")) "html-file")
  (check= (file-format (string->url "a.xyz")) "generic-file"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The registry of formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tm-convert-test checks format?, format-get-name and format-from-suffix
;; for the source code formats and a few suffixes; here the document
;; formats, their suffixes and the content recognition.
(define format-table
  ;; format, name, suffixes
  '(("texmacs" "TeXmacs" ("tm" "ts" "tp"))
    ("stm" "TeXmacs Scheme" ("stm"))
    ("tmml" "Xml" ("tmml"))
    ("html" "Html" ("html" "xhtml" "htm"))
    ("verbatim" "Verbatim" ())
    ("code" "Source code" ())
    ("scheme" "Scheme source code" ("scm"))
    ("latex" "LaTeX" ("tex"))
    ("bibtex" "RawBibTeX" ("rawbib"))
    ("tmbib" "BibTeX" ("bib"))))

(define (test-format-names)
  (check-group "format names")
  (for-each
   (lambda (e)
     (let ((fm (car e)) (name (cadr e)) (sufs (caddr e)))
       (check-true (format? fm))
       (check= (format-get-name fm) name)
       (check= (format-get-suffixes* fm) (cons 'tuple sufs))
       (check= (format-default-suffix fm) (if (null? sufs) "" (car sufs)))
       (check= (format-default-suffix (string-append fm "-file"))
               (if (null? sufs) "" (car sufs)))
       (for-each (lambda (s)
                   (check= (format-from-suffix s) fm)
                   (check= (format-from-suffix (upcase-all s)) fm))
                 sufs)))
   format-table)
  (check= (format-from-suffix "txt") "generic")
  (check= (format-from-suffix "") "generic"))

;; format-determine recognizes the content first; else it uses the suffix,
;; but not for a format which must be recognized (texmacs, stm, tmml),
;; where the text is verbatim.
(define (test-format-determine)
  (check-group "format determine")
  (check= (format-determine "<TeXmacs|2.1>\n" "") "texmacs")
  (check= (format-determine "(document (TeXmacs \"2.1\"))" "") "stm")
  (check= (format-determine "<?xml version=\"1.0\"?>\n<TeXmacs version=\"2.1\">"
                            "")
          "tmml")
  (check= (format-determine "<html><body/></html>" "") "html")
  (check= (format-determine "<!DOCTYPE html>\n<html>" "") "html")
  (check= (format-determine "  <body>x</body>" "") "html")
  (check= (format-determine "hello" "") "verbatim")
  (check= (format-determine "hello" "tm") "verbatim")
  (check= (format-determine "hello" "stm") "verbatim")
  (check= (format-determine "hello" "tmml") "verbatim")
  (check= (format-determine "hello" "txt") "verbatim")
  (check= (format-determine "hello" "html") "html")
  (check= (format-determine "hello" "scm") "scheme")
  (check-true (format-recognizes? "<TeXmacs|2.1>" "texmacs"))
  (check-true (format-recognizes? "<html>" "html"))
  (check-false (format-recognizes? "<p>x</p>" "html"))
  (check-false (format-recognizes? "<TeXmacs|2.1>" "html"))
  (check-false (format-recognizes? "x" "verbatim")))

;; Each format declared with define-format has the converters
;; X-document <-> X-file; the converters declared in init-rewrite,
;; init-tmml and init-html give these paths.
(define (test-converter-paths)
  (check-group "converter paths")
  (for-each
   (lambda (fm)
     (let ((doc (string-append fm "-document"))
           (file (string-append fm "-file")))
       (check= (converter-search doc file) (list doc file))
       (check= (converter-search file doc) (list file doc))))
   (map car format-table))
  (for-each
   (lambda (p) (check= (converter-search (car p) (cAr p)) p))
   '(("texmacs-tree" "texmacs-document") ("texmacs-document" "texmacs-tree")
     ("texmacs-tree" "texmacs-snippet") ("texmacs-snippet" "texmacs-tree")
     ("texmacs-tree" "texmacs-stree") ("texmacs-stree" "texmacs-tree")
     ("texmacs-tree" "stm-document") ("stm-snippet" "texmacs-tree")
     ("texmacs-tree" "texmacs-stree" "tmml-stree" "tmml-document")
     ("tmml-snippet" "tmml-stree" "texmacs-stree" "texmacs-tree")
     ("texmacs-tree" "texmacs-stree" "html-stree" "html-snippet")
     ("html-document" "html-stree" "texmacs-stree" "texmacs-tree")
     ("texmacs-tree" "verbatim-document") ("verbatim-snippet" "texmacs-tree")
     ("texmacs-tree" "code-snippet") ("scheme-document" "texmacs-tree")
     ("texmacs-file" "texmacs-document" "texmacs-tree" "texmacs-stree"
      "html-stree" "html-document" "html-file")
     ("html-file" "html-document" "html-stree" "texmacs-stree"
      "texmacs-tree" "texmacs-document" "texmacs-file")
     ("stm-file" "stm-document" "texmacs-tree" "texmacs-stree"
      "tmml-stree" "tmml-document" "tmml-file")))
  (check= (converter-search "texmacs-tree" "no-such-format-document") #f)
  (check= (convert (stree->tree "x") "texmacs-tree" "no-such-format-document")
          #f)
  (check= (texmacs->generic (stree->tree "x") "no-such-format")
          "Error: bad format or data")
  (check= (st (generic->texmacs "x" "no-such-format"))
          '(error "bad format or data"))
  ;; every format reachable from TeXmacs is reachable back, and conversely
  (let ((from (converters-from "texmacs-tree"))
        (to (converters-to "texmacs-tree")))
    (for-each (lambda (x) (check-true (in? x from)))
              '("texmacs-document" "texmacs-snippet" "stm-document"
                "stm-snippet" "tmml-document" "tmml-snippet" "html-document"
                "html-snippet" "verbatim-document" "verbatim-snippet"
                "code-document" "scheme-snippet" "texmacs-file" "html-file"))
    (for-each (lambda (x) (check-true (in? x to)))
              '("texmacs-document" "texmacs-snippet" "stm-document"
                "stm-snippet" "tmml-document" "tmml-snippet" "html-document"
                "html-snippet" "verbatim-document" "verbatim-snippet"
                "code-document" "scheme-snippet" "texmacs-file" "html-file"))))

;; The formats of the import and export menus: those with a -file
;; converter from or to texmacs-file, without the hidden ones (bibtex) and
;; the source code formats.
(define (test-format-menus)
  (check-group "format menus")
  (let ((ex (converters-from-special "texmacs-file" "-file" #f))
        (im (converters-to-special "texmacs-file" "-file" #f)))
    (for-each (lambda (fm) (check-true (in? fm ex)))
              '("html" "latex" "stm" "tmml" "verbatim" "code"))
    (for-each (lambda (fm) (check-true (in? fm im)))
              '("html" "latex" "stm" "tmml" "verbatim" "code"))
    (for-each (lambda (fm) (check-false (in? fm ex)))
              '("texmacs" "bibtex" "scheme" "python" "markdown"))
    (for-each (lambda (fm) (check-false (in? fm im)))
              '("texmacs" "bibtex" "scheme" "python" "markdown"))
    (check-true (in? "texmacs" (converters-from-special "texmacs-file" "-file"
                                                        #t)))
    ;; every format of the menus can be reached from and to texmacs-file
    (for-each (lambda (fm)
                (check-true (list? (converter-search
                                    "texmacs-file"
                                    (string-append fm "-file")))))
              ex)
    (for-each (lambda (fm)
                (check-true (list? (converter-search
                                    (string-append fm "-file")
                                    "texmacs-file"))))
              im)))

;; The verbatim converters have the options wrap and encoding, whose values
;; are the preferences of the same names.
(define (test-converter-options)
  (check-group "converter options")
  (for-each
   (lambda (p)
     (with opts (std-converter-options (car p) (cadr p))
       (check= (sort (map car opts) string<?) (caddr p))
       (for-each (lambda (o) (check= (cons (car o) (get-preference (car o))) o))
                 opts)))
   '(("texmacs-tree" "verbatim-document"
      ("texmacs->verbatim:encoding" "texmacs->verbatim:wrap"))
     ("texmacs-tree" "verbatim-snippet"
      ("texmacs->verbatim:encoding" "texmacs->verbatim:wrap"))
     ("verbatim-document" "texmacs-tree"
      ("verbatim->texmacs:encoding" "verbatim->texmacs:wrap"))
     ("verbatim-snippet" "texmacs-tree"
      ("verbatim->texmacs:encoding" "verbatim->texmacs:wrap"))))
  (check= (std-converter-options "texmacs-tree" "texmacs-document") '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (formats-test-failures)
  (check-suite "formats")
  (test-tm-serialize)
  (test-tm-parse)
  (test-tm-round-trip)
  (test-tm-document)
  (test-stm)
  (test-tmml-serialize)
  (test-tmml-parse)
  (test-tmml-round-trip)
  (test-html-export)
  (test-html-options)
  (test-html-document)
  (test-html-import)
  (test-html-round-trip)
  (test-markdown)
  (test-verbatim-export)
  (test-verbatim-import)
  (test-verbatim-round-trip)
  (test-code)
  (test-files)
  (test-format-names)
  (test-format-determine)
  (test-converter-paths)
  (test-format-menus)
  (test-converter-options)
  (check-end))
