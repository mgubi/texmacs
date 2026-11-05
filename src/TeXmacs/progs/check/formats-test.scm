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
  (:use (convert html html-formulas)
        (check check-lib)))

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
  ;; only a space at the start, before another space or at the end needs
  ;; the escape
  (check= (export "  x " "texmacs-snippet") "\\ \\ x\\ ")
  (check= (import (export "  x " "texmacs-snippet") "texmacs-snippet")
          '(document "  x "))
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

;; <string|abc> is a tag like <foo|abc> (see test-string-tag in trees-test): it
;; used to crash the loading of a .tm file, or to give random bytes (#678).
(define (test-tm-string-tag)
  (check-group "the tag string in a tm document")
  (let* ((body '(document (concat "x" (string "abc") "y" (string)
                                  (compound "string" "z"))))
         (doc (tmfile body))
         (text (export doc "texmacs-document")))
    (check-true (contains? text "<string|abc>"))
    (check-true (contains? text "<string>"))
    (check-true (contains? text "<compound|string|z>"))
    (check= (import text "texmacs-document") doc)
    (check= (round-trip doc "texmacs-document") doc)
    (check= (round-trip doc "tmml-document") doc)
    (check= (round-trip doc "stm-document") doc))
  (check= (import "<string|abc>" "texmacs-snippet") '(document (string "abc")))
  (check= (import "<\\string>\n  abc\n</string>" "texmacs-snippet")
          '(document (string (document "abc"))))
  (check= (export '(string "abc") "texmacs-snippet") "<string|abc>")
  (check= (import "(document (string \"abc\"))" "stm-snippet")
          '(document (string "abc")))
  ;; through the upgrade of the documents of old versions
  (for-each (lambda (form)
              (with text (string-append "<TeXmacs|1.0.0>\n\n<style|generic>\n\n"
                                        "<\\body>\n  " form "\n</body>\n")
                (check= (assoc 'body (cdr (import text "texmacs-document")))
                        '(body (document (string "abc"))))))
            '("<string|abc>" "<apply|string|abc>" "<expand|string|abc>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Scheme format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stm is the TeXmacs tree written as a Scheme expression, and gives back
;; every tree, inline or block, unchanged.
;; raw-data holds the bytes of an embedded file, written <#hexadecimal> in a tm
;; file. Without its argument, which only a hand-written or damaged file has,
;; it crashed the writer, and in the tmml format it was an error; a <#...> next
;; to other content made the reader lose the whole body (#685).
(define (test-tm-raw-data)
  (check-group "the tag raw-data in a tm document")
  (for-each
   (lambda (body)
     (with doc (tmfile `(document ,body))
       (check= (import (export doc "texmacs-document") "texmacs-document") doc)
       (check= (round-trip doc "tmml-document") doc)
       (check= (round-trip doc "stm-document") doc)))
   '((raw-data)
     (raw-data "")
     (raw-data "a\xff")
     (raw-data (em "q"))
     (concat "x " (raw-data) " y")
     (concat "x " (raw-data "") " y")
     (concat "x " (raw-data "a") " y " (raw-data "b"))
     (concat "x " (raw-data (em "q")) " y")
     (image (tuple (raw-data) "png") "1cm" "" "" "")
     (image (tuple (raw-data "") "png") "1cm" "" "" "")
     (image (tuple (raw-data "a") "png") "1cm" "" "" "")))
  (check= (export '(raw-data) "texmacs-snippet") "<raw-data>")
  (check= (export '(raw-data "") "texmacs-snippet") "<#>")
  (check= (export '(raw-data "a\xff") "texmacs-snippet") "<#61FF>")
  (check= (export '(raw-data (em "q")) "texmacs-snippet") "<raw-data|<em|q>>")
  (check= (export '(concat "x " (raw-data) " y") "texmacs-snippet")
          "x <raw-data> y")
  (check= (import "x <#61> y" "texmacs-snippet")
          '(document (concat "x " (raw-data "a") " y")))
  (check= (import "<foo|<#61> c|b>" "texmacs-snippet")
          '(document (foo (concat (raw-data "a") " c") "b")))
  (check= (import "<\\foo>\n  <#61>\n</foo>" "texmacs-snippet")
          '(document (foo (document (raw-data "a")))))
  ;; a file which ends inside the tag
  (check= (import "<#" "texmacs-snippet") '(document (raw-data "")))
  (check= (import "a<#" "texmacs-snippet")
          '(document (concat "a" (raw-data ""))))
  ;; the other formats have no raw data: the text around it is kept
  (check= (export '(concat "x " (raw-data) " y") "verbatim-snippet") "x  y")
  (check= (export '(concat "x " (raw-data) " y") "html-snippet") "x  y"))

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
  ;; the background of a cell: a pattern of 3, 4 or 5 arguments, with the base
  ;; color (opaque) as background-color
  (with cell (lambda (pat)
               (html `(tformat (cwith "1" "1" "1" "1" "cell-background" ,pat)
                               (table (row (cell "a"))))))
    (check= (cell '(pattern "a.png" "1cm" "1cm"))
            "<table style=\"display: inline-table; vertical-align: middle\"><tbody><tr><td style=\"background: url('a.png'); background-size: 1cm 1cm\">a</td></tr></tbody></table>")
    (check= (cell '(pattern "a.png" "1cm" "1cm" (eff-recolor "0" "red")))
            "<table style=\"display: inline-table; vertical-align: middle\"><tbody><tr><td style=\"background: url('a.png'); background-size: 1cm 1cm\">a</td></tr></tbody></table>")
    (check= (cell '(pattern "a.png" "1cm" "1cm" "" "#ffe0a080"))
            "<table style=\"display: inline-table; vertical-align: middle\"><tbody><tr><td style=\"background: url('a.png'); background-size: 1cm 1cm; background-color: #ffe0a0\">a</td></tr></tbody></table>")
    (check= (cell '(pattern "a.png" "1cm" "1cm" "" "none"))
            "<table style=\"display: inline-table; vertical-align: middle\"><tbody><tr><td style=\"background: url('a.png'); background-size: 1cm 1cm\">a</td></tr></tbody></table>"))
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
  ;; MathML left on with another format does not change the page or its links
  (let* ((doc (tmfile '(document (concat (with "mode" "math" "x")
                                         (hlink "y" "other.tm")))))
         (ml (cons "texmacs->html:mathml" "on"))
         (s (export doc "html-document" (static-on) ml))
         (j (export doc "html-document" (cons "texmacs->html:mathjax" "on") ml))
         (m (export doc "html-document" ml)))
    (check-false (contains? s "xhtml-math-svg"))
    (check-true (contains? s "other.html"))
    (check-false (contains? j "xhtml-math-svg"))
    (check-true (contains? j "other.html"))
    (check-true (contains? m "xhtml-math-svg"))
    (check-true (contains? m "other.xhtml")))
  ;; the one setting which chooses how formulas are written
  (check-true (in? (html-formulas)
                   '("static" "mathjax" "mathml" "images" "html")))
  (check-true (html-formulas? (html-formulas)))
  ;; the options of the converter are the preferences of the same names
  (with opts (std-converter-options "texmacs-stree" "html-stree")
    (check= (sort (map car opts) string<?)
            '("texmacs->html:css" "texmacs->html:css-stylesheet"
              "texmacs->html:images" "texmacs->html:mathjax"
              "texmacs->html:mathml" "texmacs->html:static-math"))
    (for-each (lambda (p)
                (check= (cons (car p) (get-preference (car p))) p))
              opts)))

;; Formulas typeset by TeXmacs and written as SVG (Plugins/Html_math).
(define (count-of what s)
  (let loop ((pos 0) (n 0))
    (with i (string-search-forwards what pos s)
      (if (< i 0) n (loop (+ i 1) (+ n 1))))))

(define (static-on) (cons "texmacs->html:static-math" "on"))

(define (check-html-static-buffer)
  ;; through html-expand, as the export of a buffer
  (let* ((table '(matrix (tformat (table (row (cell "1") (cell "2"))))))
         (doc `(document (equation* (document ,table))))
         (exp (tree->stree (html-expand (stree->tree doc))))
         (s (html exp (static-on)))
         (plain (html `(with "mode" "math" "math-display" "true" ,table)
                      (static-on)))
         (glyphs (lambda (t)
                   (let loop ((pos 0) (l '()))
                     (with i (string-search-forwards "<use href=\"#" pos t)
                       (if (< i 0) (reverse l)
                           (let ((e (string-search-forwards "\"" (+ i 12) t)))
                             (loop e (cons (substring t (+ i 12) e) l)))))))))
    (check= (glyphs s) (glyphs plain)))
  (let* ((a (html '(with "mode" "math" "x") (static-on)))
         (b (html '(section-title (with "mode" "math" "x")) (static-on))))
    (check-false (== a b))))

;; A formula as SVG with its hidden MathML, before other ways of writing it.
(define (test-html-static)
  (check-group "html static formulas")
  ;; a formula: SVG, its hidden source, and the glyphs it uses
  (with s (html '(with "mode" "math" (frac "a" "b")) (static-on))
    (check-true (string-starts? s "<span class=\"tm-math\""))
    (check-true (contains? s "<svg "))
    (check-true (contains? s "<use "))
    (check-true (contains? s "<mfrac><mi>a</mi><mi>b</mi></mfrac>"))
    (check-true (contains? s "style=\"vertical-align:"))
    ;; its LaTeX, as text which is searched and copied
    (check-true (contains? s "class=\"tm-tex\""))
    (check-true (contains? s "aria-hidden=\"true\">$\\frac{a}{b}$</span>")))
  ;; its lines are one line (not dot accents), empty formulas and pictures
  ;; have none, the links are below it, and cells are formulas of the text
  (let* ((terms (append-map (lambda (i) (list "a" `(rsub ,(number->string i))
                                              "+"))
                            (... 1 40)))
         (long (html `(with "mode" "math" (concat ,@terms "b")) (static-on))))
    (check-true (contains? long "a_{40} + b$"))
    (check-false (contains? long "&#x02D9;")))
  (check-false (contains? (html '(with "mode" "math" "") (static-on)) "tm-tex"))
  (check-false
   (contains? (html '(with "mode" "math"
                       (concat "x" (graphics (line (point "0" "0")
                                                   (point "1" "1")))))
                    (static-on))
              "tm-tex"))
  (check-false (contains? (html '(with "mode" "math" (tree "a" "b"))
                                (static-on))
                          "tm-tex"))
  (check-true (contains? (html '(with "mode" "math"
                                  (concat "x" (specific "latex" "y")))
                               (static-on))
                         "tm-tex"))
  (check-false (contains? (html '(with "mode" "math"
                                   (concat "x" (specific "image" "y")))
                                (static-on))
                          "tm-tex"))
  (check-true (contains? (html '(with "mode" "math" "x") (static-on))
                         "pointer-events:none"))
  ;; Safari selects a formula as a whole, without its MathML
  (with s (html '(with "mode" "math" "x") (static-on))
    (check-true (contains? s "-webkit-user-select:all;user-select:all"))
    (check-true (contains? s "-webkit-user-select:none;user-select:none")))
  (with s (html '(eqnarray* (document (tformat (table (row (cell "a")
                                                           (cell "=")
                                                           (cell "b"))))))
                (static-on))
    (check-true (contains? s ">$a$</span>"))
    (check-false (contains? s "$$")))
  ;; before the other ways of exporting formulas
  (with s (html '(with "mode" "math" "x") (static-on)
                (cons "texmacs->html:mathjax" "on")
                (cons "texmacs->html:mathml" "on"))
    (check-true (contains? s "<svg ")))
  ;; the glyphs of several formulas once, also in a snippet
  (with s (html '(concat (with "mode" "math" "x") " "
                         (with "mode" "math" "x+y"))
                (static-on))
    (check= (count-of "<svg " s) 3)
    (check= (count-of "<defs>" s) 1)
    (check= (count-of "-120\" d=" s) 1))
  ;; the html-stree between the conversions, not written out
  (let* ((on (static-on))
         (h (convert '(concat "a " (with "mode" "math" (frac "x" "y")) " b")
                     "texmacs-stree" "html-stree" on))
         (back (object->string (convert h "html-stree" "texmacs-stree"))))
    (check-true (contains? back "frac"))))

;; Colors and transparency, of glyphs, rules and fills.
(define (test-html-static-colors)
  (check-group "html static formulas: colors")
  ;; black is the color of the text around, other colors are written
  (with s (html '(with "mode" "math" (with "color" "red" "x")) (static-on))
    (check-true (contains? s "fill=\"#ff0000\"")))
  (with s (html '(with "mode" "math" "x") (static-on))
    (check= (count-of " fill=" s) 1)
    (check-true (contains? s "aria-hidden=\"true\" fill=\"currentColor\"")))
  ;; what is filled has the color of its brush
  (with s (html '(with "mode" "math"
                   (concat "x"
                     (graphics (with "fill-color" "red" "color" "blue"
                                 (cline (point "0" "0") (point "1" "0")
                                        (point "1" "1"))))))
                (static-on))
    (check-true (contains? s "fill=\"#ff0000\"")))
  ;; only outlines of glyphs have the stroke: not rules, nor bitmap glyphs
  (with s (html '(with "mode" "math" "math-font" "Fira" "<bbb-R>") (static-on))
    (check-true (contains? s "\" stroke=\"none\"/>")))
  (with s (html '(with "mode" "math" (with "color" "#ff000080" "x"))
                (static-on))
    (check-true (contains? s "fill-opacity"))
    (check-false (contains? s "stroke-opacity")))
  ;; black in a formula is black, and the color of the text is inherited
  (with s (html '(with "color" "red"
                   (with "mode" "math" (concat (with "color" "black" "k") "z")))
                (static-on))
    (check-true (contains? s "fill=\"#000000\""))
    (check-false (contains? s "fill=\"#ff0000\"")))
  (check-false (contains? (html '(with "mode" "math" "x") (static-on))
                          "fill=\"#000000\""))
  ;; copies of a glyph with an alpha make one group: not other glyphs
  (check-false (contains? (html '(with "mode" "math"
                                  (with "color" "#ff000080" "x+x"))
                                (static-on))
                          "<g opacity"))
  (when (font-exists-in-tt? "Luminari")
    (check-true
     (contains? (html '(with "mode" "math" "color" "#ff000080"
                             "font" "Luminari" "font-series" "bold"
                             "math-font" "Luminari" "math-font-series" "bold"
                             "x")
                      (static-on))
                "<g opacity")))
  ;; a colored glyph has a stroke of its color
  (check-true (contains? (html '(with "mode" "math" (with "color" "red" "x"))
                               (static-on))
                         "fill=\"#ff0000\" stroke=\"#ff0000\"")))

;; Glyphs, decorations, clips and what cannot be drawn as SVG.
(define (test-html-static-glyphs)
  (check-group "html static formulas: glyphs and pictures")
  ;; the decorations of atoms, in inline formulas too
  (check-true
   (contains? (html '(with "mode" "math" (underline "x")) (static-on))
              "<path d="))
  ;; the stretched symbols of virtual fonts are whole glyphs, in any font:
  ;; slices under clips leave seams in the browsers
  (for-each
   (lambda (fn)
     (let ((s (html `(with "mode" "math" "math-font" ,fn
                       (concat (left "{" "-3em" "3em") "x" (right ".")
                               (wide "a+b+c+d" "<wide-overbrace>")
                               (sqrt (frac (frac "1" "2") (frac "3" "4")))))
                    (static-on))))
       (check-false (contains? s "clip-path=")))
     #t)
   '("roman" "stix" "math-stix" "math-pagella" "math-termes" "math-bonum"
     "math-schola"))
  ;; also those which stand for other symbols of the font
  (with s (html '(with "mode" "math" "math-font" "Fira" "<rightleftharpoons>")
                (static-on))
    (check-false (contains? s "clip-path=")))
  ;; a glyph without outline: blackboard bold made from a font without it
  (with s (html '(with "mode" "math" "math-font" "Fira" "<bbb-R>") (static-on))
    (check-true (contains? s "href=\"#tm-b")))
  ;; a clip within another one is intersected with it
  (with s (html '(with "mode" "math"
                   (clipped (clipped (frac "a" "b") "0em" "0em" "1em" "1em")
                            "0em" "0em" "0.5em" "0.5em"))
                (static-on))
    (check-true (contains? s "<clipPath id=\"tm-c"))
    (check-true (contains? s "\" clip-path=\"url(#tm-c")))
  ;; a clip is a path in the formula
  (with s (html '(with "mode" "math"
                   (rotate "45" (clipped (frac "a" "b")
                                         "0em" "0em" "0.3em" "0.5em")))
                (static-on))
    (check-true (contains? s "<clipPath id=\"tm-c"))
    (check-true (contains? s "clip-path=\"url(#tm-c")))
  ;; a pattern is exported as a picture: no SVG here, without a target file
  (with s (html `(with "mode" "math"
                   (concat "x"
                     (graphics
                       (with "fill-color"
                             (pattern ,(string-append
                                        "$TEXMACS_PATH/misc/patterns/vintage/"
                                        "metal-brushed-medium.png")
                                      "*3/5" "100@" "#ff0000")
                         (cline (point "0" "0") (point "1" "0")
                                (point "1" "1"))))))
                (static-on))
    (check-false (contains? s "fill=\"currentColor\""))
    (check-true (contains? s "<mi>x</mi>")))
  ;; a picture cannot be captured: MathML without a target file
  (with s (html '(with "mode" "math"
                   (image "$TEXMACS_PATH/misc/images/fancy-c.png"
                          "" "" "" ""))
                (static-on))
    (check-false (contains? s "<svg "))))

;; Formulas are typeset in their environment: fonts, sizes, bold, preambles.
(define (test-html-static-environment)
  (check-group "html static formulas: environment")
  ;; bold reaches the names of the items of a description, not of other lists
  (let* ((name '(item* (concat "N " (with "mode" "math" "y"))))
         (d (html `(description (document ,name "body")) (static-on)))
         (i (html `(itemize (document ,name "body")) (static-on)))
         (n (html `(description (document (itemize (document ,name "body"))))
                  (static-on))))
    (check-true (contains? d "#tm-cmmib10-"))
    (check-false (contains? i "#tm-cmmib10-"))
    (check-false (contains? n "#tm-cmmib10-")))
  ;; a name of an item in another list than description is not bold
  (let* ((name '(item* "N"))
         (i (html `(itemize (document ,name "body")) (static-on)))
         (d (html `(description (document ,name "body")) (static-on))))
    (check-true (contains? i "<dt style=\"font-weight: normal\">"))
    (check-false (contains? d "font-weight: normal")))
  ;; formulas are typeset in their environment, as the expansion of the
  ;; document finds it: what the preambles assign, from where they are
  (let* ((f '(with "mode" "math" "x"))
         (g '(with "mode" "math" "y"))
         (one `(document (hide-preamble (assign "math-font" "Fira")) ,f))
         (two `(document (hide-preamble (assign "y" "1"))
                         (hide-preamble (assign "math-font" "Fira")) ,f))
         (cat `(document (hide-preamble (concat (assign "math-font" "Fira")
                                                (assign "zz" "1")))
                         ,f))
         (late `(document ,f (hide-preamble (assign "math-font" "Fira")) ,g))
         (s (lambda (d) (export (tmfile d) "html-document" (static-on)))))
    (check-true (contains? (s one) "#tm-FiraSans"))
    (check-true (contains? (s two) "#tm-FiraSans"))
    (check-true (contains? (s cat) "#tm-FiraSans"))
    (check-true (contains? (s late) "#tm-FiraSans"))
    (check-true (contains? (s late) "#tm-cmmi10-120"))
    (check-false (contains? (s one) "<body style")))
  ;; bold from a tag, and display style set around a formula, reach it
  (check-false
   (== (html '(with "mode" "math" "x") (static-on))
       (html '(strong (with "mode" "math" "x")) (static-on))))
  (let ((fm '(with "mode" "math" (concat (big "sum") (frac "a" "b"))))
        (svg-of (lambda (s)
                  (let* ((i (string-search-forwards "<svg " 0 s))
                         (e (string-search-forwards ">" i s)))
                    (substring s i e)))))
    (check-false
     (== (svg-of (html fm (static-on)))
         (svg-of (html `(with "math-display" "true" ,fm) (static-on))))))
  ;; a table in a displayed formula has the size of the formula (the buffer
  ;; route: tmhtml-env-patch) and bold reaches formulas in section titles
  (check-html-static-buffer)
  ;; a font-size of the preamble is the one of the text around too
  (let* ((width
          (lambda (pre)
            (let* ((f '(concat (with "mode" "math" "x")))
                   (doc (tmfile `(document ,@pre ,f)))
                   (s (export doc "html-document" (static-on)))
                   (m (string-search-forwards "<span class=\"tm-math" 0 s))
                   (i (string-search-forwards "width=\"" m s))
                   (e (string-search-forwards "\"" (+ i 7) s)))
              (substring s (+ i 7) e)))))
    (check= (width '((hide-preamble (document (assign "font-size" "1.5")))))
            (width '())))
  ;; sizes are relative: a formula takes the size of the text around it
  (with s (html '(with "font-size" "2" (with "mode" "math" "x")) (static-on))
    (check-true (contains? s "font-size: 200%"))
    (check= (count-of "font-size" s) 1))
  ;; a font around a formula is used by it, in a font of TeXmacs
  (with s (html '(with "math-font" "stix" (with "mode" "math" "x")) (static-on))
    (check-true (contains? s "STIX"))))

;; Displayed formulas: tables, numbers, labels and their widths.
(define (test-html-static-displays)
  (check-group "html static formulas: displays")
  ;; no max-width in percent, which collapses formulas in table cells
  (check-false (contains? (html '(with "mode" "math" "math-display" "true" "x")
                                (static-on))
                          "max-width"))
  ;; the parts of a displayed formula laid out on the page are cells of a
  ;; table, and its number is text
  (let* ((s (lambda (x)
              (html (tree->stree (html-expand (stree->tree `(document ,x))))
                    (static-on))))
         (row '(row (cell "a") (cell (concat "=b" (eq-number)))))
         (al (s `(align (document (tformat (table ,row))))))
         (tab (lambda (w)
                (s `(equation* (document
                                (tabular* (tformat (twith "table-width" ,w)
                                                   (cwith "1" "-1" "1" "1"
                                                          "cell-halign" "l")
                                                   (table
                                                    (row (cell "a=b")))))))))))
    (check-true (contains? al "<table width=\"100%\">"))
    (check-true (contains? al "(1)</class></td>"))
    (check= (count-of "<svg " al) 3) ;; a, =b, and the glyphs
    (check-true (contains? (tab "1par") "<table width=\"100%\">"))
    (check-true (contains? (tab "1par") "<td align=\"left\">"))
    (check-false (contains? (tab "20em") "<table width"))
    (with h (s '(equation* (document (concat "a" (htab "5mm") "b"))))
      (check= (count-of "<td width=\"50%\" align=\"center\">" h) 2)))
  ;; rows of their own format; a math tail after a number is no number
  (let* ((s (lambda (x)
              (html (tree->stree (html-expand (stree->tree `(document ,x))))
                    (static-on))))
         (ml (s '(multline (document (tformat (table (row (cell "a"))
                                                     (row (cell "b"))))))))
         (cell '(cell (concat "a" (eq-number) "=b+c")))
         (g (s `(gather (document (tformat (table (row ,cell))))))))
    (check-true (contains? ml "<td width=\"100%\" align=\"left\">"))
    (check-true (contains? ml "<td width=\"100%\" align=\"right\">"))
    (check-false (contains? g "</class>=b+c")))
  ;; a label of any form is the number: in another shape, or written by hand
  (let* ((s (lambda (x)
              (html (tree->stree (html-expand (stree->tree `(document ,x))))
                    (static-on))))
         (nr '(number (value "equation-nr") "roman"))
         (cell '(cell (concat "a=b" (eq-number))))
         (sc (s `(with "the-equation"
                       (macro (with "font-shape" "small-caps"
                                    (concat "[" ,nr "]")))
                   (gather (document (tformat (table (row ,cell))))))))
         (hand (s '(equation* (document (concat "a=b" (htab "5mm")
                                                (with "mode" "text" "(*)")))))))
    (check-true (contains? sc "<td align=\"right\">"))
    (check= (count-of "<svg " sc) 2)
    (check-true (contains? hand "(*)</td>"))
    (check= (count-of "<svg " hand) 2))
  ;; the source of a cell has no counters
  (let* ((row '(row (cell "a") (cell "=") (cell "b")))
         (doc `(document (leqnarray (document (tformat (table ,row))))))
         (s (html (tree->stree (html-expand (stree->tree doc))) (static-on))))
    (check-false (contains? s "equation-nr")))
  ;; other modes: the color of a style is for static formulas only
  (with x '(html-display
            (with "color" "red" "font-family" "rm"
                  (with "html-formula-env" "0"
                        "html-formula" "display" "math-display" "true"
                        (document "x"))))
    (with mj (html x (cons "texmacs->html:mathjax" "on"))
      (check-false (contains? mj "textrm"))
      (check-false (contains? mj "red"))))
  ;; the colored number of equation-lab: the tag keeps the formula apart
  (with ex (tree->stree
            (html-expand (stree->tree '(document (equation-lab "x" "1")))))
    (check-true (contains? (object->string ex) "html-equation-lab"))
    (with s (html ex (static-on))
      (check-true (contains? s "(1)"))))
  ;; phantoms take their space, through the expansion of a buffer
  (let* ((f '(concat (phantom "a+b+c+d+e") "=g"))
         (doc `(document (equation* (document ,f))))
         (ex (tree->stree (html-expand (stree->tree doc))))
         (s (html ex (static-on)))
         (i (string-search-forwards " width=\"" 0 s))
         (e (string-search-forwards "em\"" i s)))
    (check-true (> (string->number (substring s (+ i 8) e)) 3)))
  ;; explicit spaces at the end of a displayed formula are part of it
  (let* ((s (html '(with "mode" "math" "math-display" "true"
                    (concat "a" (space "6em")))
                  (static-on)))
         (i (string-search-forwards " width=\"" 0 s))
         (e (string-search-forwards "em\"" i s)))
    (check-true (> (string->number (substring s (+ i 8) e)) 6)))
  ;; also of those wider than a table as wide as the paragraph
  (let* ((s (html '(with "mode" "math" "math-display" "true"
                    (concat "a" (space "30em")))
                  (static-on)))
         (i (string-search-forwards " width=\"" 0 s))
         (e (string-search-forwards "em\"" i s)))
    (check-true (> (string->number (substring s (+ i 8) e)) 30)))
  ;; cutting goes with the display style which is drawn, however nested
  (let* ((long (long-sum 40))
         (boxes (lambda (x) (- (count-of "<svg " (html x (static-on))) 1))))
    (check= (boxes `(with "mode" "math" (with "math-display" "true" ,long))) 1)
    (check= (boxes `(with "mode" "math" "math-font" "roman"
                      "math-display" "true" ,long))
            1)
    (check-true
     (> (boxes `(with "math-display" "true"
                  (with "mode" "math" (with "math-display" "false" ,long))))
        1))))

;; The MathML and the source of a formula, its links, and their escapes.
(define (test-html-static-source)
  (check-group "html static formulas: source and links")
  ;; text in a formula is escaped: "<" would hide the rest of the page
  (with s (html '(with "mode" "math"
                   (concat "x" (with "mode" "text" "if a<less>b & c")))
                (static-on))
    (check-true (contains? s "&lt;"))
    (check-true (contains? s "&amp;"))
    (check-false (contains? s "a<b")))
  ;; the targets of links as for those of the text
  (with s (html '(with "mode" "math" (hlink "x" "other.tm")) (static-on))
    (check-true (contains? s "<a href=\"other.html\"")))
  ;; a target with the words which tell the glyphs from the boxes
  (check-true
   (string? (html '(concat (with "mode" "math"
                             (hlink "x" "a#position:absolute"))
                           ", then")
                  (static-on))))
  ;; the import takes a formula from its source, and nothing else
  (let* ((page (lambda (src)
                 (string-append "<p>a <span class=\"tm-math\" data-tm=\"" src
                                "\"><math><mi>m</mi></math></span> b</p>")))
         (in (lambda (src)
               (object->string
                (convert (page src) "html-snippet" "texmacs-stree")))))
    (check-true (contains? (in "(frac &quot;p&quot; &quot;q&quot;)")
                           "(frac \"p\" \"q\")"))
    (check-false (contains? (in (string-append
                                 "(with &quot;mode&quot; &quot;text&quot; "
                                 "(include &quot;/x&quot;))"))
                            "include"))
    (check-false (contains? (in (string-append (make-string 300 #\() "x"
                                               (make-string 300 #\))))
                            "\"x\"")))
  ;; the source of a formula, to read it back
  (with s (html '(with "mode" "math" (underline (frac "a" "b"))) (static-on))
    (check-true (contains? s "data-tm=\"(underline (frac &quot;a&quot;")))
  ;; a phantom in text in a formula shows no source in MathJax, which runs no
  ;; macros in \text; in the formula it stays
  (let ((mj (cons "texmacs->html:mathjax" "on")))
    (with s (html '(with "mode" "math"
                     (concat "x" (with "mode" "text"
                                   (concat "TA" (phantom "QQ") "TB"))))
                  mj)
      (check-false (contains? s "QQ"))
      (check-true (contains? s "\\text{TA TB}")))
    ;; a mode which is not the first variable, and math again within the text
    (with s (html '(with "mode" "math"
                     (concat "y" (with "color" "red" "mode" "text"
                                   (concat "E" (phantom "QQ") "F"))))
                  mj)
      (check-false (contains? s "QQ")))
    (with s (html '(with "mode" "math"
                     (text (concat "A" (with "color" "red" "mode" "math"
                                         (concat "u" (phantom "QQ") "v")))))
                  mj)
      (check-true (contains? s "\\phantom")))
    (with s (html '(with "mode" "math" (concat "x" (phantom "QQ") "y")) mj)
      (check-true (contains? s "\\phantom"))))
  ;; labels and scripts in text in a formula are no text of the MathML
  (with s (html '(with "mode" "math"
                   (concat "x" (with "mode" "text"
                                 (concat "ab" (label "t1")
                                         (action "go" "(noop)")))))
                (static-on))
    (check-false (contains? s "<mtext>t1</mtext>"))
    (check-false (contains? (string-drop s (string-search-forwards "<math" 0 s))
                            "(noop)"))
    (check-true (contains? s "<mtext>go</mtext>")))
  ;; without arguments, also as a needed part of a fraction
  (with s (html '(with "mode" "math" (frac (locus) "b")) (static-on))
    (check-true (contains? s "<mfrac><mrow></mrow><mi>b</mi></mfrac>")))
  ;; ids and targets of links in text in a formula are no text of the MathML
  (let* ((lc '(locus (id "l1") (link "hyperlink" (id "l1") (url "#x")) "bb"))
         (s (html `(with "mode" "math" (concat "x" (with "mode" "text" ,lc)))
                  (static-on)))
         (h (html '(with "mode" "math"
                     (concat "x" (with "mode" "text" (hlink "go" "http://a"))))
                  (static-on)))
         (r (html '(with "mode" "math"
                     (concat "x" (with "mode" "text" (reference "e1"))))
                  (static-on))))
    (check-true (contains? s "<mtext>bb</mtext>"))
    (check-false (contains? s "<mtext>hyperlink</mtext>"))
    (check-false (contains? s "<mtext>l1</mtext>"))
    (check-true (contains? h "<mtext>go</mtext>"))
    (check-false (contains? h "<mtext>http://a</mtext>")))
  ;; without arguments
  (for-each
   (lambda (x)
     (check-true
      (string? (html `(with "mode" "math" (concat "x" ,x))
                     (cons "texmacs->html:mathml" "on")))))
   '((locus) (hlink) (reference)))
  ;; the content of a locus stays in the MathML
  (with s (html '(with "mode" "math"
                   (concat "a" (locus (id "l1")
                                      (link "hyperlink" (id "l1") (url "#x"))
                                      "b")))
                (static-on))
    (check-true (contains? s "<mi>b</mi>")))
  ;; a link whose tree does not last: with a repository which kept it, the
  ;; process aborted
  (with s (html '(with "mode" "math" (rotate "45" (hlink "x" "http://a/b")))
                (static-on))
    (check-true (contains? s "<use ")))
  ;; attributes of the MathML are escaped
  (with s (html '(with "mode" "math" (with "color" "a&b" "x")) (static-on))
    (check-false (contains? s "mathcolor=\"a&b\"")))
  ;; symbols without a character in the MathML do not open tags
  (with s (html '(with "mode" "math" (concat (left "<lgroup>") "x" (right ".")))
                (static-on))
    (check-false (contains? s "<lgroup>"))
    (check-true (contains? s "<mi>x</mi>")))
  (with s (html '(with "mode" "math" (concat (big "<less>") "x")) (static-on))
    (check-false (contains? s "<mo><"))
    (check-false (contains? s "<mo><less>")))
  ;; links in formulas, which come with the escapes of an attribute
  (with s (html '(with "mode" "math" (hlink "x" "https://a.org/?b=1&c=2"))
                (static-on))
    (check-true (contains? s "<a href=\"https://a.org/?b=1&amp;c=2\""))))

(define (long-sum n)
  (cons 'concat
        (append-map (lambda (i)
                      (list "a" (list 'rsub (number->string i)) "+"))
                    (.. 1 n))))

;; A long inline formula is cut where lines may be broken, a display is not.
(define (test-html-static-breaks)
  (check-group "html static formulas: line breaks")
  (let* ((inline (html `(with "mode" "math" ,(long-sum 40)) (static-on)))
         (short  (html '(with "mode" "math" (concat "a" "+" "b")) (static-on)))
         (disp   (html `(with "mode" "math" "math-display" "true"
                          ,(long-sum 40))
                       (static-on))))
    (check-true (> (count-of "<svg " inline) 2))
    (check= (count-of "<math" inline) 1)
    (check= (count-of "<svg " short) 2) ;; the formula and its glyphs
    (check= (count-of "<svg " disp) 2)))

;; No line break before the punctuation after a formula, on import it stays.
;; SVG is written as it is, and the spaces around a formula remain.
(define (test-html-static-writer)
  (check-group "html static formulas: writer")
  ;; not through squash, which removes the spaces next to tags
  (with s (export '(concat "before " (with "mode" "math" "x") " after")
                  "html-snippet" (static-on))
    (check-true (contains? s "before <span class=\"tm-math\""))
    ;; (the writer may break the line there)
    (check-true (or (contains? s "</span> after")
                    (contains? s "</span>\nafter"))))
  (let* ((many (cons 'concat
                     (append-map
                      (lambda (i)
                        `("word " (with "mode" "math" (frac "a" "b")) " "))
                      (.. 0 30))))
         (doc (tmfile `(document ,many)))
         (s (export doc "html-document" (static-on))))
    (check= (count-of (string-append
                       "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                       "aria-hidden=\"true\" fill=\"currentColor\" "
                       "stroke=\"currentColor\" stroke-width=\"8\" width=")
                      s)
            30)
    (check-false (contains? s "<use\n"))
    (check-false (contains? s "\n      href=\"#tm-"))))

(define (test-html-static-punctuation)
  (check-group "html static formulas: punctuation")
  (with s (html '(concat (with "mode" "math" "x") ", then y") (static-on))
    (check-true (contains? s "white-space:nowrap"))
    ;; (a formula of one box goes with it as a whole, with its text)
    (check-true (contains? s "</math></span>,</span>"))
    (check-true (contains? s "then y"))
    (check-true (contains? s "<mi>x</mi>")))
  (check-false
   (contains? (html '(concat (with "mode" "math" "x") " then") (static-on))
              "style=\"white-space:nowrap\""))
  (let* ((doc (tmfile `(document (concat (with "mode" "math" "x") ", then y"))))
         (s (export doc "html-document" (static-on)))
         (back (object->string (import s "html-document"))))
    (check-true (contains? back "(math \"x\")"))
    (check-true (contains? back ", then y"))))

;; In a document: every glyph once, in one place, and defined.
(define (test-html-static-document)
  (check-group "html static formulas: documents")
  (let* ((math-of (lambda (x) `(with "mode" "math" ,x)))
         (doc (tmfile
               `(document
                 (hide-preamble
                  (document
                   (assign "foo" (macro "x" (frac (arg "x") "z")))))
                 (concat ,(math-of '(concat "x" "+" "y")) " and "
                         ,(math-of '(concat "x" "+" "y")) " and "
                         ,(math-of '(concat "x" "+" "q")))
                 (concat ,(math-of '(foo "w")) " end")
                 (concat ,(math-of '(frac "a" "b")) " and "
                         ,(math-of '(concat (label "eq") "x"))))))
         (s (export doc "html-document" (static-on))))
    (check-true (contains? s "<!DOCTYPE html"))
    (check= (count-of "position:absolute;width:0" s) 1)
    (check= (count-of "id=\"tm-" s)
            (length (list-remove-duplicates
                     (let loop ((pos 0) (l '()))
                       (with i (string-search-forwards "id=\"tm-" pos s)
                         (if (< i 0) l
                             (let ((e (string-search-forwards "\"" (+ i 4) s)))
                               (loop e (cons (substring s i e) l)))))))))
    ;; all that is used is defined
    (let loop ((pos 0))
      (with i (string-search-forwards "href=\"#tm-" pos s)
        (when (>= i 0)
          (let* ((e (string-search-forwards "\"" (+ i 7) s))
                 (id (substring s (+ i 7) e)))
            (check-true (contains? s (string-append "id=\"" id "\"")))
            (loop e)))))
    ;; the macros of the preamble of the document, not of the current buffer:
    ;; the w and the z of the macro are drawn (the codes of the letters)
    (check-true (contains? s "-119\""))
    (check-true (contains? s "-122\""))
    (check-true (contains? s "id=\"eq\""))
    (check-false (contains? s "<script"))
    (check-false
     (contains? (export doc "html-document" (static-on)
                        (cons "texmacs->html:mathjax" "on"))
                "<script"))
    ;; back: the formulas, and nothing of the drawings
    (with back (object->string (import s "html-document"))
      (check-false (contains? back "tm-"))
      (check-true (contains? back "(math "))
      (check-true (contains? back "(frac \"a\" \"b\")")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The numbers of the environments of formulas (see exec_with)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (formula-numbers x)
  ;; the numbers of the environments of the formulas of the expansion x
  (cond ((npair? x) '())
        ((and (func? x 'with) (>= (length x) 4)
              (== (cadr x) "html-formula-env"))
         (cons (caddr x) (append-map formula-numbers (cdr x))))
        (else (append-map formula-numbers (cdr x)))))

(define (html-expand* doc)
  (tree->stree (html-expand (stree->tree doc))))

(define (test-html-static-numbers)
  (check-group "html static formulas: the numbers of the environments")
  ;; a formula has the number of the one before when the environment is the
  ;; same (equal formulas elsewhere share their images); a new expansion
  ;; gives new numbers, so that the old ones find nothing
  (let* ((doc '(document (concat (math "a") " " (with "color" "red" (math "b"))
                                 " " (math "c"))))
         (one (formula-numbers (html-expand* doc)))
         (two (formula-numbers (html-expand* doc))))
    (check= (length one) 3)
    (check-false (== (car one) (cadr one)))
    (check= (length two) 3)
    (check-false (in? (car two) one))
    (check-false (in? (cadr two) one)))
  (with nrs (formula-numbers (html-expand* '(document (math "a") (math "b"))))
    (check= (car nrs) (cadr nrs)))
  ;; formulas are the same for the caches when what their typesetting reads
  ;; is the same: not the number of an equation between them, but a colour
  (let* ((doc '(document (math "x+y") (equation (document "a=b"))
                         (math "x+y") (with "color" "red" (math "x+y"))))
         (nrs (formula-numbers (html-expand* doc)))
         (f (stree->tree '(with "mode" "math" "x+y")))
         (first (begin (html-math-snippet f (string->number (car nrs)) #t)
                       (html-snippet-last))))
    (check= (length nrs) 4)
    (check-true (>= first 0))
    (check-false (== (car nrs) (caddr nrs)))
    (check= (html-snippet-id f (string->number (caddr nrs))) first)
    (check= (html-snippet-id f (string->number (cadddr nrs))) -1))
  ;; a picture is not typeset in the environment of a formula inside it
  (let* ((env-of (eval 'tmhtml-formula-env-of
                       (resolve-module '(convert html tmhtml))))
         (doc '(document (graphics (text-at (math "A") (point "0" "0"))
                                   (text-at "B" (point "1" "0")))))
         (pic (cadr (html-expand* doc)))
         (f (html-expand* '(document (math "x")))))
    (check-false (env-of pic))
    (check-false (env-of `(with "color" "red" ,pic)))
    ;; but a formula is, also under the with of a display (tmhtml-equation*)
    (check-true (number? (env-of f)))
    (check-true (number? (env-of `(with "mode" "math"
                                        (with "math-display" "true"
                                              ,(cadr f)))))))
  ;; the title keeps its formulas, as text
  (let* ((doc '(document (doc-data (doc-title (concat "On " (math "x^2"))))
                         "text"))
         (file (tmfile (html-expand* doc)))
         (title (lambda opts
                  (with s (apply export (cons* file "html-document" opts))
                    (contains? s "<title>On x^2</title>")))))
    (check-true (title (static-on)))
    (check-true (title '("texmacs->html:static-math" . "off")
                       '("texmacs->html:images" . "on")))
    (check-true (title '("texmacs->html:static-math" . "off")
                       '("texmacs->html:mathml" . "on")))))

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
  ;; Latin-1 has no alpha, which becomes a ? (#90; it was left out)
  (check= (export "<alpha><less><gtr>\xe9" "verbatim-snippet"
                  (cons "texmacs->verbatim:encoding" "iso-8859-1"))
          "?<>\xe9")
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
  (test-tm-string-tag)
  (test-tm-raw-data)
  (test-stm)
  (test-tmml-serialize)
  (test-tmml-parse)
  (test-tmml-round-trip)
  (test-html-export)
  (test-html-options)
  (test-html-static)
  (test-html-static-colors)
  (test-html-static-glyphs)
  (test-html-static-environment)
  (test-html-static-displays)
  (test-html-static-source)
  (test-html-static-breaks)
  (test-html-static-writer)
  (test-html-static-punctuation)
  (test-html-static-document)
  (test-html-static-numbers)
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
