
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdown-test.scm
;; DESCRIPTION : tests of the Markdown converters
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tests cover the four steps of convert/markdown, each through the
;; converters which init-markdown.scm declares:
;;
;;   - markdownin.scm: the parser, from a text to a Markdown tree;
;;   - markdownout.scm: the serializer, from a Markdown tree to a text;
;;   - markdowntm.scm: the import, from a Markdown tree to a TeXmacs tree;
;;   - tmmarkdown.scm: the export, from a TeXmacs tree to a Markdown tree;
;;
;; and the round trips in both directions. The trees are not expanded here:
;; the export of a buffer, which runs the macros of its style first
;; (tmmarkdown-expand.scm), is left to the tests with an editor.

(texmacs-module (check markdown-test)
  (:use (check check-lib)))

(define (parse s) (convert s "markdown-snippet" "markdown-stree"))
(define (write-md t) (convert t "markdown-stree" "markdown-snippet"))
(define (import s) (convert s "markdown-snippet" "texmacs-stree"))
(define (export t . opts)
  (apply convert (cons* t "texmacs-stree" "markdown-snippet" opts)))
(define (bytes . l) (list->string (map integer->char l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A text of one paragraph is the list of its nodes, without the paragraph.
(define (test-parse-inline)
  (check-group "parse inline")
  (check= (parse "") '(markdown))
  (check= (parse "plain") '(markdown "plain"))
  (check= (parse "*a* **b** ***c*** _d_ __e__ ~~f~~")
          '(markdown (em "a") " " (strong "b") " " (em (strong "c")) " "
                     (em "d") " " (strong "e") " " (del "f")))
  ;; inside a word, * is emphasis and _ is not; between spaces, neither
  (check= (parse "a*b*c a_b_c 2 * 3")
          '(markdown "a" (em "b") "c a_b_c 2 * 3"))
  (check= (parse "`x` ``a`b`` \\*lit\\*")
          '(markdown (code "x") " " (code "a`b") " *lit*"))
  (check= (parse "[t](u \"T\") <http://a.b> https://c.d/e. ![alt](i.png)")
          '(markdown (a (@ (href "u") (title "T")) "t") " "
                     (a (@ (href "http://a.b")) "http://a.b") " "
                     (a (@ (href "https://c.d/e")) "https://c.d/e") ". "
                     (img (@ (src "i.png") (alt "alt")) "alt")))
  (check= (parse "[a][r] [r]\n\n[r]: http://x.y")
          '(markdown (a (@ (href "http://x.y")) "a") " "
                     (a (@ (href "http://x.y")) "r")))
  (check= (parse "line one  \nline two\\\nthree")
          '(markdown "line one" (br) "line two" (br) "three"))
  (check= (parse "a<br>b &amp; &lt; &#65;")
          '(markdown "a" (html "<br>") "b & < A"))
  (check= (parse "1986\\. A year") '(markdown "1986. A year")))

;; The formulas are kept as LaTeX. A dollar opens a formula as for Pandoc:
;; not before a space, and the one which closes is not after a space nor
;; before a digit. \[2\] are the brackets which other programs escape.
(define (test-parse-math)
  (check-group "parse math")
  (check= (parse "$x^2$ and $5 and $6")
          '(markdown (math "x^2") " and $5 and $6"))
  (check= (parse "\\$5") '(markdown "$5"))
  (check= (parse "\\(a\\) \\[2\\] \\[x=1\\]")
          '(markdown (math "a") " [2] " (displaymath "x=1")))
  (check= (parse "$$\nx=1\n$$") '(markdown (displaymath "x=1")))
  (check= (parse "\\[\nx=1\n\\]") '(markdown (displaymath "x=1"))))

(define (test-parse-blocks)
  (check-group "parse blocks")
  (check= (parse "# H1\n\nText\nmore\n\nH2\n--\n")
          '(markdown (h1 "H1") (p "Text more") (h2 "H2")))
  (check= (parse "#no heading\n\n####### seven")
          '(markdown (p "#no heading") (p "####### seven")))
  (check= (parse "---\n\n***\n") '(markdown (hr) (hr)))
  (check= (parse "* * *") '(markdown (hr)))
  (check= (parse "a\n***\nb") '(markdown (p "a") (hr) (p "b")))
  (check= (parse "> q\nlazy\n\n> - l")
          '(markdown (blockquote (p "q lazy"))
                     (blockquote (ul (li (p "l"))))))
  (check= (parse "```python\nprint(1)\n```")
          '(markdown (pre (@ (lang "python")) "print(1)")))
  (check= (parse "    code\n    more") '(markdown (pre "code\nmore")))
  (check= (parse "<div>\nhtml\n</div>")
          '(markdown (html "<div>\nhtml\n</div>")))
  (check= (parse "x[^n] y\n\n[^n]: note")
          '(markdown (p "x" (footnote "n") " y")
                     (footnote-def "n" (p "note")))))

(define (test-parse-lists)
  (check-group "parse lists")
  (check= (parse "- a\n- b\n  - c\n\n1. x\n2. y")
          '(markdown (ul (li (p "a")) (li (p "b") (ul (li (p "c")))))
                     (ol (li (p "x")) (li (p "y")))))
  (check= (parse "+ plus list") '(markdown (ul (li (p "plus list")))))
  (check= (parse "3. a\n4. b")
          '(markdown (ol (@ (start "3")) (li (p "a")) (li (p "b")))))
  (check= (parse "- [ ] todo\n- [x] done")
          '(markdown (ul (li (@ (checked "false")) (p "todo"))
                         (li (@ (checked "true")) (p "done")))))
  ;; an empty line between the items makes the list loose
  (check= (parse "- a\n\n- b")
          '(markdown (ul (@ (loose "true")) (li (p "a")) (li (p "b"))))))

(define (test-parse-tables)
  (check-group "parse tables")
  (check= (parse "| a | b |\n|:-:|--:|\n| 1 | 2 |")
          '(markdown (table (tr (th (@ (align "center")) "a")
                                (th (@ (align "right")) "b"))
                            (tr (td (@ (align "center")) "1")
                                (td (@ (align "right")) "2")))))
  (check= (parse "a | b\n--|--\n1 | 2")
          '(markdown (table (tr (th "a") (th "b")) (tr (td "1") (td "2"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The serializer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A paragraph is one line, the blocks are separated by an empty line.
(define (test-serialize)
  (check-group "serialize")
  (check= (write-md '(markdown (p "a" (br) "b"))) "a\\\nb\n")
  (check= (write-md '(markdown (h3 "T") (hr))) "### T\n\n***\n")
  (check= (write-md '(markdown (ol (@ (start "3")) (li (p "x")) (li (p "y")))))
          "3. x\n4. y\n")
  (check= (write-md '(markdown (ul (@ (loose "true")) (li (p "a")) (li (p "b")))))
          "- a\n\n- b\n")
  (check= (write-md '(markdown (pre (@ (lang "c")) "int x;")))
          "```c\nint x;\n```\n")
  (check= (write-md '(markdown (blockquote (p "q") (ul (li (p "i"))))))
          "> q\n>\n> - i\n")
  (check= (write-md '(markdown (p "a") (displaymath "x"))) "a\n\n$$\nx\n$$\n")
  (check= (write-md '(markdown (meta (title "T") (author "A")) (p "x")))
          "---\ntitle: T\nauthor: A\n---\n\nx\n")
  ;; the code is between more backquotes than it holds
  (check= (write-md '(markdown (code "a`b"))) "``a`b``")
  (check= (write-md '(markdown (pre "```\nx"))) "````\n```\nx\n````\n"))

;; What would be read as markup is escaped, and nothing else.
(define (test-serialize-escapes)
  (check-group "serialize escapes")
  (check= (write-md '(markdown "a*b _c_ [d] <e> #f"))
          "a\\*b \\_c\\_ [d] \\<e> #f")
  (check= (write-md '(markdown "snake_case 2 * 3 a < b & c"))
          "snake_case 2 * 3 a < b & c")
  (check= (write-md '(markdown "a $5 &amp; `q`")) "a \\$5 \\&amp; \\`q\\`")
  (check= (write-md '(markdown "[x](y) [k]: v [^1]"))
          "\\[x\\](y) [k\\]: v \\[^1]")
  (check= (write-md '(markdown (p "# not heading") (p "1. not list")
                               (p "- not item") (p "> not quote")))
          "\\# not heading\n\n1\\. not list\n\n\\- not item\n\n\\> not quote\n")
  (check= (write-md '(markdown (ul (li (p "[ ] not a task")))))
          "- \\[ ] not a task\n")
  (check= (write-md '(markdown (table (tr (th "a|b") (th (math "|x|"))))))
          "| a\\|b | $\\vert x\\vert $ |\n| ---- | --------------- |\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-import-inline)
  (check-group "import inline")
  (check= (import "plain") "plain")
  (check= (import "*a* **b** ~~c~~ `d`")
          '(concat (em "a") " " (strong "b") " " (strike-through "c") " "
                   (verbatim "d")))
  (check= (import "**a *b* c**") '(strong (concat "a " (em "b") " c")))
  (check= (import "[t](u)") '(hlink "t" "u"))
  (check= (import "<https://x.y>") '(hlink "https://x.y" "https://x.y"))
  (check= (import "![](i.png)") '(image "i.png" "" "" "" ""))
  ;; a link with a title, in Markdown or in HTML
  (check= (import "[t](u \"The title\")") '(hlink* "t" "u" "The title"))
  (check= (import "<a href=\"u\" title=\"T\">t</a>") '(hlink* "t" "u" "T"))
  ;; an image in a text: its description is dropped, its sizes are kept
  ;; (the attributes of Pandoc, or the tag of HTML)
  (check= (import "x ![alt](i.png) y")
          '(concat "x " (image "i.png" "" "" "" "") " y"))
  (check= (import "x ![](i.png){width=300px height=2cm}")
          '(concat "x " (image "i.png" "300px" "2cm" "" "")))
  (check= (import "## <img src=\"i.png\" alt=\"The logo\" width=\"36\"> T")
          '(subsection (concat (image "i.png" "36px" "" "" "") " T")))
  (check= (import "a  \nb") '(concat "a" (next-line) "b"))
  (check= (import "x[^n]\n\n[^n]: note") '(concat "x" (footnote "note")))
  ;; the formulas go through the LaTeX converter, HTML through the HTML one
  (check= (import "$x^2$") '(math (concat "x" (rsup "2"))))
  (check= (import "$$\nx=1\n$$") '(equation* (document "x=1")))
  (check= (import "<b>bold</b> x<sub>2</sub>")
          '(concat (with "font-series" "bold" "bold") " x" (rsub "2")))
  ;; the text is UTF-8, the tree is in the encoding of TeXmacs
  (check= (import (string-append "caf" (bytes 195 169) " \\<y\\> &copy;"))
          (string-append "caf" (bytes 233) " <less>y<gtr> <copyright>")))

;; An image alone in its paragraph, with a description or a title, is a
;; figure with this caption, as for Pandoc.
(define (test-import-figures)
  (check-group "import figures")
  (check= (import "![alt text](i.png)")
          '(big-figure (image "i.png" "" "" "" "") "alt text"))
  (check= (import "![a](i.png \"The title\")")
          '(big-figure (image "i.png" "" "" "" "") "The title"))
  (check= (import "![a *b* $x^2$](i.png){width=50%}")
          '(big-figure (image "i.png" "0.5par" "" "" "")
                       (concat "a " (em "b") " "
                               (math (concat "x" (rsup "2"))))))
  (check= (import "<img src=\"i.png\" alt=\"The logo\" width=\"36\">")
          '(big-figure (image "i.png" "36px" "" "" "") "The logo"))
  (check= (import "para\n\n![cap](i.png)\n\nmore")
          '(document "para" (big-figure (image "i.png" "" "" "" "") "cap")
                     "more"))
  (check= (import "![](i.png)") '(image "i.png" "" "" "" "")))

;; The first level of headings of a text is the sections.
(define (test-import-blocks)
  (check-group "import blocks")
  (check= (import "a\n\nb") '(document "a" "b"))
  (check= (import "# A\n\n## B\n\ntext")
          '(document (section "A") (subsection "B") "text"))
  (check= (import "Setext\n===\n\npara") '(document (section "Setext") "para"))
  (check= (import "- a\n- b")
          '(itemize (document (concat (item) "a") (concat (item) "b"))))
  (check= (import "1. a\n2. b")
          '(enumerate (document (concat (item) "a") (concat (item) "b"))))
  (check= (import "- a\n\n  b\n- c")
          '(itemize (document (concat (item) "a") "b" (concat (item) "c"))))
  (check= (import "- [ ] a\n- [x] b")
          '(itemize (document (concat (item* (math "<Box>")) "a")
                              (concat (item* (math "<boxtimes>")) "b"))))
  (check= (import "> q") '(quotation (document "q")))
  (check= (import "```python\nprint(1)\n```")
          '(python-code (document "print(1)")))
  (check= (import "```\nx\ny\n```") '(verbatim-code (document "x" "y")))
  (check= (import "---") '(hrule))
  (check= (import "| a | b |\n|:-:|--:|\n| 1 | 2 |")
          '(block (tformat (cwith "1" "-1" "1" "1" "cell-halign" "c")
                           (cwith "1" "-1" "2" "2" "cell-halign" "r")
                           (table (row (cell (strong "a")) (cell (strong "b")))
                                  (row (cell "1") (cell "2")))))))

;; A document: a YAML header or a single heading of the first level is the
;; title, and the other headings move up.
(define (test-import-document)
  (check-group "import document")
  (let ((doc (lambda (s) (convert s "markdown-document" "texmacs-stree"))))
    (check= (doc "# T\n\n## S\n\nx\n")
            '(document (body (document (doc-data (doc-title "T"))
                                       (section "S") "x"))
                       (style "generic")))
    (check= (doc "---\ntitle: T\nauthor: A\n---\n\n# S\n\nx\n")
            '(document
               (body (document
                       (doc-data (doc-title "T")
                                 (doc-author (author-data (author-name "A"))))
                       (section "S") "x"))
               (style "generic")))
    ;; a list of authors on one line or on several, a text on its own lines
    (check= (doc "---\ntitle: \"T: U\"\nauthor: [A, B]\n---\n")
            '(document
               (body (document
                       (doc-data (doc-title "T: U")
                                 (doc-author (author-data (author-name "A")))
                                 (doc-author (author-data (author-name "B"))))))
               (style "generic")))
    (check= (doc "---\ntitle: T\nsubtitle: S\nauthor:\n  - A\nabstract: |\n  Some\n  text\n---\n\nx\n")
            '(document
               (body (document
                       (doc-data (doc-title "T") (doc-subtitle "S")
                                 (doc-author (author-data (author-name "A"))))
                       (abstract-data (abstract (document "Some text")))
                       "x"))
               (style "generic")))
    ;; two headings of the first level are two sections
    (check= (doc "# A\n\n# B\n")
            '(document (body (document (section "A") (section "B")))
                       (style "generic")))
    (check= (doc "") '(document (body (document "")) (style "generic")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-export-inline)
  (check-group "export inline")
  (check= (export "plain") "plain")
  (check= (export '(concat "x " (em "a") " y")) "x *a* y")
  (check= (export '(with "font-shape" "italic" "i")) "*i*")
  (check= (export '(with "font-series" "bold" "b")) "**b**")
  (check= (export '(em (concat "a" (strong "b")))) "*a**b***")
  ;; the spaces move out of the emphasis, two emphases in a row are one
  (check= (export '(concat "x" (em " a ") "y")) "x *a* y")
  (check= (export '(concat (em "a") (em "b"))) "*ab*")
  (check= (export '(em "")) "")
  (check= (export '(verbatim "v")) "`v`")
  (check= (export '(strike-through "s")) "~~s~~")
  (check= (export '(hlink "t" "u")) "[t](u)")
  (check= (export '(hlink "a b" "u v(w)")) "[a b](<u v(w)>)")
  (check= (export '(hlink* "t" "u" "The \"title\""))
          "[t](u \"The \\\"title\\\"\")")
  (check= (export '(hlink* "t" "u" "")) "[t](u)")
  (check= (convert '(hlink* "t" "u" "T") "texmacs-stree" "html-snippet")
          "<a href=\"u\" title=\"T\">t</a>")
  (check= (export '(href "http://a.b")) "<http://a.b>")
  (check= (export '(image "i.png" "" "" "" "")) "![](i.png)")
  (check= (export '(concat "a" (next-line) "b")) "a\\\nb")
  (check= (export '(concat "a" (footnote "note") "b")) "a[^1]b\n\n[^1]: note\n")
  (check= (export '(with "color" "red" "r")) "r")
  (check= (export '(concat (label "l") "see " (reference "l"))) "see l")
  ;; ... and ` are not the ellipsis and the quote of the conversion
  (check= (export "wait...") "wait...")
  (check= (export '(verbatim "a`b...")) "``a`b...``")
  (check= (export (string-append "caf" (bytes 233)))
          (string-append "caf" (bytes 195 169))))

;; Markdown has neither underlining nor scripts: an underlined text is
;; emphasized, a script of digits and signs is written with the characters
;; of Unicode, another one with the tag of HTML (or as plain text without
;; the option); a key is code.
(define (test-export-html)
  (check-group "export html")
  (let ((off (cons "texmacs->markdown:html" "off")))
    (check= (export '(underline "u")) "*u*")
    (check= (export '(concat "H" (rsub "2") "O")) (cork->utf8 "H<#2082>O"))
    (check= (export '(concat "x" (rsup "2") " y" (rsup "-(1+2)")))
            (cork->utf8 "x<#00B2> y<#207B><#207D><#00B9><#207A><#00B2><#207E>"))
    (check= (export '(concat "x" (rsup "n+1"))) "x<sup>n+1</sup>")
    (check= (export '(concat "x" (rsub "max"))) "x<sub>max</sub>")
    (check= (export '(concat "x" (rsup (em "a")))) "x<sup>*a*</sup>")
    (check= (export '(concat "x" (rsub "max")) off) "xmax")
    (check= (export '(render-key "F9")) "`F9`")
    (check= (export '(marked "m")) "<mark>m</mark>")
    (check= (export '(marked "m") off) "m")))

;; A figure which is a single image is this image, with the caption as its
;; description; the other figures are followed by their caption.
(define (test-export-figures)
  (check-group "export figures")
  (check= (export '(big-figure (image "i.png" "" "" "" "") "The caption"))
          "![The caption](i.png)\n")
  (check= (export '(small-figure (image "i.png" "" "" "" "")
                                 (concat "A " (em "nice") " one")))
          "![A *nice* one](i.png)\n")
  (check= (export '(render-big-figure "figure" "Figure 1"
                                      (image "i.png" "" "" "" "") "cap"))
          "![cap](i.png)\n")
  (check= (export '(big-figure (image "i.png" "" "" "" "")
                               (document (concat (label "l") "cap"))))
          "![cap](i.png)\n")
  (check= (export '(big-figure (image "i.png" "" "" "" "") "")) "![](i.png)\n")
  (check= (export '(big-figure (image "i.png" "" "" "" "") "see [1]"))
          "![see [1]](i.png)\n")
  (check= (export '(big-figure (image "i.png" "" "" "" "") "a ] b"))
          "![a \\] b](i.png)\n")
  (check= (export '(big-figure (concat (image "a.png" "" "" "" "")
                                       (image "b.png" "" "" "" ""))
                               "two"))
          "![](a.png)![](b.png)\n\n**Figure.** two\n"))

;; An image with a size is the tag of HTML: pixels, or percents for a part
;; of the paragraph; the lengths which depend on the image are dropped.
(define (test-export-image-sizes)
  (check-group "export image sizes")
  (check= (export '(image "i.png" "36px" "" "" ""))
          "<img src=\"i.png\" alt=\"\" width=\"36\">")
  (check= (export '(image "i.png" "0.5par" "2cm" "" ""))
          "<img src=\"i.png\" alt=\"\" width=\"50%\" height=\"76\">")
  (check= (export '(big-figure (image "i.png" "36px" "" "" "")
                               (concat "a \"b\" " (em "c"))))
          "<img src=\"i.png\" alt=\"a &quot;b&quot; c\" width=\"36\">\n")
  (check= (export '(image "i.png" "0.6383w" "" "" "")) "![](i.png)")
  (check= (export '(big-figure (image "i.png" "36px" "" "" "") "alt")
                  (cons "texmacs->markdown:html" "off"))
          "![alt](i.png)\n"))

(define (test-export-math)
  (check-group "export math")
  (check= (export '(math (concat "x" (rsup "2")))) "$x^2$")
  (check= (export '(document "a" (equation* (document "x=1")) "b"))
          "a\n\n$$\nx = 1\n$$\n\nb\n")
  ;; a formula of one symbol is the character
  (check= (export '(concat "a " (math "<rightarrow>") " b"))
          (string-append "a " (bytes 226 134 146) " b")))

(define (test-export-blocks)
  (check-group "export blocks")
  (check= (export '(document "a" "b")) "a\n\nb\n")
  (check= (export '(document "" "a" "" "" "b" "")) "a\n\nb\n")
  (check= (export '(itemize (document (concat (item) "a") (concat (item) "b"))))
          "- a\n- b\n")
  (check= (export '(enumerate (document (concat (item) "a")
                                        (concat (item) "b"))))
          "1. a\n2. b\n")
  (check= (export '(itemize (document (concat (item) "a")
                                      (itemize (document (concat (item) "n"))))))
          "- a\n  - n\n")
  (check= (export '(enumerate (document (concat (item) "a") "second paragraph"
                                        (concat (item) "b"))))
          "1. a\n\n   second paragraph\n\n2. b\n")
  (check= (export '(itemize (document (concat (item* (math "<Box>")) "a")
                                      (concat (item* (math "<boxtimes>")) "b"))))
          "- [ ] a\n- [x] b\n")
  (check= (export '(description (document (concat (item* "k") "v"))))
          "- **k** v\n")
  (check= (export '(quotation (document "q"))) "> q\n")
  (check= (export '(verbatim-code (document "x" "y"))) "```\nx\ny\n```\n")
  (check= (export '(python-code (document "print(1)")))
          "```python\nprint(1)\n```\n")
  (check= (export '(hrule)) "***\n")
  (check= (export '(theorem (document "T"))) "**Theorem.** T\n")
  (check= (export '(proof (document "P"))) "**Proof.** P\n")
  (check= (export '(big-table (tabular (tformat (table (row (cell "a")))))
                              "cap"))
          "| a   |\n| --- |\n\n**Table.** cap\n"))

;; The first level of headings which is used is #, or ## under a title; the
;; levels below keep their distances.
(define (test-export-headings)
  (check-group "export headings")
  (check= (export '(document (section "S") "text" (subsection "T") "more"))
          "# S\n\ntext\n\n## T\n\nmore\n")
  (check= (export '(document (subsection "T") (subsubsection "U")))
          "# T\n\n## U\n")
  (check= (export '(document (chapter "C") (section "S") (paragraph "P")))
          "# C\n\n## S\n\n##### P\n")
  (check= (export '(document (section* "S") (subsection* "T"))) "# S\n\n## T\n"))

(define (test-export-tables)
  (check-group "export tables")
  (check= (export '(tabular (tformat (table (row (cell "a") (cell "b"))
                                            (row (cell "1") (cell "2"))))))
          "| a   | b   |\n| --- | --- |\n| 1   | 2   |\n")
  (check= (export '(block (tformat (cwith "1" "-1" "2" "2" "cell-halign" "r")
                                   (table (row (cell "a") (cell "b"))
                                          (row (cell "1") (cell "2"))))))
          "| a   |   b |\n| --- | --: |\n| 1   |   2 |\n"))

;; A document: a title alone is a heading, a title with authors or a date
;; is a YAML header, unless the option says otherwise.
(define (test-export-document)
  (check-group "export document")
  (let* ((file (lambda (body)
                 `(document (TeXmacs ,(texmacs-version)) (style "generic")
                            (body ,body))))
         (doc (lambda (body . opts)
                (apply convert (cons* (file body) "texmacs-stree"
                                      "markdown-document" opts))))
         (title '(doc-data (doc-title "T")))
         (full '(doc-data (doc-title "T")
                          (doc-author (author-data (author-name "A")))
                          (doc-date "2026"))))
    (check= (doc `(document ,title (section "S") "x")) "# T\n\n## S\n\nx\n")
    (check= (doc `(document ,full (section "S") "x"))
            "---\ntitle: T\nauthor: A\ndate: 2026\n---\n\n# S\n\nx\n")
    (check= (doc `(document ,full (section "S") "x")
                 (cons "texmacs->markdown:front-matter" "off"))
            "# T\n\nA\n\n2026\n\n## S\n\nx\n")
    (check= (doc '(document "")) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Round trips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These texts come back as they are from TeXmacs...
(define md-same
  '("plain"
    "x *a* **b** ~~c~~ `d`"
    "[t](u) and <https://x.y>"
    "[t](u \"The title\")"
    "a\n\n![cap](i.png)\n\nb\n"
    "x <img src=\"i.png\" alt=\"\" width=\"50%\" height=\"20\"> y"
    "a\\\nb"
    "$x^2$ and \\$5"
    "# A\n\ntext\n\n## B\n\nmore\n"
    "- a\n- b\n"
    "1. a\n2. b\n"
    "- a\n  - n\n- b\n"
    "- [ ] a\n- [x] b\n"
    "> q\n"
    "```python\nprint(1)\n```\n"
    "***\n"
    "| a   |   b |\n| --- | --: |\n| 1   |   2 |\n"
    "a\n\n$$\nx = 1\n$$\n\nb\n"
    "x[^1]\n\n[^1]: note\n"
    "2 * 3, snake_case, \\*not em\\*, No name [2]"))

;; ... and these trees from Markdown.
(define tm-same
  '("plain"
    (concat "x " (em "a") " " (strong "b") " " (verbatim "c"))
    (hlink "t" "u")
    (hlink* "t" "u" "The title")
    (big-figure (image "i.png" "" "" "" "") "alt text")
    (big-figure (image "i.png" "" "" "" "") (concat "The " (em "logo")))
    (big-figure (image "i.png" "36px" "" "" "") "The logo")
    (concat "x " (image "i.png" "0.5par" "20px" "" "") " y")
    (math (concat "x" (rsup "2")))
    (document (section "S") "text" (subsection "T") "more")
    (itemize (document (concat (item) "a") (concat (item) "b")))
    (enumerate (document (concat (item) "a") (concat (item) "b")))
    (quotation (document "q"))
    (verbatim-code (document "x" "y"))
    (python-code (document "print(1)"))
    (document "a" (equation* (document "x=1")) "b")
    (concat "a" (footnote "note") "b")))

(define (test-round-trips)
  (check-group "round trips")
  (for-each (lambda (s) (check= (export (import s)) s)) md-same)
  (for-each (lambda (t) (check= (import (export t)) t)) tm-same)
  ;; a figure alone is a paragraph, which ends its line
  (for-each (lambda (s) (check= (export (import s)) s))
            '("![alt *text*](i.png)\n"
              "<img src=\"i.png\" alt=\"The logo\" width=\"36\">\n"))
  ;; the parser and the serializer alone
  (for-each (lambda (s) (check= (write-md (parse s)) s)) md-same)
  (for-each (lambda (s) (check= (parse (write-md (parse s))) (parse s)))
            '("*a* **b** ***c***" "- a\n\n- b" "3. a\n4. b"
              "> q\n>\n> - l" "<div>\nhtml\n</div>" "    code\n    more"
              "[t](u \"T\") ![alt](i.png)"
              "<img src=\"i.png\" alt=\"a\" width=\"36\">")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-format)
  (check-group "format")
  (check-true (format? "markdown"))
  (check= (format-get-name "markdown") "Markdown")
  (check= (format-from-suffix "md") "markdown")
  (check= (format-from-suffix "markdown") "markdown")
  (check= (format-default-suffix "markdown") "md")
  (check-true (list? (converter-search "texmacs-file" "markdown-file")))
  (check-true (list? (converter-search "markdown-file" "texmacs-file")))
  (check-true (in? "markdown" (converters-from-special "texmacs-file" "-file"
                                                       #f)))
  (check-true (in? "markdown" (converters-to-special "texmacs-file" "-file"
                                                     #f)))
  ;; a text is not taken for Markdown by its contents
  (check-false (== (format-determine "# title\n\n*text*\n" "verbatim")
                   "markdown")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (markdown-test-failures)
  (:synopsis "Run the tests of the Markdown converters, return the failures")
  (check-suite "markdown")
  (test-parse-inline)
  (test-parse-math)
  (test-parse-blocks)
  (test-parse-lists)
  (test-parse-tables)
  (test-serialize)
  (test-serialize-escapes)
  (test-import-inline)
  (test-import-figures)
  (test-import-blocks)
  (test-import-document)
  (test-export-inline)
  (test-export-html)
  (test-export-figures)
  (test-export-image-sizes)
  (test-export-math)
  (test-export-blocks)
  (test-export-headings)
  (test-export-tables)
  (test-export-document)
  (test-round-trips)
  (test-format)
  (check-end))
