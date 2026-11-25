
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
;; and the round trips in both directions. The trees are not expanded
;; there: the export of a buffer, which runs the macros of its style first
;; (tmmarkdown-expand.scm), is tested with one document at the end.

(texmacs-module (check markdown-test)
  (:use (check check-lib)))

(define (parse s) (convert s "markdown-snippet" "markdown-stree"))
(define (plain t)
  ;; the Markdown tree without the texts of the file which it keeps for
  ;; the tables and for some formulas (the attribute source)
  (cond ((not (pair? t)) t)
        ((and (pair? (cdr t)) (func? (cadr t) '@))
         (with a (list-filter (cdadr t) (lambda (x) (not (func? x 'source))))
           `(,(car t) ,@(if (null? a) '() `((@ ,@a))) ,@(map plain (cddr t)))))
        (else (cons (car t) (map plain (cdr t))))))
(define (parse* s) (plain (parse s)))
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
  (check= (parse "1986\\. A year") '(markdown "1986. A year"))
  ;; a link holds no other link, and an address in its text is text
  (check= (parse "[https://x.y](https://x.y) [a <http://b.c>](u)")
          '(markdown (a (@ (href "https://x.y")) "https://x.y") " "
                     (a (@ (href "u")) "a http://b.c")))
  ;; the runs of * and _ are matched by the lengths which they had first
  (check= (parse "_foo_bar_baz_ __foo__bar foo_bar_")
          '(markdown (em "foo_bar_baz") " _" (em "foo__bar foo_bar")))
  (check= (parse "*foo**bar**baz* *foo**bar* **foo*bar** ***a** b*")
          '(markdown (em "foo" (strong "bar") "baz") " " (em "foo**bar") " "
                     (strong "foo*bar") " " (em (strong "a") " b"))))

;; Texts which took a time growing as the square of their length, or
;; faster: images inside each other, and what opens and is never closed.
(define (test-parse-long)
  (check-group "parse long")
  (let* ((rep (lambda (s n) (apply string-append (make-list n s))))
         (len (lambda (s) (length (parse s)))))
    (check= (len (string-append (rep "![" 40) "a" (rep "](u)" 40))) 2)
    (check= (len (rep "*a_ " 20000)) 2)
    (check= (len (string-append "a**b " (rep "c* " 20000))) 2)
    ;; (the space which ends the paragraph is not text)
    (check= (len (rep "*a b~~ c* " 10000)) 20000)
    (check= (len (rep "[a " 20000)) 2)
    (check= (len (string-append (rep "[" 10000) (rep "]" 10000))) 2)
    (check= (len (rep "[a]((" 10000)) 2)
    (check= (len (rep "[a](u \"" 10000)) 2)
    (check= (len (rep "a < b " 20000)) 2)
    (check= (len (rep "<a <a " 10000)) 2)
    (check= (len (rep "AT&T " 20000)) 2)
    (check= (len (rep "\\(a " 20000)) 2)
    (check= (len (rep "word word\n" 40000)) 2)
    (check= (len (rep "<!-- x\n\n" 5000)) 5001)
    ;; formulas which do not end, blank lines after indented code, runs of
    ;; backquotes which nothing closes, footnote marks inside each other
    ;; (a formula which does not end is text of the paragraph)
    (check= (len (rep "\\[=\n" 20000)) 2)
    (check= (len (string-append "    a\n" (rep "\n" 8000) "    b\n")) 2)
    (check= (len (string-append "    a\n" (rep "\n" 8000) "b\n")) 3)
    (check= (len (apply string-append
                        (map (lambda (i) (string-append (make-string i #\`) "a "))
                             (.. 1 300)))) 2)
    (check= (len (string-append (rep "[^" 3000) " x" (rep "]" 3000))) 2)
    ;; quotes and lists inside each other: beyond a hundred levels their
    ;; marks are text
    (check= (parse "> > > x")
            '(markdown (blockquote (blockquote (blockquote (p "x"))))))
    (check= (len (string-append (rep ">" 3000) " x")) 2)
    (check= (len (string-append (rep "- " 3000) "x")) 2)))

;; The formulas are kept as LaTeX. A dollar opens a formula as for Pandoc:
;; not before a space, and the one which closes is not after a space nor
;; before a digit. \[2\] are the brackets which other programs escape.
(define (test-parse-math)
  (check-group "parse math")
  (check= (parse* "$x^2$ and $5 and $6")
          '(markdown (math "x^2") " and $5 and $6"))
  (check= (parse* "\\$5") '(markdown "$5"))
  (check= (parse* "\\(a\\) \\[2\\] \\[x=1\\]")
          '(markdown (math "a") " [2] " (displaymath "x=1")))
  (check= (parse* "$$\nx=1\n$$") '(markdown (displaymath "x=1")))
  (check= (parse* "\\[\nx=1\n\\]") '(markdown (displaymath "x=1")))
  (check= (parse* "\\[ x=1 \\]\n\ntext") '(markdown (displaymath "x=1") (p "text")))
  ;; the brackets which other programs escape hold words, formulas do not
  (check= (parse* "\\[see Smith (2010)\\] \\[sic!\\] \\[f(x)\\] \\[\\alpha\\]")
          '(markdown "[see Smith (2010)] [sic!] " (displaymath "f(x)") " "
                     (displaymath "\\alpha")))
  ;; what opens a formula and is not closed before a blank line is text,
  ;; and the text after a formula is kept
  (check= (parse* "$$ is the price\n\nsecond")
          '(markdown (p "$$ is the price") (p "second")))
  (check= (parse* "\\[x] done\n\nsecond")
          '(markdown (p "[x] done") (p "second")))
  (check= (parse* "$$x$$ and text")
          '(markdown (displaymath "x") (p "and text"))))

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
                     (footnote-def "n" (p "note"))))
  ;; a rule at the start is not a header without keys
  (check= (parse "---\n\ntext\n\n---\n\nmore")
          '(markdown (hr) (p "text") (hr) (p "more")))
  (check= (parse "---\nFoo\n---") '(markdown (hr) (h2 "Foo")))
  ;; the definition of a link has an address without spaces
  (check= (parse "[Note]: see the appendix\n\n[n]: u 'T'\n\n[n]")
          '(markdown (p "[Note]: see the appendix")
                     (p (a (@ (href "u") (title "T")) "n"))))
  ;; a comment may hold a blank line
  (check= (parse "<!-- a\n\nb -->\n\ntext")
          '(markdown (html "<!-- a\n\nb -->") (p "text"))))

(define (test-parse-lists)
  (check-group "parse lists")
  (check= (parse "- a\n- b\n  - c\n\n1. x\n2. y")
          '(markdown (ul (li (p "a")) (li (p "b") (ul (li (p "c")))))
                     (ol (li (p "x")) (li (p "y")))))
  (check= (parse "+ plus list") '(markdown (ul (@ (marker "+")) (li (p "plus list")))))
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
  (check= (parse* "| a | b |\n|:-:|--:|\n| 1 | 2 |")
          '(markdown (table (tr (th (@ (align "center")) "a")
                                (th (@ (align "right")) "b"))
                            (tr (td (@ (align "center")) "1")
                                (td (@ (align "right")) "2")))))
  (check= (parse* "a | b\n--|--\n1 | 2")
          '(markdown (table (tr (th "a") (th "b")) (tr (td "1") (td "2")))))
  ;; a table after the lines of a paragraph
  (check= (parse* "intro\n| a | b |\n|---|---|\n| 1 | 2 |")
          '(markdown (p "intro")
                     (table (tr (th "a") (th "b")) (tr (td "1") (td "2")))))
  ;; in code a backslash is one, and a bar does not end the cell
  (check= (parse* "| a | b |\n|---|---|\n| `x\\` | ``c|d`` |")
          '(markdown (table (tr (th "a") (th "b"))
                            (tr (td (code "x\\")) (td (code "c|d")))))))

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
  (check= (write-md '(markdown (pre "```\nx"))) "````\n```\nx\n````\n")
  ;; code with a space at each end gets one more, which the readers drop
  (check= (write-md '(markdown (code " a "))) "`  a  `")
  ;; a heading is one line, and the # which ends it is text
  (check= (write-md '(markdown (h1 "foo #") (h2 "a" (br) "b") (h1 "c#")))
          "# foo \\#\n\n## a b\n\n# c#\n")
  ;; two lists of a kind in a row have different markers
  (check= (write-md '(markdown (ul (li (p "a"))) (ul (li (p "b")))
                               (ol (li (p "c"))) (ol (li (p "d")))))
          "- a\n\n* b\n\n1. c\n\n1) d\n")
  ;; in an item, a table after a list or a quote, and a quote after a quote,
  ;; are read so after a blank line only, which makes the list one with
  ;; blank lines
  (for-each
    (lambda (l)
      (check= (parse* (write-md `(markdown (ul (li ,@l)))))
              `(markdown (ul (@ (loose "true")) (li ,@l)))))
    '(((p "a") (ul (li (p "sub")))
       (table (tr (th "x") (th "y")) (tr (td "1") (td "2"))))
      ((blockquote (p "q")) (table (tr (th "x")) (tr (td "1"))))
      ((blockquote (p "q1")) (blockquote (p "q2")))))
  ;; the definition of a link to which nothing refers
  (check= (write-md '(markdown (p "a") (ref-def "[//]: # (c)")))
          "a\n\n[//]: # (c)\n"))

;; What would be read as markup is escaped, and nothing else.
(define (test-serialize-escapes)
  (check-group "serialize escapes")
  (check= (write-md '(markdown "a*b _c_ [d] <e> #f"))
          "a\\*b \\_c\\_ [d] \\<e> #f")
  (check= (write-md '(markdown "snake_case 2 * 3 a < b & c"))
          "snake_case 2 * 3 a < b & c")
  (check= (write-md '(markdown "a $5 &amp; `q`")) "a \\$5 \\&amp; \\`q\\`")
  (check= (write-md '(markdown "[x](y) [k]: v [^1]"))
          "[x\\](y) [k\\]: v [\\^1]")
  (check= (write-md '(markdown (p "# not heading") (p "1. not list")
                               (p "- not item") (p "> not quote")))
          "\\# not heading\n\n1\\. not list\n\n\\- not item\n\n\\> not quote\n")
  (check= (write-md '(markdown (ul (li (p "[ ] not a task")))))
          "- [ \\] not a task\n")
  (check= (write-md '(markdown (table (tr (th "a|b") (th (math "|x|"))))))
          "| a\\|b | $\\vert x\\vert $ |\n| ---- | --------------- |\n")
  ;; an ! before a link, a ~, a ] in the text of a link
  (check= (write-md '(markdown (p "Wow!" (a (@ (href "u")) "here"))
                               (p "a ~b~ c ~ d")
                               (p (a (@ (href "u")) "a]b [ c"))))
          "Wow\\![here](u)\n\na \\~b\\~ c ~ d\n\n[a\\]b &#91; c](u)\n")
  ;; next to a letter, the punctuation at the ends of emphasis goes outside
  (check= (write-md '(markdown "f" (em "(x)") "y A " (em "note.") "Then a"
                               (strong ".") "b"))
          "f(*x*)y A *note*.Then a.b")
  ;; a digit after a formula
  (check= (write-md '(markdown "x" (math "n") "3")) "x$n$&#51;")
  ;; a line which would make a table of the line before
  (check= (write-md '(markdown (p "a | b" (br) "--|--"))) "a | b\\\n\\--|--\n")
  ;; an address in a text is a link, with all its characters
  (check= (write-md '(markdown "see http://x.org/?a=1&b=2 and http://x.org/_f_g ok"))
          "see <http://x.org/?a=1&b=2> and <http://x.org/_f_g> ok"))

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
  ;; a footnote inside itself is its mark
  (check= (import "x[^a]\n\n[^a]: y[^a]") '(concat "x" (footnote "y[^a]")))
  ;; a backquote stays one
  (check= (import "`` a`b `` c ` d") '(concat (verbatim "a`b") " c ` d"))
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
;; The sizes of an image are numbers with a unit of length, or nothing.
(define (test-import-image-sizes)
  (check-group "import image sizes")
  (check= (import "x ![a](i.png){width=50% height=2cm}")
          '(concat "x " (image "i.png" "0.5par" "2cm" "" "")))
  (check= (import "x ![a](i.png){width=120 height=1.5em}")
          '(concat "x " (image "i.png" "120px" "1.5em" "" "")))
  (check= (import "x ![a](i.png){width=1/2 height=foo}")
          '(concat "x " (image "i.png" "" "" "" "")))
  (check= (import "x ![a](i.png){width=1e3 height=-2cm}")
          '(concat "x " (image "i.png" "" "" "" ""))))

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
  (check= (import "5. a\n6. b")
          '(enumerate (document (concat (assign "item-nr" "4") (item) "a")
                                (concat (item) "b"))))
  (check= (import "| a | b |\n|:-:|--:|\n| 1 | 2 |")
          '(block (tformat (cwith "1" "-1" "1" "1" "cell-halign" "c")
                           (cwith "1" "-1" "2" "2" "cell-halign" "r")
                           (table (row (cell (strong "a")) (cell (strong "b")))
                                  (row (cell "1") (cell "2")))))))

;; A document: a YAML header or a single heading of the first level is the
;; title, and the other headings move up.
(define (test-import-document)
  (check-group "import document")
  ;; (as a document of the style generic, with a title: the option is on
  ;; by default, see the group "style markdown")
  (let ((doc (lambda (s) (convert s "markdown-document" "texmacs-stree"
                                  (cons "markdown->texmacs:faithful" "off")))))
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
    ;; the quotes and the backslashes of the header
    (check= (doc "---\ntitle: \"A \\\"b\\\" c\\\\d\"\nauthor: 'it''s'\n---\n")
            '(document
               (body (document
                       (doc-data (doc-title "A \"b\" c\\d")
                                 (doc-author (author-data (author-name "it's"))))))
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
  (check= (export '(concat "a" (footnote (concat "b" (footnote "c"))) "d"))
          "a[^1]d\n\n[^1]: b[^2]\n\n[^2]: c\n")
  ;; code after code is one piece of code
  (check= (export '(concat "a" (verbatim "x") (verbatim "``") (verbatim "`")))
          "a```` x``` ````")
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
  ;; a formula of one symbol is a formula too, and is read back as one
  (check= (export '(concat "a " (math "<rightarrow>") " b"))
          "a $\\rightarrow$ b")
  (for-each
    (lambda (x)
      (check= (import (export `(concat "a " (math ,x) " b")))
              `(concat "a " (math ,x) " b")))
    '("<alpha>" "<rightarrow>" "<infty>" "<bbb-R>" "<partial>" "x")))

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
          "| a   |   b |\n| --- | --: |\n| 1   |   2 |\n")
  ;; a cell holds text: a formula, code and a list are written as text
  (check= (export '(tabular (tformat (table
                     (row (cell "a") (cell "b"))
                     (row (cell (equation* (document "a|b")))
                          (cell (verbatim-code (document "c1" "c2"))))
                     (row (cell (enumerate (document (concat (item) "x")
                                                     (concat (item) "y"))))
                          (cell ""))))))
          (string-append
            "| a            | b            |\n"
            "| ------------ | ------------ |\n"
            "| $$a\\vert b$$ | `c1`<br>`c2` |\n"
            "| 1. x<br>2. y |              |\n")))

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
    "2 * 3, snake_case, \\*not em\\*, No name [2]"
    "[x\\] done"
    "Wow\\![here](http://x.org)"
    "a \\~b\\~ c"
    "- a\n\n* b\n"
    "1. a\n\n1) b\n"
    "5. a\n6. b\n"
    "# foo \\#\n"))

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
    (concat "a" (footnote "note") "b")
    ;; what was read back as something else
    "[x] done"
    "[^1] is a regexp"
    (document "[x] done" "second" "third")
    (itemize (document (concat (item) "[x] literal") (concat (item) "b")))
    (concat "Wow!" (hlink "here" "http://x.org"))
    "a ~b~ c"
    (verbatim " a ")
    (scm-code (document "`(a ,b)"))
    (concat "x" (math "n") "3 apples")
    (hlink "a]b" "http://x.org")
    (document (section "foo #") "t")
    (document (itemize (document (concat (item) "a")))
              (itemize (document (concat (item) "b"))))
    (enumerate (document (concat (assign "item-nr" "4") (item) "a")
                         (concat (item) "b")))
    (concat "a | b" (next-line) "--|--")
    (block (tformat (table (row (cell (strong "a")) (cell (strong "b")))
                           (row (cell (verbatim "x\\")) (cell "y")))))))

;; Long paragraphs, in both directions, and footnotes which refer to each
;; other: a footnote inside a footnote is its mark.
(define (test-long)
  (check-group "long")
  (let* ((rep (lambda (s n) (apply string-append (make-list n s))))
         (count (lambda (t) (if (func? t 'concat) (length (cdr t)) 1)))
         (ems (lambda (n)
                (let loop ((i 0) (acc '()))
                  (if (>= i n) (cons 'concat acc)
                      (loop (+ i 1) (cons '(em "a") (cons " b " acc)))))))
         (notes (lambda (n)
                  (apply string-append
                         (cons "x[^1]\n\n"
                               (map (lambda (i)
                                      (with next (number->string (+ i 1))
                                        (string-append
                                          "[^" (number->string i) "]: a [^" next
                                          "] [^" next "]\n\n")))
                                    (.. 1 31)))))))
    (check= (count (import (rep "*a* " 20000))) 39999)
    (check= (count (import (rep "a `b` " 10000))) 20000)
    (check= (string-length (export (ems 20000))) 119999)
    (check= (string-length (export (rep "a... " 20000))) 99999)
    (check= (import (string-append "``` \n" (make-string 20000 #\`) "x\n```"))
            `(verbatim-code (document ,(string-append (make-string 20000 #\`)
                                                      "x"))))
    (check= (import (notes 30)) '(concat "x" (footnote "a [^2] [^2]")))
    (check= (import "a[^1] b[^2]\n\n[^1]: one [^2]\n\n[^2]: two [^2]\n")
            '(concat "a" (footnote "one [^2]") " b" (footnote "two [^2]")))))

(define (test-round-trips)
  (check-group "round trips")
  ;; what looks like the definition of a link or of a footnote is text
  (for-each (lambda (t) (check= (import (export t)) t))
            '((document "[1]: Knuth" "text")
              (document "[^1]: y" "text")
              (hlink "x" "a b<gtr>c")
              (hlink "x" "a (b) <less>c<gtr>")
              (hlink* "x" "u" "t\\")
              (hlink* "x" "u" "say \"hi\"")))
  (check= (export '(document "[1]: Knuth")) "[1\\]: Knuth\n")
  (check= (import "[a][1]\n\n[1]: http://x.y\n") '(hlink "a" "http://x.y"))
  (check= (import "[x](<a b> \"t\") [y](<a\\>b>)")
          '(concat (hlink* "x" "a b" "t") " " (hlink "y" "a<gtr>b")))
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
;; The export of a buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A buffer is exported with the macros of its style expanded: the numbers
;; of the sections, of the theorems and of the equations, the references,
;; the title as a header, the footnotes at the end.
(define (test-export-buffer)
  (check-group "export buffer")
  (let* ((old (current-buffer))
         (u (new-buffer))
         (dir (url-append (url-temp-dir) "markdown-test"))
         (to (url-append dir "out.md")))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree
      '(document
         (doc-data (doc-title "The title")
                   (doc-author (author-data (author-name "An Author"))))
         (section "First")
         (concat "Some text" (footnote "a note") " and " (reference "eq"))
         (theorem (document "A statement."))
         (equation (concat "a=b" (label "eq")))
         (subsection "Sub")
         (itemize (document (concat (item) "one") (concat (item) "two"))))))
    (set-main-style "article")
    (update-current-buffer)
    (update-forced)
    (system-mkdir dir)
    (with r (check-run
              (lambda ()
                (export-buffer-main u to "markdown" (list :overwrite))
                (check= (string-load to)
                        (string-append
                          "---\ntitle: The title\nauthor: An Author\n---\n\n"
                          "# 1 First\n\n"
                          "Some text[^1] and 1\n\n"
                          "**Theorem 1.** A statement.\n\n"
                          "$$\na = b \\tag{1}\n$$\n\n"
                          "## 1.1 Sub\n\n"
                          "- one\n- two\n\n"
                          "[^1]: a note\n"))))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))
    (if (url-exists? to) (system-remove to))
    (system-rmdir dir)
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The style markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A file is read as a document of the style markdown (styles/markdown.ts),
;; whose macros keep what the standard tags do not say, and which is
;; written as the file it was read from.
(define (md->tm s) (convert s "markdown-document" "texmacs-stree"))
(define (tm->md t) (convert t "texmacs-stree" "markdown-document"))
(define (md->md s) (tm->md (md->tm s)))
(define (md-tree s) (convert s "markdown-document" "markdown-stree"))
(define (md-body s) (cdr (cadr (cadr (md->tm s)))))

(define markdown-file
  (string-append
    "---\ntitle: The title\ntags: [a, b]\nlist:\n  - x\n# comment\n"
    "layout: post\n---\n\n"
    "## Starts at two\n\n"
    "Some *text* with $a=b$ and \\(c+d\\) and a note[^n] and "
    "<span class=\"x\">html</span>.\n\n"
    "###### six\n\n"
    "$$\n\\begin{aligned}a&=b\\\\c&=d\\end{aligned}\n$$\n\n"
    "* star one\n\n* star two\n\n+ plus\n\n"
    "```rust ignore\nfn main() {}\n```\n\n```c\nint x;\n```\n\n"
    "![alt *text*](i.png \"The title\")\n\n"
    "Inline ![alt](i.png) image and [anchor](#starts-at-two).\n\n"
    "| a | b | c |\n|:--|:-:|--:|\n| 1 | 2 | 3 |\n\n"
    "<!-- a comment -->\n\n"
    "<div class=\"note\">\nhtml block\n</div>\n\n"
    "[^n]: The note.\n\n"
    "[^orphan]: Nobody refers to this.\n"))

(define (test-style-import)
  (check-group "style markdown, import")
  (check= (cddr (md->tm "x\n")) '((style "markdown")))
  ;; each heading at its level, and no title
  (check= (md-body "# A\n\n## B\n\n###### F\n")
          '((section "A") (subsection "B") (md-h6 "F")))
  ;; the header: a line, a key
  (check= (md-body "---\ntitle: T\ntags:\n  - a\n---\n\nx\n")
          '((md-metadata (document (md-meta "title" "T") (md-meta "tags" "")
                                   (md-meta "" "  - a")))
            "x"))
  ;; a formula as it is written, with a copy of what it was read as, also
  ;; one which TeXmacs writes in the same way (to know it would take a
  ;; conversion to LaTeX of each formula)
  (check= (md-body "$a=b$ $x^2$ \\(y\\)\n")
          '((concat (md-source (math "a=b") "$a=b$" (math "a=b")) " "
                    (md-source (math (concat "x" (rsup "2"))) "$x^2$"
                               (math (concat "x" (rsup "2")))) " "
                    (md-source (math "y") "\\(y\\)" (math "y")))))
  (check= (md-body "<!-- c -->\n")
          '((md-block-source "" "<less>!-- c --<gtr>" "")))
  ;; lists: the character of the items, the blank lines
  (check= (md-body "- a\n\n* b\n\n+ c\n")
          '((itemize-minus (document (concat (item) "a")))
            (itemize-dot (document (concat (item) "b")))
            (itemize-arrow (document (concat (item) "c")))))
  (check= (md-body "- a\n\n- b\n")
          '((md-loose (itemize-minus (document (concat (item) "a")
                                               (concat (item) "b"))))))
  ;; code: the words after the fence, when the tag does not say them
  (check= (md-body "```python\nx\n```\n\n```py\nx\n```\n\n```rust\nx\n```\n")
          '((python-code (document "x"))
            (md-fence "py" (python-code (document "x")))
            (md-fence "rust" (verbatim-code (document "x")))))
  ;; images and footnotes
  (check= (md-body "a ![d](i.png) ![](j.png) ![d](i.png \"T\")\n")
          '((concat "a " (md-image (image "i.png" "" "" "" "") "d" "") " "
                    (image "j.png" "" "" "" "") " "
                    (md-image (image "i.png" "" "" "" "") "d" "T"))))
  (check= (md-body "a[^1] b[^x]\n\n[^1]: one\n\n[^x]: two\n")
          '((concat "a" (footnote "one") " b" (md-footnote "x" "two"))))
  (check= (md-body "a[^x]\n\n[^x]: two\n\nlast\n")
          '((concat "a" (md-footnote "x" "two")) (md-footnote-text "x") "last"))
  ;; with the option off, as before the style
  (check= (convert "# T\n\nx\n" "markdown-document" "texmacs-stree"
                   (cons "markdown->texmacs:faithful" "off"))
          '(document (body (document (doc-data (doc-title "T")) "x"))
                     (style "generic"))))

(define (test-style-round-trip)
  (check-group "style markdown, round trip")
  ;; a file with all of this is written as it was read
  (check= (md->md markdown-file) markdown-file)
  ;; what Markdown writes in several ways is written in one: the file
  ;; which is written says the same, and is written as itself
  (for-each
    (lambda (s)
      (with t (md->md s)
        (check= (md-tree t) (md-tree s))
        (check= (md->md t) t)))
    '("Setext\n======\n\nSub\n---\n\ntext\n"
      "soft\nbreak and  \nhard break\n"
      "1) a\n2) b\n"
      "~~~\ntilde\n~~~\n\n    indented\n"
      "a\n\n---\n\nb\n"
      "[t][r] and http://u.v/x here\n\n[r]: http://u.v \"T\"\n"
      "a &amp; b &copy; \\*c\\* _d_ __e__\n"
      "* a\n* b:\n  ```scheme\n  (c)\n  ```\n  after\n* c\n"
      "* a\n  | x | y |\n  |---|---|\n  | 1 | 2 |\n* b\n"
      "> quote\n> more\n>\n> - a\n"
      "- [ ] todo\n- [x] done\n    - sub\n"
      "t[^n]\n\n[^n]: first\n\n    second\n"))
  ;; what was edited is written anew, and the rest as it was
  (let* ((s "$a=b$ and \\(c\\)\n\n| a |\n|:-:|\n| 1 |\n")
         (t (md->tm s))
         (edit (lambda (x)
                 ;; the body of the first formula, and of the table
                 (cond ((and (func? x 'md-source 3) (== (caddr x) "$a=b$"))
                        `(md-source (math "z") ,(caddr x) ,(cadddr x)))
                       (else x))))
         (walk (lambda (x)
                 (let loop ((x x))
                   (if (pair? x) (edit (cons (car x) (map loop (cdr x)))) x)))))
    (check= (tm->md t) s)
    (check= (tm->md (walk t)) "$z$ and \\(c\\)\n\n| a |\n|:-:|\n| 1 |\n"))
  ;; a new footnote takes a number which is not the label of another
  (check= (tm->md '(document
                     (body (document (concat "a" (footnote "new")
                                             "b" (md-footnote "1" "old"))))
                     (style "markdown")))
          "a[^2]b[^1]\n\n[^2]: new\n\n[^1]: old\n"))

(define tab-char (string #\tab))

(define (in-style . l)
  `(document (body (document ,@l)) (style "markdown")))

;; What the first version of the style lost or changed.
(define (test-style-losses)
  (check-group "style markdown, what is kept")
  (for-each
    (lambda (s) (check= (md->md s) s))
    (list
      ;; tabs, displayed as spaces, and backquotes which are text
      (string-append "a" tab-char "b\n")
      (string-append "`a" tab-char "b`\n")
      (string-append "```\na" tab-char "b\n```\n")
      (string-append "| a" tab-char "b | c |\n|---|---|\n| 1 | 2 |\n")
      (string-append "\\(a" tab-char "b\\)\n")
      (string-append "# a" tab-char "b\n")
      "a \\` b and \\`j\\`\n"
      "two  spaces\n"
      ;; definitions of footnotes in a quote, an item, another footnote
      "- item\n\n  [^z]: text\n"
      "[^a]: outer\n\n    [^b]: inner\n"
      "x[^a]\n\n> [^a]: note\n"
      "x[^a]\n\n[^a]: outer\n\n    [^b]: inner\n"
      ;; definitions of links to which nothing refers, comments
      "[//]: # (a comment)\n\ntext\n"
      "text\n\n[unused]: http://x.y \"Title\"\n"
      ;; marks of footnotes: in a footnote, without definition, repeated
      "a[^1]\n\n[^1]: see[^2]\n\n[^2]: other\n"
      "text[^a]\n"
      "a[^1]\n\n[^1]: first\n\n[^1]: second\n"
      "a[^1] b[^1]\n\n[^1]: one\n"
      ;; headers with any keys, with their spaces
      "---\ntitle: a\nfoo.bar: 1\n---\n\nx\n"
      "---\n\"og:image\": x\nkey with space: v\n_private: 1\n---\n"
      "---\ntags: [a,\n  b]\n---\n\nx\n"
      "---\ntitle:   The  title\n---\n"))
  ;; the second definition of a link, which is not the one which counts
  (check= (md->md "[a][r]\n\n[r]: http://x.y\n\n[r]: http://other.z\n")
          "[a](http://x.y)\n\n[r]: http://other.z\n")
  ;; a note with many marks is written once
  (with s (string-append "a[^1]" (apply string-append (make-list 299 " a[^1]"))
                         "\n\n[^1]: the note\n")
    (check= (md->md s) s))
  ;; a document starts with a rule, which is not a header
  (check= (md->md "---\n\ntext\n\n---\n") "***\n\ntext\n\n***\n")
  (check= (md->md "---\ntext\n\nmore\n\n---\n") "***\n\ntext\n\nmore\n\n***\n")
  ;; what is typed in the header, and in the macros
  (check= (tm->md (in-style '(md-metadata (document (md-meta "title" "T")
                                                    "author: me"
                                                    (concat (md-meta "a" "b"))))))
          "---\ntitle: T\nauthor: me\na: b\n---\n")
  ;; two notes which got the same label: both are written
  (check= (tm->md (in-style '(concat "a" (md-footnote "x" "one")
                                     " b" (md-footnote "x" "two")
                                     " c" (md-footnote "x" "one"))))
          "a[^x] b[^x-2] c[^x]\n\n[^x]: one\n\n[^x-2]: two\n")
  ;; the words after the fence are for the language they name
  (check= (tm->md (in-style '(md-fence "c" (python-code (document "x")))))
          "```python\nx\n```\n")
  (check= (tm->md (in-style '(md-fence "rust ignore"
                                       (verbatim-code (document "x")))))
          "```rust ignore\nx\n```\n")
  ;; a kept block which was moved into a heading or a cell
  (check= (tm->md (in-style '(section (concat "T " (md-block-source
                                                     (equation* (document "x"))
                                                     "\\[x\\]"
                                                     (equation* (document "x")))))))
          "# T \\[x\\]\n")
  ;; an edited cell, and a kept formula with a bar in another one
  (let* ((t (md->tm "| a | b |\n|---|---|\n| 1 | \\(x\\|y\\) |\n"))
         (edit (lambda (x)
                 (if (and (func? x 'cell 1) (== (cadr x) "1")) '(cell "ONE") x)))
         (walk (lambda (x)
                 (let loop ((x x))
                   (cond ((func? x 'md-block-source 3)
                          `(md-block-source ,(loop (cadr x)) ,@(cddr x)))
                         ((pair? x) (edit (cons (car x) (map loop (cdr x)))))
                         (else x))))))
    (check= (tm->md (walk t))
            "| a   | b        |\n| --- | -------- |\n| ONE | \\(x\\|y\\) |\n"))
  ;; the macros in code, and in LaTeX
  (check= (tm->md (in-style '(verbatim (md-source (math "y") "$y$" (math "y")))))
          "`y`\n")
  (with s (convert (md->tm markdown-file) "texmacs-stree" "latex-document")
    (check= (string-search-forwards "\\md" 0 s) -1)
    (check-true (>= (string-search-forwards "\\footnote{The note.}" 0 s) 0))))

;; What the second version of the style lost or changed.
(define (test-style-losses-2)
  (check-group "style markdown, what is kept (2)")
  (for-each
    (lambda (s) (check= (md->md s) s))
    (list
      ;; a table which is not changed is written as it was read, with the
      ;; definitions of its links and of its footnotes
      "| h |\n| --- |\n| [a] |\n\n[a]: http://x\n"
      "| h |\n| --- |\n| a[^1] |\n\ntext\n\n[^1]: note\n\nmore\n"
      "| a | b |\n|---|---|\n| x[^1] | 1 |\n\ntext[^1]\n\n[^1]: note\n"
      "text[^1]\n\n| a | b |\n|---|---|\n| x[^1] | 1 |\n\n[^1]: note\n"
      ;; tabs in a header, a link title, the words after a fence
      (string-append "---\ntitle:" tab-char "x\n---\n\ny\n")
      (string-append "[a](http://x \"t" tab-char "u\")\n")
      ;; the definition of a link in an item, before a paragraph
      "- [x]: http://y\n  b\n"))
  ;; a link of a table which was changed is written whole
  (let* ((t (md->tm "| a | b |\n|---|---|\n| [r] | 1 |\n\n[r]: http://x\n"))
         (walk (lambda (x)
                 (let loop ((x x))
                   (cond ((func? x 'md-block-source 3)
                          `(md-block-source ,(loop (cadr x)) ,@(cddr x)))
                         ((== x "1") "ONE")
                         ((pair? x) (cons (car x) (map loop (cdr x))))
                         (else x))))))
    (check= (tm->md (walk t))
            (string-append "| a             | b   |\n| ------------- | --- |\n"
                           "| [r](http://x) | ONE |\n\n[r]: http://x\n")))
  ;; a line of code with a tab keeps it when another line is changed
  (let* ((t (md->tm (string-append "```\nhead\na" tab-char "b\n```\n")))
         (walk (lambda (x)
                 (let loop ((x x))
                   (cond ((func? x 'md-block-source 3)
                          `(md-block-source ,(loop (cadr x)) ,@(cddr x)))
                         ((== x "head") "HEAD")
                         ((pair? x) (cons (car x) (map loop (cdr x))))
                         (else x))))))
    (check= (tm->md (walk t)) (string-append "```\nHEAD\na" tab-char "b\n```\n")))
  ;; rules around a heading or text are not a header
  (check= (md->tm "---\n# Title\n---\n\ntext\n")
          '(document (body (document (hrule) (section "Title") (hrule) "text"))
                     (style "markdown")))
  (check= (convert "---\n# Title\n---\n\ntext\n" "markdown-snippet" "texmacs-stree")
          '(document (hrule) (section "Title") (hrule) "text"))
  ;; a snippet has no header: what it holds is text
  (check= (convert "---\ntitle: x\n---\n\ntext\n" "markdown-snippet" "texmacs-stree")
          '(document (hrule) (subsection "title: x") "text"))
  ;; a header which was edited is read back as a header
  (for-each
    (lambda (h)
      (let* ((md (tm->md (in-style h "x")))
             (back (md->tm md)))
        (check= (tm->md back) md)
        (check-true (func? (cadr (cadr (cadr back))) 'md-metadata))))
    '((md-metadata (document "hello" (md-meta "title" "T")))
      (md-metadata (document "" (md-meta "title" "T")))
      (md-metadata (document "- a" (md-meta "title" "T")))
      (md-metadata (document "---" (md-meta "title" "T")))))
  ;; with no key, its lines are paragraphs, and an empty one is not written
  (check= (tm->md (in-style '(md-metadata (document "hello" "# world")) "x"))
          "hello\n\n\\# world\n\nx\n")
  (check= (tm->md (in-style '(md-metadata (document "")) "x")) "x\n")
  ;; many notes with labels: the same note is found at once
  (let* ((n 3000)
         (notes (map (lambda (i)
                       `(md-footnote ,(string-append "n" (number->string i))
                                     ,(number->string i)))
                     (iota n)))
         (s (tm->md (in-style `(concat "a" ,@notes)))))
    (check-true (>= (string-search-forwards "[^n2999]: 2999" 0 s) 0)))
  ;; a key in LaTeX
  (check-true
    (>= (string-search-forwards "title:}"  0
                                (convert (in-style '(md-metadata (document (md-meta "title" "T"))))
                                         "texmacs-stree" "latex-document"))
        0)))

;; Tabs at the start of lines: columns for the blocks, tabs in what they hold.
(define (tabs . l)
  ;; the strings of l, with each "~" as a tab
  (string-replace (apply string-append l) "~" tab-char))

(define (test-style-tabs)
  (check-group "style markdown, tabs at the start of lines")
  ;; kept: code, HTML, formulas, a header, inside a quote and an item
  (for-each
    (lambda (s) (check= (md->md s) s))
    (list
      (tabs "```\n~a\n~~b\n```\n")
      (tabs "```\n~\n  ~ a\n```\n")
      (tabs "> ```\n> ~x\n> ```\n")
      (tabs "- a\n\n  ```\n  ~x\n  ```\n")
      (tabs "$$\n~x\n$$\n")
      (tabs "<div>\n~x\n</div>\n")
      (tabs "---\ntitle: T\n~x: 1\n---\n\ny\n")))
  ;; the blocks see a tab as the columns up to the next multiple of 4
  (check= (parse (tabs "~code\n~~more\n")) '(markdown (pre "code\n\tmore")))
  (check= (parse (tabs "    ~x\n  ~y\n")) '(markdown (pre "\tx\ny")))
  (check= (parse (tabs "```\n~a\n```\n")) '(markdown (pre "\ta")))
  ;; columns of a tab which an item or a fence take are spaces
  (check= (parse (tabs "1. a\n\n~~code\n"))
          '(markdown (ol (@ (loose "true")) (li (p "a") (pre " code")))))
  (check= (parse (tabs "  ```\n~x\n  ```\n")) '(markdown (pre "  x")))
  ;; in a paragraph, the spaces which start a line are not text
  (check= (parse (tabs "a\n~b\n")) '(markdown "a b"))
  ;; what is written in another way says the same, and is written as itself
  (for-each
    (lambda (s)
      (with t (md->md s)
        (check= (md-tree t) (md-tree s))
        (check= (md->md t) t)))
    (list (tabs "~code\n~~more\n") (tabs "a\n~b\n")
          (tabs "1. a\n\n~~code\n") (tabs "- a\n~b\n")))
  ;; a text with the characters which stand for these tabs: spaces, as before
  (check= (parse (string-append (tabs "```\n~a\n```\n\n")
                                (string (integer->char 31))))
          `(markdown (pre "    a") (p ,(string (integer->char 31)))))
  ;; a line which starts with a tab keeps it when another line is changed
  (let* ((t (md->tm (tabs "```\nhead\n~b\n```\n")))
         (walk (lambda (x)
                 (let loop ((x x))
                   (cond ((func? x 'md-block-source 3)
                          `(md-block-source ,(loop (cadr x)) ,@(cddr x)))
                         ((== x "head") "HEAD")
                         ((pair? x) (cons (car x) (map loop (cdr x))))
                         (else x))))))
    (check= (tm->md (walk t)) (tabs "```\nHEAD\n~b\n```\n"))))

;; The same through a buffer, whose macros are expanded when it is saved.
(define (test-style-buffer)
  (check-group "style markdown, buffer")
  (let* ((old (current-buffer))
         (dir (url-append (url-temp-dir) "markdown-test"))
         (from (url-append dir "in.md"))
         (to (url-append dir "saved.md")))
    (system-mkdir dir)
    (string-save markdown-file from)
    (load-buffer from)
    (with u (current-buffer)
      (with r (check-run
                (lambda ()
                  (update-forced)
                  (check= (get-style-list) '("markdown"))
                  (export-buffer-main u to "markdown" (list :overwrite))
                  (check= (string-load to) markdown-file)
                  ;; a word is typed in the first paragraph
                  (with p (tree-ref (buffer-tree) 2)
                    (tree-set (tree-ref p 0) "Other "))
                  (update-forced)
                  (export-buffer-main u to "markdown" (list :overwrite))
                  (check= (string-load to)
                          (string-replace markdown-file "Some *text*"
                                          "Other *text*"))
                  ;; the first formula is edited: it is written anew
                  (let* ((p (tree-ref (buffer-tree) 2))
                         (f (list-find (tree-children p)
                                       (lambda (c) (tree-is? c 'md-source)))))
                    (tree-set (tree-ref f 0) '(math "z")))
                  (update-forced)
                  (export-buffer-main u to "markdown" (list :overwrite))
                  (check= (string-load to)
                          (string-replace
                            (string-replace markdown-file "Some *text*"
                                            "Other *text*")
                            "$a=b$" "$z$"))))
        (when (and (pair? r) (== (car r) 'error))
          (check-report #f "the group" (object->string r))))
      (if (url-exists? to) (system-remove to))
      (if (url-exists? from) (system-remove from))
      (system-rmdir dir)
      (buffer-pretend-saved u)
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; After the fourth audit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What the parser read otherwise than CommonMark does.
(define (test-audit-parse)
  (check-group "audit, parse")
  ;; an entity has digits only, and the entities of HTML 4 are known
  (check= (parse* "&#1e3; &#65.0; &#x41; &AElig; &alpha;")
          `(markdown ,(string-append "&#1e3; &#65.0; A " (bytes 195 134) " "
                                     (bytes 206 177))))
  ;; and those of HTML5, also of two characters, with their semicolon only
  (check= (parse* (string-append "&Dcaron; &HilbertSpace; &NotEqualTilde; "
                                 "&CounterClockwiseContourIntegral;"))
          `(markdown ,(string-append (bytes 196 142) " " (bytes 226 132 139) " "
                                     (bytes 226 137 130 204 184) " "
                                     (bytes 226 136 179))))
  (check= (parse* "&foobar; &Dcaron &amp x") '(markdown "&foobar; &Dcaron &amp x"))
  ;; the mark of byte order is not text
  (check= (parse* (string-append (bytes 239 187 191) "# T")) '(markdown (h1 "T")))
  (check= (cadr (md-tree (string-append (bytes 239 187 191)
                                        "---\ntitle: T\n---\n\nx")))
          '(markdown (meta (@ (source "title: T")) (title "T")) (p "x")))
  ;; many blocks after a line which starts with a tab
  (check= (length (cadr (md-tree (string-append tab-char "x\n\n"
                                                (apply string-append
                                                       (make-list 10000 "p\n\n"))))))
          10002)
  ;; a line without > or indentation does not continue code between fences
  (check= (parse* "> ```\n> code\nlazy")
          '(markdown (blockquote (pre "code")) (p "lazy")))
  (check= (parse* "- ```\n  a\nb") '(markdown (ul (li (pre "a"))) (p "b")))
  (check= (parse* "> ```\n> x\n> ```\nlazy")
          '(markdown (blockquote (pre "x")) (p "lazy")))
  (check= (parse* "> a\nlazy") '(markdown (blockquote (p "a lazy"))))
  ;; a blank line inside a list of an item, or inside code, does not make
  ;; the list of the item loose
  (check= (parse* "- a\n  - b\n\n    c\n- d")
          '(markdown (ul (li (p "a") (ul (@ (loose "true")) (li (p "b") (p "c"))))
                         (li (p "d")))))
  (check= (parse* "- a\n  ```\n  x\n\n  y\n  ```")
          '(markdown (ul (li (p "a") (pre "x\n\ny")))))
  ;; <pre>, <script>, <style> and <textarea> go on to their closing tag
  (check= (parse* "<script>\nvar a;\n\nvar b;\n</script>\n\nafter")
          '(markdown (html "<script>\nvar a;\n\nvar b;\n</script>") (p "after")))
  (check= (parse* "<pre>\nnever closed\n\ntext")
          '(markdown (html "<pre>\nnever closed") (p "text")))
  ;; a formula which does not end does not end the paragraph
  (check= (parse* "I paid\n$$5 for it") '(markdown "I paid $$5 for it"))
  (check= (parse* "text\n$$\nx\n$$") '(markdown (p "text") (displaymath "x")))
  ;; the address and the title of a definition on the next lines
  (check= (parse* "[foo]:\n/url\n\n[foo]") '(markdown (a (@ (href "/url")) "foo")))
  (check= (parse* "[foo]: /url\n\"T\"\n\n[foo]")
          '(markdown (a (@ (href "/url") (title "T")) "foo")))
  (check= (parse* "[foo]:\n/url\n'the title'\n\n[foo]")
          '(markdown (a (@ (href "/url") (title "the title")) "foo")))
  (check= (parse* "[foo]: /url\nnot a title\n\n[foo]")
          '(markdown (p "not a title") (p (a (@ (href "/url")) "foo"))))
  ;; an image with a style is HTML
  (check= (parse* "a <img src=\"i.png\" style=\"float: right\"> b")
          '(markdown "a " (html "<img src=\"i.png\" style=\"float: right\">")
                     " b")))

;; What the serializer wrote which was read back otherwise.
(define (test-audit-serialize)
  (check-group "audit, serialize")
  (let ((rt (lambda (t) (parse* (write-md t)))))
    ;; HTML after a list does not go into its last item
    (with s "<td>\n\n  * a\n  * b\n\n  </td>\n</tr>"
      (check= (rt (parse s)) (parse* s)))
    ;; emphasis inside emphasis of its kind is that emphasis
    (check= (write-md '(markdown (em (em "a")))) "*a*")
    (check= (rt '(markdown (em "a" (em "b")))) '(markdown (em "ab")))
    (check= (write-md '(markdown (p (del (del "a"))))) "~~a~~\n")
    (check= (rt '(markdown (em "a" (strong "b" (em "c")) "d")))
            '(markdown (em "a" (strong "bc") "d")))
    ;; a paragraph which starts with a tab
    (check= (write-md `(markdown (p ,(string-append tab-char "tab")))) "tab\n")
    ;; words after the fence with a backquote: a fence of tildes
    (check= (rt '(markdown (pre (@ (lang "a`b")) "x\n~~~")))
            '(markdown (pre (@ (lang "a`b")) "x\n~~~")))
    ;; backslashes and entities in an address and in a title
    (for-each (lambda (u)
                (with t `(markdown (a (@ (href ,u) (title "t\\")) "t"))
                  (check= (rt t) t)))
              '("u\\*v" "u&amp;v" "<u>" "a\\"))))

;; What the import and the export lost.
(define (test-audit-convert)
  (check-group "audit, convert")
  (let ((rsquo (bytes 226 128 153))
        (lsquo (bytes 226 128 152)))
    ;; the quotes of typography are themselves, in the text and in code
    (check= (import (string-append "a" rsquo "b" lsquo "c " (bytes 194 181)))
            "a<#2019>b<#2018>c <#B5>")
    (for-each (lambda (s) (check= (md->md s) s))
              (list (string-append "a " rsquo " b " lsquo " c\n")
                    (string-append "```\n'" rsquo "'\n```\n")
                    (string-append "`" rsquo "` [a](http://x/" rsquo ")\n")
                    (string-append "| a |\n| - |\n| " rsquo " |\n"))))
  ;; a list which cannot interrupt the paragraph of an item
  (check= (import (export '(itemize (document (concat (item) "a")
                                              (itemize (document (item)))))))
          '(itemize (document (concat (item) "a")
                              (itemize (document (concat (item) ""))))))
  (with t '(itemize (document (concat (item) "a")
                              (enumerate (document (concat (assign "item-nr" "1")
                                                           (item) "b")))))
    (check= (import (export t)) t))
  ;; emphasis which starts or ends with code, a link or a formula, next to
  ;; a letter
  (for-each (lambda (t) (check= (import (export t)) t))
            `((concat "the" (em (verbatim "foo")) "s")
              (concat "see " (em (hlink "x" "u")) "s")
              (concat "word" (strong (math "x")))
              (concat ,(string-append "caf" (bytes 233)) (em (verbatim "x"))
                      ,(bytes 233))))
  (check= (export '(concat "the" (em (verbatim "foo")) "s"))
          "th&#101;*`foo`*&#115;")
  ;; [a] in the text, with a definition of a which the file keeps
  (with s "Literal \\[a] text\n\n[a]: http://u\n"
    (check= (md->md s) "Literal [a\\] text\n\n[a]: http://u\n"))
  ;; many emphasized texts in a row are merged at once
  (check= (export `(concat ,@(make-list 20000 '(em "b"))))
          (string-append "*" (make-string 20000 #\b) "*")))

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
  (test-parse-long)
  (test-parse-math)
  (test-parse-blocks)
  (test-parse-lists)
  (test-parse-tables)
  (test-serialize)
  (test-serialize-escapes)
  (test-import-inline)
  (test-import-figures)
  (test-import-image-sizes)
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
  (test-long)
  (test-round-trips)
  (test-export-buffer)
  (test-style-import)
  (test-style-round-trip)
  (test-style-losses)
  (test-style-losses-2)
  (test-style-tabs)
  (test-style-buffer)
  (test-audit-parse)
  (test-audit-serialize)
  (test-audit-convert)
  (test-format)
  (check-end))
