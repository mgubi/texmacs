;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : convert-more-test.scm
;; DESCRIPTION : tests of the conversions not covered by formats-test
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The conversions which latex-test, formats-test and the old regtests
;; (htmltm, xmltm, tmhtml, tmmltm, mathtm, ...) leave out:
;;
;;   - MathML: the export texmacs->mathml (tmmath.scm), MathML inside HTML
;;     (texmacs->html:mathml), the import of MathML in HTML (mathtm.scm) and
;;     the round trip;
;;   - the XML parser (parse-xml) on entities, CDATA, comments, processing
;;     instructions and namespaces, and the XML names and character data of
;;     TMML (tm->xml-name, tm->xml-cdata, ...);
;;   - the encodings of the C++ converter tables (converter.cpp): Cork <->
;;     UTF-8 on the whole Cork table and on the symbols of TeXmacs (Greek,
;;     mathematics, characters without a Cork form written <#xxxx>), the
;;     HTML entities, the LaTeX accents, T2A for Cyrillic, iconv, the source
;;     code encoding and the guess of the encoding of a text;
;;   - the images: the format of a file, the size of small generated PNG,
;;     GIF, JPEG, PNM, SVG and EPS files (image->psdoc), the converters of
;;     init-images.scm, and those which need an external tool when it is
;;     there (ps2pdf, rsvg-convert);
;;   - the source code formats of the plugins (cpp, julia, java, scala,
;;     json, csv, python, scilab, mathemagix, caas, scheme) and the registry
;;     of all the formats;
;;   - the C++ parsers of Coq vernacular and JSON, and the compressed trees;
;;   - the serializations (tm, stm, tmml, stree) of the control characters
;;     and of a document of 2000 paragraphs, each in less than 2 s.
;;
;; Strings of TeXmacs trees are Cork-encoded: "\xe9" is e acute in Cork,
;; "\xc3\xa9" the same in UTF-8. Expected UTF-8 strings are built by u8
;; from their code points. The files go to the directory convert-more of
;; the temporary directory, removed at the start and at the end.

(texmacs-module (check convert-more-test)
  (:use (check check-lib)
        (convert mathml tmmath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (st x) (if (tree? x) (tree->stree x) x))

(define (export s fm . opts)
  (apply convert (cons* (stree->tree s) "texmacs-tree" fm opts)))

(define (import s fm . opts)
  (st (apply convert (cons* s fm "texmacs-tree" opts))))

(define (tmfile body)
  `(document (TeXmacs ,(texmacs-version)) (style "generic") (body ,body)))

(define (u8-1 n)
  ;; the UTF-8 encoding of the code point @n
  (define (b x) (integer->char x))
  (define (cont x) (b (+ #x80 (remainder x 64))))
  (cond ((< n #x80) (string (b n)))
        ((< n #x800) (string (b (+ #xC0 (quotient n 64))) (cont n)))
        ((< n #x10000)
         (string (b (+ #xE0 (quotient n 4096))) (cont (quotient n 64)) (cont n)))
        (else
         (string (b (+ #xF0 (quotient n 262144))) (cont (quotient n 4096))
                 (cont (quotient n 64)) (cont n)))))

(define (u8 . l) (apply string-append (map u8-1 l)))

(define (cork . l) (list->string (map integer->char l)))

(define (temp-dir) (url-append (url-temp-dir) "convert-more"))

(define (run-group thunk)
  ;; an error in a group counts as one failure and does not stop the suite
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(define (read-table name)
  ;; the entries of the encoding table langs/encoding/@name.scm
  (let* ((u (string->url (string-append "$TEXMACS_PATH/langs/encoding/"
                                        name ".scm")))
         (port (open-input-string (string-load u))))
    (let loop ((acc '()))
      (with x (read port)
        (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))

(define (hex-code s)
  ;; "#3B1" -> 945, #f for anything else ("#25#18", "<alpha>")
  (and (string? s) (string-starts? s "#") (> (string-length s) 1)
       (with h (substring s 1 (string-length s))
         (and (not (string-index h #\#)) (string->number h 16)))))

(define (ms . l)
  ;; MathML in HTML, as the export writes it
  (string-append "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
                 (apply string-append l) "</math>"))

(define (html-math s)
  (export s "html-snippet" (cons "texmacs->html:mathml" "on")))

(define (mathml-import . l)
  (import (apply ms l) "html-snippet"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MathML export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; texmacs->mathml takes math content and gives a MathML tree with the m:
;; prefix: identifiers mi, numbers mn, operators mo (the symbols in UTF-8),
;; scripts msub/msup/msubsup/mmultiscripts, mfrac, msqrt/mroot, accents
;; mover/munder, tables mtable, and with mstyle.
(define (test-mathml-export)
  (check-group "mathml export")
  (check= (texmacs->mathml "x") '(m:mi "x"))
  (check= (texmacs->mathml "x+1") '(m:mrow (m:mi "x") (m:mo "+") (m:mn "1")))
  (check= (texmacs->mathml "123") '(m:mn "123"))
  (check= (texmacs->mathml "") '(m:mrow))
  (check= (texmacs->mathml '(concat "sin" "x")) '(m:mrow (m:mi "sin") (m:mi "x")))
  ;; FIXME: a decimal number is cut at the point, which becomes an
  ;; identifier (tmconcat-math-sub in tmconcat.scm:71 only eats digits):
  ;; (texmacs->mathml "12.5") gives (m:mrow (m:mn "12") (m:mi ".") (m:mn "5")),
  ;; expected (m:mn "12.5").
  (check= (texmacs->mathml '(concat "x" (rsup "2"))) '(m:msup (m:mi "x") (m:mn "2")))
  (check= (texmacs->mathml '(concat "x" (rsub "i"))) '(m:msub (m:mi "x") (m:mi "i")))
  (check= (texmacs->mathml '(concat "x" (rsub "i") (rsup "2")))
          '(m:msubsup (m:mi "x") (m:mi "i") (m:mn "2")))
  (check= (texmacs->mathml '(concat (lsub "a") "x"))
          '(m:mmultiscripts (m:mi "x") (m:mprescripts) (m:mi "a") (m:none)))
  (check= (texmacs->mathml '(concat "x" (rprime "'"))) '(m:msup (m:mi "x") (m:mi "'")))
  (check= (texmacs->mathml '(frac "a" "b")) '(m:mfrac (m:mi "a") (m:mi "b")))
  (check= (texmacs->mathml '(sqrt "x")) '(m:msqrt (m:mi "x")))
  (check= (texmacs->mathml '(sqrt "x" "3")) '(m:mroot (m:mi "x") (m:mn "3")))
  (check= (texmacs->mathml '(frac (concat "x" (rsup "2")) (sqrt "y")))
          '(m:mfrac (m:msup (m:mi "x") (m:mn "2")) (m:msqrt (m:mi "y"))))
  ;; the symbols: letters are identifiers, relations and operators mo
  (check= (texmacs->mathml "<alpha>+<beta>")
          `(m:mrow (m:mi ,(u8 #x3B1)) (m:mo "+") (m:mi ,(u8 #x3B2))))
  (check= (texmacs->mathml "<leq><infty>")
          `(m:mrow (m:mo ,(u8 #x2264)) (m:mi ,(u8 #x221E))))
  (check= (texmacs->mathml "<cdot>") `(m:mo ,(u8 #x22C5)))
  (check= (texmacs->mathml "<#2A01>") `(m:mi ,(u8 #x2A01)))
  (check= (texmacs->mathml "<mathe>") '(m:mn "e"))
  (check= (texmacs->mathml "<up-d>") '(m:mo "d"))
  ;; the big operators and their limits
  (check= (texmacs->mathml '(concat "<sum>" (rsub "i") (rsup "n")))
          `(m:msubsup (m:mo ,(u8 #x2211)) (m:mi "i") (m:mi "n")))
  (check= (texmacs->mathml '(concat "a" (big "sum") "b"))
          '(m:mrow (m:mi "a") (m:mo "&Sum;") (m:mi "b")))
  (check= (texmacs->mathml '(big-around "<sum>" "x"))
          '(m:mrow (m:mo "&Sum;") (m:mi "x")))
  ;; brackets
  (check= (texmacs->mathml '(concat (left "(") "x" (right ")")))
          '(m:mrow (m:mo (@ (form "prefix")) "(") (m:mi "x")
                   (m:mo (@ (form "postfix")) ")")))
  (check= (texmacs->mathml '(around "(" "x" ")"))
          '(m:mrow (m:mo "(") (m:mi "x") (m:mo ")")))
  (check= (texmacs->mathml '(surround "(" ")" "x"))
          '(m:mrow (m:mo "(") (m:mi "x") (m:mo ")")))
  ;; accents, above, below, negation, arrows with text
  (check= (texmacs->mathml '(wide "x" "^")) '(m:mover (m:mi "x") (m:mo "&Hat;")))
  (check= (texmacs->mathml '(wide "x" "<bar>"))
          '(m:mover (m:mi "x") (m:mo "&OverBar;")))
  (check= (texmacs->mathml '(wide* "x" "<bar>"))
          '(m:munder (m:mi "x") (m:mo "&OverBar;")))
  (check= (texmacs->mathml '(above "x" "y")) '(m:mover (m:mi "x") (m:mi "y")))
  (check= (texmacs->mathml '(below "x" "y")) '(m:munder (m:mi "x") (m:mi "y")))
  (check= (texmacs->mathml '(neg "="))
          '(m:menclose (@ (notation "updiagonalstrike")) (m:mo "=")))
  (with arrow `(m:mo (@ (stretchy "true")) ,(u8 #x2192))
    (check= (texmacs->mathml '(long-arrow "<rubber-rightarrow>" "f"))
            `(m:mover ,arrow (m:mi "f")))
    (check= (texmacs->mathml '(long-arrow "<rubber-rightarrow>" "" "g"))
            `(m:munder ,arrow (m:mi "g")))
    (check= (texmacs->mathml '(long-arrow "<rubber-rightarrow>" "f" "g"))
            `(m:munderover ,arrow (m:mi "g") (m:mi "f"))))
  ;; tables: the column alignments come from the cwith
  (check= (texmacs->mathml '(tformat (cwith "1" "-1" "1" "1" "cell-halign" "r")
                                     (table (row (cell "a") (cell "b")))))
          '(m:mtable (@ (columnalign "right left"))
                     (m:mtr (m:mtd (m:mi "a")) (m:mtd (m:mi "b")))))
  (check= (texmacs->mathml '(tformat (table (row (cell "a")) (row (cell "1")))))
          '(m:mtable (@ (columnalign "left"))
                     (m:mtr (m:mtd (m:mi "a"))) (m:mtr (m:mtd (m:mn "1")))))
  ;; FIXME: a table without tformat raises an error (tmmath-table in
  ;; tmmath.scm:238 gives '() as row and cell formats, of which
  ;; tmmath-make-rows takes the car, and wraps the table in a list):
  ;; (texmacs->mathml '(table (row (cell "a")))) raises wrong-type-arg,
  ;; expected (m:mtable (@ (columnalign "left")) (m:mtr (m:mtd (m:mi "a")))).
  ;; with: the color, the series, the text mode
  (check= (texmacs->mathml '(with "color" "red" "x"))
          '(m:mstyle (@ (mathcolor "red")) (m:mi "x")))
  (check= (texmacs->mathml '(with "math-font-series" "bold" "x"))
          '(m:mstyle (@ (mathvariant "bold")) (m:mi "x")))
  (check= (texmacs->mathml '(with "math-display" "true" "x"))
          '(m:mstyle (@ (displaystyle "true")) (m:mi "x")))
  (check= (texmacs->mathml '(with "mode" "text" "if")) '(m:mtext "if"))
  ;; the spaces at the ends of a text are kept by no-break spaces
  (check= (texmacs->mathml '(with "mode" "text" "hi there "))
          '(m:mtext "hi there &#xA0;"))
  (check= (texmacs->mathml '(with "mode" "text" " a")) '(m:mtext "&#xA0; a"))
  (check= (texmacs->mathml '(rigid "x")) '(m:mrow (m:mi "x")))
  (check= (texmacs->mathml '(document "a" "b")) '(m:mrow (m:mi "a") (m:mi "b")))
  ;; the tags which are not primitives give nothing
  (check= (texmacs->mathml '(foo "x")) "")
  (check= (texmacs->mathml '(text "ab")) ""))

;; With texmacs->html:mathml, the mathematics of an HTML export is MathML in
;; the namespace of MathML, inline in the text.
(define (test-mathml-html)
  (check-group "mathml in html")
  (check= (html-math '(with "mode" "math" (frac "a" "b")))
          (ms "<mfrac><mi>a</mi><mi>b</mi></mfrac>"))
  (check= (html-math '(concat "Let " (with "mode" "math" "x") " be."))
          (string-append "Let " (ms "<mi>x</mi>") " be."))
  (check= (html-math '(with "mode" "math" (concat "<alpha><leq>2" (rsub "i"))))
          (ms "<mrow><mi>" (u8 #x3B1) "</mi><mo>" (u8 #x2264)
              "</mo><msub><mn>2</mn><mi>i</mi></msub></mrow>"))
  (check= (html-math '(with "mode" "math"
                        (concat "x" (rsup "2") "+" (sqrt "y" "3"))))
          (ms "<mrow><msup><mi>x</mi><mn>2</mn></msup><mo>+</mo>"
              "<mroot><mi>y</mi><mn>3</mn></mroot></mrow>"))
  (check= (html-math '(with "mode" "math" (tformat (table (row (cell "a") (cell "b"))))))
          (ms "<mtable columnalign=\"left left\">\n  <mtr>\n"
              "    <mtd><mi>a</mi></mtd>\n    <mtd><mi>b</mi></mtd>\n"
              "  </mtr>\n</mtable>"))
  ;; without the option, there is no MathML
  (check-false (string-occurs? "<math" (export '(with "mode" "math" (frac "a" "b"))
                                               "html-snippet"
                                               (cons "texmacs->html:mathml" "off"))))
  (with s (export (tmfile '(document (with "mode" "math" (frac "a" "b"))))
                  "html-document" (cons "texmacs->html:mathml" "on"))
    (check-true (string-occurs? (ms "<mfrac><mi>a</mi><mi>b</mi></mfrac>") s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MathML import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MathML in an HTML document, in the namespace of MathML (as TeXmacs and
;; most tools write it), becomes a math tag, or an equation* with
;; display="block".
;; FIXME: MathML without the xmlns attribute, as HTML5 allows it, loses its
;; first element and makes a paragraph of its own (htmltm-math in
;; htmltm.scm:363 puts the list of the children as one child,
;; `,(replace-nsprefix-in-stree c ...)` instead of `,@`, so that the first
;; child becomes the name of a node, and the handler of h:math is :block):
;; (convert "<math><mi>x</mi><mo>+</mo><mn>1</mn></math>" "html-snippet"
;; "texmacs-tree") gives (math "+1"), expected (math "x+1"); and
;; "<p>Let <math><mi>x</mi></math> be.</p>" gives
;; (document "Let" (math "") "be."), expected (concat "Let " (math "x") " be.").
;; FIXME: merror raises an error, a typo in mathtm.scm:154 (matthtm-error):
;; (mathml-import "<merror><mi>x</mi></merror>") raises unbound-variable,
;; expected (math (with "color" "red" "x")).
(define (test-mathml-import)
  (check-group "mathml import")
  (check= (mathml-import "<mi>x</mi>") '(math "x"))
  (check= (mathml-import "<mi>x</mi><mo>+</mo><mn>1</mn>") '(math "x+1"))
  (check= (mathml-import "<mrow><mi>x</mi><mo>=</mo><mn>12.5</mn></mrow>")
          '(math "x=12.5"))
  (check= (mathml-import "<msup><mi>x</mi><mn>2</mn></msup>")
          '(math (concat "x" (rsup "2"))))
  (check= (mathml-import "<msub><mi>x</mi><mi>i</mi></msub>")
          '(math (concat "x" (rsub "i"))))
  (check= (mathml-import "<msubsup><mi>x</mi><mi>i</mi><mn>2</mn></msubsup>")
          '(math (concat "x" (rsub "i") (rsup "2"))))
  (check= (mathml-import "<mmultiscripts><mi>x</mi><mi>a</mi><mi>b</mi>"
                         "<mprescripts/><mi>c</mi><mi>d</mi></mmultiscripts>")
          '(math (concat (lsup "d") (lsub "c") "x" (rsub "a") (rsup "b"))))
  (check= (mathml-import "<mfrac><mi>a</mi><mi>b</mi></mfrac>") '(math (frac "a" "b")))
  ;; a fraction without a line is a stack
  (check= (mathml-import "<mfrac linethickness=\"0\"><mi>a</mi><mi>b</mi></mfrac>")
          '(math (stack (tformat (table (row (cell "a")) (row (cell "b")))))))
  (check= (mathml-import "<msqrt><mi>x</mi></msqrt>") '(math (sqrt "x")))
  (check= (mathml-import "<mroot><mi>x</mi><mn>3</mn></mroot>") '(math (sqrt "x" "3")))
  ;; the entities and the UTF-8 characters become TeXmacs symbols
  (check= (mathml-import "<mi>&alpha;</mi><mo>&le;</mo><mi>&#x3B2;</mi>")
          '(math "<alpha><leq><beta>"))
  (check= (mathml-import "<mi>" (u8 #x3B1) "</mi>") '(math "<alpha>"))
  (check= (mathml-import "<mo>&rarr;</mo><mo>&InvisibleTimes;</mo><mo>&infin;</mo>")
          '(math "<rightarrow>*<infty>"))
  (check= (mathml-import "<mi>&Ropf;</mi><mi>&Ascr;</mi><mi>&gfr;</mi>")
          '(math "<bbb-R><cal-A><frak-g>"))
  ;; the brackets
  (check= (mathml-import "<mo>(</mo><mi>x</mi><mo>)</mo>")
          '(math (around* "(" "x" ")")))
  (check= (mathml-import "<mrow><mo>(</mo><mi>x</mi><mo>)</mo></mrow>")
          '(math (around* "(" "x" ")")))
  (check= (mathml-import "<mfenced><mi>x</mi><mi>y</mi></mfenced>")
          '(math (around* "(" "xy" ")")))
  ;; under and over
  (check= (mathml-import "<munderover><mo>&sum;</mo><mi>i</mi><mi>n</mi></munderover>")
          '(math (concat (big "sum") (rsub "i") (rsup "n"))))
  (check= (mathml-import "<mover><mi>x</mi><mo>^</mo></mover>") '(math (wide "x" "^")))
  (check= (mathml-import "<mover><mi>x</mi><mo>&#xAF;</mo></mover>")
          '(math (wide "x" "<wide-bar>")))
  (check= (mathml-import "<munder><mi>x</mi><mo>&#x332;</mo></munder>")
          '(math (wide* "x" "<wide-bar>")))
  ;; text, style, enclosures, tables
  (check= (mathml-import "<mtext>if </mtext>") '(math (with "mode" "text" "if")))
  (check= (mathml-import "<ms>str</ms>") '(math (with "mode" "text" "str")))
  (check= (mathml-import "<mstyle mathcolor=\"red\"><mi>x</mi></mstyle>")
          '(math (with "color" "red" "x")))
  (check= (mathml-import "<mi mathvariant=\"bold\">x</mi>")
          '(math (with "math-font-series" "bold" "x")))
  (check= (mathml-import "<menclose notation=\"updiagonalstrike\"><mi>x</mi></menclose>")
          '(math (neg "x")))
  (check= (mathml-import "<mphantom><mi>x</mi></mphantom>") '(math (phantom "x")))
  (check= (mathml-import "<mtable><mtr><mtd><mi>a</mi></mtd><mtd><mi>b</mi></mtd></mtr>"
                         "<mtr><mtd><mi>c</mi></mtd><mtd><mi>d</mi></mtd></mtr></mtable>")
          '(math (tabular (table (row (cell "a") (cell "b"))
                                 (row (cell "c") (cell "d"))))))
  ;; semantics keeps the presentation, without the annotation
  (check= (mathml-import "<semantics><mi>x</mi><annotation encoding=\"TeX\">x"
                         "</annotation></semantics>")
          '(math "x"))
  (check= (mathml-import "<mspace width=\"1em\"/>") '(math ""))
  ;; display="block" makes an equation
  (check= (import (string-append "<math display=\"block\" xmlns=\"http://www.w3.org/"
                                 "1998/Math/MathML\"><mi>x</mi></math>")
                  "html-snippet")
          '(equation* "x"))
  ;; inline in a paragraph; and with a prefix of the namespace
  (check= (import (string-append "<p>Let " (ms "<mi>x</mi>") " be.</p>") "html-snippet")
          '(concat "Let " (math "x") " be."))
  (check= (import (string-append "<m:math xmlns:m=\"http://www.w3.org/1998/Math/"
                                 "MathML\"><m:mi>x</m:mi></m:math>")
                  "html-snippet")
          '(math "x")))

;; TeXmacs -> MathML in HTML -> TeXmacs: the formula comes back in a math
;; tag.
(define (test-mathml-round-trip)
  (check-group "mathml round trip")
  (for-each
   (lambda (x)
     (check= (import (html-math `(with "mode" "math" ,x)) "html-snippet")
             `(math ,x)))
   (list "x" "x+1" "<alpha><leq><beta>" '(frac "a" "b") '(sqrt "x")
         '(sqrt "x" "3") '(concat "x" (rsup "2"))
         '(concat "x" (rsub "i") (rsup "2"))
         '(concat "x" (rsup "2") "+" (frac "a" (sqrt "b")))))
  ;; the bar comes back as the wide bar
  (check= (import (html-math '(with "mode" "math" (wide "x" "<bar>"))) "html-snippet")
          '(math (wide "x" "<wide-bar>")))
  ;; FIXME: a hat does not come back: the export writes the accent as the
  ;; entity &Hat;, which the import reads as a symbol, not as an accent
  ;; (mathtm-mover in mathtm.scm:304 only knows the characters): (wide "x" "^")
  ;; gives (math (above "x" "<#005E>")), expected (math (wide "x" "^")).
  (check= (import (html-math '(concat "a " (with "mode" "math" "x") " b"))
                  "html-snippet")
          '(concat "a " (math "x") " b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse-xml gives an SXML tree under *TOP*: the entities of XML and the
;; character references are replaced, the other entities kept, the CDATA
;; sections are text, the comments are removed, the processing instructions
;; and the doctype are *PI* and *DOCTYPE* nodes, the prefixes of the
;; namespaces are kept as they are.
(define (test-xml-parser)
  (check-group "xml parser")
  (check= (parse-xml "") '(*TOP*))
  (check= (parse-xml "text only") '(*TOP* "text only"))
  (check= (parse-xml "<a/><b/>") '(*TOP* (a) (b)))
  (check= (parse-xml "<a>&lt;&gt;&quot;&apos;&amp;</a>") '(*TOP* (a "<>\"'&")))
  (check= (parse-xml "<a>&#65;&#x42;&#xE9;&#233;&#x1F600;</a>")
          `(*TOP* (a ,(string-append "AB" (u8 #xE9 #xE9 #x1F600)))))
  (check= (parse-xml "<a>&eacute;&alpha;&unknown;</a>")
          '(*TOP* (a "&eacute;&alpha;&unknown;")))
  (check= (parse-xml "<a>&amp</a>") '(*TOP* (a "&")))
  (check= (parse-xml "<!DOCTYPE a [<!ENTITY me \"Myself\">]><a>&me;</a>")
          '(*TOP* (*DOCTYPE* "a") (a "Myself")))
  (check= (parse-xml (string-append "<?xml version=\"1.0\"?><a x=\"1\">t&amp;u<b/>"
                                    "<!-- c --><![CDATA[<raw>&]]><?pi data?></a>"))
          '(*TOP* (*PI* xml "version=\"1.0\"")
                  (a (@ (x "1")) "t&u" (b) "<raw>&" (*PI* pi "data"))))
  (check= (parse-xml "<a><!-- x --><!--y-->b</a>") '(*TOP* (a "b")))
  (check= (parse-xml "<a><![CDATA[]]></a>") '(*TOP* (a "")))
  (check= (parse-xml "<a><![CDATA[x]]>y<![CDATA[z]]></a>") '(*TOP* (a "x" "y" "z")))
  (check= (parse-xml "<?pi?><a/>") '(*TOP* (*PI* pi "") (a)))
  (check= (parse-xml "<a x=\"&lt;&amp;&quot;\" y='a\"b'/>")
          '(*TOP* (a (@ (x "<&\"") (y "a\"b")))))
  (check= (parse-xml "<a  b = 'single' c=\"d\" >  x  </a>")
          '(*TOP* (a (@ (b "single") (c "d")) "  x  ")))
  (check= (parse-xml "<n:a xmlns:n=\"urn:x\"><n:b n:c=\"d\"/></n:a>")
          '(*TOP* (n:a (@ (xmlns:n "urn:x")) (n:b (@ (n:c "d"))))))
  (check= (parse-xml (string-append "<!DOCTYPE html><a>" (u8 #xE9) "</a>"))
          `(*TOP* (*DOCTYPE* "html") (a ,(u8 #xE9))))
  ;; the parser is lenient: an unclosed element is closed
  (check= (parse-xml "<a><b>unclosed</a>") '(*TOP* (a (b "unclosed"))))
  (check= (parse-xml "<a xml:space=\"preserve\"> </a>")
          '(*TOP* (a (@ (xml:space "preserve")) " ")))
  ;; FIXME: the HTML parser does not read script as raw text (parsehtml.cpp
  ;; and parsexml.cpp have no raw text elements): (parse-html
  ;; "<script>if (a<b) x;</script>") gives (*TOP* (script "if (a" (b (@ ...)))),
  ;; expected (*TOP* (script "if (a<b) x;")); the import drops scripts anyway.
  (check= (import "<script>if (a<b) x;</script><p>after</p>" "html-snippet") "after"))

;; The names of the TeXmacs tags and the strings of TeXmacs in XML (TMML):
;; a character which is not allowed in an XML name, and _, is written
;; _code_; the symbols are written in UTF-8 when they come back as the same
;; symbol, else as tm-sym elements.
(define (test-xml-names)
  (check-group "xml names and cdata")
  (check= (tm->xml-name "equation*") "equation_42_")
  (check= (tm->xml-name "a_b") "a_95_b")
  (check= (tm->xml-name "1x") "_49_x")
  (check= (tm->xml-name "-x.y:z") "_45_x.y:z")
  (check= (tm->xml-name "a b") "a_32_b")
  (check= (tm->xml-name "foo-bar") "foo-bar")
  (for-each (lambda (s) (check= (xml-name->tm (tm->xml-name s)) s))
            '("equation*" "a_b" "1x" "-x.y:z" "a b" "foo-bar" "x<y>" "_"))
  (check= (old-tm->xml-cdata "a&b>c<alpha>d") "a&amp;b&gt;c&alpha;d")
  (check= (old-xml-cdata->tm "a&amp;b&gt;c&alpha;d<e") "a&b<gt>c<alpha>d<less>e")
  (check= (tm->xml-cdata "abc") "abc")
  (check= (tm->xml-cdata "") "")
  (check= (tm->xml-cdata "a&b>c") "a&amp;b&gt;c")
  (check= (tm->xml-cdata "\xe9<alpha>") (u8 #xE9 #x3B1))
  (check= (tm->xml-cdata "x<less>y<gtr>")
          '(!concat "x" (tm-sym "less") "y" (tm-sym "gtr")))
  (check= (tm->xml-cdata "x<foo>y") '(!concat "x" (tm-sym "foo") "y"))
  ;; <#3B1> is written as a symbol, since alpha comes back as <alpha>
  (check= (tm->xml-cdata "<#3B1>") '(tm-sym "#3B1"))
  (check= (tm->xml-cdata "<#1F600>") (u8 #x1F600))
  (check= (xml-unspace "  a   b  " #t #t) "a b")
  (check= (xml-unspace "  a   b  " #f #f) " a b ")
  (check= (xml-unspace "  a \n\t b  " #t #f) "a b ")
  (check= (xml-unspace "  a   b  " #f #t) " a b"))

;; TMML with the names and characters above
(define (test-tmml-special)
  (check-group "tmml special")
  (check= (export '(my_tag "y") "tmml-snippet") "<my_95_tag>y</my_95_tag>")
  (check= (export '(foo-bar "y") "tmml-snippet") "<foo-bar>y</foo-bar>")
  (check= (export '(with "my_var" "1" "y") "tmml-snippet")
          "<with my_95_var=\"1\">y</with>")
  (check= (export "<#3B1><#1F600><foo>" "tmml-snippet")
          (string-append "<tm-sym>#3B1</tm-sym>" (u8 #x1F600) "<tm-sym>foo</tm-sym>"))
  (check= (export (cork #x10 #x11 #x15 #x16 #x1c) "tmml-snippet")
          (u8 #x201C #x201D #x2013 #x2014 #xFB01))
  (let ((doc (tmfile `(document (foo-bar "x") (my_tag "y") (a.b "z")
                                (equation* (document "w"))
                                "<#3B1><#1F600><foo>\x10\x11\x15\x16\x1c"
                                "a & b <less> c <gtr> d \"q\" 'r'"
                                ,(string-append "a\x09" "b\x0d\x7f\xff")))))
    (check= (import (export doc "tmml-document") "tmml-document") doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encodings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Cork table (corktounicode) is used both ways: every entry of a Cork
;; byte and a code point gives the code point in UTF-8 and back.
(define (test-cork-table)
  (check-group "cork table")
  (let* ((t (read-table "corktounicode"))
         (l (list-filter t (lambda (e) (and (list? e) (= (length e) 2)
                                            (hex-code (car e)) (hex-code (cadr e))
                                            (< (hex-code (car e)) 256))))))
    (check-true (>= (length l) 250))
    (check= (list-filter
             (map (lambda (e)
                    (let ((c (cork (hex-code (car e)))) (u (u8 (hex-code (cadr e)))))
                      (and (!= (cork->utf8 c) u) e)))
                  l)
             identity)
            '())
    (check= (list-filter
             (map (lambda (e)
                    (let ((c (cork (hex-code (car e)))) (u (u8 (hex-code (cadr e)))))
                      (and (!= (utf8->cork u) c) e)))
                  l)
             identity)
            '()))
  ;; the symbols of tmuniversaltounicode: all give their code point, but
  ;; <mu> which is the Greek letter (the micro sign comes back as <mu>);
  ;; all come back but those which have a Cork byte
  (let* ((t (read-table "tmuniversaltounicode"))
         (l (list-filter t (lambda (e) (and (list? e) (= (length e) 2)
                                            (string? (car e))
                                            (string-starts? (car e) "<")
                                            (hex-code (cadr e)))))))
    (check-true (>= (length l) 900))
    (check= (list-filter
             (map (lambda (e)
                    (and (!= (cork->utf8 (car e)) (u8 (hex-code (cadr e)))) e))
                  l)
             identity)
            '(("<mu>" "#B5")))
    (check= (list-filter
             (map (lambda (e)
                    (and (!= (utf8->cork (u8 (hex-code (cadr e)))) (car e))
                         (list (car e) (utf8->cork (u8 (hex-code (cadr e)))))))
                  l)
             identity)
            `(("<sterling>" ,(cork #xbf)) ("<guillemotleft>" ,(cork #x13))
              ("<guillemotright>" ,(cork #x14))))))

(define greek
  '(("alpha" #x3B1) ("beta" #x3B2) ("gamma" #x3B3) ("delta" #x3B4)
    ("varepsilon" #x3B5) ("zeta" #x3B6) ("eta" #x3B7) ("theta" #x3B8)
    ("iota" #x3B9) ("kappa" #x3BA) ("lambda" #x3BB) ("mu" #x3BC)
    ("nu" #x3BD) ("xi" #x3BE) ("omicron" #x3BF) ("pi" #x3C0) ("rho" #x3C1)
    ("varsigma" #x3C2) ("sigma" #x3C3) ("tau" #x3C4) ("upsilon" #x3C5)
    ("varphi" #x3C6) ("chi" #x3C7) ("psi" #x3C8) ("omega" #x3C9)
    ("Gamma" #x393) ("Delta" #x394) ("Theta" #x398) ("Lambda" #x39B)
    ("Xi" #x39E) ("Pi" #x3A0) ("Sigma" #x3A3) ("Phi" #x3A6) ("Psi" #x3A8)
    ("Omega" #x3A9) ("epsilon" #x3F5) ("phi" #x3D5) ("vartheta" #x3D1)))

(define math-symbols
  '(("leq" #x2264) ("geq" #x2265) ("neq" #x2260) ("infty" #x221E)
    ("rightarrow" #x2192) ("leftarrow" #x2190) ("Rightarrow" #x21D2)
    ("mapsto" #x21A6) ("forall" #x2200) ("exists" #x2203)
    ("partial" #x2202) ("nabla" #x2207) ("in" #x2208) ("emptyset" #x2205)
    ("cap" #x2229) ("cup" #x222A) ("wedge" #x2227) ("vee" #x2228)
    ("approx" #x2248) ("equiv" #x2261) ("subset" #x2282) ("circ" #x2218)
    ("cdot" #x22C5) ("cdots" #x22EF) ("ldots" #x2026) ("langle" #x27E8)
    ("rangle" #x27E9) ("aleph" #x2135) ("ell" #x2113) ("bullet" #x2022)
    ("times" #xD7) ("pm" #xB1) ("neg" #xAC)))

;; The symbols of TeXmacs to UTF-8 and back; one way only: the big
;; operators come back as <big-...>, the symbols which have a variant as
;; the variant.
(define (test-cork-symbols)
  (check-group "cork symbols")
  (for-each (lambda (e)
              (let ((sym (string-append "<" (car e) ">")))
                (check= (cork->utf8 sym) (u8 (cadr e)))
                (when (!= (car e) "mu")
                  (check= (utf8->cork (u8 (cadr e))) sym))))
            (append greek math-symbols))
  (check= (utf8->cork (u8 #xB5)) "<mu>")
  (check= (cork->utf8 "<sum><prod><int>") (u8 #x2211 #x220F #x222B))
  (check= (utf8->cork (u8 #x2211 #x220F #x222B)) "<big-sum><big-prod><big-int>")
  (check= (cork->utf8 "<to><notin><perp><hbar><oplus><otimes>")
          (u8 #x2192 #x2209 #x22A5 #x210F #x2295 #x2297))
  (check= (utf8->cork (u8 #x2192)) "<rightarrow>")
  (check= (utf8->cork (u8 #x211D #x1D538)) "<bbb-R><bbb-A>")
  (check= (cork->utf8 "<less><gtr>") "<>")
  (check= (utf8->cork "<>&") "<less><gtr>&")
  ;; an unknown symbol is kept
  (check= (cork->utf8 "<unknownsym>") "<unknownsym>"))

;; Cork bytes: the accented Latin letters are those of Latin-1 where they
;; are there, the others (0x80-0xBF, 0x00-0x1F) are the T1 encoding of TeX.
(define (test-cork-bytes)
  (check-group "cork bytes")
  (check= (cork->utf8 "\xe9\xe0\xef\xc7") (u8 #xE9 #xE0 #xEF #xC7))
  ;; 0xDF is the capital sharp s, written SS, and 0xFF the sharp s
  (check= (cork->utf8 "\xdf\xff") (string-append "SS" (u8 #xDF)))
  (check= (cork->utf8 "\x80\x8a\x9a\xa0\xae\xbf\xd0\xdd\xde\xf0\xfe")
          (u8 #x102 #x141 #x17D #x103 #x151 #xA3 #xD0 #xDD #xDE #xF0 #xFE))
  (check= (cork->utf8 (cork 0 1 2 3 4 5 6 7 8 9 10 11 12))
          (u8 #x60 #xB4 #x2C6 #x2DC #xA8 #x2DD #x2DA #x2C7 #x2D8 #xAF #x2D9
              #xB8 #x2DB))
  (check= (cork->utf8 (cork #x0d #x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17))
          (u8 #x201A #x2039 #x203A #x201C #x201D #x201E #xAB #xBB #x2013
              #x2014 #x2060))
  ;; 0x18 (zero of the per mille), 0x1A (dotless j) are one way
  (check= (cork->utf8 (cork #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f #x7f))
          (string-append "0" (u8 #x131) "j" (u8 #xFB00 #xFB01 #xFB02 #xFB03
                                              #xFB04 #x2010)))
  (check= (utf8->cork (u8 #xE9 #xE0 #xDF #xFF)) "\xe9\xe0\xff\xb8")
  (check= (utf8->cork (u8 #x201C #x201D #x2018 #x2019)) (cork #x10 #x11 #x60 #x27))
  (check= (utf8->cork (u8 #xAB #xBB #xA7 #xA3)) (cork #x13 #x14 #x9f #xbf))
  (check= (utf8->cork (u8 #xA0)) "<varspace>")
  (check= (utf8->cork (u8 #xFB01)) (cork #x1c))
  (check= (utf8->cork (u8 #x153 #x152 #x131 #x142)) (cork #xf7 #xd7 #x19 #xaa))
  ;; the TeX ligatures of the quotes are made only by the backquote
  (check= (cork->utf8 "``x'' -- ---") (string-append (u8 #x2018 #x2018) "x'' -- ---"))
  ;; per mille
  (check= (cork->utf8 (cork #x25 #x18)) (u8 #x2030)))

;; Characters without a Cork form are written <#xxxx> (upper case
;; hexadecimal), and come back.
(define (test-cork-unicode)
  (check-group "cork unicode")
  (check= (utf8->cork (u8 #x416 #x44F #x430)) "<#416><#44F><#430>")
  (check= (cork->utf8 "<#416><#44F><#430>") (u8 #x416 #x44F #x430))
  (check= (utf8->cork (u8 #x10D3)) "<#10D3>")
  (check= (utf8->cork (u8 #x1F600)) "<#1F600>")
  (check= (utf8->cork (u8 #x4E2D #x6587)) "<#4E2D><#6587>")
  (check= (cork->utf8 "<#3B1><#10D3><#1F600>") (u8 #x3B1 #x10D3 #x1F600))
  (check= (cork->utf8 "<#3b1>") (u8 #x3B1))
  ;; <#3B1> gives alpha, which comes back as <alpha>
  (check= (utf8->cork (cork->utf8 "<#3B1>")) "<alpha>")
  (for-each (lambda (s) (check= (cork->utf8 (utf8->cork s)) s))
            (list (u8 #x416 #x430 #x431) (u8 #x5D0 #x5D1) (u8 #x627 #x644)
                  (u8 #x4E2D #x6587) (u8 #x1F600 #x1F680) (u8 #x10D3)
                  (string-append "caf" (u8 #xE9) " na" (u8 #xEF) "ve")))
  ;; bytes which are not UTF-8 are kept
  (check= (utf8->cork "\xff\xfe") "\xff\xfe")
  (check= (utf8->cork "\xc3") "\xc3")
  ;; string-convert with the Cork encoding
  (check= (string-convert "caf\xe9 <alpha>" "Cork" "UTF-8")
          (string-append "caf" (u8 #xE9) " " (u8 #x3B1)))
  (check= (string-convert (string-append "caf" (u8 #xE9) " " (u8 #x3B1)) "UTF-8" "Cork")
          "caf\xe9 <alpha>"))

;; The other encodings: HTML entities, LaTeX accents, T2A (Cyrillic), the
;; source code (UTF-8, but the TeX ligatures of ASCII kept), iconv.
(define (test-other-encodings)
  (check-group "other encodings")
  (check= (utf8->html (string-append (u8 #xE9) "<&>" (u8 #x3B1 #x20AC #x221E)))
          "&eacute;&lt;&amp;&gt;&alpha;&euro;&infin;")
  (check= (utf8->html (u8 #x10D3)) "&#x10D3;")
  (check= (utf8->html (u8 #xA0)) "&nbsp;")
  (check= (utf8->html "plain") "plain")
  (check= (html->utf8 "&eacute;&alpha;&euro;&amp;&lt;&nbsp;")
          (string-append (u8 #xE9 #x3B1 #x20AC) "&<" (u8 #xA0)))
  (check= (html->utf8 "&#xE9;") (u8 #xE9))
  (check= (html->utf8 "&bogus;") "&bogus;")
  ;; FIXME: html->utf8 only decodes the hexadecimal references of
  ;; U+0080-U+00FF (hex_entities_to_utf8 in converter.cpp:924), so that it
  ;; does not undo utf8->html: (html->utf8 "&#x10D3;") gives "&#x10D3;",
  ;; expected the UTF-8 of U+10D3; the decimal "&#233;" is not decoded either.
  ;; LaTeX
  (check= (string-convert (string-append "caf" (u8 #xE9) " " (u8 #xDF)) "UTF-8" "LaTeX")
          "caf{\\'e} {\\ss}")
  (check= (string-convert (u8 #xE7 #xF6 #xA7) "UTF-8" "LaTeX")
          "{\\c c}{\\\"o}{\\textsection}")
  (check= (string-convert "{\\'e}{\\ss}{\\c c}" "LaTeX" "UTF-8") (u8 #xE9 #xDF #xE7))
  (check= (string-convert "\\`{\\i}" "LaTeX" "UTF-8") (u8 #xEC))
  (check= (string-convert "plain" "UTF-8" "LaTeX") "plain")
  ;; T2A: the Cyrillic letters are bytes
  (check= (utf8->t2a (string-append (u8 #x416 #x44F #x430) " x"))
          (string-append (cork #xc6 #xff #xe0) " x"))
  (check= (t2a->utf8 (string-append (cork #xc6 #xff #xe0) " x"))
          (string-append (u8 #x416 #x44F #x430) " x"))
  ;; the source code keeps the ASCII ligatures
  (check= (cork->sourcecode "\xe9<alpha>-- ``x''")
          (string-append (u8 #xE9 #x3B1) "-- ``x''"))
  (check= (sourcecode->cork (string-append (u8 #xE9 #x3B1) "--``x''"))
          "\xe9<alpha>--``x''")
  ;; iconv
  (check= (string-convert "caf\xe9" "ISO-8859-1" "UTF-8") (string-append "caf" (u8 #xE9)))
  (check= (string-convert (string-append "caf" (u8 #xE9)) "UTF-8" "ISO-8859-1") "caf\xe9")
  (check= (string-convert "\xe9" "Cork" "ISO-8859-1") "\xe9")
  (check= (string-convert (u8 #x416) "UTF-8" "KOI8-R") (cork #xf6))
  ;; the guess of the encoding of a text
  (check= (guess-wencoding "abc") "ASCII")
  (check= (guess-wencoding (string-append "caf" (u8 #xE9))) "UTF-8")
  (check= (guess-wencoding (string-append (u8 #xFEFF) "caf" (u8 #xE9))) "UTF-8-BOM")
  (check= (guess-wencoding "caf\xe9") "ISO-8859")
  (check= (guess-wencoding (cork #x81 #x82 #x83)) "other")
  ;; ASCII escapes of Cork
  (check= (escape-to-ascii "\xe9\xe0<alpha>\x80x") "\\xe9\\xe0&lt;alpha&gt;\\x80x")
  ;; strings of TeXmacs characters
  (check= (tmstring->list "a\xe9<alpha><#3B1>b") '("a" "\xe9" "<alpha>" "<#3B1>" "b"))
  (check= (tmstring-length "a<alpha><#3B1>b") 4)
  (check= (downgrade-math-letters "<b-x><bbb-R><cal-A><frak-g>") "xRAg")
  (check= (encode-base64 "TeXmacs") "VGVYbWFjcw==")
  (check= (decode-base64 "VGVYbWFjcw==") "TeXmacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hex->string h)
  (let loop ((i 0) (acc '()))
    (if (>= i (string-length h)) (list->string (reverse acc))
        (loop (+ i 2)
              (cons (integer->char (string->number (substring h i (+ i 2)) 16))
                    acc)))))

;; small images made by ImageMagick (-strip): a red PNG of 30x20 pixels, a
;; blue GIF of 40x10, a green JPEG of 16x24
(define png-30x20
  (string-append
   "89504e470d0a1a0a0000000d494844520000001e000000140103000000a0651f87"
   "00000003504c5445ff000019e209370000000c4944415408d76360a03d00000064"
   "00015747236a0000000049454e44ae426082"))

(define gif-40x10
  (string-append
   "47494638396128000a00f000000000ff00000021f90400000000002c0000000028"
   "000a00000211848fa9cbed0fa39cb4da8bb3debcfbaf15003b"))

(define jpg-16x24
  (string-append
   "ffd8ffe000104a46494600010100000100010000ffdb004300100b0c0e0c0a100e"
   "0d0e1211101318281a181616183123251d283a333d3c3933383740485c4e404457"
   "453738506d51575f626768673e4d71797064785c656763ffdb0043011112121815"
   "182f1a1a2f6342384263636363636363636363636363636363636363636363636363"
   "6363636363636363636363636363636363636363636363636363ffc00011080018"
   "001003012200021101031101ffc4001500010100000000000000000000000000000005"
   "ffc40014100100000000000000000000000000000000ffc4001501010100000000"
   "000000000000000000000005ffc40014110100000000000000000000000000000000"
   "ffda000c03010002110311003f0088025a30003fffd9"))

(define (bounding-box ps)
  ;; the four numbers of the %%BoundingBox of the PostScript @ps
  (let ((i (string-search-forwards "%%BoundingBox:" 0 ps)))
    (and (>= i 0)
         (let* ((start (+ i 14))
                (end (or (string-index ps #\newline start) (string-length ps)))
                (l (string-tokenize-by-char (substring ps start end) #\space)))
           (map string->number (list-filter l (lambda (x) (!= x ""))))))))

;; The size of an image, in points (a pixel is a point when the file gives
;; no resolution), is the bounding box of its PostScript form (image->psdoc,
;; which uses Qt for the bitmaps); an EPS file gives its own bounding box.
(define (test-images)
  (check-group "images")
  (let* ((dir (temp-dir))
         (png (url-append dir "a.png")) (gif (url-append dir "a.gif"))
         (jpg (url-append dir "a.jpg")) (pnm (url-append dir "a.pnm"))
         (svg (url-append dir "a.svg")) (eps (url-append dir "a.eps")))
    (string-save (hex->string png-30x20) png)
    (string-save (hex->string gif-40x10) gif)
    (string-save (hex->string jpg-16x24) jpg)
    (string-save (string-append "P6\n3 2\n255\n" (make-string 18 (integer->char 200)))
                 pnm)
    (string-save (string-append
                  "<?xml version=\"1.0\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" "
                  "width=\"50pt\" height=\"25pt\" viewBox=\"0 0 50 25\">"
                  "<rect width=\"50\" height=\"25\" fill=\"red\"/></svg>\n")
                 svg)
    (string-save (string-append "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 10 20 110 70\n"
                                "newpath 10 20 moveto 110 70 lineto stroke\n"
                                "showpage\n%%EOF\n")
                 eps)
    (check= (url-size png) 84)
    (check= (map file-format (list png gif jpg pnm svg eps))
            '("png-file" "gif-file" "jpeg-file" "pnm-file" "svg-file"
              "postscript-file"))
    (check= (file-format (string->url "a.JPEG")) "jpeg-file")
    (check= (file-format (string->url "a.tiff")) "tif-file")
    (check-true (file-of-format? png "image"))
    (check-false (file-of-format? (string->url "a.tm") "image"))
    (check= (bounding-box (image->psdoc png)) '(0 0 30 20))
    (check= (bounding-box (image->psdoc gif)) '(0 0 40 10))
    (check= (bounding-box (image->psdoc jpg)) '(0 0 16 24))
    (check= (bounding-box (image->psdoc pnm)) '(0 0 3 2))
    (check= (bounding-box (image->psdoc svg)) '(0 0 50 25))
    ;; image->postscript goes through the converters of the format
    (check= (bounding-box (image->postscript png)) '(0 0 30 20))
    (check= (bounding-box (image->postscript eps)) '(10 20 110 70))
    (check-true (string-starts? (image->postscript eps) "%!PS-Adobe"))
    (check= (image->postscript (string->url "a.unknownformat")) "")
    ;; with an external tool, when it is there
    (when (url-exists-in-path? "ps2pdf")
      (let ((pdf (url-append dir "b.pdf")))
        (check= (convert eps "postscript-file" "pdf-file" (cons 'dest pdf)) pdf)
        (check= (and (url-exists? pdf) (substring (string-load pdf) 0 5)) "%PDF-")))
    (when (and (url-exists-in-path? "rsvg-convert")
               (not (url-exists-in-path? "inkscape")))
      (let ((out (url-append dir "c.png")))
        (check= (convert svg "svg-file" "png-file" (cons 'dest out)) out)
        (check= (and (url-exists? out) (substring (string-load out) 1 4)) "PNG")))))

;; The image formats and their converters (init-images.scm)
(define (test-image-formats)
  (check-group "image formats")
  (for-each
   (lambda (e)
     (check= (list (car e) (format-get-name (car e)) (format-get-suffixes* (car e)))
             e))
   '(("postscript" "Postscript" (tuple "ps" "eps")) ("pdf" "Pdf" (tuple "pdf"))
     ("xfig" "Xfig" (tuple "fig")) ("xmgrace" "Xmgrace" (tuple "agr" "xmgr"))
     ("svg" "Svg" (tuple "svg")) ("geogebra" "Geogebra" (tuple "ggb"))
     ("xpm" "Xpm" (tuple "xpm")) ("jpeg" "Jpeg" (tuple "jpg" "jpeg"))
     ("tif" "Tif" (tuple "tif" "tiff")) ("ppm" "Ppm" (tuple "ppm"))
     ("gif" "Gif" (tuple "gif")) ("png" "Png" (tuple "png"))
     ("pnm" "Pnm" (tuple "pnm"))))
  (check= (format-get-suffixes* "sound")
          '(tuple "au" "cdr" "cvs" "dat" "gsm" "ogg" "snd" "voc" "wav"))
  (check= (format-get-suffixes* "animation") '(tuple "gif"))
  (check= (format-default-suffix "image") "png")
  (with l (cdr (format-get-suffixes* "image"))
    (for-each (lambda (s) (check-true (in? s l)))
              '("png" "jpg" "jpeg" "gif" "pnm" "ps" "eps" "pdf" "svg" "tif"
                "ppm")))
  (with l (image-formats)
    (for-each (lambda (f) (check-true (in? f l)))
              '("gif" "jpeg" "pdf" "png" "pnm" "postscript" "ppm" "svg" "tif")))
  ;; the bitmaps go to PostScript by image->psdoc, without external tool
  (for-each (lambda (fm)
              (check= (converter-search (string-append fm "-file")
                                        "postscript-document")
                      (list (string-append fm "-file") "postscript-document")))
            '("png" "jpeg" "gif" "tif" "pnm"))
  (check= (converter-search "xfig-file" "postscript-file")
          '("xfig-file" "postscript-file")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source code formats and the registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-formats
  ;; format, name, suffixes
  '(("cpp" "C++ source code" ("cpp" "cc" "hpp" "hh"))
    ("julia" "Julia source code" ("jl"))
    ("java" "Java source code" ("java"))
    ("scala" "Scala source code" ("scala" "sc" "sbt"))
    ("json" "JSON" ("json"))
    ("csv" "CSV" ("csv"))
    ("python" "Python source code" ("py"))
    ("scilab" "Scilab source code" ("sce" "sci"))
    ("mathemagix" "Mathemagix source code" ("mmx" "mmh"))
    ("caas" "Caas source code" ())
    ("scheme" "Scheme source code" ("scm"))
    ("code" "Source code" ())))

;; Each source code format is verbatim with the encoding SourceCode: one
;; paragraph per line, the symbols in UTF-8, the ASCII kept as it is (no
;; quotes of TeX), and back.
(define (test-code-formats)
  (check-group "code formats")
  (for-each
   (lambda (e)
     (let* ((fm (car e))
            (snip (string-append fm "-snippet"))
            (doc (string-append fm "-document")))
       (check-true (format? fm))
       (check= (format-get-name fm) (cadr e))
       (check= (format-get-suffixes* fm) (cons 'tuple (caddr e)))
       (for-each (lambda (s) (check= (format-from-suffix s) fm)) (caddr e))
       (check= (export '(document "x = \"a\\b\" <less> 1" "  y\xe9<alpha>") snip)
               (string-append "x = \"a\\b\" < 1\n  y" (u8 #xE9 #x3B1)))
       (check= (export '(document "x" "y") doc) "x\ny")
       (check= (export "``x'' -- y" snip)
               (if (== fm "code")
                   (string-append (u8 #x2018 #x2018) "x'' -- y")
                   "``x'' -- y"))
       (check= (import (string-append "x = 1\n  y " (u8 #xE9 #x3B1) " <\n") snip)
               '(document "x = 1" "  y \xe9<alpha> <less>" ""))
       (check= (import "x = 1\n  y\n" doc)
               '(document (body (document "x = 1" "  y" ""))
                          (initial (collection
                                    (associate "language" "verbatim")
                                    (associate "font-family" "tt")
                                    (associate "par-first" "0cm")))))
       (with code '(document "def f(x):" "    return x <less> 1" "" "# \xe9")
         (check= (import (export code snip) snip) code))))
   code-formats)
  ;; the markup is lost, the tabs are expanded
  (check= (export '(concat "x" (em "y")) "cpp-snippet") "xy")
  (check= (import "a\tb" "python-snippet") "a       b")
  ;; a Cork byte 0x09 is a macron, not a tab
  (check= (export "a\tb" "python-snippet") (string-append "a" (u8 #xAF) "b"))
  ;; the verbatim export, unlike the source code, makes quotes of ``
  (check= (export "``x''" "verbatim-snippet") (string-append (u8 #x2018 #x2018) "x''"))
  ;; texmacs->code with an explicit encoding
  (check= (texmacs->code (stree->tree "\xe9<alpha>") "utf-8") (u8 #xE9 #x3B1))
  (check= (texmacs->code (stree->tree "\xe9<alpha>") "Cork") "\xe9<alpha>"))

;; The formats reachable from TeXmacs, and back
(define (test-format-registry)
  (check-group "format registry")
  (let ((from (converters-from "texmacs-tree"))
        (to (converters-to "texmacs-tree")))
    (for-each
     (lambda (fm)
       (for-each (lambda (kind)
                   (with x (string-append fm kind)
                     (check-true (in? x from))
                     (check-true (in? x to))))
                 '("-snippet" "-document" "-file")))
     '("cpp" "julia" "java" "scala" "json" "csv" "python" "scilab"
       "mathemagix" "caas" "scheme" "code" "texmacs" "stm" "tmml" "html"
       "verbatim" "latex" "bibtex" "tmbib"))
    ;; latex-class can only be imported
    (check-true (in? "latex-class-document" to))
    (check-false (in? "latex-class-document" from))
    ;; no image or pdf format from TeXmacs trees by convert
    (check-false (in? "png-file" from))
    (check-false (in? "pdf-file" from)))
  ;; the source code formats are not in the import and export menus
  (let ((ex* (converters-from-special* "texmacs-file" "-file" #f))
        (ex (converters-from-special "texmacs-file" "-file" #f))
        (im (converters-to-special "texmacs-file" "-file" #f)))
    (for-each (lambda (fm) (check-true (in? fm ex*)))
              '("cpp" "json" "csv" "python" "scheme" "caas"))
    (for-each (lambda (fm) (check-false (in? fm ex)))
              '("cpp" "json" "csv" "python" "scheme" "caas"))
    (check-true (in? "latex-class" im))
    (check-false (in? "latex-class" ex)))
  ;; the Coq formats are only declared by the Coq plugin, gpg by the
  ;; crypto modules
  (check-false (format? "vernac"))
  (check-false (format? "gallina"))
  (check= (format-from-suffix "v") "generic")
  (check-false (format? "tmu")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other C++ parsers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coq vernacular: the statements and proofs become coq-... tags
(define (test-vernac)
  (check-group "coq vernacular")
  (check= (st (vernac->texmacs "Lemma foo : True.\nProof. trivial. Qed."))
          '(document (coq-enunciation "" "dark grey" "Lemma" "foo" ": True.")
                     (coq-proof "" "dark grey"
                                (coq-command "" "dark grey" "Proof.")
                                (concat (coq-command "" "dark grey" "trivial.")
                                        " "
                                        (coq-command "" "dark grey" "Qed.")))))
  (check= (st (vernac-document->texmacs "Lemma foo : True."))
          '(document (body (coq-enunciation "" "dark grey" "Lemma" "foo" ": True."))
                     (style (tuple "generic" "coq")))))

;; JSON: objects are attr, arrays tuple, the other values strings (null
;; the empty string); the printer writes the trees back.
(define (test-json)
  (check-group "json")
  (check= (st (json->tree "{\"a\": [1, 2.5, \"x\"], \"b\": null, \"c\": true}"))
          '(attr "a" (tuple "1" "2.5" "x") "b" "" "c" "true"))
  (check= (st (json->tree "{\"k\": false}")) '(attr "k" "false"))
  (check= (st (json->tree "\"a\\nb\\\"c\\\\\"")) "a\nb\"c\\")
  (check= (st (json->tree "[]")) '(tuple))
  (check= (st (json->tree "{}")) '(attr))
  (check= (tree->json "x\"y") "\"x\\\"y\"")
  (check= (tree->json "a\nb\tc\\") "\"a\\nb\\tc\\\\\"")
  (check= (tree->json (stree->tree '(tuple "a"))) "[ \"a\" ]")
  (check= (tree->json (stree->tree '(attr "a" "b"))) "{ \"a\": \"b\" }")
  (check= (tree->json (stree->tree '(attr "a" (tuple "1" "2") "b" (attr "c" "d"))))
          "{\n  \"a\": [\n    \"1\",\n    \"2\"\n  ],\n  \"b\": { \"c\": \"d\" }\n}")
  (check= (tree->json (stree->tree '(tuple (json-number "1") (json-boolean "true")
                                           (json-null))))
          "[\n  1,\n  true,\n  null\n]")
  (check= (tree->json (stree->tree '(frac "a" "b"))) "")
  (with s "{\"a\": [\"1\", \"x\"], \"b\": {\"c\": \"d\"}}"
    (check= (st (json->tree (tree->json (json->tree s)))) (st (json->tree s))))
  ;; FIXME: the parser of numbers knows neither the sign nor the exponent
  ;; (json_parse_number in json.cpp only reads digits and points, json_skip
  ;; skips the minus): (json->tree "[-1, 2e3, -0.5]") gives (tuple "1" "2"),
  ;; expected (tuple "-1" "2e3" "-0.5").
  ;; FIXME: \f is read as a backspace (json_parse_string, json.cpp:89), and
  ;; \u is not read: (json->tree "\"a\\fb\"") gives "a\bb", expected "a\fb";
  ;; (json->tree "\"\\u00e9\"") gives "u00e9", expected e acute.
  ;; FIXME: an empty array or object is printed as nothing (json_print uses
  ;; is_func, false for no children, json.cpp:324-326): (tree->json (tuple)) gives
  ;; "", expected "[]"; (tree->json (attr)) gives "", expected "{}".
  )

;; The compressed trees (for the AI tools): the tags become compressed
;; nodes with an identifier, which decompress-tree undoes.
(define (test-compress)
  (check-group "compressed trees")
  (with t (stree->tree '(document "a" (frac "x" "y")))
    (check= (st (compress-tree t)) '(document "a" (compressed "x0" "x" "y")))
    (check= (st (decompress-tree (compress-tree t))) '(document "a" (frac "x" "y")))
    (check= (compress-html t 0)
            (string-append "<body><p>a</p><p><div id=\"x0\">x</div>"
                           "<div id=\"cont-x0\">y</div></p></body>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serializations of special characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-bytes
  ;; every Cork byte but < and >, which are symbols
  (list->string (list-filter (map integer->char (iota 256))
                             (lambda (c) (nin? c (list #\< #\>))))))

(define (test-serialize-special)
  (check-group "special characters")
  ;; tm: the control characters are written \@ + code
  (check= (serialize-texmacs-snippet (stree->tree (cork 0 9 10 13 #x1f #x7f #x80 #xff)))
          (string-append "\\0\\t\\n\\M\\_" (cork #x7f #x80 #xff)))
  (check= (serialize-texmacs-snippet (stree->tree "<#1F600><alpha>\\<less>"))
          "\\<#1F600\\>\\<alpha\\>\\\\\\<less\\>")
  (check= (st (parse-texmacs-snippet (serialize-texmacs-snippet (stree->tree all-bytes))))
          `(document ,all-bytes))
  ;; stm
  ;; the tabs and newlines are kept; the quote and the backslash are
  ;; escaped twice (once for TeXmacs, once for Scheme), and the reader
  ;; takes both the doubled and the plain Scheme escapes
  (check= (texmacs->stm (stree->tree (cork 9 10 34 92)))
          (cork 34 9 10 92 92 92 34 92 92 92 92 34))
  (check= (st (stm-snippet->texmacs (cork 34 97 92 92 92 34 98 34))) "a\"b")
  (check= (st (stm-snippet->texmacs (cork 34 97 92 34 98 34))) "a\"b")
  (check= (st (stm-snippet->texmacs (texmacs->stm (stree->tree all-bytes)))) all-bytes)
  ;; tmml: the Cork bytes are written in UTF-8, and come back
  (check= (export (cork 0 9 #x7f #xff) "tmml-snippet") (u8 #x60 #xAF #x2010 #xDF))
  ;; the bytes which are one way to Unicode (0x18 zero, 0x1A j, 0xDF SS)
  ;; are lost
  (with two-way (list->string (list-filter (string->list all-bytes)
                                           (lambda (c) (nin? (char->integer c)
                                                             '(#x18 #x1a #xdf)))))
    (check= (import (export (tmfile `(document ,two-way)) "tmml-document")
                    "tmml-document")
            (tmfile `(document ,two-way))))
  (check= (import (export (cork #x18 #x1a #xdf) "tmml-snippet") "tmml-snippet") "0jSS")
  ;; a whole document
  (with doc (tmfile `(document ,all-bytes "<#1F600><alpha><foo>"))
    (check= (st (parse-texmacs (serialize-texmacs (stree->tree doc)))) doc)
    (check= (import (export doc "stm-document") "stm-document") doc))
  ;; the spaces at the end of a paragraph of a document are kept
  (with doc (tmfile '(document "a " "b " (concat "c" (em "d "))))
    (check= (st (parse-texmacs (serialize-texmacs (stree->tree doc)))) doc))
  (check= (st (parse-texmacs-snippet (serialize-texmacs-snippet
                                      (stree->tree '(document "a b " "c")))))
          '(document "a b " "c"))
  ;; FIXME: a space at the very end of a snippet is lost: tree_to_texmacs
  ;; (totm.cpp:337) flushes it unprotected, unlike write_return, and the
  ;; reader drops it: (serialize-texmacs-snippet "a ") gives "a ", which
  ;; parse-texmacs-snippet reads as (document "a"), expected (document "a ").
  ;; stree <-> tree
  (check= (st (stree->tree '(concat "a" "b"))) '(concat "a" "b"))
  (check= (st (stree->tree 'foo)) "foo")
  (check= (st (stree->tree 12)) "12")
  (check= (st (stree->tree '(document))) '(document))
  (check= (tm->stree (stree->tree '(concat "a" (em "b")))) '(concat "a" (em "b")))
  (check= (tm->stree "x") "x")
  (check= (st (tm->tree '(frac "a" "b"))) '(frac "a" "b"))
  (check= (st (string->tree all-bytes)) all-bytes)
  (check= (tree->string (stree->tree all-bytes)) all-bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (big-document n)
  (cons 'document
        (map (lambda (i)
               `(concat ,(string-append "Paragraph " (number->string i) " with ")
                        (em "emphasis")
                        " and <alpha> \xe9 " (with "mode" "math" (frac "a" "b"))))
             (iota n))))

(define (timed thunk)
  ;; the value of @thunk and the time it took in ms
  (let* ((t0 (texmacs-time))
         (r (thunk)))
    (cons r (- (texmacs-time) t0))))

(define (check-fast what thunk expected)
  ;; @thunk gives @expected in less than 2 s
  (with r (check-run (lambda () (timed thunk)))
    (if (and (pair? r) (== (car r) 'error))
        (check-report #f what (object->string r))
        (begin
          (check-report (equal? (car r) expected) what "wrong result")
          (check-report (< (cdr r) 2000) (string-append what " in < 2 s")
                        (string-append (number->string (cdr r)) " ms"))))))

;; A document of 2000 paragraphs goes through each serialization and back
;; in less than 2 s.
(define (test-big-document)
  (check-group "big document")
  (let* ((body (big-document 2000))
         (doc (tmfile body))
         (t (stree->tree doc)))
    (check-fast "stree->tree" (lambda () (st (stree->tree doc))) doc)
    (check-fast "tree->stree" (lambda () (tree->stree t)) doc)
    (with s (serialize-texmacs t)
      (check-true (> (string-length s) 100000))
      (check-fast "tm" (lambda () (st (parse-texmacs (serialize-texmacs t)))) doc))
    (check-fast "stm" (lambda () (import (export doc "stm-document") "stm-document"))
                doc)
    (check-fast "tmml" (lambda () (import (export doc "tmml-document") "tmml-document"))
                doc)
    (check-fast "verbatim"
                (lambda () (length (cdr (import (export body "verbatim-snippet")
                                                "verbatim-snippet"))))
                2000)
    (check-fast "texmacs-file"
                (lambda ()
                  (let* ((u (export doc "texmacs-file"))
                         (r (st (convert u "texmacs-file" "texmacs-tree"))))
                    (system-remove u)
                    r))
                doc))
  ;; FIXME: the HTML import of a long document overflows the stack: the
  ;; export of 2000 paragraphs (concat "a" (with "mode" "math" (frac "a"
  ;; "b"))), 4000 elements at the top, raises stack-overflow in
  ;; html-snippet -> texmacs-tree, while 1000 paragraphs work (the recursion
  ;; of htmltm.scm on the list of the elements).
  (with body (cons 'document (make-list 2000 "x"))
    (check= (length (cdr (import (export body "html-snippet") "html-snippet"))) 2000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (convert-more-test-failures)
  (check-suite "convert-more")
  (when (url-exists? (temp-dir)) (system-rmdir-recursive (temp-dir)))
  (system-mkdir (temp-dir))
  (for-each run-group
            (list test-mathml-export test-mathml-html test-mathml-import
                  test-mathml-round-trip test-xml-parser test-xml-names
                  test-tmml-special test-cork-table test-cork-symbols
                  test-cork-bytes test-cork-unicode test-other-encodings
                  test-images test-image-formats test-code-formats
                  test-format-registry test-vernac test-json test-compress
                  test-serialize-special test-big-document))
  (when (url-exists? (temp-dir)) (system-rmdir-recursive (temp-dir)))
  (check-end))
