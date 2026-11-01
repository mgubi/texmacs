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
;;   - the C++ parsers of Coq vernacular and JSON, the compressed trees and
;;     their tables (freed after each request of AI or LanguageTool);
;;   - the serializations (tm, stm, tmml, stree) of the control characters
;;     and of a document of 2000 paragraphs, each in less than 2 s.
;;
;; Strings of TeXmacs trees are Cork-encoded: "\xe9" is e acute in Cork,
;; "\xc3\xa9" the same in UTF-8. Expected UTF-8 strings are built by u8
;; from their code points. The files go to the directory convert-more of
;; the temporary directory, removed at the start and at the end.

(texmacs-module (check convert-more-test)
  (:use (check check-lib)
        (convert mathml tmmath)
        (tools spell spell-lantool)
        (tools ai ai-translate)))

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
  ;; a decimal number is one mn; a point which is not between digits is not
  ;; part of the number
  (check= (texmacs->mathml "12.5") '(m:mn "12.5"))
  (check= (texmacs->mathml "x=0.25") '(m:mrow (m:mi "x") (m:mo "=") (m:mn "0.25")))
  (check= (texmacs->mathml "1.2.3") '(m:mrow (m:mn "1.2") (m:mi ".") (m:mn "3")))
  (check= (texmacs->mathml "2.") '(m:mrow (m:mn "2") (m:mi ".")))
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
  ;; a table without tformat
  (check= (texmacs->mathml '(table (row (cell "a"))))
          '(m:mtable (@ (columnalign "left")) (m:mtr (m:mtd (m:mi "a")))))
  (check= (texmacs->mathml '(table (row (cell "a") (cell "b")) (row (cell "1") (cell "2"))))
          '(m:mtable (@ (columnalign "left left"))
                     (m:mtr (m:mtd (m:mi "a")) (m:mtd (m:mi "b")))
                     (m:mtr (m:mtd (m:mn "1")) (m:mtd (m:mn "2")))))
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
    (check-true (string-occurs? (ms "<mfrac><mi>a</mi><mi>b</mi></mfrac>") s)))
  (check-group "raw html")
  ;; the html tag keeps its markup (written <less>, <gtr>) and gives its
  ;; characters in UTF-8, <alpha> and Cork ones included
  (check= (export '(specific "html" "<less>b<gtr>x<alpha><less>/b<gtr>")
                  "html-snippet")
          (string-append "<b>x" (u8 #x3B1) "</b>"))
  (check= (export `(specific "html" ,(string-append "a" (cork 233) "<less>br/<gtr>"))
                  "html-snippet")
          (string-append "a" (u8 #xE9) "<br/>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Labels inside formulas in HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (occurrences what s)
  (let loop ((pos 0) (n 0))
    (with i (string-search-forwards what pos s)
      (if (< i 0) n (loop (+ i (string-length what)) (+ n 1))))))

(define sample-number 0)

(define (with-sample-buffer body thunk)
  ;; @thunk applied to @body loaded in a buffer of its own, from a file of
  ;; its own (a name never used before: no stale file cache), then closed
  (set! sample-number (+ sample-number 1))
  (let* ((name (string-append "sample-" (number->string sample-number)))
         (tm (url-append (temp-dir) (string-append name ".tm")))
         (old (current-buffer))
         (before (buffer-list)))
    (string-save (serialize-texmacs
                  (stree->tree `(document (TeXmacs ,(texmacs-version))
                                          (style "article") (body ,body))))
                 tm)
    (load-buffer tm :strict)
    (with r (thunk name)
      (for (b (buffer-list))
        (when (nin? b before)
          (buffer-pretend-saved b)
          (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old))
      r)))

(define (html-of-buffer body . opts)
  ;; the HTML of a document as File > Export makes it for the labels: the
  ;; macros of the style are expanded by the editor (html-expand), then
  ;; converted
  (with-sample-buffer body
    (lambda (name)
      (apply convert (cons* (html-expand (buffer-tree))
                            "texmacs-tree" "html-snippet" opts)))))

(define (with-converter-options opts thunk)
  ;; the export of a buffer takes the options of the converters from the
  ;; preferences: they are set in memory only (not saved), as the
  ;; preferences dialog would set them, then restored exactly
  (let* ((olds (map (lambda (p)
                      (list (car p) (cpp-has-preference? (car p))
                            (get-preference (car p))))
                    opts)))
    (for (p opts)
      (cpp-set-preference (car p) (cdr p))
      (notify-preference (car p)))
    (with r (check-run thunk)
      (for (o olds)
        (if (cadr o)
            (cpp-set-preference (car o) (caddr o))
            (cpp-reset-preference (car o)))
        (notify-preference (car o)))
      r)))

(define (html-export-of-buffer body . opts)
  ;; the HTML file of a document exported as by File > Export > Html:
  ;; buffer-export (exec_html on the view, attach_data, the converter of
  ;; html files, with current-save-target set to the file)
  (with-sample-buffer body
    (lambda (name)
      (let ((html (url-append (temp-dir) (string-append name ".html")))
            (source current-save-source)
            (target current-save-target))
        (with-converter-options opts
          (lambda ()
            (export-buffer-main (current-buffer) html "html"
                                (list :overwrite))))
        (set! current-save-source source)
        (set! current-save-target target)
        (if (url-exists? html) (string-load html) "")))))

(define formula-label-samples
  ;; a document, the labels of its formulas, and the references to them
  (list
   ;; the document of #617: the label in front, in a concat
   (list '(document (equation (document (concat (label "eq:one") "a=b+c.")))
                    (concat "See " (eqref "eq:one") "."))
         '("eq:one") '("eq:one"))
   ;; the label at the end
   (list '(document (equation (document (concat "x=1" (label "eq:end")))))
         '("eq:end") '())
   (list '(document (equation* (document (concat "a=b" (label "eq:star")))))
         '("eq:star") '())
   ;; several labels in one formula
   (list '(document (equation (document (concat (label "eq:t1") "a=b"
                                                (label "eq:t2")))))
         '("eq:t1" "eq:t2") '())
   ;; labels in the cells of equation arrays
   (list '(document
           (eqnarray* (document
                       (tformat
                        (table (row (cell "a") (cell "=")
                                    (cell (concat "b" (label "eq:r1"))))
                               (row (cell "c") (cell "=")
                                    (cell (concat "d" (label "eq:r2")))))))))
         '("eq:r1" "eq:r2") '())
   (list '(document
           (align (document
                   (tformat
                    (table (row (cell "a") (cell (concat "=b" (label "eq:a1"))))
                           (row (cell "c") (cell (concat "=d" (label "eq:a2")))))))))
         '("eq:a1" "eq:a2") '())
   ;; an inline formula
   (list '(document (concat "Let " (math (concat "x" (label "eq:in")))
                            " and " (reference "eq:in") "."))
         '("eq:in") '("eq:in"))))

;; A label inside a formula gives one anchor in front of the formula, with
;; MathJax as with MathML (which both drop the label tag), and the
;; references link to it (#588, #617). Checked on the HTML of the converter,
;; and on the file of the export of the buffer, as File > Export makes it
;; (#666).
(define (check-formula-labels where mathjax? s labels refs)
  ;; the formula is MathJax or MathML
  (check= (list where (if mathjax?
                          (or (string-occurs? "\\(" s)
                              (string-occurs? "\\begin{" s))
                          (string-occurs? "<math" s)))
          (list where #t))
  (for (l labels)
    (check= (list where l (occurrences (string-append "<a id=\"" l "\">") s))
            (list where l 1)))
  (for (l refs)
    (check= (list where l (occurrences (string-append "href=\"#" l "\"") s))
            (list where l 1))))

(define (test-html-formula-labels)
  (check-group "html labels in formulas")
  (with before (std-converter-options "texmacs-stree" "html-stree")
    (test-html-formula-labels-with)
    ;; the options of the export are those of before
    (check= (std-converter-options "texmacs-stree" "html-stree") before)))

(define (test-html-formula-labels-with)
  (for (opts (list (list (cons "texmacs->html:mathjax" "on")
                         (cons "texmacs->html:mathml" "off"))
                   (list (cons "texmacs->html:mathjax" "off")
                         (cons "texmacs->html:mathml" "on"))))
    (with mathjax? (== (cdar opts) "on")
      (for (sample formula-label-samples)
        (with (body labels refs) sample
          (with kind (if mathjax? "mathjax" "mathml")
            (check-formula-labels (list kind body)
                                  mathjax? (apply html-of-buffer body opts)
                                  labels refs)
            (with s (apply html-export-of-buffer body opts)
              (check-formula-labels (list kind "export" body)
                                    mathjax? s labels refs)
              ;; a whole document, which loads MathJax or not
              (check= (list kind "export" body
                            (string-occurs? "<html" s)
                            (string-occurs? "</html>" s)
                            (string-occurs? "npm/mathjax" s))
                      (list kind "export" body #t #t mathjax?)))))))))

;; The LaTeX of the MathJax formulas uses only the macros and environments
;; of MathJax 3 (TeX input, packages base and ams): the accents of the
;; accents package and of TeXmacs (#607), and the arrays which the Html
;; export expands to equations-base inside a formula (#611).
(define (mathjax-of body)
  (html-of-buffer body (cons "texmacs->html:mathjax" "on")
                  (cons "texmacs->html:mathml" "off")))

(define (mathjax-formula-has? body . what)
  ;; the HTML of @body contains each string of @what
  (with s (mathjax-of body)
    (list-and (map (cut string-occurs? <> s) what))))

(define mathjax-unknown
  ;; LaTeX which MathJax does not know
  '("\\ring" "\\invbreve" "\\underaccent" "bracefill"
    "equationsbase" "equations-base" "\\["))

(define (mathjax-unknown-in body)
  (with s (mathjax-of body)
    (list-filter mathjax-unknown (cut string-occurs? <> s))))

(define mathjax-accents
  '("<hat>" "<tilde>" "<bar>" "<vect>" "<breve>" "<invbreve>" "<check>"
    "<abovering>" "<acute>" "<grave>" "<dot>" "<ddot>" "<dddot>" "<ddddot>"
    "<rightarrow>" "<leftarrow>" "<leftrightarrow>"
    "<overbrace>" "<underbrace>" "<poverbrace>" "<punderbrace>"
    "<sqoverbrace>" "<squnderbrace>"))

(define (mathjax-array env . args)
  ;; @env with two rows in an equation, as the LaTeX import makes it
  `(document
    (equation
     (document
      (,env ,@args
            (document (tformat (table (row (cell "a") (cell "=b,"))
                                      (row (cell "c") (cell "=d."))))))))))

(define (test-html-mathjax-formulas)
  (check-group "html mathjax formulas")
  ;; #607: the ring accent is \mathring
  (check-true (mathjax-formula-has? '(math (wide "O" "<abovering>"))
                                    "\\(\\mathring{O}\\)"))
  (check-true (mathjax-formula-has? '(math (wide "O" "<invbreve>"))
                                    "\\overparen{O}"))
  ;; the accents below: \underset at the size of the formula
  (check-true (mathjax-formula-has? '(math (wide* "O" "<hat>"))
                                    "\\underset{\\textstyle\\hat{}}{O}"))
  (check-true (mathjax-formula-has? '(math (wide* "O" "<abovering>"))
                                    "\\underset{\\textstyle\\mathring{}}{O}"))
  (check-true (mathjax-formula-has? '(math (wide* "O" "<invbreve>"))
                                    "\\underset{\\textstyle\\overparen{}}{O}"))
  (check-true (mathjax-formula-has?
               '(math (wide* "O" "<dddot>"))
               "\\underset{\\textstyle\\dddot{\\hphantom{O}}}{O}"))
  ;; a brace above whose tip points down, and conversely
  (check-true (mathjax-formula-has?
               '(math (wide "O" "<underbrace>"))
               "\\overset{\\textstyle\\underbrace{\\hphantom{O}}}{O}"))
  (check-true (mathjax-formula-has?
               '(math (wide* "O" "<overbrace>"))
               "\\underset{\\textstyle\\overbrace{\\hphantom{O}}}{O}"))
  (for (acc mathjax-accents)
    (for (w '(wide wide*))
      (for (x '("O" "xyz"))
        (with body `(math (,w ,x ,acc))
          (check= (list body (mathjax-unknown-in body)) (list body '()))))))
  ;; #611: aligned in an equation, as in the issue
  (with body (mathjax-array 'aligned)
    (check-true (mathjax-formula-has? body "\\begin{aligned}"
                                      "\\end{aligned}"))
    (check= (mathjax-unknown-in body) '()))
  ;; the arrays which the export expands: aligned when the columns are
  ;; aligned right and left in turn, else an array
  (for (env '(aligned* eqsplit eqsplit* align* flalign*))
    (check= (list env (mathjax-formula-has? (mathjax-array env)
                                            "\\begin{aligned}"))
            (list env #t)))
  (for (env '(alignedat alignedat*))
    (check= (list env (mathjax-formula-has? (mathjax-array env "2")
                                            "\\begin{aligned}"))
            (list env #t)))
  (check-true (mathjax-formula-has? (mathjax-array 'gather*)
                                    "\\begin{array}{cc}"))
  (check-true (mathjax-formula-has?
               '(document
                 (equation
                  (document
                   (eqnarray*
                    (document
                     (tformat (table (row (cell "a") (cell "=") (cell "b"))
                                     (row (cell "c") (cell "=") (cell "d")))))))))
               "\\begin{eqnarray*}"))
  ;; arrays of LaTeX which TeXmacs does not define are kept
  (check-true (mathjax-formula-has? (mathjax-array 'gathered)
                                    "\\begin{gathered}"))
  (check-true (mathjax-formula-has? (mathjax-array 'subarray "c")
                                    "\\begin{subarray}{c}"))
  (for (env '(aligned aligned* eqsplit align* gather* eqnarray* multline*
              gathered))
    (with body (mathjax-array env)
      (check= (list env (mathjax-unknown-in body)) (list env '()))))
  (with body (mathjax-array 'alignedat "2")
    (check= (mathjax-unknown-in body) '()))
  ;; the arrays outside formulas are unchanged
  (check-true (mathjax-formula-has?
               '(document
                 (align* (document (tformat (table (row (cell "a")
                                                        (cell "=b")))))))
               "\\begin{array}{rl}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MathML import
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MathML in an HTML document, in the namespace of MathML (as TeXmacs and
;; most tools write it), becomes a math tag, or an equation* with
;; display="block". MathML without the xmlns attribute, as HTML5 allows it,
;; is read in the same way.
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
          '(math "x"))
  ;; without xmlns
  (check= (import "<math><mi>x</mi><mo>+</mo><mn>1</mn></math>" "html-snippet")
          '(math "x+1"))
  (check= (import "<math><mfrac><mi>a</mi><mi>b</mi></mfrac></math>" "html-snippet")
          '(math (frac "a" "b")))
  (check= (import "<p>Let <math><mi>x</mi></math> be.</p>" "html-snippet")
          '(concat "Let " (math "x") " be."))
  (check= (import "<math display=\"block\"><mi>x</mi></math>" "html-snippet")
          '(equation* "x"))
  ;; an error is shown in red
  (check= (mathml-import "<merror><mi>x</mi></merror>")
          '(math (with "color" "red" "x"))))

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
  ;; the hat, which the export writes as the entity &Hat;
  (check= (import (html-math '(with "mode" "math" (wide "x" "^"))) "html-snippet")
          '(math (wide "x" "^")))
  (check= (mathml-import "<mover><mi>x</mi><mo>&Hat;</mo></mover>")
          '(math (wide "x" "^")))
  ;; decimal numbers
  (check= (import (html-math '(with "mode" "math" "x=12.5")) "html-snippet")
          '(math "x=12.5"))
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
  ;; the entities of XML are replaced once, in attributes and CDATA too
  (check= (parse-xml "<a x=\"q=1&amp;lt=2&amp;amp;&amp;#65;\"/>")
          '(*TOP* (a (@ (x "q=1&lt=2&amp;&#65;")))))
  (check= (parse-xml "<a><![CDATA[a&amp;b]]></a>") '(*TOP* (a "a&amp;b")))
  (check= (parse-xml (string-append "<!DOCTYPE a [<!ENTITY e \"E&#38;#66;\">"
                                    "<!ENTITY f \"&e;F\">]><a x=\"&f;\">&f;</a>"))
          '(*TOP* (*DOCTYPE* "a") (a (@ (x "EBF")) "EBF")))
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
  ;; script and style are raw text in HTML: no elements, no entities
  (check= (parse-html "<script>if (a<b) x;</script>") '(*TOP* (script "if (a<b) x;")))
  (check= (parse-html "<script type=\"text/javascript\">a&amp;&&<p>b</script><p>c</p>")
          '(*TOP* (script (@ (type "text/javascript")) "a&amp;&&<p>b") (p "c")))
  (check= (parse-html "<SCRIPT>x<y</SCRIPT>") '(*TOP* (script "x<y")))
  (check= (parse-html "<Script>x<y</Script><p>c</p>")
          '(*TOP* (script "x<y") (p "c")))
  (check= (parse-html "<script>x</sCrIpT><p>c</p>") '(*TOP* (script "x") (p "c")))
  (check= (parse-html "<script>x</scr") '(*TOP* (script "x</scr")))
  (check= (parse-html "<style>a>b {}</style>") '(*TOP* (style "a>b {}")))
  (check= (parse-html "<script></script><p>c</p>") '(*TOP* (script) (p "c")))
  (check= (parse-xml "<script>a<b/></script>") '(*TOP* (script "a" (b))))
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
  ;; and so is <#1F600>, which comes back by its name: the name is written
  ;; as the character
  (check= (tm->xml-cdata "<#1F600>") '(tm-sym "#1F600"))
  (check= (tm->xml-cdata "<emoji-grinning-face>") (u8 #x1F600))
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
          "<tm-sym>#3B1</tm-sym><tm-sym>#1F600</tm-sym><tm-sym>foo</tm-sym>")
  (check= (export "<emoji-grinning-face>" "tmml-snippet") (u8 #x1F600))
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

;; The symbols of tmuniversaltounicode-extra: all give their code point
;; and come back, since none takes a code point which another table reads
;; back as something else; <mathhyphen> exports to the Cork hyphen, one way.
(define (test-extra-symbols)
  (check-group "extra symbols")
  (let* ((t (read-table "tmuniversaltounicode-extra"))
         (l (list-filter t (lambda (e) (and (list? e) (= (length e) 2)
                                            (string? (car e))
                                            (string-starts? (car e) "<")
                                            (hex-code (cadr e)))))))
    (check-true (>= (length l) 150))
    (check= (list-filter
             (map (lambda (e)
                    (and (!= (cork->utf8 (car e)) (u8 (hex-code (cadr e)))) e))
                  l)
             identity)
            '())
    (check= (list-filter
             (map (lambda (e)
                    (and (!= (utf8->cork (u8 (hex-code (cadr e)))) (car e))
                         (not (member (cadr e) '("#200B")))
                         (list (car e) (utf8->cork (u8 (hex-code (cadr e)))))))
                  l)
             identity)
            '()))
  (check= (cork->utf8 "<mathhyphen>") (u8 #x2010))
  (check= (utf8->cork (u8 #x2010)) (cork #x7f)))

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
  ;; an emoji comes by its name, as alpha does (emoji-test.scm)
  (check= (utf8->cork (u8 #x1F600)) "<emoji-grinning-face>")
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
  ;; a character which cannot be converted is replaced by U+FFFD, or by ?
  ;; when the target encoding has no U+FFFD, and the conversion goes on
  (check= (string-convert "caf\xe9 x" "UTF-8" "ISO-8859-1") "caf? x")
  (check= (string-convert (string-append "a" (u8 #x20AC) "b") "UTF-8" "ISO-8859-1")
          "a?b")
  (check= (string-convert (string-append (u8 #x3B1) (u8 #x416)) "UTF-8" "KOI8-R")
          (string-append "?" (cork #xf6)))
  (check= (string-convert "ab\xc3" "UTF-8" "ISO-8859-1") "ab?")
  ;; (no hexadecimal digit after \xe9: S7 would read it in the escape)
  (check= (string-convert (string-append "a\xe9" "b\xe9") "ASCII" "UTF-8")
          (string-append "a" (u8 #xFFFD) "b" (u8 #xFFFD)))
  (check= (string-convert (string-append "a\xe9" "b") "ASCII" "Cork")
          "a<#FFFD>b")
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

;; a check left out for a missing tool
(define (images-skip what why)
  (display* "  SKIP [" what "] " why "\n"))

;; The size of an image, in points (a pixel is a point when the file gives
;; no resolution), is the bounding box of its PostScript form (image->psdoc,
;; which goes through the convert of ImageMagick for the bitmaps and SVG:
;; without it, every image becomes misc/pixmaps/unknown.eps, and these
;; checks are skipped); an EPS file gives its own bounding box.
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
    ;; (Qt 6 cannot write PostScript: the bitmaps go to PostScript with the
    ;; convert of ImageMagick, which TeXmacs does not use on Windows, where
    ;; convert is another command; without it image->psdoc gives the
    ;; picture of an unknown image)
    (if (not (has-convert?))
        (images-skip "the PostScript of bitmaps and svg"
                     "no convert of ImageMagick (never used on Windows)")
        (begin
          (check= (bounding-box (image->psdoc png)) '(0 0 30 20))
          (check= (bounding-box (image->psdoc gif)) '(0 0 40 10))
          (check= (bounding-box (image->psdoc jpg)) '(0 0 16 24))
          (check= (bounding-box (image->psdoc pnm)) '(0 0 3 2))
          ;; an SVG has the size it declares, whatever the resolution at
          ;; which ImageMagick renders it (72 dpi for its own renderer, 96
          ;; dpi through rsvg-convert): 50 x 25 pt, and 100 x 40 px, which
          ;; are 75 x 30 pt
          (check= (bounding-box (image->psdoc svg)) '(0 0 50 25))
          (with svg-px (url-append dir "b.svg")
            (string-save (string-append
                          "<?xml version=\"1.0\"?>\n<svg "
                          "xmlns=\"http://www.w3.org/2000/svg\" "
                          "width=\"100px\" height=\"40px\">"
                          "<rect width=\"100\" height=\"40\" fill=\"red\"/>"
                          "</svg>\n")
                         svg-px)
            (check= (bounding-box (image->psdoc svg-px)) '(0 0 75 30)))
          ;; image->postscript goes through the converters of the format
          (check= (bounding-box (image->postscript png)) '(0 0 30 20))))
    (check= (bounding-box (image->postscript eps)) '(10 20 110 70))
    (check-true (string-starts? (image->postscript eps) "%!PS-Adobe"))
    (check= (image->postscript (string->url "a.unknownformat")) "")
    ;; with an external tool, when it is there
    (when (url-exists-in-path? "ps2pdf")
      (let ((pdf (url-append dir "b.pdf")))
        (check= (convert eps "postscript-file" "pdf-file" (cons 'dest pdf)) pdf)
        (check= (and (url-exists? pdf) (substring (string-load pdf) 0 5)) "%PDF-")))
    ;; PDF to EPS by gs-convert, also for a name with a space: the file is
    ;; written and given back (#745: -sOutputFile= was cut from its name)
    (when (url-exists-in-path? "gs")
      (let ((pdf (url-append dir "a b.pdf"))
            (out (url-append dir "a b.eps")))
        (system-copy "$TEXMACS_PATH/misc/pixmaps/unknown.pdf" pdf)
        (check= (convert pdf "pdf-file" "postscript-file" (cons 'dest out))
                out)
        (check-true (and (url-exists? out)
                         (string-starts? (string-load out) "%!PS-Adobe")))))
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
  ;; the image formats are those which a converter takes to PostScript:
  ;; pdf needs Ghostscript (except on Windows), svg inkscape or
  ;; rsvg-convert, ppm the convert of ImageMagick
  (let* ((pdf? (or (os-mingw?) (url-exists-in-path? "gs")))
         (svg? (or (url-exists-in-path? "inkscape")
                   (url-exists-in-path? "rsvg-convert")))
         (ppm? (has-convert?))
         (opt (lambda (ok? l) (if ok? l '()))))
    (with l (cdr (format-get-suffixes* "image"))
      (for-each (lambda (s) (check-true (in? s l)))
                (append '("png" "jpg" "jpeg" "gif" "pnm" "ps" "eps" "tif")
                        (opt pdf? '("pdf")) (opt svg? '("svg"))
                        (opt ppm? '("ppm")))))
    (with l (image-formats)
      (for-each (lambda (f) (check-true (in? f l)))
                (append '("gif" "jpeg" "png" "pnm" "postscript" "tif")
                        (opt pdf? '("pdf")) (opt svg? '("svg"))
                        (opt ppm? '("ppm"))))))
  ;; the bitmaps go to PostScript by image->psdoc, without external tool
  (for-each (lambda (fm)
              (check= (converter-search (string-append fm "-file")
                                        "postscript-document")
                      (list (string-append fm "-file") "postscript-document")))
            '("png" "jpeg" "gif" "tif" "pnm"))
  ;; (fig2ps is a shell script of TeXmacs, which Windows does not run)
  (if (url-exists-in-path? "fig2ps")
      (check= (converter-search "xfig-file" "postscript-file")
              '("xfig-file" "postscript-file"))
      (images-skip "xfig to PostScript" "no fig2ps command")))

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
;; quotes of TeX), and back. A document is a source file, which keeps its
;; tabs, line ends and encoding (see source-files-test.scm).
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
                                    (associate "par-first" "0cm")
                                    ;; (to save the file as it was)
                                    (associate "source-encoding" "utf-8")
                                    (associate "source-line-ends" "unix")
                                    (associate "source-indent" "spaces")))))
       (with code '(document "def f(x):" "    return x <less> 1" "" "# \xe9")
         (check= (import (export code snip) snip) code))))
   code-formats)
  ;; the markup is lost; the tabs are kept, as the character <#9>
  (check= (export '(concat "x" (em "y")) "cpp-snippet") "xy")
  (check= (import "a\tb" "python-snippet") "a<#9>b")
  (check= (export "a<#9>b" "python-snippet") "a\tb")
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
;; the answers of the chatbots, as LaTeX documents (ai.cpp): ChatGPT gives
;; its text as it is, Gemini in JSON; the commands of LaTeX which begin
;; with \n are not newlines
(define ai-answer-doc
  (string-append "\\documentclass{article}\n\\begin{document}\n"
                 "$\\nu \\ni x$, $a\\nonumber$ \\noindent b\n"
                 "\\end{document}"))

(define (gemini-answer latex)
  (string-append "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": "
                 (tree->json latex) "\n}]}}]}"))

(define (test-ai-answers)
  (check-group "ai answers")
  (with r '(with "mode" "text"
             (concat (math "<nu><ni>x") ", " (math (concat "a" (no-number)))
                     (no-indent) "b "))
    (check= (st (cpp-ai-latex-output ai-answer-doc "chatgpt" "check")) r)
    ;; (Upsilon adds the answer as it came, folded, after the answer of an
    ;; engine which gives it in JSON)
    (with g (st (cpp-ai-latex-output (gemini-answer ai-answer-doc)
                                     "gemini" "check"))
      (check= (if (func? g 'document) (cadr g) g) r))))

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
  ;; the other control characters as \u00XX (JSON does not allow them raw)
  (check= (tree->json (string #\a (integer->char 1) #\b (integer->char 27)))
          "\"a\\u0001b\\u001b\"")
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
  ;; an empty array or object
  (check= (tree->json (stree->tree '(tuple))) "[]")
  (check= (tree->json (stree->tree '(attr))) "{}")
  (check= (tree->json (stree->tree '(attr "a" (tuple) "b" (attr))))
          "{\n  \"a\": [],\n  \"b\": {}\n}")
  (check= (tree->json (stree->tree '(tuple (tuple) (attr) (tuple "1"))))
          "[\n  [],\n  {},\n  [ \"1\" ]\n]")
  (with s "{\"a\": [], \"b\": {}}"
    (check= (st (json->tree (tree->json (json->tree s)))) (st (json->tree s))))
  ;; numbers with a sign and an exponent, kept as they are written
  (check= (st (json->tree "[-1, 2e3, -0.5, 1.5E-2, 6e+1]"))
          '(tuple "-1" "2e3" "-0.5" "1.5E-2" "6e+1"))
  ;; the escapes of strings: \f is a form feed, \/ a slash, \u a code
  ;; point in utf8, a surrogate pair one character, an unpaired surrogate
  ;; the replacement character
  (check= (st (json->tree "\"a\\fb\"")) (string-append "a" (string (integer->char 12)) "b"))
  (check= (st (json->tree "\"a\\/b\"")) "a/b")
  (check= (st (json->tree "\"\\u00e9\\u00C9\"")) (u8 #xE9 #xC9))
  (check= (st (json->tree "\"\\ud83d\\ude00\"")) (u8 #x1F600))
  (check= (st (json->tree "\"\\ud83dx\"")) (string-append (u8 #xFFFD) "x"))
  ;; the parser does not fail on what is not JSON: what it can read is kept,
  ;; an unclosed array or object ends with the input, a key without a value
  ;; has the empty string, and what cannot begin a value is skipped
  (check= (st (json->tree "[1, [2")) '(tuple "1" (tuple "2")))
  (check= (st (json->tree "{\"a\": {\"b\": 1")) '(attr "a" (attr "b" "1")))
  (check= (st (json->tree "{\"a\" 1}")) '(attr "a" "" "1" ""))
  (check= (st (json->tree "garbage [1]")) '(tuple "1"))
  (check= (st (json->tree "")) ""))

;; The compressed trees (for the AI tools): the tags become compressed
;; nodes with an identifier, which decompress-tree undoes.
(define (test-compress)
  (check-group "compressed trees")
  (with t (stree->tree '(document "a" (frac "x" "y")))
    (check= (st (compress-tree t)) '(document "a" (compressed "x0" "x" "y")))
    (check= (st (decompress-tree (compress-tree t))) '(document "a" (frac "x" "y")))
    (check= (compress-html t 0)
            (string-append "<body><p>a</p><p><div id=\"x0\">x</div>"
                           "<div id=\"cont-x0\">y</div></p></body>")))
  ;; a code which is not known (of another session of TeXmacs, or garbled
  ;; in the answer) loses its markup, and keeps its text
  (check= (st (decompress-tree
               (stree->tree '(concat "a " (compressed "x999999" "b") " c"))))
          '(concat "a " "b" " c"))
  (check= (st (decompress-html
               "<body><p>a <div id=\"x999999\">b</div> c</p></body>" 0))
          '(document (concat "a " "b" " c")))
  (check= (st (decompress-tree (stree->tree '(compressed "x999999")))) ""))

;; The tables of the codes (#438): each request of AI or LanguageTool has
;; its own, freed once its answer is decompressed, and a chat has its own,
;; freed when its session is closed; the default table is left alone.
(define (compress-table-key-list)
  (map tree->stree (tree-children (compress-table-keys))))

(define (compress-tables-like prefix)
  ;; the tables of compress-table-new with this prefix: <prefix>-<number>
  (with n (+ (string-length prefix) 1)
    (list-filter (compress-table-key-list)
                 (lambda (s)
                   (and (string? s) (string-starts? s (string-append prefix "-"))
                        (> (string-length s) n)
                        (string->number (substring s n (string-length s))))))))

(define (no-ai-request? model)
  ;; no check may reach the network, whatever the preferences: the AI tools
  ;; are only called with a model of no engine (nothing is built or sent)
  (== (cpp-ai-engine model) "unknown"))

(define (compress-table-serial name)
  ;; the number of a table of compress-table-new: <prefix>-<number>
  (with pos (string-search-backwards "-" (string-length name) name)
    (string->number (substring name (+ pos 1) (string-length name)))))

;; The answers of the translation process (ai-translate*), given by a canned
;; engine (no request): an engine often wraps its answer in <p>, which
;; gives a document; it is not nested in the document of the buffer, but
;; its paragraphs take the place of the paragraph (#526).
(define (choices-answer body)
  ;; an answer of Albert (as cpp-ai-output reads it) with this body
  (string-append "{\"choices\":[{\"message\":{\"content\":\"<body>"
                 (string-replace body "\"" "\\\"") "</body>\"}}]}"))

(define ((canned-translate answer) serial tp st next)
  ;; translate-process-one (ai-translate.scm) with the engine replaced by
  ;; @answer, a function of the HTML sent which gives the body answered
  (let* ((t (tree-pointer->tree tp))
         (table (compress-table-new "translate"))
         (t* (with-compress-table table (lambda () (compress-tree t))))
         (in (compressed->html t* 1))
         (n (string-length in))
         (text (if (and (string-starts? in "<body>") (string-ends? in "</body>"))
                   (substring in 6 (- n 7)) in))
         (return (ai-translate-callback serial tp st "albert" next table)))
    (return (choices-answer (answer text)))))

(define (translate-step answer)
  ;; one translation process on the buffer, or its selection, as a user
  ;; action (its changes are one step of the undo history)
  (archive-state)
  (start-editing)
  ((make-process (canned-translate answer) 'translate-check))
  (end-editing))

(define (translate-selection body from to answer)
  ;; the body of a buffer holding @body, once its selection from @from to
  ;; @to (paths in the body) is translated by @answer
  (buffer-set-body (current-buffer) (stree->tree body))
  (selection-set (append (buffer-path) from) (append (buffer-path) to))
  (translate-step (lambda (in) answer))
  (tm->stree (buffer-tree)))

(define translate-pending #f)

(define ((deferred-translate answer) serial tp st next)
  ;; canned-translate, whose answer comes when translate-answer-now is
  ;; called, as the answer of an engine comes after a while
  (set! translate-pending
        (lambda () ((canned-translate (lambda (in) answer))
                    serial tp st next))))

(define (translate-later answer)
  (archive-state)
  (start-editing)
  ((make-process (deferred-translate answer) 'translate-check))
  (end-editing))

(define (translate-answer-now)
  (with f translate-pending
    (set! translate-pending #f)
    (when f (f))))

(define (translate-body body answer)
  ;; the body of a buffer holding @body, once translated
  (buffer-set-body (current-buffer) (stree->tree body))
  (translate-step answer)
  (tm->stree (buffer-tree)))

(define (answer-by l)
  ;; the answer to the paragraph whose HTML is the car of an element of @l,
  ;; the HTML itself to the others
  (lambda (in)
    (with a (assoc in l)
      (if a (cadr a) in))))

(define (answer-first body)
  ;; @body for the paragraph of para (below), the HTML itself for the others
  (lambda (in)
    (if (string-starts? in "it is ") body in)))

(define (test-translate-paragraphs)
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer))
         (para '(concat "it is " (em "b"))))
    (dynamic-wind
      (lambda () (noop))
      (lambda ()
        (switch-to-buffer* u)
        ;; one paragraph in <p>: the paragraph, with its markup
        (check= (translate-body
                 `(document ,para "second")
                 (answer-first "<p>Bonjour <div id=\"x0\">b</div></p>"))
                '(document (concat "Bonjour " (em "b")) "second"))
        ;; two paragraphs: two paragraphs of the document
        (check= (translate-body `(document ,para "second")
                                (answer-first "<p>Un</p>\n<p>Deux</p>\n"))
                '(document "Un" "Deux" "second"))
        ;; without <p>, as before
        (check= (translate-body
                 `(document ,para "second")
                 (answer-first "Bonjour <div id=\"x0\">b</div>"))
                '(document (concat "Bonjour " (em "b")) "second"))
        ;; no empty paragraph for a <p></p> at the end (as ai_post)
        (check= (translate-body `(document ,para "second")
                                (answer-first "<p>Un</p><p></p>"))
                '(document "Un" "second"))
        (check= (translate-body `(document ,para "second")
                                (answer-first "<p>Un</p><p>Deux</p><p></p>"))
                '(document "Un" "Deux" "second"))
        ;; a theorem as a paragraph is left a theorem
        (check= (translate-body
                 '(document (theorem (document "a" "b")) "second")
                 (answer-by
                  '(("<div id=\"x0\"><p>a</p>\n<p>b</p></div>"
                     "<div id=\"x0\"><p>un</p>\n<p>deux</p></div>"))))
                '(document (theorem (document "un" "deux")) "second"))
        ;; one undo gives back the paragraph
        (buffer-set-body u (stree->tree `(document ,para "second")))
        (archive-state) (start-editing) (end-editing) (clear-undo-history)
        (translate-step (answer-first "<p>Un</p>\n<p>Deux</p>"))
        (check= (tm->stree (buffer-tree)) '(document "Un" "Deux" "second"))
        (archive-state) (start-editing) (undo 0) (end-editing)
        (check= (tm->stree (buffer-tree)) `(document ,para "second"))
        ;; the process goes on with the next paragraphs, the first being
        ;; translated into two
        (check= (translate-body
                 '(document "one" "two" "three")
                 (answer-by '(("one" "<p>Un</p><p>Deux</p>")
                              ("two" "<p>Trois</p>")
                              ("three" "Quatre"))))
                '(document "Un" "Deux" "Trois" "Quatre"))
        (check= (translate-body
                 '(document "one" "two" "three")
                 (answer-by '(("two" "<p>Deux</p><p>Trois</p>")
                              ("three" "<p>Quatre</p><p>Cinq</p>"))))
                '(document "one" "Deux" "Trois" "Quatre" "Cinq"))
        (check-false (process-running? 'translate-check))
        ;; empty paragraphs are no answer: the text stays
        (check= (translate-body `(document ,para "second")
                                (answer-first "<p></p>"))
                `(document ,para "second"))
        (check= (translate-body `(document ,para "second")
                                (answer-first "<p></p>\n<p></p>"))
                `(document ,para "second"))
        ;; a selection inside a paragraph (make-process puts it in an
        ;; identity tag): one paragraph in <p> takes its place, several
        ;; split the paragraph, also when the engine kept the identity
        ;; (<div id="x0">) around them
        (check= (translate-selection '(document "it is good" "second")
                                     '(0 3) '(0 5) "<p>was</p>")
                '(document "it was good" "second"))
        (check= (translate-selection '(document "it is good" "second")
                                     '(0 3) '(0 5) "<p>was</p><p>and</p>")
                '(document "it was" "and good" "second"))
        (check= (translate-selection
                 '(document "it is good" "second") '(0 3) '(0 5)
                 "<div id=\"x0\"><p>was</p><p>and</p></div>")
                '(document "it was" "and good" "second"))
        (check= (translate-selection
                 '(document (concat "it is " (em "b"))) '(0 0 3) '(0 0 5)
                 "<p>was</p><p>and</p>")
                '(document "it was" (concat "and " (em "b"))))
        ;; in a theorem and in a cell with a document, the paragraph is
        ;; split; inside em, math, a section title, a cell without a
        ;; document, the paragraphs are joined (no text is lost)
        (check= (translate-selection
                 '(document (theorem (document "it is good")))
                 '(0 0 0 3) '(0 0 0 5) "<p>was</p><p>and</p>")
                '(document (theorem (document "it was" "and good"))))
        (check= (translate-selection
                 '(document (tabular (table (row (cell (document
                                                        "it is good"))))))
                 '(0 0 0 0 0 0 3) '(0 0 0 0 0 0 5) "<p>was</p><p>and</p>")
                '(document (tabular (table (row (cell (document
                                                       "it was"
                                                       "and good")))))))
        (check= (translate-selection
                 '(document (tabular (table (row (cell "it is good")))))
                 '(0 0 0 0 0 3) '(0 0 0 0 0 5) "<p>was</p><p>and</p>")
                '(document (tabular (table (row (cell "it was and good"))))))
        (check= (translate-selection
                 '(document (tabular (table (row (cell "it is good")))))
                 '(0 0 0 0 0 0) '(0 0 0 0 0 10) "<p>was</p><p>and</p>")
                '(document (tabular (table (row (cell "was and"))))))
        (check= (translate-selection
                 '(document (em "is good") "second")
                 '(0 0 0) '(0 0 2) "<p>was</p><p>and</p>")
                '(document (em "was and good") "second"))
        (check= (translate-selection
                 '(document (math "xy+z") "second")
                 '(0 0 0) '(0 0 2) "<p>a</p><p>b</p>")
                '(document (math "a b+z") "second"))
        (check= (translate-selection
                 '(document (section "it is good") "second")
                 '(0 0 3) '(0 0 5) "<p>was</p><p>and</p>")
                '(document (section "it was and good") "second"))
        (check-false (process-running? 'translate-check))
        ;; an answer which comes later does not move the cursor of the user,
        ;; who went on editing elsewhere (a selection is cancelled by any
        ;; change of the buffer: post_notify, edit_modify.cpp)
        (buffer-set-body u (stree->tree '(document "it is good" "other")))
        (selection-set (append (buffer-path) '(0 3))
                       (append (buffer-path) '(0 5)))
        (translate-later "<p>was</p><p>and</p>")
        (go-to (append (buffer-path) '(1 2)))
        (translate-answer-now)
        (check= (tm->stree (buffer-tree))
                '(document "it was" "and good" "other"))
        (check= (cursor-path) (append (buffer-path) '(2 2)))
        (check-false (process-running? 'translate-check))
        ;; an answer for a buffer which is no longer the current one
        (buffer-set-body u (stree->tree '(document "it is good" "other")))
        (selection-set (append (buffer-path) '(0 3))
                       (append (buffer-path) '(0 5)))
        (translate-later "<p>was</p><p>and</p>")
        (let* ((v (new-buffer)))
          (switch-to-buffer* v)
          (buffer-set-body v (stree->tree '(document "elsewhere")))
          (go-end)
          (with c (cursor-path)
            (translate-answer-now)
            (check= (current-buffer) v)
            (check= (tm->stree (buffer-get-body u))
                    '(document "it was" "and good" "other"))
            (check= (tm->stree (buffer-get-body v)) '(document "elsewhere"))
            (check= (cursor-path) c))
          (switch-to-buffer* u))
        (check-false (process-running? 'translate-check))
        ;; LanguageTool: an answer of a paragraph with corrections is a
        ;; concat with spell-error, also if it came back in <p>
        (for (wrap (list (lambda (h) h)
                         (lambda (h) (string-append "<p>" h "</p>"))))
          (buffer-set-body u (stree->tree `(document ,para "second")))
          (let* ((t (tree-ref (buffer-tree) 0))
                 (tp (tree->tree-pointer t))
                 (st (tm->stree t))
                 (table (compress-table-new "lantool"))
                 (html (wrap (with-compress-table table
                               (lambda () (compress-html t 1)))))
                 (pos (string-search-forwards "is " 0 html))
                 (json (string-append
                        "{\"software\":{},\"matches\":[{"
                        "\"message\":\"A verb\","
                        "\"replacements\":[{\"value\":\"was\"}],"
                        "\"offset\":" (number->string pos) ","
                        "\"length\":2,\"rule\":{\"id\":\"A\"}}]}"))
                 (end (make-process-end 'lantool-check))
                 (serial process-serial)
                 (went-on #f))
            (with-compress-table table
              (lambda ()
                ((lantool-processed-one serial tp st
                                        (lambda () (set! went-on #t)))
                 html json)))
            (compress-table-free table)
            (end)
            (check= (tm->stree (buffer-tree))
                    `(document (concat "it " (spell-error "is" "A verb" "was")
                                       " " (em "b"))
                               "second"))
            (check-true went-on))))
      (lambda ()
        (for (b (buffer-list))
          (when (nin? b before) (buffer-close b)))
        (when (buffer-exists? old) (switch-to-buffer old))))))

(define (test-compress-tables)
  (check-group "compression tables")
  (let* ((t (stree->tree '(document (concat "it is " (em "b") " " (frac "x" "y")))))
         (default (compress-table-size ""))
         (t1 (compress-table-new "check"))
         (t2 (compress-table-new "check"))
         (h1 (with-compress-table t1 (lambda () (compress-html t 1))))
         (h2 (with-compress-table t2 (lambda () (compress-html t 1)))))
    ;; codes numbered from x0 in each table: the same HTML in both
    (check-false (== t1 t2))
    (check= h1 h2)
    (check-true (>= (string-search-forwards "id=\"x0\"" 0 h1) 0))
    (check= (compress-table-size t1) 2)
    (check= (compress-table-size "") default)
    ;; the round trip, in the table of the compression
    (check= (st (with-compress-table t1 (lambda () (decompress-html h1 1))))
            (st t))
    ;; freed: the table is gone, and its codes lose their markup, not text
    (compress-table-free t1)
    (check= (compress-table-size t1) -1)
    (check-false (in? t1 (compress-table-key-list)))
    (check= (compress-table-size t2) 2)
    (compress-table-free t2)
    (check= (st (with-compress-table t1 (lambda () (decompress-html h1 1))))
            '(document (concat "it is " "b" " " (concat "x" "y"))))
    ;; and neither decompressing nor compressing makes it again
    (check= (compress-table-size t1) -1)
    (with-compress-table t1 (lambda () (compress-html t 1)))
    (check= (compress-table-size t1) -1)
    (check= (st (with-compress-table t1 (lambda () (decompress-html h1 1))))
            '(document (concat "it is " "b" " " (concat "x" "y"))))
    (check= (compress-tables-like "check") '())
    ;; a callback decompresses in its table, which is freed afterwards
    (with t3 (compress-table-new "check")
      (let* ((h (with-compress-table t3 (lambda () (compress-html t 1))))
             (r ((compress-table-callback t3
                   (lambda (s) (st (decompress-html s 1)))) h)))
        (check= r (st t))
        (check= (compress-table-size t3) -1)))
    ;; also when the callback fails
    (with t4 (compress-table-new "check")
      (catch #t
        (lambda () ((compress-table-callback t4 (lambda () (error "fails")))))
        (lambda args #f))
      (check= (compress-table-size t4) -1))
    ;; Correct (an engine without a command: no request is sent) makes one
    ;; table, between the two tables made here, and frees it
    (when (no-ai-request? "no-such-engine")
      (let* ((before (compress-table-new "check"))
             (r (cpp-ai-correct t "english" "no-such-engine"))
             (after (compress-table-new "check")))
        (check-true (tree? r))
        (check= (- (compress-table-serial after) (compress-table-serial before))
                2)
        (compress-table-free before)
        (compress-table-free after)))
    (check= (compress-tables-like "ai") '())
    ;; a chat keeps its table until its session is stopped; the keys of
    ;; two sessions are not confused, even when their names joined by "-"
    ;; are the same (the connections a-b, c and a, b-c)
    (let* ((k1 (cpp-ai-chat-key "a-b" "c"))
           (k2 (cpp-ai-chat-key "a" "b-c"))
           (u (stree->tree '(document (concat "it is " (em "b")))))
           (clipboard (compress-table-size "ai-clipboard")))
      (check-false (== (tree->stree k1) (tree->stree k2)))
      (compress-table-open k1)
      (compress-table-open k2)
      (with-compress-table k1 (lambda () (compress-html t 1)))
      (with-compress-table k2 (lambda () (compress-html u 1)))
      (check= (compress-table-size k1) 2)
      (check= (compress-table-size k2) 1)
      (connection-stop "a-b" "c")
      (check= (compress-table-size k1) -1)
      (check= (compress-table-size k2) 1)
      (check= (compress-table-size "") default)
      (check= (compress-table-size "ai-clipboard") clipboard)
      (connection-stop "a" "b-c")
      (check= (compress-table-size k2) -1))
    ;; LanguageTool: a check (while no spell process runs, the text comes
    ;; back as its own answer, without a request) and its corrections
    (let* ((h #f) (out #f)
           (return (lambda (html r)
                     (set! h html)
                     (let* ((pos (string-search-forwards "is " 0 html))
                            (json (string-append
                                   "{\"software\":{},\"matches\":[{"
                                   "\"message\":\"A verb\","
                                   "\"replacements\":[{\"value\":\"was\"}],"
                                   "\"offset\":" (number->string pos) ","
                                   "\"length\":2,\"rule\":{\"id\":\"A\"}}]}")))
                       (set! out (st (decompress-html
                                      (lantool-correct html json) 1)))))))
      (check-false (process-running? 'spell))
      (when (not (process-running? 'spell))
        (lantool-process* t return)
        (check= h h1)
        (check= out '(document (concat "it " (spell-error "is" "A verb" "was")
                                       " " (em "b") " " (frac "x" "y"))))
        (check= (compress-tables-like "lantool") '())))
    ;; the answer of a translation: an error of the engine (here the body
    ;; of an HTTP error from Albert, as the Qt request now gives it) is no
    ;; translation, which would replace the paragraph by nothing; with the
    ;; errors of #273, it is an "Error: ..." which stops the process
    (let* ((err "{\"detail\":\"Invalid authentication credentials\"}")
           (ok (string-append "{\"choices\":[{\"message\":{\"content\":"
                              "\"<body><p>Bonjour</p></body>\"}}]}")))
      (check-false (ai-translation-answer? "" "albert"))
      (check-false (ai-translation-answer? #f "albert"))
      (check-true (or (not (ai-translation-answer? err "albert"))
                      (string-starts? (cpp-ai-output err "albert") "Error:")))
      (check-true (ai-translation-answer? ok "albert")))
    ;; the callback of one paragraph of the translation process, given these
    ;; answers directly (no request): the error and the empty answer leave
    ;; the paragraph, the answer with text replaces it (with the markup of
    ;; the codes of its table); the table is freed and the process goes on
    (let* ((old (current-buffer))
           (before (buffer-list))
           (u (new-buffer))
           (para '(concat "it is " (em "b")))
           (body (string-append "<body><p>Bonjour <div id=\\\"x0\\\">b"
                                "</div></p></body>"))
           (answers
            (list (list "{\"detail\":\"Invalid authentication credentials\"}"
                        para)
                  (list "" para)
                  (list (string-append "{\"choices\":[{\"message\":"
                                       "{\"content\":\"" body "\"}}]}")
                        ;; (the answer is a document of one paragraph,
                        ;; which takes the place of the paragraph: #526)
                        '(concat "Bonjour " (em "b")))))
           (end (make-process-end 'translate-check))
           (serial process-serial))
      (dynamic-wind
        (lambda () (noop))
        (lambda ()
          (switch-to-buffer* u)
          (for (a answers)
            (buffer-set-body u (stree->tree `(document ,para "second")))
            (let* ((t (tree-ref (buffer-tree) 0))
                   (tp (tree->tree-pointer t))
                   (st (tm->stree t))
                   (table (compress-table-new "translate"))
                   (went-on #f))
              (with-compress-table table (lambda () (compress-tree t)))
              ((ai-translate-callback serial tp st "albert"
                                      (lambda () (set! went-on #t)) table)
               (car a))
              (check= (tm->stree (tree-ref (buffer-tree) 0)) (cadr a))
              (check= (tree-arity (buffer-tree)) 2)
              (check= (compress-table-size table) -1)
              (check-true went-on))))
        (lambda ()
          (end)
          (for (b (buffer-list))
            (when (nin? b before) (buffer-close b)))
          (when (buffer-exists? old) (switch-to-buffer old)))))
    ;; the answers of the translation process in <p> (#526)
    (test-translate-paragraphs)
    ;; the translation process, whose request cannot be launched (an engine
    ;; without a command: cpp-ai-async-eval-command says so, and will not
    ;; call back): the process stops at the first paragraph, whose table is
    ;; freed all the same, and the second one is not tried (one table only,
    ;; between the two made here); both paragraphs are left as they were,
    ;; and their tree pointers detached (one left attached, once collected,
    ;; was used by buffer-close)
    (let* ((m (get-preference "ai")))
      (when (no-ai-request? m)
        (let* ((old (current-buffer))
               (before (buffer-list))
               (u (new-buffer))
               (doc '(document (concat "it is " (em "b")) (concat "c " (em "d")))))
          (dynamic-wind
            (lambda () (noop))
            (lambda ()
              (switch-to-buffer* u)
              (buffer-set-body u (stree->tree doc))
              (go-start)
              (let* ((first (compress-table-new "check"))
                     (dummy (ai-translate* "french"))
                     (last (compress-table-new "check")))
                (check= (- (compress-table-serial last)
                           (compress-table-serial first))
                        2)
                (compress-table-free first)
                (compress-table-free last))
              (check-false (process-running? 'translate))
              (check= (st (buffer-tree)) doc)
              (check= (compress-tables-like "translate") '()))
            (lambda ()
              (ai-abort-translate)
              (for (b (buffer-list))
                (when (nin? b before) (buffer-close b)))
              (when (buffer-exists? old) (switch-to-buffer old))))
          (check-false (process-running? 'translate)))))
    (check= (compress-table-size "") default)))

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
  ;; and the space at the very end of a snippet
  (check= (serialize-texmacs-snippet (stree->tree "a ")) "a\\ ")
  (check= (st (parse-texmacs-snippet (serialize-texmacs-snippet (stree->tree "a "))))
          '(document "a "))
  (check= (st (parse-texmacs-snippet (serialize-texmacs-snippet
                                      (stree->tree '(document "a" "b ")))))
          '(document "a" "b "))
  (check= (st (parse-texmacs-snippet (serialize-texmacs-snippet
                                      (stree->tree '(concat (em "a") " ")))))
          '(document (concat (em "a") " ")))
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

(define (fastest-run thunk tries)
  ;; @thunk timed up to @tries times, until once in less than 2 s: the
  ;; shared runners of CI are sometimes slower for a moment (the tmml
  ;; check took 2003 and 2101 ms there), while a slow conversion stays slow
  (with r (timed thunk)
    (if (or (<= tries 1) (< (cdr r) 2000)) r
        (with r2 (fastest-run thunk (- tries 1))
          (if (< (cdr r2) (cdr r)) r2 r)))))

(define (check-fast what thunk expected)
  ;; @thunk gives @expected in less than 2 s (the fastest of up to 3 runs)
  (with r (check-run (lambda () (fastest-run thunk 3)))
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
  (with body (cons 'document (make-list 2000 "x"))
    (check= (length (cdr (import (export body "html-snippet") "html-snippet"))) 2000))
  ;; 4000 elements at the top, a paragraph and a table for the fraction
  (with body (cons 'document (make-list 2000 '(concat "a" (with "mode" "math"
                                                                (frac "a" "b")))))
    (check= (length (cdr (import (export body "html-snippet") "html-snippet"))) 4000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The export to PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The native PDF writer does not compress the objects of the outline and
;; of the links, which are read in the file. The buffer is exported as soon
;; as it is loaded, as texmacs -c does: its hyperlinks are in the PDF. The
;; title of a bookmark is a hexadecimal string (UTF-16BE) without escapes,
;; and a URI a literal string in which ( ) and \ are escaped.
(define (test-pdf-export)
  (check-group "pdf export")
  (if (not (and (supports-native-pdf?) (get-boolean-preference "native pdf")))
      (images-skip "pdf export" "no native PDF writer")
      (let* ((tm (url-append (temp-dir) "links.tm"))
             (pdf (url-append (temp-dir) "links.pdf"))
             (old (current-buffer))
             (before (buffer-list))
             (body `(document
                      (section "f(x)")
                      (concat "See " (hlink "one" "https://example.org/a)b")
                              " and " (hlink "two" "https://example.org/a\\b")
                              "."))))
        (string-save (serialize-texmacs (stree->tree (tmfile body))) tm)
        (load-buffer tm :strict)
        (export-buffer pdf)
        (for (b (buffer-list))
          (when (nin? b before) (buffer-close b)))
        (when (buffer-exists? old) (switch-to-buffer old))
        (with s (if (url-exists? pdf) (string-load pdf) "")
          (define (has? what) (>= (string-search-forwards what 0 s) 0))
          (check-true (string-starts? s "%PDF-"))
          (check-true (has? "/Title <FEFF003100200066002800780029>"))
          (check-true (has? "/URI (https://example.org/a\\)b)"))
          (check-true (has? "/URI (https://example.org/a\\\\b)"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The images written by an export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #697: the converters put the images next to the file which they write, and
;; only knew that file when called through export-buffer-main; a target which
;; cannot be written crashed in make_eps.

(define (export-png)
  (string-load (url-append "$TEXMACS_PATH/misc/images" "fancy-c.png")))

(define (for-latex n)
  ;; the name in a document is /D/a/x.png on Windows; LaTeX reads D:/a/x.png
  (if (os-mingw?)
      (string-append (substring n 1 2) ":" (substring n 2 (string-length n)))
      n))

(define (embedded-image-sample)
  `(document (concat "a" (image (tuple (raw-data ,(export-png)) "png")
                                "1cm" "" "" "")
                     "b")))

(define (eps-suffix)
  (if (get-boolean-preference "native pdf") "pdf" "eps"))

(define (has-text? what s)
  (>= (string-search-forwards what 0 s) 0))

(define (check-image-file u)
  ;; @u holds the picture
  (with s (if (url-exists? u) (string-load u) "")
    (cond ((== (eps-suffix) "eps") (check-true (string-starts? s "%!PS")))
          ((supports-native-pdf?)
           (check-true (string-starts? s "%PDF-"))
           (check-true (has-text? "/Subtype /Image" s)))
          (else (check-true (!= s ""))))))

(define (check-latex-image root suffix)
  ;; the file @root.@suffix of the temporary directory shows its image, which
  ;; is next to it and is called after it
  (let* ((tex (url-append (temp-dir)
                          (if (== suffix "") root
                              (string-append root "." suffix))))
         (image (string-append root "-1." (eps-suffix)))
         (doc (if (url-exists? tex) (string-load tex) "")))
    (check-true (has-text? (string-append "{" image "}") doc))
    (check-image-file (url-append (temp-dir) image))))

(define (folder-files dir)
  (url->list (url-expand (url-complete (url-append dir (url-wildcard "*"))
                                       "fr"))))

(define (image-name-in s)
  ;; the name in the last braces opened in the LaTeX @s: the includegraphics
  (with b (string-search-backwards "{" (string-length s) s)
    (substring s (+ b 1) (string-search-forwards "}" b s))))

(define (linked-image-size image exporter)
  ;; the size of the image file of a document which links @image, exported
  ;; by @exporter to a folder of its own
  (with-sample-buffer `(document (image ,image "1cm" "" "" ""))
    (lambda (name)
      (let* ((dir (url-append (temp-dir) name))
             (img (url-append dir (string-append "x-1." (eps-suffix)))))
        (system-mkdir dir)
        (exporter (url-append dir "x.tex"))
        (if (url-exists? img) (url-size img) 0)))))

(define (test-export-images)
  (check-group "export: the images go next to the target")
  (with-sample-buffer (embedded-image-sample)
    (lambda (name)
      (let ((target current-save-target)
            (source current-save-source))
        ;; called directly, as from a script
        (check-false (buffer-export (current-buffer)
                                    (url-append (temp-dir) "script.tex")
                                    "latex"))
        (check-latex-image "script" "tex")
        (check-true (== current-save-target target))
        ;; each export has its own images, also after another one
        (check-false (buffer-export (current-buffer)
                                    (url-append (temp-dir) "script2.tex")
                                    "latex"))
        (check-latex-image "script2" "tex")
        ;; as from the menu File > Export
        ((buffer-exporter "latex") (url-append (temp-dir) "menu.tex"))
        (check-latex-image "menu" "tex")
        (check-true (== current-save-target target))
        (check-false (tree-export (buffer-tree)
                                  (url-append (temp-dir) "tree.tex") "latex"))
        (check-latex-image "tree" "tex")
        ;; a name without the suffix tex
        (check-false (buffer-export (current-buffer)
                                    (url-append (temp-dir) "other.ltx")
                                    "latex"))
        (check-latex-image "other" "ltx")
        ;; html
        (check-false (buffer-export (current-buffer)
                                    (url-append (temp-dir) "script.html")
                                    "html"))
        (check-true (url-exists? (url-append (temp-dir) "script-1.png")))
        (check-true (== current-save-source source))
        ;; any other name
        (check-false (buffer-export (current-buffer)
                                    (url-append (temp-dir) "text.txt")
                                    "latex"))
        (check-latex-image "text" "txt")
        (check-false (buffer-export (current-buffer)
                                    (url-append (temp-dir) "notes")
                                    "latex"))
        (check-latex-image "notes" "")
        ;; saving does not make a target for the next conversion
        (save-buffer)
        (check-true (== current-save-target target)))))
  (check-group "conversion without a target: the images are temporary")
  (let* ((dir (url-append (temp-dir) "loose"))
         (ext (eps-suffix))
         (doc (url-append dir "doc.tm"))
         (sample (embedded-image-sample))
         (old (current-buffer)))
    (system-mkdir dir)
    (string-save (serialize-texmacs (stree->tree (tmfile "x"))) doc)
    (load-buffer doc :strict)
    ;; as for the text sent to an AI, the labels of a plugin, ...
    (let* ((before (length (folder-files dir)))
           (first (image-name-in (export sample "latex-snippet")))
           (second (image-name-in (export sample "latex-snippet"))))
      (check-true (url-exists? (system->url first)))
      (check-image-file (system->url first))
      (check-true (url-descends? (system->url first) (url-temp-dir)))
      (check-false (== first second))
      ;; a name which LaTeX can read on Windows too: a drive and slashes
      (check-true (string-starts? second (image-name-for-program (url-temp-dir))))
      (check-false (has-text? "\\" second))
      ;; Windows: the drive, its colon and a slash (X:/), only there
      (when (os-mingw?)
        (check-true (and (> (string-length second) 3)
                         (char-alphabetic? (string-ref second 0))
                         (== (substring second 1 3) ":/"))))
      (export sample "html-snippet")
      (check= (length (folder-files dir)) before))
    (buffer-close doc)
    (when (buffer-exists? old) (switch-to-buffer old)))
  (check-group "the name of an image in LaTeX stays as it is")
  (check= (slashes-of-name "D:\\a\\b\\x.pdf") "D:/a/b/x.pdf")
  (check= (slashes-of-name "/a/b c/x.pdf") "/a/b c/x.pdf")
  ;; through the conversion: a long name with spaces is not broken
  (with-sample-buffer (embedded-image-sample)
    (lambda (name)
      (let* ((root "a long name with spaces so that the line must be broken at once")
             (tex (url-append (temp-dir) (string-append root ".tex"))))
        (check-false (buffer-export (current-buffer) tex "latex"))
        (check-true (has-text? (string-append "{" root "-1." (eps-suffix) "}")
                               (if (url-exists? tex) (string-load tex) ""))))))
  ;; a space, a letter which is not ASCII, an underscore: no escapes, no break
  (with name (string-append "/some/long/folder with a space/and_an_underscore/"
                            "and a long name so that the line must be broken/"
                            "h\xc3\xa9/image-1.pdf")
    (check= (serialize-latex `(includegraphics (!file-name ,name)))
            (string-append "\\includegraphics{" name "}")))
  (check-group "conversion for MathJax: the images are temporary")
  (let* ((formula `(document (math (concat "x+" (graphics ""
                                                  (point "0" "0")
                                                  (line (point "0" "0")
                                                        (point "1" "1")))))))
         (html (html-export-of-buffer formula
                                      '("texmacs->html:mathjax" . "on")))
         (image (url-append (temp-dir) (string-append "image-1." (eps-suffix))))
         (beside (url-append (temp-dir) (string-append "sample-"
                                         (number->string sample-number)
                                         "-1." (eps-suffix)))))
    (check-true (!= html ""))
    (check-false (url-exists? image))
    (check-false (url-exists? beside)))
  (check-group "conversion for the clipboard: the images")
  (let* ((dir (url-append (temp-dir) "clipboard"))
         (ext (eps-suffix))
         (doc (url-append dir "doc.tm"))
         (sample (embedded-image-sample))
         (other `(document (image (tuple (raw-data ,(export-png)) "png")
                                  "2cm" "" "" "")))
         (old (current-buffer)))
    (define (copy t fm)
      (with-global copying-to-clipboard? #t (export t fm)))
    (system-mkdir dir)
    (string-save (export-png) (url-append dir "export-linked.png"))
    (string-save (serialize-texmacs (stree->tree (tmfile "x"))) doc)
    (load-buffer doc :strict)
    ;; next to the document; the same copy, the same file; another one, another
    (let* ((name (image-name-in (copy sample "latex-snippet")))
           (u (url-append dir (system->url name)))
           (n (length (folder-files dir))))
      (check-true (string-starts? name "image-"))
      (check-false (string-starts? name "image-1."))
      (check-image-file u)
      (check= (image-name-in (copy sample "latex-snippet")) name)
      (check= (length (folder-files dir)) n)
      (with name2 (image-name-in (copy other "latex-snippet"))
        (check-false (== name2 name))
        (check-true (url-exists? (url-append dir (system->url name2))))))
    (let* ((n (length (folder-files dir)))
           (s1 (copy sample "html-snippet"))
           (m (length (folder-files dir)))
           (s2 (copy sample "html-snippet")))
      (check= m (+ n 1))
      (check= (length (folder-files dir)) m)
      (check= s1 s2))
    ;; a linked image is the one of the document
    (with s (copy '(document (image "export-linked.png" "1cm" "" "" ""))
                  "latex-snippet")
      (check-true (has-text? "{export-linked.png}" s)))
    (buffer-close doc)
    ;; a document which is not on disk: an absolute name in the home
    (with u (new-buffer)
      (switch-to-buffer u)
      (let* ((name (image-name-in (copy sample "latex-snippet")))
             (file (system->url name)))
        (check-true (url-descends? file
                                   (string->url
                                    "$TEXMACS_HOME_PATH/system/images")))
        (check-image-file file)
        (system-remove file))
      (buffer-pretend-saved u)
      (buffer-close u))
    (when (buffer-exists? old) (switch-to-buffer old)))
  (check-group "export: the name of an image, as it is (#706)")
  ;; an absolute name with spaces and an accented letter, long enough for the
  ;; line to be wrapped: the text has it whole (not h{\'e}, nor on two lines)
  (let* ((dir (string-append (url->unix (temp-dir))
                             "/a folder with a rather long name, so that"
                             " the line is wrapped, h"
                             (string (integer->char 195) (integer->char 169))))
         (name (string-append dir "/linked.png")))
    (system-mkdir (unix->url dir))
    (string-save (export-png) (unix->url name))
    (with s (export `(document (image ,name "1cm" "" "" "")) "latex-snippet")
      (check-true (has-text? (string-append "{" (for-latex name) "}") s))))
  ;; a %, which LaTeX would read as a comment, is given by a macro; the
  ;; space which ends the macro is skipped, and \space gives a space after %
  (let* ((dir (url->unix (temp-dir)))
         (text (lambda (n)
                 (export `(document (image ,(string-append dir "/" n)
                                           "1cm" "" "" ""))
                         "latex-snippet"))))
    (for (n (list "50%done.png" "a% b.png"))
      (string-save (export-png) (unix->url (string-append dir "/" n))))
    (check-true (has-text? (string-append (for-latex dir) "/50\\csname @percentchar\\endcsname done.png}")
                           (text "50%done.png")))
    (check-true (has-text? (string-append (for-latex dir) "/a\\csname @percentchar\\endcsname \\space b.png}")
                           (text "a% b.png"))))
  (check-group "copy to the clipboard: the images of a real copy (#706)")
  ;; through selection_set, as Edit > Copy to > LaTeX: only a copy to the
  ;; clipboard (primary) keeps its image next to the document, not one to
  ;; the selection of the mouse; on the offscreen platform only, whose
  ;; clipboard is that of the process, not the one of the developer
  (if (!= (getenv "QT_QPA_PLATFORM") "offscreen")
      (images-skip "real copy" "not on the offscreen platform")
      (let* ((dir (url-append (temp-dir) "real-copy"))
             (doc (url-append dir "doc.tm"))
             (old (current-buffer))
             (images (lambda ()
                       (list-filter (map (lambda (u) (url->string (url-tail u)))
                                         (folder-files dir))
                                    (lambda (s) (string-starts? s "image-"))))))
        (system-mkdir dir)
        (string-save (serialize-texmacs
                      (stree->tree `(document (TeXmacs ,(texmacs-version))
                                              (style "generic")
                                              (body ,(embedded-image-sample)))))
                     doc)
        (load-buffer doc :strict)
        ;; the buffer is closed and the selections cleared also after an
        ;; error, which would otherwise leave them to the next suites
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (switch-to-buffer doc)
            ;; the selection is found in the boxes (without them: a crash)
            (update-forced)
            (select-all)
            (clipboard-copy-export "latex" "mouse")
            (check= (images) '())
            (clipboard-copy-export "latex" "primary")
            (check= (length (images)) 1))
          (lambda ()
            (clipboard-clear "primary")
            (clipboard-clear "mouse")
            (buffer-pretend-saved doc)
            (buffer-close doc)
            (when (buffer-exists? old) (switch-to-buffer old))))))
  (check-group "export: a target which cannot be written is an error")
  (with-sample-buffer (embedded-image-sample)
    (lambda (name)
      (let* ((dir (url-append (temp-dir) "no-such-folder"))
             (snippet (stree->tree '(document "x")))
             (none (url-append dir (string-append "x." (eps-suffix))))
             (extents (print-snippet none snippet #t)))
        ;; the size of the snippet is known all the same
        (check= (length extents) 10)
        (check-false (url-exists? none))
        (check-true (buffer-export (current-buffer)
                                   (url-append dir "x.tex") "latex"))
        (check-true (buffer-export (current-buffer)
                                   (url-append dir "x.pdf") "pdf"))
        (check-false (url-exists? dir)))))
  (check-group "export: a failure is reported, and leaves no file (#705)")
  (with-sample-buffer (embedded-image-sample)
    (lambda (name)
      (let* ((messages '())
             (capture (lambda (msg . args) (set! messages (cons msg messages))))
             (last-message (lambda () (object->string (car messages))))
             (target (url-append (temp-dir) "failed.txt"))
             (tex (url-append (temp-dir) "image-failed.tex"))
             (image (url-append (temp-dir)
                                (string-append "image-failed-1."
                                               (eps-suffix)))))
        ;; a conversion which fails: an error, and no file (not the text
        ;; "Error: bad format or data", nor an empty file for a converter
        ;; which throws)
        (check-true (buffer-export (current-buffer) target "no-such-format"))
        (check-false (url-exists? target))
        (with-global set-message capture
          (export-buffer-main (current-buffer) target "no-such-format"
                              '(:overwrite)))
        (check-false (url-exists? target))
        (check-true (string-starts? (last-message) "(concat \"Could not save"))
        ;; the text is written, but not its image: said so
        (system-mkdir image)
        (image-write-failures-reset)
        (check-false (buffer-export (current-buffer) tex "latex"))
        (check= (image-write-failures) (list image))
        (with-global set-message capture
          (export-buffer-main (current-buffer) tex "latex" '(:overwrite)))
        (check-true (has-text? "an image could" (last-message)))
        ;; the message is written as a Scheme string: a \ of the name doubled
        (check-true (has-text? (let* ((o (object->string (url->system image)))
                                      (n (string-length o)))
                                 (substring o 1 (- n 1)))
                               (last-message)))
        (check= (image-write-failures) '())
        (system-rmdir image)
        ;; and when it can be written, as usual
        (with-global set-message capture
          (export-buffer-main (current-buffer) tex "latex" '(:overwrite)))
        (check-false (has-text? "an image could" (last-message)))
        (check-image-file image)
        ;; a LaTeX conversion which fails inside its C++ part gives an empty
        ;; text: no file either (a sqrt without argument fails in tmtex)
        (check-true (tree-export (stree->tree '(document (sqrt))) target
                                 "latex"))
        (check-false (url-exists? target))
        (check-false (buffer-export (current-buffer) target "latex"))
        (check-true (url-exists? target))
        ;; a PDF written twice in the same second is written both times
        ;; (its time stamp, in seconds, does not change)
        (with pdf (url-append (temp-dir) "twice.pdf")
          (check-false (buffer-export (current-buffer) pdf "pdf"))
          (check-false (buffer-export (current-buffer) pdf "pdf"))))))
  (check-group "export: a relative target is in the current directory")
  (if (not (and (supports-native-pdf?) (get-boolean-preference "native pdf")))
      (images-skip "relative target" "no native PDF writer")
      (let* ((rel (system->url "convert-more-test-697.pdf"))
             (abs (url-append (url-pwd) rel)))
        ;; the only file outside the temporary directory, removed at once
        (if (or (url-exists? abs) (not (folder-writable? (url-pwd))))
            (images-skip "relative target" "current directory not writable")
            (begin
              (print-snippet rel (stree->tree '(document "x")) #t)
              (check-true (url-exists? abs))
              (system-remove abs)))))
  (check-group "export: a linked image next to the document")
  (string-save (export-png) (url-append (temp-dir) "export-linked.png"))
  (if (not (url-exists-in-path? "gs"))
      (images-skip "linked image" "gs is needed to convert the png")
      (let* ((buffer-exporter*
              (lambda (u) (buffer-export (current-buffer) u "latex")))
             (tree-exporter
              (lambda (u) (tree-export (buffer-tree) u "latex")))
             ;; the size of the placeholder of a missing image
             (missing (linked-image-size "export-missing.png"
                                         buffer-exporter*)))
        (check-true (!= (linked-image-size "export-linked.png"
                                           buffer-exporter*)
                        missing))
        (check-true (!= (linked-image-size "export-linked.png"
                                           tree-exporter)
                        missing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (convert-more-test-failures)
  (check-suite "convert-more")
  (when (url-exists? (temp-dir)) (system-rmdir-recursive (temp-dir)))
  (system-mkdir (temp-dir))
  (for-each run-group
            (list test-mathml-export test-mathml-html test-html-formula-labels
                  test-html-mathjax-formulas
                  test-mathml-import
                  test-mathml-round-trip test-xml-parser test-xml-names
                  test-tmml-special test-cork-table test-extra-symbols test-cork-symbols
                  test-cork-bytes test-cork-unicode test-other-encodings
                  test-images test-image-formats test-code-formats
                  test-format-registry test-vernac test-json test-ai-answers
                  test-compress test-compress-tables
                  test-serialize-special test-big-document test-pdf-export
                  test-export-images))
  (when (url-exists? (temp-dir)) (system-rmdir-recursive (temp-dir)))
  (check-end))
