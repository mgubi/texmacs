
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : office-test.scm
;; DESCRIPTION : tests of the converters of the office formats
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tests cover
;;
;;   - the zip archives (zip-archive?, zip-unpack, zip-pack);
;;   - convert/office/docxin.scm and odtin.scm: the readers of Word and of
;;     OpenDocument texts, on small archives which are made here from
;;     their XML, so that a test shows what it reads;
;;   - convert/office/omml.scm: the formulas of Word as MathML;
;;   - convert/office/officetm.scm: office trees as TeXmacs trees;
;;   - convert/office/tmoffice.scm: TeXmacs trees as office trees;
;;   - convert/office/docxout.scm and odtout.scm: the writers, by reading
;;     back what they write, and the round trips through both formats;
;;   - the formats docx and odt.

(texmacs-module (check office-test)
  (:use (check check-lib)
        (convert office office-tools)
        (convert office docxin)
        (convert office odtin)
        (convert office omml)
        (convert office officetm)
        (convert office tmoffice)
        (convert office docxout)
        (convert office odtout)))

(define (bytes . l) (list->string (map integer->char l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zip archives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An archive made by zip: a.txt, four lines "hello hello hello hello hello
;; hello" which are deflated, and b.txt, "TeXmacs", which is stored.
(define zip-sample
  (bytes
    80 75 3 4 20 0 2 0 8 0 178 133 72 93 234 46 136 51 15 0 0 0 144 0 0 0 5
    0 0 0 97 46 116 120 116 203 72 205 201 201 87 200 192 71 114 101 208 81
    13 0 80 75 3 4 10 0 2 0 0 0 178 133 72 93 18 158 41 29 7 0 0 0 7 0 0 0 5
    0 0 0 98 46 116 120 116 84 101 88 109 97 99 115 80 75 1 2 30 3 20 0 2 0
    8 0 178 133 72 93 234 46 136 51 15 0 0 0 144 0 0 0 5 0 0 0 0 0 0 0 1 0 0
    0 164 129 0 0 0 0 97 46 116 120 116 80 75 1 2 30 3 10 0 2 0 0 0 178 133
    72 93 18 158 41 29 7 0 0 0 7 0 0 0 5 0 0 0 0 0 0 0 1 0 0 0 164 129 50 0 0
    0 98 46 116 120 116 80 75 5 6 0 0 0 0 2 0 2 0 102 0 0 0 92 0 0 0 0 0))

(define (test-zip)
  (check-group "zip")
  (check-true (zip-archive? zip-sample))
  (check-false (zip-archive? "hello"))
  (check-false (zip-archive? ""))
  (let* ((line "hello hello hello hello hello hello\n")
         (l (zip-unpack zip-sample)))
    (check= (length l) 4)
    (check= (car l) "a.txt")
    (check= (cadr l) (string-append line line line line))
    (check= (cddr l) '("b.txt" "TeXmacs")))
  (check= (zip-unpack "hello") '())
  ;; what is packed is unpacked the same, bytes of any value included
  (let* ((bin (bytes 0 1 2 255 0 80 75 3 4 10 13))
         (z (zip-pack '("mimetype" "dir/a.xml" "bin") (list "x/y" "<a/>" bin))))
    (check-true (zip-archive? z))
    (check= (zip-unpack z) (list "mimetype" "x/y" "dir/a.xml" "<a/>" "bin" bin))
    ;; the same entries make the same archive
    (check= (zip-pack '("mimetype" "dir/a.xml" "bin") (list "x/y" "<a/>" bin)) z))
  (check= (zip-unpack (zip-pack '() '())) '())
  ;; an archive which is cut is no archive
  (check= (zip-unpack (substring zip-sample 0 100)) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define docx-ns
  (string-append
    " xmlns:w='http://schemas.openxmlformats.org/wordprocessingml/2006/main'"
    " xmlns:r='http://schemas.openxmlformats.org/officeDocument/2006/relationships'"
    " xmlns:m='http://schemas.openxmlformats.org/officeDocument/2006/math'"))

;; the styles of the tests: the identifiers are not the English names
(define docx-styles
  (string-append
    "<w:styles" docx-ns ">"
    "<w:style w:type='paragraph' w:styleId='Titre1'><w:name w:val='heading 1'/>"
    "<w:rPr><w:b/></w:rPr></w:style>"
    "<w:style w:type='paragraph' w:styleId='Sous'><w:name w:val='My heading'/>"
    "<w:basedOn w:val='Titre1'/><w:pPr><w:outlineLvl w:val='1'/></w:pPr></w:style>"
    "<w:style w:type='paragraph' w:styleId='Titre'><w:name w:val='Title'/>"
    "<w:pPr><w:jc w:val='center'/></w:pPr></w:style>"
    "<w:style w:type='paragraph' w:styleId='Citation'><w:name w:val='Quote'/></w:style>"
    "<w:style w:type='paragraph' w:styleId='Code'><w:name w:val='Source Code'/></w:style>"
    "<w:style w:type='paragraph' w:styleId='TM1'><w:name w:val='toc 1'/></w:style>"
    "<w:style w:type='paragraph' w:styleId='Legende'><w:name w:val='caption'/></w:style>"
    "<w:style w:type='table' w:styleId='Grille'><w:name w:val='My table'/>"
    "<w:tblPr><w:tblBorders><w:bottom w:val='single'/></w:tblBorders></w:tblPr>"
    "<w:tblStylePr w:type='firstRow'><w:tcPr><w:tcBorders><w:top w:val='single'/>"
    "<w:bottom w:val='single'/></w:tcBorders><w:shd w:fill='DDDDDD'/></w:tcPr>"
    "</w:tblStylePr></w:style>"
    "<w:style w:type='character' w:styleId='Accent'><w:name w:val='Emphasis'/>"
    "<w:rPr><w:i/></w:rPr></w:style>"
    "<w:style w:type='character' w:styleId='Lien'><w:name w:val='Hyperlink'/>"
    "<w:rPr><w:u w:val='single'/></w:rPr></w:style>"
    "</w:styles>"))

(define docx-numbering
  (string-append
    "<w:numbering" docx-ns ">"
    "<w:abstractNum w:abstractNumId='0'>"
    "<w:lvl w:ilvl='0'><w:numFmt w:val='bullet'/></w:lvl>"
    "<w:lvl w:ilvl='1'><w:numFmt w:val='decimal'/></w:lvl></w:abstractNum>"
    "<w:num w:numId='1'><w:abstractNumId w:val='0'/></w:num>"
    "<w:num w:numId='2'><w:abstractNumId w:val='0'/></w:num>"
    "</w:numbering>"))

(define docx-rels
  (string-append
    "<Relationships>"
    "<Relationship Id='rId1' Target='https://www.texmacs.org' TargetMode='External'/>"
    "<Relationship Id='rId2' Target='media/pic.png'/>"
    "</Relationships>"))

(define docx-footnotes
  (string-append
    "<w:footnotes" docx-ns ">"
    "<w:footnote w:type='separator' w:id='0'><w:p><w:r><w:t>-</w:t></w:r></w:p></w:footnote>"
    "<w:footnote w:id='2'><w:p><w:r><w:footnoteRef/></w:r>"
    "<w:r><w:t xml:space='preserve'> The note.</w:t></w:r></w:p></w:footnote>"
    "</w:footnotes>"))

(define (docx-archive body)
  ;; a Word document with this body
  (zip-pack
    '("[Content_Types].xml" "word/document.xml" "word/styles.xml"
      "word/numbering.xml" "word/_rels/document.xml.rels"
      "word/footnotes.xml" "word/media/pic.png")
    (list "<Types/>"
          (string-append "<?xml version='1.0'?><w:document" docx-ns "><w:body>"
                         body "<w:sectPr/></w:body></w:document>")
          docx-styles docx-numbering docx-rels docx-footnotes "PNGDATA")))

(define (docx body)
  ;; the blocks of the office tree of a Word document with this body
  (cdr (parse-docx-document (docx-archive body))))

(define (wp . l)
  ;; a paragraph of runs of text
  (string-append "<w:p>" (apply string-append l) "</w:p>"))

(define (wr text . props)
  (string-append "<w:r><w:rPr>" (apply string-append props) "</w:rPr><w:t xml:space='preserve'>"
                 text "</w:t></w:r>"))

(define (wstyle id) (string-append "<w:pPr><w:pStyle w:val='" id "'/></w:pPr>"))

(define (test-docx-text)
  (check-group "docx text")
  (check= (docx "") '())
  (check= (docx (wp (wr "plain"))) '((p "plain")))
  ;; the runs with the same properties are one piece of text
  (check= (docx (wp (wr "a ") (wr "b" "<w:i/>") (wr "c" "<w:i/>") (wr " d" "<w:b/>")))
          '((p "a " (em "bc") (strong " d"))))
  (check= (docx (wp (wr "x" "<w:b/><w:i/>")))
          '((p (strong (em "x")))))
  ;; a property which is unset, a style of runs, a font for code
  (check= (docx (wp (wr "x" "<w:b w:val='0'/>"))) '((p "x")))
  (check= (docx (wp (wr "x" "<w:rStyle w:val='Accent'/>"))) '((p (em "x"))))
  (check= (docx (wp (wr "x" "<w:rStyle w:val='Accent'/><w:i w:val='0'/>")))
          '((p "x")))
  (check= (docx (wp (wr "f()" "<w:rFonts w:ascii='Courier New'/>")))
          '((p (code "f()"))))
  (check= (docx (wp (wr "H") (wr "2" "<w:vertAlign w:val='subscript'/>") (wr "O")))
          '((p "H" (sub "2") "O")))
  (check= (docx (wp (wr "u" "<w:u w:val='single'/>") (wr "s" "<w:strike/>")
                    (wr "c" "<w:smallCaps/>")))
          '((p (underline "u") (strike "s") (smallcaps "c"))))
  ;; a color (black is none), a highlight
  (check= (docx (wp (wr "r" "<w:color w:val='FF0000'/>") (wr "s" "<w:color w:val='FF0000'/>")
                    (wr "k" "<w:color w:val='000000'/>") (wr "h" "<w:highlight w:val='yellow'/>")))
          '((p (color (@ (value "#ff0000")) "rs") "k" (mark "h"))))
  ;; line breaks, tabs; an empty paragraph is left out
  (check= (docx (wp "<w:r><w:t>a</w:t><w:br/><w:t>b</w:t><w:tab/><w:t>c</w:t></w:r>"))
          '((p "a" (br) "b" (tab) "c")))
  (check= (docx (string-append (wp (wr "a")) (wp) (wp (wr "b"))))
          '((p "a") (p "b")))
  ;; text which was inserted is there, text which was deleted is not
  (check= (docx (wp "<w:ins>" (wr "new") "</w:ins><w:del><w:r><w:delText>old</w:delText></w:r></w:del>"))
          '((p "new"))))

(define (test-docx-links)
  (check-group "docx links")
  ;; the address of a link is a relation; its look is not kept
  (check= (docx (wp "<w:hyperlink r:id='rId1'>" (wr "site" "<w:rStyle w:val='Lien'/>")
                    "</w:hyperlink>"))
          '((p (link (@ (href "https://www.texmacs.org")) "site"))))
  (check= (docx (wp "<w:hyperlink w:anchor='there'>" (wr "see") "</w:hyperlink>"))
          '((p (link (@ (href "#there")) "see"))))
  (check= (docx (wp "<w:bookmarkStart w:id='1' w:name='there'/>" (wr "x")
                    "<w:bookmarkEnd w:id='1'/>"))
          '((p (bookmark (@ (name "there"))) "x")))
  ;; a bookmark between two paragraphs belongs to the next one
  (check= (docx (string-append "<w:bookmarkStart w:id='1' w:name='b'/>" (wp (wr "x"))))
          '((p (bookmark (@ (name "b"))) "x")))
  ;; fields: a link, a reference, and a field which is only its result
  (let ((field (lambda (instr result)
                 (wp "<w:r><w:fldChar w:fldCharType='begin'/></w:r>"
                     "<w:r><w:instrText>" instr "</w:instrText></w:r>"
                     "<w:r><w:fldChar w:fldCharType='separate'/></w:r>"
                     (wr result)
                     "<w:r><w:fldChar w:fldCharType='end'/></w:r>"))))
    (check= (docx (field " HYPERLINK \"http://a.b\" " "text"))
            '((p (link (@ (href "http://a.b")) "text"))))
    (check= (docx (field " REF fig1 \\h " "Figure 1"))
            '((p (ref (@ (name "fig1")) "Figure 1"))))
    (check= (docx (field " PAGE " "3")) '((p "3"))))
  (check= (docx (wp "<w:fldSimple w:instr=' REF sec \\h '>" (wr "2.1") "</w:fldSimple>"))
          '((p (ref (@ (name "sec")) "2.1"))))
  ;; a footnote, without its mark and the space after it
  (check= (docx (wp (wr "a") "<w:r><w:footnoteReference w:id='2'/></w:r>"))
          '((p "a" (note (p "The note."))))))

(define (test-docx-blocks)
  (check-group "docx blocks")
  ;; what a paragraph is comes from the name of its style, or of the style
  ;; this one is based on, or from its level in the outline
  (check= (docx (wp (wstyle "Titre1") (wr "Intro")))
          '((p (@ (role "heading") (level "1")) "Intro")))
  (check= (docx (wp (wstyle "Sous") (wr "Sub")))
          '((p (@ (role "heading") (level "1")) "Sub")))
  (check= (docx (wp "<w:pPr><w:outlineLvl w:val='2'/></w:pPr>" (wr "Deep")))
          '((p (@ (role "heading") (level "3")) "Deep")))
  (check= (docx (wp (wstyle "Titre") (wr "T")))
          '((p (@ (role "title") (align "center")) "T")))
  (check= (docx (wp (wstyle "Citation") (wr "q"))) '((p (@ (role "quote")) "q")))
  (check= (docx (wp (wstyle "Code") (wr "x=1"))) '((p (@ (role "code")) "x=1")))
  (check= (docx (wp "<w:pPr><w:jc w:val='right'/></w:pPr>" (wr "r")))
          '((p (@ (align "right")) "r")))
  ;; a large first letter is the start of the next paragraph
  (check= (docx (string-append (wp "<w:pPr><w:framePr w:dropCap='drop' w:lines='3'/></w:pPr>" (wr "D"))
                               (wp (wr "rop caps"))))
          '((p "Drop caps")))
  ;; the entries of a table of contents
  (check= (docx (wp "<w:pPr><w:pStyle w:val='TM1'/></w:pPr>" (wr "Intro 1")))
          '((p (@ (role "toc")) "Intro 1")))
  (check= (docx (wp "<w:pPr><w:pageBreakBefore/></w:pPr>" (wr "x")))
          '((pagebreak) (p "x")))
  ;; an image: its file in the archive, its size (360000 units are 1 cm)
  (check= (docx (wp "<w:r><w:drawing><wp:inline><wp:extent cx='720000' cy='360000'/>"
                    "<wp:docPr id='1' name='p' descr='A picture'/>"
                    "<a:blip r:embed='rId2'/></wp:inline></w:drawing></w:r>"))
          '((p (image (@ (name "pic.png") (data "PNGDATA") (width "2cm")
                         (height "1cm") (alt "A picture")))))))

(define (test-lengths)
  (check-group "lengths")
  (check= (office-emu->length "720000") "2cm")
  (check= (office-emu->length "914400") "2.54cm")
  (check= (office-emu->length "457200") "1.27cm")
  (check= (office-emu->length "36000") "0.1cm")
  (check= (office-emu->length "0") "")
  (check= (office-emu->length #f) "")
  (check= (office-resolve "word/document.xml" "media/a.png") "word/media/a.png")
  (check= (office-resolve "word/document.xml" "../customXml/x.xml") "customXml/x.xml")
  (check= (office-resolve "word/document.xml" "/word/media/a.png") "word/media/a.png")
  (check= (office-resolve "" "word/document.xml") "word/document.xml"))

(define (test-docx-lists)
  (check-group "docx lists")
  (let ((item (lambda (id level text)
                (wp "<w:pPr><w:numPr><w:ilvl w:val='" level "'/><w:numId w:val='"
                    id "'/></w:numPr></w:pPr>" (wr text)))))
    ;; the items are paragraphs with a list and a level
    (check= (docx (string-append (item "1" "0" "a") (item "1" "0" "b")))
            '((list (@ (kind "bullet")) (item (p "a")) (item (p "b")))))
    (check= (docx (string-append (item "1" "0" "a") (item "1" "1" "n")
                                 (item "1" "1" "m") (item "1" "0" "b")))
            '((list (@ (kind "bullet"))
                    (item (p "a") (list (@ (kind "number"))
                                        (item (p "n")) (item (p "m"))))
                    (item (p "b")))))
    ;; two lists of the same abstract list are one list
    (check= (docx (string-append (item "1" "0" "a") (item "2" "0" "b")))
            '((list (@ (kind "bullet")) (item (p "a")) (item (p "b")))))
    ;; a paragraph ends a list; a heading with a number is no item
    (check= (docx (string-append (item "1" "0" "a") (wp (wr "x")) (item "1" "0" "b")))
            '((list (@ (kind "bullet")) (item (p "a")))
              (p "x")
              (list (@ (kind "bullet")) (item (p "b")))))
    (check= (docx (wp "<w:pPr><w:pStyle w:val='Titre1'/><w:numPr><w:ilvl w:val='0'/>"
                      "<w:numId w:val='1'/></w:numPr></w:pPr>" (wr "H")))
            '((p (@ (role "heading") (level "1")) "H")))))

(define (test-docx-tables)
  (check-group "docx tables")
  (let ((tc (lambda (props text)
              (string-append "<w:tc><w:tcPr>" props "</w:tcPr>" (wp (wr text)) "</w:tc>"))))
    (check= (docx (string-append
                    "<w:tbl><w:tr><w:trPr><w:tblHeader/></w:trPr>" (tc "" "A") (tc "" "B")
                    "</w:tr><w:tr>" (tc "" "1") (tc "" "2") "</w:tr></w:tbl>"))
            '((table (row (cell (@ (header "true") (borders "none")) (p "A"))
                          (cell (@ (header "true") (borders "none")) (p "B")))
                     (row (cell (@ (borders "none")) (p "1"))
                          (cell (@ (borders "none")) (p "2"))))))
    ;; a cell over two columns, a cell over two rows: the cells which they
    ;; cover are there
    (check= (docx (string-append
                    "<w:tbl><w:tr>" (tc "<w:gridSpan w:val='2'/>" "wide") "</w:tr>"
                    "<w:tr>" (tc "<w:vMerge w:val='restart'/>" "tall") (tc "" "x") "</w:tr>"
                    "<w:tr><w:tc><w:tcPr><w:vMerge/></w:tcPr><w:p/></w:tc>" (tc "" "y")
                    "</w:tr></w:tbl>"))
            '((table (row (cell (@ (colspan "2") (borders "none")) (p "wide"))
                          (cell (@ (covered "true"))))
                     (row (cell (@ (rowspan "2") (borders "none")) (p "tall"))
                          (cell (@ (borders "none")) (p "x")))
                     (row (cell (@ (covered "true")))
                          (cell (@ (borders "none")) (p "y"))))))
    ;; The borders: those of the table (around it and inside), which a cell
    ;; may change; the background of a cell; the place and the width of the
    ;; table, and the parts of its columns.
    (check= (docx (string-append
                    "<w:tbl><w:tblPr><w:jc w:val='center'/><w:tblW w:w='2500' w:type='pct'/>"
                    "<w:tblBorders><w:top w:val='single'/><w:bottom w:val='single'/>"
                    "<w:insideH w:val='single'/></w:tblBorders></w:tblPr>"
                    "<w:tblGrid><w:gridCol w:w='1000'/><w:gridCol w:w='3000'/></w:tblGrid>"
                    "<w:tr>" (tc "" "a") (tc "<w:shd w:fill='FFCC00'/>" "b") "</w:tr>"
                    "<w:tr>" (tc "<w:tcBorders><w:top w:val='nil'/><w:left w:val='single'/></w:tcBorders>" "c")
                    (tc "" "d") "</w:tr></w:tbl>"))
            '((table (@ (align "center") (width "0.5par") (columns "0.25 0.75"))
                     (row (cell (@ (borders "tb")) (p "a"))
                          (cell (@ (borders "tb") (background "#ffcc00")) (p "b")))
                     (row (cell (@ (borders "bl")) (p "c"))
                          (cell (@ (borders "tb")) (p "d"))))))
    ;; the borders of the style of the table, and of its first row
    (check= (docx (string-append
                    "<w:tbl><w:tblPr><w:tblStyle w:val='Grille'/></w:tblPr>"
                    "<w:tr>" (tc "" "a") (tc "" "b") "</w:tr>"
                    "<w:tr>" (tc "" "c") (tc "" "d") "</w:tr></w:tbl>"))
            '((table (row (cell (@ (borders "tb") (background "#dddddd")) (p "a"))
                          (cell (@ (borders "tb") (background "#dddddd")) (p "b")))
                     (row (cell (@ (borders "b")) (p "c"))
                          (cell (@ (borders "b")) (p "d"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The formulas of Word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (omml s)
  ;; the MathML of a formula of Word
  (omml->mathml
    (ox-root (parse-xml (string-append "<m:oMath" docx-ns ">" s "</m:oMath>")))))

(define (mr text) (string-append "<m:r><m:t>" text "</m:t></m:r>"))

(define (test-omml)
  (check-group "omml")
  ;; a run is cut into identifiers, numbers and operators
  (check= (omml (mr "ab+12.5"))
          '(m:math (m:mrow (m:mi "a") (m:mi "b") (m:mo "+") (m:mn "12.5"))))
  (check= (omml "<m:r><m:rPr><m:sty m:val='p'/></m:rPr><m:t>sin</m:t></m:r>")
          '(m:math (m:mi "sin")))
  (check= (omml "<m:r><m:rPr><m:nor/></m:rPr><m:t>if x</m:t></m:r>")
          '(m:math (m:mtext "if x")))
  (check= (omml (string-append "<m:f><m:num>" (mr "1") "</m:num><m:den>" (mr "x")
                               "</m:den></m:f>"))
          '(m:math (m:mfrac (m:mn "1") (m:mi "x"))))
  (check= (omml (string-append "<m:sSup><m:e>" (mr "x") "</m:e><m:sup>" (mr "2")
                               "</m:sup></m:sSup>"))
          '(m:math (m:msup (m:mi "x") (m:mn "2"))))
  (check= (omml (string-append "<m:sSubSup><m:e>" (mr "x") "</m:e><m:sub>" (mr "i")
                               "</m:sub><m:sup>" (mr "2") "</m:sup></m:sSubSup>"))
          '(m:math (m:msubsup (m:mi "x") (m:mi "i") (m:mn "2"))))
  ;; a root with or without its degree
  (check= (omml (string-append "<m:rad><m:radPr><m:degHide m:val='1'/></m:radPr><m:deg/>"
                               "<m:e>" (mr "x") "</m:e></m:rad>"))
          '(m:math (m:msqrt (m:mi "x"))))
  (check= (omml (string-append "<m:rad><m:deg>" (mr "3") "</m:deg><m:e>" (mr "x")
                               "</m:e></m:rad>"))
          '(m:math (m:mroot (m:mi "x") (m:mn "3"))))
  ;; a sum with its limits under and over
  (check= (omml (string-append "<m:nary><m:naryPr><m:chr m:val='S'/>"
                               "<m:limLoc m:val='undOvr'/></m:naryPr><m:sub>" (mr "i")
                               "</m:sub><m:sup>" (mr "n") "</m:sup><m:e>" (mr "x")
                               "</m:e></m:nary>"))
          '(m:math (m:mrow (m:munderover (m:mo "S") (m:mi "i") (m:mi "n")) (m:mi "x"))))
  ;; delimiters, the usual ones or others
  (check= (omml (string-append "<m:d><m:e>" (mr "x") "</m:e><m:e>" (mr "y") "</m:e></m:d>"))
          '(m:math (m:mrow (m:mo (@ (fence "true")) "(") (m:mi "x") (m:mo "|")
                           (m:mi "y") (m:mo (@ (fence "true")) ")"))))
  (check= (omml (string-append "<m:d><m:dPr><m:begChr m:val='['/><m:endChr m:val=''/>"
                               "</m:dPr><m:e>" (mr "x") "</m:e></m:d>"))
          '(m:math (m:mrow (m:mo (@ (fence "true")) "[") (m:mi "x"))))
  (check= (omml (string-append "<m:m><m:mr><m:e>" (mr "a") "</m:e><m:e>" (mr "b")
                               "</m:e></m:mr></m:m>"))
          '(m:math (m:mtable (m:mtr (m:mtd (m:mi "a")) (m:mtd (m:mi "b"))))))
  (check= (omml (string-append "<m:limLow><m:e>" (mr "x") "</m:e><m:lim>" (mr "y")
                               "</m:lim></m:limLow>"))
          '(m:math (m:munder (m:mi "x") (m:mi "y")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpenDocument texts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define odt-ns
  (string-append
    " xmlns:office='urn:oasis:names:tc:opendocument:xmlns:office:1.0'"
    " xmlns:style='urn:oasis:names:tc:opendocument:xmlns:style:1.0'"
    " xmlns:text='urn:oasis:names:tc:opendocument:xmlns:text:1.0'"
    " xmlns:table='urn:oasis:names:tc:opendocument:xmlns:table:1.0'"
    " xmlns:draw='urn:oasis:names:tc:opendocument:xmlns:drawing:1.0'"
    " xmlns:fo='urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0'"
    " xmlns:xlink='http://www.w3.org/1999/xlink'"
    " xmlns:svg='urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0'"))

(define odt-styles
  (string-append
    "<office:document-styles" odt-ns "><office:styles>"
    "<style:style style:name='Title' style:family='paragraph'>"
    "<style:paragraph-properties fo:text-align='center'/></style:style>"
    "<style:style style:name='Quotations' style:family='paragraph'/>"
    "<style:style style:name='Preformatted_20_Text' style:family='paragraph'/>"
    "<style:style style:name='Emphasis' style:family='text'>"
    "<style:text-properties fo:font-style='italic'/></style:style>"
    "<style:style style:name='Internet_20_link' style:family='text'>"
    "<style:text-properties style:text-underline-style='solid'/></style:style>"
    "<text:list-style style:name='L1'>"
    "<text:list-level-style-bullet text:level='1' text:bullet-char='*'/>"
    "<text:list-level-style-number text:level='2' style:num-format='1'/>"
    "</text:list-style>"
    "</office:styles></office:document-styles>"))

;; the styles which the program makes for the formatting by hand
(define odt-automatic
  (string-append
    "<office:automatic-styles>"
    "<style:style style:name='T1' style:family='text'>"
    "<style:text-properties fo:font-weight='bold'/></style:style>"
    "<style:style style:name='T2' style:family='text' style:parent-style-name='Emphasis'>"
    "<style:text-properties style:text-position='super 58%'/></style:style>"
    "<style:style style:name='T4' style:family='text'>"
    "<style:text-properties fo:color='#FF0000' fo:background-color='#ffff00'/></style:style>"
    "<style:style style:name='T3' style:family='text'>"
    "<style:text-properties style:font-name='Courier New'/></style:style>"
    "<style:style style:name='Tab' style:family='table'>"
    "<style:table-properties table:align='center' style:rel-width='50%'/></style:style>"
    "<style:style style:name='ColA' style:family='table-column'>"
    "<style:table-column-properties style:column-width='1cm'/></style:style>"
    "<style:style style:name='ColB' style:family='table-column'>"
    "<style:table-column-properties style:column-width='30mm'/></style:style>"
    "<style:style style:name='CellA' style:family='table-cell'>"
    "<style:table-cell-properties fo:border='0.5pt solid #000000'/></style:style>"
    "<style:style style:name='CellB' style:family='table-cell'>"
    "<style:table-cell-properties fo:border='none' fo:border-bottom='1pt solid #000000'"
    " fo:background-color='#FFCC00'/></style:style>"
    "<style:style style:name='P1' style:family='paragraph' style:parent-style-name='Quotations'>"
    "<style:paragraph-properties fo:text-align='end'/></style:style>"
    "</office:automatic-styles>"))

(define (odt-archive body . formula)
  ;; an OpenDocument text with this body, and with a formula if any
  (zip-pack
    '("mimetype" "content.xml" "styles.xml" "Pictures/pic.png"
      "Formula-0/content.xml")
    (list "application/vnd.oasis.opendocument.text"
          (string-append "<?xml version='1.0'?><office:document-content" odt-ns ">"
                         odt-automatic "<office:body><office:text>" body
                         "</office:text></office:body></office:document-content>")
          odt-styles "PNGDATA"
          (if (null? formula) "" (car formula)))))

(define (odt body . formula)
  (cdr (parse-odt-document (apply odt-archive (cons body formula)))))

(define (tp . l) (string-append "<text:p>" (apply string-append l) "</text:p>"))
(define (tspan style text)
  (string-append "<text:span text:style-name='" style "'>" text "</text:span>"))

(define (test-odt-text)
  (check-group "odt text")
  (check= (odt "") '())
  (check= (odt (tp "plain")) '((p "plain")))
  ;; the formatting is in the styles of the spans, and of their parents
  (check= (odt (tp "a " (tspan "Emphasis" "b") " " (tspan "T1" "c")))
          '((p "a " (em "b") " " (strong "c"))))
  (check= (odt (tp "x" (tspan "T2" "2"))) '((p "x" (em (sup "2")))))
  (check= (odt (tp (tspan "T3" "f()"))) '((p (code "f()"))))
  (check= (odt (tp (tspan "T1" (string-append "a" (tspan "Emphasis" "b")))))
          '((p (strong "a" (em "b")))))
  (check= (odt (tp (tspan "T4" "red")))
          '((p (mark (color (@ (value "#ff0000")) "red")))))
  ;; the spaces of the file are one space; those which count are elements
  (check= (odt (tp "  a\n   b  ")) '((p "a b")))
  (check= (odt (tp "a<text:s text:c='3'/>b<text:tab/>c<text:line-break/>d"))
          '((p "a   b" (tab) "c" (br) "d")))
  (check= (odt (string-append (tp "a") (tp) (tp "b"))) '((p "a") (p "b")))
  ;; links, bookmarks, references and notes
  (check= (odt (tp "<text:a xlink:href='https://www.texmacs.org'>"
                   (tspan "Internet_20_link" "site") "</text:a>"))
          '((p (link (@ (href "https://www.texmacs.org")) "site"))))
  (check= (odt (tp "<text:bookmark-start text:name='b'/>x<text:bookmark-end text:name='b'/>"))
          '((p (bookmark (@ (name "b"))) "x")))
  (check= (odt (tp "see <text:bookmark-ref text:ref-name='b'>there</text:bookmark-ref>"))
          '((p "see " (ref (@ (name "b")) "there"))))
  (check= (odt (tp "a<text:note text:note-class='footnote'><text:note-citation>1"
                   "</text:note-citation><text:note-body>" (tp "The note.")
                   "</text:note-body></text:note>"))
          '((p "a" (note (p "The note."))))))

(define (test-odt-blocks)
  (check-group "odt blocks")
  (check= (odt "<text:h text:outline-level='2'>Sub</text:h>")
          '((p (@ (role "heading") (level "2")) "Sub")))
  (check= (odt "<text:p text:style-name='Title'>T</text:p>")
          '((p (@ (role "title") (align "center")) "T")))
  ;; an automatic style is what its parent is
  (check= (odt "<text:p text:style-name='P1'>q</text:p>")
          '((p (@ (role "quote") (align "right")) "q")))
  (check= (odt "<text:p text:style-name='Preformatted_20_Text'>x=1</text:p>")
          '((p (@ (role "code")) "x=1")))
  ;; the spaces at the start of a line of code count
  (check= (odt "<text:p text:style-name='Preformatted_20_Text'><text:s text:c='4'/>return x </text:p>")
          '((p (@ (role "code")) "    return x")))
  ;; text which is in no paragraph is one
  (check= (odt (string-append "loose " (tspan "Emphasis" "text") (tp "next")))
          '((p "loose " (em "text")) (p "next")))
  (check= (odt (string-append "<text:table-of-content><text:index-body>" (tp "Intro 1")
                              "</text:index-body></text:table-of-content>" (tp "x")))
          '((toc) (p "x")))
  ;; lists: the kind of each level is in the style of the outer list
  (check= (odt (string-append
                 "<text:list text:style-name='L1'><text:list-item>" (tp "a")
                 "<text:list><text:list-item>" (tp "n") "</text:list-item></text:list>"
                 "</text:list-item><text:list-item>" (tp "b") "</text:list-item></text:list>"))
          '((list (@ (kind "bullet"))
                  (item (p "a") (list (@ (kind "number")) (item (p "n"))))
                  (item (p "b")))))
  ;; a list which goes on deeper is the end of the item before
  (check= (odt (string-append
                 "<text:list text:style-name='L1'><text:list-item>" (tp "a")
                 "</text:list-item></text:list>"
                 "<text:list text:style-name='L1'><text:list-item><text:list>"
                 "<text:list-item>" (tp "n") "</text:list-item></text:list>"
                 "</text:list-item></text:list>"))
          '((list (@ (kind "bullet"))
                  (item (p "a") (list (@ (kind "number")) (item (p "n")))))))
  ;; and so one level deeper
  (check= (odt (string-append
                 "<text:list text:style-name='L1'><text:list-item>" (tp "a")
                 "<text:list><text:list-item>" (tp "n") "</text:list-item></text:list>"
                 "</text:list-item></text:list>"
                 "<text:list text:style-name='L1'><text:list-item><text:list>"
                 "<text:list-item><text:list><text:list-item>" (tp "d")
                 "</text:list-item></text:list></text:list-item></text:list>"
                 "</text:list-item></text:list>"))
          '((list (@ (kind "bullet"))
                  (item (p "a")
                        (list (@ (kind "number"))
                              (item (p "n") (list (@ (kind "bullet")) (item (p "d")))))))))
  ;; tables, with the cells which a wider one covers
  (check= (odt (string-append
                 "<table:table><table:table-header-rows><table:table-row>"
                 "<table:table-cell table:number-columns-spanned='2'>" (tp "H")
                 "</table:table-cell><table:covered-table-cell/>"
                 "</table:table-row></table:table-header-rows><table:table-row>"
                 "<table:table-cell>" (tp "1") "</table:table-cell>"
                 "<table:table-cell>" (tp "2") "</table:table-cell>"
                 "</table:table-row></table:table>"))
          '((table (row (cell (@ (header "true") (borders "none") (colspan "2")) (p "H"))
                        (cell (@ (covered "true"))))
                   (row (cell (@ (borders "none")) (p "1"))
                        (cell (@ (borders "none")) (p "2"))))))
  ;; the borders and the background of a cell are in its style; the place
  ;; of the table and the widths of its columns in theirs
  (check= (odt (string-append
                 "<table:table table:style-name='Tab'>"
                 "<table:table-column table:style-name='ColA'/>"
                 "<table:table-column table:style-name='ColB'/><table:table-row>"
                 "<table:table-cell table:style-name='CellA'>" (tp "1") "</table:table-cell>"
                 "<table:table-cell table:style-name='CellB'>" (tp "2") "</table:table-cell>"
                 "</table:table-row></table:table>"))
          '((table (@ (align "center") (width "0.5par") (columns "0.25 0.75"))
                   (row (cell (@ (borders "tblr")) (p "1"))
                        (cell (@ (borders "b") (background "#ffcc00")) (p "2"))))))
  ;; an image and a formula: files of the archive
  (check= (odt (tp "<draw:frame svg:width='2cm' svg:height='1cm'>"
                   "<draw:image xlink:href='Pictures/pic.png'/>"
                   "<svg:title>A picture</svg:title></draw:frame>"))
          '((p (image (@ (name "pic.png") (data "PNGDATA") (width "2cm")
                         (height "1cm") (alt "A picture"))))))
  (check= (odt (tp "<draw:frame text:anchor-type='as-char'>"
                   "<draw:object xlink:href='./Formula-0'/></draw:frame>")
               "<math><mi>x</mi></math>")
          '((p (math "<math><mi>x</mi></math>")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Office trees as TeXmacs trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm . blocks)
  ;; the paragraphs of the TeXmacs document for these blocks
  (with d (office->texmacs (cons 'office blocks))
    (cdr (cadr (cadr d)))))

(define (test-officetm-text)
  (check-group "officetm text")
  (check= (office->texmacs '(office))
          '(document (body (document "")) (style "generic")))
  (check= (tm '(p "plain")) '("plain"))
  (check= (tm '(p "a " (em "b") (strong "c") (underline "d") (strike "e")
                  (sub "f") (sup "g") (code "h")))
          '((concat "a " (em "b") (strong "c") (underline "d") (strike-through "e")
                    (rsub "f") (rsup "g") (verbatim "h"))))
  (check= (tm '(p (strong (em "x")))) '((strong (em "x"))))
  (check= (tm '(p (color (@ (value "#ff0000")) "r") (mark "m")))
          '((concat (with "color" "#ff0000" "r") (marked "m"))))
  (check= (tm '(p (link (@ (href "u")) "t") (br) (ref (@ (name "b")) "2")))
          '((concat (hlink "t" "u") (next-line) (hlink "2" "#b"))))
  (check= (tm '(p (bookmark (@ (name "b"))) "x")) '((concat (label "b") "x")))
  (check= (tm '(p "a" (note (p "n")))) '((concat "a" (footnote "n"))))
  (check= (tm '(p "a" (note (p "n") (p "m"))))
          '((concat "a" (footnote (document "n" "m")))))
  ;; the text is UTF-8, the tree is in the encoding of TeXmacs
  (check= (tm (list 'p (string-append "caf" (bytes 195 169) " <x>")))
          (list (string-append "caf" (bytes 233) " <less>x<gtr>")))
  ;; the characters of no width are dropped
  (check= (tm (list 'p (string-append "a" (bytes 226 128 139) "b"))) '("ab"))
  ;; an image is inside the document, with the name of its file
  (check= (tm '(p (image (@ (name "pic.png") (data "PNG") (width "2cm")))))
          '((image (tuple (raw-data "PNG") "pic.png") "2cm" "" "" ""))))

(define (test-officetm-math)
  (check-group "officetm math")
  (check= (tm '(p "a " (math "<math><msup><mi>x</mi><mn>2</mn></msup></math>")))
          '((concat "a " (math (concat "x" (rsup "2"))))))
  ;; with a prefix, with other writings of the formula beside it
  (check= (tm '(p "a " (math "<mml:math xmlns:mml='u'><mml:semantics><mml:mfrac><mml:mn>1</mml:mn><mml:mi>x</mml:mi></mml:mfrac><mml:annotation>1 over x</mml:annotation></mml:semantics></mml:math>")))
          '((concat "a " (math (frac "1" "x")))))
  (check= (tm '(p "a " (math (@ (form "sxml")) (m:math (m:msqrt (m:mi "x"))))))
          '((concat "a " (math (sqrt "x")))))
  ;; displayed when the node says so, or when the formula is alone
  (check= (tm '(p (math (@ (display "true")) "<math><mi>x</mi></math>") "."))
          '((concat (equation* (document "x")) ".")))
  (check= (tm '(p (math "<math><mi>x</mi></math>")))
          '((equation* (document "x")))))

(define (test-officetm-blocks)
  (check-group "officetm blocks")
  (check= (tm '(p (@ (role "heading") (level "1")) "A")
              '(p (@ (role "heading") (level "2")) "B")
              '(p (@ (role "heading") (level "9")) "C"))
          '((section "A") (subsection "B") (subparagraph "C")))
  (check= (tm '(p (@ (align "center")) "c")) '((with "par-mode" "center" "c")))
  ;; the paragraphs of a quotation, the lines of code are taken together
  (check= (tm '(p (@ (role "quote")) "a") '(p (@ (role "quote")) "b") '(p "c"))
          '((quotation (document "a" "b")) "c"))
  (check= (tm '(p (@ (role "code")) "x=1") '(p (@ (role "code")) "y" (br) "z"))
          '((verbatim-code (document "x=1" "y" "z"))))
  (check= (tm '(list (@ (kind "bullet")) (item (p "a")) (item (p "b") (p "c"))))
          '((itemize (document (concat (item) "a") (concat (item) "b") "c"))))
  (check= (tm '(list (@ (kind "number"))
                     (item (p "a") (list (@ (kind "bullet")) (item (p "n"))))))
          '((enumerate (document (concat (item) "a")
                                 (itemize (document (concat (item) "n")))))))
  (check= (tm '(pagebreak)) '((page-break)))
  ;; the entries of a table of contents are the table of TeXmacs
  (check= (tm '(p (@ (role "toc")) "Intro 1") '(p (@ (role "toc")) "More 2") '(p "x"))
          '((table-of-contents "toc" (document "")) "x"))
  (check= (tm '(toc)) '((table-of-contents "toc" (document ""))))
  ;; terms and their definitions
  (check= (tm '(p (@ (role "term")) "T") '(p (@ (role "definition")) "D")
              '(p (@ (role "definition")) "E") '(p (@ (role "term")) "U") '(p "x"))
          '((description (document (concat (item* "T") "D") "E" (item* "U"))) "x"))
  ;; a figure or a table with its caption, before or after
  (check= (tm '(p (@ (role "figure")) (image (@ (name "f.png"))))
              '(p (@ (role "caption")) "The figure"))
          '((big-figure (image "f.png" "" "" "" "") "The figure")))
  (check= (tm '(p (image (@ (name "f.png")))) '(p (@ (role "caption")) "The figure"))
          '((big-figure (image "f.png" "" "" "" "") "The figure")))
  (check= (tm '(p (@ (role "caption")) "The table") '(table (row (cell (p "x")))))
          '((big-table (block (tformat (table (row (cell "x"))))) "The table"))))

(define (test-officetm-tables)
  (check-group "officetm tables")
  (check= (tm '(table (row (cell (@ (header "true")) (p "A"))
                           (cell (@ (header "true")) (p (@ (align "right")) "B")))
                      (row (cell (p "1")) (cell (p "2")))))
          '((block (tformat (cwith "1" "1" "2" "2" "cell-halign" "r")
                            (table (row (cell (strong "A")) (cell (strong "B")))
                                   (row (cell "1") (cell "2")))))))
  (check= (tm '(table (row (cell (@ (colspan "2")) (p "w")) (cell (@ (covered "true"))))
                      (row (cell (@ (rowspan "2")) (p "t")) (cell (p "x")))
                      (row (cell (@ (covered "true"))) (cell (p "y")))))
          '((block (tformat (cwith "1" "1" "1" "1" "cell-col-span" "2")
                            (cwith "2" "2" "1" "1" "cell-row-span" "2")
                            (table (row (cell "w") (cell ""))
                                   (row (cell "t") (cell "x"))
                                   (row (cell "") (cell "y")))))))
  ;; The borders of the cells: all of them are a block; else the lines are
  ;; formats of rectangles of cells, and so are the backgrounds.
  (check= (tm '(table (row (cell (@ (borders "tblr")) (p "a"))
                           (cell (@ (borders "tblr")) (p "b")))))
          '((block (tformat (table (row (cell "a") (cell "b")))))))
  (check= (tm '(table (row (cell (@ (borders "none")) (p "a")))))
          '((tabular (tformat (table (row (cell "a")))))))
  (check= (tm '(table (row (cell (@ (borders "tb") (background "#dddddd")) (p "a"))
                           (cell (@ (borders "tb") (background "#dddddd")) (p "b")))
                      (row (cell (@ (borders "b")) (p "c"))
                           (cell (@ (borders "b")) (p "d")))))
          '((tabular (tformat (cwith "1" "1" "1" "2" "cell-tborder" "1ln")
                              (cwith "1" "2" "1" "2" "cell-bborder" "1ln")
                              (cwith "1" "1" "1" "2" "cell-background" "#dddddd")
                              (table (row (cell "a") (cell "b"))
                                     (row (cell "c") (cell "d")))))))
  ;; a table which is centered, of half the width of the text, with
  ;; columns of a quarter and three quarters of it
  (check= (tm '(table (@ (align "center") (width "0.5par") (columns "0.25 0.75"))
                      (row (cell (@ (borders "none")) (p "a"))
                           (cell (@ (borders "none")) (p "b")))))
          '((with "par-mode" "center"
              (tabular (tformat (twith "table-width" "0.5par")
                                (twith "table-hmode" "exact")
                                (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                                (cwith "1" "-1" "1" "1" "cell-hmode" "exact")
                                (cwith "1" "-1" "1" "1" "cell-width" "0.125par")
                                (cwith "1" "-1" "2" "2" "cell-hmode" "exact")
                                (cwith "1" "-1" "2" "2" "cell-width" "0.375par")
                                (table (row (cell "a") (cell "b"))))))))
  ;; a table inside a cell has no width of its own
  (check= (tm '(table (row (cell (@ (borders "none"))
                                 (table (@ (width "1par"))
                                        (row (cell (@ (borders "none")) (p "in"))))))))
          '((tabular (tformat (table (row (cell (tabular (tformat (table (row (cell "in"))))))))))))
  ;; a cell of several paragraphs: the table has the width of the page
  (check= (tm '(table (row (cell (p "a") (p "b")))))
          '((block (tformat (twith "table-width" "1par")
                            (twith "table-hmode" "exact")
                            (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                            (table (row (cell (document "a" "b")))))))))

(define (test-officetm-title)
  (check-group "officetm title")
  (check= (tm '(p (@ (role "title")) "T") '(p (@ (role "subtitle")) "S")
              '(p (@ (role "author")) "A") '(p (@ (role "author")) "B")
              '(p (@ (role "date")) "2026") '(p (@ (role "abstract")) "Short.")
              '(p "x"))
          '((doc-data (doc-title "T") (doc-subtitle "S")
                      (doc-author (author-data (author-name "A")))
                      (doc-author (author-data (author-name "B")))
                      (doc-date "2026"))
            (abstract-data (abstract (document "Short.")))
            "x"))
  ;; a heading "Abstract" before the abstract is left out
  (check= (tm '(p (@ (role "title")) "T") '(p (@ (role "skip")) "Abstract")
              '(p (@ (role "abstract")) "Short.") '(p "x"))
          '((doc-data (doc-title "T"))
            (abstract-data (abstract (document "Short.")))
            "x"))
  ;; the title of the properties of the file, when the text has none
  (check= (tm '(meta (title "From the file") (author "Someone")) '(p "x"))
          '((doc-data (doc-title "From the file")) "x"))
  (check= (tm '(meta (title "From the file")) '(p (@ (role "title")) "T"))
          '((doc-data (doc-title "T")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs trees as office trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (of t)
  ;; the blocks of the office tree of a TeXmacs tree
  (cdr (texmacs->office t '())))

(define (test-tmoffice-text)
  (check-group "tmoffice text")
  (check= (of "plain") '((p "plain")))
  (check= (of '(concat "a " (em "b") (strong "c") (underline "u") (strike-through "s")
                       (rsub "1") (rsup "2") (verbatim "v") (marked "m")))
          '((p "a " (em "b") (strong "c") (underline "u") (strike "s") (sub "1")
               (sup "2") (code "v") (mark "m"))))
  ;; the colors by their names or their values; small capitals
  (check= (of '(concat (with "color" "red" "r") (with "color" "#00f" "b")
                       (with "color" "black" "k")
                       (with "font-shape" "small-caps" "sc")))
          '((p (color (@ (value "#ff0000")) "r") (color (@ (value "#0000ff")) "b")
               "k" (smallcaps "sc"))))
  (check= (of '(concat "a" (footnote "note") (hlink "t" "u") (next-line) "b"))
          '((p "a" (note (p "note")) (link (@ (href "u")) "t") (br) "b")))
  ;; formulas are MathML, with characters and not their names
  (check= (of '(concat "x " (math (concat "a" (rsup "2") "+<alpha>"))))
          `((p "x " (math (@ (form "sxml"))
                          (m:math (m:mrow (m:msup (m:mi "a") (m:mn "2")) (m:mo "+")
                                          (m:mi ,(office-utf8 #x3b1))))))))
  ;; on its own lines, a sum has its limits under and over it
  (check= (of '(equation* (document (concat (big "sum") (rsub "i") (rsup "n") "x"))))
          `((p (math (@ (display "true") (form "sxml"))
                     (m:math (m:mrow (m:munderover (m:mo ,(office-utf8 #x2211))
                                                   (m:mi "i") (m:mi "n"))
                                     (m:mi "x"))))))))

(define (test-tmoffice-blocks)
  (check-group "tmoffice blocks")
  ;; the first level of headings which is used is the level 1
  (check= (of '(document (subsection "S") "text" (subsubsection "T")))
          '((p (@ (role "heading") (level "1")) "S") (p "text")
            (p (@ (role "heading") (level "2")) "T")))
  (check= (of '(itemize (document (concat (item) "a") (concat (item) "b")
                                  (enumerate (document (concat (item) "n"))))))
          '((list (@ (kind "bullet")) (item (p "a"))
                  (item (p "b") (list (@ (kind "number")) (item (p "n")))))))
  (check= (of '(description (document (concat (item* "T") "D") "E")))
          '((p (@ (role "term")) "T") (p (@ (role "definition")) "D")
            (p (@ (role "definition")) "E")))
  (check= (of '(quotation (document "q" "r")))
          '((p (@ (role "quote")) "q") (p (@ (role "quote")) "r")))
  (check= (of '(verbatim-code (document "x" "  y")))
          '((p (@ (role "code")) "x" (br) "  y")))
  (check= (of '(document "a" (page-break) (hrule)))
          '((p "a") (pagebreak) (rule)))
  (check= (of '(theorem (document "T"))) '((p (strong "Theorem.") " T")))
  ;; a figure or a table, and its caption
  (check= (of '(big-table (tabular (tformat (table (row (cell "a"))))) "cap"))
          '((table (@ (align "center")) (row (cell (@ (borders "none")) (p "a"))))
            (p (@ (role "caption")) (strong "Table.") " cap")))
  ;; the title of a document
  (check= (of '(document (TeXmacs "2.1") (style "generic")
                 (body (document
                         (doc-data (doc-title "T")
                                   (doc-author (author-data (author-name "A")))
                                   (doc-date "2026"))
                         (abstract-data (abstract (document "Abs.")))
                         (section "S") "x"))))
          '((p (@ (role "title")) "T") (p (@ (role "author")) "A")
            (p (@ (role "date")) "2026") (p (@ (role "abstract")) "Abs.")
            (p (@ (role "heading") (level "1")) "S") (p "x"))))

;; A drawing is a picture which the editor makes: a PNG, with an SVG beside
;; it where it can be made (the builds with MuPDF).
(define (test-tmoffice-drawings)
  (check-group "tmoffice drawings")
  (let* ((drawing '(with "gr-geometry" (tuple "geometry" "4cm" "2cm" "center")
                     (graphics "" (line (point "-1" "0") (point "1" "0")))))
         (r (of `(document "a" ,drawing "b")))
         (image (and (== (length r) 3) (func? (cadr r) 'p)
                     (list-find (ox-children (cadr r)) (lambda (x) (func? x 'image))))))
    (check= (car r) '(p "a"))
    (check= (cAr r) '(p "b"))
    (check-true (pair? image))
    (when image
      ;; a PNG of the size of the drawing
      (check-true (string-starts? (ox-attr image 'data)
                                  (string-append (bytes 137) "PNG")))
      (check-true (< (abs (- (office-length->cm (ox-attr image 'width)) 4.0)) 0.1))
      (check-true (< (abs (- (office-length->cm (ox-attr image 'height)) 2.0)) 0.1))
      (when (ox-attr image 'svg)
        (check-true (string-starts? (ox-attr image 'svg) "<svg"))))))

;; The pages of the documentation: the logo before the title and a rule
;; under it, a rule above the copyright.
(define (test-tmoffice-tmdoc)
  (check-group "tmoffice tmdoc")
  (let* ((r (of '(document (tmdoc-title "Creating tables") "text"
                           (tmdoc-copyright "1998" "A" "B"))))
         (title (car r)))
    (check= (ox-attr title 'role) "title")
    (check-true (func? (car (ox-children title)) 'image))
    (check= (cdr (ox-children title)) '(" Creating tables"))
    (check= (cadr r) '(rule))
    (check= (caddr r) '(p "text"))
    (check= (cdddr r)
            (list '(rule)
                  (list 'p (string-append (office-utf8 #xa9) " 1998 A, B")))))
  ;; the operators which are not seen are not written in a formula of Word
  (check= (mathml->omml `(m:math (m:mrow (m:mi "a") (m:mo ,(office-utf8 #x2062))
                                         (m:mi "b"))))
          '(m:oMath (m:r (m:t "a")) (m:r (m:t "b")))))

(define (test-tmoffice-tables)
  (check-group "tmoffice tables")
  ;; a block has all its borders; the formats of rectangles of cells
  (check= (of '(block (tformat (cwith "1" "1" "1" "-1" "cell-background" "pastel blue")
                               (cwith "1" "-1" "2" "2" "cell-halign" "r")
                               (table (row (cell "a") (cell "b"))
                                      (row (cell "1") (cell "2"))))))
          '((table (row (cell (@ (borders "tblr") (background "#dfdfff")) (p "a"))
                        (cell (@ (borders "tblr") (background "#dfdfff")
                                 (align "right")) (p "b")))
                   (row (cell (@ (borders "tblr")) (p "1"))
                        (cell (@ (borders "tblr") (align "right")) (p "2"))))))
  ;; a line under the first row, a cell over two columns
  (check= (of '(tabular (tformat (cwith "1" "1" "1" "-1" "cell-bborder" "1ln")
                                 (cwith "1" "1" "1" "1" "cell-col-span" "2")
                                 (table (row (cell "wide") (cell ""))
                                        (row (cell "1") (cell "2"))))))
          '((table (row (cell (@ (borders "b") (colspan "2")) (p "wide"))
                        (cell (@ (covered "true"))))
                   (row (cell (@ (borders "none")) (p "1"))
                        (cell (@ (borders "none")) (p "2")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MathML as formulas of Word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-mathml-omml)
  (check-group "mathml to omml")
  (check= (mathml->omml '(m:math (m:mrow (m:mi "a") (m:mo "+") (m:mn "12"))))
          '(m:oMath (m:r (m:t "a")) (m:r (m:t "+")) (m:r (m:t "12"))))
  (check= (mathml->omml '(m:math (m:mi "sin")))
          '(m:oMath (m:r (m:rPr (m:sty (@ (m:val "p")))) (m:t "sin"))))
  (check= (mathml->omml '(m:math (m:mfrac (m:mn "1") (m:mi "x"))))
          '(m:oMath (m:f (m:num (m:r (m:t "1"))) (m:den (m:r (m:t "x"))))))
  (check= (mathml->omml '(m:math (m:msup (m:mi "x") (m:mn "2"))))
          '(m:oMath (m:sSup (m:e (m:r (m:t "x"))) (m:sup (m:r (m:t "2"))))))
  (check= (mathml->omml '(m:math (m:msqrt (m:mi "x"))))
          '(m:oMath (m:rad (m:radPr (m:degHide (@ (m:val "1")))) (m:deg)
                           (m:e (m:r (m:t "x"))))))
  ;; brackets take what they enclose
  (check= (mathml->omml '(m:math (m:mrow (m:mo (@ (form "prefix")) "(") (m:mi "x")
                                         (m:mo (@ (form "postfix")) ")"))))
          '(m:oMath (m:d (m:dPr (m:begChr (@ (m:val "("))) (m:endChr (@ (m:val ")"))))
                         (m:e (m:r (m:t "x"))))))
  ;; a big operator takes what follows it, up to a relation
  (check= (mathml->omml `(m:math (m:mrow (m:munderover (m:mo ,(office-utf8 #x2211))
                                                       (m:mi "i") (m:mi "n"))
                                         (m:mi "x") (m:mo "=") (m:mi "y"))))
          `(m:oMath (m:nary (m:naryPr (m:chr (@ (m:val ,(office-utf8 #x2211))))
                                      (m:limLoc (@ (m:val "undOvr"))))
                            (m:sub (m:r (m:t "i"))) (m:sup (m:r (m:t "n")))
                            (m:e (m:r (m:t "x"))))
                    (m:r (m:t "=")) (m:r (m:t "y"))))
  ;; there and back
  (for-each
    (lambda (m) (check= (omml->mathml (mathml->omml m)) m))
    '((m:math (m:mfrac (m:mn "1") (m:mi "x")))
      (m:math (m:msubsup (m:mi "x") (m:mi "i") (m:mn "2")))
      (m:math (m:mroot (m:mi "x") (m:mn "3")))
      (m:math (m:mtable (m:mtr (m:mtd (m:mi "a")) (m:mtd (m:mi "b"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The writers and the round trips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (archive-names z)
  (let loop ((l (zip-unpack z)) (acc '()))
    (if (or (null? l) (null? (cdr l))) (reverse acc)
        (loop (cddr l) (cons (car l) acc)))))

(define (test-writers)
  (check-group "writers")
  (let* ((tree '(office (p "a " (em "b") (note (p "n")))
                        (p (image (@ (name "pic.png") (data "PNGDATA")
                                     (width "2cm") (height "1cm"))))
                        (p (math (@ (form "sxml")) (m:math (m:mi "x"))))))
         (docx (serialize-docx-document tree))
         (odt (serialize-odt-document tree)))
    (check-true (zip-archive? docx))
    (check= (archive-names docx)
            '("[Content_Types].xml" "_rels/.rels" "word/document.xml"
              "word/_rels/document.xml.rels" "word/styles.xml"
              "word/numbering.xml" "word/footnotes.xml" "word/settings.xml"
              "word/media/image1.png"))
    ;; the type of an OpenDocument text is the first file of its archive
    (check= (archive-names odt)
            '("mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml"
              "Pictures/image2.png" "Formula-3/content.xml"))
    (check= (cadr (zip-unpack odt)) "application/vnd.oasis.opendocument.text")
    ;; what is written is read back the same
    (check= (cdr (parse-docx-document docx))
            '((p "a " (em "b") (note (p "n")))
              (p (image (@ (name "image1.png") (data "PNGDATA") (width "2cm")
                           (height "1cm"))))
              (p (math (@ (form "sxml")) (m:math (m:mi "x"))))))
    (check= (list-head (cdr (parse-odt-document odt)) 2)
            '((p "a " (em "b") (note (p "n")))
              (p (image (@ (name "image2.png") (data "PNGDATA") (width "2cm")
                           (height "1cm")))))))
  ;; a picture with an SVG beside its bitmap: both are written, and the
  ;; bitmap is read back
  (let* ((tree '(office (p (image (@ (name "d.png") (data "PNGDATA") (width "2cm")
                                     (height "1cm") (svg "<svg/>"))))))
         (docx (serialize-docx-document tree))
         (odt (serialize-odt-document tree)))
    (check-true (in? "word/media/image1.svg" (archive-names docx)))
    (check-true (in? "Pictures/image1.svg" (archive-names odt)))
    (check= (cdr (parse-docx-document docx))
            '((p (image (@ (name "image1.png") (data "PNGDATA") (width "2cm")
                           (height "1cm"))))))
    (check= (cdr (parse-odt-document odt))
            '((p (image (@ (name "image1.png") (data "PNGDATA") (width "2cm")
                           (height "1cm")))))))
  ;; the text of XML: its characters are escaped, the control ones removed
  (check= (ox-serialize-element '(a (@ (x "1 & \"2\"")) "t < u" (b)))
          "<a x=\"1 &amp; &quot;2&quot;\">t &lt; u<b/></a>")
  ;; an empty document is a document
  (check= (convert (serialize-docx-document '(office)) "docx-document" "texmacs-stree")
          '(document (body (document "")) (style "generic")))
  (check= (convert (serialize-odt-document '(office)) "odt-document" "texmacs-stree")
          '(document (body (document "")) (style "generic"))))

;; These trees come back as they are from both formats.
(define office-same
  '("plain"
    (concat "a " (em "b") " " (strong "c") " " (underline "u") " "
            (strike-through "s") " H" (rsub "2") "O x" (rsup "2") " "
            (verbatim "v") " " (with "color" "#ff0000" "r") " " (marked "m")
            " " (with "font-shape" "small-caps" "sc"))
    (document (section "S") "text" (subsection "T") "more" (subsubsection "U") "x")
    (itemize (document (concat (item) "a") (concat (item) "b")
                       (enumerate (document (concat (item) "n") (concat (item) "m")))
                       (concat (item) "c")))
    (enumerate (document (concat (item) "a") "second" (concat (item) "b")))
    (concat "a" (footnote "note") " " (hlink "t" "https://x.y/") (next-line) "b")
    (concat "x " (math (concat "a" (rsup "2") "+" (frac "1" "2"))) " y")
    (document "a" (equation* (document (concat (sqrt "x") "=" (frac "1" "y")))) "b")
    (quotation (document "q" "r"))
    (verbatim-code (document "def f(x):" "    return x"))
    (block (tformat (table (row (cell "a") (cell "b")) (row (cell "1") (cell "2")))))
    (tabular (tformat (cwith "1" "1" "1" "2" "cell-bborder" "1ln")
                      (table (row (cell "a") (cell "b")) (row (cell "1") (cell "2")))))
    (document (description (document (concat (item* "T") "D"))) "x")
    (document "a" (page-break) "b")))

(define (test-round-trips)
  (check-group "round trips")
  (for-each
    (lambda (fm)
      (for-each
        (lambda (t)
          (check= (cdr (cadr (cadr (convert (convert t "texmacs-stree" fm)
                                            fm "texmacs-stree"))))
                  (if (func? t 'document) (cdr t) (list t))))
        office-same))
    '("docx-document" "odt-document")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-formats)
  (check-group "formats")
  (check-true (format? "docx"))
  (check-true (format? "odt"))
  (check= (format-from-suffix "docx") "docx")
  (check= (format-from-suffix "odt") "odt")
  (check-true (in? "docx" (converters-to-special "texmacs-file" "-file" #f)))
  (check-true (in? "odt" (converters-to-special "texmacs-file" "-file" #f)))
  (check-true (in? "docx" (converters-from-special "texmacs-file" "-file" #f)))
  (check-true (in? "odt" (converters-from-special "texmacs-file" "-file" #f)))
  ;; the whole way, from the archive to the document
  (check= (convert (docx-archive (string-append (wp (wstyle "Titre1") (wr "Intro"))
                                                (wp (wr "a ") (wr "b" "<w:i/>"))))
                   "docx-document" "texmacs-stree")
          '(document (body (document (section "Intro") (concat "a " (em "b"))))
                     (style "generic")))
  (check= (convert (odt-archive (string-append "<text:h text:outline-level='1'>Intro</text:h>"
                                               (tp "a " (tspan "Emphasis" "b"))))
                   "odt-document" "texmacs-stree")
          '(document (body (document (section "Intro") (concat "a " (em "b"))))
                     (style "generic")))
  ;; a file which is no archive is an empty document
  (check= (convert "not an archive" "docx-document" "texmacs-stree")
          '(document (body (document "")) (style "generic"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (office-test-failures)
  (:synopsis "Run the tests of the office converters, return the failures")
  (check-suite "office")
  (test-zip)
  (test-lengths)
  (test-docx-text)
  (test-docx-links)
  (test-docx-blocks)
  (test-docx-lists)
  (test-docx-tables)
  (test-omml)
  (test-odt-text)
  (test-odt-blocks)
  (test-officetm-text)
  (test-officetm-math)
  (test-officetm-blocks)
  (test-officetm-tables)
  (test-officetm-title)
  (test-tmoffice-text)
  (test-tmoffice-blocks)
  (test-tmoffice-tables)
  (test-tmoffice-drawings)
  (test-tmoffice-tmdoc)
  (test-mathml-omml)
  (test-writers)
  (test-round-trips)
  (test-formats)
  (check-end))
