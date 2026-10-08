
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
;;   - the formats docx and odt.

(texmacs-module (check office-test)
  (:use (check check-lib)
        (convert office office-tools)
        (convert office docxin)
        (convert office odtin)
        (convert office omml)
        (convert office officetm)))

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
    "<w:style w:type='paragraph' w:styleId='Legende'><w:name w:val='caption'/></w:style>"
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
            '((table (row (cell (@ (header "true")) (p "A"))
                          (cell (@ (header "true")) (p "B")))
                     (row (cell (p "1")) (cell (p "2"))))))
    ;; a cell over two columns, a cell over two rows: the cells which they
    ;; cover are there
    (check= (docx (string-append
                    "<w:tbl><w:tr>" (tc "<w:gridSpan w:val='2'/>" "wide") "</w:tr>"
                    "<w:tr>" (tc "<w:vMerge w:val='restart'/>" "tall") (tc "" "x") "</w:tr>"
                    "<w:tr><w:tc><w:tcPr><w:vMerge/></w:tcPr><w:p/></w:tc>" (tc "" "y")
                    "</w:tr></w:tbl>"))
            '((table (row (cell (@ (colspan "2")) (p "wide"))
                          (cell (@ (covered "true"))))
                     (row (cell (@ (rowspan "2")) (p "tall")) (cell (p "x")))
                     (row (cell (@ (covered "true"))) (cell (p "y"))))))))

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
    "<style:style style:name='T3' style:family='text'>"
    "<style:text-properties style:font-name='Courier New'/></style:style>"
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
  ;; tables, with the cells which a wider one covers
  (check= (odt (string-append
                 "<table:table><table:table-header-rows><table:table-row>"
                 "<table:table-cell table:number-columns-spanned='2'>" (tp "H")
                 "</table:table-cell><table:covered-table-cell/>"
                 "</table:table-row></table:table-header-rows><table:table-row>"
                 "<table:table-cell>" (tp "1") "</table:table-cell>"
                 "<table:table-cell>" (tp "2") "</table:table-cell>"
                 "</table:table-row></table:table>"))
          '((table (row (cell (@ (header "true") (colspan "2")) (p "H"))
                        (cell (@ (covered "true"))))
                   (row (cell (p "1")) (cell (p "2"))))))
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
  (check= (tm '(p (link (@ (href "u")) "t") (br) (ref (@ (name "b")) "2")))
          '((concat (hlink "t" "u") (next-line) (hlink "2" "#b"))))
  (check= (tm '(p (bookmark (@ (name "b"))) "x")) '((concat (label "b") "x")))
  (check= (tm '(p "a" (note (p "n")))) '((concat "a" (footnote "n"))))
  (check= (tm '(p "a" (note (p "n") (p "m"))))
          '((concat "a" (footnote (document "n" "m")))))
  ;; the text is UTF-8, the tree is in the encoding of TeXmacs
  (check= (tm (list 'p (string-append "caf" (bytes 195 169) " <x>")))
          (list (string-append "caf" (bytes 233) " <less>x<gtr>")))
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
  ;; a figure or a table with its caption, before or after
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
  ;; the title of the properties of the file, when the text has none
  (check= (tm '(meta (title "From the file") (author "Someone")) '(p "x"))
          '((doc-data (doc-title "From the file")) "x"))
  (check= (tm '(meta (title "From the file")) '(p (@ (role "title")) "T"))
          '((doc-data (doc-title "T")))))

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
  (test-formats)
  (check-end))
