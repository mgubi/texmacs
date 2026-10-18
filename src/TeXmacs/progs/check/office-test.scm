
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
  (check= (zip-unpack (substring zip-sample 0 100)) '())
  ;; two entries with the same name are two entries
  (check= (zip-unpack (zip-pack '("x" "x") '("1" "2"))) '("x" "1" "x" "2"))
  ;; many entries: the directory is read once
  (let* ((names (map (lambda (i) (string-append "f" (number->string i)))
                     (iota 5000)))
         (z (zip-pack names names)))
    (check= (length (zip-unpack z)) 10000))
  ;; more entries than the format has room for
  (check= (zip-pack (make-list 65536 "a") (make-list 65536 "")) "")
  ;; a name longer than the format has room for
  (check= (zip-pack (list (make-string 65536 #\a) "b") '("x" "yy")) ""))

;; Archives which were damaged, or made to harm: an entry which cannot be
;; read is left out, and nothing else happens.

(define (le n k)
  ;; the number n in k bytes, the lowest first
  (if (== k 0) '() (cons (remainder n 256) (le (quotient n 256) (- k 1)))))

(define (zip-patch s i n)
  ;; the archive s with the number n in the 4 bytes at i
  (string-append (substring s 0 i) (apply bytes (le n 4))
                 (substring s (+ i 4) (string-length s))))

(define (zip-central s k)
  ;; where the k-th entry of the directory of s starts
  (let loop ((i 0) (k k))
    (with j (string-search-forwards (bytes 80 75 1 2) i s)
      (if (== k 0) j (loop (+ j 4) (- k 1))))))

(define (zip-one data method size crc)
  ;; an archive with the entry "e" whose data in the archive is data, of
  ;; the method, which says that it is the size bytes whose CRC is crc
  (let* ((n (string-length data))
         (head (lambda (central?)
                 (append (le (if central? #x02014b50 #x04034b50) 4)
                         (if central? (le 20 2) '())
                         (le 20 2) (le 0 2) (le method 2) (le 0 4)
                         (le crc 4) (le n 4) (le size 4) (le 1 2) (le 0 2))))
         (local (string-append (apply bytes (head #f)) "e" data))
         ;; (no comment, the disk, the attributes, the offset 0)
         (dir (string-append (apply bytes (append (head #t) (le 0 10) (le 0 4)))
                             "e")))
    (string-append local dir
                   (apply bytes (append (le #x06054b50 4) (le 0 4) (le 1 2) (le 1 2)
                                        (le (string-length dir) 4)
                                        (le (string-length local) 4) (le 0 2))))))

(define (zip-crc s)
  ;; the CRC-32 of s, as the archives have it
  (let* ((z (zip-pack '("x") (list s)))
         (i (zip-central z 0)))
    (+ (char->integer (string-ref z (+ i 16)))
       (* 256 (char->integer (string-ref z (+ i 17))))
       (* 65536 (char->integer (string-ref z (+ i 18))))
       (* 16777216 (char->integer (string-ref z (+ i 19)))))))

(define (test-zip-damaged)
  (check-group "zip, damaged archives")
  (let ((a (zip-central zip-sample 0))
        (b (zip-central zip-sample 1))
        (only-a (list "a.txt" (apply string-append
                                     (make-list 4 "hello hello hello hello hello hello\n"))))
        (only-b '("b.txt" "TeXmacs")))
    ;; a size of the data which deflate cannot make, or which is wrong (it
    ;; was allocated before anything was read: TeXmacs ended)
    (check= (zip-unpack (zip-patch zip-sample (+ a 24) #x7fffffff)) only-b)
    (check= (zip-unpack (zip-patch zip-sample (+ a 24) 20000)) only-b)
    (check= (zip-unpack (zip-patch zip-sample (+ a 24) 145)) only-b)
    ;; offsets and sizes near the largest numbers
    (check= (zip-unpack (zip-patch zip-sample (+ a 42) #x7ffffff0)) only-b)
    (check= (zip-unpack (zip-patch zip-sample (+ a 42) #xfffffff0)) only-b)
    (check= (zip-unpack (zip-patch (zip-patch zip-sample (+ b 20) #x7ffffff0)
                                   (+ b 24) #x7ffffff0))
            only-a)
    (check= (zip-unpack (zip-patch zip-sample (- (string-length zip-sample) 6)
                                   #x7ffffff0))
            '())
    ;; data which was changed: its CRC-32 is not the one of the archive
    (with i (string-search-forwards "TeXmacs" 0 zip-sample)
      (check= (zip-unpack (string-append (substring zip-sample 0 i) "TeXmacZ"
                                         (substring zip-sample (+ i 7)
                                                    (string-length zip-sample))))
              only-a))
    ;; two entries of the directory for the same data (one archive of a
    ;; few Kb could make TeXmacs inflate it thousands of times): the data
    ;; is read for the first one
    (let* ((copy (lambda (s i j k)
                   ;; s with its k bytes at j replaced by the ones at i
                   (string-append (substring s 0 j) (substring s i (+ i k))
                                  (substring s (+ j k) (string-length s)))))
           (z (copy (copy zip-sample (+ a 10) (+ b 10) 18) (+ a 42) (+ b 42) 4)))
      (check= (zip-unpack z) only-a)))
  ;; two entries with the same name: both are read, and the importers take
  ;; the first one
  (let* ((z (zip-pack '("e" "e") '("first" "second")))
         (t (office-archive z)))
    (check= (zip-unpack z) '("e" "first" "e" "second"))
    (check= (office-entry t "e") "first"))
  ;; deflated data which is wrong: a stored block whose two lengths do not
  ;; agree, a code with more codes than its lengths allow, a distance before
  ;; the start of the data
  (with crc (zip-crc "TeXmacs")
    (check= (zip-unpack (zip-one (string-append (bytes 1 7 0 248 255) "TeXmacs")
                                 8 7 crc))
            '("e" "TeXmacs"))
    (check= (zip-unpack (zip-one (string-append (bytes 1 7 0 0 0) "TeXmacs")
                                 8 7 crc))
            '()))
  (check= (zip-unpack (zip-one (bytes 5 0 146 4) 8 1 0)) '())
  (check= (zip-unpack (zip-one (bytes 3 2 0) 8 3 0)) '()))

;; What the files say of their page, of their usual paragraph, of their
;; lists and of their headings, and the bugs which the audit of the
;; fidelity found.

(define (archive-entry z name)
  (let loop ((l (zip-unpack z)))
    (cond ((or (null? l) (null? (cdr l))) #f)
          ((== (car l) name) (cadr l))
          (else (loop (cddr l))))))

(define (docx-file body styles numbering)
  ;; a Word document with this body (with its sectPr), styles and lists
  (zip-pack
    '("[Content_Types].xml" "word/document.xml" "word/styles.xml"
      "word/numbering.xml" "word/_rels/document.xml.rels" "word/footnotes.xml")
    (list "<Types/>"
          (string-append "<?xml version='1.0'?><w:document" docx-ns "><w:body>"
                         body "</w:body></w:document>")
          (string-append "<w:styles" docx-ns ">" styles "</w:styles>")
          (string-append "<w:numbering" docx-ns ">" numbering "</w:numbering>")
          docx-rels docx-footnotes)))

(define (docx-blocks-of* archive)
  ;; the blocks of the office tree of a Word document, without the setup
  (bare (no-setup (cdr (parse-docx-document archive)))))

(define (docx-blocks-of body styles numbering)
  (bare (cdr (parse-docx-document (docx-file body styles numbering)))))

(define (odt-file body styles automatic)
  ;; an OpenDocument text with this body, these styles of styles.xml and
  ;; these automatic styles of content.xml
  (zip-pack
    '("mimetype" "content.xml" "styles.xml")
    (list "application/vnd.oasis.opendocument.text"
          (string-append "<?xml version='1.0'?><office:document-content" odt-ns ">"
                         "<office:automatic-styles>" automatic
                         "</office:automatic-styles><office:body><office:text>" body
                         "</office:text></office:body></office:document-content>")
          (string-append "<office:document-styles" odt-ns ">" styles
                         "</office:document-styles>"))))

(define (odt-blocks-of body styles automatic)
  (bare (cdr (parse-odt-document (odt-file body styles automatic)))))

(define (test-office-fidelity)
  (check-group "fidelity")
  (let* ((heading (string-append
                    "<w:style w:type='paragraph' w:styleId='H1'><w:name w:val='heading 1'/>"
                    "</w:style>"))
         (lists (string-append
                  "<w:abstractNum w:abstractNumId='0'>"
                  "<w:lvl w:ilvl='0'><w:start w:val='1'/><w:numFmt w:val='decimal'/></w:lvl>"
                  "</w:abstractNum>"
                  "<w:abstractNum w:abstractNumId='1'>"
                  "<w:lvl w:ilvl='0'><w:start w:val='3'/><w:numFmt w:val='lowerRoman'/></w:lvl>"
                  "</w:abstractNum>"
                  "<w:num w:numId='3'><w:abstractNumId w:val='0'/></w:num>"
                  "<w:num w:numId='4'><w:abstractNumId w:val='1'/></w:num>"))
         (item (lambda (id text)
                 (wp "<w:pPr><w:numPr><w:ilvl w:val='0'/><w:numId w:val='" id
                     "'/></w:numPr></w:pPr>" (wr text)))))
    ;; a heading has a number when its list numbers it, and none else (as
    ;; the headings of Word and of LibreOffice by default)
    (check= (docx-blocks-of (wp "<w:pPr><w:pStyle w:val='H1'/><w:numPr><w:ilvl w:val='0'/>"
                                "<w:numId w:val='3'/></w:numPr></w:pPr>" (wr "N"))
                            heading lists)
            '((p (@ (role "heading") (level "1")) "N")))
    (check= (docx-blocks-of (wp (wstyle "H1") (wr "U")) heading lists)
            '((p (@ (role "heading") (level "1") (numbered "no")) "U")))
    ;; the way of numbering of a list and its first number
    (check= (docx-blocks-of (string-append (item "4" "a") (item "4" "b")) heading lists)
            '((list (@ (kind "number") (format "i") (start "3"))
                    (item (p "a")) (item (p "b")))))
    ;; a list of Word which goes on after a paragraph keeps its numbers
    (check= (docx-blocks-of (string-append (item "3" "a") (wp (wr "x")) (item "3" "b"))
                            heading lists)
            '((list (@ (kind "number")) (item (p "a"))) (p "x")
              (list (@ (kind "number") (start "2")) (item (p "b")))))
    ;; a break of page in a paragraph cuts it in two
    (check= (docx (wp "<w:r><w:t>a</w:t><w:br w:type='page'/><w:t>b</w:t></w:r>"))
            '((p "a") (pagebreak) (p "b")))
    (check= (docx (string-append
                    (wp "<w:r><w:t>a</w:t><w:br w:type='page'/></w:r>")
                    (wp "<w:pPr><w:pageBreakBefore/></w:pPr>" (wr "b"))))
            '((p "a") (pagebreak) (p "b")))
    ;; the paragraphs of a box of text come after the paragraph of the box
    (check= (docx (wp (wr "a") "<w:r><w:drawing><wp:anchor><wps:txbx><w:txbxContent>"
                      (wp (wr "in the box")) "</w:txbxContent></wps:txbx></wp:anchor>"
                      "</w:drawing></w:r>" (wr " b")))
            '((p "a b") (p "in the box")))
    ;; the mark which a note writes itself is not text
    (check= (docx (wp (wr "a") "<w:r><w:footnoteReference w:customMarkFollows='1' w:id='2'/>"
                      "<w:t>*</w:t></w:r>"))
            '((p "a" (note (p "The note.")))))
    ;; a reference whose number Word did not write is one of TeXmacs
    (check= (docx (wp "<w:r><w:fldChar w:fldCharType='begin'/></w:r>"
                      "<w:r><w:instrText> REF fig1 \\h </w:instrText></w:r>"
                      "<w:r><w:fldChar w:fldCharType='end'/></w:r>"))
            '((p (ref (@ (name "fig1"))))))
    (check= (tm '(p "see " (ref (@ (name "fig1")))))
            '((concat "see " (reference "fig1"))))
    ;; the page and the usual paragraph
    (let* ((styles (string-append
                     "<w:docDefaults><w:rPrDefault><w:rPr><w:rFonts w:ascii='Arial'/>"
                     "<w:sz w:val='24'/></w:rPr></w:rPrDefault>"
                     "<w:pPrDefault><w:pPr><w:spacing w:after='160' w:line='360'"
                     " w:lineRule='auto'/></w:pPr></w:pPrDefault></w:docDefaults>"
                     "<w:style w:type='paragraph' w:default='1' w:styleId='Normal'>"
                     "<w:name w:val='Normal'/><w:pPr><w:ind w:firstLine='567'/>"
                     "<w:jc w:val='both'/></w:pPr></w:style>"))
           (body (string-append (wp (wr "x")) "<w:sectPr><w:pgSz w:w='8391' w:h='11906'/>"
                                "<w:pgMar w:top='1134' w:bottom='1134' w:left='1701'"
                                " w:right='1134'/></w:sectPr>"))
           (setup (car (docx-blocks-of body styles ""))))
      (check= setup
              '(setup (@ (page-width "14.799cm") (page-height "20.998cm")
                         (margin-top "2cm") (margin-bottom "2cm") (margin-left "3cm")
                         (margin-right "2cm") (font-size "12") (font "sans")
                         (first-indent "28.35") (space-after "8") (line-spacing "1.5")
                         (body-style "Normal") (align "justify"))))
      (check= (cAr (office->texmacs (parse-docx-document (docx-file body styles ""))))
              '(initial (collection (associate "page-type" "a5")
                                    (associate "page-orientation" "portrait")
                                    (associate "page-odd" "3cm") (associate "page-even" "3cm")
                                    (associate "page-right" "2cm") (associate "page-top" "2cm")
                                    (associate "page-bot" "2cm")
                                    (associate "font-base-size" "12")
                                    (associate "font-family" "ss")
                                    (associate "par-first" "28.35pt")
                                    (associate "par-par-sep" "8pt")
                                    (associate "par-sep" "0.8fn")))))
    ;; a paragraph which is not aligned as the usual ones says so
    (check= (tm '(setup (@ (align "left"))) '(p (@ (align "justify")) "j") '(p "l"))
            '((with "par-mode" "justify" "j") "l")))
  ;; OpenDocument: the numbers of the headings, hidden text, the labels of
  ;; the numbers, the marks of the citations, the lists, the page
  (let* ((outline (lambda (format)
                    (string-append
                      "<office:styles><text:outline-style style:name='Outline'>"
                      "<text:outline-level-style text:level='1' style:num-format='"
                      format "'/></text:outline-style>"
                      "<style:style style:name='Heading_20_1' style:display-name='Heading 1'"
                      " style:family='paragraph'/>"
                      "<text:list-style style:name='LA'>"
                      "<text:list-level-style-number text:level='1' style:num-format='a'"
                      " text:start-value='2'/></text:list-style>"
                      "</office:styles>"))))
    (check= (odt-blocks-of "<text:h text:outline-level='1'>N</text:h>" (outline "1") "")
            '((p (@ (role "heading") (level "1")) "N")))
    (check= (odt-blocks-of "<text:h text:outline-level='1'>U</text:h>" (outline "") "")
            '((p (@ (role "heading") (level "1") (numbered "no")) "U")))
    ;; (a paragraph of a style of headings is no heading of the outline)
    (check= (odt-blocks-of "<text:p text:style-name='Heading_20_1'>P</text:p>" (outline "1") "")
            '((p (@ (role "heading") (level "1") (numbered "no")) "P")))
    (check= (odt-blocks-of (string-append
                             "<text:p>a<text:span text:style-name='Hid'>hidden</text:span>b</text:p>"
                             "<text:p>c<text:hidden-paragraph text:is-hidden='true'/></text:p>")
                           (outline "")
                           (string-append "<style:style style:name='Hid' style:family='text'>"
                                          "<style:text-properties text:display='none'/>"
                                          "</style:style>"))
            '((p "ab")))
    (check= (odt-blocks-of (string-append
                             "<text:p>Figure <text:sequence text:name='Figure'"
                             " text:ref-name='refFigure0'>1</text:sequence>"
                             "<text:reference-mark-start text:name='ZOTERO_ITEM {&quot;x&quot;:1}'/>"
                             "</text:p>")
                           (outline "") "")
            '((p "Figure " (bookmark (@ (name "refFigure0")))
                 (seq (@ (name "Figure")) "1"))))
    (check= (odt-blocks-of (string-append
                             "<text:list text:style-name='LA'><text:list-item>"
                             "<text:p>x</text:p></text:list-item></text:list>")
                           (outline "") "")
            '((list (@ (kind "number") (format "a") (start "2")) (item (p "x")))))
    (check= (car (odt-blocks-of
                   "<text:p>x</text:p>"
                   (string-append
                     "<office:automatic-styles><style:page-layout style:name='pm1'>"
                     "<style:page-layout-properties fo:page-width='29.7cm'"
                     " fo:page-height='21cm' style:print-orientation='landscape'"
                     " fo:margin-top='1cm' fo:margin-bottom='1cm' fo:margin-left='1cm'"
                     " fo:margin-right='1cm'/></style:page-layout></office:automatic-styles>"
                     "<office:master-styles><style:master-page style:name='Standard'"
                     " style:page-layout-name='pm1'/></office:master-styles>")
                   ""))
            '(setup (@ (page-width "29.7cm") (page-height "21cm") (landscape "true")
                       (margin-top "1cm") (margin-bottom "1cm") (margin-left "1cm")
                       (margin-right "1cm") (align "left")))))
  ;; a brace of Word without the closing one does not take what follows
  (with t (convert (docx-archive
                     (wp "<m:oMath><m:d><m:dPr><m:begChr m:val='{'/><m:endChr m:val=''/>"
                         "</m:dPr><m:e><m:r><m:t>x</m:t></m:r></m:e></m:d>"
                         "<m:r><m:t>+y</m:t></m:r></m:oMath>"))
                   "docx-document" "texmacs-stree")
    (check= (cadr (cadr t))
            '(document (equation* (document (concat (around* "{" "x" "<nobracket>")
                                                    "+y"))))))
  ;; a caption which holds its image, as LibreOffice writes it in a frame
  (check= (tm '(p (@ (role "caption")) (image (@ (name "p.png") (data "PNGDATA")))
                  "Figure " (seq (@ (name "Figure")) "1") ": A picture"))
          '((big-figure (image (tuple (raw-data "PNGDATA") "p.png") "" "" "" "")
                        "A picture")))
  ;; the export: a space which does not break, what the expansion could not
  ;; do, the ways of numbering of the lists, the numbers of the headings of
  ;; a document which was not expanded, the page and the font
  (check-true (in? '(associate "nbsp" (xmacro "x" (eval-args "x")))
                   (cdr (tmoffice-env-patch))))
  (check= (of '(document (error "bad assign") "x")) '((p "x")))
  (check= (of '(enumerate-roman (document (concat (item) "a"))))
          '((list (@ (kind "number") (format "i")) (item (p "a")))))
  (check= (of '(document (TeXmacs "2.1") (style "book")
                         (body (document (chapter "C") (section "S") (subsection "T")
                                         (chapter "D") (section "S")))))
          '((p (@ (role "heading") (level "1") (number "1") (id "heading1")) "C")
            (p (@ (role "heading") (level "2") (number "1.1") (id "heading2")) "S")
            (p (@ (role "heading") (level "3") (number "1.1.1") (id "heading3")) "T")
            (pagebreak)
            (p (@ (role "heading") (level "1") (number "2") (id "heading4")) "D")
            (p (@ (role "heading") (level "2") (number "2.1") (id "heading5")) "S")))
  (check= (of '(chapter-title (concat "Chapter 2" (vspace "1fn") (new-line) "Endings")))
          '((p (@ (role "heading") (level "1") (number "2") (id "heading1")) "Endings")))
  (let* ((doc '(document (TeXmacs "2.1") (style "generic")
                         (body (document "x"
                                         (enumerate-alpha (document (concat (item) "a")))))
                         (initial (collection (associate "page-type" "a5")
                                              (associate "page-orientation" "landscape")
                                              (associate "page-odd" "3cm")
                                              (associate "font-base-size" "12")
                                              (associate "font-family" "ss")))))
         (docx (convert doc "texmacs-stree" "docx-document"))
         (odt (convert doc "texmacs-stree" "odt-document"))
         (text (archive-entry docx "word/document.xml"))
         (styles (archive-entry docx "word/styles.xml"))
         (numbering (archive-entry docx "word/numbering.xml"))
         (ostyles (archive-entry odt "styles.xml"))
         (content (archive-entry odt "content.xml")))
    (check-true (export-has? text "<w:pgSz w:w=\"11906\" w:h=\"8391\" w:orient=\"landscape\"/>"))
    (check-true (export-has? text "w:left=\"1701\""))
    (check-true (export-has? styles "<w:rFonts w:ascii=\"Arial\""))
    (check-true (export-has? styles "<w:sz w:val=\"24\"/>"))
    (check-true (export-has? numbering "<w:numFmt w:val=\"lowerLetter\"/>"))
    (check-true (export-has? ostyles "fo:page-width=\"21cm\" fo:page-height=\"14.8cm\""))
    (check-true (export-has? ostyles "style:print-orientation=\"landscape\""))
    (check-true (export-has? ostyles "fo:margin-left=\"3cm\""))
    (check-true (export-has? ostyles "style:font-name=\"Arial\" fo:font-size=\"12pt\""))
    (check-true (export-has? content "style:num-format=\"a\""))
    ;; and both are read back so
    (for (fm '("docx-document" "odt-document"))
      (let* ((back (convert (convert doc "texmacs-stree" fm) fm "texmacs-stree"))
             (init (cdr (cadr (cAr back)))))
        (check-true (in? '(associate "page-type" "a5") init))
        (check-true (in? '(associate "page-orientation" "landscape") init))
        (check-true (in? '(associate "font-base-size" "12") init))
        (check-true (in? '(associate "font-family" "ss") init))))))

;; The style office: what the files say which the standard tags do not,
;; kept in the document and written again.

(define (faithful office)
  ;; the TeXmacs document of the style office for an office tree
  (office->texmacs office (list (cons "office->texmacs:faithful" "on"))))

(define (faithful-body office)
  (cdr (cadr (cadr (faithful office)))))

(define (test-office-style)
  (check-group "style office")
  (let* ((styles (string-append
                   "<w:style w:type='paragraph' w:default='1' w:styleId='Normal'>"
                   "<w:name w:val='Normal'/></w:style>"
                   "<w:style w:type='paragraph' w:styleId='Warning'><w:name w:val='Warning'/>"
                   "<w:pPr><w:pBdr><w:top w:val='single'/></w:pBdr>"
                   "<w:shd w:fill='FFEEEE'/><w:jc w:val='center'/></w:pPr>"
                   "<w:rPr><w:b/><w:color w:val='C00000'/></w:rPr></w:style>"
                   "<w:style w:type='character' w:styleId='Key'><w:name w:val='Keyword'/>"
                   "<w:rPr><w:i/></w:rPr></w:style>"))
         (body (string-append
                 (wp (wr "plain"))
                 (wp "<w:pPr><w:pStyle w:val='Warning'/></w:pPr>" (wr "careful"))
                 (wp (wr "a ") (wr "key" "<w:rStyle w:val='Key'/>"))
                 (wp "<w:r><w:fldChar w:fldCharType='begin'/></w:r>"
                     "<w:r><w:instrText> PAGE </w:instrText></w:r>"
                     "<w:r><w:fldChar w:fldCharType='separate'/></w:r>" (wr "3")
                     "<w:r><w:fldChar w:fldCharType='end'/></w:r>")
                 (wp "<w:ins w:author='Ann' w:date='2026-01-01T00:00:00Z'>" (wr "new")
                     "</w:ins><w:del w:author='Bob'><w:r><w:delText>old</w:delText></w:r></w:del>")))
         (file (docx-file body styles ""))
         (office (parse-docx-document file))
         (blocks (cdr office)))
    ;; the named styles which the text uses, but the usual one
    (check= (list-find blocks (lambda (b) (func? b 'styles)))
            '(styles (style (@ (name "Key") (display "Keyword") (family "text") (em "true")))
                     (style (@ (name "Warning") (display "Warning") (family "paragraph")
                               (strong "true") (color "#c00000") (align "center")
                               (background "#ffeeee") (border "true")))))
    (check= (bare (no-setup blocks))
            '((p "plain")
              (p (@ (style "Warning") (align "center")) "careful")
              (p "a " (span (@ (style "Key")) "key"))
              (p (field (@ (code " PAGE ")) "3"))
              (p (ins (@ (author "Ann") (date "2026-01-01T00:00:00Z")) "new")
                 (del (@ (author "Bob")) "old"))))
    (check-true (func? (list-find blocks (lambda (b) (func? b 'package))) 'package))
    ;; the document: the macros of the style, with the named styles of the
    ;; file in its environment, and the file itself
    (let* ((doc (faithful office))
           (init (cdr (cadr (cAr doc)))))
      (check= (caddr doc) '(style "office"))
      (check= (cdr (cadr (cadr doc)))
              '("plain" (office-par "Warning" (with "par-mode" "center" "careful"))
                (concat "a " (office-span "Key" "key"))
                (office-field " PAGE " "3")
                (concat (office-ins "Ann" "2026-01-01T00:00:00Z" "new")
                        (office-del "Bob" "" "old"))))
      (check-true (in? '(associate "office-span-Key"
                                   (macro "body" (with "font-shape" "italic" (arg "body"))))
                       init))
      (check-true (list-find init (lambda (a) (== (cadr a) "office-par-Warning"))))
      (check-true (list-find init (lambda (a) (== (cadr a) "office-package"))))
      ;; without the style: the look of the named styles of text, the
      ;; results of the fields, the text with its changes accepted
      (check= (cdr (cadr (cadr (office->texmacs office))))
              '("plain" (with "par-mode" "center" "careful")
                (concat "a " (with "font-shape" "italic" "key")) "3" "new"))
      ;; written again: the styles of the file, its runs of named styles,
      ;; the field and the changes
      (let* ((back (convert doc "texmacs-stree" "docx-document"))
             (text (archive-entry back "word/document.xml"))
             (styles2 (archive-entry back "word/styles.xml")))
        (check-true (export-has? text "<w:pStyle w:val=\"Warning\"/>"))
        (check-true (export-has? text "<w:rStyle w:val=\"Key\"/>"))
        (check-true (export-has? text " PAGE </w:instrText>"))
        (check-true (export-has? text "<w:ins w:id="))
        (check-true (export-has? text "<w:delText xml:space=\"preserve\">old</w:delText>"))
        (check-true (export-has? styles2 "w:styleId=\"Warning\""))
        (check= (bare (no-setup (cdr (parse-docx-document back))))
                (bare (no-setup blocks)))))
    ;; comments, headers, sections in columns, cropped pictures
    (let* ((parts (lambda (body . extra)
                    (cdr (parse-docx-document
                           (zip-pack (append '("word/document.xml" "word/_rels/document.xml.rels")
                                             (map car extra))
                                     (append (list (string-append
                                                     "<?xml version='1.0'?><w:document" docx-ns
                                                     "><w:body>" body "</w:body></w:document>")
                                                   (string-append
                                                     "<Relationships><Relationship Id='rH'"
                                                     " Target='header1.xml'/>"
                                                     "<Relationship Id='rI' Target='media/p.png'/>"
                                                     "</Relationships>"))
                                             (map cdr extra)))))))
           (l (parts (string-append
                       (wp (wr "x") "<w:r><w:commentReference w:id='0'/></w:r>")
                       (wp (wr "one") "<w:pPr><w:sectPr><w:cols w:num='1'/></w:sectPr></w:pPr>")
                       (wp (wr "two"))
                       (wp "<w:r><w:drawing><wp:inline><wp:extent cx='720000' cy='360000'/>"
                           "<pic:blipFill><a:blip r:embed='rI'/>"
                           "<a:srcRect l='25000' r='25000'/></pic:blipFill>"
                           "</wp:inline></w:drawing></w:r>")
                       "<w:sectPr><w:headerReference w:type='default' r:id='rH'/>"
                       "<w:pgSz w:w='11906' w:h='16838'/><w:cols w:num='2'/></w:sectPr>")
                     (cons "word/comments.xml"
                           (string-append "<w:comments" docx-ns "><w:comment w:id='0'"
                                          " w:author='Ann'>" (wp (wr "a remark"))
                                          "</w:comment></w:comments>"))
                     (cons "word/header1.xml"
                           (string-append "<w:hdr" docx-ns ">"
                                          (wp (wr "Title") "<w:r><w:tab/></w:r>"
                                              "<w:r><w:fldChar w:fldCharType='begin'/></w:r>"
                                              "<w:r><w:instrText> PAGE </w:instrText></w:r>"
                                              "<w:r><w:fldChar w:fldCharType='separate'/></w:r>"
                                              (wr "1") "<w:r><w:fldChar w:fldCharType='end'/></w:r>")
                                          "</w:hdr>"))
                     (cons "word/media/p.png" "PNGDATA"))))
      (check= (list-find l (lambda (b) (func? b 'header)))
              '(header (@ (place "header") (kind "default"))
                       (p "Title" (tab) (field (@ (code " PAGE ")) "1"))))
      (check= (bare (no-setup l))
              '((p "x" (comment (@ (author "Ann")) (p "a remark")))
                (p "one")
                (columns (@ (count "2"))
                         (p "two")
                         (p (image (@ (name "p.png") (data "PNGDATA") (width "2cm")
                                      (height "1cm") (crop "0.25 0 0.25 0")))))))
      (let* ((doc (faithful (cons 'office l)))
             (init (cdr (cadr (cAr doc)))))
        (check-true (in? '(associate "page-odd-header"
                                     (concat "Title" (htab "5mm") (page-the-page)))
                         init))
        (check= (faithful-body (cons 'office l))
                '((concat "x" (office-comment "Ann" "" "a remark"))
                  "one"
                  (office-columns "2"
                    (document "two"
                              (clipped (image (tuple (raw-data "PNGDATA") "p.png")
                                              "4cm" "1cm" "" "")
                                       "0.25w" "0h" "0.75w" "1h")))))
        ;; written again: the comment, the section, the part of the picture
        (let* ((back (convert doc "texmacs-stree" "docx-document"))
               (text (archive-entry back "word/document.xml")))
          (check-true (string? (archive-entry back "word/comments.xml")))
          (check-true (export-has? text "<w:commentReference w:id=\"0\"/>"))
          (check-true (export-has? text "<w:cols w:num=\"2\""))
          (check-true (export-has? text "<a:srcRect l=\"25000\" t=\"0\" r=\"25000\" b=\"0\"/>"))))))
  ;; OpenDocument: named styles, a field, a comment, the changes, a section
  (let* ((styles (string-append
                   "<office:styles>"
                   "<style:style style:name='Note' style:family='paragraph'>"
                   "<style:paragraph-properties fo:margin-left='1cm'/></style:style>"
                   "<style:style style:name='Key' style:family='text'>"
                   "<style:text-properties fo:font-weight='bold'/></style:style>"
                   "</office:styles>"))
         (auto (string-append "<style:style style:name='S1' style:family='section'>"
                              "<style:section-properties><style:columns fo:column-count='2'/>"
                              "</style:section-properties></style:style>"))
         (body (string-append
                 "<text:tracked-changes><text:changed-region text:id='c1'><text:insertion>"
                 "<office:change-info><dc:creator>Ann</dc:creator></office:change-info>"
                 "</text:insertion></text:changed-region>"
                 "<text:changed-region text:id='c2'><text:deletion><office:change-info>"
                 "<dc:creator>Bob</dc:creator></office:change-info><text:p>gone</text:p>"
                 "</text:deletion></text:changed-region></text:tracked-changes>"
                 "<text:p text:style-name='Note'>note</text:p>"
                 "<text:p>a <text:span text:style-name='Key'>key</text:span> page "
                 "<text:page-number text:select-page='current'>2</text:page-number></text:p>"
                 "<text:p>x<office:annotation><dc:creator>Ann</dc:creator>"
                 "<text:p>remark</text:p></office:annotation></text:p>"
                 "<text:p><text:change-start text:change-id='c1'/>new"
                 "<text:change-end text:change-id='c1'/><text:change text:change-id='c2'/></text:p>"
                 "<text:section text:style-name='S1' text:name='A'><text:p>col</text:p>"
                 "</text:section>"))
         (office (parse-odt-document (odt-file body styles auto))))
    (check= (bare (no-setup (cdr office)))
            '((p (@ (style "Note")) "note")
              (p "a " (span (@ (style "Key")) "key") " page "
                 (field (@ (odt "text:page-number")
                           (xml "<text:page-number text:select-page=\"current\"/>"))
                        "2"))
              (p "x" (comment (@ (author "Ann")) (p "remark")))
              (p (ins (@ (author "Ann")) "new") (del (@ (author "Bob")) "gone"))
              (columns (@ (count "2")) (p "col"))))
    (let* ((doc (faithful office))
           (back (convert doc "texmacs-stree" "odt-document"))
           (content (archive-entry back "content.xml")))
      (check-true (export-has? content "text:style-name=\"Note\""))
      (check-true (export-has? content "<text:span text:style-name=\"Key\">key</text:span>"))
      (check-true (export-has? content "<text:page-number text:select-page=\"current\">2</text:page-number>"))
      (check-true (export-has? content "<office:annotation><dc:creator>Ann</dc:creator>"))
      (check-true (export-has? content "<text:tracked-changes>"))
      (check-true (export-has? content "<style:columns fo:column-count=\"2\""))
      (check= (bare (no-setup (cdr (parse-odt-document back))))
              (bare (no-setup (cdr office))))))
  ;; the export: lines of equations are an array of equations of Word, the
  ;; screens of a presentation are all written, each on a page
  (let* ((doc '(document (TeXmacs "2.1") (style "generic")
                 (body (document (eqnarray* (document (tformat (table (row (cell "a") (cell "=")
                                                                           (cell "b"))))))))))
         (text (archive-entry (convert doc "texmacs-stree" "docx-document") "word/document.xml")))
    (check-true (export-has? text "<m:eqArr><m:e><m:r><m:t>a</m:t></m:r><m:r><m:t>&amp;</m:t>")))
  ;; the formatting by hand: the size, the font and the highlighting of a
  ;; text, the indentations, the spaces, the line spacing, the background
  ;; and the border of a paragraph, from the file to TeXmacs and back
  (let* ((body (string-append
                 (wp "<w:pPr><w:pBdr><w:top w:val='single'/></w:pBdr>"
                     "<w:shd w:fill='FFEEDD'/><w:spacing w:before='240' w:after='120'"
                     " w:line='360' w:lineRule='auto'/><w:ind w:left='720' w:right='360'"
                     " w:hanging='360'/></w:pPr>"
                     (wr "big" "<w:rFonts w:ascii='Zzz Sans'/><w:sz w:val='28'/>")
                     (wr " green" "<w:highlight w:val='green'/>")
                     (wr " blue" "<w:shd w:fill='BDD7EE'/>"))))
         (blocks (docx body))
         (doc (office->texmacs (cons 'office blocks))))
    (check= blocks
            '((p (@ (indent-left "36") (indent-right "18") (first-indent "-18")
                    (space-before "12") (space-after "6") (line-spacing "1.5")
                    (background "#ffeedd") (border "true"))
                 (font (@ (value "Zzz Sans")) (size (@ (value "14")) "big"))
                 (mark (@ (value "#00ff00")) " green")
                 (mark (@ (value "#bdd7ee")) " blue"))))
    (check= (cdr (cadr (cadr doc)))
            '((with "par-left" "36pt" "par-right" "18pt" "par-first" "-18pt"
                    "par-sep" "0.8fn"
                (surround (vspace* "12pt") (vspace "6pt")
                  (with "ornament-color" "#ffeedd" "ornament-border" "1ln"
                    (ornamented
                      (concat (with "font-family" "ss" (with "font-size" "1.4" "big"))
                              (with "marked-color" "#00ff00" (marked " green"))
                              (with "marked-color" "#bdd7ee" (marked " blue")))))))))
    ;; and back: the same paragraph, in both formats
    (let* ((tm `(document (TeXmacs "2.1") (style "generic") (body ,(cadr (cadr doc)))))
           (docx2 (convert tm "texmacs-stree" "docx-document"))
           (text (archive-entry docx2 "word/document.xml"))
           (odt2 (convert tm "texmacs-stree" "odt-document"))
           (content (archive-entry odt2 "content.xml")))
      (check-true (export-has? text "<w:sz w:val=\"28\"/>"))
      (check-true (export-has? text "<w:highlight w:val=\"green\"/>"))
      (check-true (export-has? text "w:fill=\"BDD7EE\""))
      (check-true (export-has? text "<w:ind w:left=\"720\" w:right=\"360\" w:hanging=\"360\"/>"))
      (check-true (export-has? text "<w:spacing w:before=\"240\" w:after=\"120\" w:line=\"360\" w:lineRule=\"auto\"/>"))
      (check-true (export-has? content "fo:margin-left=\"36pt\""))
      (check-true (export-has? content "fo:font-size=\"14pt\""))
      (check-true (export-has? content "fo:background-color=\"#bdd7ee\""))
      ;; (a font which TeXmacs does not have is the closest family, here)
      (check= (no-setup (cdr (parse-docx-document docx2)))
              (list (apply office-node
                           (cons* 'p (map (lambda (a) (list (car a) (cadr a)))
                                          (ox-attrs (car blocks)))
                                  (cons '(font (@ (value "Arial")) (size (@ (value "14")) "big"))
                                        (cdr (ox-children (car blocks)))))))))
    ;; in the style office, the name of the font is kept
    (with doc2 (office->texmacs (cons 'office blocks)
                                (list (cons "office->texmacs:faithful" "on")))
      (check-true (>= (string-search-forwards "(office-font \"Zzz Sans\" \"ss\""
                                              0 (object->string doc2)) 0))
      (check-true (export-has? (archive-entry (convert doc2 "texmacs-stree" "docx-document")
                                              "word/document.xml")
                               "w:ascii=\"Zzz Sans\""))))
  ;; capitals, underlines and lines through of other kinds, spaced letters
  ;; and a font which TeXmacs has, from the file to TeXmacs and back
  (let* ((tnr (if (in? "Times New Roman" (font-database-families))
                  '(with "font" "Times New Roman" "f")
                  '(office-font "Times New Roman" "rm" "f")))
         (body (wp (wr "a" "<w:caps/>")
                   (wr "b" "<w:u w:val='double'/>")
                   (wr "c" "<w:u w:val='wave'/>")
                   (wr "d" "<w:dstrike/>")
                   (wr "e" "<w:spacing w:val='40'/>")
                   (wr "f" "<w:rFonts w:ascii='Times New Roman'/>")))
         (blocks (docx body))
         (opts (list (cons "office->texmacs:faithful" "on")))
         (doc (office->texmacs (cons 'office blocks) opts))
         (text (archive-entry (convert doc "texmacs-stree" "docx-document")
                              "word/document.xml"))
         (content (archive-entry (convert doc "texmacs-stree" "odt-document")
                                 "content.xml")))
    (check= blocks
            '((p (caps "a") (underline (@ (value "double")) "b")
                 (underline (@ (value "wave")) "c") (strike (@ (value "double")) "d")
                 (letters (@ (value "2")) "e") (font (@ (value "Times New Roman")) "f"))))
    ;; (a font which this computer does not have keeps its name)
    (check= (cadr (cadr doc))
            `(document (concat (change-case "a" "UPCASE") (office-underline "double" "b")
                               (office-underline "wave" "c") (office-strike "double" "d")
                               (office-letters "2" "e") ,tnr)))
    (check-true (export-has? text "<w:caps/>"))
    (check-true (export-has? text "<w:u w:val=\"double\"/>"))
    (check-true (export-has? text "<w:u w:val=\"wave\"/>"))
    (check-true (export-has? text "<w:dstrike/>"))
    (check-true (export-has? text "<w:spacing w:val=\"40\"/>"))
    (check-true (export-has? text "w:ascii=\"Times New Roman\""))
    (check-true (export-has? content "fo:text-transform=\"uppercase\""))
    (check-true (export-has? content "style:text-underline-type=\"double\""))
    (check-true (export-has? content "style:text-underline-style=\"wave\""))
    (check-true (export-has? content "style:text-line-through-type=\"double\""))
    (check-true (export-has? content "fo:letter-spacing=\"2pt\""))
    ;; without the style office: what TeXmacs can show
    ;; (the rest depends on the fonts of the computer: "e" and the text in
    ;; Times New Roman are one string when it does not have this font)
    (check= (list-head (cdr (cadr (cadr (cadr (office->texmacs (cons 'office blocks)))))) 4)
            '((change-case "a" "UPCASE") (underline "b") (underline "c")
              (strike-through "d")))
    ;; values which the office formats do not have are left out
    (with text (archive-entry
                 (convert '(document (TeXmacs "2.1") (style "office")
                             (body (document (concat (office-underline "zigzag" "a")
                                                     (office-letters "2pt" "b")
                                                     (office-letters "x" "c")))))
                          "texmacs-stree" "docx-document")
                 "word/document.xml")
      (check-true (export-has? text "<w:u w:val=\"single\"/>"))
      (check-true (export-has? text "<w:spacing w:val=\"40\"/>"))
      (check-false (export-has? text "zigzag"))))
  ;; the look of the headings of the file (here bold) is the one of the
  ;; titles of the sections
  (let* ((doc (office->texmacs
                (parse-docx-document
                  (docx-archive (wp (wstyle "Titre1") (wr "Head"))))
                (list (cons "office->texmacs:faithful" "on"))))
         (s (object->string doc)))
    (check-true (>= (string-search-forwards "(associate \"section-title\"" 0 s) 0))
    (check-true (>= (string-search-forwards "\"font-series\" \"bold\"" 0 s) 0))
    (check-true (>= (string-search-forwards "(section* \"Head\")" 0 s) 0)))
  ;; in the named styles too: the capitals and the kind of underline of a
  ;; style of headings and of a style of text
  (let* ((styles (string-append
                   "<w:style w:type='paragraph' w:styleId='H1'><w:name w:val='heading 1'/>"
                   "<w:rPr><w:caps/></w:rPr></w:style>"
                   "<w:style w:type='character' w:styleId='DU'><w:name w:val='Double'/>"
                   "<w:rPr><w:u w:val='double'/></w:rPr></w:style>"))
         (doc (office->texmacs
                (parse-docx-document
                  (docx-file (wp (wstyle "H1") (wr "Head")) styles ""))
                (list (cons "office->texmacs:faithful" "on"))))
         (doc2 (office->texmacs
                 (parse-docx-document
                   (docx-file (wp "<w:r><w:rPr><w:rStyle w:val='DU'/></w:rPr><w:t>u</w:t></w:r>")
                              styles ""))
                 (list (cons "office->texmacs:faithful" "on")))))
    (check-true (>= (string-search-forwards "(change-case (arg \"name\") \"UPCASE\")"
                                            0 (object->string doc)) 0))
    (check-true (>= (string-search-forwards "(office-underline \"double\" (arg \"body\"))"
                                            0 (object->string doc2)) 0)))
  ;; lower case, and the first letters of the words in capitals
  (let* ((auto (string-append
                 "<style:style style:name='L' style:family='text'>"
                 "<style:text-properties fo:text-transform='lowercase'/></style:style>"
                 "<style:style style:name='C' style:family='text'>"
                 "<style:text-properties fo:text-transform='capitalize'/></style:style>"))
         (blocks (odt-blocks-of (tp (tspan "L" "ABC") " " (tspan "C" "two words")) "" auto))
         (doc (office->texmacs (cons 'office blocks)
                               (list (cons "office->texmacs:faithful" "on"))))
         (odt2 (archive-entry (convert doc "texmacs-stree" "odt-document") "content.xml"))
         (docx2 (archive-entry (convert doc "texmacs-stree" "docx-document")
                               "word/document.xml")))
    (check= blocks '((p (caps (@ (value "lower")) "ABC") " "
                        (caps (@ (value "capital")) "two words"))))
    (check= (cadr (cadr doc))
            '(document (concat (change-case "ABC" "locase") " "
                               (office-capitalize "two words"))))
    (check= (cadr (cadr (office->texmacs (cons 'office blocks))))
            '(document (concat (change-case "ABC" "locase") " two words")))
    (check-true (export-has? odt2 "fo:text-transform=\"lowercase\""))
    (check-true (export-has? odt2 "fo:text-transform=\"capitalize\""))
    ;; (Word has neither: the text is written as it is shown)
    (check-true (export-has? docx2 ">abc</w:t>"))
    (check-false (export-has? docx2 "<w:caps/>")))
  ;; a picture which floats on the right of its paragraph, the text
  ;; around it: a paragraph of its own before it, and back in it when the
  ;; file is written
  (let* ((body (wp "<w:r><w:drawing><wp:anchor behindDoc='0'>"
                   "<wp:positionH relativeFrom='column'><wp:align>right</wp:align></wp:positionH>"
                   "<wp:positionV relativeFrom='paragraph'><wp:posOffset>36000</wp:posOffset>"
                   "</wp:positionV><wp:extent cx='720000' cy='360000'/>"
                   "<wp:wrapSquare wrapText='bothSides'/><a:blip r:embed='rId2'/>"
                   "</wp:anchor></w:drawing></w:r>" (wr "Text around.")))
         (blocks (docx body))
         (doc (office->texmacs (cons 'office blocks)
                               (list (cons "office->texmacs:faithful" "on"))))
         (back (convert doc "texmacs-stree" "docx-document"))
         (text (archive-entry back "word/document.xml"))
         (odt2 (archive-entry (convert doc "texmacs-stree" "odt-document") "content.xml")))
    (check= blocks
            '((p (image (@ (name "pic.png") (data "PNGDATA") (width "2cm") (height "1cm")
                           (float "right") (wrap "square") (wrap-side "both")
                           (h-pos "right") (h-rel "column") (v-pos "offset")
                           (v-rel "paragraph") (v-offset "0.1cm")))
                 "Text around.")))
    (check= (cadr (cadr doc))
            '(document (with "par-mode" "right"
                         (office-float "right" "wrap=square wrap-side=both h-pos=right h-rel=column v-pos=offset v-rel=paragraph v-offset=0.1cm next=true"
                                       (image (tuple (raw-data "PNGDATA") "pic.png")
                                              "2cm" "1cm" "" "")))
                       "Text around."))
    ;; (without the style office: the picture on its side, in the line)
    (check= (cadr (cadr (office->texmacs (cons 'office blocks))))
            '(document (with "par-mode" "right"
                         (image (tuple (raw-data "PNGDATA") "pic.png") "2cm" "1cm" "" ""))
                       "Text around."))
    ;; (read again: the same place, in the same paragraph)
    (with p (car (no-setup (cdr (parse-docx-document back))))
      (check= (list-filter (ox-attrs (cadr p)) (lambda (a) (in? (car a) office-float-keys)))
              (list-filter (ox-attrs (cadar blocks)) (lambda (a) (in? (car a) office-float-keys))))
      (check= (cddr p) '("Text around.")))
    (check-true (export-has? text "<wp:align>right</wp:align>"))
    (check-true (export-has? text "<wp:posOffset>36000</wp:posOffset>"))
    (check-true (export-has? text "<wp:wrapSquare wrapText=\"bothSides\"/>"))
    (check-false (export-has? text "<wp:inline"))
    (check-true (export-has? odt2 "text:anchor-type=\"paragraph\""))
    (check-true (export-has? odt2 "style:wrap=\"parallel\""))
    (check-true (export-has? odt2 "style:horizontal-pos=\"right\""))
    (check-true (export-has? odt2 "svg:y=\"0.1cm\"")))
  ;; a frame of OpenDocument which floats, and a picture of which parts are
  ;; cut (fo:clip, in the size of the picture: here 100 pixels at 96 dots
  ;; per inch, 2.646cm)
  (let* ((png (string-append (list->string (map integer->char '(137 80 78 71 13 10 26 10 0 0 0 13)))
                             "IHDR" (list->string (map integer->char '(0 0 0 100 0 0 0 100)))
                             "rest"))
         (archive (zip-pack '("mimetype" "content.xml" "styles.xml" "Pictures/p.png")
                            (list "application/vnd.oasis.opendocument.text"
                                  (string-append
                                    "<?xml version='1.0'?><office:document-content" odt-ns ">"
                                    "<office:automatic-styles>"
                                    "<style:style style:name='F' style:family='graphic'>"
                                    "<style:graphic-properties style:wrap='left'"
                                    " style:horizontal-pos='from-left' style:horizontal-rel='paragraph'"
                                    " style:vertical-pos='top' style:vertical-rel='paragraph'/></style:style>"
                                    "<style:style style:name='C' style:family='graphic'>"
                                    "<style:graphic-properties fo:clip='rect(0cm, 0.6615cm, 0cm, 0cm)'/>"
                                    "</style:style></office:automatic-styles><office:body><office:text>"
                                    "<text:p><draw:frame draw:style-name='F' text:anchor-type='paragraph'"
                                    " svg:x='12cm' svg:width='3cm' svg:height='2cm'>"
                                    "<draw:image xlink:href='Pictures/p.png'/></draw:frame>Text</text:p>"
                                    "<text:p><draw:frame draw:style-name='C' text:anchor-type='as-char'"
                                    " svg:width='3cm' svg:height='4cm'>"
                                    "<draw:image xlink:href='Pictures/p.png'/></draw:frame></text:p>"
                                    "</office:text></office:body></office:document-content>")
                                  (string-append "<office:document-styles" odt-ns "/>")
                                  png)))
         (blocks (bare (cdr (parse-odt-document archive)))))
    (check= (map (lambda (b) (list-filter (ox-attrs (cadr b))
                                          (lambda (a) (not (in? (car a) '(data name))))))
                 blocks)
            '(((width "3cm") (height "2cm") (float "right") (wrap "square")
               (wrap-side "left") (h-pos "offset") (h-rel "column") (h-offset "12cm")
               (v-pos "top") (v-rel "paragraph"))
              ((width "3cm") (height "4cm") (crop "0 0 0.25 0"))))
    (with content (archive-entry (convert (office->texmacs (cons 'office blocks)
                                                           (list (cons "office->texmacs:faithful" "on")))
                                          "texmacs-stree" "odt-document")
                                 "content.xml")
      (check-true (export-has? content "fo:clip=\"rect(0cm, 0.661cm, 0cm, 0cm)\""))
      (check-true (export-has? content "style:wrap=\"left\""))
      (check-true (export-has? content "svg:x=\"12cm\""))))
  ;; a section in columns with a line between them and a space of 1 cm
  (let* ((blocks (docx (string-append
                         (wp (wr "one") "<w:pPr><w:sectPr><w:cols w:num='1'/></w:sectPr></w:pPr>")
                         (wp (wr "two") "<w:pPr><w:sectPr><w:cols w:num='2' w:sep='1'"
                             " w:space='567'/></w:sectPr></w:pPr>"))))
         (doc (office->texmacs (cons 'office blocks)
                               (list (cons "office->texmacs:faithful" "on"))))
         (text (archive-entry (convert doc "texmacs-stree" "docx-document") "word/document.xml"))
         (content (archive-entry (convert doc "texmacs-stree" "odt-document") "content.xml")))
    (check= (cadr blocks) '(columns (@ (count "2") (line "true") (gap "1cm")) (p "two")))
    (check= (caddr (cadr (cadr doc))) '(office-columns* "2" "true" "1cm" (document "two")))
    (check-true (export-has? text "<w:cols w:num=\"2\" w:sep=\"1\" w:space=\"567\"/>"))
    (check-true (export-has? content "<style:column-sep"))
    (check-true (export-has? content "fo:column-gap=\"1cm\"")))
  ;; the underline of a link is the one of its look: Word leaves it out
  (check= (docx (wp "<w:hyperlink w:anchor='x'><w:r><w:rPr><w:rStyle w:val='Hyperlink'/>"
                    "<w:u w:val='single'/></w:rPr><w:t>l</w:t></w:r></w:hyperlink>"))
          '((p (link (@ (href "#x")) "l"))))
  ;; a formula alone in a paragraph of a cell of several paragraphs, which
  ;; Word displays, stays in the cell
  (check= (cadr (office->texmacs
                  '(office (table (row (cell (p "a")
                                             (p (math (@ (display "true") (form "sxml"))
                                                      (math (mi "x"))))))))))
          '(body (document (block (tformat (twith "table-width" "1par") (twith "table-hmode" "exact")
                                           (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                                           (table (row (cell (document "a" (math "x"))))))))))
  ;; a box of text which floats on the left of its paragraph, and an
  ;; ellipse with text in its line: their size, fill, line and text
  (let* ((wsp (lambda (geom fill txt)
                (string-append
                  "<wps:wsp><wps:spPr><a:prstGeom prst='" geom "'/>"
                  "<a:solidFill><a:srgbClr val='" fill "'/></a:solidFill>"
                  "<a:ln><a:noFill/></a:ln></wps:spPr>"
                  (if (== txt "") ""
                      (string-append "<wps:txbx><w:txbxContent>" (wp (wr txt))
                                     "</w:txbxContent></wps:txbx>"))
                  "<wps:bodyPr lIns='0' tIns='0' rIns='0' bIns='0'><a:spAutoFit/></wps:bodyPr>"
                  "</wps:wsp>")))
         (body (string-append
                 (wp "<w:r><w:drawing><wp:anchor behindDoc='0'>"
                     "<wp:positionH relativeFrom='column'><wp:align>left</wp:align></wp:positionH>"
                     "<wp:positionV relativeFrom='paragraph'><wp:posOffset>0</wp:posOffset>"
                     "</wp:positionV><wp:extent cx='1800000' cy='720000'/>"
                     "<wp:wrapSquare wrapText='bothSides'/>"
                     (wsp "rect" "FFF2CC" "In the box") "</wp:anchor></w:drawing></w:r>"
                     (wr "Text around."))
                 (wp "<w:r><w:drawing><wp:inline><wp:extent cx='720000' cy='360000'/>"
                     (wsp "ellipse" "70AD47" "") "</wp:inline></w:drawing></w:r>")))
         (blocks (docx body))
         (doc (office->texmacs (cons 'office blocks)
                               (list (cons "office->texmacs:faithful" "on"))))
         (text (archive-entry (convert doc "texmacs-stree" "docx-document") "word/document.xml"))
         (content (archive-entry (convert doc "texmacs-stree" "odt-document") "content.xml")))
    (check= blocks
            '((p (box (@ (float "left") (wrap "square") (wrap-side "both") (h-pos "left")
                         (h-rel "column") (v-pos "offset") (v-rel "paragraph") (v-offset "0cm")
                         (width "5cm") (height "2cm") (shape "rect") (fill "#fff2cc")
                         (line "none") (inset "0,0,0,0") (grow "true"))
                      (p "In the box"))
                 "Text around.")
              (p (box (@ (width "2cm") (height "1cm") (shape "ellipse") (fill "#70ad47")
                         (line "none") (inset "0,0,0,0") (grow "true"))))))
    (check= (cadr (cadr (cadr doc)))
            '(with "par-mode" "left"
               (office-box "left" "wrap=square wrap-side=both h-pos=left h-rel=column v-pos=offset v-rel=paragraph v-offset=0cm height=2cm shape=rect inset=0,0,0,0 grow=true next=true"
                           "5cm" "#fff2cc" "none" (document "In the box"))))
    (check-true (>= (string-search-forwards "(office-shape \"\" " 0 (object->string doc)) 0))
    ;; read again from both formats: the same boxes
    (check= (docx-blocks-of* (convert doc "texmacs-stree" "docx-document")) blocks)
    (check-true (export-has? text "<wps:txbx><w:txbxContent>"))
    (check-true (export-has? text "<a:prstGeom prst=\"ellipse\">"))
    (check-true (export-has? text "<a:spAutoFit/>"))
    (check-true (export-has? content "<draw:text-box"))
    (check-true (export-has? content "draw:type=\"ellipse\""))
    (check-true (export-has? content "fo:background-color=\"#fff2cc\"")))
  ;; a box of text of VML, in an older document
  (check= (docx (wp "<w:r><w:pict><v:rect style='position:absolute;width:100pt;height:50pt;"
                    "mso-position-horizontal:right' fillcolor='#ffff00' stroked='f'>"
                    "<v:textbox><w:txbxContent>" (wp (wr "old")) "</w:txbxContent></v:textbox>"
                    "<w10:wrap type='square'/></v:rect></w:pict></w:r>" (wr "x")))
          '((p (box (@ (float "right") (wrap "square") (wrap-side "both") (h-pos "right")
                       (h-rel "column") (v-pos "offset") (v-rel "paragraph")
                       (width "3.528cm") (height "1.764cm") (shape "rect") (fill "#ffff00")
                       (line "none"))
                    (p "old"))
               "x")))
  ;; a frame with a box of text and a shape of OpenDocument
  (check= (odt-blocks-of
            (tp "<draw:frame draw:style-name='B' text:anchor-type='paragraph' svg:width='4cm'"
                " svg:height='2cm'><draw:text-box>" (tp "in") "</draw:text-box></draw:frame>"
                "<draw:custom-shape draw:style-name='S' text:anchor-type='as-char' svg:width='2cm'"
                " svg:height='1cm'><draw:enhanced-geometry draw:type='ellipse'/></draw:custom-shape>"
                "t")
            ""
            (string-append
              "<style:style style:name='B' style:family='graphic'><style:graphic-properties"
              " style:wrap='right' style:horizontal-pos='left' style:horizontal-rel='paragraph'"
              " fo:background-color='#ffeeee' fo:border='0.06pt solid #ff0000'/></style:style>"
              "<style:style style:name='S' style:family='graphic'><style:graphic-properties"
              " draw:fill='solid' draw:fill-color='#00ff00' draw:stroke='none'/></style:style>"))
          '((p (box (@ (float "left") (wrap "square") (wrap-side "right") (h-pos "left")
                       (h-rel "column") (v-pos "top") (v-rel "paragraph") (width "4cm")
                       (height "2cm") (shape "rect") (fill "#ffeeee") (line "#ff0000")
                       (line-width "0.06"))
                    (p "in"))
               (box (@ (width "2cm") (height "1cm") (shape "ellipse") (fill "#00ff00")
                       (line "none")))
               "t")))
  ;; a font of OpenDocument by the name of its declaration: its family
  (check= (odt-blocks-of (tp (tspan "T1" "x"))
                         (string-append
                           "<office:font-face-decls><style:font-face style:name='Zzz Sans1'"
                           " svg:font-family='&apos;Zzz Sans&apos;'/></office:font-face-decls>")
                         (string-append
                           "<style:style style:name='T1' style:family='text'>"
                           "<style:text-properties style:font-name='Zzz Sans1'/></style:style>"))
          '((p (font (@ (value "Zzz Sans")) "x"))))
  ;; an array of equations as Word saves it (one run, its marks inside) is
  ;; lines of equations again
  (check= (cadr (office->texmacs
                  (parse-docx-document
                    (docx-archive
                      (wp "<m:oMathPara><m:oMath><m:eqArr><m:e><m:r><m:t>a&amp;=b</m:t></m:r>"
                          "</m:e><m:e><m:r><m:t>&amp;=c</m:t></m:r></m:e></m:eqArr>"
                          "</m:oMath></m:oMathPara>")))))
          '(body (document (eqnarray* (document (tformat (table (row (cell "a") (cell "=b"))
                                                                (row (cell "") (cell "=c")))))))))
  (check= (of '(screens (shown "one") (hidden "two")))
          '((p "one") (pagebreak) (p "two"))))

;; The parser of XML, which the office formats share with HTML: elements
;; inside each other and entities which expand to entities have limits.
(define (test-xml-limits)
  (check-group "xml limits")
  (let* ((rep (lambda (n s) (apply string-append (make-list n s))))
         (depth (lambda (t)
                  (let loop ((t t) (d 0))
                    (if (and (pair? t) (pair? (cdr t)) (pair? (cadr t)))
                        (loop (cadr t) (+ d 1))
                        d))))
         (deep (parse-xml (string-append (rep 3000 "<a>") "x" (rep 3000 "</a>")))))
    (check-true (<= (depth deep) 1001))
    ;; (the text is still there; searched in the tree, as the printer of
    ;; s7 shortens long lists, with a list of the subtrees still to search,
    ;; as a recursion 1000 levels deep overflows the stack of femtolisp)
    (check-true (let find ((todo (list deep)))
                  (and (pair? todo)
                       (let ((t (car todo)))
                         (or (== t "x")
                             (find (if (pair? t) (append t (cdr todo))
                                       (cdr todo))))))))
    ;; ten levels of entities of ten each: 10^10 characters
    (let* ((names '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"))
           (decl (apply string-append
                        (cons "<!ENTITY a \"aaaaaaaaaa\">"
                              (map (lambda (p q)
                                     (string-append "<!ENTITY " q " \""
                                                    (rep 10 (string-append "&" p ";"))
                                                    "\">"))
                                   (cDr names) (cdr names)))))
           (t (parse-xml (string-append "<!DOCTYPE r [" decl "]><r>&j;</r>"))))
      (check-true (< (string-length (object->string t)) 5000000)))
    ;; the entities which every document has are not counted
    (check= (parse-xml "<r>&amp;&lt;</r>") '(*TOP* (r "&<")))))

;; Numbers of a file which Scheme would read as others: the picture has
;; no size, and the document is read.
(define (test-office-odd-values)
  (check-group "odd values")
  (for (cx '("1e999" "1+2i" "-5" "1/0" "#x10"))
    (check= (docx (wp "<w:r><w:drawing><wp:inline><wp:extent cx='" cx
                      "' cy='360000'/><a:blip r:embed='rId2'/></wp:inline>"
                      "</w:drawing></w:r>"))
            '((p (image (@ (name "pic.png") (data "PNGDATA") (height "1cm")))))))
  ;; what was not foreseen gives a document which says so
  (check= (car (convert "x" "office-stree" "texmacs-stree")) 'document))

;; Long documents are read and written: the functions which walk the
;; blocks and the runs are loops (the stack of Guile 1.8 ended at 10000).
(define (test-office-long)
  (check-group "long documents")
  (let* ((n 12000)
         (pars (map (lambda (i) (string-append "P" (number->string i))) (iota n)))
         (doc `(document (body (document ,@pars)) (style "generic")))
         (runs `(document (body (document
                                  (concat ,@(map (lambda (i)
                                                   (if (even? i)
                                                       `(strong ,(number->string i))
                                                       (string-append " " (number->string i) " ")))
                                                 (iota n)))))
                          (style "generic")))
         (body (lambda (t) (cdr (cadr (cadr t))))))
    (for (fm '("docx-document" "odt-document"))
      (check= (length (body (convert (convert doc "texmacs-stree" fm)
                                     fm "texmacs-stree")))
              n)
      (with t (convert (convert runs "texmacs-stree" fm) fm "texmacs-stree")
        (check= (length (cdar (body t))) n)))))

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

(define (bare-office x)
  (cons (car x) (bare (cdr x))))

(define (bare l)
  ;; the blocks l without those of the style office: the file itself, the
  ;; named styles, the headers and footers
  (list-filter l (lambda (b) (not (and (pair? b) (in? (car b) '(package styles header)))))))

(define (docx body)
  ;; the blocks of the office tree of a Word document with this body
  (bare (cdr (parse-docx-document (docx-archive body)))))

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
  (check= (docx (wp (wr "x" "<w:rStyle w:val='Accent'/>")))
          '((p (span (@ (style "Accent")) "x"))))
  ;; (upright by hand in a style of italics: em, which changes italics
  ;; into upright text)
  (check= (docx (wp (wr "x" "<w:rStyle w:val='Accent'/><w:i w:val='0'/>")))
          (quote ((p (span (@ (style "Accent")) (em "x"))))))
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
          (quote ((p (ins "new") (del "old"))))))

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
    (check= (docx (field " PAGE " "3")) (quote ((p (field (@ (code " PAGE ")) "3"))))))
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
          '((p (@ (role "heading") (level "1") (numbered "no")) "Intro")))
  (check= (docx (wp (wstyle "Sous") (wr "Sub")))
          '((p (@ (role "heading") (level "1") (numbered "no")) "Sub")))
  (check= (docx (wp "<w:pPr><w:outlineLvl w:val='2'/></w:pPr>" (wr "Deep")))
          '((p (@ (role "heading") (level "3") (numbered "no")) "Deep")))
  (check= (docx (wp (wstyle "Titre") (wr "T")))
          '((p (@ (role "title") (style "Titre") (align "center")) "T")))
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
            '((p (@ (role "heading") (level "1") (numbered "no")) "H")))))

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
          '(m:math (m:mstyle (@ (scriptlevel "0"))
                     (m:mrow (m:mo (@ (fence "true")) "[") (m:mi "x")))))
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
  (bare (cdr (parse-odt-document (apply odt-archive (cons body formula))))))

(define (tp . l) (string-append "<text:p>" (apply string-append l) "</text:p>"))
(define (tspan style text)
  (string-append "<text:span text:style-name='" style "'>" text "</text:span>"))

(define (test-odt-text)
  (check-group "odt text")
  (check= (odt "") '())
  (check= (odt (tp "plain")) '((p "plain")))
  ;; the formatting is in the styles of the spans, and of their parents
  (check= (odt (tp "a " (tspan "Emphasis" "b") " " (tspan "T1" "c")))
          '((p "a " (span (@ (style "Emphasis")) "b") " " (strong "c"))))
  (check= (odt (tp "x" (tspan "T2" "2")))
          '((p "x" (span (@ (style "Emphasis")) (sup "2")))))
  (check= (odt (tp (tspan "T3" "f()"))) '((p (code "f()"))))
  (check= (odt (tp (tspan "T1" (string-append "a" (tspan "Emphasis" "b")))))
          '((p (strong "a" (span (@ (style "Emphasis")) "b")))))
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
          '((p (@ (role "heading") (level "2") (numbered "no")) "Sub")))
  (check= (odt "<text:p text:style-name='Title'>T</text:p>")
          '((p (@ (role "title") (align "center") (style "Title")) "T")))
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
          '((p "loose " (span (@ (style "Emphasis")) "text")) (p "next")))
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
;; What the readers must not lose, and files which are not as usual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-parts body . parts)
  ;; the blocks of a Word document with this body and these other entries
  ;; of the archive, each (name . text)
  (bare (cdr (parse-docx-document
         (zip-pack (cons "word/document.xml" (map car parts))
                   (cons (string-append "<?xml version='1.0'?><w:document" docx-ns
                                        "><w:body>" body "</w:body></w:document>")
                         (map cdr parts)))))))

(define (odt-parts body automatic . parts)
  ;; the same for an OpenDocument text, with these automatic styles
  (bare (cdr (parse-odt-document
         (zip-pack (cons "content.xml" (map car parts))
                   (cons (string-append "<?xml version='1.0'?><office:document-content"
                                        odt-ns "><office:automatic-styles>" automatic
                                        "</office:automatic-styles><office:body><office:text>"
                                        body "</office:text></office:body>"
                                        "</office:document-content>")
                         (map cdr parts)))))))

(define (in-tm l)
  ;; the paragraphs of TeXmacs for the blocks l of a reader
  (cdr (cadr (cadr (office->texmacs (cons 'office l))))))

(define (test-docx-more-text)
  (check-group "docx more text")
  ;; the runs of a text which says its direction
  (check= (docx (wp (wr "a ") "<w:dir w:val='rtl'>" (wr "b") "</w:dir>"
                    "<w:bdo w:val='ltr'>" (wr "c") "</w:bdo>"))
          '((p "a bc")))
  ;; the characters of a font of symbols: the one of Unicode, a character
  ;; which stands for one which is not known, the code in another font
  (let ((sym (lambda (font char)
               (docx (wp (wr "a ") "<w:r><w:sym w:font='" font "' w:char='" char
                         "'/></w:r>")))))
    (check= (sym "Wingdings" "F0E0") `((p ,(string-append "a " (office-utf8 #x2192)))))
    (check= (sym "Symbol" "F061") `((p ,(string-append "a " (office-utf8 #x3b1)))))
    (check= (sym "Symbol" "F0AE") `((p ,(string-append "a " (office-utf8 #x2192)))))
    (check= (sym "Wingdings" "F0B7") `((p ,(string-append "a " (office-utf8 #xfffd)))))
    (check= (sym "Arial" "0041") '((p "a A")))
    (check= (sym "Arial" "zz") '((p "a "))))
  ;; hidden text is not there; a run of a script which is written from
  ;; right to left is bold by a property of its own
  (check= (docx (wp (wr "a") (wr "hidden" "<w:vanish/>"))) '((p "a")))
  (check= (docx (wp (wr "x" "<w:rtl/><w:b/><w:iCs/>") (wr "y" "<w:bCs/>")))
          '((p (em "x") "y")))
  ;; a field which ends in a later paragraph loses nothing (the text
  ;; before its start and after its end are paragraphs of their own)
  (check= (docx (string-append
                  (wp (wr "Before ") "<w:r><w:fldChar w:fldCharType='begin'/></w:r>"
                      "<w:r><w:instrText> ADDIN ZOTERO_BIBL </w:instrText></w:r>"
                      "<w:r><w:fldChar w:fldCharType='separate'/></w:r>" (wr "One."))
                  (wp (wr "Two.") "<w:r><w:fldChar w:fldCharType='end'/></w:r>"
                      (wr " After"))))
          '((p "Before") (field-blocks (@ (code " ADDIN ZOTERO_BIBL ")) (p "One.") (p "Two."))
            (p "After")))
  (check= (docx (string-append
                  (wp (wr "a ") "<w:r><w:fldChar w:fldCharType='begin'/></w:r>"
                      "<w:r><w:instrText> HYPERLINK \"http://a.b\" </w:instrText></w:r>"
                      "<w:r><w:fldChar w:fldCharType='separate'/></w:r>" (wr "one"))
                  (wp (wr "two") "<w:r><w:fldChar w:fldCharType='end'/></w:r>")
                  (wp (wr "x"))))
          '((p "a " (link (@ (href "http://a.b")) "one"))
            (p (link (@ (href "http://a.b")) "two"))
            (p "x")))
  ;; the instructions of the fields: an address with a space, a bookmark
  ;; in a file, a bookmark with a space, the page of a bookmark
  (let ((field (lambda (instr)
                 (docx (wp "<w:fldSimple w:instr='" instr "'>" (wr "t")
                           "</w:fldSimple>")))))
    (check= (field " HYPERLINK \"http://x.org/a b.html\" \\o \"tip\" ")
            '((p (link (@ (href "http://x.org/a b.html")) "t"))))
    (check= (field " HYPERLINK \"other.docx\" \\l \"sec2\" ")
            '((p (link (@ (href "other.docx#sec2")) "t"))))
    (check= (field " HYPERLINK \\l \"My mark\" ")
            '((p (link (@ (href "#My mark")) "t"))))
    (check= (field " REF fig1 \\r \\h ") '((p (ref (@ (name "fig1")) "t"))))
    (check= (field " PAGEREF _Ref1 \\h ")
            '((p (ref (@ (name "_Ref1") (kind "page")) "t")))))
  ;; a note which refers to itself is read once
  (check= (docx-parts
            (wp (wr "a") "<w:r><w:footnoteReference w:id='1'/></w:r>")
            (cons "word/footnotes.xml"
                  (string-append
                    "<w:footnotes" docx-ns "><w:footnote w:id='1'>"
                    (wp (wr "n") "<w:r><w:footnoteReference w:id='1'/></w:r>")
                    "</w:footnote></w:footnotes>")))
          '((p "a" (note (p "n")))))
  ;; the picture of the fallback when the choice gives nothing, and all
  ;; the pictures of a group
  (let ((rels (cons "word/_rels/document.xml.rels"
                    (string-append
                      "<Relationships><Relationship Id='rId1' Target='media/a.png'/>"
                      "<Relationship Id='rId2' Target='media/b.png'/></Relationships>"))))
    (check= (docx-parts
              (wp "<w:r><mc:AlternateContent><mc:Choice Requires='cx'><w:drawing>"
                  "<cx:chart r:id='rId5'/></w:drawing></mc:Choice><mc:Fallback>"
                  "<w:drawing><a:blip r:embed='rId1'/></w:drawing></mc:Fallback>"
                  "</mc:AlternateContent></w:r>")
              rels (cons "word/media/a.png" "A"))
            '((p (image (@ (name "a.png") (data "A"))))))
    (check= (docx-parts
              (wp "<w:r><w:drawing><wp:extent cx='360000' cy='360000'/><wpg:wgp>"
                  "<a:blip r:embed='rId1'/><a:blip r:embed='rId2'/></wpg:wgp>"
                  "</w:drawing></w:r>")
              rels (cons "word/media/a.png" "A") (cons "word/media/b.png" "B"))
            '((p (image (@ (name "a.png") (data "A")))
                 (image (@ (name "b.png") (data "B"))))))))

(define (test-docx-more-blocks)
  (check-group "docx more blocks")
  (let ((item (lambda (id text)
                (wp "<w:pPr><w:numPr><w:ilvl w:val='0'/><w:numId w:val='" id
                    "'/></w:numPr></w:pPr>" (wr text))))
        (numbering (lambda (s)
                     (cons "word/numbering.xml"
                           (string-append "<w:numbering" docx-ns ">" s
                                          "</w:numbering>")))))
    ;; a level without a mark: one more paragraph of the item before, and
    ;; the list goes on after it
    (check= (docx-parts
              (string-append (item "2" "one") (item "1" "more") (item "2" "two"))
              (numbering
                (string-append
                  "<w:abstractNum w:abstractNumId='0'><w:lvl w:ilvl='0'>"
                  "<w:numFmt w:val='bullet'/><w:lvlText w:val=' '/></w:lvl></w:abstractNum>"
                  "<w:abstractNum w:abstractNumId='1'><w:lvl w:ilvl='0'>"
                  "<w:numFmt w:val='decimal'/><w:lvlText w:val='%1.'/></w:lvl></w:abstractNum>"
                  "<w:num w:numId='1'><w:abstractNumId w:val='0'/></w:num>"
                  "<w:num w:numId='2'><w:abstractNumId w:val='1'/></w:num>")))
            '((list (@ (kind "number")) (item (p "one") (p "more")) (item (p "two")))))
    ;; a list whose levels are those of a style of lists
    (check= (docx-parts
              (item "1" "one")
              (numbering
                (string-append
                  "<w:abstractNum w:abstractNumId='0'><w:lvl w:ilvl='0'>"
                  "<w:numFmt w:val='decimal'/></w:lvl></w:abstractNum>"
                  "<w:abstractNum w:abstractNumId='1'><w:numStyleLink w:val='L'/>"
                  "</w:abstractNum>"
                  "<w:num w:numId='1'><w:abstractNumId w:val='1'/></w:num>"
                  "<w:num w:numId='2'><w:abstractNumId w:val='0'/></w:num>"))
              (cons "word/styles.xml"
                    (string-append
                      "<w:styles" docx-ns "><w:style w:type='numbering' w:styleId='L'>"
                      "<w:pPr><w:numPr><w:numId w:val='2'/></w:numPr></w:pPr>"
                      "</w:style></w:styles>")))
            '((list (@ (kind "number")) (item (p "one"))))))
  (let ((tc (lambda (props text)
              (string-append "<w:tc><w:tcPr>" props "</w:tcPr>" (wp (wr text)) "</w:tc>")))
        (plain (lambda (text) `(cell (@ (borders "none")) (p ,text)))))
    ;; the rows and the cells inside a content control
    (check= (docx (string-append
                    "<w:tbl><w:tr>" (tc "" "a") "<w:sdt><w:sdtContent>" (tc "" "b")
                    "</w:sdtContent></w:sdt></w:tr><w:sdt><w:sdtContent><w:tr>"
                    (tc "" "c") (tc "" "d") "</w:tr></w:sdtContent></w:sdt></w:tbl>"))
            `((table (row ,(plain "a") ,(plain "b")) (row ,(plain "c") ,(plain "d")))))
    ;; a row which starts after the first column
    (check= (docx (string-append
                    "<w:tbl><w:tr>" (tc "" "a") (tc "" "b") "</w:tr>"
                    "<w:tr><w:trPr><w:gridBefore w:val='1'/></w:trPr>" (tc "" "c")
                    "</w:tr></w:tbl>"))
            `((table (row ,(plain "a") ,(plain "b"))
                     (row (cell (@ (borders "none"))) ,(plain "c")))))
    ;; a cell which goes on a merge that no cell starts keeps its text
    (check= (docx (string-append
                    "<w:tbl><w:tr>" (tc "" "a") "</w:tr><w:tr>" (tc "<w:vMerge/>" "b")
                    "</w:tr><w:tr>" (tc "<w:vMerge/>" "") "</w:tr></w:tbl>"))
            `((table (row ,(plain "a"))
                     (row (cell (@ (rowspan "2") (borders "none")) (p "b")))
                     (row (cell (@ (covered "true")))))))
    ;; a row which was deleted is not there
    (check= (docx (string-append
                    "<w:tbl><w:tr><w:trPr><w:del w:id='1'/></w:trPr>" (tc "" "gone")
                    "</w:tr><w:tr>" (tc "" "kept") "</w:tr></w:tbl>"))
            `((table (row ,(plain "kept")))))
    ;; numbers which are not those of a table: no error, no huge table
    (check= (docx (string-append "<w:tbl><w:tr>" (tc "<w:gridSpan w:val='0'/>" "x")
                                 "</w:tr></w:tbl>"))
            `((table (row ,(plain "x")))))
    (check= (length (cdr (cadr (car (docx (string-append
                                            "<w:tbl><w:tr>"
                                            (tc "<w:gridSpan w:val='30000000'/>" "x")
                                            "</w:tr></w:tbl>"))))))
            63)
    (check= (docx (string-append "<w:tbl><w:tblPr><w:tblLook w:val='1.5'/></w:tblPr><w:tr>"
                                 (tc "" "x") "</w:tr></w:tbl>"))
            `((table (row ,(plain "x"))))))
  (check= (docx (wp "<w:pPr><w:outlineLvl w:val='1.4'/></w:pPr>" (wr "H")))
          '((p (@ (role "heading") (level "2") (numbered "no")) "H")))
  (check= (docx (wp "<w:pPr><w:ind w:left='720.5'/></w:pPr>" (wr "x")))
          '((p (@ (indent-left "36.025")) "x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word documents which are not as usual: what the reader must not lose
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-docx-unusual)
  (check-group "docx unusual")
  (let ((fc (lambda (type)
              (string-append "<w:r><w:fldChar w:fldCharType='" type "'/></w:r>")))
        (instr (lambda (s)
              (string-append "<w:r><w:instrText xml:space='preserve'>" s
                             "</w:instrText></w:r>")))
        (item (lambda (id level text)
                (wp "<w:pPr><w:numPr><w:ilvl w:val='" level "'/><w:numId w:val='"
                    id "'/></w:numPr></w:pPr>" (wr text))))
        (rels (lambda (s)
                (cons "word/_rels/document.xml.rels"
                      (string-append "<Relationships>" s "</Relationships>"))))
        (pic (lambda (id)
               (string-append "<w:drawing><wp:inline><wp:extent cx='360000' cy='360000'/>"
                              "<a:graphic><a:graphicData><a:blip r:embed='" id "'/>"
                              "</a:graphicData></a:graphic></wp:inline></w:drawing>"))))
    ;; the text of a formula which was inserted with the changes tracked is
    ;; there, the one which was deleted is not
    (check= (omml (string-append (mr "x") "<w:ins w:id='1'>" (mr "+y") "</w:ins>"
                                 "<w:del w:id='2'>" (mr "z") "</w:del>"))
            '(m:math (m:mrow (m:mi "x") (m:mo "+") (m:mi "y"))))
    ;; a table of figures is no table of contents (LibreOffice writes \f
    ;; with \o for a table of contents)
    (check= (docx (wp (fc "begin") (instr " TOC \\h \\z \\c \"Figure\" ") (fc "separate")
                      (wr "Figure 1") (fc "end")))
            '((p "Figure 1")))
    (check= (docx (wp (fc "begin") (instr " TOC \\f \\o \"1-3\" \\h ") (fc "separate")
                      (wr "Intro") (fc "end")))
            '((toc)))
    ;; an element which is on or off, and says off
    (check= (docx (string-append (wp (wr "a"))
                                 (wp "<w:pPr><w:pageBreakBefore w:val='0'/></w:pPr>"
                                     (wr "b"))))
            '((p "a") (p "b")))
    (check= (map (lambda (h) (ox-attr h 'kind))
                 (list-filter
                   (cdr (parse-docx-document
                          (zip-pack
                            '("word/document.xml" "word/_rels/document.xml.rels"
                              "word/h1.xml" "word/h2.xml")
                            (list (string-append
                                    "<w:document" docx-ns "><w:body>" (wp (wr "x"))
                                    "<w:sectPr><w:headerReference w:type='first' r:id='h1'/>"
                                    "<w:headerReference w:type='default' r:id='h2'/>"
                                    "<w:titlePg w:val='false'/></w:sectPr>"
                                    "</w:body></w:document>")
                                  (string-append
                                    "<Relationships><Relationship Id='h1' Target='h1.xml'/>"
                                    "<Relationship Id='h2' Target='h2.xml'/></Relationships>")
                                  (string-append "<w:hdr" docx-ns ">" (wp (wr "F")) "</w:hdr>")
                                  (string-append "<w:hdr" docx-ns ">" (wp (wr "D")) "</w:hdr>")))))
                   (lambda (b) (func? b 'header))))
            '("default"))
    ;; a text with its reading above it: the text
    (check= (docx (wp (wr "A") "<w:r><w:ruby><w:rubyPr/><w:rt><w:r><w:t>kan</w:t></w:r>"
                      "</w:rt><w:rubyBase><w:r><w:t>B</w:t></w:r></w:rubyBase></w:ruby></w:r>"
                      (wr "C")))
            '((p "ABC")))
    ;; the lists of Word with the same abstract list go on after the text
    ;; between them, and so do the deeper levels, until a higher item
    (check= (docx (string-append (item "1" "1" "a") (item "1" "1" "b") (wp (wr "x"))
                                 (item "2" "1" "c")))
            '((list (@ (kind "number")) (item (p "a")) (item (p "b")))
              (p "x")
              (list (@ (kind "number") (start "3")) (item (p "c")))))
    (check= (docx (string-append (item "1" "0" "A") (item "1" "1" "a") (wp (wr "x"))
                                 (item "1" "1" "b") (item "1" "0" "B") (item "1" "1" "c")))
            '((list (@ (kind "bullet")) (item (p "A") (list (@ (kind "number")) (item (p "a")))))
              (p "x")
              (list (@ (kind "number") (start "2")) (item (p "b")))
              (list (@ (kind "bullet")) (item (p "B") (list (@ (kind "number")) (item (p "c")))))))
    ;; a field which does not end takes no text after it; one which ends
    ;; in the next paragraph still does
    (check= (docx (string-append (wp (wr "A ") (fc "begin") (instr " HYPERLINK \"http://a\" ")
                                     (wr "B"))
                                 (wp (wr "next"))))
            '((p "A B") (p "next")))
    (check= (docx (string-append (wp (fc "begin") (instr " TOC \\o ") (fc "separate") (wr "e1"))
                                 (wp (wr "after"))))
            '((p "e1") (p "after")))
    (check= (docx (string-append (wp (fc "begin") (instr " HYPERLINK \"http://a\" ")
                                     (fc "separate") (wr "one"))
                                 (wp (wr "two") (fc "end") (wr " three"))))
            '((p (link (@ (href "http://a")) "one"))
              (p (link (@ (href "http://a")) "two") " three")))
    (check= (docx (wp (fc "begin") (instr " REF bm ") "<w:hyperlink w:anchor='bm'>"
                      (fc "separate") (wr "text") (fc "end") "</w:hyperlink>" (wr " tail")))
            '((p (link (@ (href "#bm")) "text") " tail")))
    ;; the names of the entries of the archive are addresses, with %20
    (check= (ox-attr (cadr (car (docx-parts
                                  (wp (string-append "<w:r>" (pic "rId1") "</w:r>"))
                                  (rels "<Relationship Id='rId1' Target='media/my%20pic.png'/>")
                                  (cons "word/media/my pic.png" "PNG"))))
                     'name)
            "my pic.png")
    ;; the styles are the entry which the relations name
    (check= (docx-parts (wp "<w:pPr><w:pStyle w:val='H'/></w:pPr>" (wr "x"))
                        (rels (string-append
                                "<Relationship Id='s' Target='styles2.xml' Type='"
                                "http://schemas.openxmlformats.org/officeDocument/2006/"
                                "relationships/styles'/>"))
                        (cons "word/styles2.xml"
                              (string-append "<w:styles" docx-ns ">"
                                             "<w:style w:type='paragraph' w:styleId='H'>"
                                             "<w:name w:val='heading 1'/></w:style>"
                                             "</w:styles>")))
            '((p (@ (role "heading") (level "1") (numbered "no")) "x")))
    ;; choices of the contents, around runs and around paragraphs: the
    ;; first one which is read, or the fallback
    (let ((alt (lambda (req choice fallback)
                 (string-append
                   "<mc:AlternateContent xmlns:mc='http://schemas.openxmlformats.org/"
                   "markup-compatibility/2006'><mc:Choice Requires='" req "'>" choice
                   "</mc:Choice><mc:Fallback>" fallback "</mc:Fallback>"
                   "</mc:AlternateContent>"))))
      (check= (docx (wp (wr "a") (alt "w14" (wr "C") (wr "F"))))
              '((p "aC")))
      (check= (docx (alt "cx" (wp (wr "C")) (wp (wr "F"))))
              '((p "F"))))
    ;; a picture which is linked, and one in an object
    (check= (docx (wp (string-append "<w:r>" (pic "rId1") "</w:r>")))
            '((p (image (@ (name "https://www.texmacs.org") (width "1cm") (height "1cm"))))))
    (check= (docx (wp (string-append "<w:r><w:object>" (pic "rId2") "</w:object></w:r>")))
            '((p (image (@ (name "pic.png") (data "PNGDATA") (width "1cm") (height "1cm"))))))
    ;; a bar is below its formula when it does not say where; a phantom
    ;; is not shown; a run of Word in a formula gives its text only
    (check= (omml (string-append "<m:bar><m:e>" (mr "x") "</m:e></m:bar>"))
            '(m:math (m:munder (m:mi "x") (m:mo "_"))))
    (check= (omml (string-append "<m:phant><m:phantPr><m:show m:val='0'/></m:phantPr>"
                                 "<m:e>" (mr "x") "</m:e></m:phant>"))
            '(m:math (m:mphantom (m:mi "x"))))
    (check= (omml "<w:r> <w:rPr><w:b/></w:rPr> <w:t>T</w:t> </w:r>")
            '(m:math (m:mtext "T")))
    ;; a bookmark before a table is in its first cell; one after the last
    ;; paragraph is at its end
    (check= (docx (string-append "<w:bookmarkStart w:id='1' w:name='t'/>"
                                 "<w:tbl><w:tr><w:tc>" (wp (wr "c")) "</w:tc></w:tr></w:tbl>"
                                 (wp (wr "a")) "<w:bookmarkStart w:id='2' w:name='e'/>"))
            '((table (row (cell (@ (borders "none")) (p (bookmark (@ (name "t"))) "c"))))
              (p "a" (bookmark (@ (name "e"))))))
    ;; a picture which is linked (r:link) is an image of its address, which
    ;; is written back as a relation to it, and never fetched
    (check= (docx (wp "<w:r><w:drawing><wp:inline><wp:extent cx='720000' cy='360000'/>"
                      "<a:graphic><a:graphicData><a:blip r:link='rId1'/></a:graphicData>"
                      "</a:graphic></wp:inline></w:drawing></w:r>"))
            '((p (image (@ (name "https://www.texmacs.org") (width "2cm") (height "1cm"))))))
    (check= (in-tm '((p (image (@ (name "https://www.texmacs.org") (width "2cm")
                                  (height "1cm"))))))
            '((image "https://www.texmacs.org" "2cm" "1cm" "" "")))
    (let* ((z (convert '(document (image "https://www.texmacs.org/a.png" "2cm" "1cm" "" ""))
                       "texmacs-stree" "docx-document"))
           (rels (export-entry z "word/_rels/document.xml.rels")))
      (check-true (export-has? rels "Target=\"https://www.texmacs.org/a.png\""))
      (check-true (export-has? rels "TargetMode=\"External\""))
      (check-true (export-has? (export-entry z "word/document.xml") "r:link="))
      (check= (list-filter (bare (cdr (parse-docx-document z))) (lambda (b) (func? b 'p)))
              '((p (image (@ (name "https://www.texmacs.org/a.png") (width "2cm")
                             (height "1cm")))))))
    (check-true (export-has? (export-entry (serialize-odt-document
                                             '(office (p (image (@ (name "https://www.texmacs.org/a.png"))))))
                                           "content.xml")
                             "xlink:href=\"https://www.texmacs.org/a.png\""))
    ;; the bibliography of Zotero over several paragraphs stays a field,
    ;; around the same paragraphs when it is written again
    (let* ((bib (string-append
                  (wp (fc "begin") (instr " ADDIN ZOTERO_BIBL {} CSL_BIBLIOGRAPHY ")
                      (fc "separate") (wr "Doe, J."))
                  (wp (wr "Roe, R.") (fc "end"))
                  (wp (wr "After."))))
           (blocks '((field-blocks (@ (code " ADDIN ZOTERO_BIBL {} CSL_BIBLIOGRAPHY "))
                                   (p "Doe, J.") (p "Roe, R."))
                     (p "After.")))
           (t (convert (docx-archive bib) "docx-document" "texmacs-stree"))
           (z (convert t "texmacs-stree" "docx-document")))
      (check= (docx bib) blocks)
      ;; (in the style office: a field around the paragraphs)
      (check= (list-filter (cdr (cadr (cadr t))) (lambda (b) (func? b 'office-field)))
              '((office-field " ADDIN ZOTERO_BIBL {} CSL_BIBLIOGRAPHY "
                              (document "Doe, J." "Roe, R."))))
      (check= (list-filter (bare (cdr (parse-docx-document z)))
                           (lambda (b) (in? (car b) '(p field-blocks))))
              blocks))
    ;; a comment in a note is in the note
    (check= (docx-parts (wp (wr "x") "<w:r><w:footnoteReference w:id='1'/></w:r>")
                        (cons "word/footnotes.xml"
                              (string-append "<w:footnotes" docx-ns "><w:footnote w:id='1'>"
                                             (wp (wr "note") "<w:r><w:commentReference w:id='0'/></w:r>")
                                             "</w:footnote></w:footnotes>"))
                        (cons "word/comments.xml"
                              (string-append "<w:comments" docx-ns ">"
                                             "<w:comment w:id='0' w:author='Ann'>" (wp (wr "remark"))
                                             "</w:comment></w:comments>")))
            '((p "x" (note (p "note" (comment (@ (author "Ann")) (p "remark")))))))
    ;; the spaces between the lines and the paragraph kept with the next
    ;; one of a named style, which its definition writes again
    (let* ((z (zip-pack '("word/document.xml" "word/styles.xml")
                        (list (string-append "<w:document" docx-ns "><w:body>"
                                             (wp (wstyle "K") (wr "x")) (wp (wr "y")) (wp (wr "z"))
                                             "</w:body></w:document>")
                              (string-append "<w:styles" docx-ns ">"
                                             "<w:style w:type='paragraph' w:styleId='K'>"
                                             "<w:name w:val='Keep'/><w:pPr><w:keepNext/>"
                                             "<w:spacing w:line='360' w:lineRule='auto'/></w:pPr>"
                                             "</w:style></w:styles>"))))
           (styles (list-filter (cdr (parse-docx-document z))
                                (lambda (b) (func? b 'styles))))
           (all (export-entry (serialize-docx-document
                                `(office ,@styles (p (@ (style "K")) "x")))
                              "word/styles.xml"))
           ;; (the definition of the style K)
           (i (string-search-forwards "w:styleId=\"K\"" 0 all))
           (out (if (< i 0) ""
                    (substring all i (max i (string-search-forwards "</w:style>" i all))))))
      (check= styles
              '((styles (style (@ (name "K") (display "Keep") (family "paragraph")
                                  (line-spacing "1.5") (keep-with-next "true"))))))
      (check-true (export-has? out "<w:keepNext/>"))
      (check-true (export-has? out "w:line=\"360\"")))))

(define (test-odt-more)
  (check-group "odt more")
  ;; a paragraph which is bold as a whole, by its automatic style
  (check= (odt-parts
            "<text:p text:style-name='P9'>all <text:span>bold</text:span></text:p>"
            (string-append "<style:style style:name='P9' style:family='paragraph'>"
                           "<style:text-properties fo:font-weight='bold'/></style:style>"))
          '((p (strong "all bold"))))
  ;; the bibliography, the other indexes, a paragraph with a number
  (check= (odt (string-append
                 "<text:bibliography><text:index-body><text:index-title>" (tp "Refs")
                 "</text:index-title>" (tp "[1] A.") "</text:index-body></text:bibliography>"
                 "<text:alphabetical-index><text:index-body>" (tp "apple 3")
                 "</text:index-body></text:alphabetical-index>"
                 "<text:numbered-paragraph>" (tp "n") "</text:numbered-paragraph>"))
          '((p "Refs") (p "[1] A.") (p "apple 3") (p "n")))
  ;; an object which is no formula is its picture, not an equation (a
  ;; picture without a suffix gets the one of its format)
  (check= (odt-parts
            (tp "<draw:frame><draw:object xlink:href='./Object 1'/>"
                "<draw:image xlink:href='./ObjectReplacements/Object 1'/></draw:frame>")
            ""
            (cons "Object 1/content.xml"
                  (string-append "<office:document-content" odt-ns "><office:body>"
                                 "<office:chart>" (tp "Sales") "</office:chart>"
                                 "</office:body></office:document-content>"))
            (cons "ObjectReplacements/Object 1" (string-append (bytes 137) "PNGDATA")))
          `((p (image (@ (name "Object 1.png")
                         (data ,(string-append (bytes 137) "PNGDATA")))))))
  ;; numbers which are not those of a text: no error, no huge text
  (check= (odt (tp "a<text:s text:c='-1'/>b")) '((p "ab")))
  (check= (odt (tp "a<text:s text:c='1.4'/>b")) '((p "a b")))
  (check= (string-length (cadr (car (odt (tp "a<text:s text:c='30000000'/>b"))))) 1002)
  (check= (odt (string-append
                 "<table:table><table:table-row>"
                 "<table:table-cell table:number-columns-repeated='-1'"
                 " table:number-columns-spanned='x'>" (tp "c")
                 "</table:table-cell></table:table-row></table:table>"))
          '((table (row (cell (@ (borders "none")) (p "c"))))))
  (check= (odt "<text:h text:outline-level='2.4'>H</text:h>")
          '((p (@ (role "heading") (level "2") (numbered "no")) "H")))
  ;; the list around a table is not the one of its cells
  (check= (odt (string-append
                 "<text:list text:style-name='L1'><text:list-item><table:table>"
                 "<table:table-row><table:table-cell><text:list text:style-name='L1'>"
                 "<text:list-item><text:list><text:list-item>" (tp "x")
                 "</text:list-item></text:list></text:list-item></text:list>"
                 "</table:table-cell></table:table-row></table:table>"
                 "</text:list-item></text:list>"))
          '((list (@ (kind "bullet"))
                  (item (table (row (cell (@ (borders "none"))
                                          (list (@ (kind "bullet"))
                                                (item (@ (unnumbered "true"))
                                                      (list (@ (kind "number"))
                                                            (item (p "x"))))))))))))
  ;; the widths of the columns which are in a group
  (check= (odt (string-append
                 "<table:table><table:table-columns>"
                 "<table:table-column table:style-name='ColA'/>"
                 "<table:table-column table:style-name='ColB'/></table:table-columns>"
                 "<table:table-row><table:table-cell>" (tp "1") "</table:table-cell>"
                 "<table:table-cell>" (tp "2") "</table:table-cell>"
                 "</table:table-row></table:table>"))
          '((table (@ (columns "0.25 0.75"))
                   (row (cell (@ (borders "none")) (p "1"))
                        (cell (@ (borders "none")) (p "2"))))))
  ;; a table in a box of text is in the box
  (check= (odt (tp "a<draw:frame><draw:text-box><table:table><table:table-row>"
                   "<table:table-cell>" (tp "in") "</table:table-cell>"
                   "</table:table-row></table:table>" (tp "b")
                   "</draw:text-box></draw:frame>"))
          '((p "a" (box (@ (shape "rect") (fill "none") (line "none"))
                       (table (row (cell (@ (borders "none")) (p "in")))) (p "b")))))
  ;; an image which is not in the archive is not one to fetch: as the
  ;; Word reader says it, an image of its address on the web, and else
  ;; one which is linked, whose address is text
  (check= (odt (tp "<draw:frame><draw:image xlink:href='http://a.b/p.png'/>"
                   "<svg:title>A picture</svg:title></draw:frame>"))
          '((p (image (@ (name "http://a.b/p.png") (alt "A picture"))))))
  (with b (odt (tp "x <draw:frame><draw:image xlink:href='file:///etc/passwd'/>"
                   "</draw:frame>"))
    (check= b '((p "x " (image (@ (name "file:///etc/passwd") (linked "true"))))))
    (check= (in-tm b) '("x file:///etc/passwd")))
  ;; (an image of the office tree with an address on the web, as Word
  ;; links to one, is an image of this address, which is not fetched)
  (check= (in-tm '((p (image (@ (name "http://a.b/p.png"))))))
          '((image "http://a.b/p.png" "" "" "" ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpenDocument import: regressions of the audit of odtin and officetm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tp* style . l)
  (string-append "<text:p text:style-name='" style "'>" (apply string-append l)
                 "</text:p>"))

(define (odt-audit-initial r)
  ;; the associations of the initial environment of the document r
  (with init (list-find (cdr r) (lambda (y) (func? y 'initial)))
    (if init (cdadr init) '())))

(define (odt-audit-header doc)
  ;; the header of the office tree doc
  (list-find (cdr doc) (lambda (y) (func? y 'header))))

(define (test-odt-audit)
  (check-group "odt import audit")
  ;; a reference to the page of a label is one, also to a sequence; one
  ;; to the number of its chapter is its text
  (with blocks (odt (string-append
                      "<text:h text:outline-level='1'><text:bookmark text:name='s'/>"
                      "I</text:h>"
                      (tp "<text:bookmark-ref text:reference-format='page'"
                          " text:ref-name='s'>1</text:bookmark-ref> "
                          "<text:sequence-ref text:reference-format='page'"
                          " text:ref-name='s'>1</text:sequence-ref> "
                          "<text:bookmark-ref text:reference-format='chapter'"
                          " text:ref-name='s'>1</text:bookmark-ref>")))
    (check= (cadr blocks)
            '(p (ref (@ (name "s") (kind "page")) "1") " "
                (ref (@ (name "s") (kind "page")) "1") " "
                (ref (@ (name "s") (kind "text")) "1")))
    (check= (cadr (in-tm blocks))
            '(concat (pageref "s") " " (pageref "s") " " (hlink "1" "#s"))))
  ;; a span which is not bold in a bold paragraph is not
  (check= (odt-parts
            (tp* "P9" "bold <text:span text:style-name='T9'>normal</text:span> bold")
            (string-append "<style:style style:name='P9' style:family='paragraph'>"
                           "<style:text-properties fo:font-weight='bold'"
                           " fo:color='#ff0000'/></style:style>"
                           "<style:style style:name='T9' style:family='text'>"
                           "<style:text-properties fo:font-weight='normal'"
                           " fo:color='#000000'/></style:style>"))
          '((p (strong (color (@ (value "#ff0000")) "bold ")) "normal"
               (strong (color (@ (value "#ff0000")) " bold")))))
  ;; a row which is there several times, but not a million empty rows
  (with row (string-append "<table:table-row table:number-rows-repeated='3'>"
                           "<table:table-cell>" (tp "a") "</table:table-cell>"
                           "</table:table-row>")
    (check= (length (cdar (odt (string-append "<table:table>" row "</table:table>")))) 3))
  (check= (length (cdar (odt (string-append
                               "<table:table><table:table-row><table:table-cell>" (tp "a")
                               "</table:table-cell></table:table-row>"
                               "<table:table-row table:number-rows-repeated='1048576'>"
                               "<table:table-cell/></table:table-row></table:table>"))))
          2)
  ;; lists which go on at a deeper level, one after another: in a time
  ;; which grows as their number (4000 of them took a minute before)
  (let* ((one (string-append "<text:list><text:list-item><text:list><text:list-item>"
                             (tp "n") "</text:list-item></text:list></text:list-item>"
                             "</text:list>"))
         (body (string-append "<text:list><text:list-item>" (tp "top")
                              "</text:list-item></text:list>"
                              (apply string-append (map (lambda (i) one) (iota 3000)))))
         (t0 (texmacs-time))
         (blocks (odt body)))
    (check= (length blocks) 1)
    (check= (length (ox-children (caddr (car blocks)))) 3001)
    (check-true (< (- (texmacs-time) t0) 20000)))
  ;; a header of a list, an item which holds only a list: no number
  (with blocks (odt (string-append
                      "<text:list text:style-name='L1'><text:list-header>" (tp "H")
                      "</text:list-header><text:list-item><text:list><text:list-item>"
                      (tp "x") "</text:list-item></text:list></text:list-item>"
                      "<text:list-item>" (tp "top") "</text:list-item></text:list>"))
    (check= blocks
            '((list (@ (kind "bullet"))
                    (item (@ (unnumbered "true")) (p "H"))
                    (item (@ (unnumbered "true"))
                          (list (@ (kind "number")) (item (p "x"))))
                    (item (p "top")))))
    (check= (in-tm blocks)
            '((itemize (document "H" (enumerate (document (concat (item) "x")))
                                 (concat (item) "top"))))))
  ;; an item which starts at another number, anywhere in the list; an item
  ;; after one which has no number has the number after it
  (with blocks (odt (string-append
                      "<text:list text:style-name='L1'><text:list-item><text:list>"
                      "<text:list-item>" (tp "a") "</text:list-item>"
                      "<text:list-item text:start-value='7'>" (tp "b") "</text:list-item>"
                      "</text:list></text:list-item></text:list>"))
    (check= blocks
            '((list (@ (kind "bullet"))
                    (item (@ (unnumbered "true"))
                          (list (@ (kind "number"))
                                (item (p "a")) (item (@ (start "7")) (p "b")))))))
    (check= (in-tm (ox-children (caddr (car blocks))))
            '((enumerate (document (concat (item) "a")
                                   (concat (assign "item-nr" "6") (item) "b"))))))
  ;; the lists of a cell do not count in the numbers of the text
  (with blocks (odt-parts
                 (string-append
                   "<text:list text:style-name='N3'><text:list-item>" (tp "a")
                   "</text:list-item></text:list>"
                   "<table:table><table:table-row><table:table-cell>"
                   "<text:list text:style-name='N3'><text:list-item>" (tp "c1")
                   "</text:list-item><text:list-item>" (tp "c2") "</text:list-item>"
                   "</text:list></table:table-cell></table:table-row></table:table>"
                   "<text:list text:style-name='N3' text:continue-numbering='true'>"
                   "<text:list-item>" (tp "b") "</text:list-item></text:list>")
                 (string-append "<text:list-style style:name='N3'>"
                                "<text:list-level-style-number text:level='1'"
                                " style:num-format='1'/></text:list-style>"))
    (check= (caddr blocks) '(list (@ (kind "number") (start "2")) (item (p "b")))))
  ;; a length of 0 is one when it may be (a margin), and picas are lengths
  (check= (office-length->cm "0cm") #f)
  (check-true (= (office-length->cm "0cm" #t) 0))
  (check-true (< (abs (- (office-length->cm "6pc") 2.54)) 0.001))
  (with doc (parse-odt-document
              (odt-file (tp "x")
                        (string-append
                          "<office:automatic-styles><style:page-layout style:name='pm1'>"
                          "<style:page-layout-properties fo:page-width='21cm'"
                          " fo:page-height='29.7cm' fo:margin-left='0cm'"
                          " fo:margin-right='2pc'/></style:page-layout>"
                          "</office:automatic-styles><office:master-styles>"
                          "<style:master-page style:name='Standard'"
                          " style:page-layout-name='pm1'/></office:master-styles>")
                        ""))
    (with setup (list-find (cdr doc) (lambda (y) (func? y 'setup)))
      (check= (ox-attr setup 'margin-left) "0cm")
      (check= (ox-attr setup 'margin-right) "0.847cm"))
    (with env (odt-audit-initial (office->texmacs doc))
      (check-true (in? '(associate "page-odd" "0cm") env))))
  ;; the spaces on both sides of the start of a span are one; none at the
  ;; ends of a paragraph, also inside a span
  (check= (odt (tp "[a " (tspan "T1" "  b") "]")) '((p "[a " (strong "b") "]")))
  (check= (odt (tp (tspan "T1" " x ") " ")) '((p (strong "x"))))
  ;; the automatic styles of styles.xml are those of the headers, those
  ;; of content.xml of the text, also when they have the same names
  (with doc (parse-odt-document
              (odt-file (tp* "P1" "body")
                        (string-append
                          "<office:automatic-styles>"
                          "<style:style style:name='P1' style:family='paragraph'>"
                          "<style:paragraph-properties fo:text-align='center'/>"
                          "</style:style></office:automatic-styles>"
                          "<office:master-styles><style:master-page style:name='Standard'>"
                          "<style:header>" (tp* "P1" "Head") "</style:header>"
                          "</style:master-page></office:master-styles>")
                        (string-append
                          "<style:style style:name='P1' style:family='paragraph'>"
                          "<style:text-properties fo:font-style='italic'/></style:style>")))
    (check= (odt-audit-header doc)
            '(header (@ (place "header") (kind "default")) (p (@ (align "center")) "Head")))
    (check= (cAr doc) '(p (em "body"))))
  ;; a formula of MathML with a prefix, in its frame
  (with blocks (odt (tp "see <draw:frame text:anchor-type='as-char'><draw:object>"
                        "<math:math xmlns:math='http://www.w3.org/1998/Math/MathML'>"
                        "<math:mi>x</math:mi></math:math></draw:object></draw:frame>"))
    (check= (in-tm blocks) '((concat "see " (math "x")))))
  ;; a page break after a paragraph
  (check= (odt-parts (string-append (tp* "PB" "a") (tp "b"))
                     (string-append "<style:style style:name='PB' style:family='paragraph'>"
                                    "<style:paragraph-properties fo:break-after='page'/>"
                                    "</style:style>"))
          '((p "a") (pagebreak) (p "b")))
  ;; a heading or a line of code alone in a cell
  (check= (in-tm '((table (row (cell (p (@ (role "heading") (level "2")) "H"))
                               (cell (p (@ (role "code")) "x = 1"))))))
          '((block (tformat (table (row (cell (strong "H")) (cell (verbatim "x = 1"))))))))
  ;; the picture of a chart which TeXmacs cannot show (StarView, without a
  ;; suffix) is not shown, but its text is kept
  (check= (odt-parts
            (tp "<draw:frame><draw:object xlink:href='./Object 1'/>"
                "<draw:image xlink:href='./ObjectReplacements/Object 1'/>"
                "<svg:title>Sales chart</svg:title></draw:frame>")
            ""
            (cons "Object 1/content.xml"
                  (string-append "<office:document-content" odt-ns "><office:body>"
                                 "<office:chart/></office:body></office:document-content>"))
            (cons "ObjectReplacements/Object 1" "VCLMTF..."))
          '((p "Sales chart")))
  ;; a file read after another one is read as if it were the first
  (with f (lambda ()
            (odt (string-append
                   (tp "a<text:note><text:note-body>" (tp "n")
                       "</text:note-body></text:note>")
                   "<text:list text:style-name='L1' text:continue-numbering='true'>"
                   "<text:list-item>" (tp "i") "</text:list-item></text:list>")))
    (with first (f)
      (check= (f) first)))
  ;; a displayed formula in a paragraph with text: paragraphs of their
  ;; own (with the attributes of the paragraph, without its spaces at the
  ;; ends), the bookmarks before the formula with it
  (check= (in-tm '((p (@ (align "center")) (strong "see ") (bookmark (@ (name "e")))
                      (math (@ (display "true") (form "text")) "x") (strong " here"))))
          '((with "par-mode" "center" (strong "see"))
            (equation* (document (concat "x" (label "e"))))
            (with "par-mode" "center" (strong "here"))))
  ;; in a heading, in an item, in a cell: in the text (a formula which
  ;; starts an item is in its line)
  (check= (in-tm '((p (@ (role "heading") (level "1")) "T "
                      (math (@ (display "true") (form "text")) "x"))))
          '((section (concat "T " (math "x")))))
  (check= (in-tm '((list (@ (kind "bullet"))
                         (item (p (math (@ (display "true") (form "text")) "x")) (p "b"))
                         (item (p "a " (math (@ (display "true") (form "text")) "y"))))))
          '((itemize (document (concat (item) (math "x")) "b" (concat (item) "a")
                               (equation* (document "y"))))))
  ;; the same in a file of Word
  (check= (in-tm (docx (wp (wr "see ") "<m:oMathPara><m:oMath><m:r><m:t>x</m:t></m:r>"
                           "</m:oMath></m:oMathPara>" (wr " here"))))
          '("see" (equation* (document "x")) "here"))
  ;; a header cell in bold: its paragraphs and the text of its items, not
  ;; the list around them
  (check= (in-tm '((table (row (cell (@ (header "true") (borders "none"))
                                     (list (@ (kind "bullet")) (item (p "i"))))
                               (cell (@ (header "true") (borders "none")) (p "h") (p "k"))))))
          '((tabular (tformat (twith "table-width" "1par") (twith "table-hmode" "exact")
                              (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                              (table (row (cell (itemize (document (concat (item) (strong "i")))))
                                          (cell (document (strong "h") (strong "k")))))))))
  ;; such paragraphs written and read again are the same
  (with doc '(document (body (document "see" (equation* (document (concat "x" (label "e"))))
                                       "here" (section* (concat "T " (math "y")))))
                       (style "generic"))
    (for (fm '("docx-document" "odt-document"))
      (with back (convert (convert doc "texmacs-stree" fm) fm "texmacs-stree")
        (check= (cadr (list-find (cdr back) (lambda (y) (func? y 'body))))
                (cadadr doc)))))
  ;; the font of a declared name is its family (a font of the theme, whose
  ;; family is empty, is none); a name which is not declared is the font,
  ;; also in a named style; the line spacing and the keeping with the next
  ;; paragraph of a named style
  (let* ((decls (string-append
                  "<office:font-face-decls>"
                  "<style:font-face style:name='F' svg:font-family=''/>"
                  "<style:font-face style:name='LS1' svg:font-family=\"'Liberation Serif'\"/>"
                  "</office:font-face-decls>"))
         (doc (parse-odt-document
                (odt-file (string-append
                            (tp* "A" (tspan "TF" "f") " " (tspan "TL" "l") " "
                                 (tspan "TX" "x"))
                            (tp* "A" "a") (tp* "B" "b"))
                          (string-append
                            decls "<office:styles>"
                            "<style:style style:name='A' style:family='paragraph'/>"
                            "<style:style style:name='B' style:family='paragraph'>"
                            "<style:paragraph-properties style:line-spacing='0.1in'"
                            " fo:keep-with-next='always'/>"
                            "<style:text-properties style:font-name='LS1'"
                            " fo:font-size='12pt'/></style:style>"
                            "</office:styles>")
                          (string-append
                            "<style:style style:name='TF' style:family='text'>"
                            "<style:text-properties style:font-name='F'/></style:style>"
                            "<style:style style:name='TL' style:family='text'>"
                            "<style:text-properties style:font-name='LS1'/></style:style>"
                            "<style:style style:name='TX' style:family='text'>"
                            "<style:text-properties style:font-name='Foo Sans'/>"
                            "</style:style>"))))
         (blocks (list-filter (cdr doc) (lambda (y) (func? y 'p))))
         (styles (list-find (cdr doc) (lambda (y) (func? y 'styles))))
         (b (list-find (cdr styles) (lambda (y) (== (ox-attr y 'name) "B")))))
    (check= (car blocks)
            '(p "f " (font (@ (value "Liberation Serif")) "l") " "
                (font (@ (value "Foo Sans")) "x")))
    (check= (ox-attr b 'font-name) "Liberation Serif")
    (check= (ox-attr b 'line-spacing) "1.5")
    (check= (ox-attr b 'keep-with-next) "true")))

;; Import: the fixes of the second audit of the readers

(define (initial-of doc)
  ;; the initial environment of a document of TeXmacs, as an association
  ;; list
  (with init (list-find (cdr doc) (lambda (y) (func? y 'initial)))
    (map (lambda (a) (cons (cadr a) (caddr a))) (cdadr init))))

(define (test-import-audit-2)
  (check-group "import audit 2")
  (let* ((disp (lambda (s) (string-append "<m:oMathPara><m:oMath>" (mr s)
                                          "</m:oMath></m:oMathPara>")))
         (inl (lambda (s) (string-append "<m:oMath>" (mr s) "</m:oMath>")))
         (fc (lambda (type)
               (string-append "<w:r><w:fldChar w:fldCharType='" type "'/></w:r>")))
         (instr (lambda (s)
                  (string-append "<w:r><w:instrText xml:space='preserve'>" s
                                 "</w:instrText></w:r>")))
         (code " ADDIN ZOTERO_BIBL {} CSL_BIBLIOGRAPHY ")
         (start (string-append (fc "begin") (instr code) (fc "separate")))
         (zotero (lambda (x) `(field-blocks (@ (code ,code)) ,@x))))
    ;; 1. the punctuation after a displayed formula goes with it, between
    ;; two formulas too, and at the start of the text after it
    (check= (in-tm (docx (wp (wr "Hence") (disp "x") (wr "."))))
            '("Hence" (equation* (document "x."))))
    (check= (in-tm (docx (wp (wr "We have") (disp "a") (wr ", ") (disp "b") (wr " so."))))
            '("We have" (equation* (document "a,")) (equation* (document "b")) "so."))
    (check= (in-tm (docx (wp (wr "Then") (disp "x") (wr ". And more"))))
            '("Then" (equation* (document "x.")) "And more"))
    ;; 2. a formula with its number in a paragraph with text: an equation
    ;; with its number
    (check= (in-tm (docx (wp (wr "Text ") (disp "x") "<w:r><w:tab/></w:r>" (wr "(")
                             (fc "begin") (instr " SEQ Equation ") (fc "separate")
                             (wr "1") (fc "end") (wr ")"))))
            '("Text" (equation (document "x"))))
    ;; 3. a field of paragraphs ends at its end: the text after it, the
    ;; paragraph which starts with its end, are other paragraphs; two
    ;; fields of the same instruction are two fields
    (check= (docx (string-append (wp start (wr "Doe")) (wp (wr "Roe") (fc "end") (wr " Then"))))
            (list (zotero '((p "Doe") (p "Roe"))) '(p "Then")))
    (check= (docx (string-append (wp start (wr "Doe")) (wp (wr "Roe"))
                                 (wp (fc "end") (wr "Body."))))
            (list (zotero '((p "Doe") (p "Roe"))) '(p "Body.")))
    (check= (docx (string-append (wp start (wr "A")) (wp (wr "B") (fc "end"))
                                 (wp start (wr "C")) (wp (wr "D") (fc "end"))))
            (list (zotero '((p "A") (p "B"))) (zotero '((p "C") (p "D")))))
    ;; (its end in a link; its paragraphs in a list)
    (check= (docx (string-append (wp start (wr "A"))
                                 (wp "<w:hyperlink w:anchor='x'>" (wr "B") (fc "end")
                                     "</w:hyperlink>")
                                 (wp (wr "After"))))
            (list (zotero '((p "A") (p (link (@ (href "#x")) "B")))) '(p "After")))
    (let ((item (lambda l (apply wp (cons "<w:pPr><w:numPr><w:ilvl w:val='0'/><w:numId w:val='1'/></w:numPr></w:pPr>" l)))))
      (check= (docx (string-append (item start (wr "Doe")) (item (wr "Roe") (fc "end"))
                                   (wp (wr "After."))))
              (list (zotero '((list (@ (kind "bullet")) (item (p "Doe")) (item (p "Roe")))))
                    '(p "After."))))
    ;; 4. the paragraphs of the bibliography of Zotero stay as they are,
    ;; in their style, and are written again so
    (let* ((styles "<w:style w:type='paragraph' w:styleId='Bibliography'><w:name w:val='Bibliography'/></w:style>")
           (bp (lambda l (apply wp (cons "<w:pPr><w:pStyle w:val='Bibliography'/></w:pPr>" l))))
           (z (docx-file (string-append (bp start (wr "Doe, J.")) (bp (wr "Roe, R.") (fc "end"))
                                        (wp (wr "After.")) "<w:sectPr/>")
                         styles ""))
           (blocks (list (zotero '((p (@ (style "Bibliography")) "Doe, J.")
                                   (p (@ (style "Bibliography")) "Roe, R.")))
                         '(p "After.")))
           (t (convert z "docx-document" "texmacs-stree")))
      (check= (docx-blocks-of* z) blocks)
      (check= (in-tm (docx-blocks-of* z)) '("Doe, J." "Roe, R." "After."))
      (check= (list-filter (cdr (cadr (cadr t))) (lambda (b) (func? b 'office-field)))
              `((office-field ,code (document (office-par "Bibliography" "Doe, J.")
                                              (office-par "Bibliography" "Roe, R.")))))
      (check= (list-filter (docx-blocks-of* (convert t "texmacs-stree" "docx-document"))
                           (lambda (b) (in? (car b) '(p field-blocks))))
              blocks))
    ;; 5. the overline of a named style of text
    (let* ((doc (odt-file (tp "a " (tspan "O" "b"))
                          (string-append "<office:styles><style:style style:name='O' style:family='text'>"
                                         "<style:text-properties style:text-overline-style='solid'/>"
                                         "</style:style></office:styles>")
                          ""))
           (office (cdr (parse-odt-document doc))))
      (check= (in-tm office) '((concat "a " (overline "b"))))
      (check= (assoc-ref (initial-of (faithful (cons 'office office))) "office-span-O")
              '(macro "body" (overline (arg "body")))))
    ;; 6. a height of lines in a length is a part of a line of the size of
    ;; the text, which the parent says, or in percents of it
    (let ((spacing (lambda (styles)
                     (with st (list-find (cdr (parse-odt-document
                                                (odt-file "<text:p text:style-name='S'>x</text:p>"
                                                          styles "")))
                                         (lambda (y) (func? y 'styles)))
                       (and st (ox-attr (list-find (cdr st) (lambda (y) (== (ox-attr y 'name) "S")))
                                        'line-spacing))))))
      (check= (spacing (string-append
                         "<office:styles><style:style style:name='B' style:family='paragraph'>"
                         "<style:text-properties fo:font-size='10pt'/></style:style>"
                         "<style:style style:name='S' style:family='paragraph' style:parent-style-name='B'>"
                         "<style:paragraph-properties fo:line-height='18pt'/></style:style>"
                         "</office:styles>"))
              "1.5")
      (check= (spacing (string-append
                         "<office:styles><style:default-style style:family='paragraph'>"
                         "<style:text-properties fo:font-size='10pt'/></style:default-style>"
                         "<style:style style:name='S' style:family='paragraph'>"
                         "<style:paragraph-properties fo:line-height='18pt'/>"
                         "<style:text-properties fo:font-size='150%'/></style:style>"
                         "</office:styles>"))
              "1"))
    ;; 7. an inline formula with punctuation is in its line; the
    ;; punctuation before a formula stays before it
    (check= (in-tm (docx (wp (inl "x") (wr ".")))) '((concat (math "x") ".")))
    (check= (in-tm (docx (wp (wr ": ") (disp "x")))) '(":" (equation* (document "x"))))
    ;; 8. a declared font without a family is the font of its name
    (check= (odt-parts (tp "a " (tspan "T1" "b"))
                       "<style:style style:name='T1' style:family='text'><style:text-properties style:font-name='Arial'/></style:style>"
                       (cons "styles.xml"
                             (string-append "<office:document-styles" odt-ns "><office:font-face-decls>"
                                            "<style:font-face style:name='Arial'/>"
                                            "</office:font-face-decls></office:document-styles>")))
            '((p "a " (font (@ (value "Arial")) "b"))))
    ;; 9. the space before a paragraph which is cut by its formulas is
    ;; the one of its first piece
    (check= (in-tm (docx (wp "<w:pPr><w:jc w:val='center'/><w:spacing w:before='240'/></w:pPr>"
                             (wr "a ") (disp "x") (wr " b"))))
            '((surround (vspace* "12pt") (vspace "0pt") (with "par-mode" "center" "a"))
              (equation* (document "x"))
              (with "par-mode" "center" "b")))
    ;; 10. a picture which a Word file links to, not on the web: its name
    (check= (in-tm (docx-parts (wp "<w:r><w:drawing><wp:inline><wp:extent cx='720000' cy='360000'/>"
                                   "<a:graphic><a:graphicData><a:blip r:link='rId1'/></a:graphicData>"
                                   "</a:graphic></wp:inline></w:drawing></w:r>")
                               (cons "word/_rels/document.xml.rels"
                                     (string-append "<Relationships><Relationship Id='rId1' Target='img/x.png'"
                                                    " TargetMode='External'/></Relationships>"))))
            '("img/x.png"))
    ;; a style of paragraphs and one of text of the same name both have
    ;; their macros; the style of the usual paragraphs is a named style
    ;; too (for what the setup does not say)
    (let* ((doc (odt-file (string-append (tp "x") "<text:p text:style-name='N'>n</text:p>"
                                         (tp (tspan "N" "t")))
                          (string-append "<office:styles>"
                                         "<style:style style:name='N' style:family='paragraph'>"
                                         "<style:text-properties fo:font-weight='bold'/></style:style>"
                                         "<style:style style:name='N' style:family='text'>"
                                         "<style:text-properties fo:font-style='italic'/></style:style>"
                                         "</office:styles>")
                          ""))
           (init (initial-of (faithful (cons 'office (cdr (parse-odt-document doc)))))))
      (check= (assoc-ref init "office-par-N")
              '(macro "body" (with "font-series" "bold" (arg "body"))))
      (check= (assoc-ref init "office-span-N")
              '(macro "body" (with "font-shape" "italic" (arg "body")))))
    (let* ((doc (odt-file (string-append "<text:p text:style-name='K'>kept</text:p>"
                                         "<text:p text:style-name='K'>also</text:p>" (tp "next"))
                          (string-append "<office:styles>"
                                         "<style:style style:name='K' style:family='paragraph'>"
                                         "<style:paragraph-properties fo:keep-with-next='always'/>"
                                         "</style:style></office:styles>")
                          ""))
           (office (cdr (parse-odt-document doc)))
           (setup (list-find office (lambda (y) (func? y 'setup))))
           (styles (list-find office (lambda (y) (func? y 'styles)))))
      (check= (ox-attr setup 'body-style) "K")
      (check= (and styles (ox-attr (list-find (cdr styles) (lambda (y) (== (ox-attr y 'name) "K")))
                                   'keep-with-next))
              "true"))))

;; Import: the fixes of the third audit of the readers

(define (test-import-audit-3)
  (check-group "import audit 3")
  (let* ((disp (lambda (s) (string-append "<m:oMathPara><m:oMath>" (mr s)
                                          "</m:oMath></m:oMathPara>")))
         (fc (lambda (type)
               (string-append "<w:r><w:fldChar w:fldCharType='" type "'/></w:r>")))
         (instr (lambda (s)
                  (string-append "<w:r><w:instrText xml:space='preserve'>" s
                                 "</w:instrText></w:r>")))
         (fld (lambda (code res) (string-append (fc "begin") (instr code) (fc "separate")
                                                (wr res) (fc "end"))))
         (tab "<w:r><w:tab/></w:r>")
         (seq (fld " SEQ Equation " "1"))
         (code " ADDIN ZOTERO_BIBL {} CSL_BIBLIOGRAPHY ")
         (start (string-append (fc "begin") (instr code) (fc "separate")))
         (zotero (lambda (x) `(field-blocks (@ (code ,code)) ,@x)))
         (item (lambda l (apply wp (cons "<w:pPr><w:numPr><w:ilvl w:val='0'/><w:numId w:val='1'/></w:numPr></w:pPr>" l))))
         (ins (lambda l (string-append "<w:ins w:id='9' w:author='a'>" (apply string-append l)
                                       "</w:ins>"))))
    ;; 1. the closing bracket of a number with more text after it
    (check= (in-tm (docx (wp (wr "Text ") (disp "x") tab (wr "(") seq (wr ") and more"))))
            '("Text" (equation (document "x")) "and more"))
    (check= (in-tm (docx (wp (wr "Text ") (disp "x") (wr " (see above) holds"))))
            '("Text" (equation* (document "x")) "(see above) holds"))
    ;; 2. the text after the end of a field in an item, in a heading: a
    ;; plain paragraph, the field is one
    (check= (docx (string-append (item start (wr "Doe")) (item (wr "Roe") (fc "end") (wr " After"))))
            (list (zotero '((list (@ (kind "bullet")) (item (p "Doe")) (item (p "Roe")))))
                  '(p "After")))
    (check= (docx (string-append (wp start (wr "A"))
                                 "<w:p><w:pPr><w:outlineLvl w:val='0'/></w:pPr>"
                                 (wr "B") (fc "end") (wr "Title") "</w:p>"))
            (list (zotero '((p "A") (p (@ (role "heading") (level "1") (numbered "no")) "B")))
                  '(p "Title")))
    ;; (and the text before its start)
    (check= (docx (string-append (wp (wr "Intro ") start (wr "Doe")) (wp (wr "Roe") (fc "end"))))
            (list '(p "Intro") (zotero '((p "Doe") (p "Roe")))))
    ;; 3. its end in a change which is tracked
    (check= (docx (string-append (wp start (wr "Doe"))
                                 (wp (wr "Roe") (ins (fc "end") (wr " After")))))
            (list (zotero '((p "Doe") (p "Roe"))) '(p (ins (@ (author "a")) "After"))))
    (check= (docx (string-append (wp start (wr "Doe"))
                                 (wp (ins (wr "Roe") (fc "end")) (wr " After"))))
            (list (zotero '((p "Doe") (p (ins (@ (author "a")) "Roe")))) '(p "After")))
    ;; 4. a field which starts where another one ends
    (check= (docx (string-append (wp start (wr "A")) (wp (wr "B") (fc "end") start (wr "C"))
                                 (wp (wr "D") (fc "end"))))
            (list (zotero '((p "A") (p "B"))) (zotero '((p "C") (p "D")))))
    ;; 5. a decimal point after a formula is not its punctuation
    (check= (in-tm (docx (wp (wr "Thus") (disp "x") (wr ".5 is small"))))
            '("Thus" (equation* (document "x")) ".5 is small"))
    ;; 6. the punctuation after a bookmark, in a node of formatting
    (check= (in-tm (docx (wp (wr "Text") (disp "x")
                             "<w:bookmarkStart w:id='3' w:name='b3'/><w:bookmarkEnd w:id='3'/>"
                             (wr ". after"))))
            '("Text" (equation* (document (concat "x." (label "b3")))) "after"))
    (check= (in-tm (docx (wp (wr "Sum") (disp "x") (wr "." "<w:i/>") (wr " Next"))))
            '("Sum" (equation* (document "x.")) "Next"))
    ;; 7. what a named style says is not said again by hand
    (check= (docx (wp "<w:r><w:rPr><w:rStyle w:val='Accent'/><w:i/></w:rPr><w:t>e</w:t></w:r>"))
            '((p (span (@ (style "Accent")) "e"))))
    (let* ((ovl "<style:text-properties style:text-overline-style='solid'/>")
           (doc (odt-file (tp "a " (tspan "T1" "b"))
                          (string-append "<office:styles><style:style style:name='O' style:family='text'>"
                                         ovl "</style:style></office:styles>")
                          (string-append "<style:style style:name='T1' style:family='text'"
                                         " style:parent-style-name='O'>" ovl "</style:style>"))))
      (check= (in-tm (cdr (parse-odt-document doc))) '((concat "a " (overline "b")))))
    ;; 8. a formula with the number of its chapter
    (check= (in-tm (docx (wp (disp "x") tab (wr "(") (fld " STYLEREF 1 \\s " "2") (wr ".")
                             seq (wr ")"))))
            '((equation (document "x"))))
    (check= (in-tm (docx (wp (wr "Text") (disp "x") tab (wr "(") (fld " STYLEREF 1 \\s " "2")
                             (wr "-") seq (wr ")"))))
            '("Text" (equation (document "x"))))
    ;; 9. a field with a break of page, an empty paragraph
    (check= (docx (string-append (wp start (wr "A"))
                                 "<w:p><w:pPr><w:pageBreakBefore/></w:pPr>" (wr "B") "</w:p>"
                                 (wp (wr "C") (fc "end"))))
            (list (zotero '((p "A") (pagebreak) (p "B") (p "C")))))
    (with b (docx (string-append (wp start (wr "A")) (wp) (wp (wr "B") (fc "end"))))
      (check= b (list (zotero '((p "A") (p) (p "B")))))
      (check= (in-tm b) '("A" "" "B")))
    ;; a picture of an OpenDocument text which is linked: its address
    ;; (on the web, an image)
    (let* ((frame (lambda (href)
                    (string-append "<text:p><draw:frame svg:width='2cm' svg:height='1cm'>"
                                   "<draw:image xlink:href='" href "'/></draw:frame></text:p>")))
           (b (bare (cdr (parse-odt-document
                           (odt-file (string-append (frame "../pics/my pic.png")
                                                    (frame "https://ex.org/a.png"))
                                     "" ""))))))
      (check= (list-filter b (lambda (y) (func? y 'p)))
              '((p (image (@ (name "../pics/my pic.png") (width "2cm") (height "1cm")
                             (linked "true"))))
                (p (image (@ (name "https://ex.org/a.png") (width "2cm") (height "1cm"))))))
      (check= (in-tm b) '("../pics/my pic.png" (image "https://ex.org/a.png" "2cm" "1cm" "" ""))))
    ;; a size in percents in the default style
    (with st (list-find (cdr (parse-odt-document
                               (odt-file "<text:p text:style-name='S'>x</text:p>"
                                         (string-append
                                           "<office:styles><style:default-style style:family='paragraph'>"
                                           "<style:text-properties fo:font-size='150%'/></style:default-style>"
                                           "<style:style style:name='S' style:family='paragraph'>"
                                           "<style:paragraph-properties fo:line-height='18pt'/>"
                                           "</style:style></office:styles>")
                                         "")))
                        (lambda (y) (func? y 'styles)))
      (check= (ox-attr (list-find (cdr st) (lambda (y) (== (ox-attr y 'name) "S"))) 'line-spacing)
              "0.83"))))

(define (test-import-audit-4)
  (check-group "import audit 4")
  (let* ((disp (lambda (s) (string-append "<m:oMathPara><m:oMath>" (mr s)
                                          "</m:oMath></m:oMathPara>")))
         (fc (lambda (type)
               (string-append "<w:r><w:fldChar w:fldCharType='" type "'/></w:r>")))
         (instr (lambda (s)
                  (string-append "<w:r><w:instrText xml:space='preserve'>" s
                                 "</w:instrText></w:r>")))
         (fld (lambda (code res) (string-append (fc "begin") (instr code) (fc "separate")
                                                (wr res) (fc "end"))))
         (tab "<w:r><w:tab/></w:r>")
         (seq (fld " SEQ Equation " "1"))
         (code " ADDIN ZOTERO_BIBL {} CSL_BIBLIOGRAPHY ")
         (start (string-append (fc "begin") (instr code) (fc "separate")))
         (zotero (lambda (x) `(field-blocks (@ (code ,code)) ,@x)))
         (heading (lambda l (string-append "<w:p><w:pPr><w:outlineLvl w:val='0'/></w:pPr>"
                                           (apply string-append l) "</w:p>")))
         ;; (a Word document with these styles)
         (styled (lambda (body styles)
                   (cdr (parse-docx-document
                          (zip-pack '("word/document.xml" "word/styles.xml")
                                    (list (string-append "<w:document" docx-ns "><w:body>"
                                                         body "</w:body></w:document>")
                                          (string-append "<w:styles" docx-ns ">" styles
                                                         "</w:styles>")))))))
         (char-style (lambda (id rpr)
                       (string-append "<w:style w:type='character' w:styleId='" id "'>"
                                      "<w:name w:val='" id "'/><w:rPr>" rpr "</w:rPr></w:style>")))
         (par-style (lambda (id rpr)
                      (string-append "<w:style w:type='paragraph' w:styleId='" id "'>"
                                     "<w:name w:val='" id "'/><w:rPr>" rpr "</w:rPr></w:style>")))
         (rs (lambda (id text rpr)
               (string-append "<w:r><w:rPr><w:rStyle w:val='" id "'/>" rpr "</w:rPr><w:t>"
                              text "</w:t></w:r>")))
         (pp (lambda (id . l) (string-append "<w:p>" (wstyle id) (apply string-append l) "</w:p>"))))
    ;; 1. a paragraph which only starts with the end of a field keeps
    ;; what it is (a heading)
    (check= (docx (string-append (wp start (wr "Doe")) (wp (wr "Roe"))
                                 (heading (fc "end") (wr "Appendix"))))
            (list (zotero '((p "Doe") (p "Roe")))
                  '(p (@ (role "heading") (level "1") (numbered "no")) "Appendix")))
    ;; 2. the bold and italics of complex scripts are kept by a style
    (check= (bare (styled (wp (rs "R" "e" "<w:rtl/><w:bCs/><w:iCs/>")) (char-style "R" "<w:rtl/>")))
            '((p (span (@ (style "R")) (strong (em "e"))))))
    ;; 3. a span which is not bold in a bold paragraph (OpenDocument)
    (check= (bare (cdr (parse-odt-document
                         (odt-file (string-append "<text:p text:style-name='P1'>a "
                                                  (tspan "T1" "b") "</text:p>")
                                   (string-append "<office:styles><style:style style:name='N'"
                                                  " style:family='text'><style:text-properties"
                                                  " fo:font-weight='normal'/></style:style>"
                                                  "</office:styles>")
                                   (string-append "<style:style style:name='P1' style:family='paragraph'>"
                                                  "<style:text-properties fo:font-weight='bold'/>"
                                                  "</style:style><style:style style:name='T1'"
                                                  " style:family='text' style:parent-style-name='N'>"
                                                  "<style:text-properties fo:font-weight='normal'/>"
                                                  "</style:style>")))))
            '((p (strong "a ") (span (@ (style "N")) "b"))))
    ;; 4. an underline of another kind than the one of the style
    (check= (bare (styled (wp (rs "U" "e" "<w:u w:val='double'/>")) (char-style "U" "<w:u w:val='single'/>")))
            '((p (span (@ (style "U")) (underline (@ (value "double")) "e")))))
    ;; 5. the text before the start of a field in a heading is the heading
    (check= (docx (string-append (heading (wr "References ") start (wr "Doe"))
                                 (wp (wr "Roe") (fc "end"))))
            (list '(p (@ (role "heading") (level "1") (numbered "no")) "References")
                  (zotero '((p "Doe") (p "Roe")))))
    ;; 6. what is no text before the start of a field goes into it
    (check= (docx (string-append (wp tab start (wr "Doe")) (wp (wr "Roe") (fc "end"))))
            (list (zotero '((p "Doe") (p "Roe")))))
    (check= (docx (string-append (wp "<w:bookmarkStart w:id='1' w:name='bib'/>" start (wr "Doe"))
                                 (wp (wr "Roe") (fc "end"))))
            (list (zotero '((p (bookmark (@ (name "bib"))) "Doe") (p "Roe")))))
    ;; 7. the style of the text after a field in an item is defined
    (with b (styled (string-append (wp start (wr "Doe"))
                                   "<w:p><w:pPr><w:pStyle w:val='MyItem'/><w:numPr><w:ilvl w:val='0'/>"
                                   "<w:numId w:val='1'/></w:numPr></w:pPr>"
                                   (wr "Roe") (fc "end") (wr " After") "</w:p>")
                    (par-style "MyItem" "<w:color w:val='00FF00'/>"))
      (check= (map (lambda (st) (ox-attr st 'name))
                   (cdr (or (list-find b (lambda (y) (func? y 'styles))) '(styles))))
              '("MyItem")))
    ;; 8. the point after the number of a formula, a dash before it, the
    ;; text of a heading
    (check= (in-tm (docx (wp (wr "Text") (disp "x") tab (wr "(") seq (wr ")."))))
            '("Text" (equation (document "x."))))
    (check= (in-tm (docx (wp (disp "x") tab (wr "(") seq (wr ")") (wr "."))))
            '((equation (document "x."))))
    (check= (in-tm (docx (wp (disp "x") (wr " -") tab (wr "(") seq (wr ")"))))
            '((equation* (document "x")) (concat "-" (space "2em") "(1)")))
    (check= (in-tm (docx (wp (disp "x") tab (fld " STYLEREF \"Heading 1\" " "Intro") tab
                             (wr "(") seq (wr ")"))))
            '((equation* (document "x")) (concat (space "2em") "Intro" (space "2em") "(1)")))
    ;; 9. linked pictures: only addresses of files
    (let* ((frame (lambda (href)
                    (string-append "<text:p><draw:frame svg:width='2cm' svg:height='1cm'>"
                                   "<draw:image xlink:href='" href "'/></draw:frame></text:p>")))
           (b (bare (cdr (parse-odt-document
                           (odt-file (string-append (frame "#") (frame "javascript:x()")
                                                    (frame "Pictures/missing.png")
                                                    (frame (string-append "data:image/png;base64,"
                                                                          "iVBORw0KGgo=")))
                                     "" ""))))))
      (check= (map (lambda (p) (ox-attr (car (ox-children p)) 'name))
                   (list-filter b (lambda (y) (func? y 'p))))
              '("image.png")))
    ;; 10. the spaces after the end of a field, after deleted text
    (check= (docx (string-append (wp start (wr "Doe"))
                                 (wp (wr "Roe") "<w:del w:id='7' w:author='d'>" (fc "end")
                                     "<w:r><w:delText xml:space='preserve'> Gone</w:delText></w:r>"
                                     "</w:del>" (wr " Kept"))))
            (list (zotero '((p "Doe") (p "Roe"))) '(p (del (@ (author "d")) "Gone") "Kept")))
    ;; the end of a field in a change inside a link
    (check= (docx (string-append (wp start (wr "Doe"))
                                 (wp "<w:hyperlink w:anchor='x'>" (wr "Roe")
                                     "<w:ins w:id='9' w:author='a'>" (fc "end") (wr " After")
                                     "</w:ins></w:hyperlink>")))
            (list (zotero '((p "Doe") (p (link (@ (href "#x")) "Roe"))))
                  '(p (link (@ (href "#x")) (ins (@ (author "a")) "After")))))
    ;; upright or not bold by hand against the style, italics of the
    ;; paragraph and of the style which cancel each other
    (let ((st (string-append (char-style "I" "<w:i/>") (char-style "B" "<w:b/>")
                             (par-style "PI" "<w:i/>"))))
      (check= (bare (styled (wp (rs "I" "e" "<w:i w:val='0'/>")) st))
              '((p (span (@ (style "I")) (em "e")))))
      (check= (bare (styled (wp (rs "B" "e" "<w:b w:val='0'/>")) st))
              '((p (span (@ (style "B")) (strong (@ (value "off")) "e")))))
      (check= (bare (styled (pp "PI" (rs "I" "e" "")) st))
              '((setup (@ (body-style "PI"))) (p (span (@ (style "I")) (em "e")))))
      (check= (in-tm '((p (strong (@ (value "off")) "e"))))
              '((with "font-series" "medium" "e"))))))

(define (test-import-math)
  (check-group "import math")
  (let ((f (lambda (s) (in-tm (docx (wp (wr "f ") "<m:oMath>" s "</m:oMath>")))))
        (acc (lambda (code)
               (string-append "<m:acc><m:accPr><m:chr m:val='&#x" code ";'/></m:accPr>"
                              "<m:e>" (mr "x") "</m:e></m:acc>"))))
    ;; a division is no bracket
    (check= (f (mr "a/b+c")) '((concat "f " (math "a/b+c"))))
    (check= (f (string-append "<m:f><m:fPr><m:type m:val='lin'/></m:fPr><m:num>" (mr "a")
                              "</m:num><m:den>" (mr "b") "</m:den></m:f>" (mr "+c")))
            '((concat "f " (math "a/b+c"))))
    (check= (in-tm '((p "f " (math "<math><mi>a</mi><mo>/</mo><mi>b</mi></math>"))))
            '((concat "f " (math "a/b"))))
    ;; the accents are those of TeXmacs
    (check= (map (lambda (code) (f (acc code)))
                 '("302" "307" "308" "30a" "20d7" "20db" "20dc"))
            (map (lambda (a) `((concat "f " (math (wide "x" ,a)))))
                 '("^" "<dot>" "<ddot>" "<abovering>" "<vect>" "<dddot>" "<ddddot>")))
    (check= (in-tm `((p "f " (math ,(string-append
                                      "<math><mover accent='true'><mi>x</mi><mo>"
                                      (office-utf8 #x2d9) "</mo></mover></math>")))))
            '((concat "f " (math (wide "x" "<dot>")))))
    ;; the alphabets of a run
    (check= (f (string-append
                 "<m:r><m:rPr><m:scr m:val='double-struck'/></m:rPr><m:t>R</m:t></m:r>"
                 "<m:r><m:rPr><m:scr m:val='script'/></m:rPr><m:t>L</m:t></m:r>"
                 "<m:r><m:rPr><m:scr m:val='fraktur'/></m:rPr><m:t>g</m:t></m:r>"))
            '((concat "f " (math "<bbb-R><cal-L><frak-g>"))))
    ;; text in a formula keeps the spaces around it
    (check= (f (string-append (mr "x") "<m:r><m:rPr><m:nor/></m:rPr>"
                              "<m:t xml:space='preserve'> for all </m:t></m:r>" (mr "y")))
            '((concat "f " (math (concat "x" (with "mode" "text" " for all ") "y")))))))

(define (test-import-files)
  (check-group "import files")
  ;; a file which is no archive, or which has no text, says so
  (check= (convert "not an archive" "docx-document" "texmacs-stree")
          '(document (body (document (with "color" "red"
                                       "This file is not a Word document")))
                     (style "generic")))
  (check= (parse-docx-document (zip-pack '("a.txt") '("x")))
          '(office (error "This Word document has no text")))
  (check= (parse-odt-document "not an archive")
          '(office (error "This file is not an OpenDocument text")))
  (check= (parse-odt-document (zip-pack '("a.txt") '("x")))
          '(office (error "This OpenDocument file has no text")))
  ;; other prefixes for the namespaces, or none
  (let ((w "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
        (m "http://schemas.openxmlformats.org/officeDocument/2006/math")
        (t "urn:oasis:names:tc:opendocument:xmlns:text:1.0")
        (o "urn:oasis:names:tc:opendocument:xmlns:office:1.0"))
    (check= (bare-office
              (parse-docx-document
                (zip-pack '("word/document.xml")
                          (list (string-append
                                  "<x:document xmlns:x='" w "'><x:body><x:p><x:r>"
                                  "<x:rPr><x:b x:val='0'/><x:i/></x:rPr><x:t>hello</x:t>"
                                  "</x:r></x:p></x:body></x:document>")))))
            '(office (p (em "hello"))))
    (check= (bare-office
              (parse-docx-document
                (zip-pack '("word/document.xml")
                          (list (string-append
                                  "<document xmlns='" w "'><body><p><r><t>hello</t></r>"
                                  "<n:oMath xmlns:n='" m "'><n:r><n:t>x</n:t></n:r></n:oMath>"
                                  "</p></body></document>")))))
            '(office (p "hello" (math (@ (form "sxml")) (m:math (m:mi "x"))))))
    (check= (bare-office
              (parse-odt-document
                (zip-pack '("content.xml")
                          (list (string-append
                                  "<a:document-content xmlns:a='" o "' xmlns:b='" t "'>"
                                  "<a:body><a:text><b:p>hello<b:s b:c='2'/>x</b:p></a:text>"
                                  "</a:body></a:document-content>")))))
            '(office (p "hello  x")))))

(define (test-officetm-more)
  (check-group "officetm more")
  ;; a space at which a line is not broken, a place where a word may be cut
  (check= (in-tm `((p ,(string-append "a" (office-utf8 #xa0) "b sil"
                                      (office-utf8 #xad) "ben"))))
          '((concat "a" (nbsp) "b silben")))
  ;; the page of a bookmark
  (check= (in-tm '((p (bookmark (@ (name "b"))) "x")
                   (p "page " (ref (@ (name "b") (kind "page")) "5"))))
          '((concat (label "b") "x") (concat "page " (pageref "b"))))
  ;; a title of two paragraphs has two lines; a subtitle alone is kept
  (check= (in-tm '((p (@ (role "title")) "One") (p (@ (role "title")) "Two")
                   (p (@ (role "date")) "2026") (p (@ (role "date")) "May") (p "x")))
          '((doc-data (doc-title (concat "One" (next-line) "Two"))
                      (doc-date (concat "2026" (next-line) "May")))
            "x"))
  (check= (in-tm '((p (@ (role "subtitle")) "S") (p "x")))
          '((doc-data (doc-subtitle "S")) "x"))
  ;; a level which is no small integer
  (check= (in-tm '((p (@ (role "heading") (level "2.6")) "H")))
          '((subsubsection "H")))
  (check= (in-tm '((p (@ (role "heading") (level "x")) "H"))) '((section "H"))))

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
  ;; displayed when the node says so, or when the formula is alone (the
  ;; punctuation after it is in the equation)
  (check= (tm '(p (math (@ (display "true")) "<math><mi>x</mi></math>") "."))
          '((equation* (document "x."))))
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

;; What has a number in an office file is what numbers itself in TeXmacs:
;; the names and the numbers are taken away, the labels are kept, and the
;; references to them are references.
(define (test-officetm-numbers)
  (check-group "officetm numbers")
  (check= (tm '(p (@ (role "theorem")) (strong "Theorem ") (bookmark (@ (name "t")))
                  (seq (@ (name "Theorem")) "1") (strong ".") " Body.")
              '(p (@ (role "theorem")) "More.")
              '(p (@ (role "proof")) (strong "Proof.") " Easy.")
              '(p "see " (ref (@ (name "t")) "1") " and " (ref (@ (name "t")) "the theorem")))
          '((theorem (document (concat (label "t") "Body.") "More."))
            (proof (document "Easy."))
            (concat "see " (reference "t") " and " (hlink "the theorem" "#t"))))
  ;; a name which is not one of TeXmacs stays as it is
  (check= (tm '(p (@ (role "theorem")) (strong "Satz 1.") " Text."))
          '((concat (strong "Satz 1.") " Text.")))
  ;; a formula with its number is an equation
  (check= (tm '(p (math (@ (display "true")) "<math><mi>x</mi></math>") (tab) "("
                  (bookmark (@ (name "e"))) (seq (@ (name "Equation")) "1") ")"))
          '((equation (document (concat "x" (label "e"))))))
  ;; a caption without the name of its figure
  (check= (tm '(p (image (@ (name "f.png"))))
              '(p (@ (role "caption")) "Figure " (seq (@ (name "Figure")) "1")
                  ": The figure"))
          '((big-figure (image "f.png" "" "" "" "") "The figure")))
  ;; headings without a number
  (check= (tm '(p (@ (role "heading") (level "1") (numbered "no")) "A")
              '(p (@ (role "heading") (level "2")) (bookmark (@ (name "s"))) "B")
              '(p (ref (@ (name "s")) "1.1")))
          '((section* "A") (subsection (concat (label "s") "B")) (reference "s")))
  ;; the entries of a bibliography, without the heading before them
  (check= (tm '(p (@ (role "heading") (level "1")) "References")
              '(p (@ (role "bibitem")) "[" (bookmark (@ (name "b")))
                  (seq (@ (name "Reference")) "1") "] A. Author.")
              '(p (@ (role "bibitem")) "Another one."))
          '((bibliography "bib" "tm-plain" ""
              (document (bib-list "2" (document (concat (bibitem* "1") (label "b") "A. Author.")
                                                (concat (bibitem* "2") "Another one."))))))))

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
  ;; (the title of the properties of the file is not shown: it is often
  ;; the one of a template)
  (check= (tm '(meta (title "From the file") (author "Someone")) '(p "x"))
          '("x"))
  (check= (tm '(meta (title "From the file")) '(p (@ (role "title")) "T"))
          '((doc-data (doc-title "T")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs trees as office trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (no-setup l)
  ;; the blocks l without the page and the font of their document
  (list-filter (bare l) (lambda (b) (not (func? b 'setup)))))

(define (of t)
  ;; the blocks of the office tree of a TeXmacs tree
  (no-setup (cdr (texmacs->office t '()))))

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
  (check= (of '(document (subsection* "S") "text" (subsubsection* "T")))
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
  (check= (of '(theorem (document "T")))
          '((p (@ (role "theorem")) (strong "Theorem.") " T")))
  (check= (of '(remark (document "R")))
          '((p (@ (role "remark")) (strong "Remark.") " R")))
  (check= (of '(proof (document "P" "Q")))
          '((p (@ (role "proof")) (strong "Proof.") " P") (p (@ (role "proof")) "Q")))
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
                         (section* "S") "x"))))
          '((p (@ (role "title")) "T") (p (@ (role "author")) "A")
            (p (@ (role "date")) "2026") (p (@ (role "abstract")) "Abs.")
            (p (@ (role "heading") (level "1")) "S") (p "x"))))

;; The numbers and the labels. A name with its number, as the styles write
;; it, is a number of a sequence when it is the next one; a label is given
;; to the number before it, and a reference says what it refers to.
(define (test-tmoffice-numbers)
  (check-group "tmoffice numbers")
  (check= (of '(document
                 (render-theorem (concat "Theorem " (with "font-shape" "right" "1"))
                                 (document (concat (label "thm:a") "T")))
                 (render-theorem "Lemma 2" (document "L"))
                 (concat "see " (hlink "1" "#thm:a") " and " (hlink "x" "#nowhere"))))
          '((p (@ (role "theorem"))
               (strong "Theorem " (seq (@ (name "Theorem") (id "Theorem1")
                                          (labels "thm:a")) "1") ".")
               " T")
            (p (@ (role "theorem"))
               (strong "Lemma " (seq (@ (name "Theorem") (id "Theorem2")) "2") ".")
               " L")
            (p "see " (ref (@ (name "thm:a") (kind "seq") (target "Theorem1")) "1")
               " and " (ref (@ (name "nowhere")) "x"))))
  ;; a label in the text, away from its theorem: the number has its
  ;; bookmark, and a bookmark of its own (at) is at its place
  (check= (of '(document
                 (render-theorem (concat "Theorem " (with "font-shape" "right" "1"))
                                 (document "T"))
                 (concat "text " (label "p") " more")))
          '((p (@ (role "theorem"))
               (strong "Theorem " (seq (@ (name "Theorem") (id "Theorem1")
                                          (labels "p")) "1") ".")
               " T")
            (p "text " (bookmark (@ (name "p") (at "true"))) " more")))
  ;; and read: the label is at its place, not at its number
  (check= (cadr (cadr (office->texmacs
                        '(office (p (@ (role "heading") (level "1"))
                                    (bookmark (@ (name "p"))) "Head")
                                 (p "text " (bookmark (@ (name "_TmAt_p"))) " more")))))
          '(document (section "Head") (concat "text " (label "p") " more")))
  ;; a caption of LibreOffice, after the images of its frame and a break
  (check= (cadr (cadr (office->texmacs
                        '(office (p (@ (role "caption")) (image (@ (name "p.png") (data "PNGDATA")))
                                    (br) "Figure " (seq (@ (name "Figure")) "1")
                                    ": Cap")))))
          '(document (big-figure (image (tuple (raw-data "PNGDATA") "p.png") "" "" "" "")
                                 "Cap")))
  ;; a number which is not the next one of its sequence stands for itself
  (check= (of '(render-theorem "Theorem 2.1" (document "T")))
          '((p (@ (role "theorem"))
               (strong "Theorem " (seq (@ (id "Text1")) "2.1") ".") " T")))
  ;; a figure, an equation, an entry of the bibliography
  (check= (of '(render-big-figure "figure" "Figure 1" "body"
                                  (concat (label "fig:a") "Cap.")))
          '((p (@ (role "figure")) "body")
            (p (@ (role "caption"))
               (strong "Figure " (seq (@ (name "Figure") (id "Figure1")
                                         (labels "fig:a")) "1") ".")
               " Cap.")))
  (check= (of '(equation-lab (document (concat "x" (label "eq:a"))) "1"))
          '((p (math (@ (display "true") (form "sxml")) (m:math (m:mi "x")))
               (tab) "(" (seq (@ (name "Equation") (id "Equation1") (labels "eq:a")) "1")
               ")")))
  (check= (of '(description (document (concat (render-bibitem (strong "[1]  "))
                                              (label "bib-a") "A. Author."))))
          '((p (@ (role "bibitem")) "["
               (seq (@ (name "Reference") (id "Reference1") (labels "bib-a")) "1")
               "] A. Author.")))
  ;; a heading whose title is laid out with its number: the number is
  ;; apart, and a label after the heading is the one of the heading
  (check= (of '(document
                 (section-title (tformat (table (row (cell (concat "2" (space "2spc")))
                                                     (cell "Second")))))
                 (label "sec:b")
                 (concat "see " (hlink "2" "#sec:b"))
                 (section-title "Plain")))
          '((p (@ (role "heading") (level "1") (number "2") (id "heading1")
                  (labels "sec:b"))
               "Second")
            (p "see " (ref (@ (name "sec:b") (kind "heading") (target "heading1")) "2"))
            (p (@ (role "heading") (level "1")) "Plain"))))

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
    (check= (ox-attr title 'align) "left")
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
  ;; the short rows of a matrix are completed with empty cells
  (check= (mathml->omml '(m:math (m:mtable (m:mtr (m:mtd (m:mi "a")) (m:mtd (m:mi "b")))
                                           (m:mtr (m:mtd (m:mi "c"))))))
          '(m:oMath (m:m (m:mr (m:e (m:r (m:t "a"))) (m:e (m:r (m:t "b"))))
                         (m:mr (m:e (m:r (m:t "c"))) (m:e)))))
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
    (check= (no-setup (cdr (parse-docx-document docx)))
            '((p "a " (em "b") (note (p "n")))
              (p (image (@ (name "image1.png") (data "PNGDATA") (width "2cm")
                           (height "1cm"))))
              (p (math (@ (form "sxml")) (m:math (m:mi "x"))))))
    (check= (list-head (no-setup (cdr (parse-odt-document odt))) 2)
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
    (check= (no-setup (cdr (parse-docx-document docx)))
            '((p (image (@ (name "image1.png") (data "PNGDATA") (width "2cm")
                           (height "1cm"))))))
    (check= (no-setup (cdr (parse-odt-document odt)))
            '((p (image (@ (name "image1.png") (data "PNGDATA") (width "2cm")
                           (height "1cm")))))))
  ;; the text of XML: its characters are escaped, the control ones removed
  (check= (ox-serialize-element '(a (@ (x "1 & \"2\"")) "t < u" (b)))
          "<a x=\"1 &amp; &quot;2&quot;\">t &lt; u<b/></a>")
  ;; an empty document is a document
  (check= (list-head (convert (serialize-docx-document '(office))
                              "docx-document" "texmacs-stree") 3)
          '(document (body (document "")) (style "office")))
  (check= (list-head (convert (serialize-odt-document '(office))
                              "odt-document" "texmacs-stree") 3)
          '(document (body (document "")) (style "office"))))

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

;; A document with numbers, as the editor expands it, comes back from both
;; formats with its theorems, its equations, its references. (The labels
;; have other names in a Word file, which only allows some characters.)
(define (test-numbered-round-trip)
  (check-group "numbered round trip")
  (let* ((expanded
           '(document
              (section-title (tformat (table (row (cell (concat "1" (space "2spc")))
                                                  (cell "Intro")))))
              (label "s")
              (concat "see " (hlink "1" "#t") ", (" (hlink "1" "#e") ") and "
                      (hlink "1" "#s"))
              (render-theorem "Theorem 1" (document (concat (label "t") "Body.")))
              (render-proof "Proof" (document "Easy."))
              (equation-lab (document (concat "x" (label "e"))) "1")
              (section-title "Plain")))
         (back (lambda (fm)
                 (cdr (cadr (cadr (convert (convert expanded "texmacs-stree" fm)
                                           fm "texmacs-stree")))))))
    (check= (back "odt-document")
            '((section (concat (label "s") "Intro"))
              (concat "see " (reference "t") ", (" (reference "e") ") and "
                      (reference "s"))
              (theorem (document (concat (label "t") "Body.")))
              (proof (document "Easy."))
              (equation (document (concat "x" (label "e"))))
              (section* "Plain")))
    (check= (back "docx-document")
            '((section (concat (label "s") "Intro"))
              (concat "see " (reference "t") ", (" (reference "e") ") and "
                      (reference "s"))
              (theorem (document (concat (label "t") "Body.")))
              (proof (document "Easy."))
              (equation (document (concat "x" (label "e"))))
              (section* "Plain")))))

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

;; More of the export: what the converter and the writers must keep.

(define (export-entry z name)
  ;; the text of the file name of the archive z, or ""
  (or (office-entry (office-archive z) name) ""))

(define (export-has? s what)
  (>= (string-search-forwards what 0 s) 0))

(define (test-export-math)
  (check-group "export math")
  (let ((nbsp (office-utf8 #xa0)))
    ;; a binomial is a fraction without a bar in brackets; the fractions
    ;; of LaTeX are fractions; mod is a text
    (check= (of '(math (concat (binom "n" "k") "+" (tfrac "1" "2") "+a" (bmod) "b")))
            `((p (math (@ (form "sxml"))
                       (m:math
                         (m:mrow (m:mrow (m:mo (@ (form "prefix")) "(")
                                         (m:mfrac (@ (linethickness "0"))
                                                  (m:mi "n") (m:mi "k"))
                                         (m:mo (@ (form "postfix")) ")"))
                                 (m:mo "+") (m:mfrac (m:mn "1") (m:mn "2"))
                                 (m:mo "+") (m:mi "a")
                                 (m:mtext ,(string-append nbsp "mod" nbsp))
                                 (m:mi "b"))))))))
  (check= (mathml->omml '(m:math (m:mfrac (@ (linethickness "0")) (m:mi "n") (m:mi "k"))))
          '(m:oMath (m:f (m:fPr (m:type (@ (m:val "noBar"))))
                         (m:num (m:r (m:t "n"))) (m:den (m:r (m:t "k"))))))
  ;; the brackets by their characters; no bracket at a side is a bracket
  ;; without a character
  (check= (of '(math (concat (around* "<langle>" "u" "<rangle>")
                             (left "{") "y" (right "."))))
          `((p (math (@ (form "sxml"))
                     (m:math
                       (m:mrow (m:mrow (m:mo (@ (form "prefix")) ,(office-utf8 #x27e8))
                                       (m:mi "u")
                                       (m:mo (@ (form "postfix")) ,(office-utf8 #x27e9)))
                               (m:mrow (m:mo (@ (form "prefix")) "{") (m:mi "y")
                                       (m:mo (@ (form "postfix"))))))))))
  (check= (mathml->omml '(m:math (m:mrow (m:mo (@ (form "prefix")) "{") (m:mi "y")
                                         (m:mo (@ (form "postfix"))))))
          '(m:oMath (m:d (m:dPr (m:begChr (@ (m:val "{"))) (m:endChr (@ (m:val ""))))
                         (m:e (m:r (m:t "y"))))))
  ;; an operator which is a word is a name; a big operator which is
  ;; written as a word is its character; an accent which is not known is
  ;; left out
  (check= (of '(math (concat (below "lim" "n") (big "iint") "f" (wide "x" "<tilde>"))))
          `((p (math (@ (form "sxml"))
                     (m:math (m:mrow (m:munder (m:mi "lim") (m:mi "n"))
                                     (m:mo ,(office-utf8 #x222c)) (m:mi "f")
                                     (m:mi "x")))))))
  ;; the scripts after the base are kept with those before it
  (check= (mathml->omml '(m:math (m:mmultiscripts (m:mi "X") (m:mi "b") (m:none)
                                                  (m:mprescripts) (m:mi "a") (m:none))))
          '(m:oMath (m:sPre (m:sub (m:r (m:t "a"))) (m:sup)
                            (m:e (m:sSub (m:e (m:r (m:t "X")))
                                         (m:sub (m:r (m:t "b"))))))))
  ;; the spaces at the ends of a text of a formula are kept
  (check= (mathml->omml '(m:math (m:mtext " if ")))
          '(m:oMath (m:r (m:rPr (m:nor)) (m:t (@ (xml:space "preserve")) " if "))))
  ;; a formula of an OpenDocument text, as LibreOffice shows it well: a
  ;; word which is no function is text, a relation has something at each
  ;; side, a fraction without a bar is a table, a hat is the circumflex
  (let* ((odt (serialize-odt-document
                `(office (p (math (@ (form "sxml"))
                                  (m:math (m:mrow (m:mo "=") (m:mi "sup") (m:mi "sin")
                                                  (m:mfrac (@ (linethickness "0"))
                                                           (m:mi "n") (m:mi "k"))
                                                  (m:mover (m:mi "x") (m:mo "^")))))))))
         (m (export-entry odt "Formula-1/content.xml")))
    (check-true (export-has? m "<mrow><mrow/><mo>=</mo><mtext>sup</mtext><mi>sin</mi>"))
    (check-true (export-has? m "<mtable><mtr><mtd><mi>n</mi></mtd></mtr>"))
    (check-true (export-has? m (string-append "<mo>" (office-utf8 #x2c6) "</mo>")))))

(define (test-export-blocks)
  (check-group "export blocks")
  ;; an appendix is a heading of the first level which is in use
  (check= (of '(document (section* "S") (subsection* "T") (appendix "A")))
          '((p (@ (role "heading") (level "1")) "S")
            (p (@ (role "heading") (level "2")) "T")
            (p (@ (role "heading") (level "1")) "A")))
  ;; the numbers of the rows of several equations, each with its labels
  (check= (of '(eqnarray
                 (document
                   (tformat
                     (table (row (cell "x")
                                 (cell (concat "=y" (htab "5mm")
                                               (with "mode" "text" "(1)") (label "e1"))))
                            (row (cell "z")
                                 (cell (concat "=w" (htab "5mm")
                                               (with "mode" "text" "(2)") (label "e2")))))))))
          '((p (math (@ (display "true") (form "sxml"))
                     (m:math (@ (class "tm-eqarray")) (m:mtable (@ (columnalign "left left"))
 (m:mtr (m:mtd (m:mi "x"))
                                              (m:mtd (m:mrow (m:mo "=") (m:mi "y"))))
                                       (m:mtr (m:mtd (m:mi "z"))
                                              (m:mtd (m:mrow (m:mo "=") (m:mi "w")))))))
               (tab) "(" (seq (@ (name "Equation") (id "Equation1") (labels "e1")) "1")
               ") (" (seq (@ (name "Equation") (id "Equation2") (labels "e2")) "2")
               ")")))
  ;; A label with a space is one bookmark. A link to it whose text is its
  ;; number, or the ? of a number which is not known, is a reference; a
  ;; link with a text of its own keeps it.
  (check= (of '(document
                 (render-theorem "Theorem 1" (document (concat (label "a b") "T")))
                 (concat (hlink "1" "#a b") (hlink "the theorem" "#a b")
                         (hlink "?" "#a b"))))
          '((p (@ (role "theorem"))
               (strong "Theorem " (seq (@ (name "Theorem") (id "Theorem1")
                                          (labels "a_b")) "1") ".")
               " T")
            (p (ref (@ (name "a_b") (kind "seq") (target "Theorem1")) "1")
               (ref (@ (name "a_b")) "the theorem")
               (ref (@ (name "a_b") (kind "seq") (target "Theorem1")) "?"))))
  ;; formats without a table
  (check= (of '(document (tformat) "x")) '((p "x")))
  ;; the table of contents, with its title when the editor expanded it
  (check= (of '(document (render-table-of-contents "Contents" (document "entries"))
                         (table-of-contents "toc" (document ""))))
          '((toc "Contents") (toc)))
  ;; many formats of a cell each do not take the time of all the cells
  (let* ((n 300)
         (t `(tabular
               (tformat
                 ,@(map (lambda (i)
                          `(cwith ,(number->string (+ i 1)) ,(number->string (+ i 1))
                                  "1" "1" "cell-halign" "r"))
                        (iota n))
                 (table ,@(map (lambda (i) '(row (cell "a") (cell "b"))) (iota n))))))
         (start (texmacs-time))
         (r (of t)))
    (check= (length (ox-children (car r))) n)
    (check= (ox-attr (car (ox-children (cAr (ox-children (car r))))) 'align) "right")
    (check-true (< (- (texmacs-time) start) 2000))))

(define (test-export-writers)
  (check-group "export writers")
  ;; the characters which XML does not allow, and the bytes which are no
  ;; character, are the replacement character
  (check= (ox-serialize-element
            `(a ,(string-append "x" (bytes #xed #xa0 #x80 #xef #xbf #xbe #xff #xc3 #xa9)
                                "y")))
          (string-append "<a>x" (office-utf8 #xfffd) (office-utf8 #xfffd)
                         (office-utf8 #xfffd) (office-utf8 #xe9) "y</a>"))
  (check= (office-uri "http://x.y/a b?q=\"1\"#f") "http://x.y/a%20b?q=%221%22#f")
  (let* ((image '(image (@ (name "pic.png") (data "PNGDATA") (width "2cm")
                           (height "1cm"))))
         (tree `(office
                  (toc "Contents")
                  (p "a" (note (p ,image (link (@ (href "http://x.y/a b")) "l")))
                     ,image ,image (link (@ (href "http://x.y/a b")) "m"))
                  (list (@ (kind "number"))
                        (item (p "one") (list (@ (kind "bullet")) (item (p "b")))
                              (list (@ (kind "number")) (item (p "n"))))
                        (item (table (row (cell (p "t")))) (p "after"))
                        (item (p "three")))
                  (table (row (cell (p "x"))))
                  (table (row (cell (p "y"))))
                  (p (strong (seq (@ (name ,(string-append (office-utf8 #x422) " 1"))
                                     (id "s1")) "1")))))
         (docx (serialize-docx-document tree))
         (odt (serialize-odt-document tree))
         (text (export-entry docx "word/document.xml"))
         (notes (export-entry docx "word/footnotes.xml"))
         (rels (export-entry docx "word/_rels/footnotes.xml.rels"))
         (content (export-entry odt "content.xml")))
    ;; the notes have their own relations, for their links and images;
    ;; the targets of the links are addresses
    (check-true (export-has? rels "Target=\"media/image1.png\""))
    (check-true (export-has? rels "Target=\"http://x.y/a%20b\""))
    (check-true (export-has? notes "r:embed=\"rId1\""))
    (check-true (export-has? notes "<w:hyperlink r:id=\"rId2\">"))
    (check-true (export-has? content "xlink:href=\"http://x.y/a%20b\""))
    ;; an image which is used several times is stored once
    (check= (list-filter (archive-names docx)
                         (lambda (n) (string-starts? n "word/media/")))
            '("word/media/image1.png"))
    (check= (length (list-filter (archive-names odt)
                                 (lambda (n) (string-starts? n "Pictures/"))))
            1)
    ;; an empty paragraph between two tables of a Word document
    (check-true (export-has? text "</w:tbl><w:p/><w:tbl>"))
    ;; the name of a sequence keeps the letters which are not of ASCII
    (check-true (export-has? text (string-append " SEQ " (office-utf8 #x422) " ")))
    ;; the table of contents: its title and a field, or an index
    (check-true (export-has? text "<w:pStyle w:val=\"TOCHeading\"/>"))
    (check-true (export-has? text " TOC "))
    (check-true (export-has? content "<text:index-title-template>Contents<"))
    ;; in an OpenDocument text each list has the style of its kind, and a
    ;; list ends before a table of one of its items and goes on after it
    (check-true (export-has? content "<text:list text:style-name=\"L1\"><text:list-item>"))
    (check-true (export-has? content "<text:list text:style-name=\"L2\"><text:list-item>"))
    (check-true (export-has? content "</table:table><text:list text:style-name=\"L1\" text:continue-numbering=\"true\"><text:list-header>"))
    ;; both files are read back: the table of the list, the tables apart
    (let ((back (cdr (parse-odt-document odt))))
      (check= (length (list-filter back (lambda (x) (func? x 'table)))) 3))
    (let ((back (cdr (parse-docx-document docx))))
      (check= (length (list-filter back (lambda (x) (func? x 'table)))) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export: the fixes of the audit of the writers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (export-count s what)
  ;; the number of times what is in s
  (let loop ((i 0) (n 0))
    (with j (string-search-forwards what i s)
      (if (< j 0) n (loop (+ j (max 1 (string-length what))) (+ n 1))))))

(define (test-export-audit)
  (check-group "export audit")
  ;; no section in a cell, a note or other columns: their blocks only
  (let* ((docx (serialize-docx-document
                 '(office (table (row (cell (columns (@ (count "2")) (p "c")))))
                          (p "a" (note (columns (@ (count "2")) (p "n"))))
                          (columns (@ (count "2")) (p "o")
                                   (columns (@ (count "3")) (p "i"))))))
         (text (export-entry docx "word/document.xml"))
         (notes (export-entry docx "word/footnotes.xml")))
    (check= (export-count text "<w:sectPr>") 3)
    (check-false (export-has? text "w:num=\"3\""))
    (check-false (export-has? notes "sectPr"))
    (check-true (export-has? text "<w:t xml:space=\"preserve\">c</w:t>")))
  ;; a note which starts with a table has its mark in a paragraph before
  ;; it and a paragraph after it; each paragraph keeps its properties
  ;; with the style of the notes; a comment ends with a paragraph too
  (let* ((docx (serialize-docx-document
                 '(office (p "a" (note (table (row (cell (p "t")))))
                             (note (list (@ (kind "bullet")) (item (p "x")) (item (p "y"))))
                             (comment (@ (author "A")) (table (row (cell (p "c")))))))))
         (notes (export-entry docx "word/footnotes.xml"))
         (comments (export-entry docx "word/comments.xml")))
    (check-true (export-has? notes "<w:footnote w:id=\"1\"><w:p><w:pPr><w:pStyle w:val=\"FootnoteText\"/></w:pPr><w:r><w:rPr><w:rStyle w:val=\"FootnoteReference\"/></w:rPr><w:footnoteRef/>"))
    (check-true (export-has? notes "</w:tbl><w:p><w:pPr><w:pStyle w:val=\"FootnoteText\"/></w:pPr></w:p></w:footnote>"))
    (check= (export-count notes "<w:pStyle w:val=\"FootnoteText\"/><w:numPr>") 2)
    (check-true (export-has? comments "</w:tbl><w:p/></w:comment>")))
  ;; the ligatures of the text, not those of code
  (check= (of "a--b---c ``q'' `s' don't")
          `((p ,(string-append "a" (office-utf8 #x2013) "b" (office-utf8 #x2014) "c "
                               (office-utf8 #x201c) "q" (office-utf8 #x201d) " "
                               (office-utf8 #x2018) "s" (office-utf8 #x2019) " don"
                               (office-utf8 #x2019) "t"))))
  (check= (of '(verbatim "x--y ''")) '((p (code "x--y ''"))))
  ;; a link or a reference inside a link is its text
  (let* ((tree '(office (p (link (@ (href "http://a")) "x"
                                 (link (@ (href "http://b")) "y")
                                 (ref (@ (name "L") (kind "seq")) "1")))))
         (text (export-entry (serialize-docx-document tree) "word/document.xml"))
         (content (export-entry (serialize-odt-document tree) "content.xml")))
    (check= (export-count text "<w:hyperlink") 1)
    (check-false (export-has? text " REF "))
    (check= (export-count content "<text:a ") 1)
    (check-false (export-has? content "bookmark-ref")))
  ;; a line through a formula, the sides of a box
  (let ((on (lambda (tag) `(,tag (@ (m:val "1"))))))
    (check= (mathml->omml '(m:math (m:menclose (@ (notation "updiagonalstrike")) (m:mo "="))))
            `(m:oMath (m:borderBox (m:borderBoxPr ,(on 'm:hideTop) ,(on 'm:hideBot)
                                                  ,(on 'm:hideLeft) ,(on 'm:hideRight)
                                                  ,(on 'm:strikeBLTR))
                                   (m:e (m:r (m:t "="))))))
    (check= (mathml->omml '(m:math (m:menclose (@ (notation "box")) (m:mi "x"))))
            '(m:oMath (m:borderBox (m:e (m:r (m:t "x"))))))
    (check= (mathml->omml '(m:math (m:menclose (@ (notation "bottom")) (m:mi "x"))))
            `(m:oMath (m:borderBox (m:borderBoxPr ,(on 'm:hideTop) ,(on 'm:hideLeft)
                                                  ,(on 'm:hideRight))
                                   (m:e (m:r (m:t "x")))))))
  ;; a note beside the line is its text in the line, without its place
  (check= (of '(concat "A" (line-note (tformat (table (row (cell (document "note")))))
                                      "0cm" "0cm") " B"))
          '((p "A[note] B")))
  ;; a label in a note is not the label of the number before it
  (check= (of '(document (render-theorem "Theorem 1" (document "T"))
                         (concat "x" (footnote (concat "n" (label "fn"))) (hlink "1" "#fn"))))
          '((p (@ (role "theorem"))
               (strong "Theorem " (seq (@ (name "Theorem") (id "Theorem1")) "1") ".") " T")
            (p "x" (note (p "n" (bookmark (@ (name "fn"))))) (ref (@ (name "fn")) "1"))))
  ;; a reference to a page is a field of the page
  (check= (of '(pageref "t")) '((p (ref (@ (name "t") (kind "page")) "?"))))
  (let ((tree '(office (p (bookmark (@ (name "t"))) "x" (ref (@ (name "t") (kind "page")) "3")))))
    (check-true (export-has? (export-entry (serialize-docx-document tree) "word/document.xml")
                             "<w:instrText xml:space=\"preserve\"> PAGEREF t \\h </w:instrText>"))
    (check-true (export-has? (export-entry (serialize-odt-document tree) "content.xml")
                             "<text:bookmark-ref text:reference-format=\"page\" text:ref-name=\"t\">3</text:bookmark-ref>")))
  ;; a note in a note is its text in parentheses
  (let* ((tree '(office (p "a" (note (p "n" (note (p "inner")))))))
         (docx (serialize-docx-document tree))
         (content (export-entry (serialize-odt-document tree) "content.xml")))
    (check= (export-count (export-entry docx "word/document.xml") "<w:footnoteReference") 1)
    (check-true (export-has? (export-entry docx "word/footnotes.xml") ">inner<"))
    (check= (export-count content "<text:note ") 1)
    (check-true (export-has? content "(inner)")))
  ;; a bookmark is in the file once; tabs and line breaks are elements
  (let* ((tree '(office (p (bookmark (@ (name "d"))) "a\tb\nc" (bookmark (@ (name "d"))))))
         (text (export-entry (serialize-docx-document tree) "word/document.xml"))
         (content (export-entry (serialize-odt-document tree) "content.xml")))
    (check= (export-count text "w:name=\"d\"") 1)
    (check= (export-count content "text:name=\"d\"") 1)
    (check-true (export-has? text "<w:t xml:space=\"preserve\">a</w:t></w:r><w:r><w:tab/></w:r>"))
    (check-true (export-has? text "<w:r><w:br/></w:r><w:r><w:t xml:space=\"preserve\">c</w:t>")))
  ;; the format of an image is the one of its bytes; an image without a
  ;; name has one
  (let* ((jpg (bytes #xff #xd8 #xff #xc0 0 17 8 0 16 0 32 3 1 34 0 2 17 1 3 17 1))
         (png (bytes 137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 32 0 0 0 16 8 6 0))
         (name (lambda (data file)
                 (with r (of `(image (tuple (raw-data ,data) ,file) "1cm" "1cm" "" ""))
                   (ox-attr (cadr (car r)) 'name)))))
    (check= (name jpg "x.png") "x.jpg")
    (check= (name png "") "image.png")
    (check= (name png "a.PNG") "a.PNG"))
  ;; the background of a text is a highlight
  (check= (of '(with "bg-color" "yellow" "x")) '((p (mark (@ (value "#ffff00")) "x"))))
  ;; the named styles which the file does not have are in its styles
  (let* ((tree '(office (p (@ (style "MyNote")) "x" (span (@ (style "Key")) "k"))
                        (p (@ (style "My Note")) "y")))
         (styles (export-entry (serialize-docx-document tree) "word/styles.xml"))
         (odt (serialize-odt-document tree))
         (ostyles (export-entry odt "styles.xml")))
    (check-true (export-has? styles "<w:style w:type=\"paragraph\" w:styleId=\"MyNote\"><w:name w:val=\"MyNote\"/><w:basedOn w:val=\"Normal\"/>"))
    (check-true (export-has? styles "<w:style w:type=\"character\" w:styleId=\"Key\">"))
    (check-true (export-has? (export-entry odt "content.xml") "text:style-name=\"My_20_Note\""))
    (check-true (export-has? ostyles "<style:style style:name=\"My_20_Note\" style:family=\"paragraph\" style:display-name=\"My Note\""))
    (check-true (export-has? ostyles "<style:style style:name=\"Key\" style:family=\"text\"/>")))
  ;; the named styles of the document have its look in the files of the
  ;; other format: the properties of its nodes style, and of its headings
  (let* ((tree '(office (styles (style (@ (name "MyNote") (family "paragraph") (em "true")
                                          (color "#c00000") (indent-left "28.35")
                                          (space-before "6") (align "center")
                                          (font-name "Georgia") (size "11")))
                                (style (@ (name "Key") (family "text") (strong "true")
                                          (underline "true")))
                                (style (@ (name "H1") (family "paragraph") (level "1")
                                          (size "16") (color "#2f5496"))))
                        (p (@ (style "MyNote")) "x" (span (@ (style "Key")) "k"))
                        (p (@ (role "heading") (level "1")) "H")))
         (styles (export-entry (serialize-docx-document tree) "word/styles.xml"))
         (ostyles (export-entry (serialize-odt-document tree) "styles.xml")))
    (check-true (export-has? styles "<w:style w:type=\"paragraph\" w:styleId=\"MyNote\"><w:name w:val=\"MyNote\"/><w:basedOn w:val=\"Normal\"/><w:qFormat/><w:pPr><w:spacing w:before=\"120\"/><w:ind w:left=\"567\"/><w:jc w:val=\"center\"/></w:pPr><w:rPr><w:rFonts w:ascii=\"Georgia\" w:hAnsi=\"Georgia\" w:cs=\"Georgia\"/><w:i/><w:iCs/><w:color w:val=\"C00000\"/><w:sz w:val=\"22\"/><w:szCs w:val=\"22\"/></w:rPr></w:style>"))
    (check-true (export-has? styles "<w:style w:type=\"character\" w:styleId=\"Key\"><w:name w:val=\"Key\"/><w:qFormat/><w:rPr><w:b/><w:bCs/><w:u w:val=\"single\"/></w:rPr></w:style>"))
    (check-true (export-has? styles "<w:color w:val=\"2F5496\"/><w:sz w:val=\"32\"/><w:szCs w:val=\"32\"/></w:rPr></w:style>"))
    (check-true (export-has? ostyles "<style:style style:name=\"MyNote\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"text\"><style:paragraph-properties fo:margin-top=\"6pt\" fo:margin-left=\"28.35pt\" fo:text-align=\"center\"/><style:text-properties fo:font-family=\"'Georgia'\" fo:font-size=\"11pt\""))
    (check-true (export-has? ostyles "fo:color=\"#c00000\"/></style:style>"))
    (check-true (export-has? ostyles "<style:style style:name=\"Key\" style:family=\"text\"><style:text-properties fo:font-weight=\"bold\""))
    (check-true (export-has? ostyles "style:default-outline-level=\"1\" style:class=\"text\"><style:text-properties fo:font-size=\"16pt\"")))
  ;; the line spacing of a named style, and its paragraph kept with the
  ;; next one
  (let* ((tree '(office (styles (style (@ (name "Keep") (family "paragraph")
                                          (line-spacing "1.5") (keep-with-next "true")))
                                (style (@ (name "H2") (family "paragraph") (level "2")
                                          (keep-with-next "true"))))
                        (p (@ (style "Keep")) "x")
                        (p (@ (role "heading") (level "2")) "H")))
         (styles (export-entry (serialize-docx-document tree) "word/styles.xml"))
         (ostyles (export-entry (serialize-odt-document tree) "styles.xml")))
    (check-true (export-has? styles "w:styleId=\"Keep\"><w:name w:val=\"Keep\"/><w:basedOn w:val=\"Normal\"/><w:qFormat/><w:pPr><w:keepNext/><w:spacing w:line=\"360\" w:lineRule=\"auto\"/></w:pPr>"))
    (check= (export-count styles "<w:keepNext/><w:keepNext/>") 0)
    (check-true (export-has? ostyles "style:name=\"Keep\" style:family=\"paragraph\" style:parent-style-name=\"Standard\" style:class=\"text\"><style:paragraph-properties fo:line-height=\"150%\" fo:keep-with-next=\"always\"/>")))
  ;; the styles of the numbers of the fields of a file of OpenDocument
  ;; which it was read from go with them; a reference to a style which is
  ;; not there is left out
  (let* ((ns (string-append "xmlns:office='urn:oasis:names:tc:opendocument:xmlns:office:1.0' "
                            "xmlns:style='urn:oasis:names:tc:opendocument:xmlns:style:1.0' "
                            "xmlns:number='urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0'"))
         (pack (zip-pack '("mimetype" "content.xml" "styles.xml")
                         (list "application/vnd.oasis.opendocument.text"
                               (string-append "<office:document-content " ns "><office:automatic-styles><number:date-style style:name='N1'><number:day/></number:date-style><number:date-style style:name='N9'/></office:automatic-styles><office:body><office:text/></office:body></office:document-content>")
                               (string-append "<office:document-styles " ns "><office:styles/></office:document-styles>"))))
         (fields '(p (field (@ (xml "<text:date style:data-style-name=\"N1\"/>")) "today")
                     (field (@ (xml "<text:time style:data-style-name=\"N2\"/>")) "now")))
         (with-pack (export-entry (serialize-odt-document
                                    `(office (package (@ (format "odt") (data ,pack))) ,fields))
                                  "content.xml"))
         (without (export-entry (serialize-odt-document `(office ,fields)) "content.xml")))
    (check-true (export-has? with-pack "<number:date-style style:name=\"N1\"><number:day/></number:date-style>"))
    (check-false (export-has? with-pack "N9"))
    (check-true (export-has? with-pack "<text:date style:data-style-name=\"N1\">"))
    (check-false (export-has? with-pack "N2"))
    (check-false (export-has? without "data-style-name")))
  ;; a style of OpenDocument in Word has its name with its spaces
  (check-true (export-has? (export-entry (serialize-docx-document
                                           '(office (p (@ (style "My_20_Note")) "x")))
                                         "word/styles.xml")
                           "w:styleId=\"My_20_Note\"><w:name w:val=\"My Note\"/>"))
  ;; the macros of the named styles of a document are its nodes style
  (check= (caddr (texmacs->office
                  '(document (TeXmacs "2.1") (style "office")
                             (body (document (office-par "MyNote" "x")))
                             (initial (collection
                                        (associate "office-par-MyNote"
                                          (macro "body"
                                            (with "par-mode" "center" "par-left" "28.35pt"
                                              (surround (vspace* "6pt") (vspace "0pt")
                                                (with "color" "#c00000" "font-shape" "italic"
                                                  (arg "body")))))))))
                  '()))
          '(styles (style (@ (name "MyNote") (family "paragraph") (align "center")
                             (indent-left "28.35") (space-before "6") (space-after "0")
                             (color "#c00000") (em "true")))))
  ;; a comment in a note: at the mark of the note for Word (LibreOffice
  ;; does not read a comment in a note of Word), in the note of
  ;; OpenDocument
  (let* ((tree '(office (p "a" (note (p "n" (comment (@ (author "A")) (p "c")))))))
         (docx (serialize-docx-document tree))
         (odt (serialize-odt-document tree)))
    (check-false (export-has? (export-entry docx "word/footnotes.xml") "comment"))
    (check-true (export-has? (export-entry docx "word/document.xml")
                             "<w:footnoteReference w:id=\"1\"/></w:r><w:commentRangeStart w:id=\"0\"/><w:commentRangeEnd w:id=\"0\"/><w:r><w:commentReference w:id=\"0\"/></w:r>"))
    (check-true (export-has? (export-entry docx "word/comments.xml")
                             "<w:comment w:id=\"0\" w:author=\"A\"><w:p><w:r><w:t xml:space=\"preserve\">c</w:t>"))
    (check-true (export-has? (export-entry odt "content.xml")
                             "<text:note-body><text:p text:style-name=\"Footnote\">n<office:annotation><dc:creator>A</dc:creator>"))
    (check= (no-setup (cdr (parse-odt-document odt)))
            '((p "a" (note (p "n" (comment (@ (author "A")) (p "c"))))))))
  ;; a bibliography which was not expanded: its title, its entries with
  ;; their labels apart
  (check= (of '(bibliography "bib" "tm-plain" "refs"
                 (document (bib-list "2" (document (concat (bibitem* "1") (label "bib-k") "Knuth.")
                                                  (concat (bibitem "l") "Lamport."))))))
          '((p (@ (role "heading") (level "1")) "Bibliography")
            (p (@ (role "bibitem")) "[" (seq (@ (name "Reference") (id "Reference1")
                                                (labels "bib-k")) "1") "] Knuth.")
            (p (@ (role "bibitem")) "[" (seq (@ (name "Reference") (id "Reference2")
                                                (labels "bib-l")) "2") "] Lamport.")))
  ;; an overline: in OpenDocument, and read back; Word has none (the text)
  (check= (of '(concat "a" (overline "o"))) '((p "a" (overline "o"))))
  (let* ((tree '(office (p "a " (overline "o"))))
         (odt (serialize-odt-document tree)))
    (check-true (export-has? (export-entry odt "content.xml")
                             "<style:text-properties style:text-overline-style=\"solid\" style:text-overline-width=\"auto\" style:text-overline-color=\"font-color\"/>"))
    (check-true (export-has? (export-entry (serialize-docx-document tree) "word/document.xml")
                             "<w:t xml:space=\"preserve\">o</w:t>"))
    (check-true (in? '(concat "a " (overline "o"))
                     (cdr (cadr (cadr (convert odt "odt-document" "texmacs-stree"))))))))

;; The export, after the second audit: XML whose prefixes are declared,
;; the named styles of the file of the other format, odd macros, the names
;; of the styles of Word, the addresses of pictures, fields of paragraphs.

(define (export-undeclared x scope)
  ;; the prefixes of the names of the element x of parse-xml which are not
  ;; declared (scope: those declared around it)
  (if (not (and (pair? x) (symbol? (car x)))) '()
      (let* ((attrs (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cdadr x) '()))
             (body (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cddr x) (cdr x)))
             (prefix (lambda (sym)
                       (let* ((s (symbol->string sym))
                              (i (string-search-forwards ":" 0 s)))
                         (and (> i 0) (substring s 0 i)))))
             (scope (append (list-filter (map (lambda (a) (and (pair? a) (symbol? (car a))
                                                                (== (prefix (car a)) "xmlns")
                                                                (substring (symbol->string (car a)) 6
                                                                           (string-length (symbol->string (car a))))))
                                              attrs)
                                         identity)
                            scope))
             (bad (lambda (sym) (with p (prefix sym)
                                  (and p (not (in? p (cons* "xml" "xmlns" scope))) (list p))))))
        (append (if (in? (car x) '(*TOP* *PI*)) '() (or (bad (car x)) '()))
                (append-map (lambda (a) (or (and (pair? a) (symbol? (car a)) (bad (car a))) '()))
                            attrs)
                (append-map (lambda (y) (export-undeclared y scope)) body)))))

(define (export-xml-ok? z)
  ;; whether the parts of XML of the archive z are read, with their
  ;; prefixes declared
  (with a (office-archive z)
    (list-and
      (map (lambda (kv)
             (or (not (or (string-ends? (car kv) ".xml") (string-ends? (car kv) ".rels")))
                 (with x (catch #t (lambda () (parse-xml (cdr kv))) (lambda args #f))
                   (and x (null? (export-undeclared x '()))))))
           (ahash-table->list a)))))

(define (test-export-audit-2)
  (check-group "export audit 2")
  ;; 1. the styles of numbers kept from the file are in declared XML
  (let* ((ns (string-append "xmlns:office='urn:oasis:names:tc:opendocument:xmlns:office:1.0' "
                            "xmlns:style='urn:oasis:names:tc:opendocument:xmlns:style:1.0' "
                            "xmlns:number='urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0'"))
         (pack (zip-pack '("mimetype" "content.xml" "styles.xml")
                         (list "application/vnd.oasis.opendocument.text"
                               (string-append "<office:document-content " ns "><office:automatic-styles><number:date-style style:name='N1'><number:day/></number:date-style></office:automatic-styles><office:body><office:text/></office:body></office:document-content>")
                               (string-append "<office:document-styles " ns "><office:styles/></office:document-styles>"))))
         (z (serialize-odt-document
              `(office (package (@ (format "odt") (data ,pack)))
                       (p (field (@ (xml "<text:date style:data-style-name=\"N1\"/>")) "today")
                          (field (@ (xml "<foo:bar xmlns:foo=\"urn:x\"/>")) "odd"))))))
    (check-true (export-has? (export-entry z "content.xml") "<number:date-style style:name=\"N1\">"))
    (check-true (export-xml-ok? z))
    (check-true (export-has? (export-entry z "content.xml") "odd"))
    (check-false (export-has? (export-entry z "content.xml") "foo:bar")))
  (for (tree '((office (p "a" (note (p "n" (comment (p "c"))))))
               (office (p (image (@ (name "https://a.b/c d.png")))))))
    (check-true (export-xml-ok? (serialize-docx-document tree)))
    (check-true (export-xml-ok? (serialize-odt-document tree))))
  ;; 2. 3. a style of the macros of the document, with what only the file
  ;; of the other format says (its name, the paragraph kept with the next
  ;; one); the file is not read again for a file of its format
  (check= (office-merged-styles
            '((style (@ (name "A") (family "paragraph") (strong "true"))))
            '((style (@ (name "A") (family "paragraph") (display "A b") (em "true")
                        (keep-with-next "true")))
              (style (@ (name "A") (family "text") (em "true")))))
          '((style (@ (name "A") (family "paragraph") (strong "true") (display "A b")
                      (keep-with-next "true")))
            (style (@ (name "A") (family "text") (em "true")))))
  (let* ((z (zip-pack '("word/document.xml" "word/styles.xml")
                      (list (string-append "<w:document" docx-ns "><w:body>"
                                           (wp (wstyle "K") (wr "x")) (wp (wr "y")) (wp (wr "z"))
                                           "</w:body></w:document>")
                            (string-append "<w:styles" docx-ns ">"
                                           "<w:style w:type='paragraph' w:styleId='K'>"
                                           "<w:name w:val='Keep Me'/><w:pPr><w:keepNext/>"
                                           "<w:jc w:val='center'/></w:pPr>"
                                           "</w:style></w:styles>"))))
         (t (convert z "docx-document" "texmacs-stree"))
         (office (convert t "texmacs-stree" "office-stree"))
         (styles (append-map cdr (list-filter (cdr office) (lambda (y) (func? y 'styles)))))
         (ostyles (export-entry (convert t "texmacs-stree" "odt-document") "styles.xml")))
    ;; (the styles of the macros only: the file is not read again)
    (check-false (list-find styles (lambda (st) (ox-attr st 'display))))
    (check-true (export-has? ostyles "style:display-name=\"Keep Me\""))
    (check-true (export-has? ostyles "fo:keep-with-next=\"always\""))
    (check-true (export-has? ostyles "fo:text-align=\"center\""))
    (with l (office-package-styles "docx" z)
      (check= (ox-attr (car l) 'display) "Keep Me")
      (check= (ox-attr (car l) 'keep-with-next) "true")))
  ;; 4. a macro which cannot be read: a style without its look
  (let* ((doc '(document (body (document (office-par "Odd" "a")))
                         (style "office")
                         (initial (collection
                                    (associate "office-par-Odd"
                                               (macro "body" (surround (vspace*) "" (arg "body"))))))))
         (styles (export-entry (convert doc "texmacs-stree" "docx-document") "word/styles.xml")))
    (check-true (export-has? styles "w:styleId=\"Odd\"")))
  ;; a line over the text of a named style, in OpenDocument
  (let* ((doc '(document (body (document (concat "a " (office-span "Over" "b"))))
                         (style "office")
                         (initial (collection
                                    (associate "office-span-Over"
                                               (macro "body" (overline (arg "body"))))))))
         (styles (export-entry (convert doc "texmacs-stree" "odt-document") "styles.xml")))
    (check-true (export-has? styles "style:text-overline-style=\"solid\"")))
  ;; 5. the name of a style of OpenDocument in Word is its name to show
  (let* ((z (odt-file "<text:p text:style-name='My_20_Note'>x</text:p><text:p>y</text:p><text:p>z</text:p>"
                      (string-append "<office:styles><style:style style:name='My_20_Note'"
                                     " style:display-name='My Note' style:family='paragraph'>"
                                     "<style:text-properties fo:font-style='italic'/>"
                                     "</style:style></office:styles>")
                      ""))
         (t (convert z "odt-document" "texmacs-stree"))
         (styles (export-entry (convert t "texmacs-stree" "docx-document") "word/styles.xml")))
    (check-true (export-has? styles "<w:name w:val=\"My Note\"/>"))
    (check= (office-odf-name "A_20__26__20_B") "A & B")
    (check= (office-odf-name "x_y_z") "x_y_z"))
  ;; 6. 7. a paragraph and a text of the same name have two styles; a name
  ;; which Word has is another style, which does not change Word's
  (let* ((z (serialize-docx-document
              '(office (styles (style (@ (name "N") (family "paragraph") (em "true")))
                               (style (@ (name "N") (family "text") (strong "true"))))
                       (p (@ (style "N")) "a" (span (@ (style "N")) "b"))
                       (p (@ (style "Heading_20_1")) "h")
                       (p (@ (style "normal")) "n"))))
         (text (export-entry z "word/document.xml"))
         (styles (export-entry z "word/styles.xml")))
    (check-true (export-has? text "<w:pStyle w:val=\"N\"/>"))
    (check-true (export-has? text "<w:rStyle w:val=\"NChar\"/>"))
    (check-true (export-has? styles "<w:style w:type=\"character\" w:styleId=\"NChar\"><w:name w:val=\"N Char\"/>"))
    (check-true (export-has? text "<w:pStyle w:val=\"Heading_20_1\"/>"))
    (check-true (export-has? styles "<w:name w:val=\"Heading 1 Doc\"/>"))
    (check-false (export-has? text "<w:pStyle w:val=\"Heading1\"/>"))
    (check-true (export-has? text "<w:pStyle w:val=\"normalDoc\"/>"))
    (check-false (export-has? styles "w:styleId=\"normal\"")))
  ;; 8. 9. the address of a picture on the web, with %XX, 5cm by 3cm in
  ;; both formats; a field over paragraphs without them; one of
  ;; OpenDocument
  (let* ((tree '(office (p (image (@ (name "https://a.b/c d.png"))))
                        (field-blocks (@ (code " ADDIN X ")))
                        (field-blocks (@ (xml "<text:bibliography/>")) (p "b"))))
         (z (serialize-docx-document tree))
         (o (serialize-odt-document tree))
         (text (export-entry z "word/document.xml")))
    (check-true (export-has? (export-entry z "word/_rels/document.xml.rels")
                             "Target=\"https://a.b/c%20d.png\""))
    (check-true (export-has? (export-entry o "content.xml") "xlink:href=\"https://a.b/c%20d.png\""))
    (check-true (export-has? text "<wp:extent cx=\"1800000\" cy=\"1080000\"/>"))
    (check-true (export-has? (export-entry o "content.xml") "svg:width=\"5cm\" svg:height=\"3cm\""))
    (check-true (export-has? text " ADDIN X </w:instrText></w:r><w:r><w:fldChar w:fldCharType=\"separate\"/></w:r><w:r><w:fldChar w:fldCharType=\"end\"/>"))
    (check-false (export-has? text "bibliography"))
    (check-true (export-has? text ">b</w:t>")))
  (check= (convert '(document (office-field "<text:bibliography/>" (document "a" "b")))
                   "texmacs-stree" "office-stree")
          '(office (field-blocks (@ (xml "<text:bibliography/>")) (p "a") (p "b"))))
  ;; a picture which the file linked to (linked) is linked to again; a
  ;; style with a line over its text; a field around a list
  (let* ((tree '(office (styles (style (@ (name "O") (family "text") (overline "true"))))
                        (p (image (@ (name "pics/a b.png") (linked "true"))) (span (@ (style "O")) "o"))
                        (field-blocks (@ (code " ADDIN L "))
                                      (list (@ (kind "bullet")) (item (p "i")) (item (p "j"))))))
         (z (serialize-docx-document tree))
         (o (serialize-odt-document tree))
         (text (export-entry z "word/document.xml")))
    (check-true (export-has? (export-entry z "word/_rels/document.xml.rels")
                             "Target=\"pics/a%20b.png\" TargetMode=\"External\""))
    (check-true (export-has? text "r:link="))
    (check-false (list-find (map car (ahash-table->list (office-archive z)))
                            (lambda (n) (string-starts? n "word/media/"))))
    (check-true (export-has? (export-entry o "content.xml") "xlink:href=\"pics/a%20b.png\""))
    (check-true (export-has? (export-entry o "styles.xml") "style:text-overline-style=\"solid\""))
    (check-true (export-xml-ok? z))
    (check-true (export-xml-ok? o))
    (check-true (export-has? text "<w:fldChar w:fldCharType=\"separate\"/></w:r><w:r><w:t xml:space=\"preserve\">i</w:t>"))
    (check-true (export-has? text ">j</w:t></w:r><w:r><w:fldChar w:fldCharType=\"end\"/></w:r></w:p>"))
    (check= (export-count text "fldCharType=\"begin\"") 1)))

;; The export, after the third audit: the styles of the document which are
;; built-in styles keep their look, two of them are two styles, the names
;; of the styles of TeXmacs in OpenDocument, the names to show of a
;; paragraph and a text of one name, the addresses of linked pictures.

(define (export-style s id)
  ;; the element w:style of this id in the text s of word/styles.xml, or ""
  (let* ((i (string-search-forwards (string-append "w:styleId=\"" id "\"") 0 s))
         (j (if (< i 0) -1 (string-search-forwards "</w:style>" i s))))
    (if (or (< i 0) (< j 0)) "" (substring s i j))))

(define (export-odt-style s family name)
  ;; the element style:style of this family and name in the text s, up to
  ;; the next style, or ""
  (let* ((i (string-search-forwards (string-append "<style:style style:name=\"" name
                                                   "\" style:family=\"" family "\"")
                                    0 s))
         (j (if (< i 0) -1 (string-search-forwards "<style:style " (+ i 1) s))))
    (cond ((< i 0) "")
          ((< j 0) (substring s i (string-length s)))
          (else (substring s i j)))))

(define (test-export-audit-3)
  (check-group "export audit 3")
  ;; 1. 2. a style of the document of the name of a built-in style of Word
  ;; is a style of its own, with its look, and the built-in style keeps
  ;; its look; two such styles are two styles; a style used twice is one
  ;; style
  (let* ((tree '(office (setup (@ (body-style "normal")))
                        (styles (style (@ (name "normal") (family "paragraph") (em "true")
                                          (color "#0000ff")))
                                (style (@ (name "title") (family "paragraph") (color "#ff0000")
                                          (size "30")))
                                (style (@ (name "heading 1") (family "paragraph")
                                          (color "#00ff00")))
                                (style (@ (name "Heading1") (family "paragraph")
                                          (color "#0000ff")))
                                (style (@ (name "Caption") (family "paragraph")
                                          (color "#ff0000")))
                                (style (@ (name "Q") (family "paragraph") (strong "true"))))
                        (p "body")
                        (p (@ (style "title")) "t")
                        (p (@ (style "heading 1")) "h")
                        (p (@ (style "Heading1")) "H")
                        (p (@ (style "Caption")) "c")
                        (p (@ (style "Q")) "q1")
                        (p (@ (style "Q")) "q2")))
         (z (serialize-docx-document tree))
         (text (export-entry z "word/document.xml"))
         (styles (export-entry z "word/styles.xml")))
    (check-true (export-has? text "<w:pStyle w:val=\"normalDoc\"/>"))
    (check-true (export-has? (export-style styles "normalDoc") "<w:i/>"))
    (check-true (export-has? (export-style styles "normalDoc") "<w:color w:val=\"0000FF\"/>"))
    (check-false (export-has? (export-style styles "Normal") "<w:i/>"))
    (check-false (export-has? (export-style styles "Normal") "<w:color"))
    (check-true (export-has? text "<w:pStyle w:val=\"titleDoc\"/>"))
    (check-true (export-has? (export-style styles "titleDoc") "<w:color w:val=\"FF0000\"/>"))
    (check-true (export-has? (export-style styles "titleDoc") "<w:sz w:val=\"60\"/>"))
    (check-false (export-has? (export-style styles "Title") "<w:color"))
    (check-true (export-has? (export-style styles "Title") "<w:b/>"))
    (check-false (export-has? text "<w:pStyle w:val=\"Heading1\"/>"))
    (check-true (export-has? text "<w:pStyle w:val=\"Heading1Doc\"/>"))
    (check-true (export-has? (export-style styles "Heading1Doc") "<w:color w:val=\"0000FF\"/>"))
    (check-true (export-has? styles "<w:name w:val=\"heading 1 Doc\"/>"))
    (check-false (export-has? (export-style styles "Heading1") "<w:color"))
    (check-true (export-has? (export-style styles "Heading1") "<w:outlineLvl w:val=\"0\"/>"))
    (check-true (export-has? (export-style styles "CaptionDoc") "<w:color w:val=\"FF0000\"/>"))
    (check-false (export-has? (export-style styles "Caption") "<w:color"))
    (check= (export-count text "<w:pStyle w:val=\"Q\"/>") 2)
    (check= (export-count styles "w:styleId=\"Q\"") 1)
    (check-true (export-xml-ok? z)))
  ;; 3. a style of the document of the name of a style of TeXmacs in
  ;; OpenDocument (Standard, Caption) is another one, with its look, and the
  ;; style of TeXmacs keeps its look; a style of text of the name of a
  ;; style of paragraphs is a style of text; so is a style of a file of Word
  (let* ((tree '(office (styles (style (@ (name "Standard") (family "paragraph")))
                                (style (@ (name "Caption") (family "paragraph")
                                          (color "#ff0000")))
                                (style (@ (name "Caption") (family "text") (strong "true"))))
                        (p (@ (style "Standard")) "s")
                        (p (@ (style "Caption")) "c" (span (@ (style "Caption")) "t"))))
         (o (serialize-odt-document tree))
         (content (export-entry o "content.xml"))
         (styles (export-entry o "styles.xml")))
    (check-true (export-has? content "<text:p text:style-name=\"Standard_20_Doc\">"))
    (check-true (export-has? content "<text:p text:style-name=\"Caption_20_Doc\">"))
    (check-true (export-has? (export-odt-style styles "paragraph" "Caption_20_Doc") "fo:color=\"#ff0000\""))
    (check-false (export-has? (export-odt-style styles "paragraph" "Caption") "fo:color"))
    (check-true (export-has? (export-odt-style styles "text" "Caption") "fo:font-weight=\"bold\""))
    (check-true (export-xml-ok? o)))
  (let* ((z (zip-pack '("word/document.xml" "word/styles.xml")
                      (list (string-append "<w:document" docx-ns "><w:body>"
                                           (wp (wstyle "Standard") (wr "x")) (wp (wr "y"))
                                           "</w:body></w:document>")
                            (string-append "<w:styles" docx-ns ">"
                                           "<w:style w:type='paragraph' w:styleId='Standard'>"
                                           "<w:name w:val='Standard'/><w:rPr><w:i/></w:rPr>"
                                           "</w:style></w:styles>"))))
         (t (convert z "docx-document" "texmacs-stree"))
         (content (export-entry (convert t "texmacs-stree" "odt-document") "content.xml")))
    (check-true (export-has? content "text:style-name=\"Standard_20_Doc\"")))
  ;; 4. the names to show of a style of paragraphs and one of text of the
  ;; same name in OpenDocument
  (let* ((z (odt-file "<text:p text:style-name='N'>p<text:span text:style-name='N'>c</text:span></text:p>"
                      (string-append "<office:styles>"
                                     "<style:style style:name='N' style:display-name='Para N' style:family='paragraph'>"
                                     "<style:text-properties fo:color='#00aa00'/></style:style>"
                                     "<style:style style:name='N' style:display-name='Char N' style:family='text'>"
                                     "<style:text-properties fo:color='#ff0000'/></style:style>"
                                     "</office:styles>")
                      ""))
         (l (office-package-styles "odt" z))
         (find (lambda (fam) (list-find l (lambda (st) (and (== (ox-attr st 'name) "N")
                                                            (== (ox-attr st 'family) fam)))))))
    (check= (ox-attr (find "paragraph") 'display) "Para N")
    (check= (ox-attr (find "text") 'display) "Char N"))
  ;; 5. the addresses of linked pictures, as Word and LibreOffice write them
  (check= (office-link-target "https://a.b/c d.png?q=1#f" #t) "https://a.b/c%20d.png?q=1#f")
  (check= (office-link-target "https://a.b/a%20b.png" #f) "https://a.b/a%20b.png")
  (check= (office-link-target "C:\\pics\\a b.png" #t) "file:///C:\\pics\\a%20b.png")
  (check= (office-link-target "C:\\pics\\a b.png" #f) "file:///C:/pics/a%20b.png")
  (check= (office-link-target "file:///C:\\pics\\a.png" #f) "file:///C:/pics/a.png")
  (check= (office-link-target "../pics/my pic.png" #f) "../pics/my%20pic.png")
  (let* ((tree '(office (p (image (@ (name "C:\\pics\\a b.png") (linked "true"))))))
         (z (serialize-docx-document tree))
         (o (serialize-odt-document tree)))
    (check-true (export-has? (export-entry z "word/_rels/document.xml.rels")
                             "Target=\"file:///C:\\pics\\a%20b.png\" TargetMode=\"External\""))
    (check-true (export-has? (export-entry o "content.xml")
                             "xlink:href=\"file:///C:/pics/a%20b.png\""))))

;; The fourth audit of the writers: the ids of the named styles do not
;; depend on the order of the text, a name which is an id keeps it, a style
;; of a file of Word named as a style of TeXmacs is another style in
;; OpenDocument (never one of TeXmacs), the addresses of linked pictures.

(define (test-export-audit-4)
  (check-group "export audit 4")
  (let* ((styles '(styles (style (@ (name "heading 1") (family "paragraph") (color "#00ff00")))
                          (style (@ (name "Heading1") (family "paragraph") (color "#0000ff")))))
         (ids (lambda (blocks)
                (let* ((z (serialize-docx-document `(office ,styles ,@blocks)))
                       (text (export-entry z "word/document.xml")))
                  (list (export-has? text "<w:pStyle w:val=\"Heading1Doc\"/>")
                        (export-has? text "<w:pStyle w:val=\"heading1Doc2\"/>")
                        (export-has? text "<w:pStyle w:val=\"Heading1\"/>")))))
         (a '((p (@ (style "heading 1")) "h") (p (@ (style "Heading1")) "H")))
         (b '((p (@ (style "Heading1")) "H") (p (@ (style "heading 1")) "h"))))
    (check= (ids a) '(#t #t #f))
    (check= (ids b) '(#t #t #f)))
  ;; (MyNote keeps its id, My Note has another one)
  (let* ((text (export-entry (serialize-docx-document
                               '(office (p (@ (style "My Note")) "y") (p (@ (style "MyNote")) "x")))
                             "word/document.xml")))
    (check-true (export-has? text "<w:pStyle w:val=\"MyNote\"/>"))
    (check-true (export-has? text "<w:pStyle w:val=\"MyNoteDoc\"/>")))
  ;; a style Heading of a file of Word is not Heading_20_2 of TeXmacs
  (let* ((z (zip-pack '("word/document.xml" "word/styles.xml")
                      (list (string-append "<w:document" docx-ns "><w:body>"
                                           (wp (wstyle "Heading") (wr "x")) (wp (wr "y"))
                                           "</w:body></w:document>")
                            (string-append "<w:styles" docx-ns ">"
                                           "<w:style w:type='paragraph' w:styleId='Heading'>"
                                           "<w:name w:val='Heading'/><w:rPr><w:color w:val='FF0000'/></w:rPr>"
                                           "</w:style></w:styles>"))))
         (o (convert (convert z "docx-document" "texmacs-stree") "texmacs-stree" "odt-document"))
         (content (export-entry o "content.xml"))
         (styles (export-entry o "styles.xml")))
    (check-false (export-has? content "Heading_20_2"))
    (check-false (export-has? (export-odt-style styles "paragraph" "Heading_20_2") "fo:color")))
  ;; # ? and a % which is not %XX in the path of a linked picture; a path of
  ;; a server
  (check= (office-link-target "C:\\pics\\a#1 b.png" #t) "file:///C:\\pics\\a%231%20b.png")
  (check= (office-link-target "C:\\pics\\a#1 b.png" #f) "file:///C:/pics/a%231%20b.png")
  (check= (office-link-target "../p/100%.png" #f) "../p/100%25.png")
  (check= (office-link-target "../p/a%20b.png" #f) "../p/a%20b.png")
  (check= (office-link-target "\\\\server\\share\\a b.png" #t) "file://server/share/a%20b.png")
  (check= (office-link-target "https://x.y/a b.png" #t) "https://x.y/a%20b.png"))

;; Documents of styles of the names of those of the other format, opened in
;; Word and LibreOffice: a caption which is not the one of a figure keeps
;; its style, a plain paragraph is not in the style of another one of as
;; many paragraphs, a style of the document in a link is its look in Word.

(define (test-office-apps-check)
  (check-group "office apps check")
  (let* ((body (lambda (t) (cdadr (cadr t))))
         (z (zip-pack '("word/document.xml" "word/styles.xml")
                      (list (string-append "<w:document" docx-ns "><w:body>"
                                           (wp (wstyle "Caption") (wr "c"))
                                           (wp (wstyle "TextBody") (wr "t")) (wp (wr "p"))
                                           "</w:body></w:document>")
                            (string-append "<w:styles" docx-ns ">"
                                           "<w:style w:type='paragraph' w:styleId='Caption'>"
                                           "<w:name w:val='Caption'/><w:rPr><w:color w:val='FF8800'/>"
                                           "</w:rPr></w:style>"
                                           "<w:style w:type='paragraph' w:styleId='TextBody'>"
                                           "<w:name w:val='Text Body'/><w:rPr><w:color w:val='AA00AA'/>"
                                           "</w:rPr></w:style></w:styles>"))))
         (o (odt-file (string-append "<text:p text:style-name='Caption'>c</text:p>"
                                     "<text:p text:style-name='MyNote'>m</text:p>"
                                     "<text:p>p</text:p>")
                      (string-append "<office:styles>"
                                     "<style:style style:name='Caption' style:family='paragraph'>"
                                     "<style:text-properties fo:color='#ff8800'/></style:style>"
                                     "<style:style style:name='MyNote' style:family='paragraph'>"
                                     "<style:text-properties fo:color='#008888'/></style:style>"
                                     "</office:styles>")
                      "")))
    (check= (body (convert z "docx-document" "texmacs-stree"))
            '((office-par "Caption" "c") (office-par "TextBody" "t") "p"))
    (check= (body (convert o "odt-document" "texmacs-stree"))
            '((office-par "Caption" "c") (office-par "MyNote" "m") "p"))
    ;; (a caption of a figure is the one of the figure still)
    (check= (tm '(p (image (@ (name "f.png"))))
                '(p (@ (role "caption") (style "Caption")) "The figure"))
            '((big-figure (image "f.png" "" "" "" "") "The figure"))))
  (let* ((text (export-entry (serialize-docx-document
                               '(office (p (link (@ (href "https://a.b"))
                                                 (span (@ (style "Red")) "r"))
                                           " " (link (@ (href "https://a.b")) "l"))))
                             "word/document.xml")))
    (check-true (export-has? text "<w:rStyle w:val=\"Red\"/>"))
    (check= (export-count text "<w:rStyle w:val=\"Hyperlink\"/>") 1))
  ;; a footnote of a formula follows it (a formula of Word or OpenDocument
  ;; cannot hold it), and its point goes with the formula when it is read
  (let ((m '(math (@ (display "true") (form "text")) "x")))
    (check= (in-tm `((p ,m (note (p "n")) ".")))
            '((equation* (document (concat "x." (text (footnote "n")))))))
    (check= (in-tm `((p "see " ,m " " (note (p "n")) ". Then")))
            '("see" (equation* (document (concat "x." (text (footnote "n"))))) "Then")))
  (for (doc '((equation* (document (concat "x" (footnote "n") ".")))
              (equation* (document (concat "x" (label "e") (footnote "n") ".")))
              (concat "a " (math (concat "y" (footnote "m"))) " b")))
    (for (fm '("docx-document" "odt-document"))
      (let* ((d `(document (body (document ,doc)) (style "generic")))
             (back (convert (convert d "texmacs-stree" fm) fm "texmacs-stree"))
             (body (cadr (list-find (cdr back) (lambda (y) (func? y 'body))))))
        (check-true (>= (string-search-forwards "(footnote" 0 (object->string body)) 0)))))
  (let ((text (export-entry (serialize-docx-document
                              (texmacs->office
                                '(document (body (document (equation-lab (document (concat "x" (footnote "n"))) "1")))
                                           (style "generic"))
                                '()))
                            "word/document.xml")))
    ;; (the mark of the note just after the formula, before the tab of its
    ;; number)
    (check-true (export-has? text "</m:oMath><w:r><w:rPr><w:rStyle w:val=\"FootnoteReference\"/>")))
  ;; the title keeps the look of its style in the file: doc-title is
  ;; redefined in the initial environment
  (let* ((title (lambda (t)
                  (with i (list-find (cdr t) (lambda (y) (func? y 'initial)))
                    (and i (list-find (cdadr i)
                                      (lambda (a) (and (func? a 'associate)
                                                       (== (cadr a) "doc-title"))))))))
         (z (zip-pack '("word/document.xml" "word/styles.xml")
                      (list (string-append "<w:document" docx-ns "><w:body>"
                                           (wp (wstyle "Title") (wr "T")) (wp (wr "p"))
                                           "</w:body></w:document>")
                            (string-append "<w:styles" docx-ns ">"
                                           "<w:style w:type='paragraph' w:styleId='Title'>"
                                           "<w:name w:val='Title'/><w:pPr><w:jc w:val='right'/>"
                                           "</w:pPr><w:rPr><w:color w:val='FF0000'/></w:rPr>"
                                           "</w:style></w:styles>"))))
         (o (odt-file "<text:p text:style-name='Title'>T</text:p><text:p>p</text:p>"
                      (string-append "<office:styles>"
                                     "<style:style style:name='Title' style:family='paragraph'>"
                                     "<style:text-properties fo:color='#ff0000'/></style:style>"
                                     "</office:styles>")
                      ""))
         (dz (convert z "docx-document" "texmacs-stree"))
         (od (convert o "odt-document" "texmacs-stree")))
    (check= (cadr (cadr (list-find (cdr dz) (lambda (y) (func? y 'body)))))
            '(doc-data (doc-title "T")))
    (check= (title dz)
            '(associate "doc-title"
                        (macro "x" (with "par-mode" "right"
                                     (surround (vspace* "0.5fn") (vspace "0.5fn")
                                               (with "color" "#ff0000" (arg "x")))))))
    (check-true (>= (string-search-forwards "(with \"color\" \"#ff0000\" (arg \"x\"))" 0
                                            (object->string (title od)))
                    0))
    ;; (a title without its style keeps the look of TeXmacs)
    (check= (title (convert (odt-file "<text:p text:style-name='Title'>T</text:p>" "" "")
                            "odt-document" "texmacs-stree"))
            #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX of the style office
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-latex-office)
  (check-group "latex of office")
  ;; the tags of the style office are what they show, in LaTeX which
  ;; pdflatex compiles
  (let* ((doc `(document
                 (TeXmacs "2.1") (style "office")
                 (body (document
                         (office-par "Note"
                           (concat "a " (office-span "Key" "b") " "
                                   (office-ins "Ann" "" "new") " " (office-del "Bob" "" "old") " "
                                   (office-comment "Ann" "" "remark") " "
                                   (office-commented "c" "Ann" "" "r2") " "
                                   (office-font "Liberation Serif" "rm" "f")
                                   (office-letters "1" "l") (office-underline "double" "u")
                                   (office-strike "double" "s") (office-field " PAGE " "3")
                                   (office-capitalize "cap")))
                         (office-columns "2" (document "col"))
                         (office-float "left" "" "fl")
                         (office-box "left" "" "3cm" "" "" (document "in box"))
                         (with "ornament-color" "#dddddd" (ornamented "box"))
                         (marked "m")
                         (strike-through "gone")))))
         (tex (convert doc "texmacs-stree" "latex-document")))
    (check-false (export-has? tex "\\office"))
    (check-false (export-has? tex "\\marked"))
    (check-false (export-has? tex "\\strikethrough"))
    (check-true (export-has? tex "\\newcommand{\\tmstrike}"))
    (check-true (export-has? tex "\\tmstrike{old}"))
    (check-true (export-has? tex "\\tmstrike{gone}"))
    (check-true (export-has? tex "{\\underline{{\\underline{u}}}}"))
    (check-true (export-has? tex "remark]"))
    (check-true (export-has? tex "in box"))
    (check-true (export-has? tex "\\colorbox{yellow}{m}"))
    ;; (the package of the columns, a color #rrggbb by its name)
    (check-true (export-has? tex "multicol}"))
    (check-true (export-has? tex "\\begin{multicols}{2}"))
    (check-true (export-has? tex "\\definecolor{tmhexDDDDDD}{HTML}{DDDDDD}"))
    (check-true (export-has? tex "[backgroundcolor=tmhexDDDDDD]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fifth audit: what was changed in TeXmacs, the parts kept, fields, comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (doc-initial doc key)
  (with init (list-find (cdr doc) (lambda (x) (func? x 'initial)))
    (and init (with a (list-find (cdr (cadr init)) (lambda (a) (== (cadr a) key)))
                (and a (caddr a))))))

(define (doc-set-initial doc key val)
  ;; the document doc whose initial environment has val for key
  (map (lambda (x)
         (if (not (func? x 'initial)) x
             (let* ((l (cdr (cadr x)))
                    (has? (list-find l (lambda (a) (== (cadr a) key)))))
               `(initial (collection ,@(map (lambda (a) (if (== (cadr a) key)
                                                             `(associate ,key ,val) a))
                                            l)
                                     ,@(if has? '() `((associate ,key ,val))))))))
       doc))

(define (doc-set-body doc f)
  ;; the document doc whose blocks are those f makes of its blocks
  (map (lambda (x) (if (func? x 'body) `(body (document ,@(f (cdr (cadr x))))) x)) doc))

(define (tree-replace x old new)
  (cond ((== x old) new)
        ((pair? x) (map (lambda (y) (tree-replace y old new)) x))
        (else x)))

(define (archive-names* z)
  (let loop ((l (zip-unpack z)) (acc '()))
    (if (or (null? l) (null? (cdr l))) (reverse acc) (loop (cddr l) (cons (car l) acc)))))

(define (text-before? s a b)
  ;; a is in s, and before b
  (let ((i (string-search-forwards a 0 s)) (j (string-search-forwards b 0 s)))
    (and (>= i 0) (>= j 0) (< i j))))

(define audit5-rel "http://schemas.openxmlformats.org/officeDocument/2006/relationships/")

(define (audit5-docx body comments?)
  ;; a Word document with a header (and its picture), a footer, a picture
  ;; in its text, a picture which nothing uses and the picture of its first
  ;; page, a named style
  (zip-pack
    (append '("[Content_Types].xml" "_rels/.rels" "word/document.xml"
              "word/_rels/document.xml.rels" "word/styles.xml" "word/header1.xml"
              "word/_rels/header1.xml.rels" "word/footer1.xml" "word/media/p.png"
              "word/media/logo.png" "word/media/old.png" "docProps/thumbnail.jpeg")
            (if comments? '("word/comments.xml") '()))
    (append
      (list "<Types/>"
            (string-append
              "<Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>"
              "<Relationship Id='rId1' Type='" audit5-rel "officeDocument' Target='word/document.xml'/>"
              "<Relationship Id='rId2' Type='http://schemas.openxmlformats.org/package/2006/"
              "relationships/metadata/thumbnail' Target='docProps/thumbnail.jpeg'/>"
              "</Relationships>")
            (string-append "<?xml version='1.0'?><w:document" docx-ns "><w:body>" body
                           "<w:sectPr><w:headerReference w:type='default' r:id='rH'/>"
                           "<w:footerReference w:type='default' r:id='rF'/>"
                           "<w:pgSz w:w='11906' w:h='16838'/></w:sectPr></w:body></w:document>")
            (string-append
              "<Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>"
              "<Relationship Id='rH' Type='" audit5-rel "header' Target='header1.xml'/>"
              "<Relationship Id='rF' Type='" audit5-rel "footer' Target='footer1.xml'/>"
              "<Relationship Id='rI' Type='" audit5-rel "image' Target='media/p.png'/>"
              "<Relationship Id='rC' Type='" audit5-rel "comments' Target='comments.xml'/>"
              "</Relationships>")
            (string-append
              "<w:styles" docx-ns ">"
              "<w:style w:type='paragraph' w:default='1' w:styleId='Normal'>"
              "<w:name w:val='Normal'/></w:style>"
              "<w:style w:type='paragraph' w:styleId='Warning'><w:name w:val='Warning'/>"
              "<w:basedOn w:val='Normal'/><w:pPr><w:keepNext/><w:jc w:val='center'/></w:pPr>"
              "<w:rPr><w:rFonts w:asciiTheme='majorHAnsi'/><w:b/><w:color w:val='C00000'/>"
              "<w:lang w:val='fr-FR'/></w:rPr></w:style>"
              "</w:styles>")
            (string-append "<w:hdr" docx-ns ">" (wp (wr "Old head"))
                           "<w:p><w:r><w:drawing><wp:inline><wp:extent cx='720000' cy='360000'/>"
                           "<pic:blipFill><a:blip r:embed='rL'/></pic:blipFill></wp:inline>"
                           "</w:drawing></w:r></w:p></w:hdr>")
            (string-append
              "<Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'>"
              "<Relationship Id='rL' Type='" audit5-rel "image' Target='media/logo.png'/>"
              "</Relationships>")
            (string-append "<w:ftr" docx-ns ">" (wp (wr "Old foot")) "</w:ftr>")
            "PNGDATA" "LOGODATA" "OLDDATA" "JPEGDATA")
      (if comments?
          (list (string-append "<w:comments" docx-ns "><w:comment w:id='0' w:author='Ann'>"
                               (wp (wr "a remark")) "</w:comment></w:comments>"))
          '()))))

(define audit5-picture
  (string-append "<w:p><w:r><w:drawing><wp:inline><wp:extent cx='720000' cy='360000'/>"
                 "<pic:blipFill><a:blip r:embed='rI'/></pic:blipFill></wp:inline>"
                 "</w:drawing></w:r></w:p>"))

(define (test-office-changed)
  (check-group "office changed in TeXmacs")
  (let* ((z (audit5-docx (string-append (wp (wstyle "Warning") (wr "careful"))
                                        (wp (wr "plain")) audit5-picture)
                         #f))
         (doc (convert z "docx-document" "texmacs-stree"))
         (orig (doc-initial doc "office-original"))
         (export (lambda (d) (convert d "texmacs-stree" "docx-document")))
         (z0 (export doc))
         (names0 (archive-names* z0)))
    ;; what was read is kept beside the file
    (check-true (func? orig 'tuple))
    (check= (list-find (cdr orig) (lambda (y) (== (cadr y) "page-odd-header")))
            '(tuple "page-odd-header"
                    (concat "Old head " (image (tuple (raw-data "LOGODATA") "logo.png")
                                               "2cm" "1cm" "" ""))))
    (check-true (list-find (cdr orig) (lambda (y) (== (cadr y) "office-par-Warning"))))
    ;; unchanged: the header, the footer and the styles of the file
    (check-true (in? "word/header1.xml" names0))
    (check-true (in? "word/footer1.xml" names0))
    (check-false (list-find names0 (lambda (n) (string-starts? n "word/tm-header"))))
    (check-true (export-has? (archive-entry z0 "word/styles.xml") "w:asciiTheme=\"majorHAnsi\""))
    (check-false (export-has? (export-style (archive-entry z0 "word/styles.xml") "Warning") "<w:i/>"))
    ;; the parts which are kept are those which are named: the picture of
    ;; the header, not the old picture of the text, nor the one which
    ;; nothing used, nor the picture of the first page
    (check-true (in? "word/media/logo.png" names0))
    (check-false (in? "word/media/p.png" names0))
    (check-false (in? "word/media/old.png" names0))
    (check-false (in? "docProps/thumbnail.jpeg" names0))
    (check-false (export-has? (archive-entry z0 "_rels/.rels") "thumbnail"))
    (check-true (list-find names0 (lambda (n) (string-starts? n "word/media/tm-"))))
    ;; the picture deleted in TeXmacs is in the file no more
    (let* ((d (doc-set-body doc (lambda (l) (list-filter l (lambda (b) (not (export-has? (object->string b) "image")))))))
           (names (archive-names* (export d))))
      (check-false (list-find names (lambda (n) (string-starts? n "word/media/tm-"))))
      (check-false (in? "word/media/p.png" names)))
    ;; a header which was changed is written instead of the one of the file
    (let* ((d (doc-set-initial doc "page-odd-header"
                               '(concat (htab "5mm") "New head " (page-the-page) (htab "5mm"))))
           (z1 (export d))
           (names (archive-names* z1))
           (h (archive-entry z1 "word/tm-header1.xml"))
           (text (archive-entry z1 "word/document.xml"))
           (back (convert z1 "docx-document" "texmacs-stree")))
      (check-true (string? h))
      (check-true (export-has? h "New head"))
      (check-true (export-has? h "<w:jc w:val=\"center\"/>"))
      (check-true (export-has? h " PAGE </w:instrText>"))
      (check-true (export-has? (archive-entry z1 "[Content_Types].xml") "/word/tm-header1.xml"))
      (check-true (export-has? text "<w:headerReference w:type=\"default\" r:id=\"rIdTm"))
      (check-false (export-has? text "r:id=\"rH\""))
      (check-false (in? "word/header1.xml" names))
      (check-false (in? "word/media/logo.png" names))
      (check-true (in? "word/footer1.xml" names))
      (check-true (export-has? (object->string (doc-initial back "page-odd-header")) "New head"))
      (check-true (export-has? (object->string (doc-initial back "page-odd-header")) "page-the-page"))
      (check= (doc-initial back "page-odd-footer") "Old foot"))
    ;; a header of the first page, a footer which was removed
    (let* ((d (doc-set-body (doc-set-initial doc "page-odd-footer" "")
                            (lambda (l) (cons '(set-this-page-header "First") l))))
           (z1 (export d))
           (text (archive-entry z1 "word/document.xml"))
           (back (convert z1 "docx-document" "texmacs-stree")))
      (check-true (export-has? text "<w:headerReference w:type=\"first\""))
      (check-true (export-has? text "<w:titlePg/>"))
      (check-false (export-has? text "<w:footerReference"))
      (check-false (in? "word/footer1.xml" (archive-names* z1)))
      (check-true (in? '(set-this-page-header "First") (cdr (cadr (cadr back)))))
      (check= (doc-initial back "page-odd-footer") #f))
    ;; a named style whose macro was changed: what was changed is written,
    ;; the rest of the style of the file is kept
    (let* ((m (doc-initial doc "office-par-Warning"))
           (d (doc-set-initial doc "office-par-Warning"
                               `(macro "body" (with "font-shape" "italic" ,(caddr m)))))
           (st (export-style (archive-entry (export d) "word/styles.xml") "Warning")))
      (check-true (export-has? st "<w:i/><w:iCs/>"))
      (check-true (export-has? st "<w:b/>"))
      (check-true (export-has? st "w:asciiTheme=\"majorHAnsi\""))
      (check-true (export-has? st "<w:lang w:val=\"fr-FR\"/>"))
      (check-true (export-has? st "<w:keepNext/>"))
      (check-true (text-before? st "<w:keepNext/>" "<w:jc w:val=\"center\"/>")))
    (let* ((m (doc-initial doc "office-par-Warning"))
           (d (doc-set-initial doc "office-par-Warning" (tree-replace m "bold" "medium")))
           (st (export-style (archive-entry (export d) "word/styles.xml") "Warning")))
      (check-true (export-has? st "<w:b w:val=\"0\"/><w:bCs w:val=\"0\"/>"))
      (check-true (export-has? st "<w:color w:val=\"C00000\"/>"))))
  ;; OpenDocument: the header of the first page, the named styles, the
  ;; pictures which are not used any more
  (let* ((z (zip-pack
              '("mimetype" "content.xml" "styles.xml" "Pictures/p.png" "Pictures/old.png"
                "Pictures/bg.png" "manifest.rdf")
              (list "application/vnd.oasis.opendocument.text"
                    (string-append "<?xml version='1.0'?><office:document-content" odt-ns ">"
                                   "<office:body><office:text>"
                                   "<text:p text:style-name='Note'>careful</text:p>"
                                   "<text:p><draw:frame svg:width='2cm' svg:height='1cm'>"
                                   "<draw:image xlink:href='Pictures/p.png'/></draw:frame></text:p>"
                                   "</office:text></office:body></office:document-content>")
                    (string-append "<office:document-styles" odt-ns "><office:styles>"
                                   "<style:style style:name='Note' style:family='paragraph'>"
                                   "<style:paragraph-properties fo:margin-left='1cm'/>"
                                   "<style:text-properties fo:font-weight='bold' fo:color='#c00000'"
                                   " fo:language='fr'/></style:style>"
                                   "<style:style style:name='Header' style:family='paragraph'/>"
                                   "</office:styles><office:automatic-styles>"
                                   "<style:page-layout style:name='pm1'>"
                                   "<style:page-layout-properties><style:background-image"
                                   " xlink:href='Pictures/bg.png'/></style:page-layout-properties>"
                                   "</style:page-layout></office:automatic-styles>"
                                   "<office:master-styles><style:master-page style:name='Standard'"
                                   " style:page-layout-name='pm1'>"
                                   "<style:header><text:p>Old head</text:p></style:header>"
                                   "<style:footer><text:p>Old foot</text:p></style:footer>"
                                   "</style:master-page></office:master-styles>"
                                   "</office:document-styles>")
                    "PNGDATA" "OLDDATA" "BGDATA" "<rdf/>")))
         (doc (convert z "odt-document" "texmacs-stree"))
         (export (lambda (d) (convert d "texmacs-stree" "odt-document")))
         (z0 (export doc))
         (names0 (archive-names* z0)))
    (check-true (in? "Pictures/bg.png" names0))
    (check-false (in? "Pictures/old.png" names0))
    (check-false (in? "Pictures/p.png" names0))
    (check-true (export-has? (archive-entry z0 "META-INF/manifest.xml")
                             "manifest:full-path=\"manifest.rdf\" manifest:media-type=\"application/rdf+xml\""))
    (check-true (export-has? (archive-entry z0 "styles.xml") "<text:p>Old head</text:p>"))
    (let* ((d (doc-set-body (doc-set-initial doc "page-odd-header" '(concat (htab "5mm") "New head"))
                            (lambda (l) (cons '(set-this-page-footer "First foot") l))))
           (z1 (export d))
           (st (archive-entry z1 "styles.xml"))
           (back (convert z1 "odt-document" "texmacs-stree")))
      (check-false (export-has? st "Old head"))
      (check-true (export-has? st "New head"))
      (check-true (export-has? st "Old foot"))
      (check-true (export-has? st "<style:footer-first>"))
      (check-true (text-before? st "<style:header>" "<style:footer>"))
      (check-true (text-before? st "<style:footer>" "<style:footer-first>"))
      ;; (its alignment is an automatic style of styles.xml)
      (check-true (export-has? st "style:name=\"TmH1P"))
      (check= (doc-initial back "page-odd-header") '(concat (htab "5mm") "New head"))
      (check-true (in? '(set-this-page-footer "First foot") (cdr (cadr (cadr back))))))
    (let* ((m (doc-initial doc "office-par-Note"))
           (d (doc-set-initial doc "office-par-Note"
                               `(macro "body" (with "font-shape" "italic" ,(caddr m)))))
           (st (export-odt-style (archive-entry (export d) "styles.xml") "paragraph" "Note"))
           (d2 (doc-set-initial doc "office-par-Note" (tree-replace m "bold" "medium")))
           (st2 (export-odt-style (archive-entry (export d2) "styles.xml") "paragraph" "Note")))
      (check-true (export-has? st "fo:font-style=\"italic\""))
      (check-true (export-has? st "fo:font-weight=\"bold\""))
      (check-true (export-has? st "fo:language=\"fr\""))
      (check-false (export-has? (export-odt-style (archive-entry z0 "styles.xml") "paragraph" "Note")
                                "fo:font-style=\"italic\""))
      (check-true (export-has? st2 "fo:font-weight=\"normal\""))
      (check-true (export-has? st2 "fo:color=\"#c00000\""))))
  ;; a document of before office-original: nothing is taken as changed
  (let* ((z (audit5-docx (wp (wr "plain")) #f))
         (doc (convert z "docx-document" "texmacs-stree"))
         (d (doc-set-initial (doc-set-initial doc "office-original" "")
                             "page-odd-header" "New head"))
         (z1 (convert d "texmacs-stree" "docx-document")))
    (check-true (in? "word/header1.xml" (archive-names* z1)))
    (check-false (export-has? (archive-entry z1 "word/document.xml") "rIdTm"))))

(define (test-office-nested-fields)
  (check-group "nested fields")
  (let* ((b "<w:r><w:fldChar w:fldCharType='begin'/></w:r>")
         (sep "<w:r><w:fldChar w:fldCharType='separate'/></w:r>")
         (e "<w:r><w:fldChar w:fldCharType='end'/></w:r>")
         (i (lambda (t) (string-append "<w:r><w:instrText xml:space='preserve'>" t
                                       "</w:instrText></w:r>")))
         (one (string (integer->char 1)))
         (two (string (integer->char 2)))
         (body (string-append
                 (wp b (i " IF ") b (i " PAGE ") sep (wr "3") e (i " = 3 \"yes\" \"no\" ")
                     sep (wr "yes") e)
                 (wp b (i " = ") b (i " PAGE ") e (i " + 1 ") sep (wr "4") e)))
         (z (docx-archive body))
         (blocks (docx body))
         (doc (convert z "docx-document" "texmacs-stree"))
         (text (archive-entry (convert doc "texmacs-stree" "docx-document") "word/document.xml"))
         (back (docx-blocks-of* (convert doc "texmacs-stree" "docx-document"))))
    ;; the field inside the instruction stays there, not its result
    (check= blocks
            `((p (field (@ (code ,(string-append " IF " one " PAGE " two " = 3 \"yes\" \"no\" ")))
                        "yes"))
              (p (field (@ (code ,(string-append " = " one " PAGE " two " + 1 "))) "4"))))
    (check= (cdr (cadr (cadr doc)))
            '((office-field (concat " IF " (office-field " PAGE " "") " = 3 \"yes\" \"no\" ") "yes")
              (office-field (concat " = " (office-field " PAGE " "") " + 1 ") "4")))
    ;; written as a field inside the field, and read back the same
    (check= (export-count text "w:fldCharType=\"begin\"") 4)
    (check= (export-count text "w:fldCharType=\"separate\"") 2)
    (check= (export-count text "w:fldCharType=\"end\"") 4)
    (check-true (text-before? text "> IF </w:instrText>" "> PAGE </w:instrText>"))
    (check-true (text-before? text "> PAGE </w:instrText>" "> = 3 \"yes\" \"no\" </w:instrText>"))
    (check-false (export-has? text one))
    (check= back blocks)
    ;; (the braces of an instruction are text, as those of Zotero)
    (check= (docx (wp b (i " ADDIN ZOTERO_ITEM {\"a\":{}} ") sep (wr "[1]") e))
            '((p (field (@ (code " ADDIN ZOTERO_ITEM {\"a\":{}} ")) "[1]"))))
    ;; a mark which closes nothing is not written
    (check-false (export-has? (archive-entry (serialize-docx-document
                                              `(office (p (field (@ (code ,(string-append " PAGE" two " ")))
                                                                 "1"))))
                                             "word/document.xml")
                              two))))

(define (odt-blocks-of* archive)
  (bare (no-setup (cdr (parse-odt-document archive)))))

(define (test-office-comment-ranges)
  (check-group "comment ranges")
  (let* ((body (string-append
                 (wp (wr "before ") "<w:commentRangeStart w:id='0'/>" (wr "the range")
                     "<w:commentRangeEnd w:id='0'/><w:r><w:commentReference w:id='0'/></w:r>"
                     (wr " after"))))
         (z (audit5-docx body #t))
         (blocks (docx-blocks-of* z))
         (doc (convert z "docx-document" "texmacs-stree"))
         (text (archive-entry (convert doc "texmacs-stree" "docx-document") "word/document.xml")))
    (check= blocks
            '((p "before " (commented "the range" (comment (@ (author "Ann")) (p "a remark")))
                 " after")))
    (check= (cdr (cadr (cadr doc)))
            '((concat "before " (office-commented "the range" "Ann" "" "a remark") " after")))
    ;; the range around its text, then the mark of the comment
    (check-true (text-before? text "<w:commentRangeStart w:id=\"0\"/>" "the range"))
    (check-true (text-before? text "the range" "<w:commentRangeEnd w:id=\"0\"/>"))
    (check-true (text-before? text "<w:commentRangeEnd w:id=\"0\"/>" "<w:commentReference"))
    (check-true (text-before? text "<w:commentReference" " after"))
    (check= (docx-blocks-of* (convert doc "texmacs-stree" "docx-document")) blocks)
    ;; a mark without its other one: a comment on a point
    (check= (docx-blocks-of* (audit5-docx (wp "<w:commentRangeStart w:id='0'/>" (wr "x")
                                              "<w:r><w:commentReference w:id='0'/></w:r>")
                                          #t))
            '((p "x" (comment (@ (author "Ann")) (p "a remark")))))
    ;; in the other format, and in OpenDocument
    (let* ((o (convert doc "texmacs-stree" "odt-document"))
           (c (archive-entry o "content.xml"))
           (back (odt-blocks-of* o)))
      (check-true (text-before? c "<office:annotation office:name=" "the range"))
      (check-true (text-before? c "the range" "<office:annotation-end office:name="))
      (check= (list-filter back (lambda (b) (func? b 'p))) blocks)))
  (check= (odt-blocks-of
            (string-append "<text:p>a <office:annotation office:name='n1'><dc:creator>Bob</dc:creator>"
                           "<text:p>note</text:p></office:annotation>range"
                           "<office:annotation-end office:name='n1'/> b</text:p>")
            "" "")
          '((p "a " (commented "range" (comment (@ (author "Bob")) (p "note"))) " b"))))

(define (test-office-audit-5-misc)
  (check-group "fifth audit, other")
  ;; the reading of a file of the style office, by default
  (check= (get-preference "office->texmacs:faithful") "on")
  ;; no picture of Windows without its bytes
  (check-false (office-metafile->png "" "a.emf")))

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
  ;; (a document of the style office, with the file in its environment)
  (check= (list-head (convert (docx-archive (string-append (wp (wstyle "Titre1") (wr "Intro"))
                                                           (wp (wr "a ") (wr "b" "<w:i/>"))))
                              "docx-document" "texmacs-stree")
                     3)
          '(document (body (document (section* "Intro") (concat "a " (em "b"))))
                     (style "office")))
  (check= (list-head (convert (odt-archive (string-append "<text:h text:outline-level='1'>Intro</text:h>"
                                                          (tp "a " (tspan "Emphasis" "b"))))
                              "odt-document" "texmacs-stree")
                     3)
          '(document (body (document (section* "Intro")
                                     (concat "a " (office-span "Emphasis" "b"))))
                     (style "office")))
  ;; a file which is no archive is a document which says so
  (check= (convert "not an archive" "odt-document" "texmacs-stree")
          '(document (body (document (with "color" "red"
                                       "This file is not an OpenDocument text")))
                     (style "generic"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (office-test-failures)
  (:synopsis "Run the tests of the office converters, return the failures")
  (check-suite "office")
  (test-zip)
  (test-zip-damaged)
  (test-xml-limits)
  (test-office-long)
  (test-office-odd-values)
  (test-office-fidelity)
  (test-office-style)
  (test-lengths)
  (test-docx-text)
  (test-docx-links)
  (test-docx-blocks)
  (test-docx-lists)
  (test-docx-tables)
  (test-omml)
  (test-odt-text)
  (test-odt-blocks)
  (test-docx-more-text)
  (test-docx-more-blocks)
  (test-docx-unusual)
  (test-odt-more)
  (test-odt-audit)
  (test-import-audit-2)
  (test-import-audit-3)
  (test-import-audit-4)
  (test-import-math)
  (test-import-files)
  (test-officetm-more)
  (test-officetm-text)
  (test-officetm-math)
  (test-officetm-blocks)
  (test-officetm-numbers)
  (test-officetm-tables)
  (test-officetm-title)
  (test-tmoffice-text)
  (test-tmoffice-blocks)
  (test-tmoffice-tables)
  (test-tmoffice-numbers)
  (test-tmoffice-drawings)
  (test-tmoffice-tmdoc)
  (test-mathml-omml)
  (test-writers)
  (test-round-trips)
  (test-numbered-round-trip)
  (test-export-math)
  (test-export-blocks)
  (test-export-writers)
  (test-export-audit)
  (test-export-audit-2)
  (test-export-audit-3)
  (test-export-audit-4)
  (test-office-apps-check)
  (test-latex-office)
  (test-office-changed)
  (test-office-nested-fields)
  (test-office-comment-ranges)
  (test-office-audit-5-misc)
  (test-formats)
  (check-end))
