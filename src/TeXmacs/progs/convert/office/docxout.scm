
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : docxout.scm
;; DESCRIPTION : writing office trees as Word documents (.docx)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The office tree (office-tools.scm) is written as the files of a Word
;; document, in a zip archive: the text (word/document.xml), the styles
;; which its paragraphs use by their names (word/styles.xml), so that the
;; document can be given another look in Word, the lists
;; (word/numbering.xml), the notes (word/footnotes.xml), the images
;; (word/media) and the relations which tell where all these are.

(texmacs-module (convert office docxout)
  (:use (convert office office-tools)
        (convert office omml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State: the document which is written
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dx-rels '())        ; the relations of the text, reversed
(define dx-media '())       ; (name data) of the images, reversed
(define dx-notes '())       ; the elements of the notes, reversed
(define dx-lists '())       ; (list kind level) of the lists, reversed
(define dx-id 0)            ; the last number given to a thing
(define dx-in-note? #f)     ; inside a note: no relations
(define dx-item #f)         ; (list level) for the next paragraph of an item
(define dx-indent 0)        ; the depth of the lists around a paragraph
(define dx-cell-align #f)   ; the alignment of the cell around a paragraph

(define dx-text-width 9026) ; the width of the text, in twentieths of a point

(define dx-ns-w "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
(define dx-ns-r
  "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
(define dx-ns-m "http://schemas.openxmlformats.org/officeDocument/2006/math")
(define dx-ns-wp
  "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")
(define dx-ns-a "http://schemas.openxmlformats.org/drawingml/2006/main")
(define dx-ns-pic "http://schemas.openxmlformats.org/drawingml/2006/picture")
(define dx-ns-svg "http://schemas.microsoft.com/office/drawing/2016/SVG/main")
(define dx-rel-type
  "http://schemas.openxmlformats.org/officeDocument/2006/relationships/")

(define (dx-next-id)
  (set! dx-id (+ dx-id 1))
  dx-id)

(define (dx-val tag v) `(,tag (@ (w:val ,v))))

(define (dx-relation type target external?)
  ;; the identifier of a new relation of the text
  (with id (string-append "rId" (number->string (+ (length dx-rels) 1)))
    (set! dx-rels (cons (list id type target external?) dx-rels))
    id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dx-run-properties props)
  ;; the element w:rPr for the wrappers around a text, in the order which
  ;; the format wants; props has tags and (color "#rrggbb")
  (let* ((has? (lambda (tag) (in? tag props)))
         (color (list-find props pair?))
         (l (append
              (cond ((has? 'code) (list (dx-val 'w:rStyle "VerbatimChar")))
                    ((has? 'hyperlink) (list (dx-val 'w:rStyle "Hyperlink")))
                    (else '()))
              (if (has? 'strong) '((w:b)) '())
              (if (has? 'em) '((w:i)) '())
              (if (has? 'smallcaps) '((w:smallCaps)) '())
              (if (has? 'strike) '((w:strike)) '())
              (if color
                  (list (dx-val 'w:color (upcase-all (substring (cadr color) 1 7))))
                  '())
              (if (has? 'mark) (list (dx-val 'w:highlight "yellow")) '())
              (if (has? 'underline) (list (dx-val 'w:u "single")) '())
              (cond ((has? 'sup) (list (dx-val 'w:vertAlign "superscript")))
                    ((has? 'sub) (list (dx-val 'w:vertAlign "subscript")))
                    (else '())))))
    (if (null? l) '() (list (cons 'w:rPr l)))))

(define (dx-run props . content)
  `(w:r ,@(dx-run-properties props) ,@content))

(define (dx-text s)
  `(w:t (@ (xml:space "preserve")) ,s))

(define (dx-emu s)
  ;; a length in English Metric Units: 360000 are a centimeter
  (with cm (or (office-length->cm s) 5.0)
    (number->string (inexact->exact (round (* cm 360000))))))

(define (dx-image x)
  ;; a picture in the line; its file goes to word/media
  (let* ((data (ox-attr x 'data))
         (name (or (ox-attr x 'name) "image.png"))
         (n (dx-next-id))
         (dot (let loop ((i (- (string-length name) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref name i) #\.) i)
                      (else (loop (- i 1))))))
         (suffix (if dot (locase-all (substring name (+ dot 1) (string-length name)))
                     "png"))
         (file (string-append "image" (number->string n) "." suffix))
         (cx (dx-emu (ox-attr x 'width)))
         (cy (dx-emu (ox-attr x 'height))))
    (if (or (not data) dx-in-note?) '()
        (let* ((id (dx-relation "image" (string-append "media/" file) #f))
               (svg (ox-attr x 'svg))
               (svg-file (string-append "image" (number->string n) ".svg"))
               (svg-id (and svg (dx-relation "image"
                                             (string-append "media/" svg-file) #f))))
          (set! dx-media (cons (list file data) dx-media))
          (when svg (set! dx-media (cons (list svg-file svg) dx-media)))
          (list
            `(w:r
               (w:drawing
                 (wp:inline (@ (distT "0") (distB "0") (distL "0") (distR "0"))
                   (wp:extent (@ (cx ,cx) (cy ,cy)))
                   (wp:docPr (@ (id ,(number->string n))
                                (name ,(string-append "Picture " (number->string n)))
                                ,@(if (ox-attr x 'alt) `((descr ,(ox-attr x 'alt))) '())))
                   (a:graphic (@ (xmlns:a ,dx-ns-a))
                     (a:graphicData (@ (uri ,dx-ns-pic))
                       (pic:pic (@ (xmlns:pic ,dx-ns-pic))
                         (pic:nvPicPr
                           (pic:cNvPr (@ (id ,(number->string n)) (name ,file)))
                           (pic:cNvPicPr))
                         (pic:blipFill
                           ;; with an SVG of the picture, for the programs
                           ;; which show it in the place of the bitmap
                           (a:blip
                             (@ (r:embed ,id))
                             ,@(if svg-id
                                   `((a:extLst
                                       (a:ext
                                         (@ (uri "{96DAC541-7B7A-43D3-8B79-37D633B846F1}"))
                                         (asvg:svgBlip
                                           (@ (xmlns:asvg ,dx-ns-svg)
                                              (r:embed ,svg-id))))))
                                   '()))
                           (a:stretch (a:fillRect)))
                         (pic:spPr
                           (a:xfrm (a:off (@ (x "0") (y "0")))
                                   (a:ext (@ (cx ,cx) (cy ,cy))))
                           (a:prstGeom (@ (prst "rect")) (a:avLst))))))))))))))

(define (dx-math x)
  ;; a formula: MathML as a tree or as its text, or plain text
  (let* ((form (ox-attr x 'form))
         (c (ox-children x))
         (mathml (cond ((null? c) #f)
                       ((== form "text") `(m:math (m:mtext ,(car c))))
                       ((== form "sxml") (car c))
                       (else (catch #t (lambda () (ox-root (parse-xml (car c))))
                                    (lambda args #f)))))
         (formula (and mathml (mathml->omml mathml))))
    (cond ((not formula) '())
          ((== (ox-attr x 'display) "true") (list `(m:oMathPara ,formula)))
          (else (list formula)))))

(define (dx-note x)
  ;; the mark of a note; its text goes to word/footnotes.xml
  (if dx-in-note? '()
      (let ((n (+ (length dx-notes) 1))
            (old-item dx-item)
            (old-indent dx-indent)
            (old-align dx-cell-align))
        (set! dx-in-note? #t)
        (set! dx-item #f)
        (set! dx-indent 0)
        (set! dx-cell-align #f)
        (let* ((blocks (dx-blocks (ox-children x)))
               (blocks (if (null? blocks) (list '(w:p)) blocks))
               ;; the first paragraph has the style of the notes and starts
               ;; with the mark
               (first (car blocks))
               (first (if (func? first 'w:p)
                          `(w:p (w:pPr ,(dx-val 'w:pStyle "FootnoteText"))
                                (w:r (w:rPr ,(dx-val 'w:rStyle "FootnoteReference"))
                                     (w:footnoteRef))
                                ,(dx-run '() (dx-text " "))
                                ,@(list-filter (ox-children first)
                                               (lambda (y) (not (func? y 'w:pPr)))))
                          first)))
          (set! dx-in-note? #f)
          (set! dx-item old-item)
          (set! dx-indent old-indent)
          (set! dx-cell-align old-align)
          (set! dx-notes
                (cons `(w:footnote (@ (w:id ,(number->string n)))
                                   ,first ,@(cdr blocks))
                      dx-notes))
          (list `(w:r (w:rPr ,(dx-val 'w:rStyle "FootnoteReference"))
                      (w:footnoteReference (@ (w:id ,(number->string n))))))))))

(define (dx-inline x props)
  ;; the elements of a paragraph for an inline node inside the wrappers
  ;; props
  (cond ((string? x) (if (== x "") '() (list (dx-run props (dx-text x)))))
        ((not (pair? x)) '())
        (else
          (let ((l (ox-children x)))
            (case (car x)
              ((em strong underline strike sub sup code smallcaps mark)
               (dx-inlines l (cons (car x) props)))
              ((color)
               (dx-inlines l (if (office-color (ox-attr x 'value))
                                 (cons (list 'color (office-color (ox-attr x 'value)))
                                       props)
                                 props)))
              ((link ref)
               (let* ((href (if (func? x 'ref)
                                (string-append "#" (or (ox-attr x 'name) ""))
                                (or (ox-attr x 'href) "")))
                      (runs (dx-inlines l (cons 'hyperlink props))))
                 (cond ((null? runs) '())
                       ((string-starts? href "#")
                        (list `(w:hyperlink
                                 (@ (w:anchor ,(substring href 1 (string-length href))))
                                 ,@runs)))
                       ((or dx-in-note? (== href "")) (dx-inlines l props))
                       (else
                         (list `(w:hyperlink
                                  (@ (r:id ,(dx-relation "hyperlink" href #t)))
                                  ,@runs))))))
              ((bookmark)
               (with n (number->string (dx-next-id))
                 (list `(w:bookmarkStart (@ (w:id ,n) (w:name ,(or (ox-attr x 'name) ""))))
                       `(w:bookmarkEnd (@ (w:id ,n))))))
              ((note) (dx-note x))
              ((br) (list (dx-run '() '(w:br))))
              ((tab) (list (dx-run '() '(w:tab))))
              ((pagebreak) (list (dx-run '() '(w:br (@ (w:type "page"))))))
              ((image) (dx-image x))
              ((math) (dx-math x))
              (else (dx-inlines l props)))))))

(define (dx-inlines l props)
  (append-map (lambda (x) (dx-inline x props)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraphs and lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the styles of the paragraphs, by their roles
(define dx-role-styles
  '(("title" . "Title") ("subtitle" . "Subtitle") ("author" . "Author")
    ("date" . "Date") ("abstract" . "Abstract") ("quote" . "Quote")
    ("code" . "SourceCode") ("caption" . "Caption") ("figure" . "Figure")
    ("term" . "DefinitionTerm") ("definition" . "Definition")))

(define (dx-paragraph-style x)
  (with role (ox-attr x 'role)
    (cond ((== role "heading")
           (string-append "Heading" (or (ox-attr x 'level) "1")))
          ((assoc-ref dx-role-styles role) => identity)
          (else #f))))

(define (dx-paragraph x)
  (let* ((style (dx-paragraph-style x))
         (align (or (ox-attr x 'align) dx-cell-align))
         (item dx-item)
         (props
           (append
             (if style (list (dx-val 'w:pStyle style)) '())
             ;; the first paragraph of an item has its number; the others
             ;; are under it
             (cond (item
                    `((w:numPr ,(dx-val 'w:ilvl (number->string (cadr item)))
                               ,(dx-val 'w:numId (number->string (car item))))))
                   ((> dx-indent 0)
                    `((w:ind (@ (w:left ,(number->string (* 720 dx-indent)))))))
                   (else '()))
             (cond ((== align "center") (list (dx-val 'w:jc "center")))
                   ((== align "right") (list (dx-val 'w:jc "right")))
                   (else '())))))
    (set! dx-item #f)
    (list `(w:p ,@(if (null? props) '() (list (cons 'w:pPr props)))
                ,@(dx-inlines (ox-children x) '())))))

(define (dx-list x)
  ;; a list: each one is a list of its own for Word, so that its numbers
  ;; start at 1, at the level of its depth
  (let* ((kind (if (== (ox-attr x 'kind) "number") "number" "bullet"))
         (level dx-indent)
         (id (+ (length dx-lists) 1))
         (old-item dx-item))
    (set! dx-lists (cons (list id kind level) dx-lists))
    (set! dx-indent (+ level 1))
    (with r (append-map
              (lambda (item)
                (set! dx-item (list id (min level 8)))
                (with b (dx-blocks (ox-children item))
                  ;; an item without a paragraph at its start still has
                  ;; its number
                  (if dx-item
                      (begin
                        (set! dx-item (list id (min level 8)))
                        (append (dx-paragraph '(p)) b))
                      b)))
              (list-filter (ox-children x) (lambda (y) (func? y 'item))))
      (set! dx-indent level)
      (set! dx-item old-item)
      r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dx-cell-borders letters)
  ;; the borders of a cell, from the letters of its sides
  (let ((side (lambda (tag letter)
                (if (and letters (>= (string-search-forwards letter 0 letters) 0))
                    `(,tag (@ (w:val "single") (w:sz "4") (w:space "0")
                              (w:color "auto")))
                    `(,tag (@ (w:val "nil")))))))
    (if (not letters) '()
        (list `(w:tcBorders ,(side 'w:top "t") ,(side 'w:left "l")
                            ,(side 'w:bottom "b") ,(side 'w:right "r"))))))

(define (dx-cell c width span vmerge)
  ;; a cell of this width, over span columns; vmerge is #f, 'start for a
  ;; cell which goes on in the rows below, or 'continue for the place of
  ;; such a cell in one of them
  (let* ((old-align dx-cell-align)
         (old-item dx-item)
         (old-indent dx-indent))
    (set! dx-cell-align (ox-attr c 'align))
    (set! dx-item #f)
    (set! dx-indent 0)
    (let* ((blocks (if (== vmerge 'continue) '() (dx-blocks (ox-children c))))
           ;; a cell ends with a paragraph
           (blocks (if (or (null? blocks) (not (func? (cAr blocks) 'w:p)))
                       (append blocks (list '(w:p)))
                       blocks))
           (fill (office-color (ox-attr c 'background))))
      (set! dx-cell-align old-align)
      (set! dx-item old-item)
      (set! dx-indent old-indent)
      `(w:tc
         (w:tcPr
           (w:tcW (@ (w:w ,(number->string width)) (w:type "dxa")))
           ,@(if (> span 1) (list (dx-val 'w:gridSpan (number->string span))) '())
           ,@(cond ((== vmerge 'start) (list (dx-val 'w:vMerge "restart")))
                   ((== vmerge 'continue) '((w:vMerge)))
                   (else '()))
           ,@(dx-cell-borders (ox-attr c 'borders))
           ,@(if fill
                 `((w:shd (@ (w:val "clear") (w:color "auto")
                             (w:fill ,(upcase-all (substring fill 1 7))))))
                 '()))
         ,@blocks))))

(define (dx-text-length x)
  ;; a rough measure of the width of the text of a node: the longest of
  ;; its paragraphs, in characters
  (cond ((string? x) (string-length x))
        ((not (pair? x)) 0)
        ((in? (car x) '(image math)) 6)
        ((in? (car x) '(cell item list table row note))
         (apply max (cons 0 (map dx-text-length (ox-children x)))))
        (else (apply + (map dx-text-length (ox-children x))))))

(define (dx-table x)
  (let* ((rows (list-filter (ox-children x) (lambda (r) (func? r 'row))))
         (ncols (apply max (cons 1 (map (lambda (r) (length (ox-children r))) rows))))
         (nrows (length rows))
         (part (with w (ox-attr x 'width)
                 (or (and w (string-ends? w "par")
                          (string->number (substring w 0 (- (string-length w) 3))))
                     #f)))
         (total (inexact->exact (round (* dx-text-width (or part 1.0)))))
         (parts (with c (ox-attr x 'columns)
                  (and c (map string->number (string-tokenize-by-char c #\space)))))
         (parts (and parts (== (length parts) ncols) (list-and parts) parts))
         ;; The widths of the columns: the parts of the width of the table
         ;; when they are known; else, for a table of the width of its
         ;; text, from the longest text of each column.
         (natural (map (lambda (j)
                         (+ 300
                            (* 120
                               (apply max
                                      (cons 1
                                            (map (lambda (r)
                                                   (with cells (ox-children r)
                                                     (if (< j (length cells))
                                                         (dx-text-length (list-ref cells j))
                                                         0)))
                                                 rows))))))
                       (iota ncols)))
         (scale (min 1.0 (/ (* 1.0 total) (max 1 (apply + natural)))))
         (widths (cond (parts
                        (map (lambda (p) (inexact->exact (round (* p total)))) parts))
                       (part
                        (map (lambda (j) (inexact->exact (round (/ total ncols))))
                             (iota ncols)))
                       (else
                        (map (lambda (w) (inexact->exact (round (* w scale))))
                             natural))))
         ;; the cells by their place, and those which a higher one covers:
         ;; (row . column) -> the cell which covers it
         (cell-at (lambda (i j)
                    (with cells (ox-children (list-ref rows i))
                      (and (< j (length cells)) (list-ref cells j)))))
         (above (make-ahash-table))
         (span-of (lambda (c name)
                    (or (and c (ox-attr c name) (string->number (ox-attr c name))) 1)))
         (align (ox-attr x 'align)))
    (for (i (iota nrows))
      (for (j (iota ncols))
        (with c (cell-at i j)
          (when (and c (> (span-of c 'rowspan) 1))
            (for (a (iota (- (span-of c 'rowspan) 1)))
              (ahash-set! above (cons (+ i a 1) j) c))))))
    `(w:tbl
       (w:tblPr
         (w:tblW ,(if part
                      `(@ (w:w ,(number->string (inexact->exact (round (* 5000 part)))))
                          (w:type "pct"))
                      '(@ (w:w "0") (w:type "auto"))))
         ,@(cond ((== align "center") (list (dx-val 'w:jc "center")))
                 ((== align "right") (list (dx-val 'w:jc "right")))
                 (else '()))
         (w:tblLayout (@ (w:type ,(if part "fixed" "autofit"))))
         (w:tblCellMar (w:left (@ (w:w "80") (w:type "dxa")))
                       (w:right (@ (w:w "80") (w:type "dxa")))))
       (w:tblGrid ,@(map (lambda (w) `(w:gridCol (@ (w:w ,(number->string w)))))
                         widths))
       ,@(map
           (lambda (i)
             (let* ((cells (ox-children (list-ref rows i)))
                    (header? (and (pair? cells)
                                  (list-and (map (lambda (c)
                                                   (or (ox-attr c 'covered)
                                                       (== (ox-attr c 'header) "true")))
                                                 cells)))))
               `(w:tr
                  ,@(if header? '((w:trPr (w:tblHeader))) '())
                  ,@(let loop ((j 0) (acc '()))
                      (if (>= j ncols) (reverse acc)
                          (let* ((c (or (cell-at i j) '(cell)))
                                 (up (ahash-ref above (cons i j)))
                                 (span (span-of (or up c) 'colspan))
                                 (span (max 1 (min span (- ncols j))))
                                 (width (apply + (sublist widths j (+ j span)))))
                            (loop (+ j span)
                                  (cons (cond (up (dx-cell up width span 'continue))
                                              ((> (span-of c 'rowspan) 1)
                                               (dx-cell c width span 'start))
                                              (else (dx-cell c width span #f)))
                                        acc))))))))
           (iota nrows)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dx-field instruction text)
  ;; a field which Word computes: its instruction and what is shown before
  `(w:p (w:r (w:fldChar (@ (w:fldCharType "begin"))))
        (w:r (w:instrText (@ (xml:space "preserve")) ,instruction))
        (w:r (w:fldChar (@ (w:fldCharType "separate"))))
        ,(dx-run '() (dx-text text))
        (w:r (w:fldChar (@ (w:fldCharType "end"))))))

(define (dx-block x)
  (cond ((not (pair? x)) '())
        (else
          (case (car x)
            ((p) (dx-paragraph x))
            ((list) (dx-list x))
            ((table) (list (dx-table x)))
            ((pagebreak) (list `(w:p ,(dx-run '() '(w:br (@ (w:type "page")))))))
            ((rule)
             (list `(w:p (w:pPr (w:pBdr (w:bottom (@ (w:val "single") (w:sz "6")
                                                     (w:space "1") (w:color "auto"))))))))
            ((toc)
             (list (dx-field " TOC \\o \"1-3\" \\h \\z \\u "
                             "Update this field to see the table of contents.")))
            (else '())))))

(define (dx-blocks l)
  (append-map dx-block l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The files of the archive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dx-style type id name . l)
  ;; a style: its properties of paragraphs and of runs are in l
  `(w:style (@ (w:type ,type) (w:styleId ,id))
            ,(dx-val 'w:name name)
            ,@l))

(define (dx-heading-style n)
  (let ((s (number->string n))
        (size (list-ref '("36" "30" "26" "24" "22" "22" "22" "22" "22") (- n 1))))
    (dx-style "paragraph" (string-append "Heading" s) (string-append "heading " s)
              (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal") '(w:qFormat)
              `(w:pPr (w:keepNext)
                      (w:spacing (@ (w:before ,(if (<= n 2) "360" "240"))
                                    (w:after "120")))
                      ,(dx-val 'w:outlineLvl (number->string (- n 1))))
              `(w:rPr (w:b) ,@(if (>= n 4) '((w:i)) '())
                      ,(dx-val 'w:sz size) ,(dx-val 'w:szCs size)))))

(define (dx-styles)
  `(w:styles
     (@ (xmlns:w ,dx-ns-w))
     (w:docDefaults
       (w:rPrDefault (w:rPr ,(dx-val 'w:sz "22") ,(dx-val 'w:szCs "22")))
       (w:pPrDefault (w:pPr (w:spacing (@ (w:after "120"))))))
     ,(dx-style "paragraph" "Normal" "Normal" '(w:qFormat))
     ,@(map dx-heading-style (map (lambda (i) (+ i 1)) (iota 9)))
     ,(dx-style "paragraph" "Title" "Title"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal") '(w:qFormat)
                `(w:pPr (w:spacing (@ (w:before "480") (w:after "240")))
                        ,(dx-val 'w:jc "center"))
                `(w:rPr (w:b) ,(dx-val 'w:sz "44") ,(dx-val 'w:szCs "44")))
     ,(dx-style "paragraph" "Subtitle" "Subtitle"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal") '(w:qFormat)
                `(w:pPr ,(dx-val 'w:jc "center"))
                `(w:rPr ,(dx-val 'w:sz "30") ,(dx-val 'w:szCs "30")))
     ,(dx-style "paragraph" "Author" "Author"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal")
                `(w:pPr ,(dx-val 'w:jc "center"))
                `(w:rPr ,(dx-val 'w:sz "24") ,(dx-val 'w:szCs "24")))
     ,(dx-style "paragraph" "Date" "Date"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal")
                `(w:pPr ,(dx-val 'w:jc "center")))
     ,(dx-style "paragraph" "Abstract" "Abstract"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal")
                `(w:pPr (w:spacing (@ (w:before "240") (w:after "240")))
                        (w:ind (@ (w:left "720") (w:right "720"))))
                `(w:rPr ,(dx-val 'w:sz "20") ,(dx-val 'w:szCs "20")))
     ,(dx-style "paragraph" "Quote" "Quote"
                (dx-val 'w:basedOn "Normal") '(w:qFormat)
                `(w:pPr (w:ind (@ (w:left "720") (w:right "720")))))
     ,(dx-style "paragraph" "SourceCode" "Source Code"
                (dx-val 'w:basedOn "Normal")
                `(w:pPr (w:spacing (@ (w:after "120"))) ,(dx-val 'w:jc "left"))
                `(w:rPr (w:rFonts (@ (w:ascii "Courier New") (w:hAnsi "Courier New")
                                     (w:cs "Courier New")))
                        (w:noProof) ,(dx-val 'w:sz "20") ,(dx-val 'w:szCs "20")))
     ,(dx-style "paragraph" "Caption" "caption"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal") '(w:qFormat)
                `(w:pPr ,(dx-val 'w:jc "center"))
                `(w:rPr ,(dx-val 'w:sz "20") ,(dx-val 'w:szCs "20")))
     ,(dx-style "paragraph" "Figure" "Figure"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Caption")
                `(w:pPr (w:keepNext) ,(dx-val 'w:jc "center")))
     ,(dx-style "paragraph" "DefinitionTerm" "Definition Term"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Definition")
                `(w:pPr (w:keepNext) (w:spacing (@ (w:after "0"))))
                `(w:rPr (w:b)))
     ,(dx-style "paragraph" "Definition" "Definition"
                (dx-val 'w:basedOn "Normal")
                `(w:pPr (w:ind (@ (w:left "720")))))
     ,(dx-style "paragraph" "FootnoteText" "footnote text"
                (dx-val 'w:basedOn "Normal")
                `(w:pPr (w:spacing (@ (w:after "0"))))
                `(w:rPr ,(dx-val 'w:sz "20") ,(dx-val 'w:szCs "20")))
     ,(dx-style "character" "FootnoteReference" "footnote reference"
                `(w:rPr ,(dx-val 'w:vertAlign "superscript")))
     ,(dx-style "character" "Hyperlink" "Hyperlink"
                `(w:rPr ,(dx-val 'w:color "0563C1") ,(dx-val 'w:u "single")))
     ,(dx-style "character" "VerbatimChar" "Verbatim Char"
                `(w:rPr (w:rFonts (@ (w:ascii "Courier New") (w:hAnsi "Courier New")
                                     (w:cs "Courier New")))
                        (w:noProof)))))

(define (dx-utf8 n)
  ;; the character of code n < 65536, in UTF-8
  (list->string
    (map integer->char
         (cond ((< n #x80) (list n))
               ((< n #x800) (list (+ #xc0 (quotient n 64)) (+ #x80 (modulo n 64))))
               (else (list (+ #xe0 (quotient n 4096))
                           (+ #x80 (modulo (quotient n 64) 64))
                           (+ #x80 (modulo n 64))))))))

(define (dx-level kind i)
  ;; the level i of a list of bullets or of numbers
  `(w:lvl (@ (w:ilvl ,(number->string i)))
          ,(dx-val 'w:start "1")
          ,(dx-val 'w:numFmt (if (== kind "bullet") "bullet" "decimal"))
          ,(dx-val 'w:lvlText
                   (if (== kind "bullet")
                       (list-ref (list (dx-utf8 #x2022) (dx-utf8 #x25e6) (dx-utf8 #x25aa))
                                 (modulo i 3))
                       (string-append "%" (number->string (+ i 1)) ".")))
          ,(dx-val 'w:lvlJc "left")
          (w:pPr (w:ind (@ (w:left ,(number->string (* 720 (+ i 1))))
                           (w:hanging "360"))))))

(define (dx-numbering)
  ;; two abstract lists, of bullets and of numbers, and a list of one of
  ;; them for each list of the text, which starts at 1 at its level
  `(w:numbering
     (@ (xmlns:w ,dx-ns-w))
     ,@(map (lambda (kind id)
              `(w:abstractNum (@ (w:abstractNumId ,id))
                              ,(dx-val 'w:multiLevelType "hybridMultilevel")
                              ,@(map (lambda (i) (dx-level kind i)) (iota 9))))
            '("bullet" "number") '("1" "2"))
     ,@(map (lambda (l)
              `(w:num (@ (w:numId ,(number->string (car l))))
                      ,(dx-val 'w:abstractNumId (if (== (cadr l) "bullet") "1" "2"))
                      (w:lvlOverride (@ (w:ilvl ,(number->string (min 8 (caddr l)))))
                                     ,(dx-val 'w:startOverride "1"))))
            (reverse dx-lists))))

(define (dx-footnotes)
  `(w:footnotes
     (@ (xmlns:w ,dx-ns-w) (xmlns:r ,dx-ns-r) (xmlns:m ,dx-ns-m))
     (w:footnote (@ (w:type "separator") (w:id "-1"))
                 (w:p (w:r (w:separator))))
     (w:footnote (@ (w:type "continuationSeparator") (w:id "0"))
                 (w:p (w:r (w:continuationSeparator))))
     ,@(reverse dx-notes)))

(define (dx-settings)
  `(w:settings
     (@ (xmlns:w ,dx-ns-w))
     (w:footnotePr (w:footnote (@ (w:id "-1"))) (w:footnote (@ (w:id "0"))))
     (w:compat (w:compatSetting
                 (@ (w:name "compatibilityMode")
                    (w:uri "http://schemas.microsoft.com/office/word")
                    (w:val "15"))))))

(define (dx-relations l)
  ;; the file of the relations (id type target external?) of l
  `(Relationships
     (@ (xmlns "http://schemas.openxmlformats.org/package/2006/relationships"))
     ,@(map (lambda (r)
              `(Relationship
                 (@ (Id ,(car r))
                    (Type ,(string-append dx-rel-type (cadr r)))
                    (Target ,(caddr r))
                    ,@(if (cadddr r) '((TargetMode "External")) '()))))
            l)))

(define (dx-content-types)
  (let ((default (lambda (ext type)
                   `(Default (@ (Extension ,ext) (ContentType ,type)))))
        (override (lambda (part type)
                    `(Override
                       (@ (PartName ,part)
                          (ContentType
                            ,(string-append
                               "application/vnd.openxmlformats-officedocument."
                               "wordprocessingml." type "+xml")))))))
    `(Types
       (@ (xmlns "http://schemas.openxmlformats.org/package/2006/content-types"))
       ,(default "rels" "application/vnd.openxmlformats-package.relationships+xml")
       ,(default "xml" "application/xml")
       ,(default "png" "image/png")
       ,(default "jpg" "image/jpeg")
       ,(default "jpeg" "image/jpeg")
       ,(default "gif" "image/gif")
       ,(default "svg" "image/svg+xml")
       ,(override "/word/document.xml" "document.main")
       ,(override "/word/styles.xml" "styles")
       ,(override "/word/numbering.xml" "numbering")
       ,(override "/word/footnotes.xml" "footnotes")
       ,(override "/word/settings.xml" "settings"))))

(define (dx-document blocks)
  `(w:document
     (@ (xmlns:w ,dx-ns-w) (xmlns:r ,dx-ns-r) (xmlns:m ,dx-ns-m)
        (xmlns:wp ,dx-ns-wp))
     (w:body
       ,@blocks
       (w:sectPr
         (w:pgSz (@ (w:w "11906") (w:h "16838")))
         (w:pgMar (@ (w:top "1440") (w:right "1440") (w:bottom "1440")
                     (w:left "1440") (w:header "720") (w:footer "720")
                     (w:gutter "0")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (serialize-docx-document x)
  (:type (-> stree string))
  (:synopsis "Write the office tree @x as a Word document, a zip archive")
  (set! dx-rels '())
  (set! dx-media '())
  (set! dx-notes '())
  (set! dx-lists '())
  (set! dx-id 0)
  (set! dx-in-note? #f)
  (set! dx-item #f)
  (set! dx-indent 0)
  (set! dx-cell-align #f)
  ;; the relations of the files which are always there come first
  (dx-relation "styles" "styles.xml" #f)
  (dx-relation "numbering" "numbering.xml" #f)
  (dx-relation "footnotes" "footnotes.xml" #f)
  (dx-relation "settings" "settings.xml" #f)
  (let* ((blocks (dx-blocks (if (func? x 'office) (cdr x) (list x))))
         ;; a document has a paragraph at least
         (blocks (if (null? blocks) (list '(w:p)) blocks))
         (media (reverse dx-media))
         (r (zip-pack
              (append
                '("[Content_Types].xml" "_rels/.rels" "word/document.xml"
                  "word/_rels/document.xml.rels" "word/styles.xml"
                  "word/numbering.xml" "word/footnotes.xml" "word/settings.xml")
                (map (lambda (m) (string-append "word/media/" (car m))) media))
              (append
                (list (ox-serialize (dx-content-types))
                      (ox-serialize
                        (dx-relations
                          '(("rId1" "officeDocument" "word/document.xml" #f))))
                      (ox-serialize (dx-document blocks))
                      (ox-serialize (dx-relations (reverse dx-rels)))
                      (ox-serialize (dx-styles))
                      (ox-serialize (dx-numbering))
                      (ox-serialize (dx-footnotes))
                      (ox-serialize (dx-settings)))
                (map cadr media)))))
    (set! dx-rels '())
    (set! dx-media '())
    (set! dx-notes '())
    (set! dx-lists '())
    r))
