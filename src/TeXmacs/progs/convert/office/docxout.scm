
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
;; (word/media) and the relations which tell where all these are: those of
;; the text (word/_rels/document.xml.rels) and those of the notes, when
;; they have links or images (word/_rels/footnotes.xml.rels).

(texmacs-module (convert office docxout)
  (:use (convert office office-tools)
        (convert office omml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State: the document which is written
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dx-rels '())        ; the relations of the text, reversed
(define dx-note-rels '())   ; the relations of the notes, reversed
(define dx-rel-ids (make-ahash-table)) ; a relation of a file -> its identifier
(define dx-media '())       ; (name data) of the images, reversed
(define dx-media-files (make-ahash-table)) ; the bytes of an image -> its files
(define dx-notes '())       ; the elements of the notes, reversed
(define dx-lists '())       ; (id format level start) of the lists, reversed
(define dx-id 0)            ; the last number given to a thing
(define dx-in-note? #f)     ; inside a note, whose relations are its own
(define dx-item #f)         ; (list level) for the next paragraph of an item
(define dx-indent 0)        ; the depth of the lists around a paragraph
(define dx-cell-align #f)   ; the alignment of the cell around a paragraph
(define dx-numbered? #f)    ; the headings are numbered by Word
(define dx-setup '(setup))  ; the page and the font of the document
(define dx-comments '())    ; the comments, reversed
(define dx-used-styles '()) ; (id family name display) of the named styles of the text
(define dx-sections? #f)    ; whether the text has sections in columns
(define dx-italic? #f)      ; the paragraph is italic by its style (Theorem)
(define dx-package #f)      ; the entries of the file the text was read from
(define dx-num-offset 0)    ; the lists of this file come first
(define dx-abstract-offset 0)
(define dx-bookmarks (make-ahash-table)) ; a label -> its bookmark
(define dx-bookmark-names (make-ahash-table)) ; the bookmarks which are used
(define dx-bookmarks-written (make-ahash-table)) ; the bookmarks in the file
(define dx-no-sections? #f) ; in a cell, a note, a comment, a box or columns
(define dx-source-styles '()) ; the nodes style of the document

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
  ;; The identifier of the relation to this target, of the text or of the
  ;; notes: each of the two files has its own relations. The same target
  ;; has one relation in a file.
  (let* ((key (list dx-in-note? type target external?))
         (old (ahash-ref dx-rel-ids key)))
    (or old
        (let* ((l (if dx-in-note? dx-note-rels dx-rels))
               (id (string-append (if dx-package "rIdTm" "rId")
                                  (number->string (+ (length l) 1))))
               (r (list id type target external?)))
          (if dx-in-note?
              (set! dx-note-rels (cons r dx-note-rels))
              (set! dx-rels (cons r dx-rels)))
          (ahash-set! dx-rel-ids key id)
          id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dx-run-properties props)
  ;; the element w:rPr for the wrappers around a text, in the order which
  ;; the format wants; props has tags and (color "#rrggbb")
  (let* ((has? (lambda (tag) (in? tag props)))
         (color (assq 'color (list-filter props pair?)))
         (rstyle (assq 'rstyle (list-filter props pair?)))
         (size (assq 'size (list-filter props pair?)))
         (font (assq 'font (list-filter props pair?)))
         (shade (assq 'shade (list-filter props pair?)))
         (ukind (assq 'ukind (list-filter props pair?)))
         (letters (assq 'letters (list-filter props pair?)))
         (l (append
              (cond ((has? 'code) (list (dx-val 'w:rStyle "VerbatimChar")))
                    ((has? 'hyperlink) (list (dx-val 'w:rStyle "Hyperlink")))
                    (rstyle (list (dx-val 'w:rStyle (cadr rstyle))))
                    (else '()))
              ;; (in the order of the format)
              (if font
                  (with f (cond ((== (cadr font) "sans") "Arial")
                                ((== (cadr font) "serif") "Times New Roman")
                                ((== (cadr font) "mono") "Courier New")
                                (else (cadr font)))
                    `((w:rFonts (@ (w:ascii ,f) (w:hAnsi ,f) (w:cs ,f)))))
                  '())
              (if (has? 'strong) '((w:b)) '())
              ;; (emphasis in an italic paragraph is upright, as in TeXmacs)
              (if (has? 'em) (if dx-italic? '((w:i (@ (w:val "0")))) '((w:i))) '())
              (if (has? 'caps) '((w:caps)) '())
              (if (has? 'smallcaps) '((w:smallCaps)) '())
              (if (and (has? 'strike) (not (has? 'dstrike))) '((w:strike)) '())
              (if (has? 'dstrike) '((w:dstrike)) '())
              (if color
                  (list (dx-val 'w:color (upcase-all (substring (cadr color) 1 7))))
                  '())
              (with v (and letters (dx-twips-of (cadr letters)))
                (if v (list (dx-val 'w:spacing v)) '()))
              (with n (and size (office-number (cadr size)))
                (if (and n (> n 0))
                    (with v (number->string (inexact->exact (round (* 2 n))))
                      (list (dx-val 'w:sz v) (dx-val 'w:szCs v)))
                    '()))
              ;; highlighted text: in a color of the highlighting of Word, or
              ;; shaded in another one
              (cond ((has? 'mark) (list (dx-val 'w:highlight "yellow")))
                    ((and shade (office-highlight-name (cadr shade)))
                     (list (dx-val 'w:highlight (office-highlight-name (cadr shade)))))
                    (else '()))
              (cond (ukind
                     ;; (the kinds of OpenDocument as those of Word)
                     (list (dx-val 'w:u (with k (cadr ukind)
                                          (cond ((== k "solid") "single")
                                                ((== k "dash") "dash")
                                                (else k))))))
                    ((has? 'underline) (list (dx-val 'w:u "single")))
                    (else '()))
              (if (and shade (not (office-highlight-name (cadr shade))))
                  `((w:shd (@ (w:val "clear") (w:color "auto")
                              (w:fill ,(upcase-all (substring (cadr shade) 1 7))))))
                  '())
              (cond ((has? 'sup) (list (dx-val 'w:vertAlign "superscript")))
                    ((has? 'sub) (list (dx-val 'w:vertAlign "subscript")))
                    (else '())))))
    (if (null? l) '() (list (cons 'w:rPr l)))))

(define (dx-run props . content)
  `(w:r ,@(dx-run-properties props) ,@content))

(define (dx-text s)
  `(w:t (@ (xml:space "preserve")) ,s))

(define (dx-text-runs s props)
  ;; the runs of a text with tabs and line breaks
  (let loop ((l (string->list s)) (cur '()) (acc '()))
    (let ((flush (lambda (acc)
                   (if (null? cur) acc
                       (cons (dx-run props (dx-text (list->string (reverse cur)))) acc)))))
      (cond ((null? l) (reverse (flush acc)))
            ((char=? (car l) #\tab)
             (loop (cdr l) '() (cons (dx-run props '(w:tab)) (flush acc))))
            ((char=? (car l) #\newline)
             (loop (cdr l) '() (cons (dx-run props '(w:br)) (flush acc))))
            (else (loop (cdr l) (cons (car l) cur) acc))))))

(define (dx-emu s)
  ;; a length in English Metric Units: 360000 are a centimeter
  (with cm (or (office-length->cm s) 5.0)
    (number->string (inexact->exact (round (* cm 360000))))))

(define (dx-image-files x n)
  ;; The names (bitmap svg) of the files of the picture x, the thing of
  ;; number n, in word/media; svg is #f when it has none. A picture which
  ;; is used several times is there once.
  (let* ((data (ox-attr x 'data))
         (svg (ox-attr x 'svg))
         (key (cons data (or svg "")))
         (name (or (ox-attr x 'name) "image.png"))
         (dot (let loop ((i (- (string-length name) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref name i) #\.) i)
                      (else (loop (- i 1))))))
         (suffix (if dot (locase-all (substring name (+ dot 1) (string-length name)))
                     "png")))
    (or (ahash-ref dx-media-files key)
        (let* ((prefix (if dx-package "tm-image" "image"))
               (file (string-append prefix (number->string n) "." suffix))
               (svg-file (and svg (string-append prefix (number->string n) ".svg"))))
          (set! dx-media (cons (list file data) dx-media))
          (when svg (set! dx-media (cons (list svg-file svg) dx-media)))
          (ahash-set! dx-media-files key (list file svg-file))
          (list file svg-file)))))

(define (dx-image x)
  ;; a picture in the line; its file goes to word/media
  (let* ((data (ox-attr x 'data))
         (n (dx-next-id))
         (cx (dx-emu (ox-attr x 'width)))
         ;; (5cm by 3cm without a size, as in the files of OpenDocument)
         (cy (dx-emu (if (office-length->cm (ox-attr x 'height)) (ox-attr x 'height) "3cm")))
         ;; (a picture on the web: an external relation to its address,
         ;; with %XX for what an address cannot hold, as for the links)
         ;; (a picture which the file read linked to, linked: the same,
         ;; as Word writes it, file:///C:\pics\a.png for a path of Windows)
         (web (and (not data) (string? (ox-attr x 'name)) (!= (ox-attr x 'name) "")
                   (or (office-web-address? (ox-attr x 'name))
                       (== (ox-attr x 'linked) "true"))
                   (office-link-target (ox-attr x 'name) #t))))
    (if (not (or data web)) '()
        (let* ((files (if web (list "image" #f) (dx-image-files x n)))
               (file (car files))
               (id (if web (dx-relation "image" web #t)
                       (dx-relation "image" (string-append "media/" file) #f)))
               (svg-id (and (cadr files)
                            (dx-relation "image"
                                         (string-append "media/" (cadr files)) #f))))
          (list
            `(w:r
               (w:drawing
                 (,@(dx-anchor-start x n)
                   (wp:extent (@ (cx ,cx) (cy ,cy)))
                   ,@(dx-anchor-wrap x)
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
                             (@ (,(if web 'r:link 'r:embed) ,id))
                             ,@(if svg-id
                                   `((a:extLst
                                       (a:ext
                                         (@ (uri "{96DAC541-7B7A-43D3-8B79-37D633B846F1}"))
                                         (asvg:svgBlip
                                           (@ (xmlns:asvg ,dx-ns-svg)
                                              (r:embed ,svg-id))))))
                                   '()))
                           ;; the parts of the picture which are cut
                           ,@(dx-crop (ox-attr x 'crop))
                           (a:stretch (a:fillRect)))
                         (pic:spPr
                           (a:xfrm (a:off (@ (x "0") (y "0")))
                                   (a:ext (@ (cx ,cx) (cy ,cy))))
                           (a:prstGeom (@ (prst "rect")) (a:avLst))))))))))))))

(define dx-ns-wps "http://schemas.microsoft.com/office/word/2010/wordprocessingShape")
(define dx-ns-mc "http://schemas.openxmlformats.org/markup-compatibility/2006")

(define (dx-fill c)
  ;; the fill of DrawingML for a color, none, or nothing
  (cond ((== c "none") '((a:noFill)))
        ((office-color c)
         => (lambda (v) `((a:solidFill (a:srgbClr (@ (val ,(upcase-all (substring v 1 7)))))))))
        (else '())))

(define (dx-box x)
  ;; a box of text or a shape: a shape of Word (wps), in its line or
  ;; floating beside its paragraph, with the blocks of its text
  (let* ((n (dx-next-id))
         (cx (dx-emu (or (ox-attr x 'width) "2cm")))
         (cy (dx-emu (or (ox-attr x 'height) "1cm")))
         (shape (office-shape-for-word (ox-attr x 'shape)))
         (blocks (ox-children x))
         (lw (with v (office-number (ox-attr x 'line-width))
               (and v (> v 0) (number->string (inexact->exact (round (* v 12700)))))))
         (line (with f (dx-fill (ox-attr x 'line))
                 (cond ((and (null? f) (not lw)) '())
                       (else `((a:ln (@ ,@(if lw `((w ,lw)) '())) ,@f))))))
         (text (if (null? blocks) '()
                   (with l (dx-story (dx-story-blocks blocks))
                     `((wps:txbx (w:txbxContent ,@l)))))))
    (list
      `(w:r
         (mc:AlternateContent
           (@ (xmlns:mc ,dx-ns-mc) (xmlns:wps ,dx-ns-wps))
           (mc:Choice
             (@ (Requires "wps"))
             (w:drawing
               (,@(dx-anchor-start x n)
                 (wp:extent (@ (cx ,cx) (cy ,cy)))
                 ,@(dx-anchor-wrap x)
                 (wp:docPr (@ (id ,(number->string n))
                              (name ,(string-append (if (null? blocks) "Shape " "Text Box ")
                                                    (number->string n)))))
                 (a:graphic (@ (xmlns:a ,dx-ns-a))
                   (a:graphicData (@ (uri ,dx-ns-wps))
                     (wps:wsp
                       (wps:cNvSpPr ,@(if (and (pair? blocks) (== shape "rect"))
                                          '((@ (txBox "1"))) '()))
                       (wps:spPr
                         (a:xfrm (a:off (@ (x "0") (y "0"))) (a:ext (@ (cx ,cx) (cy ,cy))))
                         (a:prstGeom (@ (prst ,shape)) (a:avLst))
                         ,@(dx-fill (ox-attr x 'fill))
                         ,@line)
                       ,@text
                       (wps:bodyPr (@ (rot "0") (vert "horz") (wrap "square")
                                      ,@(dx-insets (ox-attr x 'inset)) (anchor "t"))
                         ,(if (== (ox-attr x 'grow) "true") '(a:spAutoFit) '(a:noAutofit))))))))))))))

(define (dx-insets s)
  ;; the space around the text of a box, "l,t,r,b" in cm, in EMU
  (let* ((l (and s (map office-number (string-tokenize-by-char s #\,))))
         (l (if (and l (== (length l) 4) (list-and l)) l '(0.254 0.127 0.254 0.127))))
    (map (lambda (k v) (list k (number->string (inexact->exact (round (* v 360000))))))
         '(lIns tIns rIns bIns) l)))

(define (dx-anchor-start x n)
  ;; the element wp:inline of a picture in the line, or wp:anchor and its
  ;; position for one which floats beside its paragraph
  (if (not (ox-attr x 'float))
      '(wp:inline (@ (distT "0") (distB "0") (distL "0") (distR "0")))
      (let* ((get (lambda (k) (ox-attr x k)))
             (emu (lambda (v)
                    (with c (cond ((not (string? v)) 0)
                                  ((string-starts? v "-")
                                   (- (or (office-length->cm (substring v 1 (string-length v))) 0)))
                                  (else (or (office-length->cm v) 0)))
                      (number->string (inexact->exact (round (* c 360000)))))))
             (h-pos (or (get 'h-pos) (get 'float)))
             (v-pos (or (get 'v-pos) "top"))
             (h-rel (or (assoc-ref '(("column" . "column") ("page" . "page")
                                     ("margin" . "margin") ("character" . "character")
                                     ("left-margin" . "leftMargin")
                                     ("right-margin" . "rightMargin"))
                                   (or (get 'h-rel) ""))
                        "column"))
             (v-rel (or (assoc-ref '(("paragraph" . "paragraph") ("line" . "line")
                                     ("page" . "page") ("margin" . "margin")
                                     ("top-margin" . "topMargin")
                                     ("bottom-margin" . "bottomMargin"))
                                   (or (get 'v-rel) ""))
                        "paragraph"))
             (around? (in? (get 'wrap) '(#f "square" "tight" "through"))))
        `(wp:anchor
           (@ (distT "0") (distB "0")
              (distL ,(if around? "114300" "0")) (distR ,(if around? "114300" "0"))
              (simplePos "0") (relativeHeight ,(number->string (+ 251658240 n)))
              (behindDoc ,(if (== (get 'wrap) "behind") "1" "0"))
              (locked "0") (layoutInCell "1") (allowOverlap "1"))
           (wp:simplePos (@ (x "0") (y "0")))
           (wp:positionH (@ (relativeFrom ,h-rel))
             ,(if (in? h-pos '("left" "right" "center" "inside" "outside"))
                  `(wp:align ,h-pos)
                  `(wp:posOffset ,(emu (get 'h-offset)))))
           (wp:positionV (@ (relativeFrom ,v-rel))
             ,(if (in? v-pos '("top" "bottom" "center" "inside" "outside"))
                  `(wp:align ,v-pos)
                  `(wp:posOffset ,(emu (get 'v-offset)))))))))

(define (dx-anchor-wrap x)
  ;; how the text goes around a picture which floats (after its size)
  (if (not (ox-attr x 'float)) '()
      (let* ((side (or (assoc-ref '(("both" . "bothSides") ("left" . "left")
                                    ("right" . "right") ("largest" . "largest"))
                                  (or (ox-attr x 'wrap-side) ""))
                       "bothSides"))
             (polygon '(wp:wrapPolygon (@ (edited "0"))
                         (wp:start (@ (x "0") (y "0")))
                         (wp:lineTo (@ (x "0") (y "21600")))
                         (wp:lineTo (@ (x "21600") (y "21600")))
                         (wp:lineTo (@ (x "21600") (y "0")))
                         (wp:lineTo (@ (x "0") (y "0"))))))
        `((wp:effectExtent (@ (l "0") (t "0") (r "0") (b "0")))
          ,(case (string->symbol (or (ox-attr x 'wrap) "square"))
             ((tight) `(wp:wrapTight (@ (wrapText ,side)) ,polygon))
             ((through) `(wp:wrapThrough (@ (wrapText ,side)) ,polygon))
             ((top-bottom) '(wp:wrapTopAndBottom))
             ((front behind) '(wp:wrapNone))
             (else `(wp:wrapSquare (@ (wrapText ,side)))))))))

(define (dx-crop crop)
  ;; the element a:srcRect for "l t r b", fractions of the sides
  (with l (and crop (map office-number (string-tokenize-by-char crop #\space)))
    (if (not (and l (== (length l) 4) (list-and l))) '()
        (list `(a:srcRect
                 (@ ,@(map (lambda (k v)
                             (list k (number->string (inexact->exact (round (* v 100000))))))
                           '(l t r b) l)))))))

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

(define (dx-styled-paragraph p style)
  ;; the paragraph p with this style, unless it has one of its own
  (let* ((c (ox-children p))
         (ppr (list-find c (lambda (y) (func? y 'w:pPr))))
         (rest (list-filter c (lambda (y) (not (func? y 'w:pPr))))))
    (cond ((and ppr (list-find (cdr ppr) (lambda (y) (func? y 'w:pStyle)))) p)
          (ppr `(w:p (w:pPr ,(dx-val 'w:pStyle style) ,@(cdr ppr)) ,@rest))
          (else `(w:p (w:pPr ,(dx-val 'w:pStyle style)) ,@rest)))))

(define (dx-paragraph-insert p l)
  ;; the paragraph p with the elements l at its start, after its properties
  (let* ((c (ox-children p))
         (ppr (list-filter c (lambda (y) (func? y 'w:pPr))))
         (rest (list-filter c (lambda (y) (not (func? y 'w:pPr))))))
    `(w:p ,@ppr ,@l ,@rest)))

(define (dx-story blocks)
  ;; the blocks of a note, a comment or a box: a paragraph at least, and
  ;; a paragraph at the end (Word wants it after a table)
  (cond ((null? blocks) (list '(w:p)))
        ((func? (cAr blocks) 'w:p) blocks)
        (else (append blocks (list '(w:p))))))

(define (dx-story-blocks l)
  ;; the blocks of the nodes l of a note, a comment or a box, where there
  ;; are no sections, no lists around and no alignment of a cell
  (let ((old-item dx-item)
        (old-indent dx-indent)
        (old-align dx-cell-align)
        (old-sections dx-no-sections?))
    (set! dx-item #f)
    (set! dx-indent 0)
    (set! dx-cell-align #f)
    (set! dx-no-sections? #t)
    (with r (dx-blocks l)
      (set! dx-item old-item)
      (set! dx-indent old-indent)
      (set! dx-cell-align old-align)
      (set! dx-no-sections? old-sections)
      r)))

(define (dx-note-inline x)
  ;; a note inside a note, which Word does not have: its text in
  ;; parentheses where it is
  (let* ((parts (map (lambda (b)
                       (if (func? b 'p) (dx-inlines (ox-children b) '())
                           (with t (ox-text b)
                             (if (== t "") '() (list (dx-run '() (dx-text t)))))))
                     (ox-children x)))
         (parts (list-filter parts pair?)))
    (if (null? parts) '()
        (append (list (dx-run '() (dx-text " (")))
                (apply append (list-intersperse parts (list (dx-run '() (dx-text " ")))))
                (list (dx-run '() (dx-text ")")))))))

(define (dx-note x)
  ;; the mark of a note; its text goes to word/footnotes.xml, each of its
  ;; paragraphs with the style of the notes, the mark at the start of the
  ;; first one
  (if dx-in-note? (dx-note-inline x)
      (let ((n (+ (length dx-notes) 1)))
        (set! dx-in-note? #t)
        (set! dx-note-comments '())
        (let* ((blocks (dx-story (dx-story-blocks (ox-children x))))
               (blocks (if (func? (car blocks) 'w:p) blocks (cons '(w:p) blocks)))
               (blocks (map (lambda (b)
                              (if (func? b 'w:p) (dx-styled-paragraph b "FootnoteText") b))
                            blocks))
               (first (dx-paragraph-insert
                        (car blocks)
                        (list `(w:r (w:rPr ,(dx-val 'w:rStyle "FootnoteReference"))
                                    (w:footnoteRef))
                              (dx-run '() (dx-text " "))))))
          (set! dx-in-note? #f)
          (set! dx-notes
                (cons `(w:footnote (@ (w:id ,(number->string n)))
                                   ,first ,@(cdr blocks))
                      dx-notes))
          (with comments (apply append (reverse dx-note-comments))
            (set! dx-note-comments '())
            (cons `(w:r (w:rPr ,(dx-val 'w:rStyle "FootnoteReference"))
                        (w:footnoteReference (@ (w:id ,(number->string n)))))
                  comments))))))

(define dx-in-comment? #f)  ; inside a comment, which has no comments
(define dx-note-comments '()) ; the marks of the comments of a note, reversed

(define (dx-comment x)
  ;; the mark of a comment, in the text or in a note; its text goes to
  ;; word/comments.xml
  (if dx-in-comment? '()
      (let ((id (number->string (length dx-comments)))
            (old-note dx-in-note?))
        (set! dx-in-note? #t)
        (set! dx-in-comment? #t)
        (let* ((blocks (dx-story (dx-story-blocks (ox-children x)))))
          (set! dx-in-note? old-note)
          (set! dx-in-comment? #f)
          (set! dx-comments
                (cons `(w:comment (@ (w:id ,id)
                                     (w:author ,(or (ox-attr x 'author) "TeXmacs"))
                                     ,@(if (ox-attr x 'date) `((w:date ,(ox-attr x 'date))) '()))
                                  ,@blocks)
                      dx-comments))
          (with marks (list `(w:commentRangeStart (@ (w:id ,id)))
                            `(w:commentRangeEnd (@ (w:id ,id)))
                            `(w:r (w:commentReference (@ (w:id ,id)))))
            ;; (a comment in a note is at the mark of the note in the
            ;; text: LibreOffice does not read the file of Word which has
            ;; a comment in a note)
            (if old-note
                (begin (set! dx-note-comments (cons marks dx-note-comments)) '())
                marks))))))

(define dx-known-styles #f) ; (name type id) of the styles of the file

(define dx-word-names
  ;; the names of the styles which Word has (in lower case)
  (append '("normal" "title" "subtitle" "caption" "quote" "intense quote"
            "list paragraph" "no spacing" "footnote text" "footnote reference"
            "endnote text" "endnote reference" "header" "footer" "hyperlink"
            "followedhyperlink" "strong" "emphasis" "intense emphasis"
            "subtle emphasis" "subtle reference" "intense reference" "book title"
            "default paragraph font" "body text" "toc heading" "bibliography"
            "table grid" "normal table" "no list" "comment text"
            "comment reference" "balloon text" "plain text" "block text"
            "list bullet" "list number" "macro text" "date" "signature"
            "salutation" "table of figures")
          (map (lambda (i) (string-append "heading " (number->string i))) (cdr (iota 10)))
          (map (lambda (i) (string-append "toc " (number->string i))) (cdr (iota 10)))))

(define dx-package-ids '()) ; the ids of the styles of the file of Word
(define dx-style-ids (make-ahash-table)) ; (name . family) -> id of dx-use-style
(define dx-claimed (make-ahash-table))   ; id of the file -> (name . family)
(define dx-restyled (make-ahash-table))  ; id of TeXmacs -> (type . node style)

(define (dx-known)
  ;; the styles of the file which is written, before the named ones
  (when (not dx-known-styles)
    (set! dx-package-ids
          (with root (dx-package-xml "word/styles.xml")
            (if root (map (lambda (y) (ox-attr y 'w:styleId)) (ox-childs root 'w:style))
                '())))
    (set! dx-known-styles
          (map (lambda (y)
                 (list (locase-all (or (with n (ox-child y 'w:name) (and n (ox-attr n 'w:val)))
                                       ""))
                       (ox-attr y 'w:type) (ox-attr y 'w:styleId)))
               (ox-childs (dx-merged-styles (dx-styles)) 'w:style))))
  dx-known-styles)

(define (dx-own-look? st)
  ;; whether the node style st says a look of its own
  (and st (list-or (map (lambda (k) (ox-attr st k)) office-macro-style-keys))))

(define (dx-use-style name family)
  ;; The style id of Word of the named style of the text, which the
  ;; styles get (once for each name and family).
  (if (== name "") ""
      (let ((key (cons name family)))
        (or (ahash-ref dx-style-ids key)
            (with id (dx-use-style* name family)
              (ahash-set! dx-style-ids key id)
              id)))))

(define (dx-use-style* name family)
  ;; A style of the file of this id is this one; a style of the file which
  ;; has the name of this one (a style Heading_20_1 of OpenDocument is
  ;; Word's heading 1) is this one, unless another style of the text is
  ;; already this one (heading 1 and Heading1 stay two styles); a style
  ;; of TeXmacs which is this one has the look of this one when it says
  ;; one. Else a new style whose id and name are not those of another one
  ;; (a paragraph and a text of the same name are MyNote and MyNoteChar).
  (let* ((type (if (== family "paragraph") "paragraph" "character"))
         (key (cons name family))
         (known (dx-known))
         (same (list-find known (lambda (k) (and (== (caddr k) name)
                                                 (== (cadr k) type)))))
         (st (dx-source-style name (if (== family "paragraph") "paragraph" "text")))
         (display (or (and st (ox-attr st 'display)) (office-odf-name name)))
         (named (list-find known (lambda (k) (and (== (car k) (locase-all display))
                                                  (== (cadr k) type)))))
         (target (cond (same name) (named (caddr named)) (else #f)))
         (claim (and target (ahash-ref dx-claimed target))))
    (if (and target (or (not claim) (== claim key)))
        (begin
          (ahash-set! dx-claimed target key)
          (when (and (dx-own-look? st) (not (in? target dx-package-ids)))
            (ahash-set! dx-restyled target (cons type st)))
          target)
        (let* ((ids (append (map caddr known) (map car dx-used-styles)))
               (names (append (map car known) dx-word-names
                              (map (lambda (u) (locase-all (cadddr u)))
                                   dx-used-styles)))
               (suffix (if (== type "character") "Char" "Par"))
               (id (let loop ((i 0))
                     (with c (string-append name (if (== i 0) "" suffix)
                                            (if (> i 1) (number->string i) ""))
                       (if (in? c ids) (loop (+ i 1)) c))))
               (show (let loop ((i 0))
                       (with c (string-append display
                                              (cond ((== i 0) "")
                                                    ((== type "character") " Char")
                                                    (else " Style"))
                                              (if (> i 1) (string-append " " (number->string i)) ""))
                         (if (in? (locase-all c) names) (loop (+ i 1)) c)))))
          (set! dx-used-styles
                (cons (list id family name show) dx-used-styles))
          id))))

(define (dx-word-name? name)
  ;; a name of a bookmark of Word, which is kept: letters of ASCII, digits
  ;; and _, a letter or _ first (_Toc1, _Ref2, fig1), 40 characters at most
  (and (> (string-length name) 0) (<= (string-length name) 40)
       (with c (string-ref name 0)
         (or (char=? c #\_) (and (char-alphabetic? c) (< (char->integer c) 128))))
       (list-and (map (lambda (c) (and (< (char->integer c) 128)
                                       (or (char-alphabetic? c) (char-numeric? c)
                                           (char=? c #\_))))
                      (string->list name)))))

(define (dx-bookmark-name name)
  ;; The name of the bookmark of a label. Word wants letters, digits and
  ;; _ only, a letter first, 40 characters at most: thm:main is tm_thm_main.
  (or (ahash-ref dx-bookmarks name)
      (and (dx-word-name? name) (not (ahash-ref dx-bookmark-names name))
           (begin (ahash-set! dx-bookmark-names name #t)
                  (ahash-set! dx-bookmarks name name)
                  name))
      (let* ((safe (list->string
                     (map (lambda (c)
                            (if (and (< (char->integer c) 128)
                                     (or (char-alphabetic? c) (char-numeric? c)))
                                c #\_))
                          (string->list name))))
             (base (string-append
                     "tm_" (substring safe 0 (min 30 (string-length safe)))))
             (unique (let loop ((n 1) (s base))
                       (if (ahash-ref dx-bookmark-names s)
                           (loop (+ n 1) (string-append base "_" (number->string n)))
                           s))))
        (ahash-set! dx-bookmark-names unique #t)
        (ahash-set! dx-bookmarks name unique)
        unique)))

(define (dx-new-bookmark? name)
  ;; whether the bookmark of this name is not in the file yet (a label
  ;; which is twice in the text has one bookmark, at its first place)
  (and (not (ahash-ref dx-bookmarks-written name))
       (begin (ahash-set! dx-bookmarks-written name #t) #t)))

(define (dx-bookmarked labels l)
  ;; the elements l inside the bookmarks of these labels
  (let* ((names (list-filter (map dx-bookmark-name labels) dx-new-bookmark?))
         (ids (map (lambda (name) (number->string (dx-next-id))) names)))
    (append (map (lambda (name id)
                   `(w:bookmarkStart (@ (w:id ,id) (w:name ,name))))
                 names ids)
            l
            (map (lambda (id) `(w:bookmarkEnd (@ (w:id ,id)))) (reverse ids)))))

(define (dx-sequence-name s)
  ;; the name of a sequence for Word: its letters, those of ASCII and
  ;; those of the other alphabets (the bytes of their characters), without
  ;; spaces, digits and punctuation
  (with t (list->string
            (list-filter (string->list s)
                         (lambda (c) (or (>= (char->integer c) 128)
                                         (char-alphabetic? c)))))
    (if (== t "") "Sequence" t)))

(define (dx-field-runs instruction runs)
  ;; A field in a paragraph: the runs which begin it, hold its instruction,
  ;; separate it from its result, which is runs, and end it. (The short
  ;; form w:fldSimple is not read by all programs.)
  `((w:r (w:fldChar (@ (w:fldCharType "begin"))))
    (w:r (w:instrText (@ (xml:space "preserve")) ,instruction))
    (w:r (w:fldChar (@ (w:fldCharType "separate"))))
    ,@runs
    (w:r (w:fldChar (@ (w:fldCharType "end"))))))

(define (dx-seq x props)
  ;; A number of a sequence: a field SEQ, which Word counts, inside the
  ;; bookmarks of its labels, so that a reference to one of them shows
  ;; this number.
  (let* ((name (ox-attr x 'name))
         (runs (dx-inlines (ox-children x) props)))
    (dx-bookmarked
      (office-labels x)
      (if (and name (!= name ""))
          (dx-field-runs (string-append " SEQ " (dx-sequence-name name)
                                        " \\* ARABIC ")
                         runs)
          runs))))

(define (dx-ref x props)
  ;; A reference: a field REF to the bookmark of a number, which shows
  ;; this number (the one of the paragraph, for a heading); else a link.
  (let* ((name (or (ox-attr x 'name) ""))
         (kind (ox-attr x 'kind))
         (l (ox-children x)))
    (cond ((== name "") (dx-inlines l props))
          ;; (a link has no link inside it: its text only)
          ((in? 'hyperlink props) (dx-inlines l props))
          ;; the page of the label, which Word computes
          ((== kind "page")
           (with runs (dx-inlines l props)
             (if (null? runs) '()
                 (dx-field-runs (string-append " PAGEREF " (dx-bookmark-name name) " \\h ")
                                runs))))
          ((in? kind '("seq" "heading"))
           (with runs (dx-inlines l props)
             (if (null? runs) '()
                 (dx-field-runs (string-append
                                  " REF " (dx-bookmark-name name)
                                  (if (== kind "heading") " \\r" "")
                                  " \\h ")
                                runs))))
          (else
            (with runs (dx-inlines l (cons 'hyperlink props))
              (if (null? runs) '()
                  (list `(w:hyperlink (@ (w:anchor ,(dx-bookmark-name name)))
                                      ,@runs))))))))

(define (dx-inline x props)
  ;; the elements of a paragraph for an inline node inside the wrappers
  ;; props
  (cond ((string? x)
         (cond ((== x "") '())
               ;; (the text which was deleted, while the changes are tracked)
               ((in? 'deleted props)
                (list (dx-run props `(w:delText (@ (xml:space "preserve")) ,x))))
               ;; (a tab and a line break are elements of their own)
               ((or (>= (string-search-forwards "\t" 0 x) 0)
                    (>= (string-search-forwards "\n" 0 x) 0))
                (dx-text-runs x props))
               (else (list (dx-run props (dx-text x))))))
        ((not (pair? x)) '())
        (else
          (let ((l (ox-children x)))
            (case (car x)
              ;; a named style of text
              ((span)
               (with id (dx-use-style (or (ox-attr x 'style) "") "character")
                 (dx-inlines l (cons (list 'rstyle id) props))))
              ;; the changes which are tracked
              ((ins del)
               (with runs (dx-inlines l (if (func? x 'del) (cons 'deleted props) props))
                 (if (null? runs) '()
                     (list `(,(if (func? x 'ins) 'w:ins 'w:del)
                             (@ (w:id ,(number->string (dx-next-id)))
                                (w:author ,(or (ox-attr x 'author) "TeXmacs"))
                                ,@(if (ox-attr x 'date) `((w:date ,(ox-attr x 'date))) '()))
                             ,@runs)))))
              ;; a field, with its instruction and its result
              ((field)
               (with runs (dx-inlines l props)
                 (if (ox-attr x 'code) (dx-field-runs (ox-attr x 'code) runs) runs)))
              ((comment) (dx-comment x))
              ((em strong sub sup code smallcaps)
               (dx-inlines l (cons (car x) props)))
              ;; capitals; Word has no lower case, which is written as
              ;; it is shown, nor first letters in capitals
              ((caps)
               (cond ((== (ox-attr x 'value) "lower")
                      (dx-inlines (map dx-lower-case l) props))
                     ((== (ox-attr x 'value) "capital") (dx-inlines l props))
                     (else (dx-inlines l (cons 'caps props)))))
              ;; an underline or a line through of a kind, letters spaced
              ((underline)
               (dx-inlines l (if (ox-attr x 'value)
                                 (cons* (list 'ukind (ox-attr x 'value)) 'underline props)
                                 (cons 'underline props))))
              ((strike)
               (dx-inlines l (if (== (ox-attr x 'value) "double")
                                 (cons* 'dstrike 'strike props)
                                 (cons 'strike props))))
              ((letters)
               (dx-inlines l (if (ox-attr x 'value)
                                 (cons (list 'letters (ox-attr x 'value)) props)
                                 props)))
              ;; highlighted text, of a color; the size and the font of a text
              ((mark)
               (with c (office-color (ox-attr x 'value))
                 (dx-inlines l (if (and c (!= c "#ffff00")) (cons (list 'shade c) props)
                                   (cons 'mark props)))))
              ((size font)
               (dx-inlines l (if (ox-attr x 'value)
                                 (cons (list (car x) (ox-attr x 'value)) props)
                                 props)))
              ((color)
               (dx-inlines l (if (office-color (ox-attr x 'value))
                                 (cons (list 'color (office-color (ox-attr x 'value)))
                                       props)
                                 props)))
              ((seq) (dx-seq x props))
              ((ref) (dx-ref x props))
              ((link)
               (let* ((href (or (ox-attr x 'href) ""))
                      (runs (dx-inlines l (cons 'hyperlink props))))
                 (cond ((null? runs) '())
                       ;; (a link has no link inside it: its text only)
                       ((in? 'hyperlink props) runs)
                       ((string-starts? href "#")
                        (list `(w:hyperlink
                                 (@ (w:anchor ,(dx-bookmark-name
                                                 (substring href 1 (string-length href)))))
                                 ,@runs)))
                       ((== href "") (dx-inlines l props))
                       (else
                         (list `(w:hyperlink
                                  (@ (r:id ,(dx-relation "hyperlink"
                                                         (office-uri href) #t)))
                                  ,@runs))))))
              ((bookmark)
               (if (ox-attr x 'at)
                   ;; (where a label was, away from its number)
                   (let ((id (number->string (dx-next-id)))
                         (name (dx-bookmark-name
                                 (string-append "_TmAt_"
                                                (dx-bookmark-name (or (ox-attr x 'name) ""))))))
                     (if (not (dx-new-bookmark? name)) '()
                         `((w:bookmarkStart (@ (w:id ,id) (w:name ,name)))
                           (w:bookmarkEnd (@ (w:id ,id))))))
                   (dx-bookmarked (list (or (ox-attr x 'name) "")) '())))
              ((note) (dx-note x))
              ((br) (list (dx-run '() '(w:br))))
              ((tab) (list (dx-run '() '(w:tab))))
              ((pagebreak) (list (dx-run '() '(w:br (@ (w:type "page"))))))
              ((image) (dx-image x))
              ((box) (dx-box x))
              ((math) (dx-math x))
              (else (dx-inlines l props)))))))

(define (dx-lower-case x)
  ;; the text of x in lower case (the letters of ASCII, as TeXmacs does)
  (cond ((string? x)
         (list->string (map (lambda (c) (if (< (char->integer c) 128) (char-downcase c) c))
                            (string->list x))))
        ((and (pair? x) (eq? (car x) '@)) x)
        ((pair? x) (cons (car x) (map dx-lower-case (cdr x))))
        (else x)))

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
    ("term" . "DefinitionTerm") ("definition" . "Definition")
    ("theorem" . "Theorem") ("remark" . "Remark") ("proof" . "Proof")
    ("bibitem" . "Bibliography")))

(define (dx-paragraph-style x)
  (with role (ox-attr x 'role)
    (cond ((== role "heading")
           (string-append "Heading" (or (ox-attr x 'level) "1")))
          ((assoc-ref dx-role-styles role) => identity)
          ;; a named style of the file, or the one of its usual paragraphs
          ((ox-attr x 'style) (dx-use-style (ox-attr x 'style) "paragraph"))
          ((and (ox-attr dx-setup 'body-style) (not dx-in-note?) (not dx-item))
           (dx-use-style (ox-attr dx-setup 'body-style) "paragraph"))
          (else #f))))

(define (dx-display-parts x)
  ;; (formula . rest) for a paragraph which starts with a formula on its own
  ;; lines, rest being what follows it but tabs (its number), or #f
  (with c (ox-children x)
    (and (pair? c) (func? (car c) 'math) (== (ox-attr (car c) 'display) "true")
         (cons (car c)
               (with rest (list-filter (cdr c)
                                       (lambda (y) (not (or (func? y 'tab)
                                                            (and (string? y)
                                                                 (== (string-trim-spaces y) ""))))))
                 (if (null? rest) '() (list-tail (cdr c)
                                                 (list-find-index (cdr c)
                                                                  (lambda (y) (not (func? y 'tab)))))))))))

(define (dx-paragraph x)
  ;; (a formula alone is displayed; a formula with its number is in the
  ;; middle of the line, and its number at the right, by two tabs)
  (with parts (dx-display-parts x)
    (cond ((not parts) (dx-paragraph* x #f))
          ((null? (cdr parts))
           (dx-paragraph* (apply office-node (cons* 'p (map (lambda (a) (list (car a) (cadr a)))
                                                          (ox-attrs x))
                                                  (list (car parts))))
                          #f))
          (else
            (let* ((m (car parts))
                   (inline `(math (@ ,@(list-filter (ox-attrs m)
                                                    (lambda (a) (!= (car a) 'display))))
                                  ,@(ox-children m))))
              (dx-paragraph* (apply office-node
                                    (cons* 'p (map (lambda (a) (list (car a) (cadr a)))
                                                   (ox-attrs x))
                                           (append (list '(tab) inline '(tab)) (cdr parts))))
                             #t))))))

(define (dx-twips-of s)
  ;; the number of points s in twentieths of a point, or #f
  (with v (office-number s)
    (and v (number->string (inexact->exact (round (* 20 v)))))))

(define (dx-spacing x)
  ;; the element w:spacing of the spaces of the paragraph x, by hand
  (let ((before (dx-twips-of (ox-attr x 'space-before)))
        (after (dx-twips-of (ox-attr x 'space-after)))
        (line (with v (office-number (ox-attr x 'line-spacing))
                (and v (> v 0) (number->string (inexact->exact (round (* 240 v))))))))
    (if (not (or before after line)) '()
        `((w:spacing (@ ,@(if before `((w:before ,before)) '())
                        ,@(if after `((w:after ,after)) '())
                        ,@(if line `((w:line ,line) (w:lineRule "auto")) '())))))))

(define (dx-indentation x list-left)
  ;; the element w:ind of the paragraph x: in a list, or by hand
  (let* ((left (or (dx-twips-of (ox-attr x 'indent-left))
                   (and list-left (number->string list-left))))
         (right (dx-twips-of (ox-attr x 'indent-right)))
         (first (office-number (ox-attr x 'first-indent))))
    (if (not (or left right first)) '()
        `((w:ind (@ ,@(if left `((w:left ,left)) '())
                    ,@(if right `((w:right ,right)) '())
                    ,@(cond ((not first) '())
                            ((< first 0) `((w:hanging ,(dx-twips-of (office-decimal (- first))))))
                            (else `((w:firstLine ,(dx-twips-of (office-decimal first))))))))))))

(define (dx-line-width)
  ;; the width of the text, in twentieths of a point
  (- (string->number (dx-twips 'page-width 11906))
     (string->number (dx-twips 'margin-left 1440))
     (string->number (dx-twips 'margin-right 1440))))

(define (dx-paragraph* x numbered?)
  (set! dx-italic? (== (dx-paragraph-style x) "Theorem"))
  (with r (dx-paragraph** x numbered?)
    (set! dx-italic? #f)
    r))

(define (dx-paragraph** x numbered?)
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
                   ;; a heading without a number, where the headings have
                   ;; one by their style
                   ((and dx-numbered? (== (ox-attr x 'role) "heading")
                         (not (ox-attr x 'number)))
                    `((w:numPr ,(dx-val 'w:numId "0"))))
                   (else '()))
             ;; (in the order of the format: borders, shading, tabs, spaces,
             ;; indentation, alignment)
             (if (ox-attr x 'border)
                 `((w:pBdr ,@(map (lambda (side)
                                    `(,side (@ (w:val "single") (w:sz "4") (w:space "4")
                                               (w:color "auto"))))
                                  '(w:top w:left w:bottom w:right))))
                 '())
             (with c (office-color (ox-attr x 'background))
               (if c `((w:shd (@ (w:val "clear") (w:color "auto")
                                 (w:fill ,(upcase-all (substring c 1 7))))))
                   '()))
             (if numbered?
                 `((w:tabs (w:tab (@ (w:val "center")
                                     (w:pos ,(number->string (quotient (dx-line-width) 2)))))
                           (w:tab (@ (w:val "right")
                                     (w:pos ,(number->string (dx-line-width)))))))
                 '())
             (dx-spacing x)
             (dx-indentation x (and (not item) (> dx-indent 0)
                                    (not (and dx-numbered? (== (ox-attr x 'role) "heading")
                                              (not (ox-attr x 'number))))
                                    (* 720 dx-indent)))
             (cond (numbered? '())
                   ((== align "center") (list (dx-val 'w:jc "center")))
                   ((== align "right") (list (dx-val 'w:jc "right")))
                   ((== align "left") (list (dx-val 'w:jc "left")))
                   ((== align "justify") (list (dx-val 'w:jc "both")))
                   (else '())))))
    (set! dx-item #f)
    (list `(w:p ,@(if (null? props) '() (list (cons 'w:pPr props)))
                ;; the bookmarks of the labels of a heading
                ,@(dx-bookmarked (office-labels x) '())
                ,@(dx-inlines (ox-children x) '())))))

(define (dx-list x)
  ;; a list: each one is a list of its own for Word, so that its numbers
  ;; start at 1 (or where it says), at the level of its depth
  (let* ((kind (if (== (ox-attr x 'kind) "number") "number" "bullet"))
         (format (if (== kind "number") (or (ox-attr x 'format) "1") "bullet"))
         (start (number->string (office-integer (ox-attr x 'start) 1 0 100000)))
         (level dx-indent)
         (id (+ (length dx-lists) 1 dx-num-offset))
         (old-item dx-item))
    (set! dx-lists (cons (list id format level start) dx-lists))
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
         (old-indent dx-indent)
         (old-sections dx-no-sections?))
    (set! dx-cell-align (ox-attr c 'align))
    (set! dx-item #f)
    (set! dx-indent 0)
    ;; (Word has no sections in a cell)
    (set! dx-no-sections? #t)
    (let* ((blocks (if (== vmerge 'continue) '() (dx-blocks (ox-children c))))
           ;; a cell ends with a paragraph
           (blocks (if (or (null? blocks) (not (func? (cAr blocks) 'w:p)))
                       (append blocks (list '(w:p)))
                       blocks))
           (fill (office-color (ox-attr c 'background))))
      (set! dx-cell-align old-align)
      (set! dx-item old-item)
      (set! dx-indent old-indent)
      (set! dx-no-sections? old-sections)
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
         (row-at (list->vector rows))
         (cell-at (lambda (i j)
                    (with cells (ox-children (vector-ref row-at i))
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
             (let* ((cells (ox-children (vector-ref row-at i)))
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

(define (dx-field-around instruction l)
  ;; the paragraphs l of Word as the result of a field: it begins in the
  ;; first one (after its properties) and ends in the last one
  (let* ((start (reverse (cdr (reverse (dx-field-runs (or instruction "") '())))))
         (end (list (cAr (dx-field-runs "" '()))))
         (para? (lambda (p) (func? p 'w:p)))
         (with-start
           (lambda (p)
             (if (and (pair? (cdr p)) (func? (cadr p) 'w:pPr))
                 (cons* 'w:p (cadr p) (append start (cddr p)))
                 (cons 'w:p (append start (cdr p)))))))
    (cond ((not instruction) l)
          ;; (a field without its result: one paragraph with the field)
          ((null? l) (list (cons 'w:p (append start end))))
          (else
            (let* ((l (if (para? (car l)) (cons (with-start (car l)) (cdr l))
                          (cons (cons 'w:p start) l)))
                   (r (reverse l))
                   (r (if (para? (car r)) (cons (append (car r) end) (cdr r))
                          (cons (cons 'w:p end) r))))
              (reverse r))))))

(define (dx-field instruction text)
  ;; a field which Word computes: its instruction and what is shown before
  `(w:p (w:r (w:fldChar (@ (w:fldCharType "begin"))))
        (w:r (w:instrText (@ (xml:space "preserve")) ,instruction))
        (w:r (w:fldChar (@ (w:fldCharType "separate"))))
        ,(dx-run '() (dx-text text))
        (w:r (w:fldChar (@ (w:fldCharType "end"))))))

(define (dx-columns x)
  ;; a section in columns, between two ends of sections; the columns
  ;; inside it are its blocks
  (let* ((n (number->string (office-integer (ox-attr x 'count) 2 1 10)))
         (old dx-no-sections?)
         (l (begin (set! dx-no-sections? #t) (dx-blocks (ox-children x)))))
    (set! dx-no-sections? old)
    (append (list (dx-section-end "1")) l
            (list (dx-section-end n (ox-attr x 'line)
                                  (with c (office-length->cm (ox-attr x 'gap))
                                    (and c (number->string
                                             (inexact->exact (round (* c 567)))))))))))

(define (dx-block x)
  (cond ((not (pair? x)) '())
        (else
          (case (car x)
            ((p) (dx-paragraph x))
            ((list) (dx-list x))
            ((table) (list (dx-table x)))
            ((pagebreak) (list `(w:p ,(dx-run '() '(w:br (@ (w:type "page")))))))
            ;; a field over several paragraphs (a bibliography)
            ((field-blocks)
             (dx-field-around (ox-attr x 'code) (dx-blocks (ox-children x))))
            ;; a section in columns, between two ends of sections
            ;; (in a cell, a note, a comment, a box or other columns,
            ;; where Word has no sections, the blocks only)
            ((columns)
             (if dx-no-sections? (dx-blocks (ox-children x))
                 (begin
                   (set! dx-sections? #t)
                   (dx-columns x))))
            ((rule)
             (list `(w:p (w:pPr (w:pBdr (w:bottom (@ (w:val "single") (w:sz "6")
                                                     (w:space "1") (w:color "auto"))))))))
            ((toc)
             ;; its title, and a field which Word fills from the headings
             (append
               (with title (dx-inlines (ox-children x) '())
                 (if (null? title) '()
                     (list `(w:p (w:pPr ,(dx-val 'w:pStyle "TOCHeading"))
                                 ,@title))))
               (list (dx-field " TOC \\o \"1-3\" \\h \\z \\u "
                               "Update this field to see the table of contents."))))
            (else '())))))

(define (dx-section-properties cols . opt)
  ;; the page and the columns of a section, which starts on the same page
  `(w:sectPr
     ,@(if dx-sections? '((w:type (@ (w:val "continuous")))) '())
     (w:pgSz (@ (w:w ,(dx-twips 'page-width 11906))
                (w:h ,(dx-twips 'page-height 16838))
                ,@(if (== (ox-attr dx-setup 'landscape) "true")
                      '((w:orient "landscape")) '())))
     (w:pgMar (@ (w:top ,(dx-twips 'margin-top 1440))
                 (w:right ,(dx-twips 'margin-right 1440))
                 (w:bottom ,(dx-twips 'margin-bottom 1440))
                 (w:left ,(dx-twips 'margin-left 1440))
                 (w:header "720") (w:footer "720") (w:gutter "0")))
     ;; (the line between the columns, the space between them)
     (w:cols (@ (w:num ,cols)
                ,@(if (and (pair? opt) (== (car opt) "true")) '((w:sep "1")) '())
                (w:space ,(or (and (pair? opt) (pair? (cdr opt)) (cadr opt)) "720"))))))

(define (dx-section-end cols . opt)
  ;; a paragraph which ends a section
  `(w:p (w:pPr ,(apply dx-section-properties (cons cols opt)))))

(define (dx-blocks l)
  ;; the elements of the blocks l; Word makes one table of two tables
  ;; which follow each other, so that an empty paragraph is between them
  (let loop ((l (append-map dx-block l)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((and (func? (car l) 'w:tbl) (pair? (cdr l)) (func? (cadr l) 'w:tbl))
           (loop (cdr l) (cons* '(w:p) (car l) acc)))
          (else (loop (cdr l) (cons (car l) acc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The files of the archive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dx-style type id name . l)
  ;; a style: its properties of paragraphs and of runs are in l
  `(w:style (@ (w:type ,type) (w:styleId ,id)
               ;; (the style of the paragraphs which name none)
               ,@(if (== id "Normal") '((w:default "1")) '()))
            ,(dx-val 'w:name name)
            ,@l))

(define (dx-heading-style n)
  (let ((s (number->string n))
        (size (list-ref '("36" "30" "26" "24" "22" "22" "22" "22" "22") (- n 1))))
    (dx-style "paragraph" (string-append "Heading" s) (string-append "heading " s)
              (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal") '(w:qFormat)
              `(w:pPr (w:keepNext)
                      ;; the numbers of the headings, when they have some
                      ,@(if dx-numbered?
                            `((w:numPr ,(dx-val 'w:ilvl (number->string (- n 1)))
                                       ,(dx-val 'w:numId "999")))
                            '())
                      (w:spacing (@ (w:before ,(if (<= n 2) "360" "240"))
                                    (w:after "120")))
                      ,(dx-val 'w:outlineLvl (number->string (- n 1))))
              `(w:rPr (w:b) ,@(if (>= n 4) '((w:i)) '())
                      ,(dx-val 'w:sz size) ,(dx-val 'w:szCs size)))))

(define (dx-default-fonts)
  ;; the font of the text: a font with serifs as those of TeXmacs, unless
  ;; the document says otherwise
  (with f (cond ((== (ox-attr dx-setup 'font) "sans") "Arial")
                ((== (ox-attr dx-setup 'font) "mono") "Courier New")
                (else "Times New Roman"))
    `(w:rFonts (@ (w:ascii ,f) (w:hAnsi ,f) (w:cs ,f) (w:eastAsia ,f)))))

(define (dx-default-size)
  ;; the size of the text, in half points
  (with n (office-number (ox-attr dx-setup 'font-size))
    (number->string (if (and n (>= n 5) (<= n 50)) (inexact->exact (round (* 2 n))) 22))))

(define (dx-twips key default)
  ;; the length of the attribute key of the setup, in twentieths of a point
  (with cm (office-length->cm (ox-attr dx-setup key))
    (number->string (if cm (inexact->exact (round (* cm 566.929))) default))))

(define (dx-styles)
  `(w:styles
     (@ (xmlns:w ,dx-ns-w))
     (w:docDefaults
       (w:rPrDefault (w:rPr ,(dx-default-fonts)
                            ,(dx-val 'w:sz (dx-default-size))
                            ,(dx-val 'w:szCs (dx-default-size))))
       (w:pPrDefault (w:pPr (w:spacing (@ (w:after "120"))))))
     ,(dx-style "paragraph" "Normal" "Normal" '(w:qFormat)
                `(w:pPr ,(dx-val 'w:jc (with a (ox-attr dx-setup 'align)
                                         (cond ((in? a '("center" "right" "left")) a)
                                               ((== a "justify") "both")
                                               (else "left"))))))
     ,@(map dx-heading-style (map (lambda (i) (+ i 1)) (iota 9)))
     ,(dx-style "paragraph" "Title" "Title"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal") '(w:qFormat)
                `(w:pPr (w:spacing (@ (w:before "480") (w:after "240")))
                        ,(dx-val 'w:jc "center"))
                `(w:rPr (w:b) ,(dx-val 'w:sz "44") ,(dx-val 'w:szCs "44")))
     ,(dx-style "paragraph" "TOCHeading" "TOC Heading"
                (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal")
                `(w:pPr (w:keepNext) (w:spacing (@ (w:before "360") (w:after "120"))))
                `(w:rPr (w:b) ,(dx-val 'w:sz "36") ,(dx-val 'w:szCs "36")))
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
     ,(dx-style "paragraph" "Theorem" "Theorem"
                (dx-val 'w:basedOn "Normal")
                `(w:pPr (w:spacing (@ (w:before "120") (w:after "120"))))
                `(w:rPr (w:i)))
     ,(dx-style "paragraph" "Remark" "Remark"
                (dx-val 'w:basedOn "Normal")
                `(w:pPr (w:spacing (@ (w:before "120") (w:after "120")))))
     ,(dx-style "paragraph" "Proof" "Proof"
                (dx-val 'w:basedOn "Normal")
                `(w:pPr (w:spacing (@ (w:before "120") (w:after "120")))))
     ,(dx-style "paragraph" "Bibliography" "Bibliography"
                (dx-val 'w:basedOn "Normal")
                `(w:pPr (w:ind (@ (w:left "567") (w:hanging "567")))))
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

;; the abstract lists: (format id numFmt), bullets and the ways of numbering
(define dx-formats
  '(("bullet" "1" "bullet") ("1" "2" "decimal") ("a" "4" "lowerLetter")
    ("A" "5" "upperLetter") ("i" "6" "lowerRoman") ("I" "7" "upperRoman")))

(define (dx-level kind i)
  ;; the level i of a list of bullets or of numbers of the format kind
  `(w:lvl (@ (w:ilvl ,(number->string i)))
          ,(dx-val 'w:start "1")
          ,(dx-val 'w:numFmt (caddr (or (assoc kind dx-formats) (cadr dx-formats))))
          ,(dx-val 'w:lvlText
                   (cond ((== kind "bullet")
                          (list-ref (list (dx-utf8 #x2022) (dx-utf8 #x25e6) (dx-utf8 #x25aa))
                                    (modulo i 3)))
                         ;; (letters with a bracket, as TeXmacs writes them)
                         ((in? kind '("a" "A"))
                          (string-append "%" (number->string (+ i 1)) ")"))
                         (else (string-append "%" (number->string (+ i 1)) "."))))
          ,(dx-val 'w:lvlJc "left")
          (w:pPr (w:ind (@ (w:left ,(number->string (* 720 (+ i 1))))
                           (w:hanging "360"))))))

(define (dx-abstract-id id)
  (number->string (+ (string->number id) dx-abstract-offset)))

(define (dx-numbering)
  ;; two abstract lists, of bullets and of numbers, and a list of one of
  ;; them for each list of the text, which starts at 1 at its level
  `(w:numbering
     (@ (xmlns:w ,dx-ns-w))
     ,@(map (lambda (f)
              `(w:abstractNum (@ (w:abstractNumId ,(dx-abstract-id (cadr f))))
                              ,(dx-val 'w:multiLevelType "hybridMultilevel")
                              ,@(map (lambda (i) (dx-level (car f) i)) (iota 9))))
            dx-formats)
     ;; the numbers of the headings: 1, 1.1, 1.1.1 by their styles
     ,@(if dx-numbered?
           `((w:abstractNum
               (@ (w:abstractNumId ,(dx-abstract-id "3")))
               ,(dx-val 'w:multiLevelType "multilevel")
               ,@(map (lambda (i)
                        `(w:lvl (@ (w:ilvl ,(number->string i)))
                                ,(dx-val 'w:start "1")
                                ,(dx-val 'w:numFmt "decimal")
                                ,(dx-val 'w:pStyle
                                         (string-append "Heading" (number->string (+ i 1))))
                                ,(dx-val 'w:suff "space")
                                ,(dx-val 'w:lvlText
                                         (string-recompose
                                           (map (lambda (k)
                                                  (string-append "%" (number->string (+ k 1))))
                                                (iota (+ i 1)))
                                           "."))
                                ,(dx-val 'w:lvlJc "left")
                                (w:pPr (w:ind (@ (w:left "0") (w:firstLine "0"))))))
                      (iota 9))))
           '())
     ,@(if dx-numbered?
           `((w:num (@ (w:numId "999")) ,(dx-val 'w:abstractNumId (dx-abstract-id "3"))))
           '())
     ,@(map (lambda (l)
              `(w:num (@ (w:numId ,(number->string (car l))))
                      ,(dx-val 'w:abstractNumId
                               (dx-abstract-id
                                 (cadr (or (assoc (cadr l) dx-formats) (cadr dx-formats)))))
                      (w:lvlOverride (@ (w:ilvl ,(number->string (min 8 (caddr l)))))
                                     ,(dx-val 'w:startOverride (cadddr l)))))
            (reverse dx-lists))))

(define (dx-footnotes)
  `(w:footnotes
     (@ (xmlns:w ,dx-ns-w) (xmlns:r ,dx-ns-r) (xmlns:m ,dx-ns-m)
        (xmlns:wp ,dx-ns-wp))
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

(define (dx-content-types . comments?)
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
       ,(override "/word/settings.xml" "settings")
       ,@(if (and (pair? comments?) (car comments?))
             (list (override "/word/comments.xml" "comments")) '()))))

(define (dx-document blocks)
  `(w:document
     (@ (xmlns:w ,dx-ns-w) (xmlns:r ,dx-ns-r) (xmlns:m ,dx-ns-m)
        (xmlns:wp ,dx-ns-wp))
     (w:body
       ,@blocks
       ,(dx-final-section))))

(define dx-section-order
  ;; the order of the children of w:sectPr which the format wants
  '(w:headerReference w:footerReference w:footnotePr w:endnotePr w:type w:pgSz
    w:pgMar w:paperSrc w:pgBorders w:lnNumType w:pgNumType w:cols w:formProt
    w:vAlign w:noEndnote w:titlePg w:textDirection w:bidi w:rtlGutter w:docGrid
    w:printerSettings w:sectPrChange))

(define (dx-final-section)
  ;; the properties of the last section: those of the file it was read
  ;; from, with its headers and footers, and the page and the columns of
  ;; the document
  (with orig (dx-package-section)
    (if (not orig) (dx-section-properties "1")
        (let* ((mine (cdr (dx-section-properties "1")))
               (mine-tags '(w:type w:pgSz w:pgMar w:cols))
               (theirs (list-filter (ox-elements orig)
                                    (lambda (y) (not (in? (car y) mine-tags)))))
               (all (append theirs mine))
               (rank (lambda (y) (or (list-find-index dx-section-order
                                                      (lambda (t) (== t (car y))))
                                     100))))
          `(w:sectPr ,@(sort all (lambda (a b) (< (rank a) (rank b)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dx-comments-part)
  `(w:comments
     (@ (xmlns:w ,dx-ns-w) (xmlns:r ,dx-ns-r) (xmlns:m ,dx-ns-m) (xmlns:wp ,dx-ns-wp))
     ,@(reverse dx-comments)))

;; The file the text was read from, in a document of the style office: its
;; parts which TeXmacs does not edit (the styles, the headers and footers,
;; the theme, the fonts, the settings, the properties) are written again,
;; and the text, its notes, comments and lists are those of TeXmacs.

(define (dx-package-xml name)
  (with s (and dx-package (ahash-ref dx-package name))
    (and s (ox-root (parse-xml s)))))

(define (dx-package-section)
  ;; the properties of the last section of the file
  (let* ((root (dx-package-xml "word/document.xml"))
         (body (and root (ox-child root 'w:body))))
    (and body (list-find (reverse (ox-children body))
                         (lambda (y) (func? y 'w:sectPr))))))

(define (dx-max-id root tag attr)
  ;; the largest number of the attribute attr of the elements tag
  (apply max (cons 0 (map (lambda (x) (or (office-integer (ox-attr x attr) #f 0 100000) 0))
                          (ox-childs root tag)))))

(define dx-written-types
  ;; the relations which TeXmacs writes itself
  '("styles" "numbering" "footnotes" "endnotes" "comments" "settings" "image"
    "hyperlink" "commentsExtended" "commentsIds" "commentsExtensible" "oleObject"
    "chart" "diagramData" "diagramLayout" "diagramColors" "diagramQuickStyle"
    "package" "officeDocument"))

(define (dx-rel-type-name r)
  ;; the last word of the type of a relation
  (cAr (string-tokenize-by-char (or (ox-attr r 'Type) "") #\/)))

(define (dx-kept-relations)
  ;; the relations of the text of the file to the parts which are kept
  (with root (dx-package-xml "word/_rels/document.xml.rels")
    (if (not root) '()
        (list-filter (ox-childs root 'Relationship)
                     (lambda (r) (not (in? (dx-rel-type-name r) dx-written-types)))))))

(define (dx-merged-styles mine)
  ;; the styles of the file, with those of TeXmacs which it does not have
  (with root (dx-package-xml "word/styles.xml")
    (if (not root) mine
        (let* ((ids (map (lambda (x) (ox-attr x 'w:styleId)) (ox-childs root 'w:style)))
               (new (list-filter (ox-childs mine 'w:style)
                                 (lambda (x) (not (in? (ox-attr x 'w:styleId) ids))))))
          (append root new)))))

(define dx-source-index #f) ; the table of dx-source-styles

(define (dx-source-style name family)
  ;; the node style of this name and family (paragraph or text) which the
  ;; document has, from its macros or from its file of another format
  (when (not dx-source-index)
    (set! dx-source-index (office-style-index dx-source-styles)))
  (ahash-ref dx-source-index (cons name family)))

(define (dx-twips-attr st k)
  ;; the length of the attribute k of a node style, in points, in twips
  (with v (office-number (ox-attr st k))
    (and v (number->string (inexact->exact (round (* 20 v)))))))

(define (dx-keep-next? st)
  ;; whether a node style keeps its paragraph with the next one
  (with v (or (ox-attr st 'keep-with-next) (ox-attr st 'keep-next))
    (and v (not (in? v '("false" "no" "0" "auto"))))))

(define (dx-style-paragraph-properties st)
  ;; the elements of w:pPr for the properties of a node style, in the
  ;; order of the format (borders, shading, spaces, indentation, alignment)
  (let* ((get (lambda (k) (ox-attr st k)))
         (before (dx-twips-attr st 'space-before))
         (after (dx-twips-attr st 'space-after))
         (line (with v (office-number (get 'line-spacing))
                 (and v (> v 0) (number->string (inexact->exact (round (* 240 v)))))))
         (left (dx-twips-attr st 'indent-left))
         (right (dx-twips-attr st 'indent-right))
         (first (office-number (get 'first-indent)))
         (fill (office-color (get 'background)))
         (align (get 'align)))
    (append
      (if (dx-keep-next? st) '((w:keepNext)) '())
      (if (get 'border)
          `((w:pBdr ,@(map (lambda (side)
                             `(,side (@ (w:val "single") (w:sz "4") (w:space "4")
                                        (w:color "auto"))))
                           '(w:top w:left w:bottom w:right))))
          '())
      (if fill `((w:shd (@ (w:val "clear") (w:color "auto")
                           (w:fill ,(upcase-all (substring fill 1 7))))))
          '())
      (if (or before after line)
          `((w:spacing (@ ,@(if before `((w:before ,before)) '())
                          ,@(if after `((w:after ,after)) '())
                          ,@(if line `((w:line ,line) (w:lineRule "auto")) '()))))
          '())
      (if (or left right first)
          `((w:ind (@ ,@(if left `((w:left ,left)) '())
                      ,@(if right `((w:right ,right)) '())
                      ,@(cond ((not first) '())
                              ((< first 0)
                               `((w:hanging ,(number->string
                                               (inexact->exact (round (* -20 first)))))))
                              (else
                               `((w:firstLine ,(number->string
                                                 (inexact->exact (round (* 20 first)))))))))))
          '())
      (cond ((== align "center") (list (dx-val 'w:jc "center")))
            ((== align "right") (list (dx-val 'w:jc "right")))
            ((== align "left") (list (dx-val 'w:jc "left")))
            ((== align "justify") (list (dx-val 'w:jc "both")))
            (else '())))))

(define (dx-style-run-properties st)
  ;; the elements of w:rPr for the properties of a node style, in the
  ;; order of the format
  (let* ((get (lambda (k) (ox-attr st k)))
         (font (cond ((get 'font-name) => identity)
                     ((== (get 'font) "sans") "Arial")
                     ((== (get 'font) "mono") "Courier New")
                     (else #f)))
         (color (office-color (get 'color)))
         (size (with v (office-number (get 'size))
                 (and v (> v 0) (number->string (inexact->exact (round (* 2 v)))))))
         (letters (dx-twips-attr st 'letters)))
    (append
      (if font `((w:rFonts (@ (w:ascii ,font) (w:hAnsi ,font) (w:cs ,font)))) '())
      (if (get 'strong) '((w:b) (w:bCs)) '())
      (if (get 'em) '((w:i) (w:iCs)) '())
      (if (== (get 'caps) "true") '((w:caps)) '())
      (if (get 'smallcaps) '((w:smallCaps)) '())
      (cond ((get 'dstrike) '((w:dstrike)))
            ((get 'strike) '((w:strike)))
            (else '()))
      (if color (list (dx-val 'w:color (upcase-all (substring color 1 7)))) '())
      (if letters (list (dx-val 'w:spacing letters)) '())
      (if size (list (dx-val 'w:sz size) (dx-val 'w:szCs size)) '())
      (if (get 'underline)
          (list (dx-val 'w:u (with k (get 'underline-kind)
                               (if (in? k office-underline-kinds) k "single"))))
          '()))))

(define (dx-style-of-node type id name st)
  ;; the style id of Word, whose name is name, with the properties of the
  ;; node style st (#f for none), after the usual one
  (let* ((ppr (if (and st (== type "paragraph")) (dx-style-paragraph-properties st) '()))
         (rpr (if st (dx-style-run-properties st) '())))
    (apply dx-style
           (append (list type id name)
                   (if (== type "paragraph") (list (dx-val 'w:basedOn "Normal")) '())
                   (list '(w:qFormat))
                   (if (null? ppr) '() (list (cons 'w:pPr ppr)))
                   (if (null? rpr) '() (list (cons 'w:rPr rpr)))))))

(define (dx-heading-style-of-node n st)
  ;; the style of the headings of level n in the look of the node style st
  ;; of the document (a heading of the file of another format)
  (let ((s (number->string n))
        (ppr (list-filter (dx-style-paragraph-properties st)
                          (lambda (y) (not (func? y 'w:keepNext)))))
        (rpr (dx-style-run-properties st)))
    (dx-style "paragraph" (string-append "Heading" s) (string-append "heading " s)
              (dx-val 'w:basedOn "Normal") (dx-val 'w:next "Normal") '(w:qFormat)
              `(w:pPr (w:keepNext)
                      ,@(if dx-numbered?
                            `((w:numPr ,(dx-val 'w:ilvl (number->string (- n 1)))
                                       ,(dx-val 'w:numId "999")))
                            '())
                      ,@ppr
                      ,(dx-val 'w:outlineLvl (number->string (- n 1))))
              `(w:rPr ,@rpr))))

;; The order of the properties of paragraphs and of runs in a style
(define dx-ppr-order
  '(w:pStyle w:keepNext w:keepLines w:pageBreakBefore w:framePr w:widowControl
    w:numPr w:suppressLineNumbers w:pBdr w:shd w:tabs w:suppressAutoHyphens
    w:kinsoku w:wordWrap w:overflowPunct w:topLinePunct w:autoSpaceDE
    w:autoSpaceDN w:bidi w:adjustRightInd w:snapToGrid w:spacing w:ind
    w:contextualSpacing w:mirrorIndents w:suppressOverlap w:jc w:textDirection
    w:textAlignment w:textboxTightWrap w:outlineLvl w:divId w:cnfStyle))

(define dx-rpr-order
  '(w:rStyle w:rFonts w:b w:bCs w:i w:iCs w:caps w:smallCaps w:strike w:dstrike
    w:outline w:shadow w:emboss w:imprint w:noProof w:snapToGrid w:vanish
    w:webHidden w:color w:spacing w:w w:kern w:position w:sz w:szCs w:highlight
    w:u w:effect w:bdr w:shd w:fitText w:vertAlign w:rtl w:cs w:em w:lang
    w:eastAsianLayout w:specVanish w:oMath))

(define (dx-merge-properties theirs mine drop order)
  ;; the properties mine, with those of theirs which mine does not have
  ;; and which are not dropped, in the order of the format
  (let* ((tags (map car mine))
         (all (append (list-filter theirs
                                   (lambda (y) (and (pair? y) (not (in? (car y) tags))
                                                    (not (in? (car y) drop)))))
                      mine))
         (rank (lambda (y) (or (list-find-index order (lambda (t) (== t (car y)))) 100))))
    (sort all (lambda (a b) (< (rank a) (rank b))))))

(define (dx-restyle x type st)
  ;; The style x of TeXmacs (Normal, Title...) in the look of the node
  ;; style st of the document which is this style: what the document says
  ;; wins, its run is bold, italic... only when it says so, the rest of
  ;; x (the level of a heading, its name) is kept.
  (let* ((ppr (if (== type "paragraph") (dx-style-paragraph-properties st) '()))
         (rpr (dx-style-run-properties st))
         (old-ppr (ox-children (or (ox-child x 'w:pPr) '(w:pPr))))
         (old-rpr (ox-children (or (ox-child x 'w:rPr) '(w:rPr))))
         (ppr (dx-merge-properties old-ppr ppr '() dx-ppr-order))
         (rpr (dx-merge-properties old-rpr rpr
                                   '(w:b w:bCs w:i w:iCs w:caps w:smallCaps w:strike
                                     w:dstrike w:u)
                                   dx-rpr-order)))
    `(w:style (@ ,@(ox-attrs x))
              ,@(list-filter (ox-children x)
                             (lambda (y) (not (or (func? y 'w:pPr) (func? y 'w:rPr)))))
              ,@(if (null? ppr) '() (list (cons 'w:pPr ppr)))
              ,@(if (null? rpr) '() (list (cons 'w:rPr rpr))))))

(define (dx-complete-styles styles)
  ;; The styles, with those which the text names and which they do not
  ;; have (a style of a file of another format, or of the style office
  ;; without its file): a style of this name, in the look which the
  ;; document says, after the usual one. Without a file of Word, the
  ;; headings have the look of those of the document.
  (let* ((headings
           (if dx-package '()
               (list-filter
                 (map (lambda (st)
                        (with n (office-integer (ox-attr st 'level) 0 0 9)
                          (and (>= n 1) (== (ox-attr st 'family) "paragraph")
                               (cons (string-append "Heading" (number->string n))
                                     (dx-heading-style-of-node n st)))))
                      dx-source-styles)
                 identity)))
         (styles (if (null? headings) styles
                     (map (lambda (x)
                            (with h (and (func? x 'w:style)
                                         (assoc (ox-attr x 'w:styleId) headings))
                              (if h (cdr h) x)))
                          styles)))
         ;; (a style of TeXmacs which a style of the document is has the
         ;; look of the document)
         (styles (map (lambda (x)
                        (with r (and (func? x 'w:style)
                                     (ahash-ref dx-restyled (ox-attr x 'w:styleId)))
                          (if r (dx-restyle x (car r) (cdr r)) x)))
                      styles))
         (ids (map (lambda (x) (ox-attr x 'w:styleId)) (ox-childs styles 'w:style)))
         ;; ((id family name display) of dx-use-style)
         (new (list-filter (reverse dx-used-styles)
                           (lambda (u) (and (!= (car u) "") (not (in? (car u) ids)))))))
    (dx-unshared-normal
      (if (null? new) styles
          (append styles
                  (map (lambda (u)
                         (if (== (cadr u) "paragraph")
                             (dx-style-of-node "paragraph" (car u) (cadddr u)
                                               (dx-source-style (caddr u) "paragraph"))
                             (dx-style-of-node "character" (car u) (cadddr u)
                                               (dx-source-style (caddr u) "text"))))
                       new))))))

(define dx-neutral-runs
  ;; the properties of runs of a style which the others based on it would
  ;; have, and the value which says no
  '((w:b "0") (w:bCs "0") (w:i "0") (w:iCs "0") (w:caps "0") (w:smallCaps "0")
    (w:strike "0") (w:dstrike "0") (w:u "none") (w:color "auto")))

(define (dx-unshared-normal styles)
  ;; When Normal has the look of a style of the document (bold, italic, a
  ;; color...), the other styles of paragraphs, which are based on it, do
  ;; not take it: they say no where they say nothing.
  (with r (ahash-ref dx-restyled "Normal")
    (if (not r) styles
        (let* ((normal (list-find (ox-childs styles 'w:style)
                                  (lambda (x) (== (ox-attr x 'w:styleId) "Normal"))))
               (have (map car (ox-children (or (and normal (ox-child normal 'w:rPr)) '(w:rPr)))))
               (off (list-filter dx-neutral-runs (lambda (p) (in? (car p) have)))))
          (if (null? off) styles
              (map (lambda (x)
                     (if (not (and (func? x 'w:style) (== (ox-attr x 'w:type) "paragraph")
                                   (with b (ox-child x 'w:basedOn)
                                     (and b (== (ox-attr b 'w:val) "Normal")))))
                         x
                         (let* ((old (ox-children (or (ox-child x 'w:rPr) '(w:rPr))))
                                (tags (map car old))
                                (add (map (lambda (p) `(,(car p) (@ (w:val ,(cadr p)))))
                                          (list-filter off (lambda (p) (not (in? (car p) tags))))))
                                (rpr (dx-merge-properties '() (append old add) '() dx-rpr-order)))
                           `(w:style (@ ,@(ox-attrs x))
                                     ,@(list-filter (ox-children x)
                                                    (lambda (y) (not (func? y 'w:rPr))))
                                     (w:rPr ,@rpr)))))
                   styles))))))

(define (dx-merged-numbering mine)
  ;; the lists of the file, and those of TeXmacs after them
  (with root (dx-package-xml "word/numbering.xml")
    (if (not root) mine
        (let* ((tags '(w:abstractNum w:num))
               (abstract (append (ox-childs root 'w:abstractNum) (ox-childs mine 'w:abstractNum)))
               (nums (append (ox-childs root 'w:num) (ox-childs mine 'w:num)))
               (other (list-filter (ox-elements root) (lambda (y) (not (in? (car y) tags))))))
          `(w:numbering (@ ,@(ox-attrs root))
                        ,@(list-filter other (lambda (y) (func? y 'w:numPicBullet)))
                        ,@abstract ,@nums
                        ,@(list-filter other (lambda (y) (not (func? y 'w:numPicBullet)))))))))

(define (dx-merged-settings mine)
  ;; the settings of the file, without the separators of its notes
  (with root (dx-package-xml "word/settings.xml")
    (if (not root) mine
        `(w:settings (@ ,@(ox-attrs root))
                     ,@(list-filter (ox-children root)
                                    (lambda (y) (not (and (pair? y)
                                                          (in? (car y) '(w:footnotePr
                                                                         w:endnotePr))))))))))

(define dx-replaced-parts
  '("[Content_Types].xml" "word/document.xml" "word/_rels/document.xml.rels"
    "word/styles.xml" "word/numbering.xml" "word/footnotes.xml" "word/endnotes.xml"
    "word/comments.xml" "word/commentsExtended.xml" "word/commentsIds.xml"
    "word/commentsExtensible.xml" "word/settings.xml"
    "word/_rels/footnotes.xml.rels" "word/_rels/endnotes.xml.rels"
    "word/_rels/comments.xml.rels"))

(define (dx-merged-content-types mine names)
  ;; the types of the parts of the archive which is written
  (let* ((root (dx-package-xml "[Content_Types].xml"))
         (theirs (if root (ox-elements root) '()))
         (exts (map (lambda (x) (locase-all (or (ox-attr x 'Extension) "")))
                    (list-filter theirs (lambda (y) (func? y 'Default)))))
         (parts (map (lambda (x) (ox-attr x 'PartName))
                     (list-filter theirs (lambda (y) (func? y 'Override)))))
         (exists? (lambda (part) (and part (in? (substring part 1 (string-length part)) names)))))
    `(Types (@ (xmlns "http://schemas.openxmlformats.org/package/2006/content-types"))
            ,@(list-filter theirs (lambda (y) (func? y 'Default)))
            ,@(list-filter (ox-elements mine)
                           (lambda (y) (and (func? y 'Default)
                                            (not (in? (locase-all (ox-attr y 'Extension)) exts)))))
            ,@(list-filter theirs (lambda (y) (and (func? y 'Override)
                                                   (exists? (ox-attr y 'PartName)))))
            ,@(list-filter (ox-elements mine)
                           (lambda (y) (and (func? y 'Override)
                                            (not (in? (ox-attr y 'PartName) parts))
                                            (exists? (ox-attr y 'PartName))))))))

(tm-define (serialize-docx-document x)
  (:type (-> stree string))
  (:synopsis "Write the office tree @x as a Word document, a zip archive")
  (let* ((l (if (func? x 'office) (cdr x) (list x)))
         (pack (list-find l (lambda (y) (func? y 'package)))))
    (set! dx-package (and pack (== (ox-attr pack 'format) "docx")
                          (office-archive (ox-attr pack 'data))))
    (when (and dx-package (not (ahash-ref dx-package "word/document.xml")))
      (set! dx-package #f)))
  (set! dx-rels '())
  (set! dx-note-rels '())
  (set! dx-rel-ids (make-ahash-table))
  (set! dx-media '())
  (set! dx-media-files (make-ahash-table))
  (set! dx-notes '())
  (set! dx-lists '())
  (set! dx-comments '())
  (set! dx-used-styles '())
  (set! dx-sections? #f)
  (set! dx-id 0)
  (set! dx-in-note? #f)
  (set! dx-item #f)
  (set! dx-indent 0)
  (set! dx-cell-align #f)
  (set! dx-numbered? (office-numbered-headings? x))
  (set! dx-setup (or (and (func? x 'office)
                          (list-find (cdr x) (lambda (y) (func? y 'setup))))
                     '(setup)))
  (with numbering (dx-package-xml "word/numbering.xml")
    (set! dx-num-offset (if numbering (dx-max-id numbering 'w:num 'w:numId) 0))
    (set! dx-abstract-offset
          (if numbering (+ 1 (dx-max-id numbering 'w:abstractNum 'w:abstractNumId)) 0)))
  (set! dx-bookmarks (make-ahash-table))
  (set! dx-bookmark-names (make-ahash-table))
  (set! dx-bookmarks-written (make-ahash-table))
  (set! dx-no-sections? #f)
  (set! dx-in-comment? #f)
  (set! dx-note-comments '())
  (set! dx-known-styles #f)
  (set! dx-package-ids '())
  (set! dx-style-ids (make-ahash-table))
  (set! dx-claimed (make-ahash-table))
  (set! dx-restyled (make-ahash-table))
  ;; the named styles of the document (of its macros), with those of its
  ;; file of OpenDocument (those of a file of Word are in its styles.xml)
  (set! dx-source-index #f)
  (set! dx-source-styles
        (let* ((l (if (func? x 'office) (cdr x) '()))
               (pack (list-find l (lambda (y) (func? y 'package)))))
          (office-merged-styles
            (append-map ox-children (list-filter l (lambda (y) (func? y 'styles))))
            (if (and pack (not dx-package) (== (ox-attr pack 'format) "odt"))
                (office-package-styles "odt" (ox-attr pack 'data))
                '()))))
  ;; the relations of the files which are always there come first
  (dx-relation "styles" "styles.xml" #f)
  (dx-relation "numbering" "numbering.xml" #f)
  (dx-relation "footnotes" "footnotes.xml" #f)
  (dx-relation "settings" "settings.xml" #f)
  (let* ((blocks (dx-blocks (if (func? x 'office) (cdr x) (list x))))
         ;; a document has a paragraph at least
         (blocks (if (null? blocks) (list '(w:p)) blocks))
         (document (dx-document blocks))
         (comments? (pair? dx-comments))
         (_ (when comments? (dx-relation "comments" "comments.xml" #f)))
         (media (reverse dx-media))
         ;; the relations of the notes, when they have some
         (note-rels (reverse dx-note-rels))
         (kept (if dx-package
                   (list-filter (map car (ahash-table->list dx-package))
                                (lambda (n) (and (not (in? n dx-replaced-parts))
                                                 (not (string-ends? n "/"))
                                                 (not (string-starts? n "word/media/tm-")))))
                   '()))
         (kept (sort kept string<?))
         (names (append
                  '("[Content_Types].xml" "_rels/.rels" "word/document.xml"
                    "word/_rels/document.xml.rels" "word/styles.xml"
                    "word/numbering.xml" "word/footnotes.xml" "word/settings.xml")
                  (if comments? '("word/comments.xml") '())
                  (if (null? note-rels) '() '("word/_rels/footnotes.xml.rels"))
                  (if (and comments? (pair? note-rels)) '("word/_rels/comments.xml.rels") '())
                  (map (lambda (m) (string-append "word/media/" (car m))) media)))
         (kept (list-filter kept (lambda (n) (not (in? n names)))))
         (relations (if dx-package
                        `(Relationships
                           (@ (xmlns "http://schemas.openxmlformats.org/package/2006/relationships"))
                           ,@(dx-kept-relations)
                           ,@(cddr (dx-relations (reverse dx-rels))))
                        (dx-relations (reverse dx-rels))))
         (package-rels (and dx-package (ahash-ref dx-package "_rels/.rels")))
         (r (zip-pack
              (append names kept)
              (append
                (list (ox-serialize (if dx-package
                                        (dx-merged-content-types (dx-content-types comments?)
                                                                 (append names kept))
                                        (dx-content-types comments?)))
                      (if (and package-rels
                               (>= (string-search-forwards "word/document.xml" 0 package-rels) 0))
                          package-rels
                          (ox-serialize
                            (dx-relations
                              '(("rId1" "officeDocument" "word/document.xml" #f)))))
                      (ox-serialize document)
                      (ox-serialize relations)
                      (ox-serialize (dx-complete-styles (dx-merged-styles (dx-styles))))
                      (ox-serialize (dx-merged-numbering (dx-numbering)))
                      (ox-serialize (dx-footnotes))
                      (ox-serialize (dx-merged-settings (dx-settings))))
                (if comments? (list (ox-serialize (dx-comments-part))) '())
                (if (null? note-rels) '()
                    (list (ox-serialize (dx-relations note-rels))))
                (if (and comments? (pair? note-rels))
                    (list (ox-serialize (dx-relations note-rels))) '())
                (map cadr media)
                (map (lambda (n) (ahash-ref dx-package n)) kept)))))
    (set! dx-rels '())
    (set! dx-note-rels '())
    (set! dx-rel-ids (make-ahash-table))
    (set! dx-media '())
    (set! dx-media-files (make-ahash-table))
    (set! dx-notes '())
    (set! dx-lists '())
    (set! dx-comments '())
    (set! dx-package #f)
    r))
