
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : docxin.scm
;; DESCRIPTION : reading Word documents (.docx) into office trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A .docx is a zip archive: word/document.xml is the text, a sequence of
;; paragraphs (w:p) of runs (w:r) and of tables (w:tbl); word/styles.xml
;; has the styles, which tell what a paragraph is (a heading, a quotation);
;; word/numbering.xml the lists, which in the text are paragraphs with a
;; list and a level; word/footnotes.xml the notes; the files _rels/*.rels
;; the addresses of the links and the names of the images in the archive.
;; See office-tools.scm for the tree which is made of all this.

(texmacs-module (convert office docxin)
  (:use (convert office office-tools)
        (convert office omml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State: the document which is read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define docx-archive #f)                  ; the entries of the archive
(define docx-part "word/document.xml")    ; the entry which is read
(define docx-styles (make-ahash-table))   ; style id -> properties
(define docx-style-memo (make-ahash-table))
(define docx-lists (make-ahash-table))    ; (list id . level) -> kind
(define docx-rels (make-ahash-table))     ; (entry . id) -> (target external?)
(define docx-notes (make-ahash-table))    ; (kind . id) -> element
(define docx-open-notes '())              ; the notes which are being read
(define docx-fields '())                  ; the fields which are open
(define docx-list-formats (make-ahash-table)) ; (list id . level) -> (format start)
(define docx-restarts (make-ahash-table))  ; (list id . level) which start again
(define docx-defaults '())                ; the properties of docDefaults
(define docx-default-style #f)            ; the style of the usual paragraphs
(define docx-body-styles (make-ahash-table)) ; style id -> plain paragraphs
(define docx-floats '())                  ; blocks found inside a paragraph
(define docx-theme-fonts '())             ; ("minor" . font) ("major" . font)
(define docx-italic? #f)                  ; the paragraph is italic by its style
(define docx-used-styles (make-ahash-table)) ; the named styles of the text
(define docx-comments (make-ahash-table)) ; id -> (author date element)
(define docx-sections '())                ; the columns of the sections

(define (docx-val x)
  ;; the value of an element such as <w:jc w:val="center"/>
  (and x (ox-attr x 'w:val)))

(define (docx-child-val x tag)
  (docx-val (ox-child x tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of runs and of paragraphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The properties are association lists, in which the first value of a key
;; counts: those of an element come before those of its style, and those
;; of a style before those of the style it is based on.

(define (docx-toggle x tag key)
  ;; <w:b/> sets, <w:b w:val="0"/> unsets
  (with y (and x (ox-child x tag))
    (if (not y) '()
        (list (cons key (not (in? (docx-val y) '("0" "false" "off" "none"))))))))

(define (docx-run-properties x)
  ;; the properties of an element w:rPr
  (if (not x) '()
      (append
        (docx-toggle x 'w:i 'em)
        (docx-toggle x 'w:b 'strong)
        (docx-toggle x 'w:u 'underline)
        (docx-toggle x 'w:strike 'strike)
        (docx-toggle x 'w:dstrike 'strike)
        (docx-toggle x 'w:smallCaps 'smallcaps)
        (docx-toggle x 'w:caps 'caps)
        (docx-toggle x 'w:dstrike 'dstrike)
        ;; the kind of an underline, the spacing of the letters
        (with v (docx-child-val x 'w:u)
          (if (in? v office-underline-kinds)
              (list (cons 'underline-kind v)) '()))
        (with v (docx-twips->points (docx-child-val x 'w:spacing))
          (if (and v (!= v 0)) (list (cons 'letters v)) '()))
        (docx-toggle x 'w:vanish 'hidden)
        ;; a run of a script which is written from right to left, or of a
        ;; complex one, is italic and bold by properties of its own
        (docx-toggle x 'w:iCs 'em-cs)
        (docx-toggle x 'w:bCs 'strong-cs)
        (docx-toggle x 'w:rtl 'complex)
        (docx-toggle x 'w:cs 'complex)
        (with v (docx-child-val x 'w:vertAlign)
          (cond ((== v "superscript") '((sup . #t) (sub . #f)))
                ((== v "subscript") '((sub . #t) (sup . #f)))
                ((== v "baseline") '((sub . #f) (sup . #f)))
                (else '())))
        (with c (office-color (docx-child-val x 'w:color))
          (if (ox-child x 'w:color) (list (cons 'color c)) '()))
        ;; highlighted text, with the color of its highlighting or of its
        ;; shading
        (with h (docx-child-val x 'w:highlight)
          (cond ((not h) '())
                ((== h "none") '((mark . #f)))
                ((== h "yellow") '((mark . #t)))
                (else (list (cons 'mark (or (office-highlight-color h) #t))))))
        (with c (docx-fill x)
          (if c (list (cons 'shade c)) '()))
        (with f (ox-child x 'w:rFonts)
          (if (and f (ox-attr f 'w:ascii))
              (list (cons 'code (office-monospace? (ox-attr f 'w:ascii))))
              '()))
        (with f (docx-font (ox-child x 'w:rFonts))
          (if f (list (cons 'font f)) '()))
        (with v (office-number (docx-child-val x 'w:sz))
          (if (and v (> v 0)) (list (cons 'size (/ v 2.0))) '()))
        (with s (docx-child-val x 'w:rStyle)
          (if s (list (cons 'style s)) '())))))

(define (docx-font f)
  ;; the font of an element w:rFonts: its own, or the one of the theme
  (and f (or (ox-attr f 'w:ascii) (ox-attr f 'w:hAnsi)
             (with th (or (ox-attr f 'w:asciiTheme) (ox-attr f 'w:hAnsiTheme))
               (and th (assoc-ref docx-theme-fonts
                                  (if (string-starts? th "major") "major" "minor")))))))

(define (docx-twips->points v)
  (with n (office-number v)
    (and n (/ n 20.0))))

(define (docx-spacing-properties x)
  ;; the spaces around a paragraph and between its lines, its indentation
  ;; of the first line
  (let ((sp (ox-child x 'w:spacing))
        (ind (ox-child x 'w:ind)))
    (append
      (with v (and sp (docx-twips->points (ox-attr sp 'w:before)))
        (if v (list (cons 'space-before v)) '()))
      (with v (and sp (docx-twips->points (ox-attr sp 'w:after)))
        (if v (list (cons 'space-after v)) '()))
      ;; lines of 240 are single ones, when their rule is auto
      (with v (and sp (office-number (ox-attr sp 'w:line)))
        (if (and v (> v 0) (in? (ox-attr sp 'w:lineRule) '(#f "auto")))
            (list (cons 'line-spacing (/ v 240.0)))
            '()))
      (cond ((and ind (docx-twips->points (ox-attr ind 'w:firstLine)))
             => (lambda (v) (list (cons 'first-indent v))))
            ((and ind (docx-twips->points (ox-attr ind 'w:hanging)))
             => (lambda (v) (list (cons 'first-indent (- v)))))
            (else '())))))

(define (docx-paragraph-properties x)
  ;; the properties of an element w:pPr
  (if (not x) '()
      (append
        (docx-spacing-properties x)
        (with v (docx-child-val x 'w:jc)
          (if v (list (cons 'align v)) '()))
        (with v (office-integer (docx-child-val x 'w:outlineLvl) #f -1 9)
          (if v (list (cons 'outline v)) '()))
        (with n (ox-child x 'w:numPr)
          (if (not n) '()
              (append
                (with v (docx-child-val n 'w:numId)
                  (if v (list (cons 'list-id v)) '()))
                (with v (docx-child-val n 'w:ilvl)
                  (if v (list (cons 'list-level v)) '())))))
        (if (ox-child x 'w:pageBreakBefore) '((page-break . #t)) '())
        (with ind (ox-child x 'w:ind)
          (with v (and ind (office-integer (or (ox-attr ind 'w:left)
                                               (ox-attr ind 'w:start))
                                           #f 0 100000))
            (if v (list (cons 'indent v)) '())))
        (if (ox-child x 'w:pBdr) '((border . #t)) '())
        (with c (docx-fill x)
          (if c (list (cons 'background c)) '()))
        (with ind (ox-child x 'w:ind)
          (with v (and ind (docx-twips->points (or (ox-attr ind 'w:left)
                                                   (ox-attr ind 'w:start))))
            (if v (list (cons 'indent-left v)) '())))
        (with ind (ox-child x 'w:ind)
          (with v (and ind (docx-twips->points (or (ox-attr ind 'w:right)
                                                   (ox-attr ind 'w:end))))
            (if v (list (cons 'indent-right v)) '())))
        ;; a large first letter, in a frame of its own
        (with f (ox-child x 'w:framePr)
          (if (and f (in? (ox-attr f 'w:dropCap) '("drop" "margin")))
              '((dropcap . #t)) '()))
        (with s (docx-child-val x 'w:pStyle)
          (if s (list (cons 'style s)) '())))))

(define (docx-read-styles)
  ;; the styles of word/styles.xml, with their own properties
  (set! docx-styles (make-ahash-table))
  (set! docx-style-memo (make-ahash-table))
  (with root (office-xml docx-archive "word/styles.xml")
    (when root
      (with d (ox-child root 'w:docDefaults)
        (set! docx-defaults
              (append (docx-paragraph-properties (ox-path d 'w:pPrDefault 'w:pPr))
                      (docx-text-properties (ox-path d 'w:rPrDefault 'w:rPr)))))
      (for (x (ox-childs root 'w:style))
        (with id (ox-attr x 'w:styleId)
          (when (and id (== (ox-attr x 'w:type) "paragraph")
                     (in? (ox-attr x 'w:default) '("1" "true" "on")))
            (set! docx-default-style id))
          (when id
            (ahash-set!
              docx-styles id
              (append
                (list (cons 'name (locase-all (or (docx-child-val x 'w:name) id)))
                      (cons 'display (or (docx-child-val x 'w:name) id))
                      (cons 'family (or (ox-attr x 'w:type) "paragraph")))
                (with b (docx-child-val x 'w:basedOn)
                  (if b (list (cons 'based-on b)) '()))
                ;; a style of paragraphs has properties of runs too, which
                ;; are not those of a style of runs: they are told apart
                ;; (a style of lists says in them which list it is)
                (cond ((in? (ox-attr x 'w:type) '("paragraph" "numbering"))
                       (append (docx-paragraph-properties (ox-child x 'w:pPr))
                               (docx-text-properties (ox-child x 'w:rPr))
                               (list (cons 'run (docx-run-properties
                                                  (ox-child x 'w:rPr))))))
                      ((== (ox-attr x 'w:type) "table")
                       (docx-table-style-properties x))
                      (else (docx-run-properties (ox-child x 'w:rPr))))))))))))

(define (docx-text-properties x)
  ;; the size and the font of the text of a style of paragraphs, apart
  ;; from those of its runs
  (if (not x) '()
      (append
        (with v (office-number (docx-child-val x 'w:sz))
          (if (and v (> v 0)) (list (cons 'text-size (/ v 2.0))) '()))
        (with f (docx-font (ox-child x 'w:rFonts))
          (if f (list (cons 'text-font f)) '())))))

(define (docx-get props key)
  (with p (assoc key props) (and p (cdr p))))

(define (docx-style-sub id seen)
  (with own (and id (not (in? id seen)) (ahash-ref docx-styles id))
    (if (not own) '()
        (append own (docx-style-sub (docx-get own 'based-on) (cons id seen))))))

(define (docx-style id)
  ;; the properties of the style, with those of the styles it is based on
  (cond ((not id) '())
        ((ahash-ref docx-style-memo id) => identity)
        (else (with r (docx-style-sub id '())
                (ahash-set! docx-style-memo id r)
                r))))

(define (docx-style-names id)
  ;; the names of the style and of those it is based on
  (let loop ((id id) (seen '()))
    (with own (and id (not (in? id seen)) (ahash-ref docx-styles id))
      (if (not own) '()
          (cons (docx-get own 'name)
                (loop (docx-get own 'based-on) (cons id seen)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What a paragraph is: from the names of its styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The name of a style (w:name) is the English one also where its
;; identifier is in the language of the program which wrote the file.
(define docx-roles
  '(("title" . "title") ("subtitle" . "subtitle")
    ("author" . "author") ("authors" . "author") ("date" . "date")
    ("abstract" . "abstract") ("abstract title" . "skip")
    ("quote" . "quote") ("intense quote" . "quote") ("block text" . "quote")
    ("quotations" . "quote") ("block quotation" . "quote")
    ("caption" . "caption") ("image caption" . "caption")
    ("table caption" . "caption") ("captioned figure" . "figure")
    ("source code" . "code") ("html preformatted" . "code")
    ("plain text" . "code") ("preformatted text" . "code")
    ("macro" . "code") ("code" . "code")
    ("definition term" . "term") ("definition" . "definition")
    ("theorem" . "theorem") ("remark" . "remark") ("proof" . "proof")
    ("bibliography" . "bibitem")))

(define (docx-toc-style? name)
  ;; the entries of a table of contents, and its heading
  (or (string-starts? name "toc ") (string-starts? name "contents ")
      (== name "table of figures")))

(define (docx-heading-level name)
  ;; "heading 2" is 2
  (and (string-starts? name "heading ")
       (office-integer (substring name 8 (string-length name)) #f 1 9)))

(define (docx-role props)
  ;; (role level) of a paragraph with these properties
  (let* ((names (docx-style-names (docx-get props 'style)))
         (level (list-find (map docx-heading-level names) identity))
         (role (list-find (map (lambda (n) (assoc-ref docx-roles n)) names)
                          identity))
         (outline (docx-get props 'outline)))
    (cond ((list-or (map docx-toc-style? names)) (list "toc" #f))
          (level (list "heading" (number->string level)))
          (role (list role #f))
          ;; a level of the outline without a heading style
          ((and outline (>= outline 0) (< outline 9))
           (list "heading" (number->string (+ outline 1))))
          (else (list #f #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relations, lists and notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-rels-name part)
  ;; word/document.xml has its relations in word/_rels/document.xml.rels
  (let* ((l (string-tokenize-by-char part #\/))
         (dir (reverse (cdr (reverse l)))))
    (string-recompose (append dir (list "_rels" (string-append (cAr l) ".rels")))
                      "/")))

(define (docx-read-rels part)
  (with root (office-xml docx-archive (docx-rels-name part))
    (when root
      (for (x (ox-childs root 'Relationship))
        (ahash-set! docx-rels (cons part (ox-attr x 'Id))
                    (list (or (ox-attr x 'Target) "")
                          (== (ox-attr x 'TargetMode) "External")))))))

(define (docx-rel id)
  ;; (target external?) for the relation id of the entry which is read
  (and id (ahash-ref docx-rels (cons docx-part id))))

(define (docx-blank? s)
  ;; spaces and nothing else: the mark of a level which shows none
  (and (!= s "")
       (list-and (map (lambda (c) (in? c '(#\space #\tab))) (string->list s)))))

(define (docx-list-format lvl)
  ;; "bullet", "number", or "none" for a level without a mark: its
  ;; paragraphs are not items, but more paragraphs of the item before
  (let ((format (docx-child-val lvl 'w:numFmt))
        (text (docx-child-val lvl 'w:lvlText)))
    (cond ((== format "none") "none")
          ((and (== format "bullet") text (docx-blank? text)) "none")
          ((== format "bullet") "bullet")
          (else "number"))))

(define (docx-linked-list a abstract nums)
  ;; the abstract list a, or the one of the style of lists it refers to
  ;; (w:numStyleLink): the style names a list, which has an abstract one
  (let* ((link (docx-child-val a 'w:numStyleLink))
         (id (and link (docx-get (or (ahash-ref docx-styles link) '()) 'list-id)))
         (num (and id (ahash-ref nums id)))
         (b (and num (ahash-ref abstract (docx-child-val num 'w:abstractNumId)))))
    (if (and b (not (ox-child b 'w:numStyleLink))) b a)))

(define (docx-read-lists)
  ;; word/numbering.xml: a list (w:num) refers to an abstract one, whose
  ;; levels have a format, "bullet" or a way of numbering, which the list
  ;; may override. The items of one list in the text may refer to several
  ;; lists of the same abstract one (each restarts the numbers), so the
  ;; abstract list is what tells two lists apart.
  (set! docx-lists (make-ahash-table))
  (with root (office-xml docx-archive "word/numbering.xml")
    (when root
      (let ((abstract (make-ahash-table))
            (nums (make-ahash-table)))
        (for (x (ox-childs root 'w:abstractNum))
          (ahash-set! abstract (ox-attr x 'w:abstractNumId) x))
        (for (x (ox-childs root 'w:num))
          (ahash-set! nums (ox-attr x 'w:numId) x))
        (for (x (ox-childs root 'w:num))
          (let* ((id (ox-attr x 'w:numId))
                 (aid (docx-child-val x 'w:abstractNumId))
                 (a (ahash-ref abstract aid)))
            (when (and id a)
              (ahash-set! docx-lists (cons id 'group) aid)
              (for (lvl (ox-childs (docx-linked-list a abstract nums) 'w:lvl))
                (ahash-set! docx-lists (cons id (ox-attr lvl 'w:ilvl))
                            (docx-list-format lvl))
                (docx-set-format id lvl #f))
              (for (o (ox-childs x 'w:lvlOverride))
                (with lvl (ox-child o 'w:lvl)
                  (when (and lvl (ox-child lvl 'w:numFmt))
                    (ahash-set! docx-lists (cons id (ox-attr o 'w:ilvl))
                                (docx-list-format lvl))
                    (docx-set-format id lvl #f)))
                ;; a list which starts at another number
                (with v (docx-child-val o 'w:startOverride)
                  (with f (ahash-ref docx-list-formats (cons id (ox-attr o 'w:ilvl)))
                    (when v
                      (ahash-set! docx-restarts (cons id (ox-attr o 'w:ilvl)) #t)
                      (ahash-set! docx-list-formats (cons id (ox-attr o 'w:ilvl))
                                  (list (if f (car f) "1") v)))))))))))))

(define (docx-number-format lvl)
  ;; the way of numbering of a level: 1, a, A, i or I
  (with f (docx-child-val lvl 'w:numFmt)
    (cond ((== f "lowerLetter") "a")
          ((== f "upperLetter") "A")
          ((== f "lowerRoman") "i")
          ((== f "upperRoman") "I")
          (else "1"))))

(define (docx-set-format id lvl start)
  ;; the format and the start of the level lvl of the list id
  (ahash-set! docx-list-formats (cons id (ox-attr lvl 'w:ilvl))
              (list (docx-number-format lvl)
                    (or start (docx-child-val lvl 'w:start)))))

(define (docx-list-kind id level)
  (or (ahash-ref docx-lists (cons id level)) "bullet"))

(define (docx-list-group id)
  (or (ahash-ref docx-lists (cons id 'group)) id))

(define (docx-read-notes entry tag kind)
  (with root (office-xml docx-archive entry)
    (when root
      (for (x (ox-childs root tag))
        ;; (the separators of the notes are notes with a type)
        (when (not (in? (ox-attr x 'w:type)
                        '("separator" "continuationSeparator"
                          "continuationNotice")))
          (ahash-set! docx-notes (cons kind (ox-attr x 'w:id))
                      (cons entry x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images and mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-image-node id width height alt . crop)
  ;; the image of the relation id, with the parts which are cut from its
  ;; sides, if any
  (with rel (docx-rel id)
    (if (or (not rel) (cadr rel)) '()
        (let* ((name (office-resolve docx-part (car rel)))
               (data (office-entry docx-archive name)))
          (if (not data) '()
              (list (office-node
                      'image
                      `((name ,(cAr (string-tokenize-by-char name #\/)))
                        (data ,data) (width ,width) (height ,height)
                        (alt ,alt) (crop ,(and (pair? crop) (car crop)))))))))))

(define (docx-crop x)
  ;; the parts of the picture x which are cut from its left, top, right and
  ;; bottom sides, as "l t r b" (fractions), or #f
  (let* ((r (ox-find x 'a:srcRect))
         (part (lambda (k)
                 (with v (and r (office-number (ox-attr r k)))
                   (if v (max 0 (min 0.9 (/ v 100000.0))) 0))))
         (l (list (part 'l) (part 't) (part 'r) (part 'b))))
    (and r (list-or (map (lambda (v) (> v 0)) l))
         (< (+ (car l) (caddr l)) 0.95) (< (+ (cadr l) (cadddr l)) 0.95)
         (string-recompose (map office-decimal l) " "))))

(define (docx-find-all x tag)
  ;; the elements tag inside x, at any depth
  (cond ((not (pair? x)) '())
        ((func? x tag) (list x))
        (else (append-map (lambda (y) (docx-find-all y tag)) (ox-children x)))))

(define (docx-text-boxes x)
  ;; the paragraphs of the boxes of text inside x: they come after the
  ;; paragraph which the box is in, as its text would not fit in a line
  (for (box (docx-find-all x 'w:txbxContent))
    (let ((old-fields docx-fields)
          (old-floats docx-floats))
      (set! docx-fields '())
      (set! docx-floats '())
      (with l (docx-blocks (ox-children box))
        (set! docx-fields old-fields)
        (set! docx-floats (append old-floats l docx-floats))))))

(define (docx-drawing x)
  ;; a picture of DrawingML: its file is the relation of a:blip; a group
  ;; has several pictures, and its size is not the one of each of them
  (docx-text-boxes x)
  (let* ((blips (docx-find-all x 'a:blip))
         (one? (list-1? blips))
         (extent (and one? (ox-find x 'wp:extent)))
         (props (ox-find x 'wp:docPr))
         (width (office-emu->length (and extent (ox-attr extent 'cx))))
         (float (and one? (docx-float (ox-find x 'wp:anchor) width))))
    (append-map
      (lambda (blip)
        (map (lambda (n) (if float (office-add-attrs n float) n))
             (docx-image-node
               (ox-attr blip 'r:embed)
               width
               (office-emu->length (and extent (ox-attr extent 'cy)))
               (and props (or (ox-attr props 'descr) (ox-attr props 'title)))
               (and one? (docx-crop x)))))
      blips)))

(define (docx-emu->cm s)
  ;; a length in English Metric Units, which may be negative or 0
  (with n (office-number s)
    (and n (office-cm (/ n 360000.0)))))

(define (docx-float a width)
  ;; the attributes of a picture which floats (office-float-keys), from
  ;; its element wp:anchor, or #f
  (and a
       (let* ((ph (ox-child a 'wp:positionH))
              (pv (ox-child a 'wp:positionV))
              (text (lambda (p tag) (with y (and p (ox-child p tag)) (and y (ox-text y)))))
              (h-align (text ph 'wp:align))
              (h-off (text ph 'wp:posOffset))
              (v-align (text pv 'wp:align))
              (v-off (text pv 'wp:posOffset))
              (h-rel (with r (and ph (ox-attr ph 'relativeFrom))
                       (assoc-ref '(("column" . "column") ("page" . "page")
                                    ("margin" . "margin") ("character" . "character")
                                    ("leftMargin" . "left-margin")
                                    ("rightMargin" . "right-margin")
                                    ("insideMargin" . "left-margin")
                                    ("outsideMargin" . "right-margin"))
                                  (or r ""))))
              (v-rel (with r (and pv (ox-attr pv 'relativeFrom))
                       (assoc-ref '(("paragraph" . "paragraph") ("line" . "line")
                                    ("page" . "page") ("margin" . "margin")
                                    ("topMargin" . "top-margin")
                                    ("bottomMargin" . "bottom-margin")
                                    ("insideMargin" . "top-margin")
                                    ("outsideMargin" . "bottom-margin"))
                                  (or r ""))))
              (h-pos (cond ((in? h-align '("left" "right" "center" "inside" "outside"))
                            h-align)
                           (h-off "offset")
                           (else "left")))
              (v-pos (cond ((in? v-align '("top" "bottom" "center" "inside" "outside"))
                            v-align)
                           (v-off "offset")
                           (else "top")))
              (sq (or (ox-child a 'wp:wrapSquare) (ox-child a 'wp:wrapTight)
                      (ox-child a 'wp:wrapThrough)))
              (wrap (cond ((ox-child a 'wp:wrapSquare) "square")
                          ((ox-child a 'wp:wrapTight) "tight")
                          ((ox-child a 'wp:wrapThrough) "through")
                          ((ox-child a 'wp:wrapTopAndBottom) "top-bottom")
                          ((in? (ox-attr a 'behindDoc) '("1" "true" "on")) "behind")
                          (else "front")))
              (side (with s (and sq (ox-attr sq 'wrapText))
                      (assoc-ref '(("bothSides" . "both") ("left" . "left")
                                   ("right" . "right") ("largest" . "largest"))
                                 (or s ""))))
              (h-offset (and (== h-pos "offset") (docx-emu->cm h-off)))
              (v-offset (and (== v-pos "offset") (docx-emu->cm v-off))))
         `((float ,(office-float-side h-pos h-rel h-offset width))
           (wrap ,wrap) (wrap-side ,side)
           (h-pos ,h-pos) (h-rel ,(or h-rel "column")) (h-offset ,h-offset)
           (v-pos ,v-pos) (v-rel ,(or v-rel "paragraph")) (v-offset ,v-offset)))))

(define (docx-picture x)
  ;; a picture of VML, in older documents
  (docx-text-boxes x)
  (with data (ox-find x 'v:imagedata)
    (if (not data) '()
        (docx-image-node (ox-attr data 'r:id) "" ""
                         (ox-attr data 'o:title)))))

(define (docx-math x display?)
  ;; the formulas of an element m:oMath, or m:oMathPara which holds
  ;; several: each as MathML
  (if (func? x 'm:oMathPara)
      (append-map (lambda (y) (docx-math y #t)) (ox-childs x 'm:oMath))
      (list (office-node 'math `((display ,(and display? "true"))
                                 (form "sxml"))
                         (omml->mathml x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the styles of runs which are not a way of writing: the look of a link,
;; of the mark of a note
(define docx-plain-styles
  '("hyperlink" "internet link" "followedhyperlink" "footnote reference"
    "endnote reference" "footnote characters" "endnote characters"
    "footnote anchor" "endnote anchor"))

(define (docx-run-wrappers r)
  ;; the wrappers of the text of a run, the innermost first; hidden is the
  ;; first one of a run which is not shown
  (let* ((own (docx-run-properties (ox-child r 'w:rPr)))
         (id (docx-get own 'style))
         (names (docx-style-names id))
         ;; (a named style of text is said by docx-run, which wraps the
         ;; text in a span of this style)
         (props (if (or (null? names) (in? (car names) docx-plain-styles)
                        (docx-code-style? names))
                    (if (and (pair? names) (docx-code-style? names))
                        (append own (docx-style id))
                        own)
                    own))
         (mark (with m (or (docx-get props 'mark) (docx-get own 'shade))
                 ;; (yellow is the color of highlighting of TeXmacs)
                 (if (and (string? m) (== (locase-all m) "#ffff00")) #t m)))
         (size (docx-get own 'size))
         (font (with f (docx-get own 'font)
                 (and f (not (office-monospace? f)) f)))
         (code? (or (docx-get props 'code)
                    (list-or (map (lambda (n)
                                    (in? n '("verbatim char" "html code"
                                             "html typewriter" "source text"
                                             "code" "inline code")))
                                  names))))
         (complex? (docx-get props 'complex)))
    (list-filter
      (list (and (or (docx-get props 'hidden) (docx-get (docx-style id) 'hidden))
                 'hidden)
            (and code? 'code)
            (and (docx-get props 'color) (list 'color (docx-get props 'color)))
            (cond ((string? mark) (list 'mark mark))
                  (mark 'mark)
                  (else #f))
            ;; the size and the font of the run, by hand
            (and size (list 'size (office-decimal size)))
            (and font (list 'font font))
            (and (docx-get props 'sub) 'sub)
            (and (docx-get props 'sup) 'sup)
            (and (docx-get props 'smallcaps) 'smallcaps)
            (cond ((docx-get props 'dstrike) (list 'strike "double"))
                  ((docx-get props 'strike) 'strike)
                  (else #f))
            (cond ((not (docx-get props 'underline)) #f)
                  ((docx-get props 'underline-kind) (list 'underline (docx-get props 'underline-kind)))
                  (else 'underline))
            (and (docx-get props 'caps) 'caps)
            (with v (docx-get own 'letters) (and v (list 'letters (office-decimal v))))
            ;; (upright text in an italic paragraph is emphasized)
            (and (or (docx-get props (if complex? 'em-cs 'em))
                     (and docx-italic? (assoc 'em own) (not (docx-get own 'em))))
                 'em)
            (and (docx-get props (if complex? 'strong-cs 'strong)) 'strong))
      identity)))

(define (docx-code-style? names)
  (list-or (map (lambda (n)
                  (in? n '("verbatim char" "html code" "html typewriter"
                           "source text" "code" "inline code")))
                names)))

(define (docx-span-style r)
  ;; the named style of text of the run r, or #f
  (let* ((own (docx-run-properties (ox-child r 'w:rPr)))
         (id (docx-get own 'style))
         (names (docx-style-names id)))
    (and id (pair? names) (not (in? (car names) docx-plain-styles))
         (not (docx-code-style? names))
         (begin (ahash-set! docx-used-styles id #t) id))))

(define (docx-trim-note l)
  ;; the text of a note starts with its mark and a space: without the space
  (if (and (pair? l) (func? (car l) 'p))
      (let* ((p (car l))
             (c (ox-children p)))
        (if (and (pair? c) (func? (car c) 'tab))
            (docx-trim-note (cons (apply office-node
                                         (cons* 'p (ox-attrs p) (cdr c)))
                                  (cdr l)))
        (if (and (pair? c) (string? (car c)))
            (cons (apply office-node
                         (cons* 'p (ox-attrs p)
                                (office-merge
                                  (cons (string-trim-left-spaces (car c)) (cdr c)))))
                  (cdr l))
            l)))
      l))

(define (string-trim-left-spaces s)
  (let loop ((i 0))
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
        (loop (+ i 1))
        (substring s i (string-length s)))))

(define (docx-read-comments)
  (set! docx-comments (make-ahash-table))
  (with root (office-xml docx-archive "word/comments.xml")
    (when root
      (docx-read-rels "word/comments.xml")
      (for (x (ox-childs root 'w:comment))
        (ahash-set! docx-comments (ox-attr x 'w:id) x)))))

(define (docx-comment id)
  ;; a comment, with its author and its date, where it is referred to
  (with x (ahash-ref docx-comments id)
    (if (or (not x) (pair? docx-open-notes)) '()
        (let ((old-part docx-part)
              (old-notes docx-open-notes)
              (old-fields docx-fields))
          (set! docx-part "word/comments.xml")
          (set! docx-open-notes (list (cons 'comment id)))
          (set! docx-fields '())
          (with l (docx-blocks (ox-children x))
            (set! docx-part old-part)
            (set! docx-open-notes old-notes)
            (set! docx-fields old-fields)
            (list (apply office-node
                         (cons* 'comment
                                `((author ,(ox-attr x 'w:author))
                                  (date ,(ox-attr x 'w:date)))
                                l))))))))

(define (docx-note kind id)
  ;; the note, but inside itself; the fields of the text around it are not
  ;; those of the note
  (let ((entry (ahash-ref docx-notes (cons kind id)))
        (key (cons kind id)))
    (if (or (not entry) (in? key docx-open-notes)) '()
        (let ((old-part docx-part)
              (old-notes docx-open-notes)
              (old-fields docx-fields))
          (set! docx-part (car entry))
          (set! docx-open-notes (cons key old-notes))
          (set! docx-fields '())
          (with l (docx-blocks (ox-children (cdr entry)))
            (set! docx-part old-part)
            (set! docx-open-notes old-notes)
            (set! docx-fields old-fields)
            (list (cons 'note (docx-trim-note l))))))))

;; The characters of a font of symbols (w:sym) have codes of this font,
;; from F000 on or from 0: those which are common, as codes of Unicode.
(define docx-symbol-codes
  '((#x22 . #x2200) (#x24 . #x2203) (#x27 . #x220b) (#x2a . #x2217)
    (#x2d . #x2212) (#x40 . #x2245) (#x5c . #x2234) (#x5e . #x22a5)
    (#x7e . #x223c) (#xa2 . #x2032) (#xa3 . #x2264) (#xa4 . #x2044)
    (#xa5 . #x221e) (#xa7 . #x2663) (#xa8 . #x2666) (#xa9 . #x2665)
    (#xaa . #x2660) (#xab . #x2194) (#xac . #x2190) (#xad . #x2191)
    (#xae . #x2192) (#xaf . #x2193) (#xb0 . #xb0) (#xb1 . #xb1)
    (#xb2 . #x2033) (#xb3 . #x2265) (#xb4 . #xd7) (#xb5 . #x221d)
    (#xb6 . #x2202) (#xb7 . #x2022) (#xb8 . #xf7) (#xb9 . #x2260)
    (#xba . #x2261) (#xbb . #x2248) (#xbc . #x2026) (#xc0 . #x2135)
    (#xc1 . #x2111) (#xc2 . #x211c) (#xc3 . #x2118) (#xc4 . #x2297)
    (#xc5 . #x2295) (#xc6 . #x2205) (#xc7 . #x2229) (#xc8 . #x222a)
    (#xc9 . #x2283) (#xca . #x2287) (#xcb . #x2284) (#xcc . #x2282)
    (#xcd . #x2286) (#xce . #x2208) (#xcf . #x2209) (#xd0 . #x2220)
    (#xd1 . #x2207) (#xd2 . #xae) (#xd3 . #xa9) (#xd4 . #x2122)
    (#xd5 . #x220f) (#xd6 . #x221a) (#xd7 . #x22c5) (#xd8 . #xac)
    (#xd9 . #x2227) (#xda . #x2228) (#xdb . #x21d4) (#xdc . #x21d0)
    (#xdd . #x21d1) (#xde . #x21d2) (#xdf . #x21d3) (#xe0 . #x25ca)
    (#xe1 . #x2329) (#xe5 . #x2211) (#xf1 . #x232a) (#xf2 . #x222b)))

;; the letters of the font Symbol, from A and from a, are Greek ones
(define docx-symbol-capitals
  '(#x391 #x392 #x3a7 #x394 #x395 #x3a6 #x393 #x397 #x399 #x3d1 #x39a #x39b
    #x39c #x39d #x39f #x3a0 #x398 #x3a1 #x3a3 #x3a4 #x3a5 #x3c2 #x3a9 #x39e
    #x3a8 #x396))

(define docx-symbol-smalls
  '(#x3b1 #x3b2 #x3c7 #x3b4 #x3b5 #x3c6 #x3b3 #x3b7 #x3b9 #x3d5 #x3ba #x3bb
    #x3bc #x3bd #x3bf #x3c0 #x3b8 #x3c1 #x3c3 #x3c4 #x3c5 #x3d6 #x3c9 #x3be
    #x3c8 #x3b6))

(define docx-wingdings-codes
  '((#x22 . #x2702) (#x28 . #x260e) (#x2a . #x2709) (#x45 . #x261c)
    (#x46 . #x261e) (#x4a . #x263a) (#x4b . #x1f610) (#x4c . #x2639)
    (#x6c . #x25cf) (#x6e . #x25a0) (#x6f . #x25a1) (#x71 . #x2751)
    (#x75 . #x25c6) (#x76 . #x2756) (#x9f . #x2022) (#xa1 . #x25cb)
    (#xa7 . #x25aa) (#xa8 . #x25fb) (#xab . #x2605) (#xd8 . #x27a2)
    (#xdf . #x2190) (#xe0 . #x2192) (#xe1 . #x2191) (#xe2 . #x2193)
    (#xe3 . #x2196) (#xe4 . #x2197) (#xe5 . #x2199) (#xe6 . #x2198)
    (#xe7 . #x2190) (#xe8 . #x2192) (#xe9 . #x2191) (#xea . #x2193)
    (#xef . #x21e6) (#xf0 . #x21e8) (#xf1 . #x21e7) (#xf2 . #x21e9)
    (#xf3 . #x2194) (#xf4 . #x2195) (#xfb . #x2718) (#xfc . #x2714)
    (#xfd . #x2612) (#xfe . #x2611)))

(define (docx-hexadecimal s)
  ;; the number which the string s writes with digits from 0 to F, or #f
  (and (string? s) (!= s "") (<= (string-length s) 6)
       (let loop ((l (string->list (locase-all s))) (n 0))
         (cond ((null? l) n)
               ((char-numeric? (car l))
                (loop (cdr l) (+ (* 16 n) (- (char->integer (car l)) 48))))
               ((in? (car l) '(#\a #\b #\c #\d #\e #\f))
                (loop (cdr l) (+ (* 16 n) (- (char->integer (car l)) 87))))
               (else #f)))))

(define (docx-symbol x)
  ;; the character of an element w:sym, which gives a font and a code. A
  ;; code of a font of symbols which is not known is shown as a character
  ;; which stands for it; in another font the code is the one of Unicode.
  (let* ((font (locase-all (or (ox-attr x 'w:font) "")))
         (n (docx-hexadecimal (ox-attr x 'w:char)))
         (c (and n (if (and (>= n #xf000) (<= n #xf0ff)) (- n #xf000) n)))
         (u (cond ((not c) #f)
                  ((== font "symbol")
                   (cond ((assoc-ref docx-symbol-codes c) => identity)
                         ((and (>= c #x41) (<= c #x5a))
                          (list-ref docx-symbol-capitals (- c #x41)))
                         ((and (>= c #x61) (<= c #x7a))
                          (list-ref docx-symbol-smalls (- c #x61)))
                         ;; (the digits and the punctuation are the usual ones)
                         ((and (>= c #x20) (< c #x40)) c)
                         (else #xfffd)))
                  ((== font "wingdings")
                   (or (assoc-ref docx-wingdings-codes c) #xfffd))
                  ((or (string-starts? font "wingdings")
                       (in? font '("webdings" "mt extra" "zapfdingbats")))
                   #xfffd)
                  ((or (< c #x20) (and (>= c #xd800) (<= c #xdfff))
                       (> c #x10ffff))
                   #xfffd)
                  (else c))))
    (if u (list (office-utf8 u)) '())))

(define (docx-run-item x)
  ;; the nodes of a child of a run
  (cond ((not (pair? x)) '())
        ((func? x 'w:t) (list (ox-text x)))
        ((func? x 'w:tab) '((tab)))
        ((func? x 'w:br)
         (if (== (ox-attr x 'w:type) "page") '((pagebreak)) '((br))))
        ((func? x 'w:cr) '((br)))
        ((func? x 'w:noBreakHyphen) '("-"))
        ((func? x 'w:sym) (docx-symbol x))
        ((func? x 'w:drawing) (docx-drawing x))
        ((func? x 'w:pict) (docx-picture x))
        ((func? x 'w:object) (docx-picture x))
        ((func? x 'mc:AlternateContent)
         ;; the first of the choices and of the fallback which gives
         ;; something: a choice may be a drawing which is not read
         ;; (or a box of text, whose paragraphs come after the paragraph)
         (let loop ((l (append (ox-childs x 'mc:Choice)
                               (ox-childs x 'mc:Fallback))))
           (if (null? l) '()
               (let* ((floats docx-floats)
                      (r (append-map docx-run-item (ox-children (car l)))))
                 (if (and (null? r) (eq? floats docx-floats)) (loop (cdr l)) r)))))
        ((func? x 'w:delText) (list (ox-text x)))
        ((func? x 'w:commentReference) (docx-comment (ox-attr x 'w:id)))
        ((func? x 'w:footnoteReference) (docx-note 'foot (ox-attr x 'w:id)))
        ((func? x 'w:endnoteReference) (docx-note 'end (ox-attr x 'w:id)))
        (else '())))

(define (docx-run r)
  ;; the nodes of a run: its text inside its wrappers; what is not text
  ;; stays outside. A run of hidden text gives nothing.
  (let ((w (docx-run-wrappers r))
        (style (docx-span-style r)))
    (if (and (pair? w) (== (car w) 'hidden)) '()
        (append-map (lambda (x)
                      (cond ((not (string? x)) (list x))
                            (style `((span (@ (style ,style)) ,@(office-wrap (list x) w))))
                            (else (office-wrap (list x) w))))
                    (append-map docx-run-item (docx-run-children r))))))

(define (docx-run-children r)
  ;; the children of a run; the mark of a note which the run writes after
  ;; it (a star) is left out: the note has its own
  (with l (ox-children r)
    (if (list-or (map (lambda (x)
                        (and (pair? x)
                             (in? (car x) '(w:footnoteReference w:endnoteReference))
                             (in? (ox-attr x 'w:customMarkFollows) '("1" "true" "on"))))
                      l))
        (list-filter l (lambda (x) (not (func? x 'w:t))))
        l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The text of a paragraph, with its links and its fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-field-words instr)
  ;; the words of the instruction of a field, each (text . quoted?): a
  ;; word in quotes may hold spaces, and \" and \\ for a quote and a \
  (let loop ((l (string->list instr)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((in? (car l) '(#\space #\tab #\newline #\return)) (loop (cdr l) acc))
          ((char=? (car l) #\")
           (let sub ((r (cdr l)) (w '()))
             (cond ((null? r)
                    (reverse (cons (cons (list->string (reverse w)) #t) acc)))
                   ((char=? (car r) #\")
                    (loop (cdr r) (cons (cons (list->string (reverse w)) #t) acc)))
                   ((and (char=? (car r) #\\) (pair? (cdr r))
                         (in? (cadr r) '(#\" #\\)))
                    (sub (cddr r) (cons (cadr r) w)))
                   (else (sub (cdr r) (cons (car r) w))))))
          (else
            (let sub ((r l) (w '()))
              (if (or (null? r) (in? (car r) '(#\space #\tab #\newline #\return)))
                  (loop r (cons (cons (list->string (reverse w)) #f) acc))
                  (sub (cdr r) (cons (car r) w))))))))

;; the switches of a field which are followed by a value
(define docx-field-switches
  '(("hyperlink" "\\l" "\\o" "\\t") ("ref" "\\d") ("seq" "\\r" "\\s")))

(define (docx-field-parse instr)
  ;; (name arguments switches) of the instruction of a field: its name in
  ;; small letters, its arguments, and its switches as (switch . value),
  ;; the value being #t for a switch without one
  (let* ((l (docx-field-words instr))
         (name (if (null? l) "" (locase-all (caar l))))
         (valued (append '("\\*" "\\#" "\\@")
                         (or (assoc-ref docx-field-switches name) '())))
         (switch? (lambda (w) (and (not (cdr w)) (string-starts? (car w) "\\")))))
    (let loop ((l (if (null? l) l (cdr l))) (args '()) (switches '()))
      (cond ((null? l) (list name (reverse args) (reverse switches)))
            ((switch? (car l))
             (if (and (in? (locase-all (caar l)) valued) (pair? (cdr l))
                      (not (switch? (cadr l))))
                 (loop (cddr l) args
                       (cons (cons (locase-all (caar l)) (caadr l)) switches))
                 (loop (cdr l) args
                       (cons (cons (locase-all (caar l)) #t) switches))))
            (else (loop (cdr l) (cons (caar l) args) switches))))))

(define (docx-field instr l . whole?)
  ;; the result l of a field: a link, a reference, or the result itself;
  ;; another field which is whole in its paragraph is a field node, with
  ;; its instruction (a page number, a date, a citation of Zotero)
  (let* ((f (docx-field-parse instr))
         (name (car f))
         (arg (if (null? (cadr f)) "" (caadr f))))
    (cond ((== name "toc") (list '(!toc)))
          ((and (null? l) (in? name '("ref" "pageref")) (!= arg ""))
           ;; (a reference whose result was not computed: TeXmacs does it)
           (list `(ref (@ (name ,arg) ,@(if (== name "pageref") '((kind "page")) '())))))
          ((and (null? l) (not (and (pair? whole?) (car whole?)))) l)
          ((and (null? l) (not (in? name '("page" "numpages" "date" "time"
                                         "sectionpages" "author" "title"
                                         "filename" "docproperty")))
                (not (string-starts? name "addin")))
           l)
          ((== name "hyperlink")
           ;; an address, a bookmark in it (the switch \l), or both
           (let* ((mark (assoc-ref (caddr f) "\\l"))
                  (href (string-append arg (if (string? mark)
                                               (string-append "#" mark) ""))))
             (if (== href "") l (list `(link (@ (href ,href)) ,@l)))))
          ((and (== name "ref") (!= arg ""))
           (list `(ref (@ (name ,arg)) ,@l)))
          ;; the page of a bookmark, not its number
          ((and (== name "pageref") (!= arg ""))
           (list `(ref (@ (name ,arg) (kind "page")) ,@l)))
          ;; a number of a sequence: of the figures, of the tables
          ((and (== name "seq") (!= arg ""))
           (list `(seq (@ (name ,arg)) ,@l)))
          ((and (pair? whole?) (car whole?) (!= name "")
                (not (in? name '("toc" "index" "xe" "tc" "ta" "rd"))))
           (list `(field (@ (code ,instr)) ,@l)))
          (else l))))

(define (docx-unwrap l)
  ;; the children of a paragraph, with those of the elements which only
  ;; hold others in their place
  (append-map
    (lambda (x)
      (cond ((not (pair? x)) '())
            ;; the changes which are tracked: marks around what they hold
            ;; (office-changes makes nodes of them)
            ((in? (car x) '(w:ins w:moveTo w:del w:moveFrom))
             (let ((kind (if (in? (car x) '(w:ins w:moveTo)) 'ins 'del))
                   (attrs `((author ,(ox-attr x 'w:author)) (date ,(ox-attr x 'w:date)))))
               (append (list `(!change-start ,kind ,attrs))
                       (docx-unwrap (ox-children x))
                       (list `(!change-end ,kind)))))
            ((in? (car x) '(w:smartTag w:customXml w:sdtContent w:dir w:bdo))
             (docx-unwrap (ox-children x)))
            ((func? x 'w:sdt)
             (with c (ox-child x 'w:sdtContent)
               (if c (docx-unwrap (ox-children c)) '())))
            (else (list x))))
    l))

(define (docx-inlines-sub l fields)
  ;; (nodes . fields) for the children l of a paragraph. A field is a run
  ;; which begins it, runs with its instruction, a run which separates, the
  ;; runs of its result and a run which ends it: fields is the stack of the
  ;; fields which are open, each (state instruction nodes-before). A field
  ;; may end in a later paragraph: fields are those which are open at the
  ;; start of this one, and the result has those which are open at its end.
  (let loop ((l (docx-unwrap l)) (acc '()) (fields fields))
    (if (null? l)
        ;; what there is here of the fields which go on is kept
        (let close ((acc acc) (open fields) (left '()))
          (if (null? open) (cons (reverse acc) (reverse left))
              (close (append (reverse (docx-field (cadar open)
                                                  (if (== (caar open) 'result)
                                                      (reverse acc) '())))
                             (caddar open))
                     (cdr open)
                     ;; (a field which goes on did not start in the
                     ;; next paragraph: it is not a field node there)
                     (cons (list (caar open) (cadar open) '() 'carried) left))))
        (let* ((x (car l))
               (char (and (func? x 'w:r) (ox-child x 'w:fldChar)))
               (type (and char (ox-attr char 'w:fldCharType)))
               (state (and (pair? fields) (caar fields))))
          (cond ((== type "begin")
                 (loop (cdr l) '() (cons (list 'instr "" acc) fields)))
                ((and (== type "separate") (pair? fields))
                 (loop (cdr l) '()
                       (cons (append (list 'result (cadar fields) (caddar fields))
                                     (cdddr (car fields)))
                             (cdr fields))))
                ((and (== type "end") (pair? fields))
                 (loop (cdr l)
                       (append (reverse (docx-field (cadar fields)
                                                    (if (== state 'result)
                                                        (reverse acc) '())
                                                    (null? (cdddr (car fields)))))
                               (caddar fields))
                       (cdr fields)))
                ((== state 'instr)
                 ;; the instruction, in one run or in several
                 (loop (cdr l) acc
                       (cons (cons* 'instr
                                   (string-append
                                     (cadar fields)
                                     (if (func? x 'w:r)
                                         (apply string-append
                                                (map ox-text
                                                     (ox-childs x 'w:instrText)))
                                         ""))
                                   (caddar fields)
                                   (cdddr (car fields)))
                             (cdr fields))))
                (else
                  (loop (cdr l) (append (reverse (docx-inline x)) acc)
                        fields)))))))

(define (docx-inlines l)
  ;; the nodes of the children l of an element inside a paragraph
  (office-changes (car (docx-inlines-sub l '()))))

(define (docx-inline x)
  ;; the nodes of a child of a paragraph
  (cond ((func? x 'w:r) (docx-run x))
        ((and (pair? x) (in? (car x) '(!change-start !change-end))) (list x))
        ((func? x 'w:hyperlink)
         (let* ((rel (docx-rel (ox-attr x 'r:id)))
                (anchor (ox-attr x 'w:anchor))
                (href (cond ((and rel anchor)
                             (string-append (car rel) "#" anchor))
                            (rel (car rel))
                            (anchor (string-append "#" anchor))
                            (else #f)))
                (l (docx-inlines (ox-children x))))
           (cond ((null? l) '())
                 (href (list `(link (@ (href ,href)) ,@l)))
                 (else l))))
        ((func? x 'w:fldSimple)
         (docx-field (or (ox-attr x 'w:instr) "")
                     (docx-inlines (ox-children x))
                     #t))
        ((func? x 'w:bookmarkStart)
         (with name (ox-attr x 'w:name)
           (if (or (not name) (== name "_GoBack")) '()
               (list `(bookmark (@ (name ,name)))))))
        ((func? x 'm:oMathPara) (docx-math x #t))
        ((func? x 'm:oMath) (docx-math x #f))
        (else '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraphs and tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-paragraph x)
  ;; the paragraph, with the list it is an item of as attributes list-id,
  ;; list-level and list-kind, for docx-group-lists; the boxes of text in
  ;; it come after it, and a break of page in it cuts it in two
  (let* ((own (docx-paragraph-properties (ox-child x 'w:pPr)))
         (style (docx-get own 'style))
         (props (append own (docx-style style)))
         (role (docx-role props))
         ;; (the alignment of its style, when it is not the usual one)
         (align (or (docx-get own 'align)
                    (with a (docx-get props 'align)
                      (and (!= a (docx-get (append (docx-style docx-default-style)
                                                   docx-defaults)
                                           'align))
                           a))))
         (id (docx-get props 'list-id))
         (level (or (docx-get props 'list-level) "0"))
         (kind (and id (docx-list-kind id level)))
         (format (and id (ahash-ref docx-list-formats (cons id level))))
         ;; a level without a mark is no item
         (more? (and id (!= id "0") (not (car role)) (== kind "none")))
         (item? (and id (!= id "0") (not (car role)) (not more?)))
         (old-floats docx-floats)
         (old-italic docx-italic?)
         (l (begin
              (set! docx-floats '())
              ;; (an italic paragraph: the runs of its style are italic)
              (set! docx-italic?
                    (docx-get (apply append
                                     (map cdr (list-filter (docx-style style)
                                                           (lambda (p) (eq? (car p) 'run)))))
                              'em))
              (with r (docx-inlines-sub (ox-children x) docx-fields)
                (set! docx-fields (cdr r))
                (docx-numbered-formula (office-merge (office-changes (car r)))))))
         (floats (with f docx-floats (set! docx-floats old-floats) f))
         (_ (set! docx-italic? old-italic))
         (attrs `((role ,(car role)) (level ,(cadr role))
                  ;; the named style of a plain paragraph
                  (style ,(and (not (car role)) (not id) style (null? docx-open-notes)
                               (begin (ahash-set! docx-used-styles style #t) style)))
                  (align ,(cond ((== align "center") "center")
                                ((in? align '("right" "end")) "right")
                                ((in? align '("both" "distribute")) "justify")
                                ((in? align '("left" "start")) "left")
                                (else #f)))
                  ;; a heading has a number when its list numbers it
                  (numbered ,(and (== (car role) "heading")
                                  (not (and id (!= id "0") (== kind "number")))
                                  "no"))
                  (dropcap ,(and (docx-get own 'dropcap) "true"))
                  ;; a plain paragraph which is indented, as one more
                  ;; paragraph of an item is
                  (list-more ,(with ind (docx-get own 'indent)
                                (cond (more?
                                       (number->string
                                         (+ (office-integer level 0 0 8) 1)))
                                      ((and ind (>= ind 600) (not item?)
                                            (not (car role)))
                                       (number->string
                                         (quotient (+ ind 120) 720)))
                                      (else #f))))
                  (list-id ,(and item? (docx-list-group id)))
                  (list-level ,(and item? level))
                  (list-kind ,(and item? kind))
                  ;; the formatting of a plain paragraph by hand
                  ,@(if (or (car role) id) '()
                        (map (lambda (k)
                               (with v (and (not (and (eq? k 'indent-left) more?))
                                            (docx-get own k))
                                 (list k (cond ((number? v) (office-decimal v))
                                               ((eq? v #t) "true")
                                               (else v)))))
                             '(indent-left indent-right first-indent space-before
                               space-after line-spacing background border)))
                  (list-num ,(and item? id))
                  (list-format ,(and item? format (car format)))
                  (list-start ,(and item? format (cadr format))))))
    ;; (the style of a heading, for the look of the titles)
    (when (and (== (car role) "heading") style (null? docx-open-notes))
      (ahash-set! docx-used-styles style #t))
    ;; (the style of the usual paragraphs is the one of most of them)
    (when (and (not (car role)) (not id) (null? docx-open-notes))
      (with k (or style "")
        (ahash-set! docx-body-styles k (+ 1 (or (ahash-ref docx-body-styles k) 0)))))
    (append
      (if (docx-get own 'page-break) '((pagebreak)) '())
      (cond ((list-find l (lambda (y) (func? y '!toc))) '((toc)))
            ((and (null? l) (not item?)) '())
            (else (docx-cut-pages l attrs)))
      floats
      ;; the end of a section, with its columns
      (with sect (and (ox-child x 'w:pPr) (ox-child (ox-child x 'w:pPr) 'w:sectPr))
        (if (and sect (null? docx-open-notes))
            (list `(!section ,(docx-columns sect)))
            '())))))

(define (docx-columns sect)
  ;; the number of columns of a section
  (with c (ox-child sect 'w:cols)
    (office-integer (and c (ox-attr c 'w:num)) 1 1 10)))

(define (docx-sections l last)
  ;; the blocks l with the sections in several columns as blocks columns;
  ;; last is the number of columns of the last section
  (let loop ((l l) (part '()) (acc '()))
    (let ((flush (lambda (n)
                   (cond ((null? part) acc)
                         ((> n 1)
                          (cons `(columns (@ (count ,(number->string n)))
                                          ,@(reverse part))
                                acc))
                         (else (append part acc))))))
      (cond ((null? l) (reverse (flush last)))
            ((func? (car l) '!section) (loop (cdr l) '() (flush (cadr (car l)))))
            (else (loop (cdr l) (cons (car l) part) acc))))))

(define (docx-numbered-formula l)
  ;; a formula between two tabs, and its number after them, as Word and
  ;; TeXmacs write a numbered equation: the formula displayed, and its
  ;; number after a tab
  (if (and (>= (length l) 4) (func? (car l) 'tab) (func? (cadr l) 'math)
           (func? (caddr l) 'tab))
      (let ((m (cadr l)))
        (cons (apply office-node
                     (cons* 'math (cons '(display "true")
                                        (map (lambda (a) (list (car a) (cadr a)))
                                             (list-filter (ox-attrs m)
                                                          (lambda (a) (!= (car a) 'display)))))
                            (ox-children m)))
              (cddr l)))
      l))

(define (docx-cut-pages l attrs)
  ;; the paragraph of the nodes l, or several when a break of page is in
  ;; it: the paragraphs after the first are plain ones
  (let loop ((l l) (part '()) (acc '()) (first? #t))
    (let ((flush (lambda ()
                   (if (and (null? part)
                            (not (and first? (cadr (assoc 'list-id attrs)))))
                       acc
                       (cons (apply office-node
                                    (cons* 'p (if first? attrs
                                                  (list-filter attrs
                                                    (lambda (a) (in? (car a) '(align style)))))
                                           (reverse part)))
                             acc)))))
      (cond ((null? l) (reverse (flush)))
            ((func? (car l) 'pagebreak)
             (loop (cdr l) '() (cons '(pagebreak) (flush)) #f))
            (else (loop (cdr l) (cons (car l) part) acc first?))))))

;; The borders of a table are those of its style, of the table itself and
;; of each cell, in this order. A style has borders for the whole table
;; (around it, and inside between the rows and the columns) and others for
;; some of its parts: the first row, the last one, the first column, the
;; last one, the rows of odd and of even rank. Borders are association
;; lists side -> #t or #f, with the sides top, bottom, left, right,
;; insideH and insideV.

(define docx-border-sides
  '((w:top . top) (w:bottom . bottom) (w:left . left) (w:start . left)
    (w:right . right) (w:end . right) (w:insideH . insideH)
    (w:insideV . insideV)))

(define (docx-borders x)
  ;; the borders of an element w:tblBorders or w:tcBorders
  (if (not x) '()
      (append-map
        (lambda (y)
          (with side (assoc-ref docx-border-sides (car y))
            (if side
                (list (cons side (not (in? (docx-val y) '("nil" "none")))))
                '())))
        (ox-elements x))))

(define (docx-fill x)
  ;; the color of the background of an element w:tcPr, or #f
  (let* ((shd (and x (ox-child x 'w:shd)))
         (fill (and shd (ox-attr shd 'w:fill))))
    (and fill (== (string-length fill) 6) (!= (locase-all fill) "ffffff")
         (string-append "#" (locase-all fill)))))

(define (docx-table-style-properties x)
  ;; the properties of a style of tables
  (let ((pr (ox-child x 'w:tblPr))
        (tc (ox-child x 'w:tcPr)))
    (append
      (with b (and pr (ox-child pr 'w:tblBorders))
        (if b (list (cons 'borders (docx-borders b))) '()))
      (with f (docx-fill tc)
        (if f (list (cons 'fill f)) '()))
      ;; the parts of the table, each (borders fill)
      (map (lambda (part)
             (with ptc (ox-child part 'w:tcPr)
               (cons (string->symbol (string-append "part-" (or (ox-attr part 'w:type) "")))
                     (list (docx-borders (and ptc (ox-child ptc 'w:tcBorders)))
                           (docx-fill ptc)))))
           (ox-childs x 'w:tblStylePr)))))

;; the parts of a table which a row or a cell says it is in, by the rank of
;; their bit in w:cnfStyle
(define docx-parts
  '(part-firstRow part-lastRow part-firstCol part-lastCol part-band1Vert
    part-band2Vert part-band1Horz part-band2Horz))

(define (docx-cnf x)
  ;; the parts of the element w:cnfStyle x, or #f without it
  (let* ((v (and x (docx-val x))))
    (and v (>= (string-length v) 8)
         (list-filter
           (map (lambda (part i) (and (char=? (string-ref v i) #\1) part))
                docx-parts (iota 8))
           identity))))

(define (docx-look tblpr)
  ;; the parts which the table uses: (first-row? last-row? first-col?
  ;; last-col? bands?)
  (let* ((look (and tblpr (ox-child tblpr 'w:tblLook)))
         (on? (lambda (name) (in? (and look (ox-attr look name)) '("1" "true"))))
         (n (or (and look (docx-hexadecimal (docx-val look))) #x04a0)))
    (if (and look (ox-attr look 'w:firstRow))
        (list (on? 'w:firstRow) (on? 'w:lastRow) (on? 'w:firstColumn)
              (on? 'w:lastColumn) (not (on? 'w:noHBand)))
        (list (odd? (quotient n #x20)) (odd? (quotient n #x40))
              (odd? (quotient n #x80)) (odd? (quotient n #x100))
              (not (odd? (quotient n #x200)))))))

(define (docx-side borders side)
  ;; (#t) or (#f) when the borders say something of this side, else ()
  (with p (assoc side borders)
    (if p (list (cdr p)) '())))

(define (docx-cell-format i j rows cols parts style tbl-borders own-pr)
  ;; (borders fill) of the cell at row i and column j of a table of rows
  ;; and cols: the borders as a string of the letters t, b, l and r
  (let* ((first-row? (== i 0))
         (last-row? (== i (- rows 1)))
         (first-col? (== j 0))
         (last-col? (== j (- cols 1)))
         (part-info (lambda (part) (or (docx-get style part) (list '() #f))))
         ;; what each source says of the four sides, the last one first
         (own (docx-borders (and own-pr (ox-child own-pr 'w:tcBorders))))
         (from-parts
           (lambda (side)
             (append-map
               (lambda (part)
                 (let* ((b (car (part-info part)))
                        (row-part? (in? part '(part-firstRow part-lastRow
                                               part-band1Horz part-band2Horz))))
                   (cond ((== side 'top)
                          (if (or row-part? first-row?) (docx-side b 'top)
                              (docx-side b 'insideH)))
                         ((== side 'bottom)
                          (if (or row-part? last-row?) (docx-side b 'bottom)
                              (docx-side b 'insideH)))
                         ((== side 'left)
                          (if (or (not row-part?) first-col?) (docx-side b 'left)
                              (docx-side b 'insideV)))
                         (else
                          (if (or (not row-part?) last-col?) (docx-side b 'right)
                              (docx-side b 'insideV))))))
               (reverse parts))))
         (from-table
           (lambda (side)
             (cond ((== side 'top)
                    (docx-side tbl-borders (if first-row? 'top 'insideH)))
                   ((== side 'bottom)
                    (docx-side tbl-borders (if last-row? 'bottom 'insideH)))
                   ((== side 'left)
                    (docx-side tbl-borders (if first-col? 'left 'insideV)))
                   (else
                    (docx-side tbl-borders (if last-col? 'right 'insideV))))))
         (on? (lambda (side)
                (with l (append (docx-side own side) (from-parts side)
                                (from-table side))
                  (and (pair? l) (car l)))))
         (letters (string-append (if (on? 'top) "t" "") (if (on? 'bottom) "b" "")
                                 (if (on? 'left) "l" "") (if (on? 'right) "r" "")))
         (fill (or (docx-fill own-pr)
                   (list-find (map (lambda (part) (cadr (part-info part)))
                                   (reverse parts))
                              identity)
                   (docx-get style 'fill))))
    (list (if (== letters "") "none" letters) fill)))

(define (docx-row-parts i rows row look)
  ;; the parts of the table which the row i is in
  (or (docx-cnf (with pr (ox-child row 'w:trPr) (and pr (ox-child pr 'w:cnfStyle))))
      (let ((first? (and (car look) (== i 0)))
            (last? (and (cadr look) (== i (- rows 1)) (> rows 1))))
        (cond (first? '(part-firstRow))
              (last? '(part-lastRow))
              ((not (list-ref look 4)) '())
              ;; the bands start after the first row
              ((odd? (- i (if (car look) 1 0))) '(part-band2Horz))
              (else '(part-band1Horz))))))

(define (docx-cell-parts j cols cell row-parts look)
  ;; the parts of the table which a cell of the column j is in
  (or (docx-cnf (with pr (ox-child cell 'w:tcPr) (and pr (ox-child pr 'w:cnfStyle))))
      (append row-parts
              (if (and (caddr look) (== j 0)) '(part-firstCol) '())
              (if (and (cadddr look) (== j (- cols 1)) (> cols 1))
                  '(part-lastCol) '()))))

(define (docx-cell x header? format)
  ;; the cells of a w:tc: the cell, and those which it covers on its right
  (let* ((pr (ox-child x 'w:tcPr))
         (span (docx-cell-span x))
         (merge (and pr (ox-child pr 'w:vMerge)))
         (covered `(cell (@ (covered "true")))))
    ;; (a cell which goes on a vertical merge keeps its text until
    ;; docx-row-spans knows that there is a cell above it)
    (cons (apply office-node
                 (cons* 'cell
                        `((header ,(and header? "true"))
                          (colspan ,(and (> span 1) (number->string span)))
                          (vmerge ,(and merge (if (== (docx-val merge) "restart")
                                                  "start" "continue")))
                          (borders ,(car format))
                          (background ,(cadr format)))
                        (docx-blocks (ox-children x))))
          (map (lambda (i) covered) (iota (- span 1))))))

(define (docx-cell-span x)
  ;; the number of columns of the cell, at most those of a table of Word
  (let* ((pr (ox-child x 'w:tcPr)))
    (office-integer (and pr (docx-child-val pr 'w:gridSpan)) 1 1 63)))

(define (docx-grid-before x)
  ;; the number of columns which the row x leaves empty at its start
  (let* ((pr (ox-child x 'w:trPr)))
    (office-integer (and pr (docx-child-val pr 'w:gridBefore)) 0 0 63)))

(define (docx-table-children x tag)
  ;; the children tag of a table or of a row, also those which are inside
  ;; a content control or a mark of revision
  (append-map
    (lambda (y)
      (cond ((func? y tag) (list y))
            ((and (pair? y) (in? (car y) '(w:sdt w:sdtContent w:customXml w:ins
                                           w:moveTo)))
             (docx-table-children y tag))
            (else '())))
    (ox-children x)))

(define (docx-deleted-row? x)
  ;; a row which was deleted, with the changes tracked
  (with pr (ox-child x 'w:trPr)
    (and pr (or (ox-child pr 'w:del) (ox-child pr 'w:moveFrom)) #t)))

(define (docx-row x i rows cols look style tbl-borders)
  (let* ((pr (ox-child x 'w:trPr))
         (header? (and pr (ox-child pr 'w:tblHeader)
                       (not (in? (docx-child-val pr 'w:tblHeader)
                                 '("0" "false")))))
         (row-parts (docx-row-parts i rows x look)))
    (cons 'row
          (let loop ((l (docx-table-children x 'w:tc))
                     (j (docx-grid-before x))
                     (acc (map (lambda (k) '(cell (@ (borders "none"))))
                               (iota (docx-grid-before x)))))
            (if (null? l) (reverse acc)
                (let* ((c (car l))
                       (span (docx-cell-span c))
                       ;; a wide cell has the right side of its last column
                       (jr (+ j span -1))
                       (parts (docx-cell-parts j cols c row-parts look))
                       (f (docx-cell-format i j rows cols parts style tbl-borders
                                            (ox-child c 'w:tcPr)))
                       (fr (if (== span 1) f
                               (docx-cell-format i jr rows cols parts style
                                                 tbl-borders (ox-child c 'w:tcPr))))
                       (letters (list->string
                                  (append
                                    (list-filter (string->list (car f))
                                                 (lambda (ch) (in? ch '(#\t #\b #\l))))
                                    (list-filter (string->list (car fr))
                                                 (lambda (ch) (char=? ch #\r))))))
                       (format (list (if (or (== (car f) "none") (== letters ""))
                                         (if (== letters "") "none" letters)
                                         letters)
                                     (cadr f))))
                  (loop (cdr l) (+ j span)
                        (append (reverse (docx-cell c header? format)) acc))))))))

(define (docx-row-spans rows)
  ;; a cell which starts a vertical merge gets the number of rows it
  ;; covers: the cells below it in its column which go on the merge, and
  ;; which are covered. A cell which goes on a merge that no cell above it
  ;; has started is the start of one.
  (with grid (map cdr rows)
    (let loop ((rest grid) (open '()) (acc '()))
      ;; open: the columns of the row before whose cells are in a merge
      (if (null? rest) (reverse acc)
          (loop (cdr rest)
                (list-filter (iota (length (car rest)))
                             (lambda (col)
                               (ox-attr (list-ref (car rest) col) 'vmerge)))
                (cons
                  (cons 'row
                        (map (lambda (cell col)
                               (cond ((not (ox-attr cell 'vmerge)) cell)
                                     ((and (== (ox-attr cell 'vmerge) "continue")
                                           (in? col open))
                                      `(cell (@ (covered "true"))))
                                     (else
                                       (let count ((below (cdr rest)) (n 1))
                                         (if (and (pair? below)
                                                  (< col (length (car below)))
                                                  (== (ox-attr (list-ref (car below) col)
                                                               'vmerge)
                                                      "continue"))
                                             (count (cdr below) (+ n 1))
                                             (apply office-node
                                                    (cons* 'cell
                                                           `((header ,(ox-attr cell 'header))
                                                             (colspan ,(ox-attr cell 'colspan))
                                                             (rowspan ,(and (> n 1) (number->string n)))
                                                             (borders ,(ox-attr cell 'borders))
                                                             (background ,(ox-attr cell 'background)))
                                                           (ox-children cell))))))))
                             (car rest) (iota (length (car rest)))))
                  acc))))))

(define (docx-table-columns x)
  ;; the widths of the columns of the grid, as parts of their sum
  (let* ((grid (ox-child x 'w:tblGrid))
         (l (map (lambda (c) (office-integer (ox-attr c 'w:w) 0 0 1000000))
                 (if grid (ox-childs grid 'w:gridCol) '())))
         (sum (apply + l)))
    (and (pair? l) (> sum 0)
         (string-recompose
           (map (lambda (w) (number->string (/ (round (* 1000.0 (/ w sum))) 1000.0)))
                l)
           " "))))

(define (docx-table-width pr)
  ;; the width of the table: a part of the width of the text, or #f
  (let* ((w (and pr (ox-child pr 'w:tblW)))
         (type (and w (ox-attr w 'w:type)))
         (v (and w (ox-attr w 'w:w)))
         (v (and v (if (string-ends? v "%")
                       (with n (office-number (substring v 0 (- (string-length v) 1)))
                         (and n (* n 50)))
                       (office-number v)))))
    (and v (== type "pct") (> v 0)
         (string-append (number->string (/ (round (/ v 5.0)) 1000.0)) "par"))))

(define (docx-table x)
  (let* ((pr (ox-child x 'w:tblPr))
         (style (docx-style (and pr (docx-child-val pr 'w:tblStyle))))
         (tbl-borders (append (docx-borders (and pr (ox-child pr 'w:tblBorders)))
                              (or (docx-get style 'borders) '())))
         (look (docx-look pr))
         (trs (list-filter (docx-table-children x 'w:tr)
                           (lambda (r) (not (docx-deleted-row? r)))))
         (nrows (length trs))
         (ncols (apply max (cons 1 (map (lambda (r)
                                          (apply + (cons (docx-grid-before r)
                                                         (map docx-cell-span
                                                              (docx-table-children r 'w:tc)))))
                                        trs))))
         (rows (map (lambda (r i)
                      (docx-row r i nrows ncols look style tbl-borders))
                    trs (iota nrows)))
         (align (and pr (docx-child-val pr 'w:jc))))
    (if (null? rows) '()
        (list (apply office-node
                     (cons* 'table
                            `((align ,(cond ((== align "center") "center")
                                            ((in? align '("right" "end")) "right")
                                            (else #f)))
                              (width ,(docx-table-width pr))
                              (columns ,(docx-table-columns x)))
                            (docx-row-spans rows)))))))

(define (docx-block x)
  (cond ((func? x 'w:p) (docx-paragraph x))
        ((func? x 'w:tbl) (docx-table x))
        ((func? x 'w:sdt)
         (with c (ox-child x 'w:sdtContent)
           (if c (docx-blocks-sub (ox-children c)) '())))
        ((and (pair? x) (in? (car x) '(w:ins w:customXml w:moveTo)))
         (docx-blocks-sub (ox-children x)))
        (else '())))

(define (docx-blocks-sub l)
  ;; A bookmark between two paragraphs belongs to the next one.
  (let loop ((l l) (marks '()) (acc '()))
    (cond ((null? l) (reverse acc))
          ((func? (car l) 'w:bookmarkStart)
           (loop (cdr l) (append marks (docx-inline (car l))) acc))
          (else
            (with b (docx-block (car l))
              (if (and (pair? marks) (pair? b) (func? (cAr b) 'p))
                  (let* ((p (cAr b))
                         (attrs (ox-attrs p)))
                    (loop (cdr l) '()
                          (append
                            (list (apply office-node
                                         (cons* 'p attrs
                                                (append marks (ox-children p)))))
                            (cdr (reverse b))
                            acc)))
                  (loop (cdr l) marks (append (reverse b) acc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists: from the paragraphs which are items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-item-paragraph p)
  ;; a paragraph in an item: without the attributes of its list, nor its
  ;; indentation, which is the one of the item
  (apply office-node
         (cons* 'p
                (list-filter (ox-attrs p)
                             (lambda (a) (not (in? (car a) '(list-id list-level list-kind
                                                             list-more list-format
                                                             list-start list-num
                                                             indent-left first-indent)))))
                (ox-children p))))

(define (docx-plain-paragraph p)
  ;; without the attributes of its list
  (apply office-node
         (cons* 'p
                (list-filter (ox-attrs p)
                             (lambda (a) (not (in? (car a) '(list-id list-level
                                                             list-kind list-more
                                                             list-format
                                                             list-start
                                                             list-num)))))
                (ox-children p))))

(define (docx-item-level x)
  (and (func? x 'p) (ox-attr x 'list-id)
       (office-integer (ox-attr x 'list-level) 0 0 8)))

(define (docx-list l level)
  ;; (list . rest): the list of the items at the start of l which are at
  ;; this level or deeper, the deeper ones as lists inside the items
  (let* ((id (ox-attr (car l) 'list-id))
         (kind (ox-attr (car l) 'list-kind))
         (format (and (== kind "number") (ox-attr (car l) 'list-format)))
         (start (and (== kind "number") (ox-attr (car l) 'list-start)))
         (num (ox-attr (car l) 'list-num)))
    (let loop ((l l) (items '()))
      (with n (and (pair? l) (docx-item-level (car l)))
        (cond ((and (pair? l) (pair? items) (func? (car l) 'p)
                    (ox-attr (car l) 'list-more)
                    (> (office-integer (ox-attr (car l) 'list-more) 0 0 100) level))
               ;; one more paragraph of the last item
               (loop (cdr l)
                     (cons (append (car items)
                                   (list (docx-item-paragraph (car l))))
                           (cdr items))))
              ((or (not n) (< n level)
                   (and (== n level)
                        (or (!= (ox-attr (car l) 'list-id) id)
                            (!= (ox-attr (car l) 'list-kind) kind)
                            ;; another list of Word which starts again
                            (and (pair? items)
                                 (!= (ox-attr (car l) 'list-num) num)
                                 (ahash-ref docx-restarts
                                            (cons (ox-attr (car l) 'list-num)
                                                  (ox-attr (car l) 'list-level)))))))
               (cons (apply office-node
                            (cons* 'list
                                   `((kind ,kind)
                                     (format ,(and (!= format "1") format))
                                     (start ,(and (!= start "1") start)))
                                   (reverse items)))
                     l))
              ((== n level)
               (loop (cdr l)
                     (cons (list 'item (docx-item-paragraph (car l))) items)))
              (else
                ;; a deeper level: a list inside the last item
                (with sub (docx-list l n)
                  (loop (cdr sub)
                        (if (null? items)
                            (list (list 'item (car sub)))
                            (cons (append (car items) (list (car sub)))
                                  (cdr items)))))))))))

(define (docx-continued x key counts)
  ;; the list x, which goes on with the numbers of the list key before it:
  ;; the items of one list of Word may be apart, with text between them
  (let* ((n (length (list-filter (ox-children x) (lambda (y) (func? y 'item)))))
         (before (ahash-ref counts key)))
    (ahash-set! counts key (+ n (or before 0)))
    (if (or (not before) (!= (ox-attr x 'kind) "number")) x
        (apply office-node
               (cons* 'list
                      (append (list-filter (ox-attrs x)
                                           (lambda (a) (!= (car a) 'start)))
                              `((start ,(number->string
                                          (+ before (office-integer (ox-attr x 'start)
                                                                    1 0 100000))))))
                      (ox-children x))))))

(define (docx-group-lists l)
  ;; (loops, here and below: a document may have many thousands of blocks)
  (let loop ((l l) (acc '()) (counts (make-ahash-table)))
    (cond ((null? l) (reverse acc))
          ((docx-item-level (car l))
           (let* ((key (cons (ox-attr (car l) 'list-num) (ox-attr (car l) 'list-level)))
                  (r (docx-list l (docx-item-level (car l)))))
             (loop (cdr r) (cons (docx-continued (car r) key counts) acc) counts)))
          ((and (func? (car l) 'p) (ox-attr (car l) 'list-more))
           (loop (cdr l) (cons (docx-plain-paragraph (car l)) acc) counts))
          (else (loop (cdr l) (cons (car l) acc) counts)))))

(define (docx-drop-caps l)
  ;; a large first letter is a paragraph of its own: it is the start of
  ;; the next one
  (let loop ((l l) (acc '()))
    (cond ((or (null? l) (null? (cdr l))) (append (reverse acc) l))
          ((and (func? (car l) 'p) (ox-attr (car l) 'dropcap)
                (func? (cadr l) 'p))
           (loop (cons (apply office-node
                              (cons* 'p (ox-attrs (cadr l))
                                     (office-merge
                                       (append (ox-children (car l))
                                               (ox-children (cadr l))))))
                       (cddr l))
                 acc))
          (else (loop (cdr l) (cons (car l) acc))))))

(define (docx-single-breaks l)
  ;; a break of page at the end of a paragraph and another one before the
  ;; next are one
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((and (func? (car l) 'pagebreak) (pair? acc) (func? (car acc) 'pagebreak))
           (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

(define (docx-blocks l)
  (docx-group-lists (docx-drop-caps (docx-single-breaks (docx-blocks-sub l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-meta)
  ;; the title and the author of docProps/core.xml
  (with root (office-xml docx-archive "docProps/core.xml")
    (if (not root) '()
        (let* ((get (lambda (tag key)
                      (with x (ox-child root tag)
                        (if (and x (!= (string-trim-spaces (ox-text x)) ""))
                            (list (list key (string-trim-spaces (ox-text x))))
                            '()))))
               (l (append (get 'dc:title 'title) (get 'dc:creator 'author))))
          (if (null? l) '() (list (cons 'meta l)))))))

(define (docx-read-theme)
  ;; the fonts of the theme, which the styles may name
  (set! docx-theme-fonts '())
  (let* ((root (office-xml docx-archive "word/theme/theme1.xml"))
         (scheme (and root (ox-find root 'a:fontScheme)))
         (font (lambda (tag)
                 (with x (and scheme (ox-path scheme tag 'a:latin))
                   (and x (ox-attr x 'typeface))))))
    (when (font 'a:minorFont)
      (set! docx-theme-fonts (cons (cons "minor" (font 'a:minorFont)) docx-theme-fonts)))
    (when (font 'a:majorFont)
      (set! docx-theme-fonts (cons (cons "major" (font 'a:majorFont)) docx-theme-fonts)))))

(define (docx-body-style)
  ;; the style of most of the plain paragraphs, or the usual one
  (let ((best #f) (n 0))
    (for (kv (ahash-table->list docx-body-styles))
      (when (> (cdr kv) n) (set! best (car kv)) (set! n (cdr kv))))
    (if (and best (!= best "")) best docx-default-style)))

(define (docx-twips->length v)
  (with n (office-number v)
    (and n (>= n 0) (string-append (office-decimal (/ n 567.0)) "cm"))))

(define (docx-setup body)
  ;; the page of the last section, and the usual paragraph
  (let* ((sect (and body (list-find (reverse (ox-children body))
                                    (lambda (x) (func? x 'w:sectPr)))))
         (size (and sect (ox-child sect 'w:pgSz)))
         (mar (and sect (ox-child sect 'w:pgMar)))
         (props (append (docx-style (docx-body-style)) docx-defaults))
         (num (lambda (key) (with v (docx-get props key)
                              (and v (office-decimal v)))))
         (font (docx-get props 'text-font))
         (align (docx-get props 'align))
         (attrs `((page-width ,(and size (docx-twips->length (ox-attr size 'w:w))))
                  (page-height ,(and size (docx-twips->length (ox-attr size 'w:h))))
                  (landscape ,(and size (== (ox-attr size 'w:orient) "landscape")
                                   "true"))
                  (margin-top ,(and mar (docx-twips->length (ox-attr mar 'w:top))))
                  (margin-bottom ,(and mar (docx-twips->length (ox-attr mar 'w:bottom))))
                  (margin-left ,(and mar (docx-twips->length (ox-attr mar 'w:left))))
                  (margin-right ,(and mar (docx-twips->length (ox-attr mar 'w:right))))
                  (font-size ,(num 'text-size))
                  (font ,(cond ((not font) #f)
                               ((office-monospace? font) "mono")
                               ((office-sans? font) "sans")
                               (else #f)))
                  (first-indent ,(num 'first-indent))
                  (space-before ,(num 'space-before))
                  (space-after ,(num 'space-after))
                  (line-spacing ,(num 'line-spacing))
                  ;; (left when the file says nothing, as Word does)
                  (body-style ,(docx-body-style))
                  (align ,(cond ((in? align '("both" "distribute")) "justify")
                                ((== align "center") "center")
                                ((in? align '("right" "end")) "right")
                                ((or align size) "left")
                                (else #f))))))
    (if (list-or (map cadr attrs)) (list (office-node 'setup attrs)) '())))

(define (docx-part-blocks part)
  ;; the blocks of a part of the archive, as a header
  (let ((root (office-xml docx-archive part))
        (old-part docx-part)
        (old-fields docx-fields)
        (old-notes docx-open-notes))
    (if (not root) '()
        (begin
          (docx-read-rels part)
          (set! docx-part part)
          (set! docx-fields '())
          (set! docx-open-notes (list 'header))
          (with l (docx-blocks (ox-children root))
            (set! docx-part old-part)
            (set! docx-fields old-fields)
            (set! docx-open-notes old-notes)
            l)))))

(define (docx-headers sect)
  ;; the headers and the footers of the last section: (header) blocks with
  ;; the attributes place (header, footer) and kind (default, first, even)
  (let* ((settings (office-xml docx-archive "word/settings.xml"))
         (even? (and settings (ox-child settings 'w:evenAndOddHeaders)))
         (first? (and sect (ox-child sect 'w:titlePg))))
    (if (not sect) '()
        (append-map
          (lambda (x)
            (let* ((place (if (func? x 'w:headerReference) "header" "footer"))
                   (kind (or (ox-attr x 'w:type) "default"))
                   (rel (docx-rel (ox-attr x 'r:id)))
                   (part (and rel (not (cadr rel)) (office-resolve docx-part (car rel)))))
              (if (or (not part) (and (== kind "first") (not first?))
                      (and (== kind "even") (not even?)))
                  '()
                  (list (apply office-node
                               (cons* 'header `((place ,place) (kind ,kind))
                                      (docx-part-blocks part)))))))
          (list-filter (ox-children sect)
                       (lambda (x) (or (func? x 'w:headerReference)
                                       (func? x 'w:footerReference))))))))

(define (docx-style-node id)
  ;; the definition of the named style id, for the style of the document
  (let* ((own (or (ahash-ref docx-styles id) '()))
         (props (docx-style id))
         (char? (== (docx-get own 'family) "character"))
         (text (if char? props
                   (apply append (map cdr (list-filter props
                                                       (lambda (p) (eq? (car p) 'run)))))))
         (yes (lambda (k) (and (docx-get text k) "true")))
         (num (lambda (k) (with v (docx-get props k) (and v (office-decimal v)))))
         (font (or (docx-get text 'font) (and (not char?) (docx-get props 'text-font))))
         (size (or (docx-get text 'size) (and (not char?) (docx-get props 'text-size))))
         (align (and (not char?) (docx-get props 'align))))
    (office-node 'style
                 `((name ,id) (display ,(docx-get own 'display))
                   (family ,(if char? "text" "paragraph"))
                   ;; (a style of headings, for the look of the titles)
                   (level ,(and (not char?)
                                (with n (docx-heading-level (or (docx-get own 'name) ""))
                                  (and n (number->string n)))))
                   (em ,(yes 'em)) (strong ,(yes 'strong))
                   (underline ,(yes 'underline)) (strike ,(yes 'strike))
                   (smallcaps ,(yes 'smallcaps)) (color ,(docx-get text 'color))
                   (size ,(and size (office-decimal size)))
                   (font ,(cond ((not font) #f) ((office-monospace? font) "mono")
                                ((office-sans? font) "sans") (else #f)))
                   (font-name ,(and font (not (office-monospace? font)) font))
                   (caps ,(yes 'caps)) (dstrike ,(yes 'dstrike))
                   (underline-kind ,(and (docx-get text 'underline)
                                         (docx-get text 'underline-kind)))
                   (letters ,(with v (docx-get text 'letters) (and v (office-decimal v))))
                   (align ,(cond ((not align) #f)
                                 ((in? align '("both" "distribute")) "justify")
                                 ((== align "center") "center")
                                 ((in? align '("right" "end")) "right")
                                 (else "left")))
                   (indent-left ,(and (not char?) (num 'indent-left)))
                   (first-indent ,(and (not char?) (num 'first-indent)))
                   (space-before ,(and (not char?) (num 'space-before)))
                   (space-after ,(and (not char?) (num 'space-after)))
                   (background ,(and (not char?) (docx-get props 'background)))
                   (border ,(and (not char?) (docx-get props 'border) "true"))))))

(define (docx-styles-block)
  ;; the named styles of the text, but the one of the usual paragraphs
  (let* ((body (docx-body-style))
         (ids (list-filter (map car (ahash-table->list docx-used-styles))
                           (lambda (id) (!= id body))))
         (ids (sort ids string<?)))
    (if (null? ids) '() `((styles ,@(map docx-style-node ids))))))

(define (docx-main-part)
  ;; the entry of the text: the one which _rels/.rels names, or the usual
  (let* ((root (office-xml docx-archive "_rels/.rels"))
         (rel (and root
                   (list-find (ox-childs root 'Relationship)
                              (lambda (x)
                                (string-ends? (or (ox-attr x 'Type) "")
                                              "/officeDocument")))))
         (target (and rel (ox-attr rel 'Target))))
    (if target (office-resolve "" target) "word/document.xml")))

;; A file which cannot be read gives a document which says so, and a
;; message, rather than an empty document.
(define (docx-error message)
  (debug-message "convert-error" (string-append message "\n"))
  `(office (error ,message)))

(tm-define (parse-docx-document s)
  (:type (-> string stree))
  (:synopsis "Read the Word document @s, a zip archive, into an office tree")
  ;; (what was not foreseen in a file gives a message too, and the state
  ;; of the module is reset)
  (with r (catch #t
            (lambda () (docx-parse s))
            (lambda args
              (docx-error (string-append "This Word document could not be read "
                                         (office-error-text args)))))
    (set! docx-archive #f)
    (set! docx-part "word/document.xml")
    (set! docx-rels (make-ahash-table))
    (set! docx-lists (make-ahash-table))
    (set! docx-styles (make-ahash-table))
    (set! docx-style-memo (make-ahash-table))
    (set! docx-notes (make-ahash-table))
    (set! docx-open-notes '())
    (set! docx-fields '())
    (set! docx-list-formats (make-ahash-table))
    (set! docx-defaults '())
    (set! docx-default-style #f)
    (set! docx-body-styles (make-ahash-table))
    (set! docx-floats '())
    (set! docx-theme-fonts '())
    (set! docx-used-styles (make-ahash-table))
    (set! docx-comments (make-ahash-table))
    r))

(define (docx-parse s)
  (set! docx-archive (office-archive s))
  (if (not docx-archive) (docx-error "This file is not a Word document")
      (begin
        (set! docx-part (docx-main-part))
        (set! docx-rels (make-ahash-table))
        (set! docx-notes (make-ahash-table))
        (set! docx-open-notes '())
        (set! docx-fields '())
        (set! docx-list-formats (make-ahash-table))
        (set! docx-restarts (make-ahash-table))
        (set! docx-defaults '())
        (set! docx-default-style #f)
        (set! docx-body-styles (make-ahash-table))
        (set! docx-floats '())
        (docx-read-theme)
        (docx-read-styles)
        (when (and (not docx-default-style) (ahash-ref docx-styles "Normal"))
          (set! docx-default-style "Normal"))
        (docx-read-lists)
        (for (part (list docx-part "word/footnotes.xml" "word/endnotes.xml"))
          (docx-read-rels part))
        (set! docx-used-styles (make-ahash-table))
        (docx-read-comments)
        (docx-read-notes "word/footnotes.xml" 'w:footnote 'foot)
        (docx-read-notes "word/endnotes.xml" 'w:endnote 'end)
        (let* ((root (office-xml docx-archive docx-part))
               (body (and root (ox-child root 'w:body)))
               (sect (and body (list-find (reverse (ox-children body))
                                          (lambda (x) (func? x 'w:sectPr)))))
               (blocks (and body (docx-sections (docx-blocks (ox-children body))
                                                (if sect (docx-columns sect) 1))))
               (headers (docx-headers sect))
               (blocks (and blocks (office-drop-style blocks (docx-body-style))))
               (r (if body
                      `(office ,@(docx-meta) ,@(docx-setup body) ,@(docx-styles-block)
                               ,@headers
                               (package (@ (format "docx") (data ,s)))
                               ,@blocks)
                      (docx-error "This Word document has no text"))))
          r))))
