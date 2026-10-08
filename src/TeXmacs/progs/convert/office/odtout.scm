
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : odtout.scm
;; DESCRIPTION : writing office trees as OpenDocument texts (.odt)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The office tree (office-tools.scm) is written as the files of an
;; OpenDocument text, in a zip archive: the text (content.xml), the styles
;; which its paragraphs use by their names (styles.xml), so that the
;; document can be given another look, the images (Pictures) and the
;; formulas, each a file of MathML in a directory of its own. The
;; formatting by hand is a style which is made for it ("automatic" styles,
;; in content.xml): the same formatting is the same style.

(texmacs-module (convert office odtout)
  (:use (convert office office-tools)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State: the document which is written
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ot-styles (make-ahash-table)) ; key of a formatting -> name of its style
(define ot-style-list '())            ; the automatic styles, reversed
(define ot-files '())                 ; (name type data) of the files, reversed
(define ot-id 0)                      ; the last number given to a thing
(define ot-cell-align #f)             ; the alignment of the cell around

(define ot-text-width 16.0)           ; the width of the text, in centimeters

(define (ot-next-id)
  (set! ot-id (+ ot-id 1))
  ot-id)

(define (ot-style key prefix make)
  ;; the name of the automatic style of this key; (make name) is its
  ;; element when it is new
  (or (ahash-ref ot-styles key)
      (with name (string-append prefix (number->string (+ (length ot-style-list) 1)))
        (ahash-set! ot-styles key name)
        (set! ot-style-list (cons (make name) ot-style-list))
        name)))

(define (ot-cm x)
  (string-append (office-decimal x) "cm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ot-text s)
  ;; The nodes of a piece of text. The spaces of the file do not all
  ;; count: those at the start, and those which follow another one, are
  ;; written as elements text:s.
  (let loop ((l (string->list s)) (run '()) (acc '()) (start? #t))
    (let ((flush (lambda ()
                   (if (null? run) acc (cons (list->string (reverse run)) acc)))))
      (cond ((null? l) (reverse (flush)))
            ((char=? (car l) #\space)
             (let count ((r l) (n 0))
               (cond ((and (pair? r) (char=? (car r) #\space))
                      (count (cdr r) (+ n 1)))
                     (start?
                      (loop r '()
                            (cons `(text:s (@ (text:c ,(number->string n)))) (flush))
                            #f))
                     (else
                       ;; the first space as it is, after the text before it
                       (with acc2 (cons (list->string (reverse (cons #\space run))) acc)
                         (loop r '()
                               (if (> n 1)
                                   (cons `(text:s (@ (text:c ,(number->string (- n 1)))))
                                         acc2)
                                   acc2)
                               #f))))))
            ((char=? (car l) #\tab)
             (loop (cdr l) '() (cons '(text:tab) (flush)) #f))
            ((char=? (car l) #\newline)
             (loop (cdr l) '() (cons '(text:line-break) (flush)) #t))
            (else (loop (cdr l) (cons (car l) run) acc #f))))))

(define (ot-text-style props)
  ;; the name of the style of the text inside the wrappers props, or #f
  (let* ((tags (list-filter '(code strong em smallcaps strike underline sub sup mark)
                            (lambda (t) (in? t props))))
         (color (with c (list-find props pair?) (and c (cadr c))))
         (key (cons 'text (cons color tags))))
    (and (or (pair? tags) color)
         (ot-style
           key "T"
           (lambda (name)
             `(style:style
                (@ (style:name ,name) (style:family "text"))
                (style:text-properties
                  (@ ,@(if (in? 'em tags)
                           '((fo:font-style "italic") (style:font-style-asian "italic")
                             (style:font-style-complex "italic"))
                           '())
                     ,@(if (in? 'strong tags)
                           '((fo:font-weight "bold") (style:font-weight-asian "bold")
                             (style:font-weight-complex "bold"))
                           '())
                     ,@(if (in? 'underline tags)
                           '((style:text-underline-style "solid")
                             (style:text-underline-width "auto")
                             (style:text-underline-color "font-color"))
                           '())
                     ,@(if (in? 'strike tags)
                           '((style:text-line-through-style "solid")
                             (style:text-line-through-type "single"))
                           '())
                     ,@(cond ((in? 'sup tags) '((style:text-position "super 58%")))
                             ((in? 'sub tags) '((style:text-position "sub 58%")))
                             (else '()))
                     ,@(if (in? 'smallcaps tags) '((fo:font-variant "small-caps")) '())
                     ,@(if (in? 'code tags)
                           '((style:font-name "Courier New")
                             (fo:font-family "'Courier New'")
                             (style:font-family-generic "modern")
                             (style:font-pitch "fixed"))
                           '())
                     ,@(if (in? 'mark tags) '((fo:background-color "#ffff00")) '())
                     ,@(if color `((fo:color ,color)) '())))))))))

(define (ot-span props l)
  ;; the nodes l as the text inside the wrappers props
  (with style (ot-text-style props)
    (cond ((null? l) '())
          (style (list `(text:span (@ (text:style-name ,style)) ,@l)))
          (else l))))

(define (ot-image x)
  (let* ((data (ox-attr x 'data))
         (name (or (ox-attr x 'name) "image.png"))
         (n (ot-next-id))
         (dot (let loop ((i (- (string-length name) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref name i) #\.) i)
                      (else (loop (- i 1))))))
         (suffix (if dot (locase-all (substring name (+ dot 1) (string-length name)))
                     "png"))
         (file (string-append "Pictures/image" (number->string n) "." suffix))
         (type (cond ((in? suffix '("jpg" "jpeg")) "image/jpeg")
                     ((== suffix "gif") "image/gif")
                     (else "image/png")))
         (svg (ox-attr x 'svg))
         (svg-file (string-append "Pictures/image" (number->string n) ".svg"))
         (w (or (office-length->cm (ox-attr x 'width)) 5.0))
         (h (or (office-length->cm (ox-attr x 'height)) 3.0)))
    (if (not data) '()
        (begin
          (set! ot-files (cons (list file type data) ot-files))
          (when svg
            (set! ot-files (cons (list svg-file "image/svg+xml" svg) ot-files)))
          (list `(draw:frame
                   (@ (draw:style-name "fr1")
                      (draw:name ,(string-append "Image" (number->string n)))
                      (text:anchor-type "as-char")
                      (svg:width ,(ot-cm w)) (svg:height ,(ot-cm h))
                      (draw:z-index "0"))
                   ;; the images of a frame are the same picture: the
                   ;; first one which a program can show is shown
                   ,@(if svg
                         `((draw:image (@ (xlink:href ,svg-file) (xlink:type "simple")
                                          (xlink:show "embed") (xlink:actuate "onLoad"))))
                         '())
                   (draw:image (@ (xlink:href ,file) (xlink:type "simple")
                                  (xlink:show "embed") (xlink:actuate "onLoad")))
                   ,@(if (ox-attr x 'alt) `((svg:title ,(ox-attr x 'alt))) '())))))))

(define (ot-local-name tag)
  (let* ((s (symbol->string tag))
         (i (string-search-forwards ":" 0 s)))
    (string->symbol (if (>= i 0) (substring s (+ i 1) (string-length s)) s))))

(define (ot-plain-mathml x)
  ;; a tree of MathML without the prefixes of its elements
  (cond ((not (pair? x)) x)
        ((func? x '@) x)
        ;; the brackets grow with what they enclose
        ((and (== (ot-local-name (car x)) 'mo)
              (in? (ox-attr x 'form) '("prefix" "postfix"))
              (not (ox-attr x 'stretchy)))
         `(mo (@ ,@(ox-attrs x) (stretchy "true")) ,@(ox-children x)))
        (else (cons (ot-local-name (car x)) (map ot-plain-mathml (cdr x))))))

(define (ot-math x)
  ;; a formula: an object, whose file of MathML is in a directory
  (let* ((form (ox-attr x 'form))
         (c (ox-children x))
         (display? (== (ox-attr x 'display) "true"))
         (text (cond ((null? c) #f)
                     ((== form "text")
                      (ox-serialize `(math (@ (xmlns "http://www.w3.org/1998/Math/MathML"))
                                           (mtext ,(car c)))))
                     ((== form "sxml")
                      (with m (ot-plain-mathml (car c))
                        (ox-serialize
                          `(math (@ (xmlns "http://www.w3.org/1998/Math/MathML")
                                    (display ,(if display? "block" "inline")))
                                 ,@(ox-children m)))))
                     (else (car c))))
         (n (ot-next-id))
         (dir (string-append "Formula-" (number->string n))))
    (if (not text) '()
        (begin
          (set! ot-files
                (cons* (list (string-append dir "/content.xml") "text/xml" text)
                       (list (string-append dir "/")
                             "application/vnd.oasis.opendocument.formula" #f)
                       ot-files))
          (list `(draw:frame
                   (@ (draw:style-name "fr2")
                      (draw:name ,(string-append "Formula" (number->string n)))
                      (text:anchor-type "as-char") (draw:z-index "0"))
                   (draw:object (@ (xlink:href ,(string-append "./" dir))
                                   (xlink:type "simple") (xlink:show "embed")
                                   (xlink:actuate "onLoad")))))))))

(define (ot-note x)
  (let* ((n (ot-next-id))
         (old ot-cell-align))
    (set! ot-cell-align #f)
    (with blocks (ot-blocks-with (ox-children x) "Footnote")
      (set! ot-cell-align old)
      (list `(text:note
               (@ (text:id ,(string-append "ftn" (number->string n)))
                  (text:note-class "footnote"))
               (text:note-citation ,(number->string n))
               (text:note-body ,@(if (null? blocks)
                                     '((text:p (@ (text:style-name "Footnote"))))
                                     blocks)))))))

(define (ot-inline x props)
  ;; the nodes of a paragraph for an inline node inside the wrappers props
  (cond ((string? x) (if (== x "") '() (ot-span props (ot-text x))))
        ((not (pair? x)) '())
        (else
          (let ((l (ox-children x)))
            (case (car x)
              ((em strong underline strike sub sup code smallcaps mark)
               (ot-inlines l (cons (car x) props)))
              ((color)
               (ot-inlines l (if (office-color (ox-attr x 'value))
                                 (cons (list 'color (office-color (ox-attr x 'value)))
                                       props)
                                 props)))
              ((link ref)
               (let ((href (if (func? x 'ref)
                               (string-append "#" (or (ox-attr x 'name) ""))
                               (or (ox-attr x 'href) "")))
                     (inner (ot-inlines l props)))
                 (cond ((null? inner) '())
                       ((in? href '("" "#")) inner)
                       (else (list `(text:a (@ (xlink:type "simple") (xlink:href ,href)
                                               (text:style-name "Internet_20_link")
                                               (text:visited-style-name
                                                 "Visited_20_Internet_20_Link"))
                                            ,@inner))))))
              ((bookmark)
               (list `(text:bookmark (@ (text:name ,(or (ox-attr x 'name) ""))))))
              ((note) (ot-note x))
              ((br) '((text:line-break)))
              ((tab) '((text:tab)))
              ((pagebreak) '())
              ((image) (ot-image x))
              ((math) (ot-math x))
              (else (ot-inlines l props)))))))

(define (ot-inlines l props)
  (append-map (lambda (x) (ot-inline x props)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraphs and lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the styles of the paragraphs, by their roles
(define ot-role-styles
  '(("title" . "Title") ("subtitle" . "Subtitle") ("author" . "Author")
    ("date" . "Date") ("abstract" . "Abstract") ("quote" . "Quotations")
    ("code" . "Preformatted_20_Text") ("caption" . "Caption")
    ("figure" . "Figure") ("term" . "Definition_20_Term")
    ("definition" . "Definition_20_Definition")))

(define (ot-paragraph-style base align break? rule?)
  ;; the name of the style of a paragraph: the style base, or one which
  ;; is made of it with an alignment, a page break before, a line under
  (if (not (or align break? rule?)) base
      (ot-style
        (list 'paragraph base align break? rule?) "P"
        (lambda (name)
          `(style:style
             (@ (style:name ,name) (style:family "paragraph")
                (style:parent-style-name ,base))
             (style:paragraph-properties
               (@ ,@(cond ((== align "center") '((fo:text-align "center")))
                          ((== align "right") '((fo:text-align "end")))
                          ((== align "left") '((fo:text-align "start")))
                          (else '()))
                  ,@(if break? '((fo:break-before "page")) '())
                  ,@(if rule?
                        '((fo:border-bottom "0.5pt solid #000000")
                          (fo:padding "0.05cm"))
                        '()))))))))

(define (ot-paragraph x default)
  (let* ((role (ox-attr x 'role))
         (level (or (and (== role "heading") (string->number (or (ox-attr x 'level) "1")))
                    #f))
         (base (cond (level (string-append "Heading_20_" (number->string level)))
                     ((assoc-ref ot-role-styles role) => identity)
                     (else default)))
         ;; a formula on its own lines is centered
         (formula? (with c (ox-children x)
                     (and (pair? c) (func? (car c) 'math)
                          (== (ox-attr (car c) 'display) "true"))))
         (style (ot-paragraph-style
                  base
                  (or (ox-attr x 'align) ot-cell-align (and formula? "center"))
                  #f #f))
         (l (ot-inlines (ox-children x) '())))
    (if level
        (list `(text:h (@ (text:style-name ,style)
                          (text:outline-level ,(number->string level)))
                       ,@l))
        (list `(text:p (@ (text:style-name ,style)) ,@l)))))

(define (ot-list-kinds x)
  ;; the kinds of the levels of a list: its own, and those of the first
  ;; list at each level inside it
  (cons (if (== (ox-attr x 'kind) "number") "n" "b")
        (let loop ((items (ox-children x)))
          (cond ((null? items) '())
                ((list-find (ox-children (car items)) (lambda (y) (func? y 'list)))
                 => ot-list-kinds)
                (else (loop (cdr items)))))))

(define (ot-list-style kinds)
  ;; the name of the style of a list whose levels have these kinds
  (let* ((kinds (append kinds
                        (map (lambda (i) (cAr kinds)) (iota (max 0 (- 9 (length kinds)))))))
         (kinds (sublist kinds 0 9)))
    (ot-style
      (cons 'list kinds) "L"
      (lambda (name)
        `(text:list-style
           (@ (style:name ,name))
           ,@(map (lambda (kind i)
                    (let ((props `(style:list-level-properties
                                    (@ (text:list-level-position-and-space-mode
                                         "label-alignment"))
                                    (style:list-level-label-alignment
                                      (@ (text:label-followed-by "listtab")
                                         (fo:text-indent "-0.635cm")
                                         (fo:margin-left ,(ot-cm (* 1.27 (+ i 1)))))))))
                      (if (== kind "n")
                          `(text:list-level-style-number
                             (@ (text:level ,(number->string (+ i 1)))
                                (style:num-suffix ".") (style:num-format "1"))
                             ,props)
                          `(text:list-level-style-bullet
                             (@ (text:level ,(number->string (+ i 1)))
                                (text:bullet-char
                                  ,(office-utf8 (list-ref '(#x2022 #x25e6 #x25aa)
                                                          (modulo i 3)))))
                             ,props))))
                  kinds (iota 9)))))))

(define (ot-list x top?)
  ;; a list; the style of the outer list is the one of those inside it
  (with items (list-filter (ox-children x) (lambda (y) (func? y 'item)))
    (if (null? items) '()
        (list `(text:list
                 ,@(if top?
                       `((@ (text:style-name ,(ot-list-style (ot-list-kinds x)))))
                       '())
                 ,@(map (lambda (item)
                          `(text:list-item
                             ,@(append-map
                                 (lambda (b)
                                   (if (func? b 'list) (ot-list b #f)
                                       (ot-block b "Text_20_body")))
                                 (ox-children item))))
                        items))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ot-text-length x)
  ;; a rough measure of the width of the text of a node: the longest of
  ;; its paragraphs, in characters
  (cond ((string? x) (string-length x))
        ((not (pair? x)) 0)
        ((in? (car x) '(image math)) 6)
        ((in? (car x) '(cell item list table row note))
         (apply max (cons 0 (map ot-text-length (ox-children x)))))
        (else (apply + (map ot-text-length (ox-children x))))))

(define (ot-cell-style c)
  ;; the name of the style of a cell with its borders and its background
  (let* ((letters (or (ox-attr c 'borders) "none"))
         (fill (office-color (ox-attr c 'background)))
         (side (lambda (letter)
                 (if (>= (string-search-forwards letter 0 letters) 0)
                     "0.5pt solid #000000" "none"))))
    (ot-style
      (list 'cell letters fill) "C"
      (lambda (name)
        `(style:style
           (@ (style:name ,name) (style:family "table-cell"))
           (style:table-cell-properties
             (@ (fo:padding "0.1cm")
                (fo:border-top ,(side "t")) (fo:border-bottom ,(side "b"))
                (fo:border-left ,(side "l")) (fo:border-right ,(side "r"))
                ,@(if fill `((fo:background-color ,fill)) '()))))))))

(define (ot-cell c)
  (if (ox-attr c 'covered) '(table:covered-table-cell)
      (let* ((old ot-cell-align)
             (span (lambda (name attr)
                     (with v (ox-attr c name)
                       (if (and v (string->number v) (> (string->number v) 1))
                           `((,attr ,v)) '())))))
        (set! ot-cell-align (ox-attr c 'align))
        (with blocks (ot-blocks-with (ox-children c) "Table_20_Contents")
          (set! ot-cell-align old)
          `(table:table-cell
             (@ (table:style-name ,(ot-cell-style c)) (office:value-type "string")
                ,@(span 'colspan 'table:number-columns-spanned)
                ,@(span 'rowspan 'table:number-rows-spanned))
             ,@(if (null? blocks)
                   '((text:p (@ (text:style-name "Table_20_Contents"))))
                   blocks))))))

(define (ot-table x)
  (let* ((rows (list-filter (ox-children x) (lambda (r) (func? r 'row))))
         (ncols (apply max (cons 1 (map (lambda (r) (length (ox-children r))) rows))))
         (n (ot-next-id))
         (name (string-append "Table" (number->string n)))
         (part (with w (ox-attr x 'width)
                 (and w (string-ends? w "par")
                      (string->number (substring w 0 (- (string-length w) 3))))))
         (parts (with c (ox-attr x 'columns)
                  (and c (map string->number (string-tokenize-by-char c #\space)))))
         (parts (and parts (== (length parts) ncols) (list-and parts) parts))
         ;; the widths of the columns, in centimeters: see docxout.scm
         (natural (map (lambda (j)
                         (+ 0.5
                            (* 0.21
                               (apply max
                                      (cons 1
                                            (map (lambda (r)
                                                   (with cells (ox-children r)
                                                     (if (< j (length cells))
                                                         (ot-text-length (list-ref cells j))
                                                         0)))
                                                 rows))))))
                       (iota ncols)))
         (total (* ot-text-width (or part 1.0)))
         (scale (min 1.0 (/ total (max 0.1 (apply + natural)))))
         (widths (cond (parts (map (lambda (p) (* p total)) parts))
                       (part (map (lambda (j) (/ total ncols)) (iota ncols)))
                       (else (map (lambda (w) (* w scale)) natural))))
         (align (ox-attr x 'align))
         (pad (lambda (cells)
                (append cells (map (lambda (k) '(cell (@ (borders "none"))))
                                   (iota (- ncols (length cells)))))))
         (header? (lambda (r)
                    (with cells (ox-children r)
                      (and (pair? cells)
                           (list-and (map (lambda (c) (or (ox-attr c 'covered)
                                                          (== (ox-attr c 'header) "true")))
                                          cells))))))
         (row (lambda (r)
                `(table:table-row ,@(map ot-cell (pad (ox-children r))))))
         (head (let loop ((l rows) (acc '()))
                 (if (and (pair? l) (header? (car l)))
                     (loop (cdr l) (cons (car l) acc))
                     (reverse acc))))
         (body (list-tail rows (length head))))
    (set! ot-style-list
          (cons `(style:style
                   (@ (style:name ,name) (style:family "table"))
                   (style:table-properties
                     (@ (style:width ,(ot-cm (apply + widths)))
                        (table:align ,(cond ((== align "center") "center")
                                            ((== align "right") "right")
                                            (else "left"))))))
                ot-style-list))
    (for-each
      (lambda (w j)
        (set! ot-style-list
              (cons `(style:style
                       (@ (style:name ,(string-append name "." (number->string j)))
                          (style:family "table-column"))
                       (style:table-column-properties
                         (@ (style:column-width ,(ot-cm w)))))
                    ot-style-list)))
      widths (iota ncols))
    `(table:table
       (@ (table:name ,name) (table:style-name ,name))
       ,@(map (lambda (j)
                `(table:table-column
                   (@ (table:style-name ,(string-append name "." (number->string j))))))
              (iota ncols))
       ,@(if (or (null? head) (null? body)) '()
             `((table:table-header-rows ,@(map row head))))
       ,@(map row (if (or (null? head) (null? body)) rows body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ot-block x default)
  ;; the elements of a block; default is the style of a plain paragraph
  (cond ((not (pair? x)) '())
        (else
          (case (car x)
            ((p) (ot-paragraph x default))
            ((list) (ot-list x #t))
            ((table) (list (ot-table x)))
            ((pagebreak)
             (list `(text:p (@ (text:style-name
                                 ,(ot-paragraph-style "Text_20_body" #f #t #f))))))
            ((rule)
             (list `(text:p (@ (text:style-name
                                 ,(ot-paragraph-style "Text_20_body" #f #f #t))))))
            ((toc)
             (list `(text:table-of-content
                      (@ (text:name "Table of Contents1"))
                      (text:table-of-content-source (@ (text:outline-level "3")))
                      (text:index-body
                        (text:p (@ (text:style-name "Text_20_body"))
                                "Update this index to see the table of contents.")))))
            (else '())))))

(define (ot-blocks-with l default)
  (append-map (lambda (x) (ot-block x default)) l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The files of the archive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ot-namespaces
  '((xmlns:office "urn:oasis:names:tc:opendocument:xmlns:office:1.0")
    (xmlns:style "urn:oasis:names:tc:opendocument:xmlns:style:1.0")
    (xmlns:text "urn:oasis:names:tc:opendocument:xmlns:text:1.0")
    (xmlns:table "urn:oasis:names:tc:opendocument:xmlns:table:1.0")
    (xmlns:draw "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0")
    (xmlns:fo "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0")
    (xmlns:xlink "http://www.w3.org/1999/xlink")
    (xmlns:svg "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0")
    (xmlns:dc "http://purl.org/dc/elements/1.1/")
    (office:version "1.3")))

(define ot-fonts
  '(office:font-face-decls
     (style:font-face (@ (style:name "Courier New") (svg:font-family "'Courier New'")
                         (style:font-family-generic "modern")
                         (style:font-pitch "fixed")))))

(define (ot-paragraph-style-element name display parent paragraph text . outline)
  ;; a style of paragraphs with these properties, lists of attributes
  `(style:style
     (@ (style:name ,name) (style:family "paragraph")
        ,@(if display `((style:display-name ,display)) '())
        ,@(if parent `((style:parent-style-name ,parent)) '())
        ,@(if (null? outline) '()
              `((style:default-outline-level ,(car outline))))
        (style:class "text"))
     ,@(if (null? paragraph) '() `((style:paragraph-properties (@ ,@paragraph))))
     ,@(if (null? text) '() `((style:text-properties (@ ,@text))))))

(define (ot-heading-style n)
  (let ((s (number->string n))
        (size (list-ref '("18pt" "15pt" "13pt" "12pt" "11pt" "11pt" "11pt" "11pt" "11pt")
                        (- n 1))))
    (ot-paragraph-style-element
      (string-append "Heading_20_" s) (string-append "Heading " s) "Heading"
      `((fo:margin-top ,(if (<= n 2) "0.6cm" "0.4cm")) (fo:margin-bottom "0.2cm"))
      `((fo:font-size ,size) (fo:font-weight "bold")
        ,@(if (>= n 4) '((fo:font-style "italic")) '()))
      s)))

(define (ot-styles-file)
  `(office:document-styles
     (@ ,@ot-namespaces)
     ,ot-fonts
     (office:styles
       (style:default-style
         (@ (style:family "paragraph"))
         (style:text-properties (@ (fo:font-size "11pt"))))
       ,(ot-paragraph-style-element "Standard" #f #f '() '())
       ,(ot-paragraph-style-element
          "Text_20_body" "Text body" "Standard"
          '((fo:margin-top "0cm") (fo:margin-bottom "0.2cm")) '())
       ,(ot-paragraph-style-element
          "Heading" #f "Standard"
          '((fo:keep-with-next "always")) '())
       ,@(map ot-heading-style (map (lambda (i) (+ i 1)) (iota 9)))
       ,(ot-paragraph-style-element
          "Title" #f "Heading"
          '((fo:text-align "center") (fo:margin-top "0.8cm") (fo:margin-bottom "0.4cm"))
          '((fo:font-size "22pt") (fo:font-weight "bold")))
       ,(ot-paragraph-style-element
          "Subtitle" #f "Heading" '((fo:text-align "center")) '((fo:font-size "15pt")))
       ,(ot-paragraph-style-element
          "Author" #f "Text_20_body" '((fo:text-align "center")) '((fo:font-size "12pt")))
       ,(ot-paragraph-style-element
          "Date" #f "Text_20_body" '((fo:text-align "center")) '())
       ,(ot-paragraph-style-element
          "Abstract" #f "Text_20_body"
          '((fo:margin-left "1.27cm") (fo:margin-right "1.27cm")
            (fo:margin-top "0.4cm") (fo:margin-bottom "0.4cm"))
          '((fo:font-size "10pt")))
       ,(ot-paragraph-style-element
          "Quotations" #f "Text_20_body"
          '((fo:margin-left "1.27cm") (fo:margin-right "1.27cm")) '())
       ,(ot-paragraph-style-element
          "Preformatted_20_Text" "Preformatted Text" "Standard"
          '((fo:margin-bottom "0.2cm"))
          '((style:font-name "Courier New") (fo:font-family "'Courier New'")
            (style:font-family-generic "modern") (style:font-pitch "fixed")
            (fo:font-size "10pt")))
       ,(ot-paragraph-style-element
          "Caption" #f "Text_20_body" '((fo:text-align "center"))
          '((fo:font-size "10pt")))
       ,(ot-paragraph-style-element
          "Figure" #f "Text_20_body"
          '((fo:text-align "center") (fo:keep-with-next "always")) '())
       ,(ot-paragraph-style-element
          "Definition_20_Term" "Definition Term" "Text_20_body"
          '((fo:margin-bottom "0cm") (fo:keep-with-next "always"))
          '((fo:font-weight "bold")))
       ,(ot-paragraph-style-element
          "Definition_20_Definition" "Definition Definition" "Text_20_body"
          '((fo:margin-left "1.27cm")) '())
       ,(ot-paragraph-style-element
          "Table_20_Contents" "Table Contents" "Standard" '() '())
       ,(ot-paragraph-style-element
          "Footnote" #f "Standard" '() '((fo:font-size "10pt")))
       (style:style
         (@ (style:name "Internet_20_link") (style:display-name "Internet link")
            (style:family "text"))
         (style:text-properties
           (@ (fo:color "#0563c1") (style:text-underline-style "solid")
              (style:text-underline-width "auto")
              (style:text-underline-color "font-color"))))
       (style:style
         (@ (style:name "Visited_20_Internet_20_Link")
            (style:display-name "Visited Internet Link") (style:family "text"))
         (style:text-properties
           (@ (fo:color "#954f72") (style:text-underline-style "solid")
              (style:text-underline-width "auto")
              (style:text-underline-color "font-color"))))
       (style:style (@ (style:name "Graphics") (style:family "graphic")))
       (style:style (@ (style:name "Formula") (style:family "graphic"))))))

(define (ot-content blocks)
  `(office:document-content
     (@ ,@ot-namespaces)
     ,ot-fonts
     (office:automatic-styles
       (style:style
         (@ (style:name "fr1") (style:family "graphic")
            (style:parent-style-name "Graphics"))
         (style:graphic-properties
           (@ (style:vertical-pos "top") (style:vertical-rel "baseline")
              (fo:border "none") (fo:padding "0cm"))))
       (style:style
         (@ (style:name "fr2") (style:family "graphic")
            (style:parent-style-name "Formula"))
         (style:graphic-properties
           (@ (style:vertical-pos "middle") (style:vertical-rel "text")
              (fo:border "none") (fo:padding "0cm"))))
       ,@(reverse ot-style-list))
     (office:body (office:text ,@blocks))))

(define (ot-manifest files)
  `(manifest:manifest
     (@ (xmlns:manifest "urn:oasis:names:tc:opendocument:xmlns:manifest:1.0")
        (manifest:version "1.3"))
     (manifest:file-entry
       (@ (manifest:full-path "/") (manifest:version "1.3")
          (manifest:media-type "application/vnd.oasis.opendocument.text")))
     (manifest:file-entry
       (@ (manifest:full-path "content.xml") (manifest:media-type "text/xml")))
     (manifest:file-entry
       (@ (manifest:full-path "styles.xml") (manifest:media-type "text/xml")))
     ,@(map (lambda (f)
              `(manifest:file-entry
                 (@ (manifest:full-path ,(car f)) (manifest:media-type ,(cadr f)))))
            files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (serialize-odt-document x)
  (:type (-> stree string))
  (:synopsis "Write the office tree @x as an OpenDocument text, a zip archive")
  (set! ot-styles (make-ahash-table))
  (set! ot-style-list '())
  (set! ot-files '())
  (set! ot-id 0)
  (set! ot-cell-align #f)
  (let* ((blocks (ot-blocks-with (if (func? x 'office) (cdr x) (list x))
                                 "Text_20_body"))
         (blocks (if (null? blocks)
                     '((text:p (@ (text:style-name "Text_20_body"))))
                     blocks))
         (files (reverse ot-files))
         ;; (a directory is in the manifest, not in the archive)
         (entries (list-filter files caddr))
         ;; the type of the file comes first, as the format wants
         (r (zip-pack
              (append '("mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml")
                      (map car entries))
              (append (list "application/vnd.oasis.opendocument.text"
                            (ox-serialize (ot-content blocks))
                            (ox-serialize (ot-styles-file))
                            (ox-serialize (ot-manifest files)))
                      (map caddr entries)))))
    (set! ot-styles (make-ahash-table))
    (set! ot-style-list '())
    (set! ot-files '())
    r))
