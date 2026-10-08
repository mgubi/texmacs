
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : odtin.scm
;; DESCRIPTION : reading OpenDocument texts (.odt) into office trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An .odt is a zip archive: content.xml is the text, headings (text:h),
;; paragraphs (text:p) with spans (text:span), lists (text:list) and tables
;; (table:table); styles.xml has the styles, and content.xml those which
;; the program made for the formatting by hand ("automatic" styles: a
;; span in italics is a span with a style T1 which says italics). A
;; formula is an object, a directory of the archive whose content.xml is
;; MathML; an image is a file of the archive.
;; See office-tools.scm for the tree which is made of all this.

(texmacs-module (convert office odtin)
  (:use (convert office office-tools)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State: the document which is read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define odt-archive #f)                  ; the entries of the archive
(define odt-styles (make-ahash-table))   ; (family . name) -> properties
(define odt-style-memo (make-ahash-table))
(define odt-lists (make-ahash-table))    ; (list style . level) -> kind
(define odt-list-style #f)               ; the style of the list which is read
(define odt-list-depth 0)

(define (odt-get props key)
  (with p (assoc key props) (and p (cdr p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The properties are association lists, in which the first value of a key
;; counts: those of a style come before those of its parent.

(define (odt-text-properties x)
  ;; the properties of an element style:text-properties
  (if (not x) '()
      (append
        (with v (ox-attr x 'fo:font-style)
          (if v (list (cons 'em (in? v '("italic" "oblique")))) '()))
        (with v (ox-attr x 'fo:font-weight)
          (if v (list (cons 'strong
                            (or (== v "bold")
                                (and (string->number v)
                                     (>= (string->number v) 600)))))
              '()))
        (with v (ox-attr x 'style:text-underline-style)
          (if v (list (cons 'underline (!= v "none"))) '()))
        (with v (or (ox-attr x 'style:text-line-through-style)
                    (ox-attr x 'style:text-line-through-type))
          (if v (list (cons 'strike (!= v "none"))) '()))
        (with v (ox-attr x 'style:text-position)
          (cond ((not v) '())
                ((string-starts? v "super") '((sup . #t) (sub . #f)))
                ((string-starts? v "sub") '((sub . #t) (sup . #f)))
                ;; a shift in percents: up or down
                ((and (string->number (car (string-tokenize-by-char v #\%)))
                      (> (string->number (car (string-tokenize-by-char v #\%))) 0))
                 '((sup . #t) (sub . #f)))
                ((and (string->number (car (string-tokenize-by-char v #\%)))
                      (< (string->number (car (string-tokenize-by-char v #\%))) 0))
                 '((sub . #t) (sup . #f)))
                (else '((sub . #f) (sup . #f)))))
        (with v (ox-attr x 'fo:font-variant)
          (if v (list (cons 'smallcaps (== v "small-caps"))) '()))
        (with v (or (ox-attr x 'style:font-name) (ox-attr x 'fo:font-family))
          (if v (list (cons 'code (office-monospace? v))) '())))))

(define (odt-paragraph-properties x)
  ;; the properties of an element style:paragraph-properties
  (if (not x) '()
      (append
        (with v (ox-attr x 'fo:text-align)
          (if v (list (cons 'align v)) '()))
        (with v (ox-attr x 'fo:break-before)
          (if (== v "page") '((page-break . #t)) '())))))

(define (odt-border-on? x names)
  ;; the first of the attributes names of x which is there: is it a border?
  (cond ((null? names) '())
        ((ox-attr x (car names))
         => (lambda (v) (list (not (string-starts? v "none")))))
        (else (odt-border-on? x (cdr names)))))

(define (odt-cell-properties x)
  ;; the properties of an element style:table-cell-properties
  (if (not x) '()
      (append
        (append-map
          (lambda (side)
            (with on (odt-border-on? x (list (cadr side) 'fo:border))
              (if (null? on) '() (list (cons (car side) (car on))))))
          '((border-top fo:border-top) (border-bottom fo:border-bottom)
            (border-left fo:border-left) (border-right fo:border-right)))
        (with v (ox-attr x 'fo:background-color)
          (if (and v (string-starts? v "#") (!= (locase-all v) "#ffffff"))
              (list (cons 'fill (locase-all v)))
              '())))))

(define (odt-table-properties x)
  ;; the properties of an element style:table-properties
  (if (not x) '()
      (append
        (with v (ox-attr x 'table:align)
          (if v (list (cons 'table-align v)) '()))
        (with v (ox-attr x 'style:rel-width)
          (if v (list (cons 'table-width v)) '())))))

(define (odt-column-properties x)
  ;; the width of a column, as a number: its unit does not matter, since
  ;; only the parts of the columns in the table are kept
  (let* ((v (and x (or (ox-attr x 'style:rel-column-width)
                       (ox-attr x 'style:column-width))))
         (n (and v (let loop ((i 0))
                     (if (and (< i (string-length v))
                              (or (char-numeric? (string-ref v i))
                                  (char=? (string-ref v i) #\.)))
                         (loop (+ i 1))
                         (string->number (substring v 0 i))))))
         (unit (and v n (let loop ((i 0))
                          (if (and (< i (string-length v))
                                   (or (char-numeric? (string-ref v i))
                                       (char=? (string-ref v i) #\.)))
                              (loop (+ i 1))
                              (substring v i (string-length v))))))
         (scale (assoc-ref '(("cm" . 1.0) ("mm" . 0.1) ("in" . 2.54)
                             ("pt" . 0.03528) ("*" . 1.0))
                           unit)))
    (if (and n scale) (list (cons 'col-width (* n scale))) '())))

(define (odt-plain-name s)
  ;; the name of a style as it is shown: "Heading_20_1" is "heading 1"
  (locase-all (string-replace s "_20_" " ")))

(define (odt-read-styles root)
  ;; the styles of a file: the named ones and the automatic ones
  (when root
    (for (group (append (ox-childs root 'office:styles)
                        (ox-childs root 'office:automatic-styles)))
      (for (x (ox-childs group 'style:style))
        (let ((name (ox-attr x 'style:name))
              (family (or (ox-attr x 'style:family) "paragraph")))
          (when name
            (ahash-set!
              odt-styles (cons family name)
              (append
                (list (cons 'name (odt-plain-name
                                    (or (ox-attr x 'style:display-name) name))))
                (with p (ox-attr x 'style:parent-style-name)
                  (if p (list (cons 'parent p)) '()))
                (with v (ox-attr x 'style:default-outline-level)
                  (if (and v (string->number v))
                      (list (cons 'outline (string->number v))) '()))
                (odt-text-properties (ox-child x 'style:text-properties))
                (odt-paragraph-properties
                  (ox-child x 'style:paragraph-properties))
                (odt-cell-properties
                  (ox-child x 'style:table-cell-properties))
                (odt-table-properties (ox-child x 'style:table-properties))
                (odt-column-properties
                  (ox-child x 'style:table-column-properties)))))))
      ;; the lists: a style with a kind for each level
      (for (x (ox-childs group 'text:list-style))
        (for (lvl (ox-elements x))
          (with level (ox-attr lvl 'text:level)
            (when level
              (ahash-set! odt-lists (cons (ox-attr x 'style:name) level)
                          (if (func? lvl 'text:list-level-style-number)
                              ;; numbers without a format are no numbers
                              (if (in? (ox-attr lvl 'style:num-format) '(#f ""))
                                  "bullet" "number")
                              "bullet")))))))))

(define (odt-style-sub family name seen)
  (with own (and name (not (in? name seen))
                 (ahash-ref odt-styles (cons family name)))
    (if (not own) '()
        (append own (odt-style-sub family (odt-get own 'parent)
                                   (cons name seen))))))

(define (odt-style family name)
  ;; the properties of the style, with those of its parents
  (cond ((not name) '())
        ((ahash-ref odt-style-memo (cons family name)) => identity)
        (else (with r (odt-style-sub family name '())
                (ahash-set! odt-style-memo (cons family name) r)
                r))))

(define (odt-style-names family name)
  ;; the names of the style and of its parents
  (let loop ((name name) (seen '()))
    (with own (and name (not (in? name seen))
                   (ahash-ref odt-styles (cons family name)))
      (if (not own) '()
          (cons (odt-get own 'name)
                (loop (odt-get own 'parent) (cons name seen)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What a paragraph is: from the names of its styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define odt-roles
  '(("title" . "title") ("subtitle" . "subtitle")
    ("author" . "author") ("authors" . "author") ("date" . "date")
    ("abstract" . "abstract")
    ("quotations" . "quote") ("quote" . "quote") ("block text" . "quote")
    ("block quotation" . "quote")
    ("caption" . "caption") ("figurecaption" . "caption")
    ("tablecaption" . "caption") ("image caption" . "caption")
    ("table caption" . "caption")
    ("preformatted text" . "code") ("source code" . "code")
    ("code" . "code")
    ("abstract title" . "skip") ("abstracttitle" . "skip")
    ("figurewithcaption" . "figure") ("figure with caption" . "figure")
    ("captioned figure" . "figure")
    ("definition term" . "term") ("definition definition" . "definition")
    ("definition" . "definition")
    ("list heading" . "term") ("list contents" . "definition")))

(define (odt-heading-level name)
  (and (string-starts? name "heading ")
       (string->number (substring name 8 (string-length name)))))

(define (odt-role names)
  ;; (role level) of a paragraph with the styles of these names; the
  ;; "tight" variants of a style, for the items of a list, are the style
  (let* ((names (map (lambda (n)
                      (if (string-ends? n " tight")
                          (substring n 0 (- (string-length n) 6))
                          n))
                    names))
        (level (list-find (map odt-heading-level names) identity))
        (role (list-find (map (lambda (n) (assoc-ref odt-roles n)) names)
                         identity)))
    (cond (level (list "heading" (number->string level)))
          (role (list role #f))
          (else (list #f #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images and mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (odt-entry-name href)
  ;; the name in the archive of an address: without ./ and the last /
  (let* ((s (if (string-starts? href "./")
                (substring href 2 (string-length href)) href))
         (s (if (string-ends? s "/") (substring s 0 (- (string-length s) 1)) s)))
    s))

(define (odt-frame x)
  ;; a frame: an image, a formula, or a box of text
  (let* ((image (ox-child x 'draw:image))
         (object (ox-child x 'draw:object))
         (box (ox-child x 'draw:text-box))
         (display? (!= (ox-attr x 'text:anchor-type) "as-char"))
         (alt (with t (or (ox-child x 'svg:title) (ox-child x 'svg:desc))
                (and t (ox-text t)))))
    (cond (object
           ;; the formula is in the frame, or in content.xml of a directory
           (let* ((href (ox-attr object 'xlink:href))
                  (inside (ox-find object 'math))
                  (data (and href
                             (office-entry odt-archive
                                           (string-append (odt-entry-name href)
                                                          "/content.xml")))))
             (cond (data
                    (list (office-node 'math
                                       `((display ,(and display? "true")))
                                       data)))
                   (inside
                    (list (office-node 'math
                                       `((display ,(and display? "true"))
                                         (form "sxml"))
                                       inside)))
                   ;; another object: its picture, if any
                   (image (odt-frame-image x image alt))
                   (else '()))))
          (image (odt-frame-image x image alt))
          (box
           ;; the paragraphs of the box, as lines
           (let loop ((l (odt-blocks (ox-children box))) (acc '()))
             (cond ((null? l) (reverse acc))
                   ((func? (car l) 'p)
                    (loop (cdr l)
                          (append (reverse (ox-children (car l)))
                                  (if (null? acc) acc (cons '(br) acc)))))
                   (else (loop (cdr l) acc)))))
          (else '()))))

(define (odt-frame-image x image alt)
  (let* ((href (ox-attr image 'xlink:href))
         (name (and href (odt-entry-name href)))
         (data (and name (office-entry odt-archive name))))
    (cond (data
           (list (office-node
                   'image
                   `((name ,(cAr (string-tokenize-by-char name #\/)))
                     (data ,data)
                     (width ,(ox-attr x 'svg:width))
                     (height ,(ox-attr x 'svg:height))
                     (alt ,alt)))))
          ;; an image which is not in the archive: its address
          ((and href (>= (string-search-forwards "://" 0 href) 0))
           (list (office-node 'image `((name ,href)
                                       (width ,(ox-attr x 'svg:width))
                                       (height ,(ox-attr x 'svg:height))
                                       (alt ,alt)))))
          (else '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (odt-collapse s)
  ;; spaces, tabs and ends of lines in the text of the file are one space
  (let loop ((l (string->list s)) (acc '()) (space? #f))
    (cond ((null? l) (list->string (reverse acc)))
          ((in? (car l) '(#\space #\tab #\newline #\return))
           (loop (cdr l) (if space? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

;; the styles of text which are not a way of writing
(define odt-plain-styles
  '("internet link" "visited internet link" "footnote anchor"
    "endnote anchor" "footnote characters" "endnote characters"
    "footnote symbol" "endnote symbol"))

(define (odt-span-wrappers name)
  ;; the wrappers of the text of a span, the innermost first
  (let* ((names (odt-style-names "text" name))
         (props (if (and (pair? names) (in? (car names) odt-plain-styles)) '()
                    (odt-style "text" name)))
         (code? (or (odt-get props 'code)
                    (list-or (map (lambda (n)
                                    (in? n '("source text" "teletype"
                                             "verbatim char" "code"
                                             "example")))
                                  names)))))
    (list-filter
      (list (and code? 'code)
            (and (odt-get props 'sub) 'sub)
            (and (odt-get props 'sup) 'sup)
            (and (odt-get props 'smallcaps) 'smallcaps)
            (and (odt-get props 'strike) 'strike)
            (and (odt-get props 'underline) 'underline)
            (and (odt-get props 'em) 'em)
            (and (odt-get props 'strong) 'strong))
      identity)))

(define (odt-wrap-text l wrappers)
  ;; the text of the nodes inside the wrappers; a note, an image or a
  ;; formula stays outside
  (if (null? wrappers) l
      (append-map (lambda (x)
                    (if (and (pair? x) (in? (car x) '(note image math br tab
                                                      bookmark pagebreak)))
                        (list x)
                        (office-wrap (list x) wrappers)))
                  l)))

(define (odt-inline x)
  ;; the nodes of a child of a paragraph
  (cond ((string? x) (list (odt-collapse x)))
        ((not (pair? x)) '())
        (else
          (case (car x)
            ((text:span)
             (odt-wrap-text (odt-inlines (ox-children x))
                            (odt-span-wrappers (ox-attr x 'text:style-name))))
            ((text:s)
             (with n (or (and (ox-attr x 'text:c) (string->number (ox-attr x 'text:c)))
                         1)
               ;; (spaces which count: not those which odt-trim removes)
               (list (make-string n odt-hard-space))))
            ((text:tab) '((tab)))
            ((text:line-break) '((br)))
            ((text:a)
             (let ((href (ox-attr x 'xlink:href))
                   (l (odt-inlines (ox-children x))))
               (cond ((null? l) '())
                     (href (list `(link (@ (href ,href)) ,@l)))
                     (else l))))
            ((text:note)
             (with body (ox-child x 'text:note-body)
               (if body (list (cons 'note (odt-blocks (ox-children body)))) '())))
            ((text:bookmark text:bookmark-start text:reference-mark
              text:reference-mark-start)
             (with name (ox-attr x 'text:name)
               (if name (list `(bookmark (@ (name ,name)))) '())))
            ((text:bookmark-ref text:reference-ref text:sequence-ref)
             (let ((name (ox-attr x 'text:ref-name))
                   (l (odt-inlines (ox-children x))))
               (cond ((null? l) '())
                     (name (list `(ref (@ (name ,name)) ,@l)))
                     (else l))))
            ((draw:frame) (odt-frame x))
            ((draw:a) (odt-inlines (ox-children x)))
            ((text:ruby)
             (with base (ox-child x 'text:ruby-base)
               (if base (odt-inlines (ox-children base)) '())))
            ;; comments, changes and what has no text of its own
            ((office:annotation office:annotation-end text:change
              text:change-start text:change-end text:bookmark-end
              text:reference-mark-end text:soft-page-break
              text:alphabetical-index-mark text:alphabetical-index-mark-start
              text:alphabetical-index-mark-end text:toc-mark
              text:toc-mark-start text:toc-mark-end)
             '())
            ;; fields and anything else: their text
            (else (odt-inlines (ox-children x)))))))

(define (odt-inlines l)
  (append-map odt-inline l))

(define odt-hard-space (integer->char 1))

(define (odt-soften x)
  ;; the spaces which count as usual spaces again
  (cond ((string? x)
         (list->string (map (lambda (c) (if (char=? c odt-hard-space) #\space c))
                            (string->list x))))
        ((and (pair? x) (in? (car x) '(image math note)))
         x)
        ((pair? x) (cons (car x) (map odt-soften (cdr x))))
        (else x)))

(define (odt-trim l)
  ;; without spaces at the ends of the text of a paragraph
  (map odt-soften (odt-trim-sub l)))

(define (odt-trim-sub l)
  (let* ((l (office-merge l))
         (l (if (and (pair? l) (string? (car l)))
                (with s (odt-trim-left (car l))
                  (if (== s "") (cdr l) (cons s (cdr l))))
                l))
         (r (reverse l))
         (r (if (and (pair? r) (string? (car r)))
                (with s (odt-trim-right (car r))
                  (if (== s "") (cdr r) (cons s (cdr r))))
                r)))
    (reverse r)))

(define (odt-trim-left s)
  (let loop ((i 0))
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
        (loop (+ i 1))
        (substring s i (string-length s)))))

(define (odt-trim-right s)
  (let loop ((i (string-length s)))
    (if (and (> i 0) (char=? (string-ref s (- i 1)) #\space))
        (loop (- i 1))
        (substring s 0 i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (odt-paragraph x heading?)
  (let* ((name (ox-attr x 'text:style-name))
         (props (odt-style "paragraph" name))
         (names (odt-style-names "paragraph" name))
         (outline (or (and (ox-attr x 'text:outline-level)
                           (string->number (ox-attr x 'text:outline-level)))
                      (odt-get props 'outline)))
         (role (cond ((and heading? outline)
                      (list "heading" (number->string outline)))
                     (heading? (list "heading" "1"))
                     (else (odt-role names))))
         (align (odt-get props 'align))
         (l (odt-trim (odt-inlines (ox-children x)))))
    (append
      (if (odt-get props 'page-break) '((pagebreak)) '())
      (if (null? l) '()
          (list (apply office-node
                       (cons* 'p
                              `((role ,(car role)) (level ,(cadr role))
                                (align ,(cond ((== align "center") "center")
                                              ((in? align '("end" "right")) "right")
                                              (else #f))))
                              l)))))))

(define (odt-list x)
  ;; a list: its style is its own, or the one of the list it is in
  (let* ((old-style odt-list-style)
         (old-depth odt-list-depth)
         (style (or (ox-attr x 'text:style-name) odt-list-style)))
    (set! odt-list-style style)
    (set! odt-list-depth (+ old-depth 1))
    (let* ((kind (or (ahash-ref odt-lists
                                (cons style (number->string odt-list-depth)))
                     "bullet"))
           (items (map (lambda (item)
                         (cons 'item (odt-blocks (ox-children item))))
                       (list-filter (ox-children x)
                                    (lambda (y)
                                      (and (pair? y)
                                           (in? (car y) '(text:list-item
                                                          text:list-header))))))))
      (set! odt-list-style old-style)
      (set! odt-list-depth old-depth)
      (cond ((null? items) '())
            ;; headings which are numbered as the items of a list
            ((list-and (map (lambda (item)
                              (and (list-1? (cdr item)) (func? (cadr item) 'p)
                                   (== (ox-attr (cadr item) 'role) "heading")))
                            items))
             (map cadr items))
            (else (list `(list (@ (kind ,kind)) ,@items)))))))

(define (odt-cell x header?)
  ;; the cells of a table:table-cell: itself, as many times as it says
  (let* ((n (or (and (ox-attr x 'table:number-columns-repeated)
                     (string->number (ox-attr x 'table:number-columns-repeated)))
                1))
         (props (odt-style "table-cell" (ox-attr x 'table:style-name)))
         (letters (string-append (if (odt-get props 'border-top) "t" "")
                                 (if (odt-get props 'border-bottom) "b" "")
                                 (if (odt-get props 'border-left) "l" "")
                                 (if (odt-get props 'border-right) "r" "")))
         (cell (if (func? x 'table:covered-table-cell)
                   `(cell (@ (covered "true")))
                   (apply office-node
                          (cons* 'cell
                                 `((header ,(and header? "true"))
                                   (borders ,(if (== letters "") "none" letters))
                                   (background ,(odt-get props 'fill))
                                   (colspan ,(with v (ox-attr x 'table:number-columns-spanned)
                                               (and v (!= v "1") v)))
                                   (rowspan ,(with v (ox-attr x 'table:number-rows-spanned)
                                               (and v (!= v "1") v))))
                                 (odt-blocks (ox-children x)))))))
    (map (lambda (i) cell) (iota (min n 64)))))

(define (odt-rows l header?)
  (append-map
    (lambda (x)
      (cond ((func? x 'table:table-row)
             (list (cons 'row
                         (append-map
                           (lambda (c) (odt-cell c header?))
                           (list-filter (ox-children x)
                                        (lambda (y)
                                          (and (pair? y)
                                               (in? (car y)
                                                    '(table:table-cell
                                                      table:covered-table-cell)))))))))
            ((func? x 'table:table-header-rows) (odt-rows (ox-children x) #t))
            ((and (pair? x) (in? (car x) '(table:table-rows table:table-row-group)))
             (odt-rows (ox-children x) header?))
            (else '())))
    l))

(define (odt-table-columns x)
  ;; the widths of the columns, as parts of their sum, or #f
  (let* ((cols (append-map
                 (lambda (c)
                   (cond ((func? c 'table:table-column)
                          (let ((n (or (and (ox-attr c 'table:number-columns-repeated)
                                            (string->number
                                              (ox-attr c 'table:number-columns-repeated)))
                                       1))
                                (w (odt-get (odt-style "table-column"
                                                       (ox-attr c 'table:style-name))
                                            'col-width)))
                            (map (lambda (i) w) (iota (min n 64)))))
                         ((func? c 'table:table-columns)
                          (ox-childs c 'table:table-column))
                         (else '())))
                 (ox-children x)))
         (ok? (and (pair? cols) (list-and (map number? cols))))
         (sum (if ok? (apply + cols) 0)))
    (and ok? (> sum 0)
         (string-recompose
           (map (lambda (w) (number->string (/ (round (* 1000.0 (/ w sum))) 1000.0)))
                cols)
           " "))))

(define (odt-table x)
  (let* ((rows (odt-rows (ox-children x) #f))
         (props (odt-style "table" (ox-attr x 'table:style-name)))
         (align (odt-get props 'table-align))
         (width (odt-get props 'table-width))
         (percent (and width (string-ends? width "%")
                       (string->number (substring width 0 (- (string-length width) 1))))))
    (if (null? rows) '()
        (list (apply office-node
                     (cons* 'table
                            `((align ,(cond ((== align "center") "center")
                                            ((== align "right") "right")
                                            (else #f)))
                              (width ,(and percent (< percent 100)
                                           (string-append
                                             (number->string (/ percent 100.0))
                                             "par")))
                              (columns ,(odt-table-columns x)))
                            rows))))))

(define (odt-block x)
  (cond ((not (pair? x)) '())
        (else
          (case (car x)
            ((text:h) (odt-paragraph x #t))
            ((text:p) (odt-paragraph x #f))
            ((text:list) (odt-list x))
            ((table:table) (odt-table x))
            ((text:section text:index-body) (odt-blocks (ox-children x)))
            ((draw:frame)
             (with l (odt-frame x)
               (if (null? l) '() (list (cons 'p l)))))
            (else '())))))

(define (odt-merge-lists l)
  ;; A list which goes on at a deeper level after another one is written
  ;; as a new list whose first item has no text, only a list: it is the
  ;; end of the last item of the list before.
  (cond ((or (null? l) (null? (cdr l))) l)
        ((and (func? (car l) 'list) (func? (cadr l) 'list)
              (pair? (ox-children (car l))) (pair? (ox-children (cadr l)))
              (with first (car (ox-children (cadr l)))
                (and (pair? (cdr first)) (func? (cadr first) 'list))))
         (let* ((a (car l))
                (b (cadr l))
                (items (ox-children a))
                (first (car (ox-children b)))
                (merged `(list (@ ,@(ox-attrs a))
                               ,@(cDr items)
                               ;; (the same may happen again, one level deeper)
                               ,(cons 'item
                                      (odt-merge-lists
                                        (append (cdr (cAr items)) (cdr first))))
                               ,@(cdr (ox-children b)))))
           (odt-merge-lists (cons merged (cddr l)))))
        (else (cons (car l) (odt-merge-lists (cdr l))))))

(define (odt-inline-node? x)
  ;; text where a paragraph should be: some programs write it
  (or (and (string? x) (!= (string-trim-spaces x) ""))
      (and (pair? x) (in? (car x) '(text:span text:a text:s text:line-break
                                    text:note)))))

(define (odt-blocks-sub l)
  ;; the blocks of the children l; text which is in no paragraph is one
  (let loop ((l l) (text '()) (acc '()))
    (let ((flush (lambda ()
                   (with t (odt-trim (odt-inlines (reverse text)))
                     (if (null? t) acc (cons (cons 'p t) acc))))))
      (cond ((null? l) (reverse (flush)))
            ((or (odt-inline-node? (car l))
                 (and (pair? text) (or (string? (car l)) (func? (car l) 'draw:frame))))
             (loop (cdr l) (cons (car l) text) acc))
            ((string? (car l)) (loop (cdr l) text acc))
            (else (loop (cdr l) '()
                        (append (reverse (odt-block (car l))) (flush))))))))

(define (odt-blocks l)
  (odt-merge-lists (odt-blocks-sub l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (odt-meta)
  ;; the title and the author of meta.xml
  (let* ((root (office-xml odt-archive "meta.xml"))
         (meta (and root (ox-child root 'office:meta))))
    (if (not meta) '()
        (let* ((get (lambda (tag key)
                      (with x (ox-child meta tag)
                        (if (and x (!= (string-trim-spaces (ox-text x)) ""))
                            (list (list key (string-trim-spaces (ox-text x))))
                            '()))))
               (l (append (get 'dc:title 'title)
                          (with a (get 'meta:initial-creator 'author)
                            (if (null? a) (get 'dc:creator 'author) a)))))
          (if (null? l) '() (list (cons 'meta l)))))))

(tm-define (parse-odt-document s)
  (:type (-> string stree))
  (:synopsis "Read the OpenDocument text @s, a zip archive, into an office tree")
  (set! odt-archive (office-archive s))
  (if (not odt-archive) '(office)
      (let ((content (office-xml odt-archive "content.xml")))
        (set! odt-styles (make-ahash-table))
        (set! odt-style-memo (make-ahash-table))
        (set! odt-lists (make-ahash-table))
        (set! odt-list-style #f)
        (set! odt-list-depth 0)
        (odt-read-styles (office-xml odt-archive "styles.xml"))
        (odt-read-styles content)
        (let* ((text (and content (ox-path content 'office:body 'office:text)))
               (r `(office ,@(odt-meta)
                           ,@(if text (odt-blocks (ox-children text)) '()))))
          (set! odt-archive #f)
          (set! odt-styles (make-ahash-table))
          (set! odt-style-memo (make-ahash-table))
          r))))
