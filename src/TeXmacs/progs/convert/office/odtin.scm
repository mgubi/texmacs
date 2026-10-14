
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
(define odt-floats '())                  ; blocks found inside a paragraph
(define odt-list-formats (make-ahash-table)) ; (list style . level) -> (format start)
(define odt-outline (make-ahash-table))  ; level -> whether headings have numbers
(define odt-body-styles (make-ahash-table)) ; style -> plain paragraphs
(define odt-defaults '())                ; the properties of the default style
(define odt-list-counts (make-ahash-table)) ; list style -> items so far
(define odt-used-styles (make-ahash-table)) ; (family . name) of the named styles
(define odt-changes (make-ahash-table))  ; id -> (kind attributes element)

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
                                (with n (office-number v)
                                  (and n (>= n 600))))))
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
                (else
                  (with n (or (office-number (car (string-tokenize-by-char v #\%)))
                              0)
                    (cond ((> n 0) '((sup . #t) (sub . #f)))
                          ((< n 0) '((sub . #t) (sup . #f)))
                          (else '((sub . #f) (sup . #f))))))))
        (with v (ox-attr x 'fo:color)
          (if v (list (cons 'color (office-color v))) '()))
        (with v (ox-attr x 'fo:background-color)
          (if v (list (cons 'mark (and (office-color v) (!= (locase-all v) "#ffffff"))))
              '()))
        (with v (ox-attr x 'fo:font-variant)
          (if v (list (cons 'smallcaps (== v "small-caps"))) '()))
        (with v (or (ox-attr x 'style:font-name) (ox-attr x 'fo:font-family))
          (if v (list (cons 'code (office-monospace? v)) (cons 'font v)) '()))
        (with v (ox-attr x 'text:display)
          (if v (list (cons 'hidden (== v "none"))) '()))
        ;; (a size in percents is one of the parent)
        (with v (office-points (ox-attr x 'fo:font-size))
          (if (and v (> v 0)) (list (cons 'size v)) '())))))

(define (odt-paragraph-properties x)
  ;; the properties of an element style:paragraph-properties
  (if (not x) '()
      (append
        (with v (ox-attr x 'fo:text-align)
          (if v (list (cons 'align v)) '()))
        (with v (ox-attr x 'fo:break-before)
          (if (== v "page") '((page-break . #t)) '()))
        (with v (office-points (ox-attr x 'fo:margin-top))
          (if v (list (cons 'space-before v)) '()))
        (with v (office-points (ox-attr x 'fo:margin-bottom))
          (if v (list (cons 'space-after v)) '()))
        (with v (office-points (ox-attr x 'fo:text-indent))
          (if v (list (cons 'first-indent v)) '()))
        (with v (office-points (ox-attr x 'fo:margin-left))
          (if v (list (cons 'indent-left v)) '()))
        (with v (ox-attr x 'fo:background-color)
          (if (and v (string-starts? v "#") (!= (locase-all v) "#ffffff"))
              (list (cons 'background (locase-all v))) '()))
        (with v (ox-attr x 'fo:border)
          (if (and v (not (string-starts? v "none"))) '((border . #t)) '()))
        (with v (ox-attr x 'fo:line-height)
          (cond ((not v) '())
                ((== v "normal") '((line-spacing . 1.0)))
                ((string-ends? v "%")
                 (with n (office-number (substring v 0 (- (string-length v) 1)))
                   (if (and n (> n 0)) (list (cons 'line-spacing (/ n 100.0))) '())))
                (else '()))))))

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
                ;; (a style which the program makes for what is formatted
                ;; by hand)
                (if (func? group 'office:automatic-styles)
                    '((automatic . #t)) '())
                (with v (office-integer (ox-attr x 'style:default-outline-level)
                                        #f 1 10)
                  (if v (list (cons 'outline v)) '()))
                (odt-text-properties (ox-child x 'style:text-properties))
                (odt-paragraph-properties
                  (ox-child x 'style:paragraph-properties))
                (odt-cell-properties
                  (ox-child x 'style:table-cell-properties))
                (odt-table-properties (ox-child x 'style:table-properties))
                (odt-column-properties
                  (ox-child x 'style:table-column-properties))
                ;; a section in columns
                (with c (and (ox-child x 'style:section-properties)
                             (ox-child (ox-child x 'style:section-properties)
                                       'style:columns))
                  (with n (and c (office-integer (ox-attr c 'fo:column-count) 1 1 10))
                    (if (and n (> n 1)) (list (cons 'columns n)) '()))))))))
      ;; the default style of paragraphs
      (for (x (ox-childs group 'style:default-style))
        (when (== (ox-attr x 'style:family) "paragraph")
          (set! odt-defaults
                (append (odt-text-properties (ox-child x 'style:text-properties))
                        (odt-paragraph-properties
                          (ox-child x 'style:paragraph-properties))))))
      ;; the numbers of the headings
      (for (x (ox-childs group 'text:outline-style))
        (for (lvl (ox-childs x 'text:outline-level-style))
          (with level (office-integer (ox-attr lvl 'text:level) #f 1 10)
            (when level
              (ahash-set! odt-outline level
                          (not (in? (ox-attr lvl 'style:num-format) '(#f ""))))))))
      ;; the lists: a style with a kind for each level
      (for (x (ox-childs group 'text:list-style))
        (for (lvl (ox-elements x))
          (with level (ox-attr lvl 'text:level)
            (when (and level (func? lvl 'text:list-level-style-number))
              (ahash-set! odt-list-formats (cons (ox-attr x 'style:name) level)
                          (list (with f (ox-attr lvl 'style:num-format)
                                  (if (in? f '("a" "A" "i" "I")) f "1"))
                                (ox-attr lvl 'text:start-value))))
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
    ("theorem" . "theorem") ("remark" . "remark") ("proof" . "proof")
    ("bibliography" . "bibitem") ("bibliography 1" . "bibitem")
    ("list heading" . "term") ("list contents" . "definition")))

(define (odt-heading-level name)
  (and (string-starts? name "heading ")
       (office-integer (substring name 8 (string-length name)) #f 1 10)))

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

(define odt-apart-depth 0)  ; inside a cell, a note, a box, a header

(define (odt-apart thunk)
  ;; the result of the thunk, which reads what is not in the lists around
  ;; it: a cell, a note, a box of text
  (let ((old-style odt-list-style)
        (old-depth odt-list-depth))
    (set! odt-list-style #f)
    (set! odt-list-depth 0)
    (set! odt-apart-depth (+ odt-apart-depth 1))
    (with r (thunk)
      (set! odt-apart-depth (- odt-apart-depth 1))
      (set! odt-list-style old-style)
      (set! odt-list-depth old-depth)
      r)))

(define (odt-with-floats thunk)
  ;; (result . blocks): the result of the thunk, which reads the text of a
  ;; paragraph, and the blocks which were found in this text and which
  ;; come after the paragraph
  (with old odt-floats
    (set! odt-floats '())
    (let* ((r (thunk))
           (l odt-floats))
      (set! odt-floats old)
      (cons r l))))

(define (odt-formula? name)
  ;; whether the entry name of the archive is a formula of MathML
  (with root (office-xml odt-archive name)
    (and root (pair? root) (symbol? (car root))
         (let* ((s (symbol->string (car root)))
                (i (string-search-forwards ":" 0 s)))
           (== (if (>= i 0) (substring s (+ i 1) (string-length s)) s)
               "math")))))

(define (odt-frame x)
  ;; a frame: an image, a formula, or a box of text
  (let* ((images (ox-childs x 'draw:image))
         ;; several images are the same picture: a bitmap is the one which
         ;; all the converters and renderers know
         (image (or (list-find images
                               (lambda (i)
                                 (with h (locase-all (or (ox-attr i 'xlink:href) ""))
                                   (list-or (map (lambda (s) (string-ends? h s))
                                                 '(".png" ".jpg" ".jpeg" ".gif"))))))
                    (and (pair? images) (car images))))
         (object (ox-child x 'draw:object))
         (box (ox-child x 'draw:text-box))
         (display? (!= (ox-attr x 'text:anchor-type) "as-char"))
         (alt (with t (or (ox-child x 'svg:title) (ox-child x 'svg:desc))
                (and t (ox-text t)))))
    (cond (object
           ;; the formula is in the frame, or in content.xml of a directory
           (let* ((href (ox-attr object 'xlink:href))
                  (inside (ox-find object 'math))
                  (name (and href (string-append (odt-entry-name href)
                                                 "/content.xml")))
                  ;; (another object, such as a chart, has a content too)
                  (data (and name (office-entry odt-archive name)
                             (odt-formula? name)
                             (office-entry odt-archive name))))
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
           ;; the blocks of the box come after the paragraph which the
           ;; frame is in (a figure and its caption, a note in a box)
           (with l (odt-apart (lambda () (odt-blocks (ox-children box))))
             (set! odt-floats (append odt-floats l))
             '()))
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
          ;; An image which is not in the archive is not fetched: a link
          ;; to its address on the web, and the text of any other address.
          ((and href (or (string-starts? (locase-all href) "http://")
                         (string-starts? (locase-all href) "https://")))
           (list `(link (@ (href ,href))
                        ,(if (and alt (!= (string-trim-spaces alt) "")) alt href))))
          ((and href (or (>= (string-search-forwards "://" 0 href) 0)
                         (string-starts? (locase-all href) "file:")))
           (list href))
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
  (with names (odt-style-names "text" name)
    (odt-wrappers (if (and (pair? names) (in? (car names) odt-plain-styles)) '()
                      (odt-style "text" name))
                  names)))

(define (odt-paragraph-wrappers name)
  ;; the wrappers of the text of a paragraph: what its automatic style
  ;; says of its text, when the whole paragraph is formatted by hand
  (with own (and name (ahash-ref odt-styles (cons "paragraph" name)))
    (if (and own (odt-get own 'automatic)) (odt-wrappers own '()) '())))

(define (odt-wrappers props names)
  ;; the wrappers for the properties props of a style with these names
  (let* ((code? (or (odt-get props 'code)
                    (list-or (map (lambda (n)
                                    (in? n '("source text" "teletype"
                                             "verbatim char" "code"
                                             "example")))
                                  names)))))
    (list-filter
      (list (and code? 'code)
            (and (odt-get props 'color) (list 'color (odt-get props 'color)))
            (and (odt-get props 'mark) 'mark)
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
                                                      bookmark pagebreak seq)))
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
             (let* ((name (ox-attr x 'text:style-name))
                    (named (odt-text-style name)))
               (cond ((odt-get (odt-style "text" name) 'hidden) '())
                     ;; a named style of text: a span, with what the span
                     ;; adds to it by hand
                     (named
                      (ahash-set! odt-used-styles (cons "text" named) #t)
                      (with own (ahash-ref odt-styles (cons "text" name))
                        (list `(span (@ (style ,named))
                                     ,@(odt-wrap-text
                                         (odt-inlines (ox-children x))
                                         (if (and own (odt-get own 'automatic))
                                             (odt-wrappers own '())
                                             '()))))))
                     (else
                       (odt-wrap-text (odt-inlines (ox-children x))
                                      (odt-span-wrappers name))))))
            ;; text which is hidden, or shown only when a condition holds
            ((text:hidden-text text:hidden-paragraph)
             (if (== (ox-attr x 'text:is-hidden) "false")
                 (odt-inlines (ox-children x))
                 '()))
            ((text:s)
             (with n (office-integer (ox-attr x 'text:c) 1 0 1000)
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
               (if body
                   (list (cons 'note (odt-apart
                                       (lambda () (odt-blocks (ox-children body))))))
                   '())))
            ((text:bookmark text:bookmark-start text:reference-mark
              text:reference-mark-start)
             ;; (not the marks of the citations of Zotero and others,
             ;; whose names are their data)
             (with name (ox-attr x 'text:name)
               (if (and name (odt-label-name? name))
                   (list `(bookmark (@ (name ,name))))
                   '())))
            ((text:bookmark-ref text:reference-ref text:sequence-ref)
             (let ((name (ox-attr x 'text:ref-name))
                   (l (odt-inlines (ox-children x))))
               (cond ((null? l) '())
                     (name (list `(ref (@ (name ,name)) ,@l)))
                     (else l))))
            ;; a number of a sequence: of the figures, of the tables
            ((text:sequence)
             ;; with its label, which the references to it name
             (let ((l (odt-inlines (ox-children x)))
                   (ref (ox-attr x 'text:ref-name)))
               (if (null? l) '()
                   (append
                     (if ref (list `(bookmark (@ (name ,ref)))) '())
                     (list `(seq (@ (name ,(or (ox-attr x 'text:name) ""))) ,@l))))))
            ((draw:frame) (odt-frame x))
            ;; a shape with text: its blocks come after the paragraph
            ((draw:custom-shape draw:rect draw:ellipse draw:polygon draw:path
              draw:polyline draw:g draw:circle draw:regular-polygon)
             (with l (odt-apart (lambda () (odt-blocks (ox-children x))))
               (set! odt-floats (append odt-floats l))
               '()))
            ((draw:a) (odt-inlines (ox-children x)))
            ((text:ruby)
             (with base (ox-child x 'text:ruby-base)
               (if base (odt-inlines (ox-children base)) '())))
            ((office:annotation) (odt-annotation x))
            ((text:change text:change-start text:change-end) (odt-change-node x))
            ;; what has no text of its own
            ((office:annotation-end text:bookmark-end
              text:reference-mark-end text:soft-page-break
              text:alphabetical-index-mark text:alphabetical-index-mark-start
              text:alphabetical-index-mark-end text:toc-mark
              text:toc-mark-start text:toc-mark-end)
             '())
            ;; fields and anything else: their text
            (else (if (memq (car x) odt-fields) (odt-field x)
                      (odt-inlines (ox-children x))))))))

(define (odt-inlines l)
  (append-map odt-inline l))

(define (odt-text-style name)
  ;; the named style of text of the span of the style name, or #f: not
  ;; the styles of the links and notes, nor those of code
  (let* ((named (odt-named-style name "text"))
         (names (odt-style-names "text" named)))
    (and named (pair? names)
         (ahash-ref odt-styles (cons "text" named))
         (not (odt-get (ahash-ref odt-styles (cons "text" named)) 'automatic))
         (not (in? (car names) odt-plain-styles))
         (not (list-or (map (lambda (n)
                              (in? n '("source text" "teletype" "verbatim char"
                                       "code" "example")))
                            names)))
         named)))

;; the fields of OpenDocument: their result, and the element to write again
(define odt-fields
  '(text:page-number text:page-count text:date text:time text:author-name
    text:author-initials text:initial-creator text:creator text:creation-date
    text:creation-time text:modification-date text:print-date text:title
    text:subject text:description text:keywords text:file-name
    text:template-name text:chapter text:sender-firstname
    text:sender-lastname text:sender-company text:user-field-get
    text:variable-get text:expression text:word-count text:character-count
    text:paragraph-count text:sheet-name text:user-defined))

(define (odt-field x)
  (list (apply office-node
               (cons* 'field
                      `((odt ,(symbol->string (car x)))
                        (xml ,(ox-serialize-element (cons (car x)
                                                          (if (null? (ox-attrs x)) '()
                                                              (list (cons '@ (ox-attrs x))))))))
                      (odt-inlines (ox-children x))))))

(define (odt-annotation x)
  ;; a comment, with its author and its date
  (let ((get (lambda (tag) (with y (ox-child x tag) (and y (ox-text y))))))
    (list (apply office-node
                 (cons* 'comment `((author ,(get 'dc:creator)) (date ,(get 'dc:date)))
                        (odt-apart
                          (lambda ()
                            (odt-blocks (list-filter (ox-children x)
                                                     (lambda (y)
                                                       (not (and (pair? y)
                                                                 (in? (car y) '(dc:creator dc:date))))))))))))))

(define (odt-read-changes text)
  ;; the regions of the changes which are tracked
  (set! odt-changes (make-ahash-table))
  (for (tc (if text (ox-childs text 'text:tracked-changes) '()))
    (for (r (ox-childs tc 'text:changed-region))
      (let* ((c (car (append (ox-elements r) '(#f))))
             (info (and c (ox-child c 'office:change-info)))
             (get (lambda (tag) (with y (and info (ox-child info tag)) (and y (ox-text y))))))
        (when c
          (ahash-set! odt-changes (or (ox-attr r 'text:id) (ox-attr r 'xml:id))
                      (list (cond ((func? c 'text:insertion) 'ins)
                                  ((func? c 'text:deletion) 'del)
                                  (else 'format))
                            ;; (the date which says that there is none)
                            `((author ,(get 'dc:creator))
                              (date ,(with d (get 'dc:date)
                                       (and d (not (string-starts? d "1970-01-01T00:00:00"))
                                            d))))
                            c)))))))

(define (odt-change-node x)
  ;; the mark of a change in the text
  (let* ((id (ox-attr x 'text:change-id))
         (c (and id (ahash-ref odt-changes id))))
    (cond ((not c) '())
          ((func? x 'text:change-start)
           (if (== (car c) 'ins) (list `(!change-start ins ,(cadr c))) '()))
          ((func? x 'text:change-end)
           (if (== (car c) 'ins) (list '(!change-end ins)) '()))
          ;; the text which was deleted, at its place
          ((== (car c) 'del)
           (with l (append-map (lambda (p) (odt-inlines (ox-children p)))
                               (list-filter (ox-elements (caddr c))
                                            (lambda (p) (in? (car p) '(text:p text:h)))))
             (if (null? l) '()
                 (list (apply office-node (cons* 'del (cadr c) l))))))
          (else '()))))

(define (odt-label-name? name)
  (and (< (string-length name) 200)
       (not (string-starts? name "ZOTERO_"))
       (not (string-starts? name "ZOTERO "))
       (not (string-starts? name "CSL_"))
       (not (string-starts? name "Mendeley"))
       (< (string-search-forwards "{" 0 name) 0)))

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

(define (odt-hidden-paragraph? x props)
  ;; a paragraph which is not shown: by its style, or by a field in it
  (or (odt-get props 'hidden)
      (list-or (map (lambda (y)
                      (and (func? y 'text:hidden-paragraph)
                           (!= (ox-attr y 'text:is-hidden) "false")))
                    (ox-children x)))))

(define (odt-paragraph x heading?)
  (let* ((name (ox-attr x 'text:style-name))
         (props (odt-style "paragraph" name))
         (names (odt-style-names "paragraph" name))
         (outline (or (office-integer (ox-attr x 'text:outline-level) #f 1 10)
                      (odt-get props 'outline)))
         (role (cond ((and heading? outline)
                      (list "heading" (number->string outline)))
                     (heading? (list "heading" "1"))
                     (else (odt-role names))))
         (align (odt-get props 'align))
         (r (odt-with-floats
              (lambda () (odt-trim (office-changes (odt-inlines (ox-children x)))))))
         (wrappers (if (car role) '() (odt-paragraph-wrappers name)))
         (named (and (not (car role)) (== odt-apart-depth 0) (== odt-list-depth 0)
                     (odt-named-style name)))
         (l (if (null? wrappers) (car r)
                (office-merge (odt-wrap-text (car r) wrappers)))))
    ;; (the style of the usual paragraphs is the one of most of them)
    (when (and (not (car role)) (== odt-list-depth 0) (== odt-apart-depth 0))
      (with k (or (odt-named-style name) "")
        (ahash-set! odt-body-styles k (+ 1 (or (ahash-ref odt-body-styles k) 0)))))
    (if (odt-hidden-paragraph? x props) '()
        (append
          (if (odt-get props 'page-break) '((pagebreak)) '())
          (if (null? l) (cdr r)
              (cons (apply office-node
                           (cons* 'p
                                  `((role ,(car role)) (level ,(cadr role))
                                    ;; a heading has a number when the outline
                                    ;; of its level has one
                                    ;; (a paragraph of a style of headings
                                    ;; is not numbered as one)
                                    (numbered ,(and (== (car role) "heading")
                                                    (or (not heading?)
                                                        (== (ox-attr x 'text:is-list-header)
                                                            "true")
                                                        (not (ahash-ref odt-outline
                                                                        (office-integer
                                                                          (cadr role) 1 1 10))))
                                                    "no"))
                                    (align ,(odt-align align))
                                    ;; the named style of a plain paragraph
                                    (style ,(and named
                                                 (begin
                                                   (ahash-set! odt-used-styles
                                                               (cons "paragraph" named) #t)
                                                   named))))
                                  l))
                    (cdr r)))))))

(define (odt-named-style name . family)
  ;; the style, or the one it is made from when the program made it for
  ;; what is formatted by hand
  (let* ((f (if (pair? family) (car family) "paragraph"))
         (own (and name (ahash-ref odt-styles (cons f name)))))
    (if (and own (odt-get own 'automatic)) (odt-get own 'parent) name)))

(define (odt-align a)
  (cond ((== a "center") "center")
        ((in? a '("end" "right")) "right")
        ((== a "justify") "justify")
        ((in? a '("start" "left")) "left")
        (else #f)))

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
           (format (ahash-ref odt-list-formats
                              (cons style (number->string odt-list-depth))))
           ;; (the first item may start at another number)
           (first (list-find (ox-children x) (lambda (y) (func? y 'text:list-item))))
           (count (length (list-filter (ox-children x)
                                       (lambda (y) (func? y 'text:list-item)))))
           (before (and (== odt-list-depth 1)
                        (ahash-ref odt-list-counts style)))
           ;; a list which goes on with the numbers of the one before it
           (go-on? (or (== (ox-attr x 'text:continue-numbering) "true")
                       (ox-attr x 'text:continue-list)))
           (start (or (and first (ox-attr first 'text:start-value))
                      (and go-on? before (number->string (+ before 1)))
                      (and format (cadr format))))
           (items (map (lambda (item)
                         (cons 'item (odt-blocks (ox-children item))))
                       (list-filter (ox-children x)
                                    (lambda (y)
                                      (and (pair? y)
                                           (in? (car y) '(text:list-item
                                                          text:list-header))))))))
      (set! odt-list-style old-style)
      (set! odt-list-depth old-depth)
      (when (== old-depth 0)
        (ahash-set! odt-list-counts style
                    (+ count (if (and go-on? before) before 0))))
      (cond ((null? items) '())
            ;; headings which are numbered as the items of a list
            ((list-and (map (lambda (item)
                              (and (list-1? (cdr item)) (func? (cadr item) 'p)
                                   (== (ox-attr (cadr item) 'role) "heading")))
                            items))
             (map (lambda (item)
                    (with h (cadr item)
                      (if (!= kind "number") h
                          (apply office-node
                                 (cons* 'p (list-filter (ox-attrs h)
                                                        (lambda (a) (!= (car a) 'numbered)))
                                        (ox-children h))))))
                  items))
            (else
              (list (apply office-node
                           (cons* 'list
                                  `((kind ,kind)
                                    (format ,(and (== kind "number") format
                                                  (!= (car format) "1") (car format)))
                                    (start ,(and (== kind "number") start
                                                 (!= start "1") start)))
                                  items))))))))

(define (odt-cell x header?)
  ;; the cells of a table:table-cell: itself, as many times as it says
  (let* ((n (office-integer (ox-attr x 'table:number-columns-repeated) 1 1 64))
         (span (lambda (name)
                 (with k (office-integer (ox-attr x name) 1 1 1000)
                   (and (> k 1) (number->string k)))))
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
                                   (colspan ,(span 'table:number-columns-spanned))
                                   (rowspan ,(span 'table:number-rows-spanned)))
                                 (odt-apart
                                   (lambda () (odt-blocks (ox-children x)))))))))
    (map (lambda (i) cell) (iota n))))

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

(define (odt-column-widths x)
  ;; the widths of the columns of the element x, a table or a group of
  ;; its columns: numbers, or #f for a column without a width
  (append-map
    (lambda (c)
      (cond ((func? c 'table:table-column)
             (let ((n (office-integer (ox-attr c 'table:number-columns-repeated)
                                      1 1 64))
                   (w (odt-get (odt-style "table-column"
                                          (ox-attr c 'table:style-name))
                               'col-width)))
               (map (lambda (i) w) (iota n))))
            ((and (pair? c) (in? (car c) '(table:table-columns
                                           table:table-column-group
                                           table:table-header-columns)))
             (odt-column-widths c))
            (else '())))
    (ox-children x)))

(define (odt-table-columns x)
  ;; the widths of the columns, as parts of their sum, or #f
  (let* ((cols (odt-column-widths x))
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
                       (office-number (substring width 0 (- (string-length width) 1))))))
    (if (null? rows) '()
        (list (apply office-node
                     (cons* 'table
                            `((align ,(cond ((== align "center") "center")
                                            ((== align "right") "right")
                                            (else #f)))
                              (width ,(and percent (> percent 0) (< percent 100)
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
            ;; a section in columns
            ((text:section)
             (let ((n (odt-get (odt-style "section" (ox-attr x 'text:style-name))
                               'columns))
                   (l (odt-blocks (ox-children x))))
               (if (and n (> n 1) (pair? l))
                   (list `(columns (@ (count ,(number->string n))) ,@l))
                   l)))
            ((text:index-body text:index-title
              text:numbered-paragraph draw:custom-shape draw:rect draw:g)
             (odt-blocks (ox-children x)))
            ;; a table of contents is made again by TeXmacs
            ((text:table-of-content) '((toc)))
            ;; the other indexes and the bibliography: their text
            ((text:bibliography text:alphabetical-index text:illustration-index
              text:table-index text:object-index text:user-index)
             (with body (ox-child x 'text:index-body)
               (if body (odt-blocks (ox-children body)) '())))
            ((draw:frame)
             (with r (odt-with-floats (lambda () (odt-frame x)))
               (append (if (null? (car r)) '() (list (cons 'p (car r))))
                       (cdr r))))
            (else '())))))

(define (odt-merge-lists l)
  ;; A list which goes on at a deeper level after another one is written
  ;; as a new list whose first item has no text, only a list: it is the
  ;; end of the last item of the list before. (A loop: a document may have
  ;; many thousands of blocks)
  (let loop ((l l) (acc '()))
   (cond ((or (null? l) (null? (cdr l))) (append (reverse acc) l))
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
           (loop (cons merged (cddr l)) acc)))
        (else (loop (cdr l) (cons (car l) acc))))))

(define (odt-inline-node? x)
  ;; text where a paragraph should be: some programs write it
  (or (and (string? x) (!= (string-trim-spaces x) ""))
      (and (pair? x) (in? (car x) '(text:span text:a text:s text:line-break
                                    text:note)))))

(define (odt-blocks-sub l)
  ;; the blocks of the children l; text which is in no paragraph is one
  (let loop ((l l) (text '()) (acc '()))
    (let ((flush (lambda ()
                   (with r (odt-with-floats
                             (lambda () (odt-trim (odt-inlines (reverse text)))))
                     (append (reverse (cdr r))
                             (if (null? (car r)) acc
                                 (cons (cons 'p (car r)) acc)))))))
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

(define (odt-page-layout root)
  ;; the properties of the layout of the first page of styles.xml
  (let* ((masters (and root (ox-child root 'office:master-styles)))
         (master (and masters
                      (or (list-find (ox-childs masters 'style:master-page)
                                     (lambda (m) (== (ox-attr m 'style:name) "Standard")))
                          (ox-child masters 'style:master-page))))
         (name (and master (ox-attr master 'style:page-layout-name)))
         (layouts (append-map (lambda (g) (ox-childs g 'style:page-layout))
                              (if root
                                  (append (ox-childs root 'office:automatic-styles)
                                          (ox-childs root 'office:styles))
                                  '())))
         (layout (and name (list-find layouts
                                      (lambda (l) (== (ox-attr l 'style:name) name))))))
    (and layout (ox-child layout 'style:page-layout-properties))))

(define (odt-body-style)
  ;; the style of most of the plain paragraphs, or the one of the text
  (let ((best #f) (n 0))
    (for (kv (ahash-table->list odt-body-styles))
      (when (> (cdr kv) n) (set! best (car kv)) (set! n (cdr kv))))
    (cond ((and best (!= best "")) best)
          ((ahash-ref odt-styles (cons "paragraph" "Text_20_body")) "Text_20_body")
          (else #f))))

(define (odt-setup styles)
  ;; the page, and the usual paragraph
  (let* ((page (odt-page-layout styles))
         (len (lambda (key)
                (with v (and page (office-length->cm (ox-attr page key)))
                  (and v (string-append (office-decimal v) "cm")))))
         (props (append (odt-style "paragraph" (odt-body-style)) odt-defaults))
         (num (lambda (key) (with v (odt-get props key)
                              (and v (office-decimal v)))))
         (font (odt-get props 'font))
         (attrs `((page-width ,(len 'fo:page-width))
                  (page-height ,(len 'fo:page-height))
                  (landscape ,(and page (== (ox-attr page 'style:print-orientation)
                                            "landscape")
                                   "true"))
                  (margin-top ,(len 'fo:margin-top))
                  (margin-bottom ,(len 'fo:margin-bottom))
                  (margin-left ,(len 'fo:margin-left))
                  (margin-right ,(len 'fo:margin-right))
                  (font-size ,(num 'size))
                  (font ,(cond ((not font) #f)
                               ((office-monospace? font) "mono")
                               ((office-sans? font) "sans")
                               (else #f)))
                  (first-indent ,(num 'first-indent))
                  (space-before ,(num 'space-before))
                  (space-after ,(num 'space-after))
                  (line-spacing ,(num 'line-spacing))
                  (body-style ,(odt-body-style))
                  (align ,(or (odt-align (odt-get props 'align))
                              (and page "left"))))))
    (if (list-or (map cadr attrs)) (list (office-node 'setup attrs)) '())))

(define (odt-master root)
  ;; the first page of styles.xml
  (let ((masters (and root (ox-child root 'office:master-styles))))
    (and masters
         (or (list-find (ox-childs masters 'style:master-page)
                        (lambda (m) (== (ox-attr m 'style:name) "Standard")))
             (ox-child masters 'style:master-page)))))

(define (odt-headers root)
  ;; the headers and the footers of the first page style
  (with master (odt-master root)
    (if (not master) '()
        (append-map
          (lambda (spec)
            (with x (ox-child master (car spec))
              (if (or (not x) (== (ox-attr x 'style:display) "false")) '()
                  (list (apply office-node
                               (cons* 'header `((place ,(cadr spec)) (kind ,(caddr spec)))
                                      (odt-apart (lambda () (odt-blocks (ox-children x))))))))))
          '((style:header "header" "default") (style:footer "footer" "default")
            (style:header-left "header" "even") (style:footer-left "footer" "even")
            (style:header-first "header" "first") (style:footer-first "footer" "first"))))))

(define (odt-style-node key)
  ;; the definition of the named style (family . name)
  (let* ((family (car key))
         (name (cdr key))
         (own (or (ahash-ref odt-styles key) '()))
         (props (odt-style family name))
         (yes (lambda (k) (and (odt-get props k) "true")))
         (num (lambda (k) (with v (odt-get props k) (and v (office-decimal v)))))
         (font (odt-get props 'font))
         (par? (== family "paragraph")))
    (office-node 'style
                 `((name ,name) (display ,(odt-get own 'name))
                   (family ,(if par? "paragraph" "text"))
                   (em ,(yes 'em)) (strong ,(yes 'strong))
                   (underline ,(yes 'underline)) (strike ,(yes 'strike))
                   (smallcaps ,(yes 'smallcaps)) (color ,(odt-get props 'color))
                   (size ,(num 'size))
                   (font ,(cond ((not font) #f) ((office-monospace? font) "mono")
                                ((office-sans? font) "sans") (else #f)))
                   (align ,(and par? (odt-align (odt-get props 'align))))
                   (indent-left ,(and par? (num 'indent-left)))
                   (first-indent ,(and par? (num 'first-indent)))
                   (space-before ,(and par? (num 'space-before)))
                   (space-after ,(and par? (num 'space-after)))
                   (background ,(and par? (odt-get props 'background)))
                   (border ,(and par? (odt-get props 'border) "true"))))))

(define (odt-styles-block)
  (let* ((body (odt-body-style))
         (keys (list-filter (map car (ahash-table->list odt-used-styles))
                            (lambda (k) (not (and (== (car k) "paragraph")
                                                  (== (cdr k) body))))))
         (keys (sort keys (lambda (a b) (string<? (cdr a) (cdr b))))))
    (if (null? keys) '() `((styles ,@(map odt-style-node keys))))))

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

;; A file which cannot be read gives a document which says so, and a
;; message, rather than an empty document.
(define (odt-error message)
  (debug-message "convert-error" (string-append message "\n"))
  `(office (error ,message)))

(tm-define (parse-odt-document s)
  (:type (-> string stree))
  (:synopsis "Read the OpenDocument text @s, a zip archive, into an office tree")
  ;; (what was not foreseen in a file gives a message too, and the state
  ;; of the module is reset)
  (with r (catch #t
            (lambda () (odt-parse s))
            (lambda args
              (odt-error (string-append "This OpenDocument file could not be read "
                                        (office-error-text args)))))
    (set! odt-archive #f)
    (set! odt-styles (make-ahash-table))
    (set! odt-style-memo (make-ahash-table))
    (set! odt-lists (make-ahash-table))
    (set! odt-list-style #f)
    (set! odt-list-depth 0)
    (set! odt-floats '())
    (set! odt-list-formats (make-ahash-table))
    (set! odt-outline (make-ahash-table))
    (set! odt-body-styles (make-ahash-table))
    (set! odt-defaults '())
    (set! odt-list-counts (make-ahash-table))
    (set! odt-used-styles (make-ahash-table))
    (set! odt-changes (make-ahash-table))
    r))

(define (odt-parse s)
  (set! odt-archive (office-archive s))
  (if (not odt-archive) (odt-error "This file is not an OpenDocument text")
      (let ((content (office-xml odt-archive "content.xml")))
        (set! odt-styles (make-ahash-table))
        (set! odt-style-memo (make-ahash-table))
        (set! odt-lists (make-ahash-table))
        (set! odt-list-style #f)
        (set! odt-list-depth 0)
        (set! odt-floats '())
        (set! odt-list-formats (make-ahash-table))
        (set! odt-outline (make-ahash-table))
        (set! odt-body-styles (make-ahash-table))
        (set! odt-defaults '())
        (set! odt-list-counts (make-ahash-table))
        (set! odt-used-styles (make-ahash-table))
        (with styles (office-xml odt-archive "styles.xml")
          (odt-read-styles styles)
          (odt-read-styles content)
          (let* ((text (and content (ox-path content 'office:body 'office:text)))
                 (changes (odt-read-changes text))
                 (blocks (and text (odt-blocks (ox-children text))))
                 (headers (odt-headers styles))
                 (blocks (if blocks (office-drop-style blocks (odt-body-style)) blocks))
                 (r (if text
                        `(office ,@(odt-meta) ,@(odt-setup styles) ,@(odt-styles-block)
                                 ,@headers
                                 (package (@ (format "odt") (data ,s)))
                                 ,@blocks)
                        (odt-error "This OpenDocument file has no text"))))
            r)))))
