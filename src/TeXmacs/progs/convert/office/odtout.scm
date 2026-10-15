
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
(define ot-pictures (make-ahash-table)) ; the bytes of an image -> its files
(define ot-id 0)                      ; the last number given to a thing
(define ot-cell-align #f)             ; the alignment of the cell around
(define ot-numbered? #f)              ; the headings are numbered
(define ot-sequences '())             ; the names of the sequences
(define ot-in-note? #f)               ; inside a note
(define ot-bookmarks (make-ahash-table)) ; the bookmarks which are written
(define ot-used-styles '())           ; (name family) of the named styles

(define ot-text-width 16.0)           ; the width of the text, in centimeters

(define (ot-next-id)
  (set! ot-id (+ ot-id 1))
  ot-id)

(define ot-setup '(setup))  ; the page and the font of the document
(define ot-changes '())     ; the changes which are tracked, reversed
(define ot-package #f)      ; the entries of the file the text was read from

(define (ot-style key prefix make)
  ;; the name of the automatic style of this key; (make name) is its
  ;; element when it is new
  (or (ahash-ref ot-styles key)
      (with name (string-append prefix (number->string (+ (length ot-style-list) 1)))
        (ahash-set! ot-styles key name)
        (set! ot-style-list (cons (make name) ot-style-list))
        name)))

(define (ot-new-bookmark? name)
  ;; whether the bookmark of this name is not in the file yet (a label
  ;; which is twice in the text has one bookmark, at its first place)
  (and (not (ahash-ref ot-bookmarks name))
       (begin (ahash-set! ot-bookmarks name #t) #t)))

(define (ot-named-style name family)
  ;; The name in the file of a named style of the text (family paragraph
  ;; or text), which the styles of the file get if they do not have it.
  ;; A name has letters, digits, - . and _ only: the others are _hh_ (a
  ;; space is _20_, as LibreOffice writes it).
  (if (== name "") ""
      (ot-named-style* name family)))

(define (ot-named-style* name family)
  (let* ((safe (apply string-append
                      (map (lambda (c)
                             (let ((n (char->integer c)))
                               (if (or (char-alphabetic? c) (char-numeric? c)
                                       (in? c '(#\_ #\- #\.)) (>= n 128))
                                   (char->string c)
                                   (string-append "_" (number->string n 16) "_"))))
                           (string->list name))))
         (safe (if (and (> (string-length safe) 0)
                        (let ((c (string-ref safe 0)))
                          (or (char-alphabetic? c) (char=? c #\_)
                              (>= (char->integer c) 128))))
                   safe (string-append "_" safe))))
    (when (and (!= name "") (not (assoc safe ot-used-styles)))
      (set! ot-used-styles (cons (list safe family name) ot-used-styles)))
    safe))

(define ot-source-styles '()) ; the nodes style of the document

(define (ot-source-style name family)
  ;; the node style of this name and family (paragraph or text) which the
  ;; document has, from its macros or from its file of another format
  (list-find ot-source-styles
             (lambda (st) (and (== (ox-attr st 'name) name)
                               (== (ox-attr st 'family) family)))))

(define (ot-style-pt st k)
  (with v (office-number (ox-attr st k))
    (and v (string-append (office-decimal v) "pt"))))

(define (ot-style-paragraph-properties st)
  ;; the attributes of style:paragraph-properties of a node style
  (let* ((get (lambda (k) (ox-attr st k)))
         (line (office-number (get 'line-spacing)))
         (fill (office-color (get 'background)))
         (align (get 'align)))
    (append
      (if (ot-style-pt st 'space-before) `((fo:margin-top ,(ot-style-pt st 'space-before))) '())
      (if (ot-style-pt st 'space-after) `((fo:margin-bottom ,(ot-style-pt st 'space-after))) '())
      (if (ot-style-pt st 'indent-left) `((fo:margin-left ,(ot-style-pt st 'indent-left))) '())
      (if (ot-style-pt st 'indent-right) `((fo:margin-right ,(ot-style-pt st 'indent-right))) '())
      (if (ot-style-pt st 'first-indent) `((fo:text-indent ,(ot-style-pt st 'first-indent))) '())
      (if (and line (> line 0))
          `((fo:line-height ,(string-append (number->string (inexact->exact (round (* 100 line))))
                                            "%")))
          '())
      (cond ((== align "center") '((fo:text-align "center")))
            ((== align "right") '((fo:text-align "end")))
            ((== align "left") '((fo:text-align "start")))
            ((== align "justify") '((fo:text-align "justify")))
            (else '()))
      (if fill `((fo:background-color ,fill)) '())
      (if (get 'border) '((fo:border "0.5pt solid #000000") (fo:padding "0.05cm")) '())
      (with v (or (get 'keep-with-next) (get 'keep-next))
        (if (and v (not (in? v '("false" "no" "0" "auto"))))
            '((fo:keep-with-next "always")) '())))))

(define (ot-style-text-properties st)
  ;; the attributes of style:text-properties of a node style
  (let* ((get (lambda (k) (ox-attr st k)))
         (font (cond ((get 'font-name) => identity)
                     ((== (get 'font) "sans") "Arial")
                     ((== (get 'font) "mono") "Courier New")
                     (else #f)))
         (color (office-color (get 'color)))
         (size (ot-style-pt st 'size))
         (kind (get 'underline-kind)))
    (append
      (if font `((fo:font-family ,(string-append "'" font "'"))) '())
      (if size `((fo:font-size ,size) (style:font-size-asian ,size)
                 (style:font-size-complex ,size)) '())
      (if (get 'strong) '((fo:font-weight "bold") (style:font-weight-asian "bold")
                          (style:font-weight-complex "bold")) '())
      (if (get 'em) '((fo:font-style "italic") (style:font-style-asian "italic")
                      (style:font-style-complex "italic")) '())
      (if (get 'smallcaps) '((fo:font-variant "small-caps")) '())
      (if (== (get 'caps) "true") '((fo:text-transform "uppercase")) '())
      (if color `((fo:color ,color)) '())
      (if (ot-style-pt st 'letters) `((fo:letter-spacing ,(ot-style-pt st 'letters))) '())
      (if (get 'underline)
          `((style:text-underline-style
              ,(cond ((in? kind '("wave" "dotted" "dash")) kind)
                     ((in? kind '("wavyHeavy" "wavyDouble")) "wave")
                     ((in? kind '("dottedHeavy" "dotDash" "dotDotDash")) "dotted")
                     ((in? kind '("dashLong" "dashedHeavy" "dashLongHeavy")) "dash")
                     (else "solid")))
            ,@(if (== kind "double") '((style:text-underline-type "double")) '())
            (style:text-underline-width "auto") (style:text-underline-color "font-color"))
          '())
      (if (get 'strike)
          `((style:text-line-through-style "solid")
            ,@(if (get 'dstrike) '((style:text-line-through-type "double")) '()))
          '()))))

(define (ot-style-of-node u st)
  ;; the named style u (name family display) with the properties of the
  ;; node style st (#f for none), after the usual one
  (let ((par? (== (cadr u) "paragraph"))
        (display (or (and st (ox-attr st 'display)) (caddr u))))
    `(style:style
       (@ (style:name ,(car u)) (style:family ,(cadr u))
          ,@(if (!= display (car u)) `((style:display-name ,display)) '())
          ,@(if par? '((style:parent-style-name "Standard") (style:class "text")) '()))
       ,@(with l (if (and st par?) (ot-style-paragraph-properties st) '())
           (if (null? l) '() `((style:paragraph-properties (@ ,@l)))))
       ,@(with l (if st (ot-style-text-properties st) '())
           (if (null? l) '() `((style:text-properties (@ ,@l))))))))

(define (ot-heading-of-node y st)
  ;; the style of headings y in the look of the node style st of the
  ;; document (a heading of the file of another format)
  `(style:style
     (@ ,@(ox-attrs y))
     ,@(with l (ot-style-paragraph-properties st)
         (if (null? l) '() `((style:paragraph-properties (@ ,@l)))))
     (style:text-properties
       (@ ,@(ot-style-text-properties st)
          ;; (what the style of the file does not say is the usual)
          ,@(if (ox-attr st 'strong) '() '((fo:font-weight "normal")))
          ,@(if (ox-attr st 'em) '() '((fo:font-style "normal")))))))

(define (ot-complete-styles styles)
  ;; The styles of the file, with the named styles which the text has and
  ;; they do not: a style of this name, in the look which the document
  ;; says, after the usual one. Without a file of OpenDocument, the
  ;; headings have the look of those of the document.
  (let* ((office (ox-child styles 'office:styles))
         (names (if office
                    (map (lambda (y) (or (ox-attr y 'style:name) ""))
                         (ox-childs office 'style:style))
                    '()))
         (new (list-filter (reverse ot-used-styles)
                           (lambda (u) (not (in? (car u) names)))))
         (headings
           (if ot-package '()
               (list-filter
                 (map (lambda (st)
                        (with n (office-integer (ox-attr st 'level) 0 0 9)
                          (and (>= n 1) (== (ox-attr st 'family) "paragraph")
                               (cons (string-append "Heading_20_" (number->string n)) st))))
                      ot-source-styles)
                 identity)))
         (fix (lambda (y)
                (with h (and (func? y 'style:style)
                             (assoc (ox-attr y 'style:name) headings))
                  (if h (ot-heading-of-node y (cdr h)) y)))))
    (if (or (and (null? new) (null? headings)) (not office)) styles
        (map (lambda (y)
               (if (func? y 'office:styles)
                   (append (map fix y)
                           (map (lambda (u)
                                  (ot-style-of-node
                                    u (ot-source-style (caddr u)
                                                       (if (== (cadr u) "paragraph")
                                                           "paragraph" "text"))))
                                new))
                   y))
             styles))))

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
  (let* ((tags (list-filter '(code strong em smallcaps strike underline sub sup mark
                              caps dstrike overline)
                            (lambda (t) (in? t props))))
         (ukind (with c (assq 'ukind (list-filter props pair?)) (and c (cadr c))))
         (tcase (with c (assq 'tcase (list-filter props pair?)) (and c (cadr c))))
         (letters (with c (assq 'letters (list-filter props pair?)) (and c (cadr c))))
         (color (with c (assq 'color (list-filter props pair?)) (and c (cadr c))))
         (parent (with c (assq 'rstyle (list-filter props pair?)) (and c (cadr c))))
         (size (with c (assq 'size (list-filter props pair?)) (and c (cadr c))))
         (font (with c (assq 'font (list-filter props pair?)) (and c (cadr c))))
         (shade (with c (assq 'shade (list-filter props pair?)) (and c (cadr c))))
         (key (cons 'text (cons* parent size font shade ukind letters tcase (cons color tags)))))
    (cond ((and parent (null? tags) (not (or color size font shade ukind letters tcase))) parent)
          ((not (or (pair? tags) color size font shade ukind letters tcase)) #f)
          (else
         (ot-style
           key "T"
           (lambda (name)
             `(style:style
                (@ (style:name ,name) (style:family "text")
                   ,@(if parent `((style:parent-style-name ,parent)) '()))
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
                           `((style:text-underline-style
                               ,(cond ((in? ukind '("wave" "dotted" "dash")) ukind)
                                      ((in? ukind '("dashLong" "dashedHeavy")) "dash")
                                      ((in? ukind '("wavyHeavy" "wavyDouble")) "wave")
                                      ((in? ukind '("dottedHeavy" "dotDash" "dotDotDash")) "dotted")
                                      (else "solid")))
                             (style:text-underline-width
                               ,(if (in? ukind '("thick" "dashedHeavy" "wavyHeavy" "dottedHeavy"))
                                    "bold" "auto"))
                             ,@(if (in? ukind '("double" "wavyDouble"))
                                   '((style:text-underline-type "double")) '())
                             (style:text-underline-color "font-color"))
                           '())
                     ,@(if (in? 'overline tags)
                           '((style:text-overline-style "solid")
                             (style:text-overline-width "auto")
                             (style:text-overline-color "font-color"))
                           '())
                     ,@(if (in? 'strike tags)
                           `((style:text-line-through-style "solid")
                             (style:text-line-through-type
                               ,(if (in? 'dstrike tags) "double" "single")))
                           '())
                     ,@(cond ((in? 'caps tags) '((fo:text-transform "uppercase")))
                             ((== tcase "lower") '((fo:text-transform "lowercase")))
                             ((== tcase "capital") '((fo:text-transform "capitalize")))
                             (else '()))
                     ,@(if letters `((fo:letter-spacing ,(string-append letters "pt"))) '())
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
                     ,@(if shade `((fo:background-color ,shade)) '())
                     ,@(if size `((fo:font-size ,(string-append size "pt"))) '())
                     ,@(if font
                           (with f (cond ((== font "sans") "Arial")
                                         ((== font "serif") "Times New Roman")
                                         ((== font "mono") "Courier New")
                                         (else font))
                             `((fo:font-family ,(string-append "'" f "'"))))
                           '())
                     ,@(if color `((fo:color ,color)) '()))))))))))

(define (ot-span props l)
  ;; the nodes l as the text inside the wrappers props
  (with style (ot-text-style props)
    (cond ((null? l) '())
          (style (list `(text:span (@ (text:style-name ,style)) ,@l)))
          (else l))))

(define (ot-image-files x n)
  ;; The names (bitmap svg) of the files of the picture x, the thing of
  ;; number n, in Pictures; svg is #f when it has none. A picture which is
  ;; used several times is there once.
  (let* ((data (ox-attr x 'data))
         (svg (ox-attr x 'svg))
         (key (cons data (or svg "")))
         (name (or (ox-attr x 'name) "image.png"))
         (dot (let loop ((i (- (string-length name) 1)))
                (cond ((< i 0) #f)
                      ((char=? (string-ref name i) #\.) i)
                      (else (loop (- i 1))))))
         (suffix (if dot (locase-all (substring name (+ dot 1) (string-length name)))
                     "png"))
         (type (cond ((in? suffix '("jpg" "jpeg")) "image/jpeg")
                     ((== suffix "gif") "image/gif")
                     (else "image/png"))))
    (or (ahash-ref ot-pictures key)
        (let* ((base (if ot-package "Pictures/tm-image" "Pictures/image"))
               (file (string-append base (number->string n) "." suffix))
               (svg-file (and svg (string-append base (number->string n) ".svg"))))
          (set! ot-files (cons (list file type data) ot-files))
          (when svg
            (set! ot-files (cons (list svg-file "image/svg+xml" svg) ot-files)))
          (ahash-set! ot-pictures key (list file svg-file))
          (list file svg-file)))))

(define (ot-image x)
  (let* ((data (ox-attr x 'data))
         (n (ot-next-id))
         (w (or (office-length->cm (ox-attr x 'width)) 5.0))
         (h (or (office-length->cm (ox-attr x 'height)) 3.0))
         ;; the parts of the picture which are cut, as fo:clip says them:
         ;; in the sizes of the picture itself
         (c (with s (ox-attr x 'crop)
              (and s (map (lambda (v) (or (office-number v) 0))
                          (string-tokenize-by-char s #\space)))))
         (c (and c (== (length c) 4) c))
         (own (and c data (office-image-cm data)))
         (clip (and own
                    (with l (list (* (cadr c) (cadr own)) (* (caddr c) (car own))
                                  (* (cadddr c) (cadr own)) (* (car c) (car own)))
                      (string-append "rect(" (string-recompose (map ot-cm l) ", ") ")"))))
         (float? (ox-attr x 'float))
         (style (if (or clip float?) (ot-frame-style x clip) "fr1"))
         (anchor (cond ((not float?) "as-char")
                       ((or (== (ox-attr x 'h-rel) "character")
                            (== (ox-attr x 'v-rel) "line"))
                        "char")
                       (else "paragraph")))
         (offset (lambda (k) (with v (ox-attr x k)
                               (if (and (string? v) (!= v "")) v "0cm")))))
    (if (not (or data (office-web-address? (ox-attr x 'name)))) '()
        ;; (a picture on the web: a frame which links to its address)
        (let* ((files (if data (ot-image-files x n) (list (ox-attr x 'name) #f)))
               (file (car files))
               (svg (cadr files))
               (svg-file (cadr files)))
          (list `(draw:frame
                   (@ (draw:style-name ,style)
                      (draw:name ,(string-append "Image" (number->string n)))
                      (text:anchor-type ,anchor)
                      ,@(if (and float? (== (ox-attr x 'h-pos) "offset"))
                            `((svg:x ,(offset 'h-offset))) '())
                      ,@(if (and float? (== (ox-attr x 'v-pos) "offset"))
                            `((svg:y ,(offset 'v-offset))) '())
                      (svg:width ,(ot-cm w)) (svg:height ,(ot-cm h))
                      (draw:z-index ,(if (== (ox-attr x 'wrap) "behind") "0" "1")))
                   ;; the images of a frame are the same picture: the
                   ;; first one which a program can show is shown
                   ,@(if svg
                         `((draw:image (@ (xlink:href ,svg-file) (xlink:type "simple")
                                          (xlink:show "embed") (xlink:actuate "onLoad"))))
                         '())
                   (draw:image (@ (xlink:href ,file) (xlink:type "simple")
                                  (xlink:show "embed") (xlink:actuate "onLoad")))
                   ,@(if (ox-attr x 'alt) `((svg:title ,(ox-attr x 'alt))) '())))))))

(define (ot-box x)
  ;; a box of text (a frame with a text box) or a shape (with its text), in
  ;; its line or floating beside its paragraph
  (let* ((n (ot-next-id))
         (float? (ox-attr x 'float))
         (style (ot-frame-style x #f))
         (anchor (cond ((not float?) "as-char")
                       ((or (== (ox-attr x 'h-rel) "character")
                            (== (ox-attr x 'v-rel) "line"))
                        "char")
                       (else "paragraph")))
         (offset (lambda (k) (with v (ox-attr x k)
                               (if (and (string? v) (!= v "")) v "0cm"))))
         (w (or (office-length->cm (ox-attr x 'width)) 2.0))
         (h (or (office-length->cm (ox-attr x 'height)) 1.0))
         (shape (ox-attr x 'shape))
         (attrs `((draw:style-name ,style)
                  (draw:name ,(string-append "Box" (number->string n)))
                  (text:anchor-type ,anchor)
                  ,@(if (and float? (== (ox-attr x 'h-pos) "offset"))
                        `((svg:x ,(offset 'h-offset))) '())
                  ,@(if (and float? (== (ox-attr x 'v-pos) "offset"))
                        `((svg:y ,(offset 'v-offset))) '())
                  (svg:width ,(ot-cm w)) (svg:height ,(ot-cm h))
                  (draw:z-index ,(if (== (ox-attr x 'wrap) "behind") "0" "1"))))
         (blocks (ot-blocks-with (ox-children x) "Standard")))
    (if (and (pair? (ox-children x)) (in? shape '(#f "rect")))
        (list `(draw:frame (@ ,@attrs)
                 (draw:text-box (@ (fo:min-height ,(ot-cm h))) ,@blocks)))
        (list `(draw:custom-shape (@ ,@attrs)
                 ,@blocks
                 (draw:enhanced-geometry
                   (@ (svg:viewBox "0 0 21600 21600")
                      (draw:type ,(office-shape-for-odf shape)))))))))

(define (ot-frame-style x clip)
  ;; the style of the frame of a picture which floats, or which is cut
  (let* ((get (lambda (k) (ox-attr x k)))
         (float? (get 'float))
         (wrap (or (get 'wrap) "square"))
         (side (or (assoc-ref '(("both" . "parallel") ("left" . "left") ("right" . "right")
                                ("largest" . "dynamic"))
                              (or (get 'wrap-side) ""))
                   "parallel"))
         (h-pos (or (get 'h-pos) (get 'float) "left"))
         (v-pos (or (get 'v-pos) "top"))
         (props
           (if (not float?)
               '((style:vertical-pos "top") (style:vertical-rel "baseline"))
               `((style:wrap ,(cond ((in? wrap '("square" "tight")) side)
                                    ((in? wrap '("through" "front" "behind")) "run-through")
                                    (else "none")))
                 ,@(if (in? wrap '("square" "tight"))
                       '((style:number-wrapped-paragraphs "no-limit")) '())
                 ,@(if (== wrap "tight") '((style:wrap-contour "true")
                                           (style:wrap-contour-mode "outside"))
                       '())
                 ,@(if (in? wrap '("through" "front" "behind"))
                       `((style:run-through ,(if (== wrap "behind") "background" "foreground")))
                       '())
                 (style:horizontal-pos
                   ,(cond ((in? h-pos '("left" "right" "center" "inside" "outside")) h-pos)
                          (else "from-left")))
                 (style:horizontal-rel
                   ,(or (assoc-ref '(("column" . "paragraph") ("page" . "page")
                                     ("margin" . "page-content")
                                     ("left-margin" . "page-start-margin")
                                     ("right-margin" . "page-end-margin")
                                     ("character" . "char"))
                                   (or (get 'h-rel) ""))
                        "paragraph"))
                 (style:vertical-pos
                   ,(cond ((in? v-pos '("top" "bottom")) v-pos)
                          ((== v-pos "center") "middle")
                          ((== v-pos "inside") "top")
                          ((== v-pos "outside") "bottom")
                          (else "from-top")))
                 (style:vertical-rel
                   ,(or (assoc-ref '(("paragraph" . "paragraph") ("line" . "line")
                                     ("page" . "page") ("margin" . "page-content")
                                     ("top-margin" . "page") ("bottom-margin" . "page"))
                                   (or (get 'v-rel) ""))
                        "paragraph"))
                 ,@(if (in? wrap '("square" "tight" "through"))
                       '((fo:margin-left "0.32cm") (fo:margin-right "0.32cm"))
                       '()))))
         (props (append props (if clip `((fo:clip ,clip)) '())))
         ;; (the fill and the line of a box or a shape)
         (frame? (and (func? x 'box) (pair? (ox-children x))
                      (in? (ox-attr x 'shape) '(#f "rect"))))
         (fill (ox-attr x 'fill))
         (line (ox-attr x 'line))
         (lw (with v (office-number (ox-attr x 'line-width))
               (string-append (office-decimal (if (and v (> v 0)) v 0.75)) "pt")))
         (props (append
                  props
                  (cond ((not (func? x 'box)) '())
                        ((== fill "none") '((draw:fill "none")))
                        ((office-color fill)
                         => (lambda (c) `((draw:fill "solid") (draw:fill-color ,c)
                                          ,@(if frame? `((fo:background-color ,c)) '()))))
                        (else '()))
                  (cond ((not (func? x 'box)) '())
                        ((== line "none")
                         (if frame? '((fo:border "none")) '((draw:stroke "none"))))
                        ((office-color line)
                         => (lambda (c)
                              (if frame?
                                  `((fo:border ,(string-append lw " solid " c)))
                                  `((draw:stroke "solid") (svg:stroke-color ,c)
                                    (svg:stroke-width ,lw)))))
                        (else '()))))
         (keep (lambda (k) (assq k props))))
    (ot-style (cons 'frame props) "frm"
              (lambda (name)
                `(style:style
                   (@ (style:name ,name) (style:family "graphic")
                      (style:parent-style-name "Graphics"))
                   (style:graphic-properties
                     (@ ,@props
                        ,@(if (keep 'fo:border) '() '((fo:border "none")))
                        ,@(cond ((not (func? x 'box)) '((fo:padding "0cm")))
                                ((ot-insets (ox-attr x 'inset)) => identity)
                                (else '((fo:padding "0.1cm"))))
                        ,@(if (== (ox-attr x 'grow) "true")
                              '((draw:auto-grow-height "true")) '()))))))))

(define (ot-insets s)
  ;; the paddings of a box for its space around its text, "l,t,r,b" in cm
  (let ((l (and s (map office-number (string-tokenize-by-char s #\,)))))
    (and l (== (length l) 4) (list-and l)
         (map (lambda (k v) (list k (ot-cm v)))
              '(fo:padding-left fo:padding-top fo:padding-right fo:padding-bottom) l))))

(define (ot-local-name tag)
  (let* ((s (symbol->string tag))
         (i (string-search-forwards ":" 0 s)))
    (string->symbol (if (>= i 0) (substring s (+ i 1) (string-length s)) s))))

;; LibreOffice reads the MathML of a formula into its own language of
;; formulas, and not all of MathML comes through: a name which is a word
;; of this language (in, sup, over) is an error, as is a relation without
;; something at each of its sides; a fraction has a bar always; a hat is
;; only shown as the letter-like circumflex. What follows writes these
;; with what is shown well.

(define ot-math-functions
  ;; the names which are functions of the language of LibreOffice too
  '("sin" "cos" "tan" "cot" "sinh" "cosh" "tanh" "coth" "arcsin" "arccos"
    "arctan" "arccot" "ln" "log" "exp" "lim"))

(define (ot-math-word? s)
  ;; a word: two letters of ASCII or more
  (and (> (string-length s) 1)
       (list-and (map (lambda (c) (and (< (char->integer c) 128)
                                       (char-alphabetic? c)))
                      (string->list s)))))

(define (ot-math-operator? x signs?)
  ;; an operator which needs something at its side; the signs, which may
  ;; have nothing before them, are such ones when signs? is #t
  (and (func? x 'mo) (not (ox-attr x 'form))
       (or signs?
           (not (in? (ox-text x)
                     (list "+" "-" (office-utf8 #x2212) (office-utf8 #xb1)
                           (office-utf8 #x2213) (office-utf8 #xac)))))))

(define (ot-math-operands l)
  ;; the elements l of a row, with an empty row for what an operator at
  ;; its start or at its end applies to
  (let* ((l (if (and (pair? l) (ot-math-operator? (car l) #f)) (cons '(mrow) l) l))
         (l (if (and (pair? l) (ot-math-operator? (cAr l) #t))
                (append l (list '(mrow)))
                l)))
    l))

(define (ot-plain-mathml x)
  ;; a tree of MathML without the prefixes of its elements, as LibreOffice
  ;; shows it well
  (cond ((not (pair? x)) x)
        ((func? x '@) x)
        (else
          (let* ((name (ot-local-name (car x)))
                 (c (ox-children x))
                 (text (and (list-1? c) (string? (car c)) (car c))))
            (cond
              ;; the brackets grow with what they enclose; they are said
              ;; to be brackets, or a bar is an error
              ((and (== name 'mo) (in? (ox-attr x 'form) '("prefix" "postfix"))
                    (not (ox-attr x 'stretchy)))
               `(mo (@ ,@(ox-attrs x)
                       ,@(if (ox-attr x 'fence) '() '((fence "true")))
                       (stretchy "true"))
                    ,@c))
              ;; a name which is a word, and a bar which is no bracket: text
              ((and (in? name '(mi mo)) text
                    (or (== text "|")
                        (and (ot-math-word? text) (not (in? text ot-math-functions)))))
               `(mtext ,text))
              ;; a fraction without a bar: one above the other
              ((and (== name 'mfrac) (== (length (ox-elements x)) 2)
                    (in? (ox-attr x 'linethickness) '("0" "0pt" "0px")))
               `(mtable ,@(map (lambda (y) `(mtr (mtd ,(ot-plain-mathml y))))
                               (ox-elements x))))
              ;; a hat
              ((and (== name 'mover) (== (length c) 2) (pair? (cadr c))
                    (== (ot-local-name (car (cadr c))) 'mo)
                    (== (ox-children (cadr c)) '("^")))
               `(mover ,(ot-plain-mathml (car c)) (mo ,(office-utf8 #x2c6))))
              (else
                (with l (map ot-plain-mathml (cdr x))
                  (cons name
                        (if (in? name '(math mrow mtd mstyle msqrt))
                            ;; (the attributes come first)
                            (if (and (pair? l) (func? (car l) '@))
                                (cons (car l) (ot-math-operands (cdr l)))
                                (ot-math-operands l))
                            l)))))))))

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
                                    (display ,(if display? "block" "inline"))
                                    ;; (lines of equations: see oftm-eqarray)
                                    ,@(if (== (ox-attr (car c) 'class) "tm-eqarray")
                                          '((class "tm-eqarray")) '()))
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

(define (ot-note-inline x)
  ;; a note inside a note, which OpenDocument does not have: its text in
  ;; parentheses where it is
  (let* ((parts (map (lambda (b)
                       (if (func? b 'p) (ot-inlines (ox-children b) '())
                           (with t (ox-text b) (if (== t "") '() (ot-text t)))))
                     (ox-children x)))
         (parts (list-filter parts pair?)))
    (if (null? parts) '()
        (append (ot-text " (") (apply append (list-intersperse parts (ot-text " ")))
                (ot-text ")")))))

(define (ot-note x)
  (if ot-in-note? (ot-note-inline x) (ot-note* x)))

(define (ot-note* x)
  (let* ((n (ot-next-id))
         (old ot-cell-align))
    (set! ot-cell-align #f)
    (set! ot-in-note? #t)
    (with blocks (ot-blocks-with (ox-children x) "Footnote")
      (set! ot-in-note? #f)
      (set! ot-cell-align old)
      (list `(text:note
               (@ (text:id ,(string-append "ftn" (number->string n)))
                  (text:note-class "footnote"))
               (text:note-citation ,(number->string n))
               (text:note-body ,@(if (null? blocks)
                                     '((text:p (@ (text:style-name "Footnote"))))
                                     blocks)))))))

(define (ot-seq x props)
  ;; A number of a sequence, which the program counts, inside the
  ;; bookmarks of its labels, so that a reference to one of them shows
  ;; this number.
  (let* ((name (ox-attr x 'name))
         (labels (list-filter (office-labels x) ot-new-bookmark?))
         (inner (ot-inlines (ox-children x) props))
         (number (if (and name (!= name ""))
                     (begin
                       (when (not (in? name ot-sequences))
                         (set! ot-sequences (cons name ot-sequences)))
                       (ot-span props
                                (list `(text:sequence
                                         (@ (text:name ,name)
                                            (text:formula ,(string-append "ooow:" name "+1"))
                                            (style:num-format "1"))
                                         ,@(ox-children x)))))
                     inner)))
    (append (map (lambda (l) `(text:bookmark-start (@ (text:name ,l)))) labels)
            number
            (map (lambda (l) `(text:bookmark-end (@ (text:name ,l)))) (reverse labels)))))

(define (ot-ref x props)
  ;; a reference: to the text of the bookmark of a number, to the number
  ;; of a heading, or else a link
  (let* ((name (or (ox-attr x 'name) ""))
         (kind (ox-attr x 'kind))
         (inner (ot-inlines (ox-children x) (cons 'link props))))
    (cond ((or (== name "") (null? inner)) inner)
          ;; (a link has no link inside it: its text only)
          ((in? 'link props) inner)
          ;; the page of the label, which the program computes
          ((== kind "page")
           (list `(text:bookmark-ref
                    (@ (text:reference-format "page") (text:ref-name ,name))
                    ,@inner)))
          ((in? kind '("seq" "heading"))
           (list `(text:bookmark-ref
                    (@ (text:reference-format ,(if (== kind "heading") "number" "text"))
                       (text:ref-name ,name))
                    ,@inner)))
          (else (list `(text:a (@ (xlink:type "simple")
                                  (xlink:href ,(string-append "#" name)))
                               ,@inner))))))

(define (ot-inline x props)
  ;; the nodes of a paragraph for an inline node inside the wrappers props
  (cond ((string? x) (if (== x "") '() (ot-span props (ot-text x))))
        ((not (pair? x)) '())
        (else
          (let ((l (ox-children x)))
            (case (car x)
              ((em strong sub sup code smallcaps overline)
               (ot-inlines l (cons (car x) props)))
              ;; capitals, lower case, first letters in capitals
              ((caps)
               (ot-inlines l (if (in? (ox-attr x 'value) '("lower" "capital"))
                                 (cons (list 'tcase (ox-attr x 'value)) props)
                                 (cons 'caps props))))
              ;; an underline or a line through of a kind, letters spaced
              ((underline)
               (ot-inlines l (if (ox-attr x 'value)
                                 (cons* (list 'ukind (ox-attr x 'value)) 'underline props)
                                 (cons 'underline props))))
              ((strike)
               (ot-inlines l (if (== (ox-attr x 'value) "double")
                                 (cons* 'dstrike 'strike props)
                                 (cons 'strike props))))
              ((letters)
               (ot-inlines l (if (ox-attr x 'value)
                                 (cons (list 'letters (ox-attr x 'value)) props)
                                 props)))
              ;; highlighted text, of a color; the size and the font of a text
              ((mark)
               (with c (office-color (ox-attr x 'value))
                 (ot-inlines l (if (and c (!= c "#ffff00")) (cons (list 'shade c) props)
                                   (cons 'mark props)))))
              ((size font)
               (ot-inlines l (if (ox-attr x 'value)
                                 (cons (list (car x) (ox-attr x 'value)) props)
                                 props)))
              ;; a named style of text
              ((span) (ot-inlines l (cons (list 'rstyle (ot-named-style (or (ox-attr x 'style) "")
                                                                        "text"))
                                          props)))
              ;; a field of OpenDocument, with its result; the result only of
              ;; another one
              ((field)
               (let ((inner (ot-inlines l props))
                     (el (and (ox-attr x 'xml)
                              (catch #t (lambda () (ox-root (parse-xml (ox-attr x 'xml))))
                                     (lambda args #f)))))
                 (if (and el (pair? el) (symbol? (car el)))
                     (list (append el (append-map (lambda (y)
                                                    (if (func? y 'text:span) (ox-children y)
                                                        (list y)))
                                                  inner)))
                     inner)))
              ((comment)
               (list `(office:annotation
                        (dc:creator ,(or (ox-attr x 'author) "TeXmacs"))
                        ,@(if (ox-attr x 'date) `((dc:date ,(ox-attr x 'date))) '())
                        ,@(ot-blocks-with (ox-children x) "Standard"))))
              ((ins del) (ot-change x props))
              ((color)
               (ot-inlines l (if (office-color (ox-attr x 'value))
                                 (cons (list 'color (office-color (ox-attr x 'value)))
                                       props)
                                 props)))
              ((seq) (ot-seq x props))
              ((ref) (ot-ref x props))
              ((link)
               (let ((href (or (ox-attr x 'href) ""))
                     (inner (ot-inlines l (cons 'link props))))
                 (cond ((null? inner) '())
                       ;; (a link has no link inside it: its text only)
                       ((in? 'link props) inner)
                       ((in? href '("" "#")) inner)
                       (else (list `(text:a (@ (xlink:type "simple")
                                               (xlink:href
                                                 ,(if (string-starts? href "#") href
                                                      (office-uri href)))
                                               (text:style-name "Internet_20_link")
                                               (text:visited-style-name
                                                 "Visited_20_Internet_20_Link"))
                                            ,@inner))))))
              ((bookmark)
               (with name (string-append (if (ox-attr x 'at) "_TmAt_" "")
                                         (or (ox-attr x 'name) ""))
                 (if (ot-new-bookmark? name)
                     (list `(text:bookmark (@ (text:name ,name))))
                     '())))
              ((note) (ot-note x))
              ((br) '((text:line-break)))
              ((tab) '((text:tab)))
              ((pagebreak) '())
              ((image) (ot-image x))
              ((box) (ot-box x))
              ((math) (ot-math x))
              (else (ot-inlines l props)))))))

(define (ot-change x props)
  ;; a change which is tracked: an insertion between its marks, or the
  ;; place of a deletion, whose text is in the list of the changes
  (let* ((id (string-append "ct" (number->string (+ (length ot-changes) 1))))
         (inner (ot-inlines (ox-children x) props))
         (info `(office:change-info
                  (dc:creator ,(or (ox-attr x 'author) "TeXmacs"))
                  (dc:date ,(or (ox-attr x 'date) "1970-01-01T00:00:00")))))
    (set! ot-changes
          (cons `(text:changed-region
                   (@ (text:id ,id) (xml:id ,id))
                   ,(if (func? x 'ins) `(text:insertion ,info)
                        `(text:deletion ,info (text:p ,@inner))))
                ot-changes))
    (if (func? x 'ins)
        `((text:change-start (@ (text:change-id ,id))) ,@inner
          (text:change-end (@ (text:change-id ,id))))
        `((text:change (@ (text:change-id ,id)))))))

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
    ("definition" . "Definition_20_Definition")
    ("theorem" . "Theorem") ("remark" . "Remark") ("proof" . "Proof")
    ("bibitem" . "Bibliography")))

(define (ot-direct-properties x)
  ;; the properties of paragraphs for the formatting by hand of the node x
  (let ((pt (lambda (k) (with v (office-number (ox-attr x k))
                          (and v (string-append (office-decimal v) "pt"))))))
    (append
      (if (pt 'indent-left) `((fo:margin-left ,(pt 'indent-left))) '())
      (if (pt 'indent-right) `((fo:margin-right ,(pt 'indent-right))) '())
      (if (pt 'first-indent) `((fo:text-indent ,(pt 'first-indent))) '())
      (if (pt 'space-before) `((fo:margin-top ,(pt 'space-before))) '())
      (if (pt 'space-after) `((fo:margin-bottom ,(pt 'space-after))) '())
      (with v (office-number (ox-attr x 'line-spacing))
        (if (and v (> v 0))
            `((fo:line-height ,(string-append (number->string (inexact->exact (round (* 100 v))))
                                              "%")))
            '()))
      (with c (office-color (ox-attr x 'background))
        (if c `((fo:background-color ,c)) '()))
      (if (ox-attr x 'border) '((fo:border "0.5pt solid #000000") (fo:padding "0.1cm")) '()))))

(define (ot-paragraph-style base align break? rule? . direct)
  ;; the name of the style of a paragraph: the style base, or one which
  ;; is made of it with an alignment, a page break before, a line under,
  ;; and the properties direct of its formatting by hand
  (if (not (or align break? rule? (and (pair? direct) (pair? (car direct))))) base
      (ot-style
        (list 'paragraph base align break? rule? (if (pair? direct) (car direct) '())) "P"
        (lambda (name)
          `(style:style
             (@ (style:name ,name) (style:family "paragraph")
                (style:parent-style-name ,base))
             (style:paragraph-properties
               (@ ,@(cond ((== align "center") '((fo:text-align "center")))
                          ((== align "right") '((fo:text-align "end")))
                          ((== align "left") '((fo:text-align "start")))
                          ((== align "justify") '((fo:text-align "justify")))
                          (else '()))
                  ,@(if break? '((fo:break-before "page")) '())
                  ,@(if rule?
                        '((fo:border-bottom "0.5pt solid #000000")
                          (fo:padding "0.05cm"))
                        '())
                  ,@(if (pair? direct) (car direct) '()))))))))

(define (ot-paragraph x default)
  (let* ((role (ox-attr x 'role))
         (level (or (and (== role "heading") (string->number (or (ox-attr x 'level) "1")))
                    #f))
         (base (cond (level (string-append "Heading_20_" (number->string level)))
                     ((assoc-ref ot-role-styles role) => identity)
                     ;; a named style of the file, or the one of its usual
                     ;; paragraphs
                     ((ox-attr x 'style)
                      => (lambda (n) (ot-named-style n "paragraph")))
                     ((and (== default "Text_20_body") (ox-attr ot-setup 'body-style))
                      (ot-named-style (ox-attr ot-setup 'body-style) "paragraph"))
                     (else default)))
         ;; a formula on its own lines is centered
         (formula? (with c (ox-children x)
                     (and (pair? c) (func? (car c) 'math)
                          (== (ox-attr (car c) 'display) "true"))))
         (style (ot-paragraph-style
                  base
                  (or (ox-attr x 'align) ot-cell-align (and formula? "center"))
                  #f #f (ot-direct-properties x)))
         (l (append
              ;; the bookmarks of the labels of a heading
              (map (lambda (name) `(text:bookmark (@ (text:name ,name))))
                   (list-filter (office-labels x) ot-new-bookmark?))
              (ot-inlines (ox-children x) '()))))
    (if level
        (list `(text:h (@ (text:style-name ,style)
                          (text:outline-level ,(number->string level))
                          ;; a heading without a number among numbered ones
                          ,@(if (and ot-numbered? (not (ox-attr x 'number)))
                                '((text:is-list-header "true"))
                                '()))
                       ,@l))
        (list `(text:p (@ (text:style-name ,style)) ,@l)))))

(define (ot-list-style kind)
  ;; The name of the style of a list of this kind, "b" for bullets, or the
  ;; way of numbering ("1", "a", "A", "i", "I"). A list inside another one
  ;; has its own style, of its own kind, of which it takes the level of its
  ;; depth: so the style has all the levels.
  (ot-style
    (list 'list kind) "L"
    (lambda (name)
      `(text:list-style
         (@ (style:name ,name))
         ,@(map (lambda (i)
                  (let ((props `(style:list-level-properties
                                    (@ (text:list-level-position-and-space-mode
                                         "label-alignment"))
                                    (style:list-level-label-alignment
                                      (@ (text:label-followed-by "listtab")
                                         (fo:text-indent "-0.635cm")
                                         (fo:margin-left ,(ot-cm (* 1.27 (+ i 1)))))))))
                    (if (!= kind "b")
                        `(text:list-level-style-number
                           (@ (text:level ,(number->string (+ i 1)))
                              (style:num-suffix ,(if (in? kind '("a" "A")) ")" "."))
                              (style:num-format ,kind))
                           ,props)
                        `(text:list-level-style-bullet
                           (@ (text:level ,(number->string (+ i 1)))
                              (text:bullet-char
                                ,(office-utf8 (list-ref '(#x2022 #x25e6 #x25aa)
                                                        (modulo i 3)))))
                           ,props))))
                (iota 9))))))

;; An item of a list holds paragraphs and lists, and no table: a list ends
;; before a table of one of its items and goes on after it, with the next
;; number. What follows the table in its item is in an item without a
;; number (text:list-header).

(define (ot-item-parts item)
  ;; the elements of an item: (elements table elements ... elements), the
  ;; lists of those which an item holds and, between them, the tables
  (let loop ((l (append-map (lambda (b)
                              (if (func? b 'list) (ot-list b)
                                  (ot-block b "Text_20_body")))
                            (ox-children item)))
             (cur '()) (acc '()))
    (cond ((null? l) (reverse (cons (reverse cur) acc)))
          ((in? (car (car l)) '(table:table text:table-of-content))
           (loop (cdr l) '() (cons* (car l) (reverse cur) acc)))
          (else (loop (cdr l) (cons (car l) cur) acc)))))

(define (ot-list x)
  ;; the elements of a list: the list, or its pieces with the tables of
  ;; its items between them
  (let* ((style (ot-list-style (if (== (ox-attr x 'kind) "number")
                                   (with f (ox-attr x 'format)
                                     (if (in? f '("a" "A" "i" "I")) f "1"))
                                   "b")))
         (start (office-integer (ox-attr x 'start) 1 0 100000))
         ;; the piece with these items, reversed, after the elements out
         (close (lambda (rows out)
                  (if (null? rows) out
                      (cons `(text:list
                               (@ (text:style-name ,style)
                                  ,@(if (null? out) '()
                                        '((text:continue-numbering "true"))))
                               ,@(reverse rows))
                            out)))))
    (let loop ((items (list-filter (ox-children x) (lambda (y) (func? y 'item))))
               (rows '()) (out '()))
      (if (null? items)
          (reverse (close rows out))
          (let* ((parts (ot-item-parts (car items)))
                 ;; (an item which starts with a table still has its number)
                 (first (if (and (null? (car parts)) (pair? (cdr parts)))
                            '((text:p (@ (text:style-name "Text_20_body"))))
                            (car parts))))
            (let sub ((l (cdr parts))
                      (rows (cons (if (and (null? rows) (null? out) (!= start 1))
                                      ;; (a list which starts at another number)
                                      `(text:list-item
                                         (@ (text:start-value ,(number->string start)))
                                         ,@first)
                                      `(text:list-item ,@first))
                                  rows))
                      (out out))
              (if (null? l)
                  (loop (cdr items) rows out)
                  (sub (cddr l)
                       (if (null? (cadr l)) '()
                           (list `(text:list-header ,@(cadr l))))
                       (cons (car l) (close rows out))))))))))

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
            ((list) (ot-list x))
            ((table) (list (ot-table x)))
            ;; a field over several paragraphs: its paragraphs
            ((field-blocks)
             (append-map (lambda (y) (ot-block y default)) (ox-children x)))
            ;; a section in columns
            ((columns)
             (let* ((n (number->string (office-integer (ox-attr x 'count) 2 1 10)))
                    (line? (== (ox-attr x 'line) "true"))
                    (gap (with c (office-length->cm (ox-attr x 'gap))
                           (if c (ot-cm c) "0.5cm")))
                    (style (ot-style
                             (list 'section n line? gap) "Sect"
                             (lambda (name)
                               `(style:style
                                  (@ (style:name ,name) (style:family "section"))
                                  (style:section-properties
                                    (style:columns (@ (fo:column-count ,n)
                                                      (fo:column-gap ,gap))
                                      ;; (the line between the columns)
                                      ,@(if line?
                                            '((style:column-sep
                                                (@ (style:style "solid") (style:width "0.02cm")
                                                   (style:color "#000000") (style:height "100%")
                                                   (style:vertical-align "top"))))
                                            '()))))))))
               (list `(text:section (@ (text:style-name ,style)
                                       (text:name ,(string-append "Section"
                                                                  (number->string (ot-next-id)))))
                                    ,@(ot-blocks-with (ox-children x) default)))))
            ((pagebreak)
             (list `(text:p (@ (text:style-name
                                 ,(ot-paragraph-style "Text_20_body" #f #t #f))))))
            ((rule)
             (list `(text:p (@ (text:style-name
                                 ,(ot-paragraph-style "Text_20_body" #f #f #t))))))
            ((toc)
             ;; its title, and an index which the program fills from the
             ;; headings
             (with title (ot-inlines (ox-children x) '())
               (list `(text:table-of-content
                        (@ (text:name "Table of Contents1"))
                        (text:table-of-content-source
                          (@ (text:outline-level "3"))
                          ,@(if (null? title) '()
                                `((text:index-title-template ,(ox-text x)))))
                        (text:index-body
                          ,@(if (null? title) '()
                                `((text:index-title
                                    (@ (text:name "Table of Contents1_Head"))
                                    (text:p (@ (text:style-name "Contents_20_Heading"))
                                            ,@title))))
                          (text:p (@ (text:style-name "Text_20_body"))
                                  "Update this index to see the table of contents."))))))
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

(define (ot-default-font)
  ;; the font of the text: a font with serifs as those of TeXmacs, unless
  ;; the document says otherwise
  (cond ((== (ox-attr ot-setup 'font) "sans") "Arial")
        ((== (ox-attr ot-setup 'font) "mono") "Courier New")
        (else "Times New Roman")))

(define (ot-font-decls)
  ;; the fonts which the styles name
  `(office:font-face-decls
     ,@(cdr ot-fonts)
     ,@(if (== (ot-default-font) "Courier New") '()
           `((style:font-face (@ (style:name ,(ot-default-font))
                                 (svg:font-family ,(string-append "'" (ot-default-font) "'"))
                                 (style:font-family-generic
                                   ,(if (== (ot-default-font) "Arial") "swiss" "roman"))
                                 (style:font-pitch "variable")))))))

(define (ot-setup-length key default)
  (with cm (office-length->cm (ox-attr ot-setup key))
    (if cm (ot-cm cm) default)))

(define (ot-styles-file)
  `(office:document-styles
     (@ ,@ot-namespaces)
     ,(ot-font-decls)
     (office:styles
       (style:default-style
         (@ (style:family "paragraph"))
         (style:paragraph-properties
           (@ (fo:text-align ,(with a (ox-attr ot-setup 'align)
                                (cond ((== a "center") "center")
                                      ((== a "right") "end")
                                      ((== a "justify") "justify")
                                      (else "start"))))))
         (style:text-properties
           (@ (style:font-name ,(ot-default-font))
              (fo:font-size
                ,(with n (office-number (ox-attr ot-setup 'font-size))
                   (if (and n (>= n 5) (<= n 50))
                       (string-append (office-decimal n) "pt")
                       "11pt"))))))
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
          "Contents_20_Heading" "Contents Heading" "Heading"
          '((fo:margin-top "0.6cm") (fo:margin-bottom "0.2cm"))
          '((fo:font-size "18pt") (fo:font-weight "bold")))
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
       ,(ot-paragraph-style-element
          "Theorem" #f "Text_20_body"
          '((fo:margin-top "0.2cm") (fo:margin-bottom "0.2cm"))
          '((fo:font-style "italic")))
       ,(ot-paragraph-style-element
          "Remark" #f "Text_20_body"
          '((fo:margin-top "0.2cm") (fo:margin-bottom "0.2cm")) '())
       ,(ot-paragraph-style-element
          "Proof" #f "Text_20_body"
          '((fo:margin-top "0.2cm") (fo:margin-bottom "0.2cm")) '())
       ,(ot-paragraph-style-element
          "Bibliography" #f "Text_20_body"
          '((fo:margin-left "1cm") (fo:text-indent "-1cm")) '())
       ;; the numbers of the headings: 1, 1.1, 1.1.1
       ,@(if ot-numbered?
             `((text:outline-style
                 (@ (style:name "Outline"))
                 ,@(map (lambda (i)
                          `(text:outline-level-style
                             (@ (text:level ,(number->string (+ i 1)))
                                (style:num-format "1")
                                ,@(if (> i 0)
                                      `((text:display-levels ,(number->string (+ i 1))))
                                      '()))
                             (style:list-level-properties
                               (@ (text:list-level-position-and-space-mode
                                    "label-alignment"))
                               (style:list-level-label-alignment
                                 (@ (text:label-followed-by "space"))))))
                        (iota 9))))
             '())
       (style:style (@ (style:name "Graphics") (style:family "graphic")))
       (style:style (@ (style:name "Formula") (style:family "graphic"))))
     ;; the page
     (office:automatic-styles
       (style:page-layout
         (@ (style:name "pm1"))
         (style:page-layout-properties
           (@ (fo:page-width ,(ot-setup-length 'page-width "21cm"))
              (fo:page-height ,(ot-setup-length 'page-height "29.7cm"))
              (style:print-orientation
                ,(if (== (ox-attr ot-setup 'landscape) "true") "landscape" "portrait"))
              (fo:margin-top ,(ot-setup-length 'margin-top "2cm"))
              (fo:margin-bottom ,(ot-setup-length 'margin-bottom "2cm"))
              (fo:margin-left ,(ot-setup-length 'margin-left "2cm"))
              (fo:margin-right ,(ot-setup-length 'margin-right "2cm"))))))
     (office:master-styles
       (style:master-page (@ (style:name "Standard") (style:page-layout-name "pm1"))))))

(define (ot-data-style? x)
  ;; a style of numbers, dates, times... (number:date-style)
  (and (pair? x) (symbol? (car x)) (ox-attr x 'style:name)
       (string-starts? (symbol->string (car x)) "number:")))

(define (ot-data-styles-of x)
  ;; the styles of numbers, dates and times inside x, as (name . element)
  (cond ((not (pair? x)) '())
        ((ot-data-style? x) (list (cons (ox-attr x 'style:name) x)))
        (else (append-map ot-data-styles-of (ox-elements x)))))

(define (ot-data-style-refs x)
  ;; the names of the styles of numbers which x refers to
  (cond ((not (pair? x)) '())
        ((func? x '@) '())
        (else
          (append (list-filter (list (ox-attr x 'style:data-style-name)
                                     (and (func? x 'style:map)
                                          (ox-attr x 'style:apply-style-name)))
                               string?)
                  (append-map ot-data-style-refs (ox-elements x))))))

(define (ot-drop-data-style-refs x names)
  ;; x without the references to the styles of numbers which are not names
  (cond ((not (pair? x)) x)
        ((func? x '@)
         (cons '@ (list-filter (cdr x)
                               (lambda (a) (not (and (eq? (car a) 'style:data-style-name)
                                                     (not (in? (cadr a) names))))))))
        (else (cons (car x) (map (lambda (y) (ot-drop-data-style-refs y names)) (cdr x))))))

(define (ot-with-data-styles content)
  ;; The text with the styles of numbers, dates and times which its fields
  ;; refer to: those of the file it was read from, among its automatic
  ;; styles; those of the styles of the file are kept with them; a
  ;; reference to a style which there is not is left out.
  (let* ((refs (ot-data-style-refs content)))
    (if (null? refs) content
        (let* ((their-content (ot-package-xml "content.xml"))
               (their-styles (ot-package-xml "styles.xml"))
               (common (if their-styles
                           (map car (ot-data-styles-of (ox-child their-styles 'office:styles)))
                           '()))
               (auto (append (if their-content
                                 (ot-data-styles-of
                                   (ox-child their-content 'office:automatic-styles))
                                 '())
                             (if their-styles
                                 (ot-data-styles-of
                                   (ox-child their-styles 'office:automatic-styles))
                                 '())))
               ;; (a style of numbers may apply another one)
               (copied (let loop ((todo refs) (done '()))
                         (cond ((null? todo) (reverse done))
                               ((or (assoc (car todo) done) (in? (car todo) common)
                                    (not (assoc (car todo) auto)))
                                (loop (cdr todo) done))
                               (else
                                 (let ((p (assoc (car todo) auto)))
                                   (loop (append (cdr todo) (ot-data-style-refs (cdr p)))
                                         (cons p done)))))))
               (names (append common (map car copied)))
               (content (ot-drop-data-style-refs content names)))
          (map (lambda (y)
                 (if (func? y 'office:automatic-styles)
                     (append y (map cdr copied))
                     y))
               content)))))

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
     (office:body
       (office:text
         ;; the changes which are tracked
         ,@(if (null? ot-changes) '()
               `((text:tracked-changes ,@(reverse ot-changes))))
         ;; the sequences which the text counts
         ,@(if (null? ot-sequences) '()
               `((text:sequence-decls
                   ,@(map (lambda (name)
                            `(text:sequence-decl
                               (@ (text:display-outline-level "0") (text:name ,name))))
                          (reverse ot-sequences)))))
         ,@blocks))))

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

(define (ot-package-xml name)
  (with s (and ot-package (ahash-ref ot-package name))
    (and s (ox-root (parse-xml s)))))

(define (ot-merged-styles mine)
  ;; The styles of the file which the text was read from: its named styles,
  ;; its pages with their headers and footers, its fonts, with those of
  ;; TeXmacs which it does not have, and the page of the document.
  (with root (ot-package-xml "styles.xml")
    (if (not root) mine
        (let* ((names (lambda (x tag)
                        (map (lambda (y) (or (ox-attr y 'style:name) ""))
                             (if x (ox-elements x) '()))))
               (theirs (ox-child root 'office:styles))
               (ours (ox-child mine 'office:styles))
               (their-names (names theirs 'style:style))
               (new-styles (list-filter (if ours (ox-elements ours) '())
                                        (lambda (y)
                                          (and (in? (car y) '(style:style text:list-style))
                                               (not (in? (or (ox-attr y 'style:name) "")
                                                         their-names))))))
               ;; (the numbers of the headings are those of TeXmacs)
               (outline (and ours (ox-child ours 'text:outline-style)))
               (their-fonts (ox-child root 'office:font-face-decls))
               (our-fonts (ox-child mine 'office:font-face-decls))
               (font-names (names their-fonts 'style:font-face))
               (new-fonts (list-filter (if our-fonts (ox-elements our-fonts) '())
                                       (lambda (y) (not (in? (ox-attr y 'style:name)
                                                             font-names)))))
               (master (odt-standard-master root))
               (layout-name (and master (ox-attr master 'style:page-layout-name)))
               (page (ox-child (ox-child (ox-child mine 'office:automatic-styles)
                                         'style:page-layout)
                               'style:page-layout-properties))
               (fix-layout
                 (lambda (l)
                   ;; the page of the document in the layout of the first page
                   (if (not (and (func? l 'style:page-layout)
                                 (== (ox-attr l 'style:name) layout-name) page))
                       l
                       `(style:page-layout
                          (@ ,@(ox-attrs l))
                          ,@(map (lambda (y)
                                   (if (not (func? y 'style:page-layout-properties)) y
                                       (let* ((mine-attrs (ox-attrs page))
                                              (keys (map car mine-attrs)))
                                         `(style:page-layout-properties
                                            (@ ,@mine-attrs
                                               ,@(list-filter (ox-attrs y)
                                                              (lambda (a) (not (in? (car a) keys)))))
                                            ,@(ox-children y)))))
                                 (ox-children l))))))
               (fix (lambda (x)
                      (cond ((not (pair? x)) x)
                            ((func? x 'office:styles)
                             `(office:styles (@ ,@(ox-attrs x))
                                             ,@(if outline
                                                   (list-filter (ox-children x)
                                                                (lambda (y) (not (func? y 'text:outline-style))))
                                                   (ox-children x))
                                             ,@(if outline (list outline) '())
                                             ,@new-styles))
                            ((func? x 'office:font-face-decls)
                             `(office:font-face-decls (@ ,@(ox-attrs x)) ,@(ox-children x)
                                                      ,@new-fonts))
                            ((func? x 'office:automatic-styles)
                             `(office:automatic-styles (@ ,@(ox-attrs x))
                                                       ,@(map fix-layout (ox-children x))))
                            (else x)))))
          (cons (car root) (map fix (cdr root)))))))

(define (odt-standard-master root)
  ;; the first page of the styles root
  (let ((masters (ox-child root 'office:master-styles)))
    (and masters
         (or (list-find (ox-childs masters 'style:master-page)
                        (lambda (m) (== (ox-attr m 'style:name) "Standard")))
             (ox-child masters 'style:master-page)))))

(define (ot-kept-entry? name)
  ;; the entries of the file which are written again
  (and (not (in? name '("mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml")))
       (not (string-ends? name "/"))
       (not (string-starts? name "Pictures/tm-image"))
       (not (string-starts? name "Object "))
       (not (string-starts? name "ObjectReplacements/"))
       (not (string-starts? name "Formula-"))
       ;; (not the signatures and the picture of the old text)
       (not (string-starts? name "META-INF/"))
       (not (string-starts? name "Thumbnails/"))))

(define (ot-media-type name)
  (cond ((string-ends? name ".xml") "text/xml")
        ((string-ends? name ".png") "image/png")
        ((or (string-ends? name ".jpg") (string-ends? name ".jpeg")) "image/jpeg")
        ((string-ends? name ".svg") "image/svg+xml")
        ((string-ends? name ".gif") "image/gif")
        (else "application/octet-stream")))

(tm-define (serialize-odt-document x)
  (:type (-> stree string))
  (:synopsis "Write the office tree @x as an OpenDocument text, a zip archive")
  (let* ((l (if (func? x 'office) (cdr x) (list x)))
         (pack (list-find l (lambda (y) (func? y 'package)))))
    (set! ot-package (and pack (== (ox-attr pack 'format) "odt")
                          (office-archive (ox-attr pack 'data))))
    (when (and ot-package (not (ahash-ref ot-package "styles.xml")))
      (set! ot-package #f)))
  (set! ot-styles (make-ahash-table))
  (set! ot-style-list '())
  (set! ot-files '())
  (set! ot-pictures (make-ahash-table))
  (set! ot-changes '())
  (set! ot-id 0)
  (set! ot-cell-align #f)
  (set! ot-numbered? (office-numbered-headings? x))
  (set! ot-setup (or (and (func? x 'office)
                          (list-find (cdr x) (lambda (y) (func? y 'setup))))
                     '(setup)))
  (set! ot-sequences '())
  (set! ot-in-note? #f)
  (set! ot-bookmarks (make-ahash-table))
  (set! ot-used-styles '())
  (set! ot-source-styles
        (append-map ox-children
                    (list-filter (if (func? x 'office) (cdr x) '())
                                 (lambda (y) (func? y 'styles)))))
  (let* ((blocks (ot-blocks-with (if (func? x 'office) (cdr x) (list x))
                                 "Text_20_body"))
         (blocks (if (null? blocks)
                     '((text:p (@ (text:style-name "Text_20_body"))))
                     blocks))
         (content (ot-with-data-styles (ot-content blocks)))
         (kept (if ot-package
                   (sort (list-filter (map car (ahash-table->list ot-package))
                                      ot-kept-entry?)
                         string<?)
                   '()))
         (files (append (reverse ot-files)
                        (map (lambda (n) (list n (ot-media-type n) (ahash-ref ot-package n)))
                             kept)))
         ;; (a directory is in the manifest, not in the archive)
         (entries (list-filter files caddr))
         ;; the type of the file comes first, as the format wants
         (r (zip-pack
              (append '("mimetype" "content.xml" "styles.xml" "META-INF/manifest.xml")
                      (map car entries))
              (append (list "application/vnd.oasis.opendocument.text"
                            (ox-serialize content)
                            (ox-serialize (ot-complete-styles (ot-merged-styles (ot-styles-file))))
                            (ox-serialize (ot-manifest files)))
                      (map caddr entries)))))
    (set! ot-styles (make-ahash-table))
    (set! ot-style-list '())
    (set! ot-files '())
    (set! ot-pictures (make-ahash-table))
    (set! ot-changes '())
    (set! ot-package #f)
    r))
