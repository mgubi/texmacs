
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdowntm.scm
;; DESCRIPTION : conversion of Markdown trees into TeXmacs trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Markdown trees are described in markdownin.scm. Their mathematics is
;; LaTeX and their HTML is HTML: both go through the converters of TeXmacs
;; for these formats.
;;
;; A file becomes a document of the style markdown, whose macros keep what
;; the standard tags do not say (mdtm-faithful?): the levels of the
;; headings, the header in YAML, the text of the file for a formula, a
;; table and a piece of HTML, the lists with blank lines, the words after the fence of
;; code, the description and the title of an image, the labels of the
;; footnotes. Such a document is written as the Markdown it was read from
;; (see tmmarkdown.scm). A piece of text, which may be pasted into any
;; document, gets the standard tags only.

(texmacs-module (convert markdown markdowntm)
  (:use (convert markdown tmmarkdown)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State and tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the definitions of the footnotes, the labels of those which are being
;; converted, and the tags of the six levels of headings, for the document
;; which is converted
(define mdtm-footnotes (make-ahash-table))
(define mdtm-open-footnotes '())
(define mdtm-headings '())
;; whether the macros of the style markdown are used, and then the labels
;; of the footnotes which are their numbers, in the order of the text, the
;; labels to which the text refers, those of the footnotes which were
;; converted, and whether their definitions are written after everything
(define mdtm-faithful? #f)
(define mdtm-numbered-footnotes '())
(define mdtm-used-footnotes (make-ahash-table))
(define mdtm-done-footnotes (make-ahash-table))

;; whether a table is being converted which is written back as it was read
;; while it is not changed: its marks of footnotes are kept as they are,
;; and the definitions to which only such tables refer stay where they are
(define mdtm-in-kept-table? #f)
(define mdtm-defs-last? #f)

(define mdtm-faithful-sections
  '(section subsection subsubsection paragraph subparagraph md-h6))

(define mdtm-sections
  '(section subsection subsubsection paragraph subparagraph subparagraph))

(define (mdtm-attr x name)
  (and (pair? x) (pair? (cdr x)) (func? (cadr x) '@)
       (with a (assoc name (cdadr x))
         (and a (cadr a)))))

(define (mdtm-children x)
  (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cddr x) (cdr x)))

(define (mdtm-utf8-length c)
  ;; the number of bytes of the character of UTF-8 whose first byte is c
  (cond ((< c 192) 1) ((< c 224) 2) ((< c 240) 3) (else 4)))

(define (mdtm-utf8-code s)
  ;; the code of the character of UTF-8 s, or #f when s is not one
  (let* ((b (map char->integer (string->list s)))
         (n (length b))
         (rest (lambda (v l)
                 (let loop ((v v) (l l))
                   (cond ((null? l) v)
                         ((and (>= (car l) 128) (< (car l) 192))
                          (loop (+ (* v 64) (- (car l) 128)) (cdr l)))
                         (else #f))))))
    (cond ((== n 2) (rest (- (car b) 192) (cdr b)))
          ((== n 3) (rest (- (car b) 224) (cdr b)))
          ((== n 4) (rest (- (car b) 240) (cdr b)))
          (else #f))))

(define (mdtm-non-ascii? s)
  (let loop ((i 0))
    (and (< i (string-length s))
         (or (>= (char->integer (string-ref s i)) 128) (loop (+ i 1))))))

(define (mdtm-cork s)
  ;; the text of UTF-8 in the encoding of TeXmacs. A character which this
  ;; encoding would make another one, as the apostrophe of typography which
  ;; it writes ', the opening quote which it writes ` (a backquote in a
  ;; document of the style markdown) or the micro sign which it writes as
  ;; the Greek mu, is written by its code: it is written back as it was
  (with c (utf8->cork s)
    ;; (s holds no backquote: a ` of c is such a quote)
    (if (or (not (mdtm-non-ascii? s))
            (and (== (cork->utf8 c) s) (not (string-index c #\`))))
        c
        (let loop ((i 0) (start 0) (acc '()))
          (define (flush)
            (if (< start i) (cons (utf8->cork (substring s start i)) acc) acc))
          (if (>= i (string-length s)) (apply string-append (reverse (flush)))
              (let ((b (char->integer (string-ref s i))))
                (if (< b 128) (loop (+ i 1) start acc)
                    (let* ((e (min (string-length s)
                                   (+ i (mdtm-utf8-length b))))
                           (ch (substring s i e))
                           (code (mdtm-utf8-code ch)))
                      (if (or (not code)
                              (with k (utf8->cork ch)
                                (and (== (cork->utf8 k) ch) (!= k "`"))))
                          (loop e start acc)
                          (loop e e
                                (cons (string-append
                                        "<#" (upcase-all (number->string code 16))
                                        ">")
                                      (flush))))))))))))

(define (mdtm-convert s control)
  ;; the text in the encoding of TeXmacs; a backquote stays one, as it does
  ;; for the other formats (alone, it would be the accent of this encoding),
  ;; and a control character, as a tab, is what (control code) is: in this
  ;; encoding its code would be an accent too
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (if (>= i n)
          (if (null? acc) (mdtm-cork s)
              (apply string-append
                     (reverse (cons (mdtm-cork (substring s start n)) acc))))
          (let ((c (char->integer (string-ref s i))))
            (cond ((== c 96)
                   (loop (+ i 1) (+ i 1)
                         (cons* "`" (mdtm-cork (substring s start i)) acc)))
                  ((and (< c 32) (!= c 10))
                   (loop (+ i 1) (+ i 1)
                         (cons* (control c) (mdtm-cork (substring s start i))
                                acc)))
                  (else (loop (+ i 1) start acc))))))))

(define (mdtm-char c)
  ;; the character of code c, below 32, in a string of TeXmacs
  (string-append "<#" (number->string c 16) ">"))

(define (mdtm-text s)
  ;; the text, to be displayed: a tab is a space
  (mdtm-convert s (lambda (c) (if (== c 9) " " (mdtm-char c)))))

(define (mdtm-source-text s)
  ;; the text, as it is kept to be written again
  (mdtm-convert s mdtm-char))

(define (mdtm-control? s)
  ;; does the text hold a control character, as a tab?
  (let loop ((i 0))
    (and (< i (string-length s))
         (or (with c (char->integer (string-ref s i)) (and (< c 32) (!= c 10)))
             (loop (+ i 1))))))

(define (mdtm-expand-tabs s)
  ;; the tabs of a line of code as spaces, to the next multiple of 4
  (if (not (string-index s #\tab)) s
      (let loop ((l (string->list s)) (col 0) (acc '()))
        (cond ((null? l) (list->string (reverse acc)))
              ((char=? (car l) #\tab)
               (with k (- 4 (remainder col 4))
                 (loop (cdr l) (+ col k) (append (make-list k #\space) acc))))
              (else (loop (cdr l)
                          ;; (the bytes which follow the first one of a
                          ;; character of UTF-8 take no column)
                          (with c (char->integer (car l))
                            (if (and (>= c 128) (< c 192)) col (+ col 1)))
                          (cons (car l) acc)))))))

(define (mdtm-merge l)
  ;; with the strings which follow each other as one
  (let loop ((l l) (strs '()) (acc '()))
    ;; strs: the strings before l which are not in acc yet, the last first
    (let ((flush (lambda ()
                   (with s (apply string-append (reverse strs))
                     (if (== s "") acc (cons s acc))))))
      (cond ((null? l) (reverse (flush)))
            ((string? (car l)) (loop (cdr l) (cons (car l) strs) acc))
            (else (loop (cdr l) '() (cons (car l) (flush))))))))

(define (mdtm-concat l)
  (with r (mdtm-merge l)
    (cond ((null? r) "")
          ((null? (cdr r)) (car r))
          (else `(concat ,@r)))))

(define (mdtm-document l)
  (if (null? l) '(document "") `(document ,@l)))

(define (mdtm-lines s)
  ;; the lines of a string
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((>= i (string-length s))
           (reverse (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
          (else (loop (+ i 1) start acc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics, HTML and code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdtm-unary-document x)
  ;; a document of one paragraph is that paragraph
  (if (and (func? x 'document 1)) (cadr x) x))

(define (mdtm-latex s)
  ;; the LaTeX snippet s as a TeXmacs tree, or #f
  (with r (catch #t
            (lambda () (convert s "latex-snippet" "texmacs-stree"))
            (lambda args #f))
    (and r (!= r "") (mdtm-unary-document r))))

(define (mdtm-source s)
  ;; a text of the file as an argument of a macro: its lines
  (with l (mdtm-lines s)
    (if (list-1? l) (mdtm-source-text (car l))
        `(tuple ,@(map mdtm-source-text l)))))

(define (mdtm-written x)
  ;; the text which is written for the Markdown block or text x
  (with s (convert `(markdown ,x) "markdown-stree" "markdown-snippet")
    (if (string-ends? s "\n") (substring s 0 (- (string-length s) 1)) s)))

(define (mdtm-keep t raw block?)
  ;; the tree t for the text raw of the file, with this text and a copy of
  ;; t (see tmmd-md-source)
  (if (not mdtm-faithful?) t
      `(,(if block? 'md-block-source 'md-source) ,t ,(mdtm-source raw) ,t)))

(define (mdtm-keep-math t tag s raw block?)
  ;; the formula t which was written as the LaTeX s, in the text raw, with
  ;; this text. Each formula keeps it, also one which TeXmacs would write
  ;; in the same way: to know this, the formula would have to be converted
  ;; to LaTeX again, which takes as long as reading it. Whether it is
  ;; written so is decided when it was edited (see tmmd-md-source)
  (mdtm-keep t (or raw (if (== tag 'math) (string-append "$" s "$")
                           (string-append "$$\n" s "\n$$")))
             block?))

(define (mdtm-keep-html t s) (mdtm-keep t s #f))

;; the formulas which were converted, with their texts: a formula is often
;; written several times, and its conversion takes the most time
(define mdtm-formulas (make-ahash-table))

(define (mdtm-memo key thunk)
  (with k (object->string key)
    (or (ahash-ref mdtm-formulas k)
        (with r (thunk)
          (ahash-set! mdtm-formulas k r)
          r))))

(define (mdtm-math s . raw)
  (with raw (and (nnull? raw) (car raw))
    (mdtm-memo (list 'math s raw mdtm-faithful?)
      (lambda ()
        (mdtm-keep-math (or (mdtm-latex (string-append "$" s "$"))
                            `(math ,(mdtm-text s)))
                        'math s raw #f)))))

(define (mdtm-environment? s)
  ;; a formula which is an environment of LaTeX by itself, as align
  (with t (string-trim-spaces s)
    (and (string-starts? t "\\begin{")
         (list-or (map (lambda (e) (string-starts? t (string-append "\\begin{" e)))
                       '("align" "eqnarray" "gather" "multline" "equation"
                         "flalign" "alignat"))))))

(define (mdtm-display-math s raw block?)
  ;; raw: the text of the file, or #f for a formula between two $$, with
  ;; lines of their own when it is not in a paragraph (block?)
  (mdtm-memo (list 'display s raw block? mdtm-faithful?)
    (lambda ()
      (mdtm-keep-math
        (or (mdtm-latex (if (mdtm-environment? s) s
                            (string-append "\\[" s "\\]")))
            `(equation* (document ,(mdtm-text s))))
        '!display s
        (or raw (and (not block?) (string-append "$$" s "$$")))
        block?))))

(define (mdtm-html s)
  ;; the HTML snippet s as a TeXmacs tree; a comment, or what TeXmacs does
  ;; not understand, is dropped
  (if (string-starts? (string-trim-spaces s) "<!--") ""
      (with r (catch #t
                (lambda () (convert s "html-snippet" "texmacs-stree"))
                (lambda args #f))
        (if r (mdtm-unary-document r) ""))))

(define (mdtm-pre x)
  (let* ((tag (tmmd-language-tag (or (mdtm-attr x 'lang) "")))
         (s (apply string-append (list-filter (mdtm-children x) string?)))
         ;; (a line with tabs, which are displayed as spaces, is written as
         ;; it is while it is not changed)
         (code `(,tag ,(mdtm-document
                         (map (lambda (l)
                                (with t (mdtm-text (mdtm-expand-tabs l))
                                  (if (mdtm-control? l) (mdtm-keep t l #f) t)))
                              (mdtm-lines s)))))
         (info (or (mdtm-attr x 'info) (mdtm-attr x 'lang) ""))
         ;; the words after the fence, when the tag is not written with them
         (r (if (and mdtm-faithful? (!= info (tmmd-language-name tag)))
                `(md-fence ,(mdtm-source-text info) ,code)
                code)))
    ;; (and so is the whole block)
    (if (mdtm-control? s) (mdtm-keep r (mdtm-written x) #t) r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdtm-footnote label)
  ;; the footnote, or its mark as text when it has no definition, or when it
  ;; is in the text of a footnote: a footnote does not hold others, and
  ;; definitions which refer to each other twice would make a document
  ;; whose size doubles with each of them. In a document of the style
  ;; markdown, a note to which the text refers several times is at the
  ;; first mark, and the others are kept, as such a mark, to be written as
  ;; they are
  (let ((def (ahash-ref mdtm-footnotes label))
        (mark (string-append "[^" (mdtm-text label) "]")))
    (cond ((or (not def) (nnull? mdtm-open-footnotes)
               (and mdtm-faithful? (ahash-ref mdtm-done-footnotes label)))
           (mdtm-keep mark (string-append "[^" label "]") #f))
          (mdtm-in-kept-table?
            (set! mdtm-open-footnotes (list label))
            (let* ((l (mdtm-blocks def))
                   (body (if (list-1? l) (car l) (mdtm-document l))))
              (set! mdtm-open-footnotes '())
              (mdtm-keep `(md-footnote ,(mdtm-text label) ,body)
                         (string-append "[^" label "]") #f)))
          (else
            (ahash-set! mdtm-done-footnotes label #t)
            (set! mdtm-open-footnotes (list label))
            (let* ((l (mdtm-blocks def))
                   (body (if (list-1? l) (car l) (mdtm-document l))))
              (set! mdtm-open-footnotes '())
              ;; with its label, when it is not the number it would get
              (if (and mdtm-faithful?
                       (not (in? label mdtm-numbered-footnotes)))
                  `(md-footnote ,(mdtm-text label) ,body)
                  `(footnote ,body)))))))

(define (mdtm-keep-control t s x)
  ;; the tree t for the Markdown text x whose text is s: when s holds a
  ;; control character, as a tab, which t displays as a space, with the
  ;; text which is written for x
  (if (mdtm-control? s) (mdtm-keep t (mdtm-written x) #f) t))

(define (mdtm-string s)
  ;; the trees for a text: in a document of the style markdown, a control
  ;; character is displayed as a space and written as it is
  (if (or (not mdtm-faithful?) (not (mdtm-control? s))) (list (mdtm-text s))
      (let loop ((i 0) (start 0) (acc '()))
        (cond ((>= i (string-length s))
               (reverse (if (< start i)
                            (cons (mdtm-text (substring s start i)) acc)
                            acc)))
              ((with c (char->integer (string-ref s i)) (and (< c 32) (!= c 10)))
               (let* ((c (substring s i (+ i 1)))
                      (acc (if (< start i)
                               (cons (mdtm-text (substring s start i)) acc)
                               acc)))
                 (loop (+ i 1) (+ i 1)
                       (cons (mdtm-keep (mdtm-text c) c #f) acc))))
              (else (loop (+ i 1) start acc))))))

(define (mdtm-inline x)
  ;; the TeXmacs trees for an inline node
  (cond ((string? x) (mdtm-string x))
        ((not (pair? x)) '())
        (else
          (let ((l (mdtm-children x)))
            (case (car x)
              ((em) `((em ,(mdtm-inlines l))))
              ((strong) `((strong ,(mdtm-inlines l))))
              ((del) `((strike-through ,(mdtm-inlines l))))
              ((code)
               (with s (apply string-append (list-filter l string?))
                 (list (mdtm-keep-control `(verbatim ,(mdtm-text s)) s x))))
              ((a)
               ;; a link with a title is hlink*
               (let ((body (mdtm-inlines l))
                     (url (mdtm-text (or (mdtm-attr x 'href) "")))
                     (title (or (mdtm-attr x 'title) "")))
                 (if (== title "") `((hlink ,body ,url))
                     `((hlink* ,body ,url ,(mdtm-source-text title))))))
              ((img) (list (mdtm-described-image x)))
              ((br) '((next-line)))
              ((math) (list (mdtm-math (apply string-append l)
                                       (mdtm-attr x 'source))))
              ((displaymath)
               (list (mdtm-display-math (apply string-append l)
                                        (mdtm-attr x 'source) #f)))
              ((html)
               (with s (apply string-append l)
                 (list (mdtm-keep-html (mdtm-html s) s))))
              ((footnote) (list (mdtm-footnote (apply string-append l))))
              (else (append-map mdtm-inline l)))))))

(define (mdtm-size s)
  ;; a length of TeXmacs for the width or the height of an image: a number
  ;; is in pixels and a percentage is a part of the width of the paragraph
  ;; and anything else is a number with a unit of length, or nothing
  (cond ((or (not s) (== s "")) "")
        ((mdtm-number s) (string-append s "px"))
        ((and (string-ends? s "%")
              (mdtm-number (substring s 0 (- (string-length s) 1))))
         (string-append
           (number->string
             (/ (mdtm-number (substring s 0 (- (string-length s) 1))) 100.0))
           "par"))
        (else
          (let* ((n (string-length s))
                 (k (let loop ((k n))
                      (if (and (> k 0) (char-alphabetic? (string-ref s (- k 1))))
                          (loop (- k 1)) k))))
            (if (and (mdtm-number (substring s 0 k))
                     (in? (substring s k n) mdtm-units))
                s "")))))

(define mdtm-units
  '("px" "pt" "bp" "mm" "cm" "in" "em" "ex" "pc" "par" "pag" "fn" "spc"))

(define (mdtm-number s)
  ;; the number which a string of digits with at most one dot is, or #f
  (and (!= s "") (!= s ".")
       (let loop ((i 0) (dot? #f))
         (cond ((>= i (string-length s)) (string->number s))
               ((char-numeric? (string-ref s i)) (loop (+ i 1) dot?))
               ((and (char=? (string-ref s i) #\.) (not dot?)) (loop (+ i 1) #t))
               (else #f)))))

(define (mdtm-image x)
  `(image ,(mdtm-text (or (mdtm-attr x 'src) ""))
          ,(mdtm-size (mdtm-attr x 'width))
          ,(mdtm-size (mdtm-attr x 'height)) "" ""))

(define (mdtm-described-image x)
  ;; the image, with its description and its title when it has one
  (let* ((title (or (mdtm-attr x 'title) ""))
         (alt (mdtm-inlines (mdtm-children x))))
    (if (or (not mdtm-faithful?) (and (== title "") (== alt "")))
        (mdtm-image x)
        `(md-image ,(mdtm-image x) ,alt ,(mdtm-text title)))))

(define (mdtm-figure l)
  ;; a figure, when the nodes l of a paragraph are a single image with a
  ;; title or a description, which is the caption (as Pandoc does); else #f
  (let* ((l (list-filter l (lambda (x) (not (and (string? x) (== (mdtm-no-spaces x) ""))))))
         (x (and (list-1? l) (func? (car l) 'img) (car l)))
         (title (if x (or (mdtm-attr x 'title) "") ""))
         (caption (cond ((not x) "")
                        ((!= title "") (mdtm-text title))
                        (else (mdtm-inlines (mdtm-children x))))))
    (and x (!= caption "")
         ;; (with a title, the description is not the caption)
         (not (and mdtm-faithful? (!= title "")))
         `(big-figure ,(mdtm-image x) ,caption))))

(define (mdtm-no-spaces s)
  (list->string (list-filter (string->list s)
                             (lambda (c) (not (in? c '(#\space #\newline)))))))

(define (mdtm-inlines l)
  (mdtm-concat (append-map mdtm-inline l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdtm-item x)
  ;; the paragraphs of an item of a list: the first one starts with the item
  (let* ((checked (mdtm-attr x 'checked))
         (item (cond ((not checked) '(item))
                     ((== checked "true") '(item* (math "<boxtimes>")))
                     (else '(item* (math "<Box>")))))
         (l (mdtm-blocks (mdtm-children x))))
    (if (null? l) (list `(concat ,item ""))
        (cons (mdtm-concat (cons item (if (func? (car l) 'concat)
                                          (cdar l)
                                          (list (car l)))))
              (cdr l)))))

(define (mdtm-list x tag)
  (let* ((l (append-map mdtm-item (mdtm-children x)))
         (start (string->number (or (mdtm-attr x 'start) "1")))
         ;; the number of the first item, when it is not 1
         (l (if (and start (!= start 1) (nnull? l))
                (cons `(concat (assign "item-nr" ,(number->string (- start 1)))
                               ,@(if (func? (car l) 'concat) (cdar l)
                                     (list (car l))))
                      (cdr l))
                l))
         (r `(,tag ,(mdtm-document l))))
    (if (and mdtm-faithful? (== (mdtm-attr x 'loose) "true"))
        `(md-loose ,r)
        r)))

(define (mdtm-length t)
  ;; a rough measure of the width of a piece of text
  (cond ((string? t) (string-length t))
        ((pair? t) (apply + (map mdtm-length (cdr t))))
        (else 0)))

(define (mdtm-table-width rows)
  ;; the sum of the widths of the columns, in characters
  (let loop ((rows rows) (w '()))
    (if (null? rows) (apply + w)
        (let sub ((l (map mdtm-length (cdar rows))) (w w) (r '()))
          (cond ((and (null? l) (null? w)) (loop (cdr rows) (reverse r)))
                ((null? l) (sub l (cdr w) (cons (car w) r)))
                ((null? w) (sub (cdr l) w (cons (car l) r)))
                (else (sub (cdr l) (cdr w) (cons (max (car l) (car w)) r))))))))

(define (mdtm-table x)
  ;; a table with borders, the cells of its first row in bold; a table which
  ;; would not fit in the width of a page gets this width and its cells
  ;; are broken into lines
  (let* ((rows (list-filter (mdtm-children x) (lambda (r) (func? r 'tr))))
         (first (if (null? rows) '() (mdtm-children (car rows))))
         (aligns (map (lambda (c) (or (mdtm-attr c 'align) "")) first))
         (formats
           (append-map
             (lambda (a i)
               (with j (number->string i)
                 (cond ((== a "center") `((cwith "1" "-1" ,j ,j "cell-halign" "c")))
                       ((== a "right") `((cwith "1" "-1" ,j ,j "cell-halign" "r")))
                       ((and mdtm-faithful? (== a "left"))
                        `((cwith "1" "-1" ,j ,j "cell-halign" "l")))
                       (else '()))))
             aligns (map (lambda (i) (+ i 1)) (iota (length aligns)))))
         (cell (lambda (c)
                 (with t (mdtm-inlines (mdtm-children c))
                   `(cell ,(if (and (func? c 'th) (!= t "")) `(strong ,t) t))))))
    (let* ((trows (map (lambda (r) `(row ,@(map cell (mdtm-children r)))) rows))
           (wide (if (> (mdtm-table-width trows) 72)
                     `((twith "table-width" "1par")
                       (twith "table-hmode" "exact")
                       (cwith "1" "-1" "1" "-1" "cell-hyphen" "t"))
                     '())))
      `(block (tformat ,@wide ,@formats (table ,@trows))))))

(define (mdtm-heading x)
  (let* ((n (- (char->integer (string-ref (symbol->string (car x)) 1))
               (char->integer #\0)))
         (tag (list-ref mdtm-headings (- n 1))))
    (if tag `((,tag ,(mdtm-inlines (mdtm-children x)))) '())))

(define (mdtm-block x)
  ;; the paragraphs for a block
  (cond ((string? x) (list (mdtm-text x)))
        ((not (pair? x)) '())
        (else
          (let ((l (mdtm-children x)))
            (case (car x)
              ((h1 h2 h3 h4 h5 h6) (mdtm-heading x))
              ((p) (list (or (mdtm-figure l) (mdtm-inlines l))))
              ((blockquote) `((quotation ,(mdtm-document (mdtm-blocks l)))))
              ((ul)
               ;; the items of the file start with a dash, a star or a plus
               (with m (mdtm-attr x 'marker)
                 (list (mdtm-list x (cond ((not mdtm-faithful?) 'itemize)
                                          ((== m "*") 'itemize-dot)
                                          ((== m "+") 'itemize-arrow)
                                          (else 'itemize-minus))))))
              ((ol) (list (mdtm-list x 'enumerate)))
              ((pre) (list (mdtm-pre x)))
              ((hr) '((hrule)))
              ((table)
               (let* ((raw (and mdtm-faithful? (mdtm-attr x 'source)))
                      (t (begin (set! mdtm-in-kept-table? (and raw #t))
                                (mdtm-table x))))
                 (set! mdtm-in-kept-table? #f)
                 (list (if raw (mdtm-keep t raw #t) t))))
              ((displaymath)
               (list (mdtm-display-math (apply string-append l)
                                        (mdtm-attr x 'source) #t)))
              ((html)
               (let* ((s (apply string-append l))
                      (r (mdtm-html s)))
                 (cond (mdtm-faithful? (list (mdtm-keep r s #t)))
                       ((== r "") '())
                       ((func? r 'document) (cdr r))
                       (else (list r)))))
              ((footnote-def) (if mdtm-faithful? (mdtm-footnote-def x) '()))
              ;; the definition of a link to which nothing refers
              ((ref-def)
               (if mdtm-faithful?
                   (list (mdtm-keep "" (apply string-append l) #t))
                   '()))
              ((meta) '())
              (else (list (mdtm-inlines (list x)))))))))

(define (mdtm-footnote-def x)
  ;; in a document of the style markdown, where the text of a footnote is
  ;; written, when it is not after everything else; the definition which
  ;; the text does not use (a second one of a label, one to which only
  ;; other notes refer) is kept as it is written
  (let ((label (and (pair? (cdr x)) (cadr x))))
    (if (and (string? label) (ahash-ref mdtm-used-footnotes label)
             (eq? (ahash-ref mdtm-footnotes label) (cddr x)))
        (if mdtm-defs-last? '() `((md-footnote-text ,(mdtm-text label))))
        (list (mdtm-keep "" (mdtm-written x) #t)))))

(define (mdtm-blocks l)
  (append-map mdtm-block l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdtm-collect-footnotes x)
  ;; the first definition of a label is the one which counts, as for links
  (when (and (pair? x) (in? (car x) '(markdown blockquote ul ol li
                                      footnote-def)))
    (when (and (func? x 'footnote-def) (pair? (cdr x)) (string? (cadr x))
               (not (ahash-ref mdtm-footnotes (cadr x))))
      (ahash-set! mdtm-footnotes (cadr x) (cddr x)))
    (for-each mdtm-collect-footnotes (cdr x))))

(define (mdtm-count-headings l tag)
  (length (list-filter l (lambda (x) (func? x tag)))))

(define (mdtm-doc-data meta title)
  ;; the title, the authors, the date and the abstract, from the header of
  ;; the document and from its heading
  (let* ((get (lambda (key)
                (map cadr (list-filter meta (lambda (x) (== (car x) key))))))
         (title* (cond (title (mdtm-inlines title))
                       ((nnull? (get 'title)) (mdtm-text (car (get 'title))))
                       (else #f)))
         (subtitle (get 'subtitle))
         (authors (append (get 'author) (get 'authors)))
         (date (get 'date))
         (abstract (get 'abstract)))
    (append
      (if (and (not title*) (null? authors) (null? date)) '()
          `((doc-data
              ,@(if title* `((doc-title ,title*)) '())
              ,@(if (null? subtitle) '()
                    `((doc-subtitle ,(mdtm-text (car subtitle)))))
              ,@(map (lambda (a)
                       `(doc-author (author-data (author-name ,(mdtm-text a)))))
                     authors)
              ,@(if (null? date) '() `((doc-date ,(mdtm-text (car date))))))))
      (if (null? abstract) '()
          `((abstract-data
              (abstract (document ,(mdtm-text (car abstract))))))))))

(define (mdtm-yaml-key s)
  ;; the place of the colon after the key which starts the line, or #f
  (let ((n (string-length s)))
    (and (> n 0) (char-alphabetic? (string-ref s 0))
         (let loop ((i 1))
           (and (< i n)
                (with c (string-ref s i)
                  (cond ((char=? c #\:)
                         (and (or (== (+ i 1) n)
                                  (char=? (string-ref s (+ i 1)) #\space))
                              i))
                        ((or (char-alphabetic? c) (char-numeric? c)
                             (in? c '(#\_ #\- #\.)))
                         (loop (+ i 1)))
                        (else #f))))))))

(define (mdtm-metadata source)
  ;; the header of the file: a md-meta for each of its lines
  `(md-metadata
     ,(mdtm-document
        (map (lambda (s)
               (let* ((i (mdtm-yaml-key s))
                      (v (and i (string-trim-spaces
                                  (substring s (+ i 1) (string-length s))))))
                 ;; (a line with other spaces around its colon than those
                 ;; which are written is kept as it is, and so is a line
                 ;; with a tab)
                 (cond ((mdtm-control? s)
                        `(md-meta "" ,(mdtm-keep (mdtm-text s) s #f)))
                       ((and i (== s (if (== v "") (substring s 0 (+ i 1))
                                         (string-append (substring s 0 i)
                                                        ": " v))))
                        `(md-meta ,(mdtm-text (substring s 0 i)) ,(mdtm-text v)))
                       (else `(md-meta "" ,(mdtm-text s))))))
             (if (== source "") '() (mdtm-lines source))))))

(define (mdtm-footnote-labels x)
  ;; the labels of the footnotes of the text, but for those of the
  ;; definitions and of the tables (which are written as they were read),
  ;; in their order
  (cond ((not (pair? x)) '())
        ((func? x 'footnote-def) '())
        ((and (func? x 'table) (mdtm-attr x 'source)) '())
        ((and (func? x 'footnote 1) (string? (cadr x))) (list (cadr x)))
        (else (append-map mdtm-footnote-labels (cdr x)))))

(define (mdtm-notes-last? l labels)
  ;; are the definitions of the footnotes the last blocks, those of the
  ;; labels and in their order? They are written so
  (let* ((def? (lambda (x) (func? x 'footnote-def)))
         (tail (let loop ((r (reverse l)) (acc '()))
                 (if (and (nnull? r) (def? (car r)))
                     (loop (cdr r) (cons (car r) acc))
                     acc))))
    (and (== (length (list-filter l def?)) (length tail))
         (== (map cadr tail) labels))))

(define (mdtm-faithful-body l)
  ;; the body of a document of the style markdown, from its blocks: the
  ;; header as it is written, and each heading at its level
  (let* ((source (lambda (x) (and (func? x 'meta) (mdtm-attr x 'source))))
         (marks (append-map mdtm-footnote-labels l))
         ;; (with a table: there may be thousands of them)
         (labels (with seen (make-ahash-table)
                   (let loop ((l marks) (acc '()))
                     (cond ((null? l) (reverse acc))
                           ((ahash-ref seen (car l)) (loop (cdr l) acc))
                           (else (ahash-set! seen (car l) #t)
                                 (loop (cdr l) (cons (car l) acc)))))))
         ;; the marks in the texts of the notes and in the tables, which are
         ;; kept as they are
         (inner (append (append-map mdtm-inner-labels l)
                        (append-map mdtm-table-labels l)))
         (count (make-ahash-table)))
    (for-each (lambda (m) (ahash-set! count m (+ (or (ahash-ref count m) 0) 1)))
              (append marks inner))
    (set! mdtm-headings mdtm-faithful-sections)
    (set! mdtm-used-footnotes (make-ahash-table))
    (for-each (lambda (m) (ahash-set! mdtm-used-footnotes m #t)) labels)
    (set! mdtm-defs-last? (mdtm-notes-last? l labels))
    ;; (a note with several marks keeps its label: these refer to it)
    (set! mdtm-numbered-footnotes
          (let loop ((l labels) (i 1) (acc '()))
            (cond ((null? l) acc)
                  ((and (== (car l) (number->string i))
                        (== (ahash-ref count (car l)) 1))
                   (loop (cdr l) (+ i 1) (cons (car l) acc)))
                  (else (loop (cdr l) (+ i 1) acc)))))
    (mdtm-document
      (append-map (lambda (x)
                    (cond ((source x) (list (mdtm-metadata (source x))))
                          ((func? x 'meta) '())
                          (else (mdtm-block x))))
                  l))))

(define (mdtm-table-labels x)
  ;; the labels of the footnotes which are referred to in the tables which
  ;; are written as they were read
  (cond ((not (pair? x)) '())
        ((func? x 'footnote-def) '())
        ((and (func? x 'table) (mdtm-attr x 'source)) (mdtm-footnote-labels* x))
        (else (append-map mdtm-table-labels (cdr x)))))

(define (mdtm-inner-labels x)
  ;; the labels of the footnotes which are referred to in the texts of the
  ;; definitions of the footnotes
  (cond ((not (pair? x)) '())
        ((func? x 'footnote-def)
         (append-map (lambda (y) (mdtm-footnote-labels* y)) (cdr x)))
        (else (append-map mdtm-inner-labels (cdr x)))))

(define (mdtm-footnote-labels* x)
  (cond ((not (pair? x)) '())
        ((and (func? x 'footnote 1) (string? (cadr x))) (list (cadr x)))
        (else (append-map mdtm-footnote-labels* (cdr x)))))

(define (mdtm-body l)
  ;; the body of a document, from its blocks
  (let* ((meta (append-map mdtm-children
                           (list-filter l (lambda (x) (func? x 'meta)))))
         (blocks (list-filter l (lambda (x) (not (func? x 'meta)))))
         ;; a single heading of the first level, at the start, is the title
         ;; when the header gives none
         (title? (and (nnull? blocks) (func? (car blocks) 'h1)
                      (== (mdtm-count-headings blocks 'h1) 1)
                      (not (assoc 'title meta))))
         (title (and title? (mdtm-children (car blocks))))
         ;; the other headings are the sections: from the first level, or
         ;; from the second one under a title
         (shift? (or title?
                     (and (assoc 'title meta)
                          (== (mdtm-count-headings blocks 'h1) 0)))))
    (set! mdtm-headings
          (if shift? (cons #f mdtm-sections) mdtm-sections))
    (mdtm-document
      (append (mdtm-doc-data meta title)
              (mdtm-blocks (if title? (cdr blocks) blocks))))))

(tm-define (markdown->texmacs x . opts)
  (:type (-> stree stree))
  (:synopsis "Convert the Markdown tree @x into a TeXmacs tree")
  (let* ((file? (func? x '!file 1))
         (md (if file? (cadr x) x))
         (l (if (func? md 'markdown) (cdr md) (list md))))
    (set! mdtm-footnotes (make-ahash-table))
    (set! mdtm-open-footnotes '())
    (set! mdtm-headings mdtm-sections)
    ;; (with the option off, a file is a document of the style generic
    ;; with its title, as it was before the style markdown)
    (set! mdtm-faithful?
          (and file?
               (!= (assoc-ref (if (and (nnull? opts) (list? (car opts)))
                                  (car opts) '())
                              "markdown->texmacs:faithful")
                   "off")))
    (set! mdtm-numbered-footnotes '())
    (set! mdtm-used-footnotes (make-ahash-table))
    (set! mdtm-done-footnotes (make-ahash-table))
    (set! mdtm-defs-last? #f)
    (set! mdtm-in-kept-table? #f)
    (set! mdtm-formulas (make-ahash-table))
    (mdtm-collect-footnotes md)
    (with r (cond ((and file? mdtm-faithful?)
                   `(document (body ,(mdtm-faithful-body l))
                              (style "markdown")))
                  (file? `(document (body ,(mdtm-body l)) (style "generic")))
                  ;; a snippet of text only
                  ((list-and (map (lambda (b)
                                    (or (string? b)
                                        (not (in? (car b)
                                                  '(meta h1 h2 h3 h4 h5 h6 p
                                                    blockquote ul ol pre hr table
                                                    footnote-def ref-def)))))
                                  l))
                   (or (mdtm-figure l) (mdtm-inlines l)))
                  (else (with b (mdtm-blocks l)
                          (if (list-1? b) (car b) (mdtm-document b)))))
      (set! mdtm-footnotes (make-ahash-table))
      (set! mdtm-done-footnotes (make-ahash-table))
      (set! mdtm-formulas (make-ahash-table))
      (set! mdtm-faithful? #f)
      r)))
