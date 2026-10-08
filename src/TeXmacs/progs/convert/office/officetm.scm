
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : officetm.scm
;; DESCRIPTION : conversion of office trees into TeXmacs trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The office tree of a Word or of an OpenDocument text (office-tools.scm)
;; says what its paragraphs are: they become the tags of the standard
;; styles of TeXmacs. The formatting which says nothing about the structure
;; (fonts, sizes, colors, indentations) is not kept.

(texmacs-module (convert office officetm)
  (:use (convert office office-tools)
        (convert tools environment)
        (convert mathml mathtm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (oftm-text s)
  ;; the text in the encoding of TeXmacs, without the characters of no
  ;; width which the programs put around formulas and fields
  (string-replace (string-replace (utf8->cork s) "<#200B>" "") "<#FEFF>" ""))

(define (oftm-concat l)
  ;; a list of TeXmacs trees as one tree
  (let* ((l (let loop ((l l))
              (cond ((null? l) l)
                    ((== (car l) "") (loop (cdr l)))
                    ((and (string? (car l)) (pair? (cdr l)) (string? (cadr l)))
                     (loop (cons (string-append (car l) (cadr l)) (cddr l))))
                    (else (cons (car l) (loop (cdr l))))))))
    (cond ((null? l) "")
          ((null? (cdr l)) (car l))
          (else (cons 'concat l)))))

(define (oftm-document l)
  (if (null? l) '(document "") (cons 'document l)))

(define (oftm-length t)
  ;; a rough measure of the width of a piece of text
  (cond ((string? t) (string-length t))
        ((func? t 'raw-data) 0)
        ((pair? t) (apply + (map oftm-length (cdr t))))
        (else 0)))

(define (oftm-plain x)
  ;; the text of an office node, for code
  (cond ((string? x) (oftm-text x))
        ((func? x 'br) "\n")
        ((func? x 'tab) "    ")
        ((and (pair? x) (in? (car x) '(note image math bookmark))) "")
        ((pair? x) (apply string-append (map oftm-plain (ox-children x))))
        (else "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics and images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (oftm-local-name tag)
  ;; the name of an element without its prefix
  (let* ((s (symbol->string tag))
         (i (string-search-forwards ":" 0 s)))
    (if (>= i 0) (substring s (+ i 1) (string-length s)) s)))

(define (oftm-mathml x)
  ;; The element of MathML x as the converter of MathML wants it: with the
  ;; prefix m: for its elements, whatever prefix they have or have not in
  ;; the file. The annotations are other writings of the same formula.
  (cond ((not (pair? x)) x)
        ((func? x '@) x)
        (else
          (with name (oftm-local-name (car x))
            (cons (string->symbol (string-append "m:" name))
                  (map oftm-mathml
                       (list-filter
                         (cdr x)
                         (lambda (y)
                           (not (and (pair? y) (symbol? (car y))
                                     (in? (oftm-local-name (car y))
                                          '("annotation" "annotation-xml"))))))))))))

(define (oftm-from-mathml x)
  ;; the tree of TeXmacs for the element of MathML x; a formula which the
  ;; converter cannot read is not lost: its text is kept
  (catch #t
    (lambda ()
      (let ((env (environment))
            (root (oftm-mathml x)))
        (initialize-xpath env root (cut mathtm-as-serial <> root))))
    (lambda args (oftm-text (ox-text (oftm-mathml x))))))

;; The spaces of Unicode in a formula, as the converter of MathML leaves
;; them, and what they are in TeXmacs: a space between a function and its
;; argument, spaces of a width, or nothing.
(define oftm-math-spaces
  '(("<nospace>" " ") ("<#200B>") ("<#2060>") ("<#2063>") ("<#2064>")
    ("<#2001>" (space "1em")) ("<#2003>" (space "1em"))
    ("<#2000>" (space "0.5em")) ("<#2002>" (space "0.5em"))
    ("<#2004>" (space "0.33em")) ("<#2005>" (space "0.25em"))
    ("<#2006>" (space "0.17em")) ("<#2009>" (space "0.17em"))
    ("<#200A>" (space "0.1em")) ("<#205F>" (space "0.22em"))
    ("<#A0>" (space "0.33em")) ("<varspace>" (space "0.33em"))))

(define (oftm-split-spaces s l)
  ;; the pieces of the string s around the spaces of the list l
  (if (null? l) (list s)
      (let* ((what (caar l))
             (i (string-search-forwards what 0 s)))
        (if (< i 0) (oftm-split-spaces s (cdr l))
            (append (oftm-split-spaces (substring s 0 i) (cdr l))
                    (cdar l)
                    (oftm-split-spaces
                      (substring s (+ i (string-length what)) (string-length s))
                      l))))))

(define (oftm-clean-math t)
  (cond ((string? t) (oftm-concat (oftm-split-spaces t oftm-math-spaces)))
        ((and (pair? t) (func? t 'concat))
         (oftm-concat
           (append-map (lambda (x)
                         (if (string? x) (oftm-split-spaces x oftm-math-spaces)
                             (list (oftm-clean-math x))))
                       (cdr t))))
        ((pair? t) (cons (car t) (map oftm-clean-math (cdr t))))
        (else t)))

(define (oftm-formula x)
  ;; the formula of a node math, as a tree of TeXmacs in math mode
  (let* ((form (ox-attr x 'form))
         (c (ox-children x))
         (r (cond ((null? c) "")
                  ((== form "text") (oftm-text (car c)))
                  ((== form "sxml") (oftm-from-mathml (car c)))
                  ;; the text of a file of MathML
                  (else
                    (with root (catch #t (lambda () (ox-root (parse-xml (car c))))
                                      (lambda args #f))
                      (if root (oftm-from-mathml root) ""))))))
    ;; the converter of MathML gives the formula inside a tag math or
    ;; equation*, as its attribute display says: the node says it better
    (let loop ((r r))
      (if (or (func? r 'math 1) (func? r 'equation* 1) (func? r 'document 1))
          (loop (cadr r))
          (oftm-clean-math r)))))

(define (oftm-math x)
  (with f (oftm-formula x)
    (cond ((== f "") '())
          ((== (ox-attr x 'display) "true")
           (list `(equation* (document ,f))))
          (else (list `(math ,f))))))

(define (oftm-image x)
  (let* ((name (oftm-text (or (ox-attr x 'name) "")))
         (data (ox-attr x 'data))
         (w (or (ox-attr x 'width) ""))
         (h (or (ox-attr x 'height) "")))
    (cond (data (list `(image (tuple (raw-data ,data) ,name) ,w ,h "" "")))
          ((!= name "") (list `(image ,name ,w ,h "" "")))
          (else '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (oftm-wrap tag l)
  (with t (oftm-inlines l)
    (if (== t "") '() (list (list tag t)))))

(define (oftm-inline x)
  ;; the list of TeXmacs trees for an inline node
  (cond ((string? x) (list (oftm-text x)))
        ((not (pair? x)) '())
        (else
          (let ((l (ox-children x)))
            (case (car x)
              ((em) (oftm-wrap 'em l))
              ((strong) (oftm-wrap 'strong l))
              ((underline) (oftm-wrap 'underline l))
              ((strike) (oftm-wrap 'strike-through l))
              ((sub) (oftm-wrap 'rsub l))
              ((sup) (oftm-wrap 'rsup l))
              ((code) (oftm-wrap 'verbatim l))
              ((smallcaps)
               (with t (oftm-inlines l)
                 (if (== t "") '() (list `(with "font-shape" "small-caps" ,t)))))
              ((link)
               (let ((t (oftm-inlines l))
                     (href (oftm-text (or (ox-attr x 'href) ""))))
                 (cond ((== t "") '())
                       ((== href "") (list t))
                       (else (list `(hlink ,t ,href))))))
              ((ref)
               ;; the text of the reference, as a link to its bookmark
               (let ((t (oftm-inlines l))
                     (name (oftm-text (or (ox-attr x 'name) ""))))
                 (cond ((== t "") '())
                       ((== name "") (list t))
                       (else (list `(hlink ,t ,(string-append "#" name)))))))
              ((bookmark)
               (with name (ox-attr x 'name)
                 (if name (list `(label ,(oftm-text name))) '())))
              ((note)
               (with b (oftm-blocks l)
                 (cond ((null? b) '())
                       ((null? (cdr b)) (list `(footnote ,(car b))))
                       (else (list `(footnote ,(oftm-document b)))))))
              ((br) '((next-line)))
              ((tab) '((space "2em")))
              ((pagebreak) '((page-break)))
              ((image) (oftm-image x))
              ((math) (oftm-math x))
              (else (append-map oftm-inline l)))))))

(define (oftm-inlines l)
  (oftm-concat (append-map oftm-inline l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (oftm-cell-align c)
  ;; the alignment of a cell of one paragraph, or #f
  (let* ((l (ox-children c))
         (a (and (list-1? l) (func? (car l) 'p) (ox-attr (car l) 'align))))
    (cond ((== a "center") "c")
          ((== a "right") "r")
          (else #f))))

(define (oftm-cell-body c)
  ;; the contents of a cell: text, or a document of several paragraphs
  (let* ((l (ox-children c))
         (b (if (and (list-1? l) (func? (car l) 'p))
                ;; (its alignment is the one of the cell)
                (list (oftm-inlines (ox-children (car l))))
                (oftm-blocks l)))
         (b (if (and (== (ox-attr c 'header) "true"))
                (map (lambda (t) (if (or (== t "") (func? t 'strong)) t
                                     `(strong ,t)))
                     b)
                b)))
    (cond ((null? b) "")
          ((null? (cdr b)) (car b))
          (else (cons 'document b)))))

(define (oftm-runs row)
  ;; the runs (first last value) of the equal values of a list which are
  ;; not #f, with the ranks from 1 on
  (let loop ((l row) (j 1) (acc '()))
    (cond ((null? l) (reverse acc))
          ((not (car l)) (loop (cdr l) (+ j 1) acc))
          ((and (pair? acc) (== (cadar acc) (- j 1)) (== (caddar acc) (car l)))
           (loop (cdr l) (+ j 1)
                 (cons (list (caar acc) j (car l)) (cdr acc))))
          (else (loop (cdr l) (+ j 1) (cons (list j j (car l)) acc))))))

(define (oftm-rectangles matrix)
  ;; the rectangles (row1 row2 col1 col2 value) which cover the values of
  ;; a matrix which are not #f: the runs of its rows, taken together when
  ;; the rows which follow each other have the same ones
  (let loop ((rows matrix) (i 1) (open '()) (acc '()))
    ;; open: the runs of the row before, each with the row where it starts
    (let* ((runs (if (null? rows) '() (oftm-runs (car rows))))
           (closed (list-filter open (lambda (o) (not (in? (car o) runs)))))
           (acc (append (map (lambda (o)
                               (list (cadr o) (- i 1) (caar o) (cadar o) (caddar o)))
                             closed)
                        acc)))
      (if (null? rows) (reverse acc)
          (loop (cdr rows) (+ i 1)
                (map (lambda (r)
                       (with o (list-find open (lambda (o) (== (car o) r)))
                         (list r (if o (cadr o) i))))
                     runs)
                acc)))))

(define (oftm-cwiths matrix var value)
  ;; the format var of the cells of the matrix whose value is not #f:
  ;; with this value, or with their own when value is #f
  (map (lambda (r)
         `(cwith ,(number->string (car r)) ,(number->string (cadr r))
                 ,(number->string (caddr r)) ,(number->string (cadddr r))
                 ,var ,(or value (list-ref r 4))))
       (oftm-rectangles matrix)))

(define (oftm-part s)
  ;; a part of the width of the paragraph as a number: "0.7par" is 0.7
  (or (and s (string-ends? s "par")
           (string->number (substring s 0 (- (string-length s) 3))))
      1.0))

(define (oftm-table x)
  ;; A table. The cells which a wider or a higher one covers are there,
  ;; empty, as in the tables of TeXmacs. The borders and the backgrounds
  ;; of the cells are kept, as few formats of rectangles of cells; a table
  ;; which says nothing of its borders has all of them.
  (let* ((rows (list-filter (ox-children x) (lambda (r) (func? r 'row))))
         (ncols (apply max (cons 1 (map (lambda (r) (length (ox-children r)))
                                        rows))))
         (grid (map (lambda (r)
                      (with cells (ox-children r)
                        (append cells
                                (map (lambda (k) '(cell))
                                     (iota (- ncols (length cells)))))))
                    rows))
         (formats '())
         (wide? #f)
         (trows
           (map (lambda (cells i)
                  (cons 'row
                        (map (lambda (c j)
                               (let ((is (number->string i))
                                     (js (number->string j))
                                     (body (oftm-cell-body c)))
                                 (for (span '((colspan "cell-col-span")
                                              (rowspan "cell-row-span")))
                                   (with v (ox-attr c (car span))
                                     (when v
                                       (set! formats
                                             (cons `(cwith ,is ,is ,js ,js
                                                           ,(cadr span) ,v)
                                                   formats)))))
                                 (when (func? body 'document) (set! wide? #t))
                                 (list 'cell body)))
                             cells (map (lambda (j) (+ j 1)) (iota (length cells))))))
                grid (map (lambda (i) (+ i 1)) (iota (length grid)))))
         (aligns (oftm-cwiths (map (lambda (cells) (map oftm-cell-align cells)) grid)
                              "cell-halign" #f))
         ;; the borders: a letter of the sides of each cell
         (borders? (list-or (map (lambda (cells)
                                   (list-or (map (lambda (c) (ox-attr c 'borders))
                                                 cells)))
                                 grid)))
         (side (lambda (letter)
                 (map (lambda (cells)
                        (map (lambda (c)
                               (with b (ox-attr c 'borders)
                                 (and b (>= (string-search-forwards letter 0 b) 0))))
                             cells))
                      grid)))
         (all? (lambda (m)
                 (list-and (map (lambda (row cells)
                                  (list-and (map (lambda (v c)
                                                   (or v (ox-attr c 'covered)))
                                                 row cells)))
                                m grid))))
         (sides (map side '("t" "b" "l" "r")))
         (block? (or (not borders?) (list-and (map all? sides))))
         (lines (if block? '()
                    (append-map (lambda (m var) (oftm-cwiths m var "1ln"))
                                sides
                                '("cell-tborder" "cell-bborder" "cell-lborder"
                                  "cell-rborder"))))
         (fills (oftm-cwiths (map (lambda (cells)
                                    (map (lambda (c) (ox-attr c 'background)) cells))
                                  grid)
                             "cell-background" #f))
         (width (let loop ((rows trows) (w '()))
                  ;; the sum of the widths of the columns, in characters
                  (if (null? rows) (apply + w)
                      (let sub ((l (map oftm-length (cdar rows))) (w w) (r '()))
                        (cond ((and (null? l) (null? w)) (loop (cdr rows) (reverse r)))
                              ((null? l) (sub l (cdr w) (cons (car w) r)))
                              ((null? w) (sub (cdr l) w (cons (car l) r)))
                              (else (sub (cdr l) (cdr w)
                                         (cons (max (car l) (car w)) r))))))))
         ;; a table of the width of the text, or of a part of it: when it
         ;; says so, or when its text would not fit otherwise
         (twidth (ox-attr x 'width))
         (wide? (or wide? (> width 72)))
         (cols (with c (ox-attr x 'columns)
                 (and c (map string->number (string-tokenize-by-char c #\space)))))
         (cols (and cols (== (length cols) ncols) (list-and cols) cols))
         (wide (if (or wide? twidth)
                   `((twith "table-width" ,(or twidth "1par"))
                     (twith "table-hmode" "exact")
                     (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                     ,@(if (not cols) '()
                           (append-map
                             (lambda (part j)
                               (with js (number->string j)
                                 `((cwith "1" "-1" ,js ,js "cell-hmode" "exact")
                                   (cwith "1" "-1" ,js ,js "cell-width"
                                          ,(string-append
                                             (number->string
                                               (/ (round (* 1000.0 part
                                                            (oftm-part twidth)))
                                                  1000.0))
                                             "par")))))
                             cols (map (lambda (j) (+ j 1)) (iota ncols)))))
                   '()))
         (table `(,(if block? 'block 'tabular)
                  (tformat ,@wide ,@(reverse formats) ,@aligns ,@lines ,@fills
                           (table ,@trows))))
         (align (ox-attr x 'align)))
    (cond ((== align "center") `(with "par-mode" "center" ,table))
          ((== align "right") `(with "par-mode" "right" ,table))
          (else table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define oftm-sections
  '(section subsection subsubsection paragraph subparagraph))

(define (oftm-heading x)
  (let* ((n (or (string->number (or (ox-attr x 'level) "1")) 1))
         (tag (list-ref oftm-sections (max 0 (min (- n 1) 4))))
         (t (oftm-inlines (ox-children x))))
    (if (== t "") '() (list (list tag t)))))

(define (oftm-aligned x t)
  ;; the paragraph t with the alignment of the node x
  (with a (ox-attr x 'align)
    (cond ((== a "center") `(with "par-mode" "center" ,t))
          ((== a "right") `(with "par-mode" "right" ,t))
          (else t))))

(define (oftm-item x)
  ;; the paragraphs of an item of a list: the first one starts with the item
  (with l (oftm-blocks (ox-children x))
    (if (null? l) (list '(concat (item) ""))
        (cons (oftm-concat (cons '(item) (if (func? (car l) 'concat)
                                             (cdar l)
                                             (list (car l)))))
              (cdr l)))))

(define (oftm-list x)
  (let ((tag (if (== (ox-attr x 'kind) "number") 'enumerate 'itemize))
        (items (list-filter (ox-children x) (lambda (y) (func? y 'item)))))
    (if (null? items) '()
        (list `(,tag ,(oftm-document (append-map oftm-item items)))))))

(define (oftm-role? x role)
  (and (func? x 'p) (== (ox-attr x 'role) role)))

(define (oftm-only-images? x)
  ;; a paragraph of images and nothing else
  (and (func? x 'p) (in? (ox-attr x 'role) '(#f "figure"))
       (with l (list-filter (ox-children x)
                            (lambda (y) (not (and (string? y)
                                                  (== (string-trim-spaces y) "")))))
         (and (pair? l)
              (list-and (map (lambda (y) (func? y 'image)) l))))))

(define (oftm-only-formula x)
  ;; the formula of a paragraph which holds nothing else, or #f
  (with l (list-filter (ox-children x)
                       (lambda (y) (not (and (string? y)
                                             (== (string-trim-spaces y) "")))))
    (and (list-1? l) (func? (car l) 'math) (car l))))

(define (oftm-trim-right s)
  (let loop ((i (string-length s)))
    (if (and (> i 0) (char=? (string-ref s (- i 1)) #\space))
        (loop (- i 1))
        (substring s 0 i))))

(define (oftm-description l)
  ;; (description . rest) for the terms and their definitions at the
  ;; start of the blocks l
  (let loop ((l l) (acc '()))
    (cond ((and (pair? l) (oftm-role? (car l) "term"))
           (let* ((term (oftm-inlines (ox-children (car l))))
                  (defs (let sub ((r (cdr l)) (d '()))
                          (if (and (pair? r) (oftm-role? (car r) "definition"))
                              (sub (cdr r)
                                   (cons (oftm-inlines (ox-children (car r))) d))
                              (cons (reverse d) r))))
                  (body (car defs)))
             (loop (cdr defs)
                   (append (reverse
                             (cons (oftm-concat
                                     (cons `(item* ,term)
                                           (cond ((null? body) '())
                                                 ((func? (car body) 'concat)
                                                  (cdar body))
                                                 (else (list (car body))))))
                                   (if (null? body) '() (cdr body))))
                           acc))))
          (else (cons `(description ,(oftm-document (reverse acc))) l)))))

(define (oftm-caption x)
  (oftm-inlines (ox-children x)))

(define (oftm-blocks l)
  ;; the paragraphs of TeXmacs for a list of blocks. The blocks which
  ;; belong together are taken together: the lines of a quotation or of a
  ;; piece of code, a figure or a table and its caption.
  (cond ((null? l) '())
        ((not (pair? (car l))) (oftm-blocks (cdr l)))
        ;; a quotation: the paragraphs of this style which follow each other
        ((oftm-role? (car l) "quote")
         (let loop ((r l) (acc '()))
           (if (and (pair? r) (oftm-role? (car r) "quote"))
               (loop (cdr r) (cons (oftm-inlines (ox-children (car r))) acc))
               (cons `(quotation ,(oftm-document (reverse acc)))
                     (oftm-blocks r)))))
        ;; code: each paragraph is a line, or several
        ((oftm-role? (car l) "code")
         (let loop ((r l) (acc '()))
           (if (and (pair? r) (oftm-role? (car r) "code"))
               (loop (cdr r)
                     (append (reverse (map oftm-trim-right
                                           (string-tokenize-by-char
                                             (oftm-plain (car r)) #\newline)))
                             acc))
               (cons `(verbatim-code ,(oftm-document (reverse acc)))
                     (oftm-blocks r)))))
        ;; terms and their definitions
        ((oftm-role? (car l) "term")
         (with r (oftm-description l)
           (cons (car r) (oftm-blocks (cdr r)))))
        ;; a figure and its caption, in this order or in the other
        ((and (oftm-only-images? (car l)) (pair? (cdr l))
              (oftm-role? (cadr l) "caption"))
         (cons `(big-figure ,(oftm-inlines (ox-children (car l)))
                            ,(oftm-caption (cadr l)))
               (oftm-blocks (cddr l))))
        ((and (oftm-role? (car l) "caption") (pair? (cdr l))
              (oftm-only-images? (cadr l)))
         (cons `(big-figure ,(oftm-inlines (ox-children (cadr l)))
                            ,(oftm-caption (car l)))
               (oftm-blocks (cddr l))))
        ;; a table and its caption
        ((and (func? (car l) 'table) (pair? (cdr l))
              (oftm-role? (cadr l) "caption"))
         (cons `(big-table ,(oftm-table (car l)) ,(oftm-caption (cadr l)))
               (oftm-blocks (cddr l))))
        ((and (oftm-role? (car l) "caption") (pair? (cdr l))
              (func? (cadr l) 'table))
         (cons `(big-table ,(oftm-table (cadr l)) ,(oftm-caption (car l)))
               (oftm-blocks (cddr l))))
        (else
          (let ((x (car l)))
            (append
              (case (car x)
                ((p)
                 (cond ((oftm-role? x "heading") (oftm-heading x))
                       ((oftm-role? x "skip") '())
                       ;; a formula alone in its paragraph is displayed
                       ((oftm-only-formula x)
                        => (lambda (m)
                             (with f (oftm-formula m)
                               (if (== f "") '()
                                   (list `(equation* (document ,f)))))))
                       (else
                         (with t (oftm-inlines (ox-children x))
                           (if (== t "") '() (list (oftm-aligned x t)))))))
                ((list) (oftm-list x))
                ((table) (list (oftm-table x)))
                ((pagebreak) '((page-break)))
                (else '()))
              (oftm-blocks (cdr l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The title of the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define oftm-title-roles
  '("title" "subtitle" "author" "date" "abstract" "skip"))

(define (oftm-title-block? x)
  (and (func? x 'p) (in? (ox-attr x 'role) oftm-title-roles)))

(define (oftm-doc-data l meta)
  ;; the title, from the paragraphs l at the start of the document which
  ;; have the styles of a title, or else from the properties of the file
  (let* ((get (lambda (role)
                (map (lambda (x) (oftm-inlines (ox-children x)))
                     (list-filter l (lambda (x) (oftm-role? x role))))))
         (titles (get "title"))
         (titles (if (and (null? titles) (assoc 'title meta))
                     (list (oftm-text (cadr (assoc 'title meta))))
                     titles))
         (subtitles (get "subtitle"))
         (authors (get "author"))
         (dates (get "date"))
         (abstract (get "abstract")))
    (append
      (if (and (null? titles) (null? authors) (null? dates)) '()
          `((doc-data
              ,@(if (null? titles) '() `((doc-title ,(car titles))))
              ,@(if (null? subtitles) '() `((doc-subtitle ,(car subtitles))))
              ,@(map (lambda (a) `(doc-author (author-data (author-name ,a))))
                     authors)
              ,@(if (null? dates) '() `((doc-date ,(car dates)))))))
      (if (null? abstract) '()
          `((abstract-data (abstract ,(oftm-document abstract))))))))

(define (oftm-body l)
  (let* ((meta (append-map cdr (list-filter l (lambda (x) (func? x 'meta)))))
         (blocks (list-filter l (lambda (x) (not (func? x 'meta)))))
         (title (let loop ((r blocks) (acc '()))
                  (if (and (pair? r) (oftm-title-block? (car r)))
                      (loop (cdr r) (cons (car r) acc))
                      (reverse acc))))
         (rest (list-tail blocks (length title))))
    (oftm-document (append (oftm-doc-data title meta) (oftm-blocks rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (office->texmacs x)
  (:type (-> stree stree))
  (:synopsis "Convert the office tree @x into a TeXmacs document")
  (with l (if (func? x 'office) (cdr x) (list x))
    `(document (body ,(oftm-body l)) (style "generic"))))
