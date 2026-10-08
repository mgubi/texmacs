
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

(define (oftm-text s) (utf8->cork s))

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
  ;; the tree of TeXmacs for the element of MathML x, or ""
  (catch #t
    (lambda ()
      (let ((env (environment))
            (root (oftm-mathml x)))
        (initialize-xpath env root (cut mathtm-as-serial <> root))))
    (lambda args "")))

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
          r))))

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

(define (oftm-table x)
  ;; a table with borders. The cells which a wider or a higher one covers
  ;; are there, empty, as in the tables of TeXmacs.
  (let* ((rows (list-filter (ox-children x) (lambda (r) (func? r 'row))))
         (ncols (apply max (cons 1 (map (lambda (r) (length (ox-children r)))
                                        rows))))
         (formats '())
         (wide? #f)
         (trows
           (map (lambda (r i)
                  (let* ((cells (ox-children r))
                         (cells (append cells
                                        (map (lambda (k) '(cell))
                                             (iota (- ncols (length cells)))))))
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
                                   (with a (oftm-cell-align c)
                                     (when a
                                       (set! formats
                                             (cons `(cwith ,is ,is ,js ,js
                                                           "cell-halign" ,a)
                                                   formats))))
                                   (when (func? body 'document) (set! wide? #t))
                                   (list 'cell body)))
                               cells (map (lambda (j) (+ j 1))
                                          (iota (length cells)))))))
                rows (map (lambda (i) (+ i 1)) (iota (length rows)))))
         (width (let loop ((rows trows) (w '()))
                  ;; the sum of the widths of the columns, in characters
                  (if (null? rows) (apply + w)
                      (let sub ((l (map oftm-length (cdar rows))) (w w) (r '()))
                        (cond ((and (null? l) (null? w)) (loop (cdr rows) (reverse r)))
                              ((null? l) (sub l (cdr w) (cons (car w) r)))
                              ((null? w) (sub (cdr l) w (cons (car l) r)))
                              (else (sub (cdr l) (cdr w)
                                         (cons (max (car l) (car w)) r))))))))
         (wide (if (or wide? (> width 72))
                   `((twith "table-width" "1par")
                     (twith "table-hmode" "exact")
                     (cwith "1" "-1" "1" "-1" "cell-hyphen" "t"))
                   '())))
    `(block (tformat ,@wide ,@(reverse formats) (table ,@trows)))))

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
  (and (func? x 'p) (not (ox-attr x 'role))
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
                     (append (reverse (string-tokenize-by-char
                                        (oftm-plain (car r)) #\newline))
                             acc))
               (cons `(verbatim-code ,(oftm-document (reverse acc)))
                     (oftm-blocks r)))))
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

(define oftm-title-roles '("title" "subtitle" "author" "date" "abstract"))

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
