
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

(texmacs-module (convert markdown markdowntm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State and tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the definitions of the footnotes, and the tags of the six levels of
;; headings, for the document which is converted
(define mdtm-footnotes (make-ahash-table))
(define mdtm-headings '())

(define mdtm-sections
  '(section subsection subsubsection paragraph subparagraph subparagraph))

(define (mdtm-attr x name)
  (and (pair? x) (pair? (cdr x)) (func? (cadr x) '@)
       (with a (assoc name (cdadr x))
         (and a (cadr a)))))

(define (mdtm-children x)
  (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cddr x) (cdr x)))

(define (mdtm-text s) (utf8->cork s))

(define (mdtm-merge l)
  ;; with the strings which follow each other as one
  (cond ((null? l) l)
        ((== (car l) "") (mdtm-merge (cdr l)))
        ((and (string? (car l)) (nnull? (cdr l)) (string? (cadr l)))
         (mdtm-merge (cons (string-append (car l) (cadr l)) (cddr l))))
        (else (cons (car l) (mdtm-merge (cdr l))))))

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

(define (mdtm-math s)
  (or (mdtm-latex (string-append "$" s "$"))
      `(math ,(mdtm-text s))))

(define (mdtm-environment? s)
  ;; a formula which is an environment of LaTeX by itself, as align
  (with t (string-trim-spaces s)
    (and (string-starts? t "\\begin{")
         (list-or (map (lambda (e) (string-starts? t (string-append "\\begin{" e)))
                       '("align" "eqnarray" "gather" "multline" "equation"
                         "flalign" "alignat"))))))

(define (mdtm-display-math s)
  (or (mdtm-latex (if (mdtm-environment? s) s (string-append "\\[" s "\\]")))
      `(equation* (document ,(mdtm-text s)))))

(define (mdtm-html s)
  ;; the HTML snippet s as a TeXmacs tree; a comment, or what TeXmacs does
  ;; not understand, is dropped
  (if (string-starts? (string-trim-spaces s) "<!--") ""
      (with r (catch #t
                (lambda () (convert s "html-snippet" "texmacs-stree"))
                (lambda args #f))
        (if r (mdtm-unary-document r) ""))))

(define mdtm-languages
  '(("c" . cpp-code) ("cpp" . cpp-code) ("c++" . cpp-code) ("cc" . cpp-code)
    ("h" . cpp-code) ("hpp" . cpp-code)
    ("python" . python-code) ("py" . python-code) ("python3" . python-code)
    ("scheme" . scm-code) ("scm" . scm-code) ("lisp" . scm-code)
    ("guile" . scm-code)
    ("sh" . shell-code) ("bash" . shell-code) ("shell" . shell-code)
    ("zsh" . shell-code) ("console" . shell-code)
    ("java" . java-code) ("javascript" . javascript-code)
    ("js" . javascript-code) ("json" . json-code) ("julia" . julia-code)
    ("r" . r-code) ("scala" . scala-code) ("fortran" . fortran-code)
    ("octave" . octave-code) ("matlab" . octave-code) ("dot" . dot-code)
    ("scilab" . scilab-code) ("mathemagix" . mmx-code) ("mmx" . mmx-code)))

(define (mdtm-pre x)
  (let* ((lang (locase-all (or (mdtm-attr x 'lang) "")))
         (tag (or (assoc-ref mdtm-languages lang) 'verbatim-code))
         (s (apply string-append (list-filter (mdtm-children x) string?))))
    `(,tag ,(mdtm-document (map mdtm-text (mdtm-lines s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdtm-footnote label)
  (with def (ahash-ref mdtm-footnotes label)
    (if def
        `(footnote ,(with l (mdtm-blocks def)
                      (if (list-1? l) (car l) (mdtm-document l))))
        (string-append "[^" (mdtm-text label) "]"))))

(define (mdtm-inline x)
  ;; the TeXmacs trees for an inline node
  (cond ((string? x) (list (mdtm-text x)))
        ((not (pair? x)) '())
        (else
          (let ((l (mdtm-children x)))
            (case (car x)
              ((em) `((em ,(mdtm-inlines l))))
              ((strong) `((strong ,(mdtm-inlines l))))
              ((del) `((strike-through ,(mdtm-inlines l))))
              ((code)
               `((verbatim ,(mdtm-text (apply string-append
                                              (list-filter l string?))))))
              ((a)
               `((hlink ,(mdtm-inlines l)
                        ,(mdtm-text (or (mdtm-attr x 'href) "")))))
              ((img)
               `((image ,(mdtm-text (or (mdtm-attr x 'src) "")) "" "" "" "")))
              ((br) '((next-line)))
              ((math) (list (mdtm-math (apply string-append l))))
              ((displaymath) (list (mdtm-display-math (apply string-append l))))
              ((html) (list (mdtm-html (apply string-append l))))
              ((footnote) (list (mdtm-footnote (apply string-append l))))
              (else (append-map mdtm-inline l)))))))

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
  `(,tag ,(mdtm-document (append-map mdtm-item (mdtm-children x)))))

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
              ((p) (list (mdtm-inlines l)))
              ((blockquote) `((quotation ,(mdtm-document (mdtm-blocks l)))))
              ((ul) (list (mdtm-list x 'itemize)))
              ((ol) (list (mdtm-list x 'enumerate)))
              ((pre) (list (mdtm-pre x)))
              ((hr) '((hrule)))
              ((table) (list (mdtm-table x)))
              ((displaymath) (list (mdtm-display-math (apply string-append l))))
              ((html)
               (with r (mdtm-html (apply string-append l))
                 (cond ((== r "") '())
                       ((func? r 'document) (cdr r))
                       (else (list r)))))
              ((footnote-def meta) '())
              (else (list (mdtm-inlines (list x)))))))))

(define (mdtm-blocks l)
  (append-map mdtm-block l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdtm-collect-footnotes x)
  (when (pair? x)
    (if (and (func? x 'footnote-def) (pair? (cdr x)) (string? (cadr x)))
        (ahash-set! mdtm-footnotes (cadr x) (cddr x))
        (for-each mdtm-collect-footnotes (cdr x)))))

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

(define (mdtm-body l)
  ;; the body of a document, from its blocks
  (let* ((meta (append-map cdr (list-filter l (lambda (x) (func? x 'meta)))))
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

(tm-define (markdown->texmacs x)
  (:type (-> stree stree))
  (:synopsis "Convert the Markdown tree @x into a TeXmacs tree")
  (let* ((file? (func? x '!file 1))
         (md (if file? (cadr x) x))
         (l (if (func? md 'markdown) (cdr md) (list md))))
    (set! mdtm-footnotes (make-ahash-table))
    (set! mdtm-headings mdtm-sections)
    (mdtm-collect-footnotes md)
    (with r (cond (file? `(document (body ,(mdtm-body l)) (style "generic")))
                  ;; a snippet of text only
                  ((list-and (map (lambda (b)
                                    (or (string? b)
                                        (not (in? (car b)
                                                  '(meta h1 h2 h3 h4 h5 h6 p
                                                    blockquote ul ol pre hr table
                                                    footnote-def)))))
                                  l))
                   (mdtm-inlines l))
                  (else (with b (mdtm-blocks l)
                          (if (list-1? b) (car b) (mdtm-document b)))))
      (set! mdtm-footnotes (make-ahash-table))
      r)))
