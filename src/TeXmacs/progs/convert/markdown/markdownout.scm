
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdownout.scm
;; DESCRIPTION : writing Markdown trees as Markdown
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Markdown trees are described in markdownin.scm. A block is written as
;; a list of lines, so that a block inside another one (a quote, an item of a
;; list) is the same lines with something before each of them. A paragraph is
;; one line: Markdown joins the lines of a paragraph anyway, and long lines
;; survive the tools which wrap text better than wrapped ones.

(texmacs-module (convert markdown markdownout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees and strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdout-attr x name)
  (and (pair? x) (pair? (cdr x)) (func? (cadr x) '@)
       (with a (assoc name (cdadr x))
         (and a (cadr a)))))

(define (mdout-children x)
  (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cddr x) (cdr x)))

(define (mdout-join l sep)
  (cond ((null? l) "")
        ((null? (cdr l)) (car l))
        (else (apply string-append
                     (cons (car l)
                           (append-map (lambda (x) (list sep x)) (cdr l)))))))

(define (mdout-split s)
  ;; the lines of a string
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((>= i (string-length s))
           (reverse (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
          (else (loop (+ i 1) start acc)))))

(define (mdout-width s)
  ;; the number of characters of a string in UTF-8
  (let loop ((i 0) (n 0))
    (if (>= i (string-length s)) n
        (with c (char->integer (string-ref s i))
          (loop (+ i 1) (if (and (>= c 128) (< c 192)) n (+ n 1)))))))

(define (mdout-longest-run s c)
  ;; the length of the longest run of the character c in s
  (let loop ((i 0) (cur 0) (best 0))
    (cond ((>= i (string-length s)) (max cur best))
          ((char=? (string-ref s i) c) (loop (+ i 1) (+ cur 1) best))
          (else (loop (+ i 1) 0 (max cur best))))))

(define (mdout-alnum? c)
  (or (char-alphabetic? c) (char-numeric? c) (>= (char->integer c) 128)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only the characters which would be taken for markup are escaped: a text
;; with a backslash before each punctuation is hard to read.

(define (mdout-escape s cell?)
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (if (>= i n) (apply string-append (reverse acc))
          (let* ((c (string-ref s i))
                 (prev (and (> i 0) (string-ref s (- i 1))))
                 (next (and (< (+ i 1) n) (string-ref s (+ i 1))))
                 (esc? (cond ((in? c '(#\\ #\` #\$)) #t)
                             ;; between spaces, * is not emphasis
                             ((char=? c #\*)
                              (not (and prev next (char=? prev #\space)
                                        (char=? next #\space))))
                             ;; brackets make nothing without what follows
                             ;; them: a link, a footnote, a task
                             ((char=? c #\[)
                              (and next
                                   (or (char=? next #\^)
                                       (and (in? next '(#\space #\x #\X))
                                            (< (+ i 2) n)
                                            (char=? (string-ref s (+ i 2)) #\])))))
                             ((char=? c #\])
                              (and next (in? next '(#\( #\:))))
                             ;; inside a word, _ is not emphasis
                             ((char=? c #\_)
                              (not (and prev next (mdout-alnum? prev)
                                        (mdout-alnum? next))))
                             ((char=? c #\<)
                              (and next (or (char-alphabetic? next)
                                            (in? next '(#\/ #\! #\?)))))
                             ((char=? c #\&)
                              (and next (or (char-alphabetic? next)
                                            (char=? next #\#))))
                             ((char=? c #\~) (and next (char=? next #\~)))
                             ((char=? c #\|) cell?)
                             (else #f))))
            (loop (+ i 1)
                  (cons (if esc? (string #\\ c) (string c)) acc)))))))

(define (mdout-escape-start s)
  ;; a line of a paragraph, which must not start as another block
  (let* ((n (string-length s))
         (i (let loop ((i 0))
              (if (and (< i n) (char=? (string-ref s i) #\space)) (loop (+ i 1)) i)))
         (t (substring s i n))
         (m (string-length t))
         (follows? (lambda (j) (or (>= j m) (char=? (string-ref t j) #\space)))))
    (cond ((== m 0) t)
          ((char=? (string-ref t 0) #\#)
           (let loop ((j 0))
             (cond ((and (< j m) (char=? (string-ref t j) #\#)) (loop (+ j 1)))
                   ((and (<= j 6) (follows? j)) (string-append "\\" t))
                   (else t))))
          ((char=? (string-ref t 0) #\>) (string-append "\\" t))
          ((and (in? (string-ref t 0) '(#\- #\+)) (follows? 1))
           (string-append "\\" t))
          ;; a line of = or - under a paragraph makes it a heading, and three
          ;; - are a rule
          ((and (in? (string-ref t 0) '(#\= #\-))
                (== (mdout-longest-run t (string-ref t 0)) m))
           (string-append "\\" t))
          ((char-numeric? (string-ref t 0))
           (let loop ((j 0))
             (cond ((and (< j m) (char-numeric? (string-ref t j))) (loop (+ j 1)))
                   ((and (< j m) (in? (string-ref t j) '(#\. #\))) (follows? (+ j 1)))
                    (string-append (substring t 0 j) "\\" (substring t j m)))
                   (else t))))
          (else t))))

(define (mdout-code s)
  ;; between more backquotes than it holds
  (let* ((n (+ (mdout-longest-run s #\`) 1))
         (q (make-string n #\`))
         (pad? (and (> (string-length s) 0)
                    (or (char=? (string-ref s 0) #\`)
                        (char=? (string-ref s (- (string-length s) 1)) #\`)))))
    (string-append q (if pad? " " "") s (if pad? " " "") q)))

(define (mdout-url s)
  ;; an address with spaces or parentheses is written between < and >
  (if (or (string-index s #\space) (string-index s #\() (string-index s #\)))
      (string-append "<" s ">")
      s))

(define (mdout-title s)
  (if (or (not s) (== s "")) ""
      (string-append " \"" (string-replace s "\"" "\\\"") "\"")))

(define (mdout-html-attr x name)
  ;; the attribute name of x as HTML writes it, if x has it
  (with v (mdout-attr x name)
    (if (or (not v) (and (== v "") (!= name 'alt))) ""
        (string-append
          " " (symbol->string name) "=\""
          (string-replace
            (string-replace (string-replace v "&" "&amp;") "\"" "&quot;")
            "<" "&lt;")
          "\""))))

(define (mdout-plain l)
  ;; the text of nodes, without their markup
  (apply string-append
         (map (lambda (x)
                (cond ((string? x) x)
                      ((func? x 'br) " ")
                      ((pair? x) (mdout-plain (mdout-children x)))
                      (else "")))
              l)))

(define (mdout-balanced s)
  ;; the description of an image ends at the first ] which closes no [:
  ;; a text whose brackets do not match gets all of them escaped
  (let loop ((l (string->list s)) (depth 0) (esc? #f))
    (cond ((null? l)
           (if (== depth 0) s
               (string-replace (string-replace s "[" "\\[") "]" "\\]")))
          (esc? (loop (cdr l) depth #f))
          ((char=? (car l) #\\) (loop (cdr l) depth #t))
          ((char=? (car l) #\[) (loop (cdr l) (+ depth 1) #f))
          ((char=? (car l) #\])
           (if (== depth 0)
               (string-replace (string-replace s "[" "\\[") "]" "\\]")
               (loop (cdr l) (- depth 1) #f)))
          (else (loop (cdr l) depth #f)))))

(define (mdout-spaced open close l cell?)
  ;; emphasis does not start or end with a space: the spaces go outside
  (let* ((s (mdout-inlines l cell?))
         (n (string-length s))
         (i (let loop ((i 0))
              (if (and (< i n) (char=? (string-ref s i) #\space)) (loop (+ i 1)) i)))
         (j (let loop ((j n))
              (if (and (> j i) (char=? (string-ref s (- j 1)) #\space))
                  (loop (- j 1)) j))))
    (if (== i j) s
        (string-append (substring s 0 i) open (substring s i j) close
                       (substring s j n)))))

(define (mdout-inline x cell?)
  (cond ((string? x) (mdout-escape x cell?))
        ((not (pair? x)) "")
        (else
          (let ((l (mdout-children x)))
            (case (car x)
              ((em) (mdout-spaced "*" "*" l cell?))
              ((strong) (mdout-spaced "**" "**" l cell?))
              ((del) (mdout-spaced "~~" "~~" l cell?))
              ((code)
               (with s (mdout-code (apply string-append (list-filter l string?)))
                 (if cell? (string-replace s "|" "\\|") s)))
              ((a)
               (let* ((url (or (mdout-attr x 'href) ""))
                      (title (mdout-attr x 'title)))
                 (if (and (list-1? l) (== (car l) url) (not title)
                          (string-search-forwards "://" 0 url)
                          (>= (string-search-forwards "://" 0 url) 0)
                          (not (string-index url #\space)))
                     (string-append "<" url ">")
                     (string-append "[" (mdout-inlines l cell?) "]("
                                    (mdout-url url) (mdout-title title) ")"))))
              ((img)
               (if (or (mdout-attr x 'width) (mdout-attr x 'height))
                   ;; Markdown has no sizes: the tag of HTML
                   (string-append
                     "<img"
                     (mdout-html-attr x 'src)
                     (mdout-html-attr
                       `(img (@ (alt ,(if (null? l) (or (mdout-attr x 'alt) "")
                                          (mdout-plain l)))))
                       'alt)
                     (mdout-html-attr x 'title) (mdout-html-attr x 'width)
                     (mdout-html-attr x 'height) ">")
                   ;; the description: the children, or else the text alt
                   (string-append
                     "![" (mdout-balanced
                            (if (null? l)
                                (mdout-escape (or (mdout-attr x 'alt) "") cell?)
                                (mdout-inlines l cell?)))
                     "](" (mdout-url (or (mdout-attr x 'src) ""))
                     (mdout-title (mdout-attr x 'title)) ")")))
              ((br) (if cell? "<br>" "\\\n"))
              ((math)
               (with s (mdout-join l "")
                 ;; a bar would end the cell of a table
                 (string-append
                   "$"
                   (if cell?
                       (string-replace (string-replace s "\\|" "\\Vert ")
                                       "|" "\\vert ")
                       s)
                   "$")))
              ((displaymath) (string-append "$$" (mdout-join l "") "$$"))
              ((html) (mdout-join l ""))
              ((footnote) (string-append "[^" (mdout-join l "") "]"))
              (else (mdout-inlines l cell?)))))))

(define (mdout-inlines l cell?)
  (apply string-append (map (lambda (x) (mdout-inline x cell?)) l)))

(define (mdout-text l)
  ;; the lines of a paragraph
  (map mdout-escape-start (mdout-split (mdout-inlines l #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdout-prefix lines first rest)
  ;; first before the first line, rest before the others
  (if (null? lines) (list (string-trim-spaces-right first))
      (cons (string-append first (car lines))
            (map (lambda (s) (if (== s "") s (string-append rest s)))
                 (cdr lines)))))

(define (mdout-yaml-value s)
  (if (or (== s "") (string-index s #\:) (string-index s #\#)
          (in? (string-ref s 0) '(#\" #\' #\[ #\{ #\- #\* #\& #\! #\| #\> #\% #\@)))
      (string-append "\"" (string-replace (string-replace s "\\" "\\\\")
                                          "\"" "\\\"") "\"")
      s))

(define (mdout-meta l)
  ;; the YAML header; a key with several values is a list
  (let loop ((l l) (acc '()) (seen '()))
    (cond ((null? l) `("---" ,@(reverse acc) "---"))
          ((in? (caar l) seen) (loop (cdr l) acc seen))
          (else
            (let* ((key (caar l))
                   (vals (map cadr (list-filter l (lambda (x) (== (car x) key))))))
              (loop (cdr l)
                    (if (list-1? vals)
                        (cons (string-append (symbol->string key) ": "
                                             (mdout-yaml-value (car vals)))
                              acc)
                        (append (reverse (map (lambda (v)
                                                (string-append "  - " (mdout-yaml-value v)))
                                              vals))
                                (cons (string-append (symbol->string key) ":") acc)))
                    (cons key seen)))))))

(define (mdout-pre x)
  (let* ((s (mdout-join (list-filter (mdout-children x) string?) ""))
         (n (max 3 (+ (mdout-longest-run s #\`) 1)))
         (fence (make-string n #\`)))
    `(,(string-append fence (or (mdout-attr x 'lang) ""))
      ,@(mdout-split s)
      ,fence)))

(define (mdout-item x marker loose?)
  (let* ((l (mdout-children x))
         (checked (mdout-attr x 'checked))
         (first (string-append marker
                               (cond ((not checked) "")
                                     ((== checked "true") "[x] ")
                                     (else "[ ] "))))
         (rest (make-string (string-length marker) #\space)))
    (mdout-prefix (mdout-blocks l loose?) first rest)))

(define (mdout-loose? x)
  ;; a list is written with blank lines between its items when it had some,
  ;; or when an item holds more than a paragraph and lists
  (or (== (mdout-attr x 'loose) "true")
      (list-or (map (lambda (li)
                      (with l (list-filter (mdout-children li)
                                           (lambda (b) (not (and (pair? b)
                                                                 (in? (car b) '(ul ol))))))
                        (> (length l) 1)))
                    (mdout-children x)))))

(define (mdout-list x)
  (let* ((ordered? (func? x 'ol))
         (loose? (mdout-loose? x))
         (start (or (and (mdout-attr x 'start)
                         (string->number (mdout-attr x 'start)))
                    1)))
    (let loop ((l (mdout-children x)) (i start) (acc '()))
      (if (null? l) (reverse acc)
          (let* ((marker (if ordered?
                             (string-append (number->string i) ". ")
                             "- "))
                 (lines (mdout-item (car l) marker loose?)))
            (loop (cdr l) (+ i 1)
                  (append (reverse lines)
                          (if (and loose? (nnull? acc)) (cons "" acc) acc))))))))

(define (mdout-pad s w align)
  (let* ((n (max 0 (- w (mdout-width s)))))
    (cond ((== align "right") (string-append (make-string n #\space) s))
          ((== align "center")
           (string-append (make-string (quotient n 2) #\space) s
                          (make-string (- n (quotient n 2)) #\space)))
          (else (string-append s (make-string n #\space))))))

(define (mdout-table x)
  (let* ((rows (list-filter (mdout-children x) (lambda (r) (func? r 'tr))))
         (cells (map (lambda (r)
                       (map (lambda (c)
                              (string-replace
                                (mdout-inlines (mdout-children c) #t) "\n" " "))
                            (mdout-children r)))
                     rows))
         (ncols (apply max (cons 1 (map length cells))))
         (cells (map (lambda (r)
                       (append r (make-list (- ncols (length r)) "")))
                     cells))
         (aligns (let ((first (if (null? rows) '() (mdout-children (car rows)))))
                   (map (lambda (i)
                          (or (and (< i (length first))
                                   (mdout-attr (list-ref first i) 'align))
                              ""))
                        (iota ncols))))
         (widths (map (lambda (i)
                        (apply max (cons 3 (map (lambda (r)
                                                  (mdout-width (list-ref r i)))
                                                cells))))
                      (iota ncols)))
         (line (lambda (r)
                 (string-append
                   "| "
                   (mdout-join (map mdout-pad r widths aligns) " | ")
                   " |")))
         (rule (lambda (w a)
                 (cond ((== a "center")
                        (string-append ":" (make-string (- w 2) #\-) ":"))
                       ((== a "right")
                        (string-append (make-string (- w 1) #\-) ":"))
                       ((== a "left")
                        (string-append ":" (make-string (- w 1) #\-)))
                       (else (make-string w #\-))))))
    (if (null? cells) '()
        `(,(line (car cells))
          ,(string-append "| " (mdout-join (map rule widths aligns) " | ") " |")
          ,@(map line (cdr cells))))))

(define (mdout-footnote-def x)
  (let* ((l (cdr x))
         (label (if (and (nnull? l) (string? (car l))) (car l) ""))
         (blocks (if (and (nnull? l) (string? (car l))) (cdr l) l)))
    (mdout-prefix (mdout-blocks blocks #t)
                  (string-append "[^" label "]: ") "    ")))

(define (mdout-block x)
  ;; the lines of a block
  (cond ((string? x) (mdout-text (list x)))
        ((not (pair? x)) '())
        (else
          (let ((l (mdout-children x)))
            (case (car x)
              ((meta) (mdout-meta (cdr x)))
              ((h1 h2 h3 h4 h5 h6)
               (let* ((n (- (char->integer (string-ref (symbol->string (car x)) 1))
                            (char->integer #\0)))
                      (s (string-replace (mdout-inlines l #f) "\n" " ")))
                 (list (string-append (make-string n #\#) " " s))))
              ((p) (mdout-text l))
              ((blockquote)
               (map (lambda (s) (if (== s "") ">" (string-append "> " s)))
                    (mdout-blocks l #t)))
              ((ul ol) (mdout-list x))
              ((pre) (mdout-pre x))
              ((hr) (list "***"))
              ((table) (mdout-table x))
              ((displaymath)
               `("$$" ,@(mdout-split (mdout-join l "")) "$$"))
              ((html) (mdout-split (mdout-join l "")))
              ((footnote-def) (mdout-footnote-def x))
              ((markdown) (mdout-blocks l #t))
              ;; inline markup where a block is expected: a paragraph
              (else (mdout-text (list x))))))))

(define (mdout-blocks l loose?)
  ;; the lines of blocks; between them a blank line, except in an item of a
  ;; list without blank lines, before a list inside it
  (let loop ((l l) (acc '()) (first? #t))
    (cond ((null? l) (reverse acc))
          (else
            (let* ((x (car l))
                   (lines (mdout-block x))
                   (sep? (and (not first?)
                              (or loose?
                                  (not (and (pair? x) (in? (car x) '(ul ol))))))))
              (if (null? lines)
                  (loop (cdr l) acc first?)
                  (loop (cdr l)
                        (append (reverse lines) (if sep? (cons "" acc) acc))
                        #f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdout-block? x)
  (and (pair? x)
       (in? (car x) '(meta h1 h2 h3 h4 h5 h6 p blockquote ul ol pre hr table
                      footnote-def markdown))))

(tm-define (serialize-markdown x)
  (:type (-> stree string))
  (:synopsis "Write the Markdown tree @x as Markdown")
  (let* ((x (if (func? x '!file 1) (cadr x) x))
         (l (if (func? x 'markdown) (cdr x) (list x))))
    (if (list-or (map mdout-block? l))
        (string-append (mdout-join (mdout-blocks l #t) "\n") "\n")
        ;; text only: a snippet
        (mdout-join (mdout-text l) "\n"))))
