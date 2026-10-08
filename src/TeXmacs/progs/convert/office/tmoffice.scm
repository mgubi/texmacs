
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmoffice.scm
;; DESCRIPTION : conversion of TeXmacs trees into office trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The office trees are described in office-tools.scm. A document which is
;; exported is first expanded by the editor (tmoffice-expand.scm), so that
;; the numbers of its sections, its references and its citations are text.
;;
;; The conversion is the one to Markdown (convert/markdown/tmmarkdown.scm),
;; from which this file comes: each handler takes the arguments of a tag
;; and returns a list of nodes, in a tree which is like the one of Markdown
;; (paragraphs p, lists ul and ol of li, code pre, headings (!h level ...),
;; formulas on their own (!display ...), paragraphs with a role (!role
;; "caption" ...)). tmof-final then makes the office tree of it. What the
;; office formats have and Markdown has not is added: formulas as MathML,
;; images inside the document, the borders and the backgrounds of the
;; tables, colors, notes in their place.

(texmacs-module (convert office tmoffice)
  (:use (convert office office-tools)
        (convert mathml tmmath)
        (convert mathml mathml-drd)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmof-html? #f)          ; (no HTML in an office document)
(define tmof-document? #f)      ; a whole document, and not a piece of one
(define (tmof-initialize opts)
  (set! tmof-flat? #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes: text and blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-keep s l)
  ;; the text in UTF-8, but for the sequences of the list, which the
  ;; conversion would change: ... into an ellipsis, ` into a quote
  (if (null? l) (cork->utf8 s)
      (let* ((what (car l))
             (i (string-search-forwards what 0 s)))
        (if (< i 0) (tmof-keep s (cdr l))
            (string-append
              (tmof-keep (substring s 0 i) (cdr l)) what
              (tmof-keep (substring s (+ i (string-length what))
                                    (string-length s)) l))))))

(define (tmof-text s) (tmof-keep s '("...")))
(define (tmof-code-text s) (tmof-keep s '("..." "`")))

(define (tmof-block? x)
  (and (pair? x)
       (in? (car x) '(meta h1 h2 h3 h4 h5 h6 !h !role p blockquote ul ol pre hr
                      table footnote-def !display !pagebreak))))

(define (tmof-blank? x)
  (and (string? x) (== (string-trim-spaces x) "")))

(define (tmof-merge l)
  ;; with the strings which follow each other as one, and so for the
  ;; texts with the same markup
  (cond ((null? l) l)
        ((== (car l) "") (tmof-merge (cdr l)))
        ((and (pair? (car l))
              (in? (caar l) '(em strong strike underline mark sub sup smallcaps))
              (nnull? (cdr l)) (func? (cadr l) (caar l)))
         (tmof-merge (cons `(,(caar l) ,@(tmof-merge (append (cdar l) (cdadr l))))
                           (cddr l))))
        ((and (string? (car l)) (nnull? (cdr l)) (string? (cadr l)))
         (tmof-merge (cons (string-append (car l) (cadr l)) (cddr l))))
        (else (cons (car l) (tmof-merge (cdr l))))))

(define (tmof-squeeze s)
  ;; with one space for several
  (let loop ((l (string->list s)) (acc '()) (sp? #f))
    (cond ((null? l) (list->string (reverse acc)))
          ((char=? (car l) #\space)
           (loop (cdr l) (if sp? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

(define (tmof-trim l)
  ;; text without spaces and line breaks at its ends
  (let* ((l (map (lambda (x) (if (string? x) (tmof-squeeze x) x)) (tmof-merge l)))
         (drop-start
           (lambda (l)
             (let loop ((l l))
               (cond ((null? l) l)
                     ((or (tmof-blank? (car l)) (func? (car l) 'br)) (loop (cdr l)))
                     ((string? (car l))
                      (cons (string-trim-spaces-left (car l)) (cdr l)))
                     (else l)))))
         (l (drop-start l))
         (r (reverse l))
         (r (let loop ((r r))
              (cond ((null? r) r)
                    ((or (tmof-blank? (car r)) (func? (car r) 'br)) (loop (cdr r)))
                    ((string? (car r))
                     (cons (string-trim-spaces-right (car r)) (cdr r)))
                    (else r)))))
    (reverse r)))

(define (tmof-blocks l)
  ;; the nodes as blocks: the text between two blocks is a paragraph
  (let loop ((l l) (run '()) (acc '()))
    (define (flush)
      (with p (tmof-trim (reverse run))
        (if (null? p) acc (cons `(p ,@p) acc))))
    (cond ((null? l) (reverse (flush)))
          ((tmof-block? (car l)) (loop (cdr l) '() (cons (car l) (flush))))
          (else (loop (cdr l) (cons (car l) run) acc)))))

(define (tmof-inline l)
  ;; the nodes as text: the paragraphs of blocks follow each other
  (append-map
    (lambda (x)
      (cond ((func? x 'p) (append (cdr x) (list " ")))
            ((func? x '!display)
             (list `(math (@ (display "true") (form "sxml")) ,(cadr x))))
            ((tmof-block? x) '())
            (else (list x))))
    l))

(define (tmof-has-block? l)
  (list-or (map tmof-block? l)))

(define (tmof-wrap tag l)
  ;; the markup tag around text; the paragraphs of blocks get it each
  (cond ((tmof-has-block? l)
         (map (lambda (b)
                (if (func? b 'p)
                    `(p (,tag ,@(cdr b)))
                    b))
              (tmof-blocks l)))
        ((null? (tmof-trim l)) l)
        (else `((,tag ,@(tmof-merge l))))))

(define (tmof-html-wrap name l)
  ;; HTML around text, for what Markdown has no markup
  (if (or (not tmof-html?) (tmof-has-block? l) (null? (tmof-trim l))) l
      `((html ,(string-append "<" name ">")) ,@l
        (html ,(string-append "</" name ">")))))

(define (tmof-attach before l after)
  ;; text before and after nodes: in their first and last paragraphs
  (if (not (tmof-has-block? l))
      (append before l after)
      (let* ((b (tmof-blocks l))
             (b (if (null? (tmof-trim before)) b
                    (if (func? (car b) 'p)
                        (cons `(p ,@before ,@(cdar b)) (cdr b))
                        (cons `(p ,@before) b))))
             (r (reverse b))
             (r (if (null? (tmof-trim after)) r
                    (if (func? (car r) 'p)
                        (cons `(p ,@(cdar r) ,@after) (cdr r))
                        (cons `(p ,@after) r)))))
        (reverse r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain text and mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-plain x)
  ;; the text of a tree, as for code: the paragraphs are lines
  (cond ((string? x) (tmof-code-text x))
        ((not (pair? x)) "")
        ((func? x 'document)
         (let loop ((l (cdr x)) (acc '()))
           (cond ((null? l) (apply string-append (reverse acc)))
                 ((null? (cdr l)) (loop (cdr l) (cons (tmof-plain (car l)) acc)))
                 (else (loop (cdr l) (cons "\n" (cons (tmof-plain (car l)) acc)))))))
        ((in? (car x) '(next-line new-line)) "\n")
        ((in? (car x) '(label assign hidden-binding set-binding write)) "")
        ((func? x 'with) (tmof-plain (cAr x)))
        ((func? x 'hlink 2) (tmof-plain (cadr x)))
        (else (apply string-append (map tmof-plain (cdr x))))))

(define (tmof-math-clean x)
  ;; a formula without what LaTeX would write and Markdown does not need
  (cond ((not (pair? x)) x)
        ((in? (car x) '(label assign hidden-binding set-binding write)) "")
        (else (cons (car x) (map tmof-math-clean (cdr x))))))

(define (tmof-strip s before after)
  ;; s without before at its start and after at its end, or #f
  (let ((n (string-length s)))
    (and (string-starts? s before) (string-ends? s after)
         (>= n (+ (string-length before) (string-length after)))
         (substring s (string-length before) (- n (string-length after))))))

(define (tmof-trim-lines s)
  ;; without spaces and empty lines at its ends
  (let* ((l (string->list s))
         (ws? (lambda (c) (or (char=? c #\space) (char=? c #\newline))))
         (l (let loop ((l l)) (if (and (nnull? l) (ws? (car l))) (loop (cdr l)) l)))
         (r (let loop ((r (reverse l)))
              (if (and (nnull? r) (ws? (car r))) (loop (cdr r)) r))))
    (list->string (reverse r))))

;; A formula is converted to MathML, whose named characters (&alpha;) are
;; written as the characters themselves: an office file knows no names.


;; the names which the converter of MathML writes for the big operators,
;; the accents and what is not seen, with the codes of their characters
(define tmof-entities
  '(("&Sum;" . #x2211) ("&Product;" . #x220f) ("&Integral;" . #x222b)
    ("&ContourIntegral;" . #x222e) ("&Coproduct;" . #x2210)
    ("&Intersection;" . #x22c2) ("&Union;" . #x22c3) ("&Wedge;" . #x22c0)
    ("&Vee;" . #x22c1) ("&CircleDot;" . #x2a00) ("&CirclePlus;" . #x2a01)
    ("&CircleTimes;" . #x2a02) ("&SquareIntersection;" . #x2a05)
    ("&SquareUnion;" . #x2a06) ("&UnionPlus;" . #x2a04)
    ("&Hat;" . #x5e) ("&Tilde;" . #x7e) ("&OverBar;" . #xaf)
    ("&UnderBar;" . #x5f) ("&RightVector;" . #x2192) ("&Hacek;" . #x2c7)
    ("&Breve;" . #x2d8) ("&DiacriticalAcute;" . #xb4)
    ("&DiacriticalGrave;" . #x60) ("&DiacriticalDot;" . #x2d9)
    ("&DoubleDot;" . #xa8) ("&RightArrow;" . #x2192) ("&LeftArrow;" . #x2190)
    ("&OverBrace;" . #x23de) ("&UnderBrace;" . #x23df)
    ("&ApplyFunction;" . #x2061) ("&af;" . #x2061)
    ("&InvisibleTimes;" . #x2062) ("&it;" . #x2062)
    ("&InvisibleComma;" . #x2063) ("&ic;" . #x2063)
    ("&amp;" . #x26) ("&lt;" . #x3c) ("&gt;" . #x3e) ("&nbsp;" . #xa0)))

(define (tmof-entity name)
  ;; the character of a name of MathML (&alpha;, &#x3b1;), in UTF-8
  (let* ((n (string-length name))
         (code (cond ((assoc-ref tmof-entities name) => identity)
                     ((and (> n 4) (string-starts? name "&#x"))
                      (string->number (substring name 3 (- n 1)) 16))
                     ((and (> n 3) (string-starts? name "&#"))
                      (string->number (substring name 2 (- n 1))))
                     (else #f)))
         (t (and (not code)
                 (catch #t (lambda () (logic-ref mathml-symbol->tm% name))
                        (lambda args #f)))))
    (cond (code (office-utf8 code))
          ((string? t) (cork->utf8 t))
          (else name))))

(define (tmof-plain-utf8 s)
  ;; a piece of text of MathML in UTF-8: the converter writes some
  ;; characters as such already, and others in the encoding of TeXmacs
  (if (list-or (map (lambda (c) (>= (char->integer c) 128)) (string->list s)))
      s
      (cork->utf8 s)))

(define (tmof-mathml-text s)
  ;; the text of a token of MathML, with characters for its names
  (let loop ((i 0) (acc '()))
    (let* ((a (string-search-forwards "&" i s))
           (b (if (>= a 0) (string-search-forwards ";" a s) -1)))
      (if (or (< a 0) (< b 0))
          (apply string-append
                 (reverse (cons (tmof-plain-utf8 (substring s i (string-length s)))
                                acc)))
          (loop (+ b 1)
                (cons* (tmof-entity (substring s a (+ b 1)))
                       (tmof-plain-utf8 (substring s i a))
                       acc))))))

(define (tmof-mathml-clean x)
  (cond ((string? x) (tmof-mathml-text x))
        ((func? x '@) x)
        ((pair? x) (cons (car x) (map tmof-mathml-clean (cdr x))))
        (else x)))

(define (tmof-mathml x)
  ;; the MathML of the formula x, a tree (m:math ...), or #f
  (with r (catch #t
            (lambda () (texmacs->mathml (tmof-math-clean x)))
            (lambda args #f))
    (and r (!= r "") (!= r '())
         `(m:math ,(tmof-mathml-clean r)))))

(define (tmof-symbol x)
  ;; the character of a formula which is one symbol, as an arrow, or #f
  (and (string? x) (> (string-length x) 2)
       (char=? (string-ref x 0) #\<)
       (== (string-search-forwards ">" 0 x) (- (string-length x) 1))
       (with s (tmof-text x)
         (and (not (string-starts? s "<")) s))))

(define (tmof-math l)
  (let ((x (if (list-1? l) (car l) `(concat ,@l))))
    (if (tmof-symbol x)
        (list (tmof-symbol x))
        (with m (tmof-mathml x)
          (if m `((math (@ (form "sxml")) ,m)) '())))))

(define tmof-limit-operators
  ;; the big operators whose limits are under and over them in a formula on
  ;; its own lines: all but the integrals
  (map office-utf8 '(#x2211 #x220f #x2210 #x22c0 #x22c1 #x22c2 #x22c3 #x2a00
                     #x2a01 #x2a02 #x2a04 #x2a05 #x2a06)))

(define (tmof-display-limits x)
  ;; the MathML x with the limits of its big operators under and over them
  (cond ((not (pair? x)) x)
        ((func? x '@) x)
        ((and (in? (car x) '(m:msubsup m:msub m:msup)) (pair? (cdr x))
              (func? (cadr x) 'm:mo) (pair? (cdadr x))
              (in? (cAr (cadr x)) tmof-limit-operators))
         (cons (cond ((func? x 'm:msubsup) 'm:munderover)
                     ((func? x 'm:msub) 'm:munder)
                     (else 'm:mover))
               (map tmof-display-limits (cdr x))))
        (else (cons (car x) (map tmof-display-limits (cdr x))))))

(define (tmof-display x . tag)
  ;; a formula on its own lines, with the number which follows it if any
  (with m (with r (tmof-mathml x) (and r (tmof-display-limits r)))
    (if m `((!display ,m ,(if (nnull? tag) (car tag) ""))) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-noop l) '())
(define (tmof-space l) '(" "))
(define (tmof-first l) (if (null? l) '() (tmof (car l))))
(define (tmof-last l) (if (null? l) '() (tmof (cAr l))))
(define (tmof-all l) (append-map tmof l))

(define (tmof-em l) (tmof-wrap 'em (tmof-all l)))
(define (tmof-strong l) (tmof-wrap 'strong (tmof-all l)))
(define (tmof-del l) (tmof-wrap 'strike (tmof-all l)))

(define (tmof-underline l) (tmof-wrap 'underline (tmof-all l)))

(define (tmof-marked l) (tmof-wrap 'mark (tmof-all l)))

(define (tmof-sub l) (tmof-wrap 'sub (tmof-all l)))

(define (tmof-sup l) (tmof-wrap 'sup (tmof-all l)))

(define (tmof-code l)
  ;; code in the text, or lines of code
  (if (and (list-1? l) (func? (car l) 'document) (> (length (car l)) 2))
      `((pre ,(tmof-plain (car l))))
      (with s (apply string-append (map tmof-plain l))
        (if (== s "") '() `((code ,s))))))

(define (tmof-key l)
  ;; a key of the keyboard, as code
  (with s (apply string-append (map tmof-plain l))
    (if (== s "") '() `((code ,s)))))

(define (tmof-next-line l) '((br)))

(define (tmof-hrule l) '((hr)))

(define (tmof-name s) (lambda (l) (list s)))
(define (tmof-nbsp l) (list (tmof-text "<varspace>")))
(define tmof-TeXmacs (tmof-name "TeXmacs"))
(define tmof-TeX (tmof-name "TeX"))
(define tmof-LaTeX (tmof-name "LaTeX"))

(define (tmof-hlink l)
  ;; a link; a link inside the document is its text
  (if (< (length l) 2) (tmof-all l)
      (let ((body (tmof (car l)))
            (url (tmof-plain (cadr l))))
        (if (or (== url "") (string-starts? url "#") (tmof-has-block? body))
            body
            `((a (@ (href ,url)) ,@(tmof-merge body)))))))

(define (tmof-hlink* l)
  ;; a link with a title
  (let ((r (tmof-hlink l))
        (title (if (< (length l) 3) "" (tmof-plain (caddr l)))))
    (if (and (list-1? r) (func? (car r) 'a) (!= title ""))
        `((a (@ ,@(cdadar r) (title ,title)) ,@(cddar r)))
        r)))

(define (tmof-href l)
  (with url (tmof-plain (if (null? l) "" (car l)))
    (if (== url "") '() `((a (@ (href ,url)) ,url)))))

;; An image goes into the archive with its size, which the office formats
;; want: the one which is given, or the one of the pixels of the file.

(define (tmof-byte s i) (char->integer (string-ref s i)))

(define (tmof-pixels data)
  ;; (width height) in pixels of an image PNG, GIF or JPEG, or #f
  (let ((n (string-length data)))
    (cond ((and (> n 24) (== (tmof-byte data 0) 137) (== (tmof-byte data 1) 80))
           (list (+ (* 16777216 (tmof-byte data 16)) (* 65536 (tmof-byte data 17))
                    (* 256 (tmof-byte data 18)) (tmof-byte data 19))
                 (+ (* 16777216 (tmof-byte data 20)) (* 65536 (tmof-byte data 21))
                    (* 256 (tmof-byte data 22)) (tmof-byte data 23))))
          ((and (> n 10) (== (tmof-byte data 0) 71) (== (tmof-byte data 1) 73))
           (list (+ (tmof-byte data 6) (* 256 (tmof-byte data 7)))
                 (+ (tmof-byte data 8) (* 256 (tmof-byte data 9)))))
          ((and (> n 4) (== (tmof-byte data 0) 255) (== (tmof-byte data 1) 216))
           ;; the segments of a JPEG, up to the one with the size
           (let loop ((i 2))
             (cond ((> (+ i 9) n) #f)
                   ((!= (tmof-byte data i) 255) #f)
                   ((and (>= (tmof-byte data (+ i 1)) 192)
                         (<= (tmof-byte data (+ i 1)) 207)
                         (not (in? (tmof-byte data (+ i 1)) '(196 200 204))))
                    (list (+ (* 256 (tmof-byte data (+ i 7))) (tmof-byte data (+ i 8)))
                          (+ (* 256 (tmof-byte data (+ i 5))) (tmof-byte data (+ i 6)))))
                   (else (loop (+ i 2 (* 256 (tmof-byte data (+ i 2)))
                                  (tmof-byte data (+ i 3))))))))
          (else #f))))

(define tmof-text-width 16.0) ; the width of the text, in centimeters

(define (tmof-cm x)
  ;; the length x of TeXmacs in centimeters, or #f when it is not absolute
  (and (string? x) (!= x "")
       (let* ((n (string-length x))
              (i (let loop ((i 0))
                   (if (and (< i n) (or (char-numeric? (string-ref x i))
                                        (in? (string-ref x i) '(#\. #\-))))
                       (loop (+ i 1)) i)))
              (v (string->number (substring x 0 i)))
              (scale (assoc-ref `(("cm" . 1.0) ("mm" . 0.1) ("in" . 2.54)
                                  ("pt" . 0.03528) ("px" . 0.02646)
                                  ("par" . ,tmof-text-width) ("pag" . 24.0)
                                  ("em" . 0.37) ("fn" . 0.37) ("spc" . 0.12))
                                (substring x i n))))
         (and v scale (> v 0) (* v scale)))))

(define (tmof-cm-string x)
  (string-append (office-decimal x) "cm"))

(define (tmof-image-sizes l data)
  ;; (width height) in centimeters, for the arguments l of the tag image
  (let* ((w (and (>= (length l) 2) (tmof-cm (cadr l))))
         (h (and (>= (length l) 3) (tmof-cm (caddr l))))
         (px (or (tmof-pixels data) '(300 200)))
         (ratio (/ (* 1.0 (cadr px)) (max 1 (car px)))))
    (cond ((and w h) (list w h))
          (w (list w (* w ratio)))
          (h (list (/ h ratio) h))
          (else
            ;; the size of the pixels, at 96 for an inch; not wider than
            ;; the text
            (with nw (min tmof-text-width (* (car px) 0.02646))
              (list nw (* nw ratio)))))))

(define (tmof-image-data name)
  ;; the bytes of the file of an image, which is looked for from the
  ;; document, or #f
  (catch #t
    (lambda ()
      (let* ((u (unix->url name))
             (base (if (url-none? (current-buffer-url)) (unix->url ".")
                       (current-buffer-url)))
             (v (if (url-rooted? u) u (url-relative base u))))
        (and (url-exists? v) (string-load v))))
    (lambda args #f)))

(define tmof-picture-nr 0)

(define (tmof-picture x vector)
  ;; The tree x as a picture: a drawing, or an image in a format which the
  ;; office programs do not read. The editor typesets it and makes an
  ;; image of it: a PNG, which all programs read, and an SVG beside it for
  ;; those which show it. The SVG is made from the PDF of the picture when
  ;; vector is #t (a drawing: what TeXmacs draws is drawn the same), is
  ;; the text vector when it is one (an image which is an SVG), and is
  ;; left out when vector is #f. Nothing when there is no editor, or no
  ;; picture.
  (catch #t
    (lambda ()
      (let* ((base (url-temp))
             (png (url-glue base ".png"))
             (pdf (url-glue base ".pdf"))
             (svg (url-glue base ".svg"))
             (ext (print-snippet png x #t)))
        (if (not (and (url-exists? png) (list? ext) (>= (length ext) 10)))
            '()
            (let* ((data (string-load png))
                   (dpi (max 1 (list-ref ext 9)))
                   ;; the box of the ink, in 256th of a dot
                   (cm (lambda (a b) (max 0.1 (* 2.54 (/ (- b a) (* 256.0 dpi))))))
                   (w (cm (list-ref ext 0) (list-ref ext 2)))
                   (h (cm (list-ref ext 1) (list-ref ext 3)))
                   (vector (cond ((string? vector) vector)
                                 ((not vector) #f)
                                 (else
                                   (and (begin (print-snippet pdf x #t)
                                               (url-exists? pdf))
                                        (pdf->svg-native pdf svg)
                                        (url-exists? svg)
                                        (string-load svg))))))
              (for (u (list png pdf svg))
                (when (url-exists? u) (system-remove u)))
              (set! tmof-picture-nr (+ tmof-picture-nr 1))
              (list (apply office-node
                           (list 'image
                                 `((name ,(string-append
                                            "drawing" (number->string tmof-picture-nr)
                                            ".png"))
                                   (data ,data)
                                   (width ,(tmof-cm-string w))
                                   (height ,(tmof-cm-string h))
                                   (svg ,vector)))))))))
    (lambda args '())))

(define (tmof-graphics l)
  (tmof-picture (cons 'graphics l) #t))

(define (tmof-drawing? x)
  ;; a tree which is drawn: graphics, alone or over a text, with the
  ;; variables around them
  (and (pair? x)
       (or (in? (car x) '(graphics draw-over draw-under))
           (and (func? x 'with) (pair? (cdr x)) (tmof-drawing? (cAr x))))))

(define (tmof-image l)
  ;; an image which is in the document, or a file which is read
  (let* ((inside? (and (nnull? l) (func? (car l) 'tuple 2)
                       (func? (cadar l) 'raw-data 1)
                       (string? (cadr (cadar l))) (string? (caddar l))))
         (name (cond (inside? (cork->utf8 (caddar l)))
                     ((and (nnull? l) (string? (car l))) (cork->utf8 (car l)))
                     (else "")))
         (data (cond (inside? (cadr (cadar l)))
                     ((!= name "") (tmof-image-data name))
                     (else #f)))
         (suffix (locase-all (url-suffix name))))
    (cond ((and data (in? suffix '("png" "jpg" "jpeg" "gif")) (tmof-pixels data))
           (with size (tmof-image-sizes l data)
             `((image (@ (name ,(url->string (url-tail (unix->url name))))
                         (data ,data)
                         (width ,(tmof-cm-string (car size)))
                         (height ,(tmof-cm-string (cadr size))))))))
          ;; another format (PDF, Postscript, SVG): the picture which the
          ;; editor makes of it, or else its name
          ((!= name "")
           ;; (an SVG which is made of the picture of an image is not
           ;; always shown well: only an image which is one has one)
           (with r (tmof-picture (cons 'image l)
                                 (and (== suffix "svg") (string? data)
                                      (>= (string-search-forwards "<svg" 0 data) 0)
                                      data))
             (if (null? r) (list (string-append "[" name "]")) r)))
          (else '()))))

(define (tmof-specific l)
  ;; what is for another medium only is left out
  (cond ((< (length l) 2) '())
        ((in? (car l) '("texmacs" "screen" "printer" "image")) (tmof (cadr l)))
        (else '())))

(define (tmof-reference l)
  ;; a reference which was not expanded: its label
  (list (tmof-plain (if (null? l) "" (car l)))))

(define (tmof-eqref l)
  (list (string-append "(" (tmof-plain (if (null? l) "" (car l))) ")")))

(define (tmof-cite l)
  (list (string-append
          "["
          (apply string-append
                 (list-intersperse (map tmof-plain l) ", "))
          "]")))

(define (tmof-footnote-add body)
  (with b (tmof-blocks (tmof body))
    (if (null? b) '() `((note ,@b)))))

(define (tmof-footnote l)
  (if (null? l) '() (tmof-footnote-add (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables of the environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some colors of TeXmacs by their names
(define tmof-colors
  '(("red" . "#ff0000") ("green" . "#00ff00") ("blue" . "#0000ff")
    ("yellow" . "#ffff00") ("magenta" . "#ff00ff") ("cyan" . "#00ffff")
    ("orange" . "#ff8000") ("brown" . "#804000") ("pink" . "#ffc0c0")
    ("grey" . "#808080") ("gray" . "#808080") ("white" . "#ffffff")
    ("dark red" . "#800000") ("dark green" . "#008000")
    ("dark blue" . "#000080") ("dark yellow" . "#808000")
    ("dark magenta" . "#800080") ("dark cyan" . "#008080")
    ("dark orange" . "#804000") ("dark brown" . "#402000")
    ("dark grey" . "#404040") ("light grey" . "#c0c0c0")
    ("broken white" . "#ffffdf") ("pastel yellow" . "#ffffdf")
    ("pastel red" . "#ffdfdf") ("pastel green" . "#dfffdf")
    ("pastel blue" . "#dfdfff") ("pastel grey" . "#dfdfdf")
    ("pastel orange" . "#ffdfbf") ("pastel cyan" . "#dfffff")
    ("pastel magenta" . "#ffdfff") ("pastel brown" . "#dfbfbf")))

(define (tmof-color s)
  ;; the color s of TeXmacs as #rrggbb, or #f for black and what is unknown
  (and (string? s)
       (or (office-color s)
           (with c (assoc-ref tmof-colors s)
             (and c (office-color c)))
           ;; #rgb
           (and (== (string-length s) 4) (string-starts? s "#")
                (office-color
                  (list->string
                    (append-map (lambda (c) (list c c))
                                (cdr (string->list s)))))))))

(define (tmof-colored body color)
  ;; the text of the nodes in this color
  (cond ((not color) body)
        ((tmof-has-block? body)
         (map (lambda (b)
                (if (func? b 'p) `(p (color (@ (value ,color)) ,@(cdr b))) b))
              (tmof-blocks body)))
        ((null? (tmof-trim body)) body)
        (else `((color (@ (value ,color)) ,@(tmof-merge body))))))

(define (tmof-with-one var val body)
  ;; the body, already converted, in the environment where var is val
  (cond ((and (== var "font-series") (== val "bold")) (tmof-wrap 'strong body))
        ((and (== var "font-shape") (== val "italic")) (tmof-wrap 'em body))
        ((and (== var "font-shape") (== val "slanted")) (tmof-wrap 'em body))
        ((and (== var "font-shape") (== val "small-caps"))
         (tmof-wrap 'smallcaps body))
        ((== var "color") (tmof-colored body (tmof-color val)))
        (else body)))

(define (tmof-with l)
  (cond ((null? l) '())
        ((null? (cdr l)) (tmof (car l)))
        ;; mathematics: the body as it is, with the other variables
        ((let loop ((l l))
           (cond ((or (null? l) (null? (cdr l))) #f)
                 ((and (== (car l) "mode") (== (cadr l) "math")) #t)
                 (else (loop (cddr l)))))
         (tmof-math (list (cAr l))))
        ((let loop ((l l))
           (cond ((or (null? l) (null? (cdr l))) #f)
                 ((and (== (car l) "font-family") (== (cadr l) "tt")) #t)
                 (else (loop (cddr l)))))
         (tmof-code (list (cAr l))))
        (else
          (let loop ((l l) (body (tmof (cAr l))))
            (if (or (null? l) (null? (cdr l))) body
                (loop (cddr l)
                      (if (and (string? (car l)) (string? (cadr l)))
                          (tmof-with-one (car l) (cadr l) body)
                          body)))))))

(define (tmof-surround l)
  (if (!= (length l) 3) (tmof-all l)
      (tmof-attach (tmof-inline (tmof (car l)))
                   (tmof (caddr l))
                   (tmof-inline (tmof (cadr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraphs, sections and the title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-document l)
  ;; each paragraph by itself
  (append-map (lambda (x) (tmof-blocks (tmof x))) l))

(define (tmof-concat l)
  (tmof-all l))

;; A heading is (!h level text...) until the levels which the document uses
;; are known: 0 for a part, 1 for a chapter, 2 for a section... A formula on
;; its own lines is (!display "latex") until then. The title of a section
;; may be laid out as a table, its number in a cell and its text in another:
;; inside a heading the cells of a table are text.

(define tmof-flat? #f)

(define (tmof-heading level l)
  (with old tmof-flat?
    (set! tmof-flat? #t)
    (with t (tmof-trim (tmof-inline (tmof-all l)))
      (set! tmof-flat? old)
      (if (null? t) '() `((!h ,level ,@t))))))

(define (tmof-part l) (tmof-heading 0 l))
(define (tmof-chapter l) (tmof-heading 1 l))
(define (tmof-section l) (tmof-heading 2 l))
(define (tmof-subsection l) (tmof-heading 3 l))
(define (tmof-subsubsection l) (tmof-heading 4 l))
(define (tmof-paragraph l) (tmof-heading 5 l))
(define (tmof-subparagraph l) (tmof-heading 6 l))

(define (tmof-doc-field l tag)
  ;; the values of the fields tag of the title
  (append-map
    (lambda (x)
      (cond ((func? x tag) (list (string-trim-spaces (tmof-plain `(concat ,@(cdr x))))))
            ((and (pair? x) (in? (car x) '(doc-author author-data)))
             (tmof-doc-field (cdr x) tag))
            (else '())))
    l))

(define (tmof-doc-data l)
  ;; the title, the authors and the date: paragraphs with these roles
  (let* ((some (lambda (l) (list-filter l (lambda (s) (!= s "")))))
         (role (lambda (name) (lambda (s) `(!role ,name ,s)))))
    (append
      (map (role "title") (some (tmof-doc-field l 'doc-title)))
      (map (role "subtitle") (some (tmof-doc-field l 'doc-subtitle)))
      (map (role "author") (some (tmof-doc-field l 'author-name)))
      (map (role "date") (some (tmof-doc-field l 'doc-date))))))

(define (tmof-with-role role l)
  ;; the paragraphs of the blocks l with a role
  (map (lambda (b) (if (func? b 'p) `(!role ,role ,@(cdr b)) b))
       l))

(define (tmof-abstract l)
  (tmof-with-role "abstract" (tmof-blocks (tmof-all l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists and quotations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-item-start x)
  ;; (item . rest) when the paragraph x starts with an item, else #f
  (cond ((or (func? x 'item) (func? x 'item*)) (cons x '()))
        ((and (func? x 'concat) (pair? (cdr x))
              (or (func? (cadr x) 'item) (func? (cadr x) 'item*)))
         (cons (cadr x) (cddr x)))
        (else #f)))

(define (tmof-description l)
  ;; a description: its terms and their definitions, as paragraphs with
  ;; these roles
  (let* ((body (if (null? l) "" (cAr l)))
         (pars (if (func? body 'document) (cdr body) (list body))))
    (append-map
      (lambda (par)
        (with start (tmof-item-start par)
          (if (and start (func? (car start) 'item*))
              (append
                (list `(!role "term" ,@(tmof-inline (tmof-all (cdar start)))))
                (tmof-with-role "definition"
                                (tmof-blocks (tmof `(concat ,@(cdr start))))))
              (tmof-with-role "definition"
                              (tmof-blocks
                                (tmof (if start `(concat ,@(cdr start)) par)))))))
      pars)))

(define (tmof-check item)
  ;; "true" or "false" for the box of an item of a task list, else #f
  (and (func? item 'item* 1)
       (let ((s (tmof-plain (cadr item))))
         (cond ((in? s (list (tmof-text "<boxtimes>") (tmof-text "<checkmark>")
                             "[x]" "[X]"))
                "true")
               ((in? s (list (tmof-text "<Box>") (tmof-text "<box>")
                             (tmof-text "<square>") "[ ]"))
                "false")
               (else #f)))))

(define (tmof-items l)
  ;; the items of a list, from its paragraphs: (item paragraph...) ...
  (let loop ((l l) (cur #f) (acc '()))
    (cond ((null? l) (reverse (if cur (cons (reverse cur) acc) acc)))
          ((tmof-item-start (car l))
           (with it (tmof-item-start (car l))
             (loop (cdr l)
                   (list `(concat ,@(cdr it)) (car it))
                   (if cur (cons (reverse cur) acc) acc))))
          (cur (loop (cdr l) (cons (car l) cur) acc))
          ;; text before the first item
          (else (loop (cdr l) (list (car l) '(item)) acc)))))

(define (tmof-list tag l)
  (let* ((body (if (null? l) '(document) (car l)))
         (pars (if (func? body 'document) (cdr body) (list body)))
         (items (tmof-items pars)))
    (if (null? items) '()
        `((,tag
           ,@(map (lambda (it)
                    (let* ((item (car it))
                           (check (tmof-check item))
                           (blocks (append-map (lambda (x) (tmof-blocks (tmof x)))
                                               (cdr it)))
                           ;; the name of an item of a description
                           (name (if (and (func? item 'item* 1) (not check))
                                     (tmof-trim (tmof-inline (tmof (cadr item))))
                                     '()))
                           (blocks (if (null? name) blocks
                                       (tmof-attach `((strong ,@name) " ")
                                                    (if (null? blocks) '((p)) blocks)
                                                    '()))))
                      `(li ,@(if check `((@ (checked ,check))) '()) ,@blocks)))
                  items))))))

(define (tmof-itemize l) (tmof-list 'ul l))
(define (tmof-enumerate l) (tmof-list 'ol l))

(define (tmof-quotation l)
  (with b (tmof-blocks (tmof-all l))
    (if (null? b) '() `((blockquote ,@b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theorems, figures and code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-titled name body)
  ;; the body after a name in bold, as "Theorem 1."
  (let* ((n (tmof-trim (tmof-inline name)))
         (s (if (and (nnull? n) (string? (cAr n))) (cAr n) ""))
         (dot? (and (!= s "") (not (in? (string-ref s (- (string-length s) 1))
                                        '(#\. #\: #\! #\?)))))
         (n* (if (and dot? (nnull? n)) (append n (list ".")) n)))
    (if (null? n) (tmof-blocks body)
        (tmof-attach `((strong ,@n*) " ")
                     (with b (tmof-blocks body) (if (null? b) '((p)) b))
                     '()))))

(define (tmof-render-enunciation l)
  ;; the name with its number, and the body
  (if (< (length l) 2) (tmof-all l)
      (tmof-titled (tmof (car l)) (tmof (cadr l)))))

(define tmof-enunciations
  '(theorem proposition lemma corollary conjecture axiom definition notation
    remark note example convention warning acknowledgments exercise problem
    question solution answer proof algorithm))

(define (tmof-enunciation tag l)
  ;; a theorem which was not expanded: its name without a number
  (let* ((s (symbol->string tag))
         (s (if (string-ends? s "*") (substring s 0 (- (string-length s) 1)) s)))
    (tmof-titled (list (upcase-first s)) (tmof-all l))))

(define (tmof-captioned body name caption)
  ;; a figure or a table, and its caption after the name with its number
  (append (map (lambda (b)
                 ;; a paragraph of images is the figure, and is centered
                 ;; as a table is
                 (if (and (func? b 'table)
                          (not (assoc 'align (tmof-node-attrs b))))
                     `(table (@ ,@(tmof-node-attrs b) (align "center"))
                             ,@(tmof-node-children b))
                 (if (and (func? b 'p) (nnull? (cdr b))
                          (list-and (map (lambda (y)
                                           (or (func? y 'image) (tmof-blank? y)))
                                         (cdr b))))
                     `(!role "figure" ,@(cdr b))
                     b)))
               (tmof-blocks body))
          (tmof-with-role "caption" (tmof-titled name caption))))

(define (tmof-render-figure l)
  ;; the type, the name with its number, the figure and its caption
  (if (< (length l) 4) (tmof-all l)
      (tmof-captioned (tmof (caddr l)) (tmof (cadr l)) (tmof (cadddr l)))))

(define (tmof-figure name l)
  ;; a figure which was not expanded: the figure and its caption
  (if (< (length l) 2) (tmof-all l)
      (tmof-captioned (tmof (car l)) (list name) (tmof (cadr l)))))

(define (tmof-big-figure l) (tmof-figure "Figure" l))
(define (tmof-big-table l) (tmof-figure "Table" l))

(define tmof-languages
  '((cpp-code . "cpp") (python-code . "python") (scm-code . "scheme")
    (shell-code . "sh") (java-code . "java") (javascript-code . "javascript")
    (json-code . "json") (julia-code . "julia") (r-code . "r")
    (scala-code . "scala") (fortran-code . "fortran") (octave-code . "octave")
    (scilab-code . "scilab") (dot-code . "dot") (mmx-code . "mathemagix")
    (verbatim-code . "") (pseudo-code . "") (render-code . "") (code . "")))

(define (tmof-code-block tag l)
  (let* ((lang (assoc-ref tmof-languages tag))
         (s (apply string-append (map tmof-plain l))))
    `((pre ,@(if (== lang "") '() `((@ (lang ,lang)))) ,s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A table of TeXmacs is rows of cells inside formats: (cwith row1 row2
;; col1 col2 variable value) for the cells of a rectangle, (twith variable
;; value) for the table. The formats are applied to a matrix of cells, each
;; an association list of its variables, the last value first.

(define (tmof-table-parts x)
  ;; (rows formats borders? centered?) for the table inside x
  (let loop ((x x) (formats '()) (borders? #f) (centered? #f))
    (cond ((not (pair? x)) (list '() formats borders? centered?))
          ((func? x 'table)
           (list (list-filter (cdr x) (lambda (r) (func? r 'row)))
                 formats borders? centered?))
          ((func? x 'tformat)
           (loop (cAr x) (append formats (cDr (cdr x))) borders? centered?))
          ((in? (car x) '(block block* wide-block))
           (loop (cAr x) formats #t (or centered? (func? x 'block*))))
          ((in? (car x) '(tabular tabular* wide-tabular document))
           (loop (cAr x) formats borders? (or centered? (func? x 'tabular*))))
          (else (list '() formats borders? centered?)))))

(define (tmof-index s n)
  ;; the rank from 0 of the index s of a format among n: -1 is the last
  (with i (or (and (string? s) (string->number s)) 1)
    (max 0 (min (- n 1) (if (< i 0) (+ n i) (- i 1))))))

(define (tmof-border-on? v)
  ;; a width of a border which is not zero
  (and (string? v) (!= v "")
       (let loop ((i 0))
         (cond ((>= i (string-length v)) #f)
               ((in? (string-ref v i) '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t)
               ((or (char-numeric? (string-ref v i)) (char=? (string-ref v i) #\.))
                (loop (+ i 1)))
               (else #f)))))

(define (tmof-table tag l)
  (let* ((parts (tmof-table-parts (cons tag l)))
         (rows (car parts))
         (nrows (length rows))
         (ncols (apply max (cons 1 (map (lambda (r) (length (cdr r))) rows))))
         (cells (make-ahash-table))
         (get (lambda (i j var)
                (with p (assoc var (or (ahash-ref cells (cons i j)) '()))
                  (and p (cdr p)))))
         (put (lambda (i j var val)
                (ahash-set! cells (cons i j)
                            (cons (cons var val)
                                  (or (ahash-ref cells (cons i j)) '())))))
         (width #f))
    (cond
      ((== nrows 0) '())
      ;; inside a heading, whose layout is a table: the text of the cells
      (tmof-flat?
       (append-map
         (lambda (r)
           (append-map (lambda (c)
                         (if (func? c 'cell 1)
                             (append (tmof-inline (tmof (cadr c))) (list " "))
                             '()))
                       (cdr r)))
         rows))
      (else
        (begin
          ;; a block has all the borders, before the formats which follow
          (when (caddr parts)
            (for (i (iota nrows))
              (for (j (iota ncols))
                (for (var '("cell-tborder" "cell-bborder" "cell-lborder"
                            "cell-rborder"))
                  (put i j var "1ln")))))
          (when (cadddr parts)
            (for (i (iota nrows))
              (for (j (iota ncols))
                (put i j "cell-halign" "c"))))
          (for (f (cadr parts))
            (cond ((and (func? f 'cwith 6) (string? (list-ref f 5))
                        (string? (list-ref f 6)))
                   (let ((i1 (tmof-index (list-ref f 1) nrows))
                         (i2 (tmof-index (list-ref f 2) nrows))
                         (j1 (tmof-index (list-ref f 3) ncols))
                         (j2 (tmof-index (list-ref f 4) ncols)))
                     (for (i (iota nrows))
                       (for (j (iota ncols))
                         (when (and (>= i i1) (<= i i2) (>= j j1) (<= j j2))
                           (put i j (list-ref f 5) (list-ref f 6)))))))
                  ((and (func? f 'twith 2) (string? (cadr f)) (string? (caddr f)))
                   (let ((var (cadr f))
                         (val (caddr f)))
                     ;; the borders of the table are those of its outer cells
                     (cond ((== var "table-tborder")
                            (for (j (iota ncols)) (put 0 j "cell-tborder" val)))
                           ((== var "table-bborder")
                            (for (j (iota ncols))
                              (put (- nrows 1) j "cell-bborder" val)))
                           ((== var "table-lborder")
                            (for (i (iota nrows)) (put i 0 "cell-lborder" val)))
                           ((== var "table-rborder")
                            (for (i (iota nrows))
                              (put i (- ncols 1) "cell-rborder" val)))
                           ((and (== var "table-width") (string-ends? val "par"))
                            (set! width val)))))))
          ;; the cells which a wider or a higher one covers
          (for (i (iota nrows))
            (for (j (iota ncols))
              (let ((cs (or (and (get i j "cell-col-span")
                                 (string->number (get i j "cell-col-span")))
                            1))
                    (rs (or (and (get i j "cell-row-span")
                                 (string->number (get i j "cell-row-span")))
                            1)))
                (when (and (not (get i j 'covered)) (or (> cs 1) (> rs 1)))
                  (for (a (iota rs))
                    (for (b (iota cs))
                      (when (and (or (> a 0) (> b 0))
                                 (< (+ i a) nrows) (< (+ j b) ncols))
                        (put (+ i a) (+ j b) 'covered #t))))))))
          (list
            (apply office-node
              (cons* 'table `((width ,width))
                (map (lambda (r i)
                       (cons 'row
                             (map (lambda (j)
                                    (let* ((c (and (< j (length (cdr r)))
                                                   (list-ref (cdr r) j)))
                                           (body (if (func? c 'cell 1) (cadr c) ""))
                                           (letters
                                             (string-append
                                               (if (tmof-border-on? (get i j "cell-tborder")) "t" "")
                                               (if (tmof-border-on? (get i j "cell-bborder")) "b" "")
                                               (if (tmof-border-on? (get i j "cell-lborder")) "l" "")
                                               (if (tmof-border-on? (get i j "cell-rborder")) "r" "")))
                                           (halign (get i j "cell-halign"))
                                           (span (lambda (var)
                                                   (with v (get i j var)
                                                     (and v (string->number v)
                                                          (> (string->number v) 1) v)))))
                                      (if (get i j 'covered)
                                          `(cell (@ (covered "true")))
                                          (apply office-node
                                            (cons* 'cell
                                              `((borders ,(if (== letters "") "none" letters))
                                                (background ,(tmof-color (get i j "cell-background")))
                                                (colspan ,(span "cell-col-span"))
                                                (rowspan ,(span "cell-row-span"))
                                                (align ,(cond ((in? halign '("c" "C")) "center")
                                                              ((in? halign '("r" "R")) "right")
                                                              (else #f))))
                                              (with-global tmof-flat? #f
                                                (tmof-blocks (tmof body))))))))
                                  (iota ncols))))
                     rows (iota nrows))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics on its own lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-math-body l)
  ;; the formula of an equation: the paragraph of its document
  (let ((x (if (null? l) "" (car l))))
    (if (func? x 'document 1) (cadr x) x)))

(define (tmof-equation l) (tmof-display (tmof-math-body l)))

(define (tmof-equation-lab l)
  ;; the formula and its number
  (tmof-display (tmof-math-body l)
                (if (< (length l) 2) "" (tmof-plain (cadr l)))))

(define (tmof-equations tag l)
  ;; several lines of formulas: the table of their parts, which the
  ;; converter of MathML knows
  (let loop ((x (if (null? l) "" (car l))))
    (cond ((func? x 'document 1) (loop (cadr x)))
          ((and (pair? x) (in? (car x) '(tformat table)))
           (tmof-display `(tabular* ,x)))
          (else (tmof-display x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The documentation of TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-tmdoc-title l) (tmof-heading -1 l))

(define (tmof-tmdoc-copyright l)
  (if (null? l) '()
      `((p ,(string-append
              "(c) " (tmof-plain (car l)) " "
              (apply string-append
                     (list-intersperse (map tmof-plain (cdr l)) ", ")))))))

(define (tmof-tmdoc-license l)
  (with b (tmof-blocks (tmof-all l))
    (if (null? b) '() b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-dispatch htable l)
  (let ((x (logic-ref ,htable (car l))))
    (and x (procedure? x) (x (cdr l)))))

(define (tmof-compound l)
  ;; (compound "name" arguments...) is (name arguments...)
  (if (and (nnull? l) (string? (car l)))
      (tmof (cons (string->symbol (car l)) (cdr l)))
      (tmof-all l)))

(define (tmof x)
  ;; the nodes of the tree of the handlers for the TeXmacs tree x
  (cond ((string? x) (if (== x "") '() (list (tmof-text x))))
        ((not (pair? x)) '())
        ((not (symbol? (car x))) '())
        ((in? (car x) '(page-break new-page page-break* new-page*))
         '((!pagebreak)))
        ((tmof-drawing? x) (tmof-picture x #t))
        ((tmof-dispatch 'tmoffice-methods% x) => identity)
        ((assoc (car x) tmof-languages) (tmof-code-block (car x) (cdr x)))
        ((in? (car x) '(tabular tabular* block block* wide-tabular wide-block
                        tformat table))
         (tmof-table (car x) (cdr x)))
        ((in? (car x) '(eqnarray eqnarray* align align* gather gather*
                        multline multline* eqsplit eqsplit*))
         (tmof-equations (car x) (cdr x)))
        ((let* ((s (symbol->string (car x)))
                (s (if (string-ends? s "*")
                       (substring s 0 (- (string-length s) 1)) s)))
           (in? (string->symbol s) tmof-enunciations))
         (tmof-enunciation (car x) (cdr x)))
        ;; the entries of the table of contents
        ((string-starts? (symbol->string (car x)) "toc-") '())
        ;; a tag which is not known: what it holds
        (else (tmof-all (cdr x)))))

(logic-dispatcher tmoffice-methods%
  (document tmof-document)
  (para tmof-document)
  (concat tmof-concat)
  (surround tmof-surround)
  (with tmof-with)
  (compound tmof-compound)
  ((:or rigid hgroup freeze unfreeze syntax move shift resize clipped
        repeat repeat*)
   tmof-first)
  ((:or datoms dlines dpages dbox locus float) tmof-last)
  (phantom tmof-noop)

  ;; what has no meaning in Markdown
  ((:or assign provides label hidden hidden-binding set-binding write quote
        quasiquote tuple attr tmlen macro xmacro arg value quote-value
        cwith twith tmarker
        vspace vspace* no-indent yes-indent no-indent* yes-indent*
        line-break line-sep no-break page-break page-break* no-page-break
        no-page-break* no-break-here no-break-here* no-break-start
        no-break-end new-page new-page* new-dpage new-dpage*
        with-limits flag index subindex subsubindex index-complex
        glossary glossary-explain glossary-dup glossary-line
        table-of-contents the-index the-glossary list-of-figures
        list-of-tables toc-main-1 toc-main-2 toc-normal-1 toc-normal-2
        toc-normal-3 toc-small-1 toc-small-2 toc-dots
        doc-title-block hidden-title tmdoc-flag
        inactive active-inclusion)
   tmof-noop)

  ((:or hspace space htab) tmof-space)
  ((:or next-line new-line) tmof-next-line)
  (hrule tmof-hrule)

  ;; text
  ((:or em dfn var) tmof-em)
  (strong tmof-strong)
  ((:or verbatim code* tt samp kbd) tmof-code)
  (render-key tmof-key)
  (underline tmof-underline)
  ((:or strike-through deleted) tmof-del)
  (marked tmof-marked)
  ((:or rsub lsub) tmof-sub)
  ((:or rsup lsup) tmof-sup)
  ((:or abbr acronym name small smaller large larger) tmof-first)
  (nbsp tmof-nbsp)
  (TeXmacs tmof-TeXmacs)
  (TeX tmof-TeX)
  (LaTeX tmof-LaTeX)
  ((:or hlink hyper-link) tmof-hlink)
  (hlink* tmof-hlink*)
  (action tmof-first)
  ((:or href slink) tmof-href)
  (image tmof-image)
  (specific tmof-specific)
  (reference tmof-reference)
  (pageref tmof-noop)
  (eqref tmof-eqref)
  ((:or cite nocite cite-detail) tmof-cite)
  (footnote tmof-footnote)

  ;; mathematics
  (math tmof-math)
  ((:or equation equation*) tmof-equation)
  (equation-lab tmof-equation-lab)
  (equations-base tmof-equation)

  ;; sections and the title
  ((:or part part* part-title) tmof-part)
  ((:or chapter chapter* chapter-title appendix appendix-title) tmof-chapter)
  ((:or section section* section-title) tmof-section)
  ((:or subsection subsection* subsection-title) tmof-subsection)
  ((:or subsubsection subsubsection* subsubsection-title) tmof-subsubsection)
  ((:or paragraph paragraph* paragraph-title) tmof-paragraph)
  ((:or subparagraph subparagraph* subparagraph-title) tmof-subparagraph)
  ((:or doc-data office-doc-data) tmof-doc-data)
  ((:or abstract abstract-data) tmof-abstract)

  ;; lists and quotations
  ((:or itemize itemize-minus itemize-dot itemize-arrow) tmof-itemize)
  ((:or description description-compact description-dash description-aligned
        description-long description-paragraphs)
   tmof-description)
  ((:or enumerate enumerate-numeric enumerate-roman enumerate-Roman
        enumerate-alpha enumerate-Alpha)
   tmof-enumerate)
  ((:or item item*) tmof-noop)
  ((:or quotation quote-env verse) tmof-quotation)

  ;; environments
  ((:or render-theorem render-remark render-exercise render-proof
        render-solution render-enunciation)
   tmof-render-enunciation)
  ((:or render-big-figure render-small-figure render-big-algorithm
        render-small-algorithm)
   tmof-render-figure)
  ((:or big-figure small-figure) tmof-big-figure)
  ((:or big-table small-table) tmof-big-table)
  (render-bibitem tmof-first)
  ((:or html-div-class html-div-style html-class html-style html-tag
        html-attr)
   tmof-last)

  ;; the documentation of TeXmacs
  ((:or tmdoc-title tmdoc-title* tmdoc-title**) tmof-tmdoc-title)
  (tmdoc-copyright tmof-tmdoc-copyright)
  (tmdoc-license tmof-tmdoc-license))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The office tree, from the tree of the handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmof-top-level 0)   ; the level of the first headings in use

(define (tmof-heading-levels x)
  ;; the levels of the headings of the tree
  (cond ((not (pair? x)) '())
        ((func? x '!h) (list (cadr x)))
        ((in? (car x) '(math image)) '())
        (else (append-map tmof-heading-levels (cdr x)))))

(define (tmof-node-attrs x)
  (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cdadr x) '()))

(define (tmof-node-children x)
  (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cddr x) (cdr x)))

(define (tmof-final-inline x)
  ;; the nodes of the office tree for a node of text
  (cond ((string? x) (if (== x "") '() (list x)))
        ((not (pair? x)) '())
        (else
          (case (car x)
            ((em strong strike underline mark sub sup smallcaps code)
             (with l (tmof-final-inlines (tmof-node-children x))
               (if (null? l) '() (list (cons (car x) l)))))
            ((color)
             (with l (tmof-final-inlines (tmof-node-children x))
               (if (null? l) '()
                   (list `(color (@ ,@(tmof-node-attrs x)) ,@l)))))
            ((a)
             (let ((l (tmof-final-inlines (tmof-node-children x)))
                   (href (with a (assoc 'href (tmof-node-attrs x))
                           (if a (cadr a) ""))))
               (cond ((null? l) '())
                     ((== href "") l)
                     (else (list `(link (@ (href ,href)) ,@l))))))
            ((note) (list (cons 'note (tmof-final-blocks (cdr x)))))
            ((br) '((br)))
            ((math image) (list x))
            ((html footnote img) '())
            ;; blocks inside a text: their text
            ((p !role !h)
             (tmof-final-inlines
               (if (func? x 'p) (cdr x) (cddr x))))
            (else '())))))

(define (tmof-final-inlines l)
  (office-merge (append-map tmof-final-inline l)))

(define (tmof-paragraph attrs l)
  ;; a paragraph of the office tree, or nothing when it has no text
  (with t (tmof-final-inlines l)
    (if (null? t) '() (list (apply office-node (cons* 'p attrs t))))))

(define (tmof-lines s)
  ;; the lines of the text of a piece of code, with breaks between them
  (let loop ((l (string-tokenize-by-char s #\newline)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((null? acc) (loop (cdr l) (list (car l))))
          (else (loop (cdr l) (cons* (car l) '(br) acc))))))

(define (tmof-final-block x)
  ;; the blocks of the office tree for a block
  (cond ((not (pair? x)) '())
        (else
          (case (car x)
            ((p) (tmof-paragraph '() (cdr x)))
            ((!h)
             (if (< (cadr x) 0)
                 (tmof-paragraph '((role "title")) (cddr x))
                 (tmof-paragraph
                   `((role "heading")
                     (level ,(number->string
                               (max 1 (min 9 (+ (- (cadr x) tmof-top-level) 1))))))
                   (cddr x))))
            ((!role) (tmof-paragraph `((role ,(cadr x))) (cddr x)))
            ((!display)
             (list `(p (math (@ (display "true") (form "sxml")) ,(cadr x))
                       ,@(if (and (pair? (cddr x)) (!= (caddr x) ""))
                             `((tab) ,(string-append "(" (caddr x) ")"))
                             '()))))
            ((blockquote)
             (map (lambda (b)
                    (if (and (func? b 'p) (not (ox-attr b 'role)))
                        (apply office-node
                               (cons* 'p '((role "quote")) (ox-children b)))
                        b))
                  (tmof-final-blocks (tmof-node-children x))))
            ((ul ol)
             (with items (list-filter (tmof-node-children x)
                                      (lambda (y) (func? y 'li)))
               (if (null? items) '()
                   (list `(list (@ (kind ,(if (func? x 'ol) "number" "bullet")))
                                ,@(map (lambda (item)
                                         (cons 'item
                                               (tmof-final-blocks
                                                 (tmof-node-children item))))
                                       items))))))
            ((pre)
             (with s (apply string-append
                            (list-filter (tmof-node-children x) string?))
               (list `(p (@ (role "code")) ,@(tmof-lines s)))))
            ((hr) '((rule)))
            ((!pagebreak) '((pagebreak)))
            ((table)
             (list (apply office-node
                          (cons* 'table (tmof-node-attrs x)
                                 (map (lambda (r)
                                        (cons 'row
                                              (map (lambda (c)
                                                     (apply office-node
                                                            (cons* 'cell (tmof-node-attrs c)
                                                                   (tmof-final-blocks
                                                                     (tmof-node-children c)))))
                                                   (cdr r))))
                                      (tmof-node-children x))))))
            (else '())))))

(define (tmof-final-blocks l)
  (append-map tmof-final-block l))

(define (tmof-finalize l)
  ;; the blocks of the office tree: the first level of headings which is
  ;; used is the level 1
  (with levels (list-filter (append-map tmof-heading-levels l)
                            (lambda (n) (>= n 0)))
    (set! tmof-top-level (if (null? levels) 0 (apply min levels)))
    (tmof-final-blocks l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->office x opts)
  (:type (-> stree list stree))
  (:synopsis "Convert the TeXmacs tree @x into an office tree")
  (tmof-initialize opts)
  (set! tmof-document?
        (and (func? x 'document) (tmfile-extract x 'body)
             (or (tmfile-extract x 'TeXmacs) (tmfile-extract x 'style))))
  (with body (if tmof-document? (tmfile-extract x 'body) x)
    `(office ,@(tmof-finalize (tmof-blocks (tmof body))))))
