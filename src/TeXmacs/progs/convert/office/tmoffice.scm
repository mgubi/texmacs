
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
(define tmof-seqs (make-ahash-table)) ; a sequence -> its last number
(define tmof-seq-nr 0)                ; the numbers which were made

(define (tmof-initialize opts)
  (set! tmof-flat? #f)
  (set! tmof-heading-counts (make-list 7 0))
  (set! tmof-heading-first 7)
  (set! tmof-part-nr 0)
  (set! tmof-seqs (make-ahash-table))
  (set! tmof-seq-nr 0))

;; The numbers of a document (of its theorems, figures, tables, equations,
;; references) are written as members of sequences, which the office
;; programs count themselves: Figure 1 is "Figure" and the next number of
;; the sequence Figure. The number which TeXmacs shows is only taken for
;; the next one of a sequence when it is: with another way of numbering
;; (2.1, numbers by sections) it is written as it is.

(define (tmof-seq candidates number)
  ;; the node of the number, in the first of the sequences candidates of
  ;; which it is the next number; else a number which stands for itself
  (let* ((n (string->number number))
         (name (and n (integer? n)
                    (list-find candidates
                               (lambda (c)
                                 (== n (+ 1 (or (ahash-ref tmof-seqs c) 0))))))))
    (set! tmof-seq-nr (+ tmof-seq-nr 1))
    (when name (ahash-set! tmof-seqs name n))
    `(seq (@ (name ,(or name ""))
             (id ,(string-append (or name "Text") (number->string tmof-seq-nr))))
          ,number)))

(define (tmof-number? s)
  ;; a number of a theorem or of a figure: 1, 2.3, A.1
  (and (!= s "") (<= (string-length s) 12)
       (list-or (map char-numeric? (string->list s)))
       (list-and (map (lambda (c) (or (char-numeric? c) (char-alphabetic? c)
                                      (in? c '(#\. #\-))))
                      (string->list s)))))

(define (tmof-numbered-name x candidates)
  ;; the nodes of the name x of a theorem or of a figure, "Theorem 1": its
  ;; number is a number of a sequence
  (let* ((s (tmof-squeeze (tmof-plain x)))
         (s (tmof-trim-lines s))
         (i (let loop ((i (- (string-length s) 1)))
              (cond ((< i 0) #f)
                    ((char=? (string-ref s i) #\space) i)
                    (else (loop (- i 1))))))
         (word (and i (substring s 0 i)))
         (number (and i (substring s (+ i 1) (string-length s)))))
    (if (and number (tmof-number? number) (!= word ""))
        (list (string-append (tmof-text-utf8 word) " ")
              (tmof-seq (append candidates (list (tmof-text-utf8 word))) number))
        (tmof (if (string? x) x x)))))

(define (tmof-text-utf8 s)
  ;; a text which tmof-plain made is in UTF-8 already
  s)

(define (tmof-label-name s)
  ;; The name of the bookmark of the label s. A name has no space: the
  ;; names of the bookmarks of a number are in a list (the attribute
  ;; labels), with spaces between them.
  (string-replace (cork->utf8 s) " " "_"))

(define (tmof-labels x)
  ;; the names of the labels inside the tree x
  (cond ((not (pair? x)) '())
        ((and (func? x 'label 1) (string? (cadr x)))
         (if (string-starts? (cadr x) "auto-") '()
             (list (tmof-label-name (cadr x)))))
        (else (append-map tmof-labels (cdr x)))))

(define (tmof-label l)
  ;; a label: a bookmark, which tmof-bind gives to what it numbers
  (with name (if (and (pair? l) (string? (car l))) (tmof-label-name (car l)) "")
    (if (or (== name "") (string-starts? name "auto-")) '()
        `((bookmark (@ (name ,name)))))))

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
       (in? (car x) '(meta h1 h2 h3 h4 h5 h6 !h !role !para p blockquote ul ol pre hr
                      table footnote-def !display !pagebreak !toc columns))))

(define (tmof-blank? x)
  (and (string? x) (== (string-trim-spaces x) "")))

(define (tmof-merge l)
  ;; with the strings which follow each other as one, and so for the
  ;; texts with the same markup
  ;; (a loop: a paragraph may have many thousands of pieces)
  (let loop ((l (office-join-strings l)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((and (pair? (car l))
                (in? (caar l) '(em strong strike underline mark sub sup smallcaps))
                (null? (tmof-node-attrs (car l)))
                (nnull? (cdr l)) (func? (cadr l) (caar l))
                (null? (tmof-node-attrs (cadr l))))
           ;; (not the markup with attributes: a mark of another color)
           (let run ((m (cddr l)) (kids (append (reverse (cdadr l))
                                                (reverse (cdar l)))))
             (if (and (pair? m) (func? (car m) (caar l)) (null? (tmof-node-attrs (car m))))
                 (run (cdr m) (append (reverse (cdar m)) kids))
                 (loop m (cons `(,(caar l) ,@(tmof-merge (reverse kids)))
                               acc)))))
          (else (loop (cdr l) (cons (car l) acc))))))

(define tmof-keep-spaces? #f)  ; the spaces of a document of the style office count

(define (tmof-squeeze s)
  ;; with one space for several
  (let loop ((l (string->list s)) (acc '()) (sp? #f))
    (cond ((null? l) (list->string (reverse acc)))
          ((char=? (car l) #\space)
           (loop (cdr l) (if sp? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

(define (tmof-trim l)
  ;; text without spaces and line breaks at its ends
  (let* ((l (map (lambda (x) (if (and (string? x) (not tmof-keep-spaces?)) (tmof-squeeze x) x))
                 (tmof-merge l)))
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

;; The converter of MathML does not know the macros of the fractions and
;; of the binomials, nor all the big operators: they are written with what
;; it knows. A fraction without a bar is a fraction in an environment
;; which is a mark, from which tmof-mathml-clean makes it.

(define tmof-no-bar "tmof-no-bar")

(define (tmof-stack x y)
  ;; x above y, without a bar between them
  `(with "math-level" ,tmof-no-bar
     (frac ,(tmof-math-clean x) ,(tmof-math-clean y))))

;; the big operators which the converter of MathML does not know, with the
;; codes of their characters
(define tmof-big-operators
  '(("iint" . #x222c) ("iiint" . #x222d) ("iiiint" . #x2a0c)
    ("oiint" . #x222f) ("oiiint" . #x2230) ("uplus" . #x2a04)
    ("box" . #x25a1) ("triangleup" . #x25b3) ("triangledown" . #x25bd)
    ("curlywedge" . #x22cf) ("curlyvee" . #x22ce) ("parallel" . #x2225)
    ("interleave" . #x2af4)))

(define (tmof-big-operator name)
  ;; the name of a big operator which the converter of MathML knows, or
  ;; its character: an integral is the same with its limits under it
  ;; (intlim) and upright (upint)
  (let* ((n (string-length name))
         (s (if (and (> n 3) (string-ends? name "lim")) (substring name 0 (- n 3))
                name))
         (s (if (and (string-starts? s "up") (string-ends? s "int"))
                (substring s 2 (string-length s))
                s))
         (code (assoc-ref tmof-big-operators s)))
    (if code (office-utf8 code) s)))

(define (tmof-math-clean x)
  ;; a formula without what is not seen, and with its fractions, its
  ;; binomials and its big operators as the converter of MathML knows them
  (cond ((not (pair? x)) x)
        ((in? (car x) '(label assign hidden-binding set-binding write)) "")
        ((and (in? (car x) '(binom tbinom dbinom choose)) (== (length x) 3))
         `(around* "(" ,(tmof-stack (cadr x) (caddr x)) ")"))
        ((func? x 'ontop 2) (tmof-stack (cadr x) (caddr x)))
        ((and (in? (car x) '(tfrac dfrac cfrac)) (== (length x) 3))
         `(frac ,(tmof-math-clean (cadr x)) ,(tmof-math-clean (caddr x))))
        ((func? x 'shrink-inline 1) (tmof-math-clean (cadr x)))
        ((func? x 'bmod)
         `(concat (with "mode" "text" " mod ")
                  ,@(map tmof-math-clean (cdr x))))
        ((func? x 'pmod 1)
         `(around* "(" (concat (with "mode" "text" "mod ")
                               ,(tmof-math-clean (cadr x)))
                   ")"))
        ((func? x 'pod 1) `(around* "(" ,(tmof-math-clean (cadr x)) ")"))
        ((and (func? x 'big 1) (string? (cadr x)))
         `(big ,(tmof-big-operator (cadr x))))
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
    ("&LeftAngleBracket;" . #x27e8) ("&RightAngleBracket;" . #x27e9)
    ("&LeftFloor;" . #x230a) ("&RightFloor;" . #x230b)
    ("&LeftCeiling;" . #x2308) ("&RightCeiling;" . #x2309)
    ("&LeftDoubleBracket;" . #x27e6) ("&RightDoubleBracket;" . #x27e7)
    ("&Backslash;" . #x5c) ("&OverParenthesis;" . #x23dc)
    ("&UnderParenthesis;" . #x23dd) ("&OverBracket;" . #x23b4)
    ("&UnderBracket;" . #x23b5)
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

(define (tmof-word? s)
  ;; a word: two letters of ASCII or more
  (and (> (string-length s) 1)
       (list-and (map (lambda (c) (and (< (char->integer c) 128)
                                       (char-alphabetic? c)))
                      (string->list s)))))

(define (tmof-mathml-clean x)
  ;; the MathML x of the converter, as it is in an office tree
  (cond ((string? x) (tmof-mathml-text x))
        ((func? x '@) x)
        ((not (pair? x)) x)
        ;; a fraction without a bar, see tmof-stack
        ((and (func? x 'm:mstyle) (== (ox-attr x 'scriptlevel) tmof-no-bar)
              (list-1? (ox-children x)) (func? (car (ox-children x)) 'm:mfrac))
         `(m:mfrac (@ (linethickness "0"))
                   ,@(map tmof-mathml-clean (ox-children (car (ox-children x))))))
        ;; no bracket at a side, the "." of TeXmacs: a bracket without a
        ;; character
        ((and (func? x 'm:mo) (in? (ox-attr x 'form) '("prefix" "postfix"))
              (== (ox-children x) '(".")))
         `(m:mo (@ ,@(ox-attrs x))))
        ;; an accent which the converter does not know: what is under it
        ((and (in? (car x) '(m:mover m:munder)) (== (length x) 3)
              (in? (caddr x) '((m:mo "") (m:mo))))
         (tmof-mathml-clean (cadr x)))
        ;; a space at an end of a text, which the converter protects with
        ;; a space which does not break: this space alone
        ((and (func? x 'm:mtext 1) (string? (cadr x)))
         (let* ((s (tmof-mathml-text (cadr x)))
                (nbsp (office-utf8 #xa0))
                (s (if (string-starts? s (string-append nbsp " "))
                       (string-append nbsp (substring s 3 (string-length s)))
                       s))
                (s (if (string-ends? s (string-append " " nbsp))
                       (string-append (substring s 0 (- (string-length s) 3)) nbsp)
                       s)))
           `(m:mtext ,s)))
        ;; an operator which is a word (lim, max) is a name, which is
        ;; upright
        ((and (func? x 'm:mo 1) (string? (cadr x)) (tmof-word? (cadr x)))
         `(m:mi ,(cadr x)))
        (else (cons (car x) (map tmof-mathml-clean (cdr x))))))

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

(define (tmof-display-numbered x numbers labels)
  ;; A formula on its own lines. The numbers which follow it are numbers
  ;; of the sequence of the equations: numbers is a list of (number
  ;; label...), a number and the names of its labels, which are after it.
  ;; The other labels of the formula are at the end.
  (let ((m (with r (tmof-mathml x) (and r (tmof-display-limits r))))
        (bookmarks (lambda (l)
                     (map (lambda (name) `(bookmark (@ (name ,name)))) l))))
    (if m
        `((!display ,m
                    ,@(apply append
                             (list-intersperse
                               (map (lambda (n)
                                      `("(" ,(tmof-seq '("Equation") (car n)) ")"
                                        ,@(bookmarks (cdr n))))
                                    numbers)
                               '(" ")))
                    ,@(bookmarks labels)))
        '())))

(define (tmof-display x . tag)
  ;; a formula on its own lines, with its number, if any, and its labels
  (if (and (nnull? tag) (!= (car tag) ""))
      (tmof-display-numbered x (list (cons (car tag) (tmof-labels x))) '())
      (tmof-display-numbered x '() (tmof-labels x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-noop l) '())
(define (tmof-space l)
  ;; a space; the wide one of a tab of an office document is a tab
  (if (and (pair? l) (== (car l) "2em")) '((tab)) '(" ")))
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
  ;; a link; a link inside the document is a reference to its label
  (if (< (length l) 2) (tmof-all l)
      (let ((body (tmof (car l)))
            (url (tmof-plain (cadr l))))
        (cond ((or (== url "") (tmof-has-block? body)) body)
              ((string-starts? url "#")
               (if (null? (tmof-trim body)) '()
                   `((ref (@ (name ,(string-replace
                                      (substring url 1 (string-length url))
                                      " " "_")))
                          ,@(tmof-merge body)))))
              (else `((a (@ (href ,url)) ,@(tmof-merge body))))))))

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

(define (tmof-pixels data) (office-image-pixels data))

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
      (let* ((name (if (string-starts? name "local:")
                       (substring name 6 (string-length name))
                       name))
             ;; (a name may start with a variable: $TEXMACS_PATH/...)
             (u (if (string-starts? name "$")
                    (url-complete (unix->url name) "r")
                    (unix->url name)))
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
  ;; picture. The temporary files are removed, whatever happens.
  (let* ((base (url-temp))
         (png (url-glue base ".png"))
         (pdf (url-glue base ".pdf"))
         (svg (url-glue base ".svg"))
         ;; (without a window, as texmacs -headless, there are no fonts to
         ;; typeset with: Qt would end TeXmacs)
         (r (if (headless?) '()
                (catch #t
                  (lambda () (tmof-picture* x vector png pdf svg))
                  (lambda args '())))))
    (for (u (list png pdf svg))
      (when (url-exists? u) (system-remove u)))
    r))

(define (tmof-picture* x vector png pdf svg)
  (let* ((ext (print-snippet png x #t)))
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

(define (tmof-clip-part s unit)
  ;; the part of a side which "0.1w" says, or #f
  (and (string? s) (string-ends? s unit)
       (office-number (substring s 0 (- (string-length s) 1)))))

(define (tmof-clipped l)
  ;; an image of which parts are cut: the image, with these parts (the
  ;; style office reads a cropped picture so)
  (let* ((r (if (null? l) '() (tmof (car l))))
         (img (and (list-1? r) (func? (car r) 'image) (car r)))
         (x1 (and (>= (length l) 5) (tmof-clip-part (list-ref l 1) "w")))
         (y1 (and (>= (length l) 5) (tmof-clip-part (list-ref l 2) "h")))
         (x2 (and (>= (length l) 5) (tmof-clip-part (list-ref l 3) "w")))
         (y2 (and (>= (length l) 5) (tmof-clip-part (list-ref l 4) "h"))))
    (if (not (and img x1 y1 x2 y2 (> x2 x1) (> y2 y1))) r
        (let* ((attrs (tmof-node-attrs img))
               (get (lambda (k) (with a (assoc k attrs) (and a (cadr a)))))
               (w (office-length->cm (get 'width)))
               (h (office-length->cm (get 'height)))
               (crop (string-recompose
                       (map office-decimal (list x1 (- 1 y2) (- 1 x2) y1)) " ")))
          (if (not (and w h)) r
              `((image (@ ,@(list-filter attrs (lambda (a) (not (in? (car a) '(width height)))))
                          (width ,(tmof-cm-string (* w (- x2 x1))))
                          (height ,(tmof-cm-string (* h (- y2 y1))))
                          (crop ,crop)))))))))

(define (tmof-specific l)
  ;; what is for another medium only is left out
  (cond ((< (length l) 2) '())
        ;; (not what is drawn on the screen only: a mark of a space)
        ((in? (car l) '("texmacs" "printer" "image")) (tmof (cadr l)))
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
        ;; the alignment of the paragraphs
        ((and (== var "par-mode") (in? val '("left" "center" "right" "justify")))
         (tmof-para-attr body 'align val))
        ;; the formatting of the text and of the paragraphs, as the style
        ;; office reads it from a file
        ((== var "font-size")
         (with r (office-number val)
           (if (not r) body
               (tmof-attr-wrap* 'size `((value ,(office-decimal (* r tmof-base-size)))) body))))
        ;; a font of the office programs, by its name
        ((== var "font")
         (with f (tmof-office-font val)
           (if f (tmof-attr-wrap* 'font `((value ,f)) body) body)))
        ((and (== var "font-family") (in? val '("ss" "rm")))
         (tmof-attr-wrap* 'font `((value ,(if (== val "ss") "sans" "serif"))) body))
        ((== var "marked-color")
         (map (lambda (y)
                (if (and (func? y 'mark) (not (func? (cadr y) '@)))
                    `(mark (@ (value ,(or (tmof-color val) val))) ,@(cdr y))
                    y))
              body))
        ((in? var '("par-left" "par-right" "par-first"))
         (with v (tmof-points val)
           (if (not v) body
               (tmof-para-attr body (cdr (assoc var '(("par-left" . indent-left)
                                                       ("par-right" . indent-right)
                                                       ("par-first" . first-indent))))
                               (office-decimal v)))))
        ((== var "par-sep")
         (with v (tmof-points val)
           (if (not v) body
               (tmof-para-attr body 'line-spacing
                               (office-decimal (/ (+ 1 (/ v tmof-base-size)) 1.2))))))
        ((and (== var "ornament-color") (not (in? val '("" "#fff0" "white"))))
         (tmof-para-attr body 'background (or (tmof-color val) val)))
        ((and (== var "ornament-border") (not (in? val '("" "0ln" "0pt"))))
         (tmof-para-attr body 'border "true"))
        (else body)))

(define tmof-base-size 10.0)  ; the size of the font of the document

(define tmof-texmacs-fonts
  ;; the fonts of TeXmacs, by the names of the office programs
  '(("roman" . #f) ("modern" . #f) ("concrete" . #f) ("termes" . "Times New Roman")
    ("pagella" . "Palatino Linotype") ("bonum" . "Bookman Old Style")
    ("schola" . "Century Schoolbook") ("heros" . "Arial") ("adventor" . "Century Gothic")
    ("cursor" . "Courier New") ("chorus" . "Zapf Chancery") ("stix" . "STIX Two Text")
    ("dejavu" . "DejaVu Serif") ("linux" . "Linux Libertine O") ("libertine" . "Linux Libertine O")))

(define (tmof-office-font f)
  ;; the name of the font f for the office programs, or #f for the fonts
  ;; of TeXmacs which have none
  (cond ((not (string? f)) #f)
        ((assoc (locase-all f) tmof-texmacs-fonts)
         => (lambda (p) (cdr p)))
        (else f)))

(define (tmof-points s)
  ;; the length s in points: fn are the size of the font
  (cond ((not (string? s)) #f)
        ((string-ends? s "fn")
         (with v (office-number (substring s 0 (- (string-length s) 2)))
           (and v (* v tmof-base-size))))
        (else (office-points s))))

(define (tmof-para-attr body key val)
  ;; the paragraphs of the body with the attribute key
  (if (tmof-has-block? body)
      (map (lambda (b)
             (cond ((func? b 'p) `(!para ((,key ,val)) ,@(cdr b)))
                   ((func? b '!para)
                    `(!para ((,key ,val) ,@(list-filter (cadr b)
                                                        (lambda (a) (!= (car a) key))))
                            ,@(cddr b)))
                   (else b)))
           (tmof-blocks body))
      (if (null? (tmof-trim body)) body
          `((!para ((,key ,val)) ,@body)))))

(define (tmof-attr-wrap* tag attrs body)
  ;; the text of body inside the node tag; the paragraphs of blocks each
  (cond ((tmof-has-block? body)
         (map (lambda (b)
                (cond ((func? b 'p) `(p (,tag (@ ,@attrs) ,@(cdr b))))
                      ((func? b '!para) `(!para ,(cadr b) (,tag (@ ,@attrs) ,@(cddr b))))
                      (else b)))
              (tmof-blocks body)))
        ((null? (tmof-trim body)) body)
        (else `((,tag (@ ,@attrs) ,@(tmof-merge body))))))

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
  ;; the spaces around a paragraph, as the style office writes them
  (if (and (== (length l) 3) (func? (car l) 'vspace*) (func? (cadr l) 'vspace))
      (let* ((before (tmof-points (cadr (car l))))
             (after (tmof-points (cadr (cadr l))))
             (b (tmof (caddr l)))
             (b (if before (tmof-para-attr b 'space-before (office-decimal before)) b)))
        (if after (tmof-para-attr b 'space-after (office-decimal after)) b))
      (tmof-surround* l)))

(define (tmof-surround* l)
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
;; are known: 0 for a part, 1 for a chapter, 2 for a section..., or top for
;; a heading of the first level which is in use, whatever it is: an
;; appendix is a chapter of a book and a section of an article. A formula on
;; its own lines is (!display "latex") until then. The title of a section
;; may be laid out as a table, its number in a cell and its text in another:
;; inside a heading the cells of a table are text.

(define tmof-flat? #f)

(define (tmof-numbered-title x)
  ;; (number . title) when the tree x of a title is the layout which the
  ;; styles make of a number and a title, a row of two cells; else #f
  (cond ((not (pair? x)) #f)
        ((and (func? x 'row 2) (func? (cadr x) 'cell 1) (func? (caddr x) 'cell 1))
         (cons (cadr (cadr x)) (cadr (caddr x))))
        (else (let loop ((l (cdr x)))
                (cond ((null? l) #f)
                      ((tmof-numbered-title (car l)) => identity)
                      (else (loop (cdr l))))))))

(define (tmof-named-title x)
  ;; (number . title) when the tree x of a title is "Chapter 1", a line
  ;; and the title, as the styles make the title of a chapter; else #f
  (and (func? x 'concat) (pair? (cdr x)) (string? (cadr x))
       (let* ((words (string-tokenize-by-char (cadr x) #\space))
              (n (and (== (length words) 2) (cadr words)))
              (rest (let loop ((r (cddr x)))
                      (cond ((null? r) #f)
                            ((func? (car r) 'new-line) (cdr r))
                            ((and (pair? (car r)) (in? (caar r) '(vspace vspace*)))
                             (loop (cdr r)))
                            (else #f)))))
         (and n rest (pair? rest)
              (or (tmof-number? n)
                  (list-and (map (lambda (c) (in? c '(#\I #\V #\X #\L #\C)))
                                 (string->list n))))
              (cons n (if (list-1? rest) (car rest) (cons 'concat rest)))))))

(define (tmof-heading level l)
  ;; a heading; its number, when it has one, is kept apart: the office
  ;; programs number the headings themselves. A part or a chapter starts
  ;; a page.
  (append
    (if (in? level '(0 1)) '((!pagebreak)) '())
    (if (in? level '(top 1)) (tmof-heading* level (tmof-appendix-title l))
        (tmof-heading* level l))))

(define (tmof-appendix-title l)
  ;; "Appendix A", a line and the title are "Appendix A. The title": the
  ;; office programs number the appendices as the chapters
  (or (and (list-1? l) (func? (car l) 'concat) (pair? (cdar l)) (string? (cadar l))
           ;; (not "Chapter 1": a chapter has its number)
           (with w (string-tokenize-by-char (cadar l) #\space)
             (not (and (== (length w) 2) (tmof-number? (cadr w)))))
           (let* ((x (car l))
                  (rest (let loop ((r (cddr x)))
                          (cond ((null? r) #f)
                                ((func? (car r) 'new-line) (cdr r))
                                ((and (pair? (car r)) (in? (caar r) '(vspace vspace*)))
                                 (loop (cdr r)))
                                (else #f)))))
             (and rest (pair? rest)
                  (list `(concat ,(cadr x) ". " ,@rest)))))
      l))

(define (tmof-heading* level l)
  (let* ((parts (and (list-1? l)
                     (or (tmof-numbered-title (car l))
                         (and (in? level '(0 1)) (tmof-named-title (car l))))))
         ;; (the number is the text of its cell, without the space after it)
         (number (and parts
                      (with c (car parts)
                        (tmof-trim-lines
                          (tmof-squeeze
                            (cond ((string? c) (tmof-plain c))
                                  ((func? c 'concat)
                                   (apply string-append
                                          (map tmof-plain (list-filter (cdr c) string?))))
                                  (else "")))))))
         (number (and number (tmof-number? number) number))
         (old tmof-flat?))
    (set! tmof-flat? #t)
    (with t (tmof-trim (tmof-inline (if number (tmof (cdr parts)) (tmof-all l))))
      (set! tmof-flat? old)
      (cond ((null? t) '())
            (number `((!h ,level (!number ,number) ,@t)))
            (else `((!h ,level ,@t)))))))

;; The numbers of the headings of a document which was not expanded, as
;; TeXmacs makes them: the counters of the levels from the first one which
;; is used
(define tmof-heading-counts (make-list 7 0))
(define tmof-heading-first 7)

(define tmof-part-nr 0)

(define (tmof-roman n)
  ;; the number n < 4000 in capital Roman numerals
  (let loop ((n n) (l '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                        (100 . "C") (90 . "XC") (50 . "L") (40 . "XL") (10 . "X")
                        (9 . "IX") (5 . "V") (4 . "IV") (1 . "I")))
             (acc ""))
    (cond ((or (<= n 0) (null? l)) acc)
          ((>= n (caar l)) (loop (- n (caar l)) l (string-append acc (cdar l))))
          (else (loop n (cdr l) acc)))))

(define (tmof-part-heading l)
  ;; a part: "Part I. Its title", a heading of the first level without a
  ;; number of the office programs, which do not count the chapters by
  ;; parts
  (set! tmof-part-nr (+ tmof-part-nr 1))
  (append '((!pagebreak))
          (tmof-heading* 0 (list `(concat ,(string-append "Part " (tmof-roman tmof-part-nr)
                                                          ". ")
                                          ,@(if (null? l) '() (list (car l))))))))

(define (tmof-part-numbered-title l) (tmof-part-heading l))

(define (tmof-counted-heading level l)
  (if (== level 0) (tmof-part-heading l)
      (tmof-counted-heading* level l)))

(define (tmof-counted-heading* level l)
  (set! tmof-heading-first (min tmof-heading-first level))
  (set! tmof-heading-counts
        (map (lambda (n i) (cond ((== i level) (+ n 1)) ((> i level) 0) (else n)))
             tmof-heading-counts (iota 7)))
  (let* ((number (string-recompose
                   (map number->string
                        (list-tail (list-head tmof-heading-counts (+ level 1))
                                   tmof-heading-first))
                   "."))
         (t (with old tmof-flat?
              (set! tmof-flat? #t)
              (with r (tmof-trim (tmof-inline (tmof-all l)))
                (set! tmof-flat? old)
                r))))
    (append (if (in? level '(0 1)) '((!pagebreak)) '())
            (if (null? t) '() `((!h ,level (!number ,number) ,@t))))))

(define (tmof-part-numbered l) (tmof-counted-heading 0 l))
(define (tmof-chapter-numbered l) (tmof-counted-heading 1 l))
(define (tmof-section-numbered l) (tmof-counted-heading 2 l))
(define (tmof-subsection-numbered l) (tmof-counted-heading 3 l))
(define (tmof-subsubsection-numbered l) (tmof-counted-heading 4 l))

;; The macros of the style office (office.ts): what they keep is written
;; again, as it was read

(define (tmof-office-par l)
  ;; a paragraph of a named style: each of its paragraphs has the style
  (if (< (length l) 2) '()
      (with name (tmof-plain (car l))
        (map (lambda (b)
               (cond ((func? b 'p) `(!para ((style ,name)) ,@(cdr b)))
                     ((func? b '!para) `(!para ((style ,name) ,@(cadr b)) ,@(cddr b)))
                     (else b)))
             (tmof-blocks (tmof (cadr l)))))))

(define (tmof-attr-wrap tag attrs l)
  ;; the nodes of the tree l inside the node tag with these attributes
  (with r (tmof (cadr l))
    (if (tmof-has-block? r) r
        (if (null? (tmof-trim r)) '()
            `((,tag (@ ,@attrs) ,@(tmof-merge r)))))))

(define (tmof-office-span l)
  (if (< (length l) 2) '() (tmof-attr-wrap 'span `((style ,(tmof-plain (car l)))) l)))

(define (tmof-office-field l)
  ;; a field: its instruction (or the element of OpenDocument), its result
  (if (< (length l) 2) '()
      (let* ((code (tmof-plain (car l)))
             (r (tmof (cadr l))))
        `((field (@ ,@(if (string-starts? code "<") `((xml ,code)) `((code ,code))))
                 ,@(tmof-merge r))))))

(define (tmof-office-change tag l)
  (if (< (length l) 3) '()
      (with r (tmof (caddr l))
        (if (null? r) '()
            `((,tag (@ ,@(tmof-who l)) ,@(tmof-merge r)))))))

(define (tmof-who l)
  ;; the author and the date of a comment or of a change, when it has them
  (list-filter `((author ,(tmof-plain (car l))) (date ,(tmof-plain (cadr l))))
               (lambda (a) (!= (cadr a) ""))))

(define (tmof-office-ins l) (tmof-office-change 'ins l))
(define (tmof-office-del l) (tmof-office-change 'del l))

(define (tmof-office-kind tag l)
  ;; an underline or a line through of a kind, a spacing of letters (in
  ;; points); a value which the office formats do not have is left out
  (if (< (length l) 2) '()
      (let* ((v (tmof-plain (car l)))
             (v (cond ((eq? tag 'underline) (and (in? v office-underline-kinds) v))
                      ((eq? tag 'strike) (and (== v "double") v))
                      (else (with n (or (office-number v) (tmof-points v))
                              (and n (< (abs n) 100) (office-decimal n)))))))
        (cond (v (tmof-attr-wrap tag `((value ,v)) l))
              ((eq? tag 'letters) (tmof (cadr l)))
              (else (tmof-wrap tag (tmof (cadr l))))))))

(define (tmof-office-underline l) (tmof-office-kind 'underline l))
(define (tmof-office-strike l) (tmof-office-kind 'strike l))
(define (tmof-office-letters l) (tmof-office-kind 'letters l))

(define (tmof-office-font-tag l)
  ;; a font which TeXmacs does not have, by its name
  (if (< (length l) 3) '()
      (with r (tmof (caddr l))
        (tmof-attr-wrap* 'font `((value ,(tmof-plain (car l)))) r))))

(define (tmof-office-float l)
  ;; a picture which floats beside its paragraph: where it is, and how the
  ;; text goes around it
  (if (< (length l) 3) '()
      (let* ((side (tmof-plain (car l)))
             (spec (append-map
                     (lambda (kv)
                       (with i (string-search-forwards "=" 0 kv)
                         (if (<= i 0) '()
                             (with k (string->symbol (substring kv 0 i))
                               (if (in? k (append office-float-keys '(next)))
                                   (list (list k (substring kv (+ i 1) (string-length kv))))
                                   '())))))
                     (string-tokenize-by-char (tmof-plain (cadr l)) #\space)))
             (attrs (cons (list 'float (if (in? side '("left" "right" "center")) side "left"))
                          (list-filter spec (lambda (a) (!= (car a) 'float))))))
        (map (lambda (y) (if (func? y 'image) (office-add-attrs y attrs) y))
             (tmof (caddr l))))))

(define (tmof-change-case l)
  ;; capitals, lower case; the first letter in capitals is the text as it is
  (cond ((< (length l) 2) (tmof-all l))
        ((== (cadr l) "UPCASE") (tmof-wrap 'caps (tmof (car l))))
        ((== (cadr l) "locase") (tmof-attr-wrap* 'caps '((value "lower")) (tmof (car l))))
        (else (tmof (car l)))))

(define (tmof-office-capitalize l)
  ;; the first letters of the words in capitals
  (if (null? l) '()
      (tmof-attr-wrap* 'caps '((value "capital")) (tmof (car l)))))

(define (tmof-office-comment l)
  (if (< (length l) 3) '()
      `((comment (@ ,@(tmof-who l)) ,@(tmof-blocks (tmof (caddr l)))))))

(define (tmof-office-columns* l)
  ;; columns with the line between them, and the space between them
  (if (< (length l) 4) '()
      `((columns (@ (count ,(tmof-plain (car l)))
                    ,@(if (== (tmof-plain (cadr l)) "true") '((line "true")) '())
                    ,@(with g (tmof-plain (caddr l))
                        (if (office-length->cm g) `((gap ,g)) '())))
                 ,@(tmof-blocks (tmof (cadddr l)))))))

(define (tmof-office-columns l)
  (if (< (length l) 2) '()
      `((columns (@ (count ,(tmof-plain (car l)))) ,@(tmof-blocks (tmof (cadr l)))))))

(define (tmof-part l) (tmof-heading 0 l))
(define (tmof-chapter l) (tmof-heading 1 l))
(define (tmof-appendix l) (tmof-heading 'top l))
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

(define (tmof-toc l)
  ;; The table of contents, as the editor expands it: its title and its
  ;; entries. The entries are left out: the office programs make them
  ;; from the headings.
  (with title (if (null? l) '()
                  (with-global tmof-flat? #t
                    (tmof-trim (tmof-inline (tmof (car l))))))
    `((!toc ,@title))))

(define (tmof-table-of-contents l)
  ;; a table of contents which was not expanded has no title
  '((!toc)))

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
          (if (tmof-bibitem-start? par)
              ;; an entry of the bibliography
              (tmof-with-role "bibitem" (tmof-blocks (tmof par)))
          (if (and start (func? (car start) 'item*))
              (append
                (list `(!role "term" ,@(tmof-inline (tmof-all (cdar start)))))
                (tmof-with-role "definition"
                                (tmof-blocks (tmof `(concat ,@(cdr start))))))
              (tmof-with-role "definition"
                              (tmof-blocks
                                (tmof (if start `(concat ,@(cdr start)) par))))))))
      pars)))

(define (tmof-bibitem l)
  ;; the label of an entry of the bibliography, "[1]": its number is one
  ;; of the sequence of the references
  (let* ((s (tmof-trim-lines (tmof-squeeze (tmof-plain (if (null? l) "" (car l))))))
         (n (string-length s)))
    (cond ((== s "") '())
          ((and (> n 2) (string-starts? s "[") (string-ends? s "]"))
           (list "[" (tmof-seq '("Reference") (substring s 1 (- n 1))) "] "))
          (else (list s " ")))))

(define (tmof-bibitem-start? par)
  (or (func? par 'render-bibitem)
      (and (func? par 'concat) (pair? (cdr par)) (func? (cadr par) 'render-bibitem))))

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

(define (tmof-list tag l . format)
  ;; a list; a numbered one may have a way of numbering (a, A, i, I)
  (let* ((body (if (null? l) '(document) (car l)))
         (pars (if (func? body 'document) (cdr body) (list body)))
         ;; the number before the first item, when the list starts at
         ;; another one than 1
         (first (and (pair? pars) (func? (car pars) 'concat) (pair? (cdar pars))
                     (func? (cadar pars) 'assign 2) (== (cadr (cadar pars)) "item-nr")
                     (string? (caddr (cadar pars)))
                     (office-integer (caddr (cadar pars)) #f 0 100000)))
         (pars (if first (cons (cons 'concat (cddar pars)) (cdr pars)) pars))
         (items (tmof-items pars))
         (attrs (append (if (null? format) '() `((format ,(car format))))
                        (if first `((start ,(number->string (+ first 1)))) '()))))
    (if (null? items) '()
        `((,tag
           ,@(if (null? attrs) '() `((@ ,@attrs)))
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
(define (tmof-enumerate-alpha l) (tmof-list 'ol l "a"))
(define (tmof-enumerate-Alpha l) (tmof-list 'ol l "A"))
(define (tmof-enumerate-roman l) (tmof-list 'ol l "i"))
(define (tmof-enumerate-Roman l) (tmof-list 'ol l "I"))

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
         (dot? (or (and (nnull? n) (func? (cAr n) 'seq))
                   (and (!= s "") (not (in? (string-ref s (- (string-length s) 1))
                                            '(#\. #\: #\! #\?))))))
         (n* (if (and dot? (nnull? n)) (append n (list ".")) n)))
    (if (null? n) (tmof-blocks body)
        (tmof-attach `((strong ,@n*) " ")
                     (with b (tmof-blocks body) (if (null? b) '((p)) b))
                     '()))))

(define (tmof-enunciation-as role sequence l)
  ;; the name with its number, and the body: paragraphs with a role
  (if (< (length l) 2) (tmof-all l)
      (tmof-with-role role
                      (tmof-titled (tmof-numbered-name (car l) (list sequence))
                                   (tmof (cadr l))))))

(define (tmof-render-theorem l) (tmof-enunciation-as "theorem" "Theorem" l))
(define (tmof-render-remark l) (tmof-enunciation-as "remark" "Theorem" l))
(define (tmof-render-exercise l) (tmof-enunciation-as "remark" "Exercise" l))
(define (tmof-render-proof l) (tmof-enunciation-as "proof" "Proof" l))
(define (tmof-render-enunciation l) (tmof-enunciation-as "remark" "Theorem" l))

(define tmof-enunciations
  '(theorem proposition lemma corollary conjecture axiom definition notation
    remark note example convention warning acknowledgments exercise problem
    question solution answer proof algorithm))

(define (tmof-enunciation tag l)
  ;; a theorem which was not expanded: its name without a number
  (let* ((s (symbol->string tag))
         (s (if (string-ends? s "*") (substring s 0 (- (string-length s) 1)) s))
         (role (cond ((== s "proof") "proof")
                     ((in? s '("theorem" "proposition" "lemma" "corollary"
                               "conjecture" "axiom" "definition" "notation"))
                      "theorem")
                     (else "remark"))))
    (tmof-with-role role (tmof-titled (list (upcase-first s)) (tmof-all l)))))

(define (tmof-captioned body name caption)
  ;; a figure or a table, and its caption after the name with its number
  (append (map (lambda (b)
                 ;; a paragraph in the float is the figure, and is
                 ;; centered as a table is
                 (if (and (func? b 'table)
                          (not (assoc 'align (tmof-node-attrs b))))
                     `(table (@ ,@(tmof-node-attrs b) (align "center"))
                             ,@(tmof-node-children b))
                 ;; (and so is whatever else is in the float: a formula)
                 (if (and (func? b 'p) (nnull? (cdr b)))
                     `(!role "figure" ,@(cdr b))
                     b)))
               (tmof-blocks body))
          (tmof-with-role "caption" (tmof-titled name caption))))

(define (tmof-render-figure l)
  ;; the type, the name with its number, the figure and its caption
  (if (< (length l) 4) (tmof-all l)
      (tmof-captioned (tmof (caddr l))
                      (tmof-numbered-name
                        (cadr l)
                        (list (upcase-first (tmof-plain (car l)))))
                      (tmof (cadddr l)))))

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
          ((null? (cdr x)) (list '() formats borders? centered?))
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
                     ;; (the cells of the rectangle only: many formats of
                     ;; a cell each do not take the time of all the cells)
                     (for (a (iota (max 0 (- (+ i2 1) i1))))
                       (for (b (iota (max 0 (- (+ j2 1) j1))))
                         (put (+ i1 a) (+ j1 b) (list-ref f 5) (list-ref f 6))))))
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

;; A row of several equations which has a number ends, as the editor
;; expands it, with a space which pushes to the right the text "(2)". The
;; numbers of the rows are taken out of the formula, and follow it, each
;; before the labels of its row.

(define (tmof-eq-mark x)
  ;; the number of the text x, "(2)" as the editor writes it, or #f
  (and (func? x 'with) (string? (cAr x))
       (let loop ((l (cDr (cdr x))))
         (cond ((or (null? l) (null? (cdr l))) #f)
               ((and (== (car l) "mode") (== (cadr l) "text")) #t)
               (else (loop (cddr l)))))
       (with s (tmof-strip (cAr x) "(" ")")
         (and s (tmof-number? s) s))))

(define (tmof-eq-number x)
  ;; the number at the end of the row x, or #f
  (cond ((not (pair? x)) #f)
        ((func? x 'concat)
         (let loop ((l (cdr x)) (pushed? #f))
           (cond ((null? l) #f)
                 ((func? (car l) 'htab) (loop (cdr l) #t))
                 ((and pushed? (tmof-eq-mark (car l))) => identity)
                 ((tmof-eq-number (car l)) => identity)
                 (else (loop (cdr l) pushed?)))))
        (else (let loop ((l (cdr x)))
                (cond ((null? l) #f)
                      ((tmof-eq-number (car l)) => identity)
                      (else (loop (cdr l))))))))

(define (tmof-eq-clean x)
  ;; the formula x without the numbers at the ends of its rows
  (cond ((not (pair? x)) x)
        ((func? x 'concat)
         (let loop ((l (cdr x)) (pushed? #f) (acc '()))
           (cond ((null? l) (cons 'concat (reverse acc)))
                 ((func? (car l) 'htab) (loop (cdr l) #t acc))
                 ((and pushed? (tmof-eq-mark (car l))) (loop (cdr l) pushed? acc))
                 (else (loop (cdr l) pushed? (cons (tmof-eq-clean (car l)) acc))))))
        (else (cons (car x) (map tmof-eq-clean (cdr x))))))

(define (tmof-eq-rows x)
  ;; the rows of the table of several equations
  (cond ((not (pair? x)) '())
        ((func? x 'row) (list x))
        (else (append-map tmof-eq-rows (cdr x)))))

(define (tmof-equations tag l)
  ;; several lines of formulas: the table of their parts, which the
  ;; converter of MathML knows
  (let loop ((x (if (null? l) "" (car l))))
    (cond ((func? x 'document 1) (loop (cadr x)))
          ((and (pair? x) (in? (car x) '(tformat table)))
           (let* ((rows (tmof-eq-rows x))
                  (numbered (list-filter rows tmof-eq-number))
                  (others (list-filter rows (lambda (r) (not (tmof-eq-number r))))))
             ;; (the class of the formula says that its table is lines
             ;; of equations, see mathml->omml)
             (map (lambda (b)
                    (if (and (func? b '!display) (pair? (cadr b)) (symbol? (car (cadr b))))
                        (let ((m (cadr b)))
                          `(!display (,(car m) (@ (class "tm-eqarray")
                                                  ,@(tmof-node-attrs m))
                                               ,@(tmof-node-children m))
                                     ,@(cddr b)))
                        b))
                  (tmof-display-numbered
                    `(tabular* ,(tmof-eq-clean x))
                    (map (lambda (r) (cons (tmof-eq-number r) (tmof-labels r))) numbered)
                    (append-map tmof-labels others)))))
          (else (tmof-display x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The documentation of TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmof-tmdoc-title l)
  ;; the title of a page of the documentation: the logo of TeXmacs before
  ;; it and a rule under it, at the left, as the style draws them
  (let ((logo (tmof-image '("local:$TEXMACS_PATH/misc/images/texmacs-256.png"
                            "3em" "3em" "" "")))
        (h (tmof-heading -1 l)))
    (if (and (list-1? h) (func? (car h) '!h) (list-1? logo)
             (func? (car logo) 'image))
        `((!para ((role "title") (align "left")) ,(car logo) " " ,@(cddar h)) (hr))
        (append (map (lambda (b)
                       (if (func? b '!h)
                           `(!para ((role "title") (align "left")) ,@(cddr b))
                           b))
                     h)
                '((hr))))))

(define (tmof-tmdoc-copyright l)
  (if (null? l) '()
      `((hr)
        (p ,(string-append
              (cork->utf8 "<copyright>") " " (tmof-plain (car l)) " "
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
        ;; what the expansion of the document could not do
        ((func? x 'error) '())
        ;; the screens of a presentation, each on a page of its own
        ((func? x 'screens)
         (append-map (lambda (y)
                       (with b (tmof (if (and (pair? y) (in? (car y) '(shown hidden))
                                              (pair? (cdr y)))
                                         (cadr y) y))
                         (if (null? b) '() (cons '(!pagebreak) (tmof-blocks b)))))
                     (cdr x)))
        ;; the headers and footers of a page: those of the file are kept
        ;; with it (see the style office)
        ((in? (car x) '(set-this-page-header set-this-page-footer set-header
                        set-footer set-odd-page-header set-even-page-header
                        set-odd-page-footer set-even-page-footer))
         '())
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
  ((:or rigid hgroup freeze unfreeze syntax move shift resize
        repeat repeat*)
   tmof-first)
  ((:or datoms dlines dpages dbox locus float) tmof-last)
  (phantom tmof-noop)

  ;; what has no meaning in Markdown
  ((:or assign provides hidden hidden-binding set-binding write quote
        quasiquote tuple attr tmlen macro xmacro arg value quote-value
        cwith twith tmarker
        vspace vspace* no-indent yes-indent no-indent* yes-indent*
        line-break line-sep no-break page-break page-break* no-page-break
        no-page-break* no-break-here no-break-here* no-break-start
        no-break-end new-page new-page* new-dpage new-dpage*
        with-limits flag index subindex subsubindex index-complex
        glossary glossary-explain glossary-dup glossary-line
        the-index the-glossary list-of-figures
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
  ((:or part* part-title) tmof-part)
  ((:or chapter* chapter-title) tmof-chapter)
  ((:or appendix appendix-title) tmof-appendix)
  ((:or section* section-title) tmof-section)
  ((:or subsection* subsection-title) tmof-subsection)
  ((:or subsubsection* subsubsection-title) tmof-subsubsection)
  ;; (in a document which was not expanded, the headings with numbers)
  (clipped tmof-clipped)
  (office-par tmof-office-par)
  (office-span tmof-office-span)
  (office-field tmof-office-field)
  (office-ins tmof-office-ins)
  (office-del tmof-office-del)
  (office-comment tmof-office-comment)
  (office-columns tmof-office-columns)
  (office-columns* tmof-office-columns*)
  (office-underline tmof-office-underline)
  (office-strike tmof-office-strike)
  (office-letters tmof-office-letters)
  (office-font tmof-office-font-tag)
  (change-case tmof-change-case)
  (office-capitalize tmof-office-capitalize)
  (office-float tmof-office-float)
  (part tmof-part-numbered)
  (part-numbered-title tmof-part-numbered-title)
  (chapter tmof-chapter-numbered)
  (section tmof-section-numbered)
  (subsection tmof-subsection-numbered)
  (subsubsection tmof-subsubsection-numbered)
  ((:or paragraph paragraph* paragraph-title) tmof-paragraph)
  ((:or subparagraph subparagraph* subparagraph-title) tmof-subparagraph)
  ((:or doc-data office-doc-data) tmof-doc-data)
  ((:or abstract abstract-data) tmof-abstract)
  (render-table-of-contents tmof-toc)
  (table-of-contents tmof-table-of-contents)

  ;; lists and quotations
  ((:or itemize itemize-minus itemize-dot itemize-arrow) tmof-itemize)
  ((:or description description-compact description-dash description-aligned
        description-long description-paragraphs)
   tmof-description)
  ((:or enumerate enumerate-numeric) tmof-enumerate)
  (enumerate-alpha tmof-enumerate-alpha)
  (enumerate-Alpha tmof-enumerate-Alpha)
  (enumerate-roman tmof-enumerate-roman)
  (enumerate-Roman tmof-enumerate-Roman)
  ((:or item item*) tmof-noop)
  ((:or quotation quote-env verse) tmof-quotation)

  ;; environments
  (render-theorem tmof-render-theorem)
  (render-remark tmof-render-remark)
  ((:or render-exercise render-solution) tmof-render-exercise)
  (render-proof tmof-render-proof)
  (render-enunciation tmof-render-enunciation)
  ((:or render-big-figure render-small-figure render-big-algorithm
        render-small-algorithm)
   tmof-render-figure)
  ((:or big-figure small-figure) tmof-big-figure)
  ((:or big-table small-table) tmof-big-table)
  (render-bibitem tmof-bibitem)
  (label tmof-label)
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
            ((em strong sub sup smallcaps code)
             (with l (tmof-final-inlines (tmof-node-children x))
               (if (null? l) '() (list (cons (car x) l)))))
            ((color span ins del field size font mark underline strike caps letters)
             (with l (tmof-final-inlines (tmof-node-children x))
               (cond ((and (null? l) (not (func? x 'field))) '())
                     ((null? (tmof-node-attrs x)) (list (cons (car x) l)))
                     (else (list `(,(car x) (@ ,@(tmof-node-attrs x)) ,@l))))))
            ((comment)
             (list `(comment (@ ,@(tmof-node-attrs x))
                             ,@(tmof-final-blocks (tmof-node-children x)))))
            ((a)
             (let ((l (tmof-final-inlines (tmof-node-children x)))
                   (href (with a (assoc 'href (tmof-node-attrs x))
                           (if a (cadr a) ""))))
               (cond ((null? l) '())
                     ((== href "") l)
                     (else (list `(link (@ (href ,href)) ,@l))))))
            ((note) (list (cons 'note (tmof-final-blocks (cdr x)))))
            ((br) '((br)))
            ((tab) '((tab)))
            ((math image bookmark seq) (list x))
            ((ref)
             (with l (tmof-final-inlines (tmof-node-children x))
               (if (null? l) '() (list `(ref (@ ,@(tmof-node-attrs x)) ,@l)))))
            ((html footnote img !number) '())
            ;; blocks inside a text: their text
            ((p !role !h !para)
             (tmof-final-inlines
               (if (func? x 'p) (cdr x) (cddr x))))
            (else '())))))

(define (tmof-final-inlines l)
  (office-merge (append-map tmof-final-inline l)))

(define (tmof-final-paragraph attrs l)
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
            ((p) (tmof-final-paragraph '() (cdr x)))
            ((!h)
             (if (and (number? (cadr x)) (< (cadr x) 0))
                 (tmof-final-paragraph '((role "title")) (cddr x))
                 (let* ((numbered? (and (pair? (cddr x)) (func? (caddr x) '!number)))
                        (number (and numbered? (cadr (caddr x))))
                        (level (if (number? (cadr x)) (cadr x) tmof-top-level)))
                   (tmof-final-paragraph
                     `((role "heading")
                       (level ,(number->string
                                 (max 1 (min 9 (+ (- level tmof-top-level) 1)))))
                       (number ,number))
                     (cddr x)))))
            ((!role) (tmof-final-paragraph `((role ,(cadr x))) (cddr x)))
            ;; a paragraph with its attributes
            ((!para) (tmof-final-paragraph (cadr x) (cddr x)))
            ((!display)
             (list `(p (math (@ (display "true") (form "sxml")) ,(cadr x))
                       ,@(if (pair? (cddr x)) `((tab) ,@(cddr x)) '()))))
            ((columns)
             (list `(columns (@ ,@(tmof-node-attrs x))
                             ,@(tmof-final-blocks (tmof-node-children x)))))
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
                   (list `(list (@ (kind ,(if (func? x 'ol) "number" "bullet"))
                                   ,@(list-filter (tmof-node-attrs x)
                                                  (lambda (a) (in? (car a) '(format start)))))
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
            ((!toc) (list `(toc ,@(tmof-final-inlines (cdr x)))))
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
  (tmof-anchor-floats (append-map tmof-final-block l)))

;; A picture which floats beside a paragraph was read from inside it: it
;; goes back there, at its start (attribute next of office-float)

(define (tmof-next-float? y)
  (and (func? y 'image) (== (ox-attr y 'next) "true")))

(define (tmof-drop-next y)
  (if (not (tmof-next-float? y)) y
      (apply office-node
             (cons* 'image (list-filter (map (lambda (a) (list (car a) (cadr a)))
                                             (ox-attrs y))
                                        (lambda (a) (!= (car a) 'next)))
                    (ox-children y)))))

(define (tmof-blank? y)
  (and (string? y) (== (string-trim-spaces y) "")))

(define (tmof-anchor-floats l)
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((and (func? (car l) 'p)
                (with c (list-filter (ox-children (car l)) (lambda (y) (not (tmof-blank? y))))
                  (and (pair? c) (list-and (map tmof-next-float? c))))
                (pair? (cdr l)) (func? (cadr l) 'p))
           (let* ((imgs (map tmof-drop-next
                             (list-filter (ox-children (car l)) tmof-next-float?)))
                  (next (cadr l))
                  (merged (apply office-node
                                 (cons* 'p (map (lambda (a) (list (car a) (cadr a)))
                                                (ox-attrs next))
                                        (append imgs (ox-children next))))))
             (loop (cons merged (cddr l)) acc)))
          ((func? (car l) 'p)
           (loop (cdr l)
                 (cons (if (list-or (map tmof-next-float? (ox-children (car l))))
                           (apply office-node
                                  (cons* 'p (map (lambda (a) (list (car a) (cadr a)))
                                                 (ox-attrs (car l)))
                                         (map tmof-drop-next (ox-children (car l)))))
                           (car l))
                       acc)))
          (else (loop (cdr l) (cons (car l) acc))))))

;; A label of TeXmacs is the label of the last thing which has a number
;; before it: a heading, a theorem, a figure, an equation. In an office
;; file a reference shows the number of what it refers to when its target
;; is this number. So the bookmarks of the labels are given to the numbers
;; (the attribute labels of a node seq, or of a heading), and the
;; references say what they refer to (kind and target). A link to a label
;; with a text of its own, which is not the number, is no such reference:
;; it keeps its text, and is a link to the bookmark.

(define tmof-bound (make-ahash-table))    ; label -> (kind id number)
(define tmof-targets (make-ahash-table))  ; id -> labels, reversed
(define tmof-target #f)                   ; the last thing with a number
(define tmof-heading-nr 0)
(define tmof-inside '())                  ; the numbered things which are around
(define tmof-only-labels? #f)             ; a paragraph of labels only

;; A label in the text, away from what it is the label of, goes to the
;; number (a reference shows it), and a bookmark of its own (attribute at)
;; stays where it was: the label comes back there when the file is read.

(define (tmof-bind-scan x)
  ;; first pass: what each label is the label of
  (cond ((not (pair? x)) (noop))
        ((in? (car x) '(math image @)) (noop))
        ((func? x 'seq)
         (set! tmof-target (list "seq" (ox-attr x 'id) (ox-text x))))
        ((func? x 'bookmark)
         (when tmof-target
           (ahash-set! tmof-bound (ox-attr x 'name) tmof-target)
           (ahash-set! tmof-targets (cadr tmof-target)
                       (cons (ox-attr x 'name)
                             (or (ahash-ref tmof-targets (cadr tmof-target)) '())))))
        (else
          (when (and (func? x 'p) (== (ox-attr x 'role) "heading") (ox-attr x 'number))
            (set! tmof-target (list "heading" (ox-attr x 'id) (ox-attr x 'number))))
          (for-each tmof-bind-scan (ox-children x)))))

(define (tmof-seq-ids x)
  ;; the ids of the sequence numbers in the paragraph x
  (cond ((not (pair? x)) '())
        ((in? (car x) '(math image @ note)) '())
        ((func? x 'seq) (if (ox-attr x 'id) (list (ox-attr x 'id)) '()))
        (else (append-map tmof-seq-ids (ox-children x)))))

(define (tmof-labels-attr id)
  (with l (ahash-ref tmof-targets id)
    (and l (string-recompose (reverse l) " "))))

(define (tmof-bind-node x)
  ;; second pass: the nodes for the node x
  (cond ((not (pair? x)) (list x))
        ((in? (car x) '(math image)) (list x))
        ((func? x 'bookmark)
         (with target (ahash-ref tmof-bound (ox-attr x 'name))
           (cond ((not target) (list x))
                 ((or tmof-only-labels? (in? (cadr target) tmof-inside)) '())
                 (else (list `(bookmark (@ (name ,(ox-attr x 'name)) (at "true"))))))))
        ((func? x 'seq)
         (list (apply office-node
                      (cons* 'seq `((name ,(ox-attr x 'name)) (id ,(ox-attr x 'id))
                                    (labels ,(tmof-labels-attr (ox-attr x 'id))))
                             (ox-children x)))))
        ((func? x 'ref)
         (let* ((target (ahash-ref tmof-bound (ox-attr x 'name)))
                ;; (the editor writes ? for a number which it does not know)
                (text (string-trim-spaces (ox-text x)))
                (target (and target (in? text (list (caddr target) "?")) target)))
           (list (apply office-node
                        (cons* 'ref `((name ,(ox-attr x 'name))
                                      (kind ,(and target (car target)))
                                      (target ,(and target (cadr target))))
                               (append-map tmof-bind-node (ox-children x)))))))
        ((and (func? x 'p) (not (ox-attr x 'inside-done)))
         ;; (in a heading or a paragraph with numbers, and in a paragraph
         ;; of labels only, the labels are at their place)
         (let ((old tmof-inside)
               (old-only tmof-only-labels?))
           (set! tmof-inside (append (if (and (== (ox-attr x 'role) "heading") (ox-attr x 'id))
                                         (list (ox-attr x 'id)) '())
                                     (tmof-seq-ids x)
                                     tmof-inside))
           (set! tmof-only-labels?
                 (list-and (map (lambda (y) (or (func? y 'bookmark)
                                                (and (string? y) (== (string-trim-spaces y) ""))))
                                (ox-children x))))
           (with r (tmof-bind-node (cons* 'p (cons '@ (cons '(inside-done "true") (ox-attrs x)))
                                          (ox-children x)))
             (set! tmof-inside old)
             (set! tmof-only-labels? old-only)
             (map (lambda (y)
                    (if (func? y 'p)
                        (apply office-node
                               (cons* 'p (list-filter (map (lambda (a) (list (car a) (cadr a)))
                                                           (ox-attrs y))
                                                      (lambda (a) (!= (car a) 'inside-done)))
                                      (ox-children y)))
                        y))
                  r))))
        (else
          (let* ((children (append-map tmof-bind-node (ox-children x)))
                 ;; (the texts around a label which went away are one)
                 (children (if (in? (car x) '(list item table row cell note))
                               children
                               (office-merge children)))
                 (attrs (ox-attrs x))
                 (attrs (if (and (func? x 'p) (== (ox-attr x 'role) "heading")
                                 (ox-attr x 'id))
                            (append attrs
                                    (with l (tmof-labels-attr (ox-attr x 'id))
                                      (if l `((labels ,l)) '())))
                            attrs)))
            ;; a paragraph which only had a label is no paragraph
            (if (and (func? x 'p) (null? children) (not (ox-attr x 'role))
                     (pair? (ox-children x)))
                '()
                (list (if (null? attrs) (cons (car x) children)
                          (cons* (car x) (cons '@ attrs) children))))))))

(define (tmof-bind l)
  ;; the blocks l with their labels bound
  (set! tmof-bound (make-ahash-table))
  (set! tmof-targets (make-ahash-table))
  (set! tmof-target #f)
  (set! tmof-heading-nr 0)
  ;; the headings with a number get a name
  (let* ((named (map (lambda (b)
                       (if (and (func? b 'p) (== (ox-attr b 'role) "heading")
                                (ox-attr b 'number))
                           (begin
                             (set! tmof-heading-nr (+ tmof-heading-nr 1))
                             (cons* 'p (cons '@ (append (ox-attrs b)
                                                        `((id ,(string-append
                                                                 "heading"
                                                                 (number->string tmof-heading-nr))))))
                                    (ox-children b)))
                           b))
                     l)))
    (for-each tmof-bind-scan named)
    (with r (append-map tmof-bind-node named)
      (set! tmof-bound (make-ahash-table))
      (set! tmof-targets (make-ahash-table))
      r)))

(define (tmof-finalize l)
  ;; the blocks of the office tree: the first level of headings which is
  ;; used is the level 1
  ;; (the parts are headings of the first level too, without number)
  (let* ((levels (list-filter (append-map tmof-heading-levels l)
                              (lambda (n) (and (number? n) (>= n 0)))))
         (levels (if (list-or (map positive? levels))
                     (list-filter levels positive?)
                     levels)))
    (set! tmof-top-level (if (null? levels) 0 (apply min levels)))
    (tmof-single-breaks (tmof-bind (tmof-final-blocks l)))))

(define (tmof-single-breaks l)
  ;; no break of page at the start, and none after another one
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((and (func? (car l) 'pagebreak)
                (or (null? acc) (func? (car acc) 'pagebreak)))
           (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

(define tmof-page-sizes
  ;; the sizes of the usual pages, in centimeters
  '(("a3" 29.7 42.0) ("a4" 21.0 29.7) ("a5" 14.8 21.0) ("b5" 17.6 25.0)
    ("letter" 21.59 27.94) ("legal" 21.59 35.56) ("executive" 18.42 26.67)))

(define (tmof-setup init)
  ;; the block setup for the initial environment of the document: its
  ;; page, the size and the family of its font
  (let* ((get (lambda (var)
                (with v (assoc-ref init var)
                  (and (string? v) (!= v "") (!= v "auto") v))))
         (cm (lambda (var) (with v (get var) (and v (office-length->cm v)))))
         (len (lambda (x) (string-append (office-decimal x) "cm")))
         (type (or (get "page-type") "a4"))
         (size (assoc-ref tmof-page-sizes type))
         (w (if (== type "user") (cm "page-width") (and size (car size))))
         (h (if (== type "user") (cm "page-height") (and size (cadr size))))
         (landscape? (== (get "page-orientation") "landscape"))
         (w* (if (and w h landscape?) (max w h) w))
         (h* (if (and w h landscape?) (min w h) h))
         (family (get "font-family"))
         (base (and (get "font-base-size") (office-number (get "font-base-size"))))
         (attrs `((page-width ,(and w* h* (len w*)))
                  (page-height ,(and w* h* (len h*)))
                  (landscape ,(and landscape? "true"))
                  (margin-left ,(and (cm "page-odd") (len (cm "page-odd"))))
                  (margin-right ,(and (cm "page-right") (len (cm "page-right"))))
                  (margin-top ,(and (cm "page-top") (len (cm "page-top"))))
                  (margin-bottom ,(and (cm "page-bot") (len (cm "page-bot"))))
                  (font-size ,(and base (office-decimal base)))
                  (font ,(cond ((== family "ss") "sans")
                               ((== family "tt") "mono")
                               (else #f)))
                  (body-style ,(get "office-body-style"))
                  ;; (the paragraphs of TeXmacs are justified)
                  (align ,(with m (or (get "par-mode") "justify")
                            (if (in? m '("left" "center" "right")) m "justify"))))))
    (if (list-or (map cadr attrs)) (list (office-node 'setup attrs)) '())))

(define (tmof-package init)
  ;; the file which the document was read from, in the style office
  (with p (assoc-ref init "office-package")
    (if (and (func? p 'tuple 2) (string? (cadr p)) (func? (caddr p) 'raw-data 1))
        `((package (@ (format ,(cadr p)) (data ,(cadr (caddr p))))))
        '())))

(define (tmof-initial x)
  ;; the initial environment of the document x, as an association list
  (with init (and tmof-document? (tmfile-extract x 'initial))
    (if (func? init 'collection)
        (append-map (lambda (a)
                      (if (and (func? a 'associate 2) (string? (cadr a)))
                          (list (cons (cadr a) (caddr a)))
                          '()))
                    (cdr init))
        '())))

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
  (set! tmof-keep-spaces?
        (and tmof-document? (pair? (tmof-package (tmof-initial x)))))
  (set! tmof-base-size
        (or (and tmof-document?
                 (office-number (or (assoc-ref (tmof-initial x) "font-base-size") "")))
            10.0))
  (with body (if tmof-document? (tmfile-extract x 'body) x)
    `(office ,@(if tmof-document? (tmof-setup (tmof-initial x)) '())
             ,@(if tmof-document? (tmof-package (tmof-initial x)) '())
             ,@(tmof-finalize (tmof-blocks (tmof body))))))
