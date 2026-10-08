
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmarkdown.scm
;; DESCRIPTION : conversion of TeXmacs trees into Markdown trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Markdown trees are described in markdownin.scm. A document which is
;; exported was expanded by the typesetter, except for the macros of
;; tmmarkdown-expand.scm; a selection is converted as it is. Both forms of
;; the markup are understood here: (section "Title") and its expansion
;; (section-title "1  Title"), (theorem body) and (render-theorem "Theorem 1"
;; body), and so on.
;;
;; Each handler takes the arguments of a tag and returns a list of nodes of
;; a Markdown tree, text or blocks; the text which is put together by a
;; document becomes paragraphs. The mathematics is converted to LaTeX by the
;; converter for LaTeX.

(texmacs-module (convert markdown tmmarkdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmmd-html? #t)          ; HTML for what Markdown cannot express
(define tmmd-document? #f)      ; a whole document, and not a piece of one
(define tmmd-image-nr 0)        ; the images which were saved as files
(define tmmd-front-matter? #t)  ; the title in a YAML header
(define tmmd-footnotes '())     ; the definitions of the footnotes, reversed
(define tmmd-footnote-nr 0)

(define (tmmd-initialize opts)
  (set! tmmd-html? (!= (assoc-ref opts "texmacs->markdown:html") "off"))
  (set! tmmd-front-matter?
        (!= (assoc-ref opts "texmacs->markdown:front-matter") "off"))
  (set! tmmd-footnotes '())
  (set! tmmd-footnote-nr 0)
  (set! tmmd-image-nr 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes: text and blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-keep s l)
  ;; the text in UTF-8, but for the sequences of the list, which the
  ;; conversion would change: ... into an ellipsis, ` into a quote
  (if (null? l) (cork->utf8 s)
      (let* ((what (car l))
             (i (string-search-forwards what 0 s)))
        (if (< i 0) (tmmd-keep s (cdr l))
            (string-append
              (tmmd-keep (substring s 0 i) (cdr l)) what
              (tmmd-keep (substring s (+ i (string-length what))
                                    (string-length s)) l))))))

(define (tmmd-text s) (tmmd-keep s '("...")))
(define (tmmd-code-text s) (tmmd-keep s '("..." "`")))

(define (tmmd-block? x)
  (and (pair? x)
       (in? (car x) '(meta h1 h2 h3 h4 h5 h6 !h p blockquote ul ol pre hr
                      table footnote-def !display))))

(define (tmmd-blank? x)
  (and (string? x) (== (string-trim-spaces x) "")))

(define (tmmd-merge l)
  ;; with the strings which follow each other as one, and so for the
  ;; emphasized texts: *a**b* would not be read as *ab*
  (cond ((null? l) l)
        ((== (car l) "") (tmmd-merge (cdr l)))
        ((and (pair? (car l)) (in? (caar l) '(em strong del))
              (nnull? (cdr l)) (func? (cadr l) (caar l)))
         (tmmd-merge (cons `(,(caar l) ,@(tmmd-merge (append (cdar l) (cdadr l))))
                           (cddr l))))
        ((and (string? (car l)) (nnull? (cdr l)) (string? (cadr l)))
         (tmmd-merge (cons (string-append (car l) (cadr l)) (cddr l))))
        (else (cons (car l) (tmmd-merge (cdr l))))))

(define (tmmd-squeeze s)
  ;; with one space for several
  (let loop ((l (string->list s)) (acc '()) (sp? #f))
    (cond ((null? l) (list->string (reverse acc)))
          ((char=? (car l) #\space)
           (loop (cdr l) (if sp? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

(define (tmmd-trim l)
  ;; text without spaces and line breaks at its ends
  (let* ((l (map (lambda (x) (if (string? x) (tmmd-squeeze x) x)) (tmmd-merge l)))
         (drop-start
           (lambda (l)
             (let loop ((l l))
               (cond ((null? l) l)
                     ((or (tmmd-blank? (car l)) (func? (car l) 'br)) (loop (cdr l)))
                     ((string? (car l))
                      (cons (string-trim-spaces-left (car l)) (cdr l)))
                     (else l)))))
         (l (drop-start l))
         (r (reverse l))
         (r (let loop ((r r))
              (cond ((null? r) r)
                    ((or (tmmd-blank? (car r)) (func? (car r) 'br)) (loop (cdr r)))
                    ((string? (car r))
                     (cons (string-trim-spaces-right (car r)) (cdr r)))
                    (else r)))))
    (reverse r)))

(define (tmmd-blocks l)
  ;; the nodes as blocks: the text between two blocks is a paragraph
  (let loop ((l l) (run '()) (acc '()))
    (define (flush)
      (with p (tmmd-trim (reverse run))
        (if (null? p) acc (cons `(p ,@p) acc))))
    (cond ((null? l) (reverse (flush)))
          ((tmmd-block? (car l)) (loop (cdr l) '() (cons (car l) (flush))))
          (else (loop (cdr l) (cons (car l) run) acc)))))

(define (tmmd-inline l)
  ;; the nodes as text: the paragraphs of blocks follow each other
  (append-map
    (lambda (x)
      (cond ((func? x 'p) (append (cdr x) (list " ")))
            ((func? x '!display) (list `(displaymath ,(cadr x))))
            ((tmmd-block? x) '())
            (else (list x))))
    l))

(define (tmmd-has-block? l)
  (list-or (map tmmd-block? l)))

(define (tmmd-wrap tag l)
  ;; the markup tag around text; the paragraphs of blocks get it each
  (cond ((tmmd-has-block? l)
         (map (lambda (b)
                (if (func? b 'p)
                    `(p (,tag ,@(cdr b)))
                    b))
              (tmmd-blocks l)))
        ((null? (tmmd-trim l)) l)
        (else `((,tag ,@(tmmd-merge l))))))

(define (tmmd-html-wrap name l)
  ;; HTML around text, for what Markdown has no markup
  (if (or (not tmmd-html?) (tmmd-has-block? l) (null? (tmmd-trim l))) l
      `((html ,(string-append "<" name ">")) ,@l
        (html ,(string-append "</" name ">")))))

(define (tmmd-attach before l after)
  ;; text before and after nodes: in their first and last paragraphs
  (if (not (tmmd-has-block? l))
      (append before l after)
      (let* ((b (tmmd-blocks l))
             (b (if (null? (tmmd-trim before)) b
                    (if (func? (car b) 'p)
                        (cons `(p ,@before ,@(cdar b)) (cdr b))
                        (cons `(p ,@before) b))))
             (r (reverse b))
             (r (if (null? (tmmd-trim after)) r
                    (if (func? (car r) 'p)
                        (cons `(p ,@(cdar r) ,@after) (cdr r))
                        (cons `(p ,@after) r)))))
        (reverse r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain text and mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-plain x)
  ;; the text of a tree, as for code: the paragraphs are lines
  (cond ((string? x) (tmmd-code-text x))
        ((not (pair? x)) "")
        ((func? x 'document)
         (let loop ((l (cdr x)) (acc '()))
           (cond ((null? l) (apply string-append (reverse acc)))
                 ((null? (cdr l)) (loop (cdr l) (cons (tmmd-plain (car l)) acc)))
                 (else (loop (cdr l) (cons "\n" (cons (tmmd-plain (car l)) acc)))))))
        ((in? (car x) '(next-line new-line)) "\n")
        ((in? (car x) '(label assign hidden-binding set-binding write)) "")
        ((func? x 'with) (tmmd-plain (cAr x)))
        ((func? x 'hlink 2) (tmmd-plain (cadr x)))
        (else (apply string-append (map tmmd-plain (cdr x))))))

(define (tmmd-math-clean x)
  ;; a formula without what LaTeX would write and Markdown does not need
  (cond ((not (pair? x)) x)
        ((in? (car x) '(label assign hidden-binding set-binding write)) "")
        (else (cons (car x) (map tmmd-math-clean (cdr x))))))

(define (tmmd-strip s before after)
  ;; s without before at its start and after at its end, or #f
  (let ((n (string-length s)))
    (and (string-starts? s before) (string-ends? s after)
         (>= n (+ (string-length before) (string-length after)))
         (substring s (string-length before) (- n (string-length after))))))

(define (tmmd-trim-lines s)
  ;; without spaces and empty lines at its ends
  (let* ((l (string->list s))
         (ws? (lambda (c) (or (char=? c #\space) (char=? c #\newline))))
         (l (let loop ((l l)) (if (and (nnull? l) (ws? (car l))) (loop (cdr l)) l)))
         (r (let loop ((r (reverse l)))
              (if (and (nnull? r) (ws? (car r))) (loop (cdr r)) r))))
    (list->string (reverse r))))

(define (tmmd-latex x)
  (catch #t
    (lambda ()
      (serialize-latex
        (texmacs->latex x (list (cons "texmacs->latex:mathjax" "on")))))
    (lambda args #f)))

(define (tmmd-inline-latex x)
  ;; the LaTeX of a formula in the text
  (let* ((s (or (tmmd-latex `(math ,(tmmd-math-clean x))) ""))
         (s (tmmd-trim-lines s)))
    (tmmd-trim-lines
      (or (tmmd-strip s "$" "$") (tmmd-strip s "\\(" "\\)") s))))

(define (tmmd-display-latex x)
  ;; the LaTeX of a formula on its own lines; an environment with several
  ;; lines, as eqnarray*, is kept
  (let* ((x (tmmd-math-clean x))
         (env? (and (pair? x)
                    (in? (car x) '(eqnarray eqnarray* align align* gather
                                   gather* multline multline* eqsplit
                                   eqsplit*))))
         (s (tmmd-trim-lines
              (or (tmmd-latex (if env? x `(equation* ,x))) ""))))
    (tmmd-trim-lines
      (or (tmmd-strip s "\\[" "\\]")
          (tmmd-strip s "$$" "$$")
          (tmmd-strip s "\\begin{equation*}" "\\end{equation*}")
          (tmmd-strip s "\\begin{equation}" "\\end{equation}")
          s))))

(define (tmmd-symbol x)
  ;; the character of a formula which is one symbol, as an arrow, or #f
  (and (string? x) (> (string-length x) 2)
       (char=? (string-ref x 0) #\<)
       (== (string-search-forwards ">" 0 x) (- (string-length x) 1))
       (with s (tmmd-text x)
         (and (not (string-starts? s "<")) s))))

(define (tmmd-math l)
  (let ((x (if (list-1? l) (car l) `(concat ,@l))))
    (if (tmmd-symbol x)
        (list (tmmd-symbol x))
        (with s (tmmd-inline-latex x)
          (if (== s "") '() `((math ,s)))))))

(define (tmmd-display x . tag)
  (let* ((s (tmmd-display-latex x))
         (s (if (and (nnull? tag) (!= (car tag) ""))
                (string-append s " \\tag{" (car tag) "}")
                s)))
    (if (== s "") '() `((!display ,s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-noop l) '())
(define (tmmd-space l) '(" "))
(define (tmmd-first l) (if (null? l) '() (tmmd (car l))))
(define (tmmd-last l) (if (null? l) '() (tmmd (cAr l))))
(define (tmmd-all l) (append-map tmmd l))

(define (tmmd-em l) (tmmd-wrap 'em (tmmd-all l)))
(define (tmmd-strong l) (tmmd-wrap 'strong (tmmd-all l)))
(define (tmmd-del l) (tmmd-wrap 'del (tmmd-all l)))
;; Markdown has no underlining: in a plain text it is emphasis
(define (tmmd-underline l) (tmmd-em l))
(define (tmmd-marked l) (tmmd-html-wrap "mark" (tmmd-all l)))
;; The digits and the signs which Unicode has as subscripts and as
;; superscripts, with their codes: H2O and x2 are written with them, and need
;; no markup. (It has a few letters too, which many fonts lack.)
(define tmmd-subscripts
  '((#\0 . "2080") (#\1 . "2081") (#\2 . "2082") (#\3 . "2083")
    (#\4 . "2084") (#\5 . "2085") (#\6 . "2086") (#\7 . "2087")
    (#\8 . "2088") (#\9 . "2089") (#\+ . "208A") (#\- . "208B")
    (#\= . "208C") (#\( . "208D") (#\) . "208E")))

(define tmmd-superscripts
  '((#\0 . "2070") (#\1 . "00B9") (#\2 . "00B2") (#\3 . "00B3")
    (#\4 . "2074") (#\5 . "2075") (#\6 . "2076") (#\7 . "2077")
    (#\8 . "2078") (#\9 . "2079") (#\+ . "207A") (#\- . "207B")
    (#\= . "207C") (#\( . "207D") (#\) . "207E")))

(define (tmmd-script l table name)
  ;; a subscript or a superscript: its characters when they are all in the
  ;; table, and else the tag name of HTML
  (let* ((s (and (list-1? l) (string? (car l)) (car l)))
         (codes (and s (!= s "")
                     (map (lambda (c) (assoc-ref table c)) (string->list s)))))
    (if (and codes (list-and codes))
        (list (apply string-append
                     (map (lambda (code)
                            (cork->utf8 (string-append "<#" code ">")))
                          codes)))
        (tmmd-html-wrap name (tmmd-all l)))))

(define (tmmd-sub l) (tmmd-script l tmmd-subscripts "sub"))
(define (tmmd-sup l) (tmmd-script l tmmd-superscripts "sup"))

(define (tmmd-code l)
  ;; code in the text, or lines of code
  (if (and (list-1? l) (func? (car l) 'document) (> (length (car l)) 2))
      `((pre ,(tmmd-plain (car l))))
      (with s (apply string-append (map tmmd-plain l))
        (if (== s "") '() `((code ,s))))))

(define (tmmd-key l)
  ;; a key of the keyboard, as code
  (with s (apply string-append (map tmmd-plain l))
    (if (== s "") '() `((code ,s)))))

(define (tmmd-next-line l) '((br)))

(define (tmmd-hrule l) '((hr)))

(define (tmmd-name s) (lambda (l) (list s)))
(define (tmmd-nbsp l) (list (tmmd-text "<varspace>")))
(define tmmd-TeXmacs (tmmd-name "TeXmacs"))
(define tmmd-TeX (tmmd-name "TeX"))
(define tmmd-LaTeX (tmmd-name "LaTeX"))

(define (tmmd-hlink l)
  ;; a link; a link inside the document is its text
  (if (< (length l) 2) (tmmd-all l)
      (let ((body (tmmd (car l)))
            (url (tmmd-plain (cadr l))))
        (if (or (== url "") (string-starts? url "#") (tmmd-has-block? body))
            body
            `((a (@ (href ,url)) ,@(tmmd-merge body)))))))

(define (tmmd-hlink* l)
  ;; a link with a title
  (let ((r (tmmd-hlink l))
        (title (if (< (length l) 3) "" (tmmd-plain (caddr l)))))
    (if (and (list-1? r) (func? (car r) 'a) (!= title ""))
        `((a (@ ,@(cdadar r) (title ,title)) ,@(cddar r)))
        r)))

(define (tmmd-alt-text l)
  ;; an image with a text in its place; anything else is itself
  (if (< (length l) 2) (tmmd-all l)
      (let ((r (tmmd (car l)))
            (alt (tmmd-plain (cadr l))))
        (if (and (list-1? r) (func? (car r) 'img))
            `((img (@ ,@(list-filter (cdadar r) (lambda (a) (!= (car a) 'alt)))
                      (alt ,alt))))
            r))))

(define (tmmd-size x)
  ;; the width or the height of an image as HTML has them: pixels, or a
  ;; percentage for a part of the paragraph; #f for the other lengths
  (and tmmd-html? (string? x) (!= x "")
       (let* ((n (string-length x))
              (i (let loop ((i 0))
                   (if (and (< i n)
                            (or (char-numeric? (string-ref x i))
                                (in? (string-ref x i) '(#\. #\-))))
                       (loop (+ i 1)) i)))
              (v (string->number (substring x 0 i)))
              (unit (substring x i n))
              (px (assoc-ref '(("px" . 1) ("" . 1) ("pt" . 1.3333) ("in" . 96)
                               ("cm" . 37.795) ("mm" . 3.7795))
                             unit)))
         (cond ((or (not v) (<= v 0)) #f)
               ((== unit "par")
                (string-append (number->string (inexact->exact (round (* v 100))))
                               "%"))
               (px (number->string (inexact->exact (round (* v px)))))
               (else #f)))))

(define (tmmd-image-node src l)
  ;; the image src, with the sizes of the arguments l of the tag
  (let ((w (and (>= (length l) 2) (tmmd-size (cadr l))))
        (h (and (>= (length l) 3) (tmmd-size (caddr l)))))
    `((img (@ (src ,src) (alt "")
              ,@(if w `((width ,w)) '())
              ,@(if h `((height ,h)) '()))))))

(define (tmmd-href l)
  (with url (tmmd-plain (if (null? l) "" (car l)))
    (if (== url "") '() `((a (@ (href ,url)) ,url)))))

(define (tmmd-image-root)
  ;; where the images of the document are saved: beside the Markdown file
  ;; which is written, with its name; #f for a piece of a document, or when
  ;; no such file is written
  (and tmmd-document? (url? current-save-target)
       (in? (url-suffix current-save-target) '("md" "markdown" "mkd"))
       (url-unglue current-save-target
                   (+ (string-length (url-suffix current-save-target)) 1))))

(define (tmmd-image l)
  ;; an image which is a file; one which is in the document is saved as a
  ;; file when the document is, as for HTML, and is left out otherwise
  (cond ((null? l) '())
        ((and (string? (car l)) (!= (car l) ""))
         (tmmd-image-node (tmmd-text (car l)) l))
        ((and (func? (car l) 'tuple 2) (func? (cadar l) 'raw-data 1)
              (string? (cadr (cadar l))) (string? (caddar l))
              (tmmd-image-root))
         (let* ((root (tmmd-image-root))
                (suffix (url-suffix (cork->utf8 (caddar l))))
                (nr (begin (set! tmmd-image-nr (+ tmmd-image-nr 1))
                           tmmd-image-nr))
                (post (string-append "-" (number->string nr)
                                     (if (== suffix "") "" ".") suffix)))
           (string-save (cadr (cadar l)) (url-glue root post))
           (tmmd-image-node (string-append (url->unix (url-tail root)) post)
                            l)))
        (else '())))

(define (tmmd-specific l)
  ;; what is for Markdown or for HTML only
  (cond ((< (length l) 2) '())
        ((== (car l) "markdown") `((html ,(tmmd-plain (cadr l)))))
        ((and (== (car l) "html") tmmd-html?) `((html ,(tmmd-plain (cadr l)))))
        (else '())))

(define (tmmd-reference l)
  ;; a reference which was not expanded: its label
  (list (tmmd-plain (if (null? l) "" (car l)))))

(define (tmmd-eqref l)
  (list (string-append "(" (tmmd-plain (if (null? l) "" (car l))) ")")))

(define (tmmd-cite l)
  (list (string-append
          "["
          (apply string-append
                 (list-intersperse (map tmmd-plain l) ", "))
          "]")))

(define (tmmd-footnote-add body)
  (set! tmmd-footnote-nr (+ tmmd-footnote-nr 1))
  (with label (number->string tmmd-footnote-nr)
    (set! tmmd-footnotes
          (cons `(footnote-def ,label ,@(tmmd-blocks (tmmd body)))
                tmmd-footnotes))
    `((footnote ,label))))

(define (tmmd-footnote l)
  (if (null? l) '() (tmmd-footnote-add (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables of the environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-with-one var val body)
  ;; the body, already converted, in the environment where var is val
  (cond ((and (== var "font-series") (== val "bold")) (tmmd-wrap 'strong body))
        ((and (== var "font-shape") (== val "italic")) (tmmd-wrap 'em body))
        ((and (== var "font-shape") (== val "slanted")) (tmmd-wrap 'em body))
        (else body)))

(define (tmmd-with l)
  (cond ((null? l) '())
        ((null? (cdr l)) (tmmd (car l)))
        ;; mathematics: the body as it is, with the other variables
        ((let loop ((l l))
           (cond ((or (null? l) (null? (cdr l))) #f)
                 ((and (== (car l) "mode") (== (cadr l) "math")) #t)
                 (else (loop (cddr l)))))
         (tmmd-math (list (cAr l))))
        ((let loop ((l l))
           (cond ((or (null? l) (null? (cdr l))) #f)
                 ((and (== (car l) "font-family") (== (cadr l) "tt")) #t)
                 (else (loop (cddr l)))))
         (tmmd-code (list (cAr l))))
        (else
          (let loop ((l l) (body (tmmd (cAr l))))
            (if (or (null? l) (null? (cdr l))) body
                (loop (cddr l)
                      (if (and (string? (car l)) (string? (cadr l)))
                          (tmmd-with-one (car l) (cadr l) body)
                          body)))))))

(define (tmmd-surround l)
  (if (!= (length l) 3) (tmmd-all l)
      (tmmd-attach (tmmd-inline (tmmd (car l)))
                   (tmmd (caddr l))
                   (tmmd-inline (tmmd (cadr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraphs, sections and the title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-document l)
  ;; each paragraph by itself
  (append-map (lambda (x) (tmmd-blocks (tmmd x))) l))

(define (tmmd-concat l)
  (tmmd-all l))

;; A heading is (!h level text...) until the levels which the document uses
;; are known: 0 for a part, 1 for a chapter, 2 for a section... A formula on
;; its own lines is (!display "latex") until then. The title of a section
;; may be laid out as a table, its number in a cell and its text in another:
;; inside a heading the cells of a table are text.

(define tmmd-flat? #f)

(define (tmmd-heading level l)
  (with old tmmd-flat?
    (set! tmmd-flat? #t)
    (with t (tmmd-trim (tmmd-inline (tmmd-all l)))
      (set! tmmd-flat? old)
      (if (null? t) '() `((!h ,level ,@t))))))

(define (tmmd-part l) (tmmd-heading 0 l))
(define (tmmd-chapter l) (tmmd-heading 1 l))
(define (tmmd-section l) (tmmd-heading 2 l))
(define (tmmd-subsection l) (tmmd-heading 3 l))
(define (tmmd-subsubsection l) (tmmd-heading 4 l))
(define (tmmd-paragraph l) (tmmd-heading 5 l))
(define (tmmd-subparagraph l) (tmmd-heading 6 l))

(define (tmmd-doc-field l tag)
  ;; the values of the fields tag of the title
  (append-map
    (lambda (x)
      (cond ((func? x tag) (list (string-trim-spaces (tmmd-plain `(concat ,@(cdr x))))))
            ((and (pair? x) (in? (car x) '(doc-author author-data)))
             (tmmd-doc-field (cdr x) tag))
            (else '())))
    l))

(define (tmmd-doc-data l)
  ;; the title, the authors and the date: a YAML header, or a heading and a
  ;; paragraph
  (let* ((titles (tmmd-doc-field l 'doc-title))
         (subtitles (tmmd-doc-field l 'doc-subtitle))
         (authors (tmmd-doc-field l 'author-name))
         (dates (tmmd-doc-field l 'doc-date))
         (some (lambda (l) (list-filter l (lambda (s) (!= s ""))))))
    ;; a title alone is better as a heading
    (if (and tmmd-front-matter?
             (nnull? (append (some subtitles) (some authors) (some dates))))
        (with fields (append (map (lambda (s) `(title ,s)) (some titles))
                             (map (lambda (s) `(subtitle ,s)) (some subtitles))
                             (map (lambda (s) `(author ,s)) (some authors))
                             (map (lambda (s) `(date ,s)) (some dates)))
          (if (null? fields) '() `((meta ,@fields))))
        (append
          (if (null? (some titles)) '()
              ;; the heading keeps the markup of the title
              (with t (list-find l (lambda (x) (func? x 'doc-title)))
                `((!h -1 ,@(with-global tmmd-flat? #t
                             (tmmd-trim (tmmd-all (cdr t))))))))
          (if (null? (some authors)) '()
              `((p ,(apply string-append
                           (list-intersperse (some authors) ", ")))))
          (if (null? (some dates)) '() `((p ,(car (some dates)))))))))

(define (tmmd-abstract l)
  (tmmd-attach '((strong "Abstract.") " ") (tmmd-blocks (tmmd-all l)) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists and quotations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-item-start x)
  ;; (item . rest) when the paragraph x starts with an item, else #f
  (cond ((or (func? x 'item) (func? x 'item*)) (cons x '()))
        ((and (func? x 'concat) (pair? (cdr x))
              (or (func? (cadr x) 'item) (func? (cadr x) 'item*)))
         (cons (cadr x) (cddr x)))
        (else #f)))

(define (tmmd-check item)
  ;; "true" or "false" for the box of an item of a task list, else #f
  (and (func? item 'item* 1)
       (let ((s (tmmd-plain (cadr item))))
         (cond ((in? s (list (tmmd-text "<boxtimes>") (tmmd-text "<checkmark>")
                             "[x]" "[X]"))
                "true")
               ((in? s (list (tmmd-text "<Box>") (tmmd-text "<box>")
                             (tmmd-text "<square>") "[ ]"))
                "false")
               (else #f)))))

(define (tmmd-items l)
  ;; the items of a list, from its paragraphs: (item paragraph...) ...
  (let loop ((l l) (cur #f) (acc '()))
    (cond ((null? l) (reverse (if cur (cons (reverse cur) acc) acc)))
          ((tmmd-item-start (car l))
           (with it (tmmd-item-start (car l))
             (loop (cdr l)
                   (list `(concat ,@(cdr it)) (car it))
                   (if cur (cons (reverse cur) acc) acc))))
          (cur (loop (cdr l) (cons (car l) cur) acc))
          ;; text before the first item
          (else (loop (cdr l) (list (car l) '(item)) acc)))))

(define (tmmd-list tag l)
  (let* ((body (if (null? l) '(document) (car l)))
         (pars (if (func? body 'document) (cdr body) (list body)))
         (items (tmmd-items pars)))
    (if (null? items) '()
        `((,tag
           ,@(map (lambda (it)
                    (let* ((item (car it))
                           (check (tmmd-check item))
                           (blocks (append-map (lambda (x) (tmmd-blocks (tmmd x)))
                                               (cdr it)))
                           ;; the name of an item of a description
                           (name (if (and (func? item 'item* 1) (not check))
                                     (tmmd-trim (tmmd-inline (tmmd (cadr item))))
                                     '()))
                           (blocks (if (null? name) blocks
                                       (tmmd-attach `((strong ,@name) " ")
                                                    (if (null? blocks) '((p)) blocks)
                                                    '()))))
                      `(li ,@(if check `((@ (checked ,check))) '()) ,@blocks)))
                  items))))))

(define (tmmd-itemize l) (tmmd-list 'ul l))
(define (tmmd-enumerate l) (tmmd-list 'ol l))

(define (tmmd-quotation l)
  (with b (tmmd-blocks (tmmd-all l))
    (if (null? b) '() `((blockquote ,@b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theorems, figures and code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-titled name body)
  ;; the body after a name in bold, as "Theorem 1."
  (let* ((n (tmmd-trim (tmmd-inline name)))
         (s (if (and (nnull? n) (string? (cAr n))) (cAr n) ""))
         (dot? (and (!= s "") (not (in? (string-ref s (- (string-length s) 1))
                                        '(#\. #\: #\! #\?)))))
         (n* (if (and dot? (nnull? n)) (append n (list ".")) n)))
    (if (null? n) (tmmd-blocks body)
        (tmmd-attach `((strong ,@n*) " ")
                     (with b (tmmd-blocks body) (if (null? b) '((p)) b))
                     '()))))

(define (tmmd-render-enunciation l)
  ;; the name with its number, and the body
  (if (< (length l) 2) (tmmd-all l)
      (tmmd-titled (tmmd (car l)) (tmmd (cadr l)))))

(define tmmd-enunciations
  '(theorem proposition lemma corollary conjecture axiom definition notation
    remark note example convention warning acknowledgments exercise problem
    question solution answer proof algorithm))

(define (tmmd-enunciation tag l)
  ;; a theorem which was not expanded: its name without a number
  (let* ((s (symbol->string tag))
         (s (if (string-ends? s "*") (substring s 0 (- (string-length s) 1)) s)))
    (tmmd-titled (list (upcase-first s)) (tmmd-all l))))

(define (tmmd-render-figure l)
  ;; the type, the name with its number, the figure and its caption
  (if (< (length l) 4) (tmmd-all l)
      (append (tmmd-blocks (tmmd (caddr l)))
              (tmmd-titled (tmmd (cadr l)) (tmmd (cadddr l))))))

(define (tmmd-figure name l)
  ;; a figure which was not expanded: the figure and its caption
  (if (< (length l) 2) (tmmd-all l)
      (append (tmmd-blocks (tmmd (car l)))
              (tmmd-titled (list name) (tmmd (cadr l))))))

(define (tmmd-big-figure l) (tmmd-figure "Figure" l))
(define (tmmd-big-table l) (tmmd-figure "Table" l))

(define tmmd-languages
  '((cpp-code . "cpp") (python-code . "python") (scm-code . "scheme")
    (shell-code . "sh") (java-code . "java") (javascript-code . "javascript")
    (json-code . "json") (julia-code . "julia") (r-code . "r")
    (scala-code . "scala") (fortran-code . "fortran") (octave-code . "octave")
    (scilab-code . "scilab") (dot-code . "dot") (mmx-code . "mathemagix")
    (verbatim-code . "") (pseudo-code . "") (render-code . "") (code . "")))

(define (tmmd-code-block tag l)
  (let* ((lang (assoc-ref tmmd-languages tag))
         (s (apply string-append (map tmmd-plain l))))
    `((pre ,@(if (== lang "") '() `((@ (lang ,lang)))) ,s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-table-rows x)
  ;; the rows of the table inside x, or #f
  (cond ((not (pair? x)) #f)
        ((func? x 'table) (list-filter (cdr x) (lambda (r) (func? r 'row))))
        ((in? (car x) '(tformat tabular tabular* block block* wide-tabular
                        wide-block document))
         (tmmd-table-rows (cAr x)))
        (else #f)))

(define (tmmd-table-formats x)
  ;; the formats (cwith ...) around the table inside x
  (cond ((not (pair? x)) '())
        ((func? x 'tformat)
         (append (list-filter (cDr (cdr x)) (lambda (f) (func? f 'cwith 6)))
                 (tmmd-table-formats (cAr x))))
        ((in? (car x) '(tabular tabular* block block* wide-tabular wide-block
                        document))
         (tmmd-table-formats (cAr x)))
        (else '())))

(define (tmmd-column-align formats j n default)
  ;; the alignment of column j of n, from the formats of whole columns
  (let loop ((l formats) (a default))
    (if (null? l) a
        (let* ((f (cdar l))
               (col (lambda (s) (with k (string->number s)
                                  (and k (if (< k 0) (+ n k 1) k))))))
          (loop (cdr l)
                (if (and (== (list-ref f 4) "cell-halign")
                         (== (list-ref f 0) "1") (== (list-ref f 1) "-1")
                         (col (list-ref f 2)) (col (list-ref f 3))
                         (<= (col (list-ref f 2)) j) (>= (col (list-ref f 3)) j)
                         (string? (list-ref f 5)))
                    (with v (list-ref f 5)
                      (cond ((string-starts? v "c") "center")
                            ((string-starts? v "r") "right")
                            ((string-starts? v "l") "left")
                            (else a)))
                    a))))))

(define (tmmd-cell x)
  ;; the text of a cell; its paragraphs are lines
  (let* ((body (if (func? x 'cell 1) (cadr x) x))
         (l (tmmd body)))
    (tmmd-trim
      (if (tmmd-has-block? l)
          (let loop ((b (tmmd-blocks l)) (acc '()))
            (cond ((null? b) (reverse acc))
                  ((func? (car b) 'p)
                   (loop (cdr b)
                         (append (reverse (cdar b))
                                 (if (null? acc) acc (cons '(br) acc)))))
                  (else (loop (cdr b) acc))))
          l))))

(define (tmmd-table tag l)
  (let* ((x (cons tag l))
         (rows (tmmd-table-rows x)))
    (cond
      ((or (not rows) (null? rows)) (tmmd-all l))
      ;; in a heading: the text of the cells
      (tmmd-flat?
       (append-map (lambda (r)
                     (append-map (lambda (c) (append (tmmd-cell c) (list " ")))
                                 (cdr r)))
                   rows))
      (else
        (let* ((n (apply max (map (lambda (r) (length (cdr r))) rows)))
               (formats (tmmd-table-formats x))
               ;; the cells of block and block* are centered
               (default (if (in? tag '(block* wide-block)) "center" ""))
               (aligns (map (lambda (j) (tmmd-column-align formats j n default))
                            (map (lambda (j) (+ j 1)) (iota n))))
               (unbold (lambda (c)
                         ;; the header of a table is in bold by itself
                         (if (and (list-1? c) (func? (car c) 'strong))
                             (cdar c) c)))
               (row (lambda (r head?)
                      `(tr ,@(map (lambda (c a)
                                    (with t (tmmd-cell c)
                                      `(,(if head? 'th 'td)
                                        ,@(if (== a "") '() `((@ (align ,a))))
                                        ,@(if head? (unbold t) t))))
                                  (append (cdr r)
                                          (make-list (- n (length (cdr r))) '(cell "")))
                                  aligns)))))
          `((table ,(row (car rows) #t)
                   ,@(map (lambda (r) (row r #f)) (cdr rows)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics on its own lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-math-body l)
  ;; the formula of an equation: the paragraph of its document
  (let ((x (if (null? l) "" (car l))))
    (if (func? x 'document 1) (cadr x) x)))

(define (tmmd-equation l) (tmmd-display (tmmd-math-body l)))

(define (tmmd-equation-lab l)
  ;; the formula and its number
  (tmmd-display (tmmd-math-body l)
                (if (< (length l) 2) "" (tmmd-plain (cadr l)))))

(define (tmmd-equations tag l)
  ;; several lines of formulas: the environment of LaTeX
  (tmmd-display (cons tag l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The documentation of TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-tmdoc-title l) (tmmd-heading -1 l))

(define (tmmd-tmdoc-copyright l)
  (if (null? l) '()
      `((p ,(string-append
              "(c) " (tmmd-plain (car l)) " "
              (apply string-append
                     (list-intersperse (map tmmd-plain (cdr l)) ", ")))))))

(define (tmmd-tmdoc-license l)
  (with b (tmmd-blocks (tmmd-all l))
    (if (null? b) '() b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-dispatch htable l)
  (let ((x (logic-ref ,htable (car l))))
    (and x (procedure? x) (x (cdr l)))))

(define (tmmd-compound l)
  ;; (compound "name" arguments...) is (name arguments...)
  (if (and (nnull? l) (string? (car l)))
      (tmmd (cons (string->symbol (car l)) (cdr l)))
      (tmmd-all l)))

(define (tmmd x)
  ;; the nodes of the Markdown tree for the TeXmacs tree x
  (cond ((string? x) (if (== x "") '() (list (tmmd-text x))))
        ((not (pair? x)) '())
        ((not (symbol? (car x))) '())
        ((tmmd-dispatch 'tmmarkdown-methods% x) => identity)
        ((assoc (car x) tmmd-languages) (tmmd-code-block (car x) (cdr x)))
        ((in? (car x) '(tabular tabular* block block* wide-tabular wide-block
                        tformat table))
         (tmmd-table (car x) (cdr x)))
        ((in? (car x) '(eqnarray eqnarray* align align* gather gather*
                        multline multline* eqsplit eqsplit*))
         (tmmd-equations (car x) (cdr x)))
        ((let* ((s (symbol->string (car x)))
                (s (if (string-ends? s "*")
                       (substring s 0 (- (string-length s) 1)) s)))
           (in? (string->symbol s) tmmd-enunciations))
         (tmmd-enunciation (car x) (cdr x)))
        ;; the entries of the table of contents
        ((string-starts? (symbol->string (car x)) "toc-") '())
        ;; a tag which is not known: what it holds
        (else (tmmd-all (cdr x)))))

(logic-dispatcher tmmarkdown-methods%
  (document tmmd-document)
  (para tmmd-document)
  (concat tmmd-concat)
  (surround tmmd-surround)
  (with tmmd-with)
  (compound tmmd-compound)
  ((:or rigid hgroup freeze unfreeze syntax move shift resize clipped
        repeat repeat*)
   tmmd-first)
  ((:or datoms dlines dpages dbox locus float) tmmd-last)
  (phantom tmmd-noop)

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
   tmmd-noop)

  ((:or hspace space htab) tmmd-space)
  ((:or next-line new-line) tmmd-next-line)
  (hrule tmmd-hrule)

  ;; text
  ((:or em dfn var) tmmd-em)
  (strong tmmd-strong)
  ((:or verbatim code* tt samp kbd) tmmd-code)
  (render-key tmmd-key)
  (underline tmmd-underline)
  ((:or strike-through deleted) tmmd-del)
  (marked tmmd-marked)
  ((:or rsub lsub) tmmd-sub)
  ((:or rsup lsup) tmmd-sup)
  ((:or abbr acronym name small smaller large larger) tmmd-first)
  (nbsp tmmd-nbsp)
  (TeXmacs tmmd-TeXmacs)
  (TeX tmmd-TeX)
  (LaTeX tmmd-LaTeX)
  ((:or hlink hyper-link) tmmd-hlink)
  (hlink* tmmd-hlink*)
  (alt-text tmmd-alt-text)
  (action tmmd-first)
  ((:or href slink) tmmd-href)
  (image tmmd-image)
  (specific tmmd-specific)
  (reference tmmd-reference)
  (pageref tmmd-noop)
  (eqref tmmd-eqref)
  ((:or cite nocite cite-detail) tmmd-cite)
  (footnote tmmd-footnote)

  ;; mathematics
  (math tmmd-math)
  ((:or equation equation*) tmmd-equation)
  (equation-lab tmmd-equation-lab)
  (equations-base tmmd-equation)

  ;; sections and the title
  ((:or part part* part-title) tmmd-part)
  ((:or chapter chapter* chapter-title appendix appendix-title) tmmd-chapter)
  ((:or section section* section-title) tmmd-section)
  ((:or subsection subsection* subsection-title) tmmd-subsection)
  ((:or subsubsection subsubsection* subsubsection-title) tmmd-subsubsection)
  ((:or paragraph paragraph* paragraph-title) tmmd-paragraph)
  ((:or subparagraph subparagraph* subparagraph-title) tmmd-subparagraph)
  ((:or doc-data markdown-doc-data) tmmd-doc-data)
  ((:or abstract abstract-data) tmmd-abstract)

  ;; lists and quotations
  ((:or itemize itemize-minus itemize-dot itemize-arrow) tmmd-itemize)
  ((:or description description-compact description-dash description-aligned
        description-long description-paragraphs)
   tmmd-itemize)
  ((:or enumerate enumerate-numeric enumerate-roman enumerate-Roman
        enumerate-alpha enumerate-Alpha)
   tmmd-enumerate)
  ((:or item item*) tmmd-noop)
  ((:or quotation quote-env verse) tmmd-quotation)

  ;; environments
  ((:or render-theorem render-remark render-exercise render-proof
        render-solution render-enunciation)
   tmmd-render-enunciation)
  ((:or render-big-figure render-small-figure render-big-algorithm
        render-small-algorithm)
   tmmd-render-figure)
  ((:or big-figure small-figure) tmmd-big-figure)
  ((:or big-table small-table) tmmd-big-table)
  (render-bibitem tmmd-first)
  ((:or html-div-class html-div-style html-class html-style html-tag
        html-attr)
   tmmd-last)

  ;; the documentation of TeXmacs
  ((:or tmdoc-title tmdoc-title* tmdoc-title**) tmmd-tmdoc-title)
  (tmdoc-copyright tmmd-tmdoc-copyright)
  (tmdoc-license tmmd-tmdoc-license))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The levels of the headings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-heading-levels x)
  ;; the levels of the headings of the tree
  (cond ((not (pair? x)) '())
        ((func? x '!h) (list (cadr x)))
        (else (append-map tmmd-heading-levels (cdr x)))))

(define (tmmd-set-headings x levels)
  ;; the headings of the tree: h1 for the title, or else for the first level
  ;; which is used; the levels below keep their distances
  (cond ((not (pair? x)) x)
        ((func? x '!display) `(displaymath ,(cadr x)))
        ((func? x '!h)
         (let* ((title? (in? -1 levels))
                (below (list-filter levels (lambda (l) (>= l 0))))
                (top (if (null? below) 0 (apply min below)))
                (n (if (< (cadr x) 0) 1
                       (min 6 (+ (- (cadr x) top) (if title? 2 1))))))
           `(,(string->symbol (string-append "h" (number->string n)))
             ,@(cddr x))))
        (else (cons (car x) (map (lambda (y) (tmmd-set-headings y levels))
                                 (cdr x))))))

(define (tmmd-finalize l)
  ;; the blocks of a document: the levels of its headings, its footnotes
  (let* ((l (append l (reverse tmmd-footnotes)))
         (levels (sort (list-remove-duplicates
                         (append-map tmmd-heading-levels l))
                       <)))
    (map (lambda (x) (tmmd-set-headings x levels)) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->markdown x opts)
  (:type (-> stree list stree))
  (:synopsis "Convert the TeXmacs tree @x into a Markdown tree")
  (tmmd-initialize opts)
  (set! tmmd-document?
        (and (func? x 'document) (tmfile-extract x 'body)
             (or (tmfile-extract x 'TeXmacs) (tmfile-extract x 'style))))
  (if tmmd-document?
      (with body (tmfile-extract x 'body)
        `(!file (markdown ,@(tmmd-finalize (tmmd-blocks (tmmd body))))))
      (let* ((l (tmmd x))
             (r (if (or (tmmd-has-block? l) (nnull? tmmd-footnotes))
                    (tmmd-finalize (tmmd-blocks l))
                    (tmmd-trim l))))
        `(markdown ,@r))))
