
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
(define tmmd-footnotes '())     ; the definitions of the footnotes, reversed,
(define tmmd-footnote-nr 0)     ; each with the rank of its mark in the text
(define tmmd-faithful? #f)      ; a document of the style markdown
(define tmmd-labels (make-ahash-table)) ; the labels which its footnotes keep
(define tmmd-note-labels (make-ahash-table)) ; label -> its entry of tmmd-footnotes

(define (tmmd-initialize opts)
  (set! tmmd-html? (!= (assoc-ref opts "texmacs->markdown:html") "off"))
  (set! tmmd-front-matter?
        (!= (assoc-ref opts "texmacs->markdown:front-matter") "off"))
  (set! tmmd-footnotes '())
  (set! tmmd-note-labels (make-ahash-table))
  (set! tmmd-footnote-nr 0)
  (set! tmmd-faithful? #f)
  (set! tmmd-labels (make-ahash-table))
  (set! tmmd-image-nr 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes: text and blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmmd-keep s l)
  ;; the text in UTF-8, but for the sequences of the list, which the
  ;; conversion would change: ... into an ellipsis, ` into a quote
  (if (null? l) (cork->utf8 s)
      (let* ((what (car l))
             (n (string-length what)))
        (let loop ((start 0) (acc '()))
          (with i (string-search-forwards what start s)
            (if (< i 0)
                (apply string-append
                       (reverse (cons (tmmd-keep (substring s start
                                                            (string-length s))
                                                 (cdr l))
                                      acc)))
                (loop (+ i n)
                      (cons what (cons (tmmd-keep (substring s start i) (cdr l))
                                       acc)))))))))

;; (in a document of the style markdown a backquote is one, as it was read:
;; elsewhere it is the opening quote which TeXmacs displays)
(define (tmmd-text s)
  (tmmd-keep s (if tmmd-faithful? '("..." "`") '("..."))))
(define (tmmd-code-text s) (tmmd-keep s '("..." "`")))

(define (tmmd-block? x)
  (and (pair? x)
       (in? (car x) '(meta h1 h2 h3 h4 h5 h6 !h p blockquote ul ol pre hr
                      table footnote-def !display !raw !note))))

(define (tmmd-blank? x)
  (and (string? x) (== (string-trim-spaces x) "")))

(define (tmmd-merge l)
  ;; with the strings which follow each other as one, and so for the
  ;; emphasized texts and for code: *a**b* would not be read as *ab*. The
  ;; nodes of a run are merged at once, not each one with those before it
  (let loop ((l l) (acc '()))
    ;; acc: what is merged, the last node first
    (cond ((null? l) (reverse acc))
          ((== (car l) "") (loop (cdr l) acc))
          ((string? (car l))
           (let run ((m (cdr l)) (strs (list (car l))))
             (if (and (nnull? m) (string? (car m)))
                 (run (cdr m) (cons (car m) strs))
                 (loop m (cons (apply string-append (reverse strs)) acc)))))
          ((and (pair? (car l)) (in? (caar l) '(em strong del code)))
           (let run ((m (cdr l)) (kids (list (cdar l))))
             (cond ((and (nnull? m) (== (car m) "")) (run (cdr m) kids))
                   ((and (nnull? m) (func? (car m) (caar l)))
                    (run (cdr m) (cons (cdar m) kids)))
                   (else
                     (loop m (cons `(,(caar l)
                                     ,@(tmmd-merge (apply append (reverse kids))))
                                   acc))))))
          (else (loop (cdr l) (cons (car l) acc))))))

(define (tmmd-squeeze s)
  ;; with one space for several
  (let loop ((l (string->list s)) (acc '()) (sp? #f))
    (cond ((null? l) (list->string (reverse acc)))
          ((char=? (car l) #\space)
           (loop (cdr l) (if sp? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

(define (tmmd-trim l)
  ;; text without spaces and line breaks at its ends, and with one space for
  ;; several, but in a document of the style markdown, written as it was read
  (let* ((l (map (lambda (x)
                   (if (and (string? x) (not tmmd-faithful?)) (tmmd-squeeze x) x))
                 (tmmd-merge l)))
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
            ;; (text of the file, which a line holds)
            ((func? x '!raw 2) (list `(html ,(string-replace (caddr x) "\n" " "))))
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

(define (tmmd-same-text? s t)
  ;; are the texts the same but for their spaces and tabs? A line of code
  ;; or of a header which was kept for its tabs is written as it was read,
  ;; but not Markdown, which is something else in code
  (let ((strip (lambda (s)
                 (list->string
                   (list-filter (string->list s)
                                (lambda (c) (not (in? c '(#\space #\tab)))))))))
    (== (strip s) (strip t))))

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
        ;; the macros of the style markdown: what they display, or the text
        ;; of the file while they are not changed
        ((and (in? (car x) '(md-source md-block-source)) (== (length x) 4)
              (== (cadr x) (cadddr x))
              (tmmd-same-text? (tmmd-source (caddr x)) (tmmd-plain (cadr x))))
         (tmmd-source (caddr x)))
        ((and (in? (car x) '(md-source md-block-source md-image md-loose))
              (pair? (cdr x)))
         (tmmd-plain (cadr x)))
        ((func? x 'md-fence 2) (tmmd-plain (caddr x)))
        ((func? x 'md-kept 2) (tmmd-source (caddr x)))
        ((in? (car x) '(md-footnote md-footnote-text md-metadata)) "")
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
  ;; a formula, also of one symbol, is written in LaTeX, which is read back
  ;; as a formula; a symbol which LaTeX does not write is its character
  (let* ((x (if (list-1? l) (car l) `(concat ,@l)))
         (s (tmmd-inline-latex x)))
    (cond ((!= s "") `((math ,s)))
          ((tmmd-symbol x) (list (tmmd-symbol x)))
          (else '()))))

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
      (let* ((body (tmmd (car l)))
             (url (tmmd-plain (cadr l))))
        ;; (but in a document of the style markdown, where it is a link
        ;; of the file to one of its headings)
        (if (or (== url "") (tmmd-has-block? body)
                (and (string-starts? url "#") (not tmmd-faithful?)))
            body
            `((a (@ (href ,url)) ,@(tmmd-merge body)))))))

(define (tmmd-hlink* l)
  ;; a link with a title
  (let ((r (tmmd-hlink l))
        (title (if (< (length l) 3) "" (tmmd-plain (caddr l)))))
    (if (and (list-1? r) (func? (car r) 'a) (!= title ""))
        `((a (@ ,@(cdadar r) (title ,title)) ,@(cddar r)))
        r)))

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

(define tmmd-footnote-rank 0)

(define (tmmd-footnote-add body label)
  ;; the footnote with this label, or with the next number which is not
  ;; the label of another one
  (set! tmmd-footnote-rank (+ tmmd-footnote-rank 1))
  ;; the body may hold footnotes: it is converted before the list is read
  (let* ((rank tmmd-footnote-rank)
         (other (and label (ahash-ref tmmd-note-labels label)))
         (label (or label
                    (let loop ()
                      (set! tmmd-footnote-nr (+ tmmd-footnote-nr 1))
                      (with s (number->string tmmd-footnote-nr)
                        (if (ahash-ref tmmd-labels s) (loop) s)))))
         (blocks (tmmd-blocks (tmmd body))))
    ;; a label which another note has already: the same note is written
    ;; once, and another one gets a label of its own
    (cond ((and other (== (cddr (cdr other)) blocks)) `((footnote ,label)))
          (other (tmmd-footnote-add* rank (tmmd-new-label label) blocks))
          (else (tmmd-footnote-add* rank label blocks)))))

(define (tmmd-footnote-add* rank label blocks)
  (with n (cons rank `(footnote-def ,label ,@blocks))
    (set! tmmd-footnotes (cons n tmmd-footnotes))
    (ahash-set! tmmd-note-labels label n))
  `((footnote ,label)))

(define (tmmd-new-label label)
  ;; label-2, label-3... the first which no note has
  (let loop ((i 2))
    (with s (string-append label "-" (number->string i))
      (if (or (ahash-ref tmmd-labels s) (ahash-ref tmmd-note-labels s))
          (loop (+ i 1))
          s))))

(define (tmmd-footnote l)
  (if (null? l) '() (tmmd-footnote-add (car l) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The macros of the style markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The import of a file (markdowntm.scm) uses them for what the standard
;; tags do not say, and each one is written as what it was read from.
;; md-source and md-block-source hold a body, the text of the file it was
;; read from and a copy of the body as it was read: the text is written as
;; long as the body is this copy, and the body else, which was edited.
;; When a document is expanded this is told before the expansion
;; (tmmarkdown-md-source in tmmarkdown-expand.scm, which leaves md-kept
;; with the text).

(define (tmmd-source x)
  ;; the text of an argument which holds text of the file: a line, or a
  ;; tuple of lines
  (cond ((string? x) (tmmd-code-text x))
        ((func? x 'tuple)
         (let loop ((l (cdr x)) (acc '()))
           (cond ((null? l) (apply string-append (reverse acc)))
                 ((null? acc) (loop (cdr l) (list (tmmd-source (car l)))))
                 (else (loop (cdr l) (cons (tmmd-source (car l))
                                           (cons "\n" acc)))))))
        (else "")))

(tm-define (tmmd-language-name tag)
  (:synopsis "The language which is written after the fence of the code @tag")
  (or (assoc-ref tmmd-languages tag) ""))

(define (tmmd-md-source* kind l)
  (cond ((null? l) '())
        ((and (== (length l) 3) (== (car l) (caddr l)))
         (tmmd-md-kept (list kind (cadr l))))
        (else (tmmd (car l)))))

(define (tmmd-md-source l) (tmmd-md-source* "inline" l))
(define (tmmd-md-block-source l)
  (tmmd-md-source* (if (and (nnull? l) (tmmarkdown-table? (car l)))
                       "table" "block")
                   l))

(tm-define (tmmarkdown-table? x)
  (:synopsis "Is the tree @x a table?")
  (and (pair? x)
       (in? (car x) '(block block* tabular tabular* wide-block wide-tabular
                      tformat table))))

(define (tmmd-md-kept l)
  ;; (md-kept kind text): the text of the file, in a paragraph or as
  ;; paragraphs of its own. These are (!raw kind "text") until the document
  ;; is put together, and then a table, a formula or HTML with this text
  ;; as their source (see tmmd-set-headings)
  (if (< (length l) 2) '()
      (with s (tmmd-source (cadr l))
        (cond ((== (car l) "inline") `((html ,s)))
              ((== (car l) "table") `((!raw "table" ,s)))
              ((or (string-starts? s "$$") (string-starts? s "\\["))
               `((!raw "display" ,s)))
              (else `((!raw "html" ,s)))))))

(define (tmmd-md-loose l)
  ;; the lists of the body, with blank lines between their items
  (map (lambda (x)
         (if (and (pair? x) (in? (car x) '(ul ol)))
             (if (and (pair? (cdr x)) (func? (cadr x) '@))
                 `(,(car x) (@ (loose "true") ,@(cdadr x)) ,@(cddr x))
                 `(,(car x) (@ (loose "true")) ,@(cdr x)))
             x))
       (tmmd-all l)))

(define (tmmd-md-fence l)
  ;; code with the words which follow its fence: the first is the language,
  ;; and they are written while the code is still in this language
  (if (< (length l) 2) (tmmd-all l)
      (let* ((info (string-trim-spaces (tmmd-plain (car l))))
             (i (list-find (iota (string-length info))
                           (lambda (k) (in? (string-ref info k) '(#\space #\tab)))))
             (lang (if (not i) info (substring info 0 i)))
             (code (cadr l))
             (tag (and (pair? code) (car code))))
        (if (!= tag (tmmd-language-tag lang)) (tmmd code)
            (map (lambda (x)
                   (if (func? x 'pre)
                       `(pre ,@(if (== info "") '()
                                   `((@ (lang ,lang)
                                        ,@(if (== info lang) '()
                                              `((info ,info))))))
                             ,@(if (and (pair? (cdr x)) (func? (cadr x) '@))
                                   (cddr x) (cdr x)))
                       x))
                 (tmmd code))))))

(define (tmmd-md-image l)
  ;; an image with its description and its title
  (if (< (length l) 3) (tmmd-first l)
      (let* ((alt (tmmd-trim (tmmd-inline (tmmd (cadr l)))))
             (title (tmmd-plain (caddr l))))
        (map (lambda (x)
               (if (and (func? x 'img) (pair? (cdr x)) (func? (cadr x) '@))
                   `(img (@ ,@(list-filter (cdadr x)
                                           (lambda (a) (nin? (car a) '(alt title))))
                            (alt ,(apply string-append (map tmmd-node-text alt)))
                            ,@(if (== title "") '() `((title ,title))))
                         ,@alt)
                   x))
             (tmmd (car l))))))

(define (tmmd-node-text x)
  ;; the text of a node, without its markup
  (cond ((string? x) x)
        ((not (pair? x)) "")
        ((and (pair? (cdr x)) (func? (cadr x) '@))
         (apply string-append (map tmmd-node-text (cddr x))))
        (else (apply string-append (map tmmd-node-text (cdr x))))))

(define (tmmd-md-footnote l)
  (if (< (length l) 2) (tmmd-footnote l)
      (tmmd-footnote-add (cadr l) (tmmd-plain (car l)))))

(define (tmmd-meta-text x)
  ;; the text of a paragraph of the header: a line for each md-meta, and
  ;; the text of what was typed
  (cond ((func? x 'md-meta 2)
         (let ((key (tmmd-plain (cadr x)))
               (val (tmmd-plain (caddr x))))
           (cond ((== key "") val)
                 ((== val "") (string-append key ":"))
                 (else (string-append key ": " val)))))
        ((func? x 'concat) (apply string-append (map tmmd-meta-text (cdr x))))
        ((func? x 'document)
         (with l (map tmmd-meta-text (cdr x))
           (if (null? l) ""
               (apply string-append
                      (cons (car l) (append-map (lambda (s) (list "\n" s))
                                                (cdr l)))))))
        ((func? x 'with) (tmmd-meta-text (cAr x)))
        (else (tmmd-plain x))))

(define (tmmd-yaml-key? s)
  ;; does the line start with a key, as the reader of headers wants it (see
  ;; md-yaml-key?): text before a colon which ends the line or is followed
  ;; by a space, and not a space, a dash, # or : first
  (let ((n (string-length s)))
    (and (> n 0) (not (in? (string-ref s 0) '(#\space #\tab #\- #\# #\:)))
         (let loop ((i 1))
           (and (< i n)
                (or (and (char=? (string-ref s i) #\:)
                         (or (== (+ i 1) n)
                             (in? (string-ref s (+ i 1)) '(#\space #\tab))))
                    (loop (+ i 1))))))))

(define (tmmd-yaml-line s)
  ;; the line s of the header as it is written: a line --- or ... would end
  ;; it, and is written after a space; a line which YAML does not have
  ;; (text which was typed) is written as a comment
  (cond ((in? (string-trim-spaces-right s) '("---" "...")) (string-append " " s))
        ((or (== s "") (tmmd-yaml-key? s) (string-starts? s "#")
             (string-starts? s "- ") (string-starts? s " ")
             (string-starts? s (string #\tab)))
         s)
        (else (string-append "# " s))))

(define (tmmd-md-metadata l)
  ;; the header of the file. With no key in it, it would not be read back
  ;; as a header: its lines are then paragraphs, and an empty header is
  ;; not written
  (let* ((s (tmmd-meta-text (if (null? l) '(document) (car l))))
         (lines (string-tokenize-by-char s #\newline)))
    (cond ((list-and (map (lambda (s) (== (string-trim-spaces s) "")) lines))
           '())
          ((not (list-or (map tmmd-yaml-key? lines)))
           (map (lambda (s) `(p ,s))
                (list-filter lines (lambda (s) (!= (string-trim-spaces s) "")))))
          (else
            (with lines (map tmmd-yaml-line lines)
              `((meta (@ (source ,(let loop ((l (cdr lines)) (acc (list (car lines))))
                                    (if (null? l) (apply string-append (reverse acc))
                                        (loop (cdr l)
                                              (cons (car l) (cons "\n" acc))))))))))))))

(define (tmmd-md-h6 l) (tmmd-heading 7 l))

(define (tmmd-md-footnote-text l)
  ;; where the text of the footnote with this label is written
  (if (null? l) '() `((!note ,(tmmd-plain (car l))))))

(define (tmmd-kept-labels x)
  ;; the labels of the footnotes of the tree which keep theirs
  (cond ((not (pair? x)) '())
        ((and (func? x 'md-footnote 2) (string? (cadr x)))
         (cons (tmmd-plain (cadr x)) (tmmd-kept-labels (caddr x))))
        (else (append-map tmmd-kept-labels (cdr x)))))

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
      (let* ((before (tmmd-inline (tmmd (car l))))
             (body (tmmd (caddr l)))
             (after (tmmd-inline (tmmd (cadr l)))))
        (tmmd-attach before body after))))

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

(define (tmmd-list-start pars)
  ;; (number . paragraphs) for a list which sets the number of its first
  ;; item, with (assign "item-nr" "4") before it: this number, and the
  ;; paragraphs without the assignment; else #f
  (and (nnull? pars) (func? (car pars) 'concat) (pair? (cdar pars))
       (with a (cadar pars)
         (and (func? a 'assign 2) (== (cadr a) "item-nr")
              (string? (caddr a)) (string->number (caddr a))
              (cons (+ (string->number (caddr a)) 1)
                    (cons `(concat ,@(cddar pars)) (cdr pars)))))))

(define (tmmd-list tag l)
  (let* ((body (if (null? l) '(document) (car l)))
         (pars (if (func? body 'document) (cdr body) (list body)))
         (start (and (== tag 'ol) (tmmd-list-start pars)))
         (items (tmmd-items (if start (cdr start) pars))))
    (if (null? items) '()
        `((,tag
           ,@(if (and start (!= (car start) 1))
                 `((@ (start ,(number->string (car start))))) '())
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

(define (tmmd-marked-list marker l)
  ;; in a document of the style markdown the three lists are those whose
  ;; items start with a dash, a star and a plus
  (with r (tmmd-list 'ul l)
    (if (and tmmd-faithful? (nnull? r))
        `((ul (@ (marker ,marker)) ,@(cdar r)) ,@(cdr r))
        r)))
(define (tmmd-itemize-dot l) (tmmd-marked-list "*" l))
(define (tmmd-itemize-arrow l) (tmmd-marked-list "+" l))
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
      (let* ((name (tmmd (car l)))
             (body (tmmd (cadr l))))
        (tmmd-titled name body))))

(define tmmd-enunciations
  '(theorem proposition lemma corollary conjecture axiom definition notation
    remark note example convention warning acknowledgments exercise problem
    question solution answer proof algorithm))

(define (tmmd-enunciation tag l)
  ;; a theorem which was not expanded: its name without a number
  (let* ((s (symbol->string tag))
         (s (if (string-ends? s "*") (substring s 0 (- (string-length s) 1)) s)))
    (tmmd-titled (list (upcase-first s)) (tmmd-all l))))

(define (tmmd-image-figure body caption)
  ;; a figure which is a single image is this image, with the caption as
  ;; its description: ![caption](file); #f for the other figures
  (let* ((inside (lambda (l)
                   ;; the text of a single paragraph
                   (with l (tmmd-trim l)
                     (if (and (list-1? l) (func? (car l) 'p))
                         (tmmd-trim (cdar l))
                         l))))
         (b (inside body))
         (c (inside caption)))
    (and (list-1? b) (func? (car b) 'img) (not (tmmd-has-block? c))
         `((p (img ,(cadar b) ,@c))))))

(define (tmmd-render-figure l)
  ;; the type, the name with its number, the figure and its caption
  (if (< (length l) 4) (tmmd-all l)
      (let* ((body (tmmd (caddr l)))
             (caption (tmmd (cadddr l))))
        (or (tmmd-image-figure body caption)
            (append (tmmd-blocks body)
                    (tmmd-titled (tmmd (cadr l)) caption))))))

(define (tmmd-figure name l)
  ;; a figure which was not expanded: the figure and its caption
  (if (< (length l) 2) (tmmd-all l)
      (let* ((body (tmmd (car l)))
             (caption (tmmd (cadr l))))
        (or (tmmd-image-figure body caption)
            (append (tmmd-blocks body)
                    (tmmd-titled (list name) caption))))))

(define (tmmd-big-figure l) (tmmd-figure "Figure" l))
(define (tmmd-big-table l) (tmmd-figure "Table" l))

(define tmmd-languages
  '((cpp-code . "cpp") (python-code . "python") (scm-code . "scheme")
    (shell-code . "sh") (java-code . "java") (javascript-code . "javascript")
    (json-code . "json") (julia-code . "julia") (r-code . "r")
    (scala-code . "scala") (fortran-code . "fortran") (octave-code . "octave")
    (scilab-code . "scilab") (dot-code . "dot") (mmx-code . "mathemagix")
    (verbatim-code . "") (pseudo-code . "") (render-code . "") (code . "")))

(define tmmd-language-tags
  ;; the tags of the code in the languages written after a fence
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

(tm-define (tmmd-language-tag name)
  (:synopsis "The tag of code in the language @name, written after a fence")
  (or (assoc-ref tmmd-language-tags (locase-all name)) 'verbatim-code))

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

(define (tmmd-cell-lines l)
  ;; the blocks l as the lines of a cell, each one a list of nodes: a cell
  ;; holds text only, so that a formula is written in the text, the lines of
  ;; code are code each, and the items of a list are lines with their marks
  (append-map
    (lambda (x)
      (cond ((func? x 'p) (list (cdr x)))
            ((func? x '!display) (list (list `(displaymath ,(cadr x)))))
            ((func? x '!h) (list (cddr x)))
            ((func? x '!raw 2)
             (map (lambda (s) (list `(html ,s)))
                  (list-filter (string-tokenize-by-char (caddr x) #\newline)
                               (lambda (s) (!= s "")))))
            ((func? x 'pre)
             (map (lambda (s) (list `(code ,s)))
                  (list-filter (string-tokenize-by-char (cAr x) #\newline)
                               (lambda (s) (!= s "")))))
            ((func? x 'blockquote) (tmmd-cell-lines (cdr x)))
            ((or (func? x 'ul) (func? x 'ol))
             (let loop ((items (list-filter (cdr x) (lambda (y) (func? y 'li))))
                        (nr 1) (acc '()))
               (if (null? items) (reverse acc)
                   (let* ((mark (if (func? x 'ol)
                                    (string-append (number->string nr) ". ")
                                    (string-append (tmmd-text "<bullet>") " ")))
                          (b (list-filter (cdar items)
                                          (lambda (y) (not (func? y '@)))))
                          (lines (tmmd-cell-lines b))
                          (lines (if (null? lines) (list (list mark))
                                     (cons (cons mark (car lines)) (cdr lines)))))
                     (loop (cdr items) (+ nr 1)
                           (append (reverse lines) acc))))))
            (else '())))
    l))

(define (tmmd-cell x)
  ;; the text of a cell; its paragraphs are lines
  (let* ((body (if (func? x 'cell 1) (cadr x) x))
         (l (tmmd body)))
    (tmmd-trim
      (if (tmmd-has-block? l)
          (let loop ((b (tmmd-cell-lines (tmmd-blocks l))) (acc '()))
            (if (null? b) (reverse acc)
                (loop (cdr b)
                      (append (reverse (car b))
                              (if (null? acc) acc (cons '(br) acc))))))
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
  (action tmmd-first)
  ((:or href slink) tmmd-href)
  (image tmmd-image)
  (specific tmmd-specific)
  (reference tmmd-reference)
  (pageref tmmd-noop)
  (eqref tmmd-eqref)
  ((:or cite nocite cite-detail) tmmd-cite)
  (footnote tmmd-footnote)
  (md-source tmmd-md-source)
  (md-block-source tmmd-md-block-source)
  (md-kept tmmd-md-kept)
  (md-loose tmmd-md-loose)
  (md-fence tmmd-md-fence)
  (md-image tmmd-md-image)
  (md-footnote tmmd-md-footnote)
  (md-metadata tmmd-md-metadata)
  (md-meta tmmd-noop)
  (md-h6 tmmd-md-h6)
  (md-footnote-text tmmd-md-footnote-text)

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
  ((:or itemize itemize-minus) tmmd-itemize)
  (itemize-dot tmmd-itemize-dot)
  (itemize-arrow tmmd-itemize-arrow)
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

(define (tmmd-ref-def-text? s)
  ;; is the text kept from the file the definition of a link, [label]: ...?
  (and (string-starts? s "[") (not (string-starts? s "[^"))
       (let ((i (string-search-forwards "]:" 0 s))
             (j (string-search-forwards "\n" 0 s)))
         (and (> i 0) (or (< j 0) (< i j))))))

(define (tmmd-set-headings x levels)
  ;; the headings of the tree: h1 for the title, or else for the first level
  ;; which is used; the levels below keep their distances
  (cond ((not (pair? x)) x)
        ((func? x '!display) `(displaymath ,(cadr x)))
        ((func? x '!raw 2)
         (cond ((== (cadr x) "table") `(table (@ (source ,(caddr x)))))
               ((== (cadr x) "display")
                `(displaymath (@ (source ,(caddr x))) ""))
               ;; (the definition of a link, which nothing uses)
               ((tmmd-ref-def-text? (caddr x)) `(ref-def ,(caddr x)))
               (else `(html ,(caddr x)))))
        ;; in a document of the style markdown each heading has its
        ;; level: 1 for a section, 2 for a subsection...
        ((and (func? x '!h) tmmd-faithful?)
         `(,(string->symbol
              (string-append "h" (number->string
                                   (max 1 (min 6 (- (cadr x) 1))))))
           ,@(cddr x)))
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
  (let* ((notes (map cdr (sort tmmd-footnotes
                               (lambda (a b) (< (car a) (car b))))))
         ;; the texts of the footnotes: where the document says (!note
         ;; "label"), in a quote, an item or another note as well, and
         ;; after everything else for the others
         (placed (make-ahash-table))
         (by-label (with t (make-ahash-table)
                     (for-each (lambda (n)
                                 (ahash-set! t (cadr n)
                                             (cons n (or (ahash-ref t (cadr n)) '()))))
                               (reverse notes))
                     t))
         (place (lambda (place x)
                  (if (not (and (pair? x)
                                (in? (car x) '(markdown blockquote ul ol li
                                               footnote-def))))
                      x
                      (cons (car x)
                            (append-map
                              (lambda (y)
                                (cond ((not (func? y '!note 1))
                                       (list (place place y)))
                                      ((ahash-ref placed (cadr y)) '())
                                      (else
                                        (ahash-set! placed (cadr y) #t)
                                        (map (lambda (n) (place place n))
                                             (or (ahash-ref by-label (cadr y))
                                                 '())))))
                              (cdr x))))))
         (l (cdr (place place (cons 'markdown l))))
         (l (let loop ((ns notes) (acc '()))
              (cond ((null? ns) (append l (reverse acc)))
                    ((ahash-ref placed (cadr (car ns))) (loop (cdr ns) acc))
                    (else
                      (ahash-set! placed (cadr (car ns)) #t)
                      (loop (cdr ns) (cons (place place (car ns)) acc))))))
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
  (set! tmmd-footnote-rank 0)
  (set! tmmd-document?
        (and (func? x 'document) (tmfile-extract x 'body)
             (or (tmfile-extract x 'TeXmacs) (tmfile-extract x 'style))))
  (set! tmmd-faithful?
        (and tmmd-document?
             (with style (tmfile-extract x 'style)
               (or (== style "markdown")
                   (and (pair? style) (in? "markdown" (cdr style)))))))
  (set! tmmd-labels (make-ahash-table))
  (for-each (lambda (s) (ahash-set! tmmd-labels s #t)) (tmmd-kept-labels x))
  (if tmmd-document?
      (with body (tmfile-extract x 'body)
        `(!file (markdown ,@(tmmd-finalize (tmmd-blocks (tmmd body))))))
      (let* ((l (tmmd x))
             (r (if (or (tmmd-has-block? l) (nnull? tmmd-footnotes))
                    (tmmd-finalize (tmmd-blocks l))
                    (tmmd-trim l))))
        `(markdown ,@r))))
