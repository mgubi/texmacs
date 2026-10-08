
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdownin.scm
;; DESCRIPTION : parsing Markdown into Markdown trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The syntax is CommonMark with the usual extensions: tables, strike-through,
;; task lists and bare links (GitHub), footnotes, a YAML header with the
;; title, the authors and the date, and mathematics in LaTeX between dollars
;; (Pandoc). The result is a Markdown tree, with strings in UTF-8, as they
;; are in the text:
;;
;;   (markdown block...)
;;   blocks:  (meta (key "value")...)  (h1 inline...) ... (h6 inline...)
;;            (p inline...)  (blockquote block...)  (hr)
;;            (ul (li block...)...)  (ol (li block...)...)
;;            (pre "code")  (table (tr (th inline...)...) (tr (td ...)...)...)
;;            (displaymath "latex")  (html "text")
;;            (footnote-def "label" block...)
;;   inlines: "text"  (em ...)  (strong ...)  (del ...)  (code "text")
;;            (a ...)  (img)  (br)  (math "latex")  (displaymath "latex")
;;            (html "text")  (footnote "label")
;;
;; A node may have attributes, (tag (@ (name "value")...) ...): start and
;; loose for the lists, checked for the items of task lists, lang for pre,
;; align for th and td, href and title for a, src, alt, title, width and
;; height for img (the sizes as in HTML or CSS: 300, 50%, 2cm). The
;; children of img are its description, of which alt is the plain text.
;;
;; The blocks are parsed first, the text of the paragraphs, headings and
;; cells afterwards: a link may refer to a definition which follows it.

(texmacs-module (convert markdown markdownin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings and lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (md-space? c) (or (char=? c #\space) (char=? c #\tab)))
(define (md-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (md-ascii-alpha? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))))
(define (md-punctuation? c)
  (and (< (char->integer c) 128)
       (not (md-ascii-alpha? c)) (not (md-digit? c))
       (> (char->integer c) 32)))

(define (md-skip-spaces s i)
  (let loop ((i i))
    (if (and (< i (string-length s)) (md-space? (string-ref s i)))
        (loop (+ i 1)) i)))

(define (md-trim-left s)
  (substring s (md-skip-spaces s 0) (string-length s)))

(define (md-trim-right s)
  (let loop ((n (string-length s)))
    (if (and (> n 0) (md-space? (string-ref s (- n 1))))
        (loop (- n 1))
        (substring s 0 n))))

(define (md-trim s) (md-trim-left (md-trim-right s)))

(define (md-blank? s) (== (md-skip-spaces s 0) (string-length s)))

(define (md-indent s)
  ;; the number of spaces which start the line
  (let loop ((i 0))
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
        (loop (+ i 1)) i)))

(define (md-dedent s n)
  ;; without at most n spaces at its start
  (substring s (min n (md-indent s)) (string-length s)))

(define (md-starts? s i what)
  (let ((n (string-length what)))
    (and (<= (+ i n) (string-length s))
         (string=? (substring s i (+ i n)) what))))

(define (md-run s i c)
  ;; the end of the run of characters c which starts at i
  (let loop ((j i))
    (if (and (< j (string-length s)) (char=? (string-ref s j) c))
        (loop (+ j 1)) j)))

(define (md-find s i c)
  ;; the first c from i on, or #f
  (let loop ((j i))
    (cond ((>= j (string-length s)) #f)
          ((char=? (string-ref s j) c) j)
          (else (loop (+ j 1))))))

(define (md-search s i what)
  (with r (string-search-forwards what i s)
    (and (>= r 0) r)))

(define (md-expand-tabs s)
  ;; the tabs at the start of the line as spaces, to the next multiple of 4
  (if (not (md-find s 0 #\tab)) s
      (let loop ((i 0) (col 0) (acc '()))
        (cond ((>= i (string-length s)) (list->string (reverse acc)))
              ((char=? (string-ref s i) #\space)
               (loop (+ i 1) (+ col 1) (cons #\space acc)))
              ((char=? (string-ref s i) #\tab)
               (with n (- 4 (remainder col 4))
                 (loop (+ i 1) (+ col n)
                       (append (make-list n #\space) acc))))
              (else (string-append (list->string (reverse acc))
                                   (substring s i (string-length s))))))))

(define (md-lines s)
  ;; the lines of the text, without their ends
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((>= i (string-length s))
           (reverse (if (< start i) (cons (substring s start i) acc) acc)))
          ((char=? (string-ref s i) #\newline)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
          ((char=? (string-ref s i) #\return)
           (with j (if (md-starts? s (+ i 1) "\n") (+ i 2) (+ i 1))
             (loop j j (cons (substring s start i) acc))))
          (else (loop (+ i 1) start acc)))))

(define (md-join l sep)
  (cond ((null? l) "")
        ((null? (cdr l)) (car l))
        (else (apply string-append
                     (cons (car l)
                           (append-map (lambda (x) (list sep x)) (cdr l)))))))

(define (md-label s)
  ;; the label of a link definition, as it is compared
  (let loop ((l (string->list (md-trim (locase-all s)))) (acc '()) (sp? #f))
    (cond ((null? l) (list->string (reverse acc)))
          ((or (md-space? (car l)) (char=? (car l) #\newline))
           (loop (cdr l) (if sp? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The starts of the blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (md-atx s)
  ;; (level . text) for a heading which starts with #
  (let* ((i (md-indent s))
         (j (md-run s i #\#))
         (n (- j i)))
    (and (<= i 3) (>= n 1) (<= n 6)
         (or (== j (string-length s)) (md-space? (string-ref s j)))
         (let* ((t (md-trim (substring s j (string-length s))))
                ;; without a closing sequence of #
                (k (let loop ((k (string-length t)))
                     (if (and (> k 0) (char=? (string-ref t (- k 1)) #\#))
                         (loop (- k 1)) k)))
                (t* (cond ((== k (string-length t)) t)
                          ((== k 0) "")
                          ((md-space? (string-ref t (- k 1)))
                           (md-trim-right (substring t 0 k)))
                          (else t))))
           (cons n t*)))))

(define (md-hr? s)
  ;; three or more -, * or _, with spaces only
  (let* ((i (md-indent s)))
    (and (<= i 3) (< i (string-length s))
         (in? (string-ref s i) '(#\- #\* #\_))
         (let ((c (string-ref s i)))
           (let loop ((j i) (n 0))
             (cond ((>= j (string-length s)) (>= n 3))
                   ((char=? (string-ref s j) c) (loop (+ j 1) (+ n 1)))
                   ((md-space? (string-ref s j)) (loop (+ j 1) n))
                   (else #f)))))))

(define (md-setext s)
  ;; 1 for a line of =, 2 for a line of -, under a heading
  (let* ((t (md-trim-right s))
         (i (md-indent t)))
    (and (<= i 3) (< i (string-length t))
         (in? (string-ref t i) '(#\= #\-))
         (== (md-run t i (string-ref t i)) (string-length t))
         (if (char=? (string-ref t i) #\=) 1 2))))

(define (md-fence s)
  ;; (character length indent info) for the start of a fenced code block
  (let* ((i (md-indent s)))
    (and (<= i 3) (< i (string-length s))
         (in? (string-ref s i) '(#\` #\~))
         (let* ((c (string-ref s i))
                (j (md-run s i c))
                (info (md-trim (substring s j (string-length s)))))
           (and (>= (- j i) 3)
                (not (and (char=? c #\`) (md-find info 0 #\`)))
                (list c (- j i) i info))))))

(define (md-fence-end? s fence)
  (let* ((i (md-indent s))
         (j (md-run s i (car fence))))
    (and (<= i 3) (>= (- j i) (cadr fence))
         (md-blank? (substring s j (string-length s))))))

(define (md-quote s)
  ;; the rest of a line which starts with >
  (let* ((i (md-indent s)))
    (and (<= i 3) (< i (string-length s))
         (char=? (string-ref s i) #\>)
         (let ((j (+ i 1)))
           (substring s (if (md-starts? s j " ") (+ j 1) j)
                      (string-length s))))))

(define (md-item s)
  ;; (kind indent width number) for the start of a list item: kind is the
  ;; character of the marker (- + * . or the closing parenthesis), width the
  ;; column where its contents start
  (let* ((i (md-indent s))
         (n (string-length s)))
    (and (<= i 3) (< i n)
         (let* ((c (string-ref s i))
                (j (cond ((in? c '(#\- #\+ #\*)) (+ i 1))
                         ((md-digit? c)
                          (let loop ((j i))
                            (cond ((>= j n) #f)
                                  ((md-digit? (string-ref s j)) (loop (+ j 1)))
                                  ((and (in? (string-ref s j) '(#\. #\)))
                                        (<= (- j i) 9))
                                   (+ j 1))
                                  (else #f))))
                         (else #f))))
           (and j
                (or (== j n) (md-space? (string-ref s j)))
                (let* ((k (md-skip-spaces s j))
                       (kind (string-ref s (- j 1)))
                       ;; an item which starts with indented code, or which
                       ;; is empty, has its contents one column after
                       (w (if (or (== k n) (> (- k j) 4)) (+ j 1) k))
                       (num (and (md-digit? c)
                                 (string->number (substring s i (- j 1))))))
                  (list kind i w num)))))))

(define (md-html-start? s)
  (let* ((i (md-indent s))
         (n (string-length s)))
    (and (<= i 3) (< (+ i 1) n)
         (char=? (string-ref s i) #\<)
         (let ((c (string-ref s (+ i 1))))
           (or (md-ascii-alpha? c)
               (and (char=? c #\/) (< (+ i 2) n)
                    (md-ascii-alpha? (string-ref s (+ i 2))))
               (md-starts? s (+ i 1) "!--")
               (char=? c #\?)
               (and (char=? c #\!) (< (+ i 2) n)
                    (md-ascii-alpha? (string-ref s (+ i 2)))))))))

(define (md-html-block? s)
  ;; a line with a tag alone, or which starts with a tag of a block: an
  ;; inline tag, as <b>, at the start of a line is part of a paragraph
  (and (md-html-start? s)
       (let* ((t (md-trim s))
              (i (if (md-starts? t 1 "/") 2 1))
              (j (let loop ((j i))
                   (if (and (< j (string-length t))
                            (or (md-ascii-alpha? (string-ref t j))
                                (md-digit? (string-ref t j))))
                       (loop (+ j 1)) j)))
              (name (locase-all (substring t i j))))
         (or (md-starts? t 1 "!") (md-starts? t 1 "?")
             (in? name '("address" "article" "aside" "blockquote" "body"
                         "center" "details" "dd" "div" "dl" "dt" "fieldset"
                         "figcaption" "figure" "footer" "form" "h1" "h2" "h3"
                         "h4" "h5" "h6" "head" "header" "hr" "html" "iframe"
                         "li" "main" "nav" "ol" "p" "pre" "script" "section"
                         "style" "summary" "table" "tbody" "td" "tfoot" "th"
                         "thead" "title" "tr" "ul" "video"))))))

(define (md-literal-brackets? s i)
  ;; whether \[ at i opens plain text and not a formula: \[2\], \[see below\]
  ;; are the escaped brackets which other programs write
  (with e (md-search s (+ i 2) "\\]")
    (and e
         (let* ((t (md-trim (substring s (+ i 2) e)))
                (m (string-length t)))
           (and (> m 0)
                (not (and (== m 1) (char-alphabetic? (string-ref t 0))))
                (let loop ((j 0))
                  (or (>= j m)
                      (let ((c (string-ref t j)))
                        (and (or (char-alphabetic? c) (char-numeric? c)
                                 (in? c '(#\space #\, #\. #\; #\: #\-)))
                             (loop (+ j 1)))))))))))

(define (md-math-start? s)
  (let ((t (md-trim s)))
    (or (md-starts? t 0 "$$")
        (and (md-starts? t 0 "\\[") (not (md-literal-brackets? t 0))))))

(define (md-split-row s)
  ;; the cells of a row of a table
  (let* ((t (md-trim s))
         (t (if (md-starts? t 0 "|") (substring t 1 (string-length t)) t))
         (n (string-length t))
         (t (if (and (> n 0) (char=? (string-ref t (- n 1)) #\|)
                     (not (and (> n 1) (char=? (string-ref t (- n 2)) #\\))))
                (substring t 0 (- n 1)) t)))
    (let loop ((i 0) (start 0) (acc '()) (code #f))
      (cond ((>= i (string-length t))
             (reverse (cons (md-trim (substring t start i)) acc)))
            ((and (char=? (string-ref t i) #\\) (< (+ i 1) (string-length t)))
             (loop (+ i 2) start acc code))
            ((char=? (string-ref t i) #\`)
             (loop (+ i 1) start acc (not code)))
            ((and (char=? (string-ref t i) #\|) (not code))
             (loop (+ i 1) (+ i 1)
                   (cons (md-trim (substring t start i)) acc) code))
            (else (loop (+ i 1) start acc code))))))

(define (md-unescape-pipes s)
  (string-replace s "\\|" "|"))

(define (md-table-align s)
  ;; the alignment told by a cell of the line under the header, or #f
  (let* ((n (string-length s))
         (l? (and (> n 0) (char=? (string-ref s 0) #\:)))
         (r? (and (> n 1) (char=? (string-ref s (- n 1)) #\:)))
         (i (if l? 1 0))
         (j (if r? (- n 1) n)))
    (and (> j i)
         (== (md-run s i #\-) j)
         (cond ((and l? r?) "center") (r? "right") (l? "left") (else "")))))

(define (md-table-delim s)
  ;; the alignments of the columns, for the line under the header of a table
  (and (md-find s 0 #\-)
       (or (md-find s 0 #\|) (md-find s 0 #\:))
       (let ((l (map md-table-align (md-split-row s))))
         (and (nnull? l) (list-and l) l))))

(define (md-ref-def s)
  ;; (label url title) for the definition of a link, [label]: url "title"
  (let* ((i (md-indent s)))
    (and (<= i 3) (md-starts? s i "[") (not (md-starts? s i "[^"))
         (let ((j (md-find s i #\])))
           (and j (md-starts? s j "]:") (> j (+ i 1))
                (let* ((k (md-skip-spaces s (+ j 2)))
                       (n (string-length s))
                       (angle? (md-starts? s k "<"))
                       (e (if angle?
                              (or (md-find s k #\>) n)
                              (let loop ((e k))
                                (if (and (< e n) (not (md-space? (string-ref s e))))
                                    (loop (+ e 1)) e))))
                       (url (if angle?
                                (substring s (+ k 1) e)
                                (substring s k e)))
                       (rest (md-trim (substring s (min n (if angle? (+ e 1) e))
                                                 n)))
                       (m (string-length rest))
                       (title (if (and (>= m 2)
                                       (in? (string-ref rest 0) '(#\" #\' #\()))
                                  (substring rest 1 (- m 1))
                                  "")))
                  (and (!= url "")
                       (or (== rest "") (!= title "") (>= m 2))
                       (list (md-label (substring s (+ i 1) j)) url title))))))))

(define (md-footnote-def s)
  ;; (label . text) for the definition of a footnote, [^label]: text
  (let* ((i (md-indent s)))
    (and (<= i 3) (md-starts? s i "[^")
         (let ((j (md-find s i #\])))
           (and j (md-starts? s j "]:") (> j (+ i 2))
                (cons (substring s (+ i 2) j)
                      (md-trim-left (substring s (+ j 2) (string-length s)))))))))

(define (md-interrupts? s)
  ;; does the line end the paragraph before it?
  (or (md-atx s) (md-fence s) (md-quote s) (md-hr? s) (md-html-block? s)
      (md-math-start? s)
      (with it (md-item s)
        (and it
             ;; not an empty item, and an ordered list starts with 1
             (< (caddr it) (+ (string-length s) 1))
             (not (md-blank? (substring s (min (string-length s)
                                               (- (caddr it) 1))
                                        (string-length s))))
             (or (not (cadddr it)) (== (cadddr it) 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the definitions of the links and of the footnotes of the document
(define md-refs (make-ahash-table))

;; A block whose text is not parsed yet is (!text tag attributes "text"); the
;; functions below return the blocks and the lines which remain.

(define (md-front-matter lines)
  ;; the YAML header, as (meta (key "value")...), or #f
  (and (nnull? lines) (== (md-trim-right (car lines)) "---")
       (let loop ((l (cdr lines)) (acc '()) (key #f))
         (cond ((null? l) #f)
               ((in? (md-trim-right (car l)) '("---" "..."))
                (cons `(meta ,@(reverse acc)) (cdr l)))
               (else
                 (let* ((s (car l))
                        (c (md-find s 0 #\:))
                        (item? (md-starts? (md-trim-left s) 0 "- ")))
                   (cond ((and item? key)
                          ;; an item of a list: one more value of the key
                          (with v (md-yaml-value
                                    (substring (md-trim-left s) 2
                                               (string-length (md-trim-left s))))
                            (loop (cdr l) (cons (list key v) acc) key)))
                         ((and c (> c 0) (== (md-indent s) 0))
                          (let* ((k (string->symbol (locase-all (md-trim (substring s 0 c)))))
                                 (rest (md-trim (substring s (+ c 1) (string-length s)))))
                            (if (in? rest '("|" ">" "|-" ">-" "|+" ">+"))
                                ;; a text on the lines which follow, indented
                                (let sub ((l (cdr l)) (text '()))
                                  (if (and (nnull? l)
                                           (or (md-blank? (car l))
                                               (> (md-indent (car l)) 0)))
                                      (sub (cdr l)
                                           (if (md-blank? (car l)) text
                                               (cons (md-trim (car l)) text)))
                                      (loop l
                                            (if (null? text) acc
                                                (cons (list k (md-join (reverse text) " "))
                                                      acc))
                                            k)))
                                (loop (cdr l)
                                      (append (reverse
                                                (map (lambda (v) (list k v))
                                                     (list-filter (md-yaml-values rest)
                                                                  (lambda (v) (!= v "")))))
                                              acc)
                                      k))))
                         (else (loop (cdr l) acc key)))))))))

(define (md-yaml-value s)
  (let* ((t (md-trim s))
         (n (string-length t)))
    (if (and (>= n 2) (in? (string-ref t 0) '(#\" #\'))
             (char=? (string-ref t (- n 1)) (string-ref t 0)))
        (substring t 1 (- n 1))
        t)))

(define (md-yaml-values s)
  ;; the values of a key: several for a list on one line, [a, b]
  (let* ((t (md-trim s))
         (n (string-length t)))
    (if (and (>= n 2) (char=? (string-ref t 0) #\[)
             (char=? (string-ref t (- n 1)) #\]))
        (map md-yaml-value
             (string-tokenize-by-char (substring t 1 (- n 1)) #\,))
        (list (md-yaml-value t)))))

(define (md-fenced lines fence)
  ;; the code up to the end of the fence
  (let loop ((l (cdr lines)) (acc '()))
    (if (or (null? l) (md-fence-end? (car l) fence))
        (let* ((info (cadddr fence))
               (e (let loop ((e 0))
                    (if (and (< e (string-length info))
                             (not (md-space? (string-ref info e))))
                        (loop (+ e 1)) e)))
               (lang (substring info 0 e)))
          (cons `(pre ,@(if (== lang "") '() `((@ (lang ,lang))))
                      ,(md-join (reverse acc) "\n"))
                (if (null? l) l (cdr l))))
        (loop (cdr l) (cons (md-dedent (car l) (caddr fence)) acc)))))

(define (md-indented lines)
  ;; code which is indented by four spaces
  (let loop ((l lines) (acc '()))
    (cond ((and (nnull? l) (>= (md-indent (car l)) 4))
           (loop (cdr l) (cons (md-dedent (car l) 4) acc)))
          ((and (nnull? l) (md-blank? (car l))
                ;; blank lines inside the code
                (let skip ((m l))
                  (cond ((null? m) #f)
                        ((md-blank? (car m)) (skip (cdr m)))
                        (else (>= (md-indent (car m)) 4)))))
           (loop (cdr l) (cons "" acc)))
          (else (cons `(pre ,(md-join (reverse acc) "\n")) l)))))

(define (md-math lines)
  ;; a formula between $$ and $$, or \[ and \], on lines of its own
  (let* ((first (md-trim (car lines)))
         (open (if (md-starts? first 0 "$$") "$$" "\\["))
         (close (if (== open "$$") "$$" "\\]"))
         (rest (substring first 2 (string-length first)))
         (e (md-search rest 0 close)))
    (if e
        ;; on one line; what follows the formula is dropped
        (cons `(displaymath ,(md-trim (substring rest 0 e))) (cdr lines))
        (let loop ((l (cdr lines)) (acc (if (md-blank? rest) '() (list rest))))
          (cond ((null? l)
                 (cons `(displaymath ,(md-join (reverse acc) "\n")) l))
                ((md-search (car l) 0 close)
                 (let* ((s (car l))
                        (e (md-search s 0 close))
                        (last (md-trim (substring s 0 e))))
                   (cons `(displaymath
                            ,(md-join (reverse (if (== last "") acc
                                                   (cons last acc))) "\n"))
                         (cdr l))))
                (else (loop (cdr l) (cons (car l) acc))))))))

(define (md-html lines)
  ;; HTML up to a blank line
  (let loop ((l lines) (acc '()))
    (if (or (null? l) (md-blank? (car l)))
        (cons `(html ,(md-join (reverse acc) "\n")) l)
        (loop (cdr l) (cons (car l) acc)))))

(define (md-blockquote lines)
  (let loop ((l lines) (acc '()))
    (cond ((and (nnull? l) (md-quote (car l)))
           (loop (cdr l) (cons (md-quote (car l)) acc)))
          ;; a line without > which continues a paragraph of the quote
          ((and (nnull? l) (nnull? acc)
                (not (md-blank? (car l))) (not (md-blank? (car acc)))
                (not (md-interrupts? (car l)))
                (not (md-fence (car acc))))
           (loop (cdr l) (cons (car l) acc)))
          (else (cons `(blockquote ,@(md-blocks (reverse acc))) l)))))

(define (md-same-list? it kind)
  (and it
       (if (in? kind '(#\. #\)))
           (char=? (car it) kind)
           (char=? (car it) kind))))

(define (md-list-item lines it)
  ;; the lines of the item which starts lines, without their indentation,
  ;; and the lines which remain
  (let* ((w (caddr it))
         (first (car lines))
         (start (if (>= w (string-length first)) ""
                    (substring first w (string-length first)))))
    (let loop ((l (cdr lines)) (acc (list start)) (blanks '()))
      (cond ((null? l) (cons (reverse acc) l))
            ((md-blank? (car l))
             (loop (cdr l) acc (cons "" blanks)))
            ((>= (md-indent (car l)) w)
             (loop (cdr l)
                   (cons (md-dedent (car l) w) (append blanks acc)) '()))
            ;; a line which continues the paragraph of the item
            ((and (null? blanks) (not (md-blank? (car acc)))
                  (not (md-interrupts? (car l)))
                  (not (md-item (car l))))
             (loop (cdr l) (cons (md-trim-left (car l)) acc) '()))
            (else (cons (reverse acc) (append (reverse blanks) l)))))))

(define (md-task item)
  ;; (checked . item) for an item of a task list, which starts with [ ] or [x]
  (and (nnull? item)
       (let ((s (car item)))
         (and (>= (string-length s) 3)
              (char=? (string-ref s 0) #\[) (char=? (string-ref s 2) #\])
              (in? (string-ref s 1) '(#\space #\x #\X))
              (or (== (string-length s) 3) (md-space? (string-ref s 3)))
              (cons (if (char=? (string-ref s 1) #\space) "false" "true")
                    (cons (md-trim-left (substring s 3 (string-length s)))
                          (cdr item)))))))

(define (md-list lines it)
  (let* ((kind (car it))
         (ordered? (in? kind '(#\. #\)))))
    (let loop ((l lines) (items '()) (loose? #f))
      (let* ((l* (let skip ((m l))
                   (if (and (nnull? m) (md-blank? (car m))) (skip (cdr m)) m)))
             (it* (and (nnull? l*) (not (md-hr? (car l*))) (md-item (car l*)))))
        (if (and it* (md-same-list? it* kind))
            (let* ((r (md-list-item l* it*))
                   (item (car r))
                   (task (md-task item))
                   (item* (if task (cdr task) item))
                   (blocks (md-blocks item*)))
              (loop (cdr r)
                    (cons `(li ,@(if task `((@ (checked ,(car task)))) '())
                               ,@blocks)
                          items)
                    (or loose?
                        ;; a blank line between two items, or inside an item
                        (and (nnull? items) (!= l l*))
                        (and (> (length blocks) 1)
                             (list-or (map md-blank? item))))))
            (let* ((start (cadddr it))
                   (attrs (append
                            (if (and ordered? start (!= start 1))
                                `((start ,(number->string start))) '())
                            (if loose? '((loose "true")) '()))))
              (cons `(,(if ordered? 'ol 'ul)
                      ,@(if (null? attrs) '() `((@ ,@attrs)))
                      ,@(reverse items))
                    l)))))))

(define (md-table lines aligns)
  ;; the header, the line under it, and the rows
  (define (cells tag l)
    (let loop ((l l) (a aligns) (acc '()))
      (if (null? a) (reverse acc)
          (loop (if (null? l) l (cdr l)) (cdr a)
                (cons `(!text ,tag
                              ,(if (== (car a) "") '() `((align ,(car a))))
                              ,(if (null? l) "" (md-unescape-pipes (car l))))
                      acc)))))
  (let loop ((l (cddr lines))
             (rows (list `(tr ,@(cells 'th (md-split-row (car lines)))))))
    (if (or (null? l) (md-blank? (car l)) (md-interrupts? (car l))
            (not (md-find (car l) 0 #\|)))
        (cons `(table ,@(reverse rows)) l)
        (loop (cdr l)
              (cons `(tr ,@(cells 'td (md-split-row (car l)))) rows)))))

(define (md-footnote lines def)
  ;; the definition of a footnote: its first line and the indented ones
  (let loop ((l (cdr lines)) (acc (list (cdr def))) (blanks '()))
    (cond ((and (nnull? l) (md-blank? (car l)))
           (loop (cdr l) acc (cons "" blanks)))
          ((and (nnull? l) (>= (md-indent (car l)) 4))
           (loop (cdr l) (cons (md-dedent (car l) 4) (append blanks acc)) '()))
          ((and (nnull? l) (null? blanks) (not (md-interrupts? (car l)))
                (not (md-footnote-def (car l))))
           (loop (cdr l) (cons (md-trim-left (car l)) acc) '()))
          (else
            (cons `(footnote-def ,(car def) ,@(md-blocks (reverse acc)))
                  (append (reverse blanks) l))))))

(define (md-paragraph lines)
  ;; a paragraph, a heading with a line under it, or a table
  (let loop ((l (cdr lines)) (acc (list (md-trim-left (car lines)))))
    (cond ((or (null? l) (md-blank? (car l)))
           (cons `(!text p () ,(md-join (reverse acc) "\n")) l))
          ((md-setext (car l))
           (cons `(!text ,(if (== (md-setext (car l)) 1) 'h1 'h2) ()
                         ,(md-trim (md-join (reverse acc) "\n")))
                 (cdr l)))
          ((and (null? (cdr acc)) (md-find (car acc) 0 #\|)
                (md-table-delim (car l))
                (== (length (md-table-delim (car l)))
                    (length (md-split-row (car acc)))))
           (md-table (cons (car acc) l) (md-table-delim (car l))))
          ((md-interrupts? (car l))
           (cons `(!text p () ,(md-join (reverse acc) "\n")) l))
          (else (loop (cdr l) (cons (md-trim-left (car l)) acc))))))

(define (md-block lines)
  ;; the block which starts the lines, and the lines which remain
  (with s (car lines)
    (cond ((md-fence s) (md-fenced lines (md-fence s)))
          ((>= (md-indent s) 4) (md-indented lines))
          ((md-atx s)
           (with h (md-atx s)
             (cons `(!text ,(string->symbol
                              (string-append "h" (number->string (car h))))
                           () ,(cdr h))
                   (cdr lines))))
          ((md-hr? s) (cons '(hr) (cdr lines)))
          ((md-quote s) (md-blockquote lines))
          ((md-item s) (md-list lines (md-item s)))
          ((md-math-start? s) (md-math lines))
          ((md-html-block? s) (md-html lines))
          ((md-footnote-def s) (md-footnote lines (md-footnote-def s)))
          ((md-ref-def s)
           (with d (md-ref-def s)
             (if (not (ahash-ref md-refs (car d)))
                 (ahash-set! md-refs (car d) (cdr d)))
             (cons #f (cdr lines))))
          (else (md-paragraph lines)))))

(define (md-blocks lines)
  (let loop ((l lines) (acc '()))
    (cond ((null? l) (reverse acc))
          ((md-blank? (car l)) (loop (cdr l) acc))
          (else
            (with r (md-block l)
              (loop (cdr r) (if (car r) (cons (car r) acc) acc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text: characters, code, mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define md-entities
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . 160) ("copy" . 169) ("reg" . 174) ("deg" . 176)
    ("plusmn" . 177) ("times" . 215) ("divide" . 247) ("ndash" . 8211)
    ("mdash" . 8212) ("lsquo" . 8216) ("rsquo" . 8217) ("ldquo" . 8220)
    ("rdquo" . 8221) ("hellip" . 8230) ("trade" . 8482) ("larr" . 8592)
    ("rarr" . 8594) ("le" . 8804) ("ge" . 8805) ("ne" . 8800)
    ("euro" . 8364) ("laquo" . 171) ("raquo" . 187) ("sect" . 167)
    ("para" . 182) ("middot" . 183) ("bull" . 8226)))

(define (md-utf8 n)
  ;; the character of code n, in UTF-8
  (define (byte x) (integer->char x))
  (cond ((< n 128) (string (byte n)))
        ((< n 2048)
         (string (byte (+ 192 (quotient n 64)))
                 (byte (+ 128 (remainder n 64)))))
        ((< n 65536)
         (string (byte (+ 224 (quotient n 4096)))
                 (byte (+ 128 (remainder (quotient n 64) 64)))
                 (byte (+ 128 (remainder n 64)))))
        (else
          (string (byte (+ 240 (quotient n 262144)))
                  (byte (+ 128 (remainder (quotient n 4096) 64)))
                  (byte (+ 128 (remainder (quotient n 64) 64)))
                  (byte (+ 128 (remainder n 64)))))))

(define (md-entity s i)
  ;; (text . end) for the entity which starts at the & at i, or #f
  (let ((e (md-find s i #\;)))
    (and e (> e (+ i 1)) (<= (- e i) 10)
         (let* ((name (substring s (+ i 1) e))
                (v (cond ((md-starts? name 0 "#x")
                          (string->number (substring name 2 (string-length name)) 16))
                         ((md-starts? name 0 "#X")
                          (string->number (substring name 2 (string-length name)) 16))
                         ((md-starts? name 0 "#")
                          (string->number (substring name 1 (string-length name))))
                         (else (assoc-ref md-entities name)))))
           (cond ((string? v) (cons v (+ e 1)))
                 ((and (integer? v) (> v 0) (< v 1114112))
                  (cons (md-utf8 v) (+ e 1)))
                 (else #f))))))

(define (md-code-span s i)
  ;; (node . end) for the code which starts with the backquotes at i
  (let* ((j (md-run s i #\`))
         (n (- j i)))
    (let loop ((k j))
      (with b (md-find s k #\`)
        (and b
             (with e (md-run s b #\`)
               (if (== (- e b) n)
                   (let* ((t (string-replace (substring s j b) "\n" " "))
                          (m (string-length t))
                          (t* (if (and (>= m 2)
                                       (char=? (string-ref t 0) #\space)
                                       (char=? (string-ref t (- m 1)) #\space)
                                       (not (md-blank? t)))
                                  (substring t 1 (- m 1)) t)))
                     (cons `(code ,t*) e))
                   (loop e))))))))

(define (md-dollar-math s i)
  ;; (node . end) for a formula which starts with the dollar at i
  (let ((n (string-length s)))
    (if (md-starts? s i "$$")
        (with e (md-search s (+ i 2) "$$")
          (and e (> e (+ i 2))
               (cons `(displaymath ,(md-trim (substring s (+ i 2) e)))
                     (+ e 2))))
        ;; a dollar which is followed by a space does not open a formula,
        ;; one which follows a space or is followed by a digit does not
        ;; close it: prices are text
        (and (< (+ i 1) n)
             (not (md-space? (string-ref s (+ i 1))))
             (not (char=? (string-ref s (+ i 1)) #\newline))
             (let loop ((j (+ i 1)))
               (cond ((>= j n) #f)
                     ((char=? (string-ref s j) #\\) (loop (+ j 2)))
                     ((char=? (string-ref s j) #\$)
                      (and (not (md-space? (string-ref s (- j 1))))
                           (not (and (< (+ j 1) n)
                                     (md-digit? (string-ref s (+ j 1)))))
                           (cons `(math ,(string-replace (substring s (+ i 1) j)
                                                         "\n" " "))
                                 (+ j 1))))
                     (else (loop (+ j 1)))))))))

(define (md-backslash-math s i)
  ;; (node . end) for a formula between \( and \) or \[ and \]
  (let* ((display? (md-starts? s i "\\["))
         (e (md-search s (+ i 2) (if display? "\\]" "\\)"))))
    (and e (not (and display? (md-literal-brackets? s i)))
         (cons (list (if display? 'displaymath 'math)
                     (md-trim (string-replace (substring s (+ i 2) e) "\n" " ")))
               (+ e 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text: links, images, HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (md-bracket-end s i)
  ;; the ] which closes the [ at i, or #f
  (let ((n (string-length s)))
    (let loop ((j (+ i 1)) (depth 1))
      (cond ((>= j n) #f)
            ((char=? (string-ref s j) #\\) (loop (+ j 2) depth))
            ((char=? (string-ref s j) #\`)
             (with c (md-code-span s j)
               (loop (if c (cdr c) (md-run s j #\`)) depth)))
            ((char=? (string-ref s j) #\[) (loop (+ j 1) (+ depth 1)))
            ((char=? (string-ref s j) #\])
             (if (== depth 1) j (loop (+ j 1) (- depth 1))))
            (else (loop (+ j 1) depth))))))

(define (md-unescape s)
  ;; without the backslashes before punctuation, and with the entities
  (let loop ((i 0) (acc '()))
    (cond ((>= i (string-length s)) (apply string-append (reverse acc)))
          ((and (char=? (string-ref s i) #\\) (< (+ i 1) (string-length s))
                (md-punctuation? (string-ref s (+ i 1))))
           (loop (+ i 2) (cons (string (string-ref s (+ i 1))) acc)))
          ((and (char=? (string-ref s i) #\&) (md-entity s i))
           (with e (md-entity s i)
             (loop (cdr e) (cons (car e) acc))))
          (else (loop (+ i 1) (cons (string (string-ref s i)) acc))))))

(define (md-link-tail s i)
  ;; (url title end) for the (url "title") which starts at i, or #f
  (let ((n (string-length s)))
    (and (md-starts? s i "(")
         (let* ((k (let skip ((k (+ i 1)))
                     (if (and (< k n) (or (md-space? (string-ref s k))
                                          (char=? (string-ref s k) #\newline)))
                         (skip (+ k 1)) k)))
                (angle? (md-starts? s k "<"))
                (e (if angle?
                       (md-find s k #\>)
                       ;; up to a space or to the parenthesis which closes
                       (let loop ((e k) (depth 0))
                         (cond ((>= e n) #f)
                               ((char=? (string-ref s e) #\\) (loop (+ e 2) depth))
                               ((char=? (string-ref s e) #\() (loop (+ e 1) (+ depth 1)))
                               ((char=? (string-ref s e) #\))
                                (if (== depth 0) e (loop (+ e 1) (- depth 1))))
                               ((or (md-space? (string-ref s e))
                                    (char=? (string-ref s e) #\newline)) e)
                               (else (loop (+ e 1) depth)))))))
           (and e
                (let* ((url (if angle? (substring s (+ k 1) e) (substring s k e)))
                       (m (let skip ((m (if angle? (+ e 1) e)))
                            (if (and (< m n) (or (md-space? (string-ref s m))
                                                 (char=? (string-ref s m) #\newline)))
                                (skip (+ m 1)) m))))
                  (cond ((>= m n) #f)
                        ((char=? (string-ref s m) #\))
                         (list (md-unescape url) "" (+ m 1)))
                        ((in? (string-ref s m) '(#\" #\' #\())
                         (let* ((close (if (char=? (string-ref s m) #\()
                                           #\) (string-ref s m)))
                                (t (let loop ((t (+ m 1)))
                                     (cond ((>= t n) #f)
                                           ((char=? (string-ref s t) #\\) (loop (+ t 2)))
                                           ((char=? (string-ref s t) close) t)
                                           (else (loop (+ t 1))))))
                                (c (and t (md-skip-spaces s (+ t 1)))))
                           (and c (md-starts? s c ")")
                                (list (md-unescape url)
                                      (md-unescape (substring s (+ m 1) t))
                                      (+ c 1)))))
                        (else #f))))))))

(define (md-plain-text l)
  ;; the text of inline nodes, for the description of an image
  (apply string-append
         (map (lambda (x)
                (cond ((string? x) x)
                      ((func? x 'br) " ")
                      ((and (pair? x) (in? (car x) '(code math displaymath)))
                       (cadr x))
                      ((func? x 'img)
                       (or (md-attr x 'alt) ""))
                      ((pair? x) (md-plain-text (md-children x)))
                      (else "")))
              l)))

(define (md-attr x name)
  (and (pair? x) (pair? (cdr x)) (func? (cadr x) '@)
       (with a (assoc name (cdadr x))
         (and a (cadr a)))))

(define (md-children x)
  (if (and (pair? (cdr x)) (func? (cadr x) '@)) (cddr x) (cdr x)))

(define (md-make-link image? inner url title)
  (if image?
      ;; the description is kept as text too: it may be a caption
      (with l (md-inlines inner)
        `(img (@ (src ,url)
                 (alt ,(md-plain-text l))
                 ,@(if (== title "") '() `((title ,title))))
              ,@l))
      `(a (@ (href ,url) ,@(if (== title "") '() `((title ,title))))
          ,@(md-inlines inner))))

(define (md-image-size x s i)
  ;; the image x with the width and the height of the attributes which
  ;; follow it at i, as for Pandoc: {width=50% height=2cm}; (node . end)
  (let* ((e (and (md-starts? s i "{") (md-find s i #\})))
         (l (if e (string-tokenize-by-char (substring s (+ i 1) e) #\space) '()))
         (get (lambda (key)
                (let loop ((l l))
                  (cond ((null? l) #f)
                        ((string-starts? (car l) key)
                         (md-yaml-value (substring (car l) (string-length key)
                                                   (string-length (car l)))))
                        (else (loop (cdr l)))))))
         (w (get "width="))
         (h (get "height=")))
    (if (not (or w h)) (cons x i)
        (cons `(img (@ ,@(cdadr x)
                       ,@(if w `((width ,w)) '())
                       ,@(if h `((height ,h)) '()))
                    ,@(cddr x))
              (+ e 1)))))

(define (md-link s i image?)
  ;; (node . end) for the link or the image whose [ is at i, or #f
  (with r (md-link-sub s i image?)
    (if (and r image? (func? (car r) 'img))
        (md-image-size (car r) s (cdr r))
        r)))

(define (md-link-sub s i image?)
  (let ((e (md-bracket-end s i)))
    (and e
         (let* ((inner (substring s (+ i 1) e))
                (n (string-length s)))
           (cond ;; a footnote
                 ((and (not image?) (md-starts? inner 0 "^")
                       (> (string-length inner) 1)
                       (not (md-find inner 0 #\space)))
                  (cons `(footnote ,(substring inner 1 (string-length inner)))
                        (+ e 1)))
                 ;; [text](url "title")
                 ((md-link-tail s (+ e 1))
                  (with t (md-link-tail s (+ e 1))
                    (cons (md-make-link image? inner (car t) (cadr t))
                          (caddr t))))
                 ;; [text][label], [text][] and [label]
                 (else
                   (let* ((e2 (and (md-starts? s (+ e 1) "[")
                                   (md-find s (+ e 1) #\])))
                          (label (if (and e2 (> e2 (+ e 2)))
                                     (substring s (+ e 2) e2)
                                     inner))
                          (def (ahash-ref md-refs (md-label label))))
                     (and def
                          (cons (md-make-link image? inner
                                              (md-unescape (car def))
                                              (md-unescape (cadr def)))
                                (if e2 (+ e2 1) (+ e 1)))))))))))

(define (md-autolink s i)
  ;; (node . end) for <scheme:address> or <address@host>, or #f
  (let ((e (md-find s i #\>)))
    (and e
         (let ((t (substring s (+ i 1) e)))
           (and (not (md-find t 0 #\space)) (not (md-find t 0 #\<))
                (cond ((let ((c (md-find t 0 #\:)))
                         (and c (>= c 2)
                              (list-and (map (lambda (x)
                                               (or (md-ascii-alpha? x) (md-digit? x)
                                                   (in? x '(#\+ #\. #\-))))
                                             (string->list (substring t 0 c))))
                              (md-ascii-alpha? (string-ref t 0))))
                       (cons `(a (@ (href ,t)) ,t) (+ e 1)))
                      ((let ((a (md-find t 0 #\@)))
                         (and a (> a 0) (md-find t a #\.)
                              (not (md-find t (+ a 1) #\@))))
                       (cons `(a (@ (href ,(string-append "mailto:" t))) ,t)
                             (+ e 1)))
                      (else #f)))))))

(define (md-bare-link s i)
  ;; (node . end) for an address which starts at i, without brackets
  (let* ((n (string-length s))
         (e (let loop ((e i))
              (if (and (< e n)
                       (not (md-space? (string-ref s e)))
                       (not (in? (string-ref s e) '(#\newline #\< #\>))))
                  (loop (+ e 1)) e)))
         ;; the punctuation which ends it is text
         (e (let loop ((e e))
              (if (and (> e i)
                       (in? (string-ref s (- e 1))
                            '(#\. #\, #\; #\: #\! #\? #\" #\' #\* #\_ #\~))
                       )
                  (loop (- e 1))
                  (if (and (> e i) (char=? (string-ref s (- e 1)) #\))
                           (not (md-find (substring s i e) 0 #\()))
                      (- e 1) e))))
         (t (substring s i e)))
    (and (> (string-length t) 8)
         (cons `(a (@ (href ,t)) ,t) e))))

(define md-void-tags
  '("br" "hr" "img" "input" "meta" "link" "wbr" "area" "base" "col" "embed"
    "source" "track"))

(define (md-html-attributes s)
  ;; the attributes of a tag of HTML, from the text after its name
  (let ((n (string-length s)))
    (let loop ((i 0) (acc '()))
      (let* ((i (let skip ((i i))
                  (if (and (< i n) (or (md-space? (string-ref s i))
                                       (in? (string-ref s i) '(#\newline #\/))))
                      (skip (+ i 1)) i)))
             (j (let name ((j i))
                  (if (and (< j n) (not (md-space? (string-ref s j)))
                           (not (in? (string-ref s j) '(#\= #\newline #\/))))
                      (name (+ j 1)) j))))
        (cond ((or (>= i n) (== j i)) (reverse acc))
              ((and (< j n) (char=? (string-ref s j) #\=))
               (let* ((q (and (< (+ j 1) n) (string-ref s (+ j 1))))
                      (quoted? (and q (in? q '(#\" #\'))))
                      (a (if quoted? (+ j 2) (+ j 1)))
                      (b (if quoted?
                             (or (md-find s a q) n)
                             (let val ((b a))
                               (if (and (< b n) (not (md-space? (string-ref s b))))
                                   (val (+ b 1)) b)))))
                 (loop (min n (if quoted? (+ b 1) b))
                       (cons (list (string->symbol (locase-all (substring s i j)))
                                   (md-unescape (substring s a b)))
                             acc))))
              (else (loop j acc)))))))

(define (md-html-image s)
  ;; the image of the tag img, from the text after its name
  (let* ((l (md-html-attributes s))
         (get (lambda (key) (with a (assoc key l) (if a (cadr a) ""))))
         (opt (lambda (key)
                (if (== (get key) "") '() `((,key ,(get key)))))))
    `(img (@ (src ,(get 'src)) (alt ,(get 'alt))
             ,@(opt 'title) ,@(opt 'width) ,@(opt 'height))
          ,@(if (== (get 'alt) "") '() (list (get 'alt))))))

(define (md-inline-html s i)
  ;; (node . end) for the tag which starts at i, with what it encloses when
  ;; it is closed further on, or #f
  (let ((n (string-length s)))
    (cond ((md-starts? s i "<!--")
           (with e (md-search s (+ i 4) "-->")
             (and e (cons `(html ,(substring s i (+ e 3))) (+ e 3)))))
          ((and (< (+ i 1) n)
                (or (md-ascii-alpha? (string-ref s (+ i 1)))
                    (and (char=? (string-ref s (+ i 1)) #\/) (< (+ i 2) n)
                         (md-ascii-alpha? (string-ref s (+ i 2))))))
           (let* ((e (md-find s i #\>))
                  (closing? (md-starts? s (+ i 1) "/"))
                  (j (let loop ((j (if closing? (+ i 2) (+ i 1))))
                       (if (and (< j n)
                                (or (md-ascii-alpha? (string-ref s j))
                                    (md-digit? (string-ref s j))
                                    (char=? (string-ref s j) #\-)))
                           (loop (+ j 1)) j)))
                  (name (locase-all (substring s (if closing? (+ i 2) (+ i 1)) j))))
             (and e
                  ;; the name is followed by a space, / or >
                  (or (== j e) (md-space? (string-ref s j))
                      (char=? (string-ref s j) #\/)
                      (char=? (string-ref s j) #\newline))
                  (not (md-find (substring s i e) 1 #\<))
                  (let* ((close (string-append "</" name ">"))
                         (c (and (not closing?) (not (in? name md-void-tags))
                                 (not (char=? (string-ref s (- e 1)) #\/))
                                 (md-search (locase-all s) (+ e 1) close))))
                    (cond
                      ;; an image is the same as ![...](...), with its sizes
                      ((and (== name "img") (not closing?))
                       (cons (md-html-image (substring s j e)) (+ e 1)))
                      (c
                        (cons `(html ,(substring s i (+ c (string-length close))))
                              (+ c (string-length close))))
                      (else
                        (cons `(html ,(substring s i (+ e 1))) (+ e 1))))))))
          (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text: emphasis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The text is first cut into tokens: strings, nodes, and the runs of *, _
;; and ~, as (!delim character length opens? closes?). The runs are then
;; matched, the closing ones in their order with the nearest opening one
;; before them, as CommonMark tells.

(define (md-delim s i)
  ;; the token for the run which starts at i, and its end
  (let* ((c (string-ref s i))
         (j (md-run s i c))
         (n (string-length s))
         (before (if (== i 0) #\space (string-ref s (- i 1))))
         (after (if (>= j n) #\space (string-ref s j)))
         (white? (lambda (x) (or (md-space? x) (char=? x #\newline))))
         (left? (and (not (white? after))
                     (or (not (md-punctuation? after))
                         (white? before) (md-punctuation? before))))
         (right? (and (not (white? before))
                      (or (not (md-punctuation? before))
                          (white? after) (md-punctuation? after))))
         (opens? (if (char=? c #\_)
                     (and left? (or (not right?) (md-punctuation? before)))
                     left?))
         (closes? (if (char=? c #\_)
                      (and right? (or (not left?) (md-punctuation? after)))
                      right?)))
    (cons (list '!delim c (- j i) opens? closes?) j)))

(define (md-delim? x) (func? x '!delim))

(define (md-delim-text x)
  (make-string (caddr x) (cadr x)))

(define (md-undelim l)
  ;; the runs which remain are text
  (map (lambda (x) (if (md-delim? x) (md-delim-text x) x)) l))

(define (md-match? open close)
  (and (char=? (cadr open) (cadr close))
       (cadddr open)
       (if (char=? (cadr open) #\~)
           ;; strike-through: two runs of the same length, one or two
           (and (== (caddr open) (caddr close)) (<= (caddr open) 2))
           ;; a run which both opens and closes does not match a run whose
           ;; length added to its own is a multiple of 3, unless both are
           (not (and (or (and (cadddr open) (car (cddddr open)))
                         (and (cadddr close) (car (cddddr close))))
                     (== (remainder (+ (caddr open) (caddr close)) 3) 0)
                     (not (and (== (remainder (caddr open) 3) 0)
                               (== (remainder (caddr close) 3) 0))))))))

(define (md-emphasis l)
  ;; matches the runs of the list of tokens
  (let loop ((before '()) (l l))
    ;; before: the tokens already seen, the nearest first
    (cond ((null? l) (md-undelim (reverse before)))
          ((and (md-delim? (car l)) (car (cddddr (car l))))
           (let* ((close (car l))
                  (found (let search ((b before) (inner '()))
                           (cond ((null? b) #f)
                                 ((and (md-delim? (car b)) (md-match? (car b) close))
                                  (list (car b) (cdr b) inner))
                                 (else (search (cdr b) (cons (car b) inner)))))))
             (if (not found)
                 ;; a run which does not open either is text
                 (loop (cons (if (cadddr close) close (md-delim-text close))
                             before)
                       (cdr l))
                 (let* ((open (car found))
                        (rest (cadr found))
                        (inner (md-undelim (caddr found)))
                        (strike? (char=? (cadr close) #\~))
                        (use (cond (strike? (caddr close))
                                   ((and (>= (caddr open) 2) (>= (caddr close) 2)) 2)
                                   (else 1)))
                        (node (cons (cond (strike? 'del)
                                          ((== use 2) 'strong)
                                          (else 'em))
                                    (md-merge inner)))
                        (open* (list '!delim (cadr open) (- (caddr open) use)
                                     (cadddr open) (car (cddddr open))))
                        (close* (list '!delim (cadr close) (- (caddr close) use)
                                      (cadddr close) (car (cddddr close))))
                        (before* (cons node
                                       (if (> (caddr open*) 0)
                                           (cons open* rest) rest))))
                   (if (> (caddr close*) 0)
                       (loop before* (cons close* (cdr l)))
                       (loop before* (cdr l)))))))
          (else (loop (cons (car l) before) (cdr l))))))

(define (md-merge l)
  ;; with the strings which follow each other as one
  (cond ((null? l) l)
        ((and (string? (car l)) (nnull? (cdr l)) (string? (cadr l)))
         (md-merge (cons (string-append (car l) (cadr l)) (cddr l))))
        ((== (car l) "") (md-merge (cdr l)))
        (else (cons (car l) (md-merge (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text: the tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (md-tokens s)
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      ;; start: where the text which is not yet in acc begins
      (define (flush) (if (< start i) (cons (substring s start i) acc) acc))
      (define (node r) (loop (cdr r) (cdr r) (cons (car r) (flush))))
      (define (text t j) (loop j j (cons t (flush))))
      (if (>= i n) (reverse (flush))
          (let ((c (string-ref s i)))
            (cond ((char=? c #\\)
                   (cond ((and (or (md-starts? s i "\\(") (md-starts? s i "\\["))
                               (md-backslash-math s i))
                          (node (md-backslash-math s i)))
                         ((md-starts? s i "\\\n")
                          (text '(br) (md-skip-spaces s (+ i 2))))
                         ((and (< (+ i 1) n) (md-punctuation? (string-ref s (+ i 1))))
                          (text (string (string-ref s (+ i 1))) (+ i 2)))
                         (else (loop (+ i 1) start acc))))
                  ((char=? c #\`)
                   (with r (md-code-span s i)
                     (if r (node r) (loop (md-run s i #\`) start acc))))
                  ((char=? c #\$)
                   (with r (md-dollar-math s i)
                     (if r (node r) (loop (+ i 1) start acc))))
                  ((char=? c #\newline)
                   ;; two spaces before the end of the line break it
                   (let* ((b (let back ((b i))
                               (if (and (> b start) (char=? (string-ref s (- b 1)) #\space))
                                   (back (- b 1)) b)))
                          (hard? (>= (- i b) 2))
                          (acc* (if (< start b) (cons (substring s start b) acc) acc))
                          (j (md-skip-spaces s (+ i 1))))
                     (loop j j (cons (if hard? '(br) " ") acc*))))
                  ((char=? c #\&)
                   (with r (md-entity s i)
                     (if r (text (car r) (cdr r)) (loop (+ i 1) start acc))))
                  ((and (char=? c #\!) (md-starts? s i "![") (md-link s (+ i 1) #t))
                   (node (md-link s (+ i 1) #t)))
                  ((char=? c #\[)
                   (with r (md-link s i #f)
                     (if r (node r) (loop (+ i 1) start acc))))
                  ((char=? c #\<)
                   (with r (or (md-autolink s i) (md-inline-html s i))
                     (if r (node r) (loop (+ i 1) start acc))))
                  ((in? c '(#\* #\_ #\~))
                   (with r (md-delim s i)
                     ;; a single ~ is text
                     (if (and (char=? c #\~) (> (caddr (car r)) 2))
                         (loop (cdr r) start acc)
                         (node r))))
                  ((and (char=? c #\h)
                        (or (md-starts? s i "http://") (md-starts? s i "https://"))
                        (or (== i 0)
                            (not (or (md-ascii-alpha? (string-ref s (- i 1)))
                                     (md-digit? (string-ref s (- i 1))))))
                        (md-bare-link s i))
                   (node (md-bare-link s i)))
                  (else (loop (+ i 1) start acc))))))))

(define (md-inlines s)
  (md-merge (md-emphasis (md-tokens s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (md-finish x)
  ;; parses the text of the blocks
  (cond ((func? x '!text)
         (with l (md-inlines (cadddr x))
           `(,(cadr x) ,@(if (null? (caddr x)) '() `((@ ,@(caddr x)))) ,@l)))
        ((and (pair? x) (in? (car x) '(pre html displaymath meta))) x)
        ((pair? x) (cons (car x) (map md-finish (cdr x))))
        (else x)))

(define (md-parse s)
  (set! md-refs (make-ahash-table))
  (let* ((lines (map md-expand-tabs (md-lines s)))
         (front (md-front-matter lines))
         (blocks (md-blocks (if front (cdr front) lines)))
         (r (map md-finish (if front (cons (car front) blocks) blocks))))
    (set! md-refs (make-ahash-table))
    r))

(tm-define (parse-markdown-document s)
  (:type (-> string stree))
  (:synopsis "Parse the Markdown document @s into a Markdown tree")
  `(!file (markdown ,@(md-parse s))))

(tm-define (parse-markdown-snippet s)
  (:type (-> string stree))
  (:synopsis "Parse the Markdown text @s into a Markdown tree")
  (with l (md-parse s)
    ;; a snippet of one paragraph is text
    (if (and (list-1? l) (func? (car l) 'p))
        `(markdown ,@(cdar l))
        `(markdown ,@l))))
