
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
;;            (footnote-def "label" block...)  (ref-def "text")
;;   inlines: "text"  (em ...)  (strong ...)  (del ...)  (code "text")
;;            (a ...)  (img)  (br)  (math "latex")  (displaymath "latex")
;;            (html "text")  (footnote "label")
;;
;; A node may have attributes, (tag (@ (name "value")...) ...): start and
;; loose for the lists, marker for those whose items start with * or +, checked for the items of task lists, lang for pre
;; (and info, for all the words after the fence when there are several),
;; source for meta (the lines of the header as they are written), for a
;; table, and for a formula which is not written $latex$ or, on lines of
;; its own, between two lines $$,
;; align for th and td, href and title for a, src, alt, title, width and
;; height for img (the sizes as in HTML or CSS: 300, 50%, 2cm). The
;; children of img are its description, of which alt is the plain text.
;;
;; The blocks are parsed first, the text of the paragraphs, headings and
;; cells afterwards: a link may refer to a definition which follows it. The
;; definitions are inlined in the links; one to which nothing refers, as
;; the comments [//]: # (text), is a ref-def with its line.

(texmacs-module (convert markdown markdownin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings and lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are called for each character of a text: they compare the codes
;; of the characters with <, which every interpreter does fast.
;; A tab at the start of a line goes to the next multiple of 4 columns for
;; the blocks (indentation, lists, indented code), and is kept as a tab in
;; what the blocks hold (code, HTML, formulas, tables). Such a tab of width
;; n is read as n characters: md-tab-head, then n - 1 times md-tab-fill,
;; which the blocks count as spaces. A block which takes some of these
;; columns away leaves fills without their head: spaces, as CommonMark
;; says. At the end, a head with its fills is a tab again (md-untab). The
;; two characters are control characters which the text does not hold
;; (when it does, a tab at the start of a line is spaces, as before).
(define md-tab-head #\space)
(define md-tab-fill #\space)
(define md-tabs? #f)

(define (md-space? c)
  (or (eqv? c #\space) (eqv? c #\tab)
      (eqv? c md-tab-fill) (eqv? c md-tab-head)))
(define (md-digit? c)
  (with n (char->integer c) (and (< 47 n) (< n 58))))
(define (md-hex-digit? c)
  (or (md-digit? c) (in? (char-downcase c) '(#\a #\b #\c #\d #\e #\f))))
(define (md-ascii-alpha? c)
  (with n (char->integer c)
    (or (and (< 96 n) (< n 123)) (and (< 64 n) (< n 91)))))
(define (md-punctuation? c)
  ;; ASCII, and neither a letter, a digit, a space nor a control character
  (with n (char->integer c)
    (and (< 32 n) (< n 128)
         (not (or (and (< 47 n) (< n 58)) (and (< 64 n) (< n 91))
                  (and (< 96 n) (< n 123)))))))

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
    (if (and (< i (string-length s))
             (with c (string-ref s i)
               (or (char=? c #\space) (char=? c md-tab-fill)
                   (char=? c md-tab-head))))
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

(define (md-find-unescaped s i c)
  ;; the first c from i on which has no backslash before it, or #f
  (let loop ((j i))
    (cond ((>= j (string-length s)) #f)
          ((char=? (string-ref s j) #\\) (loop (+ j 2)))
          ((char=? (string-ref s j) c) j)
          (else (loop (+ j 1))))))

(define (md-search s i what)
  (with r (string-search-forwards what i s)
    (and (>= r 0) r)))

(define (md-code-end s i n)
  ;; the end of the run of n backquotes which closes code, from i on, or #f
  (let loop ((k i))
    (with b (md-find s k #\`)
      (and b
           (with e (md-run s b #\`)
             (if (== (- e b) n) e (loop e)))))))

;; What is known of the text which is cut into tokens (see md-tokens), so
;; that a search which fails is made once and not again from each of the
;; characters which follow: the strings which do not occur any more, the
;; characters which close no title of a link any more, the lengths of the
;; runs of backquotes which close no code any more, and for the position
;; of a [ the one of its ], or 0 when it has none.
(define md-none #f)

(define (md-known key)
  (and md-none (ahash-ref md-none key)))

(define (md-note! key val)
  (if (not md-none) (set! md-none (make-ahash-table)))
  (ahash-set! md-none key val))

(define (md-search* s i what)
  ;; as md-search, in the text which is cut into tokens
  (and (not (md-known what))
       (or (md-search s i what)
           (begin (md-note! what #t) #f))))

(define (md-code-runs s)
  ;; the runs of backquotes of the text: for each length, the positions
  ;; where the runs of this length start, in a vector
  (with t (make-ahash-table)
    (let loop ((i 0))
      (with b (md-find s i #\`)
        (if b
            (with e (md-run s b #\`)
              (ahash-set! t (- e b) (cons b (or (ahash-ref t (- e b)) '())))
              (loop e)))))
    (for-each (lambda (p)
                (ahash-set! t (car p) (list->vector (reverse (cdr p)))))
              (ahash-table->list t))
    t))

(define (md-code-end* s i n)
  ;; as md-code-end, in the text which is cut into tokens and for i which
  ;; is not inside a run: the runs are looked for once, and not again to
  ;; the end of the text for each run which nothing closes
  (let* ((t (or (md-known "`")
                (with t (md-code-runs s) (md-note! "`" t) t)))
         (v (ahash-ref t n)))
    (and v
         ;; the first run which starts at i or after it
         (let loop ((a 0) (b (vector-length v)))
           (if (< a b)
               (with m (quotient (+ a b) 2)
                 (if (< (vector-ref v m) i) (loop (+ m 1) b) (loop a m)))
               (and (< a (vector-length v)) (+ (vector-ref v a) n)))))))

(define (md-expand-tabs s)
  ;; the tabs at the start of the line as as many columns as they take, to
  ;; the next multiple of 4 (see md-tab-head)
  (if (or (== s "") (not (md-space? (string-ref s 0)))
          (not (md-find s 0 #\tab)))
      s
      (let loop ((i 0) (col 0) (acc '()))
        (cond ((>= i (string-length s)) (list->string (reverse acc)))
              ((char=? (string-ref s i) #\space)
               (loop (+ i 1) (+ col 1) (cons #\space acc)))
              ((char=? (string-ref s i) #\tab)
               (set! md-tabs? (not (char=? md-tab-head #\space)))
               (with n (- 4 (remainder col 4))
                 (loop (+ i 1) (+ col n)
                       (append (make-list (- n 1) md-tab-fill)
                               (cons md-tab-head acc)))))
              (else (string-append (list->string (reverse acc))
                                   (substring s i (string-length s))))))))

(define (md-untab-string s)
  ;; a head with its fills is a tab, and fills alone are spaces
  (if (not (or (md-find s 0 md-tab-head) (md-find s 0 md-tab-fill))) s
      (let loop ((l (string->list s)) (head? #f) (acc '()))
        (cond ((null? l) (list->string (reverse acc)))
              ((char=? (car l) md-tab-head) (loop (cdr l) #t (cons #\tab acc)))
              ((char=? (car l) md-tab-fill)
               (loop (cdr l) head? (if head? acc (cons #\space acc))))
              (else (loop (cdr l) #f (cons (car l) acc)))))))

(define (md-untab x)
  ;; the same in each string of the tree x (a list of many blocks is not
  ;; gone through with a call for each of them)
  (cond ((string? x) (md-untab-string x))
        ((list? x) (map md-untab x))
        ((pair? x) (cons (md-untab (car x)) (md-untab (cdr x))))
        (else x)))

(define (md-lines s)
  ;; the lines of the text, without their ends
  (if (>= (string-search-forwards "\r" 0 s) 0)
      (md-lines-any s)
      ;; the usual case, ends of lines of Unix: from one to the next. The
      ;; search takes a time which grows with the length of its string: it
      ;; is made in pieces of the text
      (let ((n (string-length s)))
        (let piece ((a 0) (part "") (acc '()))
          ;; part: the start of a line which the pieces before did not end
          (if (>= a n)
              (reverse (if (== part "") acc (cons part acc)))
              (let* ((b (min n (+ a 8192)))
                     (t (substring s a b)))
                (let loop ((start 0) (part part) (acc acc))
                  (with i (string-search-forwards "\n" start t)
                    (if (>= i 0)
                        (loop (+ i 1) ""
                              (cons (if (== part "") (substring t start i)
                                        (string-append part
                                                       (substring t start i)))
                                    acc))
                        (piece b
                               (string-append part (substring t start (- b a)))
                               acc))))))))))

(define (md-lines-any s)
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
                         "textarea" "thead" "title" "tr" "ul" "video"))))))

(define (md-literal-brackets? s i e)
  ;; whether the \[ at i, which the \] at e closes, holds plain text and not
  ;; a formula: \[2\], \[sic!\], \[see Smith (2010)\] are the escaped brackets
  ;; which other programs write. A text has none of the characters of the
  ;; formulas, and a word of two letters or more, or no letter at all
  (and e
       (let loop ((j (+ i 2)) (run 0) (word? #f) (letter? #f))
         (if (>= j e) (or word? (not letter?))
             (let ((c (string-ref s j)))
               (cond ((or (md-ascii-alpha? c) (< 127 (char->integer c)))
                      (loop (+ j 1) (+ run 1) (or word? (> run 0)) #t))
                     ((in? c '(#\\ #\^ #\_ #\= #\+ #\< #\> #\{ #\} #\|)) #f)
                     (else (loop (+ j 1) 0 word? letter?))))))))

(define (md-math-start? s)
  ;; does the line start with what opens a formula, $$ or \[ ?
  (let ((t (md-trim s)))
    (or (md-starts? t 0 "$$")
        (and (md-starts? t 0 "\\[")
             (not (md-literal-brackets? t 0 (md-search t 2 "\\]")))))))

(define (md-split-row s)
  ;; the cells of a row of a table; the bars of code are text, as the
  ;; backslashes of code are
  (let* ((t (md-trim s))
         (t (if (md-starts? t 0 "|") (substring t 1 (string-length t)) t))
         (n (string-length t))
         (t (if (and (> n 0) (char=? (string-ref t (- n 1)) #\|)
                     (not (and (> n 1) (char=? (string-ref t (- n 2)) #\\))))
                (substring t 0 (- n 1)) t)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i (string-length t))
             (reverse (cons (md-trim (substring t start i)) acc)))
            ((and (char=? (string-ref t i) #\\) (< (+ i 1) (string-length t)))
             (loop (+ i 2) start acc))
            ((char=? (string-ref t i) #\`)
             (let* ((j (md-run t i #\`))
                    (e (md-code-end t j (- j i))))
               (loop (or e j) start acc)))
            ((char=? (string-ref t i) #\|)
             (loop (+ i 1) (+ i 1)
                   (cons (md-trim (substring t start i)) acc)))
            (else (loop (+ i 1) start acc))))))

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
  ;; (label url title) for the definition of a link, [label]: url "title".
  ;; The address has no space, or is between < and >, and nothing but a
  ;; title between quotes or parentheses follows it: [Note]: see below is
  ;; the text of a paragraph
  (let* ((i (md-indent s)))
    (and (<= i 3) (md-starts? s i "[") (not (md-starts? s i "[^"))
         ;; (a ] after a backslash is text: [1\]: x is a paragraph)
         (let ((j (md-find-unescaped s (+ i 1) #\])))
           (and j (md-starts? s j "]:") (> j (+ i 1))
                (let* ((k (md-skip-spaces s (+ j 2)))
                       (n (string-length s))
                       (angle? (md-starts? s k "<"))
                       (e (if angle?
                              (md-find s k #\>)
                              (let loop ((e k))
                                (if (and (< e n) (not (md-space? (string-ref s e))))
                                    (loop (+ e 1)) e)))))
                  (and e
                       (let* ((url (if angle?
                                       (substring s (+ k 1) e)
                                       (substring s k e)))
                              (rest (md-trim (substring s (if angle? (+ e 1) e)
                                                        n)))
                              (m (string-length rest))
                              (title? (and (>= m 2)
                                           (in? (string
                                                  (string-ref rest 0)
                                                  (string-ref rest (- m 1)))
                                                '("\"\"" "''" "()")))))
                         (and (!= url "")
                              (or (== rest "") title?)
                              (list (md-label (substring s (+ i 1) j)) url
                                    (if title? (substring rest 1 (- m 1))
                                        "")))))))))))

(define (md-ref-def-lines lines)
  ;; (definition . lines which remain) for the definition of a link which
  ;; starts the lines: its address may be on the next line, and its title
  ;; on the line after the address
  (let* ((one (md-ref-def (car lines)))
         (next (lambda (l)
                 (and (nnull? l) (not (md-blank? (car l))) (car l))))
         (join (lambda (n)
                 (with l (list-head (append lines '("" "")) n)
                   (and (list-and (map (lambda (x) (not (md-blank? x))) l))
                        (md-ref-def (md-join (map md-trim l) " ")))))))
    (cond ((and (not one) (md-starts? (md-trim (car lines)) 0 "[")
                (string-ends? (md-trim (car lines)) "]:") (next (cdr lines)))
           ;; [label]: on its own line
           (let ((two (join 2))
                 (three (and (nnull? (cdr lines)) (next (cddr lines)) (join 3))))
             (cond ((and three (!= (caddr three) "")) (cons three (cdddr lines)))
                   (two (cons two (cddr lines)))
                   (else #f))))
          ((and one (== (caddr one) "") (next (cdr lines)))
           ;; a title on the next line
           (with two (join 2)
             (if (and two (!= (caddr two) "")) (cons two (cddr lines))
                 (cons one (cdr lines)))))
          (one (cons one (cdr lines)))
          (else #f))))

(define (md-footnote-def s)
  ;; (label . text) for the definition of a footnote, [^label]: text
  (let* ((i (md-indent s)))
    (and (<= i 3) (md-starts? s i "[^")
         (let ((j (md-find-unescaped s (+ i 2) #\])))
           (and j (md-starts? s j "]:") (> j (+ i 2))
                (cons (substring s (+ i 2) j)
                      (md-trim-left (substring s (+ j 2) (string-length s)))))))))

(define (md-interrupts? s)
  ;; does the line end the paragraph before it? (A formula which starts it
  ;; does so when it ends: see md-interrupts*)
  (or (md-atx s) (md-fence s) (md-quote s) (md-hr? s) (md-html-block? s)
      (with it (md-item s)
        (and it
             ;; not an empty item, and an ordered list starts with 1
             (< (caddr it) (+ (string-length s) 1))
             (not (md-blank? (substring s (min (string-length s)
                                               (- (caddr it) 1))
                                        (string-length s))))
             (or (not (cadddr it)) (== (cadddr it) 1))))))

(define (md-lazy-after? s)
  ;; can a line without the marks of its quote or its item follow the line
  ;; s? Only as the text of a paragraph: not after a fence, a heading, a
  ;; rule
  (not (or (md-blank? s) (md-fence s) (md-atx s) (md-hr? s))))

(define (md-fence-state fence s)
  ;; the fence of code which is open after the line s, if fence was before
  (cond ((not fence) (md-fence s))
        ((md-fence-end? s fence) #f)
        (else fence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the definitions of the links and of the footnotes of the document, and
;; the labels of those to which a link refers
(define md-refs (make-ahash-table))
(define md-refs-used (make-ahash-table))

;; the labels to which a table refers: a table is written back as it was
;; read while it is not changed, and the definitions which it uses must
;; then be written too
(define md-refs-kept (make-ahash-table))
(define md-in-table? #f)

;; A block whose text is not parsed yet is (!text tag attributes "text"), and
;; the definition of a link (!def label first? "text"), first? when it is
;; the first one of its label; the functions below return the blocks and the
;; lines which remain.

;; How many quotes and lists the blocks which are parsed are in: beyond
;; md-max-depth their marks are text, as nothing is written that deep and
;; each level is a call inside the previous one
(define md-depth 0)

;; whether a snippet is parsed, and not a file
(define md-snippet? #f)
(define md-max-depth 100)

(define (md-yaml-key? s)
  ;; does the line start with a key of YAML, as in title: The title? The
  ;; key is any text, "og:image" or a word with accents, before a colon which
  ;; ends the line or is followed by a space: the place of this colon
  (let ((n (string-length s)))
    (and (> n 0) (not (md-space? (string-ref s 0)))
         (not (in? (string-ref s 0) '(#\- #\# #\:)))
         (let loop ((i 1))
           (and (< i n)
                (if (and (char=? (string-ref s i) #\:)
                         (or (== (+ i 1) n) (md-space? (string-ref s (+ i 1)))))
                    i
                    (loop (+ i 1))))))))

(define (md-yaml-line? s)
  ;; can the line be in a YAML header? A key, a comment, an item of a list,
  ;; a line which is indented (the rest of a value) or a blank line
  (or (== s "") (md-yaml-key? s) (md-starts? s 0 "#") (md-starts? s 0 "- ")
      (md-space? (string-ref s 0))))

(define (md-yaml? lines)
  ;; are the lines a YAML header and what follows it, up to a line --- or
  ;; ... ? Each of its lines is one which YAML has (see md-yaml-line?), and
  ;; one at least is a key: other lines between two rules are text, and
  ;; # Title between them is a heading
  (let loop ((l lines) (key? #f))
    (and (nnull? l)
         (let ((s (md-trim-right (car l))))
           (cond ((in? s '("---" "...")) key?)
                 ((md-yaml-line? s) (loop (cdr l) (or key? (md-yaml-key? s))))
                 (else #f))))))

(define (md-lines-until l end)
  ;; the lines of l before its tail end
  (let loop ((l l) (acc '()))
    (if (or (null? l) (eq? l end)) (reverse acc)
        (loop (cdr l) (cons (car l) acc)))))

(define (md-front-matter lines)
  ;; the YAML header, as (meta (key "value")...), or #f; a snippet has none
  ;; (its rules and its text are kept)
  (and (not md-snippet?) (nnull? lines) (== (md-trim-right (car lines)) "---")
       (md-yaml? (cdr lines))
       (let loop ((l (cdr lines)) (acc '()) (key #f))
         (cond ((null? l) #f)
               ((in? (md-trim-right (car l)) '("---" "..."))
                (cons `(meta (@ (source ,(md-join (md-lines-until (cdr lines) l)
                                                  "\n")))
                             ,@(reverse acc))
                      (cdr l)))
               ((md-starts? (car l) 0 "#") (loop (cdr l) acc key))
               (else
                 (let* ((s (car l))
                        (c (md-yaml-key? s))
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
  ;; a value of YAML without its quotes: between " a backslash escapes a
  ;; backslash or a quote, between ' two quotes are one
  (let* ((t (md-trim s))
         (n (string-length t))
         (q (and (>= n 2) (char=? (string-ref t (- n 1)) (string-ref t 0))
                 (string-ref t 0))))
    (cond ((eqv? q #\")
           (let loop ((i 1) (acc '()))
             (cond ((>= i (- n 1)) (list->string (reverse acc)))
                   ((and (char=? (string-ref t i) #\\) (< (+ i 2) n)
                         (in? (string-ref t (+ i 1)) '(#\\ #\")))
                    (loop (+ i 2) (cons (string-ref t (+ i 1)) acc)))
                   (else (loop (+ i 1) (cons (string-ref t i) acc))))))
          ((eqv? q #\') (string-replace (substring t 1 (- n 1)) "''" "'"))
          (else t))))

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
          (cons `(pre ,@(if (== lang "") '()
                            `((@ (lang ,lang)
                                 ,@(if (== (md-trim info) lang) '()
                                       `((info ,(md-trim info)))))))
                      ,(md-join (reverse acc) "\n"))
                (if (null? l) l (cdr l))))
        (loop (cdr l) (cons (md-dedent (car l) (caddr fence)) acc)))))

(define (md-indented lines)
  ;; code which is indented by four spaces
  (let loop ((l lines) (acc '()))
    (cond ((and (nnull? l) (>= (md-indent (car l)) 4))
           (loop (cdr l) (cons (md-dedent (car l) 4) acc)))
          ((and (nnull? l) (md-blank? (car l)))
           ;; blank lines inside the code, when code follows them: they
           ;; are looked at once
           (let skip ((m l) (acc* acc))
             (cond ((and (nnull? m) (md-blank? (car m)))
                    (skip (cdr m) (cons "" acc*)))
                   ((and (nnull? m) (>= (md-indent (car m)) 4))
                    (loop m acc*))
                   (else (cons `(pre ,(md-join (reverse acc) "\n")) l)))))
          (else (cons `(pre ,(md-join (reverse acc) "\n")) l)))))

;; The formulas which do not end: for what closes them, the lines (the
;; tails of the list of lines) from which it was searched and not found.
;; A formula which starts on one of these lines does not end either, and
;; the lines are not searched again from each of them.
(define md-no-math-end '())

;; tables keyed by the identity of pairs, in Guile (hashq) and in S7
(define (md-eq-table)
  (if (defined? 'hashq-set!) (make-hash-table 64) (make-hash-table 64 eq?)))
(define (md-eq-ref t key)
  (if (defined? 'hashq-ref) (hashq-ref t key #f) (hash-table-ref t key)))
(define (md-eq-set! t key)
  (if (defined? 'hashq-set!) (hashq-set! t key #t) (hash-table-set! t key #t)))

(define (md-math lines)
  ;; a formula between $$ and $$, or \[ and \], on its line or on lines of
  ;; its own; the text which follows its end is a line of those which
  ;; remain. A formula holds no blank line: #f when it does not end before
  ;; one, and what opens it is text then
  (let* ((first (md-trim (car lines)))
         (close (if (md-starts? first 0 "$$") "$$" "\\]"))
         (none (assoc-ref md-no-math-end close)))
    (and (not (and none (md-eq-ref none lines)))
      (let loop ((s (substring first 2 (string-length first)))
                 (l (cdr lines)) (acc '()))
        (with e (md-search s 0 close)
          (cond (e (let* ((last (md-trim (substring s 0 e)))
                          (after (md-trim (substring s (+ e 2)
                                                     (string-length s)))))
                     (let* ((tex (md-join (reverse (if (== last "") acc
                                                       (cons last acc))) "\n"))
                            (raw (md-join (md-lines-until lines l) "\n")))
                       (cons `(displaymath
                                ,@(if (or (!= after "")
                                          (== raw (string-append "$$\n" tex
                                                                 "\n$$")))
                                      '() `((@ (source ,raw))))
                                ,tex)
                             (if (== after "") l (cons after l))))))
                ((or (null? l) (md-blank? (car l)))
                 (with t (or none (md-eq-table))
                   (let mark ((m lines))
                     (when (and (pair? m) (not (eq? m l)))
                       (md-eq-set! t m)
                       (mark (cdr m))))
                   (if (not none)
                       (set! md-no-math-end
                             (cons (cons close t) md-no-math-end))))
                 #f)
                (else (loop (car l) (cdr l)
                            (if (md-blank? s) acc (cons s acc))))))))))

;; the ends of HTML which the blocks that are parsed do not have any more:
;; "-->", or "raw" for the closing tags of md-raw-tags
(define md-no-html-end '())

;; the tags of HTML whose text is kept up to their closing tag, also when it
;; holds blank lines
(define md-raw-tags '("pre" "script" "style" "textarea"))

(define (md-raw-start? s)
  ;; does the text s start with one of these tags?
  (with t (locase-all s)
    (list-or (map (lambda (name)
                    (let ((k (+ (string-length name) 1)))
                      (and (md-starts? t 0 (string-append "<" name))
                           (or (== k (string-length t))
                               (in? (string-ref t k) '(#\space #\tab #\>))))))
                  md-raw-tags))))

(define (md-raw-end? s)
  (with t (locase-all s)
    (list-or (map (lambda (name) (md-search t 0 (string-append "</" name ">")))
                  md-raw-tags))))

(define (md-html lines)
  ;; HTML up to a blank line; a comment up to the line where it ends, and
  ;; so for <pre>, <script>, <style> and <textarea>. The spaces before the
  ;; block are not kept: they would make an item of a list of it, once the
  ;; list before it is written with another indentation
  (let* ((first (md-trim-left (car lines)))
         (lines (cons first (cdr lines)))
         (kind (cond ((md-starts? first 0 "<!--") "-->")
                     ((md-raw-start? first) "raw")
                     (else #f)))
         (end? (if (== kind "-->") (lambda (s) (md-search s 0 "-->")) md-raw-end?))
         (closed? (and kind (not (in? kind md-no-html-end))
                       (or (list-find lines end?)
                           (begin (set! md-no-html-end (cons kind md-no-html-end))
                                  #f)))))
    (let loop ((l lines) (acc '()))
      (cond ((and closed? (nnull? l) (end? (car l)))
             (cons `(html ,(md-join (reverse (cons (car l) acc)) "\n"))
                   (cdr l)))
            ((or (null? l) (and (not closed?) (md-blank? (car l))))
             (cons `(html ,(md-join (reverse acc) "\n")) l))
            (else (loop (cdr l) (cons (car l) acc)))))))

(define (md-interrupts* l)
  ;; does the first line of l end the paragraph before it? $$ or \[ do when
  ;; the formula which they open ends: I paid\n$$5 is one paragraph
  (or (md-interrupts? (car l))
      (and (md-math-start? (car l)) (md-math l) #t)))

(define (md-blockquote lines)
  ;; fence: the code between fences which the lines of the quote leave open,
  ;; which a line without > does not continue
  (let loop ((l lines) (acc '()) (fence #f))
    (cond ((and (nnull? l) (md-quote (car l)))
           (with q (md-quote (car l))
             (loop (cdr l) (cons q acc) (md-fence-state fence q))))
          ;; a line without > which continues a paragraph of the quote
          ((and (nnull? l) (nnull? acc) (not fence)
                (not (md-blank? (car l))) (md-lazy-after? (car acc))
                (not (md-interrupts* l)))
           (loop (cdr l) (cons (car l) acc) fence))
          (else (cons `(blockquote ,@(md-blocks (reverse acc))) l)))))

(define (md-same-list? it kind)
  (and it (char=? (car it) kind)))

(define (md-list-item lines it)
  ;; the lines of the item which starts lines, without their indentation,
  ;; and the lines which remain
  (let* ((w (caddr it))
         (first (car lines))
         (start (if (>= w (string-length first)) ""
                    (substring first w (string-length first)))))
    ;; fence: the code between fences which the lines of the item leave
    ;; open, which a line that is not indented does not continue
    (let loop ((l (cdr lines)) (acc (list start)) (blanks '())
               (fence (md-fence start)))
      (cond ((null? l) (cons (reverse acc) l))
            ((md-blank? (car l))
             (loop (cdr l) acc (cons "" blanks) fence))
            ((>= (md-indent (car l)) w)
             (with t (md-dedent (car l) w)
               (loop (cdr l) (cons t (append blanks acc)) '()
                     (md-fence-state fence t))))
            ;; a line which continues the paragraph of the item
            ((and (null? blanks) (not fence) (md-lazy-after? (car acc))
                  (not (md-interrupts* l))
                  (not (md-item (car l))))
             (loop (cdr l) (cons (md-trim-left (car l)) acc) '() fence))
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
                   (blocks (md-blocks item*))
                   (gap? md-gap?))
              (loop (cdr r)
                    (cons `(li ,@(if task `((@ (checked ,(car task)))) '())
                               ,@blocks)
                          items)
                    (or loose?
                        ;; a blank line between two items, or between two
                        ;; blocks of an item
                        (and (nnull? items) (!= l l*))
                        gap?)))
            (let* ((start (cadddr it))
                   (attrs (append
                            (if (and ordered? start (!= start 1))
                                `((start ,(number->string start))) '())
                            (if loose? '((loose "true")) '())
                            ;; (a list with dashes has none)
                            (if (in? kind '(#\* #\+))
                                `((marker ,(string kind))) '()))))
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
    (if (or (null? l) (md-blank? (car l)) (md-interrupts* l)
            (not (md-find (car l) 0 #\|)))
        (cons `(table (@ (source ,(md-join (md-lines-until lines l) "\n")))
                      ,@(reverse rows))
              l)
        (loop (cdr l)
              (cons `(tr ,@(cells 'td (md-split-row (car l)))) rows)))))

(define (md-footnote lines def)
  ;; the definition of a footnote: its first line and the indented ones
  (let loop ((l (cdr lines)) (acc (list (cdr def))) (blanks '())
             (fence (md-fence (cdr def))))
    (cond ((and (nnull? l) (md-blank? (car l)))
           (loop (cdr l) acc (cons "" blanks) fence))
          ((and (nnull? l) (>= (md-indent (car l)) 4))
           (with t (md-dedent (car l) 4)
             (loop (cdr l) (cons t (append blanks acc)) '()
                   (md-fence-state fence t))))
          ((and (nnull? l) (null? blanks) (not fence) (not (md-interrupts* l))
                (not (md-footnote-def (car l))))
           (loop (cdr l) (cons (md-trim-left (car l)) acc) '() fence))
          (else
            (cons `(footnote-def ,(car def) ,@(md-blocks (reverse acc)))
                  (append (reverse blanks) l))))))

(define (md-table-start head s)
  ;; the alignments of the columns of the table whose header is the line
  ;; head and whose next line is s, or #f
  (and (md-find head 0 #\|)
       (with a (md-table-delim s)
         (and a (== (length a) (length (md-split-row head))) a))))

(define (md-paragraph lines)
  ;; a paragraph, a heading with a line under it, or a table
  (let loop ((l (cdr lines)) (acc (list (md-trim-left (car lines)))))
    (cond ((or (null? l) (md-blank? (car l)))
           (cons `(!text p () ,(md-trim-right (md-join (reverse acc) "\n"))) l))
          ((md-setext (car l))
           (cons `(!text ,(if (== (md-setext (car l)) 1) 'h1 'h2) ()
                         ,(md-trim (md-join (reverse acc) "\n")))
                 (cdr l)))
          ((md-table-start (car acc) (car l))
           => (lambda (aligns)
                (if (null? (cdr acc))
                    (md-table (cons (car acc) l) aligns)
                    ;; the lines before the header are a paragraph
                    (cons `(!text p () ,(md-trim-right (md-join (reverse (cdr acc)) "\n")))
                          (cons (car acc) l)))))
          ((md-interrupts* l)
           (cons `(!text p () ,(md-trim-right (md-join (reverse acc) "\n"))) l))
          (else (loop (cdr l) (cons (md-trim-left (car l)) acc))))))

(define (md-nested thunk)
  (set! md-depth (+ md-depth 1))
  (with r (thunk)
    (set! md-depth (- md-depth 1))
    r))

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
          ((and (md-quote s) (< md-depth md-max-depth))
           (md-nested (lambda () (md-blockquote lines))))
          ((and (md-item s) (< md-depth md-max-depth))
           (md-nested (lambda () (md-list lines (md-item s)))))
          ((and (md-math-start? s) (md-math lines)) => identity)
          ((md-html-block? s) (md-html lines))
          ((and (md-footnote-def s) (< md-depth md-max-depth))
           (md-nested (lambda () (md-footnote lines (md-footnote-def s)))))
          ((md-ref-def-lines lines)
           => (lambda (r)
                ;; the first definition of a label is the one which counts
                (let* ((d (car r))
                       (first? (not (ahash-ref md-refs (car d)))))
                  (if first? (ahash-set! md-refs (car d) (cdr d)))
                  (cons `(!def ,(car d) ,first?
                               ,(md-join (map md-trim (md-lines-until lines (cdr r)))
                                         "\n"))
                        (cdr r)))))
          (else (md-paragraph lines)))))

;; whether the last blocks which were parsed had a blank line between two of
;; them: an item of a list with such a line makes the list loose
(define md-gap? #f)

(define (md-blocks lines)
  (let ((old md-no-html-end)
        (old-math md-no-math-end))
    (set! md-no-html-end '())
    (set! md-no-math-end '())
    ;; blank?: blank lines after the last block, gap?: such lines before a
    ;; block (and not those inside a block, as code or a list)
    (let loop ((l lines) (acc '()) (blank? #f) (gap? #f))
      (cond ((null? l)
             (set! md-no-html-end old)
             (set! md-no-math-end old-math)
             (set! md-gap? gap?)
             (reverse acc))
            ((md-blank? (car l)) (loop (cdr l) acc (nnull? acc) gap?))
            (else
              (with r (md-block l)
                (if (car r)
                    (loop (cdr r) (cons (car r) acc) #f (or gap? blank?))
                    (loop (cdr r) acc blank? gap?))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text: characters, code, mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the entities of HTML5 which md-entities does not have
(lazy-define (convert markdown markdown-entities) markdown-html5-entity)

(define md-entities
  ;; the entities of HTML 4, which are those that texts use: the letters of
  ;; Latin-1, Greek, the signs of typography and of mathematics
  (let ((t (make-ahash-table))
        (from (lambda (start names)
                (let loop ((l (string-tokenize-by-char names #\space)) (n start)
                           (acc '()))
                  (if (null? l) acc
                      (loop (cdr l) (+ n 1)
                            (if (== (car l) "-") acc
                                (cons (cons (car l) n) acc))))))))
    (for-each
      (lambda (p) (ahash-set! t (car p) (cdr p)))
      (append
        '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"")
          ("apos" . "'"))
        (from 160 (string-append
                    "nbsp iexcl cent pound curren yen brvbar sect uml copy "
                    "ordf laquo not shy reg macr deg plusmn sup2 sup3 acute "
                    "micro para middot cedil sup1 ordm raquo frac14 frac12 "
                    "frac34 iquest Agrave Aacute Acirc Atilde Auml Aring AElig "
                    "Ccedil Egrave Eacute Ecirc Euml Igrave Iacute Icirc Iuml "
                    "ETH Ntilde Ograve Oacute Ocirc Otilde Ouml times Oslash "
                    "Ugrave Uacute Ucirc Uuml Yacute THORN szlig agrave aacute "
                    "acirc atilde auml aring aelig ccedil egrave eacute ecirc "
                    "euml igrave iacute icirc iuml eth ntilde ograve oacute "
                    "ocirc otilde ouml divide oslash ugrave uacute ucirc uuml "
                    "yacute thorn yuml"))
        (from 913 (string-append
                    "Alpha Beta Gamma Delta Epsilon Zeta Eta Theta Iota Kappa "
                    "Lambda Mu Nu Xi Omicron Pi Rho - Sigma Tau Upsilon Phi "
                    "Chi Psi Omega"))
        (from 945 (string-append
                    "alpha beta gamma delta epsilon zeta eta theta iota kappa "
                    "lambda mu nu xi omicron pi rho sigmaf sigma tau upsilon "
                    "phi chi psi omega"))
        (from 8194 (string-append
                     "ensp emsp - - - - - thinsp - - zwnj zwj lrm rlm - - - "
                     "ndash mdash - - - lsquo rsquo sbquo - ldquo rdquo bdquo - "
                     "dagger Dagger bull - - - hellip - - - - - - - - - permil - "
                     "prime Prime - - - - - lsaquo rsaquo - - - oline"))
        (from 8592 "larr uarr rarr darr harr")
        (from 8656 "lArr uArr rArr dArr hArr")
        '(("OElig" . 338) ("oelig" . 339) ("Scaron" . 352) ("scaron" . 353)
          ("Yuml" . 376) ("fnof" . 402) ("circ" . 710) ("tilde" . 732)
          ("thetasym" . 977) ("upsih" . 978) ("piv" . 982) ("frasl" . 8260)
          ("euro" . 8364) ("image" . 8465) ("weierp" . 8472) ("real" . 8476)
          ("trade" . 8482) ("alefsym" . 8501) ("crarr" . 8629)
          ("forall" . 8704) ("part" . 8706) ("exist" . 8707) ("empty" . 8709)
          ("nabla" . 8711) ("isin" . 8712) ("notin" . 8713) ("ni" . 8715)
          ("prod" . 8719) ("sum" . 8721) ("minus" . 8722) ("lowast" . 8727)
          ("radic" . 8730) ("prop" . 8733) ("infin" . 8734) ("ang" . 8736)
          ("and" . 8743) ("or" . 8744) ("cap" . 8745) ("cup" . 8746)
          ("int" . 8747) ("there4" . 8756) ("sim" . 8764) ("cong" . 8773)
          ("asymp" . 8776) ("ne" . 8800) ("equiv" . 8801) ("le" . 8804)
          ("ge" . 8805) ("sub" . 8834) ("sup" . 8835) ("nsub" . 8836)
          ("sube" . 8838) ("supe" . 8839) ("oplus" . 8853) ("otimes" . 8855)
          ("perp" . 8869) ("sdot" . 8901) ("lceil" . 8968) ("rceil" . 8969)
          ("lfloor" . 8970) ("rfloor" . 8971) ("lang" . 9001) ("rang" . 9002)
          ("loz" . 9674) ("spades" . 9824) ("clubs" . 9827) ("hearts" . 9829)
          ("diams" . 9830))))
    t))

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
  ;; (text . end) for the entity which starts at the & at i, or #f; its
  ;; name is short (the longest one of HTML5 has 31 letters)
  (let* ((m (min (string-length s) (+ i 34)))
         (e (let loop ((j (+ i 1)))
              (cond ((>= j m) #f)
                    ((char=? (string-ref s j) #\;) j)
                    (else (loop (+ j 1)))))))
    (and e (> e (+ i 1))
         (let* ((name (substring s (+ i 1) e))
                ;; (the digits only: string->number would read 1e3 too)
                (number (lambda (from digit? radix)
                          (with t (substring name from (string-length name))
                            (and (!= t "") (list-and (map digit? (string->list t)))
                                 (string->number t radix)))))
                (v (cond ((or (md-starts? name 0 "#x") (md-starts? name 0 "#X"))
                          (number 2 md-hex-digit? 16))
                         ((md-starts? name 0 "#") (number 1 md-digit? 10))
                         ((ahash-ref md-entities name) => identity)
                         ;; the other entities of HTML5, whose table is
                         ;; loaded for the first one only
                         ((list-and (map (lambda (c) (or (md-ascii-alpha? c)
                                                         (md-digit? c)))
                                         (string->list name)))
                          (markdown-html5-entity name))
                         (else #f))))
           (cond ((string? v) (cons v (+ e 1)))
                 ((pair? v) (cons (apply string-append (map md-utf8 v)) (+ e 1)))
                 ;; (not the half of a pair of UTF-16)
                 ((and (integer? v) (> v 0) (< v 1114112)
                       (not (and (>= v 55296) (<= v 57343))))
                  (cons (md-utf8 v) (+ e 1)))
                 (else #f))))))

(define (md-code-span s i)
  ;; (node . end) for the code which starts with the backquotes at i
  (let* ((j (md-run s i #\`))
         (e (md-code-end* s j (- j i))))
    (and e
         (let* ((t (string-replace (substring s j (- e (- j i))) "\n" " "))
                (m (string-length t))
                (t* (if (and (>= m 2)
                             (char=? (string-ref t 0) #\space)
                             (char=? (string-ref t (- m 1)) #\space)
                             (not (md-blank? t)))
                        (substring t 1 (- m 1)) t)))
           (cons `(code ,t*) e)))))

(define (md-math-node tag mark tex raw)
  ;; a formula of the text, with its source when it is not written between
  ;; two marks
  `(,tag ,@(if (== raw (string-append mark tex mark)) '()
               `((@ (source ,raw))))
         ,tex))

(define (md-dollar-math s i)
  ;; (node . end) for a formula which starts with the dollar at i
  (let ((n (string-length s)))
    (if (md-starts? s i "$$")
        (with e (md-search* s (+ i 2) "$$")
          (and e (> e (+ i 2))
               (cons (md-math-node 'displaymath "$$"
                                   (md-trim (substring s (+ i 2) e))
                                   (substring s i (+ e 2)))
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
                           (cons (md-math-node
                                   'math "$"
                                   (string-replace (substring s (+ i 1) j)
                                                   "\n" " ")
                                   (substring s i (+ j 1)))
                                 (+ j 1))))
                     (else (loop (+ j 1)))))))))

(define (md-backslash-math s i)
  ;; (node . end) for a formula between \( and \) or \[ and \]
  (let* ((display? (md-starts? s i "\\["))
         (e (md-search* s (+ i 2) (if display? "\\]" "\\)"))))
    (and e (not (and display? (md-literal-brackets? s i e)))
         (cons (md-math-node
                 (if display? 'displaymath 'math) (if display? "$$" "$")
                 (md-trim (string-replace (substring s (+ i 2) e) "\n" " "))
                 (substring s i (+ e 2)))
               (+ e 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text: links, images, HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (md-bracket-end s i)
  ;; the ] which closes the [ at i, or #f. The brackets which are met on the
  ;; way are noted with their ends: they are asked for next
  (let ((n (string-length s))
        (known (md-known i)))
    (if known (and (> known 0) known)
        (let loop ((j (+ i 1)) (open '()))
          ;; open: the [ after i which are not closed yet, the last one first
          (cond ((>= j n)
                 (for-each (lambda (k) (md-note! k 0)) (cons i open))
                 #f)
                ((char=? (string-ref s j) #\\) (loop (+ j 2) open))
                ((char=? (string-ref s j) #\`)
                 (let* ((k (md-run s j #\`))
                        (e (md-code-end* s k (- k j))))
                   (loop (or e k) open)))
                ((char=? (string-ref s j) #\[) (loop (+ j 1) (cons j open)))
                ((char=? (string-ref s j) #\])
                 (if (null? open) j
                     (begin
                       (md-note! (car open) j)
                       (loop (+ j 1) (cdr open)))))
                (else (loop (+ j 1) open)))))))

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

(define (md-angle-end s i)
  ;; the > which ends an address between < and >, from i on, or #f: such an
  ;; address holds no end of line and no other <, so that the text is not
  ;; searched to its end for each [x](< of a paragraph
  (let ((n (string-length s)))
    (let loop ((j i))
      (cond ((>= j n) #f)
            ((char=? (string-ref s j) #\\) (loop (+ j 2)))
            ((char=? (string-ref s j) #\>) j)
            ((in? (string-ref s j) '(#\< #\newline)) #f)
            (else (loop (+ j 1)))))))

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
                       (md-angle-end s (+ k 1))
                       ;; up to a space or to the parenthesis which closes;
                       ;; an address has few parentheses inside each other
                       (let loop ((e k) (depth 0))
                         (cond ((>= e n) #f)
                               ((char=? (string-ref s e) #\\) (loop (+ e 2) depth))
                               ((char=? (string-ref s e) #\()
                                (and (< depth 32) (loop (+ e 1) (+ depth 1))))
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
                                (t (and (not (md-known close))
                                        (let loop ((t (+ m 1)))
                                          (cond ((>= t n) (md-note! close #t) #f)
                                                ((char=? (string-ref s t) #\\) (loop (+ t 2)))
                                                ((char=? (string-ref s t) close) t)
                                                (else (loop (+ t 1)))))))
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

;; whether the text of a link is parsed: an address in it is text
(define md-in-link? #f)

(define (md-unlink l)
  ;; the nodes l without their links: a link holds no other link
  (append-map (lambda (x)
                (cond ((func? x 'a) (md-unlink (md-children x)))
                      ((and (pair? x) (in? (car x) '(em strong del)))
                       (list (cons (car x) (md-merge (md-unlink (cdr x))))))
                      (else (list x))))
              l))

(define (md-make-link image? inner url title)
  (if image?
      ;; the description is kept as text too: it may be a caption
      (with l (md-inlines inner)
        `(img (@ (src ,url)
                 (alt ,(md-plain-text l))
                 ,@(if (== title "") '() `((title ,title))))
              ,@l))
      (with old md-in-link?
        (set! md-in-link? #t)
        (with l (md-merge (md-unlink (md-inlines inner)))
          (set! md-in-link? old)
          `(a (@ (href ,url) ,@(if (== title "") '() `((title ,title))))
              ,@l)))))

(define (md-image-size x s i)
  ;; the image x with the width and the height of the attributes which
  ;; follow it at i, as for Pandoc: {width=50% height=2cm}; (node . end)
  (let* ((e (and (md-starts? s i "{") (md-search* s i "}")))
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

(define (md-footnote-label? s)
  ;; ^ and a label without space: a bracket in it is the start of a link,
  ;; which is not looked for further
  (let loop ((i 1))
    (or (>= i (string-length s))
        (and (not (in? (string-ref s i) '(#\space #\[)))
             (loop (+ i 1))))))

(define (md-link-sub s i image?)
  (let ((e (md-bracket-end s i)))
    (and e
         (let* ((inner (substring s (+ i 1) e))
                (n (string-length s)))
           (cond ;; a footnote
                 ((and (not image?) (md-starts? inner 0 "^")
                       (> (string-length inner) 1)
                       (md-footnote-label? inner))
                  (cons `(footnote ,(substring inner 1 (string-length inner)))
                        (+ e 1)))
                 ;; [text](url "title")
                 ((md-link-tail s (+ e 1))
                  => (lambda (t)
                       (cons (md-make-link image? inner (car t) (cadr t))
                             (caddr t))))
                 ;; [text][label], [text][] and [label]
                 (else
                   (let* ((e2 (and (md-starts? s (+ e 1) "[")
                                   (md-search* s (+ e 1) "]")))
                          (label (if (and e2 (> e2 (+ e 2)))
                                     (substring s (+ e 2) e2)
                                     inner))
                          ;; a label is short
                          (def (and (< (string-length label) 1000)
                                    (ahash-ref md-refs (md-label label)))))
                     (if def (ahash-set! (if md-in-table? md-refs-kept md-refs-used)
                                         (md-label label) #t))
                     (and def
                          (cons (md-make-link image? inner
                                              (md-unescape (car def))
                                              (md-unescape (cadr def)))
                                (if e2 (+ e2 1) (+ e 1)))))))))))

(define (md-tag-end s i)
  ;; the > which ends what the < at i starts, with no < before it, or #f
  (let ((n (string-length s)))
    (let loop ((j (+ i 1)))
      (cond ((>= j n) #f)
            ((char=? (string-ref s j) #\>) j)
            ((char=? (string-ref s j) #\<) #f)
            (else (loop (+ j 1)))))))

(define (md-autolink s i)
  ;; (node . end) for <scheme:address> or <address@host>, or #f
  (let ((e (md-tag-end s i)))
    (and e
         (let ((t (substring s (+ i 1) e)))
           (and (not (md-find t 0 #\space))
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

(define (md-plain-image? s)
  ;; has the tag img no other attributes than those of an image of
  ;; Markdown? Else it is kept as HTML, with its style or its class
  (list-and (map (lambda (a) (in? (car a) '(src alt title width height)))
                 (md-html-attributes s))))

(define (md-html-image s)
  ;; the image of the tag img, from the text after its name
  (let* ((l (md-html-attributes s))
         (get (lambda (key) (with a (assoc key l) (if a (cadr a) ""))))
         (opt (lambda (key)
                (if (== (get key) "") '() `((,key ,(get key)))))))
    `(img (@ (src ,(get 'src)) (alt ,(get 'alt))
             ,@(opt 'title) ,@(opt 'width) ,@(opt 'height))
          ,@(if (== (get 'alt) "") '() (list (get 'alt))))))

(define (md-lower s)
  ;; the text which is cut into tokens, in lowercase
  (or (md-known 'lower)
      (with t (locase-all s)
        (md-note! 'lower t)
        t)))

(define (md-inline-html s i)
  ;; (node . end) for the tag which starts at i, with what it encloses when
  ;; it is closed further on, or #f
  (let ((n (string-length s)))
    (cond ((md-starts? s i "<!--")
           (with e (md-search* s (+ i 4) "-->")
             (and e (cons `(html ,(substring s i (+ e 3))) (+ e 3)))))
          ((and (< (+ i 1) n)
                (or (md-ascii-alpha? (string-ref s (+ i 1)))
                    (and (char=? (string-ref s (+ i 1)) #\/) (< (+ i 2) n)
                         (md-ascii-alpha? (string-ref s (+ i 2))))))
           (let* ((e (md-tag-end s i))
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
                  (let* ((close (string-append "</" name ">"))
                         (c (and (not closing?) (not (in? name md-void-tags))
                                 (not (char=? (string-ref s (- e 1)) #\/))
                                 (md-search* (md-lower s) (+ e 1) close))))
                    (cond
                      ;; an image is the same as ![...](...), with its sizes
                      ((and (== name "img") (not closing?)
                            (md-plain-image? (substring s j e)))
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
;; and ~, as (!delim character length opens? closes? length), with the
;; length which remains of the run and the one which it had first. The runs
;; are then matched, the closing ones in their order with the nearest
;; opening one before them, as CommonMark tells.

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
    (cons (list '!delim c (- j i) opens? closes? (- j i)) j)))

(define (md-delim? x) (func? x '!delim))

(define (md-delim-text x)
  (make-string (caddr x) (cadr x)))

(define (md-undelim l)
  ;; the runs which remain are text
  (map (lambda (x) (if (md-delim? x) (md-delim-text x) x)) l))

(define (md-delim-length x) (cadr (cddddr x)))

(define (md-match? open close)
  (and (char=? (cadr open) (cadr close))
       (cadddr open)
       (if (char=? (cadr open) #\~)
           ;; strike-through: two runs of the same length, one or two
           (and (== (caddr open) (caddr close)) (<= (caddr open) 2))
           ;; a run which both opens and closes does not match a run whose
           ;; length added to its own is a multiple of 3, unless both are:
           ;; the lengths which the runs had first
           (let ((m (md-delim-length open))
                 (n (md-delim-length close)))
             (not (and (or (car (cddddr open)) (cadddr close))
                       (== (remainder (+ m n) 3) 0)
                       (not (and (== (remainder m 3) 0)
                                 (== (remainder n 3) 0)))))))))

(define (md-delim-kind x)
  ;; a number below 18 for what md-match? asks of the closing run x: its
  ;; character, whether it opens too, and its length modulo 3
  (+ (cond ((char=? (cadr x) #\*) 0) ((char=? (cadr x) #\_) 6) (else 12))
     (if (cadddr x) 3 0)
     (remainder (if (char=? (cadr x) #\~) (caddr x) (md-delim-length x)) 3)))

(define (md-emphasis l)
  ;; matches the runs of the list of tokens
  (let ((bottom (make-vector 18 0)))
    ;; bottom: for each kind of closing run, the number of the first tokens
    ;; seen among which no run opens it: they are not searched again
    (let loop ((before '()) (depth 0) (l l))
      ;; before: the depth tokens already seen, the nearest first
      (cond ((null? l) (md-undelim (reverse before)))
            ((and (md-delim? (car l)) (car (cddddr (car l))))
             (let* ((close (car l))
                    (kind (md-delim-kind close))
                    (low (vector-ref bottom kind))
                    (found (let search ((b before) (d depth) (inner '()))
                             (cond ((<= d low) #f)
                                   ((and (md-delim? (car b))
                                         (md-match? (car b) close))
                                    (list (car b) (cdr b) inner d))
                                   (else (search (cdr b) (- d 1)
                                                 (cons (car b) inner)))))))
               (if (not found)
                   (begin
                     (vector-set! bottom kind depth)
                     ;; a run which does not open either is text
                     (loop (cons (if (cadddr close) close (md-delim-text close))
                                 before)
                           (+ depth 1) (cdr l)))
                   (let* ((open (car found))
                          (rest (cadr found))
                          (inner (md-undelim (caddr found)))
                          (under (- (cadddr found) 1))
                          (strike? (char=? (cadr close) #\~))
                          (use (cond (strike? (caddr close))
                                     ((and (>= (caddr open) 2) (>= (caddr close) 2)) 2)
                                     (else 1)))
                          (node (cons (cond (strike? 'del)
                                            ((== use 2) 'strong)
                                            (else 'em))
                                      (md-merge inner)))
                          (open* (list '!delim (cadr open) (- (caddr open) use)
                                       (cadddr open) (car (cddddr open))
                                       (md-delim-length open)))
                          (close* (list '!delim (cadr close) (- (caddr close) use)
                                        (cadddr close) (car (cddddr close))
                                        (md-delim-length close)))
                          (open? (> (caddr open*) 0))
                          (before* (cons node (if open? (cons open* rest) rest)))
                          (depth* (+ under (if open? 2 1))))
                     ;; the tokens under the opening run are as they were
                     (let lower ((k 0))
                       (when (< k 18)
                         (if (> (vector-ref bottom k) under)
                             (vector-set! bottom k under))
                         (lower (+ k 1))))
                     (if (> (caddr close*) 0)
                         (loop before* depth* (cons close* (cdr l)))
                         (loop before* depth* (cdr l)))))))
            (else (loop (cons (car l) before) (+ depth 1) (cdr l)))))))

(define (md-merge l)
  ;; with the strings which follow each other as one
  (let loop ((l l) (run '()) (acc '()))
    ;; run: the strings just before l, the last one first
    (if (and (nnull? l) (string? (car l)))
        (loop (cdr l) (cons (car l) run) acc)
        (let* ((s (cond ((null? run) "")
                        ((null? (cdr run)) (car run))
                        (else (apply string-append (reverse run)))))
               (acc* (if (== s "") acc (cons s acc))))
          (if (null? l) (reverse acc*)
              (loop (cdr l) '() (cons (car l) acc*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text: the tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define md-token-start
  ;; the characters at which something else than text may start, as a
  ;; table: most characters of a text are passed with one look at it
  (with v (make-vector 256 #f)
    (for-each (lambda (c) (vector-set! v (char->integer c) #t))
              (string->list "\\`$\n&![<*_~h"))
    v))

(define (md-next-token s i n)
  ;; the first position from i on where a token may start, or n
  (let loop ((i i))
    (if (and (< i n)
             (not (vector-ref md-token-start
                              (char->integer (string-ref s i)))))
        (loop (+ i 1))
        i)))

(define (md-tokens s)
  ;; what is known of the text is the one of s for this time
  (with old md-none
    (set! md-none #f)
    (with r (md-tokens-sub s)
      (set! md-none old)
      r)))

(define (md-tokens-sub s)
  (let ((n (string-length s)))
    (let loop ((from 0) (start 0) (acc '()))
      ;; start: where the text which is not yet in acc begins
      (define i (md-next-token s from n))
      (define (flush) (if (< start i) (cons (substring s start i) acc) acc))
      (define (node r) (loop (cdr r) (cdr r) (cons (car r) (flush))))
      (define (text t j) (loop j j (cons t (flush))))
      (if (>= i n) (reverse (flush))
          (let ((c (string-ref s i)))
            (cond ((char=? c #\\)
                   (cond ((and (or (md-starts? s i "\\(") (md-starts? s i "\\["))
                               (md-backslash-math s i))
                          => node)
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
                   => node)
                  ((char=? c #\[)
                   (with r (md-link s i #f)
                     (if r (node r) (loop (+ i 1) start acc))))
                  ((char=? c #\<)
                   (with r (or (md-autolink s i) (md-inline-html s i))
                     (if r (node r) (loop (+ i 1) start acc))))
                  ((in? c '(#\* #\_ #\~))
                   (with r (md-delim s i)
                     ;; more than two ~ are text
                     (if (and (char=? c #\~) (> (caddr (car r)) 2))
                         (loop (cdr r) start acc)
                         (node r))))
                  ((and (char=? c #\h) (not md-in-link?)
                        (or (md-starts? s i "http://") (md-starts? s i "https://"))
                        (or (== i 0)
                            (not (or (md-ascii-alpha? (string-ref s (- i 1)))
                                     (md-digit? (string-ref s (- i 1))))))
                        (md-bare-link s i))
                   => node)
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
        ((and (pair? x) (in? (car x) '(pre html displaymath meta !def))) x)
        ((func? x 'table)
         (set! md-in-table? #t)
         (with r (cons (car x) (map md-finish (cdr x)))
           (set! md-in-table? #f)
           r))
        ((pair? x) (cons (car x) (map md-finish (cdr x))))
        (else x)))

(define (md-unused-defs x)
  ;; once the text is parsed: the definitions of links to which nothing
  ;; refers are (ref-def "text"), and the others are inlined in the links;
  ;; those to which a table refers stay too
  (if (not (and (pair? x) (in? (car x) '(markdown blockquote ul ol li
                                         footnote-def))))
      x
      (cons (car x)
            (append-map (lambda (y)
                          (cond ((not (func? y '!def 3)) (list (md-unused-defs y)))
                                ((and (caddr y) (ahash-ref md-refs-used (cadr y))
                                      (not (ahash-ref md-refs-kept (cadr y))))
                                 '())
                                (else `((ref-def ,(cadddr y))))))
                        (cdr x)))))

;; the mark of byte order which some programs write at the start of a file
;; of UTF-8: it is not text
(define md-bom
  (list->string (map integer->char '(239 187 191))))

(define (md-parse s)
  (set! md-refs (make-ahash-table))
  (set! md-refs-used (make-ahash-table))
  (set! md-refs-kept (make-ahash-table))
  (set! md-in-table? #f)
  (set! md-none #f)
  (set! md-in-link? #f)
  (set! md-depth 0)
  (set! md-no-math-end '())
  ;; (the characters for the tabs which start lines, see md-tab-head)
  (set! md-tabs? #f)
  (if (or (string-index s (integer->char 30)) (string-index s (integer->char 31)))
      (begin (set! md-tab-head #\space) (set! md-tab-fill #\space))
      (begin (set! md-tab-head (integer->char 30))
             (set! md-tab-fill (integer->char 31))))
  (let* ((s (if (md-starts? s 0 md-bom) (substring s 3 (string-length s)) s))
         (lines (map md-expand-tabs (md-lines s)))
         (front (md-front-matter lines))
         (blocks (md-blocks (if front (cdr front) lines)))
         (r (map md-finish (if front (cons (car front) blocks) blocks)))
         (r (cdr (md-unused-defs (cons 'markdown r))))
         (r (if md-tabs? (md-untab r) r)))
    (set! md-refs (make-ahash-table))
    (set! md-refs-used (make-ahash-table))
    (set! md-refs-kept (make-ahash-table))
    r))

(tm-define (parse-markdown-document s)
  (:type (-> string stree))
  (:synopsis "Parse the Markdown document @s into a Markdown tree")
  (set! md-snippet? #f)
  `(!file (markdown ,@(md-parse s))))

(tm-define (parse-markdown-snippet s)
  (:type (-> string stree))
  (:synopsis "Parse the Markdown text @s into a Markdown tree")
  (with l (begin (set! md-snippet? #t)
                 (with r (md-parse s) (set! md-snippet? #f) r))
    ;; a snippet of one paragraph is text
    (if (and (list-1? l) (func? (car l) 'p))
        `(markdown ,@(cdar l))
        `(markdown ,@l))))
