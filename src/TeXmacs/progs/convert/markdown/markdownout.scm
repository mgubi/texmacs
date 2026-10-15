
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
  (if (> (string-length s) 8192) (mdout-split-long s)
      (let loop ((start 0) (acc '()))
        (with i (string-search-forwards "\n" start s)
          (if (< i 0)
              (reverse (cons (substring s start (string-length s)) acc))
              (loop (+ i 1) (cons (substring s start i) acc)))))))

(define (mdout-split-long s)
  ;; the search takes a time which grows with the length of its string: a
  ;; long one is cut into pieces first
  (let ((n (string-length s)))
    (let piece ((a 0) (part "") (acc '()))
      ;; part: the start of a line which the pieces before did not end
      (if (>= a n) (reverse (cons part acc))
          (let* ((b (min n (+ a 8192)))
                 (l (mdout-split (substring s a b))))
            (let loop ((l (cdr l)) (part (string-append part (car l))) (acc acc))
              (if (null? l) (piece b part acc)
                  (loop (cdr l) (car l) (cons part acc)))))))))

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

(define (mdout-digit? c)
  (with n (char->integer c) (and (< 47 n) (< n 58))))

(define (mdout-punctuation? c)
  ;; ASCII, and neither a letter, a digit, a space nor a control character
  (with n (char->integer c)
    (and (< 32 n) (< n 128)
         (not (or (and (< 47 n) (< n 58)) (and (< 64 n) (< n 91))
                  (and (< 96 n) (< n 123)))))))

(define (mdout-starts? s i what)
  (let ((n (string-length what)))
    (and (<= (+ i n) (string-length s))
         (string=? (substring s i (+ i n)) what))))

(define (mdout-code-end s i n)
  ;; the end of the run of n backquotes which closes code, from i on, or #f
  (let loop ((k i) (run 0))
    (cond ((>= k (string-length s)) (and (== run n) k))
          ((char=? (string-ref s k) #\`) (loop (+ k 1) (+ run 1)))
          ((== run n) k)
          (else (loop (+ k 1) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only the characters which would be taken for markup are escaped: a text
;; with a backslash before each punctuation is hard to read. A [ is never
;; escaped: \[ opens a formula for the programs which read LaTeX so, as the
;; parser of markdownin.scm does. What would make markup of it is escaped
;; in its place: the ] of a link or of a task, the ^ of a footnote.

(define mdout-special
  ;; the characters which may have to be escaped, as a table: most
  ;; characters of a text are passed with one look at it
  (with v (make-vector 256 #f)
    (for-each (lambda (c) (vector-set! v (char->integer c) #t))
              (string->list "\\`$*]^_<&~|:"))
    v))

(define (mdout-escape? s i n cell?)
  ;; is the special character at i taken for markup?
  (let* ((c (string-ref s i))
         (prev (and (> i 0) (string-ref s (- i 1))))
         (next (and (< (+ i 1) n) (string-ref s (+ i 1)))))
    (cond ((in? c '(#\\ #\` #\$)) #t)
          ;; between spaces, * is not emphasis
          ((char=? c #\*)
           (not (and prev next (char=? prev #\space) (char=? next #\space))))
          ;; brackets make nothing without what follows them, a link, or
          ;; what they hold, a task or a footnote
          ((char=? c #\])
           (or (and next (in? next '(#\( #\:)))
               (and (> i 1) (in? prev '(#\space #\x #\X))
                    (char=? (string-ref s (- i 2)) #\[))
               (mdout-ref-label? s i)))
          ((char=? c #\^) (and prev (char=? prev #\[)))
          ;; inside a word, _ is not emphasis
          ((char=? c #\_)
           (not (and prev next (mdout-alnum? prev) (mdout-alnum? next))))
          ((char=? c #\<)
           (and next (or (char-alphabetic? next) (in? next '(#\/ #\! #\?)))))
          ((char=? c #\&)
           (and next (or (char-alphabetic? next) (char=? next #\#))))
          ;; one ~ is enough for strike-through, or for a subscript
          ((char=? c #\~)
           (not (and prev next (char=? prev #\space) (char=? next #\space))))
          ((char=? c #\|) cell?)
          ;; not escaped, but :// tells that the text may hold an address
          ((char=? c #\:)
           (if (and next (char=? next #\/)) (set! mdout-address? #t))
           #f)
          (else #f))))

;; the labels of the definitions of links which the text keeps: [label] in
;; the text would be a link to them
(define mdout-ref-labels '())

(define (mdout-label s)
  ;; a label as it is compared: in lowercase, with one space for several
  (let loop ((l (string->list (string-trim-spaces (locase-all s))))
             (acc '()) (sp? #f))
    (cond ((null? l) (list->string (reverse acc)))
          ((in? (car l) '(#\space #\tab #\newline))
           (loop (cdr l) (if sp? acc (cons #\space acc)) #t))
          (else (loop (cdr l) (cons (car l) acc) #f)))))

(define (mdout-ref-label? s i)
  ;; does the ] at i close a [ which holds one of these labels?
  (and (nnull? mdout-ref-labels)
       (let loop ((j (- i 1)))
         (cond ((< j 0) #f)
               ((char=? (string-ref s j) #\]) #f)
               ((char=? (string-ref s j) #\[)
                (in? (mdout-label (substring s (+ j 1) i)) mdout-ref-labels))
               (else (loop (- j 1)))))))

(define (mdout-collect-labels x)
  ;; the labels of the definitions (ref-def "[label]: address") in x, also
  ;; of those which the text of a file keeps (as HTML of a block)
  (cond ((and (pair? x) (in? (car x) '(ref-def html)))
         (let* ((t (string-trim-spaces
                     (mdout-join (list-filter (cdr x) string?) "")))
                (b (string-search-forwards "]:" 0 t)))
           (if (and (string-starts? t "[") (not (string-starts? t "[^"))
                    (> b 1))
               (list (mdout-label (substring t 1 b)))
               '())))
        ((and (pair? x) (in? (car x) '(!file markdown blockquote ul ol li
                                       footnote-def)))
         (append-map mdout-collect-labels (mdout-children x)))
        (else '())))

;; whether the text of a link is written: an address in it stays text
(define mdout-in-link? #f)
;; whether :/ was seen in the text which is escaped
(define mdout-address? #f)

(define (mdout-address-start s i)
  ;; where the first address http://... or https://... starts in s, from i
  ;; on, or #f
  (with j (string-search-forwards "http" i s)
    (cond ((< j 0) #f)
          ((and (or (mdout-starts? s j "http://") (mdout-starts? s j "https://"))
                (or (== j 0)
                    (not (or (char-alphabetic? (string-ref s (- j 1)))
                             (mdout-digit? (string-ref s (- j 1)))))))
           j)
          (else (mdout-address-start s (+ j 4))))))

(define (mdout-address-end s i)
  ;; where the address which starts at i ends, for the parser (see
  ;; md-bare-link): at a space, without the punctuation which ends it
  (let* ((n (string-length s))
         (e (let loop ((e i))
              (if (and (< e n)
                       (not (in? (string-ref s e)
                                 '(#\space #\tab #\newline #\< #\>))))
                  (loop (+ e 1)) e)))
         (e (let loop ((e e))
              (if (and (> e i)
                       (in? (string-ref s (- e 1))
                            '(#\. #\, #\; #\: #\! #\? #\" #\' #\* #\_ #\~)))
                  (loop (- e 1)) e))))
    (if (and (> e i) (char=? (string-ref s (- e 1)) #\))
             (not (string-index (substring s i e) #\()))
        (- e 1) e)))

(define (mdout-escape-links s cell?)
  ;; an address in a text is a link for those who read it, with all its
  ;; characters: it is written between < and >, where nothing is escaped
  (let ((n (string-length s)))
    (let loop ((start 0) (from 0) (acc '()))
      ;; start: where the text which is not yet in acc begins
      (let* ((i (mdout-address-start s from))
             (e (and i (mdout-address-end s i))))
        (cond ((not i)
               (apply string-append
                      (reverse (cons (mdout-escape-text (substring s start n)
                                                        cell?)
                                     acc))))
              ((or (<= (- e i) 8)
                   (and cell? (string-index (substring s i e) #\|)))
               (loop start (+ i 4) acc))
              (else
                (loop e e
                      (cons* ">" (substring s i e) "<"
                             (mdout-escape-text (substring s start i) cell?)
                             acc))))))))

(define (mdout-escape s cell?)
  (with r (mdout-escape-text s cell?)
    (if (not mdout-address?) r
        (begin
          (set! mdout-address? #f)
          (if (and (not mdout-in-link?) (mdout-address-start s 0))
              (mdout-escape-links s cell?)
              r)))))

(define (mdout-escape-text s cell?)
  ;; the text between two escaped characters is copied in one piece
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n)
             (if (null? acc) s
                 (apply string-append
                        (reverse (cons (substring s start n) acc)))))
            ((and (vector-ref mdout-special (char->integer (string-ref s i)))
                  (mdout-escape? s i n cell?))
             (loop (+ i 1) i (cons* "\\" (substring s start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (mdout-escape-start s)
  ;; a line of a paragraph, which must not start as another block
  (let* ((n (string-length s))
         ;; (the spaces and tabs which start the line would make code of it)
         (i (let loop ((i 0))
              (if (and (< i n) (in? (string-ref s i) '(#\space #\tab)))
                  (loop (+ i 1)) i)))
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
          ;; a line of -, : and | under a line with a | makes a table
          ((and (in? (string-ref t 0) '(#\- #\: #\|))
                (let loop ((j 0))
                  (or (>= j m)
                      (and (in? (string-ref t j) '(#\- #\: #\| #\space))
                           (loop (+ j 1)))))
                (string-index t #\-)
                (or (string-index t #\|) (string-index t #\:)))
           (string-append "\\" t))
          ((char-numeric? (string-ref t 0))
           (let loop ((j 0))
             (cond ((and (< j m) (char-numeric? (string-ref t j))) (loop (+ j 1)))
                   ((and (< j m) (in? (string-ref t j) '(#\. #\))) (follows? (+ j 1)))
                    (string-append (substring t 0 j) "\\" (substring t j m)))
                   (else t))))
          (else t))))

(define (mdout-code s)
  ;; between more backquotes than it holds, and between two spaces when it
  ;; starts or ends with a backquote, or with a space at each end, of which
  ;; those who read it drop one
  (let* ((n (+ (mdout-longest-run s #\`) 1))
         (q (make-string n #\`))
         (m (string-length s))
         (pad? (and (> m 0)
                    (or (char=? (string-ref s 0) #\`)
                        (char=? (string-ref s (- m 1)) #\`)
                        (and (char=? (string-ref s 0) #\space)
                             (char=? (string-ref s (- m 1)) #\space)
                             (!= (string-trim-spaces s) ""))))))
    (string-append q (if pad? " " "") s (if pad? " " "") q)))

(define (mdout-literal s)
  ;; the text of an address or of a title, which the reader takes the
  ;; backslashes before punctuation and the entities away from
  (let ((n (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n)
             (if (null? acc) s
                 (apply string-append (reverse (cons (substring s start n) acc)))))
            ;; (a backslash at the end would be before the ) or the " which
            ;; ends the address or the title)
            ((and (char=? (string-ref s i) #\\)
                  (or (== (+ i 1) n) (mdout-punctuation? (string-ref s (+ i 1)))))
             (loop (+ i 1) (+ i 1) (cons* "\\\\" (substring s start i) acc)))
            ((and (char=? (string-ref s i) #\&)
                  (let ((e (string-index (substring s i (min n (+ i 34))) #\;)))
                    (and e (> e 1))))
             (loop (+ i 1) (+ i 1) (cons* "&amp;" (substring s start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (mdout-url s)
  ;; an address with spaces or parentheses, or which starts with <, is
  ;; written between < and >, where these two are written after a backslash
  (let ((s (mdout-literal s)))
    (if (or (string-index s #\space) (string-index s #\() (string-index s #\))
            (mdout-starts? s 0 "<"))
        (string-append "<" (string-replace (string-replace s "<" "\\<")
                                           ">" "\\>")
                       ">")
        s)))

(define (mdout-title s)
  (if (or (not s) (== s "")) ""
      (string-append " \""
                     (string-replace (mdout-literal s) "\"" "\\\"")
                     "\"")))

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
  ;; the text of a link, or the description of an image, ends at the first ]
  ;; which closes no [: the brackets without a partner are escaped, the one
  ;; which opens as its entity (see above). Those of code do not count
  (let ((n (string-length s)))
    (let loop ((i 0) (open '()) (alone '()))
      ;; open: the [ which are not closed yet, alone: the ] which close none
      (cond ((>= i n)
             (if (and (null? open) (null? alone)) s
                 (let sub ((l (sort (append open alone) <)) (start 0) (acc '()))
                   (if (null? l)
                       (apply string-append
                              (reverse (cons (substring s start n) acc)))
                       (sub (cdr l) (+ (car l) 1)
                            (cons* (if (char=? (string-ref s (car l)) #\[)
                                       "&#91;" "\\]")
                                   (substring s start (car l)) acc))))))
            ((char=? (string-ref s i) #\\) (loop (+ i 2) open alone))
            ((char=? (string-ref s i) #\`)
             (let* ((j (let run ((j i))
                         (if (and (< j n) (char=? (string-ref s j) #\`))
                             (run (+ j 1)) j)))
                    (e (mdout-code-end s j (- j i))))
               (loop (or e j) open alone)))
            ((char=? (string-ref s i) #\[) (loop (+ i 1) (cons i open) alone))
            ((char=? (string-ref s i) #\])
             (if (null? open)
                 (loop (+ i 1) open (cons i alone))
                 (loop (+ i 1) (cdr open) alone)))
            (else (loop (+ i 1) open alone))))))

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

;; the kinds of emphasis which the text that is written is inside: *a *b**
;; would not be read as emphasis inside emphasis, which is emphasis too
(define mdout-open '())

(define (mdout-mark x)
  (cond ((func? x 'em) "*") ((func? x 'strong) "**") (else "~~")))

(define (mdout-emphasis x cell? before? after?)
  (if (in? (car x) mdout-open) (mdout-inlines (mdout-children x) cell?)
      (with old mdout-open
        (set! mdout-open (cons (car x) mdout-open))
        (with r (mdout-emphasis* x cell? before? after?)
          (set! mdout-open old)
          r))))

(define (mdout-emphasis* x cell? before? after?)
  ;; emphasis, after a letter or a digit when before? and followed by one
  ;; when after?. Its * would not open before punctuation then, or not
  ;; close after punctuation: the punctuation of its ends goes outside
  (let* ((mark (cond ((func? x 'em) "*") ((func? x 'strong) "**") (else "~~")))
         (l (mdout-children x))
         (s (and before? (nnull? l) (string? (car l)) (car l)))
         (i (if s (let loop ((i 0))
                    (if (and (< i (string-length s))
                             (mdout-punctuation? (string-ref s i)))
                        (loop (+ i 1)) i))
                0))
         (l (if (> i 0) (cons (substring s i (string-length s)) (cdr l)) l))
         (t (and after? (nnull? l) (string? (cAr l)) (cAr l)))
         (j (if t (let loop ((j (string-length t)))
                    (if (and (> j 0) (mdout-punctuation? (string-ref t (- j 1))))
                        (loop (- j 1)) j))
                0))
         (l (if (and t (< j (string-length t)))
                (append (cDr l) (list (substring t 0 j))) l)))
    (string-append
      (if (> i 0) (mdout-escape (substring s 0 i) cell?) "")
      (mdout-spaced mark mark l cell?)
      (if (and t (< j (string-length t)))
          (mdout-escape (substring t j (string-length t)) cell?) ""))))

(define (mdout-cell-math s cell?)
  ;; a bar would end the cell of a table
  (if cell?
      (string-replace (string-replace s "\\|" "\\Vert ") "|" "\\vert ")
      s))

(define (mdout-inline x cell?)
  (cond ((string? x) (mdout-escape x cell?))
        ((not (pair? x)) "")
        (else
          (let ((l (mdout-children x)))
            (case (car x)
              ((em strong del) (mdout-emphasis x cell? #f #f))
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
                     (with old mdout-in-link?
                       (set! mdout-in-link? #t)
                       (with s (mdout-balanced (mdout-inlines l cell?))
                         (set! mdout-in-link? old)
                         (string-append "[" s "](" (mdout-url url)
                                        (mdout-title title) ")"))))))
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
              ;; (a formula as it was written, but in the cell of a table,
              ;; where its bars would end the cell)
              ((math)
               (or (and (not cell?) (mdout-attr x 'source))
                   (string-append "$" (mdout-cell-math (mdout-join l "") cell?)
                                  "$")))
              ((displaymath)
               (or (and (not cell?) (mdout-attr x 'source))
                   (string-append "$$" (mdout-cell-math (mdout-join l "") cell?)
                                  "$$")))
              ;; (a bar in a cell is written \|: the parser takes the
              ;; backslash away as it splits the cells)
              ((html)
               (with s (mdout-join l "")
                 (if cell? (string-replace s "|" "\\|") s)))
              ((footnote) (string-append "[^" (mdout-join l "") "]"))
              (else (mdout-inlines l cell?)))))))

(define (mdout-utf8-start s)
  ;; where the last character of the string of UTF-8 s starts
  (let loop ((j (- (string-length s) 1)))
    (if (and (> j 0) (with c (char->integer (string-ref s j))
                       (and (>= c 128) (< c 192))))
        (loop (- j 1)) j)))

(define (mdout-entity s)
  ;; the character of UTF-8 s as an entity, which is punctuation for the
  ;; reader as markup goes
  (let* ((b (map char->integer (string->list s)))
         (code (if (< (car b) 128) (car b)
                   (let loop ((v (cond ((< (car b) 224) (- (car b) 192))
                                       ((< (car b) 240) (- (car b) 224))
                                       (else (- (car b) 240))))
                              (l (cdr b)))
                     (if (null? l) v (loop (+ (* v 64) (- (car l) 128)) (cdr l)))))))
    (string-append "&#" (number->string code) ";")))

(define (mdout-first-entity x cell?)
  ;; the text x, whose first character is written as an entity
  (let* ((c (char->integer (string-ref x 0)))
         (e (min (string-length x)
                 (cond ((< c 192) 1) ((< c 224) 2) ((< c 240) 3) (else 4)))))
    (string-append (mdout-entity (substring x 0 e))
                   (mdout-escape (substring x e (string-length x)) cell?))))

(define (mdout-inlines l cell?)
  ;; the text of the nodes, each one written with a look at its neighbours
  (if (and (pair? l) (null? (cdr l))) (mdout-inline (car l) cell?)
      (let loop ((l l) (prev #f) (acc '()) (entity? #f))
        ;; prev: the node before, acc: the texts so far, the last one first,
        ;; entity?: whether the first character of a text which follows is
        ;; written as an entity
        (if (null? l) (apply string-append (reverse acc))
            (let* ((x (car l))
                   (em? (and (pair? x) (in? (car x) '(em strong del))))
                   (before? (and (nnull? acc) (mdout-alnum? (mdout-last (car acc)))))
                   (after? (and (nnull? (cdr l)) (string? (cadr l))
                                (!= (cadr l) "")
                                (mdout-alnum? (string-ref (cadr l) 0))))
                   (s (cond (em? (mdout-emphasis x cell? before? after?))
                            ;; a $ before a digit would not close the formula,
                            ;; and the * of emphasis which ends with code
                            ;; would not close before a letter: the character
                            ;; is written as its entity
                            ((and (string? x) (!= x "")
                                  (or entity?
                                      (and (func? prev 'math)
                                           (mdout-digit? (string-ref x 0)))))
                             (mdout-first-entity x cell?))
                            (else (mdout-inline x cell?))))
                   (mark (and em? (not (in? (car x) mdout-open)) (mdout-mark x)))
                   (n (string-length s))
                   (m (if mark (string-length mark) 0))
                   ;; emphasis which starts with code after a letter: its *
                   ;; would not open, but it does after an entity
                   (open? (and mark before? (string? prev) (> n m)
                               (mdout-starts? s 0 mark)
                               (mdout-punctuation? (string-ref s m))))
                   (close? (and mark after? (> n m)
                                (mdout-starts? s (- n m) mark)
                                (mdout-punctuation? (string-ref s (- n m 1)))))
                   (acc (if (not open?) acc
                            (with j (mdout-utf8-start (car acc))
                              (cons* (mdout-entity (substring (car acc) j
                                                              (string-length (car acc))))
                                     (substring (car acc) 0 j)
                                     (cdr acc))))))
              (cond ((== s "") (loop (cdr l) prev acc entity?))
                    ;; text which ends with ! before a link: not an image
                    ((and (string? prev) (char=? (string-ref s 0) #\[)
                          (char=? (mdout-last (car acc)) #\!))
                     (loop (cdr l) x
                           (cons* s "\\!"
                                  (substring (car acc) 0
                                             (- (string-length (car acc)) 1))
                                  (cdr acc))
                           close?))
                    (else (loop (cdr l) x (cons s acc) close?))))))))

(define (mdout-last s) (string-ref s (- (string-length s) 1)))

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

(define (mdout-meta x)
  ;; the YAML header: its lines as they are written, if it has them
  (with source (mdout-attr x 'source)
    (if source
        `("---" ,@(if (== source "") '() (mdout-split source)) "---")
        (mdout-meta-values (mdout-children x)))))

(define (mdout-meta-values l)
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
  ;; between backquotes, or between tildes when the words after the fence
  ;; hold a backquote
  (let* ((s (mdout-join (list-filter (mdout-children x) string?) ""))
         (info (or (mdout-attr x 'info) (mdout-attr x 'lang) ""))
         (c (if (string-index info #\`) #\~ #\`))
         (n (max 3 (+ (mdout-longest-run s c) 1)))
         (fence (make-string n c)))
    `(,(string-append fence info)
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

(define (mdout-needs-blank? a b)
  ;; is a blank line needed between the blocks a and b which follow each
  ;; other in an item? Without it a paragraph would go on the paragraph, the
  ;; quote or the list before it, a table would go on the quote or the
  ;; list before it, a quote on the quote, a table or HTML would take what
  ;; follows, and a paragraph which starts as a title would be the title
  ;; of the definition of a link before it; code between fences, a heading,
  ;; a rule and a list start by themselves
  (and (pair? a) (pair? b)
       (or (in? (car a) '(table html))
           (and (func? a 'ref-def) (func? b 'p)
                (not (and (pair? (cdr b)) (string? (cadr b))
                          (> (string-length (cadr b)) 0)
                          (not (in? (string-ref (cadr b) 0)
                                    (string->list "\"'("))))))
           (and (func? b 'p) (in? (car a) '(p blockquote ul ol)))
           (and (func? b 'table) (in? (car a) '(blockquote ul ol)))
           (and (func? a 'blockquote) (func? b 'blockquote))
           (and (func? a 'p) (in? (car b) '(html footnote-def ref-def)))
           ;; a list which does not interrupt a paragraph: one whose number
           ;; is not 1, or whose first item is empty
           (and (func? a 'p) (in? (car b) '(ul ol))
                (or (and (func? b 'ol) (mdout-attr b 'start)
                         (!= (mdout-attr b 'start) "1"))
                    (with l (mdout-children b)
                      (or (null? l) (mdout-empty-item? (car l)))))))))

(define (mdout-empty-item? x)
  ;; an item which nothing is written in
  (list-and (map (lambda (y)
                   (or (== y "")
                       (and (func? y 'p)
                            (list-and (map (lambda (z) (== z ""))
                                           (mdout-children y))))))
                 (mdout-children x))))

(define (mdout-loose? x)
  ;; a list is written with blank lines between its items when it had some,
  ;; or when two blocks of an item need one between them: the list is read
  ;; as one with blank lines then
  (or (== (mdout-attr x 'loose) "true")
      (list-or (map (lambda (li)
                      (let loop ((l (mdout-children li)))
                        (and (nnull? l) (nnull? (cdr l))
                             (or (mdout-needs-blank? (car l) (cadr l))
                                 (loop (cdr l))))))
                    (mdout-children x)))))

(define (mdout-list-marker x avoid)
  ;; what the items of the list start with: -, * or +, a dot or a
  ;; parenthesis after a number. A list after a list of the same kind would
  ;; be read as one with the marker of that one, avoid: it takes another
  (let* ((m (if (func? x 'ol) "."
                (with a (mdout-attr x 'marker)
                  (if (in? a '("*" "+")) a "-")))))
    (cond ((!= m avoid) m)
          ((func? x 'ol) ")")
          ((== m "-") "*")
          (else "-"))))

(define (mdout-list x avoid)
  (let* ((ordered? (func? x 'ol))
         (mark (mdout-list-marker x avoid))
         (loose? (mdout-loose? x))
         (start (or (and (mdout-attr x 'start)
                         (string->number (mdout-attr x 'start)))
                    1)))
    (let loop ((l (mdout-children x)) (i start) (acc '()))
      (if (null? l) (reverse acc)
          (let* ((marker (if ordered?
                             (string-append (number->string i) mark " ")
                             (string-append mark " ")))
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

(define (mdout-one-line l)
  ;; the nodes of a heading, which is one line: with spaces for line breaks
  (map (lambda (x)
         (cond ((func? x 'br) " ")
               ((and (pair? x) (in? (car x) '(em strong del a)))
                (cons (car x) (mdout-one-line (cdr x))))
               (else x)))
       l))

(define (mdout-heading-end s)
  ;; the text of a heading: the # which end it, after a space, would be
  ;; taken for the end of the heading and dropped
  (let* ((n (string-length s))
         (k (let loop ((k n))
              (if (and (> k 0) (char=? (string-ref s (- k 1)) #\#))
                  (loop (- k 1)) k))))
    (if (and (< k n) (or (== k 0) (char=? (string-ref s (- k 1)) #\space)))
        (string-append (substring s 0 k) "\\" (substring s k n))
        s)))

(define (mdout-block x)
  ;; the lines of a block
  (cond ((string? x) (mdout-text (list x)))
        ((not (pair? x)) '())
        (else
          (let ((l (mdout-children x)))
            (case (car x)
              ((meta) (mdout-meta x))
              ((h1 h2 h3 h4 h5 h6)
               (let* ((n (- (char->integer (string-ref (symbol->string (car x)) 1))
                            (char->integer #\0)))
                      (s (string-replace (mdout-inlines (mdout-one-line l) #f)
                                         "\n" " ")))
                 (list (string-append (make-string n #\#) " "
                                      (mdout-heading-end s)))))
              ((p) (mdout-text l))
              ((blockquote)
               (map (lambda (s) (if (== s "") ">" (string-append "> " s)))
                    (mdout-blocks l #t)))
              ((ul ol) (mdout-list x #f))
              ((pre) (mdout-pre x))
              ((hr) (list "***"))
              ;; (a table and a formula as they were written, if they
              ;; have their source)
              ((table)
               (with source (mdout-attr x 'source)
                 (if source (mdout-split source) (mdout-table x))))
              ((displaymath)
               (with source (mdout-attr x 'source)
                 (if source (mdout-split source)
                     `("$$" ,@(mdout-split (mdout-join l "")) "$$"))))
              ((html ref-def) (mdout-split (mdout-join l "")))
              ((footnote-def) (mdout-footnote-def x))
              ((markdown) (mdout-blocks l #t))
              ;; inline markup where a block is expected: a paragraph
              (else (mdout-text (list x))))))))

(define (mdout-blocks l loose?)
  ;; the lines of blocks; between them a blank line, except in an item of a
  ;; list without blank lines, before a list inside it
  (let loop ((l l) (acc '()) (first? #t) (prev #f) (mark #f))
    ;; prev: the block before, mark: its marker if it is a list
    (cond ((null? l) (reverse acc))
          (else
            (let* ((x (car l))
                   (items? (and (pair? x) (in? (car x) '(ul ol))))
                   ;; two lists of a kind in a row would be read as one
                   (avoid (and items? (func? prev (car x)) mark))
                   (mark (and items? (mdout-list-marker x avoid)))
                   (lines (if items? (mdout-list x avoid) (mdout-block x)))
                   (sep? (and (not first?)
                              (or loose? (mdout-needs-blank? prev x)))))
              (if (null? lines)
                  (loop (cdr l) acc first? prev mark)
                  (loop (cdr l)
                        (append (reverse lines) (if sep? (cons "" acc) acc))
                        #f x mark)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mdout-block? x)
  (and (pair? x)
       (in? (car x) '(meta h1 h2 h3 h4 h5 h6 p blockquote ul ol pre hr table
                      footnote-def ref-def markdown))))

(tm-define (serialize-markdown x)
  (:type (-> stree string))
  (:synopsis "Write the Markdown tree @x as Markdown")
  (let* ((file? (func? x '!file 1))
         (x (if file? (cadr x) x))
         (l (if (func? x 'markdown) (cdr x) (list x))))
    (set! mdout-ref-labels (mdout-collect-labels x))
    ;; (a file is blocks, also when it is a formula or HTML alone)
    (if (or (list-or (map mdout-block? l)) (and file? (nnull? l)))
        (string-append (mdout-join (mdout-blocks l #t) "\n") "\n")
        ;; text only: a snippet
        (mdout-join (mdout-text l) "\n"))))
