
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

(define oftm-soft-hyphen (office-utf8 #xad))

(define (oftm-text s)
  ;; the text in the encoding of TeXmacs, without the characters of no
  ;; width which the programs put around formulas and fields, and without
  ;; the places where a word may be cut, which TeXmacs finds itself
  (string-replace
    (string-replace (utf8->cork (string-replace s oftm-soft-hyphen ""))
                    "<#200B>" "")
    "<#FEFF>" ""))

(define (oftm-text-nodes s)
  ;; the text as trees of TeXmacs: a space at which a line is not broken
  ;; is a tag
  (with t (oftm-text s)
    (if (< (string-search-forwards "<varspace>" 0 t) 0) (list t)
        (oftm-split-spaces t '(("<varspace>" (nbsp)))))))

(define (oftm-concat l)
  ;; a list of TeXmacs trees as one tree
  (let* ((l (office-join-strings l)))
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
  (cond ((string? x) (string-replace (oftm-text x) "<varspace>" " "))
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

;; The accents above a letter are written with a character, the accent by
;; itself or the one which combines with a letter: each one as the
;; converter of MathML knows it, so that it gives the accent of TeXmacs.
(define oftm-accents
  (map (lambda (p)
         (cons (office-utf8 (car p))
               (if (number? (cdr p)) (office-utf8 (cdr p)) (cdr p))))
       '((#x302 . "^") (#x2c6 . "^") (#x303 . "~") (#x2dc . "~")
         (#x304 . #xaf) (#x305 . #xaf) (#x203e . #xaf)
         (#x307 . "&DiacriticalDot;") (#x2d9 . "&DiacriticalDot;")
         (#x308 . "&DoubleDot;") (#xa8 . "&DoubleDot;")
         (#x30a . #x2da) (#x30c . #x2c7) (#x306 . #x2d8) (#x311 . "&#x311;")
         (#x301 . #xb4) (#x300 . #x60)
         (#x20d7 . "&RightVector;") (#x2192 . "&RightVector;")
         (#x20d6 . "&LeftArrow;") (#x2190 . "&LeftArrow;"))))

(define (oftm-accent x)
  ;; the element x of an accent, with its character as the converter of
  ;; MathML knows it
  (with a (and (func? x 'm:mo) (string? (cAr x))
               (assoc-ref oftm-accents (string-trim-spaces (cAr x))))
    (if a `(m:mo ,a) x)))

(define (oftm-mathml x)
  ;; The element of MathML x as the converter of MathML wants it: with the
  ;; prefix m: for its elements, whatever prefix they have or have not in
  ;; the file. The annotations are other writings of the same formula.
  (cond ((not (pair? x)) x)
        ((func? x '@) x)
        (else
          (let* ((name (oftm-local-name (car x)))
                 (l (map oftm-mathml
                         (list-filter
                           (cdr x)
                           (lambda (y)
                             (not (and (pair? y) (symbol? (car y))
                                       (in? (oftm-local-name (car y))
                                            '("annotation" "annotation-xml"))))))))
                 (l (if (and (== name "mover") (== (ox-attr x 'accent) "true")
                             (pair? l))
                        (rcons (cDr l) (oftm-accent (cAr l)))
                        l)))
            (cons (string->symbol (string-append "m:" name)) l)))))

(define (oftm-from-mathml x)
  ;; the tree of TeXmacs for the element of MathML x; a formula which the
  ;; converter cannot read is not lost: its text is kept
  (catch #t
    (lambda ()
      (let ((env (environment))
            (root (oftm-mathml x)))
        (initialize-xpath env root (cut mathtm-as-serial <> root))))
    (lambda args (oftm-text (ox-text (oftm-mathml x))))))

;; The spaces of Unicode in a formula, as the converter of MathML leaves
;; them, and what they are in TeXmacs: a space between a function and its
;; argument, spaces of a width, or nothing.
(define oftm-math-spaces
  '(("<nospace>" " ") ("<#200B>") ("<#2060>") ("<#2063>") ("<#2064>")
    ("<#2001>" (space "1em")) ("<#2003>" (space "1em"))
    ("<#2000>" (space "0.5em")) ("<#2002>" (space "0.5em"))
    ("<#2004>" (space "0.33em")) ("<#2005>" (space "0.25em"))
    ("<#2006>" (space "0.17em")) ("<#2009>" (space "0.17em"))
    ("<#200A>" (space "0.1em")) ("<#205F>" (space "0.22em"))
    ("<#A0>" (space "0.33em")) ("<varspace>" (space "0.33em"))))

(define (oftm-split-spaces s l)
  ;; the pieces of the string s around the spaces of the list l
  (if (null? l) (list s)
      (let* ((what (caar l))
             (i (string-search-forwards what 0 s)))
        (if (< i 0) (oftm-split-spaces s (cdr l))
            (append (oftm-split-spaces (substring s 0 i) (cdr l))
                    (cdar l)
                    (oftm-split-spaces
                      (substring s (+ i (string-length what)) (string-length s))
                      l))))))

(define (oftm-math-text t)
  ;; text inside a formula: its spaces are spaces
  (cond ((string? t) (string-replace t "<varspace>" " "))
        ((pair? t) (cons (car t) (map oftm-math-text (cdr t))))
        (else t)))

(define (oftm-clean-math t)
  (cond ((string? t) (oftm-concat (oftm-split-spaces t oftm-math-spaces)))
        ((and (func? t 'with 3) (== (cadr t) "mode") (== (caddr t) "text"))
         `(with "mode" "text" ,(oftm-math-text (cadddr t))))
        ;; four dots above: an accent which the converter does not know
        ((and (func? t 'above 2) (== (caddr t) "<#20DC>"))
         `(wide ,(oftm-clean-math (cadr t)) "<ddddot>"))
        ((and (pair? t) (func? t 'concat))
         (oftm-concat
           (append-map (lambda (x)
                         (if (string? x) (oftm-split-spaces x oftm-math-spaces)
                             (list (oftm-clean-math x))))
                       (cdr t))))
        ((pair? t) (cons (car t) (map oftm-clean-math (cdr t))))
        (else t)))

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
          (oftm-clean-math r)))))

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
          ;; (an address is not a file to load when the document is opened)
          ((or (>= (string-search-forwards "://" 0 name) 0)
               (string-starts? (locase-all name) "file:"))
           (list name))
          ((!= name "") (list `(image ,name ,w ,h "" "")))
          (else '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The labels of what has a number: the bookmarks in a heading, in a
;; caption, in a theorem, in an entry of the bibliography, or beside a
;; number of a sequence.
(define oftm-numbered-labels (make-ahash-table))

(define (oftm-number? s)
  ;; a number: 1, 2.3, A.1
  (and (!= s "") (<= (string-length s) 12)
       (list-or (map char-numeric? (string->list s)))
       (list-and (map (lambda (c) (or (char-numeric? c) (char-alphabetic? c)
                                      (in? c '(#\. #\-))))
                      (string->list s)))))

(define (oftm-has? x tag)
  ;; whether a node tag is inside x
  (cond ((not (pair? x)) #f)
        ((in? (car x) '(math image @)) #f)
        ((func? x tag) #t)
        (else (list-or (map (lambda (y) (oftm-has? y tag)) (ox-children x))))))

(define (oftm-bookmarks x)
  ;; the names of the bookmarks inside x, but those of its notes
  (cond ((not (pair? x)) '())
        ((in? (car x) '(math image @ note)) '())
        ((func? x 'bookmark) (if (ox-attr x 'name) (list (ox-attr x 'name)) '()))
        (else (append-map oftm-bookmarks (ox-children x)))))

(define (oftm-scan-labels x)
  (cond ((not (pair? x)) (noop))
        ((func? x 'p)
         (when (or (in? (ox-attr x 'role)
                        '("heading" "caption" "theorem" "remark" "proof" "bibitem"))
                   (oftm-has? x 'seq))
           (for (name (oftm-bookmarks x))
             (ahash-set! oftm-numbered-labels name #t)))
         (for-each oftm-scan-labels (ox-children x)))
        ((in? (car x) '(math image @)) (noop))
        (else (for-each oftm-scan-labels (ox-children x)))))

;; The name and the number of a theorem or of a figure are the start of
;; its paragraph: "Theorem 1." in bold, or "Figure 1:". TeXmacs writes
;; them itself: they are taken away, and their labels are kept.

(define (oftm-drop-start l n)
  ;; the nodes l without the n first characters of their text
  (cond ((or (null? l) (<= n 0)) l)
        ((string? (car l))
         (if (<= (string-length (car l)) n)
             (oftm-drop-start (cdr l) (- n (string-length (car l))))
             (cons (substring (car l) n (string-length (car l))) (cdr l))))
        (else l)))

(define (oftm-trim-start l)
  ;; the nodes l without the spaces and the punctuation at their start
  (if (and (pair? l) (string? (car l)))
      (let loop ((i 0))
        (if (and (< i (string-length (car l)))
                 (in? (string-ref (car l) i) '(#\space #\. #\: #\-)))
            (loop (+ i 1))
            (with s (substring (car l) i (string-length (car l)))
              (if (== s "") (oftm-trim-start (cdr l)) (cons s (cdr l))))))
      l))

(define (oftm-punctuation? s)
  (list-and (map (lambda (c) (in? c '(#\space #\. #\: #\-))) (string->list s))))

(define (oftm-named-start l)
  ;; (word labels rest numbered?) when the nodes l of a paragraph start
  ;; with a name, in bold or followed by a number of a sequence: "Theorem
  ;; 1." or "Figure 1:", with the bookmarks of its labels; else #f
  (let loop ((r l) (word "") (labels '()) (number? #f) (bold? #f))
    (let ((done (lambda (rest)
                  (let* ((w (string-trim-spaces word))
                         (w (let trim ((w w))
                              (if (and (!= w "")
                                       (in? (string-ref w (- (string-length w) 1))
                                            '(#\. #\: #\space)))
                                  (trim (substring w 0 (- (string-length w) 1)))
                                  w))))
                    (and (!= w "") (or bold? number?)
                         (not (string-index w #\space))
                         (list w (reverse labels) (oftm-trim-start rest) number?))))))
      (cond ((null? r) (done r))
            ((func? (car r) 'bookmark)
             (loop (cdr r) word
                   (if (ox-attr (car r) 'name) (cons (ox-attr (car r) 'name) labels)
                       labels)
                   number? bold?))
            ((and (func? (car r) 'seq) (not number?))
             (loop (cdr r) word labels #t bold?))
            ;; bold text: the name before the number, the dot after it
            ((and (func? (car r) 'strong)
                  (list-and (map (lambda (y) (or (string? y)
                                                 (and (pair? y)
                                                      (in? (car y) '(seq bookmark)))))
                                 (ox-children (car r)))))
             (let* ((c (ox-children (car r)))
                    (text (apply string-append (list-filter c string?)))
                    (inner-number? (list-or (map (lambda (y) (func? y 'seq)) c)))
                    (inner-labels (append-map
                                    (lambda (y)
                                      (if (and (func? y 'bookmark) (ox-attr y 'name))
                                          (list (ox-attr y 'name)) '()))
                                    c)))
               (cond ((and number? (oftm-punctuation? text) (not inner-number?))
                      (loop (cdr r) word (append (reverse inner-labels) labels)
                            number? #t))
                     ((and (not number?) (== word ""))
                      (loop (cdr r) text (append (reverse inner-labels) labels)
                            inner-number? #t))
                     (else (done r)))))
            ;; plain text: the name before the number
            ((and (string? (car r)) (not number?) (not bold?) (== word "")
                  (pair? (cdr r))
                  (or (func? (cadr r) 'seq) (func? (cadr r) 'bookmark)))
             (loop (cdr r) (car r) labels number? bold?))
            (else (done r))))))

(define oftm-environments
  '(("theorem" . theorem) ("proposition" . proposition) ("lemma" . lemma)
    ("corollary" . corollary) ("conjecture" . conjecture) ("axiom" . axiom)
    ("definition" . definition) ("notation" . notation) ("remark" . remark)
    ("note" . note) ("example" . example) ("convention" . convention)
    ("warning" . warning) ("exercise" . exercise) ("problem" . problem)
    ("question" . question) ("solution" . solution) ("proof" . proof)))

(define (oftm-labels names)
  (map (lambda (name) `(label ,(oftm-text name))) names))

(define (oftm-theorem l)
  ;; (environment . rest) for the theorem which starts the blocks l: the
  ;; paragraph with its name, and those of the same style after it which
  ;; do not start another one; #f when the name is not one of TeXmacs
  (let* ((x (car l))
         (start (oftm-named-start (ox-children x)))
         (env (and start (assoc-ref oftm-environments (locase-all (car start))))))
    (and env
         (let loop ((r (cdr l))
                    (acc (list (oftm-concat
                                 (append (oftm-labels (cadr start))
                                         (append-map oftm-inline (caddr start)))))))
           (if (and (pair? r) (func? (car r) 'p)
                    (== (ox-attr (car r) 'role) (ox-attr x 'role))
                    (not (oftm-named-start (ox-children (car r)))))
               (loop (cdr r) (cons (oftm-inlines (ox-children (car r))) acc))
               (cons `(,env ,(oftm-document (reverse acc))) r))))))

(define (oftm-numbered-formula x)
  ;; (formula labels) for a paragraph which is a formula on its own lines
  ;; with its number after it; else #f
  (let* ((l (list-filter (ox-children x)
                         (lambda (y) (not (and (string? y)
                                               (in? (string-trim-spaces y)
                                                    '("" "(" ")" "()"))))))))
    (and (pair? l) (func? (car l) 'math)
         (list-and (map (lambda (y) (and (pair? y) (in? (car y) '(tab seq bookmark))))
                        (cdr l)))
         (list-or (map (lambda (y) (func? y 'seq)) (cdr l)))
         (list (car l) (oftm-bookmarks (cons 'p (cdr l)))))))

(define (oftm-bibliography l)
  ;; (bibliography . rest) for the entries which start the blocks l
  (let loop ((r l) (acc '()) (n 0))
    (if (and (pair? r) (oftm-role? (car r) "bibitem"))
        (let* ((c (ox-children (car r)))
               ;; "[" the number "] " before the text of the entry
               (c (if (and (pair? c) (string? (car c))
                           (== (string-trim-spaces (car c)) "["))
                      (cdr c) c))
               (labels (oftm-bookmarks (cons 'p (list-filter c (lambda (y) (func? y 'bookmark))))))
               (number (with s (list-find c (lambda (y) (func? y 'seq)))
                         (if s (oftm-plain s) (number->string (+ n 1)))))
               (rest (let skip ((c c))
                       (if (and (pair? c) (pair? (car c))
                                (in? (caar c) '(bookmark seq)))
                           (skip (cdr c))
                           c)))
               (rest (if (and (pair? rest) (string? (car rest))
                              (string-starts? (car rest) "]"))
                         (oftm-trim-start (oftm-drop-start rest 1))
                         rest)))
          (loop (cdr r)
                (cons (oftm-concat
                        (append (list `(bibitem* ,number))
                                (oftm-labels labels)
                                (append-map oftm-inline rest)))
                      acc)
                (+ n 1)))
        (cons `(bibliography "bib" "tm-plain" ""
                             (document (bib-list ,(number->string (max 1 n))
                                                 ,(oftm-document (reverse acc)))))
              r))))

(define (oftm-wrap tag l)
  (with t (oftm-inlines l)
    (if (== t "") '() (list (list tag t)))))

(define (oftm-inline x)
  ;; the list of TeXmacs trees for an inline node
  (cond ((string? x) (oftm-text-nodes x))
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
              ((mark) (oftm-wrap 'marked l))
              ((color)
               (let ((t (oftm-inlines l))
                     (v (ox-attr x 'value)))
                 (cond ((== t "") '())
                       (v (list `(with "color" ,v ,t)))
                       (else (list t)))))
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
               ;; A reference to the label of something with a number,
               ;; whose text is this number, is a reference of TeXmacs,
               ;; which shows the number again; else the text of the
               ;; reference is kept, as a link to its bookmark.
               (let ((t (oftm-inlines l))
                     (name (oftm-text (or (ox-attr x 'name) ""))))
                 (cond ((== t "") '())
                       ((== name "") (list t))
                       ;; the number of the page of a label
                       ((== (ox-attr x 'kind) "page")
                        (if (and (string? t) (oftm-number? t))
                            (list `(pageref ,name))
                            (list `(hlink ,t ,(string-append "#" name)))))
                       ((and (ahash-ref oftm-numbered-labels (ox-attr x 'name))
                             (string? t) (oftm-number? t))
                        (list `(reference ,name)))
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

(define oftm-in-cell? #f)

(define (oftm-cell-body c)
  ;; the contents of a cell: text, or a document of several paragraphs
  (with old oftm-in-cell?
    (set! oftm-in-cell? #t)
    (with r (oftm-cell-body-sub c)
      (set! oftm-in-cell? old)
      r)))

(define (oftm-cell-body-sub c)
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

(define (oftm-runs row)
  ;; the runs (first last value) of the equal values of a list which are
  ;; not #f, with the ranks from 1 on
  (let loop ((l row) (j 1) (acc '()))
    (cond ((null? l) (reverse acc))
          ((not (car l)) (loop (cdr l) (+ j 1) acc))
          ((and (pair? acc) (== (cadar acc) (- j 1)) (== (caddar acc) (car l)))
           (loop (cdr l) (+ j 1)
                 (cons (list (caar acc) j (car l)) (cdr acc))))
          (else (loop (cdr l) (+ j 1) (cons (list j j (car l)) acc))))))

(define (oftm-rectangles matrix)
  ;; the rectangles (row1 row2 col1 col2 value) which cover the values of
  ;; a matrix which are not #f: the runs of its rows, taken together when
  ;; the rows which follow each other have the same ones
  (let loop ((rows matrix) (i 1) (open '()) (acc '()))
    ;; open: the runs of the row before, each with the row where it starts
    (let* ((runs (if (null? rows) '() (oftm-runs (car rows))))
           (closed (list-filter open (lambda (o) (not (in? (car o) runs)))))
           (acc (append (map (lambda (o)
                               (list (cadr o) (- i 1) (caar o) (cadar o) (caddar o)))
                             closed)
                        acc)))
      (if (null? rows) (reverse acc)
          (loop (cdr rows) (+ i 1)
                (map (lambda (r)
                       (with o (list-find open (lambda (o) (== (car o) r)))
                         (list r (if o (cadr o) i))))
                     runs)
                acc)))))

(define (oftm-cwiths matrix var value)
  ;; the format var of the cells of the matrix whose value is not #f:
  ;; with this value, or with their own when value is #f
  (map (lambda (r)
         `(cwith ,(number->string (car r)) ,(number->string (cadr r))
                 ,(number->string (caddr r)) ,(number->string (cadddr r))
                 ,var ,(or value (list-ref r 4))))
       (oftm-rectangles matrix)))

(define (oftm-part s)
  ;; a part of the width of the paragraph as a number: "0.7par" is 0.7
  (or (and s (string-ends? s "par")
           (string->number (substring s 0 (- (string-length s) 3))))
      1.0))

(define (oftm-table x)
  ;; A table. The cells which a wider or a higher one covers are there,
  ;; empty, as in the tables of TeXmacs. The borders and the backgrounds
  ;; of the cells are kept, as few formats of rectangles of cells; a table
  ;; which says nothing of its borders has all of them.
  (let* ((rows (list-filter (ox-children x) (lambda (r) (func? r 'row))))
         (ncols (apply max (cons 1 (map (lambda (r) (length (ox-children r)))
                                        rows))))
         (grid (map (lambda (r)
                      (with cells (ox-children r)
                        (append cells
                                (map (lambda (k) '(cell))
                                     (iota (- ncols (length cells)))))))
                    rows))
         (formats '())
         (wide? #f)
         (trows
           (map (lambda (cells i)
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
                                 (when (func? body 'document) (set! wide? #t))
                                 (list 'cell body)))
                             cells (map (lambda (j) (+ j 1)) (iota (length cells))))))
                grid (map (lambda (i) (+ i 1)) (iota (length grid)))))
         (aligns (oftm-cwiths (map (lambda (cells) (map oftm-cell-align cells)) grid)
                              "cell-halign" #f))
         ;; the borders: a letter of the sides of each cell
         (borders? (list-or (map (lambda (cells)
                                   (list-or (map (lambda (c) (ox-attr c 'borders))
                                                 cells)))
                                 grid)))
         (side (lambda (letter)
                 (map (lambda (cells)
                        (map (lambda (c)
                               (with b (ox-attr c 'borders)
                                 (and b (>= (string-search-forwards letter 0 b) 0))))
                             cells))
                      grid)))
         (all? (lambda (m)
                 (list-and (map (lambda (row cells)
                                  (list-and (map (lambda (v c)
                                                   (or v (ox-attr c 'covered)))
                                                 row cells)))
                                m grid))))
         (sides (map side '("t" "b" "l" "r")))
         (block? (or (not borders?) (list-and (map all? sides))))
         (lines (if block? '()
                    (append-map (lambda (m var) (oftm-cwiths m var "1ln"))
                                sides
                                '("cell-tborder" "cell-bborder" "cell-lborder"
                                  "cell-rborder"))))
         (fills (oftm-cwiths (map (lambda (cells)
                                    (map (lambda (c) (ox-attr c 'background)) cells))
                                  grid)
                             "cell-background" #f))
         (width (let loop ((rows trows) (w '()))
                  ;; the sum of the widths of the columns, in characters
                  (if (null? rows) (apply + w)
                      (let sub ((l (map oftm-length (cdar rows))) (w w) (r '()))
                        (cond ((and (null? l) (null? w)) (loop (cdr rows) (reverse r)))
                              ((null? l) (sub l (cdr w) (cons (car w) r)))
                              ((null? w) (sub (cdr l) w (cons (car l) r)))
                              (else (sub (cdr l) (cdr w)
                                         (cons (max (car l) (car w)) r))))))))
         ;; a table of the width of the text, or of a part of it: when it
         ;; says so, or when its text would not fit otherwise
         ;; (a table inside a cell has the width of its text)
         (twidth (and (not oftm-in-cell?) (ox-attr x 'width)))
         (wide? (and (not oftm-in-cell?) (or wide? (> width 72))))
         (cols (with c (ox-attr x 'columns)
                 (and c (map office-number (string-tokenize-by-char c #\space)))))
         (cols (and cols (== (length cols) ncols) (list-and cols) cols))
         (wide (if (or wide? twidth)
                   `((twith "table-width" ,(or twidth "1par"))
                     (twith "table-hmode" "exact")
                     (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
                     ,@(if (not cols) '()
                           (append-map
                             (lambda (part j)
                               (with js (number->string j)
                                 `((cwith "1" "-1" ,js ,js "cell-hmode" "exact")
                                   (cwith "1" "-1" ,js ,js "cell-width"
                                          ,(string-append
                                             (number->string
                                               (/ (round (* 1000.0 part
                                                            (oftm-part twidth)))
                                                  1000.0))
                                             "par")))))
                             cols (map (lambda (j) (+ j 1)) (iota ncols)))))
                   '()))
         (table `(,(if block? 'block 'tabular)
                  (tformat ,@wide ,@(reverse formats) ,@aligns ,@lines ,@fills
                           (table ,@trows))))
         (align (ox-attr x 'align)))
    (cond ((== align "center") `(with "par-mode" "center" ,table))
          ((== align "right") `(with "par-mode" "right" ,table))
          (else table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define oftm-sections
  '(section subsection subsubsection paragraph subparagraph))

(define (oftm-heading x)
  (let* ((n (office-integer (ox-attr x 'level) 1 1 5))
         (tag (list-ref oftm-sections (- n 1)))
         ;; a heading which says that it has no number
         (tag (if (== (ox-attr x 'numbered) "no")
                  (string->symbol (string-append (symbol->string tag) "*"))
                  tag))
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
  (and (func? x 'p) (in? (ox-attr x 'role) '(#f "figure"))
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

(define (oftm-trim-right s)
  (let loop ((i (string-length s)))
    (if (and (> i 0) (char=? (string-ref s (- i 1)) #\space))
        (loop (- i 1))
        (substring s 0 i))))

(define (oftm-description l)
  ;; (description . rest) for the terms and their definitions at the
  ;; start of the blocks l
  (let loop ((l l) (acc '()))
    (cond ((and (pair? l) (oftm-role? (car l) "term"))
           (let* ((term (oftm-inlines (ox-children (car l))))
                  (defs (let sub ((r (cdr l)) (d '()))
                          (if (and (pair? r) (oftm-role? (car r) "definition"))
                              (sub (cdr r)
                                   (cons (oftm-inlines (ox-children (car r))) d))
                              (cons (reverse d) r))))
                  (body (car defs)))
             (loop (cdr defs)
                   (append (reverse
                             (cons (oftm-concat
                                     (cons `(item* ,term)
                                           (cond ((null? body) '())
                                                 ((func? (car body) 'concat)
                                                  (cdar body))
                                                 (else (list (car body))))))
                                   (if (null? body) '() (cdr body))))
                           acc))))
          (else (cons `(description ,(oftm-document (reverse acc))) l)))))

(define (oftm-caption x)
  ;; the text of a caption, without "Figure 1." at its start
  (with start (oftm-named-start (ox-children x))
    (if (and start (cadddr start))
        (oftm-concat (append (oftm-labels (cadr start))
                             (append-map oftm-inline (caddr start))))
        (oftm-inlines (ox-children x)))))

(define (oftm-blocks l)
  ;; the paragraphs of TeXmacs for a list of blocks. The blocks which
  ;; belong together are taken together: the lines of a quotation or of a
  ;; piece of code, a figure or a table and its caption. (A loop: a
  ;; document may have many thousands of blocks)
  (let loop ((l l) (acc '()))
    (if (null? l) (reverse acc)
        (with r (oftm-block l)
          (loop (cdr r) (append (reverse (car r)) acc))))))

(define (oftm-block l)
  ;; the paragraphs for the first blocks of l, and the blocks which remain
  (cond ((not (pair? (car l))) (cons '() (cdr l)))
        ;; a quotation: the paragraphs of this style which follow each other
        ((oftm-role? (car l) "quote")
         (let loop ((r l) (acc '()))
           (if (and (pair? r) (oftm-role? (car r) "quote"))
               (loop (cdr r) (cons (oftm-inlines (ox-children (car r))) acc))
               (cons (list `(quotation ,(oftm-document (reverse acc)))) r))))
        ;; code: each paragraph is a line, or several
        ((oftm-role? (car l) "code")
         (let loop ((r l) (acc '()))
           (if (and (pair? r) (oftm-role? (car r) "code"))
               (loop (cdr r)
                     (append (reverse (map oftm-trim-right
                                           (string-tokenize-by-char
                                             (oftm-plain (car r)) #\newline)))
                             acc))
               (cons (list `(verbatim-code ,(oftm-document (reverse acc)))) r))))
        ;; a table of contents: TeXmacs makes it again from the headings
        ((or (func? (car l) 'toc) (oftm-role? (car l) "toc"))
         (let loop ((r l))
           (if (and (pair? r) (or (func? (car r) 'toc) (oftm-role? (car r) "toc")))
               (loop (cdr r))
               (cons '((table-of-contents "toc" (document ""))) r))))
        ;; a theorem, a remark, a proof
        ((and (func? (car l) 'p)
              (in? (ox-attr (car l) 'role) '("theorem" "remark" "proof"))
              (oftm-theorem l))
         => (lambda (r) (cons (list (car r)) (cdr r))))
        ;; the bibliography, without the heading which TeXmacs writes
        ((oftm-role? (car l) "bibitem")
         (with r (oftm-bibliography l)
           (cons (list (car r)) (cdr r))))
        ((and (oftm-role? (car l) "heading") (pair? (cdr l))
              (oftm-role? (cadr l) "bibitem")
              (in? (locase-all (oftm-plain (car l))) '("bibliography" "references")))
         (cons '() (cdr l)))
        ;; terms and their definitions
        ((oftm-role? (car l) "term")
         (with r (oftm-description l)
           (cons (list (car r)) (cdr r))))
        ;; a figure and its caption, in this order or in the other
        ((and (oftm-only-images? (car l)) (pair? (cdr l))
              (oftm-role? (cadr l) "caption"))
         (cons (list `(big-figure ,(oftm-inlines (ox-children (car l)))
                                  ,(oftm-caption (cadr l))))
               (cddr l)))
        ((and (oftm-role? (car l) "caption") (pair? (cdr l))
              (oftm-only-images? (cadr l)))
         (cons (list `(big-figure ,(oftm-inlines (ox-children (cadr l)))
                                  ,(oftm-caption (car l))))
               (cddr l)))
        ;; a table and its caption
        ((and (func? (car l) 'table) (pair? (cdr l))
              (oftm-role? (cadr l) "caption"))
         (cons (list `(big-table ,(oftm-table (car l)) ,(oftm-caption (cadr l))))
               (cddr l)))
        ((and (oftm-role? (car l) "caption") (pair? (cdr l))
              (func? (cadr l) 'table))
         (cons (list `(big-table ,(oftm-table (cadr l)) ,(oftm-caption (car l))))
               (cddr l)))
        (else
          (let ((x (car l)))
            (cons
              (case (car x)
                ((p)
                 (cond ((oftm-role? x "heading") (oftm-heading x))
                       ((oftm-role? x "skip") '())
                       ;; a formula with its number: an equation
                       ((oftm-numbered-formula x)
                        => (lambda (r)
                             (with f (oftm-formula (car r))
                               (if (== f "") '()
                                   (list `(equation
                                            (document
                                              ,(oftm-concat
                                                 (append (if (func? f 'concat) (cdr f)
                                                             (list f))
                                                         (oftm-labels (cadr r)))))))))))
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
                ;; a file which could not be read says so
                ((error)
                 (list `(with "color" "red"
                          ,(oftm-text (apply string-append
                                             (list-filter (cdr x) string?))))))
                (else '()))
              (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The title of the document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define oftm-title-roles
  '("title" "subtitle" "author" "date" "abstract" "skip"))

(define (oftm-title-block? x)
  (and (func? x 'p) (in? (ox-attr x 'role) oftm-title-roles)))

(define (oftm-lines l)
  ;; several paragraphs as the lines of one
  (oftm-concat
    (let loop ((l l) (acc '()))
      (if (null? l) (reverse acc)
          (loop (cdr l)
                (append (reverse (if (func? (car l) 'concat) (cdar l)
                                     (list (car l))))
                        (if (null? acc) acc (cons '(next-line) acc))))))))

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
      ;; (a title, a subtitle or a date of several paragraphs has lines)
      (if (and (null? titles) (null? subtitles) (null? authors) (null? dates)) '()
          `((doc-data
              ,@(if (null? titles) '() `((doc-title ,(oftm-lines titles))))
              ,@(if (null? subtitles) '()
                    `((doc-subtitle ,(oftm-lines subtitles))))
              ,@(map (lambda (a) `(doc-author (author-data (author-name ,a))))
                     authors)
              ,@(if (null? dates) '() `((doc-date ,(oftm-lines dates)))))))
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
    (set! oftm-numbered-labels (make-ahash-table))
    (with r (catch #t
              (lambda ()
                (for-each oftm-scan-labels l)
                `(document (body ,(oftm-body l)) (style "generic")))
              ;; (what was not foreseen gives a document which says so)
              (lambda args
                (with msg (string-append "This document could not be converted ("
                                         (object->string (car args)) ")")
                  (debug-message "convert-error" (string-append msg "\n"))
                  `(document (body (document (with "color" "red" ,msg)))
                             (style "generic")))))
      (set! oftm-numbered-labels (make-ahash-table))
      r)))
