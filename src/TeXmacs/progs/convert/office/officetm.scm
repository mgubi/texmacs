
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
        ((and (pair? x) (in? (car x) '(note image math bookmark box))) "")
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
        ;; a bracket without the other one, kept apart (see omml-delimiters)
        ((and (func? t 'with 3) (== (cadr t) "math-level") (== (caddr t) "0")
              (func? (cadddr t) 'around*))
         (oftm-clean-math (cadddr t)))
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

(define (oftm-local x)
  ;; the name of the element x without its prefix
  (let* ((s (if (and (pair? x) (symbol? (car x))) (symbol->string (car x)) ""))
         (i (string-search-forwards ":" 0 s)))
    (if (>= i 0) (substring s (+ i 1) (string-length s)) s)))

(define (oftm-eqarray-table x)
  ;; the table of MathML of a formula which is lines of equations (the
  ;; class tm-eqarray, on the formula or on its table), or #f
  (let* ((c (ox-children x))
         (root (cond ((null? c) #f)
                     ((== (ox-attr x 'form) "sxml") (car c))
                     ((and (not (ox-attr x 'form)) (string? (car c))
                           (>= (string-search-forwards "tm-eqarray" 0 (car c)) 0))
                      (catch #t (lambda () (ox-root (parse-xml (car c))))
                             (lambda args #f)))
                     (else #f)))
         (table? (lambda (y) (and (pair? y) (== (oftm-local y) "mtable"))))
         (rows (and root (ox-elements root)))
         (t (and rows (list-1? rows) (table? (car rows)) (car rows))))
    (and t (or (== (ox-attr root 'class) "tm-eqarray")
               (== (ox-attr t 'class) "tm-eqarray"))
         t)))

(define (oftm-mathml-prefix x)
  ;; the elements of MathML with the prefix m: (OpenDocument has none)
  (if (and (pair? x) (symbol? (car x)) (not (== (car x) '@))
           (not (string-starts? (symbol->string (car x)) "m:")))
      (cons (string->symbol (string-append "m:" (oftm-local x)))
            (map oftm-mathml-prefix (cdr x)))
      (if (pair? x) (cons (car x) (map oftm-mathml-prefix (cdr x))) x)))

(define (oftm-eqarray t)
  ;; lines of equations, each cell a formula
  (let* ((cell (lambda (d)
                 (oftm-formula `(math (@ (form "sxml"))
                                      (m:math ,@(map oftm-mathml-prefix (ox-children d)))))))
         (rows (map (lambda (r)
                      (cons 'row (map (lambda (d) (list 'cell (cell d)))
                                      (list-filter (ox-elements r)
                                                   (lambda (d) (== (oftm-local d) "mtd"))))))
                    (list-filter (ox-elements t)
                                 (lambda (r) (in? (oftm-local r) '("mtr" "mlabeledtr")))))))
    `(eqnarray* (document (tformat (table ,@rows))))))

(define (oftm-math x)
  ;; A formula in a line of text (a heading, a caption, a cell, a
  ;; paragraph): in the text, also when it is displayed in the file. (A
  ;; displayed formula is a paragraph of its own: those of a paragraph with
  ;; text are made paragraphs by oftm-split-display, the paragraph of a
  ;; formula alone is an equation in oftm-block.)
  (oftm-math* (oftm-inline-formula x)))

(define (oftm-inline-formula x)
  ;; the node math x, not displayed
  (if (!= (ox-attr x 'display) "true") x
      (apply office-node
             (cons* 'math (map (lambda (a) (list (car a) (cadr a)))
                               (list-filter (ox-attrs x)
                                            (lambda (a) (!= (car a) 'display))))
                    (ox-children x)))))

(define (oftm-display? y)
  (and (func? y 'math) (== (ox-attr y 'display) "true")))

;; A paragraph with text and displayed formulas: the paragraphs of its
;; text and the paragraph of each formula, which is an equation (not those
;; of a cell, nor a heading, a caption..., whose formulas are in the text;
;; nor a paragraph which is one formula with its number). The punctuation
;; after a formula and its number go with it; the bookmarks just before it
;; are its labels; the other bookmarks of a piece without text go with the
;; formula after them, or with the last paragraph. The space before the
;; paragraph and the indentation of its first line are those of its first
;; piece, the space after it is the one of its last piece.

(define (oftm-blank? y)
  (or (and (string? y) (== (string-trim-spaces y) "")) (func? y 'tab)))

(define (oftm-split-display x)
  (let ((l (if (and (func? x 'p) (not (ox-attr x 'role)) (not oftm-in-cell?))
               (ox-children x) '())))
    (if (not (and (list-or (map oftm-display? l)) (not (oftm-numbered-formula x))))
        (list x)
        (with pieces (oftm-display-pieces l)
          (if (or (null? pieces) (null? (cdr pieces))) (list x)
              (oftm-piece-paragraphs x pieces))))))

(define (oftm-display-pieces l)
  ;; the pieces (text kid...) and (formula kid...) of the children l of a
  ;; paragraph (in one pass: each child is looked at a bounded number of
  ;; times)
  (let loop ((l l) (cur '()) (content? #f) (acc '()))
    ;; cur: the piece of text so far, newest first; content?: whether it
    ;; holds more than spaces and bookmarks
    (cond ((null? l)
           (cond (content? (reverse (cons (cons 'text (reverse cur)) acc)))
                 ((or (null? acc) (null? (oftm-marks cur))) (reverse acc))
                 (else (reverse (cons (append (car acc) (reverse (oftm-marks cur)))
                                      (cdr acc))))))
          ((oftm-display? (car l))
           (let* ((split (oftm-split-labels cur content?))
                  (acc (if content? (cons (cons 'text (reverse (cdr split))) acc) acc))
                  (after (oftm-after-formula (cdr l))))
             (loop (cdr after) '() #f
                   (cons (append (list 'formula (car l)) (car split) (car after))
                         acc))))
          (else (loop (cdr l) (cons (car l) cur)
                      (or content? (not (or (oftm-blank? (car l))
                                            (func? (car l) 'bookmark))))
                      acc)))))

(define (oftm-marks l)
  (list-filter l (lambda (y) (func? y 'bookmark))))

(define (oftm-split-labels cur content?)
  ;; (labels . rest) for the piece cur (newest first) before a formula: its
  ;; bookmarks at its end, or all of them when it has no text
  (if (not content?) (cons (reverse (oftm-marks cur)) '())
      (let split ((before cur) (labels '()))
        (if (and (pair? before) (or (oftm-blank? (car before))
                                    (func? (car before) 'bookmark)))
            (split (cdr before)
                   (if (func? (car before) 'bookmark) (cons (car before) labels) labels))
            (cons labels before)))))

(define (oftm-leading-punctuation s)
  ;; (punctuation . rest) for a string s which starts with punctuation (a
  ;; point before a digit is not: 3.5, .5)
  (and (string? s)
       (let* ((n (string-length s))
              (i (let skip ((i 0))
                   (if (and (< i n) (char=? (string-ref s i) #\space)) (skip (+ i 1)) i)))
              (j (let more ((j i))
                   (if (and (< j n) (in? (string-ref s j) '(#\. #\, #\; #\: #\! #\?))
                            (not (and (char=? (string-ref s j) #\.) (< (+ j 1) n)
                                      (char-numeric? (string-ref s (+ j 1))))))
                       (more (+ j 1)) j))))
         (and (> j i) (cons (substring s i j) (substring s j n))))))

(define oftm-text-wrappers
  '(em strong underline strike overline smallcaps color size font span mark
    sub sup caps letters))

(define (oftm-wrapper-punctuation y)
  ;; (punctuation . rest) for a node y which wraps text (em...) and starts
  ;; with punctuation: rest is the node without it, or #f when nothing
  ;; else is in it
  (and (pair? y) (in? (car y) oftm-text-wrappers)
       (let* ((kids (ox-children y))
              (q (and (pair? kids) (oftm-leading-punctuation (car kids)))))
         (and q
              (let ((more (if (== (cdr q) "") (cdr kids) (cons (cdr q) (cdr kids)))))
                (cons (car q)
                      (and (not (list-and (map oftm-blank? more)))
                           (apply office-node
                                  (cons* (car y)
                                         (map (lambda (a) (list (car a) (cadr a)))
                                              (ox-attrs y))
                                         more)))))))))

(define (oftm-chapter-field? y)
  ;; the number of a chapter in the number of a formula (STYLEREF 1 \s:
  ;; with a switch for a number, not the text of the heading)
  (and (func? y 'field)
       (with code (locase-all (tm-string-trim (or (ox-attr y 'code) "")))
         (and (string-starts? code "styleref")
              (list-or (map (lambda (sw) (>= (string-search-forwards sw 0 code) 0))
                            '("\\s" "\\n" "\\r" "\\w")))))))

(define (oftm-number-string? y)
  ;; a string of the brackets and the separators of a number (not a point
  ;; or a dash after its closing bracket, which are text)
  (and (string? y)
       (with s (string-trim-spaces y)
         (and (!= s "")
              (list-and (map (lambda (c) (in? c '(#\( #\) #\[ #\] #\. #\-)))
                             (string->list s)))
              (with l (reverse (string->list s))
                (not (and (in? (car l) '(#\. #\-))
                          (list-or (map (lambda (c) (in? c '(#\) #\]))) l)))))))))

(define (oftm-bracket-string? y)
  ;; a string of a number with a bracket
  (and (oftm-number-string? y)
       (list-or (map (lambda (c) (in? c '(#\( #\) #\[ #\])))
                     (string->list y)))))

(define (oftm-bracket-punctuation y)
  ;; the punctuation of a string y of closing brackets and punctuation
  ;; after them (the number of a formula, then the point of the sentence),
  ;; or #f
  (with q (oftm-closing-brackets y)
    (and q (oftm-punctuation-string? (cdr q)) (string-trim-spaces (cdr q)))))

(define (oftm-number-part? y)
  (or (oftm-blank? y) (func? y 'seq) (func? y 'bookmark) (oftm-chapter-field? y)
      (oftm-number-string? y)))

(define (oftm-closing-brackets s)
  ;; (brackets . rest) for a string s which starts with closing brackets,
  ;; or #f
  (and (string? s)
       (let* ((n (string-length s))
              (i (let skip ((i 0))
                   (if (and (< i n) (char=? (string-ref s i) #\space)) (skip (+ i 1)) i)))
              (j (let more ((j i))
                   (if (and (< j n) (in? (string-ref s j) '(#\) #\])))
                       (more (+ j 1)) j))))
         (and (> j i) (cons (substring s i j) (substring s j n))))))

(define (oftm-after-formula l)
  ;; (nodes . rest) for the children l after a displayed formula: the
  ;; nodes which go with it are its punctuation (also after bookmarks,
  ;; which are its labels, and at the start of a node which wraps text),
  ;; and its number (a field seq after tabs, in brackets, perhaps after
  ;; the number of the chapter)
  (let* ((p (let absorb ((r l) (dots "") (marks '()) (pending '()))
              ;; marks: the bookmarks before punctuation, pending: those
              ;; after it so far (newest first)
              (let ((stop (lambda (r)
                            (list dots (reverse marks) (append (reverse pending) r)))))
                (cond ((null? r) (stop r))
                      ((and (string? (car r)) (oftm-blank? (car r)))
                       (absorb (cdr r) dots marks pending))
                      ((func? (car r) 'bookmark)
                       (absorb (cdr r) dots marks (cons (car r) pending)))
                      ;; (a note just after the formula is its note, and
                      ;; the punctuation after it goes with the formula)
                      ((func? (car r) 'note)
                       (absorb (cdr r) dots (cons (car r) (append pending marks)) '()))
                      ((oftm-leading-punctuation (car r))
                       => (lambda (q)
                            (let ((dots (string-append dots (car q)))
                                  (marks (append pending marks)))
                              (if (oftm-blank? (cdr q))
                                  (absorb (cdr r) dots marks '())
                                  (list dots (reverse marks) (cons (cdr q) (cdr r)))))))
                      ((oftm-wrapper-punctuation (car r))
                       => (lambda (q)
                            (let ((dots (string-append dots (car q)))
                                  (marks (append pending marks)))
                              (if (not (cdr q))
                                  (absorb (cdr r) dots marks '())
                                  (list dots (reverse marks) (cons (cdr q) (cdr r)))))))
                      (else (stop r))))))
         (with-dots (lambda (d) (append (if (== d "") '() (list d)) (cadr p))))
         (dots (with-dots (car p)))
         (r (caddr p)))
    ;; (started: whether the number has started, with a bracket, a field;
    ;; a dash or a point before it is text)
    (let num ((s r) (seen '()) (seq? #f) (started? #f))
      (cond ((and (pair? s) (oftm-number-part? (car s))
                  (or started? (not (oftm-number-string? (car s)))
                      (oftm-bracket-string? (car s))))
             (num (cdr s) (cons (car s) seen) (or seq? (func? (car s) 'seq))
                  (or started? (func? (car s) 'seq) (oftm-chapter-field? (car s))
                      (oftm-bracket-string? (car s)))))
            ;; (the closing bracket of the number, before more text; the
            ;; punctuation just after it goes with the formula)
            ((and seq? (pair? s) (oftm-closing-brackets (car s)))
             => (lambda (q)
                  (let* ((pq (oftm-leading-punctuation (cdr q)))
                         (d (if pq (string-append (car p) (car pq)) (car p)))
                         (rest (if pq (cdr pq) (cdr q))))
                    (cons (append (with-dots d) (reverse (cons (car q) seen)))
                          (if (oftm-blank? rest) (cdr s) (cons rest (cdr s)))))))
            (seq? (cons (append dots (reverse seen)) s))
            (else (cons dots r))))))

(define (oftm-piece-paragraphs x pieces)
  ;; the paragraphs of the pieces of the paragraph x
  (let* ((attrs (map (lambda (a) (list (car a) (cadr a))) (ox-attrs x)))
         (n (length pieces))
         (without (lambda (keys)
                    (list-filter attrs (lambda (a) (not (in? (car a) keys)))))))
    (let loop ((l pieces) (i 0) (acc '()))
      (if (null? l) (reverse acc)
          (let* ((keys (append (if (> i 0) '(space-before first-indent) '())
                               (if (< i (- n 1)) '(space-after) '())))
                 (kids (if (eq? (caar l) 'text) (oftm-trim-piece (cdar l)) (cdar l))))
            (loop (cdr l) (+ i 1)
                  (cons (apply office-node (cons* 'p (without keys) kids)) acc)))))))

(define (oftm-punctuation-string? y)
  ;; a string of the punctuation after a formula
  (and (string? y)
       (with s (string-trim-spaces y)
         (and (!= s "")
              (list-and (map (lambda (c) (in? c '(#\. #\, #\; #\: #\! #\?)))
                             (string->list s)))))))

(define (oftm-trim-side l left?)
  ;; the nodes l of a piece of a paragraph without the spaces at its start
  ;; (or at its end), also inside the nodes which wrap its text
  (let* ((r (if left? l (reverse l)))
         (x (and (pair? r) (car r)))
         (trim (lambda (s) (if left? (tm-string-trim s) (tm-string-trim-right s))))
         (x2 (cond ((string? x) (trim x))
                   ((and (pair? x) (not (in? (car x) '(math image note bookmark box br tab))))
                    (with kids (oftm-trim-side (ox-children x) left?)
                      (if (null? (ox-attrs x)) (cons (car x) kids)
                          (cons* (car x) (cons '@ (ox-attrs x)) kids))))
                   (else x)))
         (r (cond ((not x) r)
                  ((== x2 "") (cdr r))
                  (else (cons x2 (cdr r))))))
    (if left? r (reverse r))))

(define (oftm-trim-piece l)
  (oftm-trim-side (oftm-trim-side l #t) #f))

(define (oftm-math* x)
  (with f (oftm-formula x)
    (cond ((== f "") '())
          ((== (ox-attr x 'display) "true")
           (list `(equation* (document ,f))))
          (else (list `(math ,f))))))

(define (oftm-cropped data name w h crop)
  ;; the image of which the parts crop ("l t r b", fractions of its sides)
  ;; are cut: the whole image, larger, inside the rectangle which is shown
  (let* ((c (map (lambda (v) (or (office-number v) 0))
                 (string-tokenize-by-char crop #\space)))
         (c (if (== (length c) 4) c '(0 0 0 0)))
         (l (car c)) (t (cadr c)) (r (caddr c)) (b (cadddr c))
         (cm (lambda (v) (string-append (office-decimal v) "cm")))
         (fw (/ (office-length->cm w) (max 0.05 (- 1 l r))))
         (fh (/ (office-length->cm h) (max 0.05 (- 1 t b))))
         (u (lambda (v unit) (string-append (office-decimal v) unit))))
    `(clipped (image (tuple (raw-data ,data) ,name) ,(cm fw) ,(cm fh) "" "")
              ,(u l "w") ,(u b "h") ,(u (- 1 r) "w") ,(u (- 1 t) "h"))))

(define (oftm-image x)
  ;; a picture; one which floats beside its paragraph says where, in the
  ;; style office
  (with l (oftm-image-only x)
    (if (and oftm-faithful? (ox-attr x 'float) (pair? l))
        (list `(office-float ,(ox-attr x 'float) ,(oftm-float-spec x)
                             ,(if (null? (cdr l)) (car l) `(concat ,@l))))
        l)))

(define (oftm-float-spec x . keys)
  ;; the attributes of a floating picture but its side (and those of the
  ;; list keys), as "key=value ..."
  (string-recompose
    (append-map (lambda (k)
                  (with v (ox-attr x k)
                    (if (and v (!= v "")) (list (string-append (symbol->string k) "=" v)) '())))
                (append (cdr office-float-keys) (if (pair? keys) (car keys) '()) '(next)))
    " "))

;; A box of text or a shape: TeXmacs shows the box as a table of one cell
;; of its width, with its fill and its line, on its side; a shape without
;; text as a drawing. In the style office, office-box and office-shape
;; keep the rest.

(define (oftm-color c) (and (string? c) (string-starts? c "#") c))

(define (oftm-box-table width fill line body)
  `(tabular
     (tformat
       ,@(if (office-length->cm width)
             `((twith "table-width" ,width) (twith "table-hmode" "exact"))
             '())
       (cwith "1" "1" "1" "1" "cell-hyphen" "t")
       ,@(if (oftm-color fill) `((cwith "1" "1" "1" "1" "cell-background" ,fill)) '())
       ,@(if (oftm-color line)
             (map (lambda (b) `(cwith "1" "1" "1" "1" ,b "1ln"))
                  '("cell-lborder" "cell-rborder" "cell-tborder" "cell-bborder"))
             '())
       (table (row (cell ,body))))))

(define (oftm-shape-drawing shape width height fill line)
  ;; a drawing of the shape, of its size (centered on the origin)
  (let* ((w (or (office-length->cm width) 2.0))
         (h (or (office-length->cm height) 1.0))
         (x (/ w 2)) (y (/ h 2))
         (pt (lambda (a b) `(point ,(office-decimal a) ,(office-decimal b))))
         (obj (cond ((== shape "ellipse")
                     `(cspline ,@(map (lambda (k)
                                        (with t (* k 0.7853981633974483)
                                          (pt (* x (cos t)) (* y (sin t)))))
                                      '(0 1 2 3 4 5 6 7))))
                    ((== shape "triangle")
                     `(cline ,(pt (- x) (- y)) ,(pt x (- y)) ,(pt 0 y)))
                    ((== shape "diamond")
                     `(cline ,(pt 0 (- y)) ,(pt x 0) ,(pt 0 y) ,(pt (- x) 0)))
                    (else
                     `(cline ,(pt (- x) (- y)) ,(pt x (- y)) ,(pt x y) ,(pt (- x) y))))))
    `(with "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))
           "gr-geometry" (tuple "geometry" ,(office-cm w) ,(office-cm h) "center")
       (graphics
         (with "color" ,(or (oftm-color line) (oftm-color fill) "black")
               ,@(if (oftm-color fill) `("fill-color" ,fill) '())
           ,obj)))))

(define (oftm-box x)
  (let* ((side (or (ox-attr x 'float) ""))
         (width (or (ox-attr x 'width) ""))
         (fill (or (ox-attr x 'fill) ""))
         (line (or (ox-attr x 'line) ""))
         (blocks (oftm-blocks (ox-children x)))
         (spec (oftm-float-spec x '(height shape line-width inset grow))))
    (if (null? blocks)
        (with d (oftm-shape-drawing (ox-attr x 'shape) width (ox-attr x 'height) fill line)
          (list (if oftm-faithful?
                    `(office-shape ,side
                                   ,(oftm-float-spec x '(width height shape fill line line-width
                                                         inset grow))
                                   ,d)
                    d)))
        (with body (oftm-document blocks)
          (list (if oftm-faithful?
                    `(office-box ,side ,spec ,width ,fill ,line ,body)
                    (oftm-box-table width fill line body)))))))

(define (oftm-split-floats x)
  ;; a paragraph with text and pictures which float beside it: each picture
  ;; is a paragraph before it, on its side, and says that it goes with the
  ;; next paragraph (TeXmacs does not put text around a picture)
  (let* ((float? (lambda (y) (or (and (func? y 'image) (ox-attr y 'float))
                                 (func? y 'box))))
         (l (if (func? x 'p) (ox-children x) '()))
         (floats (list-filter l float?))
         (rest (list-filter l (lambda (y) (not (float? y))))))
    (cond
      ((or (null? floats) (oftm-role? x "heading")) (list x))
      ((not (list-or (map (lambda (y) (not (and (string? y)
                                                 (== (string-trim-spaces y) ""))))
                          rest)))
       ;; (a paragraph of floating things only: on their side)
       (with sides (map (lambda (y) (ox-attr y 'float)) floats)
         (if (and (not (ox-attr x 'align)) (car sides)
                  (list-and (map (lambda (s) (== s (car sides))) sides)))
             (list (apply office-node
                          (cons* 'p (append (map (lambda (a) (list (car a) (cadr a)))
                                                 (ox-attrs x))
                                            `((align ,(car sides))))
                                 (ox-children x))))
             (list x))))
      (else
        (append (map (lambda (y)
                       (office-node 'p `((align ,(ox-attr y 'float)))
                                    (office-add-attrs y '((next "true")))))
                     floats)
                (list (apply office-node
                             (cons* 'p (map (lambda (a) (list (car a) (cadr a)))
                                            (ox-attrs x))
                                    rest))))))))

(define (oftm-field-code s)
  ;; The instruction s of a field. A field inside it is between the
  ;; characters 1 and 2 (see docx-inlines-sub): it is a field without
  ;; result in the instruction, (concat " IF " (office-field " PAGE " "")
  ;; " = 3 ...").
  (let ((open (integer->char 1)) (close (integer->char 2)))
    (if (< (string-search-forwards (string open) 0 s) 0) (oftm-text s)
        ;; (pieces . rest of the characters), until the mark which closes
        (letrec ((parse
                   (lambda (l depth)
                     (let loop ((l l) (cs '()) (acc '()))
                       (let ((flush (lambda () (if (null? cs) acc
                                                   (cons (oftm-text (list->string (reverse cs)))
                                                         acc)))))
                         (cond ((null? l) (cons (reverse (flush)) '()))
                               ((char=? (car l) open)
                                (with r (parse (cdr l) (+ depth 1))
                                  (loop (if (pair? (cdr r)) (cddr r) '()) '()
                                        (cons `(office-field ,(oftm-concat (car r)) "")
                                              (flush)))))
                               ((char=? (car l) close)
                                (if (> depth 0) (cons (reverse (flush)) l)
                                    (loop (cdr l) cs acc)))
                               (else (loop (cdr l) (cons (car l) cs) acc))))))))
          (oftm-concat (car (parse (string->list s) 0)))))))

(define (oftm-metafiles l)
  ;; the (data name) of the pictures of Windows in the nodes l
  (let loop ((todo l) (acc '()))
    (cond ((null? todo) (reverse acc))
          ((not (pair? (car todo))) (loop (cdr todo) acc))
          ((and (func? (car todo) 'image) (ox-attr (car todo) 'data)
                (office-metafile? (ox-attr (car todo) 'name)))
           (loop (cdr todo) (cons (list (ox-attr (car todo) 'data) (ox-attr (car todo) 'name))
                                  acc)))
          ((func? (car todo) '@) (loop (cdr todo) acc))
          (else (loop (append (cdr (car todo)) (cdr todo)) acc)))))

(define (oftm-image-only x)
  (let* ((name (oftm-text (or (ox-attr x 'name) "")))
         (data (ox-attr x 'data))
         (w (or (ox-attr x 'width) ""))
         (h (or (ox-attr x 'height) "")))
    (cond ;; a picture of Windows: its PNG, or a frame with its name
          ((and data (office-metafile? name))
           (with png (office-metafile->png data name)
             (if png
                 (oftm-image-only `(image (@ (name ,(string-append name ".png")) (data ,png)
                                        ,@(list-filter (ox-attrs x)
                                                       (lambda (a) (not (in? (car a) '(name data))))))))
                 (list `(frame ,(string-append "[" name "]"))))))
          ((and data (ox-attr x 'crop) (office-length->cm w) (office-length->cm h))
           (list (oftm-cropped data name w h (ox-attr x 'crop))))
          (data (list `(image (tuple (raw-data ,data) ,name) ,w ,h "" "")))
          ;; a picture which the file links to on the web: an image of
          ;; this address (which the converters never fetch)
          ((office-web-address? name) (list `(image ,name ,w ,h "" "")))
          ;; (another address is not a file to load when the document is
          ;; opened, nor a file which a Word file links to)
          ((or (>= (string-search-forwards "://" 0 name) 0)
               (== (ox-attr x 'linked) "true")
               (string-starts? (locase-all name) "file:"))
           (list name))
          ((!= name "") (list `(image ,name ,w ,h "" "")))
          (else '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inline nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The alignment of the usual paragraphs of the document: the others say
;; theirs
(define oftm-align "justify")

;; In a document of the style office, what the standard tags do not say is
;; kept with the macros of the style (office.ts): the named styles, the
;; fields, the comments, the tracked changes. Else the look of the named
;; styles of text is written out, and the rest is left out.
(define oftm-faithful? #f)
(define oftm-styles (make-ahash-table))  ; name -> the node style
(define oftm-body-style #f)              ; the style of the usual paragraphs
(define oftm-base-size 10.0)             ; the size of the font of the text
(define oftm-base-font #f)               ; "sans" for a text without serifs

(define (oftm-style-text st t)
  ;; the text t in the look of the text of the node style st
  (let* ((get (lambda (k) (ox-attr st k)))
         (size (office-number (get 'size)))
         (vars (append
                 (if (get 'color) `("color" ,(get 'color)) '())
                 (if (get 'strong) '("font-series" "bold") '())
                 (cond ((get 'smallcaps) '("font-shape" "small-caps"))
                       ((get 'em) '("font-shape" "italic"))
                       (else '()))
                 (if (and size (> size 0) (< size 100)
                          (> (abs (- size oftm-base-size)) 0.1))
                     `("font-size" ,(office-decimal (/ size oftm-base-size)))
                     '())
                 (cond ((oftm-known-font (get 'font-name))
                        => (lambda (name) `("font" ,name)))
                       ((== (get 'font) "sans") '("font-family" "ss"))
                       ((== (get 'font) "mono") '("font-family" "tt"))
                       (else '()))))
         ;; (the kinds of underlines and of lines through, the capitals
         ;; and the spacing of the letters, as in the text)
         (t (cond ((not (get 'underline)) t)
                  ((get 'underline-kind) `(office-underline ,(get 'underline-kind) ,t))
                  (else `(underline ,t))))
         (t (if (get 'overline) `(overline ,t) t))
         (t (cond ((not (get 'strike)) t)
                  ((get 'dstrike) `(office-strike "double" ,t))
                  (else `(strike-through ,t))))
         (t (oftm-case (get 'caps) t))
         (t (if (office-number (get 'letters)) `(office-letters ,(get 'letters) ,t) t)))
    (if (null? vars) t `(with ,@vars ,t))))

(define (oftm-case kind t)
  ;; the text t in capitals, in lower case, or with the first letters of
  ;; its words in capitals (which the style office keeps, but does not show)
  (cond ((not kind) t)
        ((== kind "lower") `(change-case ,t "locase"))
        ((== kind "capital") (if oftm-faithful? `(office-capitalize ,t) t))
        (else `(change-case ,t "UPCASE"))))

(define (oftm-style-paragraph st t)
  ;; the paragraph t in the look of the node style st
  (oftm-paragraph-format st (oftm-style-text st t) #t))

(define (oftm-direct-paragraph x t)
  ;; the paragraph t with the formatting by hand of the node p x
  (oftm-paragraph-format x t #f))

(define (oftm-paragraph-format st t align?)
  ;; the paragraph t with the indentations, the spaces, the line spacing,
  ;; the background and the border which the attributes of st say
  (let* ((get (lambda (k) (ox-attr st k)))
         (pt (lambda (k) (with v (office-number (get k))
                           (and v (string-append (office-decimal v) "pt")))))
         (line (office-number (get 'line-spacing)))
         (vars (append
                 (if (and align? (in? (get 'align) '("left" "center" "right" "justify")))
                     `("par-mode" ,(get 'align)) '())
                 (if (pt 'indent-left) `("par-left" ,(pt 'indent-left)) '())
                 (if (pt 'indent-right) `("par-right" ,(pt 'indent-right)) '())
                 (if (pt 'first-indent) `("par-first" ,(pt 'first-indent)) '())
                 ;; (a line of TeXmacs is 1.2 times the size of its font)
                 (if (and line (> line 0) (< line 5))
                     `("par-sep" ,(string-append (office-decimal (max 0 (- (* 1.2 line) 1)))
                                                 "fn"))
                     '())))
         (t (if (or (get 'background) (get 'border))
                `(with "ornament-color" ,(or (get 'background) "#fff0")
                       "ornament-border" ,(if (get 'border) "1ln" "0ln")
                       (ornamented ,t))
                t))
         (t (if (or (pt 'space-before) (pt 'space-after))
                `(surround (vspace* ,(or (pt 'space-before) "0pt"))
                           (vspace ,(or (pt 'space-after) "0pt")) ,t)
                t)))
    (if (null? vars) t `(with ,@vars ,t))))

(define oftm-fonts #f)  ; the families of fonts which TeXmacs has, lower case

(define (oftm-known-font f)
  ;; the name of the family of fonts f, as TeXmacs knows it, or #f
  (and (string? f) (!= f "")
       (begin
         (when (not oftm-fonts)
           (set! oftm-fonts (make-ahash-table))
           (catch #t
             (lambda ()
               (for (fam (font-database-families))
                 (when (string? fam) (ahash-set! oftm-fonts (locase-all fam) fam))))
             (lambda args #f)))
         (ahash-ref oftm-fonts (locase-all f)))))

(define oftm-title-macros
  '("section-title" "subsection-title" "subsubsection-title" "paragraph-title"
    "subparagraph-title"))

(define (oftm-title-level st)
  ;; the level of the style of headings st, when TeXmacs has titles of
  ;; this level (the deeper ones keep the look of their paragraphs)
  (with n (office-integer (ox-attr st 'level) 0 0 9)
    (and (>= n 1) (<= n 5) n)))

(define (oftm-title-macro st)
  ;; the title of the sections of the level of the style of headings st,
  ;; in its look: the macro of the style of TeXmacs which typesets them
  (let* ((n (office-integer (ox-attr st 'level) 0 0 9))
         (pt (lambda (k) (with v (office-number (ox-attr st k))
                           (and v (string-append (office-decimal v) "pt")))))
         (body (oftm-style-text st '(arg "name")))
         (body (if (ox-attr st 'strong) body
                   `(with "font-series" "medium" ,body))))
    (and (>= n 1) (<= n 5)
         `(associate ,(list-ref oftm-title-macros (- n 1))
                     (macro "name"
                       (sectional-normal
                         (concat (vspace* ,(or (pt 'space-before) "1fn"))
                                 ,(if (ox-attr st 'strong) `(with "font-series" "bold" ,body)
                                      body)
                                 (vspace ,(or (pt 'space-after) "0.5fn")))))))))

(define (oftm-style-macros)
  ;; the macros of the named styles, for the initial environment
  (append
    (map (lambda (kv)
           (let* ((st (cdr kv))
                  (par? (== (ox-attr st 'family) "paragraph")))
             `(associate ,(string-append (if par? "office-par-" "office-span-") (cdar kv))
                         (macro "body" ,(if par? (oftm-style-paragraph st '(arg "body"))
                                            (oftm-style-text st '(arg "body")))))))
         (sort (list-filter (ahash-table->list oftm-styles)
                            (lambda (kv) (not (oftm-title-level (cdr kv)))))
               oftm-style-key<?))
    ;; the titles of the sections, in the look of the headings of the file
    (list-filter (map (lambda (kv) (oftm-title-macro (cdr kv)))
                      (sort (list-filter (ahash-table->list oftm-styles)
                                         (lambda (kv) (oftm-title-level (cdr kv))))
                            oftm-style-key<?))
                 identity)))

(define (oftm-style-key<? a b)
  ;; the order of the entries ((family . name) . style): by name, then by
  ;; family
  (or (string<? (cdar a) (cdar b))
      (and (== (cdar a) (cdar b)) (string<? (caar a) (caar b)))))

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

(define oftm-moved-labels (make-ahash-table))  ; labels with their place

(define (oftm-label-name name)
  ;; the label of a bookmark, or #f: a label which was away from its number
  ;; is where its bookmark _TmAt_name is, and not at its number
  (cond ((not name) #f)
        ((string-starts? name "_TmAt_") (substring name 6 (string-length name)))
        ((ahash-ref oftm-moved-labels name) #f)
        (else name)))

(define (oftm-bookmarks* x)
  ;; the names of the bookmarks inside x, but those of its notes
  (cond ((not (pair? x)) '())
        ((in? (car x) '(math image @ note)) '())
        ((func? x 'bookmark) (if (ox-attr x 'name) (list (ox-attr x 'name)) '()))
        (else (append-map oftm-bookmarks* (ox-children x)))))

(define (oftm-bookmarks x)
  ;; the labels of the bookmarks inside x
  (list-filter (map oftm-label-name (oftm-bookmarks* x)) identity))

(define (oftm-scan-labels x)
  (cond ((not (pair? x)) (noop))
        ((func? x 'bookmark)
         (with name (ox-attr x 'name)
           (when (and name (string-starts? name "_TmAt_"))
             (ahash-set! oftm-moved-labels (substring name 6 (string-length name)) #t))))
        ((func? x 'p)
         (when (or (in? (ox-attr x 'role)
                        '("heading" "caption" "theorem" "remark" "proof" "bibitem"))
                   (oftm-has? x 'seq))
           (for (name (oftm-bookmarks* x))
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
            ((and (func? (car r) 'strong) (not (ox-attr (car r) 'value))
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
  ;; with its number after it (perhaps with the number of the chapter),
  ;; and the punctuation after the formula or after the number; else #f
  (let* ((l (list-filter (ox-children x) (lambda (y) (not (oftm-blank? y))))))
    (and (pair? l) (func? (car l) 'math)
         (let loop ((r (cdr l)) (started? #f) (seq? #f))
           (cond ((null? r) seq?)
                 ((func? (car r) 'seq) (loop (cdr r) #t #t))
                 ((oftm-chapter-field? (car r)) (loop (cdr r) #t seq?))
                 ((func? (car r) 'bookmark) (loop (cdr r) started? seq?))
                 ;; (the notes of the formula, before its number)
                 ((and (func? (car r) 'note) (not started?)) (loop (cdr r) started? seq?))
                 ((oftm-bracket-string? (car r)) (loop (cdr r) #t seq?))
                 ((oftm-punctuation-string? (car r)) (loop (cdr r) started? seq?))
                 ((and started? (or (oftm-number-string? (car r))
                                    (oftm-bracket-punctuation (car r))))
                  (loop (cdr r) started? seq?))
                 (else #f)))
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
              ((strong)
               ;; (bold by its style, not by hand: medium)
               (if (== (ox-attr x 'value) "off")
                   (with t (oftm-inlines l)
                     (if (== t "") '() (list `(with "font-series" "medium" ,t))))
                   (oftm-wrap 'strong l)))
              ((overline) (oftm-wrap 'overline l))
              ;; an underline or a line through of a kind (double, wave...)
              ((underline strike)
               (let ((t (oftm-inlines l))
                     (kind (ox-attr x 'value)))
                 (cond ((== t "") '())
                       ((and kind oftm-faithful?)
                        (list `(,(if (func? x 'underline) 'office-underline 'office-strike)
                                ,kind ,t)))
                       ((func? x 'underline) (list `(underline ,t)))
                       (else (list `(strike-through ,t))))))
              ;; capitals, the spacing of the letters
              ((caps)
               (with t (oftm-inlines l)
                 (if (== t "") '() (list (oftm-case (or (ox-attr x 'value) "true") t)))))
              ((letters)
               (with t (oftm-inlines l)
                 (cond ((== t "") '())
                       ((and oftm-faithful? (ox-attr x 'value))
                        (list `(office-letters ,(ox-attr x 'value) ,t)))
                       (else (list t)))))
              ((sub) (oftm-wrap 'rsub l))
              ((sup) (oftm-wrap 'rsup l))
              ((code) (oftm-wrap 'verbatim l))
              ((mark)
               (with t (oftm-inlines l)
                 (cond ((== t "") '())
                       ((ox-attr x 'value) (list `(with "marked-color" ,(ox-attr x 'value)
                                                    (marked ,t))))
                       (else (list `(marked ,t))))))
              ;; the size and the font of a text, by hand
              ((size)
               (let ((t (oftm-inlines l))
                     (v (office-number (or (ox-attr x 'value) ""))))
                 (cond ((== t "") '())
                       ((and v (> v 0) (< v 200) (> (abs (- v oftm-base-size)) 0.2))
                        (list `(with "font-size" ,(office-decimal (/ v oftm-base-size)) ,t)))
                       (else (list t)))))
              ((font)
               (let ((t (oftm-inlines l))
                     (f (ox-attr x 'value)))
                 (cond ((== t "") '())
                       ;; a font which TeXmacs has, by its name
                       ((oftm-known-font f) => (lambda (name) (list `(with "font" ,name ,t))))
                       ;; another one: its name is kept in the style office
                       ((and oftm-faithful? f (not (office-monospace? f)))
                        (list `(office-font ,f
                                            ,(cond ((office-sans? f) "ss")
                                                   (else "rm"))
                                            ,t)))
                       ((and (office-sans? f) (!= oftm-base-font "sans"))
                        (list `(with "font-family" "ss" ,t)))
                       ((and f (not (office-sans? f)) (not (office-monospace? f))
                             (== oftm-base-font "sans"))
                        (list `(with "font-family" "rm" ,t)))
                       (else (list t)))))
              ((color)
               (let ((t (oftm-inlines l))
                     (v (ox-attr x 'value)))
                 (cond ((== t "") '())
                       (v (list `(with "color" ,v ,t)))
                       (else (list t)))))
              ((smallcaps)
               (with t (oftm-inlines l)
                 (if (== t "") '() (list `(with "font-shape" "small-caps" ,t)))))
              ;; a named style of text
              ((span)
               (let ((t (oftm-inlines l))
                     (st (ahash-ref oftm-styles (cons "text" (or (ox-attr x 'style) "")))))
                 (cond ((== t "") '())
                       (oftm-faithful? (list `(office-span ,(oftm-text (ox-attr x 'style)) ,t)))
                       (st (list (oftm-style-text st t)))
                       (else (list t)))))
              ;; a field, a comment, the changes which are tracked
              ((field)
               (with t (oftm-inlines l)
                 (if oftm-faithful?
                     (list `(office-field ,(oftm-field-code (or (ox-attr x 'code) (ox-attr x 'xml) ""))
                                          ,t))
                     (if (== t "") '() (list t)))))
              ;; a comment on a range of the text: the text, then the
              ;; comment
              ((commented)
               (let ((t (oftm-inlines (if (pair? l) (cDr l) '())))
                     (c (if (pair? l) (oftm-inline (cAr l)) '())))
                 (cond ((and (pair? c) (func? (car c) 'office-comment 3) (!= t ""))
                        (list `(office-commented ,t ,@(cdar c))))
                       ((== t "") c)
                       (else (cons t c)))))
              ((comment)
               (if (not oftm-faithful?) '()
                   ;; (its paragraphs as one line, after its mark)
                   (with b (oftm-blocks l)
                     (list `(office-comment ,(oftm-text (or (ox-attr x 'author) ""))
                                            ,(oftm-text (or (ox-attr x 'date) ""))
                                            ,(cond ((null? b) "")
                                                   ((null? (cdr b)) (car b))
                                                   (else (oftm-concat
                                                           (list-intersperse b '(next-line))))))))))
              ((ins del)
               (with t (oftm-inlines l)
                 (cond ((== t "") '())
                       (oftm-faithful?
                        (list `(,(if (func? x 'ins) 'office-ins 'office-del)
                                ,(oftm-text (or (ox-attr x 'author) ""))
                                ,(oftm-text (or (ox-attr x 'date) "")) ,t)))
                       ((func? x 'ins) (list t))
                       (else '()))))
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
                 (cond ((and (== t "") (!= name ""))
                        ;; (a reference whose number the file does not have)
                        (if (== (ox-attr x 'kind) "page")
                            (list `(pageref ,name))
                            (list `(reference ,name))))
                       ((== t "") '())
                       ((== name "") (list t))
                       ;; the number of the page of a label
                       ((== (ox-attr x 'kind) "page")
                        (if (and (string? t) (oftm-number? t))
                            (list `(pageref ,name))
                            (list `(hlink ,t ,(string-append "#" name)))))
                       ;; (a text which is not the number of the label,
                       ;; even when it looks like one: the number of its
                       ;; chapter)
                       ((== (ox-attr x 'kind) "text")
                        (list `(hlink ,t ,(string-append "#" name))))
                       ((and (ahash-ref oftm-numbered-labels (ox-attr x 'name))
                             (string? t) (oftm-number? t))
                        (list `(reference ,name)))
                       (else (list `(hlink ,t ,(string-append "#" name)))))))
              ((bookmark)
               (with name (oftm-label-name (ox-attr x 'name))
                 (if name (list `(label ,(oftm-text name))) '())))
              ((note)
               (with b (oftm-blocks l)
                 (cond ((null? b) '())
                       ((null? (cdr b)) (list `(footnote ,(car b))))
                       (else (list `(footnote ,(oftm-document b)))))))
              ((br) '((next-line)))
              ((!htab) '((htab "5mm")))
              ((!page) '((page-the-page)))
              ((tab) '((space "2em")))
              ((pagebreak) '((page-break)))
              ((image) (oftm-image x))
              ((box) (oftm-box x))
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
                ;; (its alignment is the one of the cell; a heading or a
                ;; line of code alone in a cell is still one)
                (with t (oftm-inlines (ox-children (car l)))
                  (list (cond ((== t "") t)
                              ((oftm-role? (car l) "heading") `(strong ,t))
                              ((oftm-role? (car l) "code") `(verbatim ,t))
                              (else t))))
                (oftm-blocks l)))
         (b (if (and (== (ox-attr c 'header) "true"))
                (map oftm-strong b)
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

(define (oftm-styled x t)
  ;; the paragraph t in the named style of the node x, in a document of
  ;; the style office (the usual style is the one of the document)
  (with name (ox-attr x 'style)
    (if (and oftm-faithful? name (!= name oftm-body-style))
        `(office-par ,(oftm-text name) ,t)
        t)))

(define (oftm-aligned x t)
  ;; the paragraph t with the alignment of the node x, when it is not the
  ;; one of the usual paragraphs
  (with a (ox-attr x 'align)
    (if (and (in? a '("center" "right" "left" "justify")) (!= a oftm-align))
        `(with "par-mode" ,a ,t)
        t)))

(define (oftm-item x)
  ;; the paragraphs of an item of a list: the first one starts with the
  ;; item, and with its number when it is not the next one; an item which
  ;; has no number (a header, an item which holds only a list) is its
  ;; paragraphs only
  (let* ((l (oftm-blocks (ox-children x)))
         (start (office-integer (ox-attr x 'start) #f 0 100000))
         (marks (append (if start
                            `((assign "item-nr" ,(number->string (- start 1))))
                            '())
                        '((item)))))
    (cond ((ox-attr x 'unnumbered) l)
          ((null? l) (list `(concat ,@marks "")))
          ;; (a formula which starts an item is in its line; another block,
          ;; a list, a quotation, is after the line of the item)
          ((func? (car l) 'equation* 1)
           (cons (oftm-concat (append marks (list `(math ,(oftm-document-body (cadar l))))))
                 (cdr l)))
          ((and (pair? (car l)) (in? (caar l) oftm-block-tags))
           (cons `(concat ,@marks "") l))
          (else (cons (oftm-concat (append marks (if (func? (car l) 'concat)
                                                     (cdar l)
                                                     (list (car l)))))
                      (cdr l))))))

;; the tags of TeXmacs which are paragraphs, not text
(define oftm-block-tags
  '(equation equation* eqnarray* quotation verbatim-code itemize enumerate
    enumerate-alpha enumerate-Alpha enumerate-roman enumerate-Roman description
    big-figure big-table section subsection subsubsection paragraph subparagraph
    section* subsection* subsubsection* paragraph* subparagraph* theorem
    proposition lemma corollary conjecture axiom definition notation remark note
    example convention warning exercise problem question solution proof
    bibliography table-of-contents document))

(define (oftm-document-body t)
  ;; the line of a document of one line
  (if (and (func? t 'document) (list-1? (cdr t))) (cadr t) t))

(define (oftm-strong t)
  ;; the paragraph t in bold: inside the items of a list, the paragraphs
  ;; of a quotation, not around them; a formula, a table stays as it is
  (cond ((or (== t "") (func? t 'strong)) t)
        ((and (pair? t) (in? (car t) '(document quotation itemize enumerate
                                       enumerate-alpha enumerate-Alpha
                                       enumerate-roman enumerate-Roman description)))
         (cons (car t) (map oftm-strong (cdr t))))
        ((and (func? t 'concat) (pair? (cdr t)) (pair? (cadr t))
              (in? (caadr t) '(item item* assign)))
         (let loop ((l (cdr t)) (marks '()))
           (if (and (pair? l) (pair? (car l)) (in? (caar l) '(item item* assign)))
               (loop (cdr l) (cons (car l) marks))
               (oftm-concat (append (reverse marks)
                                    (if (null? l) '()
                                        (list (oftm-strong (oftm-concat l)))))))))
        ((and (func? t 'with) (>= (length t) 4) (even? (length t)))
         (rcons (cDr t) (oftm-strong (cAr t))))
        ((func? t 'office-par 2) `(office-par ,(cadr t) ,(oftm-strong (caddr t))))
        ((func? t 'surround 3) `(surround ,(cadr t) ,(caddr t) ,(oftm-strong (cadddr t))))
        ((and (pair? t) (in? (car t) oftm-block-tags)) t)
        ((and (pair? t) (in? (car t) '(tabular block eqnarray*))) t)
        (else `(strong ,t))))

(define (oftm-list x)
  ;; a list; a numbered one in its way of numbering, from its first number
  (let* ((number? (== (ox-attr x 'kind) "number"))
         (tag (if (not number?) 'itemize
                  (with f (ox-attr x 'format)
                    (cond ((== f "a") 'enumerate-alpha)
                          ((== f "A") 'enumerate-Alpha)
                          ((== f "i") 'enumerate-roman)
                          ((== f "I") 'enumerate-Roman)
                          (else 'enumerate)))))
         (start (and number? (office-integer (ox-attr x 'start) 1 0 100000)))
         (items (list-filter (ox-children x) (lambda (y) (func? y 'item))))
         (l (append-map oftm-item items)))
    (if (null? items) '()
        (list `(,tag ,(oftm-document
                        (if (and start (!= start 1) (pair? l))
                            ;; (the number before the first item)
                            (cons (oftm-concat
                                    (cons `(assign "item-nr"
                                                   ,(number->string (- start 1)))
                                          (if (func? (car l) 'concat) (cdar l)
                                              (list (car l)))))
                                  (cdr l))
                            l)))))))

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

(define (oftm-leading-images x)
  ;; (images . caption) for a paragraph which starts with images and goes
  ;; on with text, or #f
  (let loop ((l (ox-children x)) (acc '()))
    (cond ((and (pair? l) (func? (car l) 'image)) (loop (cdr l) (cons (car l) acc)))
          ((and (pair? l) (string? (car l)) (== (string-trim-spaces (car l)) "")
                (pair? acc))
           (loop (cdr l) acc))
          ((null? acc) #f)
          (else (cons (reverse acc)
                      (apply office-node (cons* 'p (map (lambda (a) (list (car a) (cadr a)))
                                                       (ox-attrs x))
                                                l)))))))

(define (oftm-only-formula x)
  ;; the formula of a paragraph which holds nothing else (but bookmarks,
  ;; the tabs before its label, the punctuation after a displayed one), or
  ;; #f. (An inline formula with punctuation is in its line of text, and
  ;; the punctuation before a formula is text.)
  (with l (list-filter (ox-children x)
                       (lambda (y) (not (or (oftm-blank? y) (func? y 'bookmark)))))
    (and (pair? l) (func? (car l) 'math)
         (or (null? (cdr l))
             ;; (a formula and its notes, as OpenDocument writes it)
             (list-and (map (lambda (y) (func? y 'note)) (cdr l)))
             (and (oftm-display? (car l))
                  (list-and (map (lambda (y) (or (oftm-punctuation-string? y)
                                                 (func? y 'note)))
                                 (cdr l)))))
         (car l))))

(define (oftm-without-role b)
  ;; the block b, a paragraph of a field, not an entry of a bibliography;
  ;; an empty one stays (keep-empty)
  (if (not (and (func? b 'p) (or (oftm-role? b "bibitem") (null? (ox-children b))))) b
      (apply office-node
             (cons* 'p (append (map (lambda (a) (list (car a) (cadr a)))
                                    (list-filter (ox-attrs b) (lambda (a) (!= (car a) 'role))))
                               (if (null? (ox-children b)) '((keep-empty "true")) '()))
                    (ox-children b)))))

(define (oftm-formula-dots x f)
  ;; the formula f of the paragraph x, with the punctuation just after it
  ;; (not the separators of its number), and its notes after it, as text
  (let* ((after (let loop ((c (ox-children x)))
                  (cond ((null? c) '())
                        ((func? (car c) 'math) (cdr c))
                        (else (loop (cdr c))))))
         (notes (let loop ((c after))
                  (cond ((null? c) '())
                        ((func? (car c) 'note)
                         (append (oftm-inline (car c)) (loop (cdr c))))
                        ((or (func? (car c) 'bookmark) (oftm-blank? (car c))
                             (oftm-punctuation-string? (car c)))
                         (loop (cdr c)))
                        (else '()))))
         (dots (let loop ((c after) (acc ""))
                 (cond ((null? c) acc)
                       ((or (func? (car c) 'bookmark) (func? (car c) 'note)
                            (and (string? (car c)) (== (string-trim-spaces (car c)) "")))
                        (loop (cdr c) acc))
                       ((oftm-punctuation-string? (car c))
                        (loop (cdr c) (string-append acc (string-trim-spaces (car c)))))
                       (else acc))))
         ;; (and the punctuation after its number: "(1).")
         (dots (let loop ((c after) (seq? #f) (closed? #f) (acc dots))
                 ;; (closed?: after the closing bracket of the number)
                 (cond ((null? c) acc)
                       ((func? (car c) 'seq) (loop (cdr c) #t #f acc))
                       ((not (and seq? (string? (car c)))) (loop (cdr c) seq? closed? acc))
                       ((oftm-bracket-punctuation (car c))
                        => (lambda (d) (loop (cdr c) seq? #t (string-append acc d))))
                       ((and closed? (oftm-punctuation-string? (car c)))
                        (loop (cdr c) seq? closed? (string-append acc (string-trim-spaces (car c)))))
                       (else (loop (cdr c) seq?
                                   (or closed? (list-or (map (lambda (ch) (in? ch '(#\) #\])))
                                                             (string->list (car c)))))
                                   acc))))))
    (if (and (== dots "") (null? notes)) f
        (oftm-concat (append (if (func? f 'concat) (cdr f) (list f))
                             (if (== dots "") '() (list (oftm-text dots)))
                             (if (null? notes) '() (list `(text ,(oftm-concat notes)))))))))

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
  ;; the text of a caption, without "Figure 1." at its start (nor the break
  ;; of line after the images of a frame of LibreOffice)
  (let* ((l (let drop ((l (ox-children x)))
              (if (and (pair? l) (or (func? (car l) 'br)
                                     (and (string? (car l))
                                          (== (string-trim-spaces (car l)) ""))))
                  (drop (cdr l))
                  l)))
         (start (oftm-named-start l)))
    (if (and start (cadddr start))
        (oftm-concat (append (oftm-labels (cadr start))
                             (append-map oftm-inline (caddr start))))
        (oftm-inlines l))))

(define (oftm-blocks l)
  ;; the paragraphs of TeXmacs for a list of blocks. The blocks which
  ;; belong together are taken together: the lines of a quotation or of a
  ;; piece of code, a figure or a table and its caption. (A loop: a
  ;; document may have many thousands of blocks)
  (let loop ((l (append-map oftm-split-display (append-map oftm-split-floats l)))
             (acc '()))
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
        ;; a caption which holds its images, as LibreOffice writes it in
        ;; a frame
        ((and (oftm-role? (car l) "caption") (oftm-leading-images (car l)))
         => (lambda (r)
              (cons (list `(big-figure ,(oftm-inlines (car r))
                                       ,(oftm-caption (cdr r))))
                    (cdr l))))
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
                             (with f (oftm-formula-dots x (oftm-formula (car r)))
                               (if (== f "") '()
                                   (list `(equation
                                            (document
                                              ,(oftm-concat
                                                 (append (if (func? f 'concat) (cdr f)
                                                             (list f))
                                                         (oftm-labels (cadr r)))))))))))
                       ;; a formula alone in its paragraph is displayed (but
                       ;; in a cell, where Word displays a formula alone)
                       ((and (not oftm-in-cell?) (oftm-only-formula x))
                        => (lambda (m)
                             (with t (oftm-eqarray-table m)
                               (if t (list (oftm-eqarray t))
                                   (let* ((f (oftm-formula-dots x (oftm-formula m)))
                                          (labels (oftm-labels (oftm-bookmarks x))))
                                     (cond ((and (== f "") (null? labels)) '())
                                           ((null? labels)
                                            (list `(equation* (document ,f))))
                                           (else
                                             (list `(equation*
                                                      (document
                                                        ,(oftm-concat
                                                           (append (if (func? f 'concat)
                                                                       (cdr f) (list f))
                                                                   labels))))))))))))
                       (else
                         (with t (oftm-inlines (ox-children x))
                           (if (== t "")
                               (if (ox-attr x 'keep-empty) (list (oftm-styled x "")) '())
                               (list (oftm-styled x (oftm-direct-paragraph
                                                      x (oftm-aligned x t)))))))))
                ((list) (oftm-list x))
                ((table) (list (oftm-table x)))
                ((pagebreak) '((page-break)))
                ;; a field over several paragraphs: its result in the field
                ;; (its paragraphs as they are: those of a bibliography
                ;; are not entries of TeXmacs, which another program
                ;; writes again)
                ((field-blocks)
                 (with b (oftm-blocks (map oftm-without-role (ox-children x)))
                   (if oftm-faithful?
                       (list `(office-field ,(oftm-text (or (ox-attr x 'code) ""))
                                            ,(oftm-document b)))
                       b)))
                ;; a section in columns
                ((columns)
                 (let ((n (or (ox-attr x 'count) "2"))
                       (b (oftm-document (oftm-blocks (ox-children x))))
                       (line? (== (ox-attr x 'line) "true"))
                       (gap (with c (office-length->cm (ox-attr x 'gap))
                              (and c (office-cm c)))))
                   (list (cond ((not oftm-faithful?) `(with "par-columns" ,n ,b))
                               ((or line? gap)
                                `(office-columns* ,n ,(if line? "true" "false")
                                                  ,(or gap "") ,b))
                               (else `(office-columns ,n ,b))))))
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

;; the macros of the title (doc-title, doc-subtitle, doc-date) with the
;; node style of their paragraphs in the file
(define oftm-title-styles '())

(define (oftm-title-part-macro name st)
  ;; the part name of the title in the look of the style of its paragraphs
  ;; (and not in the one of TeXmacs): the font, the size and the color, the
  ;; spaces (0.5fn when the style has none) and the alignment (the one of
  ;; the usual paragraphs when the style has none)
  (let* ((align (ox-attr st 'align))
         (align (if (in? align '("left" "center" "right" "justify")) align oftm-align))
         (body (oftm-paragraph-format st (oftm-style-text st '(arg "x")) #f))
         (body (if (or (office-number (ox-attr st 'space-before))
                       (office-number (ox-attr st 'space-after)))
                   body
                   `(surround (vspace* "0.5fn") (vspace "0.5fn") ,body))))
    `(associate ,name (macro "x" (with "par-mode" ,align ,body)))))

(define (oftm-doc-title-macros)
  ;; the macros of the parts of the title, for the initial environment
  (map (lambda (p) (oftm-title-part-macro (car p) (cdr p))) oftm-title-styles))

(define (oftm-title-block? x)
  (and (func? x 'p) (in? (ox-attr x 'role) oftm-title-roles)))

(define (oftm-lines l)
  ;; several paragraphs as the lines of one
  (oftm-concat
    (let loop ((l l) (acc '()) (first? #t))
      (if (null? l) (reverse acc)
          (loop (cdr l)
                (append (reverse (if (func? (car l) 'concat) (cdar l)
                                     (list (car l))))
                        (if first? acc (cons '(next-line) acc)))
                #f)))))

(define (oftm-doc-data l meta)
  ;; the title, from the paragraphs l at the start of the document which
  ;; have the styles of a title, or else from the properties of the file
  (let* ((get (lambda (role)
                (map (lambda (x) (oftm-inlines (ox-children x)))
                     (list-filter l (lambda (x) (oftm-role? x role))))))
         ;; (not the title of the properties of the file: it is often the
         ;; one of a template, which the document does not show)
         (titles (get "title"))
         (subtitles (get "subtitle"))
         (authors (get "author"))
         (dates (get "date"))
         (abstract (get "abstract")))
    (set! oftm-title-styles
          (list-filter
            (map (lambda (p)
                   (let* ((x (list-find l (lambda (x) (oftm-role? x (car p)))))
                          (name (and x (ox-attr x 'style))))
                     (with st (and name (ahash-ref oftm-styles (cons "paragraph" name)))
                       (and st (cons (cdr p) st)))))
                 '(("title" . "doc-title") ("subtitle" . "doc-subtitle")
                   ("date" . "doc-date")))
            identity))
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

(define (oftm-header-node x)
  ;; the nodes of a header: its tabs spread the text, its number of the
  ;; page is the one of the page
  (cond ((func? x 'tab) '(!htab))
        ((and (func? x 'field)
              (in? (locase-all (string-trim-spaces (or (ox-attr x 'code) "")))
                   '("page" "page \\* mergeformat" "page \\* arabic")))
         '(!page))
        ((and (func? x 'field) (== (ox-attr x 'odt) "text:page-number")) '(!page))
        ((pair? x) (cons (car x) (map oftm-header-node (cdr x))))
        (else x)))

(define (oftm-header x)
  ;; the content of a header or a footer, from its paragraphs
  (let* ((ps (list-filter (ox-children x) (lambda (y) (func? y 'p))))
         (line (lambda (p)
                 (let ((t (oftm-inlines (ox-children (oftm-header-node p))))
                       (a (ox-attr p 'align)))
                   (cond ((== t "") #f)
                         ((== a "center") `(concat (htab "5mm") ,t (htab "5mm")))
                         ((== a "right") `(concat (htab "5mm") ,t))
                         (else t)))))
         (l (list-filter (map line ps) identity)))
    (cond ((null? l) #f)
          ((null? (cdr l)) (car l))
          (else (oftm-concat (list-intersperse l " "))))))

(define (oftm-headers l)
  ;; (initial . start): the headers and footers of the document, in its
  ;; initial environment, and the ones of the first page, at its start
  (let ((init '()) (start '()))
    (for (x (list-filter l (lambda (y) (func? y 'header))))
      (let ((place (or (ox-attr x 'place) "header"))
            (kind (or (ox-attr x 'kind) "default"))
            (t (oftm-header x)))
        (when t
          (cond ((== kind "first")
                 (set! start (cons `(,(if (== place "header") 'set-this-page-header
                                         'set-this-page-footer) ,t)
                                   start)))
                ((== kind "even")
                 (set! init (cons (list (string-append "page-even-" place) t) init)))
                (else
                  (set! init (cons (list (string-append "page-odd-" place) t) init))
                  (when (not (list-find l (lambda (y) (and (func? y 'header)
                                                           (== (ox-attr y 'kind) "even")
                                                           (== (ox-attr y 'place) place)))))
                    (set! init (cons (list (string-append "page-even-" place) t) init))))))))
    (cons (reverse init) (reverse start))))

(define oftm-page-types
  ;; the sizes of the usual pages, in centimeters
  '(("a3" 29.7 42.0) ("a4" 21.0 29.7) ("a5" 14.8 21.0) ("b5" 17.6 25.0)
    ("letter" 21.59 27.94) ("legal" 21.59 35.56) ("executive" 18.42 26.67)))

(define (oftm-page-type w h)
  ;; the name of the page of this size, or #f
  (with r (list-find oftm-page-types
                     (lambda (t)
                       (or (and (< (abs (- w (cadr t))) 0.2) (< (abs (- h (caddr t))) 0.2))
                           (and (< (abs (- h (cadr t))) 0.2) (< (abs (- w (caddr t))) 0.2)))))
    (and r (car r))))

(define (oftm-setup x)
  ;; the environment of the document for its block setup
  (let* ((get (lambda (key) (ox-attr x key)))
         (num (lambda (key) (office-number (get key))))
         (cm (lambda (key) (office-length->cm (get key))))
         ;; (a margin may be 0)
         (cm0 (lambda (key) (office-length->cm (get key) #t)))
         (w (cm 'page-width))
         (h (cm 'page-height))
         (type (and w h (oftm-page-type w h)))
         (pt (lambda (v) (string-append (office-decimal v) "pt")))
         (size (num 'font-size))
         (line (num 'line-spacing))
         (sep (+ (or (num 'space-before) 0) (or (num 'space-after) 0))))
    (append
      (cond ((not (and w h)) '())
            (type `(("page-type" ,type)
                    ("page-orientation" ,(if (> w h) "landscape" "portrait"))))
            (else `(("page-type" "user") ("page-width" ,(get 'page-width))
                    ("page-height" ,(get 'page-height)))))
      (if (cm0 'margin-left)
          `(("page-odd" ,(get 'margin-left)) ("page-even" ,(get 'margin-left)))
          '())
      (if (cm0 'margin-right) `(("page-right" ,(get 'margin-right))) '())
      (if (cm0 'margin-top) `(("page-top" ,(get 'margin-top))) '())
      (if (cm0 'margin-bottom) `(("page-bot" ,(get 'margin-bottom))) '())
      (if (and size (>= size 5) (<= size 50))
          `(("font-base-size" ,(number->string (inexact->exact (round size)))))
          '())
      (cond ((== (get 'font) "sans") '(("font-family" "ss")))
            ((== (get 'font) "mono") '(("font-family" "tt")))
            (else '()))
      ;; the paragraphs of the office programs have the spaces which their
      ;; style says, and no others
      (if (get 'align)
          `(("par-first" ,(pt (max 0 (or (num 'first-indent) 0))))
            ("par-par-sep" ,(pt (max 0 sep))))
          '())
      ;; (a line of TeXmacs is 1.2 times the size of its font)
      (if (and line (> line 0) (< line 5))
          `(("par-sep" ,(string-append (office-decimal (max 0 (- (* 1.2 line) 1)))
                                       "fn")))
          '())
      (if (in? (get 'align) '("left" "center" "right"))
          `(("par-mode" ,(get 'align)))
          '()))))

;; The headers, the footers and the macros of the named styles as they were
;; read, beside the file in the initial environment (office-original): the
;; writers compare them with those of the document, and write those which
;; were changed instead of those of the file.
(define (oftm-original l macros)
  ;; (only the headers which the file has: the one of the even pages is the
  ;; one of the odd pages when the file has none)
  (let ((pages (list-filter
                 (map (lambda (x)
                        (let ((place (or (ox-attr x 'place) "header"))
                              (kind (or (ox-attr x 'kind) "default"))
                              (t (oftm-header x)))
                          (and t `(tuple ,(cond ((== kind "first")
                                                 (string-append "first-" place))
                                                ((== kind "even")
                                                 (string-append "page-even-" place))
                                                (else (string-append "page-odd-" place)))
                                         ,t))))
                      (list-filter l (lambda (y) (func? y 'header))))
                 identity))
        (styles (list-filter
                  (map (lambda (m)
                         (and (func? m 'associate 2) (string? (cadr m))
                              (or (string-starts? (cadr m) "office-par-")
                                  (string-starts? (cadr m) "office-span-"))
                              `(tuple ,(cadr m) ,(caddr m))))
                       macros)
                  identity)))
    `(("office-original" (tuple ,@pages ,@styles)))))

(define (oftm-body l)
  (let* ((meta (append-map cdr (list-filter l (lambda (x) (func? x 'meta)))))
         (blocks (list-filter l (lambda (x) (not (in? (car x) '(meta setup styles
                                                                    header package))))))
         (title (let loop ((r blocks) (acc '()))
                  (if (and (pair? r) (oftm-title-block? (car r)))
                      (loop (cdr r) (cons (car r) acc))
                      (reverse acc))))
         (rest (list-tail blocks (length title))))
    (oftm-document (append (oftm-doc-data title meta) (oftm-blocks rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (office->texmacs x . opts)
  (:type (-> stree stree))
  (:synopsis "Convert the office tree @x into a TeXmacs document")
  ;; with the option office->texmacs:faithful (the converter of the files
  ;; sets it), a document of the style office
  (with l (if (func? x 'office) (cdr x) (list x))
    (set! oftm-numbered-labels (make-ahash-table))
    (set! oftm-moved-labels (make-ahash-table))
    (set! oftm-faithful?
          (== (assoc-ref (if (and (nnull? opts) (list? (car opts))) (car opts) '())
                         "office->texmacs:faithful")
              "on"))
    (set! oftm-styles (make-ahash-table))
    (set! oftm-title-styles '())
    (for (st (append-map ox-children (list-filter l (lambda (y) (func? y 'styles)))))
      ;; (a style of paragraphs and one of text may have the same name)
      (when (and (func? st 'style) (ox-attr st 'name))
        (ahash-set! oftm-styles
                    (cons (if (== (ox-attr st 'family) "paragraph") "paragraph" "text")
                          (ox-attr st 'name))
                    st)))
    (with r (catch #t
              (lambda ()
                (let* ((setup (list-find l (lambda (y) (func? y 'setup))))
                       (package (list-find l (lambda (y) (func? y 'package))))
                       (headers (oftm-headers l))
                       (env (if setup (oftm-setup setup) '()))
                       (env (append env (car headers)))
                       (env (if (and oftm-faithful? package (ox-attr package 'data))
                                (append env
                                        `(("office-package"
                                           (tuple ,(or (ox-attr package 'format) "")
                                                  (raw-data ,(ox-attr package 'data))))))
                                env))
                       (env (if (and oftm-faithful? setup (ox-attr setup 'body-style))
                                (append env `(("office-body-style"
                                               ,(ox-attr setup 'body-style))))
                                env)))
                  (set! oftm-align (or (and setup (ox-attr setup 'align)) "justify"))
                  (set! oftm-body-style (and setup (ox-attr setup 'body-style)))
                  (set! oftm-base-size
                        (or (and setup (office-number (ox-attr setup 'font-size))) 10.0))
                  (set! oftm-base-font (and setup (ox-attr setup 'font)))
                  (for-each oftm-scan-labels l)
                  ;; (the pictures of Windows, converted all at once)
                  (with pics (oftm-metafiles l)
                    (when (pair? pics) (office-metafiles->png pics)))
                  (let* ((body (oftm-body l))
                         (body (if (null? (cdr headers)) body
                                   (oftm-document (append (cdr headers) (cdr body)))))
                         (macros (if oftm-faithful?
                                     (append (oftm-style-macros) (oftm-doc-title-macros))
                                     '()))
                         (env (if (and oftm-faithful? package (ox-attr package 'data))
                                  (append env (oftm-original l macros))
                                  env)))
                    `(document (body ,body)
                               (style ,(if (and oftm-faithful?
                                                (not (list-find l (lambda (y) (func? y 'error)))))
                                           "office" "generic"))
                               ,@(if (and (null? env) (null? macros)) '()
                                     `((initial (collection
                                                  ,@(map (lambda (a) `(associate ,@a))
                                                         env)
                                                  ,@macros))))))))
              ;; (what was not foreseen gives a document which says so)
              (lambda args
                (with msg (string-append "This document could not be converted "
                                         (office-error-text args))
                  (debug-message "convert-error" (string-append msg "\n"))
                  `(document (body (document (with "color" "red" ,msg)))
                             (style "generic")))))
      (set! oftm-numbered-labels (make-ahash-table))
      (set! oftm-moved-labels (make-ahash-table))
      (set! oftm-align "justify")
      (set! oftm-faithful? #f)
      (set! oftm-styles (make-ahash-table))
      (set! oftm-title-styles '())
      (set! oftm-body-style #f)
      r)))
