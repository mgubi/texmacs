
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : office-tools.scm
;; DESCRIPTION : tools for the converters of the office formats
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A document of Word (.docx) or of OpenDocument (.odt) is a zip archive of
;; XML files and images. The converters read both into the same tree, the
;; office tree, whose strings are in UTF-8:
;;
;;   (office block...)
;;   blocks:  (meta (key "value")...)
;;            (p inline...)  with the attributes role (title, subtitle,
;;              author, date, abstract, heading, quote, code, caption,
;;              figure, term, definition, theorem, remark, proof, bibitem,
;;              toc), level (of a heading), number (of a heading which
;;              has one), labels (the bookmarks of a heading) and align
;;            (list (item block...)...)  with the attribute kind (bullet,
;;              number)
;;            (table (row (cell block...)...)...)  with the attributes
;;              align, width (a part of the paragraph: 0.5par) and columns
;;              (the parts of the columns in the table: "0.25 0.75"); the
;;              cells with the attributes header, colspan, rowspan,
;;              covered (by a wider or a higher cell), borders (the sides
;;              t, b, l, r which have one, or none) and background
;;            (toc inline...)  the table of contents, with its title
;;            (pagebreak)
;;            (error "message")  for a file which could not be read
;;   inlines: "text"  (em ...)  (strong ...)  (underline ...)  (strike ...)
;;            (sub ...)  (sup ...)  (code ...)  (smallcaps ...)  (mark ...)
;;            (color ...)  with the attribute value, #rrggbb
;;            (link ...)  with the attribute href (#name inside the document)
;;            (note block...)  (br)  (tab)
;;            (image)  with the attributes name, data (the bytes of the
;;              file), width, height (lengths of TeXmacs), alt, and svg
;;              (the text of an SVG of the same picture, when there is one)
;;            (math "MathML")  with the attribute display
;;            (bookmark)  (ref ...)  with the attribute name; a ref also
;;              has kind (seq, heading) and target when its bookmark is
;;              the one of a number, or the kind page for its page
;;            (seq "1")  a number of a sequence, with the attributes name
;;              (Figure; empty for a number which stands for itself), id
;;              and labels (the names of the bookmarks of this number)
;;
;; The attributes are those of sxml, (tag (@ (name "value")...) ...).

(texmacs-module (convert office office-tools))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees of XML, as parse-xml gives them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ox-attrs x)
  (:synopsis "The attributes of the element @x, as an association list")
  (if (and (pair? x) (pair? (cdr x)) (func? (cadr x) '@)) (cdadr x) '()))

(tm-define (ox-attr x name)
  (:synopsis "The attribute @name of the element @x, or #f")
  (with a (assoc name (ox-attrs x))
    (and a (pair? (cdr a)) (cadr a))))

(tm-define (ox-children x)
  (:synopsis "The children of the element @x, without its attributes")
  (cond ((not (pair? x)) '())
        ((and (pair? (cdr x)) (func? (cadr x) '@)) (cddr x))
        (else (cdr x))))

(tm-define (ox-elements x)
  (:synopsis "The children of the element @x which are elements")
  (list-filter (ox-children x) pair?))

(tm-define (ox-child x tag)
  (:synopsis "The first child @tag of the element @x, or #f")
  (list-find (ox-children x) (lambda (y) (func? y tag))))

(tm-define (ox-childs x tag)
  (:synopsis "The children @tag of the element @x")
  (list-filter (ox-children x) (lambda (y) (func? y tag))))

(tm-define (ox-path x . tags)
  (:synopsis "The element reached from @x by the first children @tags, or #f")
  (cond ((not x) #f)
        ((null? tags) x)
        (else (apply ox-path (cons (ox-child x (car tags)) (cdr tags))))))

(tm-define (ox-find x tag)
  (:synopsis "The first element @tag inside @x, at any depth, or #f")
  (cond ((not (pair? x)) #f)
        ((func? x tag) x)
        (else (let loop ((l (ox-children x)))
                (cond ((null? l) #f)
                      ((ox-find (car l) tag) => identity)
                      (else (loop (cdr l))))))))

(tm-define (ox-text x)
  (:synopsis "The text inside @x")
  (cond ((string? x) x)
        ((pair? x) (apply string-append (map ox-text (ox-children x))))
        (else "")))

(tm-define (ox-root x)
  (:synopsis "The element of the document @x of parse-xml")
  (if (func? x '*TOP*)
      (list-find (cdr x) (lambda (y) (and (pair? y) (!= (car y) '*PI*)
                                          (!= (car y) '*DOCTYPE*))))
      x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (office-archive s)
  (:synopsis "The table of the entries of the zip archive @s, or #f")
  (and (string? s) (zip-archive? s)
       (let ((t (make-ahash-table)))
         (let loop ((l (zip-unpack s)))
           (when (and (pair? l) (pair? (cdr l)))
             (ahash-set! t (car l) (cadr l))
             (loop (cddr l))))
         t)))

(tm-define (office-entry archive name)
  (:synopsis "The entry @name of the @archive, or #f")
  (ahash-ref archive name))

;; The converters know the elements by their usual prefixes (w:p, text:p).
;; A file may give other prefixes to the same namespaces, or none: its
;; elements and attributes then get the usual ones.
(define office-namespaces
  '(("http://schemas.openxmlformats.org/wordprocessingml/2006/main" . "w")
    ("http://purl.oclc.org/ooxml/wordprocessingml/main" . "w")
    ("http://schemas.openxmlformats.org/officeDocument/2006/relationships" . "r")
    ("http://purl.oclc.org/ooxml/officeDocument/relationships" . "r")
    ("http://schemas.openxmlformats.org/officeDocument/2006/math" . "m")
    ("http://purl.oclc.org/ooxml/officeDocument/math" . "m")
    ("http://schemas.openxmlformats.org/package/2006/relationships" . "")
    ("http://schemas.openxmlformats.org/drawingml/2006/main" . "a")
    ("http://purl.oclc.org/ooxml/drawingml/main" . "a")
    ("http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" . "wp")
    ("http://purl.oclc.org/ooxml/drawingml/wordprocessingDrawing" . "wp")
    ("http://schemas.openxmlformats.org/markup-compatibility/2006" . "mc")
    ("urn:schemas-microsoft-com:vml" . "v")
    ("urn:schemas-microsoft-com:office:office" . "o")
    ("http://purl.org/dc/elements/1.1/" . "dc")
    ("urn:oasis:names:tc:opendocument:xmlns:office:1.0" . "office")
    ("urn:oasis:names:tc:opendocument:xmlns:style:1.0" . "style")
    ("urn:oasis:names:tc:opendocument:xmlns:text:1.0" . "text")
    ("urn:oasis:names:tc:opendocument:xmlns:table:1.0" . "table")
    ("urn:oasis:names:tc:opendocument:xmlns:drawing:1.0" . "draw")
    ("urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0" . "fo")
    ("urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0" . "svg")
    ("urn:oasis:names:tc:opendocument:xmlns:meta:1.0" . "meta")
    ("http://www.w3.org/1999/xlink" . "xlink")))

(define (office-declarations x)
  ;; the namespaces which the element x declares, as (prefix . usual prefix);
  ;; a namespace which is not known keeps its prefix
  (append-map
    (lambda (a)
      (let* ((s (symbol->string (car a)))
             (p (cond ((== s "xmlns") "")
                      ((string-starts? s "xmlns:")
                       (substring s 6 (string-length s)))
                      (else #f)))
             (u (and p (pair? (cdr a)) (string? (cadr a))
                     (assoc-ref office-namespaces (cadr a)))))
        (if p (list (cons p (or u p))) '())))
    (ox-attrs x)))

;; whether the name of an attribute is the one of a declaration: 1 or 0,
;; so that each name is looked at once
(define office-declaration-names (make-ahash-table))

(define (office-declaration? name)
  (with r (ahash-ref office-declaration-names name)
    (when (not r)
      (set! r (if (string-starts? (symbol->string name) "xmlns") 1 0))
      (ahash-set! office-declaration-names name r))
    (== r 1)))

(define (office-other-prefixes? x)
  ;; whether an element of the tree x gives to a namespace which is known
  ;; another prefix than the usual one
  (and (pair? x) (symbol? (car x))
       (let loop ((l (cdr x)))
         (cond ((null? l) #f)
               ((not (pair? (car l))) (loop (cdr l)))
               ((eq? (caar l) '@)
                (or (and (list-or (map (lambda (a) (office-declaration? (car a)))
                                       (cdar l)))
                         (list-or (map (lambda (d) (!= (car d) (cdr d)))
                                       (office-declarations x))))
                    (loop (cdr l))))
               ((office-other-prefixes? (car l)) #t)
               (else (loop (cdr l)))))))

(define (office-usual-name name env attribute?)
  ;; the symbol name with the usual prefix of its namespace
  (let* ((s (symbol->string name))
         (i (string-search-forwards ":" 0 s))
         (p (if (>= i 0) (substring s 0 i) ""))
         (q (assoc-ref env p)))
    (cond ((or (not q) (== q p)) name)
          ;; an attribute without a prefix is in no namespace
          ((and attribute? (== p "")) name)
          ((string-starts? s "xmlns") name)
          (else
            (with local (if (>= i 0) (substring s (+ i 1) (string-length s)) s)
              (string->symbol (if (== q "") local
                                  (string-append q ":" local))))))))

(define (office-usual-prefixes x env)
  ;; the element x with the usual prefixes, where env is the list of the
  ;; declarations around it
  (cond ((not (pair? x)) x)
        ((not (symbol? (car x))) x)
        ((string-starts? (symbol->string (car x)) "*") x)
        (else
          (let* ((env (append (office-declarations x) env))
                 (attrs (map (lambda (a)
                               (cons (office-usual-name (car a) env #t) (cdr a)))
                             (ox-attrs x)))
                 (children (map (lambda (y) (office-usual-prefixes y env))
                                (ox-children x)))
                 (tag (office-usual-name (car x) env #f)))
            (if (null? attrs) (cons tag children)
                (cons* tag (cons '@ attrs) children))))))

(tm-define (office-xml archive name)
  (:synopsis "The element of the XML entry @name of the @archive, or #f")
  (let* ((s (ahash-ref archive name))
         (x (and s (ox-root (parse-xml s)))))
    ;; (the files of the usual programs are left as they are)
    (set! office-declaration-names (make-ahash-table))
    (if (office-other-prefixes? x) (office-usual-prefixes x '()) x)))

(tm-define (office-number s)
  (:synopsis "The number of the attribute @s, or #f")
  ;; a plain number only: a sign, digits and a decimal point, so that no
  ;; other number which Scheme reads (a fraction, an infinity) comes in
  (let* ((n (string-length (if (string? s) s "")))
         (start (if (and (> n 0) (in? (string-ref s 0) '(#\+ #\-))) 1 0)))
    (and (> n start) (<= n 15)
         (char-numeric? (string-ref s start))
         (let loop ((i start) (dot? #f))
           (cond ((>= i n) #t)
                 ((char-numeric? (string-ref s i)) (loop (+ i 1) dot?))
                 ((and (char=? (string-ref s i) #\.) (not dot?))
                  (loop (+ i 1) #t))
                 (else #f)))
         (string->number s))))

(tm-define (office-integer s default lo hi)
  (:synopsis "The integer of the attribute @s between @lo and @hi, or @default")
  ;; an exact integer, whatever the file says: a number with decimals is
  ;; rounded, and what is not a plain number gives the default
  (with v (office-number s)
    (cond ((not v) default)
          ((>= v hi) hi)
          ((<= v lo) lo)
          (else (inexact->exact (round v))))))

(tm-define (office-resolve base target)
  (:synopsis "The name in the archive of @target, relative to the entry @base")
  ;; word/document.xml and media/a.png give word/media/a.png
  (if (string-starts? target "/")
      (substring target 1 (string-length target))
      (let* ((dir (reverse (cdr (reverse (string-tokenize-by-char base #\/)))))
             (parts (string-tokenize-by-char target #\/)))
        (let loop ((dir (reverse dir)) (parts parts))
          (cond ((null? parts) (string-recompose (reverse dir) "/"))
                ((== (car parts) ".") (loop dir (cdr parts)))
                ((== (car parts) "..")
                 (loop (if (null? dir) dir (cdr dir)) (cdr parts)))
                (else (loop (cons (car parts) dir) (cdr parts))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The office tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (office-node tag attrs . children)
  (:synopsis "The node @tag with the attributes @attrs which have a value")
  ;; attrs is a list of (name value), a value of #f or "" is left out
  (with l (list-filter attrs (lambda (a) (and (cadr a) (!= (cadr a) ""))))
    (if (null? l) (cons tag children)
        (cons* tag (cons '@ l) children))))

(define office-wrappers
  '(em strong underline strike sub sup code smallcaps mark color))

(define (office-same-wrapper? x y)
  ;; two nodes which wrap text in the same way
  (and (pair? x) (pair? y) (== (car x) (car y)) (in? (car x) office-wrappers)
       (== (ox-attrs x) (ox-attrs y))))

(define (office-wrapper-node x l)
  ;; the wrapper of the node x around the nodes l
  (if (null? (ox-attrs x)) (cons (car x) l)
      (cons* (car x) (cons '@ (ox-attrs x)) l)))

(tm-define (office-join-strings l)
  (:synopsis "The list @l with the strings which follow each other as one")
  ;; (loops, and the strings of a run are joined at once: a text may have
  ;; thousands of runs)
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((string? (car l))
           (let run ((m (cdr l)) (strs (list (car l))))
             (if (and (pair? m) (string? (car m)))
                 (run (cdr m) (cons (car m) strs))
                 (with s (apply string-append (reverse strs))
                   (loop m (if (== s "") acc (cons s acc)))))))
          (else (loop (cdr l) (cons (car l) acc))))))

(tm-define (office-merge l)
  (:synopsis "The inline nodes @l, with the neighbours of the same kind as one")
  ;; the runs of a text come one by one, each with all its properties
  (let loop ((l (office-join-strings l)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((and (pair? (cdr l)) (office-same-wrapper? (car l) (cadr l)))
           ;; a run of the same wrapper is one, around the texts of all
           (let run ((m (cddr l))
                     (kids (append (reverse (ox-children (cadr l)))
                                   (reverse (ox-children (car l))))))
             (if (and (pair? m) (office-same-wrapper? (car l) (car m)))
                 (run (cdr m) (append (reverse (ox-children (car m))) kids))
                 (loop m (cons (office-wrapper-node
                                 (car l) (office-merge (reverse kids)))
                               acc)))))
          ((and (pair? (car l)) (in? (caar l) office-wrappers))
           (loop (cdr l)
                 (cons (office-wrapper-node (car l)
                                            (office-merge (ox-children (car l))))
                       acc)))
          (else (loop (cdr l) (cons (car l) acc))))))

(tm-define (office-wrap l props)
  (:synopsis "The inline nodes @l inside the wrappers of the list @props")
  ;; a wrapper is a tag, or (color "#rrggbb")
  (cond ((or (null? props) (null? l)) l)
        ((pair? (car props))
         (office-wrap (list `(,(caar props) (@ (value ,(cadar props))) ,@l))
                      (cdr props)))
        (else (office-wrap (list (cons (car props) l)) (cdr props)))))

(tm-define (office-color s)
  (:synopsis "The color @s of a text as #rrggbb, or #f for none or black")
  (let* ((s (and (string? s) (locase-all s)))
         (s (and s (if (string-starts? s "#") (substring s 1 (string-length s)) s))))
    (and s (== (string-length s) 6) (!= s "000000")
         (list-and (map (lambda (c) (or (char-numeric? c)
                                        (in? c '(#\a #\b #\c #\d #\e #\f))))
                        (string->list s)))
         (string-append "#" s))))

(tm-define (office-emu->length s)
  (:synopsis "The length of TeXmacs for @s English Metric Units")
  ;; 360000 EMU are a centimeter
  (with n (office-number s)
    (if (and n (> n 0))
        ;; in thousandths of a centimeter, without a useless .0
        (let* ((k (inexact->exact (round (/ n 360.0))))
               (whole (quotient k 1000))
               (part (modulo k 1000)))
          (string-append
            (number->string whole)
            (if (== part 0) ""
                (let* ((d (number->string (+ 1000 part)))
                       (d (substring d 1 4)))
                  (string-append
                    "." (let loop ((d d))
                          (if (string-ends? d "0")
                              (loop (substring d 0 (- (string-length d) 1)))
                              d)))))
            "cm"))
        "")))

(tm-define (office-monospace? font)
  (:synopsis "Whether the font @font is one for code")
  (and (string? font)
       (with f (locase-all font)
         (list-or (map (lambda (x) (>= (string-search-forwards x 0 f) 0))
                       '("courier" "consolas" "mono" "menlo" "monaco"
                         "typewriter" "fixed" "source code" "lucida console"
                         "andale"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ox-character-length s i)
  ;; The number of bytes of the character of UTF-8 at the place i of s,
  ;; whose first byte is 128 or more; 0 when the bytes are no character.
  ;; The number is negative for a character which XML does not allow: a
  ;; half of a pair of UTF-16, U+FFFE, U+FFFF.
  (let* ((n (string-length s))
         (byte (lambda (k) (if (< k n) (char->integer (string-ref s k)) 0)))
         (next? (lambda (k) (and (>= (byte k) #x80) (< (byte k) #xc0))))
         (b (byte i))
         (c (byte (+ i 1))))
    (cond ((and (>= b #xc2) (< b #xe0)) (if (next? (+ i 1)) 2 0))
          ((and (>= b #xe0) (< b #xf0))
           (cond ((not (and (next? (+ i 1)) (next? (+ i 2)))) 0)
                 ((and (== b #xe0) (< c #xa0)) 0)
                 ((and (== b #xed) (>= c #xa0)) -3)
                 ((and (== b #xef) (== c #xbf) (>= (byte (+ i 2)) #xbe)) -3)
                 (else 3)))
          ((and (>= b #xf0) (< b #xf5))
           (if (and (next? (+ i 1)) (next? (+ i 2)) (next? (+ i 3))
                    (not (and (== b #xf0) (< c #x90)))
                    (not (and (== b #xf4) (>= c #x90))))
               4 0))
          (else 0))))

(define ox-replacement
  ;; the replacement character, U+FFFD, in UTF-8
  (list->string (map integer->char '(#xef #xbf #xbd))))

(define (ox-escape s quote?)
  ;; The text s with the characters of XML as entities. The control
  ;; characters, which XML does not allow, are left out; the replacement
  ;; character stands for the bytes which are no character of UTF-8 and
  ;; for the other characters which XML does not allow.
  (let ((n (string-length s)))
    ;; (from start to i: characters which are written as they are)
    (let loop ((i 0) (start 0) (acc '()))
      (let ((flush (lambda ()
                     (if (> i start) (cons (substring s start i) acc) acc))))
        (if (>= i n)
            (apply string-append (reverse (flush)))
            (let* ((c (string-ref s i))
                   (b (char->integer c))
                   (entity (cond ((char=? c #\&) "&amp;")
                                 ((char=? c #\<) "&lt;")
                                 ((char=? c #\>) "&gt;")
                                 ((and quote? (char=? c #\")) "&quot;")
                                 ((and (< b 32) (not (in? c '(#\newline #\tab))))
                                  "")
                                 (else #f))))
              (cond (entity (loop (+ i 1) (+ i 1) (cons entity (flush))))
                    ((< b 128) (loop (+ i 1) start acc))
                    (else
                      (with k (ox-character-length s i)
                        (if (> k 0)
                            (loop (+ i k) start acc)
                            (with j (+ i (max 1 (- k)))
                              (loop j j (cons ox-replacement (flush))))))))))))))

(define (ox-serialize-sub x)
  ;; the pieces of the text of the node x, in a list
  (cond ((string? x) (list (ox-escape x #f)))
        ((not (pair? x)) '())
        (else
          (let ((tag (symbol->string (car x)))
                (attrs (ox-attrs x))
                (children (ox-children x)))
            (append
              (list "<" tag)
              (append-map (lambda (a)
                            (if (and (pair? (cdr a)) (string? (cadr a)))
                                (list " " (symbol->string (car a)) "=\""
                                      (ox-escape (cadr a) #t) "\"")
                                '()))
                          attrs)
              (if (null? children) (list "/>")
                  (append (list ">")
                          (append-map ox-serialize-sub children)
                          (list "</" tag ">"))))))))

(tm-define (ox-serialize x)
  (:synopsis "The text of the XML document whose element is the tree @x")
  (apply string-append
         (cons "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
               (ox-serialize-sub x))))

(tm-define (ox-serialize-element x)
  (:synopsis "The text of the element @x of XML, without a declaration")
  (apply string-append (ox-serialize-sub x)))

(tm-define (office-uri s)
  (:synopsis "The address @s, with %XX for the bytes which an address cannot hold")
  ;; the spaces, the characters which are not of ASCII, some delimiters
  (apply string-append
         (map (lambda (c)
                (with n (char->integer c)
                  (if (or (<= n 32) (>= n 127)
                          (in? c '(#\" #\< #\> #\\ #\^ #\` #\{ #\| #\})))
                      (string-append "%" (if (< n 16) "0" "")
                                     (upcase-all (number->string n 16)))
                      (string c))))
              (string->list s))))

(tm-define (office-decimal x)
  (:synopsis "The number @x with three decimals at most, and no useless ones")
  (let* ((k (inexact->exact (round (* 1000.0 (abs x)))))
         (whole (quotient k 1000))
         (part (modulo k 1000)))
    (string-append
      (if (and (< x 0) (> k 0)) "-" "")
      (number->string whole)
      (if (== part 0) ""
          (let loop ((d (substring (number->string (+ 1000 part)) 1 4)))
            (if (string-ends? d "0")
                (loop (substring d 0 (- (string-length d) 1)))
                (string-append "." d)))))))

(tm-define (office-labels x)
  (:synopsis "The names of the bookmarks of the node @x, from its attribute labels")
  (with s (ox-attr x 'labels)
    (if (or (not s) (== s "")) '()
        (list-filter (string-tokenize-by-char s #\space)
                     (lambda (t) (!= t ""))))))

(tm-define (office-numbered-headings? x)
  (:synopsis "Whether a heading of the office tree @x has a number")
  (cond ((not (pair? x)) #f)
        ((in? (car x) '(math image @)) #f)
        ((and (func? x 'p) (== (ox-attr x 'role) "heading") (ox-attr x 'number)) #t)
        (else (list-or (map office-numbered-headings? (ox-children x))))))

(tm-define (office-utf8 n)
  (:synopsis "The character of code @n, in UTF-8")
  (list->string
    (map integer->char
         (cond ((< n #x80) (list n))
               ((< n #x800) (list (+ #xc0 (quotient n 64)) (+ #x80 (modulo n 64))))
               ((< n #x10000)
                (list (+ #xe0 (quotient n 4096))
                      (+ #x80 (modulo (quotient n 64) 64))
                      (+ #x80 (modulo n 64))))
               (else
                 (list (+ #xf0 (quotient n 262144))
                       (+ #x80 (modulo (quotient n 4096) 64))
                       (+ #x80 (modulo (quotient n 64) 64))
                       (+ #x80 (modulo n 64))))))))

(tm-define (office-length->cm s)
  (:synopsis "The length @s (2cm, 1in, 10mm, 12pt, 96px) in centimeters, or #f")
  (and (string? s) (!= s "")
       (let* ((n (string-length s))
              (i (let loop ((i 0))
                   (if (and (< i n) (or (char-numeric? (string-ref s i))
                                        (char=? (string-ref s i) #\.)))
                       (loop (+ i 1)) i)))
              (v (string->number (substring s 0 i)))
              (scale (assoc-ref '(("cm" . 1.0) ("mm" . 0.1) ("in" . 2.54)
                                  ("pt" . 0.03528) ("px" . 0.02646))
                                (substring s i n))))
         (and v scale (> v 0) (* v scale)))))
