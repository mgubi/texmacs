
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
;;              author, date, abstract, heading, quote, code, caption),
;;              level (of a heading) and align (center, right, justify)
;;            (list (item block...)...)  with the attribute kind (bullet,
;;              number)
;;            (table (row (cell block...)...)...)  with the attributes
;;              align, width (a part of the paragraph: 0.5par) and columns
;;              (the parts of the columns in the table: "0.25 0.75"); the
;;              cells with the attributes header, colspan, rowspan,
;;              covered (by a wider or a higher cell), borders (the sides
;;              t, b, l, r which have one, or none) and background
;;            (toc)  the table of contents
;;            (pagebreak)
;;   inlines: "text"  (em ...)  (strong ...)  (underline ...)  (strike ...)
;;            (sub ...)  (sup ...)  (code ...)  (smallcaps ...)  (mark ...)
;;            (color ...)  with the attribute value, #rrggbb
;;            (link ...)  with the attribute href (#name inside the document)
;;            (note block...)  (br)  (tab)
;;            (image)  with the attributes name, data (the bytes of the
;;              file), width, height (lengths of TeXmacs) and alt
;;            (math "MathML")  with the attribute display
;;            (bookmark)  (ref ...)  with the attribute name
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

(tm-define (office-xml archive name)
  (:synopsis "The element of the XML entry @name of the @archive, or #f")
  (with s (ahash-ref archive name)
    (and s (ox-root (parse-xml s)))))

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

(tm-define (office-merge l)
  (:synopsis "The inline nodes @l, with the neighbours of the same kind as one")
  ;; the runs of a text come one by one, each with all its properties
  (cond ((null? l) l)
        ((== (car l) "") (office-merge (cdr l)))
        ((null? (cdr l))
         (if (and (pair? (car l)) (in? (caar l) office-wrappers))
             (list (office-wrapper-node (car l)
                                        (office-merge (ox-children (car l)))))
             l))
        ((and (string? (car l)) (string? (cadr l)))
         (office-merge (cons (string-append (car l) (cadr l)) (cddr l))))
        ((office-same-wrapper? (car l) (cadr l))
         (office-merge (cons (office-wrapper-node
                               (car l)
                               (append (ox-children (car l))
                                       (ox-children (cadr l))))
                             (cddr l))))
        ((and (pair? (car l)) (in? (caar l) office-wrappers))
         (cons (office-wrapper-node (car l) (office-merge (ox-children (car l))))
               (office-merge (cdr l))))
        (else (cons (car l) (office-merge (cdr l))))))

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
  (with n (and (string? s) (string->number s))
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
