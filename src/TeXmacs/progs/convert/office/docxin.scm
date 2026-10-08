
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : docxin.scm
;; DESCRIPTION : reading Word documents (.docx) into office trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A .docx is a zip archive: word/document.xml is the text, a sequence of
;; paragraphs (w:p) of runs (w:r) and of tables (w:tbl); word/styles.xml
;; has the styles, which tell what a paragraph is (a heading, a quotation);
;; word/numbering.xml the lists, which in the text are paragraphs with a
;; list and a level; word/footnotes.xml the notes; the files _rels/*.rels
;; the addresses of the links and the names of the images in the archive.
;; See office-tools.scm for the tree which is made of all this.

(texmacs-module (convert office docxin)
  (:use (convert office office-tools)
        (convert office omml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State: the document which is read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define docx-archive #f)                  ; the entries of the archive
(define docx-part "word/document.xml")    ; the entry which is read
(define docx-styles (make-ahash-table))   ; style id -> properties
(define docx-style-memo (make-ahash-table))
(define docx-lists (make-ahash-table))    ; (list id . level) -> kind
(define docx-rels (make-ahash-table))     ; (entry . id) -> (target external?)
(define docx-notes (make-ahash-table))    ; (kind . id) -> element

(define (docx-val x)
  ;; the value of an element such as <w:jc w:val="center"/>
  (and x (ox-attr x 'w:val)))

(define (docx-child-val x tag)
  (docx-val (ox-child x tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of runs and of paragraphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The properties are association lists, in which the first value of a key
;; counts: those of an element come before those of its style, and those
;; of a style before those of the style it is based on.

(define (docx-toggle x tag key)
  ;; <w:b/> sets, <w:b w:val="0"/> unsets
  (with y (and x (ox-child x tag))
    (if (not y) '()
        (list (cons key (not (in? (docx-val y) '("0" "false" "off" "none"))))))))

(define (docx-run-properties x)
  ;; the properties of an element w:rPr
  (if (not x) '()
      (append
        (docx-toggle x 'w:i 'em)
        (docx-toggle x 'w:b 'strong)
        (docx-toggle x 'w:u 'underline)
        (docx-toggle x 'w:strike 'strike)
        (docx-toggle x 'w:dstrike 'strike)
        (docx-toggle x 'w:smallCaps 'smallcaps)
        (with v (docx-child-val x 'w:vertAlign)
          (cond ((== v "superscript") '((sup . #t) (sub . #f)))
                ((== v "subscript") '((sub . #t) (sup . #f)))
                ((== v "baseline") '((sub . #f) (sup . #f)))
                (else '())))
        (with f (ox-child x 'w:rFonts)
          (if (and f (ox-attr f 'w:ascii))
              (list (cons 'code (office-monospace? (ox-attr f 'w:ascii))))
              '()))
        (with s (docx-child-val x 'w:rStyle)
          (if s (list (cons 'style s)) '())))))

(define (docx-paragraph-properties x)
  ;; the properties of an element w:pPr
  (if (not x) '()
      (append
        (with v (docx-child-val x 'w:jc)
          (if v (list (cons 'align v)) '()))
        (with v (docx-child-val x 'w:outlineLvl)
          (if (and v (string->number v)) (list (cons 'outline (string->number v)))
              '()))
        (with n (ox-child x 'w:numPr)
          (if (not n) '()
              (append
                (with v (docx-child-val n 'w:numId)
                  (if v (list (cons 'list-id v)) '()))
                (with v (docx-child-val n 'w:ilvl)
                  (if v (list (cons 'list-level v)) '())))))
        (if (ox-child x 'w:pageBreakBefore) '((page-break . #t)) '())
        (with s (docx-child-val x 'w:pStyle)
          (if s (list (cons 'style s)) '())))))

(define (docx-read-styles)
  ;; the styles of word/styles.xml, with their own properties
  (set! docx-styles (make-ahash-table))
  (set! docx-style-memo (make-ahash-table))
  (with root (office-xml docx-archive "word/styles.xml")
    (when root
      (for (x (ox-childs root 'w:style))
        (with id (ox-attr x 'w:styleId)
          (when id
            (ahash-set!
              docx-styles id
              (append
                (list (cons 'name (locase-all (or (docx-child-val x 'w:name) id))))
                (with b (docx-child-val x 'w:basedOn)
                  (if b (list (cons 'based-on b)) '()))
                ;; a style of paragraphs has properties of runs too, which
                ;; are not those of a style of runs: they are told apart
                (if (== (ox-attr x 'w:type) "paragraph")
                    (docx-paragraph-properties (ox-child x 'w:pPr))
                    (docx-run-properties (ox-child x 'w:rPr)))))))))))

(define (docx-get props key)
  (with p (assoc key props) (and p (cdr p))))

(define (docx-style-sub id seen)
  (with own (and id (not (in? id seen)) (ahash-ref docx-styles id))
    (if (not own) '()
        (append own (docx-style-sub (docx-get own 'based-on) (cons id seen))))))

(define (docx-style id)
  ;; the properties of the style, with those of the styles it is based on
  (cond ((not id) '())
        ((ahash-ref docx-style-memo id) => identity)
        (else (with r (docx-style-sub id '())
                (ahash-set! docx-style-memo id r)
                r))))

(define (docx-style-names id)
  ;; the names of the style and of those it is based on
  (let loop ((id id) (seen '()))
    (with own (and id (not (in? id seen)) (ahash-ref docx-styles id))
      (if (not own) '()
          (cons (docx-get own 'name)
                (loop (docx-get own 'based-on) (cons id seen)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What a paragraph is: from the names of its styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The name of a style (w:name) is the English one also where its
;; identifier is in the language of the program which wrote the file.
(define docx-roles
  '(("title" . "title") ("subtitle" . "subtitle")
    ("author" . "author") ("authors" . "author") ("date" . "date")
    ("abstract" . "abstract") ("abstract title" . "skip")
    ("quote" . "quote") ("intense quote" . "quote") ("block text" . "quote")
    ("quotations" . "quote") ("block quotation" . "quote")
    ("caption" . "caption") ("image caption" . "caption")
    ("table caption" . "caption") ("captioned figure" . "figure")
    ("source code" . "code") ("html preformatted" . "code")
    ("plain text" . "code") ("preformatted text" . "code")
    ("macro" . "code") ("code" . "code")
    ("definition term" . "term") ("definition" . "definition")))

(define (docx-heading-level name)
  ;; "heading 2" is 2
  (and (string-starts? name "heading ")
       (string->number (substring name 8 (string-length name)))))

(define (docx-role props)
  ;; (role level) of a paragraph with these properties
  (let* ((names (docx-style-names (docx-get props 'style)))
         (level (list-find (map docx-heading-level names) identity))
         (role (list-find (map (lambda (n) (assoc-ref docx-roles n)) names)
                          identity))
         (outline (docx-get props 'outline)))
    (cond (level (list "heading" (number->string level)))
          (role (list role #f))
          ;; a level of the outline without a heading style
          ((and outline (>= outline 0) (< outline 9))
           (list "heading" (number->string (+ outline 1))))
          (else (list #f #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relations, lists and notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-rels-name part)
  ;; word/document.xml has its relations in word/_rels/document.xml.rels
  (let* ((l (string-tokenize-by-char part #\/))
         (dir (reverse (cdr (reverse l)))))
    (string-recompose (append dir (list "_rels" (string-append (cAr l) ".rels")))
                      "/")))

(define (docx-read-rels part)
  (with root (office-xml docx-archive (docx-rels-name part))
    (when root
      (for (x (ox-childs root 'Relationship))
        (ahash-set! docx-rels (cons part (ox-attr x 'Id))
                    (list (or (ox-attr x 'Target) "")
                          (== (ox-attr x 'TargetMode) "External")))))))

(define (docx-rel id)
  ;; (target external?) for the relation id of the entry which is read
  (and id (ahash-ref docx-rels (cons docx-part id))))

(define (docx-list-format lvl)
  (if (in? (docx-child-val lvl 'w:numFmt) '("bullet" "none"))
      "bullet" "number"))

(define (docx-read-lists)
  ;; word/numbering.xml: a list (w:num) refers to an abstract one, whose
  ;; levels have a format, "bullet" or a way of numbering, which the list
  ;; may override. The items of one list in the text may refer to several
  ;; lists of the same abstract one (each restarts the numbers), so the
  ;; abstract list is what tells two lists apart.
  (set! docx-lists (make-ahash-table))
  (with root (office-xml docx-archive "word/numbering.xml")
    (when root
      (with abstract (make-ahash-table)
        (for (x (ox-childs root 'w:abstractNum))
          (ahash-set! abstract (ox-attr x 'w:abstractNumId) x))
        (for (x (ox-childs root 'w:num))
          (let* ((id (ox-attr x 'w:numId))
                 (aid (docx-child-val x 'w:abstractNumId))
                 (a (ahash-ref abstract aid)))
            (when (and id a)
              (ahash-set! docx-lists (cons id 'group) aid)
              (for (lvl (ox-childs a 'w:lvl))
                (ahash-set! docx-lists (cons id (ox-attr lvl 'w:ilvl))
                            (docx-list-format lvl)))
              (for (o (ox-childs x 'w:lvlOverride))
                (with lvl (ox-child o 'w:lvl)
                  (when (and lvl (ox-child lvl 'w:numFmt))
                    (ahash-set! docx-lists (cons id (ox-attr o 'w:ilvl))
                                (docx-list-format lvl))))))))))))

(define (docx-list-kind id level)
  (or (ahash-ref docx-lists (cons id level)) "bullet"))

(define (docx-list-group id)
  (or (ahash-ref docx-lists (cons id 'group)) id))

(define (docx-read-notes entry tag kind)
  (with root (office-xml docx-archive entry)
    (when root
      (for (x (ox-childs root tag))
        ;; (the separators of the notes are notes with a type)
        (when (not (in? (ox-attr x 'w:type)
                        '("separator" "continuationSeparator"
                          "continuationNotice")))
          (ahash-set! docx-notes (cons kind (ox-attr x 'w:id))
                      (cons entry x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images and mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-image-node id width height alt)
  ;; the image of the relation id
  (with rel (docx-rel id)
    (if (or (not rel) (cadr rel)) '()
        (let* ((name (office-resolve docx-part (car rel)))
               (data (office-entry docx-archive name)))
          (if (not data) '()
              (list (office-node
                      'image
                      `((name ,(cAr (string-tokenize-by-char name #\/)))
                        (data ,data) (width ,width) (height ,height)
                        (alt ,alt)))))))))

(define (docx-drawing x)
  ;; a picture of DrawingML: its file is the relation of a:blip
  (let* ((blip (ox-find x 'a:blip))
         (extent (ox-find x 'wp:extent))
         (props (ox-find x 'wp:docPr)))
    (if (not blip) '()
        (docx-image-node
          (ox-attr blip 'r:embed)
          (office-emu->length (and extent (ox-attr extent 'cx)))
          (office-emu->length (and extent (ox-attr extent 'cy)))
          (and props (or (ox-attr props 'descr) (ox-attr props 'title)))))))

(define (docx-picture x)
  ;; a picture of VML, in older documents
  (with data (ox-find x 'v:imagedata)
    (if (not data) '()
        (docx-image-node (ox-attr data 'r:id) "" ""
                         (ox-attr data 'o:title)))))

(define (docx-math x display?)
  ;; the formulas of an element m:oMath, or m:oMathPara which holds
  ;; several: each as MathML
  (if (func? x 'm:oMathPara)
      (append-map (lambda (y) (docx-math y #t)) (ox-childs x 'm:oMath))
      (list (office-node 'math `((display ,(and display? "true"))
                                 (form "sxml"))
                         (omml->mathml x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the styles of runs which are not a way of writing: the look of a link,
;; of the mark of a note
(define docx-plain-styles
  '("hyperlink" "internet link" "followedhyperlink" "footnote reference"
    "endnote reference" "footnote characters" "endnote characters"
    "footnote anchor" "endnote anchor"))

(define (docx-run-wrappers r)
  ;; the wrappers of the text of a run, the innermost first
  (let* ((own (docx-run-properties (ox-child r 'w:rPr)))
         (id (docx-get own 'style))
         (names (docx-style-names id))
         (props (if (and (pair? names) (in? (car names) docx-plain-styles))
                    own
                    (append own (docx-style id))))
         (code? (or (docx-get props 'code)
                    (list-or (map (lambda (n)
                                    (in? n '("verbatim char" "html code"
                                             "html typewriter" "source text"
                                             "code" "inline code")))
                                  names)))))
    (list-filter
      (list (and code? 'code)
            (and (docx-get props 'sub) 'sub)
            (and (docx-get props 'sup) 'sup)
            (and (docx-get props 'smallcaps) 'smallcaps)
            (and (docx-get props 'strike) 'strike)
            (and (docx-get props 'underline) 'underline)
            (and (docx-get props 'em) 'em)
            (and (docx-get props 'strong) 'strong))
      identity)))

(define (docx-trim-note l)
  ;; the text of a note starts with its mark and a space: without the space
  (if (and (pair? l) (func? (car l) 'p))
      (let* ((p (car l))
             (c (ox-children p)))
        (if (and (pair? c) (func? (car c) 'tab))
            (docx-trim-note (cons (apply office-node
                                         (cons* 'p (ox-attrs p) (cdr c)))
                                  (cdr l)))
        (if (and (pair? c) (string? (car c)))
            (cons (apply office-node
                         (cons* 'p (ox-attrs p)
                                (office-merge
                                  (cons (string-trim-left-spaces (car c)) (cdr c)))))
                  (cdr l))
            l)))
      l))

(define (string-trim-left-spaces s)
  (let loop ((i 0))
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
        (loop (+ i 1))
        (substring s i (string-length s)))))

(define (docx-note kind id)
  (with entry (ahash-ref docx-notes (cons kind id))
    (if (not entry) '()
        (with old docx-part
          (set! docx-part (car entry))
          (with l (docx-blocks (ox-children (cdr entry)))
            (set! docx-part old)
            (list (cons 'note (docx-trim-note l))))))))

(define (docx-run-item x)
  ;; the nodes of a child of a run
  (cond ((not (pair? x)) '())
        ((func? x 'w:t) (list (ox-text x)))
        ((func? x 'w:tab) '((tab)))
        ((func? x 'w:br)
         (if (== (ox-attr x 'w:type) "page") '((pagebreak)) '((br))))
        ((func? x 'w:cr) '((br)))
        ((func? x 'w:noBreakHyphen) '("-"))
        ((func? x 'w:drawing) (docx-drawing x))
        ((func? x 'w:pict) (docx-picture x))
        ((func? x 'w:object) (docx-picture x))
        ((func? x 'mc:AlternateContent)
         (with c (or (ox-child x 'mc:Choice) (ox-child x 'mc:Fallback))
           (append-map docx-run-item (ox-children c))))
        ((func? x 'w:footnoteReference) (docx-note 'foot (ox-attr x 'w:id)))
        ((func? x 'w:endnoteReference) (docx-note 'end (ox-attr x 'w:id)))
        (else '())))

(define (docx-run r)
  ;; the nodes of a run: its text inside its wrappers; what is not text
  ;; stays outside
  (let* ((l (append-map docx-run-item (ox-children r)))
         (w (docx-run-wrappers r)))
    (append-map (lambda (x)
                  (if (string? x) (office-wrap (list x) w) (list x)))
                l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The text of a paragraph, with its links and its fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-field-argument instr)
  ;; the first argument of the instruction of a field, without its quotes
  (let* ((l (list-filter (string-tokenize-by-char instr #\space)
                         (lambda (s) (!= s ""))))
         (args (list-filter (if (null? l) l (cdr l))
                            (lambda (s) (not (string-starts? s "\\"))))))
    (if (null? args) ""
        (with a (car args)
          (if (and (>= (string-length a) 2) (string-starts? a "\"")
                   (string-ends? a "\""))
              (substring a 1 (- (string-length a) 1))
              a)))))

(define (docx-field instr l)
  ;; the result l of a field: a link, a reference, or the result itself
  (let* ((s (string-trim-spaces instr))
         (arg (docx-field-argument s)))
    (cond ((null? l) l)
          ((and (string-starts? s "HYPERLINK") (!= arg ""))
           ;; with the switch \l the link is to a bookmark
           (list `(link (@ (href ,(if (>= (string-search-forwards "\\l" 0 s) 0)
                                      (string-append "#" arg)
                                      arg)))
                        ,@l)))
          ((and (or (string-starts? s "REF ") (string-starts? s "PAGEREF "))
                (!= arg ""))
           (list `(ref (@ (name ,arg)) ,@l)))
          (else l))))

(define (docx-unwrap l)
  ;; the children of a paragraph, with those of the elements which only
  ;; hold others in their place
  (append-map
    (lambda (x)
      (cond ((not (pair? x)) '())
            ((in? (car x) '(w:ins w:smartTag w:customXml w:sdtContent w:moveTo))
             (docx-unwrap (ox-children x)))
            ((func? x 'w:sdt)
             (with c (ox-child x 'w:sdtContent)
               (if c (docx-unwrap (ox-children c)) '())))
            (else (list x))))
    l))

(define (docx-inlines l)
  ;; the nodes of the children l of a paragraph. A field is a run which
  ;; begins it, runs with its instruction, a run which separates, the runs
  ;; of its result and a run which ends it: fields is the stack of the
  ;; fields which are open, each (state instruction nodes-before).
  (let loop ((l (docx-unwrap l)) (acc '()) (fields '()))
    (if (null? l) (reverse acc)
        (let* ((x (car l))
               (char (and (func? x 'w:r) (ox-child x 'w:fldChar)))
               (type (and char (ox-attr char 'w:fldCharType)))
               (state (and (pair? fields) (caar fields))))
          (cond ((== type "begin")
                 (loop (cdr l) '() (cons (list 'instr "" acc) fields)))
                ((and (== type "separate") (pair? fields))
                 (loop (cdr l) '()
                       (cons (list 'result (cadar fields) (caddar fields))
                             (cdr fields))))
                ((and (== type "end") (pair? fields))
                 (loop (cdr l)
                       (append (reverse (docx-field (cadar fields)
                                                    (if (== state 'result)
                                                        (reverse acc) '())))
                               (caddar fields))
                       (cdr fields)))
                ((== state 'instr)
                 ;; the instruction, in one run or in several
                 (loop (cdr l) acc
                       (cons (list 'instr
                                   (string-append
                                     (cadar fields)
                                     (if (func? x 'w:r)
                                         (apply string-append
                                                (map ox-text
                                                     (ox-childs x 'w:instrText)))
                                         ""))
                                   (caddar fields))
                             (cdr fields))))
                (else
                  (loop (cdr l) (append (reverse (docx-inline x)) acc)
                        fields)))))))

(define (docx-inline x)
  ;; the nodes of a child of a paragraph
  (cond ((func? x 'w:r) (docx-run x))
        ((func? x 'w:hyperlink)
         (let* ((rel (docx-rel (ox-attr x 'r:id)))
                (anchor (ox-attr x 'w:anchor))
                (href (cond ((and rel anchor)
                             (string-append (car rel) "#" anchor))
                            (rel (car rel))
                            (anchor (string-append "#" anchor))
                            (else #f)))
                (l (docx-inlines (ox-children x))))
           (cond ((null? l) '())
                 (href (list `(link (@ (href ,href)) ,@l)))
                 (else l))))
        ((func? x 'w:fldSimple)
         (docx-field (or (ox-attr x 'w:instr) "")
                     (docx-inlines (ox-children x))))
        ((func? x 'w:bookmarkStart)
         (with name (ox-attr x 'w:name)
           (if (or (not name) (== name "_GoBack")) '()
               (list `(bookmark (@ (name ,name)))))))
        ((func? x 'm:oMathPara) (docx-math x #t))
        ((func? x 'm:oMath) (docx-math x #f))
        (else '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraphs and tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-paragraph x)
  ;; the paragraph, with the list it is an item of as attributes list-id,
  ;; list-level and list-kind, for docx-group-lists
  (let* ((own (docx-paragraph-properties (ox-child x 'w:pPr)))
         (props (append own (docx-style (docx-get own 'style))))
         (role (docx-role props))
         (align (docx-get props 'align))
         (id (docx-get props 'list-id))
         (level (or (docx-get props 'list-level) "0"))
         (item? (and id (!= id "0") (not (car role))))
         (l (office-merge (docx-inlines (ox-children x)))))
    (append
      (if (docx-get own 'page-break) '((pagebreak)) '())
      (if (and (null? l) (not item?)) '()
          (list (apply office-node
                       (cons* 'p
                              `((role ,(car role)) (level ,(cadr role))
                                (align ,(cond ((== align "center") "center")
                                              ((in? align '("right" "end")) "right")
                                              (else #f)))
                                (list-id ,(and item? (docx-list-group id)))
                                (list-level ,(and item? level))
                                (list-kind ,(and item? (docx-list-kind id level))))
                              l)))))))

(define (docx-cell x header?)
  ;; the cells of a w:tc: the cell, and those which it covers on its right
  (let* ((pr (ox-child x 'w:tcPr))
         (span (or (and pr (string->number (or (docx-child-val pr 'w:gridSpan) "1")))
                   1))
         (merge (and pr (ox-child pr 'w:vMerge)))
         (covered? (and merge (!= (docx-val merge) "restart")))
         (covered `(cell (@ (covered "true")))))
    (cons (if covered? covered
              (apply office-node
                     (cons* 'cell
                            `((header ,(and header? "true"))
                              (colspan ,(and (> span 1) (number->string span)))
                              (vmerge ,(and merge "start")))
                            (docx-blocks (ox-children x)))))
          (map (lambda (i) covered) (iota (- span 1))))))

(define (docx-row x)
  (let* ((pr (ox-child x 'w:trPr))
         (header? (and pr (ox-child pr 'w:tblHeader)
                       (not (in? (docx-child-val pr 'w:tblHeader)
                                 '("0" "false"))))))
    (cons 'row (append-map (lambda (c) (docx-cell c header?))
                           (ox-childs x 'w:tc)))))

(define (docx-row-spans rows)
  ;; a cell which starts a vertical merge gets the number of rows it
  ;; covers: the cells below it in its column which are covered
  (with grid (map cdr rows)
    (let loop ((rest grid) (acc '()))
      (if (null? rest) (reverse acc)
          (loop (cdr rest)
                (cons
                  (cons 'row
                        (map (lambda (cell col)
                               (if (not (ox-attr cell 'vmerge)) cell
                                   (let count ((below (cdr rest)) (n 1))
                                     (if (and (pair? below)
                                              (< col (length (car below)))
                                              (ox-attr (list-ref (car below) col)
                                                       'covered))
                                         (count (cdr below) (+ n 1))
                                         (apply office-node
                                                (cons* 'cell
                                                       `((header ,(ox-attr cell 'header))
                                                         (colspan ,(ox-attr cell 'colspan))
                                                         (rowspan ,(and (> n 1) (number->string n))))
                                                       (ox-children cell)))))))
                             (car rest) (iota (length (car rest)))))
                  acc))))))

(define (docx-table x)
  (with rows (map docx-row (ox-childs x 'w:tr))
    (if (null? rows) '()
        (list (cons 'table (docx-row-spans rows))))))

(define (docx-block x)
  (cond ((func? x 'w:p) (docx-paragraph x))
        ((func? x 'w:tbl) (docx-table x))
        ((func? x 'w:sdt)
         (with c (ox-child x 'w:sdtContent)
           (if c (docx-blocks-sub (ox-children c)) '())))
        ((and (pair? x) (in? (car x) '(w:ins w:customXml w:moveTo)))
         (docx-blocks-sub (ox-children x)))
        (else '())))

(define (docx-blocks-sub l)
  ;; A bookmark between two paragraphs belongs to the next one.
  (let loop ((l l) (marks '()) (acc '()))
    (cond ((null? l) (reverse acc))
          ((func? (car l) 'w:bookmarkStart)
           (loop (cdr l) (append marks (docx-inline (car l))) acc))
          (else
            (with b (docx-block (car l))
              (if (and (pair? marks) (pair? b) (func? (cAr b) 'p))
                  (let* ((p (cAr b))
                         (attrs (ox-attrs p)))
                    (loop (cdr l) '()
                          (append
                            (list (apply office-node
                                         (cons* 'p attrs
                                                (append marks (ox-children p)))))
                            (cdr (reverse b))
                            acc)))
                  (loop (cdr l) marks (append (reverse b) acc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists: from the paragraphs which are items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-plain-paragraph p)
  ;; without the attributes of its list
  (apply office-node
         (cons* 'p
                (list-filter (ox-attrs p)
                             (lambda (a) (not (in? (car a) '(list-id list-level
                                                             list-kind)))))
                (ox-children p))))

(define (docx-item-level x)
  (and (func? x 'p) (ox-attr x 'list-id)
       (or (string->number (ox-attr x 'list-level)) 0)))

(define (docx-list l level)
  ;; (list . rest): the list of the items at the start of l which are at
  ;; this level or deeper, the deeper ones as lists inside the items
  (let* ((id (ox-attr (car l) 'list-id))
         (kind (ox-attr (car l) 'list-kind)))
    (let loop ((l l) (items '()))
      (with n (and (pair? l) (docx-item-level (car l)))
        (cond ((or (not n) (< n level)
                   (and (== n level)
                        (or (!= (ox-attr (car l) 'list-id) id)
                            (!= (ox-attr (car l) 'list-kind) kind))))
               (cons `(list (@ (kind ,kind)) ,@(reverse items)) l))
              ((== n level)
               (loop (cdr l)
                     (cons (list 'item (docx-plain-paragraph (car l))) items)))
              (else
                ;; a deeper level: a list inside the last item
                (with sub (docx-list l n)
                  (loop (cdr sub)
                        (if (null? items)
                            (list (list 'item (car sub)))
                            (cons (append (car items) (list (car sub)))
                                  (cdr items)))))))))))

(define (docx-group-lists l)
  (cond ((null? l) l)
        ((docx-item-level (car l))
         (with r (docx-list l (docx-item-level (car l)))
           (cons (car r) (docx-group-lists (cdr r)))))
        (else (cons (car l) (docx-group-lists (cdr l))))))

(define (docx-blocks l)
  (docx-group-lists (docx-blocks-sub l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-meta)
  ;; the title and the author of docProps/core.xml
  (with root (office-xml docx-archive "docProps/core.xml")
    (if (not root) '()
        (let* ((get (lambda (tag key)
                      (with x (ox-child root tag)
                        (if (and x (!= (string-trim-spaces (ox-text x)) ""))
                            (list (list key (string-trim-spaces (ox-text x))))
                            '()))))
               (l (append (get 'dc:title 'title) (get 'dc:creator 'author))))
          (if (null? l) '() (list (cons 'meta l)))))))

(define (docx-main-part)
  ;; the entry of the text: the one which _rels/.rels names, or the usual
  (let* ((root (office-xml docx-archive "_rels/.rels"))
         (rel (and root
                   (list-find (ox-childs root 'Relationship)
                              (lambda (x)
                                (string-ends? (or (ox-attr x 'Type) "")
                                              "/officeDocument")))))
         (target (and rel (ox-attr rel 'Target))))
    (if target (office-resolve "" target) "word/document.xml")))

(tm-define (parse-docx-document s)
  (:type (-> string stree))
  (:synopsis "Read the Word document @s, a zip archive, into an office tree")
  (set! docx-archive (office-archive s))
  (if (not docx-archive) '(office)
      (begin
        (set! docx-part (docx-main-part))
        (set! docx-rels (make-ahash-table))
        (set! docx-notes (make-ahash-table))
        (docx-read-styles)
        (docx-read-lists)
        (for (part (list docx-part "word/footnotes.xml" "word/endnotes.xml"))
          (docx-read-rels part))
        (docx-read-notes "word/footnotes.xml" 'w:footnote 'foot)
        (docx-read-notes "word/endnotes.xml" 'w:endnote 'end)
        (let* ((root (office-xml docx-archive docx-part))
               (body (and root (ox-child root 'w:body)))
               (r `(office ,@(docx-meta)
                           ,@(if body (docx-blocks (ox-children body)) '()))))
          (set! docx-archive #f)
          (set! docx-styles (make-ahash-table))
          (set! docx-style-memo (make-ahash-table))
          (set! docx-notes (make-ahash-table))
          r))))
