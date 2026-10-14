
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
(define docx-open-notes '())              ; the notes which are being read
(define docx-fields '())                  ; the fields which are open

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
        (docx-toggle x 'w:vanish 'hidden)
        ;; a run of a script which is written from right to left, or of a
        ;; complex one, is italic and bold by properties of its own
        (docx-toggle x 'w:iCs 'em-cs)
        (docx-toggle x 'w:bCs 'strong-cs)
        (docx-toggle x 'w:rtl 'complex)
        (docx-toggle x 'w:cs 'complex)
        (with v (docx-child-val x 'w:vertAlign)
          (cond ((== v "superscript") '((sup . #t) (sub . #f)))
                ((== v "subscript") '((sub . #t) (sup . #f)))
                ((== v "baseline") '((sub . #f) (sup . #f)))
                (else '())))
        (with c (office-color (docx-child-val x 'w:color))
          (if (ox-child x 'w:color) (list (cons 'color c)) '()))
        (with h (docx-child-val x 'w:highlight)
          (if h (list (cons 'mark (!= h "none"))) '()))
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
        (with v (office-integer (docx-child-val x 'w:outlineLvl) #f -1 9)
          (if v (list (cons 'outline v)) '()))
        (with n (ox-child x 'w:numPr)
          (if (not n) '()
              (append
                (with v (docx-child-val n 'w:numId)
                  (if v (list (cons 'list-id v)) '()))
                (with v (docx-child-val n 'w:ilvl)
                  (if v (list (cons 'list-level v)) '())))))
        (if (ox-child x 'w:pageBreakBefore) '((page-break . #t)) '())
        (with ind (ox-child x 'w:ind)
          (with v (and ind (office-integer (or (ox-attr ind 'w:left)
                                               (ox-attr ind 'w:start))
                                           #f 0 100000))
            (if v (list (cons 'indent v)) '())))
        ;; a large first letter, in a frame of its own
        (with f (ox-child x 'w:framePr)
          (if (and f (in? (ox-attr f 'w:dropCap) '("drop" "margin")))
              '((dropcap . #t)) '()))
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
                ;; (a style of lists says in them which list it is)
                (cond ((in? (ox-attr x 'w:type) '("paragraph" "numbering"))
                       (docx-paragraph-properties (ox-child x 'w:pPr)))
                      ((== (ox-attr x 'w:type) "table")
                       (docx-table-style-properties x))
                      (else (docx-run-properties (ox-child x 'w:rPr))))))))))))

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
    ("definition term" . "term") ("definition" . "definition")
    ("theorem" . "theorem") ("remark" . "remark") ("proof" . "proof")
    ("bibliography" . "bibitem")))

(define (docx-toc-style? name)
  ;; the entries of a table of contents, and its heading
  (or (string-starts? name "toc ") (string-starts? name "contents ")
      (== name "table of figures")))

(define (docx-heading-level name)
  ;; "heading 2" is 2
  (and (string-starts? name "heading ")
       (office-integer (substring name 8 (string-length name)) #f 1 9)))

(define (docx-role props)
  ;; (role level) of a paragraph with these properties
  (let* ((names (docx-style-names (docx-get props 'style)))
         (level (list-find (map docx-heading-level names) identity))
         (role (list-find (map (lambda (n) (assoc-ref docx-roles n)) names)
                          identity))
         (outline (docx-get props 'outline)))
    (cond ((list-or (map docx-toc-style? names)) (list "toc" #f))
          (level (list "heading" (number->string level)))
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

(define (docx-blank? s)
  ;; spaces and nothing else: the mark of a level which shows none
  (and (!= s "")
       (list-and (map (lambda (c) (in? c '(#\space #\tab))) (string->list s)))))

(define (docx-list-format lvl)
  ;; "bullet", "number", or "none" for a level without a mark: its
  ;; paragraphs are not items, but more paragraphs of the item before
  (let ((format (docx-child-val lvl 'w:numFmt))
        (text (docx-child-val lvl 'w:lvlText)))
    (cond ((== format "none") "none")
          ((and (== format "bullet") text (docx-blank? text)) "none")
          ((== format "bullet") "bullet")
          (else "number"))))

(define (docx-linked-list a abstract nums)
  ;; the abstract list a, or the one of the style of lists it refers to
  ;; (w:numStyleLink): the style names a list, which has an abstract one
  (let* ((link (docx-child-val a 'w:numStyleLink))
         (id (and link (docx-get (or (ahash-ref docx-styles link) '()) 'list-id)))
         (num (and id (ahash-ref nums id)))
         (b (and num (ahash-ref abstract (docx-child-val num 'w:abstractNumId)))))
    (if (and b (not (ox-child b 'w:numStyleLink))) b a)))

(define (docx-read-lists)
  ;; word/numbering.xml: a list (w:num) refers to an abstract one, whose
  ;; levels have a format, "bullet" or a way of numbering, which the list
  ;; may override. The items of one list in the text may refer to several
  ;; lists of the same abstract one (each restarts the numbers), so the
  ;; abstract list is what tells two lists apart.
  (set! docx-lists (make-ahash-table))
  (with root (office-xml docx-archive "word/numbering.xml")
    (when root
      (let ((abstract (make-ahash-table))
            (nums (make-ahash-table)))
        (for (x (ox-childs root 'w:abstractNum))
          (ahash-set! abstract (ox-attr x 'w:abstractNumId) x))
        (for (x (ox-childs root 'w:num))
          (ahash-set! nums (ox-attr x 'w:numId) x))
        (for (x (ox-childs root 'w:num))
          (let* ((id (ox-attr x 'w:numId))
                 (aid (docx-child-val x 'w:abstractNumId))
                 (a (ahash-ref abstract aid)))
            (when (and id a)
              (ahash-set! docx-lists (cons id 'group) aid)
              (for (lvl (ox-childs (docx-linked-list a abstract nums) 'w:lvl))
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

(define (docx-find-all x tag)
  ;; the elements tag inside x, at any depth
  (cond ((not (pair? x)) '())
        ((func? x tag) (list x))
        (else (append-map (lambda (y) (docx-find-all y tag)) (ox-children x)))))

(define (docx-drawing x)
  ;; a picture of DrawingML: its file is the relation of a:blip; a group
  ;; has several pictures, and its size is not the one of each of them
  (let* ((blips (docx-find-all x 'a:blip))
         (one? (list-1? blips))
         (extent (and one? (ox-find x 'wp:extent)))
         (props (ox-find x 'wp:docPr)))
    (append-map
      (lambda (blip)
        (docx-image-node
          (ox-attr blip 'r:embed)
          (office-emu->length (and extent (ox-attr extent 'cx)))
          (office-emu->length (and extent (ox-attr extent 'cy)))
          (and props (or (ox-attr props 'descr) (ox-attr props 'title)))))
      blips)))

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
  ;; the wrappers of the text of a run, the innermost first; hidden is the
  ;; first one of a run which is not shown
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
                                  names))))
         (complex? (docx-get props 'complex)))
    (list-filter
      (list (and (docx-get props 'hidden) 'hidden)
            (and code? 'code)
            (and (docx-get props 'color) (list 'color (docx-get props 'color)))
            (and (docx-get props 'mark) 'mark)
            (and (docx-get props 'sub) 'sub)
            (and (docx-get props 'sup) 'sup)
            (and (docx-get props 'smallcaps) 'smallcaps)
            (and (docx-get props 'strike) 'strike)
            (and (docx-get props 'underline) 'underline)
            (and (docx-get props (if complex? 'em-cs 'em)) 'em)
            (and (docx-get props (if complex? 'strong-cs 'strong)) 'strong))
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
  ;; the note, but inside itself; the fields of the text around it are not
  ;; those of the note
  (let ((entry (ahash-ref docx-notes (cons kind id)))
        (key (cons kind id)))
    (if (or (not entry) (in? key docx-open-notes)) '()
        (let ((old-part docx-part)
              (old-notes docx-open-notes)
              (old-fields docx-fields))
          (set! docx-part (car entry))
          (set! docx-open-notes (cons key old-notes))
          (set! docx-fields '())
          (with l (docx-blocks (ox-children (cdr entry)))
            (set! docx-part old-part)
            (set! docx-open-notes old-notes)
            (set! docx-fields old-fields)
            (list (cons 'note (docx-trim-note l))))))))

;; The characters of a font of symbols (w:sym) have codes of this font,
;; from F000 on or from 0: those which are common, as codes of Unicode.
(define docx-symbol-codes
  '((#x22 . #x2200) (#x24 . #x2203) (#x27 . #x220b) (#x2a . #x2217)
    (#x2d . #x2212) (#x40 . #x2245) (#x5c . #x2234) (#x5e . #x22a5)
    (#x7e . #x223c) (#xa2 . #x2032) (#xa3 . #x2264) (#xa4 . #x2044)
    (#xa5 . #x221e) (#xa7 . #x2663) (#xa8 . #x2666) (#xa9 . #x2665)
    (#xaa . #x2660) (#xab . #x2194) (#xac . #x2190) (#xad . #x2191)
    (#xae . #x2192) (#xaf . #x2193) (#xb0 . #xb0) (#xb1 . #xb1)
    (#xb2 . #x2033) (#xb3 . #x2265) (#xb4 . #xd7) (#xb5 . #x221d)
    (#xb6 . #x2202) (#xb7 . #x2022) (#xb8 . #xf7) (#xb9 . #x2260)
    (#xba . #x2261) (#xbb . #x2248) (#xbc . #x2026) (#xc0 . #x2135)
    (#xc1 . #x2111) (#xc2 . #x211c) (#xc3 . #x2118) (#xc4 . #x2297)
    (#xc5 . #x2295) (#xc6 . #x2205) (#xc7 . #x2229) (#xc8 . #x222a)
    (#xc9 . #x2283) (#xca . #x2287) (#xcb . #x2284) (#xcc . #x2282)
    (#xcd . #x2286) (#xce . #x2208) (#xcf . #x2209) (#xd0 . #x2220)
    (#xd1 . #x2207) (#xd2 . #xae) (#xd3 . #xa9) (#xd4 . #x2122)
    (#xd5 . #x220f) (#xd6 . #x221a) (#xd7 . #x22c5) (#xd8 . #xac)
    (#xd9 . #x2227) (#xda . #x2228) (#xdb . #x21d4) (#xdc . #x21d0)
    (#xdd . #x21d1) (#xde . #x21d2) (#xdf . #x21d3) (#xe0 . #x25ca)
    (#xe1 . #x2329) (#xe5 . #x2211) (#xf1 . #x232a) (#xf2 . #x222b)))

;; the letters of the font Symbol, from A and from a, are Greek ones
(define docx-symbol-capitals
  '(#x391 #x392 #x3a7 #x394 #x395 #x3a6 #x393 #x397 #x399 #x3d1 #x39a #x39b
    #x39c #x39d #x39f #x3a0 #x398 #x3a1 #x3a3 #x3a4 #x3a5 #x3c2 #x3a9 #x39e
    #x3a8 #x396))

(define docx-symbol-smalls
  '(#x3b1 #x3b2 #x3c7 #x3b4 #x3b5 #x3c6 #x3b3 #x3b7 #x3b9 #x3d5 #x3ba #x3bb
    #x3bc #x3bd #x3bf #x3c0 #x3b8 #x3c1 #x3c3 #x3c4 #x3c5 #x3d6 #x3c9 #x3be
    #x3c8 #x3b6))

(define docx-wingdings-codes
  '((#x22 . #x2702) (#x28 . #x260e) (#x2a . #x2709) (#x45 . #x261c)
    (#x46 . #x261e) (#x4a . #x263a) (#x4b . #x1f610) (#x4c . #x2639)
    (#x6c . #x25cf) (#x6e . #x25a0) (#x6f . #x25a1) (#x71 . #x2751)
    (#x75 . #x25c6) (#x76 . #x2756) (#x9f . #x2022) (#xa1 . #x25cb)
    (#xa7 . #x25aa) (#xa8 . #x25fb) (#xab . #x2605) (#xd8 . #x27a2)
    (#xdf . #x2190) (#xe0 . #x2192) (#xe1 . #x2191) (#xe2 . #x2193)
    (#xe3 . #x2196) (#xe4 . #x2197) (#xe5 . #x2199) (#xe6 . #x2198)
    (#xe7 . #x2190) (#xe8 . #x2192) (#xe9 . #x2191) (#xea . #x2193)
    (#xef . #x21e6) (#xf0 . #x21e8) (#xf1 . #x21e7) (#xf2 . #x21e9)
    (#xf3 . #x2194) (#xf4 . #x2195) (#xfb . #x2718) (#xfc . #x2714)
    (#xfd . #x2612) (#xfe . #x2611)))

(define (docx-hexadecimal s)
  ;; the number which the string s writes with digits from 0 to F, or #f
  (and (string? s) (!= s "") (<= (string-length s) 6)
       (let loop ((l (string->list (locase-all s))) (n 0))
         (cond ((null? l) n)
               ((char-numeric? (car l))
                (loop (cdr l) (+ (* 16 n) (- (char->integer (car l)) 48))))
               ((in? (car l) '(#\a #\b #\c #\d #\e #\f))
                (loop (cdr l) (+ (* 16 n) (- (char->integer (car l)) 87))))
               (else #f)))))

(define (docx-symbol x)
  ;; the character of an element w:sym, which gives a font and a code. A
  ;; code of a font of symbols which is not known is shown as a character
  ;; which stands for it; in another font the code is the one of Unicode.
  (let* ((font (locase-all (or (ox-attr x 'w:font) "")))
         (n (docx-hexadecimal (ox-attr x 'w:char)))
         (c (and n (if (and (>= n #xf000) (<= n #xf0ff)) (- n #xf000) n)))
         (u (cond ((not c) #f)
                  ((== font "symbol")
                   (cond ((assoc-ref docx-symbol-codes c) => identity)
                         ((and (>= c #x41) (<= c #x5a))
                          (list-ref docx-symbol-capitals (- c #x41)))
                         ((and (>= c #x61) (<= c #x7a))
                          (list-ref docx-symbol-smalls (- c #x61)))
                         ;; (the digits and the punctuation are the usual ones)
                         ((and (>= c #x20) (< c #x40)) c)
                         (else #xfffd)))
                  ((== font "wingdings")
                   (or (assoc-ref docx-wingdings-codes c) #xfffd))
                  ((or (string-starts? font "wingdings")
                       (in? font '("webdings" "mt extra" "zapfdingbats")))
                   #xfffd)
                  ((or (< c #x20) (and (>= c #xd800) (<= c #xdfff))
                       (> c #x10ffff))
                   #xfffd)
                  (else c))))
    (if u (list (office-utf8 u)) '())))

(define (docx-run-item x)
  ;; the nodes of a child of a run
  (cond ((not (pair? x)) '())
        ((func? x 'w:t) (list (ox-text x)))
        ((func? x 'w:tab) '((tab)))
        ((func? x 'w:br)
         (if (== (ox-attr x 'w:type) "page") '((pagebreak)) '((br))))
        ((func? x 'w:cr) '((br)))
        ((func? x 'w:noBreakHyphen) '("-"))
        ((func? x 'w:sym) (docx-symbol x))
        ((func? x 'w:drawing) (docx-drawing x))
        ((func? x 'w:pict) (docx-picture x))
        ((func? x 'w:object) (docx-picture x))
        ((func? x 'mc:AlternateContent)
         ;; the first of the choices and of the fallback which gives
         ;; something: a choice may be a drawing which is not read
         (let loop ((l (append (ox-childs x 'mc:Choice)
                               (ox-childs x 'mc:Fallback))))
           (if (null? l) '()
               (with r (append-map docx-run-item (ox-children (car l)))
                 (if (null? r) (loop (cdr l)) r)))))
        ((func? x 'w:footnoteReference) (docx-note 'foot (ox-attr x 'w:id)))
        ((func? x 'w:endnoteReference) (docx-note 'end (ox-attr x 'w:id)))
        (else '())))

(define (docx-run r)
  ;; the nodes of a run: its text inside its wrappers; what is not text
  ;; stays outside. A run of hidden text gives nothing.
  (with w (docx-run-wrappers r)
    (if (and (pair? w) (== (car w) 'hidden)) '()
        (append-map (lambda (x)
                      (if (string? x) (office-wrap (list x) w) (list x)))
                    (append-map docx-run-item (ox-children r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The text of a paragraph, with its links and its fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docx-field-words instr)
  ;; the words of the instruction of a field, each (text . quoted?): a
  ;; word in quotes may hold spaces, and \" and \\ for a quote and a \
  (let loop ((l (string->list instr)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((in? (car l) '(#\space #\tab #\newline #\return)) (loop (cdr l) acc))
          ((char=? (car l) #\")
           (let sub ((r (cdr l)) (w '()))
             (cond ((null? r)
                    (reverse (cons (cons (list->string (reverse w)) #t) acc)))
                   ((char=? (car r) #\")
                    (loop (cdr r) (cons (cons (list->string (reverse w)) #t) acc)))
                   ((and (char=? (car r) #\\) (pair? (cdr r))
                         (in? (cadr r) '(#\" #\\)))
                    (sub (cddr r) (cons (cadr r) w)))
                   (else (sub (cdr r) (cons (car r) w))))))
          (else
            (let sub ((r l) (w '()))
              (if (or (null? r) (in? (car r) '(#\space #\tab #\newline #\return)))
                  (loop r (cons (cons (list->string (reverse w)) #f) acc))
                  (sub (cdr r) (cons (car r) w))))))))

;; the switches of a field which are followed by a value
(define docx-field-switches
  '(("hyperlink" "\\l" "\\o" "\\t") ("ref" "\\d") ("seq" "\\r" "\\s")))

(define (docx-field-parse instr)
  ;; (name arguments switches) of the instruction of a field: its name in
  ;; small letters, its arguments, and its switches as (switch . value),
  ;; the value being #t for a switch without one
  (let* ((l (docx-field-words instr))
         (name (if (null? l) "" (locase-all (caar l))))
         (valued (append '("\\*" "\\#" "\\@")
                         (or (assoc-ref docx-field-switches name) '())))
         (switch? (lambda (w) (and (not (cdr w)) (string-starts? (car w) "\\")))))
    (let loop ((l (if (null? l) l (cdr l))) (args '()) (switches '()))
      (cond ((null? l) (list name (reverse args) (reverse switches)))
            ((switch? (car l))
             (if (and (in? (locase-all (caar l)) valued) (pair? (cdr l))
                      (not (switch? (cadr l))))
                 (loop (cddr l) args
                       (cons (cons (locase-all (caar l)) (caadr l)) switches))
                 (loop (cdr l) args
                       (cons (cons (locase-all (caar l)) #t) switches))))
            (else (loop (cdr l) (cons (caar l) args) switches))))))

(define (docx-field instr l)
  ;; the result l of a field: a link, a reference, or the result itself
  (let* ((f (docx-field-parse instr))
         (name (car f))
         (arg (if (null? (cadr f)) "" (caadr f))))
    (cond ((null? l) l)
          ((== name "hyperlink")
           ;; an address, a bookmark in it (the switch \l), or both
           (let* ((mark (assoc-ref (caddr f) "\\l"))
                  (href (string-append arg (if (string? mark)
                                               (string-append "#" mark) ""))))
             (if (== href "") l (list `(link (@ (href ,href)) ,@l)))))
          ((and (== name "ref") (!= arg ""))
           (list `(ref (@ (name ,arg)) ,@l)))
          ;; the page of a bookmark, not its number
          ((and (== name "pageref") (!= arg ""))
           (list `(ref (@ (name ,arg) (kind "page")) ,@l)))
          ;; a number of a sequence: of the figures, of the tables
          ((and (== name "seq") (!= arg ""))
           (list `(seq (@ (name ,arg)) ,@l)))
          (else l))))

(define (docx-unwrap l)
  ;; the children of a paragraph, with those of the elements which only
  ;; hold others in their place
  (append-map
    (lambda (x)
      (cond ((not (pair? x)) '())
            ((in? (car x) '(w:ins w:smartTag w:customXml w:sdtContent w:moveTo
                            w:dir w:bdo))
             (docx-unwrap (ox-children x)))
            ((func? x 'w:sdt)
             (with c (ox-child x 'w:sdtContent)
               (if c (docx-unwrap (ox-children c)) '())))
            (else (list x))))
    l))

(define (docx-inlines-sub l fields)
  ;; (nodes . fields) for the children l of a paragraph. A field is a run
  ;; which begins it, runs with its instruction, a run which separates, the
  ;; runs of its result and a run which ends it: fields is the stack of the
  ;; fields which are open, each (state instruction nodes-before). A field
  ;; may end in a later paragraph: fields are those which are open at the
  ;; start of this one, and the result has those which are open at its end.
  (let loop ((l (docx-unwrap l)) (acc '()) (fields fields))
    (if (null? l)
        ;; what there is here of the fields which go on is kept
        (let close ((acc acc) (open fields) (left '()))
          (if (null? open) (cons (reverse acc) (reverse left))
              (close (append (reverse (docx-field (cadar open)
                                                  (if (== (caar open) 'result)
                                                      (reverse acc) '())))
                             (caddar open))
                     (cdr open)
                     (cons (list (caar open) (cadar open) '()) left))))
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

(define (docx-inlines l)
  ;; the nodes of the children l of an element inside a paragraph
  (car (docx-inlines-sub l '())))

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
         (kind (and id (docx-list-kind id level)))
         ;; a level without a mark is no item
         (more? (and id (!= id "0") (not (car role)) (== kind "none")))
         (item? (and id (!= id "0") (not (car role)) (not more?)))
         (l (with r (docx-inlines-sub (ox-children x) docx-fields)
              (set! docx-fields (cdr r))
              (office-merge (car r)))))
    (append
      (if (docx-get own 'page-break) '((pagebreak)) '())
      (if (and (null? l) (not item?)) '()
          (list (apply office-node
                       (cons* 'p
                              `((role ,(car role)) (level ,(cadr role))
                                (align ,(cond ((== align "center") "center")
                                              ((in? align '("right" "end")) "right")
                                              (else #f)))
                                ;; a heading which says that it has no
                                ;; number, where the others have one
                                (numbered ,(and (== (car role) "heading")
                                                (== id "0") "no"))
                                (dropcap ,(and (docx-get own 'dropcap) "true"))
                                ;; a plain paragraph which is indented, as
                                ;; one more paragraph of an item is
                                (list-more ,(with ind (docx-get own 'indent)
                                              (cond (more?
                                                     (number->string
                                                       (+ (office-integer level 0 0 8) 1)))
                                                    ((and ind (>= ind 600) (not item?)
                                                          (not (car role)))
                                                     (number->string
                                                       (quotient (+ ind 120) 720)))
                                                    (else #f))))
                                (list-id ,(and item? (docx-list-group id)))
                                (list-level ,(and item? level))
                                (list-kind ,(and item? kind)))
                              l)))))))

;; The borders of a table are those of its style, of the table itself and
;; of each cell, in this order. A style has borders for the whole table
;; (around it, and inside between the rows and the columns) and others for
;; some of its parts: the first row, the last one, the first column, the
;; last one, the rows of odd and of even rank. Borders are association
;; lists side -> #t or #f, with the sides top, bottom, left, right,
;; insideH and insideV.

(define docx-border-sides
  '((w:top . top) (w:bottom . bottom) (w:left . left) (w:start . left)
    (w:right . right) (w:end . right) (w:insideH . insideH)
    (w:insideV . insideV)))

(define (docx-borders x)
  ;; the borders of an element w:tblBorders or w:tcBorders
  (if (not x) '()
      (append-map
        (lambda (y)
          (with side (assoc-ref docx-border-sides (car y))
            (if side
                (list (cons side (not (in? (docx-val y) '("nil" "none")))))
                '())))
        (ox-elements x))))

(define (docx-fill x)
  ;; the color of the background of an element w:tcPr, or #f
  (let* ((shd (and x (ox-child x 'w:shd)))
         (fill (and shd (ox-attr shd 'w:fill))))
    (and fill (== (string-length fill) 6) (!= (locase-all fill) "ffffff")
         (string-append "#" (locase-all fill)))))

(define (docx-table-style-properties x)
  ;; the properties of a style of tables
  (let ((pr (ox-child x 'w:tblPr))
        (tc (ox-child x 'w:tcPr)))
    (append
      (with b (and pr (ox-child pr 'w:tblBorders))
        (if b (list (cons 'borders (docx-borders b))) '()))
      (with f (docx-fill tc)
        (if f (list (cons 'fill f)) '()))
      ;; the parts of the table, each (borders fill)
      (map (lambda (part)
             (with ptc (ox-child part 'w:tcPr)
               (cons (string->symbol (string-append "part-" (or (ox-attr part 'w:type) "")))
                     (list (docx-borders (and ptc (ox-child ptc 'w:tcBorders)))
                           (docx-fill ptc)))))
           (ox-childs x 'w:tblStylePr)))))

;; the parts of a table which a row or a cell says it is in, by the rank of
;; their bit in w:cnfStyle
(define docx-parts
  '(part-firstRow part-lastRow part-firstCol part-lastCol part-band1Vert
    part-band2Vert part-band1Horz part-band2Horz))

(define (docx-cnf x)
  ;; the parts of the element w:cnfStyle x, or #f without it
  (let* ((v (and x (docx-val x))))
    (and v (>= (string-length v) 8)
         (list-filter
           (map (lambda (part i) (and (char=? (string-ref v i) #\1) part))
                docx-parts (iota 8))
           identity))))

(define (docx-look tblpr)
  ;; the parts which the table uses: (first-row? last-row? first-col?
  ;; last-col? bands?)
  (let* ((look (and tblpr (ox-child tblpr 'w:tblLook)))
         (on? (lambda (name) (in? (and look (ox-attr look name)) '("1" "true"))))
         (n (or (and look (docx-hexadecimal (docx-val look))) #x04a0)))
    (if (and look (ox-attr look 'w:firstRow))
        (list (on? 'w:firstRow) (on? 'w:lastRow) (on? 'w:firstColumn)
              (on? 'w:lastColumn) (not (on? 'w:noHBand)))
        (list (odd? (quotient n #x20)) (odd? (quotient n #x40))
              (odd? (quotient n #x80)) (odd? (quotient n #x100))
              (not (odd? (quotient n #x200)))))))

(define (docx-side borders side)
  ;; (#t) or (#f) when the borders say something of this side, else ()
  (with p (assoc side borders)
    (if p (list (cdr p)) '())))

(define (docx-cell-format i j rows cols parts style tbl-borders own-pr)
  ;; (borders fill) of the cell at row i and column j of a table of rows
  ;; and cols: the borders as a string of the letters t, b, l and r
  (let* ((first-row? (== i 0))
         (last-row? (== i (- rows 1)))
         (first-col? (== j 0))
         (last-col? (== j (- cols 1)))
         (part-info (lambda (part) (or (docx-get style part) (list '() #f))))
         ;; what each source says of the four sides, the last one first
         (own (docx-borders (and own-pr (ox-child own-pr 'w:tcBorders))))
         (from-parts
           (lambda (side)
             (append-map
               (lambda (part)
                 (let* ((b (car (part-info part)))
                        (row-part? (in? part '(part-firstRow part-lastRow
                                               part-band1Horz part-band2Horz))))
                   (cond ((== side 'top)
                          (if (or row-part? first-row?) (docx-side b 'top)
                              (docx-side b 'insideH)))
                         ((== side 'bottom)
                          (if (or row-part? last-row?) (docx-side b 'bottom)
                              (docx-side b 'insideH)))
                         ((== side 'left)
                          (if (or (not row-part?) first-col?) (docx-side b 'left)
                              (docx-side b 'insideV)))
                         (else
                          (if (or (not row-part?) last-col?) (docx-side b 'right)
                              (docx-side b 'insideV))))))
               (reverse parts))))
         (from-table
           (lambda (side)
             (cond ((== side 'top)
                    (docx-side tbl-borders (if first-row? 'top 'insideH)))
                   ((== side 'bottom)
                    (docx-side tbl-borders (if last-row? 'bottom 'insideH)))
                   ((== side 'left)
                    (docx-side tbl-borders (if first-col? 'left 'insideV)))
                   (else
                    (docx-side tbl-borders (if last-col? 'right 'insideV))))))
         (on? (lambda (side)
                (with l (append (docx-side own side) (from-parts side)
                                (from-table side))
                  (and (pair? l) (car l)))))
         (letters (string-append (if (on? 'top) "t" "") (if (on? 'bottom) "b" "")
                                 (if (on? 'left) "l" "") (if (on? 'right) "r" "")))
         (fill (or (docx-fill own-pr)
                   (list-find (map (lambda (part) (cadr (part-info part)))
                                   (reverse parts))
                              identity)
                   (docx-get style 'fill))))
    (list (if (== letters "") "none" letters) fill)))

(define (docx-row-parts i rows row look)
  ;; the parts of the table which the row i is in
  (or (docx-cnf (with pr (ox-child row 'w:trPr) (and pr (ox-child pr 'w:cnfStyle))))
      (let ((first? (and (car look) (== i 0)))
            (last? (and (cadr look) (== i (- rows 1)) (> rows 1))))
        (cond (first? '(part-firstRow))
              (last? '(part-lastRow))
              ((not (list-ref look 4)) '())
              ;; the bands start after the first row
              ((odd? (- i (if (car look) 1 0))) '(part-band2Horz))
              (else '(part-band1Horz))))))

(define (docx-cell-parts j cols cell row-parts look)
  ;; the parts of the table which a cell of the column j is in
  (or (docx-cnf (with pr (ox-child cell 'w:tcPr) (and pr (ox-child pr 'w:cnfStyle))))
      (append row-parts
              (if (and (caddr look) (== j 0)) '(part-firstCol) '())
              (if (and (cadddr look) (== j (- cols 1)) (> cols 1))
                  '(part-lastCol) '()))))

(define (docx-cell x header? format)
  ;; the cells of a w:tc: the cell, and those which it covers on its right
  (let* ((pr (ox-child x 'w:tcPr))
         (span (docx-cell-span x))
         (merge (and pr (ox-child pr 'w:vMerge)))
         (covered `(cell (@ (covered "true")))))
    ;; (a cell which goes on a vertical merge keeps its text until
    ;; docx-row-spans knows that there is a cell above it)
    (cons (apply office-node
                 (cons* 'cell
                        `((header ,(and header? "true"))
                          (colspan ,(and (> span 1) (number->string span)))
                          (vmerge ,(and merge (if (== (docx-val merge) "restart")
                                                  "start" "continue")))
                          (borders ,(car format))
                          (background ,(cadr format)))
                        (docx-blocks (ox-children x))))
          (map (lambda (i) covered) (iota (- span 1))))))

(define (docx-cell-span x)
  ;; the number of columns of the cell, at most those of a table of Word
  (let* ((pr (ox-child x 'w:tcPr)))
    (office-integer (and pr (docx-child-val pr 'w:gridSpan)) 1 1 63)))

(define (docx-grid-before x)
  ;; the number of columns which the row x leaves empty at its start
  (let* ((pr (ox-child x 'w:trPr)))
    (office-integer (and pr (docx-child-val pr 'w:gridBefore)) 0 0 63)))

(define (docx-table-children x tag)
  ;; the children tag of a table or of a row, also those which are inside
  ;; a content control or a mark of revision
  (append-map
    (lambda (y)
      (cond ((func? y tag) (list y))
            ((and (pair? y) (in? (car y) '(w:sdt w:sdtContent w:customXml w:ins
                                           w:moveTo)))
             (docx-table-children y tag))
            (else '())))
    (ox-children x)))

(define (docx-deleted-row? x)
  ;; a row which was deleted, with the changes tracked
  (with pr (ox-child x 'w:trPr)
    (and pr (or (ox-child pr 'w:del) (ox-child pr 'w:moveFrom)) #t)))

(define (docx-row x i rows cols look style tbl-borders)
  (let* ((pr (ox-child x 'w:trPr))
         (header? (and pr (ox-child pr 'w:tblHeader)
                       (not (in? (docx-child-val pr 'w:tblHeader)
                                 '("0" "false")))))
         (row-parts (docx-row-parts i rows x look)))
    (cons 'row
          (let loop ((l (docx-table-children x 'w:tc))
                     (j (docx-grid-before x))
                     (acc (map (lambda (k) '(cell (@ (borders "none"))))
                               (iota (docx-grid-before x)))))
            (if (null? l) (reverse acc)
                (let* ((c (car l))
                       (span (docx-cell-span c))
                       ;; a wide cell has the right side of its last column
                       (jr (+ j span -1))
                       (parts (docx-cell-parts j cols c row-parts look))
                       (f (docx-cell-format i j rows cols parts style tbl-borders
                                            (ox-child c 'w:tcPr)))
                       (fr (if (== span 1) f
                               (docx-cell-format i jr rows cols parts style
                                                 tbl-borders (ox-child c 'w:tcPr))))
                       (letters (list->string
                                  (append
                                    (list-filter (string->list (car f))
                                                 (lambda (ch) (in? ch '(#\t #\b #\l))))
                                    (list-filter (string->list (car fr))
                                                 (lambda (ch) (char=? ch #\r))))))
                       (format (list (if (or (== (car f) "none") (== letters ""))
                                         (if (== letters "") "none" letters)
                                         letters)
                                     (cadr f))))
                  (loop (cdr l) (+ j span)
                        (append (reverse (docx-cell c header? format)) acc))))))))

(define (docx-row-spans rows)
  ;; a cell which starts a vertical merge gets the number of rows it
  ;; covers: the cells below it in its column which go on the merge, and
  ;; which are covered. A cell which goes on a merge that no cell above it
  ;; has started is the start of one.
  (with grid (map cdr rows)
    (let loop ((rest grid) (open '()) (acc '()))
      ;; open: the columns of the row before whose cells are in a merge
      (if (null? rest) (reverse acc)
          (loop (cdr rest)
                (list-filter (iota (length (car rest)))
                             (lambda (col)
                               (ox-attr (list-ref (car rest) col) 'vmerge)))
                (cons
                  (cons 'row
                        (map (lambda (cell col)
                               (cond ((not (ox-attr cell 'vmerge)) cell)
                                     ((and (== (ox-attr cell 'vmerge) "continue")
                                           (in? col open))
                                      `(cell (@ (covered "true"))))
                                     (else
                                       (let count ((below (cdr rest)) (n 1))
                                         (if (and (pair? below)
                                                  (< col (length (car below)))
                                                  (== (ox-attr (list-ref (car below) col)
                                                               'vmerge)
                                                      "continue"))
                                             (count (cdr below) (+ n 1))
                                             (apply office-node
                                                    (cons* 'cell
                                                           `((header ,(ox-attr cell 'header))
                                                             (colspan ,(ox-attr cell 'colspan))
                                                             (rowspan ,(and (> n 1) (number->string n)))
                                                             (borders ,(ox-attr cell 'borders))
                                                             (background ,(ox-attr cell 'background)))
                                                           (ox-children cell))))))))
                             (car rest) (iota (length (car rest)))))
                  acc))))))

(define (docx-table-columns x)
  ;; the widths of the columns of the grid, as parts of their sum
  (let* ((grid (ox-child x 'w:tblGrid))
         (l (map (lambda (c) (office-integer (ox-attr c 'w:w) 0 0 1000000))
                 (if grid (ox-childs grid 'w:gridCol) '())))
         (sum (apply + l)))
    (and (pair? l) (> sum 0)
         (string-recompose
           (map (lambda (w) (number->string (/ (round (* 1000.0 (/ w sum))) 1000.0)))
                l)
           " "))))

(define (docx-table-width pr)
  ;; the width of the table: a part of the width of the text, or #f
  (let* ((w (and pr (ox-child pr 'w:tblW)))
         (type (and w (ox-attr w 'w:type)))
         (v (and w (ox-attr w 'w:w)))
         (v (and v (if (string-ends? v "%")
                       (with n (office-number (substring v 0 (- (string-length v) 1)))
                         (and n (* n 50)))
                       (office-number v)))))
    (and v (== type "pct") (> v 0)
         (string-append (number->string (/ (round (/ v 5.0)) 1000.0)) "par"))))

(define (docx-table x)
  (let* ((pr (ox-child x 'w:tblPr))
         (style (docx-style (and pr (docx-child-val pr 'w:tblStyle))))
         (tbl-borders (append (docx-borders (and pr (ox-child pr 'w:tblBorders)))
                              (or (docx-get style 'borders) '())))
         (look (docx-look pr))
         (trs (list-filter (docx-table-children x 'w:tr)
                           (lambda (r) (not (docx-deleted-row? r)))))
         (nrows (length trs))
         (ncols (apply max (cons 1 (map (lambda (r)
                                          (apply + (cons (docx-grid-before r)
                                                         (map docx-cell-span
                                                              (docx-table-children r 'w:tc)))))
                                        trs))))
         (rows (map (lambda (r i)
                      (docx-row r i nrows ncols look style tbl-borders))
                    trs (iota nrows)))
         (align (and pr (docx-child-val pr 'w:jc))))
    (if (null? rows) '()
        (list (apply office-node
                     (cons* 'table
                            `((align ,(cond ((== align "center") "center")
                                            ((in? align '("right" "end")) "right")
                                            (else #f)))
                              (width ,(docx-table-width pr))
                              (columns ,(docx-table-columns x)))
                            (docx-row-spans rows)))))))

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
                                                             list-kind list-more)))))
                (ox-children p))))

(define (docx-item-level x)
  (and (func? x 'p) (ox-attr x 'list-id)
       (office-integer (ox-attr x 'list-level) 0 0 8)))

(define (docx-list l level)
  ;; (list . rest): the list of the items at the start of l which are at
  ;; this level or deeper, the deeper ones as lists inside the items
  (let* ((id (ox-attr (car l) 'list-id))
         (kind (ox-attr (car l) 'list-kind)))
    (let loop ((l l) (items '()))
      (with n (and (pair? l) (docx-item-level (car l)))
        (cond ((and (pair? l) (pair? items) (func? (car l) 'p)
                    (ox-attr (car l) 'list-more)
                    (> (office-integer (ox-attr (car l) 'list-more) 0 0 100) level))
               ;; one more paragraph of the last item
               (loop (cdr l)
                     (cons (append (car items)
                                   (list (docx-plain-paragraph (car l))))
                           (cdr items))))
              ((or (not n) (< n level)
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
  ;; (loops, here and below: a document may have many thousands of blocks)
  (let loop ((l l) (acc '()))
    (cond ((null? l) (reverse acc))
          ((docx-item-level (car l))
           (with r (docx-list l (docx-item-level (car l)))
             (loop (cdr r) (cons (car r) acc))))
          ((and (func? (car l) 'p) (ox-attr (car l) 'list-more))
           (loop (cdr l) (cons (docx-plain-paragraph (car l)) acc)))
          (else (loop (cdr l) (cons (car l) acc))))))

(define (docx-drop-caps l)
  ;; a large first letter is a paragraph of its own: it is the start of
  ;; the next one
  (let loop ((l l) (acc '()))
    (cond ((or (null? l) (null? (cdr l))) (append (reverse acc) l))
          ((and (func? (car l) 'p) (ox-attr (car l) 'dropcap)
                (func? (cadr l) 'p))
           (loop (cons (apply office-node
                              (cons* 'p (ox-attrs (cadr l))
                                     (office-merge
                                       (append (ox-children (car l))
                                               (ox-children (cadr l))))))
                       (cddr l))
                 acc))
          (else (loop (cdr l) (cons (car l) acc))))))

(define (docx-blocks l)
  (docx-group-lists (docx-drop-caps (docx-blocks-sub l))))

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

;; A file which cannot be read gives a document which says so, and a
;; message, rather than an empty document.
(define (docx-error message)
  (debug-message "convert-error" (string-append message "\n"))
  `(office (error ,message)))

(tm-define (parse-docx-document s)
  (:type (-> string stree))
  (:synopsis "Read the Word document @s, a zip archive, into an office tree")
  ;; (what was not foreseen in a file gives a message too, and the state
  ;; of the module is reset)
  (with r (catch #t
            (lambda () (docx-parse s))
            (lambda args
              (docx-error (string-append "This Word document could not be read "
                                         (office-error-text args)))))
    (set! docx-archive #f)
    (set! docx-part "word/document.xml")
    (set! docx-rels (make-ahash-table))
    (set! docx-lists (make-ahash-table))
    (set! docx-styles (make-ahash-table))
    (set! docx-style-memo (make-ahash-table))
    (set! docx-notes (make-ahash-table))
    (set! docx-open-notes '())
    (set! docx-fields '())
    r))

(define (docx-parse s)
  (set! docx-archive (office-archive s))
  (if (not docx-archive) (docx-error "This file is not a Word document")
      (begin
        (set! docx-part (docx-main-part))
        (set! docx-rels (make-ahash-table))
        (set! docx-notes (make-ahash-table))
        (set! docx-open-notes '())
        (set! docx-fields '())
        (docx-read-styles)
        (docx-read-lists)
        (for (part (list docx-part "word/footnotes.xml" "word/endnotes.xml"))
          (docx-read-rels part))
        (docx-read-notes "word/footnotes.xml" 'w:footnote 'foot)
        (docx-read-notes "word/endnotes.xml" 'w:endnote 'end)
        (let* ((root (office-xml docx-archive docx-part))
               (body (and root (ox-child root 'w:body)))
               (r (if body
                      `(office ,@(docx-meta) ,@(docx-blocks (ox-children body)))
                      (docx-error "This Word document has no text"))))
          r))))
