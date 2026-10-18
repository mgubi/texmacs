;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-html.scm
;; DESCRIPTION : the external scanner of tree-sitter-html
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c and src/tag.h of tree-sitter-html (MIT): the
;; names of the tags, kept on a stack to close the elements whose end tag is
;; implicit, the raw text of script and style, and the comments. As in C,
;; the names are upper-cased and stored as the chars of C. The protocol of
;; the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-html)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars) (treesit ts-scan)))

(define t-start-tag-name 0)
(define t-script-start-tag-name 1)
(define t-style-start-tag-name 2)
(define t-end-tag-name 3)
(define t-erroneous-end-tag-name 4)
(define t-self-closing-tag-delimiter 5)
(define t-implicit-end-tag 6)
(define t-raw-text 7)
(define t-comment 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags (tag.h)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the types of the tags, in the order of the enum of C (the void tags first)
(define tag-types
  (vector
    "AREA" "BASE" "BASEFONT" "BGSOUND" "BR" "COL" "COMMAND" "EMBED" "FRAME"
    "HR" "IMAGE" "IMG" "INPUT" "ISINDEX" "KEYGEN" "LINK" "MENUITEM" "META"
    "NEXTID" "PARAM" "SOURCE" "TRACK" "WBR" "END_OF_VOID_TAGS" "A" "ABBR"
    "ADDRESS" "ARTICLE" "ASIDE" "AUDIO" "B" "BDI" "BDO" "BLOCKQUOTE" "BODY"
    "BUTTON" "CANVAS" "CAPTION" "CITE" "CODE" "COLGROUP" "DATA" "DATALIST"
    "DD" "DEL" "DETAILS" "DFN" "DIALOG" "DIV" "DL" "DT" "EM" "FIELDSET"
    "FIGCAPTION" "FIGURE" "FOOTER" "FORM" "H1" "H2" "H3" "H4" "H5" "H6"
    "HEAD" "HEADER" "HGROUP" "HTML" "I" "IFRAME" "INS" "KBD" "LABEL" "LEGEND"
    "LI" "MAIN" "MAP" "MARK" "MATH" "MENU" "METER" "NAV" "NOSCRIPT" "OBJECT"
    "OL" "OPTGROUP" "OPTION" "OUTPUT" "P" "PICTURE" "PRE" "PROGRESS" "Q" "RB"
    "RP" "RT" "RTC" "RUBY" "S" "SAMP" "SCRIPT" "SECTION" "SELECT" "SLOT"
    "SMALL" "SPAN" "STRONG" "STYLE" "SUB" "SUMMARY" "SUP" "SVG" "TABLE"
    "TBODY" "TD" "TEMPLATE" "TEXTAREA" "TFOOT" "TH" "THEAD" "TIME" "TITLE"
    "TR" "U" "UL" "VAR" "VIDEO" "CUSTOM" "END_"))

(define (type-of name)
  (let loop ((i 0))
    (cond ((= i (vector-length tag-types)) -1)
          ((== (vector-ref tag-types i) name) i)
          (else (loop (+ i 1))))))

(define end-of-void-tags (type-of "END_OF_VOID_TAGS"))
(define custom (type-of "CUSTOM"))
(define end-type (type-of "END_"))
(define (type name) (type-of name))

;; a tag is (type . name), the name being "" except for the custom tags
(define (tag-type t) (car t))
(define (tag-new) (cons end-type ""))

(define (tag-for-name name)
  ;; the names of the map of C: the types but END_OF_VOID_TAGS and END_
  (let ((i (type-of name)))
    (if (or (< i 0) (= i end-of-void-tags) (= i end-type) (= i custom))
        (cons custom name)
        (cons i ""))))

(define (tag-void? t) (< (tag-type t) end-of-void-tags))

(define (tag-eq? a b)
  (and (= (tag-type a) (tag-type b))
       (or (not (= (tag-type a) custom)) (== (cdr a) (cdr b)))))

(define not-in-paragraphs
  (map type '("ADDRESS" "ARTICLE" "ASIDE" "BLOCKQUOTE" "DETAILS" "DIV" "DL" "FIELDSET" "FIGCAPTION" "FIGURE" "FOOTER" "FORM" "H1" "H2" "H3" "H4" "H5" "H6" "HEADER" "HR" "MAIN" "NAV" "OL" "P" "PRE" "SECTION")))

(define (tag-can-contain? self other)
  (let ((child (tag-type other)) (s (tag-type self)))
    (cond ((= s (type "LI")) (not (= child (type "LI"))))
          ((or (= s (type "DT")) (= s (type "DD")))
           (not (or (= child (type "DT")) (= child (type "DD")))))
          ((= s (type "P")) (not (memv child not-in-paragraphs)))
          ((= s (type "COLGROUP")) (= child (type "COL")))
          ((memv s (list (type "RB") (type "RT") (type "RP")))
           (not (memv child (list (type "RB") (type "RT") (type "RP")))))
          ((= s (type "OPTGROUP")) (not (= child (type "OPTGROUP"))))
          ((= s (type "TR")) (not (= child (type "TR"))))
          ((or (= s (type "TD")) (= s (type "TH")))
           (not (memv child (list (type "TD") (type "TH") (type "TR")))))
          (else #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the payload is #(tags), the stack of the open tags, the innermost first
(define (tags p) (vector-ref p 0))
(define (push-tag! p t) (vector-set! p 0 (cons t (tags p))))
(define (pop-tag! p) (vector-set! p 0 (cdr (tags p))))

(define (html-serialize p)
  ;; as many tags as the buffer of C holds (4 bytes, 1 per tag, 2 + the
  ;; name for a custom one, at most 255 characters); the others become
  ;; empty tags when the state is restored
  (let ((all (reverse (tags p))))
    (and (pair? all)
         (let loop ((l all) (size 4) (acc '()))
           (let* ((t (and (pair? l) (car l)))
                  (name (and t (= (tag-type t) custom)
                             (let ((n (cdr t)))
                               (if (> (string-length n) 255) (substring n 0 255) n))))
                  (need (cond ((not t) 0) (name (+ 2 (string-length name))) (else 1))))
             (if (or (not t) (>= (+ size need) 1024))
                 (list (length all) (reverse acc))
                 (loop (cdr l) (+ size need)
                       (cons (if name (cons custom name) t) acc))))))))

(define (html-deserialize p state)
  (vector-set! p 0 '())
  (when state
    (let ((count (car state)) (l (cadr state)))
      (for-each (lambda (t) (push-tag! p t)) l)
      (let pad ((k (length l)))
        (when (< k count) (push-tag! p (tag-new)) (pad (+ k 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (upper c)
  ;; towupper, for ASCII
  (if (and (>= c 97) (<= c 122)) (- c 32) c))

(define (scan-tag-name lx)
  ;; the name, upper-cased, as the chars of C
  (let loop ((acc '()))
    (let ((c (lexer-lookahead lx)))
      (if (or (ts-walnum? c) (= c 45) (= c 58))
          (begin
            (lexer-advance! lx #f)
            (loop (cons (integer->char (modulo (upper c) 256)) acc)))
          (list->string (reverse acc))))))

(define (scan-comment lx)
  (and (= (lexer-lookahead lx) 45)
       (begin (lexer-advance! lx #f) (= (lexer-lookahead lx) 45))
       (begin
         (lexer-advance! lx #f)
         (let loop ((dashes 0))
           (let ((c (lexer-lookahead lx)))
             (cond ((= c 0) #f)
                   ((and (= c 62) (>= dashes 2))
                    (lexer-set-result! lx t-comment)
                    (lexer-advance! lx #f)
                    (lexer-mark-end! lx)
                    #t)
                   (else
                    (lexer-advance! lx #f)
                    (loop (if (= c 45) (+ dashes 1) 0)))))))))

(define (scan-raw-text p lx)
  (and (pair? (tags p))
       (let* ((delim (if (= (tag-type (car (tags p))) (type "SCRIPT")) "</SCRIPT" "</STYLE"))
              (n (string-length delim)))
         (lexer-mark-end! lx)
         (let loop ((i 0))
           (let ((c (lexer-lookahead lx)))
             (unless (= c 0)
               (if (= (upper c) (char->integer (string-ref delim i)))
                   (unless (= (+ i 1) n)
                     (lexer-advance! lx #f)
                     (loop (+ i 1)))
                   (begin
                     (lexer-advance! lx #f)
                     (lexer-mark-end! lx)
                     (loop 0))))))
         (lexer-set-result! lx t-raw-text)
         #t)))

(define (scan-implicit-end-tag p lx)
  (let* ((parent (and (pair? (tags p)) (car (tags p))))
         (closing (= (lexer-lookahead lx) 47)))
    (call-with-exit
     (lambda (out)
       (if closing
           (lexer-advance! lx #f)
           (when (and parent (tag-void? parent))
             (pop-tag! p)
             (lexer-set-result! lx t-implicit-end-tag)
             (out #t)))
       (let ((name (scan-tag-name lx)))
         (when (and (= (string-length name) 0) (not (lexer-eof? lx))) (out #f))
         (let ((next (tag-for-name name)))
           (cond (closing
                  ;; the tag closes the topmost element
                  (when (and (pair? (tags p)) (tag-eq? (car (tags p)) next)) (out #f))
                  ;; else implicit end tags, for malformed HTML
                  (when (list-find (tags p) (lambda (t) (= (tag-type t) (tag-type next))))
                    (pop-tag! p)
                    (lexer-set-result! lx t-implicit-end-tag)
                    (out #t)))
                 ((and parent
                       (or (not (tag-can-contain? parent next))
                           (and (memv (tag-type parent)
                                      (list (type "HTML") (type "HEAD") (type "BODY")))
                                (lexer-eof? lx))))
                  (pop-tag! p)
                  (lexer-set-result! lx t-implicit-end-tag)
                  (out #t)))
           #f))))))

(define (scan-start-tag-name p lx)
  (let ((name (scan-tag-name lx)))
    (and (> (string-length name) 0)
         (let ((t (tag-for-name name)))
           (push-tag! p t)
           (lexer-set-result! lx (cond ((= (tag-type t) (type "SCRIPT")) t-script-start-tag-name)
                                       ((= (tag-type t) (type "STYLE")) t-style-start-tag-name)
                                       (else t-start-tag-name)))
           #t))))

(define (scan-end-tag-name p lx)
  (let ((name (scan-tag-name lx)))
    (and (> (string-length name) 0)
         (let ((t (tag-for-name name)))
           (if (and (pair? (tags p)) (tag-eq? (car (tags p)) t))
               (begin (pop-tag! p) (lexer-set-result! lx t-end-tag-name))
               (lexer-set-result! lx t-erroneous-end-tag-name))
           #t))))

(define (scan-self-closing-tag-delimiter p lx)
  (lexer-advance! lx #f)
  (and (= (lexer-lookahead lx) 62)
       (begin
         (lexer-advance! lx #f)
         (when (pair? (tags p))
           (pop-tag! p)
           (lexer-set-result! lx t-self-closing-tag-delimiter))
         #t)))

(define (html-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (if (and (valid? t-raw-text) (not (valid? t-start-tag-name)) (not (valid? t-end-tag-name)))
      (scan-raw-text p lx)
      (begin
        (scan-skip-spaces! lx #t #t)
        (let ((c (lexer-lookahead lx)))
          (cond ((= c 60)   ; <
                 (lexer-mark-end! lx)
                 (lexer-advance! lx #f)
                 (cond ((= (lexer-lookahead lx) 33)   ; !
                        (lexer-advance! lx #f)
                        (scan-comment lx))
                       ((valid? t-implicit-end-tag) (scan-implicit-end-tag p lx))
                       (else #f)))
                ((= c 0)
                 (and (valid? t-implicit-end-tag) (scan-implicit-end-tag p lx)))
                ((= c 47)   ; /
                 (and (valid? t-self-closing-tag-delimiter)
                      (scan-self-closing-tag-delimiter p lx)))
                ((and (or (valid? t-start-tag-name) (valid? t-end-tag-name))
                      (not (valid? t-raw-text)))
                 (if (valid? t-start-tag-name)
                     (scan-start-tag-name p lx)
                     (scan-end-tag-name p lx)))
                (else #f))))))

(define-public (ts-html-scanner)
  (vector (lambda () (vector '())) html-scan html-deserialize html-serialize))
