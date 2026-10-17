;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-lean.scm
;; DESCRIPTION : the external scanner of tree-sitter-lean
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of Julian/tree-sitter-lean (MIT): dotted, quoted
;; and Unicode identifiers, nested block and doc comments, raw strings, the
;; norm bars, and the layout tokens (indent, dedent, newline) of tactic and
;; do blocks. The protocol of the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-lean)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-scan)))

(define t-scanned-ident 0)
(define t-block-comment 1)
(define t-doc-comment 2)
(define t-module-doc-comment 3)
(define t-raw-string-literal 4)
(define t-indent 5)
(define t-dedent 6)
(define t-newline 7)
(define t-norm-open 8)
(define t-norm-close 9)
(define t-error-sentinel 10)

(define norm-bar #x2016)        ; ‖
(define subscript-plus #x208a)  ; ₊
(define id-begin-escape #xab)   ; «
(define id-end-escape #xbb)     ; »

;; the payload is #(indents), the stack of the columns of the open blocks,
;; the innermost first, never empty

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters, as src/Init/Meta/Defs.lean of Lean classifies them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alpha-ascii? c)
  (or (and (>= c 97) (<= c 122)) (and (>= c 65) (<= c 90))))

(define (digit-ascii? c) (and (>= c 48) (<= c 57)))

(define (between? c a b) (and (>= c a) (<= c b)))

(define (letter-like? c)
  (or (and (between? c #x3b1 #x3c9) (not (= c #x3bb)))
      (and (between? c #x391 #x3a9) (not (= c #x3a0)) (not (= c #x3a3)))
      (between? c #x3ca #x3fb)
      (between? c #x1f00 #x1ffe)
      (between? c #x2100 #x214f)
      (between? c #x1d400 #x1d7ff)
      (and (between? c #xc0 #xff) (not (= c #xd7)) (not (= c #xf7)))
      (between? c #x100 #x17f)))

(define (subscript-alnum? c)
  (or (between? c #x2080 #x2089) (between? c #x2090 #x209c) (between? c #x1d62 #x1d6a)
      (= c #x2c7c)))

(define (id-first? c) (or (alpha-ascii? c) (= c 95) (letter-like? c)))

(define (id-rest? c)
  (or (alpha-ascii? c) (digit-ascii? c) (= c 95) (= c 39) (= c 33) (= c 63)
      (letter-like? c) (subscript-alnum? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scan-ident-part lx)
  ;; «...» or id-first id-rest*
  (let ((c (lexer-lookahead lx)))
    (cond ((= c id-begin-escape)
           (lexer-advance! lx #f)
           (let loop ()
             (let ((c (lexer-lookahead lx)))
               (when (and (not (= c 0)) (not (= c id-end-escape)) (not (= c 10)))
                 (lexer-advance! lx #f)
                 (loop))))
           (and (= (lexer-lookahead lx) id-end-escape)
                (begin (lexer-advance! lx #f) #t)))
          ((not (id-first? c)) #f)
          (else
           (lexer-advance! lx #f)
           (let loop ()
             (when (id-rest? (lexer-lookahead lx))
               (lexer-advance! lx #f)
               (loop)))
           #t))))

(define (scan-dotted-parts! lx on-part)
  ;; the parts .x, .«x» which follow
  (let loop ()
    (when (= (lexer-lookahead lx) 46)
      (lexer-advance! lx #f)
      (let ((next (lexer-lookahead lx)))
        (when (and (or (= next id-begin-escape) (id-first? next))
                   (begin (on-part) (scan-ident-part lx)))
          (lexer-mark-end! lx)
          (loop))))))

(define (scan-identifier-continue lx seen-unusual)
  ;; after the first character; #f for a plain ASCII identifier of one
  ;; part, which the lexer of tree-sitter handles (keywords)
  (let loop ()
    (let ((c (lexer-lookahead lx)))
      (when (id-rest? c)
        (when (>= c #x80) (set! seen-unusual #t))
        (lexer-advance! lx #f)
        (loop))))
  (lexer-mark-end! lx)
  (scan-dotted-parts! lx (lambda () (set! seen-unusual #t)))
  (and seen-unusual
       (begin (lexer-set-result! lx t-scanned-ident) #t)))

(define (scan-identifier lx)
  (let ((c (lexer-lookahead lx)))
    (cond ((= c id-begin-escape)
           (and (scan-ident-part lx)
                (begin
                  (lexer-mark-end! lx)
                  (scan-dotted-parts! lx (lambda () #t))
                  (lexer-set-result! lx t-scanned-ident)
                  #t)))
          ((not (id-first? c)) #f)
          (else
           (lexer-advance! lx #f)
           (scan-identifier-continue lx (>= c #x80))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments and raw strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scan-block-comment-body lx)
  ;; after /-, up to the matching -/, nested
  (let loop ((depth 1))
    (cond ((= depth 0) #t)
          ((lexer-eof? lx) #f)
          ((scan-char? lx #\/)
           (lexer-advance! lx #f)
           (if (scan-char? lx #\-)
               (begin (lexer-advance! lx #f) (loop (+ depth 1)))
               (loop depth)))
          ((scan-char? lx #\-)
           (lexer-advance! lx #f)
           (if (scan-char? lx #\/)
               (begin (lexer-advance! lx #f) (loop (- depth 1)))
               (loop depth)))
          (else
           (scan-advance-until! lx "/-" #f)
           (when (and (= (lexer-lookahead lx) 0) (not (lexer-eof? lx)))
             (lexer-advance! lx #f))
           (loop depth)))))

(define (scan-block-or-doc-comment lx valid?)
  (and (scan-char? lx #\/)
       (begin (lexer-advance! lx #f) (scan-char? lx #\-))
       (begin
         (lexer-advance! lx #f)
         (let ((kind (cond ((and (scan-char? lx #\-) (valid? t-doc-comment))
                            (lexer-advance! lx #f)
                            t-doc-comment)
                           ((and (scan-char? lx #\!) (valid? t-module-doc-comment))
                            (lexer-advance! lx #f)
                            t-module-doc-comment)
                           ((valid? t-block-comment) t-block-comment)
                           (else #f))))
           (and kind
                (scan-block-comment-body lx)
                (begin (lexer-set-result! lx kind) #t))))))

(define (scan-raw-string-after-r lx)
  (let ((hashes (scan-advance-while! lx "#" #f)))
    (and (scan-char? lx #\")
         (begin
           (lexer-advance! lx #f)
           (let loop ()
             (cond ((lexer-eof? lx) #f)
                   ((scan-char? lx #\")
                    (lexer-advance! lx #f)
                    (let hash ((seen 0))
                      (if (and (< seen hashes) (scan-char? lx #\#))
                          (begin (lexer-advance! lx #f) (hash (+ seen 1)))
                          (if (= seen hashes)
                              (begin (lexer-set-result! lx t-raw-string-literal) #t)
                              (loop)))))
                   (else
                    (scan-advance-until! lx "\"" #f)
                    (when (and (= (lexer-lookahead lx) 0) (not (lexer-eof? lx)))
                      (lexer-advance! lx #f))
                    (loop))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scan-layout p lx valid?)
  ;; at a newline: indent, dedent (one level) or newline, after the blank
  ;; lines, by the indentation of the next line
  (and (or (valid? t-indent) (valid? t-dedent) (valid? t-newline))
       (begin (scan-advance-while! lx " \t\r" #t)
              (or (scan-char? lx #\newline) (lexer-eof? lx)))
       (call-with-exit
        (lambda (out)
          (let ((indent 0) (saw-newline #f))
            (let loop ()
              (cond ((scan-char? lx #\newline)
                     (set! indent 0)
                     (set! saw-newline #t)
                     (lexer-advance! lx #t)
                     (loop))
                    ((scan-char? lx #\space)
                     (set! indent (+ indent (scan-advance-while! lx " " #t)))
                     (loop))
                    ((scan-char? lx #\tab)
                     (set! indent (+ indent 8))
                     (lexer-advance! lx #t)
                     (loop))
                    ((scan-char? lx #\return)
                     (lexer-advance! lx #t)
                     (loop))
                    ((lexer-eof? lx)
                     ;; the end closes every open block
                     (when (and (valid? t-dedent) (pair? (cdr (vector-ref p 0))))
                       (vector-set! p 0 (cdr (vector-ref p 0)))
                       (lexer-set-result! lx t-dedent)
                       (out #t))
                     (out #f))))
            (unless saw-newline (out #f))
            (let ((top (car (vector-ref p 0))))
              ;; | always continues the surrounding rule
              (when (scan-char? lx #\|) (out #f))
              (cond ((> indent top)
                     ;; else the continuation of a multi-line expression
                     (and (valid? t-indent)
                          (begin
                            (vector-set! p 0 (cons (modulo indent 65536)
                                                   (vector-ref p 0)))
                            (lexer-set-result! lx t-indent)
                            #t)))
                    ((and (< indent top) (valid? t-dedent))
                     (vector-set! p 0 (cdr (vector-ref p 0)))
                     (lexer-set-result! lx t-dedent)
                     #t)
                    ((and (= indent top) (valid? t-newline))
                     (lexer-set-result! lx t-newline)
                     #t)
                    (else #f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lean-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (call-with-exit
   (lambda (out)
     (when (valid? t-error-sentinel) (out #f))
     (when (and (or (valid? t-indent) (valid? t-dedent) (valid? t-newline))
                (scan-layout p lx valid?))
       (out #t))
     (scan-advance-while! lx " \t\r\n" #t)
     ;; the norm bars ‖ open and close
     (when (and (= (lexer-lookahead lx) norm-bar)
                (or (valid? t-norm-open) (valid? t-norm-close)))
       (lexer-advance! lx #f)
       (when (valid? t-norm-close)
         (when (= (lexer-lookahead lx) subscript-plus) (lexer-advance! lx #f))
         (lexer-mark-end! lx)
         (lexer-set-result! lx t-norm-close)
         (out #t))
       (lexer-mark-end! lx)
       (lexer-set-result! lx t-norm-open)
       (out #t))
     (when (and (or (valid? t-block-comment) (valid? t-doc-comment)
                    (valid? t-module-doc-comment))
                (scan-char? lx #\/))
       (out (scan-block-or-doc-comment lx valid?)))
     ;; r starts a raw string or an identifier
     (when (and (scan-char? lx #\r)
                (or (valid? t-raw-string-literal) (valid? t-scanned-ident)))
       (lexer-advance! lx #f)
       (when (and (valid? t-raw-string-literal)
                  (or (scan-char? lx #\") (scan-char? lx #\#)))
         (out (scan-raw-string-after-r lx)))
       (out (and (valid? t-scanned-ident) (scan-identifier-continue lx #f))))
     (when (and (valid? t-scanned-ident)
                (let ((c (lexer-lookahead lx)))
                  (or (= c id-begin-escape) (id-first? c)))
                (scan-identifier lx))
       (out #t))
     #f)))

(define-public (ts-lean-scanner)
  (record-scanner (vector '(0)) lean-scan))
