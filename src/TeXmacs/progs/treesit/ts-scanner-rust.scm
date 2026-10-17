;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-rust.scm
;; DESCRIPTION : the external scanner of tree-sitter-rust
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-rust (MIT): the content of strings,
;; raw strings r#"..."#, float literals, nested block comments and doc
;; comments. The protocol of the scanners is described in ts-scan.scm. As in
;; C, the block comments look at the low byte of the characters (a char).

(texmacs-module (treesit ts-scanner-rust)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars)
        (treesit ts-scan)))

(define t-string-content 0)
(define t-string-close 1)
(define t-raw-string-literal-start 2)
(define t-raw-string-literal-content 3)
(define t-raw-string-literal-end 4)
(define t-float-literal 5)
(define t-block-outer-doc-marker 6)
(define t-block-inner-doc-marker 7)
(define t-block-comment-content 8)
(define t-line-doc-content 9)
(define t-error-sentinel 10)

;; the payload is #(opening-hash-count)

(define (num-char? c) (or (= c 95) (ts-wdigit? c)))

(define (advance-to! lx stops)
  ;; advance up to one of the characters of stops, or the end: over a null
  ;; character too (the loops of C only stop at the end)
  (scan-advance-until! lx stops #f)
  (when (and (= (lexer-lookahead lx) 0) (not (lexer-eof? lx)))
    (lexer-advance! lx #f)
    (advance-to! lx stops)))

(define (process-string lx)
  (let loop ((has-content #f))
    (cond ((or (scan-char? lx #\") (scan-char? lx #\\))
           (lexer-set-result! lx t-string-content)
           (lexer-mark-end! lx)
           has-content)
          ((lexer-eof? lx) #f)
          (else (advance-to! lx "\"\\") (loop #t)))))

(define (scan-raw-string-start p lx)
  (when (or (scan-char? lx #\b) (scan-char? lx #\c)) (lexer-advance! lx #f))
  (and (scan-char? lx #\r)
       (begin (lexer-advance! lx #f) #t)
       (let ((hashes (scan-advance-while! lx "#" #f)))
         (and (scan-char? lx #\")
              (begin
                (lexer-advance! lx #f)
                (vector-set! p 0 (modulo hashes 256))
                (lexer-set-result! lx t-raw-string-literal-start)
                #t)))))

(define (scan-raw-string-content p lx)
  (let loop ()
    (cond ((lexer-eof? lx) #f)
          ((scan-char? lx #\")
           (lexer-mark-end! lx)
           (lexer-advance! lx #f)
           (let hash ((k 0))
             (if (and (scan-char? lx #\#) (< k (vector-ref p 0)))
                 (begin (lexer-advance! lx #f) (hash (+ k 1)))
                 (if (= k (vector-ref p 0))
                     (begin (lexer-set-result! lx t-raw-string-literal-content) #t)
                     (loop)))))
          (else (advance-to! lx "\"") (loop)))))

(define (scan-raw-string-end p lx)
  (lexer-advance! lx #f)
  (let loop ((i 0))
    (when (< i (vector-ref p 0))
      (lexer-advance! lx #f)
      (loop (+ i 1))))
  (lexer-set-result! lx t-raw-string-literal-end)
  #t)

(define (process-float-literal lx)
  (define (la) (lexer-lookahead lx))
  (define (adv!) (lexer-advance! lx #f))
  (define (nums!) (let loop () (when (num-char? (la)) (adv!) (loop))))
  (lexer-set-result! lx t-float-literal)
  (adv!)
  (nums!)
  (call-with-exit
   (lambda (out)
     (let ((has-fraction #f) (has-exponent #f))
       (when (= (la) 46)
         (set! has-fraction #t)
         (adv!)
         ;; 1.max(2) is not a float but an integer
         (when (ts-walpha? (la)) (out #f))
         (when (= (la) 46) (out #f))
         (nums!))
       (lexer-mark-end! lx)
       (when (or (= (la) 101) (= (la) 69))
         (set! has-exponent #t)
         (adv!)
         (when (or (= (la) 43) (= (la) 45)) (adv!))
         (unless (num-char? (la)) (out #t))
         (adv!)
         (nums!)
         (lexer-mark-end! lx))
       (when (and (not has-exponent) (not has-fraction)) (out #f))
       (unless (memv (la) '(117 105 102)) (out #t))  ; u i f
       (adv!)
       (unless (ts-wdigit? (la)) (out #t))
       (let loop () (when (ts-wdigit? (la)) (adv!) (loop)))
       (lexer-mark-end! lx)
       #t))))

(define (process-line-doc-content lx)
  (lexer-set-result! lx t-line-doc-content)
  (let loop ()
    (cond ((lexer-eof? lx) #t)
          ((scan-char? lx #\newline) (lexer-advance! lx #f) #t)
          (else (advance-to! lx "\n") (loop)))))

(define (low-byte c) (modulo c 256))

(define (process-block-comment lx valid?)
  (let ((first (low-byte (lexer-lookahead lx))))
    (call-with-exit
     (lambda (out)
       (when (and (valid? t-block-inner-doc-marker) (= first 33))
         (lexer-set-result! lx t-block-inner-doc-marker)
         (lexer-advance! lx #f)
         (out #t))
       (if (and (valid? t-block-outer-doc-marker) (= first 42))
           (begin
             (lexer-advance! lx #f)
             (lexer-mark-end! lx)
             ;; /**/ is an empty block comment
             (when (scan-char? lx #\/) (out #f))
             ;; the outer doc markers have two * only
             (unless (scan-char? lx #\*)
               (lexer-set-result! lx t-block-outer-doc-marker)
               (out #t)))
           (lexer-advance! lx #f))
       (if (not (valid? t-block-comment-content)) #f
           ;; the states: 0 after /, 1 after *, 2 continuing
           (let ((state (cond ((= first 42)
                               ;; /*!*/: no content
                               (when (scan-char? lx #\/) (out #f))
                               1)
                              ((= first 47) 0)
                              (else 2)))
                 (depth 1))
             (let loop ()
               (when (and (not (lexer-eof? lx)) (not (= depth 0)))
                 (let ((c (low-byte (lexer-lookahead lx))))
                   (cond ((= state 0)
                          (when (= c 42) (set! depth (+ depth 1)))
                          (set! state 2))
                         ((= state 1)
                          (cond ((= c 42) (lexer-mark-end! lx) (set! state 1))
                                (else
                                 (when (= c 47) (set! depth (- depth 1)))
                                 (set! state 2))))
                         (else
                          (lexer-mark-end! lx)
                          (cond ((= c 47) (set! state 0))
                                ((= c 42) (set! state 1)))))
                   (lexer-advance! lx #f)
                   (when (and (= c 47) (not (= depth 0))) (lexer-mark-end! lx))
                   (loop))))
             (lexer-set-result! lx t-block-comment-content)
             #t))))))

(define (rust-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (call-with-exit
   (lambda (out)
     (when (valid? t-error-sentinel) (out #f))
     (when (or (valid? t-block-comment-content) (valid? t-block-inner-doc-marker)
               (valid? t-block-outer-doc-marker))
       (out (process-block-comment lx valid?)))
     ;; without content (at " or \), the close of the string may follow
     (when (and (valid? t-string-content) (not (valid? t-float-literal))
                (process-string lx))
       (out #t))
     (when (and (valid? t-string-close) (scan-char? lx #\"))
       (lexer-advance! lx #f)
       (lexer-set-result! lx t-string-close)
       (lexer-mark-end! lx)
       (out #t))
     (when (valid? t-line-doc-content) (out (process-line-doc-content lx)))
     (scan-skip-spaces! lx #t #t)
     (when (and (valid? t-raw-string-literal-start)
                (or (scan-char? lx #\r) (scan-char? lx #\b) (scan-char? lx #\c)))
       (out (scan-raw-string-start p lx)))
     (when (valid? t-raw-string-literal-content) (out (scan-raw-string-content p lx)))
     (when (and (valid? t-raw-string-literal-end) (scan-char? lx #\"))
       (out (scan-raw-string-end p lx)))
     (when (and (valid? t-float-literal) (ts-wdigit? (lexer-lookahead lx)))
       (out (process-float-literal lx)))
     #f)))

(define-public (ts-rust-scanner)
  (record-scanner (vector 0) rust-scan))
