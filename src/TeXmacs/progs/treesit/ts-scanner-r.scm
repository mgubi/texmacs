;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-r.scm
;; DESCRIPTION : the external scanner of tree-sitter-r
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; The scanner of tree-sitter-r: Copyright (c) 2025 tree-sitter-r authors,
;; under the MIT license: see $TEXMACS_PATH/misc/tree-sitter/r/LICENSE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-r (MIT): the newlines, which end
;; an expression except inside parentheses and brackets (so the scanner
;; keeps the stack of the open brackets), the else after newlines in braces,
;; and the raw strings r"-(...)-". The protocol of the scanners is described
;; in ts-scan.scm.

(texmacs-module (treesit ts-scanner-r)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars)
        (treesit ts-scan)))

(define t-start 0)
(define t-newline 1)
(define t-semicolon 2)
(define t-raw-string-open 3)
(define t-raw-string-content 4)
(define t-raw-string-close 5)
(define t-else 6)
(define t-open-paren 7)
(define t-close-paren 8)
(define t-open-brace 9)
(define t-close-brace 10)
(define t-open-bracket 11)
(define t-close-bracket 12)
(define t-open-bracket2 13)
(define t-close-bracket2 14)
(define t-error-sentinel 15)

(define scope-top-level 0)
(define scope-brace 1)
(define scope-paren 2)
(define scope-bracket 3)
(define scope-bracket2 4)

;; as in C, where the scopes share the 1024 bytes of the state with the raw
;; string and the count
(define max-scopes 1017)

;; the payload is #(scopes closing-bracket hyphen-count closing-quote), the
;; innermost scope first, the characters as code points (0 for none)
(define (r-create) (vector '() 0 0 0))

(define (r-reset! p)
  (vector-set! p 0 '())
  (vector-set! p 1 0)
  (vector-set! p 2 0)
  (vector-set! p 3 0))

(define (r-serialize p)
  ;; the scopes as a string of digits, one per scope, to stay small
  (list (vector-ref p 1) (vector-ref p 2) (vector-ref p 3)
        (list->string (map (lambda (s) (integer->char (+ 48 s))) (vector-ref p 0)))))

(define (r-deserialize p state)
  (if (not state) (r-reset! p)
      (begin
        (vector-set! p 1 (car state))
        (vector-set! p 2 (cadr state))
        (vector-set! p 3 (caddr state))
        (vector-set! p 0 (map (lambda (c) (- (char->integer c) 48))
                              (string->list (cadddr state)))))))

(define (r-char c) (char->integer c))

(define (scopes-peek p)
  (let ((l (vector-ref p 0))) (if (null? l) scope-top-level (car l))))

(define (scopes-push! p scope)
  (and (< (length (vector-ref p 0)) max-scopes)
       (begin (vector-set! p 0 (cons scope (vector-ref p 0))) #t)))

(define (scopes-pop! p expected)
  (let ((l (vector-ref p 0)))
    (and (pair? l)
         (begin (vector-set! p 0 (cdr l))
                (= (car l) expected)))))

(define (identifier-continuation? c)
  (or (ts-walnum? c) (= c (r-char #\_)) (= c (r-char #\.)) (>= c 128)))

(define (scan-else lx)
  (and (scan-word! lx "else")
       (not (identifier-continuation? (lexer-lookahead lx)))
       (scan-accept! lx t-else)))

(define (scan-else-with-leading-newlines lx)
  (let loop ()
    (let ((c (lexer-lookahead lx)))
      (when (ts-wspace? c)
        (lexer-advance! lx #t)
        (when (= c 10)
          (lexer-mark-end! lx)
          (lexer-set-result! lx t-newline))
        (loop))))
  (and (not (= (lexer-lookahead lx) (r-char #\#)))
       (begin (scan-else lx) #t)))

(define (scan-raw-string-open p lx)
  (define (la) (lexer-lookahead lx))
  (and (memv (la) (map r-char (string->list "rR")))
       (begin (lexer-advance! lx #f) #t)
       (let ((q (la)))
         (and (memv q (map r-char (string->list "\"'")))
              (begin
                (lexer-advance! lx #f)
                (let loop ((hyphens 0))
                  (if (= (la) (r-char #\-))
                      (and (< hyphens 255)
                           (begin (lexer-advance! lx #f) (loop (+ hyphens 1))))
                      (let ((closing (cond ((= (la) (r-char #\()) (r-char #\)))
                                           ((= (la) (r-char #\[)) (r-char #\]))
                                           ((= (la) (r-char #\{)) (r-char #\}))
                                           (else #f))))
                        (and closing
                             (begin
                               (lexer-advance! lx #f)
                               (lexer-mark-end! lx)
                               (lexer-set-result! lx t-raw-string-open)
                               (vector-set! p 1 closing)
                               (vector-set! p 2 hyphens)
                               (vector-set! p 3 q)
                               #t))))))))))

(define (scan-raw-string-content-or-close p lx)
  (define (la) (lexer-lookahead lx))
  (let* ((closing (vector-ref p 1))
         (hyphens (vector-ref p 2))
         (q (vector-ref p 3))
         (stop (string (integer->char closing))))
    (let loop ((any-content #f))
      (cond ((lexer-eof? lx) #f)
            ((not (= (la) closing))
             (when (= (scan-advance-until! lx stop #f) 0)  ; a null character
               (lexer-advance! lx #f))
             (loop #t))
            (else
             (lexer-mark-end! lx)
             (lexer-advance! lx #f)
             (let hyphen ((i 0))
               (cond ((< i hyphens)
                      (if (= (la) (r-char #\-))
                          (begin (lexer-advance! lx #f) (hyphen (+ i 1)))
                          (loop #t)))
                     ((not (= (la) q)) (loop #t))
                     (else
                      (lexer-advance! lx #f)
                      (if any-content
                          (lexer-set-result! lx t-raw-string-content)
                          (begin (lexer-mark-end! lx)
                                 (lexer-set-result! lx t-raw-string-close)))
                      #t))))))))

(define (scan-raw-string-close p lx)
  (lexer-advance! lx #f)
  (do ((i 0 (+ i 1))) ((= i (vector-ref p 2))) (lexer-advance! lx #f))
  (lexer-advance! lx #f)
  (lexer-mark-end! lx)
  (lexer-set-result! lx t-raw-string-close)
  #t)

(define (scan-token lx t)
  ;; one character
  (lexer-advance! lx #f)
  (scan-accept! lx t))

(define (scan-open-block p lx scope t)
  (and (scopes-push! p scope) (scan-token lx t)))

(define (scan-close-block p lx scope t)
  (and (scopes-pop! p scope) (scan-token lx t)))

(define (scan-open-bracket-or-bracket2 p lx valid)
  (lexer-advance! lx #f)
  (cond ((and (vector-ref valid t-open-bracket2) (= (lexer-lookahead lx) (r-char #\[)))
         (scan-open-block p lx scope-bracket2 t-open-bracket2))
        ((vector-ref valid t-open-bracket)
         (and (scopes-push! p scope-bracket)
              (begin (lexer-mark-end! lx) (lexer-set-result! lx t-open-bracket) #t)))
        (else #f)))

(define (scan-close-bracket2 p lx)
  (lexer-advance! lx #f)
  (and (= (lexer-lookahead lx) (r-char #\]))
       (scan-close-block p lx scope-bracket2 t-close-bracket2)))

(define (r-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (define (la) (lexer-lookahead lx))
  (define (la? c) (= (la) (r-char c)))
  (cond ((valid? t-error-sentinel) #f)
        ((valid? t-start) (lexer-set-result! lx t-start) #t)
        ((valid? t-raw-string-content) (scan-raw-string-content-or-close p lx))
        ((valid? t-raw-string-close) (scan-raw-string-close p lx))
        (else
         ;; the whitespace, and the newlines inside parentheses and brackets
         (scan-skip-spaces! lx #t (and (memv (scopes-peek p)
                                             (list scope-paren scope-bracket
                                                   scope-bracket2))
                                       #t))
         (cond ((and (valid? t-semicolon) (la? #\;)) (scan-token lx t-semicolon))
               ((and (valid? t-open-paren) (la? #\())
                (scan-open-block p lx scope-paren t-open-paren))
               ((and (valid? t-close-paren) (la? #\)))
                (scan-close-block p lx scope-paren t-close-paren))
               ((and (valid? t-open-brace) (la? #\{))
                (scan-open-block p lx scope-brace t-open-brace))
               ((and (valid? t-close-brace) (la? #\}))
                (scan-close-block p lx scope-brace t-close-brace))
               ((and (or (valid? t-open-bracket) (valid? t-open-bracket2)) (la? #\[))
                (scan-open-bracket-or-bracket2 p lx valid))
               ((and (valid? t-close-bracket) (la? #\]) (= (scopes-peek p) scope-bracket))
                (scan-close-block p lx scope-bracket t-close-bracket))
               ((and (valid? t-close-bracket2) (la? #\]) (= (scopes-peek p) scope-bracket2))
                (scan-close-bracket2 p lx))
               ((and (valid? t-raw-string-open) (or (la? #\r) (la? #\R)))
                (scan-raw-string-open p lx))
               ((and (valid? t-else) (la? #\e)) (scan-else lx))
               ((and (valid? t-else) (= (scopes-peek p) scope-brace) (la? #\newline))
                (scan-else-with-leading-newlines lx))
               ((and (valid? t-newline) (la? #\newline)) (scan-token lx t-newline))
               (else #f)))))

(define-public (ts-r-scanner)
  (vector r-create r-scan r-deserialize r-serialize))
