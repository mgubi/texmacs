
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-python.scm
;; DESCRIPTION : the external scanner of tree-sitter-python
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-python (MIT): newlines,
;; indentation and the delimiters of strings.
;;
;; An external scanner, for ts-parse.scm, is a vector of four procedures
;;
;;   #(create scan deserialize serialize)
;;
;; create gives the payload, (scan payload lexer valid) tells whether a token
;; was found (valid is the vector of the external tokens which the parser
;; accepts, by their index in the externals of the grammar) and sets the
;; result with lexer-set-result!, (serialize payload) is the state as a value
;; which equal? compares and (deserialize payload state) restores it, #f
;; being the initial state.

(texmacs-module (treesit ts-scanner-python)
  (:use (treesit ts-base) (treesit ts-parse)))

(define t-newline 0)
(define t-indent 1)
(define t-dedent 2)
(define t-string-start 3)
(define t-string-content 4)
(define t-escape-interpolation 5)
(define t-string-end 6)
(define t-comment 7)
(define t-close-paren 8)
(define t-close-bracket 9)
(define t-close-brace 10)
(define t-except 11)

(define d-single 1)
(define d-double 2)
(define d-back 4)
(define d-raw 8)
(define d-format 16)
(define d-triple 32)
(define d-bytes 64)

(define (delim? d f) (not (= (logand d f) 0)))

(define (end-character d)
  (cond ((delim? d d-single) 39)
        ((delim? d d-double) 34)
        ((delim? d d-back) 96)
        (else 0)))

;; the payload is #(indents delimiters inside-interpolated-string?), with the
;; innermost indent and delimiter first
(define (indents s) (vector-ref s 0))
(define (delimiters s) (vector-ref s 1))

(define (python-create) (vector (list 0) '() #f))

(define (python-serialize s)
  (list (vector-ref s 2) (delimiters s) (indents s)))

(define (python-deserialize s state)
  (if state
      (begin (vector-set! s 2 (car state))
             (vector-set! s 1 (cadr state))
             (vector-set! s 0 (caddr state)))
      (begin (vector-set! s 0 (list 0))
             (vector-set! s 1 '())
             (vector-set! s 2 #f))))

(define (python-scan s lx valid)
  (define (valid? i) (vector-ref valid i))
  (define (la) (lexer-lookahead lx))
  (define (la? c) (= (lexer-lookahead lx) (char->integer c)))
  (define (advance!) (lexer-advance! lx #f))
  (define (skip!) (lexer-advance! lx #t))
  (define (mark!) (lexer-mark-end! lx))
  (define (result! t) (lexer-set-result! lx t))
  (define (pop-delimiter!) (vector-set! s 1 (cdr (delimiters s))))
  (let* ((error-recovery (and (valid? t-string-content) (valid? t-indent)))
         (within-brackets (or (valid? t-close-brace) (valid? t-close-paren)
                              (valid? t-close-bracket)))
         (advanced-once #f))
    (call-with-current-continuation
     (lambda (return)
       ;; {{ and }} in f-strings
       (when (and (valid? t-escape-interpolation) (pair? (delimiters s))
                  (or (la? #\{) (la? #\})) (not error-recovery))
         (let ((d (car (delimiters s))))
           (when (delim? d d-format)
             (mark!)
             (let ((left (la? #\{)))
               (advance!)
               (set! advanced-once #t)
               (if (or (and (la? #\{) left) (and (la? #\}) (not left)))
                   (begin (advance!) (mark!) (result! t-escape-interpolation) (return #t))
                   (return #f))))))
       ;; the content of strings
       (when (and (valid? t-string-content) (pair? (delimiters s)) (not error-recovery))
         (let* ((d (car (delimiters s)))
                (end-char (end-character d))
                (has-content advanced-once))
           (let loop ()
             (unless (= (la) 0)
               (cond
                ((and (or advanced-once (la? #\{) (la? #\})) (delim? d d-format))
                 (mark!) (result! t-string-content) (return has-content))
                ((la? #\\)
                 (cond ((delim? d d-raw)
                        (advance!)
                        (when (or (= (la) (end-character d)) (la? #\\)) (advance!))
                        (cond ((la? #\return) (advance!) (when (la? #\newline) (advance!)))
                              ((la? #\newline) (advance!)))
                        (loop))
                       ((delim? d d-bytes)
                        (mark!)
                        (advance!)
                        (if (or (la? #\N) (la? #\u) (la? #\U))
                            (begin (advance!) (advance!) (set! has-content #t) (loop))
                            (begin (result! t-string-content) (return has-content))))
                       (else (mark!) (result! t-string-content) (return has-content))))
                ((= (la) end-char)
                 (if (delim? d d-triple)
                     (begin
                       (mark!)
                       (advance!)
                       (if (= (la) end-char)
                           (begin
                             (advance!)
                             (if (= (la) end-char)
                                 (begin
                                   (if has-content
                                       (result! t-string-content)
                                       (begin (advance!) (mark!) (pop-delimiter!)
                                              (result! t-string-end)
                                              (vector-set! s 2 #f)))
                                   (return #t))
                                 (begin (mark!) (result! t-string-content) (return #t))))
                           (begin (mark!) (result! t-string-content) (return #t))))
                     (begin
                       (if has-content
                           (result! t-string-content)
                           (begin (advance!) (pop-delimiter!) (result! t-string-end)
                                  (vector-set! s 2 #f)))
                       (mark!)
                       (return #t))))
                ((and (la? #\newline) has-content (not (delim? d d-triple)))
                 (return #f))
                (else (advance!) (set! has-content #t) (loop)))))))
       ;; newlines and indentation
       (mark!)
       (let ((found-eol #f)
             (indent 0)
             (first-comment-indent -1))
         (let loop ()
           (cond ((la? #\newline) (set! found-eol #t) (set! indent 0) (skip!) (loop))
                 ((la? #\space) (set! indent (+ indent 1)) (skip!) (loop))
                 ((or (la? #\return) (= (la) 12)) (set! indent 0) (skip!) (loop))
                 ((la? #\tab) (set! indent (+ indent 8)) (skip!) (loop))
                 ((and (la? #\#) (or (valid? t-indent) (valid? t-dedent) (valid? t-newline)
                                     (valid? t-except)))
                  (unless found-eol (return #f))
                  (when (= first-comment-indent -1) (set! first-comment-indent indent))
                  (let comment () (unless (or (= (la) 0) (la? #\newline)) (skip!) (comment)))
                  (skip!)
                  (set! indent 0)
                  (loop))
                 ((la? #\\)
                  (skip!)
                  (when (la? #\return) (skip!))
                  (if (or (la? #\newline) (lexer-eof? lx))
                      (begin (skip!) (loop))
                      (return #f)))
                 ((lexer-eof? lx) (set! indent 0) (set! found-eol #t))
                 (else #t)))
         (when found-eol
           (when (pair? (indents s))
             (let ((current (car (indents s))))
               (when (and (valid? t-indent) (> indent current))
                 (vector-set! s 0 (cons indent (indents s)))
                 (result! t-indent)
                 (return #t))
               (let ((string-start-next (or (la? #\") (la? #\') (la? #\`))))
                 (when (and (or (valid? t-dedent)
                                (and (not (valid? t-newline))
                                     (not (and (valid? t-string-start) string-start-next))
                                     (not within-brackets)))
                            (< indent current)
                            (not (vector-ref s 2))
                            (< first-comment-indent current))
                   (vector-set! s 0 (cdr (indents s)))
                   (result! t-dedent)
                   (return #t)))))
           (when (and (valid? t-newline) (not error-recovery))
             (result! t-newline)
             (return #t)))
         ;; the start of strings
         (when (and (= first-comment-indent -1) (valid? t-string-start))
           (let ((d 0) (has-flags #f))
             (let loop ()
               (unless (= (la) 0)
                 (let ((c (la)))
                   (cond ((memv c (map char->integer '(#\f #\F #\t #\T)))
                          (set! d (logior d d-format)) (set! has-flags #t) (advance!) (loop))
                         ((memv c (map char->integer '(#\r #\R)))
                          (set! d (logior d d-raw)) (set! has-flags #t) (advance!) (loop))
                         ((memv c (map char->integer '(#\b #\B)))
                          (set! d (logior d d-bytes)) (set! has-flags #t) (advance!) (loop))
                         ((memv c (map char->integer '(#\u #\U)))
                          (set! has-flags #t) (advance!) (loop))))))
             (cond ((la? #\`)
                    (set! d (logior d d-back)) (advance!) (mark!))
                   ((la? #\')
                    (set! d (logior d d-single)) (advance!) (mark!)
                    (when (la? #\')
                      (advance!)
                      (when (la? #\') (advance!) (mark!) (set! d (logior d d-triple)))))
                   ((la? #\")
                    (set! d (logior d d-double)) (advance!) (mark!)
                    (when (la? #\")
                      (advance!)
                      (when (la? #\") (advance!) (mark!) (set! d (logior d d-triple))))))
             (when (> (end-character d) 0)
               (vector-set! s 1 (cons d (delimiters s)))
               (result! t-string-start)
               (vector-set! s 2 (delim? d d-format))
               (return #t))
             (when has-flags (return #f))))
         #f)))))

(define-public (ts-python-scanner)
  (vector python-create python-scan python-deserialize python-serialize))
