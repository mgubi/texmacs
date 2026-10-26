;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-julia.scm
;; DESCRIPTION : the external scanner of tree-sitter-julia
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; The scanner of tree-sitter-julia: Copyright (c) 2018 Max Brunsfeld, GitHub,
;; under the MIT license: see $TEXMACS_PATH/misc/tree-sitter/julia/LICENSE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-julia (MIT): the rest of the
;; nested block comments #= =#, the brackets and quotes which follow a
;; token immediately, and the content of strings and commands (one or three
;; delimiters, with or without interpolations). The scanner has no state.
;; The protocol of the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-julia)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-scan)))

(define t-block-comment-rest 0)
(define t-immediate-paren 1)
(define t-immediate-bracket 2)
(define t-immediate-brace 3)
(define t-immediate-string-start 4)
(define t-immediate-command-start 5)
(define t-content-cmd-1 6)
(define t-content-cmd-1-raw 7)
(define t-content-cmd-3 8)
(define t-content-cmd-3-raw 9)
(define t-content-str-1 10)
(define t-content-str-1-raw 11)
(define t-content-str-3 12)
(define t-content-str-3-raw 13)
(define t-end-cmd 14)
(define t-end-str 15)

(define (jl-char c) (char->integer c))

(define (scan-content lx content end-char n-delim interp?)
  (define (la) (lexer-lookahead lx))
  (define (advance!) (lexer-advance! lx #f))
  (let ((end (if (= end-char (jl-char #\")) t-end-str t-end-cmd))
        (stops (string #\$ #\\ (integer->char end-char))))
    (let loop ((has-content #f))
      (let ((next (la)))
        (if (= next 0) #f
            (begin
              (lexer-mark-end! lx)
              (cond ((and interp? (or (= next (jl-char #\$)) (= next (jl-char #\\))))
                     (lexer-set-result! lx content)
                     has-content)
                    ((= next (jl-char #\\))
                     ;; in raw strings, the escaped delimiters and \\
                     (advance!)
                     (if (or (= (la) end-char) (= (la) (jl-char #\\)))
                         (begin (lexer-set-result! lx content) has-content)
                         (begin (advance!) (loop #t))))
                    ((let delimiter ((i 1))
                       (cond ((> i n-delim) #t)
                             ((= (la) end-char) (advance!) (delimiter (+ i 1)))
                             (else #f)))
                     (if has-content
                         (lexer-set-result! lx content)
                         (begin (lexer-mark-end! lx) (lexer-set-result! lx end)))
                     #t)
                    (else
                     ;; and over the characters which would come here too
                     (advance!)
                     (scan-advance-until! lx stops #f)
                     (loop #t)))))))))

(define (scan-block-comment lx)
  ;; the first #= is read by the lexer of tree-sitter
  (define (la) (lexer-lookahead lx))
  (define (advance!) (lexer-advance! lx #f))
  (let loop ((after-eq #f) (depth 1))
    (let ((c (la)))
      (cond ((= c (jl-char #\=)) (advance!) (loop #t depth))
            ((= c (jl-char #\#))
             (advance!)
             (cond (after-eq
                    (if (= depth 1)
                        (begin (lexer-set-result! lx t-block-comment-rest) #t)
                        (loop #f (- depth 1))))
                   ((= (la) (jl-char #\=)) (advance!) (loop #f (+ depth 1)))
                   (else (loop #f depth))))
            ((= c 0) #f)
            (else (scan-advance-until! lx "=#" #f) (loop #f depth))))))

(define (julia-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (define (la? c) (= (lexer-lookahead lx) (jl-char c)))
  (define (immediate t) (lexer-set-result! lx t) #t)
  (cond ((and (valid? t-immediate-paren) (la? #\()) (immediate t-immediate-paren))
        ((and (valid? t-immediate-bracket) (la? #\[)) (immediate t-immediate-bracket))
        ((and (valid? t-immediate-brace) (la? #\{)) (immediate t-immediate-brace))
        ((and (valid? t-immediate-string-start) (la? #\")) (immediate t-immediate-string-start))
        ((and (valid? t-immediate-command-start) (la? #\`))
         (immediate t-immediate-command-start))
        ;; the attempts go on from where the previous ones stopped, as in C
        (else
         (let ((dq (jl-char #\")) (bq (jl-char #\`)))
           (or (and (valid? t-block-comment-rest) (scan-block-comment lx))
               (and (valid? t-content-str-1) (scan-content lx t-content-str-1 dq 1 #t))
               (and (valid? t-content-str-3) (scan-content lx t-content-str-3 dq 3 #t))
               (and (valid? t-content-cmd-1) (scan-content lx t-content-cmd-1 bq 1 #t))
               (and (valid? t-content-cmd-3) (scan-content lx t-content-cmd-3 bq 3 #t))
               (and (valid? t-content-str-1-raw) (scan-content lx t-content-str-1-raw dq 1 #f))
               (and (valid? t-content-str-3-raw) (scan-content lx t-content-str-3-raw dq 3 #f))
               (and (valid? t-content-cmd-1-raw) (scan-content lx t-content-cmd-1-raw bq 1 #f))
               (and (valid? t-content-cmd-3-raw)
                    (scan-content lx t-content-cmd-3-raw bq 3 #f)))))))

(define-public (ts-julia-scanner)
  (stateless-scanner julia-scan))
