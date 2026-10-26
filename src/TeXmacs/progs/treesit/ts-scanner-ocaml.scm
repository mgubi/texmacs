;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-ocaml.scm
;; DESCRIPTION : the external scanner of tree-sitter-ocaml
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; The scanner of tree-sitter-ocaml: Copyright (c) 2020 Max Brunsfeld and Pieter Goetschalckx,
;; under the MIT license: see $TEXMACS_PATH/misc/tree-sitter/ocaml/LICENSE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of common/scanner.h of tree-sitter-ocaml (MIT): nested comments
;; (with the strings and characters inside them), the delimiters of the
;; quoted strings {id|...|id} and of the strings, the line number
;; directives and the null character. The protocol of the scanners is
;; described in ts-scan.scm. (The C code saves the id of a quoted string
;; incompletely, bytes of an array of int32; here it is kept whole.)

(texmacs-module (treesit ts-scanner-ocaml)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars) (treesit ts-scan)))

(define t-comment 0)
(define t-left-quoted-string-delim 1)
(define t-right-quoted-string-delim 2)
(define t-string-delim 3)
(define t-line-number-directive 4)
(define t-null-character 5)
(define t-error-sentinel 6)

;; the payload is #(in-string quoted-string-id), the id a list of code points

(define (la lx) (lexer-lookahead lx))
(define (adv! lx) (lexer-advance! lx #f))

(define (scan-string lx)
  ;; after ", up to the closing "
  (let loop ()
    (let ((c (la lx)))
      (cond ((= c 92) (adv! lx) (adv! lx) (loop))
            ((= c 34) (adv! lx))
            ((and (= c 0) (lexer-eof? lx)) #t)
            (else (adv! lx) (loop))))))

;; the lower case letters of OCaml beyond ASCII, and the letters followed by
;; a combining diacritic which make one of them
(define lower-utf8-chars
  '(#xdf #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #xea #xeb #xec #xed
    #xee #xef #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf8 #xf9 #xfa #xfb #xfc #xfd
    #xfe #xff #x153 #x161 #x17e))

(define lower-utf8-pairs
  '((97 #x300 #xe0) (97 #x301 #xe1) (97 #x302 #xe2) (97 #x303 #xe3)
    (97 #x308 #xe4) (97 #x30a #xe5) (99 #x327 #xe7) (101 #x300 #xe8)
    (101 #x301 #xe9) (101 #x302 #xea) (101 #x308 #xeb) (105 #x300 #xec)
    (105 #x301 #xed) (105 #x302 #xee) (105 #x308 #xef) (110 #x303 #xf1)
    (111 #x300 #xf2) (111 #x301 #xf3) (111 #x302 #xf4) (111 #x303 #xf5)
    (111 #x308 #xf6) (115 #x30c #x161) (117 #x300 #xf9) (117 #x301 #xfa)
    (117 #x302 #xfb) (117 #x308 #xfc) (121 #x301 #xfd) (121 #x308 #xff)
    (122 #x30c #x17e)))

(define (scan-quoted-string-delim-char lx)
  ;; the next character of the id of a quoted string, or 0
  (let ((c (la lx)))
    (cond ((= c 124) 0)   ; |
          ((= c 95) (adv! lx) c)
          ((and (>= c 97) (<= c 122))
           (adv! lx)
           (let* ((d (la lx))
                  (r (and (>= d #x300) (<= d #x327)
                          (list-find lower-utf8-pairs
                                     (lambda (p) (and (= (car p) c) (= (cadr p) d)))))))
             (if r (begin (adv! lx) (caddr r)) c)))
          ((memv c lower-utf8-chars) (adv! lx) c)
          (else 0))))

(define (scan-left-quoted-string-delimiter p lx)
  (let loop ((id '()))
    (let ((c (scan-quoted-string-delim-char lx)))
      (if (not (= c 0)) (loop (cons c id))
          (if (= (la lx) 124)
              (begin
                (adv! lx)
                (vector-set! p 0 #t)
                (vector-set! p 1 (reverse id))
                #t)
              (begin (vector-set! p 1 '()) #f))))))

(define (scan-right-quoted-string-delimiter p lx)
  (let loop ((id (vector-ref p 1)))
    (cond ((pair? id)
           (and (= (scan-quoted-string-delim-char lx) (car id)) (loop (cdr id))))
          ((= (la lx) 125)   ; }
           (vector-set! p 0 #f)
           (vector-set! p 1 '())
           #t)
          (else #f))))

(define (scan-quoted-string p lx)
  (and (scan-left-quoted-string-delimiter p lx)
       (let loop ()
         (let ((c (la lx)))
           (cond ((= c 124)
                  (adv! lx)
                  (or (scan-right-quoted-string-delimiter p lx) (loop)))
                 ((and (= c 0) (lexer-eof? lx)) #f)
                 (else (adv! lx) (loop)))))))

(define (digit? c) (ts-wdigit? c))

(define (scan-character lx)
  ;; after ', a character literal: its last character, or 0
  (call-with-exit
   (lambda (out)
     (let ((last 0) (c (la lx)))
       (cond ((= c 92)   ; \
              (adv! lx)
              (let ((d (la lx)))
                (cond ((digit? d)
                       (adv! lx)
                       (do ((i 0 (+ i 1))) ((= i 2))
                         (unless (digit? (la lx)) (out 0))
                         (adv! lx)))
                      ((= d 120)   ; x
                       (adv! lx)
                       (do ((i 0 (+ i 1))) ((= i 2))
                         (let ((u (la lx)))
                           (unless (or (digit? u) (and (>= u 65) (<= u 70)) (and (>= u 97) (<= u 102)))
                             (out 0)))
                         (adv! lx)))
                      ((= d 111)   ; o
                       (adv! lx)
                       (do ((i 0 (+ i 1))) ((= i 3))
                         (unless (and (digit? (la lx)) (<= (la lx) 55)) (out 0))
                         (adv! lx)))
                      ((memv d '(39 34 92 110 116 98 114 32))
                       (set! last d)
                       (adv! lx))
                      (else (out 0)))))
             ((= c 39) #t)
             ((= c 13)
              (adv! lx)
              (let loop () (when (= (la lx) 13) (adv! lx) (loop)))
              (unless (= (la lx) 10) (out 0))
              (adv! lx))
             ((= c 0)
              (when (lexer-eof? lx) (out 0))
              (adv! lx))
             ((< c 256) (set! last c) (adv! lx))
             (else (out 0)))
       (if (= (la lx) 39)
           (begin (adv! lx) 0)
           last)))))

(define (lowercase-ext? c) (or (and (>= c 97) (<= c 122)) (= c 95) (>= c 192)))
(define (identstart? c) (or (lowercase-ext? c) (and (>= c 65) (<= c 90))))
(define (identchar? c) (or (identstart? c) (and (>= c 48) (<= c 57)) (= c 39)))

(define (scan-identifier lx)
  (and (identstart? (la lx))
       (begin
         (adv! lx)
         (let loop () (when (identchar? (la lx)) (adv! lx) (loop)))
         #t)))

(define (scan-extattrident lx)
  (let loop ()
    (and (scan-identifier lx)
         (if (not (= (la lx) 46)) #t
             (begin (adv! lx) (loop))))))

(define (scan-comment p lx)
  ;; after (, up to the matching *) (#t also at the end); last is a
  ;; character already read (by scan-character), seen as the lookahead
  (and (= (la lx) 42)
       (begin
         (adv! lx)
         (let loop ((last 0) (depth 0))
           (define (take) (if (= last 0) (adv! lx)))
           (let ((c (if (= last 0) (la lx) last)))
             (cond
              ((= c 40)   ; (
               (take)
               (if (= (la lx) 42)
                   (begin (adv! lx) (loop 0 (+ depth 1)))
                   (loop 0 depth)))
              ((= c 42)   ; *
               (take)
               (if (= (la lx) 41)
                   (begin
                     (adv! lx)
                     (if (= depth 0) #t (loop 0 (- depth 1))))
                   (loop 0 depth)))
              ((= c 39)   ; '
               (take)
               (loop (scan-character lx) depth))
              ((= c 34)   ; "
               (take)
               (scan-string lx)
               (loop 0 depth))
              ((= c 123)   ; {
               (take)
               (let ((ok (or (not (= (la lx) 37))   ; %
                             (begin
                               (adv! lx)
                               (when (= (la lx) 37) (adv! lx))
                               (and (scan-extattrident lx)
                                    (begin (scan-skip-spaces! lx #f #t) #t))))))
                 (when (and ok (scan-quoted-string p lx)) (adv! lx))
                 (loop 0 depth)))
              ((= c 0)
               (if (lexer-eof? lx) #t
                   (begin (take) (loop 0 depth))))
              (else
               (if (or (scan-identifier lx) (not (= last 0)))
                   (loop 0 depth)
                   (begin (adv! lx) (loop 0 depth))))))))))

(define (ocaml-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (define (in-string?) (vector-ref p 0))
  (call-with-exit
   (lambda (out)
     (when (and (not (valid? t-error-sentinel)) (valid? t-left-quoted-string-delim)
                (or (lowercase-ext? (la lx)) (= (la lx) 124)))
       (lexer-set-result! lx t-left-quoted-string-delim)
       (out (scan-left-quoted-string-delimiter p lx)))
     (when (and (not (valid? t-error-sentinel)) (valid? t-right-quoted-string-delim)
                (= (la lx) 124))
       (adv! lx)
       (lexer-set-result! lx t-right-quoted-string-delim)
       (out (scan-right-quoted-string-delimiter p lx)))
     (when (and (in-string?) (valid? t-string-delim) (= (la lx) 34))
       (adv! lx)
       (vector-set! p 0 #f)
       (lexer-set-result! lx t-string-delim)
       (out #t))
     (scan-skip-spaces! lx #t #t)
     (when (and (not (in-string?)) (valid? t-string-delim) (= (la lx) 34))
       (adv! lx)
       (vector-set! p 0 #t)
       (lexer-set-result! lx t-string-delim)
       (out #t))
     (when (and (not (in-string?)) (valid? t-line-number-directive)
                (= (la lx) 35) (= (lexer-column lx) 0))   ; # at the start of a line
       (adv! lx)
       (scan-advance-while! lx " \t" #f)
       (unless (digit? (la lx)) (out #f))
       (scan-advance-while! lx "0123456789" #f)
       (scan-advance-while! lx " \t" #f)
       (unless (= (la lx) 34) (out #f))
       (adv! lx)
       (let loop ()
         (unless (or (memv (la lx) '(10 13 34)) (lexer-eof? lx)) (adv! lx) (loop)))
       (unless (= (la lx) 34) (out #f))
       (adv! lx)
       (let loop ()
         (unless (or (memv (la lx) '(10 13)) (lexer-eof? lx)) (adv! lx) (loop)))
       (lexer-set-result! lx t-line-number-directive)
       (out #t))
     (when (and (not (in-string?)) (valid? t-comment) (= (la lx) 40))
       (adv! lx)
       (lexer-set-result! lx t-comment)
       (out (scan-comment p lx)))
     (when (and (valid? t-null-character) (= (la lx) 0) (not (lexer-eof? lx)))
       (adv! lx)
       (lexer-set-result! lx t-null-character)
       (out #t))
     #f)))

(define-public (ts-ocaml-scanner)
  (record-scanner (vector #f '()) ocaml-scan))
