;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-lua.scm
;; DESCRIPTION : the external scanner of tree-sitter-lua
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; The scanner of tree-sitter-lua: Copyright (c) 2021 Munif Tanjim,
;; under the MIT license: see $TEXMACS_PATH/misc/tree-sitter/lua/LICENSE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-grammars/tree-sitter-lua (MIT):
;; the long brackets [==[ ... ]==] of block strings and block comments. The
;; protocol of the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-lua)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-scan)))

(define t-block-comment-start 0)
(define t-block-comment-content 1)
(define t-block-comment-end 2)
(define t-block-string-start 3)
(define t-block-string-content 4)
(define t-block-string-end 5)

;; the payload is #(ending-char level-count); as in C, the state is never
;; empty, and restoring an empty one (the start) changes nothing
(define (lua-create) (vector 0 0))
(define (lua-serialize p) (list (vector-ref p 0) (vector-ref p 1)))
(define (lua-deserialize p state)
  (when state
    (vector-set! p 0 (car state))
    (vector-set! p 1 (cadr state))))

(define (reset-state! p)
  (vector-set! p 0 0)
  (vector-set! p 1 0))

(define (consume-char! lx c)
  (and (scan-char? lx c) (begin (lexer-advance! lx #f) #t)))

(define (scan-block-start p lx)
  (and (consume-char! lx #\[)
       (let ((level (scan-advance-while! lx "=" #f)))
         (and (consume-char! lx #\[)
              (begin (vector-set! p 1 (modulo level 256)) #t)))))

(define (scan-block-end p lx)
  (and (consume-char! lx #\])
       (let ((level (modulo (scan-advance-while! lx "=" #f) 256)))
         (and (= (vector-ref p 1) level) (consume-char! lx #\])))))

(define (scan-block-content p lx)
  (let loop ()
    (cond ((= (lexer-lookahead lx) 0) #f)
          ((scan-char? lx #\])
           (lexer-mark-end! lx)
           (or (scan-block-end p lx) (loop)))
          (else (scan-advance-until! lx "]" #f) (loop)))))

(define (scan-comment-start p lx)
  (and (consume-char! lx #\-) (consume-char! lx #\-)
       (begin (lexer-mark-end! lx) (scan-block-start p lx))
       (begin
         (lexer-mark-end! lx)
         (lexer-set-result! lx t-block-comment-start)
         #t)))

(define (scan-comment-content p lx)
  (if (= (vector-ref p 0) 0)
      ;; a block comment
      (and (scan-block-content p lx)
           (begin (lexer-set-result! lx t-block-comment-content) #t))
      (let loop ()
        (cond ((= (lexer-lookahead lx) 0) #f)
              ((= (lexer-lookahead lx) (vector-ref p 0))
               (reset-state! p)
               (lexer-set-result! lx t-block-comment-content)
               #t)
              (else (lexer-advance! lx #f) (loop))))))

(define (lua-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (cond ((and (valid? t-block-string-end) (scan-block-end p lx))
         (reset-state! p)
         (lexer-set-result! lx t-block-string-end)
         #t)
        ((and (valid? t-block-string-content) (scan-block-content p lx))
         (lexer-set-result! lx t-block-string-content)
         #t)
        ((and (valid? t-block-comment-end) (= (vector-ref p 0) 0)
              (scan-block-end p lx))
         (reset-state! p)
         (lexer-set-result! lx t-block-comment-end)
         #t)
        ((and (valid? t-block-comment-content) (scan-comment-content p lx))
         #t)
        (else
         (scan-skip-spaces! lx #t #t)
         (cond ((and (valid? t-block-string-start) (scan-block-start p lx))
                (lexer-set-result! lx t-block-string-start)
                #t)
               ((valid? t-block-comment-start) (scan-comment-start p lx))
               (else #f)))))

(define-public (ts-lua-scanner)
  (vector lua-create lua-scan lua-deserialize lua-serialize))
