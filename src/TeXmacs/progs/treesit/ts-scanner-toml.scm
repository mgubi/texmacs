;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-toml.scm
;; DESCRIPTION : the external scanner of tree-sitter-toml
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; The scanner of tree-sitter-toml: Copyright (c) Ika <ikatyang@gmail.com> (https://github.com/ikatyang),
;; under the MIT license: see $TEXMACS_PATH/misc/tree-sitter/toml/LICENSE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-grammars/tree-sitter-toml (MIT):
;; the ends of lines and the ends of the multi-line strings. The scanner
;; has no state; the protocol of the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-toml)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-scan)))

(define t-line-ending-or-eof 0)
(define t-multiline-basic-string-content 1)
(define t-multiline-basic-string-end 2)
(define t-multiline-literal-string-content 3)
(define t-multiline-literal-string-end 4)

(define (scan-multiline-string-end lx valid delimiter content end)
  ;; at most three delimiters: the end of the string, or content
  (and (vector-ref valid end)
       (= (lexer-lookahead lx) delimiter)
       (begin
         (lexer-advance! lx #f)
         (lexer-mark-end! lx)
         (cond ((not (= (lexer-lookahead lx) delimiter))
                (lexer-set-result! lx content))
               (else
                (lexer-advance! lx #f)
                (if (not (= (lexer-lookahead lx) delimiter))
                    (begin (lexer-mark-end! lx) (lexer-set-result! lx content))
                    (begin
                      (lexer-advance! lx #f)
                      (if (not (= (lexer-lookahead lx) delimiter))
                          (begin (lexer-mark-end! lx) (lexer-set-result! lx end))
                          (lexer-set-result! lx content))))))
         #t)))

(define (toml-scan p lx valid)
  (cond ((or (scan-multiline-string-end lx valid 34 t-multiline-basic-string-content
                                        t-multiline-basic-string-end)
             (scan-multiline-string-end lx valid 39 t-multiline-literal-string-content
                                        t-multiline-literal-string-end))
         #t)
        ((vector-ref valid t-line-ending-or-eof)
         (lexer-set-result! lx t-line-ending-or-eof)
         (scan-advance-while! lx " \t" #t)
         (cond ((or (= (lexer-lookahead lx) 0) (= (lexer-lookahead lx) 10)) #t)
               ((= (lexer-lookahead lx) 13)
                (lexer-advance! lx #t)
                (= (lexer-lookahead lx) 10))
               (else #f)))
        (else #f)))

(define-public (ts-toml-scanner)
  (stateless-scanner toml-scan))
