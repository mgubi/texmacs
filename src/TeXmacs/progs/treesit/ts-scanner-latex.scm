;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-latex.scm
;; DESCRIPTION : the external scanner of tree-sitter-latex
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of latex-lsp/tree-sitter-latex (MIT): the raw
;; content up to \fi or to the end of the environments whose content is not
;; LaTeX (comment, verbatim, lstlisting, minted...). The scanner has no
;; state; the protocol of the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-latex)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-scan)))

;; the keyword which ends the raw content of each external token, in order
(define raw-ends
  (vector "\\fi" "\\end{comment}" "\\end{verbatim}" "\\end{lstlisting}"
          "\\end{minted}" "\\end{asy}" "\\end{asydef}" "\\end{pycode}"
          "\\end{luacode}" "\\end{luacode*}" "\\end{sagesilent}"
          "\\end{sageblock}"))

(define (c-char c)
  ;; a code point stored in a char of C
  (let ((b (modulo c 256))) (if (>= b 128) (- b 256) b)))

(define (find-verbatim lx keyword command-name?)
  ;; up to keyword (not followed by a letter, : _ or @ for a command name);
  ;; whether some content was marked
  (let ((n (string-length keyword)))
    (let loop ((has-marked #f))
      (if (lexer-eof? lx) has-marked
          (let match ((i 0) (advanced #f))
            (cond ((= i n)
                   ;; the keyword matched
                   (if (not command-name?) has-marked
                       (if (lexer-eof? lx) has-marked
                           (let ((c (c-char (lexer-lookahead lx))))
                             (if (or (memv c '(58 95 64))   ; : _ @
                                     (and (>= c 97) (<= c 122)) (and (>= c 65) (<= c 90)))
                                 (begin (lexer-mark-end! lx) (loop #t))
                                 has-marked)))))
                  ((lexer-eof? lx) has-marked)
                  ((not (= (lexer-lookahead lx) (char->integer (string-ref keyword i))))
                   (if advanced
                       (loop has-marked)
                       (begin
                         (lexer-advance! lx #f)
                         (lexer-mark-end! lx)
                         (loop #t))))
                  (else
                   (lexer-advance! lx #f)
                   (match (+ i 1) #t))))))))

(define (latex-scan p lx valid)
  ;; exactly one of the tokens may be valid
  (let loop ((i 0) (type #f))
    (cond ((< i (vector-length raw-ends))
           (if (vector-ref valid i)
               (if type #f (loop (+ i 1) i))
               (loop (+ i 1) type)))
          ((not type) (lexer-set-result! lx 65535) #f)
          (else
           (lexer-set-result! lx type)
           (find-verbatim lx (vector-ref raw-ends type) (= type 0))))))

(define-public (ts-latex-scanner)
  (stateless-scanner latex-scan))
