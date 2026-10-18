;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-css.scm
;; DESCRIPTION : the external scanner of tree-sitter-css
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-css (MIT): the descendant operator
;; (white space between selectors) and the colon of the pseudo classes. The
;; scanner has no state; the protocol of the scanners is described in
;; ts-scan.scm.

(texmacs-module (treesit ts-scanner-css)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars) (treesit ts-scan)))

(define t-descendant-op 0)
(define t-pseudo-class-selector-colon 1)
(define t-error-recovery 2)

(define (css-scan p lx valid)
  (define (la) (lexer-lookahead lx))
  (call-with-exit
   (lambda (out)
     (when (vector-ref valid t-error-recovery) (out #f))
     (when (and (ts-wspace? (la)) (vector-ref valid t-descendant-op))
       (lexer-set-result! lx t-descendant-op)
       (lexer-advance! lx #t)
       (scan-skip-spaces! lx #t #t)
       (lexer-mark-end! lx)
       (let ((c (la)))
         (when (or (memv c '(35 46 91 45 42)) (ts-walnum? c))   ; # . [ - *
           (out #t))
         (when (= c 58)   ; :
           (lexer-advance! lx #f)
           (when (ts-wspace? (la)) (out #f))
           (let loop ()
             (cond ((or (= (la) 59) (= (la) 125) (lexer-eof? lx)) (out #f))   ; ; }
                   ((= (la) 123) (out #t))   ; {
                   (else (lexer-advance! lx #f) (loop)))))))
     (when (vector-ref valid t-pseudo-class-selector-colon)
       (scan-skip-spaces! lx #t #t)
       (when (= (la) 58)
         (lexer-advance! lx #f)
         (when (= (la) 58) (out #f))
         (lexer-mark-end! lx)
         (lexer-set-result! lx t-pseudo-class-selector-colon)
         ;; a { makes a pseudo class selector, a ; a property (not in comments)
         (let loop ((in-comment #f))
           (when (and (not (= (la) 59)) (not (= (la) 125)) (not (lexer-eof? lx)))
             (lexer-advance! lx #f)
             (when (and (= (la) 123) (not in-comment)) (out #t))
             (cond ((and (= (la) 47) (not in-comment))
                    (lexer-advance! lx #f)
                    (loop (= (la) 42)))
                   ((and (= (la) 42) in-comment)
                    (lexer-advance! lx #f)
                    (loop (not (= (la) 47))))
                   (else (loop in-comment)))))
         ;; at the end, still a pseudo class selector (for the error recovery)
         (out (lexer-eof? lx))))
     #f)))

(define-public (ts-css-scanner)
  (stateless-scanner css-scan))
