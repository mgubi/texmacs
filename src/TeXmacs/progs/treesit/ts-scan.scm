;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scan.scm
;; DESCRIPTION : what the external scanners of tree-sitter have in common
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An external scanner, for ts-parse.scm and for the C runtime
;; (Plugins/TreeSitter/tree_sitter_glue.cpp), is a vector of four procedures
;;
;;   #(create scan deserialize serialize)
;;
;; create gives the payload, (scan payload lexer valid) tells whether a token
;; was found (valid is the vector of the external tokens which the parser
;; accepts, by their index in the externals of the grammar) and sets the
;; result with lexer-set-result!, (serialize payload) is the state as a value
;; which equal? compares, which is not modified afterwards, #f for the
;; initial state, and (deserialize payload state) restores it.
;;
;; This module makes such vectors (record-scanner, stateless-scanner) and
;; gives the loops over characters which the scanners share. On the lexer of the C runtime, the loops run in C. The
;; loops stop at the lookahead 0, the end of the text or a null character.
;;
;; To leave a scan early, use call-with-exit (call/cc is costly under S7;
;; the escape is not called return: under femtolisp, return is a special
;; form, which returns from the innermost function).

(texmacs-module (treesit ts-scan)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scanners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The scanners name their tokens with plain definitions, (define t-newline
;; 0)..., in the order of the externals of the grammar: under femtolisp, a
;; module finds its private names in its source before macros are expanded,
;; so definitions made by a macro would be global and clash between scanners.

(define-public (record-scanner init scan)
  ;; the payload is a copy of the vector init, of values which are not
  ;; modified (scan replaces them with vector-set!); the state is the list
  ;; of the fields
  (let ((initial (vector->list init)))
    (vector (lambda () (list->vector initial))
            scan
            (lambda (p state)
              (let loop ((i 0) (l (or state initial)))
                (when (pair? l)
                  (vector-set! p i (car l))
                  (loop (+ i 1) (cdr l)))))
            (lambda (p)
              (let ((l (vector->list p)))
                (and (not (equal? l initial)) l))))))

(define-public (stateless-scanner scan)
  (vector (lambda () #f) scan (lambda (p state) #t) (lambda (p) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (scan-char? lx c)
  ;; is the lookahead the character c?
  (= (lexer-lookahead lx) (char->integer c)))

(define (in-chars? c chars)
  ;; is the code point c one of the (ASCII) characters of the string chars?
  (and (< c 128)
       (let ((n (string-length chars)))
         (let loop ((i 0))
           (and (< i n)
                (or (= c (char->integer (string-ref chars i)))
                    (loop (+ i 1))))))))

(define (advance-over! lx chars in? skip?)
  (let loop ((k 0))
    (let ((c (lexer-lookahead lx)))
      (if (or (= c 0) (not (eq? (in-chars? c chars) in?))) k
          (begin (lexer-advance! lx skip?) (loop (+ k 1)))))))

(define-public (scan-advance-while! lx chars skip?)
  ;; advance over the characters of the string chars (ASCII); their number
  (if (vector? lx) (advance-over! lx chars #t skip?)
      (tree-sitter-lexer-advance-while chars skip?)))

(define-public (scan-advance-until! lx chars skip?)
  ;; advance up to one of the characters of chars (ASCII) or to the end;
  ;; the number of characters advanced over
  (if (vector? lx) (advance-over! lx chars #f skip?)
      (tree-sitter-lexer-advance-until chars skip?)))

(define-public (scan-skip-spaces! lx skip? newlines?)
  ;; advance over white space (ts-wspace?), newlines included if newlines?
  (if (not (vector? lx)) (tree-sitter-lexer-skip-spaces skip? newlines?)
      (let loop ((k 0))
        (let ((c (lexer-lookahead lx)))
          (if (and (ts-wspace? c) (or newlines? (not (= c 10))))
              (begin (lexer-advance! lx skip?) (loop (+ k 1)))
              k)))))

(define-public (scan-word! lx s)
  ;; advance over the characters of the string s as long as they match;
  ;; whether they all did
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((= i n) #t)
            ((scan-char? lx (string-ref s i)) (lexer-advance! lx #f) (loop (+ i 1)))
            (else #f)))))

(define-public (scan-accept! lx t)
  ;; the token t ends here
  (lexer-mark-end! lx)
  (lexer-set-result! lx t)
  #t)
