;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-typescript.scm
;; DESCRIPTION : the external scanner of tree-sitter-typescript
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; The scanner of tree-sitter-typescript: Copyright (c) 2017 Max Brunsfeld,
;; under the MIT license: see $TEXMACS_PATH/misc/tree-sitter/typescript/LICENSE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of common/scanner.h of tree-sitter-typescript (MIT), the scanner
;; of typescript and of tsx: the automatic semicolons (also after function
;; signatures), the characters of template strings, the ? of conditional
;; expressions, the HTML comments and the text of JSX. The scanner has no
;; state; the protocol of the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-typescript)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars) (treesit ts-scan)))

(define t-automatic-semicolon 0)
(define t-template-chars 1)
(define t-ternary-qmark 2)
(define t-html-comment 3)
(define t-logical-or 4)
(define t-escape-sequence 5)
(define t-regex-pattern 6)
(define t-jsx-text 7)
(define t-function-signature-automatic-semicolon 8)
(define t-error-recovery 9)

(define (scan-template-chars lx)
  (lexer-set-result! lx t-template-chars)
  (let loop ((has-content #f))
    (lexer-mark-end! lx)
    (let ((c (lexer-lookahead lx)))
      (cond ((= c 96) has-content)   ; `
            ((= c 0) #f)
            ((= c 36)   ; $
             (lexer-advance! lx #f)
             (if (= (lexer-lookahead lx) 123) has-content (loop #t)))
            ((= c 92) has-content)   ; \
            (else (scan-advance-until! lx "`$\\" #f) (loop #t))))))

(define (scan-whitespace-and-comments lx comment)
  ;; comment: a box (vector) set when a line comment is seen
  (let loop ()
    (scan-skip-spaces! lx #t #t)
    (if (not (= (lexer-lookahead lx) 47)) #t   ; /
        (begin
          (lexer-advance! lx #t)
          (cond ((= (lexer-lookahead lx) 47)
                 (lexer-advance! lx #t)
                 (scan-advance-until! lx "\n" #t)
                 (vector-set! comment 0 #t)
                 (loop))
                ((= (lexer-lookahead lx) 42)   ; *
                 (lexer-advance! lx #t)
                 (let block ()
                   (let ((c (lexer-lookahead lx)))
                     (cond ((= c 0) #t)
                           ((= c 42)
                            (lexer-advance! lx #t)
                            (if (= (lexer-lookahead lx) 47)
                                (lexer-advance! lx #t)
                                (block)))
                           (else (lexer-advance! lx #t) (block)))))
                 (loop))
                (else #f))))))

(define (scan-automatic-semicolon lx valid comment)
  (define (la) (lexer-lookahead lx))
  (define (skip!) (lexer-advance! lx #t))
  (lexer-set-result! lx t-automatic-semicolon)
  (lexer-mark-end! lx)
  (call-with-exit
   (lambda (out)
     (let loop ()
       (cond ((= (la) 0) (out #t))
             ((= (la) 125)   ; }
              ;; not before a type annotation: ({a}: {a: number}) => number
              (skip!)
              (scan-skip-spaces! lx #t #t)
              (out (if (= (la) 58) (vector-ref valid t-logical-or) #t)))
             ((not (ts-wspace? (la))) (out #f))
             ((= (la) 10) #t)
             (else (skip!) (loop))))
     (skip!)
     (unless (scan-whitespace-and-comments lx comment) (out #f))
     (let ((c (la)))
       (cond ((memv c '(96 44 46 59 42 37 62 60 61 63 94 124 38 47 58)) #f)
             ((= c 123)   ; {
              (not (vector-ref valid t-function-signature-automatic-semicolon)))
             ((or (= c 40) (= c 91))   ; ( [ : not in a type
              (not (vector-ref valid t-logical-or)))
             ((= c 43) (skip!) (= (la) 43))   ; ++, not binary +
             ((= c 45) (skip!) (= (la) 45))
             ((= c 33) (skip!) (not (= (la) 61)))   ; not !=
             ((= c 105)   ; not in or instanceof
              (skip!)
              (cond ((not (= (la) 110)) #t)
                    (else
                     (skip!)
                     (if (not (ts-walpha? (la))) #f
                         (let word ((l (string->list "stanceof")))
                           ;; instanceof itself: no semicolon
                           (cond ((null? l) (ts-walpha? (la)))
                                 ((not (= (la) (char->integer (car l)))) #t)
                                 (else (skip!) (word (cdr l)))))))))
             (else #t))))))

(define (scan-ternary-qmark lx)
  (define (la) (lexer-lookahead lx))
  (scan-skip-spaces! lx #t #t)
  (and (= (la) 63)
       (begin
         (lexer-advance! lx #f)
         ;; not optional chaining
         (and (not (= (la) 63)) (not (= (la) 46))
              (begin
                (lexer-mark-end! lx)
                (lexer-set-result! lx t-ternary-qmark)
                ;; the optional arguments of TypeScript: ?:, maybe with spaces
                (scan-skip-spaces! lx #f #t)
                (cond ((memv (la) '(58 41 44)) #f)
                      ((= (la) 46) (lexer-advance! lx #f) (ts-wdigit? (la)))
                      (else #t)))))))

(define (js-line-end? c) (or (= c 0) (= c 10) (= c #x2028) (= c #x2029)))

(define (scan-closing-comment lx)
  (define (la) (lexer-lookahead lx))
  (let loop ()
    (when (or (ts-wspace? (la)) (= (la) #x2028) (= (la) #x2029))
      (lexer-advance! lx #t)
      (loop)))
  (and (cond ((= (la) 60) (scan-word! lx "<!--"))
             ((= (la) 45) (scan-word! lx "-->"))
             (else #f))
       (begin
         (let loop ()
           (unless (js-line-end? (la)) (lexer-advance! lx #f) (loop)))
         (lexer-set-result! lx t-html-comment)
         (lexer-mark-end! lx)
         #t)))

(define (scan-jsx-text lx)
  (let loop ((saw-text #f) (at-newline #f))
    (let ((c (lexer-lookahead lx)))
      (if (or (= c 0) (memv c '(60 62 123 125 38)))   ; < > { } &
          (begin (lexer-set-result! lx t-jsx-text) saw-text)
          (let* ((ws (ts-wspace? c))
                 (nl (if (= c 10) #t (and at-newline ws)))
                 (saw (or saw-text (and (not (= c 10)) (not nl)))))
            (lexer-advance! lx #f)
            (loop saw nl))))))

(define (typescript-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (cond ((valid? t-template-chars)
         (and (not (valid? t-automatic-semicolon)) (scan-template-chars lx)))
        ((and (valid? t-jsx-text) (scan-jsx-text lx)) #t)
        ((or (valid? t-automatic-semicolon)
             (valid? t-function-signature-automatic-semicolon))
         (let* ((comment (vector #f))
                (r (scan-automatic-semicolon lx valid comment)))
           (if (and (not r) (not (vector-ref comment 0)) (valid? t-ternary-qmark)
                    (= (lexer-lookahead lx) 63))
               (scan-ternary-qmark lx)
               r)))
        ((valid? t-ternary-qmark) (scan-ternary-qmark lx))
        ((and (valid? t-html-comment) (not (valid? t-logical-or))
              (not (valid? t-escape-sequence)) (not (valid? t-regex-pattern)))
         (scan-closing-comment lx))
        (else #f)))

(define-public (ts-typescript-scanner)
  (stateless-scanner typescript-scan))
