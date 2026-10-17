;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-javascript.scm
;; DESCRIPTION : the external scanner of tree-sitter-javascript
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-javascript (MIT): the automatic
;; semicolons, the characters of template strings, the ? of the conditional
;; expressions, the HTML comments and the text of JSX. The scanner has no
;; state. The protocol of the scanners is described in ts-scan.scm.

(texmacs-module (treesit ts-scanner-javascript)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars)
        (treesit ts-scan)))

(define-external-tokens automatic-semicolon template-chars ternary-qmark
  html-comment logical-or escape-sequence regex-pattern jsx-text)

(define (js-newline? c) (or (= c 10) (= c #x2028) (= c #x2029)))
(define (js-char c) (char->integer c))

(define (scan-template-chars lx)
  (lexer-set-result! lx t-template-chars)
  (let loop ((has-content #f))
    (lexer-mark-end! lx)
    (let ((c (lexer-lookahead lx)))
      (cond ((= c (js-char #\`)) has-content)
            ((= c 0) #f)
            ((= c (js-char #\$))
             (lexer-advance! lx #f)
             (if (= (lexer-lookahead lx) (js-char #\{)) has-content (loop #t)))
            ((= c (js-char #\\)) has-content)
            (else (scan-advance-until! lx "`$\\" #f) (loop #t))))))

;; the results of scan-whitespace-and-comments
(define ws-reject 0)
(define ws-no-newline 1)
(define ws-accept 2)

(define (scan-whitespace-and-comments lx comment consume?)
  ;; comment is a box (a vector of one boolean) set when a comment is seen
  (define (la) (lexer-lookahead lx))
  (define (skip!) (lexer-advance! lx #t))
  (let loop ((saw-block-newline #f))
    (scan-skip-spaces! lx #t #t)
    (if (not (= (la) (js-char #\/))) ws-accept
        (begin
          (skip!)
          (cond ((= (la) (js-char #\/))
                 (skip!)
                 (let line ()
                   (unless (or (= (la) 0) (js-newline? (la)))
                     (skip!) (line)))
                 (vector-set! comment 0 #t)
                 (loop saw-block-newline))
                ((= (la) (js-char #\*))
                 (skip!)
                 (let block ((saw saw-block-newline))
                   (cond ((= (la) 0) (loop saw))
                         ((= (la) (js-char #\*))
                          (skip!)
                          (if (= (la) (js-char #\/))
                              (begin
                                (skip!)
                                (vector-set! comment 0 #t)
                                (if (and (not (= (la) (js-char #\/))) (not consume?))
                                    (if saw ws-accept ws-no-newline)
                                    (loop saw)))
                              (block saw)))
                         ((js-newline? (la)) (skip!) (block #t))
                         (else (skip!) (block saw)))))
                (else ws-reject))))))

(define (scan-automatic-semicolon lx comment-condition comment)
  (define (la) (lexer-lookahead lx))
  (define (la? c) (= (la) (js-char c)))
  (define (skip!) (lexer-advance! lx #t))
  (lexer-set-result! lx t-automatic-semicolon)
  (lexer-mark-end! lx)
  ;; up to the newline: 'yes, 'no or 'newline
  (let ((r (let loop ()
             (cond ((= (la) 0) 'yes)
                   ((and (la? #\/)
                         (let ((w (scan-whitespace-and-comments lx comment #f)))
                           (cond ((= w ws-reject) 'no)
                                 ((and (= w ws-accept) comment-condition
                                       (not (la? #\,)) (not (la? #\=)))
                                  'yes)
                                 (else #f))))
                    => identity)
                   ((la? #\}) 'yes)
                   ((js-newline? (la)) 'newline)
                   ((not (ts-wspace? (la))) 'no)
                   (else (skip!) (loop))))))
    (cond ((eq? r 'yes) #t)
          ((eq? r 'no) #f)
          (else
           (skip!)
           (if (= (scan-whitespace-and-comments lx comment #t) ws-reject) #f
               (let ((c (la)))
                 (cond ((memv c (map js-char (string->list "`,:;*%><=[(?^|&/")))
                        #f)
                       ((= c (js-char #\.)) (skip!) (ts-wdigit? (la)))
                       ((= c (js-char #\+)) (skip!) (la? #\+))
                       ((= c (js-char #\-)) (skip!) (la? #\-))
                       ((= c (js-char #\!)) (skip!) (not (la? #\=)))
                       ((= c (js-char #\i))
                        (skip!)
                        (cond ((not (la? #\n)) #t)
                              (else
                               (skip!)
                               (if (not (ts-walpha? (la))) #f
                                   (let loop ((l (string->list "stanceof")))
                                     (cond ((null? l) (not (not (ts-walpha? (la)))))
                                           ((not (la? (car l))) #t)
                                           (else (skip!) (loop (cdr l)))))))))
                       (else #t))))))))

(define (scan-ternary-qmark lx)
  (define (la) (lexer-lookahead lx))
  (scan-skip-spaces! lx #t #t)
  (and (= (la) (js-char #\?))
       (begin
         (lexer-advance! lx #f)
         (and (not (= (la) (js-char #\?)))
              (begin
                (lexer-mark-end! lx)
                (lexer-set-result! lx t-ternary-qmark)
                (if (= (la) (js-char #\.))
                    (begin (lexer-advance! lx #f) (ts-wdigit? (la)))
                    #t))))))

(define (scan-html-comment lx)
  (define (la) (lexer-lookahead lx))
  (scan-skip-spaces! lx #t #t)
  (and (cond ((= (la) (js-char #\<)) (scan-word! lx "<!--"))
             ((= (la) (js-char #\-)) (scan-word! lx "-->"))
             (else #f))
       (begin
         (let line ()
           (unless (or (= (la) 0) (js-newline? (la)))
             (lexer-advance! lx #f) (line)))
         (lexer-set-result! lx t-html-comment)
         (lexer-mark-end! lx)
         #t)))

(define (scan-jsx-text lx)
  (define (la) (lexer-lookahead lx))
  (let loop ((saw-text #f) (at-newline #f))
    (let ((c (la)))
      (if (or (= c 0) (memv c (map js-char (string->list "<>{}&"))))
          (begin (lexer-set-result! lx t-jsx-text) saw-text)
          (let* ((ws (ts-wspace? c))
                 (nl (if (= c 10) #t (and at-newline ws)))
                 (saw (or saw-text (and (not (= c 10)) (not nl)))))
            (lexer-advance! lx #f)
            (loop saw nl))))))

(define (javascript-scan p lx valid)
  (define (valid? i) (vector-ref valid i))
  (cond ((valid? t-template-chars)
         (and (not (valid? t-automatic-semicolon)) (scan-template-chars lx)))
        ((and (valid? t-jsx-text) (scan-jsx-text lx)) #t)
        ((valid? t-automatic-semicolon)
         (let* ((comment (vector #f))
                (r (scan-automatic-semicolon lx (not (valid? t-logical-or)) comment)))
           (if (and (not r) (not (vector-ref comment 0)) (valid? t-ternary-qmark)
                    (= (lexer-lookahead lx) (js-char #\?)))
               (scan-ternary-qmark lx)
               r)))
        ((valid? t-ternary-qmark) (scan-ternary-qmark lx))
        ((and (valid? t-html-comment) (not (valid? t-logical-or))
              (not (valid? t-escape-sequence)) (not (valid? t-regex-pattern)))
         (scan-html-comment lx))
        (else #f)))

(define-public (ts-javascript-scanner)
  (stateless-scanner javascript-scan))
