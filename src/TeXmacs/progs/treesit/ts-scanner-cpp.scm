
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-scanner-cpp.scm
;; DESCRIPTION : the external scanner of tree-sitter-cpp
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A port of src/scanner.c of tree-sitter-cpp (MIT): the delimiter and the
;; content of the raw strings R"delimiter(content)delimiter". The protocol
;; of the scanners is described in ts-scanner-python.scm.

(texmacs-module (treesit ts-scanner-cpp)
  (:use (treesit ts-base) (treesit ts-parse) (treesit ts-chars)))

(define t-raw-string-delimiter 0)
(define t-raw-string-content 1)

(define max-delimiter-length 16)

;; the payload is #(delimiter): the list of the code points of the opening
;; delimiter of the present raw string, '() outside

(define (scan-delimiter p lx)
  (let ((d (vector-ref p 0)))
    (if (pair? d)
        ;; the closing delimiter, the same as the opening one
        (let loop ((l d))
          (cond ((null? l) (vector-set! p 0 '()) #t)
                ((not (= (lexer-lookahead lx) (car l))) #f)
                (else (lexer-advance! lx #f) (loop (cdr l)))))
        ;; the opening one, up to (
        (let loop ((acc '()) (n 0))
          (let ((c (lexer-lookahead lx)))
            (cond ((or (>= n max-delimiter-length) (lexer-eof? lx) (= c 92)
                       (ts-wspace? c))
                   #f)
                  ((= c 40)
                   (vector-set! p 0 (reverse acc))
                   (> n 0))
                  (else (lexer-advance! lx #f) (loop (cons c acc) (+ n 1)))))))))

(define (scan-content p lx)
  (let* ((d (list->vector (vector-ref p 0)))
         (n (vector-length d)))
    ;; i: the progress through the delimiter since the last ), or -1
    (let loop ((i -1))
      (if (lexer-eof? lx)
          (begin (lexer-mark-end! lx) #t)
          (let* ((c (lexer-lookahead lx))
                 (i (cond ((< i 0) i)
                          ((= i n) (if (= c 34) 'done -1))
                          ((= c (vector-ref d i)) (+ i 1))
                          (else -1))))
            (if (eq? i 'done) #t
                (let ((i (if (and (= i -1) (= c 41))
                             (begin (lexer-mark-end! lx) 0)
                             i)))
                  (lexer-advance! lx #f)
                  (loop i))))))))

(define (cpp-scan p lx valid)
  (let ((delim? (vector-ref valid t-raw-string-delimiter))
        (content? (vector-ref valid t-raw-string-content)))
    (cond ((and delim? content?) #f)   ; error recovery
          (delim?
           (lexer-set-result! lx t-raw-string-delimiter)
           (scan-delimiter p lx))
          (content?
           (lexer-set-result! lx t-raw-string-content)
           (scan-content p lx))
          (else #f))))

(define-public (ts-cpp-scanner)
  (vector (lambda () (vector '()))
          cpp-scan
          (lambda (p state) (vector-set! p 0 (or state '())))
          (lambda (p) (let ((d (vector-ref p 0))) (and (pair? d) d)))))
