
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : output.scm
;; DESCRIPTION : generation of indented and hyphenated output
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define output-accu '())
(define output-comment #f)
(define output-indentation 0)
(define output-count 0)
(define output-start-flag #t)
(define output-space-flag #f)
(define output-break-flag #t)
(define output-tail '())
(define output-exact #f)
(define output-line-length 79)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The output machinery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (output-set-exact x)
  (with old output-exact
    (set! output-exact x)
    old))

(tm-define (output-set-line-length x)
  (with old output-line-length
    (set! output-line-length x)
    old))

(tm-define (output-produce)
  (output-flush)
  (let ((r (apply string-append (reverse output-accu))))
    (set! output-accu '())
    (set! output-comment #f)
    (set! output-indentation 0)
    (set! output-count 0)
    (set! output-start-flag #t)
    (set! output-space-flag #f)
    (set! output-break-flag #t)
    (set! output-tail '())
;;    (display* "OUTPUT\n" r "\n")
    r))

(tm-define (get-output-start-flag)
  output-start-flag)

(tm-define (get-output-comment)
  output-comment)

(tm-define (set-output-comment comment)
  (set! output-comment comment))

(tm-define (get-output-indent)
  output-indentation)

(tm-define (set-output-indent indent)
  (set! output-indentation indent))

(tm-define (output-indent plus)
  (output-flush)
  (set! output-indentation (+ output-indentation plus)))

(define (output-return)
  (set! output-start-flag #t)
  (with indent (max 0 (min 40 output-indentation))
    (let ((s (if output-exact "" (make-string indent #\space)))
          (c (if output-comment "% " "")))
      (set! output-accu (cons (string-append "\n" s c) output-accu))
      (set! output-count indent))))

(define (output-raw s)
  (if (!= s "")
      (begin
	(set! output-start-flag #f)
	(set! output-break-flag #t)))
  (set! output-accu (cons s output-accu)))

(define (output-prepared s)
  (if (or (!= s "") output-space-flag)
      (begin
	(if output-space-flag (set! output-count (+ output-count 1)))
	(set! output-count (+ output-count (string-length s)))
	(if output-space-flag (set! output-break-flag #t))
	(if (or (< output-count output-line-length) output-start-flag
		(not output-break-flag) output-exact)
	    (begin
	      (if (and output-space-flag (not output-start-flag))
		  (output-raw " "))
	      (output-raw s))
	    (begin
	      (output-return)
	      (set! output-count (+ output-count (string-length s)))
	      (output-raw s)))
	(set! output-space-flag #f))))

;; The text after the last space, which is not output yet (it is output as
;; one word with the text up to the next space): output-tail is the list of
;; its pieces, the last one first, none of them empty. The text was one
;; string, made again with each piece added to it and copied again into C++
;; to look for a space (string-search-forwards): the output of a text with
;; no space was quadratic in its length, and the HTML export of a formula of
;; 1 MB for MathJax (a macro which calls itself, which s7 converts whole)
;; took minutes. A space is now looked for in each new piece only, from
;; where the last one was (string-index copies nothing)

(define (output-tail-string)
  ;; the text of output-tail, which is made one piece
  (cond ((null? output-tail) "")
        ((null? (cdr output-tail)) (car output-tail))
        (else (with s (apply string-append (reverse output-tail))
                (set! output-tail (list s))
                s))))

(define (output-set-tail s)
  (set! output-tail (if (== s "") '() (list s))))

(define (output-take-tail s)
  ;; the text of output-tail followed by @s, and output-tail emptied
  (with r (if (null? output-tail) s
              (apply string-append (reverse (cons s output-tail))))
    (set! output-tail '())
    r))

(define (output-sub s i)
  ;; the string @s from @i: each word before a space is output
  (with pos (string-index s #\space i)
    (if pos
	(begin
	  (output-prepared (output-take-tail (substring s i pos)))
	  (set! output-space-flag #t)
	  (output-sub s (+ pos 1)))
	(when (< i (string-length s))
	  (set! output-tail
		(cons (if (== i 0) s (substring s i (string-length s)))
		      output-tail))))))

(define (output-tail-ends? l s n)
  ;; do the pieces @l end with the first @n characters of @s
  (or (<= n 0)
      (and (pair? l)
           (let* ((p (car l))
                  (k (string-length p))
                  (m (min k n)))
             (and (== (substring p (- k m) k) (substring s (- n m) n))
                  (output-tail-ends? (cdr l) s (- n m)))))))

(define (output-tail-longer? l n)
  ;; do the pieces @l have at least @n characters
  (or (<= n 0)
      (and (pair? l)
           (output-tail-longer? (cdr l) (- n (string-length (car l)))))))

(define (output-tail-drop l n)
  ;; the pieces @l without their last @n characters (they have as many)
  (if (<= n 0) l
      (let* ((p (car l))
             (k (string-length p)))
        (if (> k n)
            (cons (substring p 0 (- k n)) (cdr l))
            (output-tail-drop (cdr l) (- n k))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (output-test-end? s)
  ;; to be used with extreme care
  (output-tail-ends? output-tail s (string-length s)))

(tm-define (output-remove n)
  ;; to be used with extreme care
  (when (output-tail-longer? output-tail n)
    (set! output-tail (output-tail-drop output-tail n))))

(define (trim-right-spaces s)
  (if (string-ends? s " ")
      (trim-right-spaces (string-drop-right s 1))
      s))

(tm-define (output-remove-indentation)
  ;; to be used with extreme care
  (if (null? output-tail)
      (when (nnull? output-accu)
        (with s (trim-right-spaces (car output-accu))
          (cond ((== s "")
                 (set! output-accu (cdr output-accu))
                 (set! output-count (- 1 output-count))
                 (output-remove-indentation))
                ((string-ends? s "\n")
                 (set! output-accu (cons s (cdr output-accu)))
                 (set! output-count (- 1 output-count))))))
      (with s (trim-right-spaces (output-tail-string))
        (cond ((== s "")
               (set! output-tail '())
               (output-remove-indentation))
              ((string-ends? s "\n")
               (output-set-tail s))))))

(tm-define (output-flush)
  (output-prepared (output-take-tail "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (output-verb . ss)
  ;;(display-err* "Output verb " ss "\n")
  (if (nnull? output-tail) (output-flush))
  (output-prepared (apply string-append ss)))

(tm-define (output-lf-verbatim . ss)
  ;;(display-err* "Output lf verbatim " ss "\n")
  (output-flush)
  (if (not output-start-flag) (output-raw "\n"))
  (output-raw (apply string-append ss))
  (set! output-break-flag #f))

(tm-define (output-invariant . ss)
  ;;(display-err* "Output invariant " ss "\n")
  (output-remove-indentation)
  (apply output-lf-verbatim ss))

(tm-define (output-verbatim . ss)
  ;;(display-err* "Output verbatim " ss "\n")
  (output-flush)
  (output-raw (apply string-append ss))
  (set! output-break-flag #f))

(tm-define (output-lf)
  ;;(display-err* "Output lf\n")
  (if (nnull? output-tail) (output-flush))
  (output-return))

(tm-define (output-text . ss)
  ;;(display-err* "Output text " ss "\n")
  (for-each (lambda (s) (output-sub s 0)) ss))

(tm-define (output-marker s)
  ;;(display-err* "Output marker " s "\n")
  (if (nnull? output-tail) (output-flush))
  (set! output-accu (cons s output-accu)))
