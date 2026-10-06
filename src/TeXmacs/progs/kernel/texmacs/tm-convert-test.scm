
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-convert-test.scm
;; DESCRIPTION : Test suite for tm-convert
;; COPYRIGHT   : (C) 2022  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(texmacs-module (kernel texmacs tm-convert-test)
  (:use (kernel texmacs tm-define)))

(define (regtest-format?)
  (regression-test-group
   "format?" "boolean"
   format? :none
   (test "python format" "python" #t)
   (test "scala format" "scala" #t)
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-get-name)
  (regression-test-group
   "format-get-name" "string"
   format-get-name :none
   (test "python format" "python" "Python source code")
   (test "scala format" "scala" "Scala source code")
   (test "no such format" "no-such-format" #f)))

(define (regtest-format-from-suffix)
  (regression-test-group
   "format-from-suffix" "string"
   format-from-suffix :none
   (test "scheme format" "scm" "scheme")
   (test "python format" "py" "python")
   (test "java format" "java" "java")
   (test "scala format" "scala" "scala")
   (test "julia format" "jl" "julia")
   (test "cpp format" "cpp" "cpp")
   (test "cpp format" "hpp" "cpp")
   (test "cpp format" "cc" "cpp")
   (test "cpp format" "hh" "cpp")
   (test "mathemagix format" "mmx" "mathemagix")
   (test "mathemagix format" "mmh" "mathemagix")
   (test "scilab format" "sci" "scilab")
   (test "scilab format" "sce" "scilab")
   (test "texmacs format" "tm" "texmacs")
   (test "texmacs format" "ts" "texmacs")
   (test "texmacs format" "tmml" "tmml")
   (test "texmacs format" "stm" "stm")
   (test "png format" "png" "png")
   (test "no such format" "no-such-format" "generic")))

;; a document of the TeXmacs file system (the help pages...) is read back by
;; the reader of the TeXmacs Scheme format: its strings must come back as
;; they were, backslashes and quotes included
(define tmfs-samples
  (list "two \\\\ backslashes" "backslash-quote \\\" here" "trailing backslash \\"
        "quote \" only" "hex \\x41 escape" "plain"))

(tmfs-load-handler (regtest-tmfs name)
  `(document (TeXmacs ,(texmacs-version))
             (body (document ,(list-ref tmfs-samples (string->number name))))))

(define (tmfs-sample-back i)
  (let* ((t (tree->stree (stm->texmacs (tmfs-load (string-append "tmfs://regtest-tmfs/"
                                                                  (number->string i))))))
         (b (assoc 'body (cdr t))))
    (cadr (cadr b))))

(define (regtest-tmfs-strings)
  (regression-test-group
   "tmfs documents" "strings"
   tmfs-sample-back :none
   (test "two backslashes" 0 (list-ref tmfs-samples 0))
   (test "backslash and quote" 1 (list-ref tmfs-samples 1))
   (test "trailing backslash" 2 (list-ref tmfs-samples 2))
   (test "quote" 3 (list-ref tmfs-samples 3))
   (test "backslash, x and hex digits" 4 (list-ref tmfs-samples 4))
   (test "plain" 5 (list-ref tmfs-samples 5))))

(define (regtest-object->tmstring)
  (regression-test-group
   "object->tmstring" "string"
   object->tmstring :none
   ;; a backslash of the string followed by "x41" is not an escape
   (test "backslash, x and hex digits" "\\x41" "\"\\\\x41\"")))

(tm-define (regtest-tm-convert)
  (let ((n (+ (regtest-format?)
              (regtest-format-get-name)
              (regtest-format-from-suffix)
              (regtest-tmfs-strings)
              (regtest-object->tmstring))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-convert: ok\n")))
