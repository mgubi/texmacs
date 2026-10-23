;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-secure-test.scm
;; DESCRIPTION : Test suite for the secure script checker
;; COPYRIGHT   : (C) 2026  The TeXmacs team
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-secure-test)
  (:use (kernel texmacs tm-secure)))

(define (regtest-secure-accept)
  (regression-test-group
   "secure scripts" "secure-accept"
   secure? :none
   (test "secure function" '(texmacs-version) #t)
   (test "secure function with arguments" '(string-append "a" "b") #t)
   (test "quoted data" '(cons 'system '(system "ls")) #t)
   (test "lambda expression" '(lambda (x) (cons x x)) #t)
   (test "lambda application"
     '((lambda (x) `(concat "Hallo " ,x)) "x") #t)
   (test "with data" '(with x "a" (string-append x "b")) #t)
   (test "with quoted data" '(with answer '(1 2) (car answer)) #t)
   (test "with lambda" '(with f (lambda (x) (cons x x)) (f "a")) #t)
   (test "with secure function" '(with g car (g (list 1 2))) #t)
   (test "secure function as value" '(list car cdr) #t)
   (test "if" '(if (null? (list)) "a" "b") #t)
   (test "cond" '(cond ((== 1 2) "a") (else "b" "c")) #t)
   (test "character in quasiquoted data" '(quasiquote (a #\b)) #t)
   (test "keyword in quasiquoted data" '(quasiquote (a :foo)) #t)))

(define (regtest-secure-reject)
  (regression-test-group
   "insecure scripts" "secure-reject"
   secure? :none
   (test "insecure function" '(system "ls") #f)
   (test "call through lambda parameter" '((lambda (f) (f "ls")) system) #f)
   (test "call through with" '(with f system (f "ls")) #f)
   (test "computed call head" '((car (list system)) "ls") #f)
   (test "insecure global as value" '(cons system 1) #f)
   (test "set!" '(set! car system) #f)
   (test "shadowed secure function" '((lambda (car) (car "ls")) system) #f)
   (test "shadowed special form" '((lambda (if) (if "ls")) system) #f)
   (test "local binding does not leak"
     '((lambda (x) (with x (lambda () 1) 1) (x "ls")) system) #f)
   (test "all cond clause expressions" '(cond (#t 1 (system "ls"))) #f)
   (test "unquote in vector" '(quasiquote #((unquote (system "ls")))) #f)
   (test "improper argument list" '(string-append . system) #f)))

(define (nested-with n)
  (if (== n 0) 1
      `(with f (lambda () ,(nested-with (- n 1))) 1)))

(define (secure-quickly? expr)
  (let* ((start (texmacs-time))
         (ok (secure? expr)))
    (and ok (< (- (texmacs-time) start) 500))))

(define (regtest-secure-speed)
  (regression-test-group
   "secure check speed" "secure-speed"
   :none :none
   (test "nested with, depth 30" (secure-quickly? (nested-with 30)) #t)))

(tm-define (regtest-secure)
  (let ((n (+ (regtest-secure-accept)
              (regtest-secure-reject)
              (regtest-secure-speed))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of secure: ok\n")))
