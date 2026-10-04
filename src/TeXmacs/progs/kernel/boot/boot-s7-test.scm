
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot-s7-test.scm
;; DESCRIPTION : Test suite for the S7 specific boot code: reader settings,
;;               error handling, macros, modules and symbol lookup
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot boot-s7-test)
  (:use (kernel boot srfi) (kernel logic logic-bind)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader and evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-boot-reader)
  (regression-test-group
   "boot-s7, reader and evaluation" "reader"
   :none :none
   (test "'x reads as (quote x)" (car ''x) 'quote)
   (test "quoted forms compare with the quote symbol" (== (car ''x) 'quote) #t)
   (test "free-variable? recognizes quoted symbols" (free-variable? ''x) #t)
   (test "free-variable? rejects plain symbols" (free-variable? 'x) #f)
   (test "quasiquote" (let ((x 1) (l '(2 3))) `(a ,x ,@l)) '(a 1 2 3))
   (test "nested quasiquote" `(1 `(2 ,(3 ,(+ 1 3)))) '(1 `(2 ,(3 4))))
   (test "multiple values splice into calls" (+ 1 (values 2 3)) 6)
   (test "empty values are dropped by map"
         (map (lambda (x) (if (odd? x) x (values))) '(1 2 3)) '(1 3))
   (test "call-with-values"
         (call-with-values (lambda () (values 1 2)) list) '(1 2))
   (test "tm-eval evaluates in the user module" (tm-eval '(+ 1 2)) 3)
   (test "eval-string" (eval-string "(* 6 7)") 42)
   (test "strings keep NUL characters"
         (string-length (string #\a (integer->char 0) #\b)) 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error handling: Guile style catch handlers on top of S7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (catch-guile thunk)
  ;; call thunk and return the handler arguments in Guile's convention
  (catch #t thunk (lambda (key subr msg args) (list key msg args))))

(define (regtest-boot-catch)
  (regression-test-group
   "boot-s7, catch adapter" "catch"
   :none :none
   (test "no error" (catch #t (lambda () 42) (lambda args 'error)) 42)
   (test "error with a message and arguments"
         (catch-guile (lambda () (error 'my-error "bad ~A" 1)))
         '(my-error "bad ~A" (1)))
   (test "throw without arguments"
         (catch #t (lambda () (throw 'my-key)) (lambda (key . rest) key))
         'my-key)
   (test "throw without arguments, Guile handler"
         (catch-guile (lambda () (throw 'my-key)))
         '(my-key "" ()))
   (test "throw with arguments"
         (catch-guile (lambda () (throw 'my-key "msg" 2 3)))
         '(my-key "msg" (2 3)))
   (test "errors from primitives"
         (car (catch-guile (lambda () (car 1))))
         'wrong-type-arg)
   (test "texmacs-error"
         (car (catch-guile (lambda () (texmacs-error "where" "what"))))
         'texmacs-error)
   (test "catch with a specific key"
         (catch 'my-key (lambda () (throw 'my-key 1)) (lambda args 'caught))
         'caught)
   (test "other keys are passed to enclosing handlers"
         (catch 'outer
           (lambda ()
             (catch 'inner (lambda () (throw 'outer 1)) (lambda args 'inner)))
           (lambda args 'outer))
         'outer)
   (test "lazy-catch" (lazy-catch #t (lambda () (throw 'k)) (lambda args 'lazy))
         'lazy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (boot-test-twice x) `(* 2 ,x))
(define-public-macro (boot-test-pair a b) `(cons ,a ,b))
(tm-define-macro (boot-test-tm-macro a b) `(list 'expanded ,a ,b))

(define (boot-test-template l)
  ;; macro names inside quasiquoted templates must not be expanded
  `(boot-test-tm-macro ,@l))

(define (boot-test-template* x y)
  `(boot-test-tm-macro ,x ,y))

(define (boot-test-use-macro-in-body n)
  (let ((f (case-lambda ((x) (list 'one x))
                        ((x y) (list 'two x y))
                        ((x y . z) (list 'many x y z)))))
    (list (f n) (f n n) (f n n n))))

(define (regtest-boot-macros)
  (regression-test-group
   "boot-s7, macros" "macros"
   :none :none
   (test "define-macro" (boot-test-twice 21) 42)
   (test "define-public-macro" (boot-test-pair 1 2) '(1 . 2))
   (test "tm-define-macro" (boot-test-tm-macro 1 2) '(expanded 1 2))
   (test "macros are run-time macros" (macro? boot-test-twice) #t)
   (test "macro name inside a quasiquoted template"
         (boot-test-template '(1 2)) '(boot-test-tm-macro 1 2))
   (test "macro name inside a quasiquoted template, unquotes"
         (boot-test-template* 1 2) '(boot-test-tm-macro 1 2))
   (test "macro in a function body with helper definitions"
         (boot-test-use-macro-in-body 7)
         '((one 7) (two 7 7) (many 7 7 (7))))
   (test "macro in a function body, second call"
         (boot-test-use-macro-in-body 8)
         '((one 8) (two 8 8) (many 8 8 (8))))
   (test "and-let*" (and-let* ((x 1) (y (+ x 1))) (list x y)) '(1 2))
   (test "and-let*, false binding" (and-let* ((x #f) (y 1)) y) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define boot-test-private 'private)
(define-public boot-test-public 'public)

(define (fresh-module)
  (sublet (resolve-module '(texmacs-user)) '*exports* '()))

(define (regtest-boot-modules)
  (regression-test-group
   "boot-s7, modules" "modules"
   :none :none
   (test "the module is registered"
         (let? (hash-table-ref *modules* '(kernel boot boot-s7-test))) #t)
   (test "module name" *module-name* '(kernel boot boot-s7-test))
   (test "exports of this module"
         (list (and (memq 'boot-test-public *exports*) #t)
               (and (memq 'boot-test-private *exports*) #t))
         '(#t #f))
   (test "module-available?" (module-available? '(kernel boot srfi)) #t)
   (test "resolve-module gives the module environment"
         (let ((m (resolve-module '(kernel boot srfi))))
           (list (let? m) (m '*module-name*)))
         '(#t (kernel boot srfi)))
   (test "private definitions of used modules are not visible"
         (defined? 'case-lambda:alength (curlet)) #f)
   (test "private definitions exist in their module"
         (procedure? ((resolve-module '(kernel boot srfi)) 'case-lambda:alength))
         #t)
   (test "use-modules imports exports"
         (let ((m (fresh-module)))
           (with-module m (use-modules (kernel logic logic-bind)))
           (procedure? (m 'free-variable?)))
         #t)
   (test "use-modules twice updates the binding"
         (let ((m (fresh-module)))
           (with-module m (use-modules (kernel logic logic-bind)))
           (with-module m (use-modules (kernel logic logic-bind)))
           (procedure? (m 'free-variable?)))
         #t)
   (test "use-modules does not re-export"
         (let ((m (fresh-module)))
           (with-module m (use-modules (kernel logic logic-bind)))
           (m '*exports*))
         '())
   (test "import-bindings! skips bindings visible with the same value"
         (let* ((u (inlet 'boot-test-a car))
                (m (sublet u)))
           (import-bindings! m (list (cons 'boot-test-a car)))
           (list (defined? 'boot-test-a m #t) (eq? (m 'boot-test-a) car)))
         '(#f #t))
   (test "import-bindings! copies bindings visible with another value"
         (let* ((u (inlet 'boot-test-a car))
                (m (sublet u)))
           (import-bindings! m (list (cons 'boot-test-a cdr)))
           (list (defined? 'boot-test-a m #t) (eq? (m 'boot-test-a) cdr)))
         '(#t #t))
   (test "kernel bindings are not copied into modules"
         (let ((m (fresh-module)))
           (with-module m (use-modules (kernel boot abbrevs)))
           (list (defined? '== m #t) (eq? (m '==) ==)))
         '(#f #t))
   (test "tm-define-macro defines the macro in the user module"
         (macro? ((resolve-module '(texmacs-user)) 'boot-test-tm-macro))
         #t)
   (test "inherit-modules re-exports"
         (let ((m (fresh-module)))
           (with-module m (inherit-modules (kernel logic logic-bind)))
           (and (memq 'free-variable? (m '*exports*)) #t))
         #t)
   (test "lazy-define resolves the symbol in its module"
         (procedure? (tm-eval 'tmdoc-expand-help)) #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol lookup in large environments (TeXmacs patch to s7's lookup_from)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (big-let n)
  (let ((e (inlet)))
    (do ((i 0 (+ i 1))) ((= i n) e)
      (varlet e (string->symbol (string-append "boot-test-v" (number->string i)))
              i))))

(define (big-let-sum e rounds)
  ;; the symbols are deep in e, so looking them up moves their slots
  (with-let (sublet e 'rounds rounds)
    (let loop ((k 0) (s 0))
      (if (= k rounds) s
          (loop (+ k 1) (+ s boot-test-v0 boot-test-v250 boot-test-v499))))))

(define (boot-test-sym i)
  (string->symbol (string-append "boot-test-v" (number->string i))))

(define (renumbered-big-let n)
  ;; a big let with a closure created before the let is renumbered by
  ;; with-let: its lookups of deep symbols then miss s7's O(1) fast path
  (let* ((e (big-let n))
         (child (sublet e))
         (probe (with-let child
                  (lambda () (+ boot-test-v0 boot-test-v1 boot-test-v2)))))
    (with-let e 1)
    (cons e probe)))

(define (iterate-while-looking-up n)
  ;; lookups must not reorder the let that is being iterated over
  (let* ((ep (renumbered-big-let n))
         (e (car ep)) (probe (cdr ep))
         (seen (make-ahash-table)))
    (for-each (lambda (entry) (ahash-set! seen (car entry) #t) (probe)) e)
    (ahash-size seen)))

(define (boot-test-helper boot-test-v149) boot-test-v149)

;; s7 reuses the argument let of safe closures and fills it by position,
;; so lookups must not reorder it either
(define boot-test-many
  (eval `(lambda ,(map boot-test-sym (iota 150))
           (boot-test-helper 7)
           (list boot-test-v0 boot-test-v1 boot-test-v149))
        (curlet)))
(define boot-test-call-many
  (eval `(lambda () (boot-test-many ,@(iota 150))) (curlet)))

(define (many-parameters-calls)
  (list (boot-test-call-many) (boot-test-call-many) (boot-test-call-many)))

(define (regtest-boot-lookup)
  (let ((e (big-let 500)))
    (regression-test-group
     "boot-s7, lookup in large environments" "lookup"
     :none :none
     (test "values of deep symbols" (big-let-sum e 100) (* 100 (+ 0 250 499)))
     (test "no slot is lost or duplicated" (length (let->list e)) 500)
     (test "values after moving slots"
           (list (e 'boot-test-v0) (e 'boot-test-v250) (e 'boot-test-v499))
           '(0 250 499))
     (test "every symbol is still found"
           (let loop ((i 0) (ok #t))
             (if (= i 500) ok
                 (loop (+ i 1)
                       (and ok (== (e (string->symbol
                                       (string-append "boot-test-v"
                                                      (number->string i))))
                                   i)))))
           #t)
     (test "let-set! after moving slots"
           (begin (let-set! e 'boot-test-v250 -1) (big-let-sum e 1))
           (+ 0 -1 499))
     (test "iteration over a let while looking up its symbols"
           (iterate-while-looking-up 500) 500)
     (test "repeated calls of a function with many parameters"
           (many-parameters-calls) '((0 1 149) (0 1 149) (0 1 149)))
     ;; note: regression-test-group evaluates each test expression twice
     (test "varlet of a new symbol after moving slots"
           (let ((sym (gensym))) (varlet e sym 1) (e sym))
           1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The local patches of the vendored s7 (src/Scheme/S7/patches)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0003: curried define, as in Guile
(define ((boot-test-adder a) b) (+ a b))
(define (((boot-test-adder3 a) b) c) (+ a b c))
(define ((boot-test-adder* a) . l) (apply + a l))
(define ((boot-test-rest . l) x) (cons x l))
(define-public ((boot-test-public-adder a) b) (+ a b))

(define (boot-test-butlast l) (reverse (cdr (reverse l))))

(define (boot-test-scale-all k l)
  ;; an internal curried definition next to an internal recursive one
  (define ((scale k) x) (* k x))
  (define (walk l)
    (if (null? l) '() (cons ((scale k) (car l)) (walk (cdr l)))))
  (walk (boot-test-butlast l)))

;; 0002: a run-time macro which builds the lambda of a local recursive
;; function, called from the function body with the value of a Scheme
;; function: the second call crashed s7
(define-macro (boot-test-define-lambda head . body)
  `(define ,(car head) (lambda ,(cdr head) ,@body)))

(define (boot-test-pairs t)
  (boot-test-define-lambda (pairs l)
    (if (or (null? l) (null? (cdr l))) '()
        (cons (cons (car l) (cadr l)) (pairs (cddr l)))))
  (pairs (boot-test-butlast (cdr t))))

(define (regtest-boot-patches)
  (regression-test-group
   "boot-s7, patches of s7" "patches"
   :none :none
   ;; 0001
   (test "write gives the characters of a long string of one character"
         (object->string (make-string 1500 #\x))
         (string-append "\"" (make-string 1500 #\x) "\""))
   (test "a long string of one character reads back as a string"
         (with-input-from-string (object->string (make-string 1500 #\space))
           read)
         (make-string 1500 #\space))
   (test "readable output still abbreviates"
         (object->string (make-string 1500 #\x) :readable)
         "(make-string 1500 #\\x)")
   ;; 0002
   (test "closure made by a macro at each call, twice"
         (list (boot-test-pairs '(with a 1 b 2 x))
               (boot-test-pairs '(with a 1 x)))
         '(((a . 1) (b . 2)) ((a . 1))))
   ;; 0003
   (test "curried define" ((boot-test-adder 1) 2) 3)
   (test "curried define, two levels" (((boot-test-adder3 1) 2) 3) 6)
   (test "curried define, rest arguments inside" ((boot-test-adder* 1) 2 3 4) 10)
   (test "curried define, rest arguments outside"
         ((boot-test-rest 1 2) 0) '(0 1 2))
   (test "curried define, its source"
         (procedure-source boot-test-adder)
         '(lambda (a) (lambda (b) (+ a b))))
   (test "internal curried define"
         (let () (define ((mul a) b) (* a b)) ((mul 6) 7))
         42)
   (test "internal curried and recursive definitions, twice"
         (list (boot-test-scale-all 2 '(1 2 3 0))
               (boot-test-scale-all 3 '(1 2 0)))
         '((2 4 6) (3 6)))
   (test "define-public with a curried head exports the name"
         (and (memq 'boot-test-public-adder *exports*) #t) #t)
   (test "define-public with a curried head publishes the function"
         ((((rootlet) 'boot-test-public-adder) 1) 2) 3)
   (test "plain define is unchanged" (let () (define x 5) x) 5)
   (test "define* is not curried"
         (catch #t (lambda () (eval '(define* ((f a) b) b) (inlet)) 'defined)
           (lambda args 'error))
         'error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-boot-s7)
  (let ((n (+ (regtest-boot-reader)
              (regtest-boot-catch)
              (regtest-boot-macros)
              (regtest-boot-modules)
              (regtest-boot-lookup)
              (regtest-boot-patches))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of boot-s7: ok\n")))
