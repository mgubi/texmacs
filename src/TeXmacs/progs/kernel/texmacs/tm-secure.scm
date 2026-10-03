
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-secure.scm
;; DESCRIPTION : Secure evaluation of Scheme scripts
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-secure)
  (:use (kernel texmacs tm-define) (kernel texmacs tm-plugins)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive secure functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (define-secure-symbols . l)
  (for-each (lambda (x) (property-set! x :secure #t '())) l)
  '(noop))

(define-secure-symbols
  boolean? null? symbol? string? pair? list?
  equal? == not
  string-length substring string-append
  string->list list->string string-ref string-set!
  + - * / gcd lcm quotient remainder modulo abs log exp sqrt
  car cdr caar cadr cdar cddr
  caaar caadr cadar caddr cdaar cdadr cddar cdddr
  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
  cons list append length reverse
  texmacs-version texmacs-version-release*
  display display*
  refresh-now)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The environment @env is an association list which maps the locally
;; bound variables to #t if they may hold arbitrary values and to 'proc
;; if they are known to hold secure procedures (checked lambda expressions
;; or secure symbols).  Only variables of the second kind may be called.
;; The names of the special forms understood by the checker can not be
;; rebound, since that would change the meaning of these forms.

(define (secure-args? args env)
  (cond ((null? args) #t)
        ((npair? args) #f)
        (else (and (secure-expr? (car args) env)
                   (secure-args? (cdr args) env)))))

(define (secure-cond? args env)
  (cond ((null? args) #t)
        ((or (npair? args) (npair? (car args))) #f)
        (else (and (or (== (caar args) 'else) (secure-expr? (caar args) env))
                   (secure-args? (cdar args) env)
                   (secure-cond? (cdr args) env)))))

(define (secure-bindable? x)
  (and (symbol? x)
       (not (logic-ref secure-macros% x))
       (not (in? x '(quote quasiquote unquote unquote-splicing else =>)))))

(define (secure-formals? l)
  (cond ((null? l) #t)
        ((pair? l) (and (secure-bindable? (car l)) (secure-formals? (cdr l))))
        (else (secure-bindable? l))))

(define (local-env env l kind)
  (cond ((null? l) env)
        ((pair? l) (local-env (cons (cons (car l) kind) env) (cdr l) kind))
        (else (cons (cons l kind) env))))

(define (secure-lambda? args env)
  (and (pair? args)
       (secure-formals? (car args))
       (secure-args? (cdr args) (local-env env (car args) #t))))

(define (secure-procedure? expr env)
  (cond ((symbol? expr)
         (with kind (assoc-ref env expr)
           (if kind (== kind 'proc) (property expr :secure))))
        ((pair? expr)
         (and (== (car expr) 'lambda) (secure-lambda? (cdr expr) env)))
        (else #f)))

(define (secure-with args env)
  (and (pair? args) (pair? (cdr args)) (pair? (cddr args))
       (secure-bindable? (car args))
       (secure-expr? (cadr args) env)
       ;; (cadr args) has just been checked; classify it without checking
       ;; it again (re-checking takes exponential time on nested with's).
       ;; This is safe because lambda can not be rebound.
       (let* ((v (cadr args))
              (kind (if (or (and (pair? v) (== (car v) 'lambda))
                            (and (symbol? v) (secure-procedure? v env)))
                        'proc #t)))
         (secure-args? (cddr args) (local-env env (list (car args)) kind)))))

(define (secure-quasiquote? args env)
  (cond ((pair? args)
         (cond ((func? args 'unquote 1) (secure-expr? (cadr args) env))
               ((func? args 'unquote-splicing 1) (secure-expr? (cadr args) env))
               (else (and (secure-quasiquote? (car args) env)
                          (secure-quasiquote? (cdr args) env)))))
        ((symbol? args) #t)
        ((number? args) #t)
        ((string? args) #t)
        ((char? args) #t)
        ((tree? args) #t)
        ((null? args) #t)
        ((boolean? args) #t)
        (else #f)))

(define (secure-expr? expr env)
  (cond ((pair? expr)
         (let* ((f (car expr))
                (m (and (symbol? f) (logic-ref secure-macros% f))))
           (cond ((and (symbol? f) (assoc-ref env f))
                  (and (== (assoc-ref env f) 'proc)
                       (secure-args? (cdr expr) env)))
                 (m (m (cdr expr) env))
                 ((== f 'quote) #t)
                 ((== f 'quasiquote) (secure-quasiquote? (cdr expr) env))
                 ((symbol? f)
                  (and (property f :secure)
                       (secure-args? (cdr expr) env)))
                 ((pair? f)
                  (and (== (car f) 'lambda)
                       (secure-lambda? (cdr f) env)
                       (secure-args? (cdr expr) env)))
                 (else #f))))
        ((symbol? expr)
         (or (assoc-ref env expr) (property expr :secure)))
        ((number? expr) #t)
        ((string? expr) #t)
        ((tree? expr) #t)
        ((null? expr) #t)
        ((boolean? expr) #t)
        (else #f)))

(logic-table secure-macros%
  (and ,secure-args?)
  (begin ,secure-args?)
  (cond ,secure-cond?)
  (if ,secure-args?)
  (lambda ,secure-lambda?)
  (or ,secure-args?)
  (with ,secure-with))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (secure? expr)
  "Test whether it is secure to evaluate the expression @expr"
  (and (or (secure-expr? expr '())
           (and (lazy-plugin-force) (secure-expr? expr '())))
       #t))

(define-public (secure-eval expr)
  "Evaluate @expr only when it is secure to do so"
  (and (secure? expr) (eval expr)))
