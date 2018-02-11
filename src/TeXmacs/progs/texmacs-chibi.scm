;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some glue code between Chibi and TeXmacs

(import (chibi) (scheme small))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an implementation of define-macro with rsc-macro-transformer

;; from https://stackoverflow.com/questions/15552057/is-it-possible-to-implement-define-macro-in-mit-scheme?rq=1
;; see also https://ds26gte.github.io/tyscheme/index-Z-H-20.html

;; rsc-macro-transformer is not r7rs, imported from the (chibi) module

(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . args) body ...)
     (define-syntax name
       (rsc-macro-transformer
         (let ((transformer (lambda args body ...)))
           (lambda (exp env)
              (apply transformer (cdr exp)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keywords implementation

;; from https://srfi.schemers.org/srfi-88/srfi-88.html
;; slightly modified to have keywords of the form :my-keyword
;; (i.e. lisp-like leading colon)

(define real-symbol? symbol?)
    (define real-symbol->string symbol->string)
    (define real-string->symbol string->symbol)

(define looks-like-an-unquoted-keyword?
  (lambda (s)
    (let ((n (string-length s)))
      (and (> n 1)
           (char=? (string-ref s 0) #\:)))))

(set! symbol?
  (lambda (obj)
    (and (real-symbol? obj)
         (not (looks-like-an-unquoted-keyword?
               (real-symbol->string obj))))))

(define keyword?
  (lambda (obj)
    (and (real-symbol? obj)
         (looks-like-an-unquoted-keyword?
          (real-symbol->string obj)))))

(set! symbol->string real-symbol->string)

(define keyword->string
  (lambda (k)
    (let* ((s (real-symbol->string k))
           (n (string-length s)))
      (substring s 1 n)))) ; remove the colon

(set! string->symbol
  (lambda (s)
    (if (looks-like-an-unquoted-keyword? s)
        (error "sorry... the symbol would look like a keyword!")
        (real-string->symbol s))))

(define string->keyword
  (lambda (s)
    (let ((s-colon (string-append ":" s)))
      (if (looks-like-an-unquoted-keyword? s-colon)
          (real-string->symbol s-colon)
          (error "sorry... the keyword would look like a symbol!")))))

(define-macro (define-keyword ks)
   (let ((k (string->keyword (symbol->string ks))))
   `(define ,k ',k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; take a snapshot of current bindings

;;(define *tm-base-bindings*
;;    (let p ((e (current-environment)) (l '())) (if e (p (env-parent e) (append (env-exports e) l)) l)))

(import (chibi modules)
        (only (meta) module-env)
        (only (chibi ast) env-define!))

;; utilities
(define (ca*r x) (if (pair? x) (ca*r (car x)) x))
(define (ca*dr x) (ca*r (cdr x)))

;; variables to parse texmacs modules recursively
(define *module-exports* '(()))
(define *module-names* '())

;; list of export bindings for the base environment
(define *tm-base-bindings*  (module-exports (find-module '(chibi))))
(define *tm-user-bindings*  '())

;; utility to extract all bindings from a given environment
(define (all-bindings env)
  (if env (append (env-exports env) (all-bindings (env-parent env))) '()))

;; the base environment in which texmacs modules are evaluated
(define *tm-base-env*
    (let* ((base-env (make-environment))
           (mod (find-module '(chibi))))
           (%import base-env (module-env mod) #f #t)
           base-env))

;; create a fresh evaluation environment for texmacs modules
(define make-evaluation-env
        (lambda ()
          (let ((env (make-environment)))
             (%import env *tm-base-env* *tm-base-bindings* #t)
             env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (tm-module-name->strings ls res)
  (if (null? ls)
      res
      (let ((str (cond ((symbol? (car ls)) (symbol->string (car ls)))
                       ((number? (car ls)) (number->string (car ls)))
                       ((string? (car ls)) (car ls))
                       (else (error "invalid module name" (car ls))))))
        (tm-module-name->strings (cdr ls) (cons "/" (cons str res))))))

(define (tm-module-name->file name)
  (string-concatenate
   (reverse (cons ".scm" (cdr (tm-module-name->strings name '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; texmacs modules scaffolding

(define *tm-modules* '())

;; texmacs module record
(define-record-type <tm-module>
(make-tm-module name env exports file)
tm-module?
(name get-tm-module-name set-tm-module-name!)
(env get-tm-module-env set-tm-module-env!)
(exports get-tm-module-exports set-tm-module-exports!)
(file get-tm-module-file))


;; not used
(define (show-tm-module file)
    (call-with-input-file file
      (lambda (in)
        (set-port-line! in 1)
        (let lp ()
            (let ((x (read in)))
              (display x) (newline)
               (if (not (eof-object? x)) (lp)))))))

;; load a texmacs module and extracts metadata
(define (load-tm-module file)
  (let ((env (make-evaluation-env)))
    (display "Loading ") (display file) (display "  in ") (display env) (newline)
    (set! *module-exports* (cons '() *module-exports*))
    (set! *module-names* (cons '() *module-names*))
    (load file env)
    (let ((res (make-tm-module (car *module-names*) env
                               (reverse (car *module-exports*)) file)))
         ;;(display "The module exports ") (display (car *module-exports*)) (newline)
         ;;(display "The module name    ") (display (car *module-names*)) (newline)
         (set! *module-exports* (cdr *module-exports*))
         (set! *module-names* (cdr *module-names*))
         res)))

;; require a texmacs module, loading it if it is not already present
(define (require-tm-module name)
   (cond
     ((assoc name *tm-modules*) => (lambda (p) (cdr p)))
     (else (let* ((file (tm-module-name->file name))
                  (mod (load-tm-module file)))
                 (set! *tm-modules* (cons (cons name  mod) *tm-modules*))
                 mod))))

;(define (require-tm-modules names)
;  (for-each (lambda (name) (require-tm-module name)) names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; texmacs module language

(define (tm-export name)
  ;;(display "Exporting:") (display (ca*r name)) (newline)
  ;;(display *module-exports*) (newline)
  (set! *module-exports* (cons (cons (ca*r name) (car *module-exports*))
                               (cdr *module-exports*))))

(define (tm-start-module name)
  ;;(display name) (newline)
  ;;(display *module-names*) (newline)
  (set! *module-names* (cons name (cdr *module-names*))))

(define-syntax tm-import-module
  (sc-macro-transformer
    (lambda (exp env)
      (let* ((mod-name (cadr exp))
             (mod (require-tm-module mod-name)))
        ;;(display (current-environment)) (newline)
        `(%import ,(current-environment) ,(get-tm-module-env mod)
                  ',(get-tm-module-exports mod) #t)))))

(define-syntax tm-import-modules
  (syntax-rules ()
     ((tm-import-modules) (begin))
     ((tm-import-modules x . xs)  (begin (tm-import-module x) (tm-import-modules . xs)))))

(define-syntax texmacs-module
   (syntax-rules (:use)
      ((texmacs-module x (:use z ...) . zz)
          (begin (tm-import-modules z ...)
                 (texmacs-module x . zz)))
      ((texmacs-module x) (tm-start-module `x))))

(define-syntax define-public
   (syntax-rules ()
      ((define-public x body ...)
          (begin (define x body ...) (tm-export `x)))))

(define-syntax define-public-macro
   (syntax-rules ()
      ((define-public-macro x body ...)
          (begin (define-macro x body ...) (tm-export `x)))))

(define *texmacs-env* (current-environment))
(define *texmacs-defs* '())

(define-syntax define-texmacs
   (syntax-rules ()
      ((define-texmacs (x . xs) body ...)
          (begin (define (x . xs) body ...)
                  (env-define! *texmacs-env* 'x x)
                  (set! *texmacs-defs* (cons 'x *texmacs-defs*))))))

(define-syntax lazy-define
  (syntax-rules ()
     ((lazy-define mod name)
        (define-texmacs (name . args)
          (let* ((m (require-tm-module 'mod))
                 (r (eval 'name *texmacs-env*)))
          (apply r args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up the standard environment

(define *texmacs-module-bindings* '(define-public define-public-macro define-texmacs texmacs-module))
(%import *tm-base-env* (current-environment) *texmacs-module-bindings* #t)
(set! *tm-base-bindings* (append *tm-base-bindings* *texmacs-module-bindings*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "Initializing Chibi/TeXmacs interface\n")
