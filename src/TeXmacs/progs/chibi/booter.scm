(import (chibi))

(define *texmacs-env* (current-environment))

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

;; brings in texmacs module system with selected exports

(let ((env (interaction-environment))
       (file (url-concretize "$TEXMACS_PATH/progs/chibi/module-loader.scm")))
  (display "Loading ") (display file) (newline)
  (%import env #f '(*texmacs-env* *texmacs-primitives*) #t)
  (load file env)
  (%import #f env '(*tm-modules* *texmacs-defs*) #t)
  (%import #f env (eval '*texmacs-module-bindings* env) #t)
  (display "Done\n")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-import-module (chibi keywords))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



