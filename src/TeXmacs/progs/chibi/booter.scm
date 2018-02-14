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
  (%import #f env '(*tm-modules*) #t)
  (%import #f env (eval '*texmacs-module-bindings* env) #t)
  (display "Done\n")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compatibility layer

(define-public (noop) (begin))
(define-public (plus1 x) (+ x 1))
(define-public (minus1 x) (- x 1))
(define-public (list-head l k) (if (> k 0) (cons (car l) (list-head (cdr l) (- k 1))) '()))
(define-public map-in-order map)

(define-public has-look-and-feel? (lambda (x) (== x "emacs")))

(define-public gensym
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (string->symbol (string-append "tm-gensym-" (number->string counter))))))



(define-public (quit-TeXmacs-scheme) (noop))

(define-public-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-public-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))



(tm-inherit-module (chibi keywords))

(define-keywords or and not repeat group quote exclude range and-not match replace up down first last next previous)

(define-keywords mode require type synopsis returns note argument arguments default proposals secure check-mark interactive balloon)
(define-keywords all)
(define-keywords pause every idle)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-inherit-modules (kernel boot compat) (kernel boot abbrevs)
                    (kernel boot debug) (kernel boot srfi)
                    (kernel boot ahash-table) (kernel boot prologue))

(tm-inherit-modules (kernel library base) (kernel library list)
                    (kernel library tree) (kernel library content)
                    (kernel library patch))

(tm-inherit-modules (kernel regexp regexp-match) (kernel regexp regexp-select))

(tm-inherit-modules (kernel logic logic-rules) (kernel logic logic-query)
                    (kernel logic logic-data))

(tm-inherit-modules (kernel texmacs tm-define)
                    (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
                    (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
                    (kernel texmacs tm-convert) (kernel texmacs tm-dialogue)
                    (kernel texmacs tm-language) (kernel texmacs tm-file-system)
                    (kernel texmacs tm-states))

(tm-inherit-modules (kernel gui gui-markup)
                    (kernel gui menu-define) (kernel gui menu-widget)
                    (kernel gui kbd-define) (kernel gui kbd-handlers)
                    (kernel gui menu-test)
                    (kernel old-gui old-gui-widget)
                    (kernel old-gui old-gui-factory)
                    (kernel old-gui old-gui-form)
                    (kernel old-gui old-gui-test))



