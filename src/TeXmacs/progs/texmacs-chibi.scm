;; some glue code between Chibi and TeXmacs

(import (scheme small) (chibi))


(define (noop) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

