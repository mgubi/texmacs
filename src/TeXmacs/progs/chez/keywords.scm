;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keywords implementation


(texmacs-module (chez keywords))

;; from https://srfi.schemers.org/srfi-88/srfi-88.html
;; slightly modified to have keywords of the form :my-keyword
;; (i.e. lisp-like leading colon)
(import (rename scheme (symbol? real-symbol?) (symbol->string real-symbol->string) (string->symbol real-string->symbol)))
;(define real-symbol? symbol?)
;(define real-symbol->string symbol->string)
;(define real-string->symbol string->symbol)

(define looks-like-an-unquoted-keyword?
  (lambda (s)
    (let ((n (string-length s)))
      (and (> n 1)
           (char=? (string-ref s 0) #\:)))))

(define-public symbol?
  (lambda (obj)
    (and (real-symbol? obj)
         (not (looks-like-an-unquoted-keyword?
               (real-symbol->string obj))))))

(define-public keyword?
  (lambda (obj)
    (and (real-symbol? obj)
         (looks-like-an-unquoted-keyword?
          (real-symbol->string obj)))))


(define-public keyword->string
  (lambda (k)
    (let* ((s (real-symbol->string k))
           (n (string-length s)))
      (substring s 1 n)))) ; remove the colon


(define-public string->keyword
  (lambda (s)
    (let ((s-colon (string-append ":" s)))
      (if (looks-like-an-unquoted-keyword? s-colon)
          (real-string->symbol s-colon)
          (error "sorry... the keyword would look like a symbol!")))))

(define-public symbol->string real-symbol->string)

(define-public string->symbol
  (lambda (s)
    (if (looks-like-an-unquoted-keyword? s)
        (error "sorry... the symbol would look like a keyword!")
        (real-string->symbol s))))

(define-public-macro (define-keyword ks)
   (let ((k (string->keyword (symbol->string ks))))
   `(define-public ,k ',k)))

(define-public-macro (define-keywords . kss)
  `(begin ,@(map (lambda (ks) (let ((k (string->keyword (symbol->string ks)))) `(define-public ,k ',k))) kss)))


