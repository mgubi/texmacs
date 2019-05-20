

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 curried-definitions))

(debug-set! width 200)

(eval-when (expand)
 (display "BOOT: Expanding\n"))
(eval-when (compile)
 (display "BOOT: Compiling\n"))
(eval-when (eval)
 (display "BOOT: Evaling\n"))
(eval-when (load)
 (display "BOOT: Loading\n"))

(define texmacs-user-temp (current-module))
(define temp-module (current-module))
(define temp-value #f)

(define (guile-a?) (equal? (scheme-dialect) "guile-a"))
(define (guile-b?) (equal? (scheme-dialect) "guile-b"))
(define (guile-c?) (equal? (scheme-dialect) "guile-c"))
(define (guile-b-c?) (or (guile-b?) (guile-c?)))
(if (guile-c?) (use-modules (ice-9 rdelim) (ice-9 pretty-print)))
(define has-look-and-feel? (lambda (x) (== x "emacs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirect standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define original-display display)
(define original-write write)

(define (display . l)
  "display one object on the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-display l)
      (tm-output (display-to-string (car l)))))

(define (write . l)
  "write an object to the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-write l)
      (tm-output (object->string (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide functions if not defined and public macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (provide-public head . body)
  (if (or (and (symbol? head) (not (defined? head)))
	  (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
      `(define-public ,head ,@body)
      '(noop)))

(define-macro (define-public-macro head . body)
      `(begin
	 (define-macro ,(car head)
	   (lambda ,(cdr head) ,@body))
	 (export-syntax ,(car head))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quit-TeXmacs-scheme) (noop))

(define-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (with-module module . body)
  `(begin
     (eval-when (expand load eval)
       (set! temp-module (current-module))
       (set-current-module ,module))
     ,@body
     (eval-when (expand load eval) (set-current-module temp-module))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (import-from . modules) `(use-modules ,@ modules ))

(define (module-exported-symbols m)
  (module-map (lambda (sym var) sym) (module-public-interface (resolve-module m))))

(define-macro (inherit-module which)
  (define (module-exports which)
    (let* ((m (resolve-module which))
	   (m-public (module-public-interface m)))
      (module-map (lambda (sym var) sym) m-public)))
    `(begin
       (use-modules ,which)
       (re-export ,@ (module-exports which))))

(define-macro (inherit-modules . which-list)
  `(begin ,@(append (map (lambda (w) `(inherit-module ,w)) which-list))))

(eval-when (expand load eval)
(set-current-module the-root-module)
(define texmacs-user (module-ref (resolve-module '(guile-user)) 'texmacs-user-temp))
(define-macro (texmacs-module name . options)
  (define (transform action)
    (cond ((not (pair? action)) (noop))
	  ((equal? (car action) :use) (cons 'use-modules (cdr action)))
	  ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
	  ((equal? (car action) :export)
	   (display "Warning] The option :export is no longer supported\n")
	   (display "       ] Please use tm-define instead\n"))
	  (else '(noop))))
  (let ((l (map-in-order transform options)))
	(set! l (cons `(module-use! (current-module) texmacs-user) l))
;;(display "Loading ") (display name) (display "\n")
    `(begin
       (eval-when (expand) (display "IN MODULE:") (display ',name) (display "\n"))
       (define-module ,name)
       ,@l
(eval-when (expand) (display "END MODULE HEADER:") (display ',name) (display "\n")))))
(export-syntax texmacs-module)
(set-current-module texmacs-user)
)

(define-public (module-available? module-name)
  (catch #t
    (lambda () (resolve-interface module-name) #t)
    (lambda (key . args) #f)))

(define-public (module-provide m)
  (if (not (module-available? m)) (module-load m)))
