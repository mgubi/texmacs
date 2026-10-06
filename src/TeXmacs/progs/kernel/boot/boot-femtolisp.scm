
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot-femtolisp.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               the TeXmacs module system, on femtolisp
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loaded by init-femtolisp.scm after r5rs-femtolisp.scm, as plain global
;; definitions.

(define (femtolisp-scheme?) #t)
(define (s7-scheme?) #f)
(define (guile-a?) #f)
(define (guile-b?) #f)
(define (guile-c?) #f)
(define (guile-b-c?) #f)
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
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Femtolisp has one global environment. A TeXmacs module gets its private
;; names in it under names of its own: the private definitions of a module,
;; its plain (define name ...) and (define-macro (name ...) ...), become the
;; global variables and macros name@module. When a module file is loaded, its
;; forms are read and scanned for these definitions first; then each form is
;; expanded and compiled with *current-module* set to the module, and the
;; hook resolve-global of femtolisp (see Scheme/Femtolisp/patches) replaces
;; the private names by their global names. The public definitions
;; (define-public, tm-define...) are global under their own names, as in the
;; user module of Guile, where the other modules find them.

;; a module: #(module name privates), where privates maps each private name
;; to its global name; the user module is #f
(define *modules* (table))
(define *current-module* #f)
(define *module-name* '(texmacs-user))
(define *texmacs-user-module* 'texmacs-user)
(define texmacs-user 'texmacs-user)
(define temp-module #f)
(define temp-value #f)

(define (%make-module name) (vector 'module name (table)))
(define (%module? x)
  (and (vector? x) (= (length x) 3) (eq? (aref x 0) 'module)))
(define (%module-privates m) (aref m 2))
(define (%module-of x) (if (%module? x) x #f))

(define (%module-path-string name)
  (apply string-append
         (cons (symbol->string (car name))
               (map (lambda (s) (string-append "/" (symbol->string s)))
                    (cdr name)))))

(define (%module-global-name m s)
  (symbol (string-append (symbol->string s) "@"
                         (%module-path-string (aref m 1)))))

(define (%module-declare-private! m s)
  (if (not (has? (%module-privates m) s))
      (put! (%module-privates m) s (%module-global-name m s))))

(set! resolve-global
      (lambda (s)
        (let ((m *current-module*))
          (if m (get (aref m 2) s s) s))))

(define (current-module) (or *current-module* texmacs-user))
(define (set-current-module m) (set! *current-module* (%module-of m)))
(define (module-name m) (if (%module? m) (aref m 1) '(texmacs-user)))

;; the names defined privately by a list of forms read from a file
(define %public-definers
  '(define-public define-public-macro provide-public tm-define
    tm-define-macro tm-menu tm-widget menu-bind tm-property))

(define (%defined-name head)
  (if (pair? head) (%defined-name (car head)) head))

(define *file-definitions* #f)

(define (%scan-definitions forms)
  (let ((private '()) (public '()) (public-here '()) (macros '()))
    (define (scan x)
      (if (pair? x)
          (let ((h (car x)))
            (cond ((and (memq h '(define define-macro)) (pair? (cdr x)))
                   (let ((n (%defined-name (cadr x))))
                     (if (symbol? n) (set! private (cons n private)))
                     (if (and (symbol? n) (eq? h 'define-macro))
                         (set! macros (cons n macros)))))
                  ((and (memq h %public-definers) (pair? (cdr x)))
                   (let ((n (%defined-name (cadr x))))
                     (if (symbol? n) (set! public (cons n public)))
                     (if (and (symbol? n)
                              (memq h '(define-public define-public-macro
                                        provide-public)))
                         (set! public-here (cons n public-here)))
                     (if (and (symbol? n)
                              (memq h '(define-public-macro tm-define-macro)))
                         (set! macros (cons n macros)))))
                  ((memq h '(export re-export))
                   (set! public (append (filter symbol? (cdr x)) public))
                   (set! public-here
                         (append (filter symbol? (cdr x)) public-here)))
                  ((eq? h 'begin) (for-each scan (cdr x)))
                  ((and (memq h '(if when unless)) (pair? (cdr x)))
                   (for-each scan (cddr x)))
                  ((eq? h 'cond)
                   (for-each (lambda (c) (if (pair? c) (for-each scan (cdr c))))
                             (cdr x)))))))
    (for-each scan forms)
    ;; the functions of the file are compiled as plain calls, not its macros
    ;; (a macro used before its definition is compiled at its first call)
    (if *file-definitions*
        (for-each (lambda (n)
                    (if (not (memq n macros)) (put! *file-definitions* n #t)))
                  (append private public)))
    ;; a name defined by tm-define (globally, under its quoted name) and by a
    ;; plain define stays private in the module, as in Guile, where the
    ;; binding of the module hides the one of the user module
    (filter (lambda (n) (not (memq n public-here))) private)))

(define (%read-forms file)
  (call-with-input-file file
    (lambda (p)
      (let loop ((acc '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse! acc) (loop (cons x acc))))))))

(define-override (eval x . env)
  (if (null? env)
      (%fl-eval x)
      (let ((m (%module-of (car env))))
        (with-bindings ((*current-module* m)
                        (*module-name* (module-name m)))
          (%fl-eval x)))))

(define (tm-eval x)
  (with-bindings ((*current-module* #f)
                  (*module-name* '(texmacs-user)))
    (%fl-eval x)))

(define (primitive-eval x) (eval x))

(define (%eval-forms forms)
  (let loop ((l forms) (r #t))
    (if (null? l) r (loop (cdr l) (%fl-eval (car l))))))

;; Femtolisp expands the macros when it compiles a form, and Guile when it
;; first evaluates it: TeXmacs code may use a macro which is defined after the
;; code (in a module loaded later). A call (f ...) of a name f which is
;; neither bound nor a macro when it is compiled, nor defined by the file
;; being loaded, is compiled when it is first evaluated, as the function of
;; the local variables in its scope (#(form variables module function)).
;; With TEXMACS_FL_TRACE set, (%late-calls) lists them.
(define %late-sites '())

(define (%env-variables env)
  (let ((seen '()))
    (for-each (lambda (frame)
                (if (pair? frame)
                    (for-each (lambda (v)
                                (if (and (symbol? v) (not (memq v seen)))
                                    (set! seen (cons v seen))))
                              frame)))
              env)
    (reverse! seen)))

;; (when a site is compiled, the names still unknown are plain calls, which
;; raise an unbound variable error)
(define *compiling-late-site* #f)

(define (%late-call site vals)
  (let ((f (aref site 3)))
    (if (not f)
        (begin
          (set! f (with-bindings ((*current-module* (aref site 2))
                                  (*compiling-late-site* #t))
                    (%fl-eval (list 'lambda (aref site 1) (aref site 0)))))
          (aset! site 3 f)))
    (apply f vals)))

(set! compile-unknown-call
      (lambda (x env)
        (if (or *compiling-late-site*
                (and *file-definitions* (has? *file-definitions* (car x))))
            #f
            (let* ((vars (%env-variables env))
                   (site (vector x vars *current-module* #f)))
              (if %trace-errors? (set! %late-sites (cons site %late-sites)))
              (list '%late-call (list 'quote site) (cons 'list vars))))))

(define (%late-calls)
  (map (lambda (site) (car (aref site 0))) %late-sites))

;; loads a file; when its first form is (texmacs-module name ...), it is the
;; file of the module name
(define (%load-file file)
  (trycatch (%load-file-sub file)
            (lambda (e) (raise (list 'load-error file e)))))

(define (%load-file-sub file)
  (with-bindings ((*file-definitions* (table)))
    (%load-forms (%read-forms file))))

(define (%load-forms forms)
  (begin
    (if (and (pair? forms) (pair? (car forms))
             (eq? (caar forms) 'texmacs-module) (pair? (cdar forms)))
        (let* ((name (cadar forms))
               (m (or (get *modules* name #f)
                      (let ((m (%make-module name)))
                        (put! *modules* name m)
                        m))))
          (for-each (lambda (s) (%module-declare-private! m s))
                    (%scan-definitions forms))
          (with-bindings ((*current-module* m)
                          (*module-name* name))
            (%eval-forms forms)))
        (begin
          (%scan-definitions forms)
          (%eval-forms forms)))))

(define-override (load file . env)
  (if (null? env)
      (%load-file file)
      (with-bindings ((*current-module* (%module-of (car env))))
        (%load-file file))))

(define (primitive-load file) (load file))

;; the files of the modules
(define *loaded-modules* (table))

(define (list->module module)
  (let ((u (url-unix "$GUILE_LOAD_PATH"
                     (string-append (%module-path-string module) ".scm"))))
    (url-materialize u "r")))

(define (module-available? module)
  (or (has? *modules* module)
      (let ((f (list->module module)))
        (and (string? f) (not (string-null? f))))))

(define (module-load module)
  (if (and (list? module) (not (has? *loaded-modules* module)))
      (begin
        (put! *loaded-modules* module #t)
        (let ((f (list->module module)))
          (if (or (not (string? f)) (string-null? f))
              (error "module-load: no file for module" module))
          (with-bindings ((*current-module* #f)
                          (*module-name* '(texmacs-user)))
            (%load-file f))
          (if (not (has? *modules* module))
              (put! *modules* module (%make-module module)))))))

(define (module-provide module) (module-load module))

(define (resolve-module module)
  (if (or (eq? module texmacs-user) (equal? module '(texmacs-user)))
      texmacs-user
      (begin
        (module-provide module)
        (get *modules* module #f))))
(define (resolve-interface module) (resolve-module module))
(define (module-public-interface m) m)

;; the value of sym in module m: its private definition or the global one
(define (module-ref m sym . default)
  (let ((g (if (%module? m) (get (%module-privates m) sym sym) sym)))
    (cond ((bound? g) (top-level-value g))
          ((pair? default) (car default))
          (else (error "module-ref: unbound variable" sym)))))
(define (module-defined? m sym)
  (bound? (if (%module? m) (get (%module-privates m) sym sym) sym)))
(define (module-symbols m)
  (if (%module? m) (table.keys (%module-privates m)) '()))

(define (defined? sym . env)
  (if (null? env)
      (bound? (resolve-global sym))
      (module-defined? (car env) sym)))

(define-macro (use-modules . modules)
  `(begin ,@(map (lambda (m) `(module-provide ',m)) modules) (noop)))
(define-macro (import-from . modules) `(use-modules ,@modules))
(define-macro (inherit-modules . modules) `(use-modules ,@modules))
(define-macro (export . symbols) '(noop))
(define-macro (re-export . symbols) '(noop))

(define-macro (texmacs-module name . options)
  (define (transform action)
    (cond ((not (pair? action)) '(noop))
          ((memq (car action) '(:use :inherit))
           (cons 'use-modules (cdr action)))
          ((eq? (car action) :export)
           '(begin
              (display "Warning] The option :export is no longer supported\n")
              (display "       ] Please use tm-define instead\n")))
          (else '(noop))))
  `(begin
     (%module-enter! ',name)
     ,@(map transform options)
     (noop)))

;; makes the module name the current one, as define-module in Guile (the
;; load of the file of a module also binds it, see %load-forms)
(define (%module-enter! name)
  (let ((m (or (get *modules* name #f)
               (let ((m (%make-module name)))
                 (put! *modules* name m)
                 m))))
    (put! *loaded-modules* name #t)
    (set! *current-module* m)
    (set! *module-name* name)))

;; evaluates the body in the module (as the top-level forms of its file)
(define-macro (with-module module . body)
  `(eval '(begin ,@body) ,module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guile's (define-macro name transformer) besides (define-macro (name . args))
(define-macro (define-macro form . body)
  (if (symbol? form)
      `(set-syntax! ',form ,(car body))
      `(set-syntax! ',(car form) (lambda ,(cdr form) ,@body))))

;; defines the global variable s (also when s is a builtin of femtolisp), as
;; the definitions of tm-define
(define (define-global! s v)
  (if (constant? s) (%unconstant! s))
  (set-top-level-value! s v))

;; a public definition is a global one: the scan of the module file does not
;; make its name private (curried heads, ((f a) b), are expanded by define)
(define-macro (define-public head . body)
  (let ((name (%defined-name head)))
    (if (and (symbol? name) (constant? name)) (%unconstant! name))
    `(define ,head ,@body)))

(define-macro (define-public-macro head . body)
  `(define-macro ,head ,@body))

(define-macro (provide-public head . body)
  (if (or (and (symbol? head) (not (defined? head)))
          (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
      `(define-public ,head ,@body)
      '(noop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quit-TeXmacs-scheme) (noop))

(define-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))
