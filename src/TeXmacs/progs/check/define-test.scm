;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : define-test.scm
;; DESCRIPTION : tests of tm-define, modes and lazy definitions
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TeXmacs is extended from Scheme with the macros of
;; kernel/texmacs/tm-define.scm, kernel/texmacs/tm-modes.scm and
;; kernel/boot/boot.scm. The tests check
;;
;;   - overloading: each tm-define of an existing name wraps the previous
;;     definition, which its body can call as former; the conditions of
;;     :require and :mode decide whether the new body or former runs, so
;;     the latest definition whose conditions hold wins (there is no
;;     ordering by specificity);
;;   - the properties (:synopsis, :interactive, :argument...) and how
;;     property, help and tm-property read and change them;
;;   - the bookkeeping tables (sources, names, defining modules);
;;   - tm-define-macro;
;;   - modes defined with texmacs-modes, their predicates and the sub-mode
;;     relation which the dependencies between modes give;
;;   - lazy-define and the module macros (texmacs-module :use and
;;     :inherit, define-public, provide-public).
;;
;; Everything defined here lives in the running TeXmacs, so the names all
;; start with define-test-. Nothing here needs a buffer or the GUI: the
;; modes of TeXmacs which look at the cursor (in-math% and so on) are not
;; tested, only modes defined here.

(texmacs-module (check define-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions under test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; overloading by :require, later definitions first
(tm-define (define-test-f x) (list 'base x))
(tm-define (define-test-f x) (:require (number? x)) (list 'number x))
(tm-define (define-test-f x) (:require (integer? x)) (list 'integer x))

;; a less specific condition defined later hides a more specific one
(tm-define (define-test-o x) 'base)
(tm-define (define-test-o x) (:require (integer? x)) 'integer)
(tm-define (define-test-o x) (:require (number? x)) 'number)

;; former is the previous definition
(tm-define (define-test-former x) (list 'base x))
(tm-define (define-test-former x)
  (:require (> x 10))
  (list 'big (former x)))
(tm-define (define-test-former x)
  (:require (> x 100))
  (list 'huge (former x)))

;; an unconditional redefinition replaces the definition, but may still
;; call the former one
(tm-define (define-test-wrap x) (* x 2))
(tm-define (define-test-wrap x) (+ (former x) 1))

;; several conditions are all required
(tm-define (define-test-two x y) 'base)
(tm-define (define-test-two x y)
  (:require (number? x))
  (:require (number? y))
  'both)

;; rest arguments: former is applied to all of them
(tm-define (define-test-rest . args) (cons 'base args))
(tm-define (define-test-rest . args)
  (:require (= (length args) 2))
  (cons 'two (apply former args)))
(tm-define (define-test-rest a . args)
  (:require (number? a))
  (list 'number a (apply former a args)))

;; a curried head
(tm-define ((define-test-curry a) b) (list a b))

;; a first definition with a condition gets a former which does nothing
;; (this prints a warning "conditional master routine" when loaded)
(tm-define (define-test-master x)
  (:require (number? x))
  (list 'number x))

;; properties
(tm-define (define-test-prop x)
  (:synopsis "Synopsis of @x")
  (:argument x "The argument")
  (:default x 3)
  (:proposals x '(1 2))
  (:interactive #t)
  (:secure #t)
  (:check-mark "v" number?)
  (:balloon (lambda () "balloon"))
  (:type (-> int int))
  (:returns "result")
  (:note "note")
  (:applicable #t)
  x)

(tm-define (define-test-syn*)
  (:synopsis* "Both synopses")
  'syn)

(tm-define (define-test-later) (:synopsis "first") 1)
(tm-property (define-test-later) (:synopsis "second"))

(tm-define (define-test-redef) (:synopsis "old") 'old)
(tm-define (define-test-redef) (:synopsis "new") 'new)

;; a macro, implemented by the function define-test-mac$impl
(tm-define-macro (define-test-mac x)
  (list 'quote (list x x)))
(tm-define-macro (define-test-mac2 . l)
  `(list ,@(reverse l)))

;; modes
(define define-test-flag #f)
(tm-define (define-test-flag-value) define-test-flag)

(texmacs-modes
  (define-test-on% #t)
  (define-test-off% #f)
  (define-test-flag% (define-test-flag-value))
  (define-test-on-sub% #t define-test-on%)
  (define-test-off-sub% #t define-test-off%)
  (define-test-deep% #t define-test-on-sub%)
  (define-test-both% #t define-test-on% define-test-flag%)
  (define-test-own% #f define-test-on%))

;; overloading by mode
(tm-define (define-test-m) 'base)
(tm-define (define-test-m) (:mode define-test-on?) (list 'on (former)))
(tm-define (define-test-m) (:mode define-test-off?) 'off)
(tm-define (define-test-m) (:mode define-test-off-sub?) 'off-sub)

(tm-define (define-test-dyn) 'normal)
(tm-define (define-test-dyn) (:mode define-test-flag?) 'flagged)

;; a mode and a condition on the arguments together
(tm-define (define-test-mr x) 'base)
(tm-define (define-test-mr x)
  (:mode define-test-flag?)
  (:require (number? x))
  'flagged-number)

;; module macros
(provide-public (define-test-pp) 1)
(provide-public (define-test-pp) 2)
(provide-public define-test-pv 10)
(provide-public (car x) 'not-car)
(define-public (define-test-pub) 'pub)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-overloading)
  ;; the latest definition whose conditions hold runs
  (check-group "overloading by condition")
  (check= (define-test-f 1) '(integer 1))
  (check= (define-test-f 1.5) '(number 1.5))
  (check= (define-test-f "a") '(base "a"))
  (check= (define-test-two 1 2) 'both)
  (check= (define-test-two 1 'a) 'base)
  (check= (define-test-two 'a 2) 'base)
  ;; there is no ordering by specificity: only the order of definition
  (check= (define-test-o 1) 'number)
  (check= (define-test-o 1.5) 'number)
  (check= (define-test-o 'a) 'base)
  (check= ((define-test-curry 1) 2) '(1 2))
  ;; the first definition had a condition: when it fails, former does
  ;; nothing and returns its value (#f here)
  (check= (define-test-master 1) '(number 1))
  (check-false (define-test-master "a")))

(define (test-former)
  ;; former is the definition which was current when the new one was made
  (check-group "former")
  (check= (define-test-former 5) '(base 5))
  (check= (define-test-former 50) '(big (base 50)))
  (check= (define-test-former 500) '(huge (big (base 500))))
  (check= (define-test-wrap 3) 7)
  ;; rest arguments are passed on with apply
  (check= (define-test-rest 'a) '(base a))
  (check= (define-test-rest 'a 'b) '(two base a b))
  (check= (define-test-rest 1) '(number 1 (base 1)))
  (check= (define-test-rest 1 2) '(number 1 (two base 1 2)))
  ;; the latest head decides the arity: (a . args) needs one argument
  (check-error (define-test-rest) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-properties)
  ;; quoted properties keep the option's arguments as a list, evaluated
  ;; ones (:interactive, :secure, :check-mark, :balloon) the list of their
  ;; values; :default and :proposals become thunks
  (check-group "properties")
  (check= (property 'define-test-prop :synopsis) '("Synopsis of @x"))
  (check= (property define-test-prop :synopsis) '("Synopsis of @x"))
  (check= (help define-test-prop) '("Synopsis of @x"))
  (check= (property 'define-test-prop :arguments) '(x))
  (check= (property 'define-test-prop '(:argument x)) '("The argument"))
  (check= ((property 'define-test-prop '(:default x))) 3)
  (check= ((property 'define-test-prop '(:proposals x))) '(1 2))
  (check= (property 'define-test-prop :interactive) '(#t))
  (check= (property 'define-test-prop :secure) '(#t))
  (check= (property 'define-test-prop :check-mark) (list "v" number?))
  (check= ((car (property 'define-test-prop :balloon))) "balloon")
  (check= (property 'define-test-prop :type) '((-> int int)))
  (check= (property 'define-test-prop :returns) '("result"))
  (check= (property 'define-test-prop :note) '("note"))
  (check-true (procedure? (car (property 'define-test-prop :applicable))))
  (check-true ((car (property 'define-test-prop :applicable)) 'any))
  (check-false (property 'define-test-prop :balloon-not-a-property))
  (check-false (property 'define-test-f :synopsis))
  (check-false (help define-test-f))
  (check= (define-test-prop 4) 4)
  ;; :synopsis* sets :synopsis too
  (check= (property 'define-test-syn* :synopsis) '("Both synopses"))
  (check= (property 'define-test-syn* :synopsis*) '("Both synopses"))
  ;; the latest value of a property wins
  (check= (property 'define-test-later :synopsis) '("second"))
  (check= (define-test-later) 1)
  (check= (property 'define-test-redef :synopsis) '("new"))
  (check= (define-test-redef) 'new)
  ;; FIXME: a tm-define or tm-property which has both a condition (:mode
  ;; or :require) and a property fails with wrong-type-arg in >=
  ;; (filter-conds in tm-define.scm expects the conditions as kind/value
  ;; pairs, but ctx-add-condition only keeps the values), for instance
  ;;   (tm-define (f x) (:require (number? x)) (:synopsis "s") x)
  ;; It also leaves the function defined without its property.
  ;; unknown options are refused when the definition is expanded
  (check-error (eval '(tm-define (define-test-bad) (:no-such-option 1) 1)
                     (current-module))
               'texmacs-error)
  (check-false (defined? 'define-test-bad)))

(define (test-bookkeeping)
  ;; tm-define keeps the sources (latest first), the name of each version
  ;; and the module of each definition
  (check-group "bookkeeping")
  (check= (length (procedure-sources define-test-f)) 3)
  (check= (car (procedure-sources define-test-wrap))
          '(lambda (x) (+ (former x) 1)))
  (check= (procedure-name define-test-f) 'define-test-f)
  (check= (procedure-name define-test-wrap) 'define-test-wrap)
  (check= (ahash-ref tm-defined-module 'define-test-f)
          '((check define-test) (check define-test) (check define-test)))
  ;; tm-property adds a property, not a definition
  (check= (length (procedure-sources define-test-later)) 1)
  ;; the definitions are global, in the module texmacs-user
  (check-true (module-defined? texmacs-user 'define-test-f))
  (check-true (predicate-option? 'foo?))
  (check-false (predicate-option? 'foo))
  (check-true (predicate-option? '(lambda (x) x)))
  (check= (tm-macroify 'foo) 'foo$impl)
  (check= (tm-macroify '(foo x)) '(foo$impl x)))

(define (test-contexts)
  ;; the context lists behind properties: newest entry first
  (check-group "contexts")
  (let* ((c1 (ctx-insert #f 'd1 '(c1)))
         (c2 (ctx-insert c1 'd2 '(c2))))
    (check= c1 '(((c1) . d1)))
    (check= (ctx-find c2 '(c1)) 'd1)
    (check= (ctx-find c2 '(c2)) 'd2)
    (check-false (ctx-find c2 '(c3)))
    (check= (ctx-remove c2 '(c1)) '(((c2) . d2))))
  (let ((ctx (list (cons (list (lambda (x) (> x 5))) 'big)
                   (cons '() 'any))))
    (check= (ctx-resolve ctx '(7)) 'big)
    (check= (ctx-resolve ctx '(3)) 'any))
  (check-false (ctx-resolve '() '()))
  (check= (ctx-add-condition '(a) 0 'b) '(a b)))

(define (test-macros)
  (check-group "tm-define-macro")
  (check= (define-test-mac 3) '(3 3))
  (check= (define-test-mac (+ 1 2)) '((+ 1 2) (+ 1 2)))
  (check= (define-test-mac2 1 2 3) '(3 2 1))
  (check-true (procedure? define-test-mac$impl))
  (check= (define-test-mac$impl 'a) '(quote (a a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-modes)
  ;; foo% defines the predicate foo?, which also requires the modes foo%
  ;; depends on
  (check-group "mode predicates")
  (check-true (define-test-on?))
  (check-false (define-test-off?))
  (check-true (define-test-on-sub?))
  (check-false (define-test-off-sub?))
  (check-true (define-test-deep?))
  (check-false (define-test-own?))
  (check-true (texmacs-in-mode? 'define-test-on%))
  (check-true (texmacs-in-mode? 'define-test-on?))
  (check-false (texmacs-in-mode? 'define-test-off-sub%))
  (check-false (texmacs-in-mode? 'define-test-no-such-mode%))
  (check= (symbol-procedure 'define-test-on%) define-test-on?)
  (check= (texmacs-mode-mode 'define-test-on?) 'define-test-on%)
  (check= (texmacs-mode-mode define-test-on?) 'define-test-on%)
  ;; the modes are evaluated each time
  (set! define-test-flag #f)
  (check-false (define-test-flag?))
  (check-false (define-test-both?))
  (set! define-test-flag #t)
  (check-true (define-test-flag?))
  (check-true (define-test-both?))
  (set! define-test-flag #f)

  ;; a mode is a sub-mode of the modes it depends on, transitively
  (check-group "sub-modes")
  (check-true (texmacs-submode? 'define-test-on-sub? 'define-test-on?))
  (check-true (texmacs-submode? define-test-on-sub? define-test-on?))
  (check-true (texmacs-submode? 'define-test-deep? 'define-test-on?))
  (check-true (texmacs-submode? 'define-test-both? 'define-test-flag?))
  (check-false (texmacs-submode? 'define-test-on? 'define-test-on-sub?))
  (check-false (texmacs-submode? 'define-test-on-sub? 'define-test-off?))
  (check-true (texmacs-submode? 'define-test-on? 'always?))
  (check-true (texmacs-submode? 'prevail? 'define-test-on?))

  ;; :mode is a condition like :require
  (check-group "overloading by mode")
  (check= (define-test-m) '(on base))
  (set! define-test-flag #f)
  (check= (define-test-dyn) 'normal)
  (check= (define-test-mr 1) 'base)
  (set! define-test-flag #t)
  (check= (define-test-dyn) 'flagged)
  (check= (define-test-mr 1) 'flagged-number)
  (check= (define-test-mr 'a) 'base)
  (set! define-test-flag #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-lazy)
  ;; init-texmacs.scm declares cas->stree with lazy-define; forcing it
  ;; loads (utils cas cas-out), whose definition overloads the stub
  (check-group "lazy-define")
  (lazy-define-force 'cas->stree)
  (check-true (in? '(utils cas cas-out)
                   (ahash-ref tm-defined-module 'cas->stree)))
  (check= (length (procedure-sources cas->stree)) 2)
  (check-true (pair? (cas->stree '(+ a b))))
  ;; forcing it again does nothing
  (lazy-define-force 'cas->stree)
  (check= (length (procedure-sources cas->stree)) 2)
  ;; a name which is defined already is left alone
  (eval '(lazy-define (check define-test-nowhere) define-test-f)
        (current-module))
  (check= (length (procedure-sources define-test-f)) 3)
  (check= (define-test-f 1) '(integer 1))
  ;; a new name gets a stub, a tm-define of any number of arguments
  (eval '(lazy-define (check define-test-nowhere) define-test-lazy)
        (current-module))
  (check-true (procedure? define-test-lazy))
  (check= (length (procedure-sources define-test-lazy)) 1)
  (check= (cadar (procedure-sources define-test-lazy)) 'args)
  ;; FIXME: calling a stub whose module does not define the name loops
  ;; forever instead of raising "Could not retrieve": the stub looks the
  ;; name up in the public interface of texmacs-user, where it finds
  ;; itself (lazy-define-one in tm-define.scm never uses the module m it
  ;; resolves), e.g. (lazy-define (check check-lib) foo) (foo) hangs.
  ;; FIXME: lazy-define with options, such as
  ;;   (lazy-define (m) (:interactive #t) foo)
  ;; fails when expanded: it maps lazy-define-one over all the names,
  ;; options included, instead of over the real names.
  (check-error (eval '(lazy-define (check define-test-nowhere)
                        (:interactive #t) define-test-lazy-opt)
                     (current-module))
               #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; texmacs-module switches the current module, so it is evaluated here
;; and the current module is restored afterwards
(define (define-test-make-module form)
  (let ((here (current-module)))
    (eval form here)
    (set-current-module here)))

(define (test-modules)
  (check-group "provide-public and define-public")
  ;; provide-public only defines names which are not defined yet
  (check= (define-test-pp) 1)
  (check= define-test-pv 10)
  (check= (car '(1 2)) 1)
  (check= (define-test-pub) 'pub)
  ;; (the module of this file, not the current one: when the suite runs,
  ;; the current module is the one which called it)
  (check-true (module-defined?
               (module-public-interface (resolve-module '(check define-test)))
               'define-test-pub))

  (check-group "texmacs-module")
  (define-test-make-module
    '(texmacs-module (check define-test-mod-a) (:use (check check-lib))))
  (define-test-make-module
    '(texmacs-module (check define-test-mod-b)))
  (let ((a (resolve-module '(check define-test-mod-a)))
        (b (resolve-module '(check define-test-mod-b))))
    ;; :use imports the public macros of check-lib; every module sees
    ;; texmacs-user and so the kernel and the tm-defined functions
    (check-true (module-defined? a 'check=))
    (check-false (module-defined? b 'check=))
    (check-true (module-defined? b 'tm-define))
    (check-true (module-defined? b 'check-suite))
    ;; define-public exports, define does not
    (eval '(define-public (define-test-mod-f) 42) a)
    (eval '(define (define-test-mod-private) 43) a)
    (check-true (module-defined? (module-public-interface a)
                                 'define-test-mod-f))
    (check-false (module-defined? (module-public-interface a)
                                  'define-test-mod-private))
    (check-false (module-defined? b 'define-test-mod-f))
    (check-false (defined? 'define-test-mod-f))
    ;; :inherit uses a module and exports its public names again
    (define-test-make-module
      '(texmacs-module (check define-test-mod-c)
         (:inherit (check define-test-mod-a))))
    (let ((c (resolve-module '(check define-test-mod-c))))
      (check-true (module-defined? (module-public-interface c)
                                   'define-test-mod-f))
      (check= (eval '(define-test-mod-f) c) 42))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (define-test-failures)
  (check-suite "tm-define")
  (test-overloading)
  (test-former)
  (test-properties)
  (test-bookkeeping)
  (test-contexts)
  (test-macros)
  (test-modes)
  (test-lazy)
  (test-modules)
  (check-end))
