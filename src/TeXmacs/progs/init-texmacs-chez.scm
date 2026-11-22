;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-texmacs-chez.scm
;; DESCRIPTION : Chez Scheme runtime bootstrap (normal startup only)
;; COPYRIGHT   : (C) 2026
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the
;; file LICENSE in the root directory or
;; <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (top-level-bound? '*init-texmacs-chez-loaded*)
  (display "WARNING: init-texmacs-chez.scm loaded more than once!\n"))
(define *init-texmacs-chez-loaded* #t)

;; --- Module Load Tracing & Verification --------------------------------------
(define *traced-so-files* '())
(define *traced-so-table* (make-hashtable string-hash string=?))

(define (has-prefix? prefix s)
  (and (string? s) (string? prefix)
       (let ((plen (string-length prefix))
             (slen (string-length s)))
         (and (>= slen plen)
              (string=? (substring s 0 plen) prefix)))))

(define (has-suffix? suffix s)
  (and (string? s) (string? suffix)
       (let ((suflen (string-length suffix))
             (slen (string-length s)))
         (and (>= slen suflen)
              (string=? (substring s (- slen suflen) slen) suffix)))))

(define (normalize-traced-path path)
  (let* ((boot-env (getenv "TEXMACS_BOOT_PATH"))
         (candidates (list (if boot-env (string-append boot-env "/") "")
                           "build-texmacos/boot/"
                           "boot/")))
    (let loop ((c candidates))
      (cond ((null? c) path)
            ((and (not (string=? (car c) ""))
                  (has-prefix? (car c) path))
             (substring path (string-length (car c)) (string-length path)))
            (else (loop (cdr c)))))))

(define (register-traced-so! path)
  (let ((rel (normalize-traced-path path)))
    (unless (hashtable-ref *traced-so-table* rel #f)
      (hashtable-set! *traced-so-table* rel #t)
      ;; Record in completion order (LOAD-FINISH: dependencies finish before callers)
      (set! *traced-so-files* (cons rel *traced-so-files*))
      (when (getenv "TEXMACS_TRACE_VERBOSE")
        (display (string-append "[TRACE-SO] " rel "\n"))))))

(define orig-primitive-load primitive-load)
(set! primitive-load
  (lambda (path)
    (let ((res (orig-primitive-load path)))
      (when (and (string? path) (has-suffix? ".so" path))
        (register-traced-so! path))
      res)))

;; This script is the default tm_init_file under TEXMACS_ENGINE_CHEZ.  Compile
;; mode uses init-chez-compile.scm and lint mode uses init-linter.scm, so every
;; form below is unconditional runtime initialization.

;; --- Chain to the standard initialization ------------------------------------
;;
;; Route every subsequent load through the engine's guarded loader, so a
;; failing file reports LOAD-ERROR with its condition and backtrace instead of
;; aborting the whole subtree silently.

(define load
  (lambda (f . ignore-env)
    (tm-load-file f)))

;; --- Bootstrap kernel modules directly from compiled .so binaries ------------

(unless (top-level-bound? 'notify-preferences-booted)
  (define-top-level-value 'notify-preferences-booted (lambda () (noop))))


;; Note: In this ahead-of-time compiled architecture, module inheritance and
;; scope relationships are resolved statically during compilation into the .so
;; modules (via internal module imports and topological compilation order).
;; Therefore, runtime `inherit-modules` is not needed; we simply load all
;; prerequisites into the runtime environment via `use-modules`.
(use-modules (kernel boot chez-compat)
             (kernel boot compat) (kernel boot abbrevs)
             (kernel boot debug) (kernel boot srfi)
             (kernel boot ahash-table) (kernel boot prologue))
(use-modules (kernel library base) (kernel library list)
             (kernel library tree) (kernel library content)
             (kernel library patch))
(use-modules (kernel regexp regexp-match) (kernel regexp regexp-select))
(use-modules (kernel logic logic-bind) (kernel logic logic-unify)
             (kernel logic logic-rules) (kernel logic logic-query)
             (kernel logic logic-data))
(use-modules (kernel texmacs tm-define)
             (kernel texmacs tm-program-languages)
             (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
             (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
             (kernel texmacs tm-convert) (kernel texmacs tm-dialogue)
             (kernel texmacs tm-language) (kernel texmacs tm-file-system)
             (kernel texmacs tm-states))
(use-modules (kernel gui gui-markup)
             (kernel gui menu-define) (kernel gui menu-widget)
             (kernel gui kbd-define)
             (kernel gui speech-define)
             (kernel gui kbd-handlers)
             (kernel gui menu-test)
             (kernel old-gui old-gui-widget)
             (kernel old-gui old-gui-factory)
             (kernel old-gui old-gui-form)
             (kernel old-gui old-gui-test))

;; texmacs-user is already bound to the interaction environment above; the
;; kernel bundle only needs its own name for the aggregate module's import.
(unless (top-level-bound? 'texmacs-user-kernel)
  (define-top-level-value 'texmacs-user-kernel (interaction-environment)))


;; --- Runtime overrides for the export-bearing definition macros ---------------
;;
;; boot-chez.scm's define-public / define-public-macro run through
;; with-export-if-active, which emits (export ...) while compile-export-mode?
;; is active.  `export' is only legal inside a module or library, so source
;; loaded at runtime (plugins, -x scripts, the REPL) must see these
;; export-free versions instead.  They are installed after the kernel imports
;; and before any editor registration, as plain top-level syntax.

(define-syntax define-public
  (syntax-rules ()
    ((_ (name . args) body ...)
     (define (name . args) body ...))
    ((_ name val)
     (define name val))))

(define-syntax indirect-export
  (syntax-rules ()
    ((_ id ...) (noop))))

(define-syntax define-public-macro
  (syntax-rules ()
    ((_ (nm . formals) body ...)
     (define-macro (nm . formals) body ...))
    ((_ nm val)
     (define-macro nm val))))

;; --- Register the editor program ---------------------------------------------
;;
;; The shared initialization of Guile, s7 and femtolisp (init-kernel.scm and
;; init-texmacs.scm), compiled in the boot directory by the module compiler
;; (compile-guile-script!), else read as sources.

(tm-load-init 'init-kernel)
(tm-load-init 'init-texmacs)

(define (dump-boot-order-manifest! out-path)
  (let* ((boot-chez "kernel/boot/boot-chez.so")
         (ordered-list (cons boot-chez (reverse *traced-so-files*))))
    (display (string-append "\n[TRACE] Writing boot loading order manifest ("
                            (number->string (length ordered-list))
                            " modules) to " out-path "...\n"))
    (call-with-output-file out-path
      (lambda (port)
        (display ";;; Auto-generated sequential boot loading order (DFS post-order)\n" port)
        (write ordered-list port)
        (newline port))
      'replace)
    (display "[TRACE] Boot order manifest written successfully.\n")))

(when (or (getenv "TEXMACS_TRACE_MODULES")
          (getenv "TEXMACS_DUMP_BOOT_ORDER"))
  (let ((dump-file (or (getenv "TEXMACS_DUMP_BOOT_ORDER")
                       (let ((b (getenv "TEXMACS_BOOT_PATH")))
                         (if b (string-append b "/boot-order.scm") "build-texmacos/boot/boot-order.scm")))))
    (dump-boot-order-manifest! dump-file)))


