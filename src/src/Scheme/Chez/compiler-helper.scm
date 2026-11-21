;;; compiler-helper.scm -- what compiling one TeXmacs module requires.
;;;
;;; Everything here is used by both the sequential driver (compile-texmacs.scm)
;;; and a compilation worker (compile-worker.scm), which is why it is a file and
;;; not a section of the driver:
;;;
;;;   paths and module names      "a/b/c.scm" <-> "a:b:c" <-> boot/a/b/c.so
;;;   the expand-time environment  eager forms, oracle installs, unbind
;;;   modules loaded in the process  what a module may import
;;;   compiling one module        the compile-file call and its parameters
;;;
;;; Chez expands a whole module before running any of it, and a TeXmacs macro
;;; expander may call a plain procedure while it expands (`texmacs-modes'
;;; expands into `(map texmacs-mode l)').  Those helpers are ordinary
;;; definitions, often private to some unrelated file, so nothing in the module
;;; makes them visible: they have to be evaluated into the compilation
;;; environment first, and taken back out afterwards.
;;;
;;; compiler-oracle.scm knows where a helper is defined and how to install one
;;; (`oracle-bring-in-form!'); this file decides *when*, and around which
;;; compile-file call:
;;;
;;;   Pass 1  `needs-eager-eval?' decides which forms must be evaluated before
;;;           a module can be expanded; `eager-forms-set!' records them.
;;;   Pass 2  `setup-module-environment!' imports the module's prerequisites,
;;;           installs the helpers its eager forms call and evaluates those
;;;           forms; `compile-module-file!' compiles; `unbind-ephemerals!'
;;;           restores the environment, so the next module starts clean.
;;;
;;; Loading
;;;   Plain definitions, not a library: both callers are scripts that resolve
;;;   src/Scheme/Chez themselves and `primitive-load' the files they need in
;;;   order.  Load compiler-oracle.scm first (the mc-* walkers this file's
;;;   predicates use), and import (compiler-scan) before either:
;;;
;;;     (import (compiler-scan))
;;;     (primitive-load ".../compiler-oracle.scm")
;;;     (primitive-load ".../compiler-helper.scm")
;;;
;;;   Two paths are deliberately NOT resolved here: the TeXmacs progs directory
;;;   and the directory holding these files.  The driver needs both before it
;;;   can load this file, so they stay in its prologue.

;; ---- Paths and module names ---------------------------------------------

(define (string-prefix? prefix s)
  (let ((n (string-length prefix)))
    (and (>= (string-length s) n)
         (string=? prefix (substring s 0 n)))))

(define (string-suffix? suffix s)
  (let ((n (string-length suffix))
        (m (string-length s)))
    (and (>= m n)
         (string=? suffix (substring s (- m n) m)))))

(define (strip-scm path)
  (if (string-suffix? ".scm" path)
      (substring path 0 (- (string-length path) 4))
      path))

(define (scm->so path)
  (string-append (strip-scm path) ".so"))

;; Last component of a path: the part after the final `/`, or the whole
;; string when there is none.
(define (path-basename path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((< i 0) path)
          ((char=? (string-ref path i) #\/)
           (substring path (+ i 1) (string-length path)))
          (else (loop (- i 1))))))

(define (env-flag? name)
  (let ((value (getenv name)))
    (and value
         (not (string=? value ""))
         (not (string=? value "0")))))

;; Where transpiled sources and compiled modules are written.
;;
;; TEXMACS_BOOT_PATH takes precedence, mirroring how boot-chez.scm resolves
;; compiled modules at runtime, so that build trees with another name (for
;; instance build-win) work without editing this file. The literals below are
;; the historical default.
(define *boot-dir*
  (or (getenv "TEXMACS_BOOT_PATH")
      (and (file-exists? "build-texmacos/boot") "build-texmacos/boot")
      (and (file-exists? "texmacs/build-texmacos/boot") "texmacs/build-texmacos/boot")
      (and (file-exists? "../build-texmacos/boot") "../build-texmacos/boot")
      "build-texmacos/boot"))

(define (boot-path rel-path)
  (string-append *boot-dir* "/" rel-path))

;; The TeXmacs module tree the transpiler reads.  A process may be started from
;; the repository root or from a build tree, and TEXMACS_PATH wins when it is
;; set, so probe rather than assume.
(define (progs-dir)
  (let ((path (getenv "TEXMACS_PATH")))
    (cond ((and path (file-exists? (string-append path "/progs")))
           (string-append path "/progs"))
          ((file-exists? "TeXmacs/progs") "TeXmacs/progs")
          ((file-exists? "texmacs/TeXmacs/progs") "texmacs/TeXmacs/progs")
          (else "TeXmacs/progs"))))

(define (progs-path rel-path)
  (string-append (progs-dir) "/" rel-path))

(define (dir-separator? c)
  (or (char=? c #\/) (char=? c #\\)))

(define (parent-dir path)
  (let loop ((i (- (string-length path) 1)))
    (cond ((<= i 0) "")
          ((dir-separator? (string-ref path i)) (substring path 0 i))
          (else (loop (- i 1))))))

(define (mkdir-p dir)
  (unless (or (string=? dir "")
              (string=? dir ".")
              (string=? dir "..")
              (and (= (string-length dir) 2) (char=? (string-ref dir 1) #\:))
              (file-exists? dir))
    (let ((parent (parent-dir dir)))
      (unless (string=? parent "")
        (mkdir-p parent)))
    (guard (e (else (unless (file-exists? dir) (raise e))))
      (mkdir dir))))

(define (ensure-dir path)
  (let ((dir (parent-dir path)))
    (unless (string=? dir "")
      (mkdir-p dir))))

;; ---- Module names and paths ----------------------------------------------

(define (mod-name->symbol mod-name)
  (let* ((parts (if (pair? mod-name)
                    (map symbol->string mod-name)
                    (list (symbol->string mod-name))))
         (joined (let loop ((parts parts))
                   (if (null? (cdr parts))
                       (car parts)
                       (string-append (car parts) ":" (loop (cdr parts)))))))
    (string->symbol joined)))

(define (rel-path->mod-name rel-path)
  (let* ((path (strip-scm rel-path))
         (len (string-length path)))
    (let loop ((i 0) (start 0) (parts '()))
      (cond ((= i len)
             (reverse (cons (string->symbol (substring path start i)) parts)))
            ((char=? (string-ref path i) #\/)
             (loop (+ i 1) (+ i 1)
                   (cons (string->symbol (substring path start i)) parts)))
            (else
             (loop (+ i 1) start parts))))))

;; ---- The expand-time environment -----------------------------------------

;; The environment the oracle installs into.  `interaction-environment' is the
;; only one a process has, so this is a handle on it, not a copy.
(unless (top-level-bound? 'compilation-environment)
  (define-top-level-value 'compilation-environment
    (make-parameter (interaction-environment))))

;; ---- Evaluating one definition -------------------------------------------

;; Evaluate `thunk`, reporting any exception as one flat log line
;;   (MESSAGE irritant ... <condition text>)
;; and returning `default` instead of propagating it. A `message` of #f keeps
;; the call silent. Every eager evaluation in the transpiler goes through here,
;; so a failure is attributed to the form that caused it instead of aborting
;; the module.
(define (guard-eval message irritants default thunk)
  (guard (e (else
              (when message
                (display (append (list message) irritants (list (condition-text e))))
                (newline))
              default))
    (thunk)))

;; Install NAME into the current compilation environment, dependencies first.
;; The two rewrite steps matter: the sources contain curried definitions and
;; docstrings, which `define' rejects when evaluated raw.
(define (condition-text condition)
  (if (condition? condition)
      (with-output-to-string (lambda () (display-condition condition)))
      condition))

(define (eval-in-compile! form . opt-message)
  (let ((message (if (pair? opt-message) (car opt-message) 'FAILED-EVAL)))
    (guard-eval message (list form) #f
      (lambda ()
        (parameterize ((interaction-environment (compilation-environment)))
          (eval form (interaction-environment)))))))

;; The symbol a definition form introduces:
;;   (tm-define (foo x) ...)              -> foo
;;   (tm-define foo ...)                  -> foo
;;   (define-table foo ...)               -> foo
;;   (define-top-level-syntax 'foo proc)  -> foo   (the argument is quoted)
(define (defined-name form)
  (let ((head (cadr form)))
    (if (and (pair? head) (eq? (car head) 'quote))
        (cadr head)
        (mc-leftmost-symbol head))))

(define (eval-eager-form! form)
  (let ((head (and (pair? form) (car form))))
    (cond
      ((memq head '(define define-public provide-public define-module-macro
                    define-macro define-public-macro
                    define-state tm-define tm-define-macro define-group
                    define-table define-collection extend-table
                    extend-collection smart-table
                    define-table-decls define-collection-decls
                    define-top-level-syntax))
       (eval-in-compile! form))

      ((and (eq? head 'ahash-set!)
            (pair? (cdr form))
            (eq? (cadr form) 'define-option-table))
       (eval-in-compile! form))

      (else #f))))

(define *eager-gated?*
  (let ((value (getenv "TEXMACS_EAGER_GATE")))
    (not (and value (string=? value "0")))))

;; ---- What has to be eager -----------------------------------------------

;; Expand-time state must stay eager, not just procedures. Skipping these
;; breaks option tables, `define-table' registries consulted during expansion,
;; and group predicates.
(define (needs-eager-eval? form)
  (let ((head (and (pair? form) (car form))))
    (or (not *eager-gated?*)
        (memq head *macro-definers*)
        (memq head '(define-top-level-syntax define-state define-table
                      define-collection extend-table extend-collection
                      smart-table define-group define-table-decls
                      define-collection-decls))
        (and (eq? head 'ahash-set!)
             (pair? (cdr form))
             (eq? (cadr form) 'define-option-table))
        ;; `define-property' and `define-property*' are helper procedures in tm-define.scm
        ;; that construct option handlers for `define-option-table'. They must be eagerly
        ;; evaluated so that:
        ;;   1. They shadow Chez Scheme's built-in `define-property' core syntactic keyword
        ;;      in the compile-time environment before option-table registration runs;
        ;;   2. Subsequent (ahash-set! define-option-table ...) forms can invoke them to
        ;;      populate the table handlers without throwing syntax errors.
        (and (eq? head 'define)
             (pair? (cdr form))
             (memq (mc-leftmost-symbol (cadr form))
                   '(define-property define-property*))))))

;; ---- What one module needs ----------------------------------------------

;; rel-path -> the forms Pass 2 must evaluate before compiling that module.
(define *eager-forms* (make-hashtable equal-hash equal?))

(define (eager-forms-set! rel-path forms)
  (hashtable-set! *eager-forms* rel-path forms))

(define (eager-forms-ref rel-path)
  (hashtable-ref *eager-forms* rel-path '()))

;; A worker process has no Pass 1 to hand it the table, so the forms are also
;; written next to the transpiled module -- boot/a/b.scm and boot/a/b.eager.scm
;; have the same lifetime, so the two cannot disagree.  Pass 1 writes them in
;; `transpile-module'; the worker reads them before setting up the environment.
(define (eager-path trans-scm)
  (string-append (strip-scm trans-scm) ".eager.scm"))

(define (write-eager-forms! trans-scm forms)
  (call-with-output-file (eager-path trans-scm)
    (lambda (port) (write forms port) (newline port))
    'replace))

(define (load-eager-forms! rel-path trans-scm)
  (let ((path (eager-path trans-scm)))
    (eager-forms-set! rel-path
                      (if (file-exists? path)
                          (call-with-input-file path read)
                          '()))))

;; Build this module's expand-time environment: install the helpers its eager
;; forms call, then evaluate the forms themselves.  The caller has already
;; imported the module's declared dependencies.  `compile-export-mode?' is off
;; because these definitions are evaluated, not compiled, and an `export' form
;; is only valid while compiling.
(define (run-eager-forms! rel-path)
  (parameterize ((compile-export-mode? #f))
    (for-each (lambda (form)
                (when *eager-gated?* (oracle-bring-in-form! form))
                (eval-eager-form! form))
              (eager-forms-ref rel-path))))

;; Undo every oracle installation, so the next module starts from the
;; environment the sequential build would have had at this point.  A worker
;; process is long-lived, so it must call this after each job.
(define (unbind-ephemerals!)
  (for-each (lambda (entry)
              (unbind-symbol! entry (interaction-environment))
              (hashtable-delete! *oracle-installed* (car entry)))
            *ephemeral-syms*)
  (set! *ephemeral-syms* '()))

;; ---- Modules loaded in this process -------------------------------------

;; Pass 2: modules loaded in this process, most recent first.
(define compiled-modules '())

(define (note-module-loaded! mod-sym)
  (set! compiled-modules (cons mod-sym compiled-modules)))

;; Import the modules a module declares, so that its macro expanders can call
;; them by name while `compile-file' expands it.  Skipping a module that is not
;; loaded yet is deliberate: Pass 2 walks the tiers in order, so a forward
;; reference is exactly the case the emitted `module-provide' handles.
(define (import-modules! deps)
  (for-each
    (lambda (dependency)
      (let ((module (mod-name->symbol dependency)))
        (when (memq module compiled-modules)
          (eval-in-compile! `(import (for ,module run expand))))))
    deps))

;; Build one module's expand-time environment: import what it declares, then
;; evaluate the forms a macro expander may call.
(define (setup-module-environment! rel-path deps)
  (let ((mod-name (rel-path->mod-name rel-path)))
    (parameterize ((*oracle-current-module* mod-name)
                   (*oracle-current-deps* deps))
      (import-modules! deps)
      (run-eager-forms! rel-path))))

;; ---- Compiling one module -----------------------------------------------

;; Profiling instrumentation for compile-file.  Chez accepts #f, #t, 'source,
;; or 'block; #t is a synonym for 'source.  The value is normalized here so an
;; unset variable leaves the production build byte-identical.  Source profiling
;; records execution counts only (there is no byte/allocation mode); the HTML
;; annotates the generated module under the boot directory.
(define *compile-profile-mode*
  (let ((value (getenv "TEXMACS_COMPILE_PROFILE")))
    (cond ((not value) #f)
          ((string=? value "") #f)
          ((member value '("0" "false" "no")) #f)
          ((member value '("source" "block")) (string->symbol value))
          ((member value '("1" "t" "#t" "true")) #t)
          (else (error 'compiler-helper
                       "TEXMACS_COMPILE_PROFILE must be source, block, or #t"
                       value)))))

;; Optional module filter: only paths with this prefix are instrumented.  It
;; keeps profiling off the kernel (where source annotations can leak into
;; lazily evaluated forms) and on the subsystem under study.
(define *compile-profile-filter*
  (let ((value (getenv "TEXMACS_COMPILE_PROFILE_FILTER")))
    (and value (not (string=? value "")) value)))

(define (compile-profile-mode-for rel-path)
  (and *compile-profile-mode*
       (or (not *compile-profile-filter*)
           (string-prefix? *compile-profile-filter* rel-path))
       *compile-profile-mode*))

;; Compile one transpiled module, then put the environment back.  The
;; dynamic-wind means a failed compile still unbinds what the eager forms
;; installed; the caller repeats this for every module, so a worker can call it
;; once per job.
(define (compile-module-file! rel-path trans-scm out-so)
  (dynamic-wind
    void
    (lambda ()
      (parameterize ((compile-export-mode? #t)
                     (compile-profile (compile-profile-mode-for rel-path)))
        (compile-file trans-scm out-so)))
    unbind-ephemerals!))

;; ---- Keywords ----------------------------------------------------------

(define (tm-collect-keywords form acc)
  (cond ((and (symbol? form)
              (string-prefix? ":" (symbol->string form)))
         (if (member form acc) acc (cons form acc)))
        ((pair? form)
         (tm-collect-keywords (cdr form)
                              (tm-collect-keywords (car form) acc)))
        (else acc)))

(define (tm-bind-keywords form)
  (for-each
    (lambda (keyword)
      (for-each (lambda (env)
                  (unless (top-level-bound? keyword env)
                    (define-top-level-value keyword keyword env)))
                (list (compilation-environment) (interaction-environment))))
    (tm-collect-keywords form '())))

;; ---- Progress reporting -------------------------------------------------

(define total 0)
(define current 0)

(define (progress-start! n)
  (set! total n)
  (set! current 0))

(define (progress-tick!)
  (set! current (+ current 1))
  current)

(define (string-pad-left s width)
  (let ((n (- width (string-length s))))
    (if (> n 0) (string-append (make-string n #\space) s) s)))

;; Floor, so that only the last module reports 100%.
(define (progress-pct idx n)
  (if (zero? n) 100 (quotient (* 100 idx) n)))

(define (progress-pct-tag idx n)
  (string-pad-left (string-append (number->string (progress-pct idx n)) "%") 4))

(define (format-progress-prefix idx n)
  (string-append "[" (progress-pct-tag idx n)
                 " " (string-pad-left (number->string idx)
                                      (string-length (number->string n)))
                 "/" (number->string n) "] "))

;; Flushed per line so a redirected CI log shows progress as it happens.
(define (emit-progress-line text)
  (display text)
  (newline)
  (flush-output-port (current-output-port)))

(define (log-progress action detail)
  (emit-progress-line
    (string-append (format-progress-prefix current total) action " " detail)))

;; Aggregate steps (the kernel snapshot, the dependency manifest) have no
;; index of their own; report the percentage only.
(define (log-progress-pct action detail)
  (emit-progress-line
    (string-append "[" (progress-pct-tag current total) "] " action " " detail)))

;; Detail line belonging to the preceding progress line: error text, notices.
(define (log-note text)
  (emit-progress-line
    (string-append "  " (if (string? text) text (format "~a" text)))))

