;;; convert-to-r6rs.scm -- Pass 1: pristine sources to Chez Scheme modules.
;;;
;;; This is the half of the build that reads TeXmacs/progs and writes under the
;;; boot directory.  It loads nothing into the process, evaluates no definition
;;; of its own accord and generates no native code, so its result does not
;;; depend on what was compiled before it: the same sources always produce the
;;; same module files.
;;;
;;; It also owns the two things that decide *what* to transpile and what may be
;;; compiled next:
;;;
;;;   - the compiler database (source hashes, artifact hashes, the dependency
;;;     graph) and therefore the dirty set;
;;;   - the kernel snapshot `texmacs-user.so', which has to exist before any
;;;     worker can boot, because a worker loads it as its starting environment.
;;;
;;; The sequential driver (compile-texmacs.scm) loads this file, and the pool
;;; coordinator will load the same file once it exists (compile-pool.scm,
;;; Milestone 3), so Pass 1 has one implementation and the coordinator can get
;;; the module list, the dirty set and the graph in memory instead of
;;; re-reading build artifacts.
;;;
;;; Loading
;;;   Plain definitions, loaded after the prologue those two scripts repeat:
;;;   (compiler-db), (compiler-scan), (compile-graph),
;;;   (compile-inventory), compiler-oracle.scm and compiler-helper.scm.
;;;   Loading this file indexes the tree once and opens the compiler database,
;;;   so it does carry load-time work.

;; ---- Tree index (Pass 2 and every worker install from it) ----------------

(oracle-index-tree! *oracle-defs* (progs-dir))

;; ========================================================
;; Incremental & Selective Compilation Configuration
;; ========================================================

(define (file-newer? f1 f2)
  (and (file-exists? f1)
       (or (not (file-exists? f2))
           (time>? (file-modification-time f1)
                   (file-modification-time f2)))))

(define *recompile-all?*
  (or (and (top-level-bound? '*recompile-all*) *recompile-all?*)
      (env-flag? "TEXMACS_RECOMPILE_ALL")))

(define (module-needs-recompile? rel-path trans-scm out-so)
  (or *recompile-all?*
      (not (file-exists? out-so))
      (not (file-exists? trans-scm))
      (hashtable-ref *dirty-modules* (rel-path->mod-name rel-path) #f)))

(when *recompile-all?*
  (emit-progress-line "mode: recompile from scratch (clean build)"))

(define *definition-heads*
  '(define define-syntax define-public define-public-macro define-module-macro
    define-state provide-public define-macro define-menu define-widget
    define-record-type define-values bib-define-style texmacs-modes
    cas-define-test import export indirect-export))

(define (def-form? form)
  (and (pair? form) (memq (car form) *definition-heads*)))

(define (has-def? form)
  (cond ((def-form? form) #t)
        ((and (pair? form) (eq? (car form) 'begin))
         (exists has-def? (cdr form)))
        (else #f)))

(define (cond-def? form)
  (and (pair? form)
       (case (car form)
         ((if)
          (or (and (pair? (cddr form)) (has-def? (caddr form)))
              (and (pair? (cdddr form)) (has-def? (cadddr form)))))
         ((when unless)
          (and (pair? (cddr form)) (has-def? (cons 'begin (cddr form)))))
         (else #f))))

;; ---- The compiler database ------------------------------------------------

(define module-exports-table (make-eq-hashtable))

;; Database persistence path and previous build's saved database.
(define *compiler-db-file*
  (string-append *boot-dir* "/compiler-db.fasl"))

(define *saved-db*
  (read-compiler-db-or-empty *compiler-db-file*))

;; Digest of a compiled .so artifact for integrity checking.
(define (compute-file-hash path)
  (and (file-exists? path)
       (let ((port (open-file-input-port path)))
         (dynamic-wind
           void
           (lambda ()
             (let ((bytes (get-bytevector-all port)))
               (if (eof-object? bytes) 0 (equal-hash bytes))))
           (lambda () (close-port port))))))

(define (update-compiler-db-artifact! mod-name artifact-hash built-at)
  (let ((info (compiler-db-ref *compiler-db* mod-name)))
    (when info
      (compiler-db-set!
        *compiler-db*
        (make-module-info
          (module-info-name info)
          (module-info-path info)
          (module-info-exports info)
          (module-info-global-values info)
          (module-info-global-macros info)
          (module-info-macro-impls info)
          (module-info-deps info)
          (module-info-compile-uses info)
          (module-info-runtime-uses info)
          (module-info-interface-hash info)
          (module-info-source-hash info)
          artifact-hash
          built-at)))))

(define (update-compiler-db-exports! mod-name exports)
  (let ((info (compiler-db-ref *compiler-db* mod-name)))
    (when info
      (compiler-db-set!
        *compiler-db*
        (make-module-info
          (module-info-name info)
          (module-info-path info)
          exports
          (module-info-global-values info)
          (module-info-global-macros info)
          (module-info-macro-impls info)
          (module-info-deps info)
          (module-info-compile-uses info)
          (module-info-runtime-uses info)
          (scan-interface-hash exports
                               (module-info-global-macros info)
                               (module-info-macro-impls info))
          (module-info-source-hash info)
          (module-info-artifact-hash info)
          (module-info-built-at info))))))

;; First pass: read every whitelisted file once and record its interface.
(define *compiler-db*
  (scan-source-files->db (progs-dir) *source-files*))

;; Pre-populate artifact hashes and exports for cached modules from *saved-db* or existing .so
(for-each
  (lambda (info)
    (let* ((name (module-info-name info))
           (old (compiler-db-ref *saved-db* name))
           (out-so (string-append *boot-dir* "/" (scm->so (module-info-path info)))))
      (when (and old
                 (equal? (module-info-source-hash old)
                         (module-info-source-hash info)))
        (update-compiler-db-exports! name (module-info-exports old)))
      (cond ((and old (module-info-artifact-hash old))
             (update-compiler-db-artifact! name
                                           (module-info-artifact-hash old)
                                           (module-info-built-at old)))
            ((file-exists? out-so)
             (update-compiler-db-artifact! name
                                           (compute-file-hash out-so)
                                           (time-second (current-time)))))))
  (compiler-db-modules *compiler-db*))

;; The graph is built once and shared: the cycle report reads it, and so does
;; every import decision inside transpile-module.
(define *compile-graph* (make-compile-graph-from-db *compiler-db*))

;; Module -> the SCC it belongs to, for cycle members only. Modules outside a
;; cycle are absent, so a lookup returns '() and the test below stays a single
;; list search with no #f sentinel to special-case.
(define *cycle-mates*
  (let ((table (make-hashtable equal-hash equal?)))
    (for-each (lambda (cycle)
                (for-each (lambda (module)
                            (hashtable-set! table module cycle))
                          cycle))
              (compile-graph-cycles *compile-graph*))
    table))

;; `member`, not `memq`: mod-names are lists, and two equal mod-names are not
;; the same object.
(define (cycle-mate? module dependency)
  (member dependency (hashtable-ref *cycle-mates* module '())))

;; ========================================================
;; Incremental Dependency Propagation
;; ========================================================

(define (artifact-changed? old-info out-so)
  (and old-info
       (module-info-artifact-hash old-info)
       (not (equal? (compute-file-hash out-so)
                    (module-info-artifact-hash old-info)))))

(define (source-changed? old-info new-info)
  (and old-info
       new-info
       (module-info-source-hash old-info)
       (module-info-source-hash new-info)
       (not (= (module-info-source-hash old-info)
               (module-info-source-hash new-info)))))

(define (module-directly-changed? rel-path)
  (let* ((trans-scm (boot-path rel-path))
         (out-so (scm->so trans-scm))
         (src (progs-path rel-path))
         (name (rel-path->mod-name rel-path))
         (old-info (compiler-db-ref *saved-db* name))
         (new-info (compiler-db-ref *compiler-db* name)))
    (or ;; Missing outputs
        (not (file-exists? out-so))
        (not (file-exists? trans-scm))
        ;; 1. Artifact integrity against the saved database record
        (artifact-changed? old-info out-so)
        ;; 2. Source content hash (defeats spurious mtime changes from touch/git)
        (source-changed? old-info new-info)
        ;; 3. Filesystem mtime, when database hashes are unavailable
        (file-newer? src out-so)
        (file-newer? trans-scm out-so))))

;; Kernel modules inherit from each other and every downstream tier depends on
;; the texmacs-user snapshot. If any of them is dirty, the whole tree is.
(define (compute-dirty-modules!)
  (define dirty (make-hashtable equal-hash equal?))
  (define queue '())
  (define (mark-dirty! name)
    (unless (hashtable-ref dirty name #f)
      (hashtable-set! dirty name #t)
      (set! queue (cons name queue))))
  (define (mark-rel! rel) (mark-dirty! (rel-path->mod-name rel)))

  (let* ((changed
          (let ((table (make-hashtable equal-hash equal?)))
            (unless *recompile-all?*
              (for-each (lambda (rel)
                          (when (module-directly-changed? rel)
                            (hashtable-set! table (rel-path->mod-name rel) #t)))
                        *source-files*))
            table))
         (kernel-dirty?
          (or *recompile-all?*
              (not (file-exists? (boot-path "texmacs-user.so")))
              (exists (lambda (rel)
                        (hashtable-ref changed (rel-path->mod-name rel) #f))
                      *kernel-files*))))
    (if kernel-dirty?
        (for-each mark-rel! *source-files*)
        (vector-for-each mark-dirty! (hashtable-keys changed)))

    ;; Transitive successor propagation via compile-graph.
    (let ((dependents (compile-graph-dependents *compile-graph*)))
      (let propagate ()
        (unless (null? queue)
          (let ((module (car queue)))
            (set! queue (cdr queue))
            (for-each mark-dirty! (hashtable-ref dependents module '()))
            (propagate)))))
    (values dirty kernel-dirty?)))

(define-values (*dirty-modules* *any-kernel-dirty?*)
  (compute-dirty-modules!))

;; ========================================================
;; Dependency graph report (TEXMACS_COMPILE_GRAPH=1)
;; ========================================================

(define (report-tier full title files)
  (let* ((graph (compile-graph-subgraph full (map rel-path->mod-name files)))
         (waves (compile-graph-waves graph))
         (cycles (compile-graph-cycles graph)))
    (format #t "  ~a: ~a modules, ~a waves ~a, ~a cycles~n"
            title (length (compile-graph-nodes graph))
            (length waves) (map length waves) (length cycles))
    (for-each (lambda (cycle) (format #t "      cycle ~s~n" cycle)) cycles)))

(define (report-compile-graph)
  (let ((full *compile-graph*))
    (format #t "~%Dependency graph~%")
    (format #t "========================================================~%")
    (for-each (lambda (tier)
                (report-tier full
                             (inventory-tier-name tier)
                             (inventory-tier-files tier)))
              inventory-tiers)
    (let ((waves (compile-graph-waves full)))
      (format #t "--------------------------------------------------------~%")
      (format #t "  whole tree: ~a modules, ~a waves ~a~%"
              (length (compile-graph-nodes full))
              (length waves) (map length waves))
      (format #t "  macro-defining modules: ~a~%"
              (length (compile-graph-macro-nodes full)))
      (format #t "  cycles: ~a~%" (length (compile-graph-cycles full)))
      (format #t "========================================================~%"))))

(when (env-flag? "TEXMACS_COMPILE_GRAPH")
  (report-compile-graph)
  (exit 0))

;; ---- Exports --------------------------------------------------------------

;; Seed the export table from the database. Every module the build can ask
;; about already has its public names recorded, so no transpiled file is ever
;; opened again to find out what a module exports.
(for-each
  (lambda (info)
    (hashtable-set! module-exports-table
                    (mod-name->symbol (module-info-name info))
                    (module-info-exports info)))
  (compiler-db-modules *compiler-db*))

;; A module transpiled in this run overwrites its own entry with the exact
;; export list it emitted; everything else is answered from the database.
(define (get-module-exports mod-sym)
  (or (hashtable-ref module-exports-table mod-sym #f)
      (begin
        (log-note (format "exports unknown for ~a" mod-sym))
        (hashtable-set! module-exports-table mod-sym '())
        '())))

(define (publish-global! name value)
  (define-top-level-value name value)
  (define-top-level-value name value (interaction-environment)))

(publish-global! 'module-exports-table module-exports-table)
(publish-global! 'get-module-exports get-module-exports)

;; ---- Form preparation -----------------------------------------------------

(define (tm-internal-name? name)
  (string-prefix? "_tm_def_" (symbol->string name)))

(define (add-name name names)
  (if (and (symbol? name)
           (not (keyword? name))
           (not (tm-internal-name? name))
           (not (memq name names)))
      (cons name names)
      names))

(define (cpp-alias? form)
  (and (eq? (car form) 'define)
       (symbol? (cadr form))
       (pair? (cddr form))
       (symbol? (caddr form))
       (string=? (symbol->string (cadr form))
                 (string-append "cpp-" (symbol->string (caddr form))))))

(define (branch-forms branch)
  (cond ((not branch) '())
        ((and (pair? branch) (eq? (car branch) 'begin)) (cdr branch))
        (else (list branch))))

(define (transpile-conditional form)
  (let* ((kind (car form))
         (test (if (eq? kind 'unless) `(not ,(cadr form)) (cadr form)))
         (then (if (memq kind '(when unless))
                   (cons 'begin (cddr form))
                   (caddr form)))
         (else-branch (and (eq? kind 'if)
                           (pair? (cdddr form))
                           (cadddr form)))
         (value (guard-eval #f '() 'eval-error
                  (lambda () (eval test (compilation-environment))))))
    (if (eq? value 'eval-error)
        (values #f '())
        (values #t (branch-forms (if value then else-branch))))))

;; Return the prepared forms, the public names, and every local definition,
;; all in source order. The old version kept the last two in globals and
;; re-read them between modules.
(define (prepare-forms forms)
  (let loop ((rest forms) (out '()) (public '()) (local '()))
    (cond
      ((null? rest)
       (values (reverse out) (reverse public) (reverse local)))
      ((not (car rest))
       (loop (cdr rest) out public local))
      ((not (pair? (car rest)))
       (loop (cdr rest) (cons (car rest) out) public local))
      (else
       (let ((form (car rest)))
         (case (car form)
           ((begin)
            (loop (append (cdr form) (cdr rest)) out public local))

           ((tm-define tm-define-macro lazy-define define-group define-table
             define-collection extend-table extend-collection tm-property
             define-preferences define-top-level-syntax)
            (loop (cdr rest) (cons form out) public local))

           ((define)
            (if (cpp-alias? form)
                (loop (cdr rest)
                      (cons `(define ,(cadr form)
                               (top-level-value ',(caddr form)))
                            out)
                      public local)
                (let ((normalized (normalize-definition form)))
                  (loop (cdr rest) (cons normalized out)
                        public
                        (add-name (defined-name normalized) local)))))

           ((define-public provide-public define-menu define-widget)
            (let* ((normalized (normalize-definition form))
                   (name (defined-name normalized)))
              (loop (cdr rest) (cons normalized out)
                    (if (memq (car form) '(define-public provide-public))
                        (add-name name public)
                        public)
                    (add-name name local))))

           ((define-macro define-public-macro define-module-macro)
            (let ((name (defined-name form)))
              (loop (cdr rest) (cons form out)
                    (if (memq (car form) '(define-public-macro define-module-macro))
                        (add-name name public)
                        public)
                    (add-name name local))))

           ((export)
            (loop (cdr rest) out
                  (fold-left (lambda (acc name) (add-name name acc))
                             public (cdr form))
                  local))

           ((define-syntax)
            (loop (cdr rest) (cons form out)
                  public
                  (add-name (cadr form) local)))

           ((bib-define-style)
            (let* ((style (cadr form))
                   (predicate (string->symbol (string-append "bib-" style "?"))))
              (loop (cdr rest) (cons form out)
                    (add-name predicate public)
                    (add-name predicate local))))

           ((cas-define-test)
            (let* ((name (cadr form))
                   (table (string->symbol
                            (string-append (symbol->string name) "-table")))
                   (predicate (string->symbol
                                (string-append (symbol->string name) "?"))))
              (loop (cdr rest) (cons form out)
                    public
                    (add-name predicate (add-name table local)))))

           ((define-export-macro)
            (loop (cons (cons 'define-public-macro (cdr form)) (cdr rest))
                  out public local))

           ;; R6RS forbids `import' inside expression contexts, so in-body
           ;; module declarations become runtime `module-provide' calls.
           ((use-modules inherit-modules)
            (loop (append (map (lambda (module) `(module-provide ',module))
                               (cdr form))
                          (cdr rest))
                  out public local))

           (else
            (if (cond-def? form)
                (let-values (((handled? forms) (transpile-conditional form)))
                  (if handled?
                      (loop (append forms (cdr rest)) out public local)
                      (loop (cdr rest) (cons form out) public local)))
                (loop (cdr rest) (cons form out) public local)))))))))

;; Linter: detect potential mid-load mutation hazards where a top-level
;; non-procedure definition follows an expression that mutates state, risking
;; R6RS hoisting anomalies.
(define (check-mid-load-hazards forms mod-name)
  (define (warn-hazard! value name mutated)
    (cond ((symbol? value)
           (when (memq value mutated)
             (log-note (format "warning: ~a defines ~a before ~a is set"
                               mod-name name value))))
          ((pair? value)
           (warn-hazard! (car value) name mutated)
           (warn-hazard! (cdr value) name mutated))))

  (let loop ((rest forms) (mutated '()))
    (unless (null? rest)
      (let ((form (car rest)))
        (cond
          ((and (pair? form)
                (memq (car form) '(set! ahash-set!))
                (pair? (cdr form))
                (symbol? (cadr form)))
           (loop (cdr rest) (cons (cadr form) mutated)))
          ((def-form? form)
           (when (and (pair? (cdr form))
                      (symbol? (cadr form))
                      (pair? (cddr form)))
             (let ((value (caddr form)))
               (unless (and (pair? value) (eq? (car value) 'lambda))
                 (warn-hazard! value (cadr form) mutated))))
           (loop (cdr rest) mutated))
          (else
           (loop (cdr rest) mutated)))))))

;; ---- Pass 1: syntactic transpilation -------------------------------------

;; Pass 1: modules emitted (or already up to date) so far, in tier order. This
;; is what `module-link-forms' consults to decide import vs runtime
;; module-provide, so the decision no longer depends on what has been loaded.
(define *transpiled-modules* '())

;; rel-path -> mod-sym for every module Pass 1 rewrote.
(define *pending-build* (make-hashtable equal-hash equal?))

(define compile-errors '())

(define (module-header forms)
  (and (pair? forms)
       (pair? (car forms))
       (eq? (caar forms) 'texmacs-module)
       (car forms)))

(define *r6rs-library-modules*
  '(kernel:boot:chez-compat
    kernel:boot:abbrevs))

(define (module-link-forms mod-name deps options public local)
  (define imports '())
  (define exports (if (null? public) '() (list `(export ,@public))))
  (define imported '())

  ;; Import one dependency's exports, minus any name that would collide with a
  ;; local definition or with an earlier import.
  (define (use-module! module)
    (let* ((names (get-module-exports module))
           (clash (filter (lambda (name)
                            (or (memq name local) (memq name imported)))
                          names))
           (kept (filter (lambda (name) (not (memq name clash))) names)))
      (set! imports
            (cons (if (null? clash)
                      `(import (for ,module run expand))
                      `(import (for (except ,module ,@clash) run expand)))
                  imports))
      (set! imported (append kept imported))))

  (define (inherit-module! module)
    (set! imports (cons `(import (for ,module run expand)) imports))
    (set! exports (cons `(export (import ,module)) exports)))

  ;; A dependency that Pass 1 has not reached yet falls back to a runtime
  ;; module-provide lookup. Say which kind it is: a real cycle, or merely a
  ;; later position in the tier order.
  (define (note-deferred! dependency)
    (let ((module (mod-name->symbol dependency)))
      (log-note (format (if (cycle-mate? mod-name dependency)
                            "cycle back-edge: ~a -> ~a (runtime module-provide)"
                            "deferred forward import: ~a -> ~a (acyclic)")
                        (mod-name->symbol mod-name)
                        module))))

  (for-each
    (lambda (auto-mod)
      (when (and (not (eq? (mod-name->symbol mod-name) auto-mod))
                 (memq auto-mod *transpiled-modules*))
        (use-module! auto-mod)))
    *r6rs-library-modules*)

  (for-each
    (lambda (option)
      (when (and (pair? option) (memq (car option) '(:use :inherit)))
        (for-each (lambda (dependency)
                    (let ((module (mod-name->symbol dependency)))
                      (cond ((not (memq module *transpiled-modules*))
                             (note-deferred! dependency))
                            ((eq? (car option) ':use)
                             (use-module! module))
                            (else
                             (inherit-module! module)))))
                  (cdr option))))
    options)

  (values (reverse imports) (reverse exports)))

(define (keyword-definitions forms)
  (map (lambda (keyword) `(define ,keyword ',keyword))
       (tm-collect-keywords forms '())))

;; R6RS libraries have no fallback to `(interaction-environment)', so bridge free
;; references to C++ glue and later modules via `top-level-value' shims.
(define (library-top-level-shims forms local imported)
  (let ((seen '())
        (shims '()))
    (let walk ((x forms))
      (cond
        ((pair? x)
         (if (eq? (car x) 'quote)
             (void)
             (begin (walk (car x)) (walk (cdr x)))))
        ((and (symbol? x)
              (not (keyword? x))
              (not (memq x seen))
              (not (memq x local))
              (not (memq x imported))
              (not (memq x '(define-public provide-public define-public-macro
                             _ unquote unquote-splicing quasiquote)))
              (not (top-level-bound? x (scheme-environment)))
              (not (top-level-syntax? x (scheme-environment)))
              (or (top-level-bound? x (interaction-environment))
                  (hashtable-ref *oracle-defs* x #f)))
         (set! seen (cons x seen))
         (set! shims
               (cons `(define-syntax ,x
                        (identifier-syntax (top-level-value ',x)))
                     shims)))))
    (reverse shims)))

;; Each R6RS library's `(import ...)' header depends on the `(chezscheme)'
;; builtins overridden/redefined in that .scm file and earlier libraries used.
(define (library-import-form mod-sym)
  (cond
    ((eq? mod-sym 'kernel:boot:chez-compat)
     '(import
        (except (chezscheme)
                sort current-time random record-accessor)
        (rename (only (chezscheme)
                      sort current-time random record-accessor
                      display write)
                (sort chez:sort)
                (current-time chez:current-time)
                (random chez-sys-random)
                (record-accessor old-record-accessor)
                (display chez:display)
                (write chez:write))))
    ((eq? mod-sym 'kernel:boot:abbrevs)
     '(import
        (except (chezscheme)
                sort current-time random record-accessor ...)
        (kernel boot chez-compat)))
    (else
     (error 'library-import-form "unknown library module" mod-sym))))

(define (write-module-file out-path rel-path mod-name mod-sym imports exports
                           keywords local definitions dep-provides expressions)
  (call-with-output-file out-path
    (lambda (port)
      (format port ";;; Auto-generated native Chez Scheme module from ~a~%"
              rel-path)
      (if (memq mod-sym *r6rs-library-modules*)
          (begin
            (write `(library ,mod-name
                      ,@exports
                      ,(library-import-form mod-sym)
                      (define-syntax define-public
                        (lambda (x)
                          (syntax-case x ()
                            ((_ (name . args) . body) #'(define (name . args) . body))
                            ((_ name val) #'(define name val)))))
                      (define-syntax provide-public
                        (lambda (x)
                          (syntax-case x ()
                            ((_ (name . args) . body) #'(define (name . args) . body))
                            ((_ name val) #'(define name val)))))
                      (define-syntax define-public-macro
                        (lambda (x)
                          (syntax-case x ()
                            ((_ (nm . formals) . body)
                             (with-syntax ((lib-== (datum->syntax #'nm '==)))
                               #'(define-syntax nm
                                   (lambda (stx)
                                     (let ((lib-== equal?))
                                       (datum->syntax (car (syntax->list stx))
                                         (apply (lambda formals . body)
                                                (cdr (syntax->datum stx))))))))))))
                      ,@(if (eq? mod-sym 'kernel:boot:chez-compat)
                            '()
                            (library-top-level-shims
                              definitions
                              local
                              (get-module-exports 'kernel:boot:chez-compat)))
                      ,@definitions
                      ,@expressions)
                   port)
            (newline port)
            (write `(module ,mod-sym ()
                      (import ,mod-name)
                      (export (import ,mod-name)))
                   port))
          (write `(module ,mod-sym ()
                    ,@imports
                    ,@exports
                    ,@keywords
                    ,@definitions
                    ,@dep-provides
                    ,@expressions)
                 port))
      (newline port))
    'replace))

(define (transpile-module rel-path out-path)
  (guard (e (else
              (log-progress "FAILED" rel-path)
              (log-note (condition-text e))
              (set! compile-errors (cons (cons rel-path e) compile-errors))
              #f))
    (let* ((forms (read-all-forms (progs-path rel-path)))
           (header (module-header forms)))
      (tm-bind-keywords forms)
      (ensure-dir out-path)
      (and header
           (let* ((mod-name (cadr header))
                  (mod-sym (mod-name->symbol mod-name))
                  (options (cddr header))
                  ;; Every whitelisted module was scanned into the database, so
                  ;; a miss means the scan and this loop disagree about the file.
                  (info (or (compiler-db-ref *compiler-db* mod-name)
                            (error 'transpile-module
                                   "no compiler-db record for module" mod-name)))
                  (deps (module-info-deps info)))

             (let-values (((prepared public local)
                           (prepare-forms (cdr forms))))
               (check-mid-load-hazards prepared rel-path)
               ;; Pass 2 needs these forms to build the module's expand-time
               ;; environment; keeping the subset means it never re-reads source.
               ;; A worker has no Pass 1, so the same list also goes on disk.
               (let ((eager (filter needs-eager-eval? prepared)))
                 (eager-forms-set! rel-path eager)
                 (write-eager-forms! out-path eager))
               (update-compiler-db-exports! mod-name public)
               (let-values (((imports exports)
                             (module-link-forms mod-name deps options public local)))
                 (hashtable-set! module-exports-table mod-sym public)
                 (let-values (((definitions expressions)
                               (partition def-form? prepared)))
                   (write-module-file
                     out-path rel-path mod-name mod-sym
                     imports exports
                     (keyword-definitions forms)
                     local
                     definitions
                     (map (lambda (dep) `(module-provide ',dep)) deps)
                     expressions))
                 mod-sym)))))))

;; Pass 1: rewrite each module that needs a rebuild into a Chez Scheme module
;; file. Nothing here loads a module, evaluates a definition or writes a .so.
(define (transpile-module-list tier-title file-list)
  (emit-progress-line (string-append "-- Pass 1/2: " tier-title " --"))
  (for-each
    (lambda (rel-path)
      (progress-tick!)
      (let* ((trans-scm (boot-path rel-path))
             (out-so (scm->so trans-scm)))
        (if (module-needs-recompile? rel-path trans-scm out-so)
            (let ((mod-sym (transpile-module rel-path trans-scm)))
              (cond
                (mod-sym
                 (log-progress "Transpiling"
                               (string-append rel-path " -> "
                                              (path-basename trans-scm)))
                 (hashtable-set! *pending-build* rel-path mod-sym)
                 (set! *transpiled-modules*
                       (cons mod-sym *transpiled-modules*)))
                ;; No `texmacs-module' header: reported Skipped, never compiled.
                ((not (assoc rel-path compile-errors))
                 (log-progress
                   "Skipped"
                   (string-append rel-path " (no texmacs-module header)")))))
            ;; Up to date from an earlier run: still a valid import target.
            (set! *transpiled-modules*
                  (cons (mod-name->symbol (rel-path->mod-name rel-path))
                        *transpiled-modules*)))))
    file-list))
;; ---- Kernel snapshot -----------------------------------------------------

(define (write-line port object)
  (write object port)
  (newline port))

(define (write-kernel-snapshot! user-scm)
  (call-with-output-file user-scm
    (lambda (port)
      (display ";;; Aggregate texmacs-user and texmacs-user-kernel\n" port)
      (for-each (lambda (rel)
                  (write-line port `(primitive-load ,(scm->so (boot-path rel)))))
                *kernel-files*)
      (let* ((modules (map (lambda (rel)
                             (mod-name->symbol (rel-path->mod-name rel)))
                           *kernel-files*))
             (imports (map (lambda (module)
                             `(import (for ,module run expand)))
                           modules))
             (exports (map (lambda (module)
                             `(export (import ,module)))
                           modules)))
        (write-line port `(module texmacs-user ()
                            ,@imports
                            ,@exports))
        (write-line port `(module texmacs-user-kernel ()
                            (export (import texmacs-user))))
        (write-line port `(import texmacs-user-kernel))
        (for-each (lambda (module) (write-line port `(import ,module)))
                  modules))
      ;; The module forms above bind these names to modules, and a module
      ;; binding is not a value. Rebind them to the single environment that
      ;; user space means everywhere else.
      (write-line port `(define-top-level-value 'texmacs-user
                         (interaction-environment)))
      (write-line port `(define-top-level-value 'texmacs-user-kernel
                         (interaction-environment))))
    'replace))

(define (build-kernel-snapshot!)
  (let* ((user-scm (boot-path "texmacs-user.scm"))
         (user-so (boot-path "texmacs-user.so")))
    (if (and (file-exists? user-so)
             (not *recompile-all?*)
             (not *any-kernel-dirty?*))
        (begin
          (log-progress-pct "Up-to-date" "texmacs-user.so (kernel snapshot)")
          (load user-so))
        (begin
          (log-progress-pct "Building"
                            "aggregate kernel snapshot texmacs-user.so")
          (write-kernel-snapshot! user-scm)
          (parameterize ((compile-profile
                          (and (not *compile-profile-filter*)
                               *compile-profile-mode*)))
            (compile-file user-scm user-so))
          (load user-so)
          (log-progress-pct "Verified" "texmacs-user.so")))))
