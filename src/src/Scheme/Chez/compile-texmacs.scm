;;; TODO: UNDER REVIEWED AI GENERATED CODE.
;;;
;;; compile-texmacs.scm -- Transpiles and compiles all TeXmacs modules into native Chez Scheme modules
;;;
;;; Usage:
;;;  ENV=??? TeXmacs --compile-modules


;; ---- Bootstrap -----------------------------------------------------------
;;
;; Only what is needed to find and load the compiler files themselves: the
;; directory they live in and the libraries they depend on.  Everything else --
;; the progs tree, the boot directory, the module-name vocabulary, the compile
;; environment -- comes from compiler-helper.scm, which a worker loads the same
;; way.

(define *compiler-dir*
  (or (and (file-exists? "compiler-db.scm") ".")
      (and (file-exists? "src/Scheme/Chez/compiler-db.scm") "src/Scheme/Chez")
      (and (file-exists? "texmacs/src/Scheme/Chez/compiler-db.scm")
           "texmacs/src/Scheme/Chez")
      (error 'compile-texmacs "cannot find compiler-db.scm")))

;; These libraries are loaded with `primitive-load', not `import': a TeXmacs
;; process has no library search path into src/Scheme/Chez.
(primitive-load (string-append *compiler-dir* "/tarjan.scm"))
(import (tarjan))
(primitive-load (string-append *compiler-dir* "/compiler-db.scm"))
(import (compiler-db))
(primitive-load (string-append *compiler-dir* "/compiler-scan.scm"))
(import (compiler-scan))
(primitive-load (string-append *compiler-dir* "/compile-graph.scm"))
(import (compile-graph))

;; The module whitelist lives in its own library: the linters and the tests
;; need the same seven lists, and re-reading this file to eval its `define`
;; forms was the only way to get them.
(primitive-load (string-append *compiler-dir* "/compile-inventory.scm"))
(import (compile-inventory))

;; ---- Compile-time environment -------------------------------------------

;; compiler-oracle.scm has the closure walkers and the index/install half;
;; compiler-helper.scm has the path/name vocabulary, the per-module
;; environment lifecycle, the loaded-module registry and the compile-file
;; call.  Both are plain definition files, loaded into this top-level
;; environment.
(primitive-load (string-append *compiler-dir* "/compiler-oracle.scm"))
(primitive-load (string-append *compiler-dir* "/compiler-helper.scm"))

;; compiler-helper.scm resolves the progs tree, so boot-chez is handled right
;; after it: check if it needs compiling; if yes, compile then load; otherwise
;; just load it. Safeguard: do not load twice if already loaded into this process.
(define (compile-init-file! rel-path . extra-dirty?)
  (let* ((src (progs-path rel-path))
         (out (boot-path (scm->so rel-path)))
         (base (path-basename out))
         (dirty? (or (and (pair? extra-dirty?) (car extra-dirty?))
                     (not (file-exists? out))
                     (env-flag? "TEXMACS_RECOMPILE_ALL")
                     (and (top-level-bound? '*recompile-all?*) *recompile-all?*)
                     (time>? (file-modification-time src)
                             (file-modification-time out)))))
    (ensure-dir out)
    (when dirty?
      (log-progress-pct "Compiling" (string-append rel-path " -> " base))
      (compile-file src out)
      (log-progress-pct "Verified" base))
    out))

(let ((out (compile-init-file! "kernel/boot/boot-chez.scm")))
  (unless (top-level-bound? 'boot-chez-loaded?)
    (load out)))

;; ---- Pass 1 --------------------------------------------------------------
;;
;; Transpilation, the dirty set and the kernel snapshot live in
;; convert-to-r6rs.scm, so that the pool coordinator can run exactly this
;; stage before it starts any worker.
(primitive-load (string-append *compiler-dir* "/convert-to-r6rs.scm"))

;; ---- Compilation ----------------------------------------------------------

;; The expansion environment of one module: the eager forms come from Pass 1,
;; the prerequisites from the scan database.  `setup-module-environment!' lives
;; in compiler-helper.scm because a worker needs the same step.
(define (install-eager-forms! rel-path)
  (let ((info (compiler-db-ref *compiler-db* (rel-path->mod-name rel-path))))
    (setup-module-environment! rel-path
                               (if info (module-info-deps info) '()))))

(define (compile-transpiled-module! rel-path trans-scm out-so mod-sym)
  (install-eager-forms! rel-path)
  (compile-module-file! rel-path trans-scm out-so)
  (let ((hash (compute-file-hash out-so)))
    (when hash
      (update-compiler-db-artifact! (rel-path->mod-name rel-path)
                                    hash
                                    (time-second (current-time)))))
  mod-sym)

(define (report-compile-failure! rel-path condition)
  (log-progress "FAILED" rel-path)
  (emit-progress-line (condition-text condition))
  (when (who-condition? condition)
    (log-note (format "who: ~a" (condition-who condition))))
  (when (irritants-condition? condition)
    (log-note (format "irritants: ~s" (condition-irritants condition))))
  (when (syntax-violation? condition)
    (log-note (format "form: ~s" (syntax-violation-form condition)))
    (log-note (format "subform: ~s" (syntax-violation-subform condition))))
  (when (top-level-bound? 'tm-backtrace)
    (tm-backtrace condition))
  (set! compile-errors (cons (cons rel-path condition) compile-errors)))

(define (compile-module! rel-path trans-scm out-so mod-sym)
  ;; One line per module: `Compiling' here, or `FAILED' from the handler below.
  (log-progress "Compiling"
                (string-append rel-path " -> " (path-basename out-so)))
  (call/cc
    (lambda (escape)
      (with-exception-handler
        (lambda (condition)
          (when (serious-condition? condition)
            (report-compile-failure! rel-path condition)
            (escape #f)))
        (lambda ()
          (compile-transpiled-module! rel-path trans-scm out-so mod-sym))))))

(define compiled-count 0)
(define cached-count 0)

(define (save-module-deps-manifest!)
  (let ((manifest-file (string-append *boot-dir* "/module-deps.scm")))
    (log-progress-pct "Writing"
                      (string-append "module dependency manifest " manifest-file))
    (call-with-output-file manifest-file
      (lambda (port)
        (write (map (lambda (info)
                      (list (module-info-name info)
                            (module-info-deps info)))
                    (compiler-db-modules *compiler-db*))
               port)
        (newline port))
      'replace)))

(define (load-compiled-module! rel-path out-so mod-sym mod-name)
  (hashtable-set! *texmacs-modules-loaded* mod-name #t)
  (guard (e (else
              (hashtable-delete! *texmacs-modules-loaded* mod-name)
              (log-progress "FAILED" rel-path)
              (log-note (condition-text e))
              (when (file-exists? out-so) (delete-file out-so))
              (set! compile-errors (cons (cons rel-path e) compile-errors))
              #f))
    (load out-so)
    (when (member rel-path *kernel-files*)
      (guard-eval 'IMPORT-FAIL (list mod-sym) #f
        (lambda ()
          (eval `(import ,mod-sym) (interaction-environment)))))
    #t))

(define (register-module-loaded! rel-path out-so mod-sym is-cached?)
  (let ((mod-name (rel-path->mod-name rel-path)))
    (unless (compiler-db-ref *compiler-db* mod-name)
      (error 'register-module-loaded!
             "no compiler-db record for module" rel-path mod-name))
    (when (load-compiled-module! rel-path out-so mod-sym mod-name)
      (note-module-loaded! mod-sym)
      (if is-cached?
          (begin
            (set! cached-count (+ cached-count 1))
            (log-progress "Up-to-date" rel-path))
          ;; A recompiled module already reported itself as `Compiling`; the
          ;; successful load adds no second line for the same index.
          (set! compiled-count (+ compiled-count 1))))))

;; ---- Pass 2: native code generation --------------------------------------

;; Pass 2 walks the same tiers in the same order as Pass 1, so a module's
;; prerequisites are loaded before `compile-file' expands it. Whether a module
;; needs compiling was decided in Pass 1: `*pending-build*' holds exactly the
;; modules it rewrote.
(define (compile-module-list tier-title file-list)
  (emit-progress-line (string-append "-- Pass 2/2: " tier-title " --"))
  (for-each
    (lambda (rel-path)
      (progress-tick!)
      (let* ((trans-scm (boot-path rel-path))
             (out-so (scm->so trans-scm))
             (mod-sym (hashtable-ref *pending-build* rel-path #f)))
        (if mod-sym
            (when (compile-module! rel-path trans-scm out-so mod-sym)
              (parameterize ((compile-export-mode? #t))
                (register-module-loaded! rel-path out-so mod-sym #f)))
            (parameterize ((compile-export-mode? #t))
              (register-module-loaded!
                rel-path out-so
                (mod-name->symbol (rel-path->mod-name rel-path))
                #t)))))
    file-list))

;; One tier is one list call: the banner and the file list both come from the
;; inventory row, so the driver never names a tier twice.
(define (transpile-tier! tier)
  (transpile-module-list (inventory-tier-title tier) (inventory-tier-files tier)))

(define (compile-tier! tier)
  (compile-module-list (inventory-tier-title tier) (inventory-tier-files tier)))

;; ---- Final report --------------------------------------------------------

(define (plural-suffix n)
  (if (= n 1) "" "s"))

(define (report-build-summary!)
  (newline)
  (display "========================================================\n")
  (if (null? compile-errors)
      (begin
        (display "[100%] Built target texmacs-chez-modules:\n")
        (format #t "  - Recompiled: ~a module~a\n"
                compiled-count (plural-suffix compiled-count))
        (format #t "  - Up-to-date (cached): ~a module~a\n"
                cached-count (plural-suffix cached-count))
        (format #t "  - Failures: ~a\n" (length compile-errors))
        (for-each (lambda (error)
                    (format #t "    * FAILED: ~a\n" (car error)))
                  (reverse compile-errors)))
      (begin
        (format #t "[FAILED] Built target texmacs-chez-modules: ~a failure(s)\n"
                (length compile-errors))
        (for-each (lambda (error)
                    (format #t "  - ~a\n" (car error)))
                  compile-errors)))
  (display "========================================================\n"))

;; ---- Main -----------------------------------------------------------------

;; Pass 1: transpile every module that needs a rebuild. This reads the pristine
;; sources and writes under the boot directory; it loads nothing and generates no
;; code, so it cannot fail for a reason that depends on build order.
(progress-start! (length *source-files*))
(for-each transpile-tier! inventory-tiers)

;; Pass 2: compile those files, in tier order, loading each module so the next
;; tier's imports resolve. Tier 1 runs unconditionally: the kernel snapshot and
;; every later tier depend on it, and its failures abort the rest of the build.
(unless (pair? compile-errors)
  (progress-start! (length *source-files*))
  (compile-tier! (car inventory-tiers))
  (unless (pair? compile-errors)
    (build-kernel-snapshot!)
    (for-each compile-tier! (cdr inventory-tiers))
    (save-module-deps-manifest!)
    (write-compiler-db *compiler-db* *compiler-db-file*)
    (compile-init-file! "init-texmacs-boot-file.scm" (> compiled-count 0))))

(report-build-summary!)

(when (and (top-level-bound? 'quit-TeXmacs)
           (not (top-level-bound? '*dont-quit*)))
  (quit-TeXmacs))
