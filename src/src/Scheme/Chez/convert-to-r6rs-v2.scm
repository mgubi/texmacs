;;; convert-to-r6rs-v2.scm -- Pass 1, second attempt: phased helper modules.
;;;
;;; This is the pilot implementation of docs/chez/PHASED_HELPERS_PLAN.md.
;;; It transpiles one tier of TeXmacs modules into Chez Scheme modules and
;;; compiles them, without the compiler oracle and without eager evaluation.
;;;
;;; The idea in one paragraph: a TeXmacs macro transformer is an ordinary
;;; procedure that runs while a *consumer* of the macro is being expanded, so
;;; the procedures it calls have to exist at that moment.  Today they are
;;; pushed into the interaction environment on demand by compiler-oracle.scm
;;; and taken back out afterwards.  Here, the definitions a file's macros call
;;; are compiled into a sibling helper module, and the module that owns them
;;; imports that helper module at run *and* expand time.
;;;
;;; What is deliberately *not* here, because the pilot is meant to stay small:
;;;
;;;   - the compiler database, the dependency graph and incremental rebuilds.
;;;     Every selected module is transpiled and compiled, every time.
;;;   - the oracle (compiler-oracle.scm's install half) and the eager pass.
;;;   - tiers 2-7.  Only tier 1 (the kernel) is built by default; see
;;;     TEXMACS_V2_TIER below.
;;;
;;; Reused as-is from the existing pipeline:
;;;
;;;   compiler-scan.scm      read-all-forms (the Guile-source reader)
;;;   compiler-oracle.scm    *macro-definers*, mc-leftmost-symbol
;;;   compiler-helper.scm    paths, module names, normalize-definition,
;;;                          keyword collection, progress reporting
;;;   compile-inventory.scm  the tier lists
;;;
;;; Usage
;;;
;;;   TEXMACS_PATH=$PWD/TeXmacs TEXMACS_BOOT_PATH=$PWD/build-v2 \
;;;     build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs \
;;;     -compile-modules src/Scheme/Chez/convert-to-r6rs-v2.scm
;;;
;;; The driver has to run inside the TeXmacs process: it compiles and then
;;; loads each module so the next one's imports resolve, and the kernel's
;;; foreign procedures only exist once the engine has registered them.  The
;;; `-compile-modules <script>' form (texmacs.cpp:804) runs an arbitrary
;;; driver instead of compile-texmacs.scm.
;;;
;;; TEXMACS_BOOT_PATH is where the transpiled .scm and the compiled .so files
;;; are written; point it somewhere other than build-texmacos/boot so the
;;; working build stays intact.  TEXMACS_V2_TIER=1,2 widens the run.
;;;
;;; State: no pre-eval, no oracle.  What the build does is translate, compile,
;;; load -- and every cross-module reference a transformer makes is a real
;;; import, at the phase the import declares.  This file itself never publishes
;;; anything; the *load* step does, because boot-chez's `load-module-file'
;;; imports each freshly loaded module into the interaction environment.  That
;;; publication is load-bearing here -- see `v2-compile-and-load' -- and is the
;;; reason the build needs two passes to settle.
;;;
;;;   whole tree, 453 modules, into a boot directory that already holds a
;;;   complete build            453/453, 0 failures
;;;   the same, into an empty boot directory
;;;     pass 1                   421/453, 32 failures
;;;     pass 2                   453/453, 0 failures  (the fixed point)
;;;
;;; The 32 first-pass failures are forward `:use' edges in the tier order, not
;;; translation bugs: a declared dependency that has not been compiled yet gets
;;; a runtime `(module-provide '<dep>)' instead of an import, and on a cold
;;; directory that call falls back to loading the *original Guile source* of
;;; the dependency.  It is harmless once the dependency has been built once,
;;; which is why pass 2 is clean.  Closing it properly means compiling in
;;; dependency order -- the compiler database and the dependency graph, which
;;; this pilot deliberately leaves out.  See docs/chez/PHASED_HELPERS_PLAN.md
;;; §7.6.

;; ---- Bootstrap -----------------------------------------------------------
;;
;; Same probing as compile-texmacs.scm: a TeXmacs process has no library
;; search path into src/Scheme/Chez, so the build libraries are primitive-
;; loaded and then imported by name.

(define *compiler-dir*
  (or (and (file-exists? "compiler-db.scm") ".")
      (and (file-exists? "src/Scheme/Chez/compiler-db.scm") "src/Scheme/Chez")
      (and (file-exists? "texmacs/src/Scheme/Chez/compiler-db.scm")
           "texmacs/src/Scheme/Chez")
      (error 'convert-to-r6rs-v2 "cannot find compiler-db.scm")))

(define (v2-load-library name)
  (primitive-load (string-append *compiler-dir* "/" name ".scm"))
  (eval `(import (,(string->symbol name))) (interaction-environment)))

;; tarjan is only needed because compiler-oracle.scm references it at load
;; time; nothing in this file computes strongly connected components.
(v2-load-library "tarjan")
(v2-load-library "compiler-db")
(v2-load-library "compiler-scan")
(v2-load-library "compile-inventory")

;; compiler-oracle.scm and compiler-helper.scm are plain definition files, not
;; libraries.  Order matters: compiler-helper.scm's `defined-name' calls
;; mc-leftmost-symbol.
(primitive-load (string-append *compiler-dir* "/compiler-oracle.scm"))
(primitive-load (string-append *compiler-dir* "/compiler-helper.scm"))

;; ---- boot-chez.scm ------------------------------------------------------
;;
;; The kernel sources are written against the syntax boot-chez.scm installs:
;; `define-public', `define-public-macro', `define-macro', `provide-public',
;; `compile-export-mode?'.  A module body is expanded in the enclosing
;; top-level environment, so those bindings have to be live before the first
;; `compile-file' call.  Same compile/load dance as compile-texmacs.scm.

(let* ((src (progs-path "kernel/boot/boot-chez.scm"))
       (out (boot-path "kernel/boot/boot-chez.so")))
  (ensure-dir out)
  (when (or (not (file-exists? out))
            (env-flag? "TEXMACS_RECOMPILE_ALL")
            (and (file-exists? src)
                 (time>? (file-modification-time src)
                         (file-modification-time out))))
    (emit-progress-line "Compiling kernel/boot/boot-chez.scm -> boot-chez.so")
    (compile-file src out))
  (unless (top-level-bound? 'boot-chez-loaded?)
    (load out)))

;; ---- Which tiers this pilot builds --------------------------------------

(define (string-split str sep)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((= i (string-length str))
           (reverse (cons (substring str start i) acc)))
          ((char=? (string-ref str i) sep)
           (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
          (else (loop (+ i 1) start acc)))))

(define (v2-tier-selection)
  (let ((spec (getenv "TEXMACS_V2_TIER")))
    (if (and spec (not (string=? spec "")))
        (map string->number
             (filter (lambda (s) (not (string=? s "")))
                     (string-split spec #\,)))
        '(1))))

(define *v2-tiers*
  (let ((wanted (v2-tier-selection)))
    (filter (lambda (tier) (memv (inventory-tier-number tier) wanted))
            inventory-tiers)))

;; ---- Definition vocabulary ----------------------------------------------

;; Definitions this pass may move into the helper module: pure definitions
;; whose body is a value or a lambda, so moving one changes only *where* the
;; name is bound, not *when* anything runs.
;;
;; Everything else -- `tm-define', `define-table', `define-state',
;; `define-group', `smart-table', ... -- is a stateful TeXmacs form whose
;; expansion registers into a table that lives in the module running it.
;; Those stay in the main module; when a macro needs one at expand time this
;; file reports V2-NONMOVABLE rather than silently dropping the edge.
;; Definitions this pass may move into the helper module: *value* definitions
;; only.  A macro definition stays in the main module, because a macro that
;; moves and then references a helper of the same module is an out-of-phase
;; reference -- the helper module would be using its own phase-0 binding from
;; phase 1.  Macros in the main module reach the helpers through the
;; `(for <mod>$h run expand)' import, and reach each other by sequential
;; expansion.
(define *movable-heads*
  ;; `define-state' expands to `(begin (define-public slot ...) ...)' -- real
  ;; module bindings -- so it moves like any value definition and the helper can
  ;; export it.  That is what lets one value serve both the expand-time
  ;; `(:state-slots graphics-state)' option and the run-time readers in the same
  ;; file.
  '(define define-public provide-public define-state))

;; A definition whose *name* is a syntax-rules auxiliary keyword must stay in
;; the module that defines it, and must never be imported into another one.
;; abbrevs.scm defines a procedure called `...'; once that name holds a
;; variable binding in a module body, `syntax-rules' stops reading `...' as the
;; ellipsis and every macro in the file fails with "invalid syntax" --
;; `with-global' is the first victim.  Moving `...' into the helper module (and
;; importing it from there) is exactly how it leaks, so it is excluded here.
(define *aux-syntax-names* '(... _ else => unquote unquote-splicing))

;; Every head that introduces a binding, movable or not.  Used only to split a
;; module body into "definitions" and "expressions", which Chez's module
;; grammar requires to be in that order.
;;
;; This is deliberately the *same* list as convert-to-r6rs.scm's
;; `*definition-heads*'.  The stateful forms it leaves out -- `tm-define',
;; `tm-define-macro', `define-table', ... -- are macros that expand into a
;; `begin' which is only legal in a body-final position, so they have to be
;; emitted after every plain definition.  Treating them as definitions (which
;; is what a naive reading suggests) puts them in the middle of the body and
;; the next `define' then fails with "invalid context for definition".
(define *definition-heads*
  '(define define-syntax define-public define-public-macro define-module-macro
    define-state provide-public define-macro define-menu define-widget
    define-record-type define-values bib-define-style texmacs-modes
    cas-define-test import export indirect-export))

(define (v2-definition? form)
  (and (pair? form) (memq (car form) *definition-heads*)))

;; Every head that introduces a name, wherever it is emitted.  The stateful
;; TeXmacs forms bind a name but are emitted with the expressions, so this set
;; is wider than `*definition-heads*'; it is what the helper analysis indexes.
(define *binding-heads*
  (append *definition-heads*
          '(tm-define tm-define-macro define-table define-collection
            extend-table extend-collection define-group define-preferences
            define-top-level-syntax lazy-define smart-table)))

(define (v2-binding? form)
  (and (pair? form) (memq (car form) *binding-heads*)))

;; ---- Preparing one module's forms ---------------------------------------

;; The name a definition form introduces:
;;   (define (f x) ...)           -> f
;;   (define f ...)               -> f
;;   (define ((f x) y) ...)       -> f
;;   (define-top-level-syntax 'f) -> f   (the argument is quoted)
(define (v2-form-name form)
  (let ((head (cadr form)))
    (cond ((and (pair? head) (eq? (car head) 'quote)) (cadr head))
          ((symbol? head) head)
          (else (mc-leftmost-symbol head)))))

;; A definition rewritten to plain `define' / `define-macro', with the export
;; decision taken out of the form and into the module header this file emits.
;; `normalize-definition' (compiler-helper.scm) uncurries
;;   (define ((f a) b) body) -> (define (f a) (lambda (b) body))
;; and drops a leading docstring, both of which raw `define' rejects.
(define (v2-rewrite-definition form)
  (case (car form)
    ((define) (normalize-definition form))
    ((define-public provide-public)
     (cons 'define (cdr (normalize-definition form))))
    ((define-public-macro define-export-macro)
     (cons 'define-macro (cdr form)))
    (else form)))

;; (define cpp-foo foo) is the C++ glue alias the old transpiler rewrites to a
;; top-level-value lookup: the glue installs `foo' as a top-level value, not as
;; a module binding.
(define (v2-cpp-alias? form)
  (and (eq? (car form) 'define)
       (symbol? (cadr form))
       (pair? (cddr form))
       (symbol? (caddr form))
       (string=? (symbol->string (cadr form))
                 (string-append "cpp-" (symbol->string (caddr form))))))

(define (v2-branch-forms branch)
  (cond ((not branch) '())
        ((and (pair? branch) (eq? (car branch) 'begin)) (cdr branch))
        (else (list branch))))

(define (v2-has-definition? form)
  (cond ((v2-definition? form) #t)
        ((and (pair? form) (eq? (car form) 'begin))
         (exists v2-has-definition? (cdr form)))
        (else #f)))

(define (v2-conditional? form)
  (and (pair? form)
       (case (car form)
         ((if)
          (or (and (pair? (cddr form)) (v2-has-definition? (caddr form)))
              (and (pair? (cdddr form)) (v2-has-definition? (cadddr form)))))
         ((when unless)
          (and (pair? (cddr form))
               (v2-has-definition? (cons 'begin (cddr form)))))
         (else #f))))

;; A top-level `if' / `when' / `unless' whose branches contain definitions has
;; to be resolved before emission: Chez's `if' is an expression, and a
;; definition inside one is an error.  The test is evaluated once, here, in the
;; interaction environment -- boot-chez.scm has already defined the dialect
;; predicates (`guile-c?' and friends) that these tests use.
(define (v2-resolve-conditional form)
  (let* ((kind (car form))
         (test (if (eq? kind 'unless) `(not ,(cadr form)) (cadr form)))
         (then (if (memq kind '(when unless))
                   (cons 'begin (cddr form))
                   (caddr form)))
         (else-branch (and (eq? kind 'if)
                           (pair? (cdddr form))
                           (cadddr form)))
         (value (guard (e (else 'v2-eval-error))
                  (eval test (interaction-environment)))))
    (if (eq? value 'v2-eval-error)
        (begin
          (log-note (format "V2-CONDITIONAL-UNRESOLVED ~s" test))
          '())
        (v2-branch-forms (if value then else-branch)))))

;; Returns (values PREPARED PUBLIC LOCAL):
;;   PREPARED  the module body as plain Scheme, top-level `begin's flattened
;;             and every definition rewritten to `define' / `define-macro'
;;   PUBLIC    the names this module exports, in source order
;;   LOCAL     the names it defines, public or not
(define (v2-prepare-forms forms)
  (define public '())
  (define local '())
  ;; `set!' on a procedure parameter would only rebind the parameter, so the
  ;; two accumulators are updated by their own procedures rather than through
  ;; a shared helper.
  (define (note-public! name)
    (unless (memq name public) (set! public (cons name public))))
  (define (note-local! name)
    (unless (memq name local) (set! local (cons name local))))
  (define (walk rest out)
    (cond
      ((null? rest) (values (reverse out) (reverse public) (reverse local)))
      ((not (car rest)) (walk (cdr rest) out))
      ((not (pair? (car rest))) (walk (cdr rest) (cons (car rest) out)))
      (else
       (let ((form (car rest)))
         (case (car form)
           ((begin) (walk (append (cdr form) (cdr rest)) out))

           ((export)
            (for-each note-public! (cdr form))
            (walk (cdr rest) out))

           ;; A body-level module declaration cannot be `import' (R6RS allows
           ;; it only at the top of a module), so it becomes the runtime lookup
           ;; the old transpiler also emitted.
           ((use-modules inherit-modules)
            (walk (append (map (lambda (m) `(module-provide ',m)) (cdr form))
                          (cdr rest))
                  out))

           ((define-public provide-public
             define-public-macro define-export-macro define-module-macro)
            (let ((name (v2-form-name form)))
              (note-public! name)
              (note-local! name)
              (walk (cdr rest) (cons (v2-rewrite-definition form) out))))

           ((define define-macro define-syntax)
            (note-local! (v2-form-name form))
            (walk (cdr rest)
                  (cons (if (and (eq? (car form) 'define) (v2-cpp-alias? form))
                            `(define ,(cadr form)
                               (top-level-value ',(caddr form)))
                            (v2-rewrite-definition form))
                        out)))

           ((tm-define tm-define-macro define-table define-collection
             extend-table extend-collection define-state define-group
             define-menu define-widget define-preferences
             define-top-level-syntax lazy-define smart-table
             define-record-type define-values)
            (note-local! (v2-form-name form))
            (walk (cdr rest) (cons form out)))

           ;; These two synthesise their exported name from a style / test
           ;; name, exactly as compiler-scan.scm's `scan-collect' does.
           ((bib-define-style)
            (let ((style (cadr form)))
              (note-public! (string->symbol (string-append "bib-" style "?")))
              (walk (cdr rest) (cons form out))))

           ((cas-define-test)
            (let* ((name (cadr form))
                   (table (string->symbol
                            (string-append (symbol->string name) "-table")))
                   (predicate (string->symbol
                                (string-append (symbol->string name) "?"))))
              (note-local! predicate)
              (note-local! table)
              (walk (cdr rest) (cons form out))))

           (else
            (if (v2-conditional? form)
                (walk (append (v2-resolve-conditional form) (cdr rest)) out)
                (walk (cdr rest) (cons form out)))))))))
  (walk forms '()))

;; ---- The macro helper set ------------------------------------------------

;; Every symbol a form can *call*.  Deliberately a plain syntactic walk rather
;; than compiler-oracle.scm's expand-based `mc-free-references': expanding a
;; kernel form needs the macro environment to be live, which is the very thing
;; this pass is trying to stop depending on, and `mc-free-references' answers
;; '() when expansion fails -- an under-approximation, which would silently
;; leave a helper behind.  A syntactic walk over-approximates instead, and an
;; extra name in the helper module costs nothing.
;;
;; Quoted data is skipped (a symbol in a quoted list is not a call), and inside
;; a quasiquote only the unquoted parts are live.
(define (v2-called-symbols form)
  (define acc '())
  (define (add! s) (unless (memq s acc) (set! acc (cons s acc))))
  (define (walk-quasi x)
    (cond ((pair? x)
           (case (car x)
             ((unquote unquote-splicing) (walk (cdr x)))
             (else (walk-quasi (car x)) (walk-quasi (cdr x)))))
          (else (void))))
  (define (walk x)
    (cond ((symbol? x) (add! x))
          ((pair? x)
           (case (car x)
             ((quote) (void))
             ((quasiquote) (walk-quasi (cdr x)))
             (else (walk (car x)) (walk (cdr x)))))
          ((vector? x) (vector-for-each walk x))
          (else (void))))
  (walk form)
  acc)

;; The transitive closure of "a name this file defines that a macro calls".
;; `defs' maps every locally defined name to its definition form, movable or
;; not, so a macro that reaches a `tm-define' shows up as V2-NONMOVABLE instead
;; of disappearing.
(define (v2-helper-closure macro-forms defs)
  (define seen (make-eq-hashtable))
  (define queue '())
  (define (enqueue! name)
    (when (and (symbol? name)
               (hashtable-ref defs name #f)
               (not (hashtable-ref seen name #f)))
      (hashtable-set! seen name #t)
      (set! queue (cons name queue))))
  (define (visit form)
    (for-each enqueue! (v2-called-symbols form)))
  (for-each visit macro-forms)
  (let loop ()
    (unless (null? queue)
      (let ((name (car queue)))
        (set! queue (cdr queue))
        (visit (hashtable-ref defs name #f))
        (loop))))
  seen)

;; ---- Emitting a module ---------------------------------------------------

(define (v2-keyword-definitions forms)
  (map (lambda (keyword) `(define ,keyword ',keyword))
       (tm-collect-keywords forms '())))

(define (v2-write port object)
  (write object port)
  (newline port))

;; One transpiled file holds two modules:
;;
;;   (module <mod>$h (<helper names> ...) <imports> <helper defs>)
;;   (module <mod> () (import (for <mod>$h run expand)) <imports>
;;                   (export <public names>)
;;                   <definitions> <module-provides> <expressions>)
;;
;; The helper module is a *sibling*, not a child: an internal module nested
;; inside `(module <mod> ...)' is pinned to phase 0 and cannot be imported with
;; `(for ... run expand)' (PHASED_HELPERS_PLAN.md §2.2).
;;
;; It is *not* re-exported by the main module, which is what keeps it private:
;; nothing else in the build imports it, so its names stop at this file's main
;; module.  That is also why its name does not need to be uninterned -- the
;; encapsulation comes from the missing re-export, not from the spelling.
;;
;; The export list is the module's real interface: the names its source
;; declares with `define-public' / `provide-public' / `define-public-macro' /
;; `define-module-macro' / `export'.  A plain `(define (foo ...) ...)' stays
;; private to the module -- including when it was moved into the helper module,
;; in which case it is imported here for use and left out of the export list.
;; Chez allows exporting a binding that came from an import (verified), so a
;; `define-public' helper needs no special case.
;; One transpiled file holds two modules:
;;
;;   (eval-when (compile eval load) (module <mod>$h (<helpers> ...) ...))
;;   (module <mod> () (import (for <mod>$h run expand)) ...)
;;
;; The helper module is a *sibling*, not a child: an internal module nested
;; inside `(module <mod> ...)' is pinned to phase 0 and cannot be imported with
;; `(for ... run expand)' (PHASED_HELPERS_PLAN.md §2.2).
;;
;; The `eval-when' wrapper is what makes the same file work.  `(import (for
;; <mod>$h run expand))' hands the transformer the helper's *bindings* at phase
;; 1 but does not instantiate the module, so a macro body that calls a helper
;; fails with "variable <helper> is not bound" unless the helper has been
;; evaluated first.  Wrapping the module form in `(eval-when (compile eval
;; load))' evaluates it while this file is being compiled, which is exactly
;; when the transformer needs it.  Measured on Chez 10.4.1: without the
;; wrapper the one-file shape does not compile; `(compile)', `(compile load)',
;; `(compile eval)' and `(compile eval load)' all work when the file is
;; compiled, and only `(compile eval load)' also works when it is loaded as
;; source.
(define (v2-write-module-file out-path rel-path mod-sym helper-sym
                              imports helper-names keywords
                              helper-defs helper-exprs definitions dep-provides
                              expressions public)
  (call-with-output-file out-path
    (lambda (port)
      (format port ";;; Auto-generated native Chez Scheme module from ~a~%"
              rel-path)
      (format port ";;; helper module: ~a~%" helper-sym)
      ;; Helper module: static export list, so a private helper crosses the
      ;; file boundary here and nowhere else -- and only this module imports it.
      ;; `helper-exprs' holds the option registrations this file's own
      ;; `tm-define's need while they expand (see `v2-selected-registrations');
      ;; they run when the helper is instantiated, which is before the main
      ;; module below is expanded.
      (v2-write port
                `(eval-when (compile eval load)
                   (module ,helper-sym (,@helper-names)
                     ,@imports
                     ,@keywords
                     ,@helper-defs
                     ,@helper-exprs)))
      ;; Main module.  `public' is exactly what the source declared public; a
      ;; helper that was declared public is imported above and re-exported here
      ;; by name, and a helper that was private simply is not listed.
      (v2-write port
                `(module ,mod-sym ()
                   (import (for ,helper-sym run expand))
                   ,@imports
                   ,@(if (null? public) '() `((export ,@public)))
                   ,@keywords
                   ,@definitions
                   ,@dep-provides
                   ,@expressions)))
    'replace))

;; ---- Imports --------------------------------------------------------------

;; Module symbol -> the names it exports, filled in as this run transpiles.
;; A dependency that is not in the table yet has not been reached, so the
;; module gets a runtime `module-provide' for it instead of an `import'.
(define *v2-exports* (make-eq-hashtable))

;; Every public name in the tree -> the module that exports it.  Built once
;; from the sources, before anything is compiled.
;;
;; This exists because the TeXmacs sources are not module-clean: a kernel file
;; will use a name another module exports without declaring `:use' for it
;; (tm-preferences.scm calls `ahash-ref', which ahash-table.scm exports, and
;; says nothing about it).  Under the flat namespace of Guile, and under the
;; old pipeline's eager evaluation, that just worked.  Here the reference has
;; to become a real import, so the translator infers the `:use' the source
;; left out -- one import per module whose public names this file mentions.
;;
;; Only public names are indexed.  A private name of another module is not
;; reachable by design, and a reference to one is a genuine error.
(define *v2-name-index* (make-eq-hashtable))

(define (v2-index-tree! files)
  (for-each
    (lambda (rel-path)
      (guard (e (else #f))
        (let* ((forms (read-all-forms (progs-path rel-path)))
               (header (scan-header forms)))
          (when header
            (let ((mod-sym (mod-name->symbol (cadr header))))
              (call-with-values
                (lambda () (scan-collect forms))
                (lambda (exports . _)
                  (for-each
                    (lambda (name)
                      (unless (hashtable-ref *v2-name-index* name #f)
                        (hashtable-set! *v2-name-index* name mod-sym)))
                    exports))))))))
    files))

(define (v2-import-forms deps extra-modules local)
  (define imported '())
  (define forms '())
  (define (emit! module)
    (let ((names (hashtable-ref *v2-exports* module #f)))
      (when names
        (let* ((clash (filter (lambda (name)
                                (or (memq name local) (memq name imported)))
                              names))
               (kept (filter (lambda (name) (not (memq name clash))) names)))
          (set! imported (append kept imported))
          (set! forms
                (cons (if (null? clash)
                          `(import (for ,module run expand))
                          `(import (for (except ,module ,@clash) run expand)))
                      forms))))))
  ;; Declared `:use' first, then the inferred ones.
  (for-each (lambda (dep) (emit! (mod-name->symbol dep))) deps)
  (for-each emit! extra-modules)
  (reverse forms))

;; The modules a file needs but did not declare: for every free name in its
;; body that another module exports, that module.  Only modules this run has
;; already compiled qualify -- a forward reference cannot be imported, and
;; stays the runtime `module-provide' the declared deps also fall back to.
(define (v2-inferred-modules mod-sym prepared local deps)
  (define skip (cons mod-sym (map (lambda (dep) (mod-name->symbol dep)) deps)))
  (define owners '())
  (for-each
    (lambda (name)
      (unless (memq name local)
        (let ((owner (hashtable-ref *v2-name-index* name #f)))
          (when (and owner
                     (not (memq owner skip))
                     (not (memq owner owners))
                     (hashtable-ref *v2-exports* owner #f))
            (set! owners (cons owner owners))))))
    (v2-called-symbols (cons 'begin prepared)))
  (reverse owners))

;; ---- Transpiling one module --------------------------------------------

;; Definitions in source order, split into the ones a macro needs at expand
;; time and the ones it does not.
;; The stateful forms to move into the helper module, closed to a fixed point.
;;
;; One pass is not enough: a `tm-define' that a macro calls references other
;; `tm-define's and `define-table's, and those references only enter the
;; closure once the form itself is in it.  Iterating until nothing new is
;; selected is what keeps a cluster on one side of the boundary -- menu-define's
;; `gui-make' -> `gui-make-table' -> its `extend-table' population ->
;; `gui-make-section-tabs' is one such cluster.
(define *stateful-heads*
  '(tm-define tm-define-macro define-table extend-table
    define-state define-collection extend-collection define-group
    smart-table))

(define (v2-dedupe l)
  (let loop ((rest l) (acc '()))
    (cond ((null? rest) (reverse acc))
          ((memq (car rest) acc) (loop (cdr rest) acc))
          (else (loop (cdr rest) (cons (car rest) acc))))))

;; ---- Option registrations ------------------------------------------------
;;
;; `tm-define' consults `define-option-table' *while it expands* and raises
;; "unknown option :kw" when the keyword is missing (tm-define.scm:274).  A file
;; that registers an option and then uses it in one of its own `tm-define's
;; therefore has to register it before its body is expanded: graphics-env.scm
;; installs `:state-slots' at line 386 and writes `(:state-slots graphics-state)'
;; at line 396.  In the emitted file the registration is an expression, so it
;; runs after the whole body has been expanded -- which is why the option is
;; unknown at the moment it is needed.
;;
;; The helper module is the one place that runs earlier: the `eval-when' wrapper
;; instantiates it while the file is being compiled, and the main module is
;; expanded afterwards.  Moving the registration there fixes the order.
;;
;; Moving *every* registration is too blunt -- measured at 280/453 with 173
;; failures, because tm-define.scm's own fifteen registrations would drag the
;; whole option vocabulary, and the procedures behind it, across the boundary.
;; The rule here is the narrow one the plan asks for: move a registration only
;; when the keyword it installs is used as an option by a `tm-define' in the
;; same file.
(define (v2-keyword-symbol? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
         (and (> (string-length s) 0)
              (char=? (string-ref s 0) #\:)))))

;; The forms whose bodies carry options, and whose expansion is what consults
;; the table.  `tm-property' shares `tm-define-sub''s option handling.
(define *option-using-heads* '(tm-define tm-define-macro tm-property))

;; (ahash-set! define-option-table :kw handler)
(define (v2-option-registration? form)
  (and (pair? form)
       (eq? (car form) 'ahash-set!)
       (pair? (cdr form))
       (eq? (cadr form) 'define-option-table)
       (pair? (cddr form))
       (v2-keyword-symbol? (caddr form))
       (pair? (cdddr form))
       (symbol? (cadddr form))))

;; The keyword a registration installs.  The handler itself needs no accessor:
;; the registration form seeds the helper closure as a whole, so `v2-called-symbols'
;; picks the handler up along with everything it calls.
(define (v2-registration-keyword form) (caddr form))

;; The option keywords this file's `tm-define'-family forms use.  An option is a
;; body form whose car is a keyword symbol: (tm-define (f x) (:state-slots s) ...).
;; Only the body is scanned, not the head, so a keyword appearing inside a
;; definition's own code does not count.
(define (v2-options-used prepared)
  (define acc '())
  (for-each
    (lambda (form)
      (when (and (pair? form) (memq (car form) *option-using-heads*))
        (for-each
          (lambda (item)
            (when (and (pair? item)
                       (v2-keyword-symbol? (car item))
                       (not (memq (car item) acc)))
              (set! acc (cons (car item) acc))))
          (cddr form))))
    prepared)
  acc)

;; The registration expressions that have to run before this file is expanded.
(define (v2-selected-registrations prepared)
  (let ((used (v2-options-used prepared)))
    (filter (lambda (form)
              (and (v2-option-registration? form)
                   (memq (v2-registration-keyword form) used)))
            prepared)))

(define (v2-stateful-closure macro-forms other-forms defs)
  (let loop ((needed (v2-helper-closure macro-forms defs))
             (moved '()))
    (let* ((new (filter (lambda (form)
                          (and (pair? form)
                               ;; `define-state' always moves.  It expands to
                               ;; `(define-public slot ...)', so it is a real
                               ;; binding: the helper can export it and the main
                               ;; module imports it back, which is what lets the
                               ;; same value serve the expand-time `(:state-slots
                               ;; graphics-state)' option and the run-time readers
                               ;; in the same file.  Conditional selection cannot
                               ;; decide this -- the need is a property of the
                               ;; form, not of who calls it.
                               (and (memq (car form) *stateful-heads*)
                               (or (hashtable-ref needed (v2-form-name form) #f)
                                   ;; `extend-table' binds nothing; it travels
                                   ;; with the table it populates.
                                   (and (eq? (car form) 'extend-table)
                                        (pair? (cdr form))
                                        (hashtable-ref needed (cadr form) #f))))))
                        other-forms))
           ;; `new' is recomputed over every form each round, so it re-offers
           ;; what is already moved; dedupe before comparing or the loop never
           ;; settles.
           (all (v2-dedupe (append new moved))))
      (if (= (length all) (length moved))
          (values moved needed)
          (loop (v2-helper-closure (append macro-forms all) defs) all)))))

(define (v2-partition defs helper-set)
  (let loop ((rest defs) (helpers '()) (others '()))
    (cond ((null? rest) (values (reverse helpers) (reverse others)))
          ((hashtable-ref helper-set (v2-form-name (car rest)) #f)
           (loop (cdr rest) (cons (car rest) helpers) others))
          (else (loop (cdr rest) helpers (cons (car rest) others))))))

;; The subset of NEEDED this pass is allowed to move.
(define (v2-movable-subset needed defs)
  (let ((table (make-eq-hashtable)))
    (vector-for-each
      (lambda (name)
        (let ((form (hashtable-ref defs name #f)))
          (when (and form
                     (memq (car form) *movable-heads*)
                     (not (memq name *aux-syntax-names*)))
            (hashtable-set! table name #t))))
      (hashtable-keys needed))
    table))

;; Returns (values MOD-SYM HELPER-DEFS), or (values #f '()) for a file with no
;; `texmacs-module' header.
(define (v2-transpile-module rel-path out-path)
  (let* ((forms (read-all-forms (progs-path rel-path)))
         (header (find (lambda (form)
                         (and (pair? form) (eq? (car form) 'texmacs-module)))
                       forms)))
    (if (not header)
        (begin
          (log-progress "Skipped" (string-append rel-path " (no header)"))
          (values #f '()))
        (let* ((mod-name (cadr header))
               (mod-sym (mod-name->symbol mod-name))
               ;; Deterministic, and not re-exported below: the only `import'
               ;; of it is the one this file emits, so a reader can find it in
               ;; the generated .scm without a gensym round trip.
               (helper-sym (string->symbol
                             (string-append (symbol->string mod-sym) "$h")))
               (deps (scan-header->deps header mod-name)))
          (call-with-values (lambda () (v2-prepare-forms (cdr forms)))
            (lambda (prepared public local)
              (let ((defs (make-eq-hashtable))
                    (macro-forms '()))
                ;; Index every local definition (movable or not) and collect
                ;; the macros that may need one at expand time.
                (for-each
                  (lambda (form)
                    (when (v2-binding? form)
                      (let ((name (v2-form-name form)))
                        (unless (hashtable-ref defs name #f)
                          (hashtable-set! defs name form))))
                    (when (and (pair? form) (memq (car form) *macro-definers*))
                      (set! macro-forms (cons form macro-forms))))
                  prepared)
                (let* ((option-regs (v2-selected-registrations prepared))
                       ;; The registrations seed the closure like a macro form
                       ;; does: whatever a registration names -- the handler,
                       ;; and through it the procedures the handler calls --
                       ;; has to be in the helper module for the registration
                       ;; to run there.
                       (seed (append macro-forms option-regs))
                       (def-forms (filter v2-definition? prepared))
                       (other-forms (filter (lambda (form)
                                              (not (v2-definition? form)))
                                            prepared)))
                  ;; One closure, run to a fixed point.  It starts from the
                  ;; macros and the option registrations and iterates through the
                  ;; `tm-define's it selects, so the value helpers those reach
                  ;; (menu-define's `gui-make-section-tabs', reached only from an
                  ;; `extend-table' body) come out in the same set.
                  (call-with-values
                    (lambda () (v2-stateful-closure seed other-forms defs))
                    (lambda (stateful needed)
                      (let* ((movable (v2-movable-subset needed defs))
                             (partition (call-with-values
                                            (lambda ()
                                              (v2-partition def-forms movable))
                                          list))
                             (helper-defs (append (car partition) stateful))
                             (definitions (cadr partition))
                             (other-forms
                              (filter (lambda (form)
                                        (and (not (memq form stateful))
                                             (not (memq form option-regs))))
                                      other-forms))
                             (helper-names
                              (map v2-form-name
                                   (filter (lambda (f)
                                             (not (memq (car f) *stateful-heads*)))
                                           helper-defs)))
                             (moved-names (map v2-form-name stateful))
                             (exported (filter (lambda (name)
                                                 (not (memq name moved-names)))
                                               public)))
                        ;; Names a macro needs at expand time that did *not* make
                        ;; it into the helper.  The only legitimate ones are a
                        ;; macro (which stays put by design, so a same-file
                        ;; reference expands sequentially) and a name the `...'
                        ;; exclusion keeps at home.  Anything else is a real hole
                        ;; and is reported.  This runs after the closure, so a
                        ;; `tm-define' the closure moved (menu-define's
                        ;; `gui-make') is not reported -- computed from the
                        ;; one-pass set it produced 19 reports, all of them moved
                        ;; a moment later.
                        (vector-for-each
                          (lambda (name)
                            (let ((form (hashtable-ref defs name #f)))
                              (unless (or (hashtable-ref movable name #f)
                                          (memq name moved-names)
                                          (memq name *aux-syntax-names*)
                                          (and form
                                               (memq (car form) *macro-definers*)))
                                (log-note (format "V2-NONMOVABLE ~a needs ~a"
                                                  mod-sym name)))))
                          (hashtable-keys needed))
                        (when (getenv "TEXMACS_V2_SHOW_MOVED")
                          (log-note (format "MOVED ~a -> ~a" mod-sym moved-names)))
                        (when (pair? option-regs)
                          (log-note (format "V2-OPTION-REGS ~a ~a"
                                            mod-sym
                                            (map v2-registration-keyword
                                                 option-regs))))
                        (v2-write-module-file
                          out-path rel-path mod-sym helper-sym
                          (v2-import-forms
                            deps
                            (v2-inferred-modules mod-sym prepared local deps)
                            local)
                          helper-names
                          (v2-keyword-definitions forms)
                          helper-defs
                          option-regs
                          definitions
                          (map (lambda (dep) `(module-provide ',dep)) deps)
                          other-forms
                          exported)
                        (hashtable-set! *v2-exports* mod-sym exported)
                        (values mod-sym (length helper-defs)))))))))))))

;; Compile one transpiled file and load it, so the next module's
;; `(import (for <dep> run expand))' can resolve.  Nothing is published into
;; the interaction environment: every cross-module reference a transformer
;; makes is a module import, at the phase the import declares.
;;
;; There is no eager pass and no oracle.  The definitions a macro calls at
;; expand time live in the sibling helper module, which the module imports
;; `(for ... run expand)'; that is the whole mechanism, and it is why this file
;; can stay a translator.
;;
;; `compile-export-mode?' wraps the compile only, not the load.  Holding it
;; across the load was tried and is wrong: `module-provide' is inert while it
;; is set, and the load of a module *is* where a `(module-provide '<dep>)' for
;; a dependency that was not compiled yet does its work.  That call is the only
;; thing that pulls such a dependency in and publishes its exports into the
;; interaction environment, which is how a later module's free identifier
;; resolves at run time.  Measured: holding the parameter across the load gives
;; 73/453 with 380 failures.
;;
;; The price is that a `module-provide' for a dependency that is *neither*
;; compiled yet *nor* missing loads whatever `.so' the previous run left in the
;; build directory -- see the LOAD-ERROR note in the plan, §7.6.  A build into
;; a fresh directory has no such file to pick up.
(define (v2-compile-and-load trans-scm out-so)
  (parameterize ((compile-export-mode? #t))
    (compile-file trans-scm out-so))
  (load out-so))

;; ---- Driver --------------------------------------------------------------

(define v2-compiled 0)
(define v2-failed 0)
(define v2-helper-defs 0)

;; Transpile, compile and load one module.
;;
;; Same handler shape as compile-texmacs.scm's `compile-module!': Chez's
;; compiler *raises* warnings (a suspicious call, an arity mismatch in an
;; unreachable branch), and a plain `guard' would abort the module on one.
;; Only a serious condition is a failure; a warning is reported and the
;; compilation resumes.
(define (v2-report-failure rel-path condition)
  (log-progress "FAILED" rel-path)
  (log-note (condition-text condition))
  (set! v2-failed (+ v2-failed 1)))

(define (v2-build-module rel-path)
  (let* ((trans-scm (boot-path rel-path))
         (out-so (scm->so trans-scm)))
    (ensure-dir trans-scm)
    (call/cc
      (lambda (escape)
        (with-exception-handler
          (lambda (condition)
            (when (serious-condition? condition)
              (v2-report-failure rel-path condition)
              (escape #f)))
          (lambda ()
            (call-with-values
              (lambda () (v2-transpile-module rel-path trans-scm))
              (lambda (mod-sym n-helpers)
                (when mod-sym
                  (set! v2-helper-defs (+ v2-helper-defs n-helpers))
                  (v2-compile-and-load trans-scm out-so)
                  (log-progress "Compiling"
                                (format "~a (helpers ~a)" rel-path n-helpers))
                  (set! v2-compiled (+ v2-compiled 1)))
                #t))))))))

(define (v2-run-tier tier)
  (emit-progress-line (string-append "-- Pass 1/2 v2: "
                                     (inventory-tier-title tier) " --"))
  (for-each (lambda (rel-path)
              (v2-build-module rel-path)
              (progress-tick!))
            (inventory-tier-files tier)))

(v2-index-tree! (apply append
                        (map inventory-tier-files *v2-tiers*)))

(progress-start! (apply + (map (lambda (tier)
                                 (length (inventory-tier-files tier)))
                               *v2-tiers*)))
(for-each v2-run-tier *v2-tiers*)

(newline)
(display "========================================================\n")
(format #t "  - Compiled: ~a module~a\n"
        v2-compiled (if (= v2-compiled 1) "" "s"))
(format #t "  - Helper definitions moved: ~a\n" v2-helper-defs)
(format #t "  - Failures: ~a\n" v2-failed)
(display "========================================================\n")
