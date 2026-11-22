# TeXmacs Chez Backend Optimization TODO List

This document tracks remaining open bugs, architectural improvements, and performance optimizations for the TeXmacs Chez Scheme port (`wip-chez`).

---

## 1. High Priority (Bugs, Stability & GC Safety)

### [!] Windows: User Manual Crash (`0xC00000FD` Stack Overflow in `list<rectangle>::~list()`)
- **Status**: Root cause identified via WER crash reports (`AppCrash_texmacs.exe_...`, `2026-11-17`); fix pending.
- **Location**: [`CMakeLists.txt`](CMakeLists.txt#L260), [`src/Kernel/Containers/list.hpp`](src/Kernel/Containers/list.hpp#L39-L48), `src/Typeset/Line/typesetter.cpp`
- **Symptom**:
  Opening the User manual on Windows closes `texmacs.exe` before displaying anything.
- **Evidence**:
  - WER `Report.wer` records `Exception Code: c00000fd` (stack overflow) at offset `0xcdd8c7` in `texmacs.exe`.
  - Symbolizing `0x140cdd8c7` in `build-win/src/texmacs.exe` gives `list<rectangle>::~list() [clone .isra.0] + 55` (originating in `typesetter.cpp`).
  - `list_rep<T>` in `src/Kernel/Containers/list.hpp` has a recursive destructor (`list<T> next`), while `CMakeLists.txt:260` sets `-Wl,-stack,8388608` only for Cygwin — leaving MinGW/Windows `texmacs.exe` with the default 1 MB stack.
- **Action Plan**:
  1. Add `-Wl,--stack,16777216` (or `8388608`) for `WIN32`/`MINGW` in `CMakeLists.txt` (and/or make `list_rep::~list_rep()` iterative).
  2. Rebuild, repackage with `packages/windows/make-bundle.sh --verify`, and verify opening the User manual in the Windows GUI.

### [!] Windows: Interactive Math Display & Terminal Logging
- **Status**: Open; needs targeted GUI investigation.
- **Symptom**:
  1. Typing math in the Windows GUI does not display correctly.
  2. Launching `texmacs.exe` from `cmd`/PowerShell shows no console logs because `texmacs.exe` is built as a `WIN32` GUI subsystem binary (stdout/stderr must be redirected to a file or run from MSYS2 bash).
- **Observations**:
  - Scripted insertion of `(math (concat "x" (rsup "2") "+<alpha>+<sum><int>"))` in `build-win/src/texmacs.exe` loaded `cmr10`, `cmmi10`, `cmsy10`, and `texgyrepagella-math.otf` without font-open errors (one `TeXmacs] warning, missing font, loading global substitution list` was logged).
  - `%APPDATA%\TeXmacs-chez\system\preferences.scm` showed `("look and feel" "emacs")` and `("shift-52" "$")`.
- **Action Plan**:
  - Reproduce interactive math typing in the Windows GUI and inspect the generated tree / font substitution / Qt glyph rendering.

### [!] `save-object` fails on the quit path: `open-output-file: ... file exists`
- **Location**: [`TeXmacs/progs/kernel/boot/chez-compat.scm`](TeXmacs/progs/kernel/boot/chez-compat.scm#L438-L444) (`open-file`), [`TeXmacs/progs/kernel/boot/abbrevs.scm`](TeXmacs/progs/kernel/boot/abbrevs.scm#L64-L76) (`save-object`), `save-learned` → `quit-TeXmacs-scheme`
- **Symptom** (only with a real `$HOME` that already has the file):
  ```
  (TM-CALL-ERROR failed for ~/.TeXmacs-chez/system/interactive.scm: file exists
                 FUN: quit-TeXmacs-scheme ARGS: ())
  Exception in open-output-file: failed for ~/.TeXmacs-chez/system/interactive.scm: file exists
    (FRAME 0 CODE save-object SRC (open-file (url-materialize file ) OPEN_WRITE))
    (FRAME 1 CODE quit-TeXmacs-scheme SRC (($top-level-value 'save-learned)))
  ```
- **Problem**: `save-object` is `(let ((out (open-file (url-materialize file "") OPEN_WRITE))) (write value out) (close-port out))`, and `open-file` with mode `"w"` is `(if (file-exists? file) (delete-file file))` followed by `(open-output-file file)`. Chez's `open-output-file` raises `&i/o-file-already-exists` when the target exists, so the whole thing hinges on that delete taking effect. On the quit path it does not, and the save is lost.
- **Impact**: `save-learned` throws, and `on-exit` expands to `(lambda () cmd ... (old-quit))` — so `(old-quit)` never runs and **every shutdown hook registered before it is silently skipped**. The learned state (`interactive.scm`) is never written. Any other `save-object` caller reached during shutdown has the same problem: user shortcuts, the personal spell dictionary, the GPG wallet, the server user DB. The failure is console-only, so the user-visible symptom is "my shortcuts / preferences keep reverting".
- **Ruled out** (do not re-investigate these):
  1. **Not glue shadowing.** `delete-file` and `file-exists?` are `(chezscheme)` primitives — there is no glue override for either in `src/Scheme/Glue/`, and neither is defined anywhere in `TeXmacs/progs/`.
  2. **Not "existing files always fail".** In a clean run, `(save-object "~/.TeXmacs-chez/system/x.scm" 99)` on an existing file returns `#<void>` and overwrites it (`file-exists?` → `#t`).
  3. **Not specific to `interactive.scm`.** `save-learned`'s real argument is `"$TEXMACS_HOME_PATH/system/interactive.scm"`, which `url-materialize` expands to the correct absolute path with `file-exists?` → `#t` at the moment of failure.
  4. **Not the `chez-compat` refactor.** `open-file`'s body is byte-identical to the pre-change copy in `boot-chez.scm`; the bug is pre-existing and was only never exercised because headless runs used an empty home directory where the file never pre-exists.
- **Key observation**: `(save-learned)` invoked manually from `-x` **succeeds** (`RES=#<void>`). The failure is on the **second** call — the one made by `quit-TeXmacs-scheme` during shutdown — with the same path and `file-exists?` → `#t`. So this is a shutdown-ordering problem, not a missing delete in general.
- **Next step**: wrap `delete-file` on the quit path to see whether it is called at all there and what it returns; check whether something re-creates the file between the delete and the open (the C++ side may `save` the same file during teardown), and whether tmfs/`url-materialize` teardown changes the path at shutdown. A defensive fix is to stop relying on delete-then-create (`open-output-file` with an explicit truncate option) — but confirm the mechanism first.
- **Reproducer**: with an existing `~/.TeXmacs-chez/system/interactive.scm`, `HOME=$HOME TEXMACS_BOOT_PATH=$PWD/build-texmacos/boot ./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs -H` exits 0 but prints the error at quit.
- **Diagnostic traps**: (a) never park test `.scm` files in `~/.TeXmacs-chez/system/` — TeXmacs loads `system/*.scm` at startup, so a stray file silently kills the boot; (b) `kill -9` on the app leaves `~/.TeXmacs-chez/system/boot_lock`, which blocks the next start with no output at all — `rm` it before re-testing.

### [ ] Add Missing `tmscm_is_pair` Guard in `tmscm_is_path`
- **Priority**: High (Crash Safety)
- **Location**: [`src/Scheme/Scheme/glue.cpp:569-573`](src/Scheme/Scheme/glue.cpp#L569-L573)
- **Problem**:
  - `tmscm_is_path` currently calls `tmscm_car (p)` without first checking `tmscm_is_pair (p)`:
    ```cpp
    if (tmscm_is_null (p)) return true;
    else return tmscm_is_int (tmscm_car (p)) && tmscm_is_path (tmscm_cdr (p));
    ```
  - Passing a non-pair (e.g. a string or integer, such as `(tree-search-tree "a" "b" "bad-path" 10)`) to any `TMSCM_ASSERT_PATH` routine triggers a `SIGSEGV` instead of a clean wrong-type exception (discovered while writing [`docs/chez/bench/gc-roots-stress.scm`](docs/chez/bench/gc-roots-stress.scm)).
- **Action Plan**:
  - Change line 572 to:
    ```cpp
    else return tmscm_is_pair (p) && tmscm_is_int (tmscm_car (p)) && tmscm_is_path (tmscm_cdr (p));
    ```
  - Safe for all Scheme backends (shared `glue.cpp` path).

### [ ] Missing Conversion Route for `(convert doc "texmacs-tree" "latex-document")` / `buffer-export` in `tm-convert.scm`
- **Location**: [`TeXmacs/progs/kernel/texmacs/tm-convert.scm:272-292`](TeXmacs/progs/kernel/texmacs/tm-convert.scm#L272-L292), [`TeXmacs/progs/convert/latex/init-latex.scm`](TeXmacs/progs/convert/latex/init-latex.scm)
- **Problem**:
  Direct serialization via `(serialize-latex (texmacs->latex doc '()))` works cleanly, and `(export-buffer-main ... "latex" ...)` succeeds in `conversions.scm`. However, high-level export via `(texmacs->generic doc "latex-document")` or `buffer-export` returns `"Error: bad format or data"` (also observed in `WINDOWS_MSYS2.md`).
  This indicates that the dynamic converter table in `tm-convert.scm` does not have `latex-document` registered as a target format from `texmacs-tree` until `(convert latex init-latex)` or similar converter declarations are executed (or `converter-search` cannot bridge `texmacs-tree` → `latex-document` without `"latex"`).
- **Action**:
  Investigate lazy registration / converter route resolution when `texmacs->generic` or `buffer-export` is invoked.

### [ ] Dynamic Menu Evaluation: Guard `#f` in `make-menu-items` (`(TM-APPLY-ERROR #f is not a pair)`)
- **Location**: [`TeXmacs/progs/kernel/gui/menu-widget.scm:805-825`](TeXmacs/progs/kernel/gui/menu-widget.scm#L805-L825), [`src/Scheme/Chez/chez_tm.cpp`](src/Scheme/Chez/chez_tm.cpp)
- **Problem**: When a dynamic menu thunk returns `#f` (e.g. inactive mode or delayed plugin menu not yet initialized), `make-menu-items` falls through `(if (pair? p) ...)` to `(make-menu-bad-format p style)`. Calling `car`/`cdr` on `#f` triggers Chez Scheme's `~s is not a pair (#f)`.
- **Solution**: Guard against non-pair/`#f` values in `make-menu-items` to treat `#f` as an empty item list `'()` rather than logging `menu has bad format (make-menu)` or throwing in Scheme.

### [ ] Explicit Error Reporting in `wrap-catch` & Menu Exception Masking
- **Location**: [`TeXmacs/progs/kernel/boot/debug.scm:316-332`](TeXmacs/progs/kernel/boot/debug.scm#L316-L332), [`TeXmacs/progs/kernel/gui/menu-widget.scm:1190-1195`](TeXmacs/progs/kernel/gui/menu-widget.scm#L1190-L1195), [`src/Texmacs/Window/tm_frame.cpp:28-38`](src/Texmacs/Window/tm_frame.cpp#L28-L38)
- **Problem**:
  - `(wrap-catch proc)` wraps menu-to-widget generation in `make-menu-widget`.
  - When exceptions occur inside dynamic menu generation, submenus, or widget constructors, `as_widget` in C++ receives nil or an error pair, causing `make_menu_widget` (`tm_frame.cpp:28-38`) to silently fall back to an empty menu `horizontal_menu(0)`.
  - Empty menu widgets have height 0, causing macOS Cocoa QPA to report:
    `qt.qpa.window: Window position QRect(...) outside any known screen, using primary screen`.
  - Because exceptions can be swallowed or discarded without clear diagnostics reaching the terminal, developers cannot easily see which Scheme procedure or menu item failed.
- **Action Plan**:
  - Make `wrap-catch` (and `tm-catch-runtime` in `boot-chez.scm`) output explicit error diagnostics (`display-condition`, stack context) when catching conditions during menu building.
  - In `tm_frame.cpp:make_menu_widget`, log an explicit warning with the menu expression whenever `as_widget` returns nil instead of silently returning an empty 0-height menu.

---

## 2. Compiler & Module Architecture

### [!] Warning: Eager Evaluation Side Effects & Stateful Macro Branching (`if`/`else` at Expand Time)
- **Location**: [`src/Scheme/Chez/compiler-helper.scm`](src/Scheme/Chez/compiler-helper.scm#L210), macro definers across `TeXmacs/progs/`
- **Problem**:
  Macros that inspect global tables or mutable variables to conditionally emit different code branches (e.g. `(if (ahash-ref table key) ...)` or `(if (top-level-bound? ...) ...)`) might not compile correctly under our ahead-of-time pipeline.
  Because Pass 2 (`compiler-helper.scm`) eagerly evaluates selected forms into `(compilation-environment)` via `eval-eager-form!` before `compile-file` runs macro expansion, mutable tables in compiler memory are modified before macro expansion completes. As a result, the expander can see post-eager-eval state (taking the `else` or "already initialized" branch) rather than the clean initial state that should exist when the compiled `.so` loads at runtime.
  - *Example (`define-group` in `variants.scm`)*: Eager eval populated `group-table`, causing `compile-file` macro expansion to see `old != #f` and omit procedure declarations (`tm-define (group-list) ...`) from compiled `.so` files.
  - *Example (`state-slots` in `graphics-env.scm`)*: Unquoting mutable slot lists inside macro templates (`(- (length (memq ,i ,state-slots)) 1)`) relied on expand-time environment bindings, failing downstream when compiled across module boundaries.
- **Action Plan**:
  1. **Defer Stateful Branching to Runtime**: Whenever possible, avoid expand-time `(if <table-ref> ...)` branching; move the conditional directly into the emitted runtime AST (e.g. `(begin (if (ahash-ref ...) ...) ...)`) so that table mutations and definitions occur at module load time rather than during compilation.
  2. **Audit Macros for Expand-Time State Dependencies**: Review macro definers (`define-macro`, `define-public-macro`, `define-module-macro`) across `TeXmacs/progs/` that query hash tables, property lists, or top-level bound status during expansion.
  3. **Phase-Isolated Compilation Environment**: Investigate isolating eager evaluation or resetting stateful tables before `compile-file` to prevent expand-time state leakage.

### [ ] Smart Downstream Invalidation via Interface Hash (`interface-hash` in `compiler-db`)
- **Location**: [`src/Scheme/Chez/convert-to-r6rs.scm:229-264`](src/Scheme/Chez/convert-to-r6rs.scm#L229-L264), [`src/Scheme/Chez/compiler-db.scm`](src/Scheme/Chez/compiler-db.scm)
- **Problem**:
  - The incremental compilation system (`compute-dirty-modules!`) currently invalidates all downstream successors in `*compile-graph*` whenever any non-kernel module's source changes, and cached `.so` files can still suffer FASL descriptor mismatches if stale artifacts remain (see `docs/chez/README.md` §4 & §9.3).
  - Modifying a single mid-graph utility (like `bib-complete.scm` or `tree.scm`) forces recompilation of 150+ downstream modules, even if the change was purely an internal implementation fix or performance optimization that altered no public macros or exported signatures.
- **Background & Available Infrastructure**:
  - `compiler-db.scm` already computes and stores `interface-hash` for each module via `(scan-interface-hash exports global-macros macro-impls)`.
  - In Chez Scheme, downstream modules compiling against module $M$ only depend on $M$'s expand-time definitions (macro transformers, expand-time helpers in `*oracle-defs*`, and exported public identifiers). Runtime value definitions (procedure bodies) are compiled as runtime references and do not affect the compilation or machine code of downstream modules.
- **Action Plan**:
  - In `convert-to-r6rs.scm:compute-dirty-modules!`, compare `(module-info-interface-hash old-info)` against `(module-info-interface-hash new-info)` when a module is directly changed:
    1. *Interface Unchanged*: If `interface-hash` is identical (meaning only internal runtime function bodies, comments, or private values changed, without altering `exports`, `global-macros`, or `macro-impls`), mark only the changed module for recompilation. **Do not propagate dirty status to downstream successors**.
    2. *Interface Changed*: If `interface-hash` differs (e.g. an export was added/removed, or a macro signature/transformer was modified), mark both the module and transitively invalidate all its dependents via `(compile-graph-dependents *compile-graph*)`.
  - For kernel modules (`*kernel-files*`), sequential inheritance still applies: changes to kernel modules rebuild the cumulative snapshot `texmacs-user.so` and invalidate all tiers.

### [ ] Switch from `(module ...)` to R6RS `(library ...)` Syntax
- **Location**: [`src/Scheme/Chez/convert-to-r6rs.scm`](src/Scheme/Chez/convert-to-r6rs.scm), [`src/Scheme/Chez/compile-texmacs.scm`](src/Scheme/Chez/compile-texmacs.scm), [`TeXmacs/progs/kernel/boot/boot-chez.scm`](TeXmacs/progs/kernel/boot/boot-chez.scm)
- **Motivation & Benefits**:
  - Chez Scheme's internal `(module name ...)` form uses flat symbol identifiers (e.g. `kernel:boot:srfi`).
  - Standard R6RS `(library (dir-A dir-B module-name) (export ...) (import ...) body...)` natively supports structured hierarchical path identifiers: `(kernel boot srfi)`.
  - This directly mirrors TeXmacs's module declarations (`(texmacs-module (kernel boot srfi) (:use (kernel boot abbrevs)))`), eliminates synthetic symbol mangling (`spec->symbol`), enables Chez's automatic library path resolution (`library-directories`), and aligns 1:1 with TeXmacs's `use-modules` / `inherit-modules` semantics.
  - **Dynamic Procedure-Level Loading & `lazy-define` without `eval`**:
    - With R6RS libraries, Chez Scheme's `(environment <import-spec> ...)` procedure can dynamically construct environments from runtime library specs without requiring `eval`:
      ```scheme
      (define (load-plugin-fn lib-spec fn-sym)
        (let ([lib-env (environment lib-spec)])
          (top-level-value fn-sym lib-env)))
      ```
    - In contrast, Chez's `(module ...)` forms live in the lexical/interaction environment and cannot be resolved by `environment` (which raises `unknown module`), requiring `(eval `(import ...) (interaction-environment))` to dynamically load.
- **Architectural Prerequisite (The R6RS Library Acyclic Invariant)**:
  - Standard R6RS strictly forbids circular dependencies between libraries (R6RS §7.1: *"A library definition must not import, directly or indirectly, the library being defined"*).
  - TeXmacs upstream has mutual/cyclic dependencies between modules (particularly across menus, editors, and tools; currently resolved under `(module ...)` via `DEFER-CIRCULAR-IMPORT` + runtime `module-provide` in `exprs`).
  - **Strategy Decision**: To migrate to R6RS `(library ...)`, the codebase's module dependency graph must either be refactored to be strictly acyclic (DAG) — or co-located into single compilation units for tightly coupled cycles (e.g. `tmdoc-menu.scm` + `tmdoc-edit.scm`) — OR we retain the `(module ...)` architecture as-is.

### [ ] Inline Boot-Level Helpers (`==`) Without Breaking the Boot Loader
- **Location**: [`TeXmacs/progs/kernel/boot/boot-chez.scm`](TeXmacs/progs/kernel/boot/boot-chez.scm), [`TeXmacs/progs/kernel/boot/abbrevs.scm`](TeXmacs/progs/kernel/boot/abbrevs.scm), [`src/Scheme/Chez/compile-texmacs.scm`](src/Scheme/Chez/compile-texmacs.scm)
- **Observation**:
  `==` is a normal procedure, `(define-public (== x y) (equal? x y))`. At `optimize-level` 2 Chez still leaves `(== x y)` as a call because each module is compiled separately: the compiler imports the binding, not the body. Inlining requires the definition to be visible in the same compilation unit (or an explicit macro/inline form).
- **Compile-Flow Problem**:
  `boot-chez.scm` is loaded from source at startup (`compile-texmacs.scm` itself needs it), and `abbrevs.scm` is a separate module. Moving `==` into a macro or compiling a minimal bootstrap unit changes the compile flow.
- **Proposed Direction**:
  1. **Compile a minimal bootstrap unit first**: extract the smallest boot-loading script, compile it, load it, then compile the rest of the tree against that compiled bootstrap.
  2. **Move small hot helpers into a compiled module**: e.g. `==`, `func?`, `nnull?`, `npair?` could live in a normal compiled module once the bootstrap is compiled, so later modules import a definition the optimizer can see (with cross-module inlining or R6RS library optimization).
  3. **Use an explicit inline/macro form where safe**: `==` could expand to `equal?` at call sites, but only if first-class uses such as `(map == ...)` are either eliminated or kept in a compatibility procedure.
- **Caveats / Acceptance Criteria**:
  - `docs/chez/bench/CONVERSIONS_PROFILING.md` §3.5 measured `==` and `func?` wrapper overhead at only **~6–9 ms out of 1.7 s** (≈0.4%) in LaTeX conversion, so this is primarily a compile-flow enabler for broader inlining, not an immediate hot-path fix.
  - Verify with `-compile-modules` (0 failures), normal startup, and the conversions benchmark after changing the bootstrap order.

### [ ] Multi-Stage Plan: Turn `chez-compat` and `boot-chez` into Auto-Imported R6RS `(library ...)` Units for Cross-Module `cp0` Inlining
- **Locations**:
  - [`TeXmacs/progs/kernel/boot/chez-compat.scm`](../../TeXmacs/progs/kernel/boot/chez-compat.scm)
  - [`TeXmacs/progs/kernel/boot/boot-chez.scm`](../../TeXmacs/progs/kernel/boot/boot-chez.scm)
  - [`TeXmacs/progs/kernel/boot/glue-chez.scm`](../../TeXmacs/progs/kernel/boot/glue-chez.scm)
  - [`src/Scheme/Chez/convert-to-r6rs.scm`](../../src/Scheme/Chez/convert-to-r6rs.scm)
  - [`src/Scheme/Chez/compile-texmacs.scm`](../../src/Scheme/Chez/compile-texmacs.scm)
- **Compiler Root-Cause Discovery (`(module ...)` vs. R6RS `(library ...)` in Chez Scheme `cp0`)**:
  - [`docs/chez/bench/CONVERSIONS_PROFILING.md` §3.5](bench/CONVERSIONS_PROFILING.md) observed that Chez Scheme did not inline `==` or other imported helpers across separately compiled `.so` files even at `optimize-level 2`, attributing this to separate compilation. Inspecting Chez's compiler internals reveals the actual mechanism:
    - [`src/Scheme/Chez/convert-to-r6rs.scm:586-593`](../../src/Scheme/Chez/convert-to-r6rs.scm#L586-L593) currently emits Chez top-level `(module <mod-sym> () ...)` forms, and [`TeXmacs/progs/kernel/boot/boot-chez.scm`](../../TeXmacs/progs/kernel/boot/boot-chez.scm) is a plain top-level file.
    - In Chez Scheme (`ChezScheme/s/syntax.ss`), top-level `(module ...)` exports compile as mutable top-level value cells (`(#3%$set-top-level-value! '#{sym ...} (lambda ...))`). Because top-level bindings in `(interaction-environment)` are not guaranteed immutable, Chez **never** attaches `cte-optimization-loc` boxes to `(module ...)` exports (even intra-module calls between two `define-public` procedures in the same `(module ...)` compile to indirect `(#2%$top-level-value '#{sym ...})` lookups).
    - In contrast, for R6RS `(library ...)` forms (`ChezScheme/s/syntax.ss:844-858`, `build-library-body` + `ChezScheme/s/cp0.ss:5801-5846`), R6RS export immutability lets Chez compile the entire library body inside one lexical `letrec*` and attach `(cte-optimization-loc box ...)` to every exported variable.
    - With `(enable-cross-library-optimization)` active (`#t` by default), `compile-file` serializes integrable `case-lambda` ASTs (`$install-library-clo-info`) directly into the `.so` FASL's `library/ct-info` record.
    - Any subsequent old-style `(module ...)` that imports that `(library ...)` in a separate `compile-file` pass reads the serialized `cte-info` and **inlines exported procedures at call sites** (even at default `optimize-level 0`), while keeping first-class procedure values for higher-order uses (`map`, `apply`) and propagating single-valued metadata (`cte-info-procedure-single-valued?`) and unchecked library-global references (`#3%$top-level-value`) for larger non-inlined procedures.
- **Empirical Verification (`expand/optimize` in a fresh Chez 10.4.1 process loading a compiled `.so` library)**:
  - `(delq! x l)` $\to$ `(#2%remq! x l)`
  - `(acons k v l)` $\to$ `(#2%cons (#2%cons k v) l)`
  - `(assoc-ref alist k)` $\to$ `(let ((t (#2%assoc k alist))) (if t (#2%cdr t) #f))`
  - `(string-null? s)` $\to$ `(#2%string=? s "")`
  - `(hash-set! t k v)` $\to$ `(#2%hashtable-set! t k v)`
  - `(hash-remove! t k)` $\to$ `(#2%hashtable-delete! t k)`
  - `(defined? x)` $\to$ `(#2%top-level-bound? x)`
  - `(noop x)` $\to$ `x`
  - `(keyword->symbol k)`, `(keyword->string k)`, `(ca*adr x)` $\to$ inlined directly to primitives.
- **Stage 1 (DONE — 2026-11-19): Make `chez-compat.scm` an Auto-Imported R6RS Library (`(library (kernel boot chez-compat) ...)`)**:
  - *Why `chez-compat` first*:
    - 100% Chez-specific file with 0 `:use` dependencies; compiled first in Tier 1 (`compile-inventory.scm:40`).
    - **0 downstream definition/overload collisions** (and only one downstream `set!`, handled by keeping `procedure-name` in `boot-chez.scm`): Full-tree audit across all 453 modules via `compiler-scan.scm` confirms **0** of `chez-compat.scm`'s exports are redefined or overloaded by `define-public` or `tm-define` in any downstream module, and only `procedure-name` was mutated via `set!` (`tm-define.scm:95`).
  - *Implemented Changes*:
    1. **`TeXmacs/progs/kernel/boot/chez-compat.scm` & `TeXmacs/progs/kernel/boot/boot-chez.scm`**:
       - Kept `(texmacs-module (kernel boot chez-compat))` and all `define-public` / `define-public-macro` definitions (so no manual `(export ...)` list is required in source and `compiler-scan.scm` needs 0 changes), and placed `(import (except (chezscheme) ...) (rename ...))` plus local `define-public` / `define-public-macro` `syntax-rules`/`syntax-case` shims at [`chez-compat.scm:10-36`](../../TeXmacs/progs/kernel/boot/chez-compat.scm#L10-L36).
       - **Moved `procedure-name` stub to `boot-chez.scm:700`**: `kernel/texmacs/tm-define.scm:94-98` mutates `procedure-name` via top-level `(set! procedure-name ...)`, which fails on immutable R6RS library exports (`Exception: attempt to assign immutable variable procedure-name`). Moving `(define (procedure-name p) #f)` to `boot-chez.scm` (alongside `has-look-and-feel?` and `quit-TeXmacs-scheme`) keeps it mutable in `(interaction-environment)`.
       - **4 `(chezscheme)` builtins overridden by `chez-compat`**: `sort`, `current-time`, `random`, `record-accessor`. Excluded in the library import clause via `(except (chezscheme) sort current-time random record-accessor)` and rebound via `(rename (only (chezscheme) sort current-time random record-accessor display write) ...)` instead of `top-level-value` lookups.
       - **External references inside `chez-compat.scm`**:
         - `keyword->string`, `keyword->number`, and `number->keyword` operate directly on `symbol->string` / `string->symbol` instead of calling `boot-chez.scm`'s `keyword->symbol` / `symbol->keyword`.
         - `display*` routes via `(top-level-value 'display)` so 1-arg `display*` continues to flow through `boot-chez.scm`'s `tm-output` hook.
         - `server-mode?` calls `((top-level-value 'server-started?))`, and `on-exit` wraps `(top-level-value 'quit-TeXmacs-scheme)` via `set-top-level-value!`.
       - **`hash-ref` `case-lambda` optimization (`chez-compat.scm:396-402`)**:
         Replaced variadic `(define-public (hash-ref t k . default) ...)` with `case-lambda`, letting 2-argument `(hash-ref t k)` inline down to `(#2%hashtable-ref t k #f)` (one lookup, zero rest-list consing).
    2. **`src/Scheme/Chez/convert-to-r6rs.scm`**:
       - In `write-module-file` (`convert-to-r6rs.scm:590-601`), `kernel/boot/chez-compat.scm` emits `(library (kernel boot chez-compat) ,@exports ,@definitions ,@expressions)` followed immediately by `(module kernel:boot:chez-compat () (import (kernel boot chez-compat)) (export (import (kernel boot chez-compat))))` (since `import`, `define-syntax`, `define-public`, and `define-public-macro` are already placed into `definitions` by `prepare-forms` + `partition def-form?`). Because `(export (import (kernel boot chez-compat)))` re-exports the library bindings directly, Chez preserves `cte-optimization-loc` cross-library inlining metadata for callers importing `kernel:boot:chez-compat` without changing `boot-chez.scm`, `init-texmacs-chez.scm`, `init-texmacs-boot-file.scm`, `compile-texmacs.scm`, or `texmacs-user.scm`.
       - In `module-link-forms` (`convert-to-r6rs.scm:560-562`), `(kernel boot chez-compat)` is auto-imported via `use-module!` for all subsequent modules, automatically excluding colliding local private definitions via `(except kernel:boot:chez-compat ,@clash)` (e.g. in `tm-define.scm`, `math-edit.scm`, `graphics-markup.scm`, `db-users.scm`, `menu-convert.scm`).
  - *Verification (2026-11-19)*:
    - All 5 compiler unit test suites in `src/Scheme/Chez/*-test.scm`: **206 checks, 0 failures** (50 + 29 + 17 + 41 + 69).
    - Full clean `-recompile-modules`: **453/453 modules recompiled, 0 failures** (`init-texmacs-boot-file.so` verified).
    - Cross-library `cp0` inlining verified via `expand/optimize` loading `build-texmacos/boot/kernel/boot/chez-compat.so`: `string-null?`, 2-arg `hash-ref`, `hash-set!`, `delq!`, `acons`, and `keyword->string` inline directly to `#2%string=?`, `#2%hashtable-ref`, `#2%hashtable-set!`, `#2%remq!`, `#2%cons`, and `#2%symbol->string` / `#2%substring` / `#3%string-length`.
    - Rebuilt VFASL `texmacs.boot` via `make-boot.scm` (`17.00 MB`, 178/178 modules) and verified headless startup (`-H -x`) in both Fast Boot (`TIME-SERVER-INIT: 490 ms`) and `TEXMACS_NO_BOOT=1` Normal `.so` Init (`TIME-SERVER-INIT: 1398 ms`, `TIME-PRE-GUI-LOOP: 1497 ms`), exercising `keyword->string`, `keyword->number`, `number->keyword`, `display*`, `display-to-string`, `hash-ref`, `receive`, `on-entry`, `on-exit`, and `tm-define`'s `procedure-name` override.
- **Stage 2: Turn `boot-chez.scm` + `glue-chez.scm` into `(library (kernel boot boot-chez) ...)` + Thin Top-Level Bootstrap**:
  - *Benefit*:
    - Currently, all **732 transcoded C++ glue procedures** in `glue-chez.scm` emit a top-level `(define c-glue-foo (foreign-procedure ...))` and `(define (foo a1) (recur-utf8->string (c-glue-foo (recur-string->utf8 a1))))`. At top level, Chez cannot inline `c-glue-foo` into `foo`, incurring 3 mutable top-level lookups (`recur-string->utf8`, `c-glue-foo`, `recur-utf8->string`) and an extra closure call per FFI invocation.
    - Inside `(library (kernel boot boot-chez) ...)`, `c-glue-*` are unexported lexical `letrec*` bindings: Chez inlines the `$foreign-procedure` call directly into `foo` (eliminating 732 closures and 732 global symbols from `texmacs.boot`) and compiles `recur-string->utf8` / `recur-utf8->string` as direct known-procedure calls (~20% faster wrapper execution in microbenchmarks: `132.8 ms` $\to$ `106.9 ms` for 20M calls).
    - Replacing `(define chez:symbol? (top-level-value 'symbol? (scheme-environment)))` with `(import (rename (only (chezscheme) symbol?) (symbol? chez:symbol?)))` allows `cp0` to recognize `chez:symbol?` as a primitive inside `symbol?` and `keyword?`.
  - *Constraints & Exclusions (8 symbols must remain mutable top-level bindings in `(interaction-environment)`)*:
    1. **2 `set!` targets**: `has-look-and-feel?` (mutated in `tm-preferences.scm:174`) and `quit-TeXmacs-scheme` (mutated by `on-exit`).
    2. **4 glue procedures overloaded via `tm-define`**: `select-all` (`generic/generic-edit.scm`), `url-last-modified`, `url-newer?`, `buffer-last-save` (`texmacs/texmacs/tm-files.scm:23-39`).
    3. **2 `boot-chez` procedures redefined downstream**: `promise-source` (`kernel/gui/menu-widget.scm`) and `module-dependencies` (`doc/apidoc-funcs.scm`).
    4. **Top-level re-import**: The thin top-level `boot-chez.scm` loader must still run `(import (kernel boot boot-chez))` into `(interaction-environment)` before calling `chez_cache_runtime_procedures` so C++ `Stop_level_value` (`chez_tm.cpp:779-832`) and dynamic `(eval ... tm-user-env)` resolve all runtime bindings.
- **Stage 3: Extend Auto-Imported `(library ...)` Emission to Foundational Acyclic Tier-1 Kernel Modules (`abbrevs.scm`, `ahash-table.scm`, `base.scm`, `list.scm`)**:
  - As measured in [`CONVERSIONS_PROFILING.md` §3.2–§3.5](bench/CONVERSIONS_PROFILING.md), the highest-frequency small wrappers across TeXmacs live in:
    - `kernel/boot/abbrevs.scm`: `==` (1.88M calls in LaTeX export), `nlist?` (192k), `list>0?` (183k), `in?` (137k), `!=` (74k in HTML export), `nnull?`, `npair?`, `safe-car`, `safe-cdr`.
    - `kernel/boot/ahash-table.scm`: `ahash-ref` (82k+ calls in LaTeX export), `ahash-set!`, `ahash-remove!`.
    - `kernel/library/base.scm`: `func?` (660k calls in LaTeX export + 302k calls in HTML export).
  - Emitting these acyclic Tier-1 modules as `(library ...)` (plus the 2-line `(module <flat-sym> () (import (<lib-spec>)) (export (import (<lib-spec>))))` alias) and auto-importing them into downstream module headers enables `cp0` to inline `==`, `!=`, `nnull?`, `npair?`, `safe-car`, `safe-cdr`, `ahash-ref`, `ahash-set!`, and `func?` system-wide while keeping pristine upstream sources untouched.

### [ ] Refactor `define-table` and `define-collection` (Post Pure-Expression `tm-define` Simplification)
- **Location**: [`src/Scheme/Chez/convert-to-r6rs.scm:392-393`](src/Scheme/Chez/convert-to-r6rs.scm#L392-L393), [`src/Scheme/Chez/compiler-helper.scm:216-245`](src/Scheme/Chez/compiler-helper.scm#L216-L245), [`TeXmacs/progs/kernel/boot/ahash-table.scm`](TeXmacs/progs/kernel/boot/ahash-table.scm)
- **Problem**: `define-table` and `define-collection` are still special-cased in `prepare-forms` and `eval-eager-form!` to avoid Chez Scheme multiple-definition errors during forward declarations (`(define-table style-synopsis)`) and ensure tables are exported to `texmacs-user-kernel`.
- **Target Solution**: Migrate `define-table` and `define-collection` to standard macro definitions emitting top-level table registrations or definitions and remove the ad-hoc compiler desugaring.

### [ ] Quick Hack: Implicit Cross-Module Macro Propagation via `(interaction-environment)` for `tm-define-macro`
- **Location**: [`TeXmacs/progs/kernel/texmacs/tm-define.scm:294-303`](TeXmacs/progs/kernel/texmacs/tm-define.scm#L294-L303), [`src/Scheme/Chez/convert-to-r6rs.scm:394`](src/Scheme/Chez/convert-to-r6rs.scm#L394), [`src/Scheme/Chez/compiler-helper.scm:219`](src/Scheme/Chez/compiler-helper.scm#L219)
- **Problem**: In upstream GNU Guile TeXmacs, every module implicitly inherited `texmacs-user`. Macros defined via `tm-define-macro` anywhere in the codebase (e.g. `smart-table` in `utils/library/smart-table.scm`) were automatically available as global syntax in downstream modules without explicit `(:use ...)` declarations (such as `convert/latex/latex-define.scm`).
- **Current Mechanism**:
  - `tm-define-macro` emits `(define-top-level-syntax ',var ... (interaction-environment))`.
  - Because `compile-texmacs.scm` compiles in `(interaction-environment)` and `compiler-helper.scm` evaluates `define-top-level-syntax` forms during eager eval, downstream modules implicitly see all previously compiled `tm-define-macro` transformers.
- **Side Effect**: Potential duplicate syntax definitions and global namespace pollution across compilation units (one reason isolated worker compilation needed retry passes in `PARALLEL_COMPILER_PLAN.md` §9.6).
- **Target Clean Solution**:
  - Automatically infer `tm-define-macro` dependencies during AST scanning and emit explicit `(import (for <declaring-module> run expand))`, or formalize an aggregated syntax library encapsulating public `tm-define-macro` transformers.

### [ ] Module System Modernization via Explicit Side-Effect Wrapping (`%module-side-effect`)
- **Location**: [`src/Scheme/Chez/convert-to-r6rs.scm:335-345`](src/Scheme/Chez/convert-to-r6rs.scm#L335-L345), [`TeXmacs/progs/kernel/texmacs/tm-modes.scm:36-50`](TeXmacs/progs/kernel/texmacs/tm-modes.scm#L36-L50)
- **Context**: Upstream GNU TeXmacs used dummy definitions `(define _tm_def_XX (begin ... (void)))` (via `fresh-tm-dummy` in `tm-modes.scm`) to smuggle runtime side-effects (`set-symbol-procedure!`, `logic-rules`) into macro outputs intended for definition contexts.
- **Proposed Architecture**:
  - Replace artificial `_tm_def_XX` dummy bindings with an explicit compiler marker form `(%module-side-effect <expr> ...)`, letting the transpiler route genuine definitions into `defs` and `%module-side-effect` forms into `exprs`.

---

## 3. Packaging & Build Maintenance

### [ ] Packaging Optimization: Prune Unused Qt Plugins in macOS Bundle
- **Location**: [`packages/macos/make-bundle.sh`](packages/macos/make-bundle.sh#L164-L189) (`macdeployqt` and `bundle_closure`)
- **Problem**:
  `macdeployqt` pessimistically deploys all plugins available in the host Qt installation (`sqldrivers`, `virtualkeyboard`, `networkinformation`, `tls`, `generic`).
  Beyond the plugin `.dylib`s themselves, these plugins introduce heavy transitive dependencies:
  - `sqldrivers/libqsqlite.dylib` pulls in the entire `QtSql.framework`.
  - `tls/libqopensslbackend.dylib` pulls in Homebrew OpenSSL (`libssl.*.dylib` and `libcrypto.*.dylib`, ~12 MB).
  Each unnecessary binary forces `bundle_closure` to run transitive dependency resolution, `install_name_tool` patching, and code signing, adding 15–25 MB of bloat and substantial packaging latency.
- **Action Plan**:
  1. Either invoke `macdeployqt "$APP" -no-plugins` and selectively copy only the required plugins:
     - `platforms/libqcocoa.dylib`
     - `styles/libqmacstyle.dylib`
     - `imageformats/` (PNG, JPEG, SVG)
     - `iconengines/` (SVG icons)
  2. Or prune unused directories (`sqldrivers`, `virtualkeyboard`, `networkinformation`, `tls/libqopensslbackend.dylib`) immediately after `macdeployqt` runs, before `bundle_closure` begins.
- **Expected Benefits**:
  - Drops deployment size by **15–25 MB** (avoids shipping `QtSql.framework` and OpenSSL).
  - Skips dependency resolution and code-signing on dozens of unused dylibs, significantly speeding up `make-bundle.sh --bundle-qt`.

---

## 4. Performance, FFI & Conversion Optimization

### [ ] Downstream String Concatenation & Comparison in HTML/LaTeX Exporters
- **Location**: `TeXmacs/progs/convert/html/`, `TeXmacs/progs/convert/latex/` (see [`docs/chez/bench/CONVERSIONS_PROFILING.md`](docs/chez/bench/CONVERSIONS_PROFILING.md) §3–§4)
- **Empirical Finding on `symbol->string`**:
  Calling `(symbol->string sym)` in Chez Scheme allocates **0 bytes of heap memory** (verified via `1,000,000` calls allocating 0 bytes). In Chez Scheme, symbols store an immutable string pointer to their print-name; `symbol->string` is an $O(1)$ single-instruction field dereference.
- **Where the Real Churn Lies**:
  The GC churn comes from what is done *with* the string immediately after conversion:
  1. `(string-append "<" (symbol->string s) ">")` in `htmlout.scm` (called 57,000+ times per HTML export), repeatedly allocating temporary tag strings.
  2. `(string-starts? (symbol->string (car t)) "tmmath")` in `tmtex.scm`, converting symbols to strings just for prefix checks.
  3. Per-character `tmtex-math-mode?` / `env?` scans and `string->list` in `tmtex-text-list` (largest allocation source in LaTeX export: 156.7 MB warm).
- **Optimization**:
  1. Prefer direct symbol `eq?` or `memq` checks (e.g. `(eq? tag 'td)`) rather than converting symbols to strings for comparisons.
  2. Pre-format common HTML element open/close tag strings (`'td` $\to$ `"<td"`, `"</td>"`), eliminating dynamic `string-append` in inner serialization loops.
  3. Apply the targeted `tmtex` / `tmhtml` wins identified in `CONVERSIONS_PROFILING.md` §4 (hoist `tmtex-math-mode?`, tag-set lookup in `latex-is-math?`, avoid `string->list` in `tmtex-text-list`, cache HTML title/attr scans).

### [ ] Typesetter `<extern|...>` AST Execution: Replace Dynamic `eval` with Cached Procedure Calls
- **Location**: [`src/Typeset/Env/env_exec.cpp:49-65`](src/Typeset/Env/env_exec.cpp#L49-L65)
- **Problem**: During typesetting of documents with dynamic tags, menus, or macros, `<extern|fun|arg1|...>` builds a cons cell `(cons (string_to_object fun) args)` and calls `eval(expr)`. In Chez Scheme, calls with arguments fall past `boot-chez.scm`'s 0-arg `eval` fast-path and invoke the native compiler on every dynamic tag evaluation.
- **Optimization**: Look up the Scheme procedure `fun` by symbol and invoke it directly with `call(fun, args)` / `call_scheme(proc, args)`.

### [ ] Optimize Blackbox Object Allocation & Guardian Tracking
- **Location**: [`src/Scheme/Chez/chez_tm.cpp:228-243`](src/Scheme/Chez/chez_tm.cpp#L228-L243)
- **Problem**: Every C++ object (`tree`, `widget`, `command`, `url`) passed into Scheme triggers `tm_new<blackbox>(b)` and `Scall1 (tm_make_blackbox, Sinteger ((iptr) p))`, allocating a `tm-blackbox` record and registering it with `tm_blackbox_guardian`. Reclaiming requires running `reap_blackboxes` / `reap-blackboxes!` during GC (`collect-request-handler`).
- **Optimization**: Investigate pooling/recycling C++ `blackbox` wrappers or reducing C-to-Scheme callback overhead per blackbox creation to lower allocator and GC pressure.

### [ ] Further Dynamic Evaluation (`eval`) & Menu Generation Optimizations
- **Location**: [`TeXmacs/progs/kernel/gui/menu-widget.scm:949-958`](TeXmacs/progs/kernel/gui/menu-widget.scm#L949-L958), [`TeXmacs/progs/generic/generic-menu.scm`](TeXmacs/progs/generic/generic-menu.scm), [`TeXmacs/progs/kernel/boot/boot-chez.scm:112-151`](TeXmacs/progs/kernel/boot/boot-chez.scm#L112-L151)
- **Opportunities Identified**:
  1. **Unary Call Fast-Path for Pure Functions**:
     - In dynamic menus (e.g. `generic-menu.scm`), forms like `(eval (upcase-first "file"))` or `(eval (pull-focus t "name"))` are common.
     - Extend direct invocation in `eval` to single-argument calls where the function is a bound procedure and the argument is a literal (string/number/boolean/symbol), avoiding Chez expander dispatch.
  2. **Direct Procedure Caching at Caller Sites (`menu-widget.scm`)**:
     - In `menu-widget.scm`: `menu-expand-link` executes `((eval (cadr p)))`.
     - Because `menu-widget.scm` is compiled into `menu-widget.so`, checking `(cond ((procedure? target) target) ((and (symbol? target) (top-level-bound? target)) (top-level-value target)) (else (eval target)))` allows the compiled native code of `menu-widget.so` to resolve the procedure directly without a dynamic Scheme call to `eval`.
  3. **Dynamic Menu Tree Memoization**:
     - Menus like `(texmacs-menu)` return static trees that are re-expanded repeatedly when traversing submenus.
     - Adding short-lived caching or hash-consing for static submenus during a single menu-bar refresh cycle avoids re-expanding unchanged subtrees on every key press.

### [ ] Low-Intrusion Top Bar Menu Initialization & Expansion Optimizations
- **Locations**:
  - [`TeXmacs/progs/kernel/gui/kbd-define.scm:39-60,142-147`](TeXmacs/progs/kernel/gui/kbd-define.scm#L39-L60)
  - [`TeXmacs/progs/kernel/texmacs/tm-modes.scm:59-62`](TeXmacs/progs/kernel/texmacs/tm-modes.scm#L59-L62)
  - [`TeXmacs/progs/init-texmacs-chez.scm:333,381`](TeXmacs/progs/init-texmacs-chez.scm#L333)
  - [`TeXmacs/progs/kernel/gui/menu-widget.scm:175-186,454-498,805-825,1078-1082`](TeXmacs/progs/kernel/gui/menu-widget.scm#L454-L498)
  - [`TeXmacs/progs/texmacs/menus/tools-menu.scm:62`](TeXmacs/progs/texmacs/menus/tools-menu.scm#L62)
  - [`src/Texmacs/Window/tm_window.cpp:386-462`](src/Texmacs/Window/tm_window.cpp#L386-L462)
  - [`src/Plugins/Qt/qt_tm_widget.cpp:874-942`](src/Plugins/Qt/qt_tm_widget.cpp#L874-L942)
  - [`src/Plugins/Qt/QTMMenuHelper.cpp:56-87,385-404`](src/Plugins/Qt/QTMMenuHelper.cpp#L56-L87)
- **Measured Bottlenecks** (`build-texmacos/TeXmacs.app`, Chez + Qt6):
  - **Per-item `lazy-keyboard-force` exception churn**: Until the 250 ms idle timer drains `lazy-keyboard-waiting`, every menu entry calls `kbd-find-inv-binding` $\to$ `(lazy-keyboard-force)`, taking **~1.1 ms and 37,280 bytes per item** (`view-menu`: **5.11 ms / 982 KB** before forcing vs **0.62 ms / 57 KB** after; `edit-menu`: **2.42 ms / 407 KB** vs **0.26 ms / 35 KB**). Root cause: `init-texmacs*.scm` registers `in-active-graphics?`, `graphics-wheel`, and `developer-mode?` with `lazy-keyboard` without `symbol-procedure` entries on `in-active-graphics%` / `graphics-whee%` / `developer-mode%`, so `texmacs-in-mode?` falls through to `(catch #t (lambda () (eval (list mode))) ...)`, invoking Chez's compiler and throwing an unbound-variable exception 3× per menu item (**35,760 bytes**).
  - **Warm `update-menus` overhead (~13.6 ms / 254 KB per cache-hit pass)**: `tm_window_rep::get_menu_widget` calls `eval("'" * menu)` (`eval_scheme` string parse + eval) on every refresh and **twice** on a cache miss (`tm_window.cpp:392,412`), while `menu_main` / `menu_icons` / `side_tools` / `bottom_tools` invoke `eval("(lazy-initialize-force)")` **9 times** per refresh.
- **Low-Intrusion Action Plan**:
  1. **`lazy-keyboard-force` & `texmacs-in-mode?` (highest ROI)**:
     - Guard `texmacs-in-mode?` (`tm-modes.scm:59-62`) with `(top-level-bound? mode)` (or cache missing predicates) before falling back to `eval`.
     - **[x] DONE (2026-11-19)**: Fixed the 3 `lazy-keyboard` registrations across `init-texmacs*.scm` (`in-graphics?` + `(lazy-define (graphics graphics-kbd) graphics-wheel)`) and registered `developer-mode%` / `in-graphics%` in `tm-modes.scm:312-320`. Verified (`453/453` modules, `-H` exit 0): `kbd-find-inv-binding` **1.39 ms / 37,280 B** $\to$ **0.19 ms / 1,696 B** (22× less allocation); `view-menu` **982 KB** $\to$ **95 KB**; `edit-menu` **407 KB** $\to$ **51 KB**.
     - In `lazy-keyboard-force` (`kbd-define.scm:49-60`), early-return when `(null? lazy-keyboard-waiting)` and coalesce so `make-menu-main` (`menu-widget.scm:1183`) runs it once per menu build instead of once per entry.
  2. **C++ menu string & lazy-init dispatch (`tm_window.cpp`)**:
     - Cache parsed `string -> object` menu descriptors in a static `hashmap<string, object>` and reuse the local `umenu` on cache miss instead of calling `eval("'" * menu)` twice.
     - Replace the 9× `eval("(lazy-initialize-force)")` calls in `tm_window.cpp:426-456` with `call("lazy-initialize-force")` (or run it once per `resume()` / `update_menus()`).
  3. **Per-item Scheme trim (`menu-widget.scm`, `tools-menu.scm`)**:
     - Compute `(promise-source action)` once in `make-menu-entry-sub` (`menu-widget.scm:470-498`) instead of 5× (`style`, `check`, `dots`, `shortcut`, `search-balloon-help`).
     - Remove or guard the unconditional `(match? p '(input ...))` check at `menu-widget.scm:809` (already dispatched via `make-menu-items-table` at line 848).
     - Cache `(and (qt-gui?) (os-macos?) English?)` in `adjust-translation` (`menu-widget.scm:175-186`) and use quoted constant lists to avoid per-label C++ `get-preference` FFI calls and list allocations.
     - Replace `(list? x)` with `(pair? x)` in `replace-procedures` (`menu-widget.scm:1078-1082`) to eliminate $O(N^2)$ list-length walks during `menu-expand`.
     - Memoize `(url-exists-in-path? "pdflatex")` in `tools-menu.scm:62` to avoid `$PATH` stats on every `Tools` menu expansion.
  4. **Qt top-bar installation & lazy menu expansion (`Plugins/Qt`)**:
     - In `qt_tm_widget_rep::install_main_menu()` (`qt_tm_widget.cpp:892-938`): pass `Qt::UniqueConnection` on `QMenu::aboutToShow` / `aboutToHide` (prevents stacked duplicate signals when reinstalling cached `main_menu_widget`), guard `addAction("native menubar trick")` with `if (a->menu()->isEmpty())`, and wrap the `QTMToolbar` loop in `setUpdatesEnabled(false)` / `true` (or reuse `dest->replaceActions(src)`).
     - In `QTMAction` (`QTMMenuHelper.cpp:56-83`): allocate `_timer` lazily in `showToolTip()` (only used by `balloon_widget` items) and share a `static const QFont`.
     - In `QTMLazyMenu::transferActions` (`QTMMenuHelper.cpp:385-396`): bracket with `setUpdatesEnabled(false)` / `true` and `deleteLater()` parentless removed actions to stop leaking prior `QTMAction` trees on repeated menu expansions.

### [ ] Forward Cursor-Pair Path for Long Arrays (`n >= 8`) in `chez_array_to_tmscm`
- **Priority**: Low (Optional Micro-Optimization)
- **Location**: [`src/Scheme/Scheme/glue.cpp:422-438`](src/Scheme/Scheme/glue.cpp#L422-L438)
- **Problem**:
  - Because `chez_root_stack_vec` lives in gen >= 1 after boot, each `acc.set (tail)` in the backward loop hits Chez's `S_dirty_set` write barrier (`alloc_mutex` acquisition).
- **Optimization**:
  - For long arrays (`n >= 8`), build forward using a single rooted gen-0 cursor pair `(head . tail)` so `Sset_cdr` updates stay in gen-0 (skipping `alloc_mutex`), reducing mutex acquisitions from `n - 1` to `1`.
  - Minor impact on typical TeXmacs AST trees (average arity is ~2.65); mainly benefits large flat arrays/tables.

### [ ] Avoid Double Sublist Walk in `tmscm_to_scheme_tree`
- **Priority**: Low (Optional Micro-Optimization)
- **Location**: [`src/Scheme/Scheme/glue.cpp:503-520`](src/Scheme/Scheme/glue.cpp#L503-L520)
- **Problem**:
  - `tmscm_to_scheme_tree` calls `tmscm_is_list (p)` (which invokes Chez `list?` and walks the entire sublist spine) before walking the exact same pairs a second time to convert children.
- **Optimization**:
  - Check `tmscm_is_pair (p)` (and `'()` explicitly) and validate proper list termination in a single pass, trimming overhead from `stree->tree` (`c-glue-stree_2tree`, ~7.6–8.0 ms in `conversions.scm`).
