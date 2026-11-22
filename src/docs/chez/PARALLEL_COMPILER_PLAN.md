# Multi-Threaded / Multi-Process Compiler Support for TeXmacs / Chez Scheme

**Date:** 2026-11-12  
**Platform:** Apple Silicon (macOS arm64), Chez Scheme 10.4.1 (threaded), Qt 6.12.0  
**Target:** 453 TeXmacs Scheme Modules (`TeXmacs/progs/`)  
**Status:** Architecture specification. The two-pass split (§3.1/§3.2, Milestone 1) was
implemented and verified. The worker pool (Milestones 2–3) was implemented, measured, and
reverted: it was slower than the sequential build and could not reproduce the sequential
compilation environment. §9 records what was measured, what in this document turned out to
be wrong, and what to fix first if the work is resumed.

---

## 1. Executive Summary

The TeXmacs ahead-of-time (AOT) Scheme compiler ([`src/Scheme/Chez/compile-texmacs.scm`](file:///Users/king-kong/Documents/GitHub/texmacs/src/Scheme/Chez/compile-texmacs.scm)) compiles all 453 Scheme modules into native Chez Scheme shared objects (`.so`) in **~22.8 seconds** sequentially on a single thread.

Nanosecond-precision profiling reveals that **82.8% of build time (18.88 seconds)** is spent purely inside Chez Scheme's `compile-file` primitive (nanopass optimization passes, register allocation, and native ARM64 machine code generation). 

Crucially, experimental verification reveals that **eager evaluation (`eval-eager-forms!`) is unnecessary in the first phase**. When eager evaluation is removed:
- **Phase 1 (Pure Syntactic Transpilation & Concrete DAG Generation)** executes in **< 0.50 seconds** (reading forms, docstring stripping, uncurrying, evaluating bootstrap platform conditionals like `os-macos?`, partitioning definitions vs expressions, emitting Chez Scheme `(module ...)` wrappers, and computing the dependency graph).
- **Phase 2 (Parallel Native Code Generation)** can be distributed across a pool of **isolated worker processes**, each maintaining its own private working set / `(interaction-environment)`. Workers load only the prerequisite `.so` binaries for their unit and run `compile-file`.

> **Correction (§9.2).** The claim that eager evaluation is unnecessary is wrong. Removing
> `eval-eager-forms!` breaks 8 kernel modules. It is load-bearing but *relocatable*: moving
> it from transpilation into Pass 2, immediately before `compile-file`, is what the split
> actually does — and the 2.4 s then parallelises instead of disappearing.

### Projected Performance Impact
On an 8-core Apple Silicon CPU, clean build time is projected to drop from **22.8 seconds to ~3.5 – 4.5 seconds**—a **5× to 6× end-to-end speedup**—while maintaining 100% adherence to all project invariants (clean upstream sources, hermetic environments, and deterministic AOT serialization).

---

## 2. Empirical Benchmark Profiling

Measurements taken on the active tree using the nanosecond-precision profiling harness in [`docs/chez/bench/compile-bench.scm`](file:///Users/king-kong/Documents/GitHub/texmacs/docs/chez/bench/compile-bench.scm) (`TEXMACS_RECOMPILE_ALL=1`, full clean rebuild of 453 modules):

```
========================================================================
           COMPILER DETAILED PHASE BENCHMARK BREAKDOWN                 
========================================================================
  Total Measured Wall Time: 22.813 s
  Total Modules: 453 (Recompiled: 453, Cached: 0, Failures: 0)
------------------------------------------------------------------------
Phase                              Time        % of Wall
------------------------------------------------------------------------
PRESCAN & GRAPH SETUP:             0.284 s      1.2%    
  - scan-source-files->db          0.130 s      0.6%    
  - make-compile-graph             0.001 s      0.0%    
  - compute-dirty-modules          0.001 s      0.0%    
  - oracle-index-tree              0.152 s      0.7%    
------------------------------------------------------------------------
TRANSPILATION (453 modules):       2.872 s     12.6%   
  - read forms (read-all-forms)    0.172 s      0.8%    
  - bind keywords                  0.010 s      0.0%    
  - prepare & rewrite AST          0.024 s      0.1%    
  - deps & module link forms       0.004 s      0.0%    
  - eager eval + macro closure     2.399 s     10.5%   (Unnecessary in Phase 1!)
  - emit transpiled .scm           0.264 s      1.2%    
------------------------------------------------------------------------
COMPILATION (compile-file -> .so): 18.880 s    82.8%   (Primary Amdahl target)
  - ephemeral unbind / cleanup     0.006 s      0.0%    
------------------------------------------------------------------------
LOAD & EXPORT REGISTRATION:        0.726 s      3.2%    
  - load .so into environment      0.722 s      3.2%    
  - import kernel exports          0.004 s      0.0%    
------------------------------------------------------------------------
POST-BUILD & SNAPSHOT:             0.040 s      0.2%    
  - texmacs-user.so kernel snapshot0.035 s      0.2%    
  - save module manifest           0.001 s      0.0%    
  - write compiler db              0.004 s      0.0%    
========================================================================
Sum of Accounted Phases:           22.808 s    100.0%  
========================================================================
```

### 2.1 Key Insights from Profiling
1. **The Core Bottleneck is Code Generation**: `compile-file` consumes 82.8% of build time. Parallelizing code generation across CPU cores directly unlocks Amdahl's Law.
2. **Eager Evaluation Was an Artifact of Sequential In-Process Compilation**: In the sequential compiler, `eval-eager-forms!` (2.399 s) was used to dynamically register macro transformers into the compiler's own single environment. Because Chez Scheme compiles modules against imported `.so` files, AST translation does not require eager evaluation.
3. **Pure Syntactic Translation is Blazing Fast**:
   $$\text{Read Forms } (0.172\,\text{s}) + \text{Rewrite AST } (0.024\,\text{s}) + \text{Emit .scm } (0.264\,\text{s}) = \mathbf{0.460\,\text{s}}$$
   Running Phase 1 sequentially across all 453 modules takes less than half a second.

### 2.2 Top 10 Slowest Modules in `compile-file`

| Module | Native `compile-file` Time | Subsystem |
|---|---|---|
| `convert/latex/tmtex.scm` | 0.675 s | LaTeX export compiler |
| `dynamic/dynamic-drd.scm` | 0.611 s | Dynamic session grammar |
| `math/math-kbd.scm` | 0.456 s | Math keyboard shortcuts |
| `generic/generic-edit.scm` | 0.436 s | Core editor routines |
| `kernel/gui/gui-markup.scm` | 0.384 s | GUI widget markup |
| `education/edu-drd.scm` | 0.338 s | Educational document DRD |
| `text/text-drd.scm` | 0.261 s | Text document DRD |
| `convert/latex/latex-define.scm`| 0.250 s | LaTeX translation tables |
| `client/client-widgets.scm` | 0.249 s | Client UI widgets |
| `text/text-edit.scm` | 0.237 s | Text editing handlers |

---

## 3. Two-Pass Decoupled Architecture

```
                            PRISTINE SOURCE TREE
                            (TeXmacs/progs/*.scm)
                                      │
                                      ▼
       ┌─────────────────────────────────────────────────────────────┐
       │         PASS 1: SYNTACTIC TRANSPILER (SINGLE-THREADED)      │
       │   - Reads AST and normalizes definitions (< 0.5 s total)    │
       │   - Unwraps platform/dialect conditionals (boot-chez.scm)   │
       │   - Partitions definitions vs top-level expressions         │
       │   - Generates exact Chez Scheme (module ...) files          │
       │   - Outputs Concrete Prerequisite Dependency DAG            │
       │   - Zero eager evaluation, zero code generation, zero locks │
       └──────────────────────────────┬──────────────────────────────┘
                                      │
                                      ▼
                       Concrete Compilation DAG & Manifest
                      (453 nodes, exact prerequisite edges)
                                      │
                                      ▼
       ┌─────────────────────────────────────────────────────────────┐
       │         PASS 2: MULTI-PROCESS COMPILER POOL (PARALLEL)      │
       │   N independent TeXmacs worker processes (isolated heaps)   │
       │                                                             │
       │   Worker 1               Worker 2            Worker N       │
       │  ┌─────────────────┐   ┌─────────────────┐  ┌─────────────┐ │
       │  │ Private Env     │   │ Private Env     │  │ Private Env │ │
       │  │ Load prereqs    │   │ Load prereqs    │  │Load prereqs │ │
       │  │ compile-file    │   │ compile-file    │  │compile-file │ │
       │  │   modA.so       │   │   modB.so       │  │  modC.so    │ │
       │  └────────┬────────┘   └────────┬────────┘  └──────┬──────┘ │
       │           └─────────────────────┼──────────────────┘        │
       │                                 ▼                           │
       │                 Coordinator Work Queue & Scheduling         │
       │               (Dispatches ready nodes as prereqs finish)    │
       └──────────────────────────────┬──────────────────────────────┘
                                      │
                                      ▼
                         ALL 453 .so MODULES PRODUCED
```

### 3.1 Pass 1: Pure Syntactic Transpilation (< 0.5 s)

Pass 1 operates strictly as a syntactic AST pass with zero side-effects on the running Scheme process:
1. **Read Forms**: `(read-all-forms (progs-path rel-path))`.
2. **AST Normalization (`prepare-forms`)**:
   - Strips docstrings and uncurries multi-argument function declarations.
   - Flattens nested `begin` blocks.
   - Partitions top-level forms into definitions (`tm-define`, `define`, `define-macro`) vs executable expressions.
3. **Top-Level Conditional Resolution (`transpile-conditional`)**:
   - Guile vs Chez conditional forms (`(if (supports-guile?) ...)` or `(if (os-macos?) ...)`) are evaluated against static platform constants already established in [`src/Scheme/Chez/boot-chez.scm`](file:///Users/king-kong/Documents/GitHub/texmacs/src/Scheme/Chez/boot-chez.scm).
4. **Emit Chez Scheme Module Files**:
   - Emits self-contained Chez Scheme `(module ...)` files to `build-texmacos/boot/`:
     ```scheme
     (module <module-id> ()
       (import (for <prereq-1> run expand))
       (import (for <prereq-2> run expand))
       (export ...)
       <definitions ...>
       (init-module-table ...)
       <expressions ...>)
     ```
5. **Concrete Dependency Graph Extraction**:
   - Collects all imported modules (`:use`, `:inherit`, and macro providers).
   - Writes the concrete DAG (`compile-graph.scm`) for Pass 2 scheduling.
   - **No `eval-eager-forms!` or macro evaluation.**

### 3.2 Pass 2: Multi-Process Worker Pool with Isolated Working Sets

#### Why Separate Working Sets Instead of Threads?
> **Confirmed by measurement (§9.3).** `fork-thread`, `make-mutex` and `make-condition` are
> all available, and `parameterize` is thread-local — but `make-environment`,
> `new-interaction-environment` and `make-top-level-environment` are all **unbound**. No
> second mutable top-level environment can be created, so every thread would share one
> `(interaction-environment)`: exactly the hazard described below.

In Chez Scheme, `(interaction-environment)` is a mutable global namespace. While Chez Scheme supports POSIX threads (`fork-thread`), running `compile-file` concurrently across multiple threads in a single shared environment introduces fatal hazards:
- **Prerequisite Loading**: Chez Scheme's `compile-file` requires prerequisite modules to be loaded into the compiler process so that module interfaces, record descriptors, and macros can be resolved during nanopass expansion.
- **Symbol Table Contention**: If Thread 1 loads `moduleA.so` while Thread 2 loads `moduleB.so` into the same environment, concurrent symbol table mutations and macro bindings risk data races.
- **FASL Deserialization Invariant**: Serializing shared objects from an environment undergoing asynchronous mutations violates **Hard Invariant #6** (*Hermetic AOT Serialization*), risking non-deterministic FASL binaries.

#### The Architecture: Isolated Worker Processes
By deploying a pool of $N$ **separate worker processes** (similar to `make -j`, Ninja, or Clang/GCC):
1. **Zero Shared State**: Each worker has its own isolated OS address space and its own private `(interaction-environment)`.
2. **On-Demand Prerequisite Loading**:
   - When Worker 1 compiles `math/math-kbd.scm`, it loads only the prerequisite `.so` files needed for `math-kbd` into its private environment.
   - Worker 2 concurrently compiles `convert/latex/tmtex.scm`, loading only `tmtex`'s prerequisites into its private environment.
3. **Prerequisite Cache inside Worker**:
   - Workers remain persistent daemon processes over IPC pipes/stdin throughout Pass 2.
   - If Worker 1 previously loaded `utils/base/environment.so`, it does not need to reload it for subsequent compilation jobs that share that prerequisite.
4. **100% Invariant Compliance**:
   - Every `compile-file` invocation takes place in a clean, hermetic environment containing only that module's validated prerequisites.
   - FASL code generation is 100% deterministic and reproducible.

---

## 4. Cross-Module Macro Dependencies & The Oracle

A critical characteristic of TeXmacs Scheme is that **macro expansion in module $B$ may depend on syntax or helper functions defined in module $A$, even if $A$ is not explicitly listed in $B$'s `:use` header**.

### 4.1 How the Oracle Functions
The compiler maintains an index of every definition in the TeXmacs source tree ([`src/Scheme/Chez/compiler-oracle.scm`](file:///Users/king-kong/Documents/GitHub/texmacs/src/Scheme/Chez/compiler-oracle.scm)):
```scheme
(vector name type form top rel mod)
```
- `name`: the identifier (e.g. `gui-make-let`, `tm-define-macro`, `table-set!`).
- `type`: `'macro` or `'value`.
- `rel`: relative file path (`"generic/generic-edit.scm"`).
- `mod`: defining module symbol (`'(generic generic-edit)`).

### 4.2 Handling Dependencies in Pass 1 & Pass 2
1. **Static Prerequisite Edge Addition**:
   During Pass 1's syntactic scan, AST references to known macros and expand-time helpers are looked up in `*oracle-defs*`. For any helper defined outside the kernel snapshot (`texmacs-user.so`), Pass 1 registers an explicit prerequisite dependency:
   $$\text{Prereqs}(B) = \text{DeclaredImports}(B) \cup \text{MacroDefiningModules}(B)$$
2. **Header Synthesis**:
   Pass 1 ensures that `(import (for <macro-def-mod> run expand))` is emitted directly into $B$'s generated Chez module header.
3. **Execution Order Guarantee**:
   The dependency graph guarantees that $A$'s `.so` is compiled before $B$. When the worker process assigned to $B$ starts, it loads $A$'s `.so`, ensuring all required macro transformers and expand-time helpers are fully available in the worker's environment before `compile-file` runs.

---

## 5. Dependency Waves & Dynamic Job Scheduling

Topological analysis of the 453 modules across TeXmacs reveals **15 parallel dependency waves**:

```
================================================================================
             TEXMACS MODULE DEPENDENCY WAVES (453 MODULES)
================================================================================
Wave 0:  103 modules  [========================================] (Independent leaves)
Wave 1:   50 modules  [====================]
Wave 2:   49 modules  [===================]
Wave 3:   28 modules  [===========]
Wave 4:   28 modules  [===========]
Wave 5:   27 modules  [==========]
Wave 6:   25 modules  [==========]
Wave 7:   34 modules  [=============]
Wave 8:   32 modules  [============]
Wave 9:   21 modules  [========]
Wave 10:  28 modules  [===========]
Wave 11:  15 modules  [======]
Wave 12:   7 modules  [===]
Wave 13:   4 modules  [==]
Wave 14:   2 modules  [=]
================================================================================
```

### 5.1 Concurrency Characteristics
- **Massive Early Parallelism**: Waves 0 through 2 contain **202 modules (44.6% of the tree)** with zero inter-dependencies. All worker processes will remain at 100% CPU saturation.
- **Sustained Mid-Stage Work**: Waves 3 through 10 maintain 20–34 independent modules per wave, providing steady utilization across 8–16 cores.
- **Dynamic Scheduling vs Wave Barriers**:
  Rather than using rigid wave barriers where all workers wait for the slowest module in a wave to finish, the coordinator uses a **dynamic ready queue**:
  - Each module tracks its remaining in-degree (number of pending prerequisite `.so` files).
  - As soon as a worker finishes compiling a module, its dependents' in-degrees are decremented.
  - Any dependent reaching an in-degree of 0 is immediately enqueued to the next available worker.
  - This eliminates wave synchronization stalls caused by long-running modules (e.g. `tmtex.scm` at 0.67 s).

> **Caveat (§9.4).** The graph is not the clean DAG this section assumes. `tm-files.scm` and
> `tm-print.scm` import each other, and the sequential build resolves that by deferring the
> later one to a runtime `module-provide`. Edges must follow the same direction — the emitted
> `(import ...)` forms, not the declared `:use` list — or in-degree scheduling deadlocks.

---

## 6. Hard Invariants & Concurrency Safety

1. **Hard Invariant #1 (Pristine Upstream Sources)**:
   - Untouched: Pass 1 reads `TeXmacs/progs/*.scm` strictly read-only.
2. **Hard Invariant #2 (Guile Parity)**:
   - Module semantics, initialization orders, and exports match GNU Guile behavior.
3. **Hard Invariant #3 & #6 (Hermetic Environments & Root AOT Serialization)**:
   - Each worker compiles modules in its own isolated root `(interaction-environment)`.
   - Never wrap `compile-file` in child or anonymous environments.
   - Eliminates all cross-thread interference and ensures deterministic FASL byte output.
4. **Exception Handling & Fail-Fast**:
   - If any worker process reports a compilation failure, the coordinator captures the stack trace, cancels remaining jobs, terminates the worker pool, and exits with a non-zero exit code.

---

## 7. Implementation Roadmap & Milestones

### Milestone 1: Pass Decoupling & Pure Syntactic Pass 1
- **Task:** Refactor [`src/Scheme/Chez/compile-texmacs.scm`](file:///Users/king-kong/Documents/GitHub/texmacs/src/Scheme/Chez/compile-texmacs.scm) into two distinct sequential phases:
  ```scheme
  (transpile-all-modules-syntactic! file-list)
  (compile-all-modules-sequentially! file-list)
  ```
- **Changes:**
  - Remove `(eval-eager-forms! prepared)` from the transpilation loop.
  - Ensure all platform conditionals (`os-macos?`, `supports-guile?`) resolve via `boot-chez.scm`.
  - Verify that Pass 1 writes all 453 `.scm` module files and outputs the concrete dependency DAG in **< 0.5 seconds**.
- **Verification:** Run sequential compilation of the emitted files; verify 453/453 `.so` files match existing build outputs.
  > **Correction (§9.1).** This criterion cannot be met and should not be used. Chez `.so`
  > output is not byte-reproducible: two identical clean builds differ in 445 of 454 files.
  > The usable check is the emitted `.scm`: all 455 of them are byte-identical to the
  > pre-split build, which is the real proof that Pass 1 preserved behaviour.

### Milestone 2: Persistent Worker Process (`compile-worker.scm`)
- **Task:** Create `src/Scheme/Chez/compile-worker.scm`:
  - Worker boots with `texmacs-user.so` (kernel snapshot, ~34 ms).
  - Worker enters an IPC command loop on `(current-input-port)`:
    ```scheme
    (:compile <module-rel-path> <prereq-list>)
    ```
  - Worker loads any missing prerequisites from `<prereq-list>` into its private environment.
  - Worker executes `(compile-file in-scm out-so)` and replies with `(:ok <rel-path> <elapsed-ms>)` or `(:error <rel-path> <message>)`.

### Milestone 3: Dynamic Coordinator Pool (`compile-pool.scm`)
- **Task:** Create coordinator in `src/Scheme/Chez/compile-pool.scm`:
  - Spawns $N$ worker processes (defaulting to `(cpu-count)` or `TEXMACS_COMPILE_JOBS`).
  - Initializes the dynamic ready queue using the concrete DAG from Pass 1.
  - Dispatches compilation jobs as prerequisites finish.
  - Aggregates timing statistics and handles clean worker shutdown.

### Milestone 4: Performance Verification & Certification
- **Task:** Benchmark clean and incremental builds:
  - Verify speedup across 1, 2, 4, and 8 worker processes.
  - Verify zero regressions by launching GUI (`./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs`) and running the automated test suite.

---

## 8. Projected Speedup & Scaling Analysis

Let $T_{\text{baseline}} = 22.81\,\text{s}$ (sequential clean rebuild):
- $T_{\text{prescan}} = 0.28\,\text{s}$ (sequential)
- $T_{\text{transpile\_syntactic}} = 0.46\,\text{s}$ (pure syntactic AST rewriting & file writing)
- $T_{\text{compile}} = 18.88\,\text{s}$ (parallelizable across workers)
- $T_{\text{worker\_overhead}} \approx 0.50\,\text{s}$ (prerequisite `.so` loads in workers)
- $T_{\text{post}} = 0.04\,\text{s}$ (manifest and DB write)

Using Amdahl's Law with $P$ worker processes and an estimated parallel efficiency factor $\eta \approx 0.85$:

$$T_{\text{parallel}}(P) = T_{\text{prescan}} + T_{\text{transpile}} + \frac{T_{\text{compile}}}{P \cdot \eta} + T_{\text{worker\_overhead}} + T_{\text{post}}$$

| Concurrency ($P$) | Parallel `compile-file` | Fixed Phases & Overhead | Total Wall Time | Speedup vs Baseline |
|:---:|:---:|:---:|:---:|:---:|
| **1 Worker** (Sequential) | 18.88 s | 1.28 s | **20.16 s** | 1.1× |
| **2 Workers** | 11.10 s | 1.28 s | **12.38 s** | 1.8× |
| **4 Workers** | 5.55 s | 1.28 s | **6.83 s** | 3.3× |
| **8 Workers** (Apple M-series) | **2.78 s** | **1.28 s** | **4.06 s** | **5.6×** |
| **16 Workers** (Linux server) | **1.39 s** | **1.28 s** | **2.67 s** | **8.5×** |

A clean build time drop from **22.8 seconds to ~4.0 seconds** fundamentally accelerates developer turnaround and continuous integration, transforming full module recompilation into an instantaneous operation.

> **Measured (§9.4).** Not reproduced. On this 8-core machine the model above predicts
> ~4 s at 8 workers; the implementation delivered 81 s at 8 workers and 56 s at 4, against
> 26 s sequential. The gap is not `compile-file` — it is per-worker environment
> reconstruction plus a coordinator that serialises on the slowest in-flight module.

---

## 9. Implementation Findings (2026-11-12)

All numbers below are from clean `-recompile-modules` builds on Apple Silicon (8 cores),
Chez Scheme 10.4.1.

### 9.1 The two-pass split works and is behaviour-preserving

Implemented as `transpile-module-list` (Pass 1) then `compile-module-list` (Pass 2) in
`src/Scheme/Chez/compile-texmacs.scm`, with `*transpiled-modules*`, `*pending-build*` and
`*eager-forms*` carrying the hand-off.

| phase | time |
|---|---|
| Pass 1 (453 modules) | 0.635 s (includes 0.15 s oracle index) |
| Pass 2 | 27.75 s |
| total wall | 29.6 s (baseline 31.6 s) |
| no-op incremental | 0.002 s + 0.86 s |

Pass 1 is slightly above the "< 0.5 s" target because it also builds the oracle index. All
455 emitted `.scm` files are byte-identical to the pre-split build.

`.so` files are **not** reproducible: two identical clean builds differ in 445 of 454 files,
so no build-to-build comparison of binaries is meaningful.

### 9.2 `eval-eager-forms!` is load-bearing

Deleting it fails 8 kernel modules — `tm-modes`, `tm-dialogue`, `tm-plugins`, `tm-secure`,
`tm-convert`, `old-gui-{test,form,factory}` — with `variable X is not bound` raised inside
`chi-macro`.

The mechanism: `texmacs-modes` is a `define-public-macro` whose expander calls
`(map texmacs-mode l)`, and `texmacs-mode` is an ordinary `define-public` earlier in the same
file. `needs-eager-eval?` never matched plain `define`, so nothing evaluated it; the *oracle*
installed it on demand, and the oracle only ran inside `eval-eager-forms!`.

Two consequences worth keeping:

- **Relocate, don't remove.** It now runs in Pass 2 immediately before `compile-file`, in the
  compiling module's own environment. The 2.4 s is real work and it parallelises.
- **Dependency discovery is expansion-based.** `mc-free-references` calls `(expand ...)` and
  returns `'()` when expansion raises, so a form that *calls* an uninstalled helper yields no
  dependencies and the helper is never installed. That is why the five
  `(ahash-set! define-option-table :type (define-property :type))` forms in `tm-define.scm`
  print `FAILED-EVAL` in every build, before and after the split. It is harmless — those
  entries only populate the scratch compilation environment, and the real module evaluates
  them at load time when all definitions exist — but it is a real chicken-and-egg in the
  oracle.

### 9.3 Threads cannot replace processes

| probe | result |
|---|---|
| `fork-thread`, `make-mutex`, `make-condition` | bound, work |
| `parameterize` inside a thread | thread-local (thread saw `thread`, main saw `main`) |
| `make-environment` | **unbound** |
| `new-interaction-environment` | **unbound** |
| `make-top-level-environment` | **unbound** |

`parameterize` gives each thread its own dynamic binding of `(interaction-environment)`, but
there is no way to create a second mutable top-level environment to bind it to. Every thread
would share one environment, and the transpiler installs and unbinds helpers there per module
through the single global `*ephemeral-syms*` list — thread A would unbind thread B's helpers
mid-expansion, failing with the same 8 modules as §9.2.

Threads were used, but only in the coordinator, for the ready queue.

### 9.4 The worker pool: implemented, measured, reverted

Design: worker = the driver itself with `TEXMACS_COMPILE_WORKER=1`; line protocol over
stdin/stdout (`C <rel>` / `@@TMCP OK|ERR`); prerequisites taken from the emitted module's own
`(import ...)` forms rather than a derived closure.

| workers | wall | CPU | result |
|---|---|---|---|
| 1 (sequential) | 26 s | 24.5 s | 453/453, 0 failed |
| 4 | 56 s | 63.7 s | 3 modules failed |
| 8 | 81 s | 8.5 s (10 %) | 2 modules failed |

Three things a worker must do to match the sequential environment, each found the hard way:

1. **Load kernel modules individually** through `load-compiled-module!`. The aggregate
   `texmacs-user.so` loads its members with `primitive-load`, which never registers them, so
   their deferred `module-provide` lookups fail.
2. **Push each loaded mod-sym onto `compiled-modules`.** `import-declared-deps!` tests that
   list; without it a worker compiles with none of its declared imports in the environment.
3. **Call the same exception-wrapped compile entry point.** A bare `compile-file` call lets a
   compiler *warning* escape as a fatal `no continuation in condition`; the sequential path
   survives it because its handler returns for non-serious conditions.

Why it lost:

- **Environment fidelity.** 3 modules compile differently in a worker:
  `math-speech-{en,fr}` (`invalid syntax (begin)`) and `tmtex-acm` (`smart-table-insert` is
  not bound). The sequential environment accumulates every module's `import-declared-deps!`
  imports across the whole build; a worker has only its own prerequisites. Those modules were
  quietly relying on the leakage.
- **Throughput.** Each worker recovers its prerequisite set by reading and parsing every
  dependency's `.scm`. Total CPU rose from 24.5 s to 63.7 s at 4 workers — the pool does more
  work than the build. At 8 workers wall time was 81 s against 8.5 s of CPU (10 %
  utilisation). The likely cause is head-of-line blocking: the coordinator waits on the oldest
  in-flight job, so a slow module (`tmtex.scm`, 0.67 s) leaves other workers idle. That was
  not isolated experimentally; it is the first thing to measure on a retry.
- **The dependency graph is not acyclic.** `tm-files.scm` and `tm-print.scm` import each
  other. The sequential build resolves this by deferring the later one to a runtime
  `module-provide`; any scheduling graph must respect the same direction, and a closure built
  from declared `:use` edges deadlocks on it.

### 9.5 If this is resumed

1. Ship the import graph from Pass 1 instead of re-parsing sources in every worker.
2. Use the threaded ready queue (§5.1) rather than blocking dispatch.
3. Decide the fidelity story: either a sequential repair pass for modules a worker fails
   (cheap, correct, costs nothing when nothing fails), or a warm start — fork or heap image —
   so a worker inherits the exact sequential environment instead of rebuilding one.


### 9.6 Second attempt (2026-11-12, after the refactor): one script, no threads

A refactor first split the driver into `convert-to-r6rs.scm` (Pass 1 + the dirty set + the
kernel snapshot), `compiler-helper.scm` (paths, module names, the per-module environment
lifecycle, the compile-file call) and `compiler-oracle.scm` (closure walkers + index/install).
`compile-texmacs.scm` went from 1315 to 248 lines, and every artifact stayed byte-identical.

That made the pool small enough to keep **inside the driver** — no second script, no library,
no C++ change: 296 lines added to `compile-texmacs.scm`, and `TEXMACS_COMPILE_JOBS=n`
(default 1) only switches the tier-2..7 loop. Shape:

* one coordinator **thread of control** (no mutex, no `fork-thread`): spawn N
  `compile-worker.scm` processes with `open-process-ports`, stderr redirected to
  `boot/compile-worker-N.log` so it cannot fill a pipe; then dispatch to idle workers and poll
  busy ones with `input-port-ready?` plus 2 ms sleeps;
* the schedule comes from the **emitted `(import ...)` forms**, one read per module, cached;
* a job hands the worker the module's **transitive closure** in dependency order, and the
  worker keeps what it loaded for its next job;
* a module is retried when the pool drains, and a retry hands the worker **everything built so
  far**, because the failures below are undeclared-dependency races;
* each success records the `.so` hash in the compiler DB, so incrementality survives.

Measured on the 8-core machine, clean `-recompile-modules`, 453 modules:

| jobs | wall | failures | retries | pool phase |
|---|---|---|---|---|
| 1 | 24.3 s | 0 | — | — |
| 2 | 18.1 s | 0 | 0 | ~13 s |
| 4 | 15.1 s | 0 | 10–13 | 10 s (first pass 8 s) |
| 8 | 15.2 s | 0 | 10 | 10 s (first pass 6 s, retries 4 s) |

Emitted `.scm` byte-identical to the sequential build; an incremental build straight after a
pooled one recompiles 0 modules. So it is correct, and 1.6× at 4 workers — but 8 workers buys
nothing, and the effort is not repaid. Reverted.

**What the measurements establish**

1. **Threads are unnecessary, and undesirable.** `fork-thread`, `make-mutex`,
   `mutex-acquire`, `condition-wait`/`condition-broadcast` are all bound, so it is possible —
   but the coordinator only moves bytes on pipes, and `display`/`format` go through the
   TeXmacs C++ I/O hooks, which is not something to call from several threads at once.
   `input-port-ready?` + `(sleep (make-time 'time-duration ...))` gives the same result with
   none of the concurrency reasoning. (9.3's threaded ready queue was solving a problem that
   does not exist at this scale.)
2. **The emitted import graph is acyclic** — 0 back-edges across the 453 emitted modules,
   because Pass 1 emits an `(import ...)` only for a module it has already transpiled and
   defers the rest to `module-provide`. Scheduling on `:use` deadlocks (`tm-files`/`tm-print`);
   scheduling on the emitted forms does not. A stall check is still needed for bookkeeping bugs.
3. **"Only the dependencies" is not sufficient.** `math/math-kbd.scm` uses `group-table`, a
   `tm-define` in `utils/edit/variants.scm` that it neither imports nor relates to; the same
   holds for `smart-table-insert` and for `:state` entries in `define-option-table`. The
   sequential build has those names because every earlier module was loaded and imported. A
   closure-only worker works at 2 and 4 workers (the order happens to agree) and fails at 8,
   where `math-kbd` starts while `variants` is still compiling: `variable group-table is not
   bound`, followed by ~350 cascading failures. This — not scheduling — is the ceiling.
4. **The closure must be transitive.** Direct imports are not enough: loading `math-edit.so`
   needs math-edit's own imports present.
5. **A failed module must not block its dependents**, or the pool stalls forever waiting for a
   module that will never have a `.so`; mark it done and let the dependents fail visibly.
6. **`tm-load-file` hides prerequisite load failures.** It prints `LOAD-ERROR` on stdout and
   returns instead of raising, so the worker caches the module as loaded and every later job
   in that process fails with `unknown module ...`. Prerequisite loads must use Chez's own
   `load`, captured before the engine rebinds the name.
7. **The compiler DB is the coordinator's job.** Without `update-compiler-db-artifact!` per
   success, the next incremental build recompiles every dependent (measured: 171 modules after
   6 worker-compiled modules).
8. **Where the time goes at 4 workers:** prologue ~5 s (Pass 1 0.6 s, 40 kernel modules
   ~2.7 s, snapshot, 0.6 s of worker boots), first pass 8 s, retry tail 2 s. At 8 workers the
   first pass is 6 s but the retry tail grows to 4 s, because retries load the whole tree.

**Pitfalls that cost time, for anyone retrying**

* `memq` against `(rel-path->mod-name ...)` never matches (it builds a fresh list): use
  `member`, or the load cache silently does nothing and every job reloads every prerequisite.
* Seed the "already usable" set with the kernel tier, and with cached modules; otherwise every
  module that imports a kernel module looks permanently blocked.
* A retry round must **merge** into the pending list, not replace it, or the modules waiting on
  the retried ones are dropped and `remaining` never reaches zero.
* The engine leaves `(command-line)` empty and sets no `TEXMACS_BIN`, so the binary has to be
  probed next to the boot tree (`TEXMACS_BIN` overrides) before workers can be spawned.
* `sleep` takes a time record, not seconds; the spawn command line needs `~s` quoting.

**If ever resumed, the cheap wins** are (a) declare the missing `:use` in the handful of
modules above — then closure-only workers work and the retry round (10–13 modules, 1–2 s)
disappears; (b) cap at 4 workers; (c) or warm-start workers by fork so each inherits the exact
sequential environment instead of rebuilding it (§9.5 item 3).

---

### 9.6 Compiler Database Export Parity and Incremental Integrity

A critical discrepancy was resolved between static prescan (`compiler-scan.scm`) and syntactic transpilation (`convert-to-r6rs.scm`):
1. **Conditional Unwrapping in `scan-collect`:** `scan-flatten-begins` previously only flattened top-level `begin` forms, ignoring `(if ...)`, `(when ...)`, and `(unless ...)`. This caused `scan-collect` to miss definitions inside conditional branches (such as `kernel/boot/ahash-table.scm` where all core hash table functions were wrapped in `(if (vector? ...) ...)` and `kernel/boot/abbrevs.scm` where `save-object` was wrapped in `(if (guile-c?) ...)`). Updating `scan-flatten-begins` to unwrap conditional branches and deduplicate exports brought the prescan export count from 790 to the exact authoritative count of 799 exports.
2. **Authoritative `*compiler-db*` Updates:** `prepare-forms` in `convert-to-r6rs.scm` is now the single source of truth for module exports. During transpilation, `(update-compiler-db-exports! mod-name public)` updates the database and recomputes the interface hash. On incremental builds, cached module entries are seeded directly from `*saved-db*` when source hashes match.
3. **Verification:** Full clean rebuilds produce 453/453 modules with 0 failures and 799 total exports in `compiler-db.fasl`. An incremental run immediately follows with 453/453 cached modules, and touching a leaf module rebuilds only that module and updates the database cleanly.
