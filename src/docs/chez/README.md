# TeXmacs / Chez Scheme — Developer Guide

**Status (`wip-chez`):** Chez Scheme **10.4.1**, Qt **6.12.0** (macOS arm64 / Windows MSYS2) and Qt **6.10.2** (Ubuntu 26.04 arm64). All **453/453** Scheme modules transpile and compile to native `.so` with 0 failures, packaged with a VFASL `texmacs.boot` fast-boot image and 1,176 static C++ glue procedures (`initialize_glue_chez` + `foreign-procedure`). GUI launch is verified (`texmacs-version` 2.1.5).

---

## Documentation Index

### Build & Packaging Guides
- [build_guide/MACOS.md](build_guide/MACOS.md) — Building, running, and packaging (`.app` / `.dmg`) on macOS
- [build_guide/LINUX.md](build_guide/LINUX.md) — Building Chez Scheme and TeXmacs on Linux (Ubuntu arm64 / x86_64)
- [build_guide/WINDOWS_MSYS2.md](build_guide/WINDOWS_MSYS2.md) — Building and running on Windows with MSYS2 (UCRT64)

### Architecture, Migration & Tasks
- [TODO_CHEZ.md](TODO_CHEZ.md) — Active bugs, crash/stability fixes, and remaining optimization tasks
- [CHEZ_MIGRATION_LOG.md](CHEZ_MIGRATION_LOG.md) — Phasing, `texmacs.boot` heap construction, native glue migration, and dependency fixes
- [Load_Chez_Fast.md](Load_Chez_Fast.md) — Fast boot (`texmacs.boot` VFASL) vs. normal `.so` init startup and memory findings
- [Handling_Encoding_Chez.md](Handling_Encoding_Chez.md) — String encoding duality (UTF-8 text, 8-bit Cork, raw binary bytevectors)
- [PARALLEL_COMPILER_PLAN.md](PARALLEL_COMPILER_PLAN.md) — Two-pass compiler split and lessons from the reverted parallel worker pool

### Performance, Benchmarks & Profiling
- [benchmarks.md](benchmarks.md) — 10-run UI, manual regeneration, and conversion benchmarks (Chez vs. s7 vs. Femtolisp)
- [bench/CHEZ_PERFORMANCE_ANALYSIS.md](bench/CHEZ_PERFORMANCE_ANALYSIS.md) — Root-cause analysis of Chez vs. interpreter performance
- [bench/PROFILING.md](bench/PROFILING.md) — Profiling `ui.scm` keystroke latency and memory
- [bench/CONVERSIONS_PROFILING.md](bench/CONVERSIONS_PROFILING.md) — Source-level `compile-profile` of `conversions.scm`

---

## 1. Compilation & Boot Pipeline

- **Two-pass AOT compilation** (`src/Scheme/Chez/convert-to-r6rs.scm` → `compile-texmacs.scm`, assisted by `compiler-oracle.scm`):
  1. Reads pristine sources from `TeXmacs/progs/` (kept `git diff`-clean) and transpiles them into `build-texmacos/boot/`. `chez-compat` and `abbrevs` are emitted as auto-imported R6RS libraries; remaining modules are wrapped in Chez `(module ...)` forms and compiled in topological dependency order.
  2. Writes `module-deps.scm` and `compiler-db.fasl`.
- **Fast boot vs. dev boot:** `make-boot.scm` packages the 178 startup modules into VFASL `texmacs.boot` (loaded during `Sbuild_heap()` via `init-texmacs-boot-file.so`); remaining modules lazy-load from `.so` on demand. Set `TEXMACS_NO_BOOT=1` to bypass `texmacs.boot` and load individual `.so` files via `init-texmacs-chez.scm` (see [Load_Chez_Fast.md](Load_Chez_Fast.md)).
- **Manual module rebuilds:**
  - Incremental: `TeXmacs -compile-modules`
  - Clean full rebuild (~23–30 s; recommended after structural or macro changes): `TeXmacs -recompile-modules`
  - Always recompile the C++ binary (`ninja -C build-texmacos`) before running `-compile-modules` if C++ or FFI glue files changed.

---

## 2. Core Invariants

1. **Pristine upstream sources:** Keep `TeXmacs/progs/` free of Guile-incompatible rewrites; do not load Guile's `init-texmacs.scm`.
2. **Static glue FFI:** `initialize_glue_chez()` (`src/Scheme/Chez/glue_chez.cpp`) registers C++ entry points via `Sforeign_symbol()` in `custom_init()` before `Sbuild_heap()`, and `TeXmacs/progs/kernel/boot/glue-chez.scm` binds them with `foreign-procedure`. Do not put Scheme source strings in `chez_tm.cpp`.
3. **GC & thread safety:** Process-lifetime Scheme handles cached in `initialize_scheme()` are pinned once with `Slock_object`; transient C++↔Scheme conversions use the `ChezRoot` shadow root stack (`chez_tm.hpp`). Every C++→Scheme entry from Qt must activate the Chez thread (`Sactivate_thread`) and hold `chez_lock`.
4. **Encoding boundaries:** `string-load` and `tmfs-load` return R6RS `bytevector`s for binary assets; 8-bit Cork strings tunnel through Latin-1 Scheme strings (`*latin1-tc*`, `eol-style none`). See [Handling_Encoding_Chez.md](Handling_Encoding_Chez.md).
5. **No AI git commits:** Leave agent changes unstaged/uncommitted for user review.

---

## 3. Tests & Benchmarks

**Native toolchain tests** (`src/Scheme/Chez/*-test.scm`, 195 checks):

```bash
cd texmacs
for t in compile-inventory-test.scm compile-graph-test.scm macro-closure-test.scm; do
  chez -q "src/Scheme/Chez/$t"
done
(cd src/Scheme/Chez && for t in compiler-db-test.scm compiler-scan-test.scm; do chez -q "$t"; done)
```

**TeXmacs runtime test suite** (run from `texmacs/` with the platform runtime environment + `TEXMACS_HOME_PATH="$PWD/.sandbox-texmacs"`):

```bash
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs -H \
  -x '(begin (use-modules (check check-master)) (run-all-tests) (display "ALL-TESTS-DONE") (newline) (quit-TeXmacs))'
```

*(Note: `run-all-tests` currently stops on the known `regtest-xmltm` Latin-1/UTF-8 mismatch and `regtest-tmhtml` fixture mismatch; inspect the log for `Do not match!` / `Regression failure`.)*

**Benchmarks** (`docs/chez/bench/`): Run `ui.scm`, `ui-profile.scm`, `manual.scm`, `conversions.scm`, or `memory.scm` via `-x '(load "docs/chez/bench/<script>.scm")'` with `TEXMACS_DOC_PATH="$PWD/TeXmacs/doc"`. See [benchmarks.md](benchmarks.md) for methodology and results.
