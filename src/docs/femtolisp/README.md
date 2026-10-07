# TeXmacs on femtolisp

The `wip_femto` branch (on top of `wip_s7`) lets TeXmacs run its Scheme code
on [femtolisp](https://github.com/JeffBezanson/femtolisp), a small Scheme-like
Lisp with a bytecode compiler, instead of Guile or s7. These notes describe
how it works, what differs from Guile, how it performs, and what is left.

| File | Contents |
|---|---|
| [01-cpp-binding.md](01-cpp-binding.md) | The C++ side: the C interface `fl_tm.h`, `tmscm` as a self-registering GC root, errors and longjmps, blackboxes, glue |
| [02-boot-and-modules.md](02-boot-and-modules.md) | The boot sequence, modules as renamed global names, late calls for macros defined later, `tm-define` |
| [03-compat-layer.md](03-compat-layer.md) | `r5rs-femtolisp.scm` and `compat-femtolisp.scm`: R5RS and Guile on femtolisp, byte strings, errors, complex numbers |
| [04-progs-changes.md](04-progs-changes.md) | The changes to the Scheme code shared by all the interpreters, and the tests |
| [05-build-and-vendored-femtolisp.md](05-build-and-vendored-femtolisp.md) | Choosing the interpreter, the vendored femtolisp, its 21 patches, rebuilding the boot image |
| [06-open-issues.md](06-open-issues.md) | Known differences with Guile, failing checks, fragile spots, what to do next |
| [07-performance.md](07-performance.md) | femtolisp, s7 and Guile on boot, tests, conversions, LaTeX export, the manual and the C++ boundary |
| [08-lazy-bodies.md](08-lazy-bodies.md) | function bodies expanded and compiled at their first call |
| [09-browser.md](09-browser.md) | femtolisp in the browser build (`SCHEME=femtolisp`) |
| [10-benchmarks.md](10-benchmarks.md) | the benchmarks of 2026-10-07: where the time goes, the heap, the caches, what was kept |
| [bench/](bench) | The script which runs the benchmarks of `docs/s7/bench` on several builds |

## Summary

- **The interpreter is a build option:** `./configure --with-scheme=femtolisp`
  (CMake: `-DSCHEME_IMPL=femtolisp`). The choices `s7` (default) and `guile`
  are unchanged. A femtolisp build needs nothing outside the source tree.
- **femtolisp is vendored with 21 local patches** in
  `src/Scheme/Femtolisp/patches`, each one small and described in
  [05](05-build-and-vendored-femtolisp.md). Most make femtolisp read, print
  and evaluate as Guile does; a few are hooks for the embedding.
- **The C++ side reuses the `tmscm` layer.** `femtolisp_tm.{hpp,cpp}`
  implement it on a small C interface (`fl_tm.h`), so the generated glue is
  the same as for s7 and Guile. femtolisp's collector moves objects, so a
  `tmscm` is a C++ object that registers itself as a GC root.
- **Strings are TeXmacs strings.** A string is a string of bytes (Cork), a
  character is a byte, `write` gives the bytes from 128 unchanged: what Guile
  1.8 does, and what the C++ code expects.
- **Modules are renamed global names.** femtolisp has one global environment.
  The private definitions of a module become global names `name@module` when
  the module is compiled; public definitions and `tm-define`s are global under
  their own names.
- **Macros are expanded when code is compiled**, Guile expands them when code
  first runs. Calls of names unknown at compile time are compiled at their
  first evaluation, and failing macro expansions raise their error when the
  code runs, so code written for Guile keeps working.
- **Tests:** 40 of the 43 regression suites pass; the 4 failing checks are
  listed in [06](06-open-issues.md).
- **Performance** (see [07](07-performance.md)): femtolisp and s7 are close,
  and much faster than Guile. femtolisp is the fastest on Scheme-heavy work
  (8 warm LaTeX exports: 1.8 s, s7 2.3 s, Guile 6.9–8.4 s; the regression
  suites: 0.20 s, s7 0.23 s, Guile 0.35–0.40 s); s7 boots a little faster
  (0.9 s, femtolisp 0.95–1.05 s, Guile 1.5 s). The boot relies on a cache
  of the compiled files in `$TEXMACS_HOME_PATH/system/cache/femtolisp`.
  Memory use is close to Guile's, below s7's.
