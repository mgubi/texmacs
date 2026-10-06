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
| [05-build-and-vendored-femtolisp.md](05-build-and-vendored-femtolisp.md) | Choosing the interpreter, the vendored femtolisp, its 20 patches, rebuilding the boot image |
| [06-open-issues.md](06-open-issues.md) | Known differences with Guile, failing checks, fragile spots, what to do next |
| [07-performance.md](07-performance.md) | Boot time and memory against s7, where the time goes |

## Summary

- **The interpreter is a build option:** `./configure --with-scheme=femtolisp`
  (CMake: `-DSCHEME_IMPL=femtolisp`). The choices `s7` (default) and `guile`
  are unchanged. A femtolisp build needs nothing outside the source tree.
- **femtolisp is vendored with 20 local patches** in
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
- **Performance:** boot takes about 1.3 s against 0.65 s for s7, with 165 MB
  of memory against 400 MB (see [07](07-performance.md)).
