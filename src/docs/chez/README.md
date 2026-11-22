# TeXmacs on Chez Scheme

The branch `wip_other_schemes` (merged from `wip_chez`) lets TeXmacs run its
Scheme code on [Chez Scheme](https://cisco.github.io/ChezScheme/) 10.4, beside
Guile, s7 and femtolisp. Chez compiles to machine code: the Scheme sources of
`TeXmacs/progs` are compiled ahead of time into Chez modules, and the modules
needed at start-up are packed into a boot image.

The port comes from the branch `wip-chez` of
[nerd-qp/texmacs](https://github.com/nerd-qp/texmacs) (Joy Yang), which made
the module compiler, the boot image and the C++ glue. It was then adapted to
this branch: built by `configure`, strings as byte strings (as in Guile 1.8
and the other Schemes of this branch), start through the shared
`init-texmacs.scm`, errors with Guile's keys, and the fixes needed for all the
regression suites.

| File | Contents |
|---|---|
| [01-build.md](01-build.md) | Configuring, building, compiling the modules and the boot image, running, the environment variables, the tests |
| [02-reader-and-compiler.md](02-reader-and-compiler.md) | Reading Guile code (`chez-reader.scm`), the module compiler, the boot image |
| [03-runtime.md](03-runtime.md) | `boot-chez.scm` and `chez-compat.scm`: byte strings, ports, modules, errors, blackboxes, what Guile has and Chez lacks |
| [04-cpp-binding.md](04-cpp-binding.md) | The C++ side: `chez_tm`, the generated glue, errors across the boundary, strings from C++ |
| [05-progs-changes.md](05-progs-changes.md) | The changes to the code shared by all the Schemes |
| [06-open-issues.md](06-open-issues.md) | Known differences, fragile spots, what is left |
| [07-performance.md](07-performance.md) | Start-up, the LaTeX export and the test suites on Chez, s7, femtolisp and Guile |

## Summary

- **A build option:** `./configure --with-scheme=chez`. Chez is linked
  statically from its installation (`libkernel.a`); its heap images
  (`petite.boot`, `scheme.boot`) are read at start-up.
- **Compiled ahead of time:** `make` compiles the 488 modules of
  `TeXmacs/progs` into `TeXmacs/boot/` (those whose source changed), and
  packs the 177 modules of the start-up into `TeXmacs/boot/texmacs.boot`
  (18 MB); `make install` installs them.
- **Guile code, read as Guile reads it:** the sources are unchanged Guile
  code; a reader of Guile's syntax folds the tests of the Scheme
  (`(chez-scheme?)`, `(s7-scheme?)`...) when it reads them, and the compiler
  turns each TeXmacs module into a Chez module.
- **Strings are byte strings:** one character per byte (Latin-1 for Chez), so
  Cork text and UTF-8 bytes go through Scheme unchanged, as with Guile 1.8.
- **Tests:** all the regression suites pass, 43 of 43, as on Guile and
  femtolisp (s7 has two suites of its own, 45).
- **Performance** (see [07](07-performance.md)): start-up 0.69 s (s7 0.88 s);
  a warm LaTeX export of the change log 80 ms (s7 300 ms, femtolisp 181 ms);
  the whole test run 64 s (s7 72 s, femtolisp 68 s), where most of the time
  is C++.

## The notes of the original branch

The other files of this folder come from the branch of nerd-qp and describe
it as it was there (a CMake build in `build-texmacos/`, UTF-8 strings beside
Cork text, its own copy of the start-up). They are kept for their history
and measurements; where they disagree with the numbered notes, the numbered
notes describe this branch.

- [build_guide/](build_guide) — building Chez and TeXmacs on macOS, Linux and
  Windows (MSYS2), with CMake
- [TODO_CHEZ.md](TODO_CHEZ.md), [CHEZ_MIGRATION_LOG.md](CHEZ_MIGRATION_LOG.md)
  — the tasks and the history of the port
- [Load_Chez_Fast.md](Load_Chez_Fast.md) — the boot image against loading the
  modules one by one
- [Handling_Encoding_Chez.md](Handling_Encoding_Chez.md) — the strings of the
  original port (UTF-8 and Cork); this branch uses byte strings instead
  ([03](03-runtime.md#31-byte-strings))
- [PARALLEL_COMPILER_PLAN.md](PARALLEL_COMPILER_PLAN.md) — the two passes of
  the compiler
- [benchmarks.md](benchmarks.md), [bench/](bench) — benchmarks and profiles
  of the original port
