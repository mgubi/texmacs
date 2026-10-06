# 7. Performance: femtolisp, s7 and Guile

## 7.1 Setup

- **When:** 2026-10-06, macOS on Apple silicon, Qt offscreen
  (`QT_QPA_PLATFORM=offscreen`). The machine was moderately loaded (load
  average 3.3–3.8), so absolute times are a little high.
- **Builds:**
  - femtolisp: this branch (`wip_femto`, e3c05b2406);
  - s7: a `wip_s7` build of 2026-10-05 (s7 11.9 and its patches); it
    predates the fix of the heap growth of patch 0005 (see the s7 notes), so
    its memory figures are too high;
  - Guile: a `wip_s7` build configured `--with-scheme=guile` (Guile 1.8.7).
- **Same Scheme code:** the three binaries run the same `TeXmacs/` directory,
  the one of this branch, each with its own fresh home.
- **How:** the scripts of `docs/s7/bench`, run by
  [`bench/run.sh`](bench/run.sh), which alternates the three builds on each
  workload. Ranges are over the rounds (boot 5, suites 3, LaTeX 3,
  conversions and marshal 1, manual 2 warm rounds).

## 7.2 Summary

| Workload | femtolisp | s7 | Guile |
|---|---:|---:|---:|
| Boot to exit, `-x '(exit 0)'` | 1.56–1.63 s | 0.86–0.92 s | 1.71–1.75 s |
| 15 portable regression suites (time inside TeXmacs) | 298–324 ms | 205–214 ms | 352–364 ms |
| the same, whole process (boot, loading the test modules) | 3.19–3.21 s | 1.33–1.43 s | 3.04–3.09 s |
| 8 warm LaTeX exports of the change log | 1.81–1.84 s | 1.91–3.14 s | 6.62–6.92 s |
| Regenerating the manual (124 pages), warm | 5.01–5.30 s | 4.58–4.64 s | 5.67–5.91 s |
| Peak memory: boot | 155 MB | 217 MB | 136–152 MB |
| Peak memory: LaTeX exports | 233–248 MB | 1.3–4.3 GB (see 7.1) | 229–240 MB |
| Peak memory: manual | 419–434 MB | 647–649 MB | 402–409 MB |

What this shows:
- **Running Scheme code, femtolisp is between s7 and Guile:** the fastest of
  the three on the LaTeX export (a Scheme-heavy conversion), close to s7 on
  the other conversions, about 1.5× slower than s7 on the regression suites,
  and faster than Guile on all of them.
- **Loading Scheme code is slow:** femtolisp compiles every form it loads, so
  boot and the first use of a module cost about twice as much as with s7
  (about the same as Guile).
- **Memory is close to Guile's,** well below s7's.

## 7.3 Document conversions

`conversions.scm`: each task once cold, then the median of three warm runs,
in ms (one run per build).

| Task | femto cold | s7 cold | Guile cold | femto warm | s7 warm | Guile warm |
|---|---:|---:|---:|---:|---:|---:|
| load the 4 documents | 206 | 196 | 220 | 14 | 14 | 13 |
| tree → stree → tree | 17 | 9 | 9 | 11 | 9 | 10 |
| export LaTeX | 2781 | 2893 | 5312 | 742 | 1286 | 2757 |
| export HTML | 5581 | 5875 | 6352 | 3133 | 3266 | 3930 |
| import LaTeX | 789 | 653 | 1667 | 444 | 362 | 321 |
| import HTML | 457 | 709 | 1125 | 320 | 444 | 1045 |
| menu expansion ×10 | 27 | 22 | 28 | 25 | 18 | 31 |

Peak memory: femtolisp 755 MB, Guile 784 MB (s7: 4.8 GB, see 7.1).

## 7.4 Regenerating the manual, by phase

Warm rounds (the first round, in a fresh home, spends about 12 s building
font caches with every build).

| Phase | femtolisp | s7 | Guile |
|---|---:|---:|---:|
| tmdoc expansion | 0.94–1.00 s | 0.72–0.75 s | 1.49–1.70 s |
| first update | 2.47–2.53 s | 2.29–2.34 s | 2.62–2.66 s |
| second update | 0.79–0.90 s | 0.76–0.78 s | 0.77–0.80 s |
| third update | 0.80–0.87 s | 0.78–0.81 s | 0.79–0.82 s |

Typesetting dominates; the differences come from the expansion (Scheme) and
the first update (loading the modules used by the auxiliary data).

## 7.5 The C++ ↔ Scheme boundary

`marshal.scm`, ns per call:

| | femtolisp | s7 | Guile |
|---|---:|---:|---:|
| string → tree, 10 bytes | 88 | 160 | 506 |
| tree → string, 10 bytes | 116 | 44 | 523 |
| tree → string, 100000 bytes | 5025 | 35176 | 834171 |
| glue call `string-alpha?` | 40 | 21 | 297 |
| Scheme primitive `string-length` | 34 | 7 | 287 |
| tree → stree, change log (3381 nodes) | 1.60 ms | 1.30 ms | 1.36 ms |
| stree → tree, change log | 1.42 ms | 1.36 ms | 1.50 ms |

A glue call costs about twice s7's: each argument becomes a `tmscm`, which
links itself into the list of GC roots, and the call runs inside a C++
`try` block. Strings are copied once in each direction.

## 7.6 Where the load time goes

- **Compilation.** Every top-level form of every loaded file is expanded and
  compiled to bytecode by femtolisp's compiler, itself written in Lisp. s7
  analyses only the code that runs.
- **Module scanning.** Each module file is read whole and scanned for its
  private definitions before it is compiled; `resolve-global` is called for
  every global name compiled.
- **Kept sources.** Each compiled lambda keeps its source (for
  `procedure-source`), which costs memory more than time.

Ideas, not tried yet:
- compile the bodies of top-level functions lazily, at their first call;
- cache the compiled bytecode of the kernel modules on disk;
- keep the sources only for the lambdas of menus and keyboard bindings.

## 7.7 Notes on the runs

- The s7 and Guile builds are older C++ than this branch: with this
  `TeXmacs/` directory they fail one check of `tm-convert` (an escape of
  `object->string` which newer C++ unescapes); this does not affect the
  timings.
- One of the 5 Guile boots ended with a bus error at exit (its time, 1.75 s,
  is in the range); another took 2.76 s and is left out of the range as an
  outlier.
