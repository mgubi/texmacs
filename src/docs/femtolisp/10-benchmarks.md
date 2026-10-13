# 10. Benchmarks of 2026-10-07 (branch `maxs_femto`)

What was measured to decide how to make femtolisp faster, and what was kept.
The earlier measurements (other branches, Qt builds) are in
[07](07-performance.md), [08](08-lazy-bodies.md) and [09](09-browser.md).

## 10.1 Setup

- macOS on Apple silicon, other work running (the load average is given
  where it matters). The builds were alternated on each workload, so that
  two numbers of a row saw the same machine; the rows of two tables are not
  to be compared.
- **Native:** the Vue build of this branch with femtolisp
  (`--with-gui=vue --with-scheme=femtolisp`), and the S7 build of
  `maxs_texmacs`, both run with `-headless` on the `TeXmacs/` directory of
  this branch, each with its own home, warmed by a first run.
- **Browser:** the pages `build-wasm/out-femtolisp/web` and `build-wasm/out/web`
  (S7), built from this tree, in headless Firefox
  (`misc/wasm/browser-run.mjs`). The start is the time the page reports
  ("TeXmacs: running", at its first drawing, since the page was opened). A
  first visit has a new profile of the browser, the later visits keep one.
- **Workloads:** boot (`-x '(exit 0)'`); 8 warm exports of the change log to
  LaTeX (`docs/s7/bench/latex-loop.scm`); the interactive work of
  `bench/ui.scm` (all menus, typing, opening a document), called directly:
  with `-headless`, `exec-delayed` does not run.

## 10.2 Where femtolisp stands

| | femtolisp | S7 |
|---|---:|---:|
| native boot | 1.23–1.28 s | 1.10–1.19 s |
| 8 exports to LaTeX | 1.49 s | 2.13 s |
| all menus, first time | 2.2–2.3 s | 2.2–2.6 s |
| menus, typing, opening a document, warm | same | same |
| browser, first visit | 3.05–3.24 s | 2.77–2.85 s |
| browser, later visits | 1.16–1.19 s | 1.03–1.07 s |

femtolisp is faster than S7 where Scheme does the work, the same in the
interactive work, and slower when code is loaded (the starts).

## 10.3 Where the time goes

The program sampled every millisecond (`sample`), its busy time by kind of
function:

| | TeXmacs C++ and system | Scheme interpreter | Scheme GC |
|---|---:|---:|---:|
| interactive work, femtolisp (15.3 s) | 95% | 4% | 1% |
| interactive work, S7 (15.3 s) | 98% | 1% | 1% |
| LaTeX exports, femtolisp (4.1 s) | 62% | 30% | 8% |
| LaTeX exports, S7 (4.0 s) | 64% | 18% | 18% |

(the functions are sorted by their names, which is approximate: a few
percent of C++ may count as Scheme)

- The interactive work is the C++ of TeXmacs (typesetting, fonts, files:
  `stat` alone is 7%): a faster Scheme does not show there.
- In the exports, femtolisp spends more in its interpreter and less in its
  collector than S7.
- (Sampling slows femtolisp more than S7: the exports took 2.28 s against
  1.84 s under the sampler, 1.78 s against 2.25 s without. Times are taken
  without it.)

What femtolisp does when it loads code, at a native boot (6369 forms, with
`TEXMACS_FL_PROFILE=1`):

| | with the caches | without (`TEXMACS_FL_NO_CACHE=1`) |
|---|---:|---:|
| reading the source files | 27 ms | 29 ms |
| expanding the top-level forms | 53–66 ms | 71 ms |
| compiling them, or finding them in the cache | 56 ms (39 ms reading it) | 200 ms |
| first calls of function bodies (9240 stubs, 1520 called) | 44–81 ms | 134 ms |
| **total** | **about 200 ms** | **about 430 ms** |

The macros which cost the most in the expansion of the top-level forms:
`speech-symbols` 14 ms for 2 forms (21 ms with `speech-reduce` and
`speech-map`), `tm-define` 9 ms for 2332 forms, `define-group` 8 ms,
`lazy-define` 6 ms.

Two costs are not those of femtolisp (every Scheme pays them):
`url-exists-in-path?` takes about 5 ms a call (the `:require` of the
converters: about 110 ms at boot), and the first opening of the menus runs
`kpsewhich` for each font which is not found (about 0.86 s,
`font-exists-in-tt?`).

## 10.4 The heap at start

8 exports to LaTeX, by the size of the heap at start (each half of the
copying collector; `TEXMACS_FL_HEAP`):

| | time | peak memory |
|---|---:|---:|
| 32 MB | 1.77–1.79 s | 162–165 MB |
| 48 MB | 1.52–1.57 s | 199–203 MB |
| 64 MB | 1.48–1.52 s | 233–236 MB |
| 128 MB | 1.43–1.48 s | |
| 256 MB | 1.38–1.39 s | |

S7 starts in TeXmacs with 1,024,000 cells of 48 bytes (49 MB) and 16 MB of
pointers to them, and takes about 91 MB once booted. **Kept: 48 MB**, which
has most of the gain; a boot collects the garbage 6 times (23 times with
the 8 MB of the first version).

## 10.5 The caches of compiled code

With all the caching removed from the code (not only disabled), against the
caches as they are, alternated with S7:

| | with the caches | without | S7 |
|---|---:|---:|---:|
| native boot | 1.23 s | 1.43–1.47 s | 1.10–1.19 s |
| all menus, first time (native) | 2.3 s | 2.3 s | 2.2–2.6 s |
| 8 exports to LaTeX | 1.49 s | 1.55–1.60 s | 2.13–2.45 s |
| browser, later visits | 1.16–1.19 s | 1.54–1.57 s | 1.05–1.12 s |
| browser, first visit | 3.05–3.24 s | 3.54–3.64 s | 2.93–3.10 s |

- The caches are worth 0.2 s at a native boot, 0.4 s at each start in the
  browser and 0.5 s at the first one: without them, femtolisp starts 45–50%
  later than S7 in the browser, with them 10% later.
- They take about 250 lines of `boot-femtolisp.scm` (168 for the forms, 76
  for the function bodies), 90 lines of C (`%fingerprint`), the step of the
  browser build which makes the shipped cache (0.9 MB more to download with
  brotli), and files in the home (about 9 MB).
- **Kept**, since the start in the browser is what a user sees first.
- The cache shipped in the page: the first visit starts in 3.1 s instead of
  4.2 s (there, without it, the cache is also written).
- The compiled function bodies in a file for each source file, instead of
  one for all: the first calls of a boot take 44 ms instead of 73 (the one
  file was read whole, 3.1 MB for the 3831 bodies ever compiled, of which a
  boot calls 1500), boot 1.28 s instead of 1.34 s.

## 10.6 What was tried and left out

- **`-O3` for `fl_core.c`:** slower (1.56–1.60 s against 1.47–1.53 s for the
  exports with `-O2`).
- **A binary format of the caches** (the text is read at about 78 MB/s; a
  boot spends about 80 ms reading them): some hundred lines of C in the
  interpreter, for about 50 ms.
- **Not expanding the forms found in the cache:** needs to know which macros
  have no side effects when they expand (about 20 of TeXmacs have some),
  for about 60 ms.

Both were left out to keep the interpreter simple. The rest of the gap at
boot (about 0.1 s natively) is the expansion of the top-level forms and the
reading of the caches.

## 10.7 A mistake in the measurements, to avoid

A first comparison of the page with and without the caches found no
difference (1.19 s both). The runs "without" had the caches: the shell
function which ran `browser-run.mjs` passed `--query ?env=...` as one word
(zsh does not split an unquoted variable), which the tool ignored. The
rows of §10.5 come from a page built without the caching code. An option
which changes what is measured should be checked in the page itself:
`TeXmacs.scheme("%cache?")`.

## 10.8 The size of the code, against S7

In this tree (lines of source; the compiled objects of the browser build and
of the native one, arm64):

| | S7 | femtolisp |
|---|---:|---:|
| the interpreter | 106,173 lines (`s7.c`, `s7.h`: 4.2 MB) | 16,496 lines (0.47 MB) |
| its interface with TeXmacs, C and C++ | 653 lines | 1,742 lines |
| its Scheme layer in `TeXmacs/progs` | 734 lines | 2,146 lines |
| **total** | **107,560 lines** | **20,384 lines** |
| local patches of the interpreter | 6 | 22 |
| objects of the interpreter, WebAssembly | 2,582 KB | 395 KB |
| objects of the interpreter, native | 2,612 KB | 432 KB |
| the program of the page (`texmacs.wasm`) | 23.10 MB | 22.39 MB |

- The interpreter of femtolisp: its C core (`femtolisp/*.c`, `*.h`, 8,777
  lines), its library `llt` (5,776 lines), and its compiler and standard
  library in Lisp (`system.lsp`, `compiler.lsp`, 1,943 lines), with the boot
  image made from them (`flisp.boot`, 49 KB; `fl_boot.h`, 167 KB of C).
- The interface: `s7_tm.cpp`, `s7_tm.hpp` for S7; `femtolisp_tm.cpp`,
  `femtolisp_tm.hpp`, `fl_core.c`, `fl_llt.c`, `fl_tm.h` for femtolisp (the
  strings, the roots of the garbage collector, the errors, some primitives).
- The Scheme layer: `init-s7.scm`, `boot-s7.scm`, `compat-s7.scm` for S7;
  for femtolisp `init-femtolisp.scm` (30 lines), `r5rs-femtolisp.scm` (700:
  R5RS), `compat-femtolisp.scm` (594: the functions of Guile) and
  `boot-femtolisp.scm` (822: the modules, the lazy function bodies, the
  caches).

femtolisp is a sixth of S7 in source and a seventh once compiled. It needs
three times as much code to fit TeXmacs: S7 has most of what the Scheme code
of TeXmacs expects, femtolisp is a much smaller language, to which R5RS, the
functions of Guile and the modules are added. The patches do not compare by
their number: those of S7 fix a large interpreter which fits as it is, those
of femtolisp also add what TeXmacs needs and the language had not (the
reader and the printer of Guile, the hooks for the modules and for the roots
of the collector, comparisons with any number of arguments). The page is
0.7 MB smaller with femtolisp, 3% of it: most of it is TeXmacs.
