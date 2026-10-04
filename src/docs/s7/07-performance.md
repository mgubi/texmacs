# 7. Performance: s7 versus Guile

## 7.1 Setup

- **When:** 2026-09-26, on macOS (Apple silicon).
- **What was compared:**
  - s7: s7 11.9 unmodified;
  - Guile: Guile 1.8.7.
- **Same code:** both builds come from the same tree and run the same
  `TeXmacs/` directory, offscreen (`QT_QPA_PLATFORM=offscreen`).
- **How:**
  - The builds alternated within one session, three rounds for the short
    workloads and two for the manual.
  - Every run was capped at 1.5–2.5 GB RSS and a timeout.
  - The machine was moderately loaded (load average about 4), so absolute
    times are a little high. The comparisons are sound.
- **Scripts:** they are in `docs/s7/bench` (see §7.8).

## 7.2 Summary

| Workload | s7 | Guile | Guile / s7 |
|---|---:|---:|---:|
| Boot: window ready (§7.3) | about 0.7 s | about 1.9 s | 2.7× |
| Boot: fully started, with deferred work (§7.3) | 1.35–1.69 s | 3.72–3.99 s | 2.6× |
| 15 portable regression suites | 190–208 ms | 277–282 ms (one run at 372) | 1.4× |
| 8 warm LaTeX exports of the change log | 2.81–2.98 s | 7.80–8.80 s | 2.9× |
| Regenerating the manual (124 pages) | 4.55–4.59 s | 5.61–5.87 s | 1.25× |
| Peak memory, LaTeX exports | 292–300 MB | 207–223 MB | 0.7× |
| Peak memory, manual | 488–490 MB | 407–408 MB | 0.8× |

What this shows:
- **s7 is faster on everything measured,** and much faster wherever code is
  loaded or interpreted: boot, the first use of lazily loaded modules, and
  Scheme-heavy conversions.
- **Where C++ dominates, the two are at parity:** typesetting, parsing.
- **s7 uses more memory** (§7.7).

### Regenerating the manual, by phase

`tmdoc-expand-help` builds the book; each update then generates the
auxiliary data (table of contents, index, references) and retypesets.

| Phase | s7 | Guile |
|---|---:|---:|
| tmdoc expansion | 0.73–0.81 s | 1.46–1.52 s |
| first update | 2.23–2.31 s | 2.63–2.76 s |
| second update | 0.75–0.77 s | 0.75–0.82 s |
| third update | 0.76–0.78 s | 0.77–0.78 s |

In this workload typesetting takes about 86% of the time, and Scheme about
16%. That includes the C++ that Scheme calls (loading buffers, generating
the auxiliary data).

### Document conversions

`conversions.scm` works on four documents (the change log, `env-page`,
`bigtable-test`, `superscript-test-bis`). It runs each task once cold, then
three times warm, and reports the median of the warm runs. Times are in ms,
from one run per interpreter.

| Task | s7 cold | Guile cold | s7 warm | Guile warm |
|---|---:|---:|---:|---:|
| load the 4 documents | 187 | 210 | 14 | 13 |
| tree → stree → tree | 9 | 9 | 10 | 9 |
| export LaTeX | 2343 | 3991 | 1255 | 2486 |
| export HTML | 4390 | 4963 | 3299 | 3800 |
| import LaTeX | 603 | 1600 | 320 | 307 |
| import HTML | 681 | 918 | 633 | 950 |
| menu expansion ×10 | 37 | 29 | 16 | 29 |

The cold runs include loading the converter modules, which s7 does much
faster. Peak memory was 966 MB on s7 and 817 MB on Guile.

<a id="boot"></a>
## 7.3 Boot

Starting TeXmacs has two parts:
- **Up to the first idle moment:** the Scheme init files run, the first
  window is created, and the empty document is typeset and painted.
- **After it:** deferred work runs in the background, at idle moments. This
  includes lazy keyboard modules, plugin initialization and the updater.

Quitting from a `-x` command stops at the first part, so three measures
were taken:
- **quit at once:** `-x '(quit-TeXmacs)'`;
- **window ready:** `-x '(delayed (:idle 100) (quit-TeXmacs))'`, minus the
  100 ms;
- **fully started:** `-x '(delayed (:idle 3000) (quit-TeXmacs))'`, minus the
  3 s, which includes the deferred work.

All three used a copy of a real `~/.TeXmacs` (preferences, font caches, a
user plugin). Three runs each, in wall time:

| | s7 | Guile |
|---|---:|---:|
| quit at once | 0.67–0.68 s | 1.78 s |
| window ready | about 0.7 s | about 1.96 s |
| fully started | 1.35–1.69 s | 3.72–3.99 s |
| fully started, CPU time | 1.40–1.99 s | 3.82–4.12 s |

Other things checked:
- **The first launch after copying the home directory** was slower
  (1.95 s on s7), as TeXmacs refreshed its caches.
- **The real macOS platform gives the same times as offscreen:** 0.53–0.57 s
  for quit at once and 0.77–0.86 s for window ready on s7, against 1.46–1.54
  and 1.81–1.88 s on Guile.
- **An almost empty scratch home is slower to settle** (2.1 s on s7, 4.4 s
  on Guile), because some first-start work is redone.

**Before the event loop,** TeXmacs's own timers (`texmacs.bin -debug-bench`)
give:
- Scheme initialization: 56–59 ms on s7 against 324 ms on Guile;
- plugins: 18 ms;
- the rest of the TeXmacs initialization: 50 ms.

A sample of the part up to the window shows:
- about 30% building a `QDockWidget`, because Qt's Fusion style loads its
  standard icons;
- about 25% in the Qt font database, including the family aliases for the
  missing "Sans Serif" family that Qt warns about;
- the rest in menus and toolbars (Scheme) and the first buffer.

**The deferred work is dominated by plugin detection.** Each plugin's
init file checks whether its program is installed with
`url-exists-in-path?`. `resolve_in_path` then runs `which <program>` in a
shell and waits for it, whenever `use_which` is set. It is set at startup
by running `which texmacs`, which always succeeds, since TeXmacs puts its
own `bin` directory on the `PATH`.
- With 39 plugins that check, this takes about 0.6 s of the s7 startup.
- It is upstream behavior, so Guile pays the same.
- `resolve_in_path` already has a direct search of `$PATH`, which doesn't
  spawn anything.

## 7.4 Where the LaTeX export spends its time

The export runs the Scheme converter `tmtex` on the document tree:
`tree_to_latex_document` takes 90% of the main thread. By self time:
- s7's `eval` takes 45%;
- the GC takes about 20%;
- C++ work takes a few percent.

**One query dominated until it was cached.** `latex-needs?` asks whether a
LaTeX macro needs a package. It is `(logic-ref latex-needs% x)`, a query of
the logic engine.
- `latex-symbol-drd.scm` adds rules such as
  `((latex-needs% 'x "amssymb") (latex-ams-symbol% 'x))`. Their head has a
  free variable, so every query tries them all.
- The converter asked this for every node, often twice: 42 562 calls in 8
  exports, for 71 distinct keys.
- The answers depend only on the logic rules, so `latex-needs?` now caches
  them until rules are added (`logic-rules-version`).
- **Effect:** 8 exports went from 4.7 s to 3.0 s on s7, and from about 9 to
  8.3 s on Guile.
- **Output:** the LaTeX exported from 21 documents is byte-identical with
  and without the cache, on both interpreters.

**What remains:**
- the `tmtex` conversion itself;
- a few similar logic-table lookups (`latex-texmacs-arity`,
  `latex-texmacs-option?`, the catcode definitions), which could be cached
  the same way;
- the GC.

## 7.5 Symbol lookup

With the module system of §2.2–2.3, lookups are a small share of the time.
A counting build of s7's lookup, over 8 LaTeX exports:
- about 0.24 G slot comparisons;
- mostly in small environments: function frames, and the private
  definitions of kernel modules.

Before, the exports were copied into a user module of about a thousand
bindings, and the user module was renumbered as modules loaded. The same
loop then made 5.8 G comparisons and took 9.2 s even with a patched s7.

Profiling showed that the rules of §2.3 are all needed. Without the one
renumbering after the kernel, the loop takes 6.4 s instead of 2.8 s.

<a id="crossing-the-boundary"></a>
## 7.6 Crossing the C++/Scheme boundary

From `marshal.scm`:

| | s7 | Guile |
|---|---:|---:|
| empty loop iteration with a Scheme primitive | 14 ns | 292 ns |
| same with a glue call (`string-alpha? "a"`) | 37 ns | 301 ns |
| string Scheme → C++, 10 B / 1 KB / 100 KB | 0.13 / 0.39 / 35 µs | 0.54 / 0.77 / 35 µs |
| string C++ → Scheme, 10 B / 1 KB / 100 KB | 0.05 / 0.39 / 40 µs | 0.54 / 8.6 / 925 µs |
| `tree->stree`, change log (3 381 nodes, 54 KB of text) | 1.20 ms | 1.32 ms |
| `stree->tree`, same | 1.34 ms | 1.46 ms |

- **On s7, a glue call costs about 20 ns,** and strings about 0.35 ns per
  byte.
- **Guile's own loop overhead dominates its small calls,** and its C++ →
  Scheme strings are much slower on long strings.
- **`tree->stree` and `stree->tree` cost about 400 ns per node** on both,
  because they go through an intermediate C++ tree with quoted strings
  (§1.7).

**How much this matters overall:**
- marshalling is about 3% of the LaTeX export;
- it is under 1% of the manual regeneration.

<a id="memory"></a>
## 7.7 Memory

**s7 peaks higher than Guile:**
- 300 MB against 210 MB for the LaTeX exports;
- 490 MB against 410 MB for the manual;
- 966 MB against 817 MB for the conversions.

**The heap follows s7's growth policy.**
- After a GC, the heap doubles if less than 80% of it is free, or
  quadruples if less than 67% is free (`gc-resize-heap-by-4-fraction`).
- TeXmacs starts s7 with 1 M cells (§1.3). With fewer cells the
  collections are more frequent: the test suites ran 5–10% slower.
- Setting `(*s7* 'gc-resize-heap-by-4-fraction)` lower avoids the jumps to
  a 4× larger heap, trading speed for memory.

**The module system of §2.2 also saves memory.** Copying every export into
the user module made peak memory on the LaTeX exports about 420 MB.

## 7.8 Reproducing

The scripts are in `docs/s7/bench`. Run each with
`texmacs.bin -x '(load "<path>")'`. They quit when done and write their
output files to `$TEXMACS_HOME_PATH/system/tmp/s7-bench`.

| Script | Measures |
|---|---|
| `suites.scm` | the 15 portable regression suites, one by one (`SUITES-TIME`) |
| `latex-loop.scm` | 8 warm LaTeX exports of the change log (`LOOP-DONE`) |
| `manual.scm` | regenerating the manual, by phase (`MANUAL …`) |
| `conversions.scm` | loading, converting and exporting four documents (`BENCH …`) |
| `marshal.scm` | the cost of crossing the C++/Scheme boundary (`MARSHAL …`) |

Measure boot with `time texmacs.bin -x …`, quitting from
`(delayed (:idle N) (quit-TeXmacs))` and subtracting `N` ms (§7.3). A plain
`(quit-TeXmacs)` stops before the deferred work.

- **Use a scratch home directory** (`TEXMACS_HOME_PATH`), and
  `QT_QPA_PLATFORM=offscreen` to run without a display.
- **Run the manual once before timing it.** The first run in a fresh home
  directory builds caches: it took 11–15 s instead of about 5.
- **Keep the machine quiet.** At a load average of 260, the same HTML
  export took 13 s cold instead of 4.4 s.

**The Guile build.** Configure it with `--with-scheme=guile` in a copy of
the tree, and point its binary at the same `TEXMACS_PATH`.

<a id="r7rs-benchmarks"></a>
## 7.9 Other Schemes on the R7RS benchmarks (2021)

These numbers explain the choice of s7. They were taken in January 2021 on
the `chez` branch, which held the first s7 port and an unfinished Chez
Scheme port; its `src/README.md` has the original table.

- **When:** 2021-01-06, on a MacBook Air (2019, Intel).
- **What was compared:** the s7 of that time, Chibi 0.9.1, Chez 9.5.1,
  Guile 1.8.8 and Guile 3.0.4.
- **How:** the standard suite of
  [ecraven/r7rs-benchmarks](https://github.com/ecraven/r7rs-benchmarks),
  outside TeXmacs.
- **Units:** seconds. `TIMELIM` is a run over the time limit; `NO` is a test
  which did not run (`pi` and `chudnovsky` need bignums, which that s7 build
  lacked).

What this shows:
- **Chez is the fastest by far.** It compiles to machine code. The median
  test takes 8× longer on s7, and the slowest ones 30–56× (`graphs`,
  `ctak`, `matrix`, `lattice`, `browse`).
- **s7 is much faster than Guile 1.8,** which hit the time limit on 33 of
  the 57 tests. The median test takes about 2× longer on s7 than on Guile 3.
- **Chibi is the slowest,** and often hits the time limit.
- **s7 is fastest of all on input and output, strings and floating point:**
  `read1`, `cat`, `tail`, `sum1`, `string`, `fibfp`, `sumfp`, `equal`.

TeXmacs spends most of its time in C++ (§7.2), so these ratios are an upper
bound on what a faster Scheme would change. s7 also builds for WebAssembly,
which Chez does not.

| test                           |      s7 | chibi-0.9.1 | chez-9.5.1 | guile-1.8.8 | guile-3.0.4 |
|--------------------------------|--------:|------------:|-----------:|------------:|------------:|
| browse:2000                    |   24.27 |     TIMELIM |       0.84 |       76.63 |       12.06 |
| deriv:10000000                 |   25.19 |       97.11 |       1.15 |       67.27 |       18.58 |
| destruc:600:50:4000            |   52.08 |       94.90 |       2.21 |     TIMELIM |        7.14 |
| diviter:1000:1000000           |    9.69 |       55.88 |       1.81 |       82.75 |       15.45 |
| divrec:1000:1000000            |   11.80 |       50.91 |       2.19 |       82.04 |       17.41 |
| puzzle:1000                    |   27.72 |      270.98 |       1.66 |      221.49 |       18.09 |
| triangl:22:1:50                |   33.93 |      110.86 |       1.91 |      107.15 |        8.52 |
| tak:40:20:11:1                 |   12.93 |       55.19 |       1.66 |      134.21 |        4.76 |
| takl:40:20:12:1                |   20.97 |     TIMELIM |       3.71 |     TIMELIM |        9.46 |
| ntakl:40:20:12:1               |   17.07 |       95.98 |       3.65 |     TIMELIM |        9.52 |
| cpstak:40:20:11:1              |  103.36 |      222.13 |       4.21 |      258.62 |       59.44 |
| ctak:32:16:8:1                 |   44.14 |     TIMELIM |       0.96 |     TIMELIM |     TIMELIM |
| fib:40:5                       |   10.22 |       96.25 |       3.63 |      236.65 |       12.09 |
| fibc:30:10                     |   25.80 |     TIMELIM |       0.71 |     TIMELIM |     TIMELIM |
| fibfp:35.0:10                  |    1.89 |       42.79 |       3.18 |       56.26 |       22.00 |
| sum:10000:200000               |    6.64 |       99.79 |       3.59 |     TIMELIM |        6.87 |
| sumfp:1000000.0:500            |    2.50 |       85.36 |       4.25 |      111.78 |       42.06 |
| fft:65536:100                  |   32.20 |       69.76 |       3.32 |     TIMELIM |        7.69 |
| mbrot:75:1000                  |   24.40 |      209.51 |       5.39 |     TIMELIM |       50.09 |
| mbrotZ:75:1000                 |   18.56 |     TIMELIM |       9.26 |     TIMELIM |       67.01 |
| nucleic:50                     |   19.95 |       79.05 |       2.59 |       69.32 |       15.35 |
| pi                             |      NO |     TIMELIM |       0.60 |     TIMELIM |        0.56 |
| pnpoly:1000000                 |   17.98 |      253.84 |       4.73 |     TIMELIM |       24.89 |
| ray:50                         |   20.46 |      119.63 |       2.83 |     TIMELIM |       18.51 |
| simplex:1000000                |   46.34 |      182.76 |       2.34 |     TIMELIM |       13.90 |
| ack:3:12:2                     |   10.57 |       74.51 |       3.12 |     TIMELIM |        8.41 |
| array1:1000000:500             |   11.48 |       64.18 |       8.16 |      138.45 |        9.24 |
| string:500000:100              |    1.71 |        6.34 |       6.54 |        1.81 |        1.87 |
| sum1:25                        |    0.47 |      121.78 |       2.02 |        1.71 |        4.43 |
| cat:50                         |    1.19 |       70.95 |       2.69 |     TIMELIM |       28.40 |
| tail:50                        |    1.19 |       11.21 |       3.57 |     TIMELIM |        9.82 |
| wc:inputs/bib:50               |    8.27 |     TIMELIM |       1.76 |       73.34 |       16.96 |
| read1:2500                     |    0.41 |      281.23 |       1.28 |        2.69 |        5.80 |
| compiler:2000                  |   41.16 |      115.83 |       2.99 |     TIMELIM |        5.15 |
| conform:500                    |   51.03 |      199.42 |       2.10 |     TIMELIM |       10.51 |
| dynamic:500                    |   22.74 |      288.25 |       4.01 |       71.60 |        7.37 |
| earley                         | TIMELIM |     TIMELIM |       5.03 |     TIMELIM |        9.49 |
| graphs:7:3                     |  127.61 |     TIMELIM |       2.27 |     TIMELIM |       23.03 |
| lattice:44:10                  |  139.28 |     TIMELIM |       3.91 |     TIMELIM |       15.94 |
| matrix:5:5:2500                |   72.07 |      214.16 |       1.63 |     TIMELIM |        9.88 |
| maze:20:7:10000                |   23.26 |       84.15 |       1.66 |     TIMELIM |        4.70 |
| mazefun:11:11:10000            |   19.51 |      122.02 |       2.86 |      128.66 |        9.66 |
| nqueens:13:10                  |   55.11 |      165.25 |       4.99 |     TIMELIM |       19.37 |
| paraffins:23:10                |   31.42 |     TIMELIM |       5.97 |     TIMELIM |        4.25 |
| parsing:2500                   |   39.44 |     TIMELIM |       3.35 |     TIMELIM |       10.69 |
| peval:2000                     |   29.68 |      177.10 |       2.05 |      107.05 |       15.64 |
| primes:1000:10000              |    7.73 |       21.08 |       2.64 |       43.73 |        7.52 |
| quicksort:10000:2500           |   94.00 |     TIMELIM |       3.90 |     TIMELIM |       13.25 |
| scheme:100000                  |   71.46 |      154.36 |       2.55 |     TIMELIM |       15.14 |
| slatex:500                     |   32.07 |      173.60 |       3.73 |       43.82 |       45.05 |
| chudnovsky                     |      NO |     TIMELIM |       0.32 |     TIMELIM |        0.31 |
| nboyer:5:1                     |   39.27 |       41.54 |       3.95 |      142.86 |        5.10 |
| sboyer:5:1                     |   31.54 |       39.14 |       1.30 |      155.49 |        4.76 |
| gcbench:20:1                   |   20.54 |     TIMELIM |       1.87 |     TIMELIM |        3.51 |
| mperm:20:10:2:1                |  173.33 |      659.14 |      13.33 |     TIMELIM |       10.65 |
| equal:100:100:8:1000:2000:5000 |    0.78 |       54.11 |       0.99 |     TIMELIM |     TIMELIM |
| bv2string:1000:1000:100        |   10.78 |       11.17 |       2.47 |     TIMELIM |        4.49 |
