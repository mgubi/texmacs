# 7. Performance: femtolisp, s7 and Guile

## 7.1 Setup

- **When:** 2026-10-06 evening, macOS on Apple silicon, Qt offscreen
  (`QT_QPA_PLATFORM=offscreen`). Other sessions were running: the load
  average was about 4, and went up to 8 during part of the first run, so the
  workloads were run twice and the clean runs are reported.
- **Builds:**
  - femtolisp: this branch, with the cache of compiled files (§7.6) warm;
  - s7: a `wip_s7`-based build of 2026-10-06 (s7 11.9 and its patches);
  - Guile: a `wip_s7` build configured `--with-scheme=guile` (Guile 1.8.7).
- **Same Scheme code:** the three binaries run the same `TeXmacs/` directory,
  the one of this branch, each with its own home.
- **How:** the scripts of `docs/s7/bench`, run by
  [`bench/run.sh`](bench/run.sh), which alternates the three builds on each
  workload: boot 5 rounds, suites and LaTeX 3, conversions and marshal 1,
  manual 2 (the first round in a new home builds the font caches: only the
  second round is compared).

## 7.2 Summary

| Workload | femtolisp | s7 | Guile |
|---|---:|---:|---:|
| Boot to exit, `-x '(exit 0)'` | 0.93–1.05 s | 0.88–0.94 s | 1.52–1.62 s |
| 15 portable regression suites (time inside TeXmacs) | 196–210 ms | 223–233 ms | 347–397 ms |
| 8 warm LaTeX exports of the change log | 1.78–1.84 s | 2.30–2.37 s | 6.87–8.39 s |
| Regenerating the manual (124 pages), warm | 4.71 s | 4.61 s | 5.80 s |
| Peak memory: boot | 160 MB | 217 MB | 142 MB |
| Peak memory: LaTeX exports | 238–251 MB | 299–305 MB | 213–243 MB |
| Peak memory: manual | 431 MB | 501 MB | 429 MB |

What this shows:
- **femtolisp and s7 are close**, and both are much faster than Guile.
  femtolisp is the fastest on the Scheme-heavy work (the LaTeX export, the
  conversions, the regression suites); s7 boots a little faster and converts
  values for C++ a little faster.
- **Memory:** femtolisp uses about as much as Guile, less than s7.
- **Boot depends on the cache.** Without the cache of compiled files (first
  boot after an installation or an upgrade), femtolisp compiles every
  loaded form: about 0.45 s more (§7.6).

## 7.3 Document conversions

`conversions.scm`: each task once cold, then the median of three warm runs,
in ms (one run per build, during the busier part of the session).

| Task | femto cold | s7 cold | Guile cold | femto warm | s7 warm | Guile warm |
|---|---:|---:|---:|---:|---:|---:|
| load the 4 documents | 254 | 202 | 279 | 14 | 15 | 14 |
| tree → stree → tree | 11 | 9 | 10 | 17 | 10 | 10 |
| export LaTeX | 1462 | 3358 | 6516 | 851 | 1027 | 3114 |
| export HTML | 4483 | 6323 | 6631 | 3232 | 3520 | 4231 |
| import LaTeX | 558 | 714 | 2096 | 382 | 469 | 462 |
| import HTML | 395 | 683 | 1388 | 334 | 646 | 1271 |
| menu expansion ×10 | 31 | 37 | 77 | 23 | 15 | 32 |

Peak memory: femtolisp 763 MB, s7 971 MB, Guile 776 MB. The cold runs
include loading the converter modules, which the cache makes cheap.

## 7.4 Regenerating the manual, by phase (warm)

| Phase | femtolisp | s7 | Guile |
|---|---:|---:|---:|
| tmdoc expansion | 0.79 s | 0.76 s | 1.67 s |
| first update | 2.34 s | 2.31 s | 2.59 s |
| second update | 0.79 s | 0.76 s | 0.77 s |
| third update | 0.79 s | 0.77 s | 0.78 s |

## 7.5 The C++ ↔ Scheme boundary

`marshal.scm`, ns per call:

| | femtolisp | s7 | Guile |
|---|---:|---:|---:|
| tree → string, 10 bytes | 116 | 50 | 534 |
| glue call `string-alpha?` | 41 | 40 | 307 |
| Scheme primitive `string-length` | 23 | 13 | 293 |

A glue call costs as much as with s7. Converting values is a little slower:
each `tmscm` links itself into the list of GC roots.

## 7.6 What made it faster

The first measurements gave femtolisp a boot of 1.6 s (s7 0.9 s) and slower
regression suites than s7. A profile of the boot (`TEXMACS_FL_PROFILE=1`,
§7.8) showed that half of it was the front end: 0.52 s compiling and
0.17 s expanding the 5894 forms of the loaded files.

- **Cache of the compiled files** (`boot-femtolisp.scm`). For each loaded
  file, `$TEXMACS_HOME_PATH/system/cache/femtolisp/` keeps a fingerprint of
  each expanded form (`%fingerprint` in `fl_core.c`, 128 bits of hash of its
  structure) and its compiled code. The forms are still expanded at each
  load, since expansions may have side effects; the compiled code of a form
  is reused when the fingerprint of its expansion is the cached one (the
  first version kept the expansions themselves: 55% of the cache, read only
  to be compared). The key of a cache file is
  the compiler (a checksum of the boot image), the TeXmacs version, the
  format of the cache, the module and its private names. Forms holding
  values which cannot be written and read back (uninterned symbols, tables,
  TeXmacs objects...) are compiled at each load; there were 26 of them at
  boot. Compilation then takes 0.1 s instead of 0.52 s.
  `TEXMACS_FL_NO_CACHE=1` disables the cache.
- **The reader no longer collects garbage while reading vectors** (patch
  0021). Growing a vector called the collector at each step, to update the
  references to the old one; with the heap of TeXmacs this made reading the
  cache 100 times slower than in a standalone femtolisp. A vector without a
  label is now made once its elements are read.
- **A heap of 32 MB at start** (`femtolisp_tm.cpp`, `TEXMACS_FL_HEAP` in
  MB): with 8 MB, a boot collected the garbage 23 times instead of 6, about
  60 ms more; the resident memory at boot is 8 MB larger.
- **Primitives in C:** `symbol?` and `keyword?` (they built the name of the
  symbol at each call), `ahash-ref`/`hash-ref`, `string-length`; `char=?`
  and `string=?` without their n-ary loop for two arguments.

## 7.7 Interactive work

`bench/ui.scm` (workload `ui` of `run.sh`): the expansion of all menus with
their submenus (menu bar, toolbars, context menu), typing in text and in
math (each key as the event loop handles it, then typesetting), and opening
and typesetting the change log; once cold, then the median of three warm
runs. 2026-10-07, load average about 4, three rounds alternated:

| Task | femtolisp | s7 |
|---|---:|---:|
| boot to exit | 0.88–0.92 s | 0.65–0.70 s |
| all menus, first time (loads their modules) | 2.2–2.5 s | 1.65–1.9 s |
| all menus, warm | 76–86 ms | 85–121 ms |
| 520 keys in text, warm | 775–825 ms | 723–826 ms |
| 270 keys in math, warm | 882–978 ms | 834–855 ms |
| open and typeset the change log, warm | 121–132 ms | 127–144 ms |

Once the code is loaded, femtolisp and s7 are as fast. femtolisp is slower
when code is loaded: it expands every loaded form (about 0.15 s at boot,
0.39 s more for the modules of the menus), whereas s7 expands a function
when it first runs. Two costs are common to all the Schemes: the first
expansion of the menus runs `kpsewhich` for each font which is not found
(about 0.86 s, `font-exists-in-tt?`), and `url-exists-in-path?` takes about
5 ms (the `:require` of the converters, at boot).

## 7.8 Measuring

- `TEXMACS_FL_PROFILE=1` and `(%profile-report)`: the time spent reading,
  expanding and compiling (or finding in the cache) the loaded files, and the
  cache hits; `(%profile-heads-report)`: the expansion time by head of the
  top-level forms.
- `(%gc-count)`: the number of garbage collections so far; `(%gc)`,
  `(%heap-size)`.
- The remaining front-end cost is the expansion (about 0.15 s at boot).
  Skipping it on a cache hit would need to know that a form's macros have no
  side effects when they are expanded (about 20 TeXmacs macros have some:
  `tm-define`, the `$` markup of menus, preferences...); expanding function
  bodies at their first call, as Guile and s7 do, would avoid most of it.
