# Profiling `conversions.scm`

This is a source-level profile of `docs/chez/bench/conversions.scm` (the file is
plural; there is no `conversion.scm`), plus a check of the step-by-step
`compile-profile` recipe against this Chez build. `bench/PROFILING.md` §6 rated
`compile-profile` as possible but not attempted; this is that attempt.

A readable HTML summary of the same data was produced during the profiling
session but is not checked into this tree. For the measured time budget and
optimization potential, see the sections below and
[`CHEZ_PERFORMANCE_ANALYSIS.md`](CHEZ_PERFORMANCE_ANALYSIS.md).

## Read this first

Five things this profile actually says:

1. **Export is the job.** LaTeX export takes ~1.7 s and allocates ~157 MB;
   HTML export takes ~6.1 s and allocates ~98 MB. Loading and importing are
   small by comparison.
2. **LaTeX export repeats cheap checks far too often.** The exporter asks
   "what environment/mode is this?" once per character in places, and tests 13
   environment names one at a time. That is the biggest visible block of work.
3. **HTML export keeps re-walking the same tree.** Title lookup, attribute
   tests, cell widths and vertical spacing are all separate scans of the same
   document. The big table makes this worse.
4. **Do not optimise `import-latex`/`import-html` Scheme.** `import-latex` is
   nearly all C++ parsing; `load-4-docs` has no Scheme work in `convert/**` at
   all. A Scheme profile cannot see the C++ parsers.
5. **`compile-profile` counts executions, not time or bytes.** `'bytes` is not
   a valid mode. The memory numbers here come from `sstats-bytes`; the counts
   say *which code runs*, the allocation numbers say *how much it conses*.

The rest of this document is the evidence and the reproduction recipe.

---

Measured on:

| | |
|---|---|
| Machine | Apple M1 Pro (Virtual), macOS 15.6.1, arm64 |
| Scheme | Chez Scheme **10.4.1** (Homebrew), `tarm64osx` |
| Binary | `build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs`, Release |
| Tree | `21d1e6c1a` |
| Qt | 6.12.0, `QT_QPA_PLATFORM=offscreen` |
| Modules | 453/453 compiled, 0 failures |

---

## 1. Does the step-by-step example work?

The demo itself works. Two details in the recipe are wrong, and one scope
limitation matters for `conversions.scm`.

| Step | Result | Detail |
|---|---|---|
| `(compile-profile 'source)` | **works** | Valid modes are `#f`, `#t`, `'source`, `'block`; `#t` is a synonym for `source`. |
| `(compile-profile 'bytes)` | **fails** | `Exception in compile-profile: invalid mode bytes [must be #f, #t, source, or block]`. Chez has no allocation/byte mode. |
| `(load "demo.ss")` | **works in TeXmacs** | The load is instrumented. In a plain Chez REPL the same sequence may not be: Chez interprets simple top-level forms unless `(compile-interpret-simple #f)` is set. |
| `(profile-clear)` | **works** | |
| `(run-work)` | **works** | |
| `(profile-dump-list)` | **works** | Returns `(count file start end line column)` entries, not `(count . location)`. |
| `(profile-dump-html)` | **works** | Writes `profile.html` plus one `<source>.html` per instrumented source into the current directory. |
| `for-each` printing `(car entry)` / `(cdr entry)` | **works** | `(cdr entry)` is the 5-element location `(file start end line column)`. |

Exact transcript in TeXmacs for the supplied `demo.ss`:

```
initial=#f
after-source=source
run-work=41667916675000
sites=98
first=(50001 /tmp/tmprof/demo.ss 56 133 3 5)
count=50001 location=(/tmp/tmprof/demo.ss 56 133 3 5)
bytes-mode-error: invalid mode ~s [must be #f, #t, source, or block]
```

`profile.html` and `demo.ss.html` were both produced.

The important limitation is **not** in the example's syntax but in its scope:
`(load "demo.ss")` instruments `demo.ss` itself. It does **not** instrument the
453 ahead-of-time compiled modules that `conversions.scm` calls
(`export-buffer-main`, `tree-import`, `tmhtml`, `tmtex`, ...). For that, the
modules must be recompiled with profiling enabled, which is what §2 does.

---

## 2. Building an instrumented boot tree

`compile-file` only emits profiling instrumentation when the `compile-profile`
parameter is set at compile time. Two environment variables were added to
`src/Scheme/Chez/compile-texmacs.scm`:

| Variable | Meaning |
|---|---|
| `TEXMACS_COMPILE_PROFILE` | `source` (or `#t`/`block`) enables instrumentation; unset/`0` keeps the production build untouched. |
| `TEXMACS_COMPILE_PROFILE_FILTER` | Optional module-path prefix; only matching modules are instrumented. |

The filter matters here. Instrumenting **all** 453 modules with `source` made
the export benchmark emit repeated `TM-APPLY-ERROR invalid syntax #&...` and
produced zero-byte `.tex` files. Instrumenting only `convert/` produced correct
exports, so all numbers below use `TEXMACS_COMPILE_PROFILE_FILTER=convert/`.

Rebuild into a separate boot tree, leaving `build-texmacos/boot` alone:

```bash
cd texmacs
rm -rf build-profile
mkdir -p build-profile
cp -a build-texmacos/boot build-profile/boot

QT_QPA_PLATFORM=offscreen \
SCHEMEHEAPDIRS=/opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1/tarm64osx \
HOME=$PWD/fakehome \
TEXMACS_PATH=$PWD/TeXmacs \
TEXMACS_BOOT_PATH=$PWD/build-profile/boot \
TEXMACS_COMPILE_PROFILE=source \
TEXMACS_COMPILE_PROFILE_FILTER=convert/ \
TEXMACS_RECOMPILE_ALL=1 \
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs -compile-modules
```

This recompiles the 453 modules but only `convert/**` carries counters. A
working profiled tree is present at `build-profile/boot` in this workspace.

A minimal per-task harness (warm once, clear, measure):

```scheme
(define (profile-task name thunk)
  (thunk)                          ; warm lazy initialization
  (profile-clear)
  (thunk)
  (call-with-output-file (string-append "/tmp/" name ".sexp")
    (lambda (p) (write (profile-dump-list) p) (newline p))))

(load-docs)
(profile-task "export-latex" (lambda () (export-all "latex" "tex")))
(profile-task "export-html"  (lambda () (export-all "html"  "html")))
(profile-dump-html)
```

Run it with `TEXMACS_BOOT_PATH=$PWD/build-profile/boot`.

---

## 3. Results

### 3.1 Baseline time and allocation

`conversions.scm` as shipped, 3 runs. Warm is the median of its own 3 warm
iterations; allocation is gross Scheme heap (`sstats-bytes`) on the **second**
call, after a warm-up. Wall time is noisy on this VM; allocation is stable.

| Task | Cold ms | Warm ms | Warm bytes | GCs |
|---|---:|---:|---:|---:|
| `load-4-docs` | 45 | 27 | 1.41 MB | 0 |
| `tree->stree->tree` | 43 | 32 | 3.79 MB | 1 |
| `export-latex` | 2 690 | **1 729** | **156.7 MB** | 18 |
| `export-html` | 8 076 | **6 063** | **98.1 MB** | 11 |
| `import-latex` | 611 | 827 | 0.87 MB | 0 |
| `import-html` | 348 | 328 | 59.6 MB | 7 |
| `menu-expand-x10` | 24 | 22 | 8.74 MB | 1 |

`import-latex` warm hits the importer cache; its first import allocates 27.0 MB
and 4 GCs. `export-latex` warm still allocates about 157 MB for four documents
(`~39 MB/doc`); `export-html` about 98 MB (`~25 MB/doc`). The benchmark is
dominated by export, not import.

### 3.2 LaTeX export

Warm profile: **31.4 M** counted expression evaluations, concentrated in four
modules:

| Module | Counted evaluations |
|---|---:|
| `convert/latex/tmtex.scm` | 16.90 M |
| `convert/latex/latex-tools.scm` | 6.67 M |
| `convert/latex/texout.scm` | 3.83 M |
| `convert/tools/output.scm` | 2.46 M |
| `convert/rewrite/tmtm-brackets.scm` | 0.76 M |
| `convert/tools/tmpre.scm` | 0.63 M |

Dynamic call counts (summed over call sites; special forms excluded):

| Calls | Callee | Calls | Callee |
|---:|---|---:|---|
| 1 880 163 | `==` | 82 541 | `tmtex-env-list` |
| 659 668 | `func?` | 82 397 | `tmtex-env-get` |
| 599 238 | `car` | 80 928 | `tmtex-math-mode?` |
| 304 712 | `cdr` | 67 479 | `tmtex-text-list` |
| 251 287 | `null?` | 61 512 | `latex-stree-contains?` |
| 192 120 | `nlist?` | 57 296 | `macro-definition?` |
| 182 545 | `list>0?` | 54 720 | `string->list` |
| 169 455 | `with` | 53 080 | `env-begin?` |
| 151 788 | `env?` | 40 212 | `tmtex-filter-preamble` |
| 137 370 | `in?` | 38 305 | `latex-use-which-package` |

The hottest small predicate is `env?`
(`convert/latex/latex-tools.scm:122`):

```scheme
(define (env? t x)
  (and (list>0? t) (func? (car t) '!begin) (list>1? (car t)) (== x (cadar t))))
```

It is called 151 788 times, almost all from `latex-is-math?`, which tests 13
environment names one after another. Each test re-walks the same list head.
That is ~13 redundant scans per math check.

Next is the character path. `tmtex-text-list` (67 479 calls) converts every
text string with `string->list` and then calls `tmtex-special-char?` per
character; `tmtex-string-break?` (71 914 calls) calls `tmtex-math-mode?` per
character, which does `tmtex-env-get` → `tmtex-env-list` → `ahash-ref` (82 000+
hash lookups). The character path is the largest single block of counted work;
the 157 MB allocation is consistent with it, although `compile-profile` cannot
attribute bytes to it directly.

The rest is repeated whole-tree filtering: `compute-mode-stats` (21 289),
`latex-macro-defs-sub` (20 873), `tmtex-filter-preamble` (40 212),
`tmtex-filter-body` (21 289), `tmtm-brackets` (12 289), plus
`latex-stree-contains?` (61 512) and `contains-tags?` (109 940 in the cold run).
Each pass walks the whole document for one predicate.

### 3.3 HTML export

Warm profile: **11.88 M** counted evaluations.

| Module | Counted evaluations |
|---|---:|
| `convert/html/tmhtml.scm` | 5.38 M |
| `convert/tools/output.scm` | 2.66 M |
| `convert/html/htmlout.scm` | 1.61 M |
| `convert/tools/tmtable.scm` | 0.86 M |
| `convert/tools/sxml.scm` | 0.72 M |
| `convert/tools/tmlength.scm` | 0.34 M |
| `convert/tools/tmconcat.scm` | 0.30 M |

Dynamic call counts:

| Calls | Callee | Calls | Callee |
|---:|---|---:|---|
| 302 497 | `func?` | 57 122 | `symbol->string` |
| 219 189 | `car` | 50 782 | `tmhtml-find-title` |
| 185 071 | `==` | 34 982 | `output-raw` |
| 175 490 | `cdr` | 33 593 | `output-sub` |
| 141 694 | `with` | 27 576 | `tmindex-get` |
| 100 924 | `null?` | 21 147 | `htmlout-big-tag?` |
| 91 586 | `pair?` | 19 210 | `htmlout-text` |
| 88 448 | `not` | 15 048 | `tmhtml-compute-vspace` |
| 84 013 | `cons` | 13 389 | `tmhtml-width-part` |
| 73 686 | `!=` | 10 799 | `tmhtml-make-cell-attr` |

`tmhtml-find-title` (`convert/html/tmhtml.scm:128`) is a full recursive
`car`/`cdr` walk that is invoked once per document but evaluates its `cond`
50 782 times across the four files. It is O(document) just to locate a title.

`sxml-has-attr-list?` is called 39 958 times; `sxml-element-head` and
`sxml-content` each call it, so the same element is tested repeatedly. It is
only `(pair? (cdr e))`, `(pair? (cadr e))`, `(eq? '@ (caadr e))`.

`tmhtml-make-cell-attr` is a 20-branch `cond` over string pairs, called 10 799
times for the big table. `tmhtml-width-part` is a linear scan of each cell's
attribute list; `tmhtml-compute-vspace` and `tmhtml-breaks-post` are similar
per-element scans. `output-raw`/`output-sub` account for 68 575 calls and much
of the string `cons`/`append` traffic.

### 3.4 Other tasks

- `load-4-docs`, `tree->stree->tree`, and `menu-expand-x10` execute **zero**
  counted expressions inside `convert/**`. Their time is in C++ (parsing,
  typesetting) or uninstrumented kernel modules. Profiling conversion Scheme
  will not explain them.
- `import-latex` is almost entirely C++ parsing. Its cold profile has 37 225
  counted evaluations, all in `convert/latex/latex-drd.scm`;
  `safe-string2symbol` alone accounts for 36 471 of them (2 262 calls). Warm
  import drops to 120 counted evaluations because the importer cache is hit.
  There is no large Scheme hotspot to optimize here.
- `import-html` was not profiled to completion: with source instrumentation the
  parser did not finish within 15 minutes and was killed. Allocation is 59.6 MB
  and 7 GCs per warm run, so it is worth profiling with a cheaper mode
  (`'block`) or a C++ profiler, but this run gives no Scheme call counts.

### 3.5 Are `==` and `func?` inlined?

No, not in this build.

- `==` is a real exported procedure, not a macro or alias:
  `kernel/boot/abbrevs.scm:20` defines `(define-public (== x y) (equal? x y))`.
  At the build's `optimize-level` of **0**, Chez performs no procedure inlining.
  `expand/optimize` leaves `(== x y)` untouched at levels 0, 1, 2 and 3.
  A *local* `letrec` procedure is inlined to `(equal? x y)` at level 2, which
  shows the compiler can inline when it has the body; imported module procedures
  have no body to inline.
- `func?` is **not** `procedure?`. The real definition is
  `kernel/library/base.scm:264`: it tests `(list? x)`, a non-empty list, and
  `(eq? (car x) f)`, with an optional length check. The line
  `(define (func? p kind) (procedure? p))` in `init-texmacs-chez.scm` was a
  compatibility stub that was overwritten when the kernel library loaded; it
  has been removed. At runtime, `(func? (lambda (x) x) 'a)` is `#f` and
  `(func? '(a b) 'a)` is `#t`.
- Raising the build to `optimize-level` 2 does not change this. Level 2 inlines
  primitives and procedures whose bodies are in the same compilation unit; it
  did not inline the imported `==` or `func?` (same `expand/optimize` output at
  levels 2 and 3). It can speed up the *bodies* slightly, but the wrapper calls
  remain.

Measured cost in TeXmacs (CPU time, 5–10 M iterations, globals to prevent
constant folding):

| Expression | ns/call | bytes/call |
|---|---:|---:|
| `(eq? x x)` | 1.7 | 0 |
| `(== x x)` — symbols | 3.2 | 0 |
| `(equal? s1 s2)` — strings | 10.7 | 0 |
| `(== s1 s2)` — strings | 10.7 | 0 |
| `(func? x f)` | 9.5 | 0 |
| `(func? x f 1)` | 14.6 | 16.1 |
| hand-inlined `(and (list? x) (pair? x) (eq? (car x) f))` | 4.4 | 0 |
| same plus `(= (length x) 2)` | 7.2 | 0 |
| `(procedure? x)` | 1.5 | 0 |

The LaTeX export profile counts 1.88 M `==` calls and 0.66 M `func?` calls.
At ~1.5 ns of avoidable wrapper cost per `==` and ~5 ns per `func?`, eliminating
both wrappers entirely would save on the order of **6–9 ms out of 1.7 s**
(≈0.4%). The counts are high because these predicates are everywhere, not
because each call is expensive. Optimising them is low-ROI compared with the
per-character environment lookup and the repeated tree scans in §4.

---

## 4. What the profile supports optimizing

Ordered by evidence, not by guesswork:

1. **Hoist the math-mode lookup out of the character loop.** `tmtex-string` →
   `tmtex-text-list` / `tmtex-string-break?` calls `tmtex-math-mode?` per
   character (80 928 calls) and `env?` 151 788 times. One lookup per string (or
   a parameter/local) removes most of those hash lookups and list scans.
2. **Make `env?` a single tagged lookup.** `latex-is-math?` should test the
   `!begin` name once, not run 13 separate `env?` scans. A hash set of math
   environments, or a per-node cached tag, is enough.
3. **Replace `string->list` + per-char `string-index` in `tmtex-text-list`.**
   Use `string-for-each`/`string-index` with a precomputed 256-entry special
   table; avoid consing one character at a time. This is the largest allocation
   source in LaTeX export.
4. **Fuse the full-tree passes.** `compute-mode-stats`,
   `latex-use-which-package`, `latex-macro-defs-sub`, `tmtex-filter-preamble`,
   `tmtex-filter-body`, `tmtm-brackets`, `contains-tags?`, and
   `latex-stree-contains?` each walk the document independently. One traversal
   that records tags/macros/modes, or memoized per-subtree results, removes
   most of the 6.7 M `latex-tools.scm` evaluations.
5. **Cache the HTML title and attribute tests.** `tmhtml-find-title` should not
   walk the whole document for a title that is at a known position;
   `sxml-element-head`/`sxml-content` should not call `sxml-has-attr-list?`
   twice for the same element.
6. **Replace the `tmhtml-make-cell-attr` `cond` with an attribute-name table.**
   The big-table path calls it 10 799 times; a hash keyed by
   `(car attr)`/the pair avoids 20 string comparisons per cell attribute.
7. **Precompute table cell widths and vspace.** `tmhtml-width-part` scans the
   attribute list per cell, and `tmhtml-compute-vspace` per element. The
   attribute list is immutable during serialization, so one pass can cache both.

The profile does not identify FFI marshalling as a leading cost. It can only
see Scheme, and the dominant visible paths are list traversal,
`func?`/`car`/`cdr` predicates and character-by-character string conversion
inside `convert/`. GC count is 18 during the warm LaTeX export; total GC time
was not measured, so the split between collector and mutator is not known.

---

## 5. Caveats

- `compile-profile` measures **execution counts, not time or bytes**. The
  reported “counts” are source-expression evaluations and are inclusive: a
  nested expression contributes to its enclosing function's total.
- Only Scheme is visible. `parse-latex`, `parse-html`, `update-forced`, and
  typesetting are C++; they do not appear even when the Scheme wrapper does.
- The generated boot modules are written as one very long `(module ...)` line,
  so `profile-dump-html` annotates line 2 only. Use the `start`/`end` character
  offsets from `profile-dump-list` to map counts back to expressions; the HTML
  is mostly useful for files loaded directly from source.
- Full-tree instrumentation was not usable for this benchmark on this build
  (repeated `TM-APPLY-ERROR invalid syntax #&...`, zero-byte `.tex`). The
  `convert/` filter produced valid output. The error is not fully diagnosed;
  treat the filter as a workaround, not a claim that kernel profiling is safe.
- Wall times vary with VM load (the same `export-html` ranged 5.4–9.0 s warm
  across sessions). Allocation and counts were stable and are the basis for the
  conclusions above.

---

## 6. Reproduce

```bash
cd texmacs

# 1. Instrumented conversion modules (see §2 for the exact env)
# 2. Baseline with the normal boot:
QT_QPA_PLATFORM=offscreen \
SCHEMEHEAPDIRS=/opt/homebrew/Cellar/chezscheme/10.4.1/lib/csv10.4.1/tarm64osx \
HOME=$PWD/fakehome TEXMACS_PATH=$PWD/TeXmacs \
TEXMACS_BOOT_PATH=$PWD/build-texmacos/boot \
TEXMACS_DOC_PATH=$PWD/TeXmacs/doc \
./build-texmacos/TeXmacs.app/Contents/MacOS/TeXmacs \
  -x '(load "docs/chez/bench/conversions.scm")'

# 3. Profiled boot (same command, build-profile/boot), with a harness that
#    calls profile-clear, the task, profile-dump-list, profile-dump-html.
```

The compiler changes are in `src/Scheme/Chez/compile-texmacs.scm`; unset
`TEXMACS_COMPILE_PROFILE` restores the previous build behaviour exactly.
