# 9. In the browser (branch `maxs_femto`)

`maxs_femto` is `maxs_texmacs` (Vue, the browser build) with the femtolisp
commits, lazy function bodies included.

## 9.1 Build

```
. misc/wasm/emenv.sh build-wasm
make -C build-wasm -f ../misc/wasm/Makefile -j8 SCHEME=femtolisp web
```

- `SCHEME=femtolisp` (`misc/wasm/Makefile`) replaces the sources of S7 by
  `Scheme/Femtolisp/fl_core.c`, `fl_llt.c` and `femtolisp_tm.cpp`, defines
  `USE_FEMTOLISP` (`misc/wasm/config.h` then leaves out `USE_S7`) and puts
  the objects in `build-wasm/obj-femtolisp`, the page in
  `build-wasm/out-femtolisp/web`. Without it, the build is that of S7, as
  before.
- The boot package (`misc/wasm/boot-files.txt`) has the four files of
  femtolisp (`init-femtolisp.scm`, `kernel/boot/*-femtolisp.scm`).
- femtolisp compiles as it is for 32-bit WebAssembly (`-fwasm-exceptions
  -sSUPPORT_LONGJMP=wasm`, as the rest of the page): its fixnums have 30
  bits there, larger integers are boxed (`fltm_integer`).
- The functions of Scheme for the page (`web-javascript`, `web-files`,
  `web-open-pdf`, `web-open-external`, `web-paste-dialog`, in
  `src/Plugins/Vue/vue_gui.cpp`) were defined with the API of S7; they are now
  defined on the interface of the interpreters (`tmscm`), for both.
- For debugging: `?env=NAME=VALUE` in the address of the page sets a variable
  of the environment of TeXmacs (`?env=TEXMACS_FL_TRACE=catch`), and
  `browser-run.mjs --timeout <s>` lets an `eval` of a script run longer (the
  regression suites).

## 9.2 The cache of compiled files, in the page

The page ships the cache of compiled files of femtolisp, in the boot
package, as `/texmacs/cache/femtolisp`:

- `boot-femtolisp.scm` names the cache of a file of TeXmacs by its path in
  `$TEXMACS_PATH` (`TM%progs%...flc`), and reads
  `$TEXMACS_PATH/cache/femtolisp` when the home has no valid cache of the
  file (also for the compiled function bodies, `%lazy.flc`). What is
  compiled anyway (a form which changed, or which cannot be cached) goes to
  the cache of the home.
- The build makes it (`femtolisp-cache` in `misc/wasm/Makefile`, at every
  build, a few seconds): the program for node (the same compiler, in
  WebAssembly) runs `misc/wasm/femtolisp-cache.scm`, which loads what
  TeXmacs loads at its start and when all the menus are opened, in text and
  in math; its cache (7.8 MB, 293 files) goes to `build-wasm/femtolisp-cache`,
  which `package.py` adds to the files of the page (its argument
  `<dir>:<prefix>`).
- It makes the boot package 0.9 MB larger with brotli (5.16 MB instead of
  4.26), 1.3 MB with gzip.

## 9.3 Results (2026-10-07, headless Firefox, `misc/wasm/browser-run.mjs`)

| | femtolisp | S7 |
|---|---:|---:|
| `texmacs.wasm` | 23.5 MB | 24.2 MB |
| start, first visit, without the shipped cache | 4.21–4.26 s | |
| start, first visit, with the shipped cache | 3.11–3.22 s | 2.81–2.85 s |
| start, next visits | 1.20 s | 1.05 s |

(the start is the time the page reports, "TeXmacs: running"; three runs
each, alternated, load average 3-4; without the shipped cache, the first
visit compiles the loaded forms and writes the cache, 9.5 MB in 402 files of
the home directory, in IndexedDB)

- Menus, typing in text and in math, a Scheme session and an Asymptote
  session work as with S7.
- The regression suites in the page (`(run-all-tests)`): S7 fails 10 suites
  there (things a page cannot do), femtolisp the same ones with the same
  checks, and the two known differences (`htmltm`, `graphics-edit`, §6.1),
  and one group of `math-edit` ("enter math") fails differently: with S7
  the mode is not math, with femtolisp also the new equations hold a
  `document`. Both come from the environment of the test buffer in the
  page, which is not the one of a typeset document (natively, both pass).

## 9.4 Next

- Find why the test buffers of `math-edit` are not typeset in the page.
