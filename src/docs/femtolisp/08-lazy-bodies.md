# 8. Lazy function bodies (experimental, branch `wip_femto_lazy`)

## 8.1 Why

femtolisp expanded and compiled every loaded form, including the body of
every function, whereas Guile and s7 expand a function body when it first
runs. At boot, 8773 functions are defined and only 1425 of them (16%) are
called. The expansion of the loaded forms was the main cost which s7 did not
have (§7.7).

## 8.2 How (`boot-femtolisp.scm`)

- **Top-level expansion only** (`%expand-top`). A loaded form is expanded as
  before, except that a `lambda` which is not inside a binding form (its free
  variables are global) is kept unexpanded, in a stub
  (`(%make-lazy '(lambda ...) 'name 'module)`). The macros of the top level
  (`tm-define`, `menu-bind`, `define-preferences`...) still run at load, with
  their side effects; the macros inside function bodies run at the first
  call, as in Guile.
- **Stubs.** A stub is a function made from one shared code
  (`(lambda args (apply (%lazy-force! 'record) args))`), with the name of the
  function and, as last constant, its source: `procedure-name`,
  `procedure-source` and `procedure-arity` are right before the first call.
- **First call** (`%lazy-force!`). The body is expanded and compiled in the
  module where it was loaded; then the stub *becomes* the compiled function
  (`%function-become!` in `fl_core.c` copies the code, the constants, the
  environment and the name), so that the references kept to the stub (hooks,
  menus, former definitions of `tm-define`, tables) call the compiled code.
  The stub's code ends with a tail call right after `%lazy-force!` returns:
  the interpreter keeps a pointer into the running code, which must not run
  after it is replaced (code is pinned, and the stubs share one code).
- **Hash of functions** (patch 0022). A function hashed as its code, which
  changes when the stub becomes the compiled function: `tm-defined-name`
  lost the names of the tm-defined functions. A function with its source now
  hashes as its source; the compiled function gets the source of its stub
  (the lambda before its expansion, as Guile's `procedure-source`).
- **Cache of compiled bodies.** The compiled bodies are kept in
  `%lazy.flc` in the cache of compiled files, under the fingerprint of their
  expansion, their module and its private names (bodies are still expanded at
  their first call, not compiled). The file starts with the key of the cache;
  each compilation appends an entry; it is read at the first call of a stub.
  The cache of compiled files has the format 3 with lazy bodies, 2 without.
- `TEXMACS_FL_EAGER=1` expands and compiles everything at load, as before;
  `(%lazy-report)` gives the number of stubs, of first calls, of cache hits
  and the time of the first calls (with `TEXMACS_FL_PROFILE`).

Also on this branch: the heap starts with 32 MB instead of 8
(`TEXMACS_FL_HEAP`, in MB): a boot collected the garbage 23 times instead of
5, about 60 ms more; the resident memory at boot is 8 MB larger, the peak of
the interactive benchmark is the same.

## 8.3 Results (2026-10-07)

Front end of a boot with warm caches:

| | eager | lazy |
|---|---:|---:|
| reading the files | 26 ms | 27 ms |
| expansion (top level only when lazy) | 150 ms | 60 ms |
| compiling or finding in the cache | 87 ms | 58 ms |
| first calls (expansion of the bodies, cache) | – | 45 ms |

Same binary, eager and lazy, both with a 32 MB heap, and s7, alternated (5
boots, 3 runs of `bench/ui.scm`; load average about 3.5):

| | lazy | eager | s7 |
|---|---:|---:|---:|
| boot to exit | 0.81–0.82 s | 0.84–0.86 s | 0.65–0.69 s |
| all menus, first time | 1.92–2.36 s | 2.23–2.62 s | 1.64–2.11 s |
| whole `ui.scm` run | 12.2–12.7 s | 12.7–13.6 s | 12.2–12.6 s |
| warm tasks (menus, typing, opening) | same | same | same |

The regression suites give the same results with lazy bodies, with a new and
with a warm cache (40/43, the three known failures, §6.1).

## 8.4 Limits and next steps

- A lambda inside a binding form (`let` around a `lambda`, some `tm-define`
  expansions) is still expanded at load, by the stub of the enclosing
  top-level lambda when it is called at load.
- An error in the expansion of a body appears at the first call, not at load
  (as in Guile): tests which only load code no longer see it.
- The late calls and the deferred macro errors (§2.3, §2.4) are kept; with
  lazy bodies most of them may no longer be needed.
- What remains of the boot gap with s7 is not the front end: the
  interpreter, the top-level code of the loaded files, and the glue.
