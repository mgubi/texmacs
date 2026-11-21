# 6. Open issues

## 6.1 Failing checks

The full run of `tests/scheme/check.sh` (all, typeset-check and
code-lines-check: 64 suites in one process) under femtolisp, measured by
#888 on its branch (19 November 2026, Vue on macOS arm64), fails 6 suites
and 22 checks. They are the same 22 checks as under s7 in the same run:
none is specific to femtolisp.

| Suite | Failed | Group | Cause |
|---|---|---|---|
| editing | 1 | structure | the order of the suites and the state they leave (one process, #906) |
| math-edit | 5 | enter math | the same |
| misc-modules | 1 | mirrors in a buffer with two views | the same (#906) |
| parse | 6 | highlight fortran | the same: the colours of another language mode |
| kbd-menu | 5 | entering and leaving math, closing windows | the same |
| plugins | 4 | processes: the group of a plugin | timing of pipes and sessions (#874) |

The checks which failed only under femtolisp before (`htmltm` "img, %",
the exact rationals; `graphics-edit` "create", the order of a hash table;
`formats`, the verbatim export to iso-8859-1) pass, in #888's run and in
a run of 21 November; the known differences behind the first two are in
6.2.

At `b3881bfcdb` (21 November 2026, after #900), `convert-more` no longer
ends under femtolisp: in the group "html export: a macro which calls
itself" it hashes deep trees in a table (`fl_table_put`, `equal?` with
cycle detection) for more than 10 minutes, with or without patch 0039.

## 6.2 Differences with Guile

- No exact rationals (`(/ 1 2)` is `0.5`), no bignums (integers have 64 bits;
  larger literals are read inexact).
- Complex numbers are vectors handled by `+ - * / =` through
  `*arith-fallback*`; `number?` is false for them and the other numeric
  functions do not take them.
- `call/cc` is escape-only.
- The stack of Scheme is on the heap and grows (a plain recursion a million
  deep works, as in s7), but a recursion through the builtins which call
  functions (`map`, `for-each`, `sort`...) uses the stack of C: past
  512 KB of its end it is the error `stack-overflow`, as in Guile (patch
  0025, `%guile-error`), which the HTML export catches to cut a document
  nested too deep. The conversions of C++ between trees and lists (`stree->tree`,
  the glue) are recursions of C++ too, with no check, under every Scheme:
  past about 45000 levels (femtolisp) or 50000 (s7) they overflow the stack.
  The collector of femtolisp relocates data nested as deep (lists, vectors,
  closures, hash tables...) without a recursion of C past 1000 levels
  (patches 0037 and 0039, #908). `equal?`, the printer and the hash tables
  (which compare their keys with `equal?`) still recurse in C through
  data nested deep: a list or a vector nested a million deep overflows the
  stack of C there (a crash, in a standalone femtolisp).
- Macros are expanded when code is compiled. Code which runs at expansion time
  (macros with side effects, self-recursive expansions) behaves differently
  from Guile; late calls and deferred expansion errors cover the common cases.
- A late call (a call of a name unknown at compile time) gets the values of
  the local variables: a `set!` of one of them inside the call is lost.
- `:use` does not restrict the visible names.
- The hash tables iterate in another order.
- Printing a builtin gives `#.car`.

## 6.3 Fragile spots

- Any C or C++ code which keeps a raw `fltm_value` across an allocation is a
  bug; use `tmscm` in C++ and femtolisp's stack in C.
- A `longjmp` over a C++ frame with live `tmscm`s corrupts the list of roots;
  only `fl_core.c` and the glue wrapper may let femtolisp errors propagate.
- The compatibility layer must not redefine glue functions.
- `heapsize` is 32 bits: the heap is capped at 1 GB.

## 6.4 Next steps

- The cache of compiled files: its format number (`%cache-format` in
  `boot-femtolisp.scm`) must be increased when the code which compiles
  (`boot-femtolisp.scm`, the late calls...) changes in a way the key does
  not see.
- Test CMake, Linux, Windows and the WebAssembly build (llt builds on any
  architecture, but this was only tested on macOS arm64).
- Try plugins and user code written for Guile.
- Run the integration suites and the documentation checks.
