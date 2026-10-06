# 6. Open issues

## 6.1 Failing checks

With `tests/scheme/check.sh all`, 40 of 43 suites pass. The 4 failing checks:

| Suite | Check | Cause |
|---|---|---|
| htmltm | `img, %` | no exact rationals: a width of 50% gives `0.5par`, Guile `1/2par` (the same length) |
| graphics-edit | `create` (2 checks) | the order of the attributes of `with` comes from the iteration order of a hash table (`ahash-table->list`), which differs between Guile, s7 and femtolisp |
| formats | verbatim export to iso-8859-1 | not femtolisp: `cork_to_latin1` writes `?` since 8d286547e9, the test expects the character to be left out |

## 6.2 Differences with Guile

- No exact rationals (`(/ 1 2)` is `0.5`), no bignums (integers have 64 bits;
  larger literals are read inexact).
- Complex numbers are vectors handled by `+ - * / =` through
  `*arith-fallback*`; `number?` is false for them and the other numeric
  functions do not take them.
- `call/cc` is escape-only.
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
