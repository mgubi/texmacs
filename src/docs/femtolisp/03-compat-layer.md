# 3. The compatibility layer

femtolisp is close to Scheme but has its own names (`aref`, `string.sub`,
`table`, `trycatch`...), its strings are UTF-8 aware, and its library is
small. Two files give TeXmacs the R5RS and Guile 1.8 it expects.

## 3.1 `r5rs-femtolisp.scm` (global definitions)

- **Redefining builtins.** femtolisp's builtins are constants, and the
  compiler compiles the `set!` of a constant as nothing. `define-override`
  makes the name redefinable when the macro is expanded, before compilation.
  `define-public` does the same for constants.
- **Symbols and keywords:** `:foo` is a keyword and not a symbol, as in Guile
  with prefix keywords; `symbol?` excludes keywords and the unspecified value.
- **Numbers:** `exact?`, `inexact->exact`, `floor` & co. (inexact results for
  inexact arguments), `quotient`/`remainder`/`modulo` with R5RS signs, `expt`,
  two-argument `atan`, n-ary `>`, `<=`, `>=`; `string->number` reads `a/b`
  as an inexact number. There are no exact rationals.
- **Complex numbers:** `#(%complex re im)`, with `make-rectangular`,
  `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`. `+ - * / =`
  remain femtolisp's fast instructions; for an operand which is not a number
  they call `*arith-fallback*` (patch 0020), which this file defines.
- **Characters are bytes:** `char-upcase`, `char-alphabetic?` and so on are
  ASCII-only, as in Guile 1.8 (and s7).
- **Strings are bytes.** `string-length`, `string-ref`, `substring`,
  `string-set!`, `list->string`, `make-string`, `string-index`, string search
  and comparison are C builtins in `fl_core.c` working on bytes.
- **Lists:** `list-tail`/`list-head` raise errors on bad indices,
  `delete`, `delq`, `iota`, `make-list`, `list-set!`...
- **Control:** `call/cc` and `call-with-exit` are escape-only (no TeXmacs code
  uses re-entrant continuations), `dynamic-wind`, `delay`/`force`.
- **Errors as Guile's:** errors are lists `(key subr message args rest)`.
  femtolisp's own errors (`type-error`, `unbound-error`...) are translated
  (`%guile-error`) for `catch` handlers and for the C++ code. `catch`,
  `throw`, `error`, `scm-error`, `false-if-exception`.
- **Ports:** femtolisp iostreams; `display`, `write`, `read-line`, string
  ports, file ports. `write` is on one line, labels only cycles
  (`*print-shared*`, patch 0005) and writes closures `#<procedure f>`
  (`*print-closures*`, patch 0018).
- **Sources:** compiled functions keep their source (`*keep-source*`, patch
  0011), for `procedure-source`, which the menus use.
- **Debugging:** `%report-error` prints the errors which reach C++; with
  `TEXMACS_FL_TRACE=1` also the femtolisp error and its stack, with
  `TEXMACS_FL_TRACE=catch` also the errors caught by `catch`.

## 3.2 `compat-femtolisp.scm` (module `(kernel boot compat-femtolisp)`)

Public Guile library functions, modelled on `compat-s7.scm`:
- hash tables (`make-hash-table`, `hash-ref`, `hash-fold`...) on femtolisp
  tables, with `equal?` keys;
- association lists (`assoc-ref`, `assoc-set!`, `assoc-remove!`);
- SRFI-1 (`fold`, `reduce`, `append-map`, `filter-map`, `partition`, `span`,
  `take`, `drop`...) and sorting (`sort`, `stable-sort`, merge sorts);
- SRFI-13 strings (`string-prefix?`, `string-index`, `string-trim`,
  `string-split`, `string-join`, `string-search-forward`...);
- char-sets as vectors of 256 booleans;
- `object-property`, `symbol-property`, `procedure-name`,
  `procedure-property` (arity from the kept source), debug options as no-ops;
- `format` (`~a ~s ~% ~~`), `pretty-print`, records, Guile's `while` with
  `break` and `continue`.

TeXmacs defines an editor command `fold`, which replaces SRFI-1 `fold` as in
Guile; the layer's own functions use a private `fold*`. The layer must never
redefine a glue function (`string-replace` is one).
