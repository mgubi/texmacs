# 4. Changes to the shared Scheme code

## 4.1 The `femtolisp-scheme?` branches

`boot.scm` (Guile) and `boot-s7.scm` define `(femtolisp-scheme?)` as `#f`,
`boot-femtolisp.scm` as `#t` (and `(s7-scheme?)` as `#f`). Elsewhere femtolisp
runs the Guile code, except in:

| File | femtolisp branch |
|---|---|
| `kernel/boot/prologue.scm` | module loading is in `boot-femtolisp.scm`, as for s7 |
| `kernel/boot/ahash-table.scm` | `ahash-*` directly on femtolisp tables |
| `kernel/regexp/regexp-select.scm` | `select` as on MinGW (no Guile `select`) |
| `kernel/texmacs/tm-define.scm` | global definitions, `former` and names through `top-level-value` (see [02](02-boot-and-modules.md#25-tm-define)) |
| `prog/scheme-autocomplete.scm` | the symbols from femtolisp's environment |

## 4.2 Changes for all the interpreters

These make code independent of when macros are expanded; Guile and s7 behave
as before.
- `math/math-edit.scm`: `concat-isolate!` is a function. It was a macro whose
  expansion contains a call of itself, which expands forever when macros are
  expanded before the code runs.
- `kernel/texmacs/tm-convert.scm`: `lazy-format` records its module when the
  form is evaluated, through the public `lazy-format-add!`. It recorded it
  when expanded, so `(when (url-exists-in-path? "coqtop") (lazy-format ...))`
  declared the Coq formats without Coq.
- `kernel/gui/kbd-define.scm` and `kernel/texmacs/tm-define.scm`: likewise,
  `lazy-keyboard` and `lazy-define` record their modules when the form is
  evaluated (`lazy-define-add!`). The keyboard of Coq was declared without
  Coq, and typing loaded `coq-kbd.scm`, which failed (`in-coq-style?` is
  only defined with Coq).

## 4.3 Tests

- `check/define-test.scm` skips the check that `:use` is enforced, as for s7:
  femtolisp has one global environment.
- `check/macro-drd-test.scm` skips the dates computed with `localtime` and
  `strftime`, as for s7.

Run the suites with

```
QT_QPA_PLATFORM=offscreen TM_TEST_HOME=<scratch home> tests/scheme/check.sh all
```

40 of the 43 suites pass; see [06](06-open-issues.md#61-failing-checks).
