# 6. Open issues

## 6.1 Differences with the other Schemes

- **Hash order.** Some results come out in another order than on s7, where
  the order of a hash table shows: the `\usepackage` list of a LaTeX export,
  the numbering of images. The results are valid, not byte-identical.
- **Floats** are printed with all their digits.
- **No source for closures:** `procedure-source` of a closure is `#f`, except
  for the commands of menus and keyboards.
- **The `equal?` bug of Chez 10.4.1** ([03](03-runtime.md#36-what-chez-lacks-or-does-differently))
  is worked around for symbol names only; any other immutable empty string
  (`string->immutable-string`) is still not `equal?` to `""` in compiled
  code. Not reported upstream yet; newer versions of Chez not checked.

## 6.2 Fragile spots

- **Hand-kept lists.** The compiler relies on several lists, which a change
  upstream can make wrong, breaking only Chez:
  - the modules and their order (`compile-inventory.scm`): a new module is
    loaded from source until it is added;
  - the private helpers that macros of other modules call
    (`chez-promoted-names`): a missing one fails when the macro is used;
  - the private names reached by the tests (`chez-global-names`);
  - `*definition-heads*`, the special cases of `prepare-forms`,
    `tm-chez-sources`, the raw routines of the glue (`raw-ffi-table.scm`,
    which overrides the types of the specifications), the modules of the
    boot image.
- **Top-level conditionals** with definitions inside are decided by the
  compiler, once: a test on a preference or a path would be frozen.
- **C++ exceptions outside the glue:** a conversion error thrown by
  `tmscm_to_int` or `TMSCM_ASSERT` is caught in the shims of the glue; from
  other C++ code (`as_int (call (...))`) it may not be.
- **Trees hashed by content:** the hash of a blackbox is computed by C++ from
  the content of the tree, so a table keyed by a tree loses the entry when
  the tree changes, and each lookup costs the size of the tree. Guile gives
  every smob the same hash.
- **Values are copied** when they cross between C++ and Scheme (the strings,
  recursively through lists and vectors): no shared mutation, a cost for
  large results, and a cyclic list would not end.
- `define-state` evaluates the initial value of each slot twice.

## 6.3 To do

- **Packages:** `make install` copies `TeXmacs/boot`, but the rules of the
  packages (the macOS bundle, the Linux and Windows packages) were not
  checked for Chez.
- **Platforms:** only macOS on arm64 was tested on this branch, headless and
  briefly with a window. The original branch built on Linux and Windows
  (MSYS2) with CMake ([build_guide/](build_guide)).
- **CI:** no workflow builds Chez.
- **Clean-up:** several files of the compiler still say "UNDER REVIEWED AI
  GENERATED CODE".
- `client/client-widgets.scm`: the confirmation of a pending account refers
  to `credentials`, not bound there (as in the official sources).
