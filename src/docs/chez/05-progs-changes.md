# 5. Changes to the code shared by all the Schemes

The Chez port adds about 9600 lines of new files to `TeXmacs/progs`, loaded
only by Chez: `kernel/boot/boot-chez.scm`, `chez-compat.scm`,
`chez-reader.scm`, `glue-chez.scm` (generated), `raw-ffi-table.scm`, and the
start-up files `init-texmacs-chez.scm`, `init-texmacs-boot-file.scm`,
`init-chez-compile.scm`. The changes to the shared files are small. All the
regression suites pass with them on Chez, s7, Guile 1.8 and femtolisp.

## 5.1 Under `(chez-scheme?)`

`(chez-scheme?)` is defined `#f` in `boot.scm`, `boot-s7.scm` and
`boot-femtolisp.scm`. The changes below are in its Chez branch; the other
Schemes run the code they ran before.

| File | Change |
|---|---|
| `kernel/texmacs/tm-define.scm` | an overload records its module (`tm-define-overloaded-chez`); the Chez forms of `tm-define-macro` and of lazy definitions; `tm-thunk-with-source` |
| `kernel/texmacs/tm-states.scm` | the slots of a state as top-level values |
| `kernel/texmacs/tm-modes.scm` | dummy definitions where a definition would follow an expression; the predicates of the Cyrillic keyboard modes |
| `kernel/texmacs/tm-convert.scm` | `converter` registers at load time (`converter-define!`); lazy formats loaded without `eval` |
| `kernel/boot/ahash-table.scm` | `define-table`, `define-collection` |
| `utils/edit/variants.scm` | `define-group` registers at load time (`group-define!`) |
| `kernel/texmacs/tm-secure.scm` | `define-secure-symbols` |
| `bibtex/bib-utils.scm` | `bib-define-style`: the assignment as a dummy definition |
| `kernel/gui/kbd-define.scm`, `gui-markup.scm` | the commands of keyboards and menus keep their source (`tm-thunk-with-source`, a plain `lambda` on the other Schemes) |
| `kernel/gui/menu-define.scm` | an invalid menu item raises its error when it is used (`gui-make-invalid`), since an error in the compiler would stop the whole module |
| `prog/scheme-autocomplete.scm` | the names to complete: the global functions and macros |
| `check/define-test.scm`, `macro-drd-test.scm`, `plugins-test.scm`, `kernel/boot/abbrevs-test.scm` | a few checks adapted, or skipped where Chez keeps no source |

Why these macros change: they do their work while they expand, and on Chez
the expansion happens once, in the compiler
([02](02-reader-and-compiler.md#23-macros-that-act-when-they-are-expanded)).

## 5.2 For every Scheme

| File | Change |
|---|---|
| `graphics/graphics-env.scm` | `state-slots` is read when the code runs, not when the macro expands (same result) |
| `kernel/gui/menu-widget.scm` | a procedure without source is accepted |
| `dynamic/session-edit.scm`, `program-edit.scm` | `#<void>` is also "no value" |
| `kernel/regexp/regexp-select.scm` | Chez with s7 and femtolisp |
| `kernel/texmacs/tm-modes.scm` | `in-cyrillic-koi8%` and `in-cyrillic-cp1251%` are defined: `koi8-kbd.scm` and `cp1251-kbd.scm` used them, undefined, so their keymaps now take effect |

Bugs found by compiling everything:

| File | Bug |
|---|---|
| `client/client-widgets.scm` | the confirmation of a pending account had an `if` with three branches: it records the account and logs in on success, and shows the error otherwise |
| `convert/latex/tmtex-ieee.scm` | an unquoted `()` |
| `security/gpg/gpg-edit.scm` | an `item` without its command |

## 5.3 Start-up

Chez starts through the shared `init-kernel.scm` and `init-texmacs.scm`, as
s7 through `init-s7.scm`: the two Chez start-up files keep only what is
proper to Chez (the modules of the boot image, the tracing of loads, the
definers for code loaded at run time). An earlier copy of the registrations
of `init-texmacs.scm` had drifted from it.
