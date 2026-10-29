# The TeXmacs tutorial, updated

A rewrite of `../old-tutorial` (TeXmacs 1.0.0.x, 2002) for the current
TeXmacs (2.1.x). The old tutorial only had the sources of two chapters; the
other chapters existed only as screenshots in `old-tutorial/images`. They
are rebuilt here from those screenshots, with the current menus and
keyboard shortcuts (checked against `TeXmacs/progs`).

The screenshots are replaced by live TeXmacs content: example text and
formulas, the cursor and the focus boxes are drawn with the `tmdoc` macros
`text-cursor`, `math-cursor`, `small-focus`, `big-envbox`..., keys and menus
with `key`, `shortcut` and `menu` (so the shortcuts shown always match the
running TeXmacs and its look and feel), and the window with a table.

## Layout

| File | Chapter | Old screenshots |
|---|---|---|
| `tut-tutorial.<lang>.tm` | the root (a book: Help > Full manuals style) | |
| `start/` | A first meeting with TeXmacs | `start`, `expl`, `balloon-*`, `explain` |
| `text/` | Typing your first text (name, language, style, save, print, load) | `saveas-*`, `french`, `russian`, `style-*`, `simple-*`, `load` |
| `math/` | Typing mathematical formulas | `math`, `mode-*`, `formula-*`, `symbol-*`, `heart-*`, `script-*`, `frac-*`, `symbol-menu` |
| `structure/` | Structured text (emphasis, sections, cursor, environments) | `emphasize-*`, `section-*`, `hpos-*`, `context` |
| `macros/` | Writing your own macros (macro editor, preamble, style packages) | `apply-*`, `macro-*`, `preamble`, `save-style` |
| `examples/` | the documents written in the tutorial (`test.tm`, `hairy.tm`, `sections.tm`, `formulas.tm`, `ramsey.tm`, `love.tm`, the package `love.ts` and `love-package.tm` which uses it) | |

Every page exists in English (`.en.tm`), French (`.fr.tm`), Spanish
(`.es.tm`), Portuguese (`.pt.tm`) and German (`.de.tm`), the languages of the
old tutorial; the examples are shared and in English.

## Translations

The translations follow the English text paragraph by paragraph and keep
its markup unchanged. Menus are written in English (`<menu|File|Save>`):
`tmdoc` localizes them into the language of the document, as the
translated interface names them;
the menu bar of the window figure uses `menu-item` for the same reason.
Keystrokes, file names, the status bar texts and the example texts the
reader types stay in English. The `.tm` files are Cork-encoded: to edit a
translation, edit it in TeXmacs, or write the accented letters as Cork
bytes, never as raw UTF-8.

## Installing it in the help

The paths are relative, so the folder can be copied as `TeXmacs/doc/tutorial`
(without `README.md`); `TeXmacs/progs/doc/help-menu.scm` already has the
commented entry `Help > Full manuals > Tutorial` for
`tutorial/tut-tutorial`.

## Checks

All the files pass the checker of `tests/docs` (`docs-check-files` of
`tests/docs/build.scm`: no unknown tags, language matching the suffix,
broken branches or links, missing images), and the tutorial builds as a book
with `docs-build-book` in each language (23 sections, 27 or 28 pages).

## Verification

- Menu paths, shortcuts, tooltips, status bar contents and cursor colors
  were checked against the sources, and these keystrokes were replayed
  without a window (with `key-press`): `\em`, `\section` + Return,
  `x ^ 2`, fraction + Backspace, `l Tab * l n Space x`,
  `< Tab Tab Tab = Tab /`, `\heartsuit`, and Tab adding an argument to
  `macro` in the preamble.
- The cursor moves were replayed too, with a build which has the command
  `apply-changes` (Upsilon; not yet the `wip_fixes` binary). Without a
  window, `go-right` and `go-down` do nothing while the editor has pending
  changes (`has_changed` in `edit_cursor.cpp`), and `update-forced` only
  typesets: calling `(update-forced) (apply-changes)` after each step makes
  `(kbd-right)` and `(kbd-down)` work. Likewise, the arity of the user's
  macros is only learned by `update_menus` (`drd_update`): call
  `(update-menus)` after loading a document before typing `\love`. Replayed
  this way: leaving `em` and a superscript with the right arrow, the
  numerator, Down, the denominator, the cursor walk of "bold blue" (the four
  positions of the table), moving to the second field of `assign` in the
  preamble, and `\love` Return creating one field.
- Checked only in the code: the status bar text `proof description item*`,
  the place of Preferences in the macOS menus, and the macro editor dialog.
  Note that `\arg` does not insert an argument (it is the LaTeX operator
  `arg`): the tutorial uses `\name`, which was replayed.
