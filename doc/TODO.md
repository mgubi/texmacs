# Open work on maxs_texmacs

The things known to be left to do, gathered on 2026-10-07 from the audits
and fixes of the past days. Remove an item when it is done (or say where it
went); add what you find. Paths are relative to the repository root.

## Waiting for a decision

- **Merge PR #215 and PR #311 into `wip_fixes`.** #215 fixes the keyboard
  maps (`kbd-unmap` rewrites keys, reverse bindings, `:require` conditions,
  graphics menus, a bare predicate in `kbd-unmap`); #311 makes
  `delayed (:refresh ms)` run its body and maps the BibTeX style `abstract`
  to `tm-abstract` when `bibtex` is not installed. Then merge `wip_fixes`
  into `maxs_texmacs` and update
  `src/TeXmacs/doc/devel/scheme/utils/utils-dialogue.en.tm`, which says
  that `:refresh` does not work.

## Bugs found, not fixed

None left from the list of 2026-10-07; the fixes are in the commit "Fix the
bugs of doc/TODO.md" of 2026-10-07 (Widkit duplicates, `x-gui?` and
`qt-gui?` for Cocoa and Qtwk, Qtwk clipboard and input methods, headless
X11/SDL/Cocoa, `rounded_rectangle` in Qt6, `mac_fix_paths`, CMake
`THORVG_DIR`, `macos-ns.yml`, CMake and TikZJax docs, typewriter companions,
Medium faces in the font database). Not tested beyond compiling: the X11,
SDL, Qtwk and Qt6 changes (no such build here; compiled with
`-fsyntax-only` against their configuration), and headless mode of those
ports. The Widkit duplicates are also on `wip_other_guis`.

## Kept on purpose

- `smart_font_rep::adjusted_dpi` in `src/src/Graphics/Fonts/smart_font.cpp`
  keeps an old hack of upstream (`zoom *= 0.9` for TeX Gyre Cursor with
  Pagella, "temporary hack for new manual"). No style uses Cursor with
  Pagella any more (its typewriter is Inconsolata), so it only matters when
  Cursor is chosen by hand.

## Offered, not asked for

- Invalidate the style cache automatically when a style or package
  changes (it is now stale until cleared by hand).
- Bring the wallet window variant of the desktop to the browser version.
- Fix the toggle bugs found in `src/TeXmacs/progs/utils/misc/gui-utils.scm`
  and in the style package `src/TeXmacs/packages/new-gui/gui-button.ts`
  (GUI through markup).
- A page with the classification of all the fonts of TeX Live 2025
  (232 OpenType/TrueType packages, 817 MB, by kind and size).
