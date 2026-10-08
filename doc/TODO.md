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
  into `maxs_texmacs` again (it was merged on 2026-10-07, before these
  PRs) and update
  `src/TeXmacs/doc/devel/scheme/utils/utils-dialogue.en.tm`, which says
  that `:refresh` does not work.

## Bugs found, not fixed

None left from the list of 2026-10-07. The fixes are in e6c092506c (ports,
build, docs, fonts), in the commit "Qt pipes: commands run by sh, in a
process group stopped as a whole" and in the merge of `wip_fixes`
b6cd741771 (Windows). The CI of `maxs_ci` passes on b6cd741771: macOS
(Cocoa and Vue, arm64 and x86_64), Linux and Windows (Qt 6 and Vue, with
the 48 regression suites on Qt). Still only compiled, never run: the X11,
SDL and Qtwk changes, the Qt 5 branch of the Qt pipes
(`setupChildProcess`), and headless mode of X11, SDL and Cocoa. Qtwk has
no headless mode. The Widkit duplicates are fixed on `wip_other_guis` too.

- **Remote tools: what the check of 2026-10-08 found** (issues of
  mgubi/texmacs; the fixes for `wip_fixes` reach this branch with its next
  merge). #316: a version of a remote file replaced within 5 s is missing
  from its history (PR for `wip_fixes`). #317: remote directories and the
  lists of chat rooms, shared resources and live documents are titled by
  their address (PR for `wip_fixes`). #318: in Remote > Rename, "Ok"
  ignores the name typed and a renamed file gets the name of a directory
  (PR for `wip_fixes`). #319: the backups of the server are never
  registered, `server-backup.scm` is not loaded (no fix: where to hook it
  is to decide). #322: the page cannot fetch the artwork of texmacs.org
  (CORS) and falls back to the thumbnails (no fix yet).

## Kept on purpose

- `smart_font_rep::adjusted_dpi` in `src/src/Graphics/Fonts/smart_font.cpp`
  keeps an old hack of upstream (`zoom *= 0.9` for TeX Gyre Cursor with
  Pagella, "temporary hack for new manual"). No style uses Cursor with
  Pagella any more (its typewriter is Inconsolata), so it only matters when
  Cursor is chosen by hand.

## Offered, not asked for

- Invalidate the style cache automatically when a style or package
  changes (it is now stale until cleared by hand).
- Fix the toggle bugs found in `src/TeXmacs/progs/utils/misc/gui-utils.scm`
  and in the style package `src/TeXmacs/packages/new-gui/gui-button.ts`
  (GUI through markup).
