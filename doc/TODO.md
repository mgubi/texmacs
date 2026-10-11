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

- **Remote tools: what the check of 2026-10-08 left** (issues of
  mgubi/texmacs). #319: the backups of the server are never registered,
  `server-backup.scm` is not loaded (where to hook it is to decide). #322:
  the page cannot fetch the artwork of texmacs.org (CORS) and falls back
  to the thumbnails. The fixes of #315, #316, #317 and #318 are here and
  proposed for `wip_fixes` (PRs #323 to #326): the same changes, which
  merge without conflict. The rename dialog (#318) was tried in Vue only.

- **S7 raises `car: argument is a free cell` now and then in long runs**
  (found on 2026-10-09 with the fixtures of the CSL test suite, 845 styles
  processed in one process; also seen in the benchmark of the converters).
  Four errors in each run with `(*s7* 'cache-macro-expansions?)` on, as
  TeXmacs runs S7 (the local patch 0005), rarely with it off; never with
  Guile or femtolisp, which give the same results on the same code. A pair
  seems to be collected while it is in use. Not investigated. To reproduce:
  `csl-run-fixtures` of `(csl csl-fixtures)`, see `doc/csl-design.md`.
- **CSL: the LaTeX export does not know the citations of the style.** A
  document with a `csl-` style exports its bibliography as it is rendered
  and its citations as `\cite{key}`, so LaTeX prints the labels of TeXmacs
  instead of "(Knuth, 1984)" or of the footnotes. The other open points of
  the CSL processor (the later forms of citations which are evaluated
  without being typeset, the rules of `subsequent-author-substitute` which
  work name by name, names in other scripts) are in `doc/csl-design.md`.

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
- CSL: download a style by its name from the repository of styles, attach
  the style to the document so that it travels with it, and read CSL-JSON
  from Zotero for the fields which BibTeX does not have (`doc/csl-design.md`).
