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

- **OpenType work, after the audit of 2026-10-13** (`src/doc/opentype-audit.md`,
  section "State after the fixes", has the full list). Not tried in a window:
  the cursor under an accent or around a negation in a formula (the boxes
  are wrapped as the primes are, only the tree-to-box direction is tested),
  the font design page with its row of variants, the menu Format > Font >
  Weight. Not compiled anywhere yet: the Linux and Windows branches of the
  font path in `src/src/Plugins/Freetype/tt_file.cpp` (TeX Live of any year
  on Linux). Seen and not traced: a bus error at startup (three times, never
  reproduced), four import checks of the suite `latex` which fail on the
  first run with a new home directory only.
- **The unit tests cannot be built without Qt.** `src/tests/Makefile` runs
  `moc` on the tests which declare a `Q_OBJECT` and `TM_MOC` is empty in a
  Vue or Cocoa build, so `make -C tests` and `tests/opentype/check.sh` stop
  at the first such test. The unit tests run on a Qt build only.
- **Cursor history.** PR #337 on `wip_fixes` holds the fix which is already
  here (278d12fc27): a saved position whose buffer was closed was read
  after its tree was freed.
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

- **GPU renderer, after the work of 2026-10-15** (Metal backend, the
  canvases of the Cocoa port on the GPU; `src/docs/vue-graphics-stack.md`,
  "The GPU renderer", and `doc/ns-port.md`, "Drawn by the GPU"):
  - One launch of the Cocoa test build quit by itself before its first
    snapshot (no crash report, nothing in the log); four launches after it
    did not.
  - The Cocoa port drawn by Core Graphics (`TEXMACS_NS_GPU=0`): the
    snapshots of a document of one page show the page shifted, with a copy
    of it on the right (the backing store which wraps, `cring`?). Only seen
    in the snapshots, not traced.
  - Vue on OpenGL under macOS: `SDL_GL_SwapWindow` waits 1.6 to 3 ms a
    present while scrolling; the presents are not paced as those of Metal
    (`ready_to_present`), which is the default on macOS.

## Kept on purpose

- `smart_font_rep::adjusted_dpi` in `src/src/Graphics/Fonts/smart_font.cpp`
  keeps an old hack of upstream (`zoom *= 0.9` for TeX Gyre Cursor with
  Pagella, "temporary hack for new manual"). No style uses Cursor with
  Pagella any more (its typewriter is Inconsolata), so it only matters when
  Cursor is chosen by hand.

## Offered, not asked for

- One GPU API for all the backends: sokol_gfx (OpenGL, WebGL2, Metal,
  WebGPU, shaders written once with sokol-shdc) or WebGPU (Dawn,
  emdawnwebgpu), instead of `gpu_backend_gl.cpp` and
  `gpu_backend_metal.mm`; surveyed on 2026-10-15 (WebGPU is not in every
  browser yet, so WebGL2 has to stay).
- The windows of the Vue port presented by a display link, as the canvases
  of the Cocoa port (`TMGpuPresenter`), instead of the pacing of the loop;
  and the vector shapes of the Metal backend drawn by the GPU (Core
  Graphics rasterizes them on the CPU now, over their box).

- Invalidate the style cache automatically when a style or package
  changes (it is now stale until cleared by hand).
- Fix the toggle bugs found in `src/TeXmacs/progs/utils/misc/gui-utils.scm`
  and in the style package `src/TeXmacs/packages/new-gui/gui-button.ts`
  (GUI through markup).
- CSL: download a style by its name from the repository of styles, attach
  the style to the document so that it travels with it, and read CSL-JSON
  from Zotero for the fields which BibTeX does not have (`doc/csl-design.md`).
