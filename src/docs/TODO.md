# Open work on upsilon_texmacs

The things known to be left to do, gathered on 2026-10-07 from the audits
and fixes of the past days. Remove an item when it is done (or say where it
went); add what you find. Paths are relative to the repository root.

## Waiting for a decision

- **Merge PR #215 into `wip_fixes`.** It fixes the keyboard maps
  (`kbd-unmap` rewrites keys, reverse bindings, `:require` conditions,
  graphics menus, a bare predicate in `kbd-unmap`). Then merge `wip_fixes`
  into `upsilon_texmacs` again. (#311, `delayed (:refresh ms)`, was merged on
  2026-10-14 and is here; the developer manual says how `:refresh` works.)

## Bugs found, not fixed

- **OpenType work, after the audit of 2026-10-13** (`src/docs/opentype-audit.md`,
  section "State after the fixes", has the full list). Tried in a window on
  2026-10-18, all as they should be: the cursor in a formula with primes,
  accents and negations (Vue test `math-cursor`), the font design page with
  its row of variants (IBM Plex), Format > Font > Weight (with "complex
  actions" set to menus; the eight weights of IBM Plex, Light applied). Not
  compiled anywhere yet: the Linux and Windows branches of the
  font path in `src/src/Plugins/Freetype/tt_file.cpp` (TeX Live of any year
  on Linux). Seen and not traced: a bus error at startup (three times, never
  reproduced). (The four import checks of the suite `latex` which failed on
  the first run with a new home directory: the branch of the audit lacked
  2ffd02fbcc, PR #259, which is here; checked on 2026-10-18.)
- **Cursor history.** PR #337 on `wip_fixes` holds the fix which is already
  here (278d12fc27): a saved position whose buffer was closed was read
  after its tree was freed.
None left from the list of 2026-10-07. The fixes are in e6c092506c (ports,
build, docs, fonts), in the commit "Qt pipes: commands run by sh, in a
process group stopped as a whole" and in the merge of `wip_fixes`
b6cd741771 (Windows). The CI of `upsilon_ci` passes on b6cd741771: macOS
(Cocoa and Vue, arm64 and x86_64), Linux and Windows (Qt 6 and Vue, with
the 48 regression suites on Qt). Still only compiled, never run: the X11,
SDL and Qtwk changes, the Qt 5 branch of the Qt pipes
(`setupChildProcess`), and headless mode of X11, SDL and Cocoa. Qtwk has
no headless mode. The Widkit duplicates are fixed on `wip_other_guis` too.

- **Qt pipes: the stop of a program run by a wrapper** (check "pipes:
  processes" of the suite `plugins`, which failed now and then). Fixed in
  54dcfc0cd0: the group of the program is waited for before Qt closes the
  pipes. Checked 160 times on macOS (Qt reaps the program from its SIGCHLD
  handler). The Linux branch, which skips the zombie of the program by
  reading `/proc`, passed one CI run only: watch the next runs of the
  Linux Qt job (a regression would show as a stop of 2 s each, the suites
  much slower than 5 minutes, rather than as a failure).
- **Docs check** (`src/tests/docs/check.sh`): ended its TeXmacs with
  `(quit)`, which S7 does not have, so every book waited for the timeout;
  fixed in 6415686958 (the English books take 2 minutes).

- **Remote tools: what the check of 2026-10-08 left** (issues of
  mgubi/texmacs). #319: the backups of the server are never registered,
  `server-backup.scm` is not loaded (where to hook it is to decide). #322:
  the page cannot fetch the artwork of texmacs.org (CORS) and falls back
  to the thumbnails. The fixes of #315, #316, #317 and #318 are here and
  proposed for `wip_fixes` (PRs #323 to #326): the same changes, which
  merge without conflict. The rename dialog (#318) was tried in Vue only.

- **CSL: the open points of the processor** (the later forms of citations
  which are evaluated without being typeset, the rules of
  `subsequent-author-substitute` which work name by name, names in other
  scripts, the corners of the LaTeX export in note styles) are in
  `src/docs/csl-design.md`. The LaTeX export uses the package
  `citation-style-language` since 2026-10-18.

- **GPU renderer, after the work of 2026-10-15** (Metal backend, the
  canvases of the Cocoa port on the GPU; `src/docs/vue-graphics-stack.md`,
  "The GPU renderer", and `src/docs/ns-port.md`, "Drawn by the GPU"):
  - One launch of the Cocoa test build quit by itself before its first
    snapshot (no crash report, nothing in the log); four launches after it
    did not.

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
- The vector shapes of the Metal backend drawn by the GPU (Core Graphics
  rasterizes them on the CPU now, over their box). (The windows of the Vue
  port are paced by a display link since 2026-10-18; what is left of the
  wait in nextDrawable is the latency of the GPU and of the compositor,
  see `src/docs/vue-graphics-stack.md`.)

- Invalidate the style cache automatically when a style or package
  changes (it is now stale until cleared by hand).
- Fix the toggle bugs found in `src/TeXmacs/progs/utils/misc/gui-utils.scm`
  and in the style package `src/TeXmacs/packages/new-gui/gui-button.ts`
  (GUI through markup).
- CSL: download a style by its name from the repository of styles, attach
  the style to the document so that it travels with it, and read CSL-JSON
  from Zotero for the fields which BibTeX does not have (`src/docs/csl-design.md`).
