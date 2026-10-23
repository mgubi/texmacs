# Open work on upsilon_texmacs

The things known to be left to do, gathered on 2026-10-07 from the audits
and fixes of the past days. Remove an item when it is done (or say where it
went); add what you find. Paths are relative to the repository root.

The directions of the project, wished for and not yet begun, are in
[`src/TODO.md`](../TODO.md).

## Next

- (`wip_fixes` is merged here up to #420, 2026-10-21. `delayed (:refresh ms)`, #311,
  is here since 2026-10-14; the developer manual says how `:refresh`
  works.)

## Bugs found, not fixed

- **Decided on 2026-10-20, to do here** (maintainer decisions on issues of
  `wip_fixes`):
  - #78 item 11: a CMake rule which regenerates the Scheme glue, with the
    S7 generator of this branch (95e403928a).
  - #69 item 7: Cork languages split text so that a `<...>` character is an
    item of its own, so Greek words are never hyphenated (#351 made the
    hyphenation itself handle them). Keep such characters inside words,
    try it on Greek documents, then bring it to `wip_fixes`.
  - #80 item 9: the joining of the pieces of big TeX delimiters is compiled
    out under Qt (`glyph_shrink.cpp`). Turn it on and compare screenshots of
    big delimiters before deciding.
  - #64 item 7: `new_buffer_rep::read_only` is the start of an unfinished
    feature (read-only buffers: help pages, files without write
    permission). It is read by `needs_to_be_saved` and the typesetter, and
    set by nothing. Finish it here if wanted; it stays on `wip_fixes`.
  - #380: the error recovery after a segmentation fault (a C++ exception
    thrown from the signal handler), to investigate before deciding.

- **OpenType work, after the audit of 2026-10-13** (`src/docs/opentype-audit.md`,
  section "State after the fixes", has the full list). Tried in a window on
  2026-10-18, all as they should be: the cursor in a formula with primes,
  accents and negations (Vue test `math-cursor`), the font design page with
  its row of variants (IBM Plex), Format > Font > Weight (with "complex
  actions" set to menus; the eight weights of IBM Plex, Light applied). The
  Linux and Windows branches of the font path in
  `src/src/Plugins/Freetype/tt_file.cpp` (TeX Live of any year on Linux)
  are compiled by the CI, never run against a TeX Live on Linux. Seen and
  not traced: a bus error at startup (three times, never reproduced). (The four import checks of the suite `latex` which failed on
  the first run with a new home directory: the branch of the audit lacked
  2ffd02fbcc, PR #259, which is here; checked on 2026-10-18.)
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
  reading `/proc`, passed every run of the Linux Qt job since (five on
  2026-10-20, the suites in 5 to 8 minutes, for 57 suites where there were
  48); a regression would show as a stop of 2 s each rather than as a
  failure.
- **Docs check** (`src/tests/docs/check.sh`): ended its TeXmacs with
  `(quit)`, which S7 does not have, so every book waited for the timeout;
  fixed in 6415686958 (the English books take 2 minutes). The statistics of
  the books in `src/tests/docs/ref/summary.txt` are stale for es, it, pt
  and zh (set on 2026-10-04) and for the table of contents of en: the
  manual grew since (converters, Zotero, Git, bibliography, code
  features), and the other languages take the new pages in English. A
  full run on Linux on 2026-10-20 (38c6b79047) found nothing else; refresh
  with `tests/docs/check.sh -b -u` on macOS. On a machine without the
  locales of the manuals (de, es, fr, it, pl, pt, zh) every book adds
  "locale ... not found" warnings, counted as new problems.

- **The PDF export of Vue (MuPDF) lacks the fixes of #341 and #249**
  (merged here on 2026-10-21): they are in the native renderer of the Qt
  and Cocoa builds (`pdf_hummus_renderer.cpp`): bookmarks without
  backslashes, the symbols of the TeX math fonts in the text layer, escaped
  URIs, embedded videos. To check in `Plugins/MuPDF/mupdf_pdf_renderer.cpp`
  and port what applies; `tests/documents/ref` keeps the output of Vue
  (the references of `wip_fixes` for article, math, slides and tables are
  those of the Qt renderer). The merged Hummus code was only compiled
  (syntax check), not run. Likewise #356 reloads a changed picture file in
  the picture cache of Qt only (`qt_picture.cpp`).

- **Remote tools: what the check of 2026-10-08 left** (issues of
  mgubi/texmacs). #319: the backups of the server are never registered,
  `server-backup.scm` was loaded before the server existed: fixed by #345,
  here since the merge of 2026-10-21. #322: the page cannot fetch the
  artwork of texmacs.org (CORS) and falls back to the thumbnails. The fixes
  of #315 to #318 went to `wip_fixes` within #330 (PRs #323 to #326 were
  folded into it), which is here. The rename dialog (#318) was tried in Vue
  only.

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

- **Interactive footer, after the work of 2026-10-19**
  (`src/docs/vue-interactive-footer.md`): the menus of the tags were tried
  in Vue only (tests `footer-menus` and `footer-variants`); the Cocoa build
  compiles but its footer was not tried by hand. A right click on a tag of
  the footer does nothing now (the left click gives its menu, Focus menu
  included). At start-up the log says `warning: conditional master routine
  focus-extra-menu` (the code menu of `wip_code` hooks into the Focus menu
  of every tag): harmless, not traced.
- **Windows: a typesetting crash when a highlighter is installed late**
  (a tree-sitter or LSP highlighter which starts after the document was
  typeset). Seen on the Windows CI only; not reproduced elsewhere, and not
  in the five Windows runs of 2026-10-20.
- **#376, the question dialog of Vue on Linux: not fixed for its
  reporter.** Closed on 2026-10-20 after the ThorVG fix (the dialog comes
  out right on the Azure VM, Xfce and X11), but the reporter still sees it
  now and then on GNOME with Wayland, and TeXmacs crashed (segmentation
  fault) on Ctrl+Q with unsaved changes, then Esc in the question dialog.
  To reopen, and to try on GNOME/Wayland (the VM has Xfce only).
- **Fixed, waiting for the reporter** (issues open on mgubi/texmacs): #399
  (hover balloons of the language servers, which still vanished at times:
  d2165b5b31), #400 (Save as of Cocoa and the `.tm` suffix: a494889452),
  #407 (TeX symbols as squares with `fonts-lyx` installed: 0774d20ef0),
  #408 (a book of 2000 pages 4x slower than stock: 842ee19180,
  786f99eae3), #409 (the language servers of Julia: 068f74706f).
- **GCC.** The Vue port builds with GCC since 6c494fbbaf, and the Linux Vue
  job and AppImage use it (515da759d8); the Windows Vue job stays on
  clang (MinGW GCC not tried). Two suites fail on a plain Ubuntu 24.04
  (Azure VM, with GCC and with clang alike) and pass on the CI: `git`
  (`git-remote-branches` lists no `origin` with Git 2.43) and
  `misc-modules` (spell checking of the language "klingon" answers no
  error with aspell installed): assumptions of the tests to fix.

- **Qt: a rare empty answer in the footer** (test `tests/gui/interactive`,
  branch `wip_gui_scripting`, 2026-10-20): once in some twenty runs on
  macOS the question is answered empty as it opens, before any key. The
  cause found and fixed (the input ended when the window lost the focus to
  another application, `QTMInputTextWidgetHelper::leave`) left this one,
  not caught with a trace in ten runs. `tests/gui/menu-formula` fails as
  rarely on Qt (the formula not there when the letter is typed); both pass
  when run again.

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
