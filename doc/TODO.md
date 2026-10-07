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

### Graphical ports

- **The Widkit ports do not compile.**
  `src/src/Plugins/Widkit/Basic/widkit_wrapper.cpp` defines
  `responsive_tabs_widget`, `responsive_icon_tabs_widget`,
  `setting_toggle_widget`, `setting_enum_widget` and `setting_group_widget`
  twice (a leftover of the merge 02d69cf61f, after #15 and `wip_fixes`
  both added them). This breaks X11, SDL and Qtwk, on `maxs_texmacs` and on
  `wip_other_guis` (not on `wip_fixes`).
- **Cocoa holds both `qt-gui?` and `x-gui?`.** `gui_is_x ()` in
  `src/src/Kernel/Abstractions/basic.cpp` is true for every port that is
  neither Qt nor Vue, and `gui_is_qt ()` is true for Cocoa. Scheme then
  takes branches meant for other ports: its own overwrite confirmation, and
  `spawn-supported?` in `version/git-base.scm` turns off `evaluate-system`
  for Git.
- **Qtwk passes for Qt.** It defines `QTTEXMACS`, so `qt-gui?` holds, and
  `gui_version ()` returns `"qt5"`/`"qt6"`; `(qt5-gui?)` cannot tell it
  from Qt. Its print dialog is the Widkit placeholder (a Cancel button)
  although `use-print-dialog?` may be true.
- **Qtwk lacks two fixes of Qt:** `qtwk_gui_rep::get_selection`
  (`qtwk_gui.cpp`) dereferences `mimeData` without a null check, and
  `QTWKWindow::inputMethodEvent` sends committed text one `QChar` at a time,
  splitting the characters outside the BMP into surrogate halves.
- **Headless mode only in Qt and Vue.** X11, SDL and Cocoa never test
  `is_headless`, so `-headless` still opens a display.
- `rounded_rectangle` is in `Plugins/Qt/qt_renderer.cpp` but not in
  `Plugins/Qt6/qt_renderer.cpp`.
- `mac_fix_paths` (`Plugins/MacOS/mac_utilities.mm`) is never called.
- The debug messages of `ns_window_widget_rep` say `qt_window_widget`.
- The comment of `image_gc` in `ns_gui.mm` says TeXmacs no longer uses it,
  but Vue and SDL implement it (nothing calls it).

### Build and CI

- **No GPU renderer under CMake:** CMake has no ThorVG option, so a CMake
  Vue build compiles the GPU path out.
- `.github/workflows/macos-ns.yml` still triggers on the deleted branch
  `ns_ci`; the macOS apps are built by `macos-maxs.yml` on `maxs_ci`.

### Documentation

- `src/TeXmacs/doc/devel/source/build-cmake.en.tm` says the default
  `SCHEME_IMPL` is embedded18; `CMakeLists.txt` defaults to s7 (Quick start
  and the `SCHEME_IMPL` item).
- `src/docs/wasm/tikzjax.md` still says it is on the branch `wip_tikzjax`.

### Fonts

- The macOS text fonts other than Palatino (Times New Roman, Garamond,
  Baskerville, Georgia...) have no typewriter companion: their typewriter
  text is the closest monospaced font. A companion-only profile in
  `src/TeXmacs/progs/fonts/fonts-opentype.scm` (one line each, as for
  Palatino) fixes one.
- The font scanner files Medium faces under the style Regular (IBM Plex
  Sans, Serif, Mono), and lists the medium file first, so the text came out
  semibold. The shipped `font-database.scm` is fixed by hand; a scan of the
  user's own fonts (the home database) can still do it.
- `smart_font_rep::adjusted_dpi` in `src/src/Graphics/Fonts/smart_font.cpp`
  keeps an old hack (`zoom *= 0.9` for TeX Gyre Cursor with Pagella,
  "temporary hack for new manual"); Pagella now takes Inconsolata, so it
  only matters where Inconsolata is missing.

## Offered, not asked for

- Invalidate the style cache automatically when a style or package
  changes (it is now stale until cleared by hand).
- Bring the wallet window variant of the desktop to the browser version.
- Fix the toggle bugs found in `src/TeXmacs/progs/utils/misc/gui-utils.scm`
  and in the style package `src/TeXmacs/packages/new-gui/gui-button.ts`
  (GUI through markup).
- A page with the classification of all the fonts of TeX Live 2025
  (232 OpenType/TrueType packages, 817 MB, by kind and size).
