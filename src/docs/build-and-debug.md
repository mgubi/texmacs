# Building, running and debugging

## Configure and build

The worktree `wip_other_guis/src` is configured with

    ./configure --with-gui=vue --with-mupdf=/opt/homebrew --with-sdl3 \
      --with-guile=/Users/mgubi/t/guile-1.8.7/usr/bin/guile-config --with-gnutls

(see the first lines of `config.log`; the Guile 1.8 in `~/t/guile-1.8.7` is
used, the Homebrew Guile 3 is rejected; `--with-gnutls`, added on
2026-09-21, enables the TLS protocol of the TeXmacs client/server through
the Homebrew GnuTLS and needs a full rebuild since it changes `config.h`). `make` at the root builds
`TeXmacs/bin/texmacs.bin`; `src/makefile` is generated from
`src/makefile.in`, so permanent changes go to `makefile.in`. `configure`
and `src/System/config.in` are generated from `configure.in` and the
`misc/m4/*.m4` macros with `autoconf` and `autoheader` (Homebrew autoconf);
`misc/m4/sdl3.m4` also pulls in `sdl3-ttf`.

Header dependencies are recorded while compiling (`-MMD -MP -MF Deps/$*.d`,
included at the end of the makefile), so a header change rebuilds the objects
which include it — but only objects compiled since a `.d` file exists for
them. Stale objects after a change of a class layout or vtable (a member
added to `vue_simple_widget_rep`, a virtual method in `renderer.hpp`) crash
at startup in unrelated places (SIGSEGV in `edit_interface_rep`'s constructor
or in `mupdf_renderer_rep::new_shadow`). When in doubt rebuild everything: 
`rm -f src/Objects/*.o && make -j8` takes about 25 s.

Vue-specific objects: `vue_gui.cpp`, `vue_widget.cpp` (C++20), `clay.c`
(the Clay implementation, compiled once).

## Syncing with upstream

Upstream TeXmacs (the SVN trunk) is mirrored in the `svn_sync` branch of the
main checkout (`~/t/git/texmacs`); the Vue work was merged with it on
2026-09-21 (TeXmacs 2.1.5, 791 commits). What conflicts and what to check:

* the generated `configure` and `src/System/config.in`: do not merge them,
  regenerate (`autoconf`, `autoheader`) after resolving `configure.in` and
  `aclocal.m4` (which list `LC_MUPDF`, `mupdf.m4`, `sdl3.m4` next to the
  upstream macros);
* `src/makefile.in`: the MuPDF/SDL/Vue variables, sources and object rules
  live next to the upstream ones; upstream selects the Qt plugin directory
  with `QT_PLUGIN_DIR` (`Qt` or `Qt6`), the Widkit-on-Qt port uses
  `QT_SRC_DIR= Qt`;
* `System/Link`: Qt-only sockets, keep the non-Qt stubs in sync with
  `client_server.hpp`;
* API changes of `renderer.hpp` and `widget.hpp` surface as abstract-class
  or link errors in `Plugins/MuPDF` and `Plugins/Vue` (see the notes in
  `vue-graphics-stack.md` and `vue-widgets.md`);
* upstream Qt-only code paths (a brace inside `#ifdef QTTEXMACS` in
  `texmacs.cpp` once) break the non-Qt build: build and run the tests after
  the merge.

## Running

    TEXMACS_PATH=$PWD/TeXmacs TeXmacs/bin/texmacs.bin [-x "(scheme code)"]

`-x` runs Scheme after boot; modules not yet loaded must be imported with
`use-modules`. Preferences live in `~/.TeXmacs/system/preferences.scm`
(`set-preference` saves them: do not change them from tests).

Useful keys: F1 toggles the Clay debug view of the focused window (and the red
marker of uncovered areas). `TEXMACS_VUE_DUMP=1` prints every Clay render
command of every frame (`DUMP <type> id <id> box x,y wxh ...`; border
commands are matched against the usual id patterns) — heavy, for tracking
down a stray element.

## Debugging

* Standard output carries only warnings, errors and crash reports
  (`Error message:` + a C++ backtrace). Signals are turned into C++
  exceptions by `tm_throw`, so a crash prints the report and aborts. The
  traces are behind the usual debug flags (`vue_widget.hpp`):
  `-debug-qt` (`DEBUG_VUE`: windows created and destroyed, the timings of
  the phases of a frame when they exceed their threshold),
  `-debug-qt-widgets` (`DEBUG_VUE_WIDGETS`: `unhandled SLOT_...` for slots
  a widget does not implement, `run command ...`, `Click!!`),
  `-debug-events` (`DEBUG_VUE_EVENTS`: the SDL events, the keys as they are
  translated, the mouse actions the editor receives) and `-debug-io`
  (sockets, the `openssl` calls of the legacy protocol).
* Backtrace addresses of `static`/inlined functions are attributed to the
  previous exported symbol; check with
  `objdump -d --disassemble-symbols='<mangled name>' TeXmacs/bin/texmacs.bin`
  (find the mangled name with `nm`).
* Common causes met so far: uninitialized locals (`ui_signal sig;`),
  widgets outliving their window (dangling `win` pointers: windows call
  `vue_simple_widget_rep::forget_window`), commands freed while running
  (`dialogue-end` destroys the widget which owns the command), unbalanced
  clip commands from culled elements, duplicate Clay ids, render commands
  of a layout pass drawing widgets freed by a command run after that pass
  (crash in `vue_render_widget_fn`; now avoided by `gui_needs_relayout`).
* `-debug-io -debug-sockets` on the command line trace the client/server
  sockets (`socket_link_rep::...` lines, the `openssl` commands of the
  legacy protocol with the size of their output).
* `aborting process from uncaught error!` as the last line of the log is
  MuPDF exiting after a `fz_throw` outside `fz_try` (see the error handling
  section of `vue-graphics-stack.md`); the `TeXmacs] MuPDF error:` line
  before it names the failing operation. Wrap the call with
  `mupdf_protected` or one of the `mupdf_*` helpers.
* macOS blocks `screencapture` and CGEvent injection for processes without
  the corresponding permissions: use the snapshot and script aids instead
  (see `vue-testing.md`).
