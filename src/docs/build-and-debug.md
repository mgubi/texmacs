# Building, running and debugging

## Configure and build

The worktree `wip_other_guis/src` is configured with

    ./configure --with-gui=vue --with-mupdf=/opt/homebrew --with-sdl3 \
      --with-guile=/Users/mgubi/t/guile-1.8.7/usr/bin/guile-config --with-gnutls

(see the first lines of `config.log`; the Guile 1.8 in `~/t/guile-1.8.7` is
used, the Homebrew Guile 3 is rejected; `--with-gnutls`, added on
2026-09-21, enables the TLS protocol of the TeXmacs client/server through
the Homebrew GnuTLS and needs a full rebuild since it changes `config.h`).
`--with-resvg` is *not* needed: MuPDF draws the SVG icon sets itself, see
*Icons* in [vue-graphics-stack.md](vue-graphics-stack.md). `make` at the root builds
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

## Choosing the GUI

`--with-gui=` selects the port (`misc/m4/tm_gui.m4`); whether MuPDF is used
is decided after it (`TM_MUPDF_FOR_GUI` in `misc/m4/mupdf.m4`), since the
ports do not all draw their pictures the same way:

| `--with-gui=` | define | MuPDF | plugin directories |
|---|---|---|---|
| `qt` (default) | `QTTEXMACS` | optional | `Qt` (or `Qt6`), `MacOS` |
| `qtwk` | `QTWKTEXMACS`, `QTTEXMACS` | optional | `Qtwk`, `Widkit`, a few files of `Qt` |
| `x11` | `X11TEXMACS` | not usable | `X11`, `Widkit` |
| `cocoa` or `aqua` | `AQUATEXMACS` | not usable | `Cocoa`, `MacOS` |
| `sdl` | `SDLTEXMACS` | required | `SDL`, `Widkit` |
| `vue` | `VUETEXMACS` | required | `Vue` |

With MuPDF, `MUPDF_RENDERER` makes MuPDF the screen renderer and picture
type; X11 and Cocoa have pictures of their own, which clash with MuPDF's at
link time, so for them an explicit `--with-mupdf` is an error and a MuPDF
found by itself is left out. SDL and Vue stop at configure time without
MuPDF. The X11 port needs the X11 headers: with Homebrew's `libx11`, pass
`--x-includes=/opt/homebrew/include --x-libraries=/opt/homebrew/lib`
(configure stops if it finds none).

Checked on 2026-09-26 (macOS, clean builds): qt with and without MuPDF,
qtwk, x11, cocoa, sdl and vue all build; x11 and cocoa with `--with-mupdf`,
sdl and vue without it, x11 without X11 headers and an unknown GUI all stop
in configure with a message. Only the Vue and SDL ports were run.

## Syncing with upstream

Upstream TeXmacs (the SVN trunk) is mirrored in the `svn_sync` branch of the
main checkout (`~/t/git/texmacs`); the Vue work was merged with it on
2026-09-21 (TeXmacs 2.1.5, 791 commits). What conflicts and what to check:

* the generated `configure` and `src/System/config.in`: do not merge them,
  regenerate (`autoconf`, `autoheader`) after resolving `configure.in` and
  `aclocal.m4` (which list `mupdf.m4`, `sdl3.m4` and `resvg.m4` next to the
  upstream macros; `LC_SDL3` is called from `misc/m4/tm_gui.m4`, inside the
  `sdl` and `vue` cases, so a `--with-gui` conflict and an sdl3 conflict
  arrive together);
* `src/makefile.in`: the MuPDF/SDL/Vue variables, sources and object rules
  live next to the upstream ones; upstream selects the Qt plugin directory
  with `QT_PLUGIN_DIR` (`Qt` or `Qt6`), the Widkit-on-Qt port uses
  `QT_SRC_DIR= Qt`;
* `System/Link`: the client/server sockets used to be Qt-only; they are now
  `tm_sockets.cpp` (guarded by `#ifndef QTTEXMACS`) driven by
  `socket_notifier.*`, and the Qt build keeps `Plugins/Qt/QTMSockets.cpp`.
  Keep the two implementations in sync with each other and with
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
command of every frame (`DUMP <type> id <id> box x,y wxh ...`, where
`<type>` is Clay's raw command-type number, not a name; border commands are
matched against the usual id patterns) — heavy, for tracking down a stray
element.

`TEXMACS_VUE_PROFILE=<n>` prints, every n frames of the event loop (300 by
default), where the time of a frame went: one line per phase of
`gui_start_loop` with its share, its mean and the worst frame, then the
replay of the render commands broken down by Clay command type and, for the
custom commands, into the texts, the editors' backing stores and the rest.
It measures wall time with `SDL_GetTicksNS` and costs nothing when the
variable is unset. Drive it with a script (`TEXMACS_VUE_SCRIPT`) so that a
measurement can be repeated: a scroll, a series of keystrokes and a pointer
moving over a tool bar are the three worth watching.

`TEXMACS_VUE_THEME=light|dark` and `TEXMACS_VUE_DENSITY=<x>` force
the appearance and the resolution, which is how a theme or a HiDPI bug is
reproduced on any screen; `TEXMACS_VUE_SNAPSHOT` and `TEXMACS_VUE_SCRIPT`
are described in [vue-testing.md](vue-testing.md).

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
  of a layout pass drawing widgets freed by a command, or by the repaint,
  run after that pass (crash in `vue_render_widget_fn`; the layout now
  holds a reference to every widget its commands name, see `render_ref`).
* `TeXmacs] Clay error (<n>)` in the log is a problem Clay reported to
  `HandleClayErrors`; Clay skips the offending element and carries on, so
  the interface stays up with something missing from it. Each kind is
  printed three times and then suppressed. Under each message the handler
  prints any internal array which is at its capacity, the element and
  render command counts, the frame number, the window, and the type of the
  last widget which began to lay itself out. That last one is a clue, not a
  culprit: an error raised while Clay computes the tree, after every widget
  has run, names whichever widget happened to be last. Error 7 is the one
  which needs the extra information: Clay reports an array which has run
  out of room and a genuine out of bounds read with the same text, and a
  line saying whether an array is full tells them apart. A full array wants
  a larger capacity (`Clay_SetMaxElementCount` and friends); none full is a
  bug worth reporting to Clay.
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
