# Building, running and debugging

## Configure and build

The worktree `wip_other_guis/src` is configured with

    ./configure --with-gui=vue --with-mupdf=/opt/homebrew --with-sdl3

(see the first lines of `config.log`). `make` at the root builds
`TeXmacs/bin/texmacs.bin`; `src/makefile` is generated from
`src/makefile.in`, so permanent changes go to `makefile.in`.

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

## Running

    TEXMACS_PATH=$PWD/TeXmacs TeXmacs/bin/texmacs.bin [-x "(scheme code)"]

`-x` runs Scheme after boot; modules not yet loaded must be imported with
`use-modules`. Preferences live in `~/.TeXmacs/system/preferences.scm`
(`set-preference` saves them: do not change them from tests).

Useful keys: F1 toggles the Clay debug view of the focused window (and the red
marker of uncovered areas).

## Debugging

* Standard output carries the widget traces (`unhandled SLOT_...` for slots a
  widget does not implement, `run command ...`, `Click!!`) and crash reports
  (`Error message:` + C++ backtrace). Signals are turned into C++ exceptions
  by `tm_throw`, so a crash prints the report and aborts.
* Backtrace addresses of `static`/inlined functions are attributed to the
  previous exported symbol; check with
  `objdump -d --disassemble-symbols='<mangled name>' TeXmacs/bin/texmacs.bin`
  (find the mangled name with `nm`).
* Common causes met so far: uninitialized locals (`ui_signal sig;`),
  widgets outliving their window (dangling `win` pointers: windows call
  `vue_simple_widget_rep::forget_window`), commands freed while running
  (`dialogue-end` destroys the widget which owns the command), unbalanced
  clip commands from culled elements, duplicate Clay ids.
* macOS blocks `screencapture` and CGEvent injection for processes without
  the corresponding permissions: use the snapshot and script aids instead
  (see `vue-testing.md`).
