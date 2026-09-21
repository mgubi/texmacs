# Testing the Vue GUI

Neither screenshots (`screencapture`) nor synthetic input (CGEvent) are
available to a process without the macOS permissions, so the plugin has two
in-process development aids, both controlled by environment variables and
inactive otherwise.

## Snapshots

`TEXMACS_VUE_SNAPSHOT=<dir>`: every redraw of a window writes
`<dir>/window-<id>.png` (the rendering, without decorations). Window ids: the
main window is 2, later windows 4, 6, ... (each window consumes two serials).
A script command `snapshot <name>` additionally saves the next redraw of the
target window as `<dir>/<name>.png`.

## Scripted events

`TEXMACS_VUE_SCRIPT=<file>`: the event loop replays the file line by line
(`script_step` in `vue_gui.cpp`), one command per iteration and only when no
SDL event is pending, by pushing synthetic SDL events. Coordinates are window
*points* relative to the content area of the target window.

```
# comment
wait <ms>
window <substring of title> | window #<id>    (default: the last created window)
move x y
press x y [left|right|middle]
release x y [left|right|middle]
click x y [left|right|middle]
wheel x y dx dy
key <SDL key name>          e.g. Return, Escape, Tab, Backspace, Down
text <string>               one text-input event per character
resize w h
close                       close request on the target window
snapshot <name>
```

`update_mouse_state` returns the buttons held by the script while it is
active, so drags work. Callbacks print to standard output (`choice: ...`,
`got: ...`, `Click!!`), which is how the tests are checked.

## Running the tests

`src/Plugins/Vue/tests/` holds Scheme files building test widgets (loaded with
`-x`) and the scripts driving them (`README` there). Typical run:

```sh
export TEXMACS_PATH=$PWD/TeXmacs
export TEXMACS_VUE_SNAPSHOT=/tmp/snap TEXMACS_VUE_SCRIPT=src/Plugins/Vue/tests/widgets.script
TeXmacs/bin/texmacs.bin -x '(load "src/Plugins/Vue/tests/widgets.scm")' > /tmp/run.log 2>&1 &
sleep 25; pkill -9 -f texmacs.bin
grep -n 'choice:\|Error message\|vue script: done' /tmp/run.log
```

Boot takes 5–15 s (more under load): scripts start with `wait 5000` or more,
and a missing `vue script: done` usually means the run was killed too early.
Leave a few seconds between two runs and kill only the test process (`$!`),
not every `texmacs.bin` (the user may be running one). If the log says
`Installation completed successfully`, the settings file could not be read
and a Welcome window opened: tools then go to that window (`current-window`)
and the snapshots of window `#2` are meaningless — rerun.
`Error message:` in the log is a crash report with a C++ backtrace
(`get_crash_report`); addresses without symbols can be located with
`objdump -d --disassemble-symbols=<mangled>` on `texmacs.bin`.

Tests: `widgets` (choice, enum, toggle, filtered choice, tree, ink), `dialog`
(`interactive` prompt, tab order, keyboard routing), `dialogs` (color picker,
printer, popup window), `aligned` (aligned rows, splitter drags), `tabs`,
`resize`, `styles`, `font` (open and close the font selector), `popup` and
`menus` (context menu and pull-down menus, flipping/scrolling), `checks`
(menu check marks), `tools` (side and bottom tools), `prefs-tool` (the
section tabs of the preferences tool react to clicks), `two-tools` (tools at
the top and bottom of both sides), `tools-close` (replacing a tool, adding a
bottom one, closing the top one; the paper follows the canvas),
`wheel-inertia` (a single wheel step scrolls in sync — snapshots i0/i1/i2 are
identical — while three quick steps launch a glide: the `handling wheel`
lines of the log after i3 are the synthetic decaying deltas).

## Writing a test

1. Build the widget in a `.scm` file with the `tm-widget` markup and open it
   after boot with `(delayed (:idle 1500) (top-window my-widget "Title"))`;
   modules of real dialogs must be imported with `use-modules` since `-x`
   runs before lazy loading.
2. Take a first snapshot, read the pixel positions from the PNG (divide by 2
   for points) and write the clicks.
3. Print the callback results with `display*` and check them in the log.
