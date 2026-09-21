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
window <substring of title> | window #<id>    (default: the last created window;
                            prints its position and size on the screen)
move x y
press x y [left|right|middle]
release x y [left|right|middle]
click x y [left|right|middle]
wheel x y dx dy
key [S-][C-][A-][M-]<name>   an SDL key name (Return, Escape, Tab, Backspace, Down,
                            Home...) with shift/control/option/command prefixes
text <string>               one text-input event per character
resize w h
repaint                     invalidate every editor (repaint from scratch)
compose <text>              composition of an input method (no text: ends it)
drop x y <path>|text:<text> a drag and drop of one item at that position
focus                       pretend the target window got the keyboard focus
close                       close request on the target window
snapshot <name>
```

`update_mouse_state` returns the buttons held by the script while it is
active, so drags work. `text` sends one event per UTF-8 character. A test
instance launched while another application is in use never gets the
keyboard focus, and the editor's idle time (hence the pre-edits and the
`:idle` delayed commands) stays zero without it: `focus` fakes it. Callbacks print to standard output (`choice: ...`,
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
not every `texmacs.bin` (the user may be running one). Never kill an
instance while it boots and never boot two at once: since 2.1.5
`acquire_boot_lock` writes `~/.TeXmacs/system/boot_lock` at boot and removes
it once the event loop starts; a run which finds the lock assumes the last
boot crashed and **wipes the settings and the cache**, so the next run says
`Installation completed successfully`, opens a Welcome window (tools then go
to that window, `current-window`) and the snapshots of window `#2` are
meaningless — rerun. A `window` command which matches nothing prints
`vue script: no window matches` and the following commands are skipped
until a `window` command matches (they used to go to the last created
window, e.g. closing the main window).
`Error message:` in the log is a crash report with a C++ backtrace
(`get_crash_report`); addresses without symbols can be located with
`objdump -d --disassemble-symbols=<mangled>` on `texmacs.bin`.

Tests: `widgets` (choice, enum, toggle, filtered choice, tree, ink), `dialog`
(`interactive` prompt, tab order, keyboard routing), `dialogs` (color picker,
printer, popup window), `aligned` (aligned rows, splitter drags), `tabs`,
`resize`, `styles`, `font` (open and close the font selector window;
`open-font-selector` itself uses a side tool when the "side tools" preference
is on, so the test calls `open-font-selector-window`), `popup` (the context menu of the editor opens with its corner at the pointer:
compare the `set_position` line with the window position and the click;
an item runs and closes it; near the screen border it is moved back) and
`menus` (pull-down menus, flipping/scrolling), `checks`
(menu check marks), `tools` (side and bottom tools), `prefs-tool` (the
section tabs of the preferences tool react to clicks), `prefs-dialog` (the
preferences window with icon tabs: tabs of equal height), `two-tools` (tools at
the top and bottom of both sides), `tools-close` (replacing a tool, adding a
bottom one, closing the top one; the paper follows the canvas),
`tool-replace` (replacing the font tool, whose sample text is an editor, by
another tool must not crash the redraw), `pattern` (paper mode and text
filled with patterns: the MuPDF renderer's `draw_bis` and tiling patterns),
`macro-editor` (the macro editor dialog: typing goes into the embedded
editor), `macros-editor` (the macros editor dialog: selecting a macro in the
list updates the embedded editor), `macro-tool` (the macro editor as a side
tool, `side-tools?` forced), `macros-tool` (the macros editor as a side tool:
list inside its box, selection rebuilds the tool without misdrawn widgets), `sockets` (the TeXmacs server and an anonymous legacy client in the same
instance: `SOCKETS roundtrip: ((server license ...` in the log; it creates
the server database `~/.TeXmacs/server` with an admin account and switches
the `tls-server` preference off for its duration), `sockets-tls` (the same
over TLS, run by hand: `TEXMACS_SERVER_CERT_DIR=<scratch dir>
texmacs.bin -tls-no-verify -x '(load ".../sockets-tls.scm")'` generates a
self-signed certificate there and expects `SOCKETS-TLS roundtrip:`; the
`GnuTLS ERROR (-110)` for one client is the losing half of the dual
IPv4/IPv6 connection attempt), `input-edit` (editing in a text input: select all and
replace, word selection, cut and paste, `got: Bob Smith / 42`), `drop` (a dropped file name and a dropped piece of text reach
mouse-drop-event and are inserted), `entrypoints` (the wait indicator
appears over the window and is popped by the empty message, the help
balloon appears and a pointer motion dismisses it), `pre-edit` (the composition of an input method — a dead key, a letter — is
shown in a pre-edit box and the committed text replaces it), `debug-view` (the Clay debug view of F1 over a window with a tool, hover
and click while it is shown), `focus-windows` (the keyboard focus moves
from a prompt to the editor and back: `got: BobBy / 42`), `scroll-shift` (scrolling
shifts the backing store: the snapshots before and after a `repaint` must
be identical in the editor area — the footer may show another welcome
message), `wheel-inertia` (a single wheel step
scrolls in sync — snapshots i0/i1/i2 are
identical — while three quick steps launch a glide: the `handling wheel`
lines of the log after i3 are the synthetic decaying deltas).

## Writing a test

1. Build the widget in a `.scm` file with the `tm-widget` markup and open it
   after boot with `(delayed (:pause 1500) (top-window my-widget "Title"))`;
   modules of real dialogs must be imported with `use-modules` since `-x`
   runs before lazy loading. Use `:pause`, not `:idle`: the idle time of the
   editor is zero while its window has no keyboard focus, and a test
   instance launched while another TeXmacs is in use never gets it.
2. Take a first snapshot, read the pixel positions from the PNG (divide by 2
   for points) and write the clicks.
3. Print the callback results with `display*` and check them in the log.
