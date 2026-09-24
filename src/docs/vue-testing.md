# Testing the Vue GUI

Neither screenshots (`screencapture`) nor synthetic input (CGEvent) are
available to a process without the macOS permissions, so the plugin has its
own in-process development aids. Each is switched on by an environment
variable and inactive otherwise: `TEXMACS_VUE_SNAPSHOT` and
`TEXMACS_VUE_SCRIPT` below, `TEXMACS_VUE_THEME` and `TEXMACS_VUE_DENSITY` to
force the appearance and the resolution, and `TEXMACS_VUE_DUMP` to print
every Clay render command of every frame (type, id, box, colour, label).

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
resize w h                  resize the target window (points)
repaint                     invalidate every editor (repaint from scratch)
compose <text>              composition of an input method (no text: ends it)
drop x y <path>|text:<text> a drag and drop of one item at that position
focus                       pretend the target window got the keyboard focus
snapshot <name>             save the next redraw of the target window
close                       close request on the target window
```

`update_mouse_state` returns the buttons held by the script while it is
active, so drags work. `text` sends one event per UTF-8 character. A test
instance launched while another application is in use never gets the
keyboard focus, and the editor's idle time (hence the pre-edits and the
`:idle` delayed commands) stays zero without it: `focus` fakes it. The tests
are checked on what the callbacks print to standard output with `display*`
(`choice: ...`, `got: ...`), and a few on lines the code logs, which then
need `-debug-events` on the command line.

`TEXMACS_VUE_THEME=light|dark` forces the theme of the interface. The
`theme` and `icons` tests each render one window under whichever theme is
forced, so comparing the two means running them twice; `icons` shows one row
of buttons per icon set, drawn from the vector icons of
`TeXmacs/misc/pixmaps/light` and `TeXmacs/misc/pixmaps/dark`.
`TEXMACS_VUE_DENSITY=1` runs at one device pixel per point, the layout of a
display without HiDPI whatever the screen: worth a pass over the visual
tests after a change to the sizing of the widgets.

## Running the tests

`src/Plugins/Vue/tests/` holds Scheme files building test widgets (loaded with
`-x`) and the scripts driving them (`README` there). Typical run:

```sh
export TEXMACS_PATH=$PWD/TeXmacs
export TEXMACS_VUE_SNAPSHOT=/tmp/snap TEXMACS_VUE_SCRIPT=src/Plugins/Vue/tests/widgets.script
TeXmacs/bin/texmacs.bin -x '(load "src/Plugins/Vue/tests/widgets.scm")' > /tmp/run.log 2>&1 &
TEST=$!
sleep 25; kill -9 $TEST
grep -n 'choice:\|Error message\|vue script: done' /tmp/run.log
```

Boot takes 5 to 15 s, more under load: scripts start with `wait 5000` or
more, and a missing `vue script: done` usually means the run was killed too
early. Leave a few seconds between two runs and kill only the test process,
as above, not every `texmacs.bin`, since the user may be running one.

**Never boot two instances at once, and never kill one while it boots.**
`acquire_boot_lock` writes `~/.TeXmacs/system/boot_lock` at boot and removes
it once the event loop starts. A run which finds the lock assumes the last
boot crashed and **wipes the settings and the cache**. The next run then
says `Installation completed successfully`, opens a Welcome window, which
the tools go to rather than to the window you meant (`current-window`), and
the snapshots of window `#2` are meaningless. Rerun it.

Two lines in the log are worth grepping for. `vue script: no window matches`
means a `window` command matched nothing; the commands after it are skipped
until another `window` command matches, so nothing is sent to the wrong
window. `Error message:` is a crash report with a C++ backtrace
(`get_crash_report`); addresses without symbols can be located with
`objdump -d --disassemble-symbols=<mangled>` on `texmacs.bin`.

## The tests

Each test is a `<name>.script`, most of them with a `<name>.scm` building the
widget it drives. Two of the checks need `-debug-events` on the command
line, as marked, because the lines they look for are traces rather than
callback output.

| Test | What it checks |
|---|---|
| `widgets` | choice, multiple choice, enum, toggle, filtered choice, tree, ink |
| `dialog` | an `interactive` prompt: tab order, keyboard routing |
| `dialogs` | colour picker, printer dialog, popup window |
| `palette` | the colour palette of the document "Color" menu, in a popup sized to its contents: flat cells, framed only by the highlight of the one hovered, sitting next to each other |
| `choice-style` | the four styles of a choice list side by side; a click on the inert one neither selects nor calls back |
| `input-edit` | editing in a text input: select all and replace, word selection, cut and paste (`got: Bob Smith / 42`) |
| `pre-edit` | the composition of an input method in the editor: a dead key then a letter, shown in a pre-edit box and replaced by the committed text |
| `pre-edit-input` | the same inside a dialog field |
| `focus-windows` | the keyboard focus moves from a prompt to the editor and back (`got: BobBy / 42`) |
| `aligned` | aligned rows and splitter drags |
| `tabs` | switching tabs, and the layout of each page |
| `prefs-tool` | the section tabs of the preferences tool react to clicks |
| `prefs-dialog` | the preferences window with icon tabs: tabs of equal height |
| `menus` | a pull-down menu opens and closes; in a window narrowed to 640x200 the Help menu is shifted back inside |
| `submenu` | a submenu opens without closing its parent; another menu of the bar closes both |
| `checks` | the check marks of the View menu |
| `popup` | the context menu of the editor opens with its corner at the pointer, an item runs and closes it, near the screen border it is moved back. Needs `-debug-events`: the check compares the logged `set_position` with the window position and the click |
| `icons` | one row of buttons per icon set: a vector icon must be drawn at the size of its set, so the flags of `16x16/focus`, whose files declare widths of 600 to 1500, must not be larger than the rest. Run it under both themes |
| `theme` | the widgets under the theme forced by `TEXMACS_VUE_THEME` |
| `font` | open the font selector window and close it from its title bar |
| `macro-editor` | the macro editor dialog: typing goes into the embedded editor |
| `macros-editor` | the macros editor dialog: selecting a macro updates the embedded editor |
| `macro-tool` | the macro editor as a side tool, `side-tools?` forced |
| `macros-tool` | the macros editor as a side tool: list inside its box, selection rebuilds the tool without misdrawn widgets |
| `two-tools` | three tools at once: top right, bottom right and left |
| `tools-close` | replacing a tool, adding a bottom one, closing the top one; the paper follows the canvas |
| `tool-replace` | replacing the font tool, whose sample text is an editor, must not crash the redraw |
| `debug-view` | the Clay debug view of F1 over a window with a tool; hover and click while it is shown |
| `drop` | a dropped file name and a dropped piece of text reach `mouse-drop-event` and are inserted |
| `entrypoints` | the wait indicator appears over the window and is popped by the empty message; the help balloon appears and a pointer motion dismisses it |
| `figures` | PDF figures drawn by MuPDF as drawing (a form XObject), upright and turned by `/Rotate 90`, and at a size where pixels would show; the figures are those of `Plugins/MuPDF/tests` |
| `pattern` | paper mode and glyphs filled with patterns: the MuPDF renderer's `draw_bis` and tiling patterns |
| `wheel` | scrolling with the wheel, then a balloon from a hovered toolbar button |
| `title` | the title of the window names the document (`No name [n]`) and takes a `*` at the first change. Needs `-debug-qt-widgets`: the checks are the `window title` lines |
| `bars` | contents which do not fit: in a narrow window the bars are clipped and carry a marker at the end (b1), a wheel turned over one scrolls it sideways (b2) and a click on its marker brings the next screenful in (b3), and a window made wide enough for the whole of them puts them back at their first button with no marker (b4); a menu taller than the window is shifted up to fit inside it and marked at the bottom (m1), and the wheel scrolls it (m2). No scroll bar anywhere |
| `wheel-travel` | a wheel notch is travelled over some twenty frames rather than jumped (i0, 30 ms in, has moved much less than i1) and several notches add up (i2); a swipe which opens with a whole delta scrolls the ten points of a unit and not the eighty of a notch. Needs `-debug-events`: the checks are on the deltas of the `handling wheel` lines, see the script |
| `scroll-shift` | scrolling shifts the backing store: the snapshots before and after a `repaint` must be identical in the editor area |
| `balloon` | the help balloon of a menu item: near the pointer, above everything, not clipped by the menu and not covering the next item |
| `styles` | the widget styles side by side, to compare with the Qt port: bold, mini, monospaced, grey, inert and centered |
| `tmoutput` | the extent of the typeset boxes: on its own a `texmacs-output` is as wide as what it typesets, and inside a `resize` it fills the pane it was given |
| `interactive` | the query line of the footer: the prompt and the field appear in place of the footer, the answer comes back (`answer: Bob`). It sets the "interactive questions" preference and puts it back in the same turn, since a test must not leave the settings changed |
| `search-focus` | the search toolbar asks for the keyboard with `keyboard-focus-on`, so what is typed next lands in its field rather than in the document |
| `sockets` | the TeXmacs server and an anonymous legacy client in the same instance (see below) |

One more is not run this way. `sockets-tls` repeats the `sockets` exchange
over TLS and is run by hand:

```sh
TEXMACS_SERVER_CERT_DIR=<scratch dir> \
  TeXmacs/bin/texmacs.bin -tls-no-verify \
  -x '(load "src/Plugins/Vue/tests/sockets-tls.scm")'
```

It generates a self-signed certificate there and expects `SOCKETS-TLS
roundtrip:` in the log; the `GnuTLS ERROR (-110)` reported for one client is
the losing half of the dual IPv4/IPv6 connection attempt. The `sockets` test
creates the server database `~/.TeXmacs/server` with an admin account and
switches the `tls-server` preference off for its duration, and its log line
is `SOCKETS roundtrip: ((server license ...`.

`resize.scm`, `styles.scm` and `tools.scm` have no script: they open a window
to be looked at (a window sized from a `resize` widget, the widget styles
side by side, and the three tool areas of the main window).

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
