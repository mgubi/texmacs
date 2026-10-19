# Scripted tests of the graphical interface

A test of the interface drives TeXmacs as a user would: it clicks, types and
chooses in menus, then checks what came out. The driver is in Scheme and the
same for every port; each port gives it a few hooks in C++. This page is
the reference; the plan behind it, with what is left to do, is
[plans/plan-gui-scripting.md](plans/plan-gui-scripting.md). The tests of
the Vue port, and how to write one, are in [vue-testing.md](vue-testing.md).

## How it runs

The steps of a test run from the event loop of the port, one at a time. The
driver asks the port (`gui-test-set-pump`) to call `gui-test-pump` at each
turn of its loop; that runs the next step when no event of the port is
waiting (`gui-test-busy?`) and the last `wait` is over, the rule of the old
player of Vue. A step usually ends the turn, so that the events it made
are handled before the next one; a choice of window lets the next step
follow at once.

| File | What |
|---|---|
| `TeXmacs/progs/check/gui-driver.scm` | The steps (`gt-...`), the pump, the checks (counted as those of `check-lib.scm`) and the exit status |
| `TeXmacs/progs/check/gui-script.scm` | The line language of the scripts, parsed into steps; `gui-script-start` runs a file |
| `src/Graphics/Gui/gui_test.hpp` | The hooks of a port, with the defaults in `gui_test.cpp` for a port without them |
| `src/Plugins/Vue/vue_gui.cpp` | The hooks of Vue (`script_exec`, the commands of its scripts, and the texts of the last frame) |
| `src/Plugins/Qt6/qt_test.cpp` (the same in `Qt/`) | The hooks of Qt, beside its older commands by label |
| `src/Plugins/NS/ns_test.mm` | The hooks of Cocoa |
| `tests/Plugins/Vue/run.sh` | Runs a test with a home of its own, and says PASS or FAIL (any port: run it from the top of the source tree of a build) |
| `tests/gui/` | The tests which run on every port, and `run.sh` which runs them all on a build |

`TEXMACS_TEST_SCRIPT=<file>` runs a script, on every port
(`TEXMACS_VUE_SCRIPT`, its former name, in Vue too): the port calls
`gui_test_start` as its event loop starts, which loads `(check gui-script)`
and calls `gui-script-start` at the first turn of the loop. The snapshots
go to `TEXMACS_TEST_SNAPSHOT` (or `TEXMACS_VUE_SNAPSHOT`); `run.sh` sets
both.

## The hooks of a port (`gui_test.hpp`)

| Hook | Scheme | What |
|---|---|---|
| `gui_test_port ()` | `gui-test-port` | The name of the port for its scripts (`vue`); `""` when it has no hooks |
| `gui_test_raw (line)` | `gui-test-raw` | A command of the port, as a line of a script; 0 run, 1 run (the next may follow in the same turn), 2 unknown |
| `gui_test_busy ()` | `gui-test-busy?` | Events are waiting |
| `gui_test_texts ()` | `gui-test-texts` | The texts and icons drawn last in the window of the script, as `x\ty\tw\th\tpath\ttext` (window points; the path below) |
| `gui_test_pump (on)` | `gui-test-set-pump` | Call `gui-test-pump` at each turn, while on; the script holds the mouse buttons meanwhile |
| `gui_test_eval_later (cmd)` | `gui-test-eval-later` | A Scheme command at the next turn, as `-x` (the same for all ports) |

Vue, Qt and Cocoa have them all; the other ports (X11, SDL, Qtwk) have
the defaults, which do nothing. Qt keeps its older commands too, which
find its widgets by their labels (`gui-test-click`, `gui-test-menu`,
`gui-test-type`...).

How each port does it:

| | Vue | Qt | Cocoa |
|---|---|---|---|
| Pump | at each turn of its loop | a timer of 10 ms (also in menus and modal dialogs) | a timer of 10 ms in the common modes of the run loop (also while a menu is tracked) |
| Busy | SDL events waiting | events of TeXmacs, delayed commands which are due, an update | the same, and the events of the system |
| Texts | drawn at the last frame (Clay) | the widgets of the window (buttons, labels, tabs, combo boxes, the rows of lists and trees in view), the popups (the list of a combo box) and the items of the open menus | the views of the window (buttons, labels, the rows of tables and outlines in view), the tabs and the items of the native menus, without a box |
| Bars | the layout notes them | the titles of the tool bars, the status bar (footer) | the identifiers of the rows (`TMButtonsController`) |
| Icons | the file of the picture widget | the property `tmIcon` (`qt_ui_element.cpp`) | the accessibility description of the image (`to_nsimage`) |
| Input | SDL events pushed | Qt events posted to the widget at the place (a menu is a window of its own) | Cocoa events posted to the window |
| Menus | drawn in the window, clicked | an item is chosen with the keyboard of its menu | items without a box, chosen by the port (`choose`) |
| Lists, tabs | clicked | clicked | a click on a row selects it (a table takes no click in a window which is not active, and a test does not make TeXmacs active); a tab is chosen by the port |
| Clipboard | of the script | of the system (offscreen, of the process) | of the system |

The menus of Cocoa are native: neither the menu bar nor the menus of the
buttons tell where their items are. Their items come last in the texts,
with no box (`0 0` for the size); `gt-activate` then asks the port to choose
the item (`choose <n>`, its place in the list): a submenu is listed from
then on, under the path of its item, and an item runs while the menus
close. The menus listed are those of the menu bar (`menubar > Insert >
...`), the one a button has open (its path is that of the button) and the
submenus chosen in them. A menu of Qt triggers an item at the release of
the button only when the window system said that the pointer moved onto
it, which posted events do not: a click on an item of a menu makes it the
active one and sends Return to the menu, as the keyboard would.

## The steps (`gui-driver.scm`)

```scheme
(gui-test-run
  (list (gt-wait 12000)                  ; ms
        (gt-raw "window #2")             ; a command of the port
        (gt-raw "focus")
        (gt-activate "footer > equation*")   ; click a text of the window
        (gt-activate "equation* > Displayed formula")
        (gt-activate "Displayed formula > Inline formula")
        (gt-idle)                        ; until nothing happens for a while
        (gt-check "inline" (lambda () (tree-is? (cursor-tree) :up 'math)))
        (gt-exit)))                      ; quit, with status 1 if a check failed
```

| Step | What |
|---|---|
| `(gt-wait ms)` | Pause |
| `(gt-raw line)` | A command of the port (below) |
| `(gt-eval-later "expr")` | A Scheme command at the next turn |
| `(gt-eval thunk)` | Run `thunk` now |
| `(gt-check what thunk)` | A check: `thunk` must return true; a failure is written `FAILED [] what: ...` |
| `(gt-idle [ms])` | Wait until the port has had nothing to do (`gui-test-busy?`) for 200 ms and five turns (at most 10 s): Qt on macOS flashes a chosen item of a menu before it runs it |
| `(gt-activate selector [ms])` | Click the middle of the text or icon of the window of the script which the selector names (below). Waits for it 3 s at most, then fails, with the texts it saw in the log |
| `(gt-see selector [ms])` | Wait until the text or icon is there (3 s at most, then fail) |
| `(gt-port name)` | The steps after it are for that port only; elsewhere the test stops there |
| `(gt-exit)` | At the end, quit TeXmacs with the status 1 if a check failed, else 0 |

For the checks: `(gt-record . strings)` is what a callback of the test
calls (it also writes `got: ...` to the log), `(gt-recorded)` the list of
what was recorded, in order; `(gt-document-has? text)` looks in the
document of the current buffer, `(gt-window? title)` in the titles of the
windows (`gui-test-windows`), and `(gt-find-text selector)` gives the box
of a text or `#f`. A dialog or a popup is opened with `gt-eval-later`, from
the loop of TeXmacs: a modal dialog opened by a step would hold it.

### The tests of every port

`tests/gui/run.sh [test...]`, from the top of the source tree of a build,
runs them all (or those named) and counts the failures:

| Test | What |
|---|---|
| `menu-formula` | Insert > Mathematics > Displayed formula from the menu bar, a letter typed in the formula |
| `dialog` | the fields of an interactive prompt, Tab and Return, then the keys go back to the editor |
| `submenu` | pull-down menus of a window, the first with a submenu |
| `tabs` | a tab widget shows the page of its tab, and only it |
| `font` | the font selector in a window, which opens and closes |
| `interactive` | the question in the footer, and its answer |
| `widgets` | a choice list, an enum, a filtered choice and a tree, two lists with the same items told apart by `named` |

They replace nothing: the tests of Vue with the same names check the
drawing too, by their snapshots.

### Selectors

A test names what it clicks as the user sees it, with nothing added to the
menus or the widgets: while a script runs, the layout notes where each text
and icon is, a path made of the bar it is in and of the labels of the
buttons whose menus it is in.

| Selector | Names |
|---|---|
| `Bold` | the text Bold, wherever it is |
| `menubar > Format` | the Format of the menu bar (not the one of a footer or a menu) |
| `Format > Font effects > Embold` | Embold in the submenu Font effects of the menu Format |
| `footer > equation*` | the tag equation* of the interactive footer |
| `mainbar > icon:tm_save` | the icon of the file `tm_save.svg` (or `.xpm`, `.png`...) in the main tool bar |
| `#second` | what a `(named "second" ...)` of the menus holds (below) |

The names before the last need not be the whole path, only some of it in
order: `Font effects > Embold` is enough. The bars are `menubar`,
`mainbar`, `modebar`, `focusbar`, `userbar`, `footer` (with its menus),
`prompt` (the question in the footer), `lefttools`, `sidetools`,
`bottomtools` and `extratools`. A name is the text itself, or the text
without the dots of an item which opens a dialog (`Font` for `Font...`),
or, for want of one, the beginning of a text. When several texts match,
the last one drawn is taken, the topmost (an open menu).

For the corner cases which the place does not settle (two buttons with the
same label side by side, an icon used twice in a bar), the menus may give
items a name, with nothing else changed:

```scheme
(named "first"  ("Same" (do-one)))
(named "second" ("Same" (do-other)))
```

The items are laid out as without it, and in Vue their path gets `#first`:
`#first > Same` names the button, and `#second` alone the text or icon in
`second` (the last drawn). On the ports without the hooks of the tests,
`named` is its items as they are. Use it only when a test needs it: the
places of the bars and menus are enough otherwise. The test `named` tries
it.

The names are written in English: a name is also taken in the language of
the interface (`translate`), so that `menubar > Insert` opens Insérer in a
French interface. The icons are named by their files, in every language.

## The line language (`gui-script.scm`)

Each line is a step:

```
# comment
wait <ms>
scheme <expression>
idle [ms]
activate <selector>
expect <text>          # the output must have this line (the runner checks it)
port <name>
exit
```

The other lines are the commands of the port. For Vue (`vue_gui.cpp`),
in window points from the top left of the content of the window of the
script:

```
window main | window <part of the title> | window #<id>
move x y
press x y [left|right|middle]       release x y [...]
click x y [left|right|middle] [n]   (n: a double or triple click)
wheel x y dx dy
key [S-][C-][A-][M-]<name> [text]   (Return, Escape, Down, Keypad_1...)
text <string>    commit <string>    compose [<text>]
focus    repaint    resize w h    close    tab <id>
drop x y <path> | drop x y text:<text>
snapshot <name>                     (in TEXMACS_VUE_SNAPSHOT)
clipboard <text>                    the text on the clipboard of the script
```

While a script runs, Vue keeps the clipboard to itself: a cut and a paste
of the test never go through the clipboard of the system, which the
session of the user shares (and the other tests running at the same time).
`clipboard <text>` puts a text on it, as if the system had it.

In Qt, `focus` only brings the window to the front: the keys of a script
go to the focus widget of its window anyway, and an activation would not
last (TeXmacs is not the active application of a test). Qt and Cocoa have
`window main` (the editor), `window <part of the title>`, `move`, `press`, `release`, `click`, `key`, `text`, `focus`,
`snapshot`, `resize`, `repaint` and `close`; Qt `wheel` and `clipboard`
too, Cocoa `choose`. `window main` is the first window with an editor on
every port, the way to write a test which runs on all of them; a `window`
which matches nothing lists the windows there are. Their keys use the names
of Vue (SDL): Return, Escape, Tab, Backspace, Delete, Left, Up, Home,
PageUp, F1...

The lines of the log are those of the old player: `<port> script: <line>`
for each step (`vue script:`, `qt script:`, `cocoa script:`), and `<port>
script: done` at the end.

## Pass or fail

`tests/Plugins/Vue/run.sh <test> [seconds]` runs a test of `tests/Plugins/Vue`
(`SCM=` and `SCRIPT=` give other files, by an absolute path, such as those
of `tests/gui`), from the top of the source tree of the build to test, Vue,
Qt or Cocoa. It says PASS or FAIL on its last
line, and so does its status. A test fails when TeXmacs crashed (`Error
message`), when its script did not end, when a check failed (`FAILED`), when
TeXmacs ended with a status other than 0, or when a line which an `expect`
asks for is missing from the output (the lines of the script itself do not
count). A script with `expect` lines stops as soon as it ended and they all
showed; the others run for the given time, since what they print may come
later (a `delayed` of their `.scm`).

The output is where `expect` looks, rather than the driver: under S7 the
output of Scheme goes to the standard output directly, not through TeXmacs,
which cannot see it.
