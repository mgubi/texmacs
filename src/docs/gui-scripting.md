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
| `tests/Plugins/Vue/run.sh` | Runs a test of Vue with a home of its own, and says PASS or FAIL |

`TEXMACS_VUE_SCRIPT=<file>` runs a script in Vue, as before: the port
loads `(check gui-script)` and calls `gui-script-start` at the first turn
of its loop.

## The hooks of a port (`gui_test.hpp`)

| Hook | Scheme | What |
|---|---|---|
| `gui_test_port ()` | `gui-test-port` | The name of the port for its scripts (`vue`); `""` when it has no hooks |
| `gui_test_raw (line)` | `gui-test-raw` | A command of the port, as a line of a script; 0 run, 1 run (the next may follow in the same turn), 2 unknown |
| `gui_test_busy ()` | `gui-test-busy?` | Events are waiting |
| `gui_test_texts ()` | `gui-test-texts` | The texts drawn last in the window of the script, as `x\ty\tw\th\ttext` (window points) |
| `gui_test_pump (on)` | `gui-test-set-pump` | Call `gui-test-pump` at each turn, while on; the script holds the mouse buttons meanwhile |
| `gui_test_eval_later (cmd)` | `gui-test-eval-later` | A Scheme command at the next turn, as `-x` (the same for all ports) |

Vue has them all. The other ports have the defaults, which do nothing;
Qt keeps its own commands, which find its widgets by their labels
(`gui-test-click`, `gui-test-menu`, `gui-test-type`...; `qt_test.cpp`).

## The steps (`gui-driver.scm`)

```scheme
(gui-test-run
  (list (gt-wait 12000)                  ; ms
        (gt-raw "window #2")             ; a command of the port
        (gt-raw "focus")
        (gt-activate "equation*")        ; click a text of the window
        (gt-activate "Displayed formula")
        (gt-activate "Inline formula")
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
| `(gt-idle [ms])` | Wait until the port has had no event for five turns (at most 10 s) |
| `(gt-activate text [ms])` | Click the middle of the text drawn last in the window of the script which is `text`, or starts with it; the last one drawn is the topmost (a menu). Waits for it 3 s at most, then fails |
| `(gt-port name)` | The steps after it are for that port only; elsewhere the test stops there |
| `(gt-exit)` | At the end, quit TeXmacs with the status 1 if a check failed, else 0 |

`gt-activate` finds what is drawn, in the language of the interface: a test
which uses it runs in English (the copy of the home which `run.sh` makes
has the language of the user; a test may set it).

## The line language (`gui-script.scm`)

Each line is a step:

```
# comment
wait <ms>
scheme <expression>
idle [ms]
activate <text>
expect <text>          # the output must have this line (the runner checks it)
port <name>
exit
```

The other lines are the commands of the port. For Vue (`vue_gui.cpp`),
in window points from the top left of the content of the window of the
script:

```
window <part of the title> | window #<id>
move x y
press x y [left|right|middle]       release x y [...]
click x y [left|right|middle] [n]   (n: a double or triple click)
wheel x y dx dy
key [S-][C-][A-][M-]<name> [text]   (Return, Escape, Down, Keypad_1...)
text <string>    commit <string>    compose [<text>]
focus    repaint    resize w h    close    tab <id>
drop x y <path> | drop x y text:<text>
snapshot <name>                     (in TEXMACS_VUE_SNAPSHOT)
```

The lines of the log are those of the old player: `vue script: <line>` for
each step, `vue script: done` at the end.

## Pass or fail

`tests/Plugins/Vue/run.sh <test> [seconds]` says PASS or FAIL on its last
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
