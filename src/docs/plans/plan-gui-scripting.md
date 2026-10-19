# Plan: GUI scripting for every interface

Status: proposal, 2026-10-19. Based on a read-only checkout of
`upsilon_texmacs` (`~/t/lab/fix/plan-ref`). Paths below are relative to
`src/` of that tree unless they start with `.github/`.

## 1. Summary

Today there are four separate ways to drive a TeXmacs GUI from a test:

1. the Vue `.script` line language, interpreted in C++ (`script_step`), which
   pushes synthetic SDL events, with 60 scripted tests;
2. the Cocoa port's `TEXMACS_NS_*` environment variables (one-shot type,
   click, press-by-label, window steps, benchmarks);
3. the Qt `gui-test-*` Scheme primitives (`qt_test.cpp`): click by label,
   menu by path, type, snapshot, answer modal dialogs;
4. the browser `--script` language of `misc/wasm/browser-run.mjs`
   (puppeteer), plus `eval` of JavaScript that calls Scheme.

None of them gives an exit status. Checks are `grep`s on the log or a look
at PNGs. Only Qt has a Scheme API, and it is the most portable of the four,
because it addresses widgets by label rather than by pixel.

Recommendation: a single port-independent **Scheme driver**
(`(check gui-driver)`) with a step runner, idle synchronisation, assertions
and an exit status. Under it sits a **small C++ backend interface**
(`gui_test_*`) that each port implements: inspect the widget tree, trigger a
widget by label or name, inject raw events, report idle, take a snapshot.
Scheme is the canonical test language. The existing line language stays as
sugar: it is parsed in Scheme into the same steps, so the 60 Vue tests keep
working and can be rewritten piece by piece. Effort: about 5 to 7 weeks for
Vue, Qt and Cocoa with CI. Widkit/X11 and accessibility come later.

## 2. What exists today

### 2.1 Vue: scripted SDL events (`src/Plugins/Vue/vue_gui.cpp`)

**Activation.** `script_init` (vue_gui.cpp:4521) reads
`TEXMACS_VUE_SCRIPT` and splits the file into lines. It is called before the
main loop (vue_gui.cpp:4414). The file comment at vue_gui.cpp:4480-4514 and
`docs/vue-testing.md` ("Scripted events") document the command set.

**Pump.** `loop_iteration_body` calls `script_step ()` first, at
vue_gui.cpp:4240. `script_step` (vue_gui.cpp:4605-4844) does the following:

* it returns at once while any SDL event is pending
  (`SDL_PollEvent (NULL)`, 4607). This is the only idle synchronisation,
  and it covers the SDL queue, not TeXmacs' delayed commands, typesetting
  or redraws;
* it honours `wait <ms>` against `texmacs_time ()` (4609, 4618), in wall
  clock time;
* it executes **one command per loop iteration** (the `return` at 4840)
  and prints each line as `vue script: <line>`.

**Targeting.** `window <substring>` or `window #<id>` (4622-4643) matches
the title or the id in `id_to_window`. When nothing matches,
`script_no_target` skips the commands until the next `window` command, so
input never reaches the wrong window. The default target is
`last_created_window` (`script_target`, 4537). For virtual windows (tabs in
single-window mode) `script_window_id` (4548-4559) shifts the coordinates
into the SDL host window, so the routing of single-window mode is exercised
too.

**Injection level: raw SDL events.** Every input command builds an
`SDL_Event` and calls `SDL_PushEvent`:

* `move`, `press`, `release` and `click` (with a click count) through
  `script_push_motion` and `script_push_button` (4574-4603);
* `wheel` (4660);
* `key [S-][C-][A-][M-]<SDL name> [text]`: a scancode taken from the SDL
  key name plus modifiers, optionally followed by a TEXT_INPUT event
  (4677-4712);
* `text`: one TEXT_INPUT per UTF-8 character, with buffers kept alive until
  the next command (4713-4738);
* `commit` and `compose` (TEXT_EDITING) for input methods;
* `drop` (DROP_BEGIN, then DROP_FILE or DROP_TEXT, then DROP_COMPLETE);
* `focus` (a fake FOCUS_GAINED, needed because a background test instance
  never gets the focus, so `:idle` delays never fire);
* `close` (CLOSE_REQUESTED, or `destroy_event` for a virtual window).

`mouse_buttons ()` returns the buttons the script holds while a script is
active (vue_gui.cpp:4886), so drags work without real hardware state.

**Non-input commands:**

* `resize w h`: `set_size`;
* `repaint`: `invalidate_all_editors`;
* `tab <id>`;
* `scheme <expr>`: `exec_delayed`, run at the next loop turn (4760).

**Snapshots.** With `TEXMACS_VUE_SNAPSHOT=<dir>`, every redraw writes
`window-<id>.png`. `snapshot <name>` sets `snapshot_win` and
`snapshot_name`, and the next redraw of that window (or of the host of a
virtual window) is saved as `<name>.png` (vue_gui.cpp:1220-1233 and
1379-1392). `frame_wanted ()` forces a frame while a snapshot is pending
(vue_gui.cpp:3903-3906). The pixels come from the renderer's own pixmap, so
the pipeline runs in-process and needs no screen capture permission.

Diagnostics: `TEXMACS_VUE_DUMP` prints every Clay render command, with its
type, id, box and widget kind (vue_gui.cpp:1413). This is the closest thing
Vue has to a widget-tree inspector.

**Tests** (`tests/Plugins/Vue/`). There are 60 `.script` files and 59
`.scm` files. Each `.scm` builds a widget with
`(delayed (:pause 1500) (top-window ...))`, and its callbacks
`display* "got: ..."`. The runner `run.sh` copies `~/.TeXmacs_Upsilon` into
a scratch home, starts `texmacs.bin -x '(load x.scm)'` with
`TEXMACS_VUE_SCRIPT` set, **sleeps a fixed number of seconds, then sends
`kill -9`**, and greps for `choice:|got:|enum:|answer:|Error message|vue
script: done`. There is no exit status. A person reads the output and
compares it with the expectation written in `docs/vue-testing.md`, and
looks at the PNGs. Command counts over all scripts:

| cmd | count | cmd | count |
|---|---|---|---|
| wait | 499 | text | 31 |
| scheme | 164 | wheel | 23 |
| snapshot | 146 | focus | 11 |
| key | 104 | press / release | 7 / 7 |
| click | 70 | resize | 6 |
| window | 69 | compose | 6 |
| move | 48 | repaint, drop, close | 2, 2, 1 |

`wait` makes up about 40% of all lines. Fixed waits (`wait 5000` for boot,
`wait 800` after a click) are the main source of slowness and flakiness. A
third of the `scheme` lines are probes or settings, which shows that tests
already mix the line language with Scheme. Every click is a pixel
coordinate read off a PNG: test-writing step 2 in vue-testing.md says
"read the pixel positions from the PNG", so any layout change breaks the
tests.

### 2.2 Qt: `gui-test-*` primitives (`src/Plugins/Qt/qt_test.cpp`, Qt6 copy identical)

These are exported through `Scheme/Glue/build-glue-basic.scm:40-46`:

* `gui-test-snapshot dir`: `QWidget::grab` on every visible top-level
  window (qt_test.cpp:35);
* `gui-test-buttons`: the labels of the visible, enabled buttons (69);
* `gui-test-click label`: the first matching button or tab, searched in the
  active window first (95);
* `gui-test-menu "A|B|C"`: populates lazy menus by emitting
  `aboutToShow` and `aboutToHide`, then triggers the action, with prefix
  matching (179-209);
* `gui-test-menu-entries` (211);
* `gui-test-type`: a `QKeyEvent` per character to the focus widget (234);
* `gui-test-click-later ms dir label`: a QTimer that also fires inside
  modal dialog loops, unlike TeXmacs' `delayed` (274).

Labels are normalised by removing `&` and turning `<ldots>` into `...`
(54). On every other build, `System/Misc/sys_utils.cpp:587-597` provides
no-op stubs under `#ifndef QTTEXMACS`, so Vue and Cocoa get stubs (confirm
for Cocoa). The only user is `docs/tests/git-gui-test.scm`, which runs
under `QT_QPA_PLATFORM=offscreen`. Weaknesses:

* no raw key with modifiers, no mouse at coordinates, no idle query;
* labels only, so the tests depend on the interface language;
* audit `docs/git-audit-2.md:72` (D8): compiled into every Qt build, and
  leaks an `as_charp` buffer.

`docs/git-implementation.md:351` notes that "the offscreen platform does
not render faithfully", so Qt snapshots need xcb or a real display.

### 2.3 Cocoa/NS: environment-variable aids (`src/Plugins/NS/`, `docs/ns-port.md:120-190`)

The NS port has one-shot hooks started from NSTimers in `event_loop`
(ns_gui.mm:1455-1510):

* `TEXMACS_NS_SNAPSHOT`: every 3 s, `cacheDisplayInRect` plus
  `ns_draw_gpu_canvases` (ns_gui.mm:770-805);
* `TEXMACS_NS_TYPE` and `TEXMACS_NS_CLICK`: posted `NSEvent`s, including a
  drag (ns_gui.mm:808-900);
* `TEXMACS_NS_PRESS`: `;`-separated steps, one per second, for a button,
  tab or segment by label, `field:<n>=text`, `abort-modal` and `dump-views`
  (ns_gui.mm:1309-1430);
* `TEXMACS_NS_MENUS`: prints the menu bar to a given depth
  (ns_gui.mm:901-971);
* `TEXMACS_NS_SCROLL`, `TEXMACS_NS_DROP` and `TEXMACS_NS_BENCH`;
* `TEXMACS_NS_WINDOW_TEST`: close, fullscreen, move, resize, front, menu,
  `select:<row>`, `combo:`, `eval:<scheme>` and more
  (ns_tm_widget.mm:1087-1341).

The building blocks for a backend are already there: label lookup over
`NSView` trees, posted events, in-process capture, and a modal-safe timer.
They are not composable, though: each variable is a separate fixed
schedule, with 1-second steps, and there is no Scheme API and no result
other than stderr. Activation is a real problem. With these aids TeXmacs
activates itself and steals the keyboard (ns-port.md:120-124).

### 2.4 Headless Scheme harness (no GUI)

`tests/scheme/check.sh` runs `check-master.scm` suites under `-x` with an
exit status, a timeout and a scratch home. `check/kbd-menu-test.scm` already
drives the *semantic* layer without widgets:

* `press-keys` sends keys through `keyboard-press`, the GUI's own entry
  point, wrapped as the event loop wraps it (`archive-state`,
  `start-editing`, `end-editing`, `update-forced`; kbd-menu-test.scm:66-81);
* the menu part expands every menu as `make-menu-widget` would, without
  building widgets.

This covers key bindings and menu semantics on all ports at once, but it
never reaches port code.

### 2.5 Browser

`misc/wasm/browser-run.mjs:30-45` has its own `--script` language: `wait`,
`shot`, `click`, `type`, `key`, `drag`, `upload`, `answer` and `eval <js>`.
Through `vue_web_scheme_eval` (vue_gui.cpp:5141) JavaScript can evaluate
Scheme. `misc/wasm/test/touch.script` shows the pattern: DOM pointer events,
then Scheme probes returning JSON. The browser runs the Vue port, so the
Vue backend applies there, and only the page-level events (touch, upload,
clipboard prompts) stay in JavaScript.

### 2.6 Generic layer

`docs/texmacs-gui-architecture.md` describes the abstract widget with the
verbs `send`, `query`, `read`, `write` and `notify` on slots
(`Graphics/Gui/message.hpp`), and the Scheme markup in
`kernel/gui/menu-define.scm`, `menu-widget.scm` and `gui-markup.scm`. The
markup has labels but **no widget identifiers**. Neither the slots nor the
markup offer a way to enumerate a window's widgets or to activate one.
Ports keep their own native trees (QObject children, NSView subviews, Clay
elements).

## 3. Levels at which scripting can be general

| Level | What | Portable? | Fidelity | Notes |
|---|---|---|---|---|
| L0 semantic | `keyboard-press`, run a menu action from `menu-expand`, call a widget's Scheme command | all ports, headless | skips port code | exists (kbd-menu-test) |
| L1 widget | find a widget by label, name or path in the *port's* tree, then activate, type into, read its value or rect | API portable, backend per port | real widgets, no pixels | Qt partly, NS partly |
| L2 raw | mouse and keys at window coordinates, wheel, IME, drop | per port; coordinates differ | highest | Vue complete, NS partial |
| L3 a11y | AT-SPI, NSAccessibility, UIA | per OS; Vue has none | real, external | needs permissions on macOS |

L0 already works. **L1 is where portability pays off**: a test that says
"click OK in the dialog titled Enter data" runs on every port. L2 is still
needed for things like a drag past the window edge, IME composition or
wheel inertia. It can be made *mostly* portable by expressing coordinates
relative to a widget found at L1 (`(gt-click (gt-find "Help") :dx 3)`).
L3 is rejected as the main mechanism: Vue would need an accessibility tree
first, macOS requires TCC permission (the reason the in-process aids exist
at all, vue-testing.md:3-5), and CI runners differ. Exposing L1 names as
accessibility identifiers is a cheap later bonus.

## 4. Design options

**A. A line language per port (status quo extended).** Give Qt and NS
their own `.script` interpreters in C++.
*Pros:* small steps, no Scheme async machinery. *Cons:* three C++
interpreters, each with its own commands, pixel coordinates that differ per
port, no assertions, no exit status. Rejected.

**B. Scheme driver with C++ backends (recommended).** A Scheme module owns
the step queue, waits, assertions and reporting. Each port implements
about 10 C++ hooks. Tests are Scheme.
*Pros:* one language, the full TeXmacs API available for checks (buffer
body, cursor, preferences), the `check-lib` reporting reused, a real exit
status, and the headless L0 suite shares helpers with it.
*Cons:* Scheme must run steps asynchronously (no blocking waits). Modal
loops need a pump outside TeXmacs' `delayed`.

**C. External driver (Python or Node over the TeXmacs server socket, or
OS automation).** *Pros:* language freedom, and the process survives a
crash of TeXmacs. *Cons:* a second runtime in CI, latency, and synthetic
OS input needs permissions on macOS. Under B the socket could still serve
as a transport for remote driving later (`remote-check.scm` is a
precedent).

**D. Accessibility-first.** Rejected as primary (section 3).

## 5. Recommended architecture (option B)

### 5.1 C++ backend interface (`Graphics/Gui/gui_test.hpp`, new)

Replaces the stubs in `sys_utils.cpp:587-597`. The default implementations
return "unsupported" so that a test can skip cleanly.

```
array<string> gui_test_windows ();             // "id\ttitle\tx y w h\tfocus"
scheme_tree   gui_test_inspect (int win);      // (widget kind label name enabled (x y w h) children...)
bool          gui_test_activate (int win, string selector);   // L1 click/press/select
bool          gui_test_set_text (int win, string selector, string text);
bool          gui_test_raw (int win, scheme_tree event);       // L2: (move x y) (press x y b) (key "C-S-a") ...
bool          gui_test_idle ();                // port queue empty, no frame pending, no layout pending
bool          gui_test_snapshot (int win, url file);           // next frame, offscreen pixels
void          gui_test_pump (bool on);         // run gt steps from a port timer (modal-safe)
```

Selectors are strings: `label:OK`, `name:git-commit-msg`,
`path:0/2/1` (the index path in `gui_test_inspect`) and `focus`. The
existing `gui-test-*` primitives stay as thin wrappers for one release.

Per-port mapping:

* **Vue.** Reuse `script_push_*` and the key, text, IME and drop code
  (vue_gui.cpp:4574-4835) behind `gui_test_raw`, moved to a new
  `vue_test.cpp`. `inspect` comes from the vue_widget tree with the Clay
  boxes (the same data as `TEXMACS_VUE_DUMP`). `idle` combines no pending
  SDL event with `!frame_wanted ()`, an empty `cmd_list` and no animation.
  The snapshot uses the existing `snapshot_win` and `snapshot_name` path.
  The pump runs from `loop_iteration_body`, which also runs in nested
  loops.
* **Qt.** Refactor `qt_test.cpp`. `raw` goes through
  `QApplication::sendEvent` and `QWindowSystemInterface` (QTest's
  approach, without a QtTest dependency). `inspect` walks
  `QObject::children` and reads `text`, `objectName` and
  `accessibleName`. `idle` checks `!hasPendingEvents` and that no
  `QTMDelayedMethodCall` is queued. The pump is a `QTimer`, which fires in
  modal loops (qt_test.cpp:274). Merge the Qt and Qt6 copies into one
  file.
* **Cocoa.** Move the `TEXMACS_NS_*` helpers into `ns_test.mm` behind the
  interface. `inspect` uses NSView subviews and the menu-bar code of
  `TEXMACS_NS_MENUS`, `raw` uses `postEvent`, and the pump is an NSTimer
  in `NSModalPanelRunLoopMode` and `NSEventTrackingRunLoopMode`. Keep the
  environment variables as one-line wrappers, since CI and the benchmark
  use them.
* **Widkit/X11 and SDL.** Stubs at first. Widkit could get L1 cheaply,
  since its widget tree is TeXmacs-owned.

### 5.2 Widget names

Labels are translated and sometimes duplicated, so add an optional
**`(named "id" widget)`** form to the markup (`menu-define.scm` grammar,
`menu-widget.scm` builds a `named_widget` wrapper). Each port stores the
name natively: Qt `setObjectName` and `setAccessibleName`, NS
`setAccessibilityIdentifier`, Vue a string on the vue_widget, also shown
in `TEXMACS_VUE_DUMP`. Menu items are addressed by their *untranslated*
label path, which the driver resolves with `menu-expand` before it asks the
port. Run tests with the language forced to English as a fallback.

### 5.3 Scheme driver (`TeXmacs/progs/check/gui-driver.scm`, new)

```scheme
(gui-test "dialog"
  (gt-load "tests/Plugins/Vue/dialog.scm")            ; or inline tm-widget
  (gt-window "Enter data")                              ; waits until shown (timeout)
  (gt-set-text "label:Name" "Bob")
  (gt-key "Tab") (gt-type "42")
  (gt-snapshot "d1")
  (gt-key "Return")
  (gt-expect-output "got: Bob / 42")                    ; captured display* / a hook
  (gt-window 2) (gt-type "xyz")
  (gt-check (== (buffer-body-string) "xyz")))
```

* Steps form a *list of thunks* that the runner executes one at a time
  from the port pump. After every step it waits for `gui_test_idle` plus a
  settle of 1 to 2 frames, with a per-step timeout. A step can return
  `'retry` to poll until a condition holds (`gt-window`, `gt-wait-for`).
  No `call/cc` is needed, which works under s7, Guile and femtolisp alike.
* `gt-wait ms` stays available but is discouraged. Waiting for boot
  becomes "wait for the main window to be shown and idle".
* Assertions reuse `check/check-lib.scm`. At the end the runner calls
  `(exit n)`, never `(quit)` (S7 has no `quit`). A global alarm fails the
  test instead of the `kill -9` in run.sh.
* Every step is printed to the log (`gt: <step>`), as `vue script:` lines
  are now.
* L0 fallback: `gt-menu` and `gt-key` can be forced to the semantic path
  (`TEXMACS_GT_LEVEL=semantic`), so the same test also runs headless with
  `-headless` (Vue already makes virtual windows when headless,
  vue_gui.cpp:2955).

### 5.4 The line language: keep it as sugar, compiled to Scheme

The line format is good for quick, recorded, coordinate-level reproductions,
and 60 tests depend on it. Recommendation: **one parser, in Scheme**
(`check/gui-script.scm`), mapping each line to a step. For example,
`click x y [b] [n]` becomes `(gt-raw '(click x y b n))`, `scheme <e>`
becomes `(gt-eval '<e>)`, and `snapshot n` becomes `(gt-snapshot n)`. Add
three new lines: `expect <regexp>`, `activate <selector>` and `idle`. The
C++ `script_step` shrinks to "if `TEXMACS_VUE_SCRIPT` is set, call
`(gui-script-run file)` once Scheme is up". A line script then runs on
every port, but its raw coordinates are port-specific, so a script with
raw commands declares `port vue` and is skipped elsewhere. New tests are
written in Scheme. A recorder (dumping L1 activations and raw events as
lines) is optional later.

Why not Scheme only: the line format is what a developer can write while
watching a PNG, and it is diffable. Why not lines only: it has no
conditionals and no checks, and the 164 `scheme` lines show that tests
already escape to Scheme.

## 6. Migration of the Vue tests

1. Land the Scheme parser and make `run.sh` a wrapper around
   `gui-test-run`, keeping the snapshot directory layout. All 60 scripts
   must run unchanged, which is the acceptance test of phase 2.
2. Replace the grep-on-log with an `expect` line per test, taking the
   expected output from the table in vue-testing.md. This gives the first
   exit status.
3. Replace `wait` with `idle` where it only waits for an effect. Keep
   explicit waits for timing tests (`wheel-travel`, `balloon`, `menu-drag`).
4. Rewrite the tests that only click buttons, tabs or menus (`widgets`,
   `dialog`, `dialogs`, `menus`, `submenu`, `checks`, `tabs`,
   `prefs-tool`, `typographic-palette*`, `interactive`, `font`) at L1 in
   Scheme. These then run on Qt and Cocoa too, which roughly doubles their
   value.
5. Keep the pixel-level tests (`math-cursor`, `drag-scroll`, `bars`,
   `scroll-shift`, `pre-edit*`, `wheel*`) as `port vue` line scripts.
6. For snapshots, keep PNGs as artifacts rather than golden comparisons.
   Where a pixel check matters, assert a property (as
   `packages/macos/png-has-red.py` does, or by comparing two snapshots from
   the same run, as `scroll-shift` does).
7. Fold `browser-run.mjs` scripts into the same driver. The page calls
   `gui-test-run` through `vue_web_scheme_eval`, and only DOM-level actions
   (touch, upload, prompts) stay in the `.mjs` script.

## 7. CI implications

* **Linux, Qt.** `QT_QPA_PLATFORM=offscreen` is already used for the
  Scheme suites (`.github/scripts/run-tests.sh:15`) and is enough for L1
  and L0 tests. Snapshot-based tests need `xcb` under `xvfb-run`, because
  offscreen rendering differs (git-implementation.md:351).
* **Linux, Vue.** It already runs under `xvfb-run`, with
  `TEXMACS_VUE_SNAPSHOT` and `png-has-red.py`
  (`.github/workflows/upsilon-linux-windows.yml:226-250`). Add a job that
  runs the Vue suite there, with Mesa llvmpipe for GL. To evaluate: SDL3's
  `offscreen` video driver (`SDL_VIDEO_DRIVER=offscreen`), which avoids
  Xvfb but whose GL/EGL support depends on the build. Also the MuPDF
  (non-GPU) window path, which needs no GL at all and is the most
  deterministic.
* **macOS (Cocoa and Vue).** GitHub macOS runners have a logged-in window
  server, and the Cocoa and Vue smoke tests already run there
  (`upsilon-macos.yml:158-165`, `ns-port.md:455-465`). In-process
  injection and capture need no TCC permission. The risk is
  activation and focus: use the existing `focus` faking (Vue) and
  `makeKeyWindow` (NS). Run the macOS GUI tests on one architecture only,
  because Rosetta doubles the time.
* **Windows, Vue.** The runner has an interactive desktop, so the tests
  can run as on Linux minus Xvfb. Beware of CRLF in checked-out `.script`
  files: the Scheme parser must strip `\r`.
* **Browser.** puppeteer headless Chrome, as now.
* **Determinism.** A copied or fresh test home (never the real one), the
  language forced to English, `TEXMACS_VUE_DENSITY=1`,
  `TEXMACS_VUE_THEME=light` (and `TEXMACS_NS_THEME`), a fixed window size,
  no cursor blink, and a `TEXMACS_GT_NO_ANIMATION` switch for wheel inertia
  and menu fades. Never call `change-zoom-factor`. Respect the boot lock:
  one instance at a time per home.
* **Duration.** Boot plus idle-driven steps should bring a Vue test from
  about 25 s (fixed sleep) to 6 to 10 s. That is about 8 minutes for 60
  tests, which fits as a separate job, sharded by `ls | split`.

## 8. Phased plan

| Phase | Content | Files | Effort |
|---|---|---|---|
| 0 | Spec and selector grammar; the `gui_test.hpp` interface; stubs moved out of `sys_utils.cpp`; glue entries | `Graphics/Gui/gui_test.hpp`, `System/Misc/sys_utils.cpp`, `Scheme/Glue/build-glue-basic.scm`, `docs/gui-scripting.md` | 2 d |
| 1 | Scheme driver: step runner, idle wait, timeouts, `gt-*` API, check-lib reporting, exit codes; L0 semantic backend; a headless smoke suite in `check.sh` | `progs/check/gui-driver.scm`, `progs/check/gui-script.scm`, `tests/scheme/check.sh` | 5 d |
| 2 | Vue backend: `script_step` internals moved to `vue_test.cpp` behind `gui_test_raw`; `inspect`, `idle`, pump; `TEXMACS_VUE_SCRIPT` routed to the Scheme parser; `run.sh` with an exit status; all 60 tests pass unchanged, then `expect` lines added | `Plugins/Vue/vue_gui.cpp`, `Plugins/Vue/vue_test.cpp`, `tests/Plugins/Vue/*`, `docs/vue-testing.md`, `misc/wasm/sources.txt` | 7 d |
| 3 | `named` markup plus native names in Vue, Qt and NS | `kernel/gui/menu-define.scm`, `menu-widget.scm`, `Graphics/Gui/widget.hpp`, port widget files | 3 d |
| 4 | Qt backend: one `qt_test.cpp` for Qt5 and Qt6, raw events, inspect, idle, modal-safe pump; port `git-gui-test.scm`; fix the D8 leak | `Plugins/Qt/qt_test.cpp`, `Plugins/Qt6/qt_test.cpp`, `docs/tests/git-gui-test.scm` | 5 d |
| 5 | Cocoa backend: `ns_test.mm` gathering the `TEXMACS_NS_*` helpers, env vars as wrappers | `Plugins/NS/ns_gui.mm`, `ns_tm_widget.mm`, `ns_test.mm`, `docs/ns-port.md` | 5 d |
| 6 | L1 rewrite of about 12 Vue tests into port-neutral Scheme tests under `tests/gui/`; run them on all three ports | `tests/gui/*.scm` | 4 d |
| 7 | CI: Linux Vue under xvfb, Linux Qt offscreen L1, macOS Cocoa and Vue (arm64), Windows Vue; PNG and log artifacts | `.github/workflows/*.yml`, `.github/scripts/run-gui-tests.sh` | 3 d |
| 8 (later) | Browser integration; Widkit backend; accessibility identifiers; a recorder | `misc/wasm/browser-run.mjs`, `Plugins/Widkit/*` | 5+ d |

The total for phases 0 to 7 is about 34 working days, or 5 to 7 weeks
including review and flake hunting. Phases 1 and 2 alone (about 2.5 weeks)
already give Vue tests with an exit status, running in CI.

## 9. Risks

* **Idle is hard to define.** Delayed commands with `:pause`, async
  plugins, typesetting in the background, and animations can each make
  "idle" either true too early or never true. Mitigation: idle requires
  N consecutive idle frames, `gt-wait-for <predicate>` handles the rest,
  and every step has a timeout with a clear log line.
* **Modal loops.** A Scheme step that opens a modal dialog does not return
  until the dialog closes. The pump must run from a port timer that fires
  inside the modal loop, and steps must be queued before the triggering
  step (the `click-later` pattern), or the trigger has to be posted
  asynchronously.
* **Native menus on macOS (Qt and Cocoa).** Real menu tracking can't be
  scripted in-process. Accept L1 (trigger the action) for the menu bar, and
  test popup menus where they are TeXmacs-owned.
* **Three backends to maintain.** Keep the interface small (7 functions)
  and test it with one shared conformance suite, `tests/gui/conformance.scm`.
* **Security.** `gui-test-*` must never be callable from documents. Keep
  it out of `:secure`, and compile it in only when `TEXMACS_GUI_TEST` is
  set at run time (or behind a configure flag), which also answers audit
  D8.
* **Flakiness on shared developer machines.** Focus stealing (NS), the
  boot lock wiping the settings when two instances boot together, and
  ports in use (see the test-port memory). The runner must refuse to start
  when the boot lock is present in its own home.
* **Ports diverge.** A port-neutral test may legitimately differ, for
  example native file dialogs on Qt and Cocoa. Allow `(gt-port-case ...)`.

## 10. Open questions for the maintainer

1. Is it acceptable to add `(named "id" ...)` to the widget markup, which
   upstream GNU TeXmacs does not have, or should addressing stay
   label-only plus index paths?
2. Should the test hooks always be compiled in and enabled by an
   environment variable, or only behind a configure option, given the
   Upsilon release binaries?
3. Should the line-language parser move to Scheme (recommended), or should
   the C++ `script_step` stay as is for Vue with Scheme as an alternative?
   The first removes about 270 lines of C++ but makes scripts depend on
   Scheme being up.
4. Snapshot policy: artifacts plus property checks only, or golden images
   per port with a tolerance? Golden images need a pinned font and renderer
   setup per runner.
5. Which ports are in scope for CI gating: Vue on Linux, macOS and Windows,
   Qt on Linux, Cocoa on macOS? Is Widkit/X11 still maintained enough to
   deserve a backend?
6. Should the browser `--script` language be retired in favour of the
   Scheme driver plus a few DOM actions, or kept for page-level tests?
7. Is a remote transport (driving a running TeXmacs over the server
   socket, as `remote-check.scm` does) wanted for interactive debugging?
