# The Vue graphics stack

Vue is a GUI plugin for TeXmacs built on three libraries:

* **SDL3** for windows, events, clipboard and native file dialogs
  (`vue_gui.cpp`);
* **Clay** (`clay.h`, single header, compiled once in `clay.c`) for the
  layout: an *immediate mode* layout engine — the whole UI tree is rebuilt
  every pass by calling `CLAY(id, {...}) { children }` / `CLAY_AUTO_ID({...})`
  macros;
* the TeXmacs **renderer** (MuPDF/fitz backend) for drawing: Clay produces a
  list of render commands, which `render_clay_commands` (`vue_gui.cpp`)
  replays on a `renderer`, into a backing pixmap that is blitted to the SDL
  window surface. Texts are drawn with TeXmacs fonts (`layout_text`), so the
  UI uses the same fonts as documents.

Configuration: `./configure --with-gui=vue --with-mupdf=... --with-sdl3`
defines `VUETEXMACS`; the objects are compiled with `-std=c++20`.

## Windows

`vue_window_rep` (`vue_gui.hpp`) is the abstract window. Three classes
implement it in `vue_gui.cpp`: `vue_sdl_base_window_rep` holds everything
that is SDL and nothing that draws (creation and flags, `update_density`,
`set_visibility`, `process_layout`, `set_size_limits`, `destroy_event`),
`vue_sdl_mupdf_window_rep` adds the renderer, `process_redraw` and the text
measurement, and `vue_sdl_window_rep` is an unused variant drawing through
SDL's own renderer, kept as a starting point for a GPU backend.
`plain_window (vue_widget content, string name, bool popup)` creates the
MuPDF one. Each window owns

* its SDL window and (for the MuPDF variant) a `renderer` on the window
  surface;
* its own **Clay context** (`clay_ctx`, one arena per window) — `with_window`
  (RAII) selects the context and sets the global `current_window`;
* the render commands of its last layout;
* `vue_input_state input`: the pending events and interaction state of the
  window (see *Input*);
* `layout_w/layout_h`: the size of the layout area in pixels;
* visibility bookkeeping: `visible_requested`, `shown`, `ready_to_show`.

Windows are registered in `Window_to_window` (SDL window → vue window) and
`id_to_window` (TeXmacs window id → vue window). The id is given to the
content widget through `SLOT_IDENTIFIER`; plain windows keep the pointer in
their `win` member.

**Popups** (`popup=true`, used for `popup_window_widget` and
`tooltip_window_widget`) are borderless, always on top, not focusable, sized
to their contents on every pass and clamped to the screen; they are dismissed
when the pointer leaves them (`SDL_EVENT_WINDOW_MOUSE_LEAVE`), when one of
their buttons fires (`cancel_popup`) or when a button is pressed outside. While
a popup is visible it *grabs the pointer*: `popup_grab` redirects mouse events
of other windows to the popup when the pointer is over it and drops them
otherwise (`edit_mouse.cpp` relies on this X11 behaviour: it destroys its popup
menu on any editor mouse event).

**Closing a window.** `SDL_EVENT_WINDOW_CLOSE_REQUESTED` calls
`destroy_event`, which sends `SLOT_DESTROY` to the plain window widget; that
queues the window's `quit` command once (`quit_sent`) or, when there is none
(the main TeXmacs window, `plain_window_widget (wid, name)` without command),
forwards the slot to its contents as X11 does: the texmacs widget's command
is `(safely-kill-window url)`, which kills the window or quits TeXmacs when
it was the last one.

**Showing a window.** Windows are created hidden. `set_visibility (true)`
shows the window when its contents already fit; otherwise it records the
request and the window is shown by `process_layout` once
`ready_to_show` has been set by the content's `post_layout` (the contents fit
the window), or after 10 passes as a safety net. This avoids the red/gray
flash and the resize steps of a freshly created dialog.

**Auto-sizing.** `vue_plain_window_widget_rep` sizes a new dialog to its
contents: the root is laid out `FIT` while `autosize` is on, `post_layout`
compares the measured size with the window and calls `set_size` (clamped to
the screen). Because several widgets measure themselves from the *previous*
pass, the size is only applied once two consecutive passes agree. An explicit
`SLOT_SIZE` disables auto-sizing; the main TeXmacs window never auto-sizes.
`resize_widget` contributes the default size while auto-sizing and its
min/max become the SDL window limits (`set_size_limits`).

## The event loop (`gui_start_loop`)

Each iteration:

1. `script_step` (test driver, see *vue-testing.md*), then **one** SDL event
   is polled and translated by `process_event` into the input state of its
   window. When that event is itself a motion or a wheel, the ones already
   queued behind it are handled too (their deltas add up, the last position
   wins), so that a slow frame does not fall behind a trackpad; the
   batching must not follow a press or a release, since the motion handler
   overwrites `mouse_action` and the click would never reach a widget;
2. `process_layout ()`: every window runs `Clay_BeginLayout`,
   `content->do_layout ()`, `Clay_EndLayout`, then `content->post_layout ()`.
   Layout is also where **input is dispatched**: widgets read the input state
   of the window while they lay themselves out (immediate mode);
3. the commands queued in `cmd_list` by the widgets are run (widgets never
   run TeXmacs commands during layout, they queue them);
4. the interpose handler (Scheme delayed commands, TeXmacs housekeeping,
   and the polling of the socket notifiers, see below);
5. if a widget was freed since the layout, every window is laid out again
   (`gui_needs_relayout`), or the render commands would draw an interface
   which no longer exists;
6. `vue_simple_widget_rep::repaint_all ()` repaints the editors' backing
   stores (interruptible). It is skipped while an SDL event is waiting, so
   that a burst of input is handled before the pixels are computed — but
   only until it is `vue_repaint_dt` (16 ms) old: a trackpad delivers its
   events faster than a frame is drawn and its stream never runs dry, so
   the repaint never got its turn for as long as a fast swipe lasted and
   the page stood still;
7. the same check again, up to four times: the repaint runs the typesetter,
   which executes Scheme and can replace widgets in its turn;
8. `process_redraw ()` replays the render commands of every window.

Between iterations the loop sleeps in `SDL_WaitEventTimeout` for a pause
which grows while nothing happens (10 ms → 1 s, for the periodic interpose
calls) and ends as soon as an event arrives; a plain `SDL_Delay` here made
the first event after a pause wait for the end of the pause (up to 1 s
before a scroll started to move). The wheel log lines print for how long an
event was queued (`queued for N ms`).

**Sockets.** A pipe or a socket has no SDL event to wake the loop with, so
while any notifier is registered the pause is capped at 40 ms
(`notifiers_active ()`, `src/System/Link/socket_notifier.cpp`), and the
interpose handler is what polls them (`perform_select`). The loop also
keeps running with no window at all as long as `number_of_servers () > 0`,
which is how a headless TeXmacs server stays alive. The socket layer itself
is GUI-independent and lives in `src/System/Link/tm_sockets.cpp`; see
*Client/server and sockets* in
[texmacs-gui-architecture.md](texmacs-gui-architecture.md).

**Resizes.** A window resize is handled inside SDL's event pump, in a watch
(`event_filter`), so that the window never shows stale content while it is
dragged. That watch runs a whole frame for the one window: layout, resize
notifications, the interpose handler, a relayout pass, the repaint of that
window's editors, up to four more passes and the redraw, all guarded by a
static `busy` flag against re-entry. It deliberately does not release
`styled_strings` and `layout_widgets`, since the commands of the other
windows still name their texts and widgets, so a drag accumulates one
layout's worth of each per event until the main loop lays everything out
again.

The watch runs its frame only while the loop is waiting for events
(`watch_may_run`, set around the loop's own `SDL_PollEvent` and
`SDL_WaitEventTimeout` through `loop_poll`/`loop_wait`), which is where a
drag delivers its resizes. SDL also calls watches from inside any call
which pumps events -- `SDL_ShowWindow`, which `process_layout` calls in the
middle of a frame, or `SDL_PushEvent` from a test script -- and a frame
started there ran inside another one. The event is then left to the next
frame, whose layout reads the size of the window anyway.

**Starting with files** (`texmacs file.tm other.tm`). The command line
loads the first file into the "no name" window before the loop has run a
frame, and the next ones into windows of their own. Two faults showed:

* the "no name" editor, replaced before it was ever painted, was still in
  the paint list with invalid regions, and repainting it made its detached
  view current (`SERVER` in `edit_interface_rep::update_visible`):
  TeXmacs stopped at once with "no window attached to view". A simple
  widget which is not in the layout of its window is not on the screen and
  is no longer repainted (`repaint_invalid_regions`); its regions stay
  invalid until it is laid out again;
* the editor of a window opened by a command got the keyboard focus when
  the window was first laid out, which is after the interpose handler: a
  focus is a change of the editor (freeze, focus, decorations: 546), and
  the repaint of the same frame found it pending, "Invalid situation (546)
  in edit_interface_rep::handle_repaint", in the console window. The layout
  now only records that focus (`default_focus`), and the loop gives it just
  before the interpose handler (`apply_default_focus`). What is typed into
  a new window, before any click, still lands in its document.

## Layout conventions

* **Units**: Clay works in *pixels* of the window surface, `retina_factor`
  of them per point, which is the density of the display that window is on
  (see *Pixel density* below). TeXmacs lengths are `SI` (`PIXEL` = 1 point);
  the conversion used throughout is `retina_factor*si/PIXEL` pixels, and
  back `px * PIXEL / retina_factor`. Writing 2 instead of the factor, as the
  code once did, doubles the widget on a display without HiDPI. Mouse coordinates are pixels relative to the
  window (`mouse_x/mouse_y`).
* **Coordinates**: Clay is y-down from the top-left; the TeXmacs renderer is
  y-up. `render_clay_commands` converts (`rectangle (x, -(y+h), x+w, -y)`),
  so custom render callbacks receive a TeXmacs `rectangle` with `y1 < y2`.
* **Ids**: elements which need input or measurement get an id built from the
  widget's serial `id`: `CLAY_SIDI (CLAY_TM_STRING (type), id)` for the widget
  itself, `CLAY_IDI ("label", id)` for parts, `CLAY_IDI_LOCAL ("item", i)` for
  children (hashed with the parent id). Ids must be unique in a window.
  `probe_id (label, id, k)` builds one from an interned string, so that it
  outlives the pass: the measurement probes below need that.
* **Measuring from the previous pass**: `Clay_GetElementData (id)` returns the
  bounding box computed in the *previous* layout; it is the standard way to
  size things Clay cannot express directly: `extend_widget`, the rows of
  `aligned_widget`, the largest page of `tabs_widget`, the dropdown width of
  `enum_widget`. Hidden things to measure are laid out inside a floating
  element attached to the root with a huge negative offset (culled by Clay).
  Because of this, a decision taken from the previous pass must be *sticky*
  (menu flipping, auto-size stability) to avoid flickering. A widget which
  finds no previous data (it was just created by a refresh) sets
  `layout_again`: `process_layout` then runs another pass immediately (at
  most 5), so the first frame shown is already correct.
* **Editor viewport**: `vue_simple_widget_rep` reads its Clay box in
  `repaint_invalid_regions`; when it changes, the backing store is resized
  and `resize_pending` is set. `notify_resizes ()` runs right before the
  interpose handler (never during a repaint, the editor would warn about an
  "invalid situation") and calls `handle_notify_resize`, so the editor
  re-typesets and recomputes its extents: `edit_interface.cpp` centers the
  paper in a wider canvas for `VUETEXMACS` as for X11, and the scroll
  position is clamped to the extents at every repaint. The `SLOT_SIZE` query
  of an editor returns its viewport in SI (`size * ren->pixel`), the one of
  the main widget the window size (as the Qt main window).
* **Size policy** (`widget_grows` in `vue_widget.cpp`): a container grows
  along an axis only when one of its descendants does (`resize_widget` with
  min≠max, `user_canvas`, editors, splitters, tabs, growing glue). Lists grow
  across their direction when a child does; vertical lists always fill the
  width. Vertical *menus* fit their contents and their items fill the menu
  width (`button_grow`).
* **Custom drawing**: an element with `.custom= { .customData=
  vue_render_widget }, .userData= render_ref ()` calls `widget->render (data)` with a
  `vue_render_ren_data { renderer ren; rectangle r }`; `layout_text` uses the
  same mechanism with `vue_render_text`. A command thus names a widget, and
  Clay holds that as a raw pointer in its arena, where nothing can own a
  reference. The commands outlive the layout which produced them, while the
  queued commands, the interpose handler and the repaint of the editors all
  run before the redraw and may replace widgets (a menu, a tool, a dialog
  rebuilt by TeXmacs): a widget dropped by the widget tree meanwhile would
  be drawn after its death, which crashed in `vue_render_widget_fn`. The
  layout therefore holds a reference to every widget it names
  (`render_ref`, released by `release_layout_widgets` when the next layout
  replaces the commands), exactly as it does for the texts
  (`styled_strings`). `tm_delete<vue_widget_rep>` still sets
  `gui_needs_relayout`, which is now about drawing the current interface
  rather than about safety: the loop lays the windows out again before
  redrawing, and draws the commands it has if the layout does not settle.
  Small marks (check boxes, menu marks) are drawn this way with `pencil`,
  `lines`, `rounded_rectangle`, `fill_arc`.
* **Clipping**: scrollable areas set `.clip` with `Clay_GetScrollOffset ()`;
  `scroll_bar (id, data, z)` draws floating scroll bars for any scroll
  container. `render_clay_commands` tracks the clip depth because Clay culls
  the `SCISSOR_START` of off-screen elements but not the matching end.
* **Colors** (`vue_widget.cpp`): the globals the widgets name are
  `palette[]` (four greys), `color_background` (dialogs), `color_field`
  (lists, scrollable areas, embedded editors), `color_border`, the push
  button shades and `color_pressed`. They are filled from the theme in use,
  so the values are not constants: in the light theme the greys are 160,
  192, 224 and 240, the background 192, the field 250 and the border 150,
  and the dark theme replaces them (see *Themes*). Flat buttons are
  transparent, showing their container (dialog, tool panel or menu), until
  hovered or pressed. `texmacs_output_widget` in the core paints itself the
  field colour of the light theme, hard-coded (`tm_button.cpp`).
* **Z order**: editors' scroll bars 1, menus 5 (their scroll bars 6), enum
  dropdowns and balloons 10.

## Input model

Nearly all per-window interaction state lives in `vue_input_state`
(`vue_gui.hpp`): pending `mouse_action` (`"press-left"`, `"release-right"`,
`"move"`, `"wheel"`, `"drop"`...), pointer position and wheel deltas,
pending `key_event`, the hot and active element ids, the popup chain flags,
the balloon timer and the scroll bar drag. `process_event` writes into the
window of the SDL event; `gui_init_context`/`gui_finalize_context` copy the
state into the globals used by the widgets around the layout pass of each
window and clear the one-shot events afterwards: an event lives for exactly
one layout pass of its window.

Four pieces of state are still process globals which those two functions do
not carry, so they are shared by every window: `mouse_state` (the buttons
and modifiers held, written by `update_mouse_state`), `open_pull_id` (the
pull-down being opened this pass) and the two layout flags `button_grow`
and `menu_has_marks`.

Hit testing uses `Clay_PointerOver (id)` on the element's own id. Do not use
`Clay_Hovered ()` after the element's `CLAY` block has closed: it then tests
the *parent*, and a widget laid out before its siblings (the editor before the
side tools) would swallow their events.

The pointer coordinates are **signed**: a drag may continue outside the
window, where SDL reports negative positions, and when the pointer leaves
the window the last position inside it is kept, so that a dragged element
freezes instead of jumping to an extreme.

**Pixel density.** The layout works in device pixels
(`SDL_GetWindowSizeInPixels`) while the pointer comes in points, and the
renderers draw `retina_factor` pixels per point: everywhere the two meet,
the factor is the density of the display the window is on. Each window
keeps its own (`density`, `retina`, from `SDL_GetWindowPixelDensity`,
refreshed on `PIXEL_SIZE_CHANGED` and `DISPLAY_SCALE_CHANGED`, which also
invalidate the editors so their backing stores are rebuilt at the new
size). TeXmacs reads one global `retina_factor`, so `with_window` makes
the factor of the current window current too and restores it afterwards:
windows on displays of different densities each draw at their own
resolution. The startup value, before any window exists, comes from the
desktop display mode (the *content scale* is the wrong query, macOS
reports 1 there while drawing at 2 pixels per point). Sizes coming from
TeXmacs are in SI, `PIXEL` per point, so a length becomes
`retina_factor*x/PIXEL` device pixels; writing 2 there, as the code did
throughout, made every widget twice its size on a display without HiDPI.
`TEXMACS_VUE_DENSITY=<x>` overrides the density, to draw at 1x on a HiDPI
display and to exercise the other path in the tests. The **icons** follow
the same factor, see below.

**Drag and drop.** SDL delivers a drop in four events: `DROP_BEGIN`, then
one `DROP_FILE` or `DROP_TEXT` per item, then `DROP_COMPLETE`. The handlers
build one `CONCAT` tree out of the items, images becoming `IMAGE` trees
scaled to a sensible size (`vue_pretty_image_size`), and park it in a
`hashmap<int,tree> payloads` under a ticket. The drop then reaches the
widget under the pointer as an ordinary mouse action `"drop"` whose ticket
travels in place of the modifiers (`mouse_ticket`), and `call_drop_event`
reads the payload back. The core half of this is shared with the Qt port
(`edit_mouse.cpp`).

`button_logic (id)` is the common mouse protocol of the elements, over
`Clay_PointerOver`: the element under the pointer is *hot* (hovered) unless
another one is *active*; a press over an element makes it active and
reports `pressed`; it stays active — `held`, the capture — until the button
is released wherever the pointer went (a scroll bar thumb is dragged this
way: `scroll_bar` uses `pressed`/`held` and only remembers the click and
position origins); a release over the active element yields `clicked` (1
left, 2 middle, 3 right), a release elsewhere just deactivates it
(`gui_finalize_context`), and so does a release lost outside the window
(`gui_init_context` checks the button state, but not on the release event
itself, which is the click). When the pointer leaves a window its pointer
position is set off-window, so that nothing stays hovered. Every
`ui_signal` must be initialized (`{ .clicked= 0 }`): an uninitialized one
fired commands every frame. Elements which must not be captured by the
element behind them consume the press (`mouse_action= ""`, the thumbs).

**Keyboard focus** is per window (`win->kbd_focus`); change it with
`set_kbd_focus`, which notifies editors (`handle_keyboard_focus`), and
`notify_window_focus` forwards SDL focus changes. Text inputs and editors
consume `key_event` when focused. Key names follow TeXmacs conventions
(`lookup_key`, `initialize_keyboard`).

**Typing.** A key which produces a character (printable keycode, no
control, alt or command modifier left after `postprocess_key_event`, which
folds shift and option into the keycode) is not delivered as a key: the
system sends the resulting text, composed with the dead keys and the input
method, as `SDL_EVENT_TEXT_INPUT`, and that is what is delivered (" ", "<"
and ">" become `space`, `<less>`, `<gtr>`); a dead key alone types nothing.
Every other key (return, arrows, function keys, C-, M- and A- combinations)
is delivered as a key, and a text event following it within 30 ms
(`key_stamp`) belongs to the same keystroke and is dropped. The scripted
`key` command therefore drives the control keys and `text` the characters.

**Input methods.** A composition (`SDL_EVENT_TEXT_EDITING`: dead keys, CJK)
is shown by the editor as a pre-edit: it receives the key
`pre-edit:<cursor>:<text>` as with Qt, an empty text ending it, and the
committed text arrives as a text event. The text inputs show it too
(`vue_input_text_widget_rep::pre_edit`), spliced into the string they draw,
in a pale box with an underline and the cursor inside it, replaced by the
committed text when it arrives.

The editor applies a pre-edit through `delayed-keyboard-press`, which waits
for 100 ms of `idle-time`. That idle time is zero while `check_event
(ANY_EVENT)` sees a pending event, so the Vue `check_event` must not count
SDL's poll sentinel, an internal event which sits in the queue after every
pump. It did, and nothing which depends on the idle time — the pre-edits,
the `:idle` delayed commands — ever ran. The idle time is also zero while
the window has no keyboard focus.

Popup menus (`layout_pull_button`) form a chain through `current_popup`; a
click on a `menu_button` sets `cancel_popup` which closes the chain (and popup
windows). The buttons of a *bar* (`pulldown_button`) are in addition
mutually exclusive through `open_pull_id`, which names the one whose menu
is open: `cancel_popup` and `current_popup` only reach the widgets laid out
after the click, so a menu opened before another one in the same bar would
stay open until its own away timer expired. A submenu (`pullright_button`)
must not claim that slot, since it belongs to the chain of the menu it is
in and would close its own parent. Menus flip to the other side of their
button or shift to stay in the window and are at most as large as it, in
both directions. A menu which has never been laid out has nothing to be
placed by, so `layout_again` asks for another pass rather than let it be
drawn over the edge for a frame; the box it is measured on has the shift of
the last decision in it, which is taken off before the next one, so that
placing it twice gives the same answer; and the decision is taken again
when the window is resized under an open menu.

* **Contents which do not fit** (`scroll_markers`): the bars of the main
  window (`layout_bar_content`) and the pulldown menus clip their contents
  and mark what is out of view instead of carrying a scroll bar -- a bar
  too narrow for its buttons has no room to spare for one, and in a menu it
  would lie over the labels and the arrows of the submenus. A marker is a
  strip of the colour behind, opaque at the very edge and fading over the
  contents in four steps, with a chevron in it (`render_marker_fn`: drawn
  rather than written, since Lucida Grande has no left-pointing triangle,
  U+25C2, though it has the other three); a click on one brings the next
  screenful that way into view. The wheel scrolls them as it scrolls
  anything else, except that Clay gives each axis its own delta and a mouse
  has no horizontal wheel: `vue_wheel_axes` turns the vertical delta into a
  horizontal one over a container which only scrolls sideways. Clay clamps
  the position of a container only while it is handling a wheel event, so
  `scroll_markers` clamps it at every layout as well: a bar which fits
  again, because the window was made larger, would otherwise stay where it
  had been scrolled to, with its first buttons out of reach and no marker
  left to say where they had gone.

* **Scrolling with the wheel** (`vue_gui.cpp`, `wheel_event`,
  `wheel_step`): a trackpad and a mouse wheel ask for different things and
  are answered differently. A swipe *drags* the view: its events carry the
  displacement of the fingers and are applied at once, so the page follows
  them. A notch *asks for a distance*: that distance is not jumped but
  travelled over the next few frames (`wheel_pend_x/y`, decaying with
  `wheel_smooth_tau`, 45 ms), which is what the native applications do and
  what makes a notch read as a movement rather than as a cut; several
  notches add up, so a wheel which is spun scrolls continuously and comes to
  rest shortly after the last notch. That travel replaced a glide launched
  from the estimated speed of the wheel, which went on after the wheel had
  stopped and read as the view running away.
  **Units**: SDL reports the deltas in "lines"; `wheel_event` converts them
  to device pixels — 10 points per unit for a precise (trackpad) stream,
  which is a tenth of the finger's displacement on macOS, so the page
  follows the finger exactly as a dragged scroll bar follows the pointer,
  and 80 points (about six lines) per notch of a mouse wheel — and `push_wheel`
  hands them to the editors (`mouse_data`, turned into SI with
  `ren->pixel`, the fractions carried in `scroll_rest_x/y`) and to Clay
  (`Clay_UpdateScrollContainers`, which scrolls ten pixels per unit, hence
  `/10`). The former mapping scaled a unit to a percentage of the viewport,
  so the page moved faster or slower than the finger depending on the
  window size.
  **Telling the two apart**: SDL keeps no trace of which device sent an
  event (`hasPreciseScrollingDeltas` is lost in the Cocoa backend, which
  only rounds the deltas of a device without it), so the fractions are the
  signal: a fractional delta is a trackpad (`wheel_precise`, sticky for the
  stream). Whole deltas are ambiguous — a swipe can open on one, and eight
  times too much would then be scrolled — so a second event which follows
  the opening one within `wheel_burst_dt` (16 ms), too soon for a wheel to
  have turned twice from rest, also means a trackpad. Until that is
  settled, the opening notch travels no further than a swipe would have
  (`wheel_over_x/y`, the excess), so the correction takes nothing back and
  the view never springs. The intervals are measured on the timestamps SDL
  gives the events (`wheel_stamp`, ns), not on the clock: the events queued
  during a frame are all handled at the end of it and the clock would report
  them as simultaneous.
  **Trackpads**: SDL reports their gestures without a phase, so fingers
  which pause cannot be told from fingers which are lifted. On macOS the
  system computes the momentum itself and, with the hint
  `SDL_HINT_MAC_SCROLL_MOMENTUM` set before `SDL_Init` (SDL drops these
  events by default), sends it as a stream of wheel events after the fingers
  are lifted: the view follows the fingers exactly while they are down and
  the system glide after. Where the system does not do it
  (`wheel_system_momentum` is false), the speed of the fingers is estimated
  from the events (`wheel_est_x/y`, device pixels per ms, smoothed) and,
  when no event has come for `wheel_stream_dt` (30 ms) and that speed is
  above `wheel_launch_speed` (1 px/ms), the view goes on with it
  (`wheel_vx/vy`) decaying with `wheel_tau` (350 ms), as synthetic wheel
  deltas every frame; a new event stops the glide.
  **Pacing**: a frame costs more than the interval between
  the events of a trackpad, so the loop handles all the wheel and motion
  events already queued in the same frame (their deltas add up in
  `push_wheel`) instead of one event per frame, which lagged behind the
  fingers and made the motion jerky. The editor keeps the fractional SI
  remainder of the small steps (`scroll_rest_x/y`) and adds each delta to
  the position it is already waiting to be given (`scroll_pending`) rather
  than to `backing_pos`, which only moves with a repaint: computing it from
  `backing_pos` lost every delta but the last whenever the repaint was
  skipped, which is what a fast swipe does (see step 6 of the loop), so the
  page barely moved while the fingers were flying. While a view moves by
  itself the loop paces itself at 5 ms (`SDL_WaitEventTimeout`), so any
  event wakes it at once.

## Icons

`mupdf_load_xpm` is asked for `name.xpm` and decides what to draw. MuPDF
renders SVG itself (its `source/svg`), so the vector original is preferred:
`misc/pixmaps/light/name.svg` or `misc/pixmaps/dark/name.svg`, whichever
the current icon theme is (`mupdf_set_icon_theme`, called from
`set_vue_theme`), and otherwise the `name.svg` sitting next to the xpm.
`mupdf_render_svg` draws it in a box of so many points at `retina_factor`
device pixels per point, so an icon is sharp at every resolution and comes
recoloured for a dark interface, without the `--with-resvg` build the Qt
port needs. When there is no SVG the rasters are used as before
(`name_x4.png`, `name_x2.png`, `name.png`, falling back on the smaller ones
and finally on the xpm itself), all of them being the same size in points.
The cache of `load_xpm` is keyed by the resolution and by the icon theme,
or a change of display or of theme would serve the wrong one; a picture
widget which was built under another one reloads its file (`icon_picture`
in `vue_widget.cpp`, `icon_generation`), since the menus are not
necessarily rebuilt when the theme changes. Loading `_x2.png`
unconditionally, as the code did, drew every icon at twice its size on a
display without HiDPI.

The same renderer serves document images: `mupdf_load_image` draws any
`.svg` file at the size it declares and at `retina_factor`, so an SVG
inserted in or dropped on a document is rendered without `--with-resvg`
too; `mupdf_load_svg` is the entry point which returns a `picture`.

Two traps, both found by rendering the whole set:

- MuPDF reads the presentation attributes and the inline `style` attribute
  only, **not** a `<style>` element with class selectors. The eighteen
  icons written that way (`tm_cut`, `tm_copy`, `tm_paste`, the accents...)
  came out as black squares, every fill falling back to the default; their
  styles are now inline in both `light` and `dark`. `fill: transparent` is
  not a colour MuPDF knows either, and became black: it is `fill: none`.
- The size an SVG file declares is not the size of the icon. Thirty-odd
  files, the flags in particular, declare the drawing they were made from
  (`width="1200"`) and fifteen declare nothing. The box comes instead from
  the directory the icon set lives in, which names it: `modern/24x24/main`,
  `traditional/--x17` where a dash is a free side (`icon_box_size`). The
  drawing keeps its proportions and is centered in that box.

## Error handling of the libraries

* **MuPDF** reports errors with `fz_throw`, a `longjmp` to the innermost
  `fz_try`; outside any `fz_try` block it prints "aborting process from
  uncaught error!" and exits — this was the crash on a help document.
  Everything which can fail (image decoding, font loading, pixmap and image
  creation, the draw device and processor of a renderer, the PDF objects of
  a pattern) goes through the helpers of `mupdf_picture.hpp`:
  `mupdf_image_from_file/pixmap`, `mupdf_pixmap_from_image`,
  `mupdf_new_pixmap` (a cleared pixmap, 1×1 if the size cannot be allocated)
  and `mupdf_protected ("what", lambda)`, which logs `TeXmacs] MuPDF error in
  what: ...` and returns false. The body of a protected call must not create
  C++ objects with destructors (skipped by the longjmp). `mupdf_context ()`
  installs error/warning callbacks so MuPDF's own messages reach the log.
  The drawing operators themselves (`proc->op_*`, `image ()`) are not
  wrapped: they run on objects created by protected calls and on a renderer
  which always has a processor (`begin` falls back to a 1×1 pixmap).
* **Clay** never aborts: its error handler (`HandleClayErrors`) logs each
  kind of error once (duplicate ids, capacity exceeded, floating parent not
  found...) and Clay skips the offending element. The arena is sized with
  `Clay_MinMemorySize` for the default capacity (8192 elements per window).
  **Provenance of `clay.h`**: upstream Clay `main` at commit e6cc36941ab2
  (2026-05-20; the header still says `VERSION: 0.14`, no tag has been made
  since 0.14), taken verbatim; `clay_renderer_SDL3.c` is the upstream
  renderer plus the `CLAY_RENDER_COMMAND_TYPE_CUSTOM` case (calls
  `vue_render`) and a non-static `SDL_Clay_RenderClayCommands`. Compared to
  the 0.14 snapshot used before (2026-09-21): the element hash map is pruned
  by Clay itself at `Clay_EndLayout` (our compaction patch is gone) and a
  full map is reported (`CLAY_ERROR_TYPE_HASH_MAP_CAPACITY_EXCEEDED`); since
  the map holds the ids of the previous *and* of the current frame right
  after a rebuild of the widget tree, the capacity is set to 32768 elements
  (`Clay_SetMaxElementCount`, arena of ~21 MB per window): the macros editor
  exceeded the default 8192. API changes met: `.id` left the declaration
  struct (`CLAY(id, {...})`, `CLAY_AUTO_ID({...})`), `Clay_EndLayout` takes
  a `deltaTime` (transitions API, unused, we pass 0), the render command
  types are renumbered (RECTANGLE=1, BORDER=2, TEXT=3, IMAGE=4,
  SCISSOR_START/END=5/6, OVERLAY_COLOR_START/END=7/8, CUSTOM=9) and, the one
  behavioural change which bit: an element's **background rectangle is now
  emitted after its custom command** (and after its scissor start), so a
  custom element must not have a `.backgroundColor` (the picture widgets
  lost theirs, the close mark of the tool title bars became a child of the
  round button) — the custom render command carries the color in
  `renderData.custom.backgroundColor` for renderers which want to draw it.
  100 scroll containers are upstream now.
* **SDL3** functions return `NULL`/`false` and set `SDL_GetError`. Checked:
  window creation (fatal), the layout arena, the window surface (the frame
  is skipped), surface creation and blits, `SDL_UpdateWindowSurface`, the
  primary display bounds (`SDL_GetPrimaryDisplay`, with a 1440×900 fallback),
  clipboard get/set. `SDL_Init`/`TTF_Init` failures exit at startup.

## Themes

Every colour of the interface is a field of `vue_theme` (`vue_gui.hpp`),
and the globals the widgets name (`color_background`, `color_field`,
`palette[]`...) are filled from the theme in use, `the_theme`. Two themes
ship, `vue_theme_light` (the historical look) and `vue_theme_dark`;
`set_vue_theme` chooses between them from the `gui theme` preference,
whose `"default"` follows the appearance of the system
(`SDL_GetSystemTheme`, and `SDL_EVENT_SYSTEM_THEME_CHANGED` re-applies it
while running). `TEXMACS_VUE_THEME` overrides the preference, which is how
the tests take both.

`gui_refresh ()` re-reads the preference and re-applies the theme, which is
the path the preferences dialog takes: changing the theme needs no restart.

Two rules keep a theme complete. A widget which needs a colour should get a
**field**, never a literal, or the theme will not reach it; and the two
colours the widgets (and the core) ask for without knowing about themes,
`black` and `dark_grey`, are mapped in `layout_text` to the theme's text
and grey, so that no call site has to change. `theme_color` converts a
theme colour to a TeXmacs one for the drawing routines. The surround of
the pages (`tm_background`) is set from the theme as well, while the
documents themselves keep their own colours.

Adding a theme is a constant of type `vue_theme` and a case in
`set_vue_theme`, which also picks the icon set
(`TeXmacs/misc/pixmaps/light` or `.../dark`, see *Icons*).

The rule is not yet kept everywhere. These colours are still literals and
so stay light under the dark theme: the accent and the inert grey of the
check boxes, the frames of the menus, of the popup windows and of the enum
dropdowns (grey 150), the two hover greys of the section bars, the
separators of the tool bars (grey 150), and the background
`texmacs_output_widget` paints in the core (`tm_button.cpp`).

## Animation (Clay transitions)

Clay `main` animates elements whose declaration has a `.transition`
(handler such as `Clay_EaseOut`, duration in seconds, the properties among
position, dimensions, background/overlay/border colors, corner radius):
when the declared value changes between two layouts, Clay interpolates from
the previous state over the duration, driven by the frame time passed to
`Clay_EndLayout` (`process_layout` measures it, capped at 100 ms). While a
context has a transition in progress (`vue_clay_transitions_active` in
`clay.c`, the only place where the context structure is visible)
`transitions_running` keeps the loop drawing, paced at 8 ms and woken by
events. The buttons (`menu_button`) fade their hover and press highlight in
120 ms. The highlight is taken over the colour behind it (`highlight_on`,
`color_behind`, set for the contents of a bar by `with_behind`): the bars
of the main window have different greys and the focus bar, at 232, was so
close to the highlight of the light theme, 240, that nothing showed on it.
The theme's colour is used wherever it is half its own step or more away
from the colour behind; where it is not, the step is taken from that colour
instead and only half of it, in the direction the theme takes (232 gives
white, 224 gives 248), since a highlight which is merely lighter than an
already light bar needs no more. Only where there is no room left for it,
a field which is almost white, does it go the other way (250 gives 226). Every element which highlights under
the pointer passes its own resting colour: the buttons and the pulldowns
the colour of the bar or the menu they are on, a tab `tab_inactive`, an
enum `shade[2]`, the items of the choice lists and of the tree view the
white of a field, on which the highlight of the theme was invisible too. Clay interpolates the four channels of a colour independently, so
what a flat button shows when it is *not* highlighted is the colour of the
highlight with a zero alpha (`faded`) and not a transparent black: fading
to the latter darkened the red, green and blue while the alpha fell, and
what was drawn halfway was a dark grey wash over the bar, seen as a
flicker under the pointer both on the way in and on the way out. A tool
panel which appears slides in from its edge in 150 ms
(`layout_tool_panel`: an *enter* transition on the position only, with
`enter.setInitialState` placing the panel beyond the edge, so that the
sizes the tools measure are final at once; the sizes are not animated on
purpose, since the editors read their Clay box as their viewport). Anything
else animated should use the same mechanism, and tests which snapshot
after a click must wait for it to settle (they do, 300 ms and more). The
elements' ids must be stable for this to work (see the notes on
`clay_tm_string`).

## Design decisions recorded

* **Relayout flag**: every window is laid out on every iteration of the
  loop (immediate mode: the layout *is* the event dispatch), so there is no
  per-window "needs relayout" flag; the former `vue_window_rep::relayout`
  was never read and is gone. Passes are repeated within an iteration only
  through `layout_again`/`post_layout` (widgets measured from the previous
  pass) and `gui_needs_relayout` (widgets replaced by commands).
* **Caching of rendering structures**: the per-frame objects are cheap
  (`styled_strings` and `layout_widgets` are rebuilt each layout, the
  icons are loaded once per theme and per resolution and kept by the
  `load_xpm` cache, the fonts come from TeXmacs' font cache, the glyphs from
  MuPDF's) and the profile of a frame is now
  dominated by the editors' repaint and the surface upload, so no further
  cache is kept; the Clay element ids are the one structure which must be
  stable across frames (interned strings).

## Rendering details

`vue_sdl_mupdf_window_rep::process_redraw` clears the surface with the
background of the theme (red in the F1 debug mode, to spot uncovered
areas), replays the
Clay commands and presents the SDL surface. Editors (`vue_simple_widget_rep`)
own a backing store picture repainted incrementally (`invalid_regions`, in
document coordinates) and blitted by their custom render callback.
The backing store is allocated by `native_opaque_picture`: cleared to opaque
white rather than to transparent, and flagged as opaque. Source-over on an
opaque destination leaves the alpha at 255, so it stays opaque whatever the
editor draws, and `draw_pixmap_direct` can then blit it with a loop which
has no test in it, one `memcpy` per row or a fixed reordering of the
channels which the compiler vectorises. That test per pixel, not the
reordering, was the cost: the blit of a full window went from 3.2 ms to
0.7 ms, and a frame while scrolling from 13.1 ms to 10.5 ms. MuPDF writes
the components in the RGB order whatever colorspace the pixmap is declared
with, so the store cannot simply be declared in the order of the window:
the reordering has to happen in the blit.

**Scrolling** shifts the pixels of the backing store (`translate_backing_store`,
`memmove` per row) and repaints only the exposed strips, so a scroll step
costs a strip instead of the whole viewport (the tiled neutral background
and the glyphs were the bulk of a frame): for that the scroll position is
kept on the pixel grid (`grid_floor` after the clamps; the wheel path moves
by whole pixels and carries the remainder in `scroll_rest_x/y`, the scroll
bars round their position), and a change of the viewport size or a
position off the grid falls back to a full repaint. The `repaint` script
command invalidates every editor, so that a test can compare the shifted
result with a repaint from scratch (`scroll-shift`).
`TEXMACS_VUE_SNAPSHOT=<dir>` writes every redraw as PNG.

### The MuPDF renderer

`mupdf_renderer_rep` (`Plugins/MuPDF/mupdf_renderer.cpp`) draws through a
PDF *run processor* (`proc->op_*`: the PDF content operators q/Q, cm, re, f,
Tj...) on a draw device bound to the target pixmap; it mirrors the structure
of the PDF export renderer (`Plugins/Pdf/pdf_hummus_renderer.cpp`), while the
Qt renderer (`Plugins/Qt/qt_renderer.cpp`) is the reference for on-screen
behaviour. Feature status against those two:

* native fonts (`pdf_font_desc` from the TrueType/Type1 file, `op_Tf`/`op_Tj`),
  bitmap glyphs (`shrink`, glyph cache as images) when no font file exists;
* **pattern-filled glyphs** (`draw_bis`): the glyph mask modulates the
  pattern image sampled at the glyph's device position (as Qt);
  `draw` dispatches there for `pencil_brush` pencils with a pattern;
* **pattern fills** (`register_pattern`): a PDF tiling pattern whose
  resources hold the image as an XObject (`/Resources << /XObject << ... >> >>`);
  pattern images are scaled to their requested size (`fz_scale_pixmap` in
  `mupdf_load_pixmap`), which the pattern sizes of the style files rely on;
* **`clear_device`**: white plus the tiled `neutral-pattern.png`, as Qt
  (visible between pages in paper mode; `draw_surround` covers the sides);
* **direct pixel access** (`fill_direct`, `draw_pixmap_direct`,
  `device_box`): axis-aligned boxes land on integer device pixels (`to_x`/
  `to_y` divide SI by the pixel size), so plain-color fills (`fill`, `clear`
  with a color background, the white of `clear_device`) and 1:1 blits of
  pictures (`draw_picture`: the backing stores of the editors, the icons)
  are written into the pixmap directly, source-over with premultiplied alpha
  as MuPDF does, within the clip (`clip_level` > 0) — the same result to
  one level, but without the path rasterizer and the RGB→BGR conversion
  (the window surface is BGR, pictures RGB) which took most of a frame
  while scrolling: the frame interval of a 1400×900 window went from
  22–35 ms to 13–20 ms (profiles with `sample` during
  `wheel-travel.scm` + a script of 300 wheel steps). Pattern fills,
  rounded corners, arcs and text still go through MuPDF; what remains of a
  frame is the editor repaint (`clear_device` tiles, glyphs), the blit and
  `SDL_UpdateWindowSurface`;
* polygons: nonzero winding for convex, even-odd otherwise (as the PDF and
  X11 renderers; Qt uses the winding rule for non-convex ones);
* lines/arcs/rounded rectangles, clipping, linear transformations
  (`set_transformation` as pdf_hummus), shadows (`new/get/put/apply_shadow`
  by pixmap copies), pictures and scalables (`draw_scalable` falls back to
  the generic conversion when MuPDF cannot load the file);
* **MuPDF errors**: an error nothing catches ends the process, and a
  `longjmp` skips C++ destructors (see *MuPDF's errors and C++* in
  docs/pdf-output-with-mupdf.md). What can fail for another reason than
  memory is protected: images are decoded when drawn (`image`, as
  `draw_form`), the device complains of a clip left open (`end`), q and
  clips have limits of nesting (`set_clipping` counts only the q which
  happened, so that `end` balances them), a font descriptor
  (`load_pdf_font`), a PNG which cannot be written (`save_picture`). The
  path and text operators of each glyph and line fail only for want of
  memory and stay unprotected;
* **FreeType under MuPDF's lock**: the charmap of a native font and the
  glyph of each character are asked of the FreeType face of the MuPDF font
  only through `mupdf_select_custom_charmap` and `mupdf_glyph_index`, which
  hold `fz_ft_lock` (the Fitz renderer uses them too). Those two calls do
  not allocate, so this is discipline rather than a fix; asked directly, a
  glyph name on an OpenType face did crash the PDF renderer;
* **PDF figures as drawing**: a PDF is not converted to a PNG (by
  CoreGraphics, Ghostscript or ImageMagick, at one size) but read once into
  memory and drawn through a form XObject made of its first page
  (`load_pdf_form`, `draw_form`), sharp at any zoom. Each form keeps the
  document it was read from, so that dropping it from `form_pool` (which
  `image_gc` does) frees it. Two things were needed: the processor wants a
  resource frame to run a form in (`pdf_processor_push_resources`, which
  `pdf_process_contents` would otherwise push; without it `op_Do_form`
  dereferences NULL), and the form's `/Matrix` must be taken out of fitz's
  page transform (`pdf_page_obj_transform`), which also turns y upside
  down, see *docs/pdf-output-with-mupdf.md*. A figure drawn translucent
  is made a transparency group for that use (`/Group` put on the form, and
  taken away again for an opaque one, which would otherwise cost a buffer
  the size of the figure at every repaint): the alpha then applies to the
  figure as a whole, as it did to the bitmap, and not to each of its paths
  and fills, which would show through one another;
* **patterns anchored to the document**: the pool keeps each pattern
  unplaced, and every select makes a placed copy (`placed_pattern`) whose
  matrix puts a corner of the tiles at the origin of the document, where
  the Qt port puts it (`decode (0, 0)`); the glyphs filled with a pattern
  (`draw_bis`) sample it from the same corner. Before, the matrix was
  fixed when the pattern was first used, with the scroll position of that
  moment, so a pattern stood still while the page moved and a strip
  repainted after a scroll shift did not meet the part which was moved:
  `pattern-scroll.scm` under `scroll-shift.script` differed from its
  repaint on 156 rows, now on none but the next point. Our `pdf_pattern`s
  are reference counted as MuPDF's own (`FZ_INIT_STORABLE`); they were
  made with a count of 0 before, so that they were never freed. The pool
  is keyed by the pattern *and the size of its tile* in device pixels
  (`pattern_key`), which depends on the zoom: keyed by the pattern alone,
  a fill kept the tiles of the zoom it was first drawn at
  (`pattern-zoom`: 26 pixel stripes at zoom 1 and at zoom 2, now 26 and
  52), while the glyphs, whose pixmaps are keyed by size, did not.
  `clear_device` gives the neutral pattern its size in the units of the
  renderer, a pixel of the image to a point of the screen, so that it
  stays at its natural size at every zoom as in the Qt port;
* still open: after scrolling back up on a patterned page, one row of the
  grey surround at the edge of the repainted strip shows the page colour
  (`pattern-scroll`, s3 against s4, row 337); it was there before the
  patterns were anchored. `set_brush` also resets the pencil width/caps.

The renderers of the MuPDF plugin (`mupdf_renderer_rep`, used by the Vue
windows and pictures, and `fitz_renderer_rep`) derive from
`basic_renderer_rep`. Since TeXmacs 2.1.5 `renderer_rep` carries a
`pixel_ratio` (device pixels per point, used by the Qt6 port) which enters
`zoomf`, `retina_pixel` and `shrink (glyph, ..., pixel_ratio)`, plus a pure
virtual `clear_device` (the neutral pattern behind the pages) and a `safe`
flag of `set_zoom_factor` checking the consistency of `shrinkf`. The MuPDF
renderers keep the older scheme instead: `pixel_ratio= 1`, the retina factor
is multiplied into the zoom by their own `set_zoom_factor` (hence
`safe= false`) and `shrink` is called with ratio 1; `clear_device` draws
the white and the neutral pattern, as above.
