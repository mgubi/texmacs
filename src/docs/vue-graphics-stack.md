# The Vue graphics stack

Vue is a GUI plugin for TeXmacs built on three libraries:

* **SDL3** for windows, events, clipboard and native file dialogs
  (`vue_gui.cpp`);
* **Clay** (`clay.h`, single header, compiled once in `clay.c`) for the
  layout: an *immediate mode* layout engine — the whole UI tree is rebuilt
  every pass by calling `CLAY({...}) { children }` macros;
* the TeXmacs **renderer** (MuPDF/fitz backend) for drawing: Clay produces a
  list of render commands, which `render_clay_commands` (`vue_gui.cpp`)
  replays on a `renderer`, into a backing pixmap that is blitted to the SDL
  window surface. Texts are drawn with TeXmacs fonts (`layout_text`), so the
  UI uses the same fonts as documents.

Configuration: `./configure --with-gui=vue --with-mupdf=... --with-sdl3`
defines `VUETEXMACS`; the objects are compiled with `-std=c++20`.

## Windows

`vue_window_rep` (`vue_gui.hpp`) is the abstract window; the concrete class is
`vue_sdl_mupdf_window_rep` (`vue_gui.cpp`), created by
`plain_window (vue_widget content, string name, bool popup)`. Each window owns

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

**Showing a window.** Windows are created hidden. `set_visibility (true)` only
records the request; the window is shown by `process_layout` once
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
   window;
2. `process_layout ()`: every window runs `Clay_BeginLayout`,
   `content->do_layout ()`, `Clay_EndLayout`, then `content->post_layout ()`.
   Layout is also where **input is dispatched**: widgets read the input state
   of the window while they lay themselves out (immediate mode);
3. the commands queued in `cmd_list` by the widgets are run (widgets never
   run TeXmacs commands during layout, they queue them);
4. the interpose handler (Scheme delayed commands, TeXmacs housekeeping);
5. `vue_simple_widget_rep::repaint_all ()` repaints the editors' backing
   stores (interruptible);
6. `process_redraw ()` replays the render commands of every window.

Window resizes are handled synchronously in an SDL event watch
(`event_filter`) so that the window never shows stale content.

## Layout conventions

* **Units**: Clay works in *pixels* of the window surface (retina: 2 pixels
  per point). TeXmacs lengths are `SI` (`PIXEL` = 1 point); the conversion used
  throughout is `2*si/PIXEL` pixels (i.e. `retina_factor`), and back
  `px * PIXEL / retina_factor`. Mouse coordinates are pixels relative to the
  window (`mouse_x/mouse_y`).
* **Coordinates**: Clay is y-down from the top-left; the TeXmacs renderer is
  y-up. `render_clay_commands` converts (`rectangle (x, -(y+h), x+w, -y)`),
  so custom render callbacks receive a TeXmacs `rectangle` with `y1 < y2`.
* **Ids**: elements which need input or measurement get an id built from the
  widget's serial `id`: `CLAY_SIDI (CLAY_TM_STRING (type), id)` for the widget
  itself, `CLAY_IDI ("label", id)` for parts, `CLAY_IDI_LOCAL ("item", i)` for
  children (hashed with the parent id). Ids must be unique in a window.
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
  vue_render_widget }, .userData= widget` calls `widget->render (data)` with a
  `vue_render_ren_data { renderer ren; rectangle r }`; `layout_text` uses the
  same mechanism with `vue_render_text`. Small marks (check boxes, menu
  marks) are drawn this way with `pencil`, `lines`, `rounded_rectangle`,
  `fill_arc`.
* **Clipping**: scrollable areas set `.clip` with `Clay_GetScrollOffset ()`;
  `scroll_bar (id, data, z)` draws floating scroll bars for any scroll
  container. `render_clay_commands` tracks the clip depth because Clay culls
  the `SCISSOR_START` of off-screen elements but not the matching end.
* **Colors** (`vue_widget.cpp`): `palette[]` greys (160, 192, 224, 240),
  `color_background` (192, dialogs), `color_field` (250, lists, scrollable
  areas, embedded editors), `color_border` (150), push button shades,
  `color_pressed`, selection blue `{100,100,255}` and the accent
  `{70,110,220}` of check boxes. Flat buttons are transparent (they show
  their container: dialog, tool panel or menu) until hovered or pressed. `texmacs_output_widget` in the core uses the
  field color for the Vue build (`tm_button.cpp`).
* **Z order**: editors' scroll bars 1, menus 5 (their scroll bars 6), enum
  dropdowns and balloons 10.

## Input model

All per-window interaction state lives in `vue_input_state` (`vue_gui.hpp`):
pending `mouse_action` (`"press-left"`, `"release-right"`, `"move"`,
`"wheel"`...), pointer position and wheel deltas, pending `key_event`, the hot
and active element ids, the popup chain flags, the balloon timer and the
scroll bar drag. `process_event` writes into the window of the SDL event;
`gui_init_context`/`gui_finalize_context` copy the state into the globals
used by the widgets around the layout pass of each window and clear the
one-shot events afterwards: an event lives for exactly one layout pass of its
window.

Hit testing uses `Clay_PointerOver (id)` on the element's own id. Do not use
`Clay_Hovered ()` after the element's `CLAY` block has closed: it then tests
the *parent*, and a widget laid out before its siblings (the editor before the
side tools) would swallow their events.

`button_logic (id)` implements hot/active/click over `Clay_PointerOver`: the
press makes the element active, the release over the same element yields
`clicked` (1 left, 2 middle, 3 right). Every `ui_signal` must be initialized
(`{ .clicked= 0 }`): an uninitialized one fired commands every frame.

Keyboard focus is per window (`win->kbd_focus`); change it with
`set_kbd_focus`, which notifies editors (`handle_keyboard_focus`), and
`notify_window_focus` forwards SDL focus changes. Text inputs and editors
consume `key_event` when focused. Key names follow TeXmacs conventions
(`lookup_key`, `initialize_keyboard`); `SDL_EVENT_TEXT_INPUT` is deduplicated
against the last key.

Popup menus (`layout_pull_button`) form a chain through `current_popup`; a
click on a `menu_button` sets `cancel_popup` which closes the chain (and popup
windows). Menus flip to the other side of their button or shift to stay in
the window, are at most as tall as the window and scroll.

* **Kinetic scrolling** (`vue_gui.cpp`, `wheel_inertia_step`): a wheel
  event delivers `wheel_immediate` (40%) of its delta at once; the rest
  becomes a per-window velocity (`vue_input_state::wheel_vx/vy`) decaying
  with `wheel_tau` (100 ms), turned into synthetic wheel deltas every frame
  (`push_wheel`: `mouse_action= "wheel"` for the widgets plus
  `Clay_UpdateScrollContainers` for the Clay container under the pointer).
  The total distance equals the sum of the events, so trackpad streams (which
  already carry the OS momentum) are only smoothed, while discrete wheel
  notches glide. The editor keeps the fractional SI remainder of the small
  steps (`scroll_rest_x/y`). While a view glides the loop does not sleep
  (5 ms pacing).

## Rendering details

`vue_sdl_mupdf_window_rep::process_redraw` clears the surface with the UI
background (red in the F1 debug mode, to spot uncovered areas), replays the
Clay commands and presents the SDL surface. Editors (`vue_simple_widget_rep`)
own a backing store picture repainted incrementally (`invalid_regions`,
`translate_backing_store` when scrolling) and blitted by their custom render
callback. `TEXMACS_VUE_SNAPSHOT=<dir>` writes every redraw as PNG.
