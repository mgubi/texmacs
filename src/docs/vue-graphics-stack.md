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

**Closing a window.** `SDL_EVENT_WINDOW_CLOSE_REQUESTED` calls
`destroy_event`, which sends `SLOT_DESTROY` to the plain window widget; that
queues the window's `quit` command once (`quit_sent`) or, when there is none
(the main TeXmacs window, `plain_window_widget (wid, name)` without command),
forwards the slot to its contents as X11 does: the texmacs widget's command
is `(safely-kill-window url)`, which kills the window or quits TeXmacs when
it was the last one.

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
   window — plus the wheel and motion events already queued behind it
   (their deltas add up, the last position wins), so that a slow frame does
   not fall behind a trackpad;
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

Between iterations the loop sleeps in `SDL_WaitEventTimeout` for a pause
which grows while nothing happens (10 ms → 1 s, for the periodic interpose
calls) and ends as soon as an event arrives; a plain `SDL_Delay` here made
the first event after a pause wait for the end of the pause (up to 1 s
before a scroll started to move). The wheel log lines print for how long an
event was queued (`queued for N ms`). Window resizes are handled
synchronously in an SDL event watch (`event_filter`) so that the window
never shows stale content.

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
  same mechanism with `vue_render_text`. The render commands thus point to
  widgets: since the queued commands and the interpose handler run between
  the layout and the redraw of a frame and may replace widgets (menus, tools,
  dialogs), `tm_delete<vue_widget_rep>` sets `gui_needs_relayout` and the
  loop lays the windows out again before redrawing (a freed editor of a
  replaced tool crashed in `vue_render_widget_fn`). Small marks (check boxes, menu
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

Keyboard focus is per window (`win->kbd_focus`); change it with
`set_kbd_focus`, which notifies editors (`handle_keyboard_focus`), and
`notify_window_focus` forwards SDL focus changes. Text inputs and editors
consume `key_event` when focused. Key names follow TeXmacs conventions
(`lookup_key`, `initialize_keyboard`). **Typing**: a key which produces a
character (printable keycode, no control/alt/command modifier left after
`postprocess_key_event`, which folds shift and option into the keycode) is
not delivered as a key: the system sends the resulting text — composed
with the dead keys and the input method — as `SDL_EVENT_TEXT_INPUT`, which
is delivered instead (" ", "<", ">" become `space`, `<less>`, `<gtr>`); a
dead key alone types nothing. Every other key (return, arrows, function
keys, C-/M-/A- combinations) is delivered as a key and a text event which
follows it within 30 ms (`key_stamp`) belongs to the same keystroke and is
dropped. The scripted `key` command therefore drives control keys and
`text` the characters. The composition of an input method
(`SDL_EVENT_TEXT_EDITING`: dead keys, CJK) is shown by the editor as a
pre-edit: it receives the key `pre-edit:<cursor>:<text>` as with Qt (an
empty text ends it) and the committed text arrives as a text event; the
text inputs ignore the pre-edit keys. The editor applies a pre-edit through
`delayed-keyboard-press`, which waits for 100 ms of `idle-time`, and the
idle time is zero while `check_event (ANY_EVENT)` sees a pending event:
the Vue `check_event` must not count SDL's poll sentinel, an internal event
which sits in the queue after every pump (it did, and nothing depending on
the idle time — the pre-edits, the `:idle` delayed commands — ever ran).
The idle time is also zero while the window has no keyboard focus.

Popup menus (`layout_pull_button`) form a chain through `current_popup`; a
click on a `menu_button` sets `cancel_popup` which closes the chain (and popup
windows). Menus flip to the other side of their button or shift to stay in
the window, are at most as tall as the window and scroll.

* **Kinetic scrolling** (`vue_gui.cpp`, `wheel_event`,
  `wheel_inertia_step`): wheel events scroll at once (a slowly turned wheel
  moves the view in sync) while the speed of the wheel is estimated from
  them (`vue_input_state::wheel_est_x/y`, device pixels per ms, smoothed).
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
  When no event has come for `wheel_stream_dt` (30 ms) and the speed is
  above `wheel_launch_speed` (1 px/ms), the view goes on with that velocity
  (`wheel_vx/vy`) decaying with `wheel_tau` (350 ms), as synthetic wheel
  deltas every frame (`push_wheel`: `mouse_action= "wheel"` for the widgets
  plus `Clay_UpdateScrollContainers` for the Clay container under the
  pointer); a new event stops the glide. **Trackpads**: SDL reports their
  gestures as wheel events with fractional ("precise") deltas and no phase,
  so fingers which pause cannot be told from fingers which are lifted. On
  macOS the system computes the momentum itself and, with the hint
  `SDL_HINT_MAC_SCROLL_MOMENTUM` set before `SDL_Init` (SDL drops these
  events by default), sends it as a stream of wheel events after the fingers
  are lifted: the view follows the fingers exactly while they are down and
  the system glide after; a precise stream (`wheel_precise`, sticky for the
  stream) starts no glide of ours there (`wheel_system_momentum`), only the
  integer ticks of a mouse wheel do. Elsewhere the wheel model applies to
  trackpads too. **Pacing**: a frame costs more than the interval between
  the events of a trackpad, so the loop handles all the wheel and motion
  events already queued in the same frame (their deltas add up in
  `push_wheel`) instead of one event per frame, which lagged behind the
  fingers and made the motion jerky. The editor keeps the fractional SI
  remainder of the small steps (`scroll_rest_x/y`). While a view glides the
  loop does not sleep (5 ms pacing).

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
120 ms, and a tool panel which appears slides in from its edge in 150 ms
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
  (`styled_strings` is rebuilt each layout, the pictures of the icons are
  loaded once in the widget constructors, the fonts come from TeXmacs'
  font cache, the glyphs from MuPDF's) and the profile of a frame is now
  dominated by the editors' repaint and the surface upload, so no further
  cache is kept; the Clay element ids are the one structure which must be
  stable across frames (interned strings).

## Rendering details

`vue_sdl_mupdf_window_rep::process_redraw` clears the surface with the UI
background (red in the F1 debug mode, to spot uncovered areas), replays the
Clay commands and presents the SDL surface. Editors (`vue_simple_widget_rep`)
own a backing store picture repainted incrementally (`invalid_regions`, in
document coordinates) and blitted by their custom render callback.
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
  `wheel-inertia.scm` + a script of 300 wheel steps). Pattern fills,
  rounded corners, arcs and text still go through MuPDF; what remains of a
  frame is the editor repaint (`clear_device` tiles, glyphs), the blit and
  `SDL_UpdateWindowSurface`;
* polygons: nonzero winding for convex, even-odd otherwise (as the PDF and
  X11 renderers; Qt uses the winding rule for non-convex ones);
* lines/arcs/rounded rectangles, clipping, linear transformations
  (`set_transformation` as pdf_hummus), shadows (`new/get/put/apply_shadow`
  by pixmap copies), pictures and scalables (`draw_scalable` falls back to
  the generic conversion when MuPDF cannot load the file);
* still open: the phase of tiling patterns relative to the page (matrix of
  `register_pattern`), `set_brush` also resets the pencil width/caps.

The renderers of the MuPDF plugin (`mupdf_renderer_rep`, used by the Vue
windows and pictures, and `fitz_renderer_rep`) derive from
`basic_renderer_rep`. Since TeXmacs 2.1.5 `renderer_rep` carries a
`pixel_ratio` (device pixels per point, used by the Qt6 port) which enters
`zoomf`, `retina_pixel` and `shrink (glyph, ..., pixel_ratio)`, plus a pure
virtual `clear_device` (the neutral pattern behind the pages) and a `safe`
flag of `set_zoom_factor` checking the consistency of `shrinkf`. The MuPDF
renderers keep the older scheme instead: `pixel_ratio= 1`, the retina factor
is multiplied into the zoom by their own `set_zoom_factor` (hence
`safe= false`), `shrink` is called with ratio 1 and `clear_device` is a no-op
(the editor clears the background itself).
