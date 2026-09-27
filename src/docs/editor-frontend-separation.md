# Separating the editor from the GUI

A design for future work, not implemented. It says how far the editor
(`Edit/`) depends on the GUI and on the window layer (`Texmacs/`), and how
to cut those dependencies in stages. Draft headers for the proposed
interfaces are in [editor-frontend-separation/](editor-frontend-separation/);
they compile against the current headers but nothing uses them yet.

C++ file references are relative to `src/`. Line numbers are those of
`wip_other_guis` on 2026-09-27.

## Why

- **One binary per GUI.** The editor is compiled for one GUI at a time: its
  class derives from the GUI's widget class, and `Edit/` tests which GUI it
  is built for. Qt, Qtwk, Vue, X11 and Cocoa each need their own build.
- **New GUIs touch the editor.** Every port so far has added its macro to
  the `#if`s of `edit_interface.cpp` (the last were `SDLTEXMACS` and
  `VUETEXMACS`), and then the native Cocoa port, merged on 2026-09-27,
  added `AQUATEXMACS` to them.
- **No editor without a window.** Tests fake a display
  (`QT_QPA_PLATFORM=offscreen`, the Vue snapshot harness); batch
  conversions go through a window too.

## What is already independent

The typesetter and the document model do not depend on the GUI or on the
editor: `Typeset/` (37k lines), `Data/`, `Style/` and `Graphics/` include no
header of `Edit/`, of `Texmacs/` or of a GUI plugin. What follows concerns
only `Edit/` (15k lines) and `Texmacs/` (6k lines: server, buffers, views,
windows).

## How the editor depends on the GUI today

**1. Inheritance chosen at compile time.** `Edit/editor.hpp:18-28` includes
the GUI's widget header and `editor_rep` derives from `simple_widget_rep`
(`editor.hpp:60`), a typedef to `qt_simple_widget_rep`,
`vue_simple_widget_rep` or Widkit's `simple_widget_rep`. The other core
class which does this is `box_widget_rep` (`Texmacs/Window/tm_button.cpp:113`),
for typeset boxes in dialogs. The GUIs call them through the same virtual
methods (`handle_keypress`, `handle_mouse`, `handle_repaint`, ...; see
*The editor as a widget* in [texmacs-gui-architecture.md](texmacs-gui-architecture.md)).
The protocol exists; only the inheritance ties it to one GUI.

**2. The editor asks its own widget.** Through widget messages to itself
and to the canvas of its window:
- `send_invalidate`, `send_invalidate_all`, `query_invalid`;
- `send_keyboard_focus`, `send_keyboard_focus_on`, `send_mouse_grab`,
  `send_mouse_pointer`, `send_cursor`;
- `::get_size` and `::get_position` of `::get_canvas (widget (cvw))`, where
  `cvw` is the window's widget (set in `Texmacs/Data/new_view.cpp:354` and
  `Texmacs/Window/tm_window.cpp:336`).

**3. Tests of the GUI inside `Edit/`:**

| Where | What depends on the GUI | Proposed replacement |
|---|---|---|
| `edit_interface.cpp:221` | scroll bar width from Qt's style | `canvas_host::scrollbar_width` |
| `edit_interface.cpp:246, 257` | Qt 4 divides the canvas size by the retina zoom | done by the Qt GUI in `canvas_host::get_canvas_size` (or dropped with Qt 4, which `qt.m4` and `CMakeLists.txt` still accept) |
| `edit_interface.cpp:866, 876, 887` | 1-pixel frame and centering of narrow documents, except in Qt | `canvas_host::frames_and_centers_document` |
| `edit_interface.cpp:997, 1019, 1049` | thinning of selection and spelling rectangles, except in Qt, SDL, Vue | `canvas_host::fills_selections` |
| `edit_main.cpp:34-39` | includes `qt_gui.hpp`, `qt_utilities.hpp`, `qtwk_gui.hpp` | nothing, once the two lines below move |
| `edit_main.cpp:425` | printing to PNG, JPEG, TIFF only in Qt and Vue | `canvas_host::can_print_bitmaps` |
| `edit_main.cpp:470` | `graphics_file_to_clipboard` only in Qt | a function of `gui.hpp` which every GUI provides (false where unsupported) |
| `edit_keyboard.cpp:18, 297-303` | macOS compose map of Qt's `QTMApplication` | resolved in the Qt GUI before `handle_keypress` |

`edit_spell.cpp:17` includes `MacOS/mac_spellservice.h`. That is a service
of the operating system, not of the GUI, and can stay.

**4. The window layer.** The editor calls the server with the `SERVER`
macro (`editor.hpp:660`, 32 uses), which makes this editor's view current,
calls `server_rep`, and restores the previous view; the server then finds
the window from the current view. The methods used this way:
- messages and footers: `set_message` (16 files), `recall_message`,
  `set_left_footer`, `set_right_footer`;
- scrolling: `set_extents`, `get_extents`, `get_visible`, `scroll_to`,
  `scroll_where`, `set_scrollbars`. They end in `tm_window_rep`, which sends
  them to the canvas (`::set_extents (wid, ...)`);
- the bars: `show_header`, `show_footer`, `menu_main`, `menu_icons`,
  `side_tools`, `bottom_tools`, full screen mode;
- keyboard tables: `kbd_get_command`, `kbd_post_rewrite`,
  `kbd_system_rewrite`, `get_keycomb`;
- `interactive`, `get_default_zoom_factor`.

It also calls `concrete_window`, `buffer_to_windows`, `window_to_buffer`
and `get_server` directly (`edit_interface.cpp`), and uses
`get_selection`/`set_selection` (20 uses), `beep`, `gui_interrupted`,
`check_event` from `Graphics/Gui/gui.hpp`. Those of `gui.hpp` are global
services which every GUI provides; they are no obstacle.

**5. The buffer.** The editor holds a `tm_buffer` (`Texmacs/tm_buffer.hpp`),
the application's buffer, which also lists its views, its links and a
notification flag. `Edit/` uses only `buf->buf` (file information, 26
uses), `buf->data` (42) and `buf->prj` (33).

In the other direction the GUIs include `editor.hpp` (Qt 3 files, Vue 1),
`tm_window.hpp` and `new_view.hpp`/`new_window.hpp` in a handful of files.

## Stage 1: the editor is not a widget any more

About a week. Low risk: the behavior does not change, and each step
compiles and runs.

**Interfaces** (drafts: [canvas_client.hpp](editor-frontend-separation/canvas_client.hpp),
[canvas_host.hpp](editor-frontend-separation/canvas_host.hpp)):
- `canvas_client_rep`: what the GUI calls on the contents of a drawing
  area. It has the `handle_` methods the GUIs call today, with the same
  names and signatures, plus `attach_host`. `editor_rep` and `box_widget_rep`
  implement it instead of deriving from `simple_widget_rep`.
- `canvas_host_rep`: what the contents ask of the drawing area. The widget
  messages of point 2, the scrolling of point 4, and one query for each
  test of point 3.
- `widget canvas_widget (canvas_client_rep* client)`: a new factory which
  each GUI provides next to the others of `Graphics/Gui/widget.hpp`.

**In each GUI:**
- The existing simple widget class (`qt_simple_widget_rep`,
  `vue_simple_widget_rep`, Widkit's) gets a `canvas_client_rep* client`
  member. Its `handle_` methods forward to it; they already exist, and
  `is_editor_widget` and `is_embedded_widget` become questions to the client.
- The same class implements `canvas_host_rep`, mostly by calling what the
  widget messages call today: `send_invalidate (this, ...)` becomes a
  direct call to its invalidation code.
- `canvas_widget` creates it and calls `client->attach_host (this)`.
- The `typedef ... simple_widget_rep` disappears from the GUI headers.

**In the core:**
- `editor_rep` stops deriving from `simple_widget_rep`, and keeps a
  `canvas_host_rep* host` which replaces `this` and `cvw` in widget messages.
- The `editor` handle (`editor.hpp:648`, `EXTEND_NULL (widget, editor)`)
  stops being a widget. The view keeps the widget returned by
  `canvas_widget (ed.rep)` next to the editor, and `attach_view`
  (`new_view.cpp:353`) and `tm_window.cpp:335` pass that widget to
  `set_scrollable`.
- `box_widget_rep` changes the same way. Its creators in `tm_button.cpp`
  return `canvas_widget (new box_widget_rep (...))`.
- The scrolling calls go to `host` directly instead of through `SERVER`.

**Order of the work:**
1. Add the two headers and `canvas_widget` to one GUI (Vue: it has the
   snapshot tests), with the old inheritance still in place.
2. Move `box_widget_rep`, which is small, and check the dialogs.
3. Move `editor_rep`, and replace the tests of point 3 one by one.
4. Port the Qt and Widkit GUIs (Widkit also serves X11 and Qtwk), then
   delete the typedefs.
5. Build `Edit/` once and link it with two GUIs, to prove the point.

**Tests:** the Vue snapshot harness ([vue-testing.md](vue-testing.md)) and
the regression suites before and after each step. After step 3, a
`canvas_host_rep` which records the calls allows tests of the editor
without any GUI.

**What stage 1 gives:**
- one compiled editor for every GUI, and no GUI macro in `Edit/`;
- several GUIs in one binary, chosen at startup (for instance Qt or Vue);
- a new GUI implements `canvas_widget` and two small interfaces, and does
  not touch `Edit/`;
- tests of the editor without a display;
- small mechanical changes, which could be proposed upstream.

## Stage 2: the window and the buffer behind interfaces

One to two weeks. Medium risk: it changes how the editor finds its window.
This is what the `dev` branch started in 2016 (`abs_buffer_rep`, and
`server.hpp` moved to `Edit/`), before it stopped on the ownership of
buffers. That branch has been deleted; its design is the one below.

**Interfaces** (drafts: [editor_host.hpp](editor-frontend-separation/editor_host.hpp),
[editor_buffer.hpp](editor-frontend-separation/editor_buffer.hpp)):
- `editor_host_rep`: what an editor asks of the window which shows it,
  the methods of point 4 except scrolling (which stage 1 moved to
  `canvas_host_rep`), plus `set_modified`, which replaces the loop over
  `buffer_to_windows`. The view gives each editor its host when it attaches
  it, so the editor no longer changes the current view to reach its
  window, and the `SERVER` macro disappears.
- `editor_buffer_rep`: the buffer as the editor sees it: file information,
  document data, project, root path. `tm_buffer_rep` derives from it, so
  no data moves.

**Who owns buffers:** the application, as today. Buffers are created,
looked up by url and destroyed in `Texmacs/Data/new_buffer.cpp`, and
outlive the editors which show them; several views share one buffer. The
editor gets a pointer and never creates or deletes a buffer. A user of the
editor without windows creates its own `editor_buffer_rep` and deletes it
after the editor. This is the question the `dev` branch left open ("Still
not very clear the situation with new_buffer").

**What stage 2 gives:**
- the editor as a library without windows: batch conversion, server-side
  processing, embedding in another program;
- several editors in one window, each with its own host;
- no more switching of the current view around server calls.

**Cost:** it touches `Texmacs/` and `Edit/` where upstream is active, so it
is harder to keep in sync with the SVN trunk than stage 1.

## Not proposed: the editor in another process

A remote GUI (the editor in one process, the GUI in another, talking over a
socket; the `ws` branch of 2020 had started one with WebSockets) would need
to serialize all the widget traffic. Menus, dialogs and tools are built in
Scheme (`kernel/gui/`) as widgets, and the editor glue has 315 functions.
The browser is reached another way, by running TeXmacs in it
(`wip_wasm_vue`), so this is not worth doing now.

## Caveats

- The dependencies were counted from `#include`s and from uses of the
  functions of `server.hpp`, `gui.hpp`, `tm_buffer.hpp` and
  `tm_window.hpp`. Coupling through globals (the current view, `the_et`,
  `the_drd`) and through Scheme does not show that way.
- The drafts were checked with `clang++ -fsyntax-only` against the headers
  of this branch; nothing was built or run.
