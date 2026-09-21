# How TeXmacs talks to a GUI plugin

Notes on the parts of the core a GUI plugin has to serve. File references are
relative to `src/`.

## The widget abstraction

`Graphics/Gui/widget.hpp` declares the **factory functions** every GUI must
provide (`plain_window_widget`, `texmacs_widget`, `menu_button`,
`text_widget`, `input_text_widget`, `choice_widget`, `tabs_widget`, ...).
`Graphics/Gui/widget.cpp` holds shared helpers (`get_default_styled_font`,
`use_side_tools`...). A `widget` is a refcounted pointer to a `widget_rep`;
each plugin defines its own reps (`vue_widget_rep`).

Communication uses **slots** (`Graphics/Gui/message.hpp`) with five verbs:

* `send (w, slot, blackbox)` — set a property or trigger an action
  (`SLOT_VISIBILITY`, `SLOT_POSITION`, `SLOT_SIZE`, `SLOT_NAME`,
  `SLOT_DESTROY`, `SLOT_KEYBOARD_FOCUS`, `SLOT_MOUSE_GRAB`, `SLOT_REFRESH`,
  `SLOT_INVALIDATE`, `SLOT_SCROLL_POSITION`, `SLOT_*_VISIBILITY`...);
* `query (w, slot, type_id)` — read a property (`SLOT_SIZE`, `SLOT_IDENTIFIER`,
  `SLOT_STRING_INPUT`, `SLOT_*_VISIBILITY`...);
* `read (w, slot, index)` / `write (w, slot, index, widget)` — get or set a
  sub-widget (`SLOT_MAIN_MENU`, `SLOT_MAIN_ICONS`, `SLOT_SIDE_TOOLS`,
  `SLOT_FORM_FIELD`, `SLOT_WINDOW`...);
* `notify (w, slot, blackbox)` — the GUI tells a widget something changed.

`message.hpp` also has the typed wrappers used by the core (`set_visibility`,
`get_size`, `set_position`, `send_keyboard_focus`, `set_side_tools`, ...).
Blackboxes are type-checked: `open_box<T>` fails on a type mismatch
(`vue_vertical_list` and `vue_horizontal_list` are different types even with
the same fields).

Beware of the **slot enumeration order**: the visibility slots of the main
window are `HEADER, MAIN_ICONS, MODE_ICONS, FOCUS_ICONS, USER_ICONS,
SIDE_TOOLS, LEFT_TOOLS, BOTTOM_TOOLS, EXTRA_TOOLS, FOOTER` (each interleaved
with its widget slot), whereas the `mask` of `texmacs_widget` uses bits
`header, main, mode, focus, user, footer, side, left, bottom, extra`.

## The editor as a widget

Every GUI must provide `simple_widget_rep` (a typedef to its class) with the
virtual protocol the editor overrides (`Edit/Interface`): `handle_get_size_hint`,
`handle_notify_resize`, `handle_keypress`, `handle_keyboard_focus`,
`handle_mouse (kind, x, y, mods, t, data)`, `handle_set_zoom_factor`,
`handle_clear`, `handle_repaint`. The editor drives scrolling through
`SLOT_EXTENTS`, `SLOT_SCROLL_POSITION`, `SLOT_VISIBLE_PART`, `SLOT_INVALIDATE*`
(see the long comment above `vue_simple_widget_rep` in `vue_widget.cpp`).
Mouse kinds are strings: `press-left`, `release-right`, `move`,
`dragging-left`, `enter`, `leave`, ...; `edit_mouse.cpp` destroys its popup
menu on *any* editor mouse event but `leave` (it assumes the popup grabbed the
pointer). The editor draws its cursor only when `got_focus`.

Embedded documents (`texmacs-output`, `texmacs-input` markup) are
`box_widget_rep` / editor widgets created in `Texmacs/Window/tm_button.cpp`,
which replaces a white document background by a color meant to blend into the
dialog (per GUI).

## Windows and dialogs in the core

* `Texmacs/Window/tm_window.cpp`: `window_create_*`, `window_show`,
  `window_set_size/position`, `window_delete` (sends `SLOT_DESTROY` then
  `destroy_window_widget`), tool installation (`side_tools`, `bottom_tools`
  from `edit_interface_rep::resume`), `windows_refresh`.
* `Texmacs/Window/tm_dialogue.cpp`: `interactive` creates an
  `inputs_list_widget`, sets the fields' type/proposals through
  `get_form_field`, shows it with `dialogue_start` (which calls
  `plain_window_widget` on the inputs list itself) and focuses field 0; the
  dialogue command reads the fields with `get_string_input` (a quoted string
  or `"#f"` for cancel) and ends with `(dialogue-end)` →
  `destroy_window_widget`. The dialog widget must keep its own reference to
  the command it calls.
* Scheme dialogs (`kernel/gui/menu-widget.scm`): `top-window`,
  `dialogue-window`, `interactive-window` create a window handle
  (`alt-window-handle`), build the widget with `make-menu-widget*` and show it
  with `alt-window-create-quit`/`alt-window-show`; the quit command runs
  `alt-window-delete`, so a plain window must forward `SLOT_DESTROY` only once.
* Popups: `edit_mouse.cpp` (context menu) uses `popup_widget` +
  `popup_window_widget`, positions it at the pointer and sends
  `send_mouse_grab`; tooltips go through `window_create_tooltip`.

## Scheme widget markup

`kernel/gui/menu-define.scm` is the grammar (`gui-make-*`),
`kernel/gui/gui-markup.scm` the `$...` macros, `kernel/gui/menu-widget.scm`
the conversion to C++ widgets (`make-menu-*`, calling `widget-*` glue from
`Scheme/Glue/build-glue-basic.scm`). Useful forms:

* containers: `vlist`/`vertical`, `hlist`/`horizontal`, `aligned` with
  `item`, `tabs` with `(tab label body)`, `icon-tabs` with
  `(icon-tab "icon.xpm" label body)`, `padded`, `centered`, `resize`,
  `scrollable`, `hsplit`/`vsplit`, `division "<class>"`, `class`, `extend`;
* inputs: `(input cmd type proposals width)`, `(enum cmd vals val width)`,
  `(choice cmd vals val)`, `(choices cmd vals vals)`, `(filtered-choice cmd
  vals val filter)`, `(toggle cmd on)`, `(tree-view cmd tree roles)`,
  `(ink cmd)`, `(color-input cmd bg? proposals)`; in these, `answer` is bound
  to the value in `cmd`;
* styles: `(bold ...)`, `(grey ...)`, `(mono ...)`, `(inert ...)`,
  `(centered ...)`, `(mini pred? ...)` (mini only when the "use minibars"
  preference is on), `explicit-buttons` (push buttons), `plain-style`;
* menus: `("Label" (action))`, `(-> "Submenu" ...)`, `(=> ...)`, `---`
  separators, `===` vertical space, `//` horizontal space, `>>` growing glue,
  `(link menu-name)`, `(dynamic (widget args))` (refreshable),
  `(refreshable "kind" ...)`.
  Check marks come from `:check-mark` properties of the commands and reach
  `menu_button` as the `pre` argument (`"v"`, `"*"`, `"o"`).

* settings (2.1.5, used by the preference tools): `(setting-toggle cmd
  "Description" on)`, `(setting-enum cmd "Description" vals val width)`,
  `(setting-group "Title" items...)` → `setting_*_widget`; `choice`/`choices`
  now pass the style; `tabs`/`icon-tabs` become `responsive_*_tabs_widget`
  when the "responsive tabs default mode" preference asks for it.

Tools: `tm-tool`/`tm-tool*` define a tool (`:name`, body) and its wrapper
`texmacs-side-tool`; `tool-select pos tool` (`:right`, `:left`, `:bottom`,
`:transient-*`) installs it in a window; `side-tools?` requires the "side
tools" and "developer tool" preferences. A side holds **one tool per
position** (`set-window-tool` replaces the list of the position, upstream
semantics since "Improved tool management", 2023): Edit > Preferences and
the Document tools all use `:right` and replace each other; only
`tool-toggle` (Developer > Experimental side tools) adds to a position, and
tools at different positions (`:right` + `:bottom-right`, `:left`) are
stacked by `texmacs-side-tools` (`main-menu.scm`) with a growing glue between
the top and bottom groups. `tool-close` removes one tool (the "x" of the
title bar).

## Client/server and sockets

Since 2.1.5 the TeXmacs server/client (`System/Link/texmacs_server.cpp`,
`texmacs_client.cpp`, `client_server.hpp`) is implemented on Qt sockets
(`QTMSockets`) only; the former `socket_server.cpp`/`socket_link` are gone.
Non-Qt builds compile the `#else` stubs of those files ("sockets are not
implemented"): `server_can_start` returns false, `client_protocol_version`
the protocol constant, and `connection_start` has no `socket` link type.

## Fonts and colors in the UI

`get_default_font (tt, mini, bold)` (plugin) chooses the UI font
(`apple-lucida` on macOS, `ecrm`/`ecss` otherwise, `modern/tt` for
monospaced); `get_default_styled_font (style)` maps the style flags.
Named colors: `named_color ("dark grey")`, `tm_background` (surround of the
pages), `light_grey` (208).
