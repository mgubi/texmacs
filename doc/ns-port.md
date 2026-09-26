# Native Cocoa (NS) interface: status of the port

Branch `ns-port`, worktree `~/t/lab/ns-port`, based on `svn_sync`
(8629ced4f7). The code comes from the `ns` branch (715c2ffbdc, 2018) and the
uncommitted work of its worktree `~/t/lab/ns`.

## Building

```sh
cd src
./configure --with-guile=/Users/mgubi/t/guile-1.8.7/usr/bin/guile-config \
            --disable-qt --enable-cocoa
make -k -j8
```

`--enable-cocoa` defines `AQUATEXMACS` and compiles `Plugins/NS` (it used to
compile the old `Plugins/Cocoa`), together with `Plugins/MacOS`.

## State (2026-09-27)

| | |
|---|---|
| Rest of TeXmacs | compiles (403 objects) |
| `Plugins/NS/AutoLayout`, `TMView`, `TMButtonsController`, `ns_simple_widget`, `ns_utilities` | compile |
| `ns_widget`, `ns_dialogues`, `ns_ui_element`, `ns_renderer`, `ns_gui`, `ns_menu`, `ns_picture` | 57 errors |
| Linking, running | not reached |

Commits so far:
1. import of `Plugins/NS`, unchanged;
2. `--enable-cocoa` builds `Plugins/NS`, the editor uses its simple widget;
3. headers usable from C++, AutoLayout includes, and two generic fixes for
   non-Qt builds (`unix_entrypoint.cpp` used Qt unconditionally; a brace of
   `TeXmacs_main` was inside `#ifdef QTTEXMACS`).

## Remaining compile errors, by cause

**Interfaces changed since 2018** (mechanical; follow `Plugins/Qt`):
* `check_type<T> (val, slot)` and `check_type_void (val, slot)` take the
  slot, not a string: about 20 places in `ns_widget.mm` and `ns_dialogues.mm`.
* `plain_window_widget (name, quit)` has a quit command; the preferred
  position and size are now handled by the generic `plain_window_widget`
  (`ns_widget.mm`).
* Renderer: new pure virtual `clear_device`; `get_pattern_data`,
  `decode` and `shrink` have new signatures; `image_gc` is gone
  (`ns_renderer.mm`, `ns_gui.mm`).
* `get_locale_language` was renamed or moved (`ns_gui.mm`).
* `NSBitmapImageFileType` is an enum in the current SDK (`ns_picture.mm`).

**Unfinished refactoring of 2018** (needs decisions):
* `ns_other_widgets.h` now declares the widget classes, but
  `ns_dialogues.mm` still defines `ns_chooser_widget_rep` and
  `ns_field_widget_rep` itself, with other members.
* `ns_ui_element.mm` (uncommitted work) was started from the Qt code: it
  still uses `QAction` and a `ns_glue_widget_rep` which is not declared
  where it is used.
* `ns_widget.mm` defines `make_popup_widget` and `popup_window_widget`
  twice, and `ns_view_widget_rep` does not match its declaration.
* `ns_menu.mm` includes `ns_basic_widgets.h`, deleted in 715c2ffbdc.
* `NOT_IMPLEMENTED` is used in `ns_dialogues.mm` before being defined.

## After compiling

In order:
1. **Link and start:** a window with the canvas, rendering (with Retina
   scaling), keyboard with input methods (`NSTextInputClient`), mouse.
   At this point documents can be edited: this is the main milestone.
2. The `FIXME`/`NOT_IMPLEMENTED` of 2018 (about 60): arcs, alpha, images,
   mouse grab, pointer and cursor, the wait indicator, the empty and ink
   widgets, refreshable and promise widgets.
3. Menus and toolbars, with the lazy menus of TeXmacs.
4. Dialogs and the widget set used by `tm-widget` (forms, tabs, lists,
   embedded editors), one Qt widget at a time.
5. Printing, clipboard, file dialogs.

Suggestions:
* Replace the bundled GNUstep AutoLayout (about 5000 lines of 2013) by
  AppKit's `NSStackView` and `NSGridView`.
* Port the test harness of the Qt interface (`Plugins/Qt/qt_test.cpp` on
  `wip-git-versioning`) to check each step with snapshots and scripted
  menus.
* The deprecated AppKit constants (`NSResizableWindowMask`, ...) only give
  warnings, but should be replaced by their current names.
