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

## State (2026-09-27, evening)

**TeXmacs compiles, links and starts with the NS interface:** the main window
opens with its title, canvas and footer, the Cocoa event loop runs the
TeXmacs update cycle, documents are drawn correctly (text, mathematics,
at the right scale on Retina screens), and **documents can be edited**:
typing (including return and backspace) and clicking to move the cursor
work. The TeXmacs menus are in the menu bar (after an application menu
created in the code, since there is no MainMenu.nib outside a bundle), and
all of them, with their submenus, can be built. The icon bars are shown
above the canvas (with the PNG icons, at double resolution on Retina
screens), and the footer shows the messages of TeXmacs. Dialogs made with
`tm-widget` work, such as the preferences and the page format (tabs, pop-up
menus, buttons, refreshable parts, and embedded TeXmacs editors).

Testing aids (other programs are not allowed to capture or control the
windows):
* with `TEXMACS_NS_SNAPSHOT` or `TEXMACS_NS_TYPE`, TeXmacs activates itself
  and its window becomes the key window (otherwise, started in the
  background, the editor does not keep the focus);
* `TEXMACS_NS_SNAPSHOT=<dir>`: the windows are saved as
  `<dir>/window-<i>.png` every 3 seconds;
* `TEXMACS_NS_TYPE=<text>`: after 2 seconds, the text is sent as key events
  to the canvas (`\r` is return, `\b` backspace);
* `TEXMACS_NS_CLICK=<x>,<y>[,right]`: a click at this point of the canvas
  (in points), before typing;
* `TEXMACS_NS_PRESS=<label>`: after 4 seconds, the button or the tab with
  this label is pressed (in the frontmost window which has it);
* `TEXMACS_NS_MENUS=<depth>`: after 3 seconds, the menu bar is printed with
  its submenus up to this depth (which builds the lazy menus).

For example, to check the result:

```sh
TEXMACS_NS_CLICK=30,113 TEXMACS_NS_TYPE='X' texmacs.bin -x \
  '(delayed (:pause 5000) (display* (buffer-get-body (current-buffer))) (quit-TeXmacs))'
```

### What was done

* **Widget layer** (the 2018 refactoring, completed on the model of Qt):
  `ns_widget.mm` holds the factories and the base, window, popup and view
  widgets; the main window moved to `ns_tm_widget.mm`; `ns_ui_element.mm`
  builds menu items (menus and toolbars) and views (dialogs: labels, icons,
  buttons, check boxes, popups, stacks, grids, glue, scroll and split views);
  `ns_menu.mm` has the Objective-C menu classes and `ns_menu_rep` (popup
  menus); `ns_dialogues.mm` implements the file chooser (NSOpenPanel,
  NSSavePanel), questions and input dialogs (NSAlert), the text input and the
  embedded editor.
* **Event loop** (`ns_gui.mm`): queued keyboard, mouse, resize and command
  events and the update cycle of `qt_gui.cpp`, with an NSTimer and
  `[NSApp run]`.
* **Renderer**: `clear_device`, shadows drawing in the context of their
  master (as the Qt proxy renderers), the current `get_pattern_data`,
  `decode`, `shrink`, pixel ratio; `load_picture`, `save_picture`.
* **Simple widget**: its `TMView` is created on demand (`as_nsview`).
* **Builds without Qt** (also useful for X11): stubs for the client/server
  functions, `execute_shell` defined only once, and `AQUATEXMACS` treated as
  Qt for delayed commands and native pictures.

### Known gaps (FIXME in the code)

* the rows of icons are shown or hidden together; the side tools (left,
  right, bottom, extra) are not shown;
* the backing store has the size of the whole document (the Qt interface
  uses a canvas of the size of the visible part, which scales to long
  documents);
* menus: keyboard shortcuts, the prefixes `*` and `o`, widgets inside menus;
* views: icons of the icon tabs, filter of the filtered choices, tree
  views, maximal and default sizes of the resize widgets;
* color picker and printing; picture effects and patterns;
* the interactive prompt is a dialog instead of the footer;
* the side tools (left, right, bottom, extra) are ignored.

## After compiling

In order:
1. Check with real input: the input methods (`NSTextInputClient`, with the
   text being composed shown by TeXmacs as in Qt), the contextual menu (it
   is requested at the right place, but synthetic clicks close it at once),
   drag selection.
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
