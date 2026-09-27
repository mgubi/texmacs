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

## State (2026-09-27, night)

**TeXmacs compiles, links and runs with the NS interface, with the features
of the Qt interface.** Documents are drawn and edited (text, mathematics,
pictures, graphics, patterns and picture effects), at the right scale on
Retina screens; scrolling is synchronous, as in Qt, and the document is
centered when it is narrower than the window. The menus are in the menu bar
with their keyboard shortcuts and check marks; the icon bars have their
buttons, labels and input fields (the focus bar), row by row; the footer
shows the messages and the interactive prompt. Dialogs made with
`tm-widget` work (tabs, icon tabs, pop-up menus, lists, filtered lists,
tree views, resizable parts, refreshable parts, embedded editors), and so
do the side and bottom tools (resizable with a handle), tooltips and help
balloons, the wait indicator, the file panels (with the filters of the
file types), the color picker, printing (PDF, and PostScript through
Ghostscript), the clipboard (text, HTML, TeXmacs, images, both ways),
drag and drop, trackpad gestures (pinch, rotate, swipe) and the wheel
(command-wheel zooms).

`qt-gui?` holds for this interface, since it implements the widgets of the
Qt one: the Scheme code uses the same native dialogs and shortcuts.

Testing aids (other programs are not allowed to capture or control the
windows). NOTE: with them, TeXmacs becomes the active application, so that
keys typed meanwhile go to TeXmacs, and the real mouse also reaches it.
* with `TEXMACS_NS_SNAPSHOT` or `TEXMACS_NS_TYPE`, TeXmacs activates itself
  and its window becomes the key window;
* `TEXMACS_NS_SNAPSHOT=<dir>`: the windows are saved as
  `<dir>/window-<i>.png` every 3 seconds;
* `TEXMACS_NS_TYPE=<text>`: after 2 seconds, the text is sent as key events
  to the canvas (`\r` is return, `\b` backspace);
* `TEXMACS_NS_CLICK=<x>,<y>[,right|,move]`: a click (or a mouse move) at
  this point of the canvas (in points), before typing;
* `TEXMACS_NS_PRESS=<label>`: after 4 seconds, the button or the tab with
  this label is pressed; `field:<n>=<text>` types the text in the n-th
  editable field of the window, followed by return; `abort-modal` closes
  the modal window (such as a file panel);
* `TEXMACS_NS_MENUS=<depth>`: after 3 seconds, the menu bar is printed with
  its submenus up to this depth (with the shortcuts and check marks);
* `TEXMACS_NS_SCROLL=<points>`: after 3 seconds, the document is scrolled
  in steps of 40 points, and with `TEXMACS_NS_SNAPSHOT` the window is saved
  after each step (`scroll-<i>.png`);
* `TEXMACS_NS_DROP=<file>`: after 3 seconds, the file is dropped on the
  canvas.
* `TEXMACS_NS_SCROLL_STEP=<points>`: the steps of `TEXMACS_NS_SCROLL`
  (trackpads give fractional ones); with `TEXMACS_NS_SNAPSHOT`, the backing
  store of the canvas is saved as `backing.png` at the end (upside down),
  and `TEXMACS_NS_DEBUG_RED=1` fills the parts to repaint in red first;
* `TEXMACS_NS_DEBUG_DRAW=1`: the rectangles redrawn by the canvas are
  printed (the snapshots redraw everything, and do not show what is on
  screen; other programs cannot capture the windows of TeXmacs).

For example, to check the result:

```sh
TEXMACS_NS_CLICK=30,113 TEXMACS_NS_TYPE='X' texmacs.bin -x \
  '(delayed (:pause 5000) (display* (buffer-get-body (current-buffer))) (quit-TeXmacs))'
```

### What was done

* **Widget layer** (the 2018 refactoring, completed on the model of Qt):
  `ns_widget.mm` holds the factories and the base, window, popup and view
  widgets; the main window is in `ns_tm_widget.mm`; `ns_ui_element.mm`
  builds menu items (menus and icon bars) and views (dialogs);
  `ns_menu.mm` has the Objective-C menu classes and `ns_menu_rep` (popup
  menus); `ns_dialogues.mm` implements the file chooser, questions and
  input dialogs, line inputs (as `QTMLineEdit`), the embedded editor, the
  color picker and the printer.
* **Event loop** (`ns_gui.mm`): queued keyboard, mouse, resize and command
  events and the update cycle of `qt_gui.cpp`, with an NSTimer and
  `[NSApp run]`.
* **Canvas** (`ns_simple_widget.mm`, `TMView.mm`): a document view in the
  scroll view, and the canvas which follows its visible part, with a
  backing store of that size (as in Qt); scrolling repaints at once; mouse
  state as in Qt; input methods (`NSTextInputClient`); gestures; drops.
* **Renderer**: clipping as `QPainter::setClipRect` (the graphics state is
  restored before each new clipping), pictures and the picture renderer,
  patterns, effects, arcs, shadows drawing in the context of their master.
* **Builds without Qt** (also useful for X11): stubs for the client/server
  functions, `execute_shell` defined only once, and `AQUATEXMACS` treated as
  Qt where the generic code has Qt specific parts (delayed commands,
  native pictures, drops, bitmap exports, texmacs output widgets).

### Known gaps

* as in the Qt interface: no ink widget, no empty widget, no mouse pointer
  shapes, no proposals in the color picker;
* not tested with real hardware: full screen (it takes the screen),
  printing on a printer, help balloons triggered by hovering (the tooltip
  windows themselves work), trackpad gestures;
* tree views ignore their roles; the XPM icons without a PNG equivalent
  lose their transparency; shadows with their own context are not copied
  back (they always share the context of their master here).

## Packaging

```sh
cd src
packages/macos/build-ns-app.sh --guile-config <guile-config of Guile 1.8> [--dmg] [--sign IDENTITY]
```

configures (`--disable-qt --enable-cocoa`, which no longer links X11),
builds, and makes `../distr/TeXmacs.app` with `make MACOS_BUNDLE`; with
`--dmg`, `make MACOS_PACKAGE` then makes `../distr/macos/TeXmacs-<version>.dmg`
(and removes the application, as for the Qt version). The libraries which
do not come with macOS (Guile, FreeType, GMP, libltdl, libintl, libpng) are
copied in `Contents/Resources/lib` and relinked by `bundle-libs.sh` (now
also from `/opt/homebrew`); they are signed one by one, and the application
is signed with the identity given, or ad hoc (an application which is
signed ad hoc and not notarized opens on the machine where it was built,
but Gatekeeper rejects it elsewhere). The script checks the signature, the
`Info.plist` and that no library outside the application is used.

Checked: the application, copied elsewhere and started with an empty
environment or with `open`, finds its files in the bundle and edits
documents.

## Next steps

1. Use it with real input and hardware: input methods, the contextual
   menu, drag selection, gestures, full screen, printing, several windows
   and screens.
2. Remove the bundled GNUstep AutoLayout (about 5000 lines of 2013), which
   is not used any more: the views use `NSStackView` and `NSGridView`.
3. Replace the deprecated AppKit constants (`NSResizableWindowMask`, ...),
   which only give warnings.
4. Notarization of the application, for distribution.
