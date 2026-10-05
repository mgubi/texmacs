# Native Cocoa (NS) interface: status of the port

Branch `wip_other_guis` (worktree `~/t/lab/ns-port`). The port was made on
the branch `ns-port`, based on `svn_sync` (8629ced4f7), from the `ns` branch
(715c2ffbdc, 2018) and the uncommitted work of its worktree `~/t/lab/ns`;
`ns-port` was merged in `wip_other_guis` (8a3aa0f503) and deleted.

## Building

```sh
cd src
./configure --with-guile=/Users/mgubi/t/guile-1.8.7/usr/bin/guile-config \
            --with-gui=cocoa
make -j8
```

`--with-guile` must name the `guile-config` of Guile 1.8: the Guile 3 of
Homebrew is rejected. The dependencies of the headers are followed
(`src/Deps`), also of the NS headers which `editor.hpp` includes.

`--with-gui=cocoa` defines `AQUATEXMACS` and compiles `Plugins/NS`, together
with `Plugins/MacOS` (so `--disable-macosx-extensions` is refused). The old interface (`Plugins/Cocoa`, with its nibs) and
the branch `ns` were removed (the branch is kept in the local tags
`archive/ns-2018` and `archive/ns-worktree-2026-09-27`).

## State (2026-09-28)

**TeXmacs compiles, links and runs with the NS interface, with the features
of the Qt interface.** Documents are drawn and edited (text, mathematics,
pictures, graphics, patterns and picture effects), at the right scale on
Retina screens; scrolling is synchronous, as in Qt, and the document is
centered when it is narrower than the window. The menus are in the menu bar
with their keyboard shortcuts and check marks; the icon bars have their
buttons, labels and input fields (the focus bar), row by row; the footer
shows the messages and the interactive prompt, and can be interactive
(View › Interactive status bar: the properties at the cursor as menus, the
tags around it as buttons, see src/docs/vue-interactive-footer.md). Dialogs made with
`tm-widget` work (tabs, icon tabs, pop-up menus, lists, filtered lists,
tree views, resizable parts, refreshable parts, embedded editors, pull-down
buttons, editable enums, hidden password fields), and so do the side tools
(resizable with a handle) and the bottom tools, tooltips and help
balloons, the wait indicator, the file panels (with the filters of the
file types), the color picker, printing (the PDF written by TeXmacs with
PDFHummus, handed to the print panel of the system; PostScript only through
Ghostscript), the clipboard (text, HTML, TeXmacs, images, both ways),
drag and drop, trackpad gestures (pinch, rotate, swipe) and the wheel
(command-wheel zooms). The keys are named as in Qt (`space`, `S-tab`,
`enter`, `<` and `>`, the cork names of the other characters; Option with a
letter gives `A-<letter>` when it is bound, the composed character
otherwise); an input method gets all the keys while it composes. Only the
canvas which is the first responder of the key window has the focus.

macOS itself: the files and URLs given by the Finder, the Dock or `open`
are loaded (the first one in the current window, as in Qt); a quit from the
Dock or at logout goes through `safely-quit-TeXmacs`; the close button of a
window runs its quit command (`safely-kill-window` for the main windows);
each main window has its own menu bar, installed when it becomes main (not
while a menu is open); the menus are computed each time they are shown, as
in Qt; the moves and sizes of the windows are kept; the full screen of
macOS and the one of TeXmacs are the same. The windows are not restored at
the start (`ApplePersistenceIgnoreState`).

The appearance is light or dark after the preference `gui theme`, and by
default that of the system. The icons are drawn from the SVG files of the
light or dark variant of the icon sets on `TEXMACS_PIXMAP_PATH`, as in Qt
(so the set of the preference `icon set` is used), otherwise from their PNG
equivalents. They follow a change of the appearance of the system: the
icons of the native controls are drawn in the appearance of their view
(`to_nsimage`), and the canvases are drawn again with the other variant
(`TMAppearanceObserver`).

The look follows macOS where Qt has its own: the selection is translucent
as in Qt; the icon bars are flat, with a small triangle in the corner of
the icons with a pull-down menu (a chevron after text buttons), and a line
with a shadow separates the focus and user bars from the main and mode
bars; the menus show the shortcuts as native key equivalents (they are
drawn by the menus, but the keys go to the editor, which handles them as
in Qt). Dialogs are laid out as in Qt (fields and lists take the extra
space, grids keep their size, glues share it), and a dialog with tabs is
resized to the tab shown. The color palettes show the patterns.

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
* `TEXMACS_NS_CLICK=<x>,<y>[,right|,move|,drag,<x2>,<y2>]`: a click (or a
  mouse move, or a drag to the second point) at this point of the canvas
  (in points), before typing;
* `TEXMACS_NS_PRESS=<steps>`: after 4 seconds, the steps separated by `;`
  are done, one per second; a label presses the button, the tab or the
  segment with this label; `field:<n>=<text>` types the text in the n-th
  editable field of the window, followed by return; `abort-modal` closes
  the modal window (such as a file panel); `dump-views` prints the views of
  all the windows, with their frames;
* `TEXMACS_NS_MENUS=<depth>`: after 3 seconds, the menu bar is printed with
  its submenus up to this depth (with the shortcuts and check marks); with
  `TEXMACS_NS_SNAPSHOT`, the images of the items are saved as
  `item-<i>.png`;
* `TEXMACS_NS_SCROLL=<points>`: after 3 seconds, the document is scrolled
  in steps of 40 points, and with `TEXMACS_NS_SNAPSHOT` the window is saved
  after each step (`scroll-<i>.png`);
* `TEXMACS_NS_DROP=<file>`: after 3 seconds, the file is dropped on the
  canvas;
* `TEXMACS_NS_SCROLL_STEP=<points>`: the steps of `TEXMACS_NS_SCROLL`
  (trackpads give fractional ones); with `TEXMACS_NS_SNAPSHOT`, the backing
  store of the canvas is saved as `backing.png` at the end (upside down);
* `TEXMACS_NS_BENCH=1` (or phases separated by commas: `repaint:<n>`,
  `scroll:<n>`, `zoom:<z>`): after 4 seconds, the front window is set to
  `TEXMACS_NS_BENCH_SIZE` points (default `1000x800`) and the canvas is
  timed a step at a time (each step returns once TeXmacs has updated and
  the canvas has repainted): `repaint:<n>` repaints the whole canvas n
  times, `scroll:<n>` scrolls n steps of 40 points down and n up,
  `zoom:<z>` sets the zoom with `set-window-zoom-factor` (not saved as a
  preference), `hscroll:<n>` scrolls to the right and back, `down:<n>`
  and `right:<n>` only down or to the right (up or left if n < 0), `snap`
  saves the window as `bench-<i>.png` in `TEXMACS_NS_SNAPSHOT`; then a
  table is printed, the window is put back at its size (a preference) and
  TeXmacs quits (see "Benchmark" below);
* `TEXMACS_NS_THEME=light|dark`: the appearance, instead of the preference
  `gui theme`;
* `TEXMACS_NS_GLYPHS=bitmap`: all the glyphs from the bitmaps of
  `shrink`, as before the outlines (to compare);
* `TEXMACS_NS_DEBUG_RED=1`: when the backing store moves (scrolling), the
  parts which it does not keep are red until they are repainted;
* `TEXMACS_NS_DEBUG_DRAW=1`: the rectangles redrawn by the canvas are
  printed (the snapshots redraw everything, and do not show what is on
  screen; other programs cannot capture the windows of TeXmacs);
* `TEXMACS_NS_WINDOW_TEST=<steps>`: after 4 seconds, the steps separated
  by `;`, one per second, on the windows: the close and full screen buttons,
  moves and sizes, a window becoming main, the menu bar and the last pop-up
  menu printed, the tracking of the menu bar, the buttons with a menu of
  the dialogs, the rows of the lists, the combo boxes, Scheme expressions
  (the list is at the end of `ns_tm_widget.mm`).

For example, to check the result:

```sh
TEXMACS_NS_CLICK=30,113 TEXMACS_NS_TYPE='X' texmacs.bin -x \
  '(delayed (:pause 5000) (display* (buffer-get-body (current-buffer))) (quit-TeXmacs))'
```

### Benchmark

```sh
HOME=<test home> TEXMACS_PATH=$PWD/TeXmacs TEXMACS_NS_BENCH=1 \
  [TEXMACS_NS_GLYPHS=bitmap] TeXmacs/bin/texmacs.bin TeXmacs/doc/main/faq/faq.en.tm
```

For each phase, in ms a step: the time of a step (mean, median, worst);
in it, what the canvas spent drawing into its backing store (`paint`:
TeXmacs and the renderer), showing it (`display`), moving it (`move`), and
all it did (`canvas`: the three and the checks of its size); and the
Mpixels drawn a step. The rest of a step is the update of TeXmacs (at a zoom,
mostly typesetting) and the moves of the backing store. `retina_factor`
is that of the main screen when TeXmacs starts (2 on a Retina screen, 1 on
most external ones): for runs which compare, put `("retina-factor" "on")`
(or `"off"`) in `.TeXmacs/system/preferences.scm` of the test home.

Results (2026-10-05, Apple M4, the FAQ, a canvas of 1000x641 points,
retina_factor 2, outline glyphs vs bitmap glyphs, ms):

| phase | outline: step | paint | bitmap: step | paint |
|---|---|---|---|---|
| repaint at zoom 1 (1.9 Mpixels) | 4.1 | 4.0 | 4.2 | 4.0 |
| scroll at zoom 1 | 1.7 | 0.4 | 1.8 | 0.5 |
| zoom to 2 (new glyphs) | 52 | 6.4 | 70 | 22.7 |
| repaint at zoom 2 (2.6 Mpixels) | 4.5 | 4.4 | 5.0 | 4.8 |
| zoom to 0.75 (new glyphs) | 57 | 6.8 | 62 | 10.4 |
| repaint at zoom 0.75 | 4.1 | 3.9 | 3.8 | 3.7 |

Once the glyphs are cached, both draw a screen in the same time (the
images of the glyphs are the same size); the outlines make the new glyphs
of a zoom three times cheaper than `shrink` at zoom 2. With retina_factor 1
(0.5 Mpixels) a repaint is 1.4-1.6 ms and a scroll 0.6 ms.

The backing store wraps around in both directions (`cring`, `ring` in
`ns_simple_widget_rep`): a scroll changes where the view starts in it and
paints the strips uncovered, where it used to make a new backing store and
copy the old one into it at each step (`move` about 1 ms of a step of 1.6
ms at retina_factor 2; moving the pixels in place with `memmove` costs as
much). The canvas paints, and the view draws, in up to four pieces where it
wraps; `unroll_backing_store` puts the pixels back in order (before a
resize, and for `backing.png`). The view draws the backing store through a
`CGImage` which reads its pixels where they are: drawing the
`NSBitmapImageRep` made its memory copy on write, so that the first change
of each page after it was shown copied the page (0.7 ms for the columns of
a horizontal step, which touch all the pages; a part of each repaint too).

A step, before and after (ms; horizontal: a window of 600x700 points at
zoom 2):

| | retina_factor 2 | retina_factor 1 |
|---|---|---|
| vertical scroll | 1.5-2.0 -> 0.5-0.6 | 0.55-0.6 -> 0.4-0.5 |
| horizontal scroll | 1.1-1.3 -> 0.3-0.5 | 0.45 -> 0.3 |
| repaint, 1.3 Mpixels | 2.9 -> 2.0 | |

The windows of each step of `TEXMACS_NS_SCROLL` (steps of 40 and of 13.5
points, both factors) and the backing stores are the same as before, to
the pixel; after scrolls down, right, up and left (`down`, `right`), the
window is the same as after a repaint (but for the scroll bars, which
fade).

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
  backing store of that size (as in Qt); scrolling repaints at once, and
  the backing store only moves by what it copies (also when the scale of
  the screen differs from `retina_factor`); only the canvases of a visible
  window are repainted (as `isVisible` in Qt); mouse state as in Qt; input
  methods (`NSTextInputClient`); gestures; drops.
* **Renderer**: clipping as `QPainter::setClipRect` (the graphics state is
  restored before each new clipping), the glyphs in the color of the pen
  (and text with a pattern, as `draw_bis`), pictures and the picture
  renderer, patterns (cached as in Qt, at the size asked for), effects,
  arcs, shadows drawing in the context of their master (which they do not
  end when they are deleted).
  The glyphs of the fonts with a file (what FreeType reads: TrueType,
  OpenType, Type 1) are drawn from their outlines (`tt_glyph_outline`),
  filled by Core Graphics with its antialiasing into images cached as the
  bitmaps were (one per glyph, size and color), at the place of the bitmaps
  to the pixel; the other glyphs (TeX's bitmap fonts, patterns) keep the
  bitmaps made by `shrink`, which are a little bolder.
* **Generic code**: `AQUATEXMACS` is treated as Qt where the generic code
  has Qt specific parts (delayed commands, native pictures, drops, bitmap
  exports, texmacs output widgets, the repainting and the mouse of the
  editor); `exec_pending_commands` (needed by the sockets, for a server in
  the same process) runs the delayed commands, in `ns_gui.mm`.
* **Strings**: the labels and the inputs are in the cork encoding (converted
  once); the names of files may be in cork (drops, as in Qt) or UTF-8 (the
  file chooser, the files given by macOS): `to_nsstring_utf8` keeps a
  string which looks like UTF-8, as `to_qstring` does.

### Known gaps

* as in the Qt interface: no ink widget, no empty widget, no mouse pointer
  shapes, no proposals in the color picker;
* not tested with real hardware: printing on a printer, a real input method
  (Japanese, Chinese: the keys were tested with marked text made by the
  tests), a real click in the menu bar while the menus change, help balloons triggered by hovering (the tooltip
  windows themselves work), trackpad gestures other than scrolling (which
  was used on a trackpad);
* the bottom and extra tools have no handle (their contents have a fixed
  height, without a scroll view);
* Option with a letter is decided when the key is pressed (`A-<letter>` if
  it is bound), where Qt lets the kernel decide (its fallback to the
  composed character, in `edit_keyboard.cpp` and `tm_config.cpp`, is only
  compiled for Qt);
* tree views ignore their roles; the XPM icons without a PNG equivalent
  lose their transparency; shadows with their own context are not copied
  back (they always share the context of their master here).

## PDF

The PDF is written by TeXmacs itself, with PDFHummus (`src/Plugins/Pdf`,
`PDF_RENDERER`), as in the Qt interface: the exports, and the printing,
which gives that PDF to the print panel of the system (PDFKit), with no
Ghostscript, which is not in the application. Configure enables it for
Qt and Cocoa (`misc/m4/hummus.m4`) when it finds `png.h` and libpng: with
Homebrew, pass `CPPFLAGS=-I/opt/homebrew/include LDFLAGS=-L/opt/homebrew/lib`
to configure (build-ns-app.sh passes the prefix of the dependencies). The
parts of the renderer which used Qt (the tiles of the patterns and their
pixels, the pictures drawn) go through the pictures of the interface
(`load_picture`, `save_picture`) when Qt is not there. Without MuPDF, the
Cocoa interface has no other writer of PDF (see docs/build-and-debug.md).

## Packaging

```sh
cd src
packages/macos/build-ns-app.sh --guile-config <guile-config of Guile 1.8> [--dmg] [--sign IDENTITY]
```

configures (`--with-gui=cocoa`, which does not link X11; the default
`guile-config` is usually not Guile 1.8, so give it),
builds, and makes `../distr/TeXmacs.app` with `make MACOS_BUNDLE`; with
`--dmg`, `make MACOS_PACKAGE` then makes `../distr/macos/TeXmacs-<version>.dmg`
(and removes the application, as for the Qt version). The libraries which
do not come with macOS (with the configuration here: Guile, FreeType, GMP,
libltdl, libintl, libpng) are copied in `Contents/Resources/lib` and relinked by `bundle-libs.sh` (now
also from `/opt/homebrew`); they are signed one by one, and the application
is signed with the identity given, or ad hoc (an application which is
signed ad hoc and not notarized opens on the machine where it was built,
but Gatekeeper rejects it elsewhere). The script checks the signature, the
`Info.plist` and that no library outside the application is used.

Checked: the application, copied elsewhere and started with an empty
environment or with `open`, finds its files in the bundle and edits
documents.

### Older versions of macOS, both architectures

The libraries of Homebrew are built for the version of macOS where they are
installed (and so is an application which uses them). For a version of macOS
which runs on other machines:

```sh
cd src
packages/macos/build-deps.sh ~/tm-deps-arm64 arm64 12.0
packages/macos/build-deps.sh ~/tm-deps-x86_64 x86_64 12.0     # under Rosetta
packages/macos/build-ns-app.sh --deps ~/tm-deps-arm64 --arch arm64 --min-macos 12.0
```

`build-deps.sh` builds GMP, libltdl, libpng, FreeType and Guile 1.8.8 from
their sources, as static libraries for this architecture and version of
macOS; `build-ns-app.sh --deps` then builds with them only (not with
Homebrew: its `PATH` and `pkg-config` exclude it), with
`MACOSX_DEPLOYMENT_TARGET` and `--with-osx` (hence `LSMinimumSystemVersion`)
set to the version. x86_64 on Apple silicon is built under Rosetta (both
scripts run themselves again with `arch -x86_64`). With the application
made for each architecture (from the same sources, in two copies of them),
`packages/macos/merge-universal.sh ARM64.app X86_64.app OUT.app [OUT.dmg]`
makes one of both with `lipo`, signs it again, and makes its disk image.
`packages/macos/check-app.sh` (run by both) checks the signature, and that
every program and library of the application, for each architecture, only
uses the libraries of macOS and its own (the references `@rpath`,
`@loader_path` and `@executable_path` must lead to files of the
application, and no library path leads outside), and does not need a
version of macOS after `LSMinimumSystemVersion`. The sources of the
libraries are checked with their SHA-256 (in `build-deps.sh`).

The version is 12.0. The sources compile down to 10.13 (the oldest target
of the current Xcode) for x86_64, except for a few calls, all in the
tool bars: `separatorColor` and `controlAccentColor` (10.14) and the symbol
images (11.0), which would need `@available`.

### Continuous integration

`.github/workflows/macos-ns.yml` (GitHub Actions, macOS 15 runners) does
the above: a job per architecture builds the libraries (cached, until
`build-deps.sh` changes) and the application, and a last job merges them
into `TeXmacs-<version>-universal.dmg`, then starts the application of the
disk image with each architecture (x86_64 under Rosetta), a test home,
`TEXMACS_NS_SNAPSHOT` and the document `packages/macos/ci-colors.tm` (big
red text) on the command line: the run fails unless a window shows the red
text (`packages/macos/png-has-red.py`). The disk
image and the snapshots are artifacts of the run. The disk image is signed
ad hoc: after installing it elsewhere, `xattr -dr com.apple.quarantine
/Applications/TeXmacs.app`. It runs on the branch `ns_ci` only (the work in
`wip_other_guis` triggers nothing):

```sh
git push origin wip_other_guis:ns_ci
```

or by hand from the Actions tab.

## Next steps

1. Use it with real input and hardware: input methods, the contextual
   menu, drag selection, gestures, full screen, printing, several windows
   and screens.
2. Notarization of the application, for distribution.

(Done: the bundled GNUstep AutoLayout was removed, the views use
`NSStackView` and `NSGridView`; the deprecated AppKit constants were
replaced; the unused `NSToolbar` of the main window was removed.)
