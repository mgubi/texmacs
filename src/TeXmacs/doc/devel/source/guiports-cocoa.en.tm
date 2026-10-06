<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The native <name|Cocoa> port>

  <source-link|Plugins/NS|src/Plugins/NS> is a native <name|macOS> port written in
  <name|Objective-C++> on <name|AppKit>. It maps every abstract widget to
  native views, as the <name|Qt> port maps them to <name|Qt> widgets, and
  its widget layer was completed on the model of <name|Qt>, so that it
  offers the same features: menus in the menu bar, icon bars, side and
  bottom tools, dialogs made with <scm|tm-widget>, tooltips, the file
  panels, the color picker, printing, the clipboard, drag and drop,
  trackpad gestures and input methods. The macro is <cpp|AQUATEXMACS>,
  <cpp|gui_version ()> returns <verbatim|"ns"> (<scm|ns-gui?> in
  <scheme>), and <scm|qt-gui?> holds, so that the <scheme> code uses the
  same native dialogs and shortcuts as with <name|Qt>. It replaces the
  older <verbatim|Plugins/Cocoa> port (with its nib files), which has been
  removed.

  The status of the port, its test aids and its packaging are described in
  <verbatim|doc/ns-port.md> at the top of the repository (outside the
  <verbatim|src> directory); this page summarizes it.

  <section|Building>

  <verbatim|configure --with-gui=cocoa> (or <verbatim|aqua>) compiles
  <source-link|Plugins/NS|src/Plugins/NS> together with <source-link|Plugins/MacOS|src/Plugins/MacOS> (the
  <name|macOS> extensions are required) and links with the
  <name|Cocoa> and <name|PDFKit> frameworks; <name|CMake> cannot build it.
  <name|MuPDF> is refused: the port has pictures of its own. The
  <abbr|PDF> is written by <TeXmacs> itself with <name|PDFHummus>
  (<source-link|Plugins/Pdf|src/Plugins/Pdf>), which <verbatim|configure> enables when it
  finds <name|libpng>; without it the port has no <abbr|PDF> writer. The
  dependencies of the headers are followed by the <verbatim|make> build,
  also those of the headers of <source-link|Plugins/NS|src/Plugins/NS> which the editor
  includes.

  <source-link|packages/macos/build-ns-app.sh|packages/macos/build-ns-app.sh> configures, builds and makes
  the application bundle (and with <verbatim|--dmg> the disk image), copies
  the libraries which do not come with <name|macOS> into the bundle and
  signs it. <source-link|build-deps.sh|packages/macos/build-deps.sh> builds these libraries
  statically for an older <name|macOS> and either architecture,
  <source-link|merge-universal.sh|packages/macos/merge-universal.sh> merges two applications into a
  universal one, and <source-link|check-app.sh|packages/macos/check-app.sh> checks the signature and
  that only system libraries and those of the bundle are used. The
  continuous integration workflow <verbatim|.github/workflows/macos-ns.yml>
  does all this on the branch <verbatim|ns_ci>.

  <section|Structure>

  <\description>
    <item*|<source-link|ns_widget.mm|src/Plugins/NS/ns_widget.mm>>The widget constructors, the base,
    window, popup and view widgets.

    <item*|<source-link|ns_tm_widget.mm|src/Plugins/NS/ns_tm_widget.mm>>The main window: menu bar, icon
    bars, canvas, side and bottom tools, footer (which can be
    interactive), full screen.

    <item*|<source-link|ns_ui_element.mm|src/Plugins/NS/ns_ui_element.mm>, <source-link|ns_menu.mm|src/Plugins/NS/ns_menu.mm>>Menu
    items (for the menus and the icon bars) and the views of the dialogs,
    laid out with <verbatim|NSStackView> and <verbatim|NSGridView>; the
    menu classes and the popup menus.

    <item*|<source-link|ns_dialogues.mm|src/Plugins/NS/ns_dialogues.mm>>File chooser, questions and input
    dialogs, line inputs, the embedded editor, the color picker and the
    printer.

    <item*|<source-link|ns_simple_widget.mm|src/Plugins/NS/ns_simple_widget.mm>, <source-link|TMView.mm|src/Plugins/NS/TMView.mm>>The
    canvas of the editor: a document view in a scroll view, and the canvas
    which follows its visible part, with a backing store of that size.

    <item*|<source-link|ns_renderer.mm|src/Plugins/NS/ns_renderer.mm>, <source-link|ns_picture.mm|src/Plugins/NS/ns_picture.mm>>The
    <name|Core Graphics> renderer and the pictures.

    <item*|<source-link|ns_gui.mm|src/Plugins/NS/ns_gui.mm>>The application, the event loop, the
    clipboard and the test aids.
  </description>

  <section|The event loop>

  <cpp|ns_gui_rep::event_loop> calls <verbatim|[NSApp finishLaunching]>,
  installs the test timers and then <verbatim|[NSApp run]>. As in
  <source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>, the keyboard, mouse, resize and command events are
  queued and handled by <cpp|ns_gui_rep::update>, which processes a
  bounded number of them, runs the interpose handler and repaints the
  canvases (<cpp|ns_simple_widget_rep::repaint_all>); an
  <verbatim|NSTimer> (also active in modal panels) schedules the next
  update. Only the canvases of a visible window are repainted. The delayed
  commands are also run by <cpp|exec_pending_commands>, needed by a
  server in the same process.

  <section|Keyboard and input methods>

  <verbatim|TMView> passes the key events to
  <verbatim|interpretKeyEvents:> and implements the
  <verbatim|NSTextInputClient> protocol (<verbatim|insertText:>,
  <verbatim|setMarkedText:>, ...), so that an input method gets all the
  keys while it composes, and its marked text is shown as a pre-edit. The
  keys are named as in <name|Qt> (<verbatim|space>, <verbatim|S-tab>,
  <verbatim|enter>, the Cork names of the other characters); command is
  <verbatim|M->, option <verbatim|A-> and control <verbatim|C->. Option
  with a letter gives <verbatim|A-<em|letter>> when that key is bound and
  the composed character otherwise; this is decided when the key is
  pressed, where <name|Qt> lets the kernel decide. Only the canvas which
  is the first responder of the key window has the focus. The text fields
  of the dialogs get the usual editing shortcuts (command-C, -X, -V, -A,
  -Z) through a local event monitor.

  <section|Menus and icon bars>

  Each main window has its own menu bar, installed when the window becomes
  main (not while a menu is open), and its menus are computed each time
  they are shown, as in <name|Qt>. The menus show the shortcuts as native
  key equivalents, but the keys go to the editor, which handles them as in
  <name|Qt>. The icon bars are flat rows of buttons and input fields (the
  focus bar), with a small triangle on the icons which have a pull-down
  menu. The icons are drawn from the <abbr|SVG> files of the light or dark
  variant of the icon set, and follow a change of the appearance of the
  system.

  <section|Clipboards, dialogs and printing>

  <paragraph|Clipboards.>The selection <verbatim|"primary"> is the general
  pasteboard; the other selections are internal. A copy publishes the
  <TeXmacs> snippet under the private type
  <verbatim|org.texmacs.clipboard>, the process id under
  <verbatim|org.texmacs.pid>, and plain text or <name|HTML>; pasting takes
  a <TeXmacs> snippet, an image (<abbr|PNG> or <abbr|TIFF>, converted to
  <abbr|PNG>), <name|HTML> or plain text, in this order, as
  <cpp|qt_gui_rep::get_selection> does.

  <paragraph|Dialogs.>File choosers are the open and save panels of
  <name|macOS>, with the filters of the file types; the other dialogs are
  built from the <scheme> widgets. A dialog with tabs is resized to the tab
  shown.

  <paragraph|Printing.>Since <scm|qt-gui?> holds, the print dialog is used:
  the <abbr|PDF> written by <TeXmacs> is handed to the print panel of the
  system through <name|PDFKit> (<verbatim|PDFDocument>,
  <verbatim|NSPrintOperation> in <source-link|ns_dialogues.mm|src/Plugins/NS/ns_dialogues.mm>), without
  <name|Ghostscript>; PostScript is only printed through
  <name|Ghostscript>.

  <section|The renderer>

  <cpp|ns_renderer_rep> draws with <name|Core Graphics>. Its clipping
  follows <verbatim|QPainter::setClipRect>, patterns are cached as in
  <name|Qt>, and shadows draw in the context of their master. The glyphs
  of the fonts with a file (<name|TrueType>, <name|OpenType>, Type 1) are
  drawn from their outlines (<cpp|tt_glyph_outline>), filled with the
  antialiasing of <name|Core Graphics> into images cached per glyph, size
  and color, at the place of the bitmaps to the pixel; the other glyphs use
  the bitmaps made by <cpp|shrink>. The backing store of a canvas wraps
  around in both directions, so that a scroll only repaints the uncovered
  strips.

  <section|Test aids>

  Other programs may not capture or drive the windows of <TeXmacs>, so the
  port reads environment variables (see <verbatim|doc/ns-port.md> and
  <source-link|ns_gui.mm|src/Plugins/NS/ns_gui.mm>). With them, <TeXmacs> becomes the active
  application.

  <descriptive-table|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-hpart|2>|<cwith|1|-1|2|2|cell-hpart|3>|<table|<row|<cell|Variable>|<cell|Effect>>|<row|<cell|<verbatim|TEXMACS_NS_SNAPSHOT>>|<cell|directory
  in which the windows are saved as <abbr|PNG> every 3<nbsp>s>>|<row|<cell|<verbatim|TEXMACS_NS_TYPE>>|<cell|text
  typed into the canvas after 2<nbsp>s>>|<row|<cell|<verbatim|TEXMACS_NS_CLICK>>|<cell|a
  click, move or drag on the canvas before typing>>|<row|<cell|<verbatim|TEXMACS_NS_PRESS>>|<cell|steps
  pressing buttons, tabs and segments by their label, filling fields,
  closing modal panels>>|<row|<cell|<verbatim|TEXMACS_NS_WINDOW_TEST>>|<cell|steps
  on the windows, the menu bar, the lists and the combo
  boxes>>|<row|<cell|<verbatim|TEXMACS_NS_MENUS>>|<cell|print the menu bar
  to the given depth>>|<row|<cell|<verbatim|TEXMACS_NS_SCROLL>,
  <verbatim|TEXMACS_NS_SCROLL_STEP>>|<cell|scroll the document in
  steps>>|<row|<cell|<verbatim|TEXMACS_NS_DROP>>|<cell|drop a file on the
  canvas>>|<row|<cell|<verbatim|TEXMACS_NS_BENCH>,
  <verbatim|TEXMACS_NS_BENCH_SIZE>>|<cell|time repaints, scrolls and zooms
  of the front window, print a table and quit>>|<row|<cell|<verbatim|TEXMACS_NS_THEME>>|<cell|<verbatim|light>
  or <verbatim|dark>, instead of the preference <verbatim|gui
  theme>>>|<row|<cell|<verbatim|TEXMACS_NS_GLYPHS>>|<cell|<verbatim|bitmap>:
  all glyphs from the bitmaps of <cpp|shrink>>>|<row|<cell|<verbatim|TEXMACS_NS_DEBUG_RED>,
  <verbatim|TEXMACS_NS_DEBUG_DRAW>>|<cell|show the parts of the backing
  store which are not kept, print the redrawn rectangles>>>>>

  <section|What is missing>

  <\itemize>
    <item>As in <name|Qt>: no ink widget, no empty widget, no wait widget
    (the wait indicator works), no mouse pointer shapes
    (<cpp|set_mouse_pointer> is empty), no proposals in the color picker.
    The responsive tabs are plain tabs, and tree views ignore their roles.

    <item>The bottom and extra tools have no handle and a fixed height.

    <item>No headless mode: <cpp|is_headless> is not tested.

    <item>Not tested with real hardware: printing on a printer, a real
    input method, gestures other than scrolling, help balloons triggered by
    hovering.

    <item>Since neither <cpp|QTTEXMACS> nor <cpp|VUETEXMACS> is defined,
    <scm|x-gui?> holds as well as <scm|qt-gui?>, see
    <hlink|pitfalls|guiports-pitfalls.en.tm>.
  </itemize>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
