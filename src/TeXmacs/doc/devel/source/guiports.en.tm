<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The graphical user interface ports>

  <section|Introduction>

  <TeXmacs> does not talk to a particular toolkit. The kernel only knows the
  abstract interfaces of <source-link|Graphics/Gui/gui.hpp|src/Graphics/Gui/gui.hpp> (the application:
  main loop, clipboards, fonts, screen size, ...), of
  <source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp> (the widget constructors and the
  message protocol) and of <source-link|Graphics/Renderer/renderer.hpp|src/Graphics/Renderer/renderer.hpp> (the
  drawing surface). A <em|port> is a directory in
  <source-link|src/src/Plugins/|src/Plugins> which implements these interfaces for one
  toolkit. This chapter is a comparative map of the ports which exist in
  this source tree and describes the port specific internals which are not
  covered elsewhere: how each port is selected and built, its event loop,
  keyboard and input methods, clipboards and printing.

  The abstract interfaces themselves are documented in <hlink|the abstract
  widget system|widgets.en.tm>, which also contains a checklist for
  <hlink|porting <TeXmacs> to another toolkit|widgets-port.en.tm>; the
  reference port is described in detail in <hlink|the <name|Qt>
  implementation|widgets-qt.en.tm>, and the original toolkit in <hlink|the
  graphical user interface (historical Widkit toolkit)|gui.en.tm>. The screen
  renderers of the ports are compared in <hlink|the renderer
  implementations|renderer-backends.en.tm>, and the operating system layers
  (which are independent of the graphical port) in <hlink|platform
  support|system-platforms.en.tm>. Dialogs written in <TeXmacs> markup
  (<scm|has-markup-gui?>, see <hlink|the GUI through
  markup|gui-markup.en.tm>) only use the abstract constructors, so they work
  in every port.

  All file names are relative to <source-link|src/src/|src> unless stated
  otherwise.

  <section|Overview>

  The ports fall into three families. <name|Qt> (with its fork
  <source-link|Plugins/Qt6|src/Plugins/Qt6>) and <name|Cocoa> map every
  abstract widget to a native widget of the toolkit. <name|X11>, <name|SDL>
  and <name|Qtwk> use the toolkit only for windows, events and clipboards
  and draw all widgets themselves with <name|Widkit>
  (<source-link|Plugins/Widkit|src/Plugins/Widkit>), the original widget library of
  <TeXmacs>. <name|Vue> also draws all widgets itself, but in immediate
  mode, with the layout library <name|Clay>.

  <paragraph|Selection.>Exactly one port is compiled in; it is chosen when
  <TeXmacs> is configured.

  <descriptive-table|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-hpart|2>|<cwith|1|-1|2|2|cell-hpart|4>|<cwith|1|-1|3|3|cell-hpart|4>|<cwith|1|-1|4|4|cell-hpart|3>|<cwith|1|-1|5|5|cell-hpart|3>|<table|<row|<cell|Port>|<cell|Directories>|<cell|<verbatim|configure>>|<cell|<name|CMake>
  <verbatim|TEXMACS_GUI>>|<cell|Macro>>|<row|<cell|<name|Qt>>|<cell|<source-link|Plugins/Qt|src/Plugins/Qt>>|<cell|<verbatim|--with-gui=qt>
  (default)>|<cell|<verbatim|Qt> (default), <verbatim|Qt6>, <verbatim|Qt5>,
  <verbatim|Qt4>>|<cell|<cpp|QTTEXMACS>>>|<row|<cell|<name|Qt6>
  fork>|<cell|<source-link|Plugins/Qt6|src/Plugins/Qt6>>|<cell|<verbatim|--with-gui=qt
  --enable-qt-new> (default on <name|Android>)>|<cell|none>|<cell|<cpp|QTTEXMACS>>>|<row|<cell|<name|Qtwk>>|<cell|<source-link|Plugins/Qtwk|src/Plugins/Qtwk>,
  <name|Widkit>, part of <verbatim|Plugins/Qt>>|<cell|<verbatim|--with-gui=qtwk>>|<cell|none>|<cell|<cpp|QTWKTEXMACS>
  and <cpp|QTTEXMACS>>>|<row|<cell|<name|X11>>|<cell|<source-link|Plugins/X11|src/Plugins/X11>,
  <name|Widkit>>|<cell|<verbatim|--with-gui=x11>>|<cell|<verbatim|X11>>|<cell|<cpp|X11TEXMACS>>>|<row|<cell|<name|SDL>>|<cell|<source-link|Plugins/SDL|src/Plugins/SDL>,
  <name|Widkit>, <source-link|Plugins/MuPDF|src/Plugins/MuPDF>>|<cell|<verbatim|--with-gui=sdl>>|<cell|<verbatim|SDL>>|<cell|<cpp|SDLTEXMACS>>>|<row|<cell|<name|Vue>>|<cell|<source-link|Plugins/Vue|src/Plugins/Vue>,
  <source-link|Plugins/MuPDF|src/Plugins/MuPDF>>|<cell|<verbatim|--with-gui=vue>>|<cell|<verbatim|Vue>
  (without the GPU renderer)>|<cell|<cpp|VUETEXMACS>>>|<row|<cell|<name|Cocoa>>|<cell|<source-link|Plugins/NS|src/Plugins/NS>,
  <source-link|Plugins/MacOS|src/Plugins/MacOS>>|<cell|<verbatim|--with-gui=cocoa>
  (or <verbatim|aqua>)>|<cell|none>|<cell|<cpp|AQUATEXMACS>>>>>>

  Headless mode (option <verbatim|-headless>) is not a port but a run-time
  mode, implemented by <name|Qt> (both directories) and <name|Vue>, which
  make no windows, and by <name|SDL>, <name|X11> and <name|Cocoa>, which
  make their windows but never show them, see <hlink|selecting and building
  a port|guiports-build.en.tm>.

  <paragraph|Run time.>The port can be recognized with <cpp|gui_version ()>
  (exported as <scm|gui-version>) and with the predicates of
  <source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp>, exported as <scm|qt-gui?>,
  <scm|x-gui?> and <scm|vue-gui?>. <scm|qt-gui?> means \Pimplements the
  widgets and dialogs of <name|Qt>\Q, and <scm|x-gui?> \Phas the historical
  <name|X11> look and feel\Q. <scheme> also defines <scm|ns-gui?>,
  <scm|qt5-gui?>, <scm|qt6-or-later-gui?>, ... from <scm|gui-version>, in
  <source-link|kernel/boot/abbrevs.scm|TeXmacs/progs/kernel/boot/abbrevs.scm>.

  <descriptive-table|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-hpart|2>|<cwith|1|-1|2|2|cell-hpart|3>|<cwith|1|-1|3|3|cell-hpart|3>|<cwith|1|-1|4|4|cell-hpart|4>|<cwith|1|-1|5|5|cell-hpart|2>|<table|<row|<cell|Port>|<cell|<cpp|gui_version>>|<cell|Predicates
  which hold>|<cell|Screen renderer>|<cell|Lines>>|<row|<cell|<name|Qt>>|<cell|<verbatim|"qt5">,
  <verbatim|"qt6"> (<verbatim|"qt4">)>|<cell|<scm|qt-gui?>>|<cell|<cpp|qt_renderer_rep>>|<cell|25500>>|<row|<cell|<name|Qt6>
  fork>|<cell|<verbatim|"qt6">>|<cell|<scm|qt-gui?>>|<cell|<cpp|qt_renderer_rep>>|<cell|26800>>|<row|<cell|<name|Qtwk>>|<cell|<verbatim|"qt5">,
  <verbatim|"qt6">>|<cell|<scm|x-gui?>>|<cell|<cpp|qt_renderer_rep>>|<cell|3700
  + <name|Widkit>>>|<row|<cell|<name|X11>>|<cell|<verbatim|"x11">>|<cell|<scm|x-gui?>>|<cell|<cpp|x_drawable_rep>>|<cell|4100
  + <name|Widkit>>>|<row|<cell|<name|SDL>>|<cell|<verbatim|"sdl">>|<cell|<scm|x-gui?>>|<cell|<cpp|mupdf_renderer_rep>>|<cell|2500
  + <name|Widkit>>>|<row|<cell|<name|Vue>>|<cell|<verbatim|"vue">>|<cell|<scm|vue-gui?>>|<cell|<cpp|mupdf_renderer_rep>,
  <cpp|gpu_renderer_rep>>|<cell|16600 + <name|Clay>>>|<row|<cell|<name|Cocoa>>|<cell|<verbatim|"ns">>|<cell|<scm|qt-gui?>,
  <scm|ns-gui?>>|<cell|<cpp|ns_renderer_rep>>|<cell|12700>>>>>

  The line counts are those of the <name|C++> and <name|Objective-C> sources
  of the port directories, rounded. <name|Widkit> adds 10500 lines,
  <name|Clay> is a single header library of 5100 lines
  (<source-link|Vue/clay.h|src/Plugins/Vue/clay.h>), and <source-link|Plugins/MacOS|src/Plugins/MacOS> (3600 lines of
  <name|Objective-C> helpers: images, spell checking, the application
  delegate) is compiled into the <name|Qt> ports on <name|macOS> and is
  required by <name|Cocoa>.

  Many <scheme> features test the predicates: the print dialog
  (<scm|use-print-dialog?> in <source-link|kernel/texmacs/tm-preferences.scm|TeXmacs/progs/kernel/texmacs/tm-preferences.scm>)
  and the item \PCopy to Image\Q of the Edit menu need <scm|qt-gui?> or
  <scm|vue-gui?>, <scheme> asks for a confirmation before overwriting a
  file only when <scm|x-gui?> holds (<source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>,
  the other ports have native dialogs which ask), and the typographic
  palettes of the color menus are only offered with <scm|vue-gui?>
  (<source-link|kernel/gui/menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>).

  <paragraph|Widget constructors.><source-link|widget.hpp|src/Graphics/Gui/widget.hpp> declares 49
  constructors (52 counting the overloads of <cpp|choice_widget> and
  <cpp|glue_widget>). Every port defines all of them, so that the kernel
  links, but not all definitions are real widgets:

  <descriptive-table|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-hpart|3>|<cwith|1|-1|2|2|cell-hpart|1>|<cwith|1|-1|3|3|cell-hpart|8>|<table|<row|<cell|Ports>|<cell|Real>|<cell|Stubs
  and substitutes>>|<row|<cell|<name|Qt>, <name|Qt6>
  fork>|<cell|46>|<cell|<cpp|ink_widget>, <cpp|empty_widget> and
  <cpp|wait_widget> return a nil widget>>|<row|<cell|<name|X11>, <name|SDL>,
  <name|Qtwk> (<name|Widkit>)>|<cell|44>|<cell|<cpp|printer_widget> is a
  \PCancel\Q button, <cpp|tree_view_widget> shows \PNot implemented\Q, the
  two responsive tab widgets are plain tabs, <cpp|tooltip_window_widget> is
  a popup window>>|<row|<cell|<name|Cocoa>>|<cell|44>|<cell|<cpp|ink_widget>,
  <cpp|empty_widget> and <cpp|wait_widget> as in <name|Qt>; the two
  responsive tab widgets are plain tabs>>|<row|<cell|<name|Vue>>|<cell|49>|<cell|none
  (the responsive tabs have four presentations, after the preference
  <verbatim|gui:responsive tab mode>)>>>>>

  The three setting widgets (<cpp|setting_toggle_widget>, ...) are composed
  of simpler widgets in <name|Widkit>, <name|Cocoa> and <name|Vue>, which
  counts as real here. The definitions are in
  <source-link|Qt/qt_widget.cpp|src/Plugins/Qt/qt_widget.cpp>,
  <source-link|Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>,
  <source-link|NS/ns_widget.mm|src/Plugins/NS/ns_widget.mm> and
  <source-link|Vue/vue_widget.cpp|src/Plugins/Vue/vue_widget.cpp> (most of them through the
  macro <cpp|VUE_WIDGET>).

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Gui/gui.hpp|src/Graphics/Gui/gui.hpp>, <source-link|widget.hpp|src/Graphics/Gui/widget.hpp>,
    <source-link|message.hpp|src/Graphics/Gui/message.hpp>>The interfaces every port implements.

    <item*|<source-link|misc/m4/tm_gui.m4|misc/m4/tm_gui.m4>>The <verbatim|configure> options
    <verbatim|--with-gui> and <verbatim|--enable-qtpipes> and the definition
    of the port macros; <source-link|misc/m4/qt.m4|misc/m4/qt.m4> (<verbatim|--enable-qt-new>),
    <source-link|misc/m4/sdl3.m4|misc/m4/sdl3.m4>, <source-link|misc/m4/thorvg.m4|misc/m4/thorvg.m4> and
    <source-link|misc/m4/mupdf.m4|misc/m4/mupdf.m4> (<name|MuPDF> per port) complete it.

    <item*|<source-link|CMakeLists.txt|src/CMakeLists.txt> (top level of <source-link|src/|src>)>The
    cache variable <verbatim|TEXMACS_GUI> and the source lists of the ports
    which <name|CMake> builds.

    <item*|<source-link|src/makefile.in|src/makefile.in>>The lists of port directories
    compiled by the <verbatim|make> build.

    <item*|<source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp>><cpp|gui_is_qt>,
    <cpp|gui_is_x> and <cpp|gui_is_vue>.

    <item*|<source-link|Plugins/Qt/qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>, <source-link|QTMWidget.cpp|src/Plugins/Qt/QTMWidget.cpp>,
    <source-link|QTMKeyboardEvent.cpp|src/Plugins/Qt/QTMKeyboardEvent.cpp>, <source-link|qt_printer_widget.cpp|src/Plugins/Qt/qt_printer_widget.cpp>,
    <source-link|QTMPrinterSettings.cpp|src/Plugins/Qt/QTMPrinterSettings.cpp>>Main loop, keyboard and input
    methods, clipboards and printing of the <name|Qt> port (the same file
    names in <source-link|Plugins/Qt6|src/Plugins/Qt6>).

    <item*|<source-link|Plugins/Qtwk/qtwk_gui.cpp|src/Plugins/Qtwk/qtwk_gui.cpp>, <source-link|QTWKWindow.cpp|src/Plugins/Qtwk/QTWKWindow.cpp>,
    <source-link|qtwk_window.cpp|src/Plugins/Qtwk/qtwk_window.cpp>>Main loop, clipboards and windows of
    <name|Qtwk>.

    <item*|<source-link|Plugins/X11/x_loop.cpp|src/Plugins/X11/x_loop.cpp>, <source-link|x_gui.cpp|src/Plugins/X11/x_gui.cpp>,
    <source-link|x_window.cpp|src/Plugins/X11/x_window.cpp>, <source-link|x_init.cpp|src/Plugins/X11/x_init.cpp>>Main loop, keyboard,
    selections and initialization of the <name|X11> port.

    <item*|<source-link|Plugins/SDL/sdl_gui.cpp|src/Plugins/SDL/sdl_gui.cpp>, <source-link|sdl_window.cpp|src/Plugins/SDL/sdl_window.cpp>,
    <source-link|README.md|src/Plugins/SDL/README.md>>Main loop, keyboard, clipboard, windows and
    test scripts of the <name|SDL> port.

    <item*|<source-link|Plugins/Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>>The
    implementation of the abstract widget constructors in terms of
    <name|Widkit>.

    <item*|<source-link|Plugins/Vue/vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>, <source-link|vue_widget.cpp|src/Plugins/Vue/vue_widget.cpp>,
    <source-link|vue_gpu.cpp|src/Plugins/Vue/vue_gpu.cpp>, <source-link|TODO|src/Plugins/Vue/TODO>>Windows, event loop,
    keyboard, clipboard and test driver; the widgets; the GPU renderer;
    the state of the port. The design notes are in
    <source-link|docs/vue-graphics-stack.md|docs/vue-graphics-stack.md> and
    <source-link|docs/vue-testing.md|docs/vue-testing.md>.

    <item*|<source-link|Plugins/NS/ns_gui.mm|src/Plugins/NS/ns_gui.mm>, <source-link|TMView.mm|src/Plugins/NS/TMView.mm>,
    <source-link|ns_widget.mm|src/Plugins/NS/ns_widget.mm>, <source-link|ns_dialogues.mm|src/Plugins/NS/ns_dialogues.mm>>Main
    loop and clipboard, keyboard and input methods, widget constructors and
    dialogs of the <name|Cocoa> port.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Selecting and building a port|guiports-build.en.tm>

    <branch|The <name|Qt> port: versions, keyboard, clipboards and
    printing|guiports-qt.en.tm>

    <branch|The <name|Vue> port|guiports-vue.en.tm>

    <branch|The native <name|Cocoa> port|guiports-cocoa.en.tm>

    <branch|The <name|Widkit> ports: <name|X11>, <name|SDL> and
    <name|Qtwk>|guiports-legacy.en.tm>

    <branch|Pitfalls and known problems|guiports-pitfalls.en.tm>
  </traverse>

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
