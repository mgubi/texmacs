<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The graphical user interface ports>

  <section|Introduction>

  <TeXmacs> does not talk to a particular toolkit. The kernel only knows the
  abstract interfaces of <verbatim|Graphics/Gui/gui.hpp> (the application:
  main loop, clipboards, fonts, screen size, ...), of
  <verbatim|Graphics/Gui/widget.hpp> (the widget constructors and the
  message protocol) and of <verbatim|Graphics/Renderer/renderer.hpp> (the
  drawing surface). A <em|port> is a directory in
  <verbatim|src/src/Plugins/> which implements these interfaces for one
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
  support|system-platforms.en.tm>.

  All file names are relative to <verbatim|src/src/> unless stated
  otherwise.

  <section|Overview>

  <descriptive-table|<tformat|<table|<row|<cell|Port>|<cell|Macro>|<cell|Built
  by>|<cell|Status>>|<row|<cell|<verbatim|Plugins/Qt> (<name|Qt> 4, 5,
  6)>|<cell|<cpp|QTTEXMACS>>|<cell|<name|CMake>,
  <verbatim|configure>>|<cell|reference port>>|<row|<cell|<verbatim|Plugins/Qt6>>|<cell|<cpp|QTTEXMACS>>|<cell|<verbatim|configure
  --enable-qt-new>>|<cell|experimental fork>>|<row|<cell|<verbatim|Plugins/X11>,
  <verbatim|Widkit>>|<cell|<cpp|X11TEXMACS>>|<cell|<verbatim|configure
  --disable-qt>>|<cell|historical>>|<row|<cell|<verbatim|Plugins/Cocoa>>|<cell|<cpp|AQUATEXMACS>>|<cell|<verbatim|configure
  --enable-cocoa>>|<cell|experimental>>|<row|<cell|headless>|<cell|<cpp|QTTEXMACS>>|<cell|option
  <verbatim|-headless>>|<cell|batch use>>>>>

  <verbatim|Plugins/Qt6> needs <name|Qt> 6.10 and is the default of
  <verbatim|configure> on <name|Android>; <name|CMake> cannot build it. The
  <name|X11> port is kept up with changes of the abstract interface but is
  otherwise historical, and the <name|Cocoa> port (also called <name|Aqua>)
  is an experimental native <name|macOS> port which is only adapted to
  interface changes. Headless mode is a run-time mode of the <name|Qt>
  port.

  The ports differ a lot in size: about 25000 lines for <name|Qt>, 4100
  lines for <name|X11> plus 10400 lines for <name|Widkit>, and 5400 lines
  for <name|Cocoa>. Only the <name|Qt> port implements all 49 widget
  constructors of <verbatim|widget.hpp>; <name|Widkit> misses the five most
  recent ones (<cpp|responsive_tabs_widget>,
  <cpp|responsive_icon_tabs_widget>, <cpp|setting_toggle_widget>,
  <cpp|setting_enum_widget>, <cpp|setting_group_widget>) and <name|Cocoa>
  misses the same five plus <cpp|tooltip_window_widget>.

  At run time, the port can be recognized with <cpp|gui_version ()>
  (<verbatim|"qt4">, <verbatim|"qt5">, <verbatim|"qt6"> or
  <verbatim|"x11">; exported as <scm|gui-version>) and with <cpp|gui_is_qt
  ()> or the <scheme> predicate <scm|qt-gui?>. Many features of the user
  interface are only offered when <scm|qt-gui?> holds, for instance the
  print dialog (<scm|use-print-dialog?> in
  <verbatim|kernel/texmacs/tm-preferences.scm>).

  <section|Source files>

  <\description-paragraphs>
    <item*|<verbatim|Graphics/Gui/gui.hpp>, <verbatim|widget.hpp>,
    <verbatim|message.hpp>>The interfaces every port implements.

    <item*|<verbatim|CMakeLists.txt> (top level of <verbatim|src/>)>The
    cache variable <verbatim|TEXMACS_GUI> and the selection of <name|Qt> 4,
    5 or 6.

    <item*|<verbatim|misc/m4/tm_gui.m4>>The <verbatim|configure> options
    <verbatim|--disable-qt>, <verbatim|--enable-qtpipes> and
    <verbatim|--enable-cocoa> and the definition of the port macros.

    <item*|<verbatim|src/makefile.in>>The lists of port directories
    compiled by the <verbatim|make> build.

    <item*|<verbatim|Plugins/Qt/qt_gui.cpp>, <verbatim|QTMWidget.cpp>,
    <verbatim|QTMKeyboardEvent.cpp>, <verbatim|qt_printer_widget.cpp>,
    <verbatim|QTMPrinterSettings.cpp>>Main loop, keyboard and input
    methods, clipboards and printing of the <name|Qt> port.

    <item*|<verbatim|Plugins/X11/x_loop.cpp>, <verbatim|x_gui.cpp>,
    <verbatim|x_window.cpp>, <verbatim|x_init.cpp>>Main loop, keyboard,
    selections and initialization of the <name|X11> port.

    <item*|<verbatim|Plugins/Widkit/Basic/widkit_wrapper.cpp>>The
    implementation of the abstract widget constructors in terms of
    <name|Widkit>.

    <item*|<verbatim|Plugins/Cocoa/aqua_gui.mm>, <verbatim|TMView.mm>,
    <verbatim|aqua_dialogues.mm>>Main loop, keyboard and dialogs of the
    <name|Cocoa> port.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Selecting and building a port|guiports-build.en.tm>

    <branch|The <name|Qt> port: versions, keyboard, clipboards and
    printing|guiports-qt.en.tm>

    <branch|The <name|X11>/<name|Widkit> and <name|Cocoa>
    ports|guiports-legacy.en.tm>

    <branch|Other ports and pitfalls|guiports-pitfalls.en.tm>
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
