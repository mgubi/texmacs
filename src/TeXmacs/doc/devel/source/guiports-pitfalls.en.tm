<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Pitfalls and known problems>

  <section|Pitfalls and known problems>

  <\itemize>
    <item><with|font-series|bold|Two copies of the <name|Qt> port.>
    <verbatim|Plugins/Qt> and <source-link|Plugins/Qt6|src/Plugins/Qt6> are maintained in
    parallel and synchronized by hand. A fix applied to one directory must
    be applied to the other; <name|CMake> and <name|Qtwk> use only the
    first, and <verbatim|configure --enable-qt-new> (the default on
    <name|Android>) only the second.

    <item><with|font-series|bold|What <name|CMake> builds.>
    <verbatim|TEXMACS_GUI> accepts <verbatim|Qt>, <verbatim|Qt6>,
    <verbatim|Qt5>, <verbatim|Qt4>, <verbatim|Vue>, <verbatim|SDL> and
    <verbatim|X11> (<source-link|CMakeLists.txt|src/CMakeLists.txt>, section \PGUI & Qt
    Selection\Q). The <source-link|Plugins/Qt6|src/Plugins/Qt6> fork, <name|Qtwk> and
    <name|Cocoa> can only be built with <verbatim|configure>, and a
    <name|CMake> build of <name|Vue> has no GPU renderer, since there is no
    <name|ThorVG> option.

    <item><with|font-series|bold|<name|Qtwk> passes for <name|Qt>.>
    <name|Qtwk> defines <cpp|QTTEXMACS>, so <scm|qt-gui?> holds and
    <scm|gui-version> is <verbatim|"qt5"> or <verbatim|"qt6">, although its
    widgets are those of <name|Widkit>. With the preference <verbatim|gui:print
    dialogue> on, <scm|use-print-dialog?> therefore holds and printing
    opens the <name|Widkit> <cpp|printer_widget>, a bare \PCancel\Q button.
    The same holds in <name|C++>: code which tests <cpp|QTTEXMACS> also
    applies to <name|Qtwk> unless it excludes <cpp|QTWKTEXMACS>.

    <item><with|font-series|bold|Fixes of the <name|Qt> port not made in
    <name|Qtwk>.> <cpp|qtwk_gui_rep::get_selection>
    (<source-link|qtwk_gui.cpp|src/Plugins/Qtwk/qtwk_gui.cpp>) dereferences the result of
    <cpp|QClipboard::mimeData> without checking it, which
    <cpp|qt_gui_rep::get_selection> now does, and
    <cpp|QTWKWindow::inputMethodEvent> (<source-link|QTWKWindow.cpp|src/Plugins/Qtwk/QTWKWindow.cpp>) replays
    the committed text one <cpp|QChar> at a time, so that a character
    outside the basic multilingual plane arrives as two surrogate halves,
    whereas <source-link|QTMWidget.cpp|src/Plugins/Qt/QTMWidget.cpp> keeps the pairs together (from
    reading the code; not tested).

    <item><with|font-series|bold|<name|Cocoa> is both <scm|qt-gui?> and
    <scm|x-gui?>.> <cpp|gui_is_qt> is true for <cpp|AQUATEXMACS>, and
    <cpp|gui_is_x> for every port which is neither <name|Qt> nor <name|Vue>
    (<source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp>). With <name|Cocoa>, the
    <scheme> code therefore asks itself before overwriting a file, as for
    <name|X11> (<source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>), and the Git
    interface does not use <scm|evaluate-system> (<scm|spawn-supported?>
    in <source-link|version/git-base.scm|TeXmacs/progs/version/git-base.scm>), as with <name|X11> and
    <name|SDL> (from reading the code).

    <item><with|font-series|bold|Features reserved to some ports.> Code
    which tests <cpp|QTTEXMACS> often has the <name|Widkit> code in its
    <verbatim|#else> branch, and <scheme> code offers many features only
    when <scm|qt-gui?> holds (13 files of
    <source-link|TeXmacs/progs|TeXmacs/progs>), some of them also with <scm|vue-gui?>. A
    new port has to review both, see <hlink|porting <TeXmacs> to another
    toolkit|widgets-port.en.tm>. The color menus, for instance, open the
    native color picker only with <scm|qt-gui?>, so <name|Vue> uses the
    <scheme> picker.

    <item><with|font-series|bold|Placeholder widgets.> All ports define the
    49 constructors of <source-link|widget.hpp|src/Graphics/Gui/widget.hpp>, but some definitions return
    a nil widget or a substitute (<hlink|the overview|guiports.en.tm>): a
    <cpp|tree_view_widget> shows \PNot implemented\Q in the <name|Widkit>
    ports, and <cpp|ink_widget>, <cpp|empty_widget> and <cpp|wait_widget>
    are nil in <name|Qt> and <name|Cocoa>.

    <item><with|font-series|bold|Headless mode.> Only <name|Qt> and
    <name|Vue> implement it; <name|X11>, <name|SDL> and <name|Cocoa> connect
    to the display even with <verbatim|-headless>, and the windows of
    <name|Qtwk> do not test <cpp|is_headless>.

    <item><with|font-series|bold|<name|X11> selections are
    <name|Latin-1>.> The <name|X11> port only offers and requests the
    target <verbatim|STRING> (<source-link|x_loop.cpp|src/Plugins/X11/x_loop.cpp>,
    <source-link|x_gui.cpp|src/Plugins/X11/x_gui.cpp>), never <verbatim|UTF8_STRING>, so non
    <name|Latin-1> text is not exchanged correctly with other programs, and
    since it offers a single string (plain text), a copy between two
    <TeXmacs> instances loses its structure. Pasting also busy-polls up to
    a million times for the answer of the selection owner
    (<source-link|x_gui.cpp|src/Plugins/X11/x_gui.cpp>), using the processor meanwhile and failing with
    slow owners.

    <item><with|font-series|bold|<name|SDL> and <name|Vue> on
    <name|Linux>.> <name|SDL> has only the system clipboard, so the
    <name|X11> <verbatim|PRIMARY> selection (<verbatim|"mouse">) is
    internal to <TeXmacs> in these two ports.
  </itemize>

  <section|Ports on other branches>

  The <name|WebAssembly> build of <name|Vue> for the browser is developed
  on the branch <verbatim|wip_wasm_vue>: this tree only contains the parts
  of the port compiled under <cpp|__EMSCRIPTEN__>, see <hlink|the
  <name|Vue> port|guiports-vue.en.tm>.

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
