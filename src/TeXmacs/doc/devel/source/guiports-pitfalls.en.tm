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
    <name|CMake> build of <name|Vue> has a GPU renderer only when
    <verbatim|THORVG_DIR> names a <name|ThorVG> build.

    <item><with|font-series|bold|<name|Qtwk> passes for <name|Qt> in
    <name|C++>.> <name|Qtwk> defines <cpp|QTTEXMACS> for its platform
    layer, and <cpp|gui_version> returns <verbatim|"qt5"> or
    <verbatim|"qt6">, although its widgets are those of <name|Widkit>:
    code which tests <cpp|QTTEXMACS> also applies to <name|Qtwk> unless it
    excludes <cpp|QTWKTEXMACS>. The run-time predicates do exclude it:
    <cpp|gui_is_qt> is false and <cpp|gui_is_x> true for <name|Qtwk>
    (<source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp>),
    so that <scheme> treats it as <name|X11> (no print dialog, a
    confirmation before overwriting a file).

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

    <item><with|font-series|bold|Headless mode.> <name|Qt> and <name|Vue>
    make no windows in this mode; <name|SDL>, <name|X11> and <name|Cocoa>
    make them but never show them, and <name|X11> still needs a display
    (<verbatim|xvfb-run> on a machine without one). The windows of
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

  <section|The browser>

  The <name|WebAssembly> build of <name|Vue> for the browser is in this
  tree (<source-link|misc/wasm|misc/wasm>), see <hlink|the <name|Vue>
  port|guiports-vue.en.tm>. Its pitfalls are those of a page: no processes
  (plug-ins run as Web Workers), one canvas for all the windows, a loop
  which cannot block, a clipboard which the browser hands over only on a
  paste event or a gesture of the user, and files in a virtual file system
  kept in <name|IndexedDB>; see
  <source-link|docs/wasm/README.md|docs/wasm/README.md>.

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
