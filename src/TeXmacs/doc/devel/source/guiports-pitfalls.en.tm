<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Other ports and pitfalls>

  <section|Ports on other branches>

  Further ports are developed outside the branch described here. In the
  repository, the branches <verbatim|wip_other_guis> and
  <verbatim|wip_wasm_vue> contain the plug-in directories
  <verbatim|Plugins/NS>, <verbatim|Plugins/Qtwk>, <verbatim|Plugins/SDL>,
  <verbatim|Plugins/Vue> and <verbatim|Plugins/MuPDF>, which do not exist
  in this tree, and the remote branches <verbatim|ns_ci> and
  <verbatim|vue_ci> exist as well. These ports are documented on their own
  branches; nothing in this chapter applies to them.

  <section|Pitfalls and known problems>

  <\itemize>
    <item><with|font-series|bold|Two copies of the <name|Qt> port.>
    <verbatim|Plugins/Qt> and <source-link|Plugins/Qt6|src/Plugins/Qt6> are maintained in
    parallel and synchronized by hand. A fix applied to one directory must
    be applied to the other; <name|CMake> builds only the first, and
    <verbatim|configure --enable-qt-new> (the default on <name|Android>)
    only the second.

    <item><with|font-series|bold|<name|CMake> ignores the port
    choice.> <verbatim|TEXMACS_GUI> accepts <verbatim|Aqua> and
    <verbatim|X11> but always compiles <verbatim|Plugins/Qt>, without
    defining any port macro for those values (<source-link|CMakeLists.txt|src/CMakeLists.txt>,
    section \PGUI & Qt Selection\Q).

    <item><with|font-series|bold|Stale generated files.> An in-tree
    <verbatim|make> build leaves <verbatim|moc_*.cpp> files in
    <verbatim|Plugins/Qt> (they are ignored by <source-link|src/.gitignore|.gitignore>).
    The <name|CMake> source list is a glob on
    <verbatim|Plugins/Qt/*.cpp> while <name|CMake> also runs its own
    <verbatim|AUTOMOC> (<source-link|src/CMakeLists.txt|src/CMakeLists.txt>), so a <name|CMake>
    build in the same tree compiles both sets of meta object files.

    <item><with|font-series|bold|The <name|Cocoa> port does not link.>
    <cpp|gui_version> is declared in <source-link|gui.hpp|src/Graphics/Gui/gui.hpp> and called
    unconditionally (<source-link|Texmacs/Texmacs/texmacs.cpp:537|src/Texmacs/Texmacs/texmacs.cpp:537>, the glue
    of <scm|gui-version>), but <source-link|Plugins/Cocoa|src/Plugins/Cocoa> does not define it.

    <item><with|font-series|bold|Print dialog options.> The
    <em|black and white> check box of the <name|Qt> print dialog sets
    <cpp|QTMPrinterSettings::blackWhite>, but
    <cpp|CupsQTMPrinterSettings::toSystemCommand> never turns it into a
    printing option. The unused helpers <cpp|getFromQPrinter> and
    <cpp|setToQPrinter> invert its meaning
    (<source-link|QTMPrinterSettings.cpp:71|src/Plugins/Qt/QTMPrinterSettings.cpp:71> and <verbatim|98>). The page
    range for several pages per sheet is computed with integer division,
    so the <cpp|ceil> around <verbatim|lastPage / pagesPerSide> has no
    effect and the last sheet may be left out
    (<verbatim|QTMPrinterSettings.cpp:387-388>).

    <item><with|font-series|bold|Pasting with an empty clipboard.>
    <cpp|qt_gui_rep::get_selection> dereferences the result of
    <cpp|QClipboard::mimeData> without checking it, unlike
    <cpp|clear_selection>; <name|Qt> may return a null pointer. The same
    function looks for the misspelled type <verbatim|"plain/text"> and
    therefore always falls back to <cpp|md-\<gtr\>text ()> in that branch.

    <item><with|font-series|bold|Input methods and characters outside the
    BMP.> <cpp|QTMWidget::inputMethodEvent> replays the committed text one
    <cpp|QChar> (UTF-16 unit) at a time, so a character outside the basic
    multilingual plane is delivered as two separate surrogate halves (from
    reading the code; not tested).

    <item><with|font-series|bold|<name|X11> selections are
    <name|Latin-1>.> The <name|X11> port only offers and requests the
    target <verbatim|STRING> (<source-link|x_loop.cpp|src/Plugins/X11/x_loop.cpp>,
    <source-link|x_gui.cpp|src/Plugins/X11/x_gui.cpp>), never <verbatim|UTF8_STRING>, so non
    <name|Latin-1> text is not exchanged correctly with other programs.
    Pasting also busy-polls up to a million times for the answer of the
    selection owner (<source-link|x_gui.cpp|src/Plugins/X11/x_gui.cpp>), using the processor meanwhile
    and failing with slow owners.

    <item><with|font-series|bold|Markup on the system clipboard.> Outside
    <name|Qt>, an ordinary copy puts the <TeXmacs> snippet rather than plain
    text on the system clipboard, see <hlink|common
    limitations|guiports-legacy.en.tm>.

    <item><with|font-series|bold|Port tests outside the plug-ins.> Code
    which tests <cpp|QTTEXMACS> usually falls back to <name|X11> behaviour
    in its <verbatim|#else> branch, and <scheme> code often offers features
    only when <scm|qt-gui?> holds. A new port has to review both, see
    <hlink|porting <TeXmacs> to another toolkit|widgets-port.en.tm>.
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
