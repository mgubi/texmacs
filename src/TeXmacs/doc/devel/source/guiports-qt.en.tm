<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <name|Qt> port: versions, keyboard, clipboards and
  printing>

  The structure of the <name|Qt> port (widgets, UI elements, menus, the
  event queue, windows, canvases) is described in <hlink|the <name|Qt>
  implementation|widgets-qt.en.tm>, and its renderer in <hlink|the renderer
  implementations|renderer-backends.en.tm>. This page adds what is specific
  to the <name|Qt> versions and to the input and output channels of the
  port.

  <section|<name|Qt> 4, 5 and 6>

  <verbatim|Plugins/Qt> compiles with <name|Qt> 4, 5 and 6. The differences
  are handled by about 270 tests of <cpp|QT_VERSION> in the port (most of
  them <verbatim|QT_VERSION \<gtr\>= 0x060000>, <verbatim|\<less\>
  0x060000> and <verbatim|\<gtr\>= 0x050000>) and by a few outside it, for
  instance in <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>,
  <source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp>,
  <source-link|Edit/Interface/edit_interface.cpp|src/Edit/Interface/edit_interface.cpp> and
  <source-link|System/Files/web_files.cpp|src/System/Files/web_files.cpp>. Notable differences are:

  <\itemize>
    <item>High resolution screens. Below <name|Qt> 6, <TeXmacs> manages the
    scaling itself through the variables <cpp|retina_factor>,
    <cpp|retina_zoom>, <cpp|retina_icons> and <cpp|retina_scale>, set by
    the options <verbatim|-retina> and <verbatim|-no-retina> and the
    environment variables <verbatim|TEXMACS_RETINA> and
    <verbatim|TEXMACS_RETINA_ICONS> (<source-link|texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>). With
    <name|Qt> 6 these options do not exist; the device pixel ratio of
    <name|Qt> is used instead (<cpp|QTMWidget::checkDprChange>) and the
    rounding policy is set to <verbatim|Round> at startup.

    <item>HTTP requests. <source-link|qt_http.cpp|src/Plugins/Qt/qt_http.cpp> is only compiled in
    with <name|Qt> 6 (<verbatim|#if QT_VERSION \<gtr\>= 0x060000>);
    otherwise <cpp|http_post> falls back to external programs, see
    <hlink|system utilities|system-utils.en.tm>.

    <item>The native menu bar. Its default depends on the version and the
    platform (<verbatim|use native menubar> in <source-link|texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>); on
    <name|macOS> before <name|Qt> 6 it is only used when the preference is
    <verbatim|force>.

    <item>Renamed <name|Qt> <abbr|API>s, such as <verbatim|Qt::MidButton>
    versus <verbatim|Qt::MiddleButton> or
    <verbatim|QString::SkipEmptyParts> versus
    <verbatim|Qt::SkipEmptyParts>.
  </itemize>

  <cpp|gui_version ()> returns <verbatim|"qt4">, <verbatim|"qt5"> or
  <verbatim|"qt6"> accordingly (<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>).

  <section|The <source-link|Plugins/Qt6|src/Plugins/Qt6> fork>

  <source-link|Plugins/Qt6|src/Plugins/Qt6> was created on 2026-05-27 as a copy of
  <verbatim|Plugins/Qt> (commit <verbatim|73253e50f1>, \Pduplicating qt
  folder\Q) and is compiled instead of it by <verbatim|configure
  --enable-qt-new>, which requires <name|Qt> 6.10 (see <hlink|selecting
  and building a port|guiports-build.en.tm>). Compared with
  <verbatim|Plugins/Qt> it

  <\itemize>
    <item>adds <verbatim|QTMMainTab> and <verbatim|QTMMainTabWindow>, main
    windows with tabs which can be moved from one window to another;

    <item>changes mostly <source-link|qt_http.cpp|src/Plugins/Qt/qt_http.cpp>,
    <source-link|qt_utilities.cpp|src/Plugins/Qt/qt_utilities.cpp>, <source-link|qt_tm_widget.cpp|src/Plugins/Qt/qt_tm_widget.cpp>,
    <source-link|QTMToolbar.cpp|src/Plugins/Qt/QTMToolbar.cpp> and <source-link|QTMResponsiveTabWidget.cpp|src/Plugins/Qt/QTMResponsiveTabWidget.cpp>
    (31 files differ in total), with work on responsive layouts for small
    screens and on <name|Android> according to the commit messages.
  </itemize>

  The two directories are kept in sync by hand: changes made in
  <verbatim|Plugins/Qt> are copied over in commits such as
  <verbatim|bb309fcd50> (\Ppropagating change from Qt dir to Qt6 dir\Q). A
  fix in one directory therefore has to be made in the other one too.

  <section|Keyboard and input methods>

  Key presses arrive in <cpp|QTMWidget::keyPressEvent>
  (<source-link|QTMWidget.cpp|src/Plugins/Qt/QTMWidget.cpp>). A <cpp|QTMKeyboardEvent> translates the
  <name|Qt> key code, modifiers and text into a <TeXmacs> key combination
  such as <verbatim|"C-x"> or <verbatim|"A-S-left">, using the global
  keyboard settings of <cpp|QTMKeyboard> (<source-link|QTMKeyboard.hpp|src/Plugins/Qt/QTMKeyboard.hpp>, held
  by the application object). An empty combination means that the key is
  ignored. Otherwise <cpp|qt_gui_rep::process_keypress> queues a
  <verbatim|QP_KEYPRESS> event, which the event loop later delivers to the
  editor; from there on the key is handled by the keyboard configuration
  of the server, see <hlink|keyboard configuration|server-events.en.tm>.

  Input methods (accents, Chinese and Japanese input, the macOS
  character palette, dictation) go through
  <cpp|QTMWidget::inputMethodEvent>:

  <\itemize>
    <item>Committed text is replayed character by character as synthetic
    key presses (<cpp|kbdEvent>). When the <verbatim|speech> preference is
    on and no preedit is in progress, it is sent instead as one key
    <verbatim|"speech:<em|text>">, which <cpp|handle_speech> in
    <source-link|Edit/Interface/edit_keyboard.cpp|src/Edit/Interface/edit_keyboard.cpp> interprets.

    <item>Preedit text is sent as the key
    <verbatim|"pre-edit:<em|pos>:<em|text>">, where <em|pos> is the cursor
    position inside the preedit string (an empty string ends the preedit).
    <scheme> handles these keys with <scm|delayed-keyboard-press>.

    <item><cpp|QTMWidget::inputMethodQuery> tells the input method where
    the cursor is, so that candidate windows are placed next to it.
  </itemize>

  For <name|Qt> 4 on <name|macOS>, a table in <cpp|inputMethodEvent>
  converts the committed characters of a few Option key combinations back
  into key presses; this hack only works for standard US keyboards.

  <section|Clipboards>

  The <TeXmacs> clipboards are named; the <name|Qt> port maps
  <verbatim|"primary"> to the system clipboard
  (<verbatim|QClipboard::Clipboard>) and <verbatim|"mouse"> to the
  selection clipboard of <name|X11> (<verbatim|QClipboard::Selection>) when
  the platform supports it. All other names are purely internal: they are
  stored in the hash tables <cpp|selection_t> and <cpp|selection_s> of
  <cpp|qt_gui_rep> and never reach the system.

  <paragraph|Copying.><cpp|qt_gui_rep::set_selection (key, t, s, sv, sh,
  format)> stores the tree and its serialization locally and then
  publishes a <cpp|QMimeData>. For the format <verbatim|default> it
  contains the <TeXmacs> snippet under the private type
  <verbatim|application/x-texmacs-clipboard>, the process id under
  <verbatim|application/x-texmacs-pid>, and the verbatim version
  <cpp|sv> as plain text (in the encoding of the preference
  <verbatim|texmacs-\<gtr\>verbatim:encoding>). For the formats
  <verbatim|html> and <verbatim|latex> the converted text is published as
  <name|HTML> or plain text. The caller, <cpp|edit_select_rep::selection_set>
  (<source-link|Edit/Replace/edit_select.cpp|src/Edit/Replace/edit_select.cpp>), computes <cpp|sv> only in the
  <name|Qt> port.

  <paragraph|Pasting.><cpp|qt_gui_rep::get_selection> first decides
  whether <TeXmacs> owns the clipboard, by comparing the
  <verbatim|application/x-texmacs-pid> entry with its own process id; if
  so, the local copy is returned unchanged. Otherwise, for the format
  <verbatim|default>, it picks the richest available representation, in
  this order: a <TeXmacs> snippet of another instance, an image (a single
  <abbr|URL> becomes a linked image, other image data is converted to
  <abbr|PNG> and embedded), <name|HTML> (after repairing known bugs of
  some browsers with <cpp|correct_buggy_html_paste>), and plain text. The
  result is converted to a <TeXmacs> snippet by the <scheme> function
  <scm|convert> and returned as <verbatim|(extern <em|snippet>)>. The
  pseudo clipboard <verbatim|"extern"> reads the system clipboard without
  this conversion.

  <section|Printing>

  <scm|print-buffer> (<source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>) uses a
  dialog only if <scm|use-print-dialog?> holds, that is, in the <name|Qt>
  port with the preference <verbatim|gui:print dialogue> set to
  <verbatim|on>. Otherwise it calls <scm|print>, which typesets the
  document to PostScript or <abbr|PDF> and sends the file to the printing
  command (<scm|set-printing-command>).

  With the dialog, <scm|interactive-print-buffer> first prints to the file
  <verbatim|$TEXMACS_HOME_PATH/system/tmp/tmpprint.<em|suffix>> and then
  opens <scm|widget-printer> in an alternative window
  (<scm|interactive-print> in <source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>). In
  <name|Qt> this widget is a <cpp|qt_printer_widget_rep>
  (<source-link|qt_printer_widget.cpp|src/Plugins/Qt/qt_printer_widget.cpp>) showing a <cpp|QTMPrintDialog>. As the
  comment of that file says, all options are applied as a postprocessing of
  the already typeset file: <cpp|QTMPrinterSettings::toSystemCommand> turns
  them into a command line which is run with <cpp|qt_system>. On
  <name|macOS> and <name|Linux> the settings class is
  <cpp|CupsQTMPrinterSettings>, which queries the printers with
  <verbatim|lpoptions> (asynchronously, since it may use the network) and
  prints with the printing command of <TeXmacs> or <verbatim|lp> and
  <name|CUPS> options (<verbatim|-o orientation-requested>, <verbatim|-o
  sides>, <verbatim|-o number-up>, <verbatim|-o page-ranges>, ...); on
  <name|Windows> it is <cpp|WinQTMPrinterSettings>.

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
