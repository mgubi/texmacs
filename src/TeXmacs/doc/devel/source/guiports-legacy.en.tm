<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <name|Widkit> ports: <name|X11>, <name|SDL> and
  <name|Qtwk>>

  <section|<name|Widkit>>

  Three ports draw all their widgets with <source-link|Plugins/Widkit|src/Plugins/Widkit>, a
  complete widget toolkit whose widgets are drawn with the <TeXmacs>
  renderer and communicate by <cpp|event>s. They only differ by the layer
  below, which provides the windows, the events, the clipboards and the
  renderer: <name|X11> (<source-link|Plugins/X11|src/Plugins/X11>), <name|SDL3>
  (<source-link|Plugins/SDL|src/Plugins/SDL>) or <name|Qt> (<source-link|Plugins/Qtwk|src/Plugins/Qtwk>). The design of
  <name|Widkit> is described in <hlink|the graphical user interface
  (historical Widkit toolkit)|gui.en.tm>, and the mapping of the abstract
  constructors to it is in <source-link|Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>.

  The wrapper defines all constructors of the abstract interface, but some
  are placeholders (see <hlink|the overview|guiports.en.tm>):
  <cpp|printer_widget> is a bare \PCancel\Q button, <cpp|tree_view_widget>
  shows \PNot implemented\Q, the responsive tabs are plain tabs and the
  tooltip windows are popup windows. File choosers, color pickers and the
  other dialogs are <name|Widkit> widgets. The main window
  (<source-link|Widkit/Misc/texmacs_widget.cpp|src/Plugins/Widkit/Misc/texmacs_widget.cpp>) has side panels for the tools,
  300 points wide and hidden until tools are shown on that side. After a
  window is shown or resized the whole window must be invalidated, as the
  <verbatim|Expose> events of <name|X11> do: <name|Widkit> relies on them.

  <section|The <name|X11> port>

  The <name|X11> port is the original port of <TeXmacs>. It is still
  updated when the abstract interface changes, but lacks the features which
  the <scheme> code only offers when <scm|qt-gui?> or <scm|vue-gui?> holds.

  <paragraph|The event loop.><cpp|x_gui_rep::event_loop>
  (<source-link|X11/x_loop.cpp|src/Plugins/X11/x_loop.cpp>) is a polling loop which runs while there are
  windows or remote clients. In each iteration it processes at most one
  pending <name|X> event (after <cpp|XFilterEvent>, for input methods);
  when no event arrived, it sleeps with <cpp|select> for a delay which
  starts at 10<nbsp>ms and grows to 1<nbsp>s after two minutes of
  inactivity (<verbatim|MIN_DELAY>, <verbatim|MAX_DELAY>,
  <verbatim|SLEEP_AFTER>). It then calls the interpose handler, shows
  pending help balloons, and redraws invalid windows when no events are
  pending, with an interruption deadline so that typing remains
  responsive. Repaints are skipped while the window is being resized or
  exposed.

  <paragraph|Keyboard and input methods.>When an input method could be
  opened at startup, each window gets an input context created with the
  style <verbatim|XIMPreeditNothing \| XIMStatusNothing>
  (<source-link|x_window.cpp|src/Plugins/X11/x_window.cpp>), that is, without on-the-spot or over-the-spot
  preedit. Key presses are decoded with <cpp|Xutf8LookupString>
  (<source-link|x_loop.cpp|src/Plugins/X11/x_loop.cpp>); a decoded Unicode character is converted to Cork
  and used directly, and otherwise the key symbol is looked up in the
  tables <cpp|lower_key> and <cpp|upper_key> built in
  <source-link|x_init.cpp|src/Plugins/X11/x_init.cpp>. Without an input method, <cpp|XLookupString> is
  used.

  <paragraph|Selections.>The clipboard <verbatim|"primary"> is the
  <name|X> selection <verbatim|CLIPBOARD> and <verbatim|"mouse"> is
  <verbatim|PRIMARY>. For the format <verbatim|default>, the global
  <cpp|set_selection> (<source-link|x_gui.cpp|src/Plugins/X11/x_gui.cpp>) publishes the verbatim version
  <cpp|sv> of the selection, so that other programs receive plain text;
  since the port offers a single string, another <TeXmacs> instance
  receives that plain text too, and a copy between two instances loses its
  structure. When <TeXmacs> owns a selection, it answers requests for
  <verbatim|TARGETS> and <verbatim|STRING> only (<verbatim|SelectionRequest>
  in <source-link|x_loop.cpp|src/Plugins/X11/x_loop.cpp>). To paste from another program,
  <cpp|x_gui_rep::get_selection> requests <verbatim|STRING> and polls for
  the <verbatim|SelectionNotify> event, giving up after a fixed number of
  polls.

  <paragraph|Printing.>Since <scm|use-print-dialog?> is false,
  printing always goes through the printing command.

  <section|The <name|SDL> port>

  <source-link|Plugins/SDL|src/Plugins/SDL> is an experimental port which uses <name|SDL3> for
  the windows and the events, <name|Widkit> for the widgets, and the
  <name|MuPDF> renderer for the drawing (<verbatim|configure
  --with-gui=sdl --with-mupdf=... --with-sdl3>, or <name|CMake>
  <verbatim|TEXMACS_GUI=SDL>). Its <source-link|README.md|src/Plugins/SDL/README.md> describes it.

  <paragraph|Windows and drawing.>Each window has a backing store, an
  opaque <name|MuPDF> pixmap of the size of the window in device pixels, in
  which the widgets draw with <cpp|mupdf_renderer_rep>. A repaint records
  the rectangles it changed, and only these are copied to the surface of
  the window (<cpp|SDL_ConvertPixels>, then
  <cpp|SDL_UpdateWindowSurfaceRects>); a scroll shifts the pixels of the
  backing store in place. There is no <cpp|SDL_Renderer>. The renderers
  draw at <cpp|retina_factor> pixels per point, the pixel density of the
  primary display (<verbatim|TEXMACS_SDL_DENSITY> overrides it).

  <paragraph|The event loop.><cpp|sdl_gui_rep::event_loop>
  (<source-link|sdl_gui.cpp|src/Plugins/SDL/sdl_gui.cpp>) runs while there are windows or servers. It sleeps
  in <cpp|SDL_WaitEventTimeout>, handles the waiting events in a burst (a
  mouse motion superseded by the next one is dropped), lets the editors
  apply their changes, and repaints when the queue is empty, or at least
  every 50<nbsp>ms. While a window is resized by its border, an event watch
  lays it out and repaints it from inside the event pump of <name|SDL>, but
  only while the loop is waiting.

  <paragraph|Keyboard and input methods.>Each window calls
  <cpp|SDL_StartTextInput> (<source-link|sdl_window.cpp|src/Plugins/SDL/sdl_window.cpp>). A keystroke which
  types text is delivered by its text event, which carries what the input
  method or a dead key composed; the others are delivered as keys
  (<verbatim|C-x>, <verbatim|M-s>, <verbatim|return>, ...). The composition
  of an input method is sent as <verbatim|pre-edit:<em|cursor>:<em|text>>,
  as in <name|Qt>.

  <paragraph|Clipboards.>The system clipboard holds the selection
  <verbatim|"primary">, under the types
  <verbatim|application/x-texmacs-clipboard>, <verbatim|text/html> (when
  there is an <name|HTML> version) and plain text, the latter being
  <cpp|sv> for the format <verbatim|default>. The other selections are
  kept internally; <name|SDL> has no <name|X11> <verbatim|PRIMARY>
  selection.

  <paragraph|Testing.><verbatim|TEXMACS_SDL_SCRIPT=<em|file>> replays the
  commands of the file (<verbatim|wait>, <verbatim|window>,
  <verbatim|click>, <verbatim|key>, <verbatim|text>, <verbatim|snapshot>,
  ..., see the comment in <source-link|sdl_gui.cpp|src/Plugins/SDL/sdl_gui.cpp>); <verbatim|snapshot
  <em|name>> saves the backing store of the target window in the directory
  <verbatim|TEXMACS_SDL_SNAPSHOT>.

  <paragraph|Limits.>No file dialogs of the system and no drag and drop,
  no custom cursors (except the invisible one), and the positions of
  popups may be off on a display whose density differs from
  <cpp|retina_factor>. Printing goes through the printing command, as in
  <name|X11>.

  <section|The <name|Qtwk> port>

  <source-link|Plugins/Qtwk|src/Plugins/Qtwk> (<verbatim|configure --with-gui=qtwk>, macros
  <cpp|QTWKTEXMACS> and <cpp|QTTEXMACS>) uses <name|Qt> as a platform layer
  under <name|Widkit>: a <cpp|QTWKApplication> (a <cpp|QApplication>, or a
  <cpp|QTWKCoreApplication> in headless mode), one <cpp|QTWKWindow> (a
  <cpp|QWidget>) per <TeXmacs> window, painted with the <name|Qt> renderer
  <cpp|qt_renderer_rep> of <verbatim|Plugins/Qt>, and fonts, pictures,
  pipes, sockets and <abbr|HTTP> taken from <verbatim|Plugins/Qt> (see
  <hlink|selecting and building a port|guiports-build.en.tm>).

  <paragraph|The event loop.>As in the <name|Qt> port, <name|Qt> runs the
  loop (<verbatim|qApp-\<gtr\>exec ()> in <source-link|qtwk_gui.cpp|src/Plugins/Qtwk/qtwk_gui.cpp>), and the
  events of the windows (key presses, mouse, resizes, socket
  notifications, commands) are queued as <cpp|qp_type> events and handled
  by the update cycle of <cpp|qtwk_gui_rep>.

  <paragraph|Keyboard and input methods.><cpp|QTWKWindow::keyPressEvent>
  and <cpp|QTWKWindow::inputMethodEvent> (<source-link|QTWKWindow.cpp|src/Plugins/Qtwk/QTWKWindow.cpp>) follow
  the <name|Qt> port; committed text is replayed as one synthetic key
  press per <cpp|QChar>.

  <paragraph|Clipboards.><cpp|qtwk_gui_rep::set_selection> and
  <cpp|get_selection> are those of the <name|Qt> port: the same
  <cpp|QMimeData> with <verbatim|application/x-texmacs-clipboard> and
  <verbatim|application/x-texmacs-pid>, <verbatim|"primary"> on the system
  clipboard and <verbatim|"mouse"> on the <name|X11> selection.

  <paragraph|Scheme.>Since <cpp|QTTEXMACS> is defined, <scm|qt-gui?> holds
  and <scm|gui-version> is <verbatim|"qt5"> or <verbatim|"qt6">: the
  <scheme> code offers the <name|Qt> features, which the <name|Widkit>
  widgets do not all have (see <hlink|pitfalls|guiports-pitfalls.en.tm>).

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
