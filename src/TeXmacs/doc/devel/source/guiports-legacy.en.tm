<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <name|X11>/<name|Widkit> and <name|Cocoa> ports>

  <section|The <name|X11> port>

  The <name|X11> port is the original port of <TeXmacs>. It consists of two
  parts: <source-link|Plugins/X11|src/Plugins/X11> talks to the <name|X> server (display,
  windows, events, fonts, pictures, selections), and
  <source-link|Plugins/Widkit|src/Plugins/Widkit> is a complete widget toolkit whose widgets are
  drawn with the <TeXmacs> renderer and communicate by <cpp|event>s. The
  design of <name|Widkit> is described in <hlink|the graphical user
  interface (historical Widkit toolkit)|gui.en.tm>, and the mapping of the
  abstract constructors to it is in
  <source-link|Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>. The port is still updated
  when the abstract interface changes, but lacks the most recent
  constructors (responsive tabs and setting widgets) and all features which
  the <scheme> code only offers when <scm|qt-gui?> holds.

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
  <verbatim|PRIMARY>. When <TeXmacs> owns a selection, it answers requests
  for <verbatim|TARGETS> and <verbatim|STRING> only
  (<verbatim|SelectionRequest> in <source-link|x_loop.cpp|src/Plugins/X11/x_loop.cpp>), with the
  serialized string stored by <cpp|set_selection>. To paste from another
  program, <cpp|x_gui_rep::get_selection> requests <verbatim|STRING> and
  polls for the <verbatim|SelectionNotify> event, giving up after a fixed
  number of polls. The <cpp|format> argument of <cpp|get_selection> and
  <cpp|set_selection> is ignored.

  <paragraph|Printing and dialogs.><name|Widkit> has no print dialog:
  <cpp|printer_widget> is a bare \PCancel\Q button
  (<source-link|widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>), and since <scm|use-print-dialog?> is
  false outside <name|Qt>, printing always goes through the printing
  command. File choosers and other dialogs are <name|Widkit> widgets.

  <section|The <name|Cocoa> port>

  <verbatim|Plugins/Cocoa> is an experimental native port for
  <name|macOS>, written in <name|Objective-C++> with manual reference
  counting (<cpp|NSAutoreleasePool>). It is built with
  <verbatim|configure --enable-cocoa> (<cpp|AQUATEXMACS>), and is
  independent of <verbatim|Plugins/MacOS>, which holds the
  <name|Objective-C> helpers of the <name|Qt> port on <name|macOS>. Most
  files were last changed for real in 2013; later commits only adapted it
  to changes of the abstract interface (styled widgets, extra arguments of
  mouse events).

  <paragraph|The event loop.><cpp|aqua_gui_rep::event_loop>
  (<verbatim|aqua_gui.mm>) does not call <verbatim|[NSApp run]> (that
  variant is disabled with <verbatim|#if 0>) but calls
  <verbatim|finishLaunching> and then loops itself: it waits up to half a
  second for an event with <verbatim|nextEventMatchingMask>, dispatches it
  and all further pending events, and, when no event is left, calls
  <cpp|update>, which runs the interpose handler. The loop has no exit
  condition: the program ends through <cpp|quit>.

  <paragraph|Keyboard and input methods.><verbatim|TMView>
  (<verbatim|TMView.mm>) passes key events through
  <verbatim|interpretKeyEvents:> and implements the <verbatim|NSTextInput>
  protocol (<verbatim|insertText:>, <verbatim|setMarkedText:>,
  <verbatim|doCommandBySelector:>), so that the input methods of
  <name|macOS> can be used.

  <paragraph|Clipboards.>Only the clipboard <verbatim|"primary"> is
  connected to the system, through the general <verbatim|NSPasteboard>
  and the plain string type <verbatim|NSStringPboardType>; other names are
  internal. As in <name|X11>, the <cpp|format> argument is ignored.

  <paragraph|Printing and dialogs.><cpp|printer_widget> is a \PCancel\Q
  button as in <name|Widkit> (<verbatim|aqua_dialogues.mm>), and
  <cpp|gui_refresh> is empty. File choosers and simple dialogs use
  <verbatim|NSSavePanel> and friends in <verbatim|aqua_dialogues.mm>.

  <section|Common limitations>

  Both ports publish only the string <cpp|s> passed to <cpp|set_selection>.
  For an ordinary copy this is the <TeXmacs> snippet, since the verbatim
  version <cpp|sv> is only computed when <cpp|QTTEXMACS> is defined
  (<cpp|edit_select_rep::selection_set> in
  <source-link|Edit/Replace/edit_select.cpp|src/Edit/Replace/edit_select.cpp>); other programs therefore
  receive <TeXmacs> markup rather than plain text. Neither port implements
  the recent constructors listed in <hlink|the overview|guiports.en.tm>
  (five for <name|Widkit>, six for <name|Cocoa>), and both lack the features of the user interface which <scheme> reserves to
  <scm|qt-gui?>.

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
