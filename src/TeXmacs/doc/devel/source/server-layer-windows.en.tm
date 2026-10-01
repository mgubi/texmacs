<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Windows, menus, dialogs and embedded widgets>

  This page describes the code of <verbatim|Texmacs/Window/>: the methods
  of <cpp|tm_window_rep>, the dialogs of <cpp|tm_frame_rep>, the embedded
  <TeXmacs> widgets and the integer handle based \Palternative\Q windows.
  All of it is written against the abstract widget interface of
  <verbatim|Graphics/Gui/widget.hpp> and <verbatim|message.hpp>, so it is
  independent of the toolkit; how the <name|Qt> port implements the
  corresponding widgets is described in <hlink|the <name|Qt>
  implementation|widgets-qt.en.tm>, and the <scheme> side of menus and
  dialogs in <hlink|widgets from <scheme>|widgets-scheme.en.tm>.

  <section|Geometry of top level windows>

  <cpp|texmacs_window_widget (wid, geom)> (<verbatim|tm_window.cpp>) wraps
  the <TeXmacs> widget in a top level window and decides its size and
  position:

  <\enumerate>
    <item>The default size and position come from the <verbatim|-geometry>
    command line option (<cpp|geometry_w>, <cpp|geometry_h>,
    <cpp|geometry_x>, <cpp|geometry_y>; by default 800<math|\<times\>>600
    at the origin; a size of exactly 800<math|\<times\>>600 is
    multiplied by <cpp|retina_zoom> on high resolution screens, and
    toolkits other than <name|Qt> add room for the side tools). Negative
    coordinates count from the right or bottom edge of the screen.

    <item>An explicit geometry <cpp|geom> (a tuple of width and height,
    from <scm|open-window-geometry>) overrides the default size, and
    disables the saved size <em|and> position.

    <item>Otherwise, the size and position saved for the window name are
    used.
  </enumerate>

  Window names are made unique by <cpp|unique_window_name>: the first
  <TeXmacs> window is called <verbatim|TeXmacs>, the next ones
  <verbatim|TeXmacs:2>, <verbatim|TeXmacs:3>, ...; the names are
  released by <cpp|notify_window_destroy>. When the user moves or resizes a
  window, the toolkit calls <cpp|notify_window_move> and
  <cpp|notify_window_resize>, which store the new values in the user
  preferences <verbatim|abscissa <em|name>>, <verbatim|ordinate
  <em|name>>, <verbatim|width <em|name>> and <verbatim|height <em|name>>
  (in pixels; under <name|Qt>, a resize which keeps the width and changes
  the height by at most 80 pixels is ignored, presumably to absorb
  toolbars appearing and disappearing). Popup windows are never recorded. Since the names are
  per session numbers, the second window of a session gets the geometry
  that the second window of the previous session had. Under <name|Qt>,
  <cpp|plain_window_widget> and <cpp|QTMPlainWindow> also save and restore
  the geometry of every other non-popup top level window (dialogs,
  alternative windows) under its title (<verbatim|Plugins/Qt/qt_widget.cpp>,
  <verbatim|Plugins/Qt/QTMWindow.cpp>).

  <section|Methods of <cpp|tm_window_rep>>

  <paragraph|Title and status.><cpp|set_window_name> (only sends the title
  to the widget if it changed), <cpp|set_window_url> (the file
  associated with the window),
  <cpp|set_modified> (the \Pmodified\Q mark of the title bar),
  <cpp|map> and <cpp|unmap>.

  <paragraph|Menus.>Menus and toolbars are given as a string which holds a
  <scheme> menu expression, such as <verbatim|"(horizontal (link
  texmacs-menu))">; it is quoted and evaluated to obtain the menu.
  <cpp|get_menu_widget (which, menu, w)> turns it into a widget:

  <\enumerate>
    <item>It temporarily sets <cpp|the_drd> to the <abbr|DRD> of the view
    shown in the window, since menu entries may depend on the document.

    <item>It <em|expands> the menu with the <scheme> function
    <scm|menu-expand>, which evaluates all dynamic parts and yields a
    static description.

    <item>If the expansion is in the cache <cpp|menu_cache>: if it is also
    the one currently installed in the slot <cpp|which>
    (<cpp|menu_current>), nothing needs to be done and the function
    returns <cpp|false>; otherwise, for the menu bar and the icon bars
    (slots below 10), the cached widget is reused.

    <item>Otherwise, the widget is built with the <scheme> function
    <scm|make-menu-widget> (or <scm|make-menu-widget*> with a size for the
    side tools) and cached if menu caching is enabled and either the slot
    is a tool area or <scm|cache-menu?> accepts the expansion.
  </enumerate>

  Two consequences are worth knowing. First, the \Punchanged\Q test only
  works for expansions which are in the cache: a menu which
  <scm|cache-menu?> rejects (any menu containing an <scm|input> entry) is
  rebuilt every time, even if it did not change, and <scm|menu-expand> is
  run on every call in any case. Second, for the tool areas (slots 10 and
  above) a cached widget is never reused for a different slot content;
  the cache entry only serves the \Punchanged\Q test.

  The slots are numbered as follows: <math|-1> for the main menu bar,
  <math|0> to <math|3> for the main, mode, focus and user icon bars,
  <math|10> and <math|11> for the right and left side tools, and <math|20>
  and <math|21> for the bottom and extra tools. The public methods
  <cpp|menu_main>, <cpp|menu_icons>, <cpp|side_tools> and
  <cpp|bottom_tools> first force the lazy loading of all <scheme> modules
  (<scm|(lazy-initialize-force)>), then call <cpp|get_menu_widget> and,
  if it returns <cpp|true>, install the widget in the <TeXmacs> widget.
  <cpp|refresh> empties the cache. The <cpp|set_..._flag> and
  <cpp|get_..._flag> methods show, hide and query the corresponding bars.

  These methods are normally not called directly. The editor installs
  the menus of its window when it is resumed
  (<cpp|edit_interface_rep::resume>) and rebuilds them whenever the menus
  may have changed (<cpp|update_menus>, called from <cpp|apply_changes>),
  through the server routines <cpp|menu_main>, <cpp|menu_icons>,
  <cpp|side_tools> and <cpp|bottom_tools>. For cacheable menus, the
  comparison with <cpp|menu_current> makes this cheap when nothing has
  changed.

  <paragraph|Canvas.><cpp|set_window_zoom_factor> and
  <cpp|get_window_zoom_factor> (the stored factor includes
  <cpp|retina_zoom>, the returned one does not), <cpp|get_visible>,
  <cpp|get_extents>, <cpp|set_extents>, <cpp|set_scrollbars>,
  <cpp|get_scroll_pos>, <cpp|set_scroll_pos>. They forward to the canvas
  of the <TeXmacs> widget.

  <paragraph|Footer.><cpp|get_footer_flag>, <cpp|set_footer_flag>,
  <cpp|set_left_footer>, <cpp|set_right_footer>.

  <paragraph|Interactive input in the footer.>A single line prompt can
  replace the footer:

  <\description>
    <item*|<cpp|interactive (name, type, def, s, cmd)>>Installs a text
    widget with the prompt <cpp|name> and an input field of the given type
    with the proposals <cpp|def>, switches the footer to interactive mode,
    and remembers where to store the answer (<cpp|s>) and what to call
    when the user is done (<cpp|cmd>). If the footer is already in
    interactive mode, the answer is <verbatim|"cancel"> at once.

    <item*|<cpp|interactive_return ()>>Called by the input field (through
    an <cpp|ia_command_rep>) when the user presses return: stores the
    answer, leaves interactive mode and calls the callback.
  </description>

  <section|Dialogs and interactive commands>

  <paragraph|Dialog windows.><cpp|tm_frame_rep::dialogue_start (name,
  wid)> (<verbatim|tm_dialogue.cpp>) opens <cpp|wid> in a plain window
  with the translated title <cpp|name>, centered on the current window.
  There is a single dialog slot in the server (<cpp|dialogue_win>,
  <cpp|dialogue_wid>); while it is occupied, further calls are ignored.
  <cpp|dialogue_inquire (i, arg)> reads the <math|i>-th input field of
  the dialog (field 0 is the dialog widget itself) and
  <cpp|dialogue_end> closes it. These are the routines behind the
  <scheme> function <scm|dialogue-end>.

  A <cpp|dialogue_command_rep> is the callback of such a dialog. When the
  user validates, it reads the fields from last to first, stops (and
  closes the dialog) if one of them is <verbatim|"#f"> (the dialog was
  cancelled), records the answers with the <scheme> function
  <scm|learn-interactive> (except for password fields, which are recorded
  as empty strings) and schedules both <scm|(dialogue-end)> and the call
  of the <scheme> function with the answers as arguments.

  <paragraph|File choosers.><cpp|choose_file (fun, title, type, prompt,
  name)> builds a <cpp|file_chooser_widget> with such a callback, presets
  its directory from <cpp|name> (or uses <verbatim|.> for a scratch
  buffer), and, unless the type is <verbatim|image> or empty, also the
  file name, adapting the suffix to the requested format (for instance
  when exporting). It shows the chooser with <cpp|dialogue_start> and
  gives the keyboard focus to its file or directory field. It is exported as <scm|cpp-choose-file>; <scheme>
  code normally uses the higher level <scm|choose-file>.

  <paragraph|Interactive commands.><cpp|interactive (fun, p)> asks the
  user for the arguments of the <scheme> function <cpp|fun>; it is the
  implementation of <scm|tm-interactive>, which is used by the
  <scm|interactive> menu entries. <cpp|p> is a tuple with one entry per
  argument, each of which is either a quoted prompt or a tuple of a
  prompt, a type (<verbatim|string>, <verbatim|password>, a file type,
  ...) and proposals. Depending on the situation, the arguments are asked
  for in one of two ways:

  <\itemize>
    <item>in a dialog with one field per argument
    (<cpp|inputs_list_widget>), if the <verbatim|interactive questions>
    preference is <verbatim|popup>, if there are several arguments, or if
    the current buffer is an auxiliary buffer (other than a
    <verbatim|tmfs://part/> buffer), which may not have a usable footer;

    <item>otherwise in the footer, one argument after the other, by an
    <cpp|interactive_command_rep> which calls
    <cpp|tm_window_rep::interactive> for each argument with itself as the
    callback, and calls the function once all answers are known. If the
    function returns a value, it is displayed as a message.
  </itemize>

  With no arguments at all, the function is called at once.

  Note that the <scheme> function <scm|interactive> does not always reach
  this code: it calls <scm|tm-interactive-hook>, which is set to
  <scm|tm-interactive-new> in <verbatim|generic/generic-menu.scm>. When
  the side tools are enabled (<scm|side-tools?>, that is, the
  <verbatim|side tools> and <verbatim|developer tool> preferences are both
  on), that function shows an <scm|interactive-tool> in the bottom tool
  area instead, and <cpp|tm_frame_rep::interactive> is not used.

  <section|Embedded <TeXmacs> widgets>

  An embedded <TeXmacs> widget is a complete editor inside a dialog or a
  side panel, used for instance for input fields with mathematics, for
  the search and replace panes and for the bibliography and database
  tools. It is created from <scheme> with the <scm|texmacs-input> widget,
  which calls <cpp|texmacs_input_widget (doc, style, name)>:

  <\enumerate>
    <item><cpp|enrich_embedded_document> turns <cpp|doc> into a complete
    <TeXmacs> document with the given style and an initial environment
    suitable for a small widget: automatic page size, small screen
    margins, a fixed resolution and zoom factor and the <verbatim|no-zoom>
    flag. The variables of an outer <markup|with> are moved to the
    initial environment.

    <item>The buffer name is <cpp|name>, or a fresh
    <verbatim|tmfs://aux/TeXmacs-input-<em|n>> if none is given. The
    buffer is created, or its contents are replaced if it exists.

    <item>A passive view is obtained and a window is created with the
    second constructor of <cpp|tm_window_rep>, which has no identifier and
    is not registered in <cpp|tm_window_table>.

    <item>The master of the new buffer is set to the master of the current
    buffer, so that links and relative file names are resolved as in the
    surrounding document.

    <item>The view is attached by hand: <cpp|vw-\<gtr\>win> is set, the
    editor is installed as the canvas, and <cpp|ed-\<gtr\>mvw> is set to
    the current view.

    <item>The result is the <TeXmacs> widget wrapped with a
    <cpp|close_embedded_command_rep>, which runs when the widget is
    destroyed.
  </enumerate>

  The close command gives the focus back to the window of the master view
  (or to the first window), removes the buffer and deletes the window. It
  asserts that the buffer has exactly one view: an embedded buffer must
  not be shown elsewhere.

  Embedded widgets are placed either in an \Palternative\Q window (see
  below), for instance in dialogs, or in the side tools of a main window,
  for instance the search and replace tools (<verbatim|generic/search-widgets.scm>)
  and the format tools (<verbatim|generic/format-tools.scm>). To be able to
  close the alternative windows when their buffer is closed,
  the constructor of the close command records the handle of the most
  recently allocated alternative window (<cpp|last_window_handle>) under
  the buffer name in <cpp|window_by_name>. <cpp|window_search (name)>
  returns the handles which still exist, <cpp|is_embedded_buffer> tests
  whether there is one (<scm|buffer-embedded?>), and
  <scm|safely-kill-buffer> and <scm|safely-kill-window> in
  <verbatim|tm-server.scm> use <scm|alt-window-search> to delete them.
  This relies on the order of calls in <verbatim|menu-widget.scm>, where
  the handle is allocated before the widget is built.

  <\warning>
    The mechanism is fragile. For an embedded widget in the side tools,
    no alternative window is allocated, and <cpp|last_window_handle> is
    whatever handle was allocated last, for an unrelated window. If that
    window still exists when the embedded buffer is closed,
    <cpp|window_search> returns it and <scm|safely-kill-buffer> or
    <scm|safely-kill-window> deletes the wrong window.
  </warning>

  The <scheme> constructors of these widgets (<scm|make-texmacs-input>,
  <scm|make-texmacs-output>) pass the document through
  <scm|attach-resize>; inside a <scm|resize> widget (when <scm|global-resize> is
  set), this sets the page medium to <verbatim|papyrus> and fixed page dimensions, which take
  precedence over the automatic page medium mentioned above.

  <section|Output widgets and box widgets>

  <verbatim|Texmacs/Window/tm_button.cpp> contains the widgets which only
  <em|display> typeset material:

  <\description>
    <item*|<cpp|box_widget_rep>>A <cpp|simple_widget_rep> which shows a
    box, centered and scaled, optionally on a background color. It
    forwards mouse moves, clicks and releases to the box as
    <verbatim|"enter">, <verbatim|"leave">, <verbatim|"click">,
    <verbatim|"drag"> and <verbatim|"select"> messages, which makes simple
    interactive boxes possible; <cpp|box_broadcast (msg)> broadcasts a
    message to the whole box of the box widget which last received a
    mouse event.

    <item*|<cpp|box_widget (b, trans)>, <cpp|box_widget (font, s, col,
    trans, ink)>>Widgets for a given box, or for a string in a given font.
    The second one is exported as <scm|widget-box> and used for menu
    entries with special rendering.

    <item*|<cpp|texmacs_output_widget (doc, style)>>Typesets a document
    without creating a buffer or an editor, and shows the resulting box.
    It builds an <cpp|edit_env> by hand (<cpp|initialize_environment>
    applies the style, the modules and the initial environment), typesets
    the body with the lazy typesetter at its natural width, and wraps the
    box. If the document belongs to a project, the references, auxiliary
    data and attachments of the project are used. It is behind the
    <scm|texmacs-output> widget.

    <item*|<cpp|tree_extents (doc)>>Computes the size of a document in a
    similar way (but without <cpp|enrich_embedded_document> and without
    project data), in units of 5 pixels rounded up; exported as
    <scm|tree-extents>, which is only used by the old GUI code in
    <verbatim|kernel/old-gui/>.

    <item*|<cpp|get_texmacs_widget_size (wid)>>The size hint of such a
    widget.
  </description>

  <section|Alternative windows>

  The end of <verbatim|tm_window.cpp> implements a second, simpler family
  of top level windows, identified by small integers rather than by
  <abbr|URL>s. They are used for the top level windows which <scheme> builds from
  widgets (dialogs and tool windows made with <scm|top-window>,
  <scm|dialogue-window> and similar functions of
  <verbatim|kernel/gui/menu-widget.scm>, and tooltips) through the
  <scm|alt-window-...> glue, which the code describes as
  \Ptransitional\Q. Editor windows (<scm|open-window>) are
  <cpp|tm_window_rep>s, and the dialogs of <cpp|tm_frame_rep> (file
  choosers, popup questions) use the separate dialog slot.

  <\description>
    <item*|<cpp|window_handle ()>>Allocates a new handle (and records it in
    <cpp|last_window_handle>, see above).

    <item*|<cpp|window_create (win, wid, name, quit)>,
    <cpp|window_create_plain>, <cpp|window_create_popup>,
    <cpp|window_create_tooltip>>Wrap <cpp|wid> in a top level window of the
    corresponding kind and store it in the static table
    <cpp|window_table>.

    <item*|<cpp|window_delete (win)>>Sends a destroy notification to the
    widget (so that embedded <TeXmacs> widgets inside it are closed) and
    destroys it; a guard prevents recursive deletion of the same window.

    <item*|<cpp|window_show>, <cpp|window_hide>, <cpp|window_get_size>,
    <cpp|window_set_size>, <cpp|window_get_position>,
    <cpp|window_set_position>>The obvious operations, with sizes in
    pixels.
  </description>

  These windows have no <cpp|tm_window_rep>, no view and no menus of their
  own. They are not returned by <cpp|windows_list>.

  <section|Refreshing dynamic widgets>

  Widgets built from <scheme> may contain <scm|refresh> parts, which are
  rebuilt when they are sent a refresh message. <cpp|windows_refresh
  (kind)> sends <cpp|send_refresh (w, kind)> to every alternative window.
  It is called by the interpose handler with <verbatim|"auto">, in which
  case it does nothing unless a refresh was requested with
  <cpp|windows_delayed_refresh (ms)> and the delay has elapsed; after an
  automatic refresh, the next one is postponed indefinitely. <scheme> code
  calls <scm|refresh-now> with a specific kind to refresh only the
  widgets of that kind. The mechanism is described in more detail in
  <hlink|refreshing dynamic widgets|widgets-window.en.tm>.

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
