<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Windows>

  A <em|window> is a top level <TeXmacs> window with a menu bar, icon
  bars, side and bottom tool areas, a canvas and a footer. The canvas shows
  one view, whose editor draws the document and decides what the menus,
  toolbars and footer contain. This page describes the class
  <cpp|tm_window_rep>, how windows are created and closed, how their
  menus and toolbars are built and cached, and the other kinds of windows
  which the server manages: dialogs, embedded <TeXmacs> editors and the
  \Palternative\Q windows which <scheme> builds from widgets. All of it is
  written against the abstract widget interface of
  <source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp> and <source-link|message.hpp|src/Graphics/Gui/message.hpp>, so it is
  independent of the toolkit; how the <name|Qt> port implements the
  corresponding widgets is described in <hlink|the <name|Qt>
  implementation|widgets-qt.en.tm>, and the <scheme> side of menus and
  dialogs in <hlink|widgets from <scheme>|widgets-scheme.en.tm>.

  The <TeXmacs> widget built by <cpp|texmacs_widget (mask, quit)> has the
  following parts; the numbers are the <cpp|which> arguments used to
  address the bars (see <hlink|menus and toolbars|#menus> below):

  <\verbatim-code>
    +-------------------------------------------------------------+

    \| header: main menu bar (-1) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \| icon bars: main (0), mode (1), focus (2), user (3) \ \ \ \ \ \ \ \ \ \|

    +----------+--------------------------------+-----------------+

    \| left \ \ \ \ \| canvas: the editor of the view \ \| side tools (10) \|

    \| tools \ \ \ \| (scrollable) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \| (11) \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    +----------+--------------------------------+-----------------+

    \| bottom tools (20), extra tools (21) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \| footer: messages, context, interactive prompts \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    +-------------------------------------------------------------+
  </verbatim-code>

  Three other kinds of top level windows exist besides these document
  windows, and it is easy to confuse them:

  <\description>
    <item*|Dialogs of the server>File choosers and popup questions opened
    by <cpp|tm_frame_rep::dialogue_start>; there is a single slot for them
    (<hlink|dialogs|#dialogs>).

    <item*|Alternative windows>Windows built by <scheme> from widgets
    (<scm|top-window>, <scm|dialogue-window>, tooltips), identified by
    small integers (<hlink|alternative windows|#alt-windows>).

    <item*|Embedded editors>Not top level windows themselves, but complete
    editors inside a widget, each with its own buffer, view and an
    anonymous <cpp|tm_window_rep> (<hlink|embedded
    widgets|#embedded>).
  </description>

  Only document windows have an identifier, appear in
  <cpp|windows_list> (<scm|window-list>) and can be current.

  <section|The class <cpp|tm_window_rep>>

  <\explain>
    <cpp|class tm_window_rep><explain-synopsis|a <TeXmacs> window>
  <|explain>
    Declared in <source-link|Texmacs/tm_window.hpp|src/Texmacs/tm_window.hpp>, implemented in
    <source-link|Texmacs/Window/tm_window.cpp|src/Texmacs/Window/tm_window.cpp>. Its public fields are:

    <\description>
      <item*|<cpp|widget win>>The top level window widget.

      <item*|<cpp|widget wid>>The <TeXmacs> widget inside it, with menus,
      icon bars, side and bottom tools, the canvas and the footer. All the
      methods of the class operate on <cpp|wid> through the generic widget
      messages (<cpp|set_main_menu>, <cpp|set_zoom_factor>,
      <cpp|set_left_footer>, ...).

      <item*|<cpp|url id>>The identifier <verbatim|tmfs://window/<em|n>>,
      or <cpp|url_none ()> for the window of an embedded widget.

      <item*|<cpp|hashmap\<less\>tree,tree\<gtr\> props>>Arbitrary
      properties, set and read from <scheme> with
      <scm|window-set-property> and <scm|window-get-property>.

      <item*|<cpp|int serial>>A serial number, unique for the session,
      returned by <scm|window-get-serial>.

      <item*|<cpp|double zoomf>>The zoom factor, multiplied by
      <cpp|retina_zoom>.
    </description>

    Its protected fields are the menu caches <cpp|menu_current> and
    <cpp|menu_cache>, the state of an interactive prompt in the footer
    (<cpp|text_ptr> and <cpp|call_back>), and the current title
    <cpp|cur_title>.

    There are two constructors:

    <\description>
      <item*|<cpp|tm_window_rep (widget wid, tree geom)>>For ordinary
      windows: wraps <cpp|wid> in a top level window of the given geometry
      (<cpp|texmacs_window_widget>), allocates an identifier and takes the
      default zoom factor of the server.

      <item*|<cpp|tm_window_rep (tree doc, command quit)>>For embedded
      widgets: builds a <TeXmacs> widget without any bars, uses it as both
      <cpp|win> and <cpp|wid>, does not allocate an identifier, and takes
      the zoom factor from the document if it has one.
    </description>

    The destructor releases the identifier. The methods are described
    below.
  </explain>

  <section|Window identifiers and the window table>

  Windows are owned by the static table <cpp|tm_window_table> of
  <source-link|Texmacs/Data/new_window.cpp|src/Texmacs/Data/new_window.cpp>, which maps window identifiers to
  <cpp|tm_window_rep> pointers. The identifiers are <abbr|URL>s
  <verbatim|tmfs://window/<em|n>>, allocated by <cpp|create_window_id>
  (from the constructor of <cpp|tm_window_rep>) and released by
  <cpp|destroy_window_id> (from its destructor); the counter is never
  decremented, so identifiers are not reused during a session.

  <\description>
    <item*|Conversions><cpp|concrete_window (url)> is a lookup in
    <cpp|tm_window_table>; <cpp|abstract_window> returns the field
    <cpp|id> of the window.

    <item*|Enumeration><cpp|windows_list> returns the identifiers of all
    document windows, in order of creation. <cpp|get_nr_windows>
    (<scm|windows-number>) is something else: the number of top level
    windows as counted by the GUI back-end (<cpp|nr_windows>). Under
    <name|Qt> it is maintained by <cpp|qt_window_widget_rep> for all its
    non \Pfake\Q windows, which includes dialogs and alternative windows;
    under X11 by <source-link|x_window.cpp|src/Plugins/X11/x_window.cpp>; in the <name|Cocoa> port it stays
    0. In no case is it the length of <cpp|windows_list>.

    <item*|Current window><cpp|has_current_window>,
    <cpp|get_current_window> (returns the empty <abbr|URL> if there is
    none) and <cpp|concrete_window ()>. There is no stored current window:
    it is always the window of the current view (see <hlink|the current
    view|server-views.en.tm>).

    <item*|Queries><cpp|buffer_to_windows>, <cpp|window_to_buffer> and
    <cpp|window_to_view>, which searches the view history for the view
    attached to the window.
  </description>

  <section|Creating windows>

  A new window is created by <cpp|new_window (bool map_flag, tree geom)>
  (not declared in any header). It

  <\enumerate>
    <item>builds the <TeXmacs> widget with <cpp|texmacs_widget (mask,
    quit)>, where the bits of <cpp|mask> are computed from the preferences
    <verbatim|header> (1), <verbatim|main icon bar> (2), <verbatim|mode
    dependent icons> (4), <verbatim|focus dependent icons> (8),
    <verbatim|user provided icons> (16), <verbatim|status bar> (32),
    <verbatim|bottom tools> (256) and <verbatim|extra tools> (512), so
    that only the bars enabled in the preferences are initially visible;

    <item>creates the <cpp|tm_window_rep>, which wraps the widget in a top
    level window with <cpp|texmacs_window_widget> (see <hlink|geometry of
    top level windows|#geometry>) and allocates an identifier;

    <item>registers the window in <cpp|tm_window_table> and maps it.
  </enumerate>

  The <cpp|quit> command passed to the widget is a
  <cpp|kill_window_command_rep>, which is called when the user clicks on
  the close box of the window. It does not close anything itself: it
  schedules the <scheme> command <scm|(safely-kill-window <scm-arg|id>)>
  with <cpp|exec_delayed>, so that the user can be asked for confirmation.
  (It holds a pointer to an <abbr|URL> which is filled in only once the
  identifier is known.)

  A new window is empty; it shows nothing until a view is attached to
  it. The exported routines therefore always combine the two steps:

  <\description-paragraphs>
    <item*|<cpp|open_window (geom)>>Creates a new scratch buffer and shows
    it in a new window (<scm|open-window>).

    <item*|<cpp|new_buffer_in_new_window (name, doc, geom)>>Creates the
    buffer from <cpp|doc> if it does not exist, opens a new window and
    shows a passive view on the buffer in it, with the focus
    (<scm|open-buffer-in-window>; this is how <scm|load-buffer> implements
    <scm|:new-window>).

    <item*|<cpp|clone_window ()>>Opens a new window with a passive view on
    the current buffer (<scm|clone-window>); a new view is created if all
    existing views are displayed in windows, so that the two windows show
    two views on the same document.

    <item*|<cpp|create_buffer ()>>Creates a new scratch buffer in the
    <em|current> window (<scm|new-buffer>).
  </description-paragraphs>

  The user commands <scm|new-document> and <scm|new-document*> choose
  between <scm|open-window> and <scm|new-buffer> according to the
  <verbatim|buffer management> preference: with the value
  <verbatim|separate> (the default on <name|macOS> and <name|Windows>,
  tested by the mode predicate <scm|window-per-buffer?> of
  <source-link|kernel/texmacs/tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm>) each document gets its own
  window; with <verbatim|shared> (the default elsewhere) documents replace
  each other in the same window.

  <section|Closing windows>

  At the <c++> level, windows are destroyed by the file local
  <cpp|delete_window>, which detaches the view of the window (the view is
  kept, so that the \Pmodified\Q status of its buffer remains available),
  unmaps the window, removes it from the table, destroys the window widget
  and deletes the <cpp|tm_window_rep>. It is called by

  <\description>
    <item*|<cpp|kill_window (win)>>(<scm|kill-window>) Makes the most
    recent view which is shown in <em|another> window current and deletes
    the window. If there is no other window, the program quits, unless it
    acts as a server for remote clients (<cpp|number_of_servers ()>), in
    which case the window is deleted anyway. The buffer of the window is
    not closed.

    <item*|<cpp|kill_current_window_and_buffer ()>>Quits if there is only
    one buffer; otherwise closes the current window, and also removes its
    buffer if no other window shows it.
  </description>

  The user command is <scm|safely-kill-window> in
  <source-link|texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>, called from the close box (with
  the window as argument) and, through <scm|close-document>, from the
  <menu|File> menu (without argument):

  <\enumerate>
    <item>If the current buffer is an embedded buffer and no window was
    given, it deletes the alternative windows which contain it (see
    <hlink|embedded widgets|#embedded>).

    <item>If <scm|windows-number> is at most 1, it calls
    <scm|safely-quit-TeXmacs>, which asks for confirmation if some
    non-auxiliary buffer is modified and then quits.

    <item>Otherwise it asks for confirmation if the buffer of the window
    is modified, then calls <scm|kill-window> and, after an idle delay of
    100 milliseconds, <scm|buffer-close> on that buffer.
  </enumerate>

  So closing a window <em|also closes its buffer>, even when the buffer is
  still shown in another window: <cpp|kill_buffer> then gives that other
  window a view on another buffer. Since <scm|windows-number> counts all
  toolkit windows, an open dialog or tool window can make the second test
  fail for the last document window; <cpp|kill_window> then quits anyway
  (without the confirmation of <scm|safely-quit-TeXmacs>, but only after
  the confirmation about the buffer of the window).

  The entries of the <menu|File> menu call <scm|close-document> and
  <scm|close-document*>, which choose between <scm|safely-kill-window> and
  <scm|safely-kill-buffer> according to the <verbatim|buffer management>
  preference, in the same way as <scm|new-document> above.

  <section|Geometry of top level windows><label|geometry>

  <cpp|texmacs_window_widget (wid, geom)> (<source-link|tm_window.cpp|src/Texmacs/Window/tm_window.cpp>) wraps
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
  alternative windows) under its title (<source-link|Plugins/Qt/qt_widget.cpp|src/Plugins/Qt/qt_widget.cpp>,
  <source-link|Plugins/Qt/QTMWindow.cpp|src/Plugins/Qt/QTMWindow.cpp>).

  <section|Menus and toolbars><label|menus>

  Menus and toolbars are given as a string which holds a
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

  These methods are normally not called directly: the menus of a window
  are installed by its editor. When the editor is resumed
  (<cpp|edit_interface_rep::resume>, called by <cpp|attach_view>,
  <cpp|switch_to_window> and <cpp|var_focus_on_buffer>), it calls

  <\cpp-code>
    SERVER (menu_main ("(horizontal (link texmacs-menu))"));

    SERVER (menu_icons (0, "(horizontal (link texmacs-main-icons))"));

    SERVER (menu_icons (1, "(horizontal (link texmacs-mode-icons))"));

    SERVER (menu_icons (2, "(horizontal (link texmacs-focus-icons))"));

    SERVER (menu_icons (3, "(horizontal (link texmacs-extra-icons))"));
  </cpp-code>

  and similarly <cpp|side_tools (1, ...)>, <cpp|side_tools (0, ...)>,
  <cpp|bottom_tools (0, ...)> and <cpp|bottom_tools (1, ...)> with the
  dynamic menus <scm|texmacs-left-tools>, <scm|texmacs-side-tools>,
  <scm|texmacs-bottom-tools> and <scm|texmacs-extra-tools>, which receive
  the window as an argument. <cpp|update_menus>, called from
  <cpp|apply_changes> when the document changed and the user is idle,
  does the same. The <cpp|SERVER> macro makes the editor current during
  each call, because the server routines act on the current window (see
  <hlink|working in the context of another view|server-views.en.tm>).
  For cacheable menus, the comparison with <cpp|menu_current> makes these
  repeated calls cheap when nothing has changed.

  <section|Other methods of <cpp|tm_window_rep>>

  <paragraph|Title and status.><cpp|set_window_name> (only sends the title
  to the widget if it changed), <cpp|set_window_url> (the file
  associated with the window),
  <cpp|set_modified> (the \Pmodified\Q mark of the title bar),
  <cpp|map> and <cpp|unmap>.

  <paragraph|Canvas.><cpp|set_window_zoom_factor> and
  <cpp|get_window_zoom_factor> (the stored factor includes
  <cpp|retina_zoom>, the returned one does not), <cpp|get_visible>,
  <cpp|get_extents>, <cpp|set_extents>, <cpp|set_scrollbars>,
  <cpp|get_scroll_pos>, <cpp|set_scroll_pos>. They forward to the canvas
  of the <TeXmacs> widget.

  <paragraph|Footer.><cpp|get_footer_flag>, <cpp|set_footer_flag>,
  <cpp|set_left_footer>, <cpp|set_right_footer>.

  The contents of the footer are decided by the editor
  (<source-link|Edit/Interface/edit_footer.cpp|src/Edit/Interface/edit_footer.cpp>). <cpp|set_message (left,
  right, temp)> stores a message and calls <cpp|notify_change
  (THE_DECORATIONS)>; <cpp|set_footer>, called from <cpp|update_menus>,
  displays the message if there is one, and otherwise computes a
  description of the context of the cursor (mode, language, font, and the
  path of enclosing tags) with <cpp|set_left_footer> and
  <cpp|set_right_footer>. The server routine
  <cpp|tm_frame_rep::set_message> (<scheme>: <scm|set-message>) forwards
  to the current editor.

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

  <section|Dialogs and interactive commands><label|dialogs>

  <paragraph|Dialog windows.><cpp|tm_frame_rep::dialogue_start (name,
  wid)> (<source-link|tm_dialogue.cpp|src/Texmacs/Window/tm_dialogue.cpp>) opens <cpp|wid> in a plain window
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
  <scm|tm-interactive-new> in <source-link|generic/generic-menu.scm|TeXmacs/progs/generic/generic-menu.scm>. When
  the side tools are enabled (<scm|side-tools?>, that is, the
  <verbatim|side tools> and <verbatim|developer tool> preferences are both
  on), that function shows an <scm|interactive-tool> in the bottom tool
  area instead, and <cpp|tm_frame_rep::interactive> is not used.

  <section|Embedded <TeXmacs> widgets><label|embedded>

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
  for instance the search and replace tools (<source-link|generic/search-widgets.scm|TeXmacs/progs/generic/search-widgets.scm>)
  and the format tools (<source-link|generic/format-tools.scm|TeXmacs/progs/generic/format-tools.scm>). To be able to
  close the alternative windows when their buffer is closed,
  the constructor of the close command records the handle of the most
  recently allocated alternative window (<cpp|last_window_handle>) under
  the buffer name in <cpp|window_by_name>. <cpp|window_search (name)>
  returns the handles which still exist, <cpp|is_embedded_buffer> tests
  whether there is one (<scm|buffer-embedded?>), and
  <scm|safely-kill-buffer> and <scm|safely-kill-window> in
  <source-link|tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm> use <scm|alt-window-search> to delete them.
  This relies on the order of calls in <source-link|menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>, where
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

  <source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp> contains the widgets which only
  <em|display> typeset material:

  <\description-paragraphs>
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
    <source-link|kernel/old-gui/|TeXmacs/progs/kernel/old-gui>.

    <item*|<cpp|get_texmacs_widget_size (wid)>>The size hint of such a
    widget.
  </description-paragraphs>

  <section|Alternative windows><label|alt-windows>

  The end of <source-link|tm_window.cpp|src/Texmacs/Window/tm_window.cpp> implements a second, simpler family
  of top level windows, identified by small integers rather than by
  <abbr|URL>s. They are used for the top level windows which <scheme> builds from
  widgets (dialogs and tool windows made with <scm|top-window>,
  <scm|dialogue-window> and similar functions of
  <source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>, and tooltips) through the
  <scm|alt-window-...> glue, which the code describes as
  \Ptransitional\Q. Editor windows (<scm|open-window>) are
  <cpp|tm_window_rep>s, and the dialogs of <cpp|tm_frame_rep> (file
  choosers, popup questions) use the separate dialog slot.

  <\description-paragraphs>
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
  </description-paragraphs>

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

  <section|Pitfalls>

  <\itemize>
    <item>Do not keep <cpp|tm_window> pointers across operations which may
    close windows; keep the identifier.

    <item><scm|windows-number> is not the number of document windows; use
    <scm|(length (window-list))> for that.

    <item>Closing a window with <scm|safely-kill-window> also closes its
    buffer; use <scm|kill-window> to close only the window.

    <item>The frame routines of the server act on the window of the
    current view. Right after <scm|switch-to-window>, this is still the old
    window (see <hlink|when the current view
    changes|server-views.en.tm>).

    <item>There is a single dialog slot: a second dialog requested while
    one is open is silently ignored.

    <item>The bookkeeping which closes the alternative windows of an
    embedded buffer can close the wrong window (see the warning in
    <hlink|embedded widgets|#embedded>).
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
