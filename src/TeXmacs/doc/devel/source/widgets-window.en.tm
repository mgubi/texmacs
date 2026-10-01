<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Windows, the main <TeXmacs> widget and the flow of events>

  <section|Window widgets>

  A window is represented by a widget as well. Any widget <cpp|w> can be
  put into a window with

  <\cpp-code>
    widget plain_window_widget (widget w, string name, command quit=
    command ());

    widget popup_window_widget (widget w, string name);

    widget tooltip_window_widget (widget w, string name);

    void \ \ destroy_window_widget (widget w);
  </cpp-code>

  The result is a <em|window widget>, which understands the window-related
  slots: <cpp|set_visibility> maps or unmaps it, <cpp|set_name> changes its
  title, <cpp|set_position> and <cpp|set_size> move and resize it,
  <cpp|get_identifier> returns a non-zero identifier once the window
  exists. The string <cpp|name> is not the title but a unique identifier,
  under which the geometry of the window is remembered between sessions
  (see <cpp|notify_window_move>, <cpp|notify_window_resize>,
  <cpp|get_preferred_position> and <cpp|get_preferred_size> in
  <verbatim|Texmacs/Window/tm_window.cpp>). The command <cpp|quit> is
  executed when the user closes the window.

  The abstract class <cpp|window_rep> of <verbatim|window.hpp>, with its
  constructors <cpp|plain_window> and <cpp|popup_window>, is a lower level
  interface used only by ports which build on <name|Widkit> (it is
  implemented in <verbatim|Plugins/X11/x_window.cpp>). Kernel code never
  uses it directly.

  <section|The <TeXmacs> windows>

  <subsection|The class <cpp|tm_window_rep>>

  A <TeXmacs> editing window is an instance of <cpp|tm_window_rep>
  (<verbatim|Texmacs/tm_window.hpp>). Its most important fields are

  <\description>
    <item*|<cpp|widget win>>the window widget (the result of
    <cpp|plain_window_widget>);

    <item*|<cpp|widget wid>>the <em|main <TeXmacs> widget> inside it,
    created by <cpp|texmacs_widget>;

    <item*|<cpp|url id>>the abstract name of the window, as seen from
    <scheme> (see \P<hlink|Manipulating <TeXmacs>
    windows|../scheme/buffer/window-api.en.tm>\Q);

    <item*|<cpp|menu_current>, <cpp|menu_cache>>the caches used to avoid
    rebuilding menus and icon bars (see below).
  </description>

  Windows are created by <cpp|new_window> in
  <verbatim|Texmacs/Data/new_window.cpp>. It reads the user preferences
  to build the <em|mask> argument of <cpp|texmacs_widget>, which says which
  parts of the main widget are initially visible:

  <descriptive-table|<tformat|<table|<row|<cell|Bit>|<cell|Preference>|<cell|Part>>|<row|<cell|1>|<cell|<verbatim|header>>|<cell|menu
  and icon bars as a whole>>|<row|<cell|2>|<cell|<verbatim|main icon
  bar>>|<cell|main icons>>|<row|<cell|4>|<cell|<verbatim|mode dependent
  icons>>|<cell|mode icons>>|<row|<cell|8>|<cell|<verbatim|focus dependent
  icons>>|<cell|focus icons>>|<row|<cell|16>|<cell|<verbatim|user provided
  icons>>|<cell|user icons>>|<row|<cell|32>|<cell|<verbatim|status
  bar>>|<cell|footer>>|<row|<cell|64,
  128>|<cell|(currently not set)>|<cell|right and left side
  tools>>|<row|<cell|256>|<cell|<verbatim|bottom tools>>|<cell|bottom
  tools>>|<row|<cell|512>|<cell|<verbatim|extra tools>>|<cell|extra
  tools>>>>>

  and passes a <cpp|kill_window_command_rep> as the <cpp|quit> command
  (which calls the <scheme> function <scm|safely-kill-window> through
  <cpp|exec_delayed>). The constructor <cpp|tm_window_rep (widget wid2, tree
  geom)> then calls <cpp|texmacs_window_widget>, which wraps the main widget
  with <cpp|plain_window_widget> under a unique name derived from
  <verbatim|"TeXmacs"> and sets its initial size and position.

  <subsection|Attaching views>

  The main <TeXmacs> widget is a canvas; the document is displayed by an
  <em|editor>, which is a <cpp|simple_widget_rep> (<cpp|editor_rep> derives
  from it). When a view is attached to a window (<cpp|attach_view> in
  <verbatim|Texmacs/Data/new_view.cpp>), the editor is installed in the
  main widget with

  <\cpp-code>
    set_scrollable (wid, vw-\<gtr\>ed);

    vw-\<gtr\>ed-\<gtr\>cvw= wid.rep;
  </cpp-code>

  and <cpp|detach_view> replaces it by an empty <cpp|glue_widget ()>. The
  editor uses <cpp|cvw> to send messages back to its window (for instance
  <cpp|get_canvas>, <cpp|set_extents> or <cpp|send_keyboard_focus_on>).

  <subsection|Menus, icon bars and tools>

  The contents of the bars of a window are not stored in the window. They
  are recomputed from <scheme> each time the editor thinks they may have
  changed, in <cpp|edit_interface_rep::update_menus>
  (<verbatim|Edit/Interface/edit_interface.cpp>):

  <\cpp-code>
    SERVER (menu_main ("(horizontal (link texmacs-menu))"));

    SERVER (menu_icons (0, "(horizontal (link texmacs-main-icons))"));

    SERVER (menu_icons (1, "(horizontal (link texmacs-mode-icons))"));

    SERVER (menu_icons (2, "(horizontal (link texmacs-focus-icons))"));

    SERVER (menu_icons (3, "(horizontal (link texmacs-extra-icons))"));

    ...

    SERVER (side_tools (0, "(vertical " * rdyn * ")"));
  </cpp-code>

  where <verbatim|rdyn> is a string such as <verbatim|(dynamic
  (texmacs-side-tools <em|win>))>. The same calls are made by
  <cpp|edit_interface_rep::resume> when a view gets the focus. The
  arguments are the textual form of <scheme> <em|menu items> (see
  \P<hlink|The <scheme> widget language and its
  interpreter|widgets-scheme.en.tm>\Q); the symbols <scm|texmacs-menu>,
  <scm|texmacs-main-icons>, ... are defined with <scm|menu-bind> in
  <verbatim|texmacs/menus/main-menu.scm> and other files.
  <cpp|update_menus> is called by <cpp|edit_interface_rep::apply_changes>
  when the flag <cpp|THE_MENUS> is set, or when the editor has been idle
  for a short while after a change; it can also be forced from <scheme>
  with <scm|update-menus>.

  The server methods of <cpp|tm_frame_rep> forward to the current
  <cpp|tm_window_rep>, whose methods <cpp|menu_main>, <cpp|menu_icons>,
  <cpp|side_tools> and <cpp|bottom_tools> all go through

  <\explain>
    <cpp|bool tm_window_rep::get_menu_widget (int which, string menu,
    widget& w)><explain-synopsis|build or reuse a menu widget>
  <|explain>
    <cpp|which> identifies the bar: -1 for the menu bar, 0 to 3 for the
    icon bars, 10 and 11 for the right and left side tools, 20 and 21 for
    the bottom and extra tools. The function first calls the <scheme>
    function <scm|menu-expand> on the menu item, which evaluates all its
    dynamic parts and yields a closure-free description. If this expansion
    is equal to the one currently displayed in the same bar
    (<cpp|menu_current[which]>), nothing has changed and the function
    returns <cpp|false>. For the menu bar and the icon bars (<cpp|which> \<less\> 10), a
    widget found in <cpp|menu_cache> under the same expansion is reused.
    Otherwise the widget is built by <cpp|make_menu_widget>, which calls
    the <scheme> function <scm|make-menu-widget> (or, for the side tools,
    <scm|make-menu-widget*> with a size of 400 by 1000 pixels, only used by
    the markup interface), and stored in the cache if the global flag
    <cpp|menu_caching> is set and either <cpp|which> \<geq\> 10 or the
    <scheme> predicate <scm|cache-menu?> accepts the expansion.
  </explain>

  If <cpp|get_menu_widget> returns <cpp|true>, the new widget is
  installed with <cpp|set_main_menu>, <cpp|set_main_icons>, ...,
  <cpp|set_side_tools>, <cpp|set_left_tools>, <cpp|set_bottom_tools> or
  <cpp|set_extra_tools>. Before this, <cpp|(lazy-initialize-force)> is
  evaluated so that all lazily defined menus are available. The cache is
  flushed by <cpp|tm_window_rep::refresh>, called for all windows by
  <cpp|tm_server_rep::refresh> when the interface language changes.

  The visibility of the bars is controlled by
  <cpp|tm_frame_rep::show_header>, <cpp|show_icon_bar>,
  <cpp|show_side_tools>, <cpp|show_bottom_tools> and <cpp|show_footer>,
  exported to <scheme> as <scm|show-header>, <scm|show-icon-bar>, ...,
  which send the corresponding visibility slots to the main widget.

  <subsection|The footer and interactive input>

  The left and right parts of the footer are set by the editor
  (<verbatim|Edit/Interface/edit_footer.cpp>) through
  <cpp|tm_frame_rep::set_left_footer> and <cpp|set_right_footer>, which
  send <cpp|SLOT_LEFT_FOOTER> and <cpp|SLOT_RIGHT_FOOTER>.

  The footer can also be used to ask the user for a string. In
  <cpp|tm_window_rep::interactive>, the kernel builds a prompt with
  <cpp|text_widget> and an input field with <cpp|input_text_widget>, whose
  call-back is an <cpp|ia_command_rep>, installs them with
  <cpp|set_interactive_prompt> and <cpp|set_interactive_input> and switches
  on <cpp|set_interactive_mode>. When the user validates, the call-back
  runs <cpp|interactive_return>, which reads the answer with
  <cpp|get_interactive_input>, leaves interactive mode and calls the
  continuation.

  <section|Dialogs>

  <subsection|Dialogs built by the kernel>

  <verbatim|Texmacs/Window/tm_dialogue.cpp> implements two kinds of
  dialogs directly in <c++>.

  <\itemize>
    <item><cpp|tm_frame_rep::choose_file> (glue <scm|cpp-choose-file>)
    builds a <cpp|file_chooser_widget>, initializes it with
    <cpp|set_directory> and <cpp|set_file> and opens it with
    <cpp|dialogue_start>.

    <item><cpp|tm_frame_rep::interactive> (glue <scm|tm-interactive>) asks
    for the arguments of a <scheme> function. Depending on the preference
    <verbatim|interactive questions>, the number of arguments and the
    current buffer, it either uses the footer (through an
    <cpp|interactive_command_rep>, which asks the arguments one by one with
    <cpp|tm_window_rep::interactive>), or builds an
    <cpp|inputs_list_widget> with one field per argument. The fields are
    accessed with <cpp|get_form_field>, and configured with
    <cpp|set_input_type>, <cpp|set_string_input> and
    <cpp|add_input_proposal>.
  </itemize>

  <cpp|dialogue_start> wraps the dialog into a <cpp|plain_window_widget>,
  centers it on the current window and shows it; the call-back
  (a <cpp|dialogue_command_rep>) collects the answers with
  <cpp|dialogue_inquire>, which uses <cpp|get_string_input> or
  <cpp|get_form_field>, and closes the dialog through the <scheme>
  function <scm|dialogue-end>.

  <subsection|Windows created from <scheme>>

  Most dialogs are nowadays written in <scheme> and displayed in auxiliary
  windows managed by a few functions of <verbatim|tm_window.cpp> (the glue
  names are in parentheses):

  <\explain>
    <cpp|int window_handle ()> (<scm|alt-window-handle>)

    <cpp|void window_create (int win, widget wid, string name, command
    quit)> (<scm|alt-window-create-quit>)

    <cpp|void window_create_plain (int win, widget wid, string name)>
    (<scm|alt-window-create-plain>)

    <cpp|void window_create_popup (int win, widget wid, string name)>
    (<scm|alt-window-create-popup>)

    <cpp|void window_create_tooltip (int win, widget wid, string name)>
    (<scm|alt-window-create-tooltip>)<explain-synopsis|auxiliary windows>
  <|explain>
    <cpp|window_handle> returns a fresh integer handle. The creation
    functions wrap <cpp|wid> with the corresponding window constructor and
    store the result in the table <cpp|window_table>. Further functions
    (<scm|alt-window-show>, <scm|alt-window-hide>,
    <scm|alt-window-get-size>, <scm|alt-window-set-position>, ...)
    send the usual window slots to the stored window widget.
  </explain>

  <cpp|window_delete> (<scm|alt-window-delete>) removes the window from
  the table, sends <cpp|SLOT_DESTROY> to it (so that embedded editors can
  clean up, see <cpp|wrapped_widget> below) and calls
  <cpp|destroy_window_widget>. The <scheme> functions <scm|top-window>,
  <scm|dialogue-window> and <scm|interactive-window> are built on top of
  these primitives.

  <subsection|Contextual menus>

  The contextual menu of the editor shows how a menu can be displayed
  outside the menu bar. In <cpp|edit_interface_rep::mouse_adjust>
  (<verbatim|Edit/Interface/edit_mouse.cpp>):

  <\cpp-code>
    SERVER (menu_widget ("(vertical (link " * menu * "))", wid));

    widget popup_wid= ::popup_widget (wid);

    popup_win= ::popup_window_widget (popup_wid, "Popup menu");

    ...

    set_position (popup_win, wx+ ox+ x, wy+ oy+ y);

    set_visibility (popup_win, true);

    send_keyboard_focus (this);

    send_mouse_grab (popup_wid, true);
  </cpp-code>

  In the <name|Qt> port, <cpp|popup_widget> applied to a
  <cpp|vertical_menu> yields a native <cpp|QMenu>.

  <subsection|Embedded <TeXmacs> widgets>

  <cpp|texmacs_input_widget> embeds a full editor in a dialog. It creates
  or updates an auxiliary buffer, a view on it and a
  <cpp|tm_window_rep> built with the second constructor <cpp|tm_window_rep
  (tree doc, command quit)>, which calls <cpp|texmacs_widget (0, quit)>: a
  mask of zero asks the port for a stripped-down main widget without bars
  (in <name|Qt> a <cpp|qt_tm_embedded_widget_rep>). The editor is attached
  with <cpp|set_scrollable> as for an ordinary window, and the whole thing
  is returned wrapped by <cpp|wrapped_widget>, whose command
  (<cpp|close_embedded_command>) is executed on <cpp|SLOT_DESTROY> and
  closes the auxiliary buffer.

  <cpp|texmacs_output_widget> (<verbatim|tm_button.cpp>) is much lighter:
  it typesets the document once into a box and returns a
  <cpp|box_widget_rep>, a read-only <cpp|simple_widget_rep> which paints the
  box.

  <section|Refreshing dynamic widgets>

  Besides the bars of the main window, which are rebuilt by
  <cpp|update_menus>, widgets built with <scm|refresh>,
  <scm|refreshable> or <scm|cached> are updated through the
  <cpp|SLOT_REFRESH> message. The kernel sends it to all windows in

  <\cpp-code>
    void

    windows_refresh (string kind) {

    \ \ if (kind == "auto" && texmacs_time () \<less\> refresh_time)
    return;

    \ \ iterator\<less\>int\<gtr\> it= iterate (window_table);

    \ \ while (it-\<gtr\>busy ()) {

    \ \ \ \ int id= it-\<gtr\>next ();

    \ \ \ \ send_refresh (window_table[id], kind);

    \ \ \ \ ...

    \ \ }

    \ \ if (kind == "auto") windows_delayed_refresh (1000000000);

    }
  </cpp-code>

  There are two ways to trigger it:

  <\itemize>
    <item>explicitly, from <scheme>, with <scm|(refresh-now <scm-arg|kind>)>
    (glue for <cpp|windows_refresh>);

    <item>automatically: <cpp|tm_server_rep::interpose_handler> calls
    <cpp|windows_refresh ()> with the default kind <verbatim|"auto"> at
    each pass of the main loop, but this has an effect only after
    <cpp|windows_delayed_refresh (ms)> has been called. This happens in
    particular in <cpp|edit_interface_rep::after_menu_action>, i.e. after
    every command triggered from a menu or a widget.
  </itemize>

  A refresh widget only recomputes itself if its own kind is
  <verbatim|"any"> or equal to the kind of the message, and, as for the
  bars of the main window, only rebuilds its contents when the expansion of
  its <scheme> description changed.

  <\remark>
    <cpp|windows_refresh> iterates over <cpp|window_table>, which only
    contains the auxiliary windows created with <cpp|window_create> and
    friends, not the main editing windows. In the <name|Qt> port, a window
    widget which receives <cpp|SLOT_REFRESH> emits a global <name|Qt> signal
    to which <em|all> refresh widgets are connected, including those in the
    side tools of the main windows; hence these are refreshed as well, but
    only if at least one auxiliary window is open (see \P<hlink|The <name|Qt>
    implementation|widgets-qt.en.tm>\Q). A port which dispatches
    <cpp|SLOT_REFRESH> only to the subwidgets of the receiving window must
    keep this in mind.
  </remark>

  <section|The flow of events and commands>

  <subsection|Keyboard and mouse events>

  Events on the document canvas are delivered to the <cpp|handle_*>
  methods of the <cpp|simple_widget_rep> (that is, to the editor). A port
  should not call these handlers from inside a toolkit callback, because
  they may run arbitrary <scheme> code and modify the document while the
  toolkit is in an inconsistent state. The <name|Qt> port therefore
  <em|queues> events: for example, <cpp|QTMWidget::keyPressEvent> translates
  the <name|Qt> event into a <TeXmacs> key name and calls
  <cpp|the_gui-\<gtr\>process_keypress>, which appends an event to a queue
  and asks for an update. In <cpp|qt_gui_rep::update>, queued events are
  processed one after the other (<cpp|handle_keypress>,
  <cpp|handle_mouse>, ...), then the interpose handler registered with
  <cpp|gui_interpose> is run (it lets the editors apply their changes,
  typeset and update the menus), and finally the invalid regions of all
  canvases are repainted through <cpp|handle_repaint>.

  <subsection|Commands>

  Commands attached to menu entries and other widgets follow a similar
  path. Take a menu entry <scm|("New" (new-document))>:

  <\enumerate>
    <item>the <scheme> interpreter wraps the action into a <cpp|command>
    whose closure calls <scm|exec-delayed> on
    <scm|(protected-call (lambda () (new-document)))> (see <scm|make-menu-command>
    in <verbatim|kernel/gui/menu-widget.scm>);

    <item>when the user activates the entry, the port calls the command
    (in <name|Qt>, <cpp|QTMCommand::apply> queues it with
    <cpp|the_gui-\<gtr\>process_command>);

    <item>the command, run from the main loop, only schedules the action
    with <cpp|exec_delayed>;

    <item>the delayed action is executed by <cpp|protected_call>
    (<verbatim|Scheme/Scheme/object.cpp>), which surrounds it with
    <cpp|before_menu_action> and <cpp|after_menu_action> of the current
    editor (or <cpp|cancel_menu_action> if an exception occurs): the
    editor state is archived for undo, and afterwards
    <cpp|windows_delayed_refresh (1)> requests a refresh of the dynamic
    widgets;

    <item>the resulting document changes set flags in the editor, so that
    the next <cpp|apply_changes> retypesets the document and, if needed,
    calls <cpp|update_menus>.
  </enumerate>

  Input widgets use commands with arguments: their call-backs are invoked
  with <cpp|cmd (list_object (...))> and the <scheme> wrapper
  <scm|menu-protect> also defers the actual call with <scm|exec-delayed>
  and <scm|protected-call>.

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
