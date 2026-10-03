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

  A <TeXmacs> editing window is an instance of <cpp|tm_window_rep>
  (<verbatim|Texmacs/tm_window.hpp>), which holds two widgets: the window
  widget <cpp|win> (the result of <cpp|plain_window_widget>) and, inside
  it, the <em|main <TeXmacs> widget> <cpp|wid>, created by

  <\cpp-code>
    widget texmacs_widget (int mask, command quit);
  </cpp-code>

  The main widget is implemented by each port (in <name|Qt> by
  <cpp|qt_tm_widget_rep>) and contains the menu bar, the four icon bars,
  the left and right side tools, the canvas, the bottom and extra tools
  and the footer. The bits of <cpp|mask> say which parts are initially
  visible:

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

  A mask of zero, used for embedded editors, asks the port for a
  stripped-down main widget without any bars (in <name|Qt> a
  <cpp|qt_tm_embedded_widget_rep>).

  The main widget understands a large set of slots, sent by the methods of
  <cpp|tm_window_rep>: <cpp|SLOT_SCROLLABLE> (the canvas, whose contents
  is the editor of the view shown in the window), <cpp|SLOT_MAIN_MENU>,
  <cpp|SLOT_MAIN_ICONS>, ..., <cpp|SLOT_SIDE_TOOLS>,
  <cpp|SLOT_BOTTOM_TOOLS>, the visibility slots of the bars,
  <cpp|SLOT_LEFT_FOOTER> and <cpp|SLOT_RIGHT_FOOTER>, the slots of the
  interactive prompt, the zoom factor, the extents and the scroll
  position of the canvas, and <cpp|SLOT_FULL_SCREEN>. The editor itself
  is a <cpp|simple_widget_rep>; once it is installed as the canvas, it
  keeps a pointer <cpp|cvw> to the main widget in order to send messages
  back to it (<cpp|get_canvas>, <cpp|set_extents>,
  <cpp|send_keyboard_focus_on>, ...).

  How windows are created and closed, how views are attached to them, how
  the menus and icon bars are built from <scheme> and cached
  (<cpp|tm_window_rep::get_menu_widget>), and how the footer is used for
  interactive input is described in <hlink|windows|server-windows.en.tm>,
  in the chapter on the server. The present chapter is only concerned with
  the widgets involved.

  <section|Dialogs>

  <subsection|Dialogs built by the kernel>

  <verbatim|Texmacs/Window/tm_dialogue.cpp> builds two kinds of dialogs
  directly in <c++>: file choosers (<cpp|tm_frame_rep::choose_file>, glue
  <scm|cpp-choose-file>), from a <cpp|file_chooser_widget> initialized with
  <cpp|set_directory> and <cpp|set_file>, and the forms which ask for the
  arguments of an interactive command (<cpp|tm_frame_rep::interactive>,
  glue <scm|tm-interactive>), from an <cpp|inputs_list_widget> whose fields
  are configured with <cpp|set_input_type>, <cpp|set_string_input> and
  <cpp|add_input_proposal> and read with <cpp|get_form_field>.
  <cpp|dialogue_start> wraps the dialog into a <cpp|plain_window_widget>,
  centers it on the current window and shows it. The details, including
  when the footer is used instead of a dialog, are in <hlink|dialogs and
  interactive commands|server-windows.en.tm>.

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

  <cpp|texmacs_input_widget> embeds a full editor in a dialog or a side
  panel: it creates an auxiliary buffer, a passive view on it and an
  anonymous <cpp|tm_window_rep> whose main widget is built with a mask of
  zero, installs the editor as the canvas with <cpp|set_scrollable> as for
  an ordinary window, and returns the main widget wrapped by
  <cpp|wrapped_widget>, whose command (<cpp|close_embedded_command>) is
  executed on <cpp|SLOT_DESTROY> and closes the auxiliary buffer.
  <cpp|texmacs_output_widget> (<verbatim|tm_button.cpp>) is much lighter:
  it typesets the document once into a box and returns a
  <cpp|box_widget_rep>, a read-only <cpp|simple_widget_rep> which paints
  the box. Both are described in more detail in <hlink|embedded <TeXmacs>
  widgets|server-windows.en.tm>.

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
