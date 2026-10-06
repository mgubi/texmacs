<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The abstract widget interface in <c++>>

  <section|Overview>

  The kernel of <TeXmacs> manipulates widgets exclusively through the
  interface declared in the directory <verbatim|Graphics/Gui>:

  <\description>
    <item*|<source-link|widget.hpp|src/Graphics/Gui/widget.hpp>>The abstract classes <cpp|widget_rep> and
    <cpp|widget>, the style flags <cpp|WIDGET_STYLE_*> and the list of all
    widget constructors that a port has to provide.

    <item*|<source-link|message.hpp|src/Graphics/Gui/message.hpp>>The enumeration <cpp|slot_id> of message
    slots, the class <cpp|slot>, and typed helper functions such as
    <cpp|set_size>, <cpp|send_keyboard_focus> or <cpp|set_main_menu> which
    encode messages into the generic calls of <cpp|widget_rep>.

    <item*|<source-link|gui.hpp|src/Graphics/Gui/gui.hpp>>System-wide routines (opening the <abbr|GUI>,
    running the main loop, clipboard, default fonts, help balloons,
    interruption checks).

    <item*|<source-link|window.hpp|src/Graphics/Gui/window.hpp>>An abstract class <cpp|window_rep> for
    toolkit windows. It is only used by ports which build on
    <name|Widkit>; the <name|Qt> port does not implement it.

    <item*|<source-link|widget.cpp|src/Graphics/Gui/widget.cpp>>The few port-independent parts: connection
    management, the default message handlers, <cpp|slot_name> and
    <cpp|get_default_styled_font>.
  </description>

  A port implements all these declarations; the kernel (in particular
  <verbatim|Texmacs/Window> and <verbatim|Edit/Interface>) only calls them.
  In the same spirit, <c++> code never needs to know which concrete class
  hides behind a <cpp|widget>.

  <section|The class <cpp|widget>>

  <cpp|widget> is a reference-counted pointer to a <cpp|widget_rep>, built
  with the usual <cpp|ABSTRACT_NULL> macros of
  <source-link|Kernel/Abstractions/basic.hpp|src/Kernel/Abstractions/basic.hpp>. A widget can be nil, which is used for instance as
  the \Pno widget\Q value of <cpp|read>. The representation class is:

  <\cpp-code>
    class widget_rep: public abstract_struct {

    protected:

    \ \ list\<less\>widget_connection\<gtr\> in;

    \ \ list\<less\>widget_connection\<gtr\> out;

    public:

    \ \ virtual void send (slot s, blackbox val);

    \ \ virtual blackbox query (slot s, int type_id);

    \ \ virtual widget read (slot s, blackbox index);

    \ \ virtual void write (slot s, blackbox index, widget w);

    \ \ virtual void notify (slot s, blackbox new_val);

    \ \ virtual void connect (slot s, widget w2, slot s2);

    \ \ virtual void deconnect (slot s, widget w2, slot s2);

    \ \ ...

    };
  </cpp-code>

  The five virtual methods form the whole message protocol:

  <\explain>
    <cpp|void send (slot s, blackbox val)><explain-synopsis|set a value or
    trigger an action>
  <|explain>
    Sends the value <cpp|val> to the slot <cpp|s>. This is used both for
    setting state (the size, the title, the visibility, ...) and for
    delivering events (a key press, a request to repaint, a refresh).
  </explain>

  <\explain>
    <cpp|blackbox query (slot s, int type_id)><explain-synopsis|obtain
    information>
  <|explain>
    Returns the value of type <cpp|type_id> stored in the slot <cpp|s>. The
    type identifier is passed so that the implementation can check that the
    caller expects the right type.
  </explain>

  <\explain>
    <cpp|widget read (slot s, blackbox index)><explain-synopsis|access a
    subwidget>
  <|explain>
    Returns the subwidget of kind <cpp|s> at position <cpp|index> (the index
    is often empty). Example: <cpp|get_canvas> reads <cpp|SLOT_CANVAS>,
    <cpp|get_form_field> reads the <cpp|i>-th input of a dialog.
  </explain>

  <\explain>
    <cpp|void write (slot s, blackbox index, widget w)><explain-synopsis|replace
    a subwidget>
  <|explain>
    Installs <cpp|w> as the subwidget of kind <cpp|s>. This is how menus,
    icon bars, side tools and the editor canvas are installed into the main
    window.
  </explain>

  <\explain>
    <cpp|void notify (slot s, blackbox new_val)><explain-synopsis|report a
    state change>
  <|explain>
    Informs the widget that the state variable in slot <cpp|s> changed.
    The default implementation in <source-link|widget.cpp|src/Graphics/Gui/widget.cpp> forwards the value
    to all widgets connected with <cpp|connect>, by calling their
    <cpp|send>.
  </explain>

  The default implementations of <cpp|send>, <cpp|query>, <cpp|read> and
  <cpp|write> in <source-link|widget.cpp|src/Graphics/Gui/widget.cpp> simply fail (<cpp|FAILED ("no default
  implementation")>), so that every concrete widget class must decide which
  slots it understands. Ports usually derive from a common base class which
  provides lenient defaults (the <name|Qt> base class
  <cpp|qt_widget_rep> for instance ignores unknown messages sent to it and
  returns neutral values for common queries).

  The connection mechanism (<cpp|connect>, <cpp|deconnect> and the lists
  <cpp|in> and <cpp|out> of <cpp|widget_connection>s) is a remnant of a more
  ambitious design; the destructor of <cpp|widget_rep> removes all
  connections, but the current kernel does not rely on connections between
  widgets.

  Because widgets are allocated with the <TeXmacs> fast allocator,
  <source-link|widget.hpp|src/Graphics/Gui/widget.hpp> declares a specialization of
  <cpp|tm_delete\<less\>widget_rep\<gtr\>>, which uses the virtual method
  <cpp|derived_this> to find the start of the most derived object before
  freeing it. Ports with their own base class do the same (see
  <cpp|tm_delete\<less\>qt_widget_rep\<gtr\>> in
  <source-link|Plugins/Qt/qt_widget.cpp|src/Plugins/Qt/qt_widget.cpp>).

  <section|Blackboxes and typed messages>

  The values carried by messages are <cpp|blackbox>es
  (<source-link|Kernel/Abstractions/blackbox.hpp|src/Kernel/Abstractions/blackbox.hpp>): type-erased, reference
  counted containers. <cpp|close_box\<less\>T\<gtr\> (x)> wraps a value,
  <cpp|open_box\<less\>T\<gtr\> (bb)> unwraps it (and asserts that the type
  is right) and <cpp|type_box (bb)> returns the type identifier, which is
  <cpp|type_helper\<less\>T\<gtr\>::id> for a value of type <cpp|T>.
  Messages with several arguments use the tuple types <cpp|pair>,
  <cpp|triple>, <cpp|quartet> and <cpp|quintuple> of
  <source-link|Kernel/Containers/ntuple.hpp|src/Kernel/Containers/ntuple.hpp>.

  Nobody calls <cpp|send> or <cpp|query> with explicit blackboxes. Instead,
  <source-link|message.hpp|src/Graphics/Gui/message.hpp> provides templates which do the packing:

  <\cpp-code>
    template\<less\>class T1, class T2\<gtr\> void

    send (widget w, slot s, T1 val1, T2 val2) {

    \ \ typedef pair\<less\>T1,T2\<gtr\> T;

    \ \ w-\<gtr\>send (s, close_box\<less\>T\<gtr\> (T (val1, val2)));

    }

    \;

    template\<less\>class T1, class T2\<gtr\> void

    query (widget w, slot s, T1& val1, T2& val2) {

    \ \ typedef pair\<less\>T1,T2\<gtr\> T;

    \ \ T p= open_box\<less\>T\<gtr\> (w-\<gtr\>query (s,
    type_helper\<less\>T\<gtr\>::id));

    \ \ val1= p.x1; val2= p.x2;

    }
  </cpp-code>

  and, on top of them, one inline function per message, which is what the
  kernel really uses:

  <\cpp-code>
    inline void

    set_size (widget w, SI width, SI height) {

    \ \ send\<less\>SI,SI\<gtr\> (w, SLOT_SIZE, width, height);

    }
  </cpp-code>

  The receiving side must therefore open the blackbox with exactly the same
  type. The <name|Qt> port provides <cpp|check_type\<less\>T\<gtr\> (val,
  s)>, <cpp|check_type_id\<less\>T\<gtr\> (type_id, s)> and
  <cpp|check_type_void (index, s)> in <source-link|Plugins/Qt/qt_utilities.hpp|src/Plugins/Qt/qt_utilities.hpp>
  to catch mismatches early, and uses the abbreviations
  <cpp|coord2>=<cpp|pair\<less\>SI,SI\<gtr\>> and
  <cpp|coord4>=<cpp|quartet\<less\>SI,SI,SI,SI\<gtr\>>.

  Coordinates and sizes passed in messages are in <cpp|SI> units (see the
  constant <cpp|PIXEL> in <source-link|renderer.hpp|src/Graphics/Renderer/renderer.hpp>), with the conventions of
  the <TeXmacs> renderer.

  <section|Slots>

  A <cpp|slot> is a thin wrapper around the enumeration <cpp|slot_id> of
  <source-link|message.hpp|src/Graphics/Gui/message.hpp>; the function <cpp|slot_name> in
  <source-link|widget.cpp|src/Graphics/Gui/widget.cpp> returns a printable name for debugging. The
  enumeration ends with <cpp|slot_id__LAST>, which is used by some ports to
  size tables indexed by slots (for instance <cpp|sent_slots> in
  <cpp|qt_simple_widget_rep>).

  <\warning>
    The array of names in <cpp|slot_name> must be kept in the same order as
    the enumeration. It currently contains <verbatim|"SLOT_SHRINKING_FACTOR">
    at the position of <cpp|SLOT_ZOOM_FACTOR>, an old name of this slot.
  </warning>

  The slots fall into four groups. In the following tables, \Psend\Q,
  \Pquery\Q, \Pread\Q, \Pwrite\Q and \Pnotify\Q indicate which method of
  <cpp|widget_rep> is used by the helper functions.

  <subsection|General slots>

  These slots may be understood by any widget, but most of them only make
  sense for window widgets or for canvases.

  <descriptive-table|<tformat|<table|<row|<cell|Slot>|<cell|Helpers>|<cell|Meaning>>|<row|<cell|<cpp|SLOT_IDENTIFIER>>|<cell|<cpp|get_identifier>,
  <cpp|set_identifier>, <cpp|is_attached>>|<cell|query/send <cpp|int>: the
  low-level identifier of the window of a widget, 0 if
  detached>>|<row|<cell|<cpp|SLOT_WINDOW>>|<cell|<cpp|get_window>>|<cell|read:
  the top-level window widget containing the
  widget>>|<row|<cell|<cpp|SLOT_VISIBILITY>>|<cell|<cpp|set_visibility>>|<cell|send
  <cpp|bool>: map or unmap a
  window>>|<row|<cell|<cpp|SLOT_FULL_SCREEN>>|<cell|<cpp|set_full_screen>>|<cell|send
  <cpp|bool>>>|<row|<cell|<cpp|SLOT_NAME>>|<cell|<cpp|set_name>>|<cell|send
  <cpp|string>: the window
  title>>|<row|<cell|<cpp|SLOT_MODIFIED>>|<cell|<cpp|set_modified>>|<cell|send
  <cpp|bool>: decorate the title of a modified
  document>>|<row|<cell|<cpp|SLOT_SIZE>>|<cell|<cpp|set_size>,
  <cpp|get_size>, <cpp|notify_size>>|<cell|two
  <cpp|SI>>>|<row|<cell|<cpp|SLOT_POSITION>>|<cell|<cpp|set_position>,
  <cpp|get_position>, <cpp|notify_position>>|<cell|two <cpp|SI>, relative
  to the parent>>|<row|<cell|<cpp|SLOT_UPDATE>>|<cell|<cpp|send_update>>|<cell|geometry
  may have to be recomputed>>|<row|<cell|<cpp|SLOT_REFRESH>>|<cell|<cpp|send_refresh>>|<cell|send
  <cpp|string> <em|kind>: recompute dynamic
  subwidgets>>|<row|<cell|<cpp|SLOT_KEYBOARD>>|<cell|<cpp|send_keyboard>>|<cell|a
  key press (<cpp|string>, <cpp|time_t>)>>|<row|<cell|<cpp|SLOT_KEYBOARD_FOCUS>>|<cell|<cpp|send_keyboard_focus>,
  <cpp|notify_keyboard_focus>, <cpp|query_keyboard_focus>>|<cell|obtain or
  test the keyboard focus>>|<row|<cell|<cpp|SLOT_KEYBOARD_FOCUS_ON>>|<cell|<cpp|send_keyboard_focus_on>>|<cell|focus
  a named field inside a widget>>|<row|<cell|<cpp|SLOT_MOUSE>>|<cell|<cpp|send_mouse>>|<cell|a
  mouse event (kind, position, modifiers,
  time)>>|<row|<cell|<cpp|SLOT_MOUSE_GRAB>>|<cell|<cpp|send_mouse_grab>,
  <cpp|notify_mouse_grab>, <cpp|query_mouse_grab>>|<cell|obtain or test the
  mouse grab>>|<row|<cell|<cpp|SLOT_MOUSE_POINTER>>|<cell|<cpp|send_mouse_pointer>>|<cell|change
  the shape of the pointer>>|<row|<cell|<cpp|SLOT_INVALIDATE>,
  <cpp|SLOT_INVALIDATE_ALL>>|<cell|<cpp|send_invalidate>,
  <cpp|send_invalidate_all>>|<cell|schedule
  repainting>>|<row|<cell|<cpp|SLOT_INVALID>>|<cell|<cpp|query_invalid>>|<cell|are
  there pending regions to
  repaint?>>|<row|<cell|<cpp|SLOT_REPAINT>>|<cell|<cpp|send_repaint>>|<cell|repaint
  a region on a given <cpp|renderer>>>|<row|<cell|<cpp|SLOT_DELAYED_MESSAGE>>|<cell|<cpp|send_delayed_message>>|<cell|deliver
  a message after a delay>>|<row|<cell|<cpp|SLOT_DESTROY>>|<cell|<cpp|send_destroy>>|<cell|the
  widget is about to be destroyed>>>>>

  <subsection|Canvas slots>

  A <em|canvas> is a scrollable area. The main window and the editor
  widgets understand these slots.

  <descriptive-table|<tformat|<table|<row|<cell|Slot>|<cell|Helpers>|<cell|Meaning>>|<row|<cell|<cpp|SLOT_ZOOM_FACTOR>>|<cell|<cpp|set_zoom_factor>>|<cell|send
  <cpp|double>>>|<row|<cell|<cpp|SLOT_EXTENTS>>|<cell|<cpp|set_extents>,
  <cpp|get_extents>>|<cell|four <cpp|SI>: the size of the scrollable
  contents>>|<row|<cell|<cpp|SLOT_VISIBLE_PART>>|<cell|<cpp|get_visible_part>>|<cell|query
  four <cpp|SI>>>|<row|<cell|<cpp|SLOT_SCROLLBARS_VISIBILITY>>|<cell|<cpp|set_scrollbars_visibility>>|<cell|send
  <cpp|int>>>|<row|<cell|<cpp|SLOT_SCROLL_POSITION>>|<cell|<cpp|set_scroll_position>,
  <cpp|get_scroll_position>>|<cell|two
  <cpp|SI>>>|<row|<cell|<cpp|SLOT_CANVAS>>|<cell|<cpp|get_canvas>>|<cell|read:
  the canvas itself>>|<row|<cell|<cpp|SLOT_SCROLLABLE>>|<cell|<cpp|set_scrollable>>|<cell|write:
  install the widget shown in the canvas (the
  editor)>>|<row|<cell|<cpp|SLOT_CURSOR>>|<cell|<cpp|send_cursor>>|<cell|current
  cursor position (used by input methods)>>>>>

  <subsection|Slots of the main <TeXmacs> widget>

  These slots are specific to the widget created by <cpp|texmacs_widget>
  (see \P<hlink|Windows, the main <TeXmacs> widget and the flow of
  events|widgets-window.en.tm>\Q). Each bar comes with a visibility slot
  (send and query a <cpp|bool>) and a contents slot (write a widget).

  <descriptive-table|<tformat|<table|<row|<cell|Slot>|<cell|Helpers>|<cell|Meaning>>|<row|<cell|<cpp|SLOT_HEADER_VISIBILITY>>|<cell|<cpp|set_header_visibility>,
  <cpp|get_header_visibility>>|<cell|all menus and icon
  bars>>|<row|<cell|<cpp|SLOT_MAIN_MENU>>|<cell|<cpp|set_main_menu>>|<cell|the
  menu bar>>|<row|<cell|<cpp|SLOT_MAIN_ICONS>, <cpp|SLOT_MODE_ICONS>,
  <cpp|SLOT_FOCUS_ICONS>, <cpp|SLOT_USER_ICONS>>|<cell|<cpp|set_main_icons>,
  <cpp|set_mode_icons>, <cpp|set_focus_icons>,
  <cpp|set_user_icons>>|<cell|the four icon
  bars>>|<row|<cell|<cpp|SLOT_SIDE_TOOLS>,
  <cpp|SLOT_LEFT_TOOLS>>|<cell|<cpp|set_side_tools>,
  <cpp|set_left_tools>>|<cell|right and left side
  tools>>|<row|<cell|<cpp|SLOT_BOTTOM_TOOLS>,
  <cpp|SLOT_EXTRA_TOOLS>>|<cell|<cpp|set_bottom_tools>,
  <cpp|set_extra_tools>>|<cell|tools below the
  document>>|<row|<cell|<cpp|SLOT_*_VISIBILITY>>|<cell|<cpp|set_*_visibility>,
  <cpp|get_*_visibility>>|<cell|one for each bar above and for the
  footer>>|<row|<cell|<cpp|SLOT_LEFT_FOOTER>,
  <cpp|SLOT_RIGHT_FOOTER>>|<cell|<cpp|set_left_footer>,
  <cpp|set_right_footer>>|<cell|send <cpp|string>: status
  messages>>|<row|<cell|<cpp|SLOT_INTERACTIVE_MODE>>|<cell|<cpp|set_interactive_mode>,
  <cpp|get_interactive_mode>>|<cell|footer used for
  input>>|<row|<cell|<cpp|SLOT_INTERACTIVE_PROMPT>>|<cell|<cpp|set_interactive_prompt>>|<cell|write:
  the prompt>>|<row|<cell|<cpp|SLOT_INTERACTIVE_INPUT>>|<cell|<cpp|set_interactive_input>,
  <cpp|get_interactive_input>>|<cell|write the input widget, query the
  answer>>>>>

  <subsection|Dialog slots>

  <descriptive-table|<tformat|<table|<row|<cell|Slot>|<cell|Helpers>|<cell|Meaning>>|<row|<cell|<cpp|SLOT_FORM_FIELD>>|<cell|<cpp|get_form_field>>|<cell|read:
  the <cpp|i>-th field of an <cpp|inputs_list_widget>>>|<row|<cell|<cpp|SLOT_STRING_INPUT>>|<cell|<cpp|get_string_input>,
  <cpp|set_string_input>>|<cell|the text of an input
  field>>|<row|<cell|<cpp|SLOT_INPUT_TYPE>>|<cell|<cpp|set_input_type>>|<cell|the
  type of an input field (<verbatim|"string">, <verbatim|"password">,
  ...)>>|<row|<cell|<cpp|SLOT_INPUT_PROPOSAL>>|<cell|<cpp|add_input_proposal>>|<cell|add
  a proposal (history)>>|<row|<cell|<cpp|SLOT_FILE>,
  <cpp|SLOT_DIRECTORY>>|<cell|<cpp|set_file>, <cpp|get_file>,
  <cpp|set_directory>, <cpp|get_directory>>|<cell|file chooser: send a
  <cpp|string>, read the input widget>>>>>

  <section|Commands and promises>

  Widgets receive behaviour from the kernel through two kinds of closures.

  <paragraph|Commands.>A <cpp|command> (<source-link|Kernel/Abstractions/command.hpp|src/Kernel/Abstractions/command.hpp>)
  is a reference-counted pointer to a <cpp|command_rep> with the virtual
  methods <cpp|apply ()> and <cpp|apply (object args)>. Commands can be
  built from a plain function pointer, from a callback with two
  <cpp|void*> arguments, or, most often, by deriving a small class:

  <\cpp-code>
    class ia_command_rep: public command_rep {

    \ \ tm_window_rep* win;

    public:

    \ \ ia_command_rep (tm_window_rep* win2): win (win2) {}

    \ \ void apply () { win-\<gtr\>interactive_return (); }

    \ \ tm_ostream& print (tm_ostream& out) { return out \<less\>\<less\>
    "\<less\>command ia\<gtr\>"; }

    };
  </cpp-code>

  (from <source-link|Texmacs/Window/tm_window.cpp|src/Texmacs/Window/tm_window.cpp>). Commands built from
  <scheme> closures are instances of <cpp|object_command_rep>
  (<source-link|Scheme/Scheme/object.cpp|src/Scheme/Scheme/object.cpp>), created by <cpp|as_command (object)>
  and exported to <scheme> as <scm|object-\<gtr\>command>; their
  <cpp|apply (object args)> calls the closure with the elements of the list
  <cpp|args>. This is how input widgets pass their result: the <name|Qt> port
  calls <cpp|cmd (list_object (...))> with the text, the state of a toggle
  or the selected items. From <scheme>, <scm|command-eval> and
  <scm|command-apply> invoke a command.

  <paragraph|Promises.>A <cpp|promise\<less\>T\<gtr\>>
  (<source-link|Kernel/Containers/promise.hpp|src/Kernel/Containers/promise.hpp>) is a delayed computation of a
  value of type <cpp|T>: its representation has one virtual method
  <cpp|T eval ()>, and <cpp|p ()> evaluates it. Widget promises are used for
  the contents of submenus, which are only computed when the submenu is
  about to be shown (<cpp|pulldown_button> and <cpp|pullright_button>). The
  <scheme> closure is wrapped by <cpp|as_promise_widget>, exported as
  <scm|object-\<gtr\>promise-widget>, whose <cpp|eval> calls the closure and
  checks that it returned a widget. Evaluating the promise <em|again> each
  time the menu opens is what makes such menus dynamic, so ports must not
  cache the result.

  Since these closures may execute arbitrary <scheme> code, ports never call
  them directly from inside a toolkit callback: they queue them and run them
  from the main loop (see the section on events in \P<hlink|Windows, the
  main <TeXmacs> widget and the flow of events|widgets-window.en.tm>\Q).

  <section|The widget constructors>

  Every widget is created by one of the global functions declared in
  <source-link|widget.hpp|src/Graphics/Gui/widget.hpp>. The kernel calls some of them directly; most of
  them are exported to <scheme> in
  <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm> under the names listed below,
  and used by the interpreter of <source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>. The
  third column gives the keyword of the <scheme> widget language that
  produces them.

  <subsection|Window and top-level widgets>

  <descriptive-table|<tformat|<table|<row|<cell|<c++>>|<cell|<scheme>
  glue>|<cell|Used for>>|<row|<cell|<cpp|plain_window_widget (w, name,
  quit)>>|<cell|(via <scm|alt-window-create-quit>)>|<cell|decorated
  window>>|<row|<cell|<cpp|popup_window_widget (w,
  name)>>|<cell|(via <scm|alt-window-create-popup>)>|<cell|undecorated
  window>>|<row|<cell|<cpp|tooltip_window_widget (w,
  name)>>|<cell|(via <scm|alt-window-create-tooltip>)>|<cell|tooltip
  window>>|<row|<cell|<cpp|destroy_window_widget
  (w)>>|<cell|>|<cell|destroy a window
  widget>>|<row|<cell|<cpp|texmacs_widget (mask,
  quit)>>|<cell|>|<cell|the main window
  contents>>|<row|<cell|<cpp|popup_widget (w)>>|<cell|>|<cell|contextual
  popup menu>>|<row|<cell|<cpp|file_chooser_widget (cmd, type,
  prompt)>>|<cell|>|<cell|file
  dialog>>|<row|<cell|<cpp|inputs_list_widget (cmd,
  prompts)>>|<cell|>|<cell|dialog with several
  inputs>>|<row|<cell|<cpp|printer_widget (cmd,
  file)>>|<cell|<scm|widget-printer>>|<cell|print
  dialog>>|<row|<cell|<cpp|color_picker_widget (cmd, bg,
  proposals)>>|<cell|<scm|widget-color-picker>>|<cell|<scm|color-input>>>>>>

  <subsection|Menus and buttons>

  <descriptive-table|<tformat|<table|<row|<cell|<c++>>|<cell|<scheme>
  glue>|<cell|Keyword>>|<row|<cell|<cpp|horizontal_menu
  (a)>>|<cell|<scm|widget-hmenu>>|<cell|<scm|horizontal>>>|<row|<cell|<cpp|vertical_menu
  (a)>>|<cell|<scm|widget-vmenu>>|<cell|<scm|vertical>>>|<row|<cell|<cpp|tile_menu
  (a, cols)>>|<cell|<scm|widget-tmenu>>|<cell|<scm|tile>>>|<row|<cell|<cpp|minibar_menu
  (a)>>|<cell|<scm|widget-minibar-menu>>|<cell|<scm|minibar>>>|<row|<cell|<cpp|menu_separator
  (vertical)>>|<cell|<scm|widget-separator>>|<cell|<scm|--->,
  <scm|/>>>|<row|<cell|<cpp|menu_group (name,
  style)>>|<cell|<scm|widget-menu-group>>|<cell|<scm|group>>>|<row|<cell|<cpp|pulldown_button
  (w, pw)>>|<cell|<scm|widget-pulldown-button>>|<cell|<scm|=\<gtr\>>>>|<row|<cell|<cpp|pullright_button
  (w, pw)>>|<cell|<scm|widget-pullright-button>>|<cell|<scm|-\<gtr\>>>>|<row|<cell|<cpp|menu_button
  (w, cmd, pre, ks, style)>>|<cell|<scm|widget-menu-button>>|<cell|<scm|("label"
  action)>>>|<row|<cell|<cpp|balloon_widget (w,
  help)>>|<cell|<scm|widget-balloon>>|<cell|<scm|balloon>>>|<row|<cell|<cpp|text_widget
  (s, style, col, tsp)>>|<cell|<scm|widget-text>>|<cell|<scm|text>,
  labels>>|<row|<cell|<cpp|xpm_widget (file)>>|<cell|<scm|widget-xpm>>|<cell|<scm|icon>>>>>>

  In <cpp|menu_button>, <cpp|pre> is a check mark prefix
  (<verbatim|"v">, <verbatim|"*">, <verbatim|"o"> or empty) and <cpp|ks>
  the keyboard shortcut shown next to the entry. The style flags are
  described below.

  <subsection|Layout and containers>

  <descriptive-table|<tformat|<table|<row|<cell|<c++>>|<cell|<scheme>
  glue>|<cell|Keyword>>|<row|<cell|<cpp|horizontal_list
  (a)>>|<cell|<scm|widget-hlist>>|<cell|<scm|hlist>>>|<row|<cell|<cpp|vertical_list
  (a)>>|<cell|<scm|widget-vlist>>|<cell|<scm|vlist>>>|<row|<cell|<cpp|glue_widget
  (hx, vx, w, h)>>|<cell|<scm|widget-glue>>|<cell|<scm|glue>, <scm|===>,
  <scm|\<gtr\>\<gtr\>>, ...>>|<row|<cell|<cpp|glue_widget (col, hx, vx,
  w, h)>>|<cell|<scm|widget-color>>|<cell|<scm|color>>>|<row|<cell|<cpp|division_widget
  (name, w)>>|<cell|<scm|widget-division>>|<cell|<scm|division>,
  <scm|class>>>|<row|<cell|<cpp|aligned_widget (lhs,
  rhs)>>|<cell|<scm|widget-aligned>>|<cell|<scm|aligned>,
  <scm|item>>>|<row|<cell|<cpp|tabs_widget (tabs,
  bodies)>>|<cell|<scm|widget-tabs>>|<cell|<scm|tabs>,
  <scm|tab>>>|<row|<cell|<cpp|icon_tabs_widget (us, ts,
  bs)>>|<cell|<scm|widget-icon-tabs>>|<cell|<scm|icon-tabs>>>|<row|<cell|<cpp|responsive_tabs_widget>,
  <cpp|responsive_icon_tabs_widget>>|<cell|<scm|widget-responsive-tabs>,
  <scm|widget-responsive-icon-tabs>>|<cell|<scm|responsive-tabs>,
  <scm|responsive-icon-tabs>>>|<row|<cell|<cpp|user_canvas_widget (w,
  style)>>|<cell|<scm|widget-scrollable>>|<cell|<scm|scrollable>>>|<row|<cell|<cpp|resize_widget
  (w, style, ...)>>|<cell|<scm|widget-resize>>|<cell|<scm|resize>>>|<row|<cell|<cpp|hsplit_widget>,
  <cpp|vsplit_widget>>|<cell|<scm|widget-hsplit>,
  <scm|widget-vsplit>>|<cell|<scm|hsplit>,
  <scm|vsplit>>>|<row|<cell|<cpp|extend_widget (w,
  a)>>|<cell|<scm|widget-extend>>|<cell|<scm|extend>>>|<row|<cell|<cpp|wrapped_widget
  (w, quit)>>|<cell|>|<cell|run <cpp|quit> on
  destruction>>|<row|<cell|<cpp|empty_widget
  ()>>|<cell|<scm|widget-empty>>|<cell|>>>>>

  <subsection|Input widgets>

  <descriptive-table|<tformat|<table|<row|<cell|<c++>>|<cell|<scheme>
  glue>|<cell|Keyword>>|<row|<cell|<cpp|input_text_widget (cb, type, def,
  style, width)>>|<cell|<scm|widget-input>>|<cell|<scm|input>>>|<row|<cell|<cpp|toggle_widget
  (cmd, on, style)>>|<cell|<scm|widget-toggle>>|<cell|<scm|toggle>>>|<row|<cell|<cpp|setting_toggle_widget>>|<cell|<scm|widget-setting-toggle>>|<cell|<scm|setting-toggle>>>|<row|<cell|<cpp|enum_widget
  (cb, vals, val, style, w)>>|<cell|<scm|widget-enum>>|<cell|<scm|enum>>>|<row|<cell|<cpp|setting_enum_widget>>|<cell|<scm|widget-setting-enum>>|<cell|<scm|setting-enum>>>|<row|<cell|<cpp|setting_group_widget>>|<cell|<scm|widget-setting-group>>|<cell|<scm|setting-group>>>|<row|<cell|<cpp|choice_widget>
  (three overloads)>|<cell|<scm|widget-choice>, <scm|widget-choices>,
  <scm|widget-filtered-choice>>|<cell|<scm|choice>, <scm|choices>,
  <scm|filtered-choice>>>|<row|<cell|<cpp|tree_view_widget (cmd, data,
  roles)>>|<cell|<scm|widget-tree-view>>|<cell|<scm|tree-view>>>|<row|<cell|<cpp|ink_widget
  (cb)>>|<cell|<scm|widget-ink>>|<cell|<scm|ink>>>>>>

  <subsection|Dynamic widgets>

  <descriptive-table|<tformat|<table|<row|<cell|<c++>>|<cell|<scheme>
  glue>|<cell|Keyword>>|<row|<cell|<cpp|refresh_widget (tmwid,
  kind)>>|<cell|<scm|widget-refresh>>|<cell|<scm|refresh>>>|<row|<cell|<cpp|refreshable_widget
  (prom, kind)>>|<cell|<scm|widget-refreshable>>|<cell|<scm|refreshable>,
  <scm|cached>>>>>>

  A <cpp|refresh_widget> is built from the <em|name> of a <scheme> widget;
  a <cpp|refreshable_widget> from a <scheme> closure returning a widget.
  Both rebuild their contents when they receive <cpp|SLOT_REFRESH> with a
  matching <em|kind>. See \P<hlink|The <scheme> widget language and its
  interpreter|widgets-scheme.en.tm>\Q for their use.

  <subsection|Widgets implemented by the kernel>

  A few constructors are not provided by the ports but by the kernel, in
  terms of the others: <cpp|texmacs_output_widget (doc, style)>
  (<source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp>, glue
  <scm|widget-texmacs-output>) typesets a document into a box and shows it
  in a <cpp|box_widget_rep>; <cpp|texmacs_input_widget (doc, style, name)>
  (<source-link|Texmacs/Window/tm_window.cpp|src/Texmacs/Window/tm_window.cpp>, glue <scm|widget-texmacs-input>)
  creates a hidden buffer and embeds a complete editor in the widget; the
  two <cpp|box_widget> functions declared in
  <source-link|Texmacs/tm_frame.hpp|src/Texmacs/tm_frame.hpp> (the second one is glued as
  <scm|widget-box>) also return a <cpp|box_widget_rep>, a class of
  <source-link|tm_button.cpp|src/Texmacs/Window/tm_button.cpp> derived from <cpp|simple_widget_rep> which
  paints a typeset box.

  <section|Style flags>

  Many constructors take an <cpp|int style>, a combination of:

  <descriptive-table|<tformat|<table|<row|<cell|Flag>|<cell|Value>|<cell|Meaning>>|<row|<cell|<cpp|WIDGET_STYLE_MINI>>|<cell|1>|<cell|smaller
  font>>|<row|<cell|<cpp|WIDGET_STYLE_MONOSPACED>>|<cell|2>|<cell|monospaced
  font>>|<row|<cell|<cpp|WIDGET_STYLE_GREY>>|<cell|4>|<cell|greyed
  text>>|<row|<cell|<cpp|WIDGET_STYLE_PRESSED>>|<cell|8>|<cell|button shown
  as pressed>>|<row|<cell|<cpp|WIDGET_STYLE_INERT>>|<cell|16>|<cell|no
  action (disabled)>>|<row|<cell|<cpp|WIDGET_STYLE_BUTTON>>|<cell|32>|<cell|render
  explicitly as a button>>|<row|<cell|<cpp|WIDGET_STYLE_CENTERED>>|<cell|64>|<cell|centered
  text>>|<row|<cell|<cpp|WIDGET_STYLE_BOLD>>|<cell|128>|<cell|bold
  text>>>>>

  The <scheme> constants <scm|widget-style-mini>, ...,
  <scm|widget-style-bold> of <source-link|kernel/gui/gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm> have the
  same values. The additional <scheme> constant <scm|widget-style-verb>
  (256) is only interpreted on the <scheme> side (it suppresses the
  translation of the entries of an <scm|enum>). The function
  <cpp|get_default_styled_font> of <source-link|widget.cpp|src/Graphics/Gui/widget.cpp> maps a style to one
  of the default fonts.

  <section|The <cpp|simple_widget_rep> contract>

  Besides the constructors, a port must provide a class
  <cpp|simple_widget_rep>, from which the kernel derives the editor
  (<cpp|editor_rep> in <source-link|Edit/editor.hpp|src/Edit/editor.hpp>) and the box widgets of
  <source-link|tm_button.cpp|src/Texmacs/Window/tm_button.cpp>. It represents a canvas that the kernel paints
  itself with a <cpp|renderer>, and which receives raw events. The virtual
  methods the kernel overrides are listed in a comment at the end of
  <source-link|widget.hpp|src/Graphics/Gui/widget.hpp>:

  <\cpp-code>
    bool is_editor_widget ();

    bool is_embedded_widget ();

    void handle_get_size_hint (SI& w, SI& h);

    void handle_notify_resize (SI w, SI h);

    void handle_keypress (string key, time_t t);

    void handle_keyboard_focus (bool new_focus, time_t t);

    void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
    array\<less\>double\<gtr\> data);

    void handle_set_zoom_factor (double zoom);

    void handle_clear (renderer ren, SI x1, SI y1, SI x2, SI y2);

    void handle_repaint (renderer ren, SI x1, SI y1, SI x2, SI y2);
  </cpp-code>

  Which header defines <cpp|simple_widget_rep> is chosen at compile time:
  <source-link|Edit/editor.hpp|src/Edit/editor.hpp> and <source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp>
  include <source-link|Qt/qt_simple_widget.hpp|src/Plugins/Qt/qt_simple_widget.hpp> when <cpp|QTTEXMACS> is
  defined, <source-link|Cocoa/aqua_simple_widget.h|src/Plugins/Cocoa/aqua_simple_widget.h> when <cpp|AQUATEXMACS> is
  defined, and <source-link|Widkit/simple_wk_widget.hpp|src/Plugins/Widkit/simple_wk_widget.hpp> otherwise. The
  <name|Qt> header simply ends with <cpp|typedef qt_simple_widget_rep
  simple_widget_rep>.

  <section|System-wide routines>

  <source-link|gui.hpp|src/Graphics/Gui/gui.hpp> declares the routines that do not concern a particular
  widget. The main ones are <cpp|gui_open>, <cpp|gui_start_loop> and
  <cpp|gui_close> (called from <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>),
  <cpp|gui_interpose>, which registers the function that the main loop must
  call regularly (the server registers
  <cpp|texmacs_interpose_handler>), <cpp|needs_update> and
  <cpp|check_event>, which coordinate the typesetter with pending events,
  <cpp|gui_refresh> (for instance after a change of the interface
  language), the clipboard functions <cpp|set_selection>,
  <cpp|get_selection> and <cpp|clear_selection>, and
  <cpp|show_help_balloon> and <cpp|show_wait_indicator>. The complete list,
  with the obligations of a port, is given in \P<hlink|Adding new widgets
  and porting to other toolkits|widgets-port.en.tm>\Q.

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
