<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <name|Qt> implementation>

  <section|Overview>

  The <name|Qt> port lives in <verbatim|src/src/Plugins/Qt>. A second copy
  with the same structure, <verbatim|Plugins/Qt6>, is used by the
  <verbatim|configure> build when the option <verbatim|--enable-qt-new> is
  given (it is the default on Android); it adds for instance
  <cpp|QTMMainTabWindow>. The <name|CMake> build always uses
  <verbatim|Plugins/Qt>. Both variants support <name|Qt> 5 and 6 through
  <cpp|QT_VERSION> tests. Changes to the widget layer must usually be made
  in both directories. The description below refers to
  <verbatim|Plugins/Qt>.

  The files relevant for widgets are:

  <\description-paragraphs>
    <item*|<verbatim|qt_widget.hpp/cpp>>the base class
    <cpp|qt_widget_rep> and the implementation of all the constructors of
    <verbatim|widget.hpp>;

    <item*|<verbatim|qt_ui_element.hpp/cpp>>the class
    <cpp|qt_ui_element_rep>, which implements most constructors;

    <item*|<verbatim|qt_window_widget.hpp/cpp>>windows
    (<cpp|qt_window_widget_rep>) and popups
    (<cpp|qt_popup_widget_rep>);

    <item*|<verbatim|qt_tm_widget.hpp/cpp>>the main <TeXmacs> widget
    (<cpp|qt_tm_widget_rep>) and its embedded variant
    (<cpp|qt_tm_embedded_widget_rep>);

    <item*|<verbatim|qt_simple_widget.hpp/cpp>>the canvas class
    <cpp|qt_simple_widget_rep> (alias <cpp|simple_widget_rep>), with the
    <name|Qt> widgets <cpp|QTMWidget> and <cpp|QTMScrollView>;

    <item*|<verbatim|qt_menu.hpp/cpp>>contextual menus
    (<cpp|qt_menu_rep>);

    <item*|<verbatim|qt_dialogues.hpp/cpp>, <verbatim|qt_chooser_widget.*>,
    <verbatim|qt_color_picker_widget.*>,
    <verbatim|qt_printer_widget.*>>input fields and dialogs;

    <item*|<verbatim|QTMMenuHelper.hpp/cpp>><name|Qt> helper classes:
    <cpp|QTMCommand>, <cpp|QTMLazyMenu>, <cpp|QTMAction>,
    <cpp|QTMLineEdit>, <cpp|QTMRefreshWidget>,
    <cpp|QTMRefreshableWidget>, ...;

    <item*|<verbatim|qt_gui.hpp/cpp>, <verbatim|QTMGuiHelper.*>>the
    <cpp|qt_gui_rep> singleton <cpp|the_gui>, the event queue and the
    functions of <verbatim|gui.hpp>.
  </description-paragraphs>

  <section|The base class <cpp|qt_widget_rep>>

  All widgets of the port derive from <cpp|qt_widget_rep>, itself derived
  from <cpp|widget_rep>. It holds

  <\itemize>
    <item><cpp|array\<less\>widget\<gtr\> children>, the subwidgets, kept
    alive by the reference counting of <cpp|widget> (filled with
    <cpp|add_child> and <cpp|add_children>);

    <item><cpp|QPointer\<less\>QWidget\<gtr\> qwid>, a guarded pointer to
    the last <name|Qt> widget created for it (if any);

    <item><cpp|types type>, a tag taken from an enumeration with one value
    per kind of widget (<cpp|horizontal_menu>, <cpp|text_widget>,
    <cpp|texmacs_widget>, ...), mirrored by strings in
    <cpp|type_as_string> for debugging;

    <item>a serial number <cpp|id>.
  </itemize>

  The casts <cpp|concrete (widget)> and <cpp|abstract (qt_widget)> convert
  between abstract and <name|Qt> widgets.

  <subsection|Four ways of becoming a <name|Qt> object>

  A <TeXmacs> widget can end up in very different places: in a menu bar,
  in a menu, in a toolbar, in a layout, in a window. <name|Qt> uses
  different classes for these, so <cpp|qt_widget_rep> has four virtual
  methods which <em|create> the corresponding <name|Qt> object on demand:

  <\explain>
    <cpp|QAction* as_qaction ()><explain-synopsis|for menus and toolbars>
  <|explain>
    Returns a new <cpp|QAction>, to be inserted in a <cpp|QMenu>, a menu
    bar or a toolbar. Buttons with submenus return an action carrying a
    lazy menu.
  </explain>

  <\explain>
    <cpp|QWidget* as_qwidget (QWidget* parent)><explain-synopsis|for
    windows and layouts>
  <|explain>
    Returns a new <cpp|QWidget>. The pointer <cpp|qwid> is set to it, but
    ownership goes to the caller (usually through the parent or a layout).
  </explain>

  <\explain>
    <cpp|QLayoutItem* as_qlayoutitem (QWidget* parent)><explain-synopsis|for
    layouts>
  <|explain>
    Returns a layout item: for lists a <cpp|QHBoxLayout> or
    <cpp|QVBoxLayout> filled recursively with the layout items of the
    children, for glue a <cpp|QSpacerItem>, and otherwise the result of
    <cpp|as_qwidget> wrapped in a <cpp|QWidgetItem> (the default).
  </explain>

  <\explain>
    <cpp|QList\<less\>QAction*\<gtr\>* get_qactionlist ()><explain-synopsis|for
    menu contents>
  <|explain>
    For menus and lists, returns the actions of all children
    (<cpp|as_qaction> of each), which is how a <cpp|vertical_menu> fills a
    <cpp|QMenu> and a <cpp|horizontal_menu> fills a menu bar or a
    toolbar.
  </explain>

  The general rule is that each call builds new <name|Qt> objects and gives
  them to the caller. The only <name|Qt> objects owned by <TeXmacs> widgets
  are the windows: a <cpp|qt_window_widget_rep> owns its top-level
  <cpp|QWidget>, which in turn owns (as <name|Qt> parent) all widgets built
  for its contents.

  <subsection|Windows and popups>

  <cpp|qt_widget_rep> also has the virtual methods
  <cpp|plain_window_widget>, <cpp|make_popup_widget>,
  <cpp|popup_window_widget> and <cpp|tooltip_window_widget>, to which the
  global functions of the same name delegate. The default
  <cpp|plain_window_widget> creates a <cpp|QTMPlainWindow>, puts the
  layout item or widget of the contents into it and returns a new
  <cpp|qt_window_widget_rep>; subclasses such as <cpp|qt_tm_widget_rep>
  (which already is a window) or <cpp|qt_inputs_list_widget_rep> override
  it.

  <subsection|Default message handling>

  <cpp|qt_widget_rep::send> handles a few slots valid for any widget
  (<cpp|SLOT_KEYBOARD_FOCUS>, <cpp|SLOT_KEYBOARD_FOCUS_ON>, which looks for
  a child <cpp|QWidget> whose object name is the requested field,
  <cpp|SLOT_NAME>, <cpp|SLOT_DESTROY>, which is forwarded to the children)
  and ignores the others. <cpp|query> returns neutral values for common
  queries (identifier 0, default sizes, invisible bars) and fails
  otherwise; <cpp|read>, <cpp|write> and <cpp|notify> do nothing. When the
  debugging flag <cpp|DEBUG_QT_WIDGETS> is on (debug option
  <verbatim|qt-widgets>), unhandled messages are logged, which is the easiest way to find out
  which slots a widget is expected to support.

  <subsection|Headless mode>

  When <TeXmacs> runs without a display (<cpp|headless_mode>), every
  constructor returns a <cpp|headless_widget ()>, an instance of
  <cpp|qt_headless_widget_rep> which accepts all messages and never creates
  <name|Qt> objects.

  <section|UI elements: widgets as descriptions>

  Most constructors do not create any <name|Qt> object. They create a
  <cpp|qt_ui_element_rep>, which merely stores its type and its arguments
  in a blackbox (the <em|payload>):

  <\cpp-code>
    widget text_widget (string s, int style, color col, bool tsp) {

    \ \ if (headless_mode) return headless_widget ();

    \ \ qt_widget wid = qt_ui_element_rep::create
    (qt_widget_rep::text_widget,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ s,
    style, col, tsp);

    \ \ return abstract (wid);

    }
  </cpp-code>

  The static templates <cpp|qt_ui_element_rep::create> pack one to six
  arguments into <cpp|pair>, <cpp|triple>, ..., <cpp|sextuple> and
  call the constructor <cpp|qt_ui_element_rep (types, blackbox)>. Container
  constructors also register their subwidgets with <cpp|add_children>, so
  that the whole description tree stays alive.

  The actual <name|Qt> objects are created later, in the methods
  <cpp|as_qaction>, <cpp|as_qwidget>, <cpp|as_qlayoutitem> and
  <cpp|get_qactionlist> of <cpp|qt_ui_element_rep>, which are big
  <cpp|switch> statements on <cpp|type>. Each case unpacks the payload with
  the same tuple type that was used for packing, for example:

  <\cpp-code>
    case text_widget:

    {

    \ \ typedef quartet\<less\>string, int, color, bool\<gtr\> T;

    \ \ T x = open_box\<less\>T\<gtr\>(load);

    \ \ QLabel* w = new QLabel(parent_widget);

    \ \ w-\<gtr\>setText (to_qstring (x.x1));

    \ \ ...

    \ \ qt_apply_tm_style (w, x.x2, x.x3);

    \ \ qwid = w;

    }
  </cpp-code>

  The static function <cpp|get_payload> gives access to the payload of
  another UI element; it is used when the rendering of a widget depends on
  its child. For instance a <cpp|menu_button> whose label is an
  <cpp|xpm_widget> becomes a <cpp|QToolButton> with an icon, whereas one
  with a <cpp|text_widget> becomes a <cpp|QPushButton>.

  This lazy design has several advantages. The same description can be
  materialized several times and in different forms (a menu may appear
  both in the menu bar and in a toolbar); building a menu item that is
  never shown costs almost nothing; and dynamic menus are cheap, because
  the description is rebuilt by <scheme> but <name|Qt> objects are only
  created for what is displayed. Accordingly, the results must not be
  cached in general: the comment \PDON'T try to always cache the action
  returned: this breaks dynamic menus!\Q in <cpp|as_qaction> is to be
  taken seriously. The exception is <cpp|get_qactionlist>, which caches its
  list in <cpp|cachedActionList>, since menu bars and toolbars take the
  actions from it.

  A few constructors do not use UI elements because their widgets have
  state or a complex behaviour: <cpp|input_text_widget>
  (<cpp|qt_input_text_widget_rep>), <cpp|inputs_list_widget>
  (<cpp|qt_inputs_list_widget_rep> with <cpp|qt_field_widget_rep>
  fields), <cpp|file_chooser_widget> (<cpp|qt_chooser_widget_rep>),
  <cpp|color_picker_widget> (<cpp|qt_color_picker_widget_rep>),
  <cpp|printer_widget> (<cpp|qt_printer_widget_rep>),
  <cpp|wrapped_widget> (<cpp|qt_wrapped_widget_rep>), the colored
  <cpp|glue_widget> (<cpp|qt_glue_widget_rep>) and <cpp|texmacs_widget>.
  <cpp|empty_widget>, <cpp|ink_widget> and <cpp|wait_widget> are not
  implemented, and <cpp|extend_widget> returns its first argument.

  <section|Menus>

  <paragraph|Lazy submenus.>A <cpp|pulldown_button> or
  <cpp|pullright_button> stores its label and its
  <cpp|promise\<less\>widget\<gtr\>>. Its <cpp|as_qaction> creates the
  action of the label and attaches a <cpp|QTMLazyMenu> to it. This
  <cpp|QMenu> subclass connects its signal <cpp|aboutToShow> to its slot
  <cpp|force>, which evaluates the promise, asks the resulting widget for
  its <cpp|get_qactionlist ()> and moves these actions into itself:

  <\cpp-code>
    void

    QTMLazyMenu::force () {

    BEGIN_SLOT

    \ \ QList\<less\>QAction*\<gtr\>* list = concrete
    (promise_widget())-\<gtr\>get_qactionlist();

    \ \ transferActions (list);

    END_SLOT

    }
  </cpp-code>

  Hence the <scheme> closure behind the promise is called each time the
  menu opens. <cpp|as_qwidget> does the same for buttons inside dialogs and
  toolbars, using a <cpp|QToolButton> or <cpp|QPushButton> with the lazy
  menu.

  <paragraph|Menu entries.>For a <cpp|menu_button>, <cpp|as_qaction>
  returns the action of the label and connects its <cpp|triggered> signal
  to a <cpp|QTMCommand> wrapping the <TeXmacs> command. If the entry has a
  keyboard shortcut, the shortcut is installed on the action (so that it
  is displayed in the menu), and the command is replaced by a
  <cpp|qt_key_command_rep>, which simulates the key press in the editor
  (via <cpp|the_gui-\<gtr\>process_keypress>), so that the keyboard and
  the menu always do the same thing; <cpp|QTMWidget> intercepts the
  shortcut events so that key presses still reach the editor. (When the
  preference <verbatim|use experimental keyboard patches> is on, the
  shortcut is only appended to the text of the action and the original
  command is kept.) The
  check mark prefix and <cpp|WIDGET_STYLE_PRESSED> make the action
  checkable, and <cpp|WIDGET_STYLE_INERT> disables it.

  <paragraph|The menu bar.>The main menu (a <cpp|horizontal_menu> of
  pulldown buttons) is installed by
  <cpp|qt_tm_widget_rep::install_main_menu>, which takes the menus of the
  actions of <cpp|get_qactionlist ()> and adds them to the
  <cpp|QMenuBar> (or to a <cpp|QTMToolbar> when the native menu bar is not
  used). Since <name|Qt> may not replace a menu bar while one of its menus
  is open, a new menu arriving during menu interaction
  (<cpp|menu_count> \<gtr\> 0) is postponed: the widget is appended to
  <cpp|waiting_widgets> and installed by
  <cpp|QTMGuiHelper::doPopWaitingWidgets> when the menu closes.

  <paragraph|Contextual menus.><cpp|qt_ui_element_rep::make_popup_widget>
  turns a <cpp|vertical_menu> into a <cpp|qt_menu_rep>, which owns a
  <cpp|QAction> with a <cpp|QMenu> and shows it when it receives
  <cpp|SLOT_VISIBILITY> or <cpp|SLOT_MOUSE_GRAB>. Other widgets are
  wrapped in a <cpp|qt_popup_widget_rep>.

  <section|Commands and the event queue>

  <cpp|QTMCommand> is a <cpp|QObject> wrapping a <cpp|command>; its slot
  <cpp|apply> does not execute the command but queues it:

  <\cpp-code>
    void

    QTMCommand::apply() \ {

    BEGIN_SLOT

    \ \ if (!is_nil (cmd)) {

    \ \ \ \ the_gui-\<gtr\>process_command (cmd);

    \ \ \ \ ...
  </cpp-code>

  All inputs of the <TeXmacs> kernel go through the queue of
  <cpp|qt_gui_rep>: <cpp|process_keypress>, <cpp|process_keyboard_focus>,
  <cpp|process_mouse> and <cpp|process_resize> (called by <cpp|QTMWidget>)
  and <cpp|process_command> append <cpp|queued_event>s tagged by
  <cpp|qp_type> (<cpp|QP_KEYPRESS>, <cpp|QP_MOUSE>, <cpp|QP_COMMAND>,
  <cpp|QP_COMMAND_ARGS>, ...) and call <cpp|need_update>, which starts
  a zero-delay timer. The timer calls <cpp|qt_gui_rep::update>, which:

  <\enumerate>
    <item>queues the pending delayed commands (<cpp|exec_delayed> of
    <scheme>) when their time has come;

    <item>processes the queued events with <cpp|process_queued_events>,
    dispatching them to the <cpp|handle_*> methods of the target
    <cpp|qt_simple_widget_rep> or applying the commands;

    <item>calls the interpose handler of the server, which lets the editors
    apply their changes and update the menus, and repaints all canvases
    with <cpp|qt_simple_widget_rep::repaint_all>; when only ordinary key
    presses were processed in this pass, these two steps are postponed by a
    few milliseconds, so that fast typing is handled in batches;

    <item>restarts the timer, so that the interpose handler also runs
    periodically when nothing happens.
  </enumerate>

  All <name|Qt> slots of the port are enclosed in the macros
  <cpp|BEGIN_SLOT> and <cpp|END_SLOT>, which catch <TeXmacs> exceptions so
  that they do not propagate through <name|Qt>.

  Input widgets need to pass values to their commands. Instead of
  subclassing <cpp|QTMCommand>, the port wraps the user command in small
  command classes which read the state of the <name|Qt> widget when
  applied: <cpp|qt_toggle_command_rep> (checkbox state),
  <cpp|qt_enum_command_rep> (current text of a combo box),
  <cpp|qt_choice_command_rep> (selected items of a list, as a <scheme>
  list), ... For example:

  <\cpp-code>
    void apply () { if (qwid) cmd (list_object (object
    (qwid-\<gtr\>isChecked()))); }
  </cpp-code>

  <section|Windows>

  <paragraph|Plain windows.><cpp|qt_window_widget_rep> wraps a top-level
  <cpp|QWidget> (a <cpp|QTMPlainWindow> for dialogs). It stores a pointer
  to itself in the <name|Qt> property <verbatim|texmacs_window_widget>, so
  that <cpp|widget_from_qwidget> can find the <TeXmacs> window of any
  <name|Qt> widget, assigns the identifier returned for
  <cpp|SLOT_IDENTIFIER> (and increments the global window count
  <cpp|nr_windows>, which ends the application when it drops to zero),
  and connects the <cpp|closed ()> signal emitted by <cpp|closeEvent> to
  the <cpp|quit> command. It implements the window slots (visibility,
  title, position, size, full screen) and forwards <cpp|SLOT_REFRESH> (see
  below). If the contents have no resizable children, the window gets a
  fixed size.

  <paragraph|The main window.><cpp|qt_tm_widget_rep>, the result of
  <cpp|texmacs_widget (mask, quit)> for a non-zero mask, is a window widget
  around a <cpp|QTMWindow> (a <cpp|QMainWindow>). Its constructor decodes
  the mask into the array <cpp|visibility> and creates the <name|Qt>
  furniture: tool bars (<cpp|mainToolBar>, <cpp|modeToolBar>,
  <cpp|focusToolBar>, <cpp|userToolBar>, and <cpp|menuToolBar> for the
  non-native menu bar), dock widgets for the tools (<cpp|sideTools>,
  <cpp|leftTools>, <cpp|bottomTools>, <cpp|extraTools>), the footer labels
  <cpp|leftLabel> and <cpp|rightLabel> and an interactive prompt. Its
  message handlers map the slots of the main widget to this furniture:

  <\itemize>
    <item><cpp|write> with <cpp|SLOT_SCROLLABLE> replaces the central
    canvas by the <cpp|QWidget> of the new editor (<cpp|main_widget>);

    <item><cpp|SLOT_MAIN_MENU> calls <cpp|install_main_menu> (or postpones
    it, see above);

    <item><cpp|SLOT_MAIN_ICONS>, <cpp|SLOT_MODE_ICONS>, ... replace
    the buttons of a tool bar with the actions of <cpp|get_qactionlist
    ()>;

    <item><cpp|SLOT_SIDE_TOOLS> and the other tool slots put
    <cpp|as_qwidget ()> of the new contents into the corresponding dock
    widget;

    <item>the visibility slots update <cpp|visibility> and call
    <cpp|update_visibility>;

    <item>canvas slots such as <cpp|SLOT_EXTENTS>,
    <cpp|SLOT_SCROLL_POSITION>, <cpp|SLOT_ZOOM_FACTOR> or
    <cpp|SLOT_INVALIDATE> are forwarded to <cpp|main_widget>;

    <item><cpp|SLOT_INTERACTIVE_MODE>, <cpp|SLOT_INTERACTIVE_PROMPT> and
    <cpp|SLOT_INTERACTIVE_INPUT> show the prompt and the input field in
    the footer, with <cpp|QTMInteractiveInputHelper> committing the
    answer.
  </itemize>

  With mask 0, <cpp|texmacs_widget> returns a
  <cpp|qt_tm_embedded_widget_rep> instead: no window and no bars, just the
  canvas, for editors embedded in dialogs.

  <section|Canvases>

  <cpp|qt_simple_widget_rep> is the base class of the editor and of box
  widgets. Its <cpp|as_qwidget> creates a <cpp|QTMWidget> (derived from
  <cpp|QTMScrollView>, a <cpp|QAbstractScrollArea>), which translates
  <name|Qt> events into calls of <cpp|the_gui-\<gtr\>process_keypress>,
  <cpp|process_mouse>, ... and paints from a backing pixmap. Painting
  is driven by <TeXmacs>: <cpp|SLOT_INVALIDATE> records invalid
  rectangles, and <cpp|repaint_all> (called from
  <cpp|qt_gui_rep::update>) calls <cpp|handle_repaint> on the invalid
  regions of all canvases with a renderer drawing into the backing store.
  Since the <cpp|QTMWidget> may be created after the kernel started to
  send messages (for instance extents or scroll positions), the last value
  sent to each slot is remembered in <cpp|sent_slots> and replayed with
  <cpp|reapply_sent_slots> when the <name|Qt> widget is created. The
  renderer itself is described in \P<hlink|The renderer
  interface|renderer.en.tm>\Q.

  <section|Refresh widgets>

  <cpp|refresh_widget> and <cpp|refreshable_widget> are UI elements whose
  <cpp|as_qwidget> creates a <cpp|QTMRefreshWidget> or a
  <cpp|QTMRefreshableWidget>. Both connect to the signal
  <cpp|tmSlotRefresh (string)> of the global <cpp|QTMGuiHelper>, compute
  their contents a first time with the kind <verbatim|"init">, and
  recompute them in the slot <cpp|doRefresh>:

  <\itemize>
    <item><cpp|QTMRefreshWidget::recompute> evaluates the <scheme>
    expression <verbatim|(vertical (link <em|name>))>, computes its
    <scm|menu-expand> and, if it differs from the current one, builds the
    widget with <cpp|make_menu_widget> (reusing an entry of its private
    cache when <cpp|menu_caching> is set);

    <item><cpp|QTMRefreshableWidget::recompute> calls the <scheme> closure
    and uses the widget it returns, unless it is the same object as before.
  </itemize>

  In both cases the old <cpp|QWidget> is scheduled for deletion and
  replaced by <cpp|as_qwidget> of the new contents; if the enclosing window
  had a fixed size, it is adjusted. The signal is emitted by
  <cpp|QTMGuiHelper::emitTmSlotRefresh>, which is called when a
  <cpp|qt_window_widget_rep> receives <cpp|SLOT_REFRESH>. Consequently a
  refresh sent to <em|any> window reaches <em|all> refresh widgets; the
  <em|kind> argument is what limits the work.

  When the interface language changes, <cpp|gui_refresh> calls
  <cpp|qt_gui_rep::refresh_language>, which makes <cpp|QTMGuiHelper> emit
  its <cpp|refresh> signal; <cpp|QTMAction>s listen to it to retranslate
  their text.

  <section|Input fields and dialogs>

  <cpp|qt_input_text_widget_rep> creates a <cpp|QTMLineEdit>, with a
  <cpp|QCompleter> offering the proposals (or file names); the helper
  <cpp|QTMInputTextWidgetHelper> calls the command with the text when the
  user validates or leaves the field. The <em|type> string of the field
  becomes the <name|Qt> object name of the line edit, which is what
  <cpp|SLOT_KEYBOARD_FOCUS_ON> looks for. It may have the form
  <verbatim|<em|name>#<em|serial>:<em|type>>, which
  <cpp|QTMLineEdit::set_type> splits; the type selects special behaviour
  (<verbatim|password>, file names, search and replace fields) and fields
  whose serial starts with <verbatim|form-> are committed continuously.

  <cpp|qt_inputs_list_widget_rep> implements the multi-field dialogs of
  <cpp|tm_frame_rep::interactive>; its fields are
  <cpp|qt_field_widget_rep>s, returned by <cpp|SLOT_FORM_FIELD>. For
  simple questions it may use a native message box. The file chooser, the
  color picker and the print dialog are implemented with the
  corresponding <name|Qt> dialogs (<cpp|QTMFileDialog>, ...).

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
