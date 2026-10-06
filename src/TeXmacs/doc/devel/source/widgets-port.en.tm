<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Adding new widgets and porting to other toolkits>

  <section|Adding a new kind of widget>

  A new kind of widget has to be added at every layer of the stack. The
  simplest way to proceed is to follow an existing widget of similar
  nature through all the layers; for an input widget, <cpp|toggle_widget>
  is a good model:

  <descriptive-table|<tformat|<table|<row|<cell|Layer>|<cell|File>|<cell|For
  the toggle>>|<row|<cell|<c++> declaration>|<cell|<source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp>>|<cell|<cpp|toggle_widget
  (command cmd, bool on, int style)>>>|<row|<cell|<name|Qt>
  constructor>|<cell|<source-link|Plugins/Qt/qt_widget.cpp|src/Plugins/Qt/qt_widget.cpp>>|<cell|<cpp|qt_ui_element_rep::create
  (qt_widget_rep::toggle_widget, ...)>>>|<row|<cell|<name|Qt>
  rendering>|<cell|<source-link|Plugins/Qt/qt_ui_element.cpp|src/Plugins/Qt/qt_ui_element.cpp>>|<cell|<cpp|case
  toggle_widget> in <cpp|as_qwidget>, <cpp|qt_toggle_command_rep>>>|<row|<cell|<name|Widkit>>|<cell|<source-link|Plugins/Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>>|<cell|<cpp|toggle_widget>
  wrapper>>|<row|<cell|Glue>|<cell|<source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>>|<cell|<scm|(widget-toggle
  toggle_widget (widget command bool int))>>>|<row|<cell|Markup
  macro>|<cell|<source-link|kernel/gui/gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>>|<cell|<scm|$toggle>>>|<row|<cell|Keyword>|<cell|<source-link|kernel/gui/menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>>|<cell|<scm|gui-make-toggle>,
  entry <scm|toggle> of <scm|gui-make-table>>>|<row|<cell|Interpreter>|<cell|<source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>>|<cell|grammar
  <scm|(toggle :%2)>, <scm|make-toggle>, <scm|menu-expand-toggle>>>>>>

  In more detail, suppose that we want to add a widget constructor
  <cpp|foo_widget (command cmd, string val, int style)> with a keyword
  <scm|(foo <scm-arg|cmd> <scm-arg|val>)> (these names are only an
  illustration).

  <subsection|The <c++> side>

  <\enumerate>
    <item>Declare <cpp|foo_widget> in <source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp>,
    with a comment describing its semantics. Every port has to implement
    it, otherwise <TeXmacs> will not link.

    <item>In the <name|Qt> port, add a value <cpp|foo_widget> to the
    enumeration <cpp|qt_widget_rep::types> in
    <source-link|Plugins/Qt/qt_widget.hpp|src/Plugins/Qt/qt_widget.hpp> and the corresponding string, at the
    same position, in <cpp|qt_widget_type_strings> (in
    <cpp|type_as_string>).

    <item>Implement the constructor in <source-link|Plugins/Qt/qt_widget.cpp|src/Plugins/Qt/qt_widget.cpp>:

    <\cpp-code>
      widget foo_widget (command cmd, string val, int style) {

      \ \ if (headless_mode) return headless_widget ();

      \ \ qt_widget wid = qt_ui_element_rep::create
      (qt_widget_rep::foo_widget,

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ cmd,
      val, style);

      \ \ return abstract (wid);

      }
    </cpp-code>

    and add <cpp|foo_widget> to the list of types handled by
    <cpp|qt_ui_element_rep::get_payload>.

    <item>In <source-link|Plugins/Qt/qt_ui_element.cpp|src/Plugins/Qt/qt_ui_element.cpp>, add a case to
    <cpp|as_qwidget> which unpacks the payload with exactly the same tuple
    type (<cpp|triple\<less\>command, string, int\<gtr\>>), creates the
    <name|Qt> widget with <cpp|parent_widget> as parent, stores it in
    <cpp|qwid> and connects its signals to a <cpp|QTMCommand>. If the
    command needs arguments, wrap it in a small <cpp|command_rep> which
    reads the state of the <name|Qt> widget, as
    <cpp|qt_toggle_command_rep> does, and call <cpp|cmd (list_object
    (...))>. Add <cpp|foo_widget> to the list of types for which
    <cpp|as_qlayoutitem> wraps <cpp|as_qwidget> in a <cpp|QWidgetItem>
    (otherwise it returns <cpp|NULL> and the widget does not appear in
    lists). If the widget may occur in menus or tool bars, also add a case
    to <cpp|as_qaction>, which otherwise fails.

    <item>Make the same changes in <source-link|Plugins/Qt6|src/Plugins/Qt6>.

    <item>Provide at least a stub in the other ports, for instance a
    wrapper in <source-link|Plugins/Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>
    returning an existing <name|Widkit> widget.
  </enumerate>

  If the widget needs new messages (for instance to change its value from
  the kernel), add a slot as described in the next subsection.

  <subsection|Glue>

  Add a line to <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>, next to the
  other widgets:

  <\scm-code>
    (widget-foo foo_widget (widget command string int))
  </scm-code>

  The generated file <source-link|Scheme/Glue/glue_basic.cpp|src/Scheme/Glue/glue_basic.cpp> is kept in the
  repository and must be regenerated, with a working <name|Guile>, by
  running <verbatim|./build-glue build-glue-basic.scm glue_basic.cpp> in
  <source-link|src/src/Scheme/Glue|src/Scheme/Glue> (this is what the <verbatim|GLUE> target
  of <verbatim|src/src/makefile> does). The script <verbatim|build-auto-doc>,
  called by <verbatim|build-glue>, also updates
  <source-link|progs/prog/glue-symbols.scm|TeXmacs/progs/prog/glue-symbols.scm>. The argument types must be known
  to the glue generator; types such as <verbatim|command>,
  <verbatim|promise_widget>, <verbatim|array_widget> or
  <verbatim|array_string> are already supported (see
  <source-link|Scheme/Scheme/glue.cpp|src/Scheme/Scheme/glue.cpp>).

  <subsection|The <scheme> side>

  <\enumerate>
    <item>In <source-link|kernel/gui/gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>, define the macro which
    builds the menu item, turning the parts that must be recomputed into
    closures:

    <\scm-code>
      (tm-define-macro ($foo cmd val)

      \ \ (:synopsis "Make a foo widget")

      \ \ `(list 'foo (lambda (answer) ,cmd) (lambda () ,val)))
    </scm-code>

    <item>In <source-link|kernel/gui/menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>, add the translation
    function and register it in <scm|gui-make-table>:

    <\scm-code>
      (define (gui-make-foo x)

      \ \ (require-format x '(foo :%2))

      \ \ `($foo ,@(cdr x)))

      \;

      (define-table gui-make-table

      \ \ ...

      \ \ (foo ,gui-make-foo)

      \ \ ...)
    </scm-code>

    (from another module, <scm|extend-table> can be used instead).

    <item>In <source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>, add <scm|(foo :%2)> to
    the <scm|:menu-item> grammar, write the builder and register it in
    <scm|make-menu-items-table>:

    <\scm-code>
      (define (make-foo p style)

      \ \ (with (tag cmd val) p

      \ \ \ \ (widget-foo (object-\<gtr\>command (menu-protect cmd)) (val)
      style)))

      \;

      \ \ (foo (:%2)

      \ \ \ \ \ \ \ ,(lambda (p style bar?) (list (make-foo p style))))
    </scm-code>

    <item>Add an entry to <scm|menu-expand-table> which evaluates the
    closures carrying state (here <scm|val>) and replaces the others by
    their source, as <scm|menu-expand-toggle> does. Otherwise a change of
    <scm|val> is invisible in the expansion, and the cached widget will be
    reused by <cpp|get_menu_widget> and refresh widgets.

    <item>Optionally, add the corresponding <scm|build-*> and
    <scm|markup-*> functions to <source-link|kernel/gui/menu-convert.scm|TeXmacs/progs/kernel/gui/menu-convert.scm>, so
    that the widget also works with the markup interface.

    <item>Document the keyword in the \P<hlink|Widgets reference
    guide|../scheme/gui/scheme-gui-reference.en.tm>\Q.
  </enumerate>

  <subsection|Adding a slot>

  <\enumerate>
    <item>Add <cpp|SLOT_FOO> to the enumeration <cpp|slot_id> in
    <source-link|Graphics/Gui/message.hpp|src/Graphics/Gui/message.hpp>, before <cpp|slot_id__LAST>.

    <item>Add <verbatim|"SLOT_FOO"> at the same position in the array of
    <cpp|slot_name> in <source-link|Graphics/Gui/widget.cpp|src/Graphics/Gui/widget.cpp>.

    <item>Add typed helper functions to <source-link|message.hpp|src/Graphics/Gui/message.hpp>, for
    example

    <\cpp-code>
      inline void

      set_foo (widget w, string s) {

      \ \ send\<less\>string\<gtr\> (w, SLOT_FOO, s);

      }
    </cpp-code>

    <item>Handle the slot in the <cpp|send>, <cpp|query>, <cpp|read> or
    <cpp|write> methods of the widgets concerned, in each port, opening
    the blackbox with the same type (<cpp|check_type\<less\>string\<gtr\>
    (val, s)> in <name|Qt>). For slots of the main widget, remember that
    there are two implementations in <name|Qt> (<cpp|qt_tm_widget_rep> and
    <cpp|qt_tm_embedded_widget_rep>), and that <cpp|qt_widget_rep::query>
    and <cpp|qt_headless_widget_rep::query> provide defaults for common
    queries.
  </enumerate>

  <section|Porting <TeXmacs> to another toolkit>

  <subsection|What a port consists of>

  A graphical port is a directory in <source-link|src/src/Plugins|src/Plugins> which
  implements:

  <\enumerate>
    <item>the system-wide routines of <source-link|Graphics/Gui/gui.hpp|src/Graphics/Gui/gui.hpp>;

    <item>all widget constructors of <source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp>,
    with the messages they are expected to understand;

    <item>a class <cpp|simple_widget_rep> providing canvases;

    <item>a <cpp|renderer> for the screen, pictures and fonts, as
    explained in \P<hlink|The renderer interface|renderer.en.tm>\Q;

    <item>a main loop which queues events and calls the interpose handler.
  </enumerate>

  The <name|Qt> port (\P<hlink|The <name|Qt>
  implementation|widgets-qt.en.tm>\Q) is the reference. The older
  <name|X11> port consists of <source-link|Plugins/X11|src/Plugins/X11> (the display, windows
  implementing <cpp|window_rep> of <source-link|window.hpp|src/Graphics/Gui/window.hpp>, events, fonts and
  pictures) and <source-link|Plugins/Widkit|src/Plugins/Widkit>, a complete toolkit of its own
  whose widgets are drawn with the <TeXmacs> renderer and communicate with
  <cpp|event>s (see the historical document \P<hlink|The graphical user
  interface|gui.en.tm>\Q); <source-link|widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp> maps the abstract
  constructors and slots to <name|Widkit>. The experimental <name|Cocoa>
  port is in <verbatim|Plugins/Cocoa>. Both lag behind the abstract
  interface: <name|Widkit> currently has no <cpp|responsive_tabs_widget>,
  <cpp|responsive_icon_tabs_widget>, <cpp|setting_toggle_widget>,
  <cpp|setting_enum_widget> and <cpp|setting_group_widget>, and the
  <name|Cocoa> port misses even more constructors.

  <subsection|Build integration>

  The port is selected at configuration time: <verbatim|configure> sets
  <verbatim|CONFIG_GUI> and defines one of the macros <cpp|QTTEXMACS>,
  <cpp|AQUATEXMACS> or <cpp|X11TEXMACS>, and <source-link|src/src/makefile.in|src/makefile.in>
  compiles the corresponding plug-in directories (<verbatim|X11 Widkit> for
  <name|X11>); <source-link|CMakeLists.txt|src/CMakeLists.txt> has the cache variable
  <verbatim|TEXMACS_GUI>, but currently only builds the <name|Qt> port.
  About a hundred places outside the plug-ins test these macros (search
  for <cpp|QTTEXMACS>), for instance <source-link|Edit/editor.hpp|src/Edit/editor.hpp> and
  <source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp>, which choose the header defining
  <cpp|simple_widget_rep>; the fallback in all these places is the
  <name|X11>/<name|Widkit> behaviour, so a new port has to add its own
  branches. The function <cpp|gui_is_qt ()> of
  <source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp> and the <scheme> predicate
  <scm|qt-gui?> are used for run-time tests.

  <subsection|The routines of <source-link|gui.hpp|src/Graphics/Gui/gui.hpp>>

  <descriptive-table|<tformat|<table|<row|<cell|Routine>|<cell|Obligation>|<cell|In
  <name|Qt>>>|<row|<cell|<cpp|gui_open>, <cpp|gui_close>>|<cell|create and
  destroy the application object>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|gui_interpose>>|<cell|remember
  the handler to call from the main loop>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|gui_start_loop>>|<cell|run
  the main loop until the last window is
  closed>|<cell|<cpp|qt_gui_rep::event_loop>>>|<row|<cell|<cpp|gui_root_extents>,
  <cpp|gui_maximal_extents>>|<cell|screen size in
  <cpp|SI>>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|gui_refresh>>|<cell|retranslate
  and redraw after a language change>|<cell|<cpp|refresh_language>>>|<row|<cell|<cpp|gui_version>>|<cell|a
  name such as <verbatim|"qt5"> or
  <verbatim|"qt6">>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|set_default_font>,
  <cpp|get_default_font>>|<cell|fonts used in
  widgets>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|load_system_font>>|<cell|optional
  system fonts>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|set_selection>,
  <cpp|get_selection>, <cpp|clear_selection>>|<cell|clipboards (named
  <verbatim|"primary">, <verbatim|"mouse">, ...) with format
  conversions>|<cell|<cpp|qt_gui_rep>>>|<row|<cell|<cpp|beep>>|<cell|>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|needs_update>>|<cell|schedule
  a pass of the main loop>|<cell|<cpp|qt_gui_rep::need_update>>>|<row|<cell|<cpp|check_event>>|<cell|tell
  the typesetter whether user events are pending, so that it can interrupt
  long repaints>|<cell|<cpp|qt_gui_rep::check_event>>>|<row|<cell|<cpp|show_help_balloon>>|<cell|tooltip
  at a position, hidden on the next event>|<cell|<cpp|qt_gui_rep>>>|<row|<cell|<cpp|show_wait_indicator>>|<cell|message
  during long operations, removed when the message is
  empty>|<cell|<cpp|qt_gui_rep>>>|<row|<cell|<cpp|external_event>>|<cell|events
  from other devices>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|gui_interrupted>>|<cell|usually
  in terms of <cpp|check_event>>|<cell|<source-link|Graphics/Renderer/basic_renderer.cpp|src/Graphics/Renderer/basic_renderer.cpp>>>>>>

  The routines <cpp|get_default_styled_font> and <cpp|get_widget_size> and
  the global variables <cpp|use_native_menubar>, <cpp|tm_style_sheet>,
  <cpp|tm_style_density> and <cpp|use_mini_bars> are defined in
  <source-link|widget.cpp|src/Graphics/Gui/widget.cpp> and need not be provided (the variables are
  interpreted by the port). <cpp|image_gc> is declared but currently
  neither implemented nor used.

  <subsection|Widgets and messages>

  All constructors of <source-link|widget.hpp|src/Graphics/Gui/widget.hpp> must exist. It is legitimate
  to start with trivial implementations for rarely used widgets (the
  <name|Qt> port itself has none for <cpp|ink_widget>, <cpp|empty_widget>
  and <cpp|wait_widget>), but the following are essential:

  <\itemize>
    <item><cpp|texmacs_widget (mask, quit)>, which must understand the
    canvas slots, <cpp|SLOT_SCROLLABLE> (to receive the editor), the
    menu, icon and tool slots with their visibility counterparts, the
    footer and interactive slots, <cpp|SLOT_KEYBOARD_FOCUS> and
    <cpp|SLOT_DESTROY>; with <cpp|mask> 0 it must return an embeddable
    variant;

    <item>window widgets, which must understand <cpp|SLOT_IDENTIFIER>,
    <cpp|SLOT_VISIBILITY>, <cpp|SLOT_NAME>, <cpp|SLOT_MODIFIED>,
    <cpp|SLOT_SIZE>, <cpp|SLOT_POSITION>, <cpp|SLOT_FULL_SCREEN>,
    <cpp|SLOT_REFRESH> and <cpp|SLOT_DESTROY>, run the <cpp|quit> command
    when closed by the user, and report moves, resizes and destruction to
    the kernel with <cpp|notify_window_move>, <cpp|notify_window_resize>
    and <cpp|notify_window_destroy> (so that geometries are remembered);

    <item>the menu widgets (<cpp|horizontal_menu>, <cpp|vertical_menu>,
    <cpp|menu_button>, <cpp|pulldown_button>, <cpp|pullright_button>,
    <cpp|text_widget>, <cpp|xpm_widget>, <cpp|menu_separator>,
    <cpp|balloon_widget>), which must be usable both in the menu bar, in
    menus and in tool bars;

    <item><cpp|popup_widget> and <cpp|popup_window_widget>, used for the
    contextual menu;

    <item>the lists, glue and input widgets used by dialogs, and
    <cpp|refresh_widget> and <cpp|refreshable_widget>.
  </itemize>

  A few general rules, learned from the <name|Qt> port, will save much
  trouble:

  <\itemize>
    <item><em|Keep descriptions lazy.> Store the arguments of a
    constructor and build toolkit objects only when needed, possibly
    several times. Submenus must evaluate their promise each time they
    are shown, and must not cache the result.

    <item><em|Never run <TeXmacs> code from toolkit callbacks.> Queue key
    presses, mouse events, resizes and commands, and process them from the
    main loop, followed by the interpose handler registered with
    <cpp|gui_interpose> and by the repainting of the canvases. The
    <scheme> side already defers menu actions with <scm|exec-delayed>; a
    port which does not define <cpp|QTTEXMACS> gets the implementation of
    <cpp|exec_delayed> of <source-link|Scheme/Scheme/object.cpp|src/Scheme/Scheme/object.cpp>, whose
    pending commands are run by <cpp|exec_pending_commands> from the
    interpose handler of the server.

    <item><em|Be careful with ownership.> Abstract widgets are reference
    counted and may be destroyed late; toolkit objects should be owned by
    the toolkit hierarchy (windows) and referenced from <TeXmacs> widgets
    through guarded pointers.

    <item><em|Replace, do not mutate.> The kernel installs whole new
    widgets for menus and bars; it never edits an existing menu. Installing
    a new menu bar while a menu is open must be postponed.

    <item><em|Implement <cpp|SLOT_REFRESH> globally or per window>, keeping
    in mind that <cpp|windows_refresh> only sends it to the auxiliary
    windows (see \P<hlink|Windows, the main <TeXmacs> widget and the flow
    of events|widgets-window.en.tm>\Q).
  </itemize>

  <subsection|Canvases>

  The class <cpp|simple_widget_rep> must provide the virtual methods listed
  at the end of <source-link|widget.hpp|src/Graphics/Gui/widget.hpp> (see \P<hlink|The abstract widget
  interface in <c++>|widgets-cpp.en.tm>\Q), deliver events to them
  (key names in the <TeXmacs> format, as produced for instance by
  <source-link|Plugins/Qt/QTMKeyboard.cpp|src/Plugins/Qt/QTMKeyboard.cpp> and <cpp|QTMWidget>; mouse events
  with kinds such as <verbatim|"press-left"> and modifiers), and repaint
  invalidated regions by calling <cpp|handle_repaint> with a renderer
  drawing on the canvas. It must also understand the canvas slots
  (<cpp|SLOT_EXTENTS>, <cpp|SLOT_SCROLL_POSITION>,
  <cpp|SLOT_VISIBLE_PART>, <cpp|SLOT_ZOOM_FACTOR>, <cpp|SLOT_INVALIDATE>,
  <cpp|SLOT_INVALIDATE_ALL>, <cpp|SLOT_INVALID>, <cpp|SLOT_CURSOR>,
  <cpp|SLOT_MOUSE_GRAB>, <cpp|SLOT_KEYBOARD_FOCUS>, <cpp|SLOT_WINDOW>,
  ...), which the editor uses constantly. The <name|Qt> class
  <cpp|qt_simple_widget_rep> and its <cpp|QTMWidget> are a good starting
  point.

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
