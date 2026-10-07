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
  toggle_widget> in <cpp|as_qwidget>, <cpp|qt_toggle_command_rep>>>|<row|<cell|<name|Cocoa>
  constructor>|<cell|<source-link|Plugins/NS/ns_widget.mm|src/Plugins/NS/ns_widget.mm>>|<cell|<cpp|ns_ui_element_rep::create
  (ns_widget_rep::toggle_widget, ...)>>>|<row|<cell|<name|Cocoa>
  rendering>|<cell|<source-link|Plugins/NS/ns_ui_element.mm|src/Plugins/NS/ns_ui_element.mm>>|<cell|<cpp|case
  toggle_widget> in <cpp|as_nsview>>>|<row|<cell|<name|Vue>
  constructor>|<cell|<source-link|Plugins/Vue/vue_widget.cpp|src/Plugins/Vue/vue_widget.cpp>>|<cell|<cpp|VUE_WIDGET(toggle_widget,
  command, cmd, bool, on, int, style)>>>|<row|<cell|<name|Vue>
  layout>|<cell|<source-link|Plugins/Vue/vue_widget.cpp|src/Plugins/Vue/vue_widget.cpp>>|<cell|<cpp|type
  == "toggle_widget"> in <cpp|vue_ui_rep::do_layout> and
  <cpp|vue_ui_rep::render>>>|<row|<cell|<name|Widkit>>|<cell|<source-link|Plugins/Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>>|<cell|<cpp|toggle_widget>
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

    <item>In the <name|Cocoa> port, proceed as for <name|Qt>: a value in
    the enumeration <cpp|ns_widget_rep::types> and its string in
    <cpp|ns_widget_type_strings> (<source-link|Plugins/NS/ns_widget.h|src/Plugins/NS/ns_widget.h>), the
    constructor in <source-link|Plugins/NS/ns_widget.mm|src/Plugins/NS/ns_widget.mm> with
    <cpp|ns_ui_element_rep::create>, the type in the list of
    <cpp|ns_ui_element_rep::get_payload>, and cases in <cpp|as_nsview>
    (an <cpp|NSView> for dialogs and bars) and, for menus, in
    <cpp|as_menuitem> (<source-link|Plugins/NS/ns_ui_element.mm|src/Plugins/NS/ns_ui_element.mm>).

    <item>In the <name|Vue> port, one line

    <\cpp-code>
      VUE_WIDGET(foo_widget, command, cmd, string, val, int, style);
    </cpp-code>

    in <source-link|Plugins/Vue/vue_widget.cpp|src/Plugins/Vue/vue_widget.cpp> generates the constructor, a
    payload structure <cpp|vue_foo_widget> and the type name
    <verbatim|"foo_widget"> of the <cpp|vue_ui_rep> it returns. Then add a
    case <cpp|if (type == "foo_widget")> to <cpp|vue_ui_rep::do_layout>,
    which opens the payload, declares the <name|Clay> elements of the
    widget for the current frame (with ids derived from the serial number
    <cpp|id> of the widget, so that they are stable from frame to frame),
    reads the clicks of the previous events with <cpp|button_logic>, and
    pushes the command onto <cpp|cmd_list> instead of calling it. State
    which the user changes (a toggle which is clicked) is stored back into
    the payload. Custom drawing (with the <TeXmacs> renderer) goes into a
    case of <cpp|vue_ui_rep::render>, reached through a <name|Clay> custom
    element whose <cpp|userData> is <cpp|render_ref ()>. Widgets with a
    persistent state of their own (text inputs, trees, the ink widget) are
    rather subclasses of <cpp|vue_widget_rep>.

    <item>In <source-link|Plugins/Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp> (for the
    <name|X11>, <name|SDL> and <name|Qtwk> ports), write a wrapper building
    a <name|Widkit> widget; a composition of existing widgets is enough to
    start with (<cpp|setting_toggle_widget> is a toggle and a text in a
    <cpp|horizontal_list>).
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
    (val, s)> in <name|Qt> and <name|Cocoa>, <cpp|check_open\<less\>string\<gtr\>
    (val, s)> in <name|Vue>). For slots of the main widget, remember that
    there are two implementations in <name|Qt> (<cpp|qt_tm_widget_rep> and
    <cpp|qt_tm_embedded_widget_rep>) and in <name|Cocoa>
    (<cpp|ns_tm_widget_rep> and <cpp|ns_tm_embedded_widget_rep>), and that
    <cpp|qt_widget_rep::query> and <cpp|qt_headless_widget_rep::query>
    provide defaults for common queries. In <name|Widkit>, a slot is
    usually translated into an event in <cpp|wk_widget_rep::send> or
    <cpp|query> (<source-link|widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>).
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
  implementation|widgets-qt.en.tm>\Q) is the reference. The current ports
  fall into three families, described in the last section of this page:

  <descriptive-table|<tformat|<table|<row|<cell|Port>|<cell|Directory>|<cell|Widgets>|<cell|Windows,
  events>|<cell|Drawing>>|<row|<cell|<name|Qt>>|<cell|<source-link|Plugins/Qt|src/Plugins/Qt>
  (<source-link|Qt6|src/Plugins/Qt6>)>|<cell|native <name|Qt>, built
  lazily>|<cell|<name|Qt>>|<cell|<cpp|QPainter>>>|<row|<cell|<name|Cocoa>>|<cell|<source-link|Plugins/NS|src/Plugins/NS>>|<cell|native
  <name|AppKit>, built lazily>|<cell|<name|AppKit>>|<cell|<name|Core
  Graphics>>>|<row|<cell|<name|Vue>>|<cell|<source-link|Plugins/Vue|src/Plugins/Vue>>|<cell|<name|Clay>,
  immediate mode>|<cell|<name|SDL> 3>|<cell|<name|MuPDF>, or the
  GPU>>|<row|<cell|<name|X11>>|<cell|<source-link|Plugins/X11|src/Plugins/X11>>|<cell|<name|Widkit>>|<cell|<name|Xlib>>|<cell|<name|Xlib>>>|<row|<cell|<name|SDL>>|<cell|<source-link|Plugins/SDL|src/Plugins/SDL>>|<cell|<name|Widkit>>|<cell|<name|SDL>
  3>|<cell|<name|MuPDF>>>|<row|<cell|<name|Qtwk>>|<cell|<source-link|Plugins/Qtwk|src/Plugins/Qtwk>>|<cell|<name|Widkit>>|<cell|<name|Qt>>|<cell|<cpp|QPainter>>>>>>

  The <name|Widkit> ports implement <cpp|window_rep> of
  <source-link|window.hpp|src/Graphics/Gui/window.hpp> and leave the widgets to
  <source-link|Plugins/Widkit|src/Plugins/Widkit>, a complete toolkit of its own whose widgets are
  drawn with the <TeXmacs> renderer and communicate with <cpp|event>s (see
  the historical document \P<hlink|The graphical user
  interface|gui.en.tm>\Q); <source-link|widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp> maps the abstract
  constructors and slots to <name|Widkit>. A new port for a platform
  without a usable native toolkit can start in the same way, providing
  only windows, events, a renderer and the routines of
  <source-link|gui.hpp|src/Graphics/Gui/gui.hpp>, as the <name|SDL> port does in about 2500 lines.

  <subsection|Build integration>

  The port is selected at configuration time (see \P<hlink|Selecting and
  building a port|guiports-build.en.tm>\Q): <verbatim|configure
  --with-gui=qt\|qtwk\|x11\|cocoa\|sdl\|vue>
  (<source-link|misc/m4/tm_gui.m4|misc/m4/tm_gui.m4>) sets <verbatim|CONFIG_GUI>, defines
  one of the macros <cpp|QTTEXMACS>, <cpp|AQUATEXMACS>, <cpp|X11TEXMACS>,
  <cpp|SDLTEXMACS> or <cpp|VUETEXMACS> (<name|Qtwk> defines both
  <cpp|QTWKTEXMACS> and <cpp|QTTEXMACS>), and
  <source-link|src/src/makefile.in|src/makefile.in> compiles the corresponding plug-in
  directories (<verbatim|X11 Widkit>, <verbatim|SDL Widkit>, <verbatim|Qtwk
  Widkit> with a few files of <verbatim|Qt>, <verbatim|NS> with
  <verbatim|MacOS>, <verbatim|Vue>). <source-link|CMakeLists.txt|src/CMakeLists.txt> has the
  cache variable <verbatim|TEXMACS_GUI>, which accepts <verbatim|Qt>
  (<verbatim|Qt6>, <verbatim|Qt5>, <verbatim|Qt4>), <verbatim|Vue>,
  <verbatim|SDL> and <verbatim|X11>, but not <name|Cocoa> nor <name|Qtwk>.

  About two hundred places outside the plug-ins test these macros (search
  for <cpp|QTTEXMACS>, then for the others), for instance
  <source-link|Edit/editor.hpp|src/Edit/editor.hpp> and
  <source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp>, which choose the header defining
  <cpp|simple_widget_rep>. The fallback in most of these places is the
  <name|X11>/<name|Widkit> behaviour, so a new port has to add its own
  branches: the <name|Cocoa> port mostly joins the <name|Qt> branches
  (<cpp|#if defined (QTTEXMACS) \|\| defined (AQUATEXMACS)>, for instance
  for <cpp|exec_delayed> in <source-link|Scheme/Scheme/object.cpp|src/Scheme/Scheme/object.cpp>), while
  <name|Vue> adds branches of its own (<cpp|VUETEXMACS>). Remember that
  <cpp|QTTEXMACS> alone also holds for <name|Qtwk>, whose widgets are those
  of <name|Widkit> (the run-time predicates below exclude it).

  At run time, <cpp|gui_version ()> and the functions <cpp|gui_is_qt ()>,
  <cpp|gui_is_vue ()> and <cpp|gui_is_x ()> of
  <source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp> are glued as <scm|gui-version>,
  <scm|qt-gui?>, <scm|vue-gui?> and <scm|x-gui?>; <scm|ns-gui?>,
  <scm|qt5-gui?> and friends are defined from <scm|gui-version> in
  <source-link|kernel/boot/abbrevs.scm|TeXmacs/progs/kernel/boot/abbrevs.scm>:

  <descriptive-table|<tformat|<table|<row|<cell|Port>|<cell|<scm|gui-version>>|<cell|<scm|qt-gui?>>|<cell|<scm|x-gui?>>|<cell|<scm|vue-gui?>>>|<row|<cell|<name|Qt>>|<cell|<verbatim|"qt5">,
  <verbatim|"qt6">>|<cell|yes>|<cell|no>|<cell|no>>|<row|<cell|<name|Qtwk>>|<cell|<verbatim|"qt5">,
  <verbatim|"qt6">>|<cell|no>|<cell|yes>|<cell|no>>|<row|<cell|<name|Cocoa>>|<cell|<verbatim|"ns">>|<cell|yes>|<cell|no>|<cell|no>>|<row|<cell|<name|Vue>>|<cell|<verbatim|"vue">>|<cell|no>|<cell|no>|<cell|yes>>|<row|<cell|<name|SDL>>|<cell|<verbatim|"sdl">>|<cell|no>|<cell|yes>|<cell|no>>|<row|<cell|<name|X11>>|<cell|<verbatim|"x11">>|<cell|no>|<cell|yes>|<cell|no>>>>>

  <scm|qt-gui?> means \Pimplements the widgets and dialogs of the
  <name|Qt> port\Q: the <scheme> code uses it to choose native dialogs,
  shortcuts and menus, which is why it holds for <name|Cocoa> and not for
  <name|Qtwk>, whose <scm|gui-version> is nevertheless that of <name|Qt>.
  <scm|x-gui?> means \Phas the historical <name|X11> look and feel\Q (the
  <name|Widkit> ports): <scheme> then asks itself before overwriting a
  file, which the native save panels of the other ports do. A new port
  should decide which of these predicates it satisfies, and look for the
  places in <source-link|TeXmacs/progs|TeXmacs/progs> which test them: a feature hidden
  behind <scm|(qt-gui?)> is silently missing elsewhere (in <name|Vue>, the
  color menus fall back to the <scheme> color picker for this reason).

  <subsection|The routines of <source-link|gui.hpp|src/Graphics/Gui/gui.hpp>>

  <descriptive-table|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-hpart|3>|<cwith|1|-1|2|2|cell-hpart|4>|<cwith|1|-1|3|3|cell-hpart|3>|<table|<row|<cell|Routine>|<cell|Obligation>|<cell|In
  <name|Qt>>>|<row|<cell|<cpp|gui_open>, <cpp|gui_close>>|<cell|create and
  destroy the application object>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|gui_interpose>>|<cell|remember
  the handler to call from the main loop>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|gui_start_loop>>|<cell|run
  the main loop until the last window is
  closed>|<cell|<cpp|qt_gui_rep::event_loop>>>|<row|<cell|<cpp|gui_root_extents>,
  <cpp|gui_maximal_extents>>|<cell|screen size in
  <cpp|SI>>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|gui_refresh>>|<cell|retranslate
  and redraw after a language change>|<cell|<cpp|refresh_language>>>|<row|<cell|<cpp|gui_version>>|<cell|the
  name of the port (see above)>|<cell|<source-link|qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp>>>|<row|<cell|<cpp|set_default_font>,
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
  interpreted by the port). <cpp|image_gc> is declared but not called by
  the kernel; <name|Qt> and <name|Cocoa> leave it empty, while
  <name|Vue> and <name|SDL> clear the image cache of their <name|MuPDF>
  renderer with it.

  The other ports implement these routines in
  <source-link|ns_gui.mm|src/Plugins/NS/ns_gui.mm> (<name|Cocoa>), <source-link|vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>
  (<name|Vue>), <source-link|sdl_gui.cpp|src/Plugins/SDL/sdl_gui.cpp>, <source-link|x_gui.cpp|src/Plugins/X11/x_gui.cpp>
  and <source-link|x_init.cpp|src/Plugins/X11/x_init.cpp>, and <source-link|qtwk_gui.cpp|src/Plugins/Qtwk/qtwk_gui.cpp>.

  <subsection|Widgets and messages>

  All constructors of <source-link|widget.hpp|src/Graphics/Gui/widget.hpp> must exist. It is legitimate
  to start with trivial implementations for rarely used widgets (the
  <name|Qt> and <name|Cocoa> ports have none for <cpp|ink_widget>,
  <cpp|empty_widget> and <cpp|wait_widget>; <name|Vue> implements all
  three, and <name|Widkit> falls back to the plain tabs for the
  responsive ones), but the following are essential:

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

  A few general rules, learned from the <name|Qt> port and confirmed by
  the <name|Cocoa> and <name|Vue> ports, will save much trouble:

  <\itemize>
    <item><em|Keep descriptions lazy.> Store the arguments of a
    constructor and build toolkit objects only when needed, possibly
    several times. Submenus must evaluate their promise each time they
    are shown, and must not cache the result beyond the time the menu is
    open (<name|Cocoa> refills its <cpp|TMLazyMenu> in
    <cpp|menuNeedsUpdate:>, <name|Vue> evaluates the promise when the menu
    opens and drops it when it closes).

    <item><em|Never run <TeXmacs> code from toolkit callbacks.> Queue key
    presses, mouse events, resizes and commands, and process them from the
    main loop, followed by the interpose handler registered with
    <cpp|gui_interpose> and by the repainting of the canvases. The
    <scheme> side already defers menu actions with <scm|exec-delayed>; a
    port which defines neither <cpp|QTTEXMACS> nor <cpp|AQUATEXMACS> gets
    the implementation of
    <cpp|exec_delayed> of <source-link|Scheme/Scheme/object.cpp|src/Scheme/Scheme/object.cpp>, whose
    pending commands are run by <cpp|exec_pending_commands> from the
    interpose handler of the server.

    <item><em|Be careful with ownership.> Abstract widgets are reference
    counted and may be destroyed late; toolkit objects should be owned by
    the toolkit hierarchy (windows) and referenced from <TeXmacs> widgets
    through guarded pointers. Conversely, whatever the toolkit keeps after
    a widget was replaced must keep the widget alive: <name|Vue> draws the
    render commands of the last layout again until the next one, and these
    name their widgets by raw pointers, so the layout holds a reference to
    each of them (<cpp|render_ref>, <cpp|release_layout_widgets>); the
    commands and the interpose handler may replace menus, tools and dialogs
    at any time, after which the windows are laid out again before
    anything is drawn.

    <item><em|Replace, do not mutate.> The kernel installs whole new
    widgets for menus and bars; it never edits an existing menu. Installing
    a new menu bar while a menu is open must be postponed (<name|Cocoa>
    also has one menu bar per main window, installed when the window
    becomes main).

    <item><em|Implement <cpp|SLOT_REFRESH> globally or per window>, keeping
    in mind that <cpp|windows_refresh> only sends it to the auxiliary
    windows (see \P<hlink|Windows, the main <TeXmacs> widget and the flow
    of events|widgets-window.en.tm>\Q), and that a refresh widget which is
    not shown at the time (a closed menu, a hidden tool) must still see
    the message when it is shown again.

    <item><em|Messages may arrive at the \Pwrong\Q widget.> The kernel
    sends <cpp|SLOT_FULL_SCREEN> to the window widget, but the title
    (<cpp|SLOT_NAME>) to the main widget; in <name|Qt> both are the same
    object. Ports where the window wraps the main widget must forward such
    messages in the right direction (<name|Vue> and <name|Cocoa> forward
    the full screen mode from the window to the main widget, and
    <name|Vue> the title from the main widget to its window). Use the debugging flag of unhandled messages to
    find such cases: the <name|Cocoa> and <name|Vue> ports reuse the flags
    of <name|Qt> (<verbatim|-debug-qt>, <verbatim|-debug-qt-widgets>).

    <item><em|Follow the <name|Qt> conventions for text.> Labels and inputs
    are in the Cork encoding and must be converted once, at the boundary;
    file names may be in Cork or in UTF-8 (<name|Cocoa> has
    <cpp|to_nsstring_utf8>, which keeps a string that looks like UTF-8, as
    <cpp|to_qstring> does). Keys must be named as in <name|Qt>
    (<verbatim|space>, <verbatim|S-tab>, <verbatim|enter>, ...); the
    composition of an input method is sent to the editor as a key
    <verbatim|pre-edit:<em|cursor>:<em|text>>, and a text committed at once
    is delivered one character per key (see <cpp|QTMWidget>,
    <cpp|TMView>).

    <item><em|Report the geometry of windows>, but not of popups, with
    <cpp|notify_window_move> and <cpp|notify_window_resize>; window
    systems where a move is modal (<name|macOS>) may require polling the
    geometry once per frame, as <name|Vue> does.

    <item><em|Plan for testing without a human.> Other programs often
    cannot capture or drive the windows of <TeXmacs> (on <name|macOS>
    in particular), so the <name|Cocoa> and <name|Vue> ports have test
    hooks driven by environment variables: snapshots of the windows,
    synthetic keys and clicks, scripted steps
    (<verbatim|TEXMACS_NS_SNAPSHOT>, <verbatim|TEXMACS_NS_PRESS>, ...;
    <verbatim|TEXMACS_VUE_SCRIPT>, <verbatim|TEXMACS_VUE_SNAPSHOT> with the
    scripts of <source-link|Plugins/Vue/tests|src/Plugins/Vue/tests>). <name|Vue> also runs
    headless (option <verbatim|-headless>, with the <name|SDL> video driver
    <verbatim|dummy>): the windows are then virtual, but the widgets are
    built and laid out as usual, whereas the <name|Qt> port replaces every
    widget by a <cpp|headless_widget> in that mode.
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
  point; <cpp|ns_simple_widget_rep> (with <cpp|TMView>) and
  <cpp|vue_simple_widget_rep> follow the same model. All three paint into
  a backing store of the size of the visible part, shift it when the view
  scrolls and repaint only the strips uncovered.

  <section|How the existing ports implement the interface>

  <subsection|<name|Cocoa>: native controls, on the model of
  <name|Qt>>

  The <name|Cocoa> port (<source-link|Plugins/NS|src/Plugins/NS>) is a transposition of the
  <name|Qt> port to <name|AppKit>, file by file: <cpp|ns_widget_rep> plays
  the role of <cpp|qt_widget_rep>, with the same enumeration of
  <cpp|types>, and <cpp|ns_ui_element_rep> stores the arguments of most
  constructors in a blackbox (its <cpp|create> templates are those of
  <cpp|qt_ui_element_rep>). The two ways of materializing a description
  are <cpp|as_nsview> (an <cpp|NSView>, laid out with <cpp|NSStackView>
  and <cpp|NSGridView>, for dialogs and icon bars) and <cpp|as_menuitem>
  (an <cpp|NSMenuItem>, for the menus); submenus are <cpp|TMLazyMenu>s.
  The main widget (<source-link|ns_tm_widget.mm|src/Plugins/NS/ns_tm_widget.mm>) puts the bars, the
  side tools and the footer around the canvas in an <cpp|NSWindow>; dialogs
  and standard panels are in <source-link|ns_dialogues.mm|src/Plugins/NS/ns_dialogues.mm>. Messages are
  handled in <cpp|send> and <cpp|query> as in <name|Qt>, and the event
  loop of <source-link|ns_gui.mm|src/Plugins/NS/ns_gui.mm> queues events and commands as
  <cpp|qt_gui_rep> does. Since it implements the same widgets,
  <scm|qt-gui?> holds and the generic code treats <cpp|AQUATEXMACS> as
  <name|Qt>. See \P<hlink|The <name|Cocoa> port|guiports-cocoa.en.tm>\Q.

  <subsection|<name|Vue>: immediate mode with <name|Clay>>

  The <name|Vue> port (<source-link|Plugins/Vue|src/Plugins/Vue>) keeps the tree of abstract
  widgets, but builds no toolkit objects from it. Most constructors are
  generated by the macro <cpp|VUE_WIDGET>, which defines a payload
  structure for the arguments and returns a <cpp|vue_ui_rep>, a pair of a
  type name and a blackbox; a few widgets with a state of their own are
  subclasses of <cpp|vue_widget_rep> (the main widget
  <cpp|vue_texmacs_widget_rep>, the canvas <cpp|vue_simple_widget_rep>,
  text inputs, the window widget <cpp|vue_plain_window_widget_rep>, trees,
  ink). At each frame, for each window, the main loop of
  <source-link|vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>:

  <\enumerate>
    <item>collects the <name|SDL> events into the input state of the
    window (pointer, buttons, key, wheel);

    <item>calls <cpp|do_layout> on the content of the window between
    <cpp|Clay_BeginLayout> and <cpp|Clay_EndLayout>; each widget declares
    its <name|Clay> elements, with ids derived from its serial number, and
    reacts to the input of the previous layout (<cpp|button_logic> asks
    <name|Clay> whether the pointer is over an element): a click pushes a
    command onto <cpp|cmd_list>, a pulldown button evaluates its promise,
    the canvas passes the pending key or mouse event to its
    <cpp|handle_*> methods. <cpp|post_layout> may ask for another pass (at
    most five), for instance when a window has to fit its contents;

    <item>runs the commands of <cpp|cmd_list>, then the interpose handler,
    then repaints the invalid regions of the editors into their backing
    stores;

    <item>draws the render commands of the layout: rectangles, borders,
    texts and images by the renderer of the port (<name|MuPDF>, or the GPU
    path of <source-link|vue_gpu.cpp|src/Plugins/Vue/vue_gpu.cpp>), and \Pcustom\Q elements by calling
    back <cpp|render> of the widget which declared them (the canvas copies
    its backing store, check boxes and arrows are drawn with the <TeXmacs>
    renderer).
  </enumerate>

  The messages are handled in <cpp|send> and <cpp|query> of these classes
  (<cpp|vue_ui_rep::send> for the descriptions), mostly by storing the new
  value, which the next layout shows. Menus and popups are separate
  undecorated <name|SDL> windows laid out in the same way. Since nothing
  survives a frame but the abstract widgets and the state kept in their
  payloads, there is no synchronization between a toolkit hierarchy and
  the <TeXmacs> widgets; the price is that every widget must be cheap to
  lay out. See \P<hlink|The <name|Vue> port|guiports-vue.en.tm>\Q.

  <subsection|<name|X11>, <name|SDL> and <name|Qtwk>: the <name|Widkit>
  toolkit>

  These three ports share the widgets of <source-link|Plugins/Widkit|src/Plugins/Widkit>: the
  constructors of <source-link|widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp> build <name|Widkit>
  widgets at once, <cpp|wk_widget_rep::send> and <cpp|query> translate the
  slots into <name|Widkit> events, and the main widget is the
  <cpp|texmacs_widget_rep> of
  <source-link|Widkit/Misc/texmacs_widget.cpp|src/Plugins/Widkit/Misc/texmacs_widget.cpp>. Window widgets
  (<source-link|window_widget.cpp|src/Plugins/Widkit/Basic/window_widget.cpp>) are put into a
  <cpp|window_rep> of the port, which owns a renderer, receives the events
  of the window system and passes them to the widgets. A port therefore
  only provides <cpp|window_rep>, the routines of <source-link|gui.hpp|src/Graphics/Gui/gui.hpp>, the
  main loop and a renderer: <cpp|x_window_rep> with <name|Xlib>,
  <cpp|sdl_window_rep> with a <name|MuPDF> backing store, and
  <cpp|qtwk_window_rep> with <name|Qt> windows and the renderer of the
  <name|Qt> port. The widgets look the same everywhere and are less
  complete than the native ones (no responsive tabs, for instance).

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
