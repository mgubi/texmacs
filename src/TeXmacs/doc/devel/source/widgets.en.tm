<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The abstract widget system>

  <section|Introduction>

  Everything that <TeXmacs> shows on the screen besides the typeset document
  itself (menus, icon bars, side tools, the footer, dialogs, auxiliary
  windows) is built from <em|widgets>. <TeXmacs> does not talk directly to
  a particular toolkit: the kernel only knows an <em|abstract> widget class,
  a fixed set of widget constructors and a message protocol. Each graphical
  port implements these constructors and messages in terms of its own
  toolkit: the native <name|Qt> widgets (<name|Qt> port, the default), the
  native <name|AppKit> controls of <name|macOS> (<name|Cocoa> port),
  elements drawn every frame by the immediate mode layout library
  <name|Clay> (<name|Vue> port), or the <TeXmacs> own toolkit
  <name|Widkit> on top of <name|X11>, <name|SDL> or <name|Qt> (the
  <name|X11>, <name|SDL> and <name|Qtwk> ports). On top of this, almost the
  entire user interface is described in <scheme> by a small declarative
  language, which is interpreted at run time into calls to the <c++>
  constructors.

  This part of the developer documentation describes the internals of this
  machinery: the <c++> interface, the way the main window is organized, the
  <scheme> language and its interpreter, the <name|Qt> implementation (the
  reference port), how the other ports map the same interface, and what
  has to be done to add a new kind of widget or to port <TeXmacs> to
  another toolkit. The <em|use> of the <scheme> widget language (how to
  write a menu or a dialog) is explained in the user-level tutorial
  \P<hlink|Extending the graphical user
  interface|../scheme/gui/scheme-gui.en.tm>\Q, which the present documents
  complement rather than repeat.

  <section|The layered architecture>

  The system can be seen as a stack of five layers. Data flows from top to
  bottom when a widget is built, and events and commands flow from bottom to
  top when the user interacts with it.

  <\enumerate>
    <item><em|The <scheme> widget language.> Menus and widgets are defined
    with <scm|menu-bind>, <scm|tm-menu> and <scm|tm-widget>, using keywords
    such as <scm|=\<gtr\>>, <scm|-\<gtr\>>, <scm|hlist>, <scm|toggle> or
    <scm|refreshable>. These macros are defined in
    <source-link|kernel/gui/menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm> and
    <source-link|kernel/gui/gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>.

    <item><em|Menu items.> Calling a function defined in this way does not
    create any widget: it returns a plain <scheme> list (a <em|menu item>),
    in which the dynamic parts are represented by closures. The grammar of
    menu items is given at the start of
    <source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>.

    <item><em|The interpreter.> The function <scm|make-menu-widget> of
    <source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm> walks through a menu item and calls
    the glued <c++> constructors (<scm|widget-hmenu>, <scm|widget-text>,
    <scm|widget-toggle>, ...), wrapping <scheme> closures into <c++>
    <cpp|command> and <cpp|promise\<less\>widget\<gtr\>> objects.

    <item><em|The abstract <c++> interface.> The class <cpp|widget> and the
    constructors such as <cpp|horizontal_menu>, <cpp|text_widget> or
    <cpp|texmacs_widget> are declared in
    <source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp>; the message protocol (slots) in
    <source-link|Graphics/Gui/message.hpp|src/Graphics/Gui/message.hpp>; the system-wide <abbr|GUI> routines
    in <source-link|Graphics/Gui/gui.hpp|src/Graphics/Gui/gui.hpp>. The kernel (for instance
    <source-link|Texmacs/Window/tm_window.cpp|src/Texmacs/Window/tm_window.cpp>) only uses this interface.

    <item><em|The port.> The constructors and the message handlers are
    implemented by a plug-in in <source-link|src/src/Plugins|src/Plugins>, selected at
    configuration time. The ports differ in when the toolkit objects are
    made:

    <\itemize>
      <item>in the <name|Qt> port most widgets are first stored as passive
      descriptions (<cpp|qt_ui_element_rep>) and only materialized into
      <cpp|QWidget>s, <cpp|QAction>s or <cpp|QLayoutItem>s when the toolkit
      needs them;

      <item>the <name|Cocoa> port follows the same design
      (<cpp|ns_ui_element_rep>, materialized into <cpp|NSView>s or
      <cpp|NSMenuItem>s);

      <item>the <name|Vue> port also stores descriptions
      (<cpp|vue_ui_rep>), but never builds persistent toolkit objects: at
      every frame the widget tree of a window is walked again and emits
      <name|Clay> elements, which are laid out and drawn;

      <item>the <name|Widkit> based ports build <name|Widkit> widgets at
      once (<source-link|widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>), which
      draw themselves with the <TeXmacs> renderer in windows provided by
      <name|X11>, <name|SDL> or <name|Qt>.
    </itemize>
  </enumerate>

  Schematically, for the menu entry \PNew\Q of the \PFile\Q menu:

  <\verbatim-code>
    (=\<gtr\> "File" ... ("New" (new-document)) ...) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Scheme DSL

    \ \ \ \ \ \ \ \ \| gui-make, $-macros

    \ \ \ \ \ \ \ \ v

    (=\<gtr\> "File" ... ("New" #\<less\>procedure\<gtr\>) ...) \ \ \ \ \ \ \ \ \ menu item

    \ \ \ \ \ \ \ \ \| make-menu-widget

    \ \ \ \ \ \ \ \ v

    pulldown_button (text_widget ("File"), promise) \ \ \ \ \ \ abstract C++ widget

    \ \ \ \ \ \ \ \ \| Plugins/Qt, Plugins/NS, Plugins/Vue

    \ \ \ \ \ \ \ \ v

    QAction + QTMLazyMenu, filled on aboutToShow () \ \ \ \ \ Qt objects

    NSMenuItem + TMLazyMenu, filled on menuNeedsUpdate: \ Cocoa objects

    Clay elements, promise evaluated when opened \ \ \ \ \ \ \ \ Vue (each frame)
  </verbatim-code>

  <section|Where to find the code>

  <\description-paragraphs>
    <item*|<source-link|src/src/Graphics/Gui/|src/Graphics/Gui>>The abstract interface:
    <source-link|widget.hpp|src/Graphics/Gui/widget.hpp>, <source-link|message.hpp|src/Graphics/Gui/message.hpp>, <source-link|gui.hpp|src/Graphics/Gui/gui.hpp>,
    <source-link|window.hpp|src/Graphics/Gui/window.hpp> and <source-link|widget.cpp|src/Graphics/Gui/widget.cpp>.

    <item*|<source-link|src/src/Kernel/Abstractions/command.hpp|src/Kernel/Abstractions/command.hpp>,
    <source-link|src/src/Kernel/Containers/promise.hpp|src/Kernel/Containers/promise.hpp>>Commands and promises,
    the two kinds of closures passed to widgets.

    <item*|<source-link|src/src/Texmacs/Window/|src/Texmacs/Window>>The kernel side of windows:
    <source-link|tm_window.cpp|src/Texmacs/Window/tm_window.cpp> (the class <cpp|tm_window_rep>, menus and icon
    bars of a window, auxiliary windows), <source-link|tm_frame.cpp|src/Texmacs/Window/tm_frame.cpp> (the
    <cpp|tm_frame_rep> part of the server), <source-link|tm_dialogue.cpp|src/Texmacs/Window/tm_dialogue.cpp>
    (dialogs and interactive commands) and <source-link|tm_button.cpp|src/Texmacs/Window/tm_button.cpp> (widgets
    displaying typeset boxes).

    <item*|<source-link|src/src/Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>>The <scheme>
    names of the widget constructors (<scm|widget-hmenu> and friends).

    <item*|<source-link|src/TeXmacs/progs/kernel/gui/|TeXmacs/progs/kernel/gui>>The <scheme> side:
    <source-link|gui-markup.scm|TeXmacs/progs/kernel/gui/gui-markup.scm>, <source-link|menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>,
    <source-link|menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>, <source-link|menu-convert.scm|TeXmacs/progs/kernel/gui/menu-convert.scm> and the examples
    in <source-link|menu-test.scm|TeXmacs/progs/kernel/gui/menu-test.scm>.

    <item*|<source-link|src/src/Plugins/Qt/|src/Plugins/Qt>>The <name|Qt> port (with a variant
    in <source-link|Plugins/Qt6|src/Plugins/Qt6>).

    <item*|<source-link|src/src/Plugins/NS/|src/Plugins/NS>>The native <name|Cocoa> port of
    <name|macOS> (<verbatim|--with-gui=cocoa>, macro <cpp|AQUATEXMACS>),
    modelled on the <name|Qt> port; it uses the <name|Objective-C> helpers
    of <source-link|Plugins/MacOS|src/Plugins/MacOS>, which the <name|Qt> port also uses on
    <name|macOS>.

    <item*|<source-link|src/src/Plugins/Vue/|src/Plugins/Vue>>The <name|Vue> port
    (<verbatim|--with-gui=vue>, macro <cpp|VUETEXMACS>): <name|SDL> 3
    windows and events, widgets laid out in immediate mode by <name|Clay>
    (<source-link|clay.h|src/Plugins/Vue/clay.h>), drawing with <name|MuPDF> or on the
    GPU.

    <item*|<source-link|src/src/Plugins/Widkit/|src/Plugins/Widkit>>The <TeXmacs> own widget
    kit, used by three ports which only provide windows, events and
    drawing: <source-link|Plugins/X11|src/Plugins/X11> (<verbatim|--with-gui=x11>),
    <source-link|Plugins/SDL|src/Plugins/SDL> (<verbatim|--with-gui=sdl>) and
    <source-link|Plugins/Qtwk|src/Plugins/Qtwk> (<verbatim|--with-gui=qtwk>, <name|Qt> as a
    platform layer only).
  </description-paragraphs>

  How a port is selected and built, and the details of each port, are
  described in the chapter on \P<hlink|graphical ports|guiports.en.tm>\Q,
  in particular \P<hlink|The <name|Vue> port|guiports-vue.en.tm>\Q and
  \P<hlink|The <name|Cocoa> port|guiports-cocoa.en.tm>\Q.

  <section|Contents>

  <\traverse>
    <branch|The abstract widget interface in <c++>|widgets-cpp.en.tm>

    <branch|Windows, the main <TeXmacs> widget and the flow of
    events|widgets-window.en.tm>

    <branch|The <scheme> widget language and its
    interpreter|widgets-scheme.en.tm>

    <branch|The <name|Qt> implementation|widgets-qt.en.tm>

    <branch|Adding new widgets and porting to other
    toolkits|widgets-port.en.tm>
  </traverse>

  <section|Related documents>

  The user-level description of the <scheme> widget language, with many
  examples, is found in \P<hlink|Extending the graphical user
  interface|../scheme/gui/scheme-gui.en.tm>\Q and its complete keyword list
  in the \P<hlink|Widgets reference guide|../scheme/gui/scheme-gui-reference.en.tm>\Q.
  Window manipulation from <scheme> is documented in \P<hlink|Manipulating
  <TeXmacs> windows|../scheme/buffer/window-api.en.tm>\Q.

  The older document \P<hlink|The graphical user interface|gui.en.tm>\Q
  describes the original <name|X11> toolkit of <TeXmacs> (the widget,
  event and attribute classes that now live in
  <source-link|Plugins/Widkit|src/Plugins/Widkit>). The abstract interface described here
  replaced direct use of <name|Widkit> in the kernel: the <name|Qt>,
  <name|Cocoa> and <name|Vue> ports do not use that event model, which
  survives only inside the <name|X11>, <name|SDL> and <name|Qtwk> ports.

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
