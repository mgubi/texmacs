<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The graphical user interface (historical Widkit toolkit)>

  <\warning>
    This chapter describes the original graphical toolkit of <TeXmacs>,
    called <em|Widkit>, together with its <name|X11> backend. This toolkit
    still lives in the directories <source-link|src/src/Plugins/Widkit|src/Plugins/Widkit> and
    <source-link|src/src/Plugins/X11|src/Plugins/X11>. The widgets of Widkit
    are used by three of the current ports, which only differ by the
    implementation of the window interface (<cpp|window_rep>):
    <name|X11> (<verbatim|./configure --with-gui=x11>, <cpp|x_window_rep>),
    <name|SDL> (<verbatim|--with-gui=sdl>, <cpp|sdl_window_rep> in
    <source-link|Plugins/SDL|src/Plugins/SDL>, drawing with <name|MuPDF>)
    and <name|Qtwk> (<verbatim|--with-gui=qtwk>, <cpp|qtwk_window_rep> in
    <source-link|Plugins/Qtwk|src/Plugins/Qtwk>, <name|Qt> as a mere
    platform layer). The default user interface is based on the native
    widgets of <name|Qt> (<source-link|Plugins/Qt|src/Plugins/Qt>), and the <name|Vue>
    (<source-link|Plugins/Vue|src/Plugins/Vue>) and <name|Cocoa> (<source-link|Plugins/NS|src/Plugins/NS>) ports
    have widgets of their own; all user interfaces implement the abstract
    widget interface from <source-link|src/src/Graphics/Gui|src/Graphics/Gui>. For an up-to-date
    description of the abstract widget system, of the main <TeXmacs> window
    and of the <scheme> widget language, we refer to the chapter on the
    <hlink|abstract widget system|widgets.en.tm>; for the graphical output,
    see the chapter on <hlink|renderers|renderer.en.tm>. The present chapter
    dates back to the early days of <TeXmacs>; it has been revised so that
    the names used below correspond to the current sources, but it should
    mainly be read as a description of the design of Widkit.
  </warning>

  <section|Introduction>

  <subsection|Main architecture>

  The Widkit toolkit has two main components: an abstract window interface,
  which is very similar to X Window, and the actual toolkit. The abstract
  window interface was first implemented for X Window, and later for
  <name|SDL> and for <name|Qt> (the <name|Qtwk> port).

  The <name|X11> implementation of the window interface consists of three
  main classes (see <source-link|Plugins/X11|src/Plugins/X11>): <cpp|x_gui_rep>,
  <cpp|x_window_rep> and <cpp|x_drawable_rep>. The <cpp|x_gui_rep> class
  (there is only one instance <cpp|the_gui>) is responsible for

  <\itemize>
    <item>The connection with the X server.

    <item>Managing server resources such as colors and fonts.

    <item>Inter-window communication (i.e. selections and so).

    <item>Redirection of input/output (pointer and keyboard grabs).

    <item>The main event loop.
  </itemize>

  The <cpp|x_window_rep> class implements the abstract class
  <cpp|window_rep> from <source-link|Graphics/Gui/window.hpp|src/Graphics/Gui/window.hpp> and is
  responsible for

  <\itemize>
    <item>The part of the layout of a window, which is negotiated with the
    window manager.

    <item>Implementation of some clipping and region translation routines.

    <item>Maintaining the list of regions to be repainted.

    <item>Delegation of events to the widget associated to the window.
  </itemize>

  The graphical routines are provided by the <cpp|x_drawable_rep> class,
  which inherits from the abstract <cpp|renderer_rep> class
  (<source-link|Graphics/Renderer/renderer.hpp|src/Graphics/Renderer/renderer.hpp>). The same renderer interface
  is implemented for printing and for other output devices. Hence, user
  applications draw their graphics on a <cpp|renderer>, which allows them
  to visualize them in a window or to print them. In the original design,
  windows directly inherited from the \PPostScript device\Q class, which
  played the role of renderers.

  The graphical toolkit built on top of the abstract window interface is
  widget oriented. A large number of widget classes are implemented, which
  all inherit from the abstract <cpp|wk_widget_rep> class, which itself
  derives from the abstract <cpp|widget_rep> class of
  <source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp>. Widgets may have a finite number of
  children and they are responsible for

  <\itemize>
    <item>Their, and their children, sizes and positioning in the window.

    <item>Reaction on events, which are either redraw requests, keyboard or
    pointer events or other miscellaneous events.

    <item>Other functionalities, which depend on the particular widget class.
  </itemize>

  <subsubsection|A simple example>

  In order to create a window \PTest\Q with the text \PHello world\Q in it,
  one first opens the connection with the graphical system, then creates
  the widget with the text and finally the window, with the widget attached
  to it. Schematically:

  <\cpp-code>
    gui_open (argc, argv);

    wk_widget wid= text_wk_widget ("Hello world");

    wk_widget win= plain_window_widget (wid, "Test");
  </cpp-code>

  Technically speaking, the window creation amounts to several actions:

  <\itemize>
    <item>The widget is questioned for an appropriate size for it (actually,
    only minimal, default and maximal size hints are computed, using
    <cpp|get_size> events).

    <item>The window is created.

    <item>The widget is \Pattached\Q to the window.

    <item>The subwidgets are \Ppositioned\Q at their appropriate places and
    they are given appropriate sizes within their parents.
  </itemize>

  The next step is to make the window visible by

  <\cpp-code>
    win-\<gtr\>win-\<gtr\>set_visibility (true);
  </cpp-code>

  At this point repaint requests are generated, which are handled when
  starting the event loop by

  <\cpp-code>
    gui_start_loop ();
  </cpp-code>

  The event loop terminates when all windows have been destroyed. A window
  widget <cpp|win> is destroyed using

  <\cpp-code>
    destroy_window_widget (win);
  </cpp-code>

  At the end, the connection with the graphical system is closed by

  <\cpp-code>
    gui_close ();
  </cpp-code>

  The user only has to bother about destroying windows; widgets are
  automatically destroyed if they are no longer being referenced to.

  <subsection|Widgets and event processing>

  From the implementation point of view, widgets are pointers to instances of
  the abstract widget representation class <cpp|wk_widget_rep>. Moreover,
  the <cpp|wk_widget> class supports reference counting. The
  <cpp|wk_widget_rep> class contains information about the window it is
  attached to and its location within this window, its size, the position of
  the origin of its local coordinates (<cpp|north_west>, <cpp|north>,
  <abbr|etc.>) and its children. The <cpp|wk_widget_rep> class also provides
  a virtual event handler <cpp|wk_widget_rep::handle>. This handler returns
  true if the event could be handled.

  The implemented widget representation classes are organized in a
  hierarchy, which contains both concrete and abstract classes. The abstract
  classes reimplement the virtual event handler <cpp|handle> in such a way
  that if the event is recognized, then the event is dispatched to a more
  particular virtual event handler, possibly after some processing.

  For example, instances of the <cpp|basic_widget_rep> class
  (<source-link|Plugins/Widkit/basic_widget.hpp|src/Plugins/Widkit/basic_widget.hpp>) can handle the most common
  events, such as keyboard, mouse, repaint events and so on. If a key is
  pressed, then the virtual function <cpp|basic_widget_rep::handle_keypress>
  is called with an argument of type <cpp|keypress_event>. The default
  implementation of this virtual function does nothing.

  The user can also create his own events, which he can pass to any widget.
  For instance in order to invalidate a region for redrawing, one creates an
  invalidate event using <cpp|emit_invalidate> and sends it to the
  appropriate widget using the operator <cpp|\<less\>\<less\>>. Notice that
  the user is responsible for sending events to widgets which can handle
  them. Bad matches are only discovered at run time, in which case a warning
  is printed by <cpp|\<less\>\<less\>>.

  <subsubsection|A simple example>

  Suppose that we want to make a widget, which tracks keyboard events and
  which displays them. Such a widget must have a construction routine and
  keyboard and repaint handlers:

  <\cpp-code>
    class track_widget_rep: public basic_widget_rep {

    \ \ string last_key;

    public:

    \ \ track_widget_rep ();

    \ \ operator tree ();

    \ \ void handle_keypress (keypress_event ev);

    \ \ void handle_repaint (repaint_event ev);

    };
  </cpp-code>

  The constructor is taken to be empty and places the origin at the center of
  the widget

  <\cpp-code>
    track_widget_rep::track_widget_rep (): basic_widget_rep (center) {}
  </cpp-code>

  In particular <cpp|last_key> is initialized by the empty string. The
  conversion to trees is only used for debugging purposes:

  <\cpp-code>
    track_widget_rep::operator tree () {

    \ \ return tree (TUPLE, "track", last_key); }
  </cpp-code>

  We also define the function

  <\cpp-code>
    wk_widget

    track_widget () {

    \ \ return tm_new\<less\>track_widget_rep\<gtr\> ();

    }
  </cpp-code>

  in order to create an instance of a <cpp|track_widget>.

  The event handler for keyboard events should just reset the string
  <cpp|last_key> and invalidate the entire widget region.

  <\cpp-code>
    void

    track_widget_rep::handle_keypress (keypress_event ev) {

    \ \ last_key= ev-\<gtr\>key;

    \ \ this \<less\>\<less\> emit_invalidate_all ();

    }
  </cpp-code>

  The event handler for repainting first determines the string to be
  repainted as a function of <cpp|last_key>, computes its extents and
  repaints it at the center. The renderer on which the widget should be
  drawn is part of the repaint event.

  <\cpp-code>
    void

    track_widget_rep::handle_repaint (repaint_event ev) {

    \ \ renderer ren= ev-\<gtr\>win;

    \ \ string s= (last_key == ""? string ("No key pressed"): "Pressed " * last_key);

    \ \ font fn= get_default_font ();

    \ \ metric ex;

    \ \ fn-\<gtr\>get_extents (s, ex);

    \ \ layout_default (ren, ev-\<gtr\>x1, ev-\<gtr\>y1, ev-\<gtr\>x2, ev-\<gtr\>y2);

    \ \ ren-\<gtr\>set_pencil (black);

    \ \ fn-\<gtr\>draw (ren, s, -(ex-\<gtr\>x1+ex-\<gtr\>x2)\<gtr\>\<gtr\>1, -(ex-\<gtr\>y1+ex-\<gtr\>y2)\<gtr\>\<gtr\>1);

    }
  </cpp-code>

  Real widgets, such as the text widget in
  <source-link|Plugins/Widkit/Output/text_widget.cpp|src/Plugins/Widkit/Output/text_widget.cpp>, follow this pattern
  but take additional care of styles, colors and shrinking factors.

  <section|The abstract window interface>

  The abstract interface for windows is the class <cpp|window_rep> in
  <source-link|Graphics/Gui/window.hpp|src/Graphics/Gui/window.hpp>. It provides methods to change the
  name, the size, the position and the visibility of the window, to
  invalidate regions (<cpp|invalidate>), to translate regions, to manage
  the keyboard focus (<cpp|set_keyboard_focus>) and to grab the mouse
  (<cpp|set_mouse_grab>). Global operations, such as opening and closing
  the connection with the graphical system, the event loop, selections and
  default fonts are provided by the functions in
  <source-link|Graphics/Gui/gui.hpp|src/Graphics/Gui/gui.hpp>, such as <cpp|gui_open>,
  <cpp|gui_start_loop>, <cpp|gui_close>, <cpp|set_selection> and
  <cpp|get_selection>.

  <section|Widget principles>

  <subsection|The widget class>

  Widgets are pointers to instances of the abstract widget representation
  class <cpp|wk_widget_rep>. Widgets support reference counting, so that a
  widget is automatically destroyed if it is not used any more (except in the
  case of circular referencing; see below). As a general rule, the user does
  not have to worry about the creation and destruction of widgets.

  <subsubsection|The widget representation class>

  The definition of the <cpp|wk_widget_rep> class
  (<source-link|Plugins/Widkit/wk_widget.hpp|src/Plugins/Widkit/wk_widget.hpp>) goes essentially as follows:

  <\cpp-code>
    class wk_widget_rep: public widget_rep {

    public:

    \ \ window \ \ win; \ \ \ \ \ \ \ \ \ \ \ \ \ // underlying window

    \ \ SI \ \ \ \ \ \ ox, oy; \ \ \ \ \ \ \ \ \ \ // origin of widget in window

    \ \ SI \ \ \ \ \ \ w, h; \ \ \ \ \ \ \ \ \ \ \ \ // width and height of widget

    \ \ gravity \ grav; \ \ \ \ \ \ \ \ \ \ \ \ // position of the origin in the widget

    \ \ array\<less\>wk_widget\<gtr\> \ a; \ \ \ \ \ \ // children of widget

    \ \ array\<less\>string\<gtr\> \ \ \ \ name; \ \ \ // names for the children

    \;

    \ \ wk_widget_rep (array\<less\>wk_widget\<gtr\> a, array\<less\>string\<gtr\> name, gravity grav);

    \ \ virtual operator tree () = 0;

    \ \ virtual bool handle (event ev) = 0;

    \;

    \ \ SI \ \ \ x1 (); SI y1 (); // lower left window coordinates of widget

    \ \ SI \ \ \ x2 (); SI y2 (); // upper right window coordinates of widget

    \ \ bool \ attached (); \ \ \ \ // tests whether (win != NULL)

    \ \ ...

    };
  </cpp-code>

  The <cpp|win> field specifies the window to which the widget is attached
  (<cpp|win=NULL>, by default). The origin <cpp|(ox,oy)> of the widget is
  specified with respect to the window's origin. Next come the width
  <cpp|w> and the height <cpp|h> of the widget. The gravity <cpp|grav>
  determines where the origin of the widget is located (<cpp|north_west>,
  <cpp|north>, <abbr|etc.>). The array <cpp|a> specifies the children of the
  widget. The array <cpp|name> gives names to the children of the widget.
  This is useful for addressing children by comprehensible names; the names
  are also useful for designing menu widgets.

  The virtual type casting operator for trees is used for debugging
  purposes; mainly in order to print widgets. The virtual member function
  <cpp|handle> processes an event which is sent to the widget and returns
  <cpp|true> if the event could be handled and <cpp|false> if not. The
  methods <cpp|send>, <cpp|query>, <cpp|notify>, <cpp|read> and
  <cpp|write> implement the abstract widget interface from
  <source-link|Graphics/Gui/widget.hpp|src/Graphics/Gui/widget.hpp> in terms of Widkit events (see
  <source-link|Plugins/Widkit/Basic/widkit_wrapper.cpp|src/Plugins/Widkit/Basic/widkit_wrapper.cpp>).

  <subsubsection|The widget class>

  The <cpp|wk_widget> class is declared using the macro
  <cpp|ABSTRACT_NULL> (see the chapter on <hlink|basic data
  types|types.en.tm>), with the following additional methods:

  <\cpp-code>
    class wk_widget {

    public:

    ABSTRACT_NULL(wk_widget);

    \ \ inline wk_widget operator [] (int i) { return rep-\<gtr\>a[i]; }

    \ \ wk_widget operator [] (string s);

    \ \ inline operator tree () { return (tree) (*rep); }

    \ \ inline bool operator == (wk_widget w) { return rep == w.rep; }

    \ \ inline bool operator != (wk_widget w) { return rep != w.rep; }

    };
  </cpp-code>

  Widgets may be constructed in two ways. First, we may construct a symbolic
  \Pnil\Q widget by <cpp|wk_widget ()>. The function <cpp|is_nil> is
  provided in order to test whether a widget is \Pnil\Q. Secondly, we may
  construct a widget from a pointer of type <cpp|wk_widget_rep*>. The
  functions <cpp|abstract> and <cpp|concrete> convert between
  <cpp|wk_widget> and the abstract <cpp|widget> type.

  The reference counting mechanism ensures widgets to be destroyed when they
  are no longer pointed to. An important exception is when two widgets point
  one to each other, which fools the reference counter (for instance a
  scrollbar and the widget which is scrolled need to point one to each
  other). In order to deal with such \Pcircular dependencies\Q, one works
  directly with <cpp|wk_widget_rep*> pointers if one does not want the
  pointer to be taken into account in the reference counter.

  Child widgets can again be accessed in two ways. First, we have the
  direct way, using its index in the array <cpp|a>. Secondly, we can access
  a child via its name. Actually, when using this method, a
  <cpp|get_widget> event is generated. In the basic widget class, the
  default action for this event is to search in the name array for the
  child. However, the user may override this default action and provide
  another child searching method.

  <subsection|The event class>

  Events are pointers to instances of the abstract <cpp|event_rep> class,
  which supports reference counting. Actually, concrete event representation
  classes just contain some information. Hence, events actually provide a
  safe and generic way to store and communicate information.

  <subsubsection|The event representation class>

  The definition of the <cpp|event_rep> structure
  (<source-link|Plugins/Widkit/event.hpp|src/Plugins/Widkit/event.hpp>) is as follows:

  <\cpp-code>
    struct event_rep: public abstract_struct {

    \ \ int \ \ \ \ type; \ // the event type

    \ \ inline \ event_rep (int type2): type (type2) {}

    \ \ inline \ virtual ~event_rep () {}

    \ \ virtual operator tree () = 0; \ \ // for displaying events (debugging)

    };
  </cpp-code>

  The <cpp|type> field gives the type of the event. A complete list of the
  event types is given in the file

  <\verbatim-code>
    Plugins/Widkit/Event/event_codes.hpp
  </verbatim-code>

  When adding new event types, one should make sure that the numbers of all
  event types are different. Unfortunately, there is no safe way in order to
  let this job be done by the compiler.

  <subsubsection|The event class>

  The <cpp|event> structure is defined by

  <\cpp-code>
    struct event {

    ABSTRACT(event);

    \ \ inline operator tree () { return (tree) (*rep); }

    \ \ friend class event_ptr_base;

    };

    ABSTRACT_CODE(event);
  </cpp-code>

  <subsubsection|Concrete event classes>

  Concrete event classes again come into two parts: the class itself and its
  representation class. For instance, the representation class for
  <cpp|get_widget> events is defined in
  <source-link|Plugins/Widkit/Event/basic_event.hpp|src/Plugins/Widkit/Event/basic_event.hpp> by

  <\cpp-code>
    struct get_widget_event_rep: public event_rep {

    \ \ string which; wk_widget& w;

    \ \ get_widget_event_rep (string which, wk_widget& w);

    \ \ operator tree ();

    };

    EVENT(get_widget_event);
  </cpp-code>

  The macro <cpp|EVENT> from <source-link|event_codes.hpp|src/Plugins/Widkit/Event/event_codes.hpp> defines the
  corresponding <cpp|get_widget_event> class as
  <cpp|event_ptr\<less\>get_widget_event_rep\<gtr\>>. The template
  <cpp|event_ptr> provides the operator <cpp|-\<gtr\>> for accessing the
  fields of the representation class, as well as conversions from and to the
  generic <cpp|event> class. Moreover, the generic <cpp|event> class and the
  specific <cpp|get_widget_event> class are compatible from the reference
  counting point of view.

  The implementation of the <cpp|get_widget_event_rep> class is as follows:

  <\cpp-code>
    get_widget_event_rep::get_widget_event_rep (string ww, wk_widget& w2):

    \ \ event_rep (GET_WIDGET_EVENT), which (ww), w (w2) {}

    get_widget_event_rep::operator tree () {

    \ \ return tree (TUPLE, "get_widget_event", which); }
  </cpp-code>

  The actual events are created by

  <\cpp-code>
    event get_widget (string which, wk_widget& w) {

    \ \ return tm_new\<less\>get_widget_event_rep\<gtr\> (which, w); }
  </cpp-code>

  <subsubsection|Event handlers>

  Implementations of the generic event handler
  <cpp|bool wk_widget_rep::handle (event)> usually do the following

  <\itemize>
    <item>Determine the event type.

    <item>Perform some action, depending on the event type and the widget.

    <item>Dispatch the event to a concrete or abstract specific event handler
    or to the generic event handler of some other widget representation
    class.
  </itemize>

  For instance, the event handler for composite widgets is as follows:

  <\cpp-code>
    bool

    composite_widget_rep::handle (event ev) {

    \ \ switch (ev-\<gtr\>type) {

    \ \ case CLEAN_EVENT:

    \ \ \ \ handle_clean (ev);

    \ \ \ \ return true;

    \ \ case INSERT_EVENT:

    \ \ \ \ handle_insert (ev);

    \ \ \ \ return true;

    \ \ case REMOVE_EVENT:

    \ \ \ \ handle_remove (ev);

    \ \ \ \ return true;

    \ \ }

    \ \ return basic_widget_rep::handle (ev);

    }
  </cpp-code>

  The member function <cpp|handle_insert> essentially does the following:

  <\cpp-code>
    void

    composite_widget_rep::handle_insert (insert_event ev) {

    \ \ a \<less\>\<less\> ev-\<gtr\>w;

    \ \ name \<less\>\<less\> ev-\<gtr\>s;

    }
  </cpp-code>

  In particular, we can retrieve the fields <cpp|w> and <cpp|s> from
  <cpp|insert_event_rep> from the <cpp|insert_event> in the member function.

  <subsubsection|Adding your own event classes>

  Summarizing, in order to add your own new event classes, you have to take
  care of the following steps:

  <\itemize>
    <item>Add a new event type to <source-link|event_codes.hpp|src/Plugins/Widkit/Event/event_codes.hpp>.

    <item>Declare and implement the event type and its representation type.

    <item>Declare and implement the event creation functions.

    <item>Reimplement the generic event handler
    <cpp|bool wk_widget_rep::handle (event)> in the abstract or concrete
    widget representation class, where you want to use your new event class.
  </itemize>

  <subsection|The main event loop>

  The main event loop (<cpp|x_gui_rep::event_loop> in
  <source-link|Plugins/X11/x_loop.cpp|src/Plugins/X11/x_loop.cpp>) does the following

  <\itemize>
    <item>As long as the application did not destroy all its windows (and
    as long as there are active network connections), wait for a new event
    to occur.

    <item>If an event occurs, handle it, by creating the appropriate events
    and sending them to the appropriate widgets (job of the window interface
    implementation).

    <item>Call the \Pinterpose handler\Q (see <cpp|gui_interpose>), which
    allows the application to do some work between two events. <TeXmacs>
    uses this handler for communicating with plug-ins, for the execution of
    delayed commands and for typesetting the parts of the documents which
    have been modified. Indeed, the event handling phase may consist of many
    complex actions, so that the regions to invalidate may be determined
    easier <with|font-shape|italic|a posteriori>.

    <item>Repaint the regions which have been invalidated during the event
    processing stages, starting with the window which has the focus.

    <item>Handle delayed messages (alarms) whose time has come.
  </itemize>

  <subsection|Coordinates>

  <subsubsection|Coordinates, pixels and rounding>

  All coordinates and sizes are represented by instances of type <cpp|SI>,
  which is nothing but another name for <cpp|int>. The <cpp|SI> constant
  <cpp|PIXEL> (which is equal to 256) denotes the size of a pixel on the
  screen. Since <cpp|PIXEL\<gtr\>1>, coordinates and sizes are not
  necessarily integer multiples of the pixel size. However, the coordinates
  of the origin and the size of a widget should always be such multiples.

  In order to achieve this, some rounding functions are provided by the
  renderers. The member function <cpp|round (SI& x, SI& y)> rounds its
  arguments to integer multiples of <cpp|PIXEL>. Furthermore, the member
  functions

  <\cpp-code>
    void inner_round (SI& x1, SI& y1, SI& x2, SI& y2);

    void outer_round (SI& x1, SI& y1, SI& x2, SI& y2);
  </cpp-code>

  transform a rectangle into a new one with integer multiple of <cpp|PIXEL>
  coordinates, which is enclosed <abbr|resp.> encloses the original
  rectangle. Similar functions <cpp|abs_round>, <cpp|abs_inner_round>
  <abbr|etc.> are available outside renderers.

  <subsubsection|Local and global coordinates>

  Each widget has an origin <cpp|(ox,oy)> with respect to the window to which
  it has been attached. This is the origin of the \Plocal coordinates\Q. The
  origin of the \Pglobal coordinates\Q is the origin of the window. The
  location of the local origin in the widget is determined by the widget's
  gravity, which is either one of <cpp|north_west>, <cpp|north>,
  <cpp|north_east>, <cpp|west>, <cpp|center>, <cpp|east>, <cpp|south_west>,
  <cpp|south> or <cpp|south_east>.

  As a general rule, events are transmitted in global coordinates.
  Nevertheless, in widgets which are derived from the abstract basic widget
  class, by default, all computations are done with respect to local
  coordinates. This is due to two reasons

  <\itemize>
    <item>When an event has to be processed, the abstract widget event
    handler translates global into local coordinates and calls the
    appropriate virtual event handler using local coordinates.

    <item>When an event has to be emitted, the abstract widget provides event
    construction routines w.r.t. local coordinates (such as
    <cpp|basic_widget_rep::emit_position> or
    <cpp|basic_widget_rep::emit_invalidate>), which override the global
    event construction routines w.r.t global coordinates.
  </itemize>

  <subsubsection|Screen coordinates>

  For some very particular purposes, such as popping up windows, one has to
  perform computations with respect to the screen coordinates. Given a point
  <cpp|(x,y)> in the coordinates of some window <cpp|win>, the screen
  coordinates of <cpp|(x,y)> are obtained by adding the window's origin,
  which is obtained by calling <cpp|win-\<gtr\>get_position (ox,oy)>.

  <subsection|Attaching and positioning widgets>

  <subsubsection|Attaching widgets>

  When a widget is created, the <cpp|win> field of its representation is set
  to <cpp|NULL>, since it is not yet attached to a window. In order to
  attach a widget <cpp|w> to a window <cpp|win>, one emits an
  <cpp|attach_window_event>:

  <\cpp-code>
    w \<less\>\<less\> emit_attach_window (win);
  </cpp-code>

  Notice that taking <cpp|win==NULL> results in detaching the widget.
  Notice also that a widget may be attached to at most one window: attempts
  to reattach a widget, which is already attached, to another window, result
  in an error.

  Some events can be handled by widgets which are not yet attached to a
  window, such as:

  <\itemize>
    <item>\Pget size\Q events, which determine the default, minimal and
    maximal size of a widget. Such an event is generated before the creation
    of the window to which the widget will be attached in order to determine
    the size of the window.

    <item>\Pattach window\Q events, in order to attach (or detach) a window.

    <item>Events for setting (and getting) attributes: after the creation of
    a widget some attributes of the widget may be given some default value.
    In order to change them, one might wish to set them to other values
    before attaching the widget to a window.

    <item>Events for modifying the composite structure of a widget: these
    events are used for instance in order to construct menus.
  </itemize>

  For some of these events, such as attribute changes, it may be necessary to
  emit invalidate events in case when the widget had been attached to some
  window. In order to test this one uses the member function

  <\cpp-code>
    bool wk_widget_rep::attached ();
  </cpp-code>

  <subsubsection|Positioning widgets>

  When an appropriate size <cpp|(w,h)> has been determined for a widget
  (using \Pget size\Q events) and when a widget has been attached to some
  window, the widget is positioned in the main window. By default, all
  children are recursively positioned at the top left of the window at sizes
  <cpp|(w,h)>. But for complex widgets with children, a specific positioning
  routine usually has to be implemented.

  Such a routine involves positioning of the children within the parent.
  This is done by emitting position events to the children. For instance,

  <\cpp-code>
    a[i] \<less\>\<less\> emit_position (x[i], y[i], w[i], h[i], center);
  </cpp-code>

  positions the <cpp|i>-th child, such that the origin of <cpp|a[i]> is at
  position <cpp|(x[i], y[i])> w.r.t. the local coordinates of <cpp|this> and
  such that the origin is situated in the center of <cpp|a[i]>. The width and
  height of <cpp|a[i]> are set to <cpp|w[i]> <abbr|resp.> <cpp|h[i]>.

  <subsubsection|Repositioning widgets>

  During execution, it may happen that a particular widget has changed, so
  that it obtains a different size and/or position. In this case, one emits
  an <cpp|update_event> to the closest ancestor, whose position and size did
  not change.

  For instance, consider the case of a footer <cpp|footer>, which consists
  of a left footer <cpp|footer["left"]>, followed by some glue
  <cpp|footer["middle"]> and a right footer <cpp|footer["right"]>. When the
  left footer changes:

  <\cpp-code>
    footer \<less\>\<less\> set_widget ("left", text_wk_widget ("new text"));
  </cpp-code>

  the size of <cpp|footer["left"]> changes, and the size and position of the
  glue should also be changed. Nevertheless, the size and position of
  <cpp|footer> remain unaltered, whence we update <cpp|footer>:

  <\cpp-code>
    footer \<less\>\<less\> emit_update ();
  </cpp-code>

  Updating an attached widget results in three actions to take place:

  <\itemize>
    <item>The widget is reattached to its own window. Indeed, some children
    of the widget might need be attached.

    <item>The widget is repositioned at its current position and size. Again
    this will actually affect the children.

    <item>The widget is invalidated, so that it will be repainted.
  </itemize>

  <subsection|The keyboard>

  <subsubsection|Keyboard focus>

  Each window <cpp|win> on the screen determines a main widget which is
  attached to it and a descendant <cpp|win-\<gtr\>kbd_focus> of this
  widget, which handles the keyboard input directed to the window. This
  latter widget, which is set to the main widget by default, is said to
  have keyboard focus, if the window <cpp|win> has keyboard focus (i.e. if
  all keyboard events are sent to this window). Consequently, the widget
  which has keyboard focus receives all keyboard events.

  When the keyboard focus of a window <cpp|win> changes, a
  <cpp|keyboard_focus_event> is sent to the widget with the keyboard focus.
  The field <cpp|ev-\<gtr\>flag> of this event <cpp|ev> is <cpp|true> if
  the window got the focus, and <cpp|false> if the window lost focus.

  The keyboard focus widget associated to a window can be changed by calling
  the <cpp|window_rep> member function

  <\cpp-code>
    void window_rep::set_keyboard_focus (widget wid, bool get_focus= true);
  </cpp-code>

  Setting the input focus to another widget than the main widget is useful,
  for instance, if a particular text input field of some form needs keyboard
  focus after a mouse click on it.

  <subsubsection|Keyboard events>

  When a widget has the keyboard focus, and a key is pressed, it receives a
  <cpp|keypress_event>. The <cpp|keypress_event_rep> class contains a field
  <cpp|key>, which contains a comprehensible string corresponding to the key
  which was pressed, and a field <cpp|t> with the time of the event.

  More precisely, <cpp|key> is either a one character string, a universal
  symbol like <verbatim|"\<less\>alpha\<gtr\>">, or a symbolic name like
  <verbatim|"return">, <verbatim|"right">, <verbatim|"delete">, <abbr|etc.>
  Modified keys are obtained by prefixing the key by <verbatim|"S-">
  (shift), <verbatim|"C-"> (control), <verbatim|"A-"> (alt) or
  <verbatim|"M-"> (meta), as in <verbatim|"S-F1">, <verbatim|"C-escape"> or
  <verbatim|"A-x">. The names of the special keys include:

  <\verbatim-code>
    "F1", "F2", ..., "F12", "escape", "tab", "backspace", "delete",

    "return", "space", "insert", "home", "end", "pageup", "pagedown",

    "left", "up", "down", "right"
  </verbatim-code>

  The complete translation of the <name|X11> key symbols can be found in
  <source-link|Plugins/X11/x_init.cpp|src/Plugins/X11/x_init.cpp>. The same naming conventions are used by
  the other user interfaces, and by the keyboard bindings in <scheme>.

  <subsection|The mouse>

  <subsubsection|Mouse events>

  A mouse event <cpp|ev> occurs on a button change or a mouse movement. The
  <cpp|ev-\<gtr\>type> field contains the type of the event and
  <cpp|ev-\<gtr\>x> and <cpp|ev-\<gtr\>y> the corresponding coordinates of
  the mouse. Finally, the states of the mouse buttons can be questioned
  using the routine <cpp|ev-\<gtr\>pressed (string)>.

  The possible values of <cpp|ev-\<gtr\>type> on button change events are
  the following:

  <\verbatim-code>
    "press-left", "press-middle", "press-right", "press-up", "press-down",

    "release-left", "release-middle", "release-right", "release-up",
    "release-down"
  </verbatim-code>

  where <verbatim|"up"> and <verbatim|"down"> correspond to the mouse wheel.
  The possible values for mouse movement events are

  <\verbatim-code>
    "move", "enter", "leave"
  </verbatim-code>

  The <verbatim|"enter"> and <verbatim|"leave"> events occur when the mouse
  enters <abbr|resp.> leaves the widget. Finally, the states of the left,
  middle and right mouse buttons can respectively be obtained using the
  calls

  <\cpp-code>
    ev-\<gtr\>pressed ("left")

    ev-\<gtr\>pressed ("middle")

    ev-\<gtr\>pressed ("right")
  </cpp-code>

  <subsubsection|Grabbing the mouse>

  For some applications such as popup menus or scrollbars, it is useful to
  direct all mouse events to a particular widget <cpp|w>. This is done by
  grabbing the mouse by

  <\cpp-code>
    wk_grab_pointer (w);
  </cpp-code>

  After such a grab, all mouse events are directed to <cpp|w>. The mouse
  grab is released by

  <\cpp-code>
    wk_ungrab_pointer (w);
  </cpp-code>

  Actually, the <cpp|x_gui_rep> class keeps track of a list of widgets for
  which a mouse grab occurred: if the mouse is grabbed by widgets <cpp|w1>
  next <cpp|w2>, and again ungrabbed by <cpp|w2>, then all mouse events are
  again sent to <cpp|w1>. This feature is useful for successive grabs by
  recursive popup menus. The widgets are informed of the changes by
  <cpp|mouse_grab_event>s.

  When a widget <cpp|w1> grabs the mouse, and a previous mouse grab on a
  widget <cpp|w2> is still active, then a <verbatim|"leave"> event is sent
  to <cpp|w2> and an <verbatim|"enter"> event to <cpp|w1>. Similarly, if
  <cpp|w1> releases the grab, then a <verbatim|"leave"> event is sent to
  <cpp|w1> and an <verbatim|"enter"> event to <cpp|w2>.

  <subsection|The screen>

  Each window keeps track of a list of rectangles to be repainted (moreover,
  redundant rectangles are eliminated automatically and adjacent rectangles
  are transformed in larger rectangles). During the repaint stage in the
  event loop, the widget is requested to repaint these rectangles.

  <subsubsection|Repainting rectangles>

  The repaint handler takes on input a <cpp|repaint_event ev>, which
  determines the renderer <cpp|ev-\<gtr\>win> on which to draw and the
  rectangle to be repainted. Moreover, <cpp|repaint_event_rep> contains a
  boolean field <cpp|stop>, which can be set in order to indicate that the
  repaint process was stopped somewhere in the middle.

  Indeed, for widgets which take a long time to be repainted, it may be
  useful to abort repainting if a key is pressed. The arrival of an event
  which aborts repainting can be checked using the function
  <cpp|check_event (INTERRUPT_EVENT)> or <cpp|gui_interrupted ()> from
  <source-link|Graphics/Gui/gui.hpp|src/Graphics/Gui/gui.hpp>.

  If the application decides to abort repainting, it sets
  <cpp|ev-\<gtr\>stop> to <cpp|true>. The rectangle which was being
  repainted is put back on the invalid rectangles list in the event loop; it
  will be processed again during the next pass through the repaint phase.

  <subsubsection|Invalidation of rectangles>

  When a window is mapped on the screen or when a region is exposed, the
  window interface automatically invalidates the corresponding rectangle.
  The user may also invalidate a rectangle by using either one of the
  routines

  <\cpp-code>
    event emit_invalidate_all ();

    event emit_invalidate (SI x1, SI y1, SI x2, SI y2);
  </cpp-code>

  The first routine creates an event to invalidate the entire widget area;
  the other routine invalidates a specified region.

  <subsection|The toolkit>

  The constructors of the standard widgets are declared in
  <source-link|Plugins/Widkit/wk_widget.hpp|src/Plugins/Widkit/wk_widget.hpp>; their implementations can be
  found in the subdirectories of <source-link|Plugins/Widkit|src/Plugins/Widkit>
  (<verbatim|Basic>, <verbatim|Composite>, <verbatim|Attribute>,
  <verbatim|Output>, <verbatim|Button>, <verbatim|Input>,
  <verbatim|Scrollable> and <verbatim|Misc>).

  <subsubsection|Other standard widget classes>

  Many widgets from the toolkit are derived from some other standard abstract
  widget classes, which can handle some other special events.

  <subsubsection|Composite widgets>

  These widgets allow to add or remove children to or from a widget. This
  makes them particularly useful for menu widgets. They respond to
  <cpp|clean>, <cpp|insert> and <cpp|remove> events (created by
  <cpp|emit_clean>, <cpp|emit_insert> and <cpp|emit_remove>).

  <subsubsection|Attribute widgets>

  These widgets allow to set window attributes of some common types such as
  integers, strings, commands, points, etc. They can be used for instance to
  retrieve an input string or in order to set the scroll position in a canvas
  widget. The corresponding events are created by functions like
  <cpp|get_string>, <cpp|set_string>, <cpp|get_integer>, <cpp|set_coord2>,
  <abbr|etc.>

  <subsubsection|Glue widgets>

  Glue widgets are created by

  <\cpp-code>
    wk_widget glue_wk_widget (bool hx=true, bool vx=true, SI w=0, SI h=0);
  </cpp-code>

  The first two arguments determine whether the widget is extensible
  horizontally <abbr|resp.> vertically. The last two elements determine the
  default and minimal size of the widget.

  <subsubsection|Text widgets>

  Text widgets are created using

  <\cpp-code>
    wk_widget text_wk_widget (string s, int style= 0, bool tsp= false);
  </cpp-code>

  They just display the text <cpp|s>.

  <subsubsection|Buttons>

  Two types of buttons have been implemented. First, command buttons are
  created using

  <\cpp-code>
    wk_widget command_button (wk_widget w, command cmd, int style= 0);
  </cpp-code>

  They display the widget <cpp|w> (for instance a text widget) and execute
  the command <cpp|cmd> when pressed. Secondly, we implemented popup buttons,
  which popup some window when pressed. Popup buttons are created by one of

  <\cpp-code>
    wk_widget pulldown_button (wk_widget w, wk_widget m, int style= 0);

    wk_widget pullright_button (wk_widget w, wk_widget m, int style= 0);
  </cpp-code>

  depending on where the popup window should popup. The main widget attached
  to the popup window should be created using

  <\cpp-code>
    wk_widget popup_widget (wk_widget w, gravity quit= center);
  </cpp-code>

  The <cpp|quit> argument specifies that the popup window should disappear
  as soon as the pointer leaves the widget in the <cpp|quit> direction.

  <subsubsection|Menus>

  Horizontal and vertical lists of widgets and vertical menus are created
  using

  <\cpp-code>
    wk_widget horizontal_list (array\<less\>wk_widget\<gtr\> a);

    wk_widget vertical_list (array\<less\>wk_widget\<gtr\> a);

    wk_widget vertical_menu (array\<less\>wk_widget\<gtr\> a);
  </cpp-code>

  Subsequently, they can be modified as composite widgets. In <TeXmacs>,
  menus are not constructed directly in <c++>, but described in <scheme>;
  see the chapter on the <hlink|abstract widget system|widgets.en.tm>.

  <subsubsection|Canvas widgets>

  Canvas widgets are created using

  <\cpp-code>
    wk_widget canvas_widget (wk_widget w, gravity grav= north_west, bool rf= false);
  </cpp-code>

  Canvas widgets consist of a portion of the widget <cpp|w> and scrollbars,
  which enable to scroll <cpp|w>. The events

  <\cpp-code>
    event set_scrollable (wk_widget w);

    event set_extents \ \ \ (SI x1, SI y1, SI x2, SI y2);

    event set_scroll_pos (SI x, SI y);

    event get_extents \ \ \ (SI& x1, SI& y1, SI& x2, SI& y2);

    event get_visible \ \ \ (SI& x1, SI& y1, SI& x2, SI& y2);
  </cpp-code>

  enable to change <cpp|w>, to set the extents of <cpp|w>, to set the scroll
  position, to get the extents of <cpp|w> and to get the rectangle of
  <cpp|w>, which is currently visible.

  <subsubsection|Input widgets>

  Input widgets enable to type a string and to retrieve it when finished.
  They are created using

  <\cpp-code>
    wk_widget input_text_wk_widget (command call_back, int style= 0, string w= "1w", bool pf= true);
  </cpp-code>

  Some initial text can be put in it using

  <\cpp-code>
    event set_input_string (string s);
  </cpp-code>

  The command <cpp|call_back> is executed when typing has been finished or
  aborted (by typing return, escape or C-c). The typed string can then be
  retrieved using

  <\cpp-code>
    event get_input_string (string& s);
  </cpp-code>

  Usually, the returned <cpp|s> is a string enclosed between quotes. If
  typing was aborted, <cpp|s> contains the string <verbatim|"#f">.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
