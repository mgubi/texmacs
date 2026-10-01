<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The server, buffers, views and windows>

  <section|Introduction>

  This document describes the part of <TeXmacs> which sits between the
  graphical toolkit and the typesetter: the <em|server>, which owns all open
  documents, and the <em|editors>, which implement the interactive behaviour
  of the program. It explains how documents are organized into buffers,
  views and windows, how these objects are created and destroyed, how user
  events reach the editor, how modifications of the document are propagated
  (to the typesetter, to other views on the same document and to the undo
  system), and how screen updates are scheduled.

  Two neighbouring subjects are only touched upon here. The typesetting of
  documents into boxes is the subject of <hlink|the typesetting
  algorithm|typesetter.en.tm> (see also <hlink|the boxes produced by the
  typesetter|boxes.en.tm>), and the widgets which make up menus, toolbars,
  side panes and the footer are described in <hlink|the abstract widget
  system|widgets.en.tm>. The <scheme> interface to buffers, views and windows
  is documented in <hlink|the Scheme buffer API|../scheme/buffer/scheme-buffer.en.tm>.

  The relevant sources are mainly located in the following directories
  (relative to <verbatim|src/src/>):

  <\description>
    <item*|<verbatim|Texmacs/>>The server proper: <verbatim|server.hpp>
    (abstract interface), <verbatim|tm_server.hpp>, <verbatim|tm_config.hpp>,
    <verbatim|tm_frame.hpp>, <verbatim|tm_buffer.hpp>,
    <verbatim|tm_window.hpp>, <verbatim|tm_data.hpp>; the implementations in
    <verbatim|Server/> (<verbatim|tm_server.cpp>, <verbatim|tm_config.cpp>),
    <verbatim|Window/> (<verbatim|tm_window.cpp>, <verbatim|tm_frame.cpp>,
    <verbatim|tm_dialogue.cpp>, <verbatim|tm_button.cpp>) and the buffer, view,
    window and project management in <verbatim|Data/>
    (<verbatim|new_buffer.cpp>, <verbatim|new_view.cpp>,
    <verbatim|new_window.cpp>, <verbatim|new_project.cpp>). The program entry
    point and the startup sequence live in
    <verbatim|Texmacs/Texmacs/texmacs.cpp>.

    <item*|<verbatim|Edit/>>The editor: the abstract class
    <cpp|editor_rep> in <verbatim|Edit/editor.hpp> and its implementation
    <cpp|edit_main_rep> in <verbatim|Edit/Editor/edit_main.hpp>, which is
    assembled from the classes in <verbatim|Edit/Interface> (events, cursor,
    repainting, footer), <verbatim|Edit/Modify> (modifications and undo),
    <verbatim|Edit/Replace> (selections, search, spell checking),
    <verbatim|Edit/Process> and <verbatim|Edit/Editor/edit_typeset.cpp>.

    <item*|<verbatim|Data/>>The global edit tree (<verbatim|Data/Document>),
    the observers which are attached to it (<verbatim|Data/Observers>) and
    the undo/redo history (<verbatim|Data/History>). The generic observer
    mechanism itself is implemented in
    <verbatim|Kernel/Abstractions/observer.cpp>.

    <item*|<verbatim|Scheme/Glue/>>The glue which exports the above routines
    to <scheme>: <verbatim|build-glue-basic.scm> (buffers, views, windows),
    <verbatim|build-glue-server.scm> (routines of the server) and
    <verbatim|build-glue-editor.scm> (routines of the current editor).
  </description>

  On the <scheme> side, the most relevant files (relative to
  <verbatim|src/TeXmacs/progs/>) are <verbatim|kernel/library/base.scm>,
  <verbatim|kernel/gui/kbd-handlers.scm>, <verbatim|kernel/gui/kbd-define.scm>,
  <verbatim|kernel/texmacs/tm-preferences.scm>,
  <verbatim|kernel/texmacs/tm-file-system.scm>,
  <verbatim|texmacs/texmacs/tm-files.scm> and
  <verbatim|texmacs/texmacs/tm-server.scm>.

  <section|Startup and the server singleton>

  <subsection|The startup sequence>

  The function <cpp|texmacs_entrypoint> in <verbatim|texmacs.cpp> performs
  the low level initializations: it handles the options which have to be
  treated before anything else (<cpp|immediate_options>), sets up the paths
  (<cpp|TeXmacs_init_paths>), loads the user preferences
  (<cpp|load_user_preferences>), creates the <name|Qt> application object and
  initializes the fonts. It then creates the global edit tree and attaches
  the root <em|ip observer> to it (see below), performs further system
  initializations such as the user directories, the boot lock and the
  standard <abbr|DRD> (<cpp|init_texmacs>, in
  <verbatim|System/Boot/init_texmacs.cpp>) and finally hands over control to
  the <scheme> interpreter:

  <\cpp-code>
    the_et \ \ \ \ = tuple ();

    the_et-\<gtr\>obs= ip_observer (path ());

    cache_initialize ();

    init_texmacs ();

    start_scheme (argc, argv, TeXmacs_main);
  </cpp-code>

  <cpp|TeXmacs_main> is the \Preal\Q main program. It processes the command
  line options (<cpp|set_global_options>), installs the plug-ins
  (<cpp|init_plugins>), opens the display (<cpp|gui_open>) and then creates
  the server as a local variable <cpp|server sv>. The constructor of the
  server loads the <scheme> side of <TeXmacs>. If no buffer has been opened
  by then, a first window with an empty buffer is created with
  <cpp|open_window>. Commands which were collected while processing the
  command line (for instance <scm|(load-buffer ...)> for the files given as
  arguments) are scheduled with <cpp|exec_delayed>, and the GUI event loop is
  entered with <cpp|gui_start_loop>. When this loop terminates, the server
  goes out of scope and the display is closed with <cpp|gui_close>.

  <subsection|The server classes>

  The abstract class <cpp|server_rep> in <verbatim|Texmacs/server.hpp> lists
  the services which the server offers to the rest of the program. It is
  implemented by <cpp|tm_server_rep>, which inherits from two partial
  implementations, both of which derive virtually from <cpp|server_rep>:

  <\itemize>
    <item><cpp|tm_config_rep> (<verbatim|tm_config.hpp>,
    <verbatim|Server/tm_config.cpp>): font rules and the keyboard
    configuration (wildcards, variant keys, lookup of key bindings and
    rendering of shortcuts); see section<nbsp><reference|sec-config>.

    <item><cpp|tm_frame_rep> (<verbatim|tm_frame.hpp>,
    <verbatim|Window/tm_frame.cpp>): everything which concerns the
    <em|current> window: its properties, menus, icon bars, side and bottom
    tools, footer, zoom factor, scroll position, full screen mode, dialogues
    and interactive prompts; see section<nbsp><reference|sec-frames>.
  </itemize>

  <cpp|tm_server_rep> itself (<verbatim|tm_server.hpp>,
  <verbatim|Server/tm_server.cpp>) adds miscellaneous global routines: the
  <em|interpose handler> which drives the periodic updates of all editors,
  printer settings, the default zoom factor, invalidation of the typesetting
  of all views (<cpp|typeset_update>, <cpp|typeset_update_all>,
  <cpp|style_clear_cache>), <cpp|refresh>, <cpp|shell> and <cpp|quit>.

  The server is a singleton. The handle class <cpp|server> is reference
  counted; its default constructor creates a new <cpp|tm_server_rep>, whose
  constructor registers itself in the global pointer <cpp|the_server>:

  <\cpp-code>
    tm_server_rep::tm_server_rep () ... {

    \ \ the_server= tm_new\<less\>server\<gtr\> (this);

    \ \ initialize_scheme ();

    \ \ gui_interpose (texmacs_interpose_handler);

    \ \ set_wait_handler (texmacs_wait_handler);

    \ \ ...

    \ \ if (exists (tm_init_file)) exec_file (tm_init_file);

    \ \ if (exists (my_init_file)) exec_file (my_init_file);

    \ \ ...

    }
  </cpp-code>

  Here <cpp|tm_init_file> defaults to
  <verbatim|$TEXMACS_PATH/progs/init-texmacs.scm> and <cpp|my_init_file> to
  <verbatim|$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm>. The rest of the
  code obtains the server through <cpp|get_server ()>, which asserts that
  <cpp|is_server_started ()>. The flag <cpp|texmacs_started> is set just
  before entering the event loop.

  Many routines of the server act on the <em|current> window or editor
  (section<nbsp><reference|sec-focus>). When an editor needs to call such a
  routine for its own window, it uses the <cpp|SERVER> macro from
  <verbatim|editor.hpp>, which temporarily makes the editor current:

  <\cpp-code>
    #define SERVER(cmd) { \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \\

    \ \ url temp= get_current_view_safe (); \\

    \ \ focus_on_this_editor (); \ \ \ \ \ \ \ \ \ \ \ \\

    \ \ sv-\<gtr\>cmd; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \\

    \ \ set_current_view (temp); \ \ \ \ \ \ \ \ \ \ \ \\

    }
  </cpp-code>

  <section|The object model: buffers, views and windows>

  <subsection|The global edit tree>

  All documents which are open in a <TeXmacs> session are subtrees of a single
  global tree <cpp|the_et> (<verbatim|Data/Document/new_document.cpp>). The
  root of <cpp|the_et> is a <markup|tuple>; each child is the body of one
  buffer. The function <cpp|new_document> returns the path of a free slot
  (reusing slots which were marked <cpp|UNINIT> by
  <cpp|delete_document>), and <cpp|set_document> assigns a copy of a tree to
  such a slot. The path of a buffer's body inside <cpp|the_et> is called its
  <em|root path> and is stored as <cpp|rp> both in the buffer and in each of
  its editors.

  Keeping all documents in one tree has two important consequences. First,
  any subtree of any open document can be designated by an absolute path, and
  the root <cpp|ip_observer> which is attached to <cpp|the_et> at startup
  allows to recover this path from the tree itself (<cpp|obtain_ip>). Second,
  all modifications are performed through the same functions (such as
  <cpp|assign (path, tree)>), which forward them to the observers attached
  to the modified subtrees; this is the basis for the modification pipeline
  of section<nbsp><reference|sec-modifications>.

  <subsection|Buffers>

  A <em|buffer> represents one open document. It is implemented by
  <cpp|tm_buffer_rep> in <verbatim|tm_buffer.hpp>:

  <\cpp-code>
    class tm_buffer_rep {

    public:

    \ \ new_buffer buf; \ \ \ \ \ \ \ \ // file related information

    \ \ new_data data; \ \ \ \ \ \ \ \ \ // data associated to document

    \ \ array\<less\>tm_view\<gtr\> vws; \ \ \ \ // views attached to buffer

    \ \ tm_buffer prj; \ \ \ \ \ \ \ \ \ // buffer which corresponds to the project

    \ \ path rp; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // path to the document's root in the_et

    \ \ link_repository lns; \ \ \ // global links

    \ \ bool notify; \ \ \ \ \ \ \ \ \ \ \ // notify modifications to scheme

    \ \ ...

    };
  </cpp-code>

  The file related information <cpp|new_buffer_rep> (in
  <verbatim|Texmacs/Data/new_buffer.hpp>) holds the <cpp|name> of the buffer
  (a <cpp|url>), its <cpp|master> <abbr|URL> (used for linking and
  navigation), the format <cpp|fm>, the <cpp|title> displayed in menus and
  window titles, the flags <cpp|read_only> and <cpp|secure>, the time of the
  last save <cpp|last_save> and of the last visit <cpp|last_visit>. The
  document data <cpp|new_data_rep> (<verbatim|Data/Document/new_data.hpp>)
  contains everything of a <TeXmacs> file except its body: <cpp|project>,
  <cpp|style>, and the hash tables <cpp|init>, <cpp|fin>, <cpp|ref>,
  <cpp|aux> and <cpp|att>. When a document is loaded,
  <cpp|detach_data> splits it into body and data; <cpp|attach_data>
  recombines them when the document is saved or retrieved with
  <cpp|get_buffer_tree>.

  All buffers are stored in the global array <cpp|bufs>
  (<verbatim|new_buffer.cpp>, declared in <verbatim|tm_data.hpp>). A buffer
  is <em|identified by its own <abbr|URL>>: there is no separate naming
  scheme for buffers, and <cpp|concrete_buffer (url)> simply searches
  <cpp|bufs> for a buffer with the given name. The name is typically

  <\itemize>
    <item>the file name of the document on disk or on the web;

    <item>a scratch <abbr|URL> produced by <cpp|make_new_buffer> for new
    documents (<cpp|url_scratch ("no_name_", ".tm", i)>); such buffers are
    recognized by <cpp|buffer_has_name>, which returns false for them, and
    their titles are rendered as \PNo name [<em|i>]\Q by
    <cpp|propose_title>;

    <item>a <verbatim|tmfs://> <abbr|URL> for documents that are generated
    by <scheme> handlers of the <TeXmacs> file system (help pages,
    auxiliary documents <verbatim|tmfs://aux/...>, embedded input fields
    <verbatim|tmfs://aux/TeXmacs-input-<em|n>>, and so on). See
    <hlink|the <TeXmacs> file system|../scheme/api/tmfs/tmfs.en.tm>.
  </itemize>

  A buffer is called <em|auxiliary> if its master differs from its name
  (<cpp|is_aux_buffer>). Auxiliary buffers, for instance generated
  bibliographies or help pages opened from a document, cannot be saved under
  their own name, but they resolve links and relative file names with
  respect to their master. The <scheme> routine <scm|open-auxiliary> in
  <verbatim|tm-files.scm> creates such a buffer from a tree and a master
  (via <scm|aux-set-document> and <scm|aux-set-master> in
  <verbatim|tm-file-system.scm>).

  The field <cpp|prj> points to the buffer of the project the document
  belongs to (if any). <verbatim|new_project.cpp> implements
  <cpp|project_attach>, <cpp|project_attached> and <cpp|project_get>; the
  project name is stored in <cpp|data-\<gtr\>project> and resolved relative
  to the directory of the buffer. A buffer is an <em|implicit project> if its
  suffix is <verbatim|tp> or if its <verbatim|project-flag> initial
  variable is <verbatim|true>.

  <subsection|Views>

  A <em|view> is an editor on a buffer. There may be several views on the
  same buffer; each has its own cursor, selection, typeset box tree, zoom
  factor and undo history, but they all share the document tree. A view is
  implemented by <cpp|tm_view_rep> in <verbatim|tm_window.hpp>:

  <\cpp-code>
    class tm_view_rep {

    public:

    \ \ tm_buffer buf; \ // the buffer being viewed

    \ \ editor \ \ \ ed; \ \ // the editor instance

    \ \ tm_window win; \ // the window displaying the view, or NULL

    \ \ int \ \ \ \ \ \ nr; \ \ // number of the view for this buffer

    \ \ tm_view_rep (tm_buffer buf2, editor ed2);

    };
  </cpp-code>

  (the comments are ours). Views are identified by <abbr|URL>s of the form

  <\verbatim-code>
    tmfs://view/<em|nr>/<em|encoded-buffer-name>
  </verbatim-code>

  which are computed by <cpp|abstract_view> and decoded by
  <cpp|concrete_view> in <verbatim|new_view.cpp>. The number <em|nr> is
  allocated per buffer name by <cpp|new_view_number>; the buffer name is
  encoded by the static function <cpp|encode_url> (for instance
  <verbatim|default/home/joris/a.tm> for a local file). Since
  <cpp|concrete_view> looks up the buffer first, a view <abbr|URL> becomes
  invalid as soon as its buffer is removed or renamed.

  A view whose <cpp|win> is <cpp|NULL> is called <em|passive>; a view
  attached to a window is <em|active>. Passive views are useful for
  operations on buffers which are not displayed, and at least one view must
  exist on every buffer for <cpp|buffer_modified> to work (see
  section<nbsp><reference|sec-undo>). New views are created by
  <cpp|get_new_view (url name)>, which

  <\enumerate>
    <item>creates the buffer if necessary (<cpp|create_buffer>);

    <item>creates a new editor with <cpp|new_editor (get_server ()
    -\<gtr\> get_server (), buf)>;

    <item>appends the new <cpp|tm_view_rep> to <cpp|buf-\<gtr\>vws> and passes
    the document data to the editor with <cpp|set_data>;

    <item>temporarily makes the new view current and executes
    <verbatim|$TEXMACS_PATH/progs/init-buffer.scm> and
    <verbatim|$TEXMACS_HOME_PATH/progs/my-init-buffer.scm> (the default
    <verbatim|init-buffer.scm> sets the default style of unnamed buffers).
  </enumerate>

  Other ways to obtain a view on a buffer are <cpp|get_passive_view>, which
  returns an existing view that is not attached to a window (creating one if
  needed, and loading the buffer if it does not exist yet), and
  <cpp|get_recent_view (url name)>, which prefers the current view, then the
  most recently used active view on the buffer. The <em|view history>
  <cpp|view_history> is a list of view <abbr|URL>s, most recent first; it is
  updated by <cpp|notify_set_view> whenever a view is attached to a window
  and is returned by <cpp|get_all_views>. Views are destroyed with
  <cpp|delete_view>, which removes the view from its buffer and from the
  history, sets <cpp|ed-\<gtr\>buf> to <cpp|NULL> and deletes the
  <cpp|tm_view_rep> (the editor itself is reference counted and dies with its
  last reference).

  <subsection|Windows>

  A <em|window> is a top level <TeXmacs> window with menus, toolbars, a canvas
  and a footer. It is implemented by <cpp|tm_window_rep> in
  <verbatim|tm_window.hpp>. Its main fields are the outer window widget
  <cpp|win>, the <TeXmacs> widget <cpp|wid> which contains the menus, icon
  bars, canvas and footer, the identifier <cpp|id>, a table of <cpp|props>,
  a unique <cpp|serial> number, the zoom factor <cpp|zoomf>, and the caches
  <cpp|menu_current> and <cpp|menu_cache> for the menu widgets.

  Window identifiers are <abbr|URL>s <verbatim|tmfs://window/<em|n>> which
  are allocated by <cpp|create_window_id> and released by
  <cpp|destroy_window_id> (<verbatim|new_window.cpp>). The static table
  <cpp|tm_window_table> maps them to <cpp|tm_window_rep> pointers
  (<cpp|concrete_window (url)>); <cpp|abstract_window> performs the
  converse; <cpp|windows_list> returns all window identifiers.

  A new window is created by the (file local) function <cpp|new_window
  (bool map_flag, tree geom)>. It builds the <TeXmacs> widget with
  <cpp|texmacs_widget (mask, quit)>, where the bits of <cpp|mask> are
  computed from the preferences <verbatim|header>, <verbatim|main icon bar>,
  <verbatim|mode dependent icons>, <verbatim|focus dependent icons>,
  <verbatim|user provided icons>, <verbatim|status bar>, <verbatim|bottom
  tools> and <verbatim|extra tools>, and <cpp|quit> is a
  <cpp|kill_window_command_rep> which schedules the <scheme> command
  <scm|(safely-kill-window <scm-arg|id>)> when the user closes the window.
  Windows are destroyed by the file local <cpp|delete_window>, which detaches
  (but does not delete) the view of the window, unmaps and destroys the
  window widget and deletes the <cpp|tm_window_rep>.

  At present, a window displays exactly one view at a time, and a view is
  displayed in at most one window. Different windows may display different
  views on the same buffer (<cpp|clone_window>).

  <subsection|Navigating between the objects>

  The following functions (declared in <verbatim|Data/new_buffer.hpp>,
  <verbatim|Data/new_view.hpp> and <verbatim|Data/new_window.hpp>) navigate
  between the three kinds of objects. They take and return <abbr|URL>s;
  <cpp|url_none ()> is returned when there is no answer.

  <\description>
    <item*|From buffers><cpp|buffer_to_views>, <cpp|buffer_to_windows>,
    <cpp|get_recent_view>, <cpp|get_passive_view>.

    <item*|From views><cpp|view_to_buffer>, <cpp|view_to_window>,
    <cpp|view_to_editor>.

    <item*|From windows><cpp|window_to_view>, <cpp|window_to_buffer>.

    <item*|From paths><cpp|path_to_buffer (path p)> returns the buffer whose
    root path is a prefix of <cpp|p>.

    <item*|Enumerations><cpp|get_all_buffers>, <cpp|get_all_views>,
    <cpp|windows_list>, <cpp|number_buffers>, <cpp|get_nr_windows>.

    <item*|Low level><cpp|concrete_buffer>, <cpp|concrete_buffer_insist>
    (which loads the buffer if needed), <cpp|concrete_view>,
    <cpp|abstract_view>, <cpp|concrete_window>, <cpp|abstract_window>.
  </description>

  Code outside <verbatim|Texmacs/Data> should use the <abbr|URL> based
  functions. The raw pointers <cpp|tm_buffer>, <cpp|tm_view> and
  <cpp|tm_window> are not reference counted, so they must never be stored
  across operations which might close a buffer or a window.

  <section|Life cycle of buffers, views and windows>

  <subsection|Creating and loading buffers>

  Buffers are created on demand by <cpp|set_buffer_tree (url name, tree
  doc)>. If no buffer with the given name exists, the file local
  <cpp|insert_buffer> allocates a new <cpp|tm_buffer_rep> (whose constructor
  obtains a slot in <cpp|the_et> through <cpp|new_document>), the document is
  split with <cpp|detach_data>, its body is stored with <cpp|set_document>
  and a title is proposed. If the buffer already exists, its body is
  replaced with <cpp|assign>, which goes through the normal modification
  pipeline, and the new data are passed to all editors on the buffer with
  <cpp|set_data> and <cpp|init_update>. In both cases the project buffer is
  loaded if the document declares a project, and the buffer is marked as
  saved (<cpp|pretend_buffer_saved>). The convenience function
  <cpp|create_buffer (url, tree)> only creates a buffer if it does not exist
  yet; <cpp|set_buffer_body> and <cpp|get_buffer_body> operate on the body
  only.

  Loading a file is done by <cpp|buffer_load (url name)>, which determines
  the format with <cpp|file_format> and calls <cpp|buffer_import (name,
  name, fm)>. The latter reads and converts the file with
  <cpp|import_tree> (and <cpp|import_loaded_tree>, which also registers the
  link locations of the document and handles source code formats) and then
  calls <cpp|set_buffer_tree>. Note the convention of these low level
  routines: they return <cpp|true> on <em|failure>.

  The user level loading logic is written in <scheme>, in
  <verbatim|texmacs/texmacs/tm-files.scm>. <scm|load-buffer> calls
  <scm|load-buffer-main>, which resolves relative names and then goes
  through a chain of checks: <scm|load-buffer-check-autosave> (proposes to
  load a more recent autosave file), <scm|load-buffer-check-permissions>,
  <scm|load-buffer-load> (which calls <scm|buffer-load> if the buffer does
  not exist yet, or creates an empty document if the file does not exist)
  and <scm|load-buffer-open>. The last one displays the buffer according to
  the options: nothing for <scm|:background>, a new window for
  <scm|:new-window> (through <scm|open-buffer-in-window>) and otherwise
  <scm|switch-to-buffer> in the current window.

  <subsection|Displaying buffers in windows>

  A view is shown in a window with <cpp|attach_view (url win, url view)>. It
  sets <cpp|vw-\<gtr\>win>, installs the editor as the scrollable canvas of
  the window widget (<cpp|set_scrollable (wid, vw-\<gtr\>ed)>), stores the
  canvas widget in <cpp|ed-\<gtr\>cvw>, calls <cpp|ed-\<gtr\>resume ()>
  (which rebuilds the menus and toolbars for this editor), updates the title
  and <abbr|URL> of the window and records the view in the view history.
  <cpp|detach_view> does the converse: it calls <cpp|ed-\<gtr\>suspend ()>
  and replaces the canvas by an empty <cpp|glue_widget>.

  On top of these, <cpp|window_set_view (win, view, focus)> detaches the
  previous view of a window, attaches the new one, and makes it current if
  <cpp|focus> is set or if the old view was current. The most commonly used
  higher level routines are

  <\description>
    <item*|<cpp|switch_to_buffer (url name)>>Show a passive view on
    <cpp|name> in the current window and give it the focus.

    <item*|<cpp|window_set_buffer (url win, url name)>>Show a passive view on
    <cpp|name> in <cpp|win> without changing the focus.

    <item*|<cpp|new_buffer_in_new_window (name, doc, geom)>>Create the
    buffer if needed, open a new window and show a passive view on the buffer
    in it.

    <item*|<cpp|create_buffer ()>>Create a new scratch buffer in the current
    window.

    <item*|<cpp|open_window (tree geom)>>Create a new scratch buffer in a new
    window.

    <item*|<cpp|clone_window ()>>Open a new window with a passive view on
    the current buffer (a new view is created if all existing views are
    displayed in windows).
  </description>

  <subsection|Saving buffers>

  <cpp|buffer_save (url name)> exports the buffer to its own name in the
  format given by its suffix, marks it as saved with
  <cpp|pretend_buffer_saved> and clears the \Pmodified\Q indicator of all
  windows showing it (<cpp|tm_window_rep::set_modified>).
  <cpp|buffer_export (name, dest, fm)> does the real work: it takes the
  most recent view on the buffer, retrieves the body from <cpp|the_et>,
  applies the editor based conversions for the formats which need the
  typesetter (<cpp|exec_verbatim>, <cpp|exec_html>, or
  <cpp|print_to_file> for PostScript and PDF), attaches the data
  obtained from the editor with <cpp|get_data> (without auxiliary data if
  <cpp|get_save_aux> is false), adds the link locations and writes the
  result with <cpp|export_tree>. Like the loading routines, these functions
  return <cpp|true> on failure.

  As for loading, the user level logic is in <verbatim|tm-files.scm>:
  <scm|save-buffer> calls <scm|save-buffer-main>, then
  <scm|save-buffer-check-permissions> (which asks for a file name for scratch
  buffers, refuses to save non existing or unmodified buffers and warns when
  the file changed on disk since <scm|buffer-last-save>),
  <scm|save-buffer-check-faithful> (which asks for confirmation when the
  target format is not a faithful <TeXmacs> format) and finally
  <scm|save-buffer-save>. <scm|save-buffer-as> renames the buffer with
  <scm|buffer-rename> before saving it. Autosaving is also implemented in
  this file (<scm|autosave-buffer>, <scm|autosave-all>).

  Renaming is implemented by <cpp|rename_buffer (name, new_name)>. It kills
  any buffer which already has the new name, changes <cpp|name> and
  <cpp|master>, notifies the editors (<cpp|notify_change
  (THE_ENVIRONMENT)>), updates the view history (the view <abbr|URL>s depend
  on the buffer name, see <cpp|notify_rename_before> and
  <cpp|notify_rename_after>) and proposes a new title.

  <subsection|Closing buffers and windows>

  <cpp|kill_buffer (url name)> first replaces the buffer in every window
  which displays it by the most recent view on another buffer (creating a new
  view if necessary) and then calls <cpp|remove_buffer>. The latter deletes
  all views on the buffer, removes it from <cpp|bufs> and deletes the
  <cpp|tm_buffer_rep>, whose destructor frees the slot in <cpp|the_et> with
  <cpp|delete_document>. If the last buffer is removed and <TeXmacs> does not
  act as a server for remote clients (<cpp|number_of_servers ()>), the
  program quits.

  <cpp|kill_window (url win)> gives the focus to a view in another window
  (if there is one) and deletes the window; if it was the last window, the
  program quits (again, unless remote clients are connected).
  <cpp|kill_current_window_and_buffer> also removes the buffer if no other
  window shows it.

  Again, the user level commands are written in <scheme>
  (<verbatim|texmacs/texmacs/tm-server.scm>): <scm|safely-kill-buffer>,
  <scm|safely-kill-window> and <scm|safely-quit-TeXmacs> ask for
  confirmation if unsaved buffers would be lost, then call
  <scm|buffer-close> (which calls the glue routine <scm|cpp-buffer-close>,
  that is, <cpp|kill_buffer>), <scm|kill-window> and <scm|quit-TeXmacs>.
  The latter calls <cpp|tm_server_rep::quit>, which closes all pipes to
  plug-ins, calls the <scheme> hook <scm|quit-TeXmacs-scheme> and exits the
  process. The commands <scm|new-document> and <scm|close-document> choose
  between the buffer and window versions according to the <verbatim|buffer
  management> preference (tested by the mode predicate
  <scm|window-per-buffer?>).

  <section|The current view and the focus><label|sec-focus>

  At any time, at most one view is <em|current>. It is stored in the file
  local variable <cpp|the_view> of <verbatim|new_view.cpp> and manipulated by

  <\description>
    <item*|<cpp|set_current_view (url u)>>Makes <cpp|u> current. As a side
    effect, the global <cpp|the_drd> is set to the <abbr|DRD> of the editor
    and the <cpp|last_visit> time of the buffer is updated.

    <item*|<cpp|get_current_view ()>>Returns the current view; asserts that
    there is one.

    <item*|<cpp|get_current_view_safe ()>>Returns <cpp|url_none ()> if there
    is no current view.

    <item*|<cpp|get_current_editor ()>>Returns the editor of the current
    view.
  </description>

  The current buffer and window are derived from the current view:
  <cpp|get_current_buffer> returns the buffer of the current view, and
  <cpp|has_current_window>, <cpp|get_current_window> and
  <cpp|concrete_window ()> refer to the window of the current view, if it is
  active. There is no separately stored \Pcurrent window\Q.

  The current view is the implicit argument of a large part of the program:
  all routines of <verbatim|build-glue-editor.scm> are exported as
  <cpp|get_current_editor()-\<gtr\>...>, all routines of
  <cpp|tm_frame_rep> act on <cpp|concrete_window ()>, and typesetting
  routines use <cpp|the_drd>. The current view is changed

  <\itemize>
    <item>when a window obtains the keyboard focus:
    <cpp|edit_interface_rep::handle_keyboard_focus> calls
    <cpp|focus_on_this_editor ()>, which calls <cpp|focus_on_editor>, which
    searches the view of the editor and makes it current;

    <item>when a view is shown in a window with focus
    (<cpp|window_set_view>, <cpp|switch_to_buffer>);

    <item>explicitly, by <cpp|window_focus (url win)>,
    <cpp|focus_on_buffer (url name)> (which chooses the most recent view on
    the buffer, preferably an active one) or <cpp|var_focus_on_buffer>
    (which in addition suspends the old editor, resumes the new one and moves
    the keyboard focus);

    <item>temporarily, by the <cpp|SERVER> macro and by the <scheme> macros
    <scm|with-buffer> and <scm|with-window> (defined in
    <verbatim|utils/library/cursor.scm>), which use <scm|buffer-focus> to
    execute some code in the context of another buffer.
  </itemize>

  <cpp|switch_to_window (url win)> is different: it suspends the editor of
  the current window, maps the new window and resumes its editor, and sends
  the GUI keyboard focus to it; the current view is then updated by
  <cpp|handle_keyboard_focus> when the GUI reports the focus change.

  <\warning>
    Code which temporarily changes the current view must restore it, also
    in case of errors, since the next event would otherwise be processed in
    the wrong context. Also note that <cpp|get_current_view> fails if there
    is no current view (for instance very early during startup); use the
    <cpp|_safe> variants in code which may run in such situations.
  </warning>

  <section|The editor>

  <subsection|Structure of the editor classes>

  An editor is an instance of <cpp|edit_main_rep>, created by
  <cpp|new_editor (server_rep* sv, tm_buffer buf)> in
  <verbatim|Edit/Editor/edit_main.cpp>. Its abstract base class
  <cpp|editor_rep> (<verbatim|Edit/editor.hpp>) derives from
  <cpp|simple_widget_rep>, the widget class for canvases of the GUI back-end
  (<name|Qt>, Cocoa or Widkit), so that an editor <em|is> the widget which
  the GUI displays inside the scrollable canvas of a window. The handle
  class <cpp|editor> extends <cpp|widget>.

  <cpp|editor_rep> declares the complete public interface of the editor as
  pure virtual functions, grouped by the subclass which implements them, and
  holds the members shared by all parts:

  <\cpp-code>
    class editor_rep: public simple_widget_rep {

    public:

    \ \ server_rep* \ sv; \ \ // the underlying texmacs server

    \ \ widget_rep* \ cvw; \ // non reference counted canvas widget

    \ \ tm_view_rep* mvw; \ // master view

    protected:

    \ \ tm_buffer \ \ \ buf; \ // the underlying buffer

    \ \ drd_info \ \ \ \ drd; \ // the drd for the buffer

    \ \ tree& \ \ \ \ \ \ \ et; \ \ // all TeXmacs trees

    \ \ box \ \ \ \ \ \ \ \ \ eb; \ \ // box translation of tree

    \ \ path \ \ \ \ \ \ \ \ rp; \ \ // path to the root of the document in et

    \ \ path \ \ \ \ \ \ \ \ tp; \ \ // path of cursor in tree

    \ \ ...

    };
  </cpp-code>

  <cpp|edit_main_rep> combines the implementation classes by multiple
  inheritance; all of them derive virtually from <cpp|editor_rep>:

  <\description>
    <item*|<cpp|edit_interface_rep>>(<verbatim|Edit/Interface/edit_interface.cpp>,
    <verbatim|edit_keyboard.cpp>, <verbatim|edit_mouse.cpp>,
    <verbatim|edit_repaint.cpp>, <verbatim|edit_footer.cpp>,
    <verbatim|edit_complete.cpp>) Event handlers, change notification and
    <cpp|apply_changes>, repainting, the footer, keyboard shortcuts, input
    modes and completion.

    <item*|<cpp|edit_cursor_rep>>(<verbatim|Edit/Interface/edit_cursor.cpp>)
    The cursor and cursor movements.

    <item*|<cpp|edit_graphics_rep>>(<verbatim|Edit/Interface/edit_graphics.cpp>)
    Interaction with graphics.

    <item*|<cpp|edit_typeset_rep>>(<verbatim|Edit/Editor/edit_typeset.cpp>)
    The link with the typesetter: document data, environment queries and
    invalidation; see <hlink|the typesetting algorithm|typesetter.en.tm>.

    <item*|<cpp|edit_modify_rep>>(<verbatim|Edit/Modify/edit_modify.cpp>)
    Reception of modifications, undo and redo.

    <item*|<cpp|edit_text_rep>, <cpp|edit_math_rep>, <cpp|edit_table_rep>,
    <cpp|edit_dynamic_rep>>(<verbatim|Edit/Modify/>) Structured editing
    operations on text, mathematics, tables and markup.

    <item*|<cpp|edit_process_rep>>(<verbatim|Edit/Process/>) Generation of
    bibliographies, tables of contents, indexes and glossaries.

    <item*|<cpp|edit_select_rep>>(<verbatim|Edit/Replace/edit_select.cpp>)
    Selections and the clipboard.

    <item*|<cpp|edit_replace_rep>>(<verbatim|Edit/Replace/edit_search.cpp>,
    <verbatim|edit_spell.cpp>) Searching upwards in the tree, interactive
    search and replace, spell checking.
  </description>

  <cpp|edit_main_rep> itself adds a table of editor properties
  (<cpp|set_property>, <cpp|get_property>), printing, some queries (such as
  <cpp|the_buffer>, <cpp|the_path>, <cpp|the_buffer_path>) and debugging
  routines (<cpp|show_tree>, <cpp|show_box>, ...). Its constructor attaches
  an <em|edit observer> to the root of the buffer and puts the cursor at the
  start of the document:

  <\cpp-code>
    edit_main_rep::edit_main_rep (server_rep* sv, tm_buffer buf):

    \ \ editor_rep (sv, buf), props (UNKNOWN), ed_obs (edit_observer (this))

    {

    \ \ attach_observer (subtree (et, rp), ed_obs);

    \ \ notify_change (THE_TREE);

    \ \ tp= correct_cursor (et, rp * 0);

    }
  </cpp-code>

  Similarly, the constructor of <cpp|edit_modify_rep> allocates a new author
  identifier (<cpp|new_author>) and an <cpp|archiver> for the buffer root,
  which in turn attaches an <em|undo observer> to it.

  To add a new editing command which is callable from <scheme>, one
  typically declares it as a pure virtual function in <cpp|editor_rep>,
  implements it in the appropriate <verbatim|edit_*_rep> class, and adds
  an entry to <verbatim|Scheme/Glue/build-glue-editor.scm> (after which the
  glue has to be regenerated). Since the glue calls it on
  <cpp|get_current_editor ()>, the command always acts on the current view.

  <subsection|Editor state>

  The state of an editor, other than the typesetting state, consists of the
  following items.

  <\description>
    <item*|Cursor>The cursor is the path <cpp|tp> in <cpp|et>; it always
    starts with the root path <cpp|rp> of the buffer. It is returned by
    <cpp|the_path> (<scm|cursor-path> in <scheme>); <cpp|the_buffer_path>
    (<scm|buffer-path>) returns <cpp|rp>. The graphical cursor <cpp|cu> and
    the \Pghost cursor\Q <cpp|mv> used during vertical movements are
    computed from the box tree in <cpp|edit_cursor_rep>. Cursor movements go
    through <cpp|go_to (path)> and related routines, which update
    <cpp|tp>, call <cpp|notify_change (THE_CURSOR)> and the <scheme> hook
    <scm|notify-cursor-moved>. The routine <cpp|make_cursor_accessible>
    moves the cursor to an accessible position in the document.

    <item*|Selection>The current selection is a <cpp|range_set>
    <cpp|cur_sel> in <cpp|edit_select_rep>, set by <cpp|select (start,
    end)> and friends and queried by <cpp|selection_get_start>,
    <cpp|selection_get_end> and <cpp|selection_active_any>. Additional named
    selections (<cpp|alt_sels>) are used to highlight search results
    (<verbatim|alternate>), matching brackets (<verbatim|brackets>) and spell
    errors (<verbatim|spell_errors>); see <cpp|set_alt_selection>. Copy and
    paste go through <cpp|selection_copy>, <cpp|selection_paste> and
    <cpp|selection_set (key, tree, persistant)>, which encode the selection
    as <cpp|tuple ("texmacs", t, mode, lan)> and hand it to the GUI clipboard
    identified by <cpp|key> (<verbatim|primary>, <verbatim|mouse>, ...),
    possibly after conversion to the export format.

    <item*|Focus>The <em|focus> is the innermost tree which is considered
    to be edited (used for focus dependent menus and toolbars). It is
    usually derived from the cursor or selection (<cpp|focus_get>), but can
    be set manually with <cpp|manual_focus_set>.

    <item*|Environment at the cursor>The values of environment variables at
    the cursor, in particular the current <verbatim|mode> and language, are
    obtained from the typesetter with <cpp|get_env_value>,
    <cpp|get_env_string> and related functions (<scm|get-env> and
    <scm|get-env-tree> in <scheme>); the initial values of the document are
    obtained with <cpp|get_init_value>. They are recomputed lazily and are
    only valid when the document has been typeset, which is why editing
    commands occasionally force <cpp|apply_changes>.

    <item*|Input mode><cpp|input_mode> in <cpp|edit_interface_rep> is one of
    <cpp|INPUT_NORMAL>, <cpp|INPUT_SEARCH>, <cpp|INPUT_REPLACE>,
    <cpp|INPUT_SPELL> and <cpp|INPUT_COMPLETE> and is changed by
    <cpp|set_input_mode>. In the non normal modes, the <scheme> keyboard
    handlers forward the keys to routines like <scm|key-press-search>
    (<cpp|search_keypress>) instead of inserting them.

    <item*|Pending shortcut>The keys typed so far of a multi-key shortcut
    are kept in <cpp|sh_s>, together with an undo marker <cpp|sh_mark>, so
    that the effect of a prefix can be undone when the next key completes a
    longer shortcut (section<nbsp><reference|sec-keyboard>). Input method
    pre-edit text is handled similarly with <cpp|pre_edit_s> and
    <cpp|pre_edit_mark>.

    <item*|Change flags>The bit set <cpp|env_change> records what has to be
    recomputed at the next <cpp|apply_changes>
    (section<nbsp><reference|sec-repaint>); <cpp|last_change>,
    <cpp|last_update> and <cpp|last_event> are time stamps used to decide
    when menus and the footer are updated.

    <item*|Messages>The left and right footer messages <cpp|message_l> and
    <cpp|message_r> (see <cpp|set_message>).

    <item*|Undo history>The <cpp|archiver> <cpp|arch> and the author
    identifier <cpp|author> of <cpp|edit_modify_rep>
    (section<nbsp><reference|sec-undo>).

    <item*|Display>The zoom factor <cpp|zoomf>, the magnification
    <cpp|magf>, the visible rectangle <cpp|vx1>, <cpp|vy1>, <cpp|vx2>,
    <cpp|vy2>, and <cpp|got_focus>, which tells whether the editor has the
    keyboard focus. <cpp|suspend> and <cpp|resume> are called when the view
    is detached from, respectively attached to, a window or loses,
    respectively gains, the window focus.

    <item*|Properties>Arbitrary <scheme> trees can be associated to the
    editor with <cpp|set_property> and <cpp|get_property>.
  </description>

  Positions which have to survive modifications of the document are
  represented by <em|position observers>: <cpp|position_new (path)> attaches
  a <cpp|tree_position> observer to the tree at the given path, which is
  updated by all subsequent modifications; <cpp|position_get> returns the
  corrected path and <cpp|position_delete> detaches the observer. The editor
  uses this mechanism itself to preserve the cursor across modifications,
  and it is exported to <scheme> as <scm|position-new-path>,
  <scm|position-get> and so on.

  <section|The modification pipeline><label|sec-modifications>

  <subsection|Modifications and observers>

  The document is only modified through a small set of elementary
  <em|modifications> (<verbatim|Kernel/Types/modification.hpp>):
  <cpp|MOD_ASSIGN>, <cpp|MOD_INSERT>, <cpp|MOD_REMOVE>, <cpp|MOD_SPLIT>,
  <cpp|MOD_JOIN>, <cpp|MOD_ASSIGN_NODE>, <cpp|MOD_INSERT_NODE>,
  <cpp|MOD_REMOVE_NODE> and <cpp|MOD_SET_CURSOR>. A modification consists of
  its kind <cpp|k>, a path <cpp|p> and possibly a tree <cpp|t>. The function
  <cpp|apply (tree& ref, modification mod)> in
  <verbatim|Kernel/Abstractions/observer.cpp> is the single entry point;
  wrappers like <cpp|assign (path p, tree t)>, <cpp|insert>, <cpp|remove>,
  <cpp|split>, <cpp|join>, <cpp|assign_node>, <cpp|insert_node>,
  <cpp|remove_node> and <cpp|set_cursor> build the modification and call
  it. If the modified tree is attached to <cpp|the_et>, <cpp|apply>
  translates the modification into an absolute one and applies it to
  <cpp|the_et> with <cpp|raw_apply>. Modifications which are triggered
  <em|while> another modification is being performed (for instance by an
  observer which mirrors changes to linked trees) are queued in a list of
  \Pupcoming\Q modifications and executed afterwards, unless they concern a
  path which is already being modified.

  The <cpp|raw_*> functions perform the actual change on the tree and
  notify the observers attached to the modified node, in three steps:

  <\enumerate>
    <item><cpp|obs-\<gtr\>announce (ref, mod)> before the change; the
    default implementation dispatches to <cpp|announce_assign>,
    <cpp|announce_insert>, and so on;

    <item>specific notifications like <cpp|notify_assign>,
    <cpp|notify_insert> or <cpp|notify_remove_node>, which allow observers
    to reattach themselves or update positions;

    <item><cpp|obs-\<gtr\>done (ref, mod)> after the change.
  </enumerate>

  Several observers can be attached to the same tree (they are combined with
  <cpp|list_observer>). The following observers matter for this document:

  <\description>
    <item*|<cpp|ip_observer>>(<verbatim|Data/Observers/ip_observer.cpp>)
    Every node of <cpp|the_et> carries an ip observer which knows its
    inverse path. Announcements are propagated upwards to the observers of
    the ancestors, with the path of the modification extended accordingly.
    Hence an observer attached to the root of a buffer is notified of all
    changes inside the buffer.

    <item*|<cpp|edit_observer>>(<verbatim|Data/Observers/edit_observer.cpp>)
    Attached to the buffer root by each editor. It forwards announcements to
    <cpp|edit_announce>, completions to <cpp|edit_done> and
    <cpp|touch>-notifications to <cpp|edit_touch> (in
    <verbatim|Edit/Modify/edit_modify.cpp>).

    <item*|<cpp|undo_observer>>(<verbatim|Data/Observers/undo_observer.cpp>)
    Attached to the buffer root by each archiver; records every modification
    in the undo history through <cpp|archive_announce>.

    <item*|Scheme observers>The <scheme> hooks attached through a link
    repository with a callback (<cpp|scheme_observer>, in
    <verbatim|Data/Observers/tree_pointer.cpp>) are called as
    <scm|(<scm-arg|callback> 'announce <scm-arg|tree>
    <scm-arg|modification>)>, and similarly with <scm|'done> and
    <scm|'touched>. <cpp|tm_buffer_rep::attach_notifier> (<scheme>:
    <scm|buffer-attach-notifier>) uses this to call <scm|buffer-notify> on
    all changes of a buffer, after an initial call of
    <scm|buffer-initialize>; this is used for shared buffers and mirrored
    parts (<verbatim|part/part-shared.scm>).

    <item*|Positions and pointers><cpp|tree_position> (for cursor positions,
    see above), <cpp|tree_pointer>, tree addenda (<cpp|tree_addendum_new>,
    used for instance by animations) and the observers which store syntax
    highlighting information (<cpp|highlight_observer>, used by the packrat
    parser).
  </description>

  <subsection|From modifications to editors>

  When a modification inside a buffer is announced to an editor,
  <cpp|edit_announce> calls one of the <cpp|editor_rep::notify_*> routines
  (<cpp|notify_assign>, <cpp|notify_insert>, ..., <cpp|notify_set_cursor>).
  For all modifications except cursor settings, <cpp|edit_modify_rep>
  saves the cursor in a position observer <cpp|cur_pos> and forwards the
  modification to the typesetter (the global <cpp|notify_assign
  (typesetter, path, tree)> and so on), which invalidates the corresponding
  parts of the box tree. After the modification, <cpp|edit_done> calls
  <cpp|post_notify>:

  <\cpp-code>
    void

    edit_modify_rep::post_notify (path p) {

    \ \ if (!(rp \<less\>= p)) return;

    \ \ selection_cancel ();

    \ \ cancel_alt_selections ();

    \ \ notify_change (THE_TREE);

    \ \ tp= position_get (cur_pos);

    \ \ position_delete (cur_pos);

    \ \ cur_pos= nil_observer;

    \ \ go_to_correct (tp);

    }
  </cpp-code>

  Since <em|every> editor on a buffer has its own edit observer on the
  buffer root, a modification made in one view is automatically propagated to
  all other views on the same buffer: each of them updates its typesetter
  and its cursor, cancels its selection and schedules a redraw. No explicit
  synchronization between views is needed.

  Modifications of the kind <cpp|MOD_SET_CURSOR> do not change the tree;
  they are recorded in the undo history in order to restore cursor positions
  and selections on undo and redo. <cpp|notify_set_cursor> only acts if the
  modification carries the author of the editor, in which case it moves the
  cursor or restores the selection.

  <subsection|Undo and redo><label|sec-undo>

  Each editor owns an <cpp|archiver> (<verbatim|Data/History/archiver.hpp>),
  created with the author identifier of the editor and the root path of the
  buffer. The archiver stores a <cpp|patch> <cpp|archive> of past changes and
  a patch <cpp|current> for the changes of the ongoing user action. Each
  modification received by the undo observer is stored together with its
  inverse (computed with <cpp|invert> before the modification is applied) by
  <cpp|archiver_rep::add>, and the archiver is registered as pending.

  User actions are delimited by the editor:

  <\description>
    <item*|<cpp|start_editing ()>>Sets the global current author to the
    author of the editor (<cpp|set_author>). Called at the start of each
    keyboard and mouse event.

    <item*|<cpp|end_editing ()>>Calls <cpp|global_confirm ()>, which
    confirms all pending archivers: their <cpp|current> changes become one
    new undo step. Called at the end of each event. <cpp|cancel_editing>
    (called when an error occurs) calls <cpp|global_cancel> instead.

    <item*|<cpp|archive_state ()>>Records the current cursor and selection
    as <cpp|MOD_SET_CURSOR> modifications tagged with the author (the tags
    <verbatim|cursor>, <verbatim|cursor-clear>, <verbatim|start> and
    <verbatim|end>), so that undo returns to the right position.

    <item*|<cpp|mark_start (m)>, <cpp|mark_end (m)>, <cpp|mark_cancel
    (m)>>Delimit a group of changes with a marker obtained from
    <cpp|new_marker>. <cpp|mark_cancel> undoes the changes since the
    marker. This is used for multi-key shortcuts and input method pre-edit
    text.

    <item*|<cpp|add_undo_mark ()>, <cpp|remove_undo_mark ()>>Confirm the
    current changes, respectively reopen the last undo step.
  </description>

  <cpp|undo> and <cpp|redo> call <cpp|archiver_rep::undo> and
  <cpp|archiver_rep::redo>, which apply the inverse patches (with the flag
  <cpp|versioning> set, so that the resulting modifications are not archived
  again) and return the path where the cursor should go. Since all editors on
  a buffer see all modifications, undo steps are tagged with their author;
  <cpp|archiver_rep::undo> keeps undoing steps until it has undone one of its
  own author. The whole history of all archivers is cleared with
  <cpp|clear_undo_history> (<cpp|global_clear_history>).

  The archiver also implements the \Pmodified\Q status of a buffer. It
  remembers the depth of the archive at the last save and autosave; after
  <cpp|notify_save>, <cpp|conform_save> is true as long as the document is in
  the saved state (in particular after undoing all changes, in which case
  <cpp|undo> displays \PYour document is back in its original state\Q). The
  editor exports this as <cpp|need_save>, and <cpp|require_save> forces the
  modified state. At the buffer level,

  <\itemize>
    <item><cpp|buffer_modified (name)> is true if <em|some> view on the
    buffer needs to be saved (<cpp|tm_buffer_rep::needs_to_be_saved>);
    read-only buffers are never modified;

    <item><cpp|pretend_buffer_saved>, <cpp|pretend_buffer_modified> and
    <cpp|pretend_buffer_autosaved> call <cpp|notify_save> or
    <cpp|require_save> on all views of the buffer.
  </itemize>

  This is why <cpp|delete_window> only detaches the view of a window instead
  of deleting it: a buffer without views would always be reported as
  unmodified. For the same reason, modifications of a buffer which has no
  editor are not recorded in any undo history.

  <section|The event loop and repaint scheduling><label|sec-repaint>

  <subsection|The GUI loop and the interpose handler>

  With the <name|Qt> back-end (<verbatim|Plugins/Qt/qt_gui.cpp>, and its
  counterpart in <verbatim|Plugins/Qt6/>), events delivered by <name|Qt> to
  <TeXmacs> widgets are not processed immediately. The widgets call
  <cpp|qt_gui_rep::process_keypress>, <cpp|process_mouse>,
  <cpp|process_keyboard_focus>, <cpp|process_resize> or
  <cpp|process_command>, which append a <cpp|queued_event> to a private
  queue with <cpp|add_event> and request an update. The update is performed
  by <cpp|qt_gui_rep::update>, which is triggered by a single shot timer,
  and roughly does the following:

  <\enumerate>
    <item>execute the delayed <scheme> commands whose time has come
    (<cpp|process_delayed_commands>);

    <item>process the queued events with <cpp|process_queued_events>, which
    calls <cpp|handle_keypress>, <cpp|handle_mouse>,
    <cpp|handle_keyboard_focus> or <cpp|handle_notify_resize> on the target
    widget (for the canvas of a document, this widget is the editor);

    <item>call the <em|interpose handler> and then repaint the invalid
    regions of all widgets (<cpp|qt_simple_widget_rep::repaint_all ()>);
    when only ordinary keys were typed during this round, this step is
    postponed for a few milliseconds so that fast typing is not slowed down
    by the updates;

    <item>restart the timer, immediately if events are still pending,
    otherwise after a short delay or when the next delayed command is due.
  </enumerate>

  The interpose handler was installed by the server constructor with
  <cpp|gui_interpose (texmacs_interpose_handler)>; it calls
  <cpp|tm_server_rep::interpose_handler>:

  <\cpp-code>
    void

    tm_server_rep::interpose_handler () {

    \ \ ... \ // communication with plug-ins, pending commands

    \ \ async_eval_pending ();

    \ \ if (!headless_mode) {

    \ \ \ \ for (i=0; i\<less\>N(bufs); i++) {

    \ \ \ \ \ \ tm_buffer buf= (tm_buffer) bufs[i];

    \ \ \ \ \ \ for (j=0; j\<less\>N(buf-\<gtr\>vws); j++) {

    \ \ \ \ \ \ \ \ tm_view vw= (tm_view) buf-\<gtr\>vws[j];

    \ \ \ \ \ \ \ \ if (vw-\<gtr\>win != NULL) vw-\<gtr\>ed-\<gtr\>apply_changes ();

    \ \ \ \ \ \ }

    \ \ \ \ \ \ ... \ // same loop calling animate ()

    \ \ \ \ }

    \ \ \ \ windows_refresh ();

    \ \ }

    \ \ sync_databases ();

    \ \ idle_monitor_tick ();

    }
  </cpp-code>

  Hence <em|only active views> (views displayed in a window) are updated and
  retypeset in the background; passive views are typeset on demand, for
  instance when an environment value is queried. <cpp|windows_refresh>
  sends refresh requests to the widgets of all windows, which in particular
  updates dynamic menus and widgets; it is throttled with
  <cpp|windows_delayed_refresh>. The idle monitor keeps track of the CPU
  usage in order to implement <cpp|cpu_idle_time>.

  <subsection|Keyboard events><label|sec-keyboard>

  A key press is processed by the editor as follows.

  <\enumerate>
    <item><cpp|edit_interface_rep::handle_keypress (key, t)>
    (<verbatim|Edit/Interface/edit_keyboard.cpp>) records the key for the
    optional display of typed keys, forces a first typesetting if needed,
    calls <cpp|start_editing ()>, and passes the key to the <scheme>
    function <scm|keyboard-press> (or <scm|delayed-keyboard-press> for
    pre-edit strings of input methods).

    <item><scm|keyboard-press> is defined with <scm|tm-define> in
    <verbatim|kernel/gui/kbd-handlers.scm>; its default implementation calls
    <scm|(key-press <scm-arg|key>)>. It is overloaded in several contexts,
    for instance inside input fields of widgets
    (<verbatim|utils/misc/gui-utils.scm>), during interactive spell checking
    or in the shortcut editor.

    <item><scm|key-press> is the glue for <cpp|edit_interface_rep::key_press>.
    This routine handles input method and speech input, then tries to
    interpret the key, preceded by the keys of a pending shortcut (if any),
    as a shortcut with <cpp|try_shortcut>. If this fails, the key is
    inserted as text through the <scheme> function <scm|kbd-insert>
    (default: <scm|insert>), after a call to <cpp|archive_state ()>.

    <item><cpp|try_shortcut> asks the server for the binding with
    <cpp|sv-\<gtr\>get_keycomb> (section<nbsp><reference|sec-config>). If the
    key sequence is bound, the pending changes are enclosed in a new undo
    marker (<cpp|mark_start>), the help text of the shortcut is displayed in
    the footer, and either the bound command is executed or the bound string
    is inserted with <scm|kbd-insert>. When the next key extends the
    sequence to a longer shortcut, <cpp|mark_cancel> undoes the effect of
    the prefix before the longer shortcut is applied; this is how, for
    instance, repeated presses of a variant key cycle through symbols.

    <item>Back in <cpp|handle_keypress>, the focus loci are updated,
    <cpp|notify_change (THE_DECORATIONS)> is called and <cpp|end_editing
    ()> confirms the undo step. If an exception is raised during the
    processing, the changes are cancelled with <cpp|cancel_editing>.
  </enumerate>

  Keyboard focus changes are handled by <cpp|handle_keyboard_focus>, which
  updates <cpp|got_focus>, makes the view current when it obtains the focus
  and calls the <scheme> hook <scm|keyboard-focus>.

  <subsection|Mouse events>

  <cpp|edit_interface_rep::handle_mouse (kind, x, y, mods, t, data)>
  (<verbatim|Edit/Interface/edit_mouse.cpp>) first makes sure that the
  document is typeset (the box tree is needed to interpret the coordinates),
  calls <cpp|start_editing>, converts the coordinates according to the
  magnification, detects the start of left and right drags, and passes the
  event to the <scheme> function <scm|mouse-event>. Its default definition
  in <verbatim|kbd-handlers.scm> calls the glue routine <scm|mouse-any>,
  that is, <cpp|edit_interface_rep::mouse_any>. The latter updates the loci
  under the mouse (hyperlinks, tooltips), dispatches to the graphics editor
  when the pointer is inside a <markup|graphics>, and otherwise calls
  <cpp|mouse_click>, <cpp|mouse_drag>, <cpp|mouse_select>,
  <cpp|mouse_extra_click>, <cpp|mouse_paste>, <cpp|mouse_adjust> or
  <cpp|mouse_scroll> depending on the kind of event. Drop events are passed
  to <scm|mouse-drop-event>. As for keyboard events, the handler ends with
  <cpp|end_editing ()>.

  <subsection|Change notification and <cpp|apply_changes>>

  Editing routines never redraw the screen directly. Instead, they call
  <cpp|notify_change (int flags)>, which adds the flags to
  <cpp|env_change> and asks the GUI for an update (<cpp|needs_update>).
  The flags are defined in <verbatim|editor.hpp>:

  <\description>
    <item*|<cpp|THE_TREE>>The document tree changed; the document has to be
    retypeset.

    <item*|<cpp|THE_ENVIRONMENT>>The initial environment or the style
    changed; everything has to be retypeset.

    <item*|<cpp|THE_CURSOR>, <cpp|THE_SELECTION>, <cpp|THE_FOCUS>>The
    cursor, the selection or the keyboard focus changed.

    <item*|<cpp|THE_EXTENTS>>The size of the document or of the window
    changed.

    <item*|<cpp|THE_DECORATIONS>>Menus, toolbars and footer might have
    changed.

    <item*|<cpp|THE_LOCUS>>The active loci (hyperlinks, ...) have to be
    redrawn.

    <item*|<cpp|THE_MENUS>>Force an update of the menus.

    <item*|<cpp|THE_FREEZE>>Do not scroll to make the cursor visible.

    <item*|<cpp|THE_TOOLTIP>, <cpp|THE_SPELL_ERRORS>>Tooltips and
    highlighted spelling errors.
  </description>

  <cpp|apply_changes> (<verbatim|Edit/Interface/edit_interface.cpp>),
  called by the interpose handler, processes the accumulated flags:

  <\enumerate>
    <item>If nothing changed, it only updates the menus, toolbars and footer
    with <cpp|update_menus ()>, provided that the document changed since the
    last such update and that the user has been idle for at least 1/6 of a
    second (<cpp|idle_time>).

    <item>It adapts the environment variables which depend on the window:
    the zoom factor, the page size for the <verbatim|automatic> page medium,
    scroll bars and the visibility of window bars.

    <item>On <cpp|THE_ENVIRONMENT> it invalidates the whole typesetting; on
    <cpp|THE_TREE> or <cpp|THE_ENVIRONMENT> it retypesets the invalid
    parts (<cpp|typeset>) and invalidates the corresponding screen
    rectangles.

    <item>On changes of the tree, environment or extents it recomputes the
    extents of the document and passes them to the window.

    <item>On changes of the cursor, selection or focus it recomputes the
    graphical cursor, scrolls to make it visible (unless
    <cpp|THE_FREEZE> is set), and recomputes the rectangles which
    highlight the context, focus and semantic selection.

    <item>It recomputes the rectangles of the selection and of the
    alternative selections, triggers continuous spell checking, updates the
    loci under the mouse and the focus loci (calling the <scheme> function
    <scm|link-follow-ids>), and updates the menus if <cpp|THE_MENUS> is set.

    <item>Finally, it resets <cpp|env_change> and records the time of the
    change in <cpp|last_change>.
  </enumerate>

  All drawing is done by invalidating rectangles (<cpp|invalidate>). The
  GUI then calls <cpp|handle_repaint (renderer, x1, y1, x2, y2)>
  (<verbatim|Edit/Interface/edit_repaint.cpp>), which draws the background,
  the typeset boxes, the selections, the cursor and the other decorations
  into the renderer, using a \Pstored\Q or \Pshadow\Q renderer as a
  cache. <cpp|handle_repaint> expects <cpp|env_change> to be zero: all
  changes must have been applied before the screen is repainted.

  <cpp|update_menus ()> rebuilds the main menu, the icon bars and the side
  and bottom tools (through the <cpp|SERVER> macro, see
  section<nbsp><reference|sec-frames>), updates the footer
  (<cpp|set_footer>), updates the \Pmodified\Q indicator of all windows on
  the buffer, updates the <abbr|DRD>, and saves the user preferences if they
  were modified. The <scheme> routine <scm|update-menus> calls it directly.

  <section|Windows, frames, menus and the footer><label|sec-frames>

  This section briefly describes how the server and the editors drive the
  window decorations. The widgets themselves are described in <hlink|the
  abstract widget system|widgets.en.tm>, and the <scheme> side of menus in
  <hlink|widgets in Scheme|../scheme/gui/scheme-gui.en.tm>.

  The routines of <cpp|tm_frame_rep> (<verbatim|Window/tm_frame.cpp>) are
  thin wrappers which forward to the corresponding method of the current
  window, <cpp|concrete_window ()>, for instance

  <\cpp-code>
    void

    tm_frame_rep::menu_main (string menu) {

    \ \ if (!has_current_window ()) return;

    \ \ concrete_window () -\<gtr\> menu_main (menu);

    }
  </cpp-code>

  They cover the window properties (<cpp|set_window_property>,
  <cpp|get_window_property> and typed variants, stored in
  <cpp|tm_window_rep::props>), the visibility of the header, icon bars, side
  tools, bottom tools and footer (<cpp|show_header>, <cpp|show_icon_bar>,
  <cpp|show_side_tools>, <cpp|show_bottom_tools>, <cpp|show_footer> and the
  <cpp|visible_*> queries), the zoom factor, scroll bars, extents and scroll
  position of the canvas, full screen mode, messages in the footer, and
  dialogues and interactive prompts.

  Menus and toolbars are given as <scheme> expressions. When an editor
  obtains the focus (<cpp|resume>) or when its menus have to be updated
  (<cpp|update_menus>), it calls

  <\cpp-code>
    SERVER (menu_main ("(horizontal (link texmacs-menu))"));

    SERVER (menu_icons (0, "(horizontal (link texmacs-main-icons))"));

    SERVER (menu_icons (1, "(horizontal (link texmacs-mode-icons))"));

    SERVER (menu_icons (2, "(horizontal (link texmacs-focus-icons))"));

    SERVER (menu_icons (3, "(horizontal (link texmacs-extra-icons))"));
  </cpp-code>

  and similarly <cpp|side_tools> and <cpp|bottom_tools> with the dynamic
  menus <scm|texmacs-left-tools>, <scm|texmacs-side-tools>,
  <scm|texmacs-bottom-tools> and <scm|texmacs-extra-tools>, which receive the
  window as an argument. <cpp|tm_window_rep::get_menu_widget> expands the
  menu with the <scheme> function <scm|menu-expand> (with <cpp|the_drd> set
  to that of the window's editor) and does nothing if the expansion is the
  same as the one which is currently displayed at the same place. Otherwise
  it reuses a cached widget for this expansion (only for the main menu and
  the icon bars) or builds a new one with <cpp|make_menu_widget>, which
  calls the <scheme> function <scm|make-menu-widget>. New widgets are
  stored in <cpp|menu_cache>: always for side and bottom tools, and for the
  main menu and icon bars only if <scm|cache-menu?> allows it.
  <cpp|tm_window_rep::refresh> clears this cache. The resulting widgets are
  installed with <cpp|set_main_menu>, <cpp|set_main_icons>,
  <cpp|set_mode_icons>, <cpp|set_focus_icons>, <cpp|set_user_icons>,
  <cpp|set_side_tools>, <cpp|set_left_tools>, <cpp|set_bottom_tools> and
  <cpp|set_extra_tools>. The numbering of the <cpp|which> arguments is: icon
  bars 0 to 3 (main, mode, focus, user), side tools 0 (right) and 1 (left),
  bottom tools 0 (bottom) and 1 (extra).

  The footer is managed by the editor (<verbatim|Edit/Interface/edit_footer.cpp>).
  <cpp|set_message (left, right, temp)> stores a message and calls
  <cpp|notify_change (THE_DECORATIONS)>; <cpp|set_footer> displays the
  message if there is one, and otherwise computes a description of the
  context of the cursor (mode, language, font, and the path of enclosing
  tags) with <cpp|set_left_footer> and <cpp|set_right_footer>. The server
  routine <cpp|tm_frame_rep::set_message> (<scheme>: <scm|set-message>)
  forwards to the current editor. The footer is also used for interactive
  input (<cpp|tm_frame_rep::interactive> and
  <cpp|tm_window_rep::interactive>), unless the preference
  <verbatim|interactive questions> is set to <verbatim|popup>.

  Besides the windows of documents, <TeXmacs> can embed editors inside
  widgets (input fields of dialogues and side panes).
  <cpp|texmacs_input_widget> in <verbatim|Window/tm_window.cpp> creates a
  buffer named <verbatim|tmfs://aux/TeXmacs-input-<em|n>> (unless a name is
  given), obtains a passive view on it, and creates an anonymous
  <cpp|tm_window_rep> (with <cpp|id> equal to <cpp|url_none ()>) around the
  editor; the <cpp|mvw> field of the embedded editor points to the view of
  the document from which it was opened. Such buffers are recognized by
  <cpp|is_embedded_buffer> and <cpp|edit_interface_rep::is_embedded_widget>,
  and are removed together with their widget.

  <section|Configuration and keyboard maps><label|sec-config>

  <subsection|Keyboard configuration>

  Key bindings are defined in <scheme> with the <scm|kbd-map> macro of
  <verbatim|kernel/gui/kbd-define.scm>, possibly conditioned on modes and
  contexts, and looked up with <scm|kbd-find-key-binding>. The
  <cpp|tm_config_rep> part of the server implements the lookup of a key
  sequence entered by the user in <cpp|get_keycomb (which, status, cmd,
  shorth, help)>:

  <\enumerate>
    <item>The sequence is simplified with respect to the <em|variant keys>
    (<cpp|variant_simplification>). By default the variant key is
    <verbatim|tab> and the reverse variant key is <verbatim|S-tab>; they
    can be changed with <cpp|set_variant_keys> (<scheme>:
    <scm|set-variant-keys>). Trailing variant keys which do not lead to a
    binding are dropped, and a reverse variant key removes the last variant.

    <item>The <em|post wildcards> are applied (<cpp|apply_wildcards>).
    Wildcards are rewriting rules on key sequences, declared in <scheme>
    with <scm|kbd-wildcards> and registered with
    <cpp|insert_kbd_wildcard> (<scheme>: <scm|insert-kbd-wildcard>). Pre
    wildcards are applied to the keys in the definitions
    (<cpp|kbd_pre_rewrite>), post wildcards at lookup time
    (<cpp|kbd_post_rewrite>).

    <item>The binding is looked up with the <scheme> function
    <scm|kbd-find-key-binding> (<cpp|find_key_binding>). The result
    determines <cpp|status>: 0 if there is no binding, 1 if the binding is
    a command (returned in <cpp|cmd>), 2 if it is a string to be inserted
    (returned in <cpp|shorth>). In the last two cases <cpp|help> contains
    the help text of the binding. The status is increased by 3 if the
    sequence was reduced to a bare variant key.
  </enumerate>

  <cpp|kbd_system_rewrite> translates a key sequence into a tree for
  displaying shortcuts in menus and in the footer, using system specific
  names or symbols for the modifiers and special keys (for instance the
  <name|macOS> modifier symbols). <cpp|kbd_get_command> looks up named
  commands through the <scheme> function <scm|kbd-get-command>.
  <cpp|set_font_rules> installs font substitution rules.

  <subsection|Preferences>

  User preferences are stored as pairs of strings in
  <verbatim|$TEXMACS_HOME_PATH/system/preferences.scm>. At the <c++> level,
  <cpp|load_user_preferences>, <cpp|save_user_preferences>,
  <cpp|get_user_preference> and <cpp|set_user_preference>
  (<verbatim|System/Boot/preferences.cpp>) access this file directly; they
  are used during startup, before <scheme> is available. Afterwards, code
  should use <cpp|get_preference (var, def)> and <cpp|set_preference (var,
  val)> (<verbatim|Scheme/Scheme/object.cpp>), which call the <scheme>
  functions <scm|get-preference> and <scm|set-preference> once the
  preferences have been booted. On the <scheme> side
  (<verbatim|kernel/texmacs/tm-preferences.scm>), preferences are declared
  with default values and call-back functions using
  <scm|define-preferences>; the call-back is invoked when the preference
  changes (<scm|notify-preference>). Examples can be found in
  <verbatim|texmacs/texmacs/tm-server.scm>. Modified preferences are written
  back to disk by <cpp|save_user_preferences>, which is called from
  <cpp|update_menus>.

  Several preferences are read by the code discussed in this document:
  window decorations in <cpp|new_window>, <verbatim|show full context>,
  <verbatim|show table cells>, <verbatim|show focus> and <verbatim|show only
  semantic focus> in <cpp|apply_changes>, <verbatim|look and feel> for the
  clipboard and keyboard conventions, and <verbatim|case sensitive
  shortcuts> in <cpp|kbd_system_rewrite>.

  <section|The <scheme> interface>

  Most of the functions described in this document are exported to <scheme>
  by the glue in <verbatim|Scheme/Glue/>. The user level documentation of
  these functions can be found in <hlink|the Scheme buffer
  API|../scheme/buffer/scheme-buffer.en.tm>, which describes
  <hlink|buffers|../scheme/buffer/buffer-api.en.tm>, <hlink|views|../scheme/buffer/view-api.en.tm>
  and <hlink|windows|../scheme/buffer/window-api.en.tm>. The table below
  gives the correspondence with the <c++> routines.

  <\description>
    <item*|Buffers><scm|buffer-list> (<cpp|get_all_buffers>),
    <scm|current-buffer-url> (<cpp|get_current_buffer_safe>),
    <scm|path-to-buffer> (<cpp|path_to_buffer>), <scm|buffer-new>
    (<cpp|make_new_buffer>), <scm|buffer-rename> (<cpp|rename_buffer>),
    <scm|buffer-set> and <scm|buffer-get> (<cpp|set_buffer_tree>,
    <cpp|get_buffer_tree>), <scm|buffer-set-body> and <scm|buffer-get-body>,
    <scm|buffer-set-master> and <scm|buffer-get-master>,
    <scm|buffer-set-title> and <scm|buffer-get-title>,
    <scm|buffer-last-save>, <scm|buffer-last-visited>,
    <scm|buffer-modified?> (<cpp|buffer_modified>),
    <scm|buffer-modified-since-autosave?>, <scm|buffer-pretend-modified>,
    <scm|buffer-pretend-saved>, <scm|buffer-pretend-autosaved>,
    <scm|buffer-attach-notifier>, <scm|buffer-has-name?>, <scm|buffer-aux?>
    (<cpp|is_aux_buffer>), <scm|buffer-embedded?>, <scm|buffer-import>,
    <scm|buffer-load>, <scm|buffer-export>, <scm|buffer-save>,
    <scm|buffer-focus> (<cpp|focus_on_buffer>) and <scm|buffer-focus*>
    (<cpp|var_focus_on_buffer>).

    <item*|Views><scm|view-list> (<cpp|get_all_views>),
    <scm|buffer-\<gtr\>views>, <scm|current-view-url>,
    <scm|window-\<gtr\>view>, <scm|view-\<gtr\>buffer>,
    <scm|view-\<gtr\>window-url>, <scm|view-new> (<cpp|get_new_view>),
    <scm|view-passive> (<cpp|get_passive_view>), <scm|view-recent>
    (<cpp|get_recent_view>), <scm|view-delete> (<cpp|delete_view>),
    <scm|window-set-view> (<cpp|window_set_view>), <scm|switch-to-buffer>
    (<cpp|switch_to_buffer>) and <scm|set-drd> (<cpp|set_current_drd>).

    <item*|Windows><scm|window-list> (<cpp|windows_list>),
    <scm|windows-number>, <scm|current-window>, <scm|buffer-\<gtr\>windows>,
    <scm|window-to-buffer>, <scm|window-set-buffer>, <scm|window-focus>,
    <scm|switch-to-window>, <scm|new-buffer> (<cpp|create_buffer>),
    <scm|open-buffer-in-window> (<cpp|new_buffer_in_new_window>),
    <scm|open-window>, <scm|clone-window>, <scm|cpp-buffer-close>
    (<cpp|kill_buffer>), <scm|kill-window> and
    <scm|kill-current-window-and-buffer>.

    <item*|Projects><scm|project-attach>, <scm|project-detach>,
    <scm|project-attached?> and <scm|project-get>.

    <item*|Server and current window>(<verbatim|build-glue-server.scm>)
    <scm|window-get-serial>, <scm|window-set-property>,
    <scm|window-get-property>, <scm|show-header>, <scm|show-icon-bar>,
    <scm|show-side-tools>, <scm|show-bottom-tools>, <scm|show-footer>,
    <scm|full-screen-mode>, <scm|set-window-zoom-factor>,
    <scm|set-message>, <scm|recall-message>, <scm|insert-kbd-wildcard>,
    <scm|set-variant-keys>, <scm|kbd-system-rewrite>,
    <scm|update-all-buffers>, <scm|quit-TeXmacs>, ...

    <item*|Current editor>(<verbatim|build-glue-editor.scm>)
    <scm|root-tree>, <scm|buffer-path>, <scm|buffer-tree>,
    <scm|cursor-path>, <scm|key-press>, <scm|mouse-any>,
    <scm|get-input-mode>, <scm|get-env>, <scm|go-to-path>, the selection
    routines (<scm|selection-active-any?>, <scm|selection-get-start>,
    ...), the undo routines (<scm|start-editing>, <scm|end-editing>,
    <scm|archive-state>, <scm|mark-start>, <scm|mark-end>,
    <scm|mark-cancel>, <scm|undo>, <scm|redo>, <scm|clear-undo-history>),
    <scm|notify-change>, <scm|idle-time>, <scm|update-menus>, ...
  </description>

  Several of these functions return <cpp|url_none ()> when there is no
  answer. The library <verbatim|kernel/library/base.scm> defines more
  convenient wrappers which return <scm|#f> in that case:
  <scm|current-buffer>, <scm|current-view>, <scm|window-\<gtr\>buffer>,
  <scm|view-\<gtr\>window>, <scm|path-\<gtr\>buffer>, as well as
  <scm|buffer-exists?>, <scm|buffer-\<gtr\>tree>,
  <scm|tree-\<gtr\>buffer>, <scm|buffer-master> and
  <scm|buffer-\<gtr\>window>. The user level commands for loading, saving
  and closing documents are those of <verbatim|tm-files.scm> and
  <verbatim|tm-server.scm> discussed above.

  <section|Pitfalls and guidelines>

  <\itemize>
    <item>Always go through the <abbr|URL> based <abbr|API> rather than
    keeping <cpp|tm_buffer>, <cpp|tm_view> or <cpp|tm_window> pointers:
    they are deleted when buffers, views or windows are closed, and
    <cpp|view_to_editor> or <cpp|concrete_view> return null values for
    stale <abbr|URL>s.

    <item>Remember that the editor glue and the frame routines of the server
    act on the current view and window. To act on another buffer, use
    <scm|with-buffer> in <scheme>, or change the current view temporarily
    and restore it (as the <cpp|SERVER> macro does) in <c++>.

    <item>Modify documents only through the functions of
    <verbatim|observer.cpp> (<cpp|assign>, <cpp|insert>, ...) or the
    editing routines built on top of them. Direct assignments to subtrees
    of <cpp|the_et> bypass the observers, so neither the typesetter, nor the
    other views, nor the undo system would notice them.

    <item>Group the modifications of one user action between
    <cpp|start_editing> and <cpp|end_editing> (the event handlers already do
    this); otherwise they may be merged with the next action in the undo
    history, or be attributed to the wrong author.

    <item>Do not typeset or repaint from editing routines. Call
    <cpp|notify_change> with the appropriate flags; the interpose handler
    will call <cpp|apply_changes> at the next occasion. If up-to-date
    typesetting information is needed immediately (for instance a box or
    the environment at the cursor), <cpp|apply_changes> can be called
    explicitly, but only on active views.

    <item>Passive views are not updated by the interpose handler. Keep this
    in mind when the result of an operation on a buffer depends on the
    typesetting of that buffer.

    <item>The low level loading and saving routines return <cpp|true> on
    failure.
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
