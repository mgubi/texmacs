<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Metadata: buffers, views, windows and projects>

  This page describes the classes which record what is open and where it is
  displayed, together with the global tables in which they are kept and the
  routines of <verbatim|Texmacs/Data/> which manipulate them. For the life
  cycle of these objects (in which order they are created, displayed and
  destroyed by the user level commands) see <hlink|the server, buffers,
  views and windows|server.en.tm>.

  <section|Ownership and identifiers>

  The metadata objects are plain <c++> objects allocated with
  <cpp|tm_new> and deleted with <cpp|tm_delete>; they are <em|not>
  reference counted. Ownership is as follows:

  <\description>
    <item*|Buffers>are owned by the global array <cpp|bufs>
    (<verbatim|new_buffer.cpp>). They are created by <cpp|insert_buffer>
    and destroyed by <cpp|remove_buffer>.

    <item*|Views>are owned by their buffer (the array
    <cpp|tm_buffer_rep::vws>). They are created by <cpp|get_new_view> and
    destroyed by <cpp|delete_view>, or implicitly when their buffer is
    removed.

    <item*|Windows>are owned by the static table <cpp|tm_window_table>
    (<verbatim|new_window.cpp>), which maps window identifiers to
    <cpp|tm_window_rep*>. They are created by <cpp|new_window> and
    destroyed by <cpp|delete_window>. The windows of embedded <TeXmacs>
    widgets are the exception: they are not in this table and are owned by
    the widget (see <hlink|embedded widgets|server-layer-windows.en.tm>).
  </description>

  Everything outside <verbatim|Texmacs/Data/> refers to these objects by
  <abbr|URL>, and converts with the <em|concrete> and <em|abstract>
  functions:

  <\description>
    <item*|Buffer>The <abbr|URL> is the buffer name itself.
    <cpp|concrete_buffer (url)> searches <cpp|bufs> linearly and returns
    <cpp|NULL> if there is no such buffer; <cpp|concrete_buffer_insist>
    loads the file first if needed.

    <item*|View><verbatim|tmfs://view/<em|nr>/<em|encoded-name>>.
    <cpp|abstract_view> builds it from the view number and the buffer
    name; <cpp|concrete_view> parses it, looks up the buffer, and then
    the view with the given number among the views of that buffer. The
    buffer name is encoded by <cpp|encode_url>: a relative name becomes
    <verbatim|here/...>, a local absolute name <verbatim|default/...>,
    and another root (<verbatim|tmfs>, <verbatim|http>, ...) is followed
    by a slash and the rest of the name; <cpp|decode_url> reverses this
    (with special handling of drive letters on <name|Windows>).

    <item*|Window><verbatim|tmfs://window/<em|n>>, allocated by
    <cpp|create_window_id>; the counter is never decremented, so window
    identifiers are not reused during a session. <cpp|concrete_window
    (url)> is a lookup in <cpp|tm_window_table>; <cpp|abstract_window>
    returns the field <cpp|id> of the window.
  </description>

  Since a view <abbr|URL> contains the buffer name, it changes when the
  buffer is renamed; <cpp|rename_buffer> therefore removes the old view
  <abbr|URL>s from the view history before the rename and puts the new ones
  back afterwards (<cpp|notify_rename_before>, <cpp|notify_rename_after>).
  Any view <abbr|URL> kept elsewhere (for instance in <scheme>) becomes
  invalid.

  <section|Buffers>

  A buffer is described by three objects: the <cpp|tm_buffer_rep> which
  ties everything together, the file related information
  <cpp|new_buffer_rep>, and the document data <cpp|new_data_rep>. The body
  of the document is stored in neither of them, but in the global edit tree.

  <subsection|The global edit tree>

  All document bodies are children of the global tree <cpp|the_et>
  (<verbatim|Data/Document/new_document.cpp>), a <markup|tuple> created at
  startup in <cpp|texmacs_entrypoint>. The three functions of
  <verbatim|new_document.hpp> manage its slots:

  <\description>
    <item*|<cpp|path new_document ()>>Returns the path of a free slot,
    reusing a child equal to <verbatim|UNINIT> if there is one, and
    appending a new child otherwise. The slot is initialized to an empty
    <markup|document>.

    <item*|<cpp|void set_document (path rp, tree t)>>Assigns a <em|copy>
    of <cpp|t> to the slot.

    <item*|<cpp|void delete_document (path rp)>>Sets the slot to
    <verbatim|UNINIT> and removes the observers of the old subtree.
  </description>

  The path of the slot is the <em|root path> <cpp|rp> of the buffer and of
  all its editors. Since slots are reused, a root path identifies a buffer
  only for as long as the buffer exists.

  <subsection|The class <cpp|new_buffer_rep>>

  <\explain>
    <cpp|class new_buffer_rep><explain-synopsis|file related information>
  <|explain>
    Declared in <verbatim|Texmacs/Data/new_buffer.hpp>; a
    <cpp|concrete_struct> with the handle <cpp|new_buffer>. Its fields
    are:

    <\description>
      <item*|<cpp|url name>>The name of the buffer, which is also its
      identifier.

      <item*|<cpp|url master>>The base name used to resolve relative links
      and file names, and for navigation. Equal to <cpp|name> except for
      auxiliary buffers.

      <item*|<cpp|string fm>>The format; initialized to
      <verbatim|"texmacs"> and currently not updated by loading or saving
      (the format of a save is recomputed from the file name).

      <item*|<cpp|string title>>The title shown in window titles and in
      the <menu|Go> menu; computed by <cpp|propose_title>.

      <item*|<cpp|bool read_only>>A read only buffer never needs to be
      saved, and its editors typeset it in read only mode. The flag is
      initialized to <cpp|false> and is currently not set anywhere else.

      <item*|<cpp|bool secure>>Whether the document may execute
      potentially insecure code; initialized with <cpp|is_secure (name)>.

      <item*|<cpp|int last_save>>The time stamp of the file at the last
      load or save, used to detect changes on disk. Initialized to the
      most negative <cpp|int> but one, which means \Pnever saved\Q.

      <item*|<cpp|time_t last_visit>>The last time a view on the buffer
      became current; used by <scheme> to sort recent buffers
      (<scm|buffer-last-visited>).
    </description>
  </explain>

  <subsection|The class <cpp|new_data_rep>>

  <\explain>
    <cpp|class new_data_rep><explain-synopsis|everything but the body>
  <|explain>
    Declared in <verbatim|Data/Document/new_data.hpp>; a
    <cpp|concrete_struct> with the handle <cpp|new_data>. It holds the
    parts of a <TeXmacs> document other than the body:

    <\description>
      <item*|<cpp|tree project>>The relative name of the project file, or
      the empty string.

      <item*|<cpp|tree style>>The style, a <markup|tuple> of style and
      package names.

      <item*|<cpp|init>, <cpp|fin>>The initial and final values of
      environment variables.

      <item*|<cpp|ref>, <cpp|aux>>Labels with their references, and the
      auxiliary data (tables of contents, bibliographies, indices, ...).

      <item*|<cpp|att>>Attachments, such as the <LaTeX> source of an
      imported document.
    </description>

    <cpp|detach_data (doc, data)> splits a complete document into its body
    (the return value) and <cpp|data>; <cpp|attach_data (body, data,
    no_aux)> does the converse. <cpp|attach_data> leaves out empty parts,
    removes from the initial environment the screen dependent variables
    <verbatim|page-screen-width>, <verbatim|page-screen-height> and
    <verbatim|full-screen-mode> and, unless <verbatim|no-zoom> is set, the
    zoom factor; with <cpp|no_aux> it also leaves out the references and
    the auxiliary data.
  </explain>

  The data are shared between the buffer and its editors in an
  asymmetric way. The editor keeps its <em|own> copies of the style and
  of the initial environment (<cpp|edit_typeset_rep::set_data> copies them
  in), and its own handle on the final environment, but reads and writes
  <cpp|ref>, <cpp|aux> and <cpp|att> directly in <cpp|buf-\<gtr\>data>.
  The style and initial environment of the buffer are therefore not
  updated when the user changes them in the editor. They are replaced
  wholesale by <cpp|set_buffer_tree>, and copied back from the editor by
  <cpp|buffer_export>, which calls <cpp|ed-\<gtr\>get_data
  (buf-\<gtr\>data)> before writing the file (but not for PostScript and
  PDF). The only other writer is <cpp|edit_typeset_rep::init_update>,
  which stores some variables of project chapters (such as the first page
  number) directly in <cpp|buf-\<gtr\>data-\<gtr\>init>.

  <subsection|The class <cpp|tm_buffer_rep>>

  <\explain>
    <cpp|class tm_buffer_rep><explain-synopsis|an open document>
  <|explain>
    Declared in <verbatim|Texmacs/tm_buffer.hpp>; <cpp|tm_buffer> is a
    plain pointer to it, and <cpp|nil_buffer ()> and <cpp|is_nil> test for
    <cpp|NULL>. Its fields are:

    <\description>
      <item*|<cpp|new_buffer buf>>The file related information.

      <item*|<cpp|new_data data>>The document data.

      <item*|<cpp|array\<less\>tm_view\<gtr\> vws>>The views on the buffer.

      <item*|<cpp|tm_buffer prj>>The buffer of the project, or
      <cpp|NULL>.

      <item*|<cpp|path rp>>The root path of the body in <cpp|the_et>. The
      constructor obtains it with <cpp|new_document ()> and the destructor
      releases it with <cpp|delete_document>.

      <item*|<cpp|link_repository lns>>The links of the buffer as a whole,
      used by the buffer notifier.

      <item*|<cpp|bool notify>>Whether the notifier is attached.
    </description>

    Its methods are:

    <\description>
      <item*|<cpp|attach_notifier ()>>Calls the <scheme> function
      <scm|buffer-initialize> (defined in <verbatim|part/part-shared.scm>)
      with the buffer name and its body, and registers the body as a locus
      with the link type <verbatim|"buffer-notify">, so that every
      modification of the buffer is reported to the <scheme> function
      <scm|buffer-notify>. This is used to share and mirror whole buffers
      (see <hlink|collaboration|collaboration.en.tm>). It is called from
      <scheme> with <scm|buffer-attach-notifier> and only once per buffer.

      <item*|<cpp|needs_to_be_saved ()>,
      <cpp|needs_to_be_autosaved ()>>True if the buffer is not read only
      and some view's editor reports unsaved changes
      (<cpp|ed-\<gtr\>need_save ()> and <cpp|need_save (false)>). The
      \Pmodified\Q state is thus not stored in the buffer but derived from
      the undo history of the editors (see <hlink|undo and
      redo|server.en.tm>); this is why a buffer must always keep at least
      one view.
    </description>
  </explain>

  <subsection|Buffer routines>

  Most routines of <verbatim|new_buffer.cpp> take a buffer name and do
  nothing (or return a neutral value) if there is no such buffer. The
  exceptions are <cpp|buffer_export> and <cpp|buffer_save>, which go
  through <cpp|get_recent_view> and therefore <em|create> an empty buffer
  with that name, and <cpp|last_visited>, which returns the current
  time.

  <paragraph|The list of buffers.><cpp|get_all_buffers> returns the names
  in <em|reverse> order of creation; <cpp|number_buffers>,
  <cpp|get_current_buffer> (asserts that there is a current view),
  <cpp|get_current_buffer_safe>, <cpp|path_to_buffer (p)> (the buffer
  whose root path is a prefix of <cpp|p>). <cpp|remove_buffer> is meant to delete
  all views, then remove the buffer from <cpp|bufs> and delete it; if this
  was the last buffer and no remote clients are connected, the program
  quits first. Note that its loop <verbatim|for (i=0; i\<less\>N(buf-\<gtr\>vws);
  i++) delete_view (...)> skips every other view, because
  <cpp|delete_view> removes the view from <cpp|buf-\<gtr\>vws>; with two
  or more views, some views survive the deletion of their buffer, with a
  dangling buffer pointer, and remain in the view history. This is a bug
  in the code (it should loop while <cpp|buf-\<gtr\>vws> is not empty).

  <paragraph|Names and titles.><cpp|make_new_buffer> creates an empty
  buffer with the first free scratch name <verbatim|no_name_<em|i>.tm>;
  <cpp|buffer_has_name> is false for scratch names. <cpp|propose_title>
  computes a title: the file name, \PNo name [<em|i>]\Q for scratch
  buffers, the title given by the <scheme> function <scm|tmfs-title> for
  <verbatim|tmfs://> buffers, and a suffix \P(2)\Q, \P(3)\Q, ... if
  another buffer has the same title. <cpp|set_title_buffer> also updates
  the title and file of all windows which show the buffer.
  <cpp|rename_buffer> was described above; it kills any existing buffer
  with the new name.

  <paragraph|Contents.><cpp|set_buffer_tree (name, doc)> creates the
  buffer if needed, splits the document with <cpp|detach_data>, stores the
  body (with <cpp|set_document> for a new buffer, with <cpp|assign> for an
  existing one, so that the editors are notified), passes the new data to
  the editors, recomputes the title, loads the project buffer if the
  document declares a project (for an existing buffer, only if the
  project changed), and marks the buffer as saved.
  <cpp|get_buffer_tree> returns <cpp|attach_data (body, data, true)>.
  <cpp|set_buffer_body> and <cpp|get_buffer_body> work on the body only
  (<cpp|set_buffer_body> also marks the buffer as saved).

  <paragraph|Master and auxiliary buffers.><cpp|set_master_buffer> and
  <cpp|get_master_buffer>; changing the master notifies the editors with
  <cpp|THE_ENVIRONMENT>, since relative links must be re-resolved.
  <cpp|is_aux_buffer> tests whether the master differs from the name.

  <paragraph|Save status.><cpp|buffer_modified>,
  <cpp|buffer_modified_since_autosave>, <cpp|pretend_buffer_modified>
  (calls <cpp|require_save> on the editors), <cpp|pretend_buffer_saved>
  (calls <cpp|notify_save> on the editors and records the time stamp of
  the file in <cpp|last_save>), <cpp|pretend_buffer_autosaved>,
  <cpp|get_last_save_buffer>, <cpp|set_last_save_buffer>,
  <cpp|last_visited>.

  <paragraph|Loading.><cpp|import_tree (u, fm)> resolves <cpp|u> (also
  relative to the current buffer), reads the file and calls
  <cpp|import_loaded_tree (s, u, fm)>. The latter guesses the format if it
  is <verbatim|generic>, converts the string to a tree with
  <cpp|generic_to_tree>, registers the link locations stored in the
  document, and calls <cpp|attach_subformat>, which turns source files of
  a known programming language into a document with style
  <verbatim|code>, mode <verbatim|prog> and the right
  <verbatim|prog-language>. <cpp|buffer_import (name, src, fm)> and
  <cpp|buffer_load (name)> put the result in a buffer.

  <paragraph|Saving.><cpp|export_tree (doc, u, fm)> converts a complete
  document with <cpp|tree_to_generic> and writes it; documents in
  <TeXmacs> format whose initial environment contains an
  <verbatim|encryption> variable are first passed through the <scheme> hook <scm|tree-export-encrypted>.
  <cpp|buffer_export (name, dest, fm)> uses the most recent view on the
  buffer: for PostScript and PDF it prints with the editor, for
  <verbatim|verbatim> and <verbatim|html> it first lets the editor expand
  the body, then it synchronizes the data from the editor, attaches them
  (without the auxiliary data if the editor says so), adds the link
  locations and, for <LaTeX>, the view <abbr|URL>, so that the converter
  can later expand macros with the typesetter of that view
  (<cpp|latex_expand>). <cpp|buffer_save (name)> exports to the file
  itself, marks the buffer as saved and clears the \Pmodified\Q mark of
  its windows.

  As in much of this layer, the loading and saving routines return
  <cpp|true> on <em|failure>.

  <paragraph|Inclusions and style trees.><cpp|load_inclusion (name)>
  loads and caches documents included with <markup|include>;
  <cpp|reset_inclusions> empties the cache. <cpp|load_style_tree
  (package)> loads and caches the body of a style package, and
  <cpp|with_package_definitions (package, body)> wraps a body in a
  <markup|with> which sets all the variables assigned by the package; it
  implements the <markup|with-package> primitive
  (<verbatim|Style/Evaluate/evaluate_rewrite.cpp>,
  <verbatim|Typeset/Env/env_exec.cpp>).

  <section|Views>

  <\explain>
    <cpp|class tm_view_rep><explain-synopsis|an editor on a buffer>
  <|explain>
    Declared in <verbatim|Texmacs/tm_window.hpp>; <cpp|tm_view> is a plain
    pointer to it. Its fields are:

    <\description>
      <item*|<cpp|tm_buffer buf>>The buffer.

      <item*|<cpp|editor ed>>The editor (a reference counted handle).

      <item*|<cpp|tm_window win>>The window which displays the view, or
      <cpp|NULL> for a <em|passive> view.

      <item*|<cpp|int nr>>A number which distinguishes the views on the
      same buffer. It is allocated by <cpp|new_view_number> from the
      static table <cpp|view_number_table>, indexed by buffer name, and is
      never reused, not even after the buffer has been closed and opened
      again.
    </description>
  </explain>

  <paragraph|The current view.>The static pointer <cpp|the_view> in
  <verbatim|new_view.cpp> is the current view. <cpp|set_current_view>
  also sets the global <abbr|DRD> <cpp|the_drd> to that of the editor and
  updates <cpp|last_visit>. <cpp|has_current_view>,
  <cpp|get_current_view> (asserts), <cpp|get_current_view_safe> and
  <cpp|get_current_editor> read it. <cpp|set_current_drd (name)> only
  switches the <abbr|DRD> to that of a buffer, without changing the
  current view; since it uses <cpp|get_passive_view>, it may load the
  buffer and create a view as a side effect.

  <paragraph|The view history.>The array <cpp|view_history> lists the
  <abbr|URL>s of all views which have once been attached to a window,
  most recently attached first; <cpp|notify_set_view> (called by
  <cpp|attach_view>) moves a view to the front and
  <cpp|notify_delete_view> (called by <cpp|delete_view>) removes it.
  Detaching a view does not remove it from the history. Renaming a buffer
  is an exception to the rule: <cpp|notify_rename_after> puts <em|all>
  views of the renamed buffer at the front, including views which have
  never been attached. It is the list returned
  by <cpp|get_all_views> and used for all \Pmost recent\Q queries. Note
  that a view which has never been attached to a window (for instance the
  passive view created to load a buffer in the background) is normally <em|not>
  in the history, so <cpp|get_all_views> does not return all views; use
  <cpp|buffer_to_views> to enumerate the views of a buffer.

  <paragraph|Queries.><cpp|buffer_to_views>, <cpp|view_to_buffer>,
  <cpp|view_to_window>, <cpp|view_to_editor>, and the general
  <cpp|get_recent_view (name, same, other, active, passive)>, which
  returns the first view of the history passing the given filters (on
  the same buffer, on another buffer, attached, not attached). The one
  argument <cpp|get_recent_view (name)> creates a new view if the buffer
  has none; otherwise it prefers the current view, then the most recent
  attached view on the buffer, then the most recent view on the buffer in
  the history, and finally the first view of the buffer.

  <paragraph|Creation and destruction.><cpp|get_new_view (name)> creates
  the buffer if needed, creates an editor with <cpp|new_editor>, registers
  the view in the buffer, passes the document data to the editor and runs
  the buffer initialization files <verbatim|init-buffer.scm> and
  <verbatim|my-init-buffer.scm> with the new view temporarily current.
  <cpp|get_passive_view (name)> returns a view which is not attached to a
  window, loading the buffer and creating a view if needed.
  <cpp|delete_view> removes the view from its buffer and from the history,
  clears the editor's buffer pointer and deletes the view.

  <paragraph|Attaching views to windows.><cpp|attach_view (win, view)> and
  <cpp|detach_view (view)> connect a view to a window and disconnect it again. <cpp|attach_view>
  sets <cpp|vw-\<gtr\>win>, installs the editor as the scrollable canvas of
  the window, sets <cpp|ed-\<gtr\>cvw>, resumes the editor, updates the
  window title and records the view in the history; <cpp|detach_view>
  clears <cpp|vw-\<gtr\>win>, suspends the editor, installs an empty glue
  widget and resets the title. <cpp|window_set_view (win, view, focus)>
  replaces the view of a window; it does nothing if the view is already
  shown there, asserts that the new view is not attached to another
  window, and makes the new view current only if <cpp|focus> is set or
  the old view was current. <cpp|switch_to_buffer>,
  <cpp|focus_on_editor>, <cpp|focus_on_buffer> and
  <cpp|var_focus_on_buffer> are described in <hlink|the current view and
  the focus|server.en.tm>.

  <section|Windows>

  <\explain>
    <cpp|class tm_window_rep><explain-synopsis|a <TeXmacs> window>
  <|explain>
    Declared in <verbatim|Texmacs/tm_window.hpp>, implemented in
    <verbatim|Texmacs/Window/tm_window.cpp>. Its public fields are:

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

    The destructor releases the identifier. The methods are described in
    <hlink|windows, menus, dialogs and embedded
    widgets|server-layer-windows.en.tm>.
  </explain>

  The routines of <verbatim|new_window.cpp> manage the windows:

  <\description>
    <item*|Identifiers><cpp|create_window_id> and
    <cpp|destroy_window_id> maintain the list <cpp|all_windows> returned
    by <cpp|windows_list>. <cpp|get_nr_windows> returns the number of
    top level windows as counted by the GUI back-end (<cpp|nr_windows>).
    Under <name|Qt> it is maintained by <cpp|qt_window_widget_rep> for all
    its non \Pfake\Q windows, which includes dialog windows; under X11 by
    <verbatim|x_window.cpp>; in the <name|Cocoa> port it stays 0. In no
    case is it the length of <cpp|windows_list>.

    <item*|Creation><cpp|new_window (map_flag, geom)> (not declared in any
    header) builds
    the <TeXmacs> widget with the bars enabled in the preferences, creates
    the <cpp|tm_window_rep>, registers it in <cpp|tm_window_table> and maps
    it. The <cpp|quit> command passed to the widget is a
    <cpp|kill_window_command_rep>, which holds a pointer to an
    <abbr|URL> that is filled in only once the identifier is known, and
    schedules <scm|(safely-kill-window <scm-arg|id>)>.

    <item*|Destruction><cpp|delete_window> (not declared in any
    header) detaches the view
    of the window (the view is kept, see above), unmaps the window,
    removes it from the table and destroys the widget.

    <item*|Current window><cpp|has_current_window>,
    <cpp|get_current_window> (returns the empty <abbr|URL> if there is
    none), <cpp|concrete_window ()>. There is no stored current window: it
    is always the window of the current view.

    <item*|Queries><cpp|buffer_to_windows>, <cpp|window_to_buffer>,
    <cpp|window_to_view> (a search through the view history).

    <item*|Commands><cpp|window_set_buffer>, <cpp|window_focus>,
    <cpp|switch_to_window>, <cpp|create_buffer ()>, <cpp|open_window>,
    <cpp|clone_window>, <cpp|new_buffer_in_new_window>,
    <cpp|new_buffer_in_this_window>, <cpp|kill_buffer>,
    <cpp|kill_window>, <cpp|kill_current_window_and_buffer>; see
    <hlink|life cycle of buffers, views and windows|server.en.tm>.
  </description>

  <section|Projects>

  A <em|project> is a master document (typically a book) whose chapters
  are separate files. The chapters share the references and the auxiliary
  data of the master. Projects are implemented in
  <verbatim|Texmacs/Data/new_project.cpp>:

  <\description>
    <item*|<cpp|project_attach (prj_name)>>Sets the <cpp|project> field of
    the data of the current buffer (an empty name detaches it),
    re-initializes and redecorates all its editors, marks the buffer as
    modified and loads the project buffer into <cpp|prj>.

    <item*|<cpp|project_attached ()>>True if the current buffer belongs to
    a project or <em|is> one.

    <item*|<cpp|project_get ()>>The name of the project of the current
    buffer: the buffer itself for an implicit project, otherwise the
    project name resolved relative to the directory of the buffer. The
    project buffer is not loaded by this call.
  </description>

  A buffer is an <em|implicit project> (<cpp|is_implicit_project>, file
  local) if its suffix is <verbatim|tp>, or if the
  <verbatim|project-flag> initial variable is set to <verbatim|true>:
  the first editor in which it is explicitly <verbatim|true> or
  <verbatim|false> decides, and otherwise the data of the buffer. The
  project buffer is loaded in the background, without any view, when a
  chapter is loaded or attached, so that its references are available
  (since it has no view, <cpp|buffer_modified> is always false for it).
  <cpp|texmacs_output_widget> also loads a project, given by the
  <verbatim|project> attribute of the document it renders, to render
  pieces of a project with the right references.

  <section|Pitfalls>

  <\itemize>
    <item>The pointers <cpp|tm_buffer>, <cpp|tm_view> and <cpp|tm_window>
    are not reference counted. Do not keep them across calls which may
    close buffers or windows; keep the <abbr|URL> instead and convert it
    again when needed.

    <item><cpp|get_buffer_tree> (<scm|buffer-get>) returns the style and
    initial environment stored in the buffer, which are only synchronized
    with the editors when the buffer is loaded, set or exported. After
    the user has changed the style or a document setting, the result may
    therefore be out of date; the body, on the other hand, is always
    current. It also leaves out the references and the auxiliary data.

    <item><cpp|get_all_views> only returns views which have been attached
    to a window at least once (or belong to a renamed buffer).

    <item>Because of the bug in <cpp|remove_buffer> described above, do not
    assume that all views of a closed buffer are gone.

    <item>Several routines (<cpp|get_current_buffer>,
    <cpp|get_current_view>, the one argument <cpp|get_recent_view>,
    <cpp|import_tree> for a name which cannot be resolved
    directly) assert that there is a current
    view; use the <cpp|_safe> variants in code which may run without one.

    <item><cpp|view_to_editor> on an invalid view <abbr|URL> removes the
    <abbr|URL> from the history and returns a nil editor (or fails in
    <verbatim|ADVANCED_DEVELOPER_MODE>); callers in this layer
    assume that the result is valid.
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
