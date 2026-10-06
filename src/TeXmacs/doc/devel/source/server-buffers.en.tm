<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Buffers>

  A <em|buffer> is an open document. It exists independently of any
  window: a buffer may be shown in several windows, in one, or in none at
  all (help pages loaded in the background, the project file of a book,
  the auxiliary documents built by <scheme>). This page describes how a
  buffer is represented, how it is named, which routines of
  <source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp> manipulate it, and how buffers are
  created, loaded, saved, renamed and closed, both at the <c++> level and
  by the user level commands written in <scheme>.

  A buffer is described by three objects: the <cpp|tm_buffer_rep> which
  ties everything together, the file related information
  <cpp|new_buffer_rep>, and the document data <cpp|new_data_rep>. The body
  of the document is stored in none of them, but in the global edit tree.
  The \Pmodified\Q status is not stored at all: it is derived from the
  undo histories of the editors of the buffer.

  <\verbatim-code>
    tm_buffer_rep

    \ \ \|- buf \ : new_buffer \ \ name, master, title, last_save, ...

    \ \ \|- data : new_data \ \ \ \ style, init, fin, ref, aux, att, project

    \ \ \|- rp \ \ : path \ \ \ \ \ \ \ \ \ ---\<gtr\> the_et[rp] = body of the document

    \ \ \|- vws \ : views \ \ \ \ \ \ \ \ ---\<gtr\> each with an editor (cursor, undo, ...)

    \ \ \|- prj \ : tm_buffer \ \ \ \ ---\<gtr\> the project (master document), if any

    \ \ \|- lns, notify \ \ \ \ \ \ \ \ \ \ \ <scheme> notifier (shared buffers)
  </verbatim-code>

  <section|The global edit tree>

  All document bodies are children of the global tree <cpp|the_et>
  (<source-link|Data/Document/new_document.cpp|src/Data/Document/new_document.cpp>), a <markup|tuple> created at
  startup in <cpp|texmacs_entrypoint>. The three functions of
  <source-link|new_document.hpp|src/Data/Document/new_document.hpp> manage its slots:

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

  Keeping all documents in one tree has two important consequences. First,
  any subtree of any open document can be designated by an absolute path,
  and the root <cpp|ip_observer> which is attached to <cpp|the_et> at
  startup allows to recover this path from the tree itself
  (<cpp|obtain_ip>); <cpp|path_to_buffer (p)> then finds the buffer whose
  root path is a prefix of <cpp|p>. Second, all modifications go through
  the same functions (such as <cpp|assign (path, tree)>), which forward
  them to the observers attached to the modified subtrees; this is how a
  change made in one view reaches the typesetters of all other views and
  the undo histories (see <hlink|the modification
  pipeline|server-editor.en.tm>).

  <section|Buffer names>

  A buffer is <em|identified by its name>, which is a <abbr|URL>: there is
  no separate naming scheme for buffers, and <cpp|concrete_buffer (url)>
  simply searches the global array <cpp|bufs> for a buffer with the given
  name. The name is typically

  <\itemize>
    <item>the file name of the document on disk or on the web;

    <item>a scratch <abbr|URL> produced by <cpp|make_new_buffer> for new
    documents (<cpp|url_scratch ("no_name_", ".tm", i)>, with the first
    free <math|i>); such buffers are recognized by <cpp|buffer_has_name>,
    which returns false for them;

    <item>a <verbatim|tmfs://> <abbr|URL> for documents that are generated
    by <scheme> handlers of the <TeXmacs> file system: help pages,
    auxiliary documents <verbatim|tmfs://aux/...>, embedded input fields
    <verbatim|tmfs://aux/TeXmacs-input-<em|n>>, document parts
    <verbatim|tmfs://part/...>, and so on. See <hlink|the <TeXmacs> file
    system|../scheme/api/tmfs/tmfs.en.tm>.
  </itemize>

  Besides its name, a buffer has a <em|master> <abbr|URL>, with respect to
  which relative links and file names are resolved, and which is used for
  navigation. For ordinary documents the master is the name itself. A
  buffer whose master differs from its name is called <em|auxiliary>
  (<cpp|is_aux_buffer>, <scm|buffer-aux?>): generated bibliographies, help
  pages opened from a document, the documents behind embedded widgets.
  Auxiliary buffers cannot be saved under their own name, but behave as if
  they were located at their master. The <scheme> routine
  <scm|open-auxiliary> in <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> creates
  such a buffer from a tree and a master (via <scm|aux-set-document> and
  <scm|aux-set-master> of <source-link|kernel/texmacs/tm-file-system.scm|TeXmacs/progs/kernel/texmacs/tm-file-system.scm>),
  and <scm|load-buffer-open> sets the master of a <verbatim|tmfs://> buffer
  to the one proposed by its handler (<scm|tmfs-master>).

  The <em|title> of a buffer, shown in window titles and in the <menu|Go>
  menu, is computed by <cpp|propose_title>: the last component of the file
  name, \PNo name [<em|i>]\Q for scratch buffers, the title returned by the
  <scheme> function <scm|tmfs-title> for <verbatim|tmfs://> buffers, and a
  suffix \P(2)\Q, \P(3)\Q, ... if another buffer already has the same
  title.

  Since the name is the identifier, renaming a buffer (<cpp|rename_buffer>,
  used by <menu|Save as>) changes the identity of the buffer and of all its
  views: see <hlink|view identifiers|server-views.en.tm>.

  <section|The buffer classes>

  <subsection|The class <cpp|new_buffer_rep>>

  <\explain>
    <cpp|class new_buffer_rep><explain-synopsis|file related information>
  <|explain>
    Declared in <source-link|Texmacs/Data/new_buffer.hpp|src/Texmacs/Data/new_buffer.hpp>; a
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

  <subsection|The class <cpp|new_data_rep>><label|new-data>

  <\explain>
    <cpp|class new_data_rep><explain-synopsis|everything but the body>
  <|explain>
    Declared in <source-link|Data/Document/new_data.hpp|src/Data/Document/new_data.hpp>; a
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
    Declared in <source-link|Texmacs/tm_buffer.hpp|src/Texmacs/tm_buffer.hpp>; <cpp|tm_buffer> is a
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
      <scm|buffer-initialize> (defined in <source-link|part/part-shared.scm|TeXmacs/progs/part/part-shared.scm>)
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
      redo|server-editor.en.tm>); this is why a buffer must always keep at least
      one view.
    </description>
  </explain>

  <section|Reference of the buffer routines>

  Most routines of <source-link|new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp> take a buffer name and do
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
  <cpp|rename_buffer> is described below.

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
  (<source-link|Style/Evaluate/evaluate_rewrite.cpp|src/Style/Evaluate/evaluate_rewrite.cpp>,
  <source-link|Typeset/Env/env_exec.cpp|src/Typeset/Env/env_exec.cpp>).

  <section|Life cycle of a buffer>

  The <c++> routines above are deliberately simple; the policy (which
  questions are asked, what happens to autosave files, where the buffer is
  shown) lives in <scheme>, in <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>
  (loading and saving) and <source-link|texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>
  (closing). The following subsections follow a buffer through its life.

  <subsection|Creation>

  All buffers are created by <cpp|set_buffer_tree (name, doc)>: if no
  buffer with the given name exists, the file local <cpp|insert_buffer>
  allocates a new <cpp|tm_buffer_rep> (whose constructor obtains a slot in
  <cpp|the_et> through <cpp|new_document>) and appends it to <cpp|bufs>; the
  document is split with <cpp|detach_data>, its body is stored with
  <cpp|set_document> and a title is proposed. The variants are

  <\description>
    <item*|<cpp|create_buffer (name, doc)>>Only creates the buffer if it
    does not exist yet.

    <item*|<cpp|make_new_buffer ()>>Creates an empty scratch buffer
    (<scm|buffer-new>).

    <item*|<cpp|set_buffer_body (name, body)>>Creates the buffer with
    default data if needed (<scm|buffer-set-body>).

    <item*|<cpp|get_new_view (name)>, <cpp|get_recent_view (name)>>Create
    an empty buffer as a side effect if there is none; see <hlink|views and
    the current view|server-views.en.tm>.
  </description>

  A freshly created buffer has <em|no view>, and hence no editor: it is
  not typeset, its \Pmodified\Q status is always false, and the
  routines which act on the current editor cannot be applied to it. The
  first view is created when the buffer is displayed, or explicitly with
  <scm|view-new> or <scm|view-passive> (the <scheme> macro
  <scm|with-buffer> silently does nothing on a buffer without views).

  <subsection|Loading>

  The <c++> part of loading is <cpp|buffer_load (name)>, which determines
  the format with <cpp|file_format> and calls <cpp|buffer_import (name,
  name, fm)>; the latter reads and converts the file with
  <cpp|import_tree> and passes the result to <cpp|set_buffer_tree>. Note
  the convention of these low level routines: they return <cpp|true> on
  <em|failure>.

  The user command <scm|load-buffer> (<menu|File|Load>, files on the
  command line, hyperlinks) goes through a chain of <scheme> functions,
  each of which either stops with a message or calls the next one:

  <\enumerate>
    <item><scm|load-buffer-main> resolves the name: relative to
    <verbatim|$TEXMACS_FILE_PATH> if the file only exists there, then
    relative to the current buffer (or to the working directory if there
    is none).

    <item><scm|load-buffer-check-autosave> proposes to load a more recent
    autosave file, or to rescue the file after a crash (unless the option
    <scm|:strict> is given). If the user accepts, the autosave file is
    loaded with <scm|buffer-set> and the buffer is marked as modified.

    <item><scm|load-buffer-check-permissions> checks that the file can be
    read, or created.

    <item><scm|load-buffer-load> does nothing if the buffer is already
    open, calls <scm|buffer-load> if the file exists, and otherwise
    creates an empty document with the default style.

    <item><scm|load-buffer-open> displays the buffer: not at all with the
    option <scm|:background>, in a new window with <scm|:new-window>
    (<scm|open-buffer-in-window>, that is,
    <cpp|new_buffer_in_new_window>), and otherwise in the current window
    with <scm|switch-to-buffer>. It then records the file in the list of
    recent files, asks for the passphrase of an encrypted document, and
    sets the master of <verbatim|tmfs://> buffers.
  </enumerate>

  <scm|load-buffer-in-new-window> adds <scm|:new-window>, but does nothing
  if the buffer is already shown in some window. <scm|revert-buffer>
  re-imports the file and replaces the contents with <scm|buffer-set>; as
  for any existing buffer, <cpp|set_buffer_tree> then uses <cpp|assign>, so
  that all views are updated through the modification pipeline, and passes
  the new document data to all editors (<cpp|set_data> and
  <cpp|init_update>).

  <subsection|Saving and exporting>

  <cpp|buffer_save (name)> exports the buffer to its own name in the format
  given by its suffix, marks it as saved with <cpp|pretend_buffer_saved>
  (which calls <cpp|notify_save> on the editors and records the time stamp
  of the file) and clears the \Pmodified\Q mark of all windows showing it.
  <cpp|buffer_export (name, dest, fm)> does the real work. It needs an
  editor, so it takes the most recent view on the buffer (creating one if
  necessary), retrieves the body from <cpp|the_et>, applies the editor
  based conversions for the formats which need the typesetter
  (<cpp|exec_verbatim>, <cpp|exec_html>, or <cpp|print_to_file> for
  PostScript and PDF), copies the document data back from the editor
  (<cpp|get_data>, see <hlink|the class <cpp|new_data_rep>|#new-data>),
  attaches them, adds the link locations and writes the result with
  <cpp|export_tree>. Like the loading routines, these functions return
  <cpp|true> on failure.

  The user command <scm|save-buffer> calls <scm|save-buffer-main>, then

  <\enumerate>
    <item><scm|save-buffer-check-permissions>, which asks for a file name
    for scratch buffers, refuses to save non existing, unmodified or
    unwritable buffers and warns when the file changed on disk since
    <scm|buffer-last-save>;

    <item><scm|save-buffer-check-faithful>, which asks for confirmation
    when the target format is not a faithful <TeXmacs> format;

    <item><scm|save-buffer-save>, which finally calls <scm|buffer-save>.
  </enumerate>

  <scm|save-buffer-as> renames the buffer with <scm|buffer-rename> before
  saving it. Autosaving is implemented in the same file
  (<scm|autosave-buffer>, <scm|autosave-all>, <scm|autosave-propose>).

  <subsection|Renaming>

  <cpp|rename_buffer (name, new_name)> first kills any buffer which already
  has the new name, then changes <cpp|name> and <cpp|master>, notifies the
  editors with <cpp|THE_ENVIRONMENT> (relative links must be resolved
  again), updates the view history (view identifiers contain the buffer
  name; see <cpp|notify_rename_before> and <cpp|notify_rename_after>) and
  proposes a new title.

  <subsection|Closing>

  <cpp|kill_buffer (name)> (<scm|cpp-buffer-close>) first gives every
  window which displays the buffer something else to show: the most recent
  passive view on another buffer or, failing that, a new view on the
  buffer of the most recent view on another buffer. If there is no other
  buffer at all, the window keeps its view. It then calls
  <cpp|remove_buffer>, which deletes the views of the buffer, removes it
  from <cpp|bufs> and deletes the <cpp|tm_buffer_rep>; its destructor frees
  the slot in <cpp|the_et> with <cpp|delete_document>. If the last buffer
  is removed and <TeXmacs> does not act as a server for remote clients
  (<cpp|number_of_servers ()>), the program quits.

  At the user level, <scm|safely-kill-buffer> asks for confirmation if the buffer is modified and then calls
  <scm|buffer-close>; for an embedded buffer it deletes the alternative
  windows which contain it instead (see <hlink|embedded
  widgets|server-windows.en.tm>). Closing a <em|window> also closes its
  buffer (<hlink|closing windows|server-windows.en.tm>), and
  <scm|close-document> chooses between the two according to the
  <verbatim|buffer management> preference.

  <section|Projects>

  A <em|project> is a master document (typically a book) whose chapters
  are separate files. The chapters share the references and the auxiliary
  data of the master. Projects are implemented in
  <source-link|Texmacs/Data/new_project.cpp|src/Texmacs/Data/new_project.cpp>:

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
    <item>Do not keep <cpp|tm_buffer> pointers across calls which may close
    buffers; keep the name and call <cpp|concrete_buffer> again.

    <item>A buffer without views has no editor: it is not typeset, it is
    never reported as modified, its modifications are not recorded in any
    undo history, and <scm|with-buffer> does not execute its body on it.
    Create a view (<scm|view-passive>) first if any of this matters.

    <item><cpp|get_buffer_tree> (<scm|buffer-get>) returns the style and
    initial environment stored in the buffer, which are only synchronized
    with the editors when the buffer is loaded, set or exported. After
    the user has changed the style or a document setting, the result may
    therefore be out of date; the body, on the other hand, is always
    current. It also leaves out the references and the auxiliary data.

    <item><cpp|buffer_export> and <cpp|buffer_save> on a name which is not
    a buffer create an empty buffer with that name.

    <item>Because of the bug in <cpp|remove_buffer> described above, do not
    assume that all views of a closed buffer are gone.

    <item><cpp|get_current_buffer> and <cpp|import_tree> for a name which
    cannot be resolved directly assert that there is a current view; use
    <cpp|get_current_buffer_safe> in code which may run without one.

    <item>The low level loading and saving routines return <cpp|true> on
    <em|failure>.
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
