<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The server: buffers, views and windows>

  This chapter describes the part of <TeXmacs> which sits between the
  graphical toolkit, the <scheme> interpreter and the typesetter: the
  <em|server>, a singleton which owns the open documents and the windows,
  and the <em|editors>, which implement the interactive behaviour of the
  program. It explains how documents are organized into buffers, views and
  windows, how these objects are created and destroyed, how user events
  reach the editor, how modifications of a document are propagated (to the
  typesetter, to the other views on the same document and to the undo
  system), and how screen updates are scheduled.

  <section|Buffers, views and windows in a nutshell>

  <\description>
    <item*|Buffer>An open document, identified by its name (a file name,
    a scratch name for new documents, or a <verbatim|tmfs://> <abbr|URL>
    for generated documents). The body of every buffer is a child of one
    global tree, <cpp|the_et>. A buffer may be shown in any number of
    windows, including none.

    <item*|View>An editor on a buffer: cursor, selection, typeset boxes,
    zoom and undo history. A buffer may have several views; all of them
    share the document and see each other's changes immediately. A view is
    shown in at most one window; a view in no window is <em|passive>.

    <item*|Window>A top level window with menus, toolbars, a canvas and a
    footer; it shows exactly one view at a time.

    <item*|Current view>At any moment one view is current. It determines
    the current buffer, the current window (if the view is shown in one),
    the current editor and the current <abbr|DRD>, and it is the implicit
    argument of all editing commands and of all routines which concern
    \Pthe\Q window.
  </description>

  A few invariants explain much of the code:

  <\itemize>
    <item>Every buffer which is being edited has at least one view, even
    after its last window has been closed: the \Pmodified\Q status of a
    buffer is computed from the undo histories of the editors of its
    views.

    <item>Documents are only modified through the elementary operations
    on <cpp|the_et> (<cpp|assign>, <cpp|insert>, ...), whose observers
    notify every editor on the buffer and every undo history.

    <item>Editors never redraw directly: they record what changed, and the
    <em|interpose handler> of the server retypesets and invalidates the
    views shown in windows between two rounds of event processing.

    <item>Outside the files which implement them, buffers, views and
    windows are referred to by <abbr|URL>s, never by pointers.
  </itemize>

  <section|The objects and their connections>

  The following diagram shows the main objects and the pointers between
  them. Arrows mean \Pholds a pointer or handle to\Q; the names in
  brackets are the fields.

  <\verbatim-code>
    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ server (handle, singleton the_server)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ tm_server_rep

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ /\ \ \ \ \ \ \ \ \ \ \ \\

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ tm_config_rep\ \ \ \ \ tm_frame_rep\ \ \ (both : virtual server_rep)

    \;

    bufs : array\<less\>tm_buffer\<gtr\>

    \ \ \|

    \ \ tm_buffer_rep ---[buf]--\<gtr\> new_buffer_rep\ \ \ (name, master, title, ...)

    \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ ---[data]-\<gtr\> new_data_rep\ \ \ \ (style, init, ref, aux, ...)

    \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ ---[prj]--\<gtr\> tm_buffer_rep\ \ \ (the project, if any)

    \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ ---[rp]---\<gtr\> path of the body in the_et

    \ \ \ \ [vws]

    \ \ \ \ \ \ \|

    \ \ tm_view_rep ---[ed]---\<gtr\> editor (edit_interface_rep ...)

    \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|-[sv]--\<gtr\> server_rep

    \ \ \ \ [win]\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|-[buf]-\<gtr\> tm_buffer_rep

    \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|-[cvw]-\<gtr\> canvas widget of the window

    \ \ tm_window_rep ---[wid]--\<gtr\> texmacs_widget (menus, canvas, footer)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ---[win]--\<gtr\> plain_window_widget (the top level window)
  </verbatim-code>

  <section|Ownership and identifiers>

  The metadata objects are plain <c++> objects allocated with
  <cpp|tm_new> and deleted with <cpp|tm_delete>; they are <em|not>
  reference counted. Ownership is as follows:

  <\description>
    <item*|Buffers>are owned by the global array <cpp|bufs>
    (<source-link|new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>). They are created by <cpp|insert_buffer>
    and destroyed by <cpp|remove_buffer>.

    <item*|Views>are owned by their buffer (the array
    <cpp|tm_buffer_rep::vws>). They are created by <cpp|get_new_view> and
    destroyed by <cpp|delete_view>, or implicitly when their buffer is
    removed.

    <item*|Windows>are owned by the static table <cpp|tm_window_table>
    (<source-link|new_window.cpp|src/Texmacs/Data/new_window.cpp>), which maps window identifiers to
    <cpp|tm_window_rep*>. They are created by <cpp|new_window> and
    destroyed by <cpp|delete_window>. The windows of embedded <TeXmacs>
    widgets are the exception: they are not in this table and are owned by
    the widget (see <hlink|embedded widgets|server-windows.en.tm>).
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

  <section|Navigating between the objects>

  The following functions (declared in <source-link|Data/new_buffer.hpp|src/Texmacs/Data/new_buffer.hpp>,
  <source-link|Data/new_view.hpp|src/Texmacs/Data/new_view.hpp> and <source-link|Data/new_window.hpp|src/Texmacs/Data/new_window.hpp>) navigate
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

  <section|Source files>

  All file names are relative to <verbatim|src/src/> unless stated
  otherwise. The directory <verbatim|Texmacs/> contains the server proper:

  <\description-paragraphs>
    <item*|<source-link|Texmacs/server.hpp|src/Texmacs/server.hpp>>The abstract class
    <cpp|server_rep>, the handle <cpp|server>, <cpp|get_server> and a few
    global declarations. It also includes the headers of
    <verbatim|Texmacs/Data/>, so that including <source-link|server.hpp|src/Texmacs/server.hpp>
    gives access to the whole <abbr|URL> based buffer, view and window
    interface.

    <item*|<source-link|Texmacs/tm_server.hpp|src/Texmacs/tm_server.hpp>,
    <source-link|Texmacs/Server/tm_server.cpp|src/Texmacs/Server/tm_server.cpp>>The concrete server
    <cpp|tm_server_rep>, its constructor (which boots <scheme>), the
    interpose and wait handlers, printing settings, global typesetting
    invalidation and <cpp|quit>.

    <item*|<source-link|Texmacs/tm_config.hpp|src/Texmacs/tm_config.hpp>,
    <source-link|Texmacs/Server/tm_config.cpp|src/Texmacs/Server/tm_config.cpp>>The partial server
    <cpp|tm_config_rep>: font rules and the keyboard configuration.

    <item*|<source-link|Texmacs/tm_frame.hpp|src/Texmacs/tm_frame.hpp>,
    <source-link|Texmacs/Window/tm_frame.cpp|src/Texmacs/Window/tm_frame.cpp>, <source-link|Texmacs/Window/tm_dialogue.cpp|src/Texmacs/Window/tm_dialogue.cpp>>The
    partial server <cpp|tm_frame_rep>: properties, menus, toolbars, canvas,
    footer and full screen mode of the current window; dialog windows, file
    choosers and interactive commands.

    <item*|<source-link|Texmacs/tm_buffer.hpp|src/Texmacs/tm_buffer.hpp>>The class
    <cpp|tm_buffer_rep>.

    <item*|<source-link|Texmacs/tm_window.hpp|src/Texmacs/tm_window.hpp>,
    <source-link|Texmacs/Window/tm_window.cpp|src/Texmacs/Window/tm_window.cpp>>The classes <cpp|tm_window_rep> and
    <cpp|tm_view_rep> (whose constructor is in
    <source-link|Texmacs/Data/new_view.cpp|src/Texmacs/Data/new_view.cpp>); window geometry, embedded <TeXmacs> widgets, menu
    caching, interactive input in the footer, and the \Palternative\Q top
    level windows used for <scheme> dialogs.

    <item*|<source-link|Texmacs/tm_data.hpp|src/Texmacs/tm_data.hpp>>The global array <cpp|bufs> and a
    convenience <cpp|set_message>; included by all files of
    <verbatim|Texmacs/Data/>.

    <item*|<source-link|Texmacs/Data/new_buffer.hpp|src/Texmacs/Data/new_buffer.hpp>,
    <source-link|new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>>The class <cpp|new_buffer_rep> and all buffer
    level routines: list of buffers, names and titles, contents, save
    status, loading, saving and inclusions.

    <item*|<source-link|Texmacs/Data/new_view.hpp|src/Texmacs/Data/new_view.hpp>,
    <source-link|new_view.cpp|src/Texmacs/Data/new_view.cpp>>View identifiers, the current view, the view
    history, creation and destruction of views, attaching views to windows,
    and focus changes.

    <item*|<source-link|Texmacs/Data/new_window.hpp|src/Texmacs/Data/new_window.hpp>,
    <source-link|new_window.cpp|src/Texmacs/Data/new_window.cpp>>Window identifiers, creation and destruction
    of windows, and the high level commands which open, clone and close
    windows and buffers.

    <item*|<source-link|Texmacs/Data/new_project.hpp|src/Texmacs/Data/new_project.hpp>,
    <source-link|new_project.cpp|src/Texmacs/Data/new_project.cpp>>Projects.

    <item*|<source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp>>Widgets which display a
    typeset box (<cpp|box_widget>, <cpp|texmacs_output_widget>) and the
    computation of the size of a typeset document (<cpp|tree_extents>).

    <item*|<source-link|Texmacs/Server/tm_debug.cpp|src/Texmacs/Server/tm_debug.cpp>>System and editor status reports,
    crash reports and the fatal error handler <cpp|tm_failure>.

    <item*|<source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>>The main program:
    <cpp|texmacs_entrypoint>, <cpp|TeXmacs_main>, the command line options
    and the startup preferences.
  </description-paragraphs>

  Two closely related files live elsewhere:
  <source-link|Data/Document/new_data.hpp|src/Data/Document/new_data.hpp> (the class <cpp|new_data_rep>,
  with <cpp|attach_data> and <cpp|detach_data>) and
  <source-link|Data/Document/new_document.cpp|src/Data/Document/new_document.cpp> (the global edit tree
  <cpp|the_et>). The <scheme> glue which exports the server is declared in
  <source-link|Scheme/Glue/build-glue-server.scm|src/Scheme/Glue/build-glue-server.scm> and, for buffers, views and
  windows, in <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>.

  The editor is in <verbatim|Edit/>: the abstract class <cpp|editor_rep>
  in <source-link|Edit/editor.hpp|src/Edit/editor.hpp> and its implementation <cpp|edit_main_rep>
  in <source-link|Edit/Editor/edit_main.hpp|src/Edit/Editor/edit_main.hpp>, assembled from the classes in
  <verbatim|Edit/Interface> (events, cursor, repainting, footer),
  <verbatim|Edit/Modify> (modifications and undo), <verbatim|Edit/Replace>
  (selections, search, spell checking) and <verbatim|Edit/Process>. The
  observers attached to the edit tree are in <verbatim|Data/Observers>,
  the undo history in <verbatim|Data/History>, and the generic observer
  mechanism in <source-link|Kernel/Abstractions/observer.cpp|src/Kernel/Abstractions/observer.cpp>.

  On the <scheme> side, the most relevant files (relative to
  <verbatim|src/TeXmacs/progs/>) are <source-link|kernel/library/base.scm|TeXmacs/progs/kernel/library/base.scm>,
  <source-link|kernel/gui/kbd-handlers.scm|TeXmacs/progs/kernel/gui/kbd-handlers.scm>, <source-link|kernel/gui/kbd-define.scm|TeXmacs/progs/kernel/gui/kbd-define.scm>,
  <source-link|kernel/texmacs/tm-preferences.scm|TeXmacs/progs/kernel/texmacs/tm-preferences.scm>,
  <source-link|kernel/texmacs/tm-file-system.scm|TeXmacs/progs/kernel/texmacs/tm-file-system.scm>,
  <source-link|utils/library/cursor.scm|TeXmacs/progs/utils/library/cursor.scm>,
  <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> and
  <source-link|texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>.

  <section|Contents of this chapter>

  The pages are ordered from the static structure to the dynamic
  behaviour. Readers in a hurry may start with the walk-throughs, which
  follow common operations through all the layers.

  <\traverse>
    <branch|The server classes and their connections|server-classes.en.tm>

    <branch|The main program and crash handling|server-startup.en.tm>

    <branch|Buffers|server-buffers.en.tm>

    <branch|Views and the current view|server-views.en.tm>

    <branch|Windows|server-windows.en.tm>

    <branch|The editor: classes, state, modifications and
    undo|server-editor.en.tm>

    <branch|The event loop, keyboard and mouse, and repaint
    scheduling|server-events.en.tm>

    <branch|The <scheme> interface to buffers, views and
    windows|server-scheme.en.tm>

    <branch|Walk-throughs and guidelines|server-howto.en.tm>
  </traverse>

  Neighbouring subjects are described elsewhere: the typesetting of
  documents into boxes in <hlink|the typesetting
  algorithm|typesetter.en.tm> and <hlink|the boxes produced by the
  typesetter|boxes.en.tm>; the editing operations in <hlink|structured
  editing, search and automatic content|editing.en.tm>; the widgets which
  make up menus, toolbars and dialogs in <hlink|the abstract widget
  system|widgets.en.tm> and their toolkit implementations in <hlink|the
  graphical user interface ports|guiports.en.tm>; and the <scheme>
  programming interface in <hlink|the <scheme> buffer
  API|../scheme/buffer/scheme-buffer.en.tm>.

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
