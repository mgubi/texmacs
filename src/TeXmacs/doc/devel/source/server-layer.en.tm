<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The server layer: the directory <verbatim|Texmacs/>>

  <section|Introduction>

  The directory <verbatim|src/src/Texmacs/> is the glue between the three
  big subsystems of <TeXmacs>: the <scheme> interpreter, which implements
  most of the user interface and the editing commands; the editors
  (<verbatim|src/src/Edit/>), each of which edits and typesets one document;
  and the graphical toolkit (<verbatim|src/src/Plugins/Qt/> and friends),
  which provides the windows. The directory contains

  <\itemize>
    <item>the <em|server>, a singleton object which owns the configuration
    of the program, gives access to the current window and exports a large
    part of its services to <scheme>;

    <item>the <em|metadata classes> which describe the open documents and
    the way they are displayed: buffers, views, windows and projects;

    <item>the code which builds and manages the <TeXmacs> windows, their
    menus, footers and dialogs, and the embedded <TeXmacs> widgets;

    <item>the main program, with the command line options and the crash
    handler.
  </itemize>

  This chapter describes these classes and files one by one. The chapter
  <hlink|the server, buffers, views and windows|server.en.tm> follows the
  same objects through their life cycle and through the event loop, and
  describes the editor and the modification pipeline; it is the place to
  look for the <em|dynamic> behaviour. The present chapter is the
  <em|static> reference: which class holds which data, which file
  implements which function, and how the pieces are wired together.

  All file names below are relative to <verbatim|src/src/> unless stated
  otherwise.

  <section|Overview>

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

  Each of the three kinds of metadata objects is named by a <abbr|URL>: the
  buffer by its file name, the view by <verbatim|tmfs://view/<em|n>/...>,
  the window by <verbatim|tmfs://window/<em|n>>. The rest of the program,
  and in particular <scheme>, only manipulates these <abbr|URL>s; the raw
  pointers <cpp|tm_buffer>, <cpp|tm_view> and <cpp|tm_window> are used only
  inside <verbatim|Texmacs/> and in a few low level places in the editor.

  The <em|current view> (the global pointer <cpp|the_view> of
  <verbatim|new_view.cpp>, which no other file uses directly) determines
  the current buffer, the current window and the current editor. It is
  the implicit argument of all methods of <cpp|tm_frame_rep>, and so of
  the window related routines exported with the
  <verbatim|get_server()-\<gtr\>> prefix, and of all <scheme> routines
  exported with the <verbatim|get_current_editor()-\<gtr\>> prefix.

  <section|Source files>

  <\description>
    <item*|<verbatim|Texmacs/server.hpp>>The abstract class
    <cpp|server_rep>, the handle <cpp|server>, <cpp|get_server> and a few
    global declarations. It also includes the headers of
    <verbatim|Texmacs/Data/>, so that including <verbatim|server.hpp>
    gives access to the whole <abbr|URL> based buffer, view and window
    interface.

    <item*|<verbatim|Texmacs/tm_server.hpp>,
    <verbatim|Texmacs/Server/tm_server.cpp>>The concrete server
    <cpp|tm_server_rep>, its constructor (which boots <scheme>), the
    interpose and wait handlers, printing settings, global typesetting
    invalidation and <cpp|quit>.

    <item*|<verbatim|Texmacs/tm_config.hpp>,
    <verbatim|Texmacs/Server/tm_config.cpp>>The partial server
    <cpp|tm_config_rep>: font rules and the keyboard configuration.

    <item*|<verbatim|Texmacs/tm_frame.hpp>,
    <verbatim|Texmacs/Window/tm_frame.cpp>, <verbatim|Texmacs/Window/tm_dialogue.cpp>>The
    partial server <cpp|tm_frame_rep>: properties, menus, toolbars, canvas,
    footer and full screen mode of the current window; dialog windows, file
    choosers and interactive commands.

    <item*|<verbatim|Texmacs/tm_buffer.hpp>>The class
    <cpp|tm_buffer_rep>.

    <item*|<verbatim|Texmacs/tm_window.hpp>,
    <verbatim|Texmacs/Window/tm_window.cpp>>The classes <cpp|tm_window_rep> and
    <cpp|tm_view_rep> (whose constructor is in
    <verbatim|Texmacs/Data/new_view.cpp>); window geometry, embedded <TeXmacs> widgets, menu
    caching, interactive input in the footer, and the \Palternative\Q top
    level windows used for <scheme> dialogs.

    <item*|<verbatim|Texmacs/tm_data.hpp>>The global array <cpp|bufs> and a
    convenience <cpp|set_message>; included by all files of
    <verbatim|Texmacs/Data/>.

    <item*|<verbatim|Texmacs/Data/new_buffer.hpp>,
    <verbatim|new_buffer.cpp>>The class <cpp|new_buffer_rep> and all buffer
    level routines: list of buffers, names and titles, contents, save
    status, loading, saving and inclusions.

    <item*|<verbatim|Texmacs/Data/new_view.hpp>,
    <verbatim|new_view.cpp>>View identifiers, the current view, the view
    history, creation and destruction of views, attaching views to windows,
    and focus changes.

    <item*|<verbatim|Texmacs/Data/new_window.hpp>,
    <verbatim|new_window.cpp>>Window identifiers, creation and destruction
    of windows, and the high level commands which open, clone and close
    windows and buffers.

    <item*|<verbatim|Texmacs/Data/new_project.hpp>,
    <verbatim|new_project.cpp>>Projects.

    <item*|<verbatim|Texmacs/Window/tm_button.cpp>>Widgets which display a
    typeset box (<cpp|box_widget>, <cpp|texmacs_output_widget>) and the
    computation of the size of a typeset document (<cpp|tree_extents>).

    <item*|<verbatim|Texmacs/Server/tm_debug.cpp>>System and editor status reports,
    crash reports and the fatal error handler <cpp|tm_failure>.

    <item*|<verbatim|Texmacs/Texmacs/texmacs.cpp>>The main program:
    <cpp|texmacs_entrypoint>, <cpp|TeXmacs_main>, the command line options
    and the startup preferences.
  </description>

  Two closely related files live elsewhere:
  <verbatim|Data/Document/new_data.hpp> (the class <cpp|new_data_rep>,
  with <cpp|attach_data> and <cpp|detach_data>) and
  <verbatim|Data/Document/new_document.cpp> (the global edit tree
  <cpp|the_et>). The <scheme> glue which exports the server is declared in
  <verbatim|Scheme/Glue/build-glue-server.scm> and, for buffers, views and
  windows, in <verbatim|Scheme/Glue/build-glue-basic.scm>.

  <section|Contents of this chapter>

  <\traverse>
    <branch|The server classes and their connections|server-layer-classes.en.tm>

    <branch|Metadata: buffers, views, windows and
    projects|server-layer-metadata.en.tm>

    <branch|Windows, menus, dialogs and embedded
    widgets|server-layer-windows.en.tm>

    <branch|The main program and crash handling|server-layer-startup.en.tm>
  </traverse>

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
