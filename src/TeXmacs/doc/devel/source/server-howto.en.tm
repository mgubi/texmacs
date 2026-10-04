<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Walk-throughs and guidelines>

  The other pages of this chapter describe the objects one by one. This
  page follows a few common operations through all of them, from the user
  action to the screen, and ends with the rules which follow from the
  design. Each step names the function which performs it, so that the
  walk-throughs can also be used as a map for setting breakpoints.

  <section|Opening a file from the command line>

  Running <verbatim|texmacs a.tm> goes through the following steps.

  <\enumerate>
    <item><cpp|texmacs_entrypoint> initializes the paths, the preferences,
    the toolkit and the global edit tree <cpp|the_et>, and starts the
    <scheme> interpreter, which calls <cpp|TeXmacs_main> (see <hlink|the
    startup sequence|server-startup.en.tm>).

    <item><cpp|set_global_options> turns the file argument into the
    <scheme> command <scm|(load-buffer "/.../a.tm")> and appends it to
    <cpp|extra_init_cmd>; further files get <scm|:new-window>.

    <item>The server is constructed; its constructor loads
    <verbatim|init-texmacs.scm>, which defines all menus, keyboard
    bindings and user commands.

    <item>Since no buffer exists yet, <cpp|open_window> creates the scratch
    buffer <verbatim|no_name_1.tm> with <cpp|make_new_buffer>, a window
    with <cpp|new_window>, and a first view with <cpp|get_passive_view>
    (which calls <cpp|get_new_view>, which creates the editor and runs
    <verbatim|init-buffer.scm>); <cpp|window_set_view> attaches the view
    to the window (<cpp|attach_view>, <cpp|resume>, which installs the
    menus) and makes it current.

    <item><cpp|TeXmacs_main> schedules <cpp|extra_init_cmd> with
    <cpp|exec_delayed> and enters the event loop.

    <item>In the first round of the event loop, the delayed command runs.
    <scm|load-buffer> goes through its chain of checks (<hlink|loading|server-buffers.en.tm>);
    <scm|buffer-load> reads and converts the file
    (<cpp|buffer_load>, <cpp|import_tree>) and creates the buffer with
    <cpp|set_buffer_tree>, which allocates a slot of <cpp|the_et>.

    <item><scm|load-buffer-open> calls <scm|switch-to-buffer>:
    <cpp|switch_to_buffer> obtains a passive view on <verbatim|a.tm>
    (creating its editor) and <cpp|window_set_view> replaces the view of
    the scratch buffer in the window: the old view is detached (and
    suspended), the new one attached (and resumed) and made current. The
    scratch buffer stays open, without window.

    <item>The <cpp|resume> of the new editor has called
    <cpp|notify_change> with <cpp|THE_FOCUS> and <cpp|THE_EXTENTS>, and
    the constructor of the editor <cpp|THE_TREE>. At the end of the round,
    the interpose handler calls <cpp|apply_changes> on the editor, which
    typesets the document, passes its extents to the window, computes the
    cursor and invalidates the canvas; the toolkit then calls
    <cpp|handle_repaint>, and the document appears.
  </enumerate>

  <section|Typing a key>

  <\enumerate>
    <item>The <name|Qt> canvas widget receives a key event, translates it
    into a <TeXmacs> key name such as <verbatim|"a"> or <verbatim|"C-x">,
    and queues it with <cpp|qt_gui_rep::process_keypress>.

    <item><cpp|qt_gui_rep::update> takes it from the queue and calls
    <cpp|handle_keypress> on the editor which owns the canvas. The editor
    calls <cpp|start_editing> (which sets the current author for the undo
    system) and passes the key to the <scheme> function
    <scm|keyboard-press>.

    <item>Unless a mode overrides it, <scm|keyboard-press> calls
    <scm|key-press>, that is, <cpp|edit_interface_rep::key_press>, which
    looks the key up as a shortcut with the server
    (<cpp|get_keycomb>, which calls <scm|kbd-find-key-binding>). A plain
    letter is not bound, so it is inserted with <scm|kbd-insert>, which
    eventually calls <cpp|insert (path, tree)> on the global edit tree.

    <item>The modification goes through <cpp|apply> in
    <verbatim|observer.cpp> and is announced to the observers of the
    modified node: the <cpp|ip_observer>s propagate it up to the root of
    the buffer, where the <cpp|edit_observer> of every editor on the buffer
    and the <cpp|undo_observer> of every archiver receive it. Each editor
    forwards it to its typesetter, which invalidates the corresponding
    boxes, corrects its cursor and calls <cpp|notify_change (THE_TREE)>;
    each archiver records the inverse modification.

    <item>Back in <cpp|handle_keypress>, <cpp|end_editing> confirms the
    changes as one undo step.

    <item>The interpose handler calls <cpp|apply_changes>, which retypesets
    the invalid parts of the document, invalidates the screen regions which
    changed and recomputes the cursor; the toolkit repaints them. A short
    while later, if the user is idle, <cpp|update_menus> refreshes the
    menus, the icon bars and the footer.
  </enumerate>

  Fast typing is not slowed down by typesetting, because the repaint step
  is postponed while ordinary keys keep arriving (see <hlink|the event
  loop|server-events.en.tm>).

  <section|Two views on one document>

  <scm|clone-window> (<cpp|clone_window>) opens a new window and calls
  <cpp|get_passive_view> on the current buffer. Since its only view is
  attached to the first window, a second view, with its own editor, is
  created. The two editors share the body of the document (the same
  subtree of <cpp|the_et>) and the references and auxiliary data
  (<cpp|buf-\<gtr\>data>), but each has its own copy of the style and
  initial environment, its own box tree, cursor, selection, zoom factor and
  undo history.

  When the user types in the second window, the modification is announced
  to <em|both> edit observers on the buffer root, so both typesetters are
  updated and both cursors are corrected; the interpose handler then
  retypesets and repaints both windows. No synchronization code is
  involved: this is a consequence of the global edit tree. An undo in one
  window only undoes the changes made in that window, since the archivers
  tag each step with the author of the editor.

  <section|Closing a window>

  <\enumerate>
    <item>The user clicks on the close box. The toolkit calls the
    <cpp|quit> command of the <TeXmacs> widget, a
    <cpp|kill_window_command_rep>, which schedules
    <scm|(safely-kill-window <scm-arg|id>)>.

    <item><scm|safely-kill-window> quits <TeXmacs> (after confirmation) if
    this is the last toolkit window; otherwise it asks for confirmation if
    the buffer of the window is modified and calls <scm|kill-window>.

    <item><cpp|kill_window> makes a view in another window current and
    calls <cpp|delete_window>, which detaches the view (<cpp|suspend>),
    destroys the widgets and deletes the <cpp|tm_window_rep>. The view
    itself survives as a passive view.

    <item>After 100 milliseconds of idle time, <scm|buffer-close> calls
    <cpp|kill_buffer> on the buffer of the closed window, which gives the
    other windows showing this buffer (if any) another buffer to show and
    removes the buffer with all its views.
  </enumerate>

  <section|Working on a document in the background>

  <scheme> code often needs to build or inspect a document without
  showing it, for instance to generate an auxiliary document or to convert
  a file. The pattern is

  <\scm-code>
    (let ((u (string-\<gtr\>url "tmfs://aux/my-report")))

    \ \ (buffer-set-body u '(document "")) \ \ ; create the buffer

    \ \ (view-passive u) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ; give it an editor

    \ \ (with-buffer u \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ; make it current

    \ \ \ \ (set-style-list '("article"))

    \ \ \ \ (insert "Hello"))

    \ \ (buffer-export u "/tmp/report.pdf" "pdf")

    \ \ (buffer-close u))
  </scm-code>

  Each line corresponds to a rule of this chapter: a buffer is created by
  setting its contents; editing commands need an editor, hence a view; the
  editor routines act on the current view, hence <scm|with-buffer>;
  exporting to PDF uses the typesetter of the most recent view; and the
  buffer must be closed explicitly, since it is not attached to any
  window. Note that a passive view is not retypeset by the event loop;
  <cpp|buffer_export> typesets it when it prints.

  <section|Guidelines>

  <\itemize>
    <item>Refer to buffers, views and windows by their names and
    <abbr|URL>s, not by <cpp|tm_buffer>, <cpp|tm_view> or <cpp|tm_window>
    pointers: these are not reference counted and are deleted when the
    objects are closed. Remember that view <abbr|URL>s change when a buffer
    is renamed.

    <item>Remember that the editor glue and the frame routines of the
    server act on the current view and its window. To act on another
    buffer, use <scm|with-buffer> in <scheme> (and make sure that the buffer
    has a view), or change the current view temporarily and restore it, as
    the <cpp|SERVER> macro does, in <c++>. Restore it also when an error
    occurs.

    <item>Modify documents only through the functions of
    <verbatim|observer.cpp> (<cpp|assign>, <cpp|insert>, ...) or the
    editing routines built on top of them. Direct assignments to subtrees
    of <cpp|the_et> bypass the observers, so neither the typesetter, nor
    the other views, nor the undo system would notice them.

    <item>Group the modifications of one user action between
    <cpp|start_editing> and <cpp|end_editing> (the event handlers already
    do this); otherwise they may be merged with the next action in the
    undo history, or be attributed to the wrong author.

    <item>Do not typeset or repaint from editing routines. Call
    <cpp|notify_change> with the appropriate flags; the interpose handler
    will call <cpp|apply_changes> at the next occasion. If up-to-date
    typesetting information is needed immediately (for instance a box or
    the environment at the cursor), <cpp|apply_changes> can be called
    explicitly, but only on views shown in a window.

    <item>Passive views are not updated by the interpose handler. Keep this
    in mind when the result of an operation on a buffer depends on the
    typesetting of that buffer.

    <item>Do not run arbitrary <scheme> code, or open dialogs, from
    callbacks of the toolkit; schedule it with <cpp|exec_delayed>, as the
    close box of a window does.

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
