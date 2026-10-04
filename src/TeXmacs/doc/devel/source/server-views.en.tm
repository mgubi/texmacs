<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Views and the current view>

  A <em|view> is an editor on a buffer. There may be several views on the
  same buffer; each has its own cursor, selection, typeset box tree, zoom
  factor and undo history, but they all share the document tree, so that
  a change made in one view appears at once in all the others. A view is
  shown in at most one window, and a window shows exactly one view at a
  time. A view which is not shown in any window is called <em|passive>;
  passive views are used to work on documents which are not displayed, and
  to keep the \Pmodified\Q status of a buffer alive after its last window
  has been closed.

  At any moment one view is <em|current>. The current view is the hidden
  argument of most of the program: it determines the current buffer, the
  current window, the current editor and the current <abbr|DRD>. Most
  surprises in this layer come from code which runs with another current
  view than its author expected; the second half of this page explains
  when the current view changes and how to change it safely.

  <section|The class <cpp|tm_view_rep>>

  <\explain>
    <cpp|class tm_view_rep><explain-synopsis|an editor on a buffer>
  <|explain>
    Declared in <verbatim|Texmacs/tm_window.hpp> (its constructor is in
    <verbatim|Texmacs/Data/new_view.cpp>); <cpp|tm_view> is a plain pointer
    to it. Its fields are:

    <\description>
      <item*|<cpp|tm_buffer buf>>The buffer.

      <item*|<cpp|editor ed>>The editor (a reference counted handle; see
      <hlink|the editor|server-editor.en.tm>).

      <item*|<cpp|tm_window win>>The window which displays the view, or
      <cpp|NULL> for a passive view.

      <item*|<cpp|int nr>>A number which distinguishes the views on the
      same buffer. It is allocated by <cpp|new_view_number> from the
      static table <cpp|view_number_table>, indexed by buffer name, and is
      never reused, not even after the buffer has been closed and opened
      again.
    </description>
  </explain>

  The editor points back to its buffer (<cpp|ed-\<gtr\>buf>) and, once the
  view is shown, to the canvas widget of its window
  (<cpp|ed-\<gtr\>cvw>). Views are owned by their buffer (the array
  <cpp|tm_buffer_rep::vws>); they are not reference counted.

  <section|View identifiers>

  Outside <verbatim|Texmacs/Data/>, and in particular in <scheme>, a view
  is designated by an <abbr|URL> of the form

  <\verbatim-code>
    tmfs://view/<em|nr>/<em|encoded-buffer-name>
  </verbatim-code>

  which is computed by <cpp|abstract_view> and decoded by
  <cpp|concrete_view> in <verbatim|new_view.cpp>. The buffer name is
  encoded by the static function <cpp|encode_url>: a relative name becomes
  <verbatim|here/...>, a local absolute name <verbatim|default/...> (for
  instance <verbatim|tmfs://view/1/default/home/joris/a.tm>), and another
  root (<verbatim|tmfs>, <verbatim|http>, ...) is followed by a slash and
  the rest of the name; <cpp|decode_url> reverses this, with special
  handling of drive letters on <name|Windows>. <cpp|concrete_view> looks
  up the buffer first and then the view with the given number among the
  views of that buffer.

  Two consequences follow. A view <abbr|URL> becomes invalid as soon as
  its buffer is closed. And since the identifier contains the buffer
  name, it <em|changes> when the buffer is renamed: <cpp|rename_buffer>
  removes the old view <abbr|URL>s from the view history before the rename
  and puts the new ones back afterwards (<cpp|notify_rename_before>,
  <cpp|notify_rename_after>). Any view <abbr|URL> kept elsewhere, for
  instance in a <scheme> variable, silently becomes invalid after
  <menu|File|Save as>.

  <section|Creating and destroying views>

  New views are created by <cpp|get_new_view (url name)>, which

  <\enumerate>
    <item>creates an empty buffer if necessary (<cpp|create_buffer>);

    <item>creates a new editor with <cpp|new_editor (get_server ()
    -\<gtr\> get_server (), buf)>;

    <item>appends the new <cpp|tm_view_rep> to <cpp|buf-\<gtr\>vws> and passes
    the document data to the editor with <cpp|set_data>;

    <item>temporarily makes the new view current and executes
    <verbatim|$TEXMACS_PATH/progs/init-buffer.scm> (or the file given with
    the <verbatim|-b> option) and
    <verbatim|$TEXMACS_HOME_PATH/progs/my-init-buffer.scm>; the default
    <verbatim|init-buffer.scm> sets the default style of unnamed buffers.
  </enumerate>

  A new view is passive and is not in the view history (see below). Other
  ways to obtain a view are

  <\description-paragraphs>
    <item*|<cpp|get_passive_view (name)>>An existing view on the buffer
    which is not attached to a window. It loads the buffer if it does not
    exist yet (<cpp|concrete_buffer_insist>) and creates a new view if all
    existing views are shown in windows. This is the view which is
    attached when a buffer is shown in a window.

    <item*|<cpp|get_recent_view (name)>>Creates a new view if the buffer
    has none; otherwise it prefers the current view, then the most recent
    attached view on the buffer, then the most recent view on the buffer
    in the history, and finally the first view of the buffer. It is used
    when an editor is needed for a computation on the buffer, for instance
    by <cpp|buffer_export>.

    <item*|<cpp|get_recent_view (name, same, other, active,
    passive)>>The first view of the history which passes the given
    filters: on the same buffer, on another buffer, attached to a window,
    not attached.
  </description-paragraphs>

  Views are destroyed with <cpp|delete_view>, which removes the view from
  its buffer and from the history, sets <cpp|ed-\<gtr\>buf> to <cpp|NULL>
  and deletes the <cpp|tm_view_rep>; the editor itself is reference
  counted and dies with its last reference. Views are also deleted when
  their buffer is removed. They are <em|not> deleted when their window is
  closed: <cpp|delete_window> only detaches them, since a buffer without
  views would always be reported as unmodified.

  <section|The view history>

  The array <cpp|view_history> lists the <abbr|URL>s of the views which
  have been attached to a window at least once, most recently attached
  first; <cpp|notify_set_view> (called by <cpp|attach_view>) moves a view to
  the front and <cpp|notify_delete_view> (called by <cpp|delete_view>)
  removes it. Detaching a view does not remove it from the history.
  Renaming a buffer is an exception to the rule: <cpp|notify_rename_after>
  puts <em|all> views of the renamed buffer at the front, including views
  which have never been attached.

  The history is the list returned by <cpp|get_all_views>
  (<scm|view-list>) and is used for all \Pmost recent\Q queries, and also by
  <cpp|window_to_view>, which finds the view shown in a window by searching
  the history. Since a view which has never been attached to a window (for
  instance the passive view created to load a buffer in the background) is
  normally not in the history, <cpp|get_all_views> does not return all
  views; use <cpp|buffer_to_views> to enumerate the views of a buffer.

  <section|Showing a view in a window>

  A view is shown in a window with <cpp|attach_view (url win, url view)>,
  which

  <\enumerate>
    <item>sets <cpp|vw-\<gtr\>win>;

    <item>installs the editor as the scrollable canvas of the window widget
    (<cpp|set_scrollable (wid, vw-\<gtr\>ed)>) and stores the canvas widget
    in <cpp|ed-\<gtr\>cvw>;

    <item>calls <cpp|ed-\<gtr\>resume ()>, which installs the menus, icon
    bars and side tools of this editor in the window, makes the cursor
    accessible and requests a full update;

    <item>sets the title and the <abbr|URL> of the window from the buffer;

    <item>moves the view to the front of the view history.
  </enumerate>

  <cpp|detach_view> does the converse: it calls <cpp|ed-\<gtr\>suspend ()>
  (which interrupts a pending shortcut, clears the footer message and
  frees the cached renderers), replaces the canvas by an empty
  <cpp|glue_widget> and resets the title to <verbatim|TeXmacs>.

  These two routines are rarely called directly. The usual entry points
  are

  <\description>
    <item*|<cpp|window_set_view (win, view, focus)>>Detaches the previous
    view of the window and attaches the new one. It does nothing if the
    view is already shown there, asserts that the new view is not attached
    to another window, and makes the new view current if <cpp|focus> is
    set or if the old view was current.

    <item*|<cpp|switch_to_buffer (name)>>Shows a passive view on the
    buffer (loading it if needed) in the current window, gives it the
    focus and re-applies the zoom factor of the window
    (<scm|switch-to-buffer>).

    <item*|<cpp|window_set_buffer (win, name)>>Shows a passive view on
    the buffer in the given window, without changing the focus
    (<scm|window-set-buffer>).
  </description>

  Creating windows (<cpp|open_window>, <cpp|clone_window>,
  <cpp|new_buffer_in_new_window>) is described in <hlink|windows|server-windows.en.tm>.

  <section|The current view>

  The current view is stored in the global pointer <cpp|the_view> of
  <verbatim|new_view.cpp> (no other file uses it directly) and manipulated
  by

  <\description-paragraphs>
    <item*|<cpp|set_current_view (url u)>>Makes <cpp|u> current. As a side
    effect, the global <cpp|the_drd> is set to the <abbr|DRD> of the editor
    and the <cpp|last_visit> time of the buffer is updated. An invalid
    <abbr|URL> leaves <em|no> current view.

    <item*|<cpp|get_current_view ()>>Returns the current view; asserts that
    there is one.

    <item*|<cpp|get_current_view_safe ()>, <cpp|has_current_view
    ()>>Return <cpp|url_none ()>, respectively <cpp|false>, if there is no
    current view.

    <item*|<cpp|get_current_editor ()>>Returns the editor of the current
    view.
  </description-paragraphs>

  Everything else that is \Pcurrent\Q is derived from the current view:

  <\itemize>
    <item><cpp|get_current_buffer> returns the buffer of the current view;

    <item><cpp|has_current_window>, <cpp|get_current_window> and
    <cpp|concrete_window ()> refer to the window of the current view, if it
    is attached; there is no separately stored current window;

    <item>all routines of <verbatim|build-glue-editor.scm> are exported as
    <cpp|get_current_editor()-\<gtr\>...>, so that every <scheme> editing
    command acts on the current view;

    <item>all routines of <cpp|tm_frame_rep> (menus, footer, zoom,
    dialogs) act on <cpp|concrete_window ()>;

    <item>the typesetter, the <abbr|DRD> based predicates and the menus use
    <cpp|the_drd>.
  </itemize>

  Note that the current view may be passive. Then there is a current
  buffer and a current editor, but no current window: <scm|current-window>
  returns the empty <abbr|URL>, the frame routines do nothing, and the few
  which do not check (see <hlink|the partial server
  <cpp|tm_frame_rep>|server-classes.en.tm>) crash.

  <section|When the current view changes>

  The current view is changed

  <\itemize>
    <item>when a window obtains the keyboard focus:
    <cpp|edit_interface_rep::handle_keyboard_focus> calls
    <cpp|focus_on_this_editor ()>, which calls <cpp|focus_on_editor>, which
    searches the view of the editor among the views of all buffers and makes
    it current;

    <item>when a view is shown in a window with focus
    (<cpp|window_set_view> with <cpp|focus>, <cpp|switch_to_buffer>) or in
    the window of the current view;

    <item>explicitly, by <cpp|window_focus (url win)> (which makes the view
    of the window current, without touching the GUI focus),
    <cpp|focus_on_buffer (url name)> (which chooses the most recent view on
    the buffer, preferably an attached one) or <cpp|var_focus_on_buffer>
    (which in addition suspends the old editor, resumes the new one and
    moves the keyboard focus);

    <item>when windows are closed: <cpp|kill_window> makes a view in
    another window current before deleting the window;

    <item>temporarily, by the <cpp|SERVER> macro in <c++> and by the
    <scheme> macros <scm|with-buffer> and <scm|with-window>, described
    below.
  </itemize>

  <cpp|switch_to_window (url win)> is different: it suspends the editor of
  the current window, maps the new window and resumes its editor, and sends
  the GUI keyboard focus to it, but it does <em|not> change the current
  view. The current view is updated only when the toolkit reports the
  focus change and <cpp|handle_keyboard_focus> runs, that is, during a later
  round of the event loop. Code which calls <scm|switch-to-window> and then
  immediately acts on the \Pcurrent\Q buffer still acts on the old one.

  <section|Working in the context of another view>

  <paragraph|In <c++>.>The routines of the server act on the current
  window. An editor which needs such a routine for its <em|own> window
  (for instance to install its menus or to show a message in its footer)
  uses the <cpp|SERVER> macro of <verbatim|Edit/editor.hpp>, which makes
  the editor current for the duration of the call:

  <\cpp-code>
    #define SERVER(cmd) { \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \\

    \ \ url temp= get_current_view_safe (); \\

    \ \ focus_on_this_editor (); \ \ \ \ \ \ \ \ \ \ \ \\

    \ \ sv-\<gtr\>cmd; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \\

    \ \ set_current_view (temp); \ \ \ \ \ \ \ \ \ \ \ \\

    }
  </cpp-code>

  The same pattern (save with <cpp|get_current_view_safe>, change, restore
  with <cpp|set_current_view>) is used by <cpp|get_new_view>. Note that
  the restoration is not protected against exceptions.

  <paragraph|In <scheme>.>The macros <scm|with-buffer> and
  <scm|with-window> of <verbatim|utils/library/cursor.scm> execute a body
  in the context of another buffer:

  <\scm-code>
    (with-buffer buf

    \ \ (insert "Hello"))
  </scm-code>

  <scm|with-buffer> does nothing special if <scm|buf> is the current
  buffer; otherwise it calls <scm|buffer-focus> (<cpp|focus_on_buffer>) on
  it, evaluates the body and calls <scm|buffer-focus> on the old buffer.
  <scm|with-window> calls <scm|with-buffer> on the buffer of the window.
  Three details matter:

  <\itemize>
    <item>If the buffer does not exist or has no view, <scm|buffer-focus>
    fails and the body is <em|not executed>; the macro returns <scm|#f>.
    This is why, for instance, <scm|buffer-copy> in
    <verbatim|tm-files.scm> calls <scm|view-new> on the new buffer before
    using <scm|with-buffer>.

    <item>The previous context is restored by <em|buffer>, not by view: if
    the old buffer had several views, <cpp|focus_on_buffer> may return to a
    different one. Likewise, <scm|with-window> focuses on the most recent
    view of the window's buffer, which is not necessarily the view shown in
    that window if the buffer is shown in several windows.

    <item>The restoration does not use <scm|dynamic-wind>: if the body
    raises an error, the current view is left on the other buffer, and the
    next event is processed in the wrong context.
  </itemize>

  <section|Pitfalls>

  <\itemize>
    <item>Do not keep <cpp|tm_view> pointers or view <abbr|URL>s across
    operations which may close or rename buffers. <cpp|concrete_view>
    returns <cpp|NULL> for a stale <abbr|URL>, and <cpp|view_to_editor>
    removes it from the history and returns a nil editor (or fails in
    <verbatim|ADVANCED_DEVELOPER_MODE>); most callers do not check.

    <item><cpp|get_current_view>, <cpp|get_current_editor>,
    <cpp|get_current_buffer> and the one argument <cpp|get_recent_view>
    assert that there is a current view, which is not the case very early
    during startup. Use the
    <cpp|_safe> variants in code which may run in such situations.

    <item>Code which temporarily changes the current view must restore
    it, also in case of errors.

    <item><cpp|get_all_views> only returns views which have been attached
    to a window at least once (or belong to a renamed buffer).

    <item>Passive views are not retypeset by the event loop (see
    <hlink|the interpose handler|server-events.en.tm>); their typesetting
    information may be out of date until it is explicitly requested.
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
