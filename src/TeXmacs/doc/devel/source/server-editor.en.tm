<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The editor: classes, state, modifications and undo>

  Every view owns an <em|editor>, which holds everything that is specific
  to one way of looking at a document: the cursor, the selection, the
  typeset box tree, the undo history and the window decorations which
  depend on the cursor. This page describes the classes from which the
  editor is assembled, the state it keeps, and the path which a
  modification of the document follows: from the elementary tree
  operations through the observers to the typesetters and cursors of all
  views on the buffer, and into the undo history.

  How the editor receives events and when it retypesets and repaints is
  the subject of <hlink|the event loop|server-events.en.tm>. The editing
  operations themselves (text, mathematics, tables, structure, search) are
  described in <hlink|structured editing, search and automatic
  content|editing.en.tm>, and the typesetter which the editor drives in
  <hlink|the typesetting algorithm|typesetter.en.tm>.

  <section|The editor classes>

  An editor is an instance of <cpp|edit_main_rep>, created by
  <cpp|new_editor (server_rep* sv, tm_buffer buf)> in
  <source-link|Edit/Editor/edit_main.cpp|src/Edit/Editor/edit_main.cpp>. Its abstract base class
  <cpp|editor_rep> (<source-link|Edit/editor.hpp|src/Edit/editor.hpp>) derives from
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

  <\description-paragraphs>
    <item*|<cpp|edit_interface_rep>>(<source-link|Edit/Interface/edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>,
    <source-link|edit_keyboard.cpp|src/Edit/Interface/edit_keyboard.cpp>, <source-link|edit_mouse.cpp|src/Edit/Interface/edit_mouse.cpp>,
    <source-link|edit_repaint.cpp|src/Edit/Interface/edit_repaint.cpp>, <source-link|edit_footer.cpp|src/Edit/Interface/edit_footer.cpp>,
    <source-link|edit_complete.cpp|src/Edit/Interface/edit_complete.cpp>) Event handlers, change notification and
    <cpp|apply_changes>, repainting, the footer, keyboard shortcuts, input
    modes and completion.

    <item*|<cpp|edit_cursor_rep>>(<source-link|Edit/Interface/edit_cursor.cpp|src/Edit/Interface/edit_cursor.cpp>)
    The cursor and cursor movements.

    <item*|<cpp|edit_graphics_rep>>(<source-link|Edit/Interface/edit_graphics.cpp|src/Edit/Interface/edit_graphics.cpp>)
    Interaction with graphics.

    <item*|<cpp|edit_typeset_rep>>(<source-link|Edit/Editor/edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp>)
    The link with the typesetter: document data, environment queries and
    invalidation; see <hlink|the typesetting algorithm|typesetter.en.tm>.

    <item*|<cpp|edit_modify_rep>>(<source-link|Edit/Modify/edit_modify.cpp|src/Edit/Modify/edit_modify.cpp>)
    Reception of modifications, undo and redo.

    <item*|<cpp|edit_text_rep>, <cpp|edit_math_rep>, <cpp|edit_table_rep>,
    <cpp|edit_dynamic_rep>>(<verbatim|Edit/Modify/>) Structured editing
    operations on text, mathematics, tables and markup.

    <item*|<cpp|edit_process_rep>>(<verbatim|Edit/Process/>) Generation of
    bibliographies, tables of contents, indexes and glossaries.

    <item*|<cpp|edit_select_rep>>(<source-link|Edit/Replace/edit_select.cpp|src/Edit/Replace/edit_select.cpp>)
    Selections and the clipboard.

    <item*|<cpp|edit_replace_rep>>(<source-link|Edit/Replace/edit_search.cpp|src/Edit/Replace/edit_search.cpp>,
    <source-link|edit_spell.cpp|src/Edit/Replace/edit_spell.cpp>) Searching upwards in the tree, interactive
    search and replace, spell checking.
  </description-paragraphs>

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
  an entry to <source-link|Scheme/Glue/build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm> (after which the
  glue has to be regenerated). Since the glue calls it on
  <cpp|get_current_editor ()>, the command always acts on the current view.

  <section|Editor state>

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
    longer shortcut (see <hlink|keyboard events|server-events.en.tm>). Input method
    pre-edit text is handled similarly with <cpp|pre_edit_s> and
    <cpp|pre_edit_mark>.

    <item*|Change flags>The bit set <cpp|env_change> records what has to be
    recomputed at the next <cpp|apply_changes>
    (see <hlink|change notification|server-events.en.tm>); <cpp|last_change>,
    <cpp|last_update> and <cpp|last_event> are time stamps used to decide
    when menus and the footer are updated.

    <item*|Messages>The left and right footer messages <cpp|message_l> and
    <cpp|message_r> (see <cpp|set_message>).

    <item*|Undo history>The <cpp|archiver> <cpp|arch> and the author
    identifier <cpp|author> of <cpp|edit_modify_rep>
    (see below).

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

  <section|Modifications and observers><label|sec-modifications>

  The document is only modified through a small set of elementary
  <em|modifications> (<source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>):
  <cpp|MOD_ASSIGN>, <cpp|MOD_INSERT>, <cpp|MOD_REMOVE>, <cpp|MOD_SPLIT>,
  <cpp|MOD_JOIN>, <cpp|MOD_ASSIGN_NODE>, <cpp|MOD_INSERT_NODE>,
  <cpp|MOD_REMOVE_NODE> and <cpp|MOD_SET_CURSOR>. A modification consists of
  its kind <cpp|k>, a path <cpp|p> and possibly a tree <cpp|t>. The function
  <cpp|apply (tree& ref, modification mod)> in
  <source-link|Kernel/Abstractions/observer.cpp|src/Kernel/Abstractions/observer.cpp> is the single entry point;
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
    <item*|<cpp|ip_observer>>(<source-link|Data/Observers/ip_observer.cpp|src/Data/Observers/ip_observer.cpp>)
    Every node of <cpp|the_et> carries an ip observer which knows its
    inverse path. Announcements are propagated upwards to the observers of
    the ancestors, with the path of the modification extended accordingly.
    Hence an observer attached to the root of a buffer is notified of all
    changes inside the buffer.

    <item*|<cpp|edit_observer>>(<source-link|Data/Observers/edit_observer.cpp|src/Data/Observers/edit_observer.cpp>)
    Attached to the buffer root by each editor. It forwards announcements to
    <cpp|edit_announce>, completions to <cpp|edit_done> and
    <cpp|touch>-notifications to <cpp|edit_touch> (in
    <source-link|Edit/Modify/edit_modify.cpp|src/Edit/Modify/edit_modify.cpp>).

    <item*|<cpp|undo_observer>>(<source-link|Data/Observers/undo_observer.cpp|src/Data/Observers/undo_observer.cpp>)
    Attached to the buffer root by each archiver; records every modification
    in the undo history through <cpp|archive_announce>.

    <item*|Scheme observers>The <scheme> hooks attached through a link
    repository with a callback (<cpp|scheme_observer>, in
    <source-link|Data/Observers/tree_pointer.cpp|src/Data/Observers/tree_pointer.cpp>) are called as
    <scm|(<scm-arg|callback> 'announce <scm-arg|tree>
    <scm-arg|modification>)>, and similarly with <scm|'done> and
    <scm|'touched>. <cpp|tm_buffer_rep::attach_notifier> (<scheme>:
    <scm|buffer-attach-notifier>) uses this to call <scm|buffer-notify> on
    all changes of a buffer, after an initial call of
    <scm|buffer-initialize>; this is used for shared buffers and mirrored
    parts (<source-link|part/part-shared.scm|TeXmacs/progs/part/part-shared.scm>).

    <item*|Positions and pointers><cpp|tree_position> (for cursor positions,
    see above), <cpp|tree_pointer>, tree addenda (<cpp|tree_addendum_new>,
    used for instance by animations) and the observers which store syntax
    highlighting information (<cpp|highlight_observer>, used by the packrat
    parser).
  </description>

  <section|From modifications to editors>

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

  <section|Undo, redo and the \Pmodified\Q status><label|sec-editor-undo>

  Each editor owns an <cpp|archiver> (<source-link|Data/History/archiver.hpp|src/Data/History/archiver.hpp>),
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

  The history, authors, markers and the modified state are described in
  detail in <hlink|undo, redo and the modification history|undo.en.tm>.

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
