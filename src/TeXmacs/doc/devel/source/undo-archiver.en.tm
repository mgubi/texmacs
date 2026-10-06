<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The archiver>

  <section|One archiver per editor>

  The constructor of <cpp|edit_modify_rep>
  (<source-link|edit_modify.cpp:23|src/Edit/Modify/edit_modify.cpp:23>)
  allocates a new author for the editor and an <cpp|archiver> for the root
  path <cpp|rp> of its buffer. The archiver attaches an undo observer to
  that subtree of <cpp|the_et> and registers itself in the global set
  <cpp|archs>. Since there is one editor per view, a buffer shown in two
  windows has two archivers with different authors. Both observe the same
  subtree and record every modification, so in each of them the changes
  made in the other view are changes of another author.

  The state of an archiver (<source-link|archiver.hpp|src/Data/History/archiver.hpp>)
  is:

  <\description-paragraphs>
    <item*|<cpp|current>>The modifications of the user action in progress,
    most recent first, each stored as a patch (<em|inverse>,
    <em|modification>). Applying <cpp|current> as it is undoes the action.

    <item*|<cpp|archive>>The history: an undo part and the possible
    futures, see below.

    <item*|<cpp|the_author>, <cpp|the_owner>>The author of the editor, and
    the author of the modifications in <cpp|current>.

    <item*|<cpp|depth>, <cpp|last_save>, <cpp|last_autosave>>The number of
    steps in the undo part, and its value at the last save and autosave
    (-1 when unknown). They tell whether the document is modified.

    <item*|<cpp|versioning>>True while the archiver applies a patch itself.
    The modifications it makes are then not recorded; the global flag
    <cpp|busy_versioning> (<scm|busy-versioning?> in <scheme>) is set at the
    same time.
  </description-paragraphs>

  <section|The shape of the history>

  The history is a tree, built from compound and branch patches:

  <\verbatim-code>
    archive \ \ ::= branch [ undo-part, future_1, ..., future_n ]

    undo-part ::= compound ( step, archive ) \ \ \ \ \ \ or nothing

    future_i \ ::= compound ( redo-step, archive )
  </verbatim-code>

  A branch with a single element is represented by that element
  (<cpp|make_branches>). The <em|step> of the undo part is the last
  confirmed user action. It is a patch labeled with its author whose
  modifications are the inverses of the changes, so that applying it
  undoes the action; its <cpp|get_inverse> parts redo it. The <em|futures>
  are the changes which were undone and can be redone. Each one is again a
  step, followed by the history which applies after it. Undoing a step
  adds a future instead of replacing the existing ones, so the history keeps
  several futures: after undoing a change and making a different one, the
  undone change can still be redone after undoing the new one. In that case
  <menu|Edit|Redo> becomes a submenu with one entry per branch
  (<scm|redo-menu> in <source-link|edit-menu.scm|TeXmacs/progs/texmacs/menus/edit-menu.scm>).

  <cpp|undo_possibilities> is 1 when there is an undo part and 0
  otherwise; <cpp|redo_possibilities> is the number of futures.

  <section|Recording and confirming>

  <cpp|archive_announce> (<source-link|archiver.cpp:73|src/Data/History/archiver.cpp:73>)
  receives each modification with its absolute path. Unless the archiver is
  replaying history, <cpp|add> computes the inverse on the tree before the
  change and puts the pair in front of <cpp|current>; the archiver is also
  put in the set <cpp|pending_archs>. If the global author (<cpp|get_author
  ()>) differs from <cpp|the_owner>, the pending modifications are first
  confirmed as a step of their own, so that a step always has a single
  author.

  The editor encloses every user action between <cpp|start_editing> and
  <cpp|end_editing> (<source-link|edit_modify.cpp:293|src/Edit/Modify/edit_modify.cpp:293>):
  key presses in <cpp|handle_keypress>
  (<source-link|edit_keyboard.cpp:395|src/Edit/Interface/edit_keyboard.cpp:395>),
  mouse events in <cpp|handle_mouse>
  (<source-link|edit_mouse.cpp:699|src/Edit/Interface/edit_mouse.cpp:699>)
  and menu actions in <cpp|before_menu_action> and <cpp|after_menu_action>
  (<source-link|edit_interface.cpp:1152|src/Edit/Interface/edit_interface.cpp:1152>).
  <cpp|start_editing> sets the global author to the author of the editor.
  <cpp|end_editing> calls <cpp|global_confirm>, which confirms and
  simplifies every pending archiver, so an action which modifies several
  buffers adds one step to each of them. If the action throws an error
  (the handlers catch it since <cpp|USE_EXCEPTIONS> is defined),
  <cpp|cancel_editing> calls <cpp|global_cancel>, which applies
  <cpp|current> and so restores the documents.

  <cpp|confirm> (<source-link|archiver.cpp:324|src/Data/History/archiver.cpp:324>)
  labels <cpp|current> with its owner, compactifies it, and drops it if it
  only moves the cursor. Otherwise it becomes the new step:
  <cpp|archive := (current ; archive)>. The old history, including its
  futures, becomes the history below the new step, so the futures are
  kept. When the new step belongs to another author (a plug-in, another
  view), <cpp|normalize> then moves those futures of the editor's author
  which commute with it up to the new step, so that the user can still
  redo them.

  <cpp|simplify> (<source-link|archiver.cpp:385|src/Data/History/archiver.cpp:385>)
  then merges the last two steps when they can be joined (see
  <cpp|join> in <hlink|patches|undo-patches.en.tm>), when the older one has
  no futures, and when the save point is not between them. This is why a
  word typed character by character is undone at once, while a new
  paragraph, a deletion following an insertion or a save start a new step.

  Before menu actions, keyboard shortcuts and <scm|try-modification>,
  <cpp|archive_state> (<source-link|edit_modify.cpp:280|src/Edit/Modify/edit_modify.cpp:280>)
  records the cursor and the selection as cursor modifications whose data
  name the author. When such a step is undone, <cpp|notify_set_cursor>
  restores the cursor and the selection of that author only.

  <section|Undo and redo>

  <cpp|undo_one> (<source-link|archiver.cpp:429|src/Data/History/archiver.cpp:429>)
  computes the inverse of the step of the undo part on the current tree,
  applies the step, and makes the inverse a new future, then moves to the history below the step.
  <cpp|redo_one (i)> (<source-link|archiver.cpp:451|src/Data/History/archiver.cpp:451>)
  applies future number <cpp|i>; the remaining futures become futures of
  the history below. Both refuse to act while <cpp|current> is not empty,
  and return a cursor position (<cpp|cursor_hint>) to which the editor
  moves.

  The public <cpp|undo> and <cpp|redo> take authors into account. Before
  each step, <cpp|undo> calls <cpp|expose>
  (<source-link|archiver.cpp:226|src/Data/History/archiver.cpp:226>), which
  tries to bring the most recent step of the editor's own author to the top
  by commuting it (<cpp|swap>) with the more recent steps of other authors.
  If the step on top is then the editor's own, only that step is undone.
  Otherwise the step of the other author is undone, and the loop
  continues until a step of the own author has been undone. In a session,
  for instance, the output of each evaluation has its own author, whose
  birth is recorded in the step which sent the input (<scm|start-slave> in
  <source-link|plugin-eval.scm|TeXmacs/progs/utils/plugins/plugin-eval.scm>).
  If the user edits another part of the document while the output
  arrives, undo removes the user's edit and leaves the output, since the
  two changes commute. Undoing the step which sent the input removes the
  output as well: a patch is never moved past the birth of its author.
  <cpp|redo> redoes future <cpp|i> and then continues as long as there is
  a single future which does not belong to the editor's author. Right
  after a step of the editor's author, it also stops if the next step
  belongs to another editor (the authors in <cpp|genuine_authors>), so that
  the steps of slave authors such as plug-ins are redone together with
  it.

  <section|Markers>

  A marker groups the steps made after it, or cancels them. Markers are
  numbers from <cpp|new_marker> (<scm|mark-new> in <scheme>), and are
  stored as steps containing a birth patch.

  <\description-paragraphs>
    <item*|<cpp|mark_start (m)>>Confirm the pending modifications, then
    add a step which only contains the birth patch of <cpp|m>.

    <item*|<cpp|mark_end (m)>>Confirm, then merge the steps above the
    marker into one step where possible (<cpp|compress>: consecutive steps
    of the same author without futures), and remove the marker step. When
    the marker is not found, a warning is printed and the history is
    <em|emptied> (<source-link|archiver.cpp:587|src/Data/History/archiver.cpp:587>).

    <item*|<cpp|mark_cancel (m)>>Cancel the pending modifications, then
    undo and forget the steps above the marker and remove it; return
    <cpp|true>. If a step of another author is found above the marker, the
    marker is removed, the remaining steps are kept and <cpp|false> is
    returned.
  </description-paragraphs>

  <scm|try-modification> (<source-link|tree.scm:363|TeXmacs/progs/utils/library/tree.scm:363>)
  combines them: the body is executed after <cpp|mark_start> and
  <cpp|archive_state>, and its changes become one step if it returns a true
  value, or are cancelled otherwise. Keyboard shortcuts use the same
  mechanism in <cpp|try_shortcut>
  (<source-link|edit_keyboard.cpp:98|src/Edit/Interface/edit_keyboard.cpp:98>):
  the effect of a key which starts a longer shortcut, for instance the
  insertion of <verbatim|\<less\>> before <verbatim|=>, is enclosed in a
  marker <cpp|sh_mark>, and <cpp|mark_cancel> takes it back when the next
  key completes the shortcut. <cpp|interrupt_shortcut>, called at every
  undo or redo, ends the marker.

  Three more operations edit the top of the history directly:
  <cpp|retract> (<scm|remove-undo-mark>) reopens the last step of the own
  author as <cpp|current>, so that the next modifications are added to it;
  <cpp|forget> (<scm|unredoable-undo>) undoes it without creating a future;
  <cpp|start_slave (a)> adds the birth patch of author <cpp|a> to
  <cpp|current>.

  <section|Saving and the modified state>

  <cpp|need_save> of an editor
  (<source-link|edit_modify.cpp:413|src/Edit/Modify/edit_modify.cpp:413>)
  compares <cpp|last_save> with the depth, corrected by one when the step
  on top is a marker (<cpp|corrected_depth>). <cpp|notify_save> confirms
  the pending changes and records the depth; <cpp|require_save> sets
  <cpp|last_save> to -1, so that the buffer counts as modified until the
  next save. A buffer is modified (<scm|buffer-modified?>) when one of its
  views needs saving (<source-link|new_buffer.cpp:45|src/Texmacs/Data/new_buffer.cpp:45>).
  Undoing back to the saved depth makes the document unmodified again,
  and the editor says so in the status bar
  (<em|Your document is back in its original state>).

  The depth only counts steps, so it is an approximation: <cpp|clear>,
  <cpp|expose> and <cpp|normalize> set <cpp|last_save> to -1 because they
  reorder or forget steps, and redoing another branch than the first one
  past the save point does the same.

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
