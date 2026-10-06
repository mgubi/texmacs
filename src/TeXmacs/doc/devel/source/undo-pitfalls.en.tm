<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Undo: pitfalls and known problems>

  The statements marked <em|(checked)> were verified on 2026-10-06 by
  scripts which simulate user actions with <scm|start-editing> and
  <scm|end-editing> in a headless <TeXmacs>.

  <\itemize>
    <item><em|Clearing the history acts on all buffers and marks them as
    modified> (checked, issue #301 of <verbatim|mgubi/texmacs>).
    <menu|Tools|Clear undo history> calls
    <scm|clear-undo-history>, which calls <cpp|global_clear_history>
    (<source-link|archiver.cpp:85|src/Data/History/archiver.cpp:85>) instead
    of clearing the archiver of the current editor. <cpp|clear> also sets
    <cpp|last_save> to -1, so every open buffer, including saved ones,
    loses its history and is then reported as modified.

    <item><em|An unknown marker empties the history> (checked, issue
    #301). If
    <cpp|mark_end> or <cpp|mark_cancel> does not find its marker, or finds
    it below a future, <cpp|remove_marker> prints <verbatim|warning, marker
    not found> or <verbatim|warning, cannot remove marker> on the console
    and replaces the whole history by an empty one
    (<source-link|archiver.cpp:587|src/Data/History/archiver.cpp:587>). This
    replaced a fatal error (bug #60743 of the old tracker); it happens when
    a <scm|mark-start> and its <scm|mark-end> are executed in different
    editors, or when the history was changed in between. With
    <verbatim|ADVANCED_DEVELOPER_MODE> the second case is still an
    assertion.

    <item><em|Typing is undone in large pieces> (checked). <cpp|simplify>
    joins any two consecutive insertions into the same string at adjacent
    positions, without a time limit, so text typed without moving the
    cursor, even over several minutes, is undone at once. Only a change of
    string (a new paragraph, a formula), typing at another position, a
    deletion or a save starts a new step.

    <item><em|The history belongs to a view.> The archiver is a member of the
    editor, so a buffer shown in two windows has two histories, in which the
    changes made in the other window belong to another author; and the
    history is lost when the view is deleted. Changes to a buffer without
    a view are not recorded at all (see the <verbatim|FIXME> in
    <source-link|edit_modify.cpp:145|src/Edit/Modify/edit_modify.cpp:145>).

    <item><em|Undo is silent while changes are pending.> <cpp|undo> and
    <cpp|redo> of the archiver do nothing while <cpp|current> is not empty.
    A <scheme> function which modifies the document and then calls
    <scm|(undo 0)> in the same action first has to close the step with
    <scm|(add-undo-mark)>.

    <item><em|Undo inside graphics.> While a graphical operation is in
    progress (<scm|graphics-undo-enabled> is false), <scm|undo> only resets
    the state of the graphics editor.

    <item><em|The modified state is approximate.> It compares numbers of
    steps (<cpp|depth> and <cpp|last_save>). Operations which reorder steps
    (<cpp|expose>, <cpp|normalize>) or forget them set <cpp|last_save> to
    -1, so after undoing one's own change past the output of a plug-in the
    document counts as modified even if it is back in its saved state.

    <item><em|Swapped patches may be paired with the wrong inverse> (issue
    #144 of <verbatim|mgubi/texmacs>). <cpp|swap> on two modification
    patches swaps the forward modifications and the inverses separately
    (<source-link|patch.cpp:488|src/Data/History/patch.cpp:488>); when an
    insertion lands where a removal happened, the two swaps can break the
    tie differently, and <cpp|possible_inverse> only compares kinds and
    lengths, so it accepts the result. Undoing the whole sequence still
    restores the tree, but undoing one step does not. The archiver swaps
    patches in <cpp|split> and <cpp|expose>, so a single undo in a history
    with several authors may give a wrong document. A fix is proposed in
    pull request #153.

    <item><em|<cpp|is_applicable> crashed on a join below a string> (issue
    #144). <cpp|can_join>
    (<source-link|modification.cpp:196|src/Kernel/Types/modification.cpp:196>)
    takes the length of a string for a number of children and then indexes
    the string. Fixed by pull request #152 in <verbatim|wip_fixes>.

    <item><em|Plug-in output and undo> (checked). The output of an
    evaluation can only be undone together with, or after, the step which
    sent the input, since a patch is never moved past the birth of its
    author. Undoing that step therefore also removes the output.
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
