<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Modifications and patches>

  <section|Elementary modifications>

  A <cpp|modification> (<source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>)
  has a kind <cpp|k>, a path <cpp|p> and an optional tree <cpp|t>. The
  last items of the path encode the position and the arguments of the
  operation; <cpp|root>, <cpp|index> and <cpp|argument> extract them:

  <\description-paragraphs>
    <item*|<cpp|mod_assign (p, t)>>Replace the subtree at <cpp|p> by
    <cpp|t>.

    <item*|<cpp|mod_insert (p, pos, t)>>Insert the children of <cpp|t>
    (or the characters of a string <cpp|t>) at position <cpp|pos> of the
    subtree at <cpp|p>.

    <item*|<cpp|mod_remove (p, pos, nr)>>Remove <cpp|nr> children or
    characters starting at <cpp|pos>.

    <item*|<cpp|mod_split (p, pos, at)>, <cpp|mod_join (p, pos)>>Split the
    child <cpp|pos> at position <cpp|at> into two children, or join the
    children <cpp|pos> and <cpp|pos+1>.

    <item*|<cpp|mod_assign_node (p, lab)>>Change the label of the subtree
    at <cpp|p>.

    <item*|<cpp|mod_insert_node (p, pos, t)>, <cpp|mod_remove_node (p,
    pos)>>Wrap the subtree at <cpp|p> into <cpp|t> as its child
    <cpp|pos>, or replace the subtree at <cpp|p> by its child <cpp|pos>.

    <item*|<cpp|mod_set_cursor (p, pos, data)>>Change nothing, but record
    a cursor position. The editor uses it to restore the cursor and the
    selection on undo (see <cpp|archive_state> below).
  </description-paragraphs>

  All changes of the edit tree <cpp|the_et> go through
  <cpp|apply (tree& ref, modification mod)>
  (<source-link|observer.cpp:390|src/Kernel/Abstractions/observer.cpp:390>)
  and its wrappers <cpp|assign>, <cpp|insert>, <cpp|remove>, ... When the
  tree is attached to the edit tree, the modification is made absolute
  (<cpp|reverse (ip) * mod>) and performed by <cpp|raw_apply> on
  <cpp|the_et>. Modifications made by observers while one is being
  performed are queued in <cpp|upcoming> and performed afterwards, unless
  they touch a path which is already busy, in which case they are silently
  dropped. Each <cpp|raw_*> routine calls
  <cpp|announce> on the observers of the subtree before the change and
  <cpp|done> after it; the undo observer records modifications in
  <cpp|announce>, since the inverse must be computed on the tree before the
  change (its <cpp|notify_*> methods only keep it attached to the right
  tree).

  The functions of <source-link|commute.cpp|src/Data/History/commute.cpp>
  work on single modifications:

  <\description-paragraphs>
    <item*|<cpp|invert (m, t)>>The modification which undoes <cpp|m>
    applied to <cpp|t>. It needs the tree before the change: the inverse of
    a removal re-inserts the removed part, the inverse of an assignment
    assigns the old subtree, and so on.

    <item*|<cpp|swap (m1, m2)>>Given that <cpp|m1> followed by <cpp|m2> is
    defined, compute <cpp|m2*> and <cpp|m1*> such that <cpp|m2*> followed
    by <cpp|m1*> has the same effect, by shifting the paths of one
    modification over the other. It returns <cpp|false> when this is
    impossible, for instance when one modification changes the subtree
    which the other one replaces. <cpp|commute>, <cpp|pull> and
    <cpp|co_pull> are derived from it. This is the operational
    transformation used by the undo system and by <hlink|live
    documents|collab-live.en.tm>.

    <item*|<cpp|join (m1, m2, t)>>Merge two insertions of text into the
    same string at adjacent positions, or two adjacent removals from the
    same string, into one modification. Nothing else is joined.
  </description-paragraphs>

  <section|Patches>

  A <cpp|patch> (<source-link|patch.hpp|src/Data/History/patch.hpp>)
  has one of five types:

  <\description-paragraphs>
    <item*|<cpp|PATCH_MODIFICATION>>A pair (<cpp|mod>, <cpp|inv>) of a
    modification and its inverse, built by <cpp|patch (mod, inv)>.
    <cpp|get_modification> and <cpp|get_inverse> return the two parts.

    <item*|<cpp|PATCH_COMPOUND>>A sequence of patches, applied from the
    first to the last. <cpp|patch (p1, p2)> is the sequence of two patches;
    the archiver builds its lists from such pairs.

    <item*|<cpp|PATCH_BRANCH>>A set of alternative patches. It is only used
    for the possible futures of the history.

    <item*|<cpp|PATCH_AUTHOR>>A patch labeled with an author, built by
    <cpp|patch (author, p)>.

    <item*|<cpp|PATCH_BIRTH>>A patch without effect, built by
    <cpp|patch (author, create)>, which marks the moment an author (or a
    marker) appears. The archiver always records births with <cpp|create
    = false>, for slave authors and for markers alike; markers are told
    apart by their numbers (<cpp|is_marker>).
  </description-paragraphs>

  The functions on patches (<source-link|patch.cpp|src/Data/History/patch.cpp>)
  extend those on modifications: <cpp|invert (p, t)> reverses a compound
  and inverts its parts; <cpp|apply (p, t)> applies a patch to the edit
  tree; <cpp|is_applicable> tests whether it can be applied (the archiver
  asserts this before replaying a step); <cpp|swap>
  (<source-link|patch.cpp:429|src/Data/History/patch.cpp:429>) commutes
  patches recursively, refusing branches, and refusing to move a patch past
  the birth of its own author; <cpp|join>
  (<source-link|patch.cpp:554|src/Data/History/patch.cpp:554>) joins two
  patches of the same author whose only effective parts are joinable
  modifications, keeping the cursor modifications around them.

  Three helpers prepare patches for the history:

  <\description-paragraphs>
    <item*|<cpp|compactify (p)>>Flattens nested compounds, drops pairs of a
    modification followed directly by its inverse, and moves a common author
    label of all the parts outside.

    <item*|<cpp|remove_set_cursor (p)>>Removes the cursor modifications.
    A step which only contains such modifications is not stored in the
    history.

    <item*|<cpp|cursor_hint (p, t)>>A cursor position for the first
    modification of <cpp|p> which is not a cursor modification, computed on
    the tree <cpp|t> before <cpp|p> is applied: for an insertion, the place
    where the text goes. After an undo or a redo the editor moves the
    cursor to the hint of the inverse of the replayed step, computed on the
    tree after the replay, so that the cursor ends up at the place of the
    change.
  </description-paragraphs>

  Patches are printed by <cpp|operator \<less\>\<less\>>; <scm|show-history>
  prints the history of the current editor this way on the console.

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
