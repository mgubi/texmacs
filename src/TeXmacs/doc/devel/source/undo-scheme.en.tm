<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <scheme> interface to the history>

  <section|Functions>

  The functions below are defined in <source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>
  (they act on the archiver of the current editor) and
  <source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>.

  <\description-paragraphs>
    <item*|<scm|(undo 0)>, <scm|(redo <em|i>)>>Undo the last step of the
    current editor's author (see <hlink|the archiver|undo-archiver.en.tm>
    for the steps of other authors), or redo future number <em|i>. The
    argument of <scm|undo> must be 0. They first drop the pending cursor
    modifications, and do nothing while other modifications of the current
    action are pending, and the C++ <cpp|undo> also resets
    the graphics editor and refuses to undo inside a graphics while
    <scm|graphics-undo-enabled> is false.

    <item*|<scm|(undo-possibilities)>, <scm|(redo-possibilities)>>The number
    of possible undos (0 or 1) and of futures. The <menu|Edit> menu uses
    them.

    <item*|<scm|(unredoable-undo)>>Cancel the pending modifications of the
    current action, then undo the last step and forget it, without creating
    a future.

    <item*|<scm|(start-editing)>, <scm|(end-editing)>,
    <scm|(cancel-editing)>>Delimit a user action. The event handlers already
    call them; <scheme> code only needs them when it changes documents
    outside an event, for instance in a <scm|delayed> block or a callback
    of a network request, where <scm|(commit-changes)>, the same as
    <scm|end-editing>, closes the step.

    <item*|<scm|(add-undo-mark)>, <scm|(remove-undo-mark)>>Confirm the
    pending modifications as a step now; or reopen the last step of the
    current author, so that the next modifications become part of it (the
    graphics editor uses this to merge the start of an operation with its
    end, <source-link|graphics-group.scm:339|TeXmacs/progs/graphics/graphics-group.scm:339>).

    <item*|<scm|(mark-new)>, <scm|(mark-start <em|m>)>, <scm|(mark-end
    <em|m>)>, <scm|(mark-cancel <em|m>)>>Markers: a new marker number,
    and the operations described in <hlink|the archiver|undo-archiver.en.tm>.
    Every <scm|mark-start> must be followed by exactly one <scm|mark-end> or
    <scm|mark-cancel> with the same marker, in the same editor.

    <item*|<scm|(archive-state)>>Record the cursor and the selection in the
    pending step, so that undoing it restores them.

    <item*|<scm|(new-author)>, <scm|(set-author <em|a>)>,
    <scm|(get-author)>, <scm|(start-slave <em|a>)>>Authors: a fresh author;
    the author to which the next modifications are attributed; and the
    birth of a slave author in the current step.

    <item*|<scm|(busy-versioning?)>>True while an undo, a redo or a
    cancellation is being applied. Observers which react to modifications
    can use it to avoid modifying the document in turn.

    <item*|<scm|(clear-undo-history)>>Empty the history. <em|Note>: this
    acts on the archivers of all open buffers, see <hlink|the
    pitfalls|undo-pitfalls.en.tm>.

    <item*|<scm|(show-history)>>Print the history of the current editor on
    the standard output.
  </description-paragraphs>

  <section|How to>

  <paragraph|Make a command one undo step.>Nothing has to be done for a
  command which runs within one event (a key, a menu entry, a mouse
  click): all its modifications form one step. A command which needs
  several events, for instance a dialog which applies a change for each
  input, can bracket them with a marker:

  <\scm-code>
    (define my-mark #f)

    (define (my-start)

    \ \ (set! my-mark (mark-new))

    \ \ (mark-start my-mark)

    \ \ (archive-state))

    (define (my-done ok?)

    \ \ (if ok? (mark-end my-mark) (mark-cancel my-mark))

    \ \ (set! my-mark #f))
  </scm-code>

  Within one event, the macro <scm|try-modification> does the same: its
  changes are kept as one step when the body returns a true value and
  are cancelled otherwise.

  <\scm-code>
    (try-modification

    \ \ (insert "x")

    \ \ (my-check-something))
  </scm-code>

  <paragraph|Show a temporary preview.>Commands which change the document
  repeatedly in response to a gesture, such as the pinch gestures of
  <source-link|format-geometry-edit.scm|TeXmacs/progs/generic/format-geometry-edit.scm>,
  undo the previous preview with <scm|(undo 0)> before applying the next
  one. This only works if each preview is a step of its own and nothing
  else was changed in between.

  <paragraph|Attribute changes to another author.>Changes which do not come
  from the user, such as the output of a computation, should be attributed
  to their own author, so that the user's undo does not remove them unless
  needed. Take a <scm|new-author>, record its birth with <scm|start-slave>
  in the step which starts the computation, and make the later changes
  between <scm|(set-author <em|a>)> and <scm|(commit-changes)>, restoring
  the previous author afterwards, as <scm|with-author> in <source-link|plugin-eval.scm|TeXmacs/progs/utils/plugins/plugin-eval.scm>
  does.

  <paragraph|Test undo.>The test suites check the history with
  <scm|undo-possibilities>, see for instance <source-link|check/editing-test.scm|TeXmacs/progs/check/editing-test.scm>.
  In a test which runs without events, wrap each simulated action in
  <scm|(start-editing)> and <scm|(end-editing)>: otherwise all the
  modifications end up in one step, or remain pending and are ignored by
  <scm|undo>.

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
