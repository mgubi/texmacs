<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Undo, redo and the modification history>

  <section|Introduction>

  Every change of a document in <TeXmacs>, whether it comes from a key
  press, a menu, a <scheme> program, a plug-in session or another
  participant of a live document, ends up as a sequence of <em|elementary
  modifications> of the global edit tree. The undo system records these
  modifications together with their inverses, groups them into steps, and
  replays the inverses on demand. It is a small but subtle subsystem: the
  history is not a simple stack but a tree with several possible futures,
  changes are labeled with their <em|author> so that one can undo one's own
  changes past those of others, and temporary <em|markers> allow a
  sequence of changes to be grouped into one step or cancelled as a whole.

  This chapter describes the data structures and algorithms
  (<source-link|Data/History|src/Data/History/archiver.hpp>), how the editor
  feeds them and decides the granularity of undo, the <scheme> interface,
  and the known problems. The tree observers through which modifications
  are reported are described in <hlink|the architecture
  chapter|architecture.en.tm>, the editor classes in <hlink|the
  editor|server-editor.en.tm>, and the use of the same patches for live
  collaboration in <hlink|live documents|collab-live.en.tm>.

  <section|Overview>

  <\description>
    <item*|Modification>A <cpp|modification> (<source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>)
    is one of nine elementary operations on a tree at a path: assign,
    insert, remove, split, join, assign-node, insert-node, remove-node and
    set-cursor. Every change of the edit tree <cpp|the_et> goes through
    <cpp|apply> (<source-link|observer.cpp:390|src/Kernel/Abstractions/observer.cpp:390>),
    which announces the modification to the observers of the affected
    subtree before performing it.

    <item*|Patch>A <cpp|patch> (<source-link|Data/History/patch.hpp|src/Data/History/patch.hpp>)
    is a modification with its inverse, a sequence (<em|compound>) or a set
    of alternatives (<em|branch>) of patches, a patch labeled with an
    <em|author>, or a <em|birth> patch which marks the appearance of an
    author or of a marker. Patches can be inverted, applied, commuted and
    joined.

    <item*|Archiver>Each editor, that is each view of a buffer, owns an
    <cpp|archiver> (<source-link|Data/History/archiver.hpp|src/Data/History/archiver.hpp>)
    for the root of its buffer. It accumulates the pending modifications in
    <cpp|current> and moves them into the history <cpp|archive> as one
    step when the current user action ends.

    <item*|Undo observer>The archiver attaches an <cpp|undo_observer>
    (<source-link|undo_observer.cpp|src/Data/Observers/undo_observer.cpp>)
    to the buffer root. Its <cpp|announce> method passes each modification
    to <cpp|archive_announce>, which records it unless the archiver is
    itself replaying history.

    <item*|Authors and markers>Authors and markers are numbers of type
    <cpp|double>: <cpp|new_author> returns the integers 1, 2, 3, ...,
    <cpp|new_marker> the half-integers 1.5, 2.5, ... Every editor has its own
    author; plug-in sessions get a fresh author for each evaluation.
  </description>

  The data flow for one key press is:

  <\verbatim-code>
    handle_keypress: start_editing () \ \ \ \ \ \ \ \ \ \ set_author (editor's author)

    \ \ keyboard-press (scheme) -\<gtr\> insert, remove, ...

    \ \ \ \ apply (the_et, mod)

    \ \ \ \ \ \ undo_observer::announce -\<gtr\> archive_announce -\<gtr\> archiver::add

    \ \ \ \ \ \ \ \ current := (inverse, mod) ; current

    \ \ \ \ \ \ raw_apply: the tree changes, other observers are notified

    end_editing () -\<gtr\> global_confirm ()

    \ \ for every archiver with pending changes: confirm (); simplify ()

    \ \ \ \ archive := (author: current) ; archive
  </verbatim-code>

  Undo takes the first step of the archive, applies it (it consists of
  inverses) with recording switched off, and stores its inverse as a new
  branch of the future.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>,
    <source-link|Kernel/Abstractions/observer.cpp|src/Kernel/Abstractions/observer.cpp>>The
    modification type, <cpp|apply> and the <cpp|raw_*> routines which
    perform a modification and notify the observers.

    <item*|<source-link|Data/History/patch.hpp|src/Data/History/patch.hpp>,
    <source-link|patch.cpp|src/Data/History/patch.cpp>>Patches, authors and
    markers; inversion, application, commutation, joining,
    <cpp|compactify> and <cpp|cursor_hint>.

    <item*|<source-link|Data/History/commute.cpp|src/Data/History/commute.cpp>>The
    same operations on single modifications: <cpp|invert>, <cpp|swap>,
    <cpp|commute>, <cpp|pull> and <cpp|join>.

    <item*|<source-link|Data/History/archiver.cpp|src/Data/History/archiver.cpp>>The
    history of an editor: recording, confirming and cancelling steps,
    simplification, undo, redo, authors, markers and the save state.

    <item*|<source-link|Data/Observers/undo_observer.cpp|src/Data/Observers/undo_observer.cpp>>The
    observer which feeds the archiver.

    <item*|<source-link|Edit/Modify/edit_modify.cpp|src/Edit/Modify/edit_modify.cpp>>The
    editor side: <cpp|start_editing>, <cpp|end_editing>,
    <cpp|archive_state>, <cpp|undo>, <cpp|redo> and the save state of a
    view.

    <item*|<source-link|Edit/Interface/edit_keyboard.cpp|src/Edit/Interface/edit_keyboard.cpp>,
    <source-link|edit_mouse.cpp|src/Edit/Interface/edit_mouse.cpp>,
    <source-link|edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>>The
    event handlers which delimit user actions, and the markers of keyboard
    shortcuts.

    <item*|<source-link|utils/library/tree.scm|TeXmacs/progs/utils/library/tree.scm>,
    <source-link|texmacs/menus/edit-menu.scm|TeXmacs/progs/texmacs/menus/edit-menu.scm>,
    <source-link|utils/plugins/plugin-eval.scm|TeXmacs/progs/utils/plugins/plugin-eval.scm>>The
    <scheme> side: <scm|try-modification>, the <menu|Edit|Undo> and
    <menu|Edit|Redo> menus, and the authors of plug-in output.
  </description-paragraphs>

  <\traverse>
    <branch|Modifications and patches|undo-patches.en.tm>

    <branch|The archiver|undo-archiver.en.tm>

    <branch|The <scheme> interface and how-to|undo-scheme.en.tm>

    <branch|Pitfalls and known problems|undo-pitfalls.en.tm>
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
