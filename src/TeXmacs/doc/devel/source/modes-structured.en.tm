<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The generic structured editing hooks>

  <section|The focus tree and outward recursion>

  Most editing commands act on the <em|focus tree>, returned by
  <scm|(focus-tree)> (<verbatim|kernel/library/tree.scm>), which is the tree
  at the path <scm|(get-focus-path)> computed by the editor
  (<cpp|edit_select_rep::focus_get> in
  <verbatim|Edit/Replace/edit_select.cpp>):

  <\itemize>
    <item>if a focus has been set explicitly (for instance by clicking on a
    tag in the focus bar), that tree;

    <item>otherwise, if there is a selection, the tree around the
    selection;

    <item>otherwise, the innermost tag around the cursor.
  </itemize>

  In the last two cases the editor skips the purely structural tags
  <markup|document>, <markup|concat>, <markup|tformat>, <markup|table>,
  <markup|row>, <markup|cell>, <markup|shown>, <markup|hidden> and a few
  others, so that inside a table the focus is the table environment
  (such as <markup|tabular>) and inside a list it is the list environment.

  Generic commands are written as functions of the focus tree with a
  default implementation which recurses to the parent, and a base case for
  the buffer itself:

  <\scm-code>
    (tm-define (kbd-enter t shift?)

    \ \ (and-with p (tree-outer t)

    \ \ \ \ (kbd-enter p shift?)))

    \;

    (tm-define (kbd-enter t shift?)

    \ \ (:require (tree-is-buffer? t))

    \ \ (insert-return))

    \;

    (tm-define (kbd-return)

    \ \ (kbd-enter (focus-tree) #f))
  </scm-code>

  (<verbatim|generic/generic-edit.scm>; <scm|tree-outer> returns the parent
  except for the buffer tree). A mode handles a tag by a conditional
  redefinition for that tag; the first enclosing tag which has a specific
  definition handles the command. This pattern, introduced in <hlink|the
  <TeXmacs> editing model|../scheme/edit/edit-model.en.tm>, is used for all
  the hooks below.

  <section|The hooks>

  <subsection|Keyboard>

  The keys of <verbatim|generic/generic-kbd.scm> call parameterless
  commands, which call the following hooks on the focus tree:

  <\description-paragraphs>
    <item*|<scm|(kbd-enter <scm-arg|t> <scm-arg|shift?>)>><key|return> and
    <key|S-return> (with <scm|kbd-control-enter> and
    <scm|kbd-alternate-enter> for the modified variants). The default
    inserts a paragraph break.

    <item*|<scm|(kbd-space-bar <scm-arg|t> <scm-arg|shift?>)>>The space bar.

    <item*|<scm|(kbd-remove <scm-arg|t> <scm-arg|forwards?>)>><key|backspace>
    and <key|delete>; the default removes text, or the selection.

    <item*|<scm|(kbd-variant <scm-arg|t> <scm-arg|forwards?>)>><key|tab>
    and <key|S-tab> (tab completion; for labels and citations, completion of
    the key). <scm|kbd-alternate-variant> is used for <key|A-tab> and by
    default inserts a horizontal tab.

    <item*|<scm|kbd-horizontal>, <scm|kbd-vertical>, <scm|kbd-extremal>,
    <scm|kbd-incremental>>Cursor movements with the arrow keys,
    <key|home>/<key|end> and <key|pageup>/<key|pagedown>; sessions redefine
    them to move between input fields.
  </description-paragraphs>

  In addition, the unconditional entry points <scm|kbd-insert> (insertion of
  a character or shorthand), <scm|kbd-backspace> and <scm|kbd-delete> may be
  redefined under a mode, as the semantic math mode does.

  <subsection|Structured insertion, removal and movement>

  These hooks implement the <key|structured:insert ...>,
  <key|structured:move ...> and <key|structured:cmd ...> shortcuts and the
  corresponding icons of the focus bar:

  <\description-paragraphs>
    <item*|<scm|(structured-insert-horizontal <scm-arg|t>
    <scm-arg|forwards?>)>, <scm|structured-insert-vertical>,
    <scm|structured-remove-horizontal>, <scm|structured-remove-vertical>>Insert
    or remove a column, a row, an argument, a branch, ... next to the
    current one. Tables insert and remove columns and rows; switches and
    overlays insert and remove branches; a subscript and a superscript are
    completed into a script pair.

    <item*|<scm|structured-horizontal>, <scm|structured-vertical>,
    <scm|structured-inner-extremal>, ...>Move to the next or previous
    argument, cell, branch, ...

    <item*|<scm|traverse-horizontal>, <scm|traverse-vertical>,
    <scm|traverse-incremental>, <scm|traverse-extremal>>Traversal of the
    document by structure (for instance from one session input field to
    the next).

    <item*|<scm|(variant-circulate <scm-arg|t> <scm-arg|forward?>)>>Cycle
    through the variants of a tag (<key|structured:cmd tab>); the default
    uses the <verbatim|variant-tag> groups (<verbatim|utils/edit/variants.scm>).

    <item*|<scm|(alternate-toggle <scm-arg|t>)>>Fold or unfold, toggle
    between two alternative forms (<key|C-*>); the default uses the pairs
    declared with <scm|define-alternate>.

    <item*|<scm|(numbered-toggle <scm-arg|t>)>>Toggle numbering
    (<key|C-#>); the default toggles the final <verbatim|*> of the tag name.

    <item*|<scm|geometry-horizontal>, <scm|geometry-vertical>,
    <scm|geometry-default>, ...>Change the position or size of the focus
    (spaces, brackets, table extents, animations; see
    <verbatim|generic/format-geometry-edit.scm>).

    <item*|<scm|structured-maximize>, <scm|structured-minimize>,
    <scm|swipe-horizontal>, <scm|swipe-vertical>>Gestures.
  </description-paragraphs>

  The predicates <scm|structured-horizontal?> and
  <scm|structured-vertical?> tell the generic code whether the horizontal
  and vertical commands make sense for a tree; by default the first holds for dynamic
  tags and tables, the second for <markup|tree> and tables.

  <subsection|Focus menus and focus icons>

  The <menu|Focus> menu and the focus icon bar are built by
  <scm|standard-focus-menu> and <scm|standard-focus-icons> in
  <verbatim|generic/generic-menu.scm> from a number of sub-menus, each of
  which takes the focus tree as argument and may be redefined for a tag:

  <\description>
    <item*|<scm|focus-ancestor-menu>, <scm|focus-ancestor-icons>>Entries
    for enclosing tags (for instance the document title around an author).

    <item*|<scm|focus-tag-menu>, <scm|focus-tag-icons>>The tag itself: its
    name, its variants (<scm|focus-variant-menu>), its toggles
    (<scm|focus-toggle-menu>), floats and animations, its preferences and
    rendering parameters, help and deletion.

    <item*|<scm|focus-move-menu>, <scm|focus-insert-menu>>Structured
    movements and insertions; shown only if <scm|focus-can-move?>
    <abbr|resp.> <scm|focus-can-insert-remove?> hold.

    <item*|<scm|focus-hidden-menu>, <scm|focus-extra-menu>,
    <scm|focus-label-menu>>Hidden arguments, mode specific extra entries,
    the label of the tag.
  </description>

  The parallel <verbatim|*-icons> menus build the icon bar. A mode may also
  replace <scm|standard-focus-menu> for some trees altogether, as
  <verbatim|table/table-menu.scm> does for tables.

  Several predicates and data functions feed these menus and are
  redefined per tag: <scm|focus-has-variants?>, <scm|focus-has-toggles?>,
  <scm|focus-can-move?>, <scm|focus-can-insert?>, <scm|focus-can-remove?>,
  <scm|focus-has-geometry?>, <scm|focus-has-parameters?>; and for the
  style parameters <scm|(standard-parameters <scm-arg|tag-name>)> (the
  environment variables which customize a tag),
  <scm|(customizable-parameters <scm-arg|t>)> (parameters stored in a
  <markup|with> around a particular tree) and <scm|(parameter-choice-list
  <scm-arg|var>)> (proposed values for a variable).

  <section|Adding behaviour for a new tag>

  To give a new tag <markup|my-env> its own behaviour, a module typically

  <\enumerate>
    <item>adds the tag to suitable groups (<scm|(define-group variant-tag
    ...)>, <scm|(define-alternate my-env my-env*)>, ...), so that the
    generic implementations of <scm|variant-circulate> and
    <scm|alternate-toggle> apply;

    <item>defines a context predicate, <scm|(tm-define (my-env-context? t)
    (tree-is? t 'my-env))>;

    <item>redefines the hooks it needs with <scm|(:require (my-env-context?
    t))>, for instance <scm|kbd-enter> or <scm|structured-insert-horizontal>;

    <item>redefines <scm|focus-extra-menu> and <scm|focus-extra-icons> with
    the same condition to add entries to the focus bar.
  </enumerate>

  Since the definitions are tried from the most recent one, the module must
  be loaded after the generic modules, which is ensured by a <scm|:use> of
  <verbatim|(generic generic-edit)> and <verbatim|(generic generic-menu)>.

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
