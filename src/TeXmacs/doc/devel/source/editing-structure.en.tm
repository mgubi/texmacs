<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Inserting, deleting and making structure>

  <section|Shape of the edit tree>

  The structured operations rely on a few invariants of the edit tree,
  which they both assume and restore:

  <\itemize>
    <item>A block of paragraphs is a <markup|document> node whose children
    are paragraphs; the body of a buffer is such a node.

    <item>A paragraph which contains several pieces is a <markup|concat>
    node. A <markup|concat> never has zero or one child, never contains an
    empty string, never contains two adjacent strings, never contains a
    nested <markup|concat>, and never contains a multi-paragraph object
    when it is the child of a <markup|document> (such objects are moved
    into a paragraph of their own).

    <item>The cursor is a path whose last item is a position: a character
    offset inside a string, or 0 (before) and 1 (after) for a compound tree.
  </itemize>

  The routine which re-establishes the <markup|concat> invariants is
  <cpp|edit_text_rep::correct_concat (p, done)>
  (<verbatim|Edit/Modify/edit_text.cpp>). Starting from the child
  <cpp|done>, it removes empty strings, joins adjacent strings, flattens
  nested <markup|concat> nodes, splits off multi-paragraph children into
  separate paragraphs, and replaces a <markup|concat> of arity 0 or 1 by
  the empty string or its only child. Since every step is an elementary
  modification, positions and the cursor survive. <cpp|correct (p)> calls
  it on a <markup|concat>, and moves up when the tree at <cpp|p> became
  the empty string. Most operations end with a call to <cpp|correct> or
  <cpp|correct_concat> on the parent of what they changed.

  <section|Inserting>

  <paragraph|<cpp|insert_tree (t, p_in_t)>.>This is the workhorse of all
  insertions (<scm|cpp-insert-go-to>, <scm|insert-raw-go-to>, and via
  <scm|insert> in <verbatim|utils/library/cpp-wrap.scm>). It inserts the
  tree <cpp|t> at the cursor and puts the cursor at the position
  <cpp|p_in_t> inside the inserted copy. Unless the look and feel is
  <name|Emacs>, it first deletes the selection (<cpp|selection_cut
  ("none")>). Then:

  <\itemize>
    <item>a string inserted inside a string is inserted directly;

    <item>a <markup|document> (several paragraphs) is inserted by breaking
    the current paragraph with <cpp|insert_return>, inserting the
    paragraphs between the two halves and joining the first and last
    inserted paragraphs with their neighbours (<cpp|remove_return>);

    <item>a multi-paragraph object which is not a <markup|document> (a
    theorem, a list, ...) is put in a new paragraph after the current one
    (<cpp|make_return_after>);

    <item>everything else is inserted inline: <cpp|prepare_for_insert>
    makes sure that the cursor is inside a <markup|concat>, splitting the
    current string if needed, the tree is inserted as children of that
    <markup|concat>, and <cpp|correct_concat> normalizes the result.
  </itemize>

  The one argument version <cpp|insert_tree (t)> (<scm|cpp-insert>)
  normalizes <cpp|t> with <cpp|simplify_correct> and puts the cursor at its
  end. <cpp|var_insert_tree> (<scm|cpp-insert-go-to>) wraps the selection:
  it cuts the selection to the <verbatim|primary> clipboard, inserts the
  tree and pastes the selection back at the new cursor position.

  <paragraph|Paragraph breaks.><cpp|insert_return> (<scm|insert-raw-return>)
  splits the current paragraph at the cursor. Whether a paragraph break is
  allowed at all is decided by <cpp|accepts_return>: directly inside a
  <markup|document>, in the last argument of <markup|with>, <markup|locus>,
  <markup|style-with>, <markup|canvas>, macro bodies, floats and extensions
  (provided the cursor is on a \Ppure line\Q, see <cpp|pure_line>), and in
  a few other places. If needed, a <markup|document> node is inserted
  around the current line first. <cpp|insert_return> returns <cpp|true> on
  <em|failure>. <cpp|remove_return (p)> joins the paragraph <cpp|p> with
  the next one.

  The two routines <cpp|make_return_before> and <cpp|make_return_after> of
  <verbatim|edit_dynamic.cpp> (<scm|make-return-before>,
  <scm|make-return-after>) move the cursor to the start or the end of the
  enclosing paragraph and insert a paragraph break there; they are used to
  put block content in a paragraph of its own.

  <paragraph|Spaces and images.><cpp|make_space>, <cpp|make_hspace>,
  <cpp|make_vspace_before>, <cpp|make_vspace_after> and <cpp|make_htab>
  insert the corresponding primitives. When the cursor is right after a
  space of the same kind and with the same arity, the one argument
  versions <em|add> the new width to the existing one instead of inserting
  a second space, so that pressing a spacing shortcut repeatedly widens a
  single space. <cpp|make_image (file, link, w, h, x, y)> inserts an
  <markup|image>, either as a link to the file or with the file contents
  embedded as <markup|raw-data>.

  <section|Making compound structures>

  <paragraph|<cpp|make_compound (l, n)>.>This is the generic constructor
  behind <scm|make> (<scm|cpp-make>, <scm|cpp-make-arity>), in
  <verbatim|edit_dynamic.cpp>. Given a tag and an optional arity it

  <\enumerate>
    <item>determines the smallest correct arity from the <abbr|DRD> if none
    is given, and the first accessible child, where the cursor will go;

    <item>for <markup|with>-like tags, gives the <scheme> function
    <scm|with-like-check-insert> a chance to handle the insertion;

    <item>inspects the macro definition of the tag in the current
    environment, to know whether it is a block macro (its body is a
    multi-paragraph construct), a \Plarge\Q macro, or a table macro (its
    body contains a <markup|tformat> whose last argument is a macro
    argument);

    <item>takes the current selection as the content of the new tag when it
    is small (or, for large macros, when it is any non table selection);

    <item>for block macros and for footnotes and folded comments, starts the
    first argument with an empty <markup|document>;

    <item>wraps the tag in <markup|inactive> if some children are not
    accessible (except in documents whose initial mode is <verbatim|src> and in the
    preamble), so that the
    user can fill in all arguments and then activate it with
    <key|return>;

    <item>inserts the result with <cpp|insert_tree>, creates a 1 by 1 table
    with <cpp|make_table> for table macros, and inserts the cut selection.
  </enumerate>

  It also sets a footer message which explains how to insert arguments or
  activate the tag. On the <scheme> side, <scm|make> has several
  overloads in <verbatim|utils/library/cpp-wrap.scm>: <markup|with>-like
  tags and inline tags wrap the selection themselves, and tags listed by
  <scm|make-wrapped-tag-list> cut the selection to a temporary clipboard
  and paste it inside.

  <paragraph|Activation and arguments.><cpp|activate> (<scm|activate>)
  removes the innermost <markup|inactive> around the cursor; an inactive
  <markup|compound> with a literal name is turned into the tag of that
  name. <scheme> is notified with <scm|notify-activated>.
  <cpp|insert_argument (forward)> and <cpp|remove_argument (forward)>
  (<scm|insert-argument>, <scm|remove-argument>, and the variants with a
  path) add or remove arguments of the innermost <em|dynamic> tag (one
  whose arity may vary, as told by <cpp|drd-\<gtr\>is_dynamic>), at a
  position allowed by <cpp|drd-\<gtr\>insert_point> and with an arity
  allowed by <cpp|drd-\<gtr\>correct_arity>; in source mode, arguments can
  be inserted anywhere. These are the operations behind the generic
  <scm|structured-insert-horizontal> and
  <scm|structured-remove-horizontal> for tags without a more specific
  overload.

  <paragraph|<markup|with> and <markup|style-with>.><cpp|make_with (var,
  val)> wraps the selection in a <markup|with>, first removing older
  changes of the same variable inside the selection
  (<cpp|remove_changes_in>), or inserts an empty <markup|with>.
  <cpp|insert_with (p, var, val)> and <cpp|remove_with (p, var)>
  (<scm|path-insert-with>, <scm|path-remove-with>) add, change or remove a
  variable of the <markup|with> at <cpp|p> or of an enclosing one.
  <cpp|make_style_with> and <cpp|make_mod_active> play the same role for
  <markup|style-with> and for <markup|active>, <markup|inactive> and their
  variants. On the <scheme> side, <scm|make-with> sets a cell format
  instead when a table selection is active.

  <paragraph|Hybrid commands.>Typing a backslash inserts a
  <markup|hybrid> tag (<verbatim|generic/generic-kbd.scm>) (<cpp|make_hybrid>, <scm|cpp-make-hybrid>) in which
  the user types a name. <cpp|activate_hybrid> (<scm|activate-hybrid>)
  then decides what it means: a <LaTeX> command known to the keyboard
  tables (<cpp|activate_latex>, which looks the name up with
  <cpp|kbd_get_command> and runs the command), an argument of the
  enclosing macro definition (inserted as <markup|arg>), a primitive or
  macro (inserted with <cpp|make_compound>), or an environment variable
  (inserted as <markup|value>). <cpp|activate_symbol> does the same for
  <markup|symbol> tags.

  <paragraph|Mathematics.>The constructors of <verbatim|edit_math.cpp>
  (<cpp|make_fraction>, <cpp|make_sqrt>, <cpp|make_var_sqrt>,
  <cpp|make_script>, <cpp|make_lprime>, <cpp|make_rprime>,
  <cpp|make_below>, <cpp|make_above>, <cpp|make_wide>,
  <cpp|make_wide_under>, <cpp|make_neg>, <cpp|make_rigid>,
  <cpp|make_tree>) all follow the same pattern: if a small selection is
  active, it becomes the first argument (and the cursor goes to the second
  one); otherwise an empty construct is inserted with the cursor in its
  first argument, and a footer message tells how to proceed. Primes are
  accumulated: typing a prime right after a prime extends the existing
  <markup|rprime>. <cpp|make_script> does nothing in an empty script and
  moves to the end of an existing script of the same kind. The <scheme>
  wrappers are in <verbatim|utils/library/cpp-wrap.scm> and are called by
  the math keyboard and menus (<verbatim|math/math-kbd.scm>,
  <verbatim|math/math-menu.scm>); the semantic math editor in
  <verbatim|math/math-sem-edit.scm> overrides some of them.

  <section|Deleting>

  <subsection|The deletion point>

  <cpp|remove_text (forward)> (<scm|remove-text>) is the default action of
  backspace and delete. It is implemented in <verbatim|edit_delete.cpp> as
  <cpp|remove_text_sub>, followed by <cpp|empty_document_fix>, which
  inserts an empty paragraph if the document no longer contains any
  accessible position.

  <cpp|get_deletion_point (p, last, rix, t, u, forward)> first determines
  what is to be deleted. For a forward deletion at the end of a piece of a
  <markup|concat>, it moves to the start of the next piece. Then it climbs
  out of formatting nodes (<markup|concat>, <markup|document>, ...) as long
  as the cursor is at their border, so that <cpp|t> is the innermost tree
  in which the deletion takes place, <cpp|last> the position of the cursor
  in <cpp|t>, <cpp|rix> its rightmost position, and <cpp|u> the parent of
  <cpp|t>.

  <subsection|Dispatch>

  <cpp|remove_text_sub> then distinguishes the following cases.

  <\description>
    <item*|Paragraph boundaries>When <cpp|t> is a <markup|document> and the
    cursor is between two paragraphs, the paragraphs are joined
    (<cpp|remove_return>), except when one of them is a multi-paragraph or
    sectional object, in which case an empty paragraph is removed or the
    cursor moves into the neighbour. At the border of the whole
    <markup|document> the parent decides: an empty footnote, float or
    folded comment disappears, an empty <markup|with> or extension is
    removed, a non empty extension is skipped, and tables and graphical
    text get their own treatment.

    <item*|Text>Inside a string, one character is removed (taking
    <TeXmacs> symbols like <verbatim|\<less\>alpha\<gtr\>> as single
    characters).

    <item*|Entering a tag>When the cursor is just before (forward) or
    after (backward) a compound tree, the tag decides: monolithic objects
    (spaces, tabs, <markup|raw-data>, big operators, <markup|left>,
    <markup|mid>, <markup|right>, and <markup|value> or <markup|arg>
    with a single argument) are deleted at once
    (<cpp|back_monolithic>); for <markup|around>, the bracket is replaced by
    an invisible one (<cpp|back_around>); for primes, one prime is removed
    (<cpp|back_prime>); for <markup|wide>, <markup|with> and <markup|locus>
    the cursor just enters the tag; for tables, <cpp|back_table> moves into
    the first or last cell; everything else goes to <cpp|back_general>,
    which in source mode turns an extension into an editable
    <markup|compound> and otherwise moves into the nearest argument.

    <item*|Leaving an argument>When the cursor is at the start (backward)
    or end (forward) of an argument, the parent <cpp|u> decides, through
    <cpp|back_in_around>, <cpp|back_in_long_arrow>, <cpp|back_in_wide>,
    <cpp|back_in_tree>, <cpp|back_in_table>, <cpp|back_in_with> and
    <cpp|back_in_general>. The common idea is that deleting in an empty
    argument removes that argument if the arity allows it
    (<cpp|remove_empty_argument>), and removes the whole tag when all its
    arguments are empty; otherwise the cursor moves to the neighbouring
    argument or out of the tag.
  </description>

  This is why, in <TeXmacs>, backspace never destroys a structure with
  content in one keystroke: it first moves into the structure, and only
  deletes it once it has been emptied. The <scheme> layer refines this
  per context: <scm|kbd-remove> is overloaded for sessions, programs,
  folding environments, databases, literate programs and the semantic
  math editor, and only the generic fallback calls <scm|remove-text> (or
  cuts the selection when one is active).

  <subsection|Removing structure>

  <cpp|remove_structure_upwards> (<scm|remove-structure-upwards>, the
  <em|Delete> entry of the focus tag menu in
  <verbatim|generic/generic-menu.scm>, and the action of several
  <scm|kbd-remove> overloads) removes the innermost non formatting tag
  around the cursor while keeping the argument that contains the cursor:
  the other arguments are deleted, the tag node is removed with
  <cpp|remove_node>, and the result is spliced into the surrounding
  <markup|document> if both are paragraph blocks. For tables, cells and
  similar constructs which have no border of their own, it recurses so
  that the whole table disappears. <cpp|remove_structure (forward)>
  (<scm|remove-structure>) removes a word or the neighbouring structure;
  it is exported but not used by the standard <scheme> code.

  <section|Pitfalls>

  <\itemize>
    <item><cpp|insert_return> and <cpp|make_return_after> return
    <cpp|true> on <em|failure>; <cpp|make_return_before> returns the
    result of <cpp|insert_return>.

    <item>Most operations act at the cursor <cpp|tp> and use the
    environment at the cursor (<cpp|get_env_value>), which is only valid
    after typesetting. <cpp|edit_dynamic_rep::in_source> even caches the
    mode in a static variable while an argument is being removed
    (<cpp|env_locked>), because the environment cannot be recomputed in
    the middle of a modification.

    <item>Merging spaces ignores the unit of the new space. In
    <cpp|edit_text_rep::make_space (tree u)>
    (<verbatim|Edit/Modify/edit_text.cpp:293>) the unit of each argument of
    the existing space is compared with the unit of its <em|own> first
    argument (<cpp|get_unit (t[0])>) instead of with the unit of the new
    space <cpp|u[i]>, so the test always succeeds for spaces of the same
    sign. Inserting a horizontal space of <verbatim|1cm> right after one of
    <verbatim|0.5em> therefore produces a single space of
    <verbatim|1.5em>.

    <item><cpp|remove_structure> reads an uninitialized variable in the
    backward direction: the inner declaration <cpp|int pos= max (start-1,
    0)> (<verbatim|Edit/Modify/edit_delete.cpp:329>) shadows the outer
    <cpp|pos>, and the following <cpp|end= pos> uses the outer, never
    assigned one. The routine is only reachable through the glue
    (<scm|remove-structure>).

    <item><cpp|make_return_before> compares the number of paragraphs with
    <cpp|q-\<gtr\>item+1> (<verbatim|Edit/Modify/edit_dynamic.cpp:646>),
    that is, with the <em|first> item of the path (the index of the buffer
    in the global tree) instead of the index of the paragraph
    (<cpp|last_item (q)>). The test whether the cursor is in the last
    paragraph is therefore wrong in general.

    <item>The <scheme> wrapper <scm|(make-script r? sup?)>
    (<verbatim|utils/library/cpp-wrap.scm:93>) names its parameters in the
    wrong order: they are passed unchanged to <cpp|make_script (sup,
    right)>, and all callers indeed use the order <em|superscript?,
    right?>.
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
