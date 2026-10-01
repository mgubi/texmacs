<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|How the rest of TeXmacs uses the DRD>

  <section|Overview>

  The <abbr|DRD> is consulted in three ways: through the member
  <cpp|drd> of the editor (code in <verbatim|Edit/>), through
  <cpp|env-\<gtr\>drd> (the typesetter and the evaluator), and through the
  global <cpp|the_drd> (generic tree code, languages and converters, and
  all <scheme> glue). This page lists the main call sites by subsystem, as
  a map for developers who change a property and want to know what will
  be affected. Paths are relative to <verbatim|src/src/>.

  <section|Cursor movement>

  The notion of a valid cursor position is entirely <abbr|DRD>-driven.

  <\description>
    <item*|<verbatim|Data/Tree/tree_cursor.cpp>><cpp|is_accessible_cursor
    (t, p)> decides whether the path <cpp|p> is a valid cursor position in
    <cpp|t>. It refuses positions on child-enforcing tags
    (<cpp|is_child_enforcing>) and at the inner borders of
    parent-enforcing tags (<cpp|is_parent_enforcing>, with
    <cpp|lowest_accessible_child> and <cpp|highest_accessible_child>),
    descends only into accessible children, switches to source access
    mode inside children whose environment has <verbatim|mode=src> (the
    arguments of <markup|value>, <markup|arg>, <markup|symbol>, ...), and
    maintains the writable mode according to <cpp|get_writability_child>.
    <markup|active>, <markup|inactive> and their starred versions switch
    the access mode for their body (<cpp|is_modified_accessible>).
    <cpp|closest_accessible> and friends use the same predicates.

    <item*|<verbatim|Data/Tree/tree_traverse.cpp>>The abstract cursor
    movements (<cpp|next_valid>, <cpp|previous_valid>, word and argument
    movement such as <cpp|move_argument>, <cpp|next_tag>,
    <cpp|previous_tag>, <cpp|next_argument>, ...) skip inaccessible
    children; <cpp|move_valid_bis> temporarily switches to source mode
    when the starting path is itself inaccessible.

    <item*|<verbatim|Edit/Interface/edit_cursor.cpp>><cpp|make_cursor_accessible>
    moves an inaccessible cursor to the closest valid position, in source
    access mode if the document is in source mode.
    <cpp|edit_interface_rep::resume> calls it with <cpp|the_drd> set to the
    editor <abbr|DRD>.
  </description>

  <section|Structured editing>

  <\description>
    <item*|<verbatim|Edit/Modify/edit_dynamic.cpp>>

    <\itemize>
      <item><cpp|make_compound (l, n)> chooses the smallest admissible
      arity when none is given (<cpp|correct_arity>,
      <cpp|get_arity_mode>), puts the cursor in the first accessible child,
      lets <scheme> handle with-like tags (<cpp|is_with_like>), inserts the
      tag in <markup|inactive> form if not all its children are accessible
      (<cpp|all_accessible>), and shows a message with <cpp|get_name>
      which mentions <verbatim|A-right> for variable arities.

      <item><cpp|find_dynamic> looks for the innermost ancestor with a
      variable arity (<cpp|is_dynamic>);
      <cpp|insert_argument> and <cpp|remove_empty_argument> use
      <cpp|insert_point> and <cpp|correct_arity> to insert or delete whole
      groups of children (for instance a row of a <markup|tree> or a
      variable/value pair of a <markup|with>). In source mode, these
      checks are skipped for tags which the <abbr|DRD> does not know
      (<cpp|contains>).

      <item><cpp|go_to_argument> deactivates a tag before moving into an
      inaccessible argument, and <cpp|activate> decides where to put the
      cursor after activation.

      <item><cpp|make_hybrid> and the activation of <markup|hybrid> trees
      use <cpp|contains (name)> to recognize names of primitives.
    </itemize>

    <item*|<verbatim|Edit/Modify/edit_delete.cpp>><cpp|remove_structure_upwards>
    treats macros without border (<cpp|var_without_border>) like
    <markup|concat>.

    <item*|<verbatim|Edit/Replace/edit_select.cpp>><cpp|select_enlarge>
    and <cpp|selection_adjust_border> (<verbatim|Data/Tree/tree_select.cpp>)
    enlarge selections which would end at an invisible border;
    <cpp|semantic_root> uses the environment of the first child
    (<cpp|get_env>) to find the enclosing mathematical or program
    fragment for semantic selections.
  </description>

  <section|Search, replace, spell checking and completion>

  <\description>
    <item*|<verbatim|Edit/Replace/edit_search.cpp>>Searching for tags
    (<cpp|search_previous_compound>, <cpp|search_next_compound>) only
    returns positions with <cpp|is_accessible_path>; the incremental search
    of <cpp|next_match> runs in <verbatim|DRD_ACCESS_HIDDEN> mode, or
    source mode in source documents.

    <item*|<verbatim|Data/Tree/tree_search.cpp>><cpp|is_accessible_for_search>
    accepts accessible children, the body of <markup|hidden>, and in source
    mode all children except <markup|raw-data>.

    <item*|<verbatim|Data/Tree/tree_spell.cpp>>Spell checking descends into
    accessible children and follows the <src-var|mode> and
    <src-var|language> of each child (<cpp|get_env_child (t, i, MODE,
    mode)>, <cpp|get_env_child (t, i, LANGUAGE, lan)>), so that formulas
    and program code are skipped and foreign-language fragments are checked
    with the right dictionary.

    <item*|<verbatim|Edit/Interface/edit_complete.cpp>>Word completion
    collects words in accessible children only.
  </description>

  <section|User interface>

  <\description>
    <item*|<verbatim|Edit/Interface/edit_footer.cpp>>The footer names the
    tags around the cursor with <cpp|get_name>, and recognizes primitives
    with <cpp|contains> when completing <markup|hybrid> commands.

    <item*|<verbatim|Edit/Interface/edit_interface.cpp>><cpp|compute_env_rects>
    does not draw context rectangles around child-enforcing tags.

    <item*|Focus bar and menus>On the <scheme> side
    (<verbatim|generic/generic-menu.scm>), the variant menus use the tag
    names (<scm|tree-name>), and the input fields for the
    \Phidden\Q, <abbr|i.e.> inaccessible, children of the focus tag are
    built from <scm|tree-accessible-child?>, <scm|tree-child-type>,
    <scm|tree-child-name> and <scm|tree-child-long-name>; see <hlink|the
    DRD from Scheme|drd-scheme.en.tm>.
  </description>

  <section|Environment at the cursor>

  <cpp|edit_typeset_rep::typeset_exec_until> computes the environment at
  an arbitrary path by executing the document up to that path. When it
  descends from a tree into a child, it applies the child environment from
  the <abbr|DRD> (<cpp|get_env_child (t, i, tree (ATTR))>) and
  <em|evaluates> its values with <cpp|env-\<gtr\>exec>; this is what makes
  <scm|(get-env "mode")> return <verbatim|math> inside the body of a user
  macro which wraps its argument in math mode. If the child environment is
  empty (child outside the layout), the descent stops. The same
  environment is used by <cpp|drd_update>.

  <section|Typesetting and evaluation>

  <\description>
    <item*|<verbatim|Typeset/Bridge/bridge_compound.cpp>>When a macro is
    typeset, a marker box for cursor positioning around it is inserted
    unless the macro is child-enforcing (<cpp|is_child_enforcing> on
    <cpp|the_drd>).

    <item*|<verbatim|Typeset/Env/env_inactive.cpp>>In source mode and for
    inactive markup, every argument is wrapped in <markup|src-regular>,
    <markup|src-var>, <markup|src-length>, ... according to
    <cpp|get_type_child> (function <cpp|highlight>).

    <item*|<verbatim|Typeset/Concat/concater.cpp>>The flags shown for
    vertical spaces and similar invisible primitives use <cpp|get_name>.

    <item*|<verbatim|Typeset/Concat/concat_graphics.cpp>>Graphical
    constraints are recognized by the type of the tag
    (<verbatim|TYPE_CONSTRAINT>).

    <item*|<verbatim|Typeset/Env/env_exec.cpp>>When the evaluator expands
    a macro application to find an accessible subtree
    (<cpp|edit_env_rep::expand> with <cpp|search_accessible>), it returns
    the first child which is both accessible according to the
    <abbr|DRD> and attached to the source. The experimental evaluator has
    the same logic in <verbatim|Style/Evaluate/evaluate_macro.cpp>.

    <item*|<verbatim|Data/Tree/tree_cache.cpp>>Only images with regular
    type are cached for client/server communication.
  </description>

  <section|Correction, analysis and languages>

  <\description>
    <item*|<verbatim|Data/Tree/tree_modify.cpp>><cpp|correct_node>
    replaces trees with a wrong arity by the empty string (for tags which
    the <abbr|DRD> describes).

    <item*|<verbatim|Data/Tree/tree_correct.cpp>><cpp|drd_correct (drd,
    t)> does the same recursively for an explicit <abbr|DRD>. The
    correctors for superfluous <markup|with>, invisible operators,
    homoglyphs and missing <markup|document> tags track the mode of each
    child through <cpp|get_env_child> and decide which children may be
    rewritten by their type (<cpp|is_correctable_child> in
    <verbatim|tree_analyze.cpp>). They run under
    <cpp|with_drd drd (get_document_drd (t))>.

    <item*|<verbatim|Data/Tree/tree_brackets.cpp>>,
    <verbatim|tree_math_stats.cpp>Bracket upgrading and math statistics
    follow the <src-var|mode> of children.

    <item*|<verbatim|Data/Tree/tree_analyze.cpp>><cpp|is_with_like>;
    <cpp|symbol_type> classifies symbols of mathematical formulas using
    <cpp|get_syntax>.

    <item*|<verbatim|System/Language/packrat_serializer.cpp>>The packrat
    parser serializes formulas for parsing; a tag with a <verbatim|syntax>
    attribute, or a macro, is serialized as its syntax (<cpp|get_syntax
    (t, p)>), which is how user macros take part in mathematical
    grammar checking.

    <item*|<verbatim|System/Language/dictionary.cpp>>The translation of
    menu and dialog texts (<cpp|tree_translate>) only translates accessible
    children.
  </description>

  <section|Converters>

  <\description>
    <item*|<verbatim|Data/Convert/Verbatim/verbatim.cpp>>Conversion to
    plain text outputs accessible children only (<cpp|the_drd>), and
    uses <cpp|std_drd-\<gtr\>get_env_child> for the mode.

    <item*|<verbatim|Data/Convert/Tex/fromtex_post.cpp>>The result of
    <LaTeX> import is cleaned with <cpp|drd_correct (std_drd, t)>.

    <item*|<verbatim|Data/Convert/AI/compress.cpp>><cpp|compress_tree>
    only rewrites accessible children in text mode and the current
    language.

    <item*|<scheme> converters>The <LaTeX> exporter computes the mode of
    every subtree with <scm|tree-child-env> (<scm|compute-mode-stats> in
    <verbatim|convert/latex/tmtex.scm>); most converters otherwise rely on
    <scheme> tables declared with the logic engine rather than on the
    <c++> <abbr|DRD>.
  </description>

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
