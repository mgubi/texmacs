<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Style and document DRDs>

  <section|Layers>

  The properties of a tag depend on the style: <markup|theorem> is a macro
  with one accessible argument in <verbatim|article>, and an unknown label
  in a document without style. Each <abbr|DRD> is therefore built in
  layers on top of <cpp|std_drd>, using the inheritance of
  <cpp|drd_info (name, base)>:

  <\enumerate>
    <item><cpp|std_drd>: built-in primitives and variables.

    <item>The style <abbr|DRD>: the macros and parameters of the style
    files and packages, computed once per style tuple.

    <item>The editor <abbr|DRD> (<cpp|editor_rep::drd>), which starts from
    the style <abbr|DRD> and is completed with the definitions in the
    initial environment and the preamble, and with those in the body of the
    document up to the cursor.
  </enumerate>

  Within a layer, properties come from two sources: the <em|heuristic
  inference> from the macro definitions, and explicit <markup|drd-props>
  declarations. The latter freeze the properties they set, so they always
  win, whatever the order in which both are executed.

  <section|The DRD of a style>

  <\explain>
    <cpp|bool compute_env_and_drd (tree style)><explain-synopsis|compute and
    memorize environment and DRD of a style>
  <|explain>
    Defined in <source-link|Data/Document/new_style.cpp|src/Data/Document/new_style.cpp>. The argument is a
    style tuple such as <verbatim|(tuple "article" "number-europe")>. The
    function creates <cpp|drd_info drd ("none", std_drd)> and an
    <cpp|edit_env> whose <cpp|drd> member refers to it. Then:

    <\itemize>
      <item>if the style is in the cache (see below), the cached
      environment is installed with <cpp|patch_env>, the cached
      <abbr|DRD> entries with <cpp|set_locals>, and the environment is
      recorded with <cpp|set_environment>;

      <item>otherwise the style is executed
      (<cpp|env-\<gtr\>exec (tree (USE_PACKAGE, A (style)))>), which
      evaluates all <markup|assign> and <markup|drd-props> of the style
      files (the latter act directly on <cpp|drd>), then the resulting
      environment is read back and <cpp|drd-\<gtr\>heuristic_init (H)>
      infers the properties of all its macros and variables.
    </itemize>

    The results are memorized in the global <cpp|style_data_rep>, in the
    maps <cpp|style_cached> and <cpp|drd_cached>. A set of \Pbusy\Q styles
    protects against recursion; it returns <cpp|false> if the style is
    already being computed.
  </explain>

  <\explain>
    <cpp|drd_info get_style_drd (tree style)>

    <cpp|hashmap\<less\>string,tree\<gtr\> get_style_env (tree
    style)><explain-synopsis|memoized access>
  <|explain>
    Return the memorized <abbr|DRD> or environment, computing them if
    needed. For a busy style, <cpp|get_style_drd> returns <cpp|std_drd> and
    <cpp|get_style_env> an empty environment.
  </explain>

  <\explain>
    <cpp|drd_info get_document_drd (tree doc)><explain-synopsis|DRD for a
    whole document tree>
  <|explain>
    Used by tree processing routines which are not attached to an editor.
    It takes the style of the document, gets its <abbr|DRD> and, if the
    document has a preamble (<markup|hide-preamble> or
    <markup|show-preamble>), creates a derived <cpp|drd_info
    ("preamble", ...)> in which the style and the preamble are executed
    and the heuristics are run again. For a tree without a
    <verbatim|TeXmacs> version attribute (typically a fragment), the
    current <cpp|the_drd> is used if it looks like a real document style
    is loaded (the <markup|theorem> tag has a syntax), and the
    <verbatim|generic> style otherwise. Used, through <cpp|with_drd>, by
    the correction routines of <source-link|Data/Tree/tree_correct.cpp|src/Data/Tree/tree_correct.cpp> and
    <source-link|tree_brackets.cpp|src/Data/Tree/tree_brackets.cpp>.
  </explain>

  <section|Caching>

  Computing the environment of a style means loading and executing all its
  packages, which is slow; it is therefore cached at two levels.

  <\description>
    <item*|In memory>The structure <cpp|style_data_rep> contains, per
    style tuple, the environment and the serialized local <abbr|DRD>
    entries (<cpp|style_cache>, <cpp|style_drd>) and the computed objects
    (<cpp|style_cached>, <cpp|drd_cached>).

    <item*|On disk><cpp|style_set_cache (style, H, t)> writes the pair
    <verbatim|(tuple H t)>, where <verbatim|t> is the result of
    <cpp|get_locals> (see <hlink|serialization|drd-model.en.tm>), to a file
    in <verbatim|$TEXMACS_HOME_PATH/system/cache> whose name is derived
    from the style tuple by <cpp|cache_file_name> (for instance
    <verbatim|__article__number-europe__>). The file is written only if it
    does not exist yet. <cpp|style_get_cache> looks in memory first and
    then on disk.
  </description>

  <cpp|style_invalidate_cache> discards the in-memory structure and
  deletes the files <verbatim|__*> in the cache directory. It is called by
  <cpp|tm_server_rep::style_clear_cache> (glue
  <scm|cpp-style-clear-cache>, <scheme> function <scm|style-clear-cache>,
  menu <menu|Tools|Update|Styles>), which then notifies all editors so
  that they recompute their environment. <scm|style-clear-cache> is also
  called automatically when a file with suffix <verbatim|.ts> is saved
  from <TeXmacs> (<source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>). Style files
  edited with another editor are not detected: the cache must then be
  cleared by hand.

  Since only the <em|local> entries are stored, a cached style <abbr|DRD>
  is only valid with the <cpp|std_drd> of the <TeXmacs> binary which
  produced it. The whole cache directory is emptied when the version
  changes (<cpp|init_upgrade>), after an interrupted start-up
  (<cpp|acquire_boot_lock>) and with the command line option
  <verbatim|-setup>.

  <section|The DRD of an editor>

  Each editor owns a <cpp|drd_info drd>, created by the constructor of
  <cpp|editor_rep> (<source-link|Edit/Editor/edit_main.cpp|src/Edit/Editor/edit_main.cpp>) as
  <cpp|drd_info (buf-\<gtr\>buf-\<gtr\>title, std_drd)>. Its typesetting
  environment <cpp|env> is constructed with a reference to this member, so
  that assignments to <cpp|drd> are seen by the environment, and
  <markup|drd-props> executed during typesetting modify the editor
  <abbr|DRD>.

  <\explain>
    <cpp|void edit_typeset_rep::typeset_style_use_cache (tree
    style)><explain-synopsis|install the style>
  <|explain>
    On a cache hit, the cached environment is patched into <cpp|env> and
    the cached entries are installed into the editor <abbr|DRD> with
    <cpp|set_locals>. On a miss, the environment and <abbr|DRD> are
    computed with <cpp|get_style_env> and <cpp|get_style_drd>, the cache is
    filled, and <cpp|drd> is <em|replaced> by the style <abbr|DRD> object
    (see the <hlink|pitfalls|drd-pitfalls.en.tm> for the consequences of
    this sharing).
  </explain>

  <\explain>
    <cpp|void edit_typeset_rep::typeset_preamble ()><explain-synopsis|environment
    and DRD at the start of the document>
  <|explain>
    Writes the default environment, installs the style, applies the
    initial environment of the document, reads back the resulting
    environment <cpp|pre> and runs <cpp|drd-\<gtr\>heuristic_init (pre)>.
    It is called lazily when the initial environment is first needed,
    before printing, from the conversion routines <cpp|exec_html> and
    <cpp|exec_latex>, and by <cpp|typeset_invalidate_all>, which is
    triggered by
    <cpp|notify_change (THE_ENVIRONMENT)> after any change of style,
    package or initial environment.
  </explain>

  <\explain>
    <cpp|void edit_typeset_rep::drd_update ()><explain-synopsis|take the
    document body into account>
  <|explain>
    Computes the environment just before the cursor
    (<cpp|typeset_exec_until (tp)>) and runs the heuristics on it, so that
    macros defined in the body of the document (before the cursor) get
    their properties. It is called at the end of
    <cpp|edit_interface_rep::apply_changes>, unless there are pending
    events (<cpp|gui_interrupted>); this is the \Psmall amount of free
    time\Q mentioned in the user documentation.
  </explain>

  Note that <cpp|set_locals> adds or overwrites entries but never removes
  any, and <cpp|heuristic_init> likewise only updates the properties of
  the variables present in its argument. The editor <abbr|DRD> can
  therefore retain properties of macros which are no longer defined (for
  instance after the deletion of a definition in the preamble, or after a
  style change on the cache-hit path), until the buffer is reopened.

  <section|The heuristics in brief>

  <cpp|drd_info_rep::heuristic_init (env)> iterates over all variables of
  an environment and dispatches on the value:

  <\description>
    <item*|<markup|macro>><cpp|heuristic_init_macro>: fixed arity equal to
    the number of parameters (<verbatim|CHILD_DETAILED>), type of the tag =
    type of the body, with-like flag if the body is a chain of with-like
    constructs ending in the last argument, child names = parameter names,
    and for each parameter the result of a search for <verbatim|(arg x)>
    in the body (<cpp|arg_access>), which determines whether the argument
    is accessible, its type, and its environment.

    <item*|<markup|xmacro>><cpp|heuristic_init_xmacro>:
    <verbatim|ARITY_REPEAT> with a minimal arity derived from the largest
    index used.

    <item*|anything else><cpp|heuristic_init_parameter>: arity zero,
    <verbatim|VAR_PARAMETER>, and a type guessed from the name or the
    value. Zero-parameter macros whose body is a length or a
    <markup|localize> become <verbatim|VAR_MACRO_PARAMETER>.
  </description>

  Because the properties of a macro depend on those of the macros it uses
  (accessibility propagates through <markup|with>, <markup|compound>,
  nested macro calls and the child environments of primitives), the loop
  is repeated until no record changes, and abandoned with the warning
  <verbatim|bad heuristic drd convergence> after ten rounds. The details,
  with examples, are in the <hlink|macro expansion
  chapter|macro-expansion-drd.en.tm>.

  The heuristics only use the <abbr|DRD> itself and the macro
  definitions; they never look at the boxes. A macro which uses an argument
  in a way the heuristics do not understand (inside <markup|extern>,
  <markup|merge>, a <markup|map-args> of an inaccessible tag, ...) gets an
  inaccessible argument, and a <markup|drd-props> declaration is needed.

  <section|Explicit declarations: <markup|drd-props>>

  <markup|drd-props> is evaluated by
  <cpp|edit_env_rep::exec_drd_props> (<source-link|Typeset/Env/env_exec.cpp|src/Typeset/Env/env_exec.cpp>)
  whenever the tree is executed: when a style is loaded, when the preamble
  is processed, or when a <markup|drd-props> in the body of a document is
  typeset (<cpp|concater_rep::typeset_drd_props>, which also displays a
  flag). Its arguments are not evaluated. The first argument is the tag
  name (possibly a pseudo-label <verbatim|extern:f>), followed by pairs of
  a property and a value:

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|3|3|cell-hyphen|t>|<table|<row|<cell|Property>|<cell|Value>|<cell|Effect>>|<row|<cell|<verbatim|arity>>|<cell|<math|n>,
  or <verbatim|\<less\>tuple\|repeat\|a\|b\<gtr\>>, <verbatim|repeat*>,
  <verbatim|options>>|<cell|<cpp|set_arity> with <verbatim|CHILD_DETAILED>
  for a number, <verbatim|CHILD_BIFORM> for a tuple; frozen>>|<row|<cell|<verbatim|name>>|<cell|string>|<cell|name
  attribute (not frozen)>>|<row|<cell|<verbatim|syntax>>|<cell|tree>|<cell|syntax
  attribute>>|<row|<cell|<verbatim|border>>|<cell|<verbatim|yes>,
  <verbatim|inner>, <verbatim|outer>, <verbatim|no>>|<cell|border mode;
  frozen>>|<row|<cell|<verbatim|with-like>>|<cell|<verbatim|yes>,
  <verbatim|no>>|<cell|with-like flag; frozen>>|<row|<cell|<verbatim|locals>>|<cell|<markup|attr>
  tree>|<cell|environment of all child records; frozen>>|<row|<cell|<verbatim|accessible>,
  <verbatim|hidden>, <verbatim|unaccessible>>|<cell|index,
  <verbatim|all>, <verbatim|none>>|<cell|accessibility; frozen
  (<verbatim|none> always means never accessible)>>|<row|<cell|<verbatim|normal-writability>,
  <verbatim|disable-writability>, <verbatim|enable-writability>>|<cell|index,
  <verbatim|all>>|<cell|writability; frozen>>|<row|<cell|<verbatim|returns>>|<cell|type
  name>|<cell|type of the tag; frozen>>|<row|<cell|<verbatim|parameter>,
  <verbatim|macro-parameter>>|<cell|type name>|<cell|variable kind and
  type; frozen>>|<row|<cell|a type name>|<cell|index,
  <verbatim|all>>|<cell|type of children; frozen>>>>>>
    Properties recognized by <markup|drd-props>.
  </big-table>

  Unknown properties and values are silently ignored. Some consequences of
  the implementation are worth knowing:

  <\itemize>
    <item>Indices are <em|physical> indices into the child records, not
    child numbers. For a fixed arity (<verbatim|CHILD_DETAILED>) both
    coincide; after <verbatim|arity\|\<less\>tuple\|repeat\|1\|1\<gtr\>>
    (<verbatim|CHILD_BIFORM>), index <math|0> is the fixed part and index
    <math|1> all the repeated children.

    <item>Since the child records are created by the arity, the
    <verbatim|arity> property must come first when the tag has no
    record yet (in particular for <verbatim|extern:f> pseudo-labels);
    otherwise index properties refer to non-existent records and are
    ignored.

    <item>The value <verbatim|no> of <verbatim|border> currently sets
    <verbatim|BORDER_INNER> instead of <verbatim|BORDER_NO> (see the
    <hlink|pitfalls|drd-pitfalls.en.tm>).

    <item>Type names are those accepted by <cpp|drd_encode_type>:
    <verbatim|regular>, <verbatim|adhoc>, <verbatim|variable>,
    <verbatim|argument>, <verbatim|boolean>, <verbatim|integer>,
    <verbatim|string>, <verbatim|length>, <verbatim|numeric>,
    <verbatim|code>, <verbatim|identifier>, <verbatim|url>,
    <verbatim|color>, <verbatim|graphical>, <verbatim|point>,
    <verbatim|constraint>, <verbatim|graphical-id>, <verbatim|effect>,
    <verbatim|animation>, <verbatim|duration>, <verbatim|font-size>,
    <verbatim|unknown> and <verbatim|error>.
  </itemize>

  Typical declarations in the standard style packages:

  <\tm-fragment>
    <inactive*|<drd-props|explain-macro|arity|<tuple|repeat|1|1>|accessible|all>>

    <inactive*|<drd-props|help-link|arity|2|accessible|0|url|1>>

    <inactive*|<drd-props|phantom|arity|1|accessible|none|syntax|<macro|body|>>>

    <inactive*|<drd-props|enunciation-sep|macro-parameter|string>>
  </tm-fragment>

  <section|The current DRD and the access modes>

  <subsection|<cpp|the_drd>>

  Most generic routines on trees (<verbatim|Data/Tree/>,
  <verbatim|System/Language/>, the converters) do not have access to an
  editor, and consult the global variable <cpp|the_drd> instead. It is
  set:

  <\itemize>
    <item>to the <abbr|DRD> of the editor of the current view, by
    <cpp|set_current_view> (<source-link|Texmacs/Data/new_view.cpp|src/Texmacs/Data/new_view.cpp>), which is
    called whenever the focus changes to another view;

    <item>to the <abbr|DRD> of the view of a given buffer, by
    <cpp|set_current_drd (url)> (glue <scm|set-drd>, used by
    <source-link|texmacs/texmacs/tm-print.scm|TeXmacs/progs/texmacs/texmacs/tm-print.scm> while printing);

    <item>temporarily, to the <abbr|DRD> of the editor of a window while its
    menus are computed (<cpp|tm_window_rep::get_menu_widget>), and to the
    editor <abbr|DRD> in <cpp|edit_interface_rep::resume>;

    <item>temporarily, with the helper <cpp|with_drd>, whose constructor
    saves <cpp|the_drd> and installs another <abbr|DRD> and whose destructor
    restores it:

    <\cpp-code>
      tree

      superfluous_with_correct (tree t) {

      \ \ with_drd drd (get_document_drd (t));

      \ \ return superfluous_with_correct (t, tree (WITH, MODE, "text"));

      }
    </cpp-code>
  </itemize>

  Code inside the editor uses the member <cpp|drd> directly, and the
  typesetter uses <cpp|env-\<gtr\>drd>; outside the editor,
  <cpp|the_drd> is the only way. When writing a new routine which may be
  called on a tree not belonging to the current buffer (for instance on a
  document being converted), wrap it in <cpp|with_drd> with
  <cpp|get_document_drd>.

  <subsection|Access and writable modes>

  Whether a child is \Paccessible\Q depends not only on the <abbr|DRD> but
  also on a global <em|access mode> (<source-link|Data/Drd/drd_mode.hpp|src/Data/Drd/drd_mode.hpp>):

  <\description>
    <item*|<verbatim|DRD_ACCESS_NORMAL>>The default: only
    <verbatim|ACCESSIBLE_ALWAYS> children are accessible.

    <item*|<verbatim|DRD_ACCESS_HIDDEN>>Hidden children are accessible too.
    Used by search (<cpp|edit_replace_rep::next_match>), so that matches in
    folded content are found.

    <item*|<verbatim|DRD_ACCESS_SOURCE>>Every child is accessible. Used in
    source mode (when the <src-var|mode> of the document is
    <verbatim|src>: <cpp|make_cursor_accessible>,
    <cpp|selection_correct>, <cpp|compute_env_rects>), for children whose
    environment has <verbatim|mode=src> (<cpp|is_accessible_cursor>),
    inside inactive markup (<cpp|is_modified_accessible>), and by
    <cpp|compute_selection> and <cpp|update_mouse_loci>.
  </description>

  <cpp|set_access_mode (mode)> returns the previous mode, and all callers
  follow the pattern

  <\cpp-code>
    int old_mode= set_access_mode (DRD_ACCESS_SOURCE);

    ...

    set_access_mode (old_mode);
  </cpp-code>

  The <em|writable mode> (<verbatim|DRD_WRITABLE_NORMAL>,
  <verbatim|DRD_WRITABLE_INPUT>, <verbatim|DRD_WRITABLE_ANY>, functions
  <cpp|set_writable_mode> and <cpp|get_writable_mode>) is maintained in
  the same way by <cpp|is_accessible_cursor> while it descends into
  children with disabled or enabled writability; see the <hlink|data
  model|drd-model.en.tm>. From <scheme>, the access mode is available as
  <scm|get-access-mode> and <scm|set-access-mode> (with the integer
  values <math|0>, <math|1>, <math|2>), as in <scm|tree-perform-search>
  (<source-link|generic/search-widgets.scm|TeXmacs/progs/generic/search-widgets.scm>).

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
