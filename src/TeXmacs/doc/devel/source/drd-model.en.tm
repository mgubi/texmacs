<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The DRD data model>

  <section|The class <cpp|drd_info>>

  <\explain>
    <cpp|class drd_info_rep><explain-synopsis|a data relation descriptor>
  <|explain>
    Declared in <verbatim|Data/Drd/drd_info.hpp>, with three data members:

    <\description>
      <item*|<cpp|string name>>A name used only for printing
      (<verbatim|"tm"> for the standard <abbr|DRD>, <verbatim|"none"> for
      style <abbr|DRD>s, the buffer title for editor <abbr|DRD>s,
      <verbatim|"preamble"> for document <abbr|DRD>s).

      <item*|<cpp|rel_hashmap\<less\>tree_label,tag_info\<gtr\> info>>The
      properties of each tag.

      <item*|<cpp|hashmap\<less\>string,tree\<gtr\> env>>The environment
      (typically, all macro definitions of the style) from which the
      heuristic properties were computed, set by <cpp|set_environment>. It
      is also used as a fallback by <cpp|get_syntax>.
    </description>

    <cpp|drd_info> is the usual reference counted handle
    (<cpp|CONCRETE>), so copying a <cpp|drd_info> shares the object.
  </explain>

  <\explain>
    <cpp|drd_info (string name)>

    <cpp|drd_info (string name, drd_info base)><explain-synopsis|constructors>
  <|explain>
    The second constructor creates a <abbr|DRD> which <em|inherits> from
    <cpp|base>. The member <cpp|info> is a <em|relative hash map>
    (<verbatim|Kernel/Containers/rel_hashmap.hpp>): a list of hash maps in
    which a lookup <cpp|info[l]> returns the entry of the first map that
    contains <cpp|l>, and an access for writing <cpp|info(l)> first copies
    the entry from the base into the local map. Hence modifications never
    leak into the base <abbr|DRD>. All <abbr|DRD>s are directly or
    indirectly built on top of <cpp|std_drd>.
  </explain>

  For a label which is not described anywhere in the chain, <cpp|info[l]>
  returns the default value of the hash map, <cpp|tag_info ()>: arity zero
  (<verbatim|ARITY_NORMAL>), no child records, type
  <verbatim|TYPE_REGULAR>, border <verbatim|BORDER_YES>. Code which needs to
  know whether a label is described at all uses
  <cpp|drd_info_rep::contains (string l)>, which looks through the whole
  chain. For instance, <cpp|correct_node> (<verbatim|Data/Tree/tree_modify.cpp>)
  and <cpp|edit_dynamic_rep::insert_argument> only enforce arities of
  described labels.

  All setters follow the same pattern: return immediately if the property
  is <em|frozen> (see below), make a local copy of the record if necessary,
  and modify it. Setters for children take a <em|physical index>
  <cpp|nr> into the array of child records (see <hlink|the child
  layout|#child-layout>), and silently do nothing if <cpp|nr> is out of
  range. The high-level queries (<cpp|get_type_child>,
  <cpp|is_accessible_child>, <cpp|get_writability_child>,
  <cpp|get_env_child>, <cpp|get_child_name>) take a tree and a <em|logical>
  child number instead, and do the translation themselves.

  <section|The class <cpp|tag_info>>

  <\explain>
    <cpp|class tag_info_rep><explain-synopsis|the properties of one tag>
  <|explain>
    A <cpp|tag_info> (<verbatim|Data/Drd/tag_info.hpp>) consists of

    <\description>
      <item*|<cpp|parent_info pi>>the properties of the tag itself;

      <item*|<cpp|array\<less\>child_info\<gtr\> ci>>the properties of the
      children, in a compressed layout;

      <item*|<cpp|tree extra>>either the empty string, or an
      <markup|attr> tree with further named attributes (names and syntax).
    </description>
  </explain>

  <\explain>
    <cpp|tag_info (int arity, int extra, int am, int cm, bool
    frozen)><explain-synopsis|create a record>
  <|explain>
    Creates a record with the given arity fields, arity mode <cpp|am> and
    child mode <cpp|cm>. The array <cpp|ci> receives <math|0> entries if
    <cpp|arity+extra> is zero, one entry for <verbatim|CHILD_UNIFORM>, two
    for <verbatim|CHILD_BIFORM> and <cpp|arity+extra> for
    <verbatim|CHILD_DETAILED>. With <cpp|frozen> set, the arity, border,
    block, with-like and var-type properties and all child properties are
    frozen (but see the <hlink|pitfalls|drd-pitfalls.en.tm> about
    <cpp|freeze_type>).
  </explain>

  The member functions <cpp|inner_border>, <cpp|outer_border>,
  <cpp|with_like>, <cpp|var_parameter>, <cpp|var_macro_parameter>,
  <cpp|type>, <cpp|accessible>, <cpp|hidden>, <cpp|disable_writable>,
  <cpp|enable_writable>, <cpp|locals>, <cpp|name> and <cpp|long_name>
  modify the record and return it again as a <cpp|tag_info>, so that they
  can be chained in the declarative style of <verbatim|drd_std.cpp>:

  <\cpp-code>
    fixed (2) -\<gtr\> name ("fraction") -\<gtr\>

    \ \ accessible (0) -\<gtr\> locals (0, "math-display", "false")
  </cpp-code>

  Note that <cpp|accessible (i)> and <cpp|hidden (i)> also set the type
  of the child to <verbatim|TYPE_REGULAR>, so a type declaration must come
  <em|after> them to take effect.

  <section|Properties of the tag (<cpp|parent_info>)>

  <cpp|parent_info> is a 32-bit bit field. In the order of the fields
  (which is also the order of the bits in the serialized form, starting
  from the least significant bit):

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<table|<row|<cell|Field>|<cell|Bits>|<cell|Values>>|<row|<cell|<cpp|type>>|<cell|5>|<cell|<verbatim|TYPE_*>>>|<row|<cell|<cpp|arity_mode>>|<cell|2>|<cell|<verbatim|ARITY_NORMAL>,
  <verbatim|ARITY_OPTIONS>, <verbatim|ARITY_REPEAT>,
  <verbatim|ARITY_VAR_REPEAT>>>|<row|<cell|<cpp|arity_base>>|<cell|6>|<cell|0--63>>|<row|<cell|<cpp|arity_extra>>|<cell|4>|<cell|0--15>>|<row|<cell|<cpp|child_mode>>|<cell|2>|<cell|<verbatim|CHILD_UNIFORM>,
  <verbatim|CHILD_BIFORM>, <verbatim|CHILD_DETAILED>>>|<row|<cell|<cpp|border_mode>>|<cell|2>|<cell|<verbatim|BORDER_YES>,
  <verbatim|BORDER_INNER>, <verbatim|BORDER_OUTER>,
  <verbatim|BORDER_NO>>>|<row|<cell|<cpp|block>>|<cell|2>|<cell|<verbatim|BLOCK_NO>,
  <verbatim|BLOCK_YES>, <verbatim|BLOCK_OR>>>|<row|<cell|<cpp|with_like>>|<cell|1>|<cell|boolean>>|<row|<cell|<cpp|var_type>>|<cell|2>|<cell|<verbatim|VAR_MACRO>,
  <verbatim|VAR_PARAMETER>, <verbatim|VAR_MACRO_PARAMETER>>>|<row|<cell|<cpp|freeze_type>,
  <cpp|freeze_arity>, <cpp|freeze_border>, <cpp|freeze_block>,
  <cpp|freeze_with>, <cpp|freeze_var_type>>|<cell|6\<times\>1>|<cell|booleans>>>>>>
    The fields of <cpp|parent_info>.
  </big-table>

  <subsection|Type>

  The field <cpp|type> is the type of the <em|value> of the tag: regular
  content for most tags, but <verbatim|TYPE_NUMERIC> for <markup|plus>,
  <verbatim|TYPE_LENGTH> for <markup|cm-length>,
  <verbatim|TYPE_GRAPHICAL> for <markup|line>, <verbatim|TYPE_COLOR> for
  <markup|rgb-color>, and so on. Getters: <cpp|get_type (tree_label)>
  and <cpp|get_type (tree)>; setter <cpp|set_type (tree_label, int)>. The
  type of a variable (see <cpp|var_type>) is the type of its value. The
  list of types is given <hlink|below|#types>.

  <subsection|Arity>

  The three fields <cpp|arity_mode>, <cpp|arity_base> and
  <cpp|arity_extra> describe the set of admissible arities <math|n>:

  <\description>
    <item*|<verbatim|ARITY_NORMAL>><math|n=base+extra> (fixed arity).

    <item*|<verbatim|ARITY_OPTIONS>><math|base\<leqslant\>n\<leqslant\>base+extra>
    (optional trailing arguments; the comment in <verbatim|tag_info.hpp>
    says <math|\<less\>> but the code uses <math|\<leqslant\>>).

    <item*|<verbatim|ARITY_REPEAT>><math|n=base+k\<cdot\>extra> for
    <math|k\<geqslant\>0>: a fixed prefix followed by repeated groups of
    <math|extra> children.

    <item*|<verbatim|ARITY_VAR_REPEAT>>As <verbatim|ARITY_REPEAT>, but the
    repeated groups come <em|first> and the <math|base> fixed children
    come last, as in <markup|with> (pairs of variables and values,
    followed by the body).
  </description>

  For <verbatim|ARITY_VAR_REPEAT>, <cpp|set_arity (l, arity, extra, am,
  cm)> stores its arguments swapped (<cpp|arity_base= extra>,
  <cpp|arity_extra= arity>), so that <cpp|arity> is the length of the
  repeated group and <cpp|extra> the number of trailing children, as for
  the helper <cpp|var_repeat> of <verbatim|drd_std.cpp>. The derived
  queries are:

  <\explain>
    <cpp|int get_minimal_arity (tree_label l)>

    <cpp|int get_maximal_arity (tree_label l)><explain-synopsis|bounds>
  <|explain>
    <math|base+extra> for a fixed arity; <math|base> otherwise for the
    minimum; <math|base+extra> for <verbatim|ARITY_OPTIONS> and
    <cpp|0x7fffffff> for the two repeat modes for the maximum.
  </explain>

  <\explain>
    <cpp|bool correct_arity (tree_label l, int n)><explain-synopsis|is
    <math|n> admissible?>
  <|explain>
    Used to validate trees (<cpp|drd_correct>, <cpp|correct_node>) and to
    find the arity of a new tag (<cpp|edit_dynamic_rep::make_compound>
    takes the smallest admissible positive arity, or zero for fixed
    arity <math|0>).
  </explain>

  <\explain>
    <cpp|bool insert_point (tree_label l, int i, int n)><explain-synopsis|may
    children be inserted at position <math|i>?>
  <|explain>
    For a tag of arity <math|n>: never for a fixed arity; for
    <verbatim|ARITY_OPTIONS> at any position <math|\<geqslant\>base> as
    long as <math|n\<less\>base+extra>; for <verbatim|ARITY_REPEAT> at
    positions inside the prefix or at the start of a group; for
    <verbatim|ARITY_VAR_REPEAT> at the start of a group or inside the
    trailing part. Used by <cpp|insert_argument> and
    <cpp|remove_empty_argument> (<verbatim|Edit/Modify/edit_dynamic.cpp>)
    to implement structured insertion of arguments.
  </explain>

  <\explain>
    <cpp|bool is_dynamic (tree t, bool hack= true)><explain-synopsis|does
    the tag have a variable arity?>
  <|explain>
    True if the arity mode is not <verbatim|ARITY_NORMAL>, except for
    <markup|document>, <markup|para>, <markup|concat>, <markup|table> and
    <markup|row>. With <cpp|hack> set, all non-primitive tags are considered
    dynamic (marked <verbatim|FIXME> in the source). The editor
    (<cpp|edit_dynamic_rep::find_dynamic>) uses the default, the glue
    <scm|tree-is-dynamic?> passes <cpp|false>.
  </explain>

  <cpp|get_old_arity> returns the arity for a fixed arity and <math|-1>
  otherwise; <cpp|get_arity_mode>, <cpp|get_arity_base>,
  <cpp|get_arity_extra>, <cpp|get_child_mode> give direct access to the
  fields, and <cpp|get_nr_indices> returns the number of child records.

  <subsection|Child layout><label|child-layout>

  The field <cpp|child_mode> determines how a logical child number
  <math|i> of a tree with <math|n> children is mapped to an index into the
  array <cpp|ci>; this is implemented by
  <cpp|tag_info_rep::get_index (int child, int n)>:

  <\description>
    <item*|<verbatim|CHILD_UNIFORM>>All children share <cpp|ci[0]>.

    <item*|<verbatim|CHILD_BIFORM>>Two records. For all arity modes except
    <verbatim|ARITY_VAR_REPEAT>, <cpp|ci[0]> describes the first
    <math|base> children and <cpp|ci[1]> the others; for
    <verbatim|ARITY_VAR_REPEAT>, <cpp|ci[0]> describes the repeated part
    and <cpp|ci[1]> the <math|base> trailing children. So for
    <markup|with>, <cpp|ci[0]> is the record of the variable/value pairs
    and <cpp|ci[1]> that of the body.

    <item*|<verbatim|CHILD_DETAILED>>One record per child of the
    \Pperiod\Q: for fixed and optional arities, <math|i> itself; for
    <verbatim|ARITY_REPEAT>, the prefix children have their own records
    and the children of a group cycle through the next <math|extra>
    records; for <verbatim|ARITY_VAR_REPEAT> the group comes first.
  </description>

  If the computed index falls outside <cpp|ci> (for instance for a tree
  with more children than its declared arity), the high-level queries
  answer conservatively: not accessible (except in source mode), type
  <verbatim|TYPE_INVALID>, writability <verbatim|WRITABILITY_DISABLE>,
  empty name, and an empty environment <verbatim|""> from
  <cpp|get_env_child>.

  <subsection|Border>

  The field <cpp|border_mode> says whether the cursor may stand at the
  boundaries of the tag. It is interpreted bitwise
  (<verbatim|BORDER_NO> is <verbatim|BORDER_INNER\|BORDER_OUTER>):

  <\explain>
    <cpp|bool is_child_enforcing (tree t)><explain-synopsis|bit
    <verbatim|BORDER_INNER> set and <cpp|N(t)\<gtr\>0>>
  <|explain>
    The cursor may not be positioned on the tag itself (paths ending with
    <verbatim|0> or <verbatim|1> relative to <cpp|t>); it has to go inside
    a child. This is the case for <markup|document>, <markup|concat>,
    <markup|table>, <markup|row>, <markup|cell>, <markup|hidden>, ...
  </explain>

  <\explain>
    <cpp|bool is_parent_enforcing (tree t)><explain-synopsis|bit
    <verbatim|BORDER_OUTER> set and <cpp|N(t)\<gtr\>0>>
  <|explain>
    The cursor may not be positioned at the start of the first accessible
    child or the end of the last one; the positions outside the tag are
    used instead (see <cpp|is_accessible_cursor> in
    <verbatim|Data/Tree/tree_cursor.cpp>).
  </explain>

  <cpp|var_without_border (tree_label l)> is true for non-primitive tags
  with the <verbatim|BORDER_INNER> bit; the editor treats such macros like
  <markup|concat> when selecting (<cpp|edit_select_rep::select_enlarge> in
  <verbatim|Edit/Replace/edit_select.cpp>, <cpp|selection_adjust_border>
  in <verbatim|Data/Tree/tree_select.cpp>) and when removing structure
  (<cpp|edit_text_rep::remove_structure_upwards> in
  <verbatim|Edit/Modify/edit_delete.cpp>). The heuristics never set the
  border; it is only changed by <cpp|drd_std.cpp> and by
  <markup|drd-props>.

  <subsection|Block, with-like and variable type>

  <\description>
    <item*|<cpp|block>>Meant to tell whether the tag is block or inline
    content (<verbatim|BLOCK_OR>: a block if one of its children which
    admit both is a block). The getters and setters are declared in
    <verbatim|drd_info.hpp> but not implemented, and no code reads the
    field: it is currently unused.

    <item*|<cpp|with_like>>The tag only modifies the environment of its
    last child, like <markup|with>, <markup|with-package> or a macro such
    as <markup|strong>. Queried by <cpp|is_with_like (tree t)> (which also
    requires <cpp|N(t)\<gtr\>0>); used by the correction routines of
    <verbatim|Data/Tree/tree_analyze.cpp> and <verbatim|tree_correct.cpp>
    (<scm|with-correct> and friends), by the heuristics
    (<cpp|heuristic_with_like>) and by <cpp|make_compound>, which delegates
    the insertion of with-like tags to the <scheme> routine
    <scm|with-like-check-insert>.

    <item*|<cpp|var_type>><verbatim|VAR_MACRO> for tags (the default),
    <verbatim|VAR_PARAMETER> for environment variables and
    <verbatim|VAR_MACRO_PARAMETER> for macros which are used as parameters
    (a zero-argument macro holding a length or a localized string). The
    free functions <cpp|is_macro> and <cpp|is_parameter>
    (<verbatim|Data/Tree/tree_traverse.cpp>, glue
    <scm|tree-label-macro?> and <scm|tree-label-parameter?>) test
    <cpp|var_type != VAR_PARAMETER>, <abbr|resp.> <cpp|var_type !=
    VAR_MACRO>; they are used to build the focus menus of style parameters
    in <verbatim|generic/generic-menu.scm>.
  </description>

  <subsection|Freezing>

  Each property has a <verbatim|freeze_*> bit. A frozen property is never
  changed by the corresponding <cpp|set_*> method, which simply returns.
  The methods <cpp|freeze_type>, <cpp|freeze_arity>, <cpp|freeze_border>,
  <cpp|freeze_with_like>, <cpp|freeze_var_type> (for the tag) and
  <cpp|freeze_type>, <cpp|freeze_accessible>, <cpp|freeze_writability>,
  <cpp|freeze_env> (for a child) set them. Freezing is how explicit
  declarations win over heuristics: <markup|drd-props> freezes every
  property it sets, and <cpp|init_std_drd> freezes the arity and border
  of all primitives, so that a style cannot accidentally change the arity
  of <markup|frac> by defining a macro <markup|frac>.

  <section|Properties of the children (<cpp|child_info>)>

  <cpp|child_info> is also a bit field:

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<table|<row|<cell|Field>|<cell|Bits>|<cell|Values
  (default first)>>|<row|<cell|<cpp|type>>|<cell|5>|<cell|<verbatim|TYPE_ADHOC>,
  other <verbatim|TYPE_*>>>|<row|<cell|<cpp|accessible>>|<cell|2>|<cell|<verbatim|ACCESSIBLE_NEVER>,
  <verbatim|ACCESSIBLE_HIDDEN>, <verbatim|ACCESSIBLE_ALWAYS>>>|<row|<cell|<cpp|writability>>|<cell|2>|<cell|<verbatim|WRITABILITY_NORMAL>,
  <verbatim|WRITABILITY_DISABLE>, <verbatim|WRITABILITY_ENABLE>>>|<row|<cell|<cpp|block>>|<cell|2>|<cell|<verbatim|BLOCK_REQUIRE_BLOCK>,
  <verbatim|BLOCK_REQUIRE_INLINE>, <verbatim|BLOCK_REQUIRE_NONE>>>|<row|<cell|<cpp|env>>|<cell|16>|<cell|index
  in the environment table>>|<row|<cell|<cpp|freeze_type>,
  <cpp|freeze_accessible>, <cpp|freeze_writability>, <cpp|freeze_block>,
  <cpp|freeze_env>>|<cell|5\<times\>1>|<cell|booleans>>>>>>
    The fields of <cpp|child_info>.
  </big-table>

  <subsection|Accessibility>

  <cpp|accessible> says whether the cursor may enter the child.
  <verbatim|ACCESSIBLE_ALWAYS> children are ordinary editable content (the
  numerator of a fraction, the body of a <markup|with>);
  <verbatim|ACCESSIBLE_NEVER> children are only editable in source mode
  or through the focus bar (the <abbr|URL> of a hyperlink, the width of an
  image); <verbatim|ACCESSIBLE_HIDDEN> children are accessible only when
  the global access mode allows hidden content (the body of
  <markup|hidden>, which search must still traverse). The decision is
  taken by

  <\explain>
    <cpp|bool is_accessible_child (tree t, int i)><explain-synopsis|may the
    cursor enter <cpp|t[i]>?>
  <|explain>
    Translates <cpp|i> with <cpp|get_index> and compares the stored value
    with the global access mode (<verbatim|Data/Drd/drd_mode.hpp>):
    <verbatim|DRD_ACCESS_NORMAL> accepts only
    <verbatim|ACCESSIBLE_ALWAYS>, <verbatim|DRD_ACCESS_HIDDEN> also
    <verbatim|ACCESSIBLE_HIDDEN>, and <verbatim|DRD_ACCESS_SOURCE> accepts
    every existing child. For an <markup|extern> tree whose first child is
    a string <verbatim|f>, the record of the pseudo-label
    <verbatim|extern:f> is used, which allows style files to declare the
    accessibility of the arguments of individual <scheme> functions with
    <markup|drd-props> (see <verbatim|packages/customize/math/math-check.ts>).
  </explain>

  Related queries: <cpp|is_accessible_path (t, p)> (all children along a
  path), <cpp|all_accessible (l)> (all records are
  <verbatim|ACCESSIBLE_ALWAYS>, and there is at least one) and
  <cpp|none_accessible (l)>. The low-level <cpp|get_accessible (l, nr)>
  and <cpp|set_accessible (l, nr, mode)> work on physical indices. How the
  access modes are switched is explained in <hlink|style and document
  DRDs|drd-documents.en.tm>.

  The accessibility in the <abbr|DRD> must agree with the boxes produced
  by the typesetter: a child declared accessible but typeset as a
  decoration (or conversely) leads to erratic cursor movement. See
  <hlink|the macro expansion chapter|macro-expansion-drd.en.tm> for
  details.

  <subsection|Writability>

  <cpp|writability> implements read-only regions:
  <verbatim|WRITABILITY_DISABLE> makes the whole subtree of the child
  read-only, except for descendants whose writability is re-enabled with
  <verbatim|WRITABILITY_ENABLE>. The routine <cpp|is_accessible_cursor>
  (<verbatim|Data/Tree/tree_cursor.cpp>) switches the global writable mode
  (<cpp|set_writable_mode>) to <verbatim|DRD_WRITABLE_INPUT> when it
  enters a disabled child and back to <verbatim|DRD_WRITABLE_NORMAL> when
  it enters an enabled one; in <verbatim|DRD_WRITABLE_INPUT> mode no
  cursor position in a string is accessible (outside source mode). The
  macros <markup|disable-writability> and <markup|enable-writability> of
  <verbatim|packages/gui/gui-widget.ts> are declared this way. Query:
  <cpp|get_writability_child (t, i)>.

  <subsection|Block requirements>

  <cpp|block> (<verbatim|BLOCK_REQUIRE_*>) was meant to say whether a
  child must contain block or inline content. Like the corresponding
  parent field it is never set nor read. Note that its numerical default
  is <math|0>, <abbr|i.e.> <verbatim|BLOCK_REQUIRE_BLOCK>.

  <subsection|Environment of a child><label|child-env>

  <cpp|env> describes how the environment changes when going from the tag
  into the child, for instance <verbatim|mode=math> for the body of
  <markup|equation> or <verbatim|math-display=false> for scripts. The
  value is an <markup|attr> tree <verbatim|(attr var-1 val-1 ... var-n
  val-n)> (the default is the empty <markup|with> tree, meaning no
  change). To keep <cpp|child_info> small, these trees are stored in a
  global table and the record contains only their index:

  <\explain>
    <cpp|int drd_encode (tree t)>

    <cpp|tree drd_decode (int i)><explain-synopsis|global environment
    table>
  <|explain>
    <cpp|drd_encode> returns the index of <cpp|t> in a global table,
    adding it if necessary (the table is never shrunk; more than
    <math|2<rsup|16>> different environments trigger an assertion).
  </explain>

  A value <verbatim|(arg i)> with an integer <verbatim|i> refers to the
  <math|i>-th child of the same tree: the heuristics produce such values
  when an argument is used in a <markup|with> around another argument, for
  example for

  <\tm-fragment>
    <inactive*|<assign|my-code|<macro|lang|body|<with|prog-language|<arg|lang>|<arg|body>>>>>
  </tm-fragment>

  the environment of child <math|1> is <verbatim|(attr prog-language (arg
  0))>.

  <\explain>
    <cpp|tree get_env_child (tree t, int i, tree env)><explain-synopsis|environment
    inside <cpp|t[i]>>
  <|explain>
    Returns <cpp|env> (an <markup|attr> tree) updated with the changes for
    child <cpp|i>: for <markup|with> the variables of the tree itself are
    merged in; otherwise the stored environment is decoded, references
    <verbatim|(arg j)> are replaced by copies of <cpp|t[j]>, and the result
    is merged with <cpp|drd_env_merge>. Values are <em|not evaluated>: the
    caller decides whether to evaluate them (the editor does so in
    <cpp|typeset_exec_until>, most other callers only compare strings). The
    result is the empty string if <cpp|i> is outside the layout.
  </explain>

  <cpp|get_env_child (t, i, var, val)> returns the value of a single
  variable (with default <cpp|val>), and <cpp|get_env_descendant (t, p,
  env)> and <cpp|get_env_descendant (t, p, var, val)> follow a whole path.
  The free functions <cpp|drd_env_write>, <cpp|drd_env_merge> and
  <cpp|drd_env_read> manipulate the <markup|attr> trees
  (<cpp|drd_env_write> keeps the variables sorted). The low-level access is
  <cpp|get_env (l, nr)>, <cpp|set_env (l, nr, env)> and
  <cpp|freeze_env (l, nr)>.

  <section|Named attributes>

  The tree <cpp|extra> stores further attributes as an <markup|attr>
  list, through <cpp|set_attribute> and <cpp|get_attribute>:

  <\description>
    <item*|<verbatim|name>, <verbatim|long-name>>The name of the tag shown
    to the user (footer, focus bar, menus). <cpp|get_name (l)> defaults to
    the label itself and <cpp|get_long_name (l)> to the name.

    <item*|<verbatim|name-i>, <verbatim|long-name-i>>The names of the
    children, by physical index. The heuristics use the parameter names of
    the macro. Queried by <cpp|get_child_name (t, i)> and
    <cpp|get_child_long_name (t, i)>, which translate the logical index.

    <item*|<verbatim|syntax>>A tree describing how the tag should be
    understood by the mathematical parser (packrat grammars). It may be a
    symbol (<verbatim|\<less\>int\<gtr\>>) or a <markup|macro> which is
    applied to the arguments of the tag. <cpp|get_syntax (l)> falls back on
    the definition of the tag in <cpp|env>, <abbr|i.e.> a macro is by
    default \Pparsed as its expansion\Q; <cpp|get_syntax (tree t, path
    p)> performs the substitution of the arguments (wrapping them in
    <markup|quasi> trees with their paths if <cpp|p> is given). Consumers:
    <verbatim|System/Language/packrat_serializer.cpp> and
    <cpp|symbol_type> in <verbatim|Data/Tree/tree_analyze.cpp>.
  </description>

  <section|Types><label|types>

  The constants <verbatim|TYPE_*> are used both for the value of a tag and
  for its children. <cpp|drd_decode_type> and <cpp|drd_encode_type>
  convert between the constants and the names used by <markup|drd-props>
  and by the glue (<scm|tree-child-type>, <scm|tree-label-type>).

  <\description>
    <item*|<verbatim|TYPE_REGULAR> (<verbatim|regular>)>Ordinary content.

    <item*|<verbatim|TYPE_ADHOC> (<verbatim|adhoc>)>Content without a
    well-defined type; the default for children.

    <item*|<verbatim|TYPE_RAW>>Raw binary data (child of
    <markup|raw-data>); has no name, so <cpp|drd_decode_type> returns
    <verbatim|unknown>.

    <item*|<verbatim|TYPE_VARIABLE> (<verbatim|variable>),
    <verbatim|TYPE_ARGUMENT> (<verbatim|argument>)>Names of environment
    variables and macro arguments.

    <item*|<verbatim|TYPE_BINDING>>Alternating variables and values, as in
    <markup|with> and <markup|attr>. <cpp|get_type_child> resolves it to
    <verbatim|TYPE_VARIABLE> for even and <verbatim|TYPE_REGULAR> for odd
    children; it has no name.

    <item*|<verbatim|TYPE_BOOLEAN>, <verbatim|TYPE_INTEGER>,
    <verbatim|TYPE_STRING>, <verbatim|TYPE_LENGTH>,
    <verbatim|TYPE_NUMERIC>>Scalars (<verbatim|boolean>,
    <verbatim|integer>, <verbatim|string>, <verbatim|length>,
    <verbatim|numeric>).

    <item*|<verbatim|TYPE_CODE>, <verbatim|TYPE_IDENTIFIER>,
    <verbatim|TYPE_URL>, <verbatim|TYPE_COLOR>>Program code, identifiers
    (labels, keys), <abbr|URL>s and colors (<verbatim|code>,
    <verbatim|identifier>, <verbatim|url>, <verbatim|color>).

    <item*|<verbatim|TYPE_GRAPHICAL>, <verbatim|TYPE_POINT>,
    <verbatim|TYPE_CONSTRAINT>, <verbatim|TYPE_GRAPHICAL_ID>>Graphical
    objects, points, constraints and graphical identifiers.

    <item*|<verbatim|TYPE_EFFECT>, <verbatim|TYPE_ANIMATION>,
    <verbatim|TYPE_DURATION>, <verbatim|TYPE_FONT_SIZE>>Graphical effects,
    animations, durations and font sizes.

    <item*|<verbatim|TYPE_OBSOLETE>, <verbatim|TYPE_UNKNOWN>,
    <verbatim|TYPE_ERROR>>Special values; <verbatim|TYPE_UNKNOWN> is the
    initial value used by the heuristics. <verbatim|TYPE_INVALID>
    (<math|-1>) is returned by <cpp|get_type_child> for a child outside the
    layout and is never stored.
  </description>

  The child types are used for syntax coloring in source mode
  (<cpp|highlight> in <verbatim|Typeset/Env/env_inactive.cpp> maps them to
  <markup|src-regular>, <markup|src-var>, <markup|src-length>, ...), by
  the focus bar to choose an input field for inaccessible children
  (<scm|type-\<gtr\>format> in <verbatim|generic/generic-menu.scm>), by the
  correction routine <cpp|is_correctable_child>, and by the heuristics to
  infer the types of macro arguments.

  <section|Serialization>

  A <abbr|DRD> can be converted to a tree and back, which is how style
  <abbr|DRD>s are stored in the disk cache:

  <\itemize>
    <item><cpp|parent_info::operator tree> packs the bit field into an
    integer, written as a decimal string.

    <item><cpp|child_info::operator tree> does the same with all fields
    but <cpp|env> (16 bits); if the environment is not empty, the result is
    the environment tree with the integer appended as last child.

    <item><cpp|tag_info::operator tree> returns <verbatim|(tuple pi (tuple
    ci ...))>, followed by <cpp|extra> if it is not empty.

    <item><cpp|drd_info_rep::get_locals> returns <verbatim|(collection
    (associate label info) ...)> for the <em|local> entries only (those
    which differ from the base), and <cpp|set_locals> installs such a
    collection.
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
