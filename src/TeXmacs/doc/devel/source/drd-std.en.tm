<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The standard DRD and new primitives>

  <section|Role of <verbatim|drd_std.cpp>>

  The file <verbatim|Data/Drd/drd_std.cpp> defines three global objects
  (declared in <verbatim|drd_std.hpp>):

  <\description>
    <item*|<cpp|drd_info std_drd ("tm")>>The standard <abbr|DRD>, which
    describes every built-in primitive and the built-in environment
    variables. Every other <abbr|DRD> inherits from it.

    <item*|<cpp|drd_info the_drd>>The <abbr|DRD> of the current view,
    initially equal to <cpp|std_drd>; see <hlink|style and document
    DRDs|drd-documents.en.tm>.

    <item*|<cpp|hashmap\<less\>string,int\<gtr\> STD_CODE>>The table from
    the names of the primitives to their tree labels. The function
    <cpp|std_contains (s)> tests whether <cpp|s> is the name of a
    primitive.
  </description>

  The function <cpp|init_std_drd> fills these objects. It is called once
  during start-up (<cpp|init_texmacs> in
  <verbatim|System/Boot/init_texmacs.cpp>) and, defensively, by
  <cpp|get_style_drd>; a static flag makes further calls no-ops.

  Besides describing the primitives, <cpp|init_std_drd> has a second,
  less obvious, role: it is the place where the members of the
  enumeration <cpp|tree_label> (<verbatim|Kernel/Types/tree_label.hpp>)
  receive their string names. Each declaration

  <\cpp-code>
    static void

    init (tree_label l, string name, tag_info ti) {

    \ \ STD_CODE(name)= (int) l;

    \ \ make_tree_label (l, name);

    \ \ std_drd-\<gtr\>info (l)= ti;

    \ \ std_drd-\<gtr\>freeze_arity (l);

    \ \ std_drd-\<gtr\>freeze_border (l);

    }
  </cpp-code>

  registers the name in the tables of <verbatim|tree_label.cpp> (so that
  <cpp|as_string (FRAC)> is <verbatim|"frac"> and
  <cpp|as_tree_label ("frac")> is <cpp|FRAC>), records it in
  <cpp|STD_CODE>, installs the <cpp|tag_info>, and freezes the arity and
  border. Since <cpp|STD_CODE> is used by the readers of documents
  (<cpp|tm_reader> in <verbatim|Data/Convert/Texmacs/fromtm.cpp>,
  <cpp|get_codes> in <verbatim|upgradetm.cpp>,
  <cpp|scheme_tree_to_tree> in <verbatim|Data/Convert/Scheme/from_scheme.cpp>),
  a label which is not declared here is not recognized as a primitive when
  a document is loaded. At the time of writing every member of the
  enumeration has a declaration; two further labels, <markup|shown> and
  <markup|ignore>, are not in the enumeration but are created by
  <cpp|make_tree_label> and declared as primitives in the same way.

  Environment variables are declared with

  <\cpp-code>
    static void

    init_var (string var, int tp, string vname= "") {

    \ \ tree_label l= make_tree_label (var);

    \ \ tag_info ti= fixed (0) -\<gtr\> var_parameter () -\<gtr\> type (tp);

    \ \ if (vname != "") ti= ti-\<gtr\>name (vname);

    \ \ std_drd-\<gtr\>info (l)= ti;

    \ \ std_drd-\<gtr\>freeze_arity (l);

    \ \ std_drd-\<gtr\>freeze_border (l);

    }
  </cpp-code>

  which gives the variable a <verbatim|VAR_PARAMETER> record with a type,
  for instance <cpp|init_var (FONT_SIZE, TYPE_FONT_SIZE)> or
  <cpp|init_var (PAGE_ODD_HEADER, TYPE_REGULAR, "odd page header")>. The
  variable names are the string constants of <verbatim|Data/Drd/vars.hpp>.
  Variables which are not declared here receive their type from the
  heuristics (see <hlink|style and document DRDs|drd-documents.en.tm>).

  <section|The declaration language>

  The declarations are written in a small embedded language of helper
  functions and macros, defined at the top of <verbatim|drd_std.cpp>. The
  arity is given by one of four constructors, which all create frozen
  records:

  <\explain>
    <cpp|fixed (int arity, int extra=0, int child_mode= CHILD_UNIFORM)>

    <cpp|options (int arity, int extra, int child_mode= CHILD_UNIFORM)>

    <cpp|repeat (int arity, int extra, int child_mode= CHILD_UNIFORM)>

    <cpp|var_repeat (int arity, int extra, int child_mode=
    CHILD_UNIFORM)><explain-synopsis|arity constructors>
  <|explain>
    Create a <cpp|tag_info> with arity mode <verbatim|ARITY_NORMAL>,
    <verbatim|ARITY_OPTIONS>, <verbatim|ARITY_REPEAT> or
    <verbatim|ARITY_VAR_REPEAT>. For <cpp|fixed>, <cpp|arity+extra> is the
    arity; the split only matters for the child layout
    (<cpp|fixed (1, 1, BIFORM)> is a tag with two children having two
    different records). For <cpp|options (a, x)>, the arity ranges from
    <math|a> to <math|a+x>. For <cpp|repeat (a, x)>, there are <math|a>
    fixed children followed by any number of groups of <math|x> children.
    For <cpp|var_repeat (a, x)>, groups of <math|a> children are followed by
    <math|x> fixed children (the arguments are swapped internally). The
    abbreviations <verbatim|BIFORM> and <verbatim|DETAILED> stand for
    <verbatim|CHILD_BIFORM> and <verbatim|CHILD_DETAILED>.
  </explain>

  The result is then refined by chaining member functions of
  <cpp|tag_info_rep> with <cpp|-\<gtr\>>:

  <\description>
    <item*|Children types>Macros such as <cpp|regular (i)>,
    <cpp|adhoc (i)>, <cpp|raw (i)>, <cpp|argument (i)>,
    <cpp|variable (i)>, <cpp|binding (i)>, <cpp|boolean (i)>,
    <cpp|integer (i)>, <cpp|string_type (i)>, <cpp|numeric (i)>,
    <cpp|length (i)>, <cpp|code (i)>, <cpp|url_type (i)>,
    <cpp|identifier (i)>, <cpp|color_type (i)>, <cpp|graphical (i)>,
    <cpp|constraint (i)>, <cpp|graphical_id (i)>, <cpp|point_type (i)>,
    <cpp|effect (i)>, <cpp|animation (i)> and <cpp|duration (i)>, each
    expanding to <cpp|type (i, TYPE_...)>. The index <cpp|i> is a
    <em|physical> index into the child records.

    <item*|Type of the value>Macros <cpp|returns_adhoc ()>,
    <cpp|returns_boolean ()>, <cpp|returns_integer ()>,
    <cpp|returns_string ()>, <cpp|returns_numeric ()>,
    <cpp|returns_length ()>, <cpp|returns_url ()>,
    <cpp|returns_identifier ()>, <cpp|returns_animation ()>,
    <cpp|returns_duration ()>, <cpp|returns_color ()>,
    <cpp|returns_graphical ()>, <cpp|returns_constraint ()> and
    <cpp|returns_effect ()>, expanding to <cpp|type (TYPE_...)>.

    <item*|Accessibility>The methods <cpp|accessible (i)> and
    <cpp|hidden (i)>, which also set the child type to regular, and
    <cpp|disable_writable (i)>, <cpp|enable_writable (i)>.

    <item*|Environment>The method <cpp|locals (i, var, val)> stores the
    environment <verbatim|(attr var val)> for child <cpp|i>; only one
    variable can be given this way.

    <item*|Border and kind>The methods <cpp|inner_border ()>,
    <cpp|outer_border ()>, <cpp|with_like ()>, <cpp|var_parameter ()>
    and <cpp|var_macro_parameter ()>.

    <item*|Names>The methods <cpp|name (s)>, <cpp|long_name (s)>,
    <cpp|name (i, s)> and <cpp|long_name (i, s)>.
  </description>

  A few representative declarations:

  <\cpp-code>
    init (FRAC, "frac",

    \ \ \ \ \ \ fixed (2) -\<gtr\> name ("fraction") -\<gtr\>

    \ \ \ \ \ \ accessible (0) -\<gtr\> locals (0, "math-display", "false"));

    init (WITH, "with",

    \ \ \ \ \ \ var_repeat (2, 1, BIFORM) -\<gtr\> with_like () -\<gtr\>

    \ \ \ \ \ \ binding (0) -\<gtr\> accessible (1));

    init (HLINK, "hlink",

    \ \ \ \ \ \ fixed (1, 1, BIFORM) -\<gtr\>

    \ \ \ \ \ \ accessible (0) -\<gtr\> name (0, "text") -\<gtr\>

    \ \ \ \ \ \ url_type (1) -\<gtr\> name (1, "destination") -\<gtr\>

    \ \ \ \ \ \ name ("hyperlink"));

    init (TFORMAT, "tformat",

    \ \ \ \ \ \ var_repeat (1, 1, BIFORM) -\<gtr\> inner_border () -\<gtr\>

    \ \ \ \ \ \ accessible (1) -\<gtr\> name ("table format"));

    init (EXTERN, "extern",

    \ \ \ \ \ \ repeat (1, 1, BIFORM) -\<gtr\> code (0) -\<gtr\> regular (1));
  </cpp-code>

  They read as follows. <markup|frac> has two children which share one
  record (<verbatim|CHILD_UNIFORM>): both are accessible, and in both
  <src-var|math-display> is false. <markup|with> has any number of
  variable/value pairs, typed as bindings, followed by one accessible
  body; it only modifies the environment. The text of <markup|hlink> is
  accessible, its destination is an inaccessible <abbr|URL>. In
  <markup|tformat>, the cursor may not stand on the tag itself, the format
  instructions are inaccessible and the table is accessible.
  <markup|extern> is a function name followed by regular, but inaccessible,
  arguments.

  The macro <cpp|macro (i)> defined at the top of the file refers to a
  non-existent constant <verbatim|TYPE_MACRO>; it is never used, so the
  file compiles.

  <section|Adding a new primitive>

  A new primitive touches many parts of the kernel. The <abbr|DRD>-related
  steps are:

  <\enumerate>
    <item>Add a member to the enumeration <cpp|tree_label> in
    <verbatim|Kernel/Types/tree_label.hpp>, before
    <cpp|START_EXTENSIONS>. The numbering of the enumeration is not stored
    in documents, so the position is free.

    <item>Add an <cpp|init> declaration in <cpp|init_std_drd>. Choose the
    arity constructor which matches the admissible arities exactly: the
    editor creates new instances with the smallest admissible arity
    (<cpp|make_compound>), deletes instances with a wrong arity
    (<cpp|correct_node>, <cpp|drd_correct>), and offers structured
    insertion of arguments only at the positions allowed by
    <cpp|insert_point>.

    <item>Declare as accessible exactly the children which the typesetter
    typesets as editable content (that is, with the inverse path of the
    child). Give the inaccessible children a precise type and a name: the
    focus bar shows an input field for each inaccessible child whose type
    is not <verbatim|adhoc>, <verbatim|raw>, <verbatim|graphical>,
    <verbatim|point>, <verbatim|obsolete> or <verbatim|unknown>, labelled by
    the child name (<verbatim|generic/generic-menu.scm>), and source mode
    colors children by type.

    <item>Declare with <cpp|locals> any environment change in a child which
    matters for editing (mostly <src-var|mode>, <src-var|math-display>
    and <src-var|prog-language>): the keyboard, the spell checker, the
    mathematical parser and the converters use it. Declare
    <cpp|inner_border ()> if the cursor should not stand on the tag itself,
    and <cpp|with_like ()> if the tag only modifies the environment of its
    last child.

    <item>Implement the evaluation (<verbatim|Typeset/Env/env_exec.cpp>) and
    the typesetting (<verbatim|Typeset/Concat/concater.cpp> or
    <verbatim|Typeset/Stack/stacker.cpp>), and, if needed, the
    converters. See the <hlink|typesetter|typesetter.en.tm> and
    <hlink|macro expansion|macro-expansion.en.tm> chapters, and the
    <hlink|example of a graphical primitive|graphics-editor-extend.en.tm>.

    <item>Document the primitive in the format documentation
    (<verbatim|doc/devel/format/regular/>).
  </enumerate>

  Two practical remarks. First, once the primitive exists, its name is
  reserved: older documents or styles defining a macro of the same name
  will see the primitive instead (the upgrader in
  <verbatim|Data/Convert/Texmacs/upgradetm.cpp> exists to rename such
  conflicts). Second, the style <abbr|DRD>s cached on disk in
  <verbatim|$TEXMACS_HOME_PATH/system/cache> are only removed
  automatically when the version of <TeXmacs> changes, after a crash
  during start-up, or with the option <verbatim|-setup>; during
  development, clear them with <menu|Tools|Update|Styles>
  (<scm|style-clear-cache>) after changing <verbatim|drd_std.cpp>.

  <section|Properties of the built-in environment variables>

  About 310 variables are declared with <cpp|init_var>. The types are
  used for the focus menus of style parameters
  (<verbatim|generic/generic-menu.scm> checks <scm|tree-label-type> to
  offer a color chooser, for instance), by the documentation generator
  (<verbatim|generic/generic-doc.scm>) and in source mode. A few variables
  carry a name (the page headers and footers). Variables which are not
  declared, such as those introduced by style files, are typed by
  <cpp|heuristic_init_parameter> from their name (<verbatim|color>,
  <verbatim|*-color>, <verbatim|*-length>, <verbatim|*-width>) or from
  their value, and can be typed explicitly with
  <verbatim|\<less\>drd-props\|var\|parameter\|type\<gtr\>>.

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
