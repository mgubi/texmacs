<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The data relation descriptor>

  <section|Purpose>

  A macro definition says how a tag is <em|rendered>, but the editor needs
  other information about tags: how many arguments they take, which
  arguments may contain the cursor, what kind of content (text, length,
  color, ...) each argument holds, which environment variables are changed
  inside an argument (so that, for instance, the editor knows that the
  body of <markup|equation> is in math mode), whether a tag is only an
  environment modifier, and so on. This information is stored in the
  <em|data relation descriptor> or <abbr|DRD>; see also the user-level
  description in <hlink|data relation
  descriptions|../format/basics/tm-drd.en.tm> and the primitive
  <markup|drd-props> in <hlink|macro
  primitives|../format/stylesheet/prim-macro.en.tm>.

  The code lives in <verbatim|Data/Drd/>. The DRD of built-in tags is
  hard-coded in <verbatim|drd_std.cpp>; the DRD of user tags is mostly
  <em|inferred> from their macro definitions, and can be refined or frozen
  by <markup|drd-props> declarations in style files.

  This page concentrates on the interplay between macros and the
  <abbr|DRD>; the complete reference for the <abbr|DRD> subsystem, on the
  <c++> and on the <scheme> side, is the chapter <hlink|the data relation
  descriptor (DRD)|drd.en.tm>.

  <section|Data structures>

  <\explain>
    <cpp|class drd_info_rep><explain-synopsis|a DRD>
  <|explain>
    Its main member is
    <cpp|rel_hashmap\<less\>tree_label,tag_info\<gtr\> info>, a
    <em|relative> hash map: a DRD created with the constructor
    <cpp|drd_info (string name, drd_info base)> only stores the entries which
    differ from those of <cpp|base>, and looks up the others in
    <cpp|base>. All DRDs of documents and styles are built on top of the
    standard DRD <cpp|std_drd>. The member <cpp|env> holds the environment
    (as set by <cpp|set_environment>) from which the heuristics were
    computed. <cpp|get_locals> and <cpp|set_locals> convert the local
    entries to and from a tree, which is how DRDs are stored in the style
    cache.
  </explain>

  <\explain>
    <cpp|class tag_info_rep><explain-synopsis|properties of one tag>
  <|explain>
    Consists of a <cpp|parent_info pi> with properties of the tag itself, an
    array <cpp|array\<less\>child_info\<gtr\> ci> with properties of its
    children, and a tree <cpp|extra> for miscellaneous attributes (names,
    syntax). Both structures are bit fields
    (<verbatim|Data/Drd/tag_info.hpp>):

    <\description>
      <item*|<cpp|parent_info>><cpp|type> (the type of the value of the tag,
      one of the <verbatim|TYPE_*> constants), <cpp|arity_mode>
      (<verbatim|ARITY_NORMAL>, <verbatim|ARITY_OPTIONS>,
      <verbatim|ARITY_REPEAT>, <verbatim|ARITY_VAR_REPEAT>),
      <cpp|arity_base>, <cpp|arity_extra>, <cpp|child_mode>
      (<verbatim|CHILD_UNIFORM>, <verbatim|CHILD_BIFORM>,
      <verbatim|CHILD_DETAILED>: whether one <cpp|child_info> is shared by
      all children, two are used, or one per child), <cpp|border_mode>,
      <cpp|block>, <cpp|with_like>, <cpp|var_type> (<verbatim|VAR_MACRO>,
      <verbatim|VAR_PARAMETER>, <verbatim|VAR_MACRO_PARAMETER>), and one
      <verbatim|freeze_*> bit per property.

      <item*|<cpp|child_info>><cpp|type>, <cpp|accessible>
      (<verbatim|ACCESSIBLE_NEVER>, <verbatim|ACCESSIBLE_HIDDEN>,
      <verbatim|ACCESSIBLE_ALWAYS>), <cpp|writability>, <cpp|block>,
      <cpp|env> (an index into a global table of environment trees, see
      <cpp|drd_encode> and <cpp|drd_decode> in <verbatim|tag_info.cpp>) and
      the corresponding <verbatim|freeze_*> bits.
    </description>
  </explain>

  The <cpp|freeze_*> bits are the key to the interplay between heuristics
  and explicit declarations: every <cpp|set_*> method returns immediately
  if the property is frozen, and <markup|drd-props> freezes each property it
  sets. Hence explicit declarations always win over the heuristics,
  whatever the order in which they are executed.

  The global variable <cpp|the_drd> (<verbatim|drd_std.hpp>) points to the
  DRD of the current buffer; it is set by the editor and window code, and
  can be changed temporarily with the helper struct <cpp|with_drd>. The
  environment instead uses the DRD of its own buffer, through the
  reference <cpp|edit_env_rep::drd>.

  <section|Accessibility>

  <cpp|drd_info_rep::is_accessible_child (tree t, int i)> decides whether
  the cursor may enter the <verbatim|i>-th child of <cpp|t>. The answer
  depends on the global access mode (<verbatim|drd_mode.hpp>):
  <verbatim|DRD_ACCESS_NORMAL> only accepts <verbatim|ACCESSIBLE_ALWAYS>,
  <verbatim|DRD_ACCESS_HIDDEN> also accepts <verbatim|ACCESSIBLE_HIDDEN>,
  and <verbatim|DRD_ACCESS_SOURCE> (used in source mode) makes every child
  accessible. For <markup|extern> trees, the properties are looked up
  under the pseudo label <verbatim|extern:f>, where <verbatim|f> is the
  name of the <scheme> function, so that <markup|drd-props> can declare the
  accessibility of the arguments of individual <scheme> macros.

  Accessibility in the DRD and accessibility of boxes (inverse paths without
  negative entries) are two different things which should agree: the
  routines which move the cursor through the tree consult the DRD, whereas
  the boxes determine where the cursor can actually be displayed. A child
  which the DRD declares accessible but which the macro typesets as a
  decoration (because it computes with it), or conversely, leads to
  inconsistent cursor behaviour.

  <section|Heuristic initialization from macros>

  <\explain>
    <cpp|void heuristic_init (hashmap\<less\>string,tree\<gtr\>
    env)><explain-synopsis|infer DRD properties from an environment>
  <|explain>
    Iterates over all variables of <cpp|env>, calling
    <cpp|heuristic_init_macro> for <markup|macro> values,
    <cpp|heuristic_init_xmacro> for <markup|xmacro> values and
    <cpp|heuristic_init_parameter> otherwise. Each of these returns whether
    it changed the <cpp|tag_info>. Since the properties of a macro depend on
    those of the macros it uses, the whole loop is repeated until nothing
    changes; after ten rounds it gives up with the warning <verbatim|bad
    heuristic drd convergence>.
  </explain>

  <cpp|heuristic_init_macro (string var, tree macro)> does the following for
  a macro with <verbatim|n> parameters:

  <\itemize>
    <item>it sets the arity to <verbatim|n> (<verbatim|ARITY_NORMAL>,
    <verbatim|CHILD_DETAILED>);

    <item>a macro without parameters whose body is a length, or a
    <markup|localize> of a string, is marked as a macro parameter
    (<verbatim|VAR_MACRO_PARAMETER>) of type length, respectively string;
    otherwise the type of the tag is the type of the body;

    <item>it sets the <em|with-like> flag if the body is a chain of
    with-like constructs ending in the last argument
    (<cpp|heuristic_with_like>);

    <item>the parameter names become the child names;

    <item>for each parameter <verbatim|x>, it calls
    <cpp|arg_access (body, (arg x), (attr), type, found)>, which searches the
    body for occurrences of <scm|(arg x)>. The search descends into the
    children which are accessible according to the current DRD,
    accumulating the environment changes made on the way
    (<markup|with>, <markup|tformat>, the child environments of other
    tags as given by <cpp|get_env_child>). <markup|compound> is treated as a
    call of the named tag, <markup|if> is looked into through its first
    branch, and a <markup|map-args> gives access if the mapped tag gives
    access to its first child. The search does not look inside nested
    <markup|macro> definitions. If an accessible occurrence is found, the
    child is declared <verbatim|ACCESSIBLE_ALWAYS> and the accumulated
    environment is stored as the environment of the child (with references
    to other arguments rewritten into argument numbers by
    <cpp|rewrite_symbolic_arguments>). The type of the child is taken from
    the context in which the argument occurs.
  </itemize>

  For instance, for a definition
  <scm|(assign "my-math" (macro "body" (with "mode" "math" (arg "body"))))>
  the heuristics find that the argument is accessible, that it is in math
  mode, and that <markup|my-math> is with-like. Arguments which only
  occur inside computations (such as <markup|merge>, or <markup|extern>
  with default DRD) are not accessible, which matches the fact that they
  are typeset as decorations.

  <cpp|heuristic_init_xmacro> computes a minimal arity from the largest
  index <verbatim|i> used in <scm|(arg x i)> or <markup|map-args> and
  declares a repeated arity (<verbatim|ARITY_REPEAT>).
  <cpp|heuristic_init_parameter> declares a variable of arity zero and
  guesses its type from its name (<verbatim|color>, <verbatim|*-color>,
  <verbatim|*-length>, <verbatim|*-width>) or from its value (booleans,
  integers, numbers, lengths).

  The heuristics are run at two moments: on the environment produced by
  the style (in <cpp|compute_env_and_drd>, see <hlink|the typesetting
  environment|macro-expansion-env.en.tm>), and on the initial environment
  of the document (<cpp|typeset_preamble>). <cpp|edit_typeset_rep::drd_update>
  runs them once more on the environment at the cursor, which takes into
  account macros defined in the body of the document.

  <section|Explicit declarations: <markup|drd-props>>

  <cpp|edit_env_rep::exec_drd_props> interprets
  <scm|(drd-props tag prop1 val1 ...)>. Each recognized property is set
  and frozen in <cpp|drd>:

  <\description>
    <item*|<verbatim|arity>>A number, or a tuple <verbatim|repeat>,
    <verbatim|repeat*> or <verbatim|options> with two numbers.

    <item*|<verbatim|name>, <verbatim|syntax>>Attributes used by the user
    interface.

    <item*|<verbatim|border>><verbatim|yes>, <verbatim|inner>,
    <verbatim|outer> or <verbatim|no>.

    <item*|<verbatim|with-like>><verbatim|yes> or <verbatim|no>.

    <item*|<verbatim|locals>>An environment for all children.

    <item*|<verbatim|accessible>, <verbatim|hidden>,
    <verbatim|unaccessible>>With a child number, <verbatim|all> or
    <verbatim|none>.

    <item*|<verbatim|normal-writability>, <verbatim|disable-writability>,
    <verbatim|enable-writability>>With a child number or <verbatim|all>.

    <item*|<verbatim|returns>, <verbatim|parameter>,
    <verbatim|macro-parameter>>A type name, for the tag itself.

    <item*|a type name>(as accepted by <cpp|drd_encode_type>, such as
    <verbatim|regular>, <verbatim|length>, <verbatim|boolean>) with a child
    number or <verbatim|all>: the type of children.
  </description>

  Note that <markup|drd-props> acts on the DRD passed to the environment
  at the moment it is <em|executed>: in a style file, this is the DRD
  being built by <cpp|compute_env_and_drd>, which is then cached along with
  the environment.

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
