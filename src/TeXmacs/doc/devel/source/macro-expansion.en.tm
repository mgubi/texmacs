<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Macro expansion and evaluation>

  <section|Introduction>

  A <TeXmacs> document is a tree. Only a fixed set of tags (<markup|concat>,
  <markup|with>, <markup|frac>, <markup|macro>, <markup|arg>, ...) is known
  to the C++ kernel; all other tags, such as <markup|section>,
  <markup|theorem> or <markup|strong>, are <em|macros> which are defined in
  style files by assignments like

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello <arg|name>!>>>
  </tm-fragment>

  Such definitions are stored in the <em|environment>, a map from names to
  trees which also contains all ordinary typesetting variables
  (<verbatim|font-size>, <verbatim|par-width>, <verbatim|mode>, ...). When
  the typesetter meets a tag which is not a built-in primitive, it looks up
  the tag name in the environment and, if a macro is found there, expands it.

  This chapter explains how this works inside the kernel. It covers:

  <\itemize>
    <item>the typesetting environment, <cpp|edit_env>, and the way it is
    initialized from style files;

    <item>the evaluator <cpp|edit_env_rep::exec>, which computes the
    <em|value> of a tree (used for environment variables, conditions,
    lengths and so on);

    <item>macro expansion <em|during typesetting>, which is done lazily by
    the typesetter itself so that the arguments of a macro remain editable
    and keep their correct source locations;

    <item>the <em|data relation descriptor> (<abbr|DRD>), which records
    properties of tags such as their arity or the accessibility of their
    children, and which is computed to a large extent from the macro
    definitions;

    <item>the experimental memoizing evaluator in <verbatim|Style/>.
  </itemize>

  The semantics of the individual primitives from the point of view of a
  style file writer are documented in the chapter on <hlink|primitives for
  writing style files|../format/stylesheet/stylesheet.en.tm>, in particular
  the <hlink|macro primitives|../format/stylesheet/prim-macro.en.tm>, the
  <hlink|environment primitives|../format/stylesheet/prim-env.en.tm> and the
  <hlink|evaluation control
  primitives|../format/stylesheet/prim-evaluation.en.tm>. The present chapter
  is about the implementation. The line and page breaking algorithms, and the
  general organization of the typesetter in bridges and concaters, are
  described in the chapter on the <hlink|typesetter|typesetter.en.tm>.

  All C++ file names below are relative to <verbatim|src/src/>, and all
  <scheme> file names are relative to <verbatim|src/TeXmacs/progs/>.

  <section|Two ways of evaluating a tree>

  The single most important thing to understand about <TeXmacs> macros is
  that there are two quite different ways in which a macro application is
  processed.

  <\description>
    <item*|Evaluation (<cpp|exec>)>The method
    <cpp|tree edit_env_rep::exec (tree t)> in
    <verbatim|Typeset/Env/env_exec.cpp> computes the <em|value> of
    <cpp|t> in the current environment: macros are expanded, arguments are
    substituted, arithmetic and string primitives are computed,
    <markup|if>-conditions are decided, and the result is a new tree. For
    instance, with the above definition of <markup|hello>, evaluating
    <scm|(hello "world")> yields <scm|(concat "Hello " "world" "!")>. The
    result is a fresh tree which is not attached to the document, so it has
    no meaningful source location. Evaluation is used whenever the typesetter
    needs a value: the new values in a <markup|with>, the condition of an
    <markup|if>, a length, the value of <markup|value>, the arguments of
    <markup|extern>, and so on.

    <item*|Typesetting>When the typesetter meets <scm|(hello "world")> in
    the document, it does <em|not> call <cpp|exec> on it. Instead it pushes a
    frame which binds the macro parameter <verbatim|name> to the <em|source
    subtree> <scm|"world"> together with its source path, and then typesets
    the macro body <scm|(concat "Hello " (arg "name") "!")> directly. The
    strings of the body get a <em|decoration> path (they are not editable),
    while the <markup|arg> primitive typesets the original subtree with its
    own inverse path, so that the cursor can be moved into
    <verbatim|world> and the user can edit it. The code for this lives in
    <verbatim|Typeset/Concat/concat_macro.cpp> (inline material) and in the
    bridges <verbatim|Typeset/Bridge/bridge_compound.cpp>,
    <verbatim|bridge_argument.cpp> etc. (paragraph-level material).
  </description>

  Both mechanisms share the same environment object and the same stacks of
  macro arguments (<cpp|edit_env_rep::macro_arg> and
  <cpp|edit_env_rep::macro_src>). They must be kept consistent: several
  functions in <verbatim|env_exec.cpp> carry comments such as <em|this case
  must be kept consistent with <cpp|concater_rep::typeset_if>>.

  There is a third, related, mechanism: <em|rewriting>
  (<cpp|edit_env_rep::rewrite>). A few primitives (<markup|extern>,
  <markup|include>, <markup|with-package>, <markup|map-args>,
  <markup|rewrite-inactive>) are first rewritten into an ordinary tree, which
  is then typeset (or evaluated). Rewriting is designed so that parts of the
  result which come from the document keep their source location.

  Finally, the directory <verbatim|Style/> contains an experimental,
  memoizing re-implementation of the evaluator which works on persistent
  environments. It is only compiled when <TeXmacs> is configured with the
  <verbatim|ENABLE_EXPERIMENTAL> option of <verbatim|CMakeLists.txt> (which
  defines the preprocessor symbol <verbatim|EXPERIMENTAL>) and it is not used
  for the actual typesetting; see <hlink|the experimental
  evaluator|macro-expansion-style.en.tm>.

  <section|Source map>

  <\description-paragraphs>
    <item*|<verbatim|Typeset/env.hpp>>Declaration of the class
    <cpp|edit_env_rep>, the <verbatim|Env_*> categories of environment
    variables and various constants.

    <item*|<verbatim|Typeset/Env/env.cpp>>Construction of the environment,
    global manipulations (<cpp|write_env>, <cpp|patch_env>,
    <cpp|read_env>) and the bookkeeping which allows bridges to cache their
    effect on the environment (<cpp|local_start>, <cpp|local_update>,
    <cpp|local_end>).

    <item*|<verbatim|Typeset/Env/env_exec.cpp>>The evaluator:
    <cpp|exec>, <cpp|rewrite>, the partial evaluator <cpp|exec_until>,
    <cpp|expand> and <cpp|depends>, and the implementation of all
    computational primitives.

    <item*|<verbatim|Typeset/Env/env_semantics.cpp>>The categories of the
    built-in environment variables (<cpp|initialize_default_var_type>) and
    the <cpp|update> methods which recompute cached C++ fields (fonts,
    colors, page parameters, ...) when a variable changes.

    <item*|<verbatim|Typeset/Env/env_default.cpp>>The default environment
    <cpp|default_env>, built by <cpp|initialize_default_env>.

    <item*|<verbatim|Typeset/Env/env_length.cpp>>Length arithmetic and
    length units.

    <item*|<verbatim|Typeset/Env/env_inactive.cpp>>Rewriting of inactive
    markup (source code display of macros).

    <item*|<verbatim|Typeset/Env/env_animate.cpp>>Evaluation of animations.

    <item*|<verbatim|Typeset/Concat/concat_macro.cpp>>Inline typesetting of
    <markup|with>, <markup|assign>, macro applications, <markup|arg>,
    <markup|mark>, <markup|expand-as>, <markup|eval>, rewritable and
    executable primitives.

    <item*|<verbatim|Typeset/Bridge/bridge_compound.cpp>,
    <verbatim|bridge_argument.cpp>, <verbatim|bridge_with.cpp>,
    <verbatim|bridge_rewrite.cpp>, <verbatim|bridge_eval.cpp>,
    <verbatim|bridge_expand_as.cpp>>The corresponding paragraph-level
    (incremental) typesetting.

    <item*|<verbatim|Data/Drd/>>The data relation descriptor:
    <cpp|drd_info> (<verbatim|drd_info.hpp>), <cpp|tag_info>
    (<verbatim|tag_info.hpp>), the standard <abbr|DRD> for built-in tags
    (<verbatim|drd_std.cpp>), the global access modes
    (<verbatim|drd_mode.hpp>) and the names of built-in environment
    variables (<verbatim|vars.hpp>).

    <item*|<verbatim|Data/Document/new_style.cpp>>Computation and caching of
    the environment and the <abbr|DRD> of a style.

    <item*|<verbatim|Edit/Editor/edit_typeset.cpp>>Initialization of the
    environment of a buffer (<cpp|typeset_preamble>,
    <cpp|typeset_prepare>) and computation of the environment at the cursor
    (<cpp|typeset_exec_until>).

    <item*|<verbatim|Style/>>The experimental memoizing evaluator.

    <item*|<verbatim|kernel/texmacs/tm-secure.scm>>The <scheme> side of the
    security check for <markup|extern>.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|The typesetting environment|macro-expansion-env.en.tm>

    <branch|The evaluator|macro-expansion-exec.en.tm>

    <branch|Macro expansion during typesetting|macro-expansion-typeset.en.tm>

    <branch|The data relation descriptor|macro-expansion-drd.en.tm>

    <branch|The experimental evaluator in
    <verbatim|Style/>|macro-expansion-style.en.tm>

    <branch|Pitfalls and debugging hints|macro-expansion-pitfalls.en.tm>
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
