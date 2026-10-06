<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The evaluator of the style rewriter and its coverage>

  This page describes <verbatim|src/src/Style/Evaluate/> and compares it
  with the real evaluator <cpp|edit_env_rep::exec>
  (<source-link|Typeset/Env/env_exec.cpp|src/Typeset/Env/env_exec.cpp>), whose semantics are documented in
  <hlink|evaluation of primitives|macro-expansion-exec.en.tm>.

  <section|Entry points>

  <\explain>
    <cpp|memorizer evaluate (environment env, tree t)><explain-synopsis|top
    level>
  <|explain>
    Opens the stack of sub-computations (<cpp|memorize_initialize>), makes
    <cpp|env> the current environment <cpp|std_env>, evaluates <cpp|t>,
    restores the previous environment and returns the top-level memorizer
    (<cpp|memorize_finalize>). The editor calls it as
    <cpp|mem= evaluate (ste, cct)>.
  </explain>

  <\explain>
    <cpp|tree evaluate (tree t)><explain-synopsis|memoized evaluation>
  <|explain>
    Atomic trees are returned unchanged and are not memoized. For a compound
    tree, an <cpp|evaluate_memorizer> keyed on <cpp|(std_env, t)> is
    created; if it is already known (<cpp|is_memorized>), its result tree is
    returned and its output environment becomes <cpp|std_env>. Otherwise the
    evaluation proceeds in a new level of the stack: <cpp|evaluate_impl (t)>
    computes the result, <cpp|decorate_ip (t, r)> gives the new parts of
    the result the inverse path of <cpp|t>, and the result tree and the
    environment after the evaluation are stored in the memorizer. Every call
    prints <verbatim|Evaluate>, <verbatim|Memorized> or <verbatim|Computed>
    on the console.
  </explain>

  <cpp|rewrite (tree)> (<source-link|evaluate_rewrite.cpp|src/Style/Evaluate/evaluate_rewrite.cpp>) and the evaluation
  of inactive markup (<source-link|evaluate_inactive.cpp|src/Style/Evaluate/evaluate_inactive.cpp>) are memoized in the
  same way, with memorizer types <verbatim|MEMORIZE_REWRITE> and
  <verbatim|MEMORIZE_INACTIVE>, and print similar traces. Helper functions
  such as <cpp|evaluate_string> evaluate and then extract an atomic result.

  <section|Dispatching>

  <cpp|evaluate_impl> (<source-link|evaluate_main.cpp|src/Style/Evaluate/evaluate_main.cpp>) is a large
  <cpp|switch> on the label of the tree, with the same grouping as
  <cpp|edit_env_rep::exec>: typesetting primitives with side effects,
  macro expansion, control flow, boolean, arithmetic, textual and length
  operations, style and activation commands, and miscellaneous ones.
  Labels which are not handled fall into the <cpp|default> branch:

  <\itemize>
    <item>built-in labels (below <verbatim|START_EXTENSIONS>) are evaluated
    <em|componentwise>: each child is evaluated, the label is kept, and
    <cpp|transfer_ip> copies the inverse path;

    <item>user-defined labels go to <cpp|evaluate_compound>
    (<source-link|evaluate_macro.cpp|src/Style/Evaluate/evaluate_macro.cpp>), which looks the macro up in the
    environment and applies it.
  </itemize>

  Macro application uses <em|substitution> rather than argument frames
  (<verbatim|ALTERNATIVE_MACRO_EXPANSION>): the arguments are substituted
  into the body by <cpp|expand (tree, assoc_environment)>, sharing all
  unchanged subtrees, and the result is evaluated. The details are in
  <hlink|the experimental evaluator|macro-expansion-style.en.tm>.

  <section|Coverage>

  Comparing the <cpp|case> labels of <cpp|evaluate_impl> with those of
  <cpp|edit_env_rep::exec> (with commented-out code removed), the
  experimental evaluator handles 126 of the 175 labels of the real one, and
  no label that the real one does not handle. The 49 missing labels are
  evaluated componentwise by the default branch: their children are
  evaluated, but the primitive itself is not executed and stays in the
  result:

  <\description>
    <item*|Themes><verbatim|NEW_THEME>, <verbatim|COPY_THEME>,
    <verbatim|APPLY_THEME>, <verbatim|SELECT_THEME>.

    <item*|Animations><verbatim|ANIM_STATIC>, <verbatim|ANIM_DYNAMIC>,
    <verbatim|ANIM_TIME>, <verbatim|ANIM_PORTION>, <verbatim|MORPH>.

    <item*|Box placement><verbatim|MOVE>, <verbatim|SHIFT>,
    <verbatim|RESIZE>, <verbatim|CLIPPED>, and <verbatim|BOX_INFO>,
    <verbatim|FRAME_DIRECT>, <verbatim|FRAME_INVERSE> (whose cases and
    implementations are present but commented out, in
    <source-link|evaluate_main.cpp|src/Style/Evaluate/evaluate_main.cpp> and <source-link|evaluate_misc.cpp|src/Style/Evaluate/evaluate_misc.cpp>).

    <item*|Graphical effects>All <verbatim|EFF_*> labels
    (<verbatim|EFF_MOVE>, <verbatim|EFF_GAUSSIAN>, <verbatim|EFF_OVAL>,
    <verbatim|EFF_TURBULENCE>, ...: 15 labels).

    <item*|Lengths><verbatim|GUIPX_LENGTH>, <verbatim|MS_LENGTH>,
    <verbatim|S_LENGTH>, and the corner lengths <verbatim|LCORNER_LENGTH>,
    <verbatim|BCORNER_LENGTH>, <verbatim|RCORNER_LENGTH>,
    <verbatim|TCORNER_LENGTH>.

    <item*|Other primitives><verbatim|MINIMUM>, <verbatim|MAXIMUM>,
    <verbatim|RGB_COLOR>, <verbatim|RGB_ACCESS>, <verbatim|FIND_ACCESSIBLE>,
    <verbatim|FIND_FILE_UPWARDS>, <verbatim|OCCURS_INSIDE>,
    <verbatim|HAS_BINDING>, <verbatim|GET_ATTACHMENT>,
    <verbatim|FILTER_STYLE>, <verbatim|VAR_MARK>.
  </description>

  <section|Placeholders among the handled primitives>

  Several primitives are dispatched but implemented only approximately,
  because the information they need belongs to the typesetter:

  <\description>
    <item*|<markup|provide>, <markup|or-value>>Treated as <markup|assign>
    and <markup|value> respectively (marked \Pprovisory\Q in
    <source-link|evaluate_main.cpp:112|src/Style/Evaluate/evaluate_main.cpp:112>, <verbatim|122>).

    <item*|<markup|drd-props>><cpp|evaluate_drd_props> returns the empty
    string without doing anything (<verbatim|evaluate_macro.cpp:82-85>).

    <item*|Page and graphics lengths><cpp|evaluate_par_length>,
    <cpp|evaluate_paw_length> and <cpp|evaluate_pag_length> return fixed
    values (15cm, 18cm and 23cm), <cpp|evaluate_gw_length> and
    <cpp|evaluate_gh_length> return 10cm and 6cm, and
    <cpp|evaluate_gu_length> returns 1cm (<source-link|evaluate_length.cpp|src/Style/Evaluate/evaluate_length.cpp>);
    the real computations are left in comments.

    <item*|Bindings><cpp|evaluate_set_binding> and
    <cpp|evaluate_get_binding> work on two static hash tables
    <cpp|local_ref> and <cpp|global_ref> of <source-link|evaluate_misc.cpp|src/Style/Evaluate/evaluate_misc.cpp>,
    which are never connected to the references of the buffer, and
    <markup|set-binding> evaluates to the empty string (the comment says the
    work of <cpp|concater_rep::typeset_set_binding> should be done instead).

    <item*|Tables>Cell formats are not re-evaluated (FIXME in
    <cpp|evaluate_table>).
  </description>

  <section|Rewriting>

  <cpp|rewrite> handles <markup|extern> (calling <scheme>),
  <markup|map-args>, <markup|include> (loading the file relative to
  <verbatim|base-file-name> with <cpp|load_inclusion>),
  <markup|with-package> and <markup|rewrite-inactive>;
  <cpp|evaluate_rewrite> evaluates the rewritten tree. This parallels <cpp|edit_env_rep::rewrite>,
  described in <hlink|evaluation of primitives|macro-expansion-exec.en.tm>.
  The <markup|with-package> branch is one of the places that no longer
  compiles (see <hlink|the pitfalls|rewriter.en.tm>).

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
