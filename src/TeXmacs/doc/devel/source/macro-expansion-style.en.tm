<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The experimental evaluator in <verbatim|Style/>>

  <section|Status>

  The directory <verbatim|src/src/Style/> contains a second implementation
  of the evaluator, written with the aim of computing the <em|style
  rewriting> of a whole document (the tree obtained by expanding all
  macros) incrementally, through systematic memoization. It is a
  prototype:

  <\itemize>
    <item>It is only compiled when <TeXmacs> is configured with the CMake
    option <verbatim|ENABLE_EXPERIMENTAL> (or the corresponding
    <verbatim|CONFIG_EXPERIMENTAL> setting of <source-link|makefile.in|src/makefile.in>), which
    defines the preprocessor symbol <verbatim|EXPERIMENTAL>; the default is
    off.

    <item>It is not used by the typesetter. When enabled, the editor keeps
    a <em|clean copy> <cpp|cct> of the document (created in the
    constructor of <cpp|edit_main_rep> with <cpp|copy_ip>, and kept up to
    date by <cpp|copy_announce> in <cpp|edit_done>), a persistent
    environment <cpp|ste> (rebuilt from the typesetting environment by
    <cpp|edit_typeset_rep::environment_update>, which calls
    <cpp|primitive (ste, h)>) and a memorizer <cpp|mem>. After each change
    of the tree, <cpp|edit_interface_rep> recomputes
    <cpp|mem= evaluate (ste, cct)> and prints the rewritten tree on the
    console.

    <item>The main routines <cpp|evaluate> and <cpp|rewrite> print a trace
    of every call unconditionally, and <markup|drd-props> is not implemented
    (<cpp|evaluate_drd_props> returns the empty string).
  </itemize>

  It is nevertheless useful to know about it, both because its code is
  easy to confuse with the real evaluator (the function names differ only
  in <cpp|evaluate_*> versus <cpp|exec_*>) and because it illustrates a
  different design.

  <section|Persistent environments>

  <source-link|Style/Environment/environment.hpp|src/Style/Environment/environment.hpp> defines an abstract class
  <cpp|environment_rep> with methods <cpp|contains>, <cpp|read>,
  <cpp|write>, <cpp|remove> and <cpp|print>, keyed by integers: variable
  names are converted to integers with <cpp|make_tree_label>. The
  implementations are

  <\description>
    <item*|<cpp|assoc_environment>>A small array of key/value pairs, used
    for the bindings of one <markup|with>, <markup|assign> or macro
    application.

    <item*|<cpp|basic_environment>>An open hash table stored in an array of
    <cpp|hash_node>s whose size is a power of two.

    <item*|<cpp|list_environment>>A linked list of basic environments,
    looked up from the head; it counts lookup <cpp|misses> and flattens
    itself with <cpp|compress> when there are too many.

    <item*|<cpp|std_environment>>(in <source-link|std_environment.cpp|src/Style/Environment/std_environment.cpp>) The
    environment actually used by the evaluator. It has a flag <cpp|pure>, a
    list of local variables <cpp|env>, a link <cpp|next> to the enclosing
    environment, an accelerated lookup list <cpp|accel> and a list
    <cpp|args> of macro argument frames.
  </description>

  Environments are never modified in place by the evaluator. Instead, the
  functions <cpp|assign>, <cpp|begin_with> and <cpp|end_with> (and
  <cpp|macro_down>, <cpp|macro_redown>, <cpp|macro_up> in the classical
  variant, see below) replace the global <cpp|std_env> by a new environment
  which shares most of its structure with the old one. A <markup|with>
  pushes a <em|pure> environment; an <markup|assign> inside it produces an
  impure one; <cpp|end_with_environment> computes which assignments must
  survive the end of the <markup|with> and re-applies them to the enclosing
  environment. Each of these operations is itself memoized, so that
  applying the same change to the same environment returns the very same
  environment object.

  <section|Memoization>

  A <cpp|memorizer> (<source-link|Style/Memorizer/memorizer.hpp|src/Style/Memorizer/memorizer.hpp>) represents
  one computation: its inputs, its outputs and the memorizers of its
  sub-computations. Memorizers are hash-consed in a global table: the
  constructor <cpp|memorizer (memorizer_rep*)> looks for an existing
  memorizer of the same <cpp|type> which is <cpp|equal> to the new one,
  and uses it instead. For evaluation, the key is the pair formed by the
  <em|identities> (pointers, see <cpp|weak_hash> and <cpp|weak_equal>) of
  the input environment and of the input tree:

  <\cpp-code>
    tree

    evaluate (tree t) {

    \ \ if (is_atomic (t)) return t;

    \ \ ...

    \ \ memorizer mem= evaluate_memorizer (std_env, t);

    \ \ if (is_memorized (mem)) {

    \ \ \ \ ...

    \ \ \ \ std_env= mem-\<gtr\>get_environment ();

    \ \ \ \ return mem-\<gtr\>get_tree ();

    \ \ }

    \ \ memorize_start ();

    \ \ tree r= evaluate_impl (t);

    \ \ decorate_ip (t, r);

    \ \ mem-\<gtr\>set_tree (r);

    \ \ mem-\<gtr\>set_environment (std_env);

    \ \ memorize_end ();

    \ \ ...

    \ \ return mem-\<gtr\>get_tree ();

    }
  </cpp-code>

  Because the environments are persistent and shared, and because the
  clean copy of the document shares all unmodified subtrees between
  versions, evaluating a slightly modified document with
  <cpp|evaluate (environment env, tree t)> finds most sub-computations in
  the table and only recomputes the parts on the path of the modification.
  Whether a memorizer counts as already computed is decided by
  <cpp|is_memorized>, a test on its reference count. The functions
  <cpp|memorize_initialize>, <cpp|memorize_start>, <cpp|memorize_end> and
  <cpp|memorize_finalize> maintain the stack from which the tree of
  sub-computations is built.

  The results receive inverse paths through <cpp|decorate_ip (t, r)>, which
  attaches <cpp|decorate_right (obtain_ip (t))> to all detached parts of the
  result, and through <cpp|transfer_ip> for componentwise evaluation.

  <section|Macro expansion by substitution>

  <source-link|environment.hpp|src/Style/Environment/environment.hpp> defines
  <verbatim|ALTERNATIVE_MACRO_EXPANSION> (the alternative,
  <verbatim|CLASSICAL_MACRO_EXPANSION>, is commented out). In this mode
  <cpp|evaluate_compound> does not push argument frames. It builds an
  <cpp|assoc_environment> binding the parameters to the argument trees and
  <em|substitutes> them into the body with
  <cpp|expand (tree t, assoc_environment env)>
  (<source-link|Style/Evaluate/evaluate_macro.cpp|src/Style/Evaluate/evaluate_macro.cpp>):

  <\itemize>
    <item><scm|(arg x i1 ... ik)> is replaced by the corresponding subtree
    of the argument (the indices are evaluated);

    <item><scm|(quote-arg x ...)> becomes a <markup|quote> of it;

    <item><markup|map-args> is expanded directly into the children of the
    argument;

    <item>nested <markup|macro> and <markup|xmacro> definitions shadow the
    parameters they rebind;

    <item>unchanged subtrees are shared with the body (<cpp|weak_equal>
    tests), which is essential for memoization.
  </itemize>

  The substituted body is then decorated with the inverse path of the
  application and evaluated. The classical variant, with
  <cpp|macro_down> and <cpp|macro_up> managing a stack of argument frames
  inside <cpp|std_environment>, mirrors the implementation in
  <verbatim|Typeset/Env/>.

  <section|Relation with <verbatim|Typeset/Env/>>

  The two evaluators implement the same primitives with the same names
  (compare <cpp|evaluate_with> and <cpp|edit_env_rep::exec_with>,
  <cpp|rewrite> and <cpp|edit_env_rep::rewrite>, the re-entrancy variable
  <cpp|reenter_rewrite_env> and <cpp|current_rewrite_env>), but they do not
  share code, and the Style evaluator has not been kept in sync with all
  later additions to <source-link|env_exec.cpp|src/Typeset/Env/env_exec.cpp> (themes, animations, several
  graphical primitives). The typesetter only uses
  <cpp|edit_env_rep::exec>. If you fix a bug in the semantics of a
  primitive, the place to fix it is <verbatim|Typeset/Env/> (together with
  the corresponding typesetting code in <verbatim|Typeset/Concat/> and
  <verbatim|Typeset/Bridge/>).

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
