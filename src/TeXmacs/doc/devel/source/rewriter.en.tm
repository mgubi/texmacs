<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The experimental style rewriter>

  <section|Introduction>

  The directory <verbatim|src/src/Style/> contains a second, independent
  implementation of the evaluation of <TeXmacs> documents. Its goal is to
  compute the <em|style rewriting> of a whole document, that is the tree
  obtained by expanding all macros and evaluating all primitives,
  <em|incrementally>: after a small change of the document, only the
  computations which depend on the change should be redone. To this end it
  combines three ideas:

  <\itemize>
    <item><em|persistent environments>, which are never modified in place,
    so that two evaluations in the same environment can be recognized by
    comparing pointers;

    <item>a <em|clean copy> of the document, which is updated functionally
    after each modification and therefore shares all unmodified subtrees
    with its previous version;

    <item>systematic <em|memoization> of every evaluation step, indexed by
    the identities of the input environment and of the input tree.
  </itemize>

  The code is a prototype. It is not compiled by default, it is not used by
  the typesetter, and in its present state it does not even compile when it
  is enabled (see <hlink|pitfalls|#rewriter-pitfalls>). The short overview in
  <hlink|the experimental evaluator in
  <verbatim|Style/>|macro-expansion-style.en.tm> describes the design; this
  chapter documents the code in more detail, so that it can be repaired,
  evaluated or removed with full knowledge of what it does. The semantics
  of the primitives themselves are those of the real evaluator described in
  <hlink|macro expansion and evaluation|macro-expansion.en.tm>.

  File names below are relative to <verbatim|src/src/>.

  <section|Status>

  <\description>
    <item*|Build>Off by default. The CMake option
    <verbatim|ENABLE_EXPERIMENTAL> (<verbatim|CMakeLists.txt:466>) adds all
    of <verbatim|Style/*.cpp> to the sources and defines
    <verbatim|EXPERIMENTAL>; without it, <verbatim|TeXmacs_Style_SRCS> is
    empty. With the autotools build, <verbatim|configure
    --enable-experimental> sets <verbatim|CONFIG_EXPERIMENTAL> to
    <verbatim|"Memorizer Environment Evaluate"> and defines
    <verbatim|EXPERIMENTAL>; <verbatim|makefile.in> then compiles those
    three subdirectories (<verbatim|style_src>). In a default build, no file
    of <verbatim|Style/> is compiled at all.

    <item*|Use>When enabled, the editor maintains a clean copy of its
    document and re-evaluates it after every change, but the result is only
    <em|printed on the console>; nothing in the typesetter, the editor or
    <scheme> reads it. See <hlink|integration with the
    editor|rewriter-integration.en.tm>.

    <item*|Completeness>The evaluator handles 126 of the 175 primitives
    known to <cpp|edit_env_rep::exec>. Several of the handled ones are
    placeholders (fixed lengths, <markup|drd-props>, bindings). See
    <hlink|the evaluator|rewriter-evaluator.en.tm>.

    <item*|Health>Three files of <verbatim|Style/Evaluate/> no longer
    compile, because functions they use have moved to headers they do not
    include. Every call of the main routines also prints a trace on the
    console.
  </description>

  <section|Overview>

  <\verbatim-code>
    \ \ \ editor (EXPERIMENTAL)

    \ \ \ \ \ et[rp] \ \ \ --- edit_done / copy_announce ---\<gtr\> \ cct \ (clean copy)

    \ \ \ \ \ typesetting env --- environment_update / primitive ---\<gtr\> ste

    \ \ \ \ \ apply_changes: \ \ mem= evaluate (ste, cct)

    \;

    \ \ \ evaluate (env, t) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Style/Evaluate/evaluate_main.cpp

    \ \ \ \ \ \|-- memorizer lookup by (env pointer, tree pointer)

    \ \ \ \ \ \|-- evaluate_impl: dispatch on the label \ \ \ evaluate_*.cpp

    \ \ \ \ \ \|-- assign / begin_with / end_with \ \ \ \ \ \ \ std_environment.cpp

    \ \ \ \ \ \| \ \ \ (new persistent environments, memoized too)

    \ \ \ \ \ \`-- result tree + output environment stored in the memorizer

    \;

    \ \ \ memorizers: hash-consed in a global table \ \ Style/Memorizer/memorizer.cpp
  </verbatim-code>

  <section|Source files>

  <\description>
    <item*|<verbatim|Style/Environment/environment.hpp>>The abstract
    <cpp|environment_rep> (integer keys obtained with
    <cpp|make_tree_label>), the handle <cpp|environment>, the identity
    functions <cpp|weak_hash> and <cpp|weak_equal> for trees and
    environments, and the choice <verbatim|ALTERNATIVE_MACRO_EXPANSION>
    versus <verbatim|CLASSICAL_MACRO_EXPANSION>.

    <item*|<verbatim|assoc_environment.*>, <verbatim|basic_environment.*>,
    <verbatim|list_environment.*>>Small association arrays, open hash
    tables, and linked lists of hash tables with automatic compression.

    <item*|<verbatim|std_environment.*>>The environment used by the
    evaluator, <cpp|primitive>, and the memoized operations
    <cpp|assign>, <cpp|begin_with>, <cpp|end_with> (and <cpp|macro_down>,
    <cpp|macro_redown>, <cpp|macro_up> for the classical macro expansion).

    <item*|<verbatim|Style/Memorizer/memorizer.*>>The memorizer classes, the
    global hash-consing table and the stack of sub-computations.

    <item*|<verbatim|Style/Memorizer/clean_copy.*>>Maintenance of the clean
    copy: <cpp|copy_ip> and <cpp|copy_announce>.

    <item*|<verbatim|Style/Evaluate/evaluate_main.*>>The memoized
    <cpp|evaluate>, the dispatcher <cpp|evaluate_impl> and the declarations
    of all <cpp|evaluate_*> routines.

    <item*|<verbatim|evaluate_macro.cpp>>Assignments, <markup|with>,
    values, macro application by substitution (<cpp|expand>),
    <markup|drd-props>.

    <item*|Other <verbatim|evaluate_*.cpp>>The families of primitives:
    <verbatim|evaluate_control.cpp>, <verbatim|evaluate_boolean.cpp>,
    <verbatim|evaluate_numeric.cpp>, <verbatim|evaluate_textual.cpp>,
    <verbatim|evaluate_length.cpp> and <verbatim|evaluate_quote.cpp>.

    <item*|<verbatim|evaluate_rewrite.cpp>,
    <verbatim|evaluate_inactive.cpp>>The memoized rewriting of
    <markup|extern>, <markup|include>, <markup|with-package> and the
    rendering of inactive markup.

    <item*|<verbatim|evaluate_misc.cpp>>Formatting tags, tables,
    <markup|hard-id>, scripts, bindings, patterns and points.
  </description>

  The hooks in the editor are in <verbatim|Edit/editor.hpp>,
  <verbatim|Edit/Editor/edit_main.cpp>,
  <verbatim|Edit/Editor/edit_typeset.cpp>,
  <verbatim|Edit/Modify/edit_modify.cpp> and
  <verbatim|Edit/Interface/edit_interface.cpp>, all under
  <verbatim|#ifdef EXPERIMENTAL>.

  <section|Contents of this chapter>

  <\traverse>
    <branch|Integration with the editor|rewriter-integration.en.tm>

    <branch|Persistent environments and memoization|rewriter-memoization.en.tm>

    <branch|The evaluator and its coverage|rewriter-evaluator.en.tm>
  </traverse>

  <section|Pitfalls and known bugs><label|rewriter-pitfalls>

  <\itemize>
    <item><with|font-series|bold|The experimental build does not compile.>
    Checked with <verbatim|clang++ -fsyntax-only -DEXPERIMENTAL=1> on every
    file of <verbatim|Style/>:

    <\itemize>
      <item><verbatim|Style/Evaluate/evaluate_numeric.cpp:214-215> uses
      <cpp|is_color_name> and <cpp|named_color>, now declared in
      <verbatim|Graphics/Colors/colors.hpp>, which is not included;

      <item><verbatim|Style/Evaluate/evaluate_rewrite.cpp:142> calls
      <cpp|exec_string>, which is a member of <cpp|edit_env_rep>
      (<verbatim|Typeset/env.hpp:445>), not a free function
      (<cpp|evaluate_string> is meant), and line 143 uses
      <cpp|with_package_definitions> from
      <verbatim|Texmacs/Data/new_buffer.hpp>, which is not included;

      <item><verbatim|Style/Evaluate/evaluate_textual.cpp:128> uses
      <cpp|get_date> from <verbatim|System/Language/locale.hpp>, which is
      not included.
    </itemize>

    The other files of <verbatim|Style/> pass the syntax check. The editor
    files could not be checked this way because they need the <name|Qt>
    headers.

    <item><with|font-series|bold|Environment changes are never memoized.>
    <cpp|assign> and <cpp|begin_with> are memoized on the pointer of their
    <cpp|assoc_environment> argument (<cpp|assign_memorizer_rep::hash>,
    <verbatim|std_environment.cpp:138>), but <cpp|evaluate_assign> and
    <cpp|evaluate_with> build a fresh <cpp|assoc_environment> at each call
    (<verbatim|evaluate_macro.cpp:24>, <verbatim|37>). Hence each assignment
    or <markup|with> on the path of a modification produces a new
    environment object, and every evaluation in that environment (the body
    of the <markup|with>, the siblings following the assignment) misses the
    memo table, which is keyed on the environment pointer. Incrementality
    therefore only holds for subtrees which are evaluated in an unchanged
    environment object. (Inferred from the code; not measured.)

    <item><with|font-series|bold|Dangling entry in the memo table.> When the
    constructor <cpp|memorizer (memorizer_rep*)> overwrites a slot of the
    sub-computation stack and the previous occupant loses its last
    reference, it is deleted with <cpp|tm_delete> but <em|not> removed from
    the global table (<verbatim|Style/Memorizer/memorizer.cpp:296-298>);
    the destructor and the assignment operator call <cpp|bigmem_remove>
    first. A later lookup of an equal memorizer would then compare against
    freed memory. Whether this situation is reachable has not been
    determined.

    <item><cpp|evaluate (environment, tree)> on an <em|atomic> tree returns
    before any memorizer is created, so <cpp|memorize_finalize> returns a
    null memorizer and the caller in <verbatim|edit_interface.cpp:846-847>
    would dereference it. The document body is always compound, so this is
    latent.

    <item>Creating a memorizer outside of <cpp|memorize_initialize> /
    <cpp|memorize_finalize> dereferences the null stack
    (<verbatim|memorizer.cpp:293>). All current constructions happen
    inside <cpp|evaluate (environment, tree)>.

    <item>The main routines print unconditionally (<cpp|cout> at
    <verbatim|evaluate_main.cpp:417-436>, <verbatim|evaluate_rewrite.cpp:186-202>,
    <verbatim|evaluate_inactive.cpp:478-493>), and the editor prints the
    whole rewritten document after each change; on large documents this
    dominates the run time.

    <item>The two evaluators share no code. A semantic fix made in
    <verbatim|Typeset/Env/> must be made a second time here if this code is
    ever revived.
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
