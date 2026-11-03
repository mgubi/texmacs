<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Pitfalls and debugging hints>

  <section|Pitfalls for macro writers>

  <\description>
    <item*|Computing with an argument makes it read-only>Any primitive which
    is <em|evaluated> rather than <em|typeset> (arithmetic,
    <markup|merge>, <markup|change-case>, <markup|eval>, <markup|extern>
    with <markup|arg>, <markup|value> of a stored tree, ...) produces a fresh
    tree which is displayed as a decoration. If an argument must stay
    editable, only pass it through typesetting constructs (<markup|with>,
    <markup|if>, <markup|concat>, other macros, ...), use
    <markup|quote-arg> for <markup|extern>, and use <markup|mark> or
    <markup|expand-as> to tell the editor which argument a computed
    rendering stands for.

    <item*|No closures>A macro does not capture the arguments of the macro
    in which it is defined. If the body of <markup|foo> executes
    <scm|(assign "bar" (macro "y" (arg "x")))>, then <scm|(arg "x")> is
    looked up in the frame of <markup|bar> when <markup|bar> is applied, and
    yields the error <verbatim|arg x>. Use <markup|quasi> or
    <markup|quasiquote> with <markup|unquote> to insert the value at
    definition time.

    <item*|Assignments are global>An <markup|assign> in a macro body is not
    undone at the end of the macro, and an <markup|assign> in the document
    affects all following paragraphs. Use <markup|with> for local changes.

    <item*|<markup|with> evaluates all values first>In
    <scm|(with "a" "1" "b" (value "a") body)>, the variable <verbatim|b>
    receives the <em|old> value of <verbatim|a>, because all new values are
    computed before any of them is written.

    <item*|Missing arguments>An application with fewer arguments than
    parameters binds the remaining parameters to <verbatim|UNINIT>, which
    the concater typesets like an error; extra arguments are silently
    ignored. Use an <markup|xmacro> together with <markup|get-arity> when
    the number of arguments varies.

    <item*|Conditions must be <verbatim|true> or <verbatim|false>><markup|if>,
    <markup|case> and <markup|while> reject any other value with an error.

    <item*|Style changes are cached>The environment and DRD of every style
    tuple are cached in memory and on disk in
    <verbatim|$TEXMACS_HOME_PATH/system/cache>. After editing a
    <verbatim|.ts> file outside <TeXmacs>, call <scm|style-clear-cache>.

    <item*|DRD declarations freeze properties>A property set with
    <markup|drd-props> can no longer be changed by the heuristics. Values
    which <cpp|exec_drd_props> does not recognize are silently ignored, but
    in several cases the property is frozen anyway; for instance
    <verbatim|with-like> only accepts <verbatim|yes> and <verbatim|no>, but
    <cpp|freeze_with_like> is called for any value.
  </description>

  <section|Pitfalls for C++ developers>

  <\description>
    <item*|Keep evaluation and typesetting consistent>The semantics of a
    primitive are implemented at least twice: in <cpp|edit_env_rep::exec>
    (and <cpp|exec_until>, <cpp|expand>, <cpp|depends>) and in the concater
    and bridge code. Several functions carry comments asking to keep them
    in sync (e.g. <cpp|exec_if> and <cpp|concater_rep::typeset_if>). When
    adding a primitive, also consider <cpp|exec_until> (otherwise the
    environment at the cursor is wrong inside it), <cpp|depends> (otherwise
    edits of macro arguments may not invalidate it) and the DRD in
    <source-link|drd_std.cpp|src/Data/Drd/drd_std.cpp>.

    <item*|Restore the argument stacks>Every push of <cpp|macro_arg> must
    be matched by a push of <cpp|macro_src>, and every temporary pop (as in
    <cpp|exec_arg>) must restore both lists, also on error paths.
    <cpp|exec_eval_args> reads <cpp|macro_arg-\<gtr\>item> only after
    checking that the stack is not empty; <cpp|exec_arg> and friends return
    an error tree in that case. A mismatch typically shows up much later as
    arguments resolved in the wrong frame.

    <item*|Use monitored writes for persistent changes>A persistent change of
    the environment made with <cpp|write> or <cpp|write_update> is not
    recorded in <cpp|back>, and is therefore lost when the enclosing bridge
    is later replayed from its cached <cpp|changes>. Conversely, a
    temporary change must be restored exactly, or it leaks into the
    following paragraphs.

    <item*|Call <cpp|update>>Writing a built-in variable without
    <cpp|update> leaves the C++ caches (<cpp|fn>, <cpp|pen>, <cpp|mode>,
    ...) stale.

    <item*|Values are stored evaluated, except macros>The getters
    <cpp|get_int>, <cpp|get_length> etc. do not evaluate the stored value,
    and return a default for compound values. Values stored by
    <cpp|assign> and <markup|with> are already evaluated, but values
    written directly from C++ or taken from <cpp|default_env> may be
    expressions (for instance lengths such as <scm|(plus ...)>), which is
    why <cpp|as_tmlen> and <cpp|exec_value> evaluate their argument.

    <item*|Rewriting is repeated>For <markup|extern>, <cpp|rewrite> calls
    the <scheme> function each time the tree is typeset, evaluated or
    searched by <cpp|exec_until> (<cpp|exec_until_rewrite> rewrites again).
    <scheme> functions used through <markup|extern> should be cheap and free
    of side effects.

    <item*|Dependencies through rewriting are not tracked><cpp|depends>
    only sees <markup|arg>, <markup|quote-arg>, <markup|map-args> and
    <markup|eval-args> in the unrewritten tree. A macro which accesses its
    argument in a more indirect way (for instance by building a tree which
    is later evaluated) may not be re-typeset when the argument changes.

    <item*|Global state>Several pieces of state are global:
    <cpp|the_drd>, the <cpp|current_rewrite_env> used by
    <scm|texmacs-exec>, the static <cpp|quote_substitute> flag, the style
    caches in <source-link|new_style.cpp|src/Data/Document/new_style.cpp> and <cpp|default_env>. On a cache
    miss, <cpp|typeset_style_use_cache> stores in the editor the
    <cpp|drd_info> object which is also kept in the style cache
    (<cpp|drd_cached>); since <cpp|drd_info> has reference semantics, the
    later <cpp|heuristic_init> calls for the document operate on that
    shared object.

    <item*|A suspicious line>In <cpp|edit_env_rep::exec_provide>, the test
    reads <cpp|if (provides (t-\<gtr\>label)) return "";>, where <cpp|t> is the
    compound <markup|provide> tree; the typesetting counterpart
    <cpp|concater_rep::typeset_provide> tests the evaluated variable name.
    Be careful when relying on <markup|provide> in evaluated (as opposed to
    typeset) context, for instance in style files.
  </description>

  <section|Debugging hints>

  <\itemize>
    <item>Most functions in <source-link|env_exec.cpp|src/Typeset/Env/env_exec.cpp> and in the bridges
    contain commented-out trace statements (for instance
    <cpp|// cout \<less\>\<less\> "Execute: " \<less\>\<less\> t \<less\>\<less\> "\\n";> at the start of
    <cpp|exec>, or the traces in <cpp|notify_macro>). Uncommenting them is
    the quickest way to follow an expansion.

    <item>An <cpp|edit_env> can be printed with <cpp|operator \<less\>\<less\>>,
    which prints the whole variable table.

    <item>From <scheme>, <scm|(texmacs-exec '(value "font-size"))> evaluates
    markup in the current environment and <scm|texmacs-exec*> in the
    environment at the cursor; <scm|get-env>, <scm|get-env-tree> and
    <scm|get-full-env> give access to the environment at the cursor.

    <item>Switching a document to source mode, or wrapping a fragment in
    <markup|inactive*>, shows the markup as the kernel sees it; error trees
    produced by the evaluator (for instance <verbatim|compound foo> for an
    undefined tag, or <verbatim|arg x> for an unbound argument) are
    displayed in place.
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
