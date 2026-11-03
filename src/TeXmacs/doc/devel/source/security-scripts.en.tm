<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Trusted documents and secure evaluation of scripts>

  <section|Where documents execute code>

  A document can cause <scheme> code to be evaluated in the following ways;
  each of them is subject to the checks described on this page.

  <\description>
    <item*|<markup|extern>>The primitive <markup|extern> calls a <scheme>
    function with the evaluated arguments during typesetting
    (<cpp|edit_env_rep::rewrite>, <source-link|Typeset/Env/env_exec.cpp|src/Typeset/Env/env_exec.cpp>; the
    experimental style evaluator has the same code in
    <source-link|Style/Evaluate/evaluate_rewrite.cpp|src/Style/Evaluate/evaluate_rewrite.cpp>).

    <item*|Links to scripts>A link whose target vertex is a
    <verbatim|(script ...)> is executed when the user follows it. This is
    how <markup|action> works: its default definition
    (<source-link|Typeset/Env/env_default.cpp|src/Typeset/Env/env_default.cpp>) creates a locus with a link of
    type <verbatim|action> to a script. Following the link ends up in
    <scm|go-to-vertex> and <scm|execute-script>
    (<source-link|link/link-navigate.scm|TeXmacs/progs/link/link-navigate.scm>).

    <item*|Observers>A locus may carry an <verbatim|(observer
    <em|id> <em|callback>)> attribute, whose callback is called when the
    locus is modified (<cpp|build_locus>, called when a <markup|locus> is typeset,
    <source-link|Typeset/Concat/concat_active.cpp|src/Typeset/Concat/concat_active.cpp>).

    <item*|Widgets and exercises>Commands attached to the buttons of
    widgets rendered inside documents are evaluated by <scm|gui-on-select>
    (<source-link|utils/misc/gui-utils.scm|TeXmacs/progs/utils/misc/gui-utils.scm>); scripts attached to the input
    fields of exercises by <scm|edu-exec>
    (<source-link|education/edu-edit.scm|TeXmacs/progs/education/edu-edit.scm>); <scheme> fragments of automatic
    documents by <scm|build-scheme*> (<source-link|utils/automate/auto-build.scm|TeXmacs/progs/utils/automate/auto-build.scm>).
  </description>

  Plug-in sessions are not in this list: evaluating a session input is an
  explicit action of the user, and the plug-in runs with the rights of the
  user anyway.

  <section|Trusted documents>

  A document is <em|trusted> (in the code: <em|secure>) if its file name
  lies below one of the directories of the search path
  <verbatim|$TEXMACS_SECURE_PATH>:

  <\cpp-code>
    bool is_secure (url u) {

    \ \ return descends (u, expand (url_path ("$TEXMACS_SECURE_PATH")));

    }
  </cpp-code>

  (<source-link|System/Classes/url.cpp|src/System/Classes/url.cpp>, exported to <scheme> as
  <scm|url-secure?>). At startup, <source-link|init_texmacs.cpp|src/System/Boot/init_texmacs.cpp> appends
  <verbatim|$TEXMACS_PATH:$TEXMACS_HOME_PATH> to whatever the user put in
  this environment variable. So the documentation and the style files of
  <TeXmacs>, and everything below the user's <TeXmacs> directory, are
  trusted.

  The trust status is recorded in the typesetting environment:

  <\itemize>
    <item>the constructor of <cpp|edit_env_rep> sets <cpp|secure> to
    <cpp|is_secure (base_file_name)>, where the base file name is the
    <em|master> of the buffer (<source-link|Typeset/Env/env.cpp|src/Typeset/Env/env.cpp>,
    <source-link|Edit/Editor/edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp>);

    <item>while an included file is typeset (<markup|include> in
    <source-link|concat_macro.cpp|src/Typeset/Concat/concat_macro.cpp>, <markup|var-include> in
    <source-link|bridge_rewrite.cpp|src/Typeset/Bridge/bridge_rewrite.cpp>), <cpp|secure> is set from the name of
    the included file and restored afterwards; the experimental style
    evaluator does the same with the environment variable
    <verbatim|secure> (<source-link|Style/Evaluate/evaluate_control.cpp|src/Style/Evaluate/evaluate_control.cpp>).
  </itemize>

  The field <cpp|secure> of <cpp|new_buffer_rep>, also initialized with
  <cpp|is_secure>, is not used by any of these checks.

  <section|The script policy>

  The user chooses a policy with the preference <verbatim|security>
  (the <verbatim|Security> item of the preferences menu, also in the preferences dialog). Its handler <scm|notify-security>
  (<source-link|texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>) translates it into the
  global <cpp|script_status> (<source-link|System/Misc/sys_utils.cpp|src/System/Misc/sys_utils.cpp>) with
  <scm|set-script-status>:

  <\description>
    <item*|0>\Paccept no scripts\Q.

    <item*|1>\Pprompt on scripts\Q (the default).

    <item*|2>\Paccept all scripts\Q.
  </description>

  The checks then work as follows.

  <\description>
    <item*|<markup|extern>>If the environment is not trusted and
    <cpp|script_status> is below 2, the expression (function and evaluated
    arguments) is passed to <scm|secure?>; if it is rejected, the result is
    the error <verbatim|"insecure script">. There is no prompt here, even
    with policy 1, since this happens during typesetting.

    <item*|Observers>The observer is registered only if the environment is
    trusted or if <verbatim|(<em|callback> #f #f #f)> passes <scm|secure?>.
    The policy is not consulted.

    <item*|Links>When a link is created inside a locus, the typesetter adds
    the attribute <verbatim|secure> with the trust status of the
    environment (<cpp|build_locus>). <scm|execute-script>
    executes the script at once if this attribute is <verbatim|true>, if
    the script passes <scm|secure?>, or if the policy is \Paccept all
    scripts\Q; with \Pprompt on scripts\Q it asks the user; otherwise it
    refuses with the message \PUnsecure script refused\Q. There are two
    code paths: <scm|old-execute-script> for scripts written as an
    expression <verbatim|(...)>, and <scm|new-execute-script> for a
    function name with arguments.

    <item*|Widgets and exercises>The commands are evaluated with
    <scm|secure-eval>, that is, only if they pass <scm|secure?>; the
    policy is not consulted. <scm|build-scheme*> skips the check when
    <scm|auto-safe-mode?> is set.
  </description>

  <section|The checker <scm|secure?>>

  <scm|secure?> (<source-link|kernel/texmacs/tm-secure.scm|TeXmacs/progs/kernel/texmacs/tm-secure.scm>) is a static
  analysis of the expression: nothing is evaluated. An expression is
  accepted by <scm|secure-expr?> if

  <\itemize>
    <item>it is a constant (number, string, boolean, tree, empty list) or a
    symbol;

    <item>it is a <scm|quote>d datum, or a <scm|quasiquote> all of whose
    unquoted parts are accepted;

    <item>its head is one of the special forms <scm|and>, <scm|begin>,
    <scm|cond>, <scm|if>, <scm|lambda>, <scm|or>, <scm|set!> or
    <scm|with>, and the sub-expressions are accepted (the variables bound
    by <scm|lambda> and <scm|with> are added to a local environment);

    <item>its head is a variable of the local environment, or a symbol
    whose property <scm|:secure> is set, and all arguments are accepted;

    <item>its head is itself a compound expression, and all elements are
    accepted.
  </itemize>

  The property <scm|:secure> is set in two ways:

  <\itemize>
    <item>by the option <scm|(:secure #t)> of <scm|tm-define>
    (<source-link|kernel/texmacs/tm-define.scm|TeXmacs/progs/kernel/texmacs/tm-define.scm>), which many editing routines
    use, among which most entry points of the encryption code;

    <item>by <scm|define-secure-symbols> for a list of primitive functions
    on booleans, lists, strings and numbers, <scm|display>,
    <scm|texmacs-version> and <scm|refresh-now>.
  </itemize>

  If the first test fails, <scm|secure?> forces the lazy loading of the
  plug-ins (<scm|lazy-plugin-force>) and tries again, since some secure
  functions are only defined once their plug-in has been loaded.
  <scm|secure-eval> evaluates an expression if it passes <scm|secure?>,
  and returns <scm|#f> otherwise.

  <section|Writing secure functions>

  Declaring a function <scm|:secure> means that <em|any> document may call
  it with <em|any> arguments it can compute. A function should therefore
  only be declared secure if it cannot read or write files, run programs,
  open network connections, change preferences or otherwise act beyond the
  current document, whatever its arguments. Opening a dialog is not
  harmless either: an untrusted document should not be able to show a
  passphrase prompt of its own choosing. See <hlink|threat model and
  pitfalls|security-pitfalls.en.tm> for the known limits of the checker.

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
