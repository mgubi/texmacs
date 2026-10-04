<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Pitfalls>

  <section|Calling <scheme> from <c++>>

  <\itemize>
    <item><strong|Errors are swallowed.> A <scheme> error
    during <cpp|eval>, <cpp|call> or <cpp|exec_file> is printed on the
    console and the call returns the pair <verbatim|(<em|key> .
    <em|args>)> (<verbatim|Scheme/Guile/guile_tm.cpp>,
    <cpp|TeXmacs_catcher>). Most <cpp|as_<em|type>> conversions then
    return a neutral value (0, <verbatim|"">, an empty tree, ...), so
    <cpp|as_int (call ("f"))> yields 0 when <scm|f> fails. Check the type
    of the result (<cpp|is_int>, <cpp|is_tree>, ...) when it matters.

    <item><strong|Calls by name are evaluated.>
    <cpp|call ("f", ...)> evaluates the string <verbatim|"f"> on every
    call. Do not build such strings from user data, and prefer to keep an
    <cpp|object> with the procedure in code which is called often.

    <item><strong|Strings versus symbols.> <cpp|object
    ("foo")> is the <scheme> string <verbatim|"foo">; use
    <cpp|symbol_object ("foo")> for the symbol.

    <item><strong|<cpp|exec_file> always returns
    <cpp|true>.> It compares the result of loading the file with the
    <em|string> <verbatim|"#\<less\>unspecified\<gtr\>">, which never
    matches, not even after an error (<verbatim|Scheme/Scheme/object.cpp>,
    <cpp|exec_file>). No caller uses the result.

    <item><strong|<cpp|eval_secure> is broken.> It
    evaluates <verbatim|(wrap-eval-secure <em|expr>)>, but
    <scm|wrap-eval-secure> is not defined anywhere in
    <verbatim|TeXmacs/progs>. The function has no callers; secure
    evaluation is implemented in <scheme> (<verbatim|kernel/texmacs/tm-secure.scm>).

    <item><strong|Delayed commands and pauses.> Only
    commands scheduled with <cpp|exec_delayed_pause> (and hence the
    <scheme> macro <scm|delayed>) can reschedule themselves by returning
    an integer; the result of a command scheduled with
    <cpp|exec_delayed> is ignored.
  </itemize>

  <section|Writing glue routines>

  <\itemize>
    <item><strong|Editor routines need a current view.>
    All routines of <verbatim|build-glue-editor.scm> call
    <cpp|get_current_editor ()>, which asserts that there is a current
    view. <scheme> code which may run without a buffer (very early during
    startup, or in some background tasks) must not call them.

    <item><strong|At most ten arguments.> The generator
    refers to <verbatim|TMSCM_ARG<em|n>>, which is only defined up to 10
    in <verbatim|guile_tm.hpp>. Routines with more arguments should take a
    list or an <verbatim|object>.

    <item><strong|The <verbatim|uint> check does not
    reject negative numbers.> <verbatim|TMSCM_ASSERT_UINT>
    (<verbatim|Scheme/Scheme/glue.cpp>) tests <verbatim|tmscm_is_int (i)
    && scm_positive_p (i)>, but <cpp|scm_positive_p> returns a
    <scheme> boolean, and <verbatim|#f> is not a null value in <c++>. A
    negative argument therefore passes the check, and is only rejected
    later by <cpp|scm_to_uint>, with an out-of-range error instead of a
    wrong-type error. The type is used by <scm|gnutls-random-number>.

    <item><strong|<verbatim|scheme_tree> loses floating
    point numbers.> <cpp|tmscm_to_scheme_tree> converts lists, symbols,
    strings, integers, booleans and trees; a floating point number (or any
    other value) becomes <verbatim|"?">.

    <item><strong|Destructors may run during garbage
    collection.> A <c++> value boxed in a black box is released by the
    smob free function, that is, during a garbage collection. Its
    destructor (and the destructors of whatever it holds) must not call
    <scheme>. <cpp|tmscm_object_rep> itself respects this by deferring
    its cleanup (see <hlink|protection from the garbage
    collector|scheme-bridge-objects.en.tm>).

    <item><strong|<c++> exceptions inside glue
    routines.> With <verbatim|USE_EXCEPTIONS> (the default), a failed
    <cpp|ASSERT> in a glued routine throws a <c++> exception, which
    leaves the glue function through the <name|Guile> evaluator up to the
    next <c++> catch site (for a menu action, <cpp|protected_call>). The
    <scheme> state between the two (dynamic wind handlers, catch frames)
    is not unwound in the usual <scheme> way. Avoid relying on this in new
    code: check arguments and return an error value instead.

    <item><strong|Regenerate after editing
    declarations.> The <name|CMake> build compiles the committed
    <verbatim|glue_*.cpp>; a change of a <verbatim|build-glue-*.scm> file
    has no effect until the glue is regenerated (see <hlink|regenerating
    the glue|scheme-bridge-glue.en.tm>). The script
    <verbatim|build-glue> runs the generator four times in a row on the
    same output file, which is redundant but harmless.
  </itemize>

  <section|Debug builds>

  With <verbatim|DEBUG_ON>, the evaluations started from <c++> are not
  wrapped in error catches, so a <scheme> error is not stopped at the
  boundary. In addition, while a black box is being freed
  (<cpp|scm_busy>), <cpp|eval_scheme> and <cpp|string_to_tmscm> return
  <verbatim|#f> instead of doing their work.

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
