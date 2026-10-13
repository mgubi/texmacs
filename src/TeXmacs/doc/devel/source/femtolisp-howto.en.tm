<TeXmacs|2.1.5>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|femtolisp: debugging, testing and common tasks>

  <section|Environment variables>

  <\description-long>
    <item*|<verbatim|TEXMACS_FL_TRACE=1>>An error which reaches the C++ code
    is reported with the error of femtolisp and its stack. With
    <verbatim|TEXMACS_FL_TRACE=catch>, the errors caught by <verbatim|catch>
    are reported too: many are expected, since <TeXmacs> tests things inside
    <verbatim|catch>.

    <item*|<verbatim|TEXMACS_FL_CHECK_ROOTS=1>>Checks every <verbatim|tmscm>
    of the list of roots at each collection. Use it when a crash looks like a
    value which moved.

    <item*|<verbatim|TEXMACS_FL_PROFILE=1>>Times the reading, the expansion
    and the compilation of the loaded files. <verbatim|(%profile-report)>
    prints the totals and the hits of the cache,
    <verbatim|(%profile-heads-report)> the expansion time by the first symbol
    of the forms, <verbatim|(%lazy-report)> the number of stubs and of first
    calls.

    <item*|<verbatim|TEXMACS_FL_NO_CACHE=1>>Runs without the caches of
    compiled code.

    <item*|<verbatim|TEXMACS_FL_EAGER=1>>Expands and compiles the function
    bodies when a file is loaded, with the late calls and the deferred errors
    of macros.

    <item*|<verbatim|TEXMACS_FL_HEAP=n>>The size of each half of the heap at
    start, in MB.
  </description-long>

  Also <verbatim|(%gc-count)>, the number of collections so far,
  <verbatim|(%gc)> and <verbatim|(%heap-size)>.

  <section|Reading an error>

  The stack printed with <verbatim|TEXMACS_FL_TRACE> lists the calls with
  their arguments, the innermost first: at most 20 frames, with long arguments
  cut. A function which was left by a tail call does not appear. A frame
  <verbatim|(lambda ...)> is a function without a name: a <verbatim|let> is
  one, since it expands to a call of a lambda, and so is a function made by
  <verbatim|tm-define>. A private function shows under its plain name
  <verbatim|f>, not <verbatim|f@a/b>: only the global variable which holds it
  is renamed. A frame <verbatim|(%lazy-compile (lambda ...) ...)> means that
  the error came while a body was being compiled at its first call: the error
  is in that body, often a macro which failed to expand, and the function is
  the frame which follows, with its arguments in one list.

  <\description-long>
    <item*|<verbatim|Unbound variable: f>, for an <verbatim|f> which is defined>The
    name may be private to another module, or defined later than its use at
    the top level of a file. Look for <verbatim|f@...> in
    <verbatim|(environment)>.

    <item*|A wrong number of arguments for a builtin (<verbatim|equal?: too many arguments>)>Often
    the reader: an octal character of Guile, <verbatim|#\\04>, is read as two
    data.

    <item*|A crash in the collector, or values which change>A raw
    <verbatim|fltm_value> kept across an allocation, or a <verbatim|longjmp>
    over a C++ function. Run with <verbatim|TEXMACS_FL_CHECK_ROOTS=1>.

    <item*|Something is defined although its condition is false>A macro at the
    top level of a file with an effect when it is expanded.

    <item*|A change of a Scheme file seems ignored>It should not happen: the
    caches follow the expansion of each form. If it does, run with
    <verbatim|TEXMACS_FL_NO_CACHE=1> to tell whether the cache is the cause,
    and see whether <verbatim|%cache-format> should have been increased.
  </description-long>

  <section|Looking inside>

  <\scm-code>
    ;; the source of a function, called or not

    (procedure-source f)

    ;; its bytecode (that of a stub before its first call)

    (disassemble f)

    ;; a private function of the module (a b)

    (top-level-value (symbol "f@a/b"))

    ;; the private names of a module

    (module-symbols (resolve-module '(a b)))

    ;; the late calls, with TEXMACS_FL_TRACE and TEXMACS_FL_EAGER

    (%late-calls)
  </scm-code>

  <verbatim|write> shows a function as <verbatim|#\<less\>procedure f\<gtr\>>;
  bind <verbatim|*print-closures*> to <verbatim|#t> to see its code and
  constants.

  <section|Testing>

  <\shell-code>
    TEXMACS_PATH=$PWD/TeXmacs TEXMACS_HOME_PATH=\<less\>a scratch directory\<gtr\> \\

    \ \ TeXmacs/bin/texmacs.bin -headless -x '(exit (catch #t

    \ \ \ \ (lambda () (min 1 (run-all-tests)))

    \ \ \ \ (lambda args (display* "error: " args "\\n") 2)))' -q
  </shell-code>

  runs the regression suites of <source-link|progs/check|TeXmacs/progs/check> with the Vue interface,
  which needs <verbatim|-headless> to run without a window
  (<source-link|tests/scheme/check.sh|tests/scheme/check.sh> does not pass it; with a Qt build, use
  that script with <verbatim|QT_QPA_PLATFORM=offscreen>). The option
  <verbatim|-q> must come after <verbatim|-x>: the options are run in their
  order, and <verbatim|-q> first would quit before the tests.
  <verbatim|(run-regression-suite "name")> runs one suite. With femtolisp, two
  suites have failing checks (three in all, two of them in
  <verbatim|graphics-edit>), which are not bugs of the interface:

  <\itemize>
    <item><verbatim|htmltm>: a width of 50% gives <verbatim|0.5par> where
    Guile gives <verbatim|1/2par> (no exact rationals);

    <item><verbatim|graphics-edit>: the order of the attributes of a
    <verbatim|with> comes from the order of a hash table.
  </itemize>

  Run the suites twice with the same scratch directory: the first run fills
  the caches of compiled code, the second one uses them. A change of
  <source-link|boot-femtolisp.scm|TeXmacs/progs/kernel/boot/boot-femtolisp.scm> should be tested both ways, and with
  <verbatim|TEXMACS_FL_EAGER=1>. The two suites <verbatim|boot-s7> and
  <verbatim|compat-s7> are for S7 and do not run with femtolisp.

  The benchmarks are the scripts of <source-link|docs/s7/bench|docs/s7/bench> and
  <source-link|docs/femtolisp/bench/ui.scm|docs/femtolisp/bench/ui.scm>, run on several builds by
  <source-link|docs/femtolisp/bench/run.sh|docs/femtolisp/bench/run.sh>. Compare builds by alternating them
  on the same machine, and take the times without a profiler: sampling slows
  femtolisp more than S7.

  <section|Common tasks>

  <\description-long>
    <item*|Adding a function of Guile which is missing>Define it in
    <source-link|compat-femtolisp.scm|TeXmacs/progs/kernel/boot/compat-femtolisp.scm> with <verbatim|define-public>, after
    checking that the glue does not define it. If the name is a builtin of
    femtolisp with another meaning, use <verbatim|define-override> in
    <source-link|r5rs-femtolisp.scm|TeXmacs/progs/kernel/boot/r5rs-femtolisp.scm>.

    <item*|Adding a primitive in C>For a function of <TeXmacs>, add it to the
    glue, as for the other interpreters. For a function which only femtolisp
    needs, write it in <source-link|fl_core.c|src/Scheme/Femtolisp/fl_core.c> with the signature of the builtins
    of femtolisp and add it to <verbatim|fltm_builtin_info>; there, an error
    is raised with the functions of femtolisp (<verbatim|type_error>,
    <verbatim|bounds_error>, <verbatim|argcount>; <verbatim|lerrorf> for the
    others), since no C++ is involved.

    <item*|Holding a Scheme value in C++>Keep it in a <verbatim|tmscm> (or an
    <verbatim|object>), never in a <verbatim|fltm_value>.

    <item*|Writing a macro>Its expansion must not register anything: return
    code which does it. The expansion must not contain a call of the macro
    itself with the same arguments.

    <item*|Changing how files are loaded or compiled>The code is in
    <source-link|boot-femtolisp.scm|TeXmacs/progs/kernel/boot/boot-femtolisp.scm>. Increase <verbatim|%cache-format> if the
    same expansion now compiles to other code, test with and without the
    caches, and measure a boot with <verbatim|TEXMACS_FL_PROFILE=1>.

    <item*|Changing femtolisp itself>See
    <hlink|its sources and their patches|femtolisp-vendored.en.tm>.
  </description-long>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this document under the terms of the GNU Free Documentation License, Version 1.1 or any later version published by the Free Software Foundation; with no Invariant Sections, with no Front-Cover Texts, and with no Back-Cover Texts. A copy of the license is included in the section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
