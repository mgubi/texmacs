<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Automatic tests>

  <TeXmacs> has three kinds of automatic tests, which test different
  layers: <c++> unit tests of the kernel, <scheme> regression tests of
  converters and other <scheme> code, and <em|document test suites> which
  run whole documents through the converters and the typesetter and
  compare the results with a reference run. There is no continuous
  integration configuration in the repository; the tests are run by hand.

  <section|<c++> unit tests>

  The directory <verbatim|tests/> mirrors the source tree and contains
  one test program per file, for instance
  <verbatim|tests/Data/String/analyze_test.cpp> or
  <verbatim|tests/Kernel/Containers/hashmap_test.cpp> (about twenty files:
  strings, trees, containers, <abbr|URL>s, <name|XML> parsing, image files,
  fonts, lengths, keyword parsing, the <name|Qt> and <name|macOS>
  utilities). Each file is a <name|QtTest> test class:

  <\cpp-code>
    #include \<less\>QtTest/QtTest\<gtr\>

    #include "analyze.hpp"

    \;

    class TestAnalyze: public QObject {

    \ \ Q_OBJECT

    private slots:

    \ \ void test_is_alpha ();

    \ \ ...

    };
  </cpp-code>

  <verbatim|tests/CMakeLists.txt> globs all <verbatim|*.cpp> files and
  makes one executable per file, linked against the object library
  <verbatim|texmacs_body> of the main build, with a <name|CTest> test of the
  same name, a time limit of 5 seconds, and <verbatim|TEXMACS_PATH> set to
  the <verbatim|TeXmacs/> directory of the sources (needed for instance by
  the tests of <cpp|utf8_to_cork>, which load dictionaries).
  <verbatim|tests/README.md> explains how to run them with
  <verbatim|ctest> or directly.

  <section|<scheme> regression tests>

  <paragraph|Writing tests.>The macro <scm|regression-test-group>
  (<verbatim|TeXmacs/progs/kernel/boot/debug.scm>) groups tests of a
  function:

  <\scm-code>
    (define (regtest-htmltm-grouping)

    \ \ (regression-test-group

    \ \ \ "htmltm, grouping markup" "grouping"

    \ \ \ shtml-\<gtr\>stm :none

    \ \ \ (test "div" '(div "a") '(document "a"))

    \ \ \ (test "span" '(span "a") "a")))
  </scm-code>

  (from <verbatim|convert/html/htmltm-test.scm>).

  The third and fourth arguments are functions applied to the input and to
  the expected value of each test (<scm|:none> for the identity). Each
  <scm|(test <scm-arg|name> <scm-arg|input> <scm-arg|expected>)> checks
  that the results are equal, <scm|test-fails> that they differ. The first
  failure prints both values and raises an error, so the remaining tests of
  the run are not executed. The group evaluates to the number of tests.
  The macro <scm|regtest-table-library> defines shorthands
  (<scm|cell>, <scm|row>, <scm|table>, <scm|tformat>) for writing tables
  in tests.

  For code with side effects, <scm|integration-test-group> takes a setup
  and a teardown expression which are run around each test; every test is
  run, even after a failure, each result is reported as
  <verbatim|PASS> or <verbatim|FAIL>, and the group prints a summary.

  <paragraph|Test modules.>By convention, the tests of a module
  <verbatim|<em|name>.scm> are in <verbatim|<em|name>-test.scm> next to it,
  which defines a function <scm|regtest-<em|name>>. The master file
  <verbatim|TeXmacs/progs/check/check-master.scm> loads the test modules
  and defines

  <\description>
    <item*|<scm|(run-all-tests)>>The pure regression tests: <name|HTML>
    import and export, <name|XML>, lengths, environment, <name|MathML>,
    <name|TMML>, program formatting and citation sorting.

    <item*|<scm|(run-integration-tests)>>The tests with side effects,
    mainly for the <TeXmacs> server (deletion plans, notifications,
    backups, caches).

    <item*|<scm|(check-latex-export <scm-arg|dir>)>>Exports every
    <verbatim|.tm> file under <scm-arg|dir> to <LaTeX>, runs
    <verbatim|pdflatex> and reports export failures, <LaTeX> errors and
    empty outputs. <scm|(run-checks)> does this for
    <verbatim|$TEXMACS_CHECKS/latex-export>.
  </description>

  These functions are declared lazily in <verbatim|init-texmacs.scm>, so
  that they can be called from the command line, for instance

  <\verbatim-code>
    texmacs -x "(run-all-tests)" -q
  </verbatim-code>

  or from a <scheme> session.

  <section|Document test suites>

  The module <verbatim|TeXmacs/progs/utils/test/test-convert.scm> runs
  collections of documents through <TeXmacs>. A test suite is a
  directory whose subdirectories are named after the kind of their
  contents:

  <\description>
    <item*|<verbatim|texmacs>><TeXmacs> documents, which are exported to
    <abbr|PDF> and to <LaTeX>.

    <item*|<verbatim|latex>>, <verbatim|arxiv><LaTeX> documents (possibly
    as <verbatim|tar> or <verbatim|gzip> archives, as downloaded from
    <name|arXiv>), which are imported, typeset and exported to
    <abbr|PDF>. In a directory with several <verbatim|.tex> files, the one
    with a <verbatim|\\documentclass> (or one called
    <verbatim|main.tex>, <verbatim|paper.tex> or
    <verbatim|article.tex>) is used.
  </description>

  The documentation manuals (<scm|build-manual>) can be part of the run as
  well. The entry points, also available as command line options (see
  <hlink|the main program|server-layer-startup.en.tm>), are:

  <\description>
    <item*|<scm|(build-ref-suite <scm-arg|dir>)>, option
    <verbatim|-reference-suite <em|dir>>>Unpacks <scm-arg|dir> into
    <verbatim|<em|dir>-ref> and produces all outputs there. This is done
    once with a version which is known to be good.

    <item*|<scm|(run-test-suite <scm-arg|dir>)>, option
    <verbatim|-test-suite <em|dir>>>Does the same into
    <verbatim|<em|dir>-check> and then compares it with
    <verbatim|<em|dir>-ref>, if that directory exists. Text outputs
    (<verbatim|.tex>, <verbatim|.tm>) count as changed when their edit
    distance (<scm|string-distance>) to the reference is 5 or more,
    <abbr|PDF> files when their size changes by more than 150 bytes or when
    the output of <verbatim|pdfinfo> (without creation date and file size)
    differs. The result is written to
    <verbatim|<em|dir>-check/status-report.tm>, which lists missing
    directories and files and changed files; it is removed if there is
    nothing to report.
  </description>

  Outputs are only regenerated when the source is newer than the output
  (<scm|should-update?>), so a second run is fast. The functions take
  continuations, and the command line options pass <scm|delayed-quit> so
  that <TeXmacs> exits when the run is complete. A separate module,
  <verbatim|utils/test/test-latex-export.scm>, defines
  <scm|run-latex-export-suite>, which exports the documents of a directory
  to <LaTeX> with several styles.

  <section|Other hooks>

  <verbatim|texmacs.cpp> contains a compile time hook for ad hoc <c++>
  tests: if <verbatim|ENABLE_TESTS> is defined (the <verbatim|#define> is
  commented out), <cpp|texmacs_entrypoint> calls <cpp|test_routines> before
  starting <scheme>, which currently runs <cpp|test_math>
  (<verbatim|src/Graphics/Mathematics/test_math.cpp>), a demonstration
  which prints computations with vectors, matrices, polynomials and balls.

  <section|Pitfalls>

  <\itemize>
    <item>The <c++> unit tests are not part of the <name|CMake> build:
    neither <verbatim|CMakeLists.txt> nor <verbatim|src/CMakeLists.txt>
    contains <verbatim|add_subdirectory (tests)> or
    <verbatim|enable_testing ()>, so the instructions of
    <verbatim|tests/README.md> do not work as written.

    <item>Even when <verbatim|tests/> is added, its
    <verbatim|CMakeLists.txt> links <verbatim|Qt5::Test> unconditionally,
    whereas the default build uses <name|Qt> 6 when it is available.

    <item><scm|integration-test-group> is documented (in a comment in
    <verbatim|debug.scm>) to signal an error at the end if any test failed,
    but its expansion only prints the summary and returns the number of
    tests (<verbatim|kernel/boot/debug.scm:296-303>), so a failing
    integration run is not detectable from its result.

    <item>Some test modules are not run by <scm|run-all-tests>: for
    instance <verbatim|kernel/texmacs/tm-convert-test.scm> (whose
    <scm|regtest-format> is a private <scm|define>),
    <verbatim|kernel/regexp/regexp-test.scm> and
    <verbatim|kernel/logic/logic-test.scm>.

    <item><scm|run-latex-export-suite> is not declared lazily and is not
    loaded by any other module, so it is only available after loading
    <verbatim|(utils test test-latex-export)> by hand.

    <item>In <scm|compare-text-file> (and <scm|compare-pdf-file>), a
    missing output is recorded as missing but then still loaded and
    compared, because the second test checks that the <em|directories>
    exist rather than the files.

    <item>Only files present in the reference are compared: new outputs
    which have no counterpart in <verbatim|<em|dir>-ref> are not
    reported.

    <item>In headless mode, the <verbatim|-test-suite> and
    <verbatim|-reference-suite> options have no effect unless
    <verbatim|-X> is given, because the automatic <scm|quit-TeXmacs> runs
    first; see <hlink|the main program|server-layer-startup.en.tm>.
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
