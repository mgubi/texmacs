<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Building <TeXmacs> and running the tests>

  <section|Introduction>

  This chapter explains how a <TeXmacs> binary is produced from the
  sources, which files are generated along the way, how distribution
  packages are made for the various platforms, and which automatic tests
  exist. It is meant for developers who want to build <TeXmacs> themselves,
  add a source file or a dependency, or check that a change does not break
  anything.

  The general organization of the source tree is described in
  <hlink|general architecture of <TeXmacs>|architecture.en.tm>; what the
  resulting program does when it starts, and its command line options, are
  described in <hlink|the main program and crash
  handling|server-startup.en.tm>. The generation of the <scheme>
  glue is explained in detail in <hlink|the <scheme> glue|scheme-bridge-glue.en.tm>.

  All file names in this chapter are relative to the directory
  <verbatim|src/> of the repository (the one which contains
  <verbatim|configure.in> and <verbatim|CMakeLists.txt>), unless stated
  otherwise.

  <section|Overview>

  There are two independent build systems, which compile the same sources:

  <\description>
    <item*|<name|GNU> autotools>The traditional build: <verbatim|configure>
    (generated from <verbatim|configure.in> and the macros in
    <verbatim|misc/m4/>) produces <verbatim|Makefile>,
    <verbatim|src/makefile> and the configuration headers, and
    <verbatim|make> builds the binary. This build also knows how to make
    the distribution packages (<verbatim|make PACKAGE>, <verbatim|make
    BUNDLE>) and how to regenerate the glue (<verbatim|make GLUE>).

    <item*|<name|CMake>>The newer build (<verbatim|CMakeLists.txt>,
    <verbatim|src/CMakeLists.txt> and <verbatim|cmake/>), which is
    convenient with <name|Ninja>, with IDEs and for the <c++> unit tests. It
    only builds and installs the program; packaging is left to the
    autotools build and to the scripts in <verbatim|packages/>.
  </description>

  Both builds produce the same layout: the runtime tree <verbatim|TeXmacs/>
  (style files, <scheme> programs, fonts, documentation, ...) with the
  binary <verbatim|TeXmacs/bin/texmacs.bin> inside it, and a small shell
  script <verbatim|texmacs> (from <verbatim|misc/scripts/texmacs.in>) which
  sets <verbatim|TEXMACS_PATH> and the library path before starting the
  binary. A freshly built tree can therefore be run without installing it,
  by pointing <verbatim|TEXMACS_PATH> to the <verbatim|TeXmacs/> directory.

  The <scheme> interpreter is <name|Guile>. By default both builds use an
  <em|embedded> <name|Guile> 1.8, whose sources are expected in the
  directory <verbatim|src/tm-guile188/>; they are not part of the
  <TeXmacs> sources but of the separate repository
  <verbatim|guile-texmacs> (see <verbatim|Dockerfile>, which copies it to
  that place). Alternatively, a system <name|Guile> can be used.

  The tests come in three kinds: <c++> unit tests in <verbatim|tests/>
  (built with <name|CMake> and <name|QtTest>), <scheme> regression tests
  (<verbatim|*-test.scm> files run by <scm|run-all-tests>), and document
  test suites which convert directories of documents and compare the
  results with a reference run.

  <section|Source files>

  <\description-paragraphs>
    <item*|<verbatim|configure.in>, <verbatim|misc/m4/*.m4>>The autoconf
    input: one macro file per dependency or feature (<verbatim|guile.m4>,
    <verbatim|qt.m4>, <verbatim|freetype.m4>, <verbatim|tm_gui.m4>,
    <verbatim|tm_platform.m4>, <verbatim|tm_debug.m4>, ...).

    <item*|<verbatim|Makefile.in>, <verbatim|src/makefile.in>>The top level
    makefile (installation, plug-ins, packages) and the makefile which
    compiles the sources and the glue.

    <item*|<verbatim|CMakeLists.txt>, <verbatim|src/CMakeLists.txt>>The
    <name|CMake> build: options, dependencies, source lists, configuration
    headers, and the executable targets per platform.

    <item*|<verbatim|cmake/>>Find modules (<verbatim|FindCairo.cmake>,
    <verbatim|FindGMP.cmake>, <verbatim|FindSQLite3.cmake>, ...) and a few
    helper scripts.

    <item*|<verbatim|src/System/config.in>, <verbatim|config.h.cmake>,
    <verbatim|tm_configure.in>, <verbatim|tm_configure.hpp.cmake>>Templates
    of the two generated configuration headers.

    <item*|<verbatim|src/Scheme/Glue/>>The glue declarations and the glue
    generator.

    <item*|<verbatim|packages/>>Platform packaging: <verbatim|macos/>,
    <verbatim|windows/>, <verbatim|msix/>, <verbatim|debian/>,
    <verbatim|redhat/>, <verbatim|fedora/>, <verbatim|centos/>,
    <verbatim|mandriva/>, <verbatim|appimage/>, <verbatim|android/>,
    <verbatim|linux/>, <verbatim|haiku/>.

    <item*|<verbatim|Dockerfile>>A container build on <name|Ubuntu> with the
    embedded <name|Guile>, mainly used for running a <TeXmacs> server.

    <item*|<verbatim|tests/>>The <c++> unit tests.

    <item*|<verbatim|TeXmacs/progs/check/check-master.scm>,
    <verbatim|TeXmacs/progs/kernel/boot/debug.scm>,
    <verbatim|TeXmacs/progs/utils/test/>>The <scheme> regression tests and
    their macros, and the document test suites.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Building with <name|CMake>|build-cmake.en.tm>

    <branch|Building with the autotools|build-autoconf.en.tm>

    <branch|Packages for the various platforms|build-packaging.en.tm>

    <branch|Automatic tests|build-tests.en.tm>
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
