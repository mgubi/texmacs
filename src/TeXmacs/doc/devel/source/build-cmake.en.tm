<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Building with <name|CMake>>

  <section|Quick start>

  An out-of-source build, which is the recommended way, looks like

  <\verbatim-code>
    cmake -B build -G Ninja -DSCHEME_IMPL=system

    cmake --build build
  </verbatim-code>

  run from <verbatim|src/>. With the default <verbatim|SCHEME_IMPL>
  (<verbatim|embedded18>), the sources of the embedded <name|Guile> must
  first be made available as <verbatim|src/tm-guile188/> (from the
  <verbatim|guile-texmacs> repository); <verbatim|-DSCHEME_IMPL=system>
  uses an installed <name|Guile> instead. The resulting binary is
  <verbatim|build/TeXmacs/bin/texmacs.bin> (<verbatim|texmacs.exe> on
  <name|Windows>, a <verbatim|TeXmacs.app> bundle on <name|macOS>), and
  the runtime tree is <verbatim|build/TeXmacs/>.

  <section|Options>

  The cache variables and options defined in <source-link|CMakeLists.txt|src/CMakeLists.txt>
  are:

  <\description>
    <item*|<verbatim|CMAKE_BUILD_TYPE>>Defaults to <verbatim|Release>.

    <item*|<verbatim|SCHEME_IMPL>>Either <verbatim|embedded18> (or its
    synonym <verbatim|tm-guile188>), which builds <verbatim|tm-guile188/>
    as a subproject and defines <verbatim|GUILE_C>, or anything else, in
    which case <verbatim|pkg-config> looks for <verbatim|guile-1.8>,
    <verbatim|guile-3.0>, <verbatim|guile-2.2> or <verbatim|guile-2.0>
    (in this order) and defines <verbatim|GUILE_C> for 1.8 and
    <verbatim|GUILE_D> for 2.0 and later. The meaning of these dialect
    macros is explained in <hlink|the <scheme>
    interpreter|scheme-bridge-interpreter.en.tm>.

    <item*|<verbatim|TEXMACS_GUI>>One of <verbatim|Qt> (the default, which
    uses <name|Qt> 6 if it is found and <name|Qt> 5 otherwise),
    <verbatim|Qt6>, <verbatim|Qt5> or <verbatim|Qt4>. The help string also
    mentions <verbatim|Aqua> and <verbatim|X11>, but only the <name|Qt>
    values are implemented (see the pitfalls below). Note that
    <verbatim|Qt6> is treated like <verbatim|Qt>: there is no branch which
    insists on <name|Qt> 6. All variants compile the sources of
    <verbatim|src/Plugins/Qt/>; the directory <verbatim|src/Plugins/Qt6/> is
    not used by <name|CMake>.

    <item*|<verbatim|QTPIPES>>(on) Use <name|Qt> classes instead of
    <name|Unix> pipes for plug-in connections.

    <item*|<verbatim|USE_RESVG>>(on) Use the <name|resvg> library for
    <name|SVG> images if it is found in one of the local prefixes; the
    option is silently switched off otherwise.

    <item*|<verbatim|USE_ASPELL>>(on) Link the <name|Aspell> library if it
    is found; switched off otherwise.

    <item*|<verbatim|USE_FREETYPE>>(on) <name|FreeType> is then
    <em|required>.

    <item*|<verbatim|USE_GNUTLS>>(on) <name|GnuTLS> is then required (it
    is used by the <TeXmacs> server and client, see <hlink|collaboration,
    remote servers and versioning|collaboration.en.tm>).

    <item*|<verbatim|USE_SQLITE3>>(on) Link <name|SQLite> if found, which
    enables the <verbatim|Plugins/Sqlite3> back-end of the database.

    <item*|<verbatim|ENABLE_EXPERIMENTAL>>(off) Compile the experimental
    style rewriting code in <verbatim|src/Style/> and define
    <verbatim|EXPERIMENTAL>.
  </description>

  Several settings are not options but fixed: <verbatim|DEBUG_ASSERT> is
  always 1 (so <cpp|ASSERT> and <cpp|FAILED> are active in all builds),
  <name|PNG> and <name|zlib> are required, <name|iconv> is used if found,
  the native <abbr|PDF> renderer (<name|Hummus>) is always compiled, and
  the path of <name|Ghostscript> is hard-wired
  (<verbatim|/usr/bin/gs>, or <verbatim|bin/gs.exe> on <name|Windows>).
  If <name|ccache> is installed, it is used automatically.

  Additional prefixes are searched for dependencies: the
  <verbatim|local/> subdirectory of the environment variable
  <verbatim|WORKING_DIR_WIN> or <verbatim|WORKING_DIR>, and a directory
  <verbatim|../../local> next to the repository, which is how the
  <TeXmacs> builder scripts provide their own copies of the libraries.

  <section|How the build is organized>

  <paragraph|Source lists.>The sources are collected with
  <verbatim|file (GLOB_RECURSE ...)> from the directories <verbatim|Data>,
  <verbatim|Edit>, <verbatim|Graphics>, <verbatim|Kernel>,
  <verbatim|Scheme/Scheme> and <verbatim|Scheme/Guile>, <verbatim|System>,
  <verbatim|Typeset>, part of <verbatim|Texmacs>, the <em|standard
  plug-ins> (<verbatim|Bibtex>, <verbatim|Database>, <verbatim|Freetype>,
  <verbatim|Gnutls>, <verbatim|Pdf>, <verbatim|Ghostscript>,
  <verbatim|Ispell>, <verbatim|Metafont>, <verbatim|LaTeX_Preview>,
  <verbatim|Openssl>, <verbatim|Updater>, and optionally
  <verbatim|Resvg> and <verbatim|Sqlite3>), the <name|Qt> port, and the
  operating system layer (<verbatim|Plugins/Unix> on <name|Linux>,
  <verbatim|Plugins/Windows> or <verbatim|Plugins/Windows64> on
  <name|Windows>). Since the lists are globbed, a new <verbatim|.cpp> file
  is picked up automatically, but only after <name|CMake> is run again; a
  file in a directory which is not listed (for instance a new plug-in
  directory) must be added to <source-link|CMakeLists.txt|src/CMakeLists.txt>. The
  <verbatim|Scheme/Tiny>, <verbatim|Plugins/X11>, <verbatim|Plugins/Widkit>,
  <verbatim|Plugins/Cocoa>, <verbatim|Plugins/MacOS>,
  <verbatim|Plugins/Cairo>, <verbatim|Plugins/Imlib2> and
  <verbatim|Plugins/Qt6> directories are never compiled by <name|CMake>.

  <paragraph|Targets.>All sources are compiled once into the object
  library <verbatim|texmacs_body> (<source-link|src/CMakeLists.txt|src/CMakeLists.txt>), with
  <verbatim|AUTOMOC> for the <name|Qt> classes and with the generated
  <verbatim|config.h> force-included in every file
  (<verbatim|-include .../src/System/config.h>). The executables are then
  linked from these objects:

  <\itemize>
    <item>on <name|Linux> and other <name|Unix> systems,
    <verbatim|texmacs.bin> with <source-link|Plugins/Unix/unix_entrypoint.cpp|src/Plugins/Unix/unix_entrypoint.cpp>,
    written to <verbatim|TeXmacs/bin/> of the build directory;

    <item>on <name|Windows>, <verbatim|texmacs.exe> with the 32 or 64 bit
    entry point and the resource file <verbatim|packages/windows/resource.rc>,
    and <verbatim|texmacs-open.exe> from
    <source-link|src/Launcher/texmacs_open_main.cpp|src/Launcher/texmacs_open_main.cpp>; both are copied to
    <verbatim|TeXmacs/bin/> as <verbatim|*.bin> and also to the top of the
    build directory;

    <item>on <name|macOS>, a <verbatim|MACOSX_BUNDLE> named
    <verbatim|TeXmacs> with <source-link|packages/macos/Info.plist.in|packages/macos/Info.plist.in>.
  </itemize>

  The <c++> unit tests of <verbatim|tests/> link against the same object
  library; see <hlink|automatic tests|build-tests.en.tm>.

  <paragraph|The runtime tree.>In an out-of-source build, the whole
  <verbatim|TeXmacs/> directory is copied into the build directory when
  <name|CMake> runs, and the target <verbatim|deploy_texmacs_to_build>
  (part of <verbatim|ALL>) copies it again at every build, so that changes
  to <scheme> files, styles or documentation reach the build tree. The copy
  only adds and overwrites files: a file deleted in the source tree stays
  in the build tree until the build directory is cleaned. An in-source
  build (with a warning) uses <verbatim|TeXmacs/> directly and writes the
  generated files into the source tree. In both cases the version string
  is written to <verbatim|TeXmacs/SVNREV>.

  <paragraph|Generated headers.><verbatim|config.h> and
  <verbatim|tm_configure.hpp> are generated with <verbatim|configure_file>
  from <source-link|src/System/config.h.cmake|src/System/config.h.cmake> and
  <source-link|src/System/tm_configure.hpp.cmake|src/System/tm_configure.hpp.cmake> into
  <verbatim|<em|build>/src/System/>. The first one holds the feature macros
  (<verbatim|QTTEXMACS>, <verbatim|USE_FREETYPE>, <verbatim|GUILE_C>, ...),
  the second the version, the build user and date, and the host
  description reported by <verbatim|-version> and in crash reports. The
  scripts <verbatim|texmacs> and <verbatim|fig2ps> and the manual page are
  generated by <source-link|misc/CMakeLists.txt|misc/CMakeLists.txt>.

  <paragraph|The glue.>The <name|CMake> build compiles the generated glue
  files <verbatim|src/Scheme/Glue/glue_*.cpp> as they are in the
  repository and has no rule to regenerate them. After changing a
  <verbatim|build-glue-*.scm> file, run the generator by hand or with
  <verbatim|make GLUE> from an autotools build; see <hlink|the <scheme>
  glue|scheme-bridge-glue.en.tm>.

  <paragraph|Installation.><verbatim|cmake --install> installs the
  executable, the <verbatim|TeXmacs/> tree into
  <verbatim|share/TeXmacs> (or the installation prefix itself on
  <name|Windows>), the plug-ins, and the desktop file, icons and
  <name|MIME> description for <name|Linux> desktops.

  <section|Pitfalls>

  <\itemize>
    <item><verbatim|TEXMACS_GUI=X11> or <verbatim|Aqua> configures without
    error but does not select any graphical port: only the
    <verbatim|Qt.*> values are handled (<verbatim|CMakeLists.txt:259-293>),
    so neither <verbatim|QTTEXMACS> nor <verbatim|X11TEXMACS> is defined
    and no toolkit sources are compiled.

    <item>The <name|macOS> target is built from <verbatim|texmacs_body>
    alone (<verbatim|src/CMakeLists.txt:124-142>): no entry point is
    added (the <name|Unix> one is only used in the <verbatim|else> branch),
    and the list of operating system sources is left empty for
    <name|Apple> (<source-link|CMakeLists.txt|src/CMakeLists.txt>, \PApple/Cocoa\Q), so the
    <verbatim|.mm> files of <verbatim|Plugins/MacOS> and the
    <verbatim|Plugins/Unix> files are not compiled although
    <verbatim|MACOSX_EXTENSIONS> is defined. As far as can be seen from the
    files, a <name|CMake> build on <name|macOS> cannot link; the autotools
    build is the one used on that platform.

    <item>On <name|Unix>, the executable is installed to
    <verbatim|${tmbin}/bin> (<source-link|src/CMakeLists.txt:157|src/CMakeLists.txt:157>), but
    <verbatim|tmbin> is only defined in <source-link|misc/CMakeLists.txt|misc/CMakeLists.txt>,
    which is a sibling directory processed later; in
    <verbatim|src/> the variable is empty and the binary is installed to
    <verbatim|/bin> under the installation root rather than
    <verbatim|libexec/TeXmacs/bin> where the <verbatim|texmacs> script
    looks for it.

    <item>The include path lists <verbatim|src/System> of the source tree
    before <verbatim|src/System> of the build tree. If an autotools build
    has been run in the same source tree, its <verbatim|config.h> and
    <verbatim|tm_configure.hpp> (which are not version controlled) are
    found first by <verbatim|#include "tm_configure.hpp"> and by the files
    which include <verbatim|config.h> explicitly, while the force-included
    <verbatim|config.h> is the <name|CMake> one. Remove the generated files
    from <verbatim|src/System> (or use a separate checkout) before
    switching build systems.

    <item>The helper files <source-link|cmake/CreateBundle.sh.in|cmake/CreateBundle.sh.in> and
    <source-link|cmake/CompleteBundle.cmake.in|cmake/CompleteBundle.cmake.in> are not referenced by any
    <name|CMake> file; they are leftovers of an older bundle procedure.
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
