<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Selecting and building a port>

  <section|The port macros>

  Exactly one of three macros identifies the port in <verbatim|config.h>:

  <\description>
    <item*|<cpp|QTTEXMACS>><name|Qt> port (<verbatim|Plugins/Qt>).

    <item*|<cpp|X11TEXMACS>><name|X11> port (<source-link|Plugins/X11|src/Plugins/X11> and
    <source-link|Plugins/Widkit|src/Plugins/Widkit>).

    <item*|<cpp|AQUATEXMACS>><name|Cocoa> port (<verbatim|Plugins/Cocoa>).
  </description>

  Outside the port directories, many files test these macros, typically to
  include the header which defines <cpp|simple_widget_rep> (for instance
  <source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp>, which chooses between
  <verbatim|Cocoa/aqua_simple_widget.h>, <source-link|Qt/qt_simple_widget.hpp|src/Plugins/Qt/qt_simple_widget.hpp>
  and <source-link|Widkit/simple_wk_widget.hpp|src/Plugins/Widkit/simple_wk_widget.hpp>) or to enable features which
  only exist in one port. In most of these places the <verbatim|#else>
  branch is the <name|X11>/<name|Widkit> code, so a new port must add its
  own branches. Code which depends on the <name|Qt> version tests
  <cpp|QT_VERSION> instead, see <hlink|the <name|Qt>
  port|guiports-qt.en.tm>.

  <section|<name|CMake>>

  The top level <source-link|CMakeLists.txt|src/CMakeLists.txt> declares

  <\verbatim-code>
    set (TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, Qt6, Qt5, Qt4, Aqua, X11)")

    option (QTPIPES "use Qt pipes" ON)
  </verbatim-code>

  but only the <name|Qt> values are implemented. <verbatim|Qt4> and
  <verbatim|Qt5> force the corresponding major version; every other value
  matching <verbatim|Qt.*> (including the default <verbatim|Qt> and
  <verbatim|Qt6>) looks for <name|Qt> 6 first and falls back to <name|Qt>
  5. The <name|Qt> components used are <verbatim|Core>, <verbatim|Gui>,
  <verbatim|Widgets>, <verbatim|PrintSupport>, <verbatim|Svg> and
  <verbatim|Network>. The branch then sets <verbatim|QTTEXMACS>,
  <verbatim|USE_QTSVG> and <verbatim|CONFIG_GUI= QT>. The values
  <verbatim|Aqua> and <verbatim|X11> are accepted by the cache variable
  but select nothing: no port macro is defined, and the source list always
  contains <verbatim|Plugins/Qt/*.cpp> and <verbatim|Plugins/Qt/*.hpp>
  (<verbatim|TeXmacs_Qt_SRCS>, <verbatim|TeXmacs_Qt_HDRS>). In practice
  <name|CMake> builds the <name|Qt> port only.

  <section|<verbatim|configure> and <verbatim|make>>

  The autoconf macro <verbatim|TM_GUI> (<source-link|misc/m4/tm_gui.m4|misc/m4/tm_gui.m4>) is
  more complete:

  <\description>
    <item*|default>The <name|Qt> port, with the <name|Qt> version found by
    <verbatim|LC_WITH_QT>. On <name|macOS> it also enables the
    <name|Objective-C> helpers of <verbatim|Plugins/MacOS>
    (<verbatim|CONFIG_MACOS>).

    <item*|<verbatim|--disable-qt>>The <name|X11> port. It sets
    <verbatim|CONFIG_X11= "X11 Widkit"> (plus <verbatim|Ghostscript> when
    <name|Ghostscript> is not configured otherwise), defines
    <verbatim|X11TEXMACS> and looks for the <name|X11> headers and
    libraries.

    <item*|<verbatim|--enable-cocoa>>The <name|Cocoa> port: defines
    <verbatim|AQUATEXMACS>, sets <verbatim|CONFIG_COCOA= Cocoa> and links
    with <verbatim|-framework Cocoa>. Since this test comes after the
    <name|Qt> test, it overrides the default <name|Qt> choice.

    <item*|<verbatim|--enable-qt-new>>Compile <source-link|Plugins/Qt6|src/Plugins/Qt6>
    instead of <verbatim|Plugins/Qt> (<source-link|misc/m4/qt.m4|misc/m4/qt.m4>): the
    variable <verbatim|QT_PLUGIN_DIR> becomes <verbatim|Qt6>, which selects
    both the sources and the include path <verbatim|-IPlugins/Qt6>. The
    option is on by default when <verbatim|CONFIG_OS> is
    <verbatim|ANDROID>, and <verbatim|configure> stops with an error if the
    <name|Qt> found is older than 6.10. <name|CMake> has no equivalent: it
    always compiles <verbatim|Plugins/Qt>.

    <item*|<verbatim|--enable-qtpipes>>Use <name|Qt> processes instead of
    <name|Unix> pipes for plug-ins (<cpp|QTPIPES>); only allowed with the
    <name|Qt> port.
  </description>

  <source-link|src/makefile.in|src/makefile.in> then compiles the port directories through
  the substituted variables: <verbatim|@CONFIG_X11@> for the <name|C++>
  sources of <name|X11>, <verbatim|@CONFIG_COCOA@ @CONFIG_MACOS@> for the
  <name|Objective-C> sources, and, for <name|Qt>, the directory
  <verbatim|Plugins/$(QT_PLUGIN_DIR)> (with the <name|Qt> meta object
  compiler run on its headers when <verbatim|@CONFIG_QT@> is not empty).

  <section|Android>

  The <name|Android> launcher in <source-link|src/packages/android/launcher|packages/android/launcher>
  is a separate <name|CMake> project which links a prebuilt
  <verbatim|libtexmacs.a> with <name|Qt> 6 or 5 (<verbatim|find_package
  (QT NAMES Qt6 Qt5 ...)>). The library itself is configured with
  <verbatim|configure>, where <verbatim|--enable-qt-new> is the default for
  <name|Android>, so a library configured for <name|Android> uses <source-link|Plugins/Qt6|src/Plugins/Qt6>
  unless <verbatim|--disable-qt-new> is given. The operating system side of
  <name|Android> is described in <hlink|platform
  support|system-platforms.en.tm>.

  <section|Headless mode>

  Headless mode is not a separate port but a run-time mode of the
  <name|Qt> port, selected by <verbatim|-headless> (and implied by the
  conversion and web site options, see <hlink|the main program|server-startup.en.tm>).
  A <cpp|QTMCoreApplication> replaces the <cpp|QTMApplication>, no window
  is shown, and the interpose handler skips the screen updates; see also
  the section on headless mode in <hlink|the <name|Qt>
  implementation|widgets-qt.en.tm>.

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
