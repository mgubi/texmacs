<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Selecting and building a port>

  <section|The port macros>

  One macro identifies the port in <verbatim|config.h> (and on the command
  line of the <verbatim|make> build, through <verbatim|-D@CONFIG_GUI_DEFINE@>
  in <source-link|src/makefile.in|src/makefile.in>):

  <\description>
    <item*|<cpp|QTTEXMACS>><name|Qt> port (<source-link|Plugins/Qt|src/Plugins/Qt> or
    <source-link|Plugins/Qt6|src/Plugins/Qt6>).

    <item*|<cpp|QTWKTEXMACS>><name|Qtwk> (<source-link|Plugins/Qtwk|src/Plugins/Qtwk> and
    <source-link|Plugins/Widkit|src/Plugins/Widkit>); <cpp|QTTEXMACS> is defined as well, since
    <name|Qtwk> uses the application, fonts, pictures, renderer and pipes
    of the <name|Qt> port.

    <item*|<cpp|X11TEXMACS>><name|X11> port (<source-link|Plugins/X11|src/Plugins/X11> and
    <name|Widkit>).

    <item*|<cpp|SDLTEXMACS>><name|SDL> port (<source-link|Plugins/SDL|src/Plugins/SDL> and
    <name|Widkit>).

    <item*|<cpp|VUETEXMACS>><name|Vue> port (<source-link|Plugins/Vue|src/Plugins/Vue>).

    <item*|<cpp|AQUATEXMACS>><name|Cocoa> port (<source-link|Plugins/NS|src/Plugins/NS>).
  </description>

  Outside the port directories, many files test these macros, typically to
  include the header which defines <cpp|simple_widget_rep>. For instance
  <source-link|Texmacs/Window/tm_button.cpp|src/Texmacs/Window/tm_button.cpp> chooses between
  <source-link|NS/ns_simple_widget.h|src/Plugins/NS/ns_simple_widget.h> (<cpp|AQUATEXMACS>),
  <source-link|Qt/qt_simple_widget.hpp|src/Plugins/Qt/qt_simple_widget.hpp> (<cpp|QTTEXMACS> without
  <cpp|QTWKTEXMACS>), <source-link|Vue/vue_widget.hpp|src/Plugins/Vue/vue_widget.hpp> (<cpp|VUETEXMACS>)
  and, in the <verbatim|#else> branch, <source-link|Widkit/simple_wk_widget.hpp|src/Plugins/Widkit/simple_wk_widget.hpp>.
  Other tests enable features which only exist in some ports: the
  <name|Cocoa> port is treated as <name|Qt> by the generic code wherever
  that has <name|Qt> specific parts (delayed commands, native pictures,
  drops, the repainting and the mouse of the editor), and <name|Vue> often
  shares the <name|X11> branch (for instance to center the paper in a wider
  canvas, <source-link|Edit/Interface/edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>). Because
  <cpp|QTTEXMACS> holds in <name|Qtwk>, code which really needs <name|Qt>
  widgets must test <cpp|QTWKTEXMACS> too. A new port must add its own
  branches, and the <verbatim|#else> branch is usually the <name|Widkit>
  code. Code which depends on the <name|Qt> version tests <cpp|QT_VERSION>
  instead, see <hlink|the <name|Qt> port|guiports-qt.en.tm>.

  <section|<verbatim|configure> and <verbatim|make>>

  The autoconf macro <verbatim|TM_GUI> (<source-link|misc/m4/tm_gui.m4|misc/m4/tm_gui.m4>) reads
  <verbatim|--with-gui=<em|port>>:

  <\description>
    <item*|<verbatim|qt> (default)>The <name|Qt> port, with the <name|Qt>
    version found by <verbatim|LC_WITH_QT>. On <name|macOS> it also enables
    the <name|Objective-C> helpers of <source-link|Plugins/MacOS|src/Plugins/MacOS>
    (<verbatim|CONFIG_MACOS>).

    <item*|<verbatim|qtwk>>The same <name|Qt> setup, but
    <verbatim|CONFIG_QT= "Qtwk Widkit">: <source-link|src/makefile.in|src/makefile.in> then compiles
    <source-link|Plugins/Qtwk|src/Plugins/Qtwk>, <name|Widkit> and only eight files of
    <verbatim|Plugins/Qt> (<source-link|qt_renderer.cpp|src/Plugins/Qt/qt_renderer.cpp>, <source-link|qt_font.cpp|src/Plugins/Qt/qt_font.cpp>,
    <source-link|qt_picture.cpp|src/Plugins/Qt/qt_picture.cpp>, <source-link|qt_utilities.cpp|src/Plugins/Qt/qt_utilities.cpp>, the pipes,
    the sockets and <source-link|qt_http.cpp|src/Plugins/Qt/qt_http.cpp>), and runs the meta object
    compiler on three headers of <name|Qtwk>.

    <item*|<verbatim|x11>>The <name|X11> port. It looks for the <name|X11>
    headers and libraries, sets <verbatim|CONFIG_X11= "X11 Widkit"> (plus
    <verbatim|Ghostscript> when <name|Ghostscript> is not configured
    otherwise) and defines <verbatim|X11TEXMACS>.

    <item*|<verbatim|sdl>>The <name|SDL> port: <name|SDL3> (<verbatim|--with-sdl3>,
    <source-link|misc/m4/sdl3.m4|misc/m4/sdl3.m4>), <verbatim|CONFIG_SDL= "SDL Widkit">.

    <item*|<verbatim|vue>>The <name|Vue> port: <name|SDL3> and, with
    <verbatim|--with-thorvg=<em|prefix>> (<source-link|misc/m4/thorvg.m4|misc/m4/thorvg.m4>), the GPU
    renderer (<cpp|USE_THORVG>; <name|ThorVG> is built by
    <source-link|misc/thorvg/build-thorvg.sh|misc/thorvg/build-thorvg.sh>, and <name|OpenGL> is linked).
    The sources of <source-link|Plugins/Vue|src/Plugins/Vue> are compiled with
    <verbatim|-std=c++20>, and <source-link|clay.c|src/Plugins/Vue/clay.c> as <name|C>.

    <item*|<verbatim|cocoa> or <verbatim|aqua>>The <name|Cocoa> port:
    defines <verbatim|AQUATEXMACS>, sets <verbatim|CONFIG_COCOA= NS> and
    links with <verbatim|-framework Cocoa -framework PDFKit>. It needs the
    <name|macOS> extensions (<verbatim|CONFIG_MACOS>), so
    <verbatim|--disable-macosx-extensions> is refused. The packaging script
    <source-link|packages/macos/build-ns-app.sh|packages/macos/build-ns-app.sh> configures, builds and
    bundles it.

    <item*|<verbatim|--enable-qt-new>>Compile <source-link|Plugins/Qt6|src/Plugins/Qt6>
    instead of <verbatim|Plugins/Qt> (<source-link|misc/m4/qt.m4|misc/m4/qt.m4>): the variable
    <verbatim|QT_PLUGIN_DIR> becomes <verbatim|Qt6>, which selects both the
    sources and the include path <verbatim|-IPlugins/Qt6>. The option is on
    by default when <verbatim|CONFIG_OS> is <verbatim|ANDROID>, and
    <verbatim|configure> stops with an error if the <name|Qt> found is
    older than 6.10. <name|Qtwk> always takes its files from
    <verbatim|Plugins/Qt>.

    <item*|<verbatim|--enable-qtpipes>>Use <name|Qt> processes instead of
    <name|Unix> pipes for plug-ins (<cpp|QTPIPES>). It is the default of
    <verbatim|qt> and <verbatim|qtwk> and refused with the other ports.
  </description>

  Whether <name|MuPDF> is used depends on the port
  (<verbatim|TM_MUPDF_FOR_GUI> in <source-link|misc/m4/mupdf.m4|misc/m4/mupdf.m4>): <name|SDL> and
  <name|Vue> draw with it and need <verbatim|--with-mupdf>, <name|Qt> uses
  it for pictures when it is found, and <name|X11> and <name|Cocoa>, whose
  pictures clash with those of <name|MuPDF> at link time, refuse it.

  <source-link|src/makefile.in|src/makefile.in> then compiles the port directories through
  the substituted variables: <verbatim|@CONFIG_X11@>, <verbatim|@CONFIG_SDL@>
  and <verbatim|@CONFIG_VUE@> for the <name|C++> sources,
  <verbatim|@CONFIG_COCOA@ @CONFIG_MACOS@> for the <name|Objective-C>
  sources, and, for <name|Qt>, the directory
  <verbatim|Plugins/$(QT_PLUGIN_DIR)> (with the <name|Qt> meta object
  compiler run on its headers when <verbatim|@CONFIG_QT@> is not empty).

  <section|<name|CMake>>

  The top level <source-link|CMakeLists.txt|src/CMakeLists.txt> declares

  <\verbatim-code>
    set (TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, Qt6, Qt5, Qt4, Vue, SDL, X11)")

    option (QTPIPES "use Qt pipes" ON)
  </verbatim-code>

  and stops with an error for any other value.

  <\description>
    <item*|<verbatim|Qt>, <verbatim|Qt6>, <verbatim|Qt5>,
    <verbatim|Qt4>><verbatim|Qt4> and <verbatim|Qt5> force the corresponding
    major version; <verbatim|Qt> and <verbatim|Qt6> look for <name|Qt> 6
    first and fall back to <name|Qt> 5. The components used are
    <verbatim|Core>, <verbatim|Gui>, <verbatim|Widgets>,
    <verbatim|PrintSupport>, <verbatim|Svg> and <verbatim|Network>. The
    sources are <verbatim|Plugins/Qt/*.cpp> and <verbatim|*.hpp>, without
    the <verbatim|moc_*.cpp> left by a <verbatim|make> build, since
    <name|CMake> runs its own <verbatim|AUTOMOC>. <verbatim|Qt6> does not
    select <source-link|Plugins/Qt6|src/Plugins/Qt6>.

    <item*|<verbatim|Vue>, <verbatim|SDL>><name|SDL3> and
    <name|SDL3_ttf> through <verbatim|pkg-config>, and <name|MuPDF>
    (<verbatim|USE_MUPDF>, on by default for these two, with
    <verbatim|MUPDF_DIR> as a hint), without which the configuration stops.
    The <name|Vue> sources are compiled as <name|C++20> in an object library
    of their own (<verbatim|texmacs_cxx20> in
    <source-link|src/CMakeLists.txt|src/CMakeLists.txt>). There is no <name|ThorVG> option, so a
    <name|CMake> build of <name|Vue> has no GPU renderer.

    <item*|<verbatim|X11>><verbatim|find_package (X11)>, <name|X11> and
    <name|Widkit> sources, <name|MuPDF> off.
  </description>

  <name|CMake> cannot build the <source-link|Plugins/Qt6|src/Plugins/Qt6> fork, <name|Qtwk>
  or <name|Cocoa>.

  <section|Android>

  The <name|Android> launcher in <source-link|src/packages/android/launcher|packages/android/launcher>
  is a separate <name|CMake> project which links a prebuilt
  <verbatim|libtexmacs.a> with <name|Qt> 6 or 5 (<verbatim|find_package
  (QT NAMES Qt6 Qt5 ...)>). The library itself is configured with
  <verbatim|configure>, where <verbatim|--enable-qt-new> is the default for
  <name|Android>, so a library configured for <name|Android> uses
  <source-link|Plugins/Qt6|src/Plugins/Qt6> unless <verbatim|--disable-qt-new> is given. The
  operating system side of <name|Android> is described in <hlink|platform
  support|system-platforms.en.tm>.

  <section|Headless mode>

  Headless mode is not a separate port but a run-time mode, selected by
  <verbatim|-headless> (and implied by the conversion and web site options,
  see <hlink|the main program|server-startup.en.tm>); <cpp|is_headless ()>
  in <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp> tells whether it is on. Two
  ports implement it:

  <\description>
    <item*|<name|Qt>>A <cpp|QTMCoreApplication> replaces the
    <cpp|QTMApplication>, no window is shown, and the interpose handler
    skips the screen updates; see also the section on headless mode in
    <hlink|the <name|Qt> implementation|widgets-qt.en.tm>. <name|Qtwk>
    creates a <cpp|QTWKCoreApplication> in the same way, but its windows do
    not test <cpp|is_headless>.

    <item*|<name|Vue>><cpp|gui_open> opens no display at all, the windows
    are virtual windows without an <name|SDL> window, and the event loop
    is replaced by <cpp|headless_loop> (<source-link|vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>), which runs
    the queued commands and the interpose handler every 10<nbsp>ms until a
    command quits. This is what the documentation checks of this tree use
    (with <verbatim|SDL_VIDEO_DRIVER=dummy> and
    <verbatim|TEXMACS_VUE_GPU=0> as a precaution).
  </description>

  <name|X11>, <name|SDL> and <name|Cocoa> do not test <cpp|is_headless>:
  with <verbatim|-headless> they still connect to the display (from
  reading the code).

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
