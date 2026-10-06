<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Building with the autotools>

  <section|Quick start>

  The traditional build, described for users in the file
  <verbatim|COMPILE>, is

  <\verbatim-code>
    ./configure

    make

    make install
  </verbatim-code>

  run from <verbatim|src/>. When <source-link|configure.in|configure.in> or one of the
  macros in <verbatim|misc/m4/> has changed, <verbatim|configure> must be
  regenerated first with <verbatim|autoreconf -fi> (as in the
  <verbatim|Dockerfile>). The build is done in the source tree: objects go
  to <verbatim|src/Objects/>, dependency files to <verbatim|src/Deps/>,
  and the binary to <verbatim|TeXmacs/bin/texmacs.bin>.

  <section|Configuration>

  <source-link|configure.in|configure.in> checks the compilers (preferring
  <verbatim|clang> and <verbatim|clang++>, with <name|Objective-C> for
  <name|macOS>), the sizes of the basic types and a few headers, and then
  calls one macro per subject. The options which matter most are:

  <\description>
    <item*|<name|Guile>>(<verbatim|guile.m4>) <verbatim|--with-guile=<em|path>>
    gives the <verbatim|guile-config> program to use;
    <verbatim|--with-guile=embedded18> uses the embedded <name|Guile> 1.8
    in <verbatim|tm-guile188/>, which is also chosen automatically when
    that directory exists. Without either, <verbatim|configure> looks for
    <verbatim|guile18-config>, <verbatim|guile1.8-config>, <verbatim|guile16-config>,
    ..., <verbatim|guile-config> and then <verbatim|guile20-config> and
    similar names. <name|Guile> 2 and later are refused unless
    <verbatim|--enable-guile2> is given. The version determines the
    dialect macro <verbatim|GUILE_A> to <verbatim|GUILE_D>; when cross
    compiling, it must be given in the variable <verbatim|GUILE_VERSION>.

    <item*|Graphical port>(<verbatim|tm_gui.m4>, <verbatim|qt.m4>)
    <name|Qt> is the default; <verbatim|--disable-qt> builds the
    historical <name|X11> port and <verbatim|--enable-cocoa> the
    experimental <name|Cocoa> port. <verbatim|--enable-qtpipes> replaces
    <name|Unix> pipes by <name|Qt> pipes. The <name|Qt> installation is
    found through <verbatim|qmake> (variables <verbatim|QMAKE>,
    <verbatim|QT_PATH>, <verbatim|MOC>, ...). On <name|macOS> with
    <name|Qt>, the <name|Objective-C> code of <verbatim|src/Plugins/MacOS>
    is added.

    <item*|Libraries><verbatim|--with-freetype>, <verbatim|--with-iconv>,
    <verbatim|--with-gnutls>, <verbatim|--with-aspell>,
    <verbatim|--with-cairo>, <verbatim|--with-imlib2>,
    <verbatim|--with-resvg>, <verbatim|--with-sparkle> (automatic updates
    on <name|macOS> and <name|Windows>, with <verbatim|--with-appcast>),
    <verbatim|--with-axel>, <verbatim|--disable-gs>,
    <verbatim|--disable-pdf-renderer>, and the <name|SQLite> check of
    <verbatim|sql.m4>.

    <item*|Debugging and optimization>(<verbatim|tm_debug.m4>,
    <verbatim|tm_optimize.m4>) <verbatim|--enable-debug>,
    <verbatim|--enable-assert>, <verbatim|--enable-warnings>,
    <verbatim|--enable-checks>, <verbatim|--enable-profile>,
    <verbatim|--enable-sanitizers>, <verbatim|--enable-optimize>,
    <verbatim|--disable-fastalloc> (use the system allocator instead of
    the fast allocator for small objects, useful with memory checkers) and
    <verbatim|--enable-experimental> (the style rewriting code in
    <verbatim|src/Style/>).

    <item*|Developer kit><verbatim|--with-tmrepo=<em|dir>> (<verbatim|tm_repo.m4>)
    uses a <TeXmacs> <abbr|SDK> directory: its <verbatim|bin>,
    <verbatim|include>, <verbatim|lib> and <verbatim|pkgconfig>
    directories are put first in the search paths. It is also required for
    some packaging targets (see <hlink|packages|build-packaging.en.tm>).
  </description>

  The platform macro (<verbatim|tm_platform.m4>) chooses the operating
  system layer, the static or dynamic link mode (<verbatim|tm_static.m4>)
  and the default packaging targets.

  <section|Generated files>

  <verbatim|configure> writes
  <verbatim|src/System/config.h> (from <source-link|src/System/config.in|src/System/config.in>, as
  declared by <verbatim|AC_CONFIG_HEADERS>) and the files listed in
  <verbatim|AC_CONFIG_FILES>, among which:

  <\itemize>
    <item><verbatim|Makefile>, <verbatim|src/makefile> and
    <verbatim|misc/admin/admin.makefile>;

    <item><verbatim|src/System/tm_configure.hpp> (from
    <source-link|tm_configure.in|src/System/tm_configure.in>), with the version and the build
    description;

    <item>the scripts <verbatim|misc/scripts/texmacs> and
    <verbatim|misc/scripts/fig2ps>, the manual page
    <verbatim|misc/man/texmacs.1> and the <name|Doxygen> configuration;

    <item>the package descriptions: <verbatim|packages/redhat/TeXmacs.spec>,
    the <name|Debian> control files, the <name|macOS>
    <verbatim|Info.plist> and <name|Xcode> configuration, the
    <name|Windows> resource file and <name|Inno Setup> script, the
    <name|MSIX> manifests and the <name|Android> manifest;

    <item>the makefile of the example dynamic link plug-in
    <verbatim|TeXmacs/examples/plugins/dynlink/>.
  </itemize>

  None of these generated files is under version control; only their
  templates (<verbatim|*.in>) are.

  <section|Makefile targets>

  The top level <verbatim|Makefile> (from <verbatim|Makefile.in>) has the
  following main targets:

  <\description>
    <item*|<verbatim|TEXMACS>>(the default) Builds the embedded
    <name|Guile> if needed and then the binary through
    <verbatim|src/makefile>.

    <item*|<verbatim|STATIC_TEXMACS>>A statically linked binary.

    <item*|<verbatim|deps>>Regenerates the dependency files.

    <item*|<verbatim|GLUE>>Regenerates the <scheme> glue
    (<verbatim|src/Scheme/Glue/glue_*.cpp>), see <hlink|the <scheme>
    glue|scheme-bridge-glue.en.tm>. In <verbatim|src/makefile>, a glue
    file also depends on its <verbatim|build-glue-*.scm> declaration file,
    so the glue is regenerated automatically when the dependency files are
    up to date.

    <item*|<verbatim|PLUGINS>, <verbatim|EX_PLUGINS>>The binaries of the
    plug-ins in <verbatim|plugins/> and of the example plug-ins.

    <item*|<verbatim|install>, <verbatim|uninstall>>Installation into the
    prefix: executables, data, plug-ins, icons, desktop files, include
    files and manual pages.

    <item*|<verbatim|PACKAGE>, <verbatim|BUNDLE>>The default package and
    application bundle of the platform; see <hlink|packages for the various
    platforms|build-packaging.en.tm>.

    <item*|<verbatim|clean>, <verbatim|distclean>>Remove the objects; <verbatim|distclean> also removes the embedded
    <name|Guile> build, the makefiles, the configuration headers, the
    scripts and the manual page (but not the generated package
    descriptions in <verbatim|packages/>).
  </description>

  <section|Pitfalls>

  <\itemize>
    <item>The two build systems do not accept the same <name|Guile>
    versions: the autotools build refuses <name|Guile> 2 and later without
    <verbatim|--enable-guile2>, whereas <name|CMake> with
    <verbatim|SCHEME_IMPL=system> accepts <name|Guile> 3.0 silently.

    <item>An autotools build leaves <verbatim|src/System/config.h> and
    <verbatim|tm_configure.hpp> in the source tree, where they can shadow
    the headers of a later <name|CMake> build (see <hlink|building with
    <name|CMake>|build-cmake.en.tm>).

    <item>The configuration is cached in <verbatim|config.status>; after
    switching branches with different <source-link|configure.in|configure.in> files, run
    <verbatim|autoreconf -fi> and <verbatim|./configure> again rather than
    relying on <verbatim|config.status --recheck>.
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
