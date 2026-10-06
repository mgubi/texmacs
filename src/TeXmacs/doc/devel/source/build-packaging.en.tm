<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Packages for the various platforms>

  Distribution packages are made by targets of the autotools
  <verbatim|Makefile>, which use the platform files in
  <source-link|packages/|packages>. All packages are written to the directory
  <verbatim|../distr/> next to <source-link|src/|src>. The generic targets
  <verbatim|make PACKAGE> and <verbatim|make BUNDLE> are mapped by
  <source-link|tm_platform.m4|misc/m4/tm_platform.m4> to the right target of the platform:
  <verbatim|GENERIC_PACKAGE> on <name|Linux> and other <name|Unix> systems,
  <verbatim|MACOS_BUNDLE>/<verbatim|MACOS_PACKAGE> on <name|macOS>, and
  <verbatim|WINDOWS_BUNDLE>/<verbatim|WINDOWS_PACKAGE> on <name|Windows>.

  Most targets need a complete set of libraries which are not part of a
  standard system (a static <name|Qt>, the embedded <name|Guile>, ...).
  The official packages are built with the <em|<TeXmacs> builder>, which
  provides them in an <abbr|SDK> directory given to <verbatim|configure>
  with <verbatim|--with-tmrepo>; the <name|AppImage> target refuses to run
  without it.

  <section|Common steps>

  Every binary package bundles the runtime tree <source-link|TeXmacs/|packages/macos/TeXmacs>, the
  <verbatim|ice-9> directory of the <name|Guile> library (copied from
  <verbatim|GUILE_DATA_PATH> into <verbatim|progs/>, since <TeXmacs> loads
  it from there), <name|Ghostscript> if it was found at configuration time,
  and the public key <source-link|misc/admin/texmacs_updates_dsa_pub.pem|misc/admin/texmacs_updates_dsa_pub.pem>
  used to verify automatic updates (see <hlink|automatic
  updates|../scheme/api/automatic-updates.en.tm>). When <verbatim|configure>
  was given a signing identity (<source-link|tm_sign.m4|misc/m4/tm_sign.m4>), the executables
  and installers are signed.

  <section|<name|macOS>>

  <verbatim|MACOS_BUNDLE> builds <verbatim|../distr/TeXmacs.app>: it copies
  <verbatim|Info.plist>, <verbatim|PkgInfo>, the icons and
  <verbatim|Assets.car> from <source-link|packages/macos/|packages/macos>, the binary as
  <verbatim|Contents/MacOS/TeXmacs>, the localized resources of
  <source-link|src/Plugins/Cocoa/English.lproj|src/Plugins/Cocoa/English.lproj>, and the runtime tree into
  <verbatim|Contents/Resources/share/TeXmacs>. The script
  <source-link|packages/macos/bundle-libs.sh|packages/macos/bundle-libs.sh> then copies the <name|Qt>
  frameworks and plug-ins and the other dynamic libraries into the bundle
  and rewrites their install names, and the bundle is signed with
  <verbatim|codesign> if a signing identity was configured.
  <verbatim|MACOS_PACKAGE> wraps the bundle in a disk image with
  <verbatim|hdiutil>, and <verbatim|MACOS_RELEASE> makes a signed
  <verbatim|zip> archive for the updater.

  The directory also contains an <name|Xcode> project
  (<verbatim|TeXmacs.xcodeproj>) with configuration files for the
  <name|Qt>, <name|Cocoa> and <name|X11> ports.

  <section|<name|Windows>>

  <verbatim|WINDOWS_BUNDLE> assembles <verbatim|../distr/TeXmacs-Windows>:
  the runtime tree, the binary renamed to <verbatim|texmacs.exe>, the
  <name|Aspell> dictionaries of the <abbr|SDK>, the <name|Qt> plug-ins, and
  all the <abbr|DLL>s the executables depend on (found by
  <source-link|packages/windows/copydll.sh|packages/windows/copydll.sh>). <verbatim|WINDOWS_PACKAGE> runs
  <name|Inno Setup> (<verbatim|iscc>) on
  <verbatim|packages/windows/TeXmacs.iss> to make the installer.
  <verbatim|WINDOWS_APPX> additionally makes two <name|MSIX> packages with
  <verbatim|makeappx>: one for direct distribution and one for the
  <name|Microsoft Store>, from the manifests in <source-link|packages/msix/|packages/msix>.
  The <name|Windows> builds are done with <name|MinGW> under <name|MSYS2>;
  <source-link|packages/windows/configure-tm-mingw-cross-env|packages/windows/configure-tm-mingw-cross-env> is a helper for
  cross compiling.

  <section|<name|Linux> and other <name|Unix> systems>

  <\description>
    <item*|Generic binaries><verbatim|GENERIC_PACKAGE> strips the binary
    and makes a <verbatim|tar.gz> of the runtime tree;
    <verbatim|GENERIC_X11_PACKAGE> builds a static binary of the <name|X11>
    port in a temporary copy of the sources.

    <item*|Distribution packages><verbatim|DEBIAN_PACKAGE> and
    <verbatim|UBUNTU_PACKAGE> (with <verbatim|debuild>),
    <verbatim|REDHAT_PACKAGE>, <verbatim|FEDORA_PACKAGE>,
    <verbatim|CENTOS_PACKAGE> and <verbatim|MANDRIVA_PACKAGE> (with
    <verbatim|rpmbuild> and the <verbatim|TeXmacs.spec> files) all start
    from a source archive made by <verbatim|COPY_SOURCES_TGZ>, which
    includes the embedded <name|Guile>.

    <item*|<name|AppImage>><verbatim|APPIMAGE> installs <TeXmacs> into
    <verbatim|../distr/TeXmacs.AppDir>, adds the desktop entry and the
    <verbatim|AppRun> script of <source-link|packages/appimage/|packages/appimage>, fixes the
    run path of the binary with <verbatim|patchelf> and copies the shared
    libraries of the <abbr|SDK>.

    <item*|Source archive><verbatim|SRC_PACKAGE>.
  </description>

  The scripts in <source-link|packages/linux/|packages/linux> install the icons and the
  <name|MIME> types into a desktop environment.

  <section|<name|Android>>

  <verbatim|ANDROID_LIBTEXMACS> compiles <TeXmacs> into a static library
  <verbatim|src/Objects/libtexmacs.a> (configured for an <name|Android>
  cross compiler). <verbatim|ANDROID_BUNDLE> prepares a project in
  <verbatim|../distr/TeXmacs-Android> from the launcher in
  <source-link|packages/android/launcher/|packages/android/launcher> (whose <cpp|main> calls
  <cpp|texmacs_entrypoint>), the manifest and resources, and the runtime
  tree collected as assets by <source-link|collect_assets.sh|packages/android/collect_assets.sh>.
  <verbatim|ANDROID_AAB> and <verbatim|ANDROID_DEV_APK> build it with
  <name|CMake> and the <name|Android> <abbr|NDK> and <abbr|SDK> into an
  application bundle or a development <abbr|APK>. The platform layer is
  described in <hlink|the system layer|system-platforms.en.tm>.

  <section|Pitfalls>

  <\itemize>
    <item><verbatim|MACOS_RELEASE> archives the wrong path:
    <source-link|Makefile.in:518|Makefile.in:518> writes <verbatim|$MACOS_PACKAGE_APP> with a
    single <verbatim|$>, which <verbatim|make> reads as the (empty)
    variable <verbatim|$M> followed by the text
    <verbatim|ACOS_PACKAGE_APP>, so <verbatim|zip> is asked to archive a
    non-existent file. It should be <verbatim|$(MACOS_PACKAGE_APP)>.

    <item>The packaging targets only exist in the autotools build;
    <name|CMake> can build and install, but not package (see
    <hlink|building with <name|CMake>|build-cmake.en.tm>).

    <item><verbatim|distclean> does not remove the package descriptions
    generated from <verbatim|packages/*/*.in>, which therefore keep the
    version of the last configuration until <verbatim|configure> is run
    again.
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
