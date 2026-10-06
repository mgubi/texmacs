<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Paths, directories and settings at boot time>

  The order of the boot steps is described in <hlink|the main program and
  crash handling|server-startup.en.tm>. This page describes what the
  steps which concern the system layer do: finding the installation,
  setting the environment variables which all search paths are built
  from, creating the user directories, managing temporary directories,
  and keeping the settings file. The code is in
  <source-link|System/Boot/init_texmacs.cpp|src/System/Boot/init_texmacs.cpp>, <source-link|init_upgrade.cpp|src/System/Boot/init_upgrade.cpp>,
  <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp> and the entry points of the
  platform layers.

  <section|Finding the installation>

  <verbatim|$TEXMACS_PATH> is the directory with the <scheme> code, styles,
  fonts and documentation of the installation (in the source tree,
  <source-link|src/TeXmacs|TeXmacs>). It is determined in two places:

  <\enumerate>
    <item>The <cpp|main> functions of <source-link|Plugins/Unix|src/Plugins/Unix> and
    <source-link|Plugins/Windows64|src/Plugins/Windows64> call <cpp|setup_texmacs_path>. If
    <verbatim|TEXMACS_PATH> is set and valid, it is kept. Otherwise the
    function tries directories relative to the executable
    (<cpp|texmacs_get_application_directory>): on <name|macOS>
    <verbatim|../Resources/share/TeXmacs> inside a bundle, then
    <verbatim|TeXmacs> next to the executable, the parent directory, and
    (on <name|Unix>) <verbatim|usr/share/TeXmacs>,
    <verbatim|usr/local/share/TeXmacs>, <verbatim|../usr/share/TeXmacs>,
    <verbatim|/usr/share/TeXmacs> and <verbatim|/usr/local/share/TeXmacs>.
    The first valid one becomes <verbatim|TEXMACS_PATH>. A directory is
    <em|valid> for <cpp|test_texmacs_path> if it contains
    <verbatim|doc>, <verbatim|fonts>, <verbatim|progs>, <verbatim|styles>
    and a file <verbatim|SVNREV> whose contents equal
    <cpp|ALTERNATIVE_VERSION> (the version series, such as
    <verbatim|2.1.>, read from <verbatim|src/TeXmacs/SVNREV> at build
    time). This prevents an executable from silently running with the
    files of another <TeXmacs> version.

    <item><cpp|TeXmacs_init_paths> in <source-link|texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp> then handles
    bundles: on <name|macOS> it sets <verbatim|TEXMACS_PATH> to
    <verbatim|../Resources/share/TeXmacs> if it is still unset, adds the
    bundle's <verbatim|Plugins>, <verbatim|Frameworks> and
    <verbatim|Resources/lib> directories to the <name|Qt> plug-in path and
    the dynamic library paths, and appends the <verbatim|PATH> of a login
    shell (obtained by running <verbatim|$SHELL -l -c 'echo $PATH'>) and
    the bundle's <verbatim|bin> directory to <verbatim|PATH>; on
    <name|Windows> it uses the parent of the executable's directory and
    sets <verbatim|PWD> if it is missing or comes from a <name|Unix>
    environment (<name|Wine>); on <name|Haiku> it uses
    <verbatim|../data/TeXmacs>. If <verbatim|TEXMACS_PATH> still does not
    exist, the program exits.
  </enumerate>

  The shell script <verbatim|misc/scripts/texmacs> used by <name|Unix>
  installations sets <verbatim|TEXMACS_PATH> and
  <verbatim|TEXMACS_BIN_PATH> before starting <verbatim|texmacs.bin>, and
  puts <verbatim|$TEXMACS_BIN_PATH/bin> and
  <verbatim|$TEXMACS_BIN_PATH/lib> in front of the search paths.

  <verbatim|$TEXMACS_HOME_PATH> is the user's directory: it defaults to
  <verbatim|~/.TeXmacs>, to <verbatim|%APPDATA%\\TeXmacs> on
  <name|Windows> and to <verbatim|~/config/settings/TeXmacs> on
  <name|Haiku>, and is set by <cpp|immediate_options> if it is not set in
  the environment (<cpp|init_main_paths> repeats the test later). The
  server certificates directory <verbatim|TEXMACS_SERVER_CERT_DIR>
  defaults to <verbatim|$TEXMACS_HOME_PATH/server>.

  <section|Environment variables and search paths>

  Most search paths of <TeXmacs> are environment variables, so that they
  can be overridden from outside and are inherited by plug-ins.
  <cpp|init_guile> and <cpp|init_env_vars> set them, keeping the value
  given in the environment where noted. Each path includes the
  corresponding directory of every installed plug-in, as computed by
  <cpp|plugin_path (sub)>, which searches
  <verbatim|$TEXMACS_HOME_PATH>, <verbatim|/etc/TeXmacs>,
  <verbatim|$TEXMACS_PATH> and <verbatim|/usr/share/TeXmacs> for
  <verbatim|plugins/*/<em|sub>>.

  <\description-paragraphs>
    <item*|<verbatim|GUILE_LOAD_PATH>><verbatim|$TEXMACS_PATH/progs>, the
    previous value, <verbatim|$TEXMACS_HOME_PATH/progs> and the
    <verbatim|progs> directories of plug-ins.
    <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> must be found in the first two, or the boot
    fails.

    <item*|<verbatim|PATH>, <verbatim|LD_LIBRARY_PATH>>The previous value
    followed by the plug-ins' <verbatim|bin> and <verbatim|lib>
    directories; on <name|Windows> and <name|macOS> also
    <verbatim|$TEXMACS_PATH/bin>; the <verbatim|manual path> preference
    is put in front.

    <item*|<verbatim|TEXMACS_STYLE_ROOT>,
    <verbatim|TEXMACS_PACKAGE_ROOT>, <verbatim|TEXMACS_STYLE_PATH>>The
    roots for style files and packages (user directory, installation,
    plug-ins), and all their subdirectories. Kept if set.

    <item*|<verbatim|TEXMACS_TEXT_ROOT>, <verbatim|TEXMACS_TEXT_PATH>>The
    same for the <verbatim|texts> directories. Kept if set.

    <item*|<verbatim|TEXMACS_FILE_PATH>>The text and style paths. Kept if
    set.

    <item*|<verbatim|TEXMACS_DOC_PATH>>The previous value followed by
    <verbatim|$TEXMACS_HOME_PATH/doc>, <verbatim|$TEXMACS_PATH/doc> and
    the plug-ins' <verbatim|doc> directories.

    <item*|<verbatim|TEXMACS_SECURE_PATH>>The previous value followed by
    <verbatim|$TEXMACS_PATH> and <verbatim|$TEXMACS_HOME_PATH>; see
    <cpp|is_secure> in <hlink|URLs|system-urls.en.tm>.

    <item*|<verbatim|TEXMACS_PATTERN_PATH>,
    <verbatim|TEXMACS_PIXMAP_PATH>, <verbatim|TEXMACS_DIC_PATH>,
    <verbatim|TEXMACS_THEME_PATH>>Background patterns, icons,
    dictionaries and themes. Kept if set.

    <item*|<verbatim|TEXMACS_SOURCE_PATH>>The source directory given at
    build time (empty on <name|Windows>).
  </description-paragraphs>

  Because <abbr|URL>s expand environment variables when they are
  constructed (<hlink|URLs|system-urls.en.tm>), code which builds search
  paths must run after <cpp|init_env_vars>. <cpp|init_misc> finally
  checks whether the shell command <verbatim|which> works
  (<cpp|use_which>), which <cpp|resolve_in_path> relies on.

  <section|User directories>

  <cpp|init_user_dirs> creates the directory tree of
  <verbatim|$TEXMACS_HOME_PATH> if needed: <verbatim|bin>,
  <verbatim|doc>, <verbatim|fonts> (with subdirectories for the various
  font formats and for fonts that failed to load), <verbatim|langs>,
  <verbatim|misc>, <verbatim|packages>, <verbatim|plugins>,
  <verbatim|progs>, <verbatim|server>, <verbatim|styles>,
  <verbatim|system> (with <verbatim|bib>, <verbatim|cache>,
  <verbatim|certificates>, <verbatim|database>, <verbatim|make> and
  <verbatim|tmp>), <verbatim|texts> (with <verbatim|backup> and
  <verbatim|scratch>) and <verbatim|users>. The directories
  <verbatim|server>, <verbatim|system> and <verbatim|users> are made
  accessible to the user only (mode <verbatim|0700>). The other files in
  <verbatim|system> include <verbatim|settings.scm> (below),
  <verbatim|preferences.scm> (see <hlink|preferences|server-events.en.tm>) and the boot lock.

  <section|Temporary directories>

  Each process uses its own temporary directory,
  <verbatim|$TEXMACS_HOME_PATH/system/tmp/<em|pid>>, returned by
  <cpp|url_temp_dir>. At the end of <cpp|init_user_dirs>,
  <cpp|clean_temp_dirs> removes the directories of processes which are no
  longer running: for every numeric entry of <verbatim|system/tmp>, it
  runs <verbatim|ps -p <em|pid>> and deletes the directory with
  <verbatim|rm -rf> unless the output mentions both <verbatim|texmacs> and
  the process number (<cpp|process_running>). On 32-bit <name|Windows> the
  directories are named after the start time instead, and are removed
  after seven days. Other entries of <verbatim|system/tmp>, such as
  <verbatim|tree_cache> (the cache of the remote file system) or the
  previews of the print dialog, are left alone.

  <section|Settings and upgrades>

  <verbatim|$TEXMACS_HOME_PATH/system/settings.scm> holds the tuple
  <cpp|texmacs_settings> of pairs (variable, value), read and written with
  <cpp|get_setting> and <cpp|set_setting>. Its only systematic entry is
  <verbatim|VERSION>. <cpp|init_plugins> (called from
  <cpp|TeXmacs_main>) reads it:

  <\itemize>
    <item>If there is neither <verbatim|settings.scm> nor the file
    <verbatim|TEX_PATHS> of very old versions, this is a first run:
    <cpp|setup_texmacs> creates the settings file (also running the
    <TeX> font setup <cpp|setup_tex>) and <cpp|install_status> is set to
    1, which makes the welcome message appear.

    <item>If the recorded version differs from the current one,
    <cpp|init_upgrade> recreates the settings, renames the user
    initialization files of very old versions, removes the style, file,
    documentation, directory and attribute caches, the font database
    files and the font error files, and
    reloads the cache. For upgrades from versions up to 1.0.7.9 it also
    assembles a document with the list of changes, in which case
    <cpp|install_status> is 2.
  </itemize>

  The boot lock, <verbatim|system/boot_lock>, is created by
  <cpp|acquire_boot_lock> during <cpp|init_texmacs> and removed by
  <cpp|release_boot_lock> just before the event loop starts. If it is
  found at startup, the previous run crashed while booting, and the
  settings, the setup file, the caches and the font error files are
  removed.

  <section|Pitfalls>

  <\itemize>
    <item><cpp|process_running> looks for the lower case word
    <verbatim|texmacs> in the output of <verbatim|ps>
    (<source-link|System/Boot/init_texmacs.cpp:151|src/System/Boot/init_texmacs.cpp:151>). The executable of the
    <name|macOS> bundle is <verbatim|.../TeXmacs.app/Contents/MacOS/TeXmacs>,
    which does not contain it. A running bundled instance is therefore
    considered dead, and any other instance which starts later (another
    bundle instance, or a <verbatim|texmacs.bin -headless> run sharing the
    same <verbatim|$TEXMACS_HOME_PATH>) deletes its temporary directory,
    and with it the temporary files it is using (downloads, conversions,
    previews). For example, with the bundle running as process 1665,
    <verbatim|ps -p 1665> prints
    <verbatim|/Applications/TeXmacs.app/Contents/MacOS/TeXmacs>.

    <item>On <name|Unix>, <cpp|texmacs_get_application_directory>
    returns the directory of the executable, except when
    <verbatim|TEXMACS_BIN_PATH> is set: then it returns the
    <em|parent> of <verbatim|$TEXMACS_BIN_PATH>, which the launcher script
    sets to the installation directory (the one containing
    <verbatim|bin>), so the two cases differ by two levels. On <name|Unix>
    systems other than <name|Linux> and <name|macOS>, without
    <verbatim|TEXMACS_BIN_PATH>, the function reaches its end without a
    <verbatim|return> statement (<verbatim|Plugins/Unix/unix_system.cpp:210-230>),
    which is undefined behaviour.

    <item>On <name|macOS>, every start runs a login shell to obtain the
    user's <verbatim|PATH>; a slow shell initialization slows down the
    start of <TeXmacs> accordingly.

    <item><verbatim|setup_texmacs_home_path> in
    <source-link|Plugins/Windows64/windows64_entrypoint.cpp|src/Plugins/Windows64/windows64_entrypoint.cpp> (which would use
    the <em|local> application data directory) is never called; the home
    directory on <name|Windows> is the roaming
    <verbatim|%APPDATA%\\TeXmacs> set by <cpp|immediate_options>.
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
