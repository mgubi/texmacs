<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The platform layers>

  The portable code never calls the C library or the operating system
  directly for files, directories, environment variables and processes.
  It calls a small set of functions with the prefix <cpp|texmacs_>, which
  are declared and implemented once per platform:

  <\description>
    <item*|<source-link|Plugins/Unix/unix_system.hpp|src/Plugins/Unix/unix_system.hpp>>Used on <name|Linux>,
    <name|macOS> and the other <name|Unix> systems.

    <item*|<source-link|Plugins/Windows64/windows64_system.hpp|src/Plugins/Windows64/windows64_system.hpp>>Used for
    64-bit <name|Windows> builds (<verbatim|OS_MINGW64>).

    <item*|<source-link|Plugins/Windows/windows32_system.hpp|src/Plugins/Windows/windows32_system.hpp>>Used for the
    older 32-bit <name|Windows> builds.

    <item*|<source-link|Plugins/Android/android_system.hpp|src/Plugins/Android/android_system.hpp>>Used on
    <name|Android>.
  </description>

  <source-link|System/Files/file.cpp|src/System/Files/file.cpp> and <source-link|System/Misc/sys_utils.hpp|src/System/Misc/sys_utils.hpp>
  include the right header with preprocessor tests.

  <section|The common interface>

  <\description>
    <item*|Files><cpp|texmacs_fopen (name, mode, lock)>,
    <cpp|texmacs_fread>, <cpp|texmacs_fwrite>, <cpp|texmacs_fsize>,
    <cpp|texmacs_fclose (f, unlock)>, <cpp|texmacs_lock_file>,
    <cpp|texmacs_unlock_file>. Names are <name|UTF-8> strings.

    <item*|Directories and attributes><cpp|texmacs_opendir>,
    <cpp|texmacs_readdir> (which returns a <cpp|texmacs_dirent> with a
    validity flag and the name), <cpp|texmacs_closedir>,
    <cpp|texmacs_stat>, <cpp|texmacs_mkdir>, <cpp|texmacs_rmdir>,
    <cpp|texmacs_rename>, <cpp|texmacs_chmod>, <cpp|texmacs_remove>.

    <item*|Errors><cpp|texmacs_reset_last_error>,
    <cpp|texmacs_get_last_error>, <cpp|texmacs_get_last_error_str>.

    <item*|Environment><cpp|texmacs_getenv>, <cpp|texmacs_setenv>.

    <item*|Miscellaneous><cpp|get_default_theme> (light or dark, following
    the system with <name|Qt> 6.5 and later),
    <cpp|texmacs_get_application_directory>,
    <cpp|texmacs_init_guile_hooks>.
  </description>

  Processes are run by <source-link|unix_sys_utils.cpp|src/Plugins/Unix/unix_sys_utils.cpp> on <name|Unix>, by
  <cpp|windows_system> and <cpp|mingw_system> on 64-bit <name|Windows>,
  and by <name|Qt> (<cpp|qt_system>) on 32-bit <name|Windows> and
  <name|Android>; see <hlink|programs, web requests, messages and
  timing|system-utils.en.tm>.

  <section|Unix and macOS>

  The <name|Unix> layer maps the interface directly to the C library:
  names are passed unchanged, locks are <cpp|flock> locks
  (<cpp|LOCK_EX>), and <cpp|texmacs_stat> is <cpp|stat>.
  <cpp|texmacs_get_application_directory> uses <verbatim|/proc/self/exe> on
  <name|Linux> and <cpp|_NSGetExecutablePath> on <name|macOS>.
  <source-link|unix_entrypoint.cpp|src/Plugins/Unix/unix_entrypoint.cpp> contains <cpp|main>: it installs the
  <scheme> hooks, finds <verbatim|TEXMACS_PATH> (<hlink|paths, directories
  and settings|system-boot.en.tm>), adds the <verbatim|usr/bin>
  directories of an <name|AppImage> to <verbatim|PATH>, forces the
  <name|X11> platform of <name|Qt> 5 when no <name|Wayland> display is
  present, and calls <cpp|texmacs_entrypoint>. The file also contains a
  disabled experiment (<verbatim|EXPERIMENTAL_REEXEC_DETACHED>) which
  re-executes the program detached from the terminal. Stack traces use
  <cpp|backtrace>, and the server log uses <verbatim|syslog> on
  <name|Linux> and <verbatim|os_log> on <name|macOS>.

  <verbatim|Plugins/MacOS/> adds services which only exist on
  <name|macOS>:

  <\description>
    <item*|<source-link|mac_utilities.mm|src/Plugins/MacOS/mac_utilities.mm>><cpp|mac_alternate_startup> (is
    the <key|Alt> key held during startup, in which case the settings and
    caches are reset), the event filter which repairs <key|Ctrl+Tab> in
    some <name|Qt> versions, the support for <name|Apple> remote controls,
    <cpp|mac_begin_server> and <cpp|mac_end_server>, which declare a
    background activity so that the system does not put a running
    <TeXmacs> server to sleep (App Nap), and the unified title bar of the
    windows.

    <item*|<source-link|mac_spellservice.mm|src/Plugins/MacOS/mac_spellservice.mm>>Spell checking with the system
    dictionaries.

    <item*|<source-link|mac_images.mm|src/Plugins/MacOS/mac_images.mm>>Image sizes and conversions with
    <name|Cocoa>.

    <item*|<source-link|mac_app.mm|src/Plugins/MacOS/mac_app.mm>, <source-link|cg_renderer.cpp|src/Plugins/MacOS/cg_renderer.cpp>>Support for
    the older <name|Cocoa> and <name|X11> front ends.
  </description>

  <section|Windows>

  The 64-bit layer converts all names and environment variables between
  <name|UTF-8> and the wide character <abbr|API>s of <name|Windows>
  (<source-link|windows64_encoding.cpp|src/Plugins/Windows64/windows64_encoding.cpp>), opens files in binary mode, and
  locks them with <cpp|LockFileEx>. <cpp|texmacs_init_guile_hooks>
  installs wide character versions of the file functions used by
  <name|Guile> (<cpp|stat>, <cpp|open>, <cpp|readdir>, <cpp|getenv>,
  ...), so that <scheme> code also works with non-<abbr|ASCII> names.
  <source-link|windows64_entrypoint.cpp|src/Plugins/Windows64/windows64_entrypoint.cpp> defines <cpp|main>,
  <cpp|WinMain> and <cpp|wWinMain>, which all:

  <\enumerate>
    <item>attach to the console of the parent process if there is one, so
    that messages are visible when <TeXmacs> is started from a terminal
    (except under <name|MSYS>);

    <item>set <verbatim|TEXMACS_DISPLAYNAME> to the full name of the user
    if it is not set;

    <item>find <verbatim|TEXMACS_PATH> next to the executable;

    <item>convert the command line, obtained in wide characters from the
    system, to <name|UTF-8> and call <cpp|texmacs_entrypoint>.
  </enumerate>

  The 32-bit layer (<source-link|Plugins/Windows/|src/Plugins/Windows>) uses the
  <verbatim|nowide> library for the conversion of arguments and file
  names. Both layers have their own stack trace and server log
  implementations.

  <section|Android>

  The <name|Android> layer implements <cpp|texmacs_fopen> with a
  <cpp|QFile> (the returned <cpp|FILE*> is really a <cpp|QFile*>) and does
  no locking. <abbr|URL>s with the root <verbatim|content> denote documents
  provided by other applications; resolution only tests them for the requested type, their suffix is
  derived from their <abbr|MIME> type (<cpp|android_suffix_from_mime>),
  and <cpp|resolve_in_path> looks for programs in
  <verbatim|$TEXMACS_PATH/bin>. The application directory is the home
  directory. <source-link|android.cpp|src/Plugins/Android/android.cpp> starts a background service and
  exports the <abbr|JNI> function <cpp|callScheme>, by which the
  <name|Java> side can run a <scheme> command in the main thread.

  <section|Pitfalls>

  <\itemize>
    <item><cpp|mac_begin_server> declares a <em|local> variable
    <cpp|background_activity> which hides the static one
    (<source-link|Plugins/MacOS/mac_utilities.mm:547|src/Plugins/MacOS/mac_utilities.mm:547>). The static variable
    stays <cpp|nil>, so <cpp|mac_end_server> never ends the activity, each
    start of the server creates and retains a new one, and App Nap
    remains disabled after the server is stopped. <cpp|mac_end_server>
    also does not reset the variable after releasing it.

    <item><cpp|mac_fix_paths> is declared in
    <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp> but only defined for the old
    <name|Cocoa> front end and never called.

    <item>The behaviour of locks differs: advisory <cpp|flock> locks on
    <name|Unix>, mandatory <cpp|LockFileEx> locks on <name|Windows> (which
    block other programs as long as <TeXmacs> holds the file), none on
    <name|Android>.

    <item>See also the pitfalls of <cpp|texmacs_get_application_directory>
    and <cpp|texmacs_stat> in <hlink|paths, directories and
    settings|system-boot.en.tm> and <hlink|files and
    caches|system-files.en.tm>.
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
