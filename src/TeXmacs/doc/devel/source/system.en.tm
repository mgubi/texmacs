<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The system layer: files, URLs, caches and platform support>

  <section|Introduction>

  Everything in <TeXmacs> that touches the operating system goes through a
  small set of portable routines: names of files and other resources are
  <abbr|URL>s, files are read and written as strings, external programs are
  run through a few <cpp|system> functions, and the differences between
  <name|Unix>, <name|macOS>, <name|Windows> and <name|Android> are confined
  to a thin platform layer. This chapter describes that layer:

  <\itemize>
    <item>the <cpp|url> class, its tree representation, the parsing and
    printing of names, search paths and wildcards, and the resolution and
    concretization of <abbr|URL>s into local file names;

    <item>the file routines, the on-disk caches in
    <verbatim|$TEXMACS_HOME_PATH/system/cache>, the persistent key-value
    store and the <verbatim|make> directory;

    <item>the setup of paths, environment variables, user directories,
    temporary directories and the settings file during the boot;

    <item>the utilities for running external programs, fetching web files,
    sending <abbr|HTTP> requests, printing messages and measuring time;

    <item>the platform layers in <verbatim|Plugins/Unix>,
    <verbatim|Plugins/MacOS>, <verbatim|Plugins/Windows>,
    <verbatim|Plugins/Windows64> and <verbatim|Plugins/Android>.
  </itemize>

  Several neighbouring subjects are described elsewhere. The <scheme>
  interface to <abbr|URL>s is documented in <hlink|the URL
  system|../scheme/api/url.en.tm>, and the <verbatim|tmfs> file system,
  including the way the <c++> file layer calls back into <scheme> for
  <verbatim|tmfs> <abbr|URL>s, in <hlink|internals of the <TeXmacs> file
  system|../scheme/api/tmfs/tmfs-internals.en.tm>. The order of the boot
  steps, the command line options and the crash handler are in <hlink|the
  main program and crash handling|server-startup.en.tm>; user
  preferences are in <hlink|the event loop and
  preferences|server-events.en.tm>; memory allocation and the basic containers are in
  <hlink|basic data types|types.en.tm>; pipes, sockets and dynamic
  libraries used by plug-ins are in <hlink|the plug-in
  machinery|plugin-machinery.en.tm>.

  All file names below are relative to <verbatim|src/src/> unless stated
  otherwise.

  <section|Overview>

  The layers fit together as follows.

  <\verbatim-code>
    \ \ editor, typesetter, converters, Scheme glue

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ load_string / save_string / is_of_type / read_directory \ (file.cpp)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ resolve / concretize (url.cpp) \ \ \ \ cache_get / cache_set (data_cache.cpp)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ get_from_web / get_from_server \ \ \ \ $TEXMACS_HOME_PATH/system/cache

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ texmacs_fopen / texmacs_stat / ... \ \ (Plugins/Unix, Windows64, Android)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ operating system
  </verbatim-code>

  A typical read, <cpp|load_string (u, s, false)>, first <em|resolves>
  <cpp|u> (a name which may contain search paths, wildcards and environment
  variables) to one existing resource, then <em|concretizes> it to a local
  file name (downloading it or asking a <scheme> handler for a copy if it is
  remote), consults the file cache, and finally reads the file through the
  platform function <cpp|texmacs_fopen>. Each step is described on its own
  page below.

  <section|Source files>

  <\description>
    <item*|<verbatim|System/Classes/url.hpp>, <verbatim|url.cpp>>The class
    <cpp|url>: constructors and parsing, printing, operations, resolution
    (<cpp|complete>, <cpp|resolve>) and concretization.

    <item*|<verbatim|System/Files/file.hpp>, <verbatim|file.cpp>>Loading and
    saving strings, file tests and attributes, directories, temporary,
    scratch and backup names, file operations, file searches.

    <item*|<verbatim|System/Files/web_files.hpp>,
    <verbatim|web_files.cpp>>Local copies of web, <verbatim|tmfs> and
    <em|ramdisc> resources; <abbr|HTTP> <verbatim|POST> requests.

    <item*|<verbatim|System/Files/make_file.cpp>>Generated files in
    <verbatim|$TEXMACS_HOME_PATH/system/make> (downloaded images, images
    with effects).

    <item*|<verbatim|System/Files/tm_ostream.hpp>,
    <verbatim|tm_ostream.cpp>>Output streams: <cpp|cout>, <cpp|cerr>, the
    error, warning and debug channels.

    <item*|<verbatim|System/Files/image_files.cpp>>Image sizes and image
    conversion; described with the graphics output, not here.

    <item*|<verbatim|System/Classes/tm_timer.hpp>,
    <verbatim|tm_timer.cpp>>Time in milliseconds, <abbr|CPU> time and the
    benchmarking routines.

    <item*|<verbatim|System/Misc/data_cache.hpp>,
    <verbatim|data_cache.cpp>>The caches of directory contents, file
    attributes and file contents.

    <item*|<verbatim|System/Misc/persistent.hpp>,
    <verbatim|persistent.cpp>>A persistent key-value store on disk.

    <item*|<verbatim|System/Misc/sys_utils.hpp>,
    <verbatim|sys_utils.cpp>>Running programs synchronously and
    asynchronously, environment variables, printing command, portable
    <cpp|poll>.

    <item*|<verbatim|System/Misc/server_log.hpp>>Log levels and macros for
    the <TeXmacs> server; implemented in the platform layers.

    <item*|<verbatim|System/Misc/fast_alloc.hpp>,
    <verbatim|fast_alloc.cpp>>Memory allocation (see <hlink|basic data
    types|types.en.tm>).

    <item*|<verbatim|System/Boot/init_texmacs.cpp>,
    <verbatim|init_upgrade.cpp>, <verbatim|preferences.cpp>,
    <verbatim|boot.hpp>>Paths, environment variables, user and temporary
    directories, the boot lock, the settings file, upgrades and the
    <c++> store of user preferences.

    <item*|<verbatim|Plugins/Unix/>>Entry point, file and directory
    primitives, <cpp|system>, logging and stack traces for <name|Linux>,
    <name|macOS> and the other <name|Unix> systems.

    <item*|<verbatim|Plugins/MacOS/>><name|macOS> specific services
    (startup modifiers, remote controls, <name|Cocoa> spell checking and
    image conversion, App Nap).

    <item*|<verbatim|Plugins/Windows/>, <verbatim|Plugins/Windows64/>>The
    32-bit and 64-bit <name|Windows> layers.

    <item*|<verbatim|Plugins/Android/>>The <name|Android> layer.
  </description>

  <section|Contents of this chapter>

  <\traverse>
    <branch|URLs, resolution and concretization|system-urls.en.tm>

    <branch|Files and caches|system-files.en.tm>

    <branch|Paths, directories and settings at boot time|system-boot.en.tm>

    <branch|Programs, web requests, messages and timing|system-utils.en.tm>

    <branch|The platform layers|system-platforms.en.tm>
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
