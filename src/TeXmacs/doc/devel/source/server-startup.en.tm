<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The main program and crash handling>

  This page describes <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>, which
  contains the main program, and <source-link|Texmacs/Server/tm_debug.cpp|src/Texmacs/Server/tm_debug.cpp>,
  which handles fatal errors. The platform specific <cpp|main> functions
  (<source-link|Plugins/Unix/unix_entrypoint.cpp|src/Plugins/Unix/unix_entrypoint.cpp>,
  <source-link|Plugins/Windows/windows32_entrypoint.cpp|src/Plugins/Windows/windows32_entrypoint.cpp>,
  <source-link|Plugins/Windows64/windows64_entrypoint.cpp|src/Plugins/Windows64/windows64_entrypoint.cpp> and, for
  <name|Android>, <source-link|src/packages/android/launcher/main.cpp|packages/android/launcher/main.cpp>)
  prepare the environment and the arguments (the <TeXmacs> path, the
  <name|AppImage> search path and the <name|Qt> platform on <name|Unix>,
  conversion of the arguments to <name|UTF-8> on <name|Windows>) and then
  call <cpp|texmacs_entrypoint>.

  <section|The startup sequence>

  Startup happens in two stages. The first stage,
  <cpp|texmacs_entrypoint (argc, argv)>, runs before the <scheme>
  interpreter exists:

  <\enumerate>
    <item><cpp|immediate_options> sets <verbatim|TEXMACS_HOME_PATH> if
    it is not set (<verbatim|~/.TeXmacs>, or the platform equivalent) and
    handles the options which must act before anything is loaded: the
    cache and setup removal options, <verbatim|-headless>,
    <verbatim|-open> and <verbatim|-log-file> (see below). It also sets
    <verbatim|TEXMACS_SERVER_CERT_DIR>.

    <item>Resource limits are raised (the stack size if
    <verbatim|STACK_SIZE> is defined; on <name|macOS>, the number of open
    files in <cpp|boot_hacks>), automatic widget refreshes are disabled
    until further notice (<cpp|windows_delayed_refresh>), the paths are
    initialized (<cpp|TeXmacs_init_paths>, which also fixes the
    environment of application bundles on <name|macOS>, <name|Windows> and
    <name|Haiku>) and the user preferences are loaded
    (<cpp|load_user_preferences>).

    <item>The <name|Qt> application object is created: a
    <cpp|QTMApplication> normally, a <cpp|QTMCoreApplication> without any
    GUI in headless mode. The <verbatim|gui scaling> preference is applied
    first, through <verbatim|QT_SCALE_FACTOR>.

    <item>In <verbatim|-open> mode (not on <name|macOS>; the option also
    sets headless mode), the file is sent to an already running instance
    if there is one (<cpp|send_to_single_instance>, <name|Qt> only),
    otherwise a new <TeXmacs> is started on it with <cpp|execl>; in both
    cases the current process exits.

    <item>The fonts are initialized, the global edit tree is created with
    its root <cpp|ip_observer>, the caches are initialized
    (<cpp|cache_initialize>), and <cpp|init_texmacs>
    (<source-link|System/Boot/init_texmacs.cpp|src/System/Boot/init_texmacs.cpp>) performs the remaining
    system initialization: the main paths, the user directories, the boot
    lock (if the lock file of a previous run is still there, that run
    crashed during startup, and the settings and caches are reset; the
    lock file itself stays until <cpp|release_boot_lock>), the succession
    status table, the standard <abbr|DRD>, the preferences again, the
    paths of the <scheme> interpreter, the environment variables used by
    plug-ins and some miscellaneous and deprecated settings.

    <item><cpp|start_scheme (argc, argv, TeXmacs_main)> calls
    <cpp|TeXmacs_main>. With <name|Guile> it does so through
    <cpp|scm_boot_guile> (or <cpp|gh_enter>), so that <cpp|TeXmacs_main>
    runs inside the <name|Guile> runtime; with <name|TinyScheme> it just
    calls it. The interpreter state itself (bootstrap code, types and
    glue) is set up later, by <cpp|initialize_scheme> in the constructor
    of the server.
  </enumerate>

  The second stage, <cpp|TeXmacs_main (argc, argv)>, runs inside the
  interpreter:

  <\enumerate>
    <item><cpp|set_global_options> parses the remaining command line
    options and reads a few preferences which must be known before the
    first window is built (native menu bar, mini bars).

    <item><cpp|init_plugins> installs the plug-ins, <cpp|gui_open> opens
    the display and the default font is set.

    <item>The server is constructed (<cpp|server sv>), which boots the
    <scheme> side; see <hlink|the server classes|server-classes.en.tm>.

    <item>On the first run after an installation or an upgrade
    (<cpp|install_status>), a command which loads the welcome message or
    the list of recent changes is added.

    <item>If no buffer has been opened yet, an empty window is opened
    (<cpp|open_window>). The comment in the code notes that this test is
    always true at this point, since files given on the command line are
    only loaded later.

    <item>A delayed command which gives the keyboard focus to the canvas
    is added, <cpp|texmacs_started> is set, the signal handlers are
    installed (segmentation faults go to
    the crash handler unless <verbatim|-disable-error-recovery> was given;
    <verbatim|SIGTERM> exits at once; <verbatim|SIGPIPE> is ignored, so
    that a client closing a socket does not kill the process), the
    <TeXmacs> server for remote clients is started if requested
    (<verbatim|-server>, and only if <cpp|server_can_start ()>), and the
    boot lock is released.

    <item>The collected startup commands are scheduled with
    <cpp|exec_delayed> and the GUI event loop is entered
    (<cpp|gui_start_loop>). Normally the program leaves the loop only
    through <cpp|quit>, which exits the process directly.
  </enumerate>

  <section|Startup commands>

  Most command line options do not act directly but produce <scheme>
  commands, collected in two strings:

  <\description>
    <item*|<cpp|my_init_cmds>>Filled by <verbatim|-x>, <verbatim|-q>,
    <verbatim|-c>/<verbatim|-C>, <verbatim|-W> and <verbatim|-U>; in
    headless mode without <verbatim|-server>, <scm|(quit-TeXmacs)> is
    appended (unless the option <verbatim|-X> was given, which sets
    <cpp|exec_exit> to false). These commands are wrapped in a single
    <scm|begin> and scheduled by the constructor of <cpp|tm_server_rep>,
    right after <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> and
    <verbatim|my-init-texmacs.scm> have been loaded.

    <item*|<cpp|extra_init_cmd>>Filled with a <scm|load-buffer> for each
    file named on the command line (the first one in the current window,
    the next ones with <scm|:new-window>), the welcome or upgrade message,
    <verbatim|-build-manual>, <verbatim|-reference-suite> and
    <verbatim|-test-suite>. These commands are scheduled by
    <cpp|TeXmacs_main> just before entering the event loop.
  </description>

  The delayed commands are executed by the event loop in the order in
  which they were scheduled (<cpp|exec_pending> in
  <source-link|Plugins/Qt/qt_gui.cpp|src/Plugins/Qt/qt_gui.cpp> processes a FIFO queue). This has two
  consequences:

  <\itemize>
    <item>The commands of <verbatim|-x> run <em|before> the files given
    on the command line are loaded.

    <item><scm|quit-TeXmacs> exits the process at once. So a
    <verbatim|-q>, or the <scm|(quit-TeXmacs)> appended in headless mode,
    prevents everything scheduled after it from running: the
    <verbatim|-x> and <verbatim|-c> options which come later on the
    command line, and all of <cpp|extra_init_cmd> (the files to load,
    <verbatim|-build-manual>, <verbatim|-reference-suite>,
    <verbatim|-test-suite>). In headless mode these need <verbatim|-X>.
  </itemize>

  <section|Command line options>

  The options are recognized with one or two leading dashes. The main
  ones are:

  <\description>
    <item*|Information><verbatim|-h> (help, also any unknown option),
    <verbatim|-v> (version), <verbatim|-p>, <verbatim|-hp>,
    <verbatim|-bp> (print the <TeXmacs> path, home path and binary path).

    <item*|Messages and debugging><verbatim|-s> (silent),
    <verbatim|-V> (verbose), <verbatim|-d> (debug), the
    <verbatim|-debug-<em|kind>> family (<verbatim|events>,
    <verbatim|io>, <verbatim|sockets>, <verbatim|gnutls>,
    <verbatim|bench>, <verbatim|history>, <verbatim|qt>,
    <verbatim|qt-widgets>, <verbatim|keyboard>, <verbatim|packrat>,
    <verbatim|flatten>, <verbatim|parser>, <verbatim|correct>,
    <verbatim|convert>, <verbatim|remote>, <verbatim|live>, and
    <verbatim|all>, which only turns on events, std, io, history, bench,
    qt and qt-widgets),
    <verbatim|-disable-error-recovery>, <verbatim|-log-file <em|file>>.

    <item*|Initialization><verbatim|-i <em|file>> (replaces
    <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>), <verbatim|-b <em|file>> (replaces
    <source-link|init-buffer.scm|TeXmacs/progs/init-buffer.scm>), <verbatim|-x <em|cmd>> (execute a
    <scheme> command), <verbatim|-q> (append <scm|(quit-TeXmacs)> at this point of
    the command line),
    <verbatim|-X> (do not quit automatically in headless mode).

    <item*|Display><verbatim|-g <em|w>x<em|h>+<em|x>+<em|y>> (geometry of
    the first window), <verbatim|-fn <em|font>> (default font),
    <verbatim|-r> (reverse video), <verbatim|-Oc>/<verbatim|+Oc>
    (clipping of <TeX> bitmap characters) and, for <name|Qt> 5 and
    earlier, the <verbatim|-retina> options.

    <item*|Batch processing><verbatim|-headless> or <verbatim|-H> (no
    GUI), <verbatim|-c> or <verbatim|-C <em|in> <em|out>> (convert a
    file, through <scm|load-buffer> and <scm|export-buffer>),
    <verbatim|-W> and <verbatim|-U <em|in> <em|out>> (build or update a
    web site), <verbatim|-build-manual>, <verbatim|-reference-suite>,
    <verbatim|-test-suite>. In the <name|Qt> version,
    <verbatim|-C>, <verbatim|-W>/<verbatim|-build-website> and
    <verbatim|-U>/<verbatim|-update-website> imply headless mode (but not
    the lower case <verbatim|-c>).

    <item*|Maintenance><verbatim|-S> or <verbatim|-setup> (forget the
    settings and caches), <verbatim|-delete-cache> and the more specific
    <verbatim|-delete-<em|kind>-cache> options, <verbatim|-delete-server-data>,
    <verbatim|-delete-databases>.

    <item*|Server and instances><verbatim|-server>, <verbatim|-port
    <em|n>>, <verbatim|-reset-server-preferences>,
    <verbatim|-reset-admin-password>, <verbatim|-tls-no-verify> (see
    <hlink|collaboration|collaboration.en.tm>), and <verbatim|-open
    <em|file>> (open in a running instance).
  </description>

  Several of these options are seen twice: by <cpp|immediate_options>
  and by <cpp|set_global_options>. For most of them (the cache options,
  <verbatim|-headless>, <verbatim|-log-file>) the first does the work and
  the second skips them; for <verbatim|-C>, <verbatim|-W> and
  <verbatim|-U> the first sets headless mode and the second builds the
  command. On <name|macOS>, <cpp|set_global_options> skips
  <verbatim|-open> but not its argument, which is then loaded as an
  ordinary file. A new option which
  must act before the preferences or the GUI exist belongs in
  <cpp|immediate_options>; all others belong in <cpp|set_global_options>
  and should, when possible, produce a <scheme> command rather than act
  directly.

  <section|Global flags>

  <source-link|texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp> defines a few global flags: <cpp|headless_mode>
  (read through <cpp|is_headless ()> or directly as an <cpp|extern>, for
  instance by the interpose handler, which skips all screen updates in
  this mode), <cpp|tls_no_verify> (<cpp|is_tls_no_verify ()>) and
  <cpp|disable_error_recovery> (used only in <source-link|texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp>).
  <source-link|tm_server.cpp|src/Texmacs/Server/tm_server.cpp> defines <cpp|texmacs_started>, which is set
  just before the event loop starts and is used by the wait handler to
  decide whether a window can be used.

  <section|Fatal errors and crash handling>

  Internal errors are signalled with the macros <cpp|ASSERT (cond, msg)>
  and <cpp|FAILED (msg)> of <source-link|Kernel/Abstractions/basic.hpp|src/Kernel/Abstractions/basic.hpp>. What
  they do depends on <verbatim|USE_EXCEPTIONS>, which that header
  currently always defines.

  <paragraph|With exceptions (the default).>Both macros call
  <cpp|tm_throw (msg)> (<source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp>). It
  stores the message in <cpp|the_exception>, builds a crash report with
  <cpp|get_crash_report> (see below), prints it on the console and throws
  the message as a <cpp|string>. The exception is caught at the
  following places:

  <\itemize>
    <item>key presses (<cpp|edit_interface_rep::handle_keypress>, which
    cancels the current edit and the pending shortcut) and mouse events
    (<cpp|handle_mouse>, which cancels the current edit;
    <cpp|update_mouse_loci> ignores the exception);

    <item>menu and toolbar actions (<cpp|protected_call> in
    <source-link|Scheme/Scheme/object.cpp|src/Scheme/Scheme/object.cpp>, which calls
    <cpp|cancel_menu_action> on the current editor);

    <item>typesetting (<source-link|edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp>), which does not
    cancel anything: it reports \PTypesetting failure, resetting to empty
    document\Q, <em|replaces the body of the buffer by an empty document>
    and typesets again;

    <item>as a last resort, every <name|Qt> event:
    <cpp|QTMApplication::notify> and <cpp|QTMCoreApplication::notify>
    catch the exception; a few slots (in <source-link|QTMMenuHelper.cpp|src/Plugins/Qt/QTMMenuHelper.cpp>,
    <source-link|QTMGuiHelper.cpp|src/Plugins/Qt/QTMGuiHelper.cpp>, <source-link|QTMFileDialog.cpp|src/Plugins/Qt/QTMFileDialog.cpp> and
    <source-link|QTMPipeLink.cpp|src/Plugins/Qt/QTMPipeLink.cpp>) are also wrapped in the macros
    <cpp|BEGIN_SLOT> and <cpp|END_SLOT> of
    <source-link|Plugins/Qt/qt_gui.hpp|src/Plugins/Qt/qt_gui.hpp>. (The same code exists in
    <source-link|Plugins/Qt6/|src/Plugins/Qt6>.)
  </itemize>

  The editor and <cpp|protected_call> sites call <cpp|handle_exceptions
  ()> right after catching; it prints the message and the report as an
  error (and so in the error console of <TeXmacs>) and clears
  <cpp|the_exception>. The <name|Qt> sites only store the message in
  <cpp|the_exception>, which is reported by the next call of
  <cpp|handle_exceptions>. In all cases the program goes on with the next
  event and nothing is saved.

  <paragraph|Without exceptions.>If <verbatim|USE_EXCEPTIONS> is not
  defined, the macros call <cpp|tm_failure (msg)>
  (<source-link|Texmacs/Server/tm_debug.cpp|src/Texmacs/Server/tm_debug.cpp>), followed by <cpp|assert> when
  <verbatim|DEBUG_ASSERT> is defined (which the <name|CMake> build does).
  <cpp|tm_failure> tries to save as much as possible:

  <\enumerate>
    <item>It sets the global <cpp|rescue_mode> (tested with
    <cpp|in_rescue_mode ()>). A second fatal error while in rescue mode
    exits at once (or returns, with <verbatim|DEBUG_ASSERT>).

    <item>It writes the crash report to
    <verbatim|$TEXMACS_HOME_PATH/system/crash/crash_report_<em|n>>, or
    prints it if this fails.

    <item>It writes the tree of the current buffer, with the path of each
    node, to a file with the same name followed by <verbatim|_tree>
    (<cpp|tree_report>).

    <item>It autosaves all buffers (<scm|autosave-all>), closes all
    plug-in pipes, runs the <scheme> exit hook and clears the pending
    commands.
  </enumerate>

  <cpp|tm_failure> does not exit by itself; it returns to the
  <cpp|assert> (which aborts) or to the code which raised the error. Note
  that it calls <cpp|get_server> and <cpp|get_current_editor> without
  checking that they exist, so an error before the first view exists
  fails a second time; <cpp|rescue_mode> stops the recursion.

  <paragraph|The crash report.><cpp|get_crash_report (msg)> concatenates
  the message, the system information (<cpp|get_system_information>:
  version, build user and date, host, current date), the editor status
  (<cpp|get_editor_status_report>: root path, cursor path, shifted path
  and selection of the current editor) and a stack trace.
  <cpp|get_editor_status_report> guards against being called before the
  server or the first view exist, since an error at that stage would
  otherwise cause a second failure while the report is built.

  <paragraph|Signals.>A segmentation fault is caught by
  <cpp|clean_exit_on_segfault> (unless <verbatim|-disable-error-recovery>
  was given), which calls <cpp|FAILED ("segmentation fault")>. With
  exceptions, this throws from inside a signal handler, which is not
  guaranteed to work and should not be relied on: after a segmentation
  fault the state of the program is unknown anyway. <verbatim|SIGTERM>
  exits immediately without saving.

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
