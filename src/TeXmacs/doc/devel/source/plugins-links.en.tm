<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Links and the event loop>

  <section|The abstract link>

  A link moves bytes between <TeXmacs> and an extern program; it knows
  nothing about trees, formats or sessions. The abstract class is declared in
  <source-link|System/Link/tm_link.hpp|src/System/Link/tm_link.hpp>:

  <\cpp-code>
    struct tm_link_rep: abstract_struct {

    \ \ bool \ \ alive; \ \ // link is alive

    \ \ string secret; \ // empty string or secret key for encrypted connections

    \ \ command feed_cmd; // called when async data available

    \;

    \ \ virtual string \ start () = 0;

    \ \ virtual void \ \ \ write (string s, int channel) = 0;

    \ \ virtual string& watch (int channel) = 0;

    \ \ virtual string \ read (int channel) = 0;

    \ \ virtual void \ \ \ listen (int msecs) = 0;

    \ \ virtual void \ \ \ interrupt () = 0;

    \ \ virtual void \ \ \ stop () = 0;

    \;

    \ \ void write_packet (string s, int channel);

    \ \ bool complete_packet (int channel);

    \ \ string read_packet (int channel, int timeout, bool& success);

    \ \ void secure_server (string cmd);

    \ \ void secure_client ();

    \;

    \ \ void set_command (command _cmd) { feed_cmd = _cmd; }

    \ \ void apply_command () { if (!is_nil (feed_cmd)) feed_cmd-\<gtr\>apply (); }

    };
  </cpp-code>

  The smart pointer <cpp|tm_link> is declared with <cpp|ABSTRACT_NULL>. The
  channels are numbers: <cpp|LINK_IN> and <cpp|LINK_OUT> are both 0 (input
  to the program and its standard output), <cpp|LINK_ERR> is 1 (its standard
  error). The contract of the virtual methods, as relied upon by
  <source-link|connection.cpp|src/System/Link/connection.cpp>, is the following:

  <\explain>
    <cpp|string start ()><explain-synopsis|launch>
  <|explain>
    Start the extern program and set <cpp|alive>. The returned message is
    <verbatim|"ok"> on success. Other values in use are
    <verbatim|"busy"> (already started), messages starting with
    <verbatim|"Error:">, the startup message of a dynamic library, and the
    special values <verbatim|"cmdline"> and <verbatim|"request">, which
    switch the connection to the corresponding evaluation style.
  </explain>

  <\explain>
    <cpp|void write (string s, int channel)><explain-synopsis|send input>
  <|explain>
    Send <cpp|s> to the program. Only <cpp|LINK_IN> is meaningful.
  </explain>

  <\explain>
    <cpp|string& watch (int channel)>

    <cpp|string read (int channel)><explain-synopsis|access buffered output>
  <|explain>
    <cpp|watch> returns a reference to the buffer of pending output of the
    channel without consuming it (it is only used by the packet functions);
    <cpp|read> returns the pending output and empties the buffer. Neither
    method is required to wait for data.
  </explain>

  <\explain>
    <cpp|void listen (int msecs)><explain-synopsis|wait for output>
  <|explain>
    Wait at most <cpp|msecs> milliseconds until output is available on one
    of the channels, and buffer it.
  </explain>

  <\explain>
    <cpp|void interrupt ()>

    <cpp|void stop ()><explain-synopsis|interrupt or terminate>
  <|explain>
    Interrupt the current computation, resp. terminate the program and set
    <cpp|alive> to <cpp|false>.
  </explain>

  When new output has been buffered, the link (or the code which polls it)
  must call <cpp|apply_command>, which runs the command installed by
  <cpp|connection_rep::start> and so calls <cpp|connection_rep::listen>.
  When the program has terminated, the link must set <cpp|alive> to
  <cpp|false>; the connection detects this on its next <cpp|read> and
  reports the session as dead.

  The non-virtual methods <cpp|write_packet>, <cpp|read_packet> and the
  <cpp|secure_*> methods implement the length-prefixed and optionally
  encrypted packets of the client/server protocol; plug-in connections do
  not use them. They are described in <hlink|transport and message
  protocol|collab-protocol.en.tm>.

  <section|Pipes>

  <subsection|<name|POSIX> pipes>

  <cpp|pipe_link_rep> in <source-link|System/Link/pipe_link.cpp|src/System/Link/pipe_link.cpp> holds the
  launch command, the process identifier, three pairs of pipe descriptors,
  the buffers <cpp|outbuf> and <cpp|errbuf>, and two socket notifiers. Its
  <cpp|start> method forks:

  <\cpp-code>
    pid= fork ();

    if (pid==0) { // the child

    \ \ setsid();

    \ \ ... // connect the pipes to STDIN, STDOUT and STDERR with dup2

    \ \ execute_shell (cmd); \ // execve ("/bin/sh", {"sh", "-c", cmd}, environ)

    \ \ exit (127);

    }

    else { // the main process

    \ \ ... // keep in, out and err, close the other ends

    \ \ alive= true;

    \ \ snout = socket_notifier (out, &pipe_callback, this, NULL);

    \ \ snerr = socket_notifier (err, &pipe_callback, this, NULL);

    \ \ add_notifier (snout);

    \ \ add_notifier (snerr);

    \ \ return "ok";

    }
  </cpp-code>

  The launch command is thus interpreted by <verbatim|/bin/sh>, so that it
  may contain redirections, pipes, sequences and environment assignments.
  The child is the leader of a new session and process group
  (<cpp|setsid>), so that signals can be sent to the whole group with
  <cpp|killpg>: <cpp|interrupt> sends <verbatim|SIGINT> to the group, and
  <cpp|stop> sends <verbatim|SIGTERM>, sleeps two seconds, sends
  <verbatim|SIGKILL>, closes the input pipe and calls <cpp|wait>. Since the
  start method always returns <verbatim|"ok">, a launcher which cannot be
  executed is only detected later, when the shell exits with status 127 and
  the output pipe reaches its end. (The code still contains a disabled check
  for a startup character <cpp|TERMCHAR>.)

  The method <cpp|feed> reads at most 1024 bytes from one pipe into the
  corresponding buffer; at end of file it kills the process group, sets
  <cpp|alive> to <cpp|false> and removes the notifiers. The call-back
  <cpp|pipe_callback> is invoked when a notifier fires: it calls
  <cpp|feed> for both pipes, using <cpp|select> with a zero timeout, until no
  more data is immediately available, and then applies <cpp|feed_cmd>. The
  method <cpp|listen> is the blocking counterpart, used by synchronous code.

  All pipe links are registered in the global set <cpp|pipe_link_set>.
  <cpp|process_all_pipes> applies the command of every live pipe link (and
  of every live command line and request link), and <cpp|close_all_pipes>
  kills all processes; the latter is called by
  <cpp|quit_texmacs_internal> in <source-link|Texmacs/Server/tm_server.cpp|src/Texmacs/Server/tm_server.cpp> and
  by the emergency handler in <source-link|Texmacs/Server/tm_debug.cpp|src/Texmacs/Server/tm_debug.cpp>.

  On <name|Windows> (<cpp|OS_MINGW>) without <name|Qt>, all methods of
  <cpp|pipe_link_rep> are empty and <cpp|start> returns
  <verbatim|"Error: pipes not implemented">.

  <subsection|<name|Qt> pipes>

  When <cpp|QTTEXMACS> is defined together with <cpp|OS_MINGW> or
  <cpp|QTPIPES>, <source-link|pipe_link.cpp|src/System/Link/pipe_link.cpp> compiles to nothing and
  <cpp|make_pipe_link> is provided by
  <source-link|Plugins/Qt/qt_pipe_link.cpp|src/Plugins/Qt/qt_pipe_link.cpp> instead. The class
  <cpp|qt_pipe_link_rep> delegates to a <cpp|QTMPipeLink>, a subclass of
  <cpp|QProcess> declared in <source-link|Plugins/Qt/QTMPipeLink.hpp|src/Plugins/Qt/QTMPipeLink.hpp> which
  holds the command and the two buffers. The differences with the
  <name|POSIX> implementation are significant:

  <\itemize>
    <item><em|No shell.> <cpp|QTMPipeLink::launchCmd> splits the launch
    command into a program and its arguments and starts the program
    directly: with <cpp|wordexp> on <name|Unix> systems (which performs
    quoting, variable and tilde expansion, but fails on unquoted shell
    operators such as <verbatim|;>, <verbatim|\|>, <verbatim|&>,
    <verbatim|\<less\>> or <verbatim|\<gtr\>>), with
    <cpp|CommandLineToArgvW> on <name|Windows> and with
    <cpp|QProcess::splitCommand> on <name|Android>. A launcher which needs
    shell syntax must therefore invoke a shell explicitly, as in
    <verbatim|sh -c "cd /some/dir; exec myprog">. If the program cannot be
    started, <cpp|start> returns <verbatim|"Error: cannot start
    application">.

    <item><em|Polling.> The signals <cpp|readyReadStandardOutput> and
    <cpp|readyReadStandardError> are connected to the slot
    <cpp|QTMPipeLink::readErrOut>, which only appends the available data to
    the buffers (<cpp|feedBuf>). The command <cpp|feed_cmd> is not applied
    from the slot; the connection is notified when
    <cpp|tm_server_rep::interpose_handler> calls <cpp|process_all_pipes>,
    which applies the command of every live link, whether it received data
    or not.

    <item><em|Signals.> <cpp|interrupt> sends <verbatim|SIGINT> with
    <cpp|::kill> to the process only (there is no process group); on
    <name|Windows> it is not implemented and only prints an error.
    <cpp|stop> calls <cpp|QTMPipeLink::killProcess (0)>, which calls
    <cpp|terminate> and, if the process has not finished immediately,
    <cpp|kill>.

    <item><em|Reading.> <cpp|qt_pipe_link_rep::read> first calls
    <cpp|listen (0)>, which uses <cpp|waitForReadyRead>; therefore reading
    from a <name|Qt> pipe link also pulls new data from the process, which
    is what makes the synchronous loop of <cpp|connection_retrieve> work
    without <cpp|perform_select>.

    <item><em|Writing.> <cpp|QTMPipeLink::writeStdin> waits until the data
    has been written (<cpp|waitForBytesWritten>); if writing fails, the link
    is stopped.
  </itemize>

  <section|Dynamic libraries>

  A plug-in configured with <scm|(:link <em|lib> <em|symbol> <em|init>)>
  uses a <cpp|dyn_link_rep> (<source-link|System/Link/dyn_link.cpp|src/System/Link/dyn_link.cpp>). The
  interface between <TeXmacs> and the library is declared in
  <source-link|src/TeXmacs/include/TeXmacs.h|TeXmacs/include/TeXmacs.h> and documented from the point of
  view of the library in <hlink|dynamic libraries|../interface/interface-dynlibs.en.tm>
  and <hlink|dynamic linking|../plugin/dynlibs.en.tm>.

  The function <cpp|symbol_install> looks up the library in
  <verbatim|$LD_LIBRARY_PATH> (which includes the <verbatim|lib>
  directories of all plug-ins), opens it with the function given by the
  configuration macro <cpp|TM_DYNAMIC_LINKING> (normally <cpp|dlopen>) and
  resolves the symbol with <cpp|dlsym>. Libraries and symbols are cached in
  the static table <cpp|dyn_linked>, so that a library is opened only once.
  Without <cpp|TM_DYNAMIC_LINKING>, or on <name|Windows>, dynamic linking is
  not available.

  <cpp|dyn_link_rep::start> casts the symbol to a <cpp|package_exports_1>
  structure and calls its <cpp|install> function with the
  <cpp|TeXmacs_exports_1> structure of <TeXmacs> and the initialization
  string. The returned string becomes the first output of the link (the
  banner). <cpp|dyn_link_rep::write> calls the <cpp|evaluate> function of
  the library with the input and the session name and stores the result as
  the pending output; if the result is <cpp|NULL>, the error string is used
  instead. Then it applies <cpp|feed_cmd> immediately: the whole evaluation
  is synchronous, from within <cpp|connection_write>. The strings returned
  by the library must follow the same <verbatim|DATA_BEGIN> ...
  <verbatim|DATA_END> conventions as the output of pipes, since they are
  parsed by the same <cpp|texmacs_input>. The methods <cpp|listen>,
  <cpp|interrupt> and <cpp|stop> do nothing; a dynamically linked
  computation cannot be interrupted, and the library is never unloaded.

  Dynamic links are not registered in any global set and are therefore not
  polled. Their output becomes visible only through the immediate
  <cpp|feed_cmd> call in <cpp|write>, and, for the banner, through the call
  to <cpp|listen> in <cpp|connection_rep::start>, which is only made for a
  plug-in named <verbatim|dynlink> (see <hlink|known
  issues|plugins-issues.en.tm>).

  <section|Command lines and requests>

  <cpp|cmdline_link_rep> (<source-link|System/Link/cmdline_link.cpp|src/System/Link/cmdline_link.cpp>) starts a
  new process for every request. Its <cpp|start> method only clears the
  buffers and returns <verbatim|"cmdline">. Its <cpp|write> method replaces
  newlines in the input by spaces, asks <scheme> for the command line with
  <scm|(connection-cmdline <em|name> "default" <em|input>)>, appends
  <verbatim|2\<gtr\> /dev/null>, and forks <verbatim|/bin/sh -c> like a pipe
  link, with socket notifiers on the output pipes; <cpp|write> is ignored
  while the previous command is still running. An end of file on the
  standard output kills the process group and marks the link as dead,
  which is how the connection learns that the output is complete.
  <cpp|read> returns nothing as long as the process is alive. Live command
  line links are registered in <cpp|cmdline_link_set> and polled by
  <cpp|process_all_cmdlines>, which is called from
  <cpp|process_all_pipes>; in addition, <cpp|connection_rep::listen> calls
  <cpp|listen (1)> on these links, which waits at most one millisecond for
  data.

  <cpp|request_link_rep> (<source-link|System/Link/request_link.cpp|src/System/Link/request_link.cpp>) is
  similar, but the request is a <scheme> expression returned by
  <scm|connection-request>. The only request which is understood is

  <\scm-code>
    (http_post <em|url> (tuple <em|header> ...) <em|json-data>)
  </scm-code>

  which is passed to <cpp|async_http_post_json>
  (<source-link|System/Files/web_files.cpp|src/System/Files/web_files.cpp>). The latter builds a shell command
  (with <cpp|to_shell_command>) and runs it with <cpp|async_eval_system>,
  which starts it with <cpp|popen> and reads its output in a detached
  thread. When the thread has finished, <cpp|async_eval_pending>, called from
  the interpose handler, copies the output into the buffer of the link and
  sets its status field to 0; the next poll of the link (by
  <cpp|process_all_requests>) then marks it as dead. The <cpp|kill> flag
  which <cpp|interrupt> and <cpp|stop> set is only checked by the reading
  thread before it starts reading, so a running request cannot really be
  cancelled; its result is then ignored by the connection.

  <section|Integration with the event loop>

  <subsection|Socket notifiers>

  <source-link|System/Link/socket_notifier.cpp|src/System/Link/socket_notifier.cpp> maintains a set of
  <cpp|socket_notifier> objects, each consisting of a file descriptor and a
  <cpp|command>. <cpp|add_notifier> and <cpp|remove_notifier> manage the
  set, and <cpp|perform_select> repeatedly calls <cpp|select> with a zero
  timeout on all registered descriptors and runs the commands of the ready
  ones, until none is ready. It does not block. On <name|Windows> it is not
  implemented.

  <subsection|Who polls what>

  Which mechanism delivers the output of a link depends on the build:

  <descriptive-table|<tformat|<cwith|1|1|1|-1|cell-font-series|bold>|<table|<row|<cell|Build>|<cell|Interpose
  handler calls>|<cell|Pipes>|<cell|Command lines>|<cell|Requests>>|<row|<cell|<name|Qt>
  with <cpp|QTPIPES> (default)>|<cell|<cpp|process_all_pipes>>|<cell|<cpp|QProcess>
  signals fill the buffers, polled>|<cell|polled, <cpp|listen (1)>>|<cell|polled>>|<row|<cell|<name|Qt>
  without <cpp|QTPIPES>>|<cell|<cpp|perform_select>, <cpp|process_all_pipes>>|<cell|socket
  notifiers, also polled>|<cell|notifiers, polled>|<cell|polled>>|<row|<cell|Other
  ports (<name|X11>)>|<cell|<cpp|perform_select>>|<cell|socket
  notifiers>|<cell|socket notifiers>|<cell|not polled>>>>>

  In all cases <cpp|async_eval_pending> is called by the interpose handler
  as well. The important point for implementers is that socket notifiers
  registered with <cpp|add_notifier> are <em|not> serviced in the default
  <name|Qt> build, since <cpp|perform_select> is only called when
  <cpp|QTPIPES> is undefined. A link which wants to be served in all builds
  should register itself in a set which is polled from
  <cpp|process_all_pipes>, as the command line and request links do.

  <section|Sockets>

  The header <source-link|tm_link.hpp|src/System/Link/tm_link.hpp> declares the functions
  <cpp|make_socket_link>, <cpp|make_socket_server>,
  <cpp|find_socket_link>, <cpp|close_all_sockets> and
  <cpp|close_all_servers>, but none of them is defined anywhere in the
  sources: they are left over from an older implementation. Likewise, the
  options <scm|(:socket <em|host> <em|port>)> and <scm|(:socket
  <em|variant> <em|host> <em|port>)> of <scm|plugin-configure> produce a
  launcher description <scm|(tuple "socket" <em|host> <em|port>)>, but
  <cpp|connection_start> has no branch for it. Plug-ins can therefore not
  communicate through sockets at present.

  The client/server code of the collaboration tools does have a working
  socket link: the class <cpp|socket_link_rep> in
  <source-link|Plugins/Qt/QTMSockets.hpp|src/Plugins/Qt/QTMSockets.hpp>, which derives from both
  <cpp|QObject> and <cpp|tm_link_rep>, uses <cpp|QSocketNotifier> objects and
  applies <cpp|feed_cmd> when data arrives. It is only available in
  <name|Qt> builds and is used through the packet interface (see
  <hlink|transport and message protocol|collab-protocol.en.tm>).

  <section|Implementing a new kind of link>

  The following steps are needed to add a new transport, for instance the
  missing socket connections.

  <\enumerate>
    <item><em|The link class.> Derive a class from <cpp|tm_link_rep> and
    implement the seven virtual methods according to the contract above. In
    particular, <cpp|start> must set <cpp|alive> and return
    <verbatim|"ok">, <cpp|read> must return and clear the buffered data
    without blocking, and the link must set <cpp|alive> to <cpp|false> when
    the peer disappears. Provide a factory function, declared in
    <source-link|tm_link.hpp|src/System/Link/tm_link.hpp>.

    <item><em|Delivery of the output.> Make sure that
    <cpp|apply_command> is called when data has been buffered. The simplest
    portable solution is to keep the live links in a set and to apply their
    commands from <cpp|process_all_pipes> (as <cpp|process_all_cmdlines>
    does); calling <cpp|apply_command> when nothing new arrived is harmless.
    A socket notifier alone is not enough in the default <name|Qt> build.
    Whatever the mechanism, do not call <cpp|apply_command> while
    <cpp|connection_retrieve> is running for the same connection unless
    <cpp|read> also pulls data, or synchronous evaluation will not
    terminate.

    <item><em|Termination.> Add the links to <cpp|close_all_pipes> (or to a
    function called from <cpp|quit_texmacs_internal>) so that they are
    closed when <TeXmacs> exits.

    <item><em|The connection.> Add a branch to <cpp|connection_start> in
    <source-link|System/Link/connection.cpp|src/System/Link/connection.cpp> which recognizes the launcher
    description and creates the link.

    <item><em|The configuration.> Add a clause to
    <scm|plugin-configure-cmd> in <source-link|kernel/texmacs/tm-plugins.scm|TeXmacs/progs/kernel/texmacs/tm-plugins.scm>
    which turns the new option into a launcher description with
    <scm|connection-setup>, and document the option in <hlink|the summary
    of configuration options|../plugin/plugin-config.en.tm>. If the option
    takes a variant, pass it as the optional third argument of
    <scm|connection-setup>, as is done for <scm|:launch>.

    <item><em|Remote use.> If the new link should be usable for remote
    plug-ins, extend <scm|write-local-plugin-info> and the code which reads
    <verbatim|remote-plugins.scm>; currently only pipe launchers are
    exported.
  </enumerate>

  For the socket case, the description is already produced by
  <scm|plugin-configure>, and the dispatch could look as follows (this is a
  sketch, not existing code; <cpp|make_plugin_socket_link> is a
  hypothetical factory):

  <\cpp-code>
    else if (is_tuple (t, "socket", 2)) {

    \ \ tm_link ln= make_plugin_socket_link (t[1]-\<gtr\>label, as_int (t[2]-\<gtr\>label));

    \ \ con= tm_new\<less\>connection_rep\<gtr\> (name, session, ln);

    }
  </cpp-code>

  The link would connect to the given host and port in <cpp|start>,
  configure the socket as non-blocking, append the received bytes to its
  output buffer in a polled <cpp|feed> method, and treat a closed socket as
  the death of the program. Since a socket has no separate error stream,
  <cpp|read (LINK_ERR)> would always return the empty string; a server
  which wants to report errors should use the <verbatim|error#> channel
  inside its <verbatim|DATA_BEGIN> ... <verbatim|DATA_END> blocks, keeping
  in mind that the <verbatim|error> channel of the standard output is only
  delivered to a handler declared with <scm|:handler> (see <hlink|output
  channels|../plugin/plugin-internals.en.tm>). Interrupts cannot be sent as
  signals either, so they would have to be part of the protocol, for
  instance as a special command prefixed with <verbatim|DATA_COMMAND>.

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
