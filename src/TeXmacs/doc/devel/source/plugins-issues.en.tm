<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Debugging, known issues and limitations>

  <section|Debugging the machinery>

  The basic techniques for debugging a plug-in (running the helper program
  by hand, checking the detection, using the standard error) are described
  in <hlink|debugging plug-ins|../plugin/plugin-internals.en.tm>. This
  section lists the additional tools which are useful when the problem is in
  the connection machinery itself.

  <subsection|Debugging flags>

  The flags are set on the command line (<verbatim|-debug-io>), in the
  <menu|Debug> menu (available after enabling <menu|Tools|Debugging
  tool>), or from <scheme> with <scm|(debug-set "io" #t)>; they correspond to the <c++> macros <cpp|DEBUG_IO>,
  <cpp|DEBUG_AUTO> and <cpp|DEBUG_VERBOSE> of
  <verbatim|Kernel/Abstractions/basic.hpp>.

  <\description>
    <item*|<verbatim|io>>The <name|POSIX> pipe link prints every chunk it
    receives and, preceded by <verbatim|[INPUT]>, every string it sends, with
    the control characters shown as <verbatim|[BEGIN]>, <verbatim|[END]>,
    <verbatim|[ESCAPE]>, <verbatim|[COMMAND]> and <verbatim|[ABORT]>
    (<cpp|debug_io_string>). The <name|Qt> pipe link does the same, but
    prefixes each received chunk with <verbatim|[OUTPUT <em|n>]>, where
    <em|n> is the <cpp|QProcess> channel (0 for the standard output, 1 for
    the standard error). After each complete output (the closing
    <verbatim|DATA_END> of the outermost block), <cpp|connection_rep::read>
    prints a horizontal rule. Command line links print the command they
    launch, request links the request, and the output parser reports output
    which it ignored because of a <verbatim|DATA_ABORT>.

    <item*|<verbatim|auto>>Pipe links print the launch command, and dynamic
    links the result of <cpp|symbol_install>.

    <item*|<verbatim|verbose>><cpp|connection_start> prints
    <verbatim|Starting session '<em|ses>'> whenever it creates a connection.
  </description>

  <subsection|Inspecting the state from <scheme>>

  In a <scheme> session, the state of a connection can be inspected with the
  glue functions and the exported functions of the request queue, for
  instance:

  <\scm-code>
    (connection-info "maxima" "default")    ; launcher description

    (connection-status "maxima" "default")  ; 0, 2 or 3

    (length (pending-ref "maxima" "default")) ; queued requests

    (plugin-prompt "maxima" "default")      ; last prompt
  </scm-code>

  A session which is stuck (status 3 with a non-empty queue) can be reset
  with <menu|Interrupt execution> (which empties the queue even if the
  program does not react) followed by <menu|Close session>. The
  <scm|display*> calls which are commented out at the beginning of the
  call-backs in <verbatim|plugin-eval.scm> and
  <verbatim|session-edit.scm> are a convenient way to trace the scheduler.

  The protocol can also be exercised without any program: a launcher such as

  <\shell-code>
    printf '\\002verbatim:hello\\n\\002prompt#\<gtr\> \\005\\005'; cat \<gtr\> /dev/null
  </shell-code>

  prints a banner and a prompt and then waits forever. With <name|Qt> pipes,
  this has to be wrapped as <verbatim|sh -c "...">, see below.

  <subsection|Typical symptoms>

  <\description>
    <item*|The session stays busy forever>The outermost block was not
    closed, or the program did not flush its standard output after the final
    <verbatim|DATA_END>. If this already happens right after the creation
    of the session, the banner is the culprit: as long as the start request
    is not complete, all evaluations stay in the queue. Another cause is a
    launch failure with <name|Qt> pipes (see below).

    <item*|Outputs are shifted by one request>The program closes more than
    one outermost block per request, for instance by sending the prompt as a
    separate block after the output instead of nesting it inside. If the
    second <verbatim|DATA_END> arrives after the next request was sent, it
    terminates that request prematurely.

    <item*|Output appears in bursts>The program writes to a pipe and
    therefore uses block buffering; it must flush its output. This is
    invisible when the program is tested in a terminal.

    <item*|Output is lost after an interrupt>See the race described below.

    <item*|A custom channel never shows up>Channels other than
    <verbatim|output>, <verbatim|error>, <verbatim|prompt> and
    <verbatim|input> are only delivered to handlers declared with
    <scm|:handler>, and the table of handlers is cached on first use.
  </description>

  <section|Known issues>

  The following problems are visible in the current sources. The first three
  are already known upstream.

  <subsection|Socket connections are not implemented>

  The <scm|:socket> option produces a launcher description <scm|(tuple
  "socket" <em|host> <em|port>)>, but <cpp|connection_start> has no branch
  for it, and the socket functions declared in <verbatim|tm_link.hpp> are
  not defined (see <hlink|sockets|plugins-links.en.tm>). Worse, when
  <cpp|connection_start> is called for the first time with a description
  which matches none of its branches, the variable <cpp|con> is still the
  nil connection when the code reaches <cpp|con-\<gtr\>info= t>, which
  dereferences a null pointer.

  <subsection|Startup of dynamic links is keyed by the plug-in name>

  <cpp|connection_rep::start> only reads the startup message of a link
  immediately if the <em|plug-in> is called <verbatim|dynlink>, the name of
  the example plug-in in <verbatim|src/TeXmacs/examples/plugins/dynlink>:

  <\cpp-code>
    if (name == "dynlink") {

    \ \ this-\<gtr\>listen ();

    \ \ status = WAITING_FOR_OUTPUT;

    }
  </cpp-code>

  For any other plug-in with a <scm|:link> option, dynamic links being
  neither polled nor connected to notifiers, the startup message is never
  read, the connection never reports that it is waiting for input, and the
  start request of the session is never completed. The test should be on the
  type of the link (the launcher description <scm|(tuple "dynlink" ...)>)
  rather than on the name of the plug-in.

  <subsection|The <scm|:prioritary> option has no effect>

  The value of <scm|:prioritary> is stored in the plug-in cache, but it is
  read before the cache is loaded; see <hlink|known
  limitations|../plugin/plugin-internals.en.tm> in the description of the
  plug-in internals.

  <subsection|Further problems>

  <\itemize>
    <item><em|Launch failures are not reported.> <scm|plugin-start> ignores
    the message returned by <scm|connection-start>. With the <name|POSIX>
    pipe link this is harmless, since the failure is detected later as the
    death of the shell. With <name|Qt> pipes, however, <cpp|start> fails
    immediately (for instance for a launcher containing shell operators,
    which <cpp|wordexp> rejects), no status change is ever notified, and the
    session stays busy without any message until the user interrupts it.

    <item><em|Interrupts are not synchronized with the program.>
    <scm|plugin-interrupt> cancels the queued requests at once, but the
    connection keeps reporting the status 3 until the program closes the
    interrupted output. A request which is evaluated in the meantime is
    written to the program immediately and receives the remaining output of
    the interrupted computation, and its own output is attributed to the
    request after it.

    <item><em|Several sessions with the same dynamic library.> When a
    library was already installed by another connection,
    <cpp|dyn_link_rep::start> returns a <verbatim|continuation of#...>
    message without setting <cpp|alive>, so the link of the second session
    is never alive and its writes are ignored.

    <item><em|Status of dynamic links.> <cpp|dyn_link_rep::write> notifies
    the connection synchronously, before <cpp|connection_rep::write> sets
    the status to <cpp|WAITING_FOR_OUTPUT>. The status therefore stays at 3
    between evaluations. Sessions are not affected, but
    <cpp|connection_eval> on a dynamic link waits for a status change which
    never comes.

    <item><em|Synchronous evaluation in <name|Qt> builds without
    <cpp|QTPIPES>.> <cpp|connection_retrieve> only calls
    <cpp|perform_select> when <cpp|QTTEXMACS> is undefined, while
    <cpp|pipe_link_rep::read> does not pull data from the pipe. In this
    configuration <scm|plugin-eval> on a pipe can therefore not see new
    output.

    <item><em|Stopping blocks the interface.> <cpp|pipe_link_rep::stop> and
    <cpp|close_all_pipes> sleep for two seconds between <verbatim|SIGTERM>
    and <verbatim|SIGKILL>, even when the program terminated at once.
    Moreover, neither <cpp|stop> nor the end-of-file branch of <cpp|feed>
    close the descriptors of the output and error pipes, and <cpp|wait
    (NULL)> may reap an unrelated child process.

    <item><em|Binary output with <name|Qt> pipes.>
    <cpp|QTMPipeLink::feedBuf> converts the received <cpp|QByteArray> with
    <cpp|constData ()>, that is, as a null-terminated string: everything
    after a null byte in a chunk is lost.

    <item><em|Cancelled requests.> The thread which performs a request of a
    <cpp|request_link_rep> cannot be stopped and writes its result into the
    buffers of the link when it finishes, even if the request was cancelled
    and another one has started in the meantime.

    <item><em|Simplification of output.> In
    <scm|plugin-output-std-simplify> (<verbatim|plugin-eval.scm>), the test
    <scm|(func? 'concat 0)> lacks its first argument <scm|t>, so that an
    empty <markup|concat> is not simplified to the empty string.

    <item><em|Dead declarations.> <cpp|connection_stop_all> (in
    <verbatim|connect.hpp>) and <cpp|texmacs_input_rep::ispell_flush> are
    declared but not defined; <cpp|cmdline_link_rep::write> tests whether the
    command is empty only after having appended
    <verbatim|2\<gtr\> /dev/null> to it.
  </itemize>

  <section|Design limitations>

  <\itemize>
    <item>All communication happens in the main thread, by polling; see
    <hlink|threading and the event loop|plugins.en.tm>. There is no timeout
    for synchronous evaluations.

    <item>Only one request per connection is in progress at any time; the
    protocol has no request identifiers, so the end of an output is the
    only synchronization point.

    <item>The relative order of standard output and standard error within
    one polling interval is lost.

    <item>Input is sent as a single string, and there is no flow control:
    <cpp|pipe_link_rep::write> performs a single <cpp|write> system call and
    ignores its result.

    <item>A program cannot be interrupted on <name|Windows>, and a dynamic
    library cannot be interrupted at all.

    <item>The session name is not passed to the functions of
    <scm|:cmdline> and <scm|:request> plug-ins.
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
