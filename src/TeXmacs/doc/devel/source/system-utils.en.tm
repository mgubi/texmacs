<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Programs, web requests, messages and timing>

  This page describes the utilities of <source-link|System/Misc/sys_utils.hpp|src/System/Misc/sys_utils.hpp>,
  the <abbr|HTTP> requests of <source-link|System/Files/web_files.hpp|src/System/Files/web_files.hpp>, the
  output streams of <source-link|System/Files/tm_ostream.hpp|src/System/Files/tm_ostream.hpp>, the timer of
  <source-link|System/Classes/tm_timer.hpp|src/System/Classes/tm_timer.hpp> and the server log of
  <source-link|System/Misc/server_log.hpp|src/System/Misc/server_log.hpp>. Pipes and sockets to plug-ins are
  described in <hlink|the plug-in machinery|plugin-machinery.en.tm>.

  <section|Running external programs>

  <\description-paragraphs>
    <item*|<cpp|system (cmd)>>Runs a shell command and returns its exit
    status. On <name|Unix> the output is discarded (the command is run as
    <verbatim|<em|cmd> \<gtr\> /dev/null 2\<gtr\>&1>); with the option
    <verbatim|-verbose> the output is captured and printed on the
    <verbatim|debug-shell> channel. The variants <cpp|system (cmd, u1,
    ...)> of <source-link|file.hpp|src/System/Files/file.hpp> append concretized and shell quoted file
    names.

    <item*|<cpp|system (cmd, out)>, <cpp|system (cmd, out, err)>>The same,
    but the output is redirected to temporary files, which are then read
    into <cpp|out> (and <cpp|err>). In the two-argument form, standard
    error goes to <cpp|out> as well.

    <item*|<cpp|eval_system (cmd)>, <cpp|var_eval_system (cmd)>>The
    output of the command as a string; the second one removes trailing
    newlines.

    <item*|<cpp|evaluate_system (args, fd_in, in, fd_out)>>Runs a program
    without a shell (<cpp|posix_spawnp> on <name|Unix>), feeding the
    strings <cpp|in> to the file descriptors <cpp|fd_in> and collecting
    the outputs of <cpp|fd_out>; the result is the exit status followed by
    the outputs. Not available on <name|Android>.

    <item*|<cpp|async_eval_system (cmd, call_back)>>Starts a command with
    <cpp|popen> and reads its standard output in a detached thread.
    <cpp|async_eval_pending ()>, called by the interpose handler of the
    server, delivers finished results by calling the <scheme>
    <cpp|call_back> with the output. A second variant stores the result in
    variables given by reference instead.

    <item*|<cpp|tm_poll (fds, n, timeout)>>A portable <cpp|poll>
    (<cpp|select> on <name|Windows>), for at most 64 descriptors.

    <item*|<cpp|get_env (var)>, <cpp|set_env (var, val)>>Environment
    variables, through the platform layer. <cpp|get_env ("PWD")> falls back
    to <verbatim|$HOME> if <verbatim|PWD> is not set.

    <item*|Printing><cpp|get_printing_cmd> and <cpp|set_printing_cmd>
    hold the print command (by default <verbatim|lp>, or the bundled
    <name|SumatraPDF> on <name|Windows>).

    <item*|<cpp|script_status>>Whether scripts in insecure documents are
    never accepted, accepted after a prompt, or always accepted (0, 1, 2),
    set from the security preference.

    <item*|<cpp|get_stacktrace ()>>A printable stack trace, used in crash
    reports (<source-link|unix_stacktrace.cpp|src/Plugins/Unix/unix_stacktrace.cpp> and its <name|Windows>
    and <name|Android> counterparts).
  </description-paragraphs>

  <section|Web requests>

  <cpp|http_post>, <cpp|http_post_json> and <cpp|http_post_query> send an
  <abbr|HTTP> <verbatim|POST> request with headers and a body (a string, a
  tree converted to <abbr|JSON>, or form fields) and return the answer;
  the <cpp|async_http_post...> variants deliver it later through the
  asynchronous mechanism above, and <cpp|http_from_json> parses an
  answer. With <name|Qt> 6 they are implemented with the <name|Qt> network
  classes (<cpp|qt_http_post> in <source-link|Plugins/Qt6|src/Plugins/Qt6>); otherwise they
  build a <verbatim|curl> command line. Downloads of web files are
  described in <hlink|URLs, resolution and concretization|system-urls.en.tm>.

  <section|Output streams and messages>

  <cpp|tm_ostream> is a reference counted output stream with
  <cpp|operator \<less\>\<less\>> for strings, numbers and
  <cpp|formatted> trees. Its representations write to a <cpp|FILE*>, to a
  string, to a buffer, or to a <em|debug channel>:

  <\description>
    <item*|<cpp|cout>, <cpp|cerr>>Standard output and error.
    <cpp|redirect> replaces the representation; the
    <verbatim|-log-file> option uses it to send both to a file.
    <cpp|buffer> and <cpp|unbuffer> temporarily collect the output in a
    string (exported to <scheme> as <scm|cout-buffer> and
    <scm|cout-unbuffer>).

    <item*|Channels><cpp|std_error>, <cpp|failed_error>,
    <cpp|boot_error>, <cpp|io_error>, <cpp|std_warning>,
    <cpp|io_warning>, <cpp|debug_std>, <cpp|debug_io>, <cpp|debug_boot>,
    <cpp|debug_shell>, ... are streams on named channels. Writing to them
    calls <cpp|debug_message> (<source-link|Kernel/Abstractions/basic.cpp|src/Kernel/Abstractions/basic.cpp>),
    which prints the message on <cpp|cout> with the prefix
    <verbatim|TeXmacs] <em|channel>,>, stores it in the global list
    <cpp|debug_messages> and, once the editor runs, calls the <scheme>
    function <scm|notify-debug-message>. The debugging console and the
    message windows read this list with <scm|get-debug-messages>. The
    list is never shortened.
  </description>

  Whether debugging output is produced at all is decided by the caller,
  by testing the flags set with the <verbatim|-debug-...> options
  (<cpp|DEBUG_STD>, <cpp|DEBUG_IO>, <cpp|DEBUG_BENCH>, ...).

  <section|Time and benchmarks>

  <cpp|raw_time ()> is the wall clock time in milliseconds,
  <cpp|texmacs_time ()> the number of milliseconds since the program
  started (used for time stamps such as the last visit of a buffer), and
  <cpp|cpu_time_ms ()> the processor time of the process (used by the
  idle monitor of the server). <cpp|bench_start (task)> and
  <cpp|bench_cumul (task)> accumulate the time spent in a task (nested
  calls are counted once), <cpp|bench_end>, <cpp|bench_print> and
  <cpp|bench_reset> report and reset; the reports are printed only with
  <verbatim|-debug-bench>.

  <section|The server log>

  <source-link|System/Misc/server_log.hpp|src/System/Misc/server_log.hpp> defines log levels from
  <cpp|log_emergency> to <cpp|log_debug> and the macros <cpp|SLOG>,
  <cpp|SLOGI>, <cpp|SLOGW>, <cpp|SLOGE> and their <cpp|SERRNO_...>
  variants, which add the system error message. <cpp|server_log_write>
  is implemented per platform. When <TeXmacs> runs as a server and its
  standard output is not a terminal, messages go to the system log
  (<verbatim|syslog> on <name|Linux>, <verbatim|os_log> on
  <name|macOS>); otherwise they are printed on the stream of their level
  with a date, the process and the user id. See also <hlink|the
  <TeXmacs> server|collab-server.en.tm>.

  <section|Pitfalls>

  <\itemize>
    <item>On <name|Unix>, <cpp|system (cmd, out)> appends
    <verbatim|\<gtr\> <em|tmp> 2\<gtr\>&1> to the command. A redirection
    of standard error inside <cpp|cmd>, such as the
    <verbatim|2\<gtr\> /dev/null> used in many calls of
    <cpp|eval_system>, is overridden by the final <verbatim|2\<gtr\>&1>,
    so error messages end up in the result anyway
    (<verbatim|Plugins/Unix/unix_sys_utils.cpp:31-40>). Code which tests
    the result of <cpp|eval_system> for emptiness must take this into
    account.

    <item>In <cpp|async_eval_system>, the reading thread and the main
    thread share the flag <cpp|done> and the output buffer without any
    synchronization (<source-link|System/Misc/sys_utils.cpp|src/System/Misc/sys_utils.cpp>,
    <cpp|async_read_output> and <cpp|async_eval_pending>), which is a data
    race. The exit status is always reported as 0, standard error is
    discarded, and the <cpp|kill> flag is only tested before the command
    starts, so a running command cannot be stopped.

    <item><cpp|tm_poll> silently ignores descriptors beyond the 64th.

    <item><cpp|unbuffer> assumes that the stream is buffered; calling it
    (or <scm|cout-unbuffer>) without a preceding <cpp|buffer> crashes.

    <item><cpp|server_get_stream> has no <verbatim|default> case and
    returns nothing for an unknown level.
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
