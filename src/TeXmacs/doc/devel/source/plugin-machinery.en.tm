<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Sessions, connections and links: the plug-in machinery in
  <c++>>

  <section|Introduction>

  This chapter describes the machinery which connects <TeXmacs> to extern
  programs, as seen from the source code. It is meant for developers who want
  to debug a misbehaving session, change the way output is processed, or add
  a new kind of connection (for instance the missing socket connections).

  It assumes that the reader knows what a plug-in is and how it is
  configured; this is explained in the chapter on <hlink|the plug-in
  system|../plugin/plugins.en.tm>, whose last section, <hlink|How the plug-in
  system works internally|../plugin/plugin-internals.en.tm>, already covers
  the discovery of plug-ins, the plug-in cache, the connection tables of
  <verbatim|tm-plugins.scm>, the control characters and output formats of the
  pipe protocol, serialization of the input, special commands, background
  evaluation, remote plug-ins and the basic debugging techniques. The
  protocol is described from the point of view of the extern application in
  the chapter on <hlink|interfacing <TeXmacs> with other
  programs|../interface/interface.en.tm>, and all the options of
  <scm|plugin-configure> are listed in <hlink|the summary of configuration
  options|../plugin/plugin-config.en.tm>. These topics are not repeated here;
  instead, this chapter goes one level deeper: it describes the <c++> classes
  of <verbatim|System/Link>, the output parser, the integration with the
  event loop, and the exact sequence of calls between <scheme> and <c++>
  during the life of a session.

  <\traverse>
    <branch|Connections, the output parser and the life of a
    session|plugins-sessions.en.tm>

    <branch|Links and the event loop|plugins-links.en.tm>

    <branch|Debugging, known issues and limitations|plugins-issues.en.tm>
  </traverse>

  <section|Architecture overview>

  The communication with an extern program is organized in four layers. From
  the document down to the extern program, they are:

  <\description>
    <item*|The session layer (<scheme>)>The markup of sessions (the
    <markup|session> tag with its <markup|input>, <markup|unfolded-io>,
    <markup|output> and <markup|errput> children) and the editing routines in
    <verbatim|dynamic/session-edit.scm>. When an input field is evaluated,
    this layer builds a request and hands it over to the next layer. When
    output arrives, it inserts it into the document.

    <item*|The request queue (<scheme>)>The module <verbatim|utils/plugins/plugin-eval.scm>
    keeps, for every pair (language, session), a queue of pending requests
    (<scm|plugin-pending>). Each request carries four call-backs, which
    allows the same queue to serve interactive sessions
    (<verbatim|session-edit.scm>), silent evaluations
    (<scm|silent-feed>, used by scripts) and special commands
    (<scm|plugin-command>, used by tab-completion). It is the only client of
    the <scheme> glue functions <scm|connection-start>,
    <scm|connection-write>, <scm|connection-interrupt>, <abbr|etc.>, and it
    defines the two functions <scm|connection-notify> and
    <scm|connection-notify-status> which are called back from <c++>.

    <item*|The connection layer (<c++>)>The resource <cpp|connection> in
    <verbatim|System/Link/connection.cpp>. A connection is identified by the
    string <verbatim|<em|name>-<em|session>>, owns a link, keeps track of the
    status of the extern program (waiting for input or for output), and owns
    two parsers of type <cpp|texmacs_input>
    (<verbatim|Data/Convert/Generic/input.cpp>) which turn the byte streams
    of the standard output and the standard error into <TeXmacs> trees, one
    tree per channel.

    <item*|The link layer (<c++>)>Subclasses of the abstract class
    <cpp|tm_link_rep> (<verbatim|System/Link/tm_link.hpp>) which move bytes
    to and from the extern program: <cpp|pipe_link_rep> and
    <cpp|qt_pipe_link_rep> for processes, <cpp|dyn_link_rep> for shared
    libraries, <cpp|cmdline_link_rep> for one shell command per request and
    <cpp|request_link_rep> for <abbr|HTTP> requests.
  </description>

  The following diagram shows the main calls between the layers. Downward
  arrows are calls made on behalf of the user; upward arrows are
  notifications triggered by the event loop when the extern program
  produces output.

  <\verbatim-code>
    document: (session lan ses (document ... (unfolded-io prompt in out) ...))

    \ \ \| session-feed (session-edit.scm) \ \ \ \ \ \ \ ^ session-notify, session-next

    \ \ v \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    plugin-feed -\<gtr\> plugin-do -\<gtr\> plugin-write \ \ connection-notify (lan ses ch tree)

    \ \ (plugin-eval.scm, queue per (lan ses)) \ \ connection-notify-status (lan ses st)

    \ \ \| connection-start / -write / -stop \ \ \ \ \ ^

    \ \ v \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    connection_rep (connection.cpp) \ \ \ \ \ \ \ \ \ \ \ connection_rep::listen

    \ \ texmacs_input tm_out, tm_err \ \ \ \ \ \ \ \ \ \ \ \ \<less\>- connection_callback

    \ \ \| tm_link_rep::write (LINK_IN) \ \ \ \ \ \ \ \ \ \ ^ tm_link_rep::feed_cmd

    \ \ v \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    pipe_link_rep, qt_pipe_link_rep, dyn_link_rep, cmdline_link_rep, ...

    \ \ \| stdin \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ^ stdout, stderr

    \ \ v \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    extern process, shared library, shell command or HTTP request
  </verbatim-code>

  The <verbatim|scheme> language is special: it is handled entirely inside
  <scm|plugin-write>, which evaluates the input with <scm|scheme-eval> and
  calls <scm|connection-notify> and <scm|connection-notify-status> itself,
  without any <c++> connection.

  <section|Map of the source files>

  <c++> files are given relative to <verbatim|src/src>, <scheme> files
  relative to <verbatim|src/TeXmacs/progs>.

  <\description-paragraphs>
    <item*|<verbatim|System/Link/tm_link.hpp>, <verbatim|tm_link.cpp>>The
    abstract class <cpp|tm_link_rep>, the connection status constants, the
    control characters of the protocol, the factory functions
    <cpp|make_pipe_link>, <cpp|make_dynamic_link>, <cpp|make_cmdline_link>,
    <cpp|make_request_link>, and the packet functions
    <cpp|tm_link_rep::write_packet> and <cpp|tm_link_rep::read_packet> (only
    used by the client/server sockets of the <hlink|collaboration
    tools|collab-protocol.en.tm>, not by plug-ins).

    <item*|<verbatim|System/Link/connect.hpp>,
    <verbatim|connection.cpp>>The <cpp|connection> resource and the
    functions <cpp|connection_start>, <cpp|connection_write>,
    <cpp|connection_read>, <cpp|connection_interrupt>,
    <cpp|connection_stop>, <cpp|connection_status>, <cpp|connection_eval>
    and <cpp|connection_cmd>.

    <item*|<verbatim|System/Link/pipe_link.cpp>>Pipes implemented with
    <cpp|fork>, <cpp|execve> of <verbatim|/bin/sh> and <cpp|select>. Only
    compiled when <cpp|QTTEXMACS> is not defined, or when neither
    <cpp|OS_MINGW> nor <cpp|QTPIPES> is defined.

    <item*|<verbatim|Plugins/Qt/qt_pipe_link.cpp>,
    <verbatim|Plugins/Qt/QTMPipeLink.cpp>>Pipes implemented with
    <cpp|QProcess>; the alternative to <verbatim|pipe_link.cpp> for <name|Qt>
    builds on <name|Windows> or with <cpp|QTPIPES>. The <name|CMake> option
    <verbatim|QTPIPES> is <verbatim|ON> by default, so this is the
    implementation used by most current builds.

    <item*|<verbatim|System/Link/dyn_link.hpp>,
    <verbatim|dyn_link.cpp>>Dynamic linking of shared libraries
    (<cpp|symbol_install>, <cpp|dyn_link_rep>), using the interface declared
    in <verbatim|src/TeXmacs/include/TeXmacs.h>.

    <item*|<verbatim|System/Link/cmdline_link.cpp>,
    <verbatim|request_link.cpp>>The links behind the <scm|:cmdline> and
    <scm|:request> options.

    <item*|<verbatim|System/Link/socket_notifier.hpp>,
    <verbatim|socket_notifier.cpp>>A minimal registry of file descriptors
    with call-backs, polled by <cpp|perform_select>.

    <item*|<verbatim|Data/Convert/Generic/input.hpp>,
    <verbatim|input.cpp>>The parser <cpp|texmacs_input_rep> which converts
    the output of a plug-in into trees.

    <item*|<verbatim|Texmacs/Server/tm_server.cpp>>The interpose handler
    <cpp|tm_server_rep::interpose_handler>, from which pending pipe output is
    processed, and <cpp|close_all_pipes> at exit.

    <item*|<verbatim|Scheme/Glue/build-glue-basic.scm>>The declarations of
    the glue functions <scm|connection-start>, <scm|connection-status>,
    <scm|connection-write-string>, <scm|connection-write>,
    <scm|connection-cmd>, <scm|connection-eval>, <scm|connection-interrupt>
    and <scm|connection-stop>, and of <scm|plugin-list>.

    <item*|<verbatim|Edit/Interface/edit_complete.cpp>>The <c++> part of
    tab-completion in sessions (<cpp|edit_interface_rep::session_complete_command>
    and <cpp|edit_interface_rep::custom_complete>).

    <item*|<verbatim|kernel/texmacs/tm-plugins.scm>>Translation of the
    options of <scm|plugin-configure> into launcher descriptions, and the
    functions called from <c++>: <scm|connection-defined?>,
    <scm|connection-info>, <scm|connection-get-handlers>,
    <scm|connection-cmdline>, <scm|connection-request> and
    <scm|connection-result>.

    <item*|<verbatim|utils/plugins/plugin-eval.scm>>The request queue and
    the notification call-backs.

    <item*|<verbatim|utils/plugins/plugin-cmd.scm>>Serialization
    (<scm|plugin-serialize>) and formatting of special commands
    (<scm|format-command>), both called from <c++>.

    <item*|<verbatim|dynamic/session-edit.scm>,
    <verbatim|dynamic/session-menu.scm>>Interactive sessions.

    <item*|<verbatim|dynamic/scripts-edit.scm>>Evaluation of scripts and
    executable fields in ordinary documents.
  </description-paragraphs>

  Two names are misleading. The function <cpp|init_plugins> in
  <verbatim|System/Boot/init_texmacs.cpp> does not initialize plug-ins in
  the above sense: it loads the user settings and sets up <TeX> (the
  directories of plug-ins are added to the search paths by
  <cpp|init_env_vars>, see <hlink|search
  paths|../plugin/plugin-internals.en.tm>). Similarly, the file
  <verbatim|Edit/Process/edit_process.cpp> deals with the generation of
  bibliographies, tables of contents and indexes, not with sessions.

  <section|Threading and the event loop>

  Everything described in this chapter runs in the main thread of
  <TeXmacs>. There is no reader thread per plug-in: the output of extern
  programs is collected when the event loop polls for it, and the
  notifications into <scheme> are made synchronously from the poll. The only
  exceptions are the <abbr|HTTP> requests of <cpp|request_link_rep>, which
  are run by a detached <name|POSIX> thread created in
  <cpp|async_eval_system> (<verbatim|System/Misc/sys_utils.cpp>); the result
  is handed back to the main thread by <cpp|async_eval_pending>.

  The central hook is <cpp|tm_server_rep::interpose_handler>, which the
  <abbr|GUI> calls regularly:

  <\cpp-code>
    void

    tm_server_rep::interpose_handler () {

    #ifdef QTTEXMACS

    #ifndef QTPIPES

    \ \ perform_select ();

    #endif

    \ \ process_all_pipes ();

    #else

    \ \ perform_select ();

    \ \ exec_pending_commands ();

    #endif

    \ \ async_eval_pending ();

    \ \ ... // apply pending changes to all views

    }
  </cpp-code>

  In the <name|Qt> port, it is called from <cpp|qt_gui_rep::update>, which is
  rescheduled by a single-shot timer every <verbatim|90 / 6> = 15
  milliseconds when the editor is idle (<verbatim|Plugins/Qt/qt_gui.cpp>),
  and is skipped while keyboard events are being processed. Consequently:

  <\itemize>
    <item>Output of a plug-in is processed with a latency of about one
    timer period, and in chunks: a single call to
    <cpp|connection_rep::listen> may receive many lines, or several complete
    <verbatim|DATA_BEGIN>...<verbatim|DATA_END> blocks.

    <item>Since notifications are processed in the main thread, a slow
    <scheme> call-back (for instance a costly conversion of a large output)
    blocks the user interface.

    <item>Operations which wait for the extern program block the user
    interface as well. This is the case of the synchronous evaluation
    <cpp|connection_eval> (<scm|plugin-eval>), which polls in a loop until
    the program is waiting for input again, and of
    <cpp|pipe_link_rep::stop>, which sends <verbatim|SIGTERM>, sleeps for two
    seconds and then sends <verbatim|SIGKILL>.
  </itemize>

  How each kind of link participates in the polling is described in
  <hlink|links and the event loop|plugins-links.en.tm>.

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
