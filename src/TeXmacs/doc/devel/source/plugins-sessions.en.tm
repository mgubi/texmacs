<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Connections, the output parser and the life of a session>

  <section|The connection resource>

  <subsection|Data structure>

  A connection is a <em|resource> in the sense of
  <verbatim|Kernel/Abstractions/resource.hpp>: a global object which is
  registered under a name when it is created and which can be retrieved from
  anywhere by constructing <cpp|connection (<em|name>)>. The name of a
  connection is <verbatim|<em|lan>-<em|ses>>, where <em|lan> is the name of
  the plug-in (the value of <verbatim|prog-language> in a session) and
  <em|ses> the name of the session (the value of
  <verbatim|prog-session>, <verbatim|default> unless the user chose another
  session). The structure is private to
  <verbatim|System/Link/connection.cpp>:

  <\cpp-code>
    RESOURCE(connection);

    struct connection_rep: rep\<less\>connection\<gtr\> {

    \ \ string \ name; \ \ \ \ \ \ \ \ \ // name of the pipe type

    \ \ string \ session; \ \ \ \ \ \ // name of the session

    \ \ tree \ \ \ info; \ \ \ \ \ \ \ \ \ // startup information tree

    \ \ tm_link ln; \ \ \ \ \ \ \ \ \ \ \ // the underlying link

    \ \ int \ \ \ \ status; \ \ \ \ \ \ \ // status of the connection

    \ \ int \ \ \ \ prev_status; \ \ // last notified status

    \ \ bool \ \ \ forced_eval; \ \ // forced input evaluation without call backs

    \ \ bool \ \ \ cmdline_eval; \ // command line style evaluation

    \ \ bool \ \ \ request_eval; \ // request style evaluation

    \ \ texmacs_input tm_out; \ // texmacs input handler for output from child

    \ \ texmacs_input tm_err; \ // texmacs input handler for errors from child

    \ \ ...

    };
  </cpp-code>

  The field <cpp|info> is the launcher description returned by the
  <scheme> function <scm|connection-info> (for instance <scm|(tuple "pipe"
  "tm_maxima")>), <cpp|ln> is the link which actually talks to the extern
  program, and <cpp|tm_out> and <cpp|tm_err> are the parsers for its standard
  output and standard error.

  Resources are never destroyed. Once created, a connection lives until
  <TeXmacs> exits, and stopping a session only stops its link; restarting
  the session restarts the same link object. A new <cpp|connection_rep> (with
  a new link) is only created by <cpp|connection_start> if the launcher
  description changed in the meantime, for instance after
  <scm|reinit-plugin-cache>. In that case the new object simply replaces the
  old one in the resource table.

  <subsection|Status>

  The status constants are defined in <verbatim|System/Link/tm_link.hpp>:

  <descriptive-table|<tformat|<cwith|1|1|1|-1|cell-font-series|bold>|<table|<row|<cell|Constant>|<cell|Value>|<cell|Meaning>>|<row|<cell|<cpp|CONNECTION_DEAD>>|<cell|0>|<cell|the
  link is not alive>>|<row|<cell|<cpp|CONNECTION_DYING>>|<cell|1>|<cell|the
  link was interrupted or stopped while output was
  expected>>|<row|<cell|<cpp|WAITING_FOR_INPUT>>|<cell|2>|<cell|the program
  is idle: the last request is complete>>|<row|<cell|<cpp|WAITING_FOR_OUTPUT>>|<cell|3>|<cell|a
  request was sent and its output is not complete yet>>>>>

  The value <cpp|CONNECTION_DYING> is internal: both
  <cpp|connection_status> (the <scheme> function <scm|connection-status>)
  and the notifications to <scheme> report it as
  <cpp|WAITING_FOR_OUTPUT>. Conversely, <cpp|connection_status> reports
  <cpp|CONNECTION_DEAD> whenever the link is not alive, regardless of the
  field <cpp|status>. Seen from <scheme>, the status is therefore 0 (dead),
  2 (idle) or 3 (busy). The status changes as follows:

  <\itemize>
    <item><cpp|connection_rep::start> sets it to <cpp|WAITING_FOR_OUTPUT>
    after launching the link, because the program is expected to print a
    banner terminated by a <verbatim|DATA_END>. If the link was already alive,
    the status becomes <cpp|WAITING_FOR_INPUT> and the message
    <verbatim|Continuation of '<em|name>' session> is returned.

    <item><cpp|connection_rep::write> sets it to <cpp|WAITING_FOR_OUTPUT>.

    <item><cpp|connection_rep::read> sets it to <cpp|WAITING_FOR_INPUT> as
    soon as the output parser has seen the <verbatim|DATA_END> which closes
    the outermost block, and to <cpp|CONNECTION_DEAD> as soon as the link is
    no longer alive (end of file on the output of the program).

    <item><cpp|connection_rep::interrupt> and <cpp|connection_rep::stop> set
    it to <cpp|CONNECTION_DYING> if the program was busy.
  </itemize>

  The field <cpp|prev_status> holds the last status which was reported to
  <scheme>; <cpp|connection_notify_status> only calls
  <scm|connection-notify-status> when the status changed since.

  <subsection|Creation and start>

  The entry point is <cpp|connection_start>, called from <scheme> as
  <scm|(connection-start <scm-arg|lan> <scm-arg|ses>)>:

  <\cpp-code>
    string

    connection_start (string name, string session, bool again) {

    \ \ if (!connection_declared (name))

    \ \ \ \ return "Error: connection " * name * " has not been declared";

    \ \ connection con= connection (name * "-" * session);

    \ \ tree t= connection_info (name, session);

    \ \ if (is_nil (con) \|\| con-\<gtr\>info != t) {

    \ \ \ \ if (is_tuple (t, "cmdline")) {

    \ \ \ \ \ \ tm_link ln= make_cmdline_link (name);

    \ \ \ \ \ \ con= tm_new\<less\>connection_rep\<gtr\> (name, session, ln);

    \ \ \ \ \ \ con-\<gtr\>cmdline_eval= true;

    \ \ \ \ }

    \ \ \ \ if (is_tuple (t, "request")) { ... make_request_link ... }

    \ \ \ \ if (is_tuple (t, "pipe", 1)) {

    \ \ \ \ \ \ tm_link ln= make_pipe_link (t[1]-\<gtr\>label);

    \ \ \ \ \ \ con= tm_new\<less\>connection_rep\<gtr\> (name, session, ln);

    \ \ \ \ }

    \ \ \ \ else if (is_tuple (t, "dynlink", 3)) { ... make_dynamic_link ... }

    \ \ \ \ con-\<gtr\>info= t;

    \ \ }

    \ \ return con-\<gtr\>start (again);

    }
  </cpp-code>

  The functions <cpp|connection_declared>, <cpp|connection_info> and
  <cpp|connection_handlers> are thin wrappers around the <scheme> functions
  <scm|connection-defined?>, <scm|connection-info> and
  <scm|connection-get-handlers> of <verbatim|tm-plugins.scm>, which are
  described in <hlink|connection tables and
  predicates|../plugin/plugin-internals.en.tm>. Notice that the result of
  <cpp|connection_handlers> is cached in a static table on first use, so
  that handlers which are declared later (after a re-detection of the
  plug-ins) are not taken into account until <TeXmacs> is restarted.

  The method <cpp|connection_rep::start> then starts the link if necessary,
  resets the two parsers (<cpp|bof>), installs the call-back through which
  the link signals new data, and handles a few special cases:

  <\cpp-code>
    string

    connection_rep::start (bool again) {

    \ \ string message;

    \ \ if (ln-\<gtr\>alive) {

    \ \ \ \ message= "Continuation of '" * name * "' session";

    \ \ \ \ status = WAITING_FOR_INPUT;

    \ \ }

    \ \ else {

    \ \ \ \ message= ln-\<gtr\>start ();

    \ \ \ \ tm_out = texmacs_input ("output");

    \ \ \ \ tm_err = texmacs_input ("error");

    \ \ \ \ status = WAITING_FOR_OUTPUT;

    \ \ \ \ if (again && (message == "ok")) {

    \ \ \ \ \ \ beep ();

    \ \ \ \ \ \ (void) connection_retrieve (name, session);

    \ \ \ \ }

    \ \ }

    \ \ tm_out-\<gtr\>bof ();

    \ \ tm_err-\<gtr\>bof ();

    \ \ ln-\<gtr\>set_command (command (connection_callback, this));

    \ \ if (name == "dynlink") {

    \ \ \ \ this-\<gtr\>listen ();

    \ \ \ \ status = WAITING_FOR_OUTPUT;

    \ \ }

    \ \ if (message == "cmdline") { ... status= WAITING_FOR_INPUT; ... }

    \ \ if (message == "request") { ... status= WAITING_FOR_INPUT; ... }

    \ \ return message;

    }
  </cpp-code>

  The <cpp|command> installed with <cpp|tm_link_rep::set_command> is stored
  in the field <cpp|feed_cmd> of the link. Whenever the link has new data, it
  (or the polling code, see <hlink|links and the event
  loop|plugins-links.en.tm>) applies this command, which calls the static
  function <cpp|connection_callback> and hence
  <cpp|connection_rep::listen>.

  The argument <cpp|again> is only set by <cpp|connection_get>, the helper of
  the synchronous evaluation functions: when a connection is started
  implicitly by <cpp|connection_eval>, <TeXmacs> beeps and swallows the
  banner synchronously. The special treatment of the plug-in named
  <verbatim|dynlink> is discussed in <hlink|known
  issues|plugins-issues.en.tm>.

  <subsection|Writing, reading and listening>

  Input is sent by <cpp|connection_write>. The variant taking a tree, which
  is the one behind <scm|connection-write>, first converts the tree into a
  string by calling <scm|(plugin-serialize <scm-arg|lan> <scm-arg|t>)>; the
  variant taking a string is exposed as <scm|connection-write-string> and is
  used for special commands. In both cases <cpp|connection_rep::write>
  writes the string on the input channel of the link, resets both parsers
  and marks the connection as busy.

  Output is processed by <cpp|connection_rep::listen>, the heart of the
  connection layer:

  <\cpp-code>
    void

    connection_rep::listen () {

    \ \ if (forced_eval) return;

    \ \ connection_notify_status (this);

    \ \ if (status != CONNECTION_DEAD) {

    \ \ \ \ if (cmdline_eval \|\| request_eval) ln-\<gtr\>listen (1);

    \ \ \ \ read (LINK_ERR);

    \ \ \ \ connection_notify (this, "error", tm_err-\<gtr\>get ("error"));

    \ \ \ \ read (LINK_OUT);

    \ \ \ \ connection_notify (this, "output", tm_out-\<gtr\>get ("output"));

    \ \ \ \ connection_notify (this, "prompt", tm_out-\<gtr\>get ("prompt"));

    \ \ \ \ connection_notify (this, "input", tm_out-\<gtr\>get ("input"));

    \ \ \ \ tree t= connection_handlers (name);

    \ \ \ \ for (int i=0; i\<less\>N(t); i++) {

    \ \ \ \ \ \ tree doc= tm_out-\<gtr\>get (t[i][0]-\<gtr\>label);

    \ \ \ \ \ \ if (doc != "") call (t[i][1]-\<gtr\>label, doc);

    \ \ \ \ \ \ doc= tm_err-\<gtr\>get (t[i][0]-\<gtr\>label);

    \ \ \ \ \ \ if (doc != "") call (t[i][1]-\<gtr\>label, doc);

    \ \ \ \ }

    \ \ }

    \ \ connection_notify_status (this);

    }
  </cpp-code>

  The method <cpp|connection_rep::read> takes all the bytes which the link
  has buffered for the given channel (<cpp|tm_link_rep::read>) and feeds them
  one by one to the corresponding parser using <cpp|texmacs_input_rep::put>.
  The parser accumulates the converted trees per channel; <cpp|get> removes
  and returns the accumulated document of a channel, or the empty string if
  there is nothing new. Finally, <cpp|connection_notify> calls

  <\scm-code>
    (connection-notify <scm-arg|lan> <scm-arg|ses> <scm-arg|channel> <scm-arg|tree>)
  </scm-code>

  for every non-empty channel, and <cpp|connection_notify_status> calls
  <scm|(connection-notify-status <scm-arg|lan> <scm-arg|ses>
  <scm-arg|status>)> if the status changed.

  Three consequences of this code are worth noticing. First, the standard
  error is always processed before the standard output, so the relative
  order of error and normal output produced during one polling interval is
  lost. Secondly, the channels are always notified in the order
  <verbatim|error>, <verbatim|output>, <verbatim|prompt>, <verbatim|input>,
  followed by the custom channels declared with <scm|:handler>; the custom
  handlers are called with a single argument (the tree) and bypass the
  request queue. Thirdly, the status is notified both before and after the
  output, so that a status change to 2 (end of output) always reaches
  <scheme> after the corresponding output.

  The flag <cpp|forced_eval> is set by <cpp|connection_retrieve> during
  synchronous evaluations, so that output which is read by the synchronous
  loop is not concurrently consumed by an asynchronous call-back.

  <section|The output parser>

  <subsection|State>

  The class <cpp|texmacs_input_rep>, declared in
  <verbatim|Data/Convert/Generic/input.hpp>, turns a stream of bytes into
  documents. One instance is created for the standard output (with
  <cpp|type> equal to <verbatim|"output">) and one for the standard error
  (<cpp|type> equal to <verbatim|"error">); the type is the default channel.

  <\cpp-code>
    struct texmacs_input_rep: concrete_struct {

    \ \ string type; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // default value for channel below

    \ \ int \ \ \ status; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // status of parser

    \ \ string buf; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // input buffer

    \ \ string format; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // current input format

    \ \ int \ \ \ mode; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // corresponding input mode

    \ \ string channel; \ \ \ \ \ \ \ \ \ \ \ \ \ \ // current output channel

    \ \ tree \ \ stack; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // stack for nested blocks

    \ \ bool \ \ ignore_verb; \ \ \ \ \ \ \ \ \ \ // hack to enable completion with some plugins

    \ \ hashmap\<less\>string,tree\<gtr\> docs; \ \ \ // output for each channel

    \ \ ...

    };
  </cpp-code>

  The <cpp|stack> is a linked list of triples <cpp|tuple (format, channel,
  stack)>, terminated by the empty string. The <cpp|mode> is an integer code
  which is computed from the <cpp|format> by
  <cpp|texmacs_input_rep::get_mode> and selects the flushing routine.

  <subsection|The state machine>

  The method <cpp|put> processes one character and returns <cpp|true> when
  the character completed the outermost block, that is, when the program is
  ready for new input:

  <\cpp-code>
    bool

    texmacs_input_rep::put (char c) {

    \ \ bool block_done= false;

    \ \ switch (status) {

    \ \ case STATUS_NORMAL:

    \ \ \ \ if (c == DATA_ESCAPE) status= STATUS_ESCAPE;

    \ \ \ \ else if (c == DATA_BEGIN) { flush (true); status= STATUS_BEGIN; }

    \ \ \ \ else if (c == DATA_ABORT && (format == "verbatim" \|\| format == "utf8")

    \ \ \ \ \ \ \ \ \ \ \ \ \ && buf == "") ignore_verb= true;

    \ \ \ \ else if (c == DATA_END) {

    \ \ \ \ \ \ flush (true);

    \ \ \ \ \ \ end ();

    \ \ \ \ \ \ block_done= (stack == "");

    \ \ \ \ \ \ ignore_verb= (ignore_verb && stack != "");

    \ \ \ \ }

    \ \ \ \ else buf \<less\>\<less\> c;

    \ \ \ \ break;

    \ \ case STATUS_ESCAPE:

    \ \ \ \ buf \<less\>\<less\> c; status= STATUS_NORMAL; break;

    \ \ case STATUS_BEGIN:

    \ \ \ \ if (c == ':') { begin_mode (buf); buf= ""; status= STATUS_NORMAL; }

    \ \ \ \ else if (c == '#') { begin_channel (buf); buf= ""; status= STATUS_NORMAL; }

    \ \ \ \ else buf \<less\>\<less\> c;

    \ \ \ \ break;

    \ \ }

    \ \ if (status == STATUS_NORMAL) flush ();

    \ \ return block_done;

    }
  </cpp-code>

  After a <verbatim|DATA_BEGIN>, the characters up to the first
  <verbatim|:> or <verbatim|#> are collected as the name of a format or
  channel. <cpp|begin_mode> pushes the current format and channel on the
  stack and switches the format; <cpp|begin_channel> pushes them and switches
  the channel (and empties the documents of the <verbatim|prompt> and
  <verbatim|input> channels, so that only the last prompt counts). A
  <verbatim|DATA_END> forces a flush in the current format and pops the
  stack with <cpp|end>. A <verbatim|DATA_END> with an empty stack (that is,
  without a matching <verbatim|DATA_BEGIN>) also counts as the end of the
  output.

  Before each block, and after each ordinary character, the buffer is
  flushed: <cpp|flush (true)> always converts the whole buffer, while
  <cpp|flush ()> lets the current format decide whether enough text is
  available. As an exception, text on the <verbatim|error> channel inside a
  block is only flushed when forced, that is, at the end of the block.

  <subsection|Conversion of the formats>

  The flushing routines and the conversions they perform are:

  <descriptive-table|<tformat|<cwith|1|1|1|-1|cell-font-series|bold>|<table|<row|<cell|Format>|<cell|Routine>|<cell|Flushed
  when>|<cell|Conversion>>|<row|<cell|<verbatim|verbatim>>|<cell|<cpp|verbatim_flush>>|<cell|newline>|<cell|<cpp|verbatim_to_tree
  (buf, false, "auto")>>>|<row|<cell|<verbatim|utf8>>|<cell|<cpp|utf8_flush>>|<cell|newline>|<cell|<cpp|utf8_to_cork>,
  then <cpp|verbatim_to_tree>>>|<row|<cell|<verbatim|latex>>|<cell|<cpp|latex_flush>>|<cell|empty
  line>|<cell|<cpp|generic_to_tree (buf,
  "latex-snippet")>>>|<row|<cell|<verbatim|html>>|<cell|<cpp|html_flush>>|<cell|<verbatim|\<less\>/P\<gtr\>>>|<cell|<cpp|generic_to_tree>
  with <verbatim|"html-snippet">, wrapped in
  <markup|html-text>>>|<row|<cell|<verbatim|scheme>>|<cell|<cpp|scheme_flush>>|<cell|end
  of block>|<cell|<cpp|simplify_correct (scheme_to_tree
  (buf))>>>|<row|<cell|<verbatim|math>>|<cell|<cpp|math_flush>>|<cell|end of
  block>|<cell|<scm|string-\<gtr\>object>, <scm|cas-\<gtr\>stree>,
  <scm|tm-\<gtr\>tree>, wrapped in <cpp|WITH> <verbatim|mode
  math>>>|<row|<cell|<verbatim|ps>>|<cell|<cpp|ps_flush>>|<cell|end of
  block>|<cell|<cpp|IMAGE> with <cpp|RAW_DATA>>>|<row|<cell|<verbatim|file>>|<cell|<cpp|file_flush>>|<cell|end
  of block>|<cell|file loaded with <cpp|load_string>, then
  <cpp|image_flush>>>|<row|<cell|<verbatim|command>>|<cell|<cpp|command_flush>>|<cell|end
  of block>|<cell|<cpp|eval> of <verbatim|(begin
  <em|buf>)>>>|<row|<cell|<verbatim|channel>>|<cell|<cpp|channel_flush>>|<cell|end
  of block>|<cell|replaces the channel saved on top of the
  stack>>|<row|<cell|any <scm|format?>>|<cell|<cpp|xformat_flush>>|<cell|end
  of block>|<cell|<cpp|generic_to_tree (buf, format *
  "-snippet")>>>|<row|<cell|<verbatim|cmdline-<em|name>>>|<cell|<cpp|cmdline_flush>>|<cell|end
  of output>|<cell|<scm|(connection-result <em|name> "default"
  <em|buf>)>>>|<row|<cell|<verbatim|request-<em|name>>>|<cell|<cpp|request_flush>>|<cell|end
  of output>|<cell|same as <cpp|cmdline_flush>>>>>>

  Unknown formats fall back to <verbatim|verbatim>. Since the
  <verbatim|command> format evaluates arbitrary <scheme> code, a plug-in has
  full control over the editor; this is by design (see <hlink|sending
  commands to <TeXmacs>|../interface/interface-commands.en.tm>), but it means
  that the output of untrusted programs should never be piped into a
  session. The method <cpp|ispell_flush> is declared in
  <verbatim|input.hpp> but has no implementation and no mode.

  <subsection|Accumulating the output>

  Every flushing routine passes its result to
  <cpp|texmacs_input_rep::write>, which appends it to the document of the
  current channel in <cpp|docs>. If the new tree is not a
  <markup|document>, it is first wrapped into one. Its first paragraph is
  concatenated with the last paragraph of the accumulated document (so that
  output arriving in several pieces without newline ends up in the same
  paragraph), and its remaining paragraphs are appended.

  The method <cpp|bof> is called whenever a new request starts: it resets
  the format to <verbatim|verbatim> (except for the
  <verbatim|cmdline-<em|name>> and <verbatim|request-<em|name>> formats,
  which are sticky), the channel to the default channel, and clears the
  document of the default channel. The method <cpp|eof> forces a final flush
  when the link died.

  The prompt is an ordinary channel: the program sends
  <verbatim|DATA_BEGIN prompt# ... DATA_END> (or, in the legacy syntax,
  <verbatim|DATA_BEGIN channel:prompt DATA_END>, which works by overwriting
  the channel which <cpp|end> will restore), and the session layer decides
  what to do with it.

  <section|The request queue>

  <subsection|Requests and call-backs>

  All asynchronous traffic with a connection goes through the queue of the
  pair (<em|lan>, <em|ses>) in <verbatim|utils/plugins/plugin-eval.scm>. A
  request is a list whose head is a list of five elements

  <\scm-code>
    ((<em|do> <em|notify> <em|next> <em|cancel> <em|author>) . <em|args>)
  </scm-code>

  where <em|args> is specific to the client: for sessions it is
  <scm|(<em|in> <em|out> <em|next> <em|opts>)>, as built by
  <scm|session-encode>, where <em|out> and <em|next> are tree pointers to
  the output of the evaluated field and to the next input field; for silent
  evaluations it is <scm|(<em|in> <em|out> <em|err> <em|return> <em|opts>)>,
  as built by <scm|silent-encode>. The call-backs are invoked as
  <scm|(<em|do> <em|lan> <em|ses>)>, <scm|(<em|notify> <em|lan> <em|ses>
  <em|channel> <em|tree>)>, <scm|(<em|next> <em|lan> <em|ses>)> and
  <scm|(<em|cancel> <em|lan> <em|ses> <em|dead?>)>, and always operate on
  the request at the head of the queue (<scm|(car (pending-ref <em|lan>
  <em|ses>))>).

  The <em|author> is a fresh number obtained with <scm|new-author> when the
  request is queued. <scm|plugin-feed> registers it with
  <scm|start-slave> (which calls <cpp|archiver_rep::start_slave> in
  <verbatim|Data/History/archiver.cpp>), and the notifications run inside
  <scm|with-author>, which temporarily switches the current author and
  commits the changes. This way, the modifications which the asynchronous
  output makes to the document are attributed to the request in the undo
  history rather than mixed with the edits which the user makes in the
  meantime.

  <subsection|The scheduler>

  The queue is driven by four functions:

  <\explain>
    <scm|(plugin-feed <scm-arg|lan> <scm-arg|ses> <scm-arg|do>
    <scm-arg|notify> <scm-arg|next> <scm-arg|cancel>
    <scm-arg|args>)><explain-synopsis|enqueue a request>
  <|explain>
    Append a request to the queue and call <scm|plugin-do> if the queue was
    empty. Requests queued while another one is being processed wait until
    the latter is complete.
  </explain>

  <\explain>
    <scm|(plugin-do <scm-arg|lan> <scm-arg|ses>)><explain-synopsis|process
    the head of the queue>
  <|explain>
    If the head is a start request (its first argument is <scm|:start>), call
    <scm|plugin-start> if the connection is dead, and <scm|plugin-next>
    otherwise. For <scm|:cmdline> and <scm|:request> plug-ins, call the
    <em|do> call-back directly. If the connection is dead, push a
    <em|silent> start request in front of the queue and process it first; its
    output (the banner) is discarded. Otherwise call the <em|do> call-back,
    which usually calls <scm|plugin-write>.
  </explain>

  <\explain>
    <scm|(plugin-next <scm-arg|lan> <scm-arg|ses>)><explain-synopsis|request
    complete>
  <|explain>
    Call the <em|next> call-back of the head, remove it from the queue and
    process the following request.
  </explain>

  <\explain>
    <scm|(plugin-cancel <scm-arg|lan> <scm-arg|ses>
    <scm-arg|dead?>)><explain-synopsis|cancel all requests>
  <|explain>
    Call the <em|cancel> call-back of every request in the queue and empty
    it.
  </explain>

  The notifications from <c++> are dispatched as follows.
  <scm|connection-notify> passes the tree to the <em|notify> call-back of
  the head of the queue (output on a connection with an empty queue is
  dropped), and records trees on the <verbatim|prompt> channel in the table
  <scm|plugin-prompts>, from which <scm|plugin-prompt> later takes the
  prompt of new input fields. <scm|connection-notify-status> reacts to the
  status: 2 calls <scm|plugin-next>; 0 forgets the prompt and the timing
  and calls <scm|plugin-cancel> with <scm|dead?> set to <scm|#t>, except
  for <scm|:cmdline> and <scm|:request> plug-ins, for which 0 is the normal
  end of a request and calls <scm|plugin-next>. The status 3 is ignored.

  The function <scm|plugin-write> records the starting time of the request
  (for <scm|plugin-timing>) and sends the request: a tree of the form
  <scm|(command <em|string>)> is sent verbatim with
  <scm|connection-write-string>, anything else is serialized with
  <scm|connection-write>. For the <verbatim|scheme> language, it schedules a
  <scm|delayed> evaluation with <scm|scheme-eval> which simulates the
  notifications of a real connection.

  <section|The life of a session>

  This section follows a session from its creation to its end. The
  functions are in <verbatim|dynamic/session-edit.scm> unless stated
  otherwise.

  <subsection|Creating the session>

  <menu|Insert|Session|<em|Name>> calls <scm|(make-session <scm-arg|lan>
  <scm-arg|ses>)>, which inserts

  <\scm-code>
    (session lan ses (document (output (document "")) (input prompt (document ""))))
  </scm-code>

  (with <markup|input-text> or <markup|input-math> instead of
  <markup|input> according to the preferences), adds the style package
  <verbatim|<em|lan>.ts> if it exists, and calls

  <\scm-code>
    (session-feed lan ses :start banner-document input-field '())
  </scm-code>

  The <markup|output> before the first input field will receive the banner.
  <scm|session-feed> replaces its content by <markup|script-busy> (which is
  rendered as a busy indicator) and enqueues the request.

  <subsection|Starting the program>

  Since the first argument is <scm|:start>, <scm|plugin-do> calls
  <scm|plugin-start>, and hence <cpp|connection_start>, which creates the
  connection and its link and starts the link: for a pipe, a process is
  launched. The status is now <cpp|WAITING_FOR_OUTPUT>.

  The program prints its banner followed by a prompt, for instance

  <\verbatim-code>
    DATA_BEGIN verbatim:Welcome to MyCAS 1.0

    DATA_BEGIN prompt#\<gtr\> DATA_END

    DATA_END
  </verbatim-code>

  (without the spaces after <verbatim|DATA_BEGIN>). At the next poll, the
  link has data and <cpp|connection_rep::listen> runs. The banner is
  delivered on the <verbatim|output> channel to <scm|session-notify>, which
  inserts it into the banner output with <scm|session-output> (in front of
  the <markup|script-busy> tag); the prompt is delivered on the
  <verbatim|prompt> channel and becomes the prompt of the input field if
  this is the last pending request and the field is still empty. When the
  outermost <verbatim|DATA_END> has been parsed, the status becomes
  <cpp|WAITING_FOR_INPUT>, <scm|connection-notify-status> calls
  <scm|plugin-next>, and <scm|session-next> removes the
  <markup|script-busy> tag (and the whole <markup|output> if it stayed
  empty).

  If the connection was already alive (for instance a second session with
  the same name in another document), <scm|plugin-do> calls
  <scm|plugin-next> immediately and the empty banner is removed.

  <subsection|Evaluating an input field>

  When the user presses <key|return> in an input field, <scm|kbd-enter> is
  called. If multi-line input is active, a newline is inserted. If the
  plug-in supports <scm|:test-input-done>, the special command
  <verbatim|(input-done? <em|string>)> is first sent through
  <scm|plugin-command> and the input is only evaluated if the answer is not
  <verbatim|#f>. Otherwise <scm|session-evaluate> calls
  <scm|field-process-input>, which

  <\enumerate>
    <item>checks that the language is known (<scm|session-ready?>);

    <item>turns the <markup|input> into an <markup|unfolded-io> with an
    empty output (<scm|field-insert-output>);

    <item>finds the next input field, or creates a new one with the current
    prompt (<scm|field-create>);

    <item>calls <scm|session-feed> with the input (as a <scheme> tree), the
    output and the next field, and moves the cursor to the next field.
  </enumerate>

  <scm|session-feed> preprocesses the input (<scm|plugin-preprocess>, which
  converts mathematical input if <scm|:math-input> is among the options),
  sets the output to <scm|(document (script-busy))>, and enqueues the
  request. When the request reaches the head of the queue,
  <scm|session-do> checks that the output and the next field are still at
  their place in the document (<scm|session-coherent?>; the user may have
  deleted them in the meantime), copies the current prompt into the
  evaluated field, and calls <scm|plugin-write>. Empty inputs are skipped,
  except for the <verbatim|r> plug-in.

  Several fields can be evaluated in a row (<scm|session-evaluate-all>,
  <scm|session-evaluate-above>, <scm|session-evaluate-below>): they are
  simply queued, and each one is sent when the previous one is complete.

  <subsection|Receiving the output>

  Output is inserted incrementally. Each call to
  <cpp|connection_rep::listen> delivers what the parser converted so far:
  for verbatim output, all complete lines. <scm|session-output> inserts the
  paragraphs of the received document before the trailing
  <markup|script-busy> tag (and before a trailing <markup|errput>), and
  <scm|session-errput> inserts error output into an <markup|errput> tag,
  creating it if needed. A tree received on the <verbatim|input> channel
  becomes the content of the next input field, provided that no other
  request is pending.

  When the output is complete, the status changes to 2 and
  <scm|session-next> replaces the <markup|script-busy> tag by a
  <markup|timing> tag (if timings were requested with
  <menu|Show timings> in the output options of the session, and the evaluation took at least one millisecond) or
  removes it, removes the output if it is empty, and releases the tree
  pointers.

  <subsection|Interrupting>

  <menu|Interrupt execution> calls <scm|plugin-interrupt>. If the
  connection is busy, <cpp|connection_interrupt> calls
  <cpp|tm_link_rep::interrupt> (which sends <verbatim|SIGINT> to the
  program, see <hlink|links|plugins-links.en.tm>), sets the status to
  <cpp|CONNECTION_DYING> and calls <cpp|connection_rep::listen>. On the
  <scheme> side, all pending requests are cancelled: their
  <markup|script-busy> tags become <markup|script-interrupted>.

  The program is expected to react to <verbatim|SIGINT> by terminating its
  current output with <verbatim|DATA_END>. Until it does, the connection is
  reported as busy (status 3). Notice that the requests are cancelled on
  the <scheme> side immediately, while the program may still be producing
  output. If the user evaluates a new field before the program has closed
  the interrupted output, the new request is written to the program right
  away (since the status is not 0), and the late output and the
  <verbatim|DATA_END> of the interrupted computation are attributed to the
  new request.

  <subsection|Stopping and restarting>

  <menu|Close session> calls <scm|plugin-stop>, hence
  <cpp|connection_stop>, which stops the link (for pipes: termination of
  the process) and calls <cpp|connection_rep::listen>. The link being dead,
  <cpp|connection_rep::read> flushes the parsers (<cpp|eof>) and sets the
  status to <cpp|CONNECTION_DEAD>; the resulting notification of status 0
  cancels the pending requests with <scm|dead?> set to <scm|#t>, which
  replaces their busy tags by <markup|script-dead>. The same happens when
  the program exits or crashes on its own: the link sees an end of file on
  the standard output of the program.

  There is no explicit restart. The next evaluation in a dead session finds
  the status 0 in <scm|plugin-do>, which inserts a silent start request in
  front of it: the connection is started again with the same link object,
  the new banner is discarded, and the evaluation proceeds as usual.

  <subsection|Several sessions>

  Each pair (<em|lan>, <em|ses>) has its own connection, queue, prompt and
  author, and for pipes its own process. The names of the sessions are
  chosen by the user (<menu|Insert|Session|Other>) or are the variants
  declared by the plug-in; a session name of the form
  <verbatim|<em|variant>:<em|suffix>> uses the launcher of
  <em|variant> (see <scm|connection-info>), which allows several
  independent sessions of the same variant. Conversely, several
  <markup|session> tags with the same language and session name, in the
  same or in different documents, share a single connection and a single
  queue; this is how <menu|Split session> works. The dynamic environment
  variables <verbatim|prog-language> and <verbatim|prog-session>, set by the
  <markup|session> macro, tell the editing commands which connection the
  cursor is in.

  <section|Synchronous evaluation>

  The function <cpp|connection_eval> (the <scheme> function
  <scm|connection-eval>, used by <scm|plugin-eval>) bypasses the request
  queue:

  <\cpp-code>
    tree

    connection_eval (string name, string session, tree t) {

    \ \ connection con= connection_get (name, session);

    \ \ if (is_nil (con)) return "";

    \ \ connection_write (name, session, t);

    \ \ return connection_retrieve (name, session);

    }
  </cpp-code>

  <cpp|connection_get> starts the connection if it does not exist yet, with
  <cpp|again> set to <cpp|true> so that the banner is read and discarded.
  <cpp|connection_retrieve> then loops until the status is
  <cpp|WAITING_FOR_INPUT>: with <cpp|forced_eval> set, it calls
  <cpp|perform_select> (except in <name|Qt> builds, where the
  <name|Qt> pipe link reads directly from the process in its
  <cpp|read> method), reads the <verbatim|output> channel with
  <cpp|connection_read> and accumulates the result in a document. Only the
  default output channel is returned; error output is not.

  This loop runs in the main thread and does not return to the event loop,
  so the user interface is frozen until the program has answered, and there
  is no timeout. Since it bypasses the request queue, <scm|plugin-eval>
  should not be used on a connection which has asynchronous requests in
  progress. <cpp|connection_cmd> is the same for special commands: it
  formats the command with <scm|format-command> and unwraps a
  single-paragraph result.

  <section|Command line and request connections>

  The connections of plug-ins configured with <scm|:cmdline> or
  <scm|:request> (such as the <verbatim|ai> plug-in) follow a different
  pattern: there is no long-lived process, and each request is run
  separately.

  <\enumerate>
    <item>At start, <cpp|cmdline_link_rep::start> (resp.
    <cpp|request_link_rep::start>) returns the message
    <verbatim|"cmdline"> (resp. <verbatim|"request">), and
    <cpp|connection_rep::start> switches the output parser to the format
    <verbatim|cmdline-<em|name>> (resp. <verbatim|request-<em|name>>) and
    reports the connection as idle at once.

    <item>For each request, the link calls back the <scheme> function
    <scm|(connection-cmdline <em|name> "default" <em|input>)> (resp.
    <scm|connection-request>), which applies the <em|cmd-fun> (resp.
    <em|request-fun>) of the plug-in to obtain a shell command (resp. a
    request description), and launches it. For command line links, newlines
    in the input are replaced by spaces.

    <item>The output is only made available when the command has
    terminated: <cpp|cmdline_link_rep::read> returns nothing while the link
    is alive. When it dies, <cpp|connection_rep::read> calls <cpp|eof>, which
    flushes the whole output through <cpp|cmdline_flush> and
    <scm|(connection-result <em|name> "default" <em|output>)>, that is,
    through the <em|result-fun> of the plug-in, and the status becomes
    0. For these plug-ins, <scm|connection-notify-status> treats 0 as the
    end of the request.
  </enumerate>

  The string <verbatim|"default"> passed as the second argument is a
  constant: the session name is not passed to the plug-in functions.

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
