<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The link to the servers>

  This page describes <source-link|lsp_link.cpp|src/System/Link/lsp_link.cpp>:
  how the messages of a language server are framed over its pipe, and how
  they reach the <scheme> programs. The link does not know the protocol
  itself: it only cuts the bytes into messages.

  <section|Framing>

  Each message of the protocol is preceded by a header: lines
  <verbatim|Name: value> ended by <verbatim|CR LF>, then an empty line. Only
  <verbatim|Content-Length>, the length of the message in <em|bytes>, is
  used; the other fields (<verbatim|Content-Type>) are ignored, and the
  name of the field is compared without case. A header whose lines end by
  <verbatim|LF> alone (against the protocol) is read too, and a negative
  length is refused.

  <cpp|lsp_link_rep> is a <cpp|tm_link_rep> (<source-link|tm_link.hpp|src/System/Link/tm_link.hpp>)
  built on another link, the <em|transport>, which carries the bytes:
  <cpp|make_lsp_link (make_pipe_link (cmd))> for a program
  (<source-link|pipe_link.cpp|src/System/Link/pipe_link.cpp>, whose writes
  do not block, see below); a socket or a <name|Web Worker> could be used
  the same way. Its fields:

  <\description>
    <item*|<cpp|ln>>The transport.

    <item*|<cpp|buf>, <cpp|pos>>The bytes received which are not yet a
    whole message, from <cpp|pos> on. The part before <cpp|pos> is dropped
    once all of <cpp|buf> is used or once it reaches 64 KB, so that a large
    message which comes in many parts is not copied at each part.

    <item*|<cpp|msgs>, <cpp|first>>The whole messages which have not been
    read yet, from <cpp|first> on. A read only moves <cpp|first>; the array
    is emptied once all its messages are read, so that the messages are not
    copied at each read.

    <item*|<cpp|errbuf>>What the server wrote on its standard error, and the
    errors of the framing.

    <item*|<cpp|news>>Set when something has come (bytes, errors, the end of
    the server) since the last dispatch, see below.
  </description>

  <cpp|write (s, LINK_IN)> sends <cpp|lsp_frame (s)>, that is
  <verbatim|Content-Length: <em|n>\\r\\n\\r\\n> followed by <cpp|s>, to
  the transport, and then drains it: what the server sent while it was
  written to (<cpp|flush_input> reads the output of the child meanwhile)
  is in the buffers of the pipe, whose notifier will not tell it again,
  and would otherwise wait for the next notification.
  <cpp|drain> reads what the transport has received on <cpp|LINK_OUT> and
  <cpp|LINK_ERR>, and <cpp|parse> moves the whole messages from <cpp|buf>
  to <cpp|msgs>. Some servers print lines on their standard output by
  mistake; <cpp|parse> therefore splits the header at each <verbatim|LF>
  (not only at <verbatim|CR LF>) and passes the lines which are not
  <verbatim|Name: value> to <cpp|errbuf> (<verbatim|lsp: skipped '...'>),
  and a block without <verbatim|Content-Length> is skipped with an error
  (<verbatim|lsp: no Content-Length in '...'>) instead of blocking the
  link. The end of a header is the first <verbatim|CR LF CR LF>, or an
  <verbatim|LF LF> before it; once a <verbatim|CR LF CR LF> has been found,
  the <verbatim|LF LF> is only looked for in the header before it, so that
  the body of a long message which comes in many parts is not searched
  again at each part.

  <cpp|read (LINK_OUT)> returns the next whole message (or the empty
  string), <cpp|read (LINK_ERR)> returns and empties <cpp|errbuf>. <cpp|listen
  (msecs)> waits until a whole message, an error or the end of the server
  has come, for at most <cpp|msecs>: the bytes of a message may come in
  several parts, so it listens to the transport again until the message is
  complete. When the transport ends, the link is no longer
  <cpp|alive>.

  <section|Writes which do not block>

  A server which does not read its input (it is busy, or it waits until it
  can write its own output, which nobody reads while the editor waits)
  would block a write to its pipe once the pipe is full, and thus freeze
  the editor. <cpp|tm_link_rep::set_blocking_write> (<source-link|tm_link.hpp|src/System/Link/tm_link.hpp>)
  therefore lets a link write without blocking; by default (<cpp|true>)
  it waits until all is written, as the plug-ins expect, and
  <cpp|lsp_start> sets it to <cpp|false> for the pipe of each server.

  In <source-link|pipe_link.cpp|src/System/Link/pipe_link.cpp>, the input
  of the child is then non-blocking (<verbatim|O_NONBLOCK>), and a write
  appends to <cpp|inbuf> (from <cpp|inpos> on) what the child has not
  taken yet. <cpp|flush_input (msecs)> writes what the child takes, for at
  most <cpp|msecs>, and reads its output meanwhile; <cpp|write> calls it
  with 0, that is it tries once without waiting, so that a child which
  reads no more costs no wait at each write. What is left is written by
  <cpp|listen>, whenever the pipe can take more: the virtual
  <cpp|tm_link_rep::pending_write> tells that some input is not written
  yet, and <cpp|lsp_dispatch> calls <cpp|listen (0)> on the transport of
  each client which has some, then drains it, so that the rest goes at
  the next turns of the event loop. Once 64 KB of <cpp|inbuf> have been
  written, the part which is written is cut off, so that a long queue of
  messages to a slow server does not keep growing.
  The blocking writes of the plug-ins also retry a partial or interrupted
  write instead of losing the rest. With <name|Qt>, the pipe is a
  <cpp|QProcess> (<source-link|QTMPipeLink.cpp|src/Plugins/Qt/QTMPipeLink.cpp>),
  which keeps the rest itself: its flag <cpp|blocking> only tells
  <cpp|writeStdin> whether to wait with <cpp|waitForBytesWritten>.

  <section|The clients of the <scheme> programs>

  The second half of the file keeps a list <cpp|lsp_clients> of
  <cpp|lsp_client> records: an integer <cpp|id>, the link, a <scheme> call
  back, and two flags, <cpp|exited> (the call back got its
  <verbatim|"exit">) and <cpp|removed> (stopped by <cpp|lsp_stop>). The glue
  routines of <source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>
  give them to <scheme>:

  <\description>
    <item*|<scm|(lsp-start cmd call-back)>>Starts the program <scm|cmd> on a
    pipe which does not block the writes, and returns the <scm|id> of its
    client, or a string with the error if it cannot be started.

    <item*|<scm|(lsp-send id msg)>>Sends the message <scm|msg> (the
    <name|JSON> text, without header); <scm|#f> if the client is not
    alive, also when its link died while writing.

    <item*|<scm|(lsp-listen id msecs)>>Waits for at most <scm|msecs> until
    the server has sent something, then dispatches what has come to the call
    backs. This is how a request is waited for synchronously
    (<scm|lsp-request-wait>), and how the tests wait.

    <item*|<scm|(lsp-alive? id)>>Whether the server runs.

    <item*|<scm|(lsp-stop id)>>Stops the server at once (the
    <verbatim|shutdown> and <verbatim|exit> of the protocol are sent before,
    in <scheme>); its call back gets nothing more.

    <item*|<scm|(lsp-document-text t)>>The text of the source document
    <scm|t> (a tree), in <name|UTF-8>, for its server
    (<cpp|lsp_document_text>): its lines joined by <verbatim|LF>, each
    converted from Cork apart, a line with markup as its verbatim text on
    one line. It is the text of <scm|lsp-lines-\<gtr\>text> of
    <scm|lsp-lines>, without the <scheme> lists of the lines, which fill
    the heap for a long file.
  </description>

  The call back is called with a kind and a string:
  <scm|"message"> and the <name|JSON> text of one message,
  <scm|"stderr"> and what the server wrote on its standard error (with the
  errors of the framing), or <scm|"exit"> and <scm|""> once, when the
  server has ended and everything it sent has been given.

  <section|Dispatching>

  The command of the transport (<cpp|lsp_link_callback>, called by the
  pipe when data arrive) only <em|drains>: the bytes are framed and
  <cpp|news> is set, but no <scheme> code runs there. The call backs are
  called by <cpp|lsp_dispatch>, from two places:

  <\itemize>
    <item><cpp|process_all_lsp>, which the interpose handler of the server
    (<cpp|tm_server_rep::interpose_handler> in
    <source-link|tm_server.cpp|src/Texmacs/Server/tm_server.cpp>) calls at
    each turn of the event loop, with all the GUIs;

    <item><cpp|lsp_listen>, after waiting.
  </itemize>

  <cpp|lsp_dispatch> first writes the rest of the messages which the
  servers have not taken yet (the clients whose link has a
  <cpp|pending_write>, see above). It then goes over the clients which
  have <cpp|news> (or whose
  link has ended) and gives each one first its errors, then <em|one>
  message; it sets <cpp|news> again for a client which gave a message, and
  loops until no call back was called, or for about 100 ms at most: a
  server which sends without pause (progress, logs) does not keep the
  editor from its events, and the rest is dispatched at the next turns
  (the <cpp|news> of the clients stay). A client whose link has ended and
  whose errors are all given gets <scm|"exit"> and is marked
  <cpp|exited>.

  A call back may send, listen or stop in its turn. In particular a call
  back which waits for the answer of a request (<scm|lsp-request-wait>, for
  instance a handler which asks the server something) calls
  <cpp|lsp_listen>, and thus dispatches <em|within> the dispatch. To keep
  this safe:

  <\itemize>
    <item>a counter <cpp|lsp_dispatching> holds the depth of the nested
    dispatches;

    <item>no client is deleted during a dispatch: <cpp|lsp_stop> only marks
    it <cpp|removed>, and <cpp|process_all_lsp> deletes the clients which
    are <cpp|removed> or <cpp|exited>, with their links, only when
    <cpp|lsp_dispatching> is 0 (it can be called from an event loop which
    runs inside a call back);

    <item>clients which are started during a dispatch are added at the end
    of <cpp|lsp_clients>, so that the loop over the clients stays valid;

    <item>the <cpp|news> flag is per link, so that a dispatch does not read
    the links which have nothing new.
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
