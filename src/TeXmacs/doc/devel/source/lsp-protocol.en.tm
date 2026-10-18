<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The protocol in <scheme>>

  The protocol is implemented by three modules of
  <source-link|progs/lsp|TeXmacs/progs/lsp>, which do not depend on the
  editor (the tests run them on fake servers): <verbatim|lsp-base> for the
  data, <verbatim|lsp-client> for the clients, <verbatim|lsp-documents> for
  the documents, the diagnostics and the buffers.

  <section|Messages, texts and positions>

  <paragraph*|Messages>

  In <source-link|lsp-base.scm|TeXmacs/progs/lsp/lsp-base.scm>, a message
  is the <scheme> tree (stree) of the <name|JSON> converter
  <scm|json-\<gtr\>typed-tree> (and back with <scm|typed-tree-\<gtr\>json>,
  both from <source-link|json.cpp|src/Data/Convert/AI/json.cpp>):

  <\itemize>
    <item>an object is <scm|(attr <em|key> <em|value> ...)>, an array
    <scm|(tuple ...)>;

    <item>a number is <scm|(json-number "1")>, a boolean
    <scm|(json-boolean "true")>, null <scm|(json-null)>;

    <item>a string is a <scheme> string, in <name|UTF-8>.
  </itemize>

  The <em|typed> variant of the converter keeps numbers apart from strings,
  which matters since the answer to a request carries the <verbatim|id> of
  the request, which may be a number or a string. <scm|lsp-read> and
  <scm|lsp-write> convert between text and stree (<scm|lsp-read> gives
  <scm|#f> if the conversion fails; the converter accepts almost anything,
  so <scm|lsp-client-receive> also rejects what is not an object). The constructors <scm|lsp-object>,
  <scm|lsp-array>, <scm|lsp-list-\<gtr\>array>, <scm|lsp-number>,
  <scm|lsp-boolean>, <scm|lsp-null> build messages, and <scm|lsp-ref>
  reads them: <scm|(lsp-ref msg "params" "items" 0)> follows keys and
  indices and gives <scm|#f> when one is missing. <scm|lsp-items>,
  <scm|lsp-number-value>, <scm|lsp-true?> and <scm|lsp-null?> complete
  them.

  <paragraph*|Texts and positions>

  A text is a list of lines, in the Cork encoding of <TeXmacs> documents: a
  source file is a document with one line per paragraph, so that
  <scm|lsp-lines> gives the lines of a body (markup inside a line is
  converted with <scm|texmacs-\<gtr\>verbatim>). The server gets the text
  in <name|UTF-8>: <scm|lsp-lines-\<gtr\>text> joins converted lines, and
  <scm|lsp-text-\<gtr\>lines> splits a text of a server. The text of a
  buffer, though, is built in <c++> (<scm|lsp-document-text>, see
  <hlink|the link|lsp-link.en.tm>), without a <scheme> list of its lines:
  for a long file, such lists at each pause of the typing filled the heap.
  <scm|lsp-lines-\<gtr\>text> remains for the tests and the texts which
  are not buffers. <scm|(lsp-line lines i)> gives a line of a list or of a
  vector (<scm|""> if there is none): a vector when many lines of a long
  file are looked for.

  A position of the protocol is a line and a <em|character>, counted in
  <name|UTF-16> units, the default encoding of the protocol (and the only
  one announced in the capabilities of the client): two units for the
  characters beyond <verbatim|U+FFFF>. The editor counts offsets in the
  Cork string of the line. <scm|lsp-offset-\<gtr\>character> and
  <scm|lsp-character-\<gtr\>offset> convert between them (a character in
  the middle of a Cork character goes to its start, one beyond the end of
  the line to its end; in a line whose characters are all of one byte and
  one unit, without the <verbatim|\<less\>> of the special characters of
  Cork, <scm|lsp-character-\<gtr\>offset> gives the character itself, and the last line asked about is remembered, since
  the tokens of a line come together), and <scm|lsp-position> and
  <scm|lsp-position-\<gtr\>line+offset> convert whole positions.

  <paragraph*|URIs>

  <scm|lsp-path-\<gtr\>uri> turns a system path into a
  <verbatim|file://> <abbr|URI>, encoding with <verbatim|%XX> every byte
  which is not a letter, a digit or one of <verbatim|-._~/:>; backslashes
  become slashes, and a drive letter is written in capitals.
  <scm|lsp-uri-\<gtr\>path> decodes, and removes the slash before a drive
  letter (<verbatim|file:///c:/x> on <name|Windows>), which it writes in
  capitals too (<verbatim|c:> and <verbatim|C:> are the same drive, and
  servers write either: both directions give the same form). It also accepts <verbatim|file:/x>,
  without the authority, as the same file as <verbatim|file:///x>, and
  keeps a <verbatim|%> which is not followed by two hexadecimal digits as
  it is.

  <section|Clients>

  <paragraph*|The servers>

  <source-link|lsp-client.scm|TeXmacs/progs/lsp/lsp-client.scm> keeps a
  table from languages (the <verbatim|prog-language>, which is also the
  name of the format) to lists of commands, filled with
  <scm|lsp-server-set!>. <scm|lsp-server-command> gives the first command
  whose program (its first word) is found: in the <verbatim|PATH>, or as a
  file if it contains a slash or a backslash (a <name|Windows> path).
  <scm|lsp-server-known?> tells whether a
  language has commands at all, installed or not.

  <paragraph*|The life of a client>

  A client is a hash table with its name (the command), its state, three
  procedures <verbatim|send>, <verbatim|listen> and <verbatim|stop>, the
  counter of the requests, the handlers of the pending requests, a queue,
  the capabilities of the server, its root directory, a log and its open
  documents. <scm|lsp-client-make> builds one on any procedures: the tests
  use procedures which record what is sent, and
  <scm|lsp-client-start> uses those of the link (<scm|lsp-send>,
  <scm|lsp-listen>, <scm|lsp-stop>), with a call back which passes to
  <scm|lsp-client-receive>.

  The states are:

  <\description>
    <item*|<verbatim|new>>The client has been made (<scm|lsp-client-make>),
    and nothing has been sent yet.

    <item*|<verbatim|initializing>>The request <verbatim|initialize> has
    been sent (<scm|lsp-client-initialize>), with the root as
    <verbatim|rootUri> and as the only workspace folder, and the
    capabilities of the client (<verbatim|client-capabilities>):
    <name|UTF-16> positions; <verbatim|didSave>; diagnostics with
    <verbatim|versionSupport>; hovers in plain text or markdown; completion
    without snippets; definitions, <verbatim|references>,
    <verbatim|documentHighlight> and <verbatim|callHierarchy>;
    <verbatim|foldingRange> with <verbatim|lineFoldingOnly>;
    <verbatim|documentSymbol> with
    <verbatim|hierarchicalDocumentSymbolSupport>;
    <verbatim|semanticTokens> (<verbatim|full> requests, relative format,
    the standard token types); <verbatim|inlayHint>;
    <verbatim|formatting> and <verbatim|rangeFormatting>;
    <verbatim|rename> (without <verbatim|prepareSupport>);
    <verbatim|signatureHelp> with <verbatim|labelOffsetSupport>;
    <verbatim|codeAction> with code action literals (quick fixes,
    refactorings, sources); and, for the workspace, configuration,
    <verbatim|symbol>, workspace folders, <verbatim|applyEdit>,
    <verbatim|workspaceEdit> with <verbatim|documentChanges> and
    <verbatim|executeCommand>; progress (<verbatim|workDoneProgress>). The
    answers to the requests of the server are sent at once (a server may
    ask before it answers); every other message waits in the queue, where
    a <verbatim|didChange> with the whole text of a document replaces those
    of the same document which wait (the queue of a server which is slow to
    start does not grow by a copy of the file at each pause). A server
    which has not answered after <scm|lsp-initialize-timeout> (120 s) is
    killed by <scm|lsp-client-check-initialized>, with a line in its log.

    <item*|<verbatim|running>>The answer has come: its capabilities are
    kept, <verbatim|initialized> is sent and then the queue. An error
    answer to <verbatim|initialize> kills the client.
    <scm|(lsp-client-capable? c name)> tells whether the server has a
    capability (a <verbatim|...Provider>): a capability which is absent,
    null or given as <verbatim|false> counts as absent.

    <item*|<verbatim|stopping>><scm|lsp-client-stop> has sent
    <verbatim|shutdown> (waited for at most 2 s) and <verbatim|exit>; the
    server is given 1 s more to end, then it is killed.

    <item*|<verbatim|dead>>The server has ended, has been killed
    (<scm|lsp-client-kill>), or could not be started (its log says why). The
    handlers of the requests which were not answered are called with an
    error (code <verbatim|-32099>). If the server ended without being
    stopped, the hook <scm|lsp-client-ended> is called, which the editor
    redefines to show a message.
  </description>

  <paragraph*|Requests and notifications>

  <scm|(lsp-request c method params handler)> sends a request with a new
  numeric <verbatim|id> and returns that <verbatim|id>; the
  <scm|handler> is called with the result and the error of the answer, one
  of them <scm|#f>. <scm|lsp-notify> sends a notification,
  <scm|lsp-cancel> forgets the handler of a request and sends
  <verbatim|$/cancelRequest>; a request which still waits in the queue (the
  server is initializing) is removed from it instead, and never sent.
  <scm|lsp-wait> listens until a condition
  holds or a delay has passed, and <scm|lsp-request-wait> sends a request
  and waits for its answer, which is <scm|(result <em|r>)>,
  <scm|(error <em|e>)> or <scm|(timeout)> (the request is then
  cancelled).

  <paragraph*|What the servers send>

  <scm|lsp-client-handle> sorts the messages: a message with a method and an
  <verbatim|id> is a request of the server, with a method only a
  notification, with an <verbatim|id> only the answer to one of our
  requests. An answer with <verbatim|"error": null> is no error, and an
  answer whose <verbatim|id> is null (an error of the server about a
  message which it could not read) goes to the log. The requests of the
  servers are answered by the procedures of
  <scm|lsp-server-request-set!> (method <math|\<rightarrow\>> procedure of
  the client and the parameters, whose value is the result; an unknown
  method gets the error <verbatim|-32601>, a procedure which fails
  <verbatim|-32603>). The notifications go to the procedures of
  <scm|lsp-notification-set!>, several per method, each one under a key so
  that it can be redefined. By default <TeXmacs> answers
  <verbatim|workspace/configuration> (null for each item),
  <verbatim|workspace/workspaceFolders>, <verbatim|window/workDoneProgress/create>,
  <verbatim|client/registerCapability>, <verbatim|client/unregisterCapability>
  and <verbatim|window/showMessageRequest>, and logs
  <verbatim|window/logMessage> and <verbatim|window/showMessage>.
  <source-link|lsp-actions.scm|TeXmacs/progs/lsp/lsp-actions.scm> answers
  <verbatim|workspace/applyEdit>: the edit is refused (<verbatim|applied>
  <verbatim|false>, <verbatim|failureReason> <verbatim|the file has
  changed>) when <scm|lsp-workspace-edit-stale?> holds, that is when the
  <verbatim|version> of one of its <verbatim|TextDocumentEdit>s differs
  from the version of the document which the client gave
  (<scm|lsp-document-version>), or when the buffer of that document has
  changed since its text was given (<scm|lsp-document-sent-text>).

  The log of a client keeps its last 200 entries, each with a kind
  (<verbatim|error>, <verbatim|stderr>, <verbatim|log>,
  <verbatim|message>); <scm|lsp-client-log-changed> is called at each
  change of the log or of the state, for the log page of the editor.

  <paragraph*|The clients of the languages and projects>

  The clients are kept in a table under their language and root.
  <scm|lsp-client-for> returns the client of a language and root, and
  starts one if there is none; <scm|lsp-client-find> returns the client
  only if it is alive (in the state <verbatim|new>, <verbatim|initializing>
  or <verbatim|running>, see <scm|lsp-client-alive?>) and never starts
  one. A client is kept in the table once its server has ended (stopped,
  crashed, or which could not start), and <scm|lsp-client-for> then gives
  <scm|#f>: a server is <em|not started again> until its entry is
  forgotten (<scm|lsp-client-forget>, <scm|lsp-forget-all>), which is what
  <menu|Restart server> does. Otherwise a server which ends at once would
  be started again at each pause of the typing. <scm|lsp-client-entry>
  gives the entry also when its server has ended (for its log).

  <scm|lsp-stop-all> stops all the running servers together: it sends
  <verbatim|shutdown> to each one, <verbatim|exit> to each one which
  answers, and kills those which have not ended after 2 s. Their entries
  are kept, so they are not started again.

  <section|Documents, diagnostics and buffers>

  <paragraph*|Documents>

  <source-link|lsp-documents.scm|TeXmacs/progs/lsp/lsp-documents.scm> keeps,
  per client, the open documents by <abbr|URI>, each with its version, its
  last text and its language id. <scm|lsp-document-open> sends
  <verbatim|textDocument/didOpen> (or changes a document which is open
  already); <scm|lsp-document-change> sends
  <verbatim|textDocument/didChange> with a higher version, only if the text
  has changed and if the server wants changes (its
  <verbatim|textDocumentSync> is not 0). If the server takes incremental
  changes (kind 2) and is running, only the part which differs is sent;
  otherwise (kind 1, a server which does not say, or one which is still
  initializing) the <em|whole> new text, a change without a range.
  <scm|lsp-text-change> finds the part: the old and the new text have a
  common start and a common end (in bytes of <name|UTF-8>, compared by
  blocks of 4 KB first, then moved back to the boundaries of characters),
  and what lies between them in the old text is replaced by what lies
  between them in the new one, its range given as positions (line,
  <name|UTF-16> units, the lines counted with <scm|string-search-forwards>).
  Each change also calls the hook <scm|(lsp-document-lines-changed uri
  first last added)>, with the lines <scm|first> to <scm|last> of the old
  text which became <scm|added>+1 lines (<scm|lsp-text-lines-change>): the
  editor moves the diagnostics which it shows.

  <scm|lsp-document-save> sends <verbatim|textDocument/didSave> if the
  server asks for it, with the text if it asks for
  <verbatim|includeText>; a <verbatim|textDocumentSync> which is a number
  (a kind of change alone) gets the saves too, without the text, as other
  clients do. <scm|lsp-document-close> sends
  <verbatim|textDocument/didClose> and forgets the diagnostics of the
  document. A server whose <verbatim|openClose> is <verbatim|false> gets
  neither <verbatim|didOpen> nor <verbatim|didClose> (its documents are
  still kept).

  <paragraph*|Diagnostics>

  The notification <verbatim|textDocument/publishDiagnostics> replaces the
  diagnostics kept for its <abbr|URI> and calls the hook
  <scm|lsp-diagnostics-changed>, unless it has a <verbatim|version> older
  than the one of the document which the client has given: those
  diagnostics are for an older text, and the ones of the present text come
  after them, so they are ignored. The <abbr|URI>s are kept as <TeXmacs>
  writes them (<verbatim|uri-key>: decoded and encoded again), since a
  server may write the same file otherwise (<verbatim|%c3%a9> for
  <verbatim|%C3%A9>, <verbatim|%3A> for <verbatim|:>).
  <scm|lsp-forget-diagnostics> forgets those of a document (and calls the
  hook): of a document which is closed, and of all the documents of a
  client which is <verbatim|dead> (through
  <scm|lsp-client-status-changed>), so that they do not stay underlined,
  also after a restart. <scm|lsp-diagnostics>,
  <scm|lsp-diagnostic-severity> (<scm|error>, <scm|warning>, <scm|info>,
  <scm|hint>; error by default), <scm|lsp-diagnostic-message> (in Cork) and
  <scm|lsp-diagnostic-range> (as line, offset, line, offset in given lines)
  read them.

  <paragraph*|Buffers>

  The rest of the module links buffers to servers.
  <scm|lsp-buffer-language> gives the language of a buffer: its format if a
  server is known for it (<verbatim|cpp>, <verbatim|c>,
  <verbatim|python>...), or else, for the current buffer in the mode
  <verbatim|prog>, its <verbatim|prog-language>. <scm|lsp-buffer-path>
  gives its file (not for <verbatim|tmfs> buffers), and
  <scm|lsp-project-root> its project: the nearest directory, from the
  directory of the file up (at most 64 levels), which contains
  <verbatim|.git>, <verbatim|compile_commands.json>,
  <verbatim|compile_flags.txt>, <verbatim|pyproject.toml>,
  <verbatim|setup.py>, <verbatim|package.json>, <verbatim|Cargo.toml>,
  <verbatim|go.mod>, <verbatim|build.sbt> or <verbatim|pom.xml>, else the
  directory of the file. The root is asked for at each key, so it is kept
  per directory (<scm|lsp-forget-project-roots> looks for them again).
  <scm|lsp-language-id> gives the
  <verbatim|languageId> of the protocol (<verbatim|typescript> for the
  <verbatim|.ts> files of the language <verbatim|javascript>...).

  <scm|lsp-buffer-client> gives the client of a buffer and starts it if
  needed, <scm|lsp-buffer-client*> only a live one which exists already
  (<scm|lsp-client-find>); <scm|lsp-buffer-open>, <scm|lsp-buffer-sync> and
  <scm|lsp-buffer-close> open, update and close the document of a buffer,
  with its text from <scm|lsp-buffer-text> (<scm|lsp-document-text> of its
  body), and <scm|lsp-cursor-position> gives the position of the cursor in
  the protocol. All of them take the current buffer by default.

  The requests at the cursor read one line only, not all the lines of the
  buffer: <scm|lsp-buffer-line> gives a line of the buffer in Cork,
  <scm|lsp-buffer-position> the position of the protocol at a line and an
  offset of the buffer, and <scm|lsp-buffer-line+offset> the line and the
  offset of a position. <scm|lsp-buffer-lines> (all the lines, as a list)
  is left for the diagnostics which are shown, which take them as a
  vector.

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
