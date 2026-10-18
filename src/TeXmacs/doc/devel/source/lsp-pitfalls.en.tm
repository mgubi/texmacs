<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Tests and pitfalls of the language servers>

  <section|The test suite>

  The suite <verbatim|lsp> is
  <source-link|lsp-test.scm|TeXmacs/progs/check/lsp-test.scm>
  (<scm|lsp-test-failures>), run with the other suites of
  <source-link|check-master.scm|TeXmacs/progs/check/check-master.scm> (see
  <hlink|the tests|build-tests.en.tm>). Most of it needs no language
  server:

  <\itemize>
    <item>the link is tested with ordinary commands of the shell as
    servers: <verbatim|cat> sends back the framed messages which it gets,
    and <verbatim|printf> writes the bytes of a server, headers included
    (several messages at once, headers in lower case or with a
    <verbatim|Content-Type>, a message in several parts, lines without a
    header), a call back which waits for an answer within a dispatch, and a
    server which does not read its input (<verbatim|sleep>: a long message
    must not block <scm|lsp-send>). These tests are skipped on
    <name|Windows>;

    <item>the messages, the texts and the <name|UTF-16> offsets (lines of
    one byte per character, which take a shorter path, and the others), and
    the <abbr|URI>s of <verbatim|lsp-base> (drive letters, <verbatim|file:/x>,
    a <verbatim|%> without hexadecimal digits);

    <item>the clients, on procedures which record what is sent and are given
    the answers: initialization, a server which is slow to start or never
    answers <verbatim|initialize>, the state of the diagnostics of the
    servers, the options of <verbatim|textDocumentSync>;

    <item>the reading of answers: the entries of an outline, folding
    ranges, the decoding of semantic tokens and their colours, the labels
    of inlay hints and their lines in Cork offsets;

    <item>points of the protocol: capabilities given as <verbatim|false>, a
    request cancelled during the initialization (never sent), diagnostics
    of an older version (ignored), stale workspace edits
    (<scm|lsp-workspace-edit-stale?>);

    <item>requests, requests of the servers, the end of a server;

    <item>the edits of the servers (<scm|lsp-apply-to-lines>,
    <scm|lsp-move-through-edits>): edits with the same start, overlapping
    or beyond the text, the cursor after a longer line, incremental changes
    and the lines which they change, workspace edits; the parts of a
    signature (with offsets in <name|UTF-16> units, and a parameter looked
    for after the name of the function), the progress of a server, the
    point under the mouse;

    <item>the documents and the diagnostics;

    <item>a server which ends at once (<verbatim|sh -c 'exit 0'>) is not
    started again until it is forgotten;

    <item>a C file of a throwaway project (with a
    <verbatim|compile_flags.txt>) in the temporary directory: its format,
    language, project, lines and cursor position (the group
    <verbatim|buffer>), and the decorations and folds of its lines, without
    a server (<verbatim|test-lines-of-buffer>: a line which is edited, moved
    or removed; a fold in a fold which stays folded when the outer one
    goes).
  </itemize>

  When <verbatim|clangd> is installed, the suite also runs it on that
  project (hover, diagnostics, the client of a buffer) and tests the editor
  on a <c++> file: the buffer followed, the diagnostics underlined (the
  alternative selection <verbatim|code-errors>), <key|F8>, the definition,
  the completion (synchronous and asynchronous), the log page and its
  refresh, the hover balloon, the <verbatim|didChange>, <verbatim|didSave>
  and <verbatim|didClose> sent when the buffer is saved and closed, and the
  servers stopped by <scm|lsp-stop-servers>. On a third file, it tests the
  actions: the page of the diagnostics, the focus bar, a quick fix, the
  signature, the references and the outline, the highlights, folding and
  unfolding a block, the symbols of the project and the calls, the
  colours and hints, a rename and the formatting. On a document of the
  project, it tests the code blocks (the group <verbatim|code blocks>,
  turned on by <scm|lsp-blocks-force!>, not by the preference): their
  virtual files, the target at the cursor, their diagnostics at their
  places, the definition in a block, a paragraph inserted above them, a
  block removed and one whose language changes, and a place in a block of
  a closed document.

  The suite runs <em|after> <verbatim|kbd-menu> in <scm|check-master>:
  editing a <c++> buffer loads the keyboard maps of programs, and the
  checks of <verbatim|kbd-menu> which type mathematics must come before
  that (see the comment of <scm|test-typing-math-mode> in
  <source-link|kbd-menu-test.scm|TeXmacs/progs/check/kbd-menu-test.scm>:
  once those maps are loaded, without a window, the first key after
  <key|$> in text still sees the text mode).

  <section|Pitfalls>

  <\itemize>
    <item><em|No mode predicates in <scm|notify-cursor-moved>, in the
    <scm|:require> of <scm|kbd-variant>, nor in the redefined
    <scm|mouse-event> and <scm|keyboard-press>.> These run while the editor
    applies its changes (<cpp|apply_changes>), or between a change and the
    next typesetting, when the environment at the cursor is not up to date:
    <scm|in-prog?>, <scm|in-math?>... then answer for the former
    environment. Calling one there once made <scm|in-math?> wrong after
    <scm|(make 'math)>. The code therefore asks whether the current buffer
    is followed (the table <verbatim|followed>, <scm|lsp-buffer-followed?>)
    and looks at the cursor path and the text of the line instead
    (<scm|lsp-completion-here?>, <scm|lsp-cursor-moved>). The mode
    <scm|in-lsp%> of the keys is another matter: as any mode, it is
    evaluated when a key is looked up.

    <item><em|Hooks with <scm|:require> or <scm|:mode> must call
    <scm|former>.> A hook such as <scm|notify-cursor-moved> is redefined by
    several modules: the language servers (<verbatim|lsp-edit>,
    <verbatim|lsp-navigate>) call <scm|former> and have
    no condition, but a definition with a <scm|:require> or a <scm|:mode>
    which holds replaces the definitions loaded before it. If it does not
    call <scm|former>, those are skipped: without it, the highlights of
    brackets of <source-link|cpp-edit.scm|TeXmacs/progs/prog/cpp-edit.scm>
    keep the language servers from seeing the moves of the cursor in a
    <c++> file (when they were loaded before it). The definitions of
    <scm|notify-cursor-moved> in <verbatim|prog/*-edit.scm> now call
    <scm|(former status)> first.

    <item><em|Strings compared with <scm|eq?> under <name|S7>.> A cache
    which remembers the last string by <scm|eq?> is only safe while it
    keeps a reference to that string: a string which is freed may have its
    address taken by a new one, which would then be taken for it. The
    same holds for the trees of the lines which the typesetter knows by
    their addresses: <verbatim|lsp-decorate> keeps them (its table
    <verbatim|decorated>) while they are decorated.

    <item><em|<scm|tree-bounding-rectangle> needs an up-to-date
    typesetting.> <scm|show-tooltip> places the balloon of a hover with the
    bounding rectangle of the tree at the cursor, which only exists once
    that tree has been typeset. In the event loop this is the case before
    each key; a script (or a test) which moves the cursor and then calls
    <scm|lsp-hover> must call <scm|update-forced> first.

    <item><em|Positions are in <name|UTF-16> units, lengths in bytes.> The
    <verbatim|Content-Length> of a header counts the bytes of the
    <name|UTF-8> text, the positions count <name|UTF-16> units, and the
    editor counts the characters of Cork strings: always go through the
    conversions of <verbatim|lsp-base>.

    <item><em|Diagnostics refer to an older text.> Until new ones come, the
    diagnostics shown are moved with the lines which a change moves
    (<scm|lsp-document-lines-changed>), and those in the changed lines are
    dropped; the editor drops the ranges which are no longer valid paths
    before it looks for the visible ones (a binary search, which needs the
    ranges in the order of the text), and <scm|lsp-show-diagnostics> leaves
    out those beyond the last line. The diagnostics are kept by normalized
    <abbr|URI> (a server may encode a path otherwise), and forgotten when
    their document is closed or their server ends.

    <item><em|A server which has ended stays ended.> Its entry is kept so
    that it is not restarted at each pause; after changing
    <scm|lsp-server-set!> or installing a server, use <menu|Restart server>
    (or <scm|lsp-client-forget>). The table <verbatim|followed> also caches
    the buffers which have no server: <scm|lsp-restart> empties it and
    follows all the open buffers again. The project roots are cached per
    directory too: after adding a <verbatim|.git> or a
    <verbatim|compile_commands.json>, call <scm|lsp-forget-project-roots>.

    <item><em|A server may not read its input.> A write to a full pipe
    would block the editor: the pipes of the servers write without blocking
    (<cpp|set_blocking_write>, see <hlink|the link|lsp-link.en.tm>), and a
    dispatch takes at most about 100 ms. A server which never answers
    <verbatim|initialize> is ended after <scm|lsp-initialize-timeout>; until
    then, the whole texts which wait for it replace each other.

    <item><em|Long files.> A source file of 100000 lines is a document of
    100000 paragraphs, and a <scheme> list of its lines at each pause of the
    typing, or <scm|list-ref> on such a list for each position, costs much
    time and memory. The server is therefore given a snapshot of the lines
    in <c++> (<scm|code-snapshot-new>, <scm|code-snapshot-change>), whose
    text is made only when it is asked for, the requests at the cursor read one line
    (<scm|lsp-buffer-line>), the edits and the diagnostics use vectors
    (<scm|lsp-line>), and the texts are compared by blocks. Converting a
    whole text between Cork and <name|UTF-8> at once does not work either:
    a newline is a character of its own in Cork, so the texts are converted
    line by line. <scm|string-recompose> (in
    <source-link|base.scm|TeXmacs/progs/kernel/library/base.scm>) is linear
    (it was quadratic: gigabytes for such a file), and under <name|S7>
    <source-link|s7_tm.cpp|src/Scheme/S7/s7_tm.cpp> sets
    <verbatim|gc-resize-heap-by-4-fraction> to 0.5: the heap is quadrupled
    only when less than half of it is free after a collection, since a heap
    never shrinks.

    <item><em|Edit the plug-in in <source-link|src/plugins|plugins>.> The format and
    language <verbatim|c> live in <source-link|c-lang.scm|plugins/code/progs/c-lang.scm>
    and <source-link|code-format.scm|plugins/code/progs/code-format.scm>;
    <verbatim|TeXmacs/plugins> is a copy made by the build.
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
