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
    header), and a call back which waits for an answer within a dispatch.
    These tests are skipped on <name|Windows>;

    <item>the messages, texts, positions and <abbr|URI>s of
    <verbatim|lsp-base>;

    <item>the clients, on procedures which record what is sent and are given
    the answers (initialization, requests, requests of the servers, the end
    of a server), the documents and the diagnostics;

    <item>a server which ends at once (<verbatim|sh -c 'exit 0'>) is not
    started again until it is forgotten;

    <item>a C file of a throwaway project (with a
    <verbatim|compile_flags.txt>) in the temporary directory: its format,
    language, project, lines and cursor position.
  </itemize>

  When <verbatim|clangd> is installed, the suite also runs it on that
  project and tests the editor on a <c++> file: the buffer followed, the
  diagnostics underlined (the alternative selection
  <verbatim|lsp-errors>), <key|F8>, the definition, the completion
  (synchronous and asynchronous), the log page and its refresh, the hover
  balloon, the <verbatim|didChange>, <verbatim|didSave> and
  <verbatim|didClose> sent when the buffer is saved and closed, and the
  servers stopped by <scm|lsp-stop-servers>.

  The suite runs <em|after> <verbatim|kbd-menu> in <scm|check-master>:
  editing a <c++> buffer loads the keyboard maps of programs, and the
  checks of <verbatim|kbd-menu> which type mathematics must come before
  that (see the comment of <scm|test-typing-math-mode> in
  <source-link|kbd-menu-test.scm|TeXmacs/progs/check/kbd-menu-test.scm>:
  once those maps are loaded, without a window, the first key after
  <key|$> in text still sees the text mode).

  <section|Pitfalls>

  <\itemize>
    <item><em|No mode predicates in <scm|notify-cursor-moved>, nor in the
    <scm|:require> of <scm|kbd-variant>.> These run while the editor applies
    its changes (<cpp|apply_changes>), when the environment at the cursor is
    not up to date: <scm|in-prog?>, <scm|in-math?>... then answer for the
    former environment. Calling one there once made <scm|in-math?> wrong
    after <scm|(make 'math)>. The code therefore asks whether the current
    buffer is followed (the table <verbatim|followed>) and looks at the
    cursor path and the text of the line instead
    (<scm|lsp-completion-here?>, <scm|lsp-cursor-moved>).

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

    <item><em|Diagnostics refer to an older text.> They are shown as they
    are until new ones come; the editor skips the ranges which are no
    longer valid paths, and <scm|lsp-show-diagnostics> leaves out those
    beyond the last line.

    <item><em|A server which has ended stays ended.> Its entry is kept so
    that it is not restarted at each pause; after changing
    <scm|lsp-server-set!> or installing a server, use <menu|Restart server>
    (or <scm|lsp-client-forget>). The table <verbatim|followed> also caches
    the buffers which have no server: <scm|lsp-restart> empties it.

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
