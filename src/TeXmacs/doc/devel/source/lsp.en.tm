<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The client of language servers>

  <section|Introduction>

  <TeXmacs> is a client of the <em|Language Server Protocol>
  (<abbr|LSP>): for the source files which it edits, it starts the language
  servers which other editors use (<verbatim|clangd>, <verbatim|pylsp>,
  <verbatim|jdtls>...), gives them the text of the files, underlines the
  diagnostics which they publish, and asks them for completions,
  information at the cursor and definitions. The user level description is
  <hlink|language servers for source
  code|../../main/editing/man-language-servers.en.tm>.

  A server is a program which reads and writes <name|JSON-RPC> messages on
  its standard input and output, each message preceded by a header with its
  length. <TeXmacs> keeps the transport in <c++> and the protocol in
  <scheme>:

  <\description>
    <item*|The link>A <c++> link (<source-link|lsp_link.cpp|src/System/Link/lsp_link.cpp>)
    frames the messages over the pipe of the server, and gives the whole
    messages, one by one, to a <scheme> call back, from the interpose
    handler of the server (the event loop).

    <item*|The protocol>The <scheme> modules of
    <source-link|progs/lsp|TeXmacs/progs/lsp> build and read the messages,
    keep a client per language and project, send the requests and
    notifications, answer the requests of the servers, and keep the
    documents and the diagnostics.

    <item*|The editor>The module <source-link|lsp-edit.scm|TeXmacs/progs/lsp/lsp-edit.scm>
    follows the buffers through two hooks which the editor calls
    (<scm|notify-buffer-changed>, <scm|notify-buffer-closed>), shows the
    diagnostics as alternative selections which the editor underlines, and
    implements the commands of <menu|Tools|Language servers> and their
    keys.
  </description>

  A change of a followed source file thus goes as follows:

  <\verbatim-code>
    apply_changes (THE_TREE) \ --\<gtr\> notify-buffer-changed name

    \ \ --\<gtr\> lsp-follow-buffer \ --(idle 300 ms)--\<gtr\> lsp-sync-pending

    \ \ --\<gtr\> lsp-buffer-sync \ --\<gtr\> lsp-document-text (the text, in <c++>)

    \ \ --\<gtr\> textDocument/didChange (the part which differs, or the
    whole text)

    \ \ --\<gtr\> lsp-send \ --\<gtr\> "Content-Length: n\\r\\n\\r\\n{...}" on the pipe

    server --\<gtr\> pipe --\<gtr\> lsp_link_rep::drain \ --\<gtr\> process_all_lsp

    \ \ --\<gtr\> call back "message" \ --\<gtr\> lsp-client-receive

    \ \ --\<gtr\> textDocument/publishDiagnostics \ --\<gtr\> lsp-diagnostics-changed

    \ \ --\<gtr\> set-alt-selection "lsp-errors" ... \ --\<gtr\>
    update_diagnostic_rects
  </verbatim-code>

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|lsp_link.hpp|src/System/Link/lsp_link.hpp>,
    <source-link|lsp_link.cpp|src/System/Link/lsp_link.cpp>>The
    <abbr|LSP> link (framing of the messages over another link) and the
    clients of the <scheme> programs: <cpp|lsp_start>, <cpp|lsp_send>,
    <cpp|lsp_listen>, <cpp|lsp_alive>, <cpp|lsp_stop> and
    <cpp|process_all_lsp>; <cpp|lsp_document_text>, the text of a source
    document for its server.

    <item*|<source-link|tm_link.hpp|src/System/Link/tm_link.hpp>,
    <source-link|pipe_link.cpp|src/System/Link/pipe_link.cpp>,
    <source-link|QTMPipeLink.cpp|src/Plugins/Qt/QTMPipeLink.cpp>>The
    writes which do not block (<cpp|set_blocking_write>), for the pipes of
    the servers.

    <item*|<source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>>The
    glue routines <scm|lsp-start>, <scm|lsp-send>, <scm|lsp-listen>,
    <scm|lsp-alive?>, <scm|lsp-stop> and <scm|lsp-document-text>.

    <item*|<source-link|tm_server.cpp|src/Texmacs/Server/tm_server.cpp>>The
    interpose handler, which calls <cpp|process_all_lsp>.

    <item*|<source-link|lsp-base.scm|TeXmacs/progs/lsp/lsp-base.scm>>Messages
    as <scheme> trees, texts and positions, <abbr|URI>s of files.

    <item*|<source-link|lsp-client.scm|TeXmacs/progs/lsp/lsp-client.scm>>The
    registry of the servers, the clients and their life, requests and
    notifications, what the servers send.

    <item*|<source-link|lsp-documents.scm|TeXmacs/progs/lsp/lsp-documents.scm>>The
    documents of the servers, the diagnostics, the buffers and their
    projects.

    <item*|<source-link|lsp-edit.scm|TeXmacs/progs/lsp/lsp-edit.scm>>The
    servers in the editor: following the buffers, showing the diagnostics,
    completion, information at the cursor, definitions, the log page.

    <item*|<source-link|lsp-actions.scm|TeXmacs/progs/lsp/lsp-actions.scm>>The
    edits of the servers: formatting, renames, quick fixes.

    <item*|<source-link|lsp-navigate.scm|TeXmacs/progs/lsp/lsp-navigate.scm>>The
    pages of the references of a symbol and of the outline of a file.

    <item*|<source-link|lsp-decorate.scm|TeXmacs/progs/lsp/lsp-decorate.scm>,
    <source-link|lsp_decorations.cpp|src/System/Language/lsp_decorations.cpp>,
    <source-link|concat_text.cpp|src/Typeset/Concat/concat_text.cpp>>The
    colours of semantic tokens and the inlay hints: asked for in
    <scheme>, kept for the lines in <c++>, typeset by
    <cpp|typeset_lsp_string>.

    <item*|<source-link|lsp-kbd.scm|TeXmacs/progs/lsp/lsp-kbd.scm>>The mode
    <scm|in-lsp%>, the keys, the menu <scm|lsp-menu> and the focus bar.

    <item*|<source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>,
    <source-link|tools-menu.scm|TeXmacs/progs/texmacs/menus/tools-menu.scm>>The
    lazy loading of the modules, and the submenu <menu|Tools|Language
    servers>.

    <item*|<source-link|edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>,
    <source-link|edit_repaint.cpp|src/Edit/Interface/edit_repaint.cpp>>The
    hook <scm|notify-buffer-changed> and the underlines of the
    diagnostics.

    <item*|<source-link|new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>>The
    hook <scm|notify-buffer-closed>, and the font of a source file which is
    opened (<cpp|attach_subformat> sets <cpp|PROG_FONT> to the preference
    <verbatim|editor:code font>).

    <item*|<source-link|tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>,
    <source-link|preferences-widgets.scm|TeXmacs/progs/texmacs/menus/preferences-widgets.scm>>The
    preference <verbatim|editor:code font>: <scm|notify-code-font> gives
    its new value to the open source files, and the preferences offer the
    monospaced fonts.

    <item*|<source-link|footer-menu.scm|TeXmacs/progs/texmacs/menus/footer-menu.scm>>The
    status bar of a source file (<scm|footer-in-source?>): its language,
    without fonts and colors.

    <item*|<source-link|cursor.scm|TeXmacs/progs/utils/library/cursor.scm>>The
    default (empty) definitions of the hooks.

    <item*|<source-link|c-lang.scm|plugins/code/progs/c-lang.scm>,
    <source-link|code-format.scm|plugins/code/progs/code-format.scm>>The
    format <verbatim|c> of the <verbatim|.c> and <verbatim|.h> files and
    the syntax of the language <verbatim|c>.

    <item*|<source-link|lsp-test.scm|TeXmacs/progs/check/lsp-test.scm>>The
    test suite <verbatim|lsp>.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|The link to the servers|lsp-link.en.tm>

    <branch|The protocol in <scheme>|lsp-protocol.en.tm>

    <branch|Language servers in the editor|lsp-editor.en.tm>

    <branch|Tests and pitfalls of the language servers|lsp-pitfalls.en.tm>
  </traverse>

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
