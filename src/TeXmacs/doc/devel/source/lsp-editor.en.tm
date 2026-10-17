<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Language servers in the editor>

  This page describes <source-link|lsp-edit.scm|TeXmacs/progs/lsp/lsp-edit.scm>
  and <source-link|lsp-kbd.scm|TeXmacs/progs/lsp/lsp-kbd.scm>, and the hooks
  of the editor which they use.

  <section|Loading>

  <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> declares
  <scm|(lazy-keyboard (lsp lsp-kbd) in-prog?)> and <scm|(lazy-menu (lsp
  lsp-kbd) lsp-menu)>, and, when the preference <verbatim|lsp> is not
  <verbatim|off>, loads <verbatim|(lsp lsp-kbd)> after an idle second, so
  that the source files are followed without a key or the menu being used
  first. <verbatim|lsp-kbd> uses <verbatim|lsp-edit>, which uses the other
  modules. The menu is linked as <menu|Tools|Language servers> in
  <source-link|tools-menu.scm|TeXmacs/progs/texmacs/menus/tools-menu.scm>.
  When <verbatim|lsp-edit> is loaded, a delayed command follows the buffers
  which were loaded before it.

  The preference <verbatim|lsp> (default <verbatim|on>, read by
  <scm|lsp-on?>) is toggled by <menu|Follow source files>; setting it to
  <verbatim|off> calls <scm|lsp-turn-off>, which stops and forgets all the
  servers and hides the diagnostics.

  <section|The hooks of the editor>

  Two hooks were added to the editor for the programs which follow the text
  of buffers; their default definitions, which do nothing, are in
  <source-link|cursor.scm|TeXmacs/progs/utils/library/cursor.scm>, next to
  <scm|notify-cursor-moved>:

  <\description>
    <item*|<scm|(notify-buffer-changed name)>>Called by
    <cpp|edit_interface_rep::apply_changes>
    (<source-link|edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>)
    when <cpp|env_change> contains <cpp|THE_TREE>, with the name of the
    buffer of the editor as a string, after the typesetting. It is thus
    called once per batch of changes, not once per modification.

    <item*|<scm|(notify-buffer-closed name)>>Called by <cpp|remove_buffer>
    in <source-link|new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp> before
    the views of the buffer are deleted, and by <cpp|rename_buffer> with
    the <em|former> name (<menu|File|Save as>).
  </description>

  <verbatim|lsp-edit> redefines both (with <scm|former>), as well as
  <scm|version-notify-saved> (called after a buffer is saved, in
  <source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>; the
  redefinition of <source-link|version-git.scm|TeXmacs/progs/version/version-git.scm>
  passes it on) and <scm|notify-cursor-moved>.

  <section|Following the buffers>

  A table <verbatim|followed> remembers, by buffer name, whether a server
  follows the buffer: the first time, <verbatim|follow?> checks that the
  buffer has a file, a language (<scm|lsp-buffer-language>) and an
  installed server (<scm|lsp-server-command>). <scm|notify-buffer-changed>
  calls <scm|lsp-follow-buffer>, which puts a followed buffer in the table
  <verbatim|pending> and schedules <scm|lsp-sync-pending> once, with
  <scm|(delayed (:idle 300) ...)>: the texts are given to the servers
  (<scm|lsp-buffer-sync>, which opens the document the first time, and
  starts the server if needed) only when the user pauses. Since the loading
  of a buffer is a change of its tree, the same path opens the document of
  a file which has just been loaded.

  <scm|version-notify-saved> gives the text at once and then sends
  <verbatim|didSave>; <scm|notify-buffer-closed> sends <verbatim|didClose>
  for a followed buffer and forgets it (a renamed buffer is followed under
  its new name at its next change).

  <section|Diagnostics>

  <paragraph*|Showing them>

  <scm|lsp-diagnostics-changed> marks the <abbr|URI> as <em|stale>, and
  shows the diagnostics at once if it is the one of the current buffer;
  otherwise they are shown when the cursor next moves in that buffer.
  <scm|lsp-show-diagnostics> converts each diagnostic to the lines of the
  buffer as they are now (diagnostics beyond the last line are left out, an
  empty range is widened to one character, hints count as information),
  keeps them in the table <verbatim|shown>, and sets three <em|alternative
  selections> of the editor, with <scm|set-alt-selection>:
  <verbatim|lsp-errors>, <verbatim|lsp-warnings> and
  <verbatim|lsp-infos>, each a list of paths, start and end of each range.
  <scm|lsp-hide-diagnostics> cancels them.

  <paragraph*|Drawing them>

  <cpp|edit_interface_rep::update_diagnostic_rects>
  (<source-link|edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>)
  reads the three alternative selections and computes the rectangles of
  their ranges (<cpp|compute_selection>), in the colors
  <verbatim|#e02020>, <verbatim|#e09000> and <verbatim|#2080e0>, into
  <cpp|diagnostic_rects> and <cpp|diagnostic_colors>
  (<source-link|edit_interface.hpp|src/Edit/Interface/edit_interface.hpp>).
  As for the spell errors, only about 100 ranges around the visible part
  are computed when there are many. The ranges were computed by a program
  for an older text, so a range whose paths are no longer valid cursor
  positions of the document, or whose end is before its start, is skipped
  (<cpp|valid_cursor_path>). <cpp|apply_changes> calls it when the tree, the
  environment, the extents, the selections or the spell errors have
  changed, or when the visible part has moved, but not when only the cursor
  moves. <source-link|edit_repaint.cpp|src/Edit/Interface/edit_repaint.cpp>
  draws a line of 2 pixels at the bottom of each rectangle.

  <paragraph*|Messages and moves>

  <scm|notify-cursor-moved> calls <scm|lsp-cursor-moved> in a followed
  buffer: it shows the stale diagnostics, and on the status bar the first
  line of the message of the diagnostic at the cursor (errors first), as
  <verbatim|Error: ...>, <verbatim|Warning: ...> or <verbatim|Note: ...>;
  the message is cleared when the cursor leaves it.
  <scm|lsp-next-diagnostic> goes to the start of the next (or previous)
  diagnostic of <verbatim|shown>, and wraps around.

  <section|Requests at the cursor>

  The commands use the <em|running> client of the buffer
  (<scm|lsp-buffer-client*>: synchronizing first would start a server), check
  that the server has the capability (<verbatim|definitionProvider>,
  <verbatim|hoverProvider>, <verbatim|completionProvider>), give it the
  current text (<scm|lsp-buffer-sync>) and send the request at
  <scm|lsp-cursor-position>. The position is only defined when the cursor
  is in the text of a line, that is when the cursor path is the path of the
  buffer followed by a line and an offset.

  <paragraph*|Completion>

  <scm|kbd-variant> (the action of <key|tab>) is redefined with
  <scm|(:require (and forwards? (lsp-completion-here?)))>:
  <scm|lsp-completion-here?> holds in a followed buffer whose server
  completes, after a character of a word or after <verbatim|.>,
  <verbatim|\<gtr\>> or <verbatim|:>. <scm|lsp-complete> sends
  <verbatim|textDocument/completion> <em|asynchronously>: at most one
  request is awaited (a new one cancels the former), and its handler applies
  the answer only if the buffer and the cursor path have not changed. The
  items are sorted by <verbatim|sortText>, their texts
  (<verbatim|insertText>, <verbatim|textEdit.newText> or
  <verbatim|label>) are kept if they extend the word before the cursor
  (at most 100), and the suffixes are given to <scm|custom-complete> as
  <scm|(tuple <em|prefix> <em|suffix> ...)>, which inserts the first and
  cycles through the others (<source-link|edit_complete.cpp|src/Edit/Interface/edit_complete.cpp>).
  After <verbatim|.>, <verbatim|-\<gtr\>> or <verbatim|::> the word is
  empty, and the prefix given to <scm|custom-complete> is the character
  before the cursor (<scm|lsp-completion-tuple>), since it must not be
  empty. Without completions, the completion of the editor
  (<scm|complete-try?>) is tried.

  <paragraph*|Information at the cursor>

  <scm|lsp-hover> waits (at most 5 s) for the answer to
  <verbatim|textDocument/hover>, extracts its text (<verbatim|MarkupContent>,
  <verbatim|MarkedString> or an array of them), converts it from markdown
  (<scm|lsp-hover-document>, with the converter
  <verbatim|markdown-snippet>; the useful lines as verbatim if this gives
  nothing) and shows it with <scm|show-tooltip>
  (<source-link|tooltip.scm|TeXmacs/progs/utils/misc/tooltip.scm>) in a
  <markup|preview-balloon>, at the tree of the cursor, of type
  <verbatim|keyboard>, as the previews of references. Without a window
  (<scm|headless?>), the first lines go to the status bar instead.

  <paragraph*|Information at the mouse>

  The redefined <scm|mouse-event> notes each move in a followed buffer
  (unless the preference <verbatim|lsp:mouse hover> is
  <verbatim|off>); once the mouse has rested 600 ms (a <scm|delayed> check
  which waits again while it moves), the point under it is turned into a
  cursor position by <scm|xy-\<gtr\>path>, a function of the editor
  (<cpp|edit_interface_rep::xy_to_path> in
  <source-link|edit_mouse.cpp|src/Edit/Interface/edit_mouse.cpp>, as a
  click finds its position; none before the document is typeset). If it is
  on a word, the hover is asked without waiting, and shown at the line, in
  a balloon of type <verbatim|mouse> (closed when the mouse leaves it), if
  the mouse is still on the same word when the answer comes.

  <paragraph*|Signatures>

  The redefined <scm|keyboard-press> asks for
  <verbatim|textDocument/signatureHelp> after a trigger character of the
  server (<verbatim|(> and <verbatim|,> by default), without waiting;
  <scm|lsp-signature-markup> makes the label of the active signature, with
  the active parameter (a substring, or offsets) in bold, for the status
  bar, which a <verbatim|)> clears.

  <paragraph*|Definitions>

  <scm|lsp-goto-definition> takes the first <verbatim|Location> or
  <verbatim|LocationLink> of the answer and calls
  <scm|lsp-goto-location>, which switches to the buffer of the file (or
  loads it) and moves the cursor to the start of the range.

  <section|Servers and their log>

  <scm|lsp-client-ended> is redefined to say on the status bar that the
  server has ended. <scm|lsp-restart> stops the client of the buffer if it
  runs, forgets its entry, empties <verbatim|followed> and synchronizes the
  buffer, which starts a new server. <scm|lsp-stop-servers> calls
  <scm|lsp-stop-all>; since the entries are kept, the servers stay
  stopped.

  <scm|lsp-show-log> opens the auxiliary buffer <verbatim|Language server>
  (<scm|open-auxiliary>) with the log of the client of the buffer (also of a
  server which has ended). The page follows the log: the redefined
  <scm|lsp-client-log-changed> schedules <scm|lsp-log-refresh> after a
  pause of 300 ms, which replaces the body of the page as long as it exists.

  <scm|lsp-show-diagnostics-list> opens the auxiliary buffer
  <verbatim|Diagnostics> with a line per diagnostic of the file, an
  <markup|action> which calls <scm|lsp-goto-position> (a secure function);
  the redefined <scm|lsp-diagnostics-changed> refreshes it after 300 ms,
  as the log. At most 1000 diagnostics are underlined.

  <section|Edits of the servers>

  <source-link|lsp-actions.scm|TeXmacs/progs/lsp/lsp-actions.scm> applies
  the edits which the servers send: <verbatim|TextEdit>s (a range and its
  new text) for the formatting, <verbatim|WorkspaceEdit>s (the
  <verbatim|TextEdit>s of one or more files, as <verbatim|changes> or
  <verbatim|documentChanges>) for a rename and the quick fixes.
  <scm|lsp-apply-to-lines> applies them to the Cork lines of a buffer, from
  the last one to the first, so that the ranges of the others stay valid;
  <scm|lsp-set-lines> then changes the document only where its lines
  differ (the common lines at its start and at its end are kept), which
  keeps the cursor, the markup of the other lines and the undo history
  short. A file which is not open is loaded in a buffer.

  <scm|lsp-format> asks for <verbatim|textDocument/formatting> (or
  <verbatim|rangeFormatting> for the selection), <scm|lsp-rename> (an
  interactive command) for <verbatim|textDocument/rename>, and
  <scm|lsp-quick-fix> for <verbatim|textDocument/codeAction> at the cursor,
  with the diagnostics of its line: a single action is run at once, several
  are listed on the page <verbatim|Quick fixes>, as actions which call
  <scm|lsp-run-fix>. An action may have an edit and a command
  (<verbatim|workspace/executeCommand>), after which the server may ask for
  an edit itself (<verbatim|workspace/applyEdit>, answered by
  <scm|lsp-apply-workspace-edit>). The client declares these abilities in
  <verbatim|initialize> (code action literals, label offsets of the
  parameters, <verbatim|applyEdit>, document changes), else
  <name|clangd> proposes commands instead of edits.

  <section|Keys and menu>

  <source-link|lsp-kbd.scm|TeXmacs/progs/lsp/lsp-kbd.scm> binds, in the
  mode <scm|in-prog?>, <key|F12> to <scm|lsp-goto-definition>,
  <key|S-F12> to <scm|lsp-hover>, <key|F8> and <key|S-F8> to
  <scm|lsp-next-diagnostic>, <key|std F> to <scm|lsp-format>, <key|std E> to
  the rename, <key|std .> to <scm|lsp-quick-fix>, <key|std M> to
  <scm|lsp-show-diagnostics-list> (<key|F2>, the key of renames in other
  editors, is taken), and defines <scm|lsp-menu>.

  <section|The format of C files>

  The files <verbatim|.c> and <verbatim|.h> have the format <verbatim|c>,
  <verbatim|C source code>, declared in
  <source-link|code-format.scm|plugins/code/progs/code-format.scm> with its
  converters, and the language <verbatim|c>, whose keywords (C11 and C23),
  operators and numbers are in
  <source-link|c-lang.scm|plugins/code/progs/c-lang.scm>. These files are in
  the plug-in <verbatim|code> of the source tree, <source-link|src/plugins/code|plugins/code>:
  <verbatim|TeXmacs/plugins/code> is the copy made by the build, which must
  not be edited. The server of the language <verbatim|c> is
  <verbatim|clangd>, as for <verbatim|cpp>.

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
