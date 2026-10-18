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
  servers and removes the underlines in each buffer which has some; setting
  it to <verbatim|on> again follows all the open buffers (<verbatim|follow-all>).

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
  its new name at its next change). <scm|lsp-buffer-followed?> tells whether
  the current buffer is followed (the table <verbatim|followed>, without
  mode predicates).

  <section|Diagnostics>

  <paragraph*|Showing them>

  <scm|lsp-diagnostics-changed> marks the <abbr|URI> as <em|stale>, and
  shows the diagnostics at once if it is the one of the current buffer;
  otherwise they are shown when the cursor next moves in that buffer.
  <scm|lsp-show-diagnostics> converts each diagnostic to the lines of the
  buffer as they are now (a vector of them; diagnostics beyond the last
  line are left out, an empty range is widened to one character, hints
  count as information), keeps them in the table <verbatim|shown>, and
  sets (<verbatim|draw-diagnostics>) three <em|alternative selections> of
  the editor, with <scm|set-alt-selection>: <verbatim|lsp-errors>,
  <verbatim|lsp-warnings> and <verbatim|lsp-infos>, each a list of paths,
  start and end of each range, sorted in the order of the text (the editor
  looks for the visible ones by a binary search). <scm|lsp-hide-diagnostics>
  cancels them.

  Until the server publishes new ones, the diagnostics shown follow the
  lines: the redefined hook <scm|lsp-document-lines-changed> (called at each
  <verbatim|didChange>) moves those after the changed lines, drops those in
  them (an edit removes their underlines) and draws them again; <key|F8>
  and the messages at the cursor use these positions.

  <paragraph*|Drawing them>

  <cpp|edit_interface_rep::update_diagnostic_rects>
  (<source-link|edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>)
  reads the three alternative selections and computes the rectangles of
  their ranges (<cpp|compute_selection>), in the colors
  <verbatim|#e02020>, <verbatim|#e09000> and <verbatim|#2080e0>, into
  <cpp|diagnostic_rects> and <cpp|diagnostic_colors>
  (<source-link|edit_interface.hpp|src/Edit/Interface/edit_interface.hpp>).
  The ranges were computed by a program for an older text, so a range
  whose paths are no longer valid cursor positions of the document, or
  whose end is before its start, is dropped first (<cpp|valid_cursor_path>).
  Then, as for the spell errors, only about 100 ranges around the visible
  part are computed when there are many (<cpp|find_alt_selection_index>, a
  binary search which assumes the ranges in the order of the text and
  valid). <cpp|apply_changes> calls it when the tree, the
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
  buffer followed by a line and an offset; it is computed from that line
  only (<scm|lsp-buffer-position>), and so is the word before the cursor.

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

  The redefined <scm|mouse-event> (without mode predicates, as
  <scm|notify-cursor-moved>, see <hlink|the pitfalls|lsp-pitfalls.en.tm>)
  notes each move in a followed buffer
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

  The redefined <scm|keyboard-press> (without mode predicates either) asks
  for <verbatim|textDocument/signatureHelp> after a trigger character of
  the server (<verbatim|(> and <verbatim|,> by default), without waiting; a
  new request cancels the one which is awaited, and an answer is shown only
  if the cursor is still in the same buffer and on the same line (an answer
  may come late). <scm|lsp-signature-markup> makes the label of the active
  signature, with
  the active parameter (a substring, or offsets) between guillemets, for
  the status bar, which a <verbatim|)> clears (the status bar shows text,
  not markup: a bold parameter looked as the others). The parts of the
  label (<scm|lsp-signature-parts>: before the parameter, the parameter,
  after it) are kept for the focus bar, which shows the parameter in bold.

  <paragraph*|The focus bar>

  <source-link|lsp-kbd.scm|TeXmacs/progs/lsp/lsp-kbd.scm> adds
  <scm|lsp-focus-icons> to <scm|focus-code-icons> (of
  <source-link|prog-menu.scm|TeXmacs/progs/prog/prog-menu.scm>):
  <scm|lsp-focus-status> gives the name of the server of the current
  buffer, its state (<scm|starting>, <scm|ready>, <scm|stopped>, or
  <scm|none> before it is started), its work and the numbers of errors and
  warnings shown. The focus bar is made again at each key, so the language
  and the project of a buffer are kept, and the command of a server once it
  is found in the <verbatim|PATH>. The client calls
  <scm|lsp-client-status-changed> when its state changes and when a
  <verbatim|$/progress> notification comes (<scm|lsp-client-progress>
  gives the text of the work, while it lasts); the editor then asks for an
  update of the menus, once for many changes (<scm|lsp-status-refresh>),
  as for new diagnostics and signatures.

  <paragraph*|Definitions>

  <scm|lsp-goto-definition> takes the first <verbatim|Location> or
  <verbatim|LocationLink> of the answer and calls
  <scm|lsp-goto-location>, which switches to the buffer of the file (or
  loads it) and moves the cursor to the start of the range.

  <section|References and outline>

  <source-link|lsp-navigate.scm|TeXmacs/progs/lsp/lsp-navigate.scm> makes
  two pages, as the one of the diagnostics. <scm|lsp-references> (<key|std
  U>) asks for <verbatim|textDocument/references> at the cursor, with
  <verbatim|includeDeclaration> (<scm|lsp-request-at-cursor-with> adds the
  context to the parameters of the cursor), groups the Locations by file
  (in the order of their first reference, then by position) and shows each
  one with its line: that of the buffer of the file if it is open, else of
  the file on disk. At most 2000 references are shown.

  <scm|lsp-outline> (<key|std O>) asks for
  <verbatim|textDocument/documentSymbol>; <scm|lsp-outline-entries> turns
  the answer into lines (depth, kind, name, detail, position): nested
  DocumentSymbols in their order, or flat SymbolInformations sorted by
  position. The page is asked for again (asynchronously) when the server
  publishes the diagnostics of its file, as long as it is open.

  <scm|lsp-find-symbol> (<key|std P>) asks for
  <verbatim|workspace/symbol> with the name which it is given and lists at
  most 500 SymbolInformations or WorkspaceSymbols (a location without a
  range goes to the start of its file). <scm|lsp-call-hierarchy> (<key|std
  H>) asks for <verbatim|textDocument/prepareCallHierarchy> at the cursor,
  then for the <verbatim|callHierarchy/incomingCalls> and
  <verbatim|outgoingCalls> of the first item: a caller goes to its first
  call (<verbatim|fromRanges>), a callee to its definition
  (<verbatim|selectionRange>).

  When the cursor rests (<scm|notify-cursor-moved>, then <scm|delayed> with
  <scm|:idle 300>), <scm|lsp-highlight-at-cursor> sends the changes which
  wait (<scm|lsp-sync-pending>), asks for
  <verbatim|textDocument/documentHighlight> (the previous request is
  cancelled) and, if the cursor has not moved, sets the alternative
  selection <verbatim|lsp-highlights>, which
  <source-link|edit_interface.cpp|src/Edit/Interface/edit_interface.cpp>
  draws as the matches of a search (with <verbatim|alternate> and
  <verbatim|brackets>). The highlights are cleared when the cursor leaves
  them; an edit removes all the alternative selections, and
  <scm|notify-buffer-changed> forgets them.

  <section|Colours and inlay hints>

  <source-link|lsp-decorate.scm|TeXmacs/progs/lsp/lsp-decorate.scm> asks the
  server of the current buffer for <verbatim|textDocument/semanticTokens/full>
  and <verbatim|textDocument/inlayHint> (the whole file), 200 ms after the
  server has published diagnostics for it (it has seen the text then), if
  the buffer has at most 20000 lines and the preferences
  <verbatim|lsp:semantic colors> and <verbatim|lsp:inlay hints> are on. The
  answers are used only if the buffer still has the text they were asked
  for. <scm|lsp-decode-tokens> decodes the tokens (relative lines and
  starts, the types of the legend of the server); <scm|lsp-token-color>
  gives colours to functions, methods, types and macros only (those of the
  preferences of the syntax for types and macros), the rest keeps the
  colours of the language of <TeXmacs>; <scm|lsp-line-decorations> gives,
  line by line, the coloured pieces and the hints in Cork offsets.

  <scm|lsp-apply-decorations> gives them to the typesetter with
  <scm|lsp-decorate-line> (glue), for the lines whose decorations change,
  and typesets those lines again (<scm|update-path>; the whole buffer
  beyond 2000 lines). <source-link|lsp_decorations.cpp|src/System/Language/lsp_decorations.cpp>
  keeps them for the tree of each line, with its text then:
  <cpp|lsp_line_decorations_of> gives none for a line whose text has
  changed, so that an edited line is typeset as usual until the next
  answer. <cpp|concater_rep::typeset_prog_string> (<source-link|concat_text.cpp|src/Typeset/Concat/concat_text.cpp>)
  calls <cpp|typeset_lsp_string> for a decorated line: the pieces of a token
  with colours of their own are typeset apart, and the hints come before
  the tokens where they are, as grey text boxes with decorated paths (the
  cursor does not go into them), their paddings as spaces. The lines of a
  closed buffer, and all of them when the servers are turned off, lose
  their decorations.

  <section|Servers and their log>

  <scm|lsp-client-ended> is redefined to say on the status bar that the
  server has ended. <scm|lsp-restart> stops the client of the buffer if it
  runs, forgets its entry, empties <verbatim|followed> and synchronizes the
  buffer, which starts a new server; all the open buffers are then
  followed again (<verbatim|follow-all>), so that the other files of the
  project reach the new server too. <scm|lsp-stop-servers> calls
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
  <scm|lsp-apply-to-lines> applies them to the Cork lines of a buffer in one
  pass (linear in the size of the file, also for the thousands of edits of
  a formatter): the edits are sorted by their start, edits with the same
  start in the order of the server (by their index: the <verbatim|sort> of
  <name|S7> is not stable), and the text between them is copied from a
  vector of the lines; a position beyond the text is clamped to its end,
  and an edit which overlaps the previous one (which a server should not
  send) starts where that one ended. <scm|lsp-set-lines> then changes the
  document only where its lines differ (the common lines at its start and
  at its end are kept, the others compared in vectors), which keeps the
  markup of the other lines and the undo history short. A line which
  differs is replaced, so the cursor would leave it: its new place is
  computed by <scm|lsp-move-through-edits> (the same before an edit,
  shifted after it, also on its line when the new text is longer, inside
  it the same while the new text is still there), and the
  cursor goes there. A file which is not open is loaded in a buffer.

  <scm|lsp-format> asks for <verbatim|textDocument/formatting> (or
  <verbatim|rangeFormatting> for the selection), <scm|lsp-rename> (an
  interactive command) for <verbatim|textDocument/rename>, after giving the
  servers the texts of all the buffers which have changed
  (<scm|lsp-sync-pending>: the edits are made for the texts which the
  server has), and <scm|lsp-quick-fix> for
  <verbatim|textDocument/codeAction> at the cursor, with the diagnostics of
  its line: a single action is run at once, several are listed on the page
  <verbatim|Quick fixes>, as actions which call <scm|lsp-run-fix>. The page
  keeps the buffer and its text (<verbatim|fixes-buffer>,
  <verbatim|fixes-text>), since the ranges of the fixes are those of that
  text: a fix is applied only to its buffer, and refused with a message if
  that buffer has been closed or its text has changed since. An action may
  have an edit and a command (<verbatim|workspace/executeCommand>), after
  which the server may ask for an edit itself
  (<verbatim|workspace/applyEdit>, applied by
  <scm|lsp-apply-workspace-edit>, which returns the number of files
  changed; the answer is <verbatim|applied> <verbatim|false>, with a
  <verbatim|failureReason>, when a file could not be opened). The client
  declares these abilities in
  <verbatim|initialize> (code action literals, label offsets of the
  parameters, <verbatim|applyEdit>, document changes), else
  <name|clangd> proposes commands instead of edits.

  <section|Keys and menu>

  <source-link|lsp-kbd.scm|TeXmacs/progs/lsp/lsp-kbd.scm> defines the mode
  <scm|in-lsp%>, <scm|(lsp-followed?)> within <scm|in-prog%>: a buffer which
  a server follows, while the preference <verbatim|lsp> is on
  (<scm|lsp-followed?> uses <scm|lsp-buffer-followed?>). Elsewhere
  <key|F12> and <key|S-F12> keep their meaning (the slides, the folds). It
  binds, in the mode <scm|in-lsp?>, <key|F12> to <scm|lsp-goto-definition>,
  <key|S-F12> to <scm|lsp-hover>, <key|F8> and <key|S-F8> to
  <scm|lsp-next-diagnostic>, <key|std F> to <scm|lsp-format>, <key|std E> to
  the rename, <key|std .> to <scm|lsp-quick-fix>, <key|std M> to
  <scm|lsp-show-diagnostics-list> (<key|F2>, the key of renames in other
  editors, is taken), and defines <scm|lsp-menu>.

  <section|The status bar and the font of source files>

  In a source file (<scm|footer-in-source?> in
  <source-link|footer-menu.scm|TeXmacs/progs/texmacs/menus/footer-menu.scm>:
  <scm|in-prog?> and the initial <verbatim|mode> is <verbatim|prog>), the
  status bar shows the language only, without the fonts and colors of the
  document; code in a document keeps them. The font of source files is the
  preference <verbatim|editor:code font> (Inconsolata by default, offered
  in <source-link|preferences-widgets.scm|TeXmacs/progs/texmacs/menus/preferences-widgets.scm>):
  <cpp|attach_subformat> (<source-link|new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>)
  sets it as <verbatim|prog-font> in the initial environment of a source
  file which is opened, and <scm|notify-code-font>
  (<source-link|tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>)
  gives a new value to the open source files, which stay unmodified.

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
