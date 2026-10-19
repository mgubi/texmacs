# Code features: tree-sitter and language servers together

Branch `wip_code` (from `upsilon_texmacs`) integrates two features which stay
developed on their own branches:

- `wip_ts`: tree-sitter (a grammar per language, the generator in Scheme and
  C++, a vendored C runtime in `Plugins/TreeSitter`, clients in
  `progs/treesit`). Syntax of one piece of code, synchronous, also for code
  blocks of documents and session inputs. See `src/docs/tree-sitter.md`.
- `wip_lsp`: a client of language servers (C++ link in
  `System/Link/lsp_link.cpp`, protocol and clients in `progs/lsp`). Meaning
  over a project, asynchronous, for source files only. See the developer
  chapter `devel/source/lsp.en.tm`.

Both are merged here as they are (b465277952, 773de385bc). This document is
the design of the mechanism they are to share, so that each of them is an
optional provider: a build (or a run) without one of them keeps working with
the other, and one without both is the TeXmacs of `upsilon_texmacs`.

## 1. What each one brings

| | tree-sitter | language servers |
|---|---|---|
| knowledge | syntax of one text | meaning over a project |
| time | synchronous (3-6 ms per edit) | asynchronous (the server) |
| where | source files, code blocks, sessions, inline code | source files |
| needs | a grammar (22 shipped) | a server on the system |
| colours | syntax (highlights.scm) | semantic tokens (kinds of names) |
| folds | folds.scm ranges, view state | foldingRange, lines not typeset |
| outline | tags.scm | documentSymbol |
| definitions | same code | project |
| highlights of a name | (locals, not done) | documentHighlight |
| only it | moves, selection, brackets, structural edits, indentation, session input, comments | diagnostics, completion, hover, signature, references, rename, fixes, formatting, symbols, calls, inlay hints |

## 2. Where they met at the merge (before phase 2; see the phases for now)

- The typesetter: `prog_language` returns a tree-sitter wrapper (colours in
  the highlight observers of the lines, codes >= 1000000); `typeset_prog_string`
  goes to `typeset_lsp_string` for lines with LSP decorations (a hashmap keyed
  by the address of the tree of a line and its text).
- Two folds: tree-sitter keeps the boxes of a folded line, clipped
  (`bridge_rep::fold_line`, state 999998 in the observer), the cursor stays
  valid and a fold opens when the cursor comes in (`unfold_at_cursor`); the LSP
  does not typeset folded lines (`insert_paragraph`), so the cursor has no box
  there (`notify_cursor_in_fold`). Both run in `edit_cursor.cpp`.
- Scheme commands overloaded by both with `tm-define` (indentation, brackets,
  selection, moves): the last one loaded wins.
- The Focus menu: entries of both. The keys std ( and std ): the folds of
  tree-sitter (prog-kbd.scm) and those of the LSP (lsp-kbd.scm, mode in-lsp%,
  which wins in the files which a server follows).
- Positions: tree-sitter counts bytes of the Cork strings, the LSP UTF-16
  units (converted in Scheme, character by character).
- Tabs: `wip_lsp` keeps them as the Cork character `<#9>`; the indentation
  helpers of tree-sitter (`syntax_region.cpp` 812-923) only know `' '` and
  `'\t'` bytes, and `char_column` counts `<#9>` as one column.
- The core includes the headers of both: `bridge.cpp` includes
  `Plugins/TreeSitter/ts_highlight.hpp`; `typesetter.cpp`, `concat_text.cpp`
  and `edit_cursor.cpp` use `lsp_decorations`. A build without one of them
  does not compile.

## 3. The common mechanism

The rule (also that of the comparison with the design of the user, which
says the same): tree-sitter does what has to happen at each key, a server
refines it when it runs; and what both use is in the core, so that each of
them can be left out.

Five pieces, in the core (no reference to tree-sitter or to the LSP), which
the two features use through adapters. The rule: the core defines the
mechanism and a default which does nothing; a feature registers itself; the
core never calls a feature by name.

### 3.1 Source regions (C++, core)

A source region is a run of lines of code in a language: the body of a
source file, of a code block, a session input. The core keeps, per region, a
snapshot of its lines and a version, and computes the change since the last
request (the line diff of `syntax_region.cpp`, moved out of the plugin): one
edit, a range of lines and the new text. Listeners get it:

- tree-sitter: `ts_tree_edit`, then the incremental parse;
- the LSP: an incremental `didChange` with the version (instead of the diff of
  whole texts in Scheme, `lsp-text-lines-change`, and of the text built at
  each sync).

Without listeners nothing is computed.

### 3.2 Positions (C++, core)

One service for a line: a Cork offset to and from code points, UTF-16 units,
UTF-8 bytes, and the visual column (tabs `<#9>` at the stops of
`prog-tab-width`, a character of Cork being one column). It replaces
`texmacs_decode`/`char_column` (tree-sitter) and `lsp-character->offset` /
`lsp-offset->character` (Scheme, LSP), and the column code of
`concat_text.cpp` (`prog_column`). The indentation helpers of tree-sitter use
it, which makes them right with tabs.

### 3.3 Line state (C++, core)

The state of a line which is not in the document, kept on the line's
highlight observer, which an edit of the line detaches (no address keys, no
stale state, no leak: the tree-sitter way). Layers, each set by a provider:

- syntax colours (tree-sitter, today's observer arrays);
- semantic colours (LSP): drawn over the syntax colours, for the kinds which
  the server gives only;
- hints (LSP inlay hints): texts before characters, not part of the line;
- fold state (hidden, first line of a fold with its marker).

One `typeset_prog_string` draws a line from them: tabs at their stops, the
colours, the hints, the marker. `lsp_decorations.cpp` (the hashmap) and the
LSP branch of the typesetter go.

### 3.4 Folds (core)

One mechanism: the view state of tree-sitter (the boxes of a hidden line are
kept, clipped: the cursor stays valid, no hook for a cursor without box),
with the cursor rules of the LSP (the arrows go over a fold, a jump into it
opens it, the folds in a fold which goes stay). The ranges come from a
provider: the server's `foldingRange` when a server follows the file, else
`folds.scm` of the grammar. One pair of keys (std ( and std )) and one menu.
`insert_paragraph` skipping lines and `notify_cursor_in_fold` go.

### 3.5 Features and providers (Scheme, core)

A module `progs/code/` with a table of features and their providers:

    (code-provider-set! feature name rank available? procedure)

`code-*` commands (fold ranges, outline, definition, references, rename,
format, indent, select, move, brackets, highlight, diagnostics, completion,
hover...) ask the providers of the feature, by rank, among those which are
available for the buffer and the cursor (a grammar is loaded, a server
follows the file). The default ranks:

- syntax and immediate, at each key (colours, brackets, closing keywords
  and tags, moves, selection, indentation on Return, session input,
  comments, structural edits): tree-sitter; a server only where there is no
  grammar (selectionRange, onTypeFormatting). The formatting of the server
  stays a command;
- meaning or project (diagnostics, completion, hover, signature, definitions
  and references over files, rename, fixes, formatting, symbols, calls, hints,
  semantic colours): the server; tree-sitter where there is no server
  (definitions and highlights in the same code, syntax errors as
  diagnostics);
- both (outline, folds): the server when one follows the file, else
  tree-sitter;
- completion: the server, with tree-sitter giving the context (no completion
  in a string or a comment) and, without a server, the names of the code
  (locals, definitions) as completions;
- errors: both, each as it is. The unmatched brackets of tree-sitter (red)
  at once, while typing; the diagnostics of the server when they come. The
  other syntax errors of tree-sitter (ERROR and MISSING nodes) are shown as
  diagnostics only without a server, which would say the same.

A preference per feature (`code:<feature>`: auto, syntax, server, off). The
keys and the Focus menu are those of the `code-*` commands, with the entries
of the features which have an available provider. The commands which both
override today (`program-compute-indentation`, `select-brackets-after-movement`
...) call the dispatcher: no more overloads which win by load order.

Languages: the table of `tm-program-languages.scm` (tree-sitter) gets the
server of each language (`(server "clangd")`), replacing the defaults of
`lsp-server-set!`; the C format of `wip_lsp` is one of its formats.

## 4. Graceful downgrade

- Each feature registers its providers from its own module
  (`progs/treesit/ts-code.scm`, `progs/lsp/lsp-code.scm`), loaded only when
  the feature is there (the plugin, the module); the core works with none.
- C++: the core has the regions, positions, line state and folds; the plugin
  `TreeSitter` and the files of the LSP (`System/Link/lsp_link.cpp`, glue) are
  the only places which know them, and they can be left out of the build
  (configure options `--disable-tree-sitter`, `--disable-lsp`; `#ifdef` only
  in the glue tables).
- Tests: a suite `code` with stub providers (the dispatch, the ranks, the
  layers, the folds, the positions), which runs in every build; the suites of
  the two features run when they are there. CI configurations: both, each
  alone, none.

## 5. Phases

1. Merge (done; and the later commits of `wip_ts`: closing keywords and
   tags, fold keys, the browser): `wip_ts` + `wip_lsp`, conflicts resolved conservatively, the
   two mechanisms side by side; the 55 suites pass (treesit 423 checks, lsp
   350). In a window, clangd follows a C file with tabs (colours, folds); the
   C files are not highlighted by tree-sitter yet: the grammar `c` is for the
   tests in `wip_ts`, and the format `c` comes from `wip_lsp` (phase 4, the
   table of languages).
2. The core pieces with the current behaviour: positions (3.2), line state
   with layers (3.3) and folds (3.4) in the core; the header includes of the
   core on the features removed; tree-sitter's helpers tab-aware.
   - Folds (done): the state of a hidden line is in the core
     (`System/Language/code_lines.cpp`, glue `code-line-fold-state`,
     `code-set-line-fold-state`); `bridge.cpp` and `edit_cursor.cpp` no
     longer include `ts_highlight.hpp`. The folds of the LSP (lines not
     typeset, `lsp_fold_line`, `notify_cursor_in_fold`, the markers) are
     gone. `progs/code/code-fold.scm`: the commands `code-fold`,
     `code-unfold`, `code-fold-all`, `code-unfold-all` (std ( and std ), the
     menus of both), the hidden lines (`code-fold-lines`, `code-folds`...),
     and the cursor (`code-cursor-in-fold`, which C++ calls with the last
     place of the cursor outside of the folds and the new one: the arrows
     go over a fold, a jump opens it at once). The blocks come from the
     feature `fold-ranges` / `fold-all-ranges`: the adapters
     `progs/lsp/lsp-code.scm` (rank 10, `folding-ranges` of the server) and
     `progs/treesit/ts-code.scm` (rank 20, the nodes of several lines),
     loaded by `code-features.scm` when their files are there. A fold in a
     fold is one run of hidden lines; replacing the first line of a fold
     keeps it (the LSP dropped it). Suite `code` (23 checks, providers of its
     own, without tree-sitter or a server). In a window: in a C++ file, the
     server gives the block, the arrows go over it, Fold all folds the body
     of main.
   - Positions (done): `code-column` (tabs at their stops), `code-utf16-column`
     and `code-utf16-offset` (the units of the servers), `code-indent-end`;
     used by the LSP (`lsp-offset->character`, `lsp-character->offset`), by
     the indentation of tree-sitter (`indent_columns`, `trimmed`,
     `find_delimiter`, `char_column` of `syntax_region.cpp`, now right with
     tabs), by `prog-edit.scm` and by the typesetter.
   - Layers (done): the colours and the hints of a line are kept on its
     highlight observers (`code-set-line-layers`, `code-clear-line-layers`,
     `code-line-layers`), drawn by `typeset_layered_string` over the colours
     of the language (those of tree-sitter in a C++ file); an edited line
     forgets them. `lsp_decorations.cpp` (the hashmap keyed by the
     addresses of the trees) is gone.
   - Left for phase 6: `tm_server.cpp` calls `process_all_lsp`, and
     `prog_language.cpp` wraps the languages of tree-sitter
     (`tree_sitter_wraps`): the two places where the core names a
     feature, to be turned into registrations.
3. Source regions (3.1): the diff out of `syntax_region.cpp`; tree-sitter and
   the LSP as listeners; per-edit timings on large files (s7.c, parser.c).
   - Done: `System/Language/code_regions.cpp`. `code_lines_common` (the
     common first and last lines), which `region_update` of tree-sitter
     uses; snapshots of the lines of a region as a file has them
     (`code-snapshot-new`, `-change`, `-changed?`, `-text`, `-delete`): the
     change since the snapshot as a range of positions (lines, UTF-16 units)
     and its text, by lines, or by characters when one line changed (as by
     typing). Each feature keeps its own snapshot, at its own pace: what is
     shared is the computation, not the moment (tree-sitter follows the
     region at each highlight, a server after a pause). The LSP gives a
     buffer to its server through a snapshot (`lsp-document-open-lines`,
     `lsp-document-change-lines`): the text is no longer made again at each
     sync; it is made for a server which takes whole texts only, and when
     it is asked for (`lsp-document-sent-text`). The stale checks (edits of
     a server for an older text, the page of the fixes, the answers for the
     colours) compare snapshots. s7.c (104848 lines): a sync after an edit
     8 ms, the text 129 ms (what each sync cost before). Documents given as
     texts (the tests of the protocol) keep the former path.
4. Providers (3.5): the `code-*` commands, the preferences, the menu and keys;
   the commands of both moved behind them; the language table with servers.
   - Done for the features which both give: the folds (phase 2), the
     outline and the definitions (`progs/code/code-navigate.scm`: features
     `outline`, entries (level kind name detail path), and `definition`,
     which answers #t when it went; `code-outline` shows a page which
     follows the buffer, `code-definitions-menu`, `code-go-to-definition`).
     The servers give their symbols and their definitions (also in other
     files), tree-sitter its tags (also in code blocks). One menu Focus in
     program code (`progs/code/code-menu.scm`): the entries of the features
     which a provider serves at the cursor, then those of the syntax tree
     (`code-syntax-menu`, tree-sitter); later also the diagnostics and the
     entries of a server only (`code-server-menu`, lsp-code.scm). The keys F12 (definition) and std O
     (outline) are those of all program code (prog-kbd.scm), no longer of
     the files which a server follows only. The table of the program
     languages has the commands of their servers (`servers`, and `lsp-id`
     for a languageId which is not the name), which the LSP takes when the
     table is there (22 languages); C is highlighted by tree-sitter. In a
     window: in a C file, F12 goes to the definition in another file (the
     server), the outline page shows its symbols.
   - Left as they are, given by one of them only: the indentation, the
     selection, the brackets and the structural edits (tree-sitter), the
     diagnostics, completion, hovers, references, renames, fixes, symbols of
     the project and calls (the servers).
5. Fallbacks across: syntax errors of tree-sitter as diagnostics without a
   server; LSP selectionRange and documentSymbol without a grammar; later,
   servers for code blocks of documents (virtual documents).
   - Diagnostics (done): `progs/code/code-diagnostics.scm` keeps the items
     (severity start end message, paths) of each source of a buffer: the
     server (the diagnostics of the LSP, `draw-diagnostics`) and the syntax
     (the error and missing nodes of the code at the cursor, feature
     `syntax-errors` of tree-sitter, after a pause, where no server follows
     the code: feature `server-diagnostics`); all are underlined (the
     selections are now code-errors, code-warnings and code-infos, drawn by
     edit_interface.cpp), F8 and S-F8 go through them in all program code.
     The unmatched brackets stay red at once (the colours of tree-sitter).
     In a window: a Python file without a server has its syntax error
     underlined, F8 goes to it.
   - Completion (done): `progs/code/code-complete.scm`, the feature
     `completion-context` (string, comment or code, by the syntax tree):
     the LSP does not complete in a string or a comment; the feature
     `local-names` (the names of the definitions of the code, tree-sitter):
     tab completes with them where no server follows the code. In a window:
     in Python without a server, cou + tab gives counter; not in "cou".
   - Not done: selectionRange and documentSymbol of a server for code
     without a grammar (every language with a server has a grammar now);
     servers for the code blocks of documents.
6. Builds without each feature (configure options), the suite `code`, docs:
   a developer chapter on the code features and one user page per feature.
   - Done. `--disable-tree-sitter` and `--disable-lsp` (misc/m4/tm_code.m4,
     `USE_TREE_SITTER` and `USE_LSP` in config.h; the CMake options of the
     same names; the browser build has tree-sitter, not the servers): the
     plugin TreeSitter and its C runtime are compiled only with tree-sitter
     (@CONFIG_TREE_SITTER@ in makefile.in), lsp_link.cpp only with the
     servers; their glue is in build-glue-treesit.scm and build-glue-lsp.scm
     (glue_treesit.cpp, glue_lsp.cpp, included by glue.cpp with their
     feature). The core names neither: tree-sitter wraps the program
     languages by `set_prog_language_wrapper`, the servers process their
     messages by `add_link_processor` (registered at the first lsp-start).
     In Scheme, a feature is there when its glue is (`tree-sitter-program-language`,
     `lsp-start`): the adapters are loaded then, the modules of the editing
     of programs (prog-edit, session-edit, program-edit) load
     code-syntax.scm, which loads ts-edit.scm or else code-syntax-none.scm
     (its functions answer that there is no syntax tree), the LSP is started and its menu shown only
     then, and check-master skips the suites of a missing feature.
   - Docs: the page "Editing program code" says what comes from which; the
     developer page "The features of program code" (devel/source/code-features.en.tm)
     describes the core; the pages of the LSP point to it.

7. Servers for the code blocks of documents (virtual documents).
   - What: in a TeXmacs document saved as a file, a code block of a
     language with a server (`cpp-code`, `python-code`... whose body is a
     document of lines) is a document of that server: a virtual file next
     to the document, `<dir>/.<name>.tm-block<k>.<ext>`, never written; the
     server reads its text from didOpen and didChange. The project root is
     that of the document.
   - One block, one virtual document: the positions of the block are those
     of a source file (line, offset of its lines), so that all of the
     buffer layer of the LSP works on a "target" (uri, language, lines,
     base path) rather than on the buffer: a source file is the target of
     its body, a block the target of its own lines. Blocks of the same
     language are not concatenated (the names of one block are not known
     in the next): later, if wanted, by a map of lines.
   - Opt-in: the preference `lsp:code blocks` (off by default; starting a
     server for a document with a small block costs a process), in the
     menu Tools > Language servers.
   - Features in a block: the diagnostics (those of all the blocks of the
     buffer are its items of the source 'server of code-diagnostics.scm,
     each at the place of its block), completion, hover, definitions and
     references (a location in the virtual file of a block goes to the
     block), outline, folds, highlights, formatting (of the block), renames
     and quick fixes (the edits of a virtual file are applied to its
     block). Not yet at first: semantic colours and inlay hints (source
     files only), the status of the server in the focus bar (both done
     since, see below).
   - Done: `progs/lsp/lsp-blocks.scm` (the blocks of a body, the block at
     the cursor, the virtual files, `lsp-blocks-force!` for the tests);
     in lsp-documents.scm, the functions of the buffer without a buffer
     argument are those of the "target", the code at the cursor
     (`lsp-target-root`, `lsp-target-base`: the lines and their path), and
     syncing a document gives its blocks to their servers and closes those
     which are gone. Tested with clangd (suite lsp, group code blocks) and
     in a window (the undeclared name of a block underlined, its message
     in the footer).
   - A block is identified by its tree (tree-eq?); when it is removed, or
     the document closed, its virtual document is closed on the server.

8. The tokens of the typesetting from the syntax tree.
   - Done: where tree-sitter colours a line, the typesetting cuts it at the
     starts of the leaves of the tree (marked in the colour codes of the
     line by `TS_TOKEN_START`, syntax_region.cpp), at the changes of colour
     (the injected code, whose own trees are not walked) and at the spaces
     (where the lines may break, also in comments and strings); the
     tokenizer of the language of TeXmacs is no longer called for those
     lines. A line without the colours of tree-sitter (a language without a
     grammar, a build without tree-sitter, `tree-sitter-set-highlighting`
     off) is cut by that tokenizer, as before. `tree-sitter-line-tokens`
     gives the cuts (suite treesit). The printed pages of a C++ file of 678
     lines are the same as before but for one place: a word of a comment
     is now one box, so that the kerning of its letters applies (`'))`).

9. The blocks of a language together; a keystroke in a long file.
   - Units (lsp-blocks.scm): with `lsp:join code blocks` (on by default),
     the blocks of a language of a document are one virtual file,
     `.<name>-blocks.<suffix>`, whose lines are a document of their own made
     again from the blocks; the target at the cursor is an object (its
     lines, `lsp-target-path` and `lsp-target-lo` between them and the
     places of the document, `lsp-target-key`), so that the requests, the
     diagnostics, the highlights, the jumps and the edits go through the
     mapping; an edit of a unit is written back to its blocks when it
     changes the lines of one block only. The colours and the inlay hints
     of the servers are given to the lines of blocks and units too. The
     outline of a unit is that of all its blocks, its folds those inside a block (lsp-fold.scm maps them). Suite lsp,
     group "code blocks together" (with clangd: a name of a block known in
     the next one, a definition and a rename across them); in a window.
   - Tree-sitter, a keystroke: the region is changed in place (lines,
     offsets, text, colours), the unchanged lines are compared by address,
     the text is kept and given to the queries without a copy, the
     unmatched brackets are looked for from the changed bytes, and all the
     lines are coloured only when the region coloured them all. A C file of
     100000 lines: 72 ms per key before, 21 ms now (13000 lines: 10.6 to
     2.7); the rest is the reparse of tree-sitter (the flat translation
     unit of C) and the edit of TeXmacs itself. A bug shown by a new test:
     a coloured line did not look at its region again after another line
     was removed.
   - The leftovers of the tabs: a tab in search, its name in the footer,
     the tab width of open files, the indenters of R and Mathemagix.

Audit of phase 9 (2026-10-18): the opening bracket of a closing one which is
MISSING after the bytes coloured again is painted again (the walk looks at the
last child; the opening brackets are remembered on the way, not looked for
backwards from the root); a line with markup is compared by its text (it made
a reparse at each query); the server's colours of blocks and units go when
their file is closed (also when the blocks are no longer followed or joined),
and their lines are typeset again; the edits of a unit keep the lines with
markup and the cursor, and refresh the unit first; the diagnostics of a unit
are placed as the server last saw its blocks; a query at the cursor refreshes
the lines of its block only (the whole unit when its blocks or their lengths
change); a block whose language changes leaves its unit; a block which an
edit empties keeps a line; tab in the search of code is not taken by the
indentation of R and Mathemagix, nor by the completion. Tests: an unclosed
bracket after an edit (treesit), the edits of a unit which add a line or
would join blocks, a strict rename, the switch to single blocks which closes
the unit and takes its colours away (lsp).

10. The cost of a key in a long file (2026-10-18). Measured on real keys in a
    C file of 50000 lines (Vue, TEXMACS_EDIT_PROFILE): 48 ms per key, of
    which typesetting 31 ms; the scripts with -x mislead (no typesetting
    between their edits). Two causes, both growing with the file:
    - Outside paper mode, the lines of the document were merged into one
      stack box at each key (57% of the typesetting): merged_lines now
      stacks the cached chunks of chunk_lines from 512 lines (PR #371 on
      wip_fixes, and on this branch): typesetting 31 to 20 ms.
    - Tree-sitter: its own incremental parse grows with the nodes at the
      top level (measured with its runtime alone: 13 ms after one character
      in 100000 lines of C). A region of more than 6000 lines is cut into
      parts of about 2000 lines at nodes of the top level
      (syntax_group_rep, syntax_region.cpp), each a region of its own;
      the colouring after a key: 11.4 to 2.65 ms for 100000 lines.
    With both: 40 letters 1947 to 975 ms, typesetting per key 31 to 15 ms.
    The rest which grows with the file: the comparison of the lines of the
    region at each request, the reuse checks of the bridges, the changed
    rectangles (6.6 ms).

Audit (2026-10-18), after phase 8; the fixes:
- Without tree-sitter, the modules of sessions and programs loaded ts-edit.scm
  (whose public functions replaced the stubs): all go through
  `progs/code/code-syntax.scm`, which loads it or code-syntax-none.scm.
- `code-ask` asks the next provider when one answers `'()` (a server without
  a block at the cursor lets tree-sitter fold); Fold all folds the blocks at
  the top level where there are no definitions; of two blocks with the same
  first line the larger one first; folds deeper than 9 are refused; hidden
  lines with no first line before them (it was deleted) are shown by Unfold
  all and by the cursor.
- The mark `...` of a new fold was typeset but not drawn (the part drawn
  again was that of the line without it): the window is drawn again.
- Tab completes with the names of the code only when some continue the word,
  and not in a session which completes itself; the menu Focus of code keeps
  the entries of sessions and programs (`former`); the syntax errors are
  cleared when the cursor leaves the code.
- Code blocks: the virtual files are named after the document with its
  suffix; a block whose language changes closes its former file; the
  underlines follow the blocks when the text before them changes; the
  diagnostics which came while the document was not shown are drawn; the
  focus bar and the highlights are those of the block at the cursor; the
  quick fixes of a block compare the lines of the block; a place in a block
  of a closed document opens nothing; the blocks are looked for in the whole
  document at the first sync and at each save, else the known ones and the
  one at the cursor.
- The colours and hints of a server came back after an undo (an edit removes
  them, the line keeps its tree); two hints at a place are both shown; the
  lists of observers of a line no longer grow with folds and layers; the
  last place outside of the folds is that of each editor; the glue suite
  checks the glue of the features; dead functions of ts_highlight removed.

Each phase keeps the suites passing and the two feature branches unchanged:
what is shared is written here, then given back to the branches (or the
branches rebased on it) only when the user decides it.
