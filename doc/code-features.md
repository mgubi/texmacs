# Code features: tree-sitter and language servers together

Branch `wip_code` (from `maxs_texmacs`) integrates two features which stay
developed on their own branches:

- `wip_ts`: tree-sitter (a grammar per language, the generator in Scheme and
  C++, a vendored C runtime in `Plugins/TreeSitter`, clients in
  `progs/treesit`). Syntax of one piece of code, synchronous, also for code
  blocks of documents and session inputs. See `doc/tree-sitter.md`.
- `wip_lsp`: a client of language servers (C++ link in
  `System/Link/lsp_link.cpp`, protocol and clients in `progs/lsp`). Meaning
  over a project, asynchronous, for source files only. See the developer
  chapter `devel/source/lsp.en.tm`.

Both are merged here as they are (b465277952, 773de385bc). This document is
the design of the mechanism they are to share, so that each of them is an
optional provider: a build (or a run) without one of them keeps working with
the other, and one without both is the TeXmacs of `maxs_texmacs`.

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

## 2. Where they meet today (in this merge)

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
- The Focus menu: entries of both.
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

- syntax and immediate (colours, brackets, moves, selection, indentation on
  Return, session input, comments, structural edits): tree-sitter; a server
  only where there is no grammar (selectionRange, onTypeFormatting);
- meaning or project (diagnostics, completion, hover, signature, definitions
  and references over files, rename, fixes, formatting, symbols, calls, hints,
  semantic colours): the server; tree-sitter where there is no server
  (definitions and highlights in the same code, syntax errors as
  diagnostics);
- both (outline, folds): the server when one follows the file, else
  tree-sitter.

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

1. Merge (done): `wip_ts` + `wip_lsp`, conflicts resolved conservatively, the
   two mechanisms side by side; the 55 suites pass (treesit 423 checks, lsp
   350). In a window, clangd follows a C file with tabs (colours, folds); the
   C files are not highlighted by tree-sitter yet: the grammar `c` is for the
   tests in `wip_ts`, and the format `c` comes from `wip_lsp` (phase 4, the
   table of languages).
2. The core pieces with the current behaviour: positions (3.2), line state
   with layers (3.3) and folds (3.4) in the core; the header includes of the
   core on the features removed; tree-sitter's helpers tab-aware.
3. Source regions (3.1): the diff out of `syntax_region.cpp`; tree-sitter and
   the LSP as listeners; per-edit timings on large files (s7.c, parser.c).
4. Providers (3.5): the `code-*` commands, the preferences, the menu and keys;
   the commands of both moved behind them; the language table with servers.
5. Fallbacks across: syntax errors of tree-sitter as diagnostics without a
   server; LSP selectionRange and documentSymbol without a grammar; later,
   servers for code blocks of documents (virtual documents).
6. Builds without each feature (configure options), the suite `code`, docs:
   a developer chapter on the code features and one user page per feature.

Each phase keeps the suites passing and the two feature branches unchanged:
what is shared is written here, then given back to the branches (or the
branches rebased on it) only when the user decides it.
