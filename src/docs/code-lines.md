# Blocks of lines of code (`code-lines`)

A source file of hundreds of thousands of lines (#710: a 19.8 MB `parser.c`
passed 3 GB while it was typeset) is typeset as a **block of lines**: its
document of lines is typeset by a bridge and a box of their own, chosen by the
environment variable `code-lines`. A plain line is a row of a monospaced font
on a grid of cells, with no line items and no paragraph of its own in papyrus
mode. The features of programs (tree-sitter, the language servers, see
[code-features.md](code-features.md)) work in it as in any document of lines:
the tree is the same, only its typesetting differs. Tracking issue: #797.

## The tree and the variable

    (document "line 0" "line 1" ...)      with code-lines = true

- **No tag.** The lines are an ordinary document; the environment variable
  `code-lines` (`true`, default `false`, `env_default.cpp`) says that it is
  typeset as lines of code. A source or text file has it in its initial
  environment; a block in another document is
  `<with|code-lines|true|<document|...>>`. Everything which works on a
  document of lines (editing, undo, search, the cursor, the converters, the
  language servers, tree-sitter, `go-to-line`) works unchanged, and an older
  TeXmacs, which ignores the variable, shows and edits the lines as
  paragraphs. (A first version had a tag `code-lines` around the document:
  every place which lists the primitives, the cursor, the focus and the
  converters needed a case for it, and the lines of a buffer were no longer
  its body; see #798.)
- **Loading.** `source_document_to_tree` (source files, saved as they were
  loaded) and `verbatim_document_to_tree` (texts) set the variable unless
  the preference `code lines` is `off`; `TEXMACS_CODE_LINES=0` or `1`
  overrides it (tests). `code_lines_wanted` in
  `Data/Convert/Verbatim/verbatim.cpp`; also a file of one line or none.
  Preferences › Convert › Verbatim (Source code) has the preference. Format ›
  Source code › Lines of code switches a document (`toggle-code-lines`, in
  `generic/format-menu.scm`, also offered for texts: `code-lines-text?`); a
  source file or a text is not modified by it, since it does not save it.
- **Saving.** The lines are saved as before; `tree_to_source` gives the file
  back byte for byte (tabs and the other control characters are `<#9>`,
  `<#x>`; texts keep them too now). LaTeX writes the tabs of verbatim
  content as spaces up to the next stop of 8 (`tmtex-expand-tabs`).
- **The choice of the bridge** (`Typeset/Bridge/bridge_document.cpp`). The
  bridge of a document reads `code-lines` when it is typeset (the variable is
  then one of its inputs, so that a change of it typesets the document
  again) and either makes the bridges of its paragraphs, one per line, or
  hands the document to `bridge_code`, which makes none; the notifications
  and the typesetting then go to the one in use. Its paragraph bridges are
  made at the first typesetting only, not when the bridge is made, so that a
  long file never has them. A `document` typeset by the concater (inside a
  line, a cell) is typeset as paragraphs.

## Typesetting (`Typeset/Bridge/bridge_code.cpp`)

- **Records of the lines.** A line has a record when it needs one: wrapped
  into several rows (`code_breaks`, at the width of the page; a tab goes to
  its stop, see below), with markup or layers, or always in paper mode. The
  notifications (`notify_insert`, `remove`, `split`, `join`, `assign`) keep
  the records at the places of their lines (`lines_inserted`,
  `lines_removed`, `line_changed`); the tree of the editor is changed in
  place, `st` is not rebuilt. A short plain line has no record and is looked
  at again only when it changes (`plain`). `TEXMACS_TYPESET_CACHES=check`
  makes them all again and counts the differences (suite `code-lines-check`).
- **Papyrus.** The block is one box (`code_box`, `Typeset/Boxes/Basic/
  code_boxes.cpp`), which draws the visible lines from the live document, and
  a sparse list of *special* lines: wrapped lines (their texts and the starts
  of their rows), lines typeset by the concater (their boxes), folded lines.
  The depth of a line is `k * pitch` plus what the special lines before it
  add (`tall_cum`), so a click, a cursor or a redraw finds its line by a
  binary search. The box has no children for the generic code; `subbox (k)`
  makes the box of a line when it is asked for.
- **Paper.** A line is a paragraph of its rows (`code_row_box`). The rows are
  made as those of templates of paragraphs of 1 to 5 rows, so that the page
  breaker gets the penalties of an ordinary document
  (`stacker_rep::new_paragraph`), and are made again only for the lines which
  changed (`psig`). The first and the last line are typeset with the line
  items around the block.
- **Lines typeset by the concater** (`rich_line`): a line with markup (strong,
  em, colours, math, images...), or with layers (below). Its items are put on
  the grid of the cells (`rich_paragraph`): a string a cell per character (a
  wider font is split into its characters), another box the cells it needs.
  With `code-tall-objects` = `shrink`, an object taller than a row is scaled
  to it (`code_fit_box`, which scales the clicks, the cursor and the
  selections too).

## The features of programs

- **Colours.** A language with states at the starts of the lines
  (`language_rep::has_line_states`, `colour_line`: prog and C++, for the
  comments of several lines) is coloured by a lexer of the block
  (`code_lexer`), from the first line whose state is not known; the colours
  of the lines drawn last are kept. Another language is asked each time a
  line is drawn, on the line of the document: tree-sitter's wrapper reads the
  highlight observers of the line, which `treesit-schedule-updates` changes
  after a parse (an `update-path` of the line, which drops what the block
  kept for it).
- **Layers** (`code_line_has_layers`, `code_line_layers_of`: the colours and
  the inlay hints of a server). Such a line is typeset by the concater
  (`typeset_layered_string`) and put on the grid; a hint inside a token comes
  before the token.
- **Folds** (`code_line_fold_state`). A hidden line takes no room and is not
  drawn: in papyrus, the lines after it are a line higher; in paper, its page
  items are hidden, as `bridge_rep::fold_line` does. The line before hidden
  lines ends with `...`. Not when printing (`typeset_folds`). The block looks
  for folds only when the core says some line may be folded
  (`code_folds_possible`).
- **Tabs** (`<#9>`): they go to the next stop of `prog-tab-width` (the
  preference "editor:code tab width"); another control character is a cell
  wide. The positions of the characters (`code_xpos`) are those of the font,
  with the tabs at their stops: the cursor, the clicks, the selections and
  the soft line breaks use them. Tabs are always shown as an arrow;
  `code-invisibles` = `true` (Format › Source code › Show invisible
  characters) also shows the spaces, the ends of the lines and the control
  characters.

## Measurements

A C file of 200000 lines (40000 functions of 5 lines, with tabs and
comments), the cursor at the line 150000, s7 build, macOS (M4), headless,
4 November 2026; a key is `update-forced` and `apply-changes` after it:

| | with the block | without |
|---|---|---|
| load and typeset (papyrus) | 2.4 s | 9.5 s |
| a key, papyrus | 14 ms | 50 ms |
| Return, papyrus | 16 ms | 152 ms |
| to paper mode | 1.1 s | 5.6 s |
| a key, paper | 270 ms | 360 ms |
| Return, paper | 530 ms | 900 ms |

In paper mode, most of what is left is the page breaker: it reuses its
search for a key, but its passes over all the items (signature, matching
with the previous search, tables) run again when the number of lines
changes.

Tree-sitter's parts: a long region is parsed by parts of about 2000 lines,
cut at the nodes of the top level which start a line
(`Plugins/TreeSitter/syntax_region.cpp`). A file without such nodes (e.g.
200000 statements indented at the top level of a C file, which are errors)
is one part, which an edit parses again incrementally (about 0.4 s for
200000 lines), and which `group_update` tried to cut again at each key,
parsing it whole (1 s more): a fix of tree-sitter of its own, outside this
work, the same with and without the block.

## Tests

`code-lines` (in the regression suites, also as `code-lines-check`): the
places of the lines and the cursor as in a document of lines, edits,
wrapping, the rows made again in paper mode, the records, the colour states,
markup, tabs and invisible characters, shrunk objects, export and import,
the lines of a source file, folds, layers, tree-sitter in a block. The
sample `tests/documents/samples/code-lines.tm`.
