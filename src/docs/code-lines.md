# Blocks of lines of code (`code-lines`)

A source file of hundreds of thousands of lines (#710: a 19.8 MB `parser.c`
passed 3 GB while it was typeset) is typeset as a **block of lines**: the tag
`code-lines`, typeset by a bridge and a box of its own. A plain line is a row
of a monospaced font on a grid of cells, with no line items and no paragraph
of its own in papyrus mode. The features of programs (tree-sitter, the language
servers, see [code-features.md](code-features.md)) work in it as in a
document of lines: the block is one more client of the line state of the
core, not a mechanism of its own. Tracking issue: #797.

## The tree

    (document (code-lines (document "line 0" "line 1" ...)))

- **Loading.** `source_document_to_tree` (source files, saved as they were
  loaded) and `verbatim_document_to_tree` (texts) make the block unless the
  preference `code lines` is `off`; `TEXMACS_CODE_LINES=0` or `1` overrides it
  (tests). `code_lines_wanted` in `Data/Convert/Verbatim/verbatim.cpp`.
- **Saving.** `print_verbatim` writes the lines of the block as they are
  (`CODE_LINES`), so `tree_to_source` gives the file back byte for byte (tabs
  and the other control characters are `<#9>`, `<#x>`). HTML and LaTeX
  export the lines of a block as the paragraphs of the document around it
  (`splice-code-lines` in `tmhtml.scm` and `tmtex.scm`), as before the block.
- **The cursor.** The tag has an inner border (`drd_std.cpp`): the cursor is
  always in a line, never before or after the block, so that a file opens
  with the cursor in its first line, and Home and End of the document stay
  in the lines. The focus skips the block (`focus_search`), as a document:
  in a source file it is the buffer, as without the block.
- **The lines of a buffer.** `code_lines_root` (glue `code-lines-root`,
  `System/Language/code_lines.hpp`) is the one definition of "line *l*, offset
  *o*": the document of the lines, which is the one inside the block for a
  file loaded as a block, and the tree itself otherwise. Everything which
  addresses the lines of a buffer goes through it: the language servers
  (`lsp-target-root`, `lsp-target-path`, `lsp-target-lo`, the edits, the
  decorations), tree-sitter's region (`treesit_language_rep::highlight`),
  `go-to-line`. The folds and tree-sitter's colours find the lines of a line
  from its parent document, which is the inner document: they needed nothing.
  Tests which place the cursor in a source file ask for the lines the same
  way (`line-path` in `lsp-test`, `line-cursor` in `links-test`).

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

A C file of 200000 lines, the cursor at the line 150000, s7 build (#797):

| | paper | papyrus |
|---|---|---|
| a key, with the block | 0.8–0.9 s | 71–78 ms |
| Return, with the block | 1.8–2.0 s | 78 ms |
| a key, without the block | 1.3–5 s | |
| Return, without the block | 4.4–4.8 s | |

In paper mode, most of what is left is the page breaker: it reuses its
search for a key, but its passes over all the items (signature, matching
with the previous search, tables) run again when the number of lines
changes. In papyrus mode, most of what is left is tree-sitter's region
brought up to date at each key (`get_region` → `update_lines`).

## Tests

`code-lines` (in the regression suites, also as `code-lines-check`): the
places of the lines and the cursor as in a document of lines, edits,
wrapping, the rows made again in paper mode, the records, the colour states,
markup, tabs and invisible characters, shrunk objects, export and import,
the lines of a source file, folds, layers, tree-sitter in a block. The
sample `tests/documents/samples/code-lines.tm`.
