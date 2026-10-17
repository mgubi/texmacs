# Tree-sitter in TeXmacs: survey and design

Branch `wip_tree_sitter` (from `maxs_texmacs`). Status: design, nothing
implemented yet. Measurements below were made on 2026-10-09 on an Apple
Silicon Mac with `cc -O2`.

## 1. What tree-sitter is

Tree-sitter is an incremental parsing library: a small C runtime plus one
generated C parser per language. It produces a concrete syntax tree, updates
it after an edit by reusing the unchanged parts, and recovers from syntax
errors (the tree always exists, with `ERROR` and `MISSING` nodes). Editors
(Neovim, Emacs 29+, Helix, Zed) use it for highlighting, indentation,
structural selection, folding and outlines.

| Fact | Value |
|---|---|
| Version | 0.27.1 (2026-10-08), MIT licence |
| Runtime | `lib/src`, about 17k lines of C11, libc only; one translation unit `lib.c`; 160 KB of code |
| Grammar ABI | runtime accepts 13 to 15; all the grammars below are 14 or 15 |
| Input | a `read (byte, point)` callback returning chunks; UTF-8, UTF-16 or a **custom decode function** |
| Edits | `ts_tree_edit` (byte offsets and row/column points), then a reparse with the old tree |
| After a reparse | `ts_tree_get_changed_ranges` gives the ranges whose structure changed |
| Queries | S-expression patterns with captures (`@keyword`), run on a byte or point range |
| Sub-ranges | `ts_parser_set_included_ranges` parses only some ranges (language injections) |

What the C runtime does **not** provide and we must write:

- The highlighting logic. The C API returns raw captures. Precedence between
  patterns, injections and local-variable tracking are in the Rust crate
  `tree-sitter-highlight`.
- Text predicates. `#eq?`, `#match?`, `#any-of?` are only stored with the
  query; the host evaluates them. `#match?` needs a regular expression engine
  (TeXmacs has none today; `std::regex` is available).
- Indentation. There is no standard: Neovim, Helix and Zed each define their
  own `indents.scm` captures.
- Loading grammars compiled to WebAssembly needs wasmtime, which is out of
  reach for us.

### Speed

Benchmark of the C API (`bench.c` of the session, to be kept as a test):

| File | Full parse | Reparse after one typed character | Highlight query, 60 lines | Highlight query, whole file |
|---|---|---|---|---|
| `glue_basic.cpp` (336 KB, 12.5k lines) | 22 ms | 1.3 ms | 0.04 ms | 9 ms |
| `remote-test.scm` (105 KB, 2.1k lines) | 9 ms | 0.15 ms | 0.3 ms | 10 ms |
| `s7.c` (4.2 MB, 105k lines, C++ grammar) | 350 ms | 40 ms mean, 88 ms worst | 0.1 ms | 133 ms |

Compiling the C++ highlight query takes 16 to 20 ms, once per language.
For any file a person edits in TeXmacs, parsing is far cheaper than
typesetting. The 4 MB file shows where the limit is.

### Size of the grammars

This is the real cost. Generated parsers:

| Language | `parser.c` (+scanner) | Object | Queries shipped by the grammar |
|---|---|---|---|
| json | 27 KB | 8 KB | highlights |
| lua | 0.35 MB | 61 KB | highlights, injections, locals, tags |
| scheme (6cdh) | 0.3 MB | 105 KB | highlights |
| java | 2.5 MB | 418 KB | highlights, tags |
| javascript | 2.8 MB | 417 KB | highlights, injections, locals, tags |
| python | 3.4 MB | 458 KB | highlights, tags |
| r | 4.0 MB | 482 KB | highlights, locals, tags |
| c | 3.8 MB | 630 KB | highlights, tags |
| rust | 6.4 MB | 1.1 MB | highlights, injections, tags |
| bash | 9.7 MB | 1.4 MB | highlights |
| julia | 23.5 MB | 2.6 MB | highlights, injections, locals |
| haskell | 19.5 MB | 3.8 MB | highlights, injections, locals |
| scala | 26.7 MB | 4.0 MB | highlights, indents, locals, tags |
| ocaml | 23.8 MB | 4.7 MB | highlights, locals, tags |
| cpp | 25.3 MB | 5.5 MB | highlights, injections, tags |
| fortran | 35.8 MB | 6.5 MB | folds, highlights, indents, locals, tags |

Objects compress well (cpp 421 KB, julia 241 KB, python 66 KB with gzip).
For comparison `texmacs.bin` is 21 MB. The languages TeXmacs highlights today
(next section) would add roughly 20 MB to the binary and 130 MB of generated
C to whatever holds the sources.

## 2. How TeXmacs handles code today

Paths are relative to `src/`.

### Highlighting

- `language_rep` (`src/System/Language/language.hpp:133`) is the interface:
  `advance (t, pos)`, `get_color (t, start, end)`, `highlight (t)`. The
  typesetter calls it for **one string leaf at a time**
  (`concater_rep::typeset_prog_string`, `src/Typeset/Concat/concat_text.cpp:178`).
- Two mechanisms:
  - Hand-written tokenizers: dedicated classes for scheme, mathemagix, cpp,
    scilab, r, fortran; the generic `prog_language_rep` configured by
    `(parser-feature lan key)` for python, octave, java, javascript, json,
    julia, scala, dot, csv. Nothing is cached; each line is tokenized again
    at each typesetting.
  - Packrat grammars (`hl_lan != 0`): `packrat_highlight` stores the colours
    of each line in a `highlight_observer` attached to the tree; a
    modification removes it. Only `minimal` and `std-math` use it.
- Everything else (shell, session languages without a tokenizer) has no
  colours.
- Colours are 34 classes (`comment`, `keyword`, `constant_string`,
  `declare_function`, ...) mapped to the preferences `syntax:<lan>:<class>`
  (`language.cpp:220-318`).

Known weaknesses, all structural:

- No state between lines. Only `/* */` comments span lines, found by
  rescanning neighbour lines at each token; no multi-line strings (Python
  triple quotes, C++ raw strings, Julia).
- Typing a comment delimiter does not re-typeset the other lines.
- `scheme_language_rep` rescans up to 1000 characters back in a line.

### Editing

- `progs/prog/prog-edit.scm`: brackets by counting characters (unaware of
  strings and comments, as is `find_left_bracket` in
  `src/Data/String/analyze.cpp:1420`), `program-compute-indentation` hook.
- Indentation exists for scheme, and "after a colon" for python (with a
  known bug for `#` in strings). C++ has none.
- Structured selection (`semantic_select`, `src/Edit/Replace/edit_select.cpp:99`)
  goes through packrat and applies to math and `minimal` only.
- Absent: comment toggling, outline, folding, go to definition (except
  Scheme, from the running interpreter).

### Representation

- A source file is a `document` of plain strings, one per line, with initial
  `mode=prog`, `prog-language=<format>` (`attach_subformat`,
  `src/Texmacs/Data/new_buffer.cpp:419`). Code blocks (`cpp-code`, ...) and
  session inputs have the same shape inside a larger document.
- Strings are in the TeXmacs universal encoding: Cork bytes, and
  `<less>`, `<gtr>`, `<alpha>`, `<#3B1>` entities. String offsets are
  neither UTF-8 nor code point offsets.
- Tabs are expanded on load.

### Edit notification

All modifications go through `raw_*` in `src/Kernel/Abstractions/observer.cpp`:
each observer of the node and of its ancestors gets `announce (ref, mod)`
before and `done (ref, mod)` after, with the kind (`MOD_INSERT`,
`MOD_REMOVE`, `MOD_SPLIT`, `MOD_JOIN`, `MOD_ASSIGN`, ...), the path and the
tree. `highlight_observer` and `undo_observer` are existing consumers.

## 3. Design

### Principles

1. **Additive.** A language with a grammar uses tree-sitter; one without
   (mathemagix, maxima, ...) or a build without tree-sitter keeps the present
   tokenizers. No existing highlighter is removed.
2. **Same documents.** No change of the document format, of the tags or of
   the `syntax:<lan>:<class>` preferences. The syntax tree is derived data
   attached to the document tree, never saved.
3. **One service, several clients.** Highlighting is the first client; the
   same tree serves selection, brackets, indentation, outline, and Scheme
   tools.

### Layers

```
 Scheme tools: selection, brackets, indentation, outline, completeness of input
        |                                   (progs/prog/syntax-*.scm)
 Scheme glue: syntax-node-at, syntax-query, syntax-errors, syntax-sexp
        |
 ts_language_rep : language_rep        highlight (), advance (), get_color ()
        |
 syntax_tree (per code region)         TSTree + line table + observer
        |
 grammar registry                      name -> TSLanguage*, queries
        |
 tree-sitter runtime                   src/Plugins/TreeSitter/, USE_TREE_SITTER
```

### Code regions

A **region** is the unit that gets its own syntax tree: a `document` whose
lines are typeset in prog mode with one `prog-language` (a source file, the
body of a `cpp-code`, one session input), or a single string (inline
`<cpp|...>`).

- The region of a string leaf is its parent when the parent is a `document`,
  else the leaf itself.
- A line which is not a plain string (markup inside code: a `with`, a
  formula, a reference in a literate document) is given to the parser as its
  flattened text, and gets colours only on its direct string children.
  To be refined once the simple case works.
- Trees outside the edited document (exports, menus, tests typesetting a
  detached tree) have no observers: they are parsed statelessly, with a small
  cache keyed by the tree.

### Feeding TeXmacs strings to the parser

No copy of the text is made. The parser reads through `TSInput.read`, which
receives the row and column: row `i` is line `i` of the region, followed by a
virtual `\n`.

The encoding problem is solved by `TSInputEncodingCustom`: a decode function
turns a Cork byte or a `<...>` entity into one code point and reports how
many bytes it consumed. Consequences:

- tree-sitter columns **are** positions in the TeXmacs string; no mapping
  table, no conversion on the way back;
- the lexers see real Unicode (`λ`, `≤` in Julia or Scheme identifiers work);
- entities unknown to the converter map to U+FFFD.

A line table (length of each line) gives the byte offset of a row.

### Keeping the tree up to date

A `syntax_observer` on the region root translates each modification below it
into a `ts_tree_edit`:

| Modification | Edit |
|---|---|
| `MOD_INSERT` / `MOD_REMOVE` in a string | insertion or deletion inside a row |
| `MOD_SPLIT` / `MOD_JOIN` on the document | a newline inserted or removed |
| `MOD_INSERT` / `MOD_REMOVE` of lines | several rows |
| `MOD_ASSIGN` of a line | the row replaced |
| anything else (assignment of the root, change of structure) | drop the tree, parse again |

The reparse is lazy, at the first request after an edit. After it,
`ts_tree_get_changed_ranges` names the other lines whose structure changed
(a comment opened above them): their cached colours are dropped and they are
`touch`ed so that the typesetter does them again. This is the path
`detach_highlight` already uses for packrat. The point to settle in the
prototype is **when** the reparse runs, so that lines are not invalidated
in the middle of a typesetting pass (candidate: in `done`, or before
`typeset_prepare`).

### Highlighting

`ts_language_rep` behaves like the packrat path: `highlight (t)` fills a
per-line colour array (kept in the existing `highlight_observer`), and
`advance` / `get_color` read it.

- The query runs on the rows of the requested line only.
- Precedence follows `tree-sitter-highlight`, so that upstream query files
  work unchanged. To be pinned by a test against `tree-sitter highlight`.
- Capture names map to the 34 TeXmacs classes by a table in Scheme
  (`@keyword` -> `keyword`, `@string` -> `constant_string`,
  `@function` -> `declare_function`, `@comment` -> `comment`, ...; longest
  prefix wins, `@keyword.control.repeat` -> `keyword_control`). User colours
  keep working.
- `ERROR` and `MISSING` nodes can use the existing `error` class
  (preference, off by default).
- Predicates `#eq?`, `#any-of?`, `#match?`, `#not-...`, `#set!` are
  evaluated in C++; regular expressions are compiled once per query.
- Query files live in `TeXmacs/progs/syntax/<lan>/{highlights,locals,tags,indents}.scm`,
  copied from the grammar and adapted; a file in `$TEXMACS_HOME_PATH`
  overrides. For Scheme we write our own, using the TeXmacs keyword lists
  (`tm-define`, `tm-menu`, the glue symbols), since the generic grammar only
  knows lists and symbols.

### Scheme interface

No node objects cross into Scheme (their lifetime is tied to a tree which
changes at each keystroke). Requests take a tree or the cursor and return
plain data with TeXmacs paths:

| Function | Result |
|---|---|
| `(syntax-supported? lan)` | whether a grammar is loaded |
| `(syntax-node-at t pos)` | the nodes around a position, innermost first: `(type named? start end)` |
| `(syntax-query t name [start end])` | the captures of the query `name`: `(capture start end)` |
| `(syntax-errors t)` | the ranges of error and missing nodes |
| `(syntax-sexp t)` | the tree as an S-expression, for debugging and tests |

### Clients, in order

1. **Highlighting** for every language with a grammar, including multi-line
   constructs, and for languages which have none today (shell via bash, rust,
   lua, ...).
2. **Structural selection and brackets**: `select_enlarge` walks
   `syntax-node-at`; bracket matching takes the node's delimiters and so
   ignores strings and comments.
3. **Sessions**: `syntax-errors` tells whether an input is complete (Return
   evaluates or opens a new line), and marks errors before evaluation.
4. **Indentation** through `program-compute-indentation` and `indents.scm`.
   The capture convention has to be chosen (Neovim's files cover the most
   languages, Apache 2.0; quality varies).
5. **Outline and navigation** from `tags.scm`: list of definitions of a
   buffer, go to definition inside the file, comment toggling.
6. **Injections** (JavaScript in HTML, code blocks in Markdown) and use by
   converters. Later, if at all.

Folding is left out: TeXmacs folds are tags in the document, and a source
file must stay a plain list of lines.

### Build

- Runtime vendored in `src/Plugins/TreeSitter/tree-sitter/` (0.7 MB of
  source), compiled as one unit, under `USE_TREE_SITTER`
  (`--with-tree-sitter`, default on; CMake option; `misc/wasm/config.h`).
- Objects are flat `Objects/<basename>.o`: the unit is named
  `tree_sitter_lib.c`, and each grammar gets a unit `ts_<lan>.c` including
  its `parser.c` and `scanner.c` (to check: clashes of static names inside
  one unit).
- New C++ files go into `misc/wasm/sources.txt`.

### Where the grammars come from (open decision)

| Option | For | Against |
|---|---|---|
| A. Generated parsers committed in the repository, linked statically | simplest; identical on every platform and in the browser | 130 MB of generated C in git for today's languages, growing at each grammar update; +20 MB binary |
| B. Generate from `grammar.js` at build time | small sources | needs the tree-sitter CLI (Rust, Node for some grammars) in every build and CI |
| C. Shared libraries loaded at run time (as Emacs, Neovim, Helix) | binary unchanged; users add languages | we must host binaries per platform or ask users for a compiler; impossible in the browser build |
| D. Pinned list fetched at build time, linked statically | repository stays small; reproducible (commit hashes); same pattern as `build-deps.sh` | builds need the network or a cache; binary size as in A |
| E. `grammar.json` shipped, tables generated in Scheme at first use and cached (section 6) | no C, no binaries, no build step; 14-240 KB of JSON per language; same in the browser | parsing is interpreted (45-65x slower than C under s7, see 6); external scanners must be ported to Scheme by hand |

Recommendation: **D**, with the list of languages a configure option, and
**C** added later for user-installed grammars. The prototype uses D with
scheme, python, cpp, julia and javascript. The browser build needs its own
choice of languages (cpp alone is 0.4 MB compressed) and maybe a later
split into lazily loaded modules.

### Grammars for the TeXmacs languages

| TeXmacs language | Grammar | Note |
|---|---|---|
| scheme | 6cdh/tree-sitter-scheme | generic S-expressions; our own queries |
| cpp, java, javascript, json, julia, python, scala | tree-sitter organisation | maintained |
| r | r-lib/tree-sitter-r | maintained |
| fortran | stadelmanma/tree-sitter-fortran | the largest parser |
| shell | tree-sitter-bash | new highlighting |
| octave, scilab | a MATLAB grammar | to evaluate; Scilab syntax differs |
| dot, csv | community grammars | to evaluate |
| mathemagix, maxima and other session languages | none known | keep the present highlighters |

## 4. Risks

- **Markup inside code.** TeXmacs lets any markup appear in a code line.
  The flattening rule above must not make things worse than today for
  literate documents.
- **Invalidation during typesetting.** See "Keeping the tree up to date".
- **Several views, undo, remote editing.** All go through the same observer
  stream, so they should be covered; to be tested explicitly.
- **Binary and download size**, mainly for the browser build.
- **Query quality.** Upstream `highlights.scm` files are uneven, and each
  editor maintains its own improved copies; we will end up maintaining ours
  for the main languages.
- **Grammar ABI drift.** Pinned commits and the vendored runtime move
  together.

## 5. Plan (superseded by section 7)

1. Runtime in the build, grammar registry, fetch script; `syntax-sexp` on a
   detached tree; unit test of the Cork decode function.
2. `ts_language_rep` highlighting without incremental update (parse the
   region at each change); `parse-test.scm` style tests for python, cpp,
   scheme; comparison with the present colours.
3. `syntax_observer`, incremental reparse, invalidation of changed lines;
   tests typing comment delimiters and multi-line strings; timing on
   `glue_basic.cpp`.
4. Selection and brackets.
5. Sessions (complete input), then indentation, then outline.
6. Decision on distribution and on the browser build, with measured sizes.

## 6. Experiment: tree-sitter in Scheme

Branch `wip_ts_experiment` (from `maxs_texmacs` 1fcf4aaf39), started
2026-10-16. A grammar is data (`src/grammar.json`, written by `tree-sitter
generate` from `grammar.js` and committed in every grammar repository), so
the generator can be ported to Scheme and its tables run by a Scheme port
of the runtime: no generated C, no shared libraries, nothing to build.

### What is there

`TeXmacs/progs/treesit`, ports of tree-sitter 0.27.1:

| Module | Port of | Content |
|---|---|---|
| `ts-base` | | symbols, token sets, character sets, growable vectors, UTF-8, a JSON reader |
| `ts-grammar` | `parse_grammar.rs`, `prepare_grammar/*` | reading `grammar.json`, symbols, token extraction, repeats, flattening, default aliases, inlining |
| `ts-nfa` | `expand_tokens.rs`, `nfa.rs`, `pattern.rs` | the patterns (with the simplifications of `regex_syntax`), the NFA of the tokens |
| `ts-unicode` | | `\p{L}`, `\p{XID_Start}`... generated by `misc/tree-sitter/gen-unicode.py` from Python's `unicodedata` |
| `ts-tables` | `item.rs`, `item_set_builder.rs`, `build_parse_table.rs` | LR(1) item sets and the parse table with tree-sitter's conflict resolution |
| `ts-build` | `build_tables.rs`, `token_conflicts.rs`, `minimize_parse_table.rs`, `build_lex_table.rs` | token conflicts, keywords, error state, state merging, lex tables (DFAs) |
| `ts-language` | `render.rs` (the data of `parser.c`) | the runtime tables as plain data, saved and read back |
| `ts-parse` | `parser.c`, `stack.c`, `subtree.c`, `lexer.c` | the GLR parser with error recovery, reuse of an edited tree (`ts_subtree_edit`), nodes and fields, `ts_node_string` |
| `ts-scanner-python` | `scanner.c` of tree-sitter-python | the external scanner (indentation, strings) |

An external scanner is `#(create scan deserialize serialize)`: `scan` gets
the lexer (`lexer-lookahead`, `lexer-advance!`, `lexer-mark-end!`,
`lexer-set-result!`, `lexer-eof?`, `lexer-column`) and the vector of valid
external tokens; the state is any Scheme value compared with `equal?`.

The text is a vector of code points; offsets count code points, which fits
the decoding of TeXmacs strings planned in section 3.

### Checks

- Tables: on JSON, Go, Java, C and Python the counts are those of `tree-sitter
  generate` 0.27.1 (parse states, symbols, lex and keyword lex states,
  production infos); e.g. C 2015 / 360 / 324 / 512 / 131.
- Trees: all the corpus tests of tree-sitter-json (7), -go (67), -java (108),
  -c (84) and -python (117, those marked `:error` checked as `tree-sitter
  test` does) give their expected trees. The trees of `lib/src/parser.c` of
  tree-sitter (80 KB of C, with one missing `;`) and of Python's
  `argparse.py` (98 KB) are those of the C parsers, error recovery included.
- Edits: after 20 insertions in identifiers and 30 random edits of
  `parser.c`, the reparsed tree is always the tree of a full parse.
- Suite `treesit` (`check/treesit-test.scm`, 47 checks, in `check-master`):
  helpers, the tables and trees of the JSON grammar of `misc/tree-sitter`,
  edits, saving a language, a small grammar written in the test.

### Measurements (Apple M-series, s7)

| | standalone s7 | in TeXmacs (S7) | C |
|---|---|---|---|
| generate C tables | 9.7 s | 35 s | 0.16 s (CLI) |
| generate Go / Java / Python | 4.5 / 8.2 / 6.1 s | | |
| load the saved C tables (3.3 MB of Scheme data) | 0.1 s | | |
| parse `parser.c` (80 KB) | 0.31 s | 1.2 s | 6.6 ms |
| parse `argparse.py` (98 KB) | 0.39 s | | 6 ms |
| reparse `parser.c` after typing in an identifier | 12 ms | 47 ms | |

Saved tables: JSON 11 KB, C 3.3 MB (uncompressed text; the C `parser.c`
generated by the CLI is 3.9 MB). The tables could be generated at first use and cached in
`$TEXMACS_HOME_PATH`, or generated once and shipped.

Inside TeXmacs, code loaded as a module runs about 3 times slower than the
same code defined at top level: a closure whose environment is not the
rootlet is slow in the s7 of TeXmacs (a recursive list insertion: 170 ms
in a module, 50 ms in the rootlet), while a standalone s7 built from the
same `s7.c` shows a 20 % difference only. The cause is not found yet; it
concerns all module code of TeXmacs, not only this one.

### Open

- External scanners: each grammar with a `scanner.c` needs a port (Python
  done). Without one: json, c, go, java. With one: cpp, javascript, python,
  julia, bash, r, scala, fortran (to size).
- Queries (`highlights.scm`) are S-expressions; a query engine for the
  patterns and predicates used by highlighting is still to write.
- Speed: parsing is 45-65x slower than C under s7; incremental reparsing
  makes typing cheap, a first parse of a large file is not. The 3x of the
  module environment (above) is the first thing to understand.
- Guile and femtolisp: only S7 was tried in TeXmacs; the code avoids
  S7-specific forms (own sort, maps from left to right, own writer of data).

### The hybrid: Scheme tables, C runtime

The tables generated in Scheme can also run in the C runtime of tree-sitter,
vendored in `src/Plugins/TreeSitter/tree-sitter` (0.27.1, MIT, 0.8 MB):
`ts-export-language` (`ts-export.scm`) writes them, `ts_dynamic.c` fills a
`TSLanguage` with them (every state a large state; the lex functions
interpret the lex tables, through one of 32 slots since the runtime calls
them without the language; minimal query tables). Glue:
`tree-sitter-language-new`, `tree-sitter-parse-sexp`. No C is generated per
grammar.

- JSON 7/7 and C 85/85 corpus tests pass with it (standalone harness); the
  tree of `lib/src/parser.c` is that of the Scheme parser and of the C
  parser; after 30 random edits the incremental trees are those of the
  compiled tree-sitter-c parser at every step (both differ from a fresh
  parse after edits with errors, which is tree-sitter's behaviour).
- Speed in TeXmacs: `parser.c` in 6 ms (Scheme parser 2.2 s). The C tables
  load in 1.7 ms from data of 1 MB; their export from Scheme takes 1.4 s in
  TeXmacs, so the exported data should be cached rather than the Scheme
  language.
- Only the autotools build compiles it so far (not CMake, not the browser
  build). External scanners would be callbacks into Scheme (not done).

Queries run in the C engine too (`tree-sitter-query-new`,
`tree-sitter-query-captures`, which evaluates `#eq?`, `#match?`,
`#any-of?` and their negations). This needs the query tables of `parser.c`
from the generator (public symbols, aliases of non-terminals, primary
states, supertypes); with them the captures of `highlights.scm` and
`tags.scm` of tree-sitter-c and of `highlights.scm` of tree-sitter-json are
those of the compiled parsers, and with predicates the 8814 captures of C
`highlights.scm` on `parser.c` are those of `tree-sitter query`, in order
(7 ms for parse and query in TeXmacs). The `.scm` query files are not
Scheme: s7 reads them, Guile stops at `#match?`, and captures,
quantifiers and anchors follow their node, so the C engine is the simpler
way to use them.

With the hybrid, option E keeps its advantages (no compiled code per
grammar) without the speed penalty; the generator stays in Scheme and runs
once per grammar (10-35 s for C), its output can be cached or shipped.

## 7. Plan for the TeXmacs side (with the hybrid)

The design of section 3 stands; what changes is where the grammars come
from (tables generated in Scheme, run by the vendored C runtime) and that
the runtime, the language loader and queries already exist
(`Plugins/TreeSitter`, `progs/treesit`). Each phase ends with tests and can
be merged on its own.

### Phase 1: grammar registry and cached tables

Done (2026-10-17, commit "grammars of json, c and scheme, generated at
first use"): `ts-registry.scm` (`treesit-language`, `treesit-prepare`,
`treesit-generate-cache`), the grammars in `misc/tree-sitter`, the glue
`tree-sitter-language-load`, CMake. The background generation runs
`texmacs.bin -headless -x ... (exit 0)`; a test of the whole asynchronous
path needs `-headless -X` (the loop keeps running), so it is not in the
suite. As planned initially:

- `progs/treesit/ts-registry.scm`: for a TeXmacs language name (`cpp`,
  `python`, `json`...), the grammar (`misc/tree-sitter/<lan>/grammar.json`),
  its queries and, when there is one, its Scheme scanner.
- Tables: the exported data (`ts-export-language`) cached in
  `$TEXMACS_HOME_PATH/system/cache/tree-sitter/<lan>-<hash>.tsdyn`, the
  hash being that of `grammar.json` and of the generator version, made at
  the first use of the language (decision 2 below): until then the present
  tokenizer is used.
- Load the data from C directly (no Scheme string of 1 MB): the glue takes
  a file name.
- Builds: CMake compiles `Plugins/TreeSitter` too (the browser build,
  `misc/wasm/sources.txt`, later); `USE_TREE_SITTER` and `syntax-supported?`
  let the rest of TeXmacs work without it.
- Tests: the registry, the cache (a changed grammar regenerates), loading
  every shipped language.

### Phase 2: syntax trees of code regions

Done (2026-10-17): `Plugins/TreeSitter/syntax_region.cpp`, glue
`syntax-region-sexp`, `syntax-region-node-at`, `syntax-region-errors`,
`syntax-region-statistics`, `syntax-region-forget` (region tree and
language index; results with (row column) positions in the TeXmacs
strings). One change to the design below: **no observer**. A region keeps
the lines of its last parse (TeXmacs strings, shared, so the copy is
cheap); at each request the lines are compared with the current ones and
the difference becomes one `ts_tree_edit` (first and last changed lines,
then the common prefix and suffix of the changed text). This covers every
kind of modification, undo and remote edits included, without following
the modification stream, and costs one string comparison per line per
request. Regions are kept for the 32 most recently used roots (by tree
identity). Tests: 40 random edits of `progs/treesit/ts-parse.scm` (typing
in symbols, splitting and joining lines), each reparsed incrementally and
compared with a full parse; entities and Cork characters at their string
positions; error and missing ranges. As planned initially:

- C++ `syntax_tree` (in `Plugins/TreeSitter`): the `TSTree` of a region (a
  `document` of lines in prog mode, or one string), a line table and the
  parser. Input through `TSInput.read` row by row with
  `TSInputEncodingCustom`: the decode function turns a Cork byte or a
  `<...>` entity into one code point, so that tree-sitter columns are
  string positions (section 3). Lines which are not strings are flattened.
- `syntax_observer` on the region root: the modifications of section 3
  become `ts_tree_edit`; the reparse is lazy, at the first request.
- Glue for tests and tools: `syntax-sexp`, `syntax-node-at`, `syntax-errors`
  on a tree with paths.
- Tests: the trees of documents loaded from files equal those of
  `tree-sitter parse`; after scripted edits (typing, splitting and joining
  lines, undo) the incremental tree equals a fresh parse; entities and
  non-ASCII text map to the right columns.

### Phase 3: highlighting

Done (2026-10-17), with these differences from the plan below:

- `Plugins/TreeSitter/ts_highlight.cpp`: `prog_language` wraps the usual
  language of `scheme`, `json` and `c` in a `treesit_language_rep`, which
  delegates to it until Scheme installs a highlighter
  (`progs/treesit/ts-highlight.scm`, asked for by C++ at the first
  highlight of the language: `treesit-highlighter-request`; the tables are
  then loaded from the cache or generated in the background, and the open
  buffers typeset again). Preference `tree-sitter highlighting` (default
  on); `tree-sitter-set-highlighting` switches it off without writing
  preferences (the `parse` suite does, to test the tokenizers).
- The region of a line is its parent document, found by the ip of the line;
  a detached line or a single string is a region by itself. The query runs
  on the whole region once per parse (`syntax_region_highlight` caches the
  colours per line).
- Overlaps as tree-sitter-highlight really does: for one node the **last**
  pattern wins (`highlight.rs`: "keep iterating over any later highlighting
  patterns that also match this node"), an inner node over an outer one.
  The upstream queries are written for this.
- Capture names to classes: `treesit-capture-classes`, longest dotted
  prefix; unknown names (`@variable`) keep the colour of the text. The
  colours are those of the classes (`syntax:<lan>:<class>` preferences), so
  Scheme code now has the palette of the other program languages, not the
  environment colours of `scheme_language_rep`.
- Queries: `misc/tree-sitter/c/highlights.scm` from tree-sitter-c,
  `json/highlights.scm` from tree-sitter-json, and our own
  `scheme/highlights.scm` (the forms of TeXmacs, `<less>` and `<gtr>`: the
  predicates see the TeXmacs strings).
- Lines whose colours change without being modified are typeset again by
  `treesit-schedule-updates` (delayed, `update-path`).
- No `cpp` yet: the TeXmacs `cpp` mode needs tree-sitter-cpp (phase 4, its
  scanner). Typing in `parser.c` (2316 lines): first highlight 94 ms, then
  4.8 ms per edit (reparse plus the whole query); restricting the query to
  `ts_tree_get_changed_ranges` is left for later.

As planned initially:

- `ts_language_rep : language_rep`, chosen by `prog_language (lan)` when the
  registry has the language: `highlight (t)` runs the query on the rows of
  the line and stores colours in the existing `highlight_observer`
  (`attach_highlight`), `advance` and `get_color` read them, like the
  packrat path in `language.cpp`.
- Overlaps resolved as in `tree-sitter-highlight` (first pattern wins for
  a node; nested captures stack).
- Capture names map to the TeXmacs colour classes by a table in Scheme,
  longest prefix first (`@keyword` -> `keyword`, `@string` ->
  `constant_string`, `@function` -> `declare_function`, `@type` ->
  `declare_type`, `@comment` -> `comment`...); the `syntax:<lan>:<class>`
  preferences keep working.
- Invalidation: after a reparse, `ts_tree_get_changed_ranges` names the
  lines whose colours change (a comment or a string opened above them);
  their highlight is detached and they are touched, outside the typesetting
  pass (the point to settle, section 3).
- First languages: json, c and scheme (no scanner), then cpp and python
  (phase 4).
  The present tokenizers stay for the other languages and when the build
  lacks tree-sitter.
- Tests: colours of sample files per language compared with
  `tree-sitter highlight` (as classes); typing `/*` and `"""` re-colours the
  following lines; timing of typing in a large file (`glue_basic.cpp`).

### Phase 4: external scanners

- The scanners stay in Scheme (`ts-scanner-python.scm` exists): the C
  runtime calls a C trampoline which calls the Scheme `scan` with a lexer
  object wrapping `TSLexer` (`lexer-lookahead`, `lexer-advance!`...);
  serialize and deserialize store the Scheme state in the bytes of
  tree-sitter (`object->string` and `read`, or an id in a table).
- Ports: cpp (raw strings), javascript, julia, bash, r; each checked with
  its corpus as python was.
- Tests: the corpus of each grammar through the C runtime; timing (a scanner
  call per token boundary).

### Phase 5: clients

In the order of section 3: structural selection and brackets
(`syntax-node-at`), complete inputs of sessions (`syntax-errors`),
indentation (`indents.scm`), outline (`tags.scm`).

### Decisions

1. Decided (2026-10-17): the first languages are json, c and **scheme**
   (for TeXmacs's own code; the grammar is generic, the queries are ours).
2. Decided (2026-10-17): the tables are **generated at first use** and
   cached in `$TEXMACS_HOME_PATH`; only `grammar.json` and the queries are
   shipped. Consequences for phase 1: the first opening of a file of a
   large grammar costs the generation (35 s for C in TeXmacs today, 10 s
   under a standalone s7), so it must not block: the present tokenizer
   colours the file until the tables are ready, with a message, and the
   generator should get faster (the 3x of module code under S7, section 6,
   is the first thing to look at). No build-time target.
3. Decided (2026-10-17): the work is based on `maxs_texmacs`
   (`wip_ts_experiment` started from it). The desktop builds come first;
   the browser build later.
