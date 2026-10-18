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
| `ts-scan` | | what the scanners share: their vectors, the names of their tokens, their loops over characters |

An external scanner is `#(create scan deserialize serialize)`: `scan` gets
the lexer (`lexer-lookahead`, `lexer-advance!`, `lexer-mark-end!`,
`lexer-set-result!`, `lexer-eof?`, `lexer-column`) and the vector of valid
external tokens; the state is any Scheme value compared with `equal?`
(and not modified once serialized). `ts-scan.scm` makes these vectors and
has the loops shared by the scanners (see phase 4).

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
concerns all module code of TeXmacs, not only this one. (Found later for
the calls of public functions of other modules: see "Scanner speed" in
phase 4.)

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
  4.8 ms per edit (reparse plus the whole query).
- Only the lines out of date are coloured again (2026-10-18): the colours
  are kept per line with a flag; an edit splices them as it splices the
  lines and marks the changed lines and those of
  `ts_tree_get_changed_ranges` (a comment opened above them...); the query
  and the injections run on the bytes of these lines
  (`ts_query_cursor_set_byte_range`). Typing in `glue_basic.cpp` (13101
  lines): 31 ms per edit before, 5.8 ms after, most of it the incremental
  parse.

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

Done in part (2026-10-17):

- Scanners stay in Scheme and serve both parsers:
  `tree_sitter_set_scanner` installs C trampolines (one create function
  per language, 32 slots; a language given its scanner again keeps its
  slot) which call the Scheme `create`/`scan`/
  `serialize`/`deserialize`; the public lexer functions of `ts-parse.scm`
  act on the current `TSLexer` when given something else than a Scheme
  lexer; the state goes into tree-sitter's buffer as the text of the value.
- `ts-scanner-cpp.scm` (raw strings). With the Scheme scanners, the C runtime
  passes the corpus of tree-sitter-cpp (124 tests, 16 ms in all) and of
  tree-sitter-python (117 tests, 90 ms).
- Python is highlighted (tables in 20 s in TeXmacs). C++ was registered
  but not highlighted (it is now, see below): its tables come out exact (11734 states, 570
  symbols, as `tree-sitter generate`) but cost about 10 minutes and 6 GB
  standalone, so about half an hour in TeXmacs: 100525 LR(1) states before
  merging (LR construction about 100 s and 5 GB), then about 10 million
  pairs compared in the first pass of the merging (`tree-sitter generate`:
  5 s, 0.5 GB). The later passes of the merging now only look at the
  groups which can still split (tables byte-identical).
- Open: how to make large grammars practical (C++ stages in TeXmacs,
  compact Scheme representations, shipping tables for large grammars, or
  the 3x slowdown of S7 module code).
- **Femtolisp** (2026-10-17, `--with-scheme=femtolisp`, Vue): the
  generator runs 3 to 4 times faster inside TeXmacs than under S7, and
  needs less memory, with byte-identical tables:

  | grammar | S7 in TeXmacs | femtolisp in TeXmacs |
  |---|---|---|
  | python | 24.8 s | 7.8 s |
  | c | 50.3 s | 12.1 s |
  | cpp | about 30 min, 6.5 GB (estimate) | 262 s, 3.7 GB |

  (standalone s7, outside TeXmacs: python 7.4 s, c 10.9 s, cpp 645 s and
  6.5 GB). C++ needed a heap above 1 GB: femtolisp patch 0023 makes the
  heap size 64 bits (the heap now stops at 8 GB). Two pitfalls of
  femtolisp met on the way, both fixed by renaming: `return` is a
  special form (the escape procedure of the Python scanner was called
  `return`, so `(return #t)` returned from the innermost function and the
  scan went on), and a global macro wins over a local function of the
  same name (a local `extract` of `ts-grammar.scm` was expanded as the
  macro `extract` of `kernel/library/iterator.scm`, which only the full
  test run loads). The femtolisp build of this base needed a no-op
  `tmscm_builder` (it did not compile); its suites: 48 of 49 pass (`treesit`
  included), `markdown` hangs and `office` fails one check ("xml limits",
  memory-error on 3000 nested elements), both also without patch 0023.
- Compact representation (2026-10-17, `TS_PROFILE=1` prints the stages).
  C++ on femtolisp in TeXmacs before: LR 57 s, token conflicts 5,
  coincident 12, merge 133, lex 37, language 6 (252 s, 3.6 GB). Items now
  carry their production, steps and an integer sort key, and state keys
  are integer vectors: LR 47 s (C standalone: 3.7 -> 2.5 s), tables
  identical, memory barely lower (3.5 GB). The cost of the LR stage was
  accessor chains (each `item-step` went through the grammar's hash
  table), not the size of the keys. The merge did not improve: its
  first pass walks the sorted entries of about 10 million pairs of
  states (C: 58% of the pairs have the same tokens, 10% the same
  actions, so no cheap equality shortcut), and on femtolisp a loop
  written in Scheme is slower than the built-in `equal?` it replaced
  (merge 163 s with it, reverted).
- **C++ port of the state stages** (2026-10-17,
  `Plugins/TreeSitter/ts_tables.cpp`, used through `progs/treesit/
  ts-native.scm` when the glue `tree-sitter-tables-start` exists): the LR(1)
  construction, the following tokens, the coincident tokens, the error
  state, the used symbols, the merging, the unit reductions, the unused
  states and the order. The token conflicts, the keywords and the lex
  tables stay in Scheme. The tables are byte-identical to those of the
  Scheme code (json, scheme, c, python, cpp; the treesit suite compares
  both paths, `ts-set-native-tables!` or `TS_SCHEME_TABLES=1` select the
  Scheme one). C++ grammar:

  | | S7 in TeXmacs | femtolisp in TeXmacs |
  |---|---|---|
  | before | about 30 min, 6.5 GB | 252 s, 3.6 GB |
  | LR (C++) | 3.2 s | 2.9 s |
  | merge etc. (C++) | 3.3 s | 2.9 s |
  | token conflicts | 5.5 s | 4.6 s |
  | lex tables (Scheme) | 89 s | 37 s |
  | language | 21 s | 5.7 s |
  | total | 141 s, 4.7 GB | 56 s, 1.4 GB |

  C: 9.9 s instead of 24.8 s (S7), python 7.6 s instead of 18.0 s. The lex
  tables are now most of the time.
- **Lex tables in C++ too** (same day): the NFA and the start states of the
  tokens go with the grammar; the grouping of the parse states by
  compatible token sets, the DFA of each group and of the keywords, the
  minimization and the order are in `ts_tables.cpp`; tables still
  byte-identical (json, scheme, c, python, cpp). C++ grammar: S7 52 s
  and 4.8 GB, femtolisp 19 s and 1.4 GB (C 7.4 s, python 4.8 s under S7).
  What remains in Scheme: reading and preparing the grammar and its NFA,
  the token conflicts (5 s), the runtime language (`tables->language`,
  21 s under S7, 6 s under femtolisp).
- **C++ highlighted** (same day): with the tables in about a minute,
  `cpp` joins the highlighted languages. Its captures (the query of
  tree-sitter-c followed by that of tree-sitter-cpp, as `; inherits: c`
  says) equal those of `tree-sitter query` in order: 127 on a sample with
  templates, lambdas and raw strings, 18510 on `ts_tables.cpp` (87 KB).
  The generation in TeXmacs under S7 (`TS_PROFILE=1`): LR 3.5 s, token
  conflicts 7.3 s, states and lex tables (C++) 4.0 s, language 32.7 s,
  68 s in all; the runtime language is now half of the time.
- **The runtime language written by C++** (same day). Finer stages showed
  where the 68 s went: the fragile tokens (`mark-fragile-tokens!`, 18.7 s)
  and the writing of the `.tsdyn` text (`ts-export-language`, 17.6 s, 1.7
  million numbers), more than the language itself. With the native
  tables, the states now stay in C++ (`tree-sitter-tables-finish` with
  the flag 1), and `tree-sitter-tables-export` writes the whole text: the
  external lex states, the fragile tokens, the parse table and its action
  lists, the lex modes, the reserved words, the lex tables and the primary
  states. Scheme sends what only depends on the symbols (names, metadata,
  fields, aliases, the maps of the queries: `ts-language-symbols`, shared
  with `tables->language`). `ts-generate-tsdyn` (`ts-export.scm`) is the
  entry point; `treesit-generate-cache` uses it. The texts are
  byte-identical to those of `ts-export-language` for json, scheme, c,
  python and cpp (the treesit suite compares them).

  | grammar (S7 in TeXmacs) | before | now |
  |---|---|---|
  | c | 7.4 s | 3.9 s |
  | python | 4.8 s | 2.4 s |
  | cpp | 68 s, 4.8 GB | 12.5 s, 1.3 GB |

  C++ now: grammar 0.5 s, LR 3.2 s, token conflicts 5.8 s (Scheme),
  states and lex tables 2.4 s, language 0.5 s. The token conflicts are
  the largest stage left in Scheme.
- **Token conflicts in C++** (same day): `tree-sitter-tables-start` now
  also runs the NFAs of each pair of tokens (`make-token-conflict-map`)
  and sends the statuses back with the following tokens, for the choice of
  the keywords (still in Scheme). Tables byte-identical. Generation under
  S7 in TeXmacs: cpp 6.5 s and 1.2 GB (LR and token conflicts 3.2 s,
  states and lex tables 2.2 s, grammar 0.6 s, language 0.4 s), c 0.74 s,
  python 0.49 s, scheme 0.2 s. What remains in Scheme is reading the
  grammar, its NFA and the keywords.
- **Java and JavaScript** (same day). Java has no scanner: registered and
  highlighted as it is. `ts-scanner-javascript.scm` ports the scanner of
  tree-sitter-javascript (automatic semicolons, template strings, `?` of
  the conditional expressions, HTML comments, JSX text); the highlight
  query is highlights.scm, highlights-jsx.scm and highlights-params.scm in
  the order of its tree-sitter.json. The character classes of C
  (`iswspace`...) are in `ts-chars.scm`. Generation: about 0.8 s each.
  Through the C runtime in TeXmacs, the corpus of java passes (108 tests)
  and that of javascript too (116); the highlight captures equal those of
  `tree-sitter query` on samples and on the test/highlight files of both.
- **R** (same day): `ts-scanner-r.scm` ports the scanner of r-lib/
  tree-sitter-r (the newlines, which end an expression except inside
  parentheses and brackets, so the scanner keeps the stack of the open
  brackets, serialized as a string of one digit per bracket; the else
  after newlines in braces; the raw strings `r"-(...)-"`). Tables in 0.3 s;
  corpus 89 tests pass, highlight captures equal on a sample and on the 8
  files of test/highlight.
- **Julia** (same day): `ts-scanner-julia.scm` ports the scanner of
  tree-sitter-julia (nested block comments, the brackets and quotes right
  after a token, the content of strings and commands). Its grammar found
  two bugs of the generator, which the other grammars did not show:
  - Its identifier pattern uses an intersection of classes,
    `[...&&[^0-9#*]]`; the parser of patterns did not know the operators
    of classes of the regex crate (`&&`, `--`, `~~`), so the identifier
    could start with almost anything, it conflicted with every token, no
    keyword was found for 14 of them, and the merging kept 56999 states
    (tree-sitter: 8267). Fixed in `ts-nfa.scm`; `treesit-generator-version`
    is now 2, so that the cached tables are made again.
  - Then its highlight query did not compile (error 5, "structure"): the
    symbol part of the language did not follow render.rs. An alias is the
    *public* symbol of the first symbol which is its node; the aliases of
    their own come from the productions only, sorted by name; the alias map
    and the supertype map are ordered (and the supertype map deduplicated)
    by the C identifiers of the symbols, which `ts-language.scm` now makes
    as render.rs does (`sanitize-identifier`, numbers for equal names). The
    previous output differed from parser.c on json, c, python and cpp too
    (alias ids, children of supertypes), without visible effect there.
  Now the symbol part equals that of parser.c for json, c, python, cpp and
  julia (`compare.c`), and the parse tables are the same up to the numbers
  of the states: tree-sitter orders the states of the same size with an
  unstable sort, so the numbers of some states, and with them the lex
  states and the primary states, differ. Julia: corpus 63 tests pass,
  highlight captures equal; tables in 11 s and 1.5 GB under S7 (tree-sitter
  generate: 4 s, 0.7 GB), most of it in the merging and the lex tables.
- **Bash** (same day, after phase 5): `ts-scanner-bash.scm` ports the
  scanner of tree-sitter-bash (1217 lines of C: heredocs, the words of
  expansions, regular expressions and extended globs, variable names, file
  descriptors, concatenations). The C code is followed closely, quirks
  included: restoring a state never shrinks the stack of the heredocs (an
  empty state resets their fields, a state with fewer heredocs overwrites
  the first ones), a delimiter keeps the null character which ends it and
  is compared as by `strcmp`, characters are stored as the chars of C; its
  labels (`regex:`, `extglob_pattern:`, `expansion_word:`, `brace_start:`)
  are procedures which give their result or let the next one go on, as the
  `goto`s fall through. Tables in 1.5 s under S7; the corpus passes (100
  tests, S7 and femtolisp); the highlight captures equal those of
  `tree-sitter query` on the 8 scripts of its examples. The program language
  of TeXmacs is `shell` (`shell-code`, Shell sessions), highlighted with the
  grammar of bash; indentation rules and `#` comments in `ts-edit.scm`, and
  a `tags.scm` of ours for the functions. 254 KB of scripts parse in 277 ms
  (S7) and 293 ms (femtolisp): the scanner is asked at nearly every word,
  half of its time is interpretation and a quarter the collector.
- **Go, Rust, Lua and Lean** (same day): new program languages of TeXmacs,
  highlighted by tree-sitter only (they had no support): a format each
  (`go`, `rs`, `lua`, `lean`, in `plugins/code/progs/code-format.scm`), a
  `<lan>-lang.scm` with keywords for the tokenizer until the tables are
  ready (it also makes the format a program language), the tags `go-lang`,
  `rust`, `lua`, `lean` and `<lan>-code` (`env-program.ts`), the modes
  `in-prog-<lan>?`, the Insert menus (which now also list JavaScript and
  Julia), the code blocks of the Markdown and docx/odt converters, and the
  rules of indentation and comments of `ts-edit.scm` (in Go, `case` stays at
  the indentation of `switch`, as gofmt writes it). Grammars (MIT) with
  their highlight queries and tags; `tags.scm` of ours for Lean. Scanners:
  `ts-scanner-rust.scm` (string content, raw strings, floats, nested and
  doc comments; as in C, block comments look at the low byte of the
  characters), `ts-scanner-lua.scm` (long brackets of strings and comments;
  as in C an empty state changes nothing), `ts-scanner-lean.scm` (Unicode,
  dotted and «quoted» identifiers, nested comments, raw strings, norm bars,
  and the layout tokens of tactic and do blocks with a stack of columns).
  Go has no scanner. Corpora: go 67, rust 151, lua 42, lean 301 tests,
  all passing under S7 and femtolisp; highlight captures equal to those of
  `tree-sitter query` on the examples of the grammars (22000 captures for
  Go, 12000 for Rust) and on samples for Lua and Lean. Tables: go 0.7 s,
  rust 1.5 s, lua 0.5 s; Lean 74 s and 8.7 GB (381910 LR(1) states before
  merging; `tree-sitter generate`: 104 s and 5.6 GB, same 10243 states),
  so the suite tests Lean only when its tables are cached. Memory, the
  next day: the structures of the construction (item sets, lookahead sets,
  signatures of the states) are freed when the states are built; the index
  of the symbols of a state becomes a sorted vector once the state is
  complete (it was a hash table per state); an entry keeps its first
  action in place (no allocation, it nearly always has one); the memo of
  the token conflicts of the merging takes two bits per state and token.
  Lean: peak 4.7 GB (memory in use after the construction 3.3 GB, was
  6.9), 68 s; the tables of the 14 grammars are byte-identical. With
  `TS_PROFILE`, each stage also prints the peak resident memory and the
  memory in use.
- **Shipped tables for Lean** (same day). Generating all the tables at the
  first start would take 77 s (S7) or 83 s (femtolisp) and 4.8 GB, of which
  Lean 65-70 s; the 13 others take 12 s together. The tables of Lean are
  therefore shipped: `misc/tree-sitter/lean/<name of the cache file>.xz`,
  214 KB (10 MB uncompressed; gzip would give 1.8 MB). When the cache lacks
  a language, `treesit-cached?` unpacks its shipped tables into the cache
  (glue `tree-sitter-unpack`, through the xz decoder of XZ Embedded, 0BSD,
  vendored in `Plugins/TreeSitter/xz` and compiled as `ts_xz.c`): Lean is
  ready in 140 ms instead of a minute. The name holds the hash of the
  grammar and of the generator, so tables which no longer match are not
  used and the language is generated as before;
  `misc/tree-sitter/ship-tables.sh LANGUAGE...` makes them again, and the
  treesit suite fails while they are missing. All the 14 grammars would
  take 1.0 MB with xz (31.7 MB uncompressed).
- Tried and dropped (same day): merging the states of the same core while
  they are built (Pager's weak compatibility). Lean then took 2 s and 290 MB,
  but the tables were nearly LALR (one state per core, 2399 for Lean against
  10243) and the corpora failed (bash 66 of 100, javascript 42, julia 30,
  lean 13...): a merged state accepts the tokens of all its parts, which
  changes the lexing and what the external scanners see, and takes the
  resolution of the conflicts of either part. tree-sitter refuses such
  merges after the construction, when the states are complete; doing it
  during the construction would need an IELR(1)-like analysis.
- **One list of the program languages** (2026-10-18):
  `kernel/texmacs/tm-program-languages.scm` lists each program language with
  its menu label, its inline and block tags, its grammar, its line comment
  and its names in other formats. The modes `in-<lan>%` and
  `in-prog-<lan>%`, the groups of the code tags, the Insert menus, the
  languages of the Markdown and docx/odt converters
  (`convert/tools/code-languages.scm`), the highlighting and editing of
  `progs/treesit`, and the languages which C++ wraps (`tree_sitter_wraps`,
  now filled at the start through the glue `tree-sitter-wrap-language`)
  are made from it. A new language still needs, besides its entry: the
  macros of its tags (`env-program.ts`), its format and `<lan>-lang.scm`
  (`plugins/code`), its grammar, and the port of its scanner if it has one.
- An S7 pitfall met on the way: in a `do` loop whose body calls a builtin
  through another name (`ahash-set!` is `hash-table-set!`), s7 changes the
  counter in place, so the counter stored as a value (or in a list) ends
  as its last value; called by its own name, `hash-table-set!` is safe. It
  concerns all the code of TeXmacs under S7 which stores a `do` counter
  with `ahash-set!`; in `ts-language.scm` the loop is now a named let.
- Tools (`src/misc/tree-sitter`): `corpus.py LANGUAGE CORPUS...` runs the
  corpus tests of a grammar in TeXmacs as `tree-sitter test` compares
  them; `highlights.py LANGUAGE GRAMMAR-DIR FILE...` compares the captures
  with those of `tree-sitter query`; `compare.c` compares a `.tsdyn` of
  the cache with the `parser.c` of `tree-sitter generate`.

- **Scanner speed** (same day). Time to parse a file of about 100 KB
  through the C runtime in TeXmacs, with three changes made in turn:

  | language | before | public names | trampolines | `ts-scan.scm` |
  |---|---|---|---|---|
  | c (no scanner) | 8 ms | 5 ms | 5 ms | 5 ms |
  | cpp | 20 ms | 15 ms | 15 ms | 15 ms |
  | python | 365 ms | 118 ms | 64 ms | 42 ms |
  | javascript (247 KB) | 597 ms | 181 ms | 86 ms | 71 ms |
  | r | 698 ms | 256 ms | 101 ms | 95 ms |
  | julia | 223 ms | 88 ms | 40 ms | 35 ms |

  - **Public names.** Two thirds of the time went into looking up the
    public lexer functions of `ts-parse.scm`. `define-public` bound a name
    in its module and in the rootlet; s7 caches the let of the latest local
    binding of a symbol, so a lookup from a module older than the defining
    one scanned that module and the user module (about 250 slots) before
    reaching the rootlet. `define-public` now removes the module's binding
    (s7 patch 0006, `docs/s7/02-boot-and-modules.md`), which is likely a
    good part of the 3x of module code (section 6), for all of TeXmacs.
  - **Trampolines** (`tree_sitter_glue.cpp`). Each scan built the vector of
    valid tokens with `(list->vector ...)` evaluated from a string, and
    each deserialize read the state back with the Scheme reader. Now the
    vectors are made once for each row of the table of the external
    states, the texts of the serialized states are mapped back to their
    values, and the procedures are looked up once.
  - **`ts-scan.scm`**: `record-scanner` makes a scanner whose state is a
    vector of fields, `stateless-scanner` one without state, and the loops
    over characters run in C on the lexer of the C runtime
    (`scan-advance-until!`, `scan-advance-while!`, `scan-skip-spaces!`;
    `scan-word!`, `scan-accept!`); the Python scanner leaves with
    `call-with-exit` instead of `call/cc`. The five scanners use it; the
    corpora pass, and the trees of the benchmark files are unchanged. (A
    macro naming the tokens was tried and dropped: under femtolisp, the
    definitions made by a macro are not private to their module.)
- **TypeScript, HTML, CSS, TOML, OCaml and LaTeX** (2026-10-18): one entry
  each in `tm-program-languages.scm` with its tags (`typescript-lang`,
  `typescript-code`...). TypeScript (`.tsx` `.mts` `.cts`: `.ts` are style
  files), CSS, TOML and OCaml (`.ml` `.mli`) also have a format and a
  `-lang.scm`; HTML and LaTeX have none, as they stay formats of documents
  (a `.html` or `.tex` file is imported, not opened as code). The highlight
  query of TypeScript is its own followed by that of JavaScript, as its
  `tree-sitter.json` lists them; that of LaTeX is ours (the grammar has
  none, nor a committed `parser.c`: the comparison with `tree-sitter query`
  needs `tree-sitter generate` in a copy). Scanners: toml (multi-line
  strings, line ends), css (the descendant operator, the colon of pseudo
  classes), latex (raw environments), html (the stack of open tags,
  implicit end tags, raw text of script and style), typescript (as
  javascript, with function signatures and the `?` of optional arguments),
  ocaml (nested comments with their strings and characters, quoted strings
  `{id|...|id}`, line directives). `corpus.py` skips the tests marked
  `:language(other)`, as `tree-sitter test` does. Corpora: toml 17, css 40,
  latex 104, html 20, typescript 111, ocaml 92, all passing under S7 and
  femtolisp; highlight captures equal. Tables: 0.5 to 2 s, OCaml 6.7 s.
- **YAML** (same day): `ts-scanner-yaml.scm` ports the scanner of
  tree-sitter-yaml (1417 lines of C, 113 external tokens: directives, block
  and flow structures with a stack of indentations, quoted strings and
  their escapes, block scalars, plain scalars, tags, anchors, aliases,
  comments) with its core schema: the state machine of `schema.core.c`,
  which tells null, bool, int, float and string scalars apart, is a table
  made from the C source. The C code is followed closely, quirks included
  (`]` and `}` at the indentation give the symbol of the other context).
  Corpus 131 tests pass (S7 and femtolisp), captures equal; tables 0.4 s;
  180 KB parse in 339 ms (the scanner reads nearly every character).
  Adding it made 16 languages with a scanner, and the 16 slots of the
  trampolines ran out (one more load took a slot): there are now 32, and a
  language keeps its slot. For indentation YAML needed a new option of the
  rules: a blank line after a named token (`key: value`) keeps the
  indentation of the node around it, after `key:` or `[1,` it is indented.
- **Injections** (same day): `misc/tree-sitter/<lan>/injections.scm`, the
  queries of the grammars (html: script and style in JavaScript and CSS;
  javascript and typescript: tagged templates such as `` css`...` ``; rust:
  the arguments of macros; lua: `ffi.cdef` in C; cpp: raw strings by their
  delimiter). The highlighter of a language (`treesit-install-highlighter`)
  also compiles its injections; when a region is highlighted
  (`syntax_region_highlight`), each `@injection.content` of the query is
  parsed with the grammar of its language (from `#set!
  injection.language` or the text of `@injection.language`, mapped to a
  program language through the names of `tm-program-languages.scm`) and
  painted with its highlight query over the colours of the region. The
  injected parts are parsed again whenever the region changes (not
  incrementally), and injections do not nest. An injected language whose
  tables are not ready is asked for at its first use; when its highlighter
  arrives, `tree-sitter-reset-highlights` makes the highlighted languages
  take a new highlight code, so that the lines are coloured again.
- **YAML faster** (same day): 339 ms for 180 KB before, 266 ms after. A
  profile (`(*s7* 'profile-info)`) showed the lookahead asked 14 times per
  token through the public `lexer-lookahead`; a call from another module
  costs about as much as the glue call behind it, so the scanner keeps the
  lookahead in a variable of its module, read once after each advance (the
  same cache inside `ts-parse.scm`, for all the scanners, gained nothing:
  the public call stays). Runs of plain ASCII go through the loops of
  `ts-scan.scm` in C, the schema leaves early once frozen, and the eight
  closures made at each scan became procedures of the module.
- **Shipped tables for all the grammars** (same day): 1.44 MB for the 21
  grammars of then (1.62 MB with Haskell), generated by `ship-tables.sh`
  (all the grammars of the registry by default; run again it gives the
  same files). The suite checks that each grammar has tables for the
  present grammar and generator, and that those of json and toml are what
  the generator makes.
- **Haskell** (same day): `ts-scanner-haskell.scm` ports the scanner of
  tree-sitter-haskell (3471 lines of C, half of them comments: layouts as a
  stack of contexts, newline lookahead and processing over several runs,
  the variants of the operators, qualified names, sections, comments,
  pragmas, CPP, quasiquotes, the lookahead of constraints). Its classes of
  Unicode characters, bitmaps in `unicode.h`, are range tables
  (`ts-haskell-chars.scm`, made by evaluating the C functions on every
  code point) searched by `char-in-ranges?`, a new piece of `ts-scan.scm`.
  The tables have the 12862 states of `parser.c` (1.4 s); the corpus passes
  (725 tests, S7 and femtolisp) and the highlight captures equal those of
  `tree-sitter query`; 70 KB parse in 150 ms (`tree-sitter parse`: 11 ms).
  Program language `haskell` (`.hs`), a `tags.scm` of ours, and the
  outline no longer lists a definition of the same kind and name just
  after itself (the equations of a function).

As planned initially:

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

`progs/treesit/ts-edit.scm` holds the clients. It only uses the glue, so
that the editing modules load it without the generator. A client works
where tree-sitter highlights the code: `tree-sitter-program-language`
gives the language of a program language once its highlighter is
installed, and the old behaviour stays elsewhere.

- **Selection and brackets** (2026-10-17). `program-select-enlarge`, and
  `kbd-select-enlarge` in program mode, select the smallest syntax node
  around the selection (`syntax-region-node-range`, new: the nodes around a
  range). `select-brackets-after-movement` matches a bracket token with the
  bracket at the other end of its parent node, so brackets in strings and
  comments have no match.
- **Inputs of sessions** (same day). In `kbd-enter` of sessions and of
  program fields, Return opens a new line when the input can only be
  completed by more lines: an error or a missing node reaches the end of the
  text (an open bracket), or the text ends with an empty node (Python
  accepts `def f(x):` at the end of a text with an empty block; the new
  `syntax-region-last-nodes` gives the nodes along the last children).
  Other errors go to the interpreter, which reports them.
- **Indentation** (same day). `program-compute-indentation` asks the tree
  first (the generic definition, and those of Python and Java; Scheme
  keeps its own). A line gets the indentation of the line where the
  innermost open node of an indenting type starts, plus a tab stop; none
  when it starts with the token which closes that node (`}`, `)`, `end`)
  or with a branch (`else`, `elif`, `except`, ...). The types are listed
  per language in `ts-edit.scm` (c, cpp, python, java, javascript, json,
  r, julia). For a blank line, a node which ends before it without error
  is closed; in Python only brackets close, its blocks end where the
  indentation goes back; after an unfinished construct, the ERROR node of
  the error recovery counts as open. C/C++, JavaScript, JSON, R and Julia
  had no indentation; Python's no longer takes a `#` in a string for a
  comment. Found on the way: the indentation of `scheme-edit.scm` was
  declared with `(:require in-prog-scheme?)`, a condition which is always
  true (the procedure, not its value), so it applied to every program
  language without its own; now `(:mode in-prog-scheme?)`.
- **Outline and comments** (same day). The definitions are the captures
  `@definition.<kind>` of `misc/tree-sitter/<lan>/tags.scm` with their
  `@name`, as in tree-sitter-tags (new glue `syntax-region-captures`, the
  captures of a query on a region): the files of the grammars for c, cpp,
  python, java, javascript and r, ours for julia and scheme (the TeXmacs
  definers `tm-define`, `tm-menu`...). In program code the Focus menu has
  Definitions (go to one), Go to definition (of the name at the cursor,
  in the same code) and Toggle comment (line comments of the lines of the
  selection). The `@definition.constant` pattern of tree-sitter-python
  finds nothing (`expression_statement` is a supertype), as with
  `tree-sitter query`.
- **Indentation by the queries of Neovim** (2026-10-18): `indents.scm` of
  nvim-treesitter (Apache-2.0) for 17 grammars, the files they inherit put
  in front; `syntax_region_indent` ports `get_indent` of
  `lua/nvim-treesitter/indent.lua` (begin, end, branch, dedent, align with
  its delimiters, auto, ignore, zero), and the query engine knows their
  predicates `#kind-eq?`, `#has-parent?` and `#lua-match?`. The rules of
  `ts-edit.scm` remain for Lean (no query) and YAML (that of Neovim only
  indents after a block). Python lost a pattern which its grammar of now
  makes impossible (`tree-sitter query` refuses it too).
- **Structured moves** (same day): in program code the structured moves of
  TeXmacs (structured:cmd with the arrows, home, page up and down) go from
  node to node, to the node around and into it, and from definition to
  definition (`syntax-region-node-move`); the Focus menu has Previous and
  Next definition and Select definition. Folding is not done: it would
  move lines into a fold tag inside a code block, which the regions, the
  editing of programs and the export of code do not expect.
- A pitfall of the tests: the suites run in one TeXmacs, and a key typed
  in program code loads the keyboard of programs and its modules, which
  changed the highlighting of Fortran and the entering of mathematics in
  the suites run afterwards; the tests call the functions instead.

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
