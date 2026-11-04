# `texmacs --mcp`: TeXmacs as an MCP server

Steps 2 (reading) and 3 (editing, direct mode) of the [MCP server design](https://github.com/mgubi/texmacs/wiki/MCP-server-design)
(sections 4.1, 4.3, 5, 6, 8). An agent (Claude Code, Claude Desktop, any MCP
client) starts `texmacs --mcp` as its MCP server over stdio and reads TeXmacs
documents through it, and edits them.

## Registering the server

**Claude Code**:

    claude mcp add texmacs -- texmacs --mcp

(`texmacs` is the launcher of TeXmacs Upsilon, `TeXmacs/bin/texmacs` of an
installation; give its full path if it is not in the `PATH`. To limit the folders
or to make the server read-only, put the variables in the command:
`claude mcp add texmacs -e TEXMACS_MCP_ROOTS=$HOME/papers -- texmacs --mcp`.)

**Claude desktop app**: add the server to `claude_desktop_config.json`
(Settings > Developer > Edit Config; on macOS it is
`~/Library/Application Support/Claude/claude_desktop_config.json`) and restart the
app:

    {
      "mcpServers": {
        "texmacs": {
          "command": "/Applications/TeXmacs Upsilon.app/Contents/MacOS/texmacs",
          "args": ["--mcp"],
          "env": { "TEXMACS_MCP_ROOTS": "/Users/you/papers" }
        }
      }
    }

The path is that of the binary of the application (on macOS the launcher script is
`Contents/Resources/bin/texmacs` or the executable in `Contents/MacOS`: use the one
which exists in your installation; it must be the launcher or `texmacs.bin`
with `--mcp`). Nothing is typed on the terminal: the app starts the process and talks
to it over its standard input and output. `env` is optional.

To work on a file together with a person who has it open in TeXmacs, turn on the
preference `share-open-files` in both (TeXmacs and the home the server uses).

Not yet: agent modes and proposals, `check`, `render`, `export` and resource
notifications (steps 4 to 6 of the design). The interface has no tool
that evaluates code, starts a plugin session, or touches preferences or the
network (design 5.6). The real guarantee about files is weaker than "nothing
but documents": `open_document` (and any tool given a path) opens **any file the
user can read**, as TeXmacs would, through its importers (a `.tex`, `.md`,
or a plain text file such as `/etc/passwd` opens as a document). To limit it,
start the server with **`TEXMACS_MCP_ROOTS=dir1:dir2`**: only files whose
canonical path (symbolic links resolved; a path with a `..` component is refused,
and the file loaded is the canonical path that was checked) is inside one of the folders
can be opened, and `list_documents` shows no other open file; the default is no
limit. `TEXMACS_MCP_READONLY=1` makes the server refuse (and not list) the editing
tools, `save_document`, `close_document` and `create_document`. Paths containing a colon cannot be opened (a colon separates
the alternatives of a TeXmacs url) and give a clean error.

## The process

`--mcp` (also `-mcp`) makes a headless TeXmacs (`-headless`, as for `-X`: the
event loop keeps running, there is no window) which serves the protocol on its
standard input and output, and ends when the input ends.

- **stdout is the protocol and nothing else.** Before anything is printed,
  C++ (`src/System/Link/mcp_stdio.cpp`, called from `unix_entrypoint.cpp` and
  `immediate_options` in `texmacs.cpp`) keeps copies of descriptors 0 and 1 for
  the protocol, points descriptor 0 to `/dev/null` and descriptors 1 and 2 to the
  log file `$TEXMACS_HOME_PATH/system/mcp.log` (appended). Everything that
  TeXmacs, its libraries and the programs it starts print goes there,
  including the Scheme `display`. A program started by TeXmacs inherits
  neither the protocol input nor its output. (Before the home is known the
  descriptors go to `/dev/null`: the first lines of start-up are lost.)
  `TEXMACS_MCP_DEBUG=1` also logs every message. A log over 4 MB is renamed to
  `mcp.log.1` (mode 0600) when the server starts and during the run (checked every 64 ticks; `TEXMACS_MCP_LOG_CAP` sets the cap, for tests). Several servers may share one home: a server renames the file only if its descriptor is still the file `mcp.log`, else it just reopens it; if a rename fails it stops trying.
- **The launcher** `TeXmacs/bin/texmacs` (from `misc/scripts/texmacs.in`) runs
  the binary with standard input from `/dev/null`; for `--mcp` it keeps the input.
  `claude mcp add texmacs -- texmacs --mcp` thus works with the launcher.
- **Reading is polled on the main thread**, by a delayed command every 10 ms
  while messages come, every 50 ms after 2 s of silence (idle cost under 1% of
  a CPU), like the shared files (`mcp_stdin_read` is a non-blocking `poll` +
  `read`). The pieces of a line are kept in a list and only the new piece is
  searched for the newline.
  The output is written by `mcp_stdout_write` (blocking, until the client
  has taken it). A tool call runs to its end before the next line is read.
- **The C++ part is the four functions of `mcp_stdio.cpp`** (glued as
  `mcp-stdio-active?`, `mcp-stdin-read`, `mcp-stdin-eof?`, `mcp-stdout-write`).
  The protocol and the tools are Scheme: `TeXmacs/progs/agents/mcp-server.scm`
  (`mcp-server-start`, `mcp-handle-line`), so Qt, Vue and Cocoa, Guile and s7,
  share them. The JSON reader and writer are those of C++ (`json->typed-tree`,
  `typed-tree->json`, which the LSP client uses too); the reader is lenient, so a
  line is first checked by a strict validator in C++ (`mcp_json_valid`: valid UTF-8 (no overlong forms or surrogates); the literals `true`, `false`, `null`; numbers in the JSON grammar of any length whose value is finite; strings without raw control characters (below U+0020) and with only the escapes `\" \\ \/ \b \f \n \r \t \uXXXX` (a lone surrogate in `\u` is accepted); a comma between and no comma after the members; the value an object or an array nested at most 512 deep; nothing after it but blanks) and refused as a parse error if it fails.
- **A client that is gone**: when a write to the output fails the server ends
  (SIGPIPE is ignored in this mode).
- **End of input** (or of the pipe) is a clean exit: the documents are closed
  (a shared one is left) and TeXmacs quits, status 0.
- The browser build (`__EMSCRIPTEN__`) compiles `local_link.cpp` and `mcp_stdio.cpp` as stubs that do nothing (as on Windows).
- No Windows yet: `mcp_stdio.cpp` does nothing there (the shared files do not
  work there either).

## The protocol

JSON-RPC 2.0, one message per line (a CR before the newline is accepted; a blank
line is skipped; a line over 32 MB is dropped up to its newline, with one error
-32600). A line must be one strictly valid JSON object or array, nested at most 512 deep
(the C++ reader crashes on hundreds of thousands of levels and accepts garbage):
else -32700. The 32 MB cap is on the length of the whole line. An id that is neither
a string nor a number, or a missing or non-string method, is -32600.

- `initialize`: answers the client's `protocolVersion` if it is one of
  2025-11-25, 2025-06-18, 2025-03-26, 2024-11-05, else 2025-06-18; capabilities
  `{tools: {listChanged: false}}`; `serverInfo` (`texmacs`, the version); short
  `instructions` for the model.
- `notifications/*` (a message with no id) get no answer; `ping` gives `{}`.
- `tools/list`, `tools/call`. A tool that fails gives a result with
  `isError: true` and the reason in its text (the model can read it); an unknown
  tool, `-32602`; an unknown method, `-32601`; a line that is not JSON, `-32700`
  with a null id.
- A tool answers with one text block, which is the JSON of its result (all texts
  in UTF-8).
- `resources/list` and `prompts/list` answer with empty lists (resources come
  with the notifications of step 6).

## Documents, handles, hashes

- A document of the session has an id `d1`, `d2`... (`open_document`). Every tool
  takes a `document`: an id, or an absolute path (opened if it is not yet). The
  agent never sees paths of the tree.
- Reading a node gives it a **handle** `n17`: a tree pointer of this process
  (`tree->tree-pointer`), which follows its node through edits (text typed in a
  paragraph, a paragraph inserted before it...), with the **hash** of its
  content (`tree-hash`, 12 hex digits of the 64-bit hash, over the whole
  subtree). A handle is good for as long as its node is the same tree: a tree
  pointer moves to the nearest tree when its node is removed, so each handle
  also keeps the tree it was made for, and a handle whose node was deleted or
  replaced as a whole answers "does not exist any more" (the agent reads again).
  At most 40000 handles are kept (the older half goes).
- A file is opened as TeXmacs opens one: **joined** when another process of the
  user hosts it (the person's TeXmacs, or a headless one) and
  `share-open-files` is on in both, **alone** otherwise (the default of the
  preference is off). Before each tool call the links of the shared files are
  served (`shared-poll`), so that what the host has sent is in. The `shared`
  field tells: `{role: host|participant, state, participants}` or `null`.
  When the MCP process is the only one, it hosts the file, and when the person
  opens it later hosting moves to them (shared-files.md).

## The tools

| Tool | Arguments | Result |
|---|---|---|
| `list_documents` | | `documents`: this session's (`document`, `path`, `file`, `title`, `style`, `modified`, `shared`, `blocks`, `hash`, `opened_by`); `open_elsewhere`: files another process hosts and this one has not open (`path`, `person` = that host is interactive, `pid`); a host is taken to be alive when its process exists (`kill(pid, 0)`; a stopped one is alive) and its port does not refuse a connection (a non-blocking connect, at most 100 ms; a port that does not answer counts as a busy host; the port of a reused process id or of a zombie is closed). No lock is taken, and each verdict is kept for 5 s so that listing again and again does not fill the queue of a stopped host |
| `open_document` | `path` (absolute) | the same facts for the document (the same id when it is open already) |
| `get_outline` | `document`, `limit?` (300), `start?` | `title`, `hash`, `blocks`, `sections`: `handle`, `tag`, `level` (part 0, chapter 1, section 2... subparagraph 6), `title`, `labels`, `index` (position among the top-level blocks), `chars` (size to the next section), `hash`; also `total`, `next_start` (continue with `start`). Sections are found also in a concatenation (`<section|A><label|x>` and the LaTeX importer make `(concat (section ..) (label ..))`) |
| `read` | `document`, `node?` (a handle), `format?` (`text` default, `texmacs`, `stree`, `latex`, `markdown`), `max_size?` (characters, default 12000, 200 to 400000), `start?` | `content`, `hash`, `tag`, `truncated`, and for a document or a `document` node `start`, `end`, `blocks`, `next_start` (continue with `start`) and `children` (handle, index, tag, hash, preview, at most 200); for another node the first 50 children |
| `find` | `document`, `query`, `limit?` (50) | `count`, `truncated`, `matches`: handle, tag, hash, `section` (the title of the section it is in), `context` |
| `get_references` | `document`, `limit?` (200), `start?` | `labels` (name, handle of the block that holds it, section), `references` (name, tag, handle, `defined`), `citations` (key, handle), `broken_references`, `duplicate_labels`; `labels_total`, `references_total`, `citations_total`, `truncated` (`limit` and `start` apply to the three lists) |

The editing tools (not listed when `TEXMACS_MCP_READONLY` is set):

| Tool | Arguments | Result |
|---|---|---|
| `replace` | `document`, `node`, `content`, `format?` (`texmacs` default, `stree`, `latex`, `markdown`), `expect?` | `status` `applied` with `hash` (the new hash of the document), `changed` (the new nodes: handle, tag, hash, preview) and `undo_steps`; or `conflict`; or `refused` |
| `insert` | `document`, `anchor?`, `where` (`before`, `after`, `first`, `last`), `content`, `format?`, `expect?` | the same; no anchor means the document itself (`first`, `last`) |
| `delete` | `document`, `node`, `expect?` | the same, `changed` empty |
| `edit_text` | `document`, `node`, `old`, `new`, `expect?` | the same |
| `undo_agent` | `document`, `steps?` (1) | the same, with `undone` |
| `save_document` | `document` | `saved` (or `pending`), `path`, `shared` |
| `close_document` | `document`, `discard?` | `closed`; refused with unsaved changes unless `discard` |
| `create_document` | `path` (absolute, `.tm`, new, in an existing folder), `style?` (`generic`), `content?`, `format?` | the facts of the new document, opened |

- **Content** is parsed first and refused, with nothing changed, if it does not
  parse: `texmacs` (the syntax of a snippet, with `<`, `>` balanced; non-ASCII
  characters are accepted as UTF-8), `stree` (read as data, never evaluated; a
  string, or a tag with arguments), `latex` (braces balanced), `markdown`. A
  `(document ...)` or several paragraphs are several blocks.
- **What content is refused** (the whole call is refused, nothing is changed, and the
  reason names the tag). The tree of the content is parsed (or converted) first
  and walked completely, also what the LaTeX and Markdown importers make of it.
  The filter has **two layers**, because a list of names of macros keeps leaking (a
  wrapper macro, an alias made with `with` and `value`, a macro that calls the macro
  named by its argument, a leaf of an existing node, a definition changed under its
  uses, the `with` around the place of an insertion...):
  1. *The content as written* is checked by name, cheaply, before anything is
     done: the classification below, the allowlist of tags (defined by the style or
     the document), the characters of the strings that become names in an export, the
     values of `with`, `cwith` and `twith` (typed: a size or a colour is not a
     compound), aliases (`<value|href>`, `<with|em|...>` where `em` is a tag), the
     macros that call a macro named by an argument (`map` and the like, with a
     dangerous name), the arity.
  2. *The document after the edit, compared with the document before it.* The edit
     is made tentatively inside the transaction of the editor (`mark-start`) and, if
     it introduced something the filter refuses that was not there before,
     cancelled (`mark-cancel`) with the reason ("the edit is refused: it would
     introduce what the content filter refuses (...)"). The unit is the top-level
     block that holds the edit (and the blocks the edit adds), checked in two forms:
     as written (the rules of layer 1 on the block) and as it expands (the block is
     evaluated by `texmacs-exec` in the environment of the document, after the
     `assign`s of the blocks before it, down to the primitives, and the rules of the
     primitives are applied: the destination of every `link` and `hlink`, the file of
     every `image`, `include`, `include*`, `pattern`, `postscript`, `video`, `sound`
     (also the full names that the evaluator gives), `script` and `action` (only
     `mouse-fold` and `mouse-unfold`, what the folding tags make), `specific`, `latex`,
     what the evaluator leaves of `extern`, `hybrid`, `authorize`, `observer`, and
     any `error`). Only the *new* violations count (by their text, with their
     number): what the person already has, in their preamble or their text, stays and
     does not make the edits refused. So a leaf of an existing node (the address of a
     link, a label, the file of an image, the value of a `with`), the macros whose
     definition is changed (when an `assign` of the document changes, the whole
     document is compared before and after), and the environment around the
     insertion (a `with` of the person, a macro of theirs that depends on it) are
     covered by construction.
  *Keys of the violations.* A violation is compared by its reason and the whole
  content of the node that has it (hashed, no truncation), so a change inside a
  node that already had a violation (a person's `<specific|html|...>`, their
  `<extern|...>`, a long `javascript:` address) is a new violation; the definitions of
  the document (`assign`, `macro`, `set`...) are the exception, being tied to no
  content. An edit inside, or of, code or raw text for another program (`extern`,
  `script`, `hybrid`, `action`, `compound`, `latex`, a `specific` for another program,
  the inputs and outputs of sessions...) is refused whatever the violations, and an
  edit in a block that does not evaluate within the budget is refused.
  *Budgets.* The evaluation has a budget of 1.5 million steps for a block; a
  comparison of the whole document (when a definition changed) evaluates the blocks
  in batches of about 20000 nodes whose results are dropped at once, within 12 million
  steps and 3 million nodes of results in all, and only the blocks that use the changed
  definitions (the names of the `assign`s of the edited block, and of the
  definitions that use them again and again, found as tags or as strings; all the blocks
  if the style defines one of the names). An expansion that exceeds a budget is a
  violation, the edit is refused, and the server stays alive. The budget is a counter in
  the evaluator (`exec_budget_start`, `exec_budget_end` in `env_exec.cpp`, off unless
  asked). The nesting of macros is limited always, which protects the person's
  TeXmacs: the evaluator stops at 1200 levels (`exec_compound`), the typesetter at 150
  levels for the macros of a block (`bridge_compound.cpp`) and 300 for those inside a
  paragraph (`concat_macro.cpp`), where it shows an error box (a macro that calls itself
  overflowed the stack: typesetting and the export to PDF crashed). A `set` of the
  person is taken as an `assign` of the variable when the context is built (the evaluator
  does not do it). Any failure of the check (an error, a budget) refuses the edit.
  An edit that is refused is cancelled before anything is sent to the host: the changes
  collected for the host are dropped with it (`shared-collect-mark`,
  `shared-collect-restore!`). The document itself is not changed by the check: the content
  written is inserted, not the expansion. With 20000 paragraphs an edit takes 30 to 60 ms,
  and the edit of a definition used by 100 blocks 0.2 to 0.4 s.

  Every one of the 370 primitives of the DRD (`src/Data/Drd/drd_std.cpp`) is
  classified below by its real name; the lists in `mcp-server.scm` are this table,
  and the suite `filter` checks that every name in them is a tag of the DRD.

| Name | Class | Why / which argument |
|---|---|---|
| `extern` | refused: code or definition | can run a command |
| `hybrid` | refused: code or definition | calls a function by its name |
| `authorize` | refused: code or definition | authorizes the evaluation of code |
| `observer` | refused: code or definition | calls a function |
| `action` | refused: code or definition | runs an action when clicked |
| `script` | refused: code or definition | can run code |
| `eval` | refused: code or definition | evaluates code |
| `eval-args` | refused: code or definition | evaluates code |
| `quasi` | refused: code or definition | evaluates code |
| `quasiquote` | refused: code or definition | evaluates code |
| `unquote` | refused: code or definition | evaluates code |
| `unquote*` | refused: code or definition | evaluates code |
| `compound` | refused: code or definition | calls a macro by its name |
| `apply` | refused: code or definition | calls a macro by its name |
| `expand` | refused: code or definition | calls a macro by its name |
| `expand*` | refused: code or definition | calls a macro by its name |
| `hide-expand` | refused: code or definition | calls a macro by its name |
| `begin` | refused: code or definition | calls a macro by its name |
| `end` | refused: code or definition | calls a macro by its name |
| `map-args` | refused: code or definition | evaluates the arguments of a macro |
| `assign` | refused: code or definition | changes the definitions of the whole document |
| `set` | refused: code or definition | changes the environment of the whole document |
| `reset` | refused: code or definition | changes the environment of the whole document |
| `provide` | refused: code or definition | changes the definitions of the whole document |
| `macro` | refused: code or definition | defines a macro (a macro can call itself without end) |
| `xmacro` | refused: code or definition | defines a macro |
| `func` | refused: code or definition | defines a function |
| `drd-props` | refused: code or definition | changes the properties of the tags |
| `with-package` | refused: code or definition | loads a package |
| `use-package` | refused: code or definition | loads a package |
| `use-module` | refused: code or definition | loads a module |
| `new-theme` | refused: code or definition | changes the themes |
| `copy-theme` | refused: code or definition | changes the themes |
| `apply-theme` | refused: code or definition | changes the themes |
| `select-theme` | refused: code or definition | changes the themes |
| `style-with` | refused: code or definition | changes the style |
| `style-with*` | refused: code or definition | changes the style |
| `style-only` | refused: code or definition | changes the style |
| `style-only*` | refused: code or definition | changes the style |
| `filter-style` | refused: code or definition | changes the style |
| `raw-data` | refused: code or definition | is binary data |
| `find-file` | refused: code or definition | looks for files |
| `find-file-upwards` | refused: code or definition | looks for files |
| `uninit` | refused: code or definition | is an internal marker |
| `unknown` | refused: code or definition | is an internal marker |
| `while` | refused: loop or resource | is a loop without a limit (it can use all the memory) |
| `for-each` | refused: loop or resource | is a loop (it can use all the memory) |
| `range` | refused: loop or resource | makes a list of any size |
| `image` | file argument | the first argument |
| `postscript` | file argument | the first argument |
| `include` | file argument | the first argument |
| `include*` | file argument | the first argument |
| `video` | file argument | the first argument |
| `sound` | file argument | the first argument |
| `pattern` | file argument | the first argument |
| `hlink` | link destination | the 2nd argument |
| `link` | link destination | the last argument |
| `url` | link destination | the argument |
| `specific` | raw text | refused unless its format is `texmacs`, `screen` or `printer` |
| `latex` | raw text | its text is scanned for the LaTeX commands that read files, run code or hide a command name |

  *Safe*: the 310 other primitives (layout, mathematics, tables, graphics, effects,
  animations, labels and references, arithmetic and comparisons, `if`, `case`,
  `with`, `value`, `quote`, `copy`, `delay`, `hold`, `release`, ...):
  `string`, `error`, `document`, `para`, `surround`, `concat`, `rigid`, `hgroup`, `hidden`, `freeze`, `unfreeze`, `hspace`, `vspace*`, `vspace`, `space`, `htab`, `move`, `shift`, `resize`, `clipped`, `repeat`, `repeat*`, `float`, `datoms`, `dlines`, `dpages`, `dbox`, `line-note`, `page-note`, `if-page-break`, `with-limits`, `line-break`, `new-line`, `next-line`, `no-break`, `no-indent`, `yes-indent`, `no-indent*`, `yes-indent*`, `page-break*`, `page-break`, `no-page-break*`, `no-page-break`, `no-break-here*`, `no-break-here`, `no-break-start`, `no-break-end`, `new-page*`, `new-page`, `new-dpage*`, `new-dpage`, `around`, `around*`, `big-around`, `left`, `mid`, `right`, `big`, `long-arrow`, `lprime`, `rprime`, `below`, `above`, `lsub`, `lsup`, `rsub`, `rsup`, `frac`, `sqrt`, `wide`, `wide*`, `neg`, `tree`, `syntax`, `tformat`, `twith`, `cwith`, `tmarker`, `table`, `row`, `cell`, `subtable`, `with`, `provides`, `value`, `quote-value`, `or-value`, `quote-arg`, `arg`, `get-label`, `get-arity`, `mark`, `mark*`, `expand-as`, `quote`, `copy`, `if`, `if*`, `case`, `dynamic-case`, `or`, `xor`, `and`, `not`, `plus`, `minus`, `times`, `over`, `div`, `mod`, `minimum`, `maximum`, `math-sqrt`, `exp`, `log`, `pow`, `cos`, `sin`, `tan`, `merge`, `length`, `number`, `date`, `translate`, `change-case`, `is-tuple`, `look-up`, `occurs-inside`, `equal`, `unequal`, `less`, `lesseq`, `greater`, `greatereq`, `blend`, `rgb-color`, `rgb-access`, `cm-length`, `mm-length`, `in-length`, `pt-length`, `bp-length`, `dd-length`, `pc-length`, `cc-length`, `fs-length`, `fbs-length`, `em-length`, `ln-length`, `sep-length`, `yfrac-length`, `ex-length`, `fn-length`, `fns-length`, `bls-length`, `fnbot-length`, `fntop-length`, `spc-length`, `xspc-length`, `par-length`, `paw-length`, `pag-length`, `gw-length`, `gh-length`, `gu-length`, `tmpt-length`, `px-length`, `guipx-length`, `lcorner-length`, `bcorner-length`, `rcorner-length`, `tcorner-length`, `ms-length`, `s-length`, `msec-length`, `sec-length`, `min-length`, `hr-length`, `active`, `active*`, `inactive`, `inactive*`, `rewrite-inactive`, `inline-tag`, `open-tag`, `middle-tag`, `close-tag`, `symbol`, `locus`, `id`, `hard-id`, `find-accessible`, `relay`, `set-binding`, `get-binding`, `has-binding`, `hidden-binding`, `label`, `reference`, `pageref`, `get-attachment`, `write`, `toc-notify`, `auto-id`, `cache-ref`, `tuple`, `attr`, `tmlen`, `collection`, `associate`, `backup`, `gradient`, `flag`, `hyphenate-as`, `anim-static`, `anim-dynamic`, `morph`, `anim-time`, `anim-portion`, `anim-compose`, `anim-repeat`, `anim-constant`, `anim-accelerate`, `anim-translate`, `anim-progressive`, `graphics`, `superpose`, `gr-group`, `gr-transform`, `gr-effect`, `text-at`, `math-at`, `document-at`, `point`, `line`, `cline`, `arc`, `carc`, `spline`, `spline*`, `cspline`, `bezier`, `cbezier`, `smooth`, `csmooth`, `penscript`, `calligraphy`, `fill`, `box-info`, `frame-direct`, `frame-inverse`, `is-equal`, `is-intersection`, `on-curve`, `on-text-border`, `on-grid`, `transform-3d`, `object-3d`, `triangle-3d`, `light-3d`, `light-diffuse`, `light-specular`, `eff-move`, `eff-magnify`, `eff-bubble`, `eff-crop`, `eff-turbulence`, `eff-fractal-noise`, `eff-hatch`, `eff-dots`, `eff-gaussian`, `eff-oval`, `eff-rectangular`, `eff-motion`, `eff-blur`, `eff-outline`, `eff-thicken`, `eff-erode`, `eff-degrade`, `eff-distort`, `eff-gnaw`, `eff-superpose`, `eff-add`, `eff-sub`, `eff-mul`, `eff-min`, `eff-max`, `eff-mix`, `eff-normalize`, `eff-monochrome`, `eff-color-matrix`, `eff-gradient`, `eff-make-transparent`, `eff-make-opaque`, `eff-recolor`, `eff-skin`, `canvas`, `ornament`, `art-box`, `format`, `line-sep`, `split`, `delay`, `hold`, `release`, `old-matrix`, `old-table`, `old-mosaic`, `old-mosaic-item`, `env`.

  Other rules:
  - *Which tags are accepted at all*: a primitive (classified safe above), or a macro that
    the document defines: its style and packages as this process has loaded them
    (`style-has?` in the buffer of the document), or an `assign` in the document
    itself (its preamble). Every other tag is refused with "unknown tag X (not
    defined by the style of the document)": an unknown tag would be written as a
    raw macro in an export to LaTeX (`\verbatiminput`, `\write18`...), and the
    LaTeX importer makes such tags from `\includepdf`, `\inputminted`, `\import`,
    `\usepackage{../x}`... Even a macro of the style is refused if its body runs
    commands, reads files or acts on a click. The list was made by scanning every
    `<assign|name|<macro|...>` of `TeXmacs/styles` and `TeXmacs/packages` for `action`,
    `find-file`, `include*`, `script`, `observer`, `hybrid`, `authorize`, and
    `extern` with a function that reads files or runs programs (doxygen, tmdoc,
    module documentation, remote, spell, listing, popups...), plus the macros named by
    the audit: `refused-style-macros` in `mcp-server.scm`:
    `block-comment`, `button`, `calc-check`, `carbon-comment`, `chunk-link`, `dir-entry-icon`, `dir-header-cell`, `doc-module-synopsis`, `doxygen-get`, `doxygen-load`, `doxygen-ref`, `edit-macro`, `folded-comment`, `icon`, `inline-comment`, `link-to-module-doc`, `link-to-module-source`, `live-io`, `live-io*`, `mc-popup`, `mirror`, `mirror-comment`, `radio-box`, `radio-button`, `remote-big-icon`, `remote-email`, `remote-menu`, `remote-name`, `remote-picture`, `screenshot`, `shared`, `spell-error`, `spell-propositions`, `tmdoc-file`, `tmdoc-image`, `tmdoc-translation`, `tmdoc-web`, `tmweb`, `tmweb-link`, `toggle-box`, `toggle-button`, `widget-cmd`, `widget-value`.
    A macro whose `action` runs a fixed command (the folding tags `fold`, `unfold`,
    `folded`, `summarized`, `detailed`, `fold-button`, ... call `mouse-fold` or `mouse-unfold`
    with their argument as data) is not in the list; one whose action is an argument or a
    computed command (`button`, `toggle-box`, ...) is. The macros of the style are also checked for their number of arguments
    when the DRD knows it (`(theorem)` is refused); the macros that the document
    assigns itself are not, before they are typeset.
  - *Values of the environment*: `with`, `cwith` and `twith` take only values of letters,
    digits, blanks and `. _ - + * / % # , : =` (no quote, bracket, backslash, brace or
    control character: they are written as attributes and commands in the exports); a
    colour (`color`, `*-color`) is `#hex` (3 to 8 digits) or a name of letters, digits,
    blanks and `-`; `font-size`, `font-base-size`, `magnification`, `dpi` are numbers.
    A `with` that sets the name of a tag or of a macro, and a `value`, `quote-value` or
    `or-value` of such a name, are refused (no alias).
  - *Names that go to an export*: the string arguments of `label`, `reference`, `pageref`,
    `eqref`, `cite`, `nocite`, `cite-detail`, `bibitem`, `smart-ref`, `ref`, of the file
    tags, and the values of a `with` (not its text), and the destination of a link, are
    written as they are in an export to LaTeX or HTML. A backslash, a brace, a quote (`"` or `'`), `<`, `>` or a
    control character is refused in all of them; an address also has no blank and no HTML entity (`&#106;`, `&colon;`) (`<label|a}\input{/etc/passwd}\x{>`
    would hijack the `.tex`); in labels and files also `%`, `#`, `$`, `&`, `^`, `~`; in
    addresses `^^` and a `%` that is not followed by two hexadecimal
    digits (a `#` fragment, `%20`, `&` and `~` are fine). The text of a `<latex|>` tag is
    also scanned for the commands that take a file or an address (`\includegraphics`,
    `\bibliography`, `\IfFileExists`, `\href`, `\url`...). A `<latex|>` with `^^` is refused.
    *This is a bug of the LaTeX exporter for any document* (`tmtex-label`,
    `tmtex-reference`, `tmtex-cite`, `tmtex-href` in `progs/convert/latex/tmtex.scm`
    write the strings as they are): a label with a brace breaks or hijacks the `.tex`.
    Not fixed on this branch.
  - *Macros* (tags that are not primitives): `session`, `input`, `output`, `script-input`,
    `script-output`, `script-eval` and every tag ending in `-input` or `-output`
    (`scheme-input`, `shell-input`...) are refused as code; `tmdoc-include`,
    `include-section`, `attachment`, `ocr` (first argument) and `bibliography` (all
    its string arguments) are file arguments; `href` and `slink` (first argument)
    are link destinations. A tag defined by nothing (see above) is refused.
  - *File arguments*: a plain string that is a relative name, with no `..` component, no
    colon or scheme, not starting with `/`, `\` or `~`, no `%2e`, `%2f` or `%5c`,
    that stays (symbolic links resolved, also for a file that does not exist yet) inside the
    roots (`TEXMACS_MCP_ROOTS`) or, with no roots, inside the folder of the document;
    `http` and `https` addresses for `image`, `postscript` and `pattern`. A file
    written as anything but a plain string (`concat`, `value`, `merge`...) is refused.
  - *Link destinations*: a plain string; the scheme (letters, digits, `+ - .` before a
    colon, blanks at the start skipped, case ignored) must be `http`, `https` or
    `mailto`; no scheme means a relative address, a `#label` or `other.tm#label`, which is
    accepted (a link is followed by the person, not read by the server); `//host` is
    refused. The other arguments of a link (its text) are not looked at, so
    `<hlink|See: this|http://x.org>` is fine. `smart-ref`, `reference`, `pageref` and
    `label` take labels, not addresses.
  - *LaTeX sources*: `\input`, `\include`, `\includeonly`, `\InputIfFileExists`,
    `\verbatiminput`, `\lstinputlisting`, `\openin`, `\openout`, `\immediate`, `\write`,
    `\read`, `\readline`, `\catcode`, `\scantokens`, `\csname`, `\directlua`, `\luaexec`,
    `\pdfshellescape`, `\newwrite`, `\externaldocument`, and the ways to build or hide a
    command name (`\string`, `\expandafter`, `\noexpand`, `\def`, `\edef`, `\gdef`, `\xdef`,
    `\futurelet`, `\makeatletter`, `\makeatother`) are refused before the conversion: the
    importer would read the files itself. This also refuses them inside `verbatim`
    (a false positive that stays). Also refused: `\inputminted`, `\import`, `\subimport`, `\includepdf`, `\ShellEscape`,
    `\write18`, `\pgfplotstableread`, `\CatchFileDef`, `\DTLloaddb`, `\addplot`,
    `\loadglsentries`, `\lstset`. `\documentclass`, `\usepackage`, `\RequirePackage`,
    `\bibliographystyle`, `\bibliography` and `\addbibresource` are accepted when their
    options and names are plain (letters, digits, `- _ .` and `=`, not starting with a
    dot, no `..`, no `/`, no `\`, no braces): `\usepackage{amsmath,amssymb}`,
    `\documentclass[12pt]{article}`, `\bibliography{refs}`; a path is refused. A text with `\begin{document}`, `\title`, `\author`, `\date` or `\maketitle` is
    imported as a whole document (`latex-document`: `doc-data` with the title, the authors
    and the date; the style it chooses is ignored; `\thanks` becomes a footnote,
    `\autoref` a reference, a bare `\caption` its text; `\hline` outside a table goes);
    the class, the packages and the resources are dropped (the style of the
    document says them), and `\bibliographystyle{S}` with `\bibliography{F}` become
    `<bibliography|bib|tm-S|F|<document|>>` at the end, the form of the standard style
    (4 arguments; the importer gives only 1), whose files (a list separated by commas)
    are checked as files. Packages that run programs or read files (`minted`,
    `catchfile`, `shellesc`, `pythontex`...), options with `shell` or `convert`, and
    `filecontents` are refused; a text that has only such commands is refused as having nothing to insert. `\includegraphics` becomes an `image`.
  - *Trees that cannot be*: a primitive with a number of arguments it cannot have
    (`(with)`, `(frac "a")`, an empty `(table)`...) is refused, and so is a macro of the
    style with a number of arguments its definition does not have. A `with` with no
    argument crashed the typesetter; the typesetter now shows a `with` with a wrong
    number of arguments as an inactive tree (`make_bridge`), so a patch of another
    process with such a tree no longer crashes the person's TeXmacs either.
  This is a filter on what the agent writes, not a sandbox: a document that the
  person opened already is not scanned, and the importers run in this process.
- **A node** is a block of the document (then `replace` takes one or several
  blocks), or a part of a paragraph (then one item; the paragraph is rebuilt
  with the strings merged). The whole document cannot be replaced or deleted;
  a node that is an argument of its tag cannot be deleted by itself.
- **`edit_text`** is the fine-grained tool: the old text must occur exactly once
  in the visible text of the node, inside one stretch of text without markup
  (the error says how many times it occurs, or that it was not found); it
  changes only those characters, so that the markup around it and a person's
  typing elsewhere in the same paragraph are kept and merged by the host.
- **`expect`** is the `hash` of the node as read (of the anchor for `insert`). If the
  node changed since, nothing is done and the result is `{status: "conflict",
  current, current_hash, handle}` (the current content in the format asked, at most
  4000 characters, and a fresh handle). This holds even if the host could have
  merged the two changes (design 7, question 1): leave `expect` out to accept
  merging. The check is made on this process's copy of the document, which
  follows the host within some 20 to 35 ms: a change of the person in that window is
  merged or refused by the host, as for any change without `expect`.
- **One call, one undo step, all or nothing.** The check and the parsing come
  first; the change is made between `mark-start` and `mark-end` by the editor of the
  buffer (so it carries the author of the editor of this process, and in a shared
  file it goes to the host as patches of this process; the cursor moves are
  stripped before they leave), and cancelled with `mark-cancel` if something fails
  in the middle.
- **Shared files.** The process marks itself as an agent when it joins (its
  participant id starts with `agent.`, `shared-agent-id?`). Its patches go to the
  host as those of any participant: the host applies them with an author of their
  own (so that the person's undo skips them when they commute, tested: the
  person types twice, the agent changes the start of the same paragraph, the person
  undoes twice), rebases them when it can (`patch-can-pull?`) and refuses them
  otherwise; the host's own changes are never refused. The tool waits (at most
  10 s) for the host's answer: `applied`; `refused` (with a reason; the agent's copy
  is the host's again, and the older steps of the agent are forgotten, as the
  rollback made them unsafe to undo); or `pending` after 10 s (the host is busy: the
  change is kept and sent when it answers). For an agent no file is written in the
  `recovered` folder when its patch is lost: the refusal is its report. Propose
  mode and the menu of the person are step 4: direct mode only.
- **`undo_agent`** does not use the undo of the editor, which undoes the
  changes of the person that do not commute with the step. Each step keeps what it
  made (tree pointers and hashes) and the old content; a step is undone only if
  what it made is as it left it (the later steps are undone first; if one cannot be,
  the whole undo is cancelled and the reason is given). Limit: after the agent deletes
  a block and undoes the deletion, the block is a new copy, and an older step that
  touched that block can no longer be undone.
- **Counters.** The refusals, the resynchronizations and the documents replaced are counted for each shared file (`shared-file-stat`), so that a refusal in another document during the wait of an edit is not mistaken for its own.
- **Notes.** When the document changed since the last call about it (by the
  person, or a resynchronization), the answer of the tool has a `note`: how many blocks
  changed from which index, in which sections, and the new hash, since many clients
  do not show the model notifications. The note is made by comparing the hashes of the
  top-level blocks with those seen at the last call.
- `save_document` saves in the host when the file is shared (it waits for
  it); a file created by `create_document` is written with its style, then opened
  as any file.
- With `TEXMACS_MCP_READONLY` the editing tools are not listed and are refused;
  `create_document` and the edits respect `TEXMACS_MCP_ROOTS`.

Notes:

- **`read` cuts between blocks** (the children of the body): it takes as many as
  seem to fit in `max_size` (a size estimate of the tree), converts them, and
  shrinks the window while the result is longer. A single block longer than the
  limit is cut as text, at a character of UTF-8. A `texmacs` or `stree` read of
  the whole document in pieces, one after another with `start`, concatenates
  to the whole.
- **Arguments** of the wrong type (a node that is not a string, `max_size` that
  is not an integer, `arguments` that is not an object) are JSON-RPC errors
  -32602; an empty `tag:`, `label:` or `section:` is a tool error.
- **Texts.** Inside, strings are Cork; the client gets UTF-8 with the newlines and
  tabs as they are (`cork->utf8` would map them to other characters). The
  converters to Markdown and verbatim text already give UTF-8, LaTeX gives ASCII.
- **Formats.** `texmacs` is the syntax of a `.tm` snippet (exact) in ASCII: a
  Cork character above 127 is written `\<#e9\>`. `stree` is the Scheme form with
  the strings in UTF-8. `latex` and `markdown` use the converters of the build
  (`latex-snippet`, `markdown-snippet`): in a build without one the tool fails
  with "the conversion to ... failed". `text` is the visible text, made here:
  paragraphs on lines, mathematics through the converter to verbatim text (`x∈ℝ`),
  `(ref name)` for references, `[key]` for citations, `[image]` for images, no
  labels, only the last argument of `with`, tables with `|` between cells.
- **`find`** searches the visible text, case-insensitively and with one form for
  each character (`é` as a byte or as `<#e9>`), and returns the innermost
  paragraphs (children of a `document`, also inside theorems and lists); other
  queries: `tag:theorem` (all nodes with that tag, wherever they are),
  `label:name` (the block that holds the label), `section:title` (the sections
  whose title contains the text).
- **`get_references`** scans the tree (labels, `reference`, `pageref`, `eqref`,
  `cite`, `nocite`, `cite-detail`); it does not typeset, so labels made by
  macros (numbered environments) and broken citations (against the
  bibliography) are not seen. A later step can use the typesetter's tables.

## Tests

`tests/mcp/check.sh [scenario]`: `mcp_client.py` (python3), a small MCP client,
starts `texmacs.bin --mcp` on a pipe and checks the answers; each process is
capped at 2 GB and 120 s (`TM_TEST_RSS_KB`, `TM_TEST_TIMEOUT`), offscreen, with a
home and a shared folder (`TEXMACS_SHARED_DIR`) of its own (`TM_TEST_HOME` for
the work folder).

| Scenario | What |
|---|---|
| `protocol` | versions, ping, tool list, unknown method and tool, notifications, parse error, blank lines, string ids and CRLF, errors as results, exit 0 at the end of the input, only protocol lines on stdout, the log file |
| `tools` | all tools on a generated paper: outline, every format, cutting and continuing, handles, find in all its forms, references, errors |
| `shared` | a second headless TeXmacs hosts the file (`host-live.scm`, using `tests/shared/lib.scm`); the MCP server lists it as open elsewhere, joins it, reads the host's live edit through the handle it had, then a deletion; the handle of the deleted block is an error |
| `mcp-host` | the MCP server hosts the file; another TeXmacs joins (`host-join.scm`) and edits; the MCP server reads the edit |
| `unshared` | the preference off (the default): not shared, nothing listed |
| `launcher` | the same through `TeXmacs/bin/texmacs` |
| `undo` | the undo of the editor in a headless process, single process (`undo-probe.scm`): the own steps, the step of another author skipped when it commutes, the undo back to the saved state (it crashed: `QApplication::beep` without an application) |
| `filter` | every name of the lists of refused content is a tag of the DRD |
| `content` | each case of the refused content (code, loops, files, links, LaTeX and Markdown sources, malformed trees) and the accepted ones, 126 checks, with the document unchanged and the server alive; `create_document` with dangling links and refused content |
| `edit`, `readonly`, `edit-shared` | the editing tools: each tool, conflicts, undo_agent, all-or-nothing, save/close/create, read-only and roots; with a second headless TeXmacs hosting and editing (merge in one paragraph, a refusal reported, the person's undo skipping the agent's changes) |
| `robust` | non-ASCII and newlines in every format and in the file name, a star in a name, colons refused, `TEXMACS_MCP_ROOTS` (outside file, `/etc/passwd`, `..`, a symbolic link), sections in concatenations, bad arguments and messages, 200000 nested arrays, a 34 MB line |
| `hostdeath` | a server which has not opened the file lists it, the host is killed with `kill -9`, the file leaves the list; then the entry is given the pid of a live foreign process and a closed port: still not listed |
| `nonascii` | a file with a non-ASCII name, hosted elsewhere, is listed as it is, opens, joins and reads |
| `stalled` | a host stopped with SIGSTOP stays listed and 40 listings do not block the server |
| `logcap` | the log is rotated during the run, also with two servers on one home; the older log is private |
| `outputs` | the outline and the references cut and continued; the server ends when the client is gone |

## Decisions and open points

- **The protocol is in Scheme, the C++ is the four stdio functions**, as asked.
  The JSON helpers are local to the module (small `obj`, `arr`, `jref`), not those
  of `lsp-base.scm`, because the LSP branch may be left out of a build.
- **Descriptors, not streams**: moving descriptors 1 and 2 to the log, rather
  than redirecting `cout`, catches everything printed by C, the libraries and the
  Scheme. The cost: messages printed before the home is known are lost.
- **Handles of replaced nodes are lost** (see above): conservative, matches the
  rule of 4.3 that an edit names the hash it expects.
- **The agent's TeXmacs uses the person's home** by default
  (`~/.TeXmacs_Upsilon`), so its preferences (`share-open-files`) apply to both; a
  client can set `TEXMACS_HOME_PATH` in the environment of the server.
- Not done: cursors/presence of the agent, resources and notifications, propose
  mode and the menu (step 4), `check`, `render`, `export` (step 5), `get_changes`
  (step 6), rebasing across a split or join of a paragraph far from the edit
  (the host refuses these; planned), Windows, and a typesetter-based
  `get_references`. Also not investigated: a report that `go-to-path` called from a
  `delayed` command did not move the cursor in the window of a shared buffer.
- The editor's undo of the person skips the agent's changes when they commute.
  Remote patches were applied by a participant with the author of the host; they
  now carry the author of their sender (`applied` has the id), and a participant
  re-applies its own pending changes with the author of its own editor (they were
  made foreign by the transformation). With two parties only the first was
  already right; a third party shows the difference.
- On the report that the person's second Cmd-Z removed the agent's last insertion: the archiver (`archiver_rep::undo`) undoes the steps of the editor's author, and it undoes the steps of other authors that do not commute with them (it first undoes the foreign step above); the steps that commute are swapped out of the way when they are added. Two cases are tested and keep the agent's change (the agent inserts a block; the agent changes the start of the paragraph the person types in, the person undoes twice). A change that does not commute with the person's typing (the same place, or a deleted node) is undone with it by the editor's undo: that is the behaviour of the archiver, not of the patches' authors. Patches are received with the author of their sender since this work. The rollback of a refused change and the re-application of pending changes use the author of the process's own editor (they are its own steps).
- `beep` in a headless Qt build crashed (an undo that restores the saved
  state called `QApplication::beep` without an application): it is skipped when headless.
