# `texmacs --mcp`: TeXmacs as an MCP server (read only)

Step 2 of the [MCP server design](https://github.com/mgubi/texmacs/wiki/MCP-server-design)
(sections 4.1, 4.3, 5, 6, 8). An agent (Claude Code, Claude Desktop, any MCP
client) starts `texmacs --mcp` as its MCP server over stdio and reads TeXmacs
documents through it:

    claude mcp add texmacs -- texmacs --mcp

This step only reads. Editing (`replace`, `insert`, `delete`, `edit_text`),
agent modes and proposals, `check`, `render`, `export`, notifications and
`get_changes` are the steps 3 to 6 of the design. Nothing is evaluated, no
plugin session is started, and nothing but the documents is reachable (design
5.6): the tools below are the whole interface.

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
  `TEXMACS_MCP_DEBUG=1` also logs every message.
- **Reading is polled on the main thread**, by a delayed command every 10 ms,
  like the shared files (`mcp_stdin_read` is a non-blocking `poll` + `read`).
  The output is written by `mcp_stdout_write` (blocking, until the client
  has taken it). A tool call runs to its end before the next line is read.
- **The C++ part is the four functions of `mcp_stdio.cpp`** (glued as
  `mcp-stdio-active?`, `mcp-stdin-read`, `mcp-stdin-eof?`, `mcp-stdout-write`).
  The protocol and the tools are Scheme: `TeXmacs/progs/agents/mcp-server.scm`
  (`mcp-server-start`, `mcp-handle-line`), so Qt, Vue and Cocoa, Guile and s7,
  share them. The JSON reader and writer are those of C++ (`json->typed-tree`,
  `typed-tree->json`, which the LSP client uses too); the reader is lenient, so a
  line whose brackets or quotes do not match is refused beforehand as a parse
  error.
- **End of input** (or of the pipe) is a clean exit: the documents are closed
  (a shared one is left) and TeXmacs quits, status 0.
- No Windows yet: `mcp_stdio.cpp` does nothing there (the shared files do not
  work there either).

## The protocol

JSON-RPC 2.0, one message per line (a CR before the newline is accepted; a blank
line is skipped; a line over 32 MB is dropped with an error).

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
| `list_documents` | | `documents`: this session's (`document`, `path`, `file`, `title`, `style`, `modified`, `shared`, `blocks`, `hash`, `opened_by`); `open_elsewhere`: files another process hosts and this one has not open (`path`, `person` = that host is interactive, `pid`) |
| `open_document` | `path` (absolute) | the same facts for the document (the same id when it is open already) |
| `get_outline` | `document` | `title`, `hash`, `blocks`, `sections`: `handle`, `tag`, `level` (part 0, chapter 1, section 2... subparagraph 6), `title`, `labels`, `index` (position among the top-level blocks), `chars` (size to the next section), `hash` |
| `read` | `document`, `node?` (a handle), `format?` (`text` default, `texmacs`, `stree`, `latex`, `markdown`), `max_size?` (characters, default 12000, 200 to 400000), `start?` | `content`, `hash`, `tag`, `truncated`, and for a document or a `document` node `start`, `end`, `blocks`, `next_start` (continue with `start`) and `children` (handle, index, tag, hash, preview, at most 200); for another node the first 50 children |
| `find` | `document`, `query`, `limit?` (50) | `count`, `truncated`, `matches`: handle, tag, hash, `section` (the title of the section it is in), `context` |
| `get_references` | `document` | `labels` (name, handle of the block that holds it, section), `references` (name, tag, handle, `defined`), `citations` (key, handle), `broken_references`, `duplicate_labels` |

Notes:

- **`read` cuts between blocks** (the children of the body): it takes as many as
  seem to fit in `max_size` (a size estimate of the tree), converts them, and
  shrinks the window while the result is longer. A single block longer than the
  limit is cut as text, at a character of UTF-8. A `texmacs` or `stree` read of
  the whole document in pieces, one after another with `start`, concatenates
  to the whole.
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
- Not done: cursors/presence of the agent, resources and notifications, limits
  of folders and a read-only flag (design 6), `create_document`, `save_document`,
  `close_document` (they come with editing), Windows, and a typesetter-based
  `get_references`.
