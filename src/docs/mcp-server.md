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
  and walked completely, also what the LaTeX and Markdown importers make of it:
  - *Code*: `extern`, `script`, `script-eval`, `script-input`, `script-output`,
    `eval`, `eval-args`, `quasi`, `quasiquote`, `unquote`, `unquote*`, `action`,
    `input`, `output`, `session`, `shell-input`, `plugin-input` and `compound`
    (which calls a macro by its name).
  - *The definitions and the style of the whole document*: `assign`, `drd-props`,
    `style-with`, `style-only`, `var-style-only`, `with-package`, `use-package`,
    `use-module`; and `raw-data` (binary data).
  - *Links* (`hlink`, `href`, `slink`, `smart-ref`, `locus`) whose target has a
    scheme other than `http`, `https`, `mailto` (`script:`, `file:`,
    `javascript:`...). Relative targets and `#labels` are accepted.
  - *Files*: `image`, `postscript`, `postscript*`, `include`, `var-include`,
    `tmdoc-include`, `include-section`, `sound`, `video`, `attachment`, `ocr` (the
    first argument) and `bibliography` (the files): only a relative name, with no
    `..`, no colon, not starting with `/`, `\` or `~`, which stays inside the roots
    (`TEXMACS_MCP_ROOTS`) or, with no roots, inside the folder of the document (a
    symbolic link that leads out is resolved and refused); `http` and `https`
    addresses for `image` and `postscript`. The name must be a string.
  - *LaTeX sources* that use `\input`, `\include`, `\includeonly`,
    `\InputIfFileExists`, `\verbatiminput`, `\lstinputlisting`, `\openin`,
    `\openout`, `\immediate`, `\write`, `\read`, `\readline`, `\catcode`,
    `\scantokens`, `\csname`, `\directlua`, `\newwrite`, `\bibliography`,
    `\addbibresource`, `\externaldocument` are refused before the conversion: the
    importer would read the files itself. (`\includegraphics` is converted to an
    `image`, and then checked as above.)
  - *Trees that cannot be*: a tag known to the DRD with a number of arguments it
    cannot have (`(with)`, `(frac "a")`, an empty `(table)`...) is refused. A
    `with` with no argument crashed the typesetter; the typesetter now shows a
    `with` with a wrong number of arguments as an inactive tree (`make_bridge`), so a
    patch of another process with such a tree no longer crashes the person's
    TeXmacs either. (Only `with` is guarded in C++: the other primitives with a
    wrong arity go through the failure detection of the typesetter.)
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
| `content` | each case of the refused content (code, files, links, LaTeX and Markdown sources, malformed trees) and the accepted ones, with the document unchanged and the server alive; `create_document` with dangling links and refused content |
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
