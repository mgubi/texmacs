# Plan: Lean 4 through the language server

Status: planning report, 2026-10-19. Reference tree: `upsilon_texmacs` at
`~/t/lab/fix/plan-ref` (read only). All `file:line` references are relative
to that checkout and were checked there. Item in `src/TODO.md:21`: "support
for formalization of mathematics via Lean/LSP".

## 1. Summary

Most of the work is already done. Lean is already a program language of
Upsilon. It has a format (`.lean`), the tags `lean` and `lean-code`, a
tree-sitter grammar with shipped tables, a Scheme port of its external
scanner, and the server command `lake serve`, all in the language table.
The generic LSP client already gives diagnostics, hover, completion,
definitions, references, rename, code actions, symbols, folds, semantic
colours and inlay hints to any server. It can also join the code blocks of a
document into one virtual file per language.

Four things are missing for a usable Lean workflow:

1. **Plumbing**: the server must start in the Lake project root, and that
   root must be found. Lean must also be found when TeXmacs is launched from
   the Finder.
2. **The goal view** (the "infoview"): the proof state at the cursor, plus
   the file-processing progress.
3. **Unicode abbreviation input** (`\to`, `\alpha`, `\N`).
4. **Polish**: multi-line messages, server startup cost, and the defaults
   for code blocks.

Recommended order: plumbing, then plain-text goals, then abbreviations, then
interactive (RPC) goals. Typeset-math goals come last and are a research
item. Rough total for phases 1-4: 4-6 weeks of focused work.

## 2. Goals and user stories

**Story A: a mathematician's document.** A `.tm` paper has prose and
`lean-code` blocks: `import Mathlib` in the first block, definitions and
theorems in later blocks. The author wants:

- red underlines on errors and yellow underlines on `sorry`;
- the goal state when the cursor is inside a `by` block;
- `\forall`, `\R` and `\le` typed as `∀`, `ℝ` and `≤`;
- hover on a name for its type and docstring;
- go-to-definition into Mathlib;
- a progress indicator while Mathlib loads.

The blocks behave like one Lean file, in document order, as in a notebook.

**Story B: a `.lean` source file.** Someone opens `Foo/Bar.lean` from a Lake
project in TeXmacs. They expect the same experience as in VS Code: syntax
colours at once, server colours later, diagnostics, goals in a side panel,
abbreviations, and the outline. Saving must write back exactly the same
bytes.

**Story C (later): literate formalization.** The author exports the Lean
blocks of a document to a `.lean` file to check it with `lake build` or
in CI. Optionally, they show selected goals typeset in the paper itself.

## 3. What exists today (verified)

| Piece | Where |
|---|---|
| Language entry: `servers "lake serve"`, tags, grammar `lean`, comments `--` and `/- -/`, names `lean` and `lean4` | `src/TeXmacs/progs/kernel/texmacs/tm-program-languages.scm:83-86` |
| The format `lean` | `src/plugins/code/progs/code-format.scm:51` |
| Fallback tokenizer keywords | `src/plugins/code/progs/lean-lang.scm:20-60` |
| Macros `lean` (prog mode, `font-family rm`) and `lean-code` | `src/TeXmacs/packages/environment/env-program.ts:198,288-292` |
| Mode `in-prog-lean?`, generated from the table | `tm-program-languages.scm:187-195`, `kernel/texmacs/tm-modes.scm:263` |
| Tree-sitter grammar (Julian/tree-sitter-lean@916c44f, MIT); our own `tags.scm`; tables shipped as `.tsdyn.xz` (214 KB, ready in 140 ms instead of 70 s to generate) | `src/TeXmacs/misc/tree-sitter/lean/SOURCE`, `src/docs/tree-sitter.md:831-877` |
| External scanner port (Unicode identifiers, «» quoting, nested comments, layout tokens of tactic and `do` blocks) | `src/TeXmacs/progs/treesit/ts-scanner-lean.scm`, `ts-registry.scm:31,49` |
| Indentation rules for Lean (no Neovim `indents.scm` exists for it) | `src/TeXmacs/progs/treesit/ts-edit.scm:821-828` |
| Corpus: 301 tests passing; treesit suite checks indentation, outline and parse | `tree-sitter.md:848`, `progs/check/treesit-test.scm:802,829,1291-1300` |
| Markdown and docx `lean` fences map to `lean-code` | `progs/check/markdown-test.scm:895-901` |
| Lean listed in the user manual's server table | `doc/main/editing/man-language-servers.en.tm:30` |

**The LSP mechanism** (as described in `src/docs/code-features.md`):

- Servers are taken from the language table (`lsp-client.scm:55-61`). The
  first command found in `PATH` is used (`lsp-client.scm:71-81`).
- There is one client per (language, project root) (`lsp-client.scm:568-591`).
  The root is the nearest directory containing a marker from `root-markers`
  (`lsp-documents.scm:385-418`).
- The client capabilities are fixed in `client-capabilities`
  (`lsp-client.scm:234-292`). They declare `utf-16` positions, semantic
  tokens, inlay hints, code actions and `workDoneProgress`.
- Positions in UTF-16 units go through the core (`code-utf16-column`,
  `lsp-base.scm:113-123`). This matters for Mathlib, which uses
  astral-plane letters such as `𝓝` and `𝔽` (two units each).
- Extensions can register their own handlers with
  `lsp-notification-set!` and `lsp-server-request-set!`
  (`lsp-client.scm:412-421`). Requests can be cancelled with
  `lsp-cancel` (`lsp-client.scm:190`).
- Buffers are synced after `:idle 300` (`lsp-edit.scm:68-74`). The cursor
  hook is `notify-cursor-moved` (`lsp-edit.scm:388-403`); it may not use mode
  predicates. The focus-bar status shows the progress string
  (`lsp-edit.scm:306-325`, `lsp-client.scm:506-538`).
- Hover is markdown converted to a balloon (`lsp-edit.scm:636-683`). Lean's
  markdown uses ```` ```lean ```` fences, which become `lean-code`.
- Code blocks: with `lsp:code blocks` on (it is **off** by default,
  `lsp-blocks.scm:56-60`) and `lsp:join code blocks` (on by default), all the
  blocks of a language become one virtual file `.<doc>-blocks.lean` next to
  the document (`lsp-blocks.scm:178-181`). This is exactly the notebook
  model that Story A needs.
- Servers are started with `/bin/sh -c CMD`, in TeXmacs's own working
  directory (`System/Link/pipe_link.cpp:241-265`; Qt:
  `Plugins/Qt/QTMPipeLink.cpp:86-92`).

### Works with zero new code (expected; to be confirmed in a spike)

The following should work once `lake` is on `PATH` and the root happens to
be right (for example a `.git` at the Lake root):

- diagnostics, including `sorry` warnings;
- hover with types and docstrings;
- completion, which the server triggers on `.`;
- go-to-definition into `.lake/packages/mathlib/...`;
- references, rename and document highlights;
- document symbols (outline and folds);
- semantic tokens over tree-sitter colours (Lean's legend adds
  `leanSorryLike`, which we simply ignore);
- inlay hints;
- code actions ("Try this:" suggestions of `exact?` and `simp?` arrive as
  quick fixes);
- the `$/progress` work-done reports.

Formatting is not offered by Lean. Signature help is only in recent
versions, so whether it works is unverified.

## 4. What is missing

### 4.1 Project root and working directory

- `root-markers` (`lsp-documents.scm:385-387`) lacks `lakefile.lean`,
  `lakefile.toml` and `lean-toolchain`. A Lake project that is not a git
  repository, or that is a subdirectory of a monorepo, gets the wrong root.
- **`lake serve` reads the Lake configuration from its working directory.**
  Our servers inherit TeXmacs's cwd. Lake's `serve`
  (`src/lake/Lake/CLI/Serve.lean`, `serve`) loads the workspace from the
  config. When none loads, it prints "falling back to plain `lean
  --server`", and `import Mathlib` then fails with "unknown package".
  Fix: start the command as `cd <root> && exec <cmd>` (POSIX shell; both
  pipe links already use `sh -c`). A generic `cwd` field on the client is
  preferable to a Lean special case, because rust-analyzer and gopls also
  care about it.
- **elan's `~/.elan/bin` is not on `PATH`** for a macOS app started from the
  Finder or Dock. `lsp-server-command` (`lsp-client.scm:71-81`) then
  reports no server. Fix: add `$HOME/.elan/bin/lake serve` as a fallback
  command in the table, with `$HOME` expanded in `command-program`.
  Alternatively, add a generic "extra search path" preference for servers.
- **Toolchains.** `lake` and `lean` are elan shims. Running them in a
  project with a `lean-toolchain` it lacks **downloads that toolchain**
  (hundreds of MB) before answering. The initialize timeout is 120 s
  (`lsp-client.scm:330`), so it may expire on first use. While making this
  plan, running `lean --version` started such a download. The message in
  the server's stderr log should be shown in the focus bar.

### 4.2 Initialization options and Lean client capabilities

`lsp-client-initialize` (`lsp-client.scm:294-328`) sends fixed parameters.
Lean reads `initializationOptions` (`editDelay`, `hasWidgets`) and the
experimental client capability for silent diagnostics. A per-language hook
is needed, for example `(lsp-initialization-options lan)` and
`(lsp-extra-capabilities lan)`, which overloads can extend from
`lsp-lean.scm`.

`didOpen` may carry `dependencyBuildMode` (`always`, `once` or `never`;
`Lean/Data/Lsp/Extra.lean`). The default rebuilds the imports at each open.
For a document that imports Mathlib, `once` is the friendly choice, which
means `lsp-document-open-lines` (`lsp-documents.scm:102`) needs a hook for
extra parameters.

### 4.3 The goal view (infoview)

None of this exists. Lean offers two levels:

- **Plain**: `$/lean/plainGoal` (TextDocumentPositionParams) returns
  `{rendered, goals: [String]}`; `$/lean/plainTermGoal` returns
  `{goal, range}` (`src/Lean/Data/Lsp/Extra.lean`). Each goal is text such
  as `case h\nx : ℕ\n⊢ P x`. Emacs `lean4-mode` builds its `*Lean Goal*`
  buffer from exactly this.
- **Interactive**: `$/lean/rpc/connect {uri}` returns a `sessionId`. Then
  `$/lean/rpc/call` is called with `method: "Lean.Widget.getInteractiveGoals"`
  (also `getInteractiveTermGoal` and `getInteractiveDiagnostics`). The
  result has `goals: [{hyps: [{names, type, val?, isInserted?,
  isRemoved?}], type, userName?, goalPrefix, isInserted?, isRemoved?}]`,
  where `type` is `TaggedText` of subexpression infos
  (`src/Lean/Widget/InteractiveGoal.lean`). The client must send
  `$/lean/rpc/keepAlive` about every 10 s, or the session is dropped, and
  must release references with `$/lean/rpc/release`. VS Code's infoview
  (a React web view) and lean.nvim use this level for hovers inside goals,
  goal diffs and widgets.

Both requests wait until the file is elaborated up to the position. They
must therefore be asynchronous, sent after the pending `didChange` (call
`lsp-sync-pending` first), and cancelled when the cursor moves on.

### 4.4 File progress

`$/lean/fileProgress {textDocument, processing: [{range, kind}]}` is a
notification, with kind 1 = processing and 2 = fatalError. VS Code shows an
orange bar in the gutter; lean.nvim shows it in the sign column. We have no
handler, so the status shows "ready" while Mathlib is still loading.

### 4.5 Unicode abbreviations

The reference table is `abbreviations.json` of `vscode-lean4/lean4-unicode-input`
(1865 entries, Apache-2.0, which is GPLv3-compatible). Some entries contain
`$CURSOR`, for example `"<>": "⟨$CURSOR⟩"` and `"norm": "‖$CURSOR‖"`.

VS Code's behaviour, from `package.json` settings:

- the leader is `\` (`lean4.input.leader`);
- the abbreviation is replaced when the next character no longer extends any
  abbreviation;
- it is replaced at once when it is unique (`eagerReplacementEnabled`,
  default true);
- commands convert the current abbreviation or search for a symbol.

In TeXmacs, prog mode binds `\` to a literal backslash and `\ var` to a
hybrid command (`prog/prog-kbd.scm:47-48`). Lean code almost never needs a
literal `\`; it occurs only in strings and in `fun x ↦` written as `\x`.

### 4.6 Messages and diagnostics

- The footer shows only the first line of a diagnostic
  (`lsp-edit.scm:363-383`). Lean's `#check`, `#eval` and "unsolved goals"
  messages are multi-line, and in Lean the message is the useful part.
  They belong in the goal panel ("Messages at cursor", as in VS Code).
- Lean adds fields to diagnostics: `fullRange`, `isSilent`, and
  `leanTags` (`unsolvedGoals`, `goalsAccomplished`). Silent diagnostics
  should not be underlined. "Goals accomplished" may be shown as a mark.

### 4.7 Server startup cost and code blocks

- The watchdog starts fast. Each open file gets a **worker process** that
  loads all imports (Lean `src/Lean/Server/README.md`). `import Mathlib`
  takes seconds to tens of seconds and several GB of memory-mapped
  `.olean` files, which must have been fetched with `lake exe cache get`.
- With unjoined blocks (`lsp:join code blocks` off), each block is its own
  file, worker and Mathlib import. That is unworkable. **For Lean, blocks
  must be joined** (force it per language).
- `lsp:code blocks` is off by default. A Lean document should enable
  following its blocks, either through a document-level switch or
  automatically when the document has `lean-code`.
- The virtual file `.<doc>.tm-blocks.lean` starts with a dot and contains
  dashes and dots. Lean derives a module name from the path; a scratch file
  outside the package's source directories should still work (it gets the
  workspace's search path through `lake setup-file`), but this must be
  checked in the spike.

### 4.8 Syntax colours

These already exist through tree-sitter. The grammar is
Julian/tree-sitter-lean, which its README calls an "experimental start". It
does not track Lean's extensible notation, so user and Mathlib notations
(`∑ i ∈ s, f i`, `‖x‖₊`, custom tactics) parse as errors or generic nodes.
This is acceptable because:

- the server's semantic tokens override the colours once the file is
  elaborated;
- tree-sitter syntax errors are not shown as diagnostics where a server
  follows the code (`code-features.md`, phase 5).

Without a server, the syntax errors of tree-sitter will be noisy on real
Lean code. Consider suppressing `syntax-errors` for Lean entirely.

### 4.9 Fonts

`lean` sets `font-family rm` (`env-program.ts:198`). Glyph coverage for
`⟨⟩ ⦃⦄ ℕ ℝ ∑ ∏ ⁻¹ ₊ 𝓝 ↦ ⊢` in the code font must be checked, as must
the Cork round trip of astral characters (`<#1D4DD>`) through
`cork->sourcecode` and `source->cork` (`lsp-base.scm:140-150`). Missing
glyphs are the most visible risk for mathematicians.

## 5. How TeXmacs could render goals

1. **Plain text, coloured (phase 2).** Each string of `goals` goes into a
   `lean-code` block, so tree-sitter colours it. The hypotheses come first
   and the `⊢` line is emphasised. Cheap and faithful; it is what Emacs
   does.
2. **Interactive tagged text (phase 4).** Convert `TaggedText`
   (`{text}`, `{append: [...]}`, `{tag: [info, child]}`) into TeXmacs
   markup. Each tagged subterm becomes an `action` or `mouse-over`
   region. Hovering or clicking calls
   `Lean.Widget.InteractiveDiagnostics.infoToInteractive` to show its type
   and docstring in a balloon (reuse `show-hover`, `lsp-edit.scm:656`).
   `isInserted?` and `isRemoved?` give the goal diff (green and red, as in
   VS Code).
3. **Typeset mathematics (research, phase 5).** Two routes:
   - **(a) A TeXmacs-side converter** from the pretty-printed Lean text to
     math markup: `∀ x ∈ s, f x ≤ g x` with italic variables, `ℝ` as
     `<bbb-R>`, `x⁻¹` as a superscript, `∑ i ∈ s, f i` as a big operator.
     It is fragile with arbitrary notations, but it can work on the
     `TaggedText` tree, whose subterm boundaries reduce guessing.
   - **(b) A small Lean package** with an RPC method that delaborates goals
     to LaTeX. TeXmacs already imports LaTeX math. This is more robust but
     needs a Lean dependency in the user's project.

   Either way, plain text must stay one click away: in a proof assistant,
   the exact syntax is the ground truth.

**Where the panel goes.** Use a right side tool (`tm-tool*` with
`texmacs-output`), as the git panel does (`version/git-widgets.scm:522`),
opened with `tool-select :right` (`kernel/gui/menu-widget.scm:1483`). Side
tools are rebuilt when the menus update, so the panel shows cached state
and refreshes through `update-menus` (see the note at `git-widgets.scm:505-509`
and `lsp-status-refresh`, `lsp-edit.scm:292-300`). The `Outline`
auxiliary page (`code/code-navigate.scm:91-111`) is the wrong model here:
`open-auxiliary` switches the view away from the code. A quick fallback for
phase 2 could still be a balloon at the cursor (`show-tooltip`).

## 6. Phased plan

### Phase 0: spike (0.5-1 day, no commits)

With the local elan install (`~/.elan/bin`, toolchains 4.22, 4.26 and
4.35), make a small Lake project with Mathlib cache and a project without
dependencies:

- a `.lean` file;
- a `.tm` with joined `lean-code` blocks.

Record which of the features in section 3 actually work: colours, folds,
code actions, Unicode positions and the virtual file module name. Measure
the time to the first diagnostics and the memory used. This decides the
details of phase 1.

### Phase 1: plumbing (2-3 days)

- `progs/lsp/lsp-documents.scm`: add `lakefile.lean`, `lakefile.toml` and
  `lean-toolchain` to `root-markers`. Better: prefer the nearest
  `lakefile.*` over an outer `.git`, by making the markers per language
  through a new table property `(root-markers ...)`.
- `progs/lsp/lsp-client.scm`:
  - `lsp-client-start` takes the root and wraps the command as
    `cd '<root>' && exec <cmd>` (quote the root; on Windows, use
    `cd /d`);
  - add per-language hooks for `initializationOptions` and extra
    capabilities;
  - expand `$HOME` in the server commands.
- `progs/kernel/texmacs/tm-program-languages.scm`:
  - Lean servers become `("lake serve" "$HOME/.elan/bin/lake serve")`;
  - optionally add properties `(join-blocks #t)` and `(follow-blocks
    auto)`.
- `progs/lsp/lsp-blocks.scm`: always join for languages that ask for it.
  Follow Lean blocks when the document has any, even with `lsp:code
  blocks` off (a decision for the maintainer, see section 9).
- Show elan or Lake stderr in the server log and status, so that
  "downloading toolchain" is visible.
- Docs: a "Lean" section in
  `doc/main/editing/man-language-servers.en.tm` covering the
  prerequisites (elan, `lake exe cache get`) and the root rules.
- Tests: see section 7. **Risk**: shell quoting of roots with spaces or
  quotes.

### Phase 2: plain goals, messages and progress (4-6 days)

- New `progs/lsp/lsp-lean.scm`, loaded by `lsp-code.scm` or
  `code-features.scm` when Lean is in the table. It handles:
  - `$/lean/fileProgress`: keep the processing ranges per URI and feed
    `lsp-client-progress` or the focus bar ("Lean: elaborating 34%"). A
    later step marks unprocessed lines with a layer (`code-set-line-layers`,
    from `code-features.md` §3.3) or a gutter colour;
  - goals at the cursor: on `notify-cursor-moved` in a followed Lean target,
    debounced (about 200 ms, as VS Code's `infoview.debounceTime`). Sync,
    then send `plainGoal` and `plainTermGoal`; cancel the previous request
    (`lsp-cancel`). Store the result and call `lsp-status-refresh`;
  - messages at the cursor: the diagnostics whose `fullRange` or `range`
    contains the cursor, with full text.
- New `progs/lsp/lsp-lean-widgets.scm`: `(tm-tool* (lean-goals-tool win))`
  with three parts: "Tactic state" (goals as `lean-code`), "Expected type"
  (term goal) and "Messages". Each part has a "No goals" or "Goals
  accomplished" state and a "Processing…" state.
- Menus and keys:
  - a "Lean goals" entry in `code-server-menu` (`lsp-code.scm:41`), shown
    in Lean only;
  - a key binding (e.g. `std L`) in `prog/prog-kbd.scm` under
    `in-prog-lean?`;
  - auto-open the panel the first time goals arrive, behind a preference.
- Lean diagnostics: honour `isSilent`. Show the whole message in the panel;
  the footer keeps its first line.
- **Risks**:
  - requests that block for tens of seconds while imports load (the panel
    shows "Processing", never waits synchronously);
  - goal positions inside joined units must map through `lsp-target-lo`
    and `lsp-unit-lo-of` (`lsp-blocks.scm:285-305`);
  - side tools in the Vue, Cocoa and browser front ends (the browser build
    has no servers anyway).

### Phase 3: Unicode abbreviations (2-4 days)

- New `progs/prog/lean-input.scm` (or `progs/lsp/lean-input.scm`; it does
  not need a server) and a data file
  `progs/prog/lean-abbreviations.scm`, generated from `abbreviations.json`
  by a script (`misc/admin/lean-abbrev.py`). The data file has the
  characters in Cork (`<#2200>`, never raw UTF-8) and keeps the
  Apache-2.0 notice.
- Behaviour, as in VS Code:
  - `\` under `in-prog-lean?` starts an abbreviation, overriding
    `prog-kbd.scm:47-48` in Lean only. The pending text is shown underlined
    (a selection layer);
  - each key extends it while it is a prefix of some abbreviation; when it
    no longer is, or is unique and complete, it is replaced;
  - `$CURSOR` places the cursor; `tab` and `space` force the conversion;
  - `\\` gives a literal backslash.
- Implementation: in `keyboard-press` (as `lsp-edit.scm:880` does) or with
  a state machine on `kbd-map` that falls through. Avoid 1865 `kbd-map`
  entries: they would clash with TeXmacs's own sequences.
- Also an "Insert Unicode symbol…" dialog with search. Optionally, the same
  input in math-free text fields of the goal panel.

### Phase 4: interactive goals over RPC (1.5-2 weeks)

- `lsp-lean.scm`:
  - an RPC session per URI: `$/lean/rpc/connect`, then a `keepAlive` every
    10 s with `delayed (:pause 10000)` while the document is open;
    reconnect when the worker restarts (the RPC error code
    `RpcNeedsReconnect`);
  - `Lean.Widget.getInteractiveGoals`, `getInteractiveTermGoal` and
    `getInteractiveDiagnostics`;
  - release references through `$/lean/rpc/release`.
- `TaggedText` to markup converter: subterm hover and click through
  `infoToInteractive`, the goal diff colours, hypothesis filters (hide
  instances and types, as in VS Code's infoview settings), and
  go-to-definition from a name in a goal.
- User widgets (ProofWidgets: JS components) are **out of scope**. We would
  need a JS runtime in the panel. Show "this widget is not supported" with
  its plain-text fallback.
- **Risks**: the protocol is versioned with Lean, and the RPC wire format
  changed in 2025 (`rpcWireFormat?`), so pin and test against at least two
  toolchains. Also the cost of rebuilding the side tool's markup for large
  goals.

### Phase 5: options and research (open-ended)

- Typeset goals (§5.3): a prototype of route (a) on the `TaggedText` tree.
- "Export Lean blocks to file" (tangle) and "Check with lake build" for
  Story C.
- "Restart file" (Lean's "refresh file dependencies") after editing an
  imported module, and a "Restart server" entry for the existing client
  (`lsp-client-forget`).
- An in-document `lean-goal` macro: a snapshot of the goal at a marked
  place, typeset in the paper.

## 7. Testing

**Without Lean (runs in CI everywhere).** Extend the procedure-based fake
client of `progs/check/lsp-test.scm:319-342` (`fake-client`, `feed`,
`answer-to`, `running-client`), in a new group or a new suite
`progs/check/lean-test.scm` registered in `check/check-master.scm:196,221`:

- root detection with `lakefile.toml` under a `.git`;
- the command wrapping with `cd` (inspect the command string; no process);
- the initialization options sent;
- `fileProgress` turned into the status text;
- `plainGoal` request parameters at a cursor in a joined unit, and answer
  handling, including cancellation of stale requests;
- `isSilent` filtering;
- the `TaggedText` to markup conversion, and the RPC session lifecycle
  (connect, keepAlive scheduling, reconnect on error), fed with recorded
  JSON;
- abbreviations: the state machine on synthetic key sequences in a Lean
  buffer, the `$CURSOR` cases, the prefix and unique rules, and the Cork
  output.

**Recorded fixtures.** Capture real exchanges once with
`LEAN_SERVER_LOG_DIR` and store small JSON files under
`TeXmacs/tests/lean/` (no raw UTF-8 in `.tm` files; JSON fixtures are fine
in UTF-8).

**With Lean (optional).** Guard the tests with `(url-exists-in-path?
"lake")`, as clangd is (`lsp-test.scm:781,925`). Use a tiny Lake project
without Mathlib, created in a temporary directory. Check:

- a `theorem t : 1 + 1 = 2 := by` followed by a `sorry` line: the warning;
- `plainGoal` inside the `by` block shows `⊢ 1 + 1 = 2`;
- a rename;
- a joined `.tm` with two `lean-code` blocks where the second uses the
  first's definition.

A CI job could install elan
(`curl https://elan.lean-lang.org/elan-init.sh`, about 200 MB toolchain)
nightly only, on Linux.

**Manual checks per phase.** In a window (Qt and Vue), on a Mathlib
project: time to first goal, memory, and glyph coverage. Follow the
preference rule: tests must not write preferences (use `lsp-blocks-force!`
and `lsp-blocks-join-force!`, `lsp-blocks.scm:40-48`).

## 8. Risks

- **First-use cost**: toolchain download and Mathlib import (seconds to
  minutes, GBs). This needs clear status, never a frozen UI, and docs on
  `lake exe cache get`.
- **The cwd and PATH plumbing** differs on Windows: no `sh`, and elan lives
  in `%USERPROFILE%\.elan\bin`. Windows CI is CRLF; Lean files with CRLF
  must round-trip.
- **Joined-block line mapping** under heavy editing; it is already
  audited, but goals add a new position-based request kind.
- **Tree-sitter grammar quality** for Mathlib notation: noisy colours or
  errors before the server answers.
- **Glyph coverage and astral characters** in the code font.
- **Protocol drift** of the RPC and widget APIs across Lean releases.

## 9. Open questions for the maintainer

1. Should a document with `lean-code` blocks follow them automatically,
   even though `lsp:code blocks` is off by default? Or should there be a
   per-document switch (e.g. an init variable `lean-project`)?
2. Where does the goal view go: a right side tool (recommended), a bottom
   tool, or a separate window? Should it auto-open?
3. Is overriding `\` (hybrid commands) in Lean code acceptable? Should
   `\ var` keep the hybrid command?
4. How far should typeset-math goals go, given that route (b) needs a Lean
   package in user projects?
5. Is CI with a real Lean toolchain wanted (nightly), or only fixtures?
6. Should we ship `abbreviations.json` in converted form (Apache-2.0
   notice), or read it at run time from an installed vscode-lean4 or
   lean4-mode when present?
7. Which minimum Lean version to support (affects signature help, rename
   and the RPC wire format)?
8. Should `cwd` become a generic client feature for all servers (useful
   for rust-analyzer and gopls), or stay a Lean-only wrapper?

## 10. Sources

- Lean LSP extensions: https://github.com/leanprover/lean4/blob/master/src/Lean/Data/Lsp/Extra.lean
- Server architecture (watchdog, workers, `lake setup-file`, `LEAN_SERVER_LOG_DIR`): https://github.com/leanprover/lean4/blob/master/src/Lean/Server/README.md
- Interactive goals: https://github.com/leanprover/lean4/blob/master/src/Lean/Widget/InteractiveGoal.lean
- `lake serve` fallback: https://github.com/leanprover/lean4/blob/master/src/lake/Lake/CLI/Serve.lean
- VS Code extension settings (input leader, eager replacement, infoview): https://github.com/leanprover/vscode-lean4/blob/master/vscode-lean4/package.json
- Abbreviations: https://github.com/leanprover/vscode-lean4/blob/master/lean4-unicode-input/src/abbreviations.json
- lean.nvim (infoview, abbreviations, progress in the sign column): https://github.com/Julian/lean.nvim
- Emacs lean4-mode: https://github.com/leanprover-community/lean4-mode
- tree-sitter-lean (experimental): https://github.com/Julian/tree-sitter-lean
