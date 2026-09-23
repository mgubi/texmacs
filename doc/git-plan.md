# Plan: full git support in TeXmacs

## Goals

A TeXmacs user, who is often not a git expert, should be able to do the
whole everyday cycle from inside the editor, both for a single document and
for a multi-file project (a master document with includes, styles, images,
bibliography):

* create or clone a repository, or recognise an existing one (including
  worktrees and submodules);
* see what changed: repository status, per-file status, and a *structured*
  diff of `.tm` files using the existing comparison engine;
* stage and unstage files, commit with a message, and amend;
* read the history of a file or the repository, open any revision, and
  compare against HEAD, the index, a branch or any commit;
* discard local changes to a file (restore), with confirmation;
* work with branches (list, create, switch, merge) and remotes (fetch, pull,
  push) without freezing the UI;
* resolve conflicts in `.tm` files with the structured diff instead of text
  conflict markers;
* stash, and create tags for "submitted version" / "camera ready" snapshots.

Non-goals, at least for now: interactive rebase, rewriting history,
submodule management, a generic abstraction over other VCSs beyond keeping
SVN working.

## Design principles

1. **Keep the existing backend architecture.** `version-*` generic functions
   stay overloaded with `:require (version-tool ...)`, so SVN (and remote
   files, `client-tmfs.scm`) keep working. Git-only features get a `git-`
   namespace and are shown only when `version-supports-git-style?` holds.
2. **One safe process layer.** All git calls go through a single function
   that runs argv with no shell (`evaluate-system`), uses `git -C <root>`,
   and returns `(code stdout stderr)`. Nothing else in the git code calls
   `eval-system`.
3. **Machine-readable output only.** Use `status --porcelain=v2 -z
   --branch`, `log -z --format=...` with `%x1f`/`%x00` separators,
   `for-each-ref --format`, and `rev-parse`. Never parse human output.
4. **Always pass the repository explicitly** (derived from the file URL).
   Never use `current-buffer` inside the git layer.
5. **Save before, revert after.** Any command that may touch working-tree
   files first saves the affected buffers (or asks), and afterwards reverts
   every open buffer whose file's mtime changed.
6. **Errors are visible.** A non-zero exit shows stderr in a dialog or the
   status bar, and there is a "Git output" log page with the last N commands.
7. **Structured over textual.** Wherever a diff of a TeXmacs document is
   shown, use `compare-versions` on the trees rather than a text diff.

## Architecture

```
version-menu.scm ──► version-tmfs.scm (generic API, tmfs history/revision)
                      ▲ overloads
   version-svn.scm ───┤
   version-git.scm ───┘  (backend: version-* overloads)
        │ uses
        ▼
   git/git-base.scm      process layer, repo discovery, caching, parsing
   git/git-tmfs.scm      tmfs://git/{status,log,commit,branches,output}/<root>
   git/git-actions.scm   stage/commit/restore/branch/remote/stash/tag
   git/git-widgets.scm   commit dialog, git side panel, branch chooser
   git/git-merge.scm     conflict resolution + external merge driver
```

(The `git/` sub-directory could equally be files `version-git-*.scm` next to
the others. Decide at Phase 1. A separate directory keeps
`version-git.scm` a thin adapter.)

### Process layer (`git-base.scm`)

```scheme
(git-run root args . opts)   ; => (code out err), opts: :input str, :env alist
(git-run-ok root args)       ; => out or signals/reports error
(git-repo-root url)          ; `git -C dir rev-parse --show-toplevel`, cached per dir
(git-available?)             ; executable found, version >= 2.23 (git restore/switch)
```

* The executable comes from the preference `"git executable"`
  (default `"git"`).
* The environment is `GIT_TERMINAL_PROMPT=0`, `LC_ALL=C` for parsing, and
  `GIT_OPTIONAL_LOCKS=0` for status polling. `evaluate-system` has no env
  parameter. The options are to extend the C++ glue with an env argument,
  or to prefix argv with `env VAR=... git ...` on Unix and handle Windows
  separately. Prefer the glue extension.
* A small ring buffer of `(argv code out err time)` feeds the "Git output"
  page.
* **Status cache**: `(git-status-cached root)` keeps the parsed porcelain v2
  output for about 1–2 s and is invalidated by any git action and by
  `save-buffer`. `version-status` and all menu guards read from it.

### Async jobs (needed from Phase 4)

A C++ helper `(system-async argv callback)` that spawns the process, collects
stdout and stderr, and calls `callback code out err` from the event loop when
it finishes. Build it on the existing `pipe_link` / `qt_pipe_link`
infrastructure, or on `QProcess` for the Qt GUI with a fork+poll path for
others. Until it exists, network commands run synchronously behind a
"Running git …" message.

## Phases

### Phase 0: groundwork (no user-visible change)

* Rebase `wip-git-versioning` onto `master` (1541 commits behind). Drop the
  WIP diff to version-*.scm and keep its intent in Phase 2. Move the
  `.gitignore` additions to a separate commit, or drop them if they are
  redundant.
* Add `lazy-tmfs-handler` lines for `history`, `revision`, `commit` and
  `git` in `init-texmacs.scm`.
* Write a test document and a scripted test (`texmacs -x '(load ...)'`) that
  builds a temporary repository in the scratch directory and runs the git
  layer headlessly. Record the recipe in `doc/testing.md`.

### Phase 1: a solid core (fix what exists)

* `git-base.scm`: `git-run`, `git-repo-root` via `rev-parse` (worktrees,
  submodules and paths with spaces all work), `git-available?`, the output
  log, and the status cache.
* Re-implement every existing git function on top of it: `version-status`,
  `version-history` (argv, `-z`, `--follow`, and a limit preference instead
  of the hard-coded 1000), `version-revision`, `git-master`, the commit page,
  the status page and the log page.
* Detection: `git-active?` uses `git-repo-root` (or accepts a `.git` file).
  The `version-tool` cache stores positive results only, or can be cleared
  with "Version → Refresh".
* Correct `cork` and `utf8` conversion for everything displayed and sent.
* Move the git-specific commit-page code out of `version-tmfs.scm`.
* Remove `current-git-command` and `current-git-root`.

*Done when* every current feature works in a repository whose path contains
spaces, in a worktree, and with a commit message containing
`"$\`\\`.

### Phase 2: per-file workflow (the WIP intent, done right)

Version menu, **Git** group, for the current buffer:

* **Stage** / **Unstage**: `git add -- f`, `git restore --staged -- f`.
  Also handle includes: offer to stage the files the document links to
  (images, `.ts` styles, `.bib` files) that are untracked.
* **Commit this file…**: save, then `git commit -m msg -- f` (the SVN-like
  path).
* **Discard changes…**: confirm, then `git restore -- f`, then
  `revert-buffer`. If the buffer has unsaved edits, say so in the
  confirmation.
* **Compare with** → HEAD, the index (`:0`), the last commit that touched
  the file, a branch… These are `compare-with-older` on
  `tmfs://revision/<rev>/<file>`. `version-revision` accepts any rev-spec
  (`HEAD`, `:0`, `branch`, hashes). This revives the commented-out
  `git-compare-with-*` code.
* Menu guards use the status cache, and the labels show the state, e.g.
  "Stage (modified)".

### Phase 3: repository status and commit UI

* The `tmfs://git/status/<root>` page is rewritten from porcelain v2. It
  shows the branch, ahead/behind counts, staged, unstaged, untracked and
  conflicted files, and each file has links: open, diff, stage, unstage,
  discard. Actions are hyperlinks with Scheme actions (`(action "Stage"
  "(git-stage ...)")` markup), followed by a refresh of the page.
* **Commit dialog** (`git-widgets.scm`): a message editor (multi-line
  input) with check boxes for the files to include (pre-filled from staged
  files), plus "Amend last commit" and "Stage all modified" toggles. Commit
  runs `git commit -F -` with the message on stdin, so no quoting is
  needed.
* **Git side panel** (`tm-tool*`, docked right when `(side-tools?)`, a
  window otherwise): a compact status view with the same actions, refreshed
  after saves. This is the long-term replacement for most menu entries.
* The log page gets pagination ("more…"), a per-commit page with a list of
  files, and for each `.tm` file a "show structured diff with parent" link.

### Phase 4: branches, remotes, async

* Branch list via `for-each-ref`, **switch** (`git switch`, refused when
  there are uncommitted changes or offering stash), **create**, **delete**,
  and **merge into current**.
* Remotes: **fetch**, **pull** (`--ff-only` by default, with a preference
  for rebase or merge), **push** (set-upstream on first push). These go
  through `system-async`.
* The status bar or panel shows the branch and ↑↓ counts.
* After switch, pull or merge, revert all open buffers under the root
  (principle 5).

### Phase 5: conflicts and merging TeXmacs documents

* Detect conflicts (porcelain v2 `u` entries). For a conflicted `.tm` file,
  load `:2` (ours) and `:3` (theirs) with `version-revision` and open the
  structured comparison (`compare-versions ours theirs`). The user retains
  per difference and then chooses **Mark resolved** (save + `git add`).
* Improvement: 3-way awareness. Diff base (`:1`) against each side and
  auto-retain changes made on one side only. This means extending
  `version-compare.scm` with a `merge-versions base ours theirs` that emits
  `version-both` only for real conflicts.
* Optional: a **git merge driver** in `.gitattributes`
  (`*.tm merge=texmacs`) running
  `texmacs -headless -x "(git-merge-driver base ours theirs out)" -q`, so
  merges on the command line keep valid `.tm` documents with
  `version-both` markup instead of text markers. This needs the headless
  batch mode to be reliable. Investigate `texmacs --headless` and the
  converter-style `-c`.
* Optional: a **textconv diff driver** (`*.tm diff=texmacs`) that makes
  `git diff` on the command line more readable.

### Phase 6: project-level conveniences

* **Init** ("Version → Put this project under git…"): `git init`, a default
  `.gitignore` for TeXmacs projects (`*~`, `*#`, autosave files, generated
  PDFs as an option), `.gitattributes` for `*.tm` (`text eol=lf` + drivers),
  and a first commit.
* **Clone…**: a URL plus a target directory (async), then open the main
  document.
* **Snapshots**: "Tag this version…" (annotated tag) and "Stash / Restore
  stash".
* **Project commit**: with a master document (`project-file-list`), commit
  all its parts together.
* Documentation: update `man-versioning.*.tm` with a Git section.

## Open questions

1. Minimum git version: `restore` and `switch` need 2.23 (2019). Require it,
   or fall back to `checkout`?
2. Where do the async jobs live (C++ generic, or Qt-only first)? The Vue/SDL
   GUI branch would need its own path.
3. Should the side panel be the primary UI (with the menu as a thin shortcut
   layer), or the other way round?
4. Should *save* optionally auto-stage? The svn "commit on save" style
   already exists through `save-buffer :commit`.
5. Keep SVN support in the same menu (group per tool), or move the
   tool-specific actions entirely into each backend's own menu?

## Progress log

| Date | Step |
|------|------|
| 2026-09-23 | Studied the existing code and wrote the docs and this plan. No code changes yet. |
| 2026-09-24 | Phase 0: rebased on `svn_sync` (it already has `async-eval-system`), lazy tmfs handlers, headless test harness. Phases 1–3 largely done: `git-base.scm` (argv process layer, porcelain v2 status cache, log/numstat/refs parsers), new `version-git.scm` (backend, file and repository actions, status/log/branches/output/commit pages with action links), `git-widgets.scm` (commit dialog, prompts), new Git menus. Network commands are still synchronous (Phase 4). |
| 2026-09-24 | Phase 4: C++ `async-evaluate-system` (argv, stdin, callback with code, stdout and stderr), asynchronous fetch, pull and push with a busy flag and document reloading, shell fallback for X11. Phase 5 (2-way): structured resolution of conflicts in `.tm` files (`OURS`/`THEIRS` stages plus `compare-with-newer`) and mark as resolved. Offscreen GUI test suite. |
| 2026-09-24 | Phase 5, 3-way: `version-merge.scm` (structured diff3 with `merge-versions`). Conflicts in `.tm` files are merged automatically where only one side changed, and only real conflicts are left for the user. |
| 2026-09-24 | Git merge driver (`git-drivers.scm`): command-line merges of `.tm` files are structured too, and conflicts are left as `version-both` markup. Installed per repository from the Git menu. |
| 2026-09-24 | Git side panel (`git-tool`). |
