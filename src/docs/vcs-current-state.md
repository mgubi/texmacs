# Current state of VCS support (SVN and Git)

This describes the code on `master` unless stated otherwise. Files are in
`src/TeXmacs/progs/version/`.

| File | Role |
|------|------|
| `version-tmfs.scm` | Generic API, backend detection and dispatch, `tmfs://history`, `tmfs://revision` and `tmfs://commit` pages, wrapper forwarding, buffer helpers (`update-buffer`, `register-buffer`, `commit-buffer`). |
| `version-svn.scm` | SVN backend (132 lines). |
| `version-git.scm` | Git backend and `tmfs://git/{status,log}` pages (about 440 lines). |
| `version-menu.scm` | The **Version** menu. |
| `version-compare.scm`, `version-edit.scm`, `version-drd.scm`, `version-kbd.scm` | Document diff engine, navigating and retaining differences, keyboard shortcuts. These are independent of any VCS. |

## 1. The generic backend API

Every backend overloads these with `(:require (== (version-tool name) "<tool>"))`.
The fallbacks are in `version-tmfs.scm`.

| Function | Returns | SVN | Git |
|----------|---------|-----|-----|
| `version-tool url` | `"svn"`, `"git"`, `"wrap"` or `#f` | – | – |
| `versioned? url` | bool | – | – |
| `version-status url` | `"unknown"`, `"modified"` or `"unmodified"` | `svn status` | `git status --porcelain <f>` |
| `version-history url` | list of `(rev by date msg)` | `svn log` | `git log --follow` (limit 1000) |
| `version-revision url rev` | file contents as a string | `svn cat -r` | `git show rev:path` |
| `version-beautify-revision url rev` | short label | identity | first 7 characters |
| `version-update url` | message | `svn up --accept theirs-full` | *(none)* |
| `version-register url` | message | `svn add` | `git add` (on master it calls `set-message` itself and returns its result) |
| `version-unregister url` | message | `svn remove --force` | `git reset HEAD` |
| `version-commit url msg` | message | `svn commit -m` | *(none on master: the menu hides Commit for git)* |
| `version-supports-svn-style? url` | bool | #t | #f |
| `version-supports-git-style? url` | bool | #f | #t |
| `version-supports-history? url` | bool | versioned? | versioned? |

The menu is split on the two "style" predicates. **SVN style** gives
Update / Register / Commit for the current file. **Git style** on master only
gives "Global status" and "Global log" (repository-wide pages). Both styles
get History and "Compare with" (the last 25 revisions from
`version-history*`).

### Backend detection (`version-tool`)

* `svn-active?` and `git-active?` look for a `.svn` or `.git` **directory**
  in any ancestor (`url-ancestor` + `url-directory?`). SVN is tested first.
* The result is cached **forever** per URL in `version-tool-table`. Running
  `git init` or cloning while TeXmacs is running is never noticed for files
  whose tool has already been looked up (the cached value is `""`).
* The first time a tool is detected, the backend module is loaded with
  `module-provide`. Before that, only the fallbacks exist.
* `.git` as a **file** (worktrees, submodules; this very checkout is a
  worktree) is not recognised, by either `git-active?` or `git-root`.

### Git plumbing (`version-git.scm`)

* `git-root url` walks up the tree looking for a `.git` directory and
  returns `"/"` when it finds none.
* `git-command url` builds `"git --work-tree=<root> --git-dir=<root>/.git"`.
  The paths are **unquoted**, so a space in the path breaks every command.
  It also breaks worktrees, whose `.git` is a file.
* `current-git-command` and `current-git-root` use `(current-buffer)`. The
  source comment itself says "Warning: do not use it", because the current
  buffer changes while tmfs pages load. They are still used by
  `version-history` (root), `git-commit-file-parent`, `git-commit` and the
  status and log pages.
* Everything goes through `eval-system`, which returns stdout only. **No
  exit codes and no stderr**, so failures are silent or show up as garbage.
* Commit message quoting: `(raw-quote message)` just wraps the message in
  double quotes (`kernel/library/base.scm:217`). A message containing `"`,
  `$`, a backquote or `\` breaks the command or is interpreted by the shell.
* Parsing: `git status --porcelain` (v1, not `-z`) is parsed by the first 2
  characters, with the path from column 3. Renames (`R  old -> new`) and
  quoted paths (spaces, non-ASCII) are handled incorrectly.
* `git-commit` (global) commits **whatever is currently staged**. The status
  page can't stage anything, so the only staging path is Register (`git
  add`) on the current file.
* `git-commit-diff` and `git-show-normal` show `--numstat` with `+`/`-` bars.
  That code lives in version-tmfs.scm although it is git-only.
* The history item format is `<hash>:<tmfs-url-of-path-at-that-commit>`, so
  `--follow` across renames still loads the right path
  (`version-revision-url` splits on `:`).

### SVN backend

It is small and complete for the SVN workflow it targets: per-file status,
log, cat, update, add, remove and commit. `eval-svn` strips
`svnserve: warning:` lines. It has the same quoting weaknesses (`url->string`
for paths, and commit messages escaped only for `"`).

## 2. Review of the WIP commit `0b8b565da6` ("Initial version", Dec 2020)

What it tries to do: give git an SVN-like per-file workflow (Register /
Checkout / Commit) inside a "Git" group of the Version menu.

| Change | Assessment |
|--------|------------|
| `src/.gitignore`: 32 build-artifact entries | Useful but unrelated. It should be its own commit (or dropped if master already has an equivalent). |
| `version-register` and `version-unregister` return the last output line instead of calling `set-message` | Correct direction: it matches the SVN contract, where `register-buffer` does the `set-message`. `git add` prints nothing on success, so the message is empty. |
| New `version-checkout` (git: `checkout --theirs -- f`; svn: `svn revert f`) + `checkout-buffer` | **Wrong git semantics.** `--theirs` only makes sense during a merge conflict. Discarding local changes is `git restore -- f` (or `git checkout -- f`). `checkout-buffer` reverts the buffer without first checking for unsaved edits, so they are lost silently. |
| New git `version-commit name msg` = `git commit -m msg <file>` | This per-file commit is reasonable (`git commit <path>` commits that path's working-tree state). It uses `current-git-command` instead of `(git-command name)`, and escapes only `"`. |
| Commented-out git `version-update` | Git has no per-file "update". Pull is repository-wide and belongs in the repository actions. |
| Menu: "SVN" and "Git" groups; the git group gets Register, Checkout and Commit | The SVN group gets a **"Revert" entry that calls `version-interactive-update`**, which is a copy-paste bug (it should call `checkout-buffer`). The `when`-guards call `version-status` up to 4 times per menu opening, which means 4 `git status` runs. |
| Duplicate `remove-empty-strings` in version-git.scm | Marked FIXME. It should move to a shared helper. |

Conflicts with master: master has since changed version-git.scm (raw-quote,
`--follow`, `#\tab`), version-svn.scm (`eval-svn`), version-tmfs.scm and
version-edit.scm. The WIP commit is small enough to re-apply by hand after
rebasing. Only its *intent* is worth keeping.

## 3. Consolidated bug and limitation list (master + WIP)

1. Paths with spaces break every git command (unquoted `--work-tree`).
2. `.git` files (worktrees, submodules) are not detected, and nor is
   `GIT_DIR`.
3. `version-tool` is cached forever, so a later `git init` or clone is not
   picked up.
4. There are no exit codes or stderr, so errors are invisible.
5. Commit messages are not shell-safe.
6. `current-git-*` is used where the file's own root should be.
7. Porcelain v1 parsing does not handle renames or quoted paths.
8. The global commit can't select what to commit, and nothing can be staged
   from the UI.
9. There are no branch, remote, pull, push, fetch, stash, tag, init or clone
   operations.
10. The menus run `git status` repeatedly, with no caching.
11. The `tmfs` classes for versioning are not lazily registered, so restoring
    a `tmfs://git/...` buffer at startup fails.
12. Synchronous execution freezes the UI on slow commands.
13. There is no conflict handling. A merge that leaves `<<<<<<<` markers in
    a `.tm` file produces a document that loads as garbage.
14. `utf8->cork` and `cork->utf8` are applied inconsistently to messages,
    names and paths.
15. There is no "compare with HEAD / index / branch" entry. The commented-out
    `git-compare-with-*` code in version-menu.scm was the start of one.
