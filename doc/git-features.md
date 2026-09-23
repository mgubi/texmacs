# Git support in TeXmacs: features of the `wip-git-versioning` branch

This page lists what the branch adds to TeXmacs. It is the changelog for
reviewers and testers. For how the code is organised, see
[git-implementation.md](git-implementation.md); for the plan and its
history, see [git-plan.md](git-plan.md).

**Base:** `svn_sync`, the TeXmacs 2.1.5 upstream mirror, 14 commits ahead
as of 2026-09-24.
**Scope:** `src/TeXmacs/progs/version/` (Scheme), small C++ additions in
`src/src/System/Misc/sys_utils.cpp` and
`src/src/Plugins/Unix/unix_sys_utils.cpp`, the user manual
(`man-versioning.en.tm`), and one hook in `tm-files.scm`.

Everything below is in the **Version** menu, which you enable with
Tools → Versioning tool.

---

## 1. At a glance

| Area | Before (upstream) | With this branch |
|------|-------------------|------------------|
| Repository detection | `.git` must be a directory; paths with spaces break | Worktrees, submodules and any path; no process spawned |
| Running git | Shell command string, stdout only, errors silent | argv without a shell, with exit code and stderr, and a log of recent commands |
| File actions | Register (add) only; no per-file commit for git | Add, stage, unstage, commit this file, discard, compare with any revision |
| Repository actions | Read-only status and log pages | Clickable status, log, branches and output pages; commit dialog; branches, tags, stashes; fetch, pull, push, clone |
| Network commands | None | Run in the background and can be cancelled |
| Conflicts | Text conflict markers break `.tm` files | Structured 3-way merge; only real conflicts are left to resolve |
| Command-line merges | Text merge | Optional git merge driver that runs the TeXmacs structured merge |
| Side panel | None | Git panel with status and actions |

---

## 2. Working with a single document

When the current document is in a git working tree, the Version menu
shows a **Git** group with the entries that fit the file's state:

| Entry | When shown | Effect |
|-------|------------|--------|
| **Add to repository** | the file is untracked | `git add` |
| **Stage changes** | modified, or partially staged | `git add` |
| **Unstage changes** | staged, partially staged, or newly added | `git reset`; a rename also unstages the removal of the old name |
| **Commit this file…** | there is something to commit | Prompts for a message, then commits only this file (adding it first if needed). Refused during a merge. |
| **Discard changes…** | modified | Restores the staged version after a confirmation, which warns if unsaved edits would also be lost |
| **Compare with → Last commit / Staged version / Branch …** | TeXmacs documents | Opens the **structured comparison** between the document and that revision |
| **Resolve conflict…** / **Mark as resolved** | the file has a merge conflict | See section 5 |

**History** (upstream) keeps working. It follows renames, and each
revision opens as a read-only document.

Detected states: untracked, unmodified, modified, staged, partially
staged, added, deleted, conflicted.

---

## 3. Working with the repository: Version → Git

| Entry | Effect |
|-------|--------|
| **Status** | A page listing the branch, the upstream and ahead/behind counts, then conflicts, staged changes, unstaged changes and untracked files. Each file has a link to open it and actions `[compare \| stage \| unstage \| discard \| add \| resolve \| mark resolved]`. The page header links to Commit…, Stage all, Fetch, Pull, Push and the other pages. |
| **Git panel** | The same information in a side panel: branch, Commit/Pull/Push/Cancel/Status/Refresh buttons, and one line per changed file with Stage/Unstage/Resolved buttons. It refreshes after every git action and after saving. |
| **Log** | The commit history, 250 commits per page with a "More…" link. Each commit opens a **commit page** showing author, date, parents, the full message and a per-file table of changes with +/− bars, links to the file at that revision, and "compare with current" for TeXmacs documents. |
| **Branches and tags** | Local and remote branches with upstream tracking and last commit date; actions switch, merge into current and delete. Also the tags, and the stashes with pop and drop. |
| **Commit…** | The **commit dialog** (below). |
| **Stage all changes** | `git add --update` |
| **New branch…** / **Switch to branch** / **Merge branch** | Create and switch; switch; merge into the current branch. Before switching or merging, it offers to save modified documents. |
| **Tag this version…** | Annotated tag (lightweight if the message is empty). |
| **Fetch** / **Pull** / **Push** | Run in the background (section 4). Pull is `--ff-only`. The first push of a branch sets its upstream to `origin`. |
| **Cancel running command** | Shown while a background command runs. |
| **Stash changes** / **Restore last stash** | `git stash push` / `git stash pop`, after offering to save modified documents. |
| **Merge documents structurally** | Installs the merge driver (section 6). |
| **Git output** | The last 50 git commands with exit code, stdout and stderr. |
| **Refresh** | Forgets cached state, including which files are versioned. |

Outside any repository, the Version menu offers:

* **Create Git repository…** — `git init` in the document's directory.
* **Clone Git repository…** — asks for the repository and target
  directory, clones in the background, then opens the status page.

### Commit dialog

* A message editor and a list of all changed files. The files with
  staged changes start out selected.
* An **Amend last commit** toggle. Amending with an empty message keeps
  the old message.
* On commit:
  * a selected file without staged changes is staged entirely;
  * a selected file with staged changes is committed exactly as staged,
    so partial staging is preserved;
  * files that are not selected are unstaged.
* **During a merge**, the dialog prefills the prepared merge message. It
  refuses to commit while conflicts remain, or if some files are
  deselected.
* If the commit fails, the dialog stays open and the error appears in the
  footer.

### Keeping documents in sync

* Operations that change files on disk (discard, switch, merge, pull,
  stash, stash pop) **reload the open documents** whose contents changed.
* A document that also has unsaved edits is never reloaded silently; a
  warning is shown instead.
* Saving a document updates the menus, pages and panel right away.

---

## 4. Background operations

* Fetch, pull, push and clone run **asynchronously**, so the editor stays
  responsive. The result appears in the footer when they finish, and the
  affected documents are reloaded.
* At most one background command runs per repository. Its buttons are
  disabled meanwhile.
* **Cancel** terminates the command and every process it started, such as
  `ssh` or `git-remote-https`.
* Git never waits for a password prompt it cannot show
  (`GIT_TERMINAL_PROMPT=0`). Use a credential helper or an ssh agent.

---

## 5. Merge conflicts in TeXmacs documents

**Version → Resolve conflict…** on a conflicted `.tm` file:

1. performs a **structured three-way merge** of the common ancestor, your
   version and their version;
2. applies automatically every change made on only one side. This works
   at the level of paragraphs, words, markup such as sections, and math.
   Git would report a conflict for two different edits in the same line;
   here they merge;
3. shows the remaining **real conflicts** with the usual comparison
   markup: old (red) is yours, new (green) is theirs. You step through
   them with the Version menu or the shortcuts, retaining one side each
   time;
4. **Mark as resolved** warns if some differences remain, then saves the
   document and stages it.

Examples:

| Base | Ours | Theirs | Result |
|------|------|--------|--------|
| The quick brown fox jumps. | The **slow** brown fox jumps. | The quick brown fox **leaps**. | The slow brown fox leaps. |
| A. / B. / C. | A**!** / B. / C. | A. / B. / C**!** | A! / B. / C! |
| (section Intro) Text. | (section Introduction) Text. | (section Intro) Text, more. | (section Introduction) Text, more. |
| Let *x+y* be. | Let *x+z* be. | Let *x+y* be given. | Let *x+z* be given. |
| The quick fox. | The slow fox. | The lazy fox. | The ⟨slow\|lazy⟩ fox, with one conflict to resolve |

If the document was deleted on one side, you are told to edit it by hand.
If there is no common ancestor (the file was added on both sides), a
two-way comparison is shown instead.

---

## 6. Structured merges from the command line

**Version → Git → Merge documents structurally** configures the
repository:

* `.gitattributes` gets `*.tm merge=texmacs`. This file can be committed
  and shared.
* The local git config gets a `merge.texmacs.driver` that runs TeXmacs
  headless.

After that, `git merge` and `git pull` in a terminal also merge TeXmacs
documents structurally, which takes about a second per document. Any
remaining conflicts are written into the file as TeXmacs difference
markup instead of `<<<<<<<` text markers, so the document still opens
and can be resolved in TeXmacs. Collaborators who have not installed the
driver get git's usual text merge.

---

## 7. Robustness and safety

* **Paths:** repositories whose paths contain spaces, quotes or
  non-ASCII characters work, as do linked worktrees and submodules.
* **Quoting:** commit messages, branch names and file names are passed
  without a shell, so `"`, `$`, backquotes and `\` are safe.
* **File names are never patterns**, so discarding `n[1].tm` does not
  touch `n1.tm`.
* **Links can't turn into git options.** Revisions and names that come
  from links in documents are checked.
* **Page actions only work on git pages.** The clickable actions only run
  from TeXmacs's own git pages for that repository, so a document cannot
  embed a working "discard everything" button.
* **Empty input never blocks.** Commands always see the end of their
  input, so an empty commit message cannot freeze the editor.
* **X11 build:** it has no direct process spawning, so git runs through a
  correctly quoted shell command there.
* **Encodings:** git output (UTF-8) and TeXmacs text (cork) are
  converted where they meet.

---

## 8. New developer-level APIs

| API | Where | Purpose |
|-----|-------|---------|
| `(async-evaluate-system argv input callback)` → id | C++ glue | Runs a command without a shell in the background; `callback` receives `(code stdout stderr)`. |
| `(async-evaluate-cancel id)` | C++ glue | Terminates such a command and its process group. |
| `(merge-versions base ours theirs)`, `(merge-conflicts t)` | `version-merge.scm` | Structured 3-way merge of strees. |
| `(version-normalize t)`, `(version-denormalize t)` | `version-compare.scm` | Public wrappers around the comparison helpers. |
| `(version-notify-saved name)` | `tm-files.scm` | Hook called after a document is saved. |
| `git-run`, `git-run-async`, `git-status`, `git-file-state`, `git-log`, … | `git-base.scm` | The git process layer and parsers (see git-implementation.md). |

Revision names understood by `version-revision` for git: a commit hash,
any git revision (`HEAD`, a branch), `INDEX`, and `BASE`, `OURS` and
`THEIRS` for the stages of a conflicted file.

---

## 9. Tests

```sh
doc/tests/run-git-tests.sh <scratch-dir>         # headless: 54 checks
doc/tests/run-git-tests.sh --gui <scratch-dir>   # Qt offscreen: 26 checks
```

The scripts create scratch repositories (with spaces in paths, a linked
worktree, a bare remote with clones, and conflicting merges). They use a
private `TEXMACS_HOME_PATH` and open no windows. The checks cover:

* detection and every file state;
* quoting;
* history and revisions;
* every page;
* branches, tags and stashes;
* renames and conflicts;
* the merge engine and the merge driver;
* push, fetch, pull, clone and cancel;
* reloading of open documents;
* the save hook, the side panel, and the review regressions.

---

## 10. Known limitations

* The commit dialog, panel and menus were exercised by scripts only. They
  have not yet been checked by hand in a normal session.
* A background command finishes only when all its output is closed. A
  helper that keeps it open makes the command look running until you
  cancel it.
* Plugin pipe links may collect a finished git process first, and the
  command is then reported with exit code −1.
* Opening a document inside a repository runs `git status`, which may run
  that repository's `core.fsmonitor` hook, as with any git GUI.
* Clone asks for the repository and directory in the footer; there is no
  dedicated dialog.
* There is no textconv diff driver for `git diff` of `.tm` files on the
  command line.
* Windows and Android run background commands synchronously; only the
  callback is delayed.
