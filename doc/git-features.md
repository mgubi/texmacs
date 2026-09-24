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

## 1b. User interface

* **Footer status.** For documents in a git working tree, the footer ends
  with the state of the repository, e.g. `Git main · 3 changes · ↑1 · working...`.
  It is computed from the cached status only, so it never runs git while
  you type.
* **The Version menu appears by itself** for documents in a git or SVN
  working tree. Tools → Versioning tool → Automatic is the new default;
  Always and Never are the other choices.
* **Dialogs** replace the footer prompts for "Commit this file", "Save
  snapshot", "New branch" (with "switch to it"), "Tag this version" (with
  "sign"), "Add remote" and "Compare with revision". Names are validated
  in the dialog, which stays open with an explanation until the input is
  valid.
* **Failures are explained** in a dialog. It says in plain words what
  happened (e.g. "Others sent changes first"), offers the likely next
  step (Get changes, Save snapshot, Show status), and has a Details
  button that opens the Git output page.
* **Version menu,** organized as follows:
  * *Conflict* entries on top when relevant;
  * Commit… (or Save snapshot…), Synchronize and Git panel;
  * for the document: Compare with, Restore version, History of this
    document, Who changed what;
  * submenus *This file*, *Project*, *Git (branch, state)*, *Differences*.
* **Git panel,** in these parts:
  * a sync bar with the branch, "N to send / N to get", and Get/Send
    changes (or Synchronize in simple mode);
  * a *Changes* tab with sections Conflicts / Staged / Changed / New
    files, Stage/Unstage icons, Compare and Resolve buttons, and a commit
    box with Suggest and Commit (or Save snapshot);
  * a *History* tab with the last 20 versions of the current document,
    each with Compare and Restore;
  * a *Branches* tab with Switch and New branch….
* **Git preferences** dialog (Version → Git preferences…): versioning
  tool, mode, what to do when both sides changed, git executable,
  large-file limit, blame depth, log length, signing, suggested messages.
* **First-run choice** between simple mode (snapshots) and full mode.
* **Shortcuts** (prefix `version`, i.e. `altcmd #`): `g` Git panel,
  `c` commit or save snapshot, `y` synchronize, `s` status page,
  `=` compare with the last commit.
* **Styled pages:** the status and other pages use buttons and coloured
  badges (style package `git-pages`). The blame page colours each author
  and starts with a legend.
* **Review bar.** After a comparison or a conflict resolution, a bar at
  the bottom of the window shows "Difference 2 of 7" (or "Conflict 2 of
  5"), with Previous/Next, Keep Old/New (Mine/Theirs), Show
  Both/Old/New, Mark as resolved and Close.

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
| **Compare with → Last commit / Staged version / Remote version / Before the last pull or merge / Other revision… / Tag … / Branch …** | TeXmacs documents | Opens the **structured comparison** between the document and that revision; you then accept or reject each difference |
| **Who changed what** | TeXmacs documents | **Blame by paragraph**: the document, with each paragraph (or run of paragraphs) annotated with the commit, author and date that last changed it, or "Not committed yet". It follows paragraphs, not source lines, through the last 30 commits (preference `git blame depth`). |
| **Restore version →** | tracked files | The last 15 versions of the document; restoring one makes it a new change, so the history is kept. Also available as **Restore this version** when viewing an old revision, and as "restore" on commit pages. |
| **Add N missing project files** | the document uses untracked files | Adds the included documents, images, bibliography files and local style files the document (or its project's master document) depends on |
| **Commit project…** | TeXmacs documents | Opens the commit dialog with all changed files of the project preselected |
| **Resolve conflict…** / **Mark as resolved** | the file has a merge conflict | See section 5 |

**History** (upstream) keeps working. It follows renames, and each
revision opens as a read-only document.

Detected states: untracked, unmodified, modified, staged, partially
staged, added, deleted, conflicted.

---

## 3. Working with the repository: Version → Git

| Entry | Effect |
|-------|--------|
| **Git (branch, N changed, M ahead, K behind)** | The label of the submenu itself summarizes the state of the repository. |
| **Status** | A page listing the branch, the upstream and ahead/behind counts, then conflicts, staged changes, unstaged changes and untracked files. Each file has a link to open it and actions `[compare \| stage \| unstage \| discard \| add \| resolve \| mark resolved]`. The page header links to Commit…, Stage all, Fetch, Pull, Push and the other pages. |
| **Git panel** | The same information in a side panel: branch, Commit/Pull/Push/Cancel/Status/Refresh buttons, and one line per changed file with Stage/Unstage/Resolved buttons. It refreshes after every git action and after saving. |
| **Graph** | The history of all branches as a graph, with branch and tag labels. Each commit links to its page. |
| **Log** | The commit history, 250 commits per page with a "More…" link. Each commit opens a **commit page** showing author, date, parents, the full message and a per-file table of changes with +/− bars, links to the file at that revision, and "compare with current" for TeXmacs documents. |
| **Branches and tags** | Local and remote branches with upstream tracking and last commit date; actions switch, merge into current and delete. Also the remotes with their URLs, the tags, and the stashes with pop and drop. |
| **Commit…** | The **commit dialog** (below). |
| **Stage all changes** | `git add --update` |
| **New branch…** / **Switch to branch** / **Merge branch** | Create and switch; switch; merge into the current branch. Before switching or merging, it offers to save modified documents. |
| **Tag this version…** | Annotated tag (lightweight if the message is empty). |
| **Fetch** / **Pull** / **Push** | Run in the background (section 4). When a pull cannot fast-forward because both sides changed, it offers to **merge the remote changes**. If that leads to conflicts, it opens the status page so you can resolve them (section 5). The first push of a branch sets its upstream. |
| **Remotes → Add remote… / Push to / Remove** | Manage the remote repositories. Push goes to the upstream's remote, or `origin`, or the only remote. |
| **Cancel running command** | Shown while a background command runs. |
| **Stash changes** / **Restore last stash** | `git stash push` / `git stash pop`, after offering to save modified documents. |
| **Merge documents structurally** | Installs the merge driver (section 6). |
| **Preferences → Simple mode / Sign commits and tags / Pull** | See sections 3a and 7; the pull mode is fast-forward only (default), merge or rebase. |
| **Git output** | The last 50 git commands with exit code, stdout and stderr. |
| **Refresh** | Forgets cached state, including which files are versioned. |

Outside any repository, the Version menu offers:

* **Create Git repository…** — `git init` in the document's directory,
  with a default `.gitignore` for TeXmacs projects.
* **Clone Git repository…** — a dialog for the repository and target
  directory. Cloning into an existing directory creates a subdirectory
  named after the repository. It runs in the background, then opens the
  status page.
* **Recent Git repositories** — the last ten repositories you worked with.

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
* **Suggest message** fills in a description of the selected changes, for
  example `Update paper.tm: Introduction, Proofs`. Each TeXmacs document
  lists the sections that contain changes, found by comparing it
  structurally with the last commit.
* Staging a file larger than 10 MB (preference `git large file size`)
  asks for confirmation.
* With signing enabled, commits and tags are signed with GnuPG. The commit
  page shows whether a commit's signature is good.

### 3a. Simple mode (snapshots)

For coauthors who don't want to deal with staging, **Preferences → Simple
mode** reduces the menus to:

* **Save snapshot…** — records the state of all files, after a short
  description.
* **Restore snapshot →** — the last 15 snapshots. Restoring one puts back
  all files as they were, as a new change, so the history is kept.
* **Restore version →**, **Compare with**, **Who changed what** — for the
  current document.
* **Synchronize** — pulls the others' changes (merging them if needed),
  then pushes yours.
* **Status**, **Git panel**, **History**, **Git output**.

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

(See also `git-blame`, `git-describe-changes`, `git-document-dependencies`
and the snapshot functions in `git-blame.scm` and `git-project.scm`.)

| API | Where | Purpose |
|-----|-------|---------|
| `(async-evaluate-system argv input callback)` → id | C++ glue | Runs a command without a shell in the background; `callback` receives `(code stdout stderr)`. |
| `(async-evaluate-cancel id)` | C++ glue | Terminates such a command and its process group. |
| `(merge-versions base ours theirs)`, `(merge-conflicts t)` | `version-merge.scm` | Structured 3-way merge of strees. |
| `(version-match l1 l2)` | `version-merge.scm` | Longest common subsequence of two lists, as a vector of matching indices. |
| `(version-normalize t)`, `(version-denormalize t)` | `version-compare.scm` | Public wrappers around the comparison helpers. |
| `(version-notify-saved name)` | `tm-files.scm` | Hook called after a document is saved. |
| `git-run`, `git-run-async`, `git-status`, `git-file-state`, `git-log`, … | `git-base.scm` | The git process layer and parsers (see git-implementation.md). |

Revision names understood by `version-revision` for git: a commit hash,
any git revision (`HEAD`, a branch), `INDEX`, and `BASE`, `OURS` and
`THEIRS` for the stages of a conflicted file.

---

## 9. Tests

```sh
doc/tests/run-git-tests.sh <scratch-dir>         # headless: 81 checks
doc/tests/run-git-tests.sh --gui <scratch-dir>   # Qt offscreen: 39 checks
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
* Blame follows the file's linear history (`git log --follow`), not the
  full commit graph, so a paragraph brought in by a merge is attributed to
  the merge commit.
* Snapshots use `git restore`, which needs git 2.23 or later.
* The simple mode hides staging, but it doesn't hide merges: conflicts
  still have to be resolved.
* Opening a document inside a repository runs `git status`, which may run
  that repository's `core.fsmonitor` hook, as with any git GUI.
* There is no textconv diff driver for `git diff` of `.tm` files on the
  command line.
* Windows and Android run background commands synchronously; only the
  callback is delayed.
