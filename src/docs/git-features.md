# Git support in TeXmacs: features of the `wip_git` branch

This page lists what the branch adds to TeXmacs. It is the changelog for
reviewers and testers. For how the code is organised, see
[git-implementation.md](git-implementation.md); for the plan and its
history, see [git-plan.md](git-plan.md).

**Base:** `wip_fixes` (the TeXmacs 2.1.5 upstream mirror `svn_sync` with
fixes and the test harness), about 40 commits ahead as of 2026-10-05.
**Scope:**
* `src/TeXmacs/progs/version/` (Scheme) and the style package
  `packages/miscellaneous/git-pages.ts`;
* small C++ additions in `src/src/System/Misc/sys_utils.cpp` and
  `src/src/Plugins/Unix/unix_sys_utils.cpp`, with their glue;
* the versioning tool's automatic mode (`kernel/texmacs/tm-modes.scm`,
  `texmacs/texmacs/tm-server.scm`, the Tools and Preferences menus);
* the footer hook and lazy handlers in `init-texmacs.scm`, and a save
  hook in `tm-files.scm`;
* the user manual: the chapter *Working with Git* (`man-git*.en.tm` in
  `doc/main/editing`), linked from `man-versioning.en.tm`.

Everything below is in the **Version** menu. By default (Tools →
Versioning tool → Automatic) it appears for documents in a git or SVN
working tree.

**Trust.** The configuration of a repository can make git run programs,
even for `git status`. TeXmacs therefore only runs git in repositories
that you trust:
* those created or cloned from TeXmacs;
* those you allow with **Version → Use Git in this folder…**.

For any other repository, the Version menu only offers that entry and the
plain comparison tools.

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
| Untrusted repositories | — | Git is never run until you trust the folder; `core.fsmonitor` is always disabled |

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
* **Git panel** (only its lists are rebuilt when the state changes, so a
  message being typed and the selected tab are kept), in these parts:
  * a sync bar with the branch, "N ahead, N behind", and Get/Send
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

When the current document is in a trusted git working tree, the Version
menu has, at the top, the entries for the document (Compare with, Restore
version, History of this document, Who changed what). The operations on
the file are in the **This file** submenu (hidden in simple mode); those on
the project are in the **Project** submenu. Each entry is shown only in
the states where it applies:

| Entry | When shown | Effect |
|-------|------------|--------|
| **This file → Add to repository** | the file is untracked | `git add` |
| **This file → Stage changes** | modified, or partially staged | `git add` |
| **This file → Unstage changes** | staged, partially staged, or newly added | `git reset`; a rename also unstages the removal of the old name |
| **This file → Commit this file…** | there is something to commit | Saves the document, asks for a message, then commits only this file (adding it first if needed). Refused during a merge. |
| **This file → Discard changes…** | modified or partially staged | Restores the staged version after a confirmation, which warns if unsaved edits would also be lost |
| **Compare with → Last commit / Staged version / Remote version / Before the last pull or merge / Other revision… / Tag … / Branch …** | TeXmacs documents | Opens the **structured comparison** between the document and that revision; you then accept or reject each difference |
| **Who changed what** | TeXmacs documents | **Blame by paragraph**: the document, with each paragraph (or run of paragraphs) annotated with the commit, author and date that last changed it, or "Not committed yet". It follows paragraphs, not source lines, through the last 30 commits (preference `git blame depth`). |
| **Restore version →** | tracked files | The last 15 versions of the document. Restoring one writes that version into the current file, following renames. The staged version and the history are kept, so this is a new change that can be discarded. Also available as **Restore this version** when viewing an old revision, and as "restore" on commit pages. |
| **Project → Add N missing files** | the document uses untracked files | Adds the included documents, images, bibliography files and local style files the document (or its project's master document) depends on |
| **Project → Commit project…** | TeXmacs documents | Opens the commit dialog with all changed files of the project preselected |
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
| **Status** | A page listing the branch, the upstream and ahead/behind counts, then conflicts, staged changes, unstaged changes and untracked files. Files are listed in tables with coloured badges and buttons (compare, stage, unstage, discard, add, resolve, mark resolved). The page header has buttons for Commit…, Stage all, Fetch, Get changes and Send changes, and for the other pages. |
| **Git panel** | The side panel described in section 1b. |
| **Graph** | The history of all branches as a graph, with branch and tag labels. Each commit links to its page. |
| **Log** | The commit history, 250 commits per page with a "More…" link. Each commit opens a **commit page** showing author, date, parents, the full message and a per-file table of changes with +/− bars, links to the file at that revision, and "compare with current" for TeXmacs documents. |
| **Branches and tags** | Local and remote branches with upstream tracking and last commit date; actions switch, merge into current and delete. Also the remotes with their URLs, the tags, and the stashes with pop and drop. |
| **Commit…** | The **commit dialog** (below). |
| **Stage all changes** | `git add --update` |
| **New branch…** / **Switch to branch** / **Merge branch** | Create and switch; switch; merge into the current branch. Before switching or merging, it offers to save modified documents. |
| **Tag this version…** | Annotated tag (lightweight if the message is empty). |
| **Fetch** / **Get changes (pull)** / **Send changes (push)** | Run in the background (section 4). When a pull cannot fast-forward because both sides changed, it offers to **merge the remote changes**. If that leads to conflicts, it opens the status page so you can resolve them (section 5). The first push of a branch sets its upstream. |
| **Remotes → Add remote… / Push to / Remove** | Manage the remote repositories. Send changes goes to the remote configured for the branch, or `origin`, or the only remote. |
| **Cancel running command** | Shown while a background command runs. |
| **Stash changes** / **Restore last stash** | `git stash push` / `git stash pop`, after offering to save modified documents. |
| **Merge documents structurally** | Installs the merge driver (section 6). |
| **Preferences → All preferences… / Simple mode / Sign commits and tags / Pull** | See sections 3a and 7. The pull mode is *Fast-forward only* (default: when both sides changed, it asks before merging), *Merge* or *Rebase*. |
| **Git output** | The last 50 git commands with exit code, stdout and stderr. |
| **Refresh** | Forgets cached state, including which files are versioned. |

Outside any repository, the Version menu (when shown, see Tools →
Versioning tool) offers the entries below. *Create* and *Clone* are also in
**Tools → Versioning tool**, since by default the Version menu is only
shown for documents in a working tree. *Git preferences* is always in the
Version menu, also when git is not found, so that its location can be
set.

* **Create Git repository…** — `git init` in the document's directory,
  with a default `.gitignore` for TeXmacs projects.
* **Clone Git repository…** — a dialog for the repository and target
  directory. Cloning into an existing directory creates a subdirectory
  named after the repository. It runs in the background, then opens the
  status page.
* **Recent Git repositories** — the last ten repositories you worked with.

### Commit dialog

* A message editor and a list of all changed files. The files with
  staged changes start out selected; when nothing is staged, all changed
  tracked files do.
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
* **Restore snapshot →** — the last 15 snapshots. Restoring one first
  saves the current state in an automatic snapshot, then puts back all
  files as they were, as a new change, so nothing is lost.
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
  `ssh` or `git-remote-https`. A second Cancel kills them.
* Commands run in a new session without a terminal, so an ssh prompt
  can't stop them. The ssh command fails instead.
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
   here they merge, when they do not touch (section 5a);
3. shows the remaining **real conflicts** with the usual comparison
   markup: old (red) is yours, new (green) is theirs. You step through
   them with the Version menu or the shortcuts, retaining one side each
   time (**Version → Retain → Keep my version / Keep their version**);
   the style packages are merged as a set and the initial environment
   (the settings of the Document menu) key by key; a change of the same
   setting, or of the main style, on both sides keeps git's "ours" (yours
   in a merge, the upstream one during a rebase) and counts as a
   conflict, which the message names; packages which are not plain names
   (such as macros) are kept as opaque elements of the set, and a
   document without a style part is taken as unchanged against the base;
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

### 5a. Which changes merge, and which conflict (#676)

**The principle:** a silent wrong merge is much worse than a conflict the
user resolves by hand. The merge is clean only for what is unambiguous;
everything else conflicts. (A first attempt, in PR #691, kept making more
cases clean and each audit found new silent wrong merges; its rules are not
reused. The code: `merge-chunk` and below in `version/version-merge.scm`.)

The children of a `document` or a `concat` are aligned as in `diff3`: the
elements kept by both sides are anchors, and only the *chunks* between
anchors can conflict. A chunk changed by one side only takes that side.

**In a paragraph (`concat` or string)**, the elements are words and spaces.

- Changes of the two sides **touch** when no word separates them, that is,
  when they are in the same chunk or are separated only by spaces: they
  conflict, also when one replaces a word by markup. Spaces next to markup of
  the ancestor do not count as touching: in *Let* `<math>` *be.*, the formula
  and the next word merge.
- One element with markup, changed on both sides, is merged further.
- A text directly below other markup (the string of `(theorem "...")`,
  `section`, `cell`), changed on both sides, is merged word by word only if
  both new texts are similar to the old one.

**Between blocks (`document`)**, each side changes a chunk by *hunks*: the
elements of the ancestor between two elements which this side keeps are
replaced by new ones. The hunks of both sides are compared by their places
in the ancestor:

1. *Different elements, touching*: two replacements are independent: each
   one is anchored by what the other side keeps and changes only, their order
   is forced and nothing is lost. A removal needs more, since what it relies
   on must really stay: two touching removals conflict (each side removes the
   anchor of the other, and nothing would remain), a removal conflicts when
   the two old elements are variants of each other, and when its neighbour is
   not paired with a new element of the other side. A neighbour with fewer
   than two real words can't be recognized and does not count (*A. / B. / C.*:
   the removal of B. and C. to C2. merge). Neighbours rewritten by each side
   into the same text conflict.
2. *An insertion touching a replacement* of the other side: it is placed by
   the edited element, so that the replacement must be 1-for-1 (n-for-n) and
   its element next to the insertion must be *paired* with the old one.
3. *Two insertions at the same place*: identical ones come out once;
   otherwise they conflict.
4. *The same elements replaced by both sides*, with all the hunks being
   n-for-n: the positions are merged one by one. A position where both new
   elements are paired with the old one, or which is a block with a body
   (`theorem` with its `document`: the rules apply to its paragraphs), is
   merged in turn; otherwise it is a conflict of this position alone. A new
   element which belongs to another old element shows a shift: the positions
   in between conflict together.
5. *Anything else that overlaps* (such as a removal against an edit)
   conflicts.

Two elements are **paired** when they have the same kind (the same tag, or
both text), share at least half of the *real words* of the smaller one and
more than one real word, and when no other old element nearby (also outside
the chunk) shares as many. Real words have at least 4
letters and are no stop words (`the`, `that`, `dans`...), in lower case and
without the punctuation around them. Only text counts: not the names of labels
and images, the attributes of `with`, the formats of tables. A text without
any real word (*Let x+y be.*) is compared by its short words. This also holds
for a single element, so that a paragraph with a single real word is never
paired. The words are indexed by packets of 32 old elements of the list, and a
new element is compared with the two packets near its place (at least 16
elements on each side), which makes the pairing linear.

Certainty also requires:

- **No repeated or moved paragraphs next to a touching:** when an element
  next to a hunk is also in it, the hunk could equally be one place further,
  and when a hunk brings an element of the ancestor, it is a move. Either way
  the touching hunks conflict.
- **Near-identical paragraphs are the same paragraph:** when a new element of
  a side (not an edit of an old one) is similar to new text of the other side
  in the chunk, also an edit (one side adds a paragraph, the other the same
  sentence to the next paragraph), the chunk conflicts, whatever their places,
  instead of having it twice.

A conflict is shown like before: the differences of ours and theirs
(`compare-versions`), for the elements concerned. When a chunk has a single
group of hunks, or near-identical paragraphs, this is exactly the old output
for the chunk; otherwise the certain groups of the chunk are merged and the
conflict is smaller.

| Base | Ours | Theirs | Result | Why |
|------|------|--------|--------|-----|
| A / B / C / D | A / C / D | A / B / C2 / D | A / C2 / D | removal and replacement of different blocks (#676) |
| A / T / C | A / T / C2 | A / T+label / todo / C | A / T+label / todo / C2 | two replacements, touching (#676) |
| The function is continuous. / Last. | The function is uniformly continuous. / Last. | The function is continuous. / New remark. / Last. | The function is uniformly continuous. / New remark. / Last. | an edit next to an insertion |
| The map is continuous. / Last. | The map is uniformly continuous. / Last. | The map is continuous. / New remark. / Last. | conflict | only one real word in common |
| First alpha. / Second beta. | Second beta. | First alpha. | conflict | each side removes the anchor of the other |
| P / P\' | P\' | P / We discuss another topic. | conflict | a removal, and its neighbour is rewritten |
| Alpha words. / Delta words. | Alpha words, edited. / S / Delta words. | Alpha words. / S Delta words. | conflict | S would be there twice |
| A. / B. | A. / X. / B. | A. / B2. | conflict | the same, but no real words: not certain that B2. is an edit |
| B. | B. / B2. | D. | conflict | D. may be a rewrite: where does B2. go? |
| The quick fox jumps. | The slow fox jumps. | The quick red fox jumps. | conflict | touching words, across a space |
| f is continuous on X. | f is uniformly continuous on X. | f is bounded on X. | conflict | not *f is uniformly bounded on X.* |
| The quick brown fox. | The slow brown fox. | The quick red fox. | conflict | touching words |
| The quick brown fox jumps. | The slow brown fox jumps. | The quick brown fox leaps. | The slow brown fox leaps. | a word in between |
| The map is continuous. | The map is uniformly continuous. | The set is open. | conflict | only short and stop words in common |
| Alpha beta / Gamma delta / Theta iota | Alpha beta, A / Gamma delta, A / Theta iota, A | New text / Gamma delta / Theta iota | ⟨Alpha beta, A\|New text⟩ / Gamma delta, A / Theta iota, A | n-for-n: a local conflict |
| Intro / Concl. | Intro / Thanks for coments / Concl. | Intro / Thanks for comments / Concl. revised | conflict | near-identical insertions, not twice |
| P / P | P | P2 / P | conflict | which P was removed? |
| P / Q | Q / P | P / Q2 | conflict | a move next to a change, as with Git |

**Known limits.**

- Decided by the maintainer, merging cleanly: a removal next to an edit of the
  next paragraph, a removed theorem with an edited proof, a proof added next
  to a rewritten theorem. The markup does not link a proof to its theorem, so
  that syntax can't tell what to do. (Variants of each other, or a neighbour
  rewritten, do conflict: rule 1.)
- A paragraph kept by both sides is an anchor: when each side removes a
  different paragraph around it, as in *P / K / Q* with *K / Q* and *P / K*,
  the result is *K*, as with `git merge`.
- Markup of the ancestor separates words: in *Let* `<math|x>` *be*, the formula
  changed by one side and the next word by the other merge.
- Moves are only recognized next to a change of the other side, and only for
  an unchanged copy at the top of the hunk. A paragraph moved far away, moved
  and edited, or moved into a new block, and changed, moved or removed by the
  other side, can be kept twice or stay although it was removed. `git merge`
  does the same, except that it happens to conflict when the moved copy lands
  right next to a change.
- The similarity is a heuristic: a rewrite which shares half of its real words
  with the old paragraph is merged word by word, where only changes with a word
  in between can merge. A paragraph with a single real word is never paired, so
  that an edit of it next to other changes conflicts.
- The comparison of near-identical new text is per chunk.
- Very large changes (more than 4 million elements compared by the longest
  common subsequence) keep the behaviour of the common ends only.

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
driver get git's usual text merge. The driver merges the preamble (style
packages, initial environment) like *Resolve conflict*. In a folder which
is not trusted, nothing is configured and the message says so.

---

## 7. Robustness and safety

* **Paths:** repositories whose paths contain spaces, quotes or
  non-ASCII characters work (tested), as do linked worktrees (tested).
  Submodules are detected like linked worktrees (their `.git` is a file),
  but are not tested.
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
* **Trust:** git is only run in trusted repositories, and
  `core.fsmonitor` is always disabled (see the top of this page).
* **Restoring never loses work:** restoring a snapshot first saves an
  automatic snapshot; restoring a version doesn't touch the staged
  version.
* **Commits include unsaved edits:** documents are saved before they are
  committed.
* **Merge driver fallback:** when the structured merge fails, the driver
  falls back on git's textual merge, with conflict markers.

---

## 8. New developer-level APIs

(See also `git-blame`, `git-describe-changes`, `git-document-dependencies`
and the snapshot functions in `git-blame.scm` and `git-project.scm`.)

| API | Where | Purpose |
|-----|-------|---------|
| `(async-evaluate-system argv input callback)` → id | C++ glue | Runs a command without a shell in the background; `callback` receives `(code stdout stderr)`. |
| `(async-evaluate-cancel id)` | C++ glue | Terminates such a command and its process group. |
| `(merge-versions base ours theirs)`, `(merge-conflicts t)` | `version-merge.scm` | Structured 3-way merge of strees. |
| `(merge-documents base ours theirs)` | `version-merge.scm` | 3-way merge of complete documents (body, style packages, initial environment): `(document conflicts kept)`. |
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
src/tests/scheme/check.sh git version            # headless: 400 + 224 checks
src/docs/tests/run-git-tests.sh <scratch-dir>         # the same, in a fresh home
src/docs/tests/run-git-tests.sh --gui <scratch-dir>   # Qt offscreen: 56 checks
```

The tests create scratch repositories (with spaces and accents in paths, a
linked worktree, a bare remote with clones, and conflicting merges), ignore
the global Git configuration of the user (`GIT_CONFIG_GLOBAL`, which needs
Git 2.32), put back the preferences they change, and open no windows.
`check.sh` reuses the home `src/tests/build/scheme/home` (or `TM_TEST_HOME`);
`run-git-tests.sh` makes a fresh one.

The **headless** checks are the suite `git` of the test harness
(`src/TeXmacs/progs/check/git-test.scm`, also run by `check.sh all`), and
the git group of the suite `version`, which checks the dispatch of the
`version-*` interface. They cover:

* the parsers and helpers which do not run Git;
* trust (a repository whose `core.fsmonitor` would run a program, a bare
  repository, a symbolic link into another repository, a document
  including a Git page);
* detection (also of linked worktrees) and every file state;
* upstream branches (ahead and behind), the footer and the menu label;
* quoting, names which could be taken for options, accented names;
* history (across renames) and revisions;
* every page;
* branches, tags and stashes;
* renames and conflicts;
* the merge engine (also of tables) and the merge driver;
* restoring, snapshots, blame, projects, signing;
* documents which cannot be saved, the shortcuts, the menus without Git.

The **offscreen GUI** test (`src/docs/tests/git-gui-test.scm`) covers push,
fetch, pull, clone and cancel, the reloading of open documents, the save
hook, the panel (no Git command while typing, updated after actions,
following the document), the menus and the dialogs being built, and
committing unsaved edits. Its runner also fails if a Scheme error appears
in the log, or if the chain of callbacks stops.

Not covered by the tests: the logic of the commit dialog, confirmations
that need an answer (delete branch, drop stash, remove remote), the X11
shell fallback, Windows and submodules.

---

## 10. Known limitations

* The commit dialog, panel and menus are covered by the offscreen tests
  (built, opened, menus expanded), but haven't been used by hand in a
  normal session.
* During a rebase, git's "ours" is the upstream and "theirs" your commits:
  the old (red) version of a conflict is then the upstream one, and *Mine*
  / *Keep my version* retain the new one (`git-mine-version`).
* Large changes: the matching of the paragraphs (longest common
  subsequence, `max-lcs-size` in `version-merge.scm`) is quadratic and
  limited to 4 million pairs; beyond (about 2000 by 2000 paragraphs which
  differ between the common start and end), only the common start and end
  are matched, and the middle is compared as a whole. Raising the limit
  costs memory and time quadratically.
* The style packages are merged as a set: packages which only theirs added
  go at the end, and a reordering of the packages by theirs only is not
  taken over (our order is kept).
* On X11, the background commands run synchronously, through the shell.
  On Windows and Android they run synchronously too, only the callback is
  delayed, and they can't be cancelled.
* The merge driver command assumes a POSIX shell (as git itself uses to
  run it) and `texmacs.bin` or a macOS bundle.
* The keyboard shortcuts only work while the versioning tool is active.
  Their prefix `version` is `M-C-#` on macOS (Cmd-Ctrl-Shift-3), which the
  system takes for screenshots unless that shortcut is disabled in the
  keyboard settings; the menus offer the same commands.
* A background command finishes when all its output is closed: a helper
  that keeps it open makes the command look running until you cancel it
  (which then works). A synchronous command waits at most 2 s for such a
  helper after git exits.
* Blame follows the file's linear history (`git log --follow`), not the
  full commit graph, so a paragraph brought in by a merge is attributed to
  the merge commit.
* Snapshots use `git restore`, which needs git 2.23 or later.
* The simple mode hides staging, but it doesn't hide merges: conflicts
  still have to be resolved.
* There is no textconv diff driver for `git diff` of `.tm` files on the
  command line.

