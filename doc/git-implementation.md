# How the new git support is organised

The code lives in `src/TeXmacs/progs/version/`. This describes the state as of 2026-09-24.

## Modules

| Module | Depends on | Role |
|--------|------------|------|
| `git-base.scm` | nothing | The process layer and the parsers. It has no GUI code and doesn't touch buffers. |
| `version-tmfs.scm` | git-base | The generic VCS API. It uses `git-root` for detection (`git-active?`) and adds `version-tool-reset`. |
| `version-git.scm` | version-tmfs, version-compare, version-merge, git-base | The git backend (`version-*` overloads), the file and repository actions, the `tmfs://git/...` and `tmfs://commit/...` pages, and the `:secure` page actions. |
| `version-merge.scm` | version-compare | Structured 3-way merge of documents (`merge-versions`). |
| `git-drivers.scm` | version-merge, git-base | The git merge driver (`git-merge-driver`, loaded lazily) and its installation in a repository. |
| `git-blame.scm` | version-git, version-merge | Blame by paragraph: `git-blame`, and the `tmfs://blame/<file>` page (loaded lazily). |
| `git-project.scm` | version-git, version-merge | Files used by documents, change descriptions for commit messages, snapshots and simple mode. |
| `git-widgets.scm` | version-git, git-project | Dialogs (commit, form and message dialogs, clone, failures, preferences, first-run mode), the side panel `git-tool`, and the review bar `version-review-tool`. |
| `version-menu.scm` | git-widgets, git-project, git-blame, git-drivers | `version-menu` (with `version-trusted-menu`), `git-file-menu`, `git-project-menu`, `git-repository-menu`, `git-compare-menu`, `git-restore-menu` and `version-differences-menu`. |
| `version-kbd.scm` | git-widgets | The shortcuts, in the mode `in-git-document?`. |

Outside `version/`:
* `kernel/texmacs/tm-modes.scm` has the cached predicates
  `versioning-directory`, `git-directory?`, `git-context?` and
  `versioning-tool-active?`;
* `texmacs/texmacs/tm-server.scm` has `set-versioning-tool`;
* `init-texmacs.scm` installs the footer hook;
* `packages/miscellaneous/git-pages.ts` is the style of the generated
  pages.

`init-texmacs.scm` registers the tmfs classes lazily:
`(lazy-tmfs-handler (version version-tmfs) history revision)` and
`(lazy-tmfs-handler (version version-git) git commit)`. Restored git pages
therefore load even before anyone has opened the Version menu.

## git-base conventions

* `(git-run root arg ...)` returns `(code stdout stderr)`, where `code` is an
  integer. `(git-run-with-input root input arg ...)` also feeds `input` on
  stdin; this is how commit and tag messages are passed (`--file=-`).
  `git-output` returns stdout, or `#f` on failure. `git-message` picks the
  most informative line of the output, and `git-report` (in version-git)
  shows it in the footer.
* Every command runs as `env LC_ALL=C git -C <root> -c
  core.quotepath=off -c color.ui=false -c core.fsmonitor=false
  --literal-pathspecs ...` through `evaluate-system`: argv with no shell,
  so no quoting is needed. There is no `env` prefix on Windows. The
  executable is the preference `"git executable"`.
* **Trust:** `git-run` and `git-run-async` refuse to run in a working tree
  that is not listed in the preference `"git trusted repositories"`. The
  list is filled by `git-trust`, which is called by init, clone and
  *Use Git in this folder…*.
* The first call sets `GIT_TERMINAL_PROMPT=0` and `GIT_OPTIONAL_LOCKS=0` in
  TeXmacs's own environment (`system-setenv`), so git never waits for a
  password on a terminal that isn't there.
* The last 50 commands are kept as `(time root-string args result)` in
  `git-command-history` and shown on the "Git output" page.
* `git-root u` walks up the directories looking for a `.git` **entry**,
  which can be a file (worktrees, submodules) or a directory. It never starts
  a process.
* **Status**: `git-status root` runs `status --porcelain=v2 -z --branch`
  (respecting the configuration of untracked files) and returns an alist with `head`, `oid`,
  `upstream`, `ahead`, `behind` and `entries`. An entry is
  `(kind xy path orig)`, where kind is one of
  `ordinary renamed unmerged untracked`. The result, even a failure, is
  cached for 2 s per root (with the remotes). `git-invalidate root` drops the cache, and every action calls it
  through `git-refresh`.
* `git-file-state u` returns one of `untracked unmodified modified staged
  partial added deleted conflicted`. A file that doesn't appear in the
  status is looked up with `ls-files --error-unmatch`, and that answer is
  cached with the status.
* **Log**: the format is `%x1e%H%x1f%P%x1f%an%x1f%ad%x1f%s`, split on the
  record separator and then the unit separator. A commit is
  `(hash parents author date subject files)`. `files` is only filled for
  `git-file-log` (`--follow --name-only`), where it holds the file's path at
  that commit.
* Refs come from `for-each-ref` with `%1f`-separated fields: `(name
  current? upstream track date subject)`.
* **Encoding**: git's output is UTF-8. It is converted with `utf8->cork`
  only when it is put into a document or menu. Commit and tag messages
  passed to `git-commit-staged` and `git-create-tag` must be UTF-8.
  `version-commit` takes cork, like the SVN backend does.

## Revisions

`version-revision name rev` accepts:

* the history form `<hash>:<tmfs-file>`, which `version-revision-url` turns
  into `tmfs://revision/<hash>/<file>`;
* any git revision, such as `HEAD`, a branch name or a hash;
* `INDEX`, meaning the staged version (`git show :path`);
* `BASE`, `OURS` or `THEIRS`, meaning the index stages 1, 2 and 3 of a
  file with a merge conflict.

`git-compare-with name rev` opens `name` and runs `compare-with-older` on
`tmfs://revision/<rev>/<name>`, so the comparison is the structured
TeXmacs one.

## Safety rules

These rules come out of a review of the branch (2026-09-24):

* **Paths are never patterns.** Every command runs with
  `--literal-pathspecs`. Otherwise "discard `n[1].tm`" would also revert
  `n1.tm`.
* **Names are never options.** Revisions and the names of branches, tags
  and stashes can come from untrusted places, such as links
  `tmfs://commit/<rev>/...` or `tmfs://revision/<rev>/...` in any
  document. They are checked with `git-safe-name?` (non-empty, not
  starting with `-`, no newline or NUL) before they reach git. Without
  this, a link to `tmfs://commit/--output=<file>/<root>` could overwrite
  a file.
* **Page actions only run from git pages.** The `git-page-*` functions are
  `:secure`, so any document could contain an `action` tag calling them.
  They therefore check (`page-context?`) that the current buffer is a git
  page of the same working tree.
* **Stdin is always a pipe.** When there is input it is written to the pipe;
  otherwise the pipe is closed at once, so a command that reads stdin (such
  as `commit --file=-` with an empty message) sees end of file instead of
  hanging the UI. Empty commit messages are refused up front.
* **Renames have two paths.** Unstaging a renamed file also unstages the
  removal of the old name (`git-entry-paths`).
* **Unsaved work comes first.** Everything that rewrites files (switch,
  merge, pull, stash, stash pop) first offers to save modified documents.
  A document that changed on disk *and* has unsaved edits is never
  reloaded.
* **Merges are committed whole.** While `MERGE_HEAD` exists, "Commit this
  file" is refused. The commit dialog refuses partial selections and
  conflicts, and prefills `MERGE_MSG`.

## Pages and actions

The pages are generated documents in style `(tuple "generic" "git-pages")`,
built by `git-page`.
Clickable operations are `(action text script)` tags whose script calls a
`git-page-*` function. These are declared `(:secure #t)` and take only
string arguments (the root as a system path, and a relative path), so
clicking them doesn't trigger a script-security prompt. Destructive ones,
such as discarding changes or deleting a branch or stash, ask for
confirmation. After an action, `git-refresh` re-imports every open git page
for that root in place (`git-reload-buffer`).

Operations that rewrite files (discard, switch, merge, pull, stash) run
inside `git-with-reload`, or between `git-watch` and `git-reload` for
asynchronous commands. `git-watch` records the *contents* on disk of the open
documents under the root, because modification times only have a
resolution of one second. `git-reload` re-imports the documents whose
contents changed. A changed document that
also has unsaved edits is not reloaded; a warning is shown instead.
`git-when-saved` offers to save modified documents before switch, merge,
pull and stash.

## Side panel

`git-tool` (in `git-widgets.scm`, registered with `lazy-tool`) is a
`tm-tool*`, opened on the right with "Git panel" (`git-open-tool`). It
has a sync bar and the tabs Changes (with a commit box), History and
Branches (see git-features.md, section 1b).

Only the parts that depend on the state of the working tree (the sync
bar, the lists, the History and Branches tabs) are `refreshable
"git-tool"`; `git-refresh` calls `(refresh-now "git-tool")`. The commit
box is **not** refreshed. Rebuilding the `texmacs-input` of its message
while the user types would destroy an editor with pending updates (a
crash), and would lose the message. Its aux buffer
`tmfs://aux/git-panel-<n>` is created per window. When the window switches
to a document of another repository, `panel-follow-root` (called by the
sync bar) puts the message aside and shows the one of the new repository
with `buffer-set-body`, without rebuilding the editor.

The working tree is that of the window's buffer (`git-buffer-root`, which
also understands git pages). The bodies of `(if ...)` in widgets are
evaluated eagerly, so the git layer accepts `#f` roots.

## Commit dialog

`git-interactive-commit` opens `git-commit-widget`. The dialog has a
`texmacs-input` for the message (aux buffer `tmfs://aux/git-commit-<n>`,
converted with `cpp-texmacs->verbatim ... "utf-8"`), a `choices` list with
every changed file, and an "Amend" toggle. The files that start out
selected are the ones with staged changes. When you commit:

* a selected file without staged changes is staged entirely;
* a selected file with staged changes is committed with exactly its staged
  changes, so partial staging is kept;
* a file that is not selected is unstaged.

Then `commit --file=-` runs. If preparing the index or committing fails,
the dialog stays open.

## Asynchronous commands

`fetch`, `pull` and `push` go through `git-run-async root args input cont`,
which is built on the new C++ `async-evaluate-system argv input callback`.
The callback receives `(code stdout stderr)` from the event loop
(`async_evaluate_pending` in `sys_utils.cpp`, called by
`async_eval_pending` from the event loop in `tm_server.cpp`).

On Unix, `unix_system_start` in `Plugins/Unix/unix_sys_utils.cpp` spawns
the process with `posix_spawnp` and starts background threads that
exchange data with it through the thread-safe `_channel` and `_ts_string`
already in that file. The parent's pipe ends are marked close-on-exec.
`unix_system_finished` polls `waitpid (WNOHANG)`, joins the threads and
returns the result. On Windows and Android the command runs synchronously
and only the callback is delayed.

`async-evaluate-system` returns an identifier (0 on failure) which
`async-evaluate-cancel` accepts. The child runs in a new session
(`POSIX_SPAWN_SETSID`, or a new process group where that isn't
available), so it has no controlling terminal and prompts can't stop it.
Cancelling sends `SIGTERM` (then `SIGCONT`) to the whole group, which
includes helpers such as `ssh` or `git-remote-https`; a second cancel
sends `SIGKILL`. Nothing is sent once the process has been reaped. A command only
counts as finished when it has exited *and* its reader threads have seen
end of file. The event loop therefore never blocks in `pthread_join`,
even if a grandchild keeps the pipes open.

At most one asynchronous command runs per root (`git-busy?`, which holds
the identifier). "Git → Cancel running command" and the panel's Cancel
button call `git-cancel`. While one is
running, the menu hides Fetch, Get changes and Send changes. `git-remote` in
`version-git.scm` reports the result, reloads the documents that changed,
and refreshes the pages. Each of `git-fetch`, `git-pull` and `git-push` can
take a continuation, which the tests use.

The X11 version has no argv-based `unix_system` (it is compiled out), so
`evaluate-system` would abort there. `git-base` detects this with
`(x-gui?)` and falls back to `eval-system`: arguments are single-quoted,
stdin and stderr go through temporary files, and the exit code is appended
after a `\001` byte. Note that headless mode runs no event loop, so
callbacks never fire there.

## Merge conflicts

`version-merge.scm` provides `merge-versions base ours theirs`, a
structured 3-way merge of strees. It returns a document in which the
changes made on one side only are applied, and those made on both sides are
marked with `version-both` (old = ours, new = theirs), just as in the output
of `compare-versions`. `merge-conflicts` counts the markup that remains.

The algorithm is the classical diff3, run on lists of children:

* `lcs-match` matches base against each side. It trims the common prefix
  and suffix first, then runs dynamic programming on the middle if that has
  at most 4·10⁶ cells.
* The stable elements are those matched on both sides. The chunks between
  them are merged by `merge-chunk`: take the side that changed; if both
  changed the same way, take either; if all three chunks have the same
  length, merge element by element (a replacement); otherwise fall back to
  `compare-versions` of ours against theirs.
* `merge-versions` recurses into `document` (paragraphs), text (via
  `version-denormalize`, word by word) and any other tag with the same
  label and arity on all three sides, except `graphics`, `table` and
  `tformat`, which are compared as a whole.

`git-resolve-conflict` loads `OURS` into the buffer, so the style and
initial environment come from our side. If the file has a `BASE` stage it
replaces the body with the merge of the three bodies. Otherwise (the file
was added on both sides) it falls back to the 2-way `compare-with-newer`
on `THEIRS`. The user steps through the remaining differences and retains
a side for each, with the usual Version menu actions and shortcuts.
`git-mark-resolved` warns if any `version-*` markup is left, saves the
file, and stages it.

## Merge driver

"Git → Merge documents structurally" (`git-install-merge-driver`) does
two things:

* It sets `merge.texmacs.driver` in the repository's local config to
  `TEXMACS_PATH=... <texmacs> -headless -x '(git-merge-driver "%O" "%A"
  "%B")' > /dev/null 2>&1`. The executable is `$TEXMACS_PATH/bin/texmacs.bin`
  or, in a macOS bundle, `Contents/MacOS/TeXmacs`.
* It adds `*.tm merge=texmacs` to `.gitattributes`. Collaborators who
  haven't configured the driver get git's usual text merge.

`git-merge-driver` loads the three documents, runs `merge-versions` on
their bodies, and writes the result into `%A` (ours, keeping our
preamble). It exits with 0 if no conflict remains and 1 otherwise, so git
reports the conflict. The conflicted file then holds `version-both` markup
instead of `<<<<<<<` markers: TeXmacs can open it and resolve it directly,
and "Resolve conflict" also still works. Starting TeXmacs headless costs
about a second per merged document.

## Blame, change descriptions and projects

* **Blame** (`git-blame name body`) walks the commits of `git-file-log`
  from newest to oldest. It loads each revision's body, splits it into
  paragraphs, and matches it against the next newer version with
  `version-match`, an LCS over paragraph strees. Each current paragraph is
  followed back until it no longer occurs, and the commit just before
  that point gets the credit. A paragraph that doesn't occur in HEAD is
  "not committed yet". If the depth limit cuts the history off, the
  oldest commit examined is marked "or earlier".
* **Change descriptions** (`git-describe-changes`) match the HEAD and
  working-tree bodies both ways. For each unmatched paragraph they record
  the title of the section it belongs to (`section`, `subsection`, … tags,
  flattened to text with `cpp-texmacs->verbatim`).
* **Dependencies** (`git-document-dependencies`) come from walking the
  stree for `include` (followed recursively), `image`, `bibliography`
  (with `.bib`) and `style` (local `.ts` files). The walk starts from
  `project-get` when the buffer is attached to a project.
* **Snapshots** are `add --all` followed by a commit. Restoring one is
  `git restore --source=<rev> --staged --worktree -- .` inside
  `git-with-reload`.

## Testing

`doc/tests/run-git-tests.sh [dir]` builds a repository whose path contains
spaces, adds a linked worktree, and runs `doc/tests/git-test.scm` with
`texmacs.bin -headless` and a private `TEXMACS_HOME_PATH`. It covers
detection, all file states, quoting, history and revisions, every page,
branches, tags, stashes, renames and conflicts.

`doc/tests/run-git-tests.sh --gui [dir]` runs `git-gui-test.scm` in the
Qt GUI with `QT_QPA_PLATFORM=offscreen`, so that the event loop runs but
no window appears. It creates a bare remote with two clones and a
conflicting merge, and tests push (including upstream setup), fetch and
pull, the reloading of an open document, the busy flag, and the structured
resolution of the conflict. The GUI parts (menus and
the dialog) were smoke-tested by driving the Qt app from `-x` scripts:
expanding the menus with `menu-expand` and opening the dialog. Pages can be
checked visually headlessly with `(load-buffer u) (print-to-file "x.pdf")`.

### Driving the Qt interface

`Plugins/Qt/qt_test.cpp` (and its Qt6 copy) exports a few commands to
drive the real interface from `-x` scripts, with the normal (visible)
platform. The offscreen platform does not render faithfully.

| Command | Effect |
|---------|--------|
| `(gui-test-snapshot dir)` | saves the visible windows as `dir/window-<i>.png` |
| `(gui-test-menu "Version\|Commit")` | triggers a menu entry (prefix match); lazy menus are populated first |
| `(gui-test-menu-entries "Version")` | the labels of a menu |
| `(gui-test-buttons)` | the labels of the visible buttons |
| `(gui-test-click "Yes")` | clicks a button or tab, in the active window first |
| `(gui-test-type "text")` | types into the focused widget |
| `(gui-test-click-later ms dir label)` | answers a modal dialog: Qt timers still fire in its event loop, the delayed commands of TeXmacs don't |

Labels are compared case-insensitively, with `...` for `<ldots>`. A
walkthrough runs its steps with `(delayed (:pause 1500) ...)`, and uses a
fresh `TEXMACS_HOME_PATH` initialised by a first headless run, so that no
Welcome window steals the focus. On macOS, other processes cannot capture
the TeXmacs windows, so snapshots must be taken from inside.

## Known gaps

* The textconv diff driver for readable `git diff` of `.tm` files is still
  to do.
* An asynchronous command only finishes once its output pipes are closed.
  If a helper keeps them open (for example an `ssh` master started without
  `ControlPersist` detaching), the command stays "running" until it is
  cancelled.
* Plugin pipe links reap children with `wait (NULL)` (`pipe_link.cpp`),
  which can collect a git process. `waitpid` then fails and the command
  is reported with exit code -1, even if it succeeded.
* The trust model protects against a repository's configuration only
  until you trust it. After that, its hooks and filters run as with any
  git client.
* The commit dialog, the panel and the menus are exercised by the offscreen
  tests (built and opened, menus expanded), but have not been used by
  hand.
* A focus crash showed that two dialogs must never share an aux buffer:
  each commit dialog gets its own `tmfs://aux/git-commit-<n>`.
* The panel follows the current document (TeXmacs rebuilds side tools
  when the buffer changes), and is refreshed after git actions and saves,
  through the `version-notify-saved` hook called by `save-buffer-post` in
  `tm-files.scm`. Its look has not been checked visually, since
  the tests run offscreen.
