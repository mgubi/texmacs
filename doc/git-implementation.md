# How the new git support is organised

The code lives in `src/TeXmacs/progs/version/`. This describes the state as of 2026-09-24.

## Modules

| Module | Depends on | Role |
|--------|------------|------|
| `git-base.scm` | nothing | The process layer and the parsers. It has no GUI code and doesn't touch buffers. |
| `version-tmfs.scm` | git-base | The generic VCS API. It uses `git-root` for detection (`git-active?`) and adds `version-tool-reset`. |
| `version-git.scm` | version-tmfs, version-compare, git-base | The git backend (`version-*` overloads), the file and repository actions, the `tmfs://git/...` and `tmfs://commit/...` pages, and the `:secure` page actions. |
| `version-merge.scm` | version-compare | Structured 3-way merge of documents (`merge-versions`). |
| `git-widgets.scm` | version-git | The commit dialog and the interactive prompts for branch, tag and init. |
| `version-menu.scm` | git-widgets | `git-file-menu`, `git-repository-menu`, `git-compare-menu`, and the entry points in `version-menu`. |

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
* Every command runs as `git -C <root> -c core.quotepath=off -c
  color.ui=false ...` through `evaluate-system`: argv with no shell, so no
  quoting is needed. The executable is the preference `"git executable"`.
* The first call sets `GIT_TERMINAL_PROMPT=0` and `GIT_OPTIONAL_LOCKS=0` in
  TeXmacs's own environment (`system-setenv`), so git never waits for a
  password on a terminal that isn't there.
* The last 50 commands are kept as `(time root-string args result)` in
  `git-command-history` and shown on the "Git output" page.
* `git-root u` walks up the directories looking for a `.git` **entry**,
  which can be a file (worktrees, submodules) or a directory. It never starts
  a process.
* **Status**: `git-status root` runs `status --porcelain=v2 -z --branch
  --untracked-files=all` and returns an alist with `head`, `oid`,
  `upstream`, `ahead`, `behind` and `entries`. An entry is
  `(kind xy path orig)`, where kind is one of
  `ordinary renamed unmerged untracked`. The result is cached for 2 s per
  root. `git-invalidate root` drops the cache, and every action calls it
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

## Pages and actions

The pages are generated documents in style `generic`, built by `git-page`.
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

## Commit dialog

`git-interactive-commit` opens `git-commit-widget`. The dialog has a
`texmacs-input` for the message (aux buffer `tmfs://aux/git-commit`,
converted with `cpp-texmacs->verbatim ... "utf-8"`), a `choices` list with
every changed file, and an "Amend" toggle. The files that start out
selected are the ones with staged changes. When you commit, the index is
changed to hold exactly the selected files (`add --all` for the selected
ones, `reset` for the others), but only if you changed the selection. Then
`commit --file=-` runs.

## Asynchronous commands

`fetch`, `pull` and `push` go through `git-run-async root args input cont`,
which is built on the new C++ `async-evaluate-system argv input callback`.
The callback receives `(code stdout stderr)` from the event loop
(`async_eval_pending` in `tm_server.cpp`).

On Unix, `unix_system_start` in `Plugins/Unix/unix_sys_utils.cpp` spawns
the process with `posix_spawnp` and starts background threads that
exchange data with it through the thread-safe `_channel` and `_ts_string`
already in that file. The parent's pipe ends are marked close-on-exec.
`unix_system_finished` polls `waitpid (WNOHANG)`, joins the threads and
returns the result. On Windows and Android the command runs synchronously
and only the callback is delayed.

At most one asynchronous command runs per root (`git-busy?`). While one is
running, the menu hides Fetch, Pull and Push. `git-remote` in
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

## Known gaps

* The git merge and diff drivers, which would do the structured merge from
  the command line as well, are still to do.
* A running asynchronous command cannot be cancelled.
* Clone is not implemented. Init is available from the Version menu for
  documents outside any repository.
* The commit dialog has not been exercised by hand in the GUI. Its
  interaction was only smoke-tested.
* The side panel (`tm-tool*`) has not been written.
