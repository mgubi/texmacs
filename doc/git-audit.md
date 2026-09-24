# Audit of the `wip-git-versioning` branch (2026-09-24)

Three independent reviews of `svn_sync..HEAD` (at `f563bb847a`):

1. core logic, security and C++;
2. user interface and widgets;
3. documentation, tests and consistency.

**Verified** means reproduced (in a scratch repository, or by checking
the code path by hand). The other findings come from careful reading of
the code.

Every finding is **open** until a fix commit refers to it.

---

## A. Security and data loss (fix first)

| # | Finding | Where | Status |
|---|---------|-------|--------|
| A1 | **Git runs code from untrusted repositories without any user action.** With the new default versioning tool *Automatic*, the footer runs `git status` at idle for any document under a directory with a `.git`, e.g. an unpacked archive or a shared folder. Commands in that repository's `.git/config` then run, such as `core.fsmonitor` or `filter.*.clean` for racy files. Following a `tmfs://git/status/<dir>` link in a document does the same. **Verified:** a `core.fsmonitor` command runs with the exact arguments of `git-arguments`; `-c core.fsmonitor=false` prevents it. | `git-base.scm` (`git-arguments`, footer), `init-texmacs.scm`, `tm-server.scm` | fixed (cf39377e88) |
| A2 | **Restore snapshot destroys uncommitted work.** `git restore --source=rev --staged --worktree -- .` discards uncommitted edits and deletes staged new files. The confirmation only *suggests* saving a snapshot first. **Verified.** | `git-project.scm` | fixed (next commit) |
| A3 | **Restore version overwrites the staged version** (`git checkout rev -- path` writes the index), so "Discard" cannot undo it. After a rename, it restores the *old* path. **Verified.** | `version-git.scm` (`git-restore-revision-now`), `version-menu.scm` | fixed (next commit) |
| A4 | **The panel's commit message is wiped** on every refresh of the panel: after each Stage/Unstage click, and after saving any file of the repository. The commit box is created with an empty document inside the refreshable, and `texmacs-input` resets the existing aux buffer to it. **Verified in code.** | `git-widgets.scm` (commit box) | fixed (next commit) |
| A5 | **Commits ignore unsaved edits.** "Commit this file…" is offered because the buffer is modified, but `git-commit-file` and the commit dialog commit only what is on disk, typically with the result "nothing to commit". | `version-git.scm`, `git-widgets.scm` | fixed (next commit) |
| A6 | **The merge driver fails without conflict markers.** On a failure (add/add with an empty base, a moved installation, a non-TeXmacs file matching the attribute), the driver exits 1 and leaves "ours" without any marker. A user who resolves outside TeXmacs may then drop "theirs". | `git-drivers.scm` | fixed (next commit) |

## B. Robustness and correctness

| # | Finding | Where |
|---|---------|-------|
| B1 | *Edit → Preferences → Versioning tool* calls `set-versioning-tool`, which is defined only in the lazily loaded `tools-menu.scm`, so it is unbound until the Tools menu has been opened. **Verified.** | `preferences-menu.scm`, `tools-menu.scm` |
| B2 | "Compare with → Branch feature/x": branch names containing `/` break the `tmfs://revision/…` URL. Resolve to a hash first, as the tag entries already do. | `version-menu.scm` |
| B3 | Async git can hang forever when TeXmacs was started from a terminal. The spawned process is in a background process group, so an ssh prompt on `/dev/tty` stops it with SIGTTIN. Cancel then sends only SIGTERM, which stays pending on a stopped process. A grandchild keeping the pipes open has the same effect. Fix: `POSIX_SPAWN_SETSID`, SIGCONT after SIGTERM, SIGKILL on a second cancel. | `unix_sys_utils.cpp` |
| B4 | The footer retries forever: if git is missing or `status` fails, a new `git status` is scheduled at every idle period. | `git-base.scm` |
| B5 | Rebase conflicts are not detected: only `MERGE_HEAD` is checked. `diverged?` matches English messages, so it fails with a localized git. `git-push-remote` breaks on remote names containing `/`. | `version-git.scm`, `git-base.scm` |
| B6 | `versioning-directory` and `git-root` call `url-exists?` on web URLs, possibly fetching from the network for each parent directory. | `tm-modes.scm`, `git-base.scm` |
| B7 | SVN regression: "Restore this version" is shown for SVN revisions and does nothing. | `version-menu.scm`, `version-git.scm` |
| B8 | Non-ASCII paths: the clone dialog's default directory is not converted to cork (and is then converted back, mangling it); several titles and messages show raw utf8. The compare-menu truncation may split multibyte characters. | `git-widgets.scm`, `version-git.scm`, `version-menu.scm` |
| B9 | The diff bars on the commit page are not scaled (the maximum is always 40), so a 5000-line change prints 5000 "+". **Verified.** | `version-git.scm` |
| B10 | The snapshot message in the panel is cleared even if the snapshot is not taken (cancelled save confirmation). The failure dialog reads "Committed changes failed", and its *Details* button does nothing outside the repository. The commit-file dialog always closes, even on failure, and does not restore the "git sign" preference after an error. | `git-widgets.scm` |
| B11 | The first-run mode dialog is triggered only by the panel. "git mode chosen" is set before the user answers; changing the mode in the preferences doesn't set it. | `git-widgets.scm` |
| B12 | `drd-props` in `git-pages.ts` only applies to `git-button`; one call is needed per macro. | `git-pages.ts` |
| B13 | C++ details: EINTR is taken for end of file in `_background_read_task`, and a SIGKILL to `-pid` after reaping may hit a reused pid. | `unix_sys_utils.cpp` |

## C. Performance

| # | Finding | Where |
|---|---------|-------|
| C1 | `--untracked-files=all` overrides `status.showUntrackedFiles=no`. With a dotfiles repository in `$HOME`, every document under the home directory triggers a full scan of it, synchronously, at idle, on saves and on menu opening. | `git-base.scm` |
| C2 | Every save regenerates every open git page (log, graph with 250 commits, branches) and the panel. | `version-git.scm` (`git-refresh`) |
| C3 | Menus: `git remote` runs uncached on every opening of the Version menu. The Project submenu re-parses all included documents; the restore menu runs `log --follow`. | `version-menu.scm` |

## D. User interface

| # | Finding |
|---|---------|
| D1 | People comparing two plain files lose top-level access to First/Next difference, Show and Retain, which moved into *Differences*. |
| D2 | Shortcuts `version s/g/c` do nothing on the git pages themselves (the mode requires a real file); `version y` without a remote gives a generic failure. |
| D3 | Review bar: the "i of n" count doesn't follow keyboard navigation; "Keep" does nothing outside a difference; "Mark as resolved" refreshes before its confirmation is answered. |
| D4 | Panel history: clicking a commit opens the whole log instead of that commit. Compare/Restore fail for versions before a rename (the restore menu too). |
| D5 | Inconsistent words: History/Log/Full history; Output/Git output/Details; Who changed what/Blame; Resolved/Mark as resolved; to send/ahead/↑; (detached)/(no branch); "3 to send 2 to get" without a separator; staging vocabulary on the status page in simple mode. |

## E. Documentation and tests

| # | Finding |
|---|---------|
| E1 | **The test runner always exits 0**, even on FAIL, TEST-ERROR, a timeout or a crash. |
| E2 | Checks that cannot fail (`(check "…" #t)` for the dialogs), and weak checks: "invalid branch refused", "worktree root", "mode dialog", menu checks that only test `pair?`. |
| E3 | Flaky or leaky tests: one commit relies on the global git identity; "cancelled quickly" is timing-based; `$dir/home` is reused across runs; `/tmp/git-test-owned` is written outside the scratch directory; `git init -b` needs git ≥ 2.28. |
| E4 | Untested: the commit-dialog logic; `git-commit-file`; synchronize; the pull modes; push-to/remove-remote/delete-branch/stash-drop; the merge driver on paths with spaces; the X11 fallback; the footer states; the shortcuts; the effects of the preferences; reloading documents with unsaved edits; one-sided deletions; non-ASCII paths; submodules. |
| E5 | git-features.md and git-implementation.md are out of date after the UI commits: the menu structure, the old panel, the Pull/Push labels, the page style, the aux buffers, the module table, the "14 commits" and the GUI check counts (actually 52), "clone in the footer". Undocumented limitations: cancel on Windows/Android, synchronous "background" commands on X11, the POSIX-shell requirement of the merge driver, the shortcuts requiring the versioning tool. |
| E6 | The user manual still uses the old menu paths: Tools→Versioning tool (now a submenu), Version→Move/Show/Retain/Grain (now under Differences), Version→History (now "History of this document"), and the file entries (now under This file / Project). The git submenu has a dynamic label, and the new UI (top entries, panel, preferences, footer, review bar, shortcuts) is missing. |
| E7 | git-plan.md refers to a `doc/testing.md` that doesn't exist and to a `git/` layout that was never used, and its progress log stops before the UI work. |

---

## Recommended order

1. **A1–A6** (security and data loss), with tests: repository trust and
   `core.fsmonitor=false`, safety snapshot before restoring, `git
   restore --worktree`, a commit box that survives refreshes, save before
   committing, and a textual fallback in the merge driver.
2. **E1**: make the runner fail properly, so that every later fix is
   checked.
3. **B1–B13, C1–C3**.
4. **D1–D5**, then **E2–E7**.
