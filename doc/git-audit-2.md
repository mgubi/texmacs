# Second audit of the `wip-git-versioning` branch (2026-10-05)

Four independent reviews of `origin/wip_fixes..e39dc3086b`, after the fixes
of the first audit ([git-audit.md](git-audit.md)), the rebase on `wip_fixes`,
the move of the tests into the test harness, and the merge into
`maxs_texmacs` (S7, Vue GUI):

1. core logic, security and data loss (the review stopped early; its
   scripts were re-run and its findings re-verified by hand, see the note
   at the end);
2. the C++ process layer, the glue, the Qt test harness and portability;
3. the user interface, menus, panel and shortcuts, under Guile/Qt and S7;
4. documentation, user manual and tests.

**Verified** means reproduced (scratch repositories, a scratch
`TEXMACS_HOME_PATH`, the global Git configuration ignored). The other
findings come from reading the code. Every finding is **open** until a fix
commit refers to it.

---

## A. Security and data loss (fix first)

| # | Finding | Where |
|---|---------|-------|
| A1 | **Opening a document can run a program named in the configuration of an untrusted repository.** `git-trusted?` checks the trust of `git-root`, which walks up the path *as written* looking for a `.git` entry, while Git (run with `-C root`) finds the repository by itself. They disagree in two cases: (a) a **bare repository** has no `.git` entry, so `git-root` is `#f` and the path counts as "outside any working tree", hence trusted; (b) a **symlink** inside a trusted working tree that points into an untrusted repository: `git-root` stops at the trusted tree, Git uses the other one. The tmfs handlers (`tmfs://commit/<rev>/<dir>`, also `git`, `revision`, `blame`) take the root straight from the URL, and a document may `<include>` such a URL, so typesetting it runs Git. **Verified:** a document including `tmfs://commit/<signed commit>/<bare repo>` makes Git verify the signature with the repository's `gpg.program`, a marker script, when it is merely opened and printed headlessly; the same for a `tmfs://commit` link through a symlink out of a trusted tree. | `git-base.scm` (`git-root`, `git-trusted?`), `version-git.scm` (tmfs handlers) |
| A2 | **A failed save marks the document as saved.** `git-when-saved` (and the save-first paths of `git-commit-file*`, `git-mark-resolved-now`) call `buffer-pretend-saved` after `buffer-save` whatever its outcome. With a read-only file (or a full disk), the edits stay only in memory but the buffer is unmodified, so closing it later loses them without a prompt, and Git operates on the old file. **Verified** (read-only file, then a branch switch). | `version-git.scm`, `git-project.scm` |

Suggested fixes: (A1) canonicalize the path (`realpath`) before `git-root`,
refuse every command but `init`/`clone` when there is no root, and run Git
with an explicit `--git-dir=<root>/.git --work-tree=<root>` (or
`GIT_CEILING_DIRECTORIES`), so that Git cannot pick another repository; do
not let the tmfs handlers run Git for a root which is not a trusted working
tree. (A2) check the result of the save (or `buffer-modified?` afterwards)
and stop.

## B. Robustness and correctness

| # | Finding | Where |
|---|---------|-------|
| B1 | **Cancel does nothing once Git has exited while a grandchild holds the pipes** (first audit B3, incomplete). A hook or an ssh master keeps stdout/stderr open; Git is reaped, `exited` is set, and cancel returns early; the repository stays "busy" until the grandchild exits. **Verified:** `sh -c "sleep 12 & echo hi"` cancelled twice, callback after 12 s. Fix: detect the exit with `waitid(..., WNOWAIT)` and reap after the reader threads; on cancel, kill the process group and stop waiting for EOF. | `unix_sys_utils.cpp` |
| B2 | **Synchronous Git commands which run hooks can freeze TeXmacs.** `commit` and `merge` go through `unix_system`, which joins the reader threads (blocked by a background child of a hook) and spawns without `setsid` (SIGTTIN on `/dev/tty` when started from a terminal). **Verified** (`sleep 4 &` blocks 4 s). Fix: run hook-running commands asynchronously, or add `POSIX_SPAWN_SETSID` and a timeout after the child exits. | `unix_sys_utils.cpp`, `version-git.scm` |
| B3 | **The WebAssembly build will not link** once this branch is merged into `wip_wasm_vue`: `unix_system_start` calls `posix_spawnp`, which Emscripten lacks (the other branches guard it with `__EMSCRIPTEN__`). **Verified** with emcc. Fix: return `NULL` under `__EMSCRIPTEN__`. | `unix_sys_utils.cpp` |
| B4 | Children inherit TeXmacs's descriptors (no close-on-exec): pipes, the Scheme file being loaded, server sockets. A long-lived `git-credential-cache--daemon` can keep them open. **Verified** with `lsof`. Fix: `POSIX_SPAWN_CLOEXEC_DEFAULT` (macOS), `addclosefrom_np` (glibc). | `unix_sys_utils.cpp` |
| B5 | `wait(NULL)` in the non-Qt plugin links (Vue, SDL, X11) can reap an async Git child, whose success is then reported as exit code -1. Fix: `waitpid(pid, ...)`. | `pipe_link.cpp`, `cmdline_link.cpp` (pre-existing) |
| B6 | Remaining C++ details (first audit B13): `write` returning EINTR truncates stdin; `volatile bool finished` instead of an atomic; the channel status is not checked on the async path. On Windows, "asynchronous" commands are synchronous and cannot be cancelled (undocumented). | `unix_sys_utils.cpp`, `sys_utils.cpp` |
| B7 | **The structured merge reports independent edits of different table cells as conflicts** (tables, graphics and `tformat` are excluded from the child-by-child merge). **Verified:** one cell edited on each side gives 2 conflicts instead of 0. No data is lost (both versions are kept). | `version-merge.scm` (`same-shape?`) |
| B8 | With Git not installed (or a wrong *Git executable*), the whole Git user interface is still shown, and every file looks "untracked". **Verified.** Fix: require `git-available?` in `current-git-root` and `git-document?`, and show a single "Git not found" entry. | `version-menu.scm` |
| B9 | The shortcuts skip the checks of the menus. `version =` in an untrusted repository compares with an empty document (everything shows as new), and on a Git page or a `.txt` file gives load errors; `version c` there opens an empty commit dialog. **Verified** (`version =`). | `version-kbd.scm` |
| B10 | Links on the Git pages break with non-ASCII paths: the targets are utf8, but link navigation converts them with `cork->utf8`. **Verified** (a file `résumé.tm`, a repository under `dépôt/`). Fix: `utf8->cork` the targets, or use `git-action`. | `version-git.scm` (pages) |
| B11 | Raw utf8 remains in several messages and confirmations (first audit B8): init, large file, delete branch, remove remote, unknown revision, clone, "Modified on disk", and the labels of `version-compare-menu`. `git-short-message` and `plain-text` truncate by bytes and can split a character (**verified**: a stray "Ã"). | `git-widgets.scm`, `version-git.scm`, `git-project.scm` |
| B12 | The versioning directory cache is never reset for changes made outside TeXmacs: after `git init` in a terminal, visited folders stay "not versioned" in the automatic mode, and the only Refresh entry is in the hidden Git menu. | `tm-modes.scm` |

## C. Performance and the panel

| # | Finding | Where |
|---|---------|-------|
| C1 | **With the Git panel open, every keystroke runs `git log --follow` and `git for-each-ref`** (and `status` every 2 s), synchronously: the side tools are re-expanded after each edit and widget bodies are evaluated eagerly. **Verified:** typing 4 characters ran `for-each-ref log` four times. Fix: the panel reads cached data only, filled on refresh. | `git-widgets.scm` (`git-tool-*`) |
| C2 | **The panel is not refreshed after its own actions and goes stale when switching documents.** The contents of its `refreshable`s are evaluated once, so `refresh-now "git-tool"` rebuilds the same widgets. After staging, saving, or switching to a document of another repository, the panel keeps the old list; its Commit button then acts on the repository of the current window, not the one shown, and the History tab restores a version of the previous document (the confirmation doesn't name the file). **Verified** offscreen. git-implementation.md claims the contrary. | `git-widgets.scm`, `version-git.scm` (`git-refresh`) |

## D. User interface details

| # | Finding |
|---|---------|
| D1 | "Add 0 missing files" is always shown (greyed) in the Project submenu (`when` instead of `assuming`). **Verified.** |
| D2 | The *Git executable* choice lists `git` twice. **Verified.** |
| D3 | First audit B11, incomplete: simple mode is off by default, against the design; the mode question is asked only by the panel; the menu's *Simple mode* toggle doesn't set "git mode chosen", so the panel asks again. |
| D4 | First audit D5, incomplete: Mark resolved / Mark as resolved / mark resolved; to send–to get / ahead–behind / arrows; History / Log / Full history; staging words on the status page in simple mode. |
| D5 | The snapshot, branch, tag and remote dialogs close even when Git fails, losing the typed text; `commit-error` and `form-error` are shared by dialogs open at the same time. |
| D6 | On macOS the `version` prefix is `M-C-#` (Cmd-Ctrl-Shift-3, the system's screenshot shortcut), so the new shortcuts are unreachable there. Not new, but they inherit it. |
| D7 | *Tools → Versioning tool* calls `url-exists?` on the current buffer, possibly a web document. |
| D8 | The Qt test harness (`gui-test-*`) is compiled into every Qt build; `qt_test.cpp` leaks the `as_charp` buffer on each call. Not reachable from documents (not `:secure`). |

## E. Tests

| # | Finding |
|---|---------|
| E1 | **The git group of the suite `version` depends on the global Git configuration.** **Verified:** `commit.gpgsign=true` gives 6 failures and aborts the group; `status.showUntrackedFiles=no` gives 4. Fix: set `GIT_CONFIG_GLOBAL`/`GIT_CONFIG_NOSYSTEM` as `git-test.scm` does. |
| E2 | **The offscreen GUI test and its setup use the global Git configuration.** **Verified:** with `commit.gpgsign=true`, 21 failures. Fix: export both variables in `run-git-tests.sh`. |
| E3 | The suites write preferences: `version` adds its repository to "git trusted repositories" at every run (the list grows in the reused harness home); `git-test.scm` restores with `set-preference`, leaving explicit entries equal to the defaults. Fix: restore the trusted list; `reset-preference` for keys without an entry. |
| E4 | `GIT_CONFIG_GLOBAL` is set for the rest of the process (all later suites of `check.sh all`), and needs Git ≥ 2.32 (older versions read the user's configuration, against the comment). |
| E5 | The trust check has no positive control: "fsmonitor never run" would pass on a Git which doesn't call the hook anyway. Run `git status` once without the override and check that the marker appears. |
| E6 | First audit E2, incomplete: six GUI menu checks only test `pair?`; about 11 dialog openers have no check (only the log grep); "simple file menu" doesn't check that *This file* is hidden. |
| E7 | The GUI test is one callback chain (one failure cascades, an error stalls until the 300 s alarm); "cancelled quickly" is timing-based; the groups of `git-test.scm` depend on each other's state. |
| E8 | `run-git-tests.sh` deletes `home`, `remote`, `conflict*` inside whatever directory it is given, never removes its `mktemp` directory, and has an unused variable `opts`. |

**Untested** (55 `tm-define`s of `version/*.scm` are never mentioned by a
test): synchronize, the pull modes and the "diverged" path, push-to;
delete branch, drop stash, remove remote, stage all and the destructive
page actions with their confirmations; the commit dialog logic and the
panel commit; the interactive wrappers (`git-discard`,
`git-restore-revision`, `git-restore-snapshot`); `git-when-saved` and
`git-with-reload` with modified buffers; simple mode; the shortcuts and
`in-git-document?`; `git-interactive-trust`/`git-interactive-init`; good
signatures on commit pages; the X11 fallback, Windows, submodules,
non-ASCII paths (they work in a spot check), the review bar.

## F. Documentation

| # | Finding |
|---|---------|
| F1 | git-features.md §9 attributes to the headless suite what only the GUI test covers (push/fetch/pull/clone/cancel, reloading, save hook, panel), and the "fresh home" and "Scheme errors in the log" claims hold only for `run-git-tests.sh --gui`. |
| F2 | Out of date: README.md and git-features.md say the base is `svn_sync` (it is `wip_fixes`, 35 commits), the manual is said to be only in `man-versioning`, README doesn't list git-manual-audit.md, git-implementation.md is "as of 2026-09-24"; the progress log lacks the manual chapter and the Qt test harness. |
| F3 | The commit dialog's initial selection is described incompletely in git-features.md and git-implementation.md (when nothing is staged, all tracked changes are selected; the manual is right). |
| F4 | git-features.md §7 says non-ASCII paths and submodules "work", while §9 lists them as untested. Small slips: *Discard* is shown for `modified` or `partial`; the history list is `git-history`, `git-command-history` only returns it; the `gui-test-*` commands are documented but no committed test uses them. |
| F5 | Manual: *Version → Git → Refresh* and *Version → Git → Log* don't exist in the simple mode (*History*, and the status page's Refresh button); the shortcuts also need a versioning tool other than *Never*. The chapter is English only. |

---

## Checked and found correct

* **S7 (maxs_texmacs):** every Git menu, in nine situations, expands to
  the same entries as under Guile; all widgets expand; page actions with
  non-ASCII paths work; no procedure used by the Git modules is
  Guile-only. The headless suites pass there. (`version-tool` in
  maxs_texmacs requires `git` in `PATH`, ignoring the *Git executable*
  preference.)
* **Glue:** regenerating `glue_basic.cpp` gives the committed file.
* **Process layer:** 5 MB of output and 3 MB of input without deadlock; a
  child exiting without reading its input; unknown commands; errors in one
  callback; asynchronous completion in the Vue, SDL and NS GUIs (polling,
  up to about 1 s latency).
* **Shell fallback (X11):** arguments with quotes, `$(...)`, backquotes and
  newlines are passed literally.
* **Status parsing:** names with spaces, leading spaces, newlines, accents,
  renames and untracked directories.
* **Structured merge:** deletion against edit, insertions at the same
  place, word-level merges, duplicates and changed tags behave as
  intended (apart from B7).
* **Manual:** Cork encoding clean, all links resolve, reachable from the
  help, every page loads and prints, every menu path and shortcut exists
  (apart from F5).
* **First audit B1** (`set-versioning-tool` unbound) is fixed.

## Prior findings still incomplete

B3 and B13 (see B1, B2, B6), B8 (B11), B11 (D3), C2 (C1: the pages are
fixed, the panel has the same cost), D2 (B9), D5 (D4), E2 (E6), and the
documented A4 behaviour of the panel (C2).

## Recommended order

1. **A1, A2**, with tests (a bare repository, a symlink out of a trusted
   tree, a document including a tmfs link; a read-only file).
2. **E1, E2, E3**, so that the tests are reliable on any machine.
3. **C1, C2** (the panel), **B1, B2**, then **B3** before merging into
   `wip_wasm_vue`.
4. **B4–B12**, then **D**, **E4–E8** and **F**.

---

*Note on the core review.* It stopped on an API error before its report
was complete. Its scripts were re-run: the trust bypass (A1, both
variants, and the inclusion in a document) and A2 were reproduced again by
hand; the merge, shell-quoting and parsing checks are listed above. Its
scope (core logic and data loss) may deserve a second pass on the parts it
did not reach: stash and discard with open documents, caching races with
asynchronous commands, submodules.
