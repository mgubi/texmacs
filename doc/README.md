# Developer notes: git versioning branch (`wip-git-versioning`)

Working notes for improving TeXmacs's git support. These files are for
developers, not the user manual (the manual lives in
`src/TeXmacs/doc/main/editing/man-versioning.*.tm` and, for Git, the
chapter `man-git*.en.tm`).

| File | Contents |
|------|----------|
| [git-features.md](git-features.md) | **What the branch adds**: the features by menu, examples of structured merges, safety properties, new APIs, tests and known limitations. Start here. |
| [git-ui-design.md](git-ui-design.md) | **Proposed UI improvements**: audit of the current UI, design principles, mockups for the footer indicator, panel, menus, dialogs, guided conflict resolution, pages and preferences, with priorities. |
| [git-audit.md](git-audit.md) | **Audit of the branch** (2026-09-24): security, data loss, robustness, performance, UI, docs and tests, with status per finding. |
| [git-audit-2.md](git-audit-2.md) | **Second audit** (2026-10-05), after the fixes, the rebase on wip_fixes and the merge into maxs_texmacs: a trust bypass through bare repositories and symlinks, the panel, the C++ process layer, S7, tests and docs. |
| [git-manual-audit.md](git-manual-audit.md) | Audit of the user manual chapter on Git (menu paths, wording, examples), with its fixes. |
| [texmacs-internals.md](texmacs-internals.md) | The TeXmacs machinery that version control depends on: Scheme modules and `tm-define` dispatch, the `tmfs://` virtual file system, buffers and the save pipeline, menus, widgets and side tools, running external processes, the document comparison engine. |
| [vcs-current-state.md](vcs-current-state.md) | How the current SVN and Git support works, file by file, plus a review of the WIP commit `0b8b565da6` and a list of known bugs. |
| [git-plan.md](git-plan.md) | The plan for full git support, in phases. |

Paths are relative to the repository root. Scheme code is under
`src/TeXmacs/progs/`, C++ under `src/src/`.

**Branch state.** Since 2026-10-04 `wip-git-versioning` is based on
`wip_fixes` (the upstream mirror `svn_sync` with fixes and the test
harness); from 2026-09-24 it was based on `svn_sync`. The original
2020 WIP commit is kept as the branch `wip-git-versioning-2020`; its intent
was re-implemented instead of rebased (see vcs-current-state.md, section 2).
The analysis of the *old* code in vcs-current-state.md describes `master`,
which has the same version-control code as `svn_sync`.

| Also here | |
|-----------|---|
| [TODO.md](TODO.md) | **Open work on maxs_texmacs**: PRs waiting, bugs found and not fixed (ports, build, docs, fonts), offers not taken up. |
| [tests/](tests/) | `run-git-tests.sh` runs the headless suites `git` and `version` of the test harness (`src/TeXmacs/progs/check/git-test.scm`, `version-test.scm`), or with `--gui` the offscreen `git-gui-test.scm`. |
| [git-implementation.md](git-implementation.md) | How the new git support is organised (modules, data formats, conventions). |
