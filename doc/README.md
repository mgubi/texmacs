# Developer notes: git versioning branch (`wip-git-versioning`)

Working notes for improving TeXmacs's git support. These files are for
developers, not the user manual (the manual lives in
`src/TeXmacs/doc/main/editing/man-versioning.*.tm`).

| File | Contents |
|------|----------|
| [texmacs-internals.md](texmacs-internals.md) | The TeXmacs machinery that version control depends on: Scheme modules and `tm-define` dispatch, the `tmfs://` virtual file system, buffers and the save pipeline, menus, widgets and side tools, running external processes, the document comparison engine. |
| [vcs-current-state.md](vcs-current-state.md) | How the current SVN and Git support works, file by file, plus a review of the WIP commit `0b8b565da6` and a list of known bugs. |
| [git-plan.md](git-plan.md) | The plan for full git support, in phases. |

Paths are relative to the repository root. Scheme code is under
`src/TeXmacs/progs/`, C++ under `src/src/`.

**Branch state.** Since 2026-09-24 `wip-git-versioning` is based on
`svn_sync` (the upstream mirror, 907 commits past `master`). The original
2020 WIP commit is kept as the branch `wip-git-versioning-2020`; its intent
was re-implemented instead of rebased (see vcs-current-state.md, section 2).
The analysis of the *old* code in vcs-current-state.md describes `master`,
which has the same version-control code as `svn_sync`.

| Also here | |
|-----------|---|
| [tests/](tests/) | `run-git-tests.sh` builds a scratch repository (spaces in paths, a linked worktree) and runs `git-test.scm` in headless TeXmacs. |
| [git-implementation.md](git-implementation.md) | How the new git support is organised (modules, data formats, conventions). |
