# Audit of the user manual chapter "Working with Git" (2026-09-24)

Pages audited: `src/TeXmacs/doc/main/editing/man-git*.en.tm` (commit
2d3dc1fc5d) and the Git parts of `man-versioning.en.tm`. Every claim was
checked against the code. **Verified** means reproduced by running
TeXmacs; the others were checked by reading the code.

## A. Wrong statements (fix first)

| # | Page | Statement | Fact |
|---|------|-----------|------|
| A1 | start, settings | "If Git is not found, you may indicate where it is in Version → Git preferences" | Both *Git preferences* entries are inside `(assuming (git-available?))` (`version-menu.scm`), so they are **hidden exactly when Git is not found**. There is no other way to set `git executable` from the interface. Code fix: always show *Git preferences*. |
| A2 | start, overview | "The first time you use the Git tools, TeXmacs asks you how you want to work" | Only the first opening of the **Git panel** asks (`git-with-mode` has one caller, `git-open-tool`). Until then, the full mode is used (default of `git simple mode` is `off`). |
| A3 | snapshots | "restoring is itself recorded as a new snapshot" | `git-restore-snapshot-now` runs `git restore --staged --worktree`; nothing is committed. The restored state is only recorded by the next *Save snapshot*. (The automatic snapshot of the state *before* restoring is real.) |
| A4 | overview, snapshots, remote, branches | "Parts which were changed by only one of you are merged automatically, even inside a same paragraph", "Merge branch: TeXmacs documents are merged structurally" | Without the merge driver, `git pull` and `git merge` do Git's **line** merge. A `.tm` paragraph is usually one line, so two different edits of one paragraph make Git stop with a conflict (**verified**: the demo's "Slower fox" and "Sleepier dog" conflict). The structured merge only happens afterwards, with *Resolve conflict*, which then says "Merged automatically; check the result, then mark as resolved". The pages must describe this two-step reality, and recommend *Merge documents structurally*, which makes it automatic. |
| A5 | history | Restore version: "This is an ordinary change … you may compare the result with the last commit, or discard it" | Restoring overwrites the file on disk. **Changes saved but not committed are lost**: *Discard* only goes back to the staged version, and the confirmation doesn't warn about it. The page must warn (and the code should, like *Discard changes*). |

## B. Incomplete or imprecise

| # | Page | Issue |
|---|------|-------|
| B1 | start | "the Version menu only contains *Use Git in this folder*": it also has *Compare* and *Differences*. |
| B2 | start | Footer example: "2 changes" counts changed **and new** files, not "two files were changed". The footer also shows `↓n` (commits to get), "saved" when there are no changes, the number of conflicts and "working...". |
| B3 | start, settings | "repositories which you created or cloned with TeXmacs": correct in *start*, but *settings* says only "not created by TeXmacs". |
| B4 | start | *Synchronize* is only shown when the repository has a remote; *Project* only for TeXmacs documents; the panel shows only "Conflicts / Changes" in the simple mode. |
| B5 | snapshots | "if a server was added to it": the simple mode has no *Remotes* menu. Say how (full mode, *Remotes → Add remote*, or ask a Git user). Also say that only saved snapshots are exchanged, and that uncommitted changes to the same files make getting the changes fail ("would be overwritten"). |
| B6 | conflicts | *Resolve conflict* refuses while the document has unsaved edits ("Please save or revert the document first"). Not mentioned. |
| B7 | conflicts | "When a merge leaves conflicts, TeXmacs opens the status page": true after getting changes; after *Merge branch*, the failure dialog offers *Show status* instead. |
| B8 | conflicts | The message after resolving (number of conflicts, or "Merged automatically") is not mentioned, although it tells the user what to do next. |
| B9 | branches | Stashes don't include new (untracked) files. |
| B10 | versioning page | "Using external programs such as Subversion" still presents *Register* and *Update* as generic; they are Subversion-only now (for Git: *This file → Add to repository*, *Synchronize*). |

## C. Checked and correct

All the other claims, including: the commit dialog (buttons, initial
selection, suggested message, amend, merge behavior, errors kept inside),
*This file* entries and their conditions, staging semantics, project
commit and missing files, the status page buttons, signing and the
signature shown on commit pages, the *Compare with* entries and their
conditions, the review bar, history and read-only revisions, blame
(legend, "not committed", depth 30, refused with conflicts), log/graph/
commit pages, remotes, the first push setting the upstream, the "others
sent changes first" explanation with *Get changes*, the pull modes and
their menu, background operations and Cancel, no password prompts,
reloading and the unsaved-documents rule, the offer to save first, new
branch/tag dialogs, *Mark as resolved* (warning, save, stage), deleted or
added-on-both-sides documents, rebase swapping Mine/Theirs, the merge
driver and `.gitattributes`, every preference and its default, the
shortcuts (the mode `in-git-document` covers documents and Git pages),
*File → Revert*. All pages render, and all links resolve.

## D. Missing topics

* Nothing on **what not to version**: exported PDFs, large images; the
  generated `.gitignore` has a commented `*.pdf` line.
* No end-to-end **example**, such as "writing a paper with a coauthor on
  GitHub", which would connect the pages.
* No translations; the other manual pages usually exist in several
  languages.

## Recommended fixes

1. Code: always show *Git preferences* (A1); warn in the *Restore version*
   confirmation when the file has uncommitted changes (A5).
2. Pages: correct A2–A5 and B1–B10.
3. Optionally: a short example page (D).

## Found while fixing

* **Create and Clone were unreachable by default.** With the versioning
  tool *Automatic*, the Version menu is not shown for a document outside a
  working tree, so neither *Create Git repository* nor *Clone Git
  repository* could be reached without first choosing *Tools → Versioning
  tool → Always*. **Verified.**
* A frozen menu bar, seen while testing these entries, came from the test
  harness, not from TeXmacs: it "showed" lazy menus without hiding them
  again.

## Resolution (2026-09-25)

| Findings | Fix |
|----------|-----|
| A1 | *Git preferences* is always in the Version menu. |
| A5 | The *Restore version* confirmation says that changes which were neither committed nor staged will be lost; the page explains it. |
| Create/Clone | Also in *Tools → Versioning tool*; the pages give this path. |
| A2–A4, B1–B10 | Pages corrected: the mode question, restoring snapshots, the line merge of Git followed by *Resolve conflict* (and the merge driver, which avoids it), the footer, unsaved edits before resolving, *Merge branch*, stashes, the simple mode and servers, the Subversion-only entries. |
| D | New page *Example: a paper with a coauthor*, including what to put under version control and the fact that tags are not sent. |

Still open: translations of the chapter.
