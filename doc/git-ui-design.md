# Git support: UI design proposal

This proposes improvements to the user interface of the git support on
the `wip-git-versioning` branch. The features themselves are in
[git-features.md](git-features.md); this document is only about how they
are presented. Everything proposed here uses mechanisms TeXmacs already
has (section 6), so no new C++ widgets are needed.

---

## 1. Who we design for

| Persona | Typical use | What they need from the UI |
|---------|-------------|----------------------------|
| **Coauthor** (mathematician or physicist, knows git barely or not at all) | Opens the shared paper, writes, "saves a version", gets the others' changes | To always know whether they are up to date and whether their work is safe. One obvious "send my changes" action. Conflicts explained in plain words. |
| **Maintainer** (knows git) | Branches for a revision, reviews coauthors' changes, tags "submitted" | Everything within two clicks, a clear view of what will be committed, keyboard shortcuts, no surprises. |

The first persona is the one the current UI serves worst.

---

## 2. What is wrong with the current UI

An audit of the branch as of 2026-09-24:

1. **Hidden by default.** Nothing git-related appears until the user
   turns on *Tools → Versioning tool*. A coauthor who clones a repository
   and opens the paper sees no sign that it is versioned.
2. **No permanent status.** Whether the document is committed, whether
   the branch is behind, and whether a pull is running all require opening
   a menu or a page.
3. **Overloaded menus.** *Version* mixes the generic comparison tools, SVN,
   the per-file git entries and a *Git* submenu of more than 25 entries in
   one flat list.
4. **Important input in the footer.** "Commit this file", "Save
   snapshot", "Tag", "New branch" and "Add remote" ask for text in the
   one-line footer prompt. It is easy to miss, has no room for a
   multi-line message, and cannot show options such as "sign" or "switch
   to the new branch".
5. **Feedback is one footer line.** Errors from git (for example a
   rejected push) flash in the footer and disappear. Their details are
   only on the *Git output* page, which users won't find.
6. **The side panel is plain text.** Its status codes (`~`, `+`, `?`,
   `C`) are cryptic, it has one row of text buttons, and it offers no way
   to write a commit message in place.
7. **Resolving differences is not guided.** After *Resolve conflict* or
   *Compare with*, the user must know the *Version → Move / Retain* menus
   or their shortcuts. There is no "3 of 7" indication and no "keep mine /
   keep theirs" at hand.
8. **The pages look like reports.** On the status page, the action links
   are small bracketed text, and the columns are aligned only with fixed
   spaces.
9. **Settings are scattered.** Simple mode, signing, pull mode, blame
   depth, large-file size and the merge driver live in a *Git →
   Preferences* submenu or in hidden preferences, not in *Edit →
   Preferences*.
10. **No shortcuts** for the frequent actions: status, commit, pull/push,
    and the next or previous difference while resolving.

---

## 3. Principles

1. **Status is ambient.** The user should never have to ask whether
   their work is saved in git or whether they are up to date.
2. **One hub.** The side panel becomes the main UI for git. Menus become
   short entry points, and pages are for reading (history, graph, blame).
3. **Two levels.** Simple mode is the default for newcomers: snapshots,
   synchronize, restore. Full mode shows staging, branches and remotes.
   The same hub serves both; full mode only shows more.
4. **Dialogs for decisions, the footer for news.** Text input and choices
   go in small dialogs; the footer only reports what happened.
5. **Errors are explained and actionable.** Every failure says what
   happened in plain words, offers the likely next step, and can show
   git's own output.
6. **Guided modes for multi-step work.** Resolving conflicts or reviewing
   differences gets a bottom bar with progress and the two or three
   relevant actions.
7. **Reuse TeXmacs idioms.** Use the existing widgets, icons, division
   styles, side and bottom tools, and the preferences dialog, so the git UI
   looks like the rest of TeXmacs.

---

## 4. Proposals

### 4.1 Ambient status in the footer (P1, small)

Append a short git indicator to the left footer through `footer-hook`,
which `edit_footer.cpp` calls for every footer update:

```
 text roman 10pt                           ⎇ main  ●3  ↑1 ↓2   ⟳ pushing…
```

| Part | Meaning |
|------|---------|
| `⎇ main` | current branch (`⎇ detached` when detached) |
| `●3` | 3 changed files; `✓` when clean |
| `↑1 ↓2` | ahead and behind the upstream; hidden when 0 |
| `⚠ 2 conflicts` | replaces `●` during a merge with conflicts |
| `⟳ pushing…` | a background command is running |

* It reads only the cached status, so a footer update never starts a git
  process. When the cache is stale, the indicator keeps its last value
  and refreshes on the next save or git action.
* It is shown for documents in a working tree, even when the versioning
  tool is off (see 4.2).
* The hook is chained: the git hook calls the previous `footer-hook`, so
  the debug memory display keeps working.

### 4.2 Discoverability: "auto" versioning tool (P1, small)

* The preference `"versioning tool"` gets a third value, **auto** (the new
  default): the *Version* menu appears when the current document is in a
  git or SVN working tree.
* The first time a document from a repository is opened, a one-line
  footer tip appears once: *"This document is in a Git repository — see
  Version → Git panel"*.

### 4.3 Restructured Version menu (P1, medium)

Today the menu is a flat list of 30 or more entries. The proposal:

```
Version
├─ Save snapshot…          ⌘⇧S      (simple mode)  | Commit…  (full mode)
├─ Synchronize             ⌘⇧Y      (pull, then push; disabled when up to date)
├─ Git panel               ⌘⇧G
├─ ───
├─ Compare with          ▸  Last commit · Staged version · Remote version ·
│                           Before last pull · Tag ▸ · Branch ▸ · Other revision…
├─ Restore version       ▸  (last 15 versions of this document)
├─ History of this document
├─ Who changed what
├─ ───
├─ This file             ▸  Stage · Unstage · Commit this file… · Discard changes…   (full mode)
├─ Project               ▸  Commit project… · Add N missing files
├─ Repository            ▸  Status · Log · Graph · Branches and tags ·
│                           New branch… · Switch to ▸ · Merge ▸ · Tag this version… ·
│                           Stash ▸ · Remotes ▸ · Git output
├─ ───
├─ Differences           ▸  First · Previous · Next · Last · Show ▸ · Retain ▸ · Grain ▸
└─ Git preferences…
```

* The **top level stays under about 12 entries**, and the most frequent
  actions come first.
* **Conflict entries go on top when relevant.** When the document has a
  conflict, *Resolve conflict…* and *Mark as resolved* appear at the top,
  highlighted with `(group "Conflict")`.
* **The generic tools keep their place.** *Differences* is the old
  *Move/Show/Retain/Grain* group; comparing two arbitrary files stays in
  *Compare with ▸ Other file…*.

### 4.4 The Git panel as the hub (P1, medium)

A redesigned `git-tool`, built from `division "title"` headers, `tabs`, and
icon buttons that already ship with TeXmacs (`tm_add`, `tm_close_tool`,
`tm_cloud_upload`, `tm_cloud_download`, `tm_check`):

```
┌ Git ─────────────────────────────── x ┐
│ ⎇ main          ↑1 ↓2   [⇩ Pull][⇧ Push]│   ← sync bar; Pull/Push become
│                                        │     "⟳ Pushing…  [Cancel]" while busy
├ Changes │ History │ Branches ─────────┤   ← tabs
│ Conflicts                              │
│  ⚠ paper.tm            [Resolve]       │
│ Staged                                 │
│  ✚ figures/plot.pdf              [–]   │   ← unstage
│ Changed                                │
│  ● paper.tm        [Compare]     [+]   │   ← stage
│  ● refs.bib                      [+]   │
│ Untracked                              │
│  ? notes.tm                      [+]   │
├────────────────────────────────────────┤
│ ┌────────────────────────────────────┐ │
│ │ Message                            │ │   ← texmacs-input, 3 lines
│ └────────────────────────────────────┘ │
│ [Suggest]  ☐ Amend  ☐ Sign   [Commit]  │
└────────────────────────────────────────┘
```

* **Rows are clickable.** Clicking a file name opens it; *Compare* opens
  the structured comparison with the last commit.
* **Status is shown with symbols and a legend,** not letter codes. A
  tooltip or balloon (`balloon` widget) spells out "modified", "staged"
  and so on.
* **The commit box is in the panel.** Commit takes the staged files (or,
  in simple mode, all files, labelled *Save snapshot*). The existing
  commit dialog stays, for selecting files.
* **The History tab** lists the last 20 commits of the current document,
  with *Compare* and *Restore* buttons, plus a link to the full log.
* **The Branches tab** shows the current branch, a switch-to list and
  *New branch…*.
* **Simple mode** has the same panel without the Staged section and
  without stage/unstage buttons, with *Save snapshot* in place of
  *Commit* and *Synchronize* in place of *Pull/Push*.
* **The panel can be docked** left or right, and remembers its position
  (`tool-select :right`).

### 4.5 Small dialogs instead of footer prompts (P1, small)

One generic widget, `git-form-widget`, built on `form` and `form-input`
like `gpg-widgets.scm`, covers all the inputs:

| Dialog | Fields |
|--------|--------|
| Commit this file | message (3 lines), ☐ Sign |
| Save snapshot | description (3 lines) |
| New branch | name, base (current / tag / branch ▾), ☑ Switch to it |
| Tag this version | name, message, ☐ Sign |
| Add remote | name (default `origin`), URL |
| Other revision (compare) | revision, or a pick list of the last 30 commits |

* Names are validated **in the dialog** (`git check-ref-format --branch`).
  The OK button stays disabled until the input is valid, with the reason
  shown underneath.

### 4.6 Errors and progress (P1, small)

* **Failures open a dialog.** When a command fails, show a short
  plain-words explanation with buttons: *Show details* (reusing
  `report-system-error` from `menu-widget.scm`, which lists stdout and
  stderr) and the most likely next action:

  | git failure (matched on stderr) | Explanation | Action button |
  |------|------|------|
  | push rejected, non-fast-forward | "Others pushed changes first." | *Get their changes* (pull) |
  | authentication or permission | "The server refused access." | *Open Git output* |
  | nothing to commit | "There are no changes to save." | — |
  | uncommitted changes would be overwritten | "Save a snapshot of your changes first." | *Save snapshot…* |
  | merge conflict | "Some parts were changed on both sides." | *Resolve…* |

* **Progress is visible.** Background commands show in the footer
  indicator and on the panel's sync bar, with *Cancel*. On success the
  footer says what happened ("Pushed 2 commits to origin").

### 4.7 Guided review and conflict resolution (P1, medium)

Whenever the current buffer contains `version-*` markup, from a
comparison, a structured merge, or a merge driver result, show a
**bottom tool** (`tm-tool*` with `:bottom-indent`):

```
┌───────────────────────────────────────────────────────────────────────────┐
│ Conflict 2 of 5   [◀ Previous] [Next ▶]   Keep: [Mine] [Theirs] [Both]      │
│                   Show: (•) both ( ) mine ( ) theirs   [Keep all mine ▾]   │
│                                                        [Mark as resolved] │
└───────────────────────────────────────────────────────────────────────────┘
```

* It is built from the existing `version-next-difference`,
  `version-retain`, `version-show` and `git-mark-resolved`. The count
  comes from a `tree-search` for `version-both` and related markup.
* Its wording follows the context: *Difference 2 of 12 · Keep: [Old]
  [New]* when comparing; *Conflict 2 of 5 · Keep: [Mine] [Theirs]* when
  resolving.
* When the last difference is gone, the bar says *All differences
  resolved* and offers *Mark as resolved* or *Close*.
* The existing shortcuts (`C-up`/`C-down`, `C-1`/`C-2`) are shown in the
  buttons' balloons, to teach them.

### 4.8 Pages that look like TeXmacs (P2, medium)

For the status, log, commit, branches, graph and blame pages:

* Use a **tabular layout** (`tformat` with column alignment), as the
  commit page already does, for file lists and branches.
* Replace bracketed text links with **small buttons**: an `action` around
  a boxed label, using a tiny style package `git-pages.ts` that defines
  `git-button`, `git-badge` and `git-muted`. Pages then read as UI rather
  than as reports, and adapt to dark mode through the style.
* **Empty states** say what to do: "Nothing to commit — your work is
  saved" or "No remote — Add remote…".
* **Blame gets colours:** each author gets a stable colour from a small
  palette, used as a thin left bar next to their paragraphs, with a
  legend at the top ("■ Alice 12 paragraphs · ■ Bob 5 ·
  ■ uncommitted 1"). A toggle hides the notes and shows only the bars.
* **Graph:** colour the branch lines per column, and link refs to the
  branches page.

### 4.9 Commit dialog refinements (P2, small)

* Buttons *Select all* / *Select none*, and a status symbol in front of
  each file.
* The message editor shows the conventional layout as greyed hints:
  summary line, blank line, details. A soft warning appears when the
  summary exceeds 72 characters.
* Double-clicking a file opens its structured comparison with the last
  commit, in the main window.
* If the message is empty when the dialog opens, *Suggest* fills it in
  automatically. A preference `git suggest messages`, on by default,
  controls this.
* During a merge, a banner explains that the merge commit must contain
  all changes.

### 4.10 Git preferences in Edit → Preferences (P2, small)

Add a **Git** tab to the preferences widget (`preferences-widgets.scm`),
with:

* the git executable;
* Mode: (•) Simple — snapshots ( ) Full — staging and branches;
* When pulling: fast-forward / merge / rebase;
* ☐ Sign commits and tags;
* Warn before adding files larger than [10] MB;
* Blame looks back [30] commits;
* ☐ Merge TeXmacs documents structurally in this repository (per
  repository, it installs the driver);
* the versioning tool: Auto / Always / Never.

*Git → Preferences* in the menu then becomes a link to this tab.

### 4.11 First-run choice of mode (P2, small)

The first time the user runs a git action in a session with no mode set,
ask once:

```
How do you want to work with Git?
  (•) Simple — save snapshots and synchronize with coauthors
  ( ) Full   — staging, branches, remotes (for Git users)
                                   You can change this in Preferences.  [OK]
```

### 4.12 Keyboard shortcuts (P2, small)

These go in a `kbd-map` in `version-kbd.scm`, active when the current
buffer is in a working tree. They are prefixed like the existing
`version` bindings, so they follow each platform's conventions:

| Action | Shortcut (under the `version` prefix) |
|--------|------|
| Git panel | `version g` |
| Commit… / Save snapshot… | `version c` |
| Synchronize (pull, then push) | `version y` |
| Status page | `version s` |
| Compare with last commit | `version =` |
| Next / previous difference | existing `version down` / `version up` |

### 4.13 Words (P1, trivial)

* **One verb per concept, in all menus, dialogs and messages:**
  * *Save snapshot* in simple mode and *Commit* in full mode;
  * *Get changes* for pull, *Send changes* for push, *Synchronize* for
    both, with the git term in the balloon;
  * *Mine / Theirs* when resolving conflicts, *Old / New* when comparing.
* Avoid "HEAD", "index" and "upstream" in the UI: say "last commit",
  "staged version" and "remote version".

---

## 5. Priorities and effort

| Priority | Item | Effort | Depends on |
|----------|------|--------|------------|
| P1 | 4.1 Footer indicator | S | — |
| P1 | 4.2 Auto versioning tool | S | — |
| P1 | 4.5 Small dialogs | S | — |
| P1 | 4.6 Errors and progress | S–M | 4.5 |
| P1 | 4.7 Guided review bar | M | — |
| P1 | 4.4 Panel redesign | M | 4.5 |
| P1 | 4.3 Menu restructure | M | 4.4 (so the menu can shrink) |
| P1 | 4.13 Words | S | alongside everything |
| P2 | 4.9 Commit dialog | S | — |
| P2 | 4.10 Preferences tab | S | — |
| P2 | 4.11 First-run choice | S | 4.10 |
| P2 | 4.12 Shortcuts | S | — |
| P2 | 4.8 Pages restyled, blame colours | M | — |

A reasonable first increment is **4.1 + 4.2 + 4.5 + 4.7**. These are the
changes a coauthor would notice at once: they see the status, they are
never asked for text in the footer, and conflicts become a guided task.

---

## 6. How this maps onto TeXmacs

| Need | Existing mechanism |
|------|--------------------|
| Footer indicator | `footer-hook` (defined in `kernel/boot/debug.scm`, called from `edit_footer.cpp`) |
| Panel and tabs | `tm-tool*`, `tabs`/`tab`, `division "title"`/`"plain"`, `refreshable`, `refresh-now` |
| Icons | `(icon "tm_add.svg")` etc. from `misc/pixmaps/modern/*` |
| Balloons | the `balloon` widget |
| Dialogs | `dialogue-window` with `form`, `form-input`, `form-values`, `bottom-buttons` |
| Bottom bar | `tm-tool*` with `:bottom-indent`, `tool-select :bottom` |
| Error details | `report-system-error` in `kernel/gui/menu-widget.scm` |
| Preferences tab | `preferences-widgets.scm` (tabs of the preferences dialog) |
| Shortcuts | `kbd-map` with a `:mode` predicate |
| Page styling | a small `.ts` style package for the generated pages |

---

## 7. Open questions

1. **Footer vs. title bar:** is the left footer the right place for the
   indicator, or should the window title carry the branch (e.g. "paper.tm
   — main ●")?
2. **Default mode:** should simple mode be the default for everyone, or
   only when the user has no global git configuration (no `user.name`)?
3. **Commit box in the panel:** is having both an inline commit box and a
   commit dialog confusing? The alternative is a panel with a single
   *Commit…* button.
4. **Auto versioning tool:** is it acceptable upstream that the *Version*
   menu appears automatically for documents in a repository?
