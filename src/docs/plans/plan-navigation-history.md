# Plan: back and forward with a history

Status: planning report, 2026-10-19. Reference checkout: `upsilon_texmacs` at
918c4e6c83 (`~/t/lab/fix/plan-ref`). Paths below are relative to `src/`.

## 1. Summary

TeXmacs already has a per-window cursor history with Back/Forward commands,
toolbar icons and shortcuts. It is a good kernel but behaves like a "saved
positions" stack, not like a browser history: only a dozen call sites push
entries, the place you are leaving is often not recorded, nothing is capped,
labelled or persisted, and the toolbar buttons are always enabled. The plan is
to keep the existing mechanism (observer positions, per window), fix its
semantics so that Back/Forward behave like a browser/VS Code navigation stack,
record the missing jump kinds, add enabled state and a dropdown list of recent
places, and (optionally) persist the history across sessions.

## 2. Current behaviour

### 2.1 Implementation (Scheme only)

All logic lives in `TeXmacs/progs/utils/library/cursor.scm`:

- `cursor-history` / `cursor-future` (lines 186-187): two ahash tables keyed by
  `(window-get-serial)`, so the history is **per window**, across buffers.
  Each value is a list, newest first (`history-get`/`future-get`, 189-192).
- An entry (198-217) is `(position . body)`: a `position-new` observer plus the
  body tree of its buffer (`buffer-tree`). This is the upsilon version of the
  fix of PR #337 (commit 278d12fc27, 2026-10-13). `entry-alive?` asks
  `tree-active?` on the body; `entry-valid?` additionally rejects positions
  whose parent is `uninit`.
- `cursor-history-add p` (222-229): if `p` equals the top of the *future*, it
  moves that entry back to the history (so "add" after "back" does not fork the
  future); else, unless `p` equals the top of the history, a new entry is pushed.
  **The future is never cleared** on a new add, unlike a browser.
- `cursor-history-backward` (235-250): pops the top of the history, pushes it on
  the future, and goes there; if the cursor is already there it recurses (this
  is how "from" and "to" pairs work); invalid entries are deleted and skipped.
- `cursor-history-forward` (256-268): symmetric.
- `cursor-has-history?` / `cursor-has-future?` (231, 252): used by the Go menu.

Positions are C++ observers: `position-new` (`kernel/library/base.scm:315`)
calls `position-new-path` → `edit_modify_rep::position_new`
(`src/Edit/Modify/edit_modify.cpp:429`), a `tree_position` observer
(`src/Data/Observers/tree_position.cpp`) attached to the subtree; it follows
inserts, removes, splits and joins (`notify_*`, lines 85-130), so entries
survive edits. `position-get` (`edit_modify.cpp:453`) applies `correct_cursor`.
Going to an entry uses `go-to` (`kernel/boot/abbrevs.scm:226`), which switches
buffer through `path->buffer` when the path is not in the current buffer.

### 2.2 Who records (push sites)

| Event | Site | Pushes |
|---|---|---|
| Follow a hyperlink to an id or url | `link/link-navigate.scm:513-532` via `go-to-vertex` (572-576) | from + to |
| Go to line / column (with origin) | `generic/generic-edit.scm:781-790` | from only |
| Contextual help, auto tmfs pages | `generic/generic-doc.scm:409`, `utils/automate/auto-tmfs.scm:50` | from |
| API docs | `doc/apidoc.scm:144,149` | from |
| Bibliography page | `database/bib-local.scm:162-164` | from + to |
| Edit package/macro source | `generic/document-style.scm:170-172`, `source/macro-edit.scm:183` | from (+ to) |
| Git versioning, blame, tmfs versions | `version/version-git.scm:953`, `git-blame.scm:185`, `version-tmfs.scm:106` | from |
| Font design page | `fonts/font-design.scm:1155` | from |
| Manual | `C-!`, Go > Save position | current |

**Not recorded**: buffer switches (Go menu, tabs, `switch-to-buffer`), opening
or loading a file, help pages from the Help menu (`doc/help-funcs.scm:66-77`,
`load-help-buffer-sub` has no push), search hits (`generic/search-widgets.scm:233`
`search-next-match`, interactive search), go to label / reference, table of
contents and outline jumps, `go-to-line` from `link-navigate.scm:450`,
`doc/source-links.scm:91`, `doc/apidoc-funcs.scm:242` (no origin passed),
code navigation (`lsp/lsp-edit.scm:920` `lsp-goto-location`, `code/code-navigate.scm:35`
`code-go-to-definition`), mouse clicks far away, `go-start`/`go-end`.

### 2.3 UI

- Go menu: `texmacs/menus/file-menu.scm:330-335`, Back/Forward are shown only
  `when` there is history/future (greyed otherwise), plus "Save position".
- Toolbar: `texmacs/menus/main-menu.scm:194-199` in `texmacs-main-icons`
  (line 149): `tm_back` / `tm_reload` (= `revert-buffer`!) / `tm_forward`.
  They are **always enabled**, no dropdown. Icons exist as SVG in
  `TeXmacs/misc/pixmaps/{light,dark,monochrome/*}/tm_{back,forward}.svg`.
- Keys (`generic/generic-kbd.scm`): emacs profile `C-<`, `C->`, `C-!`
  (414-416); gnome/kde/windows `A-left`/`A-right` (515, 580, 745); macOS
  `macos [` / `macos ]` = Cmd-[ / Cmd-] (628-629).
- Speech: `generic/generic-speech-en.scm:89` and `-fr.scm:73` bind "forward" /
  "suivant" to `cursor-history-backward` (bug).

### 2.4 Tests

`check/misc-modules-test.scm:903` `test-cursor-history` (registered at 2599):
an entry outliving its closed buffer, then back/back/forward in another buffer.

## 3. Gaps

1. **The place being left is lost.** Back pops the top entry; if the user moved
   (typed, scrolled, clicked) after the last push, the current place is never
   pushed to the future, so Forward does not return to it. Browsers and VS Code
   always record "where I was" when going back.
2. **Future never truncated.** A new jump after going back keeps the old future;
   then Forward replays stale branches. Browsers drop the forward list on a new
   navigation; VS Code does the same.
3. **Sparse recording** (2.2): buffer switches, file open, help pages, search,
   label/TOC/outline jumps, code navigation are not recorded; so Back after
   "open file" or "go to definition" does nothing useful.
4. **Noise and duplicates.** Only the top is compared (`cursor-same?`, 219);
   A-B-A-B sequences accumulate; nearby positions (same paragraph, a few lines
   apart) are separate entries. No cap: the lists grow unboundedly per window.
5. **Closed buffers.** In upsilon, an entry keeps its buffer *body* alive
   (`entry-new` stores `(buffer-tree)`), so closed documents stay in memory until
   back/forward walks over the entry; there is no prune on close. Back over a
   closed buffer silently skips it instead of reopening the file (browser and
   VS Code reopen).
6. **Revert / reload** replaces the body: entries of that buffer become inactive
   and are dropped (change of behaviour noted in PR #337).
7. **Help/tmfs pages**: auxiliary windows (`open-auxiliary`) have their own
   window serial, so the "from" entry is pushed in the source window while the
   help is shown elsewhere; Back in the help window has nothing.
8. **No labels**: an entry is a bare path; a dropdown cannot show "Section 3.2
   in foo.tm".
9. **No persistence** across sessions, nor per-document "last places".
10. **Toolbar**: buttons not greyed; "Reload" sits between them and is
    destructive (revert) for a misclick.
11. Speech bindings bug (2.3).

## 4. Comparison

- **Browsers**: linear stack with a current index; new navigation truncates
  forward; per tab; long-press/right-click on Back lists ~15 entries with page
  titles; Alt-Left/Right, Cmd-[ / Cmd-] on macOS; mouse buttons 4/5; history
  survives session restore.
- **VS Code**: Go Back / Go Forward (Ctrl-Alt-- / Ctrl-Shift--, macOS Ctrl-- /
  Ctrl-Shift--), global per window across editors; entries on editor open/switch,
  go-to-definition, search result, and cursor jumps of more than ~10 lines;
  consecutive entries in the same area merged; cap ~50; separate "last edit
  location" stack. Entries survive edits (offsets adjusted) and closed editors
  are reopened.
- **Emacs**: mark ring (local per buffer, global across buffers, size 16),
  `push-mark` on big jumps (search, `M-<`, `M->`), `xref` stack for
  definitions (`M-.` / `M-,`), markers follow edits.
- **Acrobat / Skim / Preview**: "Previous view / Next view" (Cmd-[ / Cmd-],
  Alt-Left), recorded on link follows, page jumps, outline clicks, searches.

TeXmacs is closest to Acrobat (a document with links) and VS Code (many
buffers). The proposal follows the VS Code rules with browser-like UI.

## 5. Proposed model

**Structure.** Per window, a single list with a current index (equivalently
the existing history/future pair, keeping the current place as an implicit
entry). Entry = `(position body url label time)`:
- `position`: the observer as now (follows edits);
- `url` of the buffer and a **path snapshot** (fallback when the buffer is
  closed: reopen `url` and go to the path, clamped by `correct_cursor`);
- `label`: computed at push time (section title via the innermost
  `section`/`subsection`/`chapter` ancestor, else the first 40 chars of the
  paragraph), plus the buffer title;
- the body is **not** kept alive: use PR #337's approach (body only to test
  `tree-active?`, prune on add) plus a `notify-buffer-closed` hook
  (`cursor.scm:165`, called from `src/Texmacs/Data/new_buffer.cpp:78,159`)
  that converts the entries of the closing buffer to "closed" entries
  (`url` + path + label, position deleted while still valid).

**What pushes** (via one helper `(navigation-jump thunk)` that records the
origin, runs the jump, records the target):
- link follows (existing), go to label/reference, TOC/outline clicks;
- buffer switch and file open/load (record the place being left in the old
  buffer, and the target);
- help pages, apidoc, tmfs pages (existing ad hoc pushes become this helper);
- search: when the search starts and when it is accepted (not each hit);
- code navigation: go to definition/references/symbol (`lsp-goto-location`,
  `code-go-to-definition`);
- large cursor jumps: `go-start`, `go-end`, `go-to-line`, and optionally any
  cursor move of more than N paragraphs (via `notify-cursor-moved`, cursor.scm:155),
  throttled.

**Rules.**
- Going back first records the current place if it differs from the current
  entry ("where I was").
- A new push truncates the future (browser rule). Keep the "re-add equals top
  of future" shortcut only when it is the same jump.
- Merge: a new entry replaces the top one if same buffer and within the same
  paragraph/section (or < 10 lines in a program), like VS Code.
- Cap: 50 entries per window (preference `navigation history size`); oldest
  dropped with `position-delete`.
- Scope: per window (as now, as browsers). Auxiliary windows (help) start from
  a copy of the opener's history so Back returns to the source document.
- Revert/reload: before replacing the body, convert entries to path snapshots
  and re-anchor them after load (fixes the PR #337 behaviour change).

**Persistence (optional, phase 4).** On quit, save per window the last 20
entries as `(url path label)` to `$TEXMACS_HOME_PATH/system/history.scm`
(not in preferences: see the "tests must not write preferences" rule);
restore lazily as closed entries. Skip `tmfs://` and untitled buffers.

## 6. UI

- **Toolbar**: replace the three icons (`main-menu.scm:194-199`) by
  `(when (cursor-has-history?) ...)` / `(when (cursor-has-future?) ...)` so they
  grey out; move Reload away (or behind `detailed-menus?`). Each button gets a
  dropdown: a `=>` pulldown with the last 15 labels (`navigation-back-menu`,
  `navigation-forward-menu`), shown via a small arrow button next to the icon,
  since long-press is not a generic menu feature. Enabled state refreshes with
  `update-menus` after each push/pop.
- **Go menu** (`file-menu.scm:330`): Back, Forward, then a "Recent places"
  submenu (same labels, with buffer titles), "Clear history".
- **Shortcuts**: keep Cmd-[ / Cmd-] (macOS), Alt-Left / Alt-Right
  (gnome/kde/windows), `C-<`/`C->` (emacs); add mouse buttons 4/5 (Qt
  `Qt::BackButton`/`ForwardButton`, Cocoa `otherMouseDown` button 3/4, SDL3
  `SDL_BUTTON_X1/X2` in Vue) mapped to the same commands; check the conflict of
  Alt-Left with word movement inside math or programs in gnome/kde profiles.
- **GUIs**: icon bars are generic Scheme menus. `when` produces
  `WIDGET_STYLE_INERT`, honoured by Qt (`src/Plugins/Qt/qt_ui_element.cpp`),
  Cocoa (`src/Plugins/NS/ns_ui_element.mm:1194,1366`) and Vue
  (`src/Plugins/Vue/vue_gui.cpp:1720`, `vue_widget.cpp:1256`); `pulldown_button`
  exists in all three (Qt 443, NS 1177/1370, Vue `vue_widget.cpp:1076,1923`).
  So no C++ is needed for the UI beyond checking that the pulldown is lazily
  rebuilt (Qt uses a `promise<widget>`) and that Vue/Cocoa refresh the inert
  state of icons on `update-menus`. The browser (wasm) build has one window:
  Back/Forward could also be wired to the browser's own history (popstate) in
  a later step.

## 7. Interplay with PR #337

PR #337 (base `wip_fixes`, open, "merge: ready") fixes the use-after-free of
positions of closed buffers. Upsilon already carries an earlier variant
(278d12fc27) that differs in two points:
- upsilon's `entry-new` stores `(buffer-tree)` (keeps the closed document's
  tree in memory); #337 takes `path->tree (list (car p))` and prunes the
  entries of closed buffers in all windows on every add (`history-prune`);
- #337's test adds `cursor-history-drain` and a "forward over a closed buffer"
  case.

Recommendation: merge #337 into wip_fixes first, then bring its final version
into upsilon (replacing 278d12fc27's variant, conflict limited to
`cursor.scm:186-268` and the test). Phase 1 below builds on #337's
entry abstraction (`entry-new/alive?/path/valid?/delete`), which is exactly
the place to add url, path snapshot and label.

## 8. Phased plan

**Phase 0 — groundwork (0.5 day).** Merge #337 (wip_fixes) and port to
upsilon; fix the speech bindings (`generic-speech-en.scm:89`,
`generic-speech-fr.scm:73`); grey toolbar buttons with `when`.

**Phase 1 — semantics (1.5 days).** New module
`TeXmacs/progs/utils/library/navigation.scm` (or keep in `cursor.scm`):
entry record with url/path/label, `navigation-jump` helper, record-current on
Back, truncate future on push, merge rule, cap, `notify-buffer-closed`
conversion to closed entries and reopening on Back/Forward, revert handling.
Keep `cursor-history-add/backward/forward` as the public names (many callers).

**Phase 2 — more push sites (1-1.5 days).** Buffer switch / load
(`texmacs/texmacs/tm-files.scm` `load-buffer`, `switch-to-buffer` wrappers),
help (`doc/help-funcs.scm:66`), search start/accept
(`generic/search-widgets.scm`), go to label and TOC
(`link/link-navigate.scm`, `generic/document-part.scm` / outline), code
navigation (`lsp/lsp-edit.scm:920`, `code/code-navigate.scm:35`),
`go-start`/`go-end`, line jumps without origin (`link-navigate.scm:450`,
`doc/source-links.scm:91`, `doc/apidoc-funcs.scm:242`). Replace ad hoc pairs of
`cursor-history-add` by `navigation-jump`.

**Phase 3 — UI (1-1.5 days).** Dropdown menus with labels, Go > Recent places,
Clear history; toolbar layout (`main-menu.scm:194-199`); mouse buttons 4/5 in
Qt (`src/Plugins/Qt/QTMWidget.cpp` mouse events), Cocoa
(`src/Plugins/NS/TMView.mm`), Vue (`src/Plugins/Vue/vue_gui.cpp` SDL events);
check inert refresh of icons in Cocoa and Vue. Doc page in the help
(`TeXmacs/doc/main/...` editing/navigation) and "Recent changes" of the
browser help page (wasm rule).

**Phase 4 — persistence and large cursor jumps (1 day, optional).**
Save/restore to `system/history.scm`; `notify-cursor-moved` jump detection
behind a preference.

Total: about 5-6 days; phases 0-1 alone fix the main usability issues.

## 9. Test strategy

Extend `check/misc-modules-test.scm` (`test-cursor-history`, 903) or add
`check/navigation-test.scm` registered in the harness suite list:
- back records the current place; forward returns to it after an edit;
- push after back truncates the future;
- merge of nearby positions; cap at N (push N+5, count);
- entries follow edits: insert text before an entry, back lands on the same
  word (compare the subtree, not the path);
- close a buffer with entries: no crash (PR #337 cases), back reopens the
  file (use a temporary file in `$TEXMACS_HOME_PATH`, not a `with-buffer-doc`
  untitled buffer) and lands on the snapshot path;
- revert keeps entries (re-anchored);
- link follow, go to label, buffer switch, search accept each push exactly
  the expected entries (drive with the commands, not the keyboard);
- labels: an entry inside `section` "Intro" has label "Intro";
- menus: `cursor-has-history?` false on a fresh window; the dropdown menu
  expands to the expected labels (`menu-expand` on `navigation-back-menu`);
- persistence: write to a temp home, never to preferences; restore yields
  closed entries.
Run under Guile/Qt and S7 (`tests/scheme/check.sh all`), the Vue build, and
the wasm node tests for one smoke case. Beware S7 pitfalls: no `(quit)`, and
named `let` instead of `do` with aliased counters.

## 10. Risks

- **Observer cost**: each entry is a `tree_position` observer on a subtree;
  50 per window is negligible, but unbounded recording via
  `notify-cursor-moved` must be throttled.
- **Memory**: entries must not keep closed bodies alive (upsilon's variant does).
- **Use-after-free** class of bugs around positions (the #337 abort): every new
  path that reads positions must go through `entry-valid?`.
- **Noise**: recording buffer switches and searches can make Back feel
  "jumpy"; the merge rule and "search accept only" mitigate it.
- **Keyboard conflicts**: Alt-Left/Right in gnome/kde profiles vs word motion
  in some modes; Cmd-[ / Cmd-] vs indentation in program modes on macOS.
- **Auxiliary windows**: copying the history into help windows interacts with
  window-per-buffer mode.
- **Behaviour change** for users of `C-!` "Save position" (manual bookmarks
  now subject to cap and merge).

## 11. Open questions for the maintainer

1. Per window (browser) or global across windows (closer to Emacs global mark
   ring)? The plan keeps per window.
2. Should buffer switches and file opens be recorded at all, or only "jumps"
   inside documents (Acrobat style)?
3. Should Back reopen a closed document, or skip it as today?
4. Record large mouse/keyboard cursor moves (VS Code) or only explicit
   navigation commands?
5. Keep Reload between Back and Forward in the toolbar (browser layout) or move
   it away?
6. Persistence across sessions: wanted? per window or a single list?
7. Should "Save position" (`C-!`) become a separate bookmark list, outside the
   capped history?
8. Default cap (50?) and merge distance; preference or fixed?
9. In the browser build, tie Back/Forward to the browser's history buttons?
