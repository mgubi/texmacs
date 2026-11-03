# TeXmacs internals relevant to version control

These notes cover only what the versioning code uses or will need. File
references are to `master`. Scheme paths are relative to `src/TeXmacs/progs/`
and C++ paths to `src/src/`.

## 1. Scheme modules, `tm-define` and dispatch

* The Scheme dialect is Guile 1.8. `master` has some S7-compatibility
  cleanups, e.g. `#\tab` instead of `#\ht` and `newline` instead of `nl`.
  Follow them in new code.
* `(texmacs-module (version version-git) (:use (version version-tmfs)))`
  declares a module. `define` is private to the module. `tm-define` and
  `define-public` are global.
* **Overloading with `:require`.** `tm-define` can be called several times for
  the same name. Each definition may add a `(:require cond)` guard. When the
  function is called, the most recently loaded definition whose guard holds is
  the one that runs, and the unguarded definition is the fallback. The whole
  VCS layer depends on this:
  ```scheme
  (tm-define (version-status name) ...generic fallback...)          ; version-tmfs
  (tm-define (version-status name)
    (:require (== (version-tool name) "git")) ...)                   ; version-git
  ```
  So a backend is a module that re-defines the generic `version-*` API under
  a `:require` on `version-tool`. Load order matters. A backend has to be
  loaded before its overloads can take effect, which is why `version-tool`
  calls `module-provide` lazily (see vcs-current-state.md).
* Other `tm-define` options you will see: `:interactive #t` (the command may
  prompt the user), `:synopsis`, `:argument`, `:check-mark`, and `:mode`
  (inside `kbd-map`).
* **Lazy loading** is set up in `init-texmacs.scm`, around line 486:
  ```scheme
  (lazy-menu (version version-menu) version-menu)
  (lazy-keyboard (version version-kbd) with-versioning-tool?)
  (lazy-define (version version-tmfs) update-buffer commit-buffer)
  ```
  The `tmfs` classes `history`, `revision`, `commit` and `git` are **not**
  registered with `lazy-tmfs-handler`. A `tmfs://git/...` URL opened before
  the version modules are loaded (for example from the recent-files list at
  startup) therefore has no handler. The fix is a
  `(lazy-tmfs-handler (version version-tmfs) history revision commit)` line
  plus one for `git`.
* **Modes.** `texmacs-modes` in `kernel/texmacs/tm-modes.scm` defines
  predicates such as `with-versioning-tool%`, which is true when the
  preference `"versioning tool"` is `"on"`. That preference is toggled from
  Tools → Versioning tool, and it decides whether the **Version** menu
  appears (`texmacs/menus/main-menu.scm`, lines 68 and 128).
* **Preferences.** `(define-preferences (name default on-change) ...)`, then
  `get-preference` and `set-preference`. Git settings such as the executable
  path, whether to auto-stage on save, and log length belong here.

## 2. The `tmfs://` virtual file system

`kernel/texmacs/tm-file-system.scm`

* A URL `tmfs://<class>/<name>` is resolved through `tmfs-handler-table`,
  keyed by `(class . action)`. The actions are `load`, `save`, `title`,
  `format`, `permission?`, `master`, `wrap`, `date`, `autosave` and `remove`.
* They are defined with the macros `tmfs-load-handler`, `tmfs-title-handler`,
  `tmfs-format-handler` and so on:
  ```scheme
  (tmfs-load-handler (git name) ...)   ; name = everything after tmfs://git/
  ```
  A load handler returns a document, either as a string in a TeXmacs format
  or as an stree (which is serialized with `object->tmstring`). The return
  value **becomes the buffer contents**, so version-control views are just
  read-only generated TeXmacs documents. There is no special widget behind
  them.
* Helpers: `tmfs-car` and `tmfs-cdr` split `name` on `/`,
  `url->tmfs-string` and `tmfs-string->url` escape a real URL into a tmfs
  path component, and `tmfs-decompose-name` returns `(class name)`.
* The versioning URL schemes now in use:
  | URL | Meaning | Defined in |
  |-----|---------|------------|
  | `tmfs://history/<url>` | history of one file | version-tmfs.scm |
  | `tmfs://revision/<rev>/<url>` | contents of `<url>` at `<rev>` | version-tmfs.scm |
  | `tmfs://commit/<rev>/<root>` | one commit (message + numstat) | version-tmfs.scm (git-specific code!) |
  | `tmfs://git/status/<root>`, `tmfs://git/log/<root>` | repo status and log | version-git.scm |
* A generated page is opened with `(revert-buffer-revert "tmfs://...")`,
  after `(cursor-history-add (cursor-path))` so that "back" works. To refresh
  a page, revert it again.
* **Wrapped URLs** (`url-wrap`, the `wrap` action) let one tmfs URL stand
  for a real file. version-tmfs.scm forwards every `version-*` call on a
  wrapped URL to the underlying file (`version-tool` returns `"wrap"`). The
  `part` class (`part/part-tmfs.scm`, for documents split into parts) also
  gets special cases.
* Markup builders for generated pages (`$generic`, `$tmfs-title`, `$link`,
  `$description-long`, `$describe-item`, `$for`, `$when`, `$with`,
  `$inline`, `$verbatim`) come from `kernel/gui/gui-markup.scm`, where
  `$`-prefixed macros build strees.
* Encoding: TeXmacs strings are "cork" encoded internally, while git output
  is UTF-8, so wrap displayed text in `utf8->cork`. Text sent to git must go
  through `cork->utf8`. Several places in the current code forget this.

## 3. Buffers, saving and reverting

`texmacs/texmacs/tm-files.scm`

* A buffer is identified by its URL (`current-buffer`). Useful functions:
  `buffer-modified?`, `buffer-tree`, `revert-buffer`, `switch-to-buffer`,
  `buffer-set-master`, `buffer->windows` and `url-last-modified`.
* `save-buffer name . opts` runs a chain: check permissions, then
  `save-buffer-check-faithful`, then `save-buffer-save`, then
  `save-buffer-post name opts`. `save-buffer-post` is **the only built-in VCS
  hook**:
  ```scheme
  (define (save-buffer-post name opts)
    (cond ((in? :update opts) (update-buffer name))
          ((in? :commit opts) (commit-buffer name))))
  ```
  "Update" and "Commit" in the Version menu call
  `(save-buffer name :update)` and `(save-buffer name :commit)`, so the file
  is always saved before the VCS operation. Any new operation that touches
  the working tree, such as pull, checkout or restore, should follow the same
  pattern: save first, act, then `revert-buffer` every open buffer whose
  file changed on disk.
* After a VCS command rewrites a file, the buffer has to be reverted by hand.
  `update-buffer` compares `url-last-modified` before and after. There is no
  automatic file watcher.
* TeXmacs formats: `.tm` (native, text-based, line-oriented enough for
  `git diff` to be readable), `.tmml`, `.stm`, `.ts` (styles), `.tp`
  (projects). The `format` tmfs handler of `revision` returns the format of
  the underlying file, so older revisions of `.tex` or `.scm` files load
  with the right converter.

## 4. Running external commands

C++ in `System/Misc/sys_utils.cpp`, glue in `Scheme/Glue/build-glue-basic.scm`
(lines 63–76).

| Scheme | C++ | Notes |
|--------|-----|-------|
| `(eval-system cmd)` | `eval_system` | Runs `cmd` **through the shell** and returns **stdout only**. The exit code and stderr are dropped. Used everywhere in version-git.scm today. |
| `(var-eval-system cmd)` | `var_eval_system` | Same, with trailing newlines removed. |
| `(evaluate-system argv fd-in in fd-out)` | `evaluate_system` | Runs **argv without a shell** and returns `("exit-code" out1 out2 ...)`. Example from `security/gpg/gpg-base.scm`: `(evaluate-system (list exe "--list-keys") '() '() '(1 2))` returns `(code stdout stderr)`. Input can be fed on fd 0: `'(0) (list text)`. |
| `(system cmd)`, `system-1`, `system-2` | `system` | Fire and forget. |

**All of these are synchronous and block the GUI.** `git fetch`, `git push`
and `git pull` over a network, or `git log` on a large repo, will freeze the
editor. Options:

1. Short local commands (status, add, commit, show, log with a limit) can
   stay synchronous through `evaluate-system`.
2. Network commands need an asynchronous path. TeXmacs already talks to
   external processes asynchronously through **pipe links**, the plugin
   mechanism (`System/Link/pipe_link.cpp`, and `Plugins/Qt/qt_pipe_link.cpp`
   with `QTMPipeLink`), and polls them from the event loop. A
   small C++ "background job" (spawn argv, collect stdout and stderr, then
   invoke a Scheme callback when it finishes) could be built on those
   classes. `exec-delayed` (`kernel/library/tree.scm`) and `delayed` are the
   Scheme-side ways to schedule work later.
3. A cheap stopgap is to run the command with `GIT_TERMINAL_PROMPT=0` so it
   fails fast rather than waiting for a password, and to show a "working…"
   message first.

Credentials: git must never prompt on a TTY that TeXmacs doesn't have. Set
`GIT_TERMINAL_PROMPT=0` and rely on the user's credential helper or ssh-agent.
`GIT_ASKPASS` could point to a small TeXmacs-provided helper later.

Windows: `url->system` gives native paths, and `eval-system` goes through
`windows_system`. Prefer argv (`evaluate-system`) so no quoting is needed.

## 5. Menus

`kernel/gui/menu-define.scm`, `menu-widget.scm`

* `(menu-bind name body...)` with the entries `("Label" action)`,
  `(-> "Submenu" ...)`, `(link other-menu)`, `---`, `(group "Title")`,
  `(when cond ...)` (greyed out when false), `(assuming cond ...)`
  (hidden when false), `(for (x list) ...)`, and `((eval name) action)` for
  computed labels.
* Menus are **re-evaluated each time they open**. Every `version-status` call
  in `version-menu.scm` runs `git status` again, several times per menu
  opening. The results need a short-lived cache, keyed by repo root and
  invalidated after any VCS command or save.

## 6. Widgets, dialogs and side tools

`kernel/gui/menu-widget.scm`; examples in `security/gpg/gpg-widgets.scm`,
`database/db-widgets.scm`, `client/client-widgets.scm`

* `(tm-widget (name args) body)` defines a widget with `hlist`, `vlist`,
  `text`, `input`, `enum`, `toggle`, `choice`/`choices` (list selection),
  `scrollable`, `refreshable "id"` + `(refresh-now "id")`,
  `explicit-buttons`, `bottom-buttons`, and so on.
* Display it with `(dialogue-window widget callback "Title")` (modal-ish,
  callback on close) or `(top-window widget "Title")`.
* **Side tools**: `(tm-tool* (my-tool win ...) ...)` defines a panel that can
  be docked with `(tool-select :right (list 'my-tool ...))`,
  `tool-toggle` or `tool-close`. `(side-tools?)` tells whether the user has
  side tools enabled, and code usually falls back to a window otherwise (see
  `open-identities` in db-widgets.scm). **This is the natural home for a
  "Git panel"** showing status and staging next to the document.
* Interactive prompts: `(interactive (lambda (message) ...))` asks for its
  arguments in the footer or a small dialog. `commit-buffer` uses this for
  the commit message. `user-confirm "question" default callback` asks
  yes/no.
* `set-message msg help` shows text in the status bar. It is the only error
  feedback the current VCS code gives.

## 7. The document comparison engine

`version/version-compare.scm`, `version/version-edit.scm`, `version/version-drd.scm`

* Everything is in Scheme and works on strees. `compare-versions t1 t2`
  returns a merged tree in which differences are marked up as
  `(version-both old new)`. `version-old` and `version-new` show a single
  side, and `version-suppressed` stands for an empty side.
* The granularity preference `"versioning grain"` is `detailed`, `block` or
  `rough`.
* `compare-with-older url` loads `url` with `tree-load-inclusion`, diffs it
  against the current buffer, and **replaces the buffer tree** with the
  result, which leaves the buffer modified. `compare-with-newer*` swaps the
  roles. The user then steps through the changes with
  `version-next-difference` and accepts or rejects each one with
  `version-retain 0|1|'current`.
* Because revision URLs are ordinary loadable URLs
  (`tmfs://revision/<rev>/<file>`), comparing with any git revision is just
  `(compare-with-older (version-revision-url file rev))`. Comparing with
  HEAD, the index, a branch or a stash is the same call with another rev
  string, provided `version-revision` understands it.
* The engine is a 2-way diff. A 3-way *merge* of `.tm` files (base, ours,
  theirs), which git needs for conflict resolution, does not exist yet. It
  can be approximated by showing `compare-versions ours theirs` and letting
  the user retain each side. A real 3-way merge would take the `base` into
  account to auto-accept one-sided changes.

## 8. Other VCS-aware code

* `client/client-tmfs.scm:338` overrides `versioned?` and `version-status`
  for remote files on a TeXmacs server (`remote-file?`). Any new generic
  predicates need fallbacks that keep working there.
* `database/db-version.scm` is unrelated. It versions database entries, not
  files.
* `generic/document-menu.scm` has `project-file-list` and
  `project-list-menu`. TeXmacs "projects" (a master `.tm` or `.tp` file with
  included parts) are the natural unit for "commit the project".
