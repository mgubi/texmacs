# Zotero as a source of references in TeXmacs: design

Branch `wip_zotero`. This document extends the first version (commit
6053fe6aa5: search dialog, *Update from Zotero*). It designs "option 1":
Zotero becomes a **live, read-only source** for every place where TeXmacs
looks for references.

**Decisions** (2026-10-05):
- The TeXmacs database wins over Zotero when both have a key.
- Items imported from Zotero into the TeXmacs database are **kept in sync**
  with Zotero, from Zotero to TeXmacs only (§3.11).
- A managed `.bib` holds only the items which TeXmacs asked Zotero for, for
  this document, never a whole library or collection.
- TeXmacs only reads from Zotero. Writing (for instance adding an item from
  a DOI) may come later, through the authorized write requests of the local
  API (Zotero 10).
- Items without a citation key get the key `zotero:<itemKey>` (§3.2).
- An entry changed both in TeXmacs and in Zotero is resolved field by
  field (§3.11).

## 1. What TeXmacs does today (facts the design relies on)

There are two modes, chosen by the preference "database tool".

**File mode** (the default):
- The `bibliography` tag names a `.bib` file, relative to the document.
- Generation is in C++ (`edit_process.cpp`, `generate_bibliography`). It
  reads the keys collected while typesetting (`aux[bib]`), then the
  `.bib` file, then TeXmacs's own `texmacs.bib`.
- Tab inside a citation completes from that `.bib` file
  (`bib-complete.scm`, `citekey-completions`).
- There is no search window.

**Database mode:**
- Generation calls `bib-compile` (`bib-manage.scm`).
- `bib-compile` looks the keys up, in order, in these sources:
  1. `:local`: the entries written in the document.
  2. The bibliography's `.bib` file, imported into a cached `.tmdb` by
     `bib-cache-database`.
  3. `:default`: the user's database `(bib-database)`.
  4. `:attached`: the entries attached to the document.
  The first source that has a key wins.
- `bib-attach` then attaches the retrieved entries to the document, so
  that the document is self-contained.
- The preference **"auto bib import"** is on by default. When entries are
  attached (`notify-set-attachment`), it saves them into the user's
  default database.
- The database is versioned (`db-import-entry`):
  - an entry is identified by `name` (the key) and `contributor`;
  - an exact duplicate is not imported again;
  - a different version of the same key and contributor supersedes the old
    one, unless the old one was edited by hand (`modus manual`).
- Tab completes from the default database (`bib-kbd.scm`,
  `index-get-name-completions`). The alternate key (`focus-open-search-tool`)
  opens the search window, `open-db-chooser`.
- The search window queries one database: `db-search-results`, with
  `:completes`, 20 results, and the results pretty-printed in a style.

**Zotero's local API** (measured with a library of 4000 items):
- The search `q=` matches authors, titles and years, and also *prefixes of
  citation keys*, which is what completion needs.
- A search takes about 20 ms. An export of up to 50 items takes 40 ms. A
  request to a Zotero that is not running fails at once.
- The only top-level items without a citation key are stand-alone
  attachments, which aren't references.
- Responses carry `Last-Modified-Version`, the library's version. It
  changes with any edit, so caches can be checked cheaply (`since=`).

## 2. Principles

1. **Zotero is read-only.** TeXmacs never writes to the Zotero library.
2. **Copies are kept honest.** Entries that come from Zotero into the
   TeXmacs database, whether imported explicitly or by "auto bib import",
   are marked as coming from Zotero and kept in sync with it (§3.11). A
   copy edited by hand in TeXmacs is never overwritten silently.
3. **Documents stay self-contained.** A document whose references came
   from Zotero still typesets on a machine without Zotero, or for a
   coauthor:
   - in file mode, through the exported `.bib` next to it;
   - in database mode, through the attached entries.
4. **Precedence is explicit and predictable**, the same in both modes, and
   collisions are shown to the user, never resolved silently.
5. **No waiting for Zotero.**
   - Interactive requests have a short timeout.
   - When Zotero has just failed, it is not asked again for a while.
   - Everything works, with what is available, when Zotero is closed.

## 3. The situations

### 3.1 State of Zotero

| State | Detection | Behaviour |
|---|---|---|
| Ready | `200` | Live search, completion, generation. |
| Running, local API disabled | `403` | One explanatory message, linking to the Zotero setting. No retry until the user retries or 60 s have passed. |
| Not running | connection refused (status 0, at once) | Search and completion fall back to the other sources, with a discreet note "Zotero is not running" in the search window. Generation uses the last export or the attachments (§3.5). |
| Hanging / very slow | timeout | Interactive requests (completion, search as you type): timeout 1.5 s. Generation: timeout 20 s. After a timeout, Zotero counts as unavailable for 30 s (circuit breaker), so typing never waits more than once. |
| Not installed | — | Same as not running. The Zotero entries of the menus stay visible, so that the feature can be discovered. Their message says what to install. |
| Elsewhere | preference `"zotero server"` | Zotero on another machine (for instance through an ssh tunnel). Nothing changes otherwise. |

The state is cached for a few seconds (`zotero-status`, with a time), so
menus and completion don't ask for it at each keystroke.

### 3.2 Keys

A citation key may be:

| Case | Example | Resolution |
|---|---|---|
| Only in Zotero | the usual case | Taken from Zotero. |
| Only in TeXmacs (local entries, `.bib`, default database) | older documents | Taken from there, as today. |
| In both, same work | imported earlier from Zotero into the database | Same result. The search window shows one line, marked as being in both. |
| In both, **different works** | `smith2020` for two different papers | The precedence (§3.4) decides. A warning, *"smith2020 is defined both by Zotero and by <source>"*, appears in the search window and once in the footer at generation. |
| Nowhere | typo, deleted item | As today: `[?]`, and the generation message lists the missing keys. |
| Renamed in Zotero | Better BibTeX regenerates keys when the title changes, unless they are pinned | See §3.6. |
| Several Zotero items with the same key | possible with hand-edited keys | The first is taken, with a warning naming both items. |

Better BibTeX and Zotero 7's own `citationKey` field put the key in the
same field, so both work. Without Better BibTeX, Zotero leaves the field
empty unless the user fills it. The items without a key are then offered
with a **derived key**, `zotero:<itemKey>`: stable, unique, valid in
`\cite`. The exported entry is rewritten to that key, since Zotero's
BibTeX export would otherwise invent its own. The search window shows
such keys, so users see that they can set a better one in Zotero.

### 3.3 Search and completion

**One search window for both modes** (revised after trying it).
- The search window of citations (`open-db-chooser`, opened by
  *Focus -> Search references* and the alternate Tab) is the only search.
  The first version had a separate dialog (*From Zotero...*), then a
  combined search dialog with merged lines; both were removed, since the
  window of the database already did most of it.
- **With the database**, it lists the entries of the database, then the
  Zotero items which the database lacks (`db-search-results` for kind
  "bib"). **Without it**, the window opens on the marker `:bib-file`, and
  lists the entries of the `.bib` file of the bibliography (unless it is
  managed: its items are Zotero's), then the Zotero items.
- Each reference says its source: *Database*, *Database, from Zotero*,
  the name of the `.bib` file, *Zotero* or *Zotero, <group>*. A line at
  the top names the sources and why Zotero is left out, when it is.
- The Zotero results are built from the exported BibTeX of the matching
  items (`bib->db`), and formatted with the same `db-pretty` style.
- **Search as you type** reuses the window's 200 ms delay: one request to
  Zotero per pause, not per key.

**Completion (Tab in a citation)**:
- The candidates are the keys from the usual source (the `.bib` file in
  file mode, the default database in database mode), plus the Zotero keys
  with the typed prefix (`q=<prefix>`, filtered on the key's prefix).
- Request timeout: 1.5 s. If Zotero is unavailable, only the usual source
  is used, without a message.
- The Zotero answers are cached for the session, per prefix and library
  version.

### 3.4 Precedence

One ordered list of sources, the same for search, completion and
generation:

1. local entries of the document (database mode);
2. the bibliography's `.bib` file **if it is not managed by Zotero** (§3.5);
3. the default TeXmacs database (database mode);
4. **Zotero**;
5. the attached entries (the document's own snapshot, used when Zotero is
   unavailable).

Rationale:
- Things the user wrote for this document come first.
- The TeXmacs database wins over Zotero (decision). It may hold copies of
  Zotero items, which the sync of §3.11 keeps up to date. Its other
  entries are the user's own, and are the user's choice.
- The attachments only serve when nothing else answers.

### 3.5 Generating the bibliography

**Database mode:**
- `bib-retrieve-entries` gets a new source, `:zotero`, inserted in
  `bib-compile` and `bib-attach` at the place given by §3.4.
- For the remaining keys, it asks Zotero for the items: one search per key
  (exact match on `citationKey`), then **one** batch export by item keys,
  converted with `bibtex->texmacs` and `bib->db`. The result is cached in
  memory per library version.
- `bib-attach` then attaches them to the document as usual (principle 3).
- **Auto import:** entries from Zotero are marked with the fields
  `zotero-item` (the item key), `zotero-library` and `zotero-version` (the
  version of the item). When "auto bib import" saves them into the default
  database, they keep these marks and the contributor "Zotero". The
  versioning (`db-import-entry`) then treats a later change in Zotero as a
  new version of the same entry, not as a duplicate of the user's own
  entry, and the sync of §3.11 finds them.

**File mode:**
- The C++ path only reads a `.bib` file, so Zotero is reached through a
  **Zotero-managed `.bib` file**: a file whose first line is the marker
  `% Exported from Zotero by TeXmacs` (as written by the first version).
- It holds exactly the items which TeXmacs asked Zotero for, for this
  document: the cited keys which no earlier source (§3.4) resolves. A key
  cited and then removed leaves the file at the next refresh.
- *Document → Update → Bibliography* and *Update → All* first refresh a
  Zotero-managed file (an override of `update-document`), then generate as
  usual.
- If Zotero is unavailable, the existing file is used as it is, with a
  message "Zotero is not running: the bibliography was made with the
  references exported on <date>".
- A file without the marker is the user's own file and is never
  rewritten.
- *Update from Zotero* (first version) stays as the explicit command. If
  the document has no bibliography yet, it proposes to insert one with a
  managed file `<document>-zotero.bib`, instead of failing.
- The managed file only contains the cited items, which keeps it small
  and readable for coauthors (principle 3).

**Both modes:** the generation message reports the keys found in Zotero,
those found elsewhere, the collisions and the missing ones, in one line,
with a *Details* button that lists them.

### 3.6 Keys renamed or items deleted in Zotero

Keys change when Better BibTeX regenerates them, for instance after a
correction of the title or the year. A citation then points to nothing.

- **Record the Zotero item with each key.** Whenever TeXmacs resolves a
  key through Zotero, it records the pair (key → item key) in a document
  attachment, `zotero-items`. The attachment is small and travels with the
  document.
- **At generation**, a key that is missing in Zotero but recorded is
  looked up by its item key (`items/<itemKey>`).
  - If the item now has another key, the message says *"smith2020 is now
    smith2020gravity in Zotero"* and offers **Update the citations**. That
    command renames the key in every citation of the document (or of the
    project), with undo.
  - If the item is gone (404), the message says so. The attached entry or
    the managed `.bib` still provides the reference until the user acts.
- The same check is available on demand, *Bibliography → Check against
  Zotero*.

### 3.7 Copies imported before this design

The default TeXmacs database may hold copies of Zotero items without the
`zotero-item` mark: imported by hand from an exported `.bib`, or by auto
import with the first version of the branch. The database wins (§3.4),
so these copies are used, and the sync (§3.11) cannot see them.

*Check against Zotero* (§3.6) finds them: database entries whose `name`
is also a citation key in Zotero. For each one, it offers to:
- **adopt it**: mark it with its Zotero item, so that it is synced from
  then on. When its fields differ from Zotero's, the report shows the
  difference, and the user chooses which version to keep;
- **leave it** as the user's own entry, never synced. A collision warning
  is shown when Zotero has the key for a different work (§3.2).

Nothing is deleted.

### 3.8 Projects, includes and several bibliographies

- **Projects:** citations are collected over the whole project (as the C++
  side uses `buf->prj->data->aux`). The command `zotero-citations` walks
  the master document and its included files, or reads the `aux[bib]` of
  the project directly. The second is better, since it follows what
  typesetting saw. A glue function to read the aux is needed (§5).
- **Several bibliographies** (bib prefixes, such as `bib-prefix`): each
  `bibliography` tag has its own aux and file. They are refreshed one by
  one.

### 3.9 Other libraries

- The local API also serves group libraries (`groups/<id>/`). A
  preference lists the libraries to search: by default "My Library" only,
  optionally all groups. The source tags then name the library.
- Keys are unique per library only. Precedence across libraries follows
  their order in the preference, and collisions are reported as in §3.2.

### 3.10 Encoding and formats

- Requests: utf8, percent-encoded. Answers: utf8, converted to cork for
  display and insertion. A key with non-ASCII characters is kept as it is.
- The export format is `bibtex` by default, the fields TeXmacs's styles
  know. `biblatex` is available through the preference "zotero export
  format".
- LaTeX escapes produced by Zotero (`{\'e}`) are decoded by TeXmacs's
  BibTeX parser, as for any `.bib` file.

### 3.11 Keeping imported items in sync

The entries of the TeXmacs database marked with `zotero-item` (§3.5) are
copies of Zotero items. Since the database wins (§3.4), they must follow
Zotero, from Zotero to TeXmacs only.

**When.**
- At the start of a session, once Zotero is reachable.
- Before generating a bibliography in database mode.
- On demand, *Bibliography → Synchronize with Zotero*.
- Never while typing.

**How**, with what the local API offers (checked on Zotero 10.0.4):
1. Compare the library version (`Last-Modified-Version`) with the version
   of the last sync, kept in the database. If they are equal, stop: this
   costs one request.
2. Otherwise, ask for the versions of the imported items, 50 item keys per
   request (`items?itemKey=…&format=versions`, 20-40 ms each).
3. **An item with a newer version** than its `zotero-version`:
   - export it again (one batch export for all of them);
   - save it as a new version of the entry (contributor "Zotero"), which
     supersedes the previous one in the database's own history, so the old
     version stays available.
4. **An item no longer returned** has been deleted, or moved to the trash,
   in Zotero. The local API has no `/deleted` endpoint, so absence is the
   signal.
   - The TeXmacs entry is kept, marked `zotero-deleted`, and listed in the
     report.
   - It still resolves citations, and the user decides whether to remove
     it.
5. **A key changed in Zotero** (Better BibTeX regenerated it): the entry
   keeps its old `name`, so existing citations still work. The report
   offers to rename the entry and the citations of the open documents
   (§3.6).
6. **A copy edited by hand in TeXmacs** (`modus manual` in the database)
   whose Zotero item also changed:
   - the TeXmacs edit is kept, since the database wins;
   - the report says "changed in Zotero too" and opens a **field-by-field
     comparison**:
     - one line per field which differs (title, authors, year, journal,
       pages, DOI, ...), with the TeXmacs value, the Zotero value and, when
       known, the value at the last sync;
     - for each field, the user keeps one value;
     - fields changed only in Zotero are taken from Zotero by default;
       fields changed only in TeXmacs are kept by default;
     - the result is saved as a new version of the entry, still marked
       `modus manual`, with the new `zotero-version`, so that it isn't
       reported again until Zotero changes the item once more.
   - The comparison uses the document comparison of TeXmacs
     (`compare-versions`) on the values of each field, so that a small
     change inside a long title shows as such.
7. Record the library version as the version of this sync.

**Cost.** Your library, with no change since the last sync: one request.
With changes: a few requests, depending on how many of the imported items
changed.

**Report.** One footer message ("Zotero: 3 references updated, 1 deleted
in Zotero"), with *Details* listing them. When nothing changed, there is
no message.

**Import.** Besides auto import, the search dialog gets an **Import into
database** button, next to *Cite*. It imports the selected items, marked as
in §3.5, and syncs them from then on.

## 4. User interface

**Document → Bibliography:**
- *Update from Zotero* (explicit refresh of the managed file / attachments);
- *Synchronize with Zotero* (§3.11);
- *Check against Zotero* (§3.6, §3.7);
- *Zotero settings…* (server, libraries, export format, completion,
  Zotero in the search window, connection test).

**In a citation:**
- Tab completes (§3.3); the alternate key opens the search window.
- It is the window of references of §3.3, in both modes (*Focus ->
  Search references*).
- The focus bar of a citation shows a small `[Z]` when its key comes from
  Zotero. Clicking it offers **Show in Zotero**, which opens
  `zotero://select/library/items/<itemKey>`.

**Messages:**
- Problems with Zotero itself (not running, local API disabled) are
  shown once in the footer, then silently remembered (§3.1).
- Resolution problems (collisions, renamed keys, missing keys) are part of
  the generation message.

## 5. Implementation plan

| Step | Content | Touches |
|---|---|---|
| 1 | Status cache, short/long timeouts, circuit breaker; derived keys `zotero:<itemKey>` and rewriting of exported keys. | `zotero.scm` |
| 2 | Resolution layer: `zotero-resolve keys` gives (key item-key entry) per key, with an in-memory cache per library version. It also records the pairs in the `zotero-items` attachment. | `zotero.scm` |
| 3 | Database mode: the `:zotero` source in `bib-retrieve-entries`, `bib-compile` and `bib-attach`, after `:default`; the marks `zotero-item`, `zotero-library`, `zotero-version` and contributor "Zotero" on the entries which come from Zotero. | `bib-manage.scm` (small hooks), `zotero-db.scm` (new) |
| 3b | Sync of the imported items (§3.11): library version check, item versions, re-export, deleted and renamed items, hand-edited copies; *Import into database*; the report. | `zotero-db.scm` |
| 4 | File mode: managed `.bib` files, refreshed by `update-document`, with a fallback message; the proposal to insert a bibliography. | `zotero.scm` |
| 5 | Completion in both modes (overrides of `kbd-variant`), and the Zotero section in the database's search window. | `zotero-db.scm`, `bib-kbd.scm` hook |
| 6 | Combined search dialog with sources and collisions; *Show in Zotero*. | `zotero-widgets.scm` |
| 7 | Renamed and deleted keys: check, message, *Update the citations*. | `zotero.scm`, `zotero-widgets.scm` |
| 8 | Projects (glue to read `aux[bib]`), group libraries, settings dialog. | glue, `zotero-widgets.scm` |
| 9 | User manual page; tests for each step with the fake Zotero of `zotero-test.scm`, in both modes. | `doc/main/...`, `check/zotero-test.scm` |

Steps 1–5 give option 1 its substance. Steps 6–9 are refinements that can
follow in any order.

**Status (2026-10-05).**
- **Done:** all steps, 1 to 9. The suite `zotero`
  (`check/zotero-test.scm`, 243 checks, with a fake Zotero which also has a
  group library) covers them, in both modes. The manual page is
  `doc/main/links/man-zotero.en.tm`, linked from the "Links" chapter.
- Steps 6 to 9, as built:
  - **Groups:** requests name their library (`users/0`, `groups/<id>`).
    The preference "zotero libraries" is "user" (default) or "all"; the
    library of the user comes first, so its keys win. Keys derived in a
    group are `zotero:g<id>:<itemKey>`. Database entries record their
    library in `zotero-library` (the first imports said "user", read as
    `users/0`), and the sync works library by library, even for groups
    which TeXmacs no longer searches. The sync version is the list of the
    versions of the libraries involved.
  - **Show in Zotero:** `zotero://select/library/items/<key>` (or
    `groups/<id>/items/<key>`), from the focus menu and focus bar of a
    citation. The menus only use what TeXmacs already
    knows (the keys found before, the `zotero-items` attachment): they
    never wait for Zotero.
  - **Renamed and deleted keys:** the `zotero-items` attachment holds
    `(key item library)`; it is written in both modes (the `:zotero`
    source records too). A missing key whose item now has another key is
    exported under its old key, in the managed file and by the `:zotero`
    source; the entry of a deleted item is kept from the previous managed
    file. *Update the citations* renames the citations of the document or
    project, and the database entry renamed by the sync (a new version
    with the new name). *Check against Zotero...* reports the keys found,
    renamed, deleted, missing, the collisions, and the unmarked copies in
    the database, which it can adopt (marked and synced; those whose
    fields differ become manual, and go through the field-by-field dialog,
    TeXmacs's values by default).
  - **Projects:** the citations are those of the master and of the files
    it includes, recursively (a walk of the `include` tags, not the aux:
    no glue was needed); the bibliography file is the master's. The
    renaming changes the open files (marked modified) and saves those
    which were not open, as the message says.
  - **Settings:** *Document -> Bibliography -> Zotero settings...*: server,
    libraries, export format, completion, and a connection test.
  - **One search window (after trying it, 2026-10-05):** the separate
    dialog of the combined search (*Insert -> Citation -> From Zotero...*)
    was removed. The search window of citations (*Focus -> Search
    references*, the alternate Tab) does it, in both modes: with the
    database, the database then Zotero; without it, the BibTeX file of the
    bibliography then Zotero (`zotero-file-search-results`, through the
    database marker `:bib-file` of `open-db-chooser`). Each reference says
    its source (Database, Database from Zotero, the file name, Zotero and
    the group). The preference "zotero in database search" leaves Zotero
    out. In database mode, *Update from Zotero* adds a bibliography
    without file: its references are kept in the document.
  - **After trying it, second round (2026-10-05):**
    - *The user's BibTeX file* (revises §3.5 and principle "a file without
      the marker is never written"): without the database, the references
      of Zotero which the user's `.bib` lacks are appended to it, at
      *Document -> Update -> Bibliography* and *Update from Zotero*, each
      after a line `% Added from Zotero by TeXmacs on <date>:
      zotero://select/...` (`zotero-add-to-bib-file`). Nothing else of
      the file changes, and an added reference is not updated later (the
      user may edit it). The comments let TeXmacs find the item again
      (`zotero-bib-file-items`, used by `zotero-check-missing` for renamed
      keys) even without the `zotero-items` attachment. Preference "zotero
      add to bib file" (on).
    - *Same work*: the same DOI when both summaries have one (normalized:
      lowercase, without `https://doi.org/` or `doi:`), otherwise the same
      title and year. The items now carry their DOI (`zotero-entry-doi`).
    - *Completion cache*: the keys of each prefix are remembered while no
      library changes; a longer prefix is answered from a shorter one whose
      answer was complete.
    - *Import by hand*: *Document -> Bibliography -> Import the citations
      into the database* and *Focus -> Import into the database*.
    - *Dates*: `pretty-date` knows no ISO format (Qt patterns, or `date`
      without Qt); `zotero-iso-date` computes `YYYY-MM-DD` (UTC).
    - *Formulas*: Zotero's BibTeX writes the LaTeX of its fields as text
      (`$\Phi^4_3$` -> `\${\textbackslash}{Phi}{\textasciicircum}4\_3\$`);
      `zotero-unescape-math` undoes it between pairs of `\$` (`$$` becomes
      `$`), and drops the field `file` (local paths of the attachments).
      A value which the LaTeX parser of `parse_bib` cannot read made the
      whole file unconverted (#75 item 10): fixed by PR #91, not merged.
    - *Translations*: the menus and dialogs in Italian, French, German,
      Spanish, Portuguese, Dutch, Russian, Chinese and Japanese.
    - *Windows*: Show in Zotero opens the link as `load-external` does
      (`start ""` takes a title first).
    - *Vue*: the search window was checked under Vue (SDL's dummy video
      driver, scripted events and snapshots); a Vue dialog now gives the
      keyboard to its first field, as Qt does (wip_other_guis 8ceec36b23).
  - **Differences from §3.5 and §4:** the footer cannot carry a
    *Details* button, so the generation reports renamed and deleted keys
    in the footer, and *Check against Zotero...* gives the details. The
    database preferences do not link to the Zotero settings.

Implementation notes:
- Zotero's marks (`zotero-*`) are meta attributes of the database
  (`db-meta-attributes`). Two versions of an entry then compare equal when
  only the marks differ, and the marks never reach BibTeX.
- The sync makes the new version with `db-update-entry`, which supersedes
  the old one explicitly. Saving through `bib-save` would let the
  versioning compare dates, with one-second resolution.
- `update-document` calls `zotero-before-update`, a lazy definition, so that
  a managed file is refreshed even before the Zotero modules are loaded.
- The local API returns, for `itemKey=`, the children of the items too
  (attachments, notes); they are ignored.
- The status request also notes the library version, so that the keys
  found before are forgotten when the library changed;
  `zotero-find-key` checks the status before its cache for that reason.
- `buffer-load` returns `#t` when it *fails*; `buffer-pretend-modified`
  only reaches buffers with a view, hence the saving of the project files
  which were not open.
- A tmdb database stays in memory after its file is removed: the test
  groups use a new file each time.
- The suite `zotero` runs after `links` (which crashes after documents were
  opened and closed) and after `database` (whose checks expect the modules
  of the bibliographic database not to be loaded yet).

## 6. Open questions

None at the moment. Settled on 2026-10-05:
- the database wins over Zotero;
- imported items are kept in sync;
- the `.bib` holds only asked items;
- reading only;
- `zotero:<itemKey>` keys;
- field-by-field comparison.

For the field-by-field comparison, sync must remember the values at the
last sync, to tell which side changed a field. They are kept with the
entry (`zotero-synced`, the fields as last exported). This adds one field
per imported entry.
