# Zotero as a source of references in TeXmacs: design

Branch `wip_zotero`. This document extends the first version (commit
6053fe6aa5: search dialog, *Update from Zotero*). It designs "option 1":
Zotero becomes a **live, read-only source** for every place where TeXmacs
looks for references. Nothing is copied into the TeXmacs database, so
Zotero stays the only source of truth.

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
2. **No silent copies into the user's database.** Entries that come from
   Zotero are not saved into the default TeXmacs database, unless the
   user asks for it.
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

**One search for both modes.**
- `open-zotero-search` (the first version) becomes a **combined search**:
  Zotero, plus the document's own sources (the `.bib` file in file mode;
  the local entries and the default database in database mode).
- Each line shows its source: `[Z]` Zotero, `[F]` file, `[D]` database,
  `[L]` local entry. Duplicate keys are merged when they are the same work
  (same DOI, or same title and year), and shown as a collision otherwise.
- **In database mode**, the alternate key in a citation
  (`focus-open-search-tool`) keeps the database's own search window. A
  hook (`db-search-results` for kind "bib") appends the Zotero results,
  formatted with the same `db-pretty` style, under a heading *"From
  Zotero"*. These results are built from the exported BibTeX of the
  matching items (`bib->db`), so they look like the others.
- **Search as you type** reuses the database window's 200 ms delay: one
  request to Zotero per pause, not per key.

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
3. **Zotero**;
4. the default TeXmacs database (database mode);
5. the attached entries (the document's own snapshot, used when Zotero is
   unavailable).

Rationale:
- Things the user wrote for this document come first.
- Zotero comes before the general database, because it is the source of
  truth and the database may hold stale copies (§3.7).
- The attachments only serve when nothing else answers.

A preference, "zotero precedence" = `before-database` (default) or
`after-database`, covers users whose TeXmacs database is primary.

### 3.5 Generating the bibliography

**Database mode:**
- `bib-retrieve-entries` gets a new source, `:zotero`, inserted in
  `bib-compile` and `bib-attach` at the place given by §3.4.
- For the remaining keys, it asks Zotero for the items: one search per key
  (exact match on `citationKey`), then **one** batch export by item keys,
  converted with `bibtex->texmacs` and `bib->db`. The result is cached in
  memory per library version.
- `bib-attach` then attaches them to the document as usual (principle 3).
- **Auto import:** entries from Zotero are marked with a field
  `zotero-item` (the item key) and `zotero-library`. `notify-set-attachment`
  skips the marked entries, so they don't enter the default database
  (principle 2). A preference, "zotero import into database", off by
  default, lets them in. They are then saved with contributor "Zotero", so
  that the versioning (`db-import-entry`) treats a later change in Zotero
  as a new version of the same entry, not as a duplicate of the user's
  own entry.

**File mode:**
- The C++ path only reads a `.bib` file, so Zotero is reached through a
  **Zotero-managed `.bib` file**: a file whose first line is the marker
  `% Exported from Zotero by TeXmacs` (as written by the first version).
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

### 3.7 Stale copies

The default TeXmacs database may hold copies of Zotero items: imported
by hand, or by auto import before this design. Precedence (§3.4) puts
Zotero first, so the copies are shadowed.

*Check against Zotero* (§3.6) can also list the database entries whose
`name` exists in Zotero with different fields, and offer to:
- mark them as superseded (with the versioning of the database), or
- leave them.

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

## 4. User interface

**Insert → Citation:**
- *From Zotero…*, the combined search (§3.3). Its title says which
  sources are active: "Search references (Zotero, refs.bib)".

**Document → Bibliography:**
- *Update from Zotero* (explicit refresh of the managed file / attachments);
- *Check against Zotero* (§3.6, §3.7);
- *Zotero settings…* (server, libraries, precedence, import into database,
  export format), also reachable from the database preferences.

**In a citation:**
- Tab completes (§3.3); the alternate key opens the search window.
- In database mode, it is the database's window with the Zotero section.
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
| 3 | Database mode: the `:zotero` source in `bib-retrieve-entries`, `bib-compile` and `bib-attach`; the `zotero-item` mark, and its exclusion from auto import. | `bib-manage.scm` (small hooks), `zotero-db.scm` (new) |
| 4 | File mode: managed `.bib` files, refreshed by `update-document`, with a fallback message; the proposal to insert a bibliography. | `zotero.scm` |
| 5 | Completion in both modes (overrides of `kbd-variant`), and the Zotero section in the database's search window. | `zotero-db.scm`, `bib-kbd.scm` hook |
| 6 | Combined search dialog with sources and collisions; *Show in Zotero*. | `zotero-widgets.scm` |
| 7 | Renamed and deleted keys: check, message, *Update the citations*. | `zotero.scm`, `zotero-widgets.scm` |
| 8 | Projects (glue to read `aux[bib]`), group libraries, settings dialog. | glue, `zotero-widgets.scm` |
| 9 | User manual page; tests for each step with the fake Zotero of `zotero-test.scm`, in both modes. | `doc/main/...`, `check/zotero-test.scm` |

Steps 1–5 give option 1 its substance. Steps 6–9 are refinements that can
follow in any order.

## 6. Open questions

1. **Precedence default.** Is Zotero-before-database right for most users,
   or should the user's TeXmacs database win by default?
2. **Derived keys** `zotero:<itemKey>` for libraries without Better
   BibTeX. Is the colon acceptable in all bibliography styles, LaTeX export
   included? The alternative is a key generated like Better BibTeX does
   (author + year + title word), which is readable but can collide.
3. **Managed `.bib` with all cited items, or with all items of a Zotero
   collection?** The first is small; the second suits users who organize
   a paper as a collection. This could be an option per document.
4. **Writing to Zotero** (adding an item from a DOI typed in TeXmacs) is
   out of scope. The local API supports writes since Zotero 10, but they
   need an authorization dialog in Zotero.
