<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Citations from Zotero>

  <section|Overview>

  <TeXmacs> can take its bibliographic references from the library of the
  Zotero desktop application. Zotero (version 7 or later) serves its
  library at <verbatim|http://localhost:23119/api/>, with the requests of
  the Zotero web <abbr|API> (version 3), once <em|Allow other applications
  on this computer to communicate with Zotero> is enabled in its advanced
  settings. Reading needs no key. <TeXmacs> only reads: it never writes to
  Zotero.

  Zotero is one more source of references, after those of the user:

  <\itemize>
    <item>without the database tool, through a <em|managed> <BibTeX> file,
    which <TeXmacs> writes from Zotero, since the <c++> bibliography
    generator only reads <verbatim|.bib> files;

    <item>with the database tool, through a new source <scm|:zotero> of
    <scm|bib-retrieve-entries>, after the database of the user; the entries
    which enter the database from Zotero are then kept in sync with it.
  </itemize>

  The citations use the citation keys of Zotero (the field
  <verbatim|citationKey>, filled by Zotero or by the Better<nbsp>BibTeX
  extension). The user side is described in <hlink|Citations from
  Zotero|../../main/links/man-zotero.en.tm>; the bibliography machinery
  which this chapter extends in <hlink|Bibliographies|database-bibliography.en.tm>.
  The design, with the situations which it handles and the decisions
  taken, is in <verbatim|doc/zotero-design.md> at the root of the
  repository.

  <section|Modules>

  <\description>
    <item*|<verbatim|bibtex/zotero.scm>>Requests to Zotero, the state of
    Zotero, libraries, items and citation keys, the resolution of keys,
    the export of <BibTeX>, the citations of a document or project, the
    managed <BibTeX> files, renamed keys, completion. It does not load the
    modules of the database, and uses <source-link|convert/bibtex/bibtextm.scm|TeXmacs/progs/convert/bibtex/bibtextm.scm>
    to read <BibTeX> files.

    <item*|<verbatim|bibtex/zotero-db.scm>>Zotero as a source of the
    database: conversion of items into database entries, the source
    <scm|:zotero>, the sync of imported entries, conflicts, adoption of
    copies, renamed entries, and the search window of references (in both
    modes). It uses <source-link|database/db-base.scm|TeXmacs/progs/database/db-base.scm>,
    <source-link|db-convert.scm|TeXmacs/progs/database/db-convert.scm>, <source-link|bib-db.scm|TeXmacs/progs/database/bib-db.scm>,
    <source-link|bib-manage.scm|TeXmacs/progs/database/bib-manage.scm> and <source-link|db-widgets.scm|TeXmacs/progs/database/db-widgets.scm>, so it is
    only loaded when one of its functions is called.

    <item*|<verbatim|bibtex/zotero-widgets.scm>>The dialogs: the window of
    the entries changed on both sides, <menu|Check against Zotero...> and
    the settings.
  </description>

  Everything else reaches these modules through lazy definitions in
  <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> (<scm|lazy-define> for the functions,
  <scm|lazy-menu> for the dialogs), so that nothing is loaded before
  Zotero is used. The hooks in other files are:

  <\description>
    <item*|<source-link|generic/document-edit.scm|TeXmacs/progs/generic/document-edit.scm>><scm|update-document> calls
    <scm|(zotero-before-update <scm-arg|what>)> first, which refreshes a
    managed file and syncs the database.

    <item*|<source-link|database/bib-manage.scm|TeXmacs/progs/database/bib-manage.scm>>The source <scm|:zotero> in
    <scm|bib-retrieve-entries-from-one> and <scm|bib-get-db>, and the
    order of the sources, <scm|bib-sources>, used by
    <scm|bib-compile-sub> and <scm|bib-attach>.

    <item*|<source-link|database/db-format.scm|TeXmacs/progs/database/db-format.scm>>The meta attributes of the
    entries from Zotero, in <scm|db-meta-attributes>.

    <item*|<source-link|database/db-widgets.scm|TeXmacs/progs/database/db-widgets.scm>>The results of Zotero and
    the sources line in the search window of references.

    <item*|<source-link|generic/generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>, <source-link|database/bib-kbd.scm|TeXmacs/progs/database/bib-kbd.scm>>Completion
    of keys with <scm|zotero-completion-suffixes>, and the search window of
    citations without the database tool.

    <item*|<source-link|generic/generic-menu.scm|TeXmacs/progs/generic/generic-menu.scm>, <source-link|generic/document-menu.scm|TeXmacs/progs/generic/document-menu.scm>>The
    entries <menu|Search references> and <menu|Show in Zotero> of the focus
    menus, and the Zotero entries of <menu|Document|Bibliography>.
  </description>

  <section|Talking to Zotero>

  <subsection|Requests>

  <\explain>
    <scm|(zotero-request <scm-arg|path> <scm-arg|interactive?>)><explain-synopsis|ask
    Zotero>
  <|explain>
    Runs <verbatim|curl> on <verbatim|<em|server>/api/<em|path>>, where
    <em|server> is the preference <verbatim|"zotero server">, with the
    header <verbatim|Zotero-API-Version: 3> and a time limit of 1.5 seconds
    for interactive requests (completion, search as you type) and of 20
    seconds otherwise. It returns <scm|(<em|status> <em|body>
    <em|version>)>: the <abbr|HTTP> status, or 0 when Zotero cannot be
    reached; the body, in utf8; the version of the library (the header
    <verbatim|Last-Modified-Version>, read with
    <verbatim|--write-out "%header{last-modified-version}">, which needs
    <verbatim|curl> 7.84), or <scm|#f>. The suite of tests replaces this
    function.
  </explain>

  The paths start with the library: <verbatim|users/0> for the library of
  the user, <verbatim|groups/<em|id>> for a group library. The requests
  used are <verbatim|items/top?format=json&q=...> (search, which also
  matches prefixes of citation keys), <verbatim|items/<em|key>>,
  <verbatim|items?itemKey=...> with the formats <verbatim|json>,
  <verbatim|versions> and <verbatim|bibtex> (or <verbatim|biblatex>), at
  most 50 keys per request, and <verbatim|users/0/groups>.

  <subsection|zotero.org>

  The library may also be read from the web <abbr|API> of
  <verbatim|zotero.org> (<verbatim|https://api.zotero.org/>), with an
  <abbr|API> key of the user. The preference <verbatim|"zotero source">
  chooses: <verbatim|"local"> (the application), <verbatim|"web">, or
  <verbatim|"auto"> (<scm|zotero-web?>: <verbatim|zotero.org> in a web
  browser, where <scm|zotero-in-browser?> holds, the application
  elsewhere). The application cannot be used from a web page: its server
  closes every request which carries an <verbatim|Origin> header (local
  <abbr|API>, connector, Better<nbsp>BibTeX), and the Zotero Connector
  offers nothing to pages.

  The key is kept in the wallet when it is on, else in the preference
  <verbatim|"zotero api key"> (<scm|zotero-api-key>,
  <scm|zotero-set-api-key>). The user it belongs to is asked once
  (<verbatim|keys/current>) and remembered in <verbatim|"zotero user">.
  The library of the user stays <verbatim|users/0> in <TeXmacs> (in the
  documents and the database), and becomes <verbatim|users/<em|id>> in the
  requests. A request without key answers 401 (state <scm|no-key>); a key
  refused gives <scm|forbidden>.

  As for the keys of the AI engines, the key is asked when it is needed
  (<scm|zotero-ask-key>): when the wallet is there but closed, its dialog
  opens first (it may hold the key), else a dialog asks for the key
  (<scm|zotero-key-dialog>); the operation which needed it is then run
  again. The commands ask before they run (<scm|zotero-command>), the
  search window when it opens (<scm|zotero-search-opened>), and
  <scm|update-document> after it ran, only if it needed Zotero
  (<scm|zotero-key-wanted>: <scm|zotero-ready?> notes that the key lacked);
  once declined, the updates do not ask again. A key given while the wallet
  is closed opens it first, to keep the key there.

  The requests are made by <verbatim|curl> on the desktop, with the headers
  in a temporary file (<verbatim|--header @<em|file>>), so that the key is
  never on a command line; in a browser, by a synchronous
  <verbatim|XMLHttpRequest> written in JavaScript and run by
  <scm|web-javascript>, which answers with the status, the header
  <verbatim|Last-Modified-Version> and the body in base64 (decoded by
  <scm|decode-base64>: the body stays the bytes of utf8).
  <verbatim|zotero.org> sends the headers which this needs (CORS).

  The searches of <verbatim|zotero.org> (<verbatim|q=>) only look at the
  titles, the creators and the years, all the words of the query being
  required, also with <verbatim|qmode=everything> (which adds the full
  text, not the other fields): the citation keys, the field
  <verbatim|extra>, the publishers and the <abbr|URL>s are never searched
  (checked on the public library <verbatim|users/475425>). A citation key
  is therefore found by its item when it is known (see <hlink|The items of a
  document|#zotero-items>), and otherwise by the words <verbatim|"<em|author>
  <em|year>"> which start and end a key of Better<nbsp>BibTeX
  (<scm|key-author-year>: <verbatim|barashkovF43MeasureGirsanovs2020>
  gives <verbatim|barashkov 2020>, a letter after the year is skipped),
  keeping the item whose key is exactly the one asked for. The completion
  finds the keys which start with the name of an author for the same
  reason. <menu|Show in Zotero> opens the page of the item on
  <verbatim|zotero.org> (<scm|zotero-web-url>).

  <subsection|Asynchronous requests in a web browser>

  A synchronous <verbatim|XMLHttpRequest> stops the page until
  <verbatim|zotero.org> answers, and a browser allows no time limit for it.
  So the operations which can wait run with a retry:

  <\explain>
    <scm|(zotero-with-retry <scm-arg|retry> <scm-arg|thunk>)><explain-synopsis|run
    with asynchronous requests>
  <|explain>
    Runs <scm-arg|thunk>; in a web browser, its requests are made with
    <verbatim|fetch> (<scm|zotero-start-request>) and answer
    <scm|(pending "" #f)> at once when their answer is not known yet. The
    answers come back through <verbatim|TeXmacs.later> to
    <scm|zotero-async-answer>, into a cache of answers (5 seconds for the
    request of the state, 60 seconds for the others, forgotten when a
    library changed); when nothing is awaited any more, the procedures
    <scm-arg|retry> of the operations which waited are called, and find
    their answers in the cache. A request made without retry is
    synchronous, as on the desktop.
  </explain>

  The search window (which shows its search and its sources again), the
  completion (which completes again if the cursor did not move),
  <scm|zotero-before-update> (which answers <scm|wait>, so that
  <scm|update-document> stops and is called again) and the commands
  (<scm|zotero-command>) use it. An awaited answer is never taken for a
  fact: <scm|zotero-check-missing> sees no deleted item, the sync changes
  nothing, and the caches of keys, completions and groups are not filled
  (<scm|zotero-asking?>). The converted references are kept per item and
  version, so that the bibliography made with the database, which asks
  Zotero while it is made, finds them once <scm|zotero-before-update> has
  asked for them.

  <subsection|Following the requests>

  While answers of <verbatim|zotero.org> are awaited, the footer says what
  is asked (<scm|zotero-progress-message>): the newest request, described
  by <scm|zotero-request-label> from its <abbr|URL> (searching
  <em|query>, fetching, exporting or looking for changes of <em|n>
  references, checking the key, the library or the groups), the number of
  the others, the seconds spent after two seconds, and after ten seconds
  that <verbatim|zotero.org> is slow. A ticker (<scm|delayed> with
  <scm|:pause>) shows it again each second while answers are awaited. When
  the last answer comes, <scm|zotero-answered-message> gives the time
  spent since the first request of the series, or the failure
  (<scm|zotero-status-message> of the first status which was not 200); the
  operations which waited run after it, and may set their own message.

  The footer of a document is not drawn again while a dialog has the
  keyboard focus (<cpp|edit_interface_rep::apply_changes> updates the
  menus and the footer only when <cpp|idle_time> grows, which it does not
  without the focus). The search window therefore shows the progress
  itself: its line of sources gives <scm|zotero-progress-message> while
  answers are awaited (the ticker refreshes it, <scm|refresh-now
  "db-search-sources">), and <scm|zotero-searching-results> adds
  <verbatim|Searching zotero.org...> to the results which wait.
  <scm|zotero-waiting?> tells whether the operation being run (its retry)
  waits for answers: unlike <scm|zotero-asking?>, which is true while any
  answer is awaited, it only holds for results which will be shown again.

  <subsection|State and caches>

  <scm|zotero-status> asks Zotero for one key, and remembers the answer:
  <scm|ready> for 5 seconds, <scm|disabled> (status 403: the local
  <abbr|API> is off) for a minute, <scm|not-running> or <scm|error> for 30
  seconds. While Zotero is known not to answer, no request is made: menus,
  completion and typing never wait for it. <scm|zotero-forget-state>
  forgets the state; the explicit commands call it first.

  The keys found in Zotero are remembered (<scm|zotero-find-key>) while no
  library changes: each answer carries the version of its library, and a
  new version forgets all the keys. The status request notes the version
  of the library of the user, and <scm|zotero-find-key> asks for the
  status before using its cache, so that a key renamed in Zotero is not
  served from the cache for more than a few seconds.

  <subsection|Answers>

  <scm|json-\<gtr\>tree> gives objects as <scm|(attr <em|key> <em|value>
  ...)> and arrays as <scm|(tuple ...)> (<scm|zotero-json>,
  <scm|zotero-attr-ref>); its strings are in utf8, and are converted to
  Cork with <scm|utf8-\<gtr\>cork>. The search and the queries by key also
  return notes, attachments and annotations, which are left out.

  <section|Items, keys and libraries>

  An item is represented by the list

  <\tm-fragment>
    <verbatim|(<em|key> <em|item> <em|title> <em|creators> <em|year>
    <em|version> <em|library>)>
  </tm-fragment>

  (accessors <scm|zotero-entry-key>, <scm|zotero-entry-item>, ...,
  <scm|zotero-entry-library>), in Cork. An item without citation key is
  cited as <verbatim|zotero:<em|item>> in the library of the user, and as
  <verbatim|zotero:g<em|id>:<em|item>> in a group library
  (<scm|zotero-derived-item>); its exported <BibTeX> entry, to which Zotero
  gives a key of its own, is rewritten to that key.

  The libraries searched are given by the preference <verbatim|"zotero
  libraries">: <verbatim|"user"> (the default) or <verbatim|"all">, which
  adds the groups of the user (<scm|zotero-groups>, remembered for a
  minute). The library of the user comes first, so its keys win;
  <scm|zotero-key-libraries> lists the libraries which have a key.

  <\explain>
    <scm|(zotero-find-key <scm-arg|key>)><explain-synopsis|resolve a
    key>
  <|explain>
    The item with the citation key <scm-arg|key>, or <scm|#f>: a direct
    request for a derived key; otherwise the item of the key when it is
    known (<scm|zotero-item-of>, asked for by <verbatim|items/<em|item>>
    and kept if it still has the key); otherwise a search of each library
    (<scm|find-in-library>: on <verbatim|zotero.org> first
    <verbatim|"<em|author> <em|year>">, then the key itself, which the
    application matches), keeping the exact match.
    <scm|zotero-resolve> resolves a list of keys: the keys whose item is
    known are asked for first, by library and at once
    (<verbatim|items?itemKey=...>, <scm|resolve-by-items!>), and nothing is
    searched while those answers are awaited (<scm|zotero-waiting?>).
    <scm|zotero-export> exports items as <BibTeX> (one request per library
    and per 50 items), <scm|zotero-item-versions> gives the versions of
    items (an item which is not returned has been deleted).
  </explain>

  <section|Sources of references>

  The sources of a bibliography come in one order, given by
  <scm|bib-sources> in <source-link|bib-manage.scm|TeXmacs/progs/database/bib-manage.scm>:

  <\enumerate>
    <item><scm|:local>, the entries of the document;

    <item>the <BibTeX> files of the user;

    <item><scm|:default>, the database of the user;

    <item>the managed <BibTeX> files;

    <item><scm|:zotero>;

    <item><scm|:attached>, the entries attached to the document, which
    serve when Zotero is not running.
  </enumerate>

  Without the database tool, only the file of the bibliography is read, by
  <c++>: if it is managed, it holds the items of Zotero; if it is the
  user's, the references of Zotero which it lacks are added to it (see
  below).

  <subsection|Managed <BibTeX> files>

  A managed file starts with the line <verbatim|% Exported from Zotero by
  TeXmacs on <em|date>; replaced by ...> (<scm|zotero-managed-file?>); any
  other file is the user's. It holds exactly the
  items which the document cites and which no earlier source has
  (<scm|zotero-write-bibliography>). <scm|zotero-before-update> refreshes
  it before the bibliography is generated; when Zotero is not available,
  the file is kept, and the message gives the date of the export.
  <menu|Update from Zotero> (<scm|zotero-update-bibliography>) inserts a
  bibliography when there is none: with a file
  <verbatim|<em|document>-zotero.bib> without the database tool, and
  without file with it (its references are then in the document, see
  below).

  Without the database tool, a bibliography without file (as
  <menu|Insert|Automatic|Bibliography> may insert it) or whose file does
  not exist yet has no references. <scm|zotero-before-update> then exports
  the references of Zotero into its file when Zotero has some of the
  citations (<scm|fill-missing-bibliography>): a bibliography without file
  is given the file <verbatim|<em|document>-zotero.bib> (the third child of
  the tag is set), a missing file is created under its name. Zotero is not
  asked about when the key of <verbatim|zotero.org> is missing, so that a
  user without Zotero is never asked for it. <scm|zotero-file-citations>
  puts given keys in the file of the bibliography by the same rules (added
  to a file of the user, exported into a file of Zotero).

  The dates are written as <verbatim|YYYY-MM-DD> by <scm|zotero-iso-date>:
  <scm|pretty-date> knows no <abbr|ISO> format.

  <subsection|References added to the user's file>

  <scm|(zotero-add-to-bib-file <scm-arg|file> <scm-arg|keys>)> appends to
  the user's <BibTeX> file the references of Zotero which it lacks, among
  the cited <scm-arg|keys>, each after a line

  <\tm-fragment>
    <verbatim|% Added from Zotero by TeXmacs on <em|date>:
    zotero://select/library/items/<em|item>>
  </tm-fragment>

  (<verbatim|zotero://select/groups/<em|id>/items/<em|item>> for a group).
  The rest of the file is kept byte for byte, and an added reference is not
  changed later. <scm|zotero-before-update> and
  <scm|zotero-update-bibliography> call it without the database tool,
  unless the preference <verbatim|"zotero add to bib file"> is off.
  <scm|zotero-bib-file-items> reads the comments back as
  <scm|(<em|key> <em|item> <em|library>)>, which <scm|zotero-check-missing>
  adds to the items recorded with the document: a key renamed in Zotero is
  found even from another document which uses the same file.

  <subsection|The source <scm|:zotero>>

  <scm|(zotero-db-entries <scm-arg|names>)> resolves the names, exports the
  items and converts them with <scm|zealous-bib-import> into database
  entries. Since <scm|bib-attach> stores the cited entries in the
  attachment <verbatim|<em|prefix>-bibliography> of the document, a
  document keeps its references from Zotero, which serve when Zotero is not
  running; with <verbatim|"auto bib import">, they enter the database of the
  user when the document is opened.

  <subsection|The items of a document><label|zotero-items>

  The item key of Zotero never changes, while a citation key may, and
  cannot be searched on <verbatim|zotero.org>: the item is the anchor of a
  citation. The attachment <verbatim|zotero-items> of a document holds a
  <scm|(tuple <em|key> <em|item> <em|library>)> for each key resolved
  through Zotero (<scm|zotero-record-items>, <scm|zotero-recorded-items>),
  in both modes; the attachment is only rewritten when a record changes.
  It is what allows to find an item again when its key changed, and
  <menu|Show in Zotero> to work without asking Zotero.

  <scm|(zotero-item-of <scm-arg|key>)> gives the <scm|(<em|item>
  <em|library>)> of a key from, in this order, the attachment, the
  comments of the user's <BibTeX> file (<scm|zotero-bib-file-items>), and
  the items which Zotero gave during the session: every answer converted by
  <scm|items-entries> (searches, completions, exports) notes the item of
  each key (<scm|seen-items>, forgotten by <scm|zotero-forget-keys>).

  <\explain>
    <scm|(zotero-cited <scm-arg|key> <scm-arg|buffer>)><explain-synopsis|take
    a citation from Zotero>
  <|explain>
    Called by the search window when a reference is chosen for a citation
    of <scm-arg|buffer> (the buffer is captured when the window opens: the
    callback runs while the results are the current buffer). When Zotero
    gave the item of <scm-arg|key> during the session
    (<scm|zotero-seen?>), the item is recorded with the document and the
    reference is copied where the bibliography reads it: imported into the
    database with the database tool (<scm|import-entry>, synced from then
    on), else put in the file of the bibliography
    (<scm|zotero-file-citations>). It runs as a command
    (<scm|zotero-command>), so that it waits for <verbatim|zotero.org> in
    a browser. Defined in <verbatim|zotero-db.scm>.
  </explain>

  <section|Entries of the database from Zotero>

  The entries from Zotero carry the contributor <verbatim|Zotero> and the
  meta attributes

  <\description>
    <item*|<verbatim|zotero-item>>the key of the item;

    <item*|<verbatim|zotero-library>>its library (<verbatim|users/0> or
    <verbatim|groups/<em|id>>; the first entries had <verbatim|user>, read
    as <verbatim|users/0>);

    <item*|<verbatim|zotero-version>>the version of the item when it was
    exported;

    <item*|<verbatim|zotero-synced>>the fields as they were then, to tell
    later which side changed a field;

    <item*|<verbatim|zotero-key>>the key in Zotero, when it was renamed
    there (the entry keeps its name, so that the citations still work);

    <item*|<verbatim|zotero-deleted>><verbatim|yes> when the item is no
    longer in Zotero (the entry is kept).
  </description>

  Being meta attributes (<scm|db-meta-attributes>), they never reach
  <BibTeX>, and two versions of an entry which only differ by them are the
  same for the versioning.

  <\explain>
    <scm|(zotero-sync-database . <scm-arg|force?>)><explain-synopsis|follow
    Zotero>
  <|explain>
    Unless <scm-arg|force?>, nothing happens when the versions of the
    libraries did not change since the last sync (preference
    <verbatim|"zotero sync version">). Otherwise, library by library, the
    imported entries whose item has a newer version are exported again: an
    entry left as imported gets a new version (<scm|db-update-entry>, which
    supersedes the old one explicitly), an entry edited in <TeXmacs>
    (<verbatim|modus manual>) is reported as a conflict. Returns the names
    updated, renamed and deleted, and the conflicts.
  </explain>

  For a conflict, <scm|zotero-conflict-fields> lists the fields which
  differ, with the <TeXmacs> value, the Zotero value and the value at the
  last sync; by default, a field changed only in Zotero takes its Zotero
  value. <scm|zotero-merge-entries> saves the user's choices as a manual
  version, synced with the current item.

  <scm|zotero-adopt-entries> marks the copies of Zotero items which the
  database had without the marks (imported by hand), so that they are
  synced from then on; those whose fields differ become manual, and go
  through the same choice.

  <section|Renamed and deleted keys>

  Zotero may change a key (Better<nbsp>BibTeX regenerates it when the title
  changes). <scm|(zotero-check-missing <scm-arg|keys>)> looks up the items
  recorded for the keys which Zotero no longer has: an item with another
  key is <em|renamed>, an item which is gone is <em|deleted>. A renamed item
  is still exported under its old key, by the managed file and by the
  source <scm|:zotero>, and the entry of a deleted item is kept from the
  previous managed file, until the user acts; the message says so.

  <scm|zotero-citation-renames> collects the renames of the document: those
  found this way and, with the database, those recorded by the sync
  (<verbatim|zotero-key>). <menu|Update the citations>
  (<scm|zotero-update-citations>) renames the keys in all the citations of
  the document or of its project (<scm|zotero-rename-citations>) and, with
  the database, gives the entry the new name (a new version). The changes
  to open documents pass through their undo history; the files of the
  project which were not open are saved. <menu|Check against Zotero...>
  (<scm|zotero-check-document>) reports the keys found in Zotero or
  elsewhere, renamed, deleted, missing, used by another source for another
  work, and the unmarked copies. Two references are the same work
  (<scm|zotero-same-work?>) when they have the same <abbr|DOI>, if both
  have one (<scm|zotero-normalized-doi> removes the prefixes such as
  <verbatim|https://doi.org/> and lowercases it), and otherwise when they
  have the same normalized title and year; the items carry their
  <abbr|DOI> (<scm|zotero-entry-doi>).

  <section|Projects>

  In a project, the citations are those of the master document
  (<scm|zotero-master>, <scm|project-get>) and of the files it includes,
  recursively (<scm|zotero-project-files>, a walk of the <markup|include>
  tags, through the open buffers or the files), and the bibliography is
  that of the master (<scm|zotero-master-bibliography-file>).

  <section|User interface>

  <subsection|Completion>

  <key|tab> in a citation (<scm|kbd-variant>, in
  <source-link|generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm> without the database tool and in
  <source-link|bib-kbd.scm|TeXmacs/progs/database/bib-kbd.scm> with it) adds the keys of Zotero with the typed
  prefix (<scm|zotero-completion-suffixes>, an interactive request),
  unless the preference <verbatim|"zotero completion"> is off. The keys of
  each prefix are remembered while no library changes, and a longer prefix
  is answered from a shorter one whose answer was complete (fewer items
  than the limit of the search), so that typing makes one request per new
  prefix at most.

  <subsection|Importing into the database>

  Besides the automatic import of the attached references,
  <menu|Document|Bibliography|Import the citations into the database>
  (<scm|zotero-import-citations>) and <menu|Focus|Import into the
  database> (<scm|zotero-import-entry>, offered when
  <scm|zotero-can-import?>) import references of Zotero by hand, through
  <scm|zotero-import-items>.

  <subsection|The search window of references>

  The search window of the database (<scm|open-db-chooser>, see
  <hlink|Editing databases|database-ui.en.tm>) is the search of citations
  in both modes. It is opened by <scm|focus-open-search-tool> and the
  alternate tab key:

  <\itemize>
    <item>with the database tool, on <scm|(bib-database)>
    (<scm|open-bib-chooser>, <source-link|bib-manage.scm|TeXmacs/progs/database/bib-manage.scm>, whose callback
    also calls <scm|zotero-cited>);
    <scm|db-search-results> appends the Zotero items which the database
    does not have (<scm|zotero-search-entries>);

    <item>without it, on the marker <scm|:bib-file>
    (<scm|zotero-open-search-tool>, whose callback sets the key and calls
    <scm|zotero-cited>): <scm|db-search-results> then calls
    <scm|zotero-file-search-results>, which lists the entries of the file
    of the bibliography matching all the words of the query (unless the
    file is managed), then the Zotero items. The entries of the file are
    converted with <scm|zealous-bib-import> and remembered while the file
    does not change.
  </itemize>

  <scm|zotero-mark-results> puts the source before each pretty result
  (<scm|zotero-source-text>: <verbatim|Database>, <verbatim|Database, from
  Zotero>, the name of the file, <verbatim|Zotero>, <verbatim|Zotero,
  <em|group>>), and <scm|zotero-search-sources-text> gives the line which
  names the sources at the top of the window. The preference
  <verbatim|"zotero in database search"> (on by default) leaves Zotero out.
  The Zotero results are database entries converted from the exported
  <BibTeX>, so they are formatted by the same <scm|db-pretty>.

  <subsection|Menus>

  <scm|focus-can-search?> and <scm|focus-open-search-tool> have
  definitions for citations without the database tool, in
  <source-link|generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>, after their default definitions. The focus
  menu and the focus bar of a citation offer <menu|Show in Zotero>
  (<scm|zotero-citation-entry>, <scm|zotero-show-item>, which opens
  <verbatim|zotero://select/library/items/<em|item>> or
  <verbatim|zotero://select/groups/<em|id>/items/<em|item>>) when the key
  is known without asking Zotero (<scm|zotero-known-entry>: the cache of
  keys, or the attachment <verbatim|zotero-items>).

  <subsection|Messages>

  The messages are built with <scm|(zotero-tr <scm-arg|template>
  <scm-arg|arg> ...)>, which translates the template and then puts the
  arguments for <verbatim|%1>, <verbatim|%2>..., so that the keys and the
  names of files are never translated; the dictionaries have the templates
  (in nine languages). The menu paths of the messages are translated item
  by item (<scm|zotero-menu-path>).

  <section|Preferences>

  <\description>
    <item*|<verbatim|"zotero server">>the address of Zotero
    (<verbatim|http://localhost:23119>);

    <item*|<verbatim|"zotero libraries">><verbatim|"user"> or
    <verbatim|"all">;

    <item*|<verbatim|"zotero export format">><verbatim|"bibtex"> or
    <verbatim|"biblatex">;

    <item*|<verbatim|"zotero completion">>completion of keys from Zotero;

    <item*|<verbatim|"zotero in database search">>Zotero in the search
    window of references;

    <item*|<verbatim|"zotero add to bib file">>the references added to the
    user's <BibTeX> file;

    <item*|<verbatim|"zotero source">><verbatim|"auto">,
    <verbatim|"local"> or <verbatim|"web">;

    <item*|<verbatim|"zotero api key">>the key of <verbatim|zotero.org>,
    when it is not in the wallet;

    <item*|<verbatim|"zotero user">>the user of the key (internal);

    <item*|<verbatim|"zotero sync version">>the versions of the libraries
    at the last sync (internal).
  </description>

  The first eight are in <menu|Document|Bibliography|Zotero settings...>
  (the key as <menu|API key of zotero.org>).

  <section|Tests>

  The suite <verbatim|zotero> (<verbatim|progs/check/zotero-test.scm>, run
  with <verbatim|src/tests/scheme/check.sh zotero>) replaces
  <scm|zotero-request> by a fake server, which answers from a small library
  with versions, a group library and the request kinds above, and records
  the requests. The groups which use the database work on a temporary
  database (<scm|bib-database> is redefined), a new file each time, and
  load <verbatim|zotero-db.scm> only when they run. The suite runs after
  the suites <verbatim|links> and <verbatim|database> in
  <source-link|check-master.scm|TeXmacs/progs/check/check-master.scm>.

  The fake library searches as the source does: the application also
  matches the citation keys (and the field <verbatim|extra> with
  <verbatim|qmode=everything>), <verbatim|zotero.org> only the titles, the
  creators and the dates, each word of the query being required. A web
  browser is simulated by redefining <scm|zotero-in-browser?> and
  <scm|zotero-start-request> (the requests are recorded, and answered with
  <scm|zotero-async-answer> one by one or all at once,
  <scm|sim-answer-all!>); a synchronous request would then fail, which
  shows that an operation asked for something it had not prefetched. The
  groups <verbatim|by item> (a key found only through its item, renamed
  items), <verbatim|cited> (the copies into the user's file, a new file
  and the database), <verbatim|async database> (<menu|Update> with the
  database in a browser) and <verbatim|async messages> (labels, progress)
  cover the work described above. Groups which reach
  <scm|zotero-command> with the source <verbatim|"web"> need the
  preference <verbatim|"zotero api key">, else the key is asked for and
  nothing runs.

  <section|Pitfalls>

  <\itemize>
    <item>The searches of <verbatim|zotero.org> do not look at the citation
    keys, even with <verbatim|qmode=everything> (see above).

    <item><scm|zotero-in-browser?> is <scm|(defined? 'web-javascript)>: a
    test must never define <scm|web-javascript>, or every group runs as in
    a browser.

    <item><scm|set-message> is invisible while a dialog has the focus (the
    footer of the document is not drawn again): feedback for a dialog goes
    into the dialog.

    <item><scm|string-search-forwards> answers -1 when nothing is found,
    which is true for <scm|or> and <scm|and>.

    <item><scm|generate-all-aux> in the suite crashes the offscreen Qt
    platform (the wait indicator takes the focus of a detached editor): the
    groups check <scm|zotero-db-entries>, which the generation calls,
    rather than generate the bibliography.

    <item>The local <abbr|API> has no list of deleted items: an item which a
    query by key does not return was deleted (or moved to the trash). The
    same queries also return the children (attachments, notes) of the
    items.

    <item>The strings of <scm|json-\<gtr\>tree> are in utf8, those of
    <TeXmacs> in Cork; <scm|cork-\<gtr\>utf8> also turns <verbatim|...>
    into an ellipsis and <verbatim|--> into a dash.

    <item>The <BibTeX> import lowercases titles (except the first letter
    and braced parts), so that copies of an entry with different
    capitalization have the same fields once imported.

    <item><scm|buffer-load> returns <scm|#t> when it <em|fails>, and
    <scm|buffer-pretend-modified> only reaches buffers with a view: this is
    why the files of a project which were not open are saved after a
    rename.

    <item>A database stays in memory after its file was removed: a test
    which removes and recreates a database file sees the old entries.

    <item>An input field of a widget also runs its command when it loses
    the focus, and a list (<markup|choices>) gives back its items converted
    to utf8 and back: compare them after the same conversion.

    <item>Under Vue, a dialog gave the keyboard to an embedded editor
    (<markup|texmacs-input>) rather than to its first field: what was typed
    in the search window of references went into the list of results. A
    field now claims the focus of a window which has none
    (<verbatim|vue_widget.cpp>).

    <item>A <markup|refreshable> widget evaluates its items when the window
    is built: refreshed, it shows the same text. A line which changes is a
    <markup|promise> in its refreshable, made again at each refresh (the
    sources of the search window, the state in the settings).

    <item>A definition with <scm|:require> must come after the default
    definition (without condition) of the same function, otherwise the
    default replaces it.
  </itemize>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
