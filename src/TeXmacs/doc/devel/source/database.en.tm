<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <TeXmacs> database and bibliographies>

  <section|Introduction>

  <TeXmacs> contains a small embedded database engine which is used for
  several unrelated purposes: the bibliographic database of the user, the
  list of user identities, the state of the <TeXmacs> server, the
  bookkeeping of file and database synchronization, a registry of AI agents
  and a few internal caches. The engine itself is written in <c++> and is
  deliberately minimalistic: it stores triples <em|(identifier, attribute,
  value)> of strings together with a creation and an expiration date, it can
  search them, and it keeps the whole history of all modifications. All
  higher level notions (entry types, encodings of values as <TeXmacs>
  snippets, users and permissions, versions of entries, editing of entries
  as documents) are implemented in <scheme> as successive layers on top of
  the engine.

  Bibliographies are the main client of this machinery, but they are also
  older than it: <TeXmacs> could already produce bibliographies from
  <BibTeX> files, either by running the external <verbatim|bibtex> program
  or with an internal reimplementation of the standard <BibTeX> styles in
  <scheme>, before the database existed. As a consequence, the bibliography
  pipeline has two main modes, depending on whether the <em|database tool>
  is enabled (preference <verbatim|"database tool">, <menu|Tools|Database
  tool>), and within each mode it may use either the external <BibTeX> or
  the internal style engine.

  This chapter describes the implementation of both. It complements the
  older <scheme> <abbr|API> reference <hlink|<TeXmacs>
  databases|../scheme/database/scheme-database.en.tm> and the guide
  <hlink|Writing <TeXmacs> bibliography
  styles|../scheme/bibliography/bibliography.en.tm>, which are not repeated
  here; see also the user manual page <hlink|Compiling a
  bibliography|../../main/links/man-bibliography.en.tm> and the
  documentation of the <hlink|citation
  markup|../../main/styles/std/std-automatic-bib.en.tm>. The
  synchronization of databases between a client and a <TeXmacs> server is
  described in <hlink|Synchronization of files and
  databases|collab-sync.en.tm>, and the use of the database by the server
  in <hlink|The remote file system|collab-remote-fs.en.tm>.

  <\traverse>
    <branch|The database engine|database-core.en.tm>

    <branch|The <scheme> database layers|database-scheme.en.tm>

    <branch|Editing databases|database-ui.en.tm>

    <branch|Bibliographies|database-bibliography.en.tm>

    <branch|Citations from Zotero|zotero.en.tm>
  </traverse>

  <section|Architecture overview>

  <subsection|The layers>

  From bottom to top, the implementation consists of the following layers.

  <\description>
    <item*|The engine (<c++>)>The class <cpp|database> in
    <verbatim|Plugins/Database/> holds an in-memory table of <em|lines>
    (<cpp|db_line>), each line associating a value to an attribute of an
    identifier during a time interval. All strings are interned as integer
    <em|atoms>. The engine maintains indices from identifiers and values to
    lines, a keyword index for full text search, and a prefix index for the
    completion of entry names. Modifications are journaled into an append
    only binary file with extension <verbatim|.tmdb>. A small set of
    functions taking the <abbr|URL> of the database as their first argument
    (<cpp|set_field>, <cpp|get_entry>, <cpp|query>, ...) are exported to
    <scheme> as <scm|tmdb-set-field>, <scm|tmdb-get-entry>, <scm|tmdb-query>,
    <abbr|etc.>

    <item*|The basic <scheme> <abbr|API>>The module <verbatim|database/db-base.scm>
    wraps the glue into the functions <scm|db-set-field>, <scm|db-get-entry>,
    <scm|db-search>, <abbr|etc.>, whose implicit arguments (the current
    database, the current time, a limit on the number of results, extra
    fields to add to new entries) are specified with the context macros
    <scm|with-database>, <scm|with-time>, <scm|with-limit>,
    <scm|with-extra-fields> and <scm|with-time-stamp>.

    <item*|Customization layers (<scheme>)>The modules
    <verbatim|db-format.scm> (encoding of values, entry formats and
    <em|kinds> of databases), <verbatim|db-users.scm> (users, groups and
    permissions) and <verbatim|db-version.scm> (versions of entries and
    importation) redefine the basic functions using <scm|tm-define> and
    <scm|former>. Each layer adds one context variable
    (<scm|with-encoding>, <scm|with-user>) while keeping the semantics of the
    basic routines.

    <item*|Databases as documents>The module <verbatim|db-convert.scm>
    converts between database entries and the markup
    <markup|db-entry>/<markup|db-field>; <verbatim|db-edit.scm> implements
    the structured editing of this markup; <verbatim|db-tmfs.scm> presents a
    database as a virtual document with <abbr|URL>
    <verbatim|tmfs://db/<em|kind>/<em|file>>; <verbatim|db-menu.scm> and
    <verbatim|db-widgets.scm> implement the <menu|Data> menu, the search
    toolbar, the search dialogue and the identities dialogue.

    <item*|Database kinds>The bibliographic kind (<verbatim|"bib">) is
    implemented by <verbatim|bib-db.scm> (entry formats and conversions
    between <BibTeX> and database entries), <verbatim|bib-manage.scm>
    (caching of <verbatim|.bib> files as databases, importation,
    exportation, retrieval of entries and compilation of bibliographies),
    <verbatim|bib-local.scm>, <verbatim|bib-menu.scm> and
    <verbatim|bib-kbd.scm>. A second, much smaller kind,
    <verbatim|"ai-agents">, is implemented by <verbatim|ai-agents-db.scm>
    and <verbatim|ai-agents-menu.scm>.

    <item*|<BibTeX> support>Independently of the database, the <c++> files
    <verbatim|Data/Convert/BibTeX/parsebib.cpp> (parser) and
    <verbatim|conservative_bib.cpp> (conservative import and export),
    <verbatim|Plugins/Bibtex/bibtex.cpp> (running the external program and
    reading <verbatim|.bbl> files) and <verbatim|bibtex_functions.cpp>
    (<c++> versions of the <BibTeX> built-in functions such as
    <verbatim|purify$> or <verbatim|format.name$>), and the <scheme> style
    engine in <verbatim|progs/bibtex/> (<verbatim|bib-utils.scm> and one
    file per style) implement the <BibTeX> side.

    <item*|The bibliography pipeline>The typesetter collects the keys of
    citations into the auxiliary data of the buffer; the <c++> editor
    routine <cpp|edit_process_rep::generate_bibliography> in
    <verbatim|Edit/Process/edit_process.cpp> selects one of the strategies
    described in <hlink|Bibliographies|database-bibliography.en.tm> and
    inserts the resulting <markup|bib-list> into the body of the
    <markup|bibliography> tag.
  </description>

  <subsection|Map of the source files>

  <\description-paragraphs>
    <item*|Engine (<c++>, <verbatim|src/src/>)><verbatim|Plugins/Database/database.hpp>
    (data structures and public functions), <verbatim|database.cpp> (atoms,
    basic operations, table of open databases), <verbatim|db_disk.cpp>
    (journal, persistence, compression, concurrent access),
    <verbatim|db_index.cpp> (keywords and completions),
    <verbatim|db_query.cpp> (queries) and <verbatim|db_sort.cpp> (sorting
    of results). The glue is declared in
    <verbatim|Scheme/Glue/build-glue-basic.scm> (section <verbatim|;; native
    TeXmacs databases>). <verbatim|Plugins/Sqlite3/> contains an unrelated
    and currently unused interface to <name|SQLite>.

    <item*|<scheme> database modules (<verbatim|src/TeXmacs/progs/database/>)><verbatim|db-base.scm>,
    <verbatim|db-format.scm>, <verbatim|db-users.scm>,
    <verbatim|db-version.scm>, <verbatim|db-edit.scm>,
    <verbatim|db-convert.scm>, <verbatim|db-markup.scm>,
    <verbatim|db-tmfs.scm>, <verbatim|db-widgets.scm>, <verbatim|db-menu.scm>,
    <verbatim|bib-db.scm>, <verbatim|bib-manage.scm>, <verbatim|bib-local.scm>,
    <verbatim|bib-menu.scm>, <verbatim|bib-kbd.scm>,
    <verbatim|ai-agents-db.scm>, <verbatim|ai-agents-menu.scm>. The same
    directory also contains <verbatim|title-markup.scm> and
    <verbatim|title-transform.scm>, which have nothing to do with databases
    (they implement the rendering of document titles and author data).

    <item*|Styles>The editing styles <verbatim|database.ts>,
    <verbatim|database-bib.ts>, <verbatim|database-ai-agents.ts> and the
    <BibTeX> presentation style <verbatim|bibliography.ts> live in
    <verbatim|src/TeXmacs/styles/test/>. The citation and bibliography
    markup is defined in <verbatim|packages/standard/std-automatic.ts> and
    <verbatim|packages/section/section-base.ts>.

    <item*|<BibTeX> (<c++>)><verbatim|Data/Convert/BibTeX/parsebib.cpp>,
    <verbatim|Data/Convert/BibTeX/conservative_bib.cpp>,
    <verbatim|Plugins/Bibtex/bibtex.cpp>,
    <verbatim|Plugins/Bibtex/bibtex_functions.cpp>, and
    <verbatim|Edit/Process/edit_process.cpp> (generation of the
    bibliography).

    <item*|<BibTeX> (<scheme>)><verbatim|progs/convert/bibtex/> (the
    <verbatim|bibtex> and <verbatim|tmbib> formats and their converters),
    <verbatim|progs/bibtex/bib-utils.scm> (style engine),
    <verbatim|plain.scm>, <verbatim|abbrv.scm>, <verbatim|abstract.scm>,
    <verbatim|acm.scm>, <verbatim|alpha.scm>, <verbatim|elsart-num.scm>,
    <verbatim|ieeetr.scm>, <verbatim|siam.scm>, <verbatim|unsrt.scm>
    (styles), <verbatim|bib-complete.scm> (completion of citation keys
    without the database) and <verbatim|bib-widgets.scm> (the
    bibliography insertion dialogue). The file
    <verbatim|src/TeXmacs/misc/bib/texmacs.bib> contains the entries with
    keys <verbatim|TeXmacs:...> used by <markup|cite-TeXmacs>.
  </description-paragraphs>

  <subsection|Where the data lives>

  All databases are ordinary files with extension <verbatim|.tmdb>. The
  following files are created on demand below
  <verbatim|$TEXMACS_HOME_PATH> (usually <verbatim|~/.TeXmacs>):

  <\description-paragraphs>
    <item*|<verbatim|users/users-master.tmdb>>User identities, the default
    user and, for each user and each database kind, the preferred database
    file (<verbatim|db-users.scm>).

    <item*|<verbatim|users/<em|uid>/<em|pseudo>-<em|kind>.tmdb>>The default
    per-user database of a given kind, for instance
    <verbatim|users/jdoe/jdoe-bib.tmdb> for the bibliographic database. The
    user may select another file in <menu|Data|Storage>.

    <item*|<verbatim|system/database/bib-master.tmdb> and
    <verbatim|system/database/bib/>>The cache of imported <verbatim|.bib>
    files: for each cached file, a copy <verbatim|<em|id>.bib> of the source,
    its conversion <verbatim|<em|id>.tm> into a <TeXmacs> document, and a
    database <verbatim|<em|id>.tmdb> containing the entries that were
    actually cited (<verbatim|bib-manage.scm>).

    <item*|<verbatim|system/database/lp-master.tmdb>>Time stamps used by
    the literate programming tools (<verbatim|utils/literate/lp-build.scm>).

    <item*|<verbatim|server/global.tmdb>>The state of a <TeXmacs> server
    (<scm|global-database>).

    <item*|<verbatim|system/bib/>>Not a database, but the working directory
    of the external <verbatim|bibtex> program: <verbatim|temp.aux>,
    <verbatim|temp.bbl>, <verbatim|temp.log>, <verbatim|auto.bib> and
    copies of <verbatim|.bst> files.
  </description-paragraphs>

  In addition, a document may carry bibliographic entries as
  <em|attachments> named <verbatim|<em|prefix>-bibliography> (entries
  retrieved when the bibliography was last compiled) and
  <verbatim|<em|prefix>-biblio> (entries edited locally), see
  <hlink|Bibliographies|database-bibliography.en.tm>.

  <subsection|Status>

  <\itemize>
    <item><em|Stable and used by default>: the <BibTeX> parser, the
    conversion between <verbatim|.bib> files and <TeXmacs> documents, the
    internal style engine with the styles <verbatim|tm-plain>,
    <verbatim|tm-abbrv>, <verbatim|tm-alpha>, <abbr|etc.>, and the
    invocation of the external <verbatim|bibtex> program. These do not need
    the database tool.

    <item><em|Stable but only active with the database tool>: the engine,
    the <scheme> layers, the bibliographic database with its editing
    interface, the caching of <verbatim|.bib> files, the attachment of
    bibliographies to documents and the automatic importation of attached
    entries. The preference <verbatim|"database tool"> is
    <verbatim|"off"> by default (<verbatim|texmacs/texmacs/tm-server.scm>).
    The engine itself is always compiled in and is used unconditionally by
    the server, the client synchronization code, the identities dialogue and
    <verbatim|lp-build.scm>.

    <item><em|Experimental or recent>: the <verbatim|"ai-agents"> kind, the
    local bibliography editor (<verbatim|tmfs://biblio/...>,
    <scm|open-biblio>), the synchronization of the bibliographic database
    with a server, and the permission layer (which is only enforced for
    reading and for owners, see <hlink|The <scheme> database
    layers|database-scheme.en.tm>).
  </itemize>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
