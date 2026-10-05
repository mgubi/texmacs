<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing databases>

  <section|Overview>

  A database is edited as a <TeXmacs> document whose body is a list of
  <markup|db-entry> tags (see <hlink|The <scheme> database
  layers|database-scheme.en.tm>). There are three kinds of such documents:

  <\description>
    <item*|Database views>the virtual document
    <verbatim|tmfs://db/<em|kind>/<em|file>> shows the result of a query on
    the user database of the given kind. Saving it stores the modified
    entries back into the database. This is what <menu|Data|Open
    bibliography> and <menu|Data|Open AI agents> open.

    <item*|Ordinary files>any document with the style
    <verbatim|database-bib> (for instance a <verbatim|.bib> file opened
    through the <verbatim|tmbib> format) can be edited with the same tools.
    Its entries are not in the database, but can be imported into it.

    <item*|Local bibliographies>the virtual document
    <verbatim|tmfs://biblio/<em|prefix>/<em|file>> shows the bibliographic
    entries attached to a document (see <hlink|Bibliographies|database-bibliography.en.tm>).
  </description>

  All the database <abbr|UI> is only reachable when the preference
  <verbatim|"database tool"> is on: the <menu|Data> menu (both in the menu
  bar and in the compact main menu) is guarded by
  <scm|with-database-tool?> in <verbatim|texmacs/menus/main-menu.scm>, and
  many actions test <scm|supports-db?>. The modules are loaded lazily from
  <verbatim|init-texmacs.scm> (<scm|lazy-menu>, <scm|lazy-define>,
  <scm|lazy-tmfs-handler> and <scm|lazy-keyboard>).

  <section|Styles and modes>

  The package <verbatim|database> (file
  <verbatim|styles/test/database.ts>) defines the rendering of
  <markup|db-entry>, <markup|db-folded-entry>, <markup|db-pretty-entry>,
  <markup|db-field>, <markup|db-field-optional>, <markup|db-field-alternative>,
  <markup|db-pretty> and <markup|db-result>, and the variable
  <verbatim|db-kind>. The styles <verbatim|database-bib> and
  <verbatim|database-ai-agents> use this package and set
  <verbatim|db-kind>; <verbatim|database-bib> moreover defines the markup
  for names in author fields (<markup|name-von>, <markup|name-jr>,
  <markup|name-sep>) and for the raw <BibTeX> constructs that may occur in
  a converted <verbatim|.bib> file (<markup|bib-comment>,
  <markup|bib-preamble>, <markup|bib-string>, ...). For a database kind
  <em|kind>, <scm|db-get-style> returns the style name
  <verbatim|database-<em|kind>>.

  The modes are defined in <verbatim|kernel/texmacs/tm-modes.scm> and in the
  kind modules:

  <\description>
    <item*|<scm|in-database?>>the style has the variable
    <verbatim|database-style>, <abbr|i.e.> uses the <verbatim|database>
    package;

    <item*|<scm|in-bib?>>the style is <verbatim|database-bib>;

    <item*|<scm|in-bib-names?>>moreover inside an <verbatim|author> or
    <verbatim|editor> field (<verbatim|bib-menu.scm>);

    <item*|<scm|in-ai-agents?>>the style is <verbatim|database-ai-agents>
    (<verbatim|ai-agents-menu.scm>).
  </description>

  In these modes <verbatim|db-menu.scm> replaces the <menu|Insert> menu and
  the mode icons by database specific ones (new entry of each type of the
  kind, import or confirmation of entries), and adds <menu|Focus> menus to
  insert and remove fields.

  <section|The <verbatim|tmfs://db> handler>

  <verbatim|db-tmfs.scm> registers the handlers for the <verbatim|db> class
  of <verbatim|tmfs> <abbr|URL>s. The name part of the <abbr|URL> is parsed
  by <scm|name-\<gtr\>query>: leading components of the form
  <verbatim|<em|var>=<em|val>> are query parameters, the next component is
  the kind and the rest is a file name. The file name is only used for the
  title (<verbatim|global> yields \PMy bibliographic database\Q); the
  documents always show <scm|(user-database <em|kind>)>.

  <\explain>
    <scm|(tmfs-load-handler (db name) ...)><explain-synopsis|showing a
    database>
  <|explain>
    Builds the query from the parameters of the <abbr|URL> and the
    per-document preferences returned by <scm|db-get-current-query>
    (<verbatim|search>, <verbatim|order>, <verbatim|direction>,
    <verbatim|limit>, <verbatim|present>, stored as preferences named
    <verbatim|<em|param>,<em|url>> by <scm|db-set-query-preference>). The
    search string is split at commas into <scm|:match> constraints (words of
    less than two characters are ignored), a <verbatim|"type"> constraint
    restricts the results to the types of the kind, and each order
    attribute gives an <scm|:order> constraint. The resulting entries are
    presented as <markup|db-entry>, <markup|db-folded-entry> or
    <markup|db-pretty-entry> depending on the <verbatim|present>
    parameter, in a document with style <verbatim|database-<em|kind>>.
  </explain>

  <\explain>
    <scm|(tmfs-save-handler (db name doc) ...)><explain-synopsis|saving>
  <|explain>
    Calls <scm|db-confirm-entries-in> on the document, which commits all
    complete entries (see below).
  </explain>

  The toolbar of <verbatim|db-menu.scm> (<scm|db-toolbar>, shown at the
  bottom of the window by <scm|db-show-toolbar> for <verbatim|tmfs://db>
  buffers) edits the query preferences and reverts the buffer. Hence
  changing the search string or the order simply reloads the virtual
  document.

  <section|Editing entries>

  <subsection|Creating and completing entries>

  <scm|(make-db-entry <scm-arg|type>)> inserts a new entry after the current
  paragraph, with a fresh identifier from <scm|db-create-id>, meta fields
  <verbatim|contributor> (the default user), <verbatim|modus>
  (<verbatim|"manual">) and <verbatim|date>, and calls
  <scm|db-complete-fields>, which uses the format of the type in
  <scm|db-format-table> to add empty mandatory fields
  (<markup|db-field>), alternatives (<markup|db-field-alternative>) and
  optional fields (<markup|db-field-optional>). The cursor is
  placed in the name of the entry.

  The keyboard handlers of <verbatim|db-edit.scm> implement a \Pfill out
  the form\Q interaction through <scm|kbd-enter>:

  <\itemize>
    <item>in the name, <key|return> moves to the first field (an empty name
    is refused);

    <item>in a mandatory field, <key|return> refuses empty values and moves
    to the next field;

    <item>in an optional field, <key|return> removes the field if it is
    empty, and otherwise turns it into an ordinary field;

    <item>in an alternative field, <key|return> checks that exactly one of
    the alternatives is filled out and removes the other ones;

    <item>after the last field, <scm|keep-completing> either moves to the
    first empty field or, when the entry is complete and the buffer is a
    database view, <em|confirms> the entry.
  </itemize>

  In author and editor fields of bibliographic entries,
  <verbatim|bib-kbd.scm> adds shortcuts: a comma or \P<verbatim|and>\Q
  starts a new name (<markup|name-sep>), <key|return> converts a plain list
  <verbatim|A. Einstein and N. Bohr> into structured names with
  <markup|name> markup for the last names, and <menu|Insert|Particle> or
  <menu|Insert|Title suffix> insert <markup|name-von> and
  <markup|name-jr>.

  <subsection|Confirming and removing entries>

  An entry of a database view is committed to the database when it is
  <em|confirmed>, either explicitly (<key|A-return>,
  <scm|kbd-alternate-enter>, or the confirmation icon), or when the buffer
  is saved. <scm|confirm-entry> computes the new entry with
  <scm|entry-\<gtr\>assoc-list> and calls <scm|db-update-entry> with extra
  fields <verbatim|contributor> and <verbatim|modus>
  (<verbatim|"manual">) and with time stamps. If the entry changed, it gets
  a <em|new identifier>, which is written back into the document together
  with the new meta fields. Before committing, <scm|detach-buffer-entries>
  gives fresh identifiers to entries which occur several times in the buffer
  with the same identifier (for instance after copy and paste), so that the
  copies become independent entries.

  Structured deletion of an entry (<key|A-backspace>, <key|A-delete>,
  <scm|structured-remove-horizontal>) removes it from the database
  (<scm|remove-entry>) and from the document. Removal only sets the
  expiration date of the lines, so an entry can be recovered with
  <scm|with-time>.

  In an ordinary document (not a database view), <menu|Insert|Import
  entry> and <menu|Insert|Import selected entries> import the entries into
  the user database of the kind, through <scm|db-import-this-entry> and
  <scm|db-import-selection>, which are redefined by the bibliographic
  kind.

  <section|Searching and choosing entries>

  <verbatim|db-widgets.scm> implements a search dialogue:

  <\explain>
    <scm|(open-db-chooser <scm-arg|db> <scm-arg|kind> <scm-arg|name>
    <scm-arg|call-back>)><explain-synopsis|choose an entry>
  <|explain>
    Opens a dialogue (or a side tool if side tools are enabled) with an
    input field and a result area. Each keystroke schedules, after 200 ms of
    inactivity, a query <scm|((:completes <scm-arg|text>) ("type"
    ...))> limited to 20 results on <scm-arg|db>. The results are
    pretty-printed with <scm|db-pretty> in the auxiliary buffer
    <verbatim|tmfs://aux/db-search-results>; clicking a result
    (<scm|db-confirm-result>) calls <scm-arg|call-back> with the name of the
    entry. Queries and loaded entries are cached for the life of the
    dialogue. The result areas can be resized.
  </explain>

  For bibliographies, <scm|open-bib-chooser> opens this dialogue on
  <scm|(bib-database)>. It is used by the focus search tool inside
  citations (<scm|focus-open-search-tool> in <verbatim|bib-menu.scm>,
  <menu|Focus|Search references>) and by the alternate tab key
  (<scm|kbd-alternate-tab>, which calls <scm|kbd-alternate-variant>) in a
  citation, while <key|tab> (<scm|kbd-variant> in
  <verbatim|bib-kbd.scm>) completes the citation key with
  <scm|index-get-name-completions>. For the kind <verbatim|"bib">, the
  dialogue also lists the matching references of Zotero after those of the
  database, puts the source of each reference before it, and names its
  sources in a line above the input field (see <hlink|Citations from
  Zotero|zotero.en.tm>).

  When the database tool is off, <key|tab> in a citation completes keys
  from the <verbatim|.bib> file of the document (<scm|citekey-completions>
  in <verbatim|bibtex/bib-complete.scm>) and from Zotero, and the same
  dialogue is opened on the marker <scm|:bib-file> instead of a database
  (<scm|zotero-open-search-tool>, through the definitions of
  <scm|focus-can-search?> and <scm|focus-open-search-tool> for citations
  in <verbatim|generic-edit.scm>): <scm|db-search-results> then lists the
  entries of the <verbatim|.bib> file of the bibliography, and those of
  Zotero.

  <subsection|Pretty printing>

  The <verbatim|"Pretty"> presentation and the search results use the
  function <scm|(db-pretty <scm-arg|l> <scm-arg|kind> <scm-arg|fm>)>,
  which is the identity by default. For bibliographies
  (<verbatim|bib-manage.scm>) it converts the entries to <BibTeX> entries
  and formats them with the internal <verbatim|siam> style, turning each
  item into <verbatim|(db-result <em|name> <em|text>)>. In a database view,
  <markup|db-pretty-entry> calls the secure <scheme> function
  <scm|ext-db-pretty-entry> through <markup|extern>, which caches the
  result per kind (<verbatim|db-markup.scm>); clicking the key of a pretty
  entry (<scm|db-pretty-notify>) turns it back into an editable
  <markup|db-entry>.

  <section|Other dialogues>

  <\description>
    <item*|Identities>(<scm|open-identities>) edit the users of
    <verbatim|users-master.tmdb>; the pseudo and full name of the default
    user are used as <verbatim|contributor> of new entries.

    <item*|Preferences>(<scm|open-db-preferences>) currently only contain
    the preference <verbatim|"auto bib import">, which controls the
    automatic importation of bibliographies attached to documents.

    <item*|Storage>(<menu|Data|Storage>) selects the file used as the user
    database of the current kind (<scm|use-database>).

    <item*|Import and export>(<menu|Data|Import>, <menu|Data|Export>) are
    dispatched through <scm|db-import-select>, <scm|db-export-select>,
    <scm|db-import-file> and <scm|db-export-file>, which do nothing by
    default in <verbatim|db-convert.scm> and are redefined in the mode
    <scm|in-bib?> by <verbatim|bib-manage.scm>. Recently used files are
    remembered with <scm|learn-interactive> (<scm|db-recent-imports>,
    <scm|db-recent-exports>).
  </description>

  <section|Pitfalls>

  <\itemize>
    <item>Saving a database view commits <em|all> complete entries of the
    buffer; incomplete entries are kept in the buffer and the user is
    asked to complete them.

    <item>Every confirmed modification creates a new identifier. Code that
    keeps identifiers of entries across edits (for instance in other
    documents) should refer to entries by name rather than by identifier.

    <item>Query preferences are stored in the user preferences file, one
    set per <verbatim|tmfs://db> <abbr|URL>.

    <item>The lazy declaration <verbatim|(lazy-define (database db-widget)
    open-db-chooser)> in <verbatim|init-texmacs.scm> names a module
    <verbatim|db-widget> which does not exist (the file is
    <verbatim|db-widgets.scm>). In practice <verbatim|db-widgets.scm> is
    loaded through <verbatim|db-menu.scm> before the chooser is needed,
    and, without the database tool, by <verbatim|bibtex/zotero-db.scm>.

    <item>No lazy <verbatim|tmfs> handler is declared for
    <verbatim|biblio>: <verbatim|tmfs://biblio/...> <abbr|URL>s only work
    once <verbatim|bib-local.scm> has been loaded, normally through
    <scm|open-biblio>.
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
