<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <scheme> database layers>

  <section|Organization>

  The <scheme> interface to databases lives in
  <verbatim|src/TeXmacs/progs/database/>. It is organized as a chain of
  modules, each of which uses the previous one and redefines some of the
  basic routines with <scm|tm-define>, calling the previous definition
  through <scm|former>:

  <\with|par-mode|center>
    <verbatim|db-base> <math|\<rightarrow\>> <verbatim|db-format>
    <math|\<rightarrow\>> <verbatim|db-users> <math|\<rightarrow\>>
    <verbatim|db-version> <math|\<rightarrow\>> <verbatim|db-edit>
    <math|\<rightarrow\>> <verbatim|db-convert>
  </with>

  followed by <verbatim|db-markup>, <verbatim|db-tmfs>,
  <verbatim|db-widgets> and <verbatim|db-menu> for the user interface, and
  by the modules of the individual kinds (<verbatim|bib-db>,
  <verbatim|bib-manage>, ..., <verbatim|ai-agents-db>). The public routines
  <scm|db-set-field>, <scm|db-get-field>, <scm|db-set-entry>,
  <scm|db-get-entry>, <scm|db-remove-entry>, <scm|db-create-entry> and
  <scm|db-search> are therefore the composition of all loaded layers. The
  implicit parameters of the layers are global variables which are bound
  dynamically with <scm|with-global> by the context macros; <scm|db-reset>
  resets all of them (every layer extends it).

  The <abbr|API> reference <hlink|<TeXmacs>
  databases|../scheme/database/scheme-database.en.tm> documents the public
  routines of these layers. Two details of that reference are outdated:
  <scm|db-get-field-first> takes a third argument (the default value
  returned when the field is empty), and the macro <scm|with-indexing>,
  <scm|index-do-indexate?> and <scm|index-attribute-table> no longer exist,
  since indexing is now done unconditionally in <c++> (see <hlink|The
  database engine|database-core.en.tm>). Below we concentrate on the
  implementation and on what the reference does not say.

  <section|The basic layer (<verbatim|db-base.scm>)>

  <subsection|Context>

  <\explain>
    <scm|current-database>

    <scm|(with-database <scm-arg|db> . <scm-arg|body>)>

    <scm|(with-database* <scm-arg|db> . <scm-arg|body>)><explain-synopsis|the
    current database>
  <|explain>
    <scm|current-database> is a <abbr|URL> (initially <scm|(url-none)>, in
    which case <scm|db-get-db> raises an error). <scm|with-database*>
    moreover disables the history of the database (<scm|tmdb-keep-history>),
    so that it gets compressed on disk.
  </explain>

  <\explain>
    <scm|db-time>

    <scm|(with-time <scm-arg|t> . <scm-arg|body>)>

    <scm|(db-get-time)><explain-synopsis|the time of queries and updates>
  <|explain>
    <scm|db-time> is <scm|:now> (the default), <scm|:always> (all lines,
    passed as time <scm|0> to <c++>), a number or a numeric string.
    <scm|db-get-time> converts it to the inexact number expected by the
    glue. Modifications should be done at <scm|:now>: nothing prevents a
    modification in the past, but the result would be inconsistent with the
    journal order.
  </explain>

  <\explain>
    <scm|(with-limit <scm-arg|n> . <scm-arg|body>)>

    <scm|(with-extra-fields <scm-arg|l> . <scm-arg|body>)>

    <scm|(with-time-stamp <scm-arg|on?> . <scm-arg|body>)><explain-synopsis|other
    context>
  <|explain>
    <scm|db-limit> bounds the number of results of <scm|db-search>
    (default <math|10<rsup|6>>). <scm|db-set-entry> adds the fields of
    <scm|db-extra-fields> which are not already present, and, if
    <scm|db-time-stamp?> holds, a <verbatim|date> field with the current
    time. Notice that only <scm|db-set-entry> (and hence
    <scm|db-create-entry>) takes these into account, not
    <scm|db-set-field>.
  </explain>

  <subsection|Routines>

  All routines are thin wrappers around the glue of the <hlink|engine|database-core.en.tm>.
  Worth noting are:

  <\itemize>
    <item><scm|(db-entry-exists? <scm-arg|id>)> tests whether the entry has
    a <verbatim|name> field, not whether the identifier occurs at all.

    <item><scm|(db-create-id)> draws identifiers with
    <scm|create-unique-id> until it finds one without attributes.

    <item><scm|(db-search <scm-arg|q>)> rewrites the keyword forms of
    constraints (<scm|:order>, <scm|:modified>, <scm|:match>,
    <scm|:prefix>, <scm|:contains>, <scm|:completes>) with
    <scm|rewrite-query> before calling <scm|tmdb-query>;
    <scm|db-search-paginate> takes an explicit limit and offset;
    <scm|db-search-name> and <scm|db-search-owner> are shortcuts.

    <item><scm|(index-get-completions <scm-arg|prefix>)> and
    <scm|(index-get-name-completions <scm-arg|prefix>)> give access to the
    keyword and name indices; the latter is used to complete citation keys
    (<verbatim|bib-kbd.scm>).

    <item><scm|(global-database)> returns
    <verbatim|$TEXMACS_HOME_PATH/server/global.tmdb>, the database of a
    <TeXmacs> server.
  </itemize>

  A typical use of the basic layer:

  <\scm-code>
    (with-database (url-\<gtr\>url "$TEXMACS_HOME_PATH/system/test.tmdb")

    \ \ (with id (db-create-entry '(("type" "note") ("name" "n1")

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ("text"
    "hello world")))

    \ \ \ \ (db-set-field id "text" (list "hello again"))

    \ \ \ \ (list (db-get-field id "text")

    \ \ \ \ \ \ \ \ \ \ (with-time :always (db-get-field id "text"))

    \ \ \ \ \ \ \ \ \ \ (db-search '((:match "hello") (:order "name"
    #t))))))
  </scm-code>

  The second call returns both values, the old and the current one. If the
  full layer stack is loaded, the values are moreover encoded as <TeXmacs>
  snippets, as explained in the next section.

  <section|Formats and encodings (<verbatim|db-format.scm>)>

  <subsection|Kinds, types and formats>

  Entries have a <verbatim|type> field, and databases have a <em|kind>
  which determines the admissible types. Both are declared in smart tables:

  <\description>
    <item*|<scm|db-kind-table>>maps a kind to its list of entry types, for
    instance <verbatim|"bib"> to <verbatim|("article" "book" ...)> in
    <verbatim|bib-db.scm> and <verbatim|"ai-agents"> to
    <verbatim|("corrector" "interlocutor" "translator")> in
    <verbatim|ai-agents-db.scm>.

    <item*|<scm|db-format-table>>maps an entry type to a <em|format>: a
    string (a mandatory field), <scm|(and <scm-arg|f1> ...)>, <scm|(or
    <scm-arg|a1> <scm-arg|a2> ...)> (exactly one of the alternatives should
    be filled out) or <scm|(optional <scm-arg|a>)>. <scm|format-\<gtr\>attributes>
    flattens a format into the list of its attributes. The format is used
    by the editor to complete entries with empty fields and to decide
    whether an entry is complete, and by <scm|db-get-entry> to put the
    fields in a canonical order.
  </description>

  <scm|(db-reserved-attributes)> (<verbatim|type>, <verbatim|location>,
  <verbatim|dir>, <verbatim|date>, <verbatim|pseudo>, <verbatim|id>) and
  <scm|(db-meta-attributes)> (<verbatim|date>, <verbatim|contributor>,
  <verbatim|modus>, <verbatim|origin>, <verbatim|newer>) are special: meta
  attributes describe the provenance of an entry, are shown separately in
  the editor and are ignored when comparing entries.

  <subsection|Encodings>

  The engine only stores strings, but most values are really <TeXmacs>
  content (a title with mathematics, an author name with <markup|name>
  markup). The encoding layer converts values on the fly. For a field
  <scm-arg|attr> of an entry of type <scm-arg|type>, the encoding is looked
  up in <scm|db-encoding-table> under the keys <scm|(<scm-arg|attr>
  <scm-arg|type> <scm-arg|enc>)>, where <scm-arg|enc> is the current value
  of <scm|db-encoding> (set with <scm|with-encoding>), trying the wildcard
  <scm|*> for each component in turn. If nothing is found, the encoding is
  <scm|:texmacs>, which means that values are stored as serialized
  <TeXmacs> snippets (<scm|serialize-texmacs-snippet> and
  <scm|parse-texmacs-snippet>). The encoders and decoders themselves are
  found in <scm|db-encoder-table> and <scm|db-decoder-table>.

  With the default value <scm|:default> of <scm|db-encoding>, all values
  are therefore stored as <TeXmacs> snippets, and plain strings are
  essentially stored as themselves. <scm|(with-encoding #f ...)> disables
  the layer and gives access to the raw strings. The users layer adds a
  second encoding: under <scm|(with-encoding :pseudos ...)>, the
  permission attributes (<verbatim|owner>, <verbatim|readable>, ...) are
  converted between user identifiers in the database and user pseudos in
  <scheme> (<scm|:users> encoder and decoder); the server uses the same
  mechanism for <verbatim|version-by> in <verbatim|server/server-tmfs.scm>.

  <scm|db-search> encodes the values of its constraints using the type
  found in a <verbatim|"type"> constraint, if any. Keyword searches work on
  the serialized strings, but since keywords are extracted from the parsed
  snippets (<hlink|see|database-core.en.tm>), they are not affected by the
  serialization.

  <section|Users and permissions (<verbatim|db-users.scm>)>

  <subsection|Users and user databases>

  The master database <verbatim|$TEXMACS_HOME_PATH/users/users-master.tmdb>
  contains entries of type <verbatim|"user"> (fields <verbatim|pseudo> and
  <verbatim|name>) and of type <verbatim|"preference">. The routines are:

  <\explain>
    <scm|(get-default-user)><explain-synopsis|current user identifier>
  <|explain>
    Returns the identifier of the default user. The first time, a user is
    created from the login name and the full name of the operating system
    account (<scm|create-default-user>). <scm|add-user> uses the pseudo as
    identifier, so that identifiers of local users usually coincide with
    their pseudos. The dialogue <menu|Data|Open identities>
    (<scm|open-identities> in <verbatim|db-widgets.scm>) allows the user to
    create, rename and delete identities (<scm|add-user>,
    <scm|remove-user>, <scm|set-default-user>, <scm|set-user-info>).
  </explain>

  <\explain>
    <scm|(user-database . <scm-arg|opt-kind>)>

    <scm|(get-preferred-database <scm-arg|uid> <scm-arg|kind>)>

    <scm|(set-preferred-database <scm-arg|uid> <scm-arg|kind> <scm-arg|db>)><explain-synopsis|per-user
    databases>
  <|explain>
    <scm|user-database> returns the preferred database of the default user
    for the given kind, or for <scm|(db-get-kind)> if no kind is given. The
    function <scm|db-get-kind> returns <verbatim|"general"> by default and
    is redefined in the modes <scm|in-bib?> (<verbatim|"bib">) and
    <scm|in-ai-agents?> (<verbatim|"ai-agents">), so that inside a buffer
    showing a database the \Pcurrent\Q database is the one of its kind. When
    no preference exists, the file
    <verbatim|users/<em|uid>/<em|pseudo>-<em|kind>.tmdb> is chosen and
    recorded. <scm|use-database> and <scm|recent-databases> implement the
    <menu|Data|Storage> menu; the list of recent databases is simply the
    history of the <verbatim|value> field of the preference entry.
  </explain>

  <subsection|Permissions>

  <scm|db-current-user> (set with <scm|with-user>) is either <scm|#t>
  (root, the default: everything is allowed), a user identifier or a list
  of identifiers. <scm|(db-allow? <scm-arg|id> <scm-arg|uid>
  <scm-arg|attr>)> checks whether one of the users obtained by expanding
  <scm-arg|uid> through the groups which delegate <scm-arg|attr>
  (<scm|db-expand-user>; the pseudo-user <verbatim|"all"> is always
  included) occurs in the field <scm-arg|attr> of the entry; owners have all
  permissions. The layer redefines the basic routines as follows:

  <\itemize>
    <item><scm|db-get-field> and <scm|db-get-entry> return empty results
    unless the user is an owner or reader of the entry;

    <item><scm|db-set-field>, <scm|db-set-entry> and <scm|db-remove-entry>
    do nothing unless the user is an <em|owner> (the <verbatim|writable>
    attribute is currently not consulted);

    <item><scm|db-create-entry> adds the current user(s) to the
    <verbatim|owner> field;

    <item><scm|db-search> adds a constraint on <verbatim|owner> or
    <verbatim|readable> and returns the union of both searches.
  </itemize>

  Locally, all code runs as root. The permission layer is mainly used by
  the server, which wraps requests of remote users in <scm|with-user>
  (see <hlink|The remote file system|collab-remote-fs.en.tm>).

  <section|Versions and importation (<verbatim|db-version.scm>)>

  An entry is never modified in place by the editor: saving a modified
  entry creates a <em|new entry> which supersedes the old one. This is
  necessary because entries circulate between users (through attachments of
  documents, synchronization with a server, or shared <verbatim|.bib>
  files), and one needs to decide which version is the most recent.

  <\explain>
    <scm|(db-update-entry <scm-arg|id> <scm-arg|new-l> .
    <scm-arg|opt-new-id>)><explain-synopsis|create a new version>
  <|explain>
    If <scm-arg|new-l> equals the current entry up to the meta attributes
    (<scm|db-same-entries?>), nothing happens and <scm-arg|id> is returned.
    Otherwise a new entry is created (with identifier <scm-arg|opt-new-id>
    if given and free), its <verbatim|newer> field is set to <scm-arg|id>
    followed by the older history, and the old entry is removed.
  </explain>

  <\explain>
    <scm|(db-import-entry <scm-arg|id> <scm-arg|l>)><explain-synopsis|import
    a foreign entry>
  <|explain>
    Imports an entry <scm-arg|l> which carries its own identifier
    <scm-arg|id>. In order:

    <\enumerate>
      <item>if <scm-arg|id> is already used, the entry is ignored (with a
      warning if the contents differ);

      <item>if an identical entry with the same name exists, that entry
      absorbs <scm-arg|id> in its <verbatim|newer> field;

      <item>if some entry declares <scm-arg|id> as older
      (<verbatim|newer> contains <scm-arg|id>), the import is cancelled;

      <item>all entries listed in the <verbatim|newer> field of
      <scm-arg|l> are removed;

      <item>if an entry with the same name and the same
      <verbatim|contributor> exists, the one entered manually
      (<verbatim|modus> is <verbatim|"manual">) wins over an imported one,
      and otherwise the one with the most recent <verbatim|date>;

      <item>otherwise the entry is simply stored.
    </enumerate>

    Warnings are sent to the debug channel <verbatim|database-warning> with
    <scm|db-warning>, and can be silenced with <scm|db-duplicate-warning?>.
  </explain>

  <section|Entries as documents (<verbatim|db-convert.scm>)>

  <subsection|The markup>

  In documents, an entry is represented by the tag

  <\tm-fragment>
    <verbatim|(db-entry <em|id> <em|type> <em|name> (document <em|meta-fields>)
    (document <em|fields>))>
  </tm-fragment>

  where each field is <verbatim|(db-field <em|attr> <em|value>)>, a
  multi-valued field being represented by several <markup|db-field> tags
  with the same attribute. The meta fields (provenance) are stored
  separately from the ordinary fields. The variants <markup|db-folded-entry>
  and <markup|db-pretty-entry> have the same children and only differ in
  presentation; <scm|db-entry-any?> recognizes all three (<scm|db-entry?>
  only the first). While editing, <markup|db-field-optional> and
  <markup|db-field-alternative> mark empty optional and alternative fields.
  Utilities in <verbatim|db-edit.scm> such as <scm|db-entry-ref>,
  <scm|db-entry-set>, <scm|db-entry-remove> and <scm|db-entry-rename>
  manipulate this markup as <scheme> trees, the pseudo attributes
  <verbatim|"id">, <verbatim|"type"> and <verbatim|"name"> referring to the
  first three children.

  <subsection|Conversion routines>

  <\explain>
    <scm|(db-load-entry <scm-arg|id>)>

    <scm|(assoc-list-\<gtr\>entry <scm-arg|id> <scm-arg|l>)>

    <scm|(db-load)>

    <scm|(db-load-types <scm-arg|types>)><explain-synopsis|database to
    markup>
  <|explain>
    Convert entries of the current database into <markup|db-entry> markup.
    <scm|db-load> returns a <markup|document> with all entries,
    <scm|db-load-types> only those of the given types. The hook
    <scm|db-load-post> is applied to each entry.
  </explain>

  <\explain>
    <scm|(entry-\<gtr\>assoc-list <scm-arg|t> . <scm-arg|opt-skip-pre?>)>

    <scm|(db-save <scm-arg|t>)>

    <scm|(db-save-types <scm-arg|t> <scm-arg|types>)>

    <scm|(db-save-selected <scm-arg|t> <scm-arg|pred?>)><explain-synopsis|markup
    to database>
  <|explain>
    <scm|db-save-selected> walks through a document, applies the hook
    <scm|db-save-pre> to each <markup|db-entry> or <markup|bib-entry> whose
    identifier is not yet in the database, and imports it with
    <scm|db-import-entry>. Entries which are already present are skipped:
    saving is an importation, not an update. Updates of existing entries are
    done by the editor (<scm|db-confirm-entries-in>, see <hlink|Editing
    databases|database-ui.en.tm>).
  </explain>

  The hooks <scm|db-save-pre> and <scm|db-load-post> are the extension
  points for kinds with a foreign native format. By default they rename the
  fields <verbatim|id>, <verbatim|type> and <verbatim|name> of the body to
  <verbatim|id*>, <verbatim|type*> and <verbatim|name*> and back, because
  these attributes are used for the first three children of
  <markup|db-entry> (for instance, the <BibTeX> field <verbatim|type> of a
  <verbatim|techreport> is stored under the attribute <verbatim|type*>).
  <verbatim|bib-db.scm> additionally converts <markup|bib-entry> markup with
  <scm|bib-\<gtr\>db> in <scm|db-save-pre>.

  <scm|(db-change-list <scm-arg|uid> <scm-arg|kind> <scm-arg|t>)> returns the
  entries of the given kind (owned by <scm-arg|uid> unless it is <scm|#t>)
  which were modified since time <scm-arg|t>, as triples of an identifier, a
  name and the current entry. It is the basis of <hlink|database
  synchronization|collab-sync.en.tm>.

  <section|Other users of the engine>

  Besides the bibliographic and AI agents kinds, the following code uses
  databases directly:

  <\itemize>
    <item>the <TeXmacs> server (<verbatim|server/*.scm>) stores accounts,
    files, versions, chat messages and notifications in
    <scm|(global-database)>, see <hlink|The <TeXmacs>
    server|collab-server.en.tm>;

    <item>the client keeps accounts in <scm|(user-database "remote")> and
    synchronization records in <scm|(user-database "sync")>, see
    <hlink|Synchronization of files and databases|collab-sync.en.tm>;

    <item><verbatim|utils/literate/lp-build.scm> calls the glue
    (<scm|tmdb-set-field>, <scm|tmdb-get-field>) directly on
    <verbatim|system/database/lp-master.tmdb> to remember build times.
  </itemize>

  <section|Adding a new kind of database>

  The kind <verbatim|"ai-agents"> is a small and complete example. The
  steps are:

  <\enumerate>
    <item>Declare the entry types and their formats, preferably in a module
    that uses <verbatim|(database db-convert)> and <verbatim|(database
    db-edit)>:

    <\scm-code>
      (smart-table db-kind-table

      \ \ ("ai-agents" ("corrector" "interlocutor" "translator")))

      \;

      (smart-table db-format-table

      \ \ ("corrector" (and "instructions"))

      \ \ ...)
    </scm-code>

    <item>Provide an accessor for the user database of this kind and
    the routines needed by the rest of <TeXmacs>, all written inside
    <scm|with-database>:

    <\scm-code>
      (tm-define (ai-agents-database) (user-database "ai-agents"))

      \;

      (tm-define (ai-agents-correctors)

      \ \ (with-database (ai-agents-database)

      \ \ \ \ (with ids (db-search `(("type" "corrector")))

      \ \ \ \ \ \ ...)))
    </scm-code>

    <item>Write an editing style <verbatim|database-<em|kind>.ts> which
    uses the package <verbatim|database> and sets <verbatim|db-kind> (see
    <verbatim|styles/test/database-ai-agents.ts>), and a mode which detects
    it, redefining <scm|db-get-kind> in this mode so that
    <scm|user-database> and the <menu|Insert> menu select the right kind:

    <\scm-code>
      (texmacs-modes

      \ \ (in-ai-agents% (style-has? "database-ai-agents-style")))

      \;

      (tm-define (db-get-kind)

      \ \ (:mode in-ai-agents?)

      \ \ "ai-agents")
    </scm-code>

    <item>Open the database as a document with <scm|(load-db-buffer
    "tmfs://db/ai-agents/global")>, register the new modules with
    <scm|lazy-define> in <verbatim|init-texmacs.scm>, and add a menu entry
    (see <scm|db-menu> in <verbatim|db-menu.scm>).

    <item>Optionally, redefine <scm|db-pretty> for the kind (used by the
    <verbatim|"Pretty"> presentation and by the search dialogue), and
    <scm|db-save-pre>/<scm|db-load-post> if entries need to be converted.
  </enumerate>

  If values should not be stored as <TeXmacs> snippets, add entries to
  <scm|db-encoding-table> (and, for a new encoding, to
  <scm|db-encoder-table> and <scm|db-decoder-table>).

  <section|Pitfalls>

  <\itemize>
    <item>Layers are combined through <scm|tm-define> overloading, so the
    behavior of <scm|db-get-field> depends on which modules have been
    loaded. Code that only uses <verbatim|(database db-base)> sees raw
    strings; code that uses <verbatim|db-format> or later layers sees decoded
    <TeXmacs> content.

    <item>Decoded values are <scheme> trees, not necessarily strings; for
    instance a field entered in the editor may come back as a
    <markup|concat>. Code such as <scm|(car (db-get-field id "name"))>
    should check for empty lists and non-string values.

    <item>Context variables are global and bound dynamically. A callback
    (for instance a widget action or a <scm|delayed> command) does not
    inherit the database of the code that created it; it must use
    <scm|with-database> itself. <scm|open-db-chooser> even calls
    <scm|db-reset>.

    <item><scm|db-save> skips entries whose identifier already exists. To
    update an entry, use <scm|db-update-entry> (which changes its
    identifier) or <scm|db-set-entry>.

    <item><scm|user-database> without argument depends on the mode of the
    current buffer through <scm|db-get-kind>. Pass the kind explicitly in
    code which may run in another buffer.
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
