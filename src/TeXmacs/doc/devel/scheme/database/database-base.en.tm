<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <TeXmacs> database model>

  The <TeXmacs> database manipulation API has mainly been designed for
  internal use. It is based on a<nbsp>dedicated
  <hlink|NoSQL|http://en.wikipedia.org/wiki/NoSQL>-style database model,
  using a variant of <hlink|column data stores|http://en.wikipedia.org/wiki/Column_%28data_store%29>.
  For the moment, we only support a limited number of entry types and field
  types, although new types can easily be added later. Currently, databases
  are used for managing remote files, bibliographies, user lists, versions,
  etc.

  The interface has been kept to be as simple as possible, so that our low
  level implementation can be most easily optimized for efficiency when
  needed. Furthermore, the routines of our basic API can all be customized
  <em|a posteriori> to add specific features. For instance, the basic API is
  string-based, so a<nbsp>special additional layer was added to support
  <TeXmacs> snippets as values instead of strings. Similarly, an additional
  layer was added for managing the permissions of specific users. The
  advantage of this design based on <em|a posteriori> customizations is that
  the routines in the basic API always keep the same semantics, no matter how
  many additional layers are added.

  A <TeXmacs> database is always a collection of database <em|entries>. Each
  entry consists of a <em|unique identifier> and a list of <em|fields>. Each
  field consists of an <em|attribute>, a list of <em|values>, a <em|creation
  date> and an <em|expiration date>. The creation and expiration dates
  cannot be manipulated directly, but it is possible to specify an
  alternative time for database queries, which make it possible to easily
  recover any past state of the database.

  The basic <scheme> API is implemented in <source-link|database/db-base.scm|TeXmacs/progs/database/db-base.scm>
  on top of a few glued <c++> routines (<scm|tmdb-set-field>,
  <scm|tmdb-get-field>, <scm|tmdb-set-entry>, <scm|tmdb-query>,
  <abbr|etc.>) whose implementation can be found in
  <source-link|src/src/Plugins/Database/|src/Plugins/Database>. The extensions described in the
  next sections are implemented in further files of the
  <verbatim|database/> directory by overloading the basic routines with
  <scm|tm-define>. The internals of the database engine (storage on disk,
  indexation, queries) are described in more detail in <hlink|the
  <TeXmacs> database|../../source/database.en.tm>.

  <paragraph|Macros for context specification>

  <\explain>
    <scm|(with-database db . body)><explain-synopsis|specify a database>
  <|explain>
    Execute <scm|body> with <scm|db> as the current database. Here <scm|db>
    should be an URL with extension <verbatim|.tmdb>. The database <scm|db>
    will be used by all routines of the database API called from within
    <scm|body>. If no database has been specified, then these routines raise
    an error. The variant <scm|(with-database* db . body)> in addition
    switches off the recording of the history of modifications of
    <scm|db> (see <scm|tmdb-keep-history>).
  </explain>

  <\explain>
    <scm|(with-time t . body)><explain-synopsis|specify a time>
  <|explain>
    Execute <scm|body> with <scm|t> as the current time. All database queries
    inside <scm|body> become relative to the time<nbsp><scm|t>, which allows
    for the inspection of past states of the database. The parameter <scm|t>
    is a number (or a string containing a number) representing a UNIX time
    stamp, <scm|:now> (the default) or <scm|:always>. The special value
    <scm|:always> disables the time filter, so that queries take into
    account all past and present values. Modifications of the database are
    normally made at the current time <scm|:now>.
  </explain>

  <\explain>
    <scm|(with-time-stamp on? . body)><explain-synopsis|add date field to new
    entries>
  <|explain>
    Whenever <scm|on?> holds, a <scm|date> attribute (with the current time
    as its value) will automatically be added by <scm|db-set-entry> to all
    entries which do not already contain a <scm|date> field. For entries which circulate among several users, this
    allows you to determine when they were created for the first time.
  </explain>

  <\explain>
    <scm|(with-extra-fields l . body)><explain-synopsis|add fields to
    entries>
  <|explain>
    Whenever an entry with fields <scm|new-l> is set using
    <scm|db-set-entry> (or created using <scm|db-create-entry>) inside
    <scm|body>, the list of fields <scm|l> is automatically added to
    <scm|new-l>. Fields of <scm|new-l> whose attributes occur in <scm|l> are
    discarded, so the extra fields take precedence. Nested uses of
    <scm|with-extra-fields> accumulate their field lists.
  </explain>

  <\explain>
    <scm|(with-limit limit . body)><explain-synopsis|limit number of return
    values>
  <|explain>
    For queries of the database inside <scm|body>, limit the number of
    returned values to <scm|limit>. By default, at most 1000000 identifiers
    are returned.
  </explain>

  <paragraph|Special attributes>

  <\description>
    <item*|<scm|name>>A name (or key) for the entry, by which it can be
    referred to. Values of <scm|name> fields are indexed separately for
    completion (see <scm|index-get-name-completions>), and an entry is
    considered to exist if and only if it has a <scm|name> field.

    <item*|<scm|date>>Creation date stamp for the entry, as determined by
    <scm|with-time-stamp>.
  </description>

  <paragraph|Main routines of the database API>

  <\explain>
    <scm|(db-set-field id attr vals)><explain-synopsis|set values for a given
    field>
  <|explain>
    For the field with attribute <scm|attr> in the entry with identifier
    <scm|id>, set the values to <scm|vals>, a list of strings.
  </explain>

  <\explain>
    <scm|(db-get-field id attr)><explain-synopsis|get all values for a given
    field>
  <|explain>
    Get the list of values for the field with attribute <scm|attr> in the
    entry with identifier <scm|id>.
  </explain>

  <\explain>
    <scm|(db-remove-field id attr)><explain-synopsis|remove a field>
  <|explain>
    Remove the field with attribute <scm|attr> from the entry with
    identifier <scm|id>.
  </explain>

  <\explain>
    <scm|(db-get-attributes id)><explain-synopsis|get the list of attributes>
  <|explain>
    Get the list of attributes for the entry with identifier <scm|id>.
  </explain>

  <\explain>
    <scm|(db-set-entry id l)><explain-synopsis|fill out a complete entry>
  <|explain>
    For the entry with identifier <scm|id>, set the list of fields to
    <scm|l>. Each field is a list <scm|(attr val1 ... valn)> of an attribute
    and its values.
  </explain>

  <\explain>
    <scm|(db-get-entry id)><explain-synopsis|retrieve a complete entry>
  <|explain>
    Get the list of fields for the entry with identifier <scm|id>.
  </explain>

  <\explain>
    <scm|(db-remove-entry id)><explain-synopsis|remove a complete entry>
  <|explain>
    Remove the entry with identifier <scm|id>.
  </explain>

  <\explain>
    <scm|(db-create-id)><explain-synopsis|create a unique identifier>
  <|explain>
    Create an identifier which does not yet exist in the database. If no
    current database has been specified, then a unique identifier is
    returned without any further checks.
  </explain>

  <\explain>
    <scm|(db-search q)><explain-synopsis|search for a list of fields>
  <|explain>
    Return the list of identifiers of entries which match a given query
    <scm|q>. The query <scm|q> is a list of constraints of the form
    <scm|(attr val1 ... valn)>. Each constraint is interpreted as \Pthe
    attribute <scm|attr> of the entry is one of the values <scm|val1>,
    <math|\<ldots\>>, <scm|valn>\Q. In addition to these <em|basic>
    constraints, extensions of the database API may implement additional
    kinds of constraints. Such <em|supplementary> constraints are always
    formed by taking a special keyword for <scm|attr>.

    The basic API already implements one type of supplementary constraint of
    the form <scm|(:order attr asc?)>, where <scm|attr> is an attribute and
    <scm|asc?> a boolean value. This kind of supplementary constraint is
    always satisfied and has the effect of ordering the output of the query
    on the attribute <scm|attr> in ascending or descending order, depending
    on <scm|asc?>. Another supplementary constraint <scm|(:modified t1 t2)>
    restricts the output to entries which were modified between the times
    <scm|t1> and <scm|t2> (strings containing integers); it is typically
    used in combination with <scm|(with-time :always ...)>. Finally, the
    keyword constraints <scm|:match> and <scm|:prefix> (with the synonyms
    <scm|:contains> and <scm|:completes>) are described in the section
    about <hlink|indexation|database-index.en.tm>.
  </explain>

  <\explain>
    <scm|(db-search-paginate q limit offset)><explain-synopsis|search with
    pagination>
  <|explain>
    Similar to <scm|db-search>, but skip the first <scm|offset> candidate
    entries and return at most <scm|limit> identifiers.
  </explain>

  <paragraph|Other useful routines>

  <\explain>
    <scm|(db-get-field-first id attr default)><explain-synopsis|get first
    value for a given field>
  <|explain>
    Get the first value in <scm|(db-get-field id attr)>, or <scm|default> if
    there are no values.
  </explain>

  <\explain>
    <scm|(db-create-entry l)><explain-synopsis|create a new entry>
  <|explain>
    Create a new entry in the current database with fields <scm|l>, and
    return the identifier of the newly created entry.
  </explain>

  <\explain>
    <scm|(db-entry-exists? id)><explain-synopsis|test existence of entry>
  <|explain>
    Test whether there exists an entry with identifier <scm|id>, <abbr|i.e.>
    whether the entry has a non-empty <scm|name> field.
  </explain>

  <\explain>
    <scm|(db-search-name name)>

    <scm|(db-search-owner owner)><explain-synopsis|search by name or owner>
  <|explain>
    Shorthands for <scm|(db-search (list (list "name" name)))> and
    <scm|(db-search (list (list "owner" owner)))>.
  </explain>

  <\explain>
    <scm|(db-reset)><explain-synopsis|reset the database context>
  <|explain>
    Reset the current database, the current time and the other context
    variables of the database API (and its extensions) to their default
    values.
  </explain>

  <\explain>
    <scm|(global-database)>

    <scm|(db-get-time)><explain-synopsis|context>
  <|explain>
    The URL of the database <verbatim|$TEXMACS_HOME_PATH/server/global.tmdb>,
    <abbr|resp.> the time used by the queries (see <scm|with-time>), as a
    floating point number.
  </explain>

  <\explain>
    <scm|(db-inspect-history name)><explain-synopsis|debugging>
  <|explain>
    Print on the standard output the identifiers of the entries whose
    <scm|name> field has, or had, the value <scm|name>, with the creation
    and expiration dates of these fields.
  </explain>

  <paragraph|Glued routines>

  The routines above call the following glued routines, which take the URL
  <scm|db> of the database and, for most of them, the time <scm|t> of the
  query explicitly: <scm|(tmdb-set-field db id attr vals t)>,
  <scm|(tmdb-get-field db id attr t)>, <scm|(tmdb-remove-field db id attr
  t)>, <scm|(tmdb-get-attributes db id t)>, <scm|(tmdb-set-entry db id l
  t)>, <scm|(tmdb-get-entry db id t)>, <scm|(tmdb-remove-entry db id t)>,
  <scm|(tmdb-query db q t limit offset)>, <scm|(tmdb-keep-history db
  flag)>, <scm|(tmdb-inspect-history db name)>, <scm|(tmdb-get-completions
  db prefix)> and <scm|(tmdb-get-name-completions db prefix)>.

  <paragraph|Other modules>

  The other files of the <verbatim|database/> directory build on this API:

  <\description>
    <item*|<source-link|db-users.scm|TeXmacs/progs/database/db-users.scm>>Users
    and permissions (see <hlink|users, groups and
    permissions|database-permissions.en.tm>), and the database of each
    user and each kind of data: <scm|(user-database kind)> returns the
    database of the current user for <scm|kind> (such as <scm|"bib">),
    which is stored with <scm|(set-preferred-database uid kind db)> and
    retrieved with <scm|(get-preferred-database uid kind)>;
    <scm|(use-database db)> makes <scm|db> the database of the current kind
    and <scm|(add-user pseudo name)> registers a user.

    <item*|<source-link|db-convert.scm|TeXmacs/progs/database/db-convert.scm>>Conversion
    between the entries of the database and <TeXmacs> markup:
    <scm|(db-load-entry id)> returns an entry as a <markup|db-entry> tree,
    <scm|(db-load)> all the entries of the current database as a document,
    and <scm|(db-save doc)> stores the entries of a document. The predicate
    <scm|(db-url? u)> tests whether <scm|u> is a database
    (<verbatim|.tmdb> file or <verbatim|tmfs://db/> url).

    <item*|<source-link|db-tmfs.scm|TeXmacs/progs/database/db-tmfs.scm>>The
    buffers <verbatim|tmfs://db/<em|kind>/<em|file>>, which show the
    entries of a database, with the search, order and limit of the query
    kept in preferences (<scm|db-get-query-preference> and
    <scm|db-set-query-preference>).

    <item*|<source-link|bib-db.scm|TeXmacs/progs/database/bib-db.scm>>The
    bibliographic database <scm|(bib-database)>, that is
    <scm|(user-database "bib")>, and the conversions <scm|(db-\<gtr\>bib
    t)> and <scm|(bib-\<gtr\>db t)> between database entries and
    <BibTeX> entries (see <hlink|bibliography
    styles|../bibliography/bibliography.en.tm>).
  </description>

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>