<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The database engine>

  <section|Data model>

  <subsection|Lines, atoms and time>

  A <TeXmacs> database is a list of <em|lines>. A line records that, during a
  certain time interval, the <em|identifier> <math|i> has the <em|value>
  <math|v> for the <em|attribute> <math|a>. Identifiers, attributes and
  values are arbitrary strings; an identifier together with all its lines
  is called an <em|entry>, and the lines of an entry which share an
  attribute form a <em|field>, which may therefore have several values. The
  class <cpp|db_line> of <verbatim|Plugins/Database/database.hpp> is:

  <\cpp-code>
    typedef int db_atom;

    typedef double db_time;

    #define DB_MAX_TIME ((db_time) 10675199166.0)

    \;

    class db_line_rep: public concrete_struct {

    public:

    \ \ db_atom id;

    \ \ db_atom attr;

    \ \ db_atom val;

    \ \ db_time created;

    \ \ db_time expires;

    \ \ ...

    };
  </cpp-code>

  All strings are <em|interned>: the first time a string is seen, it gets
  the next free integer code (an <em|atom>), and all tables work on atoms.
  The mapping is stored in <cpp|atom_encode> (a <cpp|hashmap\<less\>string,db_atom\<gtr\>>)
  and <cpp|atom_decode> (an array). Atoms are never removed.

  Times are <abbr|UNIX> time stamps in seconds, stored as doubles but
  written to disk as integers. A line is <em|alive> at time <math|t> if
  <cpp|created \<less\>= t \<less\> expires>; a line which has not been
  removed has <cpp|expires == DB_MAX_TIME>. Nothing is ever deleted from the
  table: removing a field or an entry only sets the expiration date of its
  lines. This is what makes it possible to query any past state of a
  database. The special time <math|0> means \Pat any time\Q: queries with
  <math|t=0> see all lines, dead or alive. On the <scheme> side this
  corresponds to <scm|(with-time :always ...)>, which is used for instance
  by <scm|recent-preferred-databases> to recover all past values of a field.

  Setting a field at time <math|t> expires all alive lines of this field at
  <math|t> and appends new lines created at <math|t>, one for each value;
  setting an entry does the same for all fields of the entry. Consequently
  an update always grows the table, even if the new values are identical to
  the old ones; the <scheme> layer <scm|db-update-entry> avoids useless
  updates by comparing entries first.

  <subsection|Indices>

  Besides the table <cpp|db> of lines, <cpp|database_rep> maintains:

  <\itemize>
    <item><cpp|id_lines[id]> and <cpp|val_lines[val]>: the numbers of the
    lines with a given identifier <abbr|resp.> value (for each atom, whether
    it is used as an identifier, attribute or value);

    <item><cpp|ids_list> and <cpp|ids_set>: all identifiers ever used in a
    line, in order of first appearance;

    <item>the keyword index (<cpp|key_encode>, <cpp|key_decode>,
    <cpp|key_occurrences>, <cpp|key_completions>, <cpp|atom_indexed>) and
    the name index (<cpp|name_completions>, <cpp|name_indexed>) described in
    the section on searching below.
  </itemize>

  All indices are rebuilt in memory when the database is loaded; only the
  journal of lines and atoms is stored on disk.

  <section|The <c++> interface>

  <subsection|The class <cpp|database>>

  <cpp|database> is a concrete (reference counted) handle to
  <cpp|database_rep>. The public methods of <cpp|database_rep> work on atoms:

  <\explain>
    <cpp|void set_field (db_atom id, db_atom attr, db_atoms vals, db_time
    t)>

    <cpp|db_atoms get_field (db_atom id, db_atom attr, db_time t)>

    <cpp|void remove_field (db_atom id, db_atom attr, db_time t)>

    <cpp|db_atoms get_attributes (db_atom id, db_time t)>

    <cpp|void set_entry (db_atom id, db_atoms pairs, db_time t)>

    <cpp|db_atoms get_entry (db_atom id, db_time t)>

    <cpp|void remove_entry (db_atom id, db_time t)><explain-synopsis|basic
    operations>
  <|explain>
    An entry is represented as a flat array <cpp|pairs> of alternating
    attributes and values. <cpp|get_field>, <cpp|get_attributes> and
    <cpp|get_entry> only consider the lines alive at time <cpp|t> (all lines
    if <cpp|t == 0>). The modifying routines call the notification hooks
    <cpp|notify_created_atom>, <cpp|notify_extended_field> and
    <cpp|notify_removed_field>, which append the change to the pending
    journal (see the section on storage below).
  </explain>

  <\explain>
    <cpp|db_atoms query (tree q, db_time t, query_args qargs)><explain-synopsis|search
    for entries>
  <|explain>
    Returns the identifiers of the entries which satisfy the query <cpp|q>
    at time <cpp|t>; <cpp|query_args> contains a <cpp|limit>, an
    <cpp|offset> and a <cpp|sort_flag>. The format of queries is described in
    the section on searching below.
  </explain>

  <\explain>
    <cpp|bool atom_exists (string s)>

    <cpp|db_atom as_atom (string s)>

    <cpp|string from_atom (db_atom a)>

    <cpp|tree as_tuple (db_atoms a)>

    <cpp|db_atoms entry_as_atoms (tree t)>

    <cpp|tree entry_from_atoms (db_atoms pairs)><explain-synopsis|conversions>
  <|explain>
    Conversions between strings and atoms, and between the flat
    representation of entries and the <scheme> representation. On the
    <scheme> side an entry is a list of fields <scm|((<scm-arg|attr>
    <scm-arg|val1> <scm-arg|val2> ...) ...)>; as a <cpp|tree> (via
    <cpp|scheme_tree>) this is a tuple of tuples of quoted strings, which
    explains the calls to <cpp|scm_quote> and <cpp|scm_unquote>.
    <cpp|as_atom> creates the atom if necessary <em|and journals its
    creation>, see the pitfalls at the end of this page.
  </explain>

  <subsection|The functional interface and the <scheme> glue>

  The rest of <TeXmacs> never manipulates <cpp|database> objects directly.
  Databases are designated by their <abbr|URL>, and <verbatim|database.cpp>
  keeps a global table of all databases which have been opened in this
  process:

  <\cpp-code>
    array\<less\>database\<gtr\> dbs;

    hashmap\<less\>tree,int\<gtr\> db_index;

    \;

    database

    get_database (url u) {

    \ \ check_for_updates ();

    \ \ if (!db_index-\<gtr\>contains (u-\<gtr\>t)) {

    \ \ \ \ db_index (u-\<gtr\>t)= N(dbs);

    \ \ \ \ dbs \<less\>\<less\> database (u);

    \ \ }

    \ \ return dbs [db_index [u-\<gtr\>t]];

    }
  </cpp-code>

  A database is loaded the first time it is used and stays in memory until
  the end of the session. The following functions, declared at the end of
  <verbatim|database.hpp>, convert their string arguments to atoms and
  call the corresponding method. They are exported to <scheme> by
  <verbatim|build-glue-basic.scm>; the time argument is a double and
  <scheme> code normally obtains it from <scm|(db-get-time)>.

  <\explain>
    <scm|(tmdb-set-field <scm-arg|db> <scm-arg|id> <scm-arg|attr>
    <scm-arg|vals> <scm-arg|t>)>

    <scm|(tmdb-get-field <scm-arg|db> <scm-arg|id> <scm-arg|attr>
    <scm-arg|t>)>

    <scm|(tmdb-remove-field <scm-arg|db> <scm-arg|id> <scm-arg|attr>
    <scm-arg|t>)>

    <scm|(tmdb-get-attributes <scm-arg|db> <scm-arg|id>
    <scm-arg|t>)><explain-synopsis|fields>
  <|explain>
    <c++>: <cpp|set_field>, <cpp|get_field>, <cpp|remove_field>,
    <cpp|get_attributes>. <scm-arg|vals> and the results are lists of
    strings.
  </explain>

  <\explain>
    <scm|(tmdb-set-entry <scm-arg|db> <scm-arg|id> <scm-arg|l>
    <scm-arg|t>)>

    <scm|(tmdb-get-entry <scm-arg|db> <scm-arg|id> <scm-arg|t>)>

    <scm|(tmdb-remove-entry <scm-arg|db> <scm-arg|id>
    <scm-arg|t>)><explain-synopsis|entries>
  <|explain>
    <c++>: <cpp|set_entry>, <cpp|get_entry>, <cpp|remove_entry>. The entry
    <scm-arg|l> is a list <scm|(("type" "article") ("author" "A" "B")
    ...)>; <cpp|get_entry> groups the values by attribute, in the order of
    the first occurrence of each attribute.
  </explain>

  <\explain>
    <scm|(tmdb-query <scm-arg|db> <scm-arg|q> <scm-arg|t> <scm-arg|limit>
    <scm-arg|offset>)><explain-synopsis|search>
  <|explain>
    <c++>: <cpp|query>. Returns a list of identifiers.
  </explain>

  <\explain>
    <scm|(tmdb-get-completions <scm-arg|db> <scm-arg|prefix>)>

    <scm|(tmdb-get-name-completions <scm-arg|db>
    <scm-arg|prefix>)><explain-synopsis|completion>
  <|explain>
    <c++>: <cpp|get_completions>, <cpp|get_name_completions>. The first
    function completes a prefix into indexed keywords, the second into
    values of <verbatim|name> fields (used for completing citation keys).
  </explain>

  <\explain>
    <scm|(tmdb-keep-history <scm-arg|db> <scm-arg|flag?>)><explain-synopsis|history
    policy>
  <|explain>
    <c++>: <cpp|keep_history>. By default the whole history is kept. When
    the history is not kept, the database file is periodically compressed
    by rewriting it with the alive lines only (see
    the section on storage below). The <scheme> macro
    <scm|with-database*> selects a database and disables its history; it is
    used for bookkeeping databases such as <scm|(user-database "sync")>.
  </explain>

  <\explain>
    <scm|(tmdb-inspect-history <scm-arg|db> <scm-arg|name>)><explain-synopsis|debugging>
  <|explain>
    <c++>: <cpp|inspect_history>. Prints on standard output all lines with
    attribute <verbatim|name> and value <scm-arg|name>, together with their
    creation and expiration dates.
  </explain>

  <section|Searching>

  <subsection|Queries>

  A query is a list of <em|constraints>; an entry matches if it satisfies all
  constraints at the query time. At the <c++> level a query is a
  <cpp|tree> built from a <scheme> list, in which strings are quoted and
  symbols are not. The following constraints are recognized by
  <cpp|database_rep::encode_constraint> (<verbatim|db_query.cpp>) and
  <cpp|normalize_query> (<verbatim|db_index.cpp>):

  <\description>
    <item*|<scm|(<scm-arg|attr> <scm-arg|val1> ... <scm-arg|valn>)>>where
    <scm-arg|attr> is a string: the entry has a line with attribute
    <scm-arg|attr> and one of the given values. Values which have never been
    interned are dropped; if no value remains the constraint cannot be
    satisfied.

    <item*|<scm|(any <scm-arg|val1> ... <scm-arg|valn>)>>(symbol
    <scm|any>): some attribute has one of the given values.

    <item*|<scm|(contains <scm-arg|text>)>>the text is split into keywords
    (see below) and each keyword becomes a <scm|keywords> constraint.

    <item*|<scm|(completes <scm-arg|text>)>>like <scm|contains>, except
    that the last keyword is replaced by the list of all its completions.
    This implements \Psearch as you type\Q.

    <item*|<scm|(keywords <scm-arg|kw1> ... <scm-arg|kwn>)>>some value of
    the entry contains one of the keywords. It is encoded as an
    <scm|any> constraint over all values in which the keywords occur. If
    this list contains more than 1000 values, the constraint is considered
    too weak and is ignored (it is always satisfied).

    <item*|<scm|(order <scm-arg|attr> <scm-arg|asc?>)>>always satisfied;
    requests sorting of the results on <scm-arg|attr>.

    <item*|<scm|(modified <scm-arg|t1> <scm-arg|t2>)>>where the times are
    strings: the entry has a line created or expired in the interval
    <math|[t1,t2)> (lines which were both created and removed inside the
    interval are ignored). This is used by <scm|db-change-list> for
    synchronization.
  </description>

  The <scheme> function <scm|db-search> accepts the more readable keyword
  forms <scm|(:order <scm-arg|attr> <scm-arg|asc?>)>,
  <scm|(:modified <scm-arg|t1> <scm-arg|t2>)>, <scm|(:match
  <scm-arg|text>)> and <scm|(:contains <scm-arg|text>)> (both mapped to
  <scm|contains>), and <scm|(:prefix <scm-arg|text>)> and
  <scm|(:completes <scm-arg|text>)> (both mapped to <scm|completes>); see
  <scm|rewrite-query> in <verbatim|db-base.scm>.

  <subsection|Evaluation of a query>

  <cpp|database_rep::query> proceeds as follows:

  <\enumerate>
    <item><cpp|normalize_query> expands <scm|contains> and
    <scm|completes> into <scm|keywords> constraints.

    <item><cpp|ansatz> chooses the most selective constraint, namely the one
    for which the total number of lines with one of its values
    (<cpp|compute_complexity>) is minimal, and computes the candidate
    identifiers from <cpp|val_lines>. If there is no usable constraint (for
    instance for the empty query, or when all constraints are
    <scm|order>/<scm|modified>), all identifiers are candidates.

    <item><cpp|filter> checks the remaining constraints for each candidate
    using <cpp|id_lines>. It skips the first <cpp|offset> candidates and
    stops after <cpp|limit> matches (no limit if <cpp|limit == 0>). When the
    query contains an <scm|order> constraint the limit is raised to at least
    1000, since sorting happens afterwards.

    <item><scm|modified> constraints are applied by <cpp|filter_modified>.

    <item><cpp|sort_results> (<verbatim|db_sort.cpp>) sorts the results
    lexicographically on the values of the <scm|order> attributes
    (the last alive value of each attribute is used, and the identifier
    breaks ties). Only the direction of the first <scm|order> constraint is
    taken into account.
  </enumerate>

  Notice that the limit and offset are applied <em|before> sorting and
  before the <scm|modified> filter, so that paginated or limited sorted
  queries are only approximately correct on large databases.

  <subsection|Keywords and completions>

  Whenever a line is created (<cpp|extend_field>), its value is indexed by
  <cpp|indexate>, unless the attribute is <verbatim|contributor>; values of
  <verbatim|name> fields are moreover indexed by <cpp|indexate_name>.

  <\itemize>
    <item><cpp|compute_keywords> parses the value as a serialized <TeXmacs>
    snippet (<cpp|texmacs_to_tree>), and splits all its strings into
    keywords made of digits, lowercase letters and underscores, after
    transliteration to <abbr|ASCII> with <cpp|uni_translit> and conversion to
    lowercase. Hence accented letters match their unaccented
    versions in searches, and markup in values does not produce spurious
    keywords.

    <item>Each new keyword <math|k> is registered in
    <cpp|key_completions[p]> for all prefixes <math|p> of <math|k> of at
    most 6 characters (<verbatim|MAX_PREFIX_LENGTH>). To complete a prefix,
    <cpp|compute_completions> looks up its first 6 characters and filters
    the candidates with <cpp|starts>.

    <item>The name index works in the same way, but on the full values of
    <verbatim|name> fields, without normalization.
  </itemize>

  The index is purely in memory and only grows: values which are no longer
  alive remain indexed, which is harmless because the filter checks the
  lines at the query time.

  <section|Storage on disk>

  <subsection|The journal format>

  A <verbatim|.tmdb> file is a binary journal of the modifications of the
  database, which is replayed when the database is loaded
  (<cpp|database_rep::initialize> and <cpp|replay> in
  <verbatim|db_disk.cpp>). It is a sequence of commands, each starting with
  one byte:

  <\description>
    <item*|<verbatim|1> (<verbatim|DB_CREATE_ATOM>)>followed by a string:
    creates the next atom. Atoms are numbered implicitly, in the order of
    these commands.

    <item*|<verbatim|2> (<verbatim|DB_CREATE_FIELD>)>followed by four
    numbers: identifier, attribute, value (as atoms) and creation time.
    Appends a line.

    <item*|<verbatim|3> (<verbatim|DB_REMOVE_FIELD>)>followed by two
    numbers: a line number and an expiration time.
  </description>

  Numbers <math|n\<less\>248> are written as the single byte <math|n+8>;
  larger numbers are written as a byte <math|l> giving their length
  followed by <math|l> bytes in little endian order. Strings are written as
  their length followed by their bytes. Since line numbers are implicit as
  well, the journal can only be interpreted from its beginning.

  <subsection|Writing>

  Modifications are not written immediately. The notification hooks append
  the encoded commands to the string <cpp|pending>, and the function
  <cpp|sync_databases> is called by the main loop
  (<cpp|tm_server_rep::interpose_handler>). For each open database it calls
  <cpp|purge>, which writes the pending commands:

  <\itemize>
    <item>if there are at most 4096 bytes, they are saved to a temporary
    file <verbatim|<em|name>.append-<em|random>> which is then appended to
    the database with <cpp|append_to>;

    <item>otherwise the whole new contents (<cpp|loaded * pending>) are
    saved to <verbatim|<em|name>.replace-<em|random>>, which is then moved
    over the database file.
  </itemize>

  In both cases the operation is cancelled if the modification time of the
  file is more recent than the time stamp recorded at the last read or
  write, so that another process which modified the file in the meantime is
  not overwritten. The pending changes are then kept and written after the
  next reload.

  If the history is not kept and more than half of the lines are dead,
  <cpp|sync_databases> instead builds a compressed clone with the alive
  lines only (<cpp|compress>), writes it to a temporary file, moves it over
  the original file and replaces the in-memory database by the clone.

  <subsection|Concurrent access>

  Several <TeXmacs> processes may use the same database files (typically
  the user databases). Before writing, <cpp|sync_databases> calls
  <cpp|check_for_updates>, which reloads every database whose file was
  modified on disk since it was last read or written, and replays the lines
  created locally since the last write (<cpp|replay (clone, start_pending,
  true)>) on top of the reloaded database. After a call to
  <cpp|sync_databases>, the next access through <cpp|get_database> performs
  the same check. The detection relies on file modification times with a
  resolution of one second. The comments in the code acknowledge that the
  test and the write are not a single atomic operation and that the reload
  is not incremental; this mechanism should therefore be understood as a
  best effort for occasional concurrent use, not as a transactional
  database.

  <section|Pitfalls and limitations>

  <\itemize>
    <item><em|Reading may write.> The functional interface converts all
    strings to atoms with <cpp|as_atom>, which creates and journals atoms
    for unknown strings. Reading a field of an unknown identifier, or
    testing whether an identifier is free (<scm|db-create-id> does this in
    a loop), therefore appends a few bytes to the database file.

    <item><em|No real deletion.> With history enabled (the default), a
    database only grows. Use <scm|with-database*> (or
    <scm|tmdb-keep-history>) for bookkeeping data which changes often.

    <item><em|Times have a resolution of one second.> Two modifications of
    the same field within the same second create lines which are never
    alive (created and expired at the same time), which is correct for the
    current state but loses intermediate history.

    <item><em|Limits.> <scm|db-search> uses a default limit of
    <math|10<rsup|6>> results, and sorted queries consider at most
    <math|max(limit,1000)> matches before sorting.

    <item><em|Concurrent replays lose some information.> When a database is
    reloaded because another process modified it, only the lines created
    locally since the last write are replayed; a pending removal of a line
    written earlier is not replayed, and replayed lines which were removed
    get their creation date as expiration date.

    <item><em|No <name|SQLite>.> The functions <scm|sql-exec>,
    <scm|sql-quote> and <scm|supports-sql?> (<verbatim|Plugins/Sqlite3/>)
    are an older experiment which is not used by the database engine nor by
    any <scheme> module.
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
