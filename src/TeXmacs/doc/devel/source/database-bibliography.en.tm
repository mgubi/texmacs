<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Bibliographies>

  <section|Introduction>

  This page follows a citation from the document to the typeset
  bibliography, and describes the <BibTeX> machinery which is involved:
  the parser and converters for <verbatim|.bib> files, the external
  <verbatim|bibtex> program, the internal style engine written in
  <scheme>, and the bibliographic database. The user side is described in
  <hlink|Compiling a bibliography|../../main/links/man-bibliography.en.tm>
  and the markup in <hlink|the documentation of
  <verbatim|std-automatic>|../../main/styles/std/std-automatic-bib.en.tm>;
  the <scheme> functions available to style writers are documented in
  <hlink|Writing <TeXmacs> bibliography
  styles|../scheme/bibliography/bibliography.en.tm>.

  <section|Markup>

  <subsection|Citations>

  The citation macros are defined in
  <source-link|packages/standard/std-automatic.ts|TeXmacs/packages/standard/std-automatic.ts>. The central one is

  <\tm-fragment>
    <verbatim|\<less\>assign\|cite-arg\|\<less\>macro\|key\|\<less\>write\|\<less\>value\|bib-prefix\<gtr\>\|\<less\>arg\|key\<gtr\>\<gtr\>\<less\>reference\|\<less\>merge\|\<less\>value\|bib-prefix\<gtr\>\|-\|\<less\>arg\|key\<gtr\>\<gtr\>\<gtr\>\<gtr\>\<gtr\>>
  </tm-fragment>

  Each key of <markup|cite>, <markup|cite-detail> and <markup|cite-raw>
  therefore does two things: the primitive <markup|write> appends the key
  to the auxiliary list named by the environment variable
  <verbatim|bib-prefix> (<verbatim|"bib"> by default), and
  <markup|reference> refers to the label
  <verbatim|<em|prefix>-<em|key>>. <markup|nocite> only writes the keys;
  the special key <verbatim|*> stands for all entries of the bibliography
  file. <markup|with-bib> changes <verbatim|bib-prefix> locally, which is
  how a document can contain several independent bibliographies.
  <markup|cite-TeXmacs> (in <source-link|packages/header/title-base.ts|TeXmacs/packages/header/title-base.ts> and
  <source-link|header-article.ts|TeXmacs/packages/header/header-article.ts>) cites keys of the form
  <verbatim|TeXmacs:...>, which are provided by
  <verbatim|$TEXMACS_PATH/misc/bib/texmacs.bib>.

  The typesetter implements <markup|write> in
  <cpp|concater_rep::typeset_write> (<source-link|Typeset/Concat/concat_active.cpp|src/Typeset/Concat/concat_active.cpp>):
  during a complete typesetting pass it appends the evaluated second
  argument to <cpp|env-\<gtr\>local_aux[<em|name>]>, a <markup|document>
  which is stored in the buffer as <cpp|buf-\<gtr\>data-\<gtr\>aux> and
  saved with the document (in the <verbatim|auxiliary> part of the file).
  After typesetting, <verbatim|aux["bib"]> is thus the list of cited keys,
  in order of appearance and with repetitions. For a project, the auxiliary
  data of the master document is used.

  <subsection|The bibliography>

  The bibliography itself is the tag
  <verbatim|(bibliography <em|aux> <em|style> <em|file> <em|body>)> (or
  <markup|bibliography*> with an extra title argument), defined in
  <source-link|packages/section/section-base.ts|TeXmacs/packages/section/section-base.ts>: <em|aux> is the prefix
  (<verbatim|"bib">), <em|style> the name of a <BibTeX> style
  (<verbatim|plain>) or of an internal style (<verbatim|tm-plain>),
  <em|file> the <verbatim|.bib> file (possibly empty) and <em|body> the
  generated content. It is inserted by <scm|make-bib>,
  <scm|make-database-bib> (<source-link|text/text-edit.scm|TeXmacs/progs/text/text-edit.scm>) or the dialogue
  <scm|open-bibliography-inserter> (<source-link|bibtex/bib-widgets.scm|TeXmacs/progs/bibtex/bib-widgets.scm>, used
  when the preference <verbatim|"gui:new bibliography dialogue"> is set).

  The generated body is a <markup|bib-list>:

  <\tm-fragment>
    <verbatim|(bib-list <em|largest> (document (concat (bibitem* <em|label>)
    (label <em|prefix>-<em|key>) <em|text>) ...))>
  </tm-fragment>

  where <em|largest> is used to compute the width of the labels. The
  <markup|label> matches the <markup|reference> of the citations.

  <section|The pipeline>

  <subsection|Triggering the generation>

  <menu|Document|Update|Bibliography> calls <scm|(update-document
  "bibliography")> (<source-link|generic/document-edit.scm|TeXmacs/progs/generic/document-edit.scm>). It first calls
  <scm|zotero-before-update>, which refreshes a <verbatim|.bib> file
  written from Zotero and syncs the entries of the database which come
  from Zotero (see <hlink|Citations from Zotero|zotero.en.tm>), then runs
  <scm|generate-all-aux> and retypesets the buffer (as many times as
  specified by the preference <verbatim|"document update times">). Notice
  that this regenerates all automatic content, not only bibliographies.
  <scm|(generate-aux "bibliography")> regenerates only the bibliographies,
  but it still empties every other automatic section (tables of contents,
  indexes, ...) without filling it again, because
  <cpp|generate_aux_recursively> clears each automatic body before testing
  which kind was requested; see <hlink|automatic content|editing-auxiliary.en.tm>.

  <cpp|edit_process_rep::generate_aux> (<source-link|Edit/Process/edit_process.cpp|src/Edit/Process/edit_process.cpp>)
  walks through the document. For each automatic tag (<cpp|is_aux>) it
  replaces the body by an empty document, puts the cursor there and calls
  <cpp|generate_bibliography (<em|aux>, <em|style>, <em|file>)>, which
  eventually inserts the result with <cpp|insert_tree>. If the result is a
  string starting with <verbatim|Error:>, it is shown in the status bar
  instead.

  <subsection|Choice of the strategy>

  <cpp|edit_process_rep::generate_bibliography> first retrieves the list of
  keys <cpp|bib_t= buf-\<gtr\>data-\<gtr\>aux[bib]>, uses
  <verbatim|tm-plain> if the style is empty, copies a
  <verbatim|<em|style>.bst> file found next to the document into
  <verbatim|$TEXMACS_HOME_PATH/system/bib> (<cpp|copy_bst_file>), and looks
  for the bibliography file with <cpp|find_bib_file> (the name with suffix
  <verbatim|.bib>, as given, relative to the document, or in one of its
  ancestor directories). Then:

  <\enumerate>
    <item><em|No <verbatim|.bib> file was found.>

    <\enumerate>
      <item>If a file with suffix <verbatim|.bbl> exists instead, it is
      loaded with <cpp|bibtex_load_bbl>. This allows to ship a precompiled
      bibliography.

      <item>Otherwise, with the database tool, the <scheme> function
      <scm|bib-compile> is called with <verbatim|texmacs.bib> as the only
      file (entries then come from the database and the attachments, see
      below), followed by <scm|bib-attach>.

      <item>Without the database tool, the bibliography can only be
      produced if all keys start with <verbatim|TeXmacs:>, in which case
      <scm|bib-compile> is called in the same way; otherwise the error
      \PCould not find bibliography file\Q is reported.
    </enumerate>

    <item><em|A <verbatim|.bib> file was found.>

    <\enumerate>
      <item>If <verbatim|bibtex> is not installed or the style is internal
      (<verbatim|tm->), keys <verbatim|*> are replaced by all keys of the
      file (using <cpp|parse_bib>).

      <item>If <verbatim|bibtex> is not installed and the style is not
      internal, the style is replaced by its internal equivalent
      (<verbatim|tm-abbrv>, <verbatim|tm-acm>, <verbatim|tm-alpha>,
      <verbatim|tm-elsart-num>, <verbatim|tm-ieeetr>, <verbatim|tm-siam>,
      <verbatim|tm-unsrt>) or by <verbatim|tm-plain>.

      <item>With the database tool, <scm|bib-compile> is called with the
      absolute path of the file and, unless <verbatim|*> was cited,
      <verbatim|texmacs.bib>.

      <item>Without the database tool and with an internal style, the file
      and <verbatim|texmacs.bib> are parsed with <cpp|parse_bib>,
      <cpp|bib_entries> selects the cited entries (following
      <verbatim|crossref> fields and warning about missing or duplicate
      keys), the style module <verbatim|(bibtex <em|style>)> is loaded and
      the <scheme> function <scm|bib-process> formats the entries.

      <item>Otherwise the external program is run by <cpp|bibtex_run>.

      <item>With the database tool, <scm|bib-attach> is called afterwards.
      If the result uses <markup|natbib-triple> (produced by
      <name|natbib>-compatible <BibTeX> styles), the package
      <verbatim|cite-author-year> is added to the style of the document.
    </enumerate>
  </enumerate>

  In all cases the result goes through <cpp|arrange_bib>, which replaces
  <verbatim|--> by an en dash and empty documents by a document with an
  empty paragraph.

  <subsection|Running the external <BibTeX>>

  <\explain>
    <cpp|tree bibtex_run (string bib, string style, url bib_file, tree
    bib_t)><explain-synopsis|run <verbatim|bibtex>>
  <|explain>
    Defined in <source-link|Plugins/Bibtex/bibtex.cpp|src/Plugins/Bibtex/bibtex.cpp> (the variant taking an
    <cpp|array\<less\>string\<gtr\>> of keys is exported as
    <scm|bibtex-run>). It checks that the program (by default
    <verbatim|bibtex>, see the preference <verbatim|"bibtex command"> and
    <scm|set-bibtex-command>) is in the path (<scm|supports-bibtex?>), and
    that neither the style nor the file name contain spaces. If some keys
    start with <verbatim|TeXmacs:>, <cpp|complete_bib_file> creates a copy
    <verbatim|<em|name>-extended.bib> of the bibliography file next to it,
    with <verbatim|texmacs.bib> appended, and uses that copy instead. It then
    writes <verbatim|temp.aux> with <verbatim|\\bibstyle>,
    <verbatim|\\citation> and <verbatim|\\bibdata> commands in
    <verbatim|$TEXMACS_HOME_PATH/system/bib>, runs <verbatim|bibtex temp>
    in that directory with <verbatim|BIBINPUTS> and <verbatim|BSTINPUTS>
    extended by the directory of the bibliography file, reports the lines
    containing <verbatim|Warning--> on the <verbatim|bibtex-warning> debug
    channel, and loads <verbatim|temp.bbl>.
  </explain>

  <\explain>
    <cpp|tree bibtex_load_bbl (string bib, url bbl_file)><explain-synopsis|import
    a <verbatim|.bbl> file>
  <|explain>
    Converts the file to Cork encoding, parses it as a <LaTeX> snippet,
    collects the macro definitions it contains (which are put in a
    surrounding <markup|with>), finds the <markup|thebibliography>
    environment and rewrites its items into a <markup|bib-list>:
    <verbatim|\\bibitem{<em|key>}> becomes <verbatim|(bibitem* <em|n>)>
    with a running number, <verbatim|\\bibitem[<em|l>]{<em|key>}> keeps its
    label, and a <markup|label> <verbatim|<em|bib>-<em|key>> is added.
  </explain>

  <subsection|The internal style engine>

  The internal styles are ordinary <scheme> modules
  <verbatim|(bibtex <em|name>)> in <verbatim|progs/bibtex/>, and are
  selected by the style name <verbatim|tm-<em|name>>. The entry point is:

  <\explain>
    <scm|(bib-process <scm-arg|prefix> <scm-arg|style>
    <scm-arg|doc>)><explain-synopsis|format a list of entries>
  <|explain>
    Defined in <source-link|bibtex/bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm>. <scm-arg|doc> is a
    <scm|(document (bib-entry <scm-arg|type> <scm-arg|key> (document
    (bib-field <scm-arg|name> <scm-arg|value>) ...)) ...)>, as produced by
    <cpp|parse_bib> or <scm|db-\<gtr\>bib>. The function sets the globals
    <scm|bib-current-prefix> and <scm|bib-style>, calls
    <scm|bib-preprocessing> on the entries (the <verbatim|alpha> style uses
    it to compute the labels), sorts them with <scm|bib-sorted-entries>
    (a stable sort on <scm|bib-sort-key>; <verbatim|unsrt> redefines it as
    the identity), formats each entry with <scm|(bib-format-entry
    <scm-arg|n> <scm-arg|entry>)>, and returns the simplified
    <scm|(bib-list <scm-arg|count> (document ...))>.
  </explain>

  A style is declared with <scm|(bib-define-style <scm-arg|name>
  <scm-arg|fallback>)>, which defines a <scheme> mode
  <scm|bib-<scm-arg|name>?> that implies the mode of the fallback style.
  <source-link|plain.scm|TeXmacs/progs/bibtex/plain.scm> defines the generic formatting functions
  (<scm|bib-format-entry>, <scm|bib-format-article>, ...,
  <scm|bib-format-names>, <scm|bib-sort-key>) without mode, and every other
  style overrides some of them with <scm|(:mode bib-<scm-arg|name>?)>. The
  mode <scm|bib-<scm-arg|name>?> holds when <scm|bib-style> is the name of
  the style. Since the dispatch uses modes, the most specific definition
  wins, exactly as for other context dependent <scheme> functions.

  The field values are <TeXmacs> trees, with authors and editors
  structured as <verbatim|(bib-names (bib-name <em|first> <em|von>
  <em|last> <em|jr>) ...)> and pages as <verbatim|(bib-pages <em|from>
  <em|to>)>. The <BibTeX> built-in string functions are implemented in
  <c++> (<source-link|Plugins/Bibtex/bibtex_functions.cpp|src/Plugins/Bibtex/bibtex_functions.cpp>) and exported to
  <scheme>:

  <\description-paragraphs>
    <item*|<scm|bib-field>, <scm|bib-empty?>>access to a field of a
    <markup|bib-entry>.

    <item*|<scm|bib-purify>, <scm|bib-text-length>,
    <scm|bib-prefix>>the analogues of <verbatim|purify$>,
    <verbatim|text.length$> and <verbatim|text.prefix$>.

    <item*|<scm|bib-locase>, <scm|bib-upcase>, <scm|bib-locase-first>,
    <scm|bib-upcase-first>, <scm|bib-default-preserve-case>,
    <scm|bib-default-upcase-first>>changes of case in the style of
    <verbatim|change.case$>, respecting <markup|keepcase> (the <TeXmacs>
    equivalent of braces in <BibTeX>).

    <item*|<scm|bib-add-period>>the analogue of <verbatim|add.period$>.

    <item*|<scm|bib-abbreviate>>abbreviation of first names.
  </description-paragraphs>

  The <scheme> side (<source-link|bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm>) adds helpers for building
  the output (<scm|bib-new-block>, <scm|bib-new-sentence>,
  <scm|bib-new-list>, <scm|bib-emphasize>, <scm|bib-translate>, ...) and for
  temporarily switching style (<scm|bib-with-style>).

  <subsection|Compilation with the database tool>

  When the database tool is enabled, <cpp|generate_bibliography> delegates
  to <source-link|database/bib-manage.scm|TeXmacs/progs/database/bib-manage.scm>:

  <\explain>
    <scm|(bib-compile <scm-arg|prefix> <scm-arg|style> <scm-arg|names> .
    <scm-arg|bib-files>)><explain-synopsis|compile a bibliography>
  <|explain>
    <scm-arg|names> is the list of cited keys (a <markup|document> tree is
    accepted). The entries are looked up in the following sources, in this
    order, the first source containing a key winning
    (<scm|bib-retrieve-entries>):

    <\enumerate>
      <item><scm|:local>: the entries edited locally for this document
      (attachments whose name ends with <verbatim|-biblio>);

      <item>the given files of the user: a <verbatim|.tmdb> file is used as
      a database, a <verbatim|.bib> file is first converted into a cached
      database (see below);

      <item><scm|:default>: the user's bibliographic database
      <scm|(bib-database)>; if several entries have the same name, those
      contributed by the default user are preferred;

      <item>the given <verbatim|.bib> files which were written from Zotero
      (they start with the line <verbatim|% Exported from Zotero by
      TeXmacs>);

      <item><scm|:zotero>: the library of Zotero, when Zotero runs (see
      <hlink|Citations from Zotero|zotero.en.tm>);

      <item><scm|:attached>: the entries that were attached to the document
      the last time its bibliography was compiled (attachments ending with
      <verbatim|-bibliography>).
    </enumerate>

    This order is computed by <scm|bib-sources>, for <scm|bib-compile> and
    <scm|bib-attach> alike.

    If the style is one of <scm|(bib-standard-styles)>, the entries are
    converted with <scm|db-\<gtr\>bib> and formatted by <scm|bib-generate>,
    which loads the style module and calls <scm|bib-process>. Otherwise the
    external program is used: the <verbatim|.bib> files are concatenated
    with the <BibTeX> conversion of the entries found elsewhere into
    <verbatim|$TEXMACS_HOME_PATH/system/bib/auto.bib>, on which
    <scm|bibtex-run> is called. If <verbatim|bibtex> is not installed, the
    style is replaced by <verbatim|tm-<em|style>> when available and by
    <verbatim|tm-plain> otherwise.
  </explain>

  <\explain>
    <scm|(bib-attach <scm-arg|prefix> <scm-arg|names> .
    <scm-arg|bib-files>)><explain-synopsis|attach the entries to the
    document>
  <|explain>
    Retrieves the cited entries in the same way and stores them, as
    <markup|db-entry> markup, in the attachment
    <verbatim|<em|prefix>-bibliography> of the document. A document therefore
    carries the entries it cites, and its bibliography can be recompiled
    elsewhere without the original <verbatim|.bib> file. When the preference
    <verbatim|"auto bib import"> is on (the default), the redefinition of
    <scm|notify-set-attachment> in <source-link|bib-manage.scm|TeXmacs/progs/database/bib-manage.scm> imports the
    entries of such attachments into the user's database. This hook is
    called by <cpp|edit_typeset_rep::set_data> for every attachment when the
    data of a document are installed, typically when it is opened; since
    its default definition in <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm> does nothing, the
    importation only happens once <source-link|bib-manage.scm|TeXmacs/progs/database/bib-manage.scm> has been
    loaded.
  </explain>

  <paragraph|Caching of <verbatim|.bib> files.>The conversion of a
  <verbatim|.bib> file into database entries is cached below
  <verbatim|$TEXMACS_HOME_PATH/system/database/>. The database
  <verbatim|bib-master.tmdb> maps the path of each source file
  (<verbatim|source>) to its modification time (<verbatim|stamp>) and to
  the converted document (<verbatim|target>). <scm|bib-cache-bibtex>
  creates the cache (<scm|bib-cache-create>: the source is copied to
  <verbatim|bib/<em|id>.bib> and converted to <verbatim|bib/<em|id>.tm>)
  or updates it when the stamp changed (<scm|bib-cache-update>, which uses
  the conservative importation described below, so that unchanged entries
  keep their identifiers). <scm|bib-cache-database> then imports the cited
  entries into <verbatim|bib/<em|id>.tmdb> and returns this database, in
  which the entries are looked up.

  <paragraph|Local bibliographies.><scm|open-biblio>
  (<source-link|database/bib-local.scm|TeXmacs/progs/database/bib-local.scm>) opens
  <verbatim|tmfs://biblio/<em|prefix>/<em|document>>, a virtual document
  showing the union of the attached and of the local entries. When it is
  saved (<scm|biblio-confirm>), the entries which differ from the attached
  ones are stored in the attachment <verbatim|<em|prefix>-biblio>, which
  takes precedence at the next compilation. This allows to correct an entry
  for one document without modifying the database. Only entries that
  already occur among the attached ones are taken into account.

  <section|<BibTeX> files as <TeXmacs> documents>

  <subsection|Parsing>

  <cpp|parse_bib> (<source-link|Data/Convert/BibTeX/parsebib.cpp|src/Data/Convert/BibTeX/parsebib.cpp>, exported as
  <scm|parse-bib>) parses a <verbatim|.bib> file into a <markup|document>
  containing <markup|bib-entry>, <markup|bib-comment>,
  <markup|bib-preamble> and <markup|bib-string> tags. It substitutes the
  <verbatim|@string> abbreviations (and a few predefined journal names, see
  <cpp|bib_strings_dict> in <source-link|bibtex_functions.cpp|src/Plugins/Bibtex/bibtex_functions.cpp>), and
  <cpp|bib_parse_fields> converts all field values at once from <LaTeX> to
  <TeXmacs> (by concatenating them with separators into a single <LaTeX>
  string, which is much faster than converting each field separately).
  Author and editor fields are split into names using the <BibTeX> rules
  (<cpp|bib_names>), <verbatim|pages> becomes <markup|bib-pages> and
  <abbr|URL>s become <markup|slink>.

  <subsection|The <verbatim|tmbib> format>

  <source-link|progs/convert/bibtex/init-bibtex.scm|TeXmacs/progs/convert/bibtex/init-bibtex.scm> defines two formats. The
  hidden format <verbatim|bibtex> (suffix <verbatim|rawbib>) converts
  between <verbatim|.bib> text and the parsed markup above, with the style
  <verbatim|bibliography>. The format <verbatim|tmbib> (name
  \P<BibTeX>\Q, suffix <verbatim|bib>) is the one used when opening or
  saving a <verbatim|.bib> file. Its converters are in
  <source-link|database/bib-db.scm|TeXmacs/progs/database/bib-db.scm>:

  <\itemize>
    <item><scm|tmbib-document-\<gtr\>texmacs> parses the file and converts
    each <markup|bib-entry> into a <markup|db-entry> with <scm|bib-\<gtr\>db>
    (style <verbatim|database-bib>). The original text and the converted
    body are stored as the attachments <verbatim|bibtex-source> and
    <verbatim|bibtex-target> of the document.

    <item><scm|texmacs-\<gtr\>tmbib-document> converts back. When the
    attachments are present, it calls <scm|conservative-bib-export>, so
    that entries which were not modified are written back exactly as they
    were in the original file (with their formatting and comments), and
    only modified or new entries are regenerated with
    <scm|serialize-bibtex>.
  </itemize>

  <scm|bib-\<gtr\>db> and <scm|db-\<gtr\>bib> implement the mapping between
  the two representations of entries. <BibTeX> brace conventions are
  replaced by explicit markup: last names are marked with <markup|name>
  (and <markup|name-von>, <markup|name-jr>, separated by
  <markup|name-sep>), and titles are stored in normal case with
  <markup|keepcase> only where needed; in the other direction the
  structured names become <markup|bib-names> and titles are protected with
  <markup|keepcase>. Entries of type <verbatim|conference> are imported as
  <verbatim|inproceedings>. Imported entries get the meta fields
  <verbatim|contributor> (default user), <verbatim|modus>
  (<verbatim|"imported">) and <verbatim|date>, and a fresh identifier from
  the user's bibliographic database.

  <subsection|Conservative conversions>

  <source-link|Data/Convert/BibTeX/conservative_bib.cpp|src/Data/Convert/BibTeX/conservative_bib.cpp> implements the
  incremental conversions. Both split a <verbatim|.bib> text into an
  alternation of inter-entry text and entries (<cpp|bib_break>) and index
  the entries by key.

  <\explain>
    <scm|(conservative-bib-import <scm-arg|old-s> <scm-arg|old-t>
    <scm-arg|new-s>)><explain-synopsis|incremental import>
  <|explain>
    Given an old text, its old conversion and a new text, only the entries
    whose text changed are converted again (by calling the <scheme>
    function <scm|zealous-bib-import>); the others are taken from
    <scm-arg|old-t>. Falls back to a full conversion if the preference
    <verbatim|"bibtex-\<gtr\>texmacs:conservative"> is off, or if the
    <verbatim|@string>/<verbatim|@preamble> parts differ.
  </explain>

  <\explain>
    <scm|(conservative-bib-export <scm-arg|old-t> <scm-arg|old-s>
    <scm-arg|new-t>)><explain-synopsis|incremental export>
  <|explain>
    The converse, controlled by the preference
    <verbatim|"texmacs-\<gtr\>bibtex:conservative">; modified entries are
    serialized by <scm|zealous-bib-export>.
  </explain>

  <subsection|Importing into and exporting from the database>

  <scm|bib-import-bibtex> (<menu|Data|Import>) imports a <verbatim|.bib>
  file into the user's database through the cache; <scm|bib-import-selection>
  and <scm|bib-import-current-buffer> import entries of a document with
  <scm|db-confirm-entries-in>. <scm|bib-export-bibtex> (<menu|Data|Export>)
  exports the selection, the current buffer, the whole database
  (<scm|bib-export-all>, sorted by name) or the attached entries of the
  current document (<scm|bib-export-attachments>); when the target file
  exists, the export is conservative with respect to it.

  <section|How to>

  <subsection|Add an internal bibliography style>

  <\enumerate>
    <item>Write a module <verbatim|progs/bibtex/<em|name>.scm> as explained
    in <hlink|Writing <TeXmacs> bibliography
    styles|../scheme/bibliography/bibliography.en.tm>, starting with

    <\scm-code>
      (texmacs-module (bibtex mystyle)

      \ \ (:use (bibtex bib-utils) (bibtex plain)))

      \;

      (bib-define-style "mystyle" "plain")

      \;

      (tm-define (bib-format-bibitem n x)

      \ \ (:mode bib-mystyle?)

      \ \ `(bibitem* ,(list-ref x 2)))
    </scm-code>

    Here the label of each item is its key.

    <item>Add <verbatim|"tm-mystyle"> to <scm|bib-standard-styles> in
    <source-link|bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm>. This list is used by the bibliography dialogue
    and, more importantly, by <scm|bib-compile>: with the database tool
    enabled, a style which is not in this list is passed to the external
    <verbatim|bibtex> program. Without the database tool, any style whose
    name starts with <verbatim|tm-> is loaded as <verbatim|(bibtex
    <em|name>)>.

    <item>Optionally, add a mapping from the corresponding <BibTeX> style
    in <cpp|generate_bibliography>, for the case when <verbatim|bibtex> is
    not installed.
  </enumerate>

  <subsection|Add fields or entry types>

  The admissible types and fields of bibliographic entries used by the
  editor are declared in <scm|db-format-table> in
  <source-link|database/bib-db.scm|TeXmacs/progs/database/bib-db.scm>; the internal styles format each type in
  <scm|bib-format-entry> (<source-link|plain.scm|TeXmacs/progs/bibtex/plain.scm>), falling back to
  <scm|bib-format-misc> for unknown types. Fields which need special
  treatment when converting between <BibTeX> and database entries are
  handled in <scm|db-bib-sub-sub> and <scm|bib-db-sub-sub>.

  <section|Pitfalls>

  <\itemize>
    <item>The two modes (with and without the database tool) follow
    different code paths, with different lookup rules for entries and
    different treatment of unknown internal styles. Test both.

    <item>With the database tool, entries are also found in the user
    database and in the attachments of the document, so that a
    bibliography may compile although the key is absent from the
    <verbatim|.bib> file; conversely, a stale local or attached entry takes
    precedence over the database (but not over the <verbatim|.bib> file for
    the attachments).

    <item>Opening a <verbatim|.bib> file creates identifiers in the user's
    bibliographic database (<scm|bib-\<gtr\>db> calls <scm|db-create-id>
    inside <scm|(with-database (bib-database) ...)>), even when the
    database tool is off.

    <item>The external program works in the shared directory
    <verbatim|$TEXMACS_HOME_PATH/system/bib>, so that two simultaneous
    compilations would clash, and <cpp|complete_bib_file> writes a
    <verbatim|-extended.bib> file next to the user's file when
    <verbatim|TeXmacs:> keys are cited.

    <item>Keys are compared as strings; <cpp|bib_entries> warns about
    missing keys on the <verbatim|bibtex-warning> debug channel only.

    <item>The key <verbatim|*> is only expanded (in <c++>) when a
    <verbatim|.bib> file was found and either <verbatim|bibtex> is missing
    or the style is internal; otherwise it is passed on to
    <verbatim|bibtex> as <verbatim|\\citation{*}>. Without a
    <verbatim|.bib> file it is ignored.
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
