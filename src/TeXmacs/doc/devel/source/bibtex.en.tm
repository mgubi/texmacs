<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <BibTeX> engine and bibliography styles>

  <section|Introduction>

  <TeXmacs> can produce bibliographies in two ways: by running the external
  <verbatim|bibtex> program on a <verbatim|.bst> style and importing the
  resulting <verbatim|.bbl> file, or by doing everything itself, with a
  <c++> parser for <verbatim|.bib> files, a <c++> implementation of the
  string functions of the <BibTeX> language, and bibliography styles written
  in <scheme>. The second way is used for the styles whose name starts with
  <verbatim|tm-> and whenever <verbatim|bibtex> is not installed.

  The chapter <hlink|the database and bibliographies|database-bibliography.en.tm>
  describes the markup of citations and bibliographies, how the generation
  is triggered, how the strategy is chosen (with or without the database
  tool), how <verbatim|bibtex> is run and how <verbatim|.bib> files are
  edited as <TeXmacs> documents. The guide <hlink|writing <TeXmacs>
  bibliography styles|../scheme/bibliography/bibliography.en.tm> documents
  the <scheme> functions available to style writers. The present chapter
  goes one level deeper and describes the engine itself: the exact grammar
  accepted by the parser and the trees it produces, the conversion of field
  values and person names, the semantics of the built-in functions and how
  they differ from those of <BibTeX>, the style dispatch, the shipped
  styles, and the known problems.

  Paths of <c++> files are relative to <source-link|src/src/|src>, paths of
  <scheme> files to <source-link|src/TeXmacs/progs/|TeXmacs/progs>.

  <section|Overview>

  Without the database tool, <cpp|edit_process_rep::generate_bibliography>
  (<source-link|Edit/Process/edit_process.cpp|src/Edit/Process/edit_process.cpp>) follows one of three paths:

  <\verbatim-code>
    style tm-xxx, or no bibtex program \ \ \ \ \ \ \ \ \ \ \ \ \ \ other style

    \ \ \ \ \ \ \ \ \ \ \|\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|

    \ \ .bib + texmacs.bib\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ bibtex_run

    \ \ \ \ \ \ \ \ \ \ \|\ parse_bib\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|\ temp.aux, bibtex

    \ \ (document (bib-entry ...) ...)\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ temp.bbl

    \ \ \ \ \ \ \ \ \ \ \|\ bib_entries (cited keys)\ \ \ \ \ \ \ \ \ \ \ \ \|\ bibtex_load_bbl

    \ \ \ \ \ \ \ \ \ \ \|\ bib-process (Scheme style)\ \ \ \ \ \ \ \ \ \ \|

    \ \ \ \ \ \ \ \ \ \ +----------\<gtr\> (bib-list n (document (concat (bibitem* ..) ..) ..))

    \;

    no .bib file, but a .bbl file: \ bibtex_load_bbl only
  </verbatim-code>

  With the database tool, the <scheme> function <scm|bib-compile> replaces
  both branches; for the internal styles it ends in the same
  <scm|bib-process>.
  In all cases the result is a <markup|bib-list> which replaces the body of
  the <markup|bibliography> tag.

  The engine has three layers:

  <\description>
    <item*|Parsing (<c++>)><cpp|parse_bib> turns the text of a
    <verbatim|.bib> file into a <TeXmacs> tree of <markup|bib-entry> tags,
    expands <verbatim|@string> abbreviations, converts all field values from
    <LaTeX> to <TeXmacs>, splits person names into first, von, last and jr
    parts and normalizes page ranges. <scm|serialize-bibtex> goes the other
    way. See <hlink|parsing and serializing .bib files|bibtex-parsing.en.tm>.

    <item*|Built-in functions (<c++>)><TeXmacs> analogues of the <BibTeX>
    built-in functions <verbatim|purify$>, <verbatim|text.length$>,
    <verbatim|text.prefix$>, <verbatim|change.case$>, <verbatim|add.period$>
    and of the abbreviation of first names, working on trees instead of
    strings. See <hlink|the built-in functions|bibtex-functions.en.tm>.

    <item*|Styles (<scheme>)>The style engine of
    <source-link|bibtex/bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm> and the shipped styles, which override
    each other through <scheme> modes. See <hlink|the style
    engine and the shipped styles|bibtex-styles.en.tm>.
  </description>

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Plugins/Bibtex/bibtex.cpp|src/Plugins/Bibtex/bibtex.cpp>, <source-link|bibtex.hpp|src/Plugins/Bibtex/bibtex.hpp>>The
    interface to the external program: <cpp|set_bibtex_command>,
    <cpp|bibtex_present>, <cpp|bibtex_run>, <cpp|bibtex_load_bbl>.

    <item*|<source-link|Plugins/Bibtex/bibtex_functions.cpp|src/Plugins/Bibtex/bibtex_functions.cpp>,
    <source-link|bibtex_functions.hpp|src/Plugins/Bibtex/bibtex_functions.hpp>>The built-in functions, the splitting
    of names, the conversion of field values (<cpp|bib_parse_fields>), the
    <verbatim|@string> dictionary and the selection of cited entries
    (<cpp|bib_entries>).

    <item*|<source-link|Data/Convert/BibTeX/parsebib.cpp|src/Data/Convert/BibTeX/parsebib.cpp>>The parser
    <cpp|parse_bib>.

    <item*|<source-link|Data/Convert/BibTeX/conservative_bib.cpp|src/Data/Convert/BibTeX/conservative_bib.cpp>>Conservative
    import and export, see <hlink|the database
    chapter|database-bibliography.en.tm>.

    <item*|<source-link|Edit/Process/edit_process.cpp|src/Edit/Process/edit_process.cpp>><cpp|generate_bibliography>
    and its helpers <cpp|find_bib_file>, <cpp|copy_bst_file>,
    <cpp|arrange_bib>.

    <item*|<source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>>The glue:
    <scm|set-bibtex-command>, <scm|supports-bibtex?>, <scm|bibtex-run>,
    <scm|parse-bib>, <scm|conservative-bib-import>,
    <scm|conservative-bib-export> and the <scm|bib-...> built-ins.

    <item*|<source-link|bibtex/bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm>>The style engine
    (<scm|bib-process>, <scm|bib-define-style>, the output helpers).

    <item*|<source-link|bibtex/plain.scm|TeXmacs/progs/bibtex/plain.scm> and the other files of
    <verbatim|bibtex/>>The shipped styles <verbatim|plain>,
    <verbatim|abbrv>, <verbatim|abstract>, <verbatim|acm>,
    <verbatim|alpha>, <verbatim|elsart-num>, <verbatim|ieeetr>,
    <verbatim|siam> and <verbatim|unsrt>.

    <item*|<source-link|bibtex/bib-complete.scm|TeXmacs/progs/bibtex/bib-complete.scm>,
    <source-link|bibtex/bib-widgets.scm|TeXmacs/progs/bibtex/bib-widgets.scm>>Completion of citation keys and the
    dialog which inserts or modifies a bibliography.

    <item*|<source-link|convert/bibtex/|TeXmacs/progs/convert/bibtex>>The registration of the
    <verbatim|bibtex> and <verbatim|tmbib> formats and converters
    (<source-link|init-bibtex.scm|TeXmacs/progs/convert/bibtex/init-bibtex.scm>), and the serializer
    <source-link|bibtexout.scm|TeXmacs/progs/convert/bibtex/bibtexout.scm>.

    <item*|<source-link|src/TeXmacs/misc/bib/texmacs.bib|TeXmacs/misc/bib/texmacs.bib>>Entries with keys
    <verbatim|TeXmacs:...>, appended to every bibliography so that
    documents may cite the <TeXmacs> papers without a <verbatim|.bib> file.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Parsing and serializing .bib files|bibtex-parsing.en.tm>

    <branch|The built-in functions|bibtex-functions.en.tm>

    <branch|The style engine and the shipped styles|bibtex-styles.en.tm>

    <branch|Pitfalls and known bugs|bibtex-pitfalls.en.tm>
  </traverse>

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
