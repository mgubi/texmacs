<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Parsing and serializing .bib files>

  <section|The parser>

  <cpp|tree parse_bib (string s)> (<verbatim|Data/Convert/BibTeX/parsebib.cpp>,
  glue <scm|parse-bib>) parses the text of a <verbatim|.bib> file. It is a
  hand written recursive descent parser which never fails: errors are
  reported on the <verbatim|convert-error> debug channel (with the key of
  the entry being parsed, <cpp|bib_current_tag>) and the parser resynchronizes
  as well as it can. It returns an empty tree if the input or the result is
  empty.

  <subsection|Accepted syntax>

  <\itemize>
    <item>An item starts with <verbatim|@> followed by a type, which is
    lower-cased. The body may be delimited by braces or by parentheses
    (<cpp|bib_open>); if neither follows, the parser skips to the next
    <verbatim|{> or <verbatim|(>.

    <item><verbatim|@string{<em|name> = <em|value>, ...}> defines
    abbreviations (several per command are allowed),
    <verbatim|@preamble{...}> contains <LaTeX> code, <verbatim|@comment{...}>
    is kept as a comment, and every other type is an entry
    <verbatim|@<em|type>{<em|key>, <em|field> = <em|value>, ...}>.
    Field names are lower-cased; keys are kept as they are.

    <item>A value is a concatenation, with <verbatim|#>, of brace delimited
    strings, double quote delimited strings, numbers and abbreviation names
    (<cpp|bib_arg>, <cpp|bib_atomic_arg>). Inside braces, nested braces are
    counted and a backslash protects the next character
    (<cpp|bib_within>). Inside double quotes, braces are <em|not> counted, so
    the value ends at the first <verbatim|"> which is not preceded by a
    backslash.

    <item>A bare word starting with a digit is a number and stays a string;
    any other bare word is an abbreviation, represented as
    <verbatim|(bib-var <em|name>)>.

    <item>Lines starting with <verbatim|%> between items become
    <verbatim|(bib-comment (document (bib-line ...) ...))>, and so do
    <verbatim|@comment> items and any text with letters or digits between
    the end of an entry and the next <verbatim|@> (the \Pdirty hack\Q in
    <cpp|bib_list>).

    <item>The text of every value is converted from the
    \Pwestern\Q 8-bit encoding to Cork (<cpp|western_to_cork>), and
    newlines inside values are replaced by single spaces
    (<cpp|normalize_newlines>).
  </itemize>

  <subsection|Abbreviations>

  All <verbatim|@string> definitions of the file are collected first and
  turned into a dictionary by <cpp|bib_strings_dict>
  (<verbatim|Plugins/Bibtex/bibtex_functions.cpp>). The dictionary is
  pre-filled with the twenty journal abbreviations of the standard
  <BibTeX> styles (<verbatim|acmcs>, <verbatim|cacm>, <verbatim|jacm>,
  <verbatim|tcs>, ...); the month abbreviations <verbatim|jan>, ...,
  <verbatim|dec>, which <BibTeX> styles normally define, are <em|not>
  included, so <verbatim|month = jan> yields the string <verbatim|jan>.
  Names are compared case-insensitively when a field is expanded
  (<cpp|bib_subst_vars>). An unknown abbreviation which is the whole value
  is replaced by its own name, but inside a <verbatim|#> concatenation it
  is replaced by nothing. The dictionary is built in the order of the
  definitions, so a definition may refer to earlier ones, but only with
  the same case (see <hlink|the pitfalls|bibtex-pitfalls.en.tm>). After the substitution, the
  <verbatim|@string> and <verbatim|@preamble> items are dropped from the
  result: <cpp|parse_bib> only returns entries and comments.

  <subsection|The resulting tree>

  <\scm-code>
    (document

    \ \ (bib-entry "article" "knuth84"

    \ \ \ \ (document

    \ \ \ \ \ \ (bib-field "author" (bib-names (bib-name "Donald E." "" "Knuth" "")))

    \ \ \ \ \ \ (bib-field "title" "Literate programming")

    \ \ \ \ \ \ (bib-field "pages" (bib-pages "97" "111"))

    \ \ \ \ \ \ (bib-field "url" (slink "https://..."))))

    \ \ (bib-comment (document (bib-line " a comment")))

    \ \ ...)
  </scm-code>

  The type and the key are strings; every field value is a <TeXmacs> tree.
  The same format is consumed by <cpp|bib_entries> and the <scheme> styles,
  and produced from database entries by <scm|db-\<gtr\>bib> (see
  <hlink|the database chapter|database-bibliography.en.tm>).

  <section|Conversion of field values>

  <subsection|<LaTeX> to <TeXmacs>>

  Field values are written in <LaTeX>. Converting each of them separately
  with the <LaTeX> parser would be slow, so <cpp|bib_parse_fields>
  converts all fields of the file at once:

  <\enumerate>
    <item><cpp|bib_get_fields> walks through all entries (also inside
    comments) and concatenates every field value into one <LaTeX> string,
    each preceded by the separator <verbatim|\\nextbib{}>. Each value is
    first rewritten by <cpp|bib_to_latex>, which turns <BibTeX> braces into
    <verbatim|\\keepcase{...}> (see below), escapes <verbatim|%> and protects
    <verbatim|[> and <verbatim|]>. Author and editor fields are split into
    names at this point (next subsection); <verbatim|pages> is skipped.

    <item>The string is parsed once with <cpp|parse_latex> and converted
    with <cpp|latex_to_tree>; <cpp|bib_latex_array> cuts the result back
    into pieces at the <markup|nextbib> separators.

    <item>If the number of pieces is one less than expected and the string
    ends with an empty field, the conversion is redone with a sentinel
    <verbatim|{xyzyx}> appended (the parser drops a trailing empty
    argument).

    <item>Atomic values which look like <abbr|URL>s (no space,
    <verbatim|http://>, <verbatim|https://> or <verbatim|ftp://>) become
    <markup|slink>.

    <item>Only if the number of pieces equals the number of fields are the
    values put back (<cpp|bib_set_fields>). Otherwise <em|no> field of the
    file is converted, and all values stay raw <LaTeX> strings.
  </enumerate>

  <paragraph|Braces and case.>In <BibTeX>, a brace group at depth one
  protects its contents against case changes, unless it starts with a
  backslash (a \Pspecial character\Q such as <verbatim|{\\"o}>).
  <cpp|bib_to_latex> renders such groups as <markup|keepcase>; braces of
  special characters and math (<verbatim|$...$>, which is passed through
  unchanged) are kept for the <LaTeX> parser. The built-in case functions
  then respect <markup|keepcase> (see <hlink|the built-in
  functions|bibtex-functions.en.tm>).

  <subsection|Person names>

  <cpp|bib_names> splits an author or editor field at the word
  <verbatim|and> at brace depth zero (<cpp|search_and_keyword>) and parses
  each name with <cpp|get_first_von_last>, following the three <BibTeX>
  forms:

  <\description>
    <item*|<verbatim|First von Last>>(no comma, <cpp|get_fvl>). If all
    words start with an upper case letter, the last word is the last name
    and the others are the first names. Otherwise the last name is the
    longest run of capitalized words at the end, the first names are the
    capitalized words at the beginning, and the von part is what lies in
    between. <verbatim|Jean de La Fontaine> gives first <verbatim|Jean>, von
    <verbatim|de>, last <verbatim|La Fontaine>.

    <item*|<verbatim|von Last, First>>(one comma, <cpp|get_vl_f>). The part
    before the comma is split into von and last in the same way.

    <item*|<verbatim|von Last, Jr, First>>(two commas,
    <cpp|get_vl_j_f>).
  </description>

  A word \Pstarts with a lower case letter\Q if its first ordinary
  character is lower case at brace depth zero, or inside a special
  character (<cpp|first_is_locase>). Each name becomes <verbatim|(bib-name
  <em|first> <em|von> <em|last> <em|jr>)>, with empty strings for missing
  parts, and the list is wrapped in <markup|bib-names>.

  <subsection|Page ranges>

  <cpp|bib_field_pages> reads the first integer of the value and, after
  skipping non-digits, a second one, and returns <verbatim|(bib-pages
  <em|from> <em|to>)> or <verbatim|(bib-pages <em|from>)>. Only decimal
  digits are understood: a value which does not start with a digit
  becomes <verbatim|(bib-pages "0")>, see <hlink|the
  pitfalls|bibtex-pitfalls.en.tm>.

  <section|Selection of the cited entries>

  For the internal styles without the database tool,
  <cpp|generate_bibliography> appends <verbatim|texmacs.bib> to the
  <verbatim|.bib> file, parses the whole text and calls <cpp|tree
  bib_entries (tree t, tree bib_t)>. This function
  (<cpp|bib_select_entries>) indexes the entries by key, warns about
  duplicate keys (the first one wins) and about cited keys which are not
  found (on the <verbatim|bibtex-warning> channel), and returns the cited
  entries in the order of the citations. An entry with a
  <verbatim|crossref> field adds the referenced key at the end of the list
  of keys to process, so that it is included once, after the cited ones.
  The fields of the referenced entry are <em|not> merged into the citing
  one; as in the standard <BibTeX> styles, the shipped styles instead
  write \Pin\Q followed by a citation of the referenced entry when an
  entry has a <verbatim|crossref> field. A citation <verbatim|*> (from
  <markup|nocite*>) is expanded beforehand into all keys of the
  <verbatim|.bib> file.

  <section|Serialization>

  The format <verbatim|bibtex> (hidden, suffix <verbatim|rawbib>) and its
  converters are registered in <verbatim|convert/bibtex/init-bibtex.scm>:
  <verbatim|bibtex-document> and <verbatim|bibtex-snippet> are parsed with
  <scm|parse-bib>, and the inverse is <scm|serialize-bibtex>
  (<verbatim|convert/bibtex/bibtexout.scm>). The user visible format
  <verbatim|tmbib> (suffix <verbatim|bib>) goes through the database
  representation (<verbatim|database/bib-db.scm>) but uses the same parser
  and serializer.

  <scm|serialize-bibtex> writes one item per entry, with field names padded
  to twelve characters. A value is written as follows:

  <\itemize>
    <item><markup|bib-var> as the bare name; a concatenation containing
    <markup|bib-var> pieces as <verbatim|{...} # name # {...}>;

    <item><markup|bib-names> as <verbatim|von Last, First, Jr> joined by
    <verbatim|and>;

    <item><markup|bib-pages> as <verbatim|{<em|from>--<em|to>}>;

    <item>an atomic string verbatim between braces;

    <item>any other tree through the <LaTeX> exporter (with <markup|keepcase>
    turned back into a brace group), in <abbr|ASCII> encoding.
  </itemize>

  Note that atomic strings are not converted: special characters such as
  <verbatim|%> and non-<abbr|ASCII> Cork characters are written as they
  are. When a <verbatim|.bib> file is edited as a document, the
  conservative export (see <hlink|the database
  chapter|database-bibliography.en.tm>) keeps the original text of the
  unchanged entries, so this only affects new and modified entries.

  <section|The external program>

  The way <cpp|bibtex_run> drives the external program is described in
  <hlink|the database chapter|database-bibliography.en.tm>. A few
  additional details:

  <\itemize>
    <item>The program name comes from the preference <verbatim|"bibtex
    command"> (<verbatim|texmacs/texmacs/tm-server.scm>), passed to
    <cpp|set_bibtex_command>; <cpp|bibtex_present> tests whether it is in
    the search path.

    <item>All runs share the fixed files <verbatim|temp.aux>,
    <verbatim|temp.log> and <verbatim|temp.bbl> in
    <verbatim|$TEXMACS_HOME_PATH/system/bib>, a directory created at
    startup.

    <item>Before running, <cpp|copy_bst_file> copies a <verbatim|.bst> file
    found next to the document into that directory, but only if no file of
    that name is there yet.

    <item>The <verbatim|.bbl> file is converted from the 8-bit encoding
    with <cpp|bibtex_update_encoding> before being parsed as a <LaTeX>
    snippet, and <cpp|arrange_bib> finally replaces <verbatim|--> by an en
    dash in all atomic strings of the result (for both engines).
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
