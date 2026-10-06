<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Pitfalls and known bugs>

  The general pitfalls of the bibliography pipeline (the two modes with and
  without the database tool, stale entries in the database) are listed in
  <hlink|the database chapter|database-bibliography.en.tm>. This page lists
  the problems of the engine itself. Items marked <em|(checked)> were
  reproduced by calling the functions from <scheme>; the others come from
  reading the code.

  <section|Parsing>

  <\itemize>
    <item><em|(checked)> Inside a value delimited by double quotes, braces
    are not counted (<cpp|bib_within>,
    <verbatim|Data/Convert/BibTeX/parsebib.cpp:83-102>, the test
    <cpp|cbegin != cend>). A value such as <verbatim|"A {"} b"> ends at the
    quote inside the braces; the rest of the entry, including the following
    fields, ends up in a <markup|bib-comment>. <BibTeX> allows this, so
    files written for <BibTeX> may lose fields.

    <item><em|(checked)> Inside a <verbatim|@string> definition, a
    reference to an earlier abbreviation is only found if it is written in
    lower case: <cpp|bib_strings_dict> stores the names in lower case but
    looks them up with their original case
    (<source-link|Plugins/Bibtex/bibtex_functions.cpp:1045|src/Plugins/Bibtex/bibtex_functions.cpp:1045>). With
    <verbatim|@string{Foo = "X"} @string{bar = FOO # "Y"}>, <verbatim|bar>
    is <verbatim|"Y">.

    <item><em|(checked)> An unknown abbreviation is kept as its name when
    it is the whole value, but replaced by nothing inside a <verbatim|#>
    concatenation (<cpp|bib_subst_str>, <source-link|bibtex_functions.cpp:1063|src/Plugins/Bibtex/bibtex_functions.cpp:1063>),
    without any warning.

    <item>The month abbreviations <verbatim|jan>, ..., <verbatim|dec>,
    defined by the standard <verbatim|.bst> files, are not predefined; with
    the internal styles <verbatim|month = jan> prints <verbatim|jan>.

    <item><em|(checked)> <verbatim|@preamble> items are parsed but dropped
    from the result of <cpp|parse_bib> (<source-link|parsebib.cpp:411|src/Data/Convert/BibTeX/parsebib.cpp:411>), so
    macros defined there are unknown when the field values are converted
    and are not available to the internal styles.

    <item>The conversion of field values is all or nothing: if the number
    of pieces produced by the <LaTeX> parser does not match the number of
    fields, <cpp|bib_parse_fields> (<source-link|bibtex_functions.cpp:957|src/Plugins/Bibtex/bibtex_functions.cpp:957>)
    converts <em|no> field of the file, and every value of every entry
    stays a raw <LaTeX> string. One unusual value can therefore spoil the
    whole bibliography.

    <item><em|(checked)> Page ranges which do not start with a digit
    (article numbers such as <verbatim|e1002--e1010>, roman numerals) become
    <verbatim|(bib-pages "0")> (<cpp|bib_field_pages>,
    <source-link|bibtex_functions.cpp:824|src/Plugins/Bibtex/bibtex_functions.cpp:824>). The original value is lost, also
    when a modified entry is saved back to the <verbatim|.bib> file.
  </itemize>

  <section|Built-in functions>

  <\itemize>
    <item><em|(checked)> <cpp|bib_tree_length>
    (<source-link|bibtex_functions.cpp:692|src/Plugins/Bibtex/bibtex_functions.cpp:692>) adds up the lengths of the
    children of a <markup|concat> in a local variable but then returns
    <verbatim|0>, so <scm|bib-text-length> is <verbatim|0> for every
    compound tree. The styles then use a non-breaking space where a space
    was intended after long compound values.

    <item><em|(checked)> <cpp|bib_get_prefix>
    (<source-link|bibtex_functions.cpp:713|src/Plugins/Bibtex/bibtex_functions.cpp:713>) computes the index of the body of
    a <markup|with> but never descends into it, so <scm|bib-prefix> of a
    formatted value is empty. It also counts bytes, so that the prefix of
    <verbatim|\<less\>Ccaron\<gtr\>apek> of length three is the invalid
    string <verbatim|\<less\>Cc>; <verbatim|alpha> labels of authors whose
    last name starts with such a character are broken.

    <item><em|(checked)> <scm|bib-upcase-first> and <scm|bib-locase-first>
    change the first character even inside <markup|keepcase>
    (<verbatim|iPhone> in braces becomes <verbatim|IPhone>), unlike
    <scm|bib-upcase> and <scm|bib-locase>.

    <item><scm|bib-add-period> replaces a final comma or semicolon by a
    period, which <BibTeX> does not do. <scm|bib-purify> keeps punctuation,
    which <verbatim|purify$> removes; sort keys therefore differ slightly
    from those of <BibTeX>.

    <item><scm|bib-default-upcase-first> changes no case at all; it only
    removes <markup|keepcase>.
  </itemize>

  <section|External <BibTeX>>

  <\itemize>
    <item>The command line built by <cpp|bibtex_run>
    (<source-link|Plugins/Bibtex/bibtex.cpp:192|src/Plugins/Bibtex/bibtex.cpp:192>) starts with <verbatim|cd
    $TEXMACS_HOME_PATH/system/bib;> without quotes, so it fails when the
    home path contains spaces; the directory of the <verbatim|.bib> file is
    quoted with double quotes, which does not protect it against
    <verbatim|"> or <verbatim|$> in the name.

    <item>Any non-zero exit status of the shell command is reported as an
    error, with the whole log, and the warnings are then not extracted
    (<source-link|bibtex.cpp:202|src/Plugins/Bibtex/bibtex.cpp:202>). In both cases <verbatim|temp.bbl> is
    loaded afterwards: if <verbatim|bibtex> stopped before writing it (for
    instance because the style does not exist), the bibliography of the
    previous run is silently used.

    <item>All runs share <verbatim|temp.aux>, <verbatim|temp.log> and
    <verbatim|temp.bbl> (<source-link|bibtex.cpp:186|src/Plugins/Bibtex/bibtex.cpp:186>), so two instances of
    <TeXmacs> generating bibliographies at the same time interfere.

    <item><cpp|copy_bst_file> (<source-link|Edit/Process/edit_process.cpp:71|src/Edit/Process/edit_process.cpp:71>)
    only copies a <verbatim|.bst> file from the directory of the document
    if no copy exists yet, so a later version of the file is not copied.
    Whether <verbatim|bibtex> then uses the old copy or the new file depends
    on its search path (<verbatim|BSTINPUTS> lists the directory of the
    <verbatim|.bib> file first, which is not always the directory of the
    document).

    <item>When <verbatim|TeXmacs:> keys are cited, <cpp|complete_bib_file>
    writes <verbatim|<em|name>-extended.bib> in the directory of the
    user's <verbatim|.bib> file (<source-link|bibtex.cpp:157|src/Plugins/Bibtex/bibtex.cpp:157>), which may be
    read only or under version control.

    <item><cpp|bibtex_load_bbl> accesses the second argument of every
    <markup|bibitem*> and <markup|bibitem-with-key> item
    (<source-link|bibtex.cpp:121|src/Plugins/Bibtex/bibtex.cpp:121>) after only checking that it has at least
    one, so an unusual <verbatim|.bbl> file can read past the end of the
    tree.
  </itemize>

  <section|Serialization>

  <\itemize>
    <item><em|(checked)> <scm|serialize-bibtex> writes atomic values
    verbatim between braces (<source-link|convert/bibtex/bibtexout.scm:133|TeXmacs/progs/convert/bibtex/bibtexout.scm:133>).
    <verbatim|50\\% caf\\'e> read from a <verbatim|.bib> file is written
    back as <verbatim|50% caf> followed by the raw Cork byte of
    <verbatim|e> acute: the percent sign then starts a <LaTeX> comment, and
    the accent becomes an 8-bit character. Thanks to the conservative export
    this only happens for new and modified entries.
  </itemize>

  <section|Styles and user interface>

  <\itemize>
    <item><scm|bib-define-style> sets the global <scm|bib-default-style> to
    the fallback of the style being loaded (<source-link|bibtex/bib-utils.scm:83|TeXmacs/progs/bibtex/bib-utils.scm:83>,
    <verbatim|87>), and <scm|bib-mode?> also tests this global. All shipped
    styles use <verbatim|plain> as fallback, so this is harmless today, but
    a style with another fallback would make the overrides of that fallback
    active for every style once it has been loaded.

    <item>The preview of the bibliography dialog loads the module
    <verbatim|(bibtex <em|name>)> for whatever style is selected
    (<source-link|bibtex/bib-widgets.scm:49|TeXmacs/progs/bibtex/bib-widgets.scm:49>); for an external style such as
    <verbatim|amsplain> there is no such module. The preview also formats
    every entry of the <verbatim|.bib> file, which is slow for large files.

    <item>Completion of citation keys resolves the file name of the
    <markup|bibliography> tag directly relative to the buffer
    (<source-link|bibtex/bib-complete.scm:47|TeXmacs/progs/bibtex/bib-complete.scm:47>), without the <verbatim|.bib>
    suffix and ancestor directory search of <cpp|find_bib_file>, so it may
    find nothing for bibliographies which compile fine. The result is
    cached per buffer, and the code itself notes that a change of the file
    name is not noticed.
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
