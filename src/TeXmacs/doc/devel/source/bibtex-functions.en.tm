<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The built-in functions>

  The <BibTeX> style language has a few built-in functions which operate on
  <LaTeX> strings. The <TeXmacs> styles need the same operations on
  <TeXmacs> trees, since the field values have already been converted (see
  <hlink|parsing|bibtex-parsing.en.tm>). They are implemented in <c++> in
  <source-link|Plugins/Bibtex/bibtex_functions.cpp|src/Plugins/Bibtex/bibtex_functions.cpp> and exported to <scheme>
  by <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>. All of them take
  <scheme> trees, convert them to <TeXmacs> trees and simplify them with
  <cpp|simplify_correct> before working, and most return a <scheme> tree.
  The descriptions below are meant for style writers who need to know the
  exact behaviour; the list of functions is also in <hlink|writing
  <TeXmacs> bibliography styles|../scheme/bibliography/bibliography.en.tm>.

  <section|Access to fields>

  <\explain>
    <scm|(bib-field <scm-arg|entry> <scm-arg|name>)><explain-synopsis|value
    of a field>
  <|explain>
    Returns the value of the field <scm-arg|name> of a <markup|bib-entry>,
    or <verbatim|""> if there is no such field or if <scm-arg|entry> is not
    a well formed entry (three children, the last one a
    <markup|document>). Field names are compared literally; since the
    parser lower-cases them, they should be given in lower case.
  </explain>

  <\explain>
    <scm|(bib-empty? <scm-arg|entry> <scm-arg|name>)><explain-synopsis|test
    for a missing field>
  <|explain>
    True if <scm|bib-field> returns <verbatim|"">. A field whose value is a
    non-atomic tree without text (for instance an empty
    <markup|concat>) is not considered empty; use <scm|bib-null?> from
    <source-link|bibtex/bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm> for such tests.
  </explain>

  <section|Text functions>

  <\explain>
    <scm|(bib-purify <scm-arg|t>)><explain-synopsis|analogue of
    <verbatim|purify$>>
  <|explain>
    Returns the concatenated text of <scm-arg|t>: atomic strings are kept,
    <markup|with> contributes its body, <markup|keepcase>,
    <markup|concat> and <markup|document> their children, and every other
    tag is dropped together with its contents. Unlike <verbatim|purify$>,
    punctuation is not removed and no character is replaced by a space.
    Used to build sort keys.
  </explain>

  <\explain>
    <scm|(bib-text-length <scm-arg|t>)><explain-synopsis|analogue of
    <verbatim|text.length$>>
  <|explain>
    Meant to return the length of the text of <scm-arg|t>. For an atomic
    tree it returns the number of bytes, so that a Cork entity such as
    <verbatim|\<less\>alpha\<gtr\>> counts as seven characters. For any
    compound tree it currently returns <verbatim|0> (see <hlink|the
    pitfalls|bibtex-pitfalls.en.tm>). The shipped styles use it to choose
    between a space and a non-breaking space after short volume and number
    fields.
  </explain>

  <\explain>
    <scm|(bib-prefix <scm-arg|t> <scm-arg|n>)><explain-synopsis|analogue of
    <verbatim|text.prefix$>>
  <|explain>
    Returns, as a string, the first <scm-arg|n> bytes of the text of
    <scm-arg|t>, collected from atomic strings and the children of
    <markup|concat> and <markup|document>. The special value
    <verbatim|"others"> (the <BibTeX> convention for \Pand others\Q) gives
    <verbatim|"+">. Bytes, not characters, are counted, so the result may
    end in the middle of a Cork entity, and the body of a <markup|with> is
    ignored. Used by the <verbatim|alpha> style to build labels.
  </explain>

  <\explain>
    <scm|(bib-abbreviate <scm-arg|t> <scm-arg|dot>
    <scm-arg|sep>)><explain-synopsis|abbreviate first names>
  <|explain>
    Replaces every word of the text of <scm-arg|t> by its first character
    followed by <scm-arg|dot>, and separates the abbreviated words by
    <scm-arg|sep>. A Cork entity counts as one character. In hyphenated
    words, every part is abbreviated and the hyphens are kept, so
    <verbatim|Jean-Pierre> becomes <verbatim|J.-P.> with <scm-arg|dot> equal
    to <verbatim|".">. The result is a <markup|concat>. The styles call it
    as <scm|(bib-abbreviate first "." '(nbsp))>.
  </explain>

  <\explain>
    <scm|(bib-add-period <scm-arg|t>)><explain-synopsis|analogue of
    <verbatim|add.period$>>
  <|explain>
    Looks at the last non-space character of the last non-empty atomic
    string of <scm-arg|t>. If it is a comma or a semicolon, it is
    <em|replaced> by a period; if it is <verbatim|.>, <verbatim|!> or
    <verbatim|?>, nothing happens; otherwise a period is appended. Replacing
    commas and semicolons is a difference with <BibTeX>, which only
    appends.
  </explain>

  <section|Case changes>

  Case changes respect the <markup|keepcase> tags produced by the parser
  from <BibTeX> brace groups, and never touch <markup|verbatim>,
  <markup|slink>, <markup|math>, typewriter text (<verbatim|font-family
  tt>) and text with a <verbatim|math-font>. They use the Unicode aware
  functions <cpp|uni_locase_all>, <cpp|uni_upcase_all>,
  <cpp|uni_locase_first> and <cpp|uni_upcase_first>, so accented Cork
  characters are handled.

  <\description-paragraphs>
    <item*|<scm|(bib-locase <scm-arg|t>)>,
    <scm|(bib-upcase <scm-arg|t>)>>Change the case of all text, except
    inside <markup|keepcase>. The <markup|keepcase> tags themselves are
    removed (their contents are kept as they are), so the result can be
    displayed directly. This is the analogue of <verbatim|change.case$>
    with <verbatim|"l"> and <verbatim|"u">.

    <item*|<scm|(bib-locase-first <scm-arg|t>)>,
    <scm|(bib-upcase-first <scm-arg|t>)>>Change the case of the first
    character of the first non-blank atomic string (for <markup|with>, of
    its body). Since <markup|keepcase> is not skipped specially here, the
    first character inside a <markup|keepcase> is changed as well.

    <item*|<scm|(bib-default-preserve-case <scm-arg|t>)>>Only simplifies
    the tree; <markup|keepcase> tags are kept.

    <item*|<scm|(bib-default-upcase-first <scm-arg|t>)>>Despite its name,
    changes no case: it removes the <markup|keepcase> tags (keeping their
    contents) and is what <scm|bib-format-field> applies to every field it
    formats.
  </description-paragraphs>

  The analogue of <verbatim|change.case$> with <verbatim|"t"> (title case
  conversion: lower case except the first letter) is obtained in the
  styles by combining these functions, for instance
  <scm|bib-format-field-Locase> in <source-link|bibtex/bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm> is
  <scm|(bib-upcase-first (bib-locase <scm-arg|field>))>.

  <section|Functions which are not exported>

  The header also declares <cpp|bib_preamble>, <cpp|bib_strings_dict>,
  <cpp|bib_subst_vars>, <cpp|bib_entries>, <cpp|bib_parse_fields> and
  <cpp|bib_field_pages>, which are used by the parser and by
  <cpp|generate_bibliography> but have no glue. In particular the
  contents of <verbatim|@preamble> items are not available to the
  <scheme> styles. The <c++> function <cpp|bib_num_names> (the analogue of
  <verbatim|num.names$>) is defined but not used anywhere; styles count
  the children of <markup|bib-names> instead.

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
