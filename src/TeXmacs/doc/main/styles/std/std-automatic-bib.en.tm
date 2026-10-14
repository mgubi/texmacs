<TeXmacs|2.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Bibliographies>

  The following macros may be used in the main text for citations to entries
  in a bibliographic database.

  <\explain|<explain-macro|cite|ref-1|<math|\<cdots\>>|ref-n>>
    Each argument <src-arg|ref-i> is the key of a bibliographic reference:
    an entry of the <BibTeX> file of the bibliography, of the bibliographic
    database, or of Zotero. The citations are displayed in the same way as
    they are referenced in the bibliography and they also provide hyperlinks
    to the corresponding references. The citations are displayed as question
    marks if you did not generate the bibliography. Pressing <key|tab> inside
    the arguments completes the key, and <shortcut|(kbd-alternate-tab)>
    opens a window which searches the references (see <hlink|Compiling a
    bibliography|../../links/man-bibliography.en.tm>).
  </explain>

  <\explain|<explain-macro|nocite|ref-1|<math|\<cdots\>>|ref-n>>
    Similar as <markup|cite>, but the citations are not displayed in the main
    text: a small flag marks them on the screen. The key <verbatim|*> puts
    all the references of the <BibTeX> file into the bibliography.
  </explain>

  <\explain|<explain-macro|cite-detail|ref|info>>
    A bibliographic reference <src-arg|ref> like <markup|cite> and
    <markup|nocite>, but with some additional information <src-arg|info>,
    like a chapter or a page number.
  </explain>

  <\explain|<explain-macro|cite-raw|ref-1|<math|\<cdots\>>|ref-n>>
    The citations of <markup|cite>, without the brackets of
    <markup|render-cite>.
  </explain>

  <\explain|<explain-macro|cite-TeXmacs|ref-1|<math|\<cdots\>>|ref-n>>
    A sentence which says that the document was written with <TeXmacs>,
    citing references about <TeXmacs> (<menu|Cite TeXmacs>). These
    references come with <TeXmacs>, and need no <BibTeX> file.
  </explain>

  A document may have several bibliographies, each one of which collects
  the citations made with its own prefix:

  <\explain|<explain-macro|with-bib|bib|body>>
    The citations of <src-arg|body> go into the bibliography with the
    prefix <src-arg|bib> (see <hlink|Multiple
    extractions|../../links/man-multiple-extractions.en.tm>).
  </explain>

  <\explain|<src-var|bib-prefix>>
    The prefix of the bibliography into which the citations go
    (<verbatim|bib> by default); <markup|with-bib> changes it locally.
  </explain>

  The following macros may be redefined if you want to customize the
  rendering of citations or entries in the generated bibliography:

  <\explain|<explain-macro|render-cite|body>>
    Macro for rendering a citation <src-arg|body> at the place where the
    citation is made using <markup|cite>. The <src-arg|body> may be a
    single reference, like \PTM98\Q, or a list of references, like \PEuler1,
    Gauss2\Q. By default, it puts the citations between square brackets.
  </explain>

  <\explain|<explain-macro|render-cite-detail|body|info>>
    Similar to <markup|render-cite>, but for detailed citations made with
    <markup|cite-detail>.
  </explain>

  <\explain|<explain-macro|cite-sep>>
    The separator between the references of a citation with several keys
    (a comma and a space by default).
  </explain>

  <\explain|<explain-macro|cite-arg|key>>
    The rendering of one key in a citation: the reference to the
    corresponding item of the bibliography.
  </explain>

  <\explain>
    <explain-macro|render-bibitem|content>

    <explain-macro|transform-bibitem|content>
  <|explain>
    The generated bibliography is a list of bibliographic items, based on
    macros which come from <LaTeX> (<markup|bibitem>, <markup|newblock>,
    <markup|protect>, <abbr|etc.>), whether it was produced by the styles of
    <TeXmacs> (whose names start with <verbatim|tm->) or by <BibTeX>. These
    macros are all defined internally in <TeXmacs> and eventually boil down
    to calls of the <markup|render-bibitem>, which behaves in a similar way
    as <markup|item*>, and which may be redefined by the user.

    The <markup|transform-bibitem> is used to \Pdecorate\Q the
    <src-arg|content>. For instance, <markup|transform-bibitem> may put
    angular brackets and a space around <src-arg|content>.
  </explain>

  <\explain>
    <explain-macro|bibitem|label>

    <explain-macro|bibitem-with-key|label|key>

    <explain-macro|bibitem*|label>
  <|explain>
    The start of an item of the bibliography, with the label
    <src-arg|label> (such as \P1\Q or \PKnu84\Q): <markup|bibitem> also
    makes the target of the citations of the key <src-arg|label>,
    <markup|bibitem-with-key> that of the citations of <src-arg|key>, and
    <markup|bibitem*> none.
  </explain>

  <\explain|<explain-macro|bib-list|largest|body>>
    The individual \Pbibitems\Q are enclosed in a <markup|bib-list>, which
    behaves in a similar way as the <markup|description> environment, except
    that we provide an extra parameter <src-arg|largest> which contains a
    good indication about the largest width of an item in the list.
  </explain>

  <\explain>
    <src-var|bibitem-width>

    <explain-macro|bibitem-hsep>
  <|explain>
    The minimal width of the labels of the items (<verbatim|3em> by
    default), and the space between a label and its item.
  </explain>

  <\explain>
    <explain-macro|newblock>

    <explain-macro|protect>

    <explain-macro|citeauthoryear|author|year>
  <|explain>
    Macros of <LaTeX> found in the generated bibliographies: the first two
    produce nothing, and <markup|citeauthoryear> shows its two arguments.
  </explain>

  <paragraph|Citations by authors and years>

  The package <tmpackage|cite-author-year>, added to the document when a
  bibliography style cites by authors and years (<verbatim|natbib>), offers
  the citations of the <verbatim|natbib> package of <LaTeX>. The macros
  below take keys as arguments; their variants with a star (such as
  <markup|cite-textual*>) use the full list of the authors instead of an
  abbreviated one.

  <\explain>
    <explain-macro|cite-textual|ref-1|<math|\<cdots\>>|ref-n>

    <explain-macro|citet|ref-1|<math|\<cdots\>>|ref-n>
  <|explain>
    Textual citations, such as \PEinstein (1905)\Q.
  </explain>

  <\explain>
    <explain-macro|cite-parenthesized|ref-1|<math|\<cdots\>>|ref-n>

    <explain-macro|citep|ref-1|<math|\<cdots\>>|ref-n>
  <|explain>
    Parenthesized citations, such as \P(Einstein, 1905)\Q.
  </explain>

  <\explain>
    <explain-macro|cite-author|ref>

    <explain-macro|cite-year|ref>

    <explain-macro|cite-author-link|ref>

    <explain-macro|cite-year-link|ref>
  <|explain>
    The authors or the year of a reference alone, without or with a link
    to the bibliography.
  </explain>

  The package <tmpackage|cite-sort> sorts the references of each citation
  by their labels, and joins consecutive numbers into a range (\P[1\U3]\Q).

  <tmdoc-copyright|1998--2026|Joris van der Hoeven, Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>
