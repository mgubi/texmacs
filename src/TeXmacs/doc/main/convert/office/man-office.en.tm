<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Converters for <name|Word> and <name|OpenDocument>>

  <TeXmacs> reads and writes the documents of <name|Microsoft Word>
  (<verbatim|.docx>) and the texts of <name|OpenDocument>
  (<verbatim|.odt>), the format of <name|LibreOffice> and
  <name|OpenOffice>. A file with one of these suffixes opens as a document,
  and a document is written with <menu|File|Export|Word> or
  <menu|File|Export|OpenDocument> and read with <menu|File|Import>. Nothing
  else is needed: the converters are part of <TeXmacs>.

  The two formats are read into the same structure and written from it, so
  that what is said below holds for both.

  <paragraph|What is kept>

  The converters keep the structure of a document, and the formatting
  which a reader would miss; they do not try to keep its look. A paragraph
  is what its style says: a paragraph with the style <samp|Heading 2> is a
  subsection, whatever its font is, and a text in a large bold font with
  the style of the body is a paragraph in bold.

  <\description>
    <item*|Title>The paragraphs with the styles <samp|Title>,
    <samp|Subtitle>, <samp|Author>, <samp|Date> and <samp|Abstract> at the
    start of a document are its title.

    <item*|Headings>The levels of the headings are the sections,
    subsections and so on. A table of contents is the one of <TeXmacs>,
    which makes it again.

    <item*|Text>Emphasized, strong, underlined and struck through text,
    subscripts and superscripts, small capitals, code (a font with a fixed
    width), colors and highlighting, links, line and page breaks.

    <item*|Lists>With bullets or numbers, nested, with several paragraphs
    in an item; the lists of terms and definitions.

    <item*|Tables>With the borders and the backgrounds of their cells, the
    cells over several columns or rows, the alignments, the width of the
    table and of its columns when it has one, tables inside cells.

    <item*|Mathematics>The formulas of both formats become formulas of
    <TeXmacs> which can be edited, and the other way round: the formulas
    which are written are those of the equation editors of <name|Word> and
    of <name|LibreOffice>. The conversion goes through <name|MathML>.

    <item*|Images>The images are kept inside the document, with their
    sizes. A paragraph of images followed by a caption is a figure, and so
    for a table.

    <item*|Drawings>A drawing made with <TeXmacs> is exported as a
    picture: an <name|Svg> image, which stays sharp at any size, with a
    bitmap of it for the programs which do not show <name|Svg> (the
    versions of <TeXmacs> without <name|MuPDF> write the bitmap only). An
    image in a format which the office programs do not read (<name|Pdf>,
    <name|Postscript>) is exported as a bitmap of what <TeXmacs>
    displays, and an <name|Svg> image as itself with its bitmap.

    <item*|Notes>Footnotes and endnotes are footnotes.

    <item*|Quotations and code>From the styles <samp|Quote> and
    <samp|Source Code> (<samp|Quotations> and <samp|Preformatted Text> in
    <name|OpenDocument>).
  </description>

  <paragraph|Exporting>

  A document is first expanded as for <name|Html>: the numbers of the
  sections, of the theorems and of the equations, the references and the
  citations are those which <TeXmacs> displays (update the document first,
  with <menu|Document|Update|All>). The file which is written uses named
  styles for its paragraphs (<samp|Heading 1>, <samp|Title>,
  <samp|Caption>, <samp|Quote>...), so that its look can be changed at once
  in <name|Word> or <name|LibreOffice> by changing these styles or by
  applying a template.

  Theorems, proofs and the like are written with their name and number in
  bold before their body, and figures and tables with their caption after
  them.

  <paragraph|Limitations>

  <\itemize>
    <item>Fonts, sizes, indentations and spacings of the text are not
    kept, in either direction.

    <item>The numbers of the sections and the references are written as
    text: they are not renumbered by <name|Word> or <name|LibreOffice>
    when the document changes there. In the other direction, a
    cross-reference is a link to its target, with the text it had.

    <item>A drawing which was exported is a picture: it cannot be edited
    in the office programs, and it comes back as an image. The drawings,
    charts and text boxes of the office programs are not imported.

    <item>Comments and the history of the changes are not imported: the
    text is the one with all the changes accepted.

    <item>The bibliography is text.
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
