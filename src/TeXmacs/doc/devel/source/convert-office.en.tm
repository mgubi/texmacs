<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <name|Word> and <name|OpenDocument> converters>

  The converters for the documents of <name|Word> (<verbatim|.docx>) and
  the texts of <name|OpenDocument> (<verbatim|.odt>) are written in
  <scheme>, in <source-link|convert/office|TeXmacs/progs/convert/office>. Both formats are zip archives of
  <name|XML> files with nearly the same model of a document, in two
  vocabularies: they are read into one tree, the <em|office tree>, and
  written from it, so that the conversion from and to <TeXmacs> is written
  once. The user-level description is in <hlink|converters for <name|Word>
  and <name|OpenDocument>|../../main/convert/office/man-office.en.tm>.

  <section|The formats and the converters>

  <source-link|init-office.scm|TeXmacs/progs/convert/office/init-office.scm> defines the formats
  <verbatim|docx> and <verbatim|odt>, loaded by need. Since
  <scm|define-format> makes the converters between a file and its
  <verbatim|-document> as a string, the document of these formats is the
  archive itself, a string of bytes.

  <\description>
    <item*|<verbatim|docx-document> <math|\<rightarrow\>>
    <verbatim|office-stree>><scm|parse-docx-document>, in
    <source-link|docxin.scm|TeXmacs/progs/convert/office/docxin.scm>.

    <item*|<verbatim|odt-document> <math|\<rightarrow\>>
    <verbatim|office-stree>><scm|parse-odt-document>, in
    <source-link|odtin.scm|TeXmacs/progs/convert/office/odtin.scm>.

    <item*|<verbatim|office-stree> <math|\<rightarrow\>>
    <verbatim|texmacs-stree>><scm|office-\<gtr\>texmacs>, in
    <source-link|officetm.scm|TeXmacs/progs/convert/office/officetm.scm>.

    <item*|<verbatim|texmacs-stree> <math|\<rightarrow\>>
    <verbatim|office-stree>><scm|texmacs-\<gtr\>office>, in
    <source-link|tmoffice.scm|TeXmacs/progs/convert/office/tmoffice.scm>.

    <item*|<verbatim|office-stree> <math|\<rightarrow\>>
    <verbatim|docx-document>, <verbatim|odt-document>><scm|serialize-docx-document>
    in <source-link|docxout.scm|TeXmacs/progs/convert/office/docxout.scm> and <scm|serialize-odt-document> in
    <source-link|odtout.scm|TeXmacs/progs/convert/office/odtout.scm>.
  </description>

  <section|Zip archives>

  <source-link|System/Files/zip_files.cpp|src/System/Files/zip_files.cpp> reads and writes zip archives without
  a library, so that the converters also work where none is linked (the
  browser). It reads the entries which are stored or deflated, with its own
  inflate, and writes all the entries stored, in the order which is given:
  an <name|OpenDocument> text must start with its entry
  <verbatim|mimetype>. The date of the entries is always the same, so that
  the same contents give the same archive. There is no encryption and no
  zip64.

  <\explain>
    <scm|(zip-archive? <scm-arg|s>)>

    <scm|(zip-unpack <scm-arg|s>)>

    <scm|(zip-pack <scm-arg|names> <scm-arg|datas>)>
  <|explain>
    Whether the string <scm-arg|s> is a zip archive; the list
    <scm|(name data name data ...)> of its entries (the directories are
    left out); the archive of the entries with these names and data.
  </explain>

  <section|The office tree>

  The tree is described at the head of <source-link|office-tools.scm|TeXmacs/progs/convert/office/office-tools.scm>,
  which also has the tools on <name|XML> trees (<scm|ox-attr>,
  <scm|ox-child>, <scm|ox-serialize>...). It is
  <scm|(office <scm-arg|block> ...)>, with its strings in UTF-8.

  <\description>
    <item*|Blocks><scm|(p ...)> with the attributes <verbatim|role>
    (<verbatim|title>, <verbatim|subtitle>, <verbatim|author>,
    <verbatim|date>, <verbatim|abstract>, <verbatim|heading>,
    <verbatim|quote>, <verbatim|code>, <verbatim|caption>,
    <verbatim|figure>, <verbatim|term>, <verbatim|definition>,
    <verbatim|toc>), <verbatim|level> and <verbatim|align>;
    <scm|(list (item <scm-arg|block> ...) ...)> with the attribute
    <verbatim|kind>; <scm|(table (row (cell <scm-arg|block> ...) ...) ...)>;
    <scm|(pagebreak)>, <scm|(rule)>, <scm|(toc)> and <scm|(meta ...)>.

    <item*|Tables>A table has the attributes <verbatim|align>,
    <verbatim|width> (a part of the paragraph, <verbatim|0.5par>) and
    <verbatim|columns> (the parts of the columns in the table). A cell has
    <verbatim|borders> (the letters of its sides which have one, among
    <verbatim|t>, <verbatim|b>, <verbatim|l> and <verbatim|r>, or
    <verbatim|none>), <verbatim|background>, <verbatim|colspan>,
    <verbatim|rowspan>, <verbatim|header> and <verbatim|align>. The grid is
    full: the cells which a wider or a higher one covers are there, with
    the attribute <verbatim|covered>.

    <item*|Text>Strings, the wrappers <verbatim|em>, <verbatim|strong>,
    <verbatim|underline>, <verbatim|strike>, <verbatim|sub>,
    <verbatim|sup>, <verbatim|code>, <verbatim|smallcaps>,
    <verbatim|mark> and <verbatim|color>, and <verbatim|link>,
    <verbatim|ref>, <verbatim|bookmark>, <verbatim|note>, <verbatim|br>,
    <verbatim|tab>, <verbatim|image> (with the bytes of its file as the
    attribute <verbatim|data>) and <verbatim|math>.

    <item*|Mathematics>A node <verbatim|math> holds <name|MathML>: its text
    (a file of an <name|OpenDocument> text), or a tree with the attribute
    <verbatim|form> <verbatim|sxml>. The attribute <verbatim|display> tells
    a formula on its own lines.
  </description>

  <section|Reading>

  Both readers work in the same way. They first read the styles, each with
  its own properties and the name of the style it is based on;
  <scm|docx-style> and <scm|odt-style> give the properties of a style with
  those which it inherits. What a paragraph is comes from the <em|names>
  of its style and of its parents (<scm|docx-roles>, <scm|odt-roles>): in
  a <name|Word> file the name of a style (<verbatim|heading 1>) is in
  English even where its identifier is in the language of the program
  which wrote the file. The runs of text with the same properties are then
  merged (<scm|office-merge>).

  Some of what the files of real programs have, and which the readers
  handle:

  <\itemize>
    <item>In <name|Word>, a list is not a structure: its items are
    paragraphs with the number of a list and a level, from which
    <scm|docx-group-lists> rebuilds the nested lists. The items of one
    list may refer to several lists which share their definition.

    <item>The borders of a <name|Word> table come from its style (with
    other borders for the first row, the last one, the bands of rows...),
    from the table and from each cell: <scm|docx-cell-format> works out the
    sides of each cell.

    <item>A field of <name|Word> is several runs: its beginning, its
    instruction, a separator, its result, its end (<scm|docx-inlines>). A
    field <verbatim|HYPERLINK> is a link, <verbatim|REF> a reference, and
    of the other ones the result is kept.

    <item>In <name|OpenDocument> the formatting by hand is in
    <em|automatic styles>, and the spaces of the file do not all count
    (<scm|odt-collapse>, <verbatim|text:s>).

    <item>A list which goes on at a deeper level is written by some
    programs as a new list whose first item only holds a list
    (<scm|odt-merge-lists>).
  </itemize>

  <section|Mathematics>

  The formulas go through <name|MathML> and its converters
  (<source-link|convert/mathml|TeXmacs/progs/convert/mathml>). <name|OpenDocument> has <name|MathML> as it is.
  The formulas of <name|Word> are in its own format, OMML, whose
  constructions are those of <name|MathML> under other names:
  <source-link|omml.scm|TeXmacs/progs/convert/office/omml.scm> maps one to the other in both directions
  (<scm|omml-\<gtr\>mathml>, <scm|mathml-\<gtr\>omml>). The text of a run of
  OMML is cut into identifiers, numbers and operators, which
  <name|MathML> wants apart; in the other direction a big operator takes
  what follows it as its operand, and brackets what they enclose.

  <scm|oftm-from-mathml> gives the converter of <name|MathML> its input as
  it wants it (elements with the prefix <verbatim|m:>, an environment), and
  keeps the text of a formula which cannot be converted. On export,
  <scm|tmof-mathml> writes the characters themselves for the names of
  <name|MathML> (<verbatim|&alpha;>), which an office file does not know.

  <section|From and to <TeXmacs>>

  <scm|office-\<gtr\>texmacs> takes together the blocks which belong
  together (the paragraphs of a quotation, the lines of code, a figure or
  a table and its caption, terms and definitions) and writes a table as a
  <markup|tabular> with few formats of rectangles of cells
  (<scm|oftm-rectangles>), or as a <markup|block> when all its borders are
  there.

  <scm|texmacs-\<gtr\>office> comes from the converter to
  <name|Markdown> (see <hlink|the <name|Markdown>
  converters|convert-markdown.en.tm>), with the same handlers and the same
  expansion of the document before it is converted
  (<cpp|edit_typeset_rep::exec_office>,
  <source-link|tmoffice-expand.scm|TeXmacs/progs/convert/office/tmoffice-expand.scm>). Its handlers build a tree like the
  one of <name|Markdown>, which <scm|tmof-finalize> turns into the office
  tree. What <name|Markdown> has not is added: formulas as <name|MathML>,
  images with their bytes and their sizes (<scm|tmof-image>), the formats
  of the tables (<scm|tmof-table>), colors, notes in their place.

  A drawing (<markup|graphics>, alone or over a text) and an image which
  the office programs cannot read are made into a picture by the editor
  (<scm|tmof-picture>): <scm|print-snippet> typesets the tree and writes a
  PNG of it, and its PDF, of which <scm|pdf-\<gtr\>svg-native> makes an
  <name|SVG> with <name|MuPDF> (<cpp|mupdf_pdf_to_svg> in
  <source-link|Plugins/MuPDF/mupdf_pdf_renderer.cpp|src/Plugins/MuPDF/mupdf_pdf_renderer.cpp>; false in the builds without it).
  The node <verbatim|image> then has the <name|SVG> as its attribute
  <verbatim|svg>, and the writers put both in the archive: <name|Word>
  shows the <name|SVG> named by the extension of the bitmap, and a frame
  of <name|OpenDocument> has the two images, of which the first one that a
  program can show is shown. The <name|SVG> is only made for drawings: the
  one of the picture of an embedded image is not always shown well.

  <section|Numbers, labels and references>

  The numbers of a document which is expanded are text: <samp|Theorem 1>,
  a section title laid out as a table of its number and its text, a link
  for each reference. <scm|texmacs-\<gtr\>office> makes of them what the
  office programs count themselves.

  <\itemize>
    <item>A number is a node <scm|(seq "1")> of a sequence
    (<scm|tmof-seq>): the first of its candidates of which it is the
    next number (<samp|Theorem> for all the theorems, whose counter is
    shared, or the name of the theorem; <samp|Figure>, <samp|Table>,
    <samp|Equation>, <samp|Reference>). A number which is the next one
    of no sequence has no sequence, and is written as it is.

    <item>The number of a heading is apart from its title
    (<scm|tmof-numbered-title>), in the attribute <verbatim|number>.

    <item>A label is a bookmark, and a link inside the document a node
    <verbatim|ref>. As in <TeXmacs>, a label is the label of the last
    number before it: <scm|tmof-bind> gives the bookmarks to the numbers
    (the attribute <verbatim|labels> of a <verbatim|seq> or of a heading)
    and tells each <verbatim|ref> what it refers to (<verbatim|kind>).
  </itemize>

  The writers make fields of them. In <name|Word> a number is a field
  <verbatim|SEQ> inside the bookmarks of its labels, and a reference a
  field <verbatim|REF> to one of them, which shows what the bookmark
  holds (<verbatim|\\r>, the number of its paragraph, for a heading);
  the headings are numbered by a list which their styles refer to. The
  fields are written in their long form (a run which begins the field,
  its instruction, a separator, its result, its end): not all programs
  read <verbatim|w:fldSimple>. In <name|OpenDocument> a number is a
  <verbatim|text:sequence> inside bookmarks, a reference a
  <verbatim|text:bookmark-ref> to the text of the bookmark or to the number
  of the heading, and the headings are numbered by the outline style.

  In the other direction the readers give the fields <verbatim|SEQ> and
  the elements <verbatim|text:sequence> as nodes <verbatim|seq>, and
  <scm|office-\<gtr\>texmacs> takes away the names and the numbers which
  <TeXmacs> writes itself (<scm|oftm-named-start>): a paragraph of the
  role <verbatim|theorem> which starts with <samp|Lemma 2.> is a
  <markup|lemma>, a formula followed by its number an
  <markup|equation>, the entries of a bibliography a
  <markup|bibliography>. A reference is a <markup|reference> when its
  target is the label of something with a number
  (<scm|oftm-numbered-labels>) and its text is a number; else it stays a
  link with its text.

  <section|Writing>

  The writers build the <name|XML> files as trees and serialize them
  (<scm|ox-serialize>). Both write named styles for the paragraphs, so
  that the result can be restyled.

  <\itemize>
    <item><source-link|docxout.scm|TeXmacs/progs/convert/office/docxout.scm>: each list of the text is a list of
    its own for <name|Word>, so that its numbers start at 1; the notes go
    to their file; the links and the images are relations of the text.

    <item><source-link|odtout.scm|TeXmacs/progs/convert/office/odtout.scm>: the same formatting by hand is the
    same automatic style (<scm|ot-style>); a formula is a file of
    <name|MathML> in a directory of the archive, listed in the manifest.
  </itemize>

  <section|Tests>

  The suite <verbatim|office> (<source-link|check/office-test.scm|TeXmacs/progs/check/office-test.scm>) tests the
  archives, the two readers on small archives which it makes from their
  <name|XML>, the formulas, the conversions from and to <TeXmacs>, the
  writers by reading back what they write, and the round trips through
  both formats: <scm|(run-regression-suite "office")>.

  The suite cannot tell whether the office programs accept the files. For
  this, convert a file which was exported with <name|LibreOffice>
  (<verbatim|soffice --headless --convert-to pdf>), read it with
  <name|pandoc>, and open it in <name|Word>. <name|pandoc> also makes test
  documents of both formats from <name|Markdown>.

  <section|Known limitations>

  <\itemize>
    <item>A number which is not the next one of its sequence (numbers by
    sections) is text. The names of the theorems are only known in
    English on import.

    <item>The drawings, charts and text boxes of the office programs,
    comments and tracked changes are not imported.

    <item>The entries of an archive are written without compression.
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
