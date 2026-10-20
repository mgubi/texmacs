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
    start of a document are its title. (The title of the properties of the
    file is not shown: it is often the one of a template.)

    <item*|Page and paragraphs>The size of the page, its orientation and
    its margins, and the usual paragraph of the document (the size of its
    font, a font without serifs, its alignment, the indentation of its
    first line, the spaces between its lines and around it) are the ones
    of the <TeXmacs> document, in both directions. A paragraph aligned
    otherwise keeps its alignment.

    <item*|Headings>The levels of the headings are the sections,
    subsections and so on; a heading has a number when it has one in the
    file. A table of contents is the one of <TeXmacs>, which makes it
    again.

    <item*|Text>Emphasized, strong, underlined and struck through text,
    subscripts and superscripts, small capitals, code (a font with a fixed
    width), colors and highlighting, links, line and page breaks.

    <item*|Lists>With bullets or numbers (1, a, A, i, I), nested, with
    several paragraphs in an item, from their first number (a list of
    <name|Word> which goes on after a paragraph goes on with its numbers);
    the lists of terms and definitions.

    <item*|Tables>With the borders and the backgrounds of their cells, the
    cells over several columns or rows, the alignments, the width of the
    table and of its columns when it has one, tables inside cells.

    <item*|Mathematics>The formulas of both formats become formulas of
    <TeXmacs> which can be edited, and the other way round: the formulas
    which are written are those of the equation editors of <name|Word> and
    of <name|LibreOffice>. The conversion goes through <name|MathML>.

    <item*|Images>The images are kept inside the document, with their
    sizes and the parts which are cut from them. A paragraph of images
    followed by a caption is a figure, and so for a table. The pictures of
    <name|Windows> (<name|Emf>, <name|Wmf>) which <name|Word> documents
    often have are made images by <name|LibreOffice>, when it is installed,
    and are a frame with their name else: all the pictures of a file are
    converted at once, a picture which is there several times once, and a
    <name|LibreOffice> which does not answer within two minutes is stopped
    (under <name|Windows>, the conversion waits for it 30 seconds only,
    and the pictures which it has not made by then are frames).
    It has a profile of its own, in <verbatim|system/office-profile> of the
    home directory of <TeXmacs>.

    <item*|Drawings>A drawing made with <TeXmacs> is exported as a
    picture: an <name|Svg> image, which stays sharp at any size, with a
    bitmap of it for the programs which do not show <name|Svg> (the
    versions of <TeXmacs> without <name|MuPDF> write the bitmap only). An
    image in a format which the office programs do not read (<name|Pdf>,
    <name|Postscript>) is exported as a bitmap of what <TeXmacs>
    displays, and an <name|Svg> image as itself with its bitmap.

    <item*|Notes>Footnotes and endnotes are footnotes.

    <item*|Boxes of text and shapes>A box of text, or a shape (a
    rectangle, an ellipse...) with or without text, is shown as a framed
    box of its width with its fill, on its side, in a paragraph of its own
    before the paragraph which it belongs to; a shape without text is a
    drawing. Their text can be edited, and they are written back as boxes
    and shapes, in both directions. The shapes in groups and the other
    drawings give their text after their paragraph. A figure in a frame of
    <name|LibreOffice>, with its caption, is a figure. Hidden text is left
    out.

    <item*|Quotations and code>From the styles <samp|Quote> and
    <samp|Source Code> (<samp|Quotations> and <samp|Preformatted Text> in
    <name|OpenDocument>).
  </description>

  <paragraph|The style office>

  A <name|Word> or <name|OpenDocument> file which is opened is a document
  of the style <tmstyle|office>, which keeps what the file says and the
  standard tags of <TeXmacs> do not, so that the file which is saved again
  is close to the one which was read:

  <\description>
    <item*|Named styles>A paragraph or a piece of text of a named style of
    the file (<samp|Warning>, <samp|Keyword>) is <markup|office-par> or
    <markup|office-span> with the name of the style, which is shown as the
    file shows it (its font, size, color, alignment, indentation, spacing,
    background and border): the style is a macro <verbatim|office-par->name
    or <verbatim|office-span->name of the document, which can be edited.
    When the document is saved again in its format, a style whose macro
    was changed gets the properties which were changed (as bold, italic,
    the color, the size, the font, the alignment, the indentations, the
    spaces, the background, the border); its other properties are those of
    the file. The look of the headings is the one of the titles of the
    sections (<verbatim|section-title>...), and the look of the title, the
    subtitle and the date at the start of the document is the one of the
    macros <verbatim|doc-title>, <verbatim|doc-subtitle> and
    <verbatim|doc-date> of the document; they are not written back.

    <item*|Headers and footers>They are the headers and footers of the
    pages of the document, with the number of the page; those of the
    first page are at the start of the text (a paragraph may be put
    before them). A header or a footer of the first page is removed by
    emptying it: when its marker is deleted, the one of the file is
    written back. In a document without a header of the first page in its
    file (or made in <TeXmacs>), only a marker at the very start of the
    text is the one of the first page; one further on is for a later
    page, and is not written. A header or a footer which was changed in <TeXmacs> is
    written instead of the one of the file, as one paragraph, centered or
    on the right when it starts with a tab (and ends with one); one which
    was removed is not written. The others are those of the file, as they
    were. In a file of the other format, or without the file (see below),
    every header and footer is written from <TeXmacs>, in this way.

    <item*|Fields>A field (a page number, a date, the title of the
    document, a citation of <name|Zotero>) is <markup|office-field>, which
    shows its result and keeps its instruction. A field inside the
    instruction of another one, as <verbatim|PAGE> in <verbatim|IF {
    PAGE } = 3 "yes" "no">, is an <markup|office-field> inside the
    instruction, and is written as a field inside the field. The
    bibliography of
    <name|Zotero> in a Word file, which is several paragraphs, is a field
    too, so that <name|Zotero> can refresh it after the document is
    saved.

    <item*|Comments and changes>A comment is <markup|office-comment>,
    after its mark, with its author, and a comment on a range of the text
    of a paragraph is <markup|office-commented>, with the text, which is
    marked, and the comment; the text which was inserted and the
    one which was deleted while the changes are tracked are
    <markup|office-ins> and <markup|office-del>, in color.

    <item*|Columns>A section in several columns is
    <markup|office-columns>, or <markup|office-columns*> with the line
    between the columns (which <TeXmacs> does not draw) and the space
    between them.

    <item*|Floating images>An image with the text around it is
    <markup|office-float>, with its side, where it is and how the text
    goes around it: <TeXmacs> shows it in a paragraph of its own, before
    the paragraph which it belongs to. A box of text is
    <markup|office-box> (with its side, the rest of where it is, its width,
    its fill, its line and its text), and a shape without text
    <markup|office-shape>.

    <item*|The file>The file itself is kept in the environment of the
    document (<verbatim|office-package>), with the headers, the footers
    and the macros of the named styles as they were read
    (<verbatim|office-original>): when it is saved again in its format,
    its named styles, its pages with their headers and footers, its fonts,
    its theme, its settings and its properties are written again (with
    what was changed in <TeXmacs>), and the text, its notes, comments and
    lists are those of <TeXmacs>. The parts of the file which nothing that
    is written names any more (a picture of a paragraph which was deleted,
    a header which was replaced) are not written, nor the small picture of
    its first page which <name|Word> files have.

    Since the whole file is kept, a <TeXmacs> document of the style
    <tmstyle|office>, saved as a <verbatim|.tm> file, also holds what was
    deleted from it in <TeXmacs>: the old text, comments and pictures, and
    the properties of the file (its author...). The <name|Word> or
    <name|OpenDocument> file which <TeXmacs> writes keeps the properties of
    the file too (<verbatim|docProps/core.xml> or <verbatim|meta.xml>:
    title, author, dates; the authors of the comments and the changes), but
    not the company and the titles of its parts
    (<verbatim|docProps/app.xml>). Send the <name|Word> or
    <name|OpenDocument> file which <TeXmacs> writes rather than the
    <verbatim|.tm> file when that matters, or remove the variables
    <verbatim|office-package> and <verbatim|office-original> from the
    initial environment of the document (it is then written as a new
    file). Saved in the
    other format (a Word file as an OpenDocument file, or the reverse), the
    named styles are written with their look, taken from the macros
    <verbatim|office-par->name and <verbatim|office-span->name of the
    document, else from the file.
  </description>

  The option <verbatim|office-\<gtr\>texmacs:faithful> of the converter
  (on by default, in <menu|Edit|Preferences>, <samp|Convert>,
  <samp|Office>) makes these
  documents; without it, a file is a document
  of the style <tmstyle|generic>, with the look of the named styles of its
  text, the results of its fields and its changes accepted.

  <paragraph|Exporting>

  The file which is written uses named styles for its paragraphs
  (<samp|Heading 1>, <samp|Title>, <samp|Caption>, <samp|Quote>,
  <samp|Theorem>, <samp|Proof>, <samp|Bibliography>...), so that its look
  can be changed at once in <name|Word> or <name|LibreOffice> by changing
  these styles or by applying a template. Its text has a font with serifs
  (<name|Times New Roman>), or <name|Arial> for a document without serifs,
  and the size of the font of the document. Several lines of equations
  (<markup|eqnarray>, <markup|align>) are an array of equations of
  <name|Word>, all the screens of a presentation are written, each on a
  page of its own, and so is what is folded.

  <paragraph|Numbers and references>

  The numbers of a document stay numbers in the office programs, which
  count them again when the document changes there (in <name|Word>, select
  all and press <key|F9>; in <name|LibreOffice>, <samp|Tools>,
  <samp|Update>, <samp|Fields>). Update the document in <TeXmacs> first,
  with <menu|Document|Update|All>: the numbers which are written are those
  which it displays.

  <\description>
    <item*|Headings>The sections are numbered by the office program, as
    1, 1.1, 1.1.1; a section without a number has none.

    <item*|Theorems, figures, tables, equations>Their numbers are fields
    of a sequence (<samp|Theorem>, <samp|Figure>, <samp|Table>,
    <samp|Equation>). Theorems, proofs and the like are paragraphs with
    their name and number in bold, in the style <samp|Theorem>,
    <samp|Remark> or <samp|Proof>; a figure or a table is followed by its
    caption.

    <item*|References>A reference is a field which shows the number of
    its target, and follows it. The labels are bookmarks; in a
    <name|Word> file their names only have letters, digits and
    <verbatim|_> (<verbatim|thm:main> is <verbatim|tm_thm_main>).

    <item*|Bibliography>The entries are paragraphs in the style
    <samp|Bibliography> with numbers of the sequence <samp|Reference>,
    and a citation is a reference to its entry.
  </description>

  A number is only written as a field when it is the next one of its
  sequence: with another way of numbering (by sections, as 2.1) it is
  written as it is, and the references to it show it as it is.

  In the other direction, the paragraphs in the styles <samp|Theorem>,
  <samp|Remark> and <samp|Proof> which start with an English name
  (<samp|Theorem>, <samp|Lemma>, <samp|Definition>, <samp|Proof>...) are
  the environments of <TeXmacs>, a formula with its number is a numbered
  equation, the paragraphs in the style <samp|Bibliography> are a
  bibliography, and a reference whose text is the number of its target is
  a reference of <TeXmacs>. A document which was exported comes back with
  its numbers and its references.

  <paragraph|Limitations>

  <\itemize>
    <item>Of the formatting by hand, the size of a text, its font, its
    highlighting and its color, the capitals, the kinds of underlines and
    of lines through, the spacing of the letters, and the indentations,
    the spaces, the line spacing, the background and the border of a
    paragraph are kept, in both directions. A font which <TeXmacs> does
    not have is shown in the closest family (with serifs or not), and
    written with its name. The titles of the sections have the look of
    the headings of the file (font, size, capitals, underlines), and the
    named styles keep theirs. A line over the text is kept in
    OpenDocument files; Word has none, and gets the text without it. A
    double line through and the
    underlines which are not single or double are shown as single ones,
    and the spacing of the letters is not shown. Text in lower case, and
    text whose words begin with capitals (kept, but shown as it is), are
    kept in OpenDocument files; Word files have neither, and get the text
    as it is shown. A section with another
    page than the first one is not kept.

    <item>A reference of an office document whose text is more than a
    number (\PTable 3\Q, the title of a section) is a link to its
    target, with the text it had. The theorems of a document in another
    language than English stay paragraphs. A label in the text, away from
    what it is the label of, is written at its number (a reference shows
    the number) and at its place, as a hidden bookmark <verbatim|_TmAt_>name:
    it comes back at its place.

    <item>A drawing which was exported is a picture: it cannot be edited
    in the office programs, and it comes back as an image. The drawings and
    charts of the office programs are not imported (but for their
    pictures). An image which floats beside the text (the text around it)
    is a paragraph of its own before its paragraph, on its side: <TeXmacs>
    does not put text around an image. In the style <tmstyle|office> it
    keeps where it was and how the text went around it, and it floats
    again in the file which is written. A picture of a Word file which is
    linked to an address on the web, and not in the file, is an image with
    that address, and it stays a link in the file which is written.

    <item>The bibliography is its entries as they are displayed: the
    data of the references (authors, years) are not exchanged; the
    citation fields of <name|Zotero> are kept as fields in the style
    <tmstyle|office>.

    <item>A comment in a note of a Word file is read in the note. In a
    Word file which is written, a comment in a note is at the mark of the
    note, in the text, since <name|LibreOffice> does not open Word files
    with comments in their notes.
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
