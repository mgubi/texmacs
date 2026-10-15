<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Converters for <name|Markdown>>

  <TeXmacs> reads and writes <name|Markdown>, the plain text format of the
  <verbatim|README> files, of many note taking programs and of the answers
  of AI assistants. A file with the suffix <verbatim|.md> (or
  <verbatim|.markdown>, <verbatim|.mkd>) opens as a document; a document is
  written as <name|Markdown> with <menu|File|Export|Markdown> and read with
  <menu|File|Import|Markdown>, and <menu|Edit|Copy to|Markdown> and
  <menu|Edit|Paste from|Markdown> do the same for a piece of a document. A
  text is never taken for <name|Markdown> by its contents: to paste one,
  use <menu|Edit|Paste from|Markdown>, or make it the default with
  <menu|Tools|Miscellaneous|Import selections as>.

  The dialect is <name|CommonMark> with the extensions of <name|GitHub>
  (tables, struck through text, task lists, addresses which are links) and
  those of <name|Pandoc> which matter for scientific texts: formulas,
  footnotes and a header with the title.

  <paragraph|Mathematics>

  The formulas are <LaTeX>, <verbatim|$x^2$> in the text and

  <\verbatim-code>
    $$

    \\int_0^1 x^2 \\,dx = \\frac{1}{3}

    $$
  </verbatim-code>

  on lines of their own; <verbatim|\\(x^2\\)> and <verbatim|\\[x^2\\]> are
  read too. They go through the <hlink|converters for
  <LaTeX>|../latex/man-latex.en.tm> in both directions, so that a formula
  which is read becomes a formula of <TeXmacs> which can be edited, and the
  formulas which are written are those which <name|MathJax> and
  <name|KaTeX> display, on <name|GitHub> for instance. As for
  <name|Pandoc>, a dollar opens a formula only when no space follows it,
  and closes one only when no space precedes it and no digit follows it:
  <verbatim|a price of $5 and $6> has no formula. A formula which is a
  single symbol, such as an arrow, is written in <LaTeX> too, as
  <verbatim|$\\rightarrow$>, and is a formula again when it is read.

  <paragraph|The style for Markdown files>

  A file which is opened is a document of the style <tmstyle|markdown>:
  text without serifs, headings without numbers, and a few macros which
  keep what <name|Markdown> says and the standard tags do not. Such a
  document is written as the file it was read from: what you did not
  touch keeps its meaning, and what <name|Markdown> cannot say in two ways
  keeps its text; what you edit is written anew.

  <\description>
    <item*|Headings>The six levels are <markup|section>,
    <markup|subsection>, <markup|subsubsection>, <markup|paragraph>,
    <markup|subparagraph> and <markup|md-h6>, whatever the first one of the
    file is, and a file has no title of its own.

    <item*|The header>The lines of a <name|YAML> header are kept in
    <markup|md-metadata>, one <markup|md-meta> with a key and its value
    for each of them (and without key for a line which has none, or which
    is not written <verbatim|key: value>: an item of a list, a comment, a
    line with other spaces). Keys can be edited, added and removed, and
    are written in their order; a line typed in the header is written as
    it is, or as a comment when <name|YAML> has no such line. Whatever the
    header holds between its two lines <verbatim|---> is kept, if each of
    its lines is a key, a comment, an item of a list or an indented line,
    and one of them at least is a key: a heading between two rules stays
    a heading. A header without key is written as paragraphs, and an empty
    one is not written.

    <item*|Formulas, tables, <name|Html>>They are in <markup|md-source>
    (in a paragraph) or <markup|md-block-source> (on lines of their own),
    with the text of the file for them: <verbatim|\\(x\\)> stays
    <verbatim|\\(x\\)>, a table keeps the widths of its columns, and
    <name|Html> which <TeXmacs> does not understand, a comment for
    instance, is kept and shown as a flag. As soon as the formula, the
    table or the text is edited, it is written by <TeXmacs>. So are the
    tabs, which are displayed as spaces (in code, each line with tabs
    keeps them until it is edited), the definitions of links to which
    nothing refers (the comments <verbatim|[//]: # (text)> among them),
    and the definitions of footnotes which the text does not use. The
    definitions of the links and footnotes which a table uses are kept
    too, and a mark of a footnote in a table keeps its label.

    <item*|Lists>The lists whose items start with <verbatim|->,
    <verbatim|*> and <verbatim|+> are <markup|itemize-minus>,
    <markup|itemize-dot> and <markup|itemize-arrow>, and a list with blank
    lines between its items is in <markup|md-loose>.

    <item*|Code>The words after the fence are kept by <markup|md-fence>
    when the language is not one of those which <TeXmacs> highlights, or
    is not written as <TeXmacs> writes it (<verbatim|c> for
    <verbatim|cpp>). A tab which starts a line of code stays a tab, as in
    a formula, in <name|Html> and in the header.

    <item*|Images>An image with a description or a title is in
    <markup|md-image>; an image alone with a description and no title is a
    figure with this caption, as below.

    <item*|Footnotes>A footnote whose label is not its number is
    <markup|md-footnote>, with this label, and <markup|md-footnote-text>
    marks the place where its text is written when this is not the end of
    the file, in a quotation or a list as well. The text of a footnote is
    at its first mark; the other marks of the same footnote, a mark inside
    the text of a footnote and a mark without footnote are kept as they
    are written. When two footnotes have the same label, the second one
    gets another label as it is written.
  </description>

  The text which these macros keep is not displayed, and is written as it
  is: a document of this style which someone else wrote may write text
  you do not see into the <name|Markdown> file, as <markup|specific> may.
  The other formats display the macros and leave out what they keep.

  What <name|Markdown> writes in several ways is written in one when it
  is none of the above: the lines of a paragraph are joined (the spaces
  and tabs which start them are not text), a heading
  with a line under it starts with <verbatim|#>, code is between
  <verbatim|```>, a rule is <verbatim|***>, emphasis is written with
  stars, a link through a definition is written with its address, and a
  character written as <verbatim|&amp;> or after a backslash is written
  as itself unless it would be taken for markup. The file which is
  written says the same, and is written as itself from then on.

  With <menu|Edit|Preferences|Convert|Markdown> a file is read as a
  document of the style <tmstyle|generic> instead, with the title and
  the sections which <TeXmacs> displays for it; text which is pasted is
  always read so, since it may go into any document.

  <paragraph|What is read>

  <\description>
    <item*|Headings>The lines which start with <verbatim|#>, and those
    underlined with <verbatim|===> or <verbatim|--->, are sections,
    subsections and so on. When a text has a single heading of the first
    level, at its start, it is the title of the document and the other
    headings move up.

    <item*|Header>A <name|YAML> header between two lines <verbatim|---> at
    the start of the file gives the title, the authors
    (<verbatim|author>, one or a list), the date and the abstract. Pasted
    text has no header: its lines <verbatim|---> are rules.

    <item*|Text>Emphasized, strong and struck through text, code, links,
    images, line breaks, the entities of <name|Html>. The quotes and the
    apostrophes of typography stay these characters, also in code and in
    addresses, and are written back as they were.

    <item*|Images and figures>An image alone in its paragraph, with a
    description, <verbatim|![description](file.png)>, is a figure whose
    caption is this description (or its title,
    <verbatim|![...](file.png "title")>, when it has one), as for
    <name|Pandoc>. An image inside a text is the image alone. An image may
    have a size, with the attributes of <name|Pandoc>,
    <verbatim|![...](file.png){width=50%}>, or with the tag of
    <name|Html>, <verbatim|\<less\>img src="file.png" width="300"\<gtr\>>;
    a tag <verbatim|img> with other attributes, as a style, stays
    <name|Html>.

    <item*|Links>A link with a title,
    <verbatim|[text](address "title")>, is a <markup|hlink*>, which keeps
    the title.

    <item*|Lists>With bullets or numbers, nested, with several paragraphs in
    an item; the task lists (<verbatim|- [ ]> and <verbatim|- [x]>) get a
    box as their bullet.

    <item*|Code>Between lines of three backquotes or indented by four
    spaces; a tab which starts a line goes to the next column which is a
    multiple of four, as <name|CommonMark> says, and the columns of it
    which the indentation of the code or of a list takes are spaces.
    The language which follows the backquotes selects the
    environment, <markup|python-code>, <markup|scm-code>,
    <markup|cpp-code>..., and <markup|verbatim-code> otherwise.

    <item*|Tables>With the alignment of their columns and their first row
    in bold. A table too wide for the page gets its width and its cells are
    broken into lines.

    <item*|Quotations, rules, footnotes>A footnote does not hold other
    footnotes: a mark inside the text of a footnote stays as it is written.
    Quotations and lists inside each other are read down to a hundred
    levels, and are text beyond.

    <item*|<name|Html>>The tags of <name|Html> inside the text are read by
    the <hlink|converter for <name|Html>|../html/man-html.en.tm>. A block
    of <name|Html> ends at a blank line, but a comment, <verbatim|pre>,
    <verbatim|script>, <verbatim|style> and <verbatim|textarea> go on to
    their end.
  </description>

  <paragraph|What is written>

  A document is first expanded as for <name|Html>: the numbers of the
  sections, of the theorems and of the equations, the references and the
  citations are those which <TeXmacs> displays (update the document first,
  with <menu|Document|Update|All>). A paragraph is one line, and only the
  characters which <name|Markdown> would take for markup are escaped.

  <\description>
    <item*|Title>A title alone is the heading of the first level, and the
    sections are of the second one. With authors or a date, they are
    written in a <name|YAML> header, unless
    <menu|Edit|Preferences|Convert|Markdown> says otherwise; the abstract
    follows as a paragraph.

    <item*|Theorems, proofs, tables>Their name and number in bold, then
    their body; a table is followed by its caption.

    <item*|Mathematics>Formulas in <LaTeX>, with <verbatim|\\tag> for the
    numbered equations.

    <item*|Tables>The tables of <name|GitHub>, whose first row is the
    header. Their cells are single lines: the paragraphs of a cell are
    joined.

    <item*|Links>The title of an <markup|hlink*> is written with the link.

    <item*|Figures>A figure which is a single image is written as this
    image, with the caption as its description:
    <verbatim|![caption](file.png)>. The other figures are followed by
    their caption. An image with a width or a height is written as the tag
    <verbatim|img> of <name|Html>, since <name|Markdown> has no sizes: in
    pixels, or in percents for a part of the width of the paragraph.

    <item*|Images>The images which are files keep their name. Those which
    are inside the document are saved beside the file which is written:
    with <verbatim|name.md>, the files <verbatim|name-1.png>,
    <verbatim|name-2.png> and so on.

    <item*|What <name|Markdown> cannot say>An underlined text is
    emphasized and a key of the keyboard is code. A subscript or a
    superscript made of digits and signs is written with the characters
    which Unicode has for them (as in H<rsub|2>O or
    <verbatim|x><rsup|2>). The other ones and the marked
    text are written with the tags of <name|Html>, which most programs
    display (<menu|Edit|Preferences|Convert|Markdown> turns this off: they
    are then plain text). Colors, fonts, sizes and alignments are dropped,
    and so are the drawings made with <TeXmacs>.

    <item*|What does not come back>Outside the style
    <tmstyle|markdown>, a file which is written and read again
    is not always the document it was. The sizes of the
    images come back in pixels or as parts of the paragraph, whatever
    their unit was, and a height which is neither is dropped. The headings
    start again at the first level, so that the sections of a document
    with chapters are subsections afterwards. An address which stands in
    the text is a link when it is read, and braces which follow an image
    are read as its size.
  </description>

  To write something only into the <name|Markdown> file,
  <menu|Format|Specific|Html> is used for <name|Markdown> too.

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
