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
  single symbol, such as an arrow, is written as this character.

  <paragraph|What is read>

  <\description>
    <item*|Headings>The lines which start with <verbatim|#>, and those
    underlined with <verbatim|===> or <verbatim|--->, are sections,
    subsections and so on. When a text has a single heading of the first
    level, at its start, it is the title of the document and the other
    headings move up.

    <item*|Header>A <name|YAML> header between two lines <verbatim|---> at
    the start of the file gives the title, the authors
    (<verbatim|author>, one or a list), the date and the abstract.

    <item*|Text>Emphasized, strong and struck through text, code, links,
    images, line breaks, the entities of <name|Html>.

    <item*|Images and figures>An image alone in its paragraph, with a
    description, <verbatim|![description](file.png)>, is a figure whose
    caption is this description (or its title,
    <verbatim|![...](file.png "title")>, when it has one), as for
    <name|Pandoc>. An image inside a text is the image alone. An image may
    have a size, with the attributes of <name|Pandoc>,
    <verbatim|![...](file.png){width=50%}>, or with the tag of
    <name|Html>, <verbatim|\<less\>img src="file.png" width="300"\<gtr\>>.
    The title of a link, <verbatim|[text](address "title")>, is dropped.

    <item*|Lists>With bullets or numbers, nested, with several paragraphs in
    an item; the task lists (<verbatim|- [ ]> and <verbatim|- [x]>) get a
    box as their bullet.

    <item*|Code>Between lines of three backquotes or indented by four
    spaces. The language which follows the backquotes selects the
    environment, <markup|python-code>, <markup|scm-code>,
    <markup|cpp-code>..., and <markup|verbatim-code> otherwise.

    <item*|Tables>With the alignment of their columns and their first row
    in bold. A table too wide for the page gets its width and its cells are
    broken into lines.

    <item*|Quotations, rules, footnotes>

    <item*|<name|Html>>The tags of <name|Html> inside the text are read by
    the <hlink|converter for <name|Html>|../html/man-html.en.tm>.
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
