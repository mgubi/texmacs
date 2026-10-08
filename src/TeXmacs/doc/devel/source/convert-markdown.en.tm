<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <name|Markdown> converters>

  The converters between <TeXmacs> and <name|Markdown> are written in
  <scheme>, in <source-link|convert/markdown|TeXmacs/progs/convert/markdown>, on the model of those
  for <name|HTML>: a parser and a serializer between the text and a tree,
  and two converters between this tree and the trees of <TeXmacs>. The
  user-level description is in <hlink|converters for
  <name|Markdown>|../../main/convert/markdown/man-markdown.en.tm>.

  <section|The formats and the converters>

  <source-link|init-markdown.scm|TeXmacs/progs/convert/markdown/init-markdown.scm> defines the format
  <verbatim|markdown> (suffixes <verbatim|md>, <verbatim|markdown>,
  <verbatim|mkd>) and its converters; it is loaded by need
  (<scm|lazy-format> in <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>). The format has no
  <scm|:recognize> predicate: a text is never taken for <name|Markdown> by
  its contents.

  <\description>
    <item*|<verbatim|markdown-document>, <verbatim|markdown-snippet>
    <math|\<rightarrow\>> <verbatim|markdown-stree>><scm|parse-markdown-document>
    and <scm|parse-markdown-snippet>, in <source-link|markdownin.scm|TeXmacs/progs/convert/markdown/markdownin.scm>.

    <item*|<verbatim|markdown-stree> <math|\<rightarrow\>>
    <verbatim|texmacs-stree>><scm|markdown-\<gtr\>texmacs>, in
    <source-link|markdowntm.scm|TeXmacs/progs/convert/markdown/markdowntm.scm>.

    <item*|<verbatim|texmacs-stree> <math|\<rightarrow\>>
    <verbatim|markdown-stree>><scm|texmacs-\<gtr\>markdown>, in
    <source-link|tmmarkdown.scm|TeXmacs/progs/convert/markdown/tmmarkdown.scm>, with the options
    <verbatim|texmacs-\<gtr\>markdown:html> and
    <verbatim|texmacs-\<gtr\>markdown:front-matter> (both <verbatim|on> by
    default; they are preferences, shown in the tab <menu|Markdown> of the
    preferences of the converters).

    <item*|<verbatim|markdown-stree> <math|\<rightarrow\>>
    <verbatim|markdown-document>, <verbatim|markdown-snippet>><scm|serialize-markdown>,
    in <source-link|markdownout.scm|TeXmacs/progs/convert/markdown/markdownout.scm>.
  </description>

  <section|The <name|Markdown> tree>

  The tree between the two halves is <scm|(markdown <scm-arg|block> ...)>,
  or <scm|(!file (markdown ...))> for a whole document. Its strings are in
  UTF-8, as the text; the two converters to and from <TeXmacs> change the
  encoding.

  <\description>
    <item*|Blocks><scm|(meta (<scm-arg|key> "value") ...)> for the header,
    <scm|(h1 ...)> to <scm|(h6 ...)>, <scm|(p ...)>,
    <scm|(blockquote <scm-arg|block> ...)>, <scm|(hr)>,
    <scm|(ul (li <scm-arg|block> ...) ...)>, <scm|(ol ...)>,
    <scm|(pre "code")>, <scm|(table (tr (th ...) ...) (tr (td ...) ...) ...)>,
    <scm|(displaymath "latex")>, <scm|(html "text")> and
    <scm|(footnote-def "label" <scm-arg|block> ...)>.

    <item*|Text>Strings, <scm|(em ...)>, <scm|(strong ...)>,
    <scm|(del ...)>, <scm|(code "text")>, <scm|(a ...)>, <scm|(img)>,
    <scm|(br)>, <scm|(math "latex")>, <scm|(displaymath "latex")>,
    <scm|(html "text")> and <scm|(footnote "label")>.

    <item*|Attributes>As in sxml, <scm|(<scm-arg|tag> (@ (<scm-arg|name>
    "value") ...) ...)>: <verbatim|start> and <verbatim|loose> for the
    lists, <verbatim|checked> for the items of a task list,
    <verbatim|lang> for <verbatim|pre>, <verbatim|align> for the cells,
    <verbatim|href> and <verbatim|title> for the links, <verbatim|src>,
    <verbatim|alt>, <verbatim|title>, <verbatim|width> and
    <verbatim|height> for the images (the sizes as in <name|HTML>:
    <verbatim|300>, <verbatim|50%>, or with a unit).
  </description>

  A snippet of a single paragraph is the list of its nodes, without the
  <verbatim|p>, so that a piece of text which is pasted stays inside its
  line. In a session of <scheme>,
  <scm|(convert <scm-arg|s> "markdown-snippet" "markdown-stree")> shows
  the tree of a text.

  <section|The parser>

  <source-link|markdownin.scm|TeXmacs/progs/convert/markdown/markdownin.scm> follows the two passes of the
  specification of <name|CommonMark>.

  <\description>
    <item*|Blocks>The first pass works on the lines (<scm|md-blocks>): the
    <name|YAML> header (<scm|md-front-matter>), the fences of code, the
    indented code, the headings, the rules, the quotations (with their
    lazy lines), the lists (tight or loose, with the number of their first
    item, their tasks), the tables of <name|GitHub>, the formulas between
    <verbatim|$$> or <verbatim|\\[> and <verbatim|\\]>, the blocks of
    <name|HTML>, and the definitions of the links and of the footnotes,
    which are collected before the text is read.

    <item*|Text>The second pass reads the text of the paragraphs, the
    headings and the cells (<scm|md-inlines>): the escapes, the code,
    the formulas, the entities, the links and the images, the references
    to footnotes, the addresses, the tags of <name|HTML>, and then the
    emphasis, by the algorithm of the delimiter runs of <name|CommonMark>
    (<verbatim|~~> included).
  </description>

  Formulas are recognized before the emphasis, so that <verbatim|_> and
  <verbatim|*> in a formula are left alone. <scm|md-dollar-math> has the
  rules of <name|Pandoc> for the dollars, and <scm|md-literal-brackets?>
  tells the escaped brackets which other programs write
  (<verbatim|\\[2\\]>) from a formula.

  <section|The import>

  <scm|markdown-\<gtr\>texmacs> maps the blocks and the text to the tags of
  the standard styles: the sections, <markup|itemize> and
  <markup|enumerate>, <markup|quotation>, <markup|hrule>, <markup|block>
  for the tables, <markup|hlink>, <markup|image>, <markup|footnote>. A link
  with a title is <markup|hlink*> and an image with a text is inside an
  <markup|alt-text>, two macros of <verbatim|std-markup> which are typeset
  as their first argument and which the converters for <name|HTML> know
  too; the sizes of an image are its width and height (<scm|mdtm-size>). The
  language of a fence selects a tag <markup|<em|lang>-code> by the table
  <scm|mdtm-languages>. Formulas are given to the converter of <LaTeX>
  (<verbatim|latex-snippet>) and <name|HTML> to the one of <name|HTML>
  (<verbatim|html-snippet>). A document gets the style
  <verbatim|generic>; its title comes from the header or from a single
  heading of the first level (<scm|mdtm-body>).

  <section|The export>

  <subsection|Macro expansion>

  As for <name|HTML>, a document is expanded before it is converted, so
  that the converter sees the numbers and the references as text:
  <cpp|buffer_export> (<source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>) and the
  copy of a selection (<source-link|Edit/Replace/edit_select.cpp|src/Edit/Replace/edit_select.cpp>) call
  <cpp|edit_typeset_rep::exec_markdown> (<source-link|Edit/Editor/edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp>,
  <scm|markdown-expand> in <scheme>). It evaluates the tree in the
  environment of the document, patched by <scm|(tmmarkdown-env-patch)>
  (<source-link|tmmarkdown-expand.scm|TeXmacs/progs/convert/markdown/tmmarkdown-expand.scm>), where a macro
  <verbatim|tmmarkdown-<em|name>> of the style, if any, replaces
  <verbatim|<em|name>>.

  The patch redefines as themselves the macros which the converter wants
  to see: the renderers of the theorems and of the figures, the titles of
  the sections, the lists, the environments of code and of mathematics,
  <markup|footnote>, <markup|render-key>... The title, <markup|doc-data>,
  is given back quoted as <scm|(markdown-doc-data ...)> by the function
  <scm|tmmarkdown-doc-data>: the result of an <markup|extern> is
  evaluated, and a tag which is not a macro would be an error.

  <subsection|The converter>

  <scm|texmacs-\<gtr\>markdown> dispatches on the tag with the table
  <scm|tmmarkdown-methods%> (a <scm|logic-dispatcher>, as
  <scm|tmhtml-methods%>). A method takes the arguments of the tag and
  returns a list of nodes; a tag without a method is replaced by the
  conversion of its arguments. The converter also accepts trees which were
  not expanded, with the tags of the styles, for the calls from
  <scheme>.

  <\itemize>
    <item><scm|tmmd-blocks> gathers the text between the blocks into
    paragraphs.

    <item>A heading is <scm|(!h <scm-arg|level> ...)> and a formula on its
    own lines <scm|(!display "latex")> until the end: <scm|tmmd-finalize>
    then gives the first level in use the heading <verbatim|#> (or
    <verbatim|##> under a title) and appends the footnotes.

    <item>The formulas are converted by <scm|texmacs-\<gtr\>latex> with the
    option <verbatim|texmacs-\<gtr\>latex:mathjax>, the one of the
    <name|HTML> export.

    <item>The tables are those of <name|GitHub> (<scm|tmmd-table>); inside
    a heading or a cell, the blocks are flattened (<scm|tmmd-flat?>).

    <item><name|HTML> is the last resort. An underlined text is emphasized,
    a key is code, and a subscript or a superscript of digits and signs is
    written with the characters of Unicode (<scm|tmmd-script>). What
    remains (the other scripts, the marked text, an image with a size) is
    wrapped in tags of <name|HTML>, as <scm|(html "text")> nodes, with the
    option <verbatim|html>, and is plain text without it.

    <item>An image with a width or a height gets them as attributes
    (<scm|tmmd-size>: pixels or percents), and the serializer then writes
    the tag <verbatim|img> of <name|HTML>; without the option
    <verbatim|html> the sizes are dropped.

    <item>An image inside the document is saved beside the file which is
    written (<scm|tmmd-image>, from <scm|current-save-target>), for a
    whole document only.
  </itemize>

  <section|The serializer>

  <scm|serialize-markdown> writes a block as a list of lines, to which the
  blocks around it add their prefix: <verbatim|\<gtr\>> for a quotation,
  the indentation of an item. A paragraph is one line. The text is escaped
  by <scm|mdout-escape>, only where it would be read as markup, and the
  start of a line by <scm|mdout-escape-start>, where it would start
  another block. The code is written between more backquotes than it
  holds, and the columns of a table are padded to their width.

  <section|Tests>

  The suite <verbatim|markdown> of <scm|(run-all-tests)>, in
  <source-link|check/markdown-test.scm|TeXmacs/progs/check/markdown-test.scm>, checks the four steps and
  the round trips on small texts and trees:
  <scm|(run-regression-suite "markdown")>. A text which is written and
  read again gives the same tree, and the texts which the export writes
  come back unchanged from an import and an export.

  <section|Extending the converters>

  <\itemize>
    <item>For a new tag of <TeXmacs>, add a method to
    <scm|tmmarkdown-methods%> and, if the tag is a macro, its name to the
    list of <scm|tmmarkdown-env-patch>: otherwise the converter only sees
    its expansion.

    <item>For a new language of code, add it to <scm|mdtm-languages> and
    to <scm|tmmd-languages>.

    <item>A new construct of <name|Markdown> needs a node of the tree: the
    parser and the serializer, then the two converters, and a line in the
    description above and in the header of <source-link|markdownin.scm|TeXmacs/progs/convert/markdown/markdownin.scm>.
  </itemize>

  <section|Known limitations>

  <\itemize>
    <item>The title of an image is not kept by the import.

    <item>The drawings of <TeXmacs> are not exported.

    <item>The cells of a table are single lines.

    <item>The definition lists and the attributes of <name|Pandoc>
    (<verbatim|{#id .class}>) are read as text.
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
