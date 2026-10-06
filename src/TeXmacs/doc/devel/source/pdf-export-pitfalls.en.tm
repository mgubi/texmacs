<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|PDF export: pitfalls and known problems>

  The statements marked <em|(checked)> were verified on 2026-10-06 by
  exporting small documents with a build which has the native renderer
  (<verbatim|PDF_RENDERER>) and inspecting the result with
  <verbatim|mutool>. Most problems are filed as issue #302 of
  <verbatim|mgubi/texmacs>.

  <\itemize>
    <item><em|Check which renderer wrote a file.> A build without
    <verbatim|PDF_RENDERER>, or with the preference <verbatim|native pdf>
    off, still exports <abbr|PDF>, through PostScript and
    <name|Ghostscript>: links and outlines then come from the
    <verbatim|pdfmark>s of the PostScript renderer, the fonts and the text
    layer are those of <name|Ghostscript>, and the file has
    <verbatim|/Producer (GPL Ghostscript ...)> and the file name as its
    title.
    <verbatim|mutool info <em|file>.pdf> shows the producer. The
    <name|CMake> build always enables the renderer; in the autotools build
    it needs <verbatim|--enable-pdf-renderer> and <name|FreeType>.

    <item><em|<verbatim|texmacs -c> loses the <markup|hlink>s> (checked).
    The conversion from the command line exports the buffer right after
    loading it, before it has been typeset on screen, so
    <cpp|display_links> finds no links for accessible loci, and only
    references and other generated loci become links. Exporting a buffer
    which is open in a window works.

    <item><em|Math letters are lost in the text layer> (checked). Glyphs of
    the <TeX> math fonts are mapped to their position in the font instead
    of a Unicode character, so <math|x<rsup|2>+\<alpha\>> is copied as
    <verbatim|x2 + .> and <math|\<alpha\>> in a bold heading as
    <verbatim|a>. The text fonts are fixed by pull request #157 in
    <verbatim|wip_fixes>.

    <item><em|Backslashes in bookmarks> (checked). Section titles are
    escaped for a literal string and then written as a hex string, so
    <verbatim|f(x)> appears as <verbatim|f\\(x\\)> in the outline.
    Non-<name|ASCII> titles are right.

    <item><em|Parentheses in <abbr|URL>s> (checked). The destination of a
    link is written into <verbatim|/URI (...)> without escaping: an
    unbalanced <verbatim|)> makes the annotation unreadable.

    <item><em|The <abbr|PDF> version needs <name|Ghostscript>.> Without
    <verbatim|USE_GS>, the preference <verbatim|texmacs-\<gtr\>pdf:version>
    is ignored and the version is 1.4, so the <verbatim|/ActualText> spans
    of ligatures (1.5) are never written. With it, the default is 1.4 too.

    <item><em|One annotation per word.> Links spanning several words, and
    links which wrap, give one annotation per box; viewers show them as
    separate links (see the <verbatim|FIXME> in
    <source-link|boxes.cpp:833|src/Typeset/Boxes/Basic/boxes.cpp:833>).

    <item><em|Relative links are not resolved.> A link to
    <verbatim|other.tm> becomes <verbatim|/URI (other.tm)>, which viewers
    resolve against the location of the <abbr|PDF> file, if at all.

    <item><em|The author may be the user's name.> When the document has no
    author, <cpp|get_metadata> runs <verbatim|finger `whoami`> and puts the
    full name of the user in the <abbr|PDF> file.

    <item><em|Empty bitmap glyphs> (issue #146, item 3). A Type 3 glyph
    without ink is written as an inline image of width 0, which is not
    valid <abbr|PDF>. This is fixed on the <verbatim|wip_opentype> branch.

    <item><em|Dead code.> <cpp|draw_bitmap_glyph> and the table
    <cpp|pdf_glyphs> are never filled: nothing calls
    <cpp|draw_bitmap_glyph>, and bitmap glyphs go through Type 3 fonts.
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
