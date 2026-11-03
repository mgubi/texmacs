<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|PDF export>

  <section|Introduction>

  <TeXmacs> writes <abbr|PDF> itself, without going through PostScript or
  <LaTeX>: the boxes of the typeset document are drawn on a renderer,
  <cpp|pdf_hummus_renderer_rep>, which turns the drawing commands into the
  content streams, fonts, images, links and outlines of a <abbr|PDF> file
  with the help of the <name|PDFHummus> library. When <TeXmacs> is built
  without this renderer, or when the user switches it off, the document is
  printed to PostScript and converted by <name|Ghostscript>.

  This chapter follows a document from the <menu|File|Export|Pdf> menu to
  the file: how it is typeset for printing, how the renderer is chosen and
  organized, how text and fonts are written so that the <abbr|PDF> can be
  searched and copied, and how links, bookmarks and metadata are produced.
  The images and attachments of <abbr|PDF> files are described in
  <hlink|images in PDF and PostScript output|images-pdf.en.tm>, and the
  renderer interface common to all output devices in <hlink|the renderer
  interface|renderer.en.tm>.

  <section|Overview>

  <\verbatim-code>
    export-buffer / print-to-file (scheme)

    \ \ -\<gtr\> edit_main_rep::print_doc (name, conform, first, last)

    \ \ \ \ \ \ \ typeset_as_document with page-medium paper, printing dpi

    \ \ \ \ \ \ \ printer (name, dpi, pages, ...) \ \ \ \ \ -\<gtr\> pdf_hummus_renderer

    \ \ \ \ \ \ \ set_metadata (title, author, subject)

    \ \ \ \ \ \ \ for each page: box-\<gtr\>redraw (ren); ren-\<gtr\>next_page ()

    \ \ \ \ \ \ \ \ \ draw (glyphs) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ -\<gtr\> native font or Type 3 font

    \ \ \ \ \ \ \ \ \ line, fill, image, ... \ \ \ \ \ \ \ \ -\<gtr\> content stream, XObjects

    \ \ \ \ \ \ \ \ \ href, anchor, toc_entry \ \ -\<gtr\> links, destinations, outlines

    \ \ \ \ \ \ \ tm_delete (ren) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ -\<gtr\> flush everything, EndPDF
  </verbatim-code>

  The renderer writes the pages as it goes and keeps everything else
  (fonts, images, patterns, transparency states, destinations, outlines,
  annotations, metadata) in tables, which the destructor writes at the end.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Plugins/Pdf/pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp>,
    <source-link|pdf_hummus_renderer.hpp|src/Plugins/Pdf/pdf_hummus_renderer.hpp>>The
    renderer: pages, graphics state, fonts and text, images and patterns,
    links, outlines and metadata.

    <item*|<source-link|Edit/Editor/edit_main.cpp|src/Edit/Editor/edit_main.cpp>>Printing:
    <cpp|print_doc>, <cpp|use_pdf>, <cpp|use_ps>, the metadata and the
    optional check of the result.

    <item*|<source-link|Plugins/Ghostscript/gs_utilities.cpp|src/Plugins/Ghostscript/gs_utilities.cpp>>The
    conversions between PostScript and <abbr|PDF>, the check of exported
    files and the <abbr|PDF> version.

    <item*|<source-link|texmacs/texmacs/tm-print.scm|TeXmacs/progs/texmacs/texmacs/tm-print.scm>,
    <source-link|texmacs/menus/preferences-widgets.scm|TeXmacs/progs/texmacs/menus/preferences-widgets.scm>>The
    printing preferences and their dialog.

    <item*|<source-link|Plugins/Pdf/PDFWriter|src/Plugins/Pdf/PDFWriter/PDFWriter.h>,
    <source-link|Plugins/Pdf/LibAesgm|src/Plugins/Pdf/LibAesgm/aes.h>>The
    vendored <name|PDFHummus> library and the <abbr|AES> code it uses for
    encryption.

    <item*|<source-link|fonts/pdf-font-issues.scm|TeXmacs/fonts/pdf-font-issues.scm>>Font
    files which <name|PDFHummus> must not embed.
  </description-paragraphs>

  <\traverse>
    <branch|From the menu to the file|pdf-export-path.en.tm>

    <branch|Fonts and text|pdf-export-text.en.tm>

    <branch|Links, bookmarks and metadata|pdf-export-links.en.tm>

    <branch|Pitfalls and known problems|pdf-export-pitfalls.en.tm>
  </traverse>

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
