<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|From the menu to the file>

  <section|Entry points>

  There are two ways from <scheme> to <abbr|PDF>:

  <\itemize>
    <item><menu|File|Export|Pdf> and <menu|File|Print|Print to file> call
    <scm|wrapped-print-to-file> (<source-link|tm-print.scm:94|TeXmacs/progs/texmacs/texmacs/tm-print.scm:94>).
    For a document with <markup|screens> (slides), it first copies the
    buffer and expands the slides into pages (<scm|dynamic-make-slides>);
    then it calls <scm|print-to-file>, the glue of
    <cpp|edit_main_rep::print_to_file>. <scm|wrapped-print-to-pdf-embeded-with-tm>
    does the same and attaches the <TeXmacs> source to the result (see
    <hlink|PDF attachments|images-pdf.en.tm>).

    <item><scm|export-buffer> with a <verbatim|.pdf> or <verbatim|.ps>
    target, which the command line option <verbatim|-c> uses, ends in
    <cpp|buffer_export> (<source-link|new_buffer.cpp:551|src/Texmacs/Data/new_buffer.cpp:551>),
    which calls <cpp|print_to_file> on the most recent view of the buffer
    directly. This path does not expand slides.
  </itemize>

  <section|Typesetting for print>

  <cpp|print_doc> (<source-link|edit_main.cpp:241|src/Edit/Editor/edit_main.cpp:241>)
  typesets the document anew, independently of the screen:

  <\enumerate>
    <item>With <name|Ghostscript> support (<cpp|USE_GS>), it decides
    whether the native renderers are used: <cpp|use_pdf ()> is the
    preference <verbatim|native pdf> when <TeXmacs> was built with
    <cpp|PDF_RENDERER>, and false otherwise. If a <abbr|PDF> file is wanted
    and <cpp|use_pdf ()> is false, the document is printed to a temporary
    PostScript file which <cpp|gs_to_pdf> converts at the end; the same
    happens in the other direction for PostScript.

    <item>It sets the printing environment: <verbatim|dpi> to the printing
    resolution (600 by default), headers and footers on, no screen margins
    and no page border, and <verbatim|page-medium> to <verbatim|paper>
    (except for <em|conform> printing, used by <cpp|export_ps>
    (<scm|export-postscript>), which keeps the paging of the screen and
    gives each page the size of its box).

    <item><cpp|typeset_as_document> produces the boxes of all the pages; the
    page size and orientation are read back from the environment
    (<cpp|page_real_width>, <cpp|page_real_height>, <cpp|page_landscape>).

    <item><cpp|printer (name, dpi, pages, ...)>
    (<source-link|printer.cpp:1085|src/Graphics/Renderer/printer.cpp:1085>)
    returns the <abbr|PDF> renderer when <cpp|use_pdf ()> holds and the
    file has the suffix <verbatim|pdf> (or PostScript is not produced
    natively), and the PostScript renderer otherwise.

    <item>The title, author and subject are passed with
    <cpp|set_metadata>. Each comes from the <verbatim|global-title>, ...
    environment variable, from the <markup|doc-data> of the document
    (<cpp|search_metadata>), or, failing that, from the file name for the
    title and from <verbatim|finger `whoami`> for the author
    (<source-link|edit_main.cpp:180|src/Edit/Editor/edit_main.cpp:180>).

    <item>Each page box is redrawn on the renderer, after clearing the page
    with the background color when it is not white, and
    <cpp|next_page> separates the pages. Deleting the renderer finishes the
    file.

    <item>If the preference <verbatim|texmacs-\<gtr\>pdf:check> is on,
    <cpp|gs_check> runs <name|Ghostscript> on the result and reports
    errors.
  </enumerate>

  Since the document is typeset again, the positions on paper can differ
  from those on the screen, and the links of the document are found in two
  ways at drawing time (see <hlink|links|pdf-export-links.en.tm>).

  <section|The renderer>

  <cpp|pdf_hummus_renderer_rep> (<source-link|pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp>)
  derives from <cpp|renderer_rep>; it is a printer (<cpp|is_printer> is
  true), so boxes draw their links and table of contents entries on it.

  <paragraph|Construction.>The constructor
  (<source-link|pdf_hummus_renderer.cpp:287|src/Plugins/Pdf/pdf_hummus_renderer.cpp:287>)
  computes the page size in points from the paper size in centimeters,
  chooses the <abbr|PDF> version, and starts the file with
  <cpp|PDFWriter::StartPDF> with compressed streams. The version is 1.4
  unless the preference <verbatim|texmacs-\<gtr\>pdf:version> asks for 1.5,
  1.6 or 1.7; this preference is only read in builds with
  <name|Ghostscript> support, since <cpp|pdf_version> lives in
  <source-link|gs_utilities.cpp:195|src/Plugins/Ghostscript/gs_utilities.cpp:195>.
  It registers a <cpp|DestinationsWriter>, a <name|PDFHummus> extender
  which adds the <verbatim|/Dests> and <verbatim|/Outlines> entries to the
  catalog when it is written, and an initial graphics state.

  <paragraph|Pages.><cpp|begin_page> creates a <cpp|PDFPage> with the media
  box and a content context, saves the graphics state, scales from the
  printing resolution to 72 points per inch (<cpp|cm>), and moves the origin
  to the top left corner, so that the renderer can use the coordinates of
  <TeXmacs> (in units of <cpp|pixel> at the printing resolution, <var|y>
  going down). <cpp|end_page> closes the text object and the clippings,
  writes the page with <cpp|WritePageAndRelease> and records its object
  number in <cpp|page_id>, which destinations and outlines refer to.

  <paragraph|Graphics state.>Colors, line widths, clippings and
  transformations are written as <abbr|PDF> operators when they change
  (<cpp|fg>, <cpp|bg>, <cpp|lw> cache the current values). Transparency
  is implemented with one <verbatim|ExtGState> per alpha value
  (<cpp|alpha_id>, <cpp|select_alpha>), which is why the minimum version is
  1.4. Images, patterns and pictures are described in <hlink|images in PDF
  output|images-pdf.en.tm>.

  <paragraph|Destruction.>The destructor
  (<source-link|pdf_hummus_renderer.cpp:367|src/Plugins/Pdf/pdf_hummus_renderer.cpp:367>)
  ends the last page and writes, in this order, the images, the patterns,
  the bitmap glyphs, the destinations, the outlines, the Type 3 fonts, the
  metadata, the transparency states and the link annotations; then
  <cpp|EndPDF> writes the fonts used through <name|PDFHummus>, the
  catalog, the cross reference table and the trailer, and the temporary
  image files are removed.

  <section|The PDFHummus library>

  <name|PDFHummus> (<verbatim|PDF-Writer> by Gal Kahana) is vendored in
  <source-link|Plugins/Pdf/PDFWriter|src/Plugins/Pdf/PDFWriter/PDFWriter.h>, with the
  <abbr|AES> implementation it needs for encrypted files in
  <source-link|LibAesgm|src/Plugins/Pdf/LibAesgm/aes.h>. <TeXmacs> uses it
  to write objects, content streams, fonts and embedded images, and to read
  <abbr|PDF> files (sizes of images, merging of included <abbr|PDF> pages,
  attachments). It relies on <name|FreeType> for the fonts, which is why
  the renderer is only built with <name|FreeType> 2.4.8 or later.

  The renderer is enabled by <verbatim|--enable-pdf-renderer> in the
  autotools build (<source-link|misc/m4/hummus.m4|misc/m4/hummus.m4>), and
  always in the <name|CMake> build, both with <cpp|PDFHUMMUS_NO_TIFF> and
  <cpp|PDFHUMMUS_NO_DCT> (no <name|TIFF> images, no <name|JPEG> decoding;
  <name|JPEG> files are embedded as they are).

  A few local changes are marked in the sources: <cpp|UsedFontsRepository::GetFontForFile>
  returns <cpp|NULL> for a corrupted font file instead of stopping the
  output, so that <TeXmacs> can fall back to a Type 3 font; a fix in
  <cpp|CharStringType2Flattener::Type2Cntrmask> (2021); and the font
  descriptor flags of <verbatim|HelveticaNeue.0.ttf> on <name|macOS>
  (<cpp|FontDescriptorWriter::CalculateFlags>, savannah bug #66761). When
  the library is updated, these changes have to be carried over.

  <section|Preferences>

  The section <em|TeXmacs -\<gtr\> Pdf/Postscript> of the conversion
  preferences (<scm|pdf-preferences-widget> in <source-link|preferences-widgets.scm|TeXmacs/progs/texmacs/menus/preferences-widgets.scm>,
  and the corresponding submenu of <source-link|preferences-menu.scm|TeXmacs/progs/texmacs/menus/preferences-menu.scm>,
  shown when both native <abbr|PDF> and <name|Ghostscript> are supported)
  sets:

  <\description-paragraphs>
    <item*|<verbatim|native pdf>, <verbatim|native postscript>>Use the
    native renderers (default <verbatim|on>); otherwise convert with
    <name|Ghostscript>.

    <item*|<verbatim|texmacs-\<gtr\>pdf:version>>The <abbr|PDF> version:
    <verbatim|default> (1.4), 1.5, 1.6 or 1.7.

    <item*|<verbatim|texmacs-\<gtr\>pdf:expand slides>>Export the slides of
    a presentation as separate pages.

    <item*|<verbatim|texmacs-\<gtr\>pdf:distill inclusion>>Run included
    <abbr|PDF> images through <name|Ghostscript> first.

    <item*|<verbatim|texmacs-\<gtr\>pdf:check>>Check the exported file with
    <name|Ghostscript>.
  </description-paragraphs>

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
