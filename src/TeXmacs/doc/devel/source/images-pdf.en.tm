<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Images in <abbr|PDF> and PostScript output; <abbr|PDF>
  attachments>

  How the printers are created and how pages are drawn is explained in
  <hlink|renderers at work|renderer-pipeline.en.tm>, and the general
  structure of the <abbr|PDF> renderer in <hlink|implementations, new
  renderers and pitfalls|renderer-backends.en.tm> and in <hlink|PDF
  export|pdf-export.en.tm>. This page describes how
  images end up in the output files, and how <TeXmacs> documents are
  embedded in, and recovered from, <abbr|PDF> files.

  <section|Images in the <abbr|PDF> renderer>

  All image related state of <cpp|pdf_hummus_renderer_rep>
  (<source-link|Plugins/Pdf/pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp>) lives in four tables:

  <\description>
    <item*|<cpp|image_pool>>Image files, keyed by <abbr|URL>, each with a
    <cpp|pdf_image>, that is, the file, its size in points and the object
    identifier which the image will have in the <abbr|PDF> file.

    <item*|<cpp|pattern_image_pool>, <cpp|pattern_pool>>Images used as
    background patterns and the <abbr|PDF> patterns built on them.

    <item*|<cpp|picture_cache>>Temporary <name|PNG> files for pictures,
    keyed by the unique identifier of the picture.

    <item*|<cpp|temp_images>>The temporary files to remove when the
    renderer is destroyed.
  </description>

  Drawing an image only writes a reference to an object which does not
  exist yet; the objects themselves are written when the document is
  finished (<cpp|flush_images> and <cpp|flush_patterns>, called from the
  destructor). Each image file is therefore embedded only once, however
  often it appears.

  <paragraph|Scalable images.><cpp|draw_scalable> embeds image files
  directly. If the scalable has an effect, it falls back on the default
  implementation, which rasterizes the image at the current resolution and
  calls <cpp|draw_picture>. Otherwise it calls the private method
  <cpp|image (u, w, h, x, y, alpha)>, which allocates the <cpp|pdf_image>
  if needed and writes, in the page content, a transformation which scales
  the object from its natural size to the requested size, followed by a
  <verbatim|Do> operator for the object, with the requested opacity.

  <paragraph|Pictures.><cpp|draw_picture> saves the picture as a
  temporary <name|PNG> file (with <name|Qt>; other builds report an error),
  caches the file name by the unique identifier of the picture, and then
  calls <cpp|image> like for a file. Pictures come from effects and from
  shadows, which the printer rasterizes at a fixed resolution
  (<cpp|shadow> sets the zoom factor to <cpp|5.0 * PICTURE_ZOOM>).

  <paragraph|Writing the image objects.><cpp|pdf_image_rep::flush>
  turns the file into a <abbr|PDF> form object with the allocated
  identifier:

  <\enumerate>
    <item><abbr|PDF> files are merged directly: the first page of the file
    is copied into a form object with <name|PDFHummus>
    (<cpp|CreatePDFCopyingContext>, <cpp|MergePDFPageToFormXObject>), using
    the crop box and rotation of the page (<cpp|pdf_image_info>). If the
    preference <verbatim|texmacs-\<gtr\>pdf:distill inclusion> (<menu|Distill
    encapsulated Pdf files>) is set, the file is first re-distilled with
    <name|Ghostscript> so that all its fonts are embedded
    (<cpp|gs_PDF_EmbedAllFonts>). A warning is issued when the included file
    has a higher <abbr|PDF> version than the one selected for the output.

    <item><name|JPEG> files are embedded as they are
    (<cpp|flush_jpg>, <cpp|CreateImageXObjectFromJPGFile>), and
    <name|PNG> files through <name|PDFHummus>' <name|PNG> reader
    (<cpp|flush_png>).

    <item>Any other format is converted to a temporary <abbr|PDF> file with
    <cpp|image_to_pdf> at a maximum of 300 dots per inch, and merged like a
    <abbr|PDF> file. Vector formats which can be converted to <abbr|PDF>
    stay vectorial.
  </enumerate>

  If the size of the merged page differs from the size recorded when the
  image was registered, the transformation is rescaled.

  <paragraph|Patterns.><cpp|register_pattern_image> loads a background
  image at the size of one tile with <cpp|get_image> (the <name|Qt> image
  loader, so effects of patterns are applied), saves it as a temporary
  <name|PNG> file and creates a tiling pattern which draws it.
  <cpp|flush_for_pattern> writes the image as a raw <name|RGB> image with a
  separate soft mask for the alpha channel (<cpp|qt_image_data>).

  <paragraph|Sizes.><cpp|hummus_pdf_image_size (image, w, h)> parses the
  first page of a <abbr|PDF> file with <name|PDFHummus> and returns the size
  of its crop box, in points, with width and height exchanged for pages
  rotated by 90 or 270 degrees. It is the preferred way of measuring
  <abbr|PDF> images (see <hlink|image files|images-files.en.tm>).

  <section|Images in the PostScript renderer>

  <cpp|printer_rep> (<source-link|Graphics/Renderer/printer.cpp|src/Graphics/Renderer/printer.cpp>) includes
  images as encapsulated PostScript, between <verbatim|@beginspecial> and
  <verbatim|@endspecial> in the style of <verbatim|dvips>:

  <\itemize>
    <item><cpp|draw_scalable> loads the image as PostScript code with
    <cpp|ps_load>, which converts other formats through <cpp|image_to_eps>,
    takes its bounding box (from the size cache for <abbr|EPS> files, from
    the generated code otherwise), and inserts it scaled to the size of the
    box;

    <item><cpp|draw_picture> encodes the picture with
    <cpp|picture_as_eps> and inserts it in the same way.
  </itemize>

  <section|The <name|Cairo> renderer>

  <verbatim|Plugins/Cairo/> contains a <cpp|cairo_renderer_rep>, which
  loads the <name|Cairo> library dynamically (<source-link|tm_cairo.cpp|src/Plugins/Cairo/tm_cairo.cpp>) and
  draws <name|PNG> images directly, converting PostScript and <abbr|PDF>
  images with <verbatim|convert>. It is only compiled with
  <cpp|USE_CAIRO>, which the <name|CMake> build never sets, and has not
  followed the evolution of the renderer interface (see <hlink|older
  renderers|renderer-backends.en.tm>).

  <section|<abbr|PDF> attachments>

  A <abbr|PDF> file exported with <menu|File|Export|Pdf with embedded
  document> contains the <TeXmacs> source of the document, and the
  external files it uses, as embedded files. <menu|File|Import|Pdf with
  embedded document> recovers them. The <c++> side is in
  <source-link|Plugins/Pdf/pdf_hummus_make_attachment.cpp|src/Plugins/Pdf/pdf_hummus_make_attachment.cpp> and
  <source-link|pdf_hummus_extract_attachment.cpp|src/Plugins/Pdf/pdf_hummus_extract_attachment.cpp>; without
  <cpp|PDF_RENDERER> all functions are stubs which fail.

  <\explain>
    <cpp|bool pdf_hummus_make_attachments (url pdf, array\<less\>url\<gtr\>
    files, url out)><explain-synopsis|embed files>
  <|explain>
    Open <cpp|pdf> for modification with <name|PDFHummus>
    (<cpp|PDFWriter::ModifyPDF>), add each file as an embedded file stream
    named after the last component of its path, register them in the
    <verbatim|/Names/EmbeddedFiles> tree of the catalog
    (<cpp|PDFAttachmentWriter::OnCatalogWrite>), and write the result to
    <cpp|out>. When <cpp|out> is <cpp|pdf>, <name|PDFHummus> appends an
    incremental update to the file. All files must exist; otherwise
    nothing is done and the function returns <cpp|false>. Exported as
    <scm|pdf-make-attachments>.
  </explain>

  <\explain>
    <cpp|bool extract_attachments_from_pdf (url pdf, list\<less\>url\<gtr\>&
    names)><explain-synopsis|extract files>
  <|explain>
    Read the embedded files of <cpp|pdf> and write each of them, under its
    embedded name, <em|in the directory of the <abbr|PDF> file>
    (<cpp|relative (pdf, name)>), appending the paths to <cpp|names>.
    <cpp|scm_extract_attachments> (exported as
    <scm|extract-attachments>) does the same without returning the names,
    and <cpp|get_main_tm> (<scm|pdf-get-attached-main-tm>) extracts again
    and returns the first file, which by convention is the main document.
  </explain>

  Two helpers operate on document trees: <cpp|get_linked_file_paths (t,
  path)> (<scm|pdf-get-linked-file-paths>) collects the files referenced by
  <markup|image> and <markup|include> tags and the style files which are
  not part of <TeXmacs>; <cpp|replace_with_relative_path (t, path)>
  (<scm|pdf-replace-linked-path>) rewrites these references as bare file
  names, so that they point to the extracted copies.

  On the <scheme> side, export is done by
  <scm|wrapped-print-to-pdf-embeded-with-tm> and
  <scm|attach-doc-to-exported-pdf> (<source-link|texmacs/texmacs/tm-print.scm|TeXmacs/progs/texmacs/texmacs/tm-print.scm>).
  The latter prints the document, saves a copy of it (with its attachments
  and auxiliary data) in a temporary buffer named after the <abbr|PDF>
  file, and calls <scm|pdf-make-attachments> with this copy followed by
  the linked files. Import is done by
  <scm|wrapped-import-pdf-embeded-with-tm>
  (<source-link|texmacs/menus/file-menu.scm|TeXmacs/progs/texmacs/menus/file-menu.scm>), which extracts the
  attachments, rewrites the paths of the main document, saves it as
  <verbatim|extracted.tm> in a temporary directory and opens it. The
  export of a selection as <abbr|PDF> (<scm|embbed-tm-selection-in-pdf> in
  <source-link|convert/images/tmimage.scm|TeXmacs/progs/convert/images/tmimage.scm>) attaches the selection in the same
  way.

  <section|Pitfalls>

  <\itemize>
    <item><cpp|extract_attachments_from_pdf>
    (<verbatim|Plugins/Pdf/pdf_hummus_extract_attachment.cpp:131-135>)
    writes every embedded file next to the <abbr|PDF> file, under the name
    stored in the <abbr|PDF>. Importing <verbatim|paper.pdf> which was
    exported from <verbatim|paper.tm> in the same directory therefore
    <em|overwrites> <verbatim|paper.tm> (and the linked images) with the
    embedded copies, without asking; the temporary directory created by
    <scm|wrapped-import-pdf-embeded-with-tm> is only used for the final
    <verbatim|extracted.tm>. The embedded names are not sanitized either:
    a name containing <verbatim|..> or an absolute path writes outside
    that directory, which is a security problem for <abbr|PDF> files of
    unknown origin.

    <item><cpp|get_main_tm> (<verbatim|pdf_hummus_extract_attachment.cpp:385-390>)
    extracts all attachments a second time and returns
    <cpp|attachments_paths[0]> without checking that the extraction
    succeeded; on a <abbr|PDF> without attachments the list is empty and
    the access fails.

    <item><cpp|get_linked_file_paths> also returns referenced files which do
    not exist, and <cpp|pdf_hummus_make_attachments> refuses to do
    anything as soon as one of its files is not a regular file. A single
    missing image therefore prevents the document from being attached at
    all (the user only sees \PFail to attach tm to pdf\Q).

    <item>In <cpp|pdf_image_rep::flush> (<verbatim|pdf_hummus_renderer.cpp:1765-1777>),
    the fallback to a <name|PNG> file is never reached, because
    <cpp|image_to_pdf> copies <verbatim|unknown.pdf> to its destination
    when it fails: an image which could be displayed as a bitmap but not
    converted to <abbr|PDF> is printed as the \Punknown\Q placeholder. When
    the fallback is taken, its temporary <name|PNG> file is not removed.

    <item>The PostScript printer ignores effects:
    <cpp|printer_rep::draw_scalable> (<verbatim|Graphics/Renderer/printer.cpp:929-946>)
    only checks the kind of the scalable and embeds the original file, so
    images with effects print without them, unlike in the <abbr|PDF>
    renderer. It also calls <cpp|ps_load>, and thus a conversion, each time
    an image is drawn, with no cache.

    <item>Pictures and pattern images can only be exported with <name|Qt>,
    because the temporary <name|PNG> files are written with <cpp|QImage>.
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
