<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Images, pictures and <abbr|PDF> output>

  <section|Introduction>

  <TeXmacs> documents may contain images in many formats: bitmaps
  (<name|PNG>, <name|JPEG>, <name|GIF>, ...), vector images (<abbr|PDF>,
  PostScript, <abbr|EPS>, <abbr|SVG>) and formats which can only be read by
  external programs (<name|Xfig>, <name|Geogebra>, ...). An image is
  inserted with the <markup|image> primitive, it may also serve as a
  background pattern, and graphical <em|effects> (blur, shadows, color
  transformations, ...) can be applied to images and to typeset content.
  The same images must be shown on the screen, at any zoom factor, and
  written into <abbr|PDF> and PostScript files, preferably without
  rasterizing vector images.

  This chapter describes the machinery which makes this possible:

  <\itemize>
    <item>the classes for <em|pictures> (arrays of pixels), <em|scalable
    images> and <em|effects>, and the caches built on them;

    <item>the central module <source-link|System/Files/image_files.cpp|src/System/Files/image_files.cpp>, which
    determines the size of image files and converts them between formats,
    using <name|Qt>, <name|Ghostscript>, <name|resvg>, <name|ImageMagick> and
    the converters declared in <scheme>;

    <item>the <markup|image> primitive, embedded images, background
    patterns and the export of selections as images;

    <item>the inclusion of images in <abbr|PDF> and PostScript output, and
    the embedding of <TeXmacs> sources as <abbr|PDF> attachments.
  </itemize>

  The renderer interface itself (<cpp|draw_picture>, <cpp|draw_scalable>,
  <cpp|shadow>, picture renderers) and the general organization of the
  <abbr|PDF> renderer are described in <hlink|the renderer
  interface|renderer.en.tm>, in particular in <hlink|the renderer
  API|renderer-api.en.tm> and <hlink|implementations, new renderers and
  pitfalls|renderer-backends.en.tm>; they are not repeated here. The user
  level description of the <markup|image> primitive is in <hlink|graphics
  primitives|../format/regular/prim-graphics.en.tm>, and the typesetting of
  <markup|gr-effect> in <hlink|the graphics editor: typesetting
  pictures|graphics-editor-typeset.en.tm>.

  All file names below are relative to <source-link|src/src/|src> unless stated
  otherwise.

  <section|Overview>

  The following diagram shows what happens to an image from the document to
  the output devices. Each arrow is a function call; the names on the right
  are the main classes or caches involved.

  <\verbatim-code>
    \<less\>image\|photo.png\|5cm\|\|\|\<gtr\>

    \ \ \|

    \ \ concater_rep::typeset_image \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ resolves the url

    \ \ \ \ \|-- image_size \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ size in pt, cache img_box

    \ \ \ \ \|-- image_box --\<gtr\> load_scalable_image \ \ \ \ \ \ \ scalable_image_rep

    \ \ \|

    \ \ image_box_rep::display --\<gtr\> renderer::draw_scalable

    \ \ \ \ \|

    \ \ \ \ \|-- screen: scalable_image_rep::draw

    \ \ \ \ \| \ \ \ \ \ \ --\<gtr\> cached_load_picture \ \ \ \ \ \ \ \ \ \ \ \ picture_cache

    \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ --\<gtr\> load_picture (Qt) \ \ \ \ \ \ qt_pic_cache

    \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ --\<gtr\> QImage, resvg, or image_to_png

    \ \ \ \ \| \ \ \ \ \ \ --\<gtr\> renderer::draw_picture

    \ \ \ \ \|

    \ \ \ \ \|-- PDF: \ \ \ pdf_hummus_renderer_rep::image \ \ \ \ \ \ image_pool

    \ \ \ \ \| \ \ \ \ \ \ --\<gtr\> pdf_image_rep::flush \ \ \ \ \ \ \ \ \ \ \ \ embed pdf/jpg/png,

    \ \ \ \ \| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ or image_to_pdf

    \ \ \ \ \|

    \ \ \ \ \|-- PS: \ \ \ \ printer_rep::draw_scalable

    \ \ \ \ \ \ \ \ \ \ \ \ --\<gtr\> ps_load --\<gtr\> image_to_psdoc --\<gtr\> image_to_eps
  </verbatim-code>

  Three ideas run through the whole subsystem:

  <\description>
    <item*|Sizes in points>The <em|original size> of an image file is
    always expressed in points (1/72 inch) and is cached per file in a
    table of <source-link|image_files.cpp|src/System/Files/image_files.cpp>, because the typesetter asks for it
    very often.

    <item*|One place for conversions>All conversions go through the
    functions of <source-link|image_files.cpp|src/System/Files/image_files.cpp>, which try the available tools
    in a fixed order of preference and fall back on placeholder images
    (<verbatim|$TEXMACS_PATH/misc/pixmaps/unknown.*>) when everything fails.
    The comment at the top of that file asks other modules not to call
    external tools directly.

    <item*|Rasterize late>An image stays a <cpp|scalable> until a renderer
    draws it. Screen renderers rasterize it at the current resolution;
    printers embed the original file when they can, and rasterize only for
    effects and for formats they cannot embed.
  </description>

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Pictures/picture.hpp|src/Graphics/Pictures/picture.hpp>,
    <source-link|picture.cpp|src/Graphics/Pictures/picture.cpp>>The abstract class <cpp|picture_rep>, the
    composition modes, the list of picture operations, the cache of loaded
    pictures (<cpp|cached_load_picture> and friends), <cpp|load_xpm> for
    builds without <name|Qt>, and <cpp|picture_as_eps>.

    <item*|<source-link|Graphics/Pictures/raster.hpp|src/Graphics/Pictures/raster.hpp>,
    <source-link|raster_operators.hpp|src/Graphics/Pictures/raster_operators.hpp>, <source-link|raster_picture.hpp|src/Graphics/Pictures/raster_picture.hpp>,
    <source-link|raster_picture.cpp|src/Graphics/Pictures/raster_picture.cpp>, <source-link|raster_random.cpp|src/Graphics/Pictures/raster_random.cpp>>Portable
    pixel arrays (<cpp|raster\<less\>C\<gtr\>>), the pixel operators used to
    compose them, the picture class which wraps them, and the
    implementation of all picture operations (including random noise).

    <item*|<source-link|Graphics/Pictures/effect.hpp|src/Graphics/Pictures/effect.hpp>,
    <source-link|effect.cpp|src/Graphics/Pictures/effect.cpp>>Effects and <cpp|build_effect>, the parser of
    effect trees.

    <item*|<source-link|Graphics/Pictures/scalable.hpp|src/Graphics/Pictures/scalable.hpp>,
    <source-link|scalable.cpp|src/Graphics/Pictures/scalable.cpp>>Scalable images and <cpp|scalable_image_rep>.

    <item*|<source-link|System/Files/image_files.hpp|src/System/Files/image_files.hpp>,
    <source-link|image_files.cpp|src/System/Files/image_files.cpp>>Image sizes, the size cache, the
    conversions <cpp|image_to_png>, <cpp|image_to_eps>, <cpp|image_to_pdf>,
    <cpp|image_to_psdoc>, the calls to <scheme> converters and to
    <name|ImageMagick>, <cpp|xpm_load> and <cpp|ps_load>.

    <item*|<source-link|Plugins/Qt/qt_picture.cpp|src/Plugins/Qt/qt_picture.cpp>,
    <source-link|qt_utilities.cpp|src/Plugins/Qt/qt_utilities.cpp>>The <name|Qt> pictures, the loading of image
    files into <cpp|QImage>s with their own cache, icons, effects applied to
    files, and the <name|Qt> based size determination and conversions
    (<cpp|qt_supports>, <cpp|qt_image_size>, <cpp|qt_convert_image>,
    <cpp|qt_image_to_pdf>). <source-link|Plugins/Qt6/|src/Plugins/Qt6> contains a copy of these
    files for <name|Qt> 6.

    <item*|<source-link|Plugins/Resvg/resvg.cpp|src/Plugins/Resvg/resvg.cpp>>Size determination and
    rendering of <abbr|SVG> files with the <name|resvg> library
    (<cpp|USE_RESVG>).

    <item*|<source-link|Plugins/Ghostscript/gs_utilities.cpp|src/Plugins/Ghostscript/gs_utilities.cpp>,
    <source-link|ghostscript.cpp|src/Plugins/Ghostscript/ghostscript.cpp>>Calls to the <name|Ghostscript> executable
    for PostScript and <abbr|PDF> files (<cpp|USE_GS>), and, for the
    <name|X11> port, the rendering of PostScript into pixmaps.

    <item*|<source-link|Plugins/Imlib2/imlib2.cpp|src/Plugins/Imlib2/imlib2.cpp>>Optional dynamically loaded
    <name|Imlib2> support for the <name|X11> port.

    <item*|<source-link|Plugins/MacOS/mac_images.mm|src/Plugins/MacOS/mac_images.mm>>Image sizes and
    conversion to <name|PNG> with the <name|macOS> frameworks, used only
    when <TeXmacs> is not built with <name|Qt> 6.

    <item*|<source-link|Plugins/Pdf/pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp>>The image related
    parts of the <abbr|PDF> renderer: <cpp|pdf_image_rep>, image and pattern
    pools, <cpp|hummus_pdf_image_size>.

    <item*|<source-link|Plugins/Pdf/pdf_hummus_make_attachment.cpp|src/Plugins/Pdf/pdf_hummus_make_attachment.cpp>,
    <source-link|pdf_hummus_extract_attachment.cpp|src/Plugins/Pdf/pdf_hummus_extract_attachment.cpp>>Embedding files into
    <abbr|PDF> files and extracting them again.

    <item*|<source-link|Plugins/Cairo/|src/Plugins/Cairo>>A <name|Cairo> renderer, only compiled
    with <cpp|USE_CAIRO>, which the <name|CMake> build does not set.

    <item*|<source-link|Typeset/Concat/concat_active.cpp|src/Typeset/Concat/concat_active.cpp>,
    <source-link|Typeset/Boxes/Basic/basic_boxes.cpp|src/Typeset/Boxes/Basic/basic_boxes.cpp>>The typesetting of the
    <markup|image> primitive and the image box.

    <item*|<source-link|Graphics/Renderer/renderer.cpp|src/Graphics/Renderer/renderer.cpp>,
    <source-link|printer.cpp|src/Graphics/Renderer/printer.cpp>>Background patterns
    (<cpp|renderer_rep::clear_pattern>) and the PostScript inclusion of
    images.

    <item*|<source-link|Edit/Modify/edit_text.cpp|src/Edit/Modify/edit_text.cpp>>The insertion of images
    (<cpp|edit_text_rep::make_image>).

    <item*|<verbatim|$TEXMACS_PATH/progs/convert/images/init-images.scm>>The
    image formats and the converters between them which call external
    programs.

    <item*|<verbatim|$TEXMACS_PATH/progs/convert/images/tmimage.scm>>Export
    of the selection as an image and copy of the selection to the clipboard
    as an image.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Pictures, effects and picture caches|images-pictures.en.tm>

    <branch|Image files: sizes and conversions|images-files.en.tm>

    <branch|Images in documents|images-typesetting.en.tm>

    <branch|Images in <abbr|PDF> and PostScript output; <abbr|PDF>
    attachments|images-pdf.en.tm>
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
