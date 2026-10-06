<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The renderer interface>

  <section|Introduction>

  All graphical output of <TeXmacs> goes through one abstract class,
  <cpp|renderer_rep>, declared in <source-link|Graphics/Renderer/renderer.hpp|src/Graphics/Renderer/renderer.hpp>.
  A <em|renderer> is a drawing surface: the screen (or rather a backing
  store of a window), an off-screen pixmap, an image in memory, a PostScript
  file or a <abbr|PDF> file. The typesetter produces a tree of
  <hlink|boxes|boxes.en.tm>; every box knows how to paint itself on an
  arbitrary renderer, and it never needs to know which concrete device it is
  painting on. The same <cpp|box_rep::redraw> code is used for repainting a
  window, for printing, for exporting a document to <abbr|PDF> and for
  rasterizing a snippet into a <verbatim|png> file.

  The abstract class provides three things:

  <\itemize>
    <item>A coordinate system: an origin, a zoom factor and a clipping
    rectangle, together with the conversion routines between the logical
    coordinates of the typesetter and device pixels. This part is
    implemented once and for all in <source-link|Graphics/Renderer/renderer.cpp|src/Graphics/Renderer/renderer.cpp>.

    <item>A graphical state (the current <em|pencil>, <em|brush> and
    <em|background>) and a small set of drawing primitives: glyphs, lines,
    polylines, rectangles, arcs, polygons and pictures. Most of these are pure
    virtual and must be supplied by the concrete renderers.

    <item>A few device-specific services: <em|shadows> for double buffering
    on the screen, off-screen rendering into pictures, and, for printers,
    pages, hyperlinks, a table of contents and metadata.
  </itemize>

  The type <cpp|renderer> is simply a plain pointer
  <cpp|typedef renderer_rep* renderer>; renderers are <em|not> reference
  counted. They are created with <cpp|tm_new> and destroyed with
  <cpp|tm_delete> or <cpp|delete_renderer>. For printers, destroying the
  renderer is what finishes and writes the output file.

  The relevant source files are:

  <\description>
    <item*|<source-link|Graphics/Renderer/|src/Graphics/Renderer>>The abstract class
    (<source-link|renderer.hpp|src/Graphics/Renderer/renderer.hpp>, <source-link|renderer.cpp|src/Graphics/Renderer/renderer.cpp>), the common base class
    of the screen renderers (<source-link|basic_renderer.hpp|src/Graphics/Renderer/basic_renderer.hpp>,
    <source-link|basic_renderer.cpp|src/Graphics/Renderer/basic_renderer.cpp>), the PostScript renderer
    (<source-link|printer.hpp|src/Graphics/Renderer/printer.hpp>, <source-link|printer.cpp|src/Graphics/Renderer/printer.cpp>), pencils
    (<source-link|pencil.hpp|src/Graphics/Renderer/pencil.hpp>), brushes (<source-link|brush.hpp|src/Graphics/Renderer/brush.hpp>) and paper sizes
    (<source-link|page_type.hpp|src/Graphics/Renderer/page_type.hpp>).

    <item*|<source-link|Graphics/Pictures/|src/Graphics/Pictures>>The picture abstraction
    (<source-link|picture.hpp|src/Graphics/Pictures/picture.hpp>), portable raster pictures
    (<source-link|raster.hpp|src/Graphics/Pictures/raster.hpp>, <source-link|raster_picture.hpp|src/Graphics/Pictures/raster_picture.hpp>), scalable images
    (<source-link|scalable.hpp|src/Graphics/Pictures/scalable.hpp>) and graphical effects (<source-link|effect.hpp|src/Graphics/Pictures/effect.hpp>).

    <item*|<verbatim|Plugins/Qt/>>The <name|Qt> screen renderer
    (<source-link|qt_renderer.hpp|src/Plugins/Qt/qt_renderer.hpp>, <source-link|qt_renderer.cpp|src/Plugins/Qt/qt_renderer.cpp>) and <name|Qt>
    native pictures (<source-link|qt_picture.hpp|src/Plugins/Qt/qt_picture.hpp>, <source-link|qt_picture.cpp|src/Plugins/Qt/qt_picture.cpp>).
    The directory <source-link|Plugins/Qt6/|src/Plugins/Qt6> contains a
    slightly different copy of these files.

    <item*|<source-link|Plugins/Pdf/|src/Plugins/Pdf>>The <abbr|PDF> renderer based on the
    <name|PDFHummus> library (<source-link|pdf_hummus_renderer.hpp|src/Plugins/Pdf/pdf_hummus_renderer.hpp>,
    <source-link|pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp>).

    <item*|Other back-ends>The <name|X11> renderer
    (<source-link|Plugins/X11/x_drawable.hpp|src/Plugins/X11/x_drawable.hpp>, <source-link|x_shadow.cpp|src/Plugins/X11/x_shadow.cpp>,
    <source-link|x_picture.cpp|src/Plugins/X11/x_picture.cpp>), the
    <name|MuPDF> renderers of the <name|SDL> and <name|Vue> ports and of
    <abbr|PDF> export (<source-link|Plugins/MuPDF/|src/Plugins/MuPDF>), the
    GPU renderer of <name|Vue>
    (<source-link|vue_gpu.cpp|src/Plugins/Vue/vue_gpu.cpp>), the
    <name|Cocoa> renderer
    (<source-link|ns_renderer.mm|src/Plugins/NS/ns_renderer.mm>), and the
    older <name|Cairo> renderer
    (<source-link|Plugins/Cairo/|src/Plugins/Cairo>).
  </description>

  <\traverse>
    <branch|The renderer API|renderer-api.en.tm>

    <branch|Renderers at work: boxes, repainting and
    printing|renderer-pipeline.en.tm>

    <branch|Implementations, new renderers and pitfalls|renderer-backends.en.tm>
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
