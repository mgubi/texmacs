<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The renderer interface>

  <section|Introduction>

  All graphical output of <TeXmacs> goes through one abstract class,
  <cpp|renderer_rep>, declared in <verbatim|Graphics/Renderer/renderer.hpp>.
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
    implemented once and for all in <verbatim|Graphics/Renderer/renderer.cpp>.

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
    <item*|<verbatim|Graphics/Renderer/>>The abstract class
    (<verbatim|renderer.hpp>, <verbatim|renderer.cpp>), the common base class
    of the screen renderers (<verbatim|basic_renderer.hpp>,
    <verbatim|basic_renderer.cpp>), the PostScript renderer
    (<verbatim|printer.hpp>, <verbatim|printer.cpp>), pencils
    (<verbatim|pencil.hpp>), brushes (<verbatim|brush.hpp>) and paper sizes
    (<verbatim|page_type.hpp>).

    <item*|<verbatim|Graphics/Pictures/>>The picture abstraction
    (<verbatim|picture.hpp>), portable raster pictures
    (<verbatim|raster.hpp>, <verbatim|raster_picture.hpp>), scalable images
    (<verbatim|scalable.hpp>) and graphical effects (<verbatim|effect.hpp>).

    <item*|<verbatim|Plugins/Qt/>>The <name|Qt> screen renderer
    (<verbatim|qt_renderer.hpp>, <verbatim|qt_renderer.cpp>) and <name|Qt>
    native pictures (<verbatim|qt_picture.hpp>, <verbatim|qt_picture.cpp>).
    The directory <verbatim|Plugins/Qt6/> contains a copy of these files.

    <item*|<verbatim|Plugins/Pdf/>>The <abbr|PDF> renderer based on the
    <name|PDFHummus> library (<verbatim|pdf_hummus_renderer.hpp>,
    <verbatim|pdf_hummus_renderer.cpp>).

    <item*|Other back-ends>The <name|X11> renderer
    (<verbatim|Plugins/X11/x_drawable.hpp>, <verbatim|x_shadow.cpp>,
    <verbatim|x_picture.cpp>), and the older <name|Cairo>
    (<verbatim|Plugins/Cairo/>), <name|Cocoa> (<verbatim|Plugins/Cocoa/>)
    and <name|CoreGraphics> (<verbatim|Plugins/MacOS/>) renderers.
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
