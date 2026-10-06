<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Renderer implementations>

  <section|The implementations>

  <subsection|Overview>

  <tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Class>|<cell|Files>|<cell|Device>>|<row|<cell|<cpp|basic_renderer_rep>>|<cell|<verbatim|Graphics/Renderer/basic_renderer.*>>|<cell|common
  base of screen renderers>>|<row|<cell|<cpp|qt_renderer_rep>>|<cell|<verbatim|Plugins/Qt/qt_renderer.*>>|<cell|<cpp|QPainter>>>|<row|<cell|<cpp|qt_proxy_renderer_rep>,
  <cpp|qt_shadow_renderer_rep>>|<cell|<verbatim|Plugins/Qt/qt_renderer.*>>|<cell|shadows>>|<row|<cell|<cpp|qt_image_renderer_rep>>|<cell|<verbatim|Plugins/Qt/qt_picture.*>>|<cell|<cpp|QImage>
  of a picture>>|<row|<cell|<cpp|printer_rep>>|<cell|<verbatim|Graphics/Renderer/printer.*>>|<cell|PostScript
  file>>|<row|<cell|<cpp|pdf_hummus_renderer_rep>>|<cell|<verbatim|Plugins/Pdf/pdf_hummus_renderer.*>>|<cell|<abbr|PDF>
  file>>|<row|<cell|<cpp|x_drawable_rep>>|<cell|<verbatim|Plugins/X11/x_drawable.*>,
  <source-link|x_shadow.cpp|src/Plugins/X11/x_shadow.cpp>, <source-link|x_picture.cpp|src/Plugins/X11/x_picture.cpp>>|<cell|<name|X11>
  window or pixmap>>|<row|<cell|<cpp|cairo_renderer_rep>>|<cell|<verbatim|Plugins/Cairo/cairo_renderer.*>>|<cell|<name|Cairo>
  context>>|<row|<cell|<cpp|aqua_renderer_rep>>|<cell|<verbatim|Plugins/Cocoa/aqua_renderer.*>>|<cell|<name|Cocoa>
  view>>|<row|<cell|<cpp|cg_renderer_rep>>|<cell|<verbatim|Plugins/MacOS/cg_renderer.*>>|<cell|<name|CoreGraphics>
  context>>>>>

  The <name|CMake> build compiles <verbatim|Plugins/Qt> and
  <verbatim|Plugins/Pdf> (with <cpp|PDF_RENDERER> set), and nothing of the
  other back-ends; the <verbatim|configure> based build chooses the
  <abbr|GUI> directory (<verbatim|Qt>, <verbatim|Qt6>, <verbatim|X11>,
  <verbatim|Cocoa>) at configuration time (<verbatim|Qt6> is used for <name|Qt> 6), and always
  compiles <verbatim|Plugins/Cairo>, whose code is guarded by
  <cpp|USE_CAIRO>. At the time of
  writing, <verbatim|Plugins/Qt/qt_renderer.*> and
  <verbatim|Plugins/Qt6/qt_renderer.*> are identical.

  <subsection|<cpp|basic_renderer_rep>>

  <cpp|basic_renderer_rep> derives from <cpp|renderer_rep> and adds the
  device size <cpp|w>, <cpp|h>, a pencil <cpp|pen>, a foreground brush
  <cpp|fg_brush> and a background brush <cpp|bg_brush>, with the obvious
  accessors, together with <cpp|begin (void* handle)> and <cpp|end ()> to
  bracket a drawing session on a native device, color helpers <cpp|rgb> and
  <cpp|get_rgb>, and no-op shadow operations. The file also defines
  <cpp|basic_character>, the key of glyph caches, and
  <cpp|gui_interrupted>. Note that <source-link|basic_renderer.cpp|src/Graphics/Renderer/basic_renderer.cpp> is only
  compiled when <cpp|QTTEXMACS> or <cpp|AQUATEXMACS> is defined.

  <subsection|The <name|Qt> renderer>

  <cpp|qt_renderer_rep> draws with a <cpp|QPainter* painter>. A single
  instance, returned by <cpp|the_qt_renderer (double pixel_ratio)>, is
  shared by all widgets; with <name|Qt> 6 it adapts its <cpp|pixel_ratio>
  and zoom when it is requested with another ratio. <cpp|begin (handle)>
  opens the painter on a <cpp|QPaintDevice> (the backing pixmap, or a
  <cpp|QImage> in headless mode) and records its size.

  Particularities:

  <\itemize>
    <item>Lines, polylines, arcs and polygons are anti-aliased
    (<cpp|QPainter::Antialiasing>), whereas <cpp|fill>, <cpp|clear>,
    <cpp|draw_triangle> and bitmaps are not. Rectangles are therefore
    pixel-exact, while strokes are drawn at sub-pixel positions using the
    floating point <cpp|decode>.

    <item>The pen width is <cpp|pen-\<gtr\>get_width () / pixel> device
    pixels, as a floating point number. Joins are always round, caps are
    round or square.

    <item>Pattern pencils and brushes are implemented as <cpp|QBrush>es
    textured with the pattern image, loaded with
    <cpp|get_image (url u, int w, int h, tree eff, SI pixel)> and translated
    to the device position of the logical origin.

    <item>Glyphs are rasterized once per color and font, and cached in
    <cpp|character_image>; images loaded from files are cached by
    <cpp|get_image>. <cpp|del_obj_qt_renderer> empties the static caches
    before <name|Qt> is shut down.

    <item><cpp|draw_clipped> draws a bitmap at a logical point after
    subtracting one pixel in <math|y> (<verbatim|top-left origin to
    bottom-left origin conversion>).

    <item>With <name|Qt> 6, <cpp|clear_device> paints a neutral checkerboard.
  </itemize>

  <subsection|The PostScript renderer>

  <cpp|printer_rep> generates <abbr|DSC> conforming PostScript in memory
  (the strings <cpp|prologue> and <cpp|body>) and writes the file in its
  destructor. It uses the <TeX> <verbatim|dvips> prologues
  <verbatim|tex.pro>, <verbatim|special.pro>, <verbatim|color.pro> and
  <verbatim|texps.pro> from <verbatim|$TEXMACS_PATH/misc/convert/>, and
  defines short PostScript procedures for its primitives
  (<cpp|PS_LINE>, <cpp|PS_FILL>, <cpp|PS_ARC>, <abbr|etc.>). Its
  <cpp|pixel> equals <cpp|PIXEL>: coordinates are output in dots of the
  printing resolution. Glyphs are collected per font by
  <cpp|make_tex_char>, and fonts of more than 256 glyphs are split into
  several PostScript fonts. At the end, <cpp|generate_tex_fonts> writes the
  font definitions into the prologue: for <verbatim|tt> fonts whose file is
  a <verbatim|pfb> Type 1 font (except on <name|Windows>) the font program
  itself is embedded, and otherwise the glyphs are emitted as bitmap
  characters in the format of the <verbatim|dvips> prologue. Colors with an
  alpha component are blended with the background color in
  <cpp|set_pencil>; genuine transparency (through <verbatim|pdfmark>) is
  only attempted when the preference <verbatim|experimental alpha> is on.
  Pictures are embedded as
  <abbr|EPS> via <cpp|picture_as_eps>, and scalable images by including
  their PostScript code.

  <subsection|The <abbr|PDF> renderer>

  The whole export path, the fonts and the text layer, links, bookmarks
  and metadata are described in more detail in <hlink|PDF
  export|pdf-export.en.tm>.

  <cpp|pdf_hummus_renderer_rep> writes <abbr|PDF> through the
  <name|PDFHummus> library (a <cpp|PDFWriter>, the current <cpp|PDFPage> and
  <cpp|PageContentContext>). Unlike the screen renderers it keeps the
  <math|y>-axis pointing upwards: its private helpers <cpp|to_x> and
  <cpp|to_y> add the origin and divide by <cpp|pixel> without changing the
  sign. Particularities:

  <\itemize>
    <item>Fonts are embedded natively whenever possible:
    <cpp|make_pdf_font> looks for the font file with <cpp|tt_font_find> and
    loads it with <cpp|PDFWriter::GetFontForFile>. Fonts which cannot be
    embedded (for instance <name|Metafont> generated <verbatim|pk> fonts)
    are converted into bitmap <verbatim|Type 3> fonts (class
    <cpp|t3font>), in chunks of glyphs.

    <item>Text is written inside <verbatim|BT>/<verbatim|ET> blocks
    (<cpp|begin_text>, <cpp|end_text>); every change of clipping or
    transformation closes the current text block.

    <item>Transparency is supported through extended graphics states
    (<cpp|select_alpha>), and pattern brushes through <abbr|PDF> patterns.

    <item>Pictures are saved as temporary <verbatim|png> files (with
    <name|Qt>) and embedded, cached by <cpp|get_unique_id>; image files are
    embedded directly by <cpp|draw_scalable>.

    <item>Hyperlinks, destinations, outlines and metadata are written when
    the document is finished.
  </itemize>

  <subsection|The <name|X11> renderer>

  <cpp|x_drawable_rep> derives directly from <cpp|renderer_rep>, and draws
  on an <name|X11> <cpp|Drawable> (a window or a pixmap) with a graphics
  context <cpp|gc>. It is the reference implementation of the shadow
  protocol: <cpp|new_shadow> allocates a pixmap drawable of the same size,
  and <cpp|get_shadow>, <cpp|put_shadow> and <cpp|fetch> are all
  <cpp|XCopyArea> calls. The primitives are plain <name|Xlib> calls
  (<cpp|XDrawLine>, <cpp|XDrawArc>, <cpp|XFillPolygon>, <abbr|etc.>), so
  strokes are not anti-aliased, and <cpp|draw_picture> ignores its
  <cpp|alpha> argument.

  <subsection|Older renderers>

  <cpp|cairo_renderer_rep> (used by
  <cpp|qt_simple_widget_rep::get_renderer> when <cpp|USE_CAIRO> is
  defined), <cpp|aqua_renderer_rep> and <cpp|cg_renderer_rep> all derive
  from <cpp|basic_renderer_rep>. They have not followed the recent
  evolution of the interface: for instance, their constructors still call
  <cpp|basic_renderer_rep (true, w2, h2)>, although the second argument of
  that constructor is now the pixel ratio. Treat them as starting points
  rather than as working code.

  <section|Writing a new renderer>

  A new renderer is a subclass of <cpp|renderer_rep>, or, for a screen
  device, of <cpp|basic_renderer_rep>. The following methods are pure
  virtual and must be implemented:

  <\itemize>
    <item>State: <cpp|get_pencil>, <cpp|set_pencil>, <cpp|get_background>,
    <cpp|set_background> (provided by <cpp|basic_renderer_rep>).

    <item>Drawing: <cpp|clear_device>, <cpp|draw>, <cpp|line>,
    <cpp|lines>, <cpp|clear>, <cpp|fill>, <cpp|arc>, <cpp|fill_arc>,
    <cpp|polygon>.

    <item>Shadows: <cpp|fetch>, <cpp|new_shadow>, <cpp|delete_shadow>,
    <cpp|get_shadow>, <cpp|put_shadow>, <cpp|apply_shadow> (no-ops in
    <cpp|basic_renderer_rep>; a printer can leave them empty).
  </itemize>

  In practice, the following ones must also be overridden:

  <\itemize>
    <item><cpp|set_clipping>, to clip the device as well; call the base
    implementation first so that <cpp|cx1>, <abbr|...>, <cpp|cy2> are kept
    up to date, and honor the <cpp|restore> flag if the device clipping is a
    stack.

    <item><cpp|draw_picture>, whose default aborts.

    <item><cpp|set_transformation> and <cpp|reset_transformation>, if
    rotated or scaled content should be rendered correctly.

    <item><cpp|set_brush>, for pattern fills, and
    <cpp|set_zoom_factor>, if the device has its own scaling.

    <item>For printers: <cpp|is_printer>, <cpp|next_page>, <cpp|shadow>,
    <cpp|draw_scalable>, and the hyperlink and metadata methods; the
    destructor must finish the output.
  </itemize>

  If the renderer comes with a new <abbr|GUI> back-end, that back-end must
  also provide the free functions <cpp|native_picture>,
  <cpp|picture_renderer>, <cpp|load_picture>, <cpp|as_native_picture> and
  <cpp|save_picture>, and adapt the conditional stubs at the end of
  <source-link|renderer.cpp|src/Graphics/Renderer/renderer.cpp>. A printer that should be selectable for export
  must be hooked into the factory <cpp|printer> in <source-link|printer.cpp|src/Graphics/Renderer/printer.cpp>.

  A useful way to proceed is to start from the <name|Qt> renderer, which
  shows how to map each primitive to a modern 2D graphics <abbr|API>, and
  from the <abbr|PDF> renderer for the printer-specific parts.

  <section|Pitfalls>

  <\description>
    <item*|The <math|y>-axis>Logical <math|y> grows upwards and device
    <math|y> downwards. A rectangle <cpp|(x1, y1, x2, y2)> decodes to a
    device rectangle with top left corner <cpp|(x1, y2)>. Forgetting this is
    the most common source of off-by-one-rectangle bugs. The <abbr|PDF>
    renderer is an exception, as its device axis also points upwards.

    <item*|Relative and absolute coordinates>Drawing routines and
    <cpp|get_clipping>/<cpp|set_clipping> use coordinates relative to the
    origin, whereas the fields <cpp|cx1>, <abbr|...>, <cpp|cy2> and the
    rectangles returned by <cpp|box_rep::redraw> are absolute. Code which
    reads the fields directly (as the <name|Qt> <cpp|clear> and <cpp|fill>
    do) must subtract <cpp|ox> and <cpp|oy>.

    <item*|Rounding>Integer <cpp|decode> rounds towards
    <math|-\<infty\>>, and rectangles should be passed through
    <cpp|outer_round> (as <cpp|set_clipping> and the shadow routines do)
    before being decoded; otherwise one-pixel seams or stale lines remain
    after partial repaints. The visibility test in <cpp|box_rep::redraw>
    enlarges boxes by <cpp|retina_pixel> for the same reason. Note that the
    rounding grid is that of <cpp|retina_pixel>, not <cpp|pixel>.

    <item*|Pixel ratio>On high density screens <cpp|pixel> is the size of a
    device pixel and <cpp|retina_pixel> the size of a logical pixel. Use
    <cpp|pixel> for the finest detail (hairlines) and <cpp|retina_pixel>
    for anything related to the layout on screen. With <name|Qt> older than
    6 the scaling is controlled by the global <cpp|retina_factor>; with
    <name|Qt> 6 by the <cpp|pixel_ratio> of each renderer, and a shadow must
    be recreated when the ratio changes (which <cpp|new_shadow> does).

    <item*|Zoom and state>The zoom factor changes the meaning of
    <cpp|ox>, <cpp|oy>, <cpp|cx1>, <abbr|...>: always change it with
    <cpp|set_zoom_factor> and <cpp|reset_zoom_factor>, which rescale these
    fields, and restore the previous zoom when you are done. Remember that
    <cpp|font_rep::draw> temporarily overwrites <cpp|zoomf>, <cpp|pixel>,
    <cpp|ox>, <abbr|etc.> while glyphs are drawn.

    <item*|Clipping balance>Every narrowing <cpp|set_clipping> or
    <cpp|extra_clipping> must be matched by a <cpp|set_clipping (...,
    true)> restoring the previous region (or use <cpp|clip>/<cpp|unclip>).
    On screen an unbalanced call just leaves a wrong clip, but printers
    maintain a graphics state stack and produce corrupted output.

    <item*|Interruptions>On screen, redrawing may stop at any time when the
    user types. Code in <cpp|pre_display> and <cpp|post_display> must be
    symmetric, and a box overriding <cpp|redraw> should respect
    <cpp|gui_interrupted> as <cpp|effect_box_rep::redraw> does.

    <item*|Lifetime>Renderers are raw pointers. Shadows are owned by whoever
    calls <cpp|new_shadow>, picture renderers by the caller of
    <cpp|shadow (picture&, ...)> or <cpp|picture_renderer>, and printer
    renderers must be deleted to produce their file.
  </description>

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
