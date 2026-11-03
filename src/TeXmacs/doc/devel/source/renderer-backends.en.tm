<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Renderer implementations>

  <section|The implementations>

  <subsection|Overview>

  <tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-hpart|2>|<cwith|1|-1|2|2|cell-hpart|1>|<cwith|1|-1|3|3|cell-hpart|1>|<cwith|1|-1|1|-1|cell-bsep|0.5spc>|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|<with|par-mode|left|Class and files>>|<cell|<with|par-mode|left|Device>>|<cell|<with|par-mode|left|Used by>>>|<row|<cell|<with|par-mode|left|<cpp|basic_renderer_rep> (<verbatim|Graphics/Renderer/basic_renderer.*>)>>|<cell|<with|par-mode|left|common base of screen renderers>>|<cell|<with|par-mode|left|all but <name|X11>>>>|<row|<cell|<with|par-mode|left|<cpp|qt_renderer_rep> (<verbatim|Plugins/Qt/qt_renderer.*>)>>|<cell|<with|par-mode|left|<cpp|QPainter>>>|<cell|<with|par-mode|left|<name|Qt>, <name|Qtwk>>>>|<row|<cell|<with|par-mode|left|<cpp|qt_proxy_renderer_rep>, <cpp|qt_shadow_renderer_rep> (<verbatim|Plugins/Qt/qt_renderer.*>)>>|<cell|<with|par-mode|left|shadows>>|<cell|<with|par-mode|left|<name|Qt>>>>|<row|<cell|<with|par-mode|left|<cpp|qt_image_renderer_rep> (<verbatim|Plugins/Qt/qt_picture.*>)>>|<cell|<with|par-mode|left|<cpp|QImage> of a picture>>|<cell|<with|par-mode|left|<name|Qt>, <name|Qtwk> (without <name|MuPDF>)>>>|<row|<cell|<with|par-mode|left|<cpp|mupdf_renderer_rep> (<verbatim|Plugins/MuPDF/mupdf_renderer.*>)>>|<cell|<with|par-mode|left|<name|MuPDF> pixmap>>|<cell|<with|par-mode|left|<name|SDL>, <name|Vue>; pictures of <name|Qt>>>>|<row|<cell|<with|par-mode|left|<cpp|gpu_renderer_rep> (<source-link|Plugins/Vue/vue_gpu.cpp|src/Plugins/Vue/vue_gpu.cpp>)>>|<cell|<with|par-mode|left|<name|OpenGL> texture or window, <name|ThorVG>>>|<cell|<with|par-mode|left|<name|Vue>>>>|<row|<cell|<with|par-mode|left|<cpp|ns_renderer_rep> (<verbatim|Plugins/NS/ns_renderer.*>)>>|<cell|<with|par-mode|left|<cpp|NSGraphicsContext> (<name|CoreGraphics>)>>|<cell|<with|par-mode|left|<name|Cocoa>>>>|<row|<cell|<with|par-mode|left|<cpp|x_drawable_rep> (<verbatim|Plugins/X11/x_drawable.*>, <source-link|x_shadow.cpp|src/Plugins/X11/x_shadow.cpp>, <source-link|x_picture.cpp|src/Plugins/X11/x_picture.cpp>)>>|<cell|<with|par-mode|left|<name|X11> window or pixmap>>|<cell|<with|par-mode|left|<name|X11>>>>|<row|<cell|<with|par-mode|left|<cpp|cairo_renderer_rep> (<verbatim|Plugins/Cairo/cairo_renderer.*>)>>|<cell|<with|par-mode|left|<name|Cairo> context>>|<cell|<with|par-mode|left|<name|Qt> with <cpp|USE_CAIRO>>>>|<row|<cell|<with|par-mode|left|<cpp|printer_rep> (<verbatim|Graphics/Renderer/printer.*>)>>|<cell|<with|par-mode|left|PostScript file>>|<cell|<with|par-mode|left|all>>>|<row|<cell|<with|par-mode|left|<cpp|pdf_hummus_renderer_rep> (<verbatim|Plugins/Pdf/pdf_hummus_renderer.*>)>>|<cell|<with|par-mode|left|<abbr|PDF> file>>|<cell|<with|par-mode|left|<name|Qt>, <name|Cocoa>>>>|<row|<cell|<with|par-mode|left|<cpp|mupdf_pdf_renderer_rep> (<verbatim|Plugins/MuPDF/mupdf_pdf_renderer.*>)>>|<cell|<with|par-mode|left|<abbr|PDF> file>>|<cell|<with|par-mode|left|with <name|MuPDF>, on demand>>>>>>

  The <abbr|GUI> directory is chosen at configuration time:
  <verbatim|./configure --with-gui=...> takes <verbatim|Qt> (or
  <verbatim|Qt6> with <verbatim|--enable-qt-new>), <verbatim|Qtwk> (with
  the renderer and the pictures of <source-link|Plugins/Qt|src/Plugins/Qt>), <verbatim|X11>,
  <verbatim|SDL>, <verbatim|Vue> or <verbatim|NS>; the <name|CMake> build
  (<verbatim|TEXMACS_GUI>) knows <verbatim|Qt>, <verbatim|Vue>,
  <verbatim|SDL> and <verbatim|X11>. <source-link|Plugins/Cairo|src/Plugins/Cairo>
  is always compiled, its code guarded by <cpp|USE_CAIRO>.
  <source-link|Plugins/MuPDF|src/Plugins/MuPDF> is compiled with
  <cpp|MUPDF_RENDERER> for <name|Qt>, <name|SDL> and <name|Vue> when
  <name|MuPDF> is found (it is required by the last two), never for
  <name|X11> and <name|Cocoa>, whose pictures would clash with it;
  <source-link|Plugins/Pdf|src/Plugins/Pdf> (<cpp|PDF_RENDERER>,
  <name|PDFHummus>) only with <name|Qt> and <name|Cocoa> (with <name|CMake>,
  only for <name|Qt> without <name|MuPDF>). The renderers of
  <source-link|Plugins/Qt|src/Plugins/Qt> and <source-link|Plugins/Qt6|src/Plugins/Qt6> are kept identical
  (<cpp|rounded_rectangle> included); like the rest of the two directories,
  they are synchronized by hand.

  <subsection|<cpp|basic_renderer_rep>>

  <cpp|basic_renderer_rep> derives from <cpp|renderer_rep> and adds the
  device size <cpp|w>, <cpp|h>, a pencil <cpp|pen>, a foreground brush
  <cpp|fg_brush> and a background brush <cpp|bg_brush>, with the obvious
  accessors, together with <cpp|begin (void* handle)> and <cpp|end ()> to
  bracket a drawing session on a native device, color helpers <cpp|rgb> and
  <cpp|get_rgb>, and no-op shadow operations. The file also defines
  <cpp|basic_character>, the key of glyph caches, and
  <cpp|gui_interrupted>. Note that <source-link|basic_renderer.cpp|src/Graphics/Renderer/basic_renderer.cpp> is
  compiled for all ports but <name|X11> (it is guarded by
  <cpp|!defined(X11TEXMACS)>): the <name|X11> port has its own
  <cpp|gui_interrupted>.

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
    <cpp|QImage> under <name|Qt>, <cpp|save_picture> under <name|Cocoa>)
    and embedded, cached by <cpp|get_unique_id>; image files are
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

  <subsection|The <name|MuPDF> renderers>

  <cpp|mupdf_renderer_rep>
  (<source-link|mupdf_renderer.cpp|src/Plugins/MuPDF/mupdf_renderer.cpp>)
  draws with the <name|Fitz> library of <name|MuPDF> into an
  <cpp|fz_pixmap>, which is the picture <cpp|mupdf_picture_rep>
  (<source-link|mupdf_picture.cpp|src/Plugins/MuPDF/mupdf_picture.cpp>).
  Strokes and fills are anti-aliased; glyphs are drawn from the bitmaps of
  <TeXmacs>; shadows are pixmaps. It is the screen renderer of the
  <name|SDL> port (one backing pixmap per window, see
  <source-link|sdl_window.cpp|src/Plugins/SDL/sdl_window.cpp>) and of the
  <name|Vue> port without the GPU, and it draws the pictures (icons,
  images) of <name|Qt> and <name|Qtwk> builds with <name|MuPDF>, in place
  of the <cpp|QImage>s of <source-link|qt_picture.cpp|src/Plugins/Qt/qt_picture.cpp>.

  <cpp|mupdf_pdf_renderer_rep>
  (<source-link|mupdf_pdf_renderer.cpp|src/Plugins/MuPDF/mupdf_pdf_renderer.cpp>)
  is a printer which writes <abbr|PDF> with <name|MuPDF>, a prototype
  alternative to <name|PDFHummus>: <cpp|printer> in
  <source-link|printer.cpp|src/Graphics/Renderer/printer.cpp> takes it when
  <cpp|use_mupdf_pdf ()> holds, that is with the preference
  <verbatim|native pdf renderer> set to <verbatim|mupdf>, with
  <verbatim|TEXMACS_PDF_MUPDF=1>, or always in the browser.

  <subsection|The renderers of the <name|Vue> port>

  The <name|Vue> port draws its widgets in immediate mode with <name|Clay>
  and its documents with one of two renderers, chosen once at start-up
  (<cpp|vue_gpu_enabled>):

  <\itemize>
    <item>Without the GPU, every window has an <name|MuPDF> pixmap as
    backing store, drawn by <cpp|mupdf_renderer_rep>
    (<cpp|vue_sdl_mupdf_window_rep> in
    <source-link|vue_gui.cpp|src/Plugins/Vue/vue_gui.cpp>).

    <item>With the GPU (<verbatim|./configure --with-thorvg=...>, or
    <verbatim|THORVG_DIR> with <name|CMake>, which define
    <cpp|USE_THORVG>; on unless <verbatim|TEXMACS_VUE_GPU=0>, and
    abandoned when no <name|OpenGL> context can be made),
    <cpp|gpu_renderer_rep>
    (<source-link|vue_gpu.cpp|src/Plugins/Vue/vue_gpu.cpp>) draws into
    <name|OpenGL> textures (<name|WebGL> 2 in the browser): glyphs from an
    atlas of the bitmaps of <TeXmacs>, plain fills from the same atlas,
    pictures as textures, and lines, polygons, arcs and rounded rectangles
    with the <name|OpenGL> engine of <name|ThorVG>. The backing store of an
    editor is a texture (<cpp|gpu_picture_rep>), repainted incrementally as
    the pixmap was.
  </itemize>

  A third path, through <name|SDL>'s own renderer and the example
  renderer of <name|Clay> (<cpp|vue_sdl_window_rep>), is unused. A
  <name|CMake> build without <verbatim|THORVG_DIR> has no <name|ThorVG>,
  hence no GPU path.

  <subsection|The <name|Cocoa> renderer>

  <cpp|ns_renderer_rep>
  (<source-link|ns_renderer.mm|src/Plugins/NS/ns_renderer.mm>) derives
  from <cpp|basic_renderer_rep> and draws with <name|CoreGraphics> in the
  <cpp|NSGraphicsContext> given to <cpp|begin>, that of the view
  (<cpp|TMView>) or that of a bitmap for the pictures
  (<source-link|ns_picture.mm|src/Plugins/NS/ns_picture.mm>). It keeps
  a stack of clippings per context (<cpp|clip_pushed>) and reapplies its
  state after a context is saved and restored.

  <subsection|The <name|Cairo> renderer>

  <cpp|cairo_renderer_rep> (used by
  <cpp|qt_simple_widget_rep::get_renderer> when <cpp|USE_CAIRO> is
  defined) derives from <cpp|basic_renderer_rep>. It has not followed the
  recent evolution of the interface (shadows, transformations, pictures);
  treat it as a starting point rather than as working code. The former
  <cpp|aqua_renderer_rep> and <cpp|cg_renderer_rep> are gone: the
  <name|Cocoa> port now draws with <cpp|ns_renderer_rep>.

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
  shows how to map each primitive to a modern 2D graphics <abbr|API> (the
  <name|MuPDF> and <name|Cocoa> renderers do the same with <name|Fitz> and
  <name|CoreGraphics>), and
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
