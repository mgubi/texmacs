<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The renderer API>

  <section|Coordinates and units>

  <subsection|Logical units>

  All coordinates passed to a renderer are integers of type <cpp|SI>, the
  basic length unit of the typesetter. The constant <cpp|PIXEL> (defined as
  <cpp|256> in <verbatim|renderer.hpp> and <verbatim|pencil.hpp>) is the
  number of <cpp|SI> units in one dot of the typesetting resolution: when a
  document is typeset at <verbatim|dpi> dots per inch, one inch corresponds to
  <cpp|dpi*PIXEL> units (see for instance <cpp|make_eps> in
  <verbatim|Typeset/Boxes/Basic/boxes.cpp>, which computes
  <cpp|inch= dpi * PIXEL>).

  The logical <math|y>-axis points <em|upwards>, as in <TeX> and
  PostScript, whereas the device <math|y>-axis of screens and images points
  downwards. The conversion between both systems is done by the
  <cpp|encode> and <cpp|decode> routines below, which also flip the sign of
  <math|y>. Rectangles are therefore usually written as
  <cpp|(x1, y1, x2, y2)> with <cpp|x1\<less\>x2> and <cpp|y1\<less\>y2> in
  logical coordinates, and become <cpp|(x1, y2)> (top left) and
  <cpp|(x2, y1)> (bottom right) after decoding.

  <subsection|Origin>

  A renderer has a current origin <cpp|(ox, oy)>, expressed in <cpp|SI>. All
  drawing routines receive coordinates <em|relative> to this origin. Boxes
  exploit this: when a box is redrawn, it calls
  <cpp|ren-\<gtr\>move_origin (x0, y0)> with its own offset before painting
  its children and moves the origin back afterwards, so that each box paints
  itself in its own local coordinates.

  <\explain>
    <cpp|void set_origin (SI x, SI y)>

    <cpp|void move_origin (SI dx, SI dy)><explain-synopsis|set or translate
    the origin>
  <|explain>
    Set the origin to <cpp|(x, y)>, or add <cpp|(dx, dy)> to it. These
    routines are not virtual: the origin is pure bookkeeping, and the
    concrete renderers take it into account in <cpp|decode> and friends.
  </explain>

  <subsection|Pixels, zoom and shrinking>

  The relation between logical units and device pixels is described by the
  following public fields of <cpp|renderer_rep>:

  <\description>
    <item*|<cpp|pixel>>The number of <cpp|SI> units in one device pixel.

    <item*|<cpp|retina_pixel>>The number of <cpp|SI> units in one
    <em|logical> screen pixel, that is <cpp|pixel_ratio * pixel>. On a
    high-density (<abbr|e.g.> <name|Retina>) display a logical pixel covers
    several device pixels.

    <item*|<cpp|pixel_ratio>>The device pixel ratio passed to the
    constructor <cpp|renderer_rep (bool screen_flag, double pixel_ratio= 1)>.

    <item*|<cpp|zoomf>>The zoom factor, including the pixel ratio.

    <item*|<cpp|shrinkf>>The <em|shrinking factor>: the integer number of
    typesetting dots which are merged into one device pixel. It is the
    rounded value of <cpp|pixel_ratio * std_shrinkf / zoomf>.

    <item*|<cpp|thicken>>Extra thickening used when anti-aliasing
    characters, <cpp|(shrinkf \<gtr\>\<gtr\> 1) * PIXEL>.

    <item*|<cpp|brushpx>>A hack: <cpp|-1>, or the size of a pixel to be
    used when rendering patterns (see <reference|sec-renderer-fonts>).

    <item*|<cpp|is_screen>>Whether the renderer draws on the screen. Some
    boxes behave differently on screen and on paper (for instance they
    check for interruptions only on the screen).
  </description>

  The global variable <cpp|std_shrinkf> (defined in <verbatim|renderer.cpp>
  and equal to <cpp|5>) is the standard shrinking factor: at a zoom of
  <math|100%>, one screen pixel corresponds to five dots of the typesetting
  resolution. The editor keeps its own zoom factor in
  <cpp|edit_interface_rep::zoomf> and derives from it

  <\cpp-code>
    magf \ = zoomf / std_shrinkf;

    pixel = (SI) tm_round ((std_shrinkf * PIXEL) / zoomf);
  </cpp-code>

  (see <cpp|edit_interface_rep::set_zoom_factor> in
  <verbatim|Edit/Interface/edit_interface.cpp>).

  <\explain>
    <cpp|virtual void set_zoom_factor (double zoom, bool safe= true)><explain-synopsis|change
    the zoom factor>
  <|explain>
    Set <cpp|zoomf> to <cpp|pixel_ratio * zoom> and recompute
    <cpp|shrinkf>, <cpp|thicken>, <cpp|pixel> and <cpp|retina_pixel>. The
    origin and the clipping rectangle are rescaled so that they keep
    designating the same device pixels: they are first multiplied by the old
    zoom factor and then divided by the new one. If <cpp|safe> is set and the
    current <cpp|shrinkf> is inconsistent with <cpp|zoomf>, a warning
    <verbatim|Invalid zoom> is printed.

    The <name|Qt> renderer overrides this method: with <name|Qt> 6 it
    multiplies <cpp|zoom> by <cpp|zoom_multiplier> (used by
    <cpp|QTMImpressIconEngine> to scale icons), and with older versions of
    <name|Qt> it multiplies by the global <cpp|retina_factor> and sets
    <cpp|retina_pixel= pixel * retina_factor>.
  </explain>

  <\explain>
    <cpp|void reset_zoom_factor ()>

    <cpp|void set_shrinking_factor (int sf)><explain-synopsis|standard zoom
    factors>
  <|explain>
    <cpp|reset_zoom_factor> is <cpp|set_zoom_factor (std_shrinkf)>; with
    <cpp|pixel_ratio = 1> this sets <cpp|pixel = PIXEL>, so that one device
    pixel is one <cpp|SI>-dot. <cpp|set_shrinking_factor (sf)> sets the zoom
    to <cpp|std_shrinkf / sf>.
  </explain>

  The function <cpp|double normal_zoom (double zoom)> rounds a zoom factor
  down to the largest value of the form <math|320/n> (with <math|n> a
  positive integer) which does not exceed it, up to a small tolerance.

  <subsection|High density displays>

  <verbatim|renderer.hpp> declares a few global parameters for high density
  screens: <cpp|retina_factor> (the <name|MacOS> style integer factor),
  <cpp|retina_zoom> (the <name|GNU>/<name|Linux> and <name|Windows> style
  zoom), <cpp|retina_icons>, <cpp|retina_scale> and the flags
  <cpp|retina_manual>, <cpp|retina_iman>, with accessors
  <cpp|get_retina_factor>, <cpp|set_retina_factor>, <abbr|etc.> When
  compiling against <name|Qt> 6 (<cpp|QT_VERSION \<gtr\>= 0x060000>) these
  are compile-time constants equal to one, and the setters just print
  <verbatim|Unexpected call to set_retina_...>: with <name|Qt> 6 the device
  pixel ratio is passed explicitly to each renderer through
  <cpp|pixel_ratio> instead (see <cpp|the_qt_renderer (double pixel_ratio)>
  and <cpp|qt_simple_widget_rep::repaint_invalid_regions>).

  <subsection|Encoding, decoding and rounding>

  <\explain>
    <cpp|virtual void decode (SI& x, SI& y)>

    <cpp|virtual void decode (SI x, SI y, double& rx, double& ry)><explain-synopsis|logical
    to device coordinates>
  <|explain>
    Convert logical coordinates (relative to the origin) into device pixel
    coordinates. The integer version adds the origin, divides by
    <cpp|pixel> rounding towards <math|-\<infty\>>, and negates <math|y>:

    <\cpp-code>
      x += ox; y += oy;

      if (x\<gtr\>=0) x= x/pixel; else x= (x-pixel+1)/pixel;

      if (y\<gtr\>=0) y= -(y/pixel); else y= -((y-pixel+1)/pixel);
    </cpp-code>

    The floating point version returns <cpp|rx= (x+ox)/pixel - 0.5> and
    <cpp|ry= -((y+oy)/pixel - 0.5)>; it is used by the anti-aliased
    primitives of the <name|Qt> renderer, which address pixel centers.
  </explain>

  <\explain>
    <cpp|virtual void encode (SI& x, SI& y)>

    <cpp|virtual void encode (double x, double y, SI& rx, SI& ry)><explain-synopsis|device
    to logical coordinates>
  <|explain>
    The inverse conversion: <cpp|x= x*pixel - ox> and
    <cpp|y= (-y)*pixel - oy>. Window toolkits use it to translate the
    rectangles to be repainted into logical coordinates before calling the
    widget's <cpp|handle_repaint>.
  </explain>

  <\explain>
    <cpp|void round (SI& x, SI& y)>

    <cpp|void inner_round (SI& x1, SI& y1, SI& x2, SI& y2)>

    <cpp|void outer_round (SI& x1, SI& y1, SI& x2, SI& y2)><explain-synopsis|round
    to the pixel grid>
  <|explain>
    Round a point, respectively a rectangle, to the grid of (logical)
    pixels, taking the origin into account. <cpp|outer_round> enlarges the
    rectangle to the smallest pixel-aligned rectangle containing it and
    <cpp|inner_round> shrinks it to the largest one it contains. The grid
    used is that of <cpp|retina_pixel>. The friends <cpp|abs_round>,
    <cpp|abs_inner_round> and <cpp|abs_outer_round> do the same with respect
    to an absolute grid of <cpp|PIXEL> units, ignoring the origin.
  </explain>

  <section|Clipping>

  The visible region is the rectangle <cpp|(cx1, cy1, cx2, cy2)>. Contrary
  to the arguments of the drawing routines, these fields are stored in
  <em|absolute> coordinates, <abbr|i.e.> with the origin added, so that they
  remain valid when the origin is moved.

  <\explain>
    <cpp|virtual void get_clipping (SI &x1, SI &y1, SI &x2, SI &y2)>

    <cpp|virtual void set_clipping (SI x1, SI y1, SI x2, SI y2, bool
    restore= false)><explain-synopsis|get and set the clipping rectangle>
  <|explain>
    Get or set the clipping rectangle, in coordinates relative to the
    current origin. The default <cpp|set_clipping> applies
    <cpp|outer_round> and stores the result in <cpp|cx1>,
    <abbr|...>, <cpp|cy2>. Concrete renderers override it to also clip the
    underlying device (<cpp|QPainter::setClipRect>, a PostScript
    <verbatim|gsave>/<verbatim|clip>, a <abbr|PDF> <verbatim|q>/<verbatim|W>
    sequence, <abbr|etc.>).

    The <cpp|restore> flag tells the renderer that the new rectangle is a
    <em|previous> clipping region which is being restored, rather than a
    new, smaller one. This matters for the printers, whose clipping is a
    stack of graphics states: <cpp|printer_rep::set_clipping> emits a
    <verbatim|grestore> when <cpp|restore> is true and a <verbatim|gsave>
    followed by a new clip path otherwise; the <abbr|PDF> renderer does the
    same with <verbatim|Q> and <verbatim|q>, keeping track of the nesting in
    <cpp|clip_level>. Callers must therefore always pair a narrowing call with
    a restoring call.
  </explain>

  <\explain>
    <cpp|void extra_clipping (SI x1, SI y1, SI x2, SI y2)><explain-synopsis|intersect
    the clipping rectangle>
  <|explain>
    Intersect the current clipping rectangle with the given one. The typical
    usage, from <cpp|clip_box_rep> in
    <verbatim|Typeset/Boxes/Modifier/change_boxes.cpp>, is

    <\cpp-code>
      void

      clip_box_rep::pre_display (renderer &ren) {

      \ \ ren-\<gtr\>get_clipping (old_clip_x1, old_clip_y1, old_clip_x2,
      old_clip_y2);

      \ \ ren-\<gtr\>extra_clipping (x1, y1, x2, y2);

      }

      \;

      void

      clip_box_rep::post_display (renderer &ren) {

      \ \ ren-\<gtr\>set_clipping (

      \ \ \ \ old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2, true);

      }
    </cpp-code>
  </explain>

  <\explain>
    <cpp|void clip (SI x1, SI y1, SI x2, SI y2)>

    <cpp|void unclip ()><explain-synopsis|clipping stack>
  <|explain>
    <cpp|clip> pushes the current clipping rectangle on
    <cpp|clip_stack> and sets a new one; <cpp|unclip> pops it and restores
    it (with <cpp|restore= true>). They are used by the implementations of
    <cpp|set_transformation> and <cpp|reset_transformation>.
  </explain>

  <\explain>
    <cpp|bool is_visible (SI x1, SI y1, SI x2, SI y2)><explain-synopsis|visibility
    test>
  <|explain>
    Test whether the rectangle intersects the clipping region. Boxes use this
    test on their ink extents in order to skip invisible parts of the box
    tree.
  </explain>

  <subsection|Transformations>

  <\explain>
    <cpp|virtual void set_transformation (frame fr)>

    <cpp|virtual void reset_transformation ()><explain-synopsis|linear
    transformations>
  <|explain>
    Install a linear transformation (a <cpp|frame>, see
    <verbatim|Graphics/Types/frame.hpp>) for subsequent drawing, and remove it
    again. This is used by <cpp|transformed_box_rep::pre_display> and
    <cpp|post_display> for rotated and scaled boxes. The default
    implementation does nothing. The <name|Qt> and <abbr|PDF> renderers
    conjugate the frame with the logical-to-device conversion, apply the
    result to the device (<cpp|QPainter::setTransform>, respectively the
    <abbr|PDF> operator <verbatim|cm> inside <verbatim|q>...<verbatim|Q>),
    and push the transformed clipping rectangle with <cpp|clip>. Only linear
    frames are supported (this is asserted).
  </explain>

  <section|The graphical state>

  <subsection|Colors>

  Colors are values of type <cpp|color> (a packed <abbr|RGBA> value, see
  <verbatim|Graphics/Colors/colors.hpp>), built with <cpp|rgb_color> and
  decomposed with <cpp|get_rgb_color>. The alpha component is honored by the
  screen renderers; the PostScript renderer blends it with the current
  background color since PostScript has no transparency. The global
  function <cpp|get_reverse_colors> indicates reverse video mode, in which
  the renderers invert colors with <cpp|reverse (int& r, int& g, int& b)>.

  <subsection|Pencils>

  A <cpp|pencil> (<verbatim|Graphics/Renderer/pencil.hpp>) is a reference
  counted, immutable description of how lines and glyphs are drawn: a color
  or a brush, a width in <cpp|SI>, a cap style (<cpp|cap_square>,
  <cpp|cap_flat>, <cpp|cap_round>), a join style (<cpp|join_bevel>,
  <cpp|join_miter>, <cpp|join_round>) and a miter limit. Its kind,
  returned by <cpp|get_type ()>, is one of <cpp|pencil_none> (draw
  nothing), <cpp|pencil_simple> (color and width only),
  <cpp|pencil_standard> (color with explicit cap and join) and
  <cpp|pencil_brush> (the pencil paints with a pattern brush). The default
  width is <cpp|std_shrinkf * PIXEL>, <abbr|i.e.> one screen pixel at the
  standard zoom. Pencils are built from a <cpp|color>, a <cpp|brush> or a
  <cpp|tree> with an alpha value; since they are immutable,
  <cpp|set_width> and <cpp|set_cap> return new pencils.

  <subsection|Brushes>

  A <cpp|brush> (<verbatim|Graphics/Renderer/brush.hpp>) describes how areas
  are filled. Its kind is <cpp|brush_none>, <cpp|brush_color> or
  <cpp|brush_pattern>. A pattern brush is built from a
  <markup|pattern> tree <verbatim|(pattern url width height [color])> and an
  alpha value; <cpp|get_pattern_url> resolves the image file, looking in
  <verbatim|$TEXMACS_PATTERN_PATH> and relative to the current buffer, and
  <cpp|get_pattern_data> computes the image size in <cpp|SI> for a given
  pixel size. A brush built from the tree <verbatim|""> or
  <verbatim|"none">, or from a fully transparent color, is of kind
  <cpp|brush_none>.

  <subsection|The state accessors>

  <\explain>
    <cpp|virtual pencil get_pencil () = 0>

    <cpp|virtual void set_pencil (pencil p) = 0><explain-synopsis|current
    pencil>
  <|explain>
    Get and set the pencil used by <cpp|draw>, <cpp|line>, <cpp|lines>,
    <cpp|arc>, and, for the color, by <cpp|fill>, <cpp|fill_arc> and
    <cpp|polygon>. Implementations translate the pencil into device state
    (a <cpp|QPen> and <cpp|QBrush>, a PostScript color and line width,
    <abbr|etc.>) at this point. A <cpp|color> converts implicitly into a
    pencil, so <cpp|ren-\<gtr\>set_pencil (black)> is common.
  </explain>

  <\explain>
    <cpp|virtual brush get_brush ()>

    <cpp|virtual void set_brush (brush b)><explain-synopsis|current fill
    brush>
  <|explain>
    Get and set the brush used for filling. The default implementations
    simply go through the pencil: <cpp|set_brush (b)> calls
    <cpp|set_pencil (b-\<gtr\>get_color ())> and <cpp|get_brush ()> returns
    <cpp|get_pencil ()-\<gtr\>get_brush ()>. <cpp|basic_renderer_rep> keeps a
    separate <cpp|fg_brush> and resets the pencil to <cpp|pencil (b)>; the
    <name|Qt> and <abbr|PDF> renderers also support pattern brushes for
    fills.
  </explain>

  <\explain>
    <cpp|virtual brush get_background () = 0>

    <cpp|virtual void set_background (brush b) = 0><explain-synopsis|current
    background>
  <|explain>
    Get and set the background brush used by <cpp|clear> and
    <cpp|clear_pattern>. A <cpp|tree> converts implicitly into a brush, so
    the editor simply writes <cpp|ren-\<gtr\>set_background (bg)> with the
    value of the <src-var|bg-color> environment variable.
  </explain>

  Boxes which temporarily change the state are expected to restore it. For
  example <cpp|art_box_rep::pre_display> saves <cpp|get_background ()> and
  <cpp|get_pencil ()>, and <cpp|post_display> restores them.

  <section|Drawing primitives>

  In all the routines below, coordinates are logical and relative to the
  current origin.

  <\explain>
    <cpp|virtual void line (SI x1, SI y1, SI x2, SI y2) = 0>

    <cpp|virtual void lines (array\<less\>SI\<gtr\> x,
    array\<less\>SI\<gtr\> y) = 0><explain-synopsis|lines and polylines>
  <|explain>
    Draw a line segment, respectively an open polyline through the points
    <cpp|(x[i], y[i])>, with the current pencil. The <name|Qt> renderer
    draws them anti-aliased; for a closed polyline (first and last points
    equal) it forces round caps.
  </explain>

  <\explain>
    <cpp|virtual void fill (SI x1, SI y1, SI x2, SI y2) = 0><explain-synopsis|fill
    a rectangle>
  <|explain>
    Fill a rectangle with the pencil color. This is not anti-aliased. The
    <name|Qt> implementation widens rectangles that are thinner than one
    pixel to exactly one pixel, so that thin rules never disappear.
  </explain>

  <\explain>
    <cpp|virtual void clear (SI x1, SI y1, SI x2, SI y2) = 0><explain-synopsis|clear
    with the background color>
  <|explain>
    Fill a rectangle with the color of the current background brush. Pattern
    backgrounds are handled by <cpp|clear_pattern>.
  </explain>

  <\explain>
    <cpp|virtual void clear_pattern (SI mx1, SI my1, SI mx2, SI my2, SI x1,
    SI y1, SI x2, SI y2)>

    <cpp|virtual void clear_pattern (SI x1, SI y1, SI x2, SI y2)><explain-synopsis|clear
    with the background brush>
  <|explain>
    Clear the rectangle <cpp|(x1, y1, x2, y2)> with the current background
    brush. For a color brush this is <cpp|clear>; for a <cpp|brush_none>
    nothing happens. For a pattern brush, the pattern image is loaded as a
    <cpp|scalable> with <cpp|load_scalable_image> and tiled over the
    rectangle with <cpp|draw_scalable>, under an additional clipping. The
    <em|mother rectangle> <cpp|(mx1, my1, mx2, my2)> fixes the phase of the
    tiling and the meaning of percentages in the pattern sizes
    (<verbatim|100%>, <verbatim|50@>, <abbr|etc.>), so that a background
    image stays attached to the page when only a part of the page is
    repainted. The four argument version uses the rectangle itself as the
    mother rectangle.
  </explain>

  <\explain>
    <cpp|virtual void clear_device (SI x1, SI y1, SI x2, SI y2) =
    0><explain-synopsis|paint the device background>
  <|explain>
    Paint the neutral background of the device itself, behind the page. This
    is called by <cpp|edit_interface_rep::draw_background> before the page
    background. It is a no-op for printers and for <name|Qt> 5; with
    <name|Qt> 6 the <name|Qt> renderer paints a checkerboard pattern (the
    image <verbatim|neutral-pattern.png> if it can be found), which shows
    through transparent page backgrounds. Although declared pure virtual,
    <verbatim|renderer.cpp> also provides an empty body.
  </explain>

  <\explain>
    <cpp|virtual void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta)
    = 0>

    <cpp|virtual void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int
    delta) = 0><explain-synopsis|elliptic arcs>
  <|explain>
    Draw, respectively fill, the arc of the ellipse inscribed in the
    rectangle <cpp|(x1, y1, x2, y2)>, starting at angle <cpp|alpha> and
    spanning <cpp|delta>. As in <name|X11>, angles are expressed in
    <math|1/64> of a degree, counterclockwise; callers typically write
    <cpp|90\<less\>\<less\>6> for a right angle (see
    <verbatim|Typeset/Boxes/Basic/rubber_boxes.cpp>). The <name|Qt> renderer
    converts them to the <math|1/16> degrees of <cpp|QPainter::drawArc>
    and to degrees for <cpp|QPainterPath::arcTo>.
  </explain>

  <\explain>
    <cpp|virtual void polygon (array\<less\>SI\<gtr\> x,
    array\<less\>SI\<gtr\> y, bool convex=true) = 0><explain-synopsis|fill a
    polygon>
  <|explain>
    Fill a closed polygon with the current brush (or pencil color). In the
    <name|Qt> renderer the <cpp|convex> flag actually selects the fill rule:
    <cpp|Qt::OddEvenFill> when it is true and <cpp|Qt::WindingFill>
    otherwise.
  </explain>

  <\explain>
    <cpp|virtual void draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI
    y3)><explain-synopsis|fill a triangle>
  <|explain>
    Fill a triangle. The default calls <cpp|polygon>; the <name|Qt> version
    rounds the vertices to integer pixels and disables anti-aliasing, which
    gives crisp joins for the beveled borders drawn by
    <verbatim|Typeset/Boxes/Modifier/highlight_boxes.cpp>.
  </explain>

  <\explain>
    <cpp|virtual void draw_rectangles (rectangles rs)>

    <cpp|virtual void draw_selection (rectangles rs)><explain-synopsis|rectangle
    lists>
  <|explain>
    <cpp|draw_rectangles> fills each rectangle of the list.
    <cpp|draw_selection> draws a selection: the interior of the region is
    filled with the pencil color at roughly <math|1/16> of its opacity and
    the one-pixel wide border with the opaque color. The editor uses
    <cpp|draw_selection> only when compiled with <name|Qt>
    (<cpp|QTTEXMACS>), and <cpp|draw_rectangles> otherwise.
  </explain>

  <\explain>
    <cpp|virtual void draw_spacial (spacial obj)><explain-synopsis|three
    dimensional objects>
  <|explain>
    Draw a <cpp|spacial> object (<verbatim|Graphics/Spacial/spacial.hpp>)
    by calling <cpp|obj-\<gtr\>draw (this)>.
  </explain>

  <section|Text and fonts><label|sec-renderer-fonts>

  Fonts are described in <hlink|the fonts document|fonts.en.tm>; here we only
  describe the interface with the renderer. There is exactly one text
  primitive:

  <\explain>
    <cpp|virtual void draw (int char_code, font_glyphs fn, SI x, SI y) =
    0><explain-synopsis|draw one glyph>
  <|explain>
    Draw the glyph with index <cpp|char_code> of the bitmap font
    <cpp|fn> with its origin at <cpp|(x, y)>, using the current pencil.
    <cpp|font_glyphs> (<verbatim|Graphics/Bitmap_fonts/bitmap_font.hpp>)
    gives access to the rasterized glyphs through
    <cpp|glyph& get (int char_code)>; its <cpp|res_name> identifies the font
    and its size, which the printers use to build font resources.
  </explain>

  The path of a piece of text from the box tree to the device is as
  follows.

  <\enumerate>
    <item><cpp|text_box_rep::display> (in
    <verbatim|Typeset/Boxes/Basic/text_boxes.cpp>) sets the pencil and calls
    <cpp|fn-\<gtr\>draw (ren, str, 0, 0)> on its <cpp|font>.

    <item><cpp|font_rep::draw (renderer ren, string s, SI x, SI y, SI xk,
    bool ext)> (in <verbatim|Graphics/Fonts/font.cpp>) decides at which
    resolution to render. If <cpp|ren-\<gtr\>zoomf == 1.0> or the renderer
    is a printer, it calls <cpp|draw_fixed> directly. Otherwise it uses a
    magnified font <cpp|zoomed_fn= magnify (ren-\<gtr\>zoomf)> (cached as
    long as the zoom does not change) and temporarily reconfigures the
    renderer: the origin and clipping are multiplied by the zoom,
    <cpp|zoomf> is set to <cpp|1.0>, <cpp|shrinkf> to <cpp|std_shrinkf>,
    <cpp|pixel> and <cpp|retina_pixel> to <cpp|std_shrinkf * PIXEL>, and
    <cpp|brushpx> to the old pixel size (so that pattern pencils keep their
    on-screen scale). After drawing, all these fields are restored. The code
    is marked as a <verbatim|low level rendering hack>: renderers must not
    cache anything derived from these fields across calls to <cpp|draw>.

    <item><cpp|draw_fixed> of the concrete font (for instance
    <verbatim|Plugins/Freetype/tt_font.cpp>,
    <verbatim|Plugins/Freetype/unicode_font.cpp>,
    <verbatim|Graphics/Fonts/virtual_font.cpp> or the <verbatim|poor_*.cpp>
    fonts) computes glyph positions and calls
    <cpp|ren-\<gtr\>draw (c, fng, x, y)> for each glyph.

    <item>The renderer draws the glyph. Screen renderers rasterize it:
    the <name|Qt> renderer calls
    <cpp|shrink (glyph, std_shrinkf, std_shrinkf, xo, yo, pixel_ratio)> to
    obtain an anti-aliased glyph at screen resolution, converts it into a
    colored <cpp|QImage> (or <cpp|QTMPixmapOrImage>), and caches the result
    in the static table <cpp|character_image>, keyed by a
    <cpp|basic_character> (glyph index, font, shrinking factor, foreground
    and background color). Glyphs drawn with a <cpp|pencil_brush> pencil are
    handled by <cpp|qt_renderer_rep::draw_bis>, which uses the glyph as an
    alpha mask over the pattern. Printers embed fonts instead, see
    <hlink|the renderer implementations|renderer-backends.en.tm>.
  </enumerate>

  One font bypasses the glyph interface: <cpp|qt_font_rep::draw_fixed>
  (<verbatim|Plugins/Qt/qt_font.cpp>) recovers the <name|Qt> renderer with
  <cpp|ren-\<gtr\>get_handle ()> and calls the extra method
  <cpp|qt_renderer_rep::draw (const QFont& qfn, const QString& s, SI x, SI y,
  double zoom)>, which renders the string with <cpp|QPainter::drawText>.

  <section|Pictures, scalable images and off-screen rendering>

  <subsection|Pictures>

  A <cpp|picture> (<verbatim|Graphics/Pictures/picture.hpp>) is a reference
  counted rectangular array of pixels with an origin. Its kind is
  <cpp|picture_native> (a picture of the <abbr|GUI> toolkit: a
  <cpp|qt_picture_rep> wrapping a <cpp|QImage>, an <cpp|x_picture_rep>
  wrapping an <name|X11> <cpp|Pixmap>), <cpp|picture_raster> (a portable
  <cpp|raster_picture_rep\<less\>C\<gtr\>> wrapping a
  <cpp|raster\<less\>C\<gtr\>>, by default with <cpp|true_color> pixels) or
  <cpp|picture_lazy>. Pixels are accessed with <cpp|get_pixel> and
  <cpp|set_pixel> in coordinates relative to the origin
  (<cpp|get_origin_x>, <cpp|get_origin_y>); <cpp|as_raster_picture> and
  <cpp|as_native_picture> convert between the representations. Each
  picture has a unique identifier, <cpp|get_unique_id ()>, which the
  <abbr|PDF> renderer uses to cache exported pictures.

  The header also declares a library of operations on pictures used to
  implement graphical effects: composition (<cpp|compose>, <cpp|draw_on>
  with a <cpp|composition_mode>), geometric operations (<cpp|shift>,
  <cpp|magnify>, <cpp|crop>), pens and morphological operations
  (<cpp|gaussian_pen_picture>, <cpp|blur>, <cpp|outlines>, <cpp|thicken>,
  <cpp|erode>), noise and distortions, and color operations. The class
  <cpp|effect_rep> (<verbatim|Graphics/Pictures/effect.hpp>) combines them:
  <cpp|effect build_effect (tree description)> parses an effect and
  <cpp|picture apply (array\<less\>picture\<gtr\> pics, SI pixel)> computes
  the resulting picture from pictures of its arguments.

  Pictures on disk are loaded with
  <cpp|load_picture (url u, int w, int h, tree eff, int pixel)> and, with
  caching, <cpp|cached_load_picture>; the entries of the cache are reserved
  and released with <cpp|picture_cache_reserve> and
  <cpp|picture_cache_release>. <cpp|load_xpm> loads the <verbatim|xpm>
  icons of the interface. <cpp|save_picture> writes a picture to a file,
  and <cpp|picture_as_eps> converts it to <abbr|EPS>.

  <\explain>
    <cpp|virtual void draw_picture (picture pic, SI x, SI y, int alpha=
    255)><explain-synopsis|draw a picture>
  <|explain>
    Draw a picture with its origin at <cpp|(x, y)> and the given opacity.
    The default implementation fails with
    <verbatim|rendering pictures is not supported>, so every renderer on
    which images may appear must override it. The <name|Qt> renderer
    converts the picture with <cpp|as_native_picture> and calls
    <cpp|QPainter::drawImage>; the printers encode it as an image in the
    output file.
  </explain>

  <subsection|Scalable images>

  A <cpp|scalable> (<verbatim|Graphics/Pictures/scalable.hpp>) is an image
  that is independent of the resolution, with logical and physical extents
  and a method <cpp|draw (renderer ren, SI x, SI y, int alpha)>. The only
  implementation is <cpp|scalable_image_rep>, created by
  <cpp|load_scalable_image (url file_name, SI w, SI h, tree eff, SI pixel)>,
  which represents an image file with a size and an optional effect. Its
  <cpp|draw> method loads a picture at the renderer's current <cpp|pixel>
  size with <cpp|cached_load_picture> and calls <cpp|draw_picture>.

  <\explain>
    <cpp|virtual void draw_scalable (scalable im, SI x, SI y, int alpha=
    255)><explain-synopsis|draw a scalable image>
  <|explain>
    The default implementation calls <cpp|im-\<gtr\>draw (this, x, y,
    alpha)>, <abbr|i.e.> it rasterizes. Printers override it in order to
    embed the original file without rasterization when possible: the
    PostScript renderer inserts the PostScript code of the image, and the
    <abbr|PDF> renderer embeds the image file (only if no effect is
    attached; otherwise it falls back on the default).
  </explain>

  <subsection|Rendering into pictures>

  It is possible to redirect drawing into a picture. This is used for
  graphical effects, for exporting snippets as bitmaps and for rendering
  icons.

  <\explain>
    <cpp|renderer picture_renderer (picture p, double zoom)><explain-synopsis|a
    renderer drawing on a picture>
  <|explain>
    Return a renderer which draws on <cpp|p>. It must be deleted with
    <cpp|delete_renderer> or <cpp|tm_delete> when drawing is finished. This
    function, as well as <cpp|native_picture>, <cpp|load_picture>,
    <cpp|as_native_picture> and <cpp|save_picture>, is provided by the
    <abbr|GUI> back-end (<verbatim|Plugins/Qt/qt_picture.cpp> or
    <verbatim|Plugins/X11/x_picture.cpp>); <verbatim|renderer.cpp> only
    contains failing stubs for builds without <cpp|QTTEXMACS> and
    <cpp|X11TEXMACS>. For <name|Qt> the returned renderer is a
    <cpp|qt_image_renderer_rep>, which opens a <cpp|QPainter> on the
    <cpp|QImage> of the picture after clearing it to transparent.
  </explain>

  <\explain>
    <cpp|virtual renderer shadow (picture& pic, SI x1, SI y1, SI x2, SI
    y2)><explain-synopsis|off-screen copy of a region>
  <|explain>
    Create a new native picture covering the rectangle
    <cpp|(x1, y1, x2, y2)> of the current renderer (after
    <cpp|outer_round>), store it in <cpp|pic>, and return a picture renderer
    for it which has the same origin, clipping, zoom and pixel parameters as
    the current renderer, translated so that drawing at the same logical
    coordinates lands in the picture. Printers override this method to
    rasterize at a fixed resolution: they temporarily set their zoom factor
    to <cpp|5.0 * PICTURE_ZOOM> before calling the default implementation.

    The typical client is <cpp|effect_box_rep::redraw>:

    <\cpp-code>
      array\<less\>picture\<gtr\> pics (subnr ());

      SI shad_pixel= ren-\<gtr\>pixel;

      for (int i=0; i\<less\>subnr(); i++) {

      \ \ renderer shad= ren-\<gtr\>shadow (pics[i], sx3(i), sy3(i), sx4(i),
      sy4(i));

      \ \ shad_pixel= shad-\<gtr\>pixel;

      \ \ rectangles rs;

      \ \ subbox (i)-\<gtr\>redraw (shad, path (), rs);

      \ \ delete_renderer (shad);

      }

      ...

      picture result_pic= eff-\<gtr\>apply (pics, shad_pixel);

      ren-\<gtr\>draw_picture (result_pic, 0, 0);
    </cpp-code>

    The overload <cpp|shadow (scalable& im, ...)> and the function
    <cpp|scalable_renderer> are not implemented.
  </explain>

  <section|Shadows and double buffering>

  The word <em|shadow> designates an auxiliary renderer of the same size as
  a screen renderer, which is used for double buffering: the editor draws a
  region into the shadow and then copies the result to the screen in one
  go, which avoids flickering. The design comes from the <name|X11> port,
  where the shadow is an off-screen <cpp|Pixmap>.

  <\explain>
    <cpp|virtual void new_shadow (renderer& ren) = 0>

    <cpp|virtual void delete_shadow (renderer& ren) = 0><explain-synopsis|create
    and destroy a shadow>
  <|explain>
    <cpp|new_shadow> makes sure that <cpp|ren> is a shadow compatible with
    the current renderer. If <cpp|ren> is not <cpp|NULL> and has the same
    extents (and, for <name|Qt>, the same pixel ratio), it is reused;
    otherwise it is deleted and a new one is allocated. The editor keeps its
    shadows in the fields <cpp|shadow> and <cpp|stored> of
    <cpp|edit_interface_rep> and calls <cpp|new_shadow> before every use.
    <cpp|delete_shadow> deletes <cpp|ren> and sets it to <cpp|NULL>.
  </explain>

  <\explain>
    <cpp|virtual void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2)
    = 0><explain-synopsis|prepare a shadow for drawing>
  <|explain>
    Copy the rectangle <cpp|(x1, y1, x2, y2)> of the current renderer into
    the shadow <cpp|ren>, and give the shadow the same origin, a clipping
    rectangle equal to this rectangle (intersected with the current
    clipping), and set its <cpp|master> field to the current renderer.
  </explain>

  <\explain>
    <cpp|virtual void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2)
    = 0><explain-synopsis|copy a shadow back>
  <|explain>
    Copy the rectangle <cpp|(x1, y1, x2, y2)> of the shadow <cpp|ren> onto
    the current renderer.
  </explain>

  <\explain>
    <cpp|virtual void apply_shadow (SI x1, SI y1, SI x2, SI y2) =
    0><explain-synopsis|flush part of a shadow to its master>
  <|explain>
    Called on a shadow: copy the given rectangle to the <cpp|master>
    renderer, via <cpp|master-\<gtr\>put_shadow>. <cpp|box_rep::redraw>
    calls <cpp|ren-\<gtr\>apply_shadow> for the first fifteen boxes painted
    (<cpp|nr_painted \<less\> 15>), so that the first parts of the document
    become visible immediately even though the rest is drawn into the
    shadow.
  </explain>

  <\explain>
    <cpp|virtual void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x,
    SI y) = 0><explain-synopsis|copy a region between renderers>
  <|explain>
    Copy a rectangle of <cpp|ren> to position <cpp|(x, y)> of the current
    renderer. Only the <name|X11> renderer implements it (with
    <cpp|XCopyArea>); the other renderers leave it empty.
  </explain>

  <subsection|Shadows with <name|Qt>>

  <name|Qt> already double-buffers, and a <name|Qt> program cannot reliably
  read back pixels from the screen. The <name|Qt> port therefore keeps its
  own backing store: each <cpp|qt_simple_widget_rep> owns a
  <cpp|backingPixmap>, the <name|TeXmacs> side only ever paints on this
  pixmap, and the <cpp|QWidget> copies it to the screen in its paint event.
  On top of this, the comment in <verbatim|qt_renderer.cpp> explains that
  two helper classes emulate the shadow protocol:

  <\itemize>
    <item><cpp|qt_renderer_rep::new_shadow> returns a
    <cpp|qt_proxy_renderer_rep>, which draws with the same <cpp|QPainter> as
    the original renderer. The main double buffering therefore costs
    nothing: <cpp|put_shadow> and <cpp|apply_shadow> return immediately when
    both renderers share their painter.

    <item><cpp|qt_proxy_renderer_rep::new_shadow> (a shadow of a shadow,
    such as the editor's <cpp|stored> backing store) creates a genuine
    <cpp|qt_shadow_renderer_rep>, which owns a <cpp|QTMPixmapOrImage>
    <cpp|px> and its own <cpp|QPainter>. Its <cpp|get_shadow> and the proxy's
    <cpp|get_shadow> copy pixels with <cpp|drawPixmap> (or
    <cpp|drawImage> in <verbatim|headless_mode>).
  </itemize>

  The basic screen renderer class <cpp|basic_renderer_rep> implements all
  shadow operations as no-ops (<cpp|new_shadow> just returns
  <cpp|this>), which is a valid, if unbuffered, choice.

  <section|Services for printers>

  The following methods have trivial default implementations in
  <cpp|renderer_rep> and are overridden by the PostScript and <abbr|PDF>
  renderers.

  <\explain>
    <cpp|virtual bool is_printer ()><explain-synopsis|is this a printer?>
  <|explain>
    Returns <cpp|false> by default. Boxes test it to suppress screen-only
    decorations (for instance in <verbatim|highlight_boxes.cpp> and in the
    <markup|screen>/<markup|printer> filters of
    <verbatim|decoration_boxes.cpp>), and <cpp|font_rep::draw> uses it to
    bypass zooming.
  </explain>

  <\explain>
    <cpp|virtual bool is_started ()><explain-synopsis|initialization
    succeeded>
  <|explain>
    Returns <cpp|true> by default. The <abbr|PDF> renderer returns
    <cpp|false> if the output file could not be opened, in which case
    <cpp|print_doc> draws nothing.
  </explain>

  <\explain>
    <cpp|virtual void next_page ()>

    <cpp|virtual void set_page_nr (int nr)><explain-synopsis|pages>
  <|explain>
    <cpp|next_page> finishes the current page and starts the next one;
    printers reset their cached graphical state (current color, line width,
    font) and the clipping to the full page at that moment.
    <cpp|set_page_nr> sets <cpp|cur_page>; it is called by
    <cpp|page_box_rep::pre_display> and <cpp|post_display>, so that
    decorations can depend on the parity of the page.
  </explain>

  <\explain>
    <cpp|virtual void get_extents (SI& w, SI& h)><explain-synopsis|size of
    the device>
  <|explain>
    The size of the device in pixels. It is <cpp|0, 0> by default;
    <cpp|basic_renderer_rep> returns its fields <cpp|w> and <cpp|h>, and the
    <name|Qt> renderer the size of the paint device. It is used to decide
    whether a shadow can be reused.
  </explain>

  <\explain>
    <cpp|virtual void anchor (string label, SI x1, SI y1, SI x2, SI y2)>

    <cpp|virtual void href (string label, SI x1, SI y1, SI x2, SI y2)>

    <cpp|virtual void toc_entry (string kind, string title, SI x, SI y)>

    <cpp|virtual void set_metadata (string kind, string val)><explain-synopsis|hyperlinks,
    outline and metadata>
  <|explain>
    Record a link target, a clickable link area, an entry of the document
    outline and a metadata field (<verbatim|title>, <verbatim|author>,
    <verbatim|subject>). Boxes call the first three unconditionally, the
    screen renderers simply ignoring them: <cpp|locus_box_rep::post_display>
    (in <verbatim|change_boxes.cpp>) emits <cpp|href> and <cpp|anchor>, the
    table of contents boxes of <verbatim|decoration_boxes.cpp> emit
    <cpp|toc_entry>, and <cpp|box_rep::display_links>, which
    <cpp|box_rep::redraw> only calls for non-screen renderers, emits
    <cpp|href> for hyperlinks attached to the source of the box.
    <cpp|print_doc> calls <cpp|set_metadata>. The PostScript renderer
    emits <verbatim|pdfmark> style annotations; the <abbr|PDF> renderer
    writes native annotations, destinations and outlines.
  </explain>

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
