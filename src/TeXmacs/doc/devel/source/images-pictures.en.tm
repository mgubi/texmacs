<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Pictures, effects and picture caches>

  The class <cpp|picture>, its kinds, its pixel access and its conversion
  functions are introduced in <hlink|the renderer API|renderer-api.en.tm>
  (section \Ppictures, scalable images and off-screen rendering\Q). This page
  adds the implementation side: the portable raster pictures, the effect
  engine, scalable images and the three caches through which image files
  reach the screen.

  <section|Raster pictures>

  <cpp|raster\<less\>C\<gtr\>> (<source-link|Graphics/Pictures/raster.hpp|src/Graphics/Pictures/raster.hpp>) is a
  reference counted array of pixels of type <cpp|C> with a width <cpp|w>, a
  height <cpp|h> and an origin <cpp|(ox, oy)>; the pixels are stored row by
  row in <cpp|a>. The header implements, as templates, the generic
  operations on such arrays: mapping a pixel operator over a raster,
  composing two rasters with one of the operators of
  <source-link|raster_operators.hpp|src/Graphics/Pictures/raster_operators.hpp>, shifting, magnifying, convolving with a
  pen, and so on. The pixel type used in practice is <cpp|true_color>
  (<source-link|Graphics/Colors/true_color.hpp|src/Graphics/Colors/true_color.hpp>), four doubles with
  non-premultiplied alpha.

  <cpp|raster_picture_rep\<less\>C\<gtr\>>
  (<source-link|raster_picture.hpp|src/Graphics/Pictures/raster_picture.hpp>) wraps a raster as a <cpp|picture>. Pixels
  outside the raster read as transparent black and writes to them are
  ignored. Two helpers convert between the representations:
  <cpp|raster_picture (raster\<less\>C\<gtr\>)> wraps a raster, and
  <cpp|as_raster\<less\>C\<gtr\> (picture)> returns the raster of a picture,
  copying it pixel by pixel into a new raster picture if the picture is of
  another kind (for instance a native <name|Qt> picture). The non template
  <cpp|raster_picture (w, h, ox, oy)> creates an empty
  <cpp|true_color> picture.

  <source-link|raster_picture.cpp|src/Graphics/Pictures/raster_picture.cpp> implements all the picture operations
  declared in <source-link|picture.hpp|src/Graphics/Pictures/picture.hpp> (<cpp|compose>, <cpp|draw_on>,
  <cpp|shift>, <cpp|magnify>, <cpp|blur>, <cpp|thicken>, <cpp|color_matrix>,
  <cpp|make_transparent>, ...) by converting their arguments with
  <cpp|as_raster\<less\>true_color\<gtr\>>, calling the raster template and
  wrapping the result. Every operation therefore returns a fresh raster
  picture, whatever the kind of its arguments. <source-link|raster_random.cpp|src/Graphics/Pictures/raster_random.cpp>
  contains the random functions behind <cpp|turbulence>,
  <cpp|fractal_noise>, <cpp|degrade>, <cpp|distort> and <cpp|gnaw>.

  Native pictures are cheaper to draw on the screen: <cpp|as_native_picture>
  converts a raster picture back into a picture of the <abbr|GUI> back-end
  (<cpp|qt_picture_rep> wrapping a <cpp|QImage>). A typical effect thus
  converts native pictures to rasters, computes on rasters, and lets the
  renderer convert the result back when it draws it.

  <section|Effects>

  An <em|effect> is a function from a list of pictures to a picture. It is
  described in documents by a tree built from the <verbatim|eff-*> tags,
  for instance

  <\verbatim-code>
    \<less\>eff-blur\|\|\<less\>eff-gaussian\|2px\<gtr\>\<gtr\>
  </verbatim-code>

  which blurs its first argument with a Gaussian pen. Effects are used in
  two places: as the last argument of the <markup|gr-effect> primitive,
  which applies them to typeset content (through <cpp|effect_box>, see
  <hlink|the renderer API|renderer-api.en.tm>), and as the effect of a
  background pattern (see <hlink|images in
  documents|images-typesetting.en.tm>).

  <\explain>
    <cpp|class effect_rep><explain-synopsis|an effect>
  <|explain>
    Declared in <source-link|Graphics/Pictures/effect.hpp|src/Graphics/Pictures/effect.hpp>; <cpp|effect> is the
    corresponding <cpp|ABSTRACT_NULL> handle. Its methods are

    <\description>
      <item*|<cpp|rectangle get_logical_extents (array\<less\>rectangle\<gtr\>
      rs)>>The logical extents of the result, given those of the arguments;
      by default the extents of the first argument.

      <item*|<cpp|rectangle get_extents (array\<less\>rectangle\<gtr\>
      rs)>>The ink extents of the result; for instance a blur makes the
      result larger than its argument by the radius of the pen.

      <item*|<cpp|picture apply (array\<less\>picture\<gtr\> pics, SI
      pixel)>>Compute the resulting picture. <cpp|pixel> is the size of a
      pixel of the pictures in logical units, which is needed to convert
      lengths of the effect (such as the radius of a pen) into pixels.
    </description>
  </explain>

  <cpp|build_effect (tree t)> (<source-link|effect.cpp|src/Graphics/Pictures/effect.cpp>) translates a
  description into an effect object. The empty string stands for the first
  argument (<cpp|argument_effect (0)>) and an integer <math|i> for the
  argument number <math|i>; compound trees are dispatched on their label:

  <\description>
    <item*|Geometry><markup|eff-move>, <markup|eff-magnify>,
    <markup|eff-bubble>, <markup|eff-crop>.

    <item*|Pens><markup|eff-gaussian>, <markup|eff-oval>,
    <markup|eff-rectangular> (with one radius, or two radii and an angle)
    and <markup|eff-motion>. Pens are effects without arguments which
    produce the pen picture.

    <item*|Morphology><markup|eff-blur>, <markup|eff-outline>,
    <markup|eff-thicken>, <markup|eff-erode>, each taking an effect and a
    pen.

    <item*|Textures and distortions><markup|eff-turbulence>,
    <markup|eff-fractal-noise>, <markup|eff-hatch>, <markup|eff-dots>,
    <markup|eff-degrade>, <markup|eff-distort>, <markup|eff-gnaw>.

    <item*|Composition><markup|eff-superpose>, <markup|eff-add>,
    <markup|eff-sub>, <markup|eff-mul>, <markup|eff-min>,
    <markup|eff-max> (any number of effects) and <markup|eff-mix>.

    <item*|Colors><markup|eff-normalize>, <markup|eff-monochrome>,
    <markup|eff-color-matrix>, <markup|eff-gradient>,
    <markup|eff-make-transparent>, <markup|eff-make-opaque>,
    <markup|eff-recolor>, <markup|eff-skin>. <markup|eff-monochrome> and
    <markup|eff-gradient> are translated into color matrices.
  </description>

  An unrecognized tree yields the first argument unchanged, so a mistyped
  effect is silently ignored rather than reported.

  Each effect class (<cpp|blur_effect_rep>, <cpp|compose_effect_rep>, ...)
  first applies its sub-effects to the argument pictures and then calls the
  corresponding picture operation of <source-link|raster_picture.cpp|src/Graphics/Pictures/raster_picture.cpp>.

  <section|Scalable images>

  A <cpp|scalable> (<source-link|Graphics/Pictures/scalable.hpp|src/Graphics/Pictures/scalable.hpp>) is described
  in <hlink|the renderer API|renderer-api.en.tm>. Its only implementation,
  <cpp|scalable_image_rep> (<source-link|scalable.cpp|src/Graphics/Pictures/scalable.cpp>), holds an image file
  <cpp|u>, a size <cpp|(w, h)> in logical units, an effect <cpp|eff> and the
  pixel size <cpp|px> at which it was last drawn. Its logical and physical
  extents are those of the effect applied to the rectangle
  <cpp|(0, 0, w, h)>, computed once with <cpp|build_effect>.

  Its <cpp|draw (ren, x, y, alpha)> method loads the picture at the size
  <cpp|w/ren-\<gtr\>pixel> <math|\<times\>> <cpp|h/ren-\<gtr\>pixel> pixels
  with <cpp|cached_load_picture> and draws it with <cpp|draw_picture>.
  Because the size in pixels depends on the zoom factor, each zoom level
  produces a different picture. The constructor and destructor of
  <cpp|scalable_image_rep>, and <cpp|draw> when the pixel size changes,
  reserve and release the corresponding cache entry (see below).

  <section|The picture caches>

  Three caches are involved when an image file is drawn on the screen:

  <\description>
    <item*|The size cache>The table <cpp|img_box> of
    <source-link|System/Files/image_files.cpp|src/System/Files/image_files.cpp> maps the tree of an image
    <abbr|URL> to its size in points and, for PostScript files, the origin
    of its bounding box. It is filled by <cpp|image_size>,
    <cpp|ps_bounding_box> and <cpp|gs_image_size> and cleared entry by entry
    with <cpp|clear_imgbox_cache>, or completely with
    <cpp|clearall_imgbox_cache>.

    <item*|The picture cache>The tables of <source-link|picture.cpp|src/Graphics/Pictures/picture.cpp> map a key
    <verbatim|(url, w, h, effect)>, where <verbatim|w> and <verbatim|h> are
    sizes in pixels, to the loaded <cpp|picture> and to the modification
    time of the file when it was loaded (<cpp|picture_stamp>). A reference
    count per key (<cpp|picture_count>), maintained by
    <cpp|picture_cache_reserve> and <cpp|picture_cache_release>, tells which
    pictures are still used by some image box; keys whose count dropped to
    zero are put on a black list.

    <item*|The <name|Qt> image cache>The table <cpp|qt_pic_cache> of
    <source-link|Plugins/Qt/qt_picture.cpp|src/Plugins/Qt/qt_picture.cpp> maps
    <verbatim|(url, w, h)> (plus the effect and the pixel size when there is
    an effect) to the <cpp|QImage> read from disk.
  </description>

  <cpp|cached_load_picture (u, w, h, eff, pixel, permanent)> returns the
  cached picture if there is one and the file has not been modified since
  it was loaded; if the file has changed, it also clears the size cache
  entry of the file, since its size may have changed. Otherwise it calls
  the back-end function <cpp|load_picture (u, w, h, eff, pixel)> and stores
  the result if <cpp|permanent> is set or if the key is reserved.
  <cpp|scalable_image_rep::draw> passes <cpp|permanent= false>, so that only
  pictures of image boxes which still exist are kept.

  <cpp|picture_cache_clean ()> is called after each typesetting pass
  (<cpp|edit_typeset_rep::typeset_sub>, in <source-link|Edit/Editor/edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp>).
  At most once per minute, it removes the black listed entries whose count
  is still zero. <cpp|picture_cache_reset ()> empties the picture cache, the
  size cache and the <name|Qt> image cache; it is exported as
  <scm|picture-cache-reset> and called by <scm|picture-gc> in
  <source-link|texmacs/texmacs/tm-tools.scm|TeXmacs/progs/texmacs/texmacs/tm-tools.scm>.

  With <name|Qt>, <cpp|load_picture> (<source-link|qt_picture.cpp|src/Plugins/Qt/qt_picture.cpp>) calls
  <cpp|get_image>, which looks in <cpp|qt_pic_cache> and otherwise calls
  <cpp|get_image_for_real>:

  <\enumerate>
    <item>for <abbr|SVG> files with <cpp|USE_RESVG>, the file is rendered
    by <name|resvg> directly at the requested size;

    <item>otherwise, a format which <name|Qt> can read
    (<cpp|qt_supports>) is loaded into a <cpp|QImage>; any other format is
    first converted to a temporary <name|PNG> file with <cpp|image_to_png>;

    <item>the image is scaled to the requested size (ignoring the aspect
    ratio, which the typesetter has already taken into account);

    <item>if there is an effect, it is built with <cpp|build_effect> and
    applied, with the image as its only argument.
  </enumerate>

  If the image cannot be read, <cpp|load_picture> returns
  <cpp|error_picture (w, h)>, a translucent red rectangle. The <name|X11>
  version of <cpp|load_picture> (<source-link|Plugins/X11/x_picture.cpp|src/Plugins/X11/x_picture.cpp>)
  renders the file into a pixmap with <name|Imlib2> if possible and with
  <name|Ghostscript> otherwise (<cpp|ghostscript_run>, which converts the
  image to PostScript first).

  <section|Icons>

  The icons of menus and toolbars are loaded with <cpp|load_xpm (url)>,
  which caches them by name. With <name|Qt> it calls <cpp|qt_load_xpm>,
  which prefers a <name|PNG> version of the icon (<verbatim|_x2.png> or
  <verbatim|_x4.png> on high density screens) in
  <verbatim|$TEXMACS_PIXMAP_PATH>, falls back on the <verbatim|xpm> file and
  finally on <verbatim|TeXmacs.xpm>. With a style sheet, the icons are
  rescaled to the interface scale, and with a dark style sheet their colors
  are inverted (except for the flags of languages). Without <name|Qt>,
  <cpp|load_xpm> parses the <verbatim|xpm> file itself
  (<cpp|xpm_load> in <source-link|image_files.cpp|src/System/Files/image_files.cpp>).

  <section|Saving pictures>

  <cpp|save_picture (dest, p)> writes a picture to a file in the format
  given by the suffix (with <name|Qt>; the <name|X11> version does nothing).
  <cpp|picture_as_eps (p, dpi)> encodes a picture as an <abbr|EPS> image
  with an <verbatim|ASCIIHexDecode> data stream and, if the picture has
  transparent pixels, a one bit mask (pixels with an opacity of at most 32
  are masked); colors are first blended with white according to their
  opacity. It is used by the PostScript printer to draw pictures.
  <cpp|apply_effect (eff, src, dest, w, h)> (<source-link|image_files.cpp|src/System/Files/image_files.cpp>,
  exported as <scm|apply-effect>) loads image files at a given size,
  applies an effect to them and saves the result; it only works with
  <name|Qt> (<cpp|qt_apply_effect>).

  <section|Pitfalls>

  <\itemize>
    <item>The <name|Qt> image cache <cpp|qt_pic_cache> has no expiry and
    does not record modification times. When an image file is modified,
    <cpp|picture_is_cached> notices it and calls <cpp|load_picture> again,
    but <cpp|get_image> then returns the old <cpp|QImage> from its own
    cache. The new contents only appear after <scm|picture-gc>. The cache
    also keeps one <cpp|QImage> per size in pixels, that is, per zoom level,
    for the whole session.

    <item>In <cpp|build_effect>, the test for <markup|eff-color-matrix>
    reads <verbatim|NR (m) != 4 && NC (m) != 5>
    (<source-link|Graphics/Pictures/effect.cpp:783|src/Graphics/Pictures/effect.cpp:783>); it should use
    <verbatim|\|\|>. A matrix with four rows and fewer than five columns
    passes the test and is then read out of bounds.

    <item>The cases for <markup|eff-make-transparent>,
    <markup|eff-make-opaque>, <markup|eff-recolor>, <markup|eff-skin> and
    <markup|eff-normalize> (<verbatim|effect.cpp:764-822>) do not check the
    arity of the tag before reading <verbatim|t[1]> (or <verbatim|t[0]>), so
    a tag with too few children makes the effect parser read past the end
    of the tree.

    <item><source-link|effect.hpp|src/Graphics/Pictures/effect.hpp> declares <cpp|gaussian_pen>,
    <cpp|oval_pen>, <cpp|rectangular_pen>, <cpp|motion_pen> and
    <cpp|outline>, but <source-link|effect.cpp|src/Graphics/Pictures/effect.cpp> defines
    <cpp|gaussian_pen_effect>, ..., <cpp|motion_pen_effect> and
    <cpp|outlines> instead; the declared names have no definition, and a
    call to them fails at link time.

    <item>Effects are computed on <cpp|true_color> rasters, pixel by pixel,
    by portable code; large blurred or shadowed areas are therefore slow,
    especially at high zoom factors and in printers, which rasterize
    shadows at a fixed high resolution.
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
