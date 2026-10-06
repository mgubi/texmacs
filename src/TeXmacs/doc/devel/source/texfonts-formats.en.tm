<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|TFM metrics, PK glyphs and Type 1 substitution>

  This page describes how a <TeX> font of a given family, size and resolution
  is loaded (<cpp|load_tex> in <source-link|Plugins/Metafont/load_tex.cpp|src/Plugins/Metafont/load_tex.cpp>),
  how its metrics and glyphs are decoded, and how the glyphs are represented
  and prepared for display.

  <section|Loading a font>

  <\explain>
    <cpp|void load_tex (string family, int size, int dpi, int dsize,
    tex_font_metric& tfm, font_glyphs& pk)><explain-synopsis|metrics and
    glyphs of a <TeX> font>
  <|explain>
    Loads the metric (<cpp|load_tex_tfm>) and then the glyphs
    (<cpp|load_tex_pk>) of <verbatim|<em|family><em|size>> at <cpp|dpi>
    dots per inch. <cpp|dsize> is the design size to fall back on (10 by
    default; 0 means \Pthe design size stored in the metric\Q). If the font
    cannot be found, <verbatim|ecrm> at the same size is tried instead; if
    this fails too, the program stops with <cpp|FAILED ("Tex seems not to be
    installed properly")>.

    After loading, <cpp|rubber_fix> marks the pieces of all extensible
    characters (top, middle, bottom and repeated parts) with a
    <cpp|status> (1 for a bottom piece, 2 for a top piece, 3 for middle and
    repeated pieces) and resets their vertical offset; the status is used
    when the glyphs are shrunk for display, see below.
  </explain>

  <paragraph|Sizes.>For some <LaTeX> fonts, non-integer sizes are encoded by
  multiplying the size by 100: <verbatim|larm1050> is <verbatim|larm> at
  10.5pt, and <verbatim|larm1000> is the same as <verbatim|larm10>. This is
  why <cpp|find_font> multiplies the size by 100 for the <verbatim|la> and
  <verbatim|gr> font trees, and why the helper <cpp|mag (dpi, size, dsize)>,
  which computes the magnified resolution <math|dpi\<cdot\>size/dsize>,
  first brings both sizes to the same convention when one of them is at
  least 316 and the other one is below 100.

  <paragraph|Finding a metric.><cpp|load_tex_tfm (family, size, dsize,
  tfm)> first consults the <verbatim|tfm:<em|family><em|size>> entry of
  <verbatim|font_cache.scm>, which remembers the size that was used last
  time. Otherwise it searches without generating anything (if generation
  is enabled at all), and only then searches again with generation
  allowed. The search itself (<cpp|try_tfm>) tries, in this order,

  <\enumerate>
    <item>the requested size;

    <item>for sizes above 333 (the <math|\<times\>100> convention), the
    rounded size divided by 100;

    <item>the neighbouring sizes <math|size\<pm\>1> and
    <math|size\<pm\>2> (the nearer one in the direction of 10 first), and
    the same sizes in the <math|\<times\>100> convention;

    <item>the fall-back design size <cpp|dsize>, and finally size 10.
  </enumerate>

  When a metric of another size is used, its design size field
  (<cpp|header[1]>) is scaled with <cpp|mag>, so that the font is
  magnified to the requested size. A block of code which preferred the
  traditional <TeX> design sizes (5 to 12, 17) is still present but
  disabled (<cpp|if (false)>), since the shipped fonts are <name|Type 1>
  fonts which can be scaled to any size.

  <paragraph|Finding glyphs.><cpp|load_tex_pk> calls <cpp|try_pk> for the
  requested size, then for the design size and for size 10 (with the
  resolution magnified accordingly), and finally, for sizes in the
  <math|\<times\>100> convention, for the size divided by 100. <cpp|try_pk>
  first tries a <name|Type 1> substitute, and only if there is none opens or
  generates a <name|PK> file named
  <verbatim|<em|family><em|size>.<em|dpi>pk>.

  <section|The class <cpp|tex_font_metric_rep>>

  <\explain>
    <cpp|struct tex_font_metric_rep><explain-synopsis|a decoded
    <verbatim|.tfm> file>
  <|explain>
    Declared in <source-link|Plugins/Metafont/load_tfm.hpp|src/Plugins/Metafont/load_tfm.hpp>; a resource
    (<cpp|RESOURCE(tex_font_metric)>), so that each file is decoded only
    once per session and shared by all fonts which use it. Its fields are
    those of the <verbatim|.tfm> format: the lengths <cpp|lf>, <cpp|lh>,
    <cpp|bc>, <cpp|ec> (first and last character code), <cpp|nw>,
    <cpp|nh>, <cpp|nd>, <cpp|ni>, <cpp|nl>, <cpp|nk>, <cpp|ne> and
    <cpp|np>, and the arrays <cpp|header>, <cpp|char_info>,
    <cpp|width>, <cpp|height>, <cpp|depth>, <cpp|italic>,
    <cpp|lig_kern>, <cpp|kern>, <cpp|exten> and <cpp|param>. In addition,
    <cpp|left>, <cpp|right>, <cpp|left_prog> and <cpp|right_prog> record
    the boundary character information, and <cpp|size> is the design size
    in points.
  </explain>

  <cpp|load_tfm (file, family, size)> reads the file with
  <cpp|load_string>, decodes the twelve 16-bit lengths, checks that they add
  up to the file length <cpp|lf> (and fails with <cpp|FAILED ("invalid tfm
  file")> otherwise), and then reads the arrays of 32-bit words. It also
  overrides the slant parameter of the italic and slanted variants of the
  old <name|Adobe> font clones by <name|Dobkin> (<verbatim|times-ti>,
  <verbatim|palatino-sl>, ...), whose metrics are wrong.

  The accessors decode the packed character information: <cpp|w (c)>,
  <cpp|h (c)>, <cpp|d (c)> and <cpp|i (c)> give the width, height, depth
  and italic correction of a character (0 outside
  <math|[bc,ec]>); <cpp|tag (c)> and <cpp|rem (c)> give the tag (0: none, 1:
  ligature/kerning program, 2: next larger character, 3: extensible
  recipe) and the remainder; <cpp|top>, <cpp|mid>, <cpp|bot> and <cpp|rep>
  give the pieces of an extensible character; <cpp|list_len> and
  <cpp|nth_in_list> walk the chain of successively larger characters. The
  font parameters are available as <cpp|slope> (clamped to
  <math|\<pm\>0.25> if absurd), <cpp|spc>, <cpp|spc_stretch>,
  <cpp|spc_shrink>, <cpp|x_height>, <cpp|spc_quad> and <cpp|spc_extra>.
  All values are in the fixed point units of the <verbatim|.tfm> format
  (<math|2<rsup|20>> per design size); the fonts convert them with a factor
  <cpp|unit>.

  <paragraph|Ligatures and kerning.><cpp|execute (s, n, buf, ker, m)> runs
  the ligature and kerning program of the font on the character codes
  <cpp|s[0..n-1]>. It produces the resulting characters in <cpp|buf>, with
  the kerning to be added after each character in <cpp|ker>, and returns
  their number in <cpp|m>. It handles the kerning instructions, the
  ligature instructions with their operation byte <math|4a+2b+c> (which
  says whether the left and right characters are kept and how many
  characters to pass over), the stop condition (skip byte of 128 or more)
  and the indirect start of a program (first skip byte above 128); see
  <hlink|the pitfalls|texfonts-pitfalls.en.tm> for a doubt about the
  operations which keep exactly one of the two characters. The
  input is processed on a stack, so that new ligatures can be formed with
  the result of previous ones; strings for which the buffers would
  overflow make the program fail (<cpp|FAILED ("string too complex for
  ligature kerning")>). <cpp|get_xpositions> computes the positions of the
  characters of a string in the same way, optionally without ligatures.

  <section|The <name|PK> loader>

  <\explain>
    <cpp|struct pk_loader><explain-synopsis|decoding of <verbatim|.pk>
    files>
  <|explain>
    Declared in <source-link|Plugins/Metafont/load_pk.hpp|src/Plugins/Metafont/load_pk.hpp>. It is created with
    the file, the metric and the resolution, and reads the whole file into
    memory. <cpp|load_pk ()> checks the preamble (command 247, format 89),
    then scans all character packets. For each character of the metric's
    range with a non-empty bitmap, it creates an empty <cpp|glyph> of the
    right size and offsets, and records the position and flag byte of the
    packed bitmap, without decoding it; specials (commands 240 to 244) and
    no-ops are skipped.

    Finally, the logical width <cpp|lwidth> of every glyph is computed in
    pixels from the <verbatim|.tfm> width.
  </explain>

  The bitmaps are decoded lazily: the glyphs of a <name|PK> font are a
  <cpp|pk_font_glyphs_rep> (in <source-link|load_tex.cpp|src/Plugins/Metafont/load_tex.cpp>), whose <cpp|get (c)>
  unpacks the bitmap of character <cpp|c> the first time it is requested
  (<cpp|pk_loader::unpack>, which implements the run-length and
  <verbatim|dyn_f> nybble packing of the format, and the raw bitmap
  case). Characters outside <math|[bc,ec]> return an empty glyph.

  <section|<name|Type 1> substitution>

  If <TeXmacs> is built with <name|FreeType> (<verbatim|USE_FREETYPE>),
  <cpp|try_pk> first asks <cpp|tt_find_name (family, size)>
  (<source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>) for an outline font with the
  same name. <cpp|tt_find_name_sub> tries
  <verbatim|<em|family><em|size>>, the size divided by 100 for the
  <math|\<times\>100> convention, and then a list of usual design sizes
  (17 for sizes of 15 and more, 12 above 12, 5 to 9 for small sizes, then
  10, 700, 1700, 1000 and the bare family name). An outline font exists if
  <cpp|tt_font_find> finds a file <verbatim|<em|name>.pfb> (looked up with
  <cpp|resolve_tex>, hence in the <name|Type 1> path) or, failing that, a
  <verbatim|.ttf>, <verbatim|.ttc>, <verbatim|.otf> or <verbatim|.dfont>
  file. The result is cached as <verbatim|tt:<em|family><em|size>>.

  If a substitute is found, the glyphs of the font are the
  <cpp|tt_font_glyphs> of that file at the requested size and resolution:
  <name|FreeType> rasterizes each glyph into a <cpp|glyph> on request. The
  face is opened with the <name|Adobe> custom character map
  (<cpp|ft_select_charmap (face, ft_encoding_adobe_custom)> in
  <source-link|tt_face.cpp|src/Plugins/Freetype/tt_face.cpp>), so that character codes are those of the
  <verbatim|.tfm> file. Only the glyphs come from the outline font: the
  metrics, the ligatures and the kerning still come from the
  <verbatim|.tfm> file.

  <section|Glyphs>

  <\explain>
    <cpp|struct glyph_rep><explain-synopsis|a bitmap character>
  <|explain>
    Declared in <source-link|Graphics/Bitmap_fonts/bitmap_font.hpp|src/Graphics/Bitmap_fonts/bitmap_font.hpp>; the handle
    <cpp|glyph> is reference counted and may be nil. The fields are the
    pixel <cpp|width> and <cpp|height>, the offsets <cpp|xoff> and
    <cpp|yoff> of the reference point (from the left edge and from the top
    row), the logical width <cpp|lwidth>, the <cpp|depth> (1 for a bitmap,
    more for grey levels), the extensible piece <cpp|status>, the
    <cpp|index> of the glyph in its physical font, an <cpp|artistic> flag set
    by effects, and the <cpp|raster>. One-bit rasters are packed eight
    pixels per byte, least significant bit first; deeper ones use one byte
    per pixel. <cpp|get_x (i, j)> and <cpp|set_x> address the raster with
    the origin at the top left, <cpp|get (i, j)> and <cpp|set> with the
    origin at the reference point and <math|j> pointing upwards.
  </explain>

  <cpp|font_metric_rep> and <cpp|font_glyphs_rep> are the abstract tables of
  metrics and glyphs indexed by character code, which renderers and the
  <cpp|index_glyph> mechanism of fonts use (see <hlink|<TeXmacs>
  fonts|fonts.en.tm>). For <TeX> fonts, the metric table is a
  <cpp|tfm_font_metric_rep> (<source-link|tex_font.cpp|src/Plugins/Metafont/tex_font.cpp>), which combines the
  logical box of the <verbatim|.tfm> file with the ink box of the glyph.

  <paragraph|Shrinking for display.>Glyphs are rasterized at the resolution
  of the font (600 dpi at the default zoom), and the screen renderers reduce
  them by the shrinking factor (see <hlink|the renderer
  interface|renderer-api.en.tm>) with <cpp|shrink (gl, xf, yf, xo, yo)>
  (<source-link|Graphics/Bitmap_fonts/glyph_shrink.cpp|src/Graphics/Bitmap_fonts/glyph_shrink.cpp>). Every black input
  pixel is first thickened into a small block, whose size grows with the
  shrinking factor and with the <cpp|pixel_ratio> of high resolution screens
  (no thickening for glyphs produced by artistic effects); each output pixel
  then counts the black pixels of the block of input pixels it covers, which
  gives a grey level used as an alpha value (scaled to at most 64 levels).
  For ordinary glyphs, the horizontal phase is first adjusted
  (<cpp|get_hor_shift>) so that columns containing long vertical runs, that
  is the stems, fall on whole output pixels. For the pieces of extensible
  characters marked by <cpp|rubber_fix>, the joining row is made as opaque as
  its neighbour after shrinking (<cpp|adjust_top>, <cpp|adjust_bot>) and the
  vertical offset is reset, so that the pieces of a large delimiter join
  without visible seams; note that this adjustment is compiled out in
  <name|Qt> and <name|Qtwk> builds (<verbatim|#ifndef QTTEXMACS>), and
  kept with the other ports. The renderers cache the
  shrunk glyph as an image per character, font, shrinking factor and color
  (for instance the <cpp|character_image> table of the <name|Qt>
  renderer).

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
