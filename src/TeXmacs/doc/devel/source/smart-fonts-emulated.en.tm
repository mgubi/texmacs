<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Emulated (poor man's) fonts>

  <section|Introduction>

  An <em|emulated font> is a wrapper around a base font which modifies the
  appearance of its glyphs: it makes them bolder, slanted, wider, smaller,
  hollow, irregular or blurred. The implementations are the files
  <verbatim|Graphics/Fonts/poor_*.cpp>; the constructors are declared in
  <source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp>:

  <\cpp-code>
    font poor_rubber_font (font base);

    font poor_smallcaps_font (font base);

    font poor_italic_font (font base, double slant);

    font poor_stretched_font (font base, double zoomx, double zoomy);

    font poor_extended_font (font base, double factor, double lw);

    font poor_extended_font (font base, double factor);

    font poor_mono_font (font base, double lw, double phw);

    font poor_bold_font (font base, double lofat, double upfat);

    font poor_bold_font (font base);

    font poor_bbb_font (font base, double penw, double penh, double fatw);

    font poor_bbb_font (font base);

    font poor_distorted_font (font base, tree kind);

    font poor_effected_font (font base, tree kind);
  </cpp-code>

  Emulated fonts are used in three situations:

  <\enumerate>
    <item>Automatically, when the font database has no font for the
    requested series, shape or variant (missing bold, italic, small
    capitals or blackboard bold), see below.

    <item>By smart fonts, for particular characters: bold wide accents,
    double struck letters, extensible delimiters.

    <item>Explicitly, through the <src-var|font-effects> environment
    variable (see <hlink|font effects|smart-fonts-effects.en.tm>).
  </enumerate>

  <section|Common structure>

  All emulated fonts follow the same pattern. Let us take
  <cpp|poor_italic_font_rep> as an example.

  <\itemize>
    <item>The class has a field <cpp|base> and the parameters of the
    emulation. The constructor calls <cpp|copy_math_pars (base)> and
    adjusts the global parameters which change (for italics: the
    <cpp|slope>; for bold: the interword space, <cpp|wquad> and
    <cpp|wline>; for monospaced fonts: the spaces).

    <item><cpp|supports> is delegated to the base font.

    <item><cpp|get_extents> and <cpp|get_xpositions> call the base font and
    transform the result (for italics, the ink box is enlarged according
    to the slant; for extended fonts, all horizontal coordinates are
    multiplied by the factor; for bold fonts, each glyph gets wider and the
    positions are recomputed glyph by glyph using <cpp|advance_glyph>).

    <item><cpp|index_glyph> obtains the glyph and metric tables of the base
    font and returns transformed tables, using the operations of
    <source-link|Graphics/Bitmap_fonts/bitmap_font.hpp|src/Graphics/Bitmap_fonts/bitmap_font.hpp> on whole tables:
    <cpp|slanted>, <cpp|stretched>, <cpp|extended>, <cpp|bolden>,
    <cpp|make_bbb>, <cpp|mono>, <cpp|distorted>, <cpp|effected>.
    <cpp|get_glyph> does the same for a single glyph.

    <item><cpp|draw_fixed> cuts the string into glyphs with
    <cpp|base-\<gtr\>advance_glyph> and draws each of them with
    <cpp|ren-\<gtr\>draw (c, fng, x, y)> using the transformed tables.

    <item><cpp|magnify> builds the same emulation on the magnified base
    font.

    <item>The constructor function builds a resource name such as
    <verbatim|pooritalic[<em|base>,<em|slant>]> or
    <verbatim|poorbold[<em|base>,<em|lofat>,<em|upfat>]> and uses
    <cpp|make (font, name, ...)>, so that identical emulations are shared.
  </itemize>

  Drawing through transformed bitmap tables works on any renderer, but on
  printers it would produce bitmap (<name|Type 3>) fonts. Several
  emulations therefore have a separate printer path which keeps the
  output vectorial:

  <\description-paragraphs>
    <item*|<cpp|poor_italic_font_rep>, <cpp|poor_stretched_font_rep>>On
    printers (<cpp|!ren-\<gtr\>is_screen>), install a transformation
    (<cpp|slanting> resp. <cpp|scaling>) with
    <cpp|ren-\<gtr\>set_transformation>, draw the base font, and reset the
    transformation. <cpp|poor_stretched_font_rep> also falls back on this
    method on the screen when the base font has no glyph tables.

    <item*|<cpp|poor_mono_font_rep>>Always draws the base font, glyph by
    glyph, centered in a cell of width <cpp|wquad>; glyphs which are too
    wide are horizontally compressed with a scaling transformation.

    <item*|<cpp|poor_bold_font_rep>>On printers, draws the base glyph nine
    times with small horizontal offsets (and, for characters which need
    extra width, from a horizontally stretched base font with a slight
    vertical slope), which simulates a thicker pen.
  </description-paragraphs>

  The other emulations (<cpp|poor_bbb_font_rep>,
  <cpp|poor_extended_font_rep>, <cpp|poor_distorted_font_rep>,
  <cpp|poor_effected_font_rep>) always draw bitmaps.

  <section|Catalogue>

  <\description>
    <item*|<cpp|poor_bold_font (base, lofat, upfat)>>Emboldening by
    <cpp|bolden>. <src-arg|lofat> and <src-arg|upfat> are the extra pen
    widths for lower and upper case letters, as fractions of the design
    size <cpp|wfn>; the version without parameters uses
    <cpp|wline/wfn>. A small table (<cpp|get_bold_multiplier>) makes wide
    letters such as <verbatim|M> and <verbatim|W> gain more width than the
    others.

    <item*|<cpp|poor_italic_font (base, slant)>>Slanting by
    <cpp|slanted>. The automatic emulation uses the slant
    <cpp|0.25001> on a font compressed horizontally by <math|5/6>; this
    precise value is recognized by <cpp|concat_math>
    (<source-link|Typeset/Concat/concat_math.cpp|src/Typeset/Concat/concat_math.cpp>) when positioning scripts.

    <item*|<cpp|poor_smallcaps_font (base)>>Not a bitmap transformation but a
    small dispatcher, similar to a smart font: runs of lower case
    characters (<cpp|uni_upcase_char> differs) are upcased and drawn with a
    magnified version of the base font, whose horizontal and vertical
    factors are derived from the ratio of the heights of <verbatim|x> and
    <verbatim|X>.

    <item*|<cpp|poor_bbb_font (base, penw, penh, fatw)>>Blackboard bold
    (\Pdouble struck\Q) letters built by <cpp|make_bbb>, which doubles the
    strokes; the pen dimensions and the extra width are fractions of
    <cpp|wfn>.

    <item*|<cpp|poor_stretched_font (base, zoomx, zoomy)>>Anisotropic
    magnification. Since fonts can be magnified isotropically, the
    constructor reduces to <cpp|base-\<gtr\>magnify (zoomx)> followed by a
    vertical stretching by <cpp|zoomy/zoomx>, and returns the base font if
    no stretching is needed. This is also the default
    <cpp|font_rep::poor_magnify>.

    <item*|<cpp|poor_extended_font (base, factor, lw)>>Horizontal extension
    or condensation which, unlike a plain stretching, preserves the width
    of vertical strokes (<cpp|extended>, <cpp|widen>). The two argument
    version measures the stroke width <src-arg|lw> from the glyphs
    <verbatim|o> and <verbatim|O>.

    <item*|<cpp|poor_mono_font (base, lw, phw)>>A monospaced version of a
    proportional font: every character occupies <src-arg|lw> times the quad
    width of the base font, and characters wider than <src-arg|phw> quads
    are compressed.

    <item*|<cpp|poor_distorted_font (base, kind)>>Random distortions of the
    glyphs (<cpp|distorted> in
    <source-link|Graphics/Bitmap_fonts/glyph_distorted.cpp|src/Graphics/Bitmap_fonts/glyph_distorted.cpp>); <src-arg|kind> is
    a tuple <verbatim|(degraded <em|threshold> <em|frequency>)>,
    <verbatim|(distorted <em|strength> <em|frequency>)> or
    <verbatim|(gnawed <em|strength> <em|frequency>)>.

    <item*|<cpp|poor_effected_font (base, kind)>>Graphical effects applied to
    the glyphs (<source-link|glyph_effected.cpp|src/Graphics/Bitmap_fonts/glyph_effected.cpp>). Currently only
    <verbatim|(blurred <em|radius> [<em|dx> <em|dy>])> is recognized: it is
    translated into a Gaussian blur effect (<cpp|EFF_BLUR>,
    <cpp|EFF_GAUSSIAN>), optionally moved (<cpp|EFF_MOVE>).

    <item*|<cpp|poor_rubber_font (base)>>Extensible delimiters and big
    operators for <name|Unicode> fonts without such glyphs; see below.
  </description>

  Two related wrappers are not emulations of a variant but are used by the
  font effects: <cpp|recolored_font (base, kind)> draws the base font with
  the color <src-arg|kind> (it changes the pencil of the renderer and
  restores it afterwards), and <cpp|superposed_font (fns, ref)> draws all
  fonts of <src-arg|fns> on top of each other, taking metrics and glyphs
  from <cpp|fns[ref]> (the ink boxes are merged).

  <section|Emulation of missing series and shapes>

  The automatic emulation is decided during the font selection, in
  <cpp|find_closest> (<source-link|Graphics/Fonts/font_translate.cpp|src/Graphics/Fonts/font_translate.cpp>, see
  also the chapter on <hlink|font selection|font-database-selection.en.tm>). After
  finding the closest physical font, it compares the requested logical
  features with the features of the font which was found (both those
  declared in the database and those guessed from its name). If a feature
  was requested but is not present, a suffix is appended to the name
  which is returned:

  <\description>
    <item*|<verbatim|outline> (blackboard bold) missing>The variant gets the
    suffix <verbatim|-poorbbb>.

    <item*|<verbatim|bold> missing>The series gets the suffix
    <verbatim|-poorbf>.

    <item*|<verbatim|smallcaps> missing>The shape gets the suffix
    <verbatim|-poorsc>.

    <item*|<verbatim|italic> or <verbatim|oblique> missing>The shape gets
    the suffix <verbatim|-poorit>.
  </description>

  <cpp|closest_font> then calls <cpp|find_font (family, variant, series,
  shape, sz, dpi)> (<source-link|Graphics/Fonts/find_font.cpp|src/Graphics/Fonts/find_font.cpp>), which
  recognizes these suffixes, recursively finds the font without the
  suffix, and wraps it:

  <\cpp-code>
    if (ends (shape, "-poorit")) {

    \ \ string shape2= shape (0, N(shape) - 7);

    \ \ font fn= find_font (family, variant, series, shape2, sz, dpi);

    \ \ if (!is_nil (fn)) {

    \ \ \ \ font nafn= fn-\<gtr\>magnify (5.0/6.0, 1.0);

    \ \ \ \ font itfn= poor_italic_font (nafn, 0.25001);

    \ \ \ \ // NOTE: precise value 0.25001 also used in 'concat_math'

    \ \ \ \ font::instances (s)= (pointer) itfn.rep;

    \ \ \ \ return itfn;

    \ \ }

    }

    else if (ends (shape, "-poorsc")) ...\ \ \ \ \ \ \ // poor_smallcaps_font (fn)

    else if (ends (series, "-poorbf")) ...\ \ \ \ \ \ // poor_bold_font (fn)

    else if (ends (variant, "-poorbbb")) ...\ \ \ \ // poor_bbb_font (fn)
  </cpp-code>

  Since only one suffix is handled per call and the recursive call handles
  the next one, combinations such as a bold italic emulation of a regular
  font work naturally. Because smart fonts obtain all their subfonts
  through <cpp|closest_font>, the emulation also applies to the
  mathematical alphabets: if a family has no <verbatim|outline> variant,
  the <verbatim|bbb> subfont of the smart font is an emulated blackboard
  bold version of the family.

  <section|Emulated subfonts inside smart fonts>

  Besides the automatic emulation, smart fonts create emulated subfonts
  directly in <cpp|smart_font_rep::initialize_font>:

  <\itemize>
    <item><verbatim|poor-bold>: <cpp|poor_bold_font> of the medium version
    of the smart font with fatness <math|(5/3-1)\<cdot\>wline/wfn>, used for
    wide accents in bold text.

    <item><verbatim|poor-bbb>: <cpp|poor_bbb_font> of the upright smart font,
    with pen dimensions derived from the font database characteristics,
    used for the symbols <verbatim|\<less\>bbb-X\<gtr\>>.

    <item><verbatim|rubber>: <cpp|rubber_font> of another subfont, which is
    usually a <cpp|poor_rubber_font>, but a <cpp|rubber_unicode_font> when
    the subfont has an <name|OpenType> <verbatim|MATH> table (see
    <hlink|stretchable glyphs|opentype-stretch.en.tm>).
  </itemize>

  <section|Extensible delimiters: <cpp|poor_rubber_font>>

  Mathematical typesetting needs delimiters of arbitrary size
  (<verbatim|\<less\>left-(-<em|n>\<gtr\>>,
  <verbatim|\<less\>mid-\|-<em|n>\<gtr\>>, ...) and big operators
  (<verbatim|\<less\>big-sum-1\<gtr\>>, <verbatim|\<less\>big-sum-2\<gtr\>>).
  Ordinary <name|Unicode> text fonts provide none of them.
  <cpp|poor_rubber_font (base)> builds them from the ordinary glyphs. A
  font with a <verbatim|MATH> table does not go through this emulation: its
  <cpp|make_rubber_font> returns a <cpp|rubber_unicode_font> which uses the
  size variants and the assemblies of the table, so that the poor rubber
  font now only serves the fonts which have none.

  <\itemize>
    <item>The base font is first enhanced with the bracket emulations:
    <cpp|virtual_enhance_font (base, "emu-bracket")>, so that angle
    brackets, floors, ceilings, double brackets and double bars exist.

    <item>The font keeps an array <cpp|larger> of fonts, created on demand
    by <cpp|get_font (nr)>. For <math|nr\<leqslant\>9>
    (<cpp|2*MAGNIFIED_NUMBER+1>) these are <cpp|poor_stretched_font>s of the
    base font, magnified vertically by <math|2<rsup|\<lfloor\>nr/2\<rfloor\>/4>>
    and horizontally by the square root (or, for odd <math|nr>, the fourth
    root) of this factor; odd numbers are used for thin delimiters
    (<cpp|is_thin>). Numbers 10 and 11 are stretched versions of the virtual
    font <verbatim|emu-large>, number 12 is <cpp|rubber_unicode_font
    (base)>, and number 13 is the virtual font <verbatim|emu-large> built
    on the poor rubber font itself. The numbers from 14
    (<cpp|SLASH_BASE>) to 29 are further stretched versions of the base
    font for the slashes: a slash cannot be extended by repeating a
    straight middle part, so beyond the magnified sizes it keeps being
    stretched, by <math|2<rsup|k/4>> vertically and the fourth root of this
    factor horizontally.

    <item><cpp|search_font (s, r)> maps a requested symbol to a font number
    and a symbol in that font. Small delimiters (size up to
    <cpp|MAGNIFIED_NUMBER>, as well as angle brackets of any size) are
    magnified versions of the base glyph, possibly replaced by an
    <verbatim|emu-...> construction if the base font lacks the glyph.
    Larger slashes and backslashes go to the fonts from <cpp|SLASH_BASE>
    on; a backslash whose height differs by more than 5% from the one of
    the slash is replaced by <verbatim|\<less\>emu-backslash\<gtr\>>, made
    from the slash, at the largest magnified size.
    Larger delimiters use the parameterized virtual glyphs
    <verbatim|rubber-lparenthesis-#>, <verbatim|rubber-lbracket-#>, ... of
    <verbatim|emu-large.vfn>, with the parameter
    <math|max(n-5,0)+1> (the constant <cpp|HUGE_ADJUST> is 1); they are requested with
    the legacy encoding \Pcode byte followed by the parameter\Q. Big
    operators come from the base font if it has them (<cpp|big_flag>, for
    the <TeX> Gyre fonts and for any font with a <verbatim|MATH> table,
    <cpp|ot_math>) or from <verbatim|emu-large>.
  </itemize>

  The global <cpp|has_poor_rubber> (in <source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp>,
  <cpp|true> by default) enables this mechanism; <cpp|use_poor_rubber
  (fn)> is consulted by the typesetter
  (<source-link|Typeset/Concat/concat_post.cpp|src/Typeset/Concat/concat_post.cpp>), which always creates a
  delimiter box for such fonts, even for small delimiters.

  <section|The error font>

  <cpp|error_font (fn)> (<source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp>) is the last
  resort of every smart font. It claims to support every string, measures
  it with <src-arg|fn> and draws it in red with <src-arg|fn>. The error
  font of a smart font is based on the sans serif variant of the
  <verbatim|roman> family. Note that <cpp|error_font_rep::draw_fixed> sets the pencil to red without
  restoring it.

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
