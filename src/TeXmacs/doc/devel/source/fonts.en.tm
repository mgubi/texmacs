<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<TeXmacs> fonts>

  This chapter describes the font classes of the program. How a user or a
  style file selects fonts, which files describe them and how virtual fonts
  are written is explained in the reference chapter <hlink|fonts, from
  selection to glyph|../fonts/font-guide.en.tm>, and the mathematical fonts
  shipped with <TeXmacs> are presented in the section <hlink|mathematical
  fonts|../../main/math/fonts/man-math-fonts.en.tm> of the user manual. The
  support of <name|OpenType> fonts, and in particular of the
  <verbatim|MATH> table, is the subject of the chapter <hlink|<name|OpenType>
  fonts|opentype.en.tm>.

  <section|Classical conceptions of fonts>

  The way <TeXmacs> handles fonts is quite different from classical text
  editors and even from <TeX>. Let us first analyze some classical ways of
  conceiving fonts.

  <\itemize>
    <item>Physical fonts are just given by the name of a file, which contains
    a character set, i.e. a list of bitmaps. Usually the size of a character
    set is limited by 256 (or 65536).

    <item>True type fonts essentially work in the same way, except that the
    bitmaps can now be computed for any desired size.

    <item>In the X-window system, the name of the font is replaced by a more
    systematic name, which explicitly contains a certain number of font
    parameters, such as its size, series and shape. This makes it easier for
    applications to select an appropriate font. However, character sets are
    still limited in size.

    <item>In <TeX>, symbols are seen as commands, which select an appropriate
    physical font (which corresponds to a <verbatim|.tfm> and a
    <verbatim|.pk> file), based on symbol font declarations and environment
    variables (such as size, series and shape).
  </itemize>

  Clearly, among all these methods, <TeX> provides the largest flexibility.
  However, philosophically speaking, we think that it also has some
  drawbacks:

  <\itemize>
    <item>There is no distinction between usual commands and commands to make
    symbols: the current time might be considered as a symbol.

    <item>The encoding of the font is fixed by the names of the commands. For
    instance, for mathematical symbols, no clean general encoding scheme is
    provided, except the default naming of symbols by commands.

    <item>For beginners, it remains extremely hard to use non standard fonts.
  </itemize>

  Actually, in <TeX>, the notion of \Pthe current font\Q is ill-defined: it
  is merely the superposition of all character generating commands.

  <section|The conception of a font in TeXmacs>

  Philosophically speaking, we think that a font should be characterized by
  the following two essential properties:

  <\enumerate>
    <item>A font associates graphical meanings to
    <with|font-shape|italic|words>. The words can always be represented by
    strings.

    <item>The way this association takes place is coherent as a function of
    the word.
  </enumerate>

  By a word, we either mean a word in a natural language, or a sequence of
  mathematical, technical or artistic symbols.

  This way of viewing fonts has several advantages:

  <\enumerate>
    <item>A font may take care of kerning and ligatures.

    <item>A font may consist of several \Pphysical fonts\Q, which are somehow
    merged together.

    <item>A font might in principle automatically build very complicated
    glyphs like hieroglyphs or large delimiters from words in a well chosen
    encoding.

    <item>A font is an irreducible and persistent entity, not a bunch of
    commands whose actions may depend on some environment.
  </enumerate>

  Notice finally that the \Pgraphical meaning\Q of a word might be more than
  just a bitmap: it might also contain some information about a logical
  bounding box, appropriate places for scripts, etc. Similarly, the
  \Pcoherence of the association\Q should be interpreted in its broadest
  sense: the font might contain additional information for the global
  typesetting of the words on a page, like the recommended distance between
  lines, the height of a fraction bar, etc.

  <section|String encodings>

  All text strings in <TeXmacs> consist of sequences of either specific or
  universal symbols. A specific symbol is a character, different from
  <verbatim|'\\0'>, <verbatim|'\<less\>'> and <verbatim|'\<gtr\>'>, which
  is interpreted in the Cork encoding. A universal symbol is a string
  starting with <verbatim|'\<less\>'>, followed by an arbitrary sequence of
  characters different from <verbatim|'\\0'>, <verbatim|'\<less\>'> and
  <verbatim|'\<gtr\>'>, and ending with <verbatim|'\<gtr\>'>. Examples are
  named symbols like <verbatim|\<less\>alpha\<gtr\>> or
  <verbatim|\<less\>leqslant\<gtr\>>, and arbitrary <name|Unicode>
  characters like <verbatim|\<less\>#2212\<gtr\>>. The meaning of symbols
  does not depend on the particular font which is used, but different fonts
  may render them in a different way. It is the responsibility of each font
  to translate symbols into its own internal encoding.

  Universal symbols can also be used to represent mathematical symbols of
  variable sizes like large brackets. The point here is that the shapes of
  such symbols depend on certain size parameters, which can not conveniently
  be thought of as font parameters. This problem is solved by letting the
  extra parameters be part of the symbol. For instance,
  <verbatim|"\<less\>left-(-0\<gtr\>"> is the usual opening bracket and
  <verbatim|"\<less\>left-(-1\<gtr\>"> a slightly larger one. Similarly,
  big operators are represented by symbols like
  <verbatim|"\<less\>big-sum-1\<gtr\>"> (small version) and
  <verbatim|"\<less\>big-sum-2\<gtr\>"> (display version). The typesetter
  determines the appropriate size of a delimiter as a function of the height
  of the delimited expression (see <cpp|get_delimiter> in
  <verbatim|Typeset/Boxes/Basic/text_boxes.cpp>). A font which knows the
  size variants of its glyphs, as an <name|OpenType> math font does, is
  first asked directly with <cpp|get_rubber_variant> (for heights) and
  <cpp|get_wide_variant> (for the widths of wide accents and arrows,
  <cpp|get_wide>), and the numbered names are only searched when it cannot
  tell; see <hlink|stretchable glyphs|opentype-stretch.en.tm>.

  <section|The abstract font class>

  The main abstract <cpp|font> class is defined in
  <verbatim|Graphics/Fonts/font.hpp>. Here follows an abridged version of
  its representation class:

  <\cpp-code>
    struct font_rep: rep\<less\>font\<gtr\> {

    \ \ int \ \ \ \ \ type; \ \ \ \ \ \ \ \ \ \ \ \ \ // font type

    \ \ int \ \ \ \ \ math_type; \ \ \ \ \ \ \ \ // normal, Stix, TeX Gyre or OpenType

    \ \ SI \ \ \ \ \ \ size; \ \ \ \ \ \ \ \ \ \ \ \ \ // requested size

    \ \ SI \ \ \ \ \ \ design_size; \ \ \ \ \ \ // design size in points/256

    \ \ SI \ \ \ \ \ \ display_size; \ \ \ \ \ // display size in points/PIXEL

    \ \ double \ \ slope; \ \ \ \ \ \ \ \ \ \ \ \ // italic slope

    \ \ space \ \ \ spc; \ \ \ \ \ \ \ \ \ \ \ \ \ \ // usual space between words

    \ \ space \ \ \ extra; \ \ \ \ \ \ \ \ \ \ \ \ // extra space at end of words

    \ \ SI \ \ \ \ \ \ sep; \ \ \ \ \ \ \ \ \ \ \ \ \ \ // separation space between close components

    \;

    \ \ SI \ \ \ \ \ \ y1, y2; \ \ \ \ \ \ \ \ \ \ \ // bottom and top y positions

    \ \ SI \ \ \ \ \ \ yx; \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // height of the x character

    \ \ SI \ \ \ \ \ \ yfrac; \ \ \ \ \ \ \ \ \ \ \ \ // vertical position fraction bar

    \ \ SI \ \ \ \ \ \ ysub_lo_base, ...; // positioning of scripts

    \ \ SI \ \ \ \ \ \ wpt, hpt; \ \ \ \ \ \ \ \ \ // width and height of one point in font

    \ \ SI \ \ \ \ \ \ wfn; \ \ \ \ \ \ \ \ \ \ \ \ \ \ // wpt * design size in points

    \ \ SI \ \ \ \ \ \ wline; \ \ \ \ \ \ \ \ \ \ \ \ // width of fraction bars and so

    \ \ SI \ \ \ \ \ \ wquad; \ \ \ \ \ \ \ \ \ \ \ \ // quad space

    \ \ ...

    \ \ bool \ \ \ \ ot_math; \ \ \ \ \ \ \ \ \ \ // parameters read from a MATH table

    \ \ SI \ \ \ \ \ \ frac_rule_thickness, ...; // about forty MATH constants

    \;

    \ \ virtual bool \ \ supports (string c) = 0;

    \ \ virtual void \ \ get_extents (string s, metric& ex) = 0;

    \ \ virtual void \ \ get_xpositions (string s, SI* xpos);

    \ \ virtual void \ \ draw_fixed (renderer ren, string s, SI x, SI y) = 0;

    \ \ virtual font \ \ magnify (double zoomx, double zoomy) = 0;

    \ \ virtual void \ \ draw (renderer ren, string s, SI x, SI y);

    \;

    \ \ virtual double get_left_slope \ (string s);

    \ \ virtual double get_right_slope (string s);

    \ \ virtual SI \ \ \ \ get_left_correction \ (string s);

    \ \ virtual SI \ \ \ \ get_right_correction (string s);

    \ \ virtual SI \ \ \ \ get_lsub_correction \ (string s);

    \ \ virtual SI \ \ \ \ get_lsup_correction \ (string s);

    \ \ virtual SI \ \ \ \ get_rsub_correction \ (string s);

    \ \ virtual SI \ \ \ \ get_rsup_correction \ (string s);

    \ \ virtual SI \ \ \ \ get_rsup_correction_at (string s, SI h);

    \ \ virtual bool \ \ get_rubber_variant (string s, SI height, string& r);

    \ \ virtual bool \ \ get_wide_variant (string s, SI width, string& r);

    \ \ virtual bool \ \ get_top_accent (string s, SI& x);

    \ \ virtual bool \ \ get_feature_variant (string, string, int, string&);

    \ \ ...

    \;

    \ \ virtual glyph get_glyph (string s);

    \ \ virtual int \ \ index_glyph (string s, font_metric& fnm, font_glyphs& fng);

    };
  </cpp-code>

  The main abstract routines are <cpp|get_extents> and <cpp|draw_fixed>.
  The first routine determines the logical and ink bounding boxes of the
  graphical representation of a word (in a structure of type <cpp|metric>),
  the second one draws the string on a <hlink|renderer|renderer.en.tm>
  (which may be the screen, a printer or a <name|PDF> file). The routine
  <cpp|supports> tells whether a given symbol is available in the font and
  <cpp|magnify> constructs a magnified version of the font. Some fonts can
  also export their glyphs as bitmaps (<cpp|get_glyph>) or as indexes into
  tables of glyphs and metrics (<cpp|index_glyph>), which are used by the
  renderers. Fonts are cached by name, so that each font is only
  constructed once.

  The additional data are used for global typesetting using the font. The
  other virtual routines are used for determining additional properties of
  typeset strings, such as slopes and italic corrections, which are needed
  for the <hlink|mathematical typesetting|maths.en.tm>. More recent fields
  (not shown above) contain microtypographic adjustments for scripts and
  wide accents, tables for character protrusion and tables for the spacing
  between mathematical symbols.

  A font with an <name|OpenType> <verbatim|MATH> table sets <cpp|ot_math>
  and, unless a hand-tuned branch keeps its own math type, the math type
  <cpp|MATH_TYPE_OPENTYPE> (see <hlink|activation|opentype-math.en.tm>),
  and fills about forty further
  fields with the constants of the table (fraction, radical, limit, stretch
  stack, script and accent geometry); they stay zero for the other fonts,
  and <cpp|copy_math_pars> copies them like the older fields. The hooks
  added for such fonts all have defaults in <cpp|font_rep> which answer
  \Punknown\Q: the script corrections <cpp|get_lsub_correction_at>, ...,
  <cpp|get_rsup_correction_at> take the height of the script into account
  (and fall back on the corrections without height),
  <cpp|get_rubber_variant>, <cpp|get_wide_variant> and
  <cpp|is_extended_shape> serve the stretchable glyphs,
  <cpp|get_top_accent> the placement of accents, and
  <cpp|get_feature_variant> the glyph substitutions of the <name|OpenType>
  features. <cpp|make_rubber_font>, which builds the font of the large
  delimiters (<cpp|rubber_font>), is a virtual method as well, so that a
  smart font can hand this job to its <name|OpenType> main font. These
  fields and hooks are described in <hlink|mathematics from the
  <verbatim|MATH> table|opentype-math.en.tm>.

  <section|Implementation of concrete fonts>

  Several types of concrete fonts have been implemented in <TeXmacs>:

  <\description>
    <item*|<TeX> text fonts>See <verbatim|Plugins/Metafont/tex_font.cpp>.
    These fonts use <verbatim|.tfm> metrics and <verbatim|.pk> or
    <name|Type 1> glyphs (see also <verbatim|load_tex.cpp>,
    <verbatim|load_tfm.cpp> and <verbatim|load_pk.cpp> in the same
    directory).

    <item*|<TeX> rubber fonts>See
    <verbatim|Plugins/Metafont/tex_rubber_font.cpp>. These fonts provide
    large delimiters and big operators of variable size.

    <item*|<name|TrueType> and <name|OpenType> fonts>See
    <verbatim|Plugins/Freetype>: <verbatim|tt_font.cpp> implements fonts with
    a fixed encoding, <verbatim|unicode_font.cpp> <name|Unicode> fonts, and
    <verbatim|unicode_math_font.cpp>, <verbatim|rubber_unicode_font.cpp>,
    <verbatim|rubber_stix_font.cpp> and <verbatim|rubber_assemble_font.cpp>
    mathematical fonts and their rubber (extensible) variants. The files
    <verbatim|adjust_*.cpp> contain font specific adjustments, which take
    precedence over the data of an <name|OpenType> <verbatim|MATH> table.
    <verbatim|tt_tools.cpp> reads the <verbatim|MATH>, <verbatim|GSUB> and
    <verbatim|GPOS> tables of such fonts and <verbatim|tt_face.cpp> caches
    them per face; see <hlink|the <name|OpenType> layout
    tables|opentype-tables.en.tm>.

    <item*|System fonts>See <verbatim|Plugins/Qt/qt_font.cpp> and
    <verbatim|Plugins/X11/x_font.cpp>.

    <item*|Mathematical fonts>See <verbatim|Graphics/Fonts/math_font.cpp>.
    These fonts combine several <TeX> fonts according to an encoding.

    <item*|Virtual fonts>See <verbatim|Graphics/Fonts/virtual_font.cpp>.
    Virtual fonts build new symbols out of existing ones, using the
    definitions in <verbatim|$TEXMACS_PATH/fonts/virtual/*.vfn>.

    <item*|Compound fonts>See <verbatim|Graphics/Fonts/compound_font.cpp>.

    <item*|Smart fonts>See <verbatim|Graphics/Fonts/smart_font.cpp>. A smart
    font merges several fonts: symbols which are not supported by the main
    font are looked up in other fonts, depending on their <name|Unicode>
    range and on the font database, and symbols which are not available at
    all are rendered using an error font. The profiles of the named
    <name|OpenType> math fonts, which give their text, sans serif and
    typewriter companions, are in <verbatim|math_font_profiles.cpp>; see
    <hlink|math font profiles|opentype-profiles.en.tm>.

    <item*|Synthetic fonts>The files <verbatim|poor_*.cpp> in
    <verbatim|Graphics/Fonts> implement \Ppoor man's\Q fonts, which
    synthesize bold, italic, small capitals, blackboard bold, extended,
    monospaced or distorted variants of existing fonts. Similarly,
    <verbatim|superposed_font.cpp> and <verbatim|recolored_font.cpp>
    implement superposed and recolored fonts, and
    <verbatim|feature_font.cpp> shows a font through one of its
    <name|OpenType> substitution features (<cpp|feature_font>,
    <cpp|apply_features>; see <hlink|<name|OpenType>
    features|opentype-features.en.tm>).
  </description>

  In most cases, the lowest layer of the implementation consists of a
  collection of glyphs (see <verbatim|Graphics/Bitmap_fonts>) or of outline
  fonts, together with some font metric information. The font is
  responsible for putting these glyphs together using some appropriate
  spacing. The renderers take care of displaying glyphs in a nice,
  anti-aliased way, or of embedding the fonts in <name|PDF> or
  <name|PostScript> files.

  <section|Font selection>

  After having implemented fonts themselves, an important remaining issue is
  the selection of the appropriate font as a function of a certain number of
  parameters, such as its name, variant, series, shape and size. These
  parameters are given by the environment variables <src-var|font>,
  <src-var|font-family>, <src-var|font-series>, <src-var|font-shape>,
  <src-var|font-base-size> and <src-var|font-size> (and similar variables
  for mathematics and programs); the font is recomputed by
  <cpp|edit_env_rep::update_font> in <verbatim|Typeset/Env/env_semantics.cpp>
  whenever one of them changes. <TeXmacs> currently provides two font
  selection mechanisms.

  <subsection|Selection through the font database>

  By default (when the preference <verbatim|"new style fonts"> is set to
  <verbatim|"on">), fonts are created by the function <cpp|smart_font>.
  Font names are resolved using a <em|font database>, which associates to
  each font family and style the corresponding font files and a list of
  <em|characteristics> (such as the supported scripts, whether the font is
  monospaced, sans serif or italic, its slant, its x-height, <abbr|etc.>). The global database
  consists of the files <verbatim|font-database.scm>,
  <verbatim|font-features.scm>, <verbatim|font-characteristics.scm> and
  <verbatim|font-substitutions.scm> in <verbatim|$TEXMACS_PATH/fonts>; a
  local database of the fonts which are installed on the user's system is
  maintained in <verbatim|$TEXMACS_HOME_PATH/fonts>. It can be rebuilt using
  the <scheme> command <scm|scan-disk-for-fonts>, and the entries of a newer
  shipped database are merged into it automatically (see <hlink|the font
  database|font-database.en.tm>). The implementation can be
  found in <verbatim|Graphics/Fonts/font_database.cpp> and
  <verbatim|Graphics/Fonts/font_select.cpp>: requested fonts are translated
  into lists of \Plogical\Q features, which are matched against the
  characteristics of the available fonts in order to find the closest
  match. This makes it possible to use arbitrary fonts installed on the
  system and to find reasonable substitutes for missing fonts.

  <subsection|Selection through rewriting rules>

  <TeXmacs> also comes with a macro-based font-selection scheme (using the
  <scheme> syntax), which is still used for <TeX> fonts, and for all fonts
  when the new style fonts are disabled. At the lowest level, we provide a
  fixed number of macros which directly correspond to the above types of
  concrete fonts (see <cpp|find_font> in
  <verbatim|Graphics/Fonts/find_font.cpp>). For instance, the macro

  <\scm-code>
    (tex $name $size $dpi)
  </scm-code>

  corresponds to the constructor

  <\cpp-code>
    font tex_font (string fam, int size, int dpi, int dsize=10);
  </cpp-code>

  of a <TeX> text font. Other macros are <scm|ec>, <scm|cm>, <scm|la>,
  <scm|adobe>, <scm|tex-rubber>, <scm|truetype>, <scm|unicode>,
  <scm|unimath>, <scm|math>, <scm|compound>, <scm|x> and <scm|qt>.

  At the middle level, it is possible to specify some rewriting rules like

  <\scm-code>
    ((roman rm medium right $s $d) (ec ecrm $s $d))

    ((avant-garde rm medium right $s $d) (ec avant-garde-rm $s $d))

    ((x-times rm medium right $s $d) (x adobe-times-medium-r-normal $s $d))
  </scm-code>

  When a left hand pattern is matched, it is recursively substituted by the
  right hand side. The files in the directory <verbatim|progs/fonts> contain
  a large number of rewriting rules, which are declared using
  <scm|set-font-rules>.

  At the top level, <TeXmacs> calls a macro of the form

  <\scm-code>
    ($name $variant $series $shape $size $dpi)
  </scm-code>

  as a function of the current environment in the text. If no rules were
  declared for the font family, the font database is searched instead. If
  no rule matches, <TeXmacs> successively tries macros with fewer
  parameters, and finally falls back on the Computer Modern font
  <verbatim|cmr>.

  <tmdoc-copyright|1998\U2021|Joris van der Hoeven|Darcy Shen>

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
