<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The smart font: dispatching characters to subfonts>

  <section|Creation of smart fonts>

  <subsection|From the environment to the font>

  Whenever one of the font related environment variables changes, the
  typesetter recomputes the current font in
  <cpp|edit_env_rep::update_font> (<verbatim|Typeset/Env/env_semantics.cpp>).
  In text mode it calls the six argument version of <cpp|smart_font> with
  the values of <src-var|font>, <src-var|font-family>,
  <src-var|font-series> and <src-var|font-shape>; in mathematical mode and
  in program mode it calls the ten argument version, which receives both
  the mathematical (resp. program) font and the surrounding text font:

  <\cpp-code>
    font edit_env_rep::make_current_font (int sz) {

    \ \ switch (mode) {

    \ \ case 2:

    \ \ \ \ return smart_font (get_string (MATH_FONT), get_string (MATH_FONT_FAMILY),

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_string (MATH_FONT_SERIES), get_string (MATH_FONT_SHAPE),

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_string (FONT), get_string (FONT_FAMILY),

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_string (FONT_SERIES), "mathitalic",

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ sz, (int) (magn*dpi));

    \ \ ...

    }

    \;

    int sz= get_script_size (fn_size, index_level);

    fn= make_current_font (sz);

    ... \ // OpenType MATH: script percentages and the ssty feature

    string feat= get_string (FONT_FEATURES);

    if (N(feat) != 0) fn= apply_features (fn, feat);

    string eff= get_string (FONT_EFFECTS);

    if (N(eff) != 0) fn= apply_effects (fn, eff);
  </cpp-code>

  In scripts, a font with an <name|OpenType> <verbatim|MATH> table is made
  again at the size given by the percentages of its table (unless
  <src-var|math-font-sizes> is set), and an untuned one is wrapped in a
  <cpp|feature_font> for its script size alternates (<verbatim|ssty>); see
  <hlink|mathematics from the <verbatim|MATH> table|opentype-math.en.tm>
  and <hlink|<name|OpenType> features|opentype-features.en.tm>.

  Notice that the names of the environment variables are slightly
  misleading at this level: <src-var|font> is passed as the <em|family>
  argument of <cpp|smart_font> and <src-var|font-family> (<verbatim|rm>,
  <verbatim|ss>, <verbatim|tt>, ...) as the <em|variant>.

  <\explain>
    <cpp|font smart_font (string family, string variant, string series,
    string shape, string tf, string tv, string tw, string ts, int sz, int
    dpi)><explain-synopsis|math and program fonts>
  <|explain>
    If the new style fonts are disabled (the global <cpp|new_fonts> is
    false), this falls back on the rule based <cpp|find_font>. Otherwise
    the font which is actually used is the <em|text> font <src-arg|tf>,
    <src-arg|tv>, <src-arg|tw>, <src-arg|ts>, adapted as follows: if the
    text family is <verbatim|roman>, it is replaced by the mathematical
    family <src-arg|family>; the mathematical variants <verbatim|ms> and
    <verbatim|mt> select the variants <verbatim|ss> and <verbatim|tt>; a
    mathematical series other than <verbatim|medium> replaces the text
    series, so that a family with a real bold math face uses it; and if the
    mathematical shape is <verbatim|right>, the shape becomes the special
    shape <verbatim|mathupright>. The result is the six argument
    <cpp|smart_font> applied to the adapted text font. In mathematical mode
    <src-arg|ts> is <verbatim|mathitalic>, so that the main font is the
    upright text font and the smart font takes care of the mathematical
    conventions (see below). Mathematical fonts proper are therefore
    usually specified inside the family list of <src-var|font>, using
    conditional entries such as <verbatim|math=...> or
    <verbatim|mathlarge=...>.
  </explain>

  <\explain>
    <cpp|font smart_font (string family, string variant, string series,
    string shape, int sz, int dpi)><explain-synopsis|text fonts>
  <|explain>
    For the variant <verbatim|rm>, this is just
    <cpp|smart_font_bis (family, variant, series, shape, sz, dpi, dpi)>.
    For other variants, the routine first checks with
    <cpp|logical_font> and <cpp|search_font> whether the variant is
    provided by the same physical font as the <verbatim|rm> variant. If not,
    it creates both smart fonts and magnifies the variant so that its
    x-height (<cpp|yx>) matches the one of the <verbatim|rm> variant, unless
    the ratio is within 2.5% of one. This is why a sans serif or
    typewriter font taken from another family blends with the main text.
  </explain>

  <\explain>
    <cpp|font smart_font_bis (string family, string variant, string series,
    string shape, int sz, int hdpi, int vdpi)><explain-synopsis|the actual
    constructor>
  <|explain>
    Builds (or retrieves from the font cache) the smart font with the given
    horizontal and vertical resolutions. Its name has the form
    <verbatim|family-variant-series-shape-sz-vdpi-smart> (with both
    resolutions when they differ). The routine:

    <\enumerate>
      <item>handles a few special cases: families starting with
      <verbatim|tc> (legacy symbols of <verbatim|std-symbol.ts>) go directly
      to <cpp|find_font>, and the families <verbatim|sys-chinese>,
      <verbatim|sys-japanese> and <verbatim|sys-korean> are replaced by
      <verbatim|cjk=<em|name>,roman> where <em|name> is the system default
      (<cpp|default_chinese_font_name> and friends in
      <verbatim|Graphics/Fonts/font.cpp>);

      <item>normalizes the family list with <cpp|tex_gyre_fix>,
      <cpp|kepler_fix>, <cpp|math_fix> and <cpp|profile_fix> (see below);

      <item>replaces the shapes <verbatim|mathitalic> and
      <verbatim|mathshape> by <verbatim|right> for the main font;

      <item>computes the <em|main family> with <cpp|main_family> and the
      <em|base font> <cpp|closest_font (mfam, variant, series, sh, sz, vdpi)>;

      <item>computes the error font
      <cpp|error_font (closest_font ("roman", "ss", "medium", "right", sz,
      vdpi))>;

      <item>constructs a <cpp|smart_font_rep>.
    </enumerate>
    All other routines in <verbatim|smart_font.cpp> which need a font with a
    different family, variant, series or shape call <cpp|smart_font_bis>
    again, so subfonts are often smart fonts themselves.
  </explain>

  <subsection|Family lists>

  The <em|family> argument of a smart font is a comma separated list. Each
  entry is either a plain family name or a <em|conditional entry> of the
  form <verbatim|<em|conditions>=<em|family>>. Examples from the style
  packages in <verbatim|packages/customize/fonts/> and from the code are:

  <\verbatim-code>
    mathlarge=TeX Gyre Pagella,Linux Libertine

    mathlarge=TeX Gyre Pagella,cal=TeX Gyre Termes,bold-cal=TeX Gyre Termes,frak=TeX Gyre Pagella,Fira

    cjk=Apple SD Gothic Neo,roman
  </verbatim-code>

  The <em|main family> (<cpp|main_family>) is the first unconditional
  entry, or the family of the first entry if all entries are conditional.
  The conditions are a space separated list, all of which must hold; each
  condition is a <verbatim|\|>-separated list of alternatives, one of which
  must hold. The alternatives are tested in
  <cpp|smart_font_rep::resolve (string c, string fam, int attempt)> and may
  be:

  <\itemize>
    <item>a feature of the requested logical font, as computed by
    <cpp|logical_font (family, variant, series, rshape)>; for instance
    <verbatim|bold>, <verbatim|italic> or <verbatim|mathitalic>;

    <item>a <name|Unicode> range name as returned by
    <cpp|get_unicode_range>: <verbatim|ascii>, <verbatim|latin>,
    <verbatim|greek>, <verbatim|cyrillic>, <verbatim|cjk> (which also
    accepts <verbatim|hangul> and <verbatim|hiragana>), <verbatim|hiragana>,
    <verbatim|hangul>, <verbatim|mathsymbols>, <verbatim|mathextra>,
    <verbatim|mathletters>, or one of the pseudo ranges
    <verbatim|mathlarge>, <verbatim|mathbigop> (big operators) and
    <verbatim|mathrubber> (wide accents and extensible delimiters), see
    <cpp|in_unicode_range>;

    <item>the name of a mathematical alphabet as returned by
    <cpp|substitute_math_letter (c, 2)>, such as <verbatim|cal>,
    <verbatim|bold-cal>, <verbatim|frak>, <verbatim|bbb>,
    <verbatim|bold-math> or <verbatim|ss>: the condition holds for the
    corresponding <name|Unicode> mathematical alphanumeric symbols;

    <item>the character itself;

    <item>a <em|character collection> defined in <cpp|init_collections>:
    <verbatim|digit>, <verbatim|lowercase-latin>,
    <verbatim|uppercase-latin>, <verbatim|latin>, the corresponding
    <verbatim|-bold> collections (characters <verbatim|\<less\>b-a\<gtr\>>
    and so on), <verbatim|lowercase-greek>, <verbatim|uppercase-greek>,
    <verbatim|greek>, their bold versions, and
    <verbatim|basic-letters> (digits, Latin and Greek letters, plain and
    bold);

    <item>a collection name preceded by <verbatim|!>, which holds for the
    characters <em|not> in that collection;

    <item>a code point range <verbatim|<em|c1>:<em|c2>>, where <em|c1> and
    <em|c2> are characters.
  </itemize>

  If all conditions hold, <em|family> is tried for the character in the
  same way as an unconditional entry; otherwise the entry is skipped.

  In mathematical shapes (<cpp|math_fix>), the condition <verbatim|math>
  is removed from conditional entries, so that
  <verbatim|math=TeX Gyre Termes> becomes the plain entry
  <verbatim|TeX Gyre Termes> in mathematics and is ignored in text. The
  routines <cpp|tex_gyre_fix> and <cpp|kepler_fix> rename the short names
  <verbatim|bonum>, <verbatim|pagella>, <verbatim|schola>,
  <verbatim|termes> to the <verbatim|TeX Gyre> families and append or
  remove the suffix <verbatim| Math> according to whether a medium
  mathematical shape is requested, so that the dedicated <name|OpenType>
  math fonts are used in formulas and the text fonts elsewhere.

  The last fix, <cpp|profile_fix>, generalizes this to the profiled
  <name|OpenType> math fonts (<verbatim|math_font_profiles.cpp>). For each
  unconditional entry, in a mathematical shape a text family is replaced by
  the math font of its profile when that font is installed, and in a text
  shape a math family by its text companion; the variants <verbatim|ss>
  and <verbatim|tt> are replaced by the sans serif and typewriter
  companions which the profile declares, and the result is translated into
  its master (<cpp|font_database_master>), since the selection is driven by
  masters. A profiled font, or the text companion of one, which is
  installed but absent from the database is registered on the fly
  (<cpp|register_profiled_font>). See <hlink|math font profiles, shipped
  fonts and the database|opentype-profiles.en.tm>.

  <section|The <cpp|smart_font_rep> class>

  <subsection|Fields>

  <\cpp-code>
    struct smart_font_rep: font_rep {

    \ \ string mfam;\ \ \ \ \ \ \ \ // main family

    \ \ string family;\ \ \ \ \ \ // full family list

    \ \ string variant;

    \ \ string series;

    \ \ string shape;\ \ \ \ \ \ \ // requested shape

    \ \ string rshape;\ \ \ \ \ \ // "right" for the math shapes, otherwise shape

    \ \ int\ \ \ \ sz;

    \ \ int\ \ \ \ hdpi;

    \ \ int\ \ \ \ dpi;

    \ \ int\ \ \ \ math_kind;\ \ \ // 0: no math, 1: mathitalic, 2: mathupright, 3: mathshape

    \ \ int\ \ \ \ italic_nr;\ \ \ // subfont for isolated italic letters

    \ \ bool\ \ \ ot_math;\ \ \ \ \ // main font: untuned OpenType math font

    \;

    \ \ array\<less\>font\<gtr\> fn;\ \ \ \ \ // the subfonts

    \ \ smart_map\ \ \ sm;\ \ \ \ \ // shared character -\<gtr\> subfont table

    \ \ array\<less\>int\<gtr\> origins; // debug switch "fonts": route of each subfont

    \ \ ...

    };
  </cpp-code>

  <subsection|The smart map>

  The <cpp|smart_map> is a resource whose name is the concatenation of
  family, variant, series and shape (<cpp|get_smart_map>). Since the size
  and resolution are not part of the key, all smart fonts which only
  differ by size or zoom share one smart map, and characters are resolved
  only once per logical font. The map stores:

  <\description>
    <item*|<cpp|fn_spec>>An array of trees describing the subfonts, such as
    <verbatim|("main")>, <verbatim|("error")>,
    <verbatim|("TeX Gyre Pagella" "rm" "medium" "right" "1")>,
    <verbatim|("virtual" "tradi-long")> or <verbatim|("poor-bold")>.
    Entries <cpp|SUBFONT_MAIN= 0> and <cpp|SUBFONT_ERROR= 1> always exist.

    <item*|<cpp|fn_nr>>The inverse table, from specification to number.

    <item*|<cpp|fn_rewr>>For every subfont, the kind of rewriting
    (<verbatim|REWRITE_*>, see below) which must be applied to strings
    before they are passed to it.

    <item*|<cpp|chv>>A table of 256 integers for the one-byte (Cork)
    characters, with <cpp|-1> for \Pnot yet resolved\Q.

    <item*|<cpp|cht>>A hash table for the universal symbols
    <verbatim|\<less\>...\<gtr\>>, with default <cpp|-1>.
  </description>

  <cpp|add_font (fn, rewr)> registers a subfont specification and returns
  its number; <cpp|add_char (fn, c)> registers the font if needed and
  records that <src-arg|c> is rendered by it. If a character is added
  twice, the smaller subfont number wins.

  The actual fonts live in the array <cpp|fn> of each
  <cpp|smart_font_rep>. They are created on demand by
  <cpp|smart_font_rep::initialize_font (nr)> from the specification
  <cpp|sm-\<gtr\>fn_spec[nr]>. Since the map is shared, a smart font may
  find in its map a subfont number for which its own <cpp|fn> array has no
  font yet; every user of a subfont number therefore checks
  <cpp|N(fn) \<less\>= nr \|\| is_nil (fn[nr])> and calls
  <cpp|initialize_font> first.

  <subsection|Construction and the mathematical shapes>

  The constructor installs the main font and the error font (both
  horizontally magnified by <cpp|adjust_subfont> if
  <cpp|hdpi != dpi>) and copies the mathematical parameters of the base
  font (<cpp|copy_math_pars>). For the shapes <verbatim|mathitalic>,
  <verbatim|mathupright> and <verbatim|mathshape> it does more. The flag
  <cpp|ot_math> is set when the base font has the math type
  <cpp|MATH_TYPE_OPENTYPE> (an <name|OpenType> math font without hand-tuned
  tables) and its profile does not say that the letters come from the text
  italic (key <verbatim|letters>):

  <\itemize>
    <item>For the historical <TeX> families recognized by
    <cpp|is_math_family> (<verbatim|roman>, <verbatim|concrete>,
    <verbatim|Euler>, <verbatim|ENR>), the main font is replaced, except for
    <verbatim|mathupright>, by a subfont <verbatim|("math" mfam variant
    series "right")> with rewriting <cpp|REWRITE_MATH>; this subfont is
    obtained from the rule based font selection (<cpp|get_math_font>, with
    the variant <verbatim|mr>, <verbatim|ms> or <verbatim|mt>), which
    typically yields a <cpp|math_font> (see <hlink|compound and math
    fonts|smart-fonts-compound.en.tm>).

    <item>For all other families, <cpp|math_kind> is set to 1, 2 or 3. For
    <verbatim|mathitalic> and <verbatim|mathshape> an italic subfont
    <verbatim|("fast-italic")> is created; its number is stored in
    <cpp|italic_nr> and its mathematical parameters are used for the smart
    font. When <cpp|ot_math> is set, the subfont of <verbatim|mathitalic>
    is <verbatim|("ot-italic")> instead: it is the main font itself, with
    the rewriting <cpp|REWRITE_MATH_ITALIC>, which maps the Latin letters to
    the mathematical italic alphabet of <name|Unicode> (<verbatim|U+1D434>
    and following, with the Planck constant <verbatim|U+210E> for
    <verbatim|h>), so that the italic corrections and the cut-in kerns of
    the font apply to them. Then a fixed list of subfonts is pre-registered, so that they get
    the same numbers in all smart maps: <verbatim|special>,
    <verbatim|emu-bracket>, <verbatim|other>, <verbatim|regular>, the
    mathematical alphabets <verbatim|bold-math>, <verbatim|italic-math>,
    <verbatim|bold-italic-math>, <verbatim|cal>, <verbatim|bold-cal>,
    <verbatim|frak>, <verbatim|bold-frak>, <verbatim|bbb>, <verbatim|tt>,
    <verbatim|ss>, <verbatim|bold-ss>, <verbatim|italic-ss>,
    <verbatim|bold-italic-ss>, and <verbatim|italic-roman>.
  </itemize>

  <section|Data flow when typesetting a string>

  <subsection|Cutting a string into runs>

  The heart of the smart font is the routine

  <\explain>
    <cpp|void smart_font_rep::advance (string s, int& pos, string& r, int&
    nr)><explain-synopsis|extract the next run>
  <|explain>
    Starting at <src-arg|pos>, scan the longest run of characters which are
    rendered by the same subfont. On return <src-arg|pos> points after the
    run, <src-arg|nr> is the subfont number (or <cpp|-1>) and
    <src-arg|r> is the run, already rewritten according to
    <cpp|sm-\<gtr\>fn_rewr[nr]>.
  </explain>

  For every character, the subfont is first looked up in <cpp|chv> (one
  byte characters) or <cpp|cht> (universal symbols, delimited with
  <cpp|tm_char_forwards>); if the entry is <cpp|-1>, the full resolution
  algorithm <cpp|resolve (c)> is run, which fills the table. A few
  special rules apply while building a run:

  <\itemize>
    <item>If <cpp|math_kind> is 1 or 3, an ASCII letter which has no
    alphabetic neighbour in the string is sent to the italic subfont
    <cpp|italic_nr>. This is how the variable <math|x> is rendered in italic
    while the operator name <verbatim|sin> (a single string of three
    letters) stays upright.

    <item>If the second one-byte character of a run belongs to a subfont
    with rewriting <cpp|REWRITE_SPECIAL>, the run is cut, so that strings
    like <verbatim|---> are rewritten character by character.

    <item>If the second universal symbol of a run belongs to the same
    subfont, the run is only extended if the subfont <cpp|supports> the
    two-symbol string as a whole; otherwise the symbols are processed
    separately.
  </itemize>

  <subsection|Measuring and drawing>

  All the public routines of the smart font are written in terms of
  <cpp|advance>:

  <\cpp-code>
    void

    smart_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {

    \ \ int i=0, n= N(s);

    \ \ while (i \<less\> n) {

    \ \ \ \ int nr;

    \ \ \ \ string r= s;

    \ \ \ \ metric ey;

    \ \ \ \ advance (s, i, r, nr);

    \ \ \ \ if (nr \<gtr\>= 0) {

    \ \ \ \ \ \ fn[nr]-\<gtr\>draw_fixed (ren, r, x, y);

    \ \ \ \ \ \ if (i \<less\> n) {

    \ \ \ \ \ \ \ \ fn[nr]-\<gtr\>get_extents (r, ey);

    \ \ \ \ \ \ \ \ x += ey-\<gtr\>x2;

    \ \ \ \ \ \ }

    \ \ \ \ }

    \ \ }

    }
  </cpp-code>

  <cpp|get_extents> combines the metrics of the successive runs: the
  logical widths are added, the vertical logical and ink extents are
  merged. <cpp|get_xpositions> fills the array of cursor positions; when a
  run was rewritten (so that the rewritten string <src-arg|r> has a
  different length than the original run), the positions inside the run
  are set to the start of the run and only the end position is taken from
  the subfont. The kerning variants with an extra argument <cpp|xk> are
  implemented in the same way.

  The slopes and corrections used by the <hlink|mathematical
  typesetter|maths.en.tm> are delegated to the subfont of the first run
  (<cpp|get_left_slope>, <cpp|get_left_correction>,
  <cpp|get_lsub_correction>, <cpp|get_lsup_correction>) or of the last run
  (<cpp|get_right_slope>, <cpp|get_right_correction>,
  <cpp|get_rsub_correction>, <cpp|get_rsup_correction>,
  <cpp|get_wide_correction>), and so are the height dependent script
  corrections <cpp|get_lsub_correction_at>, ...,
  <cpp|get_rsup_correction_at>. The questions which concern a single glyph
  (<cpp|get_top_accent>, <cpp|is_extended_shape>,
  <cpp|get_feature_variant>) go to the subfont of the first character;
  <cpp|get_rubber_variant> and <cpp|get_wide_variant> go to the subfont
  which renders the numbered sizes of the character (<cpp|rubber_subfont>
  probes <verbatim|\<less\><em|name>-0\<gtr\>>), since the unnumbered
  name may be routed elsewhere. <cpp|make_rubber_font> hands the building
  of the extensible font to the main font when it carries a
  <verbatim|MATH> table. <cpp|supports> always returns <cpp|true>: a smart
  font renders every character, if necessary with the error font.

  When the debug switch <verbatim|fonts> is on (<cpp|DEBUG_FONTS>),
  <cpp|draw_fixed> calls <cpp|debug_draw> instead of the subfont, which
  draws each run in the colour of its route (<cpp|debug_origin>, computed
  once per subfont from its specification); <cpp|debug_info> reports the
  route of one character from the routing tables, without resolving
  anything, for the font inspector. See <hlink|inspecting the font
  system|opentype-tools.en.tm>.

  <subsection|Glyph extraction and the renderers>

  Several clients need the actual glyph of a symbol rather than a drawing
  operation: virtual fonts compile their glyphs from bitmaps of the base
  font, emulated fonts transform bitmaps, and the renderers draw glyphs
  through <cpp|renderer_rep::draw (int char_code, font_glyphs fn, SI x, SI
  y)>. The smart font forwards these requests to the subfont of the first
  run:

  <\explain>
    <cpp|glyph smart_font_rep::get_glyph (string s)>

    <cpp|int smart_font_rep::index_glyph (string s, font_metric& fnm,
    font_glyphs& fng)><explain-synopsis|glyph access>
  <|explain>
    <cpp|get_glyph> returns the bitmap of the (first) symbol of
    <src-arg|s>, <cpp|index_glyph> returns its index in a table of glyphs
    and metrics, or <cpp|-1>. The default implementations in
    <cpp|font_rep> print a warning (or fail if <cpp|get_glyph_fatal> is
    set), so a font which is used as the base of a virtual or emulated font
    must implement them.
  </explain>

  <cpp|advance_glyph> moves to the next glyph boundary, taking the
  ligatures of the subfont into account; emulated fonts use it to draw
  glyph by glyph.

  The drawing itself is not done by the smart font but by the leaf fonts.
  Note that <cpp|draw_fixed> draws at the font's own resolution; the
  public <cpp|font_rep::draw> (<verbatim|Graphics/Fonts/font.cpp>) takes
  care of zooming. When the renderer has a zoom factor different from one
  and is not a printer, <cpp|draw> creates (and caches in
  <cpp|zoomed_fn>) a magnified version of the font and draws with it. For
  a smart font, <cpp|magnify (zoomx, zoomy)> returns
  <cpp|smart_font_bis> with the scaled resolutions, so that all subfonts,
  including virtual and emulated ones, are rebuilt at the screen
  resolution. Bitmap based constructions are therefore always computed at
  the right resolution.

  <section|Rewriting>

  Some subfonts do not use the <TeXmacs> encoding of the characters they
  render. The rewriting kind of each subfont is stored in
  <cpp|fn_rewr>, and <cpp|rewrite (s, kind)> is applied to each run in
  <cpp|advance>:

  <\description>
    <item*|<cpp|REWRITE_NONE>>No change.

    <item*|<cpp|REWRITE_MATH>>Characters of the form
    <verbatim|\<less\>#...\<gtr\>> are converted into the corresponding
    <TeXmacs> names (<cpp|rewrite_math>, via <cpp|strict_cork_to_utf8> and
    <cpp|utf8_to_cork>), for the old <TeX> math fonts.

    <item*|<cpp|REWRITE_CYRILLIC>>Conversion to the <verbatim|T2A> encoding
    of the <TeX> Cyrillic fonts
    (<cpp|code_point_to_cyrillic_subset_in_t2a>).

    <item*|<cpp|REWRITE_LETTERS>><name|Unicode> mathematical alphanumeric
    symbols are replaced by the plain letters, digits or Greek letters they
    are derived from (<cpp|rewrite_letters>, using the tables built by
    <cpp|init_unicode_substitution>); the subfont is the corresponding
    alphabet (bold, calligraphic, double struck, ...).

    <item*|<cpp|REWRITE_SPECIAL>>The fixed table built by <cpp|is_special>:
    <verbatim|-> becomes <verbatim|\<less\>minus\<gtr\>>,
    <verbatim|\|> becomes <verbatim|\<less\>mid\<gtr\>>,
    <verbatim|'> becomes <verbatim|\<less\>#2B9\<gtr\>>, <verbatim|`>
    becomes <verbatim|\<less\>backprime\<gtr\>>,
    <verbatim|\<less\>hat\<gtr\>> and <verbatim|\<less\>tilde\<gtr\>>
    become spacing accents, the invisible symbols
    <verbatim|\<less\>noplus\<gtr\>>, <verbatim|\<less\>nocomma\<gtr\>>,
    ..., <verbatim|*> and the invisible big operators
    <verbatim|\<less\>big-.-1\<gtr\>>, <verbatim|\<less\>big-.-2\<gtr\>>
    become empty, and
    big operators like <verbatim|\<less\>big-sum-1\<gtr\>> become the
    <name|Unicode> symbol <verbatim|\<less\>sum\<gtr\>> when it exists.

    <item*|<cpp|REWRITE_EMULATE>><verbatim|\<less\>name\<gtr\>> becomes
    <verbatim|\<less\>emu-name\<gtr\>>, for the virtual font
    <verbatim|emu-bracket>.

    <item*|<cpp|REWRITE_POOR_BBB>><verbatim|\<less\>bbb-X\<gtr\>> becomes
    <verbatim|X>.

    <item*|<cpp|REWRITE_ITALIC_GREEK>>Greek letters are mapped to the
    mathematical italic Greek alphabet (<verbatim|U+1D6E2> and following,
    <cpp|substitute_italic_greek>).

    <item*|<cpp|REWRITE_UPRIGHT_GREEK>><verbatim|\<less\>upalpha\<gtr\>> or
    <verbatim|\<less\>up-alpha\<gtr\>> becomes
    <verbatim|\<less\>alpha\<gtr\>> (<cpp|substitute_upright_greek>).

    <item*|<cpp|REWRITE_UPRIGHT>, <cpp|REWRITE_ITALIC>><verbatim|\<less\>up-x\<gtr\>>
    and <verbatim|\<less\>it-x\<gtr\>> become <verbatim|x>
    (<cpp|substitute_upright>, <cpp|substitute_italic>).

    <item*|<cpp|REWRITE_MATH_ITALIC>>Latin letters are mapped to the
    mathematical italic alphabet (<verbatim|U+1D434> and following, with
    <verbatim|U+210E> for <verbatim|h>; <cpp|substitute_math_italic>), for
    the subfont <verbatim|ot-italic> of an <name|OpenType> math font.

    <item*|<cpp|REWRITE_IGNORE>>The empty string. Used for characters which
    must not be drawn: the null delimiters such as
    <verbatim|\<less\>left-.\<gtr\>> and
    <verbatim|\<less\>right-nobracket\<gtr\>>, and the ideographic space
    <verbatim|\<less\>#3000\<gtr\>> when the fonts do not provide it.
  </description>

  Rewriting is applied to the whole run, which is why
  <cpp|get_xpositions> has to handle runs whose rewritten length differs
  from the original length.

  <section|The kinds of subfonts>

  The first element of a subfont specification determines how
  <cpp|initialize_font> creates it. The following kinds exist:

  <\description-paragraphs>
    <item*|<verbatim|main>, <verbatim|error>>The base font and the error
    font of the constructor.

    <item*|<verbatim|(<em|fam> <em|var> <em|ser> <em|sh> <em|attempt>)>>The
    generic case: <cpp|closest_font (fam, var, ser, sh, sz, ndpi,
    attempt)>, where <verbatim|ndpi> is computed by
    <cpp|adjusted_dpi> so that the x-height (characteristic
    <verbatim|ex> in the font database) of the fallback font matches the
    one of the main family.

    <item*|<verbatim|math>, <verbatim|greek>, <verbatim|cyrillic>>Old style
    <TeX> fonts for the historical families, obtained through
    <cpp|find_closest> and the rule based <cpp|find_font>
    (<cpp|get_math_font>, <cpp|get_greek_font>, <cpp|get_cyrillic_font>).

    <item*|<verbatim|subfont>>A whole smart font for another family, used
    for Greek and bold letters in mathematical italic shape.

    <item*|<verbatim|special>>The same smart font in upright shape.

    <item*|<verbatim|fast-italic>, <verbatim|italic-math>,
    <verbatim|it>>The same family in italic shape.

    <item*|<verbatim|bold-math>, <verbatim|bold-italic-math>,
    <verbatim|tt>, <verbatim|ss>, <verbatim|bold-ss>,
    <verbatim|italic-ss>, <verbatim|bold-italic-ss>, <verbatim|cal>,
    <verbatim|bold-cal>, <verbatim|frak>, <verbatim|bold-frak>,
    <verbatim|bbb>>Mathematical alphabets: smart fonts for the same family
    with variant <verbatim|tt>, <verbatim|ss>, <verbatim|calligraphic>,
    <verbatim|gothic> or <verbatim|outline> and the appropriate series and
    shape. These are again resolved through the font database, and are
    emulated if the variant does not exist.

    <item*|<verbatim|italic-roman>, <verbatim|other>>Smart fonts for
    shape <verbatim|mathitalic> (for <verbatim|other>: family
    <verbatim|roman>, at an adjusted resolution), used for symbols which no
    <name|Unicode> font provides and which are only found in the old
    <TeX> fonts.

    <item*|<verbatim|up>, <verbatim|upright-greek>,
    <verbatim|italic-greek>, <verbatim|ot-italic>, <verbatim|ignore>>The
    main font itself, with the corresponding rewriting.

    <item*|<verbatim|shipped-math>><cpp|unicode_font
    ("STIXTwoMath-Regular", sz, dpi)>, the math font shipped with
    <TeXmacs>, for the symbols which the main font lacks and whose
    emulation could only be exported as a bitmap (see <hlink|the
    resolution algorithm|smart-fonts-resolve.en.tm>).

    <item*|<verbatim|virtual>>
    <cpp|virtual_font (this, name, sz, hdpi, dpi, false)>: a virtual font
    whose base is the smart font itself (see <hlink|virtual
    fonts|smart-fonts-virtual.en.tm>).

    <item*|<verbatim|emu-bracket>>Same, for <verbatim|emu-bracket>.

    <item*|<verbatim|emulate>>A virtual font with base the <em|main
    font> in <em|extend> mode, stacked on top of
    <verbatim|emu-fundamental>:

    <\cpp-code>
      font vfn= fn[SUBFONT_MAIN];

      if (a[1] != "emu-fundamental")

      \ \ vfn= virtual_font (vfn, "emu-fundamental", sz, hdpi, dpi, true);

      fn[nr]= virtual_font (vfn, a[1], sz, hdpi, dpi, true);
    </cpp-code>

    <item*|<verbatim|poor-bold>><cpp|poor_bold_font> of the medium series
    of the same smart font.

    <item*|<verbatim|(poor-bbb <em|penw> <em|penh>)>><cpp|poor_bbb_font>
    of the upright smart font.

    <item*|<verbatim|(rubber <em|nr>)>><cpp|rubber_font (fn[nr])>: the
    extensible version of another subfont (see <hlink|emulated
    fonts|smart-fonts-emulated.en.tm>).
  </description-paragraphs>

  After creation, <cpp|initialize_font> checks that the new subfont is not
  the smart font itself (same <cpp|res_name>), and aborts with
  <verbatim|"substitution font loop detected"> otherwise.

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
