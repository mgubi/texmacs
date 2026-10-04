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
    case 2:

    \ \ fn= smart_font (get_string (MATH_FONT), get_string (MATH_FONT_FAMILY),

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_string (MATH_FONT_SERIES), get_string (MATH_FONT_SHAPE),

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_string (FONT), get_string (FONT_FAMILY),

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_string (FONT_SERIES), "mathitalic",

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ get_script_size (fn_size, index_level), (int) (magn*dpi));

    \ \ break;

    ...

    string eff= get_string (FONT_EFFECTS);

    if (N(eff) != 0) fn= apply_effects (fn, eff);
  </cpp-code>

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
    <verbatim|mt> select the variants <verbatim|ss> and <verbatim|tt>; and
    if the mathematical shape is <verbatim|right>, the shape becomes the
    special shape <verbatim|mathupright>. The result is the six argument
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
      <cpp|kepler_fix> and <cpp|math_fix> (see below);

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

    \;

    \ \ array\<less\>font\<gtr\> fn;\ \ \ \ \ // the subfonts

    \ \ smart_map\ \ \ sm;\ \ \ \ \ // shared character -\<gtr\> subfont table

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
  <verbatim|mathupright> and <verbatim|mathshape> it does more:

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
    font. Then a fixed list of subfonts is pre-registered, so that they get
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
  <cpp|get_wide_correction>). <cpp|supports> always returns <cpp|true>:
  a smart font renders every character, if necessary with the error font.

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
    <verbatim|italic-greek>, <verbatim|ignore>>The main font itself, with
    the corresponding rewriting.

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
