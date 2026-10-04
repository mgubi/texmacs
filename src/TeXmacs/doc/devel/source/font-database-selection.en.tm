<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The font selection algorithm>

  <section|From the environment to a font>

  The typesetter recomputes its current font in
  <cpp|edit_env_rep::update_font> (<verbatim|Typeset/Env/env_semantics.cpp>)
  whenever an environment variable of type <cpp|Env_Font> or
  <cpp|Env_Font_Size> changes, as well as on changes of the mode, the
  magnification or the script level. The size is
  <src-var|font-base-size> times <src-var|font-size>, reduced for scripts by
  <cpp|get_script_size>. Then, depending on the mode
  (<cpp|edit_env_rep::make_current_font>):

  <\itemize>
    <item>in text mode, <cpp|smart_font> is called with the values of
    <src-var|font>, <src-var|font-family>, <src-var|font-series> and
    <src-var|font-shape>;

    <item>in math mode, the ten argument variant of <cpp|smart_font> is
    called with the values of <src-var|math-font>,
    <src-var|math-font-family>, <src-var|math-font-series>,
    <src-var|math-font-shape>, followed by the text font, family and series
    and the shape <verbatim|mathitalic>;

    <item>in program mode, the ten argument variant is called with
    <src-var|prog-font>, <src-var|prog-font-family>,
    <src-var|prog-font-series>, <src-var|prog-font-shape>, followed by the
    text font settings, where the text variant gets the suffix
    <verbatim|-tt>.
  </itemize>

  When the resulting font carries an <name|OpenType> <verbatim|MATH> table
  and the script level is positive, the size is recomputed from the
  percentages of the table (<cpp|script_percent>,
  <cpp|script_script_percent>), unless <src-var|math-font-sizes> is set, and
  an untuned <name|OpenType> math font is shown through its
  <verbatim|ssty> feature (<cpp|feature_font>); see <hlink|mathematics from
  the <verbatim|MATH> table|opentype-math.en.tm>. Finally, if
  <src-var|font-features> is not empty, the result is wrapped by
  <cpp|apply_features> (see <hlink|<name|OpenType>
  features|opentype-features.en.tm>), and if <src-var|font-effects> is not
  empty, by <cpp|apply_effects>. The ten argument variant of <cpp|smart_font> reduces
  to the six argument one: if the preference <verbatim|"new style fonts">
  is disabled, it directly returns <cpp|find_font> applied to its first four
  arguments; otherwise it uses the second group of arguments, except that a
  text font <verbatim|roman> is replaced by the first family, the math
  variants <verbatim|ms> and <verbatim|mt> are mapped to <verbatim|ss> and
  <verbatim|tt>, a series other than <verbatim|medium> of the first group
  replaces the text series (so that <src-var|math-font-series> reaches the
  math font), and the shape <verbatim|right> is mapped to
  <verbatim|mathupright>.

  The six argument <cpp|smart_font> calls <cpp|smart_font_bis>. When the
  variant is not <verbatim|rm> and resolves to another physical family than
  the <verbatim|rm> variant, the resulting font is magnified so that its
  x-height matches the x-height of the <verbatim|rm> font. In
  <cpp|smart_font_bis>:

  <\itemize>
    <item>if <cpp|new_fonts> is false, the old mechanism is used:
    <cpp|find_font (family, variant, series, shape, sz, dpi)>;

    <item>families starting with <verbatim|tc> are also handled by
    <cpp|find_font> (symbols of <verbatim|std-symbol.ts>);

    <item>the families <verbatim|sys-chinese>, <verbatim|sys-japanese> and
    <verbatim|sys-korean> are rewritten into <verbatim|cjk=Name,roman>,
    where <verbatim|Name> is the result of
    <cpp|default_chinese_font_name> (resp. the Japanese and Korean
    variants);

    <item>a few family name fixes are applied (<cpp|tex_gyre_fix>,
    <cpp|kepler_fix>, <cpp|math_fix>, and last <cpp|profile_fix>, which
    replaces a text family by the math font of its profile in math shapes,
    a math family by its text companion in text shapes, takes the sans
    serif and typewriter companions declared by the profile, translates the
    result into its master, and registers a profiled font which is
    installed but missing from the database; see <hlink|math font
    profiles|opentype-profiles.en.tm>);

    <item>the base font is <cpp|closest_font (main_family (family), variant,
    series, shape, sz, dpi)>, where <cpp|main_family> extracts the main
    family from a comma separated list like <verbatim|cjk=SimSun,roman>,
    and the shapes <verbatim|mathitalic> and <verbatim|mathshape> are
    replaced by <verbatim|right>;

    <item>the error font is derived from <cpp|closest_font ("roman", "ss",
    "medium", "right", sz, dpi)>.
  </itemize>

  The construction of the smart font itself, and the meaning of the comma
  separated family lists, are described in the chapter on <hlink|smart
  fonts|smart-fonts.en.tm>. The rest of this chapter describes
  <cpp|closest_font> and the functions it relies on.

  <section|Features and logical fonts>

  <subsection|Kinds of features>

  A <em|feature> is a lower case word. The predicates in
  <verbatim|Graphics/Fonts/font_select.cpp> classify features into the
  following kinds; two features are of the <em|same kind> (<cpp|same_kind>)
  if they belong to the same row. The last column gives the default value,
  which is assumed when a font does not mention any feature of that kind.

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Kind>|<cell|Predicate>|<cell|Values>|<cell|Default>>|<row|<cell|stretch>|<cell|<cpp|is_stretch>>|<cell|<verbatim|...condensed>,
  <verbatim|...unextended>, <verbatim|...wide>,
  <verbatim|...caption>>|<cell|<verbatim|unextended>>>|<row|<cell|weight>|<cell|<cpp|is_weight>>|<cell|<verbatim|...thin>,
  <verbatim|...light>, <verbatim|regular>, <verbatim|medium>,
  <verbatim|...bold>, <verbatim|...heavy>,
  <verbatim|...black>>|<cell|<verbatim|medium>>>|<row|<cell|slant>|<cell|<cpp|is_slant>>|<cell|<verbatim|upright>,
  <verbatim|italic>, <verbatim|oblique>, <verbatim|mathitalic>,
  <verbatim|mathupright>, <verbatim|mathshape>>|<cell|<verbatim|normal>>>|<row|<cell|capitalization>|<cell|<cpp|is_capitalization>>|<cell|<verbatim|mixed>,
  <verbatim|smallcaps>>|<cell|<verbatim|mixed>>>|<row|<cell|serif>|<cell|<cpp|is_serif>>|<cell|<verbatim|serif>,
  <verbatim|sansserif>>|<cell|<verbatim|serif>>>|<row|<cell|spacing>|<cell|<cpp|is_spacing>>|<cell|<verbatim|proportional>,
  <verbatim|mono>, <verbatim|typewriter>>|<cell|<verbatim|proportional>>>|<row|<cell|device>|<cell|<cpp|is_device>>|<cell|<verbatim|print>,
  <verbatim|typewriter>, <verbatim|digital>, <verbatim|pen>,
  <verbatim|artpen>, <verbatim|chalk>,
  <verbatim|marker>>|<cell|<verbatim|print>>>|<row|<cell|category>|<cell|<cpp|is_category>>|<cell|<verbatim|ancient>,
  <verbatim|attached>, <verbatim|calligraphic>, <verbatim|comic>,
  <verbatim|decorative>, <verbatim|distorted>, <verbatim|gothic>,
  <verbatim|handwritten>, <verbatim|initials>, <verbatim|medieval>,
  <verbatim|miscellaneous>, <verbatim|outline>, <verbatim|retro>,
  <verbatim|scifi>, <verbatim|title>>|<cell|>>|<row|<cell|glyphs>|<cell|<cpp|is_glyphs>>|<cell|<verbatim|ascii>,
  <verbatim|latin>, <verbatim|greek>, <verbatim|cyrillic>,
  <verbatim|cjk>, <verbatim|hangul>, <verbatim|mathsymbols>,
  <verbatim|mathextra>, <verbatim|mathletters>>|<cell|>>>>>>
    Kinds of font features. The notation <verbatim|...bold> stands for all
    words ending with <verbatim|bold>, such as <verbatim|semibold> or
    <verbatim|extrabold>.
  </big-table>

  The word <verbatim|typewriter> is both a spacing and a device. All other
  words, except <verbatim|long> and <verbatim|flat>, are <em|other>
  features (<cpp|is_other>); they usually come from unusual style names
  (<verbatim|Display>, <verbatim|Poster>, ...). The function
  <cpp|remove_other> removes them from a logical font (and optionally the
  glyph features too).

  Features are normalized by <cpp|normalize_feature> (lower case,
  <verbatim|ultralight> into <verbatim|thin>, <verbatim|extended> and
  <verbatim|caption> into <verbatim|wide>, <verbatim|nonextended> into
  <verbatim|unextended>), encoded for storage by <cpp|encode_feature>
  (<verbatim|smallcaps> into <verbatim|SmallCaps>, <verbatim|semibold> into
  <verbatim|SemiBold>, ...) and decoded from user interface strings by
  <cpp|decode_feature> (<verbatim|Small Capitals> into <verbatim|smallcaps>,
  <verbatim|Monospaced> into <verbatim|mono>, spaces removed).

  <subsection|Logical fonts>

  A <em|logical font> is an <cpp|array\<less\>string\<gtr\>> whose first
  element is a family or master name and whose other elements are features.
  The following functions compute logical fonts for physical fonts
  <verbatim|(family, style)>:

  <\explain>
    <cpp|string family_to_master (string f)><explain-synopsis|master of a
    family>
  <|explain>
    If <cpp|f> is a list like <verbatim|cjk=SimSun,roman>, it is first
    replaced by its main family; old names are upgraded
    (<cpp|upgrade_family_name>). If the family has features, the master is
    the first of them. Otherwise, the global database is loaded (see
    <hlink|lazy loading|font-database-storage.en.tm>) and the master is
    obtained by removing words like <verbatim|Mono>, <verbatim|Console>,
    <verbatim|Typewriter>, <verbatim|Sans>, <verbatim|Serif>,
    <verbatim|Condensed>, <verbatim|Narrow>, <verbatim|Light>,
    <verbatim|Bold>, <verbatim|Black>, ... from the name (except at its
    very beginning). For instance, the master of an unknown family
    <verbatim|Foo Sans Mono> is <verbatim|Foo>.
  </explain>

  <\explain>
    <cpp|array\<less\>string\<gtr\> master_to_families (string
    m)><explain-synopsis|families of a master>
  <|explain>
    The families with master <cpp|m> according to <cpp|font_variants>, or
    just <cpp|m> if there are none.
  </explain>

  <\explain>
    <cpp|array\<less\>string\<gtr\> family_features (string f)>

    <cpp|array\<less\>string\<gtr\> master_features (string m)>

    <cpp|array\<less\>string\<gtr\> family_strict_features (string
    f)><explain-synopsis|features of families and masters>
  <|explain>
    The features of a family come from <cpp|font_features> (without the
    master) or, for unknown families, are guessed from the name
    (<verbatim|Mono>, <verbatim|Sans>, <verbatim|Condensed>,
    <verbatim|Pen>, ...). The features of a master are the features which
    are common to all its families. The <em|strict> features of a family are
    its features which are not features of its master: they are the ones
    that distinguish a family from its siblings.
  </explain>

  <\explain>
    <cpp|array\<less\>string\<gtr\> style_features (string
    s)><explain-synopsis|features of a style name>
  <|explain>
    Splits a style name into words (also at lower case to upper case
    transitions, so that <verbatim|SemiBold> and <verbatim|Semi Bold> are
    treated alike), glues the prefixes <verbatim|Demi>, <verbatim|Extra>,
    <verbatim|Semi>, <verbatim|Ultra> and <verbatim|Small> to the next word,
    ignores neutral words (<verbatim|Regular>, <verbatim|Medium>,
    <verbatim|Normal>, <verbatim|Roman>, <verbatim|Upright>,
    <verbatim|Book>, <verbatim|Unextended>, ...), translates
    <verbatim|Slanted> into <verbatim|oblique>, <verbatim|Inclined> into
    <verbatim|italic> and <verbatim|Versalitas> into <verbatim|smallcaps>,
    and normalizes the result. For instance, <verbatim|SemiBold Condensed
    Italic> yields <verbatim|["semibold", "condensed", "italic"]>.
  </explain>

  <\explain>
    <cpp|array\<less\>string\<gtr\> logical_font (string family, string
    style)>

    <cpp|array\<less\>string\<gtr\> logical_font_exact (string family,
    string style)><explain-synopsis|logical descriptions of a physical
    font>
  <|explain>
    The first function returns the master, followed by the strict features
    of the family and the features of the style. The second one returns the
    master, followed by <em|all> features of the family, the features of the
    style and the glyph ranges of the characteristics
    (<cpp|glyph_features>); for <name|CJK> and <name|Hangul> fonts, the
    category <verbatim|gothic> is translated into <verbatim|sansserif>. For
    instance, for <verbatim|("DejaVu Sans Mono", "Bold")>, the first
    function returns <verbatim|["DejaVu", "mono", "sansserif", "bold"]>
    and the second one the same array followed by <verbatim|ascii>,
    <verbatim|latin>, <verbatim|greek>, <verbatim|cyrillic>,
    <verbatim|mathsymbols>.
  </explain>

  <\explain>
    <cpp|array\<less\>string\<gtr\> logical_font_enrich
    (array\<less\>string\<gtr\> v)><explain-synopsis|implicit features>
  <|explain>
    Returns the master <cpp|v[0]> followed by the features of the master,
    patched (<cpp|patch_font>) with the features of <cpp|v>. This is the
    set of features that a request implicitly has, because they hold for
    all families of its master.
  </explain>

  <subsection|Translation from and to the internal naming scheme>

  The four argument variant of <cpp|logical_font> in
  <verbatim|Graphics/Fonts/font_translate.cpp> translates an internal font
  description into a logical font:

  <\enumerate>
    <item>the family is upgraded by <cpp|upgrade_family_name>, which maps
    old <TeXmacs> names to database names (<verbatim|pagella> into
    <verbatim|TeX Gyre Pagella>, <verbatim|dejavu> into <verbatim|DejaVu>,
    <verbatim|ms-arial> into <verbatim|Arial>, <verbatim|modern> into
    <verbatim|roman>, <verbatim|sys-chinese> into
    <cpp|default_chinese_font_name ()>, <abbr|etc.>);

    <item>the variant is split at hyphens by <cpp|variant_features>:
    <verbatim|ss> gives <verbatim|sansserif>, <verbatim|tt> gives
    <verbatim|typewriter>, devices, categories, glyph ranges and other words
    are kept, while <verbatim|rm> is dropped;

    <item>the series is kept as is (<cpp|series_features>);

    <item>the shape is split by <cpp|shape_features>: <verbatim|right>
    gives <verbatim|upright>, <verbatim|slanted> gives
    <verbatim|oblique>, <verbatim|small-caps> gives <verbatim|smallcaps>,
    and stretches, <verbatim|italic>, the math shapes, <verbatim|mono>,
    <verbatim|proportional>, <verbatim|long> and <verbatim|flat> are kept;

    <item>the default values <verbatim|medium> and <verbatim|upright> are
    removed.
  </enumerate>

  For instance, <verbatim|("roman", "ss", "bold", "slanted")> becomes
  <verbatim|["roman", "sansserif", "bold", "oblique"]>, and
  <verbatim|("pagella", "rm-cjk", "medium", "right")> becomes
  <verbatim|["TeX Gyre Pagella", "cjk"]>.

  Conversely, <cpp|get_family>, <cpp|get_variant>, <cpp|get_series> and
  <cpp|get_shape> translate a logical font back. The family is the first
  element (hence a master name); the variant collects <verbatim|tt> (for
  <verbatim|mono> or <verbatim|typewriter>), <verbatim|ss>, devices,
  categories, glyph ranges and other features, joined by hyphens, with
  default <verbatim|rm>; the series is the first weight, with default
  <verbatim|medium>; the shape collects the stretch, the slant
  (<verbatim|right>, <verbatim|italic>, <verbatim|slanted>, math shapes)
  and the capitalization (<verbatim|small-caps>), with default
  <verbatim|right>.

  <section|Distances between fonts>

  <subsection|Distance between features>

  The function <cpp|distance (string s1, string s2, bool asym)> measures
  how badly a feature <cpp|s2> of a candidate font matches a requested
  feature <cpp|s1>. Features of different kinds are at distance
  <cpp|D_HUGE>. Otherwise the distance is given by the following constants,
  chosen so that a mismatch of a more important kind always outweighs any
  number of mismatches of less important kinds:

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Constant>|<cell|Value>|<cell|Situation>>|<row|<cell|<cpp|S_STRETCH>>|<cell|10>|<cell|two
  condensed, or two wide stretches>>|<row|<cell|<cpp|D_STRETCH>>|<cell|30>|<cell|other
  stretch mismatch>>|<row|<cell|<cpp|S_WEIGHT>>|<cell|100>|<cell|two
  light, two bold or two black weights (<verbatim|bold> versus
  <verbatim|semibold>)>>|<row|<cell|<cpp|S_SLANT>>|<cell|100>|<cell|<verbatim|italic>
  versus <verbatim|oblique>>>|<row|<cell|<cpp|N_WEIGHT>>|<cell|200>|<cell|<verbatim|bold>
  or <verbatim|black> versus <verbatim|heavy>>>|<row|<cell|<cpp|Q_WEIGHT>>|<cell|300>|<cell|<verbatim|light>
  versus <verbatim|thin>, <verbatim|bold> versus
  <verbatim|black>>>|<row|<cell|<cpp|D_WEIGHT>>|<cell|1000>|<cell|other
  weight mismatch>>|<row|<cell|<cpp|D_SLANT>>|<cell|1000>|<cell|other
  slant mismatch>>|<row|<cell|<cpp|D_CAPITALIZATION>>|<cell|3000>|<cell|capitalization
  mismatch>>|<row|<cell|<cpp|D_MASTER>>|<cell|10000>|<cell|different
  master>>|<row|<cell|<cpp|D_SERIF>>|<cell|100000>|<cell|serif
  mismatch>>|<row|<cell|<cpp|D_SPACING>>|<cell|100000>|<cell|spacing
  mismatch (<verbatim|mono> and <verbatim|typewriter> are
  equivalent)>>|<row|<cell|<cpp|Q_DEVICE>>|<cell|300000>|<cell|<verbatim|pen>
  versus <verbatim|artpen>, <verbatim|marker> or
  <verbatim|chalk>>>|<row|<cell|<cpp|D_DEVICE>>|<cell|1000000>|<cell|other
  device mismatch>>|<row|<cell|<cpp|Q_CATEGORY>>|<cell|300000>|<cell|<verbatim|retro>
  versus <verbatim|medieval>>>|<row|<cell|<cpp|D_CATEGORY>>|<cell|1000000>|<cell|other
  category mismatch>>|<row|<cell|<cpp|D_GLYPHS>>|<cell|3000000>|<cell|glyph
  range mismatch>>|<row|<cell|<cpp|D_HUGE>>|<cell|30000000>|<cell|features
  of different kinds>>|<row|<cell|<cpp|D_INFINITY>>|<cell|1000000000>|<cell|no
  candidate>>>>>>
    Distances between features (<verbatim|Graphics/Fonts/font_select.cpp>).
  </big-table>

  Some distances are asymmetric: when <cpp|asym> is true, a requested
  <verbatim|bold> is reasonably served by a <verbatim|black> font
  (<cpp|Q_WEIGHT>), but a requested <verbatim|black> is not served by a
  <verbatim|bold> one (<cpp|D_WEIGHT>). Similarly for <verbatim|thin>
  versus <verbatim|light>, <verbatim|heavy> versus <verbatim|bold> and
  <verbatim|pen> versus its variants.

  The distance <cpp|distance (string s, array\<less\>string\<gtr\> v, bool
  asym)> between a feature and a logical font is zero if <cpp|s> is the
  default value of its kind and <cpp|v> has no feature of that kind;
  otherwise it is the minimum of the distances between <cpp|s> and the
  features <cpp|v[1]>, <cpp|v[2]>, ..., bounded by the penalty for a
  missing feature of the kind of <cpp|s> (<cpp|D_STRETCH>,
  <cpp|D_WEIGHT>, ..., <cpp|D_GLYPHS>, or <cpp|D_HUGE> for other
  features). As a special case, <verbatim|mono> and
  <verbatim|proportional> are at distance <cpp|D_SPACING> of a font with the
  opposite explicit feature.

  <subsection|Distance between logical fonts>

  The distance which drives the selection is

  <\cpp-code>
    int distance (array\<less\>string\<gtr\> v, array\<less\>string\<gtr\> vx,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ array\<less\>string\<gtr\> w, array\<less\>string\<gtr\> wx);
  </cpp-code>

  where <cpp|v> is the request, <cpp|vx> the request enriched with its
  implicit features (<cpp|logical_font_enrich>), <cpp|w> the candidate as
  computed by <cpp|logical_font> (master, strict features, style
  features) and <cpp|wx> the candidate as computed by
  <cpp|logical_font_exact> (all features and glyph ranges). It is the sum
  of

  <\itemize>
    <item><cpp|D_MASTER> if the masters differ;

    <item>the distances between each requested feature <cpp|v[i]> and the
    full candidate <cpp|wx>: this penalizes candidates lacking a requested
    property;

    <item>the distances between each distinguishing feature <cpp|w[i]> of
    the candidate and the enriched request <cpp|vx>: this penalizes
    candidates having properties which were not asked for (a
    <verbatim|Bold> style when no weight was requested costs
    <cpp|D_WEIGHT>);

    <item><cpp|D_GLYPHS> if the masters differ and the candidate does not
    cover <abbr|ASCII>.
  </itemize>

  <subsection|Guessed distances>

  Ties are broken using the characteristics. The function
  <cpp|characteristic_distance> in <verbatim|Plugins/Freetype/tt_analyze.cpp>
  sums: twice the discrete distances (0 or 1) for <verbatim|mono>,
  <verbatim|sans>, <verbatim|italic> and <verbatim|case>; relative
  (logarithmic) distances for <verbatim|ex>, <verbatim|em>,
  <verbatim|lvw>, <verbatim|lhw>, <verbatim|fillp>, <verbatim|vcnt>,
  <verbatim|lasprat>, <verbatim|pasprat>, <verbatim|loasc>,
  <verbatim|lodes> and <verbatim|dides>; and three times a numeric distance
  for <verbatim|slant>. A missing characteristic counts as a maximal
  difference.

  In <verbatim|Graphics/Fonts/font_guess.cpp>, <cpp|guessed_distance (fam1,
  sty1, fam2, sty2)> adds to this a <cpp|category_distance> between the
  categories of both fonts. <cpp|guessed_distance_families> takes the
  minimum over all pairs of styles (using the styles of the global database
  for families which are not installed), and <cpp|guessed_distance (master1,
  master2)> the minimum over all pairs of families of both masters. All
  three functions memoize their results.

  <section|Searching the closest font>

  <subsection|<cpp|search_font_among>>

  The core loop is

  <\cpp-code>
    void search_font_among (array\<less\>string\<gtr\> v, array\<less\>string\<gtr\> fams,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ array\<less\>string\<gtr\> avoid, int& best_d1,

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ array\<less\>string\<gtr\>& best_result, bool strict);
  </cpp-code>

  It enumerates all styles (<cpp|font_database_styles>) of all families in
  <cpp|fams> whose master is not in <cpp|avoid>, and returns the best pair
  <verbatim|(family, style)>, or <verbatim|(v[0], "Unknown")> if there is
  none. In non strict mode, the other features are removed from the request
  and from the candidates. Candidates are compared lexicographically by
  three distances:

  <\enumerate>
    <item><math|d<rsub|1>>: the distance between logical fonts described
    above;

    <item><math|d<rsub|2>>: the plain distance between the enriched request
    and the exact candidate, both without other and glyph features;

    <item><math|d<rsub|3>>: the guessed distance between the master of the
    request and the master of the candidate.
  </enumerate>

  The second and third distances are only computed when needed for
  breaking a tie.

  <subsection|<cpp|search_font>>

  <\explain>
    <cpp|array\<less\>string\<gtr\> search_font (array\<less\>string\<gtr\>
    v, bool require_exact, array\<less\>string\<gtr\>
    avoid)><explain-synopsis|closest physical font>
  <|explain>
    Returns the pair <verbatim|(family, style)> of the installed font which
    is closest to the logical font <cpp|v>, avoiding the masters in
    <cpp|avoid>:

    <\enumerate>
      <item>If <cpp|v> is empty, return <verbatim|("TeXmacs Computer
      Modern", "Unknown")>.

      <item>Let <cpp|fams> be the families of the master <cpp|v[0]>. If an
      exact result is required, or if <cpp|v> has no features besides
      other ones, perform a strict search among <cpp|fams>. The search
      succeeds if the distance is zero, or if an exact result was required;
      in the latter case, a non zero distance means that no style exactly
      matches, and the returned style name is built from the requested
      features (<cpp|encode_feature>).

      <item>Otherwise, perform a non strict search among <cpp|fams>. If the
      distance is less than <cpp|D_MASTER>, refine the result by a strict
      search among <cpp|fams>.

      <item>Otherwise, no family of the requested master is acceptable
      (typically because the font is not installed): perform a non strict
      search among <em|all> families of the database, take the master of the
      best result, and perform a strict search among the families of that
      master.
    </enumerate>
  </explain>

  <\explain>
    <cpp|array\<less\>string\<gtr\> search_font (array\<less\>string\<gtr\>
    v, int attempt= 1)><explain-synopsis|successive approximations>
  <|explain>
    The variant used by the rest of <TeXmacs>. For <cpp|attempt>
    <math|=1>, this is the previous function without exactness and without
    masters to avoid. For <math|attempt=k\<gtr\>1>, the masters of the
    results of the attempts <math|1,\<ldots\>,k-1> are avoided, so that
    successive attempts return fonts of successively less similar masters.
    Results are cached in a static table. The number of attempts made by
    smart fonts is bounded by <cpp|FONT_ATTEMPTS> (20).
  </explain>

  The function <cpp|search_font_exact (v)> is <cpp|search_font (v, true,
  empty)>.

  <subsection|Substitutions>

  Before searching, <cpp|find_closest> applies the rules of
  <verbatim|font-substitutions.scm> with <cpp|apply_substitutions>. A rule
  <verbatim|((F p1 ... pn) (G q1 ... qm))> applies to a logical font
  <cpp|v> with <cpp|v[0]> equal to <verbatim|F> if <cpp|v> contains all the
  words <verbatim|F>, <verbatim|p1>, ..., <verbatim|pn>; these words are
  removed from <cpp|v> and replaced by <verbatim|G>, <verbatim|q1>, ...,
  <verbatim|qm> (in front). The first applicable rule is used and the
  process is repeated on the result. The rules express knowledge which
  cannot be derived from the names, for instance that the sans serif
  companion of <verbatim|SimSun> is <verbatim|SimHei>, that the italic of
  <verbatim|STSong> is <verbatim|STKaiti>, or that <name|CJK> characters in
  a document typeset in <verbatim|roman> should be taken from
  <verbatim|FandolSong> (or else <verbatim|SimSun>). Recall that a rule is
  only loaded if its target family is installed.

  <subsection|<cpp|find_closest> and <cpp|closest_font>>

  <\explain>
    <cpp|bool find_closest (string& family, string& variant, string& series,
    string& shape, int attempt= 1)><explain-synopsis|closest available
    internal description>
  <|explain>
    Replaces an internal description by the closest one which can actually
    be constructed, and returns whether it changed. The steps are:

    <\enumerate>
      <item><verbatim|lfn = logical_font (family, variant, series,
      shape)>, followed by <cpp|apply_substitutions>;

      <item><verbatim|pfn = search_font (lfn, attempt)>, a physical font
      <verbatim|(family, style)>;

      <item><verbatim|nfn = logical_font (pfn[0], pfn[1])>, translated back
      with <cpp|get_family>, <cpp|get_variant>, <cpp|get_series> and
      <cpp|get_shape>;

      <item>if the request asked for <verbatim|outline>, <verbatim|bold>,
      <verbatim|smallcaps>, <verbatim|italic> or <verbatim|oblique>, but
      neither <verbatim|nfn> nor the features guessed from the
      characteristics of the found font (<cpp|guessed_features>) have it,
      the suffix <verbatim|-poorbbb> is appended to the variant,
      <verbatim|-poorbf> to the series, <verbatim|-poorsc> or
      <verbatim|-poorit> to the shape. These suffixes request synthetic
      variants (see below).
    </enumerate>

    Results are cached in a static table indexed by the original description
    and the attempt number.
  </explain>

  <\explain>
    <cpp|font closest_font (string family, string variant, string series,
    string shape, int sz, int dpi, int attempt= 1)><explain-synopsis|construct
    the closest font>
  <|explain>
    Calls <cpp|find_closest> and then <cpp|find_font> on the result. The
    font is registered in <cpp|font::instances> under the name
    <verbatim|family-variant-series-shape-sz-dpi-attempt> built from the
    original description, so that later requests are immediate.
  </explain>

  <section|Constructing the font: <cpp|find_font>>

  <subsection|From an internal description>

  The function <cpp|find_font (family, variant, series, shape, sz, dpi)> in
  <verbatim|Graphics/Fonts/find_font.cpp> first checks
  <cpp|font::instances> for the name
  <verbatim|family-variant-series-shape-sz-dpi>. Then:

  <\enumerate>
    <item>Synthetic variants are peeled off: a shape ending with
    <verbatim|-poorit> yields <cpp|poor_italic_font> applied to a slightly
    narrowed version of the font without the suffix; <verbatim|-poorsc>
    yields <cpp|poor_smallcaps_font>; a series ending with
    <verbatim|-poorbf> yields <cpp|poor_bold_font>; a variant ending with
    <verbatim|-poorbbb> yields <cpp|poor_bbb_font> (these classes are
    described with the <hlink|smart fonts|smart-fonts.en.tm>).

    <item>The families <verbatim|sys-chinese>, <verbatim|sys-japanese> and
    <verbatim|sys-korean> are replaced by the default fonts for these
    languages.

    <item>The tuples <verbatim|(family variant series shape sz dpi)>, then
    <verbatim|(family variant series sz dpi)>, then <verbatim|(family
    variant sz dpi)> are tried with <cpp|find_font (scheme_tree)>.

    <item>As a last resort, the <TeX> font <verbatim|(tex cmr sz dpi)> is
    used.
  </enumerate>

  <subsection|From a font name>

  The function <cpp|find_font (scheme_tree t)> (through
  <cpp|find_font_bis>) recognizes the following primitive font names, which
  directly call the constructors listed in <hlink|<TeXmacs>
  fonts|fonts.en.tm>:

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Name>|<cell|Constructor>>|<row|<cell|<verbatim|(compound
  ...)>>|<cell|<cpp|compound_font>>>|<row|<cell|<verbatim|(truetype name sz
  dpi)>>|<cell|<cpp|tt_font>>>|<row|<cell|<verbatim|(unicode name sz
  dpi)>>|<cell|<cpp|unicode_font>>>|<row|<cell|<verbatim|(unimath up it bup
  bit rubber)>>|<cell|<cpp|unicode_math_font>>>|<row|<cell|<verbatim|(x name
  sz dpi)>, <verbatim|(qt name sz dpi)>>|<cell|<cpp|x_font>,
  <cpp|qt_font>>>|<row|<cell|<verbatim|(tex name sz dpi [dsize])>, idem for
  <verbatim|cm>, <verbatim|ec>, <verbatim|la>, <verbatim|gr>,
  <verbatim|adobe>>|<cell|<cpp|tex_font>, <cpp|tex_cm_font>,
  <cpp|tex_ec_font>, <cpp|tex_la_font>, <cpp|tex_gr_font>,
  <cpp|tex_adobe_font>>>|<row|<cell|<verbatim|(tex-rubber trl name sz dpi
  [dsize])>>|<cell|<cpp|tex_rubber_font>>>|<row|<cell|<verbatim|(tex-dummy-rubber
  font)>>|<cell|<cpp|tex_dummy_rubber_font>>>|<row|<cell|<verbatim|(error
  font)>>|<cell|<cpp|error_font>>>|<row|<cell|<verbatim|(math (...) (...)
  base error)>>|<cell|<cpp|math_font>>>>>>>
    Primitive font names understood by <cpp|find_font>.
  </big-table>

  For any other head, the rewriting rules are tried. Rules are declared in
  <scheme> with <scm|set-font-rules>, which ends up in <cpp|font_rule>
  (through <cpp|tm_config_rep::set_font_rules>); they are stored in the
  table <cpp|font_conversion>, indexed by the head of the left hand side. A
  rule matches if the arities agree and all non variable atoms are equal;
  atoms starting with <verbatim|$> are variables. The first matching rule
  wins and its right hand side, after substitution, is looked up
  recursively. Here is a typical excerpt of
  <verbatim|progs/fonts/fonts-truetype.scm>:

  <\scm-code>
    (set-font-rules

    \ \ `(((pagella $v bold italic $s $d) (unicode texgyrepagella-bolditalic $s $d))

    \ \ \ \ ((pagella $v bold $b $s $d) (unicode texgyrepagella-bold $s $d))

    \ \ \ \ ((pagella $v $a $b $s $d) (unicode texgyrepagella-regular $s $d))))
  </scm-code>

  The rule files <verbatim|fonts-ec.scm>, <verbatim|fonts-adobe.scm>,
  <verbatim|fonts-x.scm>, <verbatim|fonts-math.scm>,
  <verbatim|fonts-foreign.scm>, <verbatim|fonts-misc.scm>,
  <verbatim|fonts-composite.scm> and <verbatim|fonts-truetype.scm> are
  loaded at boot time by <verbatim|progs/init-texmacs.scm>.

  <subsection|The database fallback>

  If no rule exists for the head of a six element tuple <verbatim|(family
  variant series shape sz dpi)>, <cpp|find_font_bis> calls the four argument
  <cpp|font_database_search>, which computes <cpp|logical_font (family,
  variant, series, shape)>, finds the closest physical font with
  <cpp|search_font> and returns its file names
  (<cpp|font_database_search (family, style)>; a subfont <math|n> of a
  collection <verbatim|file.ttc> is returned as <verbatim|file.n.ttf>). The
  first file which exists (<cpp|tt_font_exists>, after removal of the
  extension) gives the font <cpp|unicode_font (name, sz, dpi)>.

  This is how the old and new mechanisms coexist. The families returned by
  <cpp|find_closest> are masters. For the master <verbatim|roman> of
  <verbatim|TeXmacs Computer Modern>, rules exist in
  <verbatim|fonts-ec.scm>, so that <TeX> fonts are built through the rules.
  For a master such as <verbatim|TeX Gyre Pagella> or <verbatim|DejaVu>
  (with capitals; the rules use the old lower case names), there are no
  rules, and the files are found through the database. Rule heads are
  matched case sensitively.

  <section|Characters missing from the main font>

  A smart font first tries to render a character with its main font. If this
  fails, it calls <cpp|smart_font_rep::resolve>, which tries, for
  <math|attempt=1,\<ldots\>,FONT_ATTEMPTS> and for each family of the comma
  separated family list, a sequence of strategies (see <hlink|smart
  fonts|smart-fonts.en.tm>). The strategies related to the database are:

  <\itemize>
    <item>At the first attempt, <cpp|closest_font (fam, variant, series,
    shape, sz, dpi, 1)> is tried for families other than the main one, for
    instance the <verbatim|SimSun> of <verbatim|cjk=SimSun,roman>, when the
    character belongs to the indicated range.

    <item>At attempt <math|k\<gtr\>1>, the <name|Unicode> range of the
    character is computed by <cpp|get_unicode_range> (in
    <verbatim|smart_font.cpp>): <verbatim|ascii>, <verbatim|latin>,
    <verbatim|greek>, <verbatim|cyrillic>, <verbatim|cjk>,
    <verbatim|hiragana>, <verbatim|hangul>, <verbatim|mathsymbols>,
    <verbatim|mathextra>, <verbatim|mathletters>, or empty. The range is
    appended to the variant (<verbatim|rm-cjk>, <verbatim|ss-greek>, ...),
    and <cpp|closest_font (fam, v, series, shape, sz, dpi, k-1)> is tried.
  </itemize>

  The second strategy is where the database does the real work. The range
  becomes a glyph feature of the request, for instance
  <verbatim|["roman", "cjk"]>. Since a missing glyph range costs
  <cpp|D_GLYPHS>, which is larger than <cpp|D_MASTER> and than all stylistic
  penalties, <cpp|search_font> prefers any font that covers the range to a
  font of the requested master that does not. Among the fonts that cover the
  range, the one with the closest features, and then the closest
  characteristics, is chosen. If this font still does not contain the
  character, the next attempt avoids its master. Substitution rules such as
  <verbatim|((roman cjk) (FandolSong))> short-circuit the search for
  frequent cases.

  The range <verbatim|hiragana> (like any word which is not listed among
  the glyph features) is not a glyph feature, but an other feature: it does not
  steer the search towards suitable fonts, and only the successive
  attempts eventually find a font which contains the character. For
  Japanese and Korean, documents should rather use explicit family lists
  such as <verbatim|cjk=Name,roman> (this is what <verbatim|sys-japanese>
  and <verbatim|sys-korean> expand to), in which the condition
  <verbatim|cjk> also accepts <verbatim|hangul> and <verbatim|hiragana>
  characters.

  When the main font has a <verbatim|MATH> table (an <name|OpenType> or a
  <name|TeX Gyre> math font) and a symbol it lacks would be drawn by an
  emulation which a <name|PDF> export can only hold as a bitmap, the smart
  font takes the symbol from <name|STIX Two Math>, shipped with <TeXmacs>,
  instead of asking the database (<cpp|resolve_shipped_math> in
  <verbatim|smart_font.cpp>), so that the result is the same on every
  system.

  The default <name|CJK> fonts are determined by
  <cpp|default_chinese_font_name>, <cpp|default_japanese_font_name> and
  <cpp|default_korean_font_name> in <verbatim|Graphics/Fonts/font.cpp>:
  the user preferences <verbatim|"default chinese font name">,
  <verbatim|"default japanese font name"> and <verbatim|"default korean
  font name"> take precedence; otherwise a platform specific list of
  candidate files is checked with <cpp|tt_font_exists>, with
  <verbatim|roman> as the final fallback.

  <section|Worked examples>

  <subsection|An installed family with a style>

  Request <verbatim|("pagella", "rm", "bold", "italic")>. The logical font
  is <verbatim|["TeX Gyre Pagella", "bold", "italic"]>. Since it has two
  features, the first search is a non strict one among the families of the
  master <verbatim|TeX Gyre Pagella>, which only contains the family of the
  same name. The style <verbatim|Bold Italic> has the logical font
  <verbatim|["TeX Gyre Pagella", "bold", "italic"]> and is at distance 0;
  <verbatim|Bold> is at distance <cpp|D_SLANT>, <verbatim|Italic> at
  distance <cpp|D_WEIGHT>, <verbatim|Regular> at distance
  <cpp|D_WEIGHT> plus <cpp|D_SLANT>. The strict refinement gives the same
  result. Translating back gives <verbatim|("TeX Gyre Pagella", "rm",
  "bold", "italic")> without synthetic suffixes, and <cpp|find_font>
  constructs <cpp|unicode_font ("texgyrepagella-bolditalic", sz, dpi)>
  through the database fallback.

  <subsection|A style provided by a sibling family>

  Request <verbatim|("Linux Libertine", "rm", "medium", "small-caps")>,
  that is, <verbatim|["Linux Libertine", "smallcaps"]>. The master
  <verbatim|Linux Libertine> has the families <verbatim|Linux Libertine>
  and <verbatim|Linux Libertine Capitals>, the latter with the feature
  <verbatim|SmallCaps>. Since this feature is not common to both families,
  it is a strict feature of <verbatim|Linux Libertine Capitals>, so that
  <verbatim|("Linux Libertine Capitals", "Regular")> has the logical font
  <verbatim|["Linux Libertine", "smallcaps"]> and wins with distance 0,
  whereas <verbatim|("Linux Libertine", "Regular")> is at distance
  <cpp|D_CAPITALIZATION>. A genuine small capitals font is used.

  <subsection|A missing style>

  Request <verbatim|("DejaVu", "rm", "medium", "small-caps")>. No DejaVu
  family has small capitals; the best candidates, such as
  <verbatim|("DejaVu Serif", "Book")>, are at distance
  <cpp|D_CAPITALIZATION>, which is less than <cpp|D_MASTER>, so the master
  is kept (sans serif candidates additionally pay <cpp|D_SERIF>). Since
  neither the name nor the characteristics of the result provide
  <verbatim|smallcaps>, <cpp|find_closest> returns the shape
  <verbatim|right-poorsc>, and <cpp|find_font> builds a
  <cpp|poor_smallcaps_font> on top of DejaVu Serif.

  <subsection|A substitution>

  Request <verbatim|("SimSun", "ss", "medium", "right")>, that is,
  <verbatim|["SimSun", "sansserif"]>. If <verbatim|SimHei> is installed, the
  substitution <verbatim|((SimSun sansserif) (SimHei))> rewrites the request
  into <verbatim|["SimHei"]>, which is then found exactly.

  <subsection|A font which is not installed>

  Request <verbatim|("Garamond", "rm", "medium", "right")> on a system
  without that font. The master is unknown to the local database:
  <cpp|master_to_families> prints <verbatim|TeXmacs] missing 'Garamond'
  master> and loads the global database, which knows the master, the styles and the
  characteristics of Garamond. The strict and non strict searches among the
  families of the master find no installed style (distance
  <cpp|D_INFINITY>), so the non strict search is extended to all installed
  families. Every candidate then pays <cpp|D_MASTER>; among the candidates
  with the fewest stylistic penalties (typically the regular styles of
  serif, proportional, Latin fonts), the one whose master is closest to
  Garamond according to the guessed distance is chosen. Finally, a strict
  search among the families of that master selects the appropriate style.

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
