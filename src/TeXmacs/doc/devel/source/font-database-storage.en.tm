<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The font database: files, lifecycle and characteristics>

  <section|The tables of the database>

  The font database consists of a few global hash tables, all defined in
  <verbatim|Graphics/Fonts/font_database.cpp>. Keys and values are
  <scheme> trees (<cpp|tree> objects whose leaves are strings), which makes
  it trivial to load and save them as <scheme> files.

  <\description>
    <item*|<cpp|hashmap\<less\>tree,tree\<gtr\> font_table>>Associates to
    each physical font, given as a pair <verbatim|(family style)>, the list
    of its known <em|locations>. A location is a triple <verbatim|(file
    index size)> of strings: the base name of the font file (without
    directory), the index of the subfont inside a <verbatim|.ttc>
    collection (<verbatim|0> for ordinary files) and the size of the file
    in bytes. Several locations can be registered for the same font, for
    instance when different versions of a file exist on different systems;
    the size is used to distinguish them.

    <item*|<cpp|hashmap\<less\>tree,tree\<gtr\> font_global_table>>The same
    kind of table for the global database. It is only loaded on demand (see
    below) and is only used in order to know which styles exist for a
    family that is not installed locally.

    <item*|<cpp|hashmap\<less\>tree,tree\<gtr\> font_features>>Associates to
    each family name a tuple whose first element is the <em|master> family
    and whose other elements are features, such as <verbatim|SansSerif>,
    <verbatim|Mono>, <verbatim|Calligraphic> or <verbatim|Handwritten>. A
    master groups several families which belong to the same design: for
    instance <verbatim|DejaVu Sans>, <verbatim|DejaVu Sans Mono> and
    <verbatim|DejaVu Serif> all have the master <verbatim|DejaVu>.

    <item*|<cpp|hashmap\<less\>tree,tree\<gtr\> font_variants>>The inverse of
    the previous table: it associates to each master the tuple of its
    families. It is not stored on disk, but recomputed by
    <cpp|font_database_load_features>.

    <item*|<cpp|hashmap\<less\>tree,tree\<gtr\> font_characteristics>>Associates
    to each physical font <verbatim|(family style)> the tuple of its
    automatically computed characteristics (see
    <hlink|below|#characteristics>).

    <item*|<cpp|hashmap\<less\>string,tree\<gtr\> font_substitutions>>Associates
    to a family name a tuple of substitution rules (see the
    <hlink|selection algorithm|font-database-selection.en.tm>).
  </description>

  In addition, the global boolean <cpp|new_fonts> (accessed through
  <cpp|set_new_fonts> and <cpp|get_new_fonts>) records whether the database
  based selection is enabled at all. It mirrors the user preference
  <verbatim|"new style fonts">.

  <section|The files of the database>

  <subsection|File format>

  All database files are sequences of <scheme> expressions, one per line,
  read with <cpp|block_to_scheme_tree> and written with
  <cpp|scheme_tree_to_block>. Spaces inside names are escaped by a
  backslash. When saving, the entries are sorted case insensitively
  (<cpp|font_less_eq_operator>), which keeps the files readable and makes
  their differences meaningful under version control. Here are typical
  entries of the four main files:

  <\verbatim-code>
    ;; font-database.scm: (family style) -\<gtr\> locations

    ((TeX\\ Gyre\\ Pagella Bold) ((texgyrepagella-bold.otf 0 219564)))

    ((Al\\ Nile Bold) ((Al\\ Nile.ttc 1 124996) (Al\\ Nile.ttc 2 131264)))

    ((TeXmacs\\ Computer\\ Modern Bold) ((ecbx10.tfm 0 3200)))

    \;

    ;; font-features.scm: family master feature ...

    (DejaVu\\ Sans DejaVu SansSerif)

    (DejaVu\\ Sans\\ Mono DejaVu Mono SansSerif)

    (TeXmacs\\ Computer\\ Modern\\ Sans roman SansSerif)

    \;

    ;; font-characteristics.scm: (family style) -\<gtr\> characteristics

    ((TeX\\ Gyre\\ Pagella Regular) (Ascii Latin Greek mono=no sans=no
    slant=0 italic=no case=mixed regular=yes ex=79 em=200 lvw=20 lhw=8 ...))

    \;

    ;; font-substitutions.scm: (family property ...) (family property ...)

    ((SimSun sansserif) (SimHei))

    ((roman cjk) (FandolSong))
  </verbatim-code>

  Notice that <TeX> fonts also appear in the database: the family
  <verbatim|TeXmacs Computer Modern> (master <verbatim|roman>) is located in
  <verbatim|.tfm> files. Features are written in \Pcamel case\Q
  (<cpp|encode_feature>) and normalized to lower case when read
  (<cpp|normalize_feature>); substitution files directly use the lower case
  forms.

  <subsection|Global and local files>

  The following files are involved (macros at the top of
  <verbatim|font_database.cpp>):

  <\description-paragraphs>
    <item*|<verbatim|$TEXMACS_PATH/fonts/font-database.scm>,
    <verbatim|font-features.scm>, <verbatim|font-characteristics.scm>>The
    <em|global> database, shipped with <TeXmacs> (in the source tree:
    <verbatim|src/TeXmacs/fonts>). It describes several thousands of fonts
    which are commonly found on <name|Linux>, <name|macOS> and
    <name|Windows> systems or in <TeX> distributions (more than a thousand
    families), whether or not they are installed on the current machine. The features are curated by hand.

    <item*|<verbatim|$TEXMACS_PATH/fonts/font-substitutions.scm>>Substitution
    rules; there is no local version of this file.

    <item*|<verbatim|$TEXMACS_PATH/fonts/font-features.bis.scm>>Output file of
    <cpp|font_database_build_global> for the features (see below); it is
    never read.

    <item*|<verbatim|$TEXMACS_HOME_PATH/fonts/font-database.scm>,
    <verbatim|font-features.scm>, <verbatim|font-characteristics.scm>>The
    <em|local> database: the fonts which are actually available for the
    user. This is the database that is used for all queries.

    <item*|<verbatim|$TEXMACS_HOME_PATH/fonts/delta-database.scm>,
    <verbatim|delta-features.scm>, <verbatim|delta-characteristics.scm>>The
    entries of the local database which are not in the global one, written
    by <cpp|font_database_save_local_delta>. They are used by maintainers in
    order to inspect new fonts before adding them to the global database.
  </description-paragraphs>

  The file <verbatim|$TEXMACS_PATH/fonts/pdf-font-issues.scm> is unrelated
  to the database proper: it lists font files that need a special treatment
  by the <name|PDF> renderer (<cpp|no_font_issues> in
  <verbatim|Plugins/Pdf/pdf_hummus_renderer.cpp>).

  <section|Lifecycle of the database>

  <subsection|Loading>

  All query functions start by calling <cpp|font_database_load>, which does
  nothing after the first successful call (flag <cpp|fonts_loaded>).
  Otherwise, it proceeds as follows:

  <\enumerate>
    <item>Load the local database into <cpp|font_table>. If it is empty or
    missing (first start, or after an upgrade or a cache clearance), load
    the global database instead, keep only those entries whose files exist
    on this machine (<cpp|font_database_filter>), and save the result as
    the local database.

    <item>Similarly load the local features. If there are none, load the
    global features, keep only those of the families present in
    <cpp|font_table> (<cpp|font_database_filter_features>) and save them
    locally. While loading features, <cpp|font_variants> is rebuilt.

    <item>Similarly for the characteristics
    (<cpp|font_database_filter_characteristics>).

    <item>Load the global substitutions (<cpp|font_database_load_substitutions>).
    A rule is only retained if its target family has at least one style in
    <cpp|font_table>, so that substitutions never lead to missing fonts.
  </enumerate>

  In other words, on first start the local database is the intersection of
  the global database with the files on disk. Apart from the sizes of the
  files, which are used to identify them, font files are not analyzed at
  this stage. Fonts that are installed but unknown to the global database
  are only discovered by an explicit scan (see below).

  <subsection|Filtering the global database>

  <cpp|font_database_filter> works with two temporary tables. The table
  <cpp|back_font_table>, built by <cpp|build_back_table>, maps each location
  <verbatim|(file index size)>, and also each pair <verbatim|(file index)>,
  to the keys <verbatim|(family style)> which use it. Then
  <cpp|font_database_collect> traverses the font directories
  (<cpp|tt_font_path> and <cpp|tfm_font_path>). For each <verbatim|.ttf>,
  <verbatim|.ttc>, <verbatim|.otf> or <verbatim|.tfm> file and each subfont
  index <math|j=0,1,...>, it looks up the location <verbatim|(file j
  size)>:

  <\itemize>
    <item>If the exact location is known, it is added to the new table for
    all corresponding keys.

    <item>If only <verbatim|(file j)> is known (another version of the same
    file), <cpp|find_best_approximation> picks the registered location whose
    size is closest, and prints <verbatim|TeXmacs] approximating font
    ...>. In this case, the number of subfonts is checked with
    <cpp|tt_font_name> and the loop stops with <verbatim|TeXmacs] ignore
    ... and higher subfonts> if the file has fewer subfonts.

    <item>Otherwise the loop over <math|j> stops.
  </itemize>

  <subsection|Lazy loading of the global database>

  When the selection code meets a family or a master which is unknown to the
  local features (functions <cpp|family_to_master> and
  <cpp|master_to_families>, with an exception for the pseudo families
  <verbatim|tc> and <verbatim|tcx>), it prints <verbatim|TeXmacs] missing
  'name' family> (resp. <verbatim|master>) and calls
  <cpp|font_database_global_load>, which itself prints <verbatim|TeXmacs]
  warning, missing font, loading global substitution list>. This function,
  executed at most once (flag <cpp|fonts_global_loaded>), loads the global
  database into <cpp|font_global_table>, and <em|merges> the global features
  and characteristics into <cpp|font_features> and
  <cpp|font_characteristics>. Consequently, the selection algorithm knows the
  master, the features, the styles (<cpp|font_database_global_styles>) and
  the characteristics of fonts which are used by a document but which are
  not installed, and it can use this knowledge in order to find a similar
  installed font. The table <cpp|font_table> itself is not modified, so that
  an uninstalled font is never selected.

  <subsection|Scanning the disk>

  The function <cpp|font_database_build> adds the fonts found at a given
  <cpp|url> to <cpp|font_table>. It recurses through <abbr|URL>
  disjunctions and directories; in directories, only files with extensions
  <verbatim|.ttf>, <verbatim|.ttc> and <verbatim|.otf> are considered (a
  single file given explicitly is processed whatever its extension). Files
  on a small blacklist (<cpp|on_blacklist>) are skipped. For each file, it
  prints <verbatim|Process file>, calls <cpp|tt_font_name> to obtain one
  pair <verbatim|(family style)> per subfont, and registers the location
  <verbatim|(file index size)> for this pair.

  The scanning functions available to the rest of <TeXmacs> are:

  <\explain>
    <cpp|void font_database_build_local ()><explain-synopsis|full scan of
    the font path>
  <|explain>
    Loads the database, scans <cpp|tt_font_path ()>, computes the missing
    characteristics (<cpp|font_database_build_characteristics (false)>),
    guesses features for the families that have none
    (<cpp|font_database_guess_features>) and saves the local database. This
    is the implementation of <menu|Tools|Fonts|Scan disk for fonts>
    (<scm|scan-disk-for-fonts>). It may take several minutes on systems with
    many fonts, since every new font is rendered for its analysis.
  </explain>

  <\explain>
    <cpp|void font_database_extend_local (url u)><explain-synopsis|import
    fonts>
  <|explain>
    Adds the directory of <cpp|u> (or <cpp|u> itself if it is a directory)
    to the preference <verbatim|"imported fonts"> (<cpp|tt_extend_font_path>),
    so that it becomes part of the font path in future sessions, and then
    does the same as the previous function, but only for <cpp|u>. This is
    called by the <menu|Import> button of the font selector.
  </explain>

  <\explain>
    <cpp|void font_database_build_global (url u)><explain-synopsis|extend the
    global database>
  <|explain>
    For maintainers: reloads the global database, scans <cpp|u>, computes
    the characteristics and guessed features of the new fonts, and writes
    <verbatim|font-database.scm> and <verbatim|font-characteristics.scm> in
    <verbatim|$TEXMACS_PATH/fonts>. The features are written to
    <verbatim|font-features.bis.scm> instead of
    <verbatim|font-features.scm>, so that the guessed features can be
    reviewed and merged by hand. The variant without argument scans the
    whole <cpp|tt_font_path ()>. In <scheme> these are
    <scm|font-database-insert-global> and <scm|font-database-build-global>.
    Since the function changes the in-memory tables, <TeXmacs> should be
    restarted afterwards.
  </explain>

  <\explain>
    <cpp|void font_database_save_local_delta ()><explain-synopsis|save the
    difference between the local and global databases>
  <|explain>
    Writes the entries of the local database whose keys are not in the
    global one to the <verbatim|delta-*.scm> files and resets all tables
    (they are reloaded at the next query). The families of the delta are
    returned by <cpp|font_database_delta_families>.
  </explain>

  <subsection|The font path>

  The directories which are scanned, and in which font files are looked up,
  are given by <cpp|tt_font_path> in <verbatim|Plugins/Freetype/tt_file.cpp>.
  This is a disjunction of the following directories, each of them searched
  recursively (<cpp|search_sub_dirs>):

  <\itemize>
    <item>the directories in the environment variable
    <verbatim|TEXMACS_FONT_PATH>;

    <item>the directories in the preference <verbatim|"imported fonts">;

    <item><verbatim|$TEXMACS_HOME_PATH/fonts/truetype> and
    <verbatim|$TEXMACS_PATH/fonts/truetype>;

    <item>system dependent directories: <verbatim|$windir/Fonts> on
    <name|Windows>; <verbatim|$HOME/Library/Fonts>,
    <verbatim|/Library/Fonts>, <verbatim|/System/Library/Fonts> and a few
    <name|Apple> specific directories on <name|macOS>;
    <verbatim|$HOME/.fonts>, <verbatim|/usr/share/fonts/opentype>,
    <verbatim|/usr/share/fonts/truetype> and their
    <verbatim|/usr/local> counterparts on other systems; plus the
    <verbatim|opentype> and <verbatim|truetype> font directories of
    <TeX> Live 2020 to 2022 (and of <name|MacPorts> <TeX> Live on
    <name|macOS>).
  </itemize>

  Fonts in other places (for instance <verbatim|$HOME/.local/share/fonts>,
  or a more recent <TeX> Live) are only found after an explicit import or
  through <verbatim|TEXMACS_FONT_PATH>.

  <section|Reading font files>

  <subsection|Locating a font file>

  Throughout the font code, a font file is designated by its base name
  without extension, such as <verbatim|texgyrepagella-bold> or
  <verbatim|DejaVuSans>. The function <cpp|tt_font_find> maps such a name to
  a <cpp|url>, using the persistent cache <verbatim|font_cache.scm> (key
  <verbatim|"ttf:"> followed by the name). On a cache miss,
  <cpp|tt_font_find_sub> successively tries:

  <\enumerate>
    <item><cpp|tt_unpack>: if the name ends with an integer suffix, as in
    <verbatim|Al Nile.1>, the subfont <verbatim|1> of the collection
    <verbatim|Al Nile.ttc> is extracted (<cpp|tt_extract_subfont>) into
    <verbatim|$TEXMACS_HOME_PATH/fonts/unpacked/Al Nile.1.ttf>, which is
    returned. This is how fonts in <verbatim|.ttc> collections are made
    accessible to the rest of <TeXmacs>.

    <item>the extensions <verbatim|.pfb>, <verbatim|.ttf>,
    <verbatim|.ttc>, <verbatim|.otf> and <verbatim|.dfont>, using
    <cpp|tt_locate>; <verbatim|.pfb> files are searched in the <TeX>
    distribution (<cpp|resolve_tex>), the others in <cpp|tt_font_path>
    (and optionally with the system <verbatim|locate> command, when the
    global flag <cpp|use_locate> is set, which is currently never the
    case).
  </enumerate>

  The predicate <cpp|tt_font_exists> additionally caches its answers in
  memory. The function <cpp|tt_find_name> looks for design size variants of
  a font (such as <verbatim|ecrm10> or <verbatim|ecrm12>); it is mostly
  relevant for <TeX> fonts.

  <subsection|Parsing <name|TrueType> and <name|OpenType> files>

  The file <verbatim|Plugins/Freetype/tt_tools.cpp> reads font files
  directly, without <name|FreeType>. A file is loaded into a <cpp|string>
  and accessed with big endian readers (<cpp|get_U16>, <cpp|get_U32>,
  <cpp|get_tag>). The main routines are:

  <\itemize>
    <item><cpp|tt_is_collection> and <cpp|tt_nr_fonts>: a collection starts
    with the tag <verbatim|ttcf>, and the number of subfonts is stored at
    offset 8; <cpp|tt_header_index> gives the offset of the table directory
    of each subfont.

    <item><cpp|tt_correct_version>: the table directory must start with
    version <verbatim|0x00010000> or one of the tags <verbatim|OTTO>,
    <verbatim|true>, <verbatim|typ1>.

    <item><cpp|tt_table>: extracts a table by index or by tag.

    <item><cpp|name_record_family> and <cpp|name_record_shape>: the English
    family (name identifier 1) and subfamily (name identifier 2) of the
    <verbatim|name> table, taken from a record of either the <name|Macintosh>
    platform with language 0 or the <name|Windows> platform with language
    <verbatim|0x0409>. Only printable <abbr|ASCII> characters are kept
    (<cpp|filter_english>), which also turns <abbr|UTF-16> strings into
    plain <abbr|ASCII>.

    <item><cpp|tt_extract_subfont>: builds a standalone font file from one
    subfont of a collection by copying its tables and recomputing the
    offsets.
  </itemize>

  <\explain>
    <cpp|scheme_tree tt_font_name (url u)><explain-synopsis|family and style
    names of the fonts in a file>
  <|explain>
    Returns a tuple with one pair <verbatim|(family style)> per subfont of
    <cpp|u>, or an empty tuple if the file cannot be read or if one of its
    subfonts has an unsupported version. The names are normalized: words
    describing a stretch or a weight which some vendors put into the family
    name (<verbatim|Narrow>, <verbatim|Condensed>, <verbatim|Light>,
    <verbatim|Semibold>, <verbatim|Bold>, <verbatim|Black>,
    <verbatim|Italic>, <abbr|etc.>) are moved to the style by
    <cpp|move_to_shape> (with spelling normalizations, such as
    <verbatim|Ultralight> into <verbatim|Thin>, and <verbatim|Medium>
    simply dropped); leading non alphabetic characters are removed; all
    capital family names are converted to lower case; the first letter is
    capitalized; and <verbatim|STIX> becomes <verbatim|Stix>. Exported to
    <scheme> as <scm|tt-font-name>.
  </explain>

  <\explain>
    <cpp|void tt_dump (url u)><explain-synopsis|print the contents of a font
    file>
  <|explain>
    Prints the list of tables, the family and style names, and all records
    of the <verbatim|name> table for each subfont. Useful in order to
    understand why a font gets a strange name in the database. Exported to
    <scheme> as <scm|tt-dump>.
  </explain>

  <section|Font characteristics><label|characteristics>

  <subsection|Computation>

  The characteristics of a font are computed by
  <cpp|font_database_build_characteristics>, which, for every key of
  <cpp|font_table> without characteristics (or for all keys, if
  <cpp|force> is true), derives a font name from its locations
  (<verbatim|file.ttc> with index <math|n> becomes <verbatim|file.n>, the
  extension is removed, and a trailing <verbatim|10> is removed for
  <verbatim|.tfm> files if needed) and calls <cpp|tt_analyze> on it.

  <\explain>
    <cpp|array\<less\>string\<gtr\> tt_analyze (string
    family)><explain-synopsis|compute the characteristics of a font>
  <|explain>
    Loads the font with <cpp|tt_font> and <cpp|tt_font_metric> at 10pt and
    1200dpi, temporarily disables fatal errors for missing glyphs
    (<cpp|get_glyph_fatal>), and calls <cpp|analyze_range>,
    <cpp|analyze_special> and <cpp|analyze_major> in
    <verbatim|Plugins/Freetype/tt_analyze.cpp>. Without <name|FreeType>
    support, it returns an empty array. Exported to <scheme> as
    <scm|tt-analyze>.
  </explain>

  The characteristics are of two kinds: <em|glyph ranges> (plain words) and
  <em|attributes> of the form <verbatim|name=value>.

  <\description>
    <item*|Glyph ranges (<cpp|analyze_range>)><verbatim|Ascii> and
    <verbatim|Latin> if all characters in <verbatim|0x21-0x7e>,
    respectively <verbatim|0xc0-0xff>, have a non empty bounding box;
    <verbatim|Greek> and <verbatim|Cyrillic> if more than two thirds of the
    basic letters are present; <verbatim|CJK>, <verbatim|Hangul>,
    <verbatim|MathSymbols> (<verbatim|0x2000-0x23ff>),
    <verbatim|MathExtra> (<verbatim|0x2900-0x2e7f>) and
    <verbatim|MathLetters> (<verbatim|0x1d400-0x1d7ff>) if more than 20% of
    the corresponding range is present. <TeX> <verbatim|ec*10> fonts are
    declared to support the first four ranges. Greek and Cyrillic are
    removed again by <cpp|sane_font> if a test letter is missing, or, in
    <name|CJK> fonts, if it looks like a full width glyph.

    <item*|Special properties (<cpp|analyze_special>)><verbatim|mono=yes>
    if all letters have the same advance width; <verbatim|sans=yes> if the
    glyph <verbatim|L> has no serifs (<cpp|is_sans_serif>);
    <verbatim|slant=n>, the slant of <verbatim|[> times 100;
    <verbatim|italic=yes> if the <verbatim|a> has the italic (single
    storey) form and the <verbatim|f> descends below the baseline;
    <verbatim|case=mixed>, <verbatim|caps> or <verbatim|smallcaps>
    depending on the heights of lower case letters compared to upper case
    ones; <verbatim|regular=yes> if the x-height letters have a regular
    height.

    <item*|Metric properties (<cpp|analyze_major>)><verbatim|ex> (the
    x-height, an absolute value at the fixed size of the analysis), and, relative to it (in
    percent): <verbatim|em> (width of <verbatim|M>), <verbatim|lvw> and
    <verbatim|lhw> (vertical and horizontal stroke widths of
    <verbatim|o>), <verbatim|uvw> and <verbatim|uhw> (the same for
    <verbatim|O>), <verbatim|lasprat> and <verbatim|pasprat> (average
    logical and ink widths of lower case letters), <verbatim|loasc> and
    <verbatim|lodes> (maximal ascent and descent of lower case letters) and
    <verbatim|dides> (descent of digits). Finally <verbatim|fillp> is the
    average fill rate of the letters and <verbatim|vcnt> the number of
    black pixels per unit of width: both measure the weight.
  </description>

  A few accessors return metric characteristics as ratios with respect to
  the x-height (<cpp|get_M_width>, <cpp|get_lo_pen_width>,
  <cpp|get_up_pen_width>, ...); they are used by the font emulation code
  (for instance for synthetic blackboard bold). The function
  <cpp|analyze_trace>, which computes \Ptraces\Q of glyph widths and
  heights, is currently disabled.

  <subsection|Features guessed from characteristics>

  The function <cpp|guessed_features (family, style)> in
  <verbatim|Graphics/Fonts/font_guess.cpp> translates characteristics back
  into features:

  <\itemize>
    <item>a weight (<verbatim|thin>, <verbatim|light>, <verbatim|bold> or
    <verbatim|black>) from <verbatim|vcnt> and <verbatim|fillp>, after a
    correction which takes the slant and the aspect ratio into account;

    <item>a stretch (<verbatim|condensed> or <verbatim|wide>) from
    <verbatim|lasprat>, <verbatim|pasprat> and <verbatim|lvw>;

    <item><verbatim|italic> or <verbatim|oblique>, <verbatim|smallcaps>,
    <verbatim|mono> and <verbatim|sansserif> from the special properties.
  </itemize>

  The variant <cpp|guessed_features (family, pure_guess)> computes the
  features of a whole family: the features guessed for each style are
  merged into the features obtained from the names
  (<cpp|cautious_patch>, which never overrides a feature of the same kind),
  and only the features common to all styles are kept, except those which
  vary between styles according to the names. The result starts with the
  master (<cpp|family_to_master>). This is how
  <cpp|font_database_guess_features> provides features for newly scanned
  families; since <cpp|family_to_master> strips words like
  <verbatim|Sans>, <verbatim|Mono> or <verbatim|Condensed> from unknown
  family names, the families <verbatim|Foo Sans> and <verbatim|Foo Serif>
  automatically get the same master <verbatim|Foo>.

  <section|Caches and their invalidation>

  Besides the local database itself, the following caches are involved:

  <\description>
    <item*|<verbatim|$TEXMACS_HOME_PATH/system/cache/font_cache.scm>>The
    persistent cache of <cpp|tt_font_find> and <cpp|tt_find_name>
    (<verbatim|System/Misc/data_cache.cpp>). Positive entries are checked
    for existence and discarded if the file disappeared; negative entries
    (font not found) are trusted.

    <item*|<verbatim|$TEXMACS_HOME_PATH/fonts/unpacked/>>Subfonts extracted
    from collections. They are never refreshed automatically.

    <item*|<verbatim|$TEXMACS_HOME_PATH/fonts/error/>>Markers for <TeX>
    fonts which could not be generated (<verbatim|Plugins/Metafont/load_tex.cpp>).
    They are removed at startup by <cpp|cache_initialize> when the
    <verbatim|fonts/type1> or <verbatim|fonts/truetype> directories of
    <verbatim|$TEXMACS_PATH> or <verbatim|$TEXMACS_HOME_PATH> changed.

    <item*|In memory>The answers of <cpp|tt_font_exists>; the caches of
    <cpp|search_font> and <cpp|find_closest>; the memo tables of the
    <cpp|guessed_distance> functions; and the table
    <cpp|font::instances> of all constructed fonts, indexed by name.
  </description>

  These caches are invalidated in the following situations:

  <\itemize>
    <item>When <TeXmacs> is upgraded to a new version, <cpp|init_upgrade>
    (<verbatim|System/Boot/init_upgrade.cpp>) removes the local database
    files, the <verbatim|fonts/error> markers and several caches; the local
    database is then rebuilt from the global one at the next start. Fonts
    that had been found by scanning the disk are lost and require a new
    scan.

    <item>The command <menu|Tools|Fonts|Clear font cache>
    (<scm|clear-font-cache> in <verbatim|progs/texmacs/texmacs/tm-tools.scm>)
    removes <verbatim|font_cache.scm> and the three local database files.
    It does not touch the in-memory tables, so <TeXmacs> has to be
    restarted; it neither removes the unpacked subfonts.

    <item>After saving a database file, <cpp|font_database_save_database>
    and its siblings call <cpp|cache_refresh>.
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
