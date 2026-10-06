<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|<scheme> interface, font selector and debugging>

  <section|Preferences>

  The following user preferences influence the font database and the font
  selection:

  <\description-paragraphs>
    <item*|<verbatim|"new style fonts">>Default <verbatim|"on">. When it
    changes, <scm|notify-new-fonts> in
    <source-link|progs/texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm> calls
    <scm|set-new-fonts>, which sets the <c++> flag <cpp|new_fonts>. When
    the flag is off, <cpp|smart_font> reduces to <cpp|find_font> and only
    the <scheme> rewriting rules are used. Menus test the flag with
    <scm|new-fonts?>: when it is on, <menu|Format|Font> and
    <menu|Document|Font> open the font selector instead of the old font
    menus of <source-link|progs/fonts/font-old-menu.scm|TeXmacs/progs/fonts/font-old-menu.scm>. The preference can be
    toggled in the experimental section of the preferences dialog.

    <item*|<verbatim|"advanced font customization">>When on, the font
    selector shows the customization widgets (effects, variants, mathematics)
    directly; otherwise they are available through an <menu|Advanced>
    button.

    <item*|<verbatim|"imported fonts">>A list of directories added to the
    font path (<cpp|tt_extend_font_path>, <cpp|tt_font_path>).

    <item*|<verbatim|"default chinese font name">, <verbatim|"default
    japanese font name">, <verbatim|"default korean font name">>Override the
    default fonts used for <verbatim|sys-chinese>, <verbatim|sys-japanese>
    and <verbatim|sys-korean>.
  </description-paragraphs>

  The environment variable <verbatim|TEXMACS_FONT_PATH> also extends the
  font path.

  <section|<scheme> modules>

  The directory <verbatim|progs/fonts> contains:

  <\description-paragraphs>
    <item*|<source-link|fonts-ec.scm|TeXmacs/progs/fonts/fonts-ec.scm>, <source-link|fonts-adobe.scm|TeXmacs/progs/fonts/fonts-adobe.scm>,
    <source-link|fonts-x.scm|TeXmacs/progs/fonts/fonts-x.scm>, <source-link|fonts-math.scm|TeXmacs/progs/fonts/fonts-math.scm>,
    <source-link|fonts-foreign.scm|TeXmacs/progs/fonts/fonts-foreign.scm>, <source-link|fonts-misc.scm|TeXmacs/progs/fonts/fonts-misc.scm>,
    <source-link|fonts-composite.scm|TeXmacs/progs/fonts/fonts-composite.scm>, <source-link|fonts-truetype.scm|TeXmacs/progs/fonts/fonts-truetype.scm>>The
    rewriting rules of the old mechanism (<scm|set-font-rules>), loaded at
    boot time. They remain in use for <TeX> fonts (the master
    <verbatim|roman>) and for all fonts when the new style fonts are
    disabled.

    <item*|<source-link|font-old-menu.scm|TeXmacs/progs/fonts/font-old-menu.scm>>The menus <scm|text-font-menu>,
    <scm|math-font-menu> and <scm|prog-font-menu>, which list fonts by their
    old names. Loaded lazily.

    <item*|<source-link|font-new-widgets.scm|TeXmacs/progs/fonts/font-new-widgets.scm>>The font selector (dialog and side
    tool). Loaded lazily through <scm|open-font-selector>,
    <scm|open-document-font-selector> and
    <scm|open-document-other-font-selector>.

    <item*|<source-link|font-sample.scm|TeXmacs/progs/fonts/font-sample.scm>>Utilities which build tables of
    characters and font samples, used by the font selector and by
    maintainers for comparing fonts.
  </description-paragraphs>

  The commands <scm|scan-disk-for-fonts> and <scm|clear-font-cache>, bound
  to <menu|Tools|Fonts|Scan disk for fonts> and <menu|Tools|Fonts|Clear font
  cache>, are defined in <source-link|progs/texmacs/texmacs/tm-tools.scm|TeXmacs/progs/texmacs/texmacs/tm-tools.scm>.

  <section|The glue <abbr|API>>

  The routines below are exported in
  <source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>. Arrays of strings are passed
  as lists of strings, and logical fonts as lists whose first element is the
  family or master.

  <subsection|Font files>

  <\explain>
    <scm|(tt-exists? <scm-arg|name>)>

    <scm|(font-exists-in-tt? <scm-arg|name>)><explain-synopsis|is a font
    file available?>
  <|explain>
    Both call <cpp|tt_font_exists>: does a font file with base name
    <scm-arg|name> (without extension) exist in the font path?
  </explain>

  <\explain>
    <scm|(tt-font-name <scm-arg|u>)>

    <scm|(tt-dump <scm-arg|u>)>

    <scm|(tt-analyze <scm-arg|name>)><explain-synopsis|inspect font files>
  <|explain>
    Return the <verbatim|(family style)> pairs of the font file at the
    <abbr|URL> <scm-arg|u>; print its tables and names; compute the
    characteristics of the font with base name <scm-arg|name> (see
    <hlink|font characteristics|font-database-storage.en.tm>).
  </explain>

  <subsection|The database>

  <\explain>
    <scm|(font-database-build-local)>

    <scm|(font-database-extend-local <scm-arg|u>)>

    <scm|(font-database-build <scm-arg|u>)>

    <scm|(font-database-build-characteristics
    <scm-arg|force?>)><explain-synopsis|build the local database>
  <|explain>
    Scan the whole font path, respectively import the fonts at
    <scm-arg|u>, and save the local database; the last two functions are
    the individual steps (add the fonts at <scm-arg|u> to the in-memory
    table, compute missing or all characteristics) and do not save
    anything.
  </explain>

  <\explain>
    <scm|(font-database-build-global)>

    <scm|(font-database-insert-global <scm-arg|u>)>

    <scm|(font-database-save-local-delta)>

    <scm|(font-database-delta-families)><explain-synopsis|maintenance of the
    global database>
  <|explain>
    Rebuild the global database from the font path, respectively add
    the fonts at <scm-arg|u> to it; save the entries of the local database
    which are not in the global one, and list their families.
  </explain>

  <\explain>
    <scm|(font-database-load)>

    <scm|(font-database-save)>

    <scm|(font-database-filter)><explain-synopsis|low level operations>
  <|explain>
    Load the database if necessary; save the local database; keep only the
    entries of the in-memory table whose files exist.
  </explain>

  <\explain>
    <scm|(font-database-families)>

    <scm|(font-database-styles <scm-arg|family>)>

    <scm|(font-database-search <scm-arg|family>
    <scm-arg|style>)>

    <scm|(font-database-characteristics <scm-arg|family>
    <scm-arg|style>)>

    <scm|(font-database-substitutions
    <scm-arg|family>)><explain-synopsis|queries>
  <|explain>
    The sorted list of installed families; the sorted styles of a family;
    the file names of a physical font (subfonts of collections are named
    <verbatim|file.n.ttf>); its characteristics; the substitution rules for
    a family.
  </explain>

  <subsection|Features and distances>

  <\explain>
    <scm|(font-family-\<gtr\>master <scm-arg|family>)>

    <scm|(font-master-\<gtr\>families <scm-arg|master>)>

    <scm|(font-master-features <scm-arg|master>)>

    <scm|(font-family-features <scm-arg|family>)>

    <scm|(font-family-strict-features <scm-arg|family>)>

    <scm|(font-style-features <scm-arg|style>)><explain-synopsis|features>
  <|explain>
    Glue for <cpp|family_to_master>, <cpp|master_to_families>,
    <cpp|master_features>, <cpp|family_features>,
    <cpp|family_strict_features> and <cpp|style_features>.
  </explain>

  <\explain>
    <scm|(font-guessed-features <scm-arg|family> <scm-arg|style>)>

    <scm|(font-family-guessed-features <scm-arg|family>
    <scm-arg|pure?>)>

    <scm|(font-guessed-distance <scm-arg|fam1> <scm-arg|sty1>
    <scm-arg|fam2> <scm-arg|sty2>)>

    <scm|(font-master-guessed-distance <scm-arg|master1>
    <scm-arg|master2>)>

    <scm|(characteristic-distance <scm-arg|chars1>
    <scm-arg|chars2>)><explain-synopsis|guesses>
  <|explain>
    Features guessed from the characteristics and the distances based on
    them (<cpp|guessed_features>, <cpp|guessed_distance>,
    <cpp|characteristic_distance>).
  </explain>

  <subsection|Logical fonts>

  <\explain>
    <scm|(logical-font-public <scm-arg|family> <scm-arg|style>)>

    <scm|(logical-font-exact <scm-arg|family> <scm-arg|style>)>

    <scm|(logical-font-private <scm-arg|font> <scm-arg|variant>
    <scm-arg|series> <scm-arg|shape>)><explain-synopsis|construct logical
    fonts>
  <|explain>
    The logical font of a physical font (<cpp|logical_font> with two
    arguments), its exact version (<cpp|logical_font_exact>), and the
    logical font of an internal description (<cpp|logical_font> with four
    arguments). Example: <scm|(logical-font-private "pagella" "rm" "bold"
    "italic")> returns <scm|("TeX Gyre Pagella" "bold" "italic")>.
  </explain>

  <\explain>
    <scm|(logical-font-family <scm-arg|lf>)>

    <scm|(logical-font-variant <scm-arg|lf>)>

    <scm|(logical-font-series <scm-arg|lf>)>

    <scm|(logical-font-shape <scm-arg|lf>)><explain-synopsis|back to the
    internal scheme>
  <|explain>
    The values of the environment variables <src-var|font>,
    <src-var|font-family>, <src-var|font-series> and <src-var|font-shape>
    corresponding to the logical font <scm-arg|lf>.
  </explain>

  <\explain>
    <scm|(logical-font-search <scm-arg|lf>)>

    <scm|(logical-font-search-exact <scm-arg|lf>)>

    <scm|(logical-font-substitute <scm-arg|lf>)>

    <scm|(logical-font-patch <scm-arg|lf> <scm-arg|features>)><explain-synopsis|search
    and modify>
  <|explain>
    The closest physical font (<cpp|search_font> with attempt 1,
    <cpp|search_font_exact>); the logical font after substitutions
    (<cpp|apply_substitutions>); the logical font in which the given
    features (in user interface spelling, such as <verbatim|"Small
    Capitals">) replace the features of the same kind (<cpp|patch_font>).
  </explain>

  <\explain>
    <scm|(search-font-families <scm-arg|features>)>

    <scm|(search-font-styles <scm-arg|family>
    <scm-arg|features>)><explain-synopsis|filter by properties>
  <|explain>
    The installed families having at least one style with the given
    features, and the styles of a family with these features. A style is
    accepted if, for each requested feature, the asymmetric distance to the
    exact logical font of the style is zero or smaller than the distance to
    a font without any feature: for instance, a <verbatim|Black> style is
    accepted when <verbatim|Bold> is asked for, but not conversely.
  </explain>

  <\explain>
    <scm|(font-family-main <scm-arg|font>)><explain-synopsis|main family>
  <|explain>
    The main family of a family list such as <verbatim|"bold=Fira
    Sans,TeX Gyre Pagella"> (<cpp|main_family>).
  </explain>

  <section|The font selector>

  <subsection|Entry points>

  The font selector is implemented in
  <source-link|progs/fonts/font-new-widgets.scm|TeXmacs/progs/fonts/font-new-widgets.scm>. It exists in two forms: a
  dialog (<scm|font-selector>) and a side tool (<scm|font-tool>), the
  latter being used when side tools are enabled (<scm|side-tools?>). The
  public entry points are:

  <\description>
    <item*|<scm|(open-font-selector)>>Changes the font at the cursor
    position (<menu|Format|Font> in the compressed menus). The getter is
    <scm|get-env> and the setter <scm|make-multi-with>, which inserts or
    modifies a <markup|with> tag.

    <item*|<scm|(open-document-font-selector)>>Changes the document wide
    font (<menu|Document|Font>). The getter is <scm|get-init> and the setter
    <scm|init-multi>, which modifies the initial environment.

    <item*|<scm|(open-document-other-font-selector
    <scm-arg|prefix>)>>Changes a style parameter of type font, such as the
    font used for titles: the environment variables are prefixed by
    <scm-arg|prefix> (for instance <verbatim|font-series> becomes
    <scm-arg|prefix> followed by <verbatim|font-series>). It is called from
    the <menu|Other> entry of the parameter menus in
    <source-link|progs/generic/generic-menu.scm|TeXmacs/progs/generic/generic-menu.scm>.
  </description>

  <subsection|State>

  All widgets receive a list <scm-arg|specs> of the form <scm|(getter setter
  global? [window])>. The getter reads environment variables
  (<verbatim|"font">, <verbatim|"font-family">, ...), the setter applies a
  flat list of variable/value pairs, and <scm-arg|global?> tells whether the
  document wide font is being edited. The current choices are stored in the
  hash table <scm|selector-table>, indexed by <scm-arg|specs>, a variable
  and the buffer of the window, so that several selectors can coexist. The
  variables are:

  <\itemize>
    <item>the font variables <scm|:family>, <scm|:style> and <scm|:size>,
    whose initial values are computed by <scm|initial-font-data> from the
    current environment: the internal description is translated into a
    logical font (<scm|logical-font-private>) and then into the physical
    font which is shown as selected (<scm|logical-font-search-exact>);

    <item>the filter variables <scm|:weight>, <scm|:slant>, <scm|:stretch>,
    <scm|:serif>, <scm|:spacing>, <scm|:case>, <scm|:device>,
    <scm|:category> and <scm|:glyphs>, whose initial value is
    <verbatim|"Any">;

    <item>the customization variables <verbatim|"bold">,
    <verbatim|"italic">, <verbatim|"smallcaps">, <verbatim|"sansserif">,
    <verbatim|"typewriter">, <verbatim|"math">, <verbatim|"greek">,
    <verbatim|"bbb">, <verbatim|"cal">, <verbatim|"frak"> (alternative
    families for these purposes) and <verbatim|"embold">,
    <verbatim|"embbb">, <verbatim|"slant">, <verbatim|"hmagnify">,
    <verbatim|"vmagnify">, <verbatim|"hextended">, <verbatim|"vextended">
    (font effects), whose initial values are parsed from the current values
    of <src-var|font> and <src-var|font-effects>
    (<scm|initial-customize-get>).
  </itemize>

  <subsection|From the widgets to the database>

  <\itemize>
    <item>The families listed in the family column are
    <scm|(selected-families specs)>, that is,
    <scm|search-font-families> applied to the non <verbatim|"Any">
    filters (<scm|selected-properties>); the styles listed for the selected
    family are given by <scm|search-font-styles>. The user interface
    strings (<verbatim|"Sans Serif">, <verbatim|"Math Symbols">, ...) are
    decoded on the <c++> side by <cpp|decode_feature>.

    <item>The selected font is <scm|(selector-get-font specs)>: the logical
    font of the chosen family and style (<scm|logical-font-public>), patched
    with the filter properties (<scm|logical-font-patch>). Hence, choosing
    the style <verbatim|Regular> together with the filter
    <verbatim|Weight: Bold> asks for a bold font even if the chosen family
    has no bold style.

    <item>The new values of the environment variables are obtained with
    <scm|logical-font-family>, <scm|logical-font-variant>,
    <scm|logical-font-series> and <scm|logical-font-shape>. The value of
    <src-var|font> is completed by the customizations
    (<scm|logical-font-family*>), which yields family lists such as
    <verbatim|"bold=Fira Sans,TeX Gyre Pagella"> that are interpreted by
    the smart fonts; the effects are assembled into a string such as
    <verbatim|"hmagnify=1.1,bold=2"> for <src-var|font-effects>
    (<scm|selector-font-effects>).

    <item><scm|selector-get-changes> compares these values with the current
    ones and returns the list of changed variables. Each modification of a
    choice (<scm|selector-set>) immediately applies the changes through the
    setter (<scm|selector-notify>) and refreshes the dependent widgets. The
    <menu|Ok> button of the dialog closes it with the remaining changes, and
    <menu|Reset> (document fonts only) resets all font variables to their
    defaults (<scm|selector-restore>).

    <item>The sample area typesets a sample text (<scm|set-font-sample-kind>
    selects standard text, mathematics, the current selection, or tables of
    <name|Unicode> characters built by <scm|build-character-table>) in a
    <markup|with> tag with the new variables. When the physical font that
    will actually be used differs from the requested one, the widget
    <scm|selector-font-simulate-widget> shows both (\PRequested\Q and
    \PReplaced by\Q); the latter is computed with
    <scm|logical-font-search>, that is, by the selection algorithm itself.

    <item>The <menu|Import> button (and <menu|Import font> in the
    <menu|More> tab of the side tool) calls <scm|font-import>, which calls
    <scm|font-database-extend-local> on the chosen file and refreshes the
    lists. The side tool also offers <menu|Scan disk for more fonts> and
    <menu|Clear font cache>.
  </itemize>

  <section|Adding fonts>

  <subsection|Making a new font available>

  For a user, a font becomes available as soon as it is in the local
  database:

  <\enumerate>
    <item>Put the file into one of the directories of the font path (for
    instance <verbatim|$TEXMACS_HOME_PATH/fonts/truetype>), or use the
    <menu|Import> button of the font selector, which also adds the directory
    to the font path.

    <item>Run <menu|Tools|Fonts|Scan disk for fonts> (unless the font was
    imported). The new families appear in the font selector immediately.
  </enumerate>

  If the font gets an unexpected family or style name, inspect it with
  <scm|tt-dump> and <scm|tt-font-name>. If its features are wrong (for
  instance a sans serif font not recognized as such), they can be corrected
  in <verbatim|$TEXMACS_HOME_PATH/fonts/font-features.scm>; the changes are
  taken into account at the next start, and they are lost after an upgrade
  or a cache clearance.

  <subsection|Adding a font to the global database>

  For fonts which are common enough to be known by all installations, the
  global database has to be extended. A possible workflow is:

  <\enumerate>
    <item>Install the fonts and scan the disk.

    <item>Run <scm|(font-database-save-local-delta)>, then
    <scm|(font-test)> from <source-link|font-sample.scm|TeXmacs/progs/fonts/font-sample.scm> in a <scheme>
    session: it returns a table showing, for each new font, its name, the
    weights, slants, stretches and other properties that the selection
    algorithm associates to it, and some sample text. This makes it easy to
    spot wrongly classified fonts.

    <item>Run <scm|(font-database-insert-global (url "dir"))> from a
    development tree (the files in <verbatim|$TEXMACS_PATH/fonts> are
    overwritten), review the guessed features in
    <verbatim|font-features.bis.scm>, merge them by hand into
    <source-link|font-features.scm|TeXmacs/fonts/font-features.scm> (fixing masters and adding categories such
    as <verbatim|Calligraphic> or <verbatim|Handwritten>, which cannot be
    guessed), and restart <TeXmacs>.

    <item>If appropriate, add substitution rules to
    <source-link|font-substitutions.scm|TeXmacs/fonts/font-substitutions.scm>, and a short alias to
    <cpp|upgrade_family_name> in <source-link|Graphics/Fonts/font_translate.cpp|src/Graphics/Fonts/font_translate.cpp>
    (as for <verbatim|pagella> or <verbatim|dejavu>).
  </enumerate>

  Mathematical fonts usually also need microtypographic adjustments, which
  live in the files <verbatim|Plugins/Freetype/adjust_*.cpp>, and possibly
  special cases in the smart fonts (<cpp|tex_gyre_fix>, <cpp|math_fix>,
  ...); see <hlink|smart fonts|smart-fonts.en.tm> and
  <hlink|mathematical typesetting|maths.en.tm>.

  <subsection|Supporting a new font format>

  The database is built around <name|TrueType> and <name|OpenType> files.
  Supporting another format which <name|FreeType> can read requires changes
  at several places, which must remain consistent:

  <\itemize>
    <item><cpp|font_database_build> and <cpp|font_database_collect> (in
    <source-link|font_database.cpp|src/Graphics/Fonts/font_database.cpp>) decide by extension which files are
    scanned, respectively kept when filtering the global database;

    <item><cpp|tt_font_name> must be able to extract the family and style
    names, or a separate reader must be called by
    <cpp|font_database_build>;

    <item><cpp|font_database_build_characteristics> and
    <cpp|font_database_search> map locations to font names by removing
    known extensions;

    <item><cpp|tt_font_find_sub> (in <source-link|tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>) decides which
    extensions are tried when a font name is looked up; currently
    <verbatim|.pfb>, <verbatim|.ttf>, <verbatim|.ttc>, <verbatim|.otf> and
    <verbatim|.dfont>, so that <verbatim|.pfb> and <verbatim|.dfont> files
    can be used by name but are never scanned;

    <item>the font is finally constructed by <cpp|unicode_font> in the
    database fallback of <cpp|find_font_bis>, and analyzed by
    <cpp|tt_analyze> through <cpp|tt_font> and <cpp|tt_font_metric>; both
    load the file through <name|FreeType>.
  </itemize>

  <section|Debugging>

  <subsection|Inspecting the selection in a <scheme> session>

  The glue functions make it easy to replay each step of the selection:

  <\scm-code>
    (use-modules (fonts font-sample))

    (font-database-styles "TeX Gyre Pagella")

    (font-database-characteristics "TeX Gyre Pagella" "Regular")

    (logical-font-private "dejavu" "rm" "medium" "small-caps")

    (logical-font-substitute (logical-font-private "SimSun" "ss" "medium" "right"))

    (logical-font-search (logical-font-private "Garamond" "rm" "medium" "right"))

    (font-database-search "DejaVu Serif" "Book")

    (show-closest-fonts '("TeX Gyre Pagella" "Regular"))
  </scm-code>

  The module <source-link|font-sample.scm|TeXmacs/progs/fonts/font-sample.scm> is only loaded together with the
  font selector, hence the first line. The last function returns a table of
  the 25 installed fonts which are closest to the given one according to
  the guessed distance, together with their characteristics and samples.
  Remember that the internal caches of <cpp|search_font>,
  <cpp|find_closest> and <cpp|font::instances> keep old answers: after a
  change of the database, the selection in the current session may not
  reflect it.

  <subsection|Console messages and traces>

  The database code reports on the standard output:

  <\description>
    <item*|<verbatim|TeXmacs] missing 'X' family> (or
    <verbatim|master>)>A family used by a document or style is not
    installed; it is followed by <verbatim|TeXmacs] warning, missing font,
    loading global substitution list> the first time.

    <item*|<verbatim|TeXmacs] approximating font ...>>While deriving the
    local database from the global one, a file was identified by name and
    subfont index but not by size.

    <item*|<verbatim|Process ...>, <verbatim|Analyzing ...>, <verbatim|\|
    Processing ...>>Progress of a disk scan and of the computation of
    characteristics, followed by the computed characteristics.
  </description>

  Finer traces can be obtained by uncommenting the <verbatim|cout>
  statements which are already present in <cpp|search_font>,
  <cpp|search_font_among>, <cpp|find_closest> and the four argument
  <cpp|font_database_search>.

  <subsection|Starting from a clean state>

  In order to make sure that a problem is not caused by stale data, quit
  <TeXmacs> and remove <verbatim|$TEXMACS_HOME_PATH/fonts/font-*.scm>,
  <verbatim|$TEXMACS_HOME_PATH/fonts/unpacked> and
  <verbatim|$TEXMACS_HOME_PATH/system/cache/font_cache.scm>. At the next
  start, the local database is derived again from the global one, and the
  disk has to be scanned again if needed. The command <menu|Tools|Fonts|Clear
  font cache> does part of this from within <TeXmacs>, but a restart is still
  necessary.

  <section|Pitfalls>

  <\itemize>
    <item>The environment variable <src-var|font-family> contains the
    <em|variant> (<verbatim|rm>, <verbatim|ss>, <verbatim|tt>), while the
    <c++> <em|family> is the value of <src-var|font>. Similarly, the
    <em|style> of a physical font combines what <TeXmacs> calls the series
    and the shape.

    <item>The local database only contains fonts of the global database
    which are found on disk, plus the fonts found by explicit scans or
    imports. A newly installed font is invisible until the disk is
    scanned, and scanned fonts are forgotten after an upgrade or a cache
    clearance.

    <item>Fonts outside the hard coded font path are never found, unless
    they are imported or the path is extended with
    <verbatim|TEXMACS_FONT_PATH>.

    <item>Negative answers are cached in <verbatim|font_cache.scm>: a font
    file which was looked up before being installed may remain
    \Pmissing\Q until the font cache is cleared.

    <item>Subfonts extracted from <verbatim|.ttc> collections into
    <verbatim|fonts/unpacked> are not updated when the collection changes.

    <item>Rule heads are case sensitive. The old lower case names
    (<verbatim|pagella>, <verbatim|dejavu>) go through the <scheme> rules
    when the new style fonts are disabled, whereas the database names
    (<verbatim|TeX Gyre Pagella>, <verbatim|DejaVu>) go through the
    database. When adding rules, make sure not to shadow database families.

    <item>The search itself only uses the names and the features of the
    database, not the characteristics. A style whose weight or slant is
    expressed by an unusual word (in another language, for instance) is
    not recognized as such: it may not be selected for a bold or italic
    request, and a synthetic emphasis (<verbatim|-poorbf>,
    <verbatim|-poorit>) is then applied to another style. Adding features
    or substitution rules fixes this.

    <item>Substitution rules are keyed by the family name after
    <cpp|upgrade_family_name>, and a rule is silently ignored when its
    target family is not installed.

    <item>Only the English names (name identifiers 1 and 2) of a font file
    are read. A font without English name records gets an empty family
    name.

    <item>The guessed features and distances depend on the rendering of a
    few glyphs. Decorative or symbol fonts may be misclassified; the
    features in <source-link|font-features.scm|TeXmacs/fonts/font-features.scm> always take precedence over
    guesses.
  </itemize>

  <subsection|Known problems in the current code>

  The following problems were noticed while writing this documentation:

  <\itemize>
    <item>In <source-link|progs/fonts/font-new-widgets.scm|TeXmacs/progs/fonts/font-new-widgets.scm>,
    <scm|open-document-other-font-selector> calls
    <scm|(open-document-other-font-selector prefix-window)> when side tools
    are disabled; the intended call is
    <scm|(open-document-other-font-selector-window prefix)>.

    <item>In <source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>, at attempts
    <math|\<gtr\>1> of <cpp|smart_font_rep::resolve>, the test <cpp|v ==
    "rm"> is made on an empty string, where <cpp|variant == "rm"> was
    probably meant; this is harmless, since <verbatim|rm> is dropped by
    <cpp|variant_features> anyway. In the same file,
    <cpp|in_unicode_range>, which returns a <cpp|bool>, returns
    <cpp|""> (that is, <cpp|true>) for strings that cannot be decoded.

    <item>In <source-link|Plugins/Freetype/tt_analyze.cpp|src/Plugins/Freetype/tt_analyze.cpp>, the fallback
    definition of <cpp|characteristic_distance> used without
    <name|FreeType> returns an <cpp|int>, whereas the header declares a
    <cpp|double>.

    <item>The save functions of <source-link|font_database.cpp|src/Graphics/Fonts/font_database.cpp> remove
    <verbatim|$TEXMACS_PATH/system/cache/file_cache>, whereas the caches
    of <source-link|data_cache.cpp|src/System/Misc/data_cache.cpp> live in
    <verbatim|$TEXMACS_HOME_PATH/system/cache>.
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
