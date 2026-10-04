<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The font database and font selection>

  <section|Introduction>

  When a document asks for the font <verbatim|"pagella"> in bold italic, or
  for a font which is not installed at all, or for a Chinese character in a
  document typeset in Computer Modern, <TeXmacs> has to decide which physical
  font file will actually be used. This decision is taken by the <em|font
  selection> machinery, which relies on a <em|font database> describing the
  fonts that are available on the system: their family and style names, the
  files that contain them, a list of <em|features> (sans serif, monospaced,
  calligraphic, <abbr|etc.>) and a list of automatically computed
  <em|characteristics> (supported scripts, slant, x-height, stroke widths,
  <abbr|etc.>).

  This chapter describes the implementation of the database, of the
  selection algorithm and of the user interface built on top of them. It
  assumes that the reader is familiar with the general overview in
  <hlink|<TeXmacs> fonts|fonts.en.tm>, which explains what a <cpp|font> is
  and lists the concrete font classes; the present chapter does not repeat
  that material. The <em|smart fonts>, which combine several physical fonts
  in order to render arbitrary characters, as well as emulated, virtual and
  \Ppoor man's\Q synthetic fonts, are documented in the chapter on
  <hlink|smart fonts|smart-fonts.en.tm>. Here we only describe the
  interface between smart fonts and the database, that is, how a smart font
  asks the database for the closest font able to render a given character.
  How fonts are selected from the user's point of view, and which files make
  up the database, is described at a higher level in the reference chapter
  <hlink|fonts, from selection to glyph|../fonts/font-guide.en.tm>; the
  mathematical fonts shipped with <TeXmacs> are presented in <hlink|the
  section on mathematical fonts|../../main/math/fonts/man-math-fonts.en.tm>
  of the user manual, and the way the database registers the
  <name|OpenType> math fonts in <hlink|math font profiles, shipped fonts and
  the database|opentype-profiles.en.tm>.

  <\traverse>
    <branch|The font database: files, lifecycle and
    characteristics|font-database-storage.en.tm>

    <branch|The font selection algorithm|font-database-selection.en.tm>

    <branch|<scheme> interface, font selector and
    debugging|font-database-ui.en.tm>
  </traverse>

  <section|Architecture overview>

  <subsection|Two naming schemes>

  The central difficulty of font selection is that two unrelated naming
  schemes have to be reconciled.

  <\description>
    <item*|The internal (typesetter) scheme>Inside documents, fonts are
    described by the environment variables <src-var|font> (the <em|family>,
    such as <verbatim|roman>, <verbatim|pagella> or <verbatim|TeX Gyre
    Pagella>), <src-var|font-family> (the <em|variant>: <verbatim|rm>,
    <verbatim|ss>, <verbatim|tt>, ...), <src-var|font-series> (the weight:
    <verbatim|medium>, <verbatim|bold>, ...) and <src-var|font-shape>
    (<verbatim|right>, <verbatim|italic>, <verbatim|slanted>,
    <verbatim|small-caps>, ...), together with the size. Notice the
    unfortunate terminology: the environment variable <src-var|font-family>
    holds what the <c++> code calls the <em|variant>, while the <c++>
    <em|family> is the value of <src-var|font>.

    <item*|The physical (database) scheme>Font files declare a family name
    (such as <verbatim|DejaVu Sans Mono>) and a style name (such as
    <verbatim|Bold Oblique>) in their <verbatim|name> table. The database is
    indexed by such pairs <verbatim|(family, style)>.
  </description>

  Between both, the selection code uses a third, purely internal
  representation: the <em|logical font>, which is an array of strings whose
  first element is a <em|master> family name and whose other elements are
  normalized features, for instance <verbatim|["DejaVu", "mono",
  "sansserif", "bold"]>. Both internal and physical descriptions are
  translated into logical fonts, and the selection consists of finding the
  physical font whose logical description is closest to the requested one.

  <subsection|The layers>

  From bottom to top, the implementation consists of the following layers.

  <\description>
    <item*|Direct access to font files>The files
    <verbatim|Plugins/Freetype/tt_tools.cpp> and
    <verbatim|Plugins/Freetype/tt_file.cpp> read <name|TrueType> and
    <name|OpenType> files (including <verbatim|.ttc> collections) without
    going through <name|FreeType>, extract the family and style names
    (<cpp|tt_font_name>), and locate font files on disk
    (<cpp|tt_font_path>, <cpp|tt_font_find>, <cpp|tt_font_exists>; a name is
    looked for as <verbatim|.otf>, <verbatim|.ttf> and <verbatim|.ttc> before
    <verbatim|.pfb>). <verbatim|tt_tools.cpp> also reads the
    <name|OpenType> layout tables <verbatim|MATH>, <verbatim|GSUB> and
    <verbatim|GPOS>, which serve the fonts themselves rather than the
    database (see <hlink|the <name|OpenType> layout
    tables|opentype-tables.en.tm>). The file
    <verbatim|Plugins/Freetype/tt_analyze.cpp> renders a few glyphs with
    <name|FreeType> in order to compute the characteristics of a font
    (<cpp|tt_analyze>).

    <item*|The database>The file <verbatim|Graphics/Fonts/font_database.cpp>
    maintains the global hash tables <cpp|font_table>,
    <cpp|font_features>, <cpp|font_variants>,
    <cpp|font_characteristics> and <cpp|font_substitutions>, loads and
    saves them as <scheme> files, builds them by scanning the disk, and
    answers elementary queries (<cpp|font_database_families>,
    <cpp|font_database_styles>, <cpp|font_database_search>,
    <cpp|font_database_characteristics>, ...).

    <item*|Logical fonts and distances>The file
    <verbatim|Graphics/Fonts/font_select.cpp> translates physical fonts into
    logical fonts (<cpp|logical_font>, <cpp|logical_font_exact>), defines
    a distance between logical fonts and implements the search for the
    closest physical font (<cpp|search_font>). The file
    <verbatim|Graphics/Fonts/font_guess.cpp> derives features from
    characteristics (<cpp|guessed_features>) and defines a finer
    \Pguessed\Q distance used to break ties (<cpp|guessed_distance>).

    <item*|Translation to the internal scheme>The file
    <verbatim|Graphics/Fonts/font_translate.cpp> converts between the
    internal scheme and logical fonts (<cpp|logical_font> with four
    arguments, <cpp|get_family>, <cpp|get_variant>, <cpp|get_series>,
    <cpp|get_shape>), upgrades old family names
    (<cpp|upgrade_family_name>) and implements <cpp|find_closest> and
    <cpp|closest_font>, which return the closest available font for a
    given internal description, possibly decorated with synthetic bold,
    italic, small capitals or blackboard bold.

    <item*|Font construction>The file <verbatim|Graphics/Fonts/find_font.cpp>
    constructs the actual font objects from font names, either through
    rewriting rules declared in <scheme> (the \Pold\Q mechanism) or, when no
    rule exists for a family, through the database.

    <item*|Smart fonts and the typesetter>The typesetter calls
    <cpp|smart_font> from <cpp|edit_env_rep::update_font>
    (<verbatim|Typeset/Env/env_semantics.cpp>) whenever a font related
    environment variable changes. Smart fonts call <cpp|closest_font>
    repeatedly, for the main font and for each character that the main
    font cannot render.

    <item*|<scheme> and the user interface>Most database and selection
    routines are exported to <scheme> (see
    <verbatim|Scheme/Glue/build-glue-basic.scm>). The font selector dialog
    and side tool in <verbatim|progs/fonts/font-new-widgets.scm> are written
    entirely in terms of these routines.
  </description>

  <subsection|Data flow for a typical request>

  The following sequence summarizes what happens when the typesetter
  encounters the markup <verbatim|\<less\>with\|font\|pagella\|font-series\|bold\|...\<gtr\>>.
  The details of each step are given in the subsequent sections.

  <\enumerate>
    <item><cpp|edit_env_rep::update_font> calls <cpp|smart_font
    ("pagella", "rm", "bold", "right", sz, dpi)>.

    <item>When the preference <verbatim|"new style fonts"> is enabled (the
    default), <cpp|smart_font_bis> applies a few family name fixes (here
    <cpp|tex_gyre_fix> rewrites <verbatim|pagella> into <verbatim|TeX Gyre
    Pagella>; the last one, <cpp|profile_fix>, replaces a text family by the
    math font of its profile in math shapes, and conversely, and registers
    a profiled font which is installed but missing from the database) and
    calls <cpp|closest_font> on the main family in order to construct the
    base font of a new smart font.

    <item><cpp|find_closest> translates the request into the logical font
    <verbatim|["TeX Gyre Pagella", "bold"]>, applies the substitutions
    from <verbatim|font-substitutions.scm>, and calls <cpp|search_font>,
    which returns the physical font <verbatim|("TeX Gyre Pagella",
    "Bold")>. This pair is translated back into the internal description
    <verbatim|("TeX Gyre Pagella", "rm", "bold", "right")>.

    <item><cpp|find_font> finds no <scheme> rewriting rule for the family
    <verbatim|TeX Gyre Pagella>, so it asks the database for the files of
    that font (<cpp|font_database_search>) and constructs
    <cpp|unicode_font ("texgyrepagella-bold", sz, dpi)>.

    <item>Later, when the smart font meets a character that is not
    supported by this font, it calls <cpp|closest_font> again with an
    increasing <em|attempt> number and a variant that mentions the
    <name|Unicode> range of the character, so that the database returns
    successively other fonts which do support that range.
  </enumerate>

  <subsection|Source files>

  <\description-paragraphs>
    <item*|<verbatim|Graphics/Fonts/font.hpp>>Declarations of all public
    database and selection routines (sections \PFont database\Q and \PFont
    selection\Q at the end of the file) and of <cpp|FONT_ATTEMPTS>.

    <item*|<verbatim|Graphics/Fonts/font_database.cpp>>The database tables,
    their loading, saving, filtering and building.

    <item*|<verbatim|Graphics/Fonts/font_select.cpp>>Features, logical fonts,
    distances, <cpp|search_font>, <cpp|patch_font>,
    <cpp|apply_substitutions>.

    <item*|<verbatim|Graphics/Fonts/font_guess.cpp>>Features guessed from
    characteristics and guessed distances.

    <item*|<verbatim|Graphics/Fonts/font_translate.cpp>>Translation from and
    to the internal naming scheme, <cpp|find_closest>,
    <cpp|closest_font>.

    <item*|<verbatim|Graphics/Fonts/find_font.cpp>>Font rules and the
    construction of fonts from names (<cpp|find_font>).

    <item*|<verbatim|Graphics/Fonts/smart_font.cpp>>Smart fonts
    (<cpp|smart_font>, <cpp|main_family>, <cpp|get_unicode_range>), see
    <hlink|smart fonts|smart-fonts.en.tm>.

    <item*|<verbatim|Graphics/Fonts/math_font_profiles.cpp>>The profiles of
    the named <name|OpenType> math fonts (<cpp|math_font_profile_attr>,
    <cpp|math_family_for_text>, <cpp|text_family_for_math>), declared from
    <scheme> in <verbatim|progs/fonts/fonts-opentype.scm>; see <hlink|math
    font profiles|opentype-profiles.en.tm>.

    <item*|<verbatim|Plugins/Freetype/tt_file.cpp>,
    <verbatim|tt_tools.cpp>, <verbatim|tt_analyze.cpp>>Locating, parsing and
    analyzing font files.

    <item*|<verbatim|$TEXMACS_PATH/fonts/*.scm>>The global database shipped
    with <TeXmacs> (in the source tree: <verbatim|src/TeXmacs/fonts>).

    <item*|<verbatim|progs/fonts/>>Font rules for the old mechanism
    (<verbatim|fonts-*.scm>), the old font menus
    (<verbatim|font-old-menu.scm>), the font selector
    (<verbatim|font-new-widgets.scm>), tools for sampling and comparing
    fonts (<verbatim|font-sample.scm>), the profiles and menus of the
    <name|OpenType> math fonts (<verbatim|fonts-opentype.scm>,
    <verbatim|font-short-menu.scm>), the <name|OpenType> features
    (<verbatim|font-features.scm>) and the font inspector
    (<verbatim|font-debug.scm>, see <hlink|inspecting the font
    system|opentype-tools.en.tm>).
  </description-paragraphs>

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
