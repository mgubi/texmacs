<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Smart fonts, emulated fonts and virtual fonts>

  <section|Introduction>

  The <hlink|overview of fonts|fonts.en.tm> explains the general
  philosophy of fonts in <TeXmacs>: a font is an object which associates a
  graphical meaning to <em|words>, that is to strings of Cork characters and
  universal symbols like <verbatim|\<less\>alpha\<gtr\>> or
  <verbatim|\<less\>#2212\<gtr\>>. The present chapter describes the layer
  which makes this philosophy work in practice with real, incomplete
  physical fonts. It consists of three cooperating mechanisms:

  <\description>
    <item*|Smart fonts>A <em|smart font> (<cpp|smart_font_rep> in
    <source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>) is the font which the
    typesetter actually uses when the preference <verbatim|"new style
    fonts"> is on. It is a dispatcher: it cuts each string into runs of
    characters, finds for every character a <em|subfont> which can render
    it, possibly <em|rewrites> the characters into the encoding expected by
    that subfont, and delegates measuring and drawing. Subfonts are created
    lazily and the decisions are cached.

    <item*|Virtual fonts>A <em|virtual font> (<cpp|virtual_font_rep> in
    <source-link|Graphics/Fonts/virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp>) builds new glyphs out of
    glyphs of a base font, following definitions written in a small
    <scheme>-like language in the files
    <verbatim|$TEXMACS_PATH/fonts/virtual/*.vfn>. Long arrows, negated
    relations, missing brackets, integrals with several signs or extensible
    delimiters are all obtained in this way when the physical font does not
    provide them.

    <item*|Emulated fonts>The \Ppoor man's\Q fonts in
    <verbatim|Graphics/Fonts/poor_*.cpp> synthesize variants which do not
    exist physically: bold, italic, small capitals, blackboard bold,
    stretched, extended or condensed, monospaced, distorted or blurred
    variants of an existing font, as well as extensible delimiters. They
    are used automatically when the font database does not contain the
    requested series or shape, and explicitly through the
    <src-var|font-effects> environment variable.
  </description>

  The three mechanisms are composable: a smart font may have virtual fonts
  and emulated fonts among its subfonts, a virtual font may be built on top
  of a smart font or of an emulated font, and the font effects are applied
  on top of the smart font. All of them are subclasses of the abstract
  <cpp|font_rep> (see <source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp> and the
  <hlink|overview|fonts.en.tm>) and are therefore interchangeable from the
  point of view of the typesetter and of the <hlink|renderers|renderer.en.tm>.

  The selection of a <em|physical> font from a family name, variant, series
  and shape (the font database, <cpp|logical_font>, <cpp|search_font>,
  <cpp|find_closest> and <cpp|closest_font>) is documented separately in the
  chapter on the <hlink|font database and font selection|font-database.en.tm>.
  Here we take <cpp|closest_font> as a black box which returns the best
  available physical font, and we concentrate on what happens around it.
  The routing of characters is presented from the user's point of view,
  together with the font inspector, in <hlink|from markup to
  glyph|../fonts/font-guide.en.tm> and the following pages of the reference
  chapter; the <name|OpenType> math fonts, whose <verbatim|MATH> table
  changes several of the decisions described here, are treated in the
  chapter <hlink|<name|OpenType> fonts|opentype.en.tm>, and presented to
  users in <hlink|mathematical
  fonts|../../main/math/fonts/man-math-fonts.en.tm>.

  <section|Why smart fonts?>

  Physical fonts are always incomplete. A text font usually covers Latin and
  perhaps Greek and Cyrillic, but not mathematical symbols; a mathematical
  <name|OpenType> font has most symbols but no Chinese characters; few fonts
  contain bold calligraphic letters, double struck digits or a long
  <verbatim|\<less\>leftrightarrow\<gtr\>>. On the other hand, a <TeXmacs>
  document is written using a font independent encoding, and the user
  expects that every symbol is rendered, in a style as close as possible to
  the chosen font. Smart fonts reconcile these requirements:

  <\itemize>
    <item>Characters missing in the main font are taken from other fonts:
    explicitly mentioned fonts (the <src-var|font> variable may contain a
    comma separated list of families, possibly with conditions such as
    <verbatim|mathlarge=TeX Gyre Pagella,Linux Libertine>), and then from
    progressively less similar fonts found in the font database for the
    relevant <name|Unicode> range.

    <item>Fallback fonts are scaled so that their x-height matches the one of
    the main font (<cpp|smart_font_rep::adjusted_dpi>).

    <item>Mathematical conventions are implemented at the font level: in
    mathematical mode isolated Latin letters are taken from the italic
    variant of the font, Greek letters are taken from the italic math
    alphabet if available, <name|Unicode> mathematical alphanumeric symbols
    (<verbatim|U+1D400> and following) are mapped to bold, calligraphic,
    fraktur or double struck variants of the current family, and so on.
    When the main font is an <name|OpenType> math font without hand-tuned
    tables, the letters and the Greek are taken from its own mathematical
    italic alphabet instead, so that its italic corrections and kerns
    apply.

    <item>A smart font whose main font has a <verbatim|MATH> table forwards
    the questions of the typesetter about size variants, accents, extended
    shapes and feature substitutions to the subfont of the character
    (<cpp|get_rubber_variant>, <cpp|get_wide_variant>,
    <cpp|get_top_accent>, <cpp|is_extended_shape>,
    <cpp|get_feature_variant>, and the height dependent script
    corrections).

    <item>Symbols which no font provides are constructed with virtual fonts
    or emulated fonts, and only as a last resort drawn in red using the
    <em|error font>. For a main font with a <verbatim|MATH> table, an
    emulation which a <name|PDF> export could only hold as a bitmap gives
    way to the glyph of <name|STIX Two Math>, which is shipped with
    <TeXmacs>.
  </itemize>

  <section|Overview of the layers>

  The following picture summarizes the objects involved when the
  typesetter asks the current font to measure or draw a string.

  <\verbatim-code>
    edit_env_rep::update_font\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (Typeset/Env/env_semantics.cpp)

    \ \ fn = smart_font (family, variant, series, shape, size, dpi)

    \ \ fn = feature_font (fn, "ssty", ...)\ \ \ \ \ \ in scripts, OpenType math only

    \ \ fn = apply_features (fn, font-features)\ -\<gtr\> feature_font

    \ \ fn = apply_effects (fn, font-effects)\ \ \ \ -\<gtr\> poor_bold, poor_italic, ...

    \;

    smart_font_rep\ \ (one per family/variant/series/shape/size/dpi)

    \ \ sm : smart_map (shared by all sizes)\ \ \ \ \ char -\<gtr\> subfont number

    \ \ fn : array\<less\>font\<gtr\>\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ subfonts, created lazily

    \ \ \ \ fn[0]\ \ main font\ \ = closest_font (main family, ...)

    \ \ \ \ fn[1]\ \ error font = error_font (closest_font ("roman", "ss", ...))

    \ \ \ \ fn[k]\ \ e.g.\ \ closest_font (other family or range, ...)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ smart_font_bis (family, "outline", ...)\ \ \ for bbb

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ virtual_font (this, "tradi-long", ...)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ virtual_font (main, "emu-arrows", ..., true)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ poor_bold_font (...), poor_bbb_font (...)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ unicode_font ("STIXTwoMath-Regular", ...)\ \ \ "shipped-math"

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ rubber_font (fn[j])\ \ \ -\<gtr\> poor_rubber_font or MATH
  </verbatim-code>

  The rest of this chapter is organized as follows:

  <\traverse>
    <branch|The smart font: dispatching characters to
    subfonts|smart-fonts-smart.en.tm>

    <branch|The character resolution algorithm|smart-fonts-resolve.en.tm>

    <branch|Virtual fonts and the vfn language|smart-fonts-virtual.en.tm>

    <branch|Emulated (poor man's) fonts|smart-fonts-emulated.en.tm>

    <branch|Font effects|smart-fonts-effects.en.tm>

    <branch|Older composite fonts: compound and math
    fonts|smart-fonts-compound.en.tm>

    <branch|Extending, debugging and pitfalls|smart-fonts-howto.en.tm>
  </traverse>

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>>The smart font, the
    <cpp|smart_map> cache, the rewriting rules, the user entry points
    <cpp|smart_font>, <cpp|smart_font_bis> and <cpp|apply_effects>, the
    profile fixes of the <name|OpenType> math fonts (<cpp|profile_fix>), and
    the debugging support of the font inspector (<cpp|debug_draw>,
    <cpp|smart_font_debug_info>; see <hlink|inspecting the font
    system|opentype-tools.en.tm>).

    <item*|<source-link|Graphics/Fonts/math_font_profiles.cpp|src/Graphics/Fonts/math_font_profiles.cpp>,
    <source-link|feature_font.cpp|src/Graphics/Fonts/feature_font.cpp>>The profiles of the named <name|OpenType>
    math fonts, and fonts seen through an <name|OpenType> substitution
    feature (<cpp|feature_font>, <cpp|apply_features>); see <hlink|math font
    profiles|opentype-profiles.en.tm> and <hlink|<name|OpenType>
    features|opentype-features.en.tm>.

    <item*|<source-link|Graphics/Fonts/virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp>>Virtual fonts:
    compilation of <verbatim|.vfn> definitions into glyphs, metrics and
    vector drawing. <source-link|Graphics/Fonts/virtual_enhance.cpp|src/Graphics/Fonts/virtual_enhance.cpp> adds
    virtual definitions to an existing font.

    <item*|<source-link|Graphics/Fonts/translator.cpp|src/Graphics/Fonts/translator.cpp>>Loading of
    <verbatim|.enc> encodings and <verbatim|.vfn> files into
    <cpp|translator> objects.

    <item*|<verbatim|Graphics/Fonts/poor_*.cpp>>Emulated fonts:
    <verbatim|poor_bold>, <verbatim|poor_italic>,
    <verbatim|poor_smallcaps>, <verbatim|poor_bbb>,
    <verbatim|poor_stretched>, <verbatim|poor_extended>,
    <verbatim|poor_mono>, <verbatim|poor_distorted>,
    <verbatim|poor_effected> and <verbatim|poor_rubber>, together with
    <source-link|recolored_font.cpp|src/Graphics/Fonts/recolored_font.cpp> and <source-link|superposed_font.cpp|src/Graphics/Fonts/superposed_font.cpp>.

    <item*|<source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp>>The base class, the error font
    and <cpp|rubber_font>.

    <item*|<source-link|Graphics/Fonts/compound_font.cpp|src/Graphics/Fonts/compound_font.cpp>,
    <source-link|math_font.cpp|src/Graphics/Fonts/math_font.cpp>, <source-link|charmap.cpp|src/Graphics/Fonts/charmap.cpp>>Older composite fonts
    used by the rule based font selection.

    <item*|<source-link|Graphics/Bitmap_fonts/|src/Graphics/Bitmap_fonts>>Glyph (bitmap) manipulation
    routines used by virtual and emulated fonts (<cpp|join>,
    <cpp|hor_flip>, <cpp|bolden>, <cpp|slanted>, <cpp|make_bbb>, ...).

    <item*|<verbatim|$TEXMACS_PATH/fonts/virtual/>>The virtual font
    definitions <verbatim|tradi-*.vfn> and <verbatim|emu-*.vfn>.
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
