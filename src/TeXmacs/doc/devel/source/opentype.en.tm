<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|OpenType fonts>

  Most fonts on a modern system are <name|OpenType> fonts, and an
  <name|OpenType> font carries much more than outlines and metrics: layout
  tables which say how glyphs are substituted and positioned and, for the
  mathematical fonts, a <verbatim|MATH> table which declares everything a
  formula typesetter needs. This chapter describes how <TeXmacs> reads
  these tables and puts them to work: the mathematics of any
  <name|OpenType> math font without per-font <c++> code, the stretchable
  delimiters, accents and arrows built from the font's own parts, the
  features of text fonts such as old style figures, and the profiles which
  pair each mathematical font with its text companions.

  One rule shapes the whole design: <em|the hand-tuned tables win>. The
  tables of <verbatim|adjust_*.cpp> and the per-family branches of
  <source-link|unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp> (for the <name|TeX Gyre> and the first
  <name|STIX> fonts) were crafted against the layout of <TeXmacs> and keep
  precedence wherever they say something; the <verbatim|MATH> table fills
  what they leave open. The tuning can be switched off for comparison with
  <scm|(set-hand-tuned-math-fonts #f)>.

  <section|Overview>

  The support is organized in layers, from the font file to the
  typesetter:

  <\verbatim-code>
    .otf file

    \ \ \|\ \ tt_face (tt_face.cpp): MATH, and on demand GSUB and GPOS

    \ \ v

    parsed tables \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (tt_tools.cpp)

    \ \ \|\ \ unicode_font: MATH parameters, before the tuned branches

    \ \ v

    font_rep parameters and hooks \ \ \ \ \ \ \ (font.hpp)

    \ \ \|\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \|\ \ rubber_unicode_font

    \ \ v \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ v

    typesetter \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ variants and assemblies

    (fractions, radicals, scripts, \ \ \ \ \ \ (virtual-font glyph algebra)

    \ accents, delimiters, operators)

    \;

    GSUB \ -\<gtr\> feature_font: font-features, dtls, flac, ssty

    GPOS \ -\<gtr\> pair kerning of text

    profiles (fonts-opentype.scm) -\<gtr\> smart_font routing and the font menus
  </verbatim-code>

  The pages of this chapter follow these layers. They describe the
  implementation; how to choose fonts, how a request is resolved and how
  the configuration files look is the subject of the reference chapter
  <hlink|fonts, from selection to glyph|../fonts/font-guide.en.tm>, and
  the mathematical fonts as seen by users, with a specimen of every shipped
  font, are described in the section <hlink|mathematical
  fonts|../../main/math/fonts/man-math-fonts.en.tm> of the user manual.

  <section|Source files>

  All file names are relative to <source-link|src/src/|src> unless stated
  otherwise.

  <\description-paragraphs>
    <item*|<source-link|Plugins/Freetype/tt_tools.hpp|src/Plugins/Freetype/tt_tools.hpp>,
    <source-link|tt_tools.cpp|src/Plugins/Freetype/tt_tools.cpp>>The readers of the <verbatim|MATH>,
    <verbatim|GSUB> and <verbatim|GPOS> tables.

    <item*|<source-link|Plugins/Freetype/tt_face.hpp|src/Plugins/Freetype/tt_face.hpp>,
    <source-link|tt_face.cpp|src/Plugins/Freetype/tt_face.cpp>>The parsed tables, cached per face, and the
    kerning.

    <item*|<source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>>Activation of the
    <verbatim|MATH> table, the constants and the glyph corrections.

    <item*|<source-link|Plugins/Freetype/rubber_unicode_font.cpp|src/Plugins/Freetype/rubber_unicode_font.cpp>>Stretchable
    glyphs: variants and assemblies.

    <item*|<source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp>, <source-link|font.cpp|src/Graphics/Fonts/font.cpp>>The
    mathematical parameters of a font and the height-aware correction
    hooks.

    <item*|<source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>>Routing: profiles,
    mathematical italic letters, script-size alternates, and the tables read
    by the font inspector.

    <item*|<source-link|Graphics/Fonts/feature_font.cpp|src/Graphics/Fonts/feature_font.cpp>>A font seen through a
    <verbatim|GSUB> feature, and the features a document asks for.

    <item*|<source-link|Graphics/Fonts/math_font_profiles.cpp|src/Graphics/Fonts/math_font_profiles.cpp>>The table of
    profiles.

    <item*|<source-link|Graphics/Fonts/poor_rubber.cpp|src/Graphics/Fonts/poor_rubber.cpp>>The emulation of
    stretchable glyphs, now behind the table.

    <item*|<source-link|Typeset/Boxes/Composite/math_boxes.cpp|src/Typeset/Boxes/Composite/math_boxes.cpp>,
    <source-link|script_boxes.cpp|src/Typeset/Boxes/Composite/script_boxes.cpp>, <source-link|Typeset/Concat/concat_math.cpp|src/Typeset/Concat/concat_math.cpp>,
    <source-link|Typeset/Env/env_semantics.cpp|src/Typeset/Env/env_semantics.cpp>>The typesetter: radicals,
    bars and wide accents; scripts, limits and stretch stacks; delimiters,
    big operators and arrows; script sizes.

    <item*|<source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>,
    <source-link|Graphics/Fonts/font_database.cpp|src/Graphics/Fonts/font_database.cpp>>The order in which font
    files are looked for, and the merge of the shipped database into the
    local one.

    <item*|<source-link|Typeset/Boxes/Basic/font_debug_boxes.cpp|src/Typeset/Boxes/Basic/font_debug_boxes.cpp>>Glyphs
    coloured by the route they took.

    <item*|<source-link|TeXmacs/progs/fonts/fonts-opentype.scm|TeXmacs/progs/fonts/fonts-opentype.scm>,
    <source-link|font-features.scm|TeXmacs/progs/fonts/font-features.scm>, <source-link|font-debug.scm|TeXmacs/progs/fonts/font-debug.scm>,
    <source-link|font-short-menu.scm|TeXmacs/progs/fonts/font-short-menu.scm>>The profiles and the font menus, the
    features, the font inspector and the font report (relative to
    <source-link|src/|src>).
  </description-paragraphs>

  <section|Further documents>

  Three documents of the source tree accompany this chapter. They are
  written in <name|Markdown> for the developers of the branch which
  introduced the support, and are more detailed on its history and status:

  <\description-paragraphs>
    <item*|<source-link|src/src/OPENTYPEMATH.md|src/OPENTYPEMATH.md>>A summary of what is
    implemented, the build and test instructions, and a specimen of every
    profiled font.

    <item*|<source-link|src/doc/opentype-math-design.md|doc/opentype-math-design.md>>The design and status
    log: provenance, architecture, the status of each piece, the tests, the
    known defects, what is still missing, and the correspondence between the
    hand-made constructions of the typesetter and their <verbatim|MATH>
    counterparts.

    <item*|<source-link|src/doc/font-system-review.md|doc/font-system-review.md>,
    <source-link|src/doc/math-symbol-coverage.md|doc/math-symbol-coverage.md>>A review of the font system
    as a whole, and a count of the mathematical symbols which are still
    unnamed.
  </description-paragraphs>

  The support goes back to a first <verbatim|MATH> reader written in 2021
  on the <verbatim|wip-unicode-math> branch, which the <name|OSPP> 2024
  project of Ke Shi for <name|Mogan> extended; that work was ported to
  <TeXmacs>, hardened, and put to work in the typesetter, under the
  hand-tuned tables rather than in place of them.

  <section|Contents of this chapter>

  <\traverse>
    <branch|The OpenType layout tables|opentype-tables.en.tm>

    <branch|Mathematics from the MATH table|opentype-math.en.tm>

    <branch|Stretchable glyphs: variants and assemblies|opentype-stretch.en.tm>

    <branch|OpenType features in text and formulas|opentype-features.en.tm>

    <branch|Math font profiles, shipped fonts and the
    database|opentype-profiles.en.tm>

    <branch|Inspecting the font system, tests and open
    problems|opentype-tools.en.tm>
  </traverse>

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
