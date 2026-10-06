<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|OpenType features in text and formulas>

  An <name|OpenType> font may carry several shapes of the same character
  and say, in its <verbatim|GSUB> table, under which <em|feature> each of
  them is to be used: old style figures under <verbatim|onum>, small
  capitals under <verbatim|smcp>, dotless letters under <verbatim|dtls>,
  script-size alternates under <verbatim|ssty>. Its <verbatim|GPOS> table
  holds the kerning between pairs of glyphs. This page describes how
  <TeXmacs> uses these tables: the features which a document asks for
  through the environment variable <src-var|font-features>, the three
  features which the mathematics uses by itself, and the pair kerning of
  text. How the features are chosen in the menus and in the font browser,
  from the point of view of a user, is explained in <hlink|selecting
  fonts|../fonts/font-selection.en.tm>; how the tables are parsed, in
  <hlink|the <name|OpenType> layout tables|opentype-tables.en.tm>.

  <section|Substitutions in the font>

  Every feature goes through one virtual method of <cpp|font_rep>:

  <\explain>
    <cpp|bool get_feature_variant (string s, string feature, int alt,
    string& r)><explain-synopsis|the substitute of a glyph>
  <|explain>
    If the font has a substitute for the character <var|s> under the
    four-letter <var|feature>, return in <var|r> the name of the
    <var|alt>-th one, counting from 0, as a glyph name
    <verbatim|\<less\>@<var|XXXX>\<gtr\>>. The default returns
    <cpp|false>.
  </explain>

  <cpp|unicode_font_rep::get_feature_variant>
  (<source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>) implements it: it looks
  up the glyph id of <var|s>, asks the face for the map of the feature
  (<cpp|tt_face_rep::gsub_feature>, which parses it on first use and caches
  it per tag) and returns the requested alternate if there is one. A single
  substitution has one alternate, an alternate substitution several. The
  smart font forwards the call to the subfont which draws the character,
  so the substitute comes from the font which really draws it.

  <cpp|ot_font_features (name)>, exported to <scheme> as
  <scm|font-available-features>, returns the feature tags which a font file
  offers (<cpp|tt_face_rep::gsub_tags>), so that the menus can propose only
  what the font can do.

  <section|A font seen through a feature>

  Most callers do not want one glyph but a whole font in which every glyph
  is replaced by its substitute. This is the decorator
  <cpp|feature_font_rep> of <source-link|Graphics/Fonts/feature_font.cpp|src/Graphics/Fonts/feature_font.cpp>:

  <\explain>
    <cpp|font feature_font (font base, string feature, int alt)><explain-synopsis|a
    font decorator>
  <|explain>
    A font named <verbatim|<var|base>#<var|feature><var|alt>> which
    rewrites every string character by character, replacing each character
    by its substitute in <var|base> when there is one
    (<cpp|feature_font_rep::rewrite>, with a cache per character), and then
    delegates to <var|base>. Positions are mapped back from the rewritten
    string to the original one, so cursor positions, extents and the
    metric hooks (slopes, italic corrections, the cut-in kerns, the top
    accent attachment) refer to the characters of the document.
    <cpp|magnify> wraps the magnified base again, and the rubber font is
    the one of the base.
  </explain>

  Because the substitution happens when the glyph is measured and drawn,
  the document is untouched: it still holds the characters which were
  typed, and copying, searching, spell checking and exporting see them.

  <section|The variable font-features>

  The value of <src-var|font-features> (default empty,
  <source-link|Typeset/Env/env_default.cpp|src/Typeset/Env/env_default.cpp>) is a comma separated list of
  feature tags, each optionally followed by <verbatim|=<var|n>> to select
  the <var|n>-th alternate. <cpp|edit_env_rep::update_font>
  (<source-link|Typeset/Env/env_semantics.cpp|src/Typeset/Env/env_semantics.cpp>) applies it with

  <\explain>
    <cpp|font apply_features (font fn, string features)><explain-synopsis|the
    features a document asks for>
  <|explain>
    Wrap <var|fn> in one <cpp|feature_font> per tag, from left to right, so
    that a later tag sees the glyphs which the earlier ones chose:
    <verbatim|onum,tnum> takes old style figures and then their tabular
    form. Items whose tag is not four characters long are ignored, and a
    font which lacks a feature leaves its glyphs alone, so the variable is
    safe on a whole document.
  </explain>

  The order of the decorators in <cpp|update_font> is: the font of the
  current size, then the <verbatim|ssty> decorator of the mathematics (see
  below), then the features of <src-var|font-features>, then the effects of
  <src-var|font-effects>.

  On the <scheme> side, <source-link|progs/fonts/font-features.scm|TeXmacs/progs/fonts/font-features.scm> holds the
  table of the features which the menus propose
  (<scm|font-feature-table>: the figure styles, <verbatim|zero>, the
  capital forms, <verbatim|hist>, <verbatim|swsh>, <verbatim|salt>,
  <verbatim|calt> and the stylistic sets <verbatim|ss01> to
  <verbatim|ss05>), and the menus <scm|text-font-features-menu>
  (<menu|Format|Font features>) and <scm|document-font-features-menu>
  (<menu|Document|Font|Features>), which list the proposed features of the
  font at the cursor (<scm|font-features-here>, through
  <scm|font-logical-search>) with a check mark for those in force.
  <scm|font-features-toggle> sets the variable on the selection with
  <scm|make-with>, <scm|font-features-toggle-global> in the initial
  environment; turning on one figure style or spacing turns off its
  opposite (<verbatim|onum> and <verbatim|lnum>, <verbatim|tnum> and
  <verbatim|pnum>). The font browser
  (<source-link|progs/fonts/font-new-widgets.scm|TeXmacs/progs/fonts/font-new-widgets.scm>) has a column and a tab of
  toggles, <scm|font-features-selector>, which lists the features of the
  font the dialog has selected (<scm|selector-font-features-available>,
  through the logical font of the sample text) and keeps the choice in the
  selector under the key <scm|:features>.

  <section|Features used by the mathematics>

  Three features are applied by the typesetter itself, for fonts with a
  <verbatim|MATH> table. They are what the specification of the table
  expects, and they are not under the control of <src-var|font-features>.

  <\description>
    <item*|<verbatim|ssty>, script-size alternates>Many math fonts draw
    special shapes for the first and second script levels, with thicker
    strokes and wider spacing. In <cpp|edit_env_rep::update_font>, at index
    levels above 0, a font of math type <cpp|MATH_TYPE_OPENTYPE> (an untuned
    <name|OpenType> math font, not one of the hand-tuned <name|TeX Gyre>
    fonts) is wrapped in <cpp|feature_font (fn, "ssty", min (index_level, 2)
    - 1)>: alternate 0 at the first script level, alternate 1 at the second
    and deeper ones. This happens in the same branch which takes the script
    sizes from <verbatim|scriptPercentScaleDown> and
    <verbatim|scriptScriptPercentScaleDown>, that is, only when the document
    keeps the default <src-var|math-font-sizes>.

    <item*|<verbatim|dtls>, dotless letters>An accent over <math|i> or
    <math|j> should sit on a dotless letter. <cpp|concater_rep::typeset_wide>
    (<source-link|Typeset/Concat/concat_math.cpp|src/Typeset/Concat/concat_math.cpp>) replaces an atomic body
    <verbatim|i> or <verbatim|j> of an accent above by its <verbatim|dtls>
    substitute when the font has a table and offers one.

    <item*|<verbatim|flac>, flattened accents>Over a base taller than
    <verbatim|FlattenedAccentBaseHeight>, a narrow accent (one which the
    font does not stretch) is replaced by its <verbatim|flac> substitute,
    in the <name|OpenType> branch of the accent construction of
    <source-link|Typeset/Boxes/Composite/math_boxes.cpp|src/Typeset/Boxes/Composite/math_boxes.cpp>.
  </description>

  The letters of a formula are another substitution, but not a
  <verbatim|GSUB> one: for an untuned math font, the smart font takes them
  from the mathematical italic alphabet of the font
  (<cpp|REWRITE_MATH_ITALIC>, see <hlink|mathematics from the
  <verbatim|MATH> table|opentype-math.en.tm>).

  <section|Pair kerning>

  The kerning of text is computed by the font metric of the face,
  <cpp|tt_font_metric_rep::kerning> (<source-link|Plugins/Freetype/tt_face.cpp|src/Plugins/Freetype/tt_face.cpp>),
  which <cpp|unicode_font_rep::get_xpositions> and the drawing routines add
  between consecutive glyphs. <name|FreeType> only exposes the legacy
  <verbatim|kern> table, which no <name|OpenType> math font and few recent
  text fonts ship: before this work, every such font was set without any
  kerning. <cpp|kerning> now first asks the face for the pair kerning of the
  <verbatim|GPOS> feature <verbatim|kern> (<cpp|tt_face_rep::gpos_kern>,
  parsed once by <cpp|parse_gpos_kern>), both explicit pairs and class
  matrices, and only falls back to <name|FreeType> when the font has no
  such data. Only the horizontal advance of the value records is used.

  Kerning applies between the glyphs of one string drawn by one font, so it
  is not applied across a change of font, nor between a glyph and the
  substitute which a feature font put next to it in another font.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Fonts/feature_font.cpp|src/Graphics/Fonts/feature_font.cpp>><cpp|feature_font>
    and <cpp|apply_features>.

    <item*|<source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>><cpp|get_feature_variant>
    and <cpp|ot_font_features>.

    <item*|<source-link|Plugins/Freetype/tt_face.cpp|src/Plugins/Freetype/tt_face.cpp>, <source-link|tt_tools.cpp|src/Plugins/Freetype/tt_tools.cpp>>The
    cached feature maps and the kerning, and the <verbatim|GSUB> and
    <verbatim|GPOS> readers.

    <item*|<source-link|Typeset/Env/env_semantics.cpp|src/Typeset/Env/env_semantics.cpp>><cpp|update_font>:
    script sizes, <verbatim|ssty>, <src-var|font-features>.

    <item*|<source-link|Typeset/Concat/concat_math.cpp|src/Typeset/Concat/concat_math.cpp>,
    <source-link|Typeset/Boxes/Composite/math_boxes.cpp|src/Typeset/Boxes/Composite/math_boxes.cpp>><verbatim|dtls> and
    <verbatim|flac>.

    <item*|<source-link|progs/fonts/font-features.scm|TeXmacs/progs/fonts/font-features.scm>,
    <source-link|progs/fonts/font-new-widgets.scm|TeXmacs/progs/fonts/font-new-widgets.scm>>The menus and the font
    browser.
  </description-paragraphs>

  <section|Limitations and open problems>

  <\itemize>
    <item>Only single (lookup type 1) and alternate (type 3) substitutions
    are read, also when wrapped in an extension lookup (type 7). Multiple
    substitutions, ligatures (<verbatim|liga>, <verbatim|dlig>,
    <verbatim|frac>) and contextual lookups are ignored. A feature such as
    <verbatim|calt>, which the menus propose, usually works through
    contextual lookups and then has no effect.

    <item>The feature is looked up in every script and language system of
    the table at once: the parser walks all feature records with the
    requested tag, and the first substitute found for a glyph wins. A font
    which substitutes differently by language gets an arbitrary one.

    <item>Of <verbatim|GPOS>, only the horizontal advances of pair
    positioning are used. Explicit pairs take precedence over class
    matrices whatever the order of the subtables, and a later explicit pair
    overrides an earlier one, whereas the specification lets the first
    subtable which covers a pair decide. Mark attachment, cursive and
    contextual positioning are not used, which matters for fonts which place
    accents with <verbatim|mark>/<verbatim|mkmk> rather than with the top
    accent attachment of the <verbatim|MATH> table.

    <item>When a font has <verbatim|GPOS> kerning, the legacy
    <verbatim|kern> table is no longer consulted at all, even for pairs the
    <verbatim|GPOS> data do not mention.
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
