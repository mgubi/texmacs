<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The OpenType layout tables>

  <name|FreeType> gives <TeXmacs> the outlines and the advance widths of the
  glyphs of a font, and the legacy <verbatim|kern> table if the font has one.
  Everything else an <name|OpenType> font knows about layout lives in its
  <em|layout tables>, which <name|FreeType> does not interpret: the
  <verbatim|MATH> table with the geometry of mathematical typesetting, the
  <verbatim|GSUB> table with the glyph substitutions of the features, and
  the <verbatim|GPOS> table with the positioning rules, among them the pair
  kerning of modern fonts. <TeXmacs> reads these three tables itself, with
  small parsers that work directly on the bytes of the font file. This page
  describes those parsers, the data structures they fill, how the results are
  cached per font file, and what is deliberately left unread. How the data
  are used is the subject of the other pages of <hlink|this
  chapter|opentype.en.tm>.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Plugins/Freetype/tt_tools.hpp|src/Plugins/Freetype/tt_tools.hpp>,
    <source-link|tt_tools.cpp|src/Plugins/Freetype/tt_tools.cpp>>The table readers. The file is older than the
    <name|OpenType> work: it also holds the low level access to the tables
    of a font file (<cpp|tt_table>), the reading of the name table and the
    glyph analysis used by the <hlink|font database|font-database.en.tm>.
    The <verbatim|MATH> reader goes back to a version written for
    <TeXmacs> in 2021 and extended for <name|Mogan>; the <verbatim|GSUB> and
    <verbatim|GPOS> readers are new.

    <item*|<source-link|Plugins/Freetype/tt_face.hpp|src/Plugins/Freetype/tt_face.hpp>,
    <source-link|tt_face.cpp|src/Plugins/Freetype/tt_face.cpp>>The face of a font file (<cpp|tt_face_rep>),
    which holds the <name|FreeType> face, the bytes of the file, and the
    parsed tables; the metrics of a face (<cpp|tt_font_metric_rep>), whose
    <cpp|kerning> method answers from <verbatim|GPOS> first.

    <item*|<source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>>The users of the
    tables: <cpp|init_ot_math>, the correction hooks, and
    <cpp|get_feature_variant> and <cpp|ot_font_features> for the features;
    see <hlink|mathematics from the MATH table|opentype-math.en.tm>.
  </description-paragraphs>

  All three readers follow the same conventions. They take the whole font
  file as a <cpp|string> and look the table up with <cpp|tt_table (buf, 0,
  tag)>, that is, in the first font of the file; a missing table gives an
  empty result, not an error. Numbers are read big endian with
  <cpp|get_U16>, <cpp|get_S16> and <cpp|get_U32>, and every offset is
  relative to the subtable which contains it, as in the specification.
  <em|Coverage tables> (a sorted list of glyph identifiers, or a list of
  ranges) are read by the static <cpp|parse_coverage_table>, which returns
  the covered glyphs in coverage order, so that the <math|i>-th entry of a
  record array belongs to the <math|i>-th covered glyph. Glyphs are always
  identified by their glyph index in the font, never by a character code.

  <section|The MATH table>

  <cpp|parse_mathtable (buf)> returns an <cpp|ot_mathtable>, a reference
  counted handle (<cpp|CONCRETE_NULL>) whose nil value means \Pno
  <verbatim|MATH> table\Q. Only version 1.0 of the table is accepted. The
  representation <cpp|ot_mathtable_rep> has one field per part of the table:

  <\description-paragraphs>
    <item*|<cpp|constants_table>>A <cpp|MathConstantsTable>: the 51 value
    records of <verbatim|MathConstants> in <cpp|records>, in the order of
    the specification, and the five plain integers
    (<cpp|scriptPercentScaleDown>, <cpp|scriptScriptPercentScaleDown>,
    <cpp|delimitedSubFormulaMinHeight>, <cpp|displayOperatorMinHeight>,
    <cpp|radicalDegreeBottomRaisePercent>) in fields of their own. The enum
    <cpp|MathConstantRecordEnum> names all 56 constants, and
    <cpp|operator[]> answers for both kinds, so that the users write
    <cpp|mc[axisHeight]> or <cpp|mc[scriptPercentScaleDown]> alike. The
    values are in design units.

    <item*|<cpp|italics_correction>, <cpp|top_accent>>The
    <verbatim|MathItalicsCorrectionInfo> and
    <verbatim|MathTopAccentAttachment> subtables of
    <verbatim|MathGlyphInfo>, as hash maps from a glyph to a
    <cpp|MathValueRecord>.

    <item*|<cpp|extended_shape_coverage>>The glyphs of the
    <verbatim|ExtendedShapeCoverage> table, whose scripts follow their
    height (tall operators, delimiters).

    <item*|<cpp|math_kern_info>>The <verbatim|MathKernInfo> records: for each
    covered glyph, up to four <cpp|MathKernTable>s, for the top right, top
    left, bottom right and bottom left corners. A kern table is a staircase:
    <cpp|heightCount> correction heights and one more kern value than
    heights.

    <item*|<cpp|ver_glyph_variants>, <cpp|hor_glyph_variants>>The vertical
    and horizontal <verbatim|MathGlyphConstruction>s: for each covered glyph
    the list of its size variants, with their advance measurements in the
    parallel maps <cpp|ver_glyph_variants_adv> and
    <cpp|hor_glyph_variants_adv>.

    <item*|<cpp|ver_glyph_assembly>, <cpp|hor_glyph_assembly>,
    <cpp|minConnectorOverlap>>The glyph assemblies: for each glyph which
    can be built from parts, a <cpp|GlyphAssembly> with its
    <cpp|GlyphPartRecord>s (glyph, start and end connector lengths, full
    advance, and the flag which marks an extender), and the overlap the
    connectors must have at least.
  </description-paragraphs>

  A <cpp|MathValueRecord> keeps its value and, when the record points to
  one, the four fields of the header of its device table
  (<cpp|hasDevice>, <cpp|deviceTable>); the delta values themselves are not
  read.

  The parser honours <em|NULL offsets>. Each of the four subtables of
  <verbatim|MathGlyphInfo> is optional, and so are the two coverage tables
  of <verbatim|MathVariants>: a font without horizontal variants, or without
  <verbatim|MathKernInfo>, has a zero offset there. The older parser added
  the offset to its parent and went on, so that it read the parent table
  itself as a coverage table; the current code tests every offset before it
  follows it (<cpp|parse_record_with_coverage>,
  <cpp|parse_math_kern_info_table> and the tests in
  <cpp|parse_mathtable>). A NULL <verbatim|GlyphAssembly> offset in a
  construction simply means that the glyph has variants but no assembly
  (<cpp|parse_construction> returns <cpp|false>).

  Three helpers of <cpp|ot_mathtable_rep> answer the questions the font
  code asks:

  <\explain>
    <cpp|bool has_kerning (unsigned int glyphID, bool top, bool
    left)><explain-synopsis|does the glyph have a kern table at this
    corner?>
  <|explain>
    True if <verbatim|MathKernInfo> covers the glyph and has a table for the
    given corner.
  </explain>

  <\explain>
    <cpp|int get_kerning (unsigned int glyphID, int height, bool top, bool
    left)><explain-synopsis|cut-in at a height>
  <|explain>
    The kern value of the step of the staircase in which <cpp|height> (in
    design units) falls: the first value below the first correction height,
    the last one above the last height, the value between two heights
    otherwise, and the single value of a table without heights. It must be
    called after <cpp|has_kerning>.
  </explain>

  <\explain>
    <cpp|unsigned int get_init_glyphID (unsigned int
    glyphID)><explain-synopsis|base glyph of a size variant>
  <|explain>
    For a glyph which is a size variant of another one, the glyph whose
    construction lists it; the glyph itself otherwise. The inverse map is
    built on the first call, from all vertical and horizontal variants. It
    lets the extended shape test answer for the large sizes of a delimiter,
    which the coverage table usually lists only by their base glyph.
  </explain>

  <cpp|dump_mathtable> prints a table, and is called when a face is loaded
  in verbose debugging mode; <cpp|parse_mathtable (url)> reads a file
  first.

  <section|Single and alternate substitutions: GSUB>

  The <verbatim|GSUB> reader extracts one feature at a time:

  <\cpp-code>
    typedef hashmap\<less\>unsigned int, array\<less\>unsigned int\<gtr\> \<gtr\> ot_gsub_map;

    ot_gsub_map \ \ parse_gsub_feature (const string& buf, string feature);

    array\<less\>string\<gtr\> parse_gsub_tags (const string& buf);
  </cpp-code>

  <cpp|parse_gsub_feature> walks the feature list, and for every feature
  record whose tag is the requested one, every lookup of that feature, and
  every subtable of the lookup, calls <cpp|parse_gsub_subtable>. The result
  maps a glyph to its substitutes: one glyph for a single substitution, the
  alternates in the order of the font for an alternate substitution. Three
  subtable kinds are read: single substitution formats 1 (a delta added to
  the glyph index, modulo <math|2<rsup|16>>) and 2 (an explicit list), and
  alternate substitution format 1; an extension lookup (type 7) is followed
  to its real type with its 32 bit offset. Any other lookup type is skipped
  before its coverage table is read, since its fields mean something else.
  When several lookups or several feature records substitute the same glyph,
  the first one wins.

  <cpp|parse_gsub_tags> lists the feature tags of the table, without
  repetitions; it is what lets a menu offer only the features a font has
  (<cpp|ot_font_features> in <source-link|unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>).

  Two simplifications are worth knowing. The reader ignores the script and
  language system lists: a feature counts if any feature record with its tag
  exists, whatever script it is registered for. And it reads only the
  substitutions which replace one glyph by one glyph: ligatures (type 4),
  multiple substitutions (type 2) and the contextual and chaining lookups
  (types 5, 6 and 8) are not supported. The features which this is enough
  for, in text and in formulas, are described in <hlink|OpenType features in
  text and formulas|opentype-features.en.tm>.

  <section|Pair kerning: GPOS>

  Modern <name|OpenType> fonts keep their kerning in the <verbatim|kern>
  feature of <verbatim|GPOS>; the legacy <verbatim|kern> table, the only one
  <name|FreeType> exposes, is usually absent, and no <name|OpenType> math font
  ships it. <cpp|parse_gpos_kern (buf)> reads the pair adjustment lookups
  (type 2, or an extension lookup of type 9 which points to one) of every
  feature record with the tag <verbatim|kern>, and returns an
  <cpp|ot_gpos_kern> handle whose representation has two parts:

  <\description>
    <item*|<cpp|pairs>>The explicit pairs of the format 1 subtables, keyed
    by <cpp|(left \<less\>\<less\> 16) \| right>.

    <item*|<cpp|classes>>One <cpp|ot_kern_classes> per format 2 subtable:
    the coverage of the first glyph, the two class definitions, and the
    matrix of values, of size <cpp|class1_count> times
    <cpp|class2_count>. Class 0, the default, is not stored in the class
    maps, so a glyph absent from a map is in class 0.
  </description>

  Only the horizontal advance of the <em|first> glyph of a pair is taken
  (<cpp|x_advance_offset> of the first value format), which is where
  horizontal kerning lives; a subtable whose first value record has no
  advance is skipped, and the value record of the second glyph is only used
  to compute the record size. <cpp|ot_gpos_kern_rep::get (left, right)>
  looks the pair up in <cpp|pairs> first, then in the class matrices in the
  order of the font, and answers 0 for a pair which is not kerned. As for
  <verbatim|GSUB>, scripts and languages are ignored.

  <section|Caching in the face>

  A <cpp|tt_face_rep> is a resource, shared by all the fonts made from the
  same file at all sizes. Its constructor reads the whole file into
  <cpp|buffer> (it already did, to give it to <name|FreeType> as a memory
  face), and parses the <verbatim|MATH> table at once into
  <cpp|math_table>: the math path needs it as soon as a font is made, and a
  face without the table pays for one table lookup only. The two other
  tables are parsed on first use and kept:

  <\description>
    <item*|<cpp|gsub_feature (tag)>>The substitutions of one feature, cached
    per tag in <cpp|gsub_features>; a reference into the cache is returned.

    <item*|<cpp|gsub_tags ()>>The feature list, cached in
    <cpp|gsub_tag_list>.

    <item*|<cpp|gpos_kern ()>>The pair kerning, cached in
    <cpp|gpos_kern_table>.
  </description>

  Since the cache lives in the face, it lives as long as the face resource,
  that is, for the whole session.

  <section|How kerning reaches the text>

  The text of a document is measured and drawn by <cpp|unicode_font_rep>
  (<cpp|get_extents>, <cpp|get_xpositions>, <cpp|draw_fixed>), which adds
  <cpp|fnm-\<gtr\>kerning (pc, uc)> between consecutive characters of a
  string. <cpp|tt_font_metric_rep::kerning> converts the two character codes
  to glyph indices and asks the face for its <verbatim|GPOS> kerning; when
  the font has a non empty <cpp|ot_gpos_kern>, the answer, in design units,
  is scaled with the horizontal scale of the <name|FreeType> size and the
  legacy table is not consulted at all, even for a pair <verbatim|GPOS> does
  not kern. Only fonts without <verbatim|GPOS> kerning fall back on
  <cpp|FT_Get_Kerning>. Kerning thus applies inside one string, that is,
  inside one text box; two letters which end up in different boxes, as the
  single letters of a formula do, are not kerned against each other. The
  kerning of a script against its base is a different mechanism, the cut-in
  of <verbatim|MathKernInfo>, described in <hlink|mathematics from the MATH
  table|opentype-math.en.tm>.

  <section|What is not read>

  <\itemize>
    <item>The rest of <verbatim|GPOS>: single adjustments, cursive
    attachment, mark to base and mark to mark positioning
    (<verbatim|mark>, <verbatim|mkmk>), and the contextual positioning
    lookups. Fonts which place combining accents with <verbatim|mark>
    rather than with the top accent attachment of <verbatim|MATH> are
    therefore positioned by the generic accent code.

    <item>The ligature, multiple and contextual substitutions of
    <verbatim|GSUB> (see above). Standard ligatures of text are still made,
    but by the older mechanism of <cpp|unicode_font_rep>, which looks for
    the precomposed ligature characters U+FB00 to U+FB05 in the font.

    <item>The script and language systems of <verbatim|GSUB> and
    <verbatim|GPOS>.

    <item>The delta values of device tables, whose headers are kept but
    which no code applies; they would only matter for small sizes on screen.

    <item>The italic correction of a glyph assembly
    (<cpp|GlyphAssembly::italicsCorrection> is read and ignored).

    <item>Only the first font of a collection is examined
    (<cpp|tt_table (buf, 0, ...)>).
  </itemize>

  <section|Pitfalls>

  <\itemize>
    <item>All lengths in the parsed tables are in design units. Convert them
    with the factors of the font which uses them
    (<cpp|design_unit_to_metric> for vertical lengths,
    <cpp|design_unit_to_metric_x> for horizontal ones, both based on
    <verbatim|units_per_EM>), never with a constant.

    <item>The readers trust the font: offsets are not checked against the
    length of the table. A damaged font can make them read past the end of
    the table string.

    <item>A glyph index of 0 is the missing glyph. <cpp|get_glyphID> in
    <source-link|unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp> returns 0 for a character the font does not
    have, and the lookups then find nothing, which is the intended answer.

    <item>Substituted glyphs have no character code. They are named
    <verbatim|\<less\>@XXXX\<gtr\>> (the glyph index in four hexadecimal
    digits), a name which <cpp|unicode_font_rep> draws directly from the
    glyph index; see <hlink|stretchable glyphs: variants and
    assemblies|opentype-stretch.en.tm>.
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
