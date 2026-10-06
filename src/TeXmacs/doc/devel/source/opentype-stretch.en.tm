<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Stretchable glyphs: variants and assemblies>

  Delimiters, radicals, big operators, wide accents, braces and long arrows
  have to be drawn at a size which depends on what they surround. A font
  with an <name|OpenType> <verbatim|MATH> table says how: its
  <verbatim|MathVariants> subtable lists, for each such glyph, a series of
  pre-drawn variants of increasing size and, for most of them, a
  <em|glyph assembly>, a recipe which builds an arbitrarily large version
  out of parts (a top, a bottom, a middle and repeated extenders). This page
  describes how <TeXmacs> uses these data: how the typesetter names the
  stretched characters, how the rubber font of an <name|OpenType> math font
  answers those names from the table, how an assembly becomes a virtual
  glyph, and how this relates to the older emulation which every other font
  goes through. How the table is read is the subject of <hlink|the
  <name|OpenType> layout tables|opentype-tables.en.tm>, and the
  constants which place the stretched glyphs in a formula are discussed in
  <hlink|mathematics from the <verbatim|MATH> table|opentype-math.en.tm>.

  <section|The names of stretchable characters>

  The typesetter never asks a font for a delimiter of a given height
  directly. It asks for characters with a name of the form
  <verbatim|\<less\><var|head>-<var|root>-<var|n>\<gtr\>>, where
  <var|head> says what kind of character is wanted (<verbatim|left>,
  <verbatim|right>, <verbatim|mid>, <verbatim|large>, <verbatim|big>,
  <verbatim|wide> or <verbatim|rubber>), <var|root> is the name of the base
  character (<verbatim|(>, <verbatim|langle>, <verbatim|sum>,
  <verbatim|hat>, <verbatim|overbrace>, <verbatim|longrightarrow>, ...), and
  <var|n> is a size number: <verbatim|\<less\>left-(-2\<gtr\>> is the third
  size of an opening parenthesis, sizes counting from 0. Big operators are
  numbered from 1 (there is no <verbatim|\<less\>big-sum-0\<gtr\>>):
  <verbatim|\<less\>big-sum-1\<gtr\>> is the text size and
  <verbatim|\<less\>big-sum-2\<gtr\>> the display size. These names are
  stable across fonts; each font decides which glyph a size number means.

  Traditionally the typesetter found the size it needed by probing:
  <cpp|get_delimiter> and <cpp|get_wide> in
  <source-link|Typeset/Boxes/Basic/text_boxes.cpp|src/Typeset/Boxes/Basic/text_boxes.cpp> ask the font for the
  extents of size 0, 1, 2, ... until one is tall or wide enough. Fonts
  which know their sizes now short-circuit this search through two virtual
  methods of <cpp|font_rep> (<source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp>), which
  return <cpp|false> by default:

  <\explain>
    <cpp|bool get_rubber_variant (string s, SI height, string& r)>

    <cpp|bool get_wide_variant (string s, SI width, string& r)><explain-synopsis|answer
    a size search directly>
  <|explain>
    Given a name without size number, such as
    <verbatim|\<less\>left-(\<gtr\>>, return in <var|r> the name of the
    smallest variant whose advance reaches <var|height> (respectively
    <var|width>). <cpp|get_delimiter> and <cpp|get_wide> call them first and
    fall back to probing when they return <cpp|false>.
  </explain>

  The <cpp|smart_font_rep> and <cpp|feature_font_rep> wrappers forward these
  calls to the subfont which draws the character
  (<cpp|smart_font_rep::rubber_subfont>), so they reach the rubber font of
  the math font whatever lies in between.

  <section|Which rubber font a font gets>

  Every font has a <em|rubber font>, obtained with <cpp|rubber_font (base)>
  (<source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp>), which caches the result of the
  virtual method <cpp|base-\<gtr\>make_rubber_font (base)>. The method
  decides how the stretched characters of that font are drawn:

  <\description>
    <item*|<cpp|unicode_font_rep::make_rubber_font>>returns
    <cpp|rubber_unicode_font (this, ot_face)> when the font has a
    <verbatim|MATH> table; otherwise it calls the default below.

    <item*|<cpp|smart_font_rep::make_rubber_font>>returns the smart font
    itself for families given with <verbatim|mathlarge=> or
    <verbatim|mathrubber=> (they are routed character by character), the
    rubber font of the main subfont when that subfont has a
    <verbatim|MATH> table, and the default otherwise.

    <item*|<cpp|font_rep::make_rubber_font>>the default: the hand-tuned
    <cpp|rubber_stix_font> for the <name|STIX> fonts (unless the hand
    tuning has been switched off), the font itself for <verbatim|mathlarge=>
    and <verbatim|mathrubber=> families, the emulation
    <cpp|poor_rubber_font> for any other <name|Unicode> font (the global
    <cpp|has_poor_rubber> is <cpp|true>), and the font itself otherwise.
  </description>

  So a font with a <verbatim|MATH> table always goes through
  <cpp|rubber_unicode_font_rep>, and the emulation of
  <hlink|<cpp|poor_rubber.cpp>|smart-fonts-emulated.en.tm> remains what
  ordinary text fonts use.

  <section|The rubber font of an OpenType math font>

  <cpp|rubber_unicode_font_rep>
  (<source-link|Plugins/Freetype/rubber_unicode_font.cpp|src/Plugins/Freetype/rubber_unicode_font.cpp>) keeps the
  <cpp|tt_face> of the font (<cpp|math_face>, whose <cpp|math_table> is the
  parsed <verbatim|MATH> table) and seven subfonts, created on first use by
  <cpp|get_font>:

  <\description>
    <item*|0>the base font itself, which draws the pre-drawn variants under
    their glyph names <verbatim|\<less\>@<var|XXXX>\<gtr\>> (four
    hexadecimal digits of the glyph id);

    <item*|1 to 4>the legacy subfonts of fonts without a table (the base
    magnified by <math|<sqrt|1/2>>, <math|<sqrt|2>> and 2, and
    <cpp|rubber_assemble_font> for the <name|Unicode> bracket pieces);

    <item*|5>the default rubber font, <cpp|font_rep::make_rubber_font
    (base)>, for the characters the table says nothing about;

    <item*|6>a virtual font named <verbatim|opentype_virtual[<var|base>]>,
    whose definitions are the assemblies built from the table (see below).
  </description>

  The subfont and the rewritten name of a character are computed once by
  <cpp|search_font_cached> and stored in the hash maps <cpp|mapper> and
  <cpp|rewriter>. With a table present, the decision is made by
  <cpp|search_font_sub_opentype>:

  <\enumerate>
    <item><cpp|parse_variant> splits the name into head, root and size
    number; the root may itself contain dashes
    (<verbatim|\<less\>wide-var-rightarrow-2\<gtr\>>), so the size is the
    last token and the head the first. Big operators are shifted down by
    one, since their numbering starts at 1.

    <item><cpp|variant_glyph> turns the root into a glyph id. For the
    heads <verbatim|wide> and <verbatim|rubber> the static table of
    <cpp|wide_code_point> gives the code point which carries the horizontal
    variants: the combining accents (<verbatim|hat> is U+0302,
    <verbatim|vect> U+20D7), the braces U+23DC to U+23DF, and the plain
    arrows for the long ones (<verbatim|longrightarrow> is U+2192), since
    fonts stretch those and not the spacing modifier letters or the long
    arrows into which <TeXmacs> would otherwise translate the names. Other
    roots are converted from their <TeXmacs> name with
    <cpp|strict_cork_to_utf8>.

    <item>A name of a made to measure assembly (next section) is answered
    at once by <cpp|make_measured>, with subfont 6.

    <item>If the glyph has vertical or horizontal variants and the size
    number is within range, the name is rewritten to the glyph name of that
    variant and subfont 0 draws it. For the display size of a big operator
    (<verbatim|\<less\>big-<var|op>-2\<gtr\>>), the variant is instead the
    smallest one which reaches <verbatim|displayOperatorMinHeight>, capped
    at two em by <cpp|DISPLAY_OPERATOR_MAX_EM>, because some fonts declare
    absurdly large values.

    <item>Beyond the last variant, if the glyph has an assembly, every size
    up to <cpp|MAX_ASSEMBLY_REPS> (64) further sizes is defined at once in
    the virtual font, and subfont 6 draws it.

    <item>Otherwise the legacy search <cpp|search_font_sub> is tried, and
    when it has nothing either, subfont 5, the default rubber font. A
    delimiter of which the font has no larger size (<name|Fira Math> has
    none for the slashes) is thus stretched by the emulation beyond the
    base glyph of the font.
  </enumerate>

  The two size searches of the typesetter are answered in the same spirit.
  <cpp|get_rubber_variant> compares the target height, converted into design
  units, with the advances of the vertical variants which the table parser
  stored (<cpp|ver_glyph_variants_adv>) and returns the first variant which
  reaches it. Beyond the largest variant, a glyph without assembly gets the
  largest variant, and a glyph with an assembly gets a made to measure name.
  <cpp|get_wide_variant> does the same with the horizontal variants and the
  width.

  <section|Assemblies as virtual glyphs>

  An assembly is a list of <em|part records>: a glyph id, the lengths of its
  start and end connectors, its full advance, and a flag which marks the
  extenders. The specification lets consecutive parts overlap by at least
  <verbatim|minConnectorOverlap>, and by at most the shorter of the two
  connectors, and repeats the extenders as often as needed.
  <TeXmacs> does not draw assemblies with special code: it writes each size
  as a definition of the glyph algebra of <hlink|virtual
  fonts|smart-fonts-virtual.en.tm>, and lets the virtual font machinery draw
  it, on the screen as a bitmap and in <name|PDF> as the vector outlines of
  the parts. An assembly therefore exports as the font's own glyphs.

  <\description>
    <item*|<cpp|part_records>>reads the part records of the assembly and
    measures the ink of every part in the rendered font (its bottom for a
    vertical assembly, its left edge for a horizontal one). It also repairs
    parts which declare no advance: <name|KpMath> 0.35, which <TeXmacs>
    ships, declares neither advance nor connectors for the bottom part of
    its right parenthesis. Such a part gets the length of its ink and the
    shortest connector of the properly declared parts.

    <item*|<cpp|assembled_length>>computes, in design units, the length of
    an assembly whose extenders are repeated a given number of times, with
    the overlaps of the specification.

    <item*|<cpp|assemble>>writes the definition: a <verbatim|join> of the
    parts, each a leaf <verbatim|@<var|XXXX>> placed at an offset in units
    of the em. Parts are placed one by one, at the distance the table
    prescribes between the near edges of their ink, rather than glued to
    the stack built so far: many fonts (<name|KpMath>, <name|XCharter>,
    <name|STIX>, <name|New Computer Modern>, ...) draw their parts at
    origins which differ from one part to the next, and gluing measured the
    ink of the tallest part instead of the one which receives the next.
    With a target length, the overlaps are enlarged uniformly, up to the
    connector lengths, so that the assembly shrinks towards the target.
  </description>

  The numbered sizes are defined all at once, the first time a size beyond
  the variants is asked for: the first assembled size is the smallest
  number of repetitions whose length exceeds the largest pre-drawn variant,
  so that sizes keep growing with the size number, and 64 sizes are then
  defined. This matters because adding a definition invalidates the virtual
  font: <cpp|add_virtual_glyph> appends the definition to the translator
  <cpp|virt> and, if subfont 6 already exists, removes it from
  <cpp|font::instances>, <cpp|font_metric::instances> and
  <cpp|font_glyphs::instances> (all three are sized after the number of
  definitions) so that it is rebuilt.

  <paragraph|Made to measure assemblies.>Beyond the numbered sizes, the
  size searches of the typesetter return names such as
  <verbatim|\<less\>left-(-h2500\<gtr\>> (a height of 2.5 em) or
  <verbatim|\<less\>wide-overbrace-w4200\<gtr\>> (a width of 4.2 em): the
  last token is <verbatim|h> or <verbatim|w> followed by the size in
  thousandths of an em (<cpp|per_em>). <cpp|make_measured> builds the
  definition with the smallest number of repetitions which reaches the
  target and shrinks the overlaps towards it. The size is in the name, and
  in em rather than in pixels, because the screen draws with a magnified
  copy of the font, whose virtual font starts empty: the copy receives the
  name which the unmagnified font answered and must be able to rebuild the
  same glyph from it.

  <paragraph|Virtual font primitives.>Two additions to
  <source-link|Graphics/Fonts/virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp> came with this work:
  <verbatim|hor-take>, the horizontal mirror of <verbatim|ver-take>, which
  repeats a column of a glyph over a given length, in both the bitmap
  compiler (<cpp|compile_bis>) and the vector path (<cpp|draw_tree>), and an
  optional third argument of <verbatim|glue*>, an extra horizontal shift.

  <section|The emulation behind the table>

  Fonts without a table, and the characters a table does not cover, still
  go through the emulation of <source-link|Graphics/Fonts/poor_rubber.cpp|src/Graphics/Fonts/poor_rubber.cpp> and
  the <verbatim|emu-*> virtual fonts, described in <hlink|emulated
  fonts|smart-fonts-emulated.en.tm>. Two changes concern the
  <name|OpenType> path. First, <cpp|poor_rubber_font_rep> now treats a font
  with a <verbatim|MATH> table as having its own big operators, whatever its
  name (<cpp|big_flag>). Second, the slashes, which cannot be extended by
  repeating a straight middle part, keep being stretched (mostly vertically)
  in sixteen further sizes from <cpp|SLASH_BASE> on, so that a large
  <verbatim|/> is a slash and not a magnified glyph.

  In the smart font, <cpp|smart_font_rep::resolve_rubber> builds the
  stretched character from the base character of the font; a long arrow
  whose long form the font lacks stretches the plain arrow.

  <section|Symbols taken from STIX Two Math>

  An <name|OpenType> math font may lack a symbol which <TeXmacs> can
  emulate, as a construction over the glyphs the font has. Some of these
  constructions work on pixels (<verbatim|intersect>, <verbatim|exclude>,
  <verbatim|bar-*>, <verbatim|flood-fill>, <verbatim|unserif>, ...):
  <cpp|virtual_font_rep::supported (t, true)> tells which operators the
  vector path can draw, and <cpp|virtual_font_draws_vectors (fn, s)> applies
  it to one character, following an extended virtual font to the one it
  extends. A <name|PDF> export embeds such a glyph as a bitmap, in a Type 3
  font.

  To avoid that, <cpp|smart_font_rep::resolve>, when the main font has a
  <verbatim|MATH> table (<cpp|has_math_table>: the math type is
  <cpp|MATH_TYPE_OPENTYPE> or <cpp|MATH_TYPE_TEX_GYRE>) and the emulation of
  a character would not draw vectors, takes the character from
  <name|STIX Two Math> instead (<cpp|resolve_shipped_math>,
  <cpp|SHIPPED_MATH_FONT>), which is shipped with <TeXmacs>. Using a shipped
  font rather than whatever the system provides keeps the document the same
  everywhere, which is also what an emulation achieves. The emulation stays
  when <name|STIX Two Math> is not installed, when it is the main font
  itself, or when it lacks the character.

  Which emulated glyphs of a document become bitmaps is reported by the
  font report of the <hlink|font inspector|opentype-tools.en.tm>.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Plugins/Freetype/rubber_unicode_font.cpp|src/Plugins/Freetype/rubber_unicode_font.cpp>>The rubber
    font of <name|Unicode> fonts, and its <name|OpenType> path: variants,
    assemblies, made to measure sizes.

    <item*|<source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp>,
    <source-link|font.hpp|src/Graphics/Fonts/font.hpp>><cpp|rubber_font>, the default
    <cpp|make_rubber_font> and the hooks <cpp|get_rubber_variant>,
    <cpp|get_wide_variant>.

    <item*|<source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>><cpp|make_rubber_font>
    and the forwarding hooks of the smart font, <cpp|resolve_rubber>, and
    the fallback to <name|STIX Two Math> (<cpp|resolve_shipped_math>).

    <item*|<source-link|Graphics/Fonts/virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp>>The glyph algebra
    which draws the assemblies, <verbatim|hor-take>,
    <cpp|virtual_font_draws_vectors>.

    <item*|<source-link|Graphics/Fonts/poor_rubber.cpp|src/Graphics/Fonts/poor_rubber.cpp>>The emulation of
    stretched characters for fonts without a table.

    <item*|<source-link|Typeset/Boxes/Basic/text_boxes.cpp|src/Typeset/Boxes/Basic/text_boxes.cpp>><cpp|get_delimiter>
    and <cpp|get_wide>, the size searches of the typesetter.
  </description-paragraphs>

  <section|Open problems>

  <\itemize>
    <item>The comments above <cpp|get_rubber_variant> and
    <cpp|get_wide_variant> still describe the made to measure size as a
    number of pixels, while the code (<cpp|per_em>) writes thousandths of an
    em.

    <item>Assemblies are placed at the minimal overlap; only made to measure
    sizes shrink towards a target. The numbered sizes therefore grow by the
    length of one set of extenders at a time.

    <item>Six symbols which only <TeXmacs> defines (<verbatim|triangleup>,
    <verbatim|blacktriangleup> and four negated black triangles) are listed
    in <source-link|src/src/OPENTYPEMATH.md|src/OPENTYPEMATH.md> as still exporting as small
    bitmaps when the math font lacks them.

    <item>Delimiters for which the table has variants but no assembly stop
    at their largest variant (<cpp|get_rubber_variant>).
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
