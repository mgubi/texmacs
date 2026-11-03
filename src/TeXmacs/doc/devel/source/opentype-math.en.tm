<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Mathematics from the MATH table>

  A font with an <name|OpenType> <verbatim|MATH> table says how formulas
  should be laid out in it: where the axis is, how far a fraction moves its
  numerator up, how close a subscript may come to a superscript, where an
  accent attaches to a letter, how much a script may cut into the corner of
  its base. This page describes how <TeXmacs> turns those data into the
  parameters of its fonts, and where the typesetter uses them. The parsing
  of the table is described in <hlink|the OpenType layout
  tables|opentype-tables.en.tm>, the stretchable glyphs (size variants and
  assemblies) in <hlink|stretchable glyphs: variants and
  assemblies|opentype-stretch.en.tm>, and the way a document arrives at
  such a font in <hlink|math font profiles, shipped fonts and the
  database|opentype-profiles.en.tm>. For the user's point of view, see
  <hlink|how mathematical fonts
  work|../../main/math/fonts/man-math-fonts-intro.en.tm>.

  The design rule is that <em|the hand-tuned tables win>. <TeXmacs> had
  per-font corrections long before it read <verbatim|MATH> tables: the
  <verbatim|adjust_*.cpp> tables and the <name|STIX>, <name|TeX Gyre>,
  <name|Linux Libertine>, <name|Linux Biolinum>, <name|Fira Sans> and
  <name|Papyrus> branches of <source-link|unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>, tuned against the
  layout of <TeXmacs> itself. The table is layered <em|under> them: it
  fills what they leave open, and they keep precedence wherever they say
  something.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp>,
    <source-link|font.cpp|src/Graphics/Fonts/font.cpp>>The parameters of <cpp|font_rep>, the virtual hooks
    with their neutral defaults, <cpp|copy_math_pars>, and the switch
    <cpp|hand_tuned_math_fonts>.

    <item*|<source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>>Activation
    (<cpp|init_ot_math>), the conversion from design units, and the
    implementations of the hooks for fonts with a table.

    <item*|<source-link|Typeset/Boxes/Composite/script_boxes.cpp|src/Typeset/Boxes/Composite/script_boxes.cpp>>Scripts and
    limits.

    <item*|<source-link|Typeset/Boxes/Composite/math_boxes.cpp|src/Typeset/Boxes/Composite/math_boxes.cpp>>Fractions,
    radicals, wide accents, over- and underlines.

    <item*|<source-link|Typeset/Concat/concat_math.cpp|src/Typeset/Concat/concat_math.cpp>>Radicals and
    fractions at the level of the concatenator, long arrows with labels,
    accents over dotless letters, negated relations.

    <item*|<source-link|Typeset/Env/env_semantics.cpp|src/Typeset/Env/env_semantics.cpp>>Script sizes and
    script alternates.

    <item*|<source-link|Typeset/Boxes/Basic/text_boxes.cpp|src/Typeset/Boxes/Basic/text_boxes.cpp>, the composite and
    modifier boxes>The box side of the hooks.
  </description-paragraphs>

  <section|Activation>

  A <cpp|unicode_font_rep> is made for one font file at one size and
  resolution. Its constructor creates the shared face of the file
  (<cpp|tt_face (family)>), and if the face carries a <verbatim|MATH> table
  it calls <cpp|init_ot_math> <em|before> it looks at the family name. The
  order of the constructor is therefore:

  <\enumerate>
    <item>the generic metrics of every Unicode font (x-height, axis, script
    positions guessed from the glyphs, rule widths);

    <item><cpp|init_ot_math> when the font has a table, which sets
    <cpp|ot_math> and overwrites or fills the parameters below;

    <item>the per-family branches, guarded by <cpp|tuned= hand_tuned_math_fonts
    \|\| !has_ot>: a tuned font installs its correction tables
    (<cpp|lsub_correct>, <cpp|rsup_correct>, <cpp|above_correct>, ...) and
    global corrections;

    <item>only if no branch matched and the font has a table,
    <cpp|math_type= MATH_TYPE_OPENTYPE>.
  </enumerate>

  Two different flags thus come out of the constructor, and the code tests
  one or the other on purpose:

  <\description>
    <item*|<cpp|ot_math>>The font has a <verbatim|MATH> table and its
    parameters were loaded. This is true for the tuned <name|TeX Gyre> math
    fonts too.

    <item*|<cpp|math_type == MATH_TYPE_OPENTYPE>>The font has a table and
    no hand-tuned branch claimed it. Glyph-level data which the tuned tables
    already cover, italic corrections and cut-ins, are only taken from the
    table for such fonts; the tuned fonts keep <cpp|MATH_TYPE_STIX>,
    <cpp|MATH_TYPE_TEX_GYRE> or <cpp|MATH_TYPE_NORMAL>.
  </description>

  The switch <cpp|hand_tuned_math_fonts> (<source-link|font.cpp|src/Graphics/Fonts/font.cpp>, default
  true) turns the tuned branches off for fonts which have a table, so that
  the table-only result can be compared with the tuned one. It is set from
  the preference <verbatim|hand tuned math fonts> (<scm|notify-hand-tuned-math-fonts>
  in <source-link|tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>, which calls
  <scm|set-hand-tuned-math-fonts>), and the sample scripts of
  <source-link|tests/opentype|tests/opentype> turn it off with <verbatim|TM_HAND_TUNED=off>.
  The same switch also decides whether the <name|STIX> rubber font is used
  (<cpp|font_rep::make_rubber_font>) and the <name|STIX> special cases of
  <source-link|concat_math.cpp|src/Typeset/Concat/concat_math.cpp>. Fonts are cached, so the switch only affects
  fonts made after it was changed.

  <section|The parameters>

  <cpp|init_ot_math> converts the constants with
  <cpp|design_unit_to_metric> (vertical lengths) and
  <cpp|design_unit_to_metric_x> (horizontal lengths), whose factors
  <cpp|init_design_unit_factor> computes from <verbatim|units_per_EM>, the
  size and the resolution of the font. It writes two kinds of fields of
  <cpp|font_rep>.

  <paragraph|Older fields, overwritten.>Fields which every font has, and
  which the typesetter used before:

  <\description-paragraphs>
    <item*|<cpp|yfrac>>The axis, from <verbatim|axisHeight>, if positive.

    <item*|<cpp|wline>>The default rule width, from
    <verbatim|fractionRuleThickness>, if positive.

    <item*|<cpp|ysub_lo_base>, <cpp|ysub_hi_lim>, <cpp|ysup_lo_lim>,
    <cpp|ysup_lo_base>, <cpp|ysup_hi_lim>, <cpp|yshift>>The script
    positions, from <verbatim|subscriptShiftDown>,
    <verbatim|subscriptTopMax>, <verbatim|superscriptBottomMin>,
    <verbatim|superscriptShiftUp> and the cramped shift; only when both
    shifts are positive. <cpp|ysup_hi_lim> has no counterpart in the table
    and becomes the larger of the superscript shift and the x-height.
  </description-paragraphs>

  <paragraph|New fields.>About forty fields added to <cpp|font_rep> for
  this purpose (<source-link|font.hpp|src/Graphics/Fonts/font.hpp>), all zero for a font without table:

  <\description-paragraphs>
    <item*|Fractions><cpp|frac_rule_thickness>, <cpp|frac_num_shift_up>,
    <cpp|frac_num_disp_shift_up>, <cpp|frac_num_gap_min>,
    <cpp|frac_num_disp_gap_min>, <cpp|frac_denom_shift_down>,
    <cpp|frac_denom_disp_shift_down>, <cpp|frac_denom_gap_min>,
    <cpp|frac_denom_disp_gap_min>.

    <item*|Radicals><cpp|sqrt_ver_gap>, <cpp|sqrt_ver_disp_gap>,
    <cpp|sqrt_rule_thickness>, <cpp|sqrt_extra_ascender>,
    <cpp|sqrt_degree_rise_percent>, <cpp|sqrt_kern_before_degree>,
    <cpp|sqrt_kern_after_degree>.

    <item*|Limits and stretch stacks><cpp|upper_limit_gap_min>,
    <cpp|upper_limit_baseline_rise_min>, <cpp|lower_limit_gap_min>,
    <cpp|lower_limit_baseline_drop_min>,
    <cpp|stretch_stack_top_shift_up>,
    <cpp|stretch_stack_bottom_shift_down>,
    <cpp|stretch_stack_gap_above_min>, <cpp|stretch_stack_gap_below_min>.

    <item*|Scripts><cpp|sub_sup_gap_min>, <cpp|sup_drop_max>,
    <cpp|sub_drop_min>, <cpp|sup_bottom_max_with_sub>,
    <cpp|space_after_script>, <cpp|script_percent>,
    <cpp|script_script_percent>.

    <item*|Accents and bars><cpp|accent_base_height>,
    <cpp|flattened_accent_base_height>, <cpp|overbar_vertical_gap>,
    <cpp|overbar_rule_thickness>, <cpp|overbar_extra_ascender>,
    <cpp|underbar_vertical_gap>, <cpp|underbar_rule_thickness>,
    <cpp|underbar_extra_descender>.
  </description-paragraphs>

  The comments in <source-link|font.hpp|src/Graphics/Fonts/font.hpp> name the constant each field comes
  from. <cpp|copy_math_pars> copies all of them, and <cpp|ot_math>, so that
  a font made from another one (a smart font from its main font, a
  magnified or emulated font from its base) inherits them. The typesetter
  sees the smart font of the current mode (<cpp|env-\<gtr\>fn>), whose
  parameters come from its main subfont or, for mathematics, from the
  subfont that draws the letters.

  <section|Glyph-level data and the box hooks>

  The data which belong to one glyph are reached through virtual hooks of
  <cpp|font_rep>, whose defaults in <source-link|font.cpp|src/Graphics/Fonts/font.cpp> answer
  \Punknown\Q, and are relayed by virtual methods of the boxes, so that a
  composite box answers for the glyph that represents it.

  <\description-paragraphs>
    <item*|<cpp|get_lsub_correction_at (s, h)>, <cpp|get_lsup_correction_at>,
    <cpp|get_rsub_correction_at>, <cpp|get_rsup_correction_at>>The script
    corrections at a height <cpp|h>: the height, relative to the baseline
    of <cpp|s>, of the edge of the script which faces <cpp|s> (the bottom of
    a superscript, the top of a subscript). The defaults ignore <cpp|h> and
    return the older corrections without height. For
    <cpp|MATH_TYPE_OPENTYPE>, <cpp|unicode_font_rep> answers from the
    <verbatim|MathKernInfo> cut-in at that height (<cpp|get_ot_kerning>,
    which converts the height to design units and the kern back), plus,
    on the right side, the italic correction of the last glyph
    (<cpp|get_ot_italic_correction>): all of it for a superscript, none for
    a subscript, except on integral signs (<cpp|is_ot_integral>), which
    take two fifths of it above and lose three fifths below. The
    corrections without height call the <cpp|_at> versions at the bottom,
    respectively the top, of the font.

    <item*|<cpp|get_right_correction (s)>>For <cpp|MATH_TYPE_OPENTYPE>, the
    italic correction of the table when the glyph has one.

    <item*|<cpp|is_extended_shape (s)>>Whether the glyph, or the glyph
    whose size variant it is (<cpp|get_init_glyphID>), is in
    <verbatim|ExtendedShapeCoverage>. Answered whenever <cpp|ot_math> is
    set.

    <item*|<cpp|get_top_accent (s, x)>>The top accent attachment of the
    glyph, when the table has one; answered whenever <cpp|ot_math> is set.

    <item*|<cpp|get_feature_variant (s, feature, alt, r)>>The substitute of
    the glyph under a <verbatim|GSUB> feature, as a native glyph name; see
    <hlink|OpenType features in text and formulas|opentype-features.en.tm>.
  </description-paragraphs>

  On the box side, <source-link|boxes.hpp|src/Typeset/boxes.hpp> adds <cpp|lsub_correction_at>,
  <cpp|lsup_correction_at>, <cpp|rsub_correction_at>,
  <cpp|rsup_correction_at>, <cpp|extended_shape> and <cpp|top_accent>.
  A text box forwards them to its font, but only a text box of exactly one
  character can be an ordinary glyph: a longer string answers true to
  <cpp|extended_shape> and false to <cpp|top_accent>. Concatenations
  forward the left corrections to their first box and the right ones to
  their last box; change and modifier boxes forward to their body, and
  the box of a wide accent to its base, shifting the attachment point by
  the position of the base.

  <section|The typesetter>

  Each construction tests for the data it needs, using a parameter which is
  only non zero when it was read from a table, and falls back on its older
  code otherwise.

  <subsection|Scripts and limits>

  <cpp|script_box_rep> (<source-link|script_boxes.cpp|src/Typeset/Boxes/Composite/script_boxes.cpp>) places scripts with
  <cpp|ot_script_shifts> when <cpp|fn-\<gtr\>ot_math> and
  <cpp|sub_sup_gap_min \<gtr\> 0>. A superscript starts at
  <cpp|sup_lo_base> of the base; over an extended shape (or a composite
  box) it may not drop more than <cpp|sup_drop_max> below the top of the
  base; its bottom stays above <cpp|ysup_lo_lim>. A subscript is placed
  symmetrically with <cpp|sub_drop_min> and <cpp|ysub_hi_lim>. When both
  are present and the gap between them is less than
  <cpp|sub_sup_gap_min>, the subscript moves down, and the superscript up
  by as much of the difference as keeps its bottom under
  <cpp|sup_bottom_max_with_sub>, as in <TeX>. The horizontal corrections are
  then evaluated with the <cpp|_at> methods, at the height of the edge of
  the script that faces the base and of the edge of the base that faces
  the script, which is what the cut-in kerning of the specification asks
  for, and <cpp|space_after_script> is added after right scripts.

  <cpp|lim_box_rep> uses the limit constants when the font has positive
  <cpp|lower_limit_gap_min> and <cpp|lower_limit_baseline_drop_min>: the
  gap between the operator and its limits, and the minimal rise and drop of
  their baselines. A <em|stretched> base, flagged by the new argument of
  <cpp|limit_box>, takes the stretch stack constants instead, which keep a
  label closer to a long arrow or a wide brace than a limit to an operator;
  <cpp|typeset_long_arrow> in <source-link|concat_math.cpp|src/Typeset/Concat/concat_math.cpp> passes it, and
  uses <cpp|wide_box_covering>, which keeps trying the next horizontal
  variant until the arrow is at least as wide as its labels.

  <subsection|Fractions>

  <cpp|frac_box_rep> takes a new argument <cpp|disp> (the display style of
  the environment) and, when <cpp|frac_num_gap_min \<gtr\> 0>, sets the rule
  to <cpp|frac_rule_thickness> and places the numerator at the larger of
  its shift and the minimal gap above the rule, the denominator at the
  smaller of its shift and the minimal gap below, with the display style
  constants in display style.

  <subsection|Radicals>

  At the level of the concatenator, the radical sign is asked to cover the
  body plus <cpp|sqrt_ver_gap>, or <cpp|sqrt_ver_disp_gap> in display style,
  when that constant is positive. In <cpp|sqrt_box_rep>, with
  <cpp|sqrt_degree_rise_percent \<gtr\> 0>, the rule has the thickness
  <cpp|sqrt_rule_thickness> and is drawn flush with the top of the sign
  (half a thickness below it), the box gets <cpp|sqrt_extra_ascender>
  above, the degree is raised by <verbatim|radicalDegreeBottomRaisePercent>
  of the height of the sign and tucked into it by
  <cpp|sqrt_kern_after_degree> (a negative value), and
  <cpp|sqrt_kern_before_degree> keeps it clear of what precedes.

  <subsection|Accents, over- and underlines>

  In <cpp|wide_box_rep> (<source-link|math_boxes.cpp|src/Typeset/Boxes/Composite/math_boxes.cpp>), fonts with
  <cpp|ot_math> which are neither <name|TeX Gyre> nor <name|STIX> by math
  type get three new paths:

  <\itemize>
    <item>a <verbatim|bar> over or under a base is a rule with the
    thickness, gap and extra ascender or descender of the table, the gap
    measured from the ink of the base;

    <item>a wide accent is asked from the font with <cpp|get_wide_variant>,
    and drawn from the variant or assembly it returns (see
    <hlink|stretchable glyphs|opentype-stretch.en.tm>);

    <item>for <cpp|MATH_TYPE_OPENTYPE>, a narrow accent is the accent glyph,
    or its <verbatim|flac> substitute over a base taller than
    <cpp|flattened_accent_base_height>, raised by the excess of the base
    over <cpp|accent_base_height>, and attached at the top accent
    attachment points of the base and of the accent when both are known.
  </itemize>

  The base of an accent which is a single <verbatim|i> or <verbatim|j> is
  replaced by its <verbatim|dtls> substitute when the font has one
  (<source-link|concat_math.cpp|src/Typeset/Concat/concat_math.cpp>).

  <subsection|Delimiters, big operators and negations>

  The size of a delimiter, a radical sign or a big operator is chosen by
  the rubber font of the math font, which knows the variants and the
  assemblies of the table: <cpp|get_rubber_variant> and
  <cpp|get_wide_variant> are asked first by the generic code which picks
  a size. A display operator is the smallest variant at least
  <verbatim|displayOperatorMinHeight> tall, capped at two em
  (<cpp|DISPLAY_OPERATOR_MAX_EM> in <source-link|rubber_unicode_font.cpp|src/Plugins/Freetype/rubber_unicode_font.cpp>)
  because some fonts declare much larger values. The details are in
  <hlink|stretchable glyphs: variants and
  assemblies|opentype-stretch.en.tm>.

  For <cpp|MATH_TYPE_OPENTYPE>, a negated relation (<markup|neg> applied to
  <verbatim|=>, <verbatim|\<less\>in\<gtr\>>,
  <verbatim|\<less\>subseteq\<gtr\>> and about forty others) is set as the
  precomposed Unicode symbol when the font has it, instead of a stroke drawn
  through the relation (<cpp|negated_symbols> in
  <source-link|concat_math.cpp|src/Typeset/Concat/concat_math.cpp>).

  <subsection|Script sizes and script alternates>

  <cpp|edit_env_rep::update_font> (<source-link|env_semantics.cpp|src/Typeset/Env/env_semantics.cpp>) builds
  the font of the current mode with the new <cpp|make_current_font>. At
  script levels, when the font has <cpp|ot_math> and
  <src-var|math-font-sizes> is <verbatim|default>, the size is taken from
  <verbatim|scriptPercentScaleDown> at the first level and
  <verbatim|scriptScriptPercentScaleDown> beyond, and for
  <cpp|MATH_TYPE_OPENTYPE> the font is wrapped in a <cpp|feature_font> for
  the <verbatim|ssty> feature (alternate 0 at the first level, 1 beyond),
  which gives the script size shapes of the font. The features a document
  asks for with <src-var|font-features> are applied after that
  (<cpp|apply_features>).

  <section|Constants which are not used>

  Ten of the 56 constants are parsed and not used anywhere outside the
  parser:

  <\description-paragraphs>
    <item*|<verbatim|stackTopShiftUp>, <verbatim|stackTopDisplayStyleShiftUp>,
    <verbatim|stackBottomShiftDown>,
    <verbatim|stackBottomDisplayStyleShiftDown>, <verbatim|stackGapMin>,
    <verbatim|stackDisplayStyleGapMin>>The plain stacks. <markup|above>,
    <markup|below> and binomials over an ordinary base are limit boxes and
    use the limit constants; the stack constants would place the top much
    higher (444 against 111 units in <name|Latin Modern Math>), a visible
    change to every such construct, so they are left out on purpose.

    <item*|<verbatim|delimitedSubFormulaMinHeight>>Deliberately ignored:
    <TeXmacs> sizes every bracket automatically, and the constant would make
    the plain parenthesis of <verbatim|(x)> jump to a larger variant. <TeX>
    and <verbatim|unicode-math> ignore it too.

    <item*|<verbatim|skewedFractionHorizontalGap>,
    <verbatim|skewedFractionVerticalGap>>No primitive of <TeXmacs> draws a
    skewed fraction.

    <item*|<verbatim|mathLeading>>Not needed by the line breaker.
  </description-paragraphs>

  The device tables are not applied either (<hlink|the OpenType layout
  tables|opentype-tables.en.tm>).

  <section|Pitfalls>

  <\itemize>
    <item>Test the right flag. <cpp|ot_math> means \Pthe parameters come
    from a table\Q, and holds for the tuned <name|TeX Gyre> fonts;
    <cpp|math_type == MATH_TYPE_OPENTYPE> means \Pthe table is the only
    source\Q. Code which replaces a tuned behaviour must test the latter, or
    exclude the tuned math types explicitly as <cpp|wide_box_rep> does.

    <item><cpp|smart_font_rep> has a member of its own named
    <cpp|ot_math> (<source-link|smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>), which means \Pthe main font
    is an untuned <name|OpenType> math font whose letters come from its own
    alphabets\Q. It hides the field of <cpp|font_rep> inside the methods of
    the smart font, while the typesetter, which holds a <cpp|font> handle,
    reads the field of <cpp|font_rep>, copied by <cpp|copy_math_pars>.

    <item>Each construction decides on its own whether the table applies, by
    testing one constant for being positive (<cpp|frac_num_gap_min>,
    <cpp|sqrt_degree_rise_percent>, <cpp|sub_sup_gap_min>, the two limit
    constants, <cpp|sqrt_ver_gap>, the bar thicknesses,
    <cpp|accent_base_height>). A font with a table in which that constant
    is zero gets the older layout for that construction only.

    <item>The list of integral signs of <cpp|is_ot_integral> is written out
    by name; <verbatim|\<less\>iint\<gtr\>> is not in it, so the double
    integral gets the italic correction of an ordinary glyph.

    <item>Linux Libertine carries a stub <verbatim|MATH> table in its
    regular face: it is laid out by its tuned branch while the switch is
    on, and by the table otherwise.
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
