
/******************************************************************************
* MODULE     : tt_tools_test.cpp
* DESCRIPTION: tests of the OpenType MATH table parser
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// The expected values below were extracted independently with fontTools
// (python3 -m fontTools.ttx or a small script over font['MATH']).
// Glyph ids refer to the fonts shipped in TeXmacs/fonts/truetype.

#include "tm_test.hpp"
#include "Freetype/tt_tools.hpp"
#include "sys_utils.hpp"
#include "file.hpp"

static url
shipped_font (string name) {
  return url ("$TEXMACS_PATH/fonts/truetype") * url (name);
}

// optional directory with additional fonts, see tests/Makefile
static url
extra_font (string name) {
  string dir= get_env ("TM_TEST_FONT_DIR");
  if (dir == "") return url_none ();
  url u= complete (search_sub_dirs (url_system (dir)) * url_wildcard (name), "fr");
  while (is_or (u)) u= u[1];
  return u;
}


static void
test_no_math_table () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-regular.otf"));
  CHECK (is_nil (t));
}

static void
test_garbage () {
  CHECK (is_nil (parse_mathtable (string (""))));
  CHECK (is_nil (parse_mathtable (string ("this is not a font"))));
  CHECK (is_nil (parse_mathtable (url ("$TEXMACS_PATH/fonts/no-such-font.otf"))));
}

static void
test_pagella_constants () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  CHECK (!is_nil (t));
  CHECK_EQ ((int) t->majorVersion, 1);
  CHECK_EQ ((int) t->minorVersion, 0);
  MathConstantsTable& c= t->constants_table;
  CHECK_EQ (c[scriptPercentScaleDown], 74);
  CHECK_EQ (c[scriptScriptPercentScaleDown], 55);
  CHECK_EQ (c[delimitedSubFormulaMinHeight], 1500);
  CHECK_EQ (c[displayOperatorMinHeight], 1500);
  CHECK_EQ (c[radicalDegreeBottomRaisePercent], 55);
  CHECK_EQ (c[mathLeading], 160);
  CHECK_EQ (c[axisHeight], 250);
  CHECK_EQ (c[accentBaseHeight], 500);
  CHECK_EQ (c[flattenedAccentBaseHeight], 670);
  CHECK_EQ (c[subscriptShiftDown], 232);
  CHECK_EQ (c[superscriptShiftUp], 354);
  CHECK_EQ (c[superscriptShiftUpCramped], 296);
  CHECK_EQ (c[spaceAfterScript], 40);
  CHECK_EQ (c[upperLimitGapMin], 120);
  CHECK_EQ (c[upperLimitBaselineRiseMin], 120);
  CHECK_EQ (c[lowerLimitGapMin], 120);
  CHECK_EQ (c[lowerLimitBaselineDropMin], 468);
  CHECK_EQ (c[stackTopDisplayStyleShiftUp], 720);
  CHECK_EQ (c[fractionNumeratorShiftUp], 469);
  CHECK_EQ (c[fractionNumeratorDisplayStyleShiftUp], 720);
  CHECK_EQ (c[fractionDenominatorShiftDown], 311);
  CHECK_EQ (c[fractionDenominatorDisplayStyleShiftDown], 666);
  CHECK_EQ (c[fractionNumeratorGapMin], 120);
  CHECK_EQ (c[fractionNumDisplayStyleGapMin], 160);
  CHECK_EQ (c[fractionRuleThickness], 60);
  CHECK_EQ (c[fractionDenominatorGapMin], 120);
  CHECK_EQ (c[fractionDenomDisplayStyleGapMin], 160);
  CHECK_EQ (c[skewedFractionHorizontalGap], 350);
  CHECK_EQ (c[overbarExtraAscender], 40);
  CHECK_EQ (c[underbarExtraDescender], 40);
  CHECK_EQ (c[radicalVerticalGap], 120);
  CHECK_EQ (c[radicalDisplayStyleVerticalGap], 120);
  CHECK_EQ (c[radicalRuleThickness], 60);
  CHECK_EQ (c[radicalExtraAscender], 120);
  CHECK_EQ (c[radicalKernBeforeDegree], 40);
  // negative value: checks signed 16 bit decoding
  CHECK_EQ (c[radicalKernAfterDegree], -344);
}

static void
test_pagella_glyph_info () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  CHECK (!is_nil (t));
  CHECK_EQ (N (t->italics_correction), 1253);
  CHECK_EQ (N (t->top_accent), 1941);
  CHECK_EQ (N (t->extended_shape_coverage), 290);

  // U+222B integral, glyph 4113
  CHECK (t->italics_correction->contains (4113));
  CHECK_EQ (t->italics_correction[4113].value, 189);
  CHECK (t->extended_shape_coverage->contains (4113));
  // U+2211 summation, glyph 4214
  CHECK (t->extended_shape_coverage->contains (4214));
  CHECK (!t->italics_correction->contains (4214));
  // U+1D453 math italic f, glyph 474
  CHECK_EQ (t->italics_correction[474].value, 163);
  CHECK_EQ (t->top_accent[474].value, 356);
  // U+1D434 math italic A, glyph 443: top accent only
  CHECK (!t->italics_correction->contains (443));
  CHECK_EQ (t->top_accent[443].value, 485);
  // U+0302 combining circumflex, glyph 3374: negative top accent
  CHECK_EQ (t->top_accent[3374].value, -250);
  // 'm', glyph 78
  CHECK_EQ (t->italics_correction[78].value, 14);
}

static void
test_pagella_vertical_variants () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  CHECK (!is_nil (t));
  CHECK_EQ ((int) t->minConnectorOverlap, 30);
  CHECK_EQ (N (t->ver_glyph_variants), 95);

  // U+0028 parenleft, glyph 9: the first variant is the base glyph itself
  CHECK (t->ver_glyph_variants->contains (9));
  array<unsigned int> v= t->ver_glyph_variants[9];
  array<unsigned int> a= t->ver_glyph_variants_adv[9];
  CHECK_EQ (N (v), 7);
  CHECK_EQ (N (a), 7);
  unsigned int ev[]= {9, 3461, 3483, 3505, 3527, 3549, 3571};
  unsigned int ea[]= {829, 989, 1181, 1411, 1687, 2019, 2417};
  for (int i= 0; i < 7; i++) {
    CHECK_EQ (v[i], ev[i]);
    CHECK_EQ (a[i], ea[i]);
  }
  // variants map back to their base glyph
  CHECK_EQ (t->get_init_glyphID (3483), 9u);
  CHECK_EQ (t->get_init_glyphID (3571), 9u);
  CHECK_EQ (t->get_init_glyphID (9), 9u);
  CHECK_EQ (t->get_init_glyphID (4214), 4214u); // no variants: itself

  // assembly of parenleft: bottom, extender, top
  CHECK (t->ver_glyph_assembly->contains (9));
  GlyphAssembly g= t->ver_glyph_assembly[9];
  CHECK_EQ (g.partCount, 3);
  CHECK_EQ (N (g.partRecords), 3);
  CHECK_EQ (g.italicsCorrection.value, 0);
  CHECK_EQ (g.partRecords[0].glyphID, 3575u);
  CHECK_EQ (g.partRecords[0].startConnectorLength, 0u);
  CHECK_EQ (g.partRecords[0].endConnectorLength, 198u);
  CHECK_EQ (g.partRecords[0].fullAdvance, 1208u);
  CHECK_EQ (g.partRecords[0].partFlags, 0u);
  CHECK_EQ (g.partRecords[1].glyphID, 3576u);
  CHECK_EQ (g.partRecords[1].startConnectorLength, 396u);
  CHECK_EQ (g.partRecords[1].endConnectorLength, 396u);
  CHECK_EQ (g.partRecords[1].fullAdvance, 396u);
  CHECK_EQ (g.partRecords[1].partFlags, 1u); // extender
  CHECK_EQ (g.partRecords[2].glyphID, 3577u);
  CHECK_EQ (g.partRecords[2].partFlags, 0u);

  // braceleft, glyph 92: five parts with two extenders
  GlyphAssembly b= t->ver_glyph_assembly[92];
  CHECK_EQ (b.partCount, 5);
  CHECK_EQ (b.partRecords[1].glyphID, 3590u);
  CHECK_EQ (b.partRecords[3].glyphID, 3590u);
  CHECK_EQ (b.partRecords[1].partFlags, 1u);
  CHECK_EQ (b.partRecords[3].partFlags, 1u);
  CHECK_EQ (b.partRecords[2].fullAdvance, 1194u);

  // summation, glyph 4214: two sizes, no assembly
  CHECK_EQ (N (t->ver_glyph_variants[4214]), 2);
  CHECK_EQ (t->ver_glyph_variants[4214][1], 4217u);
  CHECK (!t->ver_glyph_assembly->contains (4214));

  // integral, glyph 4113: seven sizes and an assembly
  CHECK_EQ (N (t->ver_glyph_variants[4113]), 7);
  CHECK_EQ (t->ver_glyph_variants[4113][6], 4185u);
  CHECK_EQ (t->ver_glyph_variants_adv[4113][6], 2903u);
  CHECK (t->ver_glyph_assembly->contains (4113));
}

static void
test_pagella_horizontal_variants () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  CHECK (!is_nil (t));
  CHECK_EQ (N (t->hor_glyph_variants), 86);

  // U+2192 arrowright, glyph 2962
  CHECK (t->hor_glyph_variants->contains (2962));
  CHECK (!t->ver_glyph_variants->contains (2962));
  array<unsigned int> v= t->hor_glyph_variants[2962];
  CHECK_EQ (N (v), 2);
  CHECK_EQ (v[0], 2962u);
  CHECK_EQ (v[1], 2964u);
  CHECK_EQ (t->hor_glyph_variants_adv[2962][1], 1211u);
  CHECK_EQ (t->get_init_glyphID (2964), 2962u);
  GlyphAssembly g= t->hor_glyph_assembly[2962];
  CHECK_EQ (g.partCount, 3);
  CHECK_EQ (g.partRecords[0].glyphID, 2968u);
  CHECK_EQ (g.partRecords[1].glyphID, 2969u);
  CHECK_EQ (g.partRecords[1].partFlags, 1u);
  CHECK_EQ (g.partRecords[2].glyphID, 2970u);

  // U+0302 circumflex, glyph 3374: seven widths, no assembly
  CHECK_EQ (N (t->hor_glyph_variants[3374]), 7);
  CHECK_EQ (t->hor_glyph_variants[3374][6], 3434u);
  CHECK (!t->hor_glyph_assembly->contains (3374));

  // U+23DE overbrace, glyph 3453: five parts with a middle piece
  GlyphAssembly b= t->hor_glyph_assembly[3453];
  CHECK_EQ (b.partCount, 5);
  CHECK_EQ (b.partRecords[2].glyphID, 3621u);
  CHECK_EQ (b.partRecords[2].fullAdvance, 1769u);
}

static void
test_pagella_no_kerning () {
  ot_mathtable t= parse_mathtable (shipped_font ("texgyre/texgyrepagella-math.otf"));
  CHECK (!is_nil (t));
  CHECK_EQ (N (t->math_kern_info), 0);
  CHECK (!t->has_kerning (474, true, false));
  CHECK (!t->has_kerning (474, false, true));
}

static void
test_stixtwo_kerning () {
  url u= extra_font ("STIXTwoMath-Regular.otf");
  if (is_none (u)) SKIP ("set TM_TEST_FONT_DIR to a directory containing STIXTwoMath-Regular.otf");
  ot_mathtable t= parse_mathtable (u);
  CHECK (!is_nil (t));
  CHECK_EQ (N (t->math_kern_info), 233);
  CHECK_EQ ((int) t->minConnectorOverlap, 100);

  // U+1D449 math italic V, glyph 3321:
  //   top right:    no heights, single value 0
  //   bottom right: heights [156, 280], values [-222, -118, 202]
  CHECK (t->has_kerning (3321, true, false));
  CHECK (t->has_kerning (3321, false, false));
  CHECK (!t->has_kerning (3321, true, true));
  CHECK (!t->has_kerning (3321, false, true));
  CHECK_EQ (t->get_kerning (3321, -1000, true, false), 0);
  CHECK_EQ (t->get_kerning (3321, 1000, true, false), 0);
  CHECK_EQ (t->get_kerning (3321, 0, false, false), -222);
  CHECK_EQ (t->get_kerning (3321, 155, false, false), -222);
  CHECK_EQ (t->get_kerning (3321, 156, false, false), -118);
  CHECK_EQ (t->get_kerning (3321, 200, false, false), -118);
  CHECK_EQ (t->get_kerning (3321, 279, false, false), -118);
  CHECK_EQ (t->get_kerning (3321, 280, false, false), 202);
  CHECK_EQ (t->get_kerning (3321, 5000, false, false), 202);

  // U+1D434 math italic A, glyph 3300: top right [213, 350] -> [58, -58, -70]
  CHECK (t->has_kerning (3300, true, false));
  CHECK_EQ (t->get_kerning (3300, 100, true, false), 58);
  CHECK_EQ (t->get_kerning (3300, 300, true, false), -58);
  CHECK_EQ (t->get_kerning (3300, 400, true, false), -70);
}

static void
test_gpos_kern () {
  // Pair kerning of the GPOS 'kern' feature; expected values and glyph ids
  // extracted with fontTools. These fonts have no legacy 'kern' table.
  string buf;
  CHECK (!load_string (shipped_font ("stix2/STIXTwoText-Regular.otf"),
                         buf, false));
  ot_gpos_kern k= parse_gpos_kern (buf);
  CHECK (!is_nil (k));
  CHECK (!k->empty ());
  CHECK_EQ (k->get (3, 24), -100);   // A V
  CHECK_EQ (k->get (24, 3), -105);   // V A
  CHECK_EQ (k->get (22, 270), -70);  // T o
  CHECK_EQ (k->get (25, 255), -60);  // W a
  CHECK_EQ (k->get (260, 263), 10);  // f i, a positive adjustment
  CHECK_EQ (k->get (273, 1829), -65);// r .
  CHECK_EQ (k->get (18, 255), -25);  // P a
  CHECK_EQ (k->get (3, 3), 0);       // A A is not kerned

  string buf2;
  CHECK (!load_string (shipped_font ("newcm/NewCM10-Regular.otf"),
                         buf2, false));
  ot_gpos_kern k2= parse_gpos_kern (buf2);
  CHECK (!is_nil (k2) && !k2->empty ());
  CHECK_EQ (k2->get (34, 55), -111); // A V
  CHECK_EQ (k2->get (53, 80), -83);  // T o
  CHECK_EQ (k2->get (49, 66), -28);  // P a

  // a font without GPOS kerning gives an empty table
  string buf3;
  CHECK (!load_string (shipped_font ("lm/latinmodern-math.otf"),
                         buf3, false));
  ot_gpos_kern k3= parse_gpos_kern (buf3);
  CHECK (is_nil (k3) || k3->empty ());
}

static void
test_gsub_tags () {
  // the feature tags a font offers, which a menu asks for before proposing
  string buf;
  CHECK (!load_string (shipped_font ("lm/lmroman10-regular.otf"), buf, false));
  array<string> tags= parse_gsub_tags (buf);
  CHECK (N (tags) > 0);
  CHECK (contains (string ("onum"), tags));
  CHECK (!contains (string ("zzzz"), tags));
  // only the features for which something can be applied: not the
  // ligatures, and not 'locl', which this font has for some languages only
  CHECK (!contains (string ("liga"), tags));
  CHECK (!contains (string ("locl"), tags));
  string buf2;
  CHECK (!load_string (shipped_font ("libertinus/LibertinusSans-Regular.otf"),
                         buf2, false));
  tags= parse_gsub_tags (buf2);
  CHECK (contains (string ("smcp"), tags));
  CHECK (contains (string ("c2sc"), tags));
  CHECK (!contains (string ("calt"), tags));
  // a font without GSUB gives nothing, and so does a broken buffer
  CHECK_EQ (N (parse_gsub_tags (string (""))), 0);
  CHECK_EQ (N (parse_gsub_tags (string ("this is not a font"))), 0);
}

/******************************************************************************
* Truncated and wrong tables, made here from the shipped fonts
******************************************************************************/

static int
u16_at (string s, int i) {
  return (((int) ((unsigned char) s[i])) << 8) + ((unsigned char) s[i+1]);
}

static int
u32_at (string s, int i) {
  return (u16_at (s, i) << 16) + u16_at (s, i+2);
}

static string
be16 (int x) {
  string r;
  r << ((char) ((x >> 8) & 255)) << ((char) (x & 255));
  return r;
}

static string
be32 (unsigned int x) {
  return be16 ((int) (x >> 16)) * be16 ((int) (x & 0xffff));
}

// position of the directory entry of a table in a font file, or -1
static int
table_entry (string font, string tag) {
  int nr= u16_at (font, 4);
  for (int k= 0; k < nr; k++)
    if (font (12 + 16*k, 16 + 16*k) == tag) return 12 + 16*k;
  return -1;
}

static string
table_of (string font, string tag) {
  int e= table_entry (font, tag);
  if (e < 0) return "";
  return font (u32_at (font, e + 8), u32_at (font, e + 8) + u32_at (font, e + 12));
}

// the font with other contents for one of its tables: the new table is put
// at the end of the file and the directory is made to point to it
static string
with_table (string font, string tag, string data) {
  int e= table_entry (font, tag);
  if (e < 0) return font;
  string r= copy (font);
  while ((N(r) & 3) != 0) r << '\0';
  string entry= be32 ((unsigned int) N(r)) * be32 ((unsigned int) N(data));
  for (int i= 0; i < 8; i++) r[e + 8 + i]= entry[i];
  r << data;
  return r;
}

static string
load_shipped (string name) {
  string s;
  load_string (shipped_font (name), s, false);
  return s;
}

static void
test_truncated_math () {
  // a MATH table which stops before its end is not used at all
  string font= load_shipped ("texgyre/texgyrepagella-math.otf");
  string math= table_of (font, "MATH");
  CHECK (N (math) > 20000);
  CHECK (!is_nil (parse_mathtable (with_table (font, "MATH", math))));
  int cuts[]= { 0, 1, 4, 9, 10, 11, 12, 100, 225, 1000,
                N (math) / 10, N (math) / 3, N (math) / 2, 2 * N (math) / 3 };
  for (int i= 0; i < 14; i++) {
    ot_mathtable t=
      parse_mathtable (with_table (font, "MATH", math (0, cuts[i])));
    CHECK_MSG (is_nil (t), "MATH cut at " * as_string (cuts[i]));
  }
  // the file itself cut short, anywhere: the table is whole or it is not
  // used
  int e= table_entry (font, "MATH");
  int start= u32_at (font, e + 8), end= start + u32_at (font, e + 12);
  for (int n= 0; n < N (font); n += 4099) {
    ot_mathtable t= parse_mathtable (font (0, n));
    if (n >= end) CHECK_MSG (!is_nil (t), "file cut at " * as_string (n));
    if (n <= (start + end) / 2)
      CHECK_MSG (is_nil (t), "file cut at " * as_string (n));
  }
  // header offsets which point outside the table
  string far= math (0, 4) * be16 (0xffff) * be16 (0xffff) * be16 (0xffff) *
              math (10, N (math));
  CHECK (is_nil (parse_mathtable (with_table (font, "MATH", far))));
}

static void
test_math_null_parts () {
  // MathGlyphInfo and MathVariants may be absent; the header is not to be
  // read in their place, whatever the constants look like
  string font= load_shipped ("texgyre/texgyrepagella-math.otf");
  string math= table_of (font, "MATH");
  int    co  = u16_at (math, 4);
  for (int scale= 1; scale <= 2; scale++) {
    string m= math (0, 6) * be16 (0) * be16 (0) * math (10, co) *
              be16 (scale) * math (co + 2, co + 214);
    ot_mathtable t= parse_mathtable (with_table (font, "MATH", m));
    CHECK (!is_nil (t));
    if (is_nil (t)) continue;
    CHECK_EQ (t->constants_table[scriptPercentScaleDown], scale);
    CHECK_EQ (t->constants_table[axisHeight], 250);
    CHECK_EQ (N (t->italics_correction), 0);
    CHECK_EQ (N (t->top_accent), 0);
    CHECK_EQ (N (t->extended_shape_coverage), 0);
    CHECK_EQ (N (t->math_kern_info), 0);
    CHECK_EQ (N (t->ver_glyph_variants), 0);
    CHECK_EQ (N (t->hor_glyph_variants), 0);
  }
  // without constants there is nothing to typeset with
  string m= math (0, 4) * be16 (0) * math (6, N (math));
  CHECK (is_nil (parse_mathtable (with_table (font, "MATH", m))));
}

// a MATH table whose vertical coverage has nr ranges of all the glyphs
static string
math_with_ranges (int nr) {
  string r= be16 (1) * be16 (0) * be16 (10) * be16 (226) * be16 (234);
  r << string ('\0', 216);                               // constants
  r << be16 (0) << be16 (0) << be16 (0) << be16 (0);     // glyph info
  r << be16 (0) << be16 (10) << be16 (0) << be16 (0) << be16 (0);
  r << be16 (2) << be16 (nr);
  for (int i= 0; i < nr; i++) r << be16 (0) << be16 (0xffff) << be16 (0);
  return r;
}

static void
test_oversized_coverage () {
  // a coverage cannot hold more glyphs than a font has; reading one which
  // claims to takes no time and no memory
  string font= load_shipped ("texgyre/texgyrepagella-math.otf");
  int nrs[]= { 2, 40, 300, 65535 };
  for (int i= 0; i < 4; i++) {
    string m= math_with_ranges (nrs[i]);
    CHECK_MSG (is_nil (parse_mathtable (with_table (font, "MATH", m))),
               as_string (nrs[i]) * " ranges");
  }
  // one range of all the glyphs, for which the table has no construction
  ot_mathtable t=
    parse_mathtable (with_table (font, "MATH", math_with_ranges (1)));
  CHECK (!is_nil (t));
  if (!is_nil (t)) CHECK_EQ (N (t->ver_glyph_variants), 0);
}

// a GSUB or GPOS table with one feature made of one lookup with one subtable
static string
layout_table (string tag, int lookup_type, string subtable) {
  string scripts = be16 (0);
  string features= be16 (1) * tag * be16 (8) *
                   be16 (0) * be16 (1) * be16 (0);
  string lookups = be16 (1) * be16 (4) *
                   be16 (lookup_type) * be16 (0) * be16 (1) * be16 (8) *
                   subtable;
  return be16 (1) * be16 (0) * be16 (10) * be16 (10 + N (scripts)) *
         be16 (10 + N (scripts) + N (features)) *
         scripts * features * lookups;
}

static void
test_wrong_gsub () {
  string font= load_shipped ("texgyre/texgyrepagella-regular.otf");
  string gsub= table_of (font, "GSUB");
  CHECK (N (gsub) > 1000);
  // a sound table made the same way is read: glyph 5 becomes glyph 7
  string single= be16 (1) * be16 (6) * be16 (2) * be16 (1) * be16 (1) *
                 be16 (5);
  ot_gsub_map ok= parse_gsub_feature (
    with_table (font, "GSUB", layout_table ("ss01", 1, single)), "ss01");
  CHECK_EQ (N (ok), 1);
  CHECK (ok->contains (5) && N (ok[5]) == 1 && ok[5][0] == 7);
  // an extension subtable which leads to itself
  string loop= be16 (1) * be16 (7) * be32 (0);
  string f1  = with_table (font, "GSUB", layout_table ("ss01", 7, loop));
  CHECK_EQ (N (parse_gsub_feature (f1, "ss01")), 0);
  CHECK_EQ (N (parse_gsub_tags (f1)), 0);
  // an extension subtable which leads far outside the table
  unsigned int offs[]= { 0x7ffffff0u, 0xfffffff0u, 0x10000u };
  for (int i= 0; i < 3; i++) {
    string far= be16 (1) * be16 (1) * be32 (offs[i]);
    string f2 = with_table (font, "GSUB", layout_table ("ss01", 7, far));
    CHECK_EQ (N (parse_gsub_feature (f2, "ss01")), 0);
  }
  // a set of alternates which claims more glyphs than the table holds
  string alt= be16 (1) * be16 (8) * be16 (1) * be16 (14) *
              be16 (1) * be16 (1) * be16 (5) * be16 (0xffff);
  string f3 = with_table (font, "GSUB", layout_table ("ss01", 3, alt));
  CHECK_EQ (N (parse_gsub_feature (f3, "ss01")), 0);
  // the table of the font, cut anywhere
  for (int n= 0; n < N (gsub); n += 487) {
    string f4= with_table (font, "GSUB", gsub (0, n));
    parse_gsub_tags (f4);
    parse_gsub_feature (f4, "smcp");
    parse_gsub_feature (f4, "aalt");
  }
  CHECK (N (parse_gsub_feature (font, "smcp")) > 100);
}

// a pair adjustment subtable by classes, with the given numbers of classes
// and the given x advances for them
static string
pair_classes (int c1, int c2, string values) {
  int n= 16 + N (values);
  return be16 (2) * be16 (n) * be16 (4) * be16 (0) *
         be16 (n + 6) * be16 (n + 10) * be16 (c1) * be16 (c2) * values *
         be16 (1) * be16 (1) * be16 (5) *   // coverage: glyph 5
         be16 (2) * be16 (0) *              // class definitions, empty
         be16 (2) * be16 (0);
}

static void
test_wrong_gpos () {
  string font= load_shipped ("texgyre/texgyrepagella-regular.otf");
  string gpos= table_of (font, "GPOS");
  CHECK (N (gpos) > 1000);
  // a sound table made the same way is read
  string one= pair_classes (1, 1, be16 (-50 & 0xffff));
  ot_gpos_kern ok= parse_gpos_kern (
    with_table (font, "GPOS", layout_table ("kern", 2, one)));
  CHECK (!is_nil (ok) && !ok->empty ());
  if (!is_nil (ok)) CHECK_EQ (ok->get (5, 9), -50);
  // class counts whose product does not fit the table, or an integer
  int counts[]= { 3000, 46341, 65535 };
  for (int i= 0; i < 3; i++) {
    string sub= pair_classes (counts[i], counts[i], "");
    ot_gpos_kern k= parse_gpos_kern (
      with_table (font, "GPOS", layout_table ("kern", 2, sub)));
    CHECK_MSG (is_nil (k) || k->empty (), as_string (counts[i]) * " classes");
    if (!is_nil (k)) CHECK_EQ (k->get (5, 9), 0);
  }
  // the table of the font, cut anywhere
  for (int n= 0; n < N (gpos); n += 911) {
    ot_gpos_kern k= parse_gpos_kern (with_table (font, "GPOS", gpos (0, n)));
    if (!is_nil (k)) k->get (34, 55);
  }
  ot_gpos_kern k= parse_gpos_kern (font);
  CHECK (!is_nil (k) && !k->empty ());
}

static void
test_gpos_several_lookups () {
  // The 'kern' feature of Libertinus Serif has several lookups, and the
  // first one covers V and A with a class of value zero for these pairs;
  // the kerning comes from a later lookup. Values from fontTools.
  string buf= load_shipped ("libertinus/LibertinusSerif-Regular.otf");
  ot_gpos_kern k= parse_gpos_kern (buf);
  CHECK (!is_nil (k) && !k->empty ());
  if (is_nil (k)) return;
  CHECK_EQ (k->get (55, 70), -80);   // V e
  CHECK_EQ (k->get (34, 69), -12);   // A d
  CHECK_EQ (k->get (55, 70), -80);   // asked again
}

static void
test_gsub_default_language () {
  // Libertinus Sans has one 'smcp' feature for Turkish and its relatives,
  // listed first, and one for the other languages. Without a language the
  // i becomes i.sc (glyph 2203) and not the dotted idotaccent.sc (2372).
  // The small capitals themselves are in a lookup of type 2.
  string buf= load_shipped ("libertinus/LibertinusSans-Regular.otf");
  ot_gsub_map m= parse_gsub_feature (buf, "smcp");
  CHECK (N (m) >= 90);
  CHECK (m->contains (74));
  if (m->contains (74)) {
    CHECK_EQ (N (m[74]), 1);
    CHECK_EQ ((int) m[74][0], 2203);
  }
  CHECK (N (parse_gsub_feature (buf, "c2sc")) >= 90);
}

int
main () {
  RUN (test_no_math_table);
  RUN (test_garbage);
  RUN (test_pagella_constants);
  RUN (test_pagella_glyph_info);
  RUN (test_pagella_vertical_variants);
  RUN (test_pagella_horizontal_variants);
  RUN (test_pagella_no_kerning);
  RUN (test_stixtwo_kerning);
  RUN (test_gpos_kern);
  RUN (test_gsub_tags);
  RUN (test_truncated_math);
  RUN (test_math_null_parts);
  RUN (test_oversized_coverage);
  RUN (test_wrong_gsub);
  RUN (test_wrong_gpos);
  RUN (test_gpos_several_lookups);
  RUN (test_gsub_default_language);
  return test_report ();
}
