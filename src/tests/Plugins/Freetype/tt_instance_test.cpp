/******************************************************************************
* MODULE     : tt_instance_test.cpp
* DESCRIPTION: tests of the static fonts made from variable fonts
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// The features which a variable font switches at some points of its design
// space (FeatureVariations). The fonts are those of the test suite of
// HarfBuzz (test/subset/data/fonts), found through TM_TEST_FONT_DIR. The
// expected glyphs were computed with fontTools: the glyph of a character
// in an instance by fontTools.varLib.instancer, after the substitutions of
// rvrn in that instance.

#include "tm_test.hpp"
#include "Freetype/tt_tools.hpp"
#include "Freetype/free_type.hpp"
#include "sys_utils.hpp"
#include "file.hpp"

string tt_table (string tt, int i, string tag);

static url
extra_font (string name) {
  string dir= get_env ("TM_TEST_FONT_DIR");
  if (dir == "") return url_none ();
  url u= complete (search_sub_dirs (url_system (dir)) * url_wildcard (name), "fr");
  while (is_or (u)) u= u[1];
  return u;
}

static string
load_font (string name) {
  string tt;
  url u= extra_font (name);
  if (is_none (u) || load_string (u, tt, false)) return "";
  return tt;
}

static int
U16 (string s, int i) {
  return (((int) (unsigned char) s[i]) << 8) + ((int) (unsigned char) s[i+1]);
}

// the glyph of the character c in the font tt, as FreeType finds it
static int
glyph_of (string tt, int c) {
  if (ft_initialize ()) return -1;
  FT_Face face;
  if (ft_new_memory_face (ft_library, (const FT_Byte*) &(tt[0]), N(tt),
                          0, &face))
    return -1;
  int g= (int) ft_get_char_index (face, c);
  ft_done_face (face);
  return g;
}

// the static font at the given design coordinates, the others default
static string
instance (string tt, array<string> tags, array<double> values) {
  string fv= tt_table (tt, 0, "fvar");
  array<string> axes= tt_axis_tags (fv);
  array<double> coords= tt_axis_values (fv, 1);
  for (int i=0; i<N(tags); i++)
    for (int a=0; a<N(axes); a++)
      if (axes[a] == tags[i]) coords[a]= values[i];
  return tt_make_variation (tt, coords);
}

static string
instance (string tt) {
  return instance (tt, array<string> (), array<double> ());
}

static string
instance (string tt, string t1, double v1) {
  array<string> tags; tags << t1;
  array<double> values; values << v1;
  return instance (tt, tags, values);
}

static string
instance (string tt, string t1, double v1, string t2, double v2) {
  array<string> tags; tags << t1 << t2;
  array<double> values; values << v1 << v2;
  return instance (tt, tags, values);
}

static void
test_layout_version () {
  // the instance has GSUB 1.0, without FeatureVariations, and the features
  // of the FeatureList are those of the point
  string tt= load_font ("Fraunces.ttf");
  if (tt == "") SKIP ("set TM_TEST_FONT_DIR to a directory with Fraunces.ttf");
  string gsub= tt_table (tt, 0, "GSUB");
  CHECK_EQ (U16 (gsub, 2), 1);
  CHECK_EQ (N (parse_gsub_feature_lookups (gsub, "rvrn")), 0);
  string inst= instance (tt);
  CHECK (inst != "");
  string g= tt_table (inst, 0, "GSUB");
  CHECK_EQ (U16 (g, 0), 1);
  CHECK_EQ (U16 (g, 2), 0);
  array<int> lk= parse_gsub_feature_lookups (g, "rvrn");
  CHECK_EQ (N (lk), 1);
  if (N (lk) == 1) CHECK_EQ (lk[0], 3);
  // the other features and the lookups themselves are as they were
  CHECK_EQ (N (parse_gsub_tags (inst)), N (parse_gsub_tags (tt)));
  CHECK_EQ (N (parse_gsub_feature (inst, "smcp")),
            N (parse_gsub_feature (tt, "smcp")));
  CHECK_EQ (N (parse_gsub_feature (inst, "onum")),
            N (parse_gsub_feature (tt, "onum")));
  // a point where no record applies keeps the features of the FeatureList
  string plain= instance (tt, "opsz", 36.0);
  CHECK_EQ (N (parse_gsub_feature_lookups (tt_table (plain, 0, "GSUB"),
                                           "rvrn")), 0);
}

static void
test_varies_at_default () {
  string fr= load_font ("Fraunces.ttf");
  string rf= load_font ("RobotoFlex-Variable.ttf");
  if (fr == "" || rf == "")
    SKIP ("set TM_TEST_FONT_DIR to a directory with Fraunces.ttf and "
          "RobotoFlex-Variable.ttf");
  CHECK (tt_varies_at_default (tt_table (fr, 0, "GSUB")));
  CHECK (!tt_varies_at_default (tt_table (rf, 0, "GSUB")));
  CHECK (!tt_varies_at_default (tt_table (fr, 0, "GPOS")));
  CHECK (!tt_varies_at_default (string ("")));
}

static void
test_fraunces_wonky () {
  // the wonky h, m, n and ampersand of Fraunces, at small optical sizes
  // and heavy weights, or wherever WONK is off
  string tt= load_font ("Fraunces.ttf");
  if (tt == "") SKIP ("set TM_TEST_FONT_DIR to a directory with Fraunces.ttf");
  CHECK_EQ (glyph_of (tt, 'h'), 37);
  string d= instance (tt);
  CHECK_EQ (glyph_of (d, 'h'), 38);
  CHECK_EQ (glyph_of (d, 'm'), 44);
  CHECK_EQ (glyph_of (d, 'n'), 46);
  CHECK_EQ (glyph_of (d, '&'), 61);
  CHECK_EQ (glyph_of (d, 'a'), 29);
  CHECK_EQ (glyph_of (instance (tt, "wght", 400.0), 'h'), 38);
  string big= instance (tt, "opsz", 36.0);
  CHECK_EQ (glyph_of (big, 'h'), 37);
  CHECK_EQ (glyph_of (big, '&'), 60);
  string off= instance (tt, "opsz", 36.0, "WONK", 0.0);
  CHECK_EQ (glyph_of (off, 'h'), 38);
  CHECK_EQ (glyph_of (off, '&'), 61);
}

static void
test_roboto_flex_dollar () {
  // the dollar and the cent of Roboto Flex, with one bar at heavy weights
  // and narrow widths
  string tt= load_font ("RobotoFlex-Variable.ttf");
  if (tt == "")
    SKIP ("set TM_TEST_FONT_DIR to a directory with RobotoFlex-Variable.ttf");
  CHECK_EQ (glyph_of (instance (tt, "wght", 500.0), '$'), 7);
  CHECK_EQ (glyph_of (instance (tt, "wght", 700.0), '$'), 602);
  string narrow= instance (tt, "wdth", 50.0);
  CHECK_EQ (glyph_of (narrow, '$'), 602);
  CHECK_EQ (glyph_of (narrow, 0xA2), 887);
  // a character beyond the substitutions keeps its glyph
  CHECK_EQ (glyph_of (narrow, 'a'), glyph_of (tt, 'a'));
}

static void
test_names () {
  // the name of a point keeps the case of the axes, which tells WONK from
  // a registered axis; Fraunces at its default is an instance of its own
  string dir= get_env ("TM_TEST_FONT_DIR");
  if (load_font ("Fraunces.ttf") == "")
    SKIP ("set TM_TEST_FONT_DIR to a directory with Fraunces.ttf");
  set_env ("TEXMACS_FONT_PATH", dir);
  CHECK_EQ (tt_variation_name ("Fraunces", "opsz=36,WONK=0", 10),
            string ("Fraunces.var_opsz36_WONK0"));
  CHECK_EQ (tt_variation_name ("Fraunces", "", 10),
            string ("Fraunces.var_default"));
  CHECK_EQ (tt_variation_name ("Fraunces.var_opsz36_WONK0", "opsz=9", 10),
            string ("Fraunces.var_WONK0"));
  CHECK_EQ (tt_variation_name ("RobotoFlex-Variable", "", 10),
            string ("RobotoFlex-Variable"));
  CHECK (tt_is_instance_name ("Fraunces.var_WONK0"));
  CHECK (tt_is_instance_name ("Fraunces.var_default"));
}

int
main () {
  RUN (test_layout_version);
  RUN (test_varies_at_default);
  RUN (test_fraunces_wonky);
  RUN (test_roboto_flex_dollar);
  RUN (test_names);
  return test_report ();
}
