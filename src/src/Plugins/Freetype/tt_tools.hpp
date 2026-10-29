
/******************************************************************************
* MODULE     : tt_tools.hpp
* DESCRIPTION: Direct access of True Type font (independent from FreeType)
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TT_TOOLS_H
#define TT_TOOLS_H
#include "url.hpp"

void tt_dump (url u);
scheme_tree tt_font_name (url u);
url tt_unpack (string name);

string find_attribute_value (array<string> a, string s);
array<string> tt_analyze (string family);
double characteristic_distance (array<string> a1, array<string> a2);
double trace_distance (string v1, string v2, double m);

// quantities with respect to ex height
double get_M_width       (array<string> a);
double get_lo_pen_width  (array<string> a);
double get_lo_pen_height (array<string> a);
double get_up_pen_width  (array<string> a);
double get_up_pen_height (array<string> a);

// The tables of OpenType and of AAT which make one glyph of a sequence of
// characters (the emoji: a family, a flag, a skin tone, a keycap)
#include "hashmap.hpp"
// the ligatures (type 4) of one feature: for the first glyph of each, the
// ligatures which start with it, in the order of the font (the preferred
// first), each as the ligature glyph followed by the other components
typedef hashmap<unsigned int, array<array<unsigned int> > > ot_gsub_ligatures;
ot_gsub_ligatures parse_gsub_ligatures (const char* buf, int n, string feature);
// a table of a font file in memory (a TrueType collection gives its first
// font), or "" when it has none
string ot_find_table (const char* buf, int n, const char* tag);
// the same, from the GSUB table itself
ot_gsub_ligatures parse_gsub_ligatures_table (string gsub, string feature);
// the glyphs of a sequence after the substitutions of the default features
// of an AAT morx table (the fonts of Apple: non contextual, contextual and
// ligature subtables), the deleted glyphs left out; nglyphs is the number
// of glyphs of the font
array<unsigned int> aat_morx_apply (string morx, array<unsigned int> glyphs,
                                    int nglyphs);

#endif // TT_TOOLS_H
