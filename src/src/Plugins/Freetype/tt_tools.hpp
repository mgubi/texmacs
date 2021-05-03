
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
#include "hashmap.hpp"

/******************************************************************************
* OpenType MATH table
******************************************************************************/
// for the specification
// see https://docs.microsoft.com/en-gb/typography/opentype/spec/math

struct ot_mathtable_rep : concrete_struct {
  int majorVersion, minorVersion;
  int params[58];
  int minConnectorOverlap;
  hashmap<unsigned int, array<unsigned int> > ver_glyph_variants;
  hashmap<unsigned int, array<int> > ver_glyph_variants_adv;
  hashmap<unsigned int, array<unsigned int> > hor_glyph_variants;
  hashmap<unsigned int, array<int> > hor_glyph_variants_adv;
  hashmap<unsigned int, array<unsigned int> > ver_glyph_assembly;
  hashmap<unsigned int, array<unsigned int> > hor_glyph_assembly;
  
  ot_mathtable_rep () {};
};

struct ot_mathtable {
  CONCRETE_NULL(ot_mathtable);
  inline ot_mathtable (ot_mathtable_rep* rep2):
    rep(rep2) { INC_COUNT_NULL (this->rep); }
};
CONCRETE_NULL_CODE(ot_mathtable);

ot_mathtable parse_mathtable (string buf);
void dump_mathtable (ot_mathtable table);

/******************************************************************************
* interface
******************************************************************************/

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

#endif // TT_TOOLS_H
