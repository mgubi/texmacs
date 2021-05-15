
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

// indexes into ot_mathtable_rep::params
enum otmath_constants {
  scriptPercentScaleDown,
  scriptScriptPercentScaleDown,
  delimitedSubFormulaMinHeight,
  displayOperatorMinHeight,
  mathLeading_value,
  mathLeading_devoff,
  axisHeight_value,
  axisHeight_devoff,
  accentBaseHeight_value,
  accentBaseHeight_devoff,
  flattenedAccentBaseHeight_value,
  flattenedAccentBaseHeight_devoff,
  subscriptShiftDown_value,
  subscriptShiftDown_devoff,
  subscriptTopMax_value,
  subscriptTopMax_devoff,
  subscriptBaselineDropMin_value,
  subscriptBaselineDropMin_devoff,
  superscriptShiftUp_value,
  superscriptShiftUp_devoff,
  superscriptShiftUpCramped_value,
  superscriptShiftUpCramped_devoff,
  superscriptBottomMin_value,
  superscriptBottomMin_devoff,
  superscriptBaselineDropMax_value,
  superscriptBaselineDropMax_devoff,
  subSuperscriptGapMin_value,
  subSuperscriptGapMin_devoff,
  superscriptBottomMaxWithSubscript_value,
  superscriptBottomMaxWithSubscript_devoff,
  spaceAfterScript_value,
  spaceAfterScript_devoff,
  upperLimitGapMin_value,
  upperLimitGapMin_devoff,
  upperLimitBaselineRiseMin_value,
  upperLimitBaselineRiseMin_devoff,
  lowerLimitGapMin_value,
  lowerLimitGapMin_devoff,
  lowerLimitBaselineDropMin_value,
  lowerLimitBaselineDropMin_devoff,
  stackTopShiftUp_value,
  stackTopShiftUp_devoff,
  stackTopDisplayStyleShiftUp_value,
  stackTopDisplayStyleShiftUp_devoff,
  stackBottomShiftDown_value,
  stackBottomShiftDown_devoff,
  stackBottomDisplayStyleShiftDown_value,
  stackBottomDisplayStyleShiftDown_devoff,
  stackGapMin_value,
  stackGapMin_devoff,
  stackDisplayStyleGapMin_value,
  stackDisplayStyleGapMin_devoff,
  stretchStackTopShiftUp_value,
  stretchStackTopShiftUp_devoff,
  stretchStackBottomShiftDown_value,
  stretchStackBottomShiftDown_devoff,
  stretchStackGapAboveMin_value,
  stretchStackGapAboveMin_devoff,
  stretchStackGapBelowMin_value,
  stretchStackGapBelowMin_devoff,
  fractionNumeratorShiftUp_value,
  fractionNumeratorShiftUp_devoff,
  fractionNumeratorDisplayStyleShiftUp_value,
  fractionNumeratorDisplayStyleShiftUp_devoff,
  fractionDenominatorShiftDown_value,
  fractionDenominatorShiftDown_devoff,
  fractionDenominatorDisplayStyleShiftDown_value,
  fractionDenominatorDisplayStyleShiftDown_devoff,
  fractionNumeratorGapMin_value,
  fractionNumeratorGapMin_devoff,
  fractionNumDisplayStyleGapMin_value,
  fractionNumDisplayStyleGapMin_devoff,
  fractionRuleThickness_value,
  fractionRuleThickness_devoff,
  fractionDenominatorGapMin_value,
  fractionDenominatorGapMin_devoff,
  fractionDenomDisplayStyleGapMin_value,
  fractionDenomDisplayStyleGapMin_devoff,
  skewedFractionHorizontalGap_value,
  skewedFractionHorizontalGap_devoff,
  skewedFractionVerticalGap_value,
  skewedFractionVerticalGap_devoff,
  overbarVerticalGap_value,
  overbarVerticalGap_devoff,
  overbarRuleThickness_value,
  overbarRuleThickness_devoff,
  overbarExtraAscender_value,
  overbarExtraAscender_devoff,
  underbarVerticalGap_value,
  underbarVerticalGap_devoff,
  underbarRuleThickness_value,
  underbarRuleThickness_devoff,
  underbarExtraDescender_value,
  underbarExtraDescender_devoff,
  radicalVerticalGap_value,
  radicalVerticalGap_devoff,
  radicalDisplayStyleVerticalGap_value,
  radicalDisplayStyleVerticalGap_devoff,
  radicalRuleThickness_value,
  radicalRuleThickness_devoff,
  radicalExtraAscender_value,
  radicalExtraAscender_devoff,
  radicalKernBeforeDegree_value,
  radicalKernBeforeDegree_devoff,
  radicalKernAfterDegree_value,
  radicalKernAfterDegree_devoff,
  radicalDegreeBottomRaisePercent,
  otmath_constants_end // keep at the end
};



struct ot_mathtable_rep : concrete_struct {
  int majorVersion, minorVersion;
  int params [otmath_constants_end];
  int minConnectorOverlap;
  hashmap<unsigned int, array<unsigned int> > ver_glyph_variants;
  hashmap<unsigned int, array<int> > ver_glyph_variants_adv;
  hashmap<unsigned int, array<unsigned int> > hor_glyph_variants;
  hashmap<unsigned int, array<int> > hor_glyph_variants_adv;
  hashmap<unsigned int, array<unsigned int> > ver_glyph_assembly;
  hashmap<unsigned int, array<unsigned int> > hor_glyph_assembly;
  hashmap<unsigned int, int> italics_correction_value;
  hashmap<unsigned int, int> italics_correction_devoff;
  hashmap<unsigned int, int> top_accent_value;
  hashmap<unsigned int, int> top_accent_devoff;

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
