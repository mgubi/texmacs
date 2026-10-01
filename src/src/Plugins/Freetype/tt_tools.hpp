
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
#include "hashset.hpp"

void tt_dump (url u);
scheme_tree tt_font_name (url u);
scheme_tree tt_font_instances (url u);
bool tt_is_variable (string tt, int i);
bool tt_file_is_variable (url u);
int tt_nr_instances (string fvar);
int tt_instance_record (string fvar, int k);
double tt_instance_coordinate (string fvar, int k, string axis, double def);
string tt_make_instance (string tt, int k);
string tt_make_variation (string tt, array<double> coords);
string tt_vary_gpos (string gpos, string gdef, array<double> nc);
string tt_vary_layout (string table, array<double> nc);
bool tt_varies_at_default (string gsub_or_gpos);
array<string> tt_axis_tags (string fvar);
array<double> tt_axis_values (string fvar, int which);
array<double> tt_variation_coordinates (string fvar, string suffix);
string tt_variation_name (string name, string spec, int sz);
scheme_tree tt_font_axes (string name);
url tt_unpack (string name);
bool tt_is_instance_name (string name);
void tt_clean_instances (int max_mb);

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


/******************************************************************************
 * OpenType MATH table
 ******************************************************************************/
// for the specification
// see https://docs.microsoft.com/en-gb/typography/opentype/spec/math

// index of the MathConstantsTable.records
enum MathConstantRecordEnum {
  mathLeading,
  axisHeight,
  accentBaseHeight,
  flattenedAccentBaseHeight,
  subscriptShiftDown,
  subscriptTopMax,
  subscriptBaselineDropMin,
  superscriptShiftUp,
  superscriptShiftUpCramped,
  superscriptBottomMin,
  superscriptBaselineDropMax,
  subSuperscriptGapMin,
  superscriptBottomMaxWithSubscript,
  spaceAfterScript,
  upperLimitGapMin,
  upperLimitBaselineRiseMin,
  lowerLimitGapMin,
  lowerLimitBaselineDropMin,
  stackTopShiftUp,
  stackTopDisplayStyleShiftUp,
  stackBottomShiftDown,
  stackBottomDisplayStyleShiftDown,
  stackGapMin,
  stackDisplayStyleGapMin,
  stretchStackTopShiftUp,
  stretchStackBottomShiftDown,
  stretchStackGapAboveMin,
  stretchStackGapBelowMin,
  fractionNumeratorShiftUp,
  fractionNumeratorDisplayStyleShiftUp,
  fractionDenominatorShiftDown,
  fractionDenominatorDisplayStyleShiftDown,
  fractionNumeratorGapMin,
  fractionNumDisplayStyleGapMin,
  fractionRuleThickness,
  fractionDenominatorGapMin,
  fractionDenomDisplayStyleGapMin,
  skewedFractionHorizontalGap,
  skewedFractionVerticalGap,
  overbarVerticalGap,
  overbarRuleThickness,
  overbarExtraAscender,
  underbarVerticalGap,
  underbarRuleThickness,
  underbarExtraDescender,
  radicalVerticalGap,
  radicalDisplayStyleVerticalGap,
  radicalRuleThickness,
  radicalExtraAscender,
  radicalKernBeforeDegree,
  radicalKernAfterDegree,
  otmathConstantsRecordsEnd, // count the number of records
  scriptPercentScaleDown,
  scriptScriptPercentScaleDown,
  delimitedSubFormulaMinHeight,
  displayOperatorMinHeight,
  radicalDegreeBottomRaisePercent
};

struct DeviceTable {
  unsigned int startSize;
  unsigned int endSize;
  unsigned int deltaFormat;
  unsigned int deltaValues;
};

struct MathValueRecord {
  int         value;
  bool        hasDevice;
  DeviceTable deviceTable;
  MathValueRecord () : hasDevice (false) {}

  // cast to int
//  operator int () const { return value; }
};

struct MathConstantsTable {
  array<MathValueRecord> records;
  int                    scriptPercentScaleDown;
  int                    scriptScriptPercentScaleDown;
  unsigned int           delimitedSubFormulaMinHeight;
  unsigned int           displayOperatorMinHeight;
  int                    radicalDegreeBottomRaisePercent;

  MathConstantsTable ()
      : records (MathConstantRecordEnum::otmathConstantsRecordsEnd){};

  int operator[] (int i) {
    if (i >= 0 && i < MathConstantRecordEnum::otmathConstantsRecordsEnd)
      return records[i].value;
    switch (i) {
    case MathConstantRecordEnum::scriptPercentScaleDown:
      return scriptPercentScaleDown;
    case MathConstantRecordEnum::scriptScriptPercentScaleDown:
      return scriptScriptPercentScaleDown;
    case MathConstantRecordEnum::delimitedSubFormulaMinHeight:
      return delimitedSubFormulaMinHeight;
    case MathConstantRecordEnum::displayOperatorMinHeight:
      return displayOperatorMinHeight;
    case MathConstantRecordEnum::radicalDegreeBottomRaisePercent:
      return radicalDegreeBottomRaisePercent;
    }
    FAILED ("MathConstantsTable: index out of range");
    return 0; // should never reach here
  }
};

struct MathKernTable {
  unsigned int           heightCount;
  array<MathValueRecord> correctionHeight;
  array<MathValueRecord> kernValues;
  MathKernTable ()= default;
  MathKernTable (unsigned int h)
      : heightCount (h), correctionHeight (h), kernValues (h + 1) {}
};

struct MathKernInfoRecord {
  MathKernTable topRight;
  MathKernTable topLeft;
  MathKernTable bottomRight;
  MathKernTable bottomLeft;
  bool          hasTopRight;
  bool          hasTopLeft;
  bool          hasBottomRight;
  bool          hasBottomLeft;
  MathKernInfoRecord ()
      : hasTopRight (false), hasTopLeft (false), hasBottomRight (false),
        hasBottomLeft (false) {}

//  bool has_kerning (bool top, bool left);
//  int  get_kerning (int height, bool top, bool left);
};

struct GlyphPartRecord {
  unsigned int glyphID;
  unsigned int startConnectorLength;
  unsigned int endConnectorLength;
  unsigned int fullAdvance;
  unsigned int partFlags;
};

struct GlyphAssembly {
  MathValueRecord        italicsCorrection;
  array<GlyphPartRecord> partRecords;
  int                    partCount;

//  const GlyphPartRecord& operator[] (int i) { return partRecords[i]; }
};

struct ot_mathtable_rep : concrete_struct {
  unsigned int                               majorVersion, minorVersion;
  MathConstantsTable                         constants_table;
  unsigned int                               minConnectorOverlap;
  hashmap<unsigned int, MathValueRecord>     italics_correction;
  hashmap<unsigned int, MathValueRecord>     top_accent;
  hashset<unsigned int>                      extended_shape_coverage;
  hashmap<unsigned int, MathKernInfoRecord>  math_kern_info;
  hashmap<unsigned int, array<unsigned int>> ver_glyph_variants;
  hashmap<unsigned int, array<unsigned int>> ver_glyph_variants_adv;
  hashmap<unsigned int, array<unsigned int>> hor_glyph_variants;
  hashmap<unsigned int, array<unsigned int>> hor_glyph_variants_adv;
  hashmap<unsigned int, GlyphAssembly>       ver_glyph_assembly;
  hashmap<unsigned int, GlyphAssembly>       hor_glyph_assembly;

  // helper functions and data
  hashmap<unsigned int, unsigned int> get_init_glyphID_cache;
  // for variant glyph, get the glyphID of the base glyph
  unsigned int get_init_glyphID (unsigned int glyphID);

  bool has_kerning (unsigned int glyphID, bool top, bool left);
  int  get_kerning (unsigned int glyphID, int height, bool top, bool left);
};

struct ot_mathtable {
  CONCRETE_NULL (ot_mathtable);
  ot_mathtable (ot_mathtable_rep* rep2) : rep (rep2) {}
};
CONCRETE_NULL_CODE (ot_mathtable);

ot_mathtable parse_mathtable (const string& buf);

/******************************************************************************
 * OpenType GSUB: single and alternate substitutions of one feature
 ******************************************************************************/
// glyph -> substitutes (one for single substitutions, the alternates in
// order for alternate substitutions), for all lookups of the feature tag
typedef hashmap<unsigned int, array<unsigned int> > ot_gsub_map;
ot_gsub_map parse_gsub_feature (const string& buf, string feature);
// the same, from the GSUB table, one lookup at a time
array<int> parse_gsub_feature_lookups (const string& gsub, string feature);
ot_gsub_map parse_gsub_lookup (const string& gsub, int lookup_index);
array<string> parse_gsub_tags (const string& buf);

/******************************************************************************
 * OpenType GPOS: pair kerning
 ******************************************************************************/
// Modern OpenType fonts keep their kerning in the GPOS table; the legacy
// 'kern' table which FreeType exposes is usually absent. We read the pair
// adjustments of the 'kern' feature: the explicit pairs of a format 1
// subtable and the class matrices of a format 2 one.

struct ot_kern_classes {
  hashset<unsigned int>      coverage;
  hashmap<unsigned int, int> class1, class2;
  array<int>                 values; // class1_count x class2_count
  int                        class1_count, class2_count;
  ot_kern_classes ()
      : class1 (0), class2 (0), class1_count (0), class2_count (0) {}
};

struct ot_gpos_kern_rep : concrete_struct {
  hashmap<unsigned int, int> pairs; // (left << 16) | right -> x advance
  array<ot_kern_classes>     classes;
  ot_gpos_kern_rep () : pairs (0) {}
  bool empty ();
  // horizontal adjustment in design units, zero when the pair is not kerned
  int  get (unsigned int left, unsigned int right);
};

struct ot_gpos_kern {
  CONCRETE_NULL (ot_gpos_kern);
  ot_gpos_kern (ot_gpos_kern_rep* rep2) : rep (rep2) {}
};
CONCRETE_NULL_CODE (ot_gpos_kern);

ot_gpos_kern parse_gpos_kern (const string& buf);
ot_mathtable parse_mathtable (url u);
void dump_mathtable (tm_ostream& str, ot_mathtable table);

#endif // TT_TOOLS_H
