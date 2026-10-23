
/******************************************************************************
* MODULE     : unicode_font.cpp
* DESCRIPTION: True Type fonts (using FreeType II)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "Freetype/free_type.hpp"
#include "Freetype/tt_file.hpp"
#include "Freetype/tt_face.hpp"
#include "Freetype/tt_tools.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "colors.hpp"
#include "picture.hpp"

#ifdef USE_FREETYPE

#define std_dpi 600
#define std_pixel (std_shrinkf*256)
#define ROUND(l) ((l*hdpi+(std_dpi>>1))/std_dpi)
#define FLOOR(l) ((((l*hdpi)/std_dpi)/std_pixel)*std_pixel)
#define CEIL(l) (((((l*hdpi+(std_dpi-1))/std_dpi)+std_pixel-1)/std_pixel)*std_pixel)

#define LIGATURE_FF   1
#define LIGATURE_FI   2
#define LIGATURE_FL   4
#define LIGATURE_FT   8
#define LIGATURE_FFI 16
#define LIGATURE_FFL 32
#define LIGATURE_ST  64

font unicode_font (string family, int size, int hdpi, int vdpi);
font rubber_unicode_font (font base, tt_face face);
inline string get_right (string s);

hashmap<string,double> lsup_guessed_table ();
hashmap<string,double> rsub_guessed_table ();

hashmap<string,double> lsub_stix_table ();
hashmap<string,double> lsup_stix_table ();
hashmap<string,double> rsub_stix_table ();
hashmap<string,double> rsup_stix_table ();
hashmap<string,double> above_stix_table ();
hashmap<string,double> lsub_termes_table ();
hashmap<string,double> lsup_termes_table ();
hashmap<string,double> rsub_termes_table ();
hashmap<string,double> rsup_termes_table ();
hashmap<string,double> above_termes_table ();
hashmap<string,double> lsub_pagella_table ();
hashmap<string,double> lsup_pagella_table ();
hashmap<string,double> rsub_pagella_table ();
hashmap<string,double> rsup_pagella_table ();
hashmap<string,double> above_pagella_table ();
hashmap<string,double> lsub_schola_table ();
hashmap<string,double> lsup_schola_table ();
hashmap<string,double> rsub_schola_table ();
hashmap<string,double> rsup_schola_table ();
hashmap<string,double> above_schola_table ();
hashmap<string,double> lsub_bonum_table ();
hashmap<string,double> lsup_bonum_table ();
hashmap<string,double> rsub_bonum_table ();
hashmap<string,double> rsup_bonum_table ();
hashmap<string,double> above_bonum_table ();
hashmap<string,double> lsub_libertine_table ();
hashmap<string,double> lsup_libertine_table ();
hashmap<string,double> rsub_libertine_table ();
hashmap<string,double> rsup_libertine_table ();
hashmap<string,double> above_libertine_table ();
hashmap<string,double> lsub_biolinum_table ();
hashmap<string,double> lsup_biolinum_table ();
hashmap<string,double> rsub_biolinum_table ();
hashmap<string,double> rsup_biolinum_table ();
hashmap<string,double> above_biolinum_table ();
hashmap<string,double> lsub_fira_table ();
hashmap<string,double> lsup_fira_table ();
hashmap<string,double> rsub_fira_table ();
hashmap<string,double> rsup_fira_table ();
hashmap<string,double> above_fira_table ();

hashmap<string,double> lsub_stix_italic_table ();
hashmap<string,double> lsup_stix_italic_table ();
hashmap<string,double> rsub_stix_italic_table ();
hashmap<string,double> rsup_stix_italic_table ();
hashmap<string,double> above_stix_italic_table ();
hashmap<string,double> lsub_termes_italic_table ();
hashmap<string,double> lsup_termes_italic_table ();
hashmap<string,double> rsub_termes_italic_table ();
hashmap<string,double> rsup_termes_italic_table ();
hashmap<string,double> above_termes_italic_table ();
hashmap<string,double> lsub_pagella_italic_table ();
hashmap<string,double> lsup_pagella_italic_table ();
hashmap<string,double> rsub_pagella_italic_table ();
hashmap<string,double> rsup_pagella_italic_table ();
hashmap<string,double> above_pagella_italic_table ();
hashmap<string,double> lsub_schola_italic_table ();
hashmap<string,double> lsup_schola_italic_table ();
hashmap<string,double> rsub_schola_italic_table ();
hashmap<string,double> rsup_schola_italic_table ();
hashmap<string,double> above_schola_italic_table ();
hashmap<string,double> lsub_bonum_italic_table ();
hashmap<string,double> lsup_bonum_italic_table ();
hashmap<string,double> rsub_bonum_italic_table ();
hashmap<string,double> rsup_bonum_italic_table ();
hashmap<string,double> above_bonum_italic_table ();
hashmap<string,double> lsub_libertine_italic_table ();
hashmap<string,double> lsup_libertine_italic_table ();
hashmap<string,double> rsub_libertine_italic_table ();
hashmap<string,double> rsup_libertine_italic_table ();
hashmap<string,double> above_libertine_italic_table ();
hashmap<string,double> lsub_biolinum_italic_table ();
hashmap<string,double> lsup_biolinum_italic_table ();
hashmap<string,double> rsub_biolinum_italic_table ();
hashmap<string,double> rsup_biolinum_italic_table ();
hashmap<string,double> above_biolinum_italic_table ();
hashmap<string,double> lsub_fira_italic_table ();
hashmap<string,double> lsup_fira_italic_table ();
hashmap<string,double> rsub_fira_italic_table ();
hashmap<string,double> rsup_fira_italic_table ();
hashmap<string,double> above_fira_italic_table ();

/******************************************************************************
* True Type fonts
******************************************************************************/

struct unicode_font_rep: font_rep {
  string      family;
  int         hdpi;
  int         vdpi;
  font_metric fnm;
  font_glyphs fng;
  int         ligs;

  hashmap<string,int> native; // additional native (non unicode) characters
  
  // only for OpenType fonts /////////

  tt_face      ot_face;  // the parsed face: MATH, GSUB and GPOS
  ot_mathtable math_table;

  font make_rubber_font (font base);
  bool get_ot_kerning (string s, SI height, bool top, bool left, SI& kerning);
  bool get_ot_italic_correction (string s, SI& r);
  bool is_ot_integral (string s);
  bool is_extended_shape (string s);
  bool get_top_accent (string s, SI& x);
  bool get_feature_variant (string s, string feature, int alt, string& r);
  void set_glyph_unicode (string s, int code);
  int  source_unicode (string s);
  void init_ot_math (tt_face face);
  hashset<unsigned int> ot_integral;

  double design_unit_to_metric_factor;   // vertical
  double design_unit_to_metric_x_factor; // horizontal
  double metric_to_design_unit_factor;   // vertical
  void   init_design_unit_factor ();
  SI     design_unit_to_metric (int du);   // vertical lengths
  SI     design_unit_to_metric_x (int du); // horizontal lengths
  int    metric_to_design_unit (SI m);     // vertical lengths

  ////////////////////////////

  unicode_font_rep (string name, string family, int size, int hdpi, int vdpi);
  void tex_gyre_operators ();

  unsigned int get_glyphID (string s);
  unsigned int read_unicode_char (string s, int& i);
  unsigned int ligature_replace (unsigned int c, string s, int& i);
  unsigned int read_glyph (string s, int& i, bool ligf);
  unsigned int color_ligature (unsigned int c, string s, int& i);
  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos, bool ligf);
  void   get_xpositions (string s, SI* xpos);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool ligf);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  bool   color_font; // with glyphs in several colors (COLR)
  bool   draw_color_glyph (renderer ren, unsigned int uc, string c,
                           SI x, SI y);
  bool   draw_color_bitmap (renderer ren, unsigned int uc, string c,
                            SI x, SI y);
  int    emoji_marks_end (string s, int i);
  font   magnify (double zoomx, double zoomy);
  void   advance_glyph (string s, int& pos, bool ligf);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
  SI     get_lsub_correction_at (string s, SI h);
  SI     get_lsup_correction_at (string s, SI h);
  SI     get_rsub_correction_at (string s, SI h);
  SI     get_rsup_correction_at (string s, SI h);
  SI     get_wide_correction  (string s, int mode);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

unicode_font_rep::unicode_font_rep (string name,
  string family2, int size2, int hdpi2, int vdpi2):
    font_rep (name), family (family2), hdpi (hdpi2), vdpi (vdpi2), ligs (0),
    native (0), color_font (false)
{
  type= FONT_TYPE_UNICODE;
  size= size2;
  fnm = tt_font_metric (family, size, std_dpi, (std_dpi * vdpi) / hdpi);
  fng = tt_font_glyphs (family, size, hdpi, vdpi);
  color_font= tt_color_font (fng);
  if (fnm->bad_font_metric || fng->bad_font_glyphs) {
    fnm= std_font_metric (res_name, NULL, 0, -1);
    fng= std_font_glyphs (res_name, NULL, 0, -1);
    if (DEBUG_AUTO)
      debug_fonts << "TeXmacs] Font " << family << " " << size << "pt "
                  << "at " << hdpi << " dpi could not be loaded\n";
    
  }

  // get main font parameters
  metric ex;
  get_extents ("f", ex);
  y1= ex->y1;
  y2= ex->y2;
  get_extents ("p", ex);
  y1= min (y1, ex->y1);
  y2= max (y2, ex->y2);
  get_extents ("d", ex);
  y1= min (y1, ex->y1);
  y2= max (y2, ex->y2);
  display_size = y2-y1;
  design_size  = size << 8;

  // get character dimensions
  get_extents ("x", ex);
  yx           = ex->y2;
  get_extents ("M", ex);
  wquad        = ex->x2;

  // compute other heights
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  // compute other widths
  wpt          = (hdpi*PIXEL)/72;
  hpt          = (vdpi*PIXEL)/72;
  wfn          = (wpt*design_size) >> 8;
  wline        = wfn/20;

  // get fraction bar parameters; reasonable compromise between several fonts
  if (supports ("<#2212>")) get_extents ("<#2212>", ex);
  else if (supports ("+")) get_extents ("+", ex);
  else if (supports ("-")) get_extents ("-", ex);
  else get_extents ("x", ex);
  yfrac= (ex->y1 + ex->y2) >> 1;
  if (supports ("<#2212>") || supports ("+") || supports ("-")) {
    wline= ex->y2 - ex->y1;
    if (supports ("<#2212>"));
    else if (supports ("<#2013>")) {
      get_extents ("<#2013>", ex);
      wline= min (wline, ex->y2 - ex->y1);
    }
    wline= max (min (wline, wfn/8), wfn/48);
    if (!supports ("<#2212>")) yfrac += wline/4;
  }
  if (starts (res_name, "unicode:Papyrus.")) wline= (2*wline)/3;

  // get space length
  get_extents (" ", ex);
  spc  = space ((3*(ex->x2-ex->x1))>>2, ex->x2-ex->x1, (3*(ex->x2-ex->x1))>>1);
  extra= spc/2;
  mspc = spc;
  sep  = wfn/10;

  // get_italic space
  get_extents ("f", ex);
  SI italic_spc= (ex->x4-ex->x3)-(ex->x2-ex->x1);
  slope= ((double) italic_spc) / ((double) display_size) - 0.05;
  if (slope<0.15) slope= 0.0;

  // determine whether we are dealing with a monospaced font
  get_extents ("m", ex);
  SI em= ex->x2 - ex->x1;
  get_extents ("i", ex);
  SI ei= ex->x2 - ex->x1;
  bool mono= (em == ei);
  // the space of a monospaced font takes one cell, also when the font
  // gives it another width (KpMono has a space of 333 for cells of 530)
  if (mono && em > 0 && em != (spc->def)) {
    spc  = space ((3*em)>>2, em, (3*em)>>1);
    extra= spc/2;
    mspc = spc;
  }

  // available standard ligatures
  if (!mono) {
    if (fnm->exists (0xfb00)) ligs += LIGATURE_FF;
    if (fnm->exists (0xfb01)) ligs += LIGATURE_FI;
    if (fnm->exists (0xfb02)) ligs += LIGATURE_FL;
    if (fnm->exists (0xfb03)) ligs += LIGATURE_FFI;
    if (fnm->exists (0xfb04)) ligs += LIGATURE_FFL;
    if (fnm->exists (0xfb05)) ligs += LIGATURE_FT;
    //if (fnm->exists (0xfb06)) ligs += LIGATURE_ST;
  }
  if (family == "Times New Roman")
    ligs= LIGATURE_FI + LIGATURE_FL;
  if (family == "Zapfino")
    ligs= LIGATURE_FF + LIGATURE_FI + LIGATURE_FL + LIGATURE_FFI;
  //cout << "ligs= " << ligs << ", " << family << ", " << size << "\n";

  // direct translations for certain characters without Unicode names; the
  // glyph numbers are those of the four hand-tuned TeX Gyre math fonts,
  // not those of TeX Gyre DejaVu Math, which has no tuning of its own
  bool tuned_gyre=
    starts (family, "texgyretermes-") || starts (family, "texgyrepagella-") ||
    starts (family, "texgyreschola-") || starts (family, "texgyrebonum-");
  if (tuned_gyre && ends (family, "-math"))
    tex_gyre_operators ();

  // Fonts with an OpenType MATH table are typeset from the table; the
  // hand-tuned customizations below take precedence over the table when
  // both exist, unless the user switched them off (preference
  // "hand tuned math fonts") to compare with the table-only result.
  this->ot_face= tt_face (family);
  bool    has_ot  = !is_nil (ot_face) && !is_nil (ot_face->math_table);
  bool    tuned   = hand_tuned_math_fonts || !has_ot;
  // a font which is being hand tuned is typeset as if it had no MATH table
  bool    by_hand = tuned &&
    (starts (family, "STIX-") || tuned_gyre || starts (family, "Papyrus") ||
     starts (family, "LinLibertine") || starts (family, "LinBiolinum") ||
     starts (family, "FiraSans"));
  if (has_ot && !by_hand) init_ot_math (ot_face);

  if (tuned && starts (family, "STIX-")) {
    if (!ends (family, "italic")) {
      global_rsub_correct= (SI) (0.04 * wfn);
      global_rsup_correct= (SI) (0.04 * wfn);
      lsub_correct= lsub_stix_table ();
      lsup_correct= lsup_stix_table ();
      rsub_correct= rsub_stix_table ();
      rsup_correct= rsup_stix_table ();
      above_correct= above_stix_table ();
    }
    else {
      global_rsub_correct= (SI) (0.04 * wfn);
      global_rsup_correct= (SI) (0.04 * wfn);
      lsub_correct= lsub_stix_italic_table ();
      lsup_correct= lsup_stix_italic_table ();
      rsub_correct= rsub_stix_italic_table ();
      rsup_correct= rsup_stix_italic_table ();
      above_correct= above_stix_italic_table ();
    }
  }

  else if (tuned && tuned_gyre) {
    if (!ends (family, "italic")) {
      if (starts (family, "texgyretermes-")) {
        global_rsup_correct= (SI) (0.04 * wfn);
        lsub_correct= lsub_termes_table ();
        lsup_correct= lsup_termes_table ();
        rsub_correct= rsub_termes_table ();
        rsup_correct= rsup_termes_table ();
        above_correct= above_termes_table ();
      }
      else if (starts (family, "texgyrepagella-")) {
        global_rsub_correct= (SI) (0.03 * wfn);
        global_rsup_correct= (SI) (0.03 * wfn);
        lsub_correct= lsub_pagella_table ();
        lsup_correct= lsup_pagella_table ();
        rsub_correct= rsub_pagella_table ();
        rsup_correct= rsup_pagella_table ();
        above_correct= above_pagella_table ();
      }
      else if (starts (family, "texgyreschola-")) {
        lsub_correct= lsub_schola_table ();
        lsup_correct= lsup_schola_table ();
        rsub_correct= rsub_schola_table ();
        rsup_correct= rsup_schola_table ();
        above_correct= above_schola_table ();
      }
      else if (starts (family, "texgyrebonum-")) {
        lsub_correct= lsub_bonum_table ();
        lsup_correct= lsup_bonum_table ();
        rsub_correct= rsub_bonum_table ();
        rsup_correct= rsup_bonum_table ();
        above_correct= above_bonum_table ();
      }
    }
    else {
      if (starts (family, "texgyretermes-")) {
        global_rsup_correct= (SI) (0.04 * wfn);
        lsub_correct= lsub_termes_italic_table ();
        lsup_correct= lsup_termes_italic_table ();
        rsub_correct= rsub_termes_italic_table ();
        rsup_correct= rsup_termes_italic_table ();
        above_correct= above_termes_italic_table ();
      }
      else if (starts (family, "texgyrepagella-")) {
        global_rsub_correct= (SI) (0.03 * wfn);
        global_rsup_correct= (SI) (0.03 * wfn);
        lsub_correct= lsub_pagella_italic_table ();
        lsup_correct= lsup_pagella_italic_table ();
        rsub_correct= rsub_pagella_italic_table ();
        rsup_correct= rsup_pagella_italic_table ();
        above_correct= above_pagella_italic_table ();
      }
      else if (starts (family, "texgyreschola-")) {
        lsub_correct= lsub_schola_italic_table ();
        lsup_correct= lsup_schola_italic_table ();
        rsub_correct= rsub_schola_italic_table ();
        rsup_correct= rsup_schola_italic_table ();
        above_correct= above_schola_italic_table ();
      }
      else if (starts (family, "texgyrebonum-")) {
        lsub_correct= lsub_bonum_italic_table ();
        lsup_correct= lsup_bonum_italic_table ();
        rsub_correct= rsub_bonum_italic_table ();
        rsup_correct= rsup_bonum_italic_table ();
        above_correct= above_bonum_italic_table ();
      }
    }
    if (starts (family, "texgyrepagella-")) {
      mspc= spc + 0.5 * space (spc->def);
      double_bracket_correct= -wquad >> 3;
    }
  }

  else if (tuned && starts (family, "Papyrus")) {
    lsup_correct= copy (lsup_guessed_table ());
    rsub_correct= copy (rsub_guessed_table ());
    adjust_integral (lsup_correct, "1", -0.15);
    adjust_integral (lsup_correct, "2", -0.15);
    adjust_integral (rsub_correct, "1", 0.15);
    adjust_integral (rsub_correct, "2", 0.15);
  }

  else if (tuned && starts (family, "LinLibertine")) {
    if (!ends (family, "I")) {
      lsub_correct= lsub_libertine_table ();
      lsup_correct= lsup_libertine_table ();
      rsub_correct= rsub_libertine_table ();
      rsup_correct= rsup_libertine_table ();
      above_correct= above_libertine_table ();
    }
    else {
      lsub_correct= lsub_libertine_italic_table ();
      lsup_correct= lsup_libertine_italic_table ();
      rsub_correct= rsub_libertine_italic_table ();
      rsup_correct= rsup_libertine_italic_table ();
      above_correct= above_libertine_italic_table ();
    }
    if (starts (family, "LinLibertine_a")) ligs= 0;
  }
  else if (tuned && starts (family, "LinBiolinum")) {
    if (!ends (family, "I")) {
      lsub_correct= lsub_biolinum_table ();
      lsup_correct= lsup_biolinum_table ();
      rsub_correct= rsub_biolinum_table ();
      rsup_correct= rsup_biolinum_table ();
      above_correct= above_biolinum_table ();
    }
    else {
      lsub_correct= lsub_biolinum_italic_table ();
      lsup_correct= lsup_biolinum_italic_table ();
      rsub_correct= rsub_biolinum_italic_table ();
      rsup_correct= rsup_biolinum_italic_table ();
      above_correct= above_biolinum_italic_table ();
    }
    if (starts (family, "LinBiolinum_a")) ligs= 0;
  }
  else if (tuned && starts (family, "FiraSans")) {
    if (!ends (family, "Italic")) {
      lsub_correct= lsub_fira_table ();
      lsup_correct= lsup_fira_table ();
      rsub_correct= rsub_fira_table ();
      rsup_correct= rsup_fira_table ();
      above_correct= above_fira_table ();
    }
    else {
      lsub_correct= lsub_fira_italic_table ();
      lsup_correct= lsup_fira_italic_table ();
      rsub_correct= rsub_fira_italic_table ();
      rsup_correct= rsup_fira_italic_table ();
      above_correct= above_fira_italic_table ();
    }
  }
  else if (has_ot && !by_hand) math_type= MATH_TYPE_OPENTYPE;
}

// Load the layout parameters of an OpenType MATH table. This runs before
// the hand-tuned per-family customizations of the constructor, which may
// override any of these values.
void
unicode_font_rep::init_ot_math (tt_face face) {
  this->ot_face   = face;
  this->math_table= face->math_table;
  ot_math= true;
  init_design_unit_factor ();
  MathConstantsTable& mc= math_table->constants_table;
  // general parameters: math axis, rule thickness, script placement
  if (mc[axisHeight] > 0)
    yfrac= design_unit_to_metric (mc[axisHeight]);
  if (mc[fractionRuleThickness] > 0)
    wline= design_unit_to_metric (mc[fractionRuleThickness]);
  if (mc[superscriptShiftUp] > 0 && mc[subscriptShiftDown] > 0) {
    ysub_lo_base= -design_unit_to_metric (mc[subscriptShiftDown]);
    ysub_hi_lim = design_unit_to_metric (mc[subscriptTopMax]);
    ysup_lo_lim = design_unit_to_metric (mc[superscriptBottomMin]);
    ysup_lo_base= design_unit_to_metric (mc[superscriptShiftUp]);
    ysup_hi_lim = max (ysup_lo_base, yx);
    yshift      = design_unit_to_metric (mc[superscriptShiftUp] -
                                         mc[superscriptShiftUpCramped]);
  }
  // limit boxes
  upper_limit_gap_min=
      design_unit_to_metric (math_table->constants_table[upperLimitGapMin]);
  upper_limit_baseline_rise_min= design_unit_to_metric (
      math_table->constants_table[upperLimitBaselineRiseMin]);
  lower_limit_gap_min=
      design_unit_to_metric (math_table->constants_table[lowerLimitGapMin]);
  lower_limit_baseline_drop_min= design_unit_to_metric (
      math_table->constants_table[lowerLimitBaselineDropMin]);
  // labels above and below a stretched glyph (long arrows, wide braces)
  stretch_stack_top_shift_up= design_unit_to_metric (
      math_table->constants_table[stretchStackTopShiftUp]);
  stretch_stack_bottom_shift_down= design_unit_to_metric (
      math_table->constants_table[stretchStackBottomShiftDown]);
  stretch_stack_gap_above_min= design_unit_to_metric (
      math_table->constants_table[stretchStackGapAboveMin]);
  stretch_stack_gap_below_min= design_unit_to_metric (
      math_table->constants_table[stretchStackGapBelowMin]);
  // frac boxes
  frac_rule_thickness= design_unit_to_metric (
      math_table->constants_table[fractionRuleThickness]);
  frac_num_shift_up= design_unit_to_metric (
      math_table->constants_table[fractionNumeratorShiftUp]);
  frac_num_disp_shift_up= design_unit_to_metric (
      math_table->constants_table[fractionNumeratorDisplayStyleShiftUp]);
  frac_num_gap_min= design_unit_to_metric (
      math_table->constants_table[fractionNumeratorGapMin]);
  frac_num_disp_gap_min= design_unit_to_metric (
      math_table->constants_table[fractionNumDisplayStyleGapMin]);
  frac_denom_shift_down= design_unit_to_metric (
      math_table->constants_table[fractionDenominatorShiftDown]);
  frac_denom_disp_shift_down= design_unit_to_metric (
      math_table
          ->constants_table[fractionDenominatorDisplayStyleShiftDown]);
  frac_denom_gap_min= design_unit_to_metric (
      math_table->constants_table[fractionDenominatorGapMin]);
  frac_denom_disp_gap_min= design_unit_to_metric (
      math_table->constants_table[fractionDenomDisplayStyleGapMin]);
  // sqrt boxes
  sqrt_ver_gap= design_unit_to_metric (
      math_table->constants_table[radicalVerticalGap]);
  sqrt_ver_disp_gap= design_unit_to_metric (
      math_table->constants_table[radicalDisplayStyleVerticalGap]);
  sqrt_rule_thickness= design_unit_to_metric (
      math_table->constants_table[radicalRuleThickness]);
  sqrt_extra_ascender= design_unit_to_metric (
      math_table->constants_table[radicalExtraAscender]);
  sqrt_degree_rise_percent=
      math_table->constants_table[radicalDegreeBottomRaisePercent];
  sqrt_kern_before_degree= design_unit_to_metric (
      math_table->constants_table[radicalKernBeforeDegree]);
  sqrt_kern_after_degree= design_unit_to_metric (
      math_table->constants_table[radicalKernAfterDegree]);
  // scripts
  sub_sup_gap_min= design_unit_to_metric (mc[subSuperscriptGapMin]);
  sup_drop_max   = design_unit_to_metric (mc[superscriptBaselineDropMax]);
  sub_drop_min   = design_unit_to_metric (mc[subscriptBaselineDropMin]);
  sup_bottom_max_with_sub=
      design_unit_to_metric (mc[superscriptBottomMaxWithSubscript]);
  space_after_script= design_unit_to_metric_x (mc[spaceAfterScript]);
  script_percent       = mc[scriptPercentScaleDown];
  script_script_percent= mc[scriptScriptPercentScaleDown];
  // accents
  accent_base_height= design_unit_to_metric (mc[accentBaseHeight]);
  flattened_accent_base_height=
      design_unit_to_metric (mc[flattenedAccentBaseHeight]);
  // over- and underlines
  overbar_vertical_gap    = design_unit_to_metric (mc[overbarVerticalGap]);
  overbar_rule_thickness  = design_unit_to_metric (mc[overbarRuleThickness]);
  overbar_extra_ascender  = design_unit_to_metric (mc[overbarExtraAscender]);
  underbar_vertical_gap   = design_unit_to_metric (mc[underbarVerticalGap]);
  underbar_rule_thickness = design_unit_to_metric (mc[underbarRuleThickness]);
  underbar_extra_descender= design_unit_to_metric (mc[underbarExtraDescender]);
}

/******************************************************************************
* Big operators in TeX Gyre fonts
******************************************************************************/

static void
bracket (hashmap<string,int>& h, string c, int n1, int n2, int im, int d) {
  for (int n= n1; n <= n2; n++, im += d) {
    string s= c * "-" * as_string (n) * ">";
    h ("<large-" * s)= im;
    h ("<left-" * s)= im;
    h ("<mid-" * s)= im;
    h ("<right-" * s)= im;
  }
}

static void
wide (hashmap<string,int>& h, string c, int n1, int n2, int im, int d) {
  for (int n= n1; n <= n2; n++, im += d) {
    string s= c * "-" * as_string (n) * ">";
    h ("<wide-" * s)= im;
  }
}

static hashmap<string,int>
tex_gyre_native () {
  static hashmap<string,int> native;
  if (N(native) != 0) return native;
  native ("<big-prod-2>")= 4215;
  native ("<big-amalg-2>")= 4216;
  native ("<big-sum-2>")= 4217;
  native ("<big-int-2>")= 4149;
  native ("<big-iint-2>")= 4150;
  native ("<big-iiint-2>")= 4151;
  native ("<big-iiiint-2>")= 4152;
  native ("<big-oint-2>")= 4153;
  native ("<big-oiint-2>")= 4154;
  native ("<big-oiiint-2>")= 4155;
  native ("<big-wedge-2>")= 3833;
  native ("<big-vee-2>")= 3835;
  native ("<big-cap-2>")= 3827;
  native ("<big-cup-2>")= 3829;
  native ("<big-odot-2>")= 3864;
  native ("<big-oplus-2>")= 3868;
  native ("<big-otimes-2>")= 3873;
  native ("<big-pluscup-2>")= 3861;
  native ("<big-sqcap-2>")= 3852;
  native ("<big-sqcup-2>")= 3854;
  native ("<big-intlim-2>")= 4149;
  native ("<big-iintlim-2>")= 4150;
  native ("<big-iiintlim-2>")= 4151;
  native ("<big-iiiintlim-2>")= 4152;
  native ("<big-ointlim-2>")= 4153;
  native ("<big-oiintlim-2>")= 4154;
  native ("<big-oiiintlim-2>")= 4155;
  native ("<big-upint-2>")= 4149;
  native ("<big-upiint-2>")= 4150;
  native ("<big-upiiint-2>")= 4151;
  native ("<big-upiiiint-2>")= 4152;
  native ("<big-upoint-2>")= 4153;
  native ("<big-upoiint-2>")= 4154;
  native ("<big-upoiiint-2>")= 4155;
  native ("<big-upintlim-2>")= 4149;
  native ("<big-upiintlim-2>")= 4150;
  native ("<big-upiiintlim-2>")= 4151;
  native ("<big-upiiiintlim-2>")= 4152;
  native ("<big-upointlim-2>")= 4153;
  native ("<big-upoiintlim-2>")= 4154;
  native ("<big-upoiiintlim-2>")= 4155;

  native ("<large-sqrt-1>")= 4136;
  native ("<large-sqrt-2>")= 4148;
  native ("<large-sqrt-3>")= 4160;
  native ("<large-sqrt-4>")= 4172;
  native ("<large-sqrt-5>")= 4184;
  native ("<large-sqrt-6>")= 4196;

  bracket (native, "(", 1, 5, 3461, 22);
  bracket (native, ")", 1, 5, 3462, 22);
  bracket (native, "{", 1, 5, 3465, 22);
  bracket (native, "}", 1, 5, 3466, 22);
  bracket (native, "[", 1, 5, 3467, 22);
  bracket (native, "]", 1, 5, 3468, 22);
  bracket (native, "lceil", 1, 5, 3469, 22);
  bracket (native, "rceil", 1, 5, 3470, 22);
  bracket (native, "lfloor", 1, 5, 3471, 22);
  bracket (native, "rfloor", 1, 5, 3472, 22);
  bracket (native, "llbracket", 1, 5, 3473, 22);
  bracket (native, "rrbracket", 1, 5, 3474, 22);
  bracket (native, "langle", 1, 6, 3655, 4);
  bracket (native, "rangle", 1, 6, 3656, 4);
  bracket (native, "llangle", 1, 6, 3657, 4);
  bracket (native, "rrangle", 1, 6, 3658, 4);
  bracket (native, "/", 1, 6, 3742, 7);
  bracket (native, "\\", 1, 6, 3743, 7);
  bracket (native, "|", 1, 6, 3745, 7);
  bracket (native, "||", 1, 6, 3746, 7);

  native ("<wide-hat-0>")= 125;
  native ("<wide-tilde-0>")= 126;
  native ("<wide-breve-0>")= 128;
  native ("<wide-check-0>")= 135;
  wide (native, "breve", 1, 6, 3378, 10);
  wide (native, "invbreve", 1, 6, 3380, 10);
  wide (native, "check", 1, 6, 3382, 10);
  wide (native, "hat", 1, 6, 3384, 10);
  wide (native, "tilde", 1, 6, 3386, 10);
  wide (native, "overbrace", 0, 5, 3453, 22);
  wide (native, "overbrace*", 0, 5, 3453, 22);
  wide (native, "underbrace", 0, 5, 3454, 22);
  wide (native, "underbrace*", 0, 5, 3454, 22);
  wide (native, "poverbrace", 0, 5, 3455, 22);
  wide (native, "poverbrace*", 0, 5, 3455, 22);
  wide (native, "punderbrace", 0, 5, 3456, 22);
  wide (native, "punderbrace*", 0, 5, 3456, 22);
  wide (native, "sqoverbrace", 0, 5, 3457, 22);
  wide (native, "sqoverbrace*", 0, 5, 3457, 22);
  wide (native, "squnderbrace", 0, 5, 3458, 22);
  wide (native, "squnderbrace*", 0, 5, 3458, 22);
  return native;
}

void
unicode_font_rep::tex_gyre_operators () {
  native= tex_gyre_native ();
}

/******************************************************************************
* Routines for font
******************************************************************************/

unsigned int
unicode_font_rep::read_unicode_char (string s, int& i) {
  if (s[i] == '<') {
    i++;
    int start= i, n= N(s);
    while (true) {
      if (i == n) {
	i= start;
	return (int) '<';
      }
      if (s[i] == '>') break;
      i++;
    }
    if (s[start] == '#') {
      start++;
      return (unsigned int) from_hexadecimal (s (start, i++));
    }
    else if (s[start] == '@') {
      // <@XXXX> are native glyph ids generated by rubber_unicode_font
      start++;
      return 0xc000000 + (unsigned int) from_hexadecimal (s (start, i++));
    }
    else {
      string ss= s (start-1, ++i);
      string uu= strict_cork_to_utf8 (ss);
      if (uu == ss) {
        if (native->contains (ss)) return 0xc000000 + native[ss];
        return 0;
      }
      int j= 0;
      return decode_from_utf8 (uu, j);
    }
  }
  else {
    unsigned int c= (unsigned int) s[i++];
    if (c >= 32 && c <= 127) return c;
    string ss= s (i-1, i);
    string uu= strict_cork_to_utf8 (ss);
    int j= 0;
    return decode_from_utf8 (uu, j);
  }
}

// The glyph which starts at i, with the ligatures of the font: the usual
// ones (fi, fl...) and, in a color font, the sequences of emoji (joined by
// U+200D, with variation selectors, skin tones, flags, keycaps), which are
// ligatures of its ccmp feature
unsigned int
unicode_font_rep::read_glyph (string s, int& i, bool ligf) {
  int start= i;
  unsigned int uc= read_unicode_char (s, i);
  if (ligs > 0 && ligf && (uc == 'f' || uc == 's'))
    return ligature_replace (uc, s, i);
  if (color_font && i < N(s)) {
    unsigned int lig= color_ligature (uc, s, i);
    if (lig != uc) return lig;
    // the fonts of Apple have their ligatures in morx, not in GSUB: the
    // whole sequence of emoji which starts here
    int end= tm_grapheme_end (s, start);
    if (end > i && !is_nil (ot_face)) {
      array<unsigned int> codes;
      for (int j= start; j < end; ) codes << read_unicode_char (s, j);
      int g= ot_face->morx_sequence (codes);
      if (g > 0) { i= end; return 0xc000000 + (unsigned int) g; }
    }
  }
  return uc;
}

static unsigned int
glyph_index_of (FT_Face face, unsigned int uc) {
  if (uc >= 0xc000000) return uc - 0xc000000;
  return ft_get_char_index (face, uc);
}

unsigned int
unicode_font_rep::color_ligature (unsigned int uc, string s, int& i) {
  if (is_nil (ot_face) || ot_face->bad_face) return uc;
  FT_Face face= ot_face->ft_face;
  ot_gsub_ligatures& m= ot_face->gsub_ligatures ("ccmp");
  unsigned int g= glyph_index_of (face, uc);
  if (g == 0 || !m->contains (g)) return uc;
  array<array<unsigned int> >& ligs= m (g);
  for (int k= 0; k < N(ligs); k++) {
    array<unsigned int>& lig= ligs[k];
    int  j = i;
    bool ok= true;
    for (int c= 1; c < N(lig) && ok; c++) {
      if (j >= N(s)) { ok= false; break; }
      unsigned int u= read_unicode_char (s, j);
      unsigned int gu= glyph_index_of (face, u);
      // a variation selector which the ligature does not list (the keycaps
      // of Noto, whose U+FE0F other lookups take away) is passed over
      if (gu != lig[c] && u >= 0xfe00 && u <= 0xfe0f) { c--; continue; }
      ok= (gu == lig[c]);
    }
    if (ok) { i= j; return 0xc000000 + lig[0]; }
  }
  return uc;
}

unsigned int
unicode_font_rep::ligature_replace (unsigned int uc, string s, int& i) {
  int n= N(s);
  if (uc == 'f') {
    if (i<n && s[i] == 'i' && (ligs & LIGATURE_FI) != 0) {
      i++; return 0xfb01; }
    else if (i<n && s[i] == 'l' && (ligs & LIGATURE_FL) != 0) {
      i++; return 0xfb02; }
    else if (i<n && s[i] == 't' && (ligs & LIGATURE_FT) != 0) {
      i++; return 0xfb05; }
    else if ((i+1)<n && s[i] == 'f' && s[i+1] == 'i' &&
             (ligs & LIGATURE_FFI) != 0) {
      i+=2; return 0xfb03; }
    else if ((i+1)<n && s[i] == 'f' && s[i+1] == 'l' &&
             (ligs & LIGATURE_FFL) != 0) {
      i+=2; return 0xfb04; }
    else if (i<n && s[i] == 'f' && (ligs & LIGATURE_FF) != 0) {
      i++; return 0xfb00; }
    else return uc;
  }
  else if (uc == 's') {
    if (i<n && s[i] == 't' && (ligs & LIGATURE_ST) != 0) {
      i++; return 0xfb06; }
    else return uc;
  }
  else return uc;
}

bool
unicode_font_rep::supports (string c) {
  if (N(c) == 0) return false;
  int i= 0;
  unsigned int uc= read_unicode_char (c, i);
  if (uc == 0 || !fnm->exists (uc)) return false;
  // a character of a color font which draws nothing alone (a regional
  // indicator of Apple, which only its flags draw) is still the font's
  if (color_font && uc < 0xc000000 && uc >= 0x80) return true;
  if (uc >= 0x42 && uc <= 0x5a && !fnm->exists (0x41)) return false;
  if (uc >= 0x62 && uc <= 0x7a && !fnm->exists (0x61)) return false;
  metric_struct* m= fnm->get (uc);
  // native glyphs (<@XXXX>) may be combining marks without advance
  if (uc >= 0xc000000) return m->x3 < m->x4 && m->y3 < m->y4;
  return m->x1 < m->x2 && m->y1 < m->y2;
}

void
unicode_font_rep::get_extents (string s, metric& ex) {
  if (N(s)==0) {
    ex->x1= ex->x3= ex->x2= ex->x4=0;
    ex->y3= ex->y1= 0; ex->y4= ex->y2= yx;
  }
  else {
    int i= 0, n= N(s);
    unsigned int uc= read_glyph (s, i, true);
    metric_struct* first= fnm->get (uc);
    ex->x1= ROUND (first->x1);
    ex->y1= ROUND (first->y1);
    ex->x2= ROUND (first->x2);
    ex->y2= ROUND (first->y2);
    ex->x3= FLOOR (first->x3);
    ex->y3= FLOOR (first->y3);
    ex->x4= CEIL  (first->x4);
    ex->y4= CEIL  (first->y4);
    SI x= ROUND (first->x2);

    while (i<n) {
      unsigned int pc= uc;
      uc= read_glyph (s, i, true);
      x += ROUND (fnm->kerning (pc, uc));
      metric_struct* next= fnm->get (uc);
      ex->x1= min (ex->x1, x+ ROUND (next->x1));
      ex->y1= min (ex->y1, ROUND (next->y1));
      ex->x2= max (ex->x2, x+ ROUND (next->x2));
      ex->y2= max (ex->y2, ROUND (next->y2));
      ex->x3= min (ex->x3, x+ FLOOR (next->x3));
      ex->y3= min (ex->y3, FLOOR (next->y3));
      ex->x4= max (ex->x4, x+ CEIL (next->x4));
      ex->y4= max (ex->y4, CEIL (next->y4));
      x += ROUND (next->x2);
      //if (fnm->kerning (pc, uc) != 0)
      //cout << "Kerning " << ((char) pc) << ((char) uc) << " " << ROUND (fnm->kerning (pc, uc)) << ", " << ROUND (next->x2) << "\n";
    }
  }
}

void
unicode_font_rep::get_xpositions (string s, SI* xpos, bool ligf) {
  int i= 0, n= N(s);
  if (n == 0) { xpos[0]= 0; return; }

  SI x= 0;
  unsigned int uc= 0xffffffff;
  while (i<n) {
    int start= i;
    unsigned int pc= uc;
    uc= read_glyph (s, i, ligf);
    if (pc != 0xffffffff) x += ROUND (fnm->kerning (pc, uc));
    metric_struct* next= fnm->get (uc);
    for (int j= start; j<i; j++) xpos[j]= x;
    x += ROUND (next->x2);
    //if (fnm->kerning (pc, uc) != 0)
    //cout << "Kerning " << ((char) pc) << ((char) uc) << " " << ROUND (fnm->kerning (pc, uc)) << ", " << ROUND (next->x2) << "\n";
  }
  xpos[n]= x;
}

void
unicode_font_rep::get_xpositions (string s, SI* xpos) {
  get_xpositions (s, xpos, true);
}

// The characters which modify the emoji before them: the variation
// selectors, the zero width joiner, the keycap and the tags
static bool
is_emoji_mark (unsigned int c) {
  return (c >= 0xfe00 && c <= 0xfe0f) || c == 0x200d || c == 0x20e3 ||
         (c >= 0xe0020 && c <= 0xe007f);
}

// A glyph of a font of bitmaps in color (sbix, CBDT: the emoji of Apple,
// Noto, Android) as a picture: the bitmap of the fixed size of the font,
// averaged down to the pixels of the renderer (cached), and drawn as the
// renderers draw pictures, each in its own way; a PDF keeps it as an image.
// The text of a PDF as for the layers (draw_color_glyph): the picture
// stands for c, and the characters are invisible glyphs of the shipped
// emoji font, which a PDF can embed (a font of bitmaps cannot be: Apple
// Color Emoji has 190 MB).

#define SHIPPED_EMOJI_FONT "TwemojiMozilla"

static hashmap<string,picture> color_bitmap_pictures;

static picture
color_bitmap_picture (string bgra, int w, int h, int pw, int ph) {
  picture pic= raster_picture (pw, ph, 0, 0);
  double sx= ((double) w) / pw, sy= ((double) h) / ph;
  for (int Y= 0; Y < ph; Y++)
    for (int X= 0; X < pw; X++) {
      // the mean of the pixels of the bitmap under the pixel of the
      // picture (premultiplied, then divided again)
      int x1= (int) (X * sx), x2= max (x1 + 1, (int) ((X + 1) * sx));
      int y1= (int) (Y * sy), y2= max (y1 + 1, (int) ((Y + 1) * sy));
      x2= min (x2, w); y2= min (y2, h);
      double b= 0, g= 0, r= 0, a= 0;
      int n= 0;
      for (int y= y1; y < y2; y++)
        for (int x= x1; x < x2; x++) {
          const char* p= &bgra[4 * (y * w + x)];
          b += (unsigned char) p[0]; g += (unsigned char) p[1];
          r += (unsigned char) p[2]; a += (unsigned char) p[3];
          n++;
        }
      // (the rows of a picture go upwards, those of the bitmap downwards)
      int PY= ph - 1 - Y;
      if (n == 0 || a == 0) { pic->set_pixel (X, PY, rgb_color (0, 0, 0, 0)); continue; }
      int A= (int) (a / n + 0.5);
      int R= min (255, (int) (255.0 * r / a + 0.5));
      int G= min (255, (int) (255.0 * g / a + 0.5));
      int B= min (255, (int) (255.0 * b / a + 0.5));
      pic->set_pixel (X, PY, rgb_color (R, G, B, A));
    }
  return pic;
}

bool
unicode_font_rep::draw_color_bitmap (renderer ren, unsigned int uc, string c,
                                     SI x, SI y) {
  string bgra;
  int w, h, left, top, adv;
  if (!tt_color_bitmap (fng, uc, bgra, w, h, left, top, adv) ||
      w <= 0 || h <= 0 || adv <= 0) return false;
  // a pixel of the bitmap, in the units of the renderer, and in its pixels
  SI W= ROUND (fnm->get (uc)->x2);
  double k = ((double) W) / adv;
  double px= k / max (ren->pixel, 1);
  int pw= max (1, (int) (w * px + 0.5)), ph= max (1, (int) (h * px + 0.5));
  string key= fng->res_name * "#" * as_string ((int) uc) * "@" *
              as_string (pw) * "x" * as_string (ph);
  if (!color_bitmap_pictures->contains (key))
    color_bitmap_pictures (key)= color_bitmap_picture (bgra, w, h, pw, ph);
  picture pic= color_bitmap_pictures [key];
  // the characters, as invisible glyphs of the shipped emoji font
  if (tt_font_exists (SHIPPED_EMOJI_FONT)) {
    font_glyphs  tg= tt_font_glyphs (SHIPPED_EMOJI_FONT, size, hdpi, vdpi);
    array<unsigned int> chars;
    for (int i= 0; i < N(c); ) chars << read_unicode_char (c, i);
    int n= N(chars);
    // the advance of an emoji of the shipped font, which is one em
    int em= tt_glyph_units_advance (tg, 0x1F600);
    SI  E = (SI) (((double) size) * hdpi / 72.0 * PIXEL);
    ren->begin_actual_text ("");
    for (int j= 0; j < n && em > 0 && n > 0; j++) {
      int a= tt_glyph_units_advance (tg, chars[j]);
      double hscale= (a > 0)? (((double) W) / n) / (E * a / ((double) em)): 1.0;
      ren->draw_invisible (chars[j], tg, x + (j * W) / n, y, hscale);
    }
    ren->end_actual_text ();
  }
  ren->begin_actual_text (c);
  ren->draw_picture (pic, x + (SI) (left * k), y + (SI) ((top - h) * k));
  ren->end_actual_text ();
  return true;
}

// The end of the marks of emoji which follow i, which are not drawn (like
// the U+FE0F of a heart in color): the characters of the emoji before them
// in the text of a PDF
int
unicode_font_rep::emoji_marks_end (string s, int i) {
  while (i < N(s)) {
    int j= i;
    if (!is_emoji_mark (read_unicode_char (s, j))) break;
    i= j;
  }
  return i;
}

void
unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool ligf) {
  int i= 0, n= N(s);
  unsigned int uc= 0xffffffff;
  while (i<n) {
    unsigned int pc= uc;
    int start= i;
    uc= read_glyph (s, i, ligf);
    if (pc != 0xffffffff) x += ROUND (fnm->kerning (pc, uc));
    if (!color_font) ren->draw (uc, fng, x, y);
    else if (!draw_color_glyph (ren, uc, s (start, emoji_marks_end (s, i)),
                                x, y) &&
             !is_emoji_mark (uc))
      // (what is left of a sequence of emoji is not drawn: the font may
      // not even have it, and would draw the box of a missing glyph)
      ren->draw (uc, fng, x, y);
    metric_struct* ex= fnm->get (uc);
    x += ROUND (ex->x2);
    //if (fnm->kerning (pc, uc) != 0)
    //cout << "Kerning " << ((char) pc) << ((char) uc) << " " << ROUND (fnm->kerning (pc, uc)) << ", " << ROUND (ex->x2) << "\n";
  }
}

void
unicode_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  draw_fixed (ren, s, x, y, true);
}

// A glyph of a color font (COLR): its layers one above the other, each an
// ordinary glyph in its own color, so that every renderer draws it as it
// draws text, and a PDF keeps it as vectors
bool
unicode_font_rep::draw_color_glyph (renderer ren, unsigned int uc, string c,
                                    SI x, SI y) {
  if (tt_color_bitmap_scale (fng) > 0.0)
    return draw_color_bitmap (ren, uc, c, x, y);
  array<int> codes;
  array<color> colors;
  array<bool> text;
  if (!tt_color_layers (fng, uc, codes, colors, text)) return false;
  // The text of a PDF, c (the characters of the emoji or of its sequence),
  // for the two kinds of readers: those which take it from the character
  // map of the font find it in invisible glyphs of the characters (Preview),
  // those which take the ActualText of a span find it on the first layer,
  // since they leave out the glyphs with no ink (MuPDF). Every other glyph
  // is in a span which stands for nothing.
  // The invisible glyphs follow each other over the width of the emoji,
  // each narrowed to its share: one on top of the other, they would be
  // words of their own for a reader (spaces in the middle of a sequence).
  array<unsigned int> chars;
  for (int i= 0; i < N(c); ) chars << read_unicode_char (c, i);
  int n= N(chars);
  int w= tt_glyph_units_advance (fng, uc);
  SI  W= ROUND (fnm->get (uc)->x2);
  ren->begin_actual_text ("");
  for (int k= 0; k < n; k++) {
    int a= tt_glyph_units_advance (fng, chars[k]);
    double hscale= (a > 0 && w > 0)? (((double) w) / n) / a: 1.0;
    ren->draw_invisible (chars[k], fng, x + (k * W) / n, y, hscale);
  }
  ren->end_actual_text ();
  pencil pen= ren->get_pencil ();
  int fr, fg, fb, fa;
  get_rgb_color (pen->get_color (), fr, fg, fb, fa);
  for (int k= 0; k < N(codes); k++) {
    if (k <= 1) {
      if (k == 1) ren->end_actual_text ();
      ren->begin_actual_text (k == 0? c: string (""));
    }
    if (text[k]) ren->set_pencil (pen);
    else {
      // the opacity of the text applies to the colors of the layers too
      int r, g, b, a;
      get_rgb_color (colors[k], r, g, b, a);
      ren->set_pencil (pencil (rgb_color (r, g, b, (a * fa + 127) / 255),
                               pen->get_width ()));
    }
    ren->draw (codes[k], fng, x, y);
  }
  ren->set_pencil (pen);
  ren->end_actual_text ();
  return true;
}


font
unicode_font_rep::magnify (double zoomx, double zoomy) {
  return unicode_font (family, size,
                       (int) tm_round (hdpi * zoomx),
                       (int) tm_round (vdpi * zoomy));
}

void
unicode_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  if (pos >= N(s)) return;
  (void) read_glyph (s, pos, ligf);
}

glyph
unicode_font_rep::get_glyph (string s) {
  int i= 0, n= N(s);
  if (n == 0) return font_rep::get_glyph (s);
  unsigned int uc= read_glyph (s, i, true);
  if (i != n) return font_rep::get_glyph (s);
  glyph gl= fng->get (uc);
  if (is_nil (gl)) return font_rep::get_glyph (s);
  return gl;
}

int
unicode_font_rep::index_glyph (string s, font_metric& rm, font_glyphs& rg) {
  int i= 0, n= N(s);
  if (n == 0) return font_rep::index_glyph (s, rm, rg);
  unsigned int uc= read_glyph (s, i, true);
  if (i != n) return font_rep::index_glyph (s, rm, rg);
  glyph gl= fng->get (uc);
  if (is_nil (gl)) return font_rep::index_glyph (s, rm, rg);
  rm= fnm;
  rg= fng;
  return uc;
}

static bool
is_math_italic (string c) {
  if (N(c) <= 2) return false;
  int i= 0;
  int code= decode_from_utf8 (strict_cork_to_utf8 (c), i);
  if (code < 0x2100 || code > 0x1d7ff) return false;
  if (code <= 0x213a) {
    if (code == 0x210a || code == 0x210b || code == 0x210e ||
        code == 0x210f || code == 0x2110 || code == 0x2112 ||
        code == 0x2113 || code == 0x211b || code == 0x212c ||
        code == 0x212f || code == 0x2130 || code == 0x2131 ||
        code == 0x2133 || code == 0x2134)
      return true;
  }
  else if (code >= 0x1d400) {
    if (code >= 0x1d434 && code <= 0x1d503) return true;
    if (code >= 0x1d608 && code <= 0x1d66f) return true;
    if (code >= 0x1d6e2 && code <= 0x1d755) return true;
    if (code >= 0x1d790 && code <= 0x1d7c9) return true;
  }
  return false;
}

static bool
is_integral (string s) {
  if (!starts (s, "<big-")) return false;
  int pos= 5, n= N(s);
  if (pos+1 < n && s[pos] == 'u' && s[pos+1] == 'p') pos += 2;
  if (pos < n && s[pos] == 'o') pos++;
  while (pos+1 < n && s[pos] == 'i' && s[pos+1] == 'i') pos++;
  return test (s, pos, "int-") ||
         test (s, pos, "int>") ||
         test (s, pos, "idotsint");
}

static bool
is_alt_integral (string s) {
  if (!starts (s, "<")) return false;
  int pos= 1, n= N(s);
  if (pos+1 < n && s[pos] == 'u' && s[pos+1] == 'p') pos += 2;
  if (pos < n && s[pos] == 'o') pos++;
  while (pos+1 < n && s[pos] == 'i' && s[pos+1] == 'i') pos++;
  return test (s, pos, "int>") || test (s, pos, "idotsint");
}

double
unicode_font_rep::get_left_slope (string s) {
  if (N(s) == 0) return slope;
  int pos= 0;
  tm_char_forwards (s, pos);
  if (pos == 1) return slope;
  metric ex;
  string c= s (pos, N(s));
  if (N(c) >= 3) {
    if (is_math_italic (c))
      return max (slope, 0.2); // FIXME: should be determined more reliably
    else if (math_type == MATH_TYPE_TEX_GYRE && is_integral (s))
      return 0.1;
  }
  get_extents (c, ex);
  if (ex->y3 >= 0) return slope;
  double sl= ((double) (ex->x3 - ex->x1)) / ((double) ex->y3);
  if (sl > slope + 0.05) return sl;
  else return slope;
}

double
unicode_font_rep::get_right_slope (string s) {
  if (N(s) == 0) return slope;
  int pos= N(s);
  tm_char_backwards (s, pos);
  if (pos == N(s) - 1) return slope;
  metric ex;
  string c= s (pos, N(s));
  if (N(c) >= 3) {
    if (is_math_italic (c))
      return max (slope, 0.2); // FIXME: should be determined more reliably
    else if (math_type == MATH_TYPE_TEX_GYRE && is_integral (s))
      return 0.1;
  }
  get_extents (c, ex);
  if (ex->y4 <= 0) return slope;
  double sl= ((double) (ex->x4 - ex->x2)) / ((double) ex->y4);
  if (sl > slope + 0.05) return sl;
  else return slope;
}

SI
unicode_font_rep::get_left_correction  (string s) {
  metric ex;
  get_extents (s, ex);
  if (math_type == MATH_TYPE_TEX_GYRE && is_integral (s))
    return - (((ex->x2 - ex->x1) / 16));
  else if (ex->x3 < ex->x1)
    return ex->x1 - ex->x3;
  return 0;
}

SI
unicode_font_rep::get_right_correction (string s) {
  if (math_type == MATH_TYPE_OPENTYPE) {
    SI r= 0;
    if (get_ot_italic_correction (s, r)) return r;
  }
  metric ex;
  get_extents (s, ex);
  if (math_type == MATH_TYPE_TEX_GYRE && is_integral (s))
    return (ex->x2 - ex->x1) / 16;
  else if (ex->x4 > ex->x2)
    return ex->x4 - ex->x2;
  return 0;
}

SI
unicode_font_rep::get_lsub_correction (string s) {
  return get_lsub_correction_at (s, y1);
}

SI
unicode_font_rep::get_lsup_correction (string s) {
  return get_lsup_correction_at (s, y2);
}

SI
unicode_font_rep::get_rsub_correction (string s) {
  return get_rsub_correction_at (s, y1);
}

SI
unicode_font_rep::get_rsup_correction (string s) {
  return get_rsup_correction_at (s, y2);
}

SI
unicode_font_rep::get_lsub_correction_at (string s, SI h) {
  if (math_type == MATH_TYPE_OPENTYPE) {
    SI r= 0;
    if (get_ot_kerning (s, h, false, true, r)) return r;
  }
  SI r= -get_left_correction (s) + global_lsub_correct;
  if (math_type == MATH_TYPE_STIX &&
      (is_integral (s) || is_alt_integral (s)));
  else if (lsub_correct->contains (s)) r += (SI) (lsub_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[0]) &&
           lsub_correct->contains (s (0, 1)))
    r += (SI) (lsub_correct[s (0, 1)] * wfn);
  return r;
}

SI
unicode_font_rep::get_lsup_correction_at (string s, SI h) {
  if (math_type == MATH_TYPE_OPENTYPE) {
    SI r= 0;
    if (get_ot_kerning (s, h, true, true, r)) return r;
  }
  SI r= global_lsup_correct;
  if (math_type == MATH_TYPE_STIX &&
      (is_integral (s) || is_alt_integral (s)))
    r += get_right_correction (s);
  else if (lsup_correct->contains (s)) r += (SI) (lsup_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[0]) &&
           lsup_correct->contains (s (0, 1)))
    r += (SI) (lsup_correct[s (0, 1)] * wfn);
  return r;
}

SI
unicode_font_rep::get_rsub_correction_at (string s, SI h) {
  if (math_type == MATH_TYPE_OPENTYPE) {
    SI   ic= 0, kern= 0;
    bool has_ic  = get_ot_italic_correction (s, ic);
    bool has_kern= get_ot_kerning (s, h, false, false, kern);

    if (has_ic || has_kern) {
      // for integral, we use 3/5 of italic correction for rsub, otherwise 0
      ic  = is_ot_integral (get_right (s)) ? (SI) (0.6 * ic) : 0;
      return -ic + kern;
    }
  }
  SI r= global_rsub_correct;
  if (math_type == MATH_TYPE_STIX &&
      (is_integral (s) || is_alt_integral (s)));
  else if (rsub_correct->contains (s)) r += (SI) (rsub_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[N(s)-1]) &&
           rsub_correct->contains (s (N(s)-1, N(s))))
    r += (SI) (rsub_correct[s (N(s)-1, N(s))] * wfn);
  return r;
}

SI
unicode_font_rep::get_rsup_correction_at (string s, SI h) {
  if (math_type == MATH_TYPE_OPENTYPE) {
    SI   ic= 0, kern= 0;
    bool has_ic  = get_ot_italic_correction (s, ic);
    bool has_kern= get_ot_kerning (s, h, true, false, kern);

    if (has_ic || has_kern) {
      // for integral signs, we use 2/5 of italic correction for rsup
      if (is_ot_integral (get_right (s))) ic= (SI) (0.4 * ic);
      return ic + kern;
    }
  }
  SI r= get_right_correction (s) + global_rsup_correct;
  if (math_type == MATH_TYPE_STIX &&
      (is_integral (s) || is_alt_integral (s)));
  else if (rsup_correct->contains (s)) r += (SI) (rsup_correct[s] * wfn);
  else if (N(s) > 1 && is_alpha (s[N(s)-1]) &&
           rsup_correct->contains (s (N(s)-1, N(s))))
    r += (SI) (rsup_correct[s (N(s)-1, N(s))] * wfn);
  return r;
}

SI
unicode_font_rep::get_wide_correction (string s, int mode) {
  if (mode > 0 && above_correct->contains (s)) {
    //cout << s << " ~> " << ((SI) (above_correct[s] * wfn)) << LF;
    return (SI) (above_correct[s] * wfn);
  }
  else if (mode < 0 && below_correct->contains (s))
    return (SI) (below_correct[s] * wfn);
  else return 0;
}

/******************************************************************************
 * OpenType
 ******************************************************************************/

inline void
unicode_font_rep::init_design_unit_factor () {
  // The face is scaled to 'size' points at hdpi x vdpi, so one design unit
  // measures size/units_per_EM points, i.e. size*hpt/units_per_EM vertically
  // and size*wpt/units_per_EM horizontally (in SI units).
  double upem= (double) ot_face->ft_face->units_per_EM;
  if (upem <= 0.0) upem= 1000.0;
  design_unit_to_metric_factor  = ((double) size * (double) hpt) / upem;
  design_unit_to_metric_x_factor= ((double) size * (double) wpt) / upem;
  metric_to_design_unit_factor  = 1.0 / design_unit_to_metric_factor;
}

inline SI
unicode_font_rep::design_unit_to_metric (int du) {
  return (SI) tm_round (design_unit_to_metric_factor * du);
}

inline SI
unicode_font_rep::design_unit_to_metric_x (int du) {
  return (SI) tm_round (design_unit_to_metric_x_factor * du);
}

inline int
unicode_font_rep::metric_to_design_unit (SI m) {
  return (int) tm_round (metric_to_design_unit_factor * m);
}

font
unicode_font_rep::make_rubber_font (font base) {
  if (!is_nil (this->math_table)) {
    return rubber_unicode_font (this, this->ot_face);
  }
  return font_rep::make_rubber_font (base);
}

// The rubber font which stretches the glyphs of a font from the MATH table
// of its file, also for a font which is hand tuned and typeset without
// that table; the empty font when the file has no such table
font
math_table_rubber_font (font base) {
  unicode_font_rep* u= dynamic_cast<unicode_font_rep*> (base.rep);
  if (u == NULL || is_nil (u->ot_face) || is_nil (u->ot_face->math_table))
    return font ();
  return rubber_unicode_font (base, u->ot_face);
}

inline int
decode_index (FT_Face face, int i) {
  if (i < 0xc000000) return ft_get_char_index (face, i);
  return i - 0xc000000;
}

inline unsigned int
unicode_font_rep::get_glyphID (string s) {
  // <@XXXX>
  if (starts (s, "<@")) {
    return from_hexadecimal (s (2, 6));
  }
  font_metric fm;
  font_glyphs fg;
  if (is_nil (ot_face)) return 0;
  // a glyph belongs to one character
  int pos= 0;
  tm_char_forwards (s, pos);
  if (pos != N(s)) return 0;
  int         index= index_glyph (s, fm, fg);
  return decode_index (ot_face->ft_face, index);
}

inline string
get_left (string s) {
  if (N (s) == 0) return s;
  int i= 0;
  tm_char_forwards (s, i);
  return s (0, i);
}

inline string
get_right (string s) {
  if (N (s) == 0) return s;
  int i= N (s);
  tm_char_backwards (s, i);
  return s (i, N (s));
}

bool
unicode_font_rep::get_ot_italic_correction (string s, SI& r) {
  if (math_type != MATH_TYPE_OPENTYPE || N (s) == 0) return false;

  auto italics_correction= math_table->italics_correction;
  // italic correction is only available for right side of the glyph
  string ss     = get_right (s);
  unsigned int glyphID= get_glyphID (ss);

  if (italics_correction->contains (glyphID)) {
    int correction= italics_correction[glyphID].value;
    r= design_unit_to_metric_x (correction);
    return true;
  }
  return false;
}

bool
unicode_font_rep::get_ot_kerning (string s, SI height, bool top, bool left,
                                  SI& kerning) {
  if (math_type != MATH_TYPE_OPENTYPE || N (s) == 0) return false;

  string       ss     = left ? get_left (s) : get_right (s);
  unsigned int glyphID= get_glyphID (ss);

  if (!math_table->has_kerning (glyphID, top, left)) return false;

  int kerning_unit= math_table->get_kerning (
      glyphID, metric_to_design_unit (height), top, left);

  kerning= design_unit_to_metric_x (kerning_unit);
  // cout << "Kerning for " << ss << " with height: " << kerning_unit << " -> "
  //      << kerning << LF;
  return true;
}

bool
unicode_font_rep::is_extended_shape (string s) {
  if (!ot_math || N(s) == 0) return false;
  unsigned int glyphID= get_glyphID (s);
  glyphID= math_table->get_init_glyphID (glyphID);
  return math_table->extended_shape_coverage->contains (glyphID);
}

bool
unicode_font_rep::get_top_accent (string s, SI& x) {
  if (!ot_math || N(s) == 0) return false;
  unsigned int glyphID= get_glyphID (s);
  if (!math_table->top_accent->contains (glyphID)) return false;
  x= design_unit_to_metric_x (math_table->top_accent[glyphID].value);
  // an attachment point lies over the glyph; a font which puts it far
  // from the ink is not believed (GFS Neohellenic Math has the points of
  // zero width accents for its acute and grave accents, which are not)
  metric ex;
  get_extents (s, ex);
  SI w= ex->x4 - ex->x3;
  if (w > 0 && (x < ex->x3 - w || x > ex->x4 + w)) return false;
  return true;
}

bool
unicode_font_rep::get_feature_variant (string s, string feature, int alt,
                                       string& r) {
  if (N(s) == 0 || is_nil (ot_face)) return false;
  unsigned int glyphID= get_glyphID (s);
  if (glyphID == 0) return false;
  ot_gsub_map& m= ot_face->gsub_feature (feature);
  if (!m->contains (glyphID)) return false;
  array<unsigned int> alts= m[glyphID];
  if (alt < 0 || alt >= N(alts)) return false;
  r= "<@" * as_hexadecimal (alts[alt], 4) * ">";
  set_glyph_unicode (r, source_unicode (s));
  return true;
}

// The Unicode value of the character s, or the one recorded for it when s
// is a native glyph name
int
unicode_font_rep::source_unicode (string s) {
  int i= 0;
  unsigned int uc= read_unicode_char (s, i);
  if (uc < 0xc000000) return (int) uc;
  return max (0, native_glyph_get_unicode (fng->res_name, uc - 0xc000000));
}

void
unicode_font_rep::set_glyph_unicode (string s, int code) {
  if (!starts (s, "<@")) return;
  int i= 0;
  unsigned int uc= read_unicode_char (s, i);
  if (uc >= 0xc000000)
    native_glyph_set_unicode (fng->res_name, uc - 0xc000000, code);
}

// The OpenType features a font file offers, for a menu or a dialog to
// propose only what the font is able to do. The name is the one a font
// rule gives to the file, as in "lmroman10-regular".
array<string>
ot_font_features (string name) {
  array<string> r;
  // the database gives a file name, a font rule gives the name without
  // its extension; accept both
  if (ends (name, ".otf") || ends (name, ".ttf") || ends (name, ".ttc") ||
      ends (name, ".pfb"))
    name= name (0, N(name) - 4);
  if (!tt_font_exists (name)) return r;
  tt_face face= load_tt_face (name);
  if (is_nil (face) || face->bad_face) return r;
  return face->gsub_tags ();
}

bool
unicode_font_rep::is_ot_integral (string s) {
  if (!ot_math) return false;
  if (N (ot_integral) == 0) {
    array<string> integrals;
    integrals << string ("<int>") << string ("<iiint>") << string ("<iiiint>")
              << string ("<oint>") << string ("<oiint>") << string ("<oiiint>")
              << string ("<upint>") << string ("<upiint>")
              << string ("<upiiint>") << string ("<upiiiint>")
              << string ("<upoint>") << string ("<upoiint>")
              << string ("<upoiiint>") << string ("<intlim>")
              << string ("<iintlim>") << string ("<iiintlim>")
              << string ("<iiiintlim>") << string ("<ointlim>")
              << string ("<oiintlim>") << string ("<oiiintlim>");
    for (int i=0; i < N(integrals); i++) {
      ot_integral << get_glyphID (integrals[i]);
    }
  }
  unsigned int glyphID= get_glyphID (s);
  // if variant, we turn to the base glyphID
  glyphID= math_table->get_init_glyphID (glyphID);
  return ot_integral->contains (glyphID);
}

/******************************************************************************
* Interface
******************************************************************************/

font
unicode_font (string family, int size, int hdpi, int vdpi) {
  string name= "unicode:" * family * as_string (size) * "@" * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
  name << hand_tuned_suffix ();
  return make (font, name,
               tm_new<unicode_font_rep> (name, family, size, hdpi, vdpi));
}

font
unicode_font (string family, int size, int dpi) {
  return unicode_font (family, size, dpi, dpi);
}

#else

font
unicode_font (string family, int size, int dpi) {
  string name= "unicode:" * family * as_string (size) * "@" * as_string (dpi);
  failed_error << "Font name= " << name << "\n";
  FAILED ("true type support was disabled");
  return font ();
}

#endif
