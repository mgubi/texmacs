
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
#include "analyze.hpp"
#include "converter.hpp"
#include "colors.hpp"
#include "picture.hpp"
#include "Freetype/tt_tools.hpp"

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
  
  unicode_font_rep (string name, string family, int size, int hdpi, int vdpi);
  void tex_gyre_operators ();

  unsigned int read_unicode_char (string s, int& i);
  unsigned int ligature_replace (unsigned int c, string s, int& i);
  unsigned int read_glyph (string s, int& i, bool ligf);
  unsigned int color_ligature (unsigned int c, string s, int& i);
  tt_face ot_face;    // the parsed face: GSUB (ccmp) and morx, for the emoji
  bool   color_font;  // with glyphs in several colors (COLR, sbix, CBDT)
  bool   draw_color_glyph (renderer ren, unsigned int uc, string c,
                           SI x, SI y);
  bool   draw_color_bitmap (renderer ren, unsigned int uc, string c,
                            SI x, SI y);
  int    emoji_marks_end (string s, int i);
  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos, bool ligf);
  void   get_xpositions (string s, SI* xpos);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool ligf);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
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
  ot_face= load_tt_face (family);
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

  // direct translations for certain characters without Unicode names
  if (starts (family, "texgyre") && ends (family, "-math"))
    tex_gyre_operators ();

  if (starts (family, "STIX-")) {
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

  else if (starts (family, "texgyre")) {
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

  else if (starts (family, "Papyrus")) {
    lsup_correct= copy (lsup_guessed_table ());
    rsub_correct= copy (rsub_guessed_table ());
    adjust_integral (lsup_correct, "1", -0.15);
    adjust_integral (lsup_correct, "2", -0.15);
    adjust_integral (rsub_correct, "1", 0.15);
    adjust_integral (rsub_correct, "2", 0.15);
  }

  else if (starts (family, "LinLibertine")) {
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
  else if (starts (family, "LinBiolinum")) {
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
  else if (starts (family, "FiraSans")) {
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
  if (((char) uc) == 'f') {
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
  else if (((char) uc) == 's') {
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

// the bitmaps of the fixed size (FreeType decodes a PNG to give one), with
// their sizes (w, h, left, top, advance; empty when the glyph has none),
// and the pictures made of them at the sizes drawn; emptied when they hold
// too many (a zoom makes pictures of new sizes)
static hashmap<string,string>     color_bitmap_data;
static hashmap<string,array<int> > color_bitmap_info;
static hashmap<string,picture>    color_bitmap_pictures;
#define COLOR_BITMAP_CACHE 4096

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
  string bkey= fng->res_name * "#" * as_string ((int) uc);
  if (!color_bitmap_info->contains (bkey)) {
    if (N(color_bitmap_info) >= COLOR_BITMAP_CACHE) {
      color_bitmap_data= hashmap<string,string> ();
      color_bitmap_info= hashmap<string,array<int> > ();
    }
    string bgra;
    int w, h, left, top, adv;
    array<int> info;
    if (tt_color_bitmap (fng, uc, bgra, w, h, left, top, adv) &&
        w > 0 && h > 0 && adv > 0) {
      info << w << h << left << top << adv;
      color_bitmap_data (bkey)= bgra;
    }
    color_bitmap_info (bkey)= info;
  }
  array<int> info= color_bitmap_info [bkey];
  if (N(info) < 5) return false;
  int w= info[0], h= info[1], left= info[2], top= info[3], adv= info[4];
  // a pixel of the bitmap, in the units of the renderer, and in its pixels
  SI W= ROUND (fnm->get (uc)->x2);
  double k = ((double) W) / adv;
  double px= k / max (ren->pixel, 1);
  int pw= max (1, (int) (w * px + 0.5)), ph= max (1, (int) (h * px + 0.5));
  string key= bkey * "@" * as_string (pw) * "x" * as_string (ph);
  if (!color_bitmap_pictures->contains (key)) {
    if (N(color_bitmap_pictures) >= COLOR_BITMAP_CACHE)
      color_bitmap_pictures= hashmap<string,picture> ();
    color_bitmap_pictures (key)=
      color_bitmap_picture (color_bitmap_data [bkey], w, h, pw, ph);
  }
  picture pic= color_bitmap_pictures [key];
  // the characters, as invisible glyphs of the shipped emoji font (for the
  // text of a PDF: nothing on a screen)
  if (ren->is_printer () && tt_font_exists (SHIPPED_EMOJI_FONT)) {
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
unicode_font_rep::get_lsup_correction (string s) {
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
unicode_font_rep::get_rsub_correction (string s) {
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
unicode_font_rep::get_rsup_correction (string s) {
  //cout << "Check " << s << ", " << rsup_correct[s] << ", " << this->res_name << LF;
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
* Interface
******************************************************************************/

font
unicode_font (string family, int size, int hdpi, int vdpi) {
  string name= "unicode:" * family * as_string (size) * "@" * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
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
