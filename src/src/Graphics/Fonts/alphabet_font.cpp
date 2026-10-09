
/******************************************************************************
* MODULE     : alphabet_font.cpp
* DESCRIPTION: Fonts whose letters are those of another alphabet
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* TeX had fonts whose letters are calligraphic (cmsy, rsfs), fraktur (eufm)
* or blackboard bold (msbm, bbm), and the families "cal", "Euler", "Bbb" of
* TeXmacs were made of them: "A" in such a font is what Unicode calls a
* mathematical script, fraktur or double-struck capital A. Tau has no TeX
* fonts: an alphabet font draws the letters of its strings as the symbols
* <cal-A>, <frak-A>, <bbb-A> of a base font, an OpenType math font.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"

struct alphabet_font_rep: font_rep {
  font   base;
  string kind;   // "cal", "frak" or "bbb"

  alphabet_font_rep (string name, font base, string kind);
  string rewrite (string s);
  string rewrite (string s, array<int>& where);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, bool lig);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool ligf);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
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


alphabet_font_rep::alphabet_font_rep (string name, font base2, string kind2):
  font_rep (name, base2), base (base2), kind (kind2)
{
  // not an OpenType math font for the smart font: it would take the single
  // letters of the formulas from the math italic alphabet of the base font
  math_type= MATH_TYPE_NORMAL;
  ot_math= false;
}

string
alphabet_font_rep::rewrite (string s, array<int>& where) {
  // the string with its letters as symbols; where[i] is the position in it
  // of the character at position i of s (and where[N(s)] its length)
  string r;
  where= array<int> (N(s) + 1);
  int i= 0;
  while (i < N(s)) {
    int start= i;
    tm_char_forwards (s, i);
    for (int j= start; j < i; j++) where[j]= N(r);
    string c= s (start, i);
    bool letter= N(c) == 1 && (is_upcase (c[0]) ||
                               (kind != "cal" && is_locase (c[0])));
    if (letter) {
      string sym= "<" * kind * "-" * c * ">";
      if (base->supports (sym)) c= sym;
    }
    r << c;
  }
  where[N(s)]= N(r);
  return r;
}

string
alphabet_font_rep::rewrite (string s) {
  array<int> where;
  return rewrite (s, where);
}

bool
alphabet_font_rep::supports (string c) {
  return base->supports (rewrite (c));
}

void
alphabet_font_rep::get_extents (string s, metric& ex) {
  base->get_extents (rewrite (s), ex);
}

#define ALPHABET_XPOSITIONS(CALL) \
  array<int> where; \
  string r= rewrite (s, where); \
  STACK_NEW_ARRAY (rpos, SI, N(r) + 1); \
  for (int i= 0; i <= N(r); i++) rpos[i]= 0; \
  CALL; \
  for (int i= 0; i <= N(s); i++) xpos[i]= rpos[where[i]]; \
  STACK_DELETE_ARRAY (rpos);

void
alphabet_font_rep::get_xpositions (string s, SI* xpos) {
  ALPHABET_XPOSITIONS (base->get_xpositions (r, rpos));
}

void
alphabet_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  ALPHABET_XPOSITIONS (base->get_xpositions (r, rpos, lig));
}

void
alphabet_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  ALPHABET_XPOSITIONS (base->get_xpositions (r, rpos, xk));
}

void
alphabet_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  base->draw_fixed (ren, rewrite (s), x, y);
}

void
alphabet_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool lf) {
  base->draw_fixed (ren, rewrite (s), x, y, lf);
}

void
alphabet_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  base->draw_fixed (ren, rewrite (s), x, y, xk);
}

font
alphabet_font_rep::magnify (double zoomx, double zoomy) {
  return alphabet_font (base->magnify (zoomx, zoomy), kind);
}

void
alphabet_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  (void) ligf;
  tm_char_forwards (s, pos);
}

glyph
alphabet_font_rep::get_glyph (string s) {
  return base->get_glyph (rewrite (s));
}

int
alphabet_font_rep::index_glyph (string s, font_metric& fnm,
                                font_glyphs& fng) {
  return base->index_glyph (rewrite (s), fnm, fng);
}

double
alphabet_font_rep::get_left_slope (string s) {
  return base->get_left_slope (rewrite (s));
}

double
alphabet_font_rep::get_right_slope (string s) {
  return base->get_right_slope (rewrite (s));
}

SI
alphabet_font_rep::get_left_correction (string s) {
  return base->get_left_correction (rewrite (s));
}

SI
alphabet_font_rep::get_right_correction (string s) {
  return base->get_right_correction (rewrite (s));
}

SI
alphabet_font_rep::get_lsub_correction (string s) {
  return base->get_lsub_correction (rewrite (s));
}

SI
alphabet_font_rep::get_lsup_correction (string s) {
  return base->get_lsup_correction (rewrite (s));
}

SI
alphabet_font_rep::get_rsub_correction (string s) {
  return base->get_rsub_correction (rewrite (s));
}

SI
alphabet_font_rep::get_rsup_correction (string s) {
  return base->get_rsup_correction (rewrite (s));
}

SI
alphabet_font_rep::get_wide_correction (string s, int mode) {
  return base->get_wide_correction (rewrite (s), mode);
}

font
alphabet_font (font base, string kind) {
  string name= "alphabet[" * base->res_name * "," * kind * "]";
  return make (font, name, tm_new<alphabet_font_rep> (name, base, kind));
}

