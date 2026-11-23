
/******************************************************************************
* MODULE     : bitmap_font.cpp
* DESCRIPTION: bitmap fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"
#include "analyze.hpp"

RESOURCE_CODE(font_metric);
RESOURCE_CODE(font_glyphs);

/******************************************************************************
* font_metrics
******************************************************************************/

font_metric_rep::font_metric_rep (string name):
  rep<font_metric> (name), bad_font_metric (false) {}

font_metric_rep::~font_metric_rep () {
  FAILED ("not yet implemented"); }

bool font_metric_rep::exists (int char_code) {
  (void) char_code; return true; }

SI font_metric_rep::kerning (int left_code, int right_code) {
  (void) left_code; (void) right_code; return 0; }

/******************************************************************************
* Standard bitmap metrics
******************************************************************************/

metric error_metric;

struct std_font_metric_rep: public font_metric_rep {
  int bc, ec;
  metric* fnm;

  std_font_metric_rep (string name, metric* fnm, int bc, int ec);
  metric& get (int char_code);
};

std_font_metric_rep::std_font_metric_rep (
  string name, metric* fnm2, int bc2, int ec2):
    font_metric_rep (name), bc (bc2), ec (ec2), fnm (fnm2)
{
  error_metric->x1= error_metric->y1= 0;
  error_metric->x2= error_metric->y2= 0;
  error_metric->x3= error_metric->y3= 0;
  error_metric->x4= error_metric->y4= 0;
}

metric&
std_font_metric_rep::get (int c) {
  if ((c<bc) || (c>ec)) return error_metric;
  return fnm [c-bc];
}

font_metric
std_font_metric (string name, metric* fnm, int bc, int ec) {
  return make (font_metric, name,
	       tm_new<std_font_metric_rep> (name, fnm, bc, ec));
}

/******************************************************************************
* font_glyphs
******************************************************************************/

font_glyphs_rep::font_glyphs_rep (string name):
  rep<font_glyphs> (name), bad_font_glyphs (false) {}

font_glyphs_rep::~font_glyphs_rep () {
  FAILED ("not yet implemented"); }

// (the encodings of a list in a fixed order, whatever order the fonts were
// loaded in: by name, those of the letters as themselves, alphanum, last,
// so that a glyph drawn as a letter of a style and as a symbol, the
// Fraktur A of eufm, is the symbol)
static bool
encoding_before (string a, string b) {
  bool la= starts (a, "alphanum"), lb= starts (b, "alphanum");
  if (la != lb) return lb;
  return a < b;
}

void
font_glyphs_rep::add_encoding (string enc) {
  if (N(enc) == 0 || encoding == "mixed") return;
  array<string> list;
  int i= 0, n= N(encoding);
  while (i < n) {
    int j= i;
    while (j < n && encoding[j] != '+') j++;
    list << encoding (i, j);
    i= j + 1;
  }
  for (i=0; i<N(list); i++)
    if (list[i] == enc) return;
  // (the same glyphs drawn as Unicode and as positions: the codes say
  // nothing)
  if (N(list) > 0 && (enc == "unicode" || encoding == "unicode")) {
    encoding= "mixed";
    return;
  }
  list << copy (enc);
  for (i=N(list)-1; i>0 && encoding_before (list[i], list[i-1]); i--) {
    string t= list[i]; list[i]= list[i-1]; list[i-1]= t; }
  // (a new string: << would change one shared with a caller)
  string r;
  for (i=0; i<N(list); i++) r= (i == 0? list[i]: r * "+" * list[i]);
  encoding= r;
}

string font_glyphs_rep::get_encoding () { return encoding; }
string font_glyphs_rep::get_metric () { return metric; }

/******************************************************************************
* Standard bitmap fonts
******************************************************************************/

glyph error_glyph;

struct std_font_glyphs_rep: public font_glyphs_rep {
  int bc, ec;
  glyph* fng; // definitions of the characters

  std_font_glyphs_rep (string name, glyph* fng, int bc, int ec);
  glyph& get (int char_code);
};

std_font_glyphs_rep::std_font_glyphs_rep (
  string name, glyph* fng2, int bc2, int ec2):
    font_glyphs_rep (name), bc (bc2), ec (ec2), fng (fng2) {}

glyph&
std_font_glyphs_rep::get (int c) {
  if ((c<bc) || (c>ec)) return error_glyph;
  return fng [c-bc];
}

font_glyphs
std_font_glyphs (string name, glyph* fng, int bc, int ec) {
  return make (font_glyphs, name, tm_new<std_font_glyphs_rep> (name, fng, bc, ec));
}
