
/******************************************************************************
* MODULE     : tt_face.hpp
* DESCRIPTION: resources for true type faces, gliefs and metrics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TT_FACE_H
#define TT_FACE_H
#include "bitmap_font.hpp"
#include "Freetype/free_type.hpp"
#include "hashmap.hpp"
#include "Freetype/tt_tools.hpp"

#ifdef USE_FREETYPE

RESOURCE(tt_face);

struct tt_face_rep: rep<tt_face> {
  bool bad_face = true;
  FT_Face ft_face = nullptr;
  FT_Byte *buffer = nullptr;
  int buffer_size = 0;
  hashmap<string,ot_gsub_ligatures> gsub_ligature_table;
  // the ligatures of a GSUB feature (cached)
  ot_gsub_ligatures& gsub_ligatures (string tag);
  // the fonts made of bitmaps (the emoji in color of Apple, of Noto, of
  // Android) have sizes of their own only: the largest one is taken, and
  // its glyphs are scaled to the size asked for
  int          strike= -1;
  int          strike_ppem= 0;
  bool         color_bitmaps= false;
  // the size of the glyphs (ft_set_char_size, or the largest fixed size of
  // a font of bitmaps): true when it cannot be set
  bool set_size (int size, int hdpi, int vdpi);
  void init_strike ();
  // how much a glyph of the fixed size is scaled to be of the size asked
  double strike_scale (int size, int dpi);
  // whether the glyphs may have colors (layers or bitmaps)
  bool is_color ();
  string       morx_table;
  bool         morx_ready= false;
  hashmap<string,int> morx_sequences= hashmap<string,int> (-1);
  // the glyph which the morx table of the font (Apple) makes of the
  // sequence of characters c, or -1 when it makes more than one glyph
  int morx_sequence (array<unsigned int> codes);
  bool         colors_ready= false;
  array<color> palette;
  // the layers of a glyph of a color font (COLR version 0), from the
  // bottom up: the glyph of each layer and the index of its color in the
  // palette (CPAL), -1 for the color of the text; false for a glyph which
  // has no layers
  bool color_layers (unsigned int gid, array<int>& layers,
                     array<int>& colors);

  tt_face_rep (string name);
  ~tt_face_rep () override;
};

struct tt_font_metric_rep: font_metric_rep {
  bool bad_metric;
  tt_face face;
  int size, hdpi, vdpi;
  hashmap<int,pointer> fnm;
  //metric* fnm;
  //bool* done;
  tt_font_metric_rep (string name, string family, int size, int hdpi, int vdpi);
  bool exists (int char_code);
  metric& get (int char_code);
  SI kerning (int left_code, int right_code);
};

struct tt_font_glyphs_rep: font_glyphs_rep {
  bool bad_glyphs;
  tt_face face;
  int size, hdpi, vdpi;
  hashmap<int,glyph> fng;
  //glyph* fng;
  //bool* done;
  tt_font_glyphs_rep (string name, string family, int size, int hdpi, int vdpi);
  glyph& get (int char_code);
  glyph color_glyph_union (glyph base, array<int> layers);
};

tt_face load_tt_face (string name);
// the layers of the glyph of a character of a color font, as characters of
// the same font (native glyphs), with their colors; text[k] when layer k has
// the color of the text: false when the glyph has no colors
bool tt_color_layers (font_glyphs fng, int i, array<int>& codes,
                      array<color>& colors, array<bool>& text);
// whether the glyphs of a FreeType font may have colors (COLR and CPAL, or
// bitmaps in color: sbix, CBDT)
bool tt_color_font (font_glyphs fng);
// the bitmap in color of the glyph of a character of a font of bitmaps, at
// the fixed size of the font: its pixels (blue, green, red and alpha,
// premultiplied, the rows from the top), its size, the place of its top
// left corner from the origin (y upwards) and the advance, in its pixels
bool tt_color_bitmap (font_glyphs fng, int i, string& bgra, int& w, int& h,
                      int& left, int& top, int& adv);
// how much the bitmaps of tt_color_bitmap are scaled for the glyphs of fng
// (0 when it is not a font of bitmaps in color)
double tt_color_bitmap_scale (font_glyphs fng);
// the advance of the glyph of a character in the units of the font, as
// the font has it (0 when unknown)
int tt_glyph_units_advance (font_glyphs fng, int i);
font_metric tt_font_metric (string family, int size, int hdpi, int vdpi);
//font_glyphs tt_font_glyphs (string family, int size, int hdpi, int vdpi);

#endif // USE_FREETYPE

#endif // defined TT_FACE_H
