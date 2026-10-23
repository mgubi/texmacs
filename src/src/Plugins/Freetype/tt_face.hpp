
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
#include "Freetype/tt_tools.hpp"
#include "hashmap.hpp"

#ifdef USE_FREETYPE

RESOURCE(tt_face);

struct tt_face_rep: rep<tt_face> {
  bool bad_face = true;
  FT_Face ft_face = nullptr;
  FT_Byte *buffer = nullptr;
  int buffer_size = 0;
  ot_mathtable math_table;
  hashmap<string,ot_gsub_map> gsub_features;
  array<string> gsub_tag_list;
  bool          gsub_tags_ready= false;
  // the single and alternate substitutions of a GSUB feature (cached)
  ot_gsub_map& gsub_feature (string tag);
  array<string> gsub_tags ();
  ot_gpos_kern gpos_kern_table;
  bool         gpos_kern_ready= false;
  // the pair kerning of the GPOS 'kern' feature (cached)
  ot_gpos_kern gpos_kern ();
  hashmap<string,ot_gsub_ligatures> gsub_ligature_table;
  // the ligatures of a GSUB feature (cached)
  ot_gsub_ligatures& gsub_ligatures (string tag);
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
// the outline of the glyph of a character of a FreeType font, unhinted, as
// get makes its bitmap: false when the font is not a FreeType font (see
// tt_face.cpp for the commands and the units)
bool tt_glyph_outline (font_glyphs fng, int i, array<int>& cmds, array<double>& pts);
// the layers of the glyph of a character of a color font, as characters of
// the same font (native glyphs), with their colors; text[k] when layer k has
// the color of the text: false when the glyph has no colors
bool tt_color_layers (font_glyphs fng, int i, array<int>& codes,
                      array<color>& colors, array<bool>& text);
// whether the glyphs of a FreeType font may have colors (COLR, CPAL)
bool tt_color_font (font_glyphs fng);
// the advance of the glyph of a character in the units of the font, as
// the font has it (0 when unknown)
int tt_glyph_units_advance (font_glyphs fng, int i);
// the glyph of a character of a FreeType font in shades of grey, at shrinkf
// times less than the resolution of get (see tt_face.cpp): false when the
// font is not a FreeType font
bool tt_glyph_coverage (font_glyphs fng, int i, int shrinkf, int flags, double bold,
                        string& cov, int& w, int& h, int& x0, int& y0);
font_metric tt_font_metric (string family, int size, int hdpi, int vdpi);
//font_glyphs tt_font_glyphs (string family, int size, int hdpi, int vdpi);

#endif // USE_FREETYPE

#endif // defined TT_FACE_H
