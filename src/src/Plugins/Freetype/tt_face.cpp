
/******************************************************************************
* MODULE     : tt_face.cpp
* DESCRIPTION: resources for true type faces, gliefs and metrics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "font.hpp"
#include "tt_face.hpp"
#include "tt_file.hpp"
#include "tm_timer.hpp"
#include "sys_utils.hpp"
#include "colors.hpp"
#include "tt_tools.hpp"

#ifdef USE_FREETYPE

RESOURCE_CODE(tt_face);

/******************************************************************************
* Utilities
******************************************************************************/

inline int tt_round (int l) { return ((l+0x400020) >> 6) - 0x10000; }
inline SI tt_si (int l) { return l<<2; }

inline FT_UInt
decode_index (FT_Face face, int i) {
  if (i < 0xc000000) return ft_get_char_index (face, i);
  return i - 0xc000000;  
}

/******************************************************************************
* Freetype faces
******************************************************************************/

tt_face_rep::tt_face_rep (string name): rep<tt_face> (name) {
  bad_face= true;
  if (ft_initialize ()) return;
  if (DEBUG_VERBOSE)
    debug_fonts << "Loading True Type font " << name << "\n";
  url u= tt_font_find (name);
  if (is_none (u)) return;

  FILE *font_file = texmacs_fopen (concretize (u), "r");
  if (!font_file) {
    debug_fonts << "Can't load " << name << LF;
    return;
  }
  ssize_t fsize = texmacs_fsize (font_file);
  if (fsize <= 0) {
    texmacs_fclose (font_file);
    debug_fonts << "Can't load " << name << LF; 
    return;
  }

  if (fsize > (64 << 20)) {
    // a font too large to be kept in memory (Apple Color Emoji, 190 MB):
    // FreeType reads it from the file, and the tables TeXmacs parses
    // itself come from FreeType (gsub_ligatures)
    texmacs_fclose (font_file);
    c_string path (concretize (u));
    if (ft_new_face (ft_library, path, 0, &ft_face)) {
      debug_fonts << "Can't load font " << name << LF;
      return;
    }
    ft_select_charmap (ft_face, ft_encoding_adobe_custom);
    init_strike ();
    bad_face= false;
    return;
  }

  buffer = (FT_Byte*) malloc (fsize);
  ssize_t readed = texmacs_fread ((char*)buffer, fsize, font_file);
  if (readed != fsize) {
    free (buffer);
    buffer = nullptr;
    texmacs_fclose (font_file);
    debug_fonts << "Can't read " << name << LF;
    return;
  }
  texmacs_fclose(font_file);

  if (ft_new_memory_face (ft_library, buffer, fsize, 0, &ft_face)) {  
    debug_fonts << "Can't load font " << name << LF;
    free (buffer);
    buffer = nullptr;
    return; 
  }
  ft_select_charmap (ft_face, ft_encoding_adobe_custom);
  init_strike ();
  bad_face= false;
  buffer_size= (int) fsize;
}

ot_gsub_ligatures&
tt_face_rep::gsub_ligatures (string tag) {
  if (!gsub_ligature_table->contains (tag)) {
    ot_gsub_ligatures m;
    if (buffer != nullptr)
      m= parse_gsub_ligatures ((const char*) buffer, buffer_size, tag);
    else if (!bad_face && ft_load_sfnt_table != NULL) {
      // the font is not in memory: its GSUB table from FreeType
      FT_ULong len= 0;
      FT_ULong gtag= FT_MAKE_TAG ('G', 'S', 'U', 'B');
      if (!ft_load_sfnt_table (ft_face, gtag, 0, NULL, &len) && len > 0) {
        string gsub ((int) len);
        if (!ft_load_sfnt_table (ft_face, gtag, 0, (FT_Byte*) &gsub[0], &len))
          m= parse_gsub_ligatures_table (gsub, tag);
      }
    }
    gsub_ligature_table (tag)= m;
  }
  return gsub_ligature_table (tag);
}
int
tt_face_rep::morx_sequence (array<unsigned int> codes) {
  if (bad_face) return -1;
  if (!morx_ready) {
    morx_ready= true;
    if (buffer != nullptr)
      morx_table= ot_find_table ((const char*) buffer, buffer_size, "morx");
    else if (ft_load_sfnt_table != NULL) {
      FT_ULong len= 0;
      FT_ULong mtag= FT_MAKE_TAG ('m', 'o', 'r', 'x');
      if (!ft_load_sfnt_table (ft_face, mtag, 0, NULL, &len) && len > 0) {
        string m ((int) len);
        if (!ft_load_sfnt_table (ft_face, mtag, 0, (FT_Byte*) &m[0], &len))
          morx_table= m;
      }
    }
  }
  if (N(morx_table) == 0 || N(codes) < 2) return -1;
  string key;
  for (int k= 0; k < N(codes); k++) key << as_string ((int) codes[k]) << ",";
  if (morx_sequences->contains (key)) return morx_sequences [key];
  array<unsigned int> g;
  for (int k= 0; k < N(codes); k++) {
    unsigned int gi= ft_get_char_index (ft_face, codes[k]);
    if (gi == 0) { morx_sequences (key)= -1; return -1; }
    g << gi;
  }
  array<unsigned int> r= aat_morx_apply (morx_table, g, (int) ft_face->num_glyphs);
  int res= (N(r) == 1)? (int) r[0]: -1;
  morx_sequences (key)= res;
  return res;
}
void
tt_face_rep::init_strike () {
  if (FT_IS_SCALABLE (ft_face) || ft_face->num_fixed_sizes <= 0 ||
      ft_select_size == NULL) return;
  for (int k= 0; k < ft_face->num_fixed_sizes; k++) {
    int ppem= (int) ((ft_face->available_sizes[k].y_ppem + 32) >> 6);
    if (ppem > strike_ppem) { strike= k; strike_ppem= ppem; }
  }
  color_bitmaps= (strike >= 0 && FT_HAS_COLOR (ft_face));
}
bool
tt_face_rep::set_size (int size, int hdpi, int vdpi) {
  if (bad_face) return true;
  if (strike >= 0) return ft_select_size (ft_face, strike) != 0;
  return ft_set_char_size (ft_face, 0, size<<6, hdpi, vdpi) != 0;
}
double
tt_face_rep::strike_scale (int size, int dpi) {
  if (strike_ppem <= 0) return 1.0;
  return (size * dpi / 72.0) / strike_ppem;
}
bool
tt_face_rep::is_color () {
  array<int> layers, cols;
  (void) color_layers (0, layers, cols); // reads the palette
  return N(palette) > 0 || color_bitmaps;
}
bool
tt_face_rep::color_layers (unsigned int gid, array<int>& layers,
                           array<int>& colors) {
#ifdef FT_HAS_COLOR_LAYERS
  if (bad_face || ft_get_color_glyph_layer == NULL) return false;
  if (!colors_ready) {
    colors_ready= true;
    FT_Palette_Data pd;
    FT_Color* pal= NULL;
    if (!ft_palette_data_get (ft_face, &pd) && pd.num_palettes > 0 &&
        !ft_palette_select (ft_face, 0, &pal) && pal != NULL)
      for (int k= 0; k < (int) pd.num_palette_entries; k++)
        palette << rgb_color (pal[k].red, pal[k].green, pal[k].blue,
                              pal[k].alpha);
  }
  if (N(palette) == 0) return false;
  layers= array<int> ();
  colors= array<int> ();
  FT_LayerIterator it;
  it.p= NULL;
  FT_UInt lg, ci;
  while (ft_get_color_glyph_layer (ft_face, gid, &lg, &ci, &it)) {
    layers << (int) lg;
    // 0xFFFF: the color of the text
    colors << ((ci >= (FT_UInt) N(palette))? -1: (int) ci);
  }
  return N(layers) > 0;
#else
  (void) gid; (void) layers; (void) colors;
  return false;
#endif
}
tt_face_rep::~tt_face_rep () {
  std_warning << "tt_face_rep should not be deleted\n";
  if (ft_face) ft_done_face (ft_face);
  if (buffer) free (buffer);
}

tt_face
load_tt_face (string name) {
  bench_start ("load tt face");
  tt_face face= make (tt_face, name, tm_new<tt_face_rep> (name));
  bench_cumul ("load tt face");
  return face;
}

/******************************************************************************
* Font metrics
******************************************************************************/

// The characters which only modify the emoji around them: the variation
// selectors, the zero width joiner, the keycap and the tags (of the flags
// of regions)
static bool
is_emoji_mark (int c) {
  return (c >= 0xfe00 && c <= 0xfe0f) || c == 0x200d || c == 0x20e3 ||
         (c >= 0xe0020 && c <= 0xe007f);
}

static metric error_metric;

tt_font_metric_rep::tt_font_metric_rep (
  string name, string family, int size2, int hdpi2, int vdpi2):
  font_metric_rep (name), size (size2), hdpi (hdpi2), vdpi (vdpi2), fnm (NULL)
{
  face= load_tt_face (family);
  bad_font_metric= face->bad_face || face->set_size (size, hdpi, vdpi);
  if (bad_font_metric) return;

  error_metric->x1= error_metric->y1= 0;
  error_metric->x2= error_metric->y2= 0;
  error_metric->x3= error_metric->y3= 0;
  error_metric->x4= error_metric->y4= 0;
}

bool
tt_font_metric_rep::exists (int i) {
  if (face->bad_face) return false;
  if (fnm->contains (i)) return true;
  FT_UInt glyph_index= decode_index (face->ft_face, i);
  return glyph_index != 0;
}

// The metrics of a glyph of a font of bitmaps: those of its fixed size,
// scaled
static metric_struct*
strike_metric (tt_face face, FT_UInt glyph_index, int size, int hdpi,
               int vdpi) {
  if (face->set_size (size, hdpi, vdpi) ||
      ft_load_glyph (face->ft_face, glyph_index, FT_LOAD_COLOR)) return NULL;
  FT_Glyph_Metrics& gm= face->ft_face->glyph->metrics;
  double sx= face->strike_scale (size, hdpi);
  double sy= face->strike_scale (size, vdpi);
  SI w = tt_si ((int) (gm.width * sx)), h = tt_si ((int) (gm.height * sy));
  SI dx= tt_si ((int) (gm.horiBearingX * sx));
  SI dy= tt_si ((int) (gm.horiBearingY * sy));
  SI ll= tt_si ((int) (gm.horiAdvance * sx));
  metric_struct* M= tm_new<metric_struct> ();
  M->x1= 0;  M->y1= dy - h; M->x2= ll;     M->y2= dy;
  M->x3= dx; M->y3= dy - h; M->x4= dx + w; M->y4= dy;
  return M;
}

metric&
tt_font_metric_rep::get (int i) {
  if (!face->bad_face && !fnm->contains(i) && face->strike >= 0) {
    FT_UInt glyph_index= decode_index (face->ft_face, i);
    metric_struct* M= strike_metric (face, glyph_index, size, hdpi, vdpi);
    if (M == NULL) return error_metric;
    if (face->color_bitmaps && is_emoji_mark (i)) {
      // what is left of a sequence takes no room (as below)
      M->x2= M->x3= M->x4= 0;
      M->y1= M->y2= M->y3= M->y4= 0;
    }
    fnm(i)= (pointer) M;
  }
  if (!face->bad_face && !fnm->contains(i)) {
    ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
    FT_UInt glyph_index= decode_index (face->ft_face, i);
    if (ft_load_glyph (face->ft_face, glyph_index, FT_LOAD_DEFAULT))
      return error_metric;
    FT_GlyphSlot slot= face->ft_face->glyph;
    if (ft_render_glyph (slot, ft_render_mode_mono)) return error_metric;
    metric_struct* M= tm_new<metric_struct> ();
    fnm(i)= (pointer) M;
    int w= slot->bitmap.width;
    int h= slot->bitmap.rows;
    SI ww= w * PIXEL;
    SI hh= h * PIXEL;
    SI xw= tt_si (slot->metrics.width);
    SI xh= tt_si (slot->metrics.height);
    SI dx= tt_si (slot->metrics.horiBearingX);
    SI dy= tt_si (slot->metrics.horiBearingY);
    SI ll= tt_si (slot->metrics.horiAdvance);
    (void) xw;
    M->x1= 0;
    M->y1= dy - xh;
    M->x2= ll;
    M->y2= dy;
    M->x3= dx;
    M->y3= dy - hh;
    M->x4= dx + ww;
    M->y4= dy;
    array<int> layers, colors;
    if (face->color_layers (glyph_index, layers, colors)) {
      // a glyph of a color font, whose own outline is often empty: the
      // boxes of its layers
      for (int k= 0; k < N(layers); k++) {
        metric_struct* L= get (0xc000000 + layers[k]);
        if (k == 0) {
          M->y1= L->y1; M->y2= L->y2;
          M->x3= L->x3; M->y3= L->y3; M->x4= L->x4; M->y4= L->y4;
        }
        else {
          M->y1= min (M->y1, L->y1); M->y2= max (M->y2, L->y2);
          M->x3= min (M->x3, L->x3); M->y3= min (M->y3, L->y3);
          M->x4= max (M->x4, L->x4); M->y4= max (M->y4, L->y4);
        }
      }
    }
    else if (N(face->palette) > 0 && is_emoji_mark (i)) {
      // in a color font, what is left of a sequence of emoji which is not
      // one of its ligatures takes no room
      M->x2= M->x3= M->x4= 0;
      M->y1= M->y2= M->y3= M->y4= 0;
    }
    //cout << "Glyph " << i << " of " << res_name << "\n";
    //cout << "Logical : " << M->x1/PIXEL << ", " << M->y1/PIXEL
    //     << "; " << M->x2/PIXEL << ", " << M->y2/PIXEL << "\n";
    //cout << "Physical: " << M->x3/PIXEL << ", " << M->y3/PIXEL
    //     << "; " << M->x4/PIXEL << ", " << M->y4/PIXEL << "\n";
  }
  return *((metric*) ((void*) fnm [i]));
}

SI
tt_font_metric_rep::kerning (int left, int right) {
  if (face->bad_face || face->strike >= 0 ||
      !FT_HAS_KERNING (face->ft_face)) return 0;
  FT_Vector k;
  FT_UInt l= decode_index (face->ft_face, left);
  FT_UInt r= decode_index (face->ft_face, right);
  ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
  if (ft_get_kerning (face->ft_face, l, r, FT_KERNING_DEFAULT, &k)) return 0;
  return tt_si (k.x);
}

font_metric
tt_font_metric (string family, int size, int hdpi, int vdpi) {
  string name= family * as_string (size) * "@" * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
  return make (font_metric, name,
	       tm_new<tt_font_metric_rep> (name, family, size, hdpi, vdpi));
}

/******************************************************************************
* Font glyphs
******************************************************************************/

static glyph error_glyph;

tt_font_glyphs_rep::tt_font_glyphs_rep (
  string name, string family, int size2, int hdpi2, int vdpi2):
  font_glyphs_rep (name), size (size2),
  hdpi (hdpi2), vdpi (vdpi2), fng (glyph ())
{
  face= load_tt_face (family);
  bad_font_glyphs= face->bad_face || face->set_size (size, hdpi, vdpi);
  if (bad_font_glyphs) return;
}

int
tt_glyph_units_advance (font_glyphs fng, int i) {
  tt_font_glyphs_rep* g= dynamic_cast<tt_font_glyphs_rep*> (fng.operator-> ());
  if (g == NULL || g->face->bad_face) return 0;
  FT_Face face= g->face->ft_face;
  if (ft_load_glyph (face, decode_index (face, i), FT_LOAD_NO_SCALE)) return 0;
  return (int) face->glyph->metrics.horiAdvance;
}
bool
tt_color_bitmap (font_glyphs fng, int i, string& bgra, int& w, int& h,
                 int& left, int& top, int& adv) {
  tt_font_glyphs_rep* g= dynamic_cast<tt_font_glyphs_rep*> (fng.operator-> ());
  if (g == NULL || g->face->bad_face || !g->face->color_bitmaps) return false;
  tt_face face= g->face;
  if (face->set_size (g->size, g->hdpi, g->vdpi) ||
      ft_load_glyph (face->ft_face, decode_index (face->ft_face, i),
                     FT_LOAD_COLOR)) return false;
  FT_GlyphSlot slot= face->ft_face->glyph;
  FT_Bitmap& bm= slot->bitmap;
  adv= (int) ((slot->metrics.horiAdvance + 32) >> 6);
  if (bm.pixel_mode != FT_PIXEL_MODE_BGRA) return false;
  w= (int) bm.width; h= (int) bm.rows;
  left= slot->bitmap_left; top= slot->bitmap_top;
  bgra= string (4 * w * h);
  for (int y= 0; y < h; y++) {
    unsigned char* row= bm.buffer + y * bm.pitch;
    for (int k= 0; k < 4 * w; k++) bgra[4 * w * y + k]= (char) row[k];
  }
  return true;
}
double
tt_color_bitmap_scale (font_glyphs fng) {
  tt_font_glyphs_rep* g= dynamic_cast<tt_font_glyphs_rep*> (fng.operator-> ());
  if (g == NULL || g->face->bad_face || !g->face->color_bitmaps) return 0.0;
  return g->face->strike_scale (g->size, g->hdpi);
}
bool
tt_color_font (font_glyphs fng) {
  tt_font_glyphs_rep* g= dynamic_cast<tt_font_glyphs_rep*> (fng.operator-> ());
  if (g == NULL || g->face->bad_face) return false;
  return g->face->is_color ();
}
bool
tt_color_layers (font_glyphs fng, int i, array<int>& codes,
                 array<color>& colors, array<bool>& text) {
  tt_font_glyphs_rep* g= dynamic_cast<tt_font_glyphs_rep*> (fng.operator-> ());
  if (g == NULL || g->face->bad_face) return false;
  tt_face face= g->face;
  array<int> layers, cols;
  if (!face->color_layers (decode_index (face->ft_face, i), layers, cols))
    return false;
  codes= array<int> ();
  colors= array<color> ();
  text= array<bool> ();
  for (int k= 0; k < N(layers); k++) {
    codes  << (0xc000000 + layers[k]);
    colors << ((cols[k] < 0)? ((color) 0): face->palette[cols[k]]);
    text   << (cols[k] < 0);
  }
  return true;
}
glyph
tt_font_glyphs_rep::color_glyph_union (glyph G, array<int> layers) {
  array<glyph> gs;
  int x1= 0, y1= 0, x2= 0, y2= 0;
  for (int k= 0; k < N(layers); k++) {
    glyph L= get (0xc000000 + layers[k]);
    if (is_nil (L) || L->width * L->height == 0) continue;
    // the pixels of L from -xoff to width - xoff, and from yoff - height
    // to yoff upwards
    int lx1= -L->xoff, lx2= L->width - L->xoff;
    int ly1= L->yoff - L->height, ly2= L->yoff;
    if (N(gs) == 0) { x1= lx1; x2= lx2; y1= ly1; y2= ly2; }
    else {
      x1= min (x1, lx1); x2= max (x2, lx2);
      y1= min (y1, ly1); y2= max (y2, ly2);
    }
    gs << L;
  }
  if (N(gs) == 0) return G;
  glyph U (x2 - x1, y2 - y1, -x1, y2);
  U->index = G->index;
  U->lwidth= G->lwidth;
  for (int k= 0; k < N(gs); k++) {
    glyph L= gs[k];
    int dx= -L->xoff - x1, dy= y2 - L->yoff;
    for (int y= 0; y < L->height; y++)
      for (int x= 0; x < L->width; x++)
        if (L->get_1 (x, y)) U->set_1 (x + dx, y + dy, 1);
  }
  return U;
}

glyph&
tt_font_glyphs_rep::get (int i) {
  if (!face->bad_face && !fng->contains(i) && face->strike >= 0) {
    // a font of bitmaps: in one color, where the bitmap of its fixed size
    // covers more than half of a pixel of the size asked for (the
    // renderers draw it in color, see unicode_font.cpp)
    fng (i)= error_glyph;
    string bgra;
    int w, h, left, top, adv;
    if (tt_color_bitmap (this, i, bgra, w, h, left, top, adv) && w > 0 && h > 0) {
      double s= face->strike_scale (size, hdpi);
      int W= max (1, (int) (w * s + 0.5)), H= max (1, (int) (h * s + 0.5));
      glyph G (W, H, -((int) (left * s + 0.5)), (int) (top * s + 0.5));
      for (int y= 0; y < H; y++)
        for (int x= 0; x < W; x++) {
          int sx= min (w - 1, (int) ((x + 0.5) / s));
          int sy= min (h - 1, (int) ((y + 0.5) / s));
          if (((unsigned char) bgra[4 * (sy * w + sx) + 3]) >= 128)
            G->set_1 (x, y, 1);
        }
      G->index = decode_index (face->ft_face, i);
      G->lwidth= (int) (adv * s + 0.5);
      fng (i)= G;
    }
    else if (decode_index (face->ft_face, i) != 0) {
      glyph B (1, 1, 0, 0);    // nothing to draw, but text in a PDF
      B->index = decode_index (face->ft_face, i);
      fng (i)= B;
    }
  }
  if (!face->bad_face && !fng->contains(i)) {
    ft_set_char_size (face->ft_face, 0, size<<6, hdpi, vdpi);
    FT_UInt glyph_index= decode_index (face->ft_face, i);
    if (ft_load_glyph (face->ft_face, glyph_index, FT_LOAD_DEFAULT))
      return error_glyph;
    FT_GlyphSlot slot= face->ft_face->glyph;
    if (ft_render_glyph (slot, ft_render_mode_mono)) return error_glyph;

    int w= slot->bitmap.width;
    int h= slot->bitmap.rows;
    int ox= tt_round (slot->metrics.horiBearingX);
    int oy= tt_round (slot->metrics.horiBearingY);
    int pitch= slot->bitmap.pitch;
    unsigned char *buf= slot->bitmap.buffer;
    if (pitch<0) buf -= pitch*h;
    int x, y;
    glyph G (w, h, -ox, oy);
    // mg:
    // the index variable is used by code who need the glyph_index for unicode characters
    // to locate the right glyph in the font file
    G->index = (face->ft_face->charmap &&
                face->ft_face->charmap->encoding == FT_ENCODING_UNICODE) ?
                  glyph_index : i;
    G->lwidth= (tt_si (slot->metrics.horiAdvance)+(PIXEL>>1))/PIXEL;

    for (y=0; y<h; y++) {
      for (x=0; x<w; x++) {
	unsigned char c= buf[x>>3];
	G->set_1 (x, y, (c >> (7-(x&7))) & 1);
      }
      buf += pitch;
    }
    //cout << "Glyph " << i << " of " << res_name << "\n";
    //cout << G << "\n";
    array<int> layers, colors;
    if (face->color_layers (glyph_index, layers, colors))
      // a glyph of a color font: in one color, the union of its layers (the
      // renderers draw the layers in their colors, see unicode_font.cpp)
      G= color_glyph_union (G, layers);
    if (G->width * G->height == 0 && glyph_index != 0 &&
        N(face->palette) > 0) {
      // a character of a color font which draws nothing by itself (U+200D,
      // U+FE0F, the base of a keycap): a blank glyph, which a PDF writes
      // as the text of an emoji (unicode_font.cpp, draw_color_glyph)
      glyph B (1, 1, 0, 0);
      B->index = G->index;
      B->lwidth= G->lwidth;
      G= B;
    }
    if (G->width * G->height == 0) G= error_glyph;
    fng(i)= G;
  }
  return fng(i);
}

font_glyphs
tt_font_glyphs (string family, int size, int hdpi, int vdpi) {
  string name=
    family * ":" * as_string (size) * "." * as_string (hdpi);
  if (vdpi != hdpi) name << "x" << as_string (vdpi);
  name << "tt";
  return make (font_glyphs, name,
	       tm_new<tt_font_glyphs_rep> (name, family, size, hdpi, vdpi));
}

#endif // USE_FREETYPE
