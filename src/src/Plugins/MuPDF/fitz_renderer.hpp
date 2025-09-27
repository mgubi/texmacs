/******************************************************************************
* MODULE     : fitz_renderer.hpp
* DESCRIPTION: Direct Fitz device renderer
* COPYRIGHT  : (C) 2025
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FITZ_RENDERER_HPP
#define FITZ_RENDERER_HPP

#include "basic_renderer.hpp"

#include <mupdf/fitz.h>

/******************************************************************************
* Global Fitz context access
******************************************************************************/

fz_context* get_fitz_context ();

/******************************************************************************
* Fitz device renderer - uses MuPDF's core Fitz device API directly
******************************************************************************/

class fitz_renderer_rep: public basic_renderer_rep {
protected:
  // Core Fitz components
  fz_context    *ctx;
  fz_pixmap     *pixmap;
  fz_device     *device;
  fz_matrix     transform;

  // Graphics state
  color         fg, bg;
  SI            lw;
  float         current_width;

  // Path and color management
  fz_path       *current_path;
  fz_colorspace *colorspace_rgb;
  fz_colorspace *colorspace_gray;

  // Font handling
  fz_text       *current_text;
  fz_font       *current_font;
  float         font_size;
  string        current_font_name;

  // Native font cache
  hashmap<string, fz_font*> native_fonts;

  // Pattern support
  hashmap<tree, fz_shade*> pattern_cache;
  fz_shade *current_fill_pattern;
  fz_shade *current_stroke_pattern;

  // Tiling pattern support (for manual pattern implementation)
  struct pattern_info {
    fz_pixmap *pixmap;
    SI width, height;
    url source_url;
  };
  hashmap<tree, pattern_info> tile_pattern_cache;
  tree current_fill_pattern_key;
  tree current_stroke_pattern_key;

  // Coordinate conversion helpers
  float to_fitz_x (SI x) {
    x += ox;
    if (x >= 0) x = x / pixel; else x = (x - pixel + 1) / pixel;
    return (float)x;
  }

  float to_fitz_y (SI y) {
    y += oy;
    if (y >= 0) y = y / pixel; else y = (y - pixel + 1) / pixel;
    return (float)y;
  }

  // Color conversion
  void fitz_color_from_color (color c, float *fz_color, int *alpha);
  fz_colorspace* get_colorspace_for_color (color c);

  // Path operations
  void begin_path ();
  void end_path ();
  void stroke_current_path ();
  void fill_current_path ();

  // Text operations
  void begin_text ();
  void end_text ();
  fz_font* load_fitz_font (string fontname);
  float extract_font_size (string fontname);
  void setup_font (font_glyphs fn);
  unsigned int decode_glyph_index (fz_font* font, int char_code);

  // Graphics state management
  void select_stroke_color (color c);
  void select_fill_color (color c);
  void select_line_width (SI w);

  // Pattern management
  void register_pattern (brush br, SI pixel);
  void select_fill_pattern (brush br);
  void select_stroke_pattern (brush br);
  fz_shade* create_pattern_shade (url u, SI w, SI h, tree eff, SI pixel);

public:
  fitz_renderer_rep (int w = 0, int h = 0);
  ~fitz_renderer_rep ();
  void* get_handle ();

  void set_zoom_factor (double zoom);

  void begin (void* handle);
  void end ();

  void get_extents (SI& w, SI& h);

  void set_transformation (frame fr);
  void reset_transformation ();

  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore = false);
  void set_pencil (pencil p);
  void set_brush (brush b);
  void set_background (brush bg);

  void draw (int char_code, font_glyphs fn, SI x, SI y);
  void draw_scalable (scalable im, SI x, SI y, int alpha);

  void line (SI x1, SI y1, SI x2, SI y2);
  void lines (array<SI> x, array<SI> y);
  void clear (SI x1, SI y1, SI x2, SI y2);
  void fill (SI x1, SI y1, SI x2, SI y2);
  void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void polygon (array<SI> x, array<SI> y, bool convex = true);

  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);
  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);

  void draw_picture (picture pict, SI x, SI y, int alpha);

  friend class fitz_proxy_renderer_rep;
};

/******************************************************************************
* Factory functions
******************************************************************************/

renderer fitz_renderer (int w, int h);
renderer fitz_renderer (picture& p, double zoom);

void set_fitz_renderer (renderer ren);
renderer get_fitz_renderer ();

fitz_renderer_rep* the_fitz_renderer ();

/******************************************************************************
* Picture renderer and native picture support
******************************************************************************/

renderer picture_renderer (picture p, double zoomf);

#ifdef FITZ_RENDERER
picture as_native_picture (picture pict);
picture native_picture (int w, int h, int ox, int oy);
picture load_picture (url u, int w, int h, tree eff, int pixel);
void save_picture (url dest, picture p);
#endif

class fitz_shadow_renderer_rep: public fitz_renderer_rep {
public:
  fitz_renderer_rep *master;

public:
  fitz_shadow_renderer_rep (int w, int h);
  ~fitz_shadow_renderer_rep ();
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};

class fitz_proxy_renderer_rep: public fitz_renderer_rep {
public:
  fitz_renderer_rep *base;

public:
  fitz_proxy_renderer_rep (fitz_renderer_rep *_base);
  ~fitz_proxy_renderer_rep ();
  void new_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};

#endif // defined FITZ_RENDERER_HPP