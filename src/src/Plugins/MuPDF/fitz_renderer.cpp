/******************************************************************************
* MODULE     : fitz_renderer.cpp
* DESCRIPTION: Direct Fitz device renderer implementation
* COPYRIGHT  : (C) 2025
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "fitz_renderer.hpp"
#include "fitz_picture.hpp"
#include "analyze.hpp"
#include "image_files.hpp"
#include "file.hpp"
#include "scheme.hpp"
#include "frame.hpp"
#include "iterator.hpp"

#include "Freetype/tt_file.hpp"
#include "Freetype/free_type.hpp"

// Global Fitz context - shared across all renderers
static fz_context* global_fitz_context = NULL;

fz_context*
get_fitz_context () {
  if (!global_fitz_context) {
    global_fitz_context = fz_new_context (NULL, NULL, FZ_STORE_UNLIMITED);
    if (!global_fitz_context) {
      convert_error << "fitz_renderer: Failed to create Fitz context" << LF;
    }
  }
  return global_fitz_context;
}

void
cleanup_fitz_context () {
  if (global_fitz_context) {
    fz_drop_context (global_fitz_context);
    global_fitz_context = NULL;
  }
}

/******************************************************************************
* Character and image caches
******************************************************************************/

static hashmap<basic_character, fz_image*> character_image_cache;
static hashmap<tree, fz_image*> image_cache;
static hashmap<string, fz_font*> global_font_cache;
static hashmap<tree, fz_shade*> pattern_shade_cache;

void del_obj_fitz_renderer (void) {
  character_image_cache = hashmap<basic_character, fz_image*> ();
  image_cache = hashmap<tree, fz_image*> ();

  // Clean up caches
  fz_context *ctx = get_fitz_context ();
  if (ctx) {
    // Clean up font cache
    iterator<string> font_it = iterate (global_font_cache);
    while (font_it->busy ()) {
      string fontname = font_it->next ();
      if (global_font_cache[fontname]) {
        fz_drop_font (ctx, global_font_cache[fontname]);
      }
    }

    // Clean up image cache
    iterator<tree> image_it = iterate (image_cache);
    while (image_it->busy ()) {
      tree key = image_it->next ();
      if (image_cache[key]) {
        fz_drop_image (ctx, image_cache[key]);
      }
    }

    // Clean up character image cache
    iterator<basic_character> char_it = iterate (character_image_cache);
    while (char_it->busy ()) {
      basic_character key = char_it->next ();
      if (character_image_cache[key]) {
        fz_drop_image (ctx, character_image_cache[key]);
      }
    }

    // Clean up pattern shade cache
    iterator<tree> pattern_it = iterate (pattern_shade_cache);
    while (pattern_it->busy ()) {
      tree key = pattern_it->next ();
      if (pattern_shade_cache[key]) {
        fz_drop_shade (ctx, pattern_shade_cache[key]);
      }
    }
  }
  global_font_cache = hashmap<string, fz_font*> ();
  image_cache = hashmap<tree, fz_image*> ();
  character_image_cache = hashmap<basic_character, fz_image*> ();
  pattern_shade_cache = hashmap<tree, fz_shade*> ();

  cleanup_fitz_context ();
}

/******************************************************************************
* fitz_renderer_rep implementation
******************************************************************************/

fitz_renderer_rep::fitz_renderer_rep (int w2, int h2)
  : basic_renderer_rep (true, w2, h2),
    ctx (NULL), pixmap (NULL), device (NULL),
    fg (-1), bg (-1), lw (-1), current_width (-1.0),
    current_path (NULL), colorspace_rgb (NULL), colorspace_gray (NULL),
    current_text (NULL), current_font (NULL), font_size (10.0),
    current_font_name (""), current_fill_pattern (NULL), current_stroke_pattern (NULL)
{
  ctx = get_fitz_context ();
  if (ctx) {
    colorspace_rgb = fz_device_rgb (ctx);
    colorspace_gray = fz_device_gray (ctx);
    transform = fz_identity;
  }
  reset_zoom_factor ();
}

fitz_renderer_rep::~fitz_renderer_rep () {
  end ();
}

void*
fitz_renderer_rep::get_handle () {
  return (void*) this;
}

void
fitz_renderer_rep::get_extents (SI& w, SI& h) {
  if (pixmap) {
    // Convert from raw pixel dimensions to TeXmacs scaled pixels
    w = fz_pixmap_width (ctx, pixmap) * pixel;
    h = fz_pixmap_height (ctx, pixmap) * pixel;
  } else {
    // Use the renderer's dimensions converted to scaled pixels
    w = this->w * pixel;
    h = this->h * pixel;
  }
}

void
fitz_renderer_rep::set_zoom_factor (double zoom) {
  renderer_rep::set_zoom_factor (retina_factor * zoom);
  retina_pixel = pixel * retina_factor;
}

void
fitz_renderer_rep::begin (void* handle) {
  fz_pixmap *_pixmap = static_cast<fz_pixmap*>(handle);
  if (_pixmap && ctx) {
    if (device) end ();

    pixmap = _pixmap;
    fz_keep_pixmap (ctx, pixmap);

    w = fz_pixmap_width (ctx, pixmap);
    h = fz_pixmap_height (ctx, pixmap);

    device = fz_new_draw_device (ctx, transform, pixmap);

    fg = -1;
    bg = -1;
    lw = -1;
    current_width = -1.0;
    current_font_name = "";

    if (current_path) {
      fz_drop_path (ctx, current_path);
      current_path = NULL;
    }

    if (current_text) {
      fz_drop_text (ctx, current_text);
      current_text = NULL;
    }
  } else {
    convert_error << "fitz_renderer: Invalid pixmap or context" << LF;
  }
}

void
fitz_renderer_rep::end () {
  end_text ();
  end_path ();

  // Clean up tile pattern cache
  iterator<tree> it = iterate (tile_pattern_cache);
  while (it->busy()) {
    tree key = it->next();
    pattern_info info = tile_pattern_cache [key];
    if (info.pixmap) {
      fz_drop_pixmap (ctx, info.pixmap);
    }
  }
  tile_pattern_cache = hashmap<tree, pattern_info> ();

  if (device) {
    fz_close_device (ctx, device);
    fz_drop_device (ctx, device);
    device = NULL;
  }

  if (pixmap) {
    fz_drop_pixmap (ctx, pixmap);
    pixmap = NULL;
  }
}

/******************************************************************************
* Color management
******************************************************************************/

void
fitz_renderer_rep::fitz_color_from_color (color c, float *fz_color, int *alpha) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);

  fz_color[0] = ((float) r) / 255.0f;
  fz_color[1] = ((float) g) / 255.0f;
  fz_color[2] = ((float) b) / 255.0f;
  *alpha = a;
}

fz_colorspace*
fitz_renderer_rep::get_colorspace_for_color (color c) {
  // For simplicity, always use RGB colorspace
  return colorspace_rgb;
}

void
fitz_renderer_rep::select_stroke_color (color c) {
  fg = c;
}

void
fitz_renderer_rep::select_fill_color (color c) {
  fg = c;
}

void
fitz_renderer_rep::select_line_width (SI w) {
  lw = w;
  current_width = (float)w / (float)pixel;
}

/******************************************************************************
* Path operations
******************************************************************************/

void
fitz_renderer_rep::begin_path () {
  if (current_path) {
    fz_drop_path (ctx, current_path);
  }
  current_path = fz_new_path (ctx);
}

void
fitz_renderer_rep::end_path () {
  if (current_path) {
    fz_drop_path (ctx, current_path);
    current_path = NULL;
  }
}

void
fitz_renderer_rep::stroke_current_path () {
  if (!device || !current_path) return;

  fz_stroke_state stroke;
  stroke.start_cap = FZ_LINECAP_ROUND;
  stroke.dash_cap = FZ_LINECAP_ROUND;
  stroke.end_cap = FZ_LINECAP_ROUND;
  stroke.linejoin = FZ_LINEJOIN_ROUND;
  stroke.linewidth = current_width;
  stroke.miterlimit = 10.0f;
  stroke.dash_phase = 0.0f;
  stroke.dash_len = 0;

  if (current_stroke_pattern) {
    // For pattern strokes, we need to implement custom logic since MuPDF
    // doesn't directly support pattern strokes. For now, fall back to solid stroke.
    float fz_color[3];
    int alpha;
    fitz_color_from_color (fg, fz_color, &alpha);
    fz_stroke_path (ctx, device, current_path, &stroke, transform,
                    colorspace_rgb, fz_color, (float)alpha/255.0f, fz_default_color_params);
  } else {
    // Use solid color stroke
    float fz_color[3];
    int alpha;
    fitz_color_from_color (fg, fz_color, &alpha);

    fz_stroke_path (ctx, device, current_path, &stroke, transform,
                    get_colorspace_for_color (fg), fz_color,
                    ((float)alpha) / 255.0f, fz_default_color_params);
  }
}

void
fitz_renderer_rep::fill_current_path () {
  if (!device || !current_path) return;

  if (current_fill_pattern) {
    // Use pattern fill
    fz_try (ctx) {
      fz_fill_shade (ctx, device, current_fill_pattern, transform, 1.0f, fz_default_color_params);
    }
    fz_catch (ctx) {
      convert_warning << "fitz_renderer: Failed to fill with pattern" << LF;
    }
  } else {
    // Use solid color fill
    float fz_color[3];
    int alpha;
    fitz_color_from_color (fg, fz_color, &alpha);

    fz_fill_path (ctx, device, current_path, 0, transform,
                  get_colorspace_for_color (fg), fz_color,
                  ((float)alpha) / 255.0f, fz_default_color_params);
  }
}

/******************************************************************************
* Text operations
******************************************************************************/

void
fitz_renderer_rep::begin_text () {
  if (current_text) {
    fz_drop_text (ctx, current_text);
  }
  current_text = fz_new_text (ctx);
}

void
fitz_renderer_rep::end_text () {
  if (current_text) {
    fz_drop_text (ctx, current_text);
    current_text = NULL;
  }
}

fz_font*
fitz_renderer_rep::load_fitz_font (string fontname) {
  // Check global cache first
  if (global_font_cache->contains (fontname)) {
    return global_font_cache [fontname];
  }

  // Extract font family name (before colon if present)
  int pos = search_forwards (":", fontname);
  string fname = (pos == -1) ? fontname : fontname (0, pos);

  // Find font file using FreeType system
  url u = tt_font_find (fname);
  fz_font* font = NULL;

  if (!is_none (u)) {
    c_string path (concretize (u));
    fz_try (ctx) {
      font = fz_new_font_from_file (ctx, NULL, path, 0, 0);
      if (font) {
        // Set up proper encoding for FreeType fonts
        // This matches the MuPDF renderer approach
        FT_Face face = (FT_Face) font->ft_face;
        if (face) {
          ft_select_charmap (face, ft_encoding_adobe_custom);
        }
      }
    }
    fz_catch (ctx) {
      convert_warning << "fitz_renderer: Failed to load font " << fname
                      << " from " << u << LF;
      font = NULL;
    }
  }

  // Fallback to base14 font if loading failed
  if (!font) {
    fz_try (ctx) {
      font = fz_new_base14_font (ctx, "Helvetica");
    }
    fz_catch (ctx) {
      convert_error << "fitz_renderer: Failed to load fallback font" << LF;
      font = NULL;
    }
  }

  // Cache the result (even if NULL)
  global_font_cache (fontname) = font;
  return font;
}

float
fitz_renderer_rep::extract_font_size (string fontname) {
  // Extract font size from fontname - same logic as MuPDF renderer
  int pos = search_backwards (".", fontname);
  if (pos == -1) return 10.0f; // Default size

  int szpos = pos - 1;
  while ((szpos > 0) && is_numeric (fontname[szpos - 1])) szpos--;

  double size = as_double (fontname (szpos, pos));
  if (size == 0) size = 10;

  int end = pos + 1;
  while (end < N(fontname) && is_numeric (fontname[end])) end++;

  double dpi = as_double (fontname (pos + 1, end));
  if (dpi == 0) dpi = 72.0; // Default DPI

  double mag = size * (dpi / 72.0);
  return (float) mag;
}

unsigned int
fitz_renderer_rep::decode_glyph_index (fz_font* font, int char_code) {
  // Same logic as MuPDF renderer for glyph index decoding
  if (!font || !font->ft_face) return 0;

  FT_Face face = (FT_Face) font->ft_face;

  // Use the same decode_index logic as MuPDF renderer
  if (char_code < 0xc000000) {
    return ft_get_char_index (face, char_code);
  } else {
    return char_code - 0xc000000;
  }
}

void
fitz_renderer_rep::setup_font (font_glyphs fn) {
  string fontname = fn->res_name;
  if (current_font_name != fontname) {
    current_font_name = fontname;

    // Release previous font reference
    if (current_font) {
      // Don't drop here - it's managed by the global cache
      current_font = NULL;
    }

    // Load new font
    current_font = load_fitz_font (fontname);
    font_size = extract_font_size (fontname);
  }
}

/******************************************************************************
* Rendering state management
******************************************************************************/

void
fitz_renderer_rep::set_transformation (frame fr) {
  // For simplicity, we'll just store the identity transform
  // A full implementation would properly convert the frame to fz_matrix
  transform = fz_identity;
}

void
fitz_renderer_rep::reset_transformation () {
  transform = fz_identity;
}

void
fitz_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);

  if (!device) return;

  // Convert coordinates and set clipping rectangle
  outer_round (x1, y1, x2, y2);

  fz_rect clip_rect;
  clip_rect.x0 = to_fitz_x (min (x1, x2));
  clip_rect.y0 = to_fitz_y (min (y1, y2));
  clip_rect.x1 = to_fitz_x (max (x1, x2));
  clip_rect.y1 = to_fitz_y (max (y1, y2));

  begin_path ();
  fz_moveto (ctx, current_path, clip_rect.x0, clip_rect.y0);
  fz_lineto (ctx, current_path, clip_rect.x1, clip_rect.y0);
  fz_lineto (ctx, current_path, clip_rect.x1, clip_rect.y1);
  fz_lineto (ctx, current_path, clip_rect.x0, clip_rect.y1);
  fz_closepath (ctx, current_path);

  fz_clip_path (ctx, device, current_path, 0, transform, clip_rect);
  end_path ();
}

void
fitz_renderer_rep::set_pencil (pencil pen2) {
  pen = pen2;
  lw = pen->get_width ();
  select_line_width (lw);
  color c = pen->get_color ();
  fg = c;
  select_stroke_color (c);
  select_fill_color (c);
}

void
fitz_renderer_rep::set_brush (brush br) {
  fg_brush = br;

  if (!is_nil (br) && br->get_type () == brush_pattern) {
    // Handle pattern brushes
    select_fill_pattern (br);
  } else {
    // Handle solid color brushes
    current_fill_pattern = NULL;
    pen = pencil (br);
    set_pencil (pen);
  }
}

void
fitz_renderer_rep::set_background (brush b) {
  bg_brush = b;
  bg = b->get_color ();
}

/******************************************************************************
* Drawing primitives
******************************************************************************/

void
fitz_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  if (!device) return;

  begin_path ();
  fz_moveto (ctx, current_path, to_fitz_x (x1), to_fitz_y (y1));
  fz_lineto (ctx, current_path, to_fitz_x (x2), to_fitz_y (y2));
  stroke_current_path ();
  end_path ();
}

void
fitz_renderer_rep::lines (array<SI> x, array<SI> y) {
  if (!device || N(x) == 0 || N(y) != N(x)) return;

  begin_path ();
  fz_moveto (ctx, current_path, to_fitz_x (x[0]), to_fitz_y (y[0]));
  for (int i = 1; i < N(x); i++) {
    fz_lineto (ctx, current_path, to_fitz_x (x[i]), to_fitz_y (y[i]));
  }
  stroke_current_path ();
  end_path ();
}

void
fitz_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  if (!device) return;

  // Save current color and set to background
  color old_fg = fg;
  fg = bg;

  begin_path ();
  float xx1 = to_fitz_x (min (x1, x2));
  float yy1 = to_fitz_y (min (y1, y2));
  float xx2 = to_fitz_x (max (x1, x2));
  float yy2 = to_fitz_y (max (y1, y2));

  fz_moveto (ctx, current_path, xx1, yy1);
  fz_lineto (ctx, current_path, xx2, yy1);
  fz_lineto (ctx, current_path, xx2, yy2);
  fz_lineto (ctx, current_path, xx1, yy2);
  fz_closepath (ctx, current_path);

  fill_current_path ();
  end_path ();

  // Restore original color
  fg = old_fg;
}

void
fitz_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if (!device || x1 >= x2 || y1 >= y2) return;

  if (current_fill_pattern) {
    // For shade-based pattern fills
    fz_rect clip_rect;
    clip_rect.x0 = to_fitz_x (min (x1, x2));
    clip_rect.y0 = to_fitz_y (min (y1, y2));
    clip_rect.x1 = to_fitz_x (max (x1, x2));
    clip_rect.y1 = to_fitz_y (max (y1, y2));

    fz_try (ctx) {
      fz_path *clip_path = fz_new_path (ctx);
      fz_moveto (ctx, clip_path, clip_rect.x0, clip_rect.y0);
      fz_lineto (ctx, clip_path, clip_rect.x1, clip_rect.y0);
      fz_lineto (ctx, clip_path, clip_rect.x1, clip_rect.y1);
      fz_lineto (ctx, clip_path, clip_rect.x0, clip_rect.y1);
      fz_closepath (ctx, clip_path);
      fz_clip_path (ctx, device, clip_path, 0, transform, fz_infinite_rect);
      fz_drop_path (ctx, clip_path);
      fz_fill_shade (ctx, device, current_fill_pattern, transform, 1.0f, fz_default_color_params);
      fz_pop_clip (ctx, device);
    }
    fz_catch (ctx) {
      convert_warning << "fitz_renderer: Failed to fill rectangle with pattern" << LF;
    }
  } else if (!is_nil (current_fill_pattern_key) && tile_pattern_cache->contains (current_fill_pattern_key)) {
    // For tiling pattern fills - manual tiling implementation
    pattern_info info = tile_pattern_cache [current_fill_pattern_key];

    float fill_x0 = to_fitz_x (min (x1, x2));
    float fill_y0 = to_fitz_y (min (y1, y2));
    float fill_x1 = to_fitz_x (max (x1, x2));
    float fill_y1 = to_fitz_y (max (y1, y2));

    // Get pixmap dimensions for tiling calculations
    int pixmap_w = fz_pixmap_width (ctx, info.pixmap);
    int pixmap_h = fz_pixmap_height (ctx, info.pixmap);

    // Get pattern tile size in Fitz coordinates
    float tile_w = (float) info.width / pixel;
    float tile_h = (float) info.height / pixel;

    fz_try (ctx) {
      // Set up clipping for the fill area
      fz_path *clip_path = fz_new_path (ctx);
      fz_moveto (ctx, clip_path, fill_x0, fill_y0);
      fz_lineto (ctx, clip_path, fill_x1, fill_y0);
      fz_lineto (ctx, clip_path, fill_x1, fill_y1);
      fz_lineto (ctx, clip_path, fill_x0, fill_y1);
      fz_closepath (ctx, clip_path);
      fz_clip_path (ctx, device, clip_path, 0, transform, fz_infinite_rect);
      fz_drop_path (ctx, clip_path);

      // Calculate tiling grid (similar to pdf_show_pattern)
      int start_x = (int) floorf (fill_x0 / tile_w);
      int start_y = (int) floorf (fill_y0 / tile_h);
      int end_x = (int) ceilf (fill_x1 / tile_w);
      int end_y = (int) ceilf (fill_y1 / tile_h);

      // Draw tiles by converting pixmap to image for each tile
      for (int ty = start_y; ty < end_y; ty++) {
        for (int tx = start_x; tx < end_x; tx++) {
          float tile_x = tx * tile_w;
          float tile_y = ty * tile_h;

          // Create a temporary image from the pixmap for this tile
          fz_image *tile_image = fz_new_image_from_pixmap (ctx, info.pixmap, NULL);

          fz_matrix tile_matrix = fz_scale (tile_w / pixmap_w, tile_h / pixmap_h);
          tile_matrix = fz_pre_translate (tile_matrix, tile_x, tile_y);
          tile_matrix = fz_concat (tile_matrix, transform);

          fz_fill_image (ctx, device, tile_image, tile_matrix, 1.0f, fz_default_color_params);

          fz_drop_image (ctx, tile_image);
        }
      }

      fz_pop_clip (ctx, device);
    }
    fz_catch (ctx) {
      convert_warning << "fitz_renderer: Failed to tile pattern fill" << LF;
    }
  } else {
    // Standard solid fill
    begin_path ();
    float xx1 = to_fitz_x (min (x1, x2));
    float yy1 = to_fitz_y (min (y1, y2));
    float xx2 = to_fitz_x (max (x1, x2));
    float yy2 = to_fitz_y (max (y1, y2));

    fz_moveto (ctx, current_path, xx1, yy1);
    fz_lineto (ctx, current_path, xx2, yy1);
    fz_lineto (ctx, current_path, xx2, yy2);
    fz_lineto (ctx, current_path, xx1, yy2);
    fz_closepath (ctx, current_path);

    fill_current_path ();
    end_path ();
  }
}

void
fitz_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (!device) return;

  // Convert to Fitz coordinates
  float cx = (to_fitz_x (x1) + to_fitz_x (x2)) / 2.0f;
  float cy = (to_fitz_y (y1) + to_fitz_y (y2)) / 2.0f;
  float rx = abs (to_fitz_x (x2) - to_fitz_x (x1)) / 2.0f;
  float ry = abs (to_fitz_y (y2) - to_fitz_y (y1)) / 2.0f;

  begin_path ();
  // Simplified arc using a circle approximation
  // A full implementation would use proper arc curves
  int num_segments = 16;
  float start_angle = (float)alpha * M_PI / (180.0f * 64.0f);
  float end_angle = start_angle + (float)delta * M_PI / (180.0f * 64.0f);

  for (int i = 0; i <= num_segments; i++) {
    float angle = start_angle + (end_angle - start_angle) * i / num_segments;
    float x = cx + rx * cos (angle);
    float y = cy + ry * sin (angle);

    if (i == 0) {
      fz_moveto (ctx, current_path, x, y);
    } else {
      fz_lineto (ctx, current_path, x, y);
    }
  }

  stroke_current_path ();
  end_path ();
}

void
fitz_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (!device) return;

  // Similar to arc() but with filling
  float cx = (to_fitz_x (x1) + to_fitz_x (x2)) / 2.0f;
  float cy = (to_fitz_y (y1) + to_fitz_y (y2)) / 2.0f;
  float rx = abs (to_fitz_x (x2) - to_fitz_x (x1)) / 2.0f;
  float ry = abs (to_fitz_y (y2) - to_fitz_y (y1)) / 2.0f;

  begin_path ();
  int num_segments = 16;
  float start_angle = (float)alpha * M_PI / (180.0f * 64.0f);
  float end_angle = start_angle + (float)delta * M_PI / (180.0f * 64.0f);

  fz_moveto (ctx, current_path, cx, cy);
  for (int i = 0; i <= num_segments; i++) {
    float angle = start_angle + (end_angle - start_angle) * i / num_segments;
    float x = cx + rx * cos (angle);
    float y = cy + ry * sin (angle);
    fz_lineto (ctx, current_path, x, y);
  }
  fz_closepath (ctx, current_path);

  fill_current_path ();
  end_path ();
}

void
fitz_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  if (!device || N(x) == 0 || N(y) != N(x)) return;

  begin_path ();
  fz_moveto (ctx, current_path, to_fitz_x (x[0]), to_fitz_y (y[0]));
  for (int i = 1; i < N(x); i++) {
    fz_lineto (ctx, current_path, to_fitz_x (x[i]), to_fitz_y (y[i]));
  }
  fz_closepath (ctx, current_path);

  fill_current_path ();
  end_path ();
}

/******************************************************************************
* Text rendering
******************************************************************************/

void
fitz_renderer_rep::draw (int char_code, font_glyphs fn, SI x, SI y) {
  if (!device) return;

  // Setup font for this glyph
  setup_font (fn);

  // Try native font rendering first
  if (current_font) {
    // Use native Fitz text rendering
    unsigned int glyph_id = decode_glyph_index (current_font, char_code);
    if (glyph_id != 0) {
      // Create text object for this single glyph
      begin_text ();

      float x_pos = to_fitz_x (x);
      float y_pos = to_fitz_y (y);
      float scaled_size = font_size / std_shrinkf;

      // Add glyph to text object
      fz_text* text = fz_new_text (ctx);
      fz_try (ctx) {
        fz_matrix text_matrix = fz_scale (scaled_size, scaled_size);
        text_matrix = fz_pre_translate (text_matrix, x_pos / scaled_size, y_pos / scaled_size);

        fz_show_glyph (ctx, text, current_font, text_matrix, glyph_id, char_code, 0, 0, FZ_BIDI_NEUTRAL, FZ_LANG_UNSET);

        // Render the text
        float fz_color[3];
        int alpha;
        fitz_color_from_color (fg, fz_color, &alpha);

        fz_fill_text (ctx, device, text, transform,
                      get_colorspace_for_color (fg), fz_color,
                      ((float)alpha) / 255.0f, fz_default_color_params);
      }
      fz_always (ctx) {
        fz_drop_text (ctx, text);
      }
      fz_catch (ctx) {
        convert_warning << "fitz_renderer: Failed to render native glyph " << char_code << LF;
      }

      end_text ();
      return;
    }
  }

  // Fallback to bitmap rendering if native font failed
  color fgc = pen->get_color ();
  basic_character xc (char_code, fn, std_shrinkf, fgc, 0);

  // Check cache first
  fz_image* cached_image = NULL;
  if (character_image_cache->contains (xc)) {
    cached_image = character_image_cache [xc];
  } else {
    // Create new character image - same as before
    int r, g, b, a;
    get_rgb (fgc, r, g, b, a);
    if (get_reverse_colors ()) reverse (r, g, b);

    SI xo, yo;
    glyph pre_gl = fn->get (char_code);
    if (is_nil (pre_gl)) return;

    glyph gl = shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
    int gw = gl->width, gh = gl->height;

    if (gw == 0 || gh == 0) return; // Empty glyph

    // Create pixmap for glyph
    unsigned char *samples = (unsigned char *)
      fz_malloc (ctx, gh * gw * 4);

    int nr_cols = std_shrinkf * std_shrinkf;
    if (nr_cols >= 64) nr_cols = 64;

    unsigned char *d = samples;
    for (int gy = 0; gy < gh; gy++) {
      for (int gx = 0; gx < gw; gx++) {
        int col = gl->get_x (gx, gy);
        int alpha_val = ((a * col) / nr_cols) & 0xFF;
        d[0] = (r * alpha_val) / 255;
        d[1] = (g * alpha_val) / 255;
        d[2] = (b * alpha_val) / 255;
        d[3] = alpha_val;
        d += 4;
      }
    }

    fz_pixmap* pix = fz_new_pixmap_with_data (ctx, colorspace_rgb,
                                             gw, gh, NULL, 1, gw * 4, samples);
    cached_image = fz_new_image_from_pixmap (ctx, pix, NULL);
    fz_drop_pixmap (ctx, pix);

    character_image_cache (xc) = cached_image;
  }

  if (cached_image) {
    fz_matrix image_transform = fz_translate (to_fitz_x (x), to_fitz_y (y));
    fz_fill_image (ctx, device, cached_image, image_transform, 1.0f, fz_default_color_params);
  }
}

void
fitz_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  if (!device) return;

  // Handle scalable images - similar to MuPDF renderer approach
  if (im->get_type () != scalable_image ||
      (im->get_type () == scalable_image && im->get_effect () != tree (""))) {
    // Fall back to base renderer for non-image scalables or those with effects
    renderer_rep::draw_scalable (im, x, y, alpha);
    return;
  }

  // Load and cache the image
  url u = im->get_name ();
  tree lookup = tuple (u->t);
  fz_image* fz_im = NULL;

  if (image_cache->contains (lookup)) {
    fz_im = image_cache [lookup];
  } else {
    // Load image using Fitz
    fz_im = fitz_load_image (u);
    if (fz_im) {
      fz_keep_image (ctx, fz_im); // Keep reference for cache
      image_cache (lookup) = fz_im;
    }
  }

  if (!fz_im) {
    convert_warning << "fitz_renderer: Failed to load scalable image " << u << LF;
    return;
  }

  // Get image dimensions and positioning
  rectangle r = im->get_logical_extents ();
  SI w = r->x2 - r->x1, h = r->y2 - r->y1;
  int ox = r->x1, oy = r->y1;

  // Create transformation matrix for positioning and scaling
  fz_matrix image_transform = fz_identity;
  image_transform = fz_pre_scale (image_transform,
                                  ((double)w) / pixel, ((double)h) / pixel);
  image_transform = fz_pre_translate (image_transform,
                                      to_fitz_x (x - ox), to_fitz_y (y - oy));

  // Render the image
  fz_try (ctx) {
    fz_fill_image (ctx, device, fz_im, image_transform,
                   ((float)alpha) / 255.0f, fz_default_color_params);
  }
  fz_catch (ctx) {
    convert_warning << "fitz_renderer: Failed to render scalable image" << LF;
  }
}

void
fitz_renderer_rep::draw_picture (picture pict, SI x, SI y, int alpha) {
  if (!device) return;

  // Convert picture to Fitz format if needed
  picture p = as_fitz_picture (pict);
  if (is_nil (p)) {
    convert_warning << "fitz_renderer: Failed to convert picture" << LF;
    return;
  }

  fitz_picture_rep* pic_rep = (fitz_picture_rep*) p->get_handle ();
  if (!pic_rep || !pic_rep->pix) {
    convert_warning << "fitz_renderer: Invalid picture handle" << LF;
    return;
  }

  // Create image from pixmap if not already cached
  if (!pic_rep->im) {
    fz_try (ctx) {
      pic_rep->im = fz_new_image_from_pixmap (ctx, pic_rep->pix, NULL);
    }
    fz_catch (ctx) {
      convert_error << "fitz_renderer: Failed to create image from pixmap" << LF;
      return;
    }
  }

  // Get picture dimensions and origin
  int w = pic_rep->w;
  int h = pic_rep->h;
  int ox = pic_rep->ox;
  int oy = pic_rep->oy;

  // Create transformation matrix
  fz_matrix image_transform = fz_identity;
  image_transform = fz_pre_scale (image_transform, 1.0f, 1.0f);
  image_transform = fz_pre_translate (image_transform,
                                      to_fitz_x (x - ox * pixel),
                                      to_fitz_y (y - oy * pixel));

  // Render the picture
  fz_try (ctx) {
    fz_fill_image (ctx, device, pic_rep->im, image_transform,
                   ((float)alpha) / 255.0f, fz_default_color_params);
  }
  fz_catch (ctx) {
    convert_warning << "fitz_renderer: Failed to render picture" << LF;
  }
}


/******************************************************************************
* Copying regions
******************************************************************************/


// fitz does not have a function for low lever copy of pixmaps
// we have to do it ourselves

static void
translate_pixmap (fz_pixmap *dest_pix, fz_irect dest_rect,
                  fz_pixmap *src_pix, int dx, int dy) {
  fz_context *ctx= get_fitz_context ();
  fz_irect src_rect = fz_translate_irect (dest_rect,  -dx, -dy);
  src_rect = fz_intersect_irect (src_rect, fz_pixmap_bbox (ctx, src_pix));
  src_rect = fz_intersect_irect (src_rect,
                fz_translate_irect (fz_pixmap_bbox (ctx, dest_pix), -dx, -dy));
  dest_rect = fz_translate_irect (src_rect,  dx, dy);
  if (fz_is_empty_irect(dest_rect))  return;
  // all set up, let's do the work now
  {
    unsigned char *srcp;
    unsigned char *destp;
    unsigned int y, w;
    size_t destspan, srcspan;
    w = (unsigned int)(dest_rect.x1 - dest_rect.x0);
    y = (unsigned int)(dest_rect.y1 - dest_rect.y0);

    srcspan = src_pix->stride;
    srcp = src_pix->samples + srcspan * (src_rect.y0 - src_pix->y) +
           (src_rect.x0 - src_pix->x) * (size_t)src_pix->n;
    destspan = dest_pix->stride;
    destp = dest_pix->samples + destspan * (dest_rect.y0 - dest_pix->y) +
            (dest_rect.x0 - dest_pix->x) * (size_t)dest_pix->n;

    if (src_pix->n == dest_pix->n)
    {
      w *= src_pix->n;
      do
      {
        memcpy (destp, srcp, w);
        srcp += srcspan;
        destp += destspan;
      }
      while (--y);
    }
    else
    {
      cout << "fitz_renderer_rep / translate_pixmap : non compatible pixmaps"
           << LF;
    }
  }
}


void
fitz_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  ASSERT (ren != NULL, "invalid situation");
  if (ren->is_printer ()) return;
  fitz_renderer_rep* src= (fitz_renderer_rep*) ren->get_handle ();
  if (src->pixmap == pixmap && x1 == x && y1 == y) return;
  outer_round (x1, y1, x2, y2);
  SI X1= x1, Y1= y1;
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (X1, Y1);
  decode (x1, y1);
  decode (x2, y2);
  src->decode (x, y);
  x += x1 - X1;
  y += y1 - Y1;
  if (x1<x2 && y2<y1) {
    //  XCopyArea (dpy, src->win, win, gc, x, y, x2-x1, y1-y2, x1, y2);
    translate_pixmap (pixmap, fz_make_irect (x1, y2, x2, y1),
                      src->pixmap, x1-x, y1-y);
  }
}

/******************************************************************************
 * Shadow management methods 
 ******************************************************************************/

void
fitz_renderer_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw != mw || sh != mh) {
      delete_shadow (ren);
      ren= NULL;
    }
  }
  if (ren == NULL)  {
    ren= (renderer) tm_new<fitz_renderer_rep> (mw, mh);
    fz_pixmap *pix= fz_new_pixmap (get_fitz_context (),
                                   fz_device_rgb (get_fitz_context ()), mw, mh,
                                   NULL, 1);
    static_cast<fitz_renderer_rep*>(ren)->begin(pix);
    fz_drop_pixmap (get_fitz_context (), pix);
  }
}

void 
fitz_renderer_rep::delete_shadow (renderer& ren)  {
  if (ren != NULL) {
    static_cast<fitz_renderer_rep*>(ren)->end();
    tm_delete (ren);
    ren= NULL;
  }
}

extern "C" {
// not exported from fitz/pixmap-imp.h
  void fz_copy_pixmap_rect(fz_context *ctx, fz_pixmap *dest, fz_pixmap *src, fz_irect r, const fz_default_colorspaces *default_cs);
}

void 
fitz_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  fitz_renderer_rep* shadow= static_cast<fitz_renderer_rep*>(ren);
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  shadow->ox= ox;
  shadow->oy= oy;
  shadow->master= this;
  shadow->cx1= x1+ ox;
  shadow->cy1= y1+ oy;
  shadow->cx2= x2+ ox;
  shadow->cy2= y2+ oy;
  
  decode (x1, y1);
  decode (x2, y2);
  if (x1<x2 && y2<y1) {
    fz_irect rect= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (get_fitz_context(), shadow->pixmap, pixmap, rect, NULL);
  }
}

void
fitz_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  fitz_renderer_rep* shadow= static_cast<fitz_renderer_rep*>(ren);
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (x1, y1);
  decode (x2, y2);
  if (x1<x2 && y2<y1) {
    fz_irect rect= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (get_fitz_context(), pixmap, shadow->pixmap, rect, NULL);
  }
}

void 
fitz_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2)  {
  if (master == NULL) return;
  if (pixmap == static_cast<fitz_renderer_rep*>(master)->pixmap) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  static_cast<fitz_renderer_rep*>(master)->encode (x1, y1);
  static_cast<fitz_renderer_rep*>(master)->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}


/******************************************************************************
* Global renderer instance
******************************************************************************/

fitz_renderer_rep*
the_fitz_renderer () {
  static fitz_renderer_rep* the_renderer = NULL;
  if (!the_renderer) {
    the_renderer = tm_new<fitz_renderer_rep> ();
  }
  return the_renderer;
}

/******************************************************************************
* Shadow and proxy renderers (basic implementations)
******************************************************************************/

fitz_shadow_renderer_rep::fitz_shadow_renderer_rep (int w, int h)
  : fitz_renderer_rep (w, h), master (NULL) {
}

fitz_shadow_renderer_rep::~fitz_shadow_renderer_rep () {
}

void
fitz_shadow_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // Basic shadow implementation
}

fitz_proxy_renderer_rep::fitz_proxy_renderer_rep (fitz_renderer_rep *_base)
  : fitz_renderer_rep (_base->w, _base->h), base (_base) {
}

fitz_proxy_renderer_rep::~fitz_proxy_renderer_rep () {
}

void
fitz_proxy_renderer_rep::new_shadow (renderer& ren) {
  base->new_shadow (ren);
}

void
fitz_proxy_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  base->get_shadow (ren, x1, y1, x2, y2);
}

/******************************************************************************
* Pattern support implementation
******************************************************************************/

fz_shade*
fitz_renderer_rep::create_pattern_shade (url u, SI w, SI h, tree eff, SI pixel) {
  if (!ctx) return NULL;

  // Load the pattern image
  fz_image* pattern_image = fitz_load_image (u);
  if (!pattern_image) {
    convert_warning << "fitz_renderer: Failed to load pattern image " << u << LF;
    return NULL;
  }

  // Store pattern information for later use in fill operations
  tree pattern_key = tuple ("pattern", as_string (u), as_string (w), as_string (h));

  fz_try (ctx) {
    // Convert image to pixmap and store for tiling
    fz_pixmap* pattern_pixmap = fz_get_pixmap_from_image (ctx, pattern_image, NULL, NULL, NULL, NULL);
    if (pattern_pixmap) {
      pattern_info info;
      info.pixmap = fz_keep_pixmap (ctx, pattern_pixmap);
      info.width = w;
      info.height = h;
      info.source_url = u;

      tile_pattern_cache (pattern_key) = info;
      convert_warning << "fitz_renderer: Pattern stored for manual tiling: " << u << LF;

      fz_drop_pixmap (ctx, pattern_pixmap);
    } else {
      convert_warning << "fitz_renderer: Failed to convert image to pixmap for pattern: " << u << LF;
    }
  }
  fz_always (ctx) {
    if (pattern_image) fz_drop_image (ctx, pattern_image);
  }
  fz_catch (ctx) {
    convert_warning << "fitz_renderer: Failed to store pattern image" << LF;
  }

  // Return NULL - patterns will be handled manually in fill operations
  // This tells the renderer to fall back to manual pattern implementation
  return NULL;
}

void
fitz_renderer_rep::register_pattern (brush br, SI pixel) {
  if (!ctx || is_nil (br) || br->get_type () != brush_pattern) {
    return;
  }

  tree p = br->get_pattern ();
  if (pattern_cache->contains (p)) return; // Already registered

  // Get pattern data using TeXmacs utility
  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);

  // Create a shade for the pattern
  fz_shade* shade = create_pattern_shade (u, w, h, eff, pixel);
  if (shade) {
    pattern_cache (p) = shade;
  } else {
    convert_warning << "fitz_renderer: Failed to register pattern" << LF;
  }
}

void
fitz_renderer_rep::select_fill_pattern (brush br) {
  if (!ctx || is_nil (br) || br->get_type () != brush_pattern) {
    current_fill_pattern = NULL;
    current_fill_pattern_key = tree ();
    return;
  }

  tree p_tree = br->get_pattern ();
  register_pattern (br, pixel);

  // Try shade-based pattern first
  if (pattern_cache->contains (p_tree)) {
    current_fill_pattern = pattern_cache [p_tree];
  } else {
    current_fill_pattern = NULL;
  }

  // Also check for tile-based pattern
  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  tree pattern_key = tuple ("pattern", as_string (u), as_string (w), as_string (h));

  if (tile_pattern_cache->contains (pattern_key)) {
    current_fill_pattern_key = pattern_key;
  } else {
    current_fill_pattern_key = tree ();
  }
}

void
fitz_renderer_rep::select_stroke_pattern (brush br) {
  if (!ctx || is_nil (br) || br->get_type () != brush_pattern) {
    current_stroke_pattern = NULL;
    return;
  }

  tree p_tree = br->get_pattern ();
  register_pattern (br, pixel);

  if (pattern_cache->contains (p_tree)) {
    current_stroke_pattern = pattern_cache [p_tree];
  } else {
    convert_warning << "fitz_renderer: Pattern not found for stroke" << LF;
    current_stroke_pattern = NULL;
  }
}

/******************************************************************************
* Factory functions
******************************************************************************/

static renderer the_fitz_ren = NULL;

void
set_fitz_renderer (renderer ren) {
  if (the_fitz_ren != NULL) {
    delete_renderer (the_fitz_ren);
  }
  the_fitz_ren = ren;
}

renderer
get_fitz_renderer () {
  return the_fitz_ren;
}

renderer
fitz_renderer (int w, int h) {
  fz_context *ctx = get_fitz_context ();
  if (!ctx) {
    convert_error << "fitz_renderer: No Fitz context available" << LF;
    return NULL;
  }

  fz_pixmap *pix = fz_new_pixmap (ctx, fz_device_rgb (ctx), w, h, NULL, 1);
  if (!pix) {
    convert_error << "fitz_renderer: Failed to create pixmap" << LF;
    return NULL;
  }

  // Clear the pixmap to white
  fz_clear_pixmap_with_value (ctx, pix, 255);

  fitz_renderer_rep *ren = tm_new<fitz_renderer_rep> (w, h);
  ren->begin (pix);
  fz_drop_pixmap (ctx, pix);

  return ren;
}

renderer
fitz_renderer (picture& p, double zoom) {
  int w = (int) (zoom * p->get_width ());
  int h = (int) (zoom * p->get_height ());

  renderer ren = fitz_renderer (w, h);
  if (ren) {
    ren->set_zoom_factor (zoom);
    ren->set_origin (p->get_origin_x (), p->get_origin_y ());
  }

  return ren;
}

#ifdef FITZ_RENDERER
renderer
picture_renderer (picture p, double zoomf) {
  return fitz_picture_renderer (p, zoomf);
}
#endif

/******************************************************************************
* Native picture support (if FITZ_RENDERER is defined)
******************************************************************************/

#ifdef FITZ_RENDERER
picture
as_native_picture (picture pict) {
  return as_fitz_picture (pict);
}

picture
native_picture (int w, int h, int ox, int oy) {
  fz_context *ctx = get_fitz_context ();
  if (!ctx) return picture ();

  fz_pixmap *pix = fz_new_pixmap (ctx, fz_device_rgb (ctx), w, h, NULL, 1);
  if (!pix) return picture ();

  fz_clear_pixmap (ctx, pix);
  picture p = fitz_picture (pix, ox, oy);
  fz_drop_pixmap (ctx, pix);
  return p;
}

picture
load_picture (url u, int w, int h, tree eff, int pixel) {
  fz_pixmap* pix = fitz_load_pixmap (u, w, h, eff, pixel);
  if (pix == NULL) {
    // Return error picture - need to implement or use existing
    convert_warning << "fitz_renderer: Failed to load picture " << u << LF;
    return native_picture (w > 0 ? w : 100, h > 0 ? h : 100, 0, 0);
  }
  picture p = fitz_picture (pix, 0, 0);
  fz_drop_pixmap (get_fitz_context (), pix);
  return p;
}

void
save_picture (url dest, picture p) {
  if (suffix(dest) != "png") {
    convert_warning << "fitz_renderer: Cannot save " << concretize (dest)
                    << ", only PNG format supported" << LF;
    return;
  }

  picture q = as_fitz_picture (p);
  fitz_picture_rep* pict = (fitz_picture_rep*) q->get_handle ();
  if (!pict || !pict->pix) {
    convert_error << "fitz_renderer: Invalid picture for saving" << LF;
    return;
  }

  fz_context *ctx = get_fitz_context ();
  if (!ctx) return;

  if (exists (dest)) remove (dest);
  c_string path = concretize (dest);

  fz_try (ctx) {
    fz_output *out = fz_new_output_with_path (ctx, path, 0);
    fz_write_pixmap_as_png (ctx, out, pict->pix);
    fz_close_output (ctx, out);
    fz_drop_output (ctx, out);
  }
  fz_catch (ctx) {
    convert_error << "fitz_renderer: Failed to save picture to " << dest << LF;
  }
}
#endif