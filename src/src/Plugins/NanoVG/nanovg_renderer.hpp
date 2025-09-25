/******************************************************************************
* MODULE     : nanovg_renderer.hpp
* DESCRIPTION: NanoVG drawing interface class
* COPYRIGHT  : (C) 2024
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NANOVG_RENDERER_HPP
#define NANOVG_RENDERER_HPP

#include "basic_renderer.hpp"
#include "hashmap.hpp"

// Forward declarations for NanoVG
struct NVGcontext;
struct NVGcolor;
struct NVGpaint;

/******************************************************************************
* NanoVG image wrapper
******************************************************************************/

struct nanovg_image_rep: concrete_struct {
  int handle;     // NanoVG image handle
  SI xo, yo;      // origin offset
  int w, h;       // dimensions
  nanovg_image_rep(int h, SI xo2, SI yo2, int w2, int h2) :
    handle(h), xo(xo2), yo(yo2), w(w2), h(h2) {}
  ~nanovg_image_rep();
};

class nanovg_image {
  CONCRETE_NULL(nanovg_image);
  nanovg_image(int handle, SI xo, SI yo, int w, int h) :
    rep(tm_new<nanovg_image_rep>(handle, xo, yo, w, h)) {}
};

CONCRETE_NULL_CODE(nanovg_image);

/******************************************************************************
* Main NanoVG renderer class
******************************************************************************/

class nanovg_renderer_rep: public basic_renderer_rep {
private:
  NVGcontext* vg;           // NanoVG context
  bool context_owned;       // Whether we own the context
  int framebuffer_width;    // Current framebuffer dimensions
  int framebuffer_height;
  float device_pixel_ratio; // For high-DPI displays

  // Caching
  hashmap<basic_character, nanovg_image> character_cache;
  hashmap<string, nanovg_image> image_cache;

  // Graphics state
  bool state_dirty;
  float current_line_width;
  int current_line_cap;
  int current_line_join;

  // Text state
  int current_font_id;
  string current_font_name;
  float current_font_size;

  // Coordinate transformation
  float coord_scale_x;
  float coord_scale_y;
  float coord_offset_x;
  float coord_offset_y;

public:
  nanovg_renderer_rep(NVGcontext* vg_context, int w = 0, int h = 0,
                      bool own_context = false);
  nanovg_renderer_rep(int flags, int w = 0, int h = 0);
  virtual ~nanovg_renderer_rep();

  // Core renderer interface
  void* get_handle() override;
  bool is_started() override;

  // Coordinate system
  void set_zoom_factor(double zoom) override;
  void set_transformation(frame fr) override;
  void reset_transformation() override;

  // Clipping
  void set_clipping(SI x1, SI y1, SI x2, SI y2, bool restore = false) override;

  // Graphics state
  pencil get_pencil() override;
  brush get_background() override;
  void set_pencil(pencil p) override;
  void set_background(brush b) override;

  // Basic drawing
  void clear_device(SI x1, SI y1, SI x2, SI y2) override;
  void draw(int char_code, font_glyphs fn, SI x, SI y) override;
  void line(SI x1, SI y1, SI x2, SI y2) override;
  void lines(array<SI> x, array<SI> y) override;
  void clear(SI x1, SI y1, SI x2, SI y2) override;
  void fill(SI x1, SI y1, SI x2, SI y2) override;
  void arc(SI x1, SI y1, SI x2, SI y2, int alpha, int delta) override;
  void fill_arc(SI x1, SI y1, SI x2, SI y2, int alpha, int delta) override;
  void polygon(array<SI> x, array<SI> y, bool convex = true) override;
  void draw_triangle(SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) override;

  // Images
  void draw_picture(picture pic, SI x, SI y, int alpha = 255) override;
  void draw_scalable(scalable im, SI x, SI y, int alpha = 255) override;

  // Shadow operations
  void fetch(SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) override;
  void new_shadow(renderer& ren) override;
  void delete_shadow(renderer& ren) override;
  void get_shadow(renderer ren, SI x1, SI y1, SI x2, SI y2) override;
  void put_shadow(renderer ren, SI x1, SI y1, SI x2, SI y2) override;
  void apply_shadow(SI x1, SI y1, SI x2, SI y2) override;

  // Frame management
  void begin_frame();
  void end_frame();
  void set_frame_size(int w, int h, float pixel_ratio = 1.0f);

private:
  // Helper methods
  void sync_graphics_state();
  void convert_color(color c, NVGcolor& nvg_color);
  float to_nvg_x(SI x);
  float to_nvg_y(SI y);
  SI from_nvg_x(float x);
  SI from_nvg_y(float y);
  void setup_font(font_glyphs fn);
  bool render_glyph_native(int char_code, font_glyphs fn, SI x, SI y);
  void render_glyph_bitmap(int char_code, font_glyphs fn, SI x, SI y);
  void cache_glyph(basic_character bc);
  int load_font(const string& font_name, const string& font_path);
  int create_image_from_glyph(glyph gl, int& w, int& h);
};

/******************************************************************************
* Shadow renderer for offscreen rendering
******************************************************************************/

class nanovg_shadow_renderer_rep: public nanovg_renderer_rep {
private:
  unsigned int framebuffer_id;
  unsigned int texture_id;
  nanovg_renderer_rep* master;

public:
  nanovg_shadow_renderer_rep(int w, int h, nanovg_renderer_rep* master_renderer);
  ~nanovg_shadow_renderer_rep();

  void get_shadow(renderer ren, SI x1, SI y1, SI x2, SI y2) override;
};

/******************************************************************************
* Factory functions
******************************************************************************/

nanovg_renderer_rep* the_nanovg_renderer();
renderer nanovg_renderer(NVGcontext* vg_context, int w = 0, int h = 0);
renderer nanovg_renderer(int flags, int w = 0, int h = 0);

// Default flags for NanoVG context creation
#define NANOVG_DEFAULT_FLAGS (1 | 2)  // NVG_ANTIALIAS | NVG_STENCIL_STROKES

#endif // NANOVG_RENDERER_HPP