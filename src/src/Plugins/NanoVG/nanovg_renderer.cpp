/******************************************************************************
* MODULE     : nanovg_renderer.cpp
* DESCRIPTION: NanoVG drawing interface class implementation
* COPYRIGHT  : (C) 2024
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "nanovg_renderer.hpp"
#include "analyze.hpp"
#include "image_files.hpp"
#include "file.hpp"
#include "iterator.hpp"
#include "font.hpp"
#include "Freetype/tt_file.hpp"
#include "Freetype/free_type.hpp"

#include <nanovg.h>
#include <nanovg_gl.h>
#include <cmath>

#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

/******************************************************************************
* NanoVG image implementation
******************************************************************************/

nanovg_image_rep::~nanovg_image_rep() {
  // Note: NanoVG context handles cleanup
}

/******************************************************************************
* Global support variables
******************************************************************************/

static hashmap<basic_character, nanovg_image> character_image;
static hashmap<string, nanovg_image> loaded_images;
static nanovg_renderer_rep* the_renderer = NULL;

/******************************************************************************
* Coordinate conversion helpers
******************************************************************************/

float
nanovg_renderer_rep::to_nvg_x(SI x) {
  return coord_offset_x + coord_scale_x * ((x + ox) / (float)pixel);
}

float
nanovg_renderer_rep::to_nvg_y(SI y) {
  return coord_offset_y + coord_scale_y * ((y + oy) / (float)pixel);
}

SI
nanovg_renderer_rep::from_nvg_x(float x) {
  return (SI)((x - coord_offset_x) / coord_scale_x * pixel) - ox;
}

SI
nanovg_renderer_rep::from_nvg_y(float y) {
  return (SI)((y - coord_offset_y) / coord_scale_y * pixel) - oy;
}

/******************************************************************************
* Color conversion
******************************************************************************/

void
nanovg_renderer_rep::convert_color(color c, NVGcolor& nvg_color) {
  int r, g, b, a;
  get_rgb(c, r, g, b, a);
  nvg_color = nvgRGBA(r, g, b, a);
}

/******************************************************************************
* Constructor and destructor
******************************************************************************/

nanovg_renderer_rep::nanovg_renderer_rep(NVGcontext* vg_context, int w2, int h2, bool own_context) :
  basic_renderer_rep(true, w2, h2),
  vg(vg_context),
  context_owned(own_context),
  framebuffer_width(w2),
  framebuffer_height(h2),
  device_pixel_ratio(1.0f),
  state_dirty(true),
  current_line_width(1.0f),
  current_line_cap(0),
  current_line_join(0),
  current_font_id(-1),
  current_font_name(""),
  current_font_size(12.0f),
  coord_scale_x(1.0f),
  coord_scale_y(-1.0f),  // Flip Y coordinate
  coord_offset_x(0.0f),
  coord_offset_y(h2)
{
  if (w2 > 0 && h2 > 0) {
    set_frame_size(w2, h2);
  }
}

nanovg_renderer_rep::nanovg_renderer_rep(int flags, int w2, int h2) :
  basic_renderer_rep(true, w2, h2),
  context_owned(true),
  framebuffer_width(w2),
  framebuffer_height(h2),
  device_pixel_ratio(1.0f),
  state_dirty(true),
  current_line_width(1.0f),
  current_line_cap(0),
  current_line_join(0),
  current_font_id(-1),
  current_font_name(""),
  current_font_size(12.0f),
  coord_scale_x(1.0f),
  coord_scale_y(-1.0f),
  coord_offset_x(0.0f),
  coord_offset_y(h2)
{
  // Create NanoVG context with OpenGL3 backend
  vg = nvgCreateGL3(flags);
  if (!vg) {
    // Fall back to GL2 if GL3 fails
    vg = nvgCreateGL2(flags);
  }

  if (w2 > 0 && h2 > 0) {
    set_frame_size(w2, h2);
  }
}

nanovg_renderer_rep::~nanovg_renderer_rep() {
  if (context_owned && vg) {
    nvgDeleteGL3(vg);  // This handles both GL2 and GL3 cleanup
  }
}

/******************************************************************************
* Core renderer interface
******************************************************************************/

void*
nanovg_renderer_rep::get_handle() {
  return (void*)vg;
}

bool
nanovg_renderer_rep::is_started() {
  return vg != NULL;
}

void
nanovg_renderer_rep::set_frame_size(int w2, int h2, float pixel_ratio) {
  w = framebuffer_width = w2;
  h = framebuffer_height = h2;
  device_pixel_ratio = pixel_ratio;
  coord_offset_y = h2;
}

void
nanovg_renderer_rep::begin_frame() {
  if (vg) {
    nvgBeginFrame(vg, framebuffer_width, framebuffer_height, device_pixel_ratio);
    sync_graphics_state();
  }
}

void
nanovg_renderer_rep::end_frame() {
  if (vg) {
    nvgEndFrame(vg);
  }
}

/******************************************************************************
* Coordinate system and transformations
******************************************************************************/

void
nanovg_renderer_rep::set_zoom_factor(double zoom) {
  basic_renderer_rep::set_zoom_factor(zoom);
  coord_scale_x = (float)zoom;
  coord_scale_y = -(float)zoom;  // Keep Y-axis flipped
}

void
nanovg_renderer_rep::set_transformation(frame fr) {
  basic_renderer_rep::set_transformation(fr);
  if (vg) {
    nvgSave(vg);
    // Apply transformation matrix
    point p1 = fr(point(0.0, 0.0));
    point p2 = fr(point(1.0, 0.0));
    point p3 = fr(point(0.0, 1.0));

    float a = to_nvg_x((SI)(p2.x * PIXEL)) - to_nvg_x((SI)(p1.x * PIXEL));
    float b = to_nvg_y((SI)(p2.y * PIXEL)) - to_nvg_y((SI)(p1.y * PIXEL));
    float c = to_nvg_x((SI)(p3.x * PIXEL)) - to_nvg_x((SI)(p1.x * PIXEL));
    float d = to_nvg_y((SI)(p3.y * PIXEL)) - to_nvg_y((SI)(p1.y * PIXEL));
    float e = to_nvg_x((SI)(p1.x * PIXEL));
    float f = to_nvg_y((SI)(p1.y * PIXEL));

    nvgTransform(vg, a, b, c, d, e, f);
  }
}

void
nanovg_renderer_rep::reset_transformation() {
  basic_renderer_rep::reset_transformation();
  if (vg) {
    nvgRestore(vg);
  }
}

/******************************************************************************
* Clipping
******************************************************************************/

void
nanovg_renderer_rep::set_clipping(SI x1, SI y1, SI x2, SI y2, bool restore) {
  basic_renderer_rep::set_clipping(x1, y1, x2, y2, restore);

  if (vg) {
    if (restore) {
      nvgRestore(vg);
      nvgSave(vg);
    } else {
      nvgSave(vg);
    }

    float nx1 = to_nvg_x(x1);
    float ny1 = to_nvg_y(y1);
    float nx2 = to_nvg_x(x2);
    float ny2 = to_nvg_y(y2);

    nvgScissor(vg, nx1, ny2, nx2 - nx1, ny1 - ny2);
  }
}

/******************************************************************************
* Graphics state management
******************************************************************************/

pencil
nanovg_renderer_rep::get_pencil() {
  return basic_renderer_rep::get_pencil();
}

brush
nanovg_renderer_rep::get_background() {
  return basic_renderer_rep::get_background();
}

void
nanovg_renderer_rep::set_pencil(pencil p) {
  basic_renderer_rep::set_pencil(p);
  state_dirty = true;
}

void
nanovg_renderer_rep::set_background(brush b) {
  basic_renderer_rep::set_background(b);
  state_dirty = true;
}

void
nanovg_renderer_rep::sync_graphics_state() {
  if (!vg || !state_dirty) return;

  // Set stroke color and properties
  NVGcolor stroke_color;
  convert_color(pen->get_color(), stroke_color);
  nvgStrokeColor(vg, stroke_color);

  // Set fill color
  NVGcolor fill_color;
  convert_color(pen->get_color(), fill_color);
  nvgFillColor(vg, fill_color);

  // Set line width
  float line_width = pen->get_width() / (float)pixel;
  if (line_width != current_line_width) {
    nvgStrokeWidth(vg, line_width);
    current_line_width = line_width;
  }

  // Set line cap and join
  int line_cap = (pen->get_cap() == cap_round) ? NVG_ROUND : NVG_SQUARE;
  if (line_cap != current_line_cap) {
    nvgLineCap(vg, line_cap);
    current_line_cap = line_cap;
  }

  nvgLineJoin(vg, NVG_ROUND);  // Always use round joins
  current_line_join = NVG_ROUND;

  state_dirty = false;
}

/******************************************************************************
* Basic drawing operations
******************************************************************************/

void
nanovg_renderer_rep::clear_device(SI x1, SI y1, SI x2, SI y2) {
  if (!vg) return;

  float nx1 = to_nvg_x(x1);
  float ny1 = to_nvg_y(y1);
  float nx2 = to_nvg_x(x2);
  float ny2 = to_nvg_y(y2);

  NVGcolor bg_color;
  convert_color(bg_brush->get_color(), bg_color);

  nvgBeginPath(vg);
  nvgRect(vg, nx1, ny2, nx2 - nx1, ny1 - ny2);
  nvgFillColor(vg, bg_color);
  nvgFill(vg);
}

void
nanovg_renderer_rep::line(SI x1, SI y1, SI x2, SI y2) {
  if (!vg) return;

  sync_graphics_state();

  float nx1 = to_nvg_x(x1);
  float ny1 = to_nvg_y(y1);
  float nx2 = to_nvg_x(x2);
  float ny2 = to_nvg_y(y2);

  nvgBeginPath(vg);
  nvgMoveTo(vg, nx1, ny1);
  nvgLineTo(vg, nx2, ny2);
  nvgStroke(vg);
}

void
nanovg_renderer_rep::lines(array<SI> x, array<SI> y) {
  if (!vg || N(x) != N(y) || N(x) < 2) return;

  sync_graphics_state();

  nvgBeginPath(vg);
  nvgMoveTo(vg, to_nvg_x(x[0]), to_nvg_y(y[0]));

  for (int i = 1; i < N(x); i++) {
    nvgLineTo(vg, to_nvg_x(x[i]), to_nvg_y(y[i]));
  }

  nvgStroke(vg);
}

void
nanovg_renderer_rep::clear(SI x1, SI y1, SI x2, SI y2) {
  if (!vg) return;

  float nx1 = to_nvg_x(x1);
  float ny1 = to_nvg_y(y1);
  float nx2 = to_nvg_x(x2);
  float ny2 = to_nvg_y(y2);

  NVGcolor bg_color;
  convert_color(bg_brush->get_color(), bg_color);

  nvgBeginPath(vg);
  nvgRect(vg, nx1, ny2, nx2 - nx1, ny1 - ny2);
  nvgFillColor(vg, bg_color);
  nvgFill(vg);
}

void
nanovg_renderer_rep::fill(SI x1, SI y1, SI x2, SI y2) {
  if (!vg) return;

  sync_graphics_state();

  float nx1 = to_nvg_x(x1);
  float ny1 = to_nvg_y(y1);
  float nx2 = to_nvg_x(x2);
  float ny2 = to_nvg_y(y2);

  nvgBeginPath(vg);
  nvgRect(vg, nx1, ny2, nx2 - nx1, ny1 - ny2);
  nvgFill(vg);
}

void
nanovg_renderer_rep::arc(SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (!vg) return;

  sync_graphics_state();

  float cx = to_nvg_x((x1 + x2) / 2);
  float cy = to_nvg_y((y1 + y2) / 2);
  float rx = abs(to_nvg_x(x2) - to_nvg_x(x1)) / 2.0f;
  float ry = abs(to_nvg_y(y2) - to_nvg_y(y1)) / 2.0f;
  float r = (rx + ry) / 2.0f;  // Approximate ellipse with circle

  float start_angle = alpha * M_PI / 180.0f / 64.0f;
  float end_angle = (alpha + delta) * M_PI / 180.0f / 64.0f;

  nvgBeginPath(vg);
  nvgArc(vg, cx, cy, r, start_angle, end_angle, delta > 0 ? NVG_CCW : NVG_CW);
  nvgStroke(vg);
}

void
nanovg_renderer_rep::fill_arc(SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (!vg) return;

  sync_graphics_state();

  float cx = to_nvg_x((x1 + x2) / 2);
  float cy = to_nvg_y((y1 + y2) / 2);
  float rx = abs(to_nvg_x(x2) - to_nvg_x(x1)) / 2.0f;
  float ry = abs(to_nvg_y(y2) - to_nvg_y(y1)) / 2.0f;
  float r = (rx + ry) / 2.0f;  // Approximate ellipse with circle

  float start_angle = alpha * M_PI / 180.0f / 64.0f;
  float end_angle = (alpha + delta) * M_PI / 180.0f / 64.0f;

  nvgBeginPath(vg);
  nvgMoveTo(vg, cx, cy);
  nvgArc(vg, cx, cy, r, start_angle, end_angle, delta > 0 ? NVG_CCW : NVG_CW);
  nvgClosePath(vg);
  nvgFill(vg);
}

void
nanovg_renderer_rep::polygon(array<SI> x, array<SI> y, bool convex) {
  if (!vg || N(x) != N(y) || N(x) < 3) return;

  sync_graphics_state();

  nvgBeginPath(vg);
  nvgMoveTo(vg, to_nvg_x(x[0]), to_nvg_y(y[0]));

  for (int i = 1; i < N(x); i++) {
    nvgLineTo(vg, to_nvg_x(x[i]), to_nvg_y(y[i]));
  }

  nvgClosePath(vg);
  nvgFill(vg);
}

void
nanovg_renderer_rep::draw_triangle(SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
  if (!vg) return;

  sync_graphics_state();

  nvgBeginPath(vg);
  nvgMoveTo(vg, to_nvg_x(x1), to_nvg_y(y1));
  nvgLineTo(vg, to_nvg_x(x2), to_nvg_y(y2));
  nvgLineTo(vg, to_nvg_x(x3), to_nvg_y(y3));
  nvgClosePath(vg);
  nvgFill(vg);
}

/******************************************************************************
* Text rendering (basic implementation)
******************************************************************************/

void
nanovg_renderer_rep::draw(int char_code, font_glyphs fn, SI x, SI y) {
  if (!vg) return;

  // Try native text rendering first
  if (render_glyph_native(char_code, fn, x, y)) {
    return;
  }

  // Fall back to bitmap rendering
  render_glyph_bitmap(char_code, fn, x, y);
}

bool
nanovg_renderer_rep::render_glyph_native(int char_code, font_glyphs fn, SI x, SI y) {
  // For now, return false to use bitmap fallback
  // TODO: Implement native font rendering with NanoVG
  return false;
}

void
nanovg_renderer_rep::render_glyph_bitmap(int char_code, font_glyphs fn, SI x, SI y) {
  basic_character bc(char_code, fn, std_shrinkf, pen->get_color(), bg_brush->get_color());
  nanovg_image img = character_image[bc];

  if (is_nil(img)) {
    cache_glyph(bc);
    img = character_image[bc];
  }

  if (!is_nil(img)) {
    float nx = to_nvg_x(x - img->xo * std_shrinkf);
    float ny = to_nvg_y(y + img->yo * std_shrinkf);

    NVGpaint paint = nvgImagePattern(vg, nx, ny, img->w, img->h, 0.0f, img->handle, 1.0f);
    nvgBeginPath(vg);
    nvgRect(vg, nx, ny, img->w, img->h);
    nvgFillPaint(vg, paint);
    nvgFill(vg);
  }
}

void
nanovg_renderer_rep::cache_glyph(basic_character bc) {
  glyph gl = bc->fng->get(bc->c);
  if (is_nil(gl)) return;

  SI xo, yo;
  glyph sgl = shrink(gl, bc->sf, bc->sf, xo, yo);
  int w, h;
  int img_handle = create_image_from_glyph(sgl, w, h);

  if (img_handle >= 0) {
    nanovg_image img(img_handle, xo, yo, w, h);
    character_image(bc) = img;
  }
}

int
nanovg_renderer_rep::create_image_from_glyph(glyph gl, int& w, int& h) {
  if (!vg || is_nil(gl)) return -1;

  w = gl->width;
  h = gl->height;

  if (w <= 0 || h <= 0) return -1;

  // Create RGBA buffer from glyph
  unsigned char* data = new unsigned char[w * h * 4];
  int nr_cols = std_shrinkf * std_shrinkf;
  if (nr_cols >= 64) nr_cols = 64;

  for (int j = 0; j < h; j++) {
    for (int i = 0; i < w; i++) {
      int col = gl->get_x(i, j);
      int alpha = (255 * col) / (nr_cols + 1);
      int idx = (j * w + i) * 4;
      data[idx] = 255;      // R
      data[idx + 1] = 255;  // G
      data[idx + 2] = 255;  // B
      data[idx + 3] = alpha; // A
    }
  }

  int handle = nvgCreateImageRGBA(vg, w, h, 0, data);
  delete[] data;

  return handle;
}

void
nanovg_renderer_rep::setup_font(font_glyphs fn) {
  // TODO: Load and setup fonts with NanoVG
}

/******************************************************************************
* Image rendering stubs
******************************************************************************/

void
nanovg_renderer_rep::draw_picture(picture pic, SI x, SI y, int alpha) {
  // TODO: Implement picture rendering
}

void
nanovg_renderer_rep::draw_scalable(scalable im, SI x, SI y, int alpha) {
  // TODO: Implement scalable image rendering
}

/******************************************************************************
* Shadow operations
******************************************************************************/

void
nanovg_renderer_rep::fetch(SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  // TODO: Implement fetch operation
}

void
nanovg_renderer_rep::new_shadow(renderer& ren) {
  int w = (cx2 - cx1) / pixel;
  int h = (cy2 - cy1) / pixel;
  if (w <= 0 || h <= 0) return;

  nanovg_shadow_renderer_rep* shadow_ren =
    tm_new<nanovg_shadow_renderer_rep>(w, h, this);
  ren = (renderer)shadow_ren;
}

void
nanovg_renderer_rep::delete_shadow(renderer& ren) {
  if (ren) {
    tm_delete(ren);
    ren = NULL;
  }
}

void
nanovg_renderer_rep::get_shadow(renderer ren, SI x1, SI y1, SI x2, SI y2) {
  if (!ren) return;
  nanovg_shadow_renderer_rep* shadow_ren =
    (nanovg_shadow_renderer_rep*)ren;
  shadow_ren->get_shadow(this, x1, y1, x2, y2);
}

void
nanovg_renderer_rep::put_shadow(renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // TODO: Implement put_shadow
}

void
nanovg_renderer_rep::apply_shadow(SI x1, SI y1, SI x2, SI y2) {
  // TODO: Implement apply_shadow
}

/******************************************************************************
* Shadow renderer implementation
******************************************************************************/

nanovg_shadow_renderer_rep::nanovg_shadow_renderer_rep(int w, int h,
                                                       nanovg_renderer_rep* master_renderer) :
  nanovg_renderer_rep(master_renderer->vg, w, h, false),
  framebuffer_id(0),
  texture_id(0),
  master(master_renderer)
{
  // Create framebuffer for offscreen rendering
  glGenFramebuffers(1, &framebuffer_id);
  glGenTextures(1, &texture_id);

  glBindTexture(GL_TEXTURE_2D, texture_id);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glBindFramebuffer(GL_FRAMEBUFFER, framebuffer_id);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture_id, 0);

  if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
    // Framebuffer creation failed
    glDeleteFramebuffers(1, &framebuffer_id);
    glDeleteTextures(1, &texture_id);
    framebuffer_id = texture_id = 0;
  }

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

nanovg_shadow_renderer_rep::~nanovg_shadow_renderer_rep() {
  if (framebuffer_id) {
    glDeleteFramebuffers(1, &framebuffer_id);
  }
  if (texture_id) {
    glDeleteTextures(1, &texture_id);
  }
}

void
nanovg_shadow_renderer_rep::get_shadow(renderer ren, SI x1, SI y1, SI x2, SI y2) {
  if (!framebuffer_id || !ren) return;

  // Bind our framebuffer and render the content
  glBindFramebuffer(GL_FRAMEBUFFER, framebuffer_id);
  glViewport(0, 0, w, h);
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  begin_frame();

  // TODO: Copy content from source renderer
  // This would involve rendering the specified region

  end_frame();
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
}

/******************************************************************************
* Factory functions and global renderer
******************************************************************************/

nanovg_renderer_rep*
the_nanovg_renderer() {
  if (!the_renderer) {
    the_renderer = tm_new<nanovg_renderer_rep>(NANOVG_DEFAULT_FLAGS);
  }
  return the_renderer;
}

renderer
nanovg_renderer(NVGcontext* vg_context, int w, int h) {
  return tm_new<nanovg_renderer_rep>(vg_context, w, h, false);
}

renderer
nanovg_renderer(int flags, int w, int h) {
  return tm_new<nanovg_renderer_rep>(flags, w, h);
}