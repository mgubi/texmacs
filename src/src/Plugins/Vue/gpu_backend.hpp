/******************************************************************************
* MODULE     : gpu_backend.hpp
* DESCRIPTION: What the GPU renderer of the Vue port (vue_gpu.cpp) asks of
*              a graphics API: OpenGL (gpu_backend_gl.cpp, WebGL2 in the
*              browser) or Metal (gpu_backend_metal.mm)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GPU_BACKEND_HPP
#define GPU_BACKEND_HPP
#include "string.hpp"
#include <vector>

struct SDL_Window;

// The renderer (vue_gpu.cpp) keeps everything which does not depend on the
// API: the batches of quads, the atlas of the glyphs and its contents, the
// outlines of Slug, the caches of the textures of the pictures, the hash of
// the frames. A backend has textures, draws batches into a target with a
// scissor, copies between targets, reads pixels back, and draws the vector
// shapes. Its coordinates are those of the renderer: pixels from the top
// left of the target, y down.

// a texture, as the backend makes it (opaque to the renderer)
struct gpu_texture_rep;
typedef gpu_texture_rep* gpu_texture;

enum gpu_format {
  GPU_R8,        // the atlas of the glyphs (a coverage)
  GPU_RGBA8,     // pictures and targets: premultiplied RGBA
  GPU_RGBA32F,   // the curves of Slug
  GPU_R32UI      // the bands of Slug
};

// A vector shape (a line, a polygon, an arc, a rounded rectangle), in
// pixels of the target: 0 move to (one point), 1 line to (one), 2 cubic to
// (three: two controls and the end), 3 close. Filled and/or stroked; the
// joins are round.
struct gpu_shape {
  std::vector<unsigned char> cmds;
  std::vector<float> pts;
  bool fill= false, even_odd= false;
  unsigned char fill_rgba[4]= { 0, 0, 0, 0 };    // not premultiplied
  bool stroke= false;
  unsigned char stroke_rgba[4]= { 0, 0, 0, 0 };
  float width= 1.f;
  int cap= 0;                                    // 0 butt, 1 round, 2 square
  void move_to (float x, float y) { cmds.push_back (0); pts.push_back (x); pts.push_back (y); }
  void line_to (float x, float y) { cmds.push_back (1); pts.push_back (x); pts.push_back (y); }
  void cubic_to (float x1, float y1, float x2, float y2, float x3, float y3) {
    cmds.push_back (2);
    pts.push_back (x1); pts.push_back (y1); pts.push_back (x2); pts.push_back (y2);
    pts.push_back (x3); pts.push_back (y3); }
  void close () { cmds.push_back (3); }
};

// where a draw goes: a target texture, or the window being drawn (NULL)
struct gpu_pass {
  gpu_texture target;
  int w, h;                // the pixels of the layout: those of the vertices
  int dw, dh;              // the pixels of the target (a scaled window: not w, h)
  int sx1, sy1, sx2, sy2;  // the scissor (pixels of the layout), within w x h
};

// a batch of quads, as triangles of vertices x y u v r g b a (a color
// premultiplied): mode 1 a texture times the alpha, 2 the atlas (a
// coverage) times the color, 3 the atlas times a repeated pattern (tex)
// placed at porig with tiles of ptile pixels
struct gpu_quads {
  int mode;
  gpu_texture tex;
  gpu_texture atlas;
  float porig_x, porig_y, ptile_w, ptile_h;
  const float* verts;
  int nr_verts;
};

class gpu_backend {
public:
  virtual ~gpu_backend () {}
  virtual const char* name () = 0;

  // the windows (SDL): the flags of a window to be drawn into; before the
  // first window, whether the backend can draw at all (false: MuPDF draws);
  // make the window the one which is drawn (true when it can be); show
  // what was drawn into it
  virtual unsigned long long window_flags () = 0;
  // when TeXmacs starts: begin what takes time (Metal: the shaders are
  // compiled in the background), which prepare waits for
  virtual void warm_up () {}
  virtual bool prepare () = 0;
  virtual bool attach (SDL_Window* w) = 0;
  // the same for a layer which the GUI made (a CAMetalLayer of the Cocoa
  // port): false when the backend does not draw into such layers
  virtual bool attach_layer (void* layer) { (void) layer; return false; }
  virtual string description () = 0;     // the device, once attached
  virtual void begin_screen (int dw, int dh) = 0;
  virtual void present (SDL_Window* w) = 0;
  // whether the window (or layer) attached last can be presented without
  // waiting for the screen (false: the compositor still holds the frames
  // presented before, and the window is better drawn a little later)
  virtual bool ready_to_present () { return true; }

  // textures: data has rows from the top, without padding (or is NULL: zeros)
  virtual gpu_texture new_texture (int w, int h, gpu_format f, bool linear,
                                   bool repeat, const void* data) = 0;
  virtual void update_texture (gpu_texture t, int x, int y, int w, int h,
                               const void* data) = 0;
  virtual void delete_texture (gpu_texture t) = 0;
  // a target: an RGBA8 texture which can be drawn into, opaque white or
  // transparent
  virtual gpu_texture new_target (int w, int h, bool transparent= false) = 0;
  // the largest side of a texture or a target: larger ones are not made
  // (new_texture and new_target return NULL)
  virtual int max_texture_side () = 0;
  // the texture of a target has its first row (v = 0) at the bottom (GL)
  virtual bool targets_flipped () = 0;

  // drawing (in the order of the calls)
  virtual void draw_quads (const gpu_pass& p, const gpu_quads& q) = 0;
  // the glyphs of Slug: instances of 13 floats (see vue_gpu.cpp), made
  // bold pixels bolder
  virtual void draw_slug (const gpu_pass& p, gpu_texture curves,
                          gpu_texture bands, const float* inst, int n,
                          float bold) = 0;
  // the shapes, composed over the box x1..x2, y1..y2 (pixels of the
  // layout, within the scissor of the pass) of the target
  virtual void draw_shapes (const gpu_pass& p, const std::vector<gpu_shape>& s,
                            int x1, int y1, int x2, int y2) = 0;
  // a copy of a rectangle, in the pixels of the two targets (NULL: the
  // window, of dw x dh pixels); scaled (linearly) when the sizes differ
  virtual void copy (gpu_texture src, int sw, int sh,
                     int x1, int y1, int x2, int y2,
                     gpu_texture dst, int dw, int dh,
                     int dx1, int dy1, int dx2, int dy2) = 0;
  // the pixels of a rectangle of a target (NULL: the window), premultiplied
  // RGBA, rows from the top
  virtual bool read (gpu_texture src, int sw, int sh,
                     int x, int y, int w, int h, unsigned char* rgba) = 0;
  // the frames of the windows: one begins (gpu_begin_screen), and is
  // presented or not (the same as the last one, vue_gpu_skip); a backend
  // which pays for each call (WebGPU over the stream of WASIX) drops a
  // frame which is not shown
  virtual void begin_frame () {}
  virtual void skip_frame () {}
  virtual void flush () = 0;    // what was drawn goes to the GPU
  virtual void finish () = 0;   // and is done
};

gpu_backend* gpu_gl_backend ();     // NULL when not compiled (USE_THORVG)
gpu_backend* gpu_metal_backend ();  // NULL when not compiled (USE_METAL)
gpu_backend* gpu_webgpu_backend (); // WASIX only (gpu_backend_wg.cpp)

#endif // defined GPU_BACKEND_HPP
