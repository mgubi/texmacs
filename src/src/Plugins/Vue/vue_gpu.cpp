/******************************************************************************
* MODULE     : vue_gpu.cpp
* DESCRIPTION: The GPU renderer of the Vue port (OpenGL, WebGL2 in the
*              browser): a glyph atlas, textures and ThorVG
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// What the GPU draws, and how (see docs/vue-graphics-stack.md, "The GPU
// renderer", and misc/thorvg-bench for the measurements behind it):
//
// * the windows are drawn into their default framebuffers, all with one GL
//   context (SDL_GL_MakeCurrent for the window being drawn), so that the
//   textures, and the framebuffers of the editors, belong to all of them;
// * the backing store of an editor is a texture with a framebuffer
//   (gpu_picture_rep), which the editor repaints incrementally as it did
//   its MuPDF pixmap, and which the window draws as a textured quad; a
//   scroll copies it to a second texture, shifted, and the two are swapped;
// * glyphs come from an atlas (one R8 texture) of the glyph bitmaps of
//   TeXmacs (shrink, as the X11 and the Qt ports draw them), plain fills
//   from a white corner of the same atlas, so that text and fills go in
//   one draw call; the pictures (icons, images) are uploaded once, as
//   textures, and drawn as quads;
// * lines, polygons, arcs and rounded rectangles go to ThorVG's GL engine,
//   which draws into a scratch texture of its own (it draws into a
//   multisampled framebuffer and copies the result over all of its
//   target, so it cannot draw over what is there); the part it drew is
//   then composed over the target. Consecutive vector operations are one
//   ThorVG pass.
//
// Device coordinates are pixels from the top left of the target, y down.
// A target is drawn with y = 0 at the top of its framebuffer, as ThorVG
// draws: the texture of a target has its first row (v = 0) at the bottom of
// the picture, the texture of an uploaded image at the top.

#include "vue_gpu.hpp"
#include "sys_utils.hpp"

#ifndef USE_THORVG

bool vue_gpu_enabled () { return false; }
void vue_gpu_prepare () {}
bool vue_gpu_attach (SDL_Window* w) { (void) w; return false; }
void vue_gpu_present (SDL_Window* w) { (void) w; }
picture  gpu_backing_picture (int w, int h) { (void) w; (void) h; return picture (); }
bool     is_gpu_picture (picture p) { (void) p; return false; }
renderer gpu_picture_renderer (picture p, double z) { (void) p; (void) z; return NULL; }
void     gpu_translate_picture (picture p, int x, int y) { (void) p; (void) x; (void) y; }
picture  gpu_copy_picture (picture p) { return p; }
renderer gpu_screen_renderer (double z) { (void) z; return NULL; }
void     gpu_begin_screen (renderer r, int w, int h) { (void) r; (void) w; (void) h; }
void     gpu_flush () {}
unsigned long long gpu_frame_hash () { return 0; }
void     gpu_finish () {}
picture  gpu_read_screen (int w, int h) { (void) w; (void) h; return picture (); }
bool gpu_draw_picture_scaled (renderer r, picture p, SI x, SI y, double s, int a) {
  (void) r; (void) p; (void) x; (void) y; (void) s; (void) a; return false; }
bool is_gpu_renderer (renderer r) { (void) r; return false; }

#else

#include "basic_renderer.hpp"
#include "frame.hpp"
#include "brush.hpp"
#include "image_files.hpp"
#include "file.hpp"
#include "../MuPDF/mupdf_picture.hpp"
#include <SDL3/SDL.h>
#include <thorvg.h>
#ifdef __EMSCRIPTEN__
#include <GLES3/gl3.h>
#else
#include <OpenGL/gl3.h>
#include <dlfcn.h>
// ThorVG's static library defines its OpenGL entry points as global
// function pointers named as the functions themselves (glCreateProgram...):
// a call to the function would be linked to its pointer. The functions
// used here are loaded from the OpenGL framework into pointers of their
// own, which their names are made to mean.
#define VUE_GL_FUNCTIONS(X) \
  X(glActiveTexture) \
  X(glAttachShader) \
  X(glBindBuffer) \
  X(glBindFramebuffer) \
  X(glBindTexture) \
  X(glBindVertexArray) \
  X(glBlendEquation) \
  X(glBlendFunc) \
  X(glBlitFramebuffer) \
  X(glBufferData) \
  X(glCheckFramebufferStatus) \
  X(glClear) \
  X(glClearColor) \
  X(glColorMask) \
  X(glCompileShader) \
  X(glCreateProgram) \
  X(glCreateShader) \
  X(glDeleteFramebuffers) \
  X(glDeleteTextures) \
  X(glDisable) \
  X(glDrawArrays) \
  X(glDrawArraysInstanced) \
  X(glEnable) \
  X(glEnableVertexAttribArray) \
  X(glFinish) \
  X(glFramebufferTexture2D) \
  X(glGenBuffers) \
  X(glGenFramebuffers) \
  X(glGenTextures) \
  X(glGenVertexArrays) \
  X(glGetProgramiv) \
  X(glGetShaderInfoLog) \
  X(glGetShaderiv) \
  X(glGetString) \
  X(glGetUniformLocation) \
  X(glLinkProgram) \
  X(glPixelStorei) \
  X(glReadPixels) \
  X(glScissor) \
  X(glShaderSource) \
  X(glTexImage2D) \
  X(glTexParameteri) \
  X(glTexSubImage2D) \
  X(glUniform1i) \
  X(glUniform2f) \
  X(glUseProgram) \
  X(glVertexAttribDivisor) \
  X(glVertexAttribIPointer) \
  X(glVertexAttribPointer) \
  X(glViewport)
#define VUE_GL_POINTER(f) static decltype (&::f) vue_##f= NULL;
VUE_GL_FUNCTIONS (VUE_GL_POINTER)
static bool
vue_gl_load () {
  void* lib= dlopen ("/System/Library/Frameworks/OpenGL.framework/OpenGL", RTLD_LAZY);
  if (lib == NULL) return false;
  bool ok= true;
#define VUE_GL_LOAD(f) \
  vue_##f= (decltype (vue_##f)) dlsym (lib, #f); if (vue_##f == NULL) ok= false;
  VUE_GL_FUNCTIONS (VUE_GL_LOAD)
  return ok;
}
#define glActiveTexture vue_glActiveTexture
#define glAttachShader vue_glAttachShader
#define glBindBuffer vue_glBindBuffer
#define glBindFramebuffer vue_glBindFramebuffer
#define glBindTexture vue_glBindTexture
#define glBindVertexArray vue_glBindVertexArray
#define glBlendEquation vue_glBlendEquation
#define glBlendFunc vue_glBlendFunc
#define glBlitFramebuffer vue_glBlitFramebuffer
#define glBufferData vue_glBufferData
#define glCheckFramebufferStatus vue_glCheckFramebufferStatus
#define glClear vue_glClear
#define glClearColor vue_glClearColor
#define glColorMask vue_glColorMask
#define glCompileShader vue_glCompileShader
#define glCreateProgram vue_glCreateProgram
#define glCreateShader vue_glCreateShader
#define glDeleteFramebuffers vue_glDeleteFramebuffers
#define glDeleteTextures vue_glDeleteTextures
#define glDisable vue_glDisable
#define glDrawArrays vue_glDrawArrays
#define glDrawArraysInstanced vue_glDrawArraysInstanced
#define glEnable vue_glEnable
#define glEnableVertexAttribArray vue_glEnableVertexAttribArray
#define glFinish vue_glFinish
#define glFramebufferTexture2D vue_glFramebufferTexture2D
#define glGenBuffers vue_glGenBuffers
#define glGenFramebuffers vue_glGenFramebuffers
#define glGenTextures vue_glGenTextures
#define glGenVertexArrays vue_glGenVertexArrays
#define glGetProgramiv vue_glGetProgramiv
#define glGetShaderInfoLog vue_glGetShaderInfoLog
#define glGetShaderiv vue_glGetShaderiv
#define glGetString vue_glGetString
#define glGetUniformLocation vue_glGetUniformLocation
#define glLinkProgram vue_glLinkProgram
#define glPixelStorei vue_glPixelStorei
#define glReadPixels vue_glReadPixels
#define glScissor vue_glScissor
#define glShaderSource vue_glShaderSource
#define glTexImage2D vue_glTexImage2D
#define glTexParameteri vue_glTexParameteri
#define glTexSubImage2D vue_glTexSubImage2D
#define glUniform1i vue_glUniform1i
#define glUniform2f vue_glUniform2f
#define glUseProgram vue_glUseProgram
#define glVertexAttribDivisor vue_glVertexAttribDivisor
#define glVertexAttribIPointer vue_glVertexAttribIPointer
#define glVertexAttribPointer vue_glVertexAttribPointer
#define glViewport vue_glViewport
#endif
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <utility>
#include <cmath>

/******************************************************************************
* Targets: a texture with a framebuffer, or the default framebuffer
******************************************************************************/

struct gpu_target {
  int w, h;
  GLuint fbo, tex;      // fbo 0 (and no texture): the window
  GLuint fbo2, tex2;    // the other one of a scrolled backing store
  bool screen;
  bool made;            // the GL objects exist (made when first drawn)
  unsigned long long gen; // changes with what the texture shows
};

static void gpu_flush_all ();

static bool
make_texture_target (GLuint& fbo, GLuint& tex, int w, int h) {
  glGenTextures (1, &tex);
  glBindTexture (GL_TEXTURE_2D, tex);
  glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA8, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glGenFramebuffers (1, &fbo);
  glBindFramebuffer (GL_FRAMEBUFFER, fbo);
  glFramebufferTexture2D (GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, tex, 0);
  bool ok= glCheckFramebufferStatus (GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE;
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  return ok;
}

static void
free_texture_target (GLuint& fbo, GLuint& tex) {
  if (fbo != 0) glDeleteFramebuffers (1, &fbo);
  if (tex != 0) glDeleteTextures (1, &tex);
  fbo= tex= 0;
}

/******************************************************************************
* The state of the GPU (one context)
******************************************************************************/

struct glyph_key {
  void* fng; int c;
  bool operator == (const glyph_key& k) const { return fng == k.fng && c == k.c; }
};
struct glyph_key_hash {
  size_t operator () (const glyph_key& k) const {
    return std::hash<void*> () (k.fng) ^ (std::hash<int> () (k.c) * 31); }
};
struct glyph_slot {
  int x, y, w, h;       // in the atlas
  SI xo, yo;            // the offsets of shrink
  bool mupdf;           // rendered by MuPDF: placed by dx, dy instead
  int dx, dy;           // its top left corner from the origin (pixels, y down)
};

struct slug_glyph {
  float bx0, by0, bx1, by1;  // the box of the outline, in em units (y up)
  unsigned offset;           // of its header in the texture of bands
  unsigned nb;               // bands in each direction (0: no ink)
  float em;                  // pixels a em
  bool outline;              // false: the font has no file, a bitmap then
};

struct cached_texture {
  GLuint tex;
  int w, h;
  picture keep;         // the picture stays (its unique id is the key)
  unsigned long long last;
  size_t bytes;
};

static const int ATLAS= 2048;

struct gpu_state {
  SDL_GLContext ctx= NULL;
  bool failed= false;
  GLuint prog= 0, vao= 0, vbo= 0;
  GLint u_size= -1, u_mode= -1, u_tex= -1, u_atlas= -1, u_porig= -1, u_ptile= -1;
  // the batch being made: quads of one target, one texture, one clip
  std::vector<float> verts;     // x y u v r g b a (premultiplied)
  gpu_target* target= NULL;
  int mode= 2;                  // 1: texture, 2: atlas, 3: atlas * pattern
  float porig_x= 0, porig_y= 0, ptile_w= 1, ptile_h= 1; // the pattern (3)
  GLuint tex= 0;
  int sx1= 0, sy1= 0, sx2= 0, sy2= 0; // the clip (device pixels)
  // the atlas of the glyphs, with a white corner for the fills
  GLuint atlas= 0;
  int ax= 4, ay= 0, arow= 4;
  std::unordered_map<glyph_key, glyph_slot, glyph_key_hash> glyphs;
  std::unordered_map<glyph_key, slug_glyph, glyph_key_hash> slugs;
  std::vector<font_glyphs> fonts;  // kept alive: their address is the key
  // ThorVG: canvases drawing into scratch targets, a pass pending. The
  // pass ends by copying all of the scratch target (GlBlitTask), whatever
  // it drew, so a pass which fits in SMALL is drawn on a small canvas,
  // moved to its origin; the others on one as large as the windows
  tvg::GlCanvas* tvg= NULL;
  gpu_target scratch= { 0, 0, 0, 0, 0, 0, false, false };
  tvg::GlCanvas* tvg_small= NULL;
  gpu_target small= { 0, 0, 0, 0, 0, 0, false, false };
  std::vector<tvg::Paint*> shapes;
  bool pending= false;
  gpu_target* vtarget= NULL;
  int vx1= 0, vy1= 0, vx2= 0, vy2= 0;          // what the pass covers
  int vsx1= 0, vsy1= 0, vsx2= 0, vsy2= 0;      // its clip
  // the textures of the pictures, by their unique id; of the patterns
  std::unordered_map<unsigned long long, cached_texture> textures;
  std::unordered_map<std::string, cached_texture> patterns;
  unsigned long long tick= 0;
  size_t texture_bytes= 0;
  // the default framebuffer of the window being drawn
  gpu_target screen= { 0, 0, 0, 0, 0, 0, true, true, 0 };
  // glyphs drawn from their outlines (Slug, TEXMACS_VUE_SLUG=1)
  bool slug_on= false;
  GLuint slug_prog= 0, slug_vao= 0, slug_vbo= 0, curve_tex= 0, index_tex= 0;
  GLint su_size= -1, su_curves= -1, su_bands= -1;
  int curve_rows= 0, index_rows= 0;      // allocated rows of the textures
  std::vector<float> curves;             // 4 floats a texel, 2 texels a curve
  std::vector<unsigned> bands;           // headers and lists of curves
  size_t curves_sent= 0, bands_sent= 0;  // what the textures hold
  std::vector<float> slug_inst;          // the instances of the batch
  std::vector<font_glyphs> slug_fonts;   // kept alive: their address is a key
  // what is drawn on the screen in this frame, folded into a hash
  // (gpu_frame_hash): a frame which draws what the last one drew is not
  // presented
  unsigned long long frame_hash= 0;
};

static gpu_state G;

// folding what is drawn on the screen into the hash of the frame (FNV-1a)
static inline void
feed_bytes (const void* p, size_t n) {
  const unsigned char* b= (const unsigned char*) p;
  unsigned long long h= G.frame_hash;
  for (size_t i= 0; i < n; i++) { h ^= b[i]; h *= 1099511628211ULL; }
  G.frame_hash= h;
}
template<typename T> static inline void
feed (const T& v) { feed_bytes (&v, sizeof (T)); }

static inline bool
on_screen (gpu_target* t) { return t != NULL && t->screen; }

bool
vue_gpu_enabled () {
  static int on= -1;
  if (on < 0) on= (N (get_env ("TEXMACS_VUE_GPU")) > 0 &&
                   get_env ("TEXMACS_VUE_GPU") != "0") ? 1 : 0;
  return on == 1 && !G.failed;
}

void
vue_gpu_prepare () {
  static bool done= false;
  if (done || !vue_gpu_enabled ()) return;
  done= true;
#ifdef __EMSCRIPTEN__
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 0);
#else
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 4);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 1);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG);
#endif
  SDL_GL_SetAttribute (SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute (SDL_GL_DEPTH_SIZE, 0);
  SDL_GL_SetAttribute (SDL_GL_STENCIL_SIZE, 0);
  SDL_GL_SetAttribute (SDL_GL_SHARE_WITH_CURRENT_CONTEXT, 1);
}

/******************************************************************************
* The program: textured quads, from the atlas (a coverage times a color)
* or from a texture (premultiplied, times an alpha)
******************************************************************************/

#ifdef __EMSCRIPTEN__
#define GLSL_HEADER "#version 300 es\nprecision highp float;\n"
#else
#define GLSL_HEADER "#version 330 core\n"
#endif

static const char* vertex_src= GLSL_HEADER
  "layout(location=0) in vec2 a_pos;\n"
  "layout(location=1) in vec2 a_uv;\n"
  "layout(location=2) in vec4 a_col;\n"
  "uniform vec2 u_size;\n"
  "out vec2 v_uv; out vec4 v_col; out vec2 v_pos;\n"
  "void main () {\n"
  "  v_uv= a_uv; v_col= a_col; v_pos= a_pos;\n"
  "  gl_Position= vec4 (a_pos.x / u_size.x * 2.0 - 1.0,\n"
  "                     1.0 - a_pos.y / u_size.y * 2.0, 0.0, 1.0);\n"
  "}\n";

static const char* fragment_src= GLSL_HEADER
  "uniform int u_mode;\n"
  "uniform sampler2D u_tex;\n"
  "uniform sampler2D u_atlas;\n"
  "uniform vec2 u_porig, u_ptile;\n"
  "in vec2 v_uv; in vec4 v_col; in vec2 v_pos;\n"
  "out vec4 o_col;\n"
  "void main () {\n"
  "  if (u_mode == 1) o_col= texture (u_tex, v_uv) * v_col.a;\n"
  "  else if (u_mode == 3)\n"   // a glyph filled with a pattern
  "    o_col= texture (u_atlas, v_uv).r *\n"
  "           texture (u_tex, (v_pos - u_porig) / u_ptile) * v_col.a;\n"
  "  else o_col= texture (u_atlas, v_uv).r * v_col;\n"
  "}\n";

/******************************************************************************
* Glyphs drawn from their outlines (Slug: E. Lengyel, "GPU-Centered Font
* Rendering Directly from Glyph Outlines", JCGT 6 (2), 2017; measured in
* misc/thorvg-bench). The outline of a glyph is a list of quadratic curves
* in em units (mupdf_glyph_outline), in a texture of curves (two texels a
* curve); its box is cut into bands, horizontal and vertical, each listing
* the curves which cross it, sorted by decreasing maximal x or y, in a
* texture of indices. For each pixel the fragment shader casts a ray
* towards +x through the curves of its horizontal band and one towards +y
* through those of its vertical band; each curve adds or removes coverage
* from its crossings, the roots which count chosen by the signs of its
* control points (the 0x2E74 table of the paper, which keeps the winding
* number right where curves meet); the two estimates are weighted by how
* close their nearest crossings are to the pixel. A glyph is an instance
* of a quad: its origin, its size, its box, its bands, its color.
******************************************************************************/

static const int SLUG_W= 1024;   // the width of the two textures

static const char* slug_vertex_src= GLSL_HEADER
  "layout(location=0) in vec3 i_org;\n"   // origin (pixels), pixels a em
  "layout(location=1) in vec4 i_box;\n"   // the box (em)
  "layout(location=2) in uvec2 i_band;\n" // offset of the header, bands
  "layout(location=3) in vec4 i_col;\n"
  "uniform vec2 u_size;\n"
  "out vec2 v_em; flat out vec4 v_box; flat out uvec2 v_band;\n"
  "flat out vec4 v_col; flat out float v_scale;\n"
  "void main () {\n"
  "  int id= gl_VertexID;\n"
  "  vec2 c= vec2 ((id == 1 || id == 3 || id == 4) ? 1.0 : 0.0,\n"
  "                (id == 2 || id == 4 || id == 5) ? 1.0 : 0.0);\n"
  "  float pad= 1.0 / i_org.z;\n"
  "  vec2 em= mix (i_box.xy - pad, i_box.zw + pad, c);\n"
  "  vec2 p= i_org.xy + vec2 (em.x, -em.y) * i_org.z;\n"
  "  v_em= em; v_box= i_box; v_band= i_band; v_col= i_col; v_scale= i_org.z;\n"
  "  gl_Position= vec4 (p.x / u_size.x * 2.0 - 1.0, 1.0 - p.y / u_size.y * 2.0, 0.0, 1.0);\n"
  "}\n";

static const char* slug_fragment_src= GLSL_HEADER
  "precision highp int; precision highp usampler2D;\n"
  "uniform sampler2D u_curves; uniform usampler2D u_bands;\n"
  "in vec2 v_em; flat in vec4 v_box; flat in uvec2 v_band;\n"
  "flat in vec4 v_col; flat in float v_scale;\n"
  "out vec4 o_col;\n"
  "const int W= 1024;\n"
  "uint idx (uint i) { return texelFetch (u_bands, ivec2 (int (i) % W, int (i) / W), 0).r; }\n"
  "vec4 crv (uint k, int j) { int i= int (k) * 2 + j; return texelFetch (u_curves, ivec2 (i % W, i / W), 0); }\n"
  "float ray (uint start, uint count, bool vertical, out float near) {\n"
  "  float cov= 0.0; near= 1.0e9;\n"
  "  for (uint n= 0u; n < count; n++) {\n"
  "    uint k= idx (start + n);\n"
  "    vec4 a= crv (k, 0); vec4 b= crv (k, 1);\n"
  "    vec2 p1= a.xy - v_em, p2= a.zw - v_em, p3= b.xy - v_em;\n"
  "    if (vertical) { p1= p1.yx; p2= p2.yx; p3= p3.yx; }\n"
  "    if (max (max (p1.x, p2.x), p3.x) * v_scale < -0.5) break;\n"
  "    uint code= (0x2E74u >> ((p1.y > 0.0 ? 2u : 0u) + (p2.y > 0.0 ? 4u : 0u) +\n"
  "                            (p3.y > 0.0 ? 8u : 0u))) & 3u;\n"
  "    if (code == 0u) continue;\n"
  "    vec2 A= p1 - p2 * 2.0 + p3, B= p1 - p2;\n"
  "    float t1, t2;\n"
  "    if (abs (A.y) < 1.0e-5) { t1= t2= p1.y * 0.5 / B.y; }\n"
  "    else {\n"
  "      float ra= 1.0 / A.y, d= sqrt (max (B.y * B.y - A.y * p1.y, 0.0));\n"
  "      t1= (B.y - d) * ra; t2= (B.y + d) * ra;\n"
  "    }\n"
  "    float x1= (A.x * t1 - B.x * 2.0) * t1 + p1.x;\n"
  "    float x2= (A.x * t2 - B.x * 2.0) * t2 + p1.x;\n"
  "    if ((code & 1u) != 0u) {\n"
  "      cov += clamp (x1 * v_scale + 0.5, 0.0, 1.0); near= min (near, abs (x1 * v_scale)); }\n"
  "    if (code > 1u) {\n"
  "      cov -= clamp (x2 * v_scale + 0.5, 0.0, 1.0); near= min (near, abs (x2 * v_scale)); }\n"
  "  }\n"
  "  return cov;\n"
  "}\n"
  "void main () {\n"
  "  uint nb= v_band.y, h= v_band.x;\n"
  "  vec2 rel= (v_em - v_box.xy) / max (v_box.zw - v_box.xy, vec2 (1.0e-6));\n"
  "  uint by= uint (clamp (floor (rel.y * float (nb)), 0.0, float (nb) - 1.0));\n"
  "  uint bx= uint (clamp (floor (rel.x * float (nb)), 0.0, float (nb) - 1.0));\n"
  "  float nx, ny;\n"
  "  float cx= abs (ray (idx (h + 2u * by), idx (h + 2u * by + 1u), false, nx));\n"
  "  float cy= abs (ray (idx (h + 2u * (nb + bx)), idx (h + 2u * (nb + bx) + 1u), true, ny));\n"
  "  float wx= clamp (1.0 - nx * 2.0, 0.0, 1.0), wy= clamp (1.0 - ny * 2.0, 0.0, 1.0);\n"
  "  float c= max ((cx * wx + cy * wy) / max (wx + wy, 1.0 / 65536.0), min (cx, cy));\n"
  "  o_col= v_col * clamp (c, 0.0, 1.0);\n"
  "}\n";

static GLuint
compile_shader (GLenum type, const char* src) {
  GLuint s= glCreateShader (type);
  glShaderSource (s, 1, &src, NULL);
  glCompileShader (s);
  GLint ok= 0;
  glGetShaderiv (s, GL_COMPILE_STATUS, &ok);
  if (!ok) {
    char log[2000]; glGetShaderInfoLog (s, 2000, NULL, log);
    cout << "TeXmacs] GPU: shader: " << log << LF;
  }
  return s;
}

static bool
init_gl () {
  G.prog= glCreateProgram ();
  glAttachShader (G.prog, compile_shader (GL_VERTEX_SHADER, vertex_src));
  glAttachShader (G.prog, compile_shader (GL_FRAGMENT_SHADER, fragment_src));
  glLinkProgram (G.prog);
  GLint ok= 0;
  glGetProgramiv (G.prog, GL_LINK_STATUS, &ok);
  if (!ok) { cout << "TeXmacs] GPU: the program does not link" << LF; return false; }
  G.u_size = glGetUniformLocation (G.prog, "u_size");
  G.u_mode = glGetUniformLocation (G.prog, "u_mode");
  G.u_tex  = glGetUniformLocation (G.prog, "u_tex");
  G.u_atlas= glGetUniformLocation (G.prog, "u_atlas");
  G.u_porig= glGetUniformLocation (G.prog, "u_porig");
  G.u_ptile= glGetUniformLocation (G.prog, "u_ptile");
  glGenVertexArrays (1, &G.vao);
  glGenBuffers (1, &G.vbo);
  // the atlas, with a white 4x4 corner (the fills)
  glGenTextures (1, &G.atlas);
  glBindTexture (GL_TEXTURE_2D, G.atlas);
  std::vector<unsigned char> zero (ATLAS * ATLAS, 0);
  for (int y= 0; y < 4; y++) for (int x= 0; x < 4; x++) zero[y * ATLAS + x]= 255;
  glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D (GL_TEXTURE_2D, 0, GL_R8, ATLAS, ATLAS, 0, GL_RED, GL_UNSIGNED_BYTE, zero.data ());
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  // the program of Slug (TEXMACS_VUE_SLUG=1)
  G.slug_on= (get_env ("TEXMACS_VUE_SLUG") == "1");
  if (G.slug_on) {
    G.slug_prog= glCreateProgram ();
    glAttachShader (G.slug_prog, compile_shader (GL_VERTEX_SHADER, slug_vertex_src));
    glAttachShader (G.slug_prog, compile_shader (GL_FRAGMENT_SHADER, slug_fragment_src));
    glLinkProgram (G.slug_prog);
    glGetProgramiv (G.slug_prog, GL_LINK_STATUS, &ok);
    if (!ok) {
      cout << "TeXmacs] GPU: the program of Slug does not link, bitmap glyphs" << LF;
      G.slug_on= false;
    }
    else {
      G.su_size  = glGetUniformLocation (G.slug_prog, "u_size");
      G.su_curves= glGetUniformLocation (G.slug_prog, "u_curves");
      G.su_bands = glGetUniformLocation (G.slug_prog, "u_bands");
      glGenVertexArrays (1, &G.slug_vao);
      glGenBuffers (1, &G.slug_vbo);
      glGenTextures (1, &G.curve_tex);
      glGenTextures (1, &G.index_tex);
      cout << "TeXmacs] GPU: glyphs drawn from their outlines (Slug)" << LF;
    }
  }
  // ThorVG
  if (tvg::Initializer::init (0) != tvg::Result::Success) {
    cout << "TeXmacs] GPU: ThorVG does not start" << LF; return false; }
  G.tvg= tvg::GlCanvas::gen ();
  G.tvg_small= tvg::GlCanvas::gen ();
  if (G.tvg == NULL || G.tvg_small == NULL) {
    cout << "TeXmacs] GPU: no GL canvas in ThorVG" << LF; return false; }
  return true;
}

bool
vue_gpu_attach (SDL_Window* w) {
  if (!vue_gpu_enabled () || w == NULL) return false;
  if (G.ctx == NULL) {
#ifndef __EMSCRIPTEN__
    if (!vue_gl_load ()) {
      cout << "TeXmacs] GPU: no OpenGL, drawing with MuPDF" << LF;
      G.failed= true;
      return false;
    }
#endif
    G.ctx= SDL_GL_CreateContext (w);
    if (G.ctx == NULL) {
      cout << "TeXmacs] GPU: no GL context (" << SDL_GetError ()
           << "), drawing with MuPDF" << LF;
      G.failed= true;
      return false;
    }
    SDL_GL_MakeCurrent (w, G.ctx);
    SDL_GL_SetSwapInterval (0); // several windows: each would wait a frame
    if (!init_gl ()) { G.failed= true; return false; }
    cout << "TeXmacs] GPU: " << (const char*) glGetString (GL_RENDERER)
           << ", " << (const char*) glGetString (GL_VERSION) << LF;
    return true;
  }
  return SDL_GL_MakeCurrent (w, G.ctx);
}

void
vue_gpu_present (SDL_Window* w) {
  gpu_flush_all ();
  SDL_GL_SwapWindow (w);
}

static bool
ensure_target (gpu_target* t) {
  if (t->made || t->screen) return true;
  if (G.ctx == NULL || t->w <= 0 || t->h <= 0) return false;
  if (!make_texture_target (t->fbo, t->tex, t->w, t->h)) return false;
  // opaque white, as native_opaque_picture
  glBindFramebuffer (GL_FRAMEBUFFER, t->fbo);
  glDisable (GL_SCISSOR_TEST);
  glClearColor (1, 1, 1, 1);
  glClear (GL_COLOR_BUFFER_BIT);
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  t->made= true;
  return true;
}

/******************************************************************************
* Batches of quads
******************************************************************************/

static void
bind_target (gpu_target* t) {
  glBindFramebuffer (GL_FRAMEBUFFER, t->screen ? 0 : t->fbo);
  glViewport (0, 0, t->w, t->h);
}

static void
set_scissor (gpu_target* t, int x1, int y1, int x2, int y2) {
  x1= max (x1, 0); y1= max (y1, 0); x2= min (x2, t->w); y2= min (y2, t->h);
  if (x2 < x1) x2= x1;
  if (y2 < y1) y2= y1;
  glEnable (GL_SCISSOR_TEST);
  glScissor (x1, t->h - y2, x2 - x1, y2 - y1);
}

static void flush_slug ();

static void
flush_quads () {
  if (G.mode == 4) { flush_slug (); return; }
  if (G.verts.empty () || G.target == NULL) { G.verts.clear (); return; }
  gpu_target* t= G.target;
  bind_target (t);
  set_scissor (t, G.sx1, G.sy1, G.sx2, G.sy2);
  glDisable (GL_DEPTH_TEST);
  glDisable (GL_STENCIL_TEST);
  glDisable (GL_CULL_FACE);
  glColorMask (GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glEnable (GL_BLEND);
  glBlendEquation (GL_FUNC_ADD);
  glBlendFunc (GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram (G.prog);
  glUniform2f (G.u_size, (float) t->w, (float) t->h);
  glUniform1i (G.u_mode, G.mode);
  glUniform2f (G.u_porig, G.porig_x, G.porig_y);
  glUniform2f (G.u_ptile, G.ptile_w, G.ptile_h);
  glActiveTexture (GL_TEXTURE0);
  glBindTexture (GL_TEXTURE_2D, G.mode != 2 ? G.tex : 0);
  glUniform1i (G.u_tex, 0);
  glActiveTexture (GL_TEXTURE1);
  glBindTexture (GL_TEXTURE_2D, G.atlas);
  glUniform1i (G.u_atlas, 1);
  glActiveTexture (GL_TEXTURE0);
  glBindVertexArray (G.vao);
  glBindBuffer (GL_ARRAY_BUFFER, G.vbo);
  glBufferData (GL_ARRAY_BUFFER, G.verts.size () * sizeof (float), G.verts.data (), GL_STREAM_DRAW);
  glEnableVertexAttribArray (0);
  glEnableVertexAttribArray (1);
  glEnableVertexAttribArray (2);
  glVertexAttribPointer (0, 2, GL_FLOAT, GL_FALSE, 32, (void*) 0);
  glVertexAttribPointer (1, 2, GL_FLOAT, GL_FALSE, 32, (void*) 8);
  glVertexAttribPointer (2, 4, GL_FLOAT, GL_FALSE, 32, (void*) 16);
  glDrawArrays (GL_TRIANGLES, 0, (GLsizei) (G.verts.size () / 8));
  glBindVertexArray (0);
  G.verts.clear ();
  t->gen++;
}


// the rows of the textures of Slug which are not there yet; a texture
// which is full is made twice as tall and sent again
static void
send_rows (GLuint tex, int& rows, size_t& sent, size_t total, size_t per_texel,
           bool is_float, const void* data) {
  size_t texels= total / per_texel;
  int need= (int) ((texels + SLUG_W - 1) / SLUG_W);
  if (need == 0) return;
  glBindTexture (GL_TEXTURE_2D, tex);
  glPixelStorei (GL_UNPACK_ALIGNMENT, 4);
  GLenum ifmt= is_float ? GL_RGBA32F : GL_R32UI;
  GLenum fmt= is_float ? GL_RGBA : GL_RED_INTEGER;
  GLenum type= is_float ? GL_FLOAT : GL_UNSIGNED_INT;
  if (need > rows) {
    int nr= max (need, max (16, rows * 2));
    std::vector<unsigned char> zero ((size_t) nr * SLUG_W * per_texel * 4, 0);
    memcpy (zero.data (), data, total * 4);
    glTexImage2D (GL_TEXTURE_2D, 0, ifmt, SLUG_W, nr, 0, fmt, type, zero.data ());
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    rows= nr; sent= total;
    return;
  }
  if (sent >= total) return;
  // from the row of the first texel not sent, whole rows
  int r0= (int) ((sent / per_texel) / SLUG_W);
  std::vector<unsigned char> buf ((size_t) (need - r0) * SLUG_W * per_texel * 4, 0);
  size_t from= (size_t) r0 * SLUG_W * per_texel;
  memcpy (buf.data (), (const char*) data + from * 4, (total - from) * 4);
  glTexSubImage2D (GL_TEXTURE_2D, 0, 0, r0, SLUG_W, need - r0, fmt, type, buf.data ());
  sent= total;
}

static void
flush_slug () {
  gpu_target* t= G.target;
  if (G.slug_inst.empty () || t == NULL) { G.slug_inst.clear (); return; }
  send_rows (G.curve_tex, G.curve_rows, G.curves_sent, G.curves.size (), 4, true,
             G.curves.data ());
  send_rows (G.index_tex, G.index_rows, G.bands_sent, G.bands.size (), 1, false,
             G.bands.data ());
  bind_target (t);
  set_scissor (t, G.sx1, G.sy1, G.sx2, G.sy2);
  glDisable (GL_DEPTH_TEST);
  glDisable (GL_STENCIL_TEST);
  glDisable (GL_CULL_FACE);
  glEnable (GL_BLEND);
  glBlendEquation (GL_FUNC_ADD);
  glBlendFunc (GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram (G.slug_prog);
  glUniform2f (G.su_size, (float) t->w, (float) t->h);
  glActiveTexture (GL_TEXTURE0);
  glBindTexture (GL_TEXTURE_2D, G.curve_tex);
  glUniform1i (G.su_curves, 0);
  glActiveTexture (GL_TEXTURE1);
  glBindTexture (GL_TEXTURE_2D, G.index_tex);
  glUniform1i (G.su_bands, 1);
  glActiveTexture (GL_TEXTURE0);
  glBindVertexArray (G.slug_vao);
  glBindBuffer (GL_ARRAY_BUFFER, G.slug_vbo);
  glBufferData (GL_ARRAY_BUFFER, G.slug_inst.size () * 4, G.slug_inst.data (), GL_STREAM_DRAW);
  const int st= 13 * 4;  // x y em, box, offset nb, color
  glEnableVertexAttribArray (0); glVertexAttribPointer (0, 3, GL_FLOAT, GL_FALSE, st, (void*) 0);
  glEnableVertexAttribArray (1); glVertexAttribPointer (1, 4, GL_FLOAT, GL_FALSE, st, (void*) 12);
  glEnableVertexAttribArray (2); glVertexAttribIPointer (2, 2, GL_UNSIGNED_INT, st, (void*) 28);
  glEnableVertexAttribArray (3); glVertexAttribPointer (3, 4, GL_FLOAT, GL_FALSE, st, (void*) 36);
  for (int i= 0; i < 4; i++) glVertexAttribDivisor (i, 1);
  glDrawArraysInstanced (GL_TRIANGLES, 0, 6, (GLsizei) (G.slug_inst.size () / 13));
  for (int i= 0; i < 4; i++) glVertexAttribDivisor (i, 0);
  glBindVertexArray (0);
  G.slug_inst.clear ();
  t->gen++;
}

// the outline of a glyph, made once: NULL when its font has no file
static slug_glyph*
slug_lookup (font_glyphs fng, int c) {
  glyph_key k { (void*) fng.rep, c };
  auto it= G.slugs.find (k);
  if (it != G.slugs.end ()) return it->second.outline ? &it->second : NULL;
  slug_glyph g= { 0, 0, 0, 0, 0, 0, 0, false };
  array<double> q;
  double em= 0;
  G.slug_fonts.push_back (fng);
  if (!mupdf_glyph_outline (fng->res_name, c, q, em) || em <= 0) {
    G.slugs[k]= g;
    return NULL;
  }
  g.outline= true;
  g.em= (float) em;
  int n= N(q) / 6;
  if (n > 0) {
    float bx0= 1e9, by0= 1e9, bx1= -1e9, by1= -1e9;
    for (int i= 0; i < n; i++)
      for (int j= 0; j < 3; j++) {
        bx0= min (bx0, (float) q[6*i+2*j]); bx1= max (bx1, (float) q[6*i+2*j]);
        by0= min (by0, (float) q[6*i+2*j+1]); by1= max (by1, (float) q[6*i+2*j+1]);
      }
    g.bx0= bx0; g.by0= by0; g.bx1= bx1; g.by1= by1;
    unsigned first= (unsigned) (G.curves.size () / 8);
    for (int i= 0; i < n; i++) {
      float v[8]= { (float) q[6*i], (float) q[6*i+1], (float) q[6*i+2],
                    (float) q[6*i+3], (float) q[6*i+4], (float) q[6*i+5], 0, 0 };
      G.curves.insert (G.curves.end (), v, v + 8);
    }
    unsigned nb= (unsigned) max (1, min (8, n / 3));
    g.nb= nb;
    g.offset= (unsigned) G.bands.size ();
    G.bands.resize (G.bands.size () + 4 * nb, 0);
    for (int dir= 0; dir < 2; dir++)
      for (unsigned b= 0; b < nb; b++) {
        float lo, hi;
        if (dir == 0) { float bh= (by1 - by0) / nb; lo= by0 + b*bh; hi= lo + bh; }
        else          { float bw= (bx1 - bx0) / nb; lo= bx0 + b*bw; hi= lo + bw; }
        std::vector<std::pair<float, unsigned> > in;
        for (int i= 0; i < n; i++) {
          const double* c3= &q[6*i];
          int o= (dir == 0) ? 1 : 0;   // the coordinate across the ray
          double a0= std::min (c3[o], std::min (c3[o+2], c3[o+4]));
          double a1= std::max (c3[o], std::max (c3[o+2], c3[o+4]));
          if (a1 < lo || a0 > hi) continue;
          if (a0 == a1) continue;      // along the ray: never crossed
          int r= 1 - o;                // the coordinate along the ray
          double key= std::max (c3[r], std::max (c3[r+2], c3[r+4]));
          in.push_back ({ (float) key, first + (unsigned) i });
        }
        std::sort (in.begin (), in.end (),
                   [] (const std::pair<float, unsigned>& p, const std::pair<float, unsigned>& r) {
                     return p.first > r.first; });
        unsigned h= g.offset + 2 * (dir * nb + b);
        G.bands[h]= (unsigned) G.bands.size ();
        G.bands[h + 1]= (unsigned) in.size ();
        for (auto& p: in) G.bands.push_back (p.second);
      }
  }
  G.slugs[k]= g;
  return &G.slugs[k];
}

static void flush_vectors ();

// start or go on with a batch for the target, mode, texture and clip
static void
batch (gpu_target* t, int mode, GLuint tex, int x1, int y1, int x2, int y2,
       float pox= 0, float poy= 0, float ptw= 1, float pth= 1) {
  if (G.pending) flush_vectors ();
  if (G.target != t || G.mode != mode || (mode != 2 && G.tex != tex) ||
      G.sx1 != x1 || G.sy1 != y1 || G.sx2 != x2 || G.sy2 != y2 ||
      (mode == 3 && (G.porig_x != pox || G.porig_y != poy ||
                     G.ptile_w != ptw || G.ptile_h != pth))) {
    flush_quads ();
    G.target= t; G.mode= mode; G.tex= tex;
    G.sx1= x1; G.sy1= y1; G.sx2= x2; G.sy2= y2;
    G.porig_x= pox; G.porig_y= poy; G.ptile_w= ptw; G.ptile_h= pth;
    if (on_screen (t)) {
      int st[6]= { mode, (int) tex, x1, y1, x2, y2 };
      float pt[4]= { pox, poy, ptw, pth };
      feed (st); feed (pt);
    }
  }
}

// a quad from its four corners (device pixels: top left, top right, bottom
// left, bottom right) and the texture coordinates of its corners
static inline void
quad (const float* px, const float* py, float u0, float v0, float u1, float v1,
      float r, float g, float b, float a) {
  float c[4][4]= { { px[0], py[0], u0, v0 }, { px[1], py[1], u1, v0 },
                   { px[2], py[2], u0, v1 }, { px[3], py[3], u1, v1 } };
  if (on_screen (G.target)) {
    float d[16]= { px[0], py[0], px[1], py[1], px[2], py[2], px[3], py[3],
                   u0, v0, u1, v1, r, g, b, a };
    feed (d);
  }
  int order[6]= { 0, 1, 2, 1, 3, 2 };
  for (int k= 0; k < 6; k++) {
    float* v= c[order[k]];
    G.verts.push_back (v[0]); G.verts.push_back (v[1]);
    G.verts.push_back (v[2]); G.verts.push_back (v[3]);
    G.verts.push_back (r); G.verts.push_back (g);
    G.verts.push_back (b); G.verts.push_back (a);
  }
}

static void
gpu_flush_all () {
  if (G.pending) flush_vectors ();
  flush_quads ();
}

void
gpu_flush () {
  gpu_flush_all ();
}

void
gpu_finish () {
  // the profile counts what the CPU does, the GPU working in parallel as
  // it does when nothing is measured (its work shows as more or fewer
  // frames); TEXMACS_VUE_GPU_SYNC=1 waits for it, so that the times of
  // the phases include what the GPU did (an upper bound: nothing overlaps)
  static int sync= -1;
  if (sync < 0) sync= (get_env ("TEXMACS_VUE_GPU_SYNC") == "1") ? 1 : 0;
  gpu_flush_all ();
  if (G.ctx != NULL && sync) glFinish ();
}

/******************************************************************************
* ThorVG passes
******************************************************************************/

static bool
ensure_scratch (int w, int h) {
  if (G.scratch.made && G.scratch.w >= w && G.scratch.h >= h) return true;
  int nw= max (w, G.scratch.w), nh= max (h, G.scratch.h);
  if (G.scratch.made) free_texture_target (G.scratch.fbo, G.scratch.tex);
  G.scratch.made= false;
  if (!make_texture_target (G.scratch.fbo, G.scratch.tex, nw, nh)) return false;
  G.scratch.w= nw; G.scratch.h= nh; G.scratch.made= true;
  tvg::Result r= G.tvg->target (NULL, NULL, (void*) G.ctx, (int32_t) G.scratch.fbo,
                                (uint32_t) nw, (uint32_t) nh, tvg::ColorSpace::ABGR8888S);
  if (r != tvg::Result::Success) {
    cout << "TeXmacs] GPU: ThorVG cannot draw into its target (" << (int) r << ")" << LF;
    return false;
  }
  return true;
}

static const int SMALL= 512;

static bool
ensure_small () {
  if (G.small.made) return true;
  if (!make_texture_target (G.small.fbo, G.small.tex, SMALL, SMALL)) return false;
  G.small.w= G.small.h= SMALL; G.small.made= true;
  return G.tvg_small->target (NULL, NULL, (void*) G.ctx, (int32_t) G.small.fbo,
                              SMALL, SMALL, tvg::ColorSpace::ABGR8888S)
         == tvg::Result::Success;
}

static void
flush_vectors () {
  if (!G.pending) return;
  G.pending= false;
  gpu_target* t= G.vtarget;
  int x1= max (G.vx1, G.vsx1), y1= max (G.vy1, G.vsy1);
  int x2= min (G.vx2, G.vsx2), y2= min (G.vy2, G.vsy2);
  x1= max (x1, 0); y1= max (y1, 0); x2= min (x2, t->w); y2= min (y2, t->h);
  std::vector<tvg::Paint*> shapes;
  shapes.swap (G.shapes);
  bool small= (x2 - x1 <= SMALL && y2 - y1 <= SMALL);
  if (x1 >= x2 || y1 >= y2 ||
      (small ? !ensure_small () : !ensure_scratch (t->w, t->h))) {
    for (tvg::Paint* p: shapes) tvg::Paint::rel (p);
    return;
  }
  // the canvas draws all of its target (its viewport does not limit the
  // copy which ends the pass); only the box is composed below
  tvg::GlCanvas* c= small ? G.tvg_small : G.tvg;
  gpu_target& sc= small ? G.small : G.scratch;
  int ox= small ? x1 : 0, oy= small ? y1 : 0;
  if (small) {
    tvg::Scene* scene= tvg::Scene::gen ();
    for (tvg::Paint* p: shapes) scene->add (p);
    scene->translate ((float) -ox, (float) -oy);
    c->add (scene);
  }
  else for (tvg::Paint* p: shapes) c->add (p);
  c->draw (true);
  c->sync ();
  c->remove ();
  // compose what ThorVG drew over the target (premultiplied)
  float sw= (float) sc.w, sh= (float) sc.h;
  batch (t, 1, sc.tex, G.vsx1, G.vsy1, G.vsx2, G.vsy2);
  float px[4]= { (float) x1, (float) x2, (float) x1, (float) x2 };
  float py[4]= { (float) y1, (float) y1, (float) y2, (float) y2 };
  quad (px, py, (x1 - ox) / sw, 1.f - (y1 - oy) / sh,
        (x2 - ox) / sw, 1.f - (y2 - oy) / sh, 1, 1, 1, 1);
  flush_quads ();
}

// a ThorVG shape for the target, within the clip, covering a box
static void
add_vector (gpu_target* t, tvg::Shape* p, int sx1, int sy1, int sx2, int sy2,
            float bx1, float by1, float bx2, float by2) {
  flush_quads ();
  if (G.pending && (G.vtarget != t || G.vsx1 != sx1 || G.vsy1 != sy1 ||
                    G.vsx2 != sx2 || G.vsy2 != sy2))
    flush_vectors ();
  int x1= (int) floor (bx1) - 1, y1= (int) floor (by1) - 1;
  int x2= (int) ceil (bx2) + 1, y2= (int) ceil (by2) + 1;
  if (on_screen (t)) {
    // the shape as drawn: its path, its fill and its stroke
    int k[9]= { 9, sx1, sy1, sx2, sy2, x1, y1, x2, y2 };
    feed (k);
    const tvg::PathCommand* cmds= NULL; const tvg::Point* pts= NULL;
    uint32_t nc= 0, np= 0;
    if (p->path (&cmds, &nc, &pts, &np) == tvg::Result::Success) {
      if (nc > 0) feed_bytes (cmds, nc * sizeof (tvg::PathCommand));
      if (np > 0) feed_bytes (pts, np * sizeof (tvg::Point));
    }
    uint8_t c[8]= { 0, 0, 0, 0, 0, 0, 0, 0 };
    p->fill (&c[0], &c[1], &c[2], &c[3]);
    p->strokeFill (&c[4], &c[5], &c[6], &c[7]);
    feed (c);
    feed (p->strokeWidth ());
    feed (p->fillRule ());
    feed (p->strokeCap ());
  }
  if (!G.pending) {
    G.pending= true; G.vtarget= t;
    G.vsx1= sx1; G.vsy1= sy1; G.vsx2= sx2; G.vsy2= sy2;
    G.vx1= x1; G.vy1= y1; G.vx2= x2; G.vy2= y2;
  }
  else {
    G.vx1= min (G.vx1, x1); G.vy1= min (G.vy1, y1);
    G.vx2= max (G.vx2, x2); G.vy2= max (G.vy2, y2);
  }
  G.shapes.push_back (p);
}

/******************************************************************************
* Textures of pictures
******************************************************************************/

static void
evict_textures () {
  const size_t max_bytes= 256 << 20;
  const size_t max_count= 512;
  while (G.texture_bytes > max_bytes || G.textures.size () > max_count) {
    auto oldest= G.textures.end ();
    for (auto it= G.textures.begin (); it != G.textures.end (); ++it)
      if (oldest == G.textures.end () || it->second.last < oldest->second.last)
        oldest= it;
    if (oldest == G.textures.end ()) break;
    glDeleteTextures (1, &oldest->second.tex);
    G.texture_bytes -= oldest->second.bytes;
    G.textures.erase (oldest);
  }
}

// a texture from a MuPDF pixmap (premultiplied RGBA, rows from the top);
// over white when 'over_white' (the neutral pattern)
static GLuint
upload_pixmap (fz_pixmap* pix, bool repeat, bool over_white= false) {
  if (pix == NULL || pix->samples == NULL || pix->w <= 0 || pix->h <= 0) return 0;
  fz_context* ctx= mupdf_context ();
  int w= pix->w, h= pix->h, n= pix->n;
  bool bgr= (pix->colorspace == fz_device_bgr (ctx));
  std::vector<unsigned char> rgba ((size_t) w * h * 4);
  for (int y= 0; y < h; y++) {
    const unsigned char* s= pix->samples + (size_t) y * pix->stride;
    unsigned char* d= rgba.data () + (size_t) y * w * 4;
    for (int x= 0; x < w; x++, s += n, d += 4) {
      int r= s[0], g= n >= 3 ? s[1] : s[0], b= n >= 3 ? s[2] : s[0];
      int a= pix->alpha ? s[n - 1] : 255;
      if (bgr) { int tmp= r; r= b; b= tmp; }
      if (over_white) { r += 255 - a; g += 255 - a; b += 255 - a; a= 255; }
      d[0]= (unsigned char) r; d[1]= (unsigned char) g;
      d[2]= (unsigned char) b; d[3]= (unsigned char) a;
    }
  }
  GLuint tex;
  glGenTextures (1, &tex);
  glBindTexture (GL_TEXTURE_2D, tex);
  glPixelStorei (GL_UNPACK_ALIGNMENT, 4);
  glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA8, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, rgba.data ());
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  GLint wrap= repeat ? GL_REPEAT : GL_CLAMP_TO_EDGE;
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrap);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrap);
  return tex;
}

static cached_texture*
picture_texture (picture p) {
  unsigned long long id= p->get_unique_id ();
  auto it= G.textures.find (id);
  if (it != G.textures.end ()) { it->second.last= ++G.tick; return &it->second; }
  picture mp= as_mupdf_picture (p);
  mupdf_picture_rep* r= (mupdf_picture_rep*) mp->get_handle ();
  if (r == NULL || r->pix == NULL) return NULL;
  GLuint tex= upload_pixmap (r->pix, false);
  if (tex == 0) return NULL;
  size_t bytes= (size_t) r->pix->w * r->pix->h * 4;
  G.textures[id]= { tex, r->pix->w, r->pix->h, p, ++G.tick, bytes };
  G.texture_bytes += bytes;
  evict_textures ();
  it= G.textures.find (id);
  return it == G.textures.end () ? NULL : &it->second;
}

/******************************************************************************
* Pictures on the GPU: the backing stores of the editors
******************************************************************************/

class gpu_picture_rep: public picture_rep {
public:
  gpu_target t;
  int ox, oy;
  gpu_picture_rep (int w, int h):
    t { w, h, 0, 0, 0, 0, false, false }, ox (0), oy (0) {}
  ~gpu_picture_rep () {
    if (G.target == &t || G.vtarget == &t) gpu_flush_all ();
    if (G.target == &t) G.target= NULL;
    if (t.made) {
      free_texture_target (t.fbo, t.tex);
      free_texture_target (t.fbo2, t.tex2);
    }
  }
  picture_kind get_type () { return picture_native; }
  void* get_handle () { return (void*) this; }
  int get_width () { return t.w; }
  int get_height () { return t.h; }
  int get_origin_x () { return ox; }
  int get_origin_y () { return oy; }
  void set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }
protected:
  color internal_get_pixel (int x, int y) {
    if (!ensure_target (&t) || x < 0 || y < 0 || x >= t.w || y >= t.h) return 0;
    gpu_flush_all ();
    unsigned char px[4]= { 255, 255, 255, 255 };
    glBindFramebuffer (GL_FRAMEBUFFER, t.fbo);
    glReadPixels (x, t.h - 1 - y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, px);
    glBindFramebuffer (GL_FRAMEBUFFER, 0);
    return rgb_color (px[0], px[1], px[2], px[3]);
  }
  void internal_set_pixel (int x, int y, color c) {
    (void) x; (void) y; (void) c; }
};

picture
gpu_backing_picture (int w, int h) {
  return picture (tm_new<gpu_picture_rep> (max (w, 1), max (h, 1)));
}

static gpu_picture_rep*
as_gpu (picture p) {
  if (is_nil (p)) return NULL;
  return dynamic_cast<gpu_picture_rep*> (p.operator-> ());
}

bool
is_gpu_picture (picture p) {
  return as_gpu (p) != NULL;
}

// copy a rectangle of src (device pixels, y down) to dst at (dx, dy)
static void
blit (GLuint src_fbo, int src_h, int x1, int y1, int x2, int y2,
      GLuint dst_fbo, int dst_h, int dx, int dy) {
  int w= x2 - x1, h= y2 - y1;
  if (w <= 0 || h <= 0) return;
  glBindFramebuffer (GL_READ_FRAMEBUFFER, src_fbo);
  glBindFramebuffer (GL_DRAW_FRAMEBUFFER, dst_fbo);
  glDisable (GL_SCISSOR_TEST);
  glBlitFramebuffer (x1, src_h - y2, x2, src_h - y1,
                     dx, dst_h - (dy + h), dx + w, dst_h - dy,
                     GL_COLOR_BUFFER_BIT, GL_NEAREST);
  glBindFramebuffer (GL_READ_FRAMEBUFFER, 0);
  glBindFramebuffer (GL_DRAW_FRAMEBUFFER, 0);
}

void
gpu_translate_picture (picture p, int dpx, int dpy) {
  gpu_picture_rep* g= as_gpu (p);
  if (g == NULL || !ensure_target (&g->t)) return;
  gpu_target& t= g->t;
  if (dpx >= t.w || -dpx >= t.w || dpy >= t.h || -dpy >= t.h) return;
  gpu_flush_all ();
  if (t.fbo2 == 0 && !make_texture_target (t.fbo2, t.tex2, t.w, t.h)) return;
  int x0= max (0, dpx), x1= min (t.w, t.w + dpx);
  int y0= max (0, dpy), y1= min (t.h, t.h + dpy);
  blit (t.fbo, t.h, x0 - dpx, y0 - dpy, x1 - dpx, y1 - dpy, t.fbo2, t.h, x0, y0);
  // the strips which were not copied are repainted by the caller
  std::swap (t.fbo, t.fbo2);
  std::swap (t.tex, t.tex2);
  t.gen++;
}

picture
gpu_copy_picture (picture p) {
  gpu_picture_rep* g= as_gpu (p);
  if (g == NULL || !ensure_target (&g->t)) return picture ();
  gpu_flush_all ();
  gpu_picture_rep* c= tm_new<gpu_picture_rep> (g->t.w, g->t.h);
  picture cp (c);
  if (!ensure_target (&c->t)) return picture ();
  blit (g->t.fbo, g->t.h, 0, 0, g->t.w, g->t.h, c->t.fbo, c->t.h, 0, 0);
  c->t.gen++;
  c->ox= g->ox; c->oy= g->oy;
  return cp;
}

/******************************************************************************
* The renderer
******************************************************************************/

class gpu_renderer_rep: public basic_renderer_rep {
public:
  picture pic;          // keeps the target of a backing store alive
  gpu_target* t;
  bool proxy;
  color fg, bg;
  SI lw;
  std::vector<std::vector<double> > trs; // the transformations (user space)

  gpu_renderer_rep (picture p, gpu_target* t2, double zoom);
  ~gpu_renderer_rep () {}
  void* get_handle () { return (void*) this; }

  // geometry, as the MuPDF renderer: PDF points at integer positions (y
  // up), then device pixels (y down)
  double to_x (SI x) {
    x += ox;
    if (x>=0) x= x/pixel; else x= (x-pixel+1)/pixel;
    return x; }
  double to_y (SI y) {
    y += oy;
    if (y>=0) y= y/pixel; else y= (y-pixel+1)/pixel;
    return y; }
  void device (double ux, double uy, float& dx, float& dy) {
    if (!trs.empty ()) {
      const std::vector<double>& m= trs.back ();
      double nx= m[0]*ux + m[2]*uy + m[4], ny= m[1]*ux + m[3]*uy + m[5];
      ux= nx; uy= ny;
    }
    dx= (float) ux; dy= (float) -uy; }
  void map (SI x, SI y, float& dx, float& dy) { device (to_x (x), to_y (y), dx, dy); }
  void clip_box (int& x1, int& y1, int& x2, int& y2) {
    SI ax1, ay1, ax2, ay2;
    get_clipping (ax1, ay1, ax2, ay2);
    x1= (int) to_x (ax1); x2= (int) to_x (ax2);
    y1= (int) -to_y (ay2); y2= (int) -to_y (ay1); }
  bool ready () { return ensure_target (t); }

  // a quad of user space corners (x1, y1) bottom left, (x2, y2) top right
  void user_quad (double x1, double y1, double x2, double y2, int mode, GLuint tex,
                  float u0, float v0, float u1, float v1,
                  float r, float g, float b, float a) {
    int s1, s2, s3, s4;
    clip_box (s1, s2, s3, s4);
    batch (t, mode, tex, s1, s2, s3, s4);
    float px[4], py[4];
    device (x1, y2, px[0], py[0]); device (x2, y2, px[1], py[1]);
    device (x1, y1, px[2], py[2]); device (x2, y1, px[3], py[3]);
    quad (px, py, u0, v0, u1, v1, r, g, b, a); }
  void solid (double x1, double y1, double x2, double y2, color c) {
    int r, g, b, a;
    get_rgb_color (c, r, g, b, a);
    if (a <= 0) return;
    float fa= a / 255.f;
    float w= 2.f / ATLAS; // the white corner
    user_quad (x1, y1, x2, y2, 2, 0, w * 0.5f, w * 0.5f, w * 1.5f, w * 1.5f,
               r / 255.f * fa, g / 255.f * fa, b / 255.f * fa, fa); }
  void pattern_fill (SI x1, SI y1, SI x2, SI y2, brush br);
  void vector (tvg::Shape* s, float bx1, float by1, float bx2, float by2) {
    int s1, s2, s3, s4;
    clip_box (s1, s2, s3, s4);
    add_vector (t, s, s1, s2, s3, s4, bx1, by1, bx2, by2); }
  void stroke_style (tvg::Shape* s, bool closed= false);
  void fill_style (tvg::Shape* s, color c) {
    int r, g, b, a; get_rgb_color (c, r, g, b, a);
    s->fill ((uint8_t) r, (uint8_t) g, (uint8_t) b, (uint8_t) a); }

  void set_zoom_factor (double zoom, bool safe= true);
  void get_extents (SI& w2, SI& h2) { w2= t->w; h2= t->h; }

  void set_transformation (frame fr);
  void reset_transformation ();
  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  void set_pencil (pencil p);
  void set_brush (brush b);
  void set_background (brush b);

  void clear_device (SI x1, SI y1, SI x2, SI y2);
  void draw (int c, font_glyphs fn, SI x, SI y);
  void line (SI x1, SI y1, SI x2, SI y2);
  void lines (array<SI> x, array<SI> y);
  void clear (SI x1, SI y1, SI x2, SI y2);
  void fill (SI x1, SI y1, SI x2, SI y2);
  void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void polygon (array<SI> x, array<SI> y, bool convex= true);
  void rounded_rectangle (SI x1, SI y1, SI x2, SI y2,
                          SI r_tl, SI r_tr, SI r_br, SI r_bl, bool filled);
  void bezier_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta, bool filled);

  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  void draw_picture (picture pict, SI x, SI y, int alpha);
  void draw_picture_scaled (picture pict, SI x, SI y, double s, int alpha);
};

gpu_renderer_rep::gpu_renderer_rep (picture p, gpu_target* t2, double zoom):
  basic_renderer_rep (true, 1.0, t2->w, t2->h), pic (p), t (t2),
  proxy (false), fg (black), bg (white), lw (PIXEL)
{
  // as mupdf_image_renderer_rep: the zoom of a picture renderer
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI)  tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;
  cx1= 0; cy1= -t->h * pixel; cx2= t->w * pixel; cy2= 0;
}

void
gpu_renderer_rep::set_zoom_factor (double zoom, bool safe) {
  // as the MuPDF renderer: the retina factor is applied here
  (void) safe;
  renderer_rep::set_zoom_factor (retina_factor * zoom, false);
  retina_pixel= pixel * retina_factor;
}

void
gpu_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");
  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);
  frame cv= scaling (point (pixel, -pixel), point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  // in the user space of to_x and to_y (y up), composed with the current one
  std::vector<double> m= { ux[0], ux[1], uy[0], uy[1], o[0], o[1] };
  if (!trs.empty ()) {
    const std::vector<double>& p= trs.back ();
    std::vector<double> c= {
      p[0]*m[0] + p[2]*m[1], p[1]*m[0] + p[3]*m[1],
      p[0]*m[2] + p[2]*m[3], p[1]*m[2] + p[3]*m[3],
      p[0]*m[4] + p[2]*m[5] + p[4], p[1]*m[4] + p[3]*m[5] + p[5] };
    m= c;
  }
  trs.push_back (m);
  rectangle nclip= fr [oclip];
  clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
gpu_renderer_rep::reset_transformation () {
  unclip ();
  if (!trs.empty ()) trs.pop_back ();
}

void
gpu_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  // the clip is read when something is drawn (the scissor of its batch)
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);
}

void
gpu_renderer_rep::set_pencil (pencil p) {
  pen= p;
  lw= pen->get_width ();
  fg= pen->get_color ();
  if (pen->get_type () == pencil_brush) fg_brush= pen->get_brush ();
}

void
gpu_renderer_rep::set_brush (brush b) {
  fg_brush= b;
  pen= pencil (b);
  fg= pen->get_color ();
  lw= pen->get_width ();
  if (!is_nil (b) && b->get_type () == brush_none) {
    pen= pencil ();
    fg_brush= brush ();
  }
}

void
gpu_renderer_rep::set_background (brush b) {
  bg_brush= b;
  bg= b->get_color ();
}

// a fill with a pattern: its tiles have a corner at the origin of the
// document, as the patterns of the MuPDF renderer (placed_pattern)
// the texture of a pattern, at the size of its tiles in device pixels
static cached_texture*
pattern_texture (brush br, SI px) {
  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, px);
  if (w <= 0 || h <= 0) return NULL;
  c_string name (as_string (u));
  std::string key= std::string ((char*) name) + "#" +
                   std::to_string (w) + "x" + std::to_string (h);
  auto it= G.patterns.find (key);
  if (it != G.patterns.end ()) return &it->second;
  fz_pixmap* pix= mupdf_load_pixmap (u, w, h, eff, px);
  GLuint tex= upload_pixmap (pix, true);
  if (pix != NULL) fz_drop_pixmap (mupdf_context (), pix);
  if (tex == 0) return NULL;
  G.patterns[key]= { tex, (int) w, (int) h, picture (), 0, 0 };
  return &G.patterns[key];
}

void
gpu_renderer_rep::pattern_fill (SI x1, SI y1, SI x2, SI y2, brush br) {
  SI px= (brushpx == -1 ? pixel : brushpx);
  cached_texture* ct= pattern_texture (br, px);
  if (ct == NULL) return;
  double ux1= to_x (min (x1, x2)), uy1= to_y (min (y1, y2));
  double ux2= to_x (max (x1, x2)), uy2= to_y (max (y1, y2));
  // a device pixel (x, y) takes the pixel (x - dx0, y - dy0) of the tiles
  double dx0= to_x (0), dy0= -to_y (0);
  float u0= (float) ((ux1 - dx0) / ct->w), u1= (float) ((ux2 - dx0) / ct->w);
  float v0= (float) ((-uy2 - dy0) / ct->h), v1= (float) ((-uy1 - dy0) / ct->h);
  float a= br->get_alpha () / 255.f;
  user_quad (ux1, uy1, ux2, uy2, 1, ct->tex, u0, v0, u1, v1, a, a, a, a);
}

void
gpu_renderer_rep::clear_device (SI x1, SI y1, SI x2, SI y2) {
  // white with the neutral pattern over it, as the Qt port: a tile composed
  // over white once, at its natural size whatever the zoom
  if (!ready ()) return;
  static url u= url_none ();
  static int iw= 0, ih= 0;
  static bool resolved= false;
  if (!resolved) {
    resolved= true;
    u= resolve_pattern (url ("neutral-pattern.png"));
    if (!is_none (u)) image_size (u, iw, ih);
  }
  double ux1= to_x (min (x1, x2)), uy1= to_y (min (y1, y2));
  double ux2= to_x (max (x1, x2)), uy2= to_y (max (y1, y2));
  int tw= iw * retina_factor, th= ih * retina_factor;
  if (is_none (u) || tw <= 0 || th <= 0) {
    solid (ux1, uy1, ux2, uy2, white);
    return;
  }
  std::string key= "neutral#" + std::to_string (tw);
  auto it= G.patterns.find (key);
  if (it == G.patterns.end ()) {
    brush neutral (compound ("pattern", as_string (u),
                             as_string ((int) (tw * pixel)), as_string ((int) (th * pixel))),
                   255);
    url pu; SI w, h; tree eff;
    get_pattern_data (pu, w, h, eff, neutral, pixel);
    fz_pixmap* pix= mupdf_load_pixmap (pu, w, h, eff, pixel);
    GLuint tex= upload_pixmap (pix, true, true);
    if (pix != NULL) fz_drop_pixmap (mupdf_context (), pix);
    if (tex == 0) { solid (ux1, uy1, ux2, uy2, white); return; }
    G.patterns[key]= { tex, (int) w, (int) h, picture (), 0, 0 };
    it= G.patterns.find (key);
  }
  cached_texture& ct= it->second;
  double dx0= to_x (0), dy0= -to_y (0);
  user_quad (ux1, uy1, ux2, uy2, 1, ct.tex,
             (float) ((ux1 - dx0) / ct.w), (float) ((-uy2 - dy0) / ct.h),
             (float) ((ux2 - dx0) / ct.w), (float) ((-uy1 - dy0) / ct.h), 1, 1, 1, 1);
}

void
gpu_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  if (!ready ()) return;
  if (!is_nil (bg_brush) && bg_brush->get_type () == brush_pattern) {
    pattern_fill (x1, y1, x2, y2, bg_brush);
    return;
  }
  solid (to_x (min (x1, x2)), to_y (min (y1, y2)),
         to_x (max (x1, x2)), to_y (max (y1, y2)), bg);
}

void
gpu_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if (!ready () || x1 >= x2 || y1 >= y2) return;
  if (!is_nil (fg_brush) && fg_brush->get_type () == brush_pattern) {
    pattern_fill (x1, y1, x2, y2, fg_brush);
    return;
  }
  // a rule thinner than a pixel still covers one (a fraction bar)
  double ux1= to_x (x1), uy1= to_y (y1), ux2= to_x (x2), uy2= to_y (y2);
  if (ux2 <= ux1) ux2= ux1 + 1;
  if (uy2 <= uy1) uy2= uy1 + 1;
  solid (ux1, uy1, ux2, uy2, fg);
}

void
gpu_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // the glyph bitmaps of TeXmacs (as the X11 and the Qt ports), in the
  // atlas; a pencil with a pattern draws them in its color for now
  if (!ready ()) return;
  bool pattern= pen->get_type () == pencil_brush && !is_nil (pen->get_brush ()) &&
                pen->get_brush ()->get_type () == brush_pattern;
  if (G.slug_on && trs.empty () && !pattern) {
    // from its outline (Slug), where the MuPDF renderer draws it: the
    // glyph of its font file at the origin, a em of g->em pixels
    slug_glyph* g= slug_lookup (fng, c);
    if (g != NULL) {
      if (g->nb == 0) return; // no ink
      int s1, s2, s3, s4;
      clip_box (s1, s2, s3, s4);
      batch (t, 4, 0, s1, s2, s3, s4);
      int r, gg, b, a;
      get_rgb_color (fg, r, gg, b, a);
      if (get_reverse_colors ()) reverse (r, gg, b);
      float fa= a / 255.f;
      float ox_= (float) to_x (x), oy_= (float) -to_y (y);
      unsigned off= g->offset, nb= g->nb;
      float inst[13]= { ox_, oy_, g->em, g->bx0, g->by0, g->bx1, g->by1, 0, 0,
                        r / 255.f * fa, gg / 255.f * fa, b / 255.f * fa, fa };
      memcpy (&inst[7], &off, 4); memcpy (&inst[8], &nb, 4);
      G.slug_inst.insert (G.slug_inst.end (), inst, inst + 13);
      if (on_screen (t)) feed (inst);
      return;
    }
  }
  glyph_key k { (void*) fng.rep, c };
  auto it= G.glyphs.find (k);
  if (it == G.glyphs.end ()) {
    // the glyph rendered by MuPDF when its font has a file, antialiased at
    // its size and as the MuPDF renderer draws it; else the bitmap of
    // TeXmacs, rendered at std_shrinkf times its size and shrunk
    string mcov;
    int mw= 0, mh= 0, mx= 0, my= 0;
    bool from_mupdf= mupdf_glyph_bitmap (fng->res_name, c, mcov, mw, mh, mx, my);
    glyph gl;
    SI xo= 0, yo= 0;
    int w, h;
    if (from_mupdf) { w= mw; h= mh; }
    else {
      glyph pre_gl= fng->get (c);
      if (is_nil (pre_gl)) return;
      gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo, 1.0);
      w= gl->width; h= gl->height;
    }
    if (w > 512 || h > 512) return;
    if (G.ax + w + 1 > ATLAS) { G.ax= 0; G.ay += G.arow + 1; G.arow= 0; }
    if (G.ay + h + 1 > ATLAS) {
      // full: start again (what is queued uses the old content)
      gpu_flush_all ();
      G.glyphs.clear ();
      G.fonts.clear ();
      G.ax= 4; G.ay= 0; G.arow= 4;
    }
    if (w > 0 && h > 0) {
      std::vector<unsigned char> cov ((size_t) w * h);
      if (from_mupdf) memcpy (cov.data (), &mcov[0], (size_t) w * h);
      else {
        int nr_cols= std_shrinkf * std_shrinkf;
        if (nr_cols >= 64) nr_cols= 64;
        for (int j= 0; j < h; j++)
          for (int i= 0; i < w; i++)
            cov[j*w + i]= (unsigned char) min (255, (255 * gl->get_x (i, j)) / nr_cols);
      }
      // what is queued from the atlas is drawn before it changes
      if (G.mode == 2) flush_quads ();
      glBindTexture (GL_TEXTURE_2D, G.atlas);
      glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
      glTexSubImage2D (GL_TEXTURE_2D, 0, G.ax, G.ay, w, h, GL_RED, GL_UNSIGNED_BYTE, cov.data ());
    }
    G.glyphs[k]= { G.ax, G.ay, w, h, xo, yo, from_mupdf, mx, my };
    G.fonts.push_back (fng);
    G.ax += w + 1; G.arow= max (G.arow, h);
    it= G.glyphs.find (k);
  }
  const glyph_slot& s= it->second;
  if (s.w <= 0 || s.h <= 0) return;
  // placed as the MuPDF renderer places its glyphs: rendered by MuPDF from
  // the origin, or as its images of the bitmaps of TeXmacs
  double left, bottom;
  if (s.mupdf) {
    left= to_x (x) + s.dx;
    bottom= to_y (y) - s.dy - s.h;  // y up: the top is s.dy below the origin
  }
  else {
    left= to_x (x - s.xo * std_shrinkf);
    bottom= to_y (y + s.yo * std_shrinkf - s.h * pixel);
  }
  float A= (float) ATLAS;
  if (pen->get_type () == pencil_brush && !is_nil (pen->get_brush ()) &&
      pen->get_brush ()->get_type () == brush_pattern) {
    // filled with the pattern, sampled where the glyph is on the screen
    // from the origin of the document (as the MuPDF renderer's draw_bis)
    brush br= pen->get_brush ();
    cached_texture* ct= pattern_texture (br, brushpx == -1 ? pixel : brushpx);
    if (ct == NULL) return;
    int s1, s2, s3, s4;
    clip_box (s1, s2, s3, s4);
    batch (t, 3, ct->tex, s1, s2, s3, s4,
           (float) to_x (0), (float) -to_y (0), (float) ct->w, (float) ct->h);
    float px[4], py[4];
    device (left, bottom + s.h, px[0], py[0]); device (left + s.w, bottom + s.h, px[1], py[1]);
    device (left, bottom, px[2], py[2]);       device (left + s.w, bottom, px[3], py[3]);
    float a= br->get_alpha () / 255.f;
    quad (px, py, s.x / A, s.y / A, (s.x + s.w) / A, (s.y + s.h) / A, a, a, a, a);
    return;
  }
  int r, g, b, a;
  get_rgb_color (fg, r, g, b, a);
  if (get_reverse_colors ()) reverse (r, g, b);
  float fa= a / 255.f;
  user_quad (left, bottom, left + s.w, bottom + s.h, 2, 0,
             s.x / A, s.y / A, (s.x + s.w) / A, (s.y + s.h) / A,
             r / 255.f * fa, g / 255.f * fa, b / 255.f * fa, fa);
}

void
gpu_renderer_rep::stroke_style (tvg::Shape* s, bool closed) {
  float w= (float) lw / pixel;
  if (w < 1.f) w= 1.f;
  s->strokeWidth (w);
  int r, g, b, a; get_rgb_color (fg, r, g, b, a);
  s->strokeFill ((uint8_t) r, (uint8_t) g, (uint8_t) b, (uint8_t) a);
  if (closed || pen->get_cap () == cap_round) s->strokeCap (tvg::StrokeCap::Round);
  else if (pen->get_cap () == cap_flat) s->strokeCap (tvg::StrokeCap::Butt);
  else s->strokeCap (tvg::StrokeCap::Square);
  s->strokeJoin (tvg::StrokeJoin::Round);
}

void
gpu_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  if (!ready ()) return;
  float ax, ay, bx, by;
  map (x1, y1, ax, ay); map (x2, y2, bx, by);
  float w= max (1.f, (float) lw / pixel);
  if (trs.empty () && (ax == bx || ay == by) && w <= 3.f) {
    // a thin horizontal or vertical line (the rules of mathematics, the
    // borders of tables): a quad, square and round caps extending it by
    // half its width (a round cap of a pixel or two looks the same), so
    // that it is not a pass of ThorVG of its own
    float e= (pen->get_cap () == cap_flat) ? 0.f : w / 2.f;
    float qx1= min (ax, bx), qx2= max (ax, bx), qy1= min (ay, by), qy2= max (ay, by);
    if (ay == by) { qx1 -= e; qx2 += e; qy1 -= w / 2.f; qy2 += w / 2.f; }
    else { qy1 -= e; qy2 += e; qx1 -= w / 2.f; qx2 += w / 2.f; }
    solid (qx1, -qy2, qx2, -qy1, fg);
    return;
  }
  tvg::Shape* s= tvg::Shape::gen ();
  s->moveTo (ax, ay);
  s->lineTo (bx, by);
  stroke_style (s);
  vector (s, min (ax, bx) - w, min (ay, by) - w, max (ax, bx) + w, max (ay, by) + w);
}

void
gpu_renderer_rep::lines (array<SI> x, array<SI> y) {
  int n= N(x);
  if (!ready () || N(y) != n || n < 1) return;
  tvg::Shape* s= tvg::Shape::gen ();
  float bx1= 1e9, by1= 1e9, bx2= -1e9, by2= -1e9;
  for (int i= 0; i < n; i++) {
    float px, py;
    map (x[i], y[i], px, py);
    if (i == 0) s->moveTo (px, py); else s->lineTo (px, py);
    bx1= min (bx1, px); by1= min (by1, py); bx2= max (bx2, px); by2= max (by2, py);
  }
  stroke_style (s, x[n-1] == x[0] && y[n-1] == y[0]);
  float w= max (1.f, (float) lw / pixel);
  vector (s, bx1 - w, by1 - w, bx2 + w, by2 + w);
}

void
gpu_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  int n= N(x);
  if (!ready () || N(y) != n || n < 1) return;
  tvg::Shape* s= tvg::Shape::gen ();
  float bx1= 1e9, by1= 1e9, bx2= -1e9, by2= -1e9;
  for (int i= 0; i < n; i++) {
    float px, py;
    map (x[i], y[i], px, py);
    if (i == 0) s->moveTo (px, py); else s->lineTo (px, py);
    bx1= min (bx1, px); by1= min (by1, py); bx2= max (bx2, px); by2= max (by2, py);
  }
  s->close ();
  // as the MuPDF renderer: nonzero winding for convex polygons, even-odd
  // for the others
  s->fillRule (convex ? tvg::FillRule::NonZero : tvg::FillRule::EvenOdd);
  fill_style (s, fg);
  vector (s, bx1, by1, bx2, by2);
}

void
gpu_renderer_rep::bezier_arc (SI x1, SI y1, SI x2, SI y2,
                              int alpha, int delta, bool filled) {
  // as the MuPDF renderer: sub-arcs of at most 90 degrees
  if (!ready ()) return;
  double xx1= to_x (x1), yy1= to_y (y1), xx2= to_x (x2), yy2= to_y (y2);
  double cx= (xx1 + xx2) / 2, cy= (yy1 + yy2) / 2;
  double rx= (xx2 - xx1) / 2, ry= (yy2 - yy1) / 2;
  int n= 1 + abs (delta) / (90*64);
  if ((abs (delta) % (90*64)) == 0) n--;
  if (n <= 0) return;
  double phi= 2.0 * M_PI * delta / (n * 360.0 * 64.0);
  double a= 2.0 * M_PI * alpha / (360.0 * 64.0);
  double sphi= sin (phi/2), cphi= cos (phi/2);
  double bx0= cphi, by0= -sphi;
  double bx1= (4.0 - bx0) / 3.0, by1= (1.0 - bx0) * (3.0 - bx0) / (3.0 * by0);
  double bx2= bx1, by2= -by1, bx3= bx0, by3= -by0;
  tvg::Shape* s= tvg::Shape::gen ();
  auto P= [&] (double bx, double by, double sp, double cp, float& dx, float& dy) {
    device (cx + rx * (bx*cp - by*sp), cy + ry * (bx*sp + by*cp), dx, dy); };
  for (int k= 0; k < n; k++) {
    double sp= sin (phi * (k + 0.5) + a), cp= cos (phi * (k + 0.5) + a);
    float p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y;
    P (bx0, by0, sp, cp, p0x, p0y);
    P (bx1, by1, sp, cp, p1x, p1y);
    P (bx2, by2, sp, cp, p2x, p2y);
    P (bx3, by3, sp, cp, p3x, p3y);
    if (k == 0) s->moveTo (p0x, p0y);
    s->cubicTo (p1x, p1y, p2x, p2y, p3x, p3y);
  }
  if (filled) fill_style (s, fg);
  else {
    if (abs (delta) == 360*64) s->close ();
    stroke_style (s, abs (delta) == 360*64);
  }
  float w= filled ? 0.f : max (1.f, (float) lw / pixel);
  float ax, ay, bx, by;
  device (xx1, yy1, ax, ay); device (xx2, yy2, bx, by);
  float m= (float) max (fabs (rx), fabs (ry)) + w;
  vector (s, (float) min (ax, bx) - m, (float) min (ay, by) - m,
          (float) max (ax, bx) + m, (float) max (ay, by) + m);
}

void
gpu_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  bezier_arc (x1, y1, x2, y2, alpha, delta, false);
}

void
gpu_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  bezier_arc (x1, y1, x2, y2, alpha, delta, true);
}

void
gpu_renderer_rep::rounded_rectangle (SI x1, SI y1, SI x2, SI y2,
                                     SI r_tl, SI r_tr, SI r_br, SI r_bl,
                                     bool filled) {
  if (!ready ()) return;
  double xx1= to_x (min (x1, x2)), yy1= to_y (min (y1, y2));
  double xx2= to_x (max (x1, x2)), yy2= to_y (max (y1, y2));
  if (filled && r_tl <= 0 && r_tr <= 0 && r_br <= 0 && r_bl <= 0 && trs.empty ()) {
    solid (xx1, yy1, xx2, yy2, fg);
    return;
  }
  double mr= min (xx2 - xx1, yy2 - yy1) / 2.0;
  double rtl= min ((double) r_tl / pixel, mr), rtr= min ((double) r_tr / pixel, mr);
  double rbr= min ((double) r_br / pixel, mr), rbl= min ((double) r_bl / pixel, mr);
  const double k= 0.5522847498;
  tvg::Shape* s= tvg::Shape::gen ();
  float px, py, c1x, c1y, c2x, c2y;
  // user space (y up): yy1 is the bottom, yy2 the top
  device (xx1 + rbl, yy1, px, py); s->moveTo (px, py);
  device (xx2 - rbr, yy1, px, py); s->lineTo (px, py);
  if (rbr > 0) {
    device (xx2 - rbr + rbr*k, yy1, c1x, c1y); device (xx2, yy1 + rbr - rbr*k, c2x, c2y);
    device (xx2, yy1 + rbr, px, py); s->cubicTo (c1x, c1y, c2x, c2y, px, py); }
  device (xx2, yy2 - rtr, px, py); s->lineTo (px, py);
  if (rtr > 0) {
    device (xx2, yy2 - rtr + rtr*k, c1x, c1y); device (xx2 - rtr + rtr*k, yy2, c2x, c2y);
    device (xx2 - rtr, yy2, px, py); s->cubicTo (c1x, c1y, c2x, c2y, px, py); }
  device (xx1 + rtl, yy2, px, py); s->lineTo (px, py);
  if (rtl > 0) {
    device (xx1 + rtl - rtl*k, yy2, c1x, c1y); device (xx1, yy2 - rtl + rtl*k, c2x, c2y);
    device (xx1, yy2 - rtl, px, py); s->cubicTo (c1x, c1y, c2x, c2y, px, py); }
  device (xx1, yy1 + rbl, px, py); s->lineTo (px, py);
  if (rbl > 0) {
    device (xx1, yy1 + rbl - rbl*k, c1x, c1y); device (xx1 + rbl - rbl*k, yy1, c2x, c2y);
    device (xx1 + rbl, yy1, px, py); s->cubicTo (c1x, c1y, c2x, c2y, px, py); }
  s->close ();
  if (filled) fill_style (s, fg);
  else stroke_style (s, true);
  float w= filled ? 0.f : max (1.f, (float) lw / pixel);
  float ax, ay, bx, by;
  device (xx1, yy1, ax, ay); device (xx2, yy2, bx, by);
  vector (s, min (ax, bx) - w, min (ay, by) - w, max (ax, bx) + w, max (ay, by) + w);
}

/******************************************************************************
* Pictures
******************************************************************************/

void
gpu_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  draw_picture_scaled (p, x - p->get_origin_x () * pixel,
                       y - p->get_origin_y () * pixel, 1.0, alpha);
}

// the picture scaled by s, its lower left corner at (x, y)
void
gpu_renderer_rep::draw_picture_scaled (picture p, SI x, SI y, double sc, int alpha) {
  if (!ready () || is_nil (p) || alpha <= 0) return;
  float a= alpha / 255.f;
  double left= to_x (x), bottom= to_y (y);
  double w= p->get_width () * sc, h= p->get_height () * sc;
  gpu_picture_rep* g= as_gpu (p);
  if (g != NULL) {
    if (!ensure_target (&g->t)) return;
    // the texture of a target: its first row at the bottom
    if (on_screen (t)) feed (g->t.gen);
    user_quad (left, bottom, left + w, bottom + h, 1, g->t.tex, 0, 1, 1, 0, a, a, a, a);
    return;
  }
  cached_texture* ct= picture_texture (p);
  if (ct == NULL) return;
  if (on_screen (t)) feed (p->get_unique_id ());
  user_quad (left, bottom, left + w, bottom + h, 1, ct->tex, 0, 0, 1, 1, a, a, a, a);
}

/******************************************************************************
* Shadows: proxies on the same target, as the MuPDF and the Qt renderers
******************************************************************************/

void
gpu_renderer_rep::new_shadow (renderer& ren) {
  bool want_proxy= !proxy;
  if (ren != NULL) {
    gpu_renderer_rep* old= dynamic_cast<gpu_renderer_rep*> (ren);
    if (old == NULL || old->proxy != want_proxy ||
        (want_proxy && old->t != t) ||
        (!want_proxy && (old->t->w != t->w || old->t->h != t->h))) {
      delete_shadow (ren);
      ren= NULL;
    }
  }
  if (ren == NULL) {
    gpu_renderer_rep* sh;
    if (want_proxy) {
      sh= tm_new<gpu_renderer_rep> (pic, t, zoomf);
      sh->proxy= true;
    }
    else {
      picture sp= gpu_backing_picture (t->w, t->h);
      sh= tm_new<gpu_renderer_rep> (sp, &as_gpu (sp)->t, zoomf);
    }
    ren= (renderer) sh;
  }
}

void
gpu_renderer_rep::delete_shadow (renderer& ren) {
  if (ren != NULL) {
    if (dynamic_cast<gpu_renderer_rep*> (ren) != NULL) gpu_flush_all ();
    tm_delete (ren);
    ren= NULL;
  }
}

void
gpu_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  gpu_renderer_rep* sh= dynamic_cast<gpu_renderer_rep*> (ren);
  if (sh == NULL) return;
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox); y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox); y2= min (y2, cy2- oy);
  sh->ox= ox; sh->oy= oy;
  sh->master= this;
  sh->cx1= x1+ ox; sh->cy1= y1+ oy;
  sh->cx2= x2+ ox; sh->cy2= y2+ oy;
  sh->trs.clear ();
  if (sh->t == t || !ready () || !sh->ready ()) return;
  // the store of the active graphics: a real copy
  decode (x1, y1); decode (x2, y2);
  gpu_flush_all ();
  blit (t->screen ? 0 : t->fbo, t->h, x1, y2, x2, y1,
        sh->t->fbo, sh->t->h, x1, y2);
  sh->t->gen++;
}

void
gpu_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  gpu_renderer_rep* sh= dynamic_cast<gpu_renderer_rep*> (ren);
  if (sh == NULL || sh->t == t || !ready () || !sh->ready ()) return;
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox); y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox); y2= min (y2, cy2- oy);
  decode (x1, y1); decode (x2, y2);
  gpu_flush_all ();
  blit (sh->t->fbo, sh->t->h, x1, y2, x2, y1,
        t->screen ? 0 : t->fbo, t->h, x1, y2);
  t->gen++;
  if (on_screen (t)) { SI k[5]= { 7, x1, y1, x2, y2 }; feed (k); }
}

void
gpu_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  if (master == NULL) return;
  gpu_renderer_rep* m= dynamic_cast<gpu_renderer_rep*> (master);
  if (m == NULL || m->t == t) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1); decode (x2, y2);
  m->encode (x1, y1); m->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}

void
gpu_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  gpu_renderer_rep* src= dynamic_cast<gpu_renderer_rep*> (ren);
  if (src == NULL || !ready () || !src->ready ()) return;
  outer_round (x1, y1, x2, y2);
  SI X1= x1, Y1= y1;
  x1= max (x1, cx1- ox); y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox); y2= min (y2, cy2- oy);
  decode (X1, Y1); decode (x1, y1); decode (x2, y2);
  src->decode (x, y);
  x += x1 - X1; y += y1 - Y1;
  if (x1 >= x2 || y2 >= y1) return;
  gpu_flush_all ();
  if (src->t == t) return; // within one target: see gpu_translate_picture
  blit (src->t->screen ? 0 : src->t->fbo, src->t->h, x, y - (y1 - y2), x + (x2 - x1), y,
        t->screen ? 0 : t->fbo, t->h, x1, y2);
  t->gen++;
  if (on_screen (t)) { SI k[6]= { 8, x1, y1, x2, y2, (SI) src->t->gen }; feed (k); }
}

/******************************************************************************
* Interface
******************************************************************************/

renderer
gpu_picture_renderer (picture p, double zoom) {
  gpu_picture_rep* g= as_gpu (p);
  if (g == NULL) return NULL;
  return (renderer) tm_new<gpu_renderer_rep> (p, &g->t, zoom);
}

renderer
gpu_screen_renderer (double zoom) {
  return (renderer) tm_new<gpu_renderer_rep> (picture (), &G.screen, zoom);
}

void
gpu_begin_screen (renderer ren, int w, int h) {
  gpu_flush_all ();
  G.screen.w= w; G.screen.h= h;
  G.frame_hash= 14695981039346656037ULL;
  int sz[2]= { w, h };
  feed (sz);
  gpu_renderer_rep* g= dynamic_cast<gpu_renderer_rep*> (ren);
  if (g != NULL) { g->w= w; g->h= h; g->trs.clear (); }
}

unsigned long long
gpu_frame_hash () {
  gpu_flush_all (); // a pending batch is fed when it is made, not drawn
  return G.frame_hash;
}

bool
is_gpu_renderer (renderer ren) {
  return dynamic_cast<gpu_renderer_rep*> (ren) != NULL;
}

bool
gpu_draw_picture_scaled (renderer ren, picture p, SI x, SI y, double s, int alpha) {
  gpu_renderer_rep* g= dynamic_cast<gpu_renderer_rep*> (ren);
  if (g == NULL) return false;
  g->draw_picture_scaled (p, x, y, s, alpha);
  return true;
}

picture
gpu_read_screen (int w, int h) {
  gpu_flush_all ();
  fz_pixmap* pix= mupdf_new_pixmap (w, h);
  if (pix == NULL || pix->w != w || pix->h != h) return picture ();
  std::vector<unsigned char> rows ((size_t) w * h * 4);
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  glPixelStorei (GL_PACK_ALIGNMENT, 4);
  glReadPixels (0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, rows.data ());
  for (int y= 0; y < h; y++)
    memcpy (pix->samples + (size_t) y * pix->stride,
            rows.data () + (size_t) (h - 1 - y) * w * 4, (size_t) w * 4);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
}

#endif // USE_THORVG
