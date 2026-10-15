/******************************************************************************
* MODULE     : gpu_backend_gl.cpp
* DESCRIPTION: The OpenGL backend of the GPU renderer of the Vue port
*              (OpenGL 3.3 core, 4.1 on macOS, WebGL2 in the browser), with
*              ThorVG's GL engine for the vector shapes
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// All the windows are drawn with one GL context (SDL_GL_MakeCurrent for the
// window being drawn), so that the textures, and the framebuffers of the
// editors, belong to all of them. A target is a texture with a framebuffer,
// drawn with y = 0 at the top of its framebuffer, as ThorVG draws: its
// texture has its first row (v = 0) at the bottom of the picture, the
// texture of an uploaded image at the top. ThorVG's GL engine draws into a
// multisampled framebuffer of its own and copies the result over all of its
// target, so it cannot draw over what is there: it draws into a scratch
// texture, and the part it drew is then composed over the target.

#include "gpu_backend.hpp"

#ifndef USE_THORVG

gpu_backend* gpu_gl_backend () { return NULL; }

#else

#include "tm_ostream.hpp"
#include "basic.hpp"
#include <SDL3/SDL.h>
#include <thorvg.h>
#ifdef __EMSCRIPTEN__
#include <GLES3/gl3.h>
#else
#ifdef __APPLE__
#include <OpenGL/gl3.h>
#else
// the prototypes of OpenGL 3 (Linux, Windows), for the types below
#define GL_GLEXT_PROTOTYPES 1
#include <SDL3/SDL_opengl.h>
#endif
// ThorVG's static library defines its OpenGL entry points as global
// function pointers named as the functions themselves (glCreateProgram...):
// a call to the function would be linked to its pointer. The functions
// used here are loaded by SDL (from the OpenGL framework, libGL or
// opengl32, once the context is current, as Windows wants) into pointers
// of their own, which their names are made to mean.
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
  X(glFlush) \
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
  X(glUniform1f) \
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
  bool ok= true;
#define VUE_GL_LOAD(f) \
  vue_##f= (decltype (vue_##f)) SDL_GL_GetProcAddress (#f); \
  if (vue_##f == NULL) ok= false;
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
#define glFlush vue_glFlush
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
#define glUniform1f vue_glUniform1f
#define glUniform1i vue_glUniform1i
#define glUniform2f vue_glUniform2f
#define glUseProgram vue_glUseProgram
#define glVertexAttribDivisor vue_glVertexAttribDivisor
#define glVertexAttribIPointer vue_glVertexAttribIPointer
#define glVertexAttribPointer vue_glVertexAttribPointer
#define glViewport vue_glViewport
#endif
#include <cmath>

struct gpu_texture_rep {
  GLuint tex;
  GLuint fbo;          // a target: its framebuffer
  int w, h;
  gpu_format f;
};

/******************************************************************************
* The programs: textured quads, from the atlas (a coverage times a color)
* or from a texture (premultiplied, times an alpha); the glyphs of Slug
* (see vue_gpu.cpp)
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

static const char* slug_vertex_src= GLSL_HEADER
  "layout(location=0) in vec3 i_org;\n"   // origin (pixels), pixels a em
  "layout(location=1) in vec4 i_box;\n"   // the box (em)
  "layout(location=2) in uvec2 i_band;\n" // offset of the header, bands
  "layout(location=3) in vec4 i_col;\n"
  "uniform vec2 u_size; uniform float u_bold;\n"
  "out vec2 v_em; flat out vec4 v_box; flat out uvec2 v_band;\n"
  "flat out vec4 v_col; flat out float v_scale;\n"
  "void main () {\n"
  "  int id= gl_VertexID;\n"
  "  vec2 c= vec2 ((id == 1 || id == 3 || id == 4) ? 1.0 : 0.0,\n"
  "                (id == 2 || id == 4 || id == 5) ? 1.0 : 0.0);\n"
  "  float pad= (1.0 + u_bold) / i_org.z;\n"
  "  vec2 em= mix (i_box.xy - pad, i_box.zw + pad, c);\n"
  "  vec2 p= i_org.xy + vec2 (em.x, -em.y) * i_org.z;\n"
  "  v_em= em; v_box= i_box; v_band= i_band; v_col= i_col; v_scale= i_org.z;\n"
  "  gl_Position= vec4 (p.x / u_size.x * 2.0 - 1.0, 1.0 - p.y / u_size.y * 2.0, 0.0, 1.0);\n"
  "}\n";

static const char* slug_fragment_src= GLSL_HEADER
  "precision highp int; precision highp usampler2D;\n"
  "uniform sampler2D u_curves; uniform usampler2D u_bands; uniform float u_bold;\n"
  "in vec2 v_em; flat in vec4 v_box; flat in uvec2 v_band;\n"
  "flat in vec4 v_col; flat in float v_scale;\n"
  "out vec4 o_col;\n"
  "const int W= 1024;\n"
  "uint idx (uint i) { return texelFetch (u_bands, ivec2 (int (i) % W, int (i) / W), 0).r; }\n"
  "vec4 crv (uint k, int j) { int i= int (k) * 2 + j; return texelFetch (u_curves, ivec2 (i % W, i / W), 0); }\n"
  "float ray (uint start, uint count, bool vertical, float hb, out float near) {\n"
  "  float cov= 0.0; near= 1.0e9;\n"
  "  for (uint n= 0u; n < count; n++) {\n"
  "    uint k= idx (start + n);\n"
  "    vec4 a= crv (k, 0); vec4 b= crv (k, 1);\n"
  "    vec2 p1= a.xy - v_em, p2= a.zw - v_em, p3= b.xy - v_em;\n"
  "    if (vertical) { p1= p1.yx; p2= p2.yx; p3= p3.yx; }\n"
  "    if (max (max (p1.x, p2.x), p3.x) * v_scale < -0.5 - abs (hb)) break;\n"
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
  "      cov += clamp (x1 * v_scale + 0.5 + hb, 0.0, 1.0); near= min (near, abs (x1 * v_scale + hb)); }\n"
  "    if (code > 1u) {\n"
  "      cov -= clamp (x2 * v_scale + 0.5 - hb, 0.0, 1.0); near= min (near, abs (x2 * v_scale - hb)); }\n"
  "  }\n"
  "  return cov;\n"
  "}\n"
  "void main () {\n"
  "  uint nb= v_band.y & 0x7FFFFFFFu, h= v_band.x;\n"
  // the edges moved outwards by half of u_bold (the direction of the
  // outline in the high bit; the vertical ray sees it mirrored)
  "  float hb= ((v_band.y >> 31) != 0u ? 0.5 : -0.5) * u_bold;\n"
  "  vec2 rel= (v_em - v_box.xy) / max (v_box.zw - v_box.xy, vec2 (1.0e-6));\n"
  "  uint by= uint (clamp (floor (rel.y * float (nb)), 0.0, float (nb) - 1.0));\n"
  "  uint bx= uint (clamp (floor (rel.x * float (nb)), 0.0, float (nb) - 1.0));\n"
  "  float nx, ny;\n"
  "  float cx= abs (ray (idx (h + 2u * by), idx (h + 2u * by + 1u), false, hb, nx));\n"
  "  float cy= abs (ray (idx (h + 2u * (nb + bx)), idx (h + 2u * (nb + bx) + 1u), true, -hb, ny));\n"
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

static GLuint
link_program (const char* vs, const char* fs) {
  GLuint p= glCreateProgram ();
  glAttachShader (p, compile_shader (GL_VERTEX_SHADER, vs));
  glAttachShader (p, compile_shader (GL_FRAGMENT_SHADER, fs));
  glLinkProgram (p);
  GLint ok= 0;
  glGetProgramiv (p, GL_LINK_STATUS, &ok);
  return ok ? p : 0;
}

/******************************************************************************
* The backend
******************************************************************************/

static const int SMALL= 512;

class gl_backend: public gpu_backend {
  SDL_GLContext ctx= NULL;
  bool failed= false;
  GLuint prog= 0, vao= 0, vbo= 0;
  GLint u_size= -1, u_mode= -1, u_tex= -1, u_atlas= -1, u_porig= -1, u_ptile= -1;
  GLuint blank= 0;  // 1x1, at the unit of u_tex when a draw has no texture
  GLuint slug_prog= 0, slug_vao= 0, slug_vbo= 0;
  GLint su_size= -1, su_curves= -1, su_bands= -1, su_bold= -1;
  // ThorVG: canvases drawing into scratch targets. A pass ends by copying
  // all of the scratch target (GlBlitTask), whatever it drew, so a pass
  // which fits in SMALL is drawn on a small canvas, moved to its origin;
  // the others on one as large as the windows
  tvg::GlCanvas* tvg= NULL;
  gpu_texture_rep scratch= { 0, 0, 0, 0, GPU_RGBA8 };
  tvg::GlCanvas* tvg_small= NULL;
  gpu_texture_rep small= { 0, 0, 0, 0, GPU_RGBA8 };

  bool init ();
  bool make_target (gpu_texture_rep& t, int w, int h);
  void free_target (gpu_texture_rep& t);
  void bind_target (const gpu_pass& p);
  void set_scissor (const gpu_pass& p);
  bool ensure_scratch (int w, int h);
  bool ensure_small ();
  tvg::Shape* as_tvg (const gpu_shape& s);

public:
  const char* name () { return "OpenGL"; }
  unsigned long long window_flags () { return SDL_WINDOW_OPENGL; }
  bool prepare ();
  bool attach (SDL_Window* w);
  string description ();
  void begin_screen (int dw, int dh) { (void) dw; (void) dh; }
  void present (SDL_Window* w) { SDL_GL_SwapWindow (w); }

  gpu_texture new_texture (int w, int h, gpu_format f, bool linear, bool repeat,
                           const void* data);
  void update_texture (gpu_texture t, int x, int y, int w, int h, const void* data);
  void delete_texture (gpu_texture t);
  gpu_texture new_target (int w, int h, bool transparent= false);
  bool targets_flipped () { return true; }

  void draw_quads (const gpu_pass& p, const gpu_quads& q);
  void draw_slug (const gpu_pass& p, gpu_texture curves, gpu_texture bands,
                  const float* inst, int n, float bold);
  void draw_shapes (const gpu_pass& p, const std::vector<gpu_shape>& s,
                    int x1, int y1, int x2, int y2);
  void copy (gpu_texture src, int sw, int sh, int x1, int y1, int x2, int y2,
             gpu_texture dst, int dw, int dh, int dx1, int dy1, int dx2, int dy2);
  bool read (gpu_texture src, int sw, int sh, int x, int y, int w, int h,
             unsigned char* rgba);
  void flush () {}  // (the context draws in order: nothing to submit)
  void finish () { if (ctx != NULL) glFinish (); }
};

bool
gl_backend::prepare () {
  static bool done= false;
  if (done) return !failed;
  done= true;
#ifdef __EMSCRIPTEN__
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_ES);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 0);
#else
  // NOTE: 4.1 is the core profile of macOS; elsewhere 3.3 (the GLSL of the
  // shaders, and what ThorVG needs)
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
#ifdef __APPLE__
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 4);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 1);
#else
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 3);
#endif
  SDL_GL_SetAttribute (SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG);
#endif
  SDL_GL_SetAttribute (SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute (SDL_GL_DEPTH_SIZE, 0);
  SDL_GL_SetAttribute (SDL_GL_STENCIL_SIZE, 0);
  SDL_GL_SetAttribute (SDL_GL_SHARE_WITH_CURRENT_CONTEXT, 1);
#ifndef __EMSCRIPTEN__
  // NOTE: whether there is OpenGL 3, before the first window is made: the
  // windows made for the GPU are not drawn without it (Windows with the
  // driver of Microsoft only, as on the runners of GitHub, has OpenGL 1.1)
  bool ok= false;
  SDL_Window* probe= SDL_CreateWindow ("", 1, 1,
                                       SDL_WINDOW_OPENGL | SDL_WINDOW_HIDDEN);
  if (probe != NULL) {
    SDL_GLContext c= SDL_GL_CreateContext (probe);
    if (c != NULL) {
      ok= vue_gl_load ();
      SDL_GL_MakeCurrent (probe, NULL);
      SDL_GL_DestroyContext (c);
    }
    SDL_DestroyWindow (probe);
  }
  if (!ok) {
    cout << "TeXmacs] GPU: no OpenGL 3 (" << SDL_GetError ()
         << "), drawing with MuPDF" << LF;
    failed= true;
  }
#endif
  return !failed;
}

bool
gl_backend::init () {
  prog= link_program (vertex_src, fragment_src);
  if (prog == 0) { cout << "TeXmacs] GPU: the program does not link" << LF; return false; }
  u_size = glGetUniformLocation (prog, "u_size");
  u_mode = glGetUniformLocation (prog, "u_mode");
  u_tex  = glGetUniformLocation (prog, "u_tex");
  u_atlas= glGetUniformLocation (prog, "u_atlas");
  u_porig= glGetUniformLocation (prog, "u_porig");
  u_ptile= glGetUniformLocation (prog, "u_ptile");
  glGenVertexArrays (1, &vao);
  glGenBuffers (1, &vbo);
  // a texture of 1x1 for the units which a draw does not use: the samplers
  // of the program must always have a complete texture (macOS warns of the
  // texture 0, "unloadable", otherwise)
  glGenTextures (1, &blank);
  glBindTexture (GL_TEXTURE_2D, blank);
  unsigned char white[4]= { 255, 255, 255, 255 };
  glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA8, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, white);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  // Slug: compiled when it is asked for (vue_gpu.cpp)
  slug_prog= link_program (slug_vertex_src, slug_fragment_src);
  if (slug_prog != 0) {
    su_size  = glGetUniformLocation (slug_prog, "u_size");
    su_curves= glGetUniformLocation (slug_prog, "u_curves");
    su_bands = glGetUniformLocation (slug_prog, "u_bands");
    su_bold  = glGetUniformLocation (slug_prog, "u_bold");
    glGenVertexArrays (1, &slug_vao);
    glGenBuffers (1, &slug_vbo);
  }
  // ThorVG
  if (tvg::Initializer::init (0) != tvg::Result::Success) {
    cout << "TeXmacs] GPU: ThorVG does not start" << LF; return false; }
  tvg= tvg::GlCanvas::gen ();
  tvg_small= tvg::GlCanvas::gen ();
  if (tvg == NULL || tvg_small == NULL) {
    cout << "TeXmacs] GPU: no GL canvas in ThorVG" << LF; return false; }
  return true;
}

bool
gl_backend::attach (SDL_Window* w) {
  if (failed || w == NULL) return false;
  if (ctx == NULL) {
    ctx= SDL_GL_CreateContext (w);
    if (ctx == NULL) {
      cout << "TeXmacs] GPU: no GL context (" << SDL_GetError ()
           << "), drawing with MuPDF" << LF;
      failed= true;
      return false;
    }
    SDL_GL_MakeCurrent (w, ctx);
#ifndef __EMSCRIPTEN__
    if (!vue_gl_load ()) {
      cout << "TeXmacs] GPU: no OpenGL 3, drawing with MuPDF" << LF;
      SDL_GL_DestroyContext (ctx);
      ctx= NULL;
      failed= true;
      return false;
    }
#endif
    SDL_GL_SetSwapInterval (0); // several windows: each would wait a frame
    if (!init ()) { failed= true; return false; }
    return true;
  }
  return SDL_GL_MakeCurrent (w, ctx);
}

string
gl_backend::description () {
  if (ctx == NULL) return "";
  return string ((const char*) glGetString (GL_RENDERER)) * ", " *
         string ((const char*) glGetString (GL_VERSION));
}

/******************************************************************************
* Textures and targets
******************************************************************************/

static void
formats (gpu_format f, GLenum& ifmt, GLenum& fmt, GLenum& type, int& bpp) {
  switch (f) {
  case GPU_R8: ifmt= GL_R8; fmt= GL_RED; type= GL_UNSIGNED_BYTE; bpp= 1; break;
  case GPU_RGBA32F: ifmt= GL_RGBA32F; fmt= GL_RGBA; type= GL_FLOAT; bpp= 16; break;
  case GPU_R32UI: ifmt= GL_R32UI; fmt= GL_RED_INTEGER; type= GL_UNSIGNED_INT; bpp= 4; break;
  default: ifmt= GL_RGBA8; fmt= GL_RGBA; type= GL_UNSIGNED_BYTE; bpp= 4; break;
  }
}

gpu_texture
gl_backend::new_texture (int w, int h, gpu_format f, bool linear, bool repeat,
                         const void* data) {
  if (ctx == NULL || w <= 0 || h <= 0) return NULL;
  GLenum ifmt, fmt, type; int bpp;
  formats (f, ifmt, fmt, type, bpp);
  gpu_texture t= tm_new<gpu_texture_rep> ();
  t->fbo= 0; t->w= w; t->h= h; t->f= f;
  glGenTextures (1, &t->tex);
  glBindTexture (GL_TEXTURE_2D, t->tex);
  glPixelStorei (GL_UNPACK_ALIGNMENT, bpp == 1 ? 1 : 4);
  glTexImage2D (GL_TEXTURE_2D, 0, ifmt, w, h, 0, fmt, type, data);
  GLint filter= linear ? GL_LINEAR : GL_NEAREST;
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filter);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filter);
  GLint wrap= repeat ? GL_REPEAT : GL_CLAMP_TO_EDGE;
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrap);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrap);
  return t;
}

void
gl_backend::update_texture (gpu_texture t, int x, int y, int w, int h,
                            const void* data) {
  if (t == NULL || w <= 0 || h <= 0) return;
  GLenum ifmt, fmt, type; int bpp;
  formats (t->f, ifmt, fmt, type, bpp);
  glBindTexture (GL_TEXTURE_2D, t->tex);
  glPixelStorei (GL_UNPACK_ALIGNMENT, bpp == 1 ? 1 : 4);
  glTexSubImage2D (GL_TEXTURE_2D, 0, x, y, w, h, fmt, type, data);
}

void
gl_backend::delete_texture (gpu_texture t) {
  if (t == NULL) return;
  if (t->fbo != 0) glDeleteFramebuffers (1, &t->fbo);
  if (t->tex != 0) glDeleteTextures (1, &t->tex);
  tm_delete (t);
}

bool
gl_backend::make_target (gpu_texture_rep& t, int w, int h) {
  glGenTextures (1, &t.tex);
  glBindTexture (GL_TEXTURE_2D, t.tex);
  glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA8, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glGenFramebuffers (1, &t.fbo);
  glBindFramebuffer (GL_FRAMEBUFFER, t.fbo);
  glFramebufferTexture2D (GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, t.tex, 0);
  bool ok= glCheckFramebufferStatus (GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE;
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  t.w= w; t.h= h; t.f= GPU_RGBA8;
  return ok;
}

void
gl_backend::free_target (gpu_texture_rep& t) {
  if (t.fbo != 0) glDeleteFramebuffers (1, &t.fbo);
  if (t.tex != 0) glDeleteTextures (1, &t.tex);
  t.fbo= t.tex= 0;
}

gpu_texture
gl_backend::new_target (int w, int h, bool transparent) {
  if (ctx == NULL || w <= 0 || h <= 0) return NULL;
  gpu_texture t= tm_new<gpu_texture_rep> ();
  if (!make_target (*t, w, h)) { free_target (*t); tm_delete (t); return NULL; }
  // opaque white, as native_opaque_picture, or transparent
  glBindFramebuffer (GL_FRAMEBUFFER, t->fbo);
  glDisable (GL_SCISSOR_TEST);
  if (transparent) glClearColor (0, 0, 0, 0);
  else glClearColor (1, 1, 1, 1);
  glClear (GL_COLOR_BUFFER_BIT);
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  return t;
}

/******************************************************************************
* Drawing
******************************************************************************/

void
gl_backend::bind_target (const gpu_pass& p) {
  glBindFramebuffer (GL_FRAMEBUFFER, p.target == NULL ? 0 : p.target->fbo);
  // (the shaders map w x h to the viewport: a scaled window is scaled here)
  glViewport (0, 0, p.dw, p.dh);
}

void
gl_backend::set_scissor (const gpu_pass& p) {
  int x1= p.sx1, y1= p.sy1, x2= p.sx2, y2= p.sy2;
  glEnable (GL_SCISSOR_TEST);
  if (p.dw != p.w || p.dh != p.h) {
    // a scaled window: the pixels which the box touches
    double fx= (double) p.dw / p.w, fy= (double) p.dh / p.h;
    int a1= (int) floor (x1 * fx), a2= (int) ceil (x2 * fx);
    int b1= (int) floor ((p.h - y2) * fy), b2= (int) ceil ((p.h - y1) * fy);
    glScissor (a1, b1, a2 - a1, b2 - b1);
    return;
  }
  glScissor (x1, p.h - y2, x2 - x1, y2 - y1);
}

void
gl_backend::draw_quads (const gpu_pass& p, const gpu_quads& q) {
  if (ctx == NULL || q.nr_verts <= 0) return;
  bind_target (p);
  set_scissor (p);
  glDisable (GL_DEPTH_TEST);
  glDisable (GL_STENCIL_TEST);
  glDisable (GL_CULL_FACE);
  glColorMask (GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glEnable (GL_BLEND);
  glBlendEquation (GL_FUNC_ADD);
  glBlendFunc (GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram (prog);
  glUniform2f (u_size, (float) p.w, (float) p.h);
  glUniform1i (u_mode, q.mode);
  glUniform2f (u_porig, q.porig_x, q.porig_y);
  glUniform2f (u_ptile, q.ptile_w, q.ptile_h);
  glActiveTexture (GL_TEXTURE0);
  glBindTexture (GL_TEXTURE_2D, (q.mode != 2 && q.tex != NULL) ? q.tex->tex : blank);
  glUniform1i (u_tex, 0);
  glActiveTexture (GL_TEXTURE1);
  glBindTexture (GL_TEXTURE_2D, q.atlas != NULL ? q.atlas->tex : blank);
  glUniform1i (u_atlas, 1);
  glActiveTexture (GL_TEXTURE0);
  glBindVertexArray (vao);
  glBindBuffer (GL_ARRAY_BUFFER, vbo);
  glBufferData (GL_ARRAY_BUFFER, q.nr_verts * 8 * sizeof (float), q.verts, GL_STREAM_DRAW);
  glEnableVertexAttribArray (0);
  glEnableVertexAttribArray (1);
  glEnableVertexAttribArray (2);
  glVertexAttribPointer (0, 2, GL_FLOAT, GL_FALSE, 32, (void*) 0);
  glVertexAttribPointer (1, 2, GL_FLOAT, GL_FALSE, 32, (void*) 8);
  glVertexAttribPointer (2, 4, GL_FLOAT, GL_FALSE, 32, (void*) 16);
  glDrawArrays (GL_TRIANGLES, 0, (GLsizei) q.nr_verts);
  glBindVertexArray (0);
}

void
gl_backend::draw_slug (const gpu_pass& p, gpu_texture curves, gpu_texture bands,
                       const float* inst, int n, float bold) {
  if (ctx == NULL || slug_prog == 0 || n <= 0 || curves == NULL || bands == NULL)
    return;
  bind_target (p);
  set_scissor (p);
  glDisable (GL_DEPTH_TEST);
  glDisable (GL_STENCIL_TEST);
  glDisable (GL_CULL_FACE);
  glEnable (GL_BLEND);
  glBlendEquation (GL_FUNC_ADD);
  glBlendFunc (GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram (slug_prog);
  glUniform2f (su_size, (float) p.w, (float) p.h);
  glUniform1f (su_bold, bold);
  glActiveTexture (GL_TEXTURE0);
  glBindTexture (GL_TEXTURE_2D, curves->tex);
  glUniform1i (su_curves, 0);
  glActiveTexture (GL_TEXTURE1);
  glBindTexture (GL_TEXTURE_2D, bands->tex);
  glUniform1i (su_bands, 1);
  glActiveTexture (GL_TEXTURE0);
  glBindVertexArray (slug_vao);
  glBindBuffer (GL_ARRAY_BUFFER, slug_vbo);
  glBufferData (GL_ARRAY_BUFFER, n * 13 * 4, inst, GL_STREAM_DRAW);
  const int st= 13 * 4;  // x y em, box, offset nb, color
  glEnableVertexAttribArray (0); glVertexAttribPointer (0, 3, GL_FLOAT, GL_FALSE, st, (void*) 0);
  glEnableVertexAttribArray (1); glVertexAttribPointer (1, 4, GL_FLOAT, GL_FALSE, st, (void*) 12);
  glEnableVertexAttribArray (2); glVertexAttribIPointer (2, 2, GL_UNSIGNED_INT, st, (void*) 28);
  glEnableVertexAttribArray (3); glVertexAttribPointer (3, 4, GL_FLOAT, GL_FALSE, st, (void*) 36);
  for (int i= 0; i < 4; i++) glVertexAttribDivisor (i, 1);
  glDrawArraysInstanced (GL_TRIANGLES, 0, 6, (GLsizei) n);
  for (int i= 0; i < 4; i++) glVertexAttribDivisor (i, 0);
  glBindVertexArray (0);
}

/******************************************************************************
* Vector shapes (ThorVG)
******************************************************************************/

bool
gl_backend::ensure_scratch (int w, int h) {
  if (scratch.tex != 0 && scratch.w >= w && scratch.h >= h) return true;
  int nw= max (w, scratch.w), nh= max (h, scratch.h);
  if (scratch.tex != 0) free_target (scratch);
  if (!make_target (scratch, nw, nh)) { free_target (scratch); return false; }
  tvg::Result r= tvg->target (NULL, NULL, (void*) ctx, (int32_t) scratch.fbo,
                              (uint32_t) nw, (uint32_t) nh, tvg::ColorSpace::ABGR8888S);
  if (r != tvg::Result::Success) {
    cout << "TeXmacs] GPU: ThorVG cannot draw into its target (" << (int) r << ")" << LF;
    return false;
  }
  return true;
}

bool
gl_backend::ensure_small () {
  if (small.tex != 0) return true;
  if (!make_target (small, SMALL, SMALL)) { free_target (small); return false; }
  return tvg_small->target (NULL, NULL, (void*) ctx, (int32_t) small.fbo,
                            SMALL, SMALL, tvg::ColorSpace::ABGR8888S)
         == tvg::Result::Success;
}

tvg::Shape*
gl_backend::as_tvg (const gpu_shape& s) {
  tvg::Shape* p= tvg::Shape::gen ();
  size_t k= 0;
  for (unsigned char c: s.cmds)
    switch (c) {
    case 0: p->moveTo (s.pts[k], s.pts[k+1]); k += 2; break;
    case 1: p->lineTo (s.pts[k], s.pts[k+1]); k += 2; break;
    case 2: p->cubicTo (s.pts[k], s.pts[k+1], s.pts[k+2], s.pts[k+3],
                        s.pts[k+4], s.pts[k+5]); k += 6; break;
    default: p->close (); break;
    }
  if (s.fill) {
    p->fill (s.fill_rgba[0], s.fill_rgba[1], s.fill_rgba[2], s.fill_rgba[3]);
    p->fillRule (s.even_odd ? tvg::FillRule::EvenOdd : tvg::FillRule::NonZero);
  }
  if (s.stroke) {
    p->strokeWidth (s.width);
    p->strokeFill (s.stroke_rgba[0], s.stroke_rgba[1], s.stroke_rgba[2], s.stroke_rgba[3]);
    p->strokeCap (s.cap == 1 ? tvg::StrokeCap::Round :
                  s.cap == 2 ? tvg::StrokeCap::Square : tvg::StrokeCap::Butt);
    p->strokeJoin (tvg::StrokeJoin::Round);
  }
  return p;
}

void
gl_backend::draw_shapes (const gpu_pass& p, const std::vector<gpu_shape>& shapes,
                         int x1, int y1, int x2, int y2) {
  if (ctx == NULL || shapes.empty () || x1 >= x2 || y1 >= y2) return;
  bool sm= (x2 - x1 <= SMALL && y2 - y1 <= SMALL);
  if (sm ? !ensure_small () : !ensure_scratch (p.w, p.h)) return;
  // the canvas draws all of its target (its viewport does not limit the
  // copy which ends the pass); only the box is composed below
  tvg::GlCanvas* c= sm ? tvg_small : tvg;
  gpu_texture_rep& sc= sm ? small : scratch;
  int ox= sm ? x1 : 0, oy= sm ? y1 : 0;
  if (sm) {
    tvg::Scene* scene= tvg::Scene::gen ();
    for (const gpu_shape& s: shapes) scene->add (as_tvg (s));
    scene->translate ((float) -ox, (float) -oy);
    c->add (scene);
  }
  else for (const gpu_shape& s: shapes) c->add (as_tvg (s));
  c->draw (true);
  c->sync ();
  c->remove ();
  // compose what ThorVG drew over the target (premultiplied; the texture
  // of a target has its first row at the bottom)
  float sw= (float) sc.w, sh= (float) sc.h;
  float u0= (x1 - ox) / sw, u1= (x2 - ox) / sw;
  float v0= 1.f - (y1 - oy) / sh, v1= 1.f - (y2 - oy) / sh;
  float X1= (float) x1, X2= (float) x2, Y1= (float) y1, Y2= (float) y2;
  float v[6][8]= { { X1, Y1, u0, v0, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X1, Y2, u0, v1, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X2, Y2, u1, v1, 1, 1, 1, 1 }, { X1, Y2, u0, v1, 1, 1, 1, 1 } };
  gpu_quads q= { 1, &sc, NULL, 0, 0, 1, 1, &v[0][0], 6 };
  draw_quads (p, q);
}

/******************************************************************************
* Copies and reading
******************************************************************************/

void
gl_backend::copy (gpu_texture src, int sw, int sh, int x1, int y1, int x2, int y2,
                  gpu_texture dst, int dw, int dh,
                  int dx1, int dy1, int dx2, int dy2) {
  (void) sw; // (the rows of GL go up)
  if (ctx == NULL || x2 <= x1 || y2 <= y1 || dx2 <= dx1 || dy2 <= dy1) return;
  bool same= (x2 - x1 == dx2 - dx1) && (y2 - y1 == dy2 - dy1);
  glBindFramebuffer (GL_READ_FRAMEBUFFER, src == NULL ? 0 : src->fbo);
  glBindFramebuffer (GL_DRAW_FRAMEBUFFER, dst == NULL ? 0 : dst->fbo);
  glDisable (GL_SCISSOR_TEST);
  glBlitFramebuffer (x1, sh - y2, x2, sh - y1,
                     dx1, dh - dy2, dx2, dh - dy1,
                     GL_COLOR_BUFFER_BIT, same ? GL_NEAREST : GL_LINEAR);
  glBindFramebuffer (GL_READ_FRAMEBUFFER, 0);
  glBindFramebuffer (GL_DRAW_FRAMEBUFFER, 0);
  (void) dw;
}

bool
gl_backend::read (gpu_texture src, int sw, int sh, int x, int y, int w, int h,
                  unsigned char* rgba) {
  (void) sw;
  if (ctx == NULL || w <= 0 || h <= 0) return false;
  std::vector<unsigned char> rows ((size_t) w * h * 4);
  glBindFramebuffer (GL_FRAMEBUFFER, src == NULL ? 0 : src->fbo);
  glPixelStorei (GL_PACK_ALIGNMENT, 4);
  glReadPixels (x, sh - (y + h), w, h, GL_RGBA, GL_UNSIGNED_BYTE, rows.data ());
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  for (int r= 0; r < h; r++)
    memcpy (rgba + (size_t) r * w * 4, rows.data () + (size_t) (h - 1 - r) * w * 4,
            (size_t) w * 4);
  return true;
}

gpu_backend*
gpu_gl_backend () {
  static gl_backend* b= NULL;
  if (b == NULL) b= tm_new<gl_backend> ();
  return b;
}

#endif // USE_THORVG
