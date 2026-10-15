/******************************************************************************
* MODULE     : vue_gpu.cpp
* DESCRIPTION: The GPU renderer of the Vue port: a glyph atlas, batches of
*              quads, textures and vector shapes, drawn by a backend
*              (OpenGL and WebGL2, or Metal: gpu_backend.hpp)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// What the GPU draws, and how (see docs/vue-graphics-stack.md, "The GPU
// renderer", and misc/thorvg-bench for the measurements behind it):
//
// * the windows are drawn by the backend (gpu_backend_gl.cpp: one GL
//   context for all of them; gpu_backend_metal.mm: one Metal device), so
//   that the textures, and the targets of the editors, belong to all of
//   them;
// * the backing store of an editor is a target texture (gpu_picture_rep),
//   which the editor repaints incrementally as it did its MuPDF pixmap,
//   and which the window draws as a textured quad; a scroll copies it to a
//   second texture, shifted, and the two are swapped;
// * glyphs come from an atlas (one R8 texture) of the glyph bitmaps of
//   TeXmacs (shrink, as the X11 and the Qt ports draw them), plain fills
//   from a white corner of the same atlas, so that text and fills go in
//   one draw call; the pictures (icons, images) are uploaded once, as
//   textures, and drawn as quads;
// * lines, polygons, arcs and rounded rectangles are vector shapes
//   (gpu_shape), which the backend draws over a box of the target (ThorVG
//   in the GL backend, Core Graphics in the Metal one). Consecutive
//   vector operations within one clip are one pass.
//
// This file does not depend on the API: device coordinates are pixels from
// the top left of the target, y down, and the backend says whether the
// texture of a target has its first row at the bottom (GL) or at the top.

#include "vue_gpu.hpp"
#include "gpu_backend.hpp"
#include "sys_utils.hpp"

#if !defined (USE_THORVG) && !defined (USE_METAL)

bool vue_gpu_enabled () { return false; }
void vue_gpu_prepare () {}
unsigned long long vue_gpu_window_flags () { return 0; }
bool vue_gpu_follow_glyph_settings () { return false; }
bool vue_gpu_attach (SDL_Window* w) { (void) w; return false; }
bool vue_gpu_ready (SDL_Window* w) { (void) w; return false; }
void vue_gpu_present (SDL_Window* w) { (void) w; }
picture  gpu_backing_picture (int w, int h, bool t) { (void) w; (void) h; (void) t; return picture (); }
bool     is_gpu_picture (picture p) { (void) p; return false; }
renderer gpu_picture_renderer (picture p, double z) { (void) p; (void) z; return NULL; }
void     gpu_translate_picture (picture p, int x, int y) { (void) p; (void) x; (void) y; }
picture  gpu_copy_picture (picture p) { return p; }
renderer gpu_screen_renderer (double z) { (void) z; return NULL; }
void     gpu_begin_screen (renderer r, int w, int h, int dw, int dh) {
  (void) r; (void) w; (void) h; (void) dw; (void) dh; }
void     gpu_flush () {}
unsigned long long gpu_frame_hash () { return 0; }
void     gpu_finish () {}
picture  gpu_read_screen (int w, int h) { (void) w; (void) h; return picture (); }
bool gpu_draw_picture_scaled (renderer r, picture p, SI x, SI y, double s, int a) {
  (void) r; (void) p; (void) x; (void) y; (void) s; (void) a; return false; }
bool is_gpu_renderer (renderer r) { (void) r; return false; }
bool gpu_layers_enabled () { return false; }
bool gpu_show_picture (void* l, picture p, int dw, int dh) {
  (void) l; (void) p; (void) dw; (void) dh; return false; }
bool gpu_read_picture (picture p, unsigned char* rgba) {
  (void) p; (void) rgba; return false; }

#else

#include "basic_renderer.hpp"
#include "frame.hpp"
#include "brush.hpp"
#include "image_files.hpp"
#include "file.hpp"
#ifdef USE_MUPDF
#include "../MuPDF/mupdf_picture.hpp"
#endif
#include "Freetype/tt_face.hpp"
#include "boot.hpp"
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <utility>
#include <cmath>

/******************************************************************************
* Targets: a texture of the backend, or the window being drawn
******************************************************************************/

struct gpu_target {
  int w, h;
  gpu_texture tex;      // NULL for the window
  gpu_texture tex2;     // the other one of a scrolled backing store
  bool screen;
  bool made;            // the texture exists (made when first drawn)
  // the window may be drawn scaled (the interface scaling, vue_scaling in
  // vue_gui.cpp): w x h are then the pixels of its layout, dw x dh those
  // of its framebuffer. Equal to w x h otherwise, and for every texture
  int dw, dh;
  int dev_w () const { return (screen && dw > 0) ? dw : w; }
  int dev_h () const { return (screen && dh > 0) ? dh : h; }
  unsigned long long gen; // changes with what the texture shows
  bool transparent;     // made transparent (the canvases of Cocoa), not white
};

static void gpu_flush_all ();
static bool read_glyph_settings ();

/******************************************************************************
* The state of the GPU (one backend)
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
  unsigned nb;               // bands in each direction (0: no ink), and
                             // the direction of the outline (high bit)
  float em;                  // pixels a em
  bool outline;              // false: the font has no file, a bitmap then
};

struct cached_texture {
  gpu_texture tex;
  int w, h;
  picture keep;         // the picture stays (its unique id is the key)
  unsigned long long last;
  size_t bytes;
};

static const int ATLAS= 2048;
static const int SLUG_W= 1024;   // the width of the two textures of Slug

struct gpu_state {
  gpu_backend* B= NULL;         // the backend, chosen once (vue_gpu_enabled)
  bool failed= false;
  bool started= false;          // the atlas exists: the backend is attached
  // the batch being made: quads of one target, one texture, one clip
  std::vector<float> verts;     // x y u v r g b a (premultiplied)
  gpu_target* target= NULL;
  int mode= 2;                  // 1: texture, 2: atlas, 3: atlas * pattern
  float porig_x= 0, porig_y= 0, ptile_w= 1, ptile_h= 1; // the pattern (3)
  gpu_texture tex= NULL;
  int sx1= 0, sy1= 0, sx2= 0, sy2= 0; // the clip (device pixels)
  // the atlas of the glyphs, with a white corner for the fills
  gpu_texture atlas= NULL;
  int ax= 4, ay= 0, arow= 4;
  std::unordered_map<glyph_key, glyph_slot, glyph_key_hash> glyphs;
  std::unordered_map<glyph_key, slug_glyph, glyph_key_hash> slugs;
  std::vector<font_glyphs> fonts;  // kept alive: their address is the key
  // the vector shapes of the pass being made: one target, one clip
  std::vector<gpu_shape> shapes;
  bool pending= false;
  gpu_target* vtarget= NULL;
  int vx1= 0, vy1= 0, vx2= 0, vy2= 0;          // what the pass covers
  int vsx1= 0, vsy1= 0, vsx2= 0, vsy2= 0;      // its clip
  // the textures of the pictures, by their unique id; of the patterns
  std::unordered_map<unsigned long long, cached_texture> textures;
  std::unordered_map<std::string, cached_texture> patterns;
  unsigned long long tick= 0;
  size_t texture_bytes= 0;
  // the window being drawn
  gpu_target screen= { 0, 0, NULL, NULL, true, true, 0, 0, 0 };
  // the source of the glyphs of the atlas (see read_glyph_settings): 0 the
  // bitmaps of TeXmacs, 1 MuPDF, 2 FreeType in shades of grey, hinted as
  // ft_flags say and made ft_bold pixels bolder
  int glyph_source= 2;
  int ft_flags= 0;
  double ft_bold= 0.22;
  // glyphs drawn from their outlines (Slug, TEXMACS_VUE_SLUG=1)
  bool slug_on= false;
  gpu_texture curve_tex= NULL, index_tex= NULL;
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

#ifndef USE_METAL
gpu_backend* gpu_metal_backend () { return NULL; }
#endif

// The backend: TEXMACS_VUE_GPU=0 draws with MuPDF, =gl or =metal chooses
// the API; by default Metal on macOS when it is compiled (USE_METAL), else
// OpenGL (USE_THORVG). Decided once.
static gpu_backend*
choose_backend () {
  string v= get_env ("TEXMACS_VUE_GPU");
  if (v == "0") return NULL;
  gpu_backend* gl= gpu_gl_backend ();
  gpu_backend* mtl= gpu_metal_backend ();
  if (v == "gl" || v == "opengl") return gl;
  if (v == "metal") return mtl;
  return mtl != NULL ? mtl : gl;
}

bool
vue_gpu_enabled () {
  static bool chosen= false;
  if (!chosen) { chosen= true; G.B= choose_backend (); }
  return G.B != NULL && !G.failed;
}

void
vue_gpu_prepare () {
  static bool done= false;
  if (done || !vue_gpu_enabled ()) return;
  done= true;
  if (!G.B->prepare ()) G.failed= true;
}

unsigned long long
vue_gpu_window_flags () {
  return vue_gpu_enabled () ? G.B->window_flags () : 0;
}

// what the renderer keeps on the backend, once it can draw
static void
start_gpu () {
  // the atlas, with a white 4x4 corner (the fills)
  std::vector<unsigned char> zero (ATLAS * ATLAS, 0);
  for (int y= 0; y < 4; y++) for (int x= 0; x < 4; x++) zero[y * ATLAS + x]= 255;
  G.atlas= G.B->new_texture (ATLAS, ATLAS, GPU_R8, true, false, zero.data ());
  read_glyph_settings ();
#ifdef USE_MUPDF
  G.slug_on= (get_env ("TEXMACS_VUE_SLUG") == "1");
#endif
  if (G.slug_on) cout << "TeXmacs] GPU: glyphs drawn from their outlines (Slug)" << LF;
  G.started= true;
  cout << "TeXmacs] GPU: " << G.B->description () << LF;
}

bool
vue_gpu_attach (SDL_Window* w) {
  if (!vue_gpu_enabled () || w == NULL) return false;
  if (!G.B->attach (w)) {
    if (!G.started) G.failed= true;
    return false;
  }
  if (!G.started) start_gpu ();
  return true;
}

bool
vue_gpu_ready (SDL_Window* w) {
  return vue_gpu_attach (w) && G.B->ready_to_present ();
}

void
vue_gpu_present (SDL_Window* w) {
  gpu_flush_all ();
  G.B->present (w);
}

static bool
ensure_target (gpu_target* t) {
  if (t->made || t->screen) return true;
  if (!G.started || t->w <= 0 || t->h <= 0) return false;
  t->tex= G.B->new_target (t->w, t->h, t->transparent);
  if (t->tex == NULL) return false;
  t->made= true;
  return true;
}

/******************************************************************************
* The glyphs of the atlas, as the preferences say: "gpu glyphs" (freetype,
* texmacs or mupdf), "gpu glyph boldness" (in pixels, for FreeType) and
* "gpu glyph hinting" (light, none or full, for FreeType), which the
* variables TEXMACS_VUE_GLYPHS, TEXMACS_VUE_FT_BOLD and TEXMACS_VUE_FT_HINT
* override (tests). A change empties the atlas: the loop looks at them at
* each iteration (vue_gpu_follow_glyph_settings), and repaints everything.
******************************************************************************/

static string
glyph_setting (const char* env, string pref, string def) {
  string v= get_env (env);
  return (N(v) > 0)? v: get_user_preference (pref, def);
}

// true when they changed
static bool
read_glyph_settings () {
  string src = glyph_setting ("TEXMACS_VUE_GLYPHS", "gpu glyphs", "freetype");
  string hint= glyph_setting ("TEXMACS_VUE_FT_HINT", "gpu glyph hinting", "light");
  string bold= glyph_setting ("TEXMACS_VUE_FT_BOLD", "gpu glyph boldness", "0.22");
  int    source= (src == "mupdf")? 1: (src == "texmacs")? 0: 2;
  int    flags = (hint == "none")? FT_LOAD_NO_HINTING:
                 (hint == "full")? FT_LOAD_DEFAULT: FT_LOAD_TARGET_LIGHT;
  double b= is_double (bold)? as_double (bold): 0.22;
  b= max (0.0, min (b, 2.0));
  if (source == G.glyph_source && flags == G.ft_flags && b == G.ft_bold)
    return false;
  G.glyph_source= source; G.ft_flags= flags; G.ft_bold= b;
  return true;
}

static void
clear_atlas () {
  // what is queued uses the old content
  gpu_flush_all ();
  G.glyphs.clear ();
  G.fonts.clear ();
  G.ax= 4; G.ay= 0; G.arow= 4;
}

bool
vue_gpu_follow_glyph_settings () {
  if (!G.started) return false; // no GPU, or not started
  if (!read_glyph_settings ()) return false;
  clear_atlas ();
  return true;
}

/******************************************************************************
* Batches of quads
******************************************************************************/

// a pass of the backend into the target, with a clip (device pixels)
static gpu_pass
make_pass (gpu_target* t, int x1, int y1, int x2, int y2) {
  x1= max (x1, 0); y1= max (y1, 0); x2= min (x2, t->w); y2= min (y2, t->h);
  if (x2 < x1) x2= x1;
  if (y2 < y1) y2= y1;
  gpu_pass p= { t->screen ? NULL : t->tex, t->w, t->h, t->dev_w (), t->dev_h (),
                x1, y1, x2, y2 };
  return p;
}

static void flush_slug ();

static void
flush_quads () {
  if (G.mode == 4) { flush_slug (); return; }
  if (G.verts.empty () || G.target == NULL) { G.verts.clear (); return; }
  gpu_target* t= G.target;
  gpu_pass p= make_pass (t, G.sx1, G.sy1, G.sx2, G.sy2);
  gpu_quads q= { G.mode, G.tex, G.atlas, G.porig_x, G.porig_y, G.ptile_w, G.ptile_h,
                 G.verts.data (), (int) (G.verts.size () / 8) };
  G.B->draw_quads (p, q);
  G.verts.clear ();
  t->gen++;
}

// the rows of the textures of Slug which are not there yet; a texture
// which is full is made twice as tall and sent again
static void
send_rows (gpu_texture& tex, int& rows, size_t& sent, size_t total,
           size_t per_texel, gpu_format fmt, const void* data) {
  size_t texels= total / per_texel;
  int need= (int) ((texels + SLUG_W - 1) / SLUG_W);
  if (need == 0) return;
  if (need > rows || tex == NULL) {
    int nr= max (need, max (16, rows * 2));
    std::vector<unsigned char> zero ((size_t) nr * SLUG_W * per_texel * 4, 0);
    memcpy (zero.data (), data, total * 4);
    if (tex != NULL) G.B->delete_texture (tex);
    tex= G.B->new_texture (SLUG_W, nr, fmt, false, false, zero.data ());
    rows= nr; sent= total;
    return;
  }
  if (sent >= total) return;
  // from the row of the first texel not sent, whole rows
  int r0= (int) ((sent / per_texel) / SLUG_W);
  std::vector<unsigned char> buf ((size_t) (need - r0) * SLUG_W * per_texel * 4, 0);
  size_t from= (size_t) r0 * SLUG_W * per_texel;
  memcpy (buf.data (), (const char*) data + from * 4, (total - from) * 4);
  G.B->update_texture (tex, 0, r0, SLUG_W, need - r0, buf.data ());
  sent= total;
}

static void
flush_slug () {
  gpu_target* t= G.target;
  if (G.slug_inst.empty () || t == NULL) { G.slug_inst.clear (); return; }
  send_rows (G.curve_tex, G.curve_rows, G.curves_sent, G.curves.size (), 4,
             GPU_RGBA32F, G.curves.data ());
  send_rows (G.index_tex, G.index_rows, G.bands_sent, G.bands.size (), 1,
             GPU_R32UI, G.bands.data ());
  gpu_pass p= make_pass (t, G.sx1, G.sy1, G.sx2, G.sy2);
  // as bold as the glyphs of the atlas (not those of MuPDF)
  float bold= (G.glyph_source == 1) ? 0.f : (float) G.ft_bold;
  G.B->draw_slug (p, G.curve_tex, G.index_tex, G.slug_inst.data (),
                  (int) (G.slug_inst.size () / 13), bold);
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
#ifdef USE_MUPDF
  bool ok= mupdf_glyph_outline (fng->res_name, c, q, em);
#else
  bool ok= false; // (the outlines come from MuPDF)
#endif
  if (!ok || em <= 0) {
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
    // the direction of the outline (the sign of its area, its curves taken
    // as the polygons of their control points), with which the shader moves
    // the edges outwards to make the glyph bolder: the high bit of nb
    double area= 0;
    for (int i= 0; i < n; i++)
      for (int j= 0; j < 2; j++)
        area += q[6*i+2*j] * q[6*i+2*j+3] - q[6*i+2*j+2] * q[6*i+2*j+1];
    g.nb= nb | (area < 0 ? 0x80000000u : 0u);
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
batch (gpu_target* t, int mode, gpu_texture tex, int x1, int y1, int x2, int y2,
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
      long long st[6]= { mode, (long long) (size_t) tex, x1, y1, x2, y2 };
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
  if (G.started) G.B->flush ();
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
  if (G.started) { if (sync) G.B->finish (); else G.B->flush (); }
}

/******************************************************************************
* Passes of vector shapes
******************************************************************************/

static void
flush_vectors () {
  if (!G.pending) return;
  G.pending= false;
  gpu_target* t= G.vtarget;
  int x1= max (G.vx1, G.vsx1), y1= max (G.vy1, G.vsy1);
  int x2= min (G.vx2, G.vsx2), y2= min (G.vy2, G.vsy2);
  x1= max (x1, 0); y1= max (y1, 0); x2= min (x2, t->w); y2= min (y2, t->h);
  std::vector<gpu_shape> shapes;
  shapes.swap (G.shapes);
  if (x1 >= x2 || y1 >= y2) return;
  gpu_pass p= make_pass (t, G.vsx1, G.vsy1, G.vsx2, G.vsy2);
  G.B->draw_shapes (p, shapes, x1, y1, x2, y2);
  t->gen++;
}

// a shape for the target, within the clip, covering a box
static void
add_vector (gpu_target* t, gpu_shape& s, int sx1, int sy1, int sx2, int sy2,
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
    if (!s.cmds.empty ()) feed_bytes (s.cmds.data (), s.cmds.size ());
    if (!s.pts.empty ()) feed_bytes (s.pts.data (), s.pts.size () * sizeof (float));
    unsigned char c[12]= { s.fill_rgba[0], s.fill_rgba[1], s.fill_rgba[2], s.fill_rgba[3],
                           s.stroke_rgba[0], s.stroke_rgba[1], s.stroke_rgba[2],
                           s.stroke_rgba[3], (unsigned char) s.fill,
                           (unsigned char) s.stroke, (unsigned char) s.even_odd,
                           (unsigned char) s.cap };
    feed (c);
    feed (s.width);
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
  G.shapes.push_back (std::move (s));
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
    G.B->delete_texture (oldest->second.tex);
    G.texture_bytes -= oldest->second.bytes;
    G.textures.erase (oldest);
  }
}

// a texture from premultiplied RGBA pixels, rows from the top; over white
// when 'over_white' (the neutral pattern)
static gpu_texture
upload_rgba (int w, int h, std::vector<unsigned char>& rgba, bool repeat,
             bool over_white) {
  if (w <= 0 || h <= 0) return NULL;
  if (over_white)
    for (size_t i= 0; i < (size_t) w * h; i++) {
      unsigned char* d= &rgba[4*i];
      int a= d[3];
      d[0]= (unsigned char) (d[0] + 255 - a); d[1]= (unsigned char) (d[1] + 255 - a);
      d[2]= (unsigned char) (d[2] + 255 - a); d[3]= 255;
    }
  return G.B->new_texture (w, h, GPU_RGBA8, true, repeat, rgba.data ());
}

#ifndef USE_MUPDF
// a texture from any picture (the pixels of TeXmacs have y up, colors not
// premultiplied), when there is no MuPDF (the Cocoa port)
static gpu_texture
upload_picture (picture p, bool repeat, bool over_white= false) {
  if (is_nil (p)) return NULL;
  int w= p->get_width (), h= p->get_height ();
  int ox= p->get_origin_x (), oy= p->get_origin_y ();
  if (w <= 0 || h <= 0) return NULL;
  std::vector<unsigned char> rgba ((size_t) w * h * 4);
  for (int r= 0; r < h; r++)
    for (int x= 0; x < w; x++) {
      int cr, cg, cb, ca;
      get_rgb_color (p->get_pixel (x - ox, (h - 1 - r) - oy), cr, cg, cb, ca);
      unsigned char* d= &rgba[4 * ((size_t) r * w + x)];
      d[0]= (unsigned char) ((cr * ca + 127) / 255);
      d[1]= (unsigned char) ((cg * ca + 127) / 255);
      d[2]= (unsigned char) ((cb * ca + 127) / 255);
      d[3]= (unsigned char) ca;
    }
  return upload_rgba (w, h, rgba, repeat, over_white);
}
#endif

#ifdef USE_MUPDF
// a texture from a MuPDF pixmap (premultiplied RGBA, rows from the top)
static gpu_texture
upload_pixmap (fz_pixmap* pix, bool repeat, bool over_white= false) {
  if (pix == NULL || pix->samples == NULL || pix->w <= 0 || pix->h <= 0) return NULL;
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
      d[0]= (unsigned char) r; d[1]= (unsigned char) g;
      d[2]= (unsigned char) b; d[3]= (unsigned char) a;
    }
  }
  return upload_rgba (w, h, rgba, repeat, over_white);
}
#endif

// the texture of the tiles of a pattern, at a size in device pixels
static gpu_texture
upload_tile (url u, SI w, SI h, tree eff, SI px, bool over_white= false) {
#ifdef USE_MUPDF
  fz_pixmap* pix= mupdf_load_pixmap (u, w, h, eff, px);
  gpu_texture tex= upload_pixmap (pix, true, over_white);
  if (pix != NULL) fz_drop_pixmap (mupdf_context (), pix);
  return tex;
#else
  return upload_picture (load_picture (u, w, h, eff, px), true, over_white);
#endif
}

static cached_texture*
picture_texture (picture p) {
  unsigned long long id= p->get_unique_id ();
  auto it= G.textures.find (id);
  if (it != G.textures.end ()) { it->second.last= ++G.tick; return &it->second; }
#ifdef USE_MUPDF
  picture mp= as_mupdf_picture (p);
  mupdf_picture_rep* r= (mupdf_picture_rep*) mp->get_handle ();
  if (r == NULL || r->pix == NULL) return NULL;
  int w= r->pix->w, h= r->pix->h;
  gpu_texture tex= upload_pixmap (r->pix, false);
#else
  int w= p->get_width (), h= p->get_height ();
  gpu_texture tex= upload_picture (p, false);
#endif
  if (tex == NULL) return NULL;
  size_t bytes= (size_t) w * h * 4;
  G.textures[id]= { tex, w, h, p, ++G.tick, bytes };
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
  gpu_picture_rep (int w, int h, bool transparent= false):
    t { w, h, NULL, NULL, false, false, 0, 0, 0, transparent }, ox (0), oy (0) {}
  ~gpu_picture_rep () {
    if (G.target == &t || G.vtarget == &t) gpu_flush_all ();
    if (G.target == &t) G.target= NULL;
    if (t.tex != NULL) G.B->delete_texture (t.tex);
    if (t.tex2 != NULL) G.B->delete_texture (t.tex2);
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
    G.B->read (t.tex, t.w, t.h, x, y, 1, 1, px);
    return rgb_color (px[0], px[1], px[2], px[3]);
  }
  void internal_set_pixel (int x, int y, color c) {
    (void) x; (void) y; (void) c; }
};

picture
gpu_backing_picture (int w, int h, bool transparent) {
  return picture (tm_new<gpu_picture_rep> (max (w, 1), max (h, 1), transparent));
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

// copy a rectangle of src (pixels of its layout, y down) to dst at (dx,
// dy), one of which may be a scaled window
static void
blit (gpu_target* src, int x1, int y1, int x2, int y2,
      gpu_target* dst, int dx, int dy) {
  int w= x2 - x1, h= y2 - y1;
  if (w <= 0 || h <= 0) return;
  gpu_texture st= src->screen ? NULL : src->tex, dt= dst->screen ? NULL : dst->tex;
  if (src->dev_h () == src->h && src->dev_w () == src->w &&
      dst->dev_h () == dst->h && dst->dev_w () == dst->w) {
    G.B->copy (st, src->w, src->h, x1, y1, x2, y2,
               dt, dst->w, dst->h, dx, dy, dx + w, dy + h);
    return;
  }
  double sfx= (double) src->dev_w () / src->w, sfy= (double) src->dev_h () / src->h;
  double dfx= (double) dst->dev_w () / dst->w, dfy= (double) dst->dev_h () / dst->h;
  G.B->copy (st, src->dev_w (), src->dev_h (),
             (int) floor (x1 * sfx + 0.5), (int) floor (y1 * sfy + 0.5),
             (int) floor (x2 * sfx + 0.5), (int) floor (y2 * sfy + 0.5),
             dt, dst->dev_w (), dst->dev_h (),
             (int) floor (dx * dfx + 0.5), (int) floor (dy * dfy + 0.5),
             (int) floor ((dx + w) * dfx + 0.5), (int) floor ((dy + h) * dfy + 0.5));
}

void
gpu_translate_picture (picture p, int dpx, int dpy) {
  gpu_picture_rep* g= as_gpu (p);
  if (g == NULL || !ensure_target (&g->t)) return;
  gpu_target& t= g->t;
  if (dpx >= t.w || -dpx >= t.w || dpy >= t.h || -dpy >= t.h) return;
  gpu_flush_all ();
  if (t.tex2 == NULL) t.tex2= G.B->new_target (t.w, t.h, t.transparent);
  if (t.tex2 == NULL) return;
  int x0= max (0, dpx), x1= min (t.w, t.w + dpx);
  int y0= max (0, dpy), y1= min (t.h, t.h + dpy);
  G.B->copy (t.tex, t.w, t.h, x0 - dpx, y0 - dpy, x1 - dpx, y1 - dpy,
             t.tex2, t.w, t.h, x0, y0, x1, y1);
  // the strips which were not copied are repainted by the caller
  std::swap (t.tex, t.tex2);
  t.gen++;
}

picture
gpu_copy_picture (picture p) {
  gpu_picture_rep* g= as_gpu (p);
  if (g == NULL || !ensure_target (&g->t)) return picture ();
  gpu_flush_all ();
  gpu_picture_rep* c= tm_new<gpu_picture_rep> (g->t.w, g->t.h, g->t.transparent);
  picture cp (c);
  if (!ensure_target (&c->t)) return picture ();
  G.B->copy (g->t.tex, g->t.w, g->t.h, 0, 0, g->t.w, g->t.h,
             c->t.tex, c->t.w, c->t.h, 0, 0, c->t.w, c->t.h);
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
  void user_quad (double x1, double y1, double x2, double y2, int mode, gpu_texture tex,
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
    user_quad (x1, y1, x2, y2, 2, NULL, w * 0.5f, w * 0.5f, w * 1.5f, w * 1.5f,
               r / 255.f * fa, g / 255.f * fa, b / 255.f * fa, fa); }
  void pattern_fill (SI x1, SI y1, SI x2, SI y2, brush br);
  void vector (gpu_shape& s, float bx1, float by1, float bx2, float by2) {
    int s1, s2, s3, s4;
    clip_box (s1, s2, s3, s4);
    add_vector (t, s, s1, s2, s3, s4, bx1, by1, bx2, by2); }
  void stroke_style (gpu_shape& s, bool closed= false);
  void fill_style (gpu_shape& s, color c) {
    int r, g, b, a; get_rgb_color (c, r, g, b, a);
    s.fill= true;
    s.fill_rgba[0]= (unsigned char) r; s.fill_rgba[1]= (unsigned char) g;
    s.fill_rgba[2]= (unsigned char) b; s.fill_rgba[3]= (unsigned char) a; }

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
  gpu_texture tex= upload_tile (u, w, h, eff, px);
  if (tex == NULL) return NULL;
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
    gpu_texture tex= upload_tile (pu, w, h, eff, pixel, true);
    if (tex == NULL) { solid (ux1, uy1, ux2, uy2, white); return; }
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
      batch (t, 4, NULL, s1, s2, s3, s4);
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
    // the glyph of a FreeType font rendered at its size in shades of grey,
    // made as bold as the bitmap of TeXmacs (rendered at std_shrinkf times
    // its size in black and white and shrunk, which thickens its strokes),
    // as the Qt port draws it, at the cost of one rasterization; or that
    // bitmap itself (TEXMACS_VUE_GLYPHS=texmacs, and the fonts with no
    // file); or the glyph rendered by MuPDF, as the MuPDF renderer draws it
    // (TEXMACS_VUE_GLYPHS=mupdf: lighter)
    string mcov;
    int mw= 0, mh= 0, mx= 0, my= 0;
    bool from_mupdf=
#ifdef USE_MUPDF
      (G.glyph_source == 1 &&
       mupdf_glyph_bitmap (fng->res_name, c, mcov, mw, mh, mx, my)) ||
#endif
      (G.glyph_source == 2 &&
       tt_glyph_coverage (fng, c, std_shrinkf, G.ft_flags, G.ft_bold,
                          mcov, mw, mh, mx, my));
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
    if (G.ay + h + 1 > ATLAS) clear_atlas (); // full: start again
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
      G.B->update_texture (G.atlas, G.ax, G.ay, w, h, cov.data ());
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
  user_quad (left, bottom, left + s.w, bottom + s.h, 2, NULL,
             s.x / A, s.y / A, (s.x + s.w) / A, (s.y + s.h) / A,
             r / 255.f * fa, g / 255.f * fa, b / 255.f * fa, fa);
}

void
gpu_renderer_rep::stroke_style (gpu_shape& s, bool closed) {
  float w= (float) lw / pixel;
  if (w < 1.f) w= 1.f;
  s.stroke= true;
  s.width= w;
  int r, g, b, a; get_rgb_color (fg, r, g, b, a);
  s.stroke_rgba[0]= (unsigned char) r; s.stroke_rgba[1]= (unsigned char) g;
  s.stroke_rgba[2]= (unsigned char) b; s.stroke_rgba[3]= (unsigned char) a;
  if (closed || pen->get_cap () == cap_round) s.cap= 1;
  else if (pen->get_cap () == cap_flat) s.cap= 0;
  else s.cap= 2;
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
  gpu_shape s;
  s.move_to (ax, ay);
  s.line_to (bx, by);
  stroke_style (s);
  vector (s, min (ax, bx) - w, min (ay, by) - w, max (ax, bx) + w, max (ay, by) + w);
}

void
gpu_renderer_rep::lines (array<SI> x, array<SI> y) {
  int n= N(x);
  if (!ready () || N(y) != n || n < 1) return;
  gpu_shape s;
  float bx1= 1e9, by1= 1e9, bx2= -1e9, by2= -1e9;
  for (int i= 0; i < n; i++) {
    float px, py;
    map (x[i], y[i], px, py);
    if (i == 0) s.move_to (px, py); else s.line_to (px, py);
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
  gpu_shape s;
  float bx1= 1e9, by1= 1e9, bx2= -1e9, by2= -1e9;
  for (int i= 0; i < n; i++) {
    float px, py;
    map (x[i], y[i], px, py);
    if (i == 0) s.move_to (px, py); else s.line_to (px, py);
    bx1= min (bx1, px); by1= min (by1, py); bx2= max (bx2, px); by2= max (by2, py);
  }
  s.close ();
  // as the MuPDF renderer: nonzero winding for convex polygons, even-odd
  // for the others
  s.even_odd= !convex;
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
  gpu_shape s;
  auto P= [&] (double bx, double by, double sp, double cp, float& dx, float& dy) {
    device (cx + rx * (bx*cp - by*sp), cy + ry * (bx*sp + by*cp), dx, dy); };
  for (int k= 0; k < n; k++) {
    double sp= sin (phi * (k + 0.5) + a), cp= cos (phi * (k + 0.5) + a);
    float p0x, p0y, p1x, p1y, p2x, p2y, p3x, p3y;
    P (bx0, by0, sp, cp, p0x, p0y);
    P (bx1, by1, sp, cp, p1x, p1y);
    P (bx2, by2, sp, cp, p2x, p2y);
    P (bx3, by3, sp, cp, p3x, p3y);
    if (k == 0) s.move_to (p0x, p0y);
    s.cubic_to (p1x, p1y, p2x, p2y, p3x, p3y);
  }
  if (filled) fill_style (s, fg);
  else {
    if (abs (delta) == 360*64) s.close ();
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
  gpu_shape s;
  float px, py, c1x, c1y, c2x, c2y;
  // user space (y up): yy1 is the bottom, yy2 the top
  device (xx1 + rbl, yy1, px, py); s.move_to (px, py);
  device (xx2 - rbr, yy1, px, py); s.line_to (px, py);
  if (rbr > 0) {
    device (xx2 - rbr + rbr*k, yy1, c1x, c1y); device (xx2, yy1 + rbr - rbr*k, c2x, c2y);
    device (xx2, yy1 + rbr, px, py); s.cubic_to (c1x, c1y, c2x, c2y, px, py); }
  device (xx2, yy2 - rtr, px, py); s.line_to (px, py);
  if (rtr > 0) {
    device (xx2, yy2 - rtr + rtr*k, c1x, c1y); device (xx2 - rtr + rtr*k, yy2, c2x, c2y);
    device (xx2 - rtr, yy2, px, py); s.cubic_to (c1x, c1y, c2x, c2y, px, py); }
  device (xx1 + rtl, yy2, px, py); s.line_to (px, py);
  if (rtl > 0) {
    device (xx1 + rtl - rtl*k, yy2, c1x, c1y); device (xx1, yy2 - rtl + rtl*k, c2x, c2y);
    device (xx1, yy2 - rtl, px, py); s.cubic_to (c1x, c1y, c2x, c2y, px, py); }
  device (xx1, yy1 + rbl, px, py); s.line_to (px, py);
  if (rbl > 0) {
    device (xx1, yy1 + rbl - rbl*k, c1x, c1y); device (xx1 + rbl - rbl*k, yy1, c2x, c2y);
    device (xx1 + rbl, yy1, px, py); s.cubic_to (c1x, c1y, c2x, c2y, px, py); }
  s.close ();
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
    if (G.B->targets_flipped ())
      user_quad (left, bottom, left + w, bottom + h, 1, g->t.tex, 0, 1, 1, 0, a, a, a, a);
    else
      user_quad (left, bottom, left + w, bottom + h, 1, g->t.tex, 0, 0, 1, 1, a, a, a, a);
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
  blit (t, x1, y2, x2, y1, sh->t, x1, y2);
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
  blit (sh->t, x1, y2, x2, y1, t, x1, y2);
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
  blit (src->t, x, y - (y1 - y2), x + (x2 - x1), y, t, x1, y2);
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
gpu_begin_screen (renderer ren, int w, int h, int dw, int dh) {
  gpu_flush_all ();
  G.screen.w= w; G.screen.h= h;
  G.screen.dw= dw; G.screen.dh= dh;
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

/******************************************************************************
* Canvases of other ports (Cocoa): backing stores shown in layers
******************************************************************************/

bool
gpu_layers_enabled () {
  static int on= -1;
  if (on < 0) {
    on= 0;
    if (get_env ("TEXMACS_NS_GPU") != "0" && vue_gpu_enabled () &&
        G.B == gpu_metal_backend ()) {
      vue_gpu_prepare ();
      if (vue_gpu_enabled ()) {
        if (!G.started) start_gpu ();
        on= 1;
      }
    }
  }
  return on == 1 && !G.failed;
}

bool
gpu_show_picture (void* layer, picture p, int dw, int dh) {
  gpu_picture_rep* g= as_gpu (p);
  if (g == NULL || !G.started || dw <= 0 || dh <= 0) return false;
  if (!ensure_target (&g->t)) return false;
  gpu_flush_all ();
  if (!G.B->attach_layer (layer)) return false;
  G.B->begin_screen (dw, dh);
  G.B->copy (g->t.tex, g->t.w, g->t.h, 0, 0, g->t.w, g->t.h,
             NULL, dw, dh, 0, 0, dw, dh);
  G.B->present (NULL);
  return true;
}

bool
gpu_read_picture (picture p, unsigned char* rgba) {
  gpu_picture_rep* g= as_gpu (p);
  if (g == NULL || !ensure_target (&g->t)) return false;
  gpu_flush_all ();
  return G.B->read (g->t.tex, g->t.w, g->t.h, 0, 0, g->t.w, g->t.h, rgba);
}

picture
gpu_read_screen (int w, int h) {
  gpu_flush_all ();
  if (w <= 0 || h <= 0) return picture ();
  std::vector<unsigned char> rows ((size_t) w * h * 4);
  if (!G.B->read (NULL, w, h, 0, 0, w, h, rows.data ())) return picture ();
#ifdef USE_MUPDF
  fz_pixmap* pix= mupdf_new_pixmap (w, h);
  if (pix == NULL || pix->w != w || pix->h != h) return picture ();
  for (int y= 0; y < h; y++)
    memcpy (pix->samples + (size_t) y * pix->stride,
            rows.data () + (size_t) y * w * 4, (size_t) w * 4);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
#else
  picture p= native_picture (w, h, 0, 0);
  for (int y= 0; y < h; y++)
    for (int x= 0; x < w; x++) {
      const unsigned char* d= &rows[4 * ((size_t) y * w + x)];
      int a= d[3];
      if (a == 0) { p->set_pixel (x, h - 1 - y, rgb_color (0, 0, 0, 0)); continue; }
      p->set_pixel (x, h - 1 - y, rgb_color ((255 * d[0] + a/2) / a, (255 * d[1] + a/2) / a,
                                             (255 * d[2] + a/2) / a, a));
    }
  return p;
#endif
}

#endif // USE_THORVG || USE_METAL
