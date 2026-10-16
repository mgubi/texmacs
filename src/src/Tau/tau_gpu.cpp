
/******************************************************************************
* MODULE     : tau_gpu.cpp
* DESCRIPTION: The GPU renderer of Vue in the core of Tau, in the browser
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// The core built with the GPU renderer (misc/tau/Makefile, THORVG): the
// renderer of the shell (vue_gpu.cpp) on a WebGL 2 context of the worker
// (gpu_backend_gl.cpp, GL_OFFSCREEN), which draws the commands of the core
// in the worker (docs/tau-design.md, "The renderer in C++"): each view in
// a target of its own, and what changed in it given to the page as an
// ImageBitmap in place of the commands (tau_gpu_show, tau_gui.cpp), which
// the page copies to its canvas as it does the pixels of the core. Also whether it draws, for the tests,
// and how long the frames take it (tau_gpu_bench).

#include "renderer.hpp"
#include "colors.hpp"
#include "Vue/vue_gpu.hpp"
#include "MuPDF/mupdf_picture.hpp"
#include <string>
#include <map>
#include <vector>
#include <cstring>
#include <emscripten.h>

// a rectangle (quads of a colour), a thick line (a shape, ThorVG), a glyph
// given as its coverage (the atlas) and a rectangle moved by a matrix,
// drawn into a target of 64 x 48 pixels: the pixels read back at the
// places of each, and the GPU ("no GPU" when there is none)
extern "C" EMSCRIPTEN_KEEPALIVE const char*
tau_gpu_check () {
  static std::string out;
  if (!vue_gpu_start ()) return "no GPU";
  picture p= gpu_backing_picture (64, 48);
  renderer r= gpu_picture_renderer (p, 1.0);
  if (r == NULL) return "no renderer";
  SI P= r->pixel;
  // (pixels of the target, y down, as the commands say them)
  auto rect= [&] (int x1, int y1, int x2, int y2, color c) {
    r->set_pencil (pencil (c));
    r->fill (x1 * P, -y2 * P, x2 * P, -y1 * P); };
  rect (4, 4, 20, 20, rgb_color (255, 0, 0));
  r->set_pencil (pencil (rgb_color (0, 0, 255), 4 * P));
  r->line (24 * P, -12 * P, 60 * P, -12 * P);
  unsigned char cov[8 * 8];
  for (int i= 0; i < 64; i++) cov[i]= 255;
  gpu_draw_coverage (r, 1, 8, 8, cov, 30, 30, rgb_color (0, 128, 0));
  // (the matrix of the commands: in pixels, y up; moved 40 pixels right)
  double m[6]= { 1, 0, 0, 1, 40, 0 };
  gpu_push_matrix (r, m);
  rect (4, 30, 12, 38, rgb_color (255, 0, 255));
  gpu_pop_matrix (r);
  delete_renderer (r);
  std::string px (64 * 48 * 4, '\0');
  if (!gpu_read_picture (p, (unsigned char*) &px[0])) return "not read";
  auto at= [&] (int x, int y) {
    const unsigned char* q= (const unsigned char*) &px[4 * (y * 64 + x)];
    return std::to_string (q[0]) + "," + std::to_string (q[1]) + "," +
           std::to_string (q[2]) + "," + std::to_string (q[3]); };
  out= "rect " + at (12, 12) + "; line " + at (40, 12) + "; glyph " + at (34, 34) +
       "; moved " + at (48, 34) + "; white " + at (2, 46);
  return out.c_str ();
}

/******************************************************************************
* The commands of the views drawn in the worker, as the shell draws them
* (tau_shell.cpp, commands), into a target of each view, which a scroll
* shifts as the core shifts its pixels (the rectangle which the core says
* is drawn has a margin which the commands do not draw: the pixels which
* were there, shifted). The rectangle alone goes to the page, which puts it
* in its place after it shifted its canvas, as with the pixels of the core
* (the whole view cost a copy of it more, and in Firefox a read back: 9 ms).
* Timed by misc/tau/test/replay-bench.mjs --native, --core
******************************************************************************/

static bool bench= false, show= false;
static std::map<int, std::pair<int,int> > glyph_size;
static std::map<int, std::string> glyph_cov;
static std::map<int, picture> pictures;
static std::map<int, picture> frames;  // the target of each view
struct bench_part { double calls, show, gpu; int w, h; };
static std::vector<bench_part> parts;

extern "C" EMSCRIPTEN_KEEPALIVE void
tau_gpu_bench (int on) {
  bench= (on != 0) && vue_gpu_start ();
  parts.clear ();
}

// the views drawn here and shown to the page (?coregl, tau-worker.js), or
// not: whether they are (false: no GPU, the page draws the pixels)
extern "C" EMSCRIPTEN_KEEPALIVE int
tau_gpu_show (int on) {
  show= (on != 0) && vue_gpu_start ();
  return show ? 1 : 0;
}

// ?direct: the view whose canvas the page gave to the worker, drawn on it
// whole at each frame from its target (the canvas has nothing kept between
// frames); the others are pixels (one WebGL context, on that canvas)
static int direct= -1;

extern "C" EMSCRIPTEN_KEEPALIVE void
tau_gpu_direct (int view) {
  direct= view;
}

extern "C" int
tau_gpu_records (int view) {
  return (!show || direct < 0 || view == direct) ? 1 : 0;
}

// the target of a view which is no more (tau_widget.cpp)
extern "C" void
tau_gpu_forget (int view) {
  frames.erase (view);
}

// the means of the frames drawn since tau_gpu_bench, in ms: the shift and
// the calls, the rectangle on the canvas given as a bitmap (show), the GPU
// waited for; and their number and pixels
extern "C" EMSCRIPTEN_KEEPALIVE const char*
tau_gpu_stats () {
  static std::string out;
  double c= 0, s= 0, g= 0, a= 0; int n= (int) parts.size ();
  for (auto& p: parts) { c += p.calls; s += p.show; g += p.gpu; a += (double) p.w * p.h; }
  if (n > 0) { c /= n; s /= n; g /= n; a /= n; }
  out= "{\"calls\":" + std::to_string (c) + ",\"show\":" + std::to_string (s) +
       ",\"gpu\":" + std::to_string (g) + ",\"frames\":" + std::to_string (n) +
       ",\"area\":" + std::to_string (a) + "}";
  return out.c_str ();
}

static void
learn (const unsigned char* gl, int ngl, const unsigned char* im, int nim) {
  auto get= [] (const unsigned char* p) { int32_t v; memcpy (&v, p, 4); return (int) v; };
  for (size_t p= 0; p + 12 <= (size_t) ngl;) {
    int id= get (gl + p), gw= get (gl + p + 4), gh= get (gl + p + 8);
    p += 12;
    if (gw < 0 || gh < 0 || p + (size_t) gw * gh > (size_t) ngl) break;
    glyph_size[id]= std::make_pair (gw, gh);
    glyph_cov[id]= std::string ((const char*) gl + p, (size_t) gw * gh);
    p += (size_t) gw * gh;
  }
  fz_context* ctx= mupdf_context ();
  for (size_t p= 0; p + 12 <= (size_t) nim;) {
    int id= get (im + p), pw= get (im + p + 4), ph= get (im + p + 8);
    p += 12;
    if (pw <= 0 || ph <= 0 || p + (size_t) 4 * pw * ph > (size_t) nim) break;
    fz_pixmap* pm= mupdf_new_pixmap (pw, ph);
    if (pm != NULL) {
      unsigned char* d= fz_pixmap_samples (ctx, pm);
      ptrdiff_t st= fz_pixmap_stride (ctx, pm);
      for (int j= 0; j < ph; j++)
        for (int i= 0; i < pw; i++) {
          const unsigned char* q= im + p + 4 * ((size_t) j * pw + i);
          unsigned char* t= d + (ptrdiff_t) j * st + 4 * i;
          int a= q[3];
          t[0]= (unsigned char) (q[0] * a / 255); t[1]= (unsigned char) (q[1] * a / 255);
          t[2]= (unsigned char) (q[2] * a / 255); t[3]= (unsigned char) a;
        }
      pictures[id]= mupdf_picture (pm, 0, 0);
      fz_drop_pixmap (ctx, pm);
    }
    p += (size_t) 4 * pw * ph;
  }
}

static void
replay (renderer r, const int32_t* o, int nops, int x, int y, int dw, int dh) {
  SI P= r->pixel;
  auto col= [] (int v) { unsigned u= (unsigned) v;
    return rgb_color ((int) (u >> 24), (int) ((u >> 16) & 255), (int) ((u >> 8) & 255), (int) (u & 255)); };
  int rx1= x, ry1= y, rx2= x + dw, ry2= y + dh;
  auto clip= [&] (int x1, int y1, int x2, int y2) {
    x1= std::max (x1, rx1); y1= std::max (y1, ry1);
    x2= std::max (x1, std::min (x2, rx2)); y2= std::max (y1, std::min (y2, ry2));
    r->set_clipping (x1 * P, -y2 * P, x2 * P, -y1 * P); };
  clip (rx1, ry1, rx2, ry2);
  // (the glyphs first, see gpu_place_coverage)
  for (int i= 0, k; i < nops; i += k) {
    int op= o[i];
    k= op == 3 ? 4 + 2 * o[i+1] : op == 4 ? 3 + 2 * o[i+1] :
       op == 1 ? 6 : op == 2 || op == 6 || op == 7 || op == 8 ? 5 :
       op == 5 ? 10 : op == 9 ? 7 : op == 10 ? 1 : 0;
    if (k <= 0 || i + k > nops) break;
    if (op == 2) {
      auto gs= glyph_size.find (o[i+1]);
      if (gs != glyph_size.end () && gs->second.first > 0)
        gpu_place_coverage (o[i+1], gs->second.first, gs->second.second,
                            (const unsigned char*) glyph_cov[o[i+1]].data ());
    }
  }
  for (int i= 0; i < nops;) {
    int op= o[i];
    if (op == 1 && i + 6 <= nops) {
      r->set_pencil (pencil (col (o[i+5])));
      r->fill (o[i+1] * P, -o[i+4] * P, o[i+3] * P, -o[i+2] * P);
      i += 6;
    }
    else if (op == 2 && i + 5 <= nops) {
      auto gs= glyph_size.find (o[i+1]);
      if (gs != glyph_size.end () && gs->second.first > 0)
        gpu_draw_coverage (r, o[i+1], gs->second.first, gs->second.second,
                           (const unsigned char*) glyph_cov[o[i+1]].data (),
                           o[i+2], o[i+3], col (o[i+4]));
      i += 5;
    }
    else if ((op == 3 || op == 4) && i + 3 <= nops) {
      bool line= (op == 3);
      int k= o[i+1], at= i + (line? 4: 3);
      if (k < 0 || at + 2 * k > nops) break;
      array<SI> xs (k), ys (k);
      for (int j= 0; j < k; j++) { xs[j]= o[at + 2*j] * P; ys[j]= -o[at + 2*j + 1] * P; }
      if (line) {
        r->set_pencil (pencil (col (o[i+3]), (o[i+2] * P) / 16));
        if (k == 2) r->line (xs[0], ys[0], xs[1], ys[1]);
        else r->lines (xs, ys);
      }
      else {
        r->set_pencil (pencil (col (o[i+2])));
        r->set_brush (brush (col (o[i+2])));
        r->polygon (xs, ys);
      }
      i= at + 2 * k;
    }
    else if (op == 5 && i + 10 <= nops) {
      r->set_pencil (pencil (col (o[i+8]), (o[i+7] * P) / 16));
      r->set_brush (brush (col (o[i+8])));
      SI ax1= o[i+1] * P, ay1= -o[i+4] * P, ax2= o[i+3] * P, ay2= -o[i+2] * P;
      if (o[i+9]) r->fill_arc (ax1, ay1, ax2, ay2, o[i+5], o[i+6]);
      else r->arc (ax1, ay1, ax2, ay2, o[i+5], o[i+6]);
      i += 10;
    }
    else if (op == 6 && i + 5 <= nops) {
      clip (o[i+1], o[i+2], o[i+3], o[i+4]);
      i += 5;
    }
    else if (op == 7 && i + 5 <= nops) {
      r->set_pencil (pencil (rgb_color (216, 216, 216)));
      r->fill (o[i+1] * P, -o[i+4] * P, o[i+3] * P, -o[i+2] * P);
      i += 5;
    }
    else if (op == 9 && i + 7 <= nops) {
      double m[6];
      for (int k= 0; k < 6; k++) { float v; memcpy (&v, &o[i+1+k], 4); m[k]= v; }
      gpu_push_matrix (r, m);
      i += 7;
    }
    else if (op == 10) {
      gpu_pop_matrix (r);
      i += 1;
    }
    else if (op == 8 && i + 5 <= nops) {
      auto pc= pictures.find (o[i+1]);
      if (pc != pictures.end ())
        r->draw_picture (pc->second, o[i+2] * P,
                         -(o[i+3] + pc->second->get_height ()) * P, o[i+4]);
      i += 5;
    }
    else break;
  }
}

// (called by tau_gui.cpp with each frame of commands, before the page has
// it: the bitmap goes with them, tau_js_dlist)
extern "C" void
tau_gpu_frame (int view, int w, int h, const int* ops, int nops,
               const unsigned char* gl, int ngl, const unsigned char* im, int nim,
               int x1, int y1, int x2, int y2, int dx, int dy) {
  // (the glyphs and the pictures are sent once: learnt also when the frames
  // are not drawn)
  learn (gl, ngl, im, nim);
  if ((!bench && !show) || x2 <= x1 || y2 <= y1) return;
  picture& f= frames[view];
  if (is_nil (f) || f->get_width () != w || f->get_height () != h)
    f= gpu_backing_picture (w, h);
  double t0= emscripten_get_now ();
  if (dx != 0 || dy != 0) gpu_translate_picture (f, dx, dy);
  renderer r= gpu_picture_renderer (f, std_shrinkf);
  if (r == NULL) return;
  replay (r, (const int32_t*) ops, nops, x1, y1, x2 - x1, y2 - y1);
  delete_renderer (r);
  double t1= emscripten_get_now ();
  if (show && view == direct) gpu_show_part (NULL, f, 0, 0, w, h);
  else if (show) gpu_show_part (NULL, f, x1, y1, x2 - x1, y2 - y1);
  double t2= emscripten_get_now ();
  if (!bench) return;
  gpu_sync_picture (f);
  double t3= emscripten_get_now ();
  parts.push_back ({ t1 - t0, t2 - t1, t3 - t2, x2 - x1, y2 - y1 });
}
