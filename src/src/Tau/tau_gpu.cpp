
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
// (gpu_backend_gl.cpp, GL_OFFSCREEN), which will draw the commands of the
// core in the worker (docs/tau-design.md, "The renderer in C++"). For now:
// whether it draws, for the tests.

#include "renderer.hpp"
#include "colors.hpp"
#include "Vue/vue_gpu.hpp"
#include <string>
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
