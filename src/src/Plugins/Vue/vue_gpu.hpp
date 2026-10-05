/******************************************************************************
* MODULE     : vue_gpu.hpp
* DESCRIPTION: The GPU renderer of the Vue port (OpenGL, WebGL2 in the
*              browser): a glyph atlas, textures and ThorVG
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef VUE_GPU_HPP
#define VUE_GPU_HPP

#include "renderer.hpp"
#include "picture.hpp"

struct SDL_Window;

// Whether the windows are drawn by the GPU: compiled with ThorVG
// (--with-thorvg), unless TEXMACS_VUE_GPU=0. Decided once.
bool vue_gpu_enabled ();
// the windows are drawn by the GPU (vue_gui.cpp: not in the single window)
bool vue_gpu_windows ();
// before the first window is created: the attributes of its GL context
void vue_gpu_prepare ();
// make the (one, shared) GL context current for the window, creating it
// with the first window; false when there is no GL
bool vue_gpu_attach (SDL_Window* w);
void vue_gpu_present (SDL_Window* w);

// the backing store of an editor: an opaque texture, white at first
picture  gpu_backing_picture (int w, int h);
bool     is_gpu_picture (picture p);
renderer gpu_picture_renderer (picture p, double zoom);
// shift the content of a backing store by (dpx, dpy) pixels (y down)
void     gpu_translate_picture (picture p, int dpx, int dpy);
picture  gpu_copy_picture (picture p);

// the renderer of the window being drawn (its default framebuffer)
renderer gpu_screen_renderer (double zoom);
void     gpu_begin_screen (renderer ren, int w, int h);
// draw what is queued (before presenting, or reading the pixels)
void     gpu_flush ();
// the same, and wait for the GPU (the profile: TEXMACS_VUE_PROFILE)
void     gpu_finish ();
// what the window being drawn drew in this frame, as a hash: a frame which
// draws what the last one drew is not presented (on macOS a present waits
// for the display, even with no swap interval)
unsigned long long gpu_frame_hash ();
// the default framebuffer as a (MuPDF) picture, for the snapshots
picture  gpu_read_screen (int w, int h);

// draw_picture_scaled of the MuPDF renderer (the smooth zoom)
bool gpu_draw_picture_scaled (renderer ren, picture p, SI x, SI y,
                              double s, int alpha);
bool is_gpu_renderer (renderer ren);

#endif // defined VUE_GPU_HPP
