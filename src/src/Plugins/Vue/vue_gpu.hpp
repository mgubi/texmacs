/******************************************************************************
* MODULE     : vue_gpu.hpp
* DESCRIPTION: The GPU renderer of the Vue port: a glyph atlas, textures
*              and vector shapes, drawn by OpenGL (WebGL2 in the browser)
*              or Metal (gpu_backend.hpp)
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

// Whether the windows are drawn by the GPU: compiled with a backend
// (OpenGL with --with-thorvg, Metal on macOS), unless TEXMACS_VUE_GPU=0
// (=gl or =metal chooses the backend). Decided once.
bool vue_gpu_enabled ();
// the flags of an SDL window drawn by the GPU (SDL_WINDOW_OPENGL...)
unsigned long long vue_gpu_window_flags ();
// the windows are drawn by the GPU (vue_gui.cpp: not in the single window)
bool vue_gpu_windows ();
// the glyphs as the preferences say ("gpu glyphs"...): true when they
// changed, and everything must be drawn again
bool vue_gpu_follow_glyph_settings ();
// when the GUI starts: the backend begins what takes time (the shaders of
// Metal), while TeXmacs boots
void vue_gpu_warm_up ();
// before the first window is created: the attributes of its GL context
void vue_gpu_prepare ();
// draw into the window from now on (the backend makes the one, shared GL
// context or Metal device with the first window); false when it cannot
bool vue_gpu_attach (SDL_Window* w);
void vue_gpu_present (SDL_Window* w);
// attached, and presenting into the window would not wait for the screen
// (Metal: the compositor does not hold too many of its frames); else the
// window is drawn at a later turn of the loop
bool vue_gpu_ready (SDL_Window* w);

// the backing store of an editor: an opaque texture, white at first
// (transparent: the canvases of Cocoa, transparent where not painted, as
// their bitmaps were; the editors of Vue are opaque white)
picture  gpu_backing_picture (int w, int h, bool transparent= false);
bool     is_gpu_picture (picture p);
renderer gpu_picture_renderer (picture p, double zoom);
// shift the content of a backing store by (dpx, dpy) pixels (y down)
void     gpu_translate_picture (picture p, int dpx, int dpy);
picture  gpu_copy_picture (picture p);

// the renderer of the window being drawn (its default framebuffer)
renderer gpu_screen_renderer (double zoom);
// (w x h: the pixels of the layout, drawn over the dw x dh of the window)
void     gpu_begin_screen (renderer ren, int w, int h, int dw, int dh);
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
// the canvases of the Cocoa port drawn by the GPU (Metal, unless
// TEXMACS_NS_GPU=0): their backing stores (gpu_backing_picture) are shown in
// a CAMetalLayer of their view (dw x dh pixels, scaled when the sizes
// differ), and read back (premultiplied RGBA, rows from the top: the
// snapshots, which draw the views)
bool gpu_layers_enabled ();
bool gpu_show_picture (void* metal_layer, picture p, int dw, int dh);
bool gpu_read_picture (picture p, unsigned char* rgba);

#endif // defined VUE_GPU_HPP
