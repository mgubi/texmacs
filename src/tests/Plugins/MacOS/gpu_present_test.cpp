/******************************************************************************
* MODULE     : gpu_present_test.cpp
* DESCRIPTION: the frames of the GPU renderer on macOS, presented as
*              IOSurfaces set as the contents of a layer (issue #988)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"

#if defined (USE_METAL) && (defined (AQUATEXMACS) || defined (VUETEXMACS))

#include "Vue/vue_gpu.hpp"
#include "renderer.hpp"
#include "pencil.hpp"
#include "colors.hpp"
#include <CoreFoundation/CoreFoundation.h>
#include <vector>
#include <algorithm>
#include <time.h>

// No window is opened: the layer is drawn by Core Animation into a bitmap
// (renderInContext), which shows what the surface set as its contents holds

static double
now_ms () {
  struct timespec ts;
  clock_gettime (CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1e3 + ts.tv_nsec * 1e-6;
}

// the transactions of the presents are committed by the run loop, whose
// completion blocks give up the use count of the surfaces they replaced
static void
spin () {
  CFRunLoopRunInMode (kCFRunLoopDefaultMode, 0, true);
  CFRunLoopRunInMode (kCFRunLoopDefaultMode, 0, true);
}

static picture store;   // 64 x 32: its top half red, transparent below
static void* layer= NULL;

static bool
ready () {
  if (!gpu_layers_enabled ()) return false;
  if (!gpu_presents_at_once ()) return false;
  if (layer == NULL) layer= gpu_new_layer ();
  if (is_nil (store)) {
    store= gpu_backing_picture (64, 32, true);
    renderer ren= gpu_picture_renderer (store, std_shrinkf);
    SI px= ren->pixel;
    ren->set_origin (0, 0);
    ren->set_clipping (0, -32 * px, 64 * px, 0);
    ren->set_pencil (rgb_color (255, 0, 0));
    ren->fill (0, -16 * px, 64 * px, 0);
    tm_delete (ren);
  }
  return layer != NULL;
}

// a pixel of an RGBA image
static int
px (const std::vector<unsigned char>& img, int w, int x, int y, int c) {
  return img[4 * ((size_t) y * w + x) + c];
}

static void
test_store () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  std::vector<unsigned char> img (64 * 32 * 4);
  CHECK (gpu_read_picture (store, img.data ()));
  CHECK_EQ (px (img, 64, 10, 2, 0), 255);
  CHECK_EQ (px (img, 64, 10, 2, 3), 255);
  CHECK_EQ (px (img, 64, 10, 30, 3), 0);
}

static void
test_present () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  CHECK (gpu_show_picture (layer, store, 64, 32));
  std::vector<unsigned char> img (64 * 32 * 4);
  int n= 0;
  CHECK (gpu_layer_pixels (layer, 64, 32, img.data (), n));
  CHECK_EQ (n, 1);
  // the first row of the surface at the top of the layer
  CHECK_EQ (px (img, 64, 10, 2, 0), 255);
  CHECK_EQ (px (img, 64, 10, 2, 1), 0);
  CHECK_EQ (px (img, 64, 10, 2, 3), 255);
  CHECK_EQ (px (img, 64, 10, 29, 3), 0);
}

static void
test_ring () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  // presents in a burst do not wait (no compositor here: the surface shown
  // is the only one in use once the transaction which replaced the one
  // before is committed, and the frames alternate between two)
  double worst= 0;
  for (int i= 0; i < 30; i++) {
    double t= now_ms ();
    CHECK (gpu_show_picture (layer, store, 64, 32));
    worst= max (worst, now_ms () - t);
    spin ();
  }
  CHECK_MSG (worst < 50, "a present waited");
  std::vector<unsigned char> img (64 * 32 * 4);
  int n= 0;
  CHECK (gpu_layer_pixels (layer, 64, 32, img.data (), n));
  CHECK_EQ (n, 2);
  CHECK_EQ (px (img, 64, 10, 2, 0), 255);
}

static void
test_scaled () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  // twice the pixels of the store (a retina_factor of 1 on a Retina
  // screen): the store is scaled into new surfaces, cleared first (the
  // copy at another scale is composed over them)
  CHECK (gpu_show_picture (layer, store, 128, 64)); spin ();
  CHECK (gpu_show_picture (layer, store, 128, 64)); spin ();
  CHECK (gpu_show_picture (layer, store, 128, 64)); spin ();
  std::vector<unsigned char> img (128 * 64 * 4);
  int n= 0;
  CHECK (gpu_layer_pixels (layer, 128, 64, img.data (), n));
  CHECK_EQ (n, 2);   // (those of the old size are gone)
  CHECK_EQ (px (img, 128, 20, 4, 0), 255);
  CHECK_EQ (px (img, 128, 20, 4, 3), 255);
  CHECK_EQ (px (img, 128, 20, 60, 3), 0);
  // and back to the size of the store
  CHECK (gpu_show_picture (layer, store, 64, 32));
  std::vector<unsigned char> small (64 * 32 * 4);
  CHECK (gpu_layer_pixels (layer, 64, 32, small.data (), n));
  CHECK_EQ (px (small, 64, 10, 2, 0), 255);
  CHECK_EQ (px (small, 64, 10, 29, 3), 0);
}

static void
test_uncommitted () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  // two presents before the transaction is committed (two repaints in one
  // pass of the run loop): the second does not draw into the surface which
  // the compositor still shows, so a third one is made
  CHECK (gpu_show_picture (layer, store, 64, 32));
  CHECK (gpu_show_picture (layer, store, 64, 32));
  std::vector<unsigned char> img (64 * 32 * 4);
  int n= 0;
  CHECK (gpu_layer_pixels (layer, 64, 32, img.data (), n));
  CHECK_EQ (n, 3);
  spin ();
  // committed: the next present takes a free surface again (three are
  // kept)
  CHECK (gpu_show_picture (layer, store, 64, 32));
  spin ();
  CHECK (gpu_layer_pixels (layer, 64, 32, img.data (), n));
  CHECK_EQ (n, 3);
  CHECK_EQ (px (img, 64, 10, 2, 0), 255);
}

static void
test_screen () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  // the route of the windows of the Vue port: the screen renderer draws
  // into the surface which gpu_begin_screen takes, and vue_gpu_present
  // shows it (the layer attached last, here by gpu_show_picture)
  CHECK (gpu_show_picture (layer, store, 64, 32));
  spin ();
  renderer ren= gpu_screen_renderer (std_shrinkf);
  gpu_begin_screen (ren, 64, 32, 64, 32);
  SI p= ren->pixel;
  ren->set_origin (0, 0);
  ren->set_clipping (0, -32 * p, 64 * p, 0);
  ren->set_pencil (rgb_color (0, 0, 255));
  ren->fill (0, -32 * p, 64 * p, 0);
  vue_gpu_present (NULL);
  spin ();
  tm_delete (ren);
  std::vector<unsigned char> img (64 * 32 * 4);
  int n= 0;
  CHECK (gpu_layer_pixels (layer, 64, 32, img.data (), n));
  CHECK_EQ (px (img, 64, 10, 2, 2), 255);
  CHECK_EQ (px (img, 64, 10, 29, 2), 255);
  CHECK_EQ (px (img, 64, 10, 29, 0), 0);
}

static void
test_timing () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  // a window of 1000 x 800 points on a Retina screen: a present (the copy
  // of the store into a surface, the wait for the GPU, the contents set)
  picture big= gpu_backing_picture (2000, 1600, true);
  void* l= gpu_new_layer ();
  std::vector<double> t;
  for (int i= 0; i < 21; i++) {
    double t0= now_ms ();
    CHECK (gpu_show_picture (l, big, 2000, 1600));
    t.push_back (now_ms () - t0);
    spin ();
  }
  std::sort (t.begin (), t.end ());
  cout << "TeXmacs] present of 2000x1600: median " << t[10] << " ms, worst "
       << t[20] << " ms" << LF;
  CHECK_MSG (t[10] < 20, "a present of a window took 20 ms");
  gpu_forget_layer (l);
  CFRelease ((CFTypeRef) l);
}

static void
test_forget () {
  if (!ready ()) SKIP ("no Metal, or TEXMACS_NS_PRESENT/TEXMACS_VUE_PRESENT=metal");
  gpu_forget_layer (layer);
  std::vector<unsigned char> img (64 * 32 * 4);
  int n= -1;
  CHECK (gpu_layer_pixels (layer, 64, 32, img.data (), n));
  CHECK_EQ (n, 0);
  CHECK_EQ (px (img, 64, 10, 2, 3), 0);   // no contents
  CFRelease ((CFTypeRef) layer);
  layer= NULL;
}

int
main () {
  RUN (test_store);
  RUN (test_present);
  RUN (test_ring);
  RUN (test_scaled);
  RUN (test_uncommitted);
  RUN (test_screen);
  RUN (test_timing);
  RUN (test_forget);
  return test_report ();
}

#else

static void
test_none () {
  SKIP ("no Metal in this build (or not the Cocoa or Vue interface)");
}

int
main () {
  RUN (test_none);
  return test_report ();
}

#endif
