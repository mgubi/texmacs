/******************************************************************************
* MODULE     : ns_canvas_factor_test.cpp
* DESCRIPTION: the pixels per point of a canvas of the Cocoa port follow the
*              screen of its window (#989)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"

#ifdef AQUATEXMACS

#include "renderer.hpp"   // PIXEL, retina_factor

// (ns_simple_widget.h is Objective-C: the functions alone)
int ns_canvas_factor_for (double backing_scale);
void ns_store_pixels (int k, SI ox, SI oy, SI& x1, SI& y1, SI& x2, SI& y2);

static void
test_factor () {
  // the backingScaleFactor of the screen of the window
  CHECK_EQ (ns_canvas_factor_for (1.0), 1);
  CHECK_EQ (ns_canvas_factor_for (2.0), 2);
  // (no screen: 0; other scales round to the nearest)
  CHECK_EQ (ns_canvas_factor_for (0.0), 1);
  CHECK_EQ (ns_canvas_factor_for (1.49), 1);
  CHECK_EQ (ns_canvas_factor_for (1.5), 2);
  CHECK_EQ (ns_canvas_factor_for (3.0), 2);
}

static void
test_invalidate () {
  // the editor invalidates the points [100, 200] x [50, 60] (y up, so -60
  // to -50): the pixels of the store of the canvas, whatever the global
  // retina_factor (the default) is (#989: it used the shared renderer,
  // whose pixel was that of its last reset)
  int saved= retina_factor;
  for (int g= 1; g <= 2; g++) {
    retina_factor= g;
    SI x1= 100*PIXEL, y1= -60*PIXEL, x2= 200*PIXEL, y2= -50*PIXEL;
    ns_store_pixels (1, 0, 0, x1, y1, x2, y2);
    CHECK_EQ ((int) x1, 100);
    CHECK_EQ ((int) x2, 200);
    CHECK_EQ ((int) y2, 50);
    CHECK_EQ ((int) y1, 60);
    x1= 100*PIXEL; y1= -60*PIXEL; x2= 200*PIXEL; y2= -50*PIXEL;
    ns_store_pixels (2, 0, 0, x1, y1, x2, y2);
    CHECK_EQ ((int) x1, 200);
    CHECK_EQ ((int) x2, 400);
    CHECK_EQ ((int) y2, 100);
    CHECK_EQ ((int) y1, 120);
  }
  retina_factor= saved;
  // the store starting 30 points down the document (backing_pos.y 30, so
  // that oy is 30 points): the same rectangle 30 points higher in the store
  SI x1= 100*PIXEL, y1= -60*PIXEL, x2= 200*PIXEL, y2= -50*PIXEL;
  ns_store_pixels (2, 0, 30*PIXEL, x1, y1, x2, y2);
  CHECK_EQ ((int) y2, 40);
  CHECK_EQ ((int) y1, 60);
}

int
main () {
  RUN (test_factor);
  RUN (test_invalidate);
  return test_report ();
}

#else

static void
test_none () {
  SKIP ("not the Cocoa interface");
}

int
main () {
  RUN (test_none);
  return test_report ();
}

#endif
