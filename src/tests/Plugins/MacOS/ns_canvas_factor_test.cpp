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

// (ns_simple_widget.h is Objective-C: the function alone)
int ns_canvas_factor_for (double backing_scale);

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

int
main () {
  RUN (test_factor);
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
