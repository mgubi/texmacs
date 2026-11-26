
/******************************************************************************
* MODULE     : wheel_steps_test.cpp
* DESCRIPTION: the wheel of the Qt port, as the Cocoa and Vue ports replay it
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "wheel_steps.hpp"

// (issue #971) the distances are those of QScrollBar in the Qt port for a
// view of 800 points: a single step of 800/16 + 1 = 51 points

/******************************************************************************
* The angle delta of -[QNSView scrollWheel:]
******************************************************************************/

static void
test_mac_angle () {
  // a mouse wheel: 120 an event, whatever the acceleration of macOS (as
  // the deltas of SDL, rounded away from zero, give it in the Vue port)
  CHECK_EQ (qt_wheel_mac_angle (10.0, 1.0, false), 120);
  CHECK_EQ (qt_wheel_mac_angle (37.0, 3.7, false), 120);
  CHECK_EQ (qt_wheel_mac_angle (-90.0, -9.0, false), -120);
  CHECK_EQ (qt_wheel_mac_angle (0.0, 0.0, false), 0);
  CHECK_EQ (qt_wheel_mac_angle (0.05, 0.005, false), 50);
  // a trackpad: twice its scrolling delta in points, truncated
  CHECK_EQ (qt_wheel_mac_angle (7.6, 0.76, true), 15);
  CHECK_EQ (qt_wheel_mac_angle (-7.6, -0.76, true), -15);
  CHECK_EQ (qt_wheel_mac_angle (0.4, 0.04, true), 0);
  CHECK_EQ (qt_wheel_mac_angle (100.0, 10.0, true), 200);
}

/******************************************************************************
* QAbstractScrollArea and QAbstractSliderPrivate::scrollByDelta
******************************************************************************/

static void
test_single_step () {
  CHECK_EQ (qt_wheel_single_step (800), 51);
  CHECK_EQ (qt_wheel_single_step (15), 1);
  CHECK_EQ (qt_wheel_single_step (16), 2);
}

static void
test_axis () {
  CHECK (!qt_wheel_horizontal (0, 120));
  CHECK (qt_wheel_horizontal (-120, 0));
  CHECK (!qt_wheel_horizontal (30, -30)); // a tie goes to the vertical bar
  CHECK (qt_wheel_horizontal (31, -30));
}

static void
test_notch () {
  // a notch scrolls three single steps at once, in both directions
  qt_wheel_axis a;
  CHECK_EQ (a.scroll (120, 51, 800), 153);
  CHECK_EQ (a.scroll (120, 51, 800), 153);
  CHECK_EQ (a.scroll (-120, 51, 800), -153);
  // never more than a page in one event (a small view)
  qt_wheel_axis s;
  CHECK_EQ (s.scroll (120, 7, 20), 20);
}

static void
test_fractions () {
  // a trackpad: the fractions of a point are carried to the next event
  qt_wheel_axis a;
  CHECK_EQ (a.scroll (15, 51, 800), 19);   // 19.125
  int total= 19;
  for (int i= 0; i < 7; i++) total += a.scroll (15, 51, 800);
  CHECK_EQ (total, 153);                   // 8 x 19.125
  // ... and dropped when the direction changes
  qt_wheel_axis b;
  CHECK_EQ (b.scroll (100, 51, 800), 127); // 127.5, 0.5 carried
  CHECK_EQ (b.scroll (-1, 51, 800), -1);   // -1.275 (-0.775 with the 0.5)
  CHECK_EQ (b.scroll (-1, 51, 800), -1);   // -0.275 - 1.275 = -1.55
  CHECK_EQ (b.scroll (-1, 51, 800), -1);   // -1.825
  CHECK_EQ (b.scroll (-1, 51, 800), -2);   // -2.1
  // a large delta of the momentum: at most a page
  qt_wheel_axis c;
  CHECK_EQ (c.scroll (2400, 51, 800), 800);
  c.reset ();
  CHECK_EQ (c.scroll (2, 51, 800), 2);     // 2.55: nothing left from before
}

int
main () {
  RUN (test_mac_angle);
  RUN (test_single_step);
  RUN (test_axis);
  RUN (test_notch);
  RUN (test_fractions);
  return test_report ();
}
