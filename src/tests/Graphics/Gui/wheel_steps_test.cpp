
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

static void
test_scroll_bar () {
  // a slow trackpad in a small view (single step 8): less than a point an
  // event, which adds up while the bar can move, and is not lost (#976)
  qt_wheel_axis a;
  bool taken= false;
  int total= 0;
  for (int i= 0; i < 10; i++) {
    total += a.scroll_bar (2, 8, 120, true, taken);
    CHECK (taken);
  }
  CHECK_EQ (total, 4);                       // 10 x 0.4
  // at its end that way: nothing scrolled, the event is not taken (the
  // enclosing scroll view gets it) and the fraction is dropped
  CHECK_EQ (a.scroll_bar (2, 8, 120, false, taken), 0);
  CHECK (!taken);
  CHECK_EQ (a.scroll_bar (2, 8, 120, true, taken), 0); // 0.4, from nothing
  CHECK (taken);
  CHECK_EQ (a.scroll_bar (2, 8, 120, true, taken), 0); // 0.8
  CHECK_EQ (a.scroll_bar (2, 8, 120, true, taken), 1); // 1.2
}

static void
test_area () {
  // a QScrollArea (side tools, dialogs; the menus and lists of Vue): 60
  // points a notch, a trackpad as many points as it moves
  CHECK_EQ (QT_WHEEL_AREA_STEP, 20);
  CHECK_EQ (qt_wheel_area_points (120), 60.0);
  CHECK_EQ (qt_wheel_area_points (-120), -60.0);
  CHECK_EQ (qt_wheel_area_points (qt_wheel_mac_angle (7.0, 0.7, true)), 7.0);
  qt_wheel_axis a;
  CHECK_EQ (a.scroll (120, QT_WHEEL_AREA_STEP, 300), 60);
}

static qt_wheel_record
record (double dy, double sy, bool precise) {
  qt_wheel_record r;
  r.dx= 0; r.dy= dy; r.sx= 0; r.sy= sy;
  r.precise= precise; r.cancelled= false; r.ignored= false;
  return r;
}

static void
test_record () {
  // Qt's angles from the NSEvent, never from the deltas of SDL (whose
  // formula changed between SDL 3.4.2 and 3.4.20): a swipe opening with a
  // whole delta, a slow notch (less than 120, as in Qt), the exact angle
  qt_wheel_record t= record (3.0, 30.0, true);
  CHECK_EQ (qt_wheel_mac_angle (t.sy, t.dy, t.precise), 60);
  CHECK_EQ (qt_wheel_mac_angle (0.0, 0.005, false), 50);
  CHECK_EQ (qt_wheel_mac_angle (7.0, 0.7, true), 14);
  // the pixel delta (a QPoint): truncated, 20 points a line for a mouse
  CHECK_EQ (qt_wheel_mac_pixels (7.9, true), 7);
  CHECK_EQ (qt_wheel_mac_pixels (-7.9, true), -7);
  CHECK_EQ (qt_wheel_mac_pixels (0.1, false), 2);
  CHECK_EQ (qt_wheel_mac_pixels (1.37, false), 27);
}

static void
test_queue () {
  qt_wheel_queue q;
  qt_wheel_record e;
  // each wheel event of SDL gets the record of its NSEvent, by the serial
  // number it carries
  q.push (1, record (3.0, 30.0, true));
  q.push (2, record (1.0, 1.0, false));
  CHECK (q.take (1, e));
  CHECK (e.precise);
  CHECK (q.take (2, e));
  CHECK (!e.precise);
  CHECK_EQ (q.count, 0);
  CHECK (!q.take (3, e));
  // the record of an event handled without asking does not outlive it: a
  // later event does not take it, whatever its deltas (#976)
  q.push (4, record (1.0, 1.0, false));   // a notch, zoomed
  q.push (5, record (1.0, 10.0, true));   // a swipe
  CHECK (q.take (5, e));
  CHECK (e.precise);
  CHECK_EQ (q.count, 0);
  // the oldest records go when there are too many
  for (int i= 0; i < qt_wheel_queue::max + 5; i++)
    q.push (100 + i, record (1.0, 1.0, false));
  CHECK_EQ (q.count, (int) qt_wheel_queue::max);
  CHECK (!q.take (100, e));
  CHECK (q.take (100 + qt_wheel_queue::max + 4, e));
}

static void
test_zoom () {
  // QTMWidget::wheelEvent with command: the pixel delta, by its sixteenth
  // root; a notch of a mouse gives 20 pixels, a trackpad its points
  double notch= qt_wheel_zoom (0, qt_wheel_mac_pixels (1.0, false), 0, 120);
  CHECK (fabs (notch - pow (20.0, 1.0 / 16)) < 1e-12);
  CHECK (qt_wheel_zoom (0, -7, 0, -14) < 0);
  CHECK (fabs (qt_wheel_zoom (0, -16, 0, -32) + pow (16.0, 1.0 / 16)) < 1e-12);
  // no pixel delta: the angle in degrees; nothing at all: no zoom
  CHECK (fabs (qt_wheel_zoom (0, 0, 0, 120) - pow (15.0, 1.0 / 16)) < 1e-12);
  CHECK_EQ (qt_wheel_zoom (0, 0, 0, 0), 0.0);
  // a cancelled gesture (zeroed deltas) and a sideways pixel delta: none
  CHECK_EQ (qt_wheel_zoom (5, 0, 10, 0), 0.0);
}

static void
test_serial_wrap () {
  // the serial numbers wrap around after 2^32 events
  CHECK (qt_wheel_serial_before (1, 2));
  CHECK (!qt_wheel_serial_before (2, 1));
  CHECK (qt_wheel_serial_before (0xfffffffeu, 1));
  CHECK (!qt_wheel_serial_before (1, 0xfffffffeu));
  qt_wheel_queue q;
  qt_wheel_record e= record (1.0, 1.0, false);
  q.push (0xfffffffeu, e);
  q.push (0xffffffffu, e);
  q.push (1, e);
  CHECK (q.take (1, e));     // the two before the wrap are older: dropped
  CHECK_EQ (q.count, 0);
}

int
main () {
  RUN (test_mac_angle);
  RUN (test_single_step);
  RUN (test_axis);
  RUN (test_notch);
  RUN (test_fractions);
  RUN (test_scroll_bar);
  RUN (test_area);
  RUN (test_record);
  RUN (test_queue);
  RUN (test_zoom);
  RUN (test_serial_wrap);
  return test_report ();
}
