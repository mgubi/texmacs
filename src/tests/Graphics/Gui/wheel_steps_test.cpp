
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

static void
test_sdl_delta () {
  // the deltas of SDL: a mouse rounded away from zero, a trackpad as it is
  CHECK_EQ (qt_wheel_sdl_delta (0.1, false), 1.0f);
  CHECK_EQ (qt_wheel_sdl_delta (-2.3, false), -3.0f);
  CHECK_EQ (qt_wheel_sdl_delta (0.0, false), 0.0f);
  CHECK_EQ (qt_wheel_sdl_delta (0.25, true), 0.25f);
  CHECK_EQ (qt_wheel_sdl_delta (3.0, true), 3.0f);
}

static void
test_queue () {
  // a swipe which opens with a whole delta is a trackpad, from the event
  // (#976: it was taken for a notch, six times too far)
  qt_wheel_queue q;
  qt_wheel_record e;
  q.push ({ 0.0f, 3.0f, true, false });     // a trackpad, whole delta
  q.push ({ 0.0f, 1.0f, false, false });    // a mouse
  q.push ({ 0.0f, 0.0f, true, false });     // no delta: SDL sends nothing
  CHECK (q.take (0.0f, 3.0f, e));
  CHECK (e.precise);
  CHECK (q.take (0.0f, 1.0f, e));
  CHECK (!e.precise);
  CHECK_EQ (q.count, 0);
  // an event which went to no window of SDL is skipped; a scripted event
  // has no record
  q.push ({ 0.0f, -1.0f, false, false });   // over a menu of the system
  q.push ({ 0.5f, 0.0f, true, true });      // cancelled
  CHECK (q.take (0.5f, 0.0f, e));
  CHECK (e.cancelled);
  CHECK_EQ (q.count, 0);
  CHECK (!q.take (0.0f, -1.0f, e));
  // the oldest records go when there are too many
  for (int i= 0; i < qt_wheel_queue::max + 5; i++)
    q.push ({ 0.0f, (float) (i + 1), false, false });
  CHECK_EQ (q.count, (int) qt_wheel_queue::max);
  CHECK (!q.take (0.0f, 1.0f, e));
  CHECK (q.take (0.0f, (float) (qt_wheel_queue::max + 5), e));
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
  RUN (test_sdl_delta);
  RUN (test_queue);
  return test_report ();
}
