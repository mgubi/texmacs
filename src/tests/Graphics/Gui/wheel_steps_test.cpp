
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
  r.mods= 0; r.momentum= false;
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
  // root; a notch of a mouse gives 20 times its scrolling delta (a slow
  // notch, 0.1: 2 points, 2^(1/16) = 1.044; an accelerated notch more), a
  // trackpad its points
  double slow= qt_wheel_zoom (0, qt_wheel_mac_pixels (0.1, false), 0, 120);
  CHECK (fabs (slow - pow (2.0, 1.0 / 16)) < 1e-12);
  double fast= qt_wheel_zoom (0, qt_wheel_mac_pixels (1.0, false), 0, 120);
  CHECK (fabs (fast - pow (20.0, 1.0 / 16)) < 1e-12);
  CHECK (qt_wheel_zoom (0, -7, 0, -14) < 0);
  CHECK (fabs (qt_wheel_zoom (0, -16, 0, -32) + pow (16.0, 1.0 / 16)) < 1e-12);
  // no pixel delta: the angle in degrees; nothing at all: no zoom
  CHECK (fabs (qt_wheel_zoom (0, 0, 0, 120) - pow (15.0, 1.0 / 16)) < 1e-12);
  CHECK_EQ (qt_wheel_zoom (0, 0, 0, 0), 0.0);
  // a cancelled gesture (zeroed deltas) and a sideways pixel delta: none
  CHECK_EQ (qt_wheel_zoom (5, 0, 10, 0), 0.0);
  // the degrees are rounded as QPoint / 8 (qRound): 12 / 8 = 1.5 gives 2
  CHECK (fabs (qt_wheel_zoom (0, 0, 0, 12) - pow (2.0, 1.0 / 16)) < 1e-12);
  CHECK (fabs (qt_wheel_zoom (0, 0, 0, -12) + pow (2.0, 1.0 / 16)) < 1e-12);
  CHECK_EQ (qt_wheel_zoom (0, 0, 0, 3), 0.0);  // 0.375 gives 0
}

static void
test_modifiers () {
  // command (control in Qt) alone zooms an editor; command or shift, with
  // any other keys, pages (an editor when it does not zoom, a bar, an area)
  CHECK (qt_wheel_zooms (QT_WHEEL_CONTROL));
  CHECK (!qt_wheel_zooms (QT_WHEEL_CONTROL | QT_WHEEL_SHIFT));
  CHECK (!qt_wheel_zooms (QT_WHEEL_CONTROL | QT_WHEEL_ALT));
  CHECK (!qt_wheel_zooms (QT_WHEEL_META));
  CHECK (qt_wheel_pages (QT_WHEEL_SHIFT));
  CHECK (qt_wheel_pages (QT_WHEEL_CONTROL | QT_WHEEL_SHIFT));
  CHECK (!qt_wheel_pages (QT_WHEEL_ALT));
  CHECK (!qt_wheel_pages (0));
  // the modifiers of a gesture hold during its momentum
  qt_wheel_latch l;
  CHECK_EQ (l.update (QT_WHEEL_CONTROL, false), (int) QT_WHEEL_CONTROL);
  CHECK_EQ (l.update (0, true), (int) QT_WHEEL_CONTROL);
  CHECK_EQ (l.update (QT_WHEEL_SHIFT, true), (int) QT_WHEEL_CONTROL);
  CHECK_EQ (l.update (0, false), 0);
}

static void
test_paging () {
  // scrollByDelta with control or shift: a page an event, scaled by the
  // delta, the fraction dropped
  qt_wheel_axis a;
  CHECK_EQ (a.scroll (120, 51, 800, true), 800);
  CHECK_EQ (a.scroll (-120, 51, 800, true), -800);
  CHECK_EQ (a.scroll (15, 51, 800, true), 100);   // 800 / 8
  CHECK_EQ (a.scroll (2400, 51, 800, true), 800);
  CHECK_EQ (a.scroll (15, 51, 800), 19);          // not paging: 19.125
  CHECK_EQ (a.scroll (15, 51, 800, true), 100);   // the 0.125 dropped
  CHECK_EQ (a.scroll (7, 51, 800), 8);            // 8.925, from nothing
  // less than a point of a page: the bar does not take the event (Qt
  // passes it to the parent widget)
  bool taken= true;
  CHECK_EQ (a.scroll_bar (1, 51, 80, true, taken, true), 0);
  CHECK (!taken);
  CHECK_EQ (a.scroll_bar (2, 51, 80, true, taken, true), 1);
  CHECK (taken);
}

static void
test_bar () {
  // QScrollBar::wheelEvent: a bar scrolls by the dominant angle, a vertical
  // wheel also scrolls a horizontal bar, but not with a sideways part
  int a= 0;
  CHECK (qt_wheel_bar_angle (false, 0, 120, a));
  CHECK_EQ (a, 120);
  CHECK (qt_wheel_bar_angle (true, 0, -120, a));
  CHECK_EQ (a, -120);
  CHECK (!qt_wheel_bar_angle (true, 4, 120, a));
  CHECK (qt_wheel_bar_angle (true, 120, 4, a));
  CHECK_EQ (a, 120);
  CHECK (qt_wheel_bar_angle (false, -60, 4, a));  // a horizontal wheel too
  CHECK_EQ (a, -60);
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

static void
test_linux_windows () {
  // the delta of SDL is the angle delta of Qt over 120 on Linux and
  // Windows: a notch (X11 buttons, Wayland value120 / 120, WM_MOUSEWHEEL /
  // 120), a wheel of high resolution (Windows: 30 of WM_MOUSEWHEEL)
  CHECK_EQ (qt_wheel_unit_angle (1.0, false), 120);
  CHECK_EQ (qt_wheel_unit_angle (-1.0, true), -120);
  CHECK_EQ (qt_wheel_unit_angle (0.25, false), 30);
  // a touchpad: X11 (XInput 2, valuator over increment), truncated as Qt
  // there; Wayland (pixels over 10, Qt 12 a pixel), rounded
  CHECK_EQ (qt_wheel_unit_angle (0.33, true), 39);     // 39.6
  CHECK_EQ (qt_wheel_unit_angle (0.33, false), 40);
  CHECK_EQ (qt_wheel_unit_angle (-0.33, true), -39);
  CHECK_EQ (qt_wheel_unit_angle (-0.33, false), -40);
  // a notch scrolls wheelScrollLines single steps: 3, or what Windows says
  qt_wheel_axis a;
  CHECK_EQ (a.scroll (120, 51, 800), 153);
  CHECK_EQ (a.scroll (120, 51, 800, false, 5), 255);
  CHECK_EQ (a.scroll (120, 51, 800, false, 1), 51);
  CHECK_EQ (qt_wheel_area_points (120, 5), 100.0);
  // the zoom with control: no pixel delta (X11, Windows, a wheel on
  // Wayland), by the degrees, 15 a notch; the fingers on Wayland, by
  // their pixels
  CHECK (fabs (qt_wheel_zoom (0, 0, 0, 120) - pow (15.0, 1.0 / 16)) < 1e-12);
  CHECK (fabs (qt_wheel_zoom (0, 7, 0, qt_wheel_unit_angle (0.7, false))
               - pow (7.0, 1.0 / 16)) < 1e-12);
}

static void
test_alt () {
  // alt in the signs of SDL (x to the right, y up): xcb and Wayland
  // transpose Qt's angle (x to the left), so a wheel turned up (y > 0)
  // scrolls left (x < 0), and a horizontal one to the right turns down
  int x= 0, y= 120;
  qt_wheel_alt_swap (x, y, false);
  CHECK_EQ (x, -120);
  CHECK_EQ (y, 0);
  x= 120; y= 0;
  qt_wheel_alt_swap (x, y, false);
  CHECK_EQ (x, 0);
  CHECK_EQ (y, -120);
  // Windows makes the vertical wheel horizontal the same way, and leaves
  // a horizontal one (WM_MOUSEHWHEEL) as it is
  x= 0; y= -120;
  qt_wheel_alt_swap (x, y, true);
  CHECK_EQ (x, 120);
  CHECK_EQ (y, 0);
  x= 120; y= 0;
  qt_wheel_alt_swap (x, y, true);
  CHECK_EQ (x, 120);
  CHECK_EQ (y, 0);
  // the deltas of SDL follow (an editor capturing the wheel)
  double dx= 0.25, dy= -1.5;
  qt_wheel_alt_swap (dx, dy, false);
  CHECK_EQ (dx, 1.5);
  CHECK_EQ (dy, -0.25);
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
  RUN (test_modifiers);
  RUN (test_paging);
  RUN (test_bar);
  RUN (test_linux_windows);
  RUN (test_alt);
  RUN (test_serial_wrap);
  return test_report ();
}
