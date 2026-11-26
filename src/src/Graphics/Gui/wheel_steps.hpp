
/******************************************************************************
* MODULE     : wheel_steps.hpp
* DESCRIPTION: the scrolling of the wheel of the Qt port, for the other ports
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WHEEL_STEPS_H
#define WHEEL_STEPS_H

// How the Qt port scrolls an editor with the wheel, so that the Cocoa and
// Vue ports scroll the same distances (issue #971). The editor is a
// QAbstractScrollArea (QTMScrollView), whose scroll bars have a single step
// of a sixteenth of the viewport plus a pixel and a page step of the
// viewport (QTMScrollView::updateScrollBars); QAbstractScrollArea::wheelEvent
// gives the event to the bar of its dominant axis (the larger angle delta),
// and QScrollBar::wheelEvent scrolls it by QAbstractSliderPrivate::
// scrollByDelta: wheelScrollLines (3) single steps per 120 of the angle
// delta (a notch), the fractions of a step carried to the next event, at
// most a page per event, at once (no animation). The angle delta comes,
// on macOS, from -[QNSView scrollWheel:]: a mouse wheel (no precise
// deltas) gives 120 per event whatever its acceleration, a trackpad gives
// twice its scrolling delta in points; the pixel delta is not used for
// scrolling (only by an editor which captures the wheel).

#include <math.h>

// the single step of a scroll bar of the Qt port for a viewport of n points
inline int
qt_wheel_single_step (int n) {
  return (n >> 4) + 1;
}

// the angle delta of -[QNSView scrollWheel:] for one axis: of the
// scrollingDelta (points) of a precise event, of the delta (lines) of the
// event of a mouse wheel
inline int
qt_wheel_mac_angle (double scrolling_delta, double delta, bool precise) {
  if (precise) return (int) (scrolling_delta * 2);
  int a= (int) (delta * 10000);
  return a < -120 ? -120 : (a > 120 ? 120 : a);
}

// is the horizontal axis the dominant one (QAbstractScrollArea::wheelEvent)?
inline bool
qt_wheel_horizontal (int angle_x, int angle_y) {
  return (angle_x < 0 ? -angle_x : angle_x) > (angle_y < 0 ? -angle_y : angle_y);
}

// QAbstractSliderPrivate::scrollByDelta for one scroll bar: the points to
// scroll for an angle delta (of the same sign: a positive delta scrolls
// towards the start), given the single step and the page step in points
struct qt_wheel_axis {
  double acc;
  qt_wheel_axis (): acc (0) {}
  // (paging: with control or shift, a page an event, scaled by the delta,
  // the fraction dropped)
  int scroll (int delta, int step, int page, bool paging= false) {
    double offset= ((double) delta) / 120.0;
    if (paging) {
      acc= 0;
      int n= (int) (offset * page);
      return n < -page ? -page : (n > page ? page : n);
    }
    double f= 3 * offset * step; // wheelScrollLines
    if (acc != 0 && offset / acc < 0) acc= 0; // the wheel changed direction
    if (acc != 0 && offset / acc < 0) acc= 0; // the wheel changed direction
    acc += f;
    int n= (int) acc;
    acc -= n;
    if (n < -page) n= -page;
    if (n > page) n= page;
    return n;
  }
  // the same for a scroll bar which can or cannot move the way of the
  // delta (QAbstractSlider: value > minimum, value < maximum): at its end
  // the fraction is dropped and the event is not taken (Qt passes it to the
  // parent widget); else the fraction is kept, also when less than a point
  // is scrolled (stepsToScroll == 0)
  int scroll_bar (int delta, int step, int page, bool can_move, bool& taken,
                  bool paging= false) {
    if (!can_move) { acc= 0; taken= false; return 0; }
    taken= true;
    return scroll (delta, step, page, paging);
  }
  void reset () { acc= 0; }
};

// the single step of a QScrollArea, in points (set by the constructor of
// QScrollArea, qscrollarea.cpp; the default of a QScrollBar is 1)
#define QT_WHEEL_AREA_STEP 20

// the points a QScrollArea scrolls for an angle delta, as a float (the
// scroll containers of Vue take fractions)
inline double
qt_wheel_area_points (int delta) {
  return 3.0 * delta / 120.0 * QT_WHEEL_AREA_STEP;
}

// The scroll events of AppKit behind the wheel events of SDL, kept so that
// the Vue port reads what SDL drops (vue_mac.mm): the precise flag, the
// deltas, the scrolling deltas and the phases; everything is computed from
// them as in -[QNSView scrollWheel:] and the Cocoa port, never from the
// deltas of SDL, whose formula changes between versions of SDL
struct qt_wheel_record {
  double dx, dy;   // deltaX, deltaY of the NSEvent
  double sx, sy;   // scrollingDeltaX, scrollingDeltaY
  bool precise;    // hasPreciseScrollingDeltas
  int mods;        // its modifiers (QT_WHEEL_SHIFT...)
  bool momentum;   // momentumPhase != None
  bool cancelled;  // a cancelled phase: Qt zeroes its deltas (an editor
                   // which captures the wheel gets a wheel of (0, 0))
  bool ignored;    // an ended phase which a momentum follows: Qt drops it
                   // (-[QNSView scrollWheel:])
};

// the pixel delta of -[QNSView scrollWheel:] for one axis (a QPoint: the
// points truncated): the scrolling delta, 20 points a line for a mouse
inline int
qt_wheel_mac_pixels (double scrolling_delta, bool precise) {
  return (int) (precise ? scrolling_delta : 20.0 * scrolling_delta);
}

// the zoom of QTMWidget::wheelEvent (Qt 6, with control, the command key
// of macOS): of the pixel delta when it has one, by its vertical part, else
// of the angle delta in degrees (an eighth of it), by the sixteenth root;
// 0 for no zoom (a pixel delta with no vertical part, where Qt zooms out by
// a factor 0), the sign for in (> 0) or out (< 0)
inline double
qt_wheel_zoom (int pix_x, int pix_y, int angle_x, int angle_y) {
  int v;
  if (pix_x != 0 || pix_y != 0) v= pix_y;
  else if (angle_x != 0 || angle_y != 0) {
    // (QPoint / 8: qRound, half away from zero)
    double d= angle_y / 8.0;
    v= (int) (d >= 0 ? d + 0.5 : d - 0.5);
  }
  else return 0;
  if (v == 0) return 0;
  double f= sqrt (sqrt (sqrt (sqrt ((double) (v < 0 ? -v : v)))));
  return v > 0 ? f : -f;
}

// The modifiers of a wheel event as Qt has them on macOS (command is
// control, control is meta): -[QNSView scrollWheel:] takes those of an
// event of a gesture or of a wheel, and keeps them during the momentum
// which follows it (momentumPhase != None), so that a key pressed while
// the view glides does not change what the glide does
enum { QT_WHEEL_SHIFT= 1, QT_WHEEL_CONTROL= 2, QT_WHEEL_ALT= 4,
       QT_WHEEL_META= 8 };

struct qt_wheel_latch {
  int mods;
  qt_wheel_latch (): mods (0) {}
  int update (int event_mods, bool momentum) {
    if (!momentum) mods= event_mods;
    return mods;
  }
};

// QTMWidget::wheelEvent zooms with control alone (keyboardModifiers () ==
// ControlModifier); QAbstractSliderPrivate::scrollByDelta pages with
// control or shift
inline bool qt_wheel_zooms (int mods) { return mods == QT_WHEEL_CONTROL; }
inline bool qt_wheel_pages (int mods) {
  return (mods & (QT_WHEEL_CONTROL | QT_WHEEL_SHIFT)) != 0; }

// QScrollBar::wheelEvent, for a wheel over a scroll bar: the angle by
// which the bar scrolls (a vertical wheel also scrolls a horizontal bar),
// or false when it ignores the event (a horizontal bar, a vertical wheel
// with some sideways part)
inline bool
qt_wheel_bar_angle (bool horizontal_bar, int angle_x, int angle_y, int& a) {
  bool hor= qt_wheel_horizontal (angle_x, angle_y);
  if (!hor && angle_x != 0 && horizontal_bar) return false;
  a= hor ? angle_x : angle_y;
  return true;
}

// the serial numbers of the wheel events of SDL wrap around: a is before b
inline bool
qt_wheel_serial_before (unsigned a, unsigned b) {
  return (int) (a - b) < 0;
}

// the records by the serial numbers which the wheel events of SDL carry
// (numbered by the port when SDL queues them, one clock for all), in the
// order of the events: a record is taken when its event is handled, and
// those of older events (handled without asking) are dropped then, so that
// no record outlives its event; the oldest go when there are too many
struct qt_wheel_queue {
  enum { max= 1024 };
  qt_wheel_record r[max];
  unsigned serial[max];
  int first, count;
  qt_wheel_queue (): first (0), count (0) {}
  void push (unsigned id, const qt_wheel_record& e) {
    if (count == max) { first= (first + 1) % max; count--; }
    r[(first + count) % max]= e;
    serial[(first + count) % max]= id;
    count++;
  }
  bool take (unsigned id, qt_wheel_record& e) {
    while (count > 0 && qt_wheel_serial_before (serial[first], id)) {
      first= (first + 1) % max;
      count--;
    }
    if (count == 0 || serial[first] != id) return false;
    e= r[first];
    first= (first + 1) % max;
    count--;
    return true;
  }
};

#endif // defined WHEEL_STEPS_H
