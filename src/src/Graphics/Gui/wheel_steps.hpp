
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
  int scroll (int delta, int step, int page, int lines= 3) {
    double offset= ((double) delta) / 120.0;
    double f= lines * offset * step;
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
  int scroll_bar (int delta, int step, int page, bool can_move, bool& taken) {
    if (!can_move) { acc= 0; taken= false; return 0; }
    taken= true;
    return scroll (delta, step, page);
  }
  void reset () { acc= 0; }
};

// the single step of a QScrollArea (of QScrollBar by default), in points
#define QT_WHEEL_AREA_STEP 20

// the points a QScrollArea scrolls for an angle delta, as a float (the
// scroll containers of Vue take fractions)
inline double
qt_wheel_area_points (int delta) {
  return 3.0 * delta / 120.0 * QT_WHEEL_AREA_STEP;
}

#endif // defined WHEEL_STEPS_H
