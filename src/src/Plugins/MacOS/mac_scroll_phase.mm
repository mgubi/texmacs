/******************************************************************************
 * MODULE     : mac_scroll_phase.mm
 * DESCRIPTION: phases of the trackpad scroll gestures (for SDL based GUIs)
 * COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "mac_scroll_phase.h"
#import <Cocoa/Cocoa.h>
#include <math.h>

struct mac_scroll_entry { int phase; double x, y; };
static const int mac_scroll_capacity= 256;
static mac_scroll_entry mac_scroll_queue[mac_scroll_capacity];
static int  mac_scroll_head= 0, mac_scroll_count= 0;
static bool mac_scroll_fingers= false;
static bool mac_scroll_released= false;
static id   mac_scroll_monitor= nil;

static void
mac_scroll_push (int phase, double x, double y) {
  if (mac_scroll_count == mac_scroll_capacity) { // overflow: forget the oldest
    mac_scroll_head= (mac_scroll_head + 1) % mac_scroll_capacity;
    mac_scroll_count--;
  }
  int i= (mac_scroll_head + mac_scroll_count) % mac_scroll_capacity;
  mac_scroll_queue[i]= (mac_scroll_entry) { phase, x, y };
  mac_scroll_count++;
}

void
mac_scroll_monitor_start () {
  if (mac_scroll_monitor != nil) return;
  mac_scroll_monitor= [NSEvent addLocalMonitorForEventsMatchingMask: NSEventMaskScrollWheel
    handler: ^NSEvent* (NSEvent* e) {
      NSEventPhase phase= [e phase];
      NSEventPhase momentum= [e momentumPhase];
      int kind= MAC_SCROLL_WHEEL;
      if (momentum != NSEventPhaseNone) kind= MAC_SCROLL_MOMENTUM;
      else if (phase & (NSEventPhaseMayBegin | NSEventPhaseBegan |
                        NSEventPhaseChanged | NSEventPhaseStationary))
        kind= MAC_SCROLL_FINGERS;
      else if (phase & (NSEventPhaseEnded | NSEventPhaseCancelled))
        kind= MAC_SCROLL_RELEASE;
      if (kind == MAC_SCROLL_FINGERS) {
        mac_scroll_fingers= true;
        mac_scroll_released= false;
      }
      else if (kind == MAC_SCROLL_RELEASE) {
        mac_scroll_fingers= false;
        mac_scroll_released= true;
      }
      double x= -[e deltaX], y= [e deltaY];
      if (x != 0 || y != 0) mac_scroll_push (kind, x, y); // SDL drops the others
      return e;
    }];
}

int
mac_scroll_phase_pop (double x, double y) {
  while (mac_scroll_count > 0) {
    mac_scroll_entry e= mac_scroll_queue[mac_scroll_head];
    mac_scroll_head= (mac_scroll_head + 1) % mac_scroll_capacity;
    mac_scroll_count--;
    // SDL rounds the deltas of the wheels of conventional mice to full ticks
    // and may flip the signs: match loosely
    if (fabs (fabs (e.x) - fabs (x)) < 1.0 && fabs (fabs (e.y) - fabs (y)) < 1.0)
      return e.phase;
  }
  return MAC_SCROLL_UNKNOWN;
}

bool
mac_scroll_fingers_down () {
  return mac_scroll_fingers;
}

bool
mac_scroll_take_release () {
  if (!mac_scroll_released || mac_scroll_count > 0) return false;
  mac_scroll_released= false;
  return true;
}
