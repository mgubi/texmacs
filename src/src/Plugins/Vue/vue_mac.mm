/******************************************************************************
* MODULE     : vue_mac.mm
* DESCRIPTION: The document of a window of the Vue port in the title bar of
*              macOS: proxy icon, path menu and edited state
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// A window of macOS which shows a file has the icon of the file next to its
// title (which can be dragged), a menu of the folders of the file on a
// Cmd-click on the title, and a dot in its close button while the document
// has unsaved changes. AppKit draws all of it from the represented URL and
// the edited flag of the NSWindow, which SDL lets us reach; the Qt port
// gets the same from setWindowFilePath and setWindowModified, the Cocoa
// port from setRepresentedFilename and setDocumentEdited (ns_tm_widget.mm).

#include "vue_mac.hpp"
#import <AppKit/AppKit.h>
#include <stdio.h>

void
vue_mac_set_document (void* ns_window, const char* path, bool edited) {
  if (ns_window == NULL) return;
  // (CMake compiles this file without ARC: the objects made here go with
  // the pool)
  @autoreleasepool {
    NSWindow* win= (__bridge NSWindow*) ns_window;
    if (![win isKindOfClass: [NSWindow class]]) return;
    NSURL* u= nil;
    if (path != NULL && path[0] != '\0') {
      NSString* s= [NSString stringWithUTF8String: path];
      if (s != nil) u= [NSURL fileURLWithPath: s];
    }
    NSURL* old= [win representedURL];
    if (!(u == old || (u != nil && old != nil && [u isEqual: old])))
      [win setRepresentedURL: u];
    if ([win isDocumentEdited] != (edited ? YES : NO))
      [win setDocumentEdited: edited ? YES : NO];
  }
}

const char*
vue_mac_document_state (void* ns_window) {
  static char buf[4096];
  if (ns_window == NULL) {
    snprintf (buf, sizeof (buf), "no NSWindow");
    return buf;
  }
  @autoreleasepool {
    NSWindow* win= (__bridge NSWindow*) ns_window;
    NSURL* u= [win representedURL];
    const char* p= (u != nil) ? [[u path] UTF8String] : "(nil)";
    snprintf (buf, sizeof (buf), "representedURL=%s documentEdited=%s",
              p != NULL ? p : "(nil)", [win isDocumentEdited] ? "yes" : "no");
  }
  return buf;
}

/******************************************************************************
* The precise deltas and the phases of the scroll events (issue #971)
******************************************************************************/

static qt_wheel_queue wheel_queue;
static qt_wheel_record wheel_last;    // of the last scroll event
static bool wheel_last_valid= false;
static bool wheel_last_ended= false;  // its phase is NSEventPhaseEnded
static id wheel_monitor= nil;

void
vue_mac_wheel_monitor () {
  if (wheel_monitor != nil) return;
  wheel_monitor=
    [NSEvent addLocalMonitorForEventsMatchingMask: NSEventMaskScrollWheel
                                          handler: ^NSEvent* (NSEvent* e) {
      qt_wheel_record r;
      r.dx= [e deltaX]; r.dy= [e deltaY];
      r.sx= [e scrollingDeltaX]; r.sy= [e scrollingDeltaY];
      r.precise= [e hasPreciseScrollingDeltas];
      r.ignored= ([e phase] == NSEventPhaseCancelled);
      wheel_last= r;
      wheel_last_valid= true;
      wheel_last_ended= ([e phase] == NSEventPhaseEnded);
      return e;
    }];
}

void
vue_mac_wheel_attach (float x, float y, unsigned long long stamp) {
  if (!wheel_last_valid) return; // an event of a script
  wheel_last_valid= false;
  if (!wheel_last.matches (x, y)) return;
  qt_wheel_record r= wheel_last;
  // -[QNSView scrollWheel:]: an ended phase which a momentum follows does
  // not scroll (what the queue has now: this is called while AppKit hands
  // the ended event to SDL)
  if (wheel_last_ended) {
    NSEvent* n= [NSApp nextEventMatchingMask: NSEventMaskScrollWheel
                                   untilDate: [NSDate distantPast]
                                      inMode: @"TMMomentumEventSearchMode"
                                     dequeue: NO];
    if (n != nil && [n momentumPhase] == NSEventPhaseBegan) r.ignored= true;
  }
  wheel_queue.push (stamp, r);
}

bool
vue_mac_wheel_take (unsigned long long stamp, qt_wheel_record& r) {
  return wheel_queue.take (stamp, r);
}
