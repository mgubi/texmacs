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

#include "wheel_steps.hpp"

static qt_wheel_queue wheel_queue;
static id wheel_monitor= nil;

void
vue_mac_wheel_monitor () {
  if (wheel_monitor != nil) return;
  wheel_monitor=
    [NSEvent addLocalMonitorForEventsMatchingMask: NSEventMaskScrollWheel
                                          handler: ^NSEvent* (NSEvent* e) {
      bool precise= [e hasPreciseScrollingDeltas];
      qt_wheel_record r= { qt_wheel_sdl_delta (- [e deltaX], precise),
                           qt_wheel_sdl_delta ([e deltaY], precise),
                           precise, [e phase] == NSEventPhaseCancelled };
      wheel_queue.push (r);
      return e;
    }];
}

bool
vue_mac_wheel_info (float x, float y, bool& precise, bool& cancelled) {
  qt_wheel_record r;
  if (!wheel_queue.take (x, y, r)) return false;
  precise= r.precise;
  cancelled= r.cancelled;
  return true;
}
