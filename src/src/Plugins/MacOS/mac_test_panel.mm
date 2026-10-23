/******************************************************************************
* MODULE     : mac_test_panel.mm
* DESCRIPTION: The file panels of macOS in the scripted tests of the interface
* COPYRIGHT  : (C) 2026  The TeXmacs Upsilon developers
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gui_test.hpp"
#include "analyze.hpp"
#include "MacOS/mac_cocoa.h"

/******************************************************************************
* The panels to open and save files are shown by a service of the system:
* their contents are drawn out of the process, out of reach of the events
* of a test. A test asks AppKit for its panels of the process instead
* (NSUseRemoteSavePanel), its other implementation, with the same name
* field, buttons and alert to replace a file, which the test types in and
* presses. Every port shows these panels on macOS (Cocoa, Qt, Vue by SDL),
* so the hooks of gui_test.hpp for the file dialogs are here for all.
******************************************************************************/

void
mac_test_panel_init () {
  // the panel of the process keeps its settings under the identifier of the
  // application, and throws without one: texmacs.bin run outside of an
  // application (as the tests do) gets one of its own for the tests
  NSBundle* main= [NSBundle mainBundle];
  if ([main bundleIdentifier] == nil &&
      [[main infoDictionary] isKindOfClass: [NSMutableDictionary class]])
    [(NSMutableDictionary*) [main infoDictionary]
      setObject: @"org.texmacs.upsilon.test" forKey: @"CFBundleIdentifier"];
  [[NSUserDefaults standardUserDefaults]
    registerDefaults: [NSDictionary dictionaryWithObject: @NO
                                                  forKey: @"NSUseRemoteSavePanel"]];
}

static NSString*
to_ns (string s) {
  c_string cs (s);
  return [NSString stringWithUTF8String: cs];
}

static string
from_ns (NSString* s) {
  return s? string ([s UTF8String]): string ("");
}

static NSWindow*
shown_panel () {
  // (NSSavePanel hands over to the window of its implementation, an
  // NSLocalSavePanel or NSLocalOpenPanel, of NSPanel, or its remote one)
  for (NSWindow* w in [NSApp windows]) {
    NSString* c= NSStringFromClass ([w class]);
    if ([w isVisible] &&
        ([c rangeOfString: @"SavePanel"].location != NSNotFound ||
         [c rangeOfString: @"OpenPanel"].location != NSNotFound))
      return w;
  }
  return nil;
}

static bool
remote (NSWindow* p) {
  // the panel of the service: its window here holds no controls
  return [NSStringFromClass ([p class]) rangeOfString: @"Remote"].location
         != NSNotFound;
}

static bool
shown (NSView* v) {
  return ![v isHiddenOrHasHiddenAncestor] && [v window] != nil;
}

static bool
name_field_like (NSView* v) {
  // the editable field of the name (not the tags, not the search)
  return [v isKindOfClass: [NSTextField class]] &&
         ![v isKindOfClass: [NSTokenField class]] &&
         ![v isKindOfClass: [NSSearchField class]] &&
         [(NSTextField*) v isEditable];
}

static bool
usable (NSButton* b) {
  // the panel of the process disables its buttons while it is not the key
  // window, as when the application is not the active one, which a test
  // does not make it (it would take the keyboard from the user): they are
  // taken as they are when it is
  return [b isEnabled] || ![[b window] isKeyWindow];
}

static void
collect (NSView* v, string pre, array<string>& r) {
  if (!shown (v)) return;
  if ([v isKindOfClass: [NSPopUpButton class]]) {
    string t= from_ns ([(NSPopUpButton*) v titleOfSelectedItem]);
    if (N(t) > 0) r << (pre * "popup\t" * t);
    return;
  }
  if ([v isKindOfClass: [NSButton class]]) {
    string t= from_ns ([(NSButton*) v title]);
    if (N(t) > 0)
      r << (pre * (usable ((NSButton*) v)? "button\t": "disabled\t") * t);
    return;
  }
  if (name_field_like (v))
    r << (pre * "field\t" * from_ns ([(NSTextField*) v stringValue]));
  else if ([v isKindOfClass: [NSTextField class]]) {
    string t= from_ns ([(NSTextField*) v stringValue]);
    // (the panel keeps texts which are not shown, with this one)
    if (N(t) > 0 && t != "<<Do not localize>>") r << (pre * "label\t" * t);
  }
  for (NSView* s in [v subviews]) collect (s, pre, r);
}

static NSView*
find_view (NSView* v, bool (*pred) (NSView*, NSString*), NSString* arg) {
  if (!shown (v)) return nil;
  if (pred (v, arg)) return v;
  for (NSView* s in [v subviews]) {
    NSView* f= find_view (s, pred, arg);
    if (f) return f;
  }
  return nil;
}

static bool
is_name_field (NSView* v, NSString* arg) {
  (void) arg;
  return name_field_like (v);
}

static bool
is_button (NSView* v, NSString* title) {
  return [v isKindOfClass: [NSButton class]] &&
         ![v isKindOfClass: [NSPopUpButton class]] &&
         [[(NSButton*) v title] isEqualToString: title];
}

array<string>
gui_test_file_dialog () {
  array<string> r;
  NSWindow* p= shown_panel ();
  if (!p) return r;
  if (remote (p)) {
    r << string ("remote\tthe panel of the system service, out of reach");
    return r;
  }
  r << ("title\t" * from_ns ([p title]));
  collect ([p contentView], "", r);
  if (NSWindow* sheet= [p attachedSheet])
    collect ([sheet contentView], "alert-", r);
  return r;
}

static void
type_text (NSWindow* w, string text) {
  // key events, one per character, to the focus of the window, as typed
  NSString* s= to_ns (text);
  for (NSUInteger i= 0; i < [s length]; i++) {
    NSString* c= [s substringWithRange: NSMakeRange (i, 1)];
    for (int down= 1; down >= 0; down--) {
      NSEvent* e= [NSEvent keyEventWithType: down? NSEventTypeKeyDown:
                                                   NSEventTypeKeyUp
                                   location: NSZeroPoint
                              modifierFlags: 0
                                  timestamp: [[NSProcessInfo processInfo]
                                               systemUptime]
                               windowNumber: [w windowNumber]
                                    context: nil
                                 characters: c
                charactersIgnoringModifiers: c
                                  isARepeat: NO
                                    keyCode: 0];
      [w sendEvent: e];
    }
  }
}

int
gui_test_file_dialog_do (string line) {
  // folder <path>  the folder the panel shows (the only change which is not
  //                made as a user would: it is not what the tests test)
  // name <text>    the name typed in the field of the name, instead of the
  //                name there
  // type <text>    keys typed to the focus (a list of files selects by them)
  // press <title>  press the button of the alert over the panel, else of
  //                the panel
  // 0 done, 1 not yet (no panel, or the button is disabled: the panel
  // checks a name typed a moment later), 2 impossible; "supported" is 0
  if (line == "supported") return 0;
  NSWindow* p= shown_panel ();
  if (!p) return 1;
  if (remote (p)) return 2;
  int i= search_forwards (" ", line);
  string cmd= (i < 0? line: line (0, i));
  string arg= (i < 0? string (""): line (i + 1, N(line)));
  NSWindow* sheet= [p attachedSheet];
  if (cmd == "folder" && !sheet &&
      [p respondsToSelector: @selector(setDirectoryURL:)]) {
    [(NSSavePanel*) p setDirectoryURL: [NSURL fileURLWithPath: to_ns (arg)]];
    return 0;
  }
  if (cmd == "name" && !sheet) {
    NSView* f= find_view ([p contentView], is_name_field, nil);
    if (!f || ![p makeFirstResponder: f]) return 2;
    NSText* editor= [p fieldEditor: NO forObject: f];
    if (!editor) return 2;
    [editor selectAll: nil];
    type_text (p, arg);
    return 0;
  }
  if (cmd == "type" && !sheet) {
    type_text (p, arg);
    return 0;
  }
  if (cmd == "press") {
    NSView* b= nil;
    if (sheet) b= find_view ([sheet contentView], is_button, to_ns (arg));
    else b= find_view ([p contentView], is_button, to_ns (arg));
    if (!b) return 2;
    if (!usable ((NSButton*) b)) return 1;
    // what a click does (performClick does nothing for a button disabled
    // because the application is not active)
    NSButton* button= (NSButton*) b;
    if ([button isEnabled]) [button performClick: nil];
    else [NSApp sendAction: [button action] to: [button target] from: button];
    return 0;
  }
  return 2;
}
