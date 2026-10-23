/******************************************************************************
* MODULE     : ns_test.mm
* DESCRIPTION: The hooks of the scripted tests for the Cocoa interface
* COPYRIGHT  : (C) 2026  The TeXmacs Upsilon developers
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_cocoa.h"
#include "ns_utilities.h"
#include "ns_gui.h"
#include "gui_test.hpp"
#include "scheme.hpp"
#include "analyze.hpp"
#include "sys_utils.hpp"
#import "TMView.h"
#import <objc/runtime.h>

/******************************************************************************
* The driver (check gui-driver) runs its steps from a timer (gui_test_pump),
* which also fires while a menu is tracked and while a dialog is modal. The
* input is posted. Positions are in points from the top left of the content
* of the window of the script.
*
* The menus of Cocoa are native: the menu bar and the menus of the buttons
* (popUpMenuPositioningItem) give no place to their items. Their items are
* listed without a box (gui_test_texts), and the driver asks for them to be
* chosen ("choose <n>"): a submenu is opened (it is listed from then on), an
* item runs and the menus close. The menus listed are those of the menu bar,
* the one a button has open (NSMenuDidBeginTrackingNotification), and the
* submenus chosen in them.
******************************************************************************/

void ns_draw_gpu_canvases (NSView* root, NSBitmapImageRep* rep); // ns_gui.mm

static NSWindow* script_win= nil;         // not retained: checked in the list
static bool script_no_target= false;      // the last "window" matched nothing
static string popup_path;                 // the path of the last click
static NSMutableArray* open_menus= nil;   // the menus listed
static array<string> open_paths;          // and their paths
static NSMenu* tracked_menu= nil;         // the menu of a button, while open
                                          // (retained)
static NSMutableArray* listed_items= nil; // the items of the last list
static bool menu_closing= false;          // a menu was cancelled, not ended

string gui_test_port () { return "cocoa"; }

static string
normalize (string s) {
  // (the menus View and Help are written with a Cyrillic e on macOS, so
  // that the system does not add its items to them)
  return replace (replace (trim_spaces (s), "<ldots>", "..."), "<#435>", "e");
}

static bool
alive (NSWindow* w) {
  return w != nil && [[NSApp windows] containsObject: w] && [w isVisible];
}

static NSView*
find_canvas (NSView* v) {
  if ([v isKindOfClass: [TMView class]]) return v;
  for (NSView* sub in [v subviews]) {
    NSView* r= find_canvas (sub);
    if (r) return r;
  }
  return nil;
}

static NSString*
first_label (NSView* v) {
  if ([v isKindOfClass: [NSTextField class]] && ![(NSTextField*) v isEditable] &&
      [[(NSTextField*) v stringValue] length] > 0)
    return [(NSTextField*) v stringValue];
  for (NSView* sub in [v subviews]) {
    NSString* s= first_label (sub);
    if (s) return s;
  }
  return nil;
}

static string
window_title (NSWindow* w) {
  // its title, or for a window without one (an alert, whose message says
  // what it is: "Enter data" of the inputs of ns_dialogues.mm) its first
  // label
  NSString* t= [w title];
  if ([t length] == 0) t= first_label ([w contentView]);
  return t? from_nsstring (t): string ("");
}

static NSWindow*
main_window () {
  // the editor: the first window with a canvas of TeXmacs
  NSWindow* key= [NSApp mainWindow];
  if (alive (key) && find_canvas ([key contentView])) return key;
  for (NSWindow* w in [NSApp orderedWindows])
    if (alive (w) && find_canvas ([w contentView])) return w;
  return nil;
}

static NSWindow*
script_target () {
  if (script_no_target) return nil;
  if (alive (script_win)) return script_win;
  NSWindow* w= [NSApp modalWindow];
  if (!alive (w)) w= [NSApp keyWindow];
  if (!alive (w)) w= main_window ();
  return w;
}

/******************************************************************************
* Texts and their paths
******************************************************************************/

static string join_path (string a, string b) {
  if (N(a) == 0) return b;
  if (N(b) == 0) return a;
  return a * " > " * b;
}

static string
named_of (id obj) {
  // the names of the (named ...) of the menus around a view or a menu item,
  // which ns_ui_element_rep gives it (and TMButtonsController its buttons):
  // the outer first, as "#outer > #inner"
  NSString* s= obj? objc_getAssociatedObject (obj, @selector(tmNamed)): nil;
  if (!s) return "";
  array<string> l= tokenize (from_nsstring (s), "\t");
  string r;
  for (int i= 0; i < N(l); i++)
    if (starts (l[i], "named:"))
      r= (N(r) == 0 ? r : r * " > ") * "#" * l[i] (6, N(l[i]));
  return r;
}

static string
icon_of (id obj) {
  // "icon:<name of its file>" of an image or a button (to_nsimage)
  NSString* s= obj? objc_getAssociatedObject (obj, @selector(tmIconName)): nil;
  return s? from_nsstring (s): string ("");
}

static string
view_path (NSView* v) {
  // the bars (the identifiers which TMButtonsController gives its rows)
  // and the names of the (named ...) of the menus, its own included
  string r= named_of (v);
  for (NSView* p= [v superview]; p != nil; p= [p superview]) {
    NSString* id= [p identifier];
    if (id && ([id isEqualToString: @"mainbar"] || [id isEqualToString: @"modebar"] ||
               [id isEqualToString: @"focusbar"] || [id isEqualToString: @"userbar"] ||
               [id isEqualToString: @"footer"]))
      r= join_path (from_nsstring (id), r);
    else r= join_path (named_of (p), r);
  }
  return r;
}

static string
view_label (NSView* v) {
  if ([v isKindOfClass: [NSButton class]]) {
    NSButton* b= (NSButton*) v;
    if (N(icon_of (b)) > 0) return icon_of (b);
    string t= normalize (from_nsstring ([b title]));
    if (N(t) > 0) return t;
    return icon_of ([b image]);
  }
  if ([v isKindOfClass: [NSTextField class]])
    return normalize (from_nsstring ([(NSTextField*) v stringValue]));
  return "";
}

static void
add_text (array<string>& r, NSRect b, string path, string text) {
  if (N(text) == 0) return;
  r << (as_string ((int) b.origin.x) * "\t" * as_string ((int) b.origin.y) *
        "\t" * as_string ((int) b.size.width) * "\t" *
        as_string ((int) b.size.height) * "\t" * path * "\t" * text);
}

static NSRect
script_rect (NSWindow* win, NSView* v, NSRect r) {
  // a rectangle of a view in the points of the script (from the top left)
  NSRect w= [v convertRect: r toView: nil];
  CGFloat h= [[win contentView] frame].size.height;
  return NSMakeRect (w.origin.x, h - NSMaxY (w), w.size.width, w.size.height);
}

static NSString*
row_text (NSTableView* t, NSInteger row) {
  // the text of a row of a list or of a tree (TMChoiceList, the tree view)
  NSTableColumn* col= [[t tableColumns] firstObject];
  id ds= [t dataSource];
  id v= nil;
  // what the row shows (a view-based table), else what its data source has
  NSView* cv= [t viewAtColumn: 0 row: row makeIfNecessary: NO];
  if ([cv isKindOfClass: [NSTableCellView class]])
    v= [[(NSTableCellView*) cv textField] stringValue];
  else if ([cv isKindOfClass: [NSTextField class]])
    v= [(NSTextField*) cv stringValue];
  if (v) ;
  else if ([t isKindOfClass: [NSOutlineView class]]) {
    id item= [(NSOutlineView*) t itemAtRow: row];
    if ([ds respondsToSelector: @selector(outlineView:objectValueForTableColumn:byItem:)])
      v= [ds outlineView: (NSOutlineView*) t objectValueForTableColumn: col
                  byItem: item];
  }
  else if ([ds respondsToSelector: @selector(tableView:objectValueForTableColumn:row:)])
    v= [ds tableView: t objectValueForTableColumn: col row: row];
  if ([v isKindOfClass: [NSAttributedString class]]) v= [v string];
  return [v isKindOfClass: [NSString class]]? (NSString*) v: nil;
}

static void
collect_texts (NSWindow* win, NSView* v, array<string>& r) {
  if ([v isHidden]) return;
  if ([v isKindOfClass: [NSTableView class]]) {
    // the rows in view, in their places (not the views of their cells)
    NSTableView* t= (NSTableView*) v;
    NSRange rows= [t rowsInRect: [t visibleRect]];
    for (NSUInteger i= rows.location; i < NSMaxRange (rows) && i < rows.location + 1000; i++) {
      NSString* txt= row_text (t, i);
      if (txt)
        add_text (r, script_rect (win, t, [t frameOfCellAtColumn: 0 row: i]),
                  view_path (t), normalize (from_nsstring (txt)));
    }
    return;
  }
  if ([v isKindOfClass: [NSTabView class]]) {
    // the tabs, without a box (their places are not told): chosen by the
    // port (choose), as the items of the native menus
    for (NSTabViewItem* it in [(NSTabView*) v tabViewItems]) {
      r << (as_string ((int) [listed_items count]) * "\t0\t0\t0\t" *
            view_path (v) * "\t" * normalize (from_nsstring ([it label])));
      [listed_items addObject: it];
    }
  }
  if ([v isKindOfClass: [NSSegmentedControl class]]) {
    NSSegmentedControl* sc= (NSSegmentedControl*) v;
    NSInteger n= [sc segmentCount];
    CGFloat x= 0, w= [sc bounds].size.width;
    for (NSInteger i=0; i<n; i++) {
      CGFloat sw= [sc widthForSegment: i];
      if (sw <= 0) sw= w / MAX (n, 1);
      add_text (r, script_rect (win, sc, NSMakeRect (x, 0, sw, [sc bounds].size.height)),
                view_path (sc), normalize (from_nsstring ([sc labelForSegment: i])));
      x += sw;
    }
  }
  else {
    string l= view_label (v);
    if (N(l) > 0) add_text (r, script_rect (win, v, [v bounds]), view_path (v), l);
  }
  for (NSView* sub in [v subviews]) collect_texts (win, sub, r);
}

static void
populate (NSMenu* m) {
  // the menus of TeXmacs are made when they open
  if ([m delegate] && [[m delegate] respondsToSelector: @selector(menuNeedsUpdate:)])
    [[m delegate] menuNeedsUpdate: m];
}

static void
list_menu (NSMenu* m, string path, array<string>& r) {
  // the items of a menu, without a box: x is their place in listed_items
  populate (m);
  for (NSMenuItem* mi in [m itemArray]) {
    if ([mi isSeparatorItem] || [mi isHidden]) continue;
    string t= normalize (from_nsstring ([mi title]));
    if (N(t) == 0) t= icon_of ([mi image]);
    if (N(t) == 0) continue;
    r << (as_string ((int) [listed_items count]) * "\t0\t0\t0\t" *
          join_path (path, named_of (mi)) * "\t" * t);
    [listed_items addObject: mi];
  }
}

array<string>
gui_test_windows () {
  array<string> r;
  for (NSWindow* w in [NSApp windows])
    if (alive (w)) r << window_title (w);
  return r;
}

array<string>
gui_test_texts () {
  // the texts and icons of the window of the script, then the items of the
  // menus: the menu bar, then the menus open, the last opened last
  array<string> r;
  if (!listed_items) listed_items= [[NSMutableArray alloc] init];
  [listed_items removeAllObjects];
  NSWindow* win= script_target ();
  if (win) collect_texts (win, [win contentView], r);
  if ([NSApp mainMenu]) list_menu ([NSApp mainMenu], "menubar", r);
  if (open_menus)
    for (NSUInteger i=0; i<[open_menus count]; i++)
      list_menu ([open_menus objectAtIndex: i], open_paths[i], r);
  return r;
}

static void
open_menu (NSMenu* m, string path, NSMenu* parent) {
  // list m from now on, after its parent (the menus opened after the
  // parent are closed)
  if (!open_menus) open_menus= [[NSMutableArray alloc] init];
  NSUInteger keep= 0;
  for (NSUInteger i=0; i<[open_menus count]; i++)
    if ([open_menus objectAtIndex: i] == parent) keep= i + 1;
  while ([open_menus count] > keep) [open_menus removeLastObject];
  open_paths= range (open_paths, 0, (int) keep);
  [open_menus addObject: m];
  open_paths << path;
}

static string
menu_path (NSMenu* m) {
  if (m == [NSApp mainMenu]) return "menubar";
  if (open_menus)
    for (NSUInteger i=0; i<[open_menus count]; i++)
      if ([open_menus objectAtIndex: i] == m) return open_paths[i];
  return "";
}

static int
choose (NSMenuItem* mi) {
  NSMenu* m= [mi menu];
  if (!m) {
    // the menu was made again since the list: the item is no longer there
    cout << "cocoa script: the item is no longer in its menu" << LF;
    return 2;
  }
  if (![mi isEnabled]) {
    cout << "cocoa script: the item " << from_nsstring ([mi title])
         << " is disabled" << LF;
    return 2;
  }
  if ([mi hasSubmenu]) {
    string t= normalize (from_nsstring ([mi title]));
    open_menu ([mi submenu], join_path (join_path (menu_path (m),
                                                   named_of (mi)), t), m);
    return 0;
  }
  NSInteger i= [m indexOfItem: mi];
  if (i < 0) return 2;
  [m performActionForItemAtIndex: i];
  // the menus close, as after a choice
  if (tracked_menu) {
    menu_closing= true; // the end comes later: the script waits for it
    [tracked_menu cancelTracking];
  }
  if (open_menus) [open_menus removeAllObjects];
  open_paths= array<string> ();
  return 0;
}

@interface TMTestHelper : NSObject
- (void) tick: (NSTimer*) timer;
- (void) beginTracking: (NSNotification*) n;
- (void) endTracking: (NSNotification*) n;
@end

@implementation TMTestHelper
- (void) tick: (NSTimer*) timer
{
  (void) timer;
  static bool running= false; // not again while a step runs (a nested loop)
  if (running) return;
  running= true;
  call ("gui-test-pump");
  running= false;
}
- (void) beginTracking: (NSNotification*) n
{
  // a menu of a button opens (popUpMenuPositioningItem): its path is the
  // place of the button the script clicked
  NSMenu* m= [n object];
  // (not the menus of the menu bar, those of a button: the submenu of
  // the item of its row, which is not in the menu bar)
  for (NSMenu* p= m; p != nil; p= [p supermenu])
    if (p == [NSApp mainMenu]) return;
  [m retain];
  [tracked_menu release];
  tracked_menu= m;
  if (!open_menus) open_menus= [[NSMutableArray alloc] init];
  [open_menus removeAllObjects];
  [open_menus addObject: m];
  open_paths= array<string> ();
  open_paths << popup_path;
}
- (void) endTracking: (NSNotification*) n
{
  if ([n object] != tracked_menu) return;
  [tracked_menu release];
  tracked_menu= nil;
  menu_closing= false;
  if (open_menus) [open_menus removeAllObjects];
  open_paths= array<string> ();
}
@end

/******************************************************************************
* The commands of the scripts
******************************************************************************/

static void
post_mouse (NSWindow* win, NSEventType type, NSPoint p, int clicks) {
  NSEvent* e= [NSEvent mouseEventWithType: type location: p modifierFlags: 0
                                timestamp: [[NSProcessInfo processInfo] systemUptime]
                             windowNumber: [win windowNumber] context: nil
                              eventNumber: 0 clickCount: clicks
                                 pressure: (type == NSEventTypeLeftMouseUp ||
                                            type == NSEventTypeRightMouseUp)? 0.0: 1.0];
  [NSApp postEvent: e atStart: NO];
}

static NSPoint
window_point (NSWindow* win, array<string> a) {
  // the point of the script in the coordinates of the window (from below)
  CGFloat h= [[win contentView] frame].size.height;
  return NSMakePoint (as_double (a[1]), h - as_double (a[2]));
}

static void
note_click (NSWindow* win, NSPoint p) {
  // the place of what is clicked: the path of the menu it may open
  NSView* v= [[[win contentView] superview] hitTest: p];
  if (!v) v= [[win contentView] hitTest: p];
  while (v && N(view_label (v)) == 0 && ![v isKindOfClass: [NSControl class]])
    v= [v superview];
  popup_path= v? join_path (view_path (v), view_label (v)): string ("");
}

static bool
select_row (NSWindow* win, NSPoint p) {
  // a click on a row of a list or of a tree: the row is selected, as with
  // the keyboard (a table takes no click in a window which is not the
  // active one, which a test does not make it); the selection runs the
  // command of the list
  NSView* v= [[[win contentView] superview] hitTest: p];
  while (v && ![v isKindOfClass: [NSTableView class]]) v= [v superview];
  if (!v) return false;
  NSTableView* t= (NSTableView*) v;
  NSInteger row= [t rowAtPoint: [t convertPoint: p fromView: nil]];
  if (row < 0) return false;
  [t selectRowIndexes: [NSIndexSet indexSetWithIndex: row]
       byExtendingSelection: NO];
  return true;
}

static bool
unkeyed_modal (NSWindow* win) {
  // a modal window or a panel without an editor (an alert, also before its
  // modal session began): its keys are done directly (modal_key). Posted,
  // they reach it only if it is the key window, which it is not while
  // TeXmacs is not the active application (a test does not make it), and
  // they did not always do their work when it was
  return [NSApp modalWindow] == win ||
         ([win isKindOfClass: [NSPanel class]] && !find_canvas ([win contentView]));
}

static NSArray*
editable_fields (NSWindow* win) {
  // the editable text fields of the window, in the order of the views
  NSMutableArray* fields= [NSMutableArray array];
  NSMutableArray* todo= [NSMutableArray arrayWithObject: [win contentView]];
  while ([todo count] > 0) {
    NSView* x= [todo objectAtIndex: 0];
    [todo removeObjectAtIndex: 0];
    if ([x isKindOfClass: [NSTextField class]] && [(NSTextField*) x isEditable])
      [fields addObject: x];
    [todo addObjectsFromArray: [x subviews]];
  }
  return fields;
}

static NSText*
field_editor (NSWindow* win) {
  // the editor of the text field of the window being edited, the first
  // editable one if none is
  id r= [win firstResponder];
  if ([r isKindOfClass: [NSText class]]) return (NSText*) r;
  NSArray* fields= editable_fields (win);
  if ([fields count] == 0) return nil;
  NSTextField* f= [r isKindOfClass: [NSTextField class]] && [fields containsObject: r]?
                  (NSTextField*) r: [fields objectAtIndex: 0];
  [win makeFirstResponder: f];
  [f selectText: nil];
  r= [win firstResponder];
  return [r isKindOfClass: [NSText class]]? (NSText*) r: nil;
}

static void
next_field (NSWindow* win) {
  // Tab: the field after the one being edited
  NSArray* fields= editable_fields (win);
  id r= [win firstResponder];
  id cur= [r isKindOfClass: [NSText class]]? [(NSText*) r delegate]: r;
  NSUInteger i= [fields indexOfObject: cur];
  if ([fields count] == 0) return;
  NSTextField* f= [fields objectAtIndex:
                    (i == NSNotFound)? 0: (i + 1) % [fields count]];
  [win makeFirstResponder: f];
  [f selectText: nil];
}

static bool
modal_key (NSWindow* win, string name, NSString* text) {
  // the keys of an unkeyed modal window, done as they would be: the text
  // in the field being edited, Backspace, Tab to the next field, Return
  // presses the default button, Escape cancels
  NSText* ed= field_editor (win);
  if (text != nil) { if (ed) [ed insertText: text]; return ed != nil; }
  if (name == "Backspace" && ed) { [ed deleteBackward: nil]; return true; }
  if (name == "Tab") { next_field (win); return true; }
  if (name == "Return") {
    if (ed) [[win firstResponder] doCommandBySelector: @selector(insertNewline:)];
    NSButtonCell* d= [win defaultButtonCell];
    if (d) [d performClick: nil];
    return true;
  }
  if (name == "Escape") { [NSApp abortModal]; return true; }
  return false;
}

static void
post_key (NSWindow* win, string name, string text) {
  // "[S-][C-][A-][M-]<name>": shift, control, option, command (as Vue)
  if (unkeyed_modal (win) &&
      modal_key (win, name, N(text) > 0? to_nsstring (text): nil)) return;
  NSEventModifierFlags mods= 0;
  while (N(name) > 2 && name[1] == '-') {
    if (name[0] == 'S') mods |= NSEventModifierFlagShift;
    else if (name[0] == 'C') mods |= NSEventModifierFlagControl;
    else if (name[0] == 'A') mods |= NSEventModifierFlagOption;
    else if (name[0] == 'M') mods |= NSEventModifierFlagCommand;
    else break;
    name= name (2, N(name));
  }
  unichar c= 0;
  unsigned short code= 0;
  if (name == "Return") { c= '\r'; code= 36; }
  else if (name == "Escape") { c= 27; code= 53; }
  else if (name == "Tab") { c= '\t'; code= 48; }
  else if (name == "Backspace") { c= 127; code= 51; }
  else if (name == "Delete") { c= NSDeleteFunctionKey; code= 117; }
  else if (name == "Space") { c= ' '; code= 49; }
  else if (name == "Left") { c= NSLeftArrowFunctionKey; code= 123; }
  else if (name == "Right") { c= NSRightArrowFunctionKey; code= 124; }
  else if (name == "Down") { c= NSDownArrowFunctionKey; code= 125; }
  else if (name == "Up") { c= NSUpArrowFunctionKey; code= 126; }
  else if (name == "Home") { c= NSHomeFunctionKey; code= 115; }
  else if (name == "End") { c= NSEndFunctionKey; code= 119; }
  else if (name == "PageUp") { c= NSPageUpFunctionKey; code= 116; }
  else if (name == "PageDown") { c= NSPageDownFunctionKey; code= 121; }
  else if (N(name) > 1 && name[0] == 'F' && is_int (name (1, N(name))) &&
           as_int (name (1, N(name))) >= 1 && as_int (name (1, N(name))) <= 35)
    c= NSF1FunctionKey + as_int (name (1, N(name))) - 1;
  else if (N(name) == 1) {
    c= (unichar) (unsigned char) name[0];
    if ((mods & NSEventModifierFlagShift) && c >= 'a' && c <= 'z') c += 'A' - 'a';
  }
  else {
    cout << "cocoa script: unknown key " << name << LF;
    return;
  }
  NSString* chars= N(text) > 0? to_nsstring (text):
                   [NSString stringWithCharacters: &c length: 1];
  NSString* plain= [NSString stringWithCharacters: &c length: 1];
  for (int up=0; up<2; up++) {
    NSEvent* e= [NSEvent keyEventWithType: up? NSEventTypeKeyUp: NSEventTypeKeyDown
                                 location: NSZeroPoint modifierFlags: mods
                                timestamp: [[NSProcessInfo processInfo] systemUptime]
                             windowNumber: [win windowNumber] context: nil
                               characters: chars charactersIgnoringModifiers: plain
                                isARepeat: NO keyCode: code];
    [NSApp postEvent: e atStart: NO];
  }
}

static void
post_text (NSWindow* win, string text) {
  NSString* all= to_nsstring (text);
  if (unkeyed_modal (win) && modal_key (win, "", all)) return;
  for (NSUInteger i=0; i<[all length]; ) {
    // a character as the user sees it (a pair of UTF-16 units, an accent)
    NSRange r= [all rangeOfComposedCharacterSequenceAtIndex: i];
    NSString* c= [all substringWithRange: r];
    i= NSMaxRange (r);
    for (int up=0; up<2; up++) {
      NSEvent* e= [NSEvent keyEventWithType: up? NSEventTypeKeyUp: NSEventTypeKeyDown
                                   location: NSZeroPoint modifierFlags: 0
                                  timestamp: [[NSProcessInfo processInfo] systemUptime]
                               windowNumber: [win windowNumber] context: nil
                                 characters: c charactersIgnoringModifiers: c
                                  isARepeat: NO keyCode: 0];
      [NSApp postEvent: e atStart: NO];
    }
  }
}

static void
snapshot (NSWindow* win, string name) {
  string dir= get_env ("TEXMACS_TEST_SNAPSHOT");
  if (N(dir) == 0) dir= get_env ("TEXMACS_NS_SNAPSHOT");
  if (N(dir) == 0) return;
  NSView* v= [win contentView];
  NSRect r= [v bounds];
  NSBitmapImageRep* rep= [v bitmapImageRepForCachingDisplayInRect: r];
  if (!rep) return;
  [v cacheDisplayInRect: r toBitmapImageRep: rep];
  ns_draw_gpu_canvases (v, rep);
  NSData* data= [rep representationUsingType: NSBitmapImageFileTypePNG
                                  properties: [NSDictionary dictionary]];
  [data writeToFile: to_nsstring (dir * "/" * name * ".png") atomically: NO];
}

int
gui_test_raw (string line) {
  line= trim_spaces (line);
  if (N(line) == 0) return 1;
  array<string> a= tokenize (line, " ");
  string cmd= a[0];
  string rest= N(line) > N(cmd)? line (N(cmd) + 1, N(line)): string ("");
  if (cmd == "window" && N(a) > 1) {
    // "window main" (the editor), or a part of the title of a window
    script_win= nil;
    if (rest == "main") script_win= main_window ();
    else
      // (all the windows: a modal alert is not among the ordered ones)
      for (NSWindow* w in [NSApp windows])
        if (alive (w) && occurs (rest, window_title (w))) {
          script_win= w;
          break;
        }
    script_no_target= (script_win == nil);
    if (script_no_target) {
      cout << "cocoa script: no window matches " << rest << "; there are";
      for (NSWindow* w in [NSApp windows])
        cout << " [" << window_title (w) << "]"
             << ([w isVisible]? "": "(hidden)")
             << ([NSApp modalWindow] == w? "(modal)": "");
      cout << LF;
    }
    else {
      NSRect f= [script_win frame];
      cout << "cocoa script: window at " << (int) f.origin.x << ","
           << (int) f.origin.y << " size " << (int) f.size.width << "x"
           << (int) f.size.height << LF;
    }
    return 1;
  }
  if (cmd == "choose" && N(a) > 1) {
    // an item of a menu or a tab, by its place in the last list of the texts
    int i= as_int (a[1]);
    id obj= (listed_items && i >= 0 && i < (int) [listed_items count])?
            [listed_items objectAtIndex: i]: nil;
    if ([obj isKindOfClass: [NSTabViewItem class]])
      [[(NSTabViewItem*) obj tabView] selectTabViewItem: obj];
    else if (obj)
      return choose ((NSMenuItem*) obj);
    else {
      cout << "cocoa script: no item " << a[1] << LF;
      return 2;
    }
    return 0;
  }
  NSWindow* win= script_target ();
  if (!win) {
    // after a "window" which matched nothing: skipped, until one matches
    cout << "cocoa script: no window, skipped: " << line << LF;
    return 1;
  }
  if (cmd == "move" && N(a) > 2)
    post_mouse (win, NSEventTypeMouseMoved, window_point (win, a), 0);
  else if ((cmd == "press" || cmd == "release" || cmd == "click") && N(a) > 2) {
    NSPoint p= window_point (win, a);
    bool right= N(a) > 3 && a[3] == "right";
    int clicks= (N(a) > 4 && is_int (a[4]))? max (1, as_int (a[4])): 1;
    if (cmd != "release") note_click (win, p);
    if (cmd == "click" && !right && select_row (win, p)) return 0;
    if (cmd != "release")
      post_mouse (win, right? NSEventTypeRightMouseDown: NSEventTypeLeftMouseDown,
                  p, clicks);
    if (cmd != "press")
      post_mouse (win, right? NSEventTypeRightMouseUp: NSEventTypeLeftMouseUp,
                  p, clicks);
  }
  else if (cmd == "key" && N(a) > 1)
    post_key (win, a[1], N(a) > 2? a[2]: string (""));
  else if (cmd == "text" && N(a) > 1)
    post_text (win, rest);
  else if (cmd == "focus") {
    // the window does not become the key window while TeXmacs is not the
    // active application (which a test does not make it): the canvas is
    // told that it has the focus
    [win makeKeyAndOrderFront: nil];
    NSView* v= find_canvas ([win contentView]);
    if (v && [v respondsToSelector: @selector(focusIn)])
      [v performSelector: @selector(focusIn)];
  }
  else if (cmd == "snapshot" && N(a) > 1)
    snapshot (win, a[1]);
  else if (cmd == "resize" && N(a) > 2)
    [win setContentSize: NSMakeSize (as_int (a[1]), as_int (a[2]))];
  else if (cmd == "repaint")
    [[win contentView] setNeedsDisplay: YES];
  else if (cmd == "close")
    [win performClose: nil];
  else {
    cout << "cocoa script: unknown command " << line << LF;
    return 2;
  }
  return 0;
}

bool
gui_test_busy () {
  // a menu closing, events of TeXmacs, delayed commands which are due, an
  // update
  // (not the events of the system: to look at them runs the run loop,
  // within a step; the driver waits for some quiet time anyway, gt-idle)
  // While a dialog is modal, the delayed commands and the updates of TeXmacs
  // wait for it to close, which the script does: they are not counted
  if (menu_closing) return true;
  if ([NSApp modalWindow] != nil) return false;
  return the_gui && the_gui->test_busy ();
}

void
gui_test_pump (bool on) {
  static TMTestHelper* helper= nil;
  static NSTimer* timer= nil;
  if (!helper) {
    helper= [[TMTestHelper alloc] init];
    NSNotificationCenter* nc= [NSNotificationCenter defaultCenter];
    [nc addObserver: helper selector: @selector(beginTracking:)
               name: NSMenuDidBeginTrackingNotification object: nil];
    [nc addObserver: helper selector: @selector(endTracking:)
               name: NSMenuDidEndTrackingNotification object: nil];
  }
  if (on && !timer) {
    timer= [[NSTimer timerWithTimeInterval: 0.01 target: helper
                                  selector: @selector(tick:)
                                  userInfo: nil repeats: YES] retain];
    [[NSRunLoop currentRunLoop] addTimer: timer forMode: NSRunLoopCommonModes];
  }
  if (!on && timer) {
    [timer invalidate];
    [timer release];
    timer= nil;
  }
}
