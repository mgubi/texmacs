/******************************************************************************
* MODULE     : ns_menu.mm
* DESCRIPTION: Menus for the NS port
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_cocoa.h"
#include "ns_menu.h"
#include "ns_utilities.h"
#include "ns_renderer.h"
#include "ns_simple_widget.h"

#include "widget.hpp"
#include "message.hpp"
#include "analyze.hpp"
#include "promise.hpp"
#include "dictionary.hpp"

/******************************************************************************
* TMMenuItem
******************************************************************************/

@implementation TMMenuItem
- (void)setCommand:(command_rep *)_c
{
  if (cmd) { DEC_COUNT_NULL(cmd); } cmd = _c;
  if (cmd) {
    INC_COUNT_NULL(cmd);
    [self setAction:@selector(doit)];
    [self setTarget:self];
  }
}

- (void)setWidget:(ns_simple_widget_rep *)_w
{
  if (wid) { DEC_COUNT_NULL(wid); } wid = _w;
  if (wid) { INC_COUNT_NULL(wid); }
}

- (void)dealloc
{
  [self setCommand:NULL];
  [self setWidget:NULL];
  [super dealloc];
}

- (void)doit
{
  NS_ENTRY_BEGIN
    if (cmd) cmd->apply();
  NS_ENTRY_END ("the action of a menu item")
}

- (NSImage*) image
{
  // NOTE: also when the menus are drawn or searched, outside of the events
  NS_ENTRY_BEGIN
  // The image of an item drawn by a simple widget is computed lazily
  NSImage *img = [super image];
  if ((!img) && (wid)) {
    NSBitmapImageRep* rep = wid->impress ();
    if (rep) {
      img = [[[NSImage alloc] initWithSize: [rep size]] autorelease];
      [img addRepresentation: rep];
      [super setImage:img];
    }
    [self setWidget:NULL];
  }
  return img;
  NS_ENTRY_END ("-[TMMenuItem image]")
  [self setWidget: NULL]; // (not again)
  return [super image];
}
@end

/******************************************************************************
* TMLazyMenu
******************************************************************************/

extern string the_report; // (basic.cpp)

// whether a menu is being tracked (the menu bar, a pop-up, the keyboard)
static BOOL ns_menu_tracking= NO;
int ns_menu_stats[4]= {0, 0, 0, 0};


@implementation TMLazyMenu
+ (void)initialize
{
  if (self != [TMLazyMenu class]) return;
  NSNotificationCenter* c= [NSNotificationCenter defaultCenter];
  [c addObserverForName: NSMenuDidBeginTrackingNotification object: nil
     queue: nil usingBlock: ^(NSNotification* n) {
       (void) n; ns_menu_tracking= YES; ns_menu_stats[0]++; }];
  [c addObserverForName: NSMenuDidEndTrackingNotification object: nil
     queue: nil usingBlock: ^(NSNotification* n) {
       (void) n; ns_menu_tracking= NO; ns_menu_stats[1]++; }];
}

- (void)setPromise:(promise_rep<widget> *)p
{
  if (pm) { DEC_COUNT_NULL(pm); }  pm = p;  INC_COUNT_NULL(pm);
  [self setDelegate:self];
}

- (promise_rep<widget> *)promise
{
  return pm;
}

- (void)dealloc
{
  // NOTE: not setPromise:, which would make a weak reference to self
  [self setDelegate: nil];
  if (pm) { DEC_COUNT_NULL(pm); pm= NULL; }
  [super dealloc];
}

- (void)menuNeedsUpdate:(NSMenu *)menu
{
  // As QTMLazyMenu::force, each time the menu is shown: the items are
  // computed again, so that the check marks, the enabled items and the
  // lists (of documents, ...) are up to date
  if (!pm) return;
  NSMenu *menu2 = nil;
  // An error while computing the menu (a Scheme error in its definition,
  // such as Debug > Miscellaneous > Provoke menu error, thrown as a C++
  // string by tm_throw; an Objective-C exception) must not cross AppKit,
  // which would abort TeXmacs (macOS computes all the menus at once, e.g.
  // when Spotlight opens). As QTMLazyMenu::force (BEGIN_SLOT/END_SLOT) and
  // the other entry points (NS_ENTRY_BEGIN/END): caught; the menu gets a
  // disabled item which says so. The error is not shown yet: macOS also
  // computes the menus without opening them (-[NSMenu _simulateOpening:]),
  // and the errors would fill the window of the error messages while one
  // does nothing with the menus (#720). It is kept, logged, and shown by
  // menuWillOpen: if the menu is really opened.
  bool failed= false;
  failure= ""; failure_report= "";
  try {
    @try {
      widget w = pm->eval();
      menu2 = to_nsmenu (w);
    }
    @catch (NSException* e) {
      failure= from_nsstring_utf8 ([e name]) * ", " *
               from_nsstring_utf8 ([e reason]);
      failure_report= "Objective-C exception " * failure;
      NSArray* st= [e callStackSymbols];
      for (NSUInteger i= 0; i < [st count] && i < 30; i++)
        failure_report << "\n  " << from_nsstring_utf8 ([st objectAtIndex: i]);
      failed= true; }
  }
  catch (string& s) {
    failure= s; failure_report= the_report; failed= true; }
  catch (std::exception& x) {
    failure= string (x.what ()); failed= true; }
  catch (...) {
    failure= "an unknown C++ exception"; failed= true; }
  if (failed) {
    if (N (failure) == 0) failure= "an error";
    // (tm_throw has set these for its error; the report is ours)
    the_exception= ""; the_report= "";
    // (once for the same error in a row: a scan repeats it for each menu)
    static string last_logged;
    if (failure != last_logged) {
      last_logged= failure;
      cout << "Cocoa: an error was caught while a menu was computed (shown "
           << "if the menu is opened): " << failure << LF;
    }
  }
  if (failed || !menu2) {
    [menu removeAllItems];
    NSMenuItem *itm = [[[NSMenuItem alloc]
                         initWithTitle: to_nsstring (translate ("Error in menu"))
                                action: NULL keyEquivalent: @""] autorelease];
    [itm setEnabled: NO];
    [menu addItem: itm];
    return;
  }
  [menu removeAllItems];
  NSInteger count = [menu2 numberOfItems];
  for (NSInteger j=0; j<count; j++) {
    NSMenuItem *itm = [[[menu2 itemAtIndex:0] retain] autorelease];
    [menu2 removeItem:itm];
    [menu addItem:itm];
  }
}

- (void)menuWillOpen:(NSMenu *)menu
{
  // macOS also computes the menus without showing them
  // (-[NSMenu _simulateOpening:], with Spotlight or Apple Intelligence),
  // and for these it calls menuNeedsUpdate: and menuWillOpen: one after
  // the other, in the same call, and not menuDidClose: (checked on macOS
  // 15.7). So these two do not tell a real opening. The tracking of the
  // menus does: NSMenuDidBeginTrackingNotification comes before the menu
  // is computed, whatever opens it (the menu bar, a key, a pop-up). The
  // error is shown at once when the menu opens in a tracking, and in any
  // case when it closes (menuDidClose:), which a real opening always ends
  // with: it is never lost. A scan neither tracks nor closes, so nothing
  // is shown (#720). If a scan did one of them, the error would be shown:
  // a repeated message rather than a hidden one.
  (void) menu;
  ns_menu_stats[2]++;
  if (N (failure) > 0 && ns_menu_tracking) [self reportFailure];
}

- (void)menuDidClose:(NSMenu *)menu
{
  (void) menu;
  ns_menu_stats[3]++;
  [self reportFailure];
}

- (void)reportFailure
{
  if (N (failure) == 0) return;
  string what= failure, report= failure_report;
  failure= ""; failure_report= "";
  the_exception= what; the_report= report;
  ns_report_exception ("a menu", what);
}

- (BOOL)menuHasKeyEquivalent:(NSMenu *)menu forEvent:(NSEvent *)event
                      target:(id *)target action:(SEL *)action
{
  // disable keyboard handling for lazy menus
  (void) menu; (void) event; (void) target; (void) action;
  return NO;
}
@end

/******************************************************************************
* TMTileView
******************************************************************************/

@implementation TMTileView
- (id) initWithObjects:(NSArray*)objs cols:(int)_cols
{
  self = [super init];
  if (self != nil) {
    int current_col;
    int current_row;
    cols = _cols;
    current_col = cols;
    current_row = -1;
    [self setCellSize:NSMakeSize(20,20)];
    [self renewRows:0 columns:cols];
    for (NSMenuItem *mi in objs) {
      if (current_col == cols) {
        current_col=0; current_row++;
        [self addRow];
      }
      NSImageCell *cell = [[[NSImageCell alloc] initImageCell:[mi image]] autorelease];
      [cell setRepresentedObject:mi];
      [self putCell:cell atRow:current_row column:current_col];
      current_col++;
    }
    [self setTarget:self];
    [self setAction:@selector(click:)];
    [self sizeToCells];
  }
  return self;
}

- (void) click:(TMTileView*)tile
{
  (void) tile;
  // on mouse up, we want to dismiss the menu being tracked
  NSMenuItem* mi = [self enclosingMenuItem];
  [[mi menu] cancelTracking];
  TMMenuItem* item = [(NSCell*)[self selectedCell] representedObject];
  [item doit];
}
@end

/******************************************************************************
* ns_menu_rep
******************************************************************************/

ns_menu_rep::ns_menu_rep (NSMenuItem* _item):
  ns_widget_rep (vertical_menu), item (_item), position (coord2 (0, 0))
{ [item retain]; }

ns_menu_rep::~ns_menu_rep () { [item release]; }

widget
ns_menu_rep::make_popup_widget () {
  return this;
}

widget
ns_menu_rep::popup_window_widget (string s) {
  [item setTitle: to_nsstring (s)];
  return this;
}

TMMenuItem*
ns_menu_rep::as_menuitem () {
  return (TMMenuItem*) item;
}

void
ns_menu_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_POSITION:
      check_type<coord2> (val, s);
      position= open_box<coord2> (val);
      break;
    case SLOT_VISIBILITY:
      check_type<bool> (val, s);
      break;
    case SLOT_MOUSE_GRAB:
      {
        // show the menu at the position (the origin of the screen
        // coordinates of TeXmacs is at the top left of the main screen)
        check_type<bool> (val, s);
        if (open_box<bool> (val) && [item submenu]) {
          NSPoint p= to_nspoint (position);
          p.y= main_screen_height () - p.y;
          [[item submenu] popUpMenuPositioningItem: nil atLocation: p
                                            inView: nil];
        }
      }
      break;
    default:
      ns_widget_rep::send (s, val);
  }
}

/******************************************************************************
* Conversions
******************************************************************************/

NSMenu*
to_nsmenu (widget w) {
  // The submenu of the item of a menu widget (horizontal_menu, ...)
  if (is_nil (w)) return nil;
  NSMenuItem* mi = concrete (w)->as_menuitem ();
  if (!mi) return nil;
  NSMenu *m = [[[mi submenu] retain] autorelease];
  [mi setSubmenu:nil];
  return m;
}

NSMenuItem*
to_nsmenuitem (widget w) {
  if (is_nil (w)) return nil;
  return concrete (w)->as_menuitem ();
}

TMMenuItem*
ns_simple_widget_rep::as_menuitem () {
  // A menu item drawn by the simple widget (see tm_button.cpp)
  TMMenuItem *mi = [[[TMMenuItem alloc] init] autorelease];
  [mi setWidget: this];
  return mi;
}
