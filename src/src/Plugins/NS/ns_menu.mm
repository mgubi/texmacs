/******************************************************************************
* MODULE     : ns_menu.mm
* DESCRIPTION: Menus for the NS port
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
#include "ns_menu.h"
#include "ns_utilities.h"
#include "ns_renderer.h"
#include "ns_simple_widget.h"

#include "widget.hpp"
#include "message.hpp"
#include "analyze.hpp"
#include "promise.hpp"

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

- (void)doit { if (cmd) cmd->apply(); }

- (NSImage*) image
{
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
}
@end

/******************************************************************************
* TMLazyMenu
******************************************************************************/

@implementation TMLazyMenu
- (void)setPromise:(promise_rep<widget> *)p
{
  if (pm) { DEC_COUNT_NULL(pm); }  pm = p;  INC_COUNT_NULL(pm);
  forced = NO;
  [self setDelegate:self];
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
  if (!forced) {
    widget w = pm->eval();
    NSMenu *menu2 = to_nsmenu (w);
    NSInteger count = [menu2 numberOfItems];
    for (NSInteger j=0; j<count; j++) {
      NSMenuItem *itm = [[[menu2 itemAtIndex:0] retain] autorelease];
      [menu2 removeItem:itm];
      [menu insertItem:itm atIndex:j];
    }
    DEC_COUNT_NULL(pm); pm = NULL;
    forced = YES;
  }
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
