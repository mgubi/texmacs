
/******************************************************************************
* MODULE     : TMButtonsController.m
* DESCRIPTION: Controller for the widget bar
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#import "TMButtonsController.h"

@protocol TMMenuItemDoit
- (void) doit;
@end

@interface TMFlippedView : NSView
@end

@implementation TMFlippedView
- (BOOL) isFlipped { return YES; }
@end

@implementation TMButtonsController

- (id) init
{
  self = [super init];
  if (self != nil) {
    rowArray = [[NSMutableArray alloc] initWithCapacity:4];
    menuArray = [[NSMutableArray alloc] initWithCapacity:4];
    shownArray = [[NSMutableArray alloc] initWithCapacity:4];
    view = [[TMFlippedView alloc] init];
  }
  return self;
}

- (void) dealloc
{
  [rowArray release];
  [menuArray release];
  [shownArray release];
  [view release];
  [super dealloc];
}

- (void) buttonsAction:(id) sc
{
  NSInteger idx = [sc selectedSegment];
  NSArray *arr = [[sc cell] representedObject];
  if (idx < 0 || idx >= (NSInteger) [arr count]) return;
  NSMenuItem *mi = [arr objectAtIndex:idx];
  NSMenu *sm = [mi submenu];
  if (sm) {
    // the menu below the segment, as the menus of the Qt tool bars
    NSRect r = [sc bounds];
    CGFloat x = 0;
    for (NSInteger j = 0; j < idx; j++) x += [sc widthForSegment:j];
    [sm popUpMenuPositioningItem:nil
                      atLocation:NSMakePoint (x, [sc isFlipped]?
                                              NSMaxY (r) + 2: -2)
                          inView:sc];
  }
  else if ([mi respondsToSelector:@selector(doit)]) [(id)mi doit];
}

- (NSSegmentedControl*) segmentFor:(NSArray*) items
{
  NSSegmentedControl *sc = [[[NSSegmentedControl alloc] init] autorelease];
  [sc setSegmentStyle: NSSegmentStyleTexturedSquare];
  [sc setSegmentCount:[items count]];
  for (NSUInteger j = 0; j < [items count]; j++) {
    NSMenuItem *mi = [items objectAtIndex:j];
    [sc setEnabled:[mi isEnabled] forSegment:j];
    if ([mi representedObject]) {
      [sc setImage:[mi representedObject] forSegment:j];
      [sc setImageScaling: NSImageScaleProportionallyDown forSegment:j];
      [sc setLabel:@"" forSegment:j];
      [sc setWidth:25.0 forSegment:j];
    }
    else {
      // buttons with a text instead of an icon (focus bar)
      [sc setImage:nil forSegment:j];
      [sc setLabel:[mi title] forSegment:j];
      [sc setWidth:0.0 forSegment:j];
    }
    [(NSSegmentedCell*)[sc cell] setToolTip:[mi toolTip] forSegment:j];
  }
  [(NSCell*)[sc cell] setRepresentedObject:items];
  [[sc cell] setTrackingMode: NSSegmentSwitchTrackingMomentary];
  [sc setTarget: self];
  [sc setAction:@selector(buttonsAction:)];
  [sc sizeToFit];
  return sc;
}

- (NSView*) rowFor:(NSMenu*) menu
{
  NSStackView *row = [[[NSStackView alloc] init] autorelease];
  [row setOrientation: NSUserInterfaceLayoutOrientationHorizontal];
  [row setAlignment: NSLayoutAttributeCenterY];
  [row setSpacing: 5.0];
  [row setEdgeInsets: NSEdgeInsetsMake (2, 6, 2, 6)];
  NSMutableArray *segs = [NSMutableArray array];
  NSInteger c = [menu numberOfItems];
  for (NSInteger i = 0; i <= c; i++) {
    NSMenuItem *mi = (i < c)? [menu itemAtIndex:i]: nil;
    BOOL button = mi && ![mi isSeparatorItem] && ![mi view] &&
      ([mi representedObject] || [mi submenu] || [mi action]);
    if (button) { [segs addObject:mi]; continue; }
    if ([segs count] > 0) {
      [row addArrangedSubview: [self segmentFor: segs]];
      segs = [NSMutableArray array];
    }
    if (!mi || [mi isSeparatorItem]) continue;
    if ([mi view]) {
      NSView *v = [[[mi view] retain] autorelease];
      [mi setView:nil];
      [row addArrangedSubview: v];
    }
    else if ([[mi title] length] > 0) {
      NSTextField *t = [NSTextField labelWithString:[mi title]];
      [row addArrangedSubview: t];
    }
  }
  // NOTE: the row has its natural size until it is laid out
  [row setFrameSize: [row fittingSize]];
  return row;
}

- (void) ensureRow:(unsigned) idx
{
  while ([rowArray count] <= idx) {
    [rowArray addObject:[[[NSView alloc] init] autorelease]];
    [menuArray addObject:[[[NSMenu alloc] init] autorelease]];
    [shownArray addObject:[NSNumber numberWithBool:YES]];
  }
}

- (void)setMenu:(NSMenu *)menu forRow:(unsigned) idx
{
  [self ensureRow: idx];
  [[rowArray objectAtIndex:idx] removeFromSuperview];
  NSView *row = menu? [self rowFor: menu]: [[[NSView alloc] init] autorelease];
  [rowArray replaceObjectAtIndex:idx withObject:row];
  if (menu) [menuArray replaceObjectAtIndex:idx withObject:menu];
  [self layout];
}

- (void) setVisible:(BOOL) flag forRow:(unsigned) idx
{
  [self ensureRow: idx];
  [shownArray replaceObjectAtIndex:idx
                        withObject:[NSNumber numberWithBool:flag]];
  [self layout];
}

- (void) layout
{
  // The rows from the top to the bottom, with their natural height
  // NOTE: a little space below the title bar and above the canvas
  CGFloat y = 6.0, w = [view frame].size.width;
  for (NSUInteger i = 0; i < [rowArray count]; i++) {
    NSView *row = [rowArray objectAtIndex:i];
    BOOL shown = [[shownArray objectAtIndex:i] boolValue] &&
                 [[row subviews] count] > 0;
    if (!shown) { [row removeFromSuperview]; continue; }
    if ([row superview] != view) [view addSubview:row];
    NSSize sz = [row fittingSize];
    // NOTE: the input fields shrink when the row is too long
    [row setFrame: NSMakeRect (0, y, w > 0? w: sz.width, sz.height)];
    y += sz.height;
  }
  NSRect r = [view frame];
  r.size.height = y > 6.0? y + 4.0: 0.0;
  [view setFrame:r];
  [view setNeedsDisplay:YES];
}

- (NSView*) bar
{
  return view;
}

@end
