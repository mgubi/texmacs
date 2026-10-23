
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
#import <objc/runtime.h>

@protocol TMMenuItemDoit
- (void) doit;
@end

// the controller is told of the right clicks on its buttons
@protocol TMContextClick
- (BOOL) contextClick:(id) button event:(NSEvent*) e;
@end

@interface TMFlippedView : NSView
@end

@implementation TMFlippedView
- (BOOL) isFlipped { return YES; }
@end

/******************************************************************************
* Buttons of the icon bars, in the style of the toolbars of macOS: no border,
* a rounded highlight when the mouse is over them or presses them, an accent
* background when they are on, and a chevron for those with a menu
******************************************************************************/

@interface TMBarButton : NSButton
{
@public
  NSMenuItem *item;
  BOOL hover;
}
@end

@implementation TMBarButton
- (void) dealloc
{
  [item release];
  [super dealloc];
}

- (void) updateTrackingAreas
{
  for (NSTrackingArea *ta in [self trackingAreas]) [self removeTrackingArea: ta];
  [self addTrackingArea:
     [[[NSTrackingArea alloc] initWithRect: NSZeroRect
         options: NSTrackingMouseEnteredAndExited | NSTrackingActiveInActiveApp |
                  NSTrackingInVisibleRect
           owner: self userInfo: nil] autorelease]];
  [super updateTrackingAreas];
}
- (void) mouseEntered: (NSEvent*) e { (void) e; hover= YES; [self setNeedsDisplay: YES]; }
- (void) mouseExited: (NSEvent*) e { (void) e; hover= NO; [self setNeedsDisplay: YES]; }

- (BOOL) hasMenu { return [item submenu] != nil; }

// a right click, or a click with Control or Option (the right click of a
// mouse or trackpad with one button), goes to the context target of the
// controller if there is one
- (BOOL) contextClick: (NSEvent*) e
{
  id c= [self target];
  if (![c respondsToSelector: @selector(contextClick:event:)]) return NO;
  return [(id<TMContextClick>) c contextClick: self event: e];
}
- (void) rightMouseDown: (NSEvent*) e
{
  if (![self contextClick: e]) [super rightMouseDown: e];
}
- (void) mouseDown: (NSEvent*) e
{
  if (([e modifierFlags] & (NSEventModifierFlagControl | NSEventModifierFlagOption))
      && [self contextClick: e]) return;
  [super mouseDown: e];
}

- (NSSize) intrinsicContentSize
{
  NSSize s= [super intrinsicContentSize];
  // the text buttons with a menu have a chevron after the text; the icon
  // buttons a small triangle in their corner
  BOOL text= [[self title] length] > 0;
  s.width += (text? 12: 8) + ((text && [self hasMenu])? 9: 0);
  s.height= MAX (s.height, 24);
  return s;
}

- (void) drawRect: (NSRect) r
{
  NSRect b= NSInsetRect ([self bounds], 0.5, 1.5);
  NSBezierPath *p= [NSBezierPath bezierPathWithRoundedRect: b xRadius: 5 yRadius: 5];
  BOOL on= [item state] == NSControlStateValueOn;
  if ([[self cell] isHighlighted]) {
    [[NSColor colorWithWhite: 0.0 alpha: 0.16] setFill];
    [p fill];
  }
  else if (on) {
    [[[NSColor controlAccentColor] colorWithAlphaComponent: 0.22] setFill];
    [p fill];
  }
  else if (hover && [self isEnabled]) {
    [[NSColor colorWithWhite: 0.0 alpha: 0.07] setFill];
    [p fill];
  }
  BOOL text= [[self title] length] > 0;
  if ([self hasMenu] && text) {
    // a chevron after the text
    NSImage *ch= [NSImage imageWithSystemSymbolName: @"chevron.down"
                            accessibilityDescription: nil];
    NSImageSymbolConfiguration *cf=
      [NSImageSymbolConfiguration configurationWithPointSize: 7
                                                      weight: NSFontWeightBold];
    ch= [ch imageWithSymbolConfiguration: cf];
    NSSize cs= [ch size];
    NSRect inner= [self bounds];
    inner.size.width -= 9;
    NSRect cr= NSMakeRect (NSMaxX (inner) - 3,
                           floor (NSMidY ([self bounds]) - cs.height / 2) + 1,
                           cs.width, cs.height);
    [ch drawInRect: cr fromRect: NSZeroRect
         operation: NSCompositingOperationSourceOver
          fraction: [self isEnabled]? 0.6: 0.25 respectFlipped: YES hints: nil];
    [[self cell] drawInteriorWithFrame: inner inView: self];
  }
  else {
    [[self cell] drawInteriorWithFrame: [self bounds] inView: self];
    if ([self hasMenu]) {
      // a small triangle in the bottom right corner (as in Xcode)
      NSRect b= [self bounds];
      CGFloat x= NSMaxX (b) - 3, y= NSMaxY (b) - 4, d= 4;
      NSBezierPath *t= [NSBezierPath bezierPath];
      [t moveToPoint: NSMakePoint (x, y - d)];
      [t lineToPoint: NSMakePoint (x, y)];
      [t lineToPoint: NSMakePoint (x - d, y)];
      [t closePath];
      [[[NSColor labelColor] colorWithAlphaComponent:
          [self isEnabled]? 0.55: 0.2] setFill];
      [t fill];
    }
  }
  (void) r;
}
@end

/*! The separators between the groups of buttons: a thin line across the
 row, vertical in a row and horizontal in a column */
@interface TMBarSeparator : NSBox
{
@public
  NSLayoutConstraint *wc, *hc;
}
- (void) setVertical: (BOOL) flag;
@end

@implementation TMBarSeparator
- (void) setVertical: (BOOL) flag
{
  [wc setConstant: flag? 18: 1];
  [hc setConstant: flag? 1: 18];
}
@end

/*! The divider between the main and mode icons and the focus and user
 icons, which have another meaning: a line with a soft shadow below it. */
@interface TMBarDivider : NSView
@end

@implementation TMBarDivider
- (BOOL) isFlipped { return YES; }
- (void) drawRect: (NSRect) r
{
  (void) r;
  NSRect b= [self bounds];
  [[NSColor separatorColor] setFill];
  NSRectFill (NSMakeRect (0, 0, b.size.width, 1));
  NSGradient *g= [[[NSGradient alloc]
                    initWithStartingColor: [NSColor colorWithWhite: 0.0 alpha: 0.10]
                              endingColor: [NSColor colorWithWhite: 0.0 alpha: 0.0]]
                   autorelease];
  [g drawInRect: NSMakeRect (0, 1, b.size.width, b.size.height - 1) angle: 90];
}
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
    divider = [[TMBarDivider alloc] init];
    // a hairline below the icon bars, as below the toolbars of macOS
    line = [[NSBox alloc] init];
    [line setBoxType: NSBoxSeparator];
    [view addSubview: line];
    // the columns at the left, which scroll when they are too long
    sidePane = [[NSView alloc] init];
    sideScroll = [[NSScrollView alloc] init];
    [sideScroll setDrawsBackground: NO];
    [sideScroll setHasVerticalScroller: YES];
    [sideScroll setAutohidesScrollers: YES];
    [sideScroll setScrollerStyle: NSScrollerStyleOverlay];
    sideView = [[TMFlippedView alloc] init];
    [sideScroll setDocumentView: sideView];
    [sidePane addSubview: sideScroll];
    sideLine = [[NSBox alloc] init];
    [sideLine setBoxType: NSBoxSeparator];
    [sidePane addSubview: sideLine];
  }
  return self;
}

- (void) dealloc
{
  [rowArray release];
  [menuArray release];
  [shownArray release];
  [line release];
  [divider release];
  [view release];
  [sideLine release];
  [sideView release];
  [sideScroll release];
  [sidePane release];
  [super dealloc];
}

- (void) buttonAction:(TMBarButton*) b
{
  // NOTE: the row of the button may be replaced while its menu is shown
  // (setMenu:forRow: during the menu loop); the button and its item stay
  // until the end of the event (and so does the controller)
  [[b retain] autorelease];
  [[self retain] autorelease];
  NSMenuItem *mi = [[b->item retain] autorelease];
  NSMenu *sm = [[[mi submenu] retain] autorelease];
  if (sm) {
    // the menu below the button, or above it (the footer, which is at the
    // bottom of the window: the button is flipped, y goes down)
    NSPoint at= NSMakePoint (0, NSMaxY ([b bounds]) + 3);
    NSView *row = [b superview];
    if ([row isKindOfClass: [NSStackView class]] &&
        [(NSStackView*) row orientation] == NSUserInterfaceLayoutOrientationVertical)
    {
      // in a column at the left: to the right of the button; in a column
      // at the right: to its left
      if (right) {
        [sm update];
        at= NSMakePoint (- [sm size].width - 3, 0);
      }
      else at= NSMakePoint (NSMaxX ([b bounds]) + 3, 0);
    }
    else if (upward) {
      [sm update];
      at= NSMakePoint (0, - [sm size].height - 3);
    }
    [sm popUpMenuPositioningItem:nil atLocation: at inView:b];
    [b setNeedsDisplay: YES];
  }
  else if ([mi respondsToSelector:@selector(doit)]) [(id)mi doit];
}

- (NSView*) buttonFor:(NSMenuItem*) mi
{
  TMBarButton *b = [[[TMBarButton alloc] init] autorelease];
  b->item = [mi retain];
  [b setBordered: NO];
  [b setButtonType: NSButtonTypeMomentaryChange];
  [b setFocusRingType: NSFocusRingTypeNone];
  NSImage *img = [mi representedObject];
  if (img) {
    NSImage *small = [[img copy] autorelease];
    NSSize s = [small size];
    CGFloat k = MIN (1.0, 20.0 / MAX (s.width, s.height));
    [small setSize: NSMakeSize (s.width * k, s.height * k)];
    [b setImage: small];
    [b setImagePosition: NSImageOnly];
    [b setTitle: @""];
    // "icon:<name of its file>", for the scripted tests (ns_test.mm; the
    // copy of the image does not keep it)
    id name = objc_getAssociatedObject (img, @selector(tmIconName));
    if (name) objc_setAssociatedObject (b, @selector(tmIconName), name,
                                        OBJC_ASSOCIATION_RETAIN);
  }
  else {
    // bold if the title of the item is (the innermost tag of the footer)
    BOOL bold = NO;
    NSAttributedString *at = [mi attributedTitle];
    if ([at length] > 0) {
      NSFont *f = [at attribute: NSFontAttributeName atIndex: 0 effectiveRange: NULL];
      bold = f && ([[f fontDescriptor] symbolicTraits] & NSFontDescriptorTraitBold);
    }
    CGFloat fs = [NSFont smallSystemFontSize] + 1;
    NSDictionary *attrs =
      [NSDictionary dictionaryWithObjectsAndKeys:
        bold? [NSFont boldSystemFontOfSize: fs]: [NSFont systemFontOfSize: fs],
        NSFontAttributeName,
        [mi isEnabled]? [NSColor labelColor]: [NSColor tertiaryLabelColor],
        NSForegroundColorAttributeName, nil];
    [b setAttributedTitle: [[[NSAttributedString alloc]
                              initWithString: [mi title] attributes: attrs]
                             autorelease]];
    [b setImagePosition: NSNoImage];
  }
  // the name of a (named ...) of its item, for the scripted tests
  id named = objc_getAssociatedObject (mi, @selector(tmNamed));
  if (named) objc_setAssociatedObject (b, @selector(tmNamed), named,
                                       OBJC_ASSOCIATION_RETAIN);
  [b setEnabled: [mi isEnabled]];
  [b setToolTip: [mi toolTip]];
  [b setTarget: self];
  [b setAction: @selector(buttonAction:)];
  return b;
}

- (NSView*) separator
{
  TMBarSeparator *sep = [[[TMBarSeparator alloc] init] autorelease];
  [sep setBoxType: NSBoxSeparator];
  [sep setTranslatesAutoresizingMaskIntoConstraints: NO];
  sep->wc = [sep.widthAnchor constraintEqualToConstant: 1];
  sep->hc = [sep.heightAnchor constraintEqualToConstant: 18];
  [sep->wc setActive: YES];
  [sep->hc setActive: YES];
  return sep;
}

// a row from left to right, or a column from top to bottom (the main and
// mode icons at the left of the document); turned around in place, since
// the views of the items were taken from the menu when the row was made
- (void) orient:(NSView*) v vertical:(BOOL) flag
{
  if (![v isKindOfClass: [NSStackView class]]) return;
  NSStackView *row = (NSStackView*) v;
  if (flag) {
    [row setOrientation: NSUserInterfaceLayoutOrientationVertical];
    [row setAlignment: NSLayoutAttributeCenterX];
    [row setEdgeInsets: NSEdgeInsetsMake (4, 3, 8, 3)];
  }
  else {
    [row setOrientation: NSUserInterfaceLayoutOrientationHorizontal];
    [row setAlignment: NSLayoutAttributeCenterY];
    [row setEdgeInsets: NSEdgeInsetsMake (1, 8, 1, 8)];
  }
  for (NSView *sub in [row arrangedSubviews])
    if ([sub isKindOfClass: [TMBarSeparator class]])
      [(TMBarSeparator*) sub setVertical: flag];
  [row setFrameSize: [row fittingSize]];
}

- (BOOL) isSideRow:(unsigned) idx
{
  return side && idx < 2;
}

- (NSView*) rowFor:(NSMenu*) menu
{
  NSStackView *row = [[[NSStackView alloc] init] autorelease];
  [row setOrientation: NSUserInterfaceLayoutOrientationHorizontal];
  [row setAlignment: NSLayoutAttributeCenterY];
  [row setSpacing: 1.0];
  [row setEdgeInsets: NSEdgeInsetsMake (1, 8, 1, 8)];
  BOOL first = YES, pending_sep = NO;
  NSInteger c = [menu numberOfItems];
  for (NSInteger i = 0; i < c; i++) {
    NSMenuItem *mi = [menu itemAtIndex:i];
    if ([mi isSeparatorItem]) { pending_sep = !first; continue; }
    NSView *v = nil;
    if ([mi view]) {
      v = [[[mi view] retain] autorelease];
      [mi setView:nil];
    }
    else if ([mi representedObject] || [mi submenu] || [mi action])
      v = [self buttonFor: mi];
    else if ([mi image]) {
      // an image of its own (a swatch of colour): the image
      NSImageView *iv = [NSImageView imageViewWithImage: [mi image]];
      [iv setToolTip: [mi title]];
      v = iv;
    }
    else if ([[mi title] length] > 0) {
      NSTextField *t = [NSTextField labelWithString:[mi title]];
      [t setTextColor: [NSColor secondaryLabelColor]];
      [t setFont: [NSFont systemFontOfSize: [NSFont smallSystemFontSize] + 1]];
      v = t;
    }
    if (!v) continue;
    if (pending_sep) {
      // the groups are separated by a thin line, with some space
      NSView *sep = [self separator];
      [row addArrangedSubview: sep];
      [row setCustomSpacing: 7 afterView: [[row arrangedSubviews] objectAtIndex:
                                             [[row arrangedSubviews] count] - 2]];
      [row setCustomSpacing: 7 afterView: sep];
      pending_sep = NO;
    }
    [row addArrangedSubview: v];
    first = NO;
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
  // the name of the bar for the scripted tests (ns_test.mm)
  NSArray *names = @[@"mainbar", @"modebar", @"focusbar", @"userbar"];
  if (idx < [names count]) [row setIdentifier: [names objectAtIndex: idx]];
  if ([self isSideRow: idx]) [self orient: row vertical: YES];
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
  // The rows from the top to the bottom, with their natural height, a
  // little space below the title bar, and a hairline below the rows
  CGFloat y = 4.0, w = [view frame].size.width;
  BOOL any = NO, upper = NO, divided = NO;
  for (NSUInteger i = 0; i < [rowArray count]; i++) {
    NSView *row = [rowArray objectAtIndex:i];
    BOOL shown = [[shownArray objectAtIndex:i] boolValue] &&
                 [[row subviews] count] > 0;
    if (!shown) { [row removeFromSuperview]; continue; }
    if ([self isSideRow: i]) continue; // in a column (layoutSide)
    if (i < 2) upper = YES;
    else if (upper && !divided) {
      // the focus and user icons are below a divider
      if ([divider superview] != view) [view addSubview: divider];
      [divider setFrame: NSMakeRect (0, y + 1, w, 5)];
      y += 6.0;
      divided = YES;
    }
    if ([row superview] != view) [view addSubview:row];
    NSSize sz = [row fittingSize];
    // NOTE: the input fields shrink when the row is too long
    [row setFrame: NSMakeRect (0, y, w > 0? w: sz.width, sz.height)];
    y += sz.height + 2.0;
    any = YES;
  }
  NSRect r = [view frame];
  r.size.height = any? y + 3.0: 0.0;
  [view setFrame:r];
  [line setFrame: NSMakeRect (0, r.size.height - 1, w, 1)];
  [line setHidden: !any];
  if (!divided) [divider removeFromSuperview];
  [view setNeedsDisplay:YES];
}

- (NSView*) bar
{
  return view;
}

- (void) setSide:(BOOL) flag
{
  if (flag == side) return;
  side = flag;
  for (NSUInteger i = 0; i < [rowArray count] && i < 2; i++) {
    NSView *row = [rowArray objectAtIndex: i];
    [row removeFromSuperview];
    [self orient: row vertical: flag];
  }
  [self layout];
}

- (void) setRight:(BOOL) flag
{
  right = flag;
}

- (NSView*) sideBar
{
  return sidePane;
}

- (BOOL) sideShown:(NSUInteger) i
{
  if (![self isSideRow: i] || i >= [rowArray count]) return NO;
  NSView *row = [rowArray objectAtIndex: i];
  return [[shownArray objectAtIndex: i] boolValue] && [[row subviews] count] > 0;
}

- (CGFloat) sideWidth
{
  CGFloat w = 0;
  for (NSUInteger i = 0; i < 2; i++)
    if ([self sideShown: i])
      w += [[rowArray objectAtIndex: i] fittingSize].width;
  return w > 0? w + 1: 0;   // and the hairline
}

- (void) layoutSide
{
  // the columns from left to right (main, then mode; mirrored at the right
  // of the document), from the top of the pane, which is placed by the
  // window (ns_tm_widget_rep::layout); the hairline is on the side of the
  // document
  NSRect pr = [sidePane frame];
  CGFloat x = 0, h = 0;
  NSUInteger n = MIN ((NSUInteger) 2, [rowArray count]);
  for (NSUInteger k = 0; k < n; k++) {
    NSUInteger i = right? n - 1 - k: k;
    NSView *row = [rowArray objectAtIndex: i];
    if (![self sideShown: i]) {
      if ([row superview] == sideView) [row removeFromSuperview];
      continue;
    }
    if ([row superview] != sideView) [sideView addSubview: row];
    NSSize sz = [row fittingSize];
    [row setFrame: NSMakeRect (x, 0, sz.width, sz.height)];
    x += sz.width;
    h = MAX (h, sz.height);
  }
  CGFloat w = MAX (0, pr.size.width - 1);
  [sideScroll setFrame: NSMakeRect (right? 1: 0, 0, w, pr.size.height)];
  [sideView setFrame: NSMakeRect (0, 0, w, MAX (h, pr.size.height))];
  [sideLine setFrame: NSMakeRect (right? 0: w, 0, 1, pr.size.height)];
  [sidePane setHidden: x == 0];
}

- (NSView*) rowForMenu:(NSMenu*) menu
{
  // NOTE: the items are retained by the buttons, the menu by the controller
  if (menu) [menuArray addObject: menu];
  if ([menuArray count] > 8) [menuArray removeObjectAtIndex: 0];
  NSView *row = [self rowFor: menu];
  [row setIdentifier: @"footer"]; // for the scripted tests (ns_test.mm)
  return row;
}

- (BOOL) contextClick:(id) button event:(NSEvent*) e
{
  TMBarButton* b= (TMBarButton*) button;
  if (!contextTarget || ![contextTarget respondsToSelector: contextAction])
    return NO;
  [[b retain] autorelease];
  [contextTarget performSelector: contextAction withObject: b->item withObject: e];
  return YES;
}

@end
