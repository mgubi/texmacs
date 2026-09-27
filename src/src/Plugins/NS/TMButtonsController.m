
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
  [super dealloc];
}

- (void) buttonAction:(TMBarButton*) b
{
  NSMenuItem *mi = b->item;
  NSMenu *sm = [mi submenu];
  if (sm) {
    // the menu below the button
    [sm popUpMenuPositioningItem:nil
                      atLocation:NSMakePoint (0, NSMaxY ([b bounds]) + 3)
                          inView:b];
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
  }
  else {
    NSDictionary *attrs =
      [NSDictionary dictionaryWithObjectsAndKeys:
        [NSFont systemFontOfSize: [NSFont smallSystemFontSize] + 1],
        NSFontAttributeName,
        [mi isEnabled]? [NSColor labelColor]: [NSColor tertiaryLabelColor],
        NSForegroundColorAttributeName, nil];
    [b setAttributedTitle: [[[NSAttributedString alloc]
                              initWithString: [mi title] attributes: attrs]
                             autorelease]];
    [b setImagePosition: NSNoImage];
  }
  [b setEnabled: [mi isEnabled]];
  [b setToolTip: [mi toolTip]];
  [b setTarget: self];
  [b setAction: @selector(buttonAction:)];
  return b;
}

- (NSView*) separator
{
  NSBox *sep = [[[NSBox alloc] init] autorelease];
  [sep setBoxType: NSBoxSeparator];
  [sep setTranslatesAutoresizingMaskIntoConstraints: NO];
  [[sep.widthAnchor constraintEqualToConstant: 1] setActive: YES];
  [[sep.heightAnchor constraintEqualToConstant: 18] setActive: YES];
  return sep;
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

@end
