/******************************************************************************
* MODULE     : ns_tm_widget.mm
* DESCRIPTION: The main TeXmacs window for the NS port
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
#include "ns_simple_widget.h"
#include "ns_other_widgets.h"
#include "ns_renderer.h"
#include "ns_utilities.h"
#include "ns_menu.h"
#include "ns_gui.h"

#include "gui.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "promise.hpp"
#include "analyze.hpp"
#include "file.hpp"

#import "TMView.h"
#import "TMButtonsController.h"

#pragma mark ns_tm_widget_rep

NSString *TMToolbarIdentifier = @"TMToolbarIdentifier";
NSString *TMButtonsIdentifier = @"TMButtonsIdentifier";

@interface TMToolbarItem : NSToolbarItem
@end
NSColor* to_nscolor (color col);

@interface TMFlippedToolView : NSView
@end

@implementation TMFlippedToolView
- (BOOL) isFlipped { return YES; }
@end

/*! The handle between the canvas and the side tools, which resizes them as
 the splitters of the docks of the Qt interface. */
@interface TMSplitHandle : NSView
{
@public
  ns_tm_widget_rep* wid;
  int which;  // 0 for the right tools, 1 for the left ones
}
@end

/******************************************************************************
* TMClipView: the document is centered when it is smaller than the window
******************************************************************************/

@interface TMClipView : NSClipView
@end

@implementation TMClipView
- (NSRect) constrainBoundsRect: (NSRect) r
{
  r= [super constrainBoundsRect: r];
  NSView* d= [self documentView];
  if (d) {
    NSRect f= [d frame];
    if (r.size.width > f.size.width)
      r.origin.x= floor ((f.size.width - r.size.width) / 2);
    if (r.size.height > f.size.height)
      r.origin.y= floor ((f.size.height - r.size.height) / 2);
  }
  // NOTE: the scroll positions are whole pixels, so that the backing store
  // of the canvas moves by whole pixels (no seams between its parts)
  CGFloat k= [self window]? [[self window] backingScaleFactor]: 2.0;
  r.origin.x= round (r.origin.x * k) / k;
  r.origin.y= round (r.origin.y * k) / k;
  return r;
}
@end

@implementation TMToolbarItem
- (void)validate
{
  NSSize s = [[self view] frame].size;
  NSSize s2 = [self minSize];
  if ((s.width != s2.width)||(s.height!=s2.height)) {
    [self setMinSize:s];
    [self setMaxSize:s];
  }
  //	NSLog(@"validate\n");
}
@end



@interface TMWidgetHelper : NSObject
{
@public
  ns_tm_widget_rep *wid;
  NSToolbarItem *ti;
}
- (void)notify:(NSNotification*)obj;
@end

@implementation TMWidgetHelper
-(void)dealloc
{
  [ti release]; [super dealloc];
}
- (void)notify:(NSNotification*)n
{
  wid->layout();
}
- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar itemForItemIdentifier:(NSString *)itemIdentifier willBeInsertedIntoToolbar:(BOOL)flag
{
  if (itemIdentifier == TMButtonsIdentifier) {
    if (!ti) {
      ti = [[TMToolbarItem alloc] initWithItemIdentifier:TMButtonsIdentifier];
      [ti setView:[wid->bc bar]];
      NSRect f = [[wid->bc bar] frame];
      //	NSSize s = NSMakeSize(900,70);
      NSSize s = f.size;
      [ti setMinSize:s];
      [ti setMaxSize:s];
      
    }
    return ti;
  }
  return nil;
}
- (NSArray *)toolbarAllowedItemIdentifiers:(NSToolbar *)toolbar
{
  return [NSArray arrayWithObjects:TMButtonsIdentifier,nil];
}
- (NSArray *)toolbarDefaultItemIdentifiers:(NSToolbar *)toolbar
{
  return [NSArray arrayWithObjects:TMButtonsIdentifier,nil];
}
@end


ns_tm_widget_rep::ns_tm_widget_rep (int mask, command _quit):
  ns_view_widget_rep ([[[NSView alloc] initWithFrame:NSMakeRect(0,0,100,100)] autorelease],
                      texmacs_widget),
  sv(nil), leftField(nil), rightField(nil), bc(nil), toolbar(nil),
  prompt_view(nil),
  quit (_quit)
{
  // decode mask
  visibility[0] = (mask & 1)   == 1;   // header
  visibility[1] = (mask & 2)   == 2;   // main
  visibility[2] = (mask & 4)   == 4;   // mode
  visibility[3] = (mask & 8)   == 8;   // focus
  visibility[4] = (mask & 16)  == 16;  // user
  visibility[5] = (mask & 32)  == 32;  // footer
  visibility[6] = (mask & 64)  == 64;  // right side tools
  visibility[7] = (mask & 128) == 128; // left side tools
  visibility[8] = (mask & 256) == 256; // bottom tools
  visibility[9] = (mask & 512) == 512; // extra bottom tools
  
  
  NSSize s = NSMakeSize(100,20); // size of the right footer;
  NSRect r = [view bounds];
  NSRect r0 = r;
  //	r.size.height -= 100;
  //	r0.origin.y =+ r.size.height; r0.size.height = 100;
  NSRect r1 = r; r1.origin.y += s.height; r1.size.height -= s.height;
  NSRect r2 = r; r2.size.height = s.height;
  NSRect r3 = r2; 
  r2.size.width -= s.width; r3.origin.x =+ r2.size.width;
  sv = [[[NSScrollView alloc] initWithFrame:r1] autorelease];
  [sv setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
  [sv setHasVerticalScroller:YES];
  [sv setHasHorizontalScroller:YES];
  [sv setBorderType:NSNoBorder];
  // NOTE: the document is centered when it is smaller than the window, on
  // the background color of TeXmacs (as QTMScrollView)
  [sv setContentView: [[[TMClipView alloc] init] autorelease]];
  [sv setDrawsBackground: YES];
  [sv setBackgroundColor: to_nscolor (tm_background)];
  [sv setDocumentView:[[[NSView alloc] initWithFrame: NSMakeRect(0,0,100,100)] autorelease]];
  [view addSubview:sv];
  
  leftField = [[[NSTextField alloc] initWithFrame:r2] autorelease];
  rightField = [[[NSTextField alloc] initWithFrame:r3] autorelease];
  [leftField setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
  [rightField setAutoresizingMask:NSViewMinXMargin|NSViewMaxYMargin];
  [leftField setEditable: NO];
  [rightField setEditable: NO];
  [leftField setBackgroundColor:[NSColor windowBackgroundColor]];
  [rightField setBackgroundColor:[NSColor windowBackgroundColor]];
  [leftField setBezeled:NO];
  [rightField setBezeled:NO];
  [rightField setAlignment:NSTextAlignmentRight];
  [view addSubview:leftField];
  [view addSubview:rightField];
  
  bc = [[TMButtonsController alloc] init];
  // NOTE: the icon bars are shown above the canvas, as in the Qt interface
  [[bc bar] setAutoresizingMask: NSViewWidthSizable | NSViewMinYMargin];
  [view addSubview: [bc bar]];
  //NSView *mt = [bc bar];
  //[mt setFrame:r0];
  //[mt setAutoresizingMask:NSViewMaxXMargin|NSViewMinYMargin];
  //[view addSubview:mt];
  //	[mt setPostsFrameChangedNotifications:YES];
  wh = [[TMWidgetHelper alloc] init];
  wh->wid = this;
  // the side tools and the canvas are laid out again when the window is
  // resized and when the contents of the tools change (see TMRefreshView)
  [view setIdentifier: @"TMMainView"];
  [view setPostsFrameChangedNotifications: YES];
  [[NSNotificationCenter defaultCenter] addObserver: wh
      selector: @selector(notify:)
          name: NSViewFrameDidChangeNotification object: view];
  [[NSNotificationCenter defaultCenter] addObserver: wh
      selector: @selector(notify:)
          name: @"TMToolsChanged" object: view];
  for (int i=0; i<4; i++) {
    tool_views[i]= [[NSView alloc] initWithFrame: NSZeroRect];
    [tool_views[i] setHidden: YES];
    [view addSubview: tool_views[i]];
    if (i < 2) {
      TMSplitHandle* h= [[TMSplitHandle alloc] initWithFrame: NSZeroRect];
      h->wid= this;
      h->which= i;
      [h setHidden: YES];
      tool_handles[i]= h;
      tool_widths[i]= 0;
    }
  }
  // the handles are above the tools
  for (int i=0; i<2; i++) [view addSubview: tool_handles[i]];
	
  toolbar = [[NSToolbar alloc] initWithIdentifier:TMToolbarIdentifier ];
  [toolbar setDelegate:wh];
  
  updateVisibility();
  
}

ns_tm_widget_rep::~ns_tm_widget_rep() 
{ 
  [[NSNotificationCenter defaultCenter] removeObserver: wh];
  for (int i=0; i<4; i++) [tool_views[i] release];
  for (int i=0; i<2; i++) {
    ((TMSplitHandle*) tool_handles[i])->wid= NULL;
    [tool_handles[i] release];
  }
  [wh release];	
  [bc release]; 
}



static NSSize
tool_size (NSView* v) {
  // The size wanted by the contents of a tool container (the side tools are
  // in a scroll view, as in the Qt interface)
  if ([[v subviews] count] == 0) return NSZeroSize;
  NSView* c= [[v subviews] firstObject];
  if ([c isKindOfClass: [NSScrollView class]]) {
    NSView* doc= [(NSScrollView*) c documentView];
    if ([[doc subviews] count] == 0) return NSZeroSize;
    NSSize fs= [[[doc subviews] firstObject] fittingSize];
    return NSMakeSize (fs.width + 8 + [NSScroller scrollerWidthForControlSize:
                         NSControlSizeRegular scrollerStyle: [NSScroller preferredScrollerStyle]],
                       fs.height + 8);
  }
  NSSize fs= [c fittingSize];
  return NSMakeSize (fs.width + 8, fs.height + 8);
}


@implementation TMSplitHandle
- (void) resetCursorRects
{
  [self addCursorRect: [self bounds] cursor: [NSCursor resizeLeftRightCursor]];
}
- (void) drawRect: (NSRect) r
{
  (void) r;
  [[NSColor separatorColor] setFill];
  NSRect b= [self bounds];
  NSRectFill (NSMakeRect (which == 0? 0: b.size.width - 1, 0, 1, b.size.height));
}
- (void) mouseDragged: (NSEvent*) e
{
  if (!wid) return;
  NSView* sup= [self superview];
  NSPoint p= [sup convertPoint: [e locationInWindow] fromView: nil];
  double w= (which == 0)? [sup bounds].size.width - p.x: p.x;
  wid->tool_widths[which]= max (w, 60.0);
  wid->layout ();
}
@end

void ns_tm_widget_rep::layout()
{
  // From top to bottom: the icon bars, the left tools, the canvas and the
  // side tools, the bottom and extra tools, and the footer
  // the footer: the messages centered vertically, with some padding
  CGFloat pad= 10.0;
  CGFloat text_h= [[leftField cell] cellSize].height;
  CGFloat right_w= max ((CGFloat) 100.0, [[rightField cell] cellSize].width + 4);
  NSSize fs = NSMakeSize (right_w, 26); // size of the right footer
  NSRect r = [view bounds];
  // NOTE: the header contains the rows of icons, which are shown or hidden
  // one by one (see updateVisibility)
  [[bc bar] setFrameSize: NSMakeSize (r.size.width, [[bc bar] frame].size.height)];
  [bc layout];
  CGFloat bar_h = visibility[0]? [[bc bar] frame].size.height: 0;
  CGFloat foot_h= visibility[5]? fs.height: 0;
  if (prompt_view)
    foot_h= max (fs.height, [prompt_view fittingSize].height);
  bool show[4];
  NSSize sz[4];
  for (int i=0; i<4; i++) {
    sz[i]= tool_size (tool_views[i]);
    show[i]= visibility[6+i] && sz[i].width > 0 && sz[i].height > 0;
    [tool_views[i] setHidden: !show[i]];
  }
  // the widths chosen with the handles, or the natural ones
  CGFloat side_w = show[0]? min (tool_widths[0] > 0? tool_widths[0]: sz[0].width,
                                 r.size.width / 2): 0;
  CGFloat left_w = show[1]? min (tool_widths[1] > 0? tool_widths[1]: sz[1].width,
                                 r.size.width / 2): 0;
  CGFloat extra_h= show[3]? sz[3].height: 0;
  CGFloat bot_h  = show[2]? sz[2].height: 0;
  CGFloat y0= foot_h + extra_h + bot_h;
  CGFloat mid_h= max (0.0, r.size.height - bar_h - y0);
  [[bc bar] setFrame: NSMakeRect (0, r.size.height - bar_h,
                                  r.size.width, bar_h)];
  [[bc bar] setHidden: bar_h == 0];
  [tool_views[1] setFrame: NSMakeRect (0, y0, left_w, mid_h)];
  [tool_views[0] setFrame: NSMakeRect (r.size.width - side_w, y0, side_w, mid_h)];
  [sv setFrame: NSMakeRect (left_w, y0, r.size.width - left_w - side_w, mid_h)];
  [tool_handles[0] setFrame: NSMakeRect (r.size.width - side_w - 3, y0, 6, mid_h)];
  [tool_handles[1] setFrame: NSMakeRect (left_w - 3, y0, 6, mid_h)];
  [tool_handles[0] setHidden: !show[0]];
  [tool_handles[1] setHidden: !show[1]];
  [tool_views[2] setFrame: NSMakeRect (0, foot_h + extra_h, r.size.width, bot_h)];
  [tool_views[3] setFrame: NSMakeRect (0, foot_h, r.size.width, extra_h)];
  CGFloat ty= max ((CGFloat) 0.0, floor ((foot_h - text_h) / 2));
  [leftField setFrame: NSMakeRect (pad, ty, r.size.width - fs.width - 2*pad,
                                   min (text_h, foot_h))];
  [rightField setFrame: NSMakeRect (r.size.width - fs.width - pad, ty,
                                    fs.width, min (text_h, foot_h))];
  [leftField setHidden: foot_h == 0 || prompt_view];
  [rightField setHidden: foot_h == 0 || prompt_view];
  if (prompt_view) [prompt_view setFrame: NSMakeRect (0, 0, r.size.width, foot_h)];
}


static NSView*
canvas_of (NSView* v) {
  // The canvas (TMView) in the document view of a simple widget
  if ([v isKindOfClass: [TMView class]]) return v;
  for (NSView* sub in [v subviews])
    if ([sub isKindOfClass: [TMView class]]) return sub;
  return v;
}

static int
visibility_index (slot s) {
  // The index in ns_tm_widget_rep::visibility (see the constructor)
  switch (s) {
    case SLOT_FOCUS_ICONS_VISIBILITY: return 3;
    case SLOT_SIDE_TOOLS_VISIBILITY: return 6;
    case SLOT_LEFT_TOOLS_VISIBILITY: return 7;
    case SLOT_BOTTOM_TOOLS_VISIBILITY: return 8;
    default: return 9;
  }
}

static NSView*
view_with_identifier (NSView* v, NSString* name) {
  // The first view with this identifier (or with a name ending with ":name",
  // for the input fields of the form "name#serial:type")
  if (!v) return nil;
  NSString* id= [v identifier];
  if (id && ([id isEqualToString: name] ||
             [id hasSuffix: [@":" stringByAppendingString: name]]))
    return v;
  for (NSView* w in [v subviews]) {
    NSView* r= view_with_identifier (w, name);
    if (r) return r;
  }
  return nil;
}

void ns_tm_widget_rep::updateVisibility()
{
  // The main, mode, focus and user icons are the rows of the icon bar
  for (int i=0; i<4; i++) [bc setVisible: visibility[1+i] forRow: i];
  layout ();
}



void
ns_tm_widget_rep::send (slot s, blackbox val) {
  switch (s) {
  // NOTE: as in the Qt interface, the canvas handles these messages
  case SLOT_INVALIDATE:
  case SLOT_INVALIDATE_ALL:
  case SLOT_EXTENTS:
  case SLOT_SCROLL_POSITION:
  case SLOT_ZOOM_FACTOR:
  case SLOT_MOUSE_GRAB:
    if (!is_nil (main_widget)) main_widget->send (s, val);
    return;
  case SLOT_KEYBOARD_FOCUS_ON:
    {
      // As in the Qt interface (focus to the widget of this name): the
      // editor gets the focus, which is noticed as a change, so that the
      // menus and tools are updated
      check_type<string> (val, s);
      string name= open_box<string> (val);
      if (name == "canvas" && !is_nil (main_widget)) {
        NSView* v= canvas_of (concrete (main_widget)->as_nsview ());
        if (v && [v window]) [[v window] makeFirstResponder: v];
        the_gui->process_keyboard_focus
          ((ns_simple_widget_rep*) main_widget.rep, true, texmacs_time ());
      }
      else {
        // an input field, by its type (see qt_tm_widget_rep)
        NSView* v= view_with_identifier (view, to_nsstring (name));
        if (v && [v window]) [[v window] makeFirstResponder: v];
      }
    }
    break;
  case SLOT_MODIFIED:
    {
      // the "edited" dot in the close button of the window
      check_type<bool> (val, s);
      [[view window] setDocumentEdited: open_box<bool> (val)];
    }
    break;
  case SLOT_HEADER_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool f= open_box<bool> (val);
      visibility[0] = f;
      updateVisibility();
    }
    break;
  case SLOT_MAIN_ICONS_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool f= open_box<bool> (val);
      visibility[1] = f;
      updateVisibility();
    }
    break;
  case SLOT_MODE_ICONS_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool f= open_box<bool> (val);
      visibility[2] = f;
      updateVisibility();
    }
    break;
  case SLOT_USER_ICONS_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool f= open_box<bool> (val);
      visibility[4] = f;
      updateVisibility();
    }
    break;
  case SLOT_FOOTER_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool f= open_box<bool> (val);
      visibility[5] = f;
      updateVisibility();
    }
    break;
  case SLOT_FOCUS_ICONS_VISIBILITY:
  case SLOT_SIDE_TOOLS_VISIBILITY:
  case SLOT_LEFT_TOOLS_VISIBILITY:
  case SLOT_BOTTOM_TOOLS_VISIBILITY:
  case SLOT_EXTRA_TOOLS_VISIBILITY:
    {
      check_type<bool> (val, s);
      visibility[visibility_index (s)] = open_box<bool> (val);
      updateVisibility();
    }
    break;
    
  case SLOT_LEFT_FOOTER:
    {
      check_type<string> (val, s);
      string msg = open_box<string> (val);
      [leftField setStringValue:to_nsstring_utf8 (tm_var_encode (msg))];
      [leftField displayIfNeeded];
    }
    break;
  case SLOT_RIGHT_FOOTER:
    {
      check_type<string> (val, s);
      string msg = open_box<string> (val);
      [rightField setStringValue:to_nsstring_utf8 (tm_var_encode (msg))];
      // the field takes the width of its text
      CGFloat need= max ((CGFloat) 100.0, [[rightField cell] cellSize].width + 4);
      if (fabs (need - [rightField frame].size.width) > 0.5) layout ();
      [rightField displayIfNeeded];
    }
    break;
    
    
  case SLOT_SCROLLBARS_VISIBILITY:
    // ignore this: cocoa handles scrollbars independently
    //			send_int (THIS, "scrollbars", val);
    break;
    
  case SLOT_INTERACTIVE_MODE:
    {
      check_type<bool> (val, s);
      if (open_box<bool>(val) == true) do_interactive_prompt ();
      else end_interactive_prompt ();
    }
    break;
    
  case SLOT_FILE:
    {
      // the file of the window (for the proxy icon of the title)
      check_type<string> (val, s);
      string file = open_box<string> (val);
      if (DEBUG_EVENTS) cout << "File: " << file << LF;
      url u= url_system (file);
      if (file != "" && is_rooted (u) && exists (u))
        [[view window] setRepresentedFilename: to_nsstring_utf8 (as_string (u))];
      else [[view window] setRepresentedFilename: @""];
    }
      break;

  case SLOT_FULL_SCREEN:
    {
      // As in the Qt interface: a black background, without scroll bars
      check_type<bool> (val, s);
      bool flag= open_box<bool> (val);
      NSWindow* win= [view window];
      bool is_full= win && ([win styleMask] & NSWindowStyleMaskFullScreen);
      [sv setBackgroundColor: flag? [NSColor blackColor]
                                  : to_nscolor (tm_background)];
      [sv setHasVerticalScroller: !flag];
      [sv setHasHorizontalScroller: !flag];
      if (win && flag != is_full) [win toggleFullScreen: nil];
    }
      break;
      
      
  default:
    ns_view_widget_rep::send(s,val);
  }
}

blackbox
ns_tm_widget_rep::query (slot s, int type_id) {
  switch (s) {
  case SLOT_SCROLL_POSITION:
  case SLOT_EXTENTS:
  case SLOT_VISIBLE_PART:
  case SLOT_ZOOM_FACTOR:
    if (!is_nil (main_widget)) return main_widget->query (s, type_id);
    return ns_view_widget_rep::query (s, type_id);

        
  case SLOT_USER_ICONS_VISIBILITY:
    check_type_id<bool> (type_id, s);
    return close_box<bool> (visibility[4]);
  case SLOT_FOCUS_ICONS_VISIBILITY:
  case SLOT_SIDE_TOOLS_VISIBILITY:
  case SLOT_LEFT_TOOLS_VISIBILITY:
  case SLOT_BOTTOM_TOOLS_VISIBILITY:
  case SLOT_EXTRA_TOOLS_VISIBILITY:
    check_type_id<bool> (type_id, s);
    return close_box<bool> (visibility[visibility_index (s)]);
        
  case SLOT_MODE_ICONS_VISIBILITY:
    check_type_id<bool> (type_id, s);
    return close_box<bool> (visibility[2]);
    
  case SLOT_MAIN_ICONS_VISIBILITY:
    check_type_id<bool> (type_id, s);
    return close_box<bool> (visibility[1]);
    
  case SLOT_HEADER_VISIBILITY:
    check_type_id<bool> (type_id, s);
    return close_box<bool> (visibility[0]);
    
  case SLOT_FOOTER_VISIBILITY:
    check_type_id<bool> (type_id, s);
    return close_box<bool> (visibility[5]);
    
  case SLOT_INTERACTIVE_INPUT:
    {
      check_type_id<string> (type_id, s);
      // as in the Qt interface
      ns_input_text_widget_rep* w= (ns_input_text_widget_rep*) int_input.rep;
      if (w && w->is_ok ()) return close_box<string> (scm_quote (w->get_input ()));
      return close_box<string> ("#f");
    }
  case SLOT_INTERACTIVE_MODE:
    {
      check_type_id<bool> (type_id, s);
      return close_box<bool> (prompt_view != nil);
    }
    
  default:
    return ns_view_widget_rep::query(s,type_id);
  }
}

widget
ns_tm_widget_rep::read (slot s, blackbox index) {
  switch (s) {
  case SLOT_CANVAS:
    check_type_void (index, s);
    return main_widget;
  default:
    return ns_view_widget_rep::read(s,index);
  }
}



@interface TMMenuHelper : NSObject
{
@public
  NSMenuItem *mi;
  NSMenu *menu;
}
+ (TMMenuHelper *)sharedHelper;
- init;
- (void)setMenu:(NSMenu *)_mi;
@end

TMMenuHelper *the_menu_helper = nil;

@implementation TMMenuHelper
- init {
  [super init]; mi = nil; menu = nil;
  return self;
}
- (void)setMenu:(NSMenu *)_m
{
  // The menus of TeXmacs (File, Edit, ...) become the menus of the menu
  // bar, after the application menu
  if (!_m) return;
  NSMenu* main= [NSApp mainMenu];
  while ([main numberOfItems] > 1) [main removeItemAtIndex: 1];
  if (menu) [menu release];  menu = _m; [menu retain];
  NSArray* items= [[[menu itemArray] copy] autorelease];
  for (NSMenuItem* item in items) {
    [menu removeItem: item];
    // NOTE: the menu bar shows the titles of the submenus
    if ([item hasSubmenu]) [[item submenu] setTitle: [item title]];
    [main addItem: item];
  }
};
- (void)dealloc { [mi release]; [menu release]; [super dealloc]; }
+ (TMMenuHelper *)sharedHelper 
{ 
  if (!the_menu_helper) 
    {
      the_menu_helper = [[TMMenuHelper alloc] init];
    }
  return the_menu_helper; 
}

#if 0
- (BOOL)menu:(NSMenu *)menu updateItem:(NSMenuItem *)item atIndex:(int)index shouldCancel:(BOOL)shouldCancel
{
  return NO;
}
#endif
@end




void
ns_tm_widget_rep::write (slot s, blackbox index, widget w) {
  switch (s) {
  case SLOT_SIDE_TOOLS:
  case SLOT_LEFT_TOOLS:
  case SLOT_BOTTOM_TOOLS:
  case SLOT_EXTRA_TOOLS:
    {
      check_type_void (index, s);
      int i= (s == SLOT_SIDE_TOOLS? 0: s == SLOT_LEFT_TOOLS? 1:
              s == SLOT_BOTTOM_TOOLS? 2: 3);
      tool_widgets[i]= w;
      for (NSView* old in [[[tool_views[i] subviews] copy] autorelease])
        [old removeFromSuperview];
      NSView* v= is_nil (w)? nil: concrete (w)->as_nsview ();
      if (v && i < 2) {
        // the side tools can be scrolled (as the QScrollArea of Qt)
        NSScrollView* sc= [[[NSScrollView alloc] initWithFrame: [tool_views[i] bounds]] autorelease];
        [sc setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
        [sc setHasVerticalScroller: YES];
        [sc setAutohidesScrollers: YES];
        [sc setDrawsBackground: NO];
        NSView* doc= [[[TMFlippedToolView alloc] init] autorelease];
        [doc setTranslatesAutoresizingMaskIntoConstraints: NO];
        [sc setDocumentView: doc];
        NSClipView* clip= [sc contentView];
        [v setTranslatesAutoresizingMaskIntoConstraints: NO];
        [doc addSubview: v];
        NSLayoutConstraint* tr= [v.trailingAnchor constraintEqualToAnchor:
                                   doc.trailingAnchor constant: -4];
        [tr setPriority: NSLayoutPriorityDefaultHigh];
        [NSLayoutConstraint activateConstraints: @[
          [v.leadingAnchor constraintEqualToAnchor: doc.leadingAnchor constant: 4],
          tr,
          [v.topAnchor constraintEqualToAnchor: doc.topAnchor constant: 4],
          [v.bottomAnchor constraintLessThanOrEqualToAnchor: doc.bottomAnchor constant: -4],
          [doc.leadingAnchor constraintEqualToAnchor: clip.leadingAnchor],
          [doc.widthAnchor constraintEqualToAnchor: clip.widthAnchor],
          [doc.topAnchor constraintEqualToAnchor: clip.topAnchor],
          [doc.heightAnchor constraintGreaterThanOrEqualToAnchor: clip.heightAnchor]]];
        [tool_views[i] addSubview: sc];
      }
      else if (v) {
        [v setTranslatesAutoresizingMaskIntoConstraints: NO];
        [tool_views[i] addSubview: v];
        // NOTE: the trailing edge gives way while the tools are hidden
        NSLayoutConstraint* tr= [v.trailingAnchor constraintEqualToAnchor:
                                   tool_views[i].trailingAnchor constant: -4];
        [tr setPriority: NSLayoutPriorityDefaultHigh];
        [NSLayoutConstraint activateConstraints: @[
          [v.leadingAnchor constraintEqualToAnchor: tool_views[i].leadingAnchor constant: 4],
          tr,
          [v.topAnchor constraintEqualToAnchor: tool_views[i].topAnchor constant: 4]]];
      }
      layout ();
    }
    break;
  case SLOT_SCROLLABLE: 
    {
      check_type_void (index, s);
      main_widget = w;
      NSView *v = concrete (w)->as_nsview ();
      [sv setDocumentView: v];
      [[sv window] makeFirstResponder: canvas_of (v)];
    }
    break;
  case SLOT_MAIN_MENU:
    check_type_void (index, s);
    [[TMMenuHelper sharedHelper] setMenu:to_nsmenu(w)];
    break;
  case SLOT_MAIN_ICONS:
    check_type_void (index, s);
    [bc setMenu:to_nsmenu(w) forRow:0];
    layout();
    break;
  case SLOT_MODE_ICONS:
    check_type_void (index, s);
    [bc setMenu:to_nsmenu(w) forRow:1];
    layout();
    break;
  case SLOT_FOCUS_ICONS:
    check_type_void (index, s);
    [bc setMenu:to_nsmenu(w) forRow:2];
    layout();
    break;
  case SLOT_USER_ICONS:
    check_type_void (index, s);
    [bc setMenu:to_nsmenu(w) forRow:3];
    layout();
    break;
  case SLOT_INTERACTIVE_PROMPT:
    check_type_void (index, s);
    int_prompt = concrete(w); 
    //			THIS << set_widget ("interactive prompt", concrete (w));
    break;
  case SLOT_INTERACTIVE_INPUT:
    check_type_void (index, s);
    int_input = concrete(w);
    //			THIS << set_widget ("interactive input", concrete (w));
    break;
  default:
    ns_view_widget_rep::write(s,index,w);
  }
}

widget
ns_tm_widget_rep::plain_window_widget (string s, command q) {
  // creates a decorated window with name s and contents w
  widget w = ns_widget_rep::plain_window_widget (s, q);
  // to manage correctly retain counts
  ns_window_widget_rep * wid = (ns_window_widget_rep *)(w.rep);
  // the icon bars continue the title bar, as the toolbars of macOS
  NSWindow* win= [[wid->get_windowcontroller () window] retain];
  [win setTitlebarAppearsTransparent: YES];
  [win release];
  return wid;
}

/******************************************************************************
* Interactive prompt
******************************************************************************/

void
ns_tm_widget_rep::do_interactive_prompt () {
  // As QTMInteractivePrompt: the prompt and the input replace the messages
  // of the footer, and the input gets the focus
  if (is_nil (int_prompt) || is_nil (int_input)) return;
  end_interactive_prompt ();
  NSStackView* st= [[[NSStackView alloc] init] autorelease];
  [st setOrientation: NSUserInterfaceLayoutOrientationHorizontal];
  [st setEdgeInsets: NSEdgeInsetsMake (1, 6, 1, 6)];
  [st setSpacing: 6];
  NSView* p= int_prompt->as_nsview ();
  NSView* i= int_input->as_nsview ();
  if (p) [st addArrangedSubview: p];
  if (i) {
    [st addArrangedSubview: i];
    // the input takes the rest of the footer
    NSLayoutConstraint* c= [i.trailingAnchor constraintEqualToAnchor:
                              st.trailingAnchor constant: -6];
    [c setPriority: NSLayoutPriorityDefaultLow + 10];
    [c setActive: YES];
  }
  prompt_view= [st retain];
  [view addSubview: prompt_view];
  layout ();
  if (i && [i window]) [[i window] makeFirstResponder: i];
}

void
ns_tm_widget_rep::end_interactive_prompt () {
  if (!prompt_view) return;
  NSWindow* win= [prompt_view window];
  [prompt_view removeFromSuperview];
  [prompt_view release];
  prompt_view= nil;
  layout ();
  // the editor gets the focus back (as in Qt 6)
  if (!is_nil (main_widget) && win) {
    NSView* v= canvas_of (concrete (main_widget)->as_nsview ());
    if (v) [win makeFirstResponder: v];
  }
}
