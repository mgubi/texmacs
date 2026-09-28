/******************************************************************************
* MODULE     : ns_tm_widget.mm
* DESCRIPTION: The main TeXmacs window for the NS port
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_cocoa.h"
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
#include "scheme.hpp"
#include "sys_utils.hpp"

#import "TMView.h"
#import "TMButtonsController.h"

#pragma mark ns_tm_widget_rep

static void forget_main_menu (ns_tm_widget_rep* w);
static void ns_start_window_test ();

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

/*! Follows the main view and its window for the widget: the layout, the
 menu bar of the window when it becomes the main one, and the full screen
 mode entered or left with the controls of macOS. */
@interface TMWidgetHelper : NSObject
{
@public
  ns_tm_widget_rep *wid;
}
- (void)notify:(NSNotification*)obj;
@end

@implementation TMWidgetHelper
- (void)notify:(NSNotification*)n
{
  (void) n;
  if (wid) wid->layout();
}
- (void)windowNotify:(NSNotification*)n
{
  if (!wid || [n object] != [wid->view window]) return;
  NSString* name= [n name];
  if ([name isEqualToString: NSWindowDidBecomeMainNotification])
    wid->window_became_main ();
  else if ([name isEqualToString: NSWindowDidEnterFullScreenNotification])
    wid->window_full_screen (true);
  else if ([name isEqualToString: NSWindowDidExitFullScreenNotification])
    wid->window_full_screen (false);
}
@end


ns_tm_widget_rep::ns_tm_widget_rep (int mask, command _quit):
  ns_view_widget_rep ([[[NSView alloc] initWithFrame:NSMakeRect(0,0,100,100)] autorelease],
                      texmacs_widget),
  sv(nil), leftField(nil), rightField(nil), bc(nil), menu_items(nil),
  full_screen(false), prompt_view(nil),
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
  // NOTE: the window comes later (see plain_window_widget)
  NSArray* names= @[NSWindowDidBecomeMainNotification,
                    NSWindowDidEnterFullScreenNotification,
                    NSWindowDidExitFullScreenNotification];
  for (NSString* name in names)
    [[NSNotificationCenter defaultCenter] addObserver: wh
        selector: @selector(windowNotify:) name: name object: nil];
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

  updateVisibility();
  ns_start_window_test ();
}

ns_tm_widget_rep::~ns_tm_widget_rep() 
{ 
  [[NSNotificationCenter defaultCenter] removeObserver: wh];
  wh->wid= NULL;
  forget_main_menu (this);
  [menu_items release];
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
  // the footer: the messages centered vertically, with some padding (also
  // above and below)
  CGFloat pad= 10.0, vpad= 4.0;
  CGFloat text_h= [[leftField cell] cellSize].height;
  CGFloat right_w= max ((CGFloat) 100.0, [[rightField cell] cellSize].width + 4);
  NSSize fs = NSMakeSize (right_w, 26 + 2*vpad); // size of the right footer
  NSRect r = [view bounds];
  // NOTE: the header contains the rows of icons, which are shown or hidden
  // one by one (see updateVisibility)
  [[bc bar] setFrameSize: NSMakeSize (r.size.width, [[bc bar] frame].size.height)];
  [bc layout];
  CGFloat bar_h = visibility[0]? [[bc bar] frame].size.height: 0;
  CGFloat foot_h= visibility[5]? fs.height: 0;
  if (prompt_view)
    foot_h= max (fs.height, [prompt_view fittingSize].height + 2*vpad);
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
  if (prompt_view)
    [prompt_view setFrame: NSMakeRect (0, vpad, r.size.width, foot_h - 2*vpad)];
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
  case SLOT_KEYBOARD_FOCUS:
    {
      // as in the Qt interface: the canvas gets the focus
      check_type<bool> (val, s);
      if (open_box<bool> (val) && !is_nil (main_widget)) {
        NSView* v= canvas_of (concrete (main_widget)->as_nsview ());
        if (v && [v window] && [[v window] firstResponder] != v)
          [[v window] makeFirstResponder: v];
      }
    }
    break;
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
      full_screen= flag;
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



/******************************************************************************
* The menu bar: macOS has one, which shows the menus of the main window (as
* the menu bar of each QTMWindow in the Qt interface); it is not changed
* while it is used (see menu_count and waiting_widgets in the Qt interface)
******************************************************************************/

static ns_tm_widget_rep* menu_owner= NULL;   // the window of the menu bar
static ns_tm_widget_rep* menu_waiting= NULL; // to be installed after tracking
static bool menu_tracking= false;            // the menu bar is being used

@interface TMMenuHelper : NSObject
{
@public
  NSArray *items;  // the items installed in the menu bar
}
+ (TMMenuHelper *)sharedHelper;
- (void)setItems:(NSArray *)its;
@end

TMMenuHelper *the_menu_helper = nil;

@implementation TMMenuHelper
- init {
  [super init]; items = nil;
  NSNotificationCenter* nc= [NSNotificationCenter defaultCenter];
  [nc addObserver: self selector: @selector(beginTracking:)
             name: NSMenuDidBeginTrackingNotification object: nil];
  [nc addObserver: self selector: @selector(endTracking:)
             name: NSMenuDidEndTrackingNotification object: nil];
  return self;
}
- (void)setItems:(NSArray *)its
{
  // The menus of TeXmacs (File, Edit, ...) become the menus of the menu
  // bar, after the application menu
  if (!its || its == items) return;
  NSMenu* main= [NSApp mainMenu];
  while ([main numberOfItems] > 1) [main removeItemAtIndex: 1];
  [its retain]; [items release]; items= its;
  for (NSMenuItem* item in items) {
    // NOTE: the menu bar shows the titles of the submenus
    if ([item hasSubmenu]) [[item submenu] setTitle: [item title]];
    [main addItem: item];
  }
}
- (void)beginTracking:(NSNotification*)n
{
  if ([n object] == [NSApp mainMenu]) menu_tracking= true;
}
- (void)endTracking:(NSNotification*)n
{
  if ([n object] != [NSApp mainMenu]) return;
  menu_tracking= false;
  // NOTE: after the action of the chosen item, which comes after this
  [self performSelector: @selector(installWaiting) withObject: nil
             afterDelay: 0];
}
- (void)installWaiting
{
  if (menu_tracking || !menu_waiting) return;
  ns_tm_widget_rep* w= menu_waiting;
  menu_waiting= NULL;
  w->install_main_menu ();
}
- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver: self];
  [items release]; [super dealloc];
}
+ (TMMenuHelper *)sharedHelper 
{ 
  if (!the_menu_helper) 
    {
      the_menu_helper = [[TMMenuHelper alloc] init];
    }
  return the_menu_helper; 
}
@end

static void
forget_main_menu (ns_tm_widget_rep* w) {
  // The widget is deleted
  if (menu_owner == w) menu_owner= NULL;
  if (menu_waiting == w) menu_waiting= NULL;
}

static const char*
ns_test_menu_state (NSWindow* w) {
  // for the testing aid: whether the menu bar belongs to the window
  if (!menu_owner || [menu_owner->view window] != w) return "";
  TMMenuHelper* h= [TMMenuHelper sharedHelper];
  return h->items == menu_owner->menu_items? "menu-bar ": "menu-bar-waiting ";
}

void
ns_tm_widget_rep::install_main_menu () {
  // The menus of this window in the menu bar, now or when it is not used
  if (menu_owner != this || !menu_items) return;
  if (menu_tracking) { menu_waiting= this; return; }
  [[TMMenuHelper sharedHelper] setItems: menu_items];
}

void
ns_tm_widget_rep::window_became_main () {
  // The menu bar shows the menus of the main window
  menu_owner= this;
  install_main_menu ();
}

void
ns_tm_widget_rep::window_full_screen (bool flag) {
  // The full screen mode was entered or left with the controls of macOS (the
  // green button, the menus or escape): TeXmacs follows, as when it changes
  // the mode itself (which then does not change the window again)
  if (flag == full_screen) return;
  if (flag)
    exec_delayed (scheme_cmd ("(when (not (or (full-screen?) (full-screen-edit?)))"
                              "  (toggle-full-screen-edit-mode))"));
  else
    exec_delayed (scheme_cmd ("(cond ((full-screen?) (toggle-full-screen-mode))"
                              "      ((full-screen-edit?)"
                              "       (toggle-full-screen-edit-mode)))"));
}



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
      NSView* v= is_nil (w)? nil: concrete (w)->as_nsview ();
      // NOTE: the scroll view of the side tools is kept, with its position,
      // when their contents change (as the QScrollArea of Qt, whose widget
      // is replaced): the tools are made again after most clicks in them
      NSScrollView* sc= nil;
      if (v && i < 2)
        for (NSView* old in [tool_views[i] subviews])
          if ([old isKindOfClass: [NSScrollView class]]) sc= (NSScrollView*) old;
      for (NSView* old in [[[tool_views[i] subviews] copy] autorelease])
        if (old != sc) [old removeFromSuperview];
      if (v && i < 2) {
        // the side tools can be scrolled (as the QScrollArea of Qt)
        NSView* doc;
        if (sc) {
          doc= [sc documentView];
          for (NSView* old in [[[doc subviews] copy] autorelease])
            [old removeFromSuperview];
        }
        else {
          sc= [[[NSScrollView alloc] initWithFrame: [tool_views[i] bounds]] autorelease];
          [sc setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
          [sc setHasVerticalScroller: YES];
          [sc setAutohidesScrollers: YES];
          [sc setDrawsBackground: NO];
          doc= [[[TMFlippedToolView alloc] init] autorelease];
          [doc setTranslatesAutoresizingMaskIntoConstraints: NO];
          [sc setDocumentView: doc];
          NSClipView* clip= [sc contentView];
          [NSLayoutConstraint activateConstraints: @[
            [doc.leadingAnchor constraintEqualToAnchor: clip.leadingAnchor],
            [doc.widthAnchor constraintEqualToAnchor: clip.widthAnchor],
            [doc.topAnchor constraintEqualToAnchor: clip.topAnchor],
            [doc.heightAnchor constraintGreaterThanOrEqualToAnchor: clip.heightAnchor]]];
          [tool_views[i] addSubview: sc];
        }
        NSPoint pos= [[sc contentView] bounds].origin;
        [v setTranslatesAutoresizingMaskIntoConstraints: NO];
        [doc addSubview: v];
        NSLayoutConstraint* tr= [v.trailingAnchor constraintEqualToAnchor:
                                   doc.trailingAnchor constant: -4];
        [tr setPriority: NSLayoutPriorityRequired - 1];
        [NSLayoutConstraint activateConstraints: @[
          [v.leadingAnchor constraintEqualToAnchor: doc.leadingAnchor constant: 4],
          tr,
          [v.topAnchor constraintEqualToAnchor: doc.topAnchor constant: 4],
          [v.bottomAnchor constraintLessThanOrEqualToAnchor: doc.bottomAnchor constant: -4]]];
        // the same position in the new contents (as far as they go)
        [sc layoutSubtreeIfNeeded];
        [[sc contentView] scrollToPoint:
          [[sc contentView] constrainBoundsRect:
            NSMakeRect (pos.x, pos.y, [[sc contentView] bounds].size.width,
                        [[sc contentView] bounds].size.height)].origin];
        [sc reflectScrolledClipView: [sc contentView]];
      }
      else if (v) {
        [v setTranslatesAutoresizingMaskIntoConstraints: NO];
        [tool_views[i] addSubview: v];
        // NOTE: the trailing edge gives way while the tools are hidden
        NSLayoutConstraint* tr= [v.trailingAnchor constraintEqualToAnchor:
                                   tool_views[i].trailingAnchor constant: -4];
        [tr setPriority: NSLayoutPriorityRequired - 1];
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
    {
      check_type_void (index, s);
      // the menus of the window, shown in the menu bar while it is the main
      // window (or the first window, before a window becomes the main one)
      NSMenu* m= to_nsmenu (w);
      if (!m) break;
      NSArray* its= [[[m itemArray] copy] autorelease];
      [m removeAllItems];
      [its retain]; [menu_items release]; menu_items= its;
      NSWindow* win= [view window];
      if (!menu_owner || (win && [win isMainWindow])) menu_owner= this;
      install_main_menu ();
    }
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
  // NOTE: as in the Qt interface, the widget already has its quit command
  // (for kill_window_command), which the close button of the window runs
  (void) q;
  widget w = ns_widget_rep::plain_window_widget (s, quit);
  // to manage correctly retain counts
  ns_window_widget_rep * wid = (ns_window_widget_rep *)(w.rep);
  // the icon bars continue the title bar, as the toolbars of macOS
  NSWindow* win= [[(NSWindowController*) wid->get_windowcontroller () window] retain];
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

/******************************************************************************
 * Testing aid: TEXMACS_NS_WINDOW_TEST=<steps> (separated by ";"), one per
 * second after four seconds, on the windows of TeXmacs (main is the main
 * window, or the first window of TeXmacs which is shown):
 *   close[:<title>]   the close button of the main window (or of the window
 *                     with this title) (performClose:)
 *   fullscreen[:<title>]
 *                     the full screen button of macOS (toggleFullScreen:)
 *   move:<x>,<y>      the top left corner of the main window (in points,
 *                     from the top left of the main screen)
 *   resize:<w>,<h>    the size of the contents of the main window
 *   front:<title>     the window with this title becomes the main window
 *   main:<title>      as if it became the main window (the notification)
 *   windows           prints the windows (title, frame, main, shown)
 *   menubar[:<depth>] prints the menu bar (as TEXMACS_NS_MENUS)
 *   menu              prints the last menu which was shown (a pop-up menu)
 *   cancel            closes the menu which is shown
 *   pull:<n>          presses the n-th button with a menu of the dialogs
 *   begin-tracking, end-tracking
 *                     as if the menu bar were being used, or not anymore
 *   select:<text>     selects the row with this text in a list
 *   combo:<n>=<text>  types the text in the n-th combo box, then return
 *   eval:<scheme>     evaluates a Scheme expression (delayed)
 *   side-scroll:<y>   scrolls the side tools which are shown to y
 *   side-pos          prints the scroll position of the side tools
 *   side-width:<w>    the width of the right side tools (as with the handle)
 ******************************************************************************/

static NSMenu* ns_test_menu= nil;  // the last menu which was shown
static const char* ns_test_menu_state (NSWindow* w);

static NSWindow*
ns_test_main_window () {
  NSWindow* w= [NSApp mainWindow];
  if (w && !is_nil (ns_window_widget_of (w))) return w;
  for (NSWindow* win in [NSApp orderedWindows])
    if ([win isVisible] && !is_nil (ns_window_widget_of (win))) return win;
  return nil;
}

static void
ns_test_print_menu (NSMenu* m, int depth, int max_depth) {
  if (!m || depth > max_depth) return;
  if ([m delegate] && [[m delegate] respondsToSelector: @selector(menuNeedsUpdate:)])
    [[m delegate] menuNeedsUpdate: m];
  for (NSMenuItem* mi in [m itemArray]) {
    NSString* t= [mi isSeparatorItem]? @"---": [mi title];
    fprintf (stderr, "NSTEST menu %*s%s%s%s\n", 2 * depth, "",
             [mi state] == NSControlStateValueOn? "[x] ": "",
             [t UTF8String], [mi isEnabled]? "": " (disabled)");
    if ([mi hasSubmenu]) ns_test_print_menu ([mi submenu], depth + 1, max_depth);
  }
}

static bool
ns_test_select (NSView* v, NSString* text) {
  if ([v isKindOfClass: [NSTableView class]] &&
      ![v isKindOfClass: [NSOutlineView class]]) {
    NSTableView* t= (NSTableView*) v;
    for (NSInteger i=0; i<[t numberOfRows]; i++) {
      id o= [[t dataSource] tableView: t objectValueForTableColumn:
                [[t tableColumns] firstObject] row: i];
      if ([o isKindOfClass: [NSString class]] && [o isEqualToString: text]) {
        [t selectRowIndexes: [NSIndexSet indexSetWithIndex: i]
       byExtendingSelection: NO];
        return true;
      }
    }
  }
  for (NSView* sub in [v subviews])
    if (ns_test_select (sub, text)) return true;
  return false;
}

static void
ns_test_pull_buttons (NSView* v, NSMutableArray* a) {
  if ([v isKindOfClass: NSClassFromString (@"TMPullButton")]) [a addObject: v];
  for (NSView* sub in [v subviews]) ns_test_pull_buttons (sub, a);
}

static void
ns_test_combos (NSView* v, NSMutableArray* a) {
  if ([v isKindOfClass: [NSComboBox class]]) [a addObject: v];
  for (NSView* sub in [v subviews]) ns_test_combos (sub, a);
}

@interface TMWindowTester : NSObject
- (void) step: (NSTimer*) timer;
@end

@implementation TMWindowTester
- (id) init
{
  self= [super init];
  [[NSNotificationCenter defaultCenter]
    addObserver: self selector: @selector(tracking:)
           name: NSMenuDidBeginTrackingNotification object: nil];
  return self;
}
- (void) tracking: (NSNotification*) n
{
  [ns_test_menu release];
  ns_test_menu= [[n object] retain];
  fprintf (stderr, "NSTEST tracking %s\n",
           [n object] == [NSApp mainMenu]? "menu bar": "pop-up menu");
}
- (void) step: (NSTimer*) timer
{
  static int step= 0;
  NSArray* steps= [to_nsstring (get_env ("TEXMACS_NS_WINDOW_TEST"))
                    componentsSeparatedByString: @";"];
  if (step >= (int) [steps count]) { [timer invalidate]; return; }
  NSString* st= [steps objectAtIndex: step++];
  NSString* arg= @"";
  NSRange colon= [st rangeOfString: @":"];
  if (colon.location != NSNotFound) {
    arg= [st substringFromIndex: colon.location + 1];
    st= [st substringToIndex: colon.location];
  }
  NSArray* nums= [arg componentsSeparatedByString: @","];
  NSWindow* win= ns_test_main_window ();
  if ([st isEqualToString: @"close"] || [st isEqualToString: @"fullscreen"])
    for (NSWindow* w in [NSApp windows])
      if ([arg length] > 0 && [w isVisible] && [[w title] isEqualToString: arg])
        win= w;
  fprintf (stderr, "NSTEST step %s\n", [[steps objectAtIndex: step-1] UTF8String]);
  if ([st isEqualToString: @"close"]) [win performClose: nil];
  else if ([st isEqualToString: @"fullscreen"]) [win toggleFullScreen: nil];
  else if ([st isEqualToString: @"move"] && [nums count] == 2)
    [win setFrameTopLeftPoint:
       NSMakePoint ([[nums objectAtIndex: 0] doubleValue],
                    main_screen_height () - [[nums objectAtIndex: 1] doubleValue])];
  else if ([st isEqualToString: @"resize"] && [nums count] == 2)
    [win setContentSize: NSMakeSize ([[nums objectAtIndex: 0] doubleValue],
                                     [[nums objectAtIndex: 1] doubleValue])];
  else if ([st isEqualToString: @"front"]) {
    [NSApp activateIgnoringOtherApps: YES];
    for (NSWindow* w in [NSApp windows])
      if ([w isVisible] && [[w title] isEqualToString: arg])
        [w makeKeyAndOrderFront: nil];
  }
  else if ([st isEqualToString: @"main"]) {
    // as if the window became the main one (when TeXmacs is not active,
    // macOS does not change the main window)
    for (NSWindow* w in [NSApp windows])
      if ([w isVisible] && [[w title] isEqualToString: arg])
        [[NSNotificationCenter defaultCenter]
          postNotificationName: NSWindowDidBecomeMainNotification object: w];
  }
  else if ([st isEqualToString: @"windows"]) {
    fprintf (stderr, "NSTEST nr_windows %d\n", nr_windows);
    for (NSWindow* w in [NSApp windows]) {
      if (is_nil (ns_window_widget_of (w))) continue;
      NSRect f= [w frame];
      NSRect c= [w contentRectForFrameRect: f];
      fprintf (stderr, "NSTEST window '%s' %s%s%s%sat %.0f,%.0f content %.0fx%.0f\n",
               [[w title] UTF8String], [w isVisible]? "shown ": "hidden ",
               ns_test_menu_state (w),
               [w isMainWindow]? "main ": "",
               ([w styleMask] & NSWindowStyleMaskFullScreen)? "full-screen ": "",
               f.origin.x, main_screen_height () - NSMaxY (f),
               c.size.width, c.size.height);
    }
  }
  else if ([st isEqualToString: @"menubar"])
    ns_test_print_menu ([NSApp mainMenu], 0, [arg length] > 0? [arg intValue]: 0);
  else if ([st isEqualToString: @"menu"]) ns_test_print_menu (ns_test_menu, 0, 0);
  else if ([st isEqualToString: @"cancel"]) [ns_test_menu cancelTracking];
  else if ([st isEqualToString: @"begin-tracking"])
    [[NSNotificationCenter defaultCenter]
      postNotificationName: NSMenuDidBeginTrackingNotification
                    object: [NSApp mainMenu]];
  else if ([st isEqualToString: @"end-tracking"])
    [[NSNotificationCenter defaultCenter]
      postNotificationName: NSMenuDidEndTrackingNotification
                    object: [NSApp mainMenu]];
  else if ([st isEqualToString: @"select"]) {
    bool done= false;
    for (NSWindow* w in [NSApp orderedWindows])
      if (!done && [w isVisible]) done= ns_test_select ([w contentView], arg);
    fprintf (stderr, "NSTEST select %s\n", done? "done": "not found");
  }
  else if ([st isEqualToString: @"combo"]) {
    NSRange eq= [arg rangeOfString: @"="];
    NSMutableArray* a= [NSMutableArray array];
    for (NSWindow* w in [NSApp orderedWindows])
      if ([w isVisible]) ns_test_combos ([w contentView], a);
    int n= eq.location == NSNotFound? -1: [[arg substringToIndex: eq.location] intValue];
    if (n >= 0 && n < (int) [a count]) {
      NSComboBox* cb= [a objectAtIndex: n];
      NSWindow* w= [cb window];
      [w makeFirstResponder: cb];
      NSText* ed= [w fieldEditor: YES forObject: cb];
      [ed selectAll: nil];
      [ed insertText: [arg substringFromIndex: eq.location + 1]];
      [ed doCommandBySelector: @selector(insertNewline:)];
      fprintf (stderr, "NSTEST combo '%s'\n", [[cb stringValue] UTF8String]);
    }
    else fprintf (stderr, "NSTEST combo not found (%d)\n", (int) [a count]);
  }
  else if ([st isEqualToString: @"pull"]) {
    NSMutableArray* a= [NSMutableArray array];
    for (NSWindow* w in [NSApp orderedWindows])
      if ([w isVisible]) ns_test_pull_buttons ([w contentView], a);
    int n= [arg intValue];
    fprintf (stderr, "NSTEST pull buttons %d\n", (int) [a count]);
    // NOTE: the menu is shown after this step (it has its own event loop)
    if (n >= 0 && n < (int) [a count])
      [[a objectAtIndex: n] performSelector: @selector(performClick:)
                                 withObject: nil afterDelay: 0];
  }
  else if ([st isEqualToString: @"eval"])
    exec_delayed (scheme_cmd (from_nsstring (arg)));
  else if ([st isEqualToString: @"side-width"]) {
    // as the handle of the right side tools
    NSMutableArray* todo= [NSMutableArray arrayWithObject: [win contentView]];
    while ([todo count] > 0) {
      NSView* v= [todo lastObject];
      [todo removeLastObject];
      if ([v isKindOfClass: [TMSplitHandle class]] && ![v isHidden] &&
          ((TMSplitHandle*) v)->which == 0 && ((TMSplitHandle*) v)->wid) {
        ((TMSplitHandle*) v)->wid->tool_widths[0]= [arg doubleValue];
        ((TMSplitHandle*) v)->wid->layout ();
      }
      else [todo addObjectsFromArray: [v subviews]];
    }
  }
  else if ([st isEqualToString: @"side-scroll"] ||
           [st isEqualToString: @"side-pos"]) {
    // the scroll views of the side tools which are shown
    NSMutableArray* todo= [NSMutableArray arrayWithObject: [win contentView]];
    while ([todo count] > 0) {
      NSView* v= [todo lastObject];
      [todo removeLastObject];
      if ([v isKindOfClass: [NSScrollView class]] &&
          [[(NSScrollView*) v documentView] isKindOfClass: [TMFlippedToolView class]] &&
          ![v isHiddenOrHasHiddenAncestor]) {
        NSClipView* c= [(NSScrollView*) v contentView];
        if ([st isEqualToString: @"side-scroll"]) {
          [c scrollToPoint: NSMakePoint (0, [arg doubleValue])];
          [(NSScrollView*) v reflectScrolledClipView: c];
        }
        fprintf (stderr, "NSTEST side tools at %g (height %g)\n",
                 [c bounds].origin.y, [[(NSScrollView*) v documentView] frame].size.height);
      }
      else [todo addObjectsFromArray: [v subviews]];
    }
  }
}
@end

static void
ns_start_window_test () {
  static bool started= false;
  if (started || get_env ("TEXMACS_NS_WINDOW_TEST") == "") return;
  started= true;
  TMWindowTester* h= [[TMWindowTester alloc] init];
  NSTimer* t= [NSTimer timerWithTimeInterval: 1.0 target: h
                                    selector: @selector(step:)
                                    userInfo: nil repeats: YES];
  [t setFireDate: [NSDate dateWithTimeIntervalSinceNow: 4.0]];
  [[NSRunLoop currentRunLoop] addTimer: t forMode: NSRunLoopCommonModes];
}
