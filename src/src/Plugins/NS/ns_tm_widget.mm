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

#import "TMView.h"
#import "TMButtonsController.h"

#pragma mark ns_tm_widget_rep

NSString *TMToolbarIdentifier = @"TMToolbarIdentifier";
NSString *TMButtonsIdentifier = @"TMButtonsIdentifier";

@interface TMToolbarItem : NSToolbarItem
@end
NSColor* to_nscolor (color col);

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
  [leftField setBackgroundColor:[NSColor windowFrameColor]];
  [rightField setBackgroundColor:[NSColor windowFrameColor]];
  [leftField setBezeled:NO];
  [rightField setBezeled:NO];
  [rightField setAlignment:NSRightTextAlignment];
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
  }
	
  toolbar = [[NSToolbar alloc] initWithIdentifier:TMToolbarIdentifier ];
  [toolbar setDelegate:wh];
  
  updateVisibility();
  
}

ns_tm_widget_rep::~ns_tm_widget_rep() 
{ 
  [[NSNotificationCenter defaultCenter] removeObserver: wh];
  for (int i=0; i<4; i++) [tool_views[i] release];
  [wh release];	
  [bc release]; 
}



static NSSize
tool_size (NSView* v) {
  // The size wanted by the contents of a tool container
  if ([[v subviews] count] == 0) return NSZeroSize;
  NSSize fs= [[[v subviews] firstObject] fittingSize];
  return NSMakeSize (fs.width + 8, fs.height + 8);
}

void ns_tm_widget_rep::layout()
{
  // From top to bottom: the icon bars, the left tools, the canvas and the
  // side tools, the bottom and extra tools, and the footer
  NSSize fs = NSMakeSize (100, 20); // size of the right footer
  NSRect r = [view bounds];
  CGFloat bar_h = (visibility[1] || visibility[2] || visibility[3] ||
                   visibility[4])? [[bc bar] frame].size.height: 0;
  CGFloat foot_h= visibility[5]? fs.height: 0;
  bool show[4];
  NSSize sz[4];
  for (int i=0; i<4; i++) {
    sz[i]= tool_size (tool_views[i]);
    show[i]= visibility[6+i] && sz[i].width > 0 && sz[i].height > 0;
    [tool_views[i] setHidden: !show[i]];
  }
  CGFloat side_w = show[0]? min (sz[0].width, r.size.width / 2): 0;
  CGFloat left_w = show[1]? min (sz[1].width, r.size.width / 2): 0;
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
  [tool_views[2] setFrame: NSMakeRect (0, foot_h + extra_h, r.size.width, bot_h)];
  [tool_views[3] setFrame: NSMakeRect (0, foot_h, r.size.width, extra_h)];
  [leftField setFrame: NSMakeRect (0, 0, r.size.width - fs.width, foot_h)];
  [rightField setFrame: NSMakeRect (r.size.width - fs.width, 0,
                                    fs.width, foot_h)];
  [leftField setHidden: foot_h == 0];
  [rightField setHidden: foot_h == 0];
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

void ns_tm_widget_rep::updateVisibility()
{
  // FIXME: the rows of icons are shown or hidden together
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
      if (open_box<string> (val) == "canvas" && !is_nil (main_widget)) {
        NSView* v= canvas_of (concrete (main_widget)->as_nsview ());
        if (v && [v window]) [[v window] makeFirstResponder: v];
        the_gui->process_keyboard_focus
          ((ns_simple_widget_rep*) main_widget.rep, true, texmacs_time ());
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
  // FIXME: the focus icons and the tools are not shown yet
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
      if (open_box<bool>(val) == true) {
        //FIXME: to postpone once we return to the runloop
	    do_interactive_prompt();
      }
    }
    break;
    
  case SLOT_FILE:
    {
      check_type<string> (val, s);
      string file = open_box<string> (val);
      if (DEBUG_EVENTS) cout << "File: " << file << LF;
//      view->window()->setWindowFilePath(to_qstring(file));
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
      return close_box<string> ( ((ns_input_text_widget_rep*) int_input.rep)->get_input () );
      
    }
  case SLOT_INTERACTIVE_MODE:
    {
      check_type_id<bool> (type_id, s);
      return close_box<bool> (false);  // FIXME: who needs this info?
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
      if (v) {
        [v setTranslatesAutoresizingMaskIntoConstraints: NO];
        [tool_views[i] addSubview: v];
        [NSLayoutConstraint activateConstraints: @[
          [v.leadingAnchor constraintEqualToAnchor: tool_views[i].leadingAnchor constant: 4],
          [v.trailingAnchor constraintEqualToAnchor: tool_views[i].trailingAnchor constant: -4],
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
  return wid;
}

/******************************************************************************
* Interactive prompt
******************************************************************************/

void
ns_tm_widget_rep::do_interactive_prompt () {
  // FIXME: the Qt interface shows the prompt in the footer
  if (is_nil (int_prompt) || is_nil (int_input)) return;
  NSStackView* sv= [[[NSStackView alloc] init] autorelease];
  [sv setOrientation: NSUserInterfaceLayoutOrientationHorizontal];
  NSView* p= int_prompt->as_nsview ();
  NSView* i= int_input->as_nsview ();
  if (p) [sv addArrangedSubview: p];
  if (i) {
    [sv addArrangedSubview: i];
    [[i.widthAnchor constraintGreaterThanOrEqualToConstant: 250] setActive: YES];
  }
  [sv setFrameSize: [sv fittingSize]];
  NSAlert* alert= [[[NSAlert alloc] init] autorelease];
  [alert setMessageText: @""];
  [alert setAccessoryView: sv];
  [alert addButtonWithTitle: @"OK"];
  [alert addButtonWithTitle: @"Cancel"];
  if (i) [[alert window] setInitialFirstResponder: i];
  bool ok= [alert runModal] == NSAlertFirstButtonReturn;
  ((ns_input_text_widget_rep*) int_input.rep)->commit (ok);
}
