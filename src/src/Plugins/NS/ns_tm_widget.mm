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
  visibility[0] = (mask & 1)  == 1;  // header
  visibility[1] = (mask & 2)  == 2;  // main
  visibility[2] = (mask & 4)  == 4;  // context
  visibility[3] = (mask & 8)  == 8;  // user
  visibility[4] = (mask & 16) == 16; // footer
  
  
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
  //  [sv setBackgroundColor:[NSColor redColor]];
  [sv setBackgroundColor:[NSColor grayColor]];
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
  //NSView *mt = [bc bar];
  //[mt setFrame:r0];
  //[mt setAutoresizingMask:NSViewMaxXMargin|NSViewMinYMargin];
  //[view addSubview:mt];
  //	[mt setPostsFrameChangedNotifications:YES];
  wh = [[TMWidgetHelper alloc] init];
  wh->wid = this;
#if 0
  [(NSNotificationCenter*)[NSNotificationCenter defaultCenter] addObserver:wh
			  selector:@selector(notify:)
			  name:NSViewFrameDidChangeNotification 
			  object:mt];
#endif
	
  toolbar = [[NSToolbar alloc] initWithIdentifier:TMToolbarIdentifier ];
  [toolbar setDelegate:wh];
  
  updateVisibility();
  
}

ns_tm_widget_rep::~ns_tm_widget_rep() 
{ 
  //	[(NSNotificationCenter*)[NSNotificationCenter defaultCenter] removeObserver:wh];
  [wh release];	
  [bc release]; 
}


void ns_tm_widget_rep::layout()
{
  NSSize s = NSMakeSize(100,20); // size of the right footer;
  NSRect r = [view bounds];
  NSRect r0 = r;
  //	NSRect rh = [[bc bar] frame];
  NSRect rh = NSMakeRect(0,0,0,0);
  r.size.height -= rh.size.height;
  r0.origin.y =+ r.size.height; r0.size.height = rh.size.height;
  NSRect r1 = r; r1.origin.y += s.height; r1.size.height -= s.height;
  NSRect r2 = r; r2.size.height = s.height;
  NSRect r3 = r2; 
  r2.size.width -= s.width; r3.origin.x =+ r2.size.width;
  r3.size.width -= r2.size.width + 15.0;
  [sv setFrame:r1];
  [leftField setFrame:r2];
  [rightField setFrame:r3];
  //[[bc bar] setFrame:r0];
  [NSApp setWindowsNeedUpdate:YES];
}


void ns_tm_widget_rep::updateVisibility()
{
  //FIXME: this implementation is from the Qt port. to be adapted.
#if 0
  mainToolBar->setVisible (visibility[1] && visibility[0]);
  contextToolBar->setVisible (visibility[2] && visibility[0]);
  userToolBar->setVisible (visibility[3] && visibility[0]);
  tm_mainwindow()->statusBar()->setVisible (visibility[4]);
#ifndef Q_WS_MAC
  tm_mainwindow()->menuBar()->setVisible (visibility[0]);
#endif
#endif
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
      visibility[3] = f;
      updateVisibility();
    }
    break;
  case SLOT_FOOTER_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool f= open_box<bool> (val);
      visibility[4] = f;
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
    return close_box<bool> (visibility[3]);
        
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
    return close_box<bool> (visibility[4]);
    
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
  
  mi = [[NSMenuItem allocWithZone:[NSMenu menuZone]] initWithTitle:@"Menu" action:NULL keyEquivalent:@""];
  NSMenu *sm = [[[NSMenu allocWithZone:[NSMenu menuZone]] initWithTitle:@"Menu"] autorelease];
  [mi  setSubmenu:sm];
  //[[NSApp mainMenu] removeItem: [[NSApp mainMenu] itemWithTitle:@"Help"]]; //FIXME: Help menu causes problems (crash)
  
  [[NSApp mainMenu] insertItem: mi atIndex:1];	
  //	[sm setDelegate: self];
  
  return self; 
}
- (void)setMenu:(NSMenu *)_m  
{ 
  if (menu) [menu release];  menu = _m; [menu retain];
  [mi  setSubmenu:menu];
  [menu setTitle:@"Menu"];	
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
  case SLOT_SCROLLABLE: 
    {
      check_type_void (index, s);
      main_widget = w;
      NSView *v = concrete (w)->as_nsview ();
      [sv setDocumentView: v];
      [[sv window] makeFirstResponder:v];
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
  [[wid->get_windowcontroller() window] setToolbar:toolbar];
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
