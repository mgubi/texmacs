
/******************************************************************************
* MODULE     : ns_gui.mm
* DESCRIPTION: Cocoa display class
* COPYRIGHT  : (C) 2018 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <locale.h>

#include "iterator.hpp"
#include "dictionary.hpp"
#include "analyze.hpp"
#include "language.hpp"
#include "locale.hpp"
#include "message.hpp"
#include "scheme.hpp"
#include "boot.hpp"
#include "sys_utils.hpp"

#include "ns_gui.h"
#include "ns_utilities.h"
#include "ns_renderer.h" // for the_ns_renderer
#include "MacOS/mac_utilities.h"


//extern hashmap<id, pointer> NSWindow_to_window;
//extern window (*get_current_window) (void);

ns_gui_rep* the_gui= NULL;

int nr_windows = 0; // FIXME: fake variable, referenced in tm_server

bool ns_update_flag= false;

int time_credit;
int timeout_time;

/******************************************************************************
 * Constructor and geometry
 ******************************************************************************/


ns_gui_rep::ns_gui_rep (int& argc, char** argv)
 : updatetimer (nil), interrupted (false), popup_wid_time (0),
   time_credit (100),
   do_check_events (false), updating (false), needing_update (false),
   selection (NULL)
{
  (void) argc; (void) argv;

  interrupted  = false;
  time_credit  = 100;
  timeout_time = texmacs_time () + time_credit;
  
  set_output_language (get_locale_language ());
  refresh_language();
  
  
  if (!retina_manual) {
    retina_manual= true;
    double mac_hidpi = mac_screen_scale_factor();
    if (DEBUG_STD)
      debug_boot << "Mac Screen scale factor: " << mac_hidpi <<  "\n";
    
    if (mac_hidpi == 2) {
      if (DEBUG_STD) debug_boot << "Setting up HiDPI mode\n";
      retina_factor= 2;
      if (!retina_iman) {
        retina_iman  = true;
        retina_icons = 2;
        // retina_icons = 1;
        // retina_icons = 2;  // FIXME: why is this not better?
      }
      retina_scale = 1.4;
    }
  }
  if (has_user_preference ("retina-factor"))
    retina_factor= get_user_preference ("retina-factor") == "on"? 2: 1;
  if (has_user_preference ("retina-icons"))
    retina_icons= get_user_preference ("retina-icons") == "on"? 2: 1;
  if (has_user_preference ("retina-scale"))
    retina_scale= as_double (get_user_preference ("retina-scale"));
}


/* important routines */
void
ns_gui_rep::get_extents (SI& width, SI& height) {
  coord2 size = from_nssize ([[NSScreen mainScreen] visibleFrame].size);
  width  = size.x1;
  height = size.x2;
}

void
ns_gui_rep::get_max_size (SI& width, SI& height) {
  width = 8000 * PIXEL;
  height= 6000 * PIXEL;
}

ns_gui_rep::~ns_gui_rep()  {
  // FIXME: update this
#if 0
  delete gui_helper;
  
  while (waitDialogs.count()) {
    waitDialogs.last()->deleteLater();
    waitDialogs.removeLast();
  }
  if (waitWindow) delete waitWindow;
  
  // delete updatetimer; we do not need this given that gui_helper is the
  // parent of updatetimer
#endif
}


/******************************************************************************
 * interclient communication
 ******************************************************************************/

/* NOTE: as in the Qt interface, the clipboard contains the selection in the
 format of TeXmacs (with the process which put it there), and as text or
 HTML for the other applications */

static NSString* texmacs_clipboard_type= @"org.texmacs.clipboard";
static NSString* texmacs_pid_type= @"org.texmacs.pid";

static bool
owns_pasteboard (NSPasteboard* pb) {
  NSString* pid= [pb stringForType: texmacs_pid_type];
  return pid && [pid intValue] == [[NSProcessInfo processInfo] processIdentifier];
}

bool
ns_gui_rep::get_selection (string key, tree& t, string& s, string format) {
  bool direct_selection= (key == "extern");
  if (direct_selection) key= "primary";
  s= "";
  t= "none";
  NSPasteboard *pb = [NSPasteboard generalPasteboard];
  bool owns= (format != "temp" && format != "wrapbuf" && key != "primary") ||
             (key == "primary" && owns_pasteboard (pb));
  if (owns) {
    if (!selection_t->contains (key)) return false;
    t= copy (selection_t [key]);
    s= copy (selection_s [key]);
    return true;
  }
  if (key != "primary") return false;

  string input_format;
  NSData* data= nil;
  if (format == "default") {
    if ((data= [pb dataForType: texmacs_clipboard_type]))
      input_format= "texmacs-snippet";
    else if ((data= [[pb stringForType: NSPasteboardTypeHTML]
                      dataUsingEncoding: NSUTF8StringEncoding]))
      input_format= "html-snippet";
    else if ((data= [[pb stringForType: NSPasteboardTypeString]
                      dataUsingEncoding: NSUTF8StringEncoding]))
      input_format= "verbatim-snippet";
    // FIXME: pictures, as in the Qt interface
  }
  else data= [[pb stringForType: NSPasteboardTypeString]
               dataUsingEncoding: NSUTF8StringEncoding];
  if (data && [data length] > 0)
    s << string ((char*) [data bytes], (int) [data length]);
  if (input_format != "" && !direct_selection)
    s= as_string (call ("convert", s, input_format, "texmacs-snippet"));
  if (input_format == "html-snippet") {
    tree h= as_tree (call ("convert", s, "texmacs-snippet", "texmacs-tree"));
    s= as_string (call ("convert", h, "texmacs-tree", "texmacs-snippet"));
  }
  t= tuple ("extern", s);
  return true;
}

bool
ns_gui_rep::set_selection (string key, tree t,
                           string s, string sv, string sh, string format) {
  (void) sh;
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  if (key != "primary") return true;
  NSPasteboard *pb = [NSPasteboard generalPasteboard];
  [pb clearContents];
  string text= s;
  if (format == "default") {
    c_string cs (s);
    [pb setData: [NSData dataWithBytes: (char*) cs length: N(s)]
        forType: texmacs_clipboard_type];
    [pb setString: [NSString stringWithFormat: @"%d",
                      [[NSProcessInfo processInfo] processIdentifier]]
          forType: texmacs_pid_type];
    text= sv;
  }
  c_string ct (text);
  NSString* str= [[[NSString alloc] initWithBytes: (char*) ct length: N(text)
                                         encoding: NSUTF8StringEncoding] autorelease];
  if (!str)
    str= [[[NSString alloc] initWithBytes: (char*) ct length: N(text)
                                 encoding: NSISOLatin1StringEncoding] autorelease];
  if (format == "html") [pb setString: str forType: NSPasteboardTypeHTML];
  else [pb setString: str forType: NSPasteboardTypeString];
  return true;
}

void
ns_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  if (key != "primary") return;
  NSPasteboard *pb = [NSPasteboard generalPasteboard];
  if (owns_pasteboard (pb)) [pb clearContents];
}


/******************************************************************************
 * Miscellaneous
 ******************************************************************************/

void ns_gui_rep::set_mouse_pointer (string name) { (void) name; }
// FIXME: implement this function
void ns_gui_rep::set_mouse_pointer (string curs_name, string mask_name)  { (void) curs_name; (void) mask_name; } ;

/******************************************************************************
 * Main loop
 ******************************************************************************/

static bool check_mask(int mask)
{
  NSEvent * event = [NSApp nextEventMatchingMask: mask
                                       untilDate: nil
                                          inMode: NSDefaultRunLoopMode
                                         dequeue: NO];
  // if (event != nil) NSLog(@"%@",event);
  return (event != nil);
  
}



void
ns_gui_rep::show_wait_indicator (widget w, string message, string arg) {
  // FIXME: implement show_wait_indicator
}


void (*the_interpose_handler) (void) = NULL;

void gui_interpose (void (*r) (void)) { the_interpose_handler= r; }

/******************************************************************************
* Queued processing (see qt_gui.cpp)
******************************************************************************/

@interface TMUpdateHelper : NSObject
- (void) doUpdate: (NSTimer*) timer;
@end

@implementation TMUpdateHelper
- (void) doUpdate: (NSTimer*) timer
{
  (void) timer;
  the_gui->update ();
}
@end

static TMUpdateHelper* update_helper= nil;

static void
start_update_timer (time_t delay) {
  // Run update () after delay milliseconds, when the other events are done
  if (!update_helper) update_helper= [[TMUpdateHelper alloc] init];
  NSTimer* t= [NSTimer timerWithTimeInterval: ((double) delay) / 1000.0
                                      target: update_helper
                                    selector: @selector(doUpdate:)
                                    userInfo: nil
                                     repeats: NO];
  [[NSRunLoop currentRunLoop] addTimer: t forMode: NSRunLoopCommonModes];
  [[NSRunLoop currentRunLoop] addTimer: t forMode: NSModalPanelRunLoopMode];
  if (the_gui->updatetimer) [the_gui->updatetimer invalidate];
  the_gui->updatetimer= t;
}

static int keyboard_events = 0;
static int keyboard_special= 0;

void
ns_gui_rep::process_queued_events (int max) {
  int count = 0;
  while (max < 0 || count < max)  {
    const queued_event& ev = waiting_events.next();
    if (ev.x1 == qp_type::QP_NULL) break;
    switch (ev.x1) {
      case qp_type::QP_NULL :
        break;
      case qp_type::QP_KEYPRESS :
      {
        typedef triple<widget, string, time_t > T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1)) {
          ((ns_simple_widget_rep*) x.x1.rep)->handle_keypress (x.x2, x.x3);
          keyboard_events++;
          if (N(x.x2) > 1) keyboard_special++;
        }
      }
        break;
      case qp_type::QP_KEYBOARD_FOCUS :
      {
        typedef triple<widget, bool, time_t > T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1))
          ((ns_simple_widget_rep*) x.x1.rep)->handle_keyboard_focus (x.x2, x.x3);
      }
        break;
      case qp_type::QP_MOUSE :
      {
        typedef sextuple<string, SI, SI, int, time_t, array<double> > T1;
        typedef pair<widget, T1> T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1))
          ((ns_simple_widget_rep*) x.x1.rep)->handle_mouse (x.x2.x1, x.x2.x2,
                                                            x.x2.x3, x.x2.x4,
                                                            x.x2.x5, x.x2.x6);
      }
        break;
      case qp_type::QP_RESIZE :
      {
        typedef triple<widget, SI, SI > T;
        T x = open_box <T> (ev.x2);
        if (!is_nil (x.x1))
          ((ns_simple_widget_rep*) x.x1.rep)->handle_notify_resize (x.x2, x.x3);
      }
        break;
      case qp_type::QP_COMMAND :
      {
        command cmd = open_box <command> (ev.x2) ;
        cmd->apply();
      }
        break;
      case qp_type::QP_COMMAND_ARGS :
      {
        typedef pair<command, object> T;
        T x = open_box <T> (ev.x2);
        x.x1->apply (x.x2);
      }
        break;
      case qp_type::QP_DELAYED_COMMANDS :
        delayed_commands.exec_pending();
        break;
      default:
        FAILED ("Unexpected queued event");
    }
    switch (ev.x1) {
      case qp_type::QP_COMMAND:
      case qp_type::QP_COMMAND_ARGS:
      case qp_type::QP_RESIZE:
      case qp_type::QP_DELAYED_COMMANDS:
        break;
      default:
        count++;
        break;
    }
  }
}

void
ns_gui_rep::process_keypress (ns_simple_widget_rep *wid, string key, time_t t) {
  typedef triple<widget, string, time_t > T;
  add_event (queued_event (qp_type::QP_KEYPRESS,
                           close_box<T> (T (wid, key, t))));
}

void
ns_gui_rep::process_keyboard_focus (ns_simple_widget_rep *wid, bool has_focus,
                                    time_t t) {
  typedef triple<widget, bool, time_t > T;
  add_event (queued_event (qp_type::QP_KEYBOARD_FOCUS,
                           close_box<T> (T (wid, has_focus, t))));
}

void
ns_gui_rep::process_mouse (ns_simple_widget_rep *wid, string kind, SI x, SI y,
                           int mods, time_t t, array<double> data) {
  typedef sextuple<string, SI, SI, int, time_t, array<double> > T1;
  typedef pair<widget, T1> T;
  add_event (queued_event (qp_type::QP_MOUSE,
                           close_box<T> (T (wid, T1 (kind, x, y, mods, t, data)))));
}

void
ns_gui_rep::process_resize (ns_simple_widget_rep *wid, SI x, SI y) {
  typedef triple<widget, SI, SI > T;
  add_event (queued_event (qp_type::QP_RESIZE, close_box<T> (T (wid, x, y))));
}

void
ns_gui_rep::process_command (command _cmd) {
  add_event (queued_event (qp_type::QP_COMMAND, close_box<command> (_cmd)));
}

void
ns_gui_rep::process_command (command _cmd, object _args) {
  typedef pair<command, object > T;
  add_event (queued_event (qp_type::QP_COMMAND_ARGS,
                           close_box<T> (T (_cmd,_args))));
}

void
ns_gui_rep::process_delayed_commands () {
  add_event (queued_event (qp_type::QP_DELAYED_COMMANDS, blackbox()));
}

bool
ns_gui_rep::check_event (int type) {
  // do not interrupt if not updating (e.g. while painting the icons in menus)
  if (!updating || !do_check_events) return false;
  switch (type) {
    case INTERRUPT_EVENT:
      if (interrupted) return true;
      else {
        time_t now = texmacs_time ();
        if (now - timeout_time < 0) return false;
        timeout_time = now + time_credit;
        interrupted  = !waiting_events.is_empty();
        return interrupted;
      }
    case INTERRUPTED_EVENT:
      return interrupted;
    default:
      return false;
  }
}

void
ns_gui_rep::set_check_events (bool enable_check) {
  do_check_events = enable_check;
}

void
ns_gui_rep::add_event (const queued_event& ev) {
  waiting_events.append (ev);
  if (updating) needing_update = true;
  else need_update();
}

void
ns_gui_rep::update () {
  time_t std_delay= 90 / 6;
  if (updating) {
    cout << "NESTED UPDATING: This should not happen" << LF;
    need_update();
    return;
  }
  if (updatetimer) { [updatetimer invalidate]; updatetimer= nil; }
  updating = true;

  static int count_events    = 0;
  static int max_proc_events = 40;

  time_t     now = texmacs_time();
  needing_update = false;
  time_credit    = 9 / (waiting_events.size() + 1);

  if (popup_wid_time > 0 && now > popup_wid_time) {
    popup_wid_time = 0;
    _popup_wid->send (SLOT_VISIBILITY, close_box<bool> (true));
  }

  // Delayed commands
  if (delayed_commands.must_wait (now))
    process_delayed_commands();

  // Pending events, until the limit is reached
  while (waiting_events.size() > 0 && count_events < max_proc_events) {
    process_queued_events (1);
    count_events++;
  }

  // Repaint invalid regions and redraw
  bool postpone_treatment= (keyboard_events > 0 && keyboard_special == 0);
  keyboard_events = 0;
  keyboard_special= 0;
  count_events    = 0;

  interrupted  = false;
  timeout_time = texmacs_time() + time_credit;

  if (!postpone_treatment) {
    if (the_interpose_handler) the_interpose_handler();
    ns_simple_widget_rep::repaint_all ();
  }

  if (waiting_events.size() > 0) needing_update = true;
  if (interrupted)               needing_update = true;
  if (nr_windows == 0) {
    [NSApp stop: nil];
    // NOTE: stop only takes effect after an event
    [NSApp postEvent: [NSEvent otherEventWithType: NSEventTypeApplicationDefined
                                         location: NSZeroPoint
                                    modifierFlags: 0
                                        timestamp: 0
                                     windowNumber: 0
                                          context: nil
                                          subtype: 0
                                            data1: 0
                                            data2: 0]
             atStart: YES];
  }

  time_t delay = delayed_commands.lapse - texmacs_time();
  if (needing_update) delay = 0;
  else                delay = max ((time_t) 0, min (std_delay, delay));
  if (postpone_treatment) delay= 9; // NOTE: force occasional display

  start_update_timer (delay);
  updating = false;
}

void
ns_gui_rep::force_update () {
  if (updating) needing_update = true;
  else          update();
}

void
ns_gui_rep::need_update () {
  if (updating) needing_update = true;
  else          start_update_timer (0);
}

void
ns_gui_rep::refresh_language () {
  // FIXME: update the texts of the menus
}

/******************************************************************************
* Snapshots of the windows (for testing)
******************************************************************************/

// NOTE: when the environment variable TEXMACS_NS_SNAPSHOT is a directory,
// the windows are saved as dir/window-<i>.png every few seconds, since other
// programs are not allowed to capture the windows of TeXmacs

static void
ns_snapshot (string dir) {
  int n= 0;
  for (NSWindow* win in [NSApp windows]) {
    if (![win isVisible]) continue;
    NSView* v= [[win contentView] superview];
    if (!v) v= [win contentView];
    NSRect r= [v bounds];
    NSBitmapImageRep* rep= [v bitmapImageRepForCachingDisplayInRect: r];
    if (!rep) continue;
    [v cacheDisplayInRect: r toBitmapImageRep: rep];
    NSData* data= [rep representationUsingType: NSBitmapImageFileTypePNG
                                    properties: [NSDictionary dictionary]];
    string name= dir * "/window-" * as_string (n++) * ".png";
    [data writeToFile: to_nsstring (name) atomically: NO];
  }
}

@interface TMSnapshotHelper : NSObject
- (void) snapshot: (NSTimer*) timer;
@end

@implementation TMSnapshotHelper
- (void) snapshot: (NSTimer*) timer
{
  (void) timer;
  string dir= get_env ("TEXMACS_NS_SNAPSHOT");
  if (dir != "") ns_snapshot (dir);
}
@end

// NOTE: when the environment variable TEXMACS_NS_TYPE is set, its characters
// are sent as key events to the key window after two seconds (\r stands for
// return and \b for backspace), for testing the keyboard handling

@interface TMTypeHelper : NSObject
- (void) type: (NSTimer*) timer;
@end

@implementation TMTypeHelper
- (void) type: (NSTimer*) timer
{
  (void) timer;
  string click= get_env ("TEXMACS_NS_CLICK");
  string text= get_env ("TEXMACS_NS_TYPE");
  text= replace (replace (text, "\\r", "\r"), "\\b", "\x7f");
  NSWindow* win= [NSApp keyWindow];
  if (!win) win= [[NSApp windows] firstObject];
  NSView* v= [win firstResponder];
  // NOTE: the window does not become the key window when TeXmacs is not the
  // active application, so that the canvas does not get the focus
  if ([v respondsToSelector: @selector(focusIn)])
    [v performSelector: @selector(focusIn)];
  if (click != "" && [v isKindOfClass: [NSView class]]) {
    // a click at the point x,y of the view which has the focus
    // x,y or x,y,right
    array<string> xy= tokenize (click, ",");
    bool right= N(xy) > 2 && xy[2] == "right";
    NSPoint p= NSMakePoint (as_double (xy[0]), as_double (xy[1]));
    p= [v convertPoint: p toView: nil];
    for (int up=0; up<2; up++) {
      NSEvent* e= [NSEvent mouseEventWithType:
                             right? (up? NSEventTypeRightMouseUp: NSEventTypeRightMouseDown)
                                  : (up? NSEventTypeLeftMouseUp: NSEventTypeLeftMouseDown)
                                     location: p
                                modifierFlags: 0
                                    timestamp: [[NSProcessInfo processInfo] systemUptime]
                                 windowNumber: [win windowNumber]
                                      context: nil
                                  eventNumber: 0
                                   clickCount: 1
                                     pressure: 1.0];
      [NSApp postEvent: e atStart: NO];
    }
  }
  NSString* all= to_nsstring (text);
  for (NSUInteger i=0; i<[all length]; i++) {
    NSString* c= [all substringWithRange: NSMakeRange (i, 1)];
    for (int up=0; up<2; up++) {
      NSEvent* e= [NSEvent keyEventWithType: up? NSEventTypeKeyUp: NSEventTypeKeyDown
                                   location: NSZeroPoint
                              modifierFlags: 0
                                  timestamp: [[NSProcessInfo processInfo] systemUptime]
                               windowNumber: [win windowNumber]
                                    context: nil
                                 characters: c
                charactersIgnoringModifiers: c
                                  isARepeat: NO
                                    keyCode: 0];
      [NSApp postEvent: e atStart: NO];
    }
  }
}
@end

// NOTE: when the environment variable TEXMACS_NS_MENUS is set, the menu bar
// is printed after three seconds, with the submenus up to that depth

static void
ns_print_menu (NSMenu* m, int depth, int max_depth) {
  if (!m || depth > max_depth) return;
  if ([m delegate] && [[m delegate] respondsToSelector: @selector(menuNeedsUpdate:)])
    [[m delegate] menuNeedsUpdate: m];
  for (NSMenuItem* mi in [m itemArray]) {
    string title= [mi isSeparatorItem]? string ("---")
                                      : from_nsstring ([mi title]);
    if ([mi image] && N(title) == 0) title= "[icon]";
    if (![mi isEnabled]) title= title * " (disabled)";
    fprintf (stderr, "NSMENU %s%s\n",
             as_charp (string (' ', 2 * depth)), as_charp (title));
    if ([mi hasSubmenu]) ns_print_menu ([mi submenu], depth + 1, max_depth);
  }
}

@interface TMMenuPrinter : NSObject
- (void) print: (NSTimer*) timer;
@end

@implementation TMMenuPrinter
- (void) print: (NSTimer*) timer
{
  (void) timer;
  ns_print_menu ([NSApp mainMenu], 0,
                 as_int (get_env ("TEXMACS_NS_MENUS")));
}
@end

// NOTE: when the environment variable TEXMACS_NS_PRESS is set, the button
// or the tab with this label is pressed after four seconds

static bool
ns_press (NSView* v, NSString* label) {
  if ([v isKindOfClass: [NSTabView class]]) {
    for (NSTabViewItem* it in [(NSTabView*) v tabViewItems])
      if ([[it label] isEqualToString: label]) {
        [(NSTabView*) v selectTabViewItem: it];
        return true;
      }
  }
  if ([v isKindOfClass: [NSButton class]] &&
      [[(NSButton*) v title] isEqualToString: label]) {
    [(NSButton*) v performClick: nil];
    return true;
  }
  for (NSView* sub in [v subviews])
    if (ns_press (sub, label)) return true;
  return false;
}

@interface TMPressHelper : NSObject
- (void) press: (NSTimer*) timer;
@end

@implementation TMPressHelper
- (void) press: (NSTimer*) timer
{
  (void) timer;
  NSString* label= to_nsstring (get_env ("TEXMACS_NS_PRESS"));
  bool done= false;
  for (NSWindow* win in [[[NSApp orderedWindows] copy] autorelease])
    if (!done && [win isVisible])
      done= ns_press ([win contentView], label);
  fprintf (stderr, "TEXMACS_NS_PRESS %s\n", done? "done": "not found");
}
@end

void
ns_gui_rep::event_loop () {
  [NSApp finishLaunching];
  need_update ();
  if (get_env ("TEXMACS_NS_TYPE") != "" || get_env ("TEXMACS_NS_SNAPSHOT") != "") {
    // NOTE: when testing, TeXmacs is started in the background; the window
    // must be the key window, otherwise the editor loses its focus
    [NSApp activateIgnoringOtherApps: YES];
    [[[NSApp windows] firstObject] makeKeyAndOrderFront: nil];
  }
  if (get_env ("TEXMACS_NS_PRESS") != "") {
    TMPressHelper* h= [[TMPressHelper alloc] init];
    [NSTimer scheduledTimerWithTimeInterval: 4.0 target: h
                                   selector: @selector(press:)
                                   userInfo: nil repeats: NO];
  }
  if (get_env ("TEXMACS_NS_MENUS") != "") {
    TMMenuPrinter* h= [[TMMenuPrinter alloc] init];
    [NSTimer scheduledTimerWithTimeInterval: 3.0 target: h
                                   selector: @selector(print:)
                                   userInfo: nil repeats: NO];
  }
  if (get_env ("TEXMACS_NS_TYPE") != "") {
    TMTypeHelper* h= [[TMTypeHelper alloc] init];
    [NSTimer scheduledTimerWithTimeInterval: 2.0 target: h
                                   selector: @selector(type:)
                                   userInfo: nil repeats: NO];
  }
  if (get_env ("TEXMACS_NS_SNAPSHOT") != "") {
    // NOTE: also while menus are tracked or dialogs are modal
    TMSnapshotHelper* h= [[TMSnapshotHelper alloc] init];
    NSTimer* t= [NSTimer timerWithTimeInterval: 3.0 target: h
                                      selector: @selector(snapshot:)
                                      userInfo: nil repeats: YES];
    [[NSRunLoop currentRunLoop] addTimer: t forMode: NSRunLoopCommonModes];
  }
  [NSApp run];
}

/* interface ******************************************************************/
#pragma mark GUI interface

//static display cur_display= NULL;

static NSAutoreleasePool *pool = nil;
//static NSApplication *app = nil;


/******************************************************************************
* Main routines
******************************************************************************/

/*! Quitting from the application menu goes through TeXmacs, which asks
 about unsaved documents. */
@interface TMQuitHelper : NSObject
- (void) quit: (id) sender;
@end

@implementation TMQuitHelper
- (void) quit: (id) sender
{
  (void) sender;
  exec_delayed (scheme_cmd ("(safely-quit-TeXmacs)"));
  the_gui->need_update ();
}
@end

static void
make_main_menu () {
  // The main menu, when it does not come from MainMenu.nib (as when TeXmacs
  // is not started from an application bundle); the menus of TeXmacs are
  // added after the application menu (see TMMenuHelper)
  static TMQuitHelper* quit_helper= [[TMQuitHelper alloc] init];
  NSMenu* main= [[[NSMenu alloc] initWithTitle: @"MainMenu"] autorelease];
  NSMenuItem* app_item= [[[NSMenuItem alloc] initWithTitle: @"TeXmacs"
                                                    action: NULL
                                             keyEquivalent: @""] autorelease];
  NSMenu* app= [[[NSMenu alloc] initWithTitle: @"TeXmacs"] autorelease];
  [app addItemWithTitle: @"About TeXmacs"
                 action: @selector(orderFrontStandardAboutPanel:)
          keyEquivalent: @""];
  [app addItem: [NSMenuItem separatorItem]];
  [app addItemWithTitle: @"Hide TeXmacs" action: @selector(hide:)
          keyEquivalent: @"h"];
  NSMenuItem* others= [app addItemWithTitle: @"Hide Others"
                                     action: @selector(hideOtherApplications:)
                              keyEquivalent: @"h"];
  [others setKeyEquivalentModifierMask:
            NSEventModifierFlagCommand | NSEventModifierFlagOption];
  [app addItemWithTitle: @"Show All"
                 action: @selector(unhideAllApplications:)
          keyEquivalent: @""];
  [app addItem: [NSMenuItem separatorItem]];
  NSMenuItem* quit= [app addItemWithTitle: @"Quit TeXmacs"
                                   action: @selector(quit:)
                            keyEquivalent: @"q"];
  [quit setTarget: quit_helper];
  [app_item setSubmenu: app];
  [main addItem: app_item];
  [NSApp setMainMenu: main];
}

void gui_open (int& argc, char** argv)
  // start the gui
{
  if (!NSApp) {
    // initialize app
    [NSApplication sharedApplication];
    [NSBundle loadNibNamed:@"MainMenu" owner:NSApp];
    // NOTE: needed for a menu bar when TeXmacs is not in a bundle
    [NSApp setActivationPolicy: NSApplicationActivationPolicyRegular];
    // NOTE: otherwise these items are added to the Edit menu each time the
    // menu bar is rebuilt
    NSUserDefaults* d= [NSUserDefaults standardUserDefaults];
    [d setBool: YES forKey: @"NSDisabledDictationMenuItem"];
    [d setBool: YES forKey: @"NSDisabledCharacterPaletteMenuItem"];
    if (![NSApp mainMenu]) make_main_menu ();
  }
  if (!pool) {
    // create autorelease pool 
    pool = [[NSAutoreleasePool alloc] init];
  } else [pool retain];
  
  the_gui = tm_new <ns_gui_rep> (argc, argv);
}

void gui_start_loop ()
  // start the main loop
{
  the_gui->event_loop ();
}

void gui_close ()
  // cleanly close the gui
{
  ASSERT (the_gui != NULL, "gui not yet open");
  [pool release];
  tm_delete (the_gui);
  the_gui = NULL;
}

void
gui_root_extents (SI& width, SI& height) {   
	// get the screen size
  the_gui->get_extents (width, height);
}

void
gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  the_gui->get_max_size (width, height);
}

void gui_refresh ()
{
  // update and redraw all windows (e.g. on change of output language)
  // FIXME: add suitable code
}



/******************************************************************************
* Font support
******************************************************************************/

void
set_default_font (string name) {
	(void) name;
  // set the name of the default font
  // this is ignored since Qt handles fonts for the widgets
}

font
get_default_font (bool tt, bool mini, bool bold) {
  (void) tt; (void) mini;
  // get the default font or monospaced font (if tt is true)
	
  // return a null font since this function is not called in the Qt port.
  if (DEBUG_EVENTS) cout << "get_default_font(): SHOULD NOT BE CALLED\n";
  return NULL;
  //return tex_font (this, "ecrm", 10, 300, 0);
}

// load the metric and glyphs of a system font
// you are not obliged to provide any system fonts

void
load_system_font (string family, int size, int dpi,
                  font_metric& fnm, font_glyphs& fng)
{
	(void) family; (void) size; (void) dpi; (void) fnm; (void) fng;
	if (DEBUG_EVENTS) cout << "load_system_font(): SHOULD NOT BE CALLED\n";
}

/******************************************************************************
* Clipboard support
******************************************************************************/

// Copy a selection 't' with string equivalent 's' to the clipboard 'cb'
// and possibly the variants 'sv' and 'sh' for verbatim and html
// Returns true on success
bool
set_selection (string key, tree t,
               string s, string sv, string sh, string format) {
  return the_gui->set_selection (key, t, s, sv, sh, format);
}

  // Retrieve the selection 't' with string equivalent 's' from clipboard 'cb'
  // Returns true on success; sets t to (extern s) for external selections
bool
get_selection (string key, tree& t, string& s, string format) { 
  return the_gui->get_selection (key, t, s, format);
}

  // Clear the selection on clipboard 'cb'
void
clear_selection (string key) {
  the_gui->clear_selection (key);
}


/******************************************************************************
* Miscellaneous
******************************************************************************/
int char_clip=0;

void 
beep () {
  // Issue a beep
  NSBeep ();
}

void 
needs_update () {
  the_gui->need_update ();
}

bool check_event (int type)
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
{ return the_gui->check_event(type); }

void image_gc (string name) {
  // Garbage collect images of a given name (may use wildcards)
  // NOTE: not used by TeXmacs any more (nor implemented by Qt)
  (void) name;
}

void
show_help_balloon (widget balloon, SI x, SI y) {
  // FIXME: implement
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
  (void) balloon; (void) x; (void) y;
}

void
show_wait_indicator (widget base, string message, string argument) {
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty
  the_gui->show_wait_indicator(base,message,argument); 
}

void
external_event (string type, time_t t) {
  // External events, such as pushing a button of a remote infrared commander
#if 0
  QTMWidget *tm_focus = qobject_cast<QTMWidget*>(qApp->focusWidget());
  if (tm_focus) {
    simple_widget_rep *wid = tm_focus->tm_widget();
    if (wid) the_gui -> process_keypress (wid, type, t);
  }
#endif
}


/******************************************************************************
 * Delayed commands
 ******************************************************************************/

command_queue::command_queue() : lapse (0), wait (true) { }
command_queue::~command_queue() { clear_pending(); /* implicit */ }

void
command_queue::exec (object cmd) {
  q << cmd;
  start_times << (((time_t) texmacs_time ()) - 1000000000);
  lapse = texmacs_time();
  the_gui->need_update();
  wait= true;
}

void
command_queue::exec_pause (object cmd) {
  q << cmd;
  start_times << ((time_t) texmacs_time ());
  lapse = texmacs_time();
  the_gui->need_update();
  wait= true;
}

void
command_queue::exec_pending () {
  array<object> a = q;
  array<time_t> b = start_times;
  q = array<object> (0);
  start_times = array<time_t> (0);
  int i, n = N(a);
  for (i = 0; i<n; i++) {
    time_t now =  texmacs_time ();
    if ((now - b[i]) >= 0) {
      object obj = call (a[i]);
      if (is_int (obj) && (now - b[i] < 1000000000)) {
        time_t pause = as_int (obj);
        //cout << "pause = " << obj << "\n";
        q << a[i];
        start_times << (now + pause);
      }
    }
    else {
      q << a[i];
      start_times << b[i];
    }
  }
  if (N(q) > 0) {
    wait = true;  // wait_for_delayed_commands
    lapse = start_times[0];
    int n = N(start_times);
    for (i = 1; i<n; i++) {
      if (lapse > start_times[i]) lapse = start_times[i];
    }
  } else
    wait = false;
}

void
command_queue::clear_pending () {
  q = array<object> (0);
  start_times = array<time_t> (0);
  wait = false;
}

bool
command_queue::must_wait (time_t now) const {
  return wait && (lapse <= now);
}


/******************************************************************************
 * Delayed commands interface
 ******************************************************************************/

void exec_delayed (object cmd) {
  the_gui->delayed_commands.exec (cmd);
}
void exec_delayed_pause (object cmd) {
  the_gui->delayed_commands.exec_pause (cmd);
}
void clear_pending_commands () {
  the_gui->delayed_commands.clear_pending ();
}


/******************************************************************************
 * Queued events
 ******************************************************************************/

event_queue::event_queue() : n(0) { }

void
event_queue::append (const queued_event& ev) {
  q << ev;
  ++n;
}

queued_event
event_queue::next () {
  if (is_nil(q))
    return queued_event();
  queued_event ev = q->item;
  q = q->next;
  --n;
  return ev;
}

bool
event_queue::is_empty() const {
  ASSERT (!(n!=0 && is_nil(q)), "WTF?");
  return n == 0;
}

int
event_queue::size() const {
  return n;
}




string
gui_version () {
  return "ns";
}
