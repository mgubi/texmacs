
/******************************************************************************
* MODULE     : ns_widget.mm
* DESCRIPTION: Aqua widget class
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "window.hpp"

#include "mac_cocoa.h" 
#include "ns_utilities.h"
#include "ns_widget.h"
#include "ns_ui_element.h"
#include "ns_other_widgets.h"
#include "ns_menu.h"
#include "gui.hpp"


/******************************************************************************
 * ns_widget_rep: the base widget for the NS port.
 ******************************************************************************/


static long widget_counter = 0;

ns_widget_rep::ns_widget_rep (types _type)
  : widget_rep (), id (widget_counter++), type (_type), parent (NULL)
{
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_widget_rep: created a " << type_as_string() << LF;
}

ns_widget_rep::~ns_widget_rep() {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "~ns_widget_rep: deleted a " << type_as_string() << LF;
}

void
ns_widget_rep::add_child (widget w) {
  children << w;
  concrete(w)->parent = this;
}

void
ns_widget_rep::add_children (array<widget> a) {
  children << a;
  for (int i = 0; i < N(a); ++i)
    if (!is_nil(a[i])) concrete(a[i])->parent = this;
}

void
ns_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_KEYBOARD_FOCUS:
    {
      check_type<bool> (val, s);
      bool focus = open_box<bool> (val);
      // FIXME: maybe handle elsewhere
      //if (focus && qwid && !qwid->hasFocus())
      //  qwid->setFocus (Qt::OtherFocusReason);
    }
      break;
    case SLOT_NAME:
    {
      if (parent) parent->send (SLOT_NAME, val);
    }
      break;
    case SLOT_KEYBOARD_FOCUS_ON:
    case SLOT_DESTROY:
    {
      if (DEBUG_QT_WIDGETS)
        debug_widgets << "Resending to " << N(children) << " children" << LF;
      for (int i = 0; i < N(children); ++i)
        if (!is_nil(children[i])) children[i]->send (s, val);
    }
      break;
    default:
      if (DEBUG_QT_WIDGETS)
        debug_widgets << "ns_widget_rep::send(), unhandled " << slot_name (s)
        << " for widget of type: " << type_as_string() << ".\n";
  }
}

tm_ostream& operator << (tm_ostream& out, ns_widget w) {
  return out << "ns_widget of type: " << w.rep->type_as_string();
}


/*! Returns the widget as a window.
 
 Each TeXmacs widget can at some point be asked to present itself into a window.
 The scheme-originating function window_create () expects this method in every
 widget.
 
 This default implementation constructs a wrapper ns_window_widget for the
 widget and returns it. This wrapper will hold a new QTMPlainWindow object
 which will manage close events and take ownership of the original QWidget.
 ns_window_widget owns the QTMPlainWindow and is responsible for its deletion.
 
 The default implementation should suffice in most cases.
 
 \param name A unique identifier for the window. This is *not* the window title.
 \param quit A command to be executed when the window closes.
 \return The new ns_window_widget.
 */
widget
ns_widget_rep::plain_window_widget (string name, command quit) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_widget_rep::plain_window_widget() around a "
    << type_as_string() << LF;
  
  ns_window_widget_rep* wid = tm_new<ns_window_widget_rep> (this, name, quit);
  return widget (wid);
}

/*! Instantiates and returns a new widget which will act as a popup widget.
 
 This is used by popup_window_widget: subclasses reimplement this method and
 return the appropriate widget, and ns_widget_rep::popup_window_widget()
 is the "interface".
 */
widget
ns_widget_rep::make_popup_widget () {
  return tm_new<ns_popup_widget_rep> ((widget_rep*)this, command());
}

/*! Interface for the creation of popups.
 FIXME: the check below should be unnecessary, but current design is ugly.
 */
widget
ns_widget_rep::popup_window_widget (string s) {
  widget wid= make_popup_widget();
  ASSERT(concrete(wid) != this, "Loop in call to popup_window_widget()");
  return concrete(wid)->popup_window_widget(s);
}

/******************************************************************************
 * ns_window_widget_rep
 ******************************************************************************/

#pragma mark ns_window_widget_rep


@interface TMWindowController : NSWindowController
{
  ns_window_widget_rep *wid;
}
- (void) setWidget:(widget_rep*) w;
- (widget_rep*) widget;
@end


@implementation TMWindowController
- (void) setWidget: (widget_rep*) w
{
  wid = (ns_window_widget_rep*)w;
}

- (widget_rep*)widget
{
  return (ns_widget_rep*)wid;
}
@end

extern int nr_windows;

ns_window_widget_rep::ns_window_widget_rep (ns_widget wid, string _name,
                                            command _quit, bool _fake)
: ns_widget_rep (window_widget), orig_name (_name), quit (_quit), fake (_fake)
{
  add_child (abstract(wid));
  // HACK: don't increment window count for side tools or any other fake windows
  if (!fake) ++nr_windows;
  
  if (DEBUG_QT)
    debug_qt << "Creating ns_window_widget " << id << "\n";

  NSView* v = wid->as_nsview ();
  NSRect screen_frame = [[NSScreen mainScreen] visibleFrame];
  NSWindow *win = [[[NSWindow alloc] initWithContentRect: NSMakeRect(0,0,100,100)
                                               styleMask: NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask | NSResizableWindowMask
                                                 backing: NSBackingStoreBuffered
                                                   defer: NO] autorelease];
  
  [win setContentView: v];
  [win setTitle: to_nsstring (orig_name)];
  [win setAcceptsMouseMovedEvents: YES];
  
  wc = [[TMWindowController alloc] initWithWindow: win];
  [wc setWidget: this];
}

ns_window_widget_rep::~ns_window_widget_rep()
{
  if (DEBUG_QT)
    debug_qt << "Deleting qt_window_widget " << id << "\n";
  if (!fake) nr_windows--;
  [wc setWidget: nil];
  [wc autorelease];
}

void
ns_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_window_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
      NSWindow *win = [wc window];
      if (win) {
        NSSize size = to_nssize (p);
        [win setContentSize: size];
      }
    }
      break;
    case SLOT_POSITION:
    {
      check_type<coord2>(val, s);
      coord2 p = open_box<coord2> (val);
      NSWindow *win = [wc window];
      if (win) {
        [win setFrameOrigin: to_nspoint (p)];
      }
    }
      break;
    case SLOT_VISIBILITY:
    {
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);
      NSWindow *win = [wc window];
      if (win) {
        if (flag)
          [win makeKeyAndOrderFront: nil];
        else
          [win orderOut:nil];
      }
    }
      break;
    case SLOT_MOUSE_GRAB:
    {
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);  // true= get grab, false= release grab
      NSWindow *win = [wc window];
      if (win) {
        // FIXME: not sure how to handle this
        cout << "TeXmacs] Warning: mouse grabbing is currenlty ignored in the NS backend\n";
      }
    }
      break;
    case SLOT_NAME: // sets window *title* not the name
    {
      check_type<string> (val, s);
      string name = open_box<string> (val);
      NSWindow *win = [wc window];
      if (win) {
        NSString *title = to_nsstring (name);
        [win setTitle: title];
        // FIXME: what about the modified state? (see the Qt code)
      }
    }
      break;
    case SLOT_MODIFIED:
    {
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);
      [wc setDocumentEdited: flag];
    }
      break;
    case SLOT_REFRESH:
    {
      check_type<string> (val, s);
      string kind = open_box<string> (val);
      // FIXME: implement
//      the_gui->gui_helper->emitTmSlotRefresh (kind);
    }
      break;
    default:
      ns_widget_rep::send(s, val);
  }
}


blackbox
ns_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      check_type_id<int> (type_id, s);
      return close_box<int> (id);
    }
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      NSRect frame = [[wc window] frame];
      return close_box<coord2> (from_nspoint (frame.origin));
    }
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      NSRect frame = [[wc window] frame];
      return close_box<coord2> (from_nssize (frame.size));
    }
    default:
      return ns_widget_rep::query (s, type_id);
  }
}

widget
ns_window_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_window_widget_rep::read " << slot_name(s)
    << "\tWidget id: " << id << LF;
  
  switch (s) {
    case SLOT_WINDOW:  // We use this in qt_gui_rep::show_help_balloon()
      check_type_void (index, s);
      return this;
    default:
      return ns_widget_rep::read (s, index);
  }
}

void
ns_window_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_window_widget_rep::notify " << slot_name(s) << LF;
  ns_widget_rep::notify (s, new_val);
}

void
ns_window_widget_rep::write (slot s, blackbox index, widget w) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_window_widget_rep::write " << slot_name(s) << LF;
  ns_widget_rep::write (s, index, w);
}




/******************************************************************************
 * popup widget
 ******************************************************************************/

#pragma mark ns_popup_widget

ns_popup_widget_rep::ns_popup_widget_rep (widget wid, command _quit)
: ns_widget_rep (ns_widget_rep::popup_widget), quit(_quit) {
  
}

ns_popup_widget_rep::~ns_popup_widget_rep () {
}

widget
ns_popup_widget_rep::popup_window_widget(string s) {
  //qwid->setWindowTitle (to_qstring (s)); // useless for Qt::Popup
  return this;
}

void
ns_popup_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_SIZE:
    {
      check_type<coord2>(val, s);
//      qwid->resize (to_qsize (open_box<coord2> (val)));
    }
      break;
    case SLOT_POSITION:
    {
      check_type<coord2>(val, s);
//      qwid->move (to_qpoint (open_box<coord2> (val)));
    }
      break;
    case SLOT_VISIBILITY:
    {
      check_type<bool> (val, s);
//      qwid->setVisible(open_box<bool> (val));
    }
      break;
      //FIXME: what's this?
    case SLOT_MOUSE_GRAB:
    {
      check_type<bool> (val, s);
      bool flag = open_box<bool> (val);  // true= get grab, false= release grab

#if 0
      qwid->hide();
      if (flag) qwid->setWindowModality(Qt::WindowModal); //ok?
      else      qwid->setWindowModality(Qt::NonModal);    //ok?
      qwid->show();
#endif
    }
      break;
    default:
      ns_widget_rep::send(s, val);
  }
  
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_popup_widget_rep: sent " << slot_name (s)
    << "\t\tto widget\t"         << type_as_string() << LF;
}

blackbox
ns_popup_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "ns_popup_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
     // return close_box<coord2> (from_qpoint (qwid->pos()));
    }
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
    //  return close_box<coord2> (from_qsize (qwid->size()));
    }
    default:
      return ns_widget_rep::query (s, type_id);
  }
}


/******************************************************************************
 * Global functions we export for the creation of windowed widgets by TeXmacs
 ******************************************************************************/

/*! Creates a decorated window using the given widget.
 
 Each widget type may choose how to present itself as a window, by
 reimplementing ns_widget_rep::plain_window_widget(), although the base class
 ns_widget_rep provides a default implementation which suffices in most cases.
 See its documentation.
 
 \param w    The contents of the window.
 \param name A unique identifier for the window. This is *not* the window title.
 \param q    A command to be executed when the window closes.
 */
widget
plain_window_widget (widget w, string name, command q) {
  widget win= concrete(w)->plain_window_widget (name, q);
  if (name != "popup") {
    SI xx, yy, ww, hh;
    xx = yy = ww = hh = -1;
    get_preferred_position (name, xx, yy);
    get_preferred_size (name, ww, hh);
    if (xx != -1)
      set_position (win, xx, yy);
    if (ww != -1)
      set_size (win, ww, hh);
  }
  return win;
}

/*! Creates an undecorated window with name s and contents w.
 */
widget
popup_window_widget (widget w, string s) {
  return concrete(w)->popup_window_widget (s);
}

/*! A factory for a popup widget container whose contents are to be unmapped as
 soon as the mouse exits the widget.
 
 There are currently two kinds of popup widgets: those whose underlying QWidget
 is a QMenu, and those that hold any sort of QWidget. The former are used in
 edit_mouse.cpp to implement a contextual menu in the canvas and are implemented
 using ns_menu_rep; the latter are used for help-balloons and are implemented
 using ns_popup_widget_rep.
 
 \param w The widget to be placed in the popup. It will be deleted after the
 mouse leaves the popup.
 \return The popup widget.
 */
widget
popup_widget (widget w) {
  return concrete(w)->make_popup_widget();
}

/*! Destroys a window as created via ns_window_widget.
 
 In the QT implementation explicitly destroying window widgets should
 not be necessary since the widget itself destroys the QWidget as soon as
 its destructor is called. No memory leak should be caused by this trivial
 implementation.
 */
void
destroy_window_widget (widget w) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "destroy_window_widget() on "
    << concrete(w)->type_as_string() << LF;
}


/******************************************************************************
 * TeXmacs interface for the creation of widgets.
 * See Graphics/Gui/widget.hpp for comments.
 ******************************************************************************/

widget horizontal_menu (array<widget> a) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::horizontal_menu, a);
  wid->add_children (a);
  return abstract (wid);
}

widget vertical_menu (array<widget> a)  {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::vertical_menu, a);
  wid->add_children (a);
  return abstract (wid);
}

widget horizontal_list (array<widget> a) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::horizontal_list, a);
  wid->add_children (a);
  return abstract (wid);
}

widget vertical_list (array<widget> a) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::vertical_list, a);
  wid->add_children (a);
  return abstract (wid);
}

widget aligned_widget (array<widget> lhs, array<widget> rhs, SI hsep, SI vsep,
                       SI lpad, SI rpad) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::aligned_widget,
                                             lhs, rhs, coord4 (hsep, vsep, lpad, rpad));
  wid->add_children (lhs);
  wid->add_children (rhs);
  return abstract (wid);
}

widget tabs_widget (array<widget> tabs, array<widget> bodies) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::tabs_widget,
                                             tabs, bodies);
  wid->add_children (tabs);
  wid->add_children (bodies);
  return abstract (wid);
}

widget icon_tabs_widget (array<url> us, array<widget> ts, array<widget> bs) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::icon_tabs_widget,
                                             us, ts, bs);
  wid->add_children (ts);
  wid->add_children (bs);
  return abstract (wid);
}

widget wrapped_widget (widget w, command cmd) {
  return tm_new<ns_wrapped_widget_rep> (w, cmd);
}

widget tile_menu (array<widget> a, int cols) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::tile_menu, a, cols);
  wid->add_children (a);
  return abstract (wid);
}

widget minibar_menu (array<widget> a) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::minibar_menu, a);
  wid->add_children (a);
  return abstract (wid);
}

widget menu_separator (bool vertical) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::menu_separator,
                                             vertical);
  return abstract (wid);
}

widget menu_group (string name, int style) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::menu_group,
                                             name, style);
  return abstract (wid);
}

widget pulldown_button (widget w, promise<widget> pw) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::pulldown_button,
                                             w, pw);
  // FIXME: the promise widget isn't added to the children when it's evaluated
  //  wid->add_child (??);
  return abstract(wid);
}

widget pullright_button (widget w, promise<widget> pw) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::pullright_button,
                                             w, pw);
  // FIXME: the promise widget isn't added to the children when it's evaluated
  //  wid->add_child (??);
  return abstract(wid);
}

widget menu_button (widget w, command cmd, string pre, string ks, int style) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::menu_button,
                                             w, cmd, pre, ks, style);
  wid->add_child (w);
  return abstract (wid);
}

widget balloon_widget (widget w, widget help) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::balloon_widget,
                                             w, help);
  wid->add_child (w);
  return abstract (wid);
}

widget text_widget (string s, int style, color col, bool tsp) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::text_widget,
                                             s, style, col, tsp);
  return abstract (wid);
}

widget xpm_widget (url file_name) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::xpm_widget,
                                             file_name);
  return abstract (wid);
}

widget toggle_widget (command cmd, bool on, int style) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::toggle_widget,
                                             cmd, on, style);
  return abstract (wid);
}

widget enum_widget (command cmd, array<string> vals, string val, int style,
                    string width) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::enum_widget,
                                             cmd, vals, val, style, width);
  return abstract (wid);
}

widget choice_widget (command cmd, array<string> vals, array<string> chosen,
                      int style) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::choice_widget,
                                             cmd, vals, chosen, true, style);
  return abstract (wid);
}

widget choice_widget (command cmd, array<string> vals, string cur, int style) {
  array<string> chosen (1);
  chosen[0]= cur;
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::choice_widget,
                                             cmd, vals, chosen, false, style);
  return abstract (wid);
}

widget choice_widget (command cmd, array<string> vals, string cur, string filter) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::filtered_choice_widget,
                                             cmd, vals, cur, filter);
  return abstract (wid);
}

widget user_canvas_widget (widget w, int style) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::scrollable_widget,
                                             w, style);
  wid->add_child (w);
  return abstract (wid);
}

widget resize_widget (widget w, int style, string w1, string h1,
                      string w2, string h2, string w3, string h3,
                      string hpos, string vpos) {
  typedef triple<string, string, string> T1;
  (void) hpos; (void) vpos;
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::resize_widget,
                                             w, style, T1(w1, w2, w3),
                                             T1(h1, h2, h3));
  wid->add_child (w);
  return abstract (wid);
}

widget hsplit_widget (widget l, widget r) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::hsplit_widget, l, r);
  wid->add_children (array<widget> (l, r));
  return abstract (wid);
}

widget vsplit_widget (widget t, widget b) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::vsplit_widget, t, b);
  wid->add_children (array<widget> (t, b));
  return abstract (wid);
}

widget refresh_widget (string tmwid, string kind) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::refresh_widget,
                                             tmwid, kind);
  // FIXME: decide what to do with children in QTMRefresh::recompute()
  return abstract (wid);
}

widget refreshable_widget (object promise, string kind) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::refreshable_widget,
                                             promise, kind);
  // FIXME: decide what to do with children in QTMRefreshable::recompute()
  return abstract (wid);
}

widget glue_widget (bool hx, bool vx, SI w, SI h) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::glue_widget,
                                             hx, vx, w/PIXEL, h/PIXEL);
  return abstract (wid);
}

widget glue_widget (tree col, bool hx, bool vx, SI w, SI h) {
  return tm_new<ns_glue_widget_rep> (col, hx, vx, w, h);
}

widget inputs_list_widget (command call_back, array<string> prompts) {
  return tm_new<ns_inputs_list_widget_rep> (call_back, prompts);
}

widget input_text_widget (command call_back, string type, array<string> def,
                          int style, string width) {
  return tm_new<ns_input_text_widget_rep> (call_back, type, def, style, width);
}

widget color_picker_widget (command call_back, bool bg, array<tree> proposals) {
  return tm_new<ns_color_picker_widget_rep> (call_back, bg, proposals);
}

widget file_chooser_widget (command cmd, string type, string prompt) {
  return tm_new<ns_chooser_widget_rep> (cmd, type, prompt);
}

widget printer_widget (command cmd, url ps_pdf_file) {
  return tm_new<ns_printer_widget_rep> (cmd, ps_pdf_file);
}

widget texmacs_widget (int mask, command quit) {
  if (mask) return tm_new<ns_tm_widget_rep> (mask, quit);
  else      return tm_new<ns_tm_embedded_widget_rep> (quit);
}

widget ink_widget (command cb) {
  NOT_IMPLEMENTED ("Ink widget");
  (void) cb; return widget();
}

widget tree_view_widget (command cmd, tree data, tree actions) {
  ns_widget wid = ns_ui_element_rep::create (ns_widget_rep::tree_view_widget,
                                             cmd, data, actions);
  return abstract (wid);
  
}
//// Widgets which are not strictly required by TeXmacs have void implementations

widget tooltip_window_widget (widget w, string s) {
  // FIXME: a proper tooltip window
  return popup_window_widget (w, s);
}

widget responsive_tabs_widget (array<widget> tabs, array<widget> bodies) {
  return tabs_widget (tabs, bodies);
}

widget responsive_icon_tabs_widget (array<url> us, array<widget> ts,
                                    array<widget> bs) {
  return icon_tabs_widget (us, ts, bs);
}

widget division_widget (string name, widget w) {
  // NOTE: divisions only matter for the style of the Qt widgets
  (void) name; return w;
}

widget setting_toggle_widget (command cmd, string text, bool on, int style) {
  return horizontal_list (array<widget> (toggle_widget (cmd, on, style),
                                         text_widget (text, style, black, true)));
}

widget setting_enum_widget (command cb, string text, array<string> vals,
                            string val, int st, string w) {
  return horizontal_list (array<widget> (text_widget (text, st, black, true),
                                         enum_widget (cb, vals, val, st, w)));
}

widget setting_group_widget (string text, array<widget> vals, int style) {
  array<widget> a;
  a << text_widget (text, style, black, true);
  a << vals;
  return vertical_list (a);
}

widget empty_widget () { NOT_IMPLEMENTED("empty_widget"); return widget(); }
widget extend_widget (widget w, array<widget> a) { (void) a; return w; }
widget wait_widget (SI width, SI height, string message) {
  (void) width; (void) height; (void) message; return widget();
}

/******************************************************************************
 * Keyboard focus and menu items
 ******************************************************************************/

widget the_keyboard_focus (NULL);

NSView*
ns_widget_rep::as_nsview () {
  // NOTE: the widgets which can be shown in windows redefine this method
  return nil;
}

TMMenuItem*
ns_widget_rep::as_menuitem () {
  // NOTE: the widgets which can be shown in menus redefine this method
  return nil;
}

/******************************************************************************
* ns_view_widget_rep
******************************************************************************/

#pragma mark ns_view_widget_rep

ns_view_widget_rep::ns_view_widget_rep (NSView *v, types _type):
  ns_widget_rep(_type), view(v) { 
  [v retain]; 
}

ns_view_widget_rep::~ns_view_widget_rep()  { 
  [view release]; 
}



void
ns_view_widget_rep::send (slot s, blackbox val) {
  switch (s) {
  case SLOT_NAME:
    {	
      check_type<string> (val, s);
      string name = open_box<string> (val);
      NSWindow *win = [view window];
      if (win) {
	[win setTitle:to_nsstring(name)];
      }
    }
    break;
  case SLOT_INVALIDATE:
    {
      check_type<coord4> (val, s);
      coord4 p= open_box<coord4> (val);
      NSRect rect = to_nsrect(p);
      if (DEBUG_EVENTS) NSLog(@"invalidating %@",NSStringFromRect(rect));
      [view setNeedsDisplayInRect:rect];
    }
    break;
  case SLOT_INVALIDATE_ALL:
    {
      ASSERT (is_nil (val), "type mismatch");
      [view setNeedsDisplay:YES];
    }
    break;
  case SLOT_MOUSE_GRAB:
    NOT_IMPLEMENTED ("ns_view_widget_rep");
    //			send_mouse_grab (THIS, val);
    break;
  case SLOT_MOUSE_POINTER:
    NOT_IMPLEMENTED ("ns_view_widget_rep");
    //			send_mouse_pointer (THIS, val);
    break;
    
  case SLOT_KEYBOARD_FOCUS:
    //			send_keyboard_focus (THIS, val);
    {
      check_type<bool> (val, s);
      if (open_box<bool>(val)) the_keyboard_focus = this;
    }
    break;
	
  default:
    ns_widget_rep::send (s, val);
  }
}

/******************************************************************************
* Querying
******************************************************************************/

blackbox
ns_view_widget_rep::query (slot s, int type_id) {
  switch (s) {
  case SLOT_IDENTIFIER:
    check_type_id<int> (type_id, s);
    return close_box<int> ([view window] ? 1 : 0);
#if 0
  case SLOT_RENDERER:
    check_type_id<renderer> (type_id, s);
    return close_box<renderer> ((renderer) the_ns_renderer());
#endif
  case SLOT_POSITION:  
    {
      typedef pair<SI,SI> coord2;
      check_type_id<coord2> (type_id, s);
      NSPoint pos = [view frame].origin;
      return close_box<coord2> (from_nspoint(pos));
    }
    
  default:
    return ns_widget_rep::query (s, type_id);
    return blackbox ();
  }
}

/******************************************************************************
 * Notification of state changes
 ******************************************************************************/

void
ns_view_widget_rep::notify (slot s, blackbox new_val) {
  ns_widget_rep::notify (s, new_val);
}

/******************************************************************************
 * Read and write access of subwidgets
 ******************************************************************************/

widget
ns_view_widget_rep::read (slot s, blackbox index) {
  switch (s) {
  case SLOT_WINDOW:
    check_type_void (index, s);
    return [(TMWindowController*)[[view window] windowController] widget];
  default:
    return ns_widget_rep::read (s, index);
    return widget();
  }
}

void
ns_view_widget_rep::write (slot s, blackbox index, widget w) {
  switch (s) {
  default:
    ns_widget_rep::write (s, index, w);
  }
}




