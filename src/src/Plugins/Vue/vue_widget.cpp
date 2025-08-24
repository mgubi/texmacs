/******************************************************************************
* MODULE     : vue_widget.cpp
* DESCRIPTION: Definition of Vue widgets
* COPYRIGHT  : (C) 2025  Masssimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "vue_gui.hpp"

#include "config.h"
#include "vue_widget.hpp"
#include "blackbox.hpp"
#include "array.hpp"
#include "message.hpp"
#include "promise.hpp"
#include "iterator.hpp"
#include "object.hpp"
#include "window.hpp"
#include "message.hpp"
#include "font.hpp"

#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include "../MuPDF/mupdf_picture.hpp"

#include "clay.h"

#define DEBUG_VUE (debug (DEBUG_FLAG_QT))
#define DEBUG_VUE_WIDGETS (debug (DEBUG_FLAG_QT_WIDGETS))


Clay_Sizing layoutExpand = {
    .width = CLAY_SIZING_GROW(0),
    .height = CLAY_SIZING_GROW(0)
};



/******************************************************************************
 * Type checking
 ******************************************************************************/

inline void
check_type_void (blackbox bb, slot s) {
  if (!is_nil (bb)) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T> inline void
check_type_id (int type_id, slot s) {
  if (type_id != type_helper<T>::id) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T> void
check_type (blackbox bb, slot s) {
  if (type_box (bb) != type_helper<T>::id) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T> T
check_open (blackbox bb, slot s) {
  if (type_box (bb) != type_helper<T>::id) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
  return open_box<T> (bb);
}

template<class T1, class T2> inline void
check_type (blackbox bb, string s) {
  check_type<pair<T1,T2> > (bb, s);
}

/******************************************************************************/

#ifdef NO_FAST_ALLOC
template<> void
tm_delete<vue_widget_rep> (vue_widget_rep* ptr) {
  if (ptr == NULL) return;
  delete ptr;
}
#else
template<> void
tm_delete<vue_widget_rep> (vue_widget_rep* ptr) {
  if (ptr == NULL) return;
  void *mem= ptr->derived_this ();
  ptr -> ~vue_widget_rep ();
  fast_delete (mem);
}
#endif

/******************************************************************************
* umbrella widget class
******************************************************************************/

class vue_ui_rep : public vue_widget_rep {
protected:
  blackbox data;
  
public:
  vue_ui_rep (string _type, blackbox _data= NULL)
    : vue_widget_rep (_type), data (_data) {};
  virtual ~vue_ui_rep () {};
  
  void do_layout ();
};

template<typename T> widget vue_create (string type, T args) {
  return abstract (tm_new<vue_ui_rep> (type, close_box (args)));
}

//******************************************************************************
// FOR_EACH-ish macro
// adapted from https://www.scs.stanford.edu/~dm/blog/va-opt.html

#define PARENS ()
#define EXPAND(...) EXPAND4(EXPAND4(EXPAND4(EXPAND4(__VA_ARGS__))))
#define EXPAND4(...) EXPAND3(EXPAND3(EXPAND3(EXPAND3(__VA_ARGS__))))
#define EXPAND3(...) EXPAND2(EXPAND2(EXPAND2(EXPAND2(__VA_ARGS__))))
#define EXPAND2(...) EXPAND1(EXPAND1(EXPAND1(EXPAND1(__VA_ARGS__))))
#define EXPAND1(...) __VA_ARGS__

#define FOR_EACH_PAIR(macro, sep, ...)                                   \
  __VA_OPT__(EXPAND(FOR_EACH_PAIR_HELPER(macro, sep, __VA_ARGS__)))
#define FOR_EACH_PAIR_HELPER(macro, sep, a1, a2, ...)                    \
  macro(a1, a2)                                                          \
  __VA_OPT__(sep() FOR_EACH_PAIR_AGAIN PARENS (macro, sep, __VA_ARGS__))
#define FOR_EACH_PAIR_AGAIN() FOR_EACH_PAIR_HELPER

//******************************************************************************
// Description of widgets
//
// this set of macros uses FOR_EACH_PAIR to automatically generate code for each
// of the widget creation functions of TeXmacs, effectively implementing some
// kind of algebraic datatype on which we will do some pattern matching later
//
// the entrypoint is VUE_WIDGET

#define MAKE_PARAM(a,b) a b
#define MAKE_INIT(a,b)  .b= b
#define MAKE_MEMBER(a,b) a b;
#define MAKE_EQ(a,b) (lhs.b == rhs.b)
#define MAKE_OUT(a,b) bb.b
#define COMMA() ,
#define SEMICOLON() ;
#define NONE()
#define BOOLAND() &&
#define LESSLESS() <<

#define VUE_WIDGET_HELPER_PARAMS(...) FOR_EACH_PAIR(MAKE_PARAM, COMMA, __VA_ARGS__)
#define VUE_WIDGET_HELPER_INIT(...) FOR_EACH_PAIR(MAKE_INIT, COMMA, __VA_ARGS__)
#define VUE_WIDGET_HELPER_MEMBER(...) FOR_EACH_PAIR(MAKE_MEMBER, NONE, __VA_ARGS__)

#define VUE_WIDGET_DATA(NAME, ...)\
  struct vue_##NAME { VUE_WIDGET_HELPER_MEMBER(__VA_ARGS__) };\
  inline bool operator==(const vue_##NAME &lhs, const vue_##NAME &rhs)\
  { return true __VA_OPT__(&& FOR_EACH_PAIR(MAKE_EQ, BOOLAND, __VA_ARGS__)); }\
  inline tm_ostream& operator << (tm_ostream& out, vue_##NAME &bb)\
  { return out __VA_OPT__(<< FOR_EACH_PAIR(MAKE_OUT, LESSLESS, __VA_ARGS__)); }\
  string type_vue_##NAME(#NAME);

#define VUE_WIDGET_HEADER(NAME, ...) \
  widget NAME (VUE_WIDGET_HELPER_PARAMS(__VA_ARGS__))

#define VUE_WIDGET(NAME, ...)\
  VUE_WIDGET_DATA(NAME __VA_OPT__(, __VA_ARGS__))\
  widget NAME (VUE_WIDGET_HELPER_PARAMS(__VA_ARGS__))\
  { return vue_create (type_vue_##NAME,\
           vue_##NAME { VUE_WIDGET_HELPER_INIT(__VA_ARGS__) }); }

//******************************************************************************
// TeXmacs widgets

/******************************************************************************
* Window widgets
******************************************************************************/


void destroy_window_widget (widget w) {
}
// destroys a window as created by the above routines

// VUE_WIDGET(plain_window_widget, widget, w, string, s, command, quit);
// creates a decorated window with name s and contents w
VUE_WIDGET(popup_window_widget, widget, w, string, s);
// creates an undecorated popup window with name s and contents w
VUE_WIDGET(tooltip_window_widget, widget, w, string, s);
// creates an undecorated tooltip window with name s and contents w

/******************************************************************************
* Top-level widgets, typically given as an argument to plain_window_widget
* See also message.hpp for specific messages for these widgets
******************************************************************************/

//VUE_WIDGET(texmacs_widget, int, mask, command, quit);
// the main TeXmacs widget and a command which is called on exit
// the mask variable indicates whether the menu, icon bars, status bar, etc.
// are visible or not
VUE_WIDGET(file_chooser_widget, command, cmd, string, type, string, prompt);
// file chooser widget for files of a given 'type';
// for files of type "image", the widget includes a previsualizer for images
// 'prompt' contains a prompt if we intend to save the file
// and the empty string otherwise
VUE_WIDGET(printer_widget, command, cmd, url, ps_pdf_file);
// widget for printing a file, offering a way for selecting a page range,
// changing the paper type and orientation, previewing, etc.;
// the command cmd is called on exit
VUE_WIDGET(color_picker_widget, command, cmd, bool, bg, array<tree>, proposals);
// widgets for selecting a color, a pattern or a background image,
// encoded by a tree. On input, we give a list of recently used proposals
// on termination the command is called with the selected color as argument
// the bg flag specifies whether we are picking a background color or fill
VUE_WIDGET(inputs_list_widget, command, call_back, array<string>, prompts);
// a dialogue widget with Ok and Cancel buttons and a series of textual
// input widgets with specified prompts
VUE_WIDGET(popup_widget, widget, w);
// a widget container which results w to be unmapped as soon as
// the pointer quits the widget

/******************************************************************************
* Widgets for the construction of menus
******************************************************************************/

VUE_WIDGET(horizontal_menu, array<widget>, a);
// a horizontal menu made up of the widgets in a
VUE_WIDGET(vertical_menu, array<widget>, a);
  // a vertical menu made up of the widgets in a
VUE_WIDGET(tile_menu, array<widget>, a, int, cols);
  // a menu rendered as a table of cols columns wide & made up of widgets in a
VUE_WIDGET(minibar_menu, array<widget>, a);
  // a small minibar, which can for instance occur inside another iconbar
VUE_WIDGET(menu_separator, bool, vertical);
  // a horizontal or vertical menu separator
VUE_WIDGET(menu_group, string, name, int, style);
  // a menu group of a given style; the name should be greyed and centered

VUE_WIDGET(pulldown_button, widget, w, promise<widget>, pw);
  // a button w with a lazy pulldown menu pw
VUE_WIDGET(pullright_button, widget, w, promise<widget>, pw);
  // a button w with a lazy pullright menu pw
VUE_WIDGET(menu_button, widget, w, command, cmd,
        string, pre, string, ks, int, style);
  // a command button with an optional prefix (o, * or v) and
  // keyboard shortcut; if ok does not hold, then the button is greyed
  // for pressed styles, the button is displayed as a pressed button
VUE_WIDGET(balloon_widget, widget, w, widget, help);
  // given a button widget w, specify a help balloon which should be displayed
  // when the user leaves the mouse pointer on the button for a small while

VUE_WIDGET(text_widget, string, s, int, style, color, col, bool, tsp);
  // a text widget with a given style, color and transparency
VUE_WIDGET(xpm_widget, url, file_name);
  // a widget with an X pixmap icon
VUE_WIDGET(input_text_widget, command, call_back, string, type, array<string>, def,
        int, style, string, width);
  // a textual input widget for input of a given type and a list of suggested
  // default inputs (the first one should be displayed, if there is one)
  // an optional width may be specified for the input field
  // the width is specified in TeXmacs length format with units em, px or w
VUE_WIDGET(enum_widget, command, cb, array<string>, vals, string, val,
                    int, st, string, w);
  // select a value from a list of possible values
VUE_WIDGET(choice_widget, command, cb, array<string>, vals, array<string>, chosen, bool, flag);
widget choice_widget (command cmd, array<string> vals, array<string> chosen) {
  return choice_widget(cmd, vals, chosen, true);
}
  // select a value from a long list of possible values
widget choice_widget (command cmd, array<string> vals, string cur) {
  array<string> chosen (1);
  chosen[0]= cur;
  return choice_widget(cmd, vals, chosen, false);
}
  // select multiple values from a long list
VUE_WIDGET(filtered_choice_widget, command, cb, array<string>, vals, string, val, string, filter);
widget choice_widget (command cmd, array<string> vals, string cur, string filter) {
  return filtered_choice_widget(cmd, vals, cur, filter);
}
  // select a value from a long list with scrollbars and an input to filter
VUE_WIDGET(tree_view_widget, command, cmd, tree, data, tree, data_roles);
  // A widget with a tree view which observes the data and updates automatically

/******************************************************************************
* Other widgets
******************************************************************************/

VUE_WIDGET(empty_widget);
  // an empty widget of size zero
VUE_WIDGET(glue_widget, bool, hx, bool, vx, SI, w, SI, h);
  // an empty widget of minimal width w and height h and which is horizontally
  // resp. vertically extensible if hx resp. vx is true
VUE_WIDGET(colored_glue_widget, tree, col, bool, hx, bool, vx, SI, w, SI, h);
widget glue_widget(tree col, bool hx, bool vx, SI w, SI h) {
  return colored_glue_widget (col, hx, vx, w, h);
}
  // a colored variant of the above widget, with colors as in the color picker
VUE_WIDGET(horizontal_list, array<widget>, a);
  // a horizontal list made up of the widgets in a
VUE_WIDGET(vertical_list, array<widget>, a);
  // a vertical list made up of the widgets in a
VUE_WIDGET(division_widget, string, name, widget, w);
  // a widget with a CSS style name
VUE_WIDGET(aligned_widget, array<widget>, lhs, array<widget>, rhs,
                       SI, hsep, SI, vsep,
                       SI, lpad, SI, rpad);
  // a table with two columns, the first one being right aligned and
  // the second one being left aligned
VUE_WIDGET(tabs_widget, array<widget>, tabs, array<widget>, bodies);
  // a tab bar where one and only of the bodies can be selected
VUE_WIDGET(icon_tabs_widget, array<url>, us, array<widget>, ss, array<widget>, bs);
  // a variant of tabs_widget with named icon tabs
VUE_WIDGET(wrapped_widget, widget, w, command, quit);
  // copy of w, but with a separate reference counter,
  // and with a command to be called upon destruction
VUE_WIDGET(user_canvas_widget, widget, wid, int, style);
  // a widget whose contents can be scrolled
  // if the size of the inner contents exceed the specified size
VUE_WIDGET(resize_widget, widget, w, int, style, string, w1, string, h1,
                      string, w2, string, h2, string, w3, string, h3,
                      string, hpos, string, vpos);
  // resize the widget w to be of minimal size (w1, h1),
  // of default size (w2, h2), of maximal size (w3, h3),
  // and initial scrolling position (hpos, vpos)
VUE_WIDGET(hsplit_widget, widget, l, widget, r);
  // two horizontally juxtaposed widgets l and r with an ajustable border
VUE_WIDGET(vsplit_widget, widget, t, widget, b);
  // two vertically juxtaposed widgets t and b with an ajustable border
VUE_WIDGET(extend_widget, widget, w, array<widget>, a);
  // extend the size of w to the maximum of the sizes of
  // the widgets in the list a
VUE_WIDGET(toggle_widget, command, cmd, bool, on, int, style);
  // an input toggle
VUE_WIDGET(wait_widget, SI, width, SI, height, string, message);
  // a widget of a specified width and height, displaying a wait message
  // this widget is only needed when using the X11 plugin
VUE_WIDGET(ink_widget, command, cb);
  // widget for inking a sketch. The input may later be passed to
  // an external program for handwriting recognition,
  // using the callback routine
VUE_WIDGET(refresh_widget, string, tmwid, string, kind);
  // a widget which is automatically constructed from the a dynamic
  // scheme widget tmwid. When receiving the send_refresh event,
  // the contents should also be updated dynamically by reevaluating
  // the scheme widget (in case of matching kind)
VUE_WIDGET(refreshable_widget, object, prom, string, kind);
  // a widget which is automatically constructed from the a dynamic
  // scheme widget promise. When receiving the send_refresh event,
  // the contents should also be updated dynamically by reevaluating
  // the scheme widget promise (in case of matching kind)

//******************************************************************************


void
vue_ui_rep::do_layout () {
}

vue_widget current_window_widget; // used during layout to propagate information

/******************************************************************************
* Besides the widget constructors, any GUI implementation should also provide
* a simple_widget_rep class with the following virtual methods:
******************************************************************************/
// bool simple_widget_rep::is_editor_widget ();
//   should return true for editor widgets only
// bool simple_widget_rep::is_embedded_widget ();
//   should return true for embedded editor widgets only
// void simple_widget_rep::handle_get_size_hint (SI& w, SI& h);
//   propose a size for the widget
// void simple_widget_rep::handle_notify_resize (SI w, SI h);
//   issued when the size of the widget has changed
// void simple_widget_rep::handle_keypress (string key, time_t t);
//   issed when a key is pressed
// void simple_widget_rep::handle_keyboard_focus (bool new_focus, time_t t);
//   issued when the keyboard focus of the widget has changed
// void simple_widget_rep::handle_mouse
//        (string kind, SI x, SI y, int mods, time_t t, array<double> data);
//   a mouse event of a given kind at position (x, y) and time t
//   mods contains the active keyboard modifiers at time t
//   data contains extra information about pen or gesture events
// void simple_widget_rep::handle_set_zoom_factor (double zoom);
//   set the zoom factor for painting
// void simple_widget_rep::handle_clear
//        (renderer ren, SI x1, SI y1, SI x2, SI y2);
//   clear the widget to the background color
//   this event may for instance occur when scrolling
// void simple_widget_rep::handle_repaint
//        (renderer ren, SI x1, SI y1, SI x2, SI y2);
//   repaint the region (x1, y1, x2, y2)

// Here it is:

string vue_type_simple_widget("simple_widget");

vue_simple_widget_rep::vue_simple_widget_rep ()
: vue_widget_rep (vue_type_simple_widget),
  size (coord2 (10, 10)), extents (coord4 (0,0,0,0)),
  scroll_pos (coord2 (0, 0)), mouse_cursor (coord2 (0, 0)),
  backing_pos (pair<SI,SI>(0, 0)) {
  // note that size is set to an arbitrary value to init the backing_store
  // create a backing store and the renderer
  backing_store= native_picture (size.x1, size.x2, 0, 0);
  ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
};

void
vue_simple_widget_rep::send (slot s, blackbox val) {
  //save_send_slot (s, val);
  switch (s) {
    case SLOT_INVALIDATE:
      {
        coord4 p= check_open<coord4> (val, s);
        SI ox = backing_pos.x1;
        SI oy = backing_pos.x2;
        ren->set_origin (ox,oy);
        SI x1 = p.x1, y1 = p.x2, x2 = p.x3, y2 = p.x4;
        ren->outer_round (x1, y1, x2, y2);
        ren->decode (x1, y1);
        ren->decode (x2, y2);
        invalidate_rect (x1, y2, x2, y1);
      }
      break;
    case SLOT_INVALIDATE_ALL:
      {
        check_type_void (val, s);
        invalidate_all ();
      }
      break;
    case SLOT_EXTENTS:
      {
        extents= check_open<coord4> (val, s);
      }
      break;
      
    case SLOT_SIZE:
      {
        size= check_open<coord2> (val, s);
      }
      break;
      
    case SLOT_SCROLL_POSITION:
    {
      scroll_pos= check_open<coord2> (val, s);
    }
      break;
      
    case SLOT_ZOOM_FACTOR:
      {
        new_zoom= check_open<double> (val, s);
      }
      break;
    case SLOT_MOUSE_GRAB:
      {
        mouse_grab= check_open<bool> (val, s);
      }
      break;
    case SLOT_MOUSE_POINTER:
      {
        typedef pair<string, string> T;
        T contents = check_open<T> (val, s); // x1 = name, x2 = mask.
        //NOT_IMPLEMENTED("qt_simple_widget::SLOT_MOUSE_POINTER");
      }
      break;
    case SLOT_CURSOR:
      {
        mouse_cursor= check_open <coord2> (val, s);
      }
      break;
    default:
      vue_widget_rep::send(s, val);
      return;
  }
  if (DEBUG_VUE_WIDGETS && s != SLOT_INVALIDATE)
    debug_widgets << "vue_simple_widget_rep: sent " << slot_name (s)
    << "\t\tto widget\t" << type << LF;
}

blackbox
vue_simple_widget_rep::query (slot s, int type_id) {
    // Some slots are too noisy
  if (DEBUG_VUE_WIDGETS && (s != SLOT_IDENTIFIER))
    debug_widgets << "vue_simple_widget_rep: queried " << slot_name(s)
                  << "\t\tto widget\t" << type << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      if (is_nil(win))
        return close_box<int>(0);
      else
        return win->query(s, type_id);
    }
    case SLOT_INVALID:
    {
      return close_box<bool> (is_invalid());
    }
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      //FIXME: implement
      return close_box<coord2> (coord2 (0, 0));
    }
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (size);
    }
    case SLOT_SCROLL_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (scroll_pos);
    }
    case SLOT_EXTENTS:
    {
      check_type_id<coord4> (type_id, s);
      return close_box<coord4> (extents);
    }
    case SLOT_VISIBLE_PART:
    {
      check_type_id<coord4> (type_id, s);
      return close_box<coord4> (
        coord4 (scroll_pos.x1, scroll_pos.x2,
                scroll_pos.x1 + size.x1,
                scroll_pos.x2 - size.x2));
    }
    default:
      return vue_widget_rep::query(s, type_id);
  }
}

widget
vue_simple_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_simple_widget_rep::read " << slot_name(s)
    << "\tWidget type: " << type << LF;
  
  switch (s) {
    case SLOT_WINDOW:
      check_type_void (index, s);
      return abstract (win);
    default:
      return vue_widget_rep::read (s, index);
  }
}

/******************************************************************************
* Empty handlers for redefinition by our subclasses editor_rep,
* box_widget_rep...
******************************************************************************/

bool
vue_simple_widget_rep::is_editor_widget () {
  return false;
}

bool
vue_simple_widget_rep::is_embedded_widget () {
  return false;
}

void
vue_simple_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  gui_root_extents (w, h);
}

void
vue_simple_widget_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
}

void
vue_simple_widget_rep::handle_keypress (string key, time_t t) {
  (void) key; (void) t;
}

void
vue_simple_widget_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  (void) has_focus; (void) t;
}

void
vue_simple_widget_rep::handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                                    array<double> data) {
  (void) kind; (void) x; (void) y; (void) mods; (void) t; (void) data;
}

void
vue_simple_widget_rep::handle_set_zoom_factor (double zoom) {
  (void) zoom;
}

void
vue_simple_widget_rep::handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2) {
  (void) win; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
vue_simple_widget_rep::handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2) {
  (void) win; (void) x1; (void) y1; (void) x2; (void) y2;
}

void vue_simple_widget_rep::do_layout () {
  win= current_window_widget; // save the info
  CLAY({
    .layout = { .sizing= layoutExpand },
    .custom = { .customData = this } }) {}
}

/******************************************************************************
 * Backing store management
 ******************************************************************************/

void
vue_simple_widget_rep::invalidate_rect (int x1, int y1, int x2, int y2) {
  int padding = 16;
  rectangle r = rectangle (x1-padding, y1-padding, x2+padding, y2+padding);
  invalid_regions = invalid_regions | rectangles (r);
}

void
vue_simple_widget_rep::invalidate_all () {
  //cout << "invalidate all " << LF;
  invalid_regions = rectangles();
  invalidate_rect (0, 0, retina_factor * size.x1,
                   retina_factor * size.x2);
}

bool
vue_simple_widget_rep::is_invalid () {
  return !is_nil (invalid_regions);
}

void
vue_simple_widget_rep::translate_backing_store (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) {
  ren->set_origin (0,0);
  SI X1= x1+ dx;
  SI Y2= y2+ dy;
  ren->decode (x1, y1);
  ren->decode (x2, y2);
  ren->decode (X1, Y2);
  dx= X1- x1;
  dy= Y2- y2;

  rectangles region (rectangle (x1, y2, x2, y1));
  rectangles invalid_intern= invalid_regions & region;
  rectangles invalid_extern= invalid_regions - invalid_intern;
  invalid_intern = ::translate (invalid_intern, dx, dy) & region;
  invalid_regions= invalid_extern | invalid_intern;

  rectangles extra= thicken (region - ::translate (region, dx, dy), 1, 1);
  invalid_regions= invalid_regions | extra;

  if (x1<x2 && y2<y1) {
//    cout << "translate " << x1 << ", " << y1 << ", " << x2 << ", " << y2 << ", " << X1 << ", " << Y2  << LF;
    fz_pixmap *pix= ((mupdf_picture_rep*)backing_store->get_handle())->pix;
    int w= fz_pixmap_width (mupdf_context (), pix);
    int h= fz_pixmap_height (mupdf_context (), pix);
    fz_pixmap *area= fz_new_pixmap (mupdf_context (),
                                   fz_device_rgb (mupdf_context ()),
                                   w, h, NULL, 1);
    fz_irect r= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (mupdf_context(), area, pix, r, NULL);
    area->x= dx;
    area->y= dy;
    fz_copy_pixmap_rect (mupdf_context(), pix, area, r, NULL);
    fz_drop_pixmap (mupdf_context(), area);
  }
}

void
vue_simple_widget_rep::repaint_invalid_regions () {
  int bs_w= backing_store->get_width ();
  int bs_h= backing_store->get_height ();


  // Look if the scroll position has changed. backing_pos is the old position,
  // while origin is the new one. Instead of repainting the whole backing store,
  // we move the contents of the backing store, and invalidate the regions that
  // are not covered by the moved contents.
  
  if (backing_pos != scroll_pos) {
    int dx =  retina_factor * (scroll_pos.x1 - backing_pos.x1);
    int dy =  retina_factor * (scroll_pos.x2 - backing_pos.x2);

    backing_pos = scroll_pos;
    translate_backing_store (0, 0, bs_w, bs_h, -dx, -dy);
    //cout << "SCROLL CONTENTS BY " << dx << " " << dy << LF;
        
    rectangles invalid;
    while (!is_nil (invalid_regions)) {
      rectangle r = invalid_regions->item ;
      rectangle q = rectangle (r->x1-dx,r->y1-dy,r->x2-dx,r->y2-dy);
      invalid = rectangles (q, invalid);
      //cout << r << " ---> " << q << LF;
      invalid_regions = invalid_regions->next;
    }
    invalid_regions= invalid & rectangles (rectangle (0,0, bs_w, bs_h));

    if (!backing_valid) {
      invalidate_rect (0, 0, bs_w, bs_h);
    } else {
      if (dy<0)
  invalidate_rect (0, 0, bs_w, min (bs_h,-dy));
      else if (dy>0)
  invalidate_rect (0, max (0,bs_h-dy), bs_w, bs_h);
      
      if (dx<0)
  invalidate_rect (0, 0, min (-dx, bs_w), bs_h);
      else if (dx>0)
  invalidate_rect (max (0, bs_w-dx), 0, bs_w, bs_h);
    }
  }
  
  // Check if the window has been resized. If so, we need to resize the backing
  // store as well. During the resize, the origin remain the same. So we can just
  // crop the backing store if the window is smaller, or fill the new regions with
  // the background color if the window is bigger.

  int new_bs_w, new_bs_h;
  new_bs_w= retina_factor*size.x1;
  new_bs_h= retina_factor*size.x2;
  
  if ((new_bs_w != bs_w)   || (new_bs_h != bs_h)) {
    // the viewport size changed, reset the backing store
    
    // create a new backing store with updated viewport and the renderer
    picture new_backing_store= native_picture (new_bs_w, new_bs_h, 0, 0);
    renderer ren2= picture_renderer (new_backing_store, std_shrinkf * retina_factor);
    
    // copy the old backingstore
    SI x1=0, y1=0, x2=bs_w, y2=bs_h;
    ren->encode (x1, y1);
    ren->encode (x2, y2);
    ren2->fetch (x1, y2, x2, y1, ren, x1, y2);
    
    // compute new invalid regions
    // add new exposed regions due to resize
    if (new_bs_w > bs_w) {
      rectangle r = rectangle (bs_w, 0, new_bs_w, new_bs_h);
      invalid_regions = invalid_regions | rectangles (r);
    }
    if (new_bs_h > bs_h) {
      rectangle r = rectangle (0, bs_h, new_bs_w, new_bs_h);
      invalid_regions = invalid_regions | rectangles (r);
    }
    
    // update the state
    bs_w = new_bs_w;
    bs_h = new_bs_h;
    backing_store= new_backing_store;
    delete_renderer (ren);
    ren= ren2;
  }
  
  //invalid_regions= rectangles (rectangle (0,0, bs_w, bs_h));
  
  // repaint invalid rectangles if needed
  if (!is_nil (invalid_regions)) {
    rectangles new_regions;
    
    // simplify
    rectangle lub= least_upper_bound (invalid_regions);
    if (area (lub) < 1.2 * area (invalid_regions))
      invalid_regions= rectangles (lub);
    
    while (!is_nil (invalid_regions)) {
      ren->set_origin (0, 0);
      rectangle r= copy (invalid_regions->item);
//      cout << "repaint " << r->x1 << ", " << r->y1 << ", "
//           << r->x2 << ", " << r->y2 << LF;
      r= thicken (r, 1, 1);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
      send_repaint (this, ren, r->x1, r->y2, r->x2, r->y1);
      ren->set_clipping (r->x1, r->y2, r->x2, r->y1, true);
      if (gui_interrupted ())
        new_regions= rectangles (invalid_regions->item, new_regions);
      invalid_regions= invalid_regions->next;
    }
    invalid_regions= new_regions;
  } // if (!is_nil (invalid_regions))
}

void draw_picture (SDL_Renderer *sdl_ren, picture pic, SDL_FRect *dest) {
  // propagate immediately the changes to the screen
  fz_pixmap *pix= ((mupdf_picture_rep*)pic->get_handle())->pix;
  //snapshot_pixmap (pix);
  unsigned char *samples= fz_pixmap_samples (mupdf_context (), pix);
  int w= fz_pixmap_width (mupdf_context (), pix);
  int h= fz_pixmap_height (mupdf_context (), pix);
  //  fz_keep_pixmap (mupdf_context (), pix);
  SDL_Surface *surf= NULL;
  unsigned char *pixels= tm_new_array<unsigned char>(w*h*4);
  memcpy (pixels, samples, w*h*4);
  // the SDL pixel data is not copied so we need to ensure that the pixmap stays alive.
  surf= SDL_CreateSurfaceFrom (w, h, SDL_PIXELFORMAT_RGBA32, pixels, 4*w);
  // FIXME: premultiplied?
  SDL_Texture *tex= SDL_CreateTextureFromSurface (sdl_ren, surf);
  SDL_SetTextureBlendMode (tex, SDL_BLENDMODE_NONE);
  SDL_RenderClear (sdl_ren);
  SDL_RenderTexture (sdl_ren, tex, NULL, dest);
  SDL_DestroyTexture (tex);
  unsigned char *p= (unsigned char*)surf->pixels;
  SDL_DestroySurface (surf);
  tm_delete_array (p);
//  SDL_RenderPresent (sdl_ren);
}


/******************************************************************************
* Message passing
******************************************************************************/

void
vue_widget_rep::send (slot s, blackbox val) {
  (void) val;
  cout << "vue_widget_rep::send(), unhandled " << slot_name (s)
             << " for widget of type: " << type << LF;
  //FAILED ("no default implementation");
}

blackbox fake_query (slot s, int type_id) {
  static int id= 1;
  switch (s) {
  case SLOT_IDENTIFIER:
    {
      check_type_id<int> (type_id, s);
      return close_box<int> (id++);
    }
  case SLOT_SCROLL_POSITION:
    return close_box<coord2> (coord2 (0, 0));
  case SLOT_EXTENTS:
  case SLOT_VISIBLE_PART:
    return close_box<coord4> (coord4 (0, 0, 640, 400));
  case SLOT_ZOOM_FACTOR:
    return close_box<double> (1.0);
  case SLOT_POSITION:
    return close_box<coord2> (coord2 (0, 0));
  case SLOT_SIZE:
    return close_box<coord2> (coord2 (640, 400));
  case SLOT_HEADER_VISIBILITY:
  case SLOT_MAIN_ICONS_VISIBILITY:
  case SLOT_MODE_ICONS_VISIBILITY:
  case SLOT_FOCUS_ICONS_VISIBILITY:
  case SLOT_USER_ICONS_VISIBILITY:
  case SLOT_FOOTER_VISIBILITY:
  case SLOT_SIDE_TOOLS_VISIBILITY:
  case SLOT_LEFT_TOOLS_VISIBILITY:
  case SLOT_BOTTOM_TOOLS_VISIBILITY:
  case SLOT_EXTRA_TOOLS_VISIBILITY:
    check_type_id<bool> (type_id, s);
    return close_box<bool> (false);
  case SLOT_INVALID:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (false);
  default:
      return blackbox ();
  }
}

blackbox
vue_widget_rep::query (slot s, int type_id)  {
  (void) type_id;
  if ((slot_id(s) != SLOT_INVALID) && (slot_id(s) != SLOT_VISIBLE_PART)) {
    cout << "vue_widget_rep::query(), unhandled " << slot_name (s)
    << " for widget of type: " << type << LF;
  }
  return fake_query (s, type_id);
}

widget
vue_widget_rep::read (slot s, blackbox index)  {
  (void) index;
  cout << "vue_widget_rep::read(), unhandled " << slot_name (s)
       << " for widget of type: " << type << LF;
  return empty_widget ();
}

void
vue_widget_rep::write (slot s, blackbox index, widget w)  {
  (void) index; (void) w;
  cout << "vue_widget_rep::write(), unhandled " << slot_name (s)
       << " for widget of type: " << type << LF;
}

void
vue_widget_rep::notify (slot s, blackbox new_val) {
  cout << "vue_widget_rep::notify(), unhandled " << slot_name (s)
           << " for widget of type: " << type << LF;
  widget_rep::notify (s, new_val);
}

/******************************************************************************
* plain windows
******************************************************************************/

string type_vue_plain_window_widget("vue_plain_window_widget");

hashmap<int, pointer> id_to_window;

class vue_window_rep {
public:
  static int serial;
  
  int id;
  SDL_Window *sdl_win;
  SDL_Renderer *sdl_ren;
  vue_widget content;
  string name;
  
  string the_name;
  string mod_name;
  
  picture backing_store;
  renderer ren;
  Clay_Context *clay_ctx;
  Clay_Arena clay_arena;
  
  bool relayout;
  
  vue_window_rep (vue_widget w, string name);
  ~vue_window_rep ();
  
  void   set_name (string name);
  string get_name ();
  void   set_modified (bool flag);
  void   set_visibility (bool flag);
  void   set_size (SI w, SI h);
  void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h);
  void   get_size (SI& w, SI& h);
  void   get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h);
  void   set_position (SI x, SI y);
  void   get_position (SI& x, SI& y);
  
  void process_layout ();
};

typedef vue_window_rep* vue_window;

class vue_plain_window_widget_rep : public vue_widget_rep {
  widget wid;
  string name;
  command quit;
  
  vue_window win;
  
  bool visible;
  bool mouse_grab;
  bool modified;
  bool refresh;
  string title;
  string refresh_kind;
  
public:
  vue_plain_window_widget_rep (widget _wid, string _name, command _quit);
  ~vue_plain_window_widget_rep() {}
  
  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
  void notify (slot s, blackbox new_val);
  
  void do_layout ();
}; // class vue_plain_window_widget_rep

widget plain_window_widget (widget wid, string s, command quit) {
  SI root_w, root_h;
  gui_root_extents (root_w, root_h);
  SI min_w= 0, min_h= 0, def_w= root_w, def_h= root_h,
     max_w= root_w, max_h= root_h;
  
  vue_plain_window_widget_rep *wwid= tm_new<vue_plain_window_widget_rep> (wid, s, quit);
  //wwid->win=
  tm_new<vue_window_rep> (wwid, s);
//  plain_window (wwid, s, min_w, min_h, def_w, def_h, max_w, max_h);
  return abstract (wwid);
}
  
vue_plain_window_widget_rep::vue_plain_window_widget_rep (widget _wid, string _name, command _quit)
: vue_widget_rep (type_vue_plain_window_widget), wid(_wid), name(_name), quit(_quit), visible (false) {
  cout << "Creating vue_plain_window_widget" << LF;
}

void
vue_plain_window_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_plain_window_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_IDENTIFIER:
      {
        int id= check_open<int> (val, s);
        win= (vue_window) id_to_window [id];
      }
      break;
    case SLOT_SIZE:
      {
        coord2 p= check_open<coord2> (val, s);
        if (win) {
          win->set_size (p.x1, p.x2);
        }
      }
      break;
    case SLOT_POSITION:
      {
        coord2 p= check_open<coord2> (val, s);
        if (win) {
          win->set_position (p.x1, p.x2);
        }
      }
      break;
    case SLOT_VISIBILITY:
      {
        bool flag= check_open<bool> (val, s);
        if (win) {
          win->set_visibility (flag);
        }
      }
      break;
    case SLOT_MOUSE_GRAB:
      {
        check_type<bool> (val, s);
        bool flag = open_box<bool> (val);  // true= get grab, false= release grab
        if (win) {
          //win->set_mouse_grab (this, flag);
        }
      }
      break;
    case SLOT_NAME:   // sets window *title* not the name
      {
        string name= check_open<string> (val, s);
        if (win) {
          win->set_name (name);
        }
      }
      break;
    case SLOT_MODIFIED:
      {
        bool flag = check_open<bool> (val, s);
        if (win) {
          win->set_modified (flag);
        }
      }
      break;
    case SLOT_REFRESH:
      {
        string kind = check_open<string> (val, s);
        refresh_kind= kind;
      }
      break;
    default:
      vue_widget_rep::send(s, val);
  }
}

blackbox
vue_plain_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "vue_plain_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      check_type_id<int> (type_id, s);
      return close_box<int> (win->id);
    }
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      SI x, y;
      if (win) win->get_position (x, y);
      return close_box<coord2> (coord2 (x, y));
    }
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      SI w, h;
      if (win) win->get_size (w, h);
      return close_box<coord2> (coord2 (w, h));
    }
    default:
      return vue_widget_rep::query (s, type_id);
  }
}

widget
vue_plain_window_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "vue_plain_window_widget_rep::read " << slot_name(s)
                  << "\t type: " << type << LF;
  switch (s) {
    case SLOT_WINDOW:  // We use this in qt_gui_rep::show_help_balloon()
      check_type_void (index, s);
      return this;
    default:
      return vue_widget_rep::read (s, index);
  }
}

void
vue_plain_window_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "vue_plain_window_widget_rep::notify " << slot_name(s) << LF;
  vue_widget_rep::notify (s, new_val);
}

void
vue_plain_window_widget_rep::write (slot s, blackbox index, widget w)  {
  (void) index; (void) w;
  cout << "vue_plain_window_widget_rep::write(), unhandled " << slot_name (s)
       << " for widget of type: " << type << LF;
}

void
vue_plain_window_widget_rep::do_layout () {
  concrete(wid)->do_layout ();
}

//******************************************************************************
// vue_texmacs_widget

//VUE_WIDGET(texmacs_widget, int, mask, command, quit);

class vue_texmacs_widget_rep : public vue_widget_rep {
  int mask;
  command quit;
  vue_widget main_widget;
  
public:
  vue_texmacs_widget_rep (int _mask, command _quit) : mask(_mask), quit(_quit), vue_widget_rep ("vue_texmacs_widget_rep") {};
  
  void send (slot s, blackbox val);
//  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
  void notify (slot s, blackbox new_val);
  
  void do_layout ();
}; // class vue_plain_window_widget_rep

widget texmacs_widget (int mask, command quit) {
  return abstract (tm_new<vue_texmacs_widget_rep> (mask, quit));
}
  

void
vue_texmacs_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_texmacs_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    default:
      vue_widget_rep::send(s, val);
  }
}

widget
vue_texmacs_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_texmacs_widget_rep::read " << slot_name(s)
                  << "\t type: " << type << LF;
  switch (s) {
    case SLOT_CANVAS:
      check_type_void (index, s);
      return abstract (main_widget);

    default:
      return vue_widget_rep::read (s, index);
  }
}

void
vue_texmacs_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_texmacs_widget_rep::notify " << slot_name(s) << LF;
  vue_widget_rep::notify (s, new_val);
}

void
vue_texmacs_widget_rep::write (slot s, blackbox index, widget w)  {
  (void) index; (void) w;
  cout << "vue_texmacs_widget_rep::write(), unhandled " << slot_name (s)
       << " for widget of type: " << type << LF;
  switch (s) {
    case SLOT_SCROLLABLE:
      check_type_void (index, s);
      main_widget= concrete (w);
      break;
    default:
      break;
  }
}

void vue_texmacs_widget_rep::do_layout () {
  // Define an element with 16px of x and y padding
  CLAY({ .id = CLAY_ID("TeXmacsWidget"),
      .layout = {
          .layoutDirection = CLAY_TOP_TO_BOTTOM,
          .sizing = layoutExpand,
          .padding = CLAY_PADDING_ALL(16),
          .childGap = 16
      }}) {
      // A nested child element
      CLAY({ .id = CLAY_ID("MainMenuBar"), .layout = { .childGap = 16 } }) {
          // Children laid out top to bottom with a 16 px gap between them
      }
      // A vertical scrolling container with a colored background
      CLAY({
          .layout = { .layoutDirection = CLAY_TOP_TO_BOTTOM, .childGap = 16 },
          .backgroundColor = { 200, 200, 100, 255 },
          .cornerRadius = CLAY_CORNER_RADIUS(10),
          .clip = { .vertical = true, .childOffset = Clay_GetScrollOffset() }
      }) {
          // child elements
         if (!is_nil (main_widget)) main_widget->do_layout ();
      }
  }
}

//******************************************************************************
// vue_window

int nr_windows= 0;

hashmap<SDL_Window*, pointer> Window_to_window;

typedef vue_window_rep* vue_window;

int vue_window_rep::serial= 1; // serial identifier for windows

void HandleClayErrors (Clay_ErrorData errorData) {
    // See the Clay_ErrorData struct for more information
    printf ("%s", errorData.errorText.chars);
    switch (errorData.errorType) {
      default:
        // etc
    }
}

vue_window_rep::vue_window_rep (vue_widget _content, string _name)
  : content (_content), name (_name), id (serial++)
{
  cout << "create vue_window_rep " << id << LF;
  SDL_WindowFlags flags= SDL_WINDOW_HIGH_PIXEL_DENSITY;
  int win_w= 600, win_h= 400;
  int win_x=100, win_y= 100;
  c_string buf (name);
  
  if (!SDL_CreateWindowAndRenderer (buf, win_w, win_h, flags, &sdl_win, &sdl_ren)) {
    SDL_LogError (SDL_LOG_CATEGORY_APPLICATION, "Couldn't create window and renderer: %s", SDL_GetError());
  }
  
  nr_windows++;
  SDL_SetWindowPosition (sdl_win, win_x, win_y);
  Window_to_window (sdl_win)= (void*) this;
  id= serial++;
  id_to_window (id)= this;
  
  backing_store= native_picture (win_w * retina_factor, win_h  * retina_factor, 0, 0);
  ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
  
  // update widget state
  set_identifier (abstract (content), id);
  notify_position (abstract (content), 0, 0);
  notify_size (abstract (content), win_w,  win_h);
  
  // init Clay
  uint64_t totalMemorySize = Clay_MinMemorySize ();
  Clay_Arena clay_arena = (Clay_Arena) {
      .memory =  (char*) SDL_malloc (totalMemorySize),
      .capacity = totalMemorySize
  };

  clay_ctx= Clay_Initialize (clay_arena, (Clay_Dimensions) { (float) win_w, (float) win_h }, (Clay_ErrorHandler) { HandleClayErrors });
  relayout= true;
}

vue_window_rep::~vue_window_rep () {
  cout << "destroy vue_window_rep " << id << LF;
  id_to_window->reset (id);
  id= 0;
  set_identifier (abstract (content), 0); // FIXME: is this ok?
  Window_to_window->reset (sdl_win);
  nr_windows--;

  SDL_free (clay_arena.memory);
  SDL_DestroyRenderer (sdl_ren);
  SDL_DestroyWindow (sdl_win);
  delete_renderer (ren);
}

void
vue_window_rep::get_position (SI& x, SI& y) {
  int xx, yy;
  SDL_GetWindowPosition (sdl_win, &xx, &yy);
  x=  xx*PIXEL;
  y= -yy*PIXEL;
}

void
vue_window_rep::get_size (SI& ww, SI& hh) {
  int win_w, win_h;
  SDL_GetWindowSize (sdl_win, &win_w, &win_h);
  ww= win_w*PIXEL;
  hh= win_h*PIXEL;
}

void
vue_window_rep::get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h) {
  //min_w= Min_w; min_h= Min_h; max_w= Max_w; max_h= Max_h;
}

void
vue_window_rep::set_position (SI x, SI y) {
  SI screen_w, screen_h;
  gui_root_extents (screen_w, screen_h);
  screen_w /= PIXEL; screen_h /= PIXEL;
  
  int win_w, win_h;
  SDL_GetWindowSize (sdl_win, &win_w, &win_h);

  x= x/PIXEL;
  y= -y/PIXEL;
  if ((x+ win_w) > screen_w) x= screen_w- win_w;
  if (x<0) x=0;
  if ((y+ win_h) > screen_h) y= screen_h- win_h;
  if (y<0) y=0;
  SI win_x= x, win_y= y;
  SDL_SetWindowPosition (sdl_win, win_x, win_y);
}

void
vue_window_rep::set_size (SI w, SI h) {
  w= w/PIXEL; h= h/PIXEL;
  //h=-h; ren->decode (w, h);
  SDL_SetWindowSize (sdl_win, w, h);
}

void
vue_window_rep::set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) {
#if 0
  if (min_w == Min_w && min_h == Min_h && max_w == Max_w && max_h == Max_h)
    return;
  Min_w= min_w; Min_h= min_h; Max_w= max_w; Max_h= max_h;
  min_w= min_w/PIXEL; min_h= min_h/PIXEL;
  max_w= max_w/PIXEL; max_h= max_h/PIXEL;
  SDL_SetWindowMaximumSize (sdl_win, max_w, max_h);
  SDL_SetWindowMinimumSize (sdl_win, min_w, min_h);
#endif
}

void
vue_window_rep::set_name (string name) {
  if (the_name != name) {
    c_string s (name);
    SDL_SetWindowTitle (sdl_win, s);
    the_name= name;
    mod_name= name;
  }
}

string
vue_window_rep::get_name () {
  return the_name;
}

void
vue_window_rep::set_modified (bool flag) {
  string name= (flag? (the_name * " *"): the_name);
  if (mod_name != name) {
    c_string s (name);
    SDL_SetWindowTitle (sdl_win, s);
    mod_name= name;
  }
}

void
vue_window_rep::set_visibility (bool flag) {
  if (flag) SDL_ShowWindow (sdl_win);
  else SDL_HideWindow (sdl_win);
}
 
void
vue_window_rep::process_layout () {
  
  Clay_SetCurrentContext (clay_ctx);
  current_window_widget= content;
  
  int win_x, win_y, win_w, win_h;
  SDL_GetWindowSize (sdl_win, &win_w, &win_h);
  SDL_GetWindowPosition (sdl_win, &win_w, &win_h);
  Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) win_w, (float) win_h });
  
  // All clay layouts are declared between Clay_BeginLayout and Clay_EndLayout
  Clay_BeginLayout ();
  
  content->do_layout ();
  
  // All clay layouts are declared between Clay_BeginLayout and Clay_EndLayout
  Clay_RenderCommandArray renderCommands= Clay_EndLayout ();
  current_window_widget= NULL;
  
  //FIXME: render!
}

//******************************************************************************
// vue_gui

/******************************************************************************
* Main routines
******************************************************************************/

bool char_clip= true;

void gui_open (int& argc, char** argv) {
  // start the gui
  
  if (!TTF_Init()) {
      return SDL_APP_FAILURE;
  }

  if (!SDL_Init (SDL_INIT_VIDEO|SDL_INIT_AUDIO)) {
    SDL_Log ("Unable to initialize SDL: %s", SDL_GetError ());
    exit (-1);
  }
  SDL_SetHint (SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, "1");
  
  set_retina_factor (2);
  initialize_colors ();
//  initialize_keyboard ();
}

void gui_close () {
  // cleanly close the gui
  SDL_Quit();
}

void gui_root_extents (SI& width, SI& height)
{
  // get the screen size
  SDL_Rect r;
  if (SDL_GetDisplayBounds (1, &r)) {
    width= r.w*PIXEL;
    height= r.h*PIXEL;
    //cout << "SCREEN:" << screen_width << "," << screen_height << LF;
  } else {
    SDL_Log ("SDL_GetDisplayBounds failed: %s", SDL_GetError ());
  }
}

void gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  gui_root_extents (width, height);
}

void gui_refresh () {
  // update and redraw all windows (e.g. on change of output language)
}

string gui_version () {
  // retrieve the type of GUI that is being used
  return "vue";
}

/******************************************************************************
* Hack for getting the remote time
******************************************************************************/

static bool   time_initialized= false;
static time_t time_difference = 0;

static void
synchronize_time (Uint32 t) {
  if (time_initialized && time_difference == 0) return;
  time_t d= texmacs_time () - ((time_t) t);
  if (time_initialized) {
    if (d < time_difference)
      time_difference= d;
  }
  else {
    time_initialized= true;
    time_difference = d;
  }
  if (-1000 <= time_difference && time_difference <= 1000)
    time_difference= 0;
}

static time_t
remote_time (Uint32 t) {
  return ((time_t) t) + time_difference;
}

/******************************************************************************
* Event loop
******************************************************************************/

#define MIN_DELAY   10
#define MAX_DELAY   1000
#define SLEEP_AFTER 120000

extern int nr_windows;
static void (*the_interpose_handler) (void) = NULL;

///////// Gui state

static int  kbd_count= 0;
static bool request_partial_redraw= false;
static bool interrupted= false;
static time_t interrupt_time=0;

static unsigned int mouse_state= 0;

hashmap<int,string> lower_key;
hashmap<int,string> upper_key;

/////////

void gui_interpose (void (*f) (void)) {
  // specify an interpose routine for the main loop
  the_interpose_handler= f;
}

int number_of_servers (); // in texmacs_server.hpp

void process_event(SDL_Event *event);
void process_messages();
void process_layout ();

void gui_start_loop () {
  // start the main loop
  bool wait = true;
  int  count= 0;
  int  delay= MIN_DELAY;

  while (nr_windows>0 || number_of_servers () != 0) {
    request_partial_redraw= false;

    // Get events
    SDL_Event event;
    if (SDL_PollEvent (&event)) {
      process_event (&event);
      count= 0;
      delay= MIN_DELAY;
      wait = false;
    }
    if (nr_windows == 0) continue;

    // FIXME: Don't typeset when resizing window

    // Wait for events on all channels and interpose
    //time_t t1= texmacs_time ();
    if (wait) {
//      struct timeval tv;
//      tv.tv_sec  = delay/1000;
//      tv.tv_usec = 1000 * (delay%1000);
//      select (0, NULL, NULL, NULL, &tv);
      SDL_Delay (delay);
      count += delay;
      if (count >= SLEEP_AFTER) delay= MAX_DELAY;
    }
    else wait= true;
    
    if (the_interpose_handler != NULL) the_interpose_handler ();
    if (nr_windows == 0) continue;
    //time_t t2= texmacs_time ();
    //if (t2 - t1 >= 10) cout << "interpose took " << t2-t1 << "ms\n";

    // process layout
    process_layout ();
    
    // Redraw invalid windows
    //time_t t3= texmacs_time ();
    int n_events= SDL_PollEvent (NULL);
    if (n_events == 0 || request_partial_redraw) {
      interrupted= false;
      interrupt_time= texmacs_time () + (100 / (n_events + 1));
      iterator<SDL_Window*> it= iterate (Window_to_window);
      while (it->busy()) { // first the window which has the focus
        vue_window_rep *win= (vue_window_rep*) Window_to_window [it->next()];
        //if (win->has_focus) win->repaint_invalid_regions();
      }
      it= iterate (Window_to_window);
      while (it->busy()) { // and then the other windows
        vue_window_rep *win= (vue_window_rep*) Window_to_window [it->next()];
        //if (!win->has_focus) win->repaint_invalid_regions();
      }
    }
    //time_t t4= texmacs_time ();
    //if (t4 - t3 >= 10) cout << "redraw took " << t4-t3 << "ms\n";

    process_messages ();
  }
}

void process_layout () {
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy()) { // and then the other windows
    vue_window_rep *win= (vue_window_rep*) Window_to_window [it->next()];
    win->process_layout ();
  }
}


static vue_window
get_window_from_ID (Uint32 ID) {
  SDL_Window *w= SDL_GetWindowFromID (ID);
  if (w == NULL) return NULL;
  vue_window win= (vue_window) Window_to_window [w];
  return win;
}

static void update_mouse_state () {
  unsigned int state= 0;

  float x, y;

  Uint32 buttons= SDL_GetGlobalMouseState (&x, &y);
  SDL_Keymod mods= SDL_GetModState();

  // compute state
  if ((buttons & SDL_BUTTON_LMASK) != 0)  state += 1;
  if ((buttons & SDL_BUTTON_MMASK) != 0)  state += 2;
  if ((buttons & SDL_BUTTON_RMASK) != 0)  state += 4;
  if ((buttons & SDL_BUTTON_X1MASK) != 0) state += 8;
  if ((buttons & SDL_BUTTON_X2MASK) != 0) state += 16;
  if ((mods & SDL_KMOD_SHIFT) != 0) state += 256;
  if ((mods & SDL_KMOD_CTRL)  != 0) state += 1024 + 4;
  if ((mods & SDL_KMOD_ALT)  != 0)  state += 2048 + 2;
  if ((mods & SDL_KMOD_GUI)  != 0)  state += 4096;
//  if ((mods & SDL_KMOD_CAPS)  != 0) state += 1024;
  mouse_state= state;
}

static string
lookup_mouse (Uint8 button) {
  if (button == SDL_BUTTON_LEFT)   return "left";
  if (button == SDL_BUTTON_MIDDLE) return "middle";
  if (button == SDL_BUTTON_RIGHT)  return "right";
  if (button == SDL_BUTTON_X1)     return "extra1";
  if (button == SDL_BUTTON_X2)     return "extra2";
  return "button-error";
}

static string
mouse_decode (unsigned int mstate) {
  // we check (mstate & 1) at last since it is usually set
  if (mstate & 2)       return "middle";
  else if (mstate & 4)  return "right";
  else if (mstate & 8)  return "up";
  else if (mstate & 16) return "down";
  else if (mstate & 1)  return "left";
  return "unknown";
}

static string
lookup_key (SDL_Keycode key, SDL_Keymod mod) {
  const char* str= SDL_GetKeyName (key);
  string r (str, (int)strlen (str));
  r= utf8_to_cork (r);
  if (contains_unicode_char (r)) return r;
//  string s=r;
  string s= ((mod & SDL_KMOD_SHIFT) ? upper_key [key] : lower_key [key]);
  if ((N(s)>=2) && (s[0]=='K') && (s[1]=='-')) s= s (2, N(s));

  if (mod & SDL_KMOD_CTRL) s= "C-" * s;
  if (mod & SDL_KMOD_ALT)  s= "A-" * s;
  if (mod & SDL_KMOD_GUI)  s= "M-" * s;
  cout << "key press: " << s << LF;
  return s;
}

// Print modifier info
static string
print_modifiers (SDL_Keymod mod) {
  string s;
  s << " Modifers: [" << as_string (mod) << " ";
  
  // If there are none then say so and return.
  if( mod == SDL_KMOD_NONE ){
    s << "None ]\n";
    return s;
  }
  
  // Check for the presence of each SDLMod value
  if( mod & SDL_KMOD_NUM )    s << "NUMLOCK ";
  if( mod & SDL_KMOD_CAPS )   s << "CAPSLOCK ";
  if( mod & SDL_KMOD_LCTRL )  s << "LCTRL ";
  if( mod & SDL_KMOD_RCTRL )  s << "RCTRL ";
  if( mod & SDL_KMOD_RSHIFT ) s << "RSHIFT ";
  if( mod & SDL_KMOD_LSHIFT ) s << "LSHIFT ";
  if( mod & SDL_KMOD_RALT )   s << "RALT ";
  if( mod & SDL_KMOD_LALT )   s << "LALT ";
  if( mod & SDL_KMOD_RGUI )   s << "RGUI ";
  if( mod & SDL_KMOD_LGUI )   s << "LGUI ";
  if( mod & SDL_KMOD_CTRL )   s << "CTRL ";
  if( mod & SDL_KMOD_SHIFT )  s << "SHIFT ";
  if( mod & SDL_KMOD_ALT )    s << "ALT ";
  if( mod & SDL_KMOD_GUI )    s << "GUI ";
  s << "]";
  return s;
}

// Print all information about a key event
static string
print_key_info ( SDL_KeyboardEvent *key ) {
  string s;
  // Is it a release or a press?
  s <<  (key->type == SDL_EVENT_KEY_UP ? "Release:- " : "Press:- ");
  // Print the hardware scancode first
  s << "Scancode: " << as_hexadecimal (key->scancode);
  // Print the name of the key
  s << ", Name: " << SDL_GetKeyName (key->key);
  // Print modifier info
  s << print_modifiers (key->mod);
  return s;
}

void
process_event (SDL_Event *event) {
  vue_window win;
  switch (event->type) {
    case SDL_EVENT_WINDOW_SHOWN:
      SDL_Log("Window %d shown", event->window.windowID);
      win= get_window_from_ID (event->window.windowID);
      break;
    case SDL_EVENT_WINDOW_HIDDEN:
      SDL_Log("Window %d hidden", event->window.windowID);
      break;
    case SDL_EVENT_WINDOW_EXPOSED:
      SDL_Log("Window %d exposed", event->window.windowID);
      break;
    case SDL_EVENT_WINDOW_MOVED:
      SDL_Log("Window %d moved to %d,%d",
              event->window.windowID, event->window.data1,
              event->window.data2);
      win= get_window_from_ID (event->window.windowID);
      break;
    case SDL_EVENT_WINDOW_RESIZED:
      SDL_Log("Window %d resized to %dx%d",
              event->window.windowID, event->window.data1,
              event->window.data2);
      win= get_window_from_ID (event->window.windowID);
      if (win) {
        Clay_SetCurrentContext (win->clay_ctx);
        Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) event->window.data1, (float) event->window.data2 });
        win->relayout= true;
      }
      break;
    case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
      SDL_Log("Window %d pixel size changed to %dx%d",
              event->window.windowID, event->window.data1,
              event->window.data2);
      break;
    case SDL_EVENT_WINDOW_MINIMIZED:
      SDL_Log("Window %d minimized", event->window.windowID);
      break;
    case SDL_EVENT_WINDOW_MAXIMIZED:
      SDL_Log("Window %d maximized", event->window.windowID);
      break;
    case SDL_EVENT_WINDOW_RESTORED:
      SDL_Log("Window %d restored", event->window.windowID);
      break;
    case SDL_EVENT_WINDOW_MOUSE_ENTER:
      SDL_Log("Mouse entered window %d",
              event->window.windowID);
        //unmap_balloon ();
      win= get_window_from_ID (event->window.windowID);
      if (win) {
        // FIXME: not quite right
        float x,y;
        int ox,oy;
        update_mouse_state ();
        SDL_GetGlobalMouseState (&x, &y);
        SDL_GetWindowPosition (win->sdl_win, &ox, &oy);
        x -= ox; y -= oy;
        //win->mouse_event ("enter", x, y, texmacs_time ());
      }
        break;
    case SDL_EVENT_WINDOW_MOUSE_LEAVE:
      SDL_Log("Mouse left window %d", event->window.windowID);
      //unmap_balloon ();
      win= get_window_from_ID (event->window.windowID);
      if (win) {
        // FIXME: not quite right
        float x,y;
        int ox,oy;
        update_mouse_state ();
        SDL_GetGlobalMouseState (&x, &y);
        SDL_GetWindowPosition(win->sdl_win, &ox, &oy);
        x -= ox; y -= oy;
        //win->mouse_event ("leave", x, y, texmacs_time ());
      }
      break;
    case SDL_EVENT_WINDOW_FOCUS_GAINED:
      SDL_Log("Window %d gained keyboard focus",
              event->window.windowID);
      win= get_window_from_ID (event->window.windowID);
      //if (win) win->focus_in_event ();
      break;
    case SDL_EVENT_WINDOW_FOCUS_LOST:
      SDL_Log("Window %d lost keyboard focus",
                event->window.windowID);
      win= get_window_from_ID (event->window.windowID);
      //if (win) win->focus_out_event ();
      break;
    case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
      SDL_Log("Window %d close requested", event->window.windowID);
      win= get_window_from_ID (event->window.windowID);
      //if (win) win->destroy_event();
      break;
    case SDL_EVENT_WINDOW_HIT_TEST:
      SDL_Log("Window %d has a special hit test", event->window.windowID);
      break;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
    {
      update_mouse_state ();
      // we need to take into account explicitly the current button
      cout << "new mouse state " << mouse_state << LF;
      win= get_window_from_ID (event->button.windowID);
      if (win) {
        string action;
        if (event->button.type == SDL_EVENT_MOUSE_BUTTON_DOWN) {
          action= "press-" * mouse_decode (mouse_state);
        } else {
          action= "release-" * mouse_decode (mouse_state | SDL_BUTTON_MASK (event->button.button));
        }
        //FIXME: this is not yet correct, as we need to take into account modifiers
        //        action = action * lookup_mouse (event->button.button);
        cout << ">>>>>" << action << LF;
        //        set_button_state (event->button.state ^ get_button_mask (&ev->xbutton));
        //win->mouse_event (action,
        //                 event->button.x, event->button.y, texmacs_time ());
        
        Clay_SetCurrentContext (win->clay_ctx);
        Clay_SetPointerState ((Clay_Vector2) { event->button.x, event->button.y },
                             event->button.button & SDL_BUTTON_LMASK);
      }
      break;
    } // case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_WHEEL:
    {
      update_mouse_state ();
      SDL_Log("Window %d got wheel event event %f %f",
              event->wheel.windowID, event->wheel.x, event->wheel.y);
      win= get_window_from_ID (event->wheel.windowID);
      if (win) {
        int x, y;
        x= event->wheel.mouse_x;
        y= event->wheel.mouse_y;
        //float deltaX= event->wheel.x;
        float deltaY= event->wheel.y;
        if (deltaY >= 0.5) {
          //win->mouse_event ("press-up", x, y, texmacs_time ());
        } else if (deltaY <= -0.5) {
          //win->mouse_event ("press-down", x, y, texmacs_time ());
        }

        Clay_SetCurrentContext (win->clay_ctx);
        Clay_UpdateScrollContainers (true, (Clay_Vector2) { event->wheel.x, event->wheel.y }, 0.01f);
      }
      break;
    } // case SDL_EVENT_MOUSE_WHEEL:
    case SDL_EVENT_MOUSE_MOTION:
    {
      update_mouse_state ();
      win= get_window_from_ID (event->motion.windowID);
      if (win) {
        Clay_SetCurrentContext (win->clay_ctx);
        Clay_SetPointerState ((Clay_Vector2) { event->button.x, event->button.y },
                             event->button.button & SDL_BUTTON_LMASK);

        //win->mouse_event ("move",
        //                  event->motion.x, event->motion.y, texmacs_time ());
      }
      break;
    } // case SDL_EVENT_MOUSE_MOTION:
    case SDL_EVENT_KEY_DOWN:
    {
      SDL_Keycode keycode= SDL_GetKeyFromScancode(event->key.scancode, event->key.mod, false);
      {
        c_string buf (print_key_info (&(event->key)));
        SDL_Log("Keydown: %s ", (char*)buf);
      }
      win= get_window_from_ID (event->key.windowID);
      if (win) {
        string key= lookup_key(keycode, event->key.mod);
        //cout << "Press " << key << " at " << (time_t) ev->xkey.time
        //<< " (" << texmacs_time() << ")\n";
        kbd_count++;
        //FIXME: conversion below loses precision from UInt64 to UInt32
        synchronize_time (event->key.timestamp);
        if (texmacs_time () - remote_time (event->key.timestamp) < 100 ||
            (kbd_count & 15) == 0)
          request_partial_redraw= true;
        //cout << "key   : " << key << "\n";
        //cout << "redraw: " << request_partial_redraw << "\n";
        //if (N(key)>0) win->key_event (key);
      }
      break;
    } // case SDL_EVENT_KEY_DOWN:
  } // switch (event->type)
}

/******************************************************************************
* Font support
******************************************************************************/

void set_default_font (string name) {
  // set the name of the default font
  //FIXME: ignore?
}

font get_default_font (bool tt, bool mini, bool bold) {
  // get the default font, depending on desired characteristics:
  // tt for a monospaced font, mini for a smaller font and bold for a bold font
  FAILED("this should not be called");
  return NULL;
}

void load_system_font (string family, int size, int dpi,
                       font_metric& fnm, font_glyphs& fng) {
  // load the metric and glyphs of a system font
  // you are not obliged to provide any system fonts
  FAILED("this should not be called");
}

/******************************************************************************
* Clipboard support
******************************************************************************/

bool set_selection (string cb, tree t,
                    string s, string sv, string sh, string format) {
  
  // Copy a selection 't' of a given 'format' to the clipboard 'cb',
  // where 's' contains the string serialization of t according to the format
  // and possibly the variants 'sv' and 'sh' for verbatim and html
  // Returns true on success
  
  //FIXME: implement
  return true;
}

bool get_selection (string cb, tree& t, string& s, string format) {
  // Retrieve the selection 't' of a given 'format' from the clipboard 'cb',
  // where 's' is the string serialization of t according to the format
  // Returns true on success; sets t to (extern s) for external selections
  
  //FIXME: implement
  return true;
}

void clear_selection (string cb) {
  // Clear the selection on clipboard 'cb'
  //FIXME: implement
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void beep () {
  // Issue a beep
  //FIXME: implement
}

void needs_update () {
  // Inform the gui that the editor needs to update itself
  // before repainting can start
  //FIXME: implement
}

bool check_event (int type) {
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
  bool status;
  switch (type) {
  case INTERRUPT_EVENT:
    if (interrupted) return true;
    else {
      int n=1; // n=XPending (dpy);
      time_t now= texmacs_time ();
      if (now - interrupt_time < 0) return false;
      else interrupt_time= now + (100 / (n + 1));
      interrupted= (SDL_HasEvent (SDL_EVENT_KEY_DOWN) == true) ||
                   (SDL_HasEvent (SDL_EVENT_MOUSE_BUTTON_DOWN) == true);
      return interrupted;
    }
  case INTERRUPTED_EVENT:
    return interrupted;
  case ANY_EVENT:
    return (SDL_HasEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST) == true);
  case MOTION_EVENT:
    status= (SDL_HasEvent (SDL_EVENT_MOUSE_MOTION) == true);
    return status;
  case DRAG_EVENT:
    {
      status= false;
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_MOTION)) {
        if (event.motion.state) {
          status= true;
        }
      }
    }
    return status;
  case MENU_EVENT:
    status= (SDL_HasEvent (SDL_EVENT_MOUSE_BUTTON_UP) == true);
    if (!status) {
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_MOTION)) {
        status=  (event.motion.state != 0);
      }
    }
    return status;
  }
  return interrupted;
}

void image_gc (string name) {
  // Garbage collect images of a given name (may use wildcards)
  // This routine only needs to be implemented if you use your own image cache
  //FIXME: implement
}

void show_help_balloon (widget balloon, SI x, SI y) {
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
  //FIXME: implement
}

void show_wait_indicator (widget base, string message, string argument) {
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty
  //FIXME: implement
}

void external_event (string type, time_t t) {
  // External events, such as pushing a button of a remote infrared commander
  //FIXME: implement
}

/******************************************************************************
* Delayed messages (NOT NEEDED YET)
******************************************************************************/

struct message_rep: concrete_struct {
  widget wid;
  string s;
  time_t t;
  message_rep (widget wid2, string s2, time_t t2):
    wid (wid2), s (s2), t (t2) {}
  friend class message;
};

class message {
  CONCRETE(message);
  message (widget wid, string s, time_t t):
    rep (tm_new<message_rep> (wid, s, t)) {}

};
CONCRETE_CODE(message);

tm_ostream&
operator << (tm_ostream& out, message m) {
  return out << "message " << m->s << " to " << m->wid
       << "at time " << m->t << "\n";
}

list<message> messages;

static list<message>
insert_message (list<message> l, widget wid, string s, time_t cur, time_t t) {
  if (is_nil (l)) return list<message> (message (wid, s, t));
  time_t ref= l->item->t;
  if ((t-cur) <= (ref-cur)) return list<message> (message (wid, s, t), l);
  return list<message> (l->item, insert_message (l->next, wid, s, cur, t));
}

void
delayed_message (widget wid, string s, time_t delay) {
  time_t ct= texmacs_time ();
  messages= insert_message (messages, wid, s, ct, ct+ delay);
}

void process_messages() {
  // Handle alarm messages
  if (!is_nil (messages)) {
    list<message> not_ready;
    while (!is_nil (messages)) {
      time_t ct= texmacs_time ();
      message m= messages->item;
      if ((m->t - ct) <= 0) send_delayed_message (m->wid, m->s, m->t);
      else not_ready= list<message> (m, not_ready);
      messages= messages->next;
    }
    messages= not_ready;
  }
}


/******************************************************************************
* Set up keyboard
******************************************************************************/

#ifndef SDLK_ISO_Left_Tab
#define SDLK_ISO_Left_Tab 0xFE20
#endif

void
map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= "S-" * s;
}

void
Map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= s;
}

void
MMap (int key, string s1, string s2) {
  lower_key (key)= s1;
  upper_key (key)= s2;
}

void
initialize_keyboard () {
  static bool initialized= false;
  if (initialized) return;
  initialized= true;
  
  // Latin characters
  MMap (SDLK_A, "a", "A");
  MMap (SDLK_B, "b", "B");
  MMap (SDLK_C, "c", "C");
  MMap (SDLK_D, "d", "D");
  MMap (SDLK_E, "e", "E");
  MMap (SDLK_F, "f", "F");
  MMap (SDLK_G, "g", "G");
  MMap (SDLK_H, "h", "H");
  MMap (SDLK_I, "i", "I");
  MMap (SDLK_J, "j", "J");
  MMap (SDLK_K, "k", "K");
  MMap (SDLK_L, "l", "L");
  MMap (SDLK_M, "m", "M");
  MMap (SDLK_N, "n", "N");
  MMap (SDLK_O, "o", "O");
  MMap (SDLK_P, "p", "P");
  MMap (SDLK_Q, "q", "Q");
  MMap (SDLK_R, "r", "R");
  MMap (SDLK_S, "s", "S");
  MMap (SDLK_T, "t", "T");
  MMap (SDLK_U, "u", "U");
  MMap (SDLK_V, "v", "V");
  MMap (SDLK_W, "w", "W");
  MMap (SDLK_X, "x", "X");
  MMap (SDLK_Y, "y", "Y");
  MMap (SDLK_Z, "z", "Z");
#if 0
  Map (SDLK_A, "A");
  Map (SDLK_B, "B");
  Map (SDLK_C, "C");
  Map (SDLK_D, "D");
  Map (SDLK_E, "E");
  Map (SDLK_F, "F");
  Map (SDLK_G, "G");
  Map (SDLK_H, "H");
  Map (SDLK_I, "I");
  Map (SDLK_J, "J");
  Map (SDLK_K, "K");
  Map (SDLK_L, "L");
  Map (SDLK_M, "M");
  Map (SDLK_N, "N");
  Map (SDLK_O, "O");
  Map (SDLK_P, "P");
  Map (SDLK_Q, "Q");
  Map (SDLK_R, "R");
  Map (SDLK_S, "S");
  Map (SDLK_T, "T");
  Map (SDLK_U, "U");
  Map (SDLK_V, "V");
  Map (SDLK_W, "W");
  Map (SDLK_X, "X");
  Map (SDLK_Y, "Y");
  Map (SDLK_Z, "Z");
#endif
  Map (SDLK_0, "0");
  Map (SDLK_1, "1");
  Map (SDLK_2, "2");
  Map (SDLK_3, "3");
  Map (SDLK_4, "4");
  Map (SDLK_5, "5");
  Map (SDLK_6, "6");
  Map (SDLK_7, "7");
  Map (SDLK_8, "8");
  Map (SDLK_9, "9");

#if 0
  // Cyrillic letters
  Map (SDLK_Cyrillic_a,   "\xe0");
  Map (SDLK_Cyrillic_be,  "\xe1");
  Map (SDLK_Cyrillic_ve,  "\xe2");
  Map (SDLK_Cyrillic_ghe, "\xe3");
  Map (SDLK_Cyrillic_de,  "\xe4");
  Map (SDLK_Cyrillic_ie,  "\xe5");
  Map (SDLK_Cyrillic_io,  "\xbc");
  Map (SDLK_Cyrillic_zhe, "\xe6");
  Map (SDLK_Cyrillic_ze,  "\xe7");
  Map (SDLK_Cyrillic_i,   "\xe8");
  Map (SDLK_Cyrillic_shorti,   "\xe9");
  Map (SDLK_Cyrillic_ka,  "\xea");
  Map (SDLK_Cyrillic_el,  "\xeb");
  Map (SDLK_Cyrillic_em,  "\xec");
  Map (SDLK_Cyrillic_en,  "\xed");
  Map (SDLK_Cyrillic_o,   "\xee");
  Map (SDLK_Cyrillic_pe,  "\xef");
  Map (SDLK_Cyrillic_er,  "\xf0");
  Map (SDLK_Cyrillic_es,  "\xf1");
  Map (SDLK_Cyrillic_te,  "\xf2");
  Map (SDLK_Cyrillic_u,   "\xf3");
  Map (SDLK_Cyrillic_ef,  "\xf4");
  Map (SDLK_Cyrillic_ha,  "\xf5");
  Map (SDLK_Cyrillic_tse, "\xf6");
  Map (SDLK_Cyrillic_che, "\xf7");
  Map (SDLK_Cyrillic_sha, "\xf8");
  Map (SDLK_Cyrillic_shcha,    "\xf9");
  Map (SDLK_Cyrillic_hardsign, "\xfa");
  Map (SDLK_Cyrillic_yeru,     "\xfb");
  Map (SDLK_Cyrillic_softsign, "\xfc");
  Map (SDLK_Cyrillic_e,   "\xfd");
  Map (SDLK_Cyrillic_yu,  "\xfe");
  Map (SDLK_Cyrillic_ya,  "\xff");
  Map (SDLK_Cyrillic_A,   "\xc0");
  Map (SDLK_Cyrillic_BE,  "\xc1");
  Map (SDLK_Cyrillic_VE,  "\xc2");
  Map (SDLK_Cyrillic_GHE, "\xc3");
  Map (SDLK_Cyrillic_DE,  "\xc4");
  Map (SDLK_Cyrillic_IE,  "\xc5");
  Map (SDLK_Cyrillic_IO,  "\x9c");
  Map (SDLK_Cyrillic_ZHE, "\xc6");
  Map (SDLK_Cyrillic_ZE,  "\xc7");
  Map (SDLK_Cyrillic_I,   "\xc8");
  Map (SDLK_Cyrillic_SHORTI,   "\xc9");
  Map (SDLK_Cyrillic_KA,  "\xca");
  Map (SDLK_Cyrillic_EL,  "\xcb");
  Map (SDLK_Cyrillic_EM,  "\xcc");
  Map (SDLK_Cyrillic_EN,  "\xcd");
  Map (SDLK_Cyrillic_O,   "\xce");
  Map (SDLK_Cyrillic_PE,  "\xcf");
  Map (SDLK_Cyrillic_ER,  "\xd0");
  Map (SDLK_Cyrillic_ES,  "\xd1");
  Map (SDLK_Cyrillic_TE,  "\xd2");
  Map (SDLK_Cyrillic_U,   "\xd3");
  Map (SDLK_Cyrillic_EF,  "\xd4");
  Map (SDLK_Cyrillic_HA,  "\xd5");
  Map (SDLK_Cyrillic_TSE, "\xd6");
  Map (SDLK_Cyrillic_CHE, "\xd7");
  Map (SDLK_Cyrillic_SHA, "\xd8");
  Map (SDLK_Cyrillic_SHCHA,    "\xd9");
  Map (SDLK_Cyrillic_HARDSIGN, "\xda");
  Map (SDLK_Cyrillic_YERU,     "\xdb");
  Map (SDLK_Cyrillic_SOFTSIGN, "\xdc");
  Map (SDLK_Cyrillic_E,   "\xdd");
  Map (SDLK_Cyrillic_YU,  "\xde");
  Map (SDLK_Cyrillic_YA,  "\xdf");

  //Ukrainian letters in T2A encoding
  Map (SDLK_Ukrainian_i,   "i"); // Fall back!
  Map (SDLK_Ukrainian_I,   "I"); // Fall back!
  Map (SDLK_Ukrainian_yi,   "\xa8");
  Map (SDLK_Ukrainian_YI,   "\x88");
  Map (SDLK_Ukrainian_ie,   "\xb9");
  Map (SDLK_Ukrainian_IE,   "\x99");
  // Map (SDLK_Ukrainian_ghe_with_upturn,   "\xa0");
  // Map (SDLK_Ukrainian_GHE_WITH_UPTURN,   "\x80");
  Map (0x6ad,   "\xa0");
  Map (0x6bd,   "\x80");
#endif
  
  // Standard ASCII Symbols
  Map (SDLK_EXCLAIM, "!");
  Map (SDLK_DBLAPOSTROPHE, "\x22");
  Map (SDLK_HASH, "#");
  Map (SDLK_DOLLAR, "$");
  Map (SDLK_PERCENT, "%");
  Map (SDLK_AMPERSAND, "&");
  Map (SDLK_APOSTROPHE, "'");
  Map (SDLK_LEFTPAREN, "(");
  Map (SDLK_RIGHTPAREN, ")");
  Map (SDLK_ASTERISK, "*");
  Map (SDLK_PLUS, "+");
  Map (SDLK_COMMA, ",");
  Map (SDLK_MINUS, "-");
  Map (SDLK_PERIOD, ".");
  Map (SDLK_SLASH, "/");
  Map (SDLK_COLON, ":");
  Map (SDLK_SEMICOLON, ";");
  Map (SDLK_LESS, "<");
  Map (SDLK_EQUALS, "=");
  Map (SDLK_GREATER, ">");
  Map (SDLK_QUESTION, "?");
  Map (SDLK_AT, "@");
  Map (SDLK_LEFTBRACKET, "[");
  Map (SDLK_BACKSLASH, "\\");
  Map (SDLK_RIGHTBRACKET, "]");
  Map (SDLK_CARET, "^");
  Map (SDLK_UNDERSCORE, "_");
  Map (SDLK_GRAVE, "`");
  Map (SDLK_LEFTBRACKET, "{");
  Map (SDLK_KP_VERTICALBAR, "|");
  Map (SDLK_RIGHTBRACKET, "}");
  //Map (SDLK_TILDA, "~");

  // dead keys
  Map (0xFE50, "grave");
  Map (0xFE51, "acute");
  Map (0xFE52, "hat");
  Map (0xFE53, "tilde");
  Map (0xFE54, "macron");
  Map (0xFE55, "breve");
  Map (0xFE56, "abovedot");
  Map (0XFE57, "umlaut");
  Map (0xFE58, "abovering");
  Map (0xFE59, "doubleacute");
  Map (0xFE5A, "check");
  Map (0xFE5B, "cedilla");
  Map (0xFE5C, "ogonek");
  Map (0xFE5D, "iota");
  Map (0xFE5E, "voicedsound");
  Map (0xFE5F, "semivoicedsound");
  Map (0xFE60, "belowdot");

#if 0
  // Extended symbols and accented characters
  Map (SDLK_nobreakspace, "varspace");
  Map (SDLK_exclamdown, "exclamdown");
  Map (SDLK_cent, "cent");
  Map (SDLK_sterling, "sterling");
  Map (SDLK_currency, "currency");
  Map (SDLK_yen, "yen");
  Map (SDLK_brokenbar, "brokenbar");
  Map (SDLK_section, "section");
  Map (SDLK_diaeresis, "umlaut");
  Map (SDLK_copyright, "copyright");
  Map (SDLK_ordfeminine, "ordfeminine");
  Map (SDLK_guillemotleft, "guillemotleft");
  Map (SDLK_notsign, "notsign");
  Map (SDLK_hyphen, "hyphen");
  Map (SDLK_registered, "registered");
  Map (SDLK_macron, "macron");
  Map (SDLK_degree, "degree");
  Map (SDLK_plusminus, "plusminus");
  Map (SDLK_twosuperior, "twosuperior");
  Map (SDLK_threesuperior, "threesuperior");
  Map (SDLK_acute, "acute");
  Map (SDLK_mu, "mu");
  Map (SDLK_paragraph, "paragraph");
  Map (SDLK_periodcentered, "periodcentered");
  Map (SDLK_cedilla, "cedilla");
  Map (SDLK_onesuperior, "onesuperior");
  Map (SDLK_masculine, "masculine");
  Map (SDLK_guillemotright, "guillemotright");
  Map (SDLK_onequarter, "onequarter");
  Map (SDLK_onehalf, "onehalf");
  Map (SDLK_threequarters, "threequarters");
  Map (SDLK_questiondown, "questiondown");
  Map (SDLK_multiply, "times");
  Map (SDLK_division, "div");

  Map (SDLK_Agrave, "\xc0");
  Map (SDLK_Aacute, "\xc1");
  Map (SDLK_Acircumflex, "\xc2");
  Map (SDLK_Atilde, "\xc3");
  Map (SDLK_Adiaeresis, "\xc4");
  Map (SDLK_Aring, "\xc5");
  Map (SDLK_AE, "\xc6");
  Map (SDLK_Ccedilla, "\xc7");
  Map (SDLK_Egrave, "\xc8");
  Map (SDLK_Eacute, "\xc9");
  Map (SDLK_Ecircumflex, "\xca");
  Map (SDLK_Ediaeresis, "\xcb");
  Map (SDLK_Igrave, "\xcc");
  Map (SDLK_Iacute, "\xcd");
  Map (SDLK_Icircumflex, "\xce");
  Map (SDLK_Idiaeresis, "\xcf");
  Map (SDLK_ETH, "\xd0");
  Map (SDLK_Eth, "\xd0");
  Map (SDLK_Ntilde, "\xd1");
  Map (SDLK_Ograve, "\xd2");
  Map (SDLK_Oacute, "\xd3");
  Map (SDLK_Ocircumflex, "\xd4");
  Map (SDLK_Otilde, "\xd5");
  Map (SDLK_Odiaeresis, "\xd6");
  Map (SDLK_OE, "\xd7");
  Map (SDLK_Ooblique, "\xd8");
  Map (SDLK_Ugrave, "\xd9");
  Map (SDLK_Uacute, "\xda");
  Map (SDLK_Ucircumflex, "\xdb");
  Map (SDLK_Udiaeresis, "\xdc");
  Map (SDLK_Yacute, "\xdd");
  Map (SDLK_THORN, "\xde");
  Map (SDLK_Thorn, "\xde");
  Map (SDLK_ssharp, "sz");
  Map (SDLK_agrave, "\xe0");
  Map (SDLK_aacute, "\xe1");
  Map (SDLK_acircumflex, "\xe2");
  Map (SDLK_atilde, "\xe3");
  Map (SDLK_adiaeresis, "\xe4");
  Map (SDLK_aring, "\xe5");
  Map (SDLK_ae, "\xe6");
  Map (SDLK_ccedilla, "\xe7");
  Map (SDLK_egrave, "\xe8");
  Map (SDLK_eacute, "\xe9");
  Map (SDLK_ecircumflex, "\xea");
  Map (SDLK_ediaeresis, "\xeb");
  Map (SDLK_igrave, "\xec");
  Map (SDLK_iacute, "\xed");
  Map (SDLK_icircumflex, "\xee");
  Map (SDLK_idiaeresis, "\xef");
  Map (SDLK_eth, "\xf0");
  Map (SDLK_ntilde, "\xf1");
  Map (SDLK_ograve, "\xf2");
  Map (SDLK_oacute, "\xf3");
  Map (SDLK_ocircumflex, "\xf4");
  Map (SDLK_otilde, "\xf5");
  Map (SDLK_odiaeresis, "\xf6");
  Map (SDLK_oe, "\xf7");
  Map (SDLK_oslash, "\xf8");
  Map (SDLK_ugrave, "\xf9");
  Map (SDLK_uacute, "\xfa");
  Map (SDLK_ucircumflex, "\xfb");
  Map (SDLK_udiaeresis, "\xfc");
  Map (SDLK_yacute, "\xfd");
  Map (SDLK_thorn, "\xfe");
  Map (SDLK_ydiaeresis, "\xff");

  // Symbols from iso-latin-2
  Map (SDLK_Aogonek, "\x81");
  Map (SDLK_breve, "breve");
  Map (SDLK_Lstroke, "\x8a");
  Map (SDLK_Lcaron, "\x89");
  Map (SDLK_Sacute, "\x91");
  Map (SDLK_Scaron, "\x92");
  Map (SDLK_Scedilla, "\x93");
  Map (SDLK_Tcaron, "\x94");
  Map (SDLK_Zacute, "\x99");
  Map (SDLK_Zcaron, "\x9a");
  Map (SDLK_Zabovedot, "\x9b");
  Map (SDLK_aogonek, "\xa1");
  Map (SDLK_ogonek, "ogonek");
  Map (SDLK_lstroke, "\xaa");
  Map (SDLK_lcaron, "\xa9");
  Map (SDLK_sacute, "\xb1");
  Map (SDLK_caron, "caron");
  Map (SDLK_scaron, "\xb2");
  Map (SDLK_scedilla, "\xb3");
  Map (SDLK_tcaron, "\xb4");
  Map (SDLK_zacute, "\xb9");
  Map (SDLK_doubleacute, "doubleacute");
  Map (SDLK_zcaron, "\xba");
  Map (SDLK_zabovedot, "\xbb");
  Map (SDLK_Racute, "\x8f");
  Map (SDLK_Abreve, "\x80");
  Map (SDLK_Lacute, "\x88");
  Map (SDLK_Cacute, "\x82");
  Map (SDLK_Ccaron, "\x83");
  Map (SDLK_Eogonek, "\x86");
  Map (SDLK_Ecaron, "\x85");
  Map (SDLK_Dcaron, "\x84");
  Map (SDLK_Dstroke, "\xd0");
  Map (SDLK_Nacute, "\x8b");
  Map (SDLK_Ncaron, "\x8c");
  Map (SDLK_Odoubleacute, "\x8e");
  Map (SDLK_Rcaron, "\x90");
  Map (SDLK_Uring, "\x97");
  Map (SDLK_Udoubleacute, "\x96");
  Map (SDLK_Tcedilla, "\x95");
  Map (SDLK_racute, "\xaf");
  Map (SDLK_abreve, "\xa0");
  Map (SDLK_lacute, "\xa8");
  Map (SDLK_cacute, "\xa2");
  Map (SDLK_ccaron, "\xa3");
  Map (SDLK_eogonek, "\xa6");
  Map (SDLK_ecaron, "\xa5");
  Map (SDLK_dcaron, "\xa4");
  Map (SDLK_dstroke, "\x9e");
  Map (SDLK_nacute, "\xab");
  Map (SDLK_ncaron, "\xac");
  Map (SDLK_odoubleacute, "\xae");
  Map (SDLK_udoubleacute, "\xb6");
  Map (SDLK_rcaron, "\xb0");
  Map (SDLK_uring, "\xb7");
  Map (SDLK_tcedilla, "\xb5");
  Map (SDLK_abovedot, "abovedot");
#endif
  
  // Special control keys
  Map (SDLK_PAGEUP, "pageup");
  Map (SDLK_PAGEDOWN, "pagedown");
  Map (SDLK_UNDO, "undo");
//  Map (SDLK_REDO, "redo");
  Map (SDLK_CANCEL, "cancel");

  // Control keys
  map (SDLK_SPACE, "space");
  map (SDLK_RETURN, "return");
  map (SDLK_BACKSPACE, "backspace");
  map (SDLK_DELETE, "delete");
  map (SDLK_INSERT, "insert");
  map (SDLK_TAB, "tab");
  map (SDLK_ISO_Left_Tab, "tab");
  map (SDLK_ESCAPE, "escape");
  map (SDLK_LEFT, "left");
  map (SDLK_RIGHT, "right");
  map (SDLK_UP, "up");
  map (SDLK_DOWN, "down");
  map (SDLK_PAGEUP, "pageup");
  map (SDLK_PAGEDOWN, "pagedown");
  map (SDLK_HOME, "home");
  map (SDLK_END, "end");
  map (SDLK_F1, "F1");
  map (SDLK_F2, "F2");
  map (SDLK_F3, "F3");
  map (SDLK_F4, "F4");
  map (SDLK_F5, "F5");
  map (SDLK_F6, "F6");
  map (SDLK_F7, "F7");
  map (SDLK_F8, "F8");
  map (SDLK_F9, "F9");
  map (SDLK_F10, "F10");
  map (SDLK_F11, "F11");
  map (SDLK_F12, "F12");
  map (SDLK_F13, "F13");
  map (SDLK_F14, "F14");
  map (SDLK_F15, "F15");
  map (SDLK_F16, "F16");
  map (SDLK_F17, "F17");
  map (SDLK_F18, "F18");
  map (SDLK_F19, "F19");
  map (SDLK_F20, "F20");
  // map (SDLK_Mode_switch, "modeswitch");

  // Keypad keys
  Map (SDLK_KP_SPACE, "K-space");
  Map (SDLK_KP_ENTER, "K-return");
//  Map (SDLK_KP_DELETE, "K-delete");
//  Map (SDLK_KP_INSERT, "K-insert");
  Map (SDLK_KP_TAB, "K-tab");
//  Map (SDLK_KP_LEFT, "K-left");
//  Map (SDLK_KP_Right, "K-right");
//  Map (SDLK_KP_Up, "K-up");
//  Map (SDLK_KP_Down, "K-down");
//  Map (SDLK_KP_Page_Up, "K-pageup");
//  Map (SDLK_KP_Page_Down, "K-pagedown");
//  Map (SDLK_KP_Home, "K-home");
//  Map (SDLK_KP_Begin, "K-begin");
//  Map (SDLK_KP_End, "K-end");
//  Map (SDLK_KP_F1, "K-F1");
//  Map (SDLK_KP_F2, "K-F2");
//  Map (SDLK_KP_F3, "K-F3");
//  Map (SDLK_KP_F4, "K-F4");
  Map (SDLK_KP_EQUALS, "K-=");
  Map (SDLK_KP_MULTIPLY, "K-*");
  Map (SDLK_KP_PLUS, "K-+");
  Map (SDLK_KP_MINUS, "K--");
  Map (SDLK_KP_PERIOD, "K-.");
  Map (SDLK_KP_COMMA, "K-,");
  Map (SDLK_KP_DIVIDE, "K-/");
  Map (SDLK_KP_0, "K-0");
  Map (SDLK_KP_1, "K-1");
  Map (SDLK_KP_2, "K-2");
  Map (SDLK_KP_3, "K-3");
  Map (SDLK_KP_4, "K-4");
  Map (SDLK_KP_5, "K-5");
  Map (SDLK_KP_6, "K-6");
  Map (SDLK_KP_7, "K-7");
  Map (SDLK_KP_8, "K-8");
  Map (SDLK_KP_9, "K-9");

  // Miscellaneous
  Map (0x20ac, "euro");
}
