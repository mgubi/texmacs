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
#include "scheme.hpp"
#include "window.hpp"
#include "message.hpp"
#include "font.hpp"
#include "dictionary.hpp"

#include "file.hpp" // for file_completions
#include "url.hpp"
#include "tm_window.hpp"

#include "../MuPDF/mupdf_picture.hpp"

widget make_menu_widget (object wid);
extern bool menu_caching;


// A note on TeXmacs' coordinates.
//
// TeXmacs uses a cartesian coordinate system oriended upwards and rightwards,
// rectangles are defined in such way that the second point towards larger
// coordinates than the left. this is assumed in all operations in rectangle.cpp
// (mg: it seems to me)
// graphics API instead uses a downward oriented system, so this has to be
// taken into account when calling APIs. In Vue we stick to TeXmacs' conventions
// and convert right before calling APIs.

// TODO: check what the MuPDF is doing

/*****************************************************************************/
// Clay

#include "clay.h"
#include "clay_grid.h"

Clay_Sizing layoutExpand= {
    .width= CLAY_SIZING_GROW(0),
    .height= CLAY_SIZING_GROW(0)
};

Clay_Sizing layoutFit= {
    .width= CLAY_SIZING_FIT(),
    .height= CLAY_SIZING_FIT()
};

#define CLAY_TM_STRING(s) (CLAY__INIT(Clay_String) { .isStaticallyAllocated= true, .length= N(s), .chars= &(s[0]) })

Clay_Color palette[4]= { {160, 160, 160, 255}, {192, 192, 192, 255},{224, 224, 224, 255},{240, 240, 240, 255} };

Clay_Color color_background= palette[1];
Clay_Color color_highlight=  palette[3];
Clay_Color color_text= {0, 0, 0, 255};

Clay_TextElementConfig *text_config_ui;
Clay_TextElementConfig *text_config_ui_grayed;

/*****************************************************************************/
// UI layout context (maybe refactor in a structure)

// keyboard events
string key_event;
time_t key_time;

// pointer info
string mouse_action;
time_t mouse_time;
unsigned int mouse_x;
unsigned int mouse_y;
unsigned int mouse_state= 0;
array<double> mouse_data;

bool current_popup; // is there an active popup?
bool cancel_popup;  // should we cancel popups?
time_t away_time;   // tolerance for mouse motion

// some more context during layout
Clay_ElementId last_id;
bool debug_clay=false;

// ask the buttons to fit all horizontal space
bool button_grow= false;

uint32_t current_balloon;
time_t balloon_time;

// list of commands
list<command> cmd_list;

vue_window current_window; // used during layout to propagate information

void
gui_init_context() {
  // popup state initialization
  current_popup= false;
  cancel_popup= false;
  
  // make refresh messages available to widgets during layout
  current_window->refresh_kinds= current_window->next_refresh_kinds;
  current_window->next_refresh_kinds= hashset<string>();
  
  // setup text style
  //FIXME: can we do it only once?
  text_config_ui= CLAY_TEXT_CONFIG({
    .fontSize= 26, .textColor= color_text,
    .wrapMode= CLAY_TEXT_WRAP_NONE });
  text_config_ui_grayed= CLAY_TEXT_CONFIG ({
    .fontSize= 26, .textColor= {150, 150, 150, 255},
    .wrapMode= CLAY_TEXT_WRAP_NONE });
}

/*****************************************************************************/

#define DEBUG_VUE (debug (DEBUG_FLAG_QT))
#define DEBUG_VUE_WIDGETS (debug (DEBUG_FLAG_QT_WIDGETS))

/******************************************************************************
 * Type checking
 ******************************************************************************/

// TODO: refactor, this is used by many files

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

unsigned int vue_widget_rep::serial_id= 0;

/******************************************************************************
* umbrella widget class
******************************************************************************/

class vue_ui_rep : public vue_widget_rep {
public:
  blackbox data;
  
  vue_ui_rep (string _type, blackbox _data= NULL);
  virtual ~vue_ui_rep () {};
  
  void send (slot s, blackbox val);
  void do_layout ();
  void render (void *data);
};

template<typename T> widget vue_create (string type, T args) {
  return abstract (tm_new<vue_ui_rep> (type, close_box (args)));
}

// for blackbox
inline bool operator==(const picture &lhs, const picture &rhs)
{ return false; }
inline tm_ostream& operator << (tm_ostream& out, picture &bb)
{ return out << "picture"; }

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
  { return out __VA_OPT__(<< FOR_EACH_PAIR(MAKE_OUT, LESSLESS, __VA_ARGS__)); }

#define VUE_WIDGET_HEADER(NAME, ...) \
  widget NAME (VUE_WIDGET_HELPER_PARAMS(__VA_ARGS__))

#define VUE_WIDGET(NAME, ...)\
  VUE_WIDGET_DATA(NAME __VA_OPT__(, __VA_ARGS__))\
  string type_vue_##NAME(#NAME);\
  widget NAME (VUE_WIDGET_HELPER_PARAMS(__VA_ARGS__))\
  { return vue_create (type_vue_##NAME,\
           vue_##NAME { VUE_WIDGET_HELPER_INIT(__VA_ARGS__) }); }

//******************************************************************************
// TeXmacs widgets

/******************************************************************************
* Window widgets
******************************************************************************/


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
//VUE_WIDGET(file_chooser_widget, command, cmd, string, type, string, prompt);
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
//VUE_WIDGET(inputs_list_widget, command, call_back, array<string>, prompts);
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
//VUE_WIDGET(input_text_widget, command, call_back, string, type, array<string>, def,
//        int, style, string, width);
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

string debug_style (int style) {
  string buf;
  if (style & WIDGET_STYLE_MINI) buf << "mini ";
  if (style & WIDGET_STYLE_MONOSPACED) buf << "mono ";
  if (style & WIDGET_STYLE_GREY) buf << "grey ";
  if (style & WIDGET_STYLE_PRESSED) buf << "pressed ";
  if (style & WIDGET_STYLE_INERT) buf << "inert ";
  if (style & WIDGET_STYLE_BUTTON) buf << "button ";
  if (style & WIDGET_STYLE_CENTERED) buf << "centered ";
  if (style & WIDGET_STYLE_BOLD) buf << "bold ";
  return buf (0, N (buf)-1);
}

#define SHRINK 3

SI
decode_length (string width, vue_window win, int style) {
  SI ex, ey;
  if (win == NULL) gui_maximal_extents (ex, ey);
  else win->get_size (ex, ey);

  double w_len;
  string w_unit;
  parse_length (width, w_len, w_unit);
  if (w_unit == "w") return (SI) (w_len * ex);
  else if (w_unit == "h") return (SI) (w_len * ey);
  else if (w_unit == "px") return (SI) (w_len * PIXEL);
  // Absolute EM units (temporarily fixed to 14px)
  else if (w_unit == "em") {
    return (SI) (w_len * 14 * PIXEL);
//    font fn= get_default_styled_font (style);
//    return (SI) ((w_len * fn->wquad) / SHRINK);
  }
  else return ex;
}

// additional widgets for caching and drawing
VUE_WIDGET_DATA(picture_widget, picture, p);
VUE_WIDGET_DATA(cached_pull_button, widget, w, promise<widget>, pw, widget, cw);
// data for a button w with a lazy pulldown menu pw and a cached value
VUE_WIDGET_DATA(cached_glue_widget, picture, pic, tree, col, bool, hx, bool, vx, SI, w, SI, h);

VUE_WIDGET_DATA(tabs_widget_star, array<widget>, tabs, array<widget>, icons, array<widget>, bodies, int, current);

VUE_WIDGET_DATA(refreshable_widget_star, object, prom, string, kind, widget, current, object, curobj);

VUE_WIDGET_DATA(refresh_widget_star, string, tmwid, string, kind, widget, current, object, curobj);


vue_ui_rep::vue_ui_rep (string _type, blackbox _data)
  : vue_widget_rep (_type), data (_data)
{
  if (type == "refresh_widget") {
    vue_refresh_widget d= open_box<vue_refresh_widget> (data);
    vue_refresh_widget_star dd { .tmwid= d.tmwid, .kind= d.kind };
    data= close_box (dd);
    return;
  }
  if (type == "refreshable_widget") {
    vue_refreshable_widget d= open_box<vue_refreshable_widget> (data);
    vue_refreshable_widget_star dd { .prom= d.prom, .kind= d.kind };
    data= close_box (dd);
    return;
  }
  if (type == "tabs_widget") {
    vue_tabs_widget d= open_box<vue_tabs_widget> (data);
    vue_tabs_widget_star dd { .tabs= d.tabs, .bodies= d.bodies, .current= 0 };
    data= close_box (dd);
    return;
  }
  if (type == "icon_tabs_widget") {
    vue_icon_tabs_widget d= open_box<vue_icon_tabs_widget> (data);
    array<widget> icons;
    for (int i=0; i< N(d.us); i++) {
      // FIXME: maybe don't use load_xpm
      vue_picture_widget pd { .p= load_xpm (d.us[i]) };
      icons << vue_create<vue_picture_widget> ("picture_widget", pd);
    }
    vue_tabs_widget_star dd { .icons= icons, .tabs= d.ss, .bodies= d.bs, .current= 0 };
    data= close_box (dd);
    return;
  }
  if (type == "pulldown_button") {
    // add more space in the struct for caching the widget
    vue_pulldown_button d= open_box<vue_pulldown_button> (data);
    widget cw;
    vue_cached_pull_button cd { d.w, d.pw, cw };
    data= close_box (cd);
    return;
  }
  if (type == "pullright_button") {
    // add more space in the struct for caching the widget
    vue_pullright_button d= open_box<vue_pullright_button> (data);
    widget cw;
    vue_cached_pull_button cd { d.w, d.pw, cw };
    data= close_box(cd);
    return;
  }
  if (type == "xpm_widget") {
    //VUE_WIDGET(xpm_widget, url, file_name);
    vue_xpm_widget d= open_box<vue_xpm_widget> (data);
    vue_picture_widget dd { .p= load_xpm (d.file_name) };
    data= close_box(dd);
    type= "picture_widget";
    return;
  }
  if (type == "colored_glue_widget") {
    vue_colored_glue_widget d= open_box<vue_colored_glue_widget> (data);
    picture p= native_picture (0,0, 0, 0); // empty cache
    type= "cached_glue_widget";
    data= close_box (vue_cached_glue_widget { .pic=p, .col= d.col, .w= d.w, .h= d.h, .vx= d.vx, .hx= d.hx});
    return;
  }
};

void
vue_ui_rep::send (slot s, blackbox val) {
  if (type == "wrapped_widget") {
    //VUE_WIDGET(wrapped_widget, widget, w, command, quit);
    vue_wrapped_widget d= open_box<vue_wrapped_widget> (data);
    if (s == SLOT_DESTROY) {
      // queue our quit command
      cmd_list= list(d.quit, cmd_list);
    }
    d.w->send (s, val);
    return;
  } else {
    vue_widget_rep::send (s, val);
  }
}

void
layout_pull_button (vue_ui_rep *w) {
  vue_cached_pull_button d= open_box<vue_cached_pull_button> (w->data);
  bool down= w->type == "pulldown_button";
  Clay_ElementId button_id= CLAY_SIDI(CLAY_TM_STRING(w->type), w->id);
  Clay_ElementId float_id=  CLAY_IDI("pull_button_float", w->id);
  Clay_Sizing s= layoutExpand;
  if (down) s= { CLAY_SIZING_FIT(.min=20) };
  CLAY({
    .id= button_id,
    .layout= {
      .padding= CLAY_PADDING_ALL(5),
      .sizing= s },
    .backgroundColor= Clay_Hovered() ?  color_highlight : color_background })
  {
    concrete(d.w)->do_layout ();
    if (!down) {
      CLAY({ .layout= { .sizing= layoutExpand }}){};
      layout_text("<#25B8>", 0, black); // right arrow
    }
    if (Clay_PointerOver (button_id) && (mouse_action == "press-left")) {
      mouse_action= ""; // reset
      if (is_nil (d.cw)) {
        // we clicked an inactive button, we evalutate the promise
        d.cw= d.pw->eval ();
        current_popup= true;
        away_time= 0;
      } else {
        // we clicked an active button, we go back to an inactive state
        d.cw= NULL;
        current_popup= false;
      }
    } else if (current_popup) {
      // some other popup is active, we should be inactive
      d.cw= NULL;
    }
    // if we are active then we draw the float window
    if (!is_nil (d.cw)) {
      CLAY({
        .id= float_id,
        .floating= {
          .attachTo= CLAY_ATTACH_TO_PARENT,
          .attachPoints= {
             .parent= down ? CLAY_ATTACH_POINT_LEFT_BOTTOM
                           : CLAY_ATTACH_POINT_RIGHT_TOP }},
        .layout= {
          .padding= { 8, 8, 8, 8 },
          .sizing= { .width= CLAY_SIZING_FIT(.min= 300) }},
        .backgroundColor= color_background,
        .border= {
          .width= { 1, 1, 1, 1 },
          .color= { 150, 150, 150, 255 }}})
      {
        current_popup= false;
        concrete (d.cw)->do_layout ();
        bool away= false;
        if (!(Clay_PointerOver (float_id) || Clay_PointerOver (button_id))) {
          if (away_time == 0) away_time= texmacs_time ();
          else if (texmacs_time () - away_time > 500) away= true;
        }
        if (cancel_popup || (!current_popup && away)) {
          // we are requested to cancel or
          // we are the last popup of the chain and we are not hovered:
          // then we need to deactivate
          d.cw= NULL;
          current_popup= false; // well, noop, but keep for clarity
        } else {
          // ok, we are the current popup now in this layout cycle
          current_popup= true;
        }
      }
    }
  }
  // store back changes
  w->data= close_box (d);
}

void
layout_menu (unsigned int id, array<widget> a, bool vert) {
  CLAY({
    .id= vert ? CLAY_IDI("vertical_menu", id) : CLAY_IDI("horizontal_menu", id),
    .layout= {
      .layoutDirection= vert ? CLAY_TOP_TO_BOTTOM : CLAY_LEFT_TO_RIGHT,
      .sizing= layoutExpand,
      .childGap= 10 }})
  {
    bool save= button_grow;
    button_grow= vert ? true : false;
    for (int i=0, n=N(a); i< n; i++) {
      concrete (a[i])->do_layout ();
    }
    button_grow= save;
  }
}

void
layout_list (unsigned int id, array<widget> a, bool vert) {
  Clay_Sizing s= layoutFit;
  if (vert) {
    s.width=  CLAY_SIZING_GROW(0);
  } else {
    s.height= CLAY_SIZING_GROW(0);
  }
  CLAY({
     .id= vert ? CLAY_IDI("vertical_list", id) : CLAY_IDI("horizontal_list", id),
     .layout= {
       .layoutDirection= vert ? CLAY_TOP_TO_BOTTOM : CLAY_LEFT_TO_RIGHT,
       .sizing= s }})
  {
    for (int i=0, n=N(a); i< n; i++) {
      concrete (a[i])->do_layout ();
    }
  }
}

class applied_command_rep: public command_rep {
  command cmd;
  object arg;
public:
  applied_command_rep (command _cmd, object _arg): cmd (_cmd), arg (_arg) {}
  void apply () { cmd (arg); }
  void apply (object arg2) { cmd (arg2); }
  tm_ostream& print (tm_ostream& out) {
    return out << "<applied_command " << cmd << " " << arg << ">"; }
};


typedef struct
{
    Clay_Vector2 clickOrigin;
    Clay_Vector2 positionOrigin;
    bool mouseDown;
} ScrollbarData;

ScrollbarData scrollbarData= { {0, 0}, {0, 0}, false };

void
scroll_bar (unsigned int id, Clay_ScrollContainerData &scrollData, Clay_ElementId &my_id, Clay_ElementData &canvas_layout) {
  Clay_ElementId sb_id= CLAY_IDI("ScrollBarV", id);
  CLAY({
    .id= sb_id,
    .floating= {
      .attachTo= CLAY_ATTACH_TO_ELEMENT_WITH_ID,
        .offset= { .y= -(scrollData.scrollPosition->y / scrollData.contentDimensions.height) * scrollData.scrollContainerDimensions.height },
        .zIndex= 1,
        .parentId= my_id.id,
        .attachPoints= {
          .element= CLAY_ATTACH_POINT_RIGHT_TOP,
          .parent=  CLAY_ATTACH_POINT_RIGHT_TOP }}})
  {
    CLAY({
      .id= CLAY_IDI("ScrollBarButtonV", id),
      .layout= {
        .sizing= {
           CLAY_SIZING_FIXED(24),
           CLAY_SIZING_FIXED((scrollData.scrollContainerDimensions.height / scrollData.contentDimensions.height) * scrollData.scrollContainerDimensions.height) }},
      .backgroundColor= Clay_PointerOver (sb_id)
            ? (Clay_Color){100, 100, 140, 150}
            : (Clay_Color){120, 120, 160, 150} ,
      .cornerRadius= CLAY_CORNER_RADIUS(12) }) {}
  }
  //FIXME: mouse handling still not ok
  if (!(mouse_state & 1)) {
      scrollbarData.mouseDown= false;
  }
  if (mouse_action == "press-left" && !scrollbarData.mouseDown && Clay_PointerOver (sb_id)) {
    mouse_action= "";
    scrollbarData.clickOrigin= { (float) mouse_x, (float) mouse_y };
    scrollbarData.positionOrigin= *scrollData.scrollPosition;
    scrollbarData.mouseDown= true;
  } else if (scrollbarData.mouseDown) {
    if (scrollData.contentDimensions.height > 0) {
      Clay_Vector2 ratio= (Clay_Vector2) {
        scrollData.contentDimensions.width / scrollData.scrollContainerDimensions.width,
        scrollData.contentDimensions.height / scrollData.scrollContainerDimensions.height,
      };
      if (scrollData.config.vertical) {
        scrollData.scrollPosition->y= scrollbarData.positionOrigin.y + (scrollbarData.clickOrigin.y - mouse_y) * ratio.y;
        scrollData.scrollPosition->y= min ( max (scrollData.scrollPosition->y, -(max(scrollData.contentDimensions.height - canvas_layout.boundingBox.height, 0.0f))), 0.0f);
      }
      if (scrollData.config.horizontal) {
        scrollData.scrollPosition->x= scrollbarData.positionOrigin.x + (scrollbarData.clickOrigin.x - mouse_x) * ratio.x;
        scrollData.scrollPosition->x= min ( max (scrollData.scrollPosition->x, -(max(scrollData.contentDimensions.width - canvas_layout.boundingBox.width, 0.0f))), 0.0f);
      }
    }
  }

}



void
vue_ui_rep::do_layout () {
  if (type == "horizontal_menu") {
    vue_horizontal_menu d= open_box<vue_horizontal_menu> (data);
    layout_menu (id, d.a, false);
    return;
  }
  if (type == "vertical_menu") {
    vue_vertical_menu d= open_box<vue_vertical_menu> (data);
    layout_menu (id, d.a, true);
    return;
  }
  if (type == "horizontal_list") {
    vue_horizontal_list d= open_box<vue_horizontal_list> (data);
    layout_list (id, d.a, false);
    return;
  }
  if (type == "vertical_list") {
    vue_vertical_list d= open_box<vue_vertical_list> (data);
    layout_list (id, d.a, true);
    return;
  }
  if (type == "division_widget") {
    vue_division_widget d= open_box<vue_division_widget> (data);
    cout << "division_widget, ignoring " << d.name << LF;
    concrete (d.w)->do_layout ();
    return;
  }
  if (type == "aligned_widget") {
    //VUE_WIDGET(aligned_widget, array<widget>, lhs, array<widget>, rhs,
    //            SI, hsep, SI, vsep,
    //            SI, lpad, SI, rpad);
    vue_aligned_widget d= open_box<vue_aligned_widget> (data);
    CLAY({
      .id= CLAY_IDI("aligned_widget", id),
      .layout= {
        .padding= { (uint16_t)(2*d.lpad / PIXEL), (uint16_t)(2*d.rpad / PIXEL), 0, 0 },
        .layoutDirection= CLAY_LEFT_TO_RIGHT,
        .childGap= (uint16_t)(2*d.hsep / PIXEL),
        .sizing= { CLAY_SIZING_FIT(0), CLAY_SIZING_FIT(0) }}})
    {
      //FIXME: size correctly
      CLAY({
        .layout= {
          .layoutDirection= CLAY_TOP_TO_BOTTOM,
          .childGap= (uint16_t)(2*d.vsep / PIXEL),
          .childAlignment= { .x= CLAY_ALIGN_X_RIGHT }}})
      {
        for (int i=0, n= N(d.lhs); i< n; i++) {
          CLAY({
            .layout= { .sizing= { .height= CLAY_SIZING_FIXED(40) }}})
          {
            concrete (d.lhs[i])->do_layout ();
          }
        }
      }
      CLAY({
        .layout= {
          .layoutDirection= CLAY_TOP_TO_BOTTOM,
          .childGap= (uint16_t)(2*d.vsep / PIXEL),
          .childAlignment= { .x= CLAY_ALIGN_X_LEFT }}})
      {
        for (int i=0, n= N(d.lhs); i< n; i++) {
          CLAY({
            .layout= { .sizing= { .height= CLAY_SIZING_FIXED(40) }}})
          {
            concrete (d.rhs[i])->do_layout ();
          }
        }
      }
    }
    return;
  }
  if (type == "aligned_widget_grid") {
    vue_aligned_widget d= open_box<vue_aligned_widget> (data);
    CLAY({
      .id= CLAY_IDI("aligned_widget", id),
      .layout= {
        .layoutDirection= CLAY_LEFT_TO_RIGHT,
        .sizing= layoutFit }})
    {
      //FIXME: size correctly
      GRID(2 /* Two columns */ ) {
        for (int i=0, n= N(d.lhs); i< n; i++) {
          GRID_ELEMENT() {
            CLAY({
              .layout= {
                .childAlignment= { .x= CLAY_ALIGN_X_RIGHT }}})
            {
                concrete (d.lhs[i])->do_layout ();
            }
          }
          GRID_ELEMENT() {
            CLAY({
              .layout= {
                .childAlignment= { .x= CLAY_ALIGN_X_LEFT }}})
            {
                concrete (d.rhs[i])->do_layout ();
            }
          }
        }
      }
    }
    return;
  }
  if (type == "tabs_widget" || type == "icon_tabs_widget") {
    vue_tabs_widget_star d= open_box<vue_tabs_widget_star> (data);
    int next= d.current;
    auto clay_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    CLAY({
      .id= clay_id,
      .layout= {
        .layoutDirection= CLAY_TOP_TO_BOTTOM,
        .sizing= layoutExpand }})
    {
      CLAY({
        .id= CLAY_ID_LOCAL("tabs_widget_tab_bar"),
        .backgroundColor= {100, 100, 100, 255},
        .cornerRadius= { 10, 10, 0, 0, },
        .layout= {
          .padding= { 10, 10, 10, 0 },
          .layoutDirection= CLAY_LEFT_TO_RIGHT,
          .childGap= 20,
          .sizing= { .width=  CLAY_SIZING_GROW(0),
            .height= CLAY_SIZING_FIT(0) } }})
      {
        for (int i= 0, n= N(d.tabs); i< n; i++) {
          CLAY({
            .id= CLAY_IDI_LOCAL("tab", i),
            .backgroundColor= d.current == i ? palette[3] : palette[2],
            .cornerRadius= { 5, 5, 0, 0, },
            .layout= { .padding= { 10, 10, 10, 10 } },
            .border= { .width= { 1, 1, 1, 1 },
              .color= palette[3] }})
          {
            if (N(d.icons) > i) {
              concrete (d.icons[i])->do_layout();
            }
            concrete (d.tabs[i])->do_layout ();
            if (Clay_Hovered () && (mouse_action == "press-left")) {
              mouse_action= "";
              next= i;
            }
          }
        }
      }
      CLAY({
        .id= CLAY_ID_LOCAL("tab_area"),
        .layout= { .sizing= layoutExpand },
        .border= { .width= {1, 1, 1, 1},
          .color= palette[3] }})
      {
        concrete (d.bodies[d.current]) -> do_layout ();
      }
    }
    d.current= next;
    data= close_box(d);
    return;
  }
  if (type == "menu_button") {
    //VUE_WIDGET(menu_button, widget, w, command, cmd, string, pre, string, ks, int, style);
    vue_menu_button d= open_box<vue_menu_button> (data);
    bool inert= (d.style & WIDGET_STYLE_INERT) != 0;
    string st= debug_style (d.style);
    if (N(st)>0 && st != "inert") cout << type << " " << st << LF;
    Clay_ElementId button_id= CLAY_IDI ("menu_button", id);
    Clay_Sizing sz= layoutExpand;
    if (!button_grow) sz= { CLAY_SIZING_FIT(.min=20) };
    CLAY({
      .id= button_id,
      .layout= { .padding= CLAY_PADDING_ALL(5), .sizing= sz  },
      .backgroundColor= !inert && Clay_Hovered() ?  color_highlight : color_background
    }) {
      last_id= button_id;
      concrete(d.w)->do_layout ();
      if (N(d.ks) > 0) {
        // add shortcut
        CLAY({ .layout= { .sizing= layoutExpand }}) {}
        layout_text (d.ks, d.style, inert ? grey : black);
      }
      if (!inert && Clay_Hovered () && (mouse_state & 1)) {
        // close any active popup chain (see pull_widget)
        cancel_popup= true;
        cout << "Click!! " << id << LF;
        cmd_list= list(d.cmd, cmd_list);
      }
    }
    return;
  }
  if (type == "pulldown_button" || type == "pullright_button") {
    layout_pull_button (this);
    return;
  }
  if (type == "text_widget") {
    //VUE_WIDGET(text_widget, string, s, int, style, color, col, bool, tsp);
    vue_text_widget d= open_box<vue_text_widget> (data);
    layout_text (d.s, d.style, d.style & WIDGET_STYLE_INERT ? grey : black);
    if (debug_clay) cout << "text_widget " << id <<  "  [" << d.s << "] last_id: " << last_id.id << LF;
    return;
  }
  if (type == "menu_separator") {
    //VUE_WIDGET(menu_separator, bool, vertical);
    vue_menu_separator d= open_box<vue_menu_separator> (data);
    if (d.vertical) {
      CLAY({
        .id= CLAY_IDI("menu_separator (v)", id),
        .layout= {
          .sizing= { .height= CLAY_SIZING_GROW(0) },
          .padding= {5,5,5,5} },
        .border= {
          .width= { .left= 2 },
          .color=  { 210, 210, 210, 255 } } });
    } else {
      CLAY({
        .id= CLAY_IDI("menu_separator (h)", id),
        .layout= {
          .sizing= { .width= CLAY_SIZING_GROW(0) },
          .padding= {5,5,5,5} },
        .border= {
          .width= { .top= 2 } ,
          .color=  { 210, 210, 210, 255 } } });
    }
    return;
  }
  if (type == "menu_group") {
    //VUE_WIDGET(menu_group, string, name, int, style);
    vue_menu_group d= open_box<vue_menu_group> (data);
    layout_text (d.name, d.style, grey);
    return;
  }
  if (type == "balloon_widget") {
    //VUE_WIDGET(balloon_widget, widget, w, widget, help);
    vue_balloon_widget d= open_box<vue_balloon_widget> (data);
    concrete(d.w)->do_layout ();
    Clay_ElementId target_id= CLAY_SIDI(CLAY_TM_STRING(concrete (d.w)->type), concrete (d.w)->id);
    if (Clay_PointerOver (target_id)) {
      if (current_balloon != id) {
        // hovered and not active, then become active and start counting time
        current_balloon= id;
        balloon_time= texmacs_time ();
      }
      time_t elapsed= texmacs_time () - balloon_time;
      if ((elapsed > 1000) && (elapsed < 5000)) {
        // show the balloon
        CLAY({
          .backgroundColor= { 240, 240, 0, 255 },
          .layout= { .padding= { 10, 10, 10, 10 } },
          .border= {
            .width= { 2, 2, 2, 2 },
            .color= { 200, 200, 0, 255 }},
          .floating= {
            .zIndex= 10,
            .offset= { (float)mouse_x + 30, (float)mouse_y + 30 },
            .attachTo= CLAY_ATTACH_TO_ROOT,
            .attachPoints= {
              .parent= CLAY_ATTACH_POINT_LEFT_TOP }}})
        {
          concrete(d.help)->do_layout ();
        }
      }
    } else {
      // not hovered, reset if we were active
      if (current_balloon == id) {
        current_balloon= 0;
        balloon_time= 0;
      }
    }
    return;
  }
  if (type == "picture_widget") {
    //VUE_WIDGET(xpm_widget, url, file_name);
    vue_picture_widget d= open_box<vue_picture_widget> (data);
    SI w= d.p->get_width ();
    SI h= d.p->get_height ();
    CLAY({
      .backgroundColor= color_background,
      .layout= {
        .sizing= { CLAY_SIZING_FIXED( (float)w), CLAY_SIZING_FIXED( (float)h) } },
        .custom= { .customData=  vue_render_widget },
      .userData= this }) {};
    return;
  }
  if (type == "glue_widget") {
    //VUE_WIDGET(glue_widget, bool, hx, bool, vx, SI, w, SI, h);
    vue_glue_widget d= open_box<vue_glue_widget> (data);
    CLAY({
      .id= CLAY_IDI("glue_widget", id),
      .layout= {
        .sizing= {
          .width=  d.hx ? CLAY_SIZING_GROW( .min= (float)d.w/PIXEL)
                        : CLAY_SIZING_FIXED((float)d.w/PIXEL),
          .height= d.vx ? CLAY_SIZING_GROW( .min= (float)2*d.h/PIXEL)
                        : CLAY_SIZING_FIXED((float)2*d.h/PIXEL) }}}) {};
    return;
  }
  if (type == "cached_glue_widget") {
    //VUE_WIDGET(colored_glue_widget, tree, col, bool, hx, bool, vx, SI, w, SI, h);
    vue_cached_glue_widget d= open_box<vue_cached_glue_widget> (data);
    CLAY({
      //.id= CLAY_IDI("colored_glue_widget", id),
      .custom= { .customData=  vue_render_widget },
      .userData= this,
      .layout= {
        .sizing= {
          .width= d.hx  ? CLAY_SIZING_GROW( .min= (float)2*d.w/PIXEL)
                        : CLAY_SIZING_FIT( .min= (float)2*d.w/PIXEL),
          .height= d.vx ? CLAY_SIZING_GROW( .min= (float)2*d.h/PIXEL)
                        : CLAY_SIZING_FIT( .min= (float)2*d.h/PIXEL) }}}) {};
    return;
  }
  if (type == "tile_menu") {
    //VUE_WIDGET(tile_menu, array<widget>, a, int, cols);
    // a menu rendered as a table of cols columns wide & made up of widgets in a
    vue_tile_menu d= open_box<vue_tile_menu> (data);
    int c=0, n= N(d.a);
    CLAY({ .layout= {
      .layoutDirection= CLAY_TOP_TO_BOTTOM,
      .childGap= 5 }})
    {
      while (c < n) {
        CLAY({ .layout= {
          .layoutDirection= CLAY_LEFT_TO_RIGHT,
          .childGap= 5 }})
        {
          for (int i=0; i< d.cols; i++) {
            if (c == n) break;
            concrete (d.a[c])-> do_layout ();
            c++;
          }
        }
      }
    }
    return;
  }
  if (type == "toggle_widget") {
    // VUE_WIDGET(toggle_widget, command, cmd, bool, on, int, style);
    vue_toggle_widget d= open_box<vue_toggle_widget> (data);
    string st= debug_style (d.style);
    if (N(st)>0) cout << type << " " << st << LF;
    bool x= d.style & WIDGET_STYLE_INERT;
    CLAY({
      .layout= {
        .sizing= { CLAY_SIZING_FIT(40),
                   CLAY_SIZING_FIT(40) }}})
    {
      if (d.on) {
        layout_text ("[X]", d.style, black);
      } else {
        layout_text ("[ ]", d.style, black);
      }
      if (Clay_Hovered() && (mouse_action == "press-left")) {
        mouse_action= "";
        cout << "Click toggle! [" << (d.on ? "X" : " ") << "]" << LF;
        d.on= !d.on;
        data= close_box (d);
        command c (tm_new<applied_command_rep> (d.cmd, list_object (object (d.on))));
        cmd_list= list (c, cmd_list);
      }
    }
    return;
  }
  if (type == "enum_widget") {
    //VUE_WIDGET(enum_widget, command, cb, array<string>, vals, string, val, int, st, string, w);
    //FIXME: implement
    vue_enum_widget d= open_box<vue_enum_widget> (data);
    SI w= decode_length (d.w, current_window, d.st);
    CLAY({
      .id= CLAY_SIDI(CLAY_TM_STRING(type), id),
      .layout= {
        .sizing= {
          CLAY_SIZING_FIXED ((float) 2*w/PIXEL),
          CLAY_SIZING_FIT (0) }}})
    {
      layout_text (d.vals [d.st], 0, black);
      if (Clay_Hovered () && (mouse_action == "press-left")) {
        mouse_action= "";
        //FIXME: implement
        cout << "Clicked enum_widget!" << LF;
      }
    }
    return;
  }
  if (type == "resize_widget") {
    //VUE_WIDGET(resize_widget, widget, w, int, style, string, w1, string, h1,
    //string, w2, string, h2, string, w3, string, h3,
    //string, hpos, string, vpos);
    //FIXME: implement
    vue_resize_widget d= open_box<vue_resize_widget> (data);
    string st= debug_style (d.style);
    if (N(st)>0) cout << type << " " << st << LF;
    SI minw, minh, defw, defh, maxw, maxh;
    minw= decode_length (d.w1, current_window, d.style);
    minh= decode_length (d.h1, current_window, d.style);
    defw= decode_length (d.w2, current_window, d.style);
    defh= decode_length (d.h2, current_window, d.style);
    maxw= decode_length (d.w3, current_window, d.style);
    maxh= decode_length (d.h3, current_window, d.style);
    Clay_Sizing sizing= layoutFit;
    if (defw == maxw && defw == minw) {
      sizing.width= CLAY_SIZING_FIXED((float) 2*defw/PIXEL);
    } else {
      sizing.width= CLAY_SIZING_FIT(.min= (float) 2*minw/PIXEL, .max=(float) 2*maxw/PIXEL );
    }
    if (defh == maxh && defh == minh) {
      sizing.height= CLAY_SIZING_FIXED((float) 2*defh/PIXEL);
    } else {
      sizing.height= CLAY_SIZING_FIT(.min= (float) 2*minh/PIXEL, .max=(float) 2*maxh/PIXEL );
    }
    CLAY({
      .id= CLAY_SIDI(CLAY_TM_STRING(type), id),
      .layout= { .sizing= sizing }})
    {
      concrete(d.w)->do_layout ();
    }
    return;
  }
  if (type == "refreshable_widget") {
    //VUE_WIDGET(refreshable_widget, object, prom, string, kind);
    vue_refreshable_widget_star d= open_box<vue_refreshable_widget_star> (data);
    if (is_nil (d.current) ||
        current_window->refresh_kinds->contains ("any") ||
        current_window->refresh_kinds->contains (d.kind) ) {
      // (re)initialize the widget
      eval ("(lazy-initialize-force)");
      object xwid= call (d.prom);
      if (d.curobj != xwid)  {
        // cache does not match
        if (is_widget (xwid)) {
          d.curobj= xwid;
          d.current= as_widget (xwid);
        } else {
          d.current= glue_widget();
        }
      }
      data= close_box (d);
    }
    CLAY({
      .id= CLAY_SIDI (CLAY_TM_STRING (type), id),
      .layout= { .sizing= layoutFit }})
    {
      if (!is_nil (d.current)) {
        concrete (d.current)->do_layout ();
      }
    }
    return;
  }
  if (type == "refresh_widget") {
    //VUE_WIDGET(refreshable_widget, object, prom, string, kind);
    vue_refresh_widget_star d= open_box<vue_refresh_widget_star> (data);
    if (is_nil (d.current) ||
        current_window->refresh_kinds->contains ("any") ||
        current_window->refresh_kinds->contains (d.kind) ) {
      // (re)initialize the widget
      string s = "'(vertical (link " * d.tmwid * "))";
      eval ("(lazy-initialize-force)");
      object xwid = call ("menu-expand", eval (s));
      static hashmap<object, widget> cache;
      if (cache->contains (xwid)) {
        if (d.curobj == xwid) return false;
        d.curobj = xwid;
        d.current= cache [xwid];
      } else {
        d.curobj = xwid;
        object uwid = eval (s);
        d.current = make_menu_widget (uwid);
        //tmwid->add_child (cur); // FIXME?! Is this ok? what when we refresh?
        if (menu_caching) cache (xwid) = d.current;
      }
      data= close_box (d);
    }
    CLAY({
      .id= CLAY_SIDI (CLAY_TM_STRING (type), id),
      .layout= { .sizing= layoutFit }})
    {
      if (!is_nil (d.current)) {
        concrete (d.current)->do_layout ();
      }
    }
    return;
  }
  if (type == "wrapped_widget") {
    //VUE_WIDGET(wrapped_widget, widget, w, command, quit);
    vue_wrapped_widget d= open_box<vue_wrapped_widget> (data);
    // we just behave as our content
    concrete (d.w)->do_layout ();
    return;
  }
  if (type == "user_canvas_widget") {
    //VUE_WIDGET(user_canvas_widget, widget, wid, int, style);
    vue_user_canvas_widget d= open_box<vue_user_canvas_widget> (data);
    Clay_ElementId my_id= CLAY_SIDI (CLAY_TM_STRING(type), id);
    CLAY({
      .id= my_id,
      .layout= { .sizing= layoutExpand },
      .border= {
        .width= { 2, 2, 2, 2 },
        .color= palette[3] },
      .clip= {
        .horizontal= true, .vertical= true,
        .childOffset= Clay_GetScrollOffset () }})
    {
      concrete (d.wid)->do_layout ();
    }
    Clay_ScrollContainerData scrollData= Clay_GetScrollContainerData (my_id);
    Clay_ElementData canvas_layout= Clay_GetElementData (my_id);
    if (scrollData.found && canvas_layout.found) {
      scroll_bar (id, scrollData, my_id, canvas_layout);
    }
    return;
  }
  if (type == "hsplit_widget") {
    //VUE_WIDGET(hsplit_widget, widget, l, widget, r);
    vue_hsplit_widget d= open_box<vue_hsplit_widget> (data);
    CLAY({
      .id= CLAY_SIDI(CLAY_TM_STRING(type), id),
      .layout= {
        .layoutDirection= CLAY_LEFT_TO_RIGHT }})
    {
      concrete (d.l)->do_layout ();
      CLAY({
        .id= CLAY_IDI("splitter", id),
        .backgroundColor= palette [3],
        .layout= {
          .sizing= {
            .width=  CLAY_SIZING_FIXED(40),
            .height= CLAY_SIZING_GROW(0) }}}) {};
      concrete (d.r)->do_layout ();
    }
    return;
  }
  if (type == "vsplit_widget") {
    //VUE_WIDGET(hsplit_widget, widget, l, widget, r);
    vue_vsplit_widget d= open_box<vue_vsplit_widget> (data);
    CLAY({
      .id= CLAY_SIDI(CLAY_TM_STRING(type), id),
      .layout= {
        .layoutDirection= CLAY_TOP_TO_BOTTOM }})
    {
      concrete (d.t)->do_layout ();
      CLAY({
        .id= CLAY_IDI("splitter", id),
        .backgroundColor= palette [3],
        .layout= {
          .sizing= {
            .width=  CLAY_SIZING_GROW(0),
            .height= CLAY_SIZING_FIXED(40)}}}) {};
      concrete (d.b)->do_layout ();
    }
    return;
  }
  if (type == "choice_widget") {
    //VUE_WIDGET(choice_widget, command, cb, array<string>, vals, array<string>, chosen, bool, flag);
    vue_choice_widget d= open_box<vue_choice_widget> (data);
    CLAY({
      .id= CLAY_SIDI(CLAY_TM_STRING(type), id),
      .layout= {
        .layoutDirection=  CLAY_TOP_TO_BOTTOM,
        .sizing= layoutFit,
        .childGap= 10 }
    }) {
      for (int i=0; i<N(d.vals); i++) {
        bool active= false;
        for (int j=0; j<N(d.chosen); j++)
          if (d.chosen[j] == d.vals[i]) { active= true; break; }
        Clay_Color bg= color_background;
        if (active) bg= (Clay_Color){ 100, 100, 255, 255 };
        CLAY({ .backgroundColor= bg }) {
          layout_text (d.vals [i], 0, black);
        }
      }
    }
    return;
  }
  cout << "WARNING: Need do_layout for widget " << type << LF;
}

void
vue_widget_rep::render (void *data) {
  // empty
  cout << "WARNING: Called empty vue_widget_rep::render" << LF;
}

picture
print_glue (int w, int h, tree col)
{
  picture pic= native_picture (w, h, 0, 0);
  renderer ren= picture_renderer (pic, std_shrinkf * retina_factor);
  ren->set_shrinking_factor (1);
  rectangle r= rectangle (0, 0, pic->get_width(), pic->get_height());
  ren->set_origin (0,0);
  ren->encode (r->x1, r->y1);
  ren->encode (r->x2, r->y2);
  ren->set_clipping (r->x1, r->y2, r->x2, r->y1);
  if (col == "") {
    // do nothing
  } else {
    if (is_atomic (col)) {
      color c= named_color (col->label);
      ren->set_background (c);
      ren->set_pencil (c);
      ren->fill (r->x1, r->y2, r->x2, r->y1);
    } else {
      ren->set_shrinking_factor (std_shrinkf);
      ren->set_background (col);
      ren->clear_pattern (5*r->x1, 5*r->y2, 5*r->x2, 5*r->y1);
    }
  }
  return pic;
}

void
vue_ui_rep::render (void *render_data) {
  if (type == "picture_widget") {
    vue_picture_widget d= open_box<vue_picture_widget> (data);
    current_window->draw_picture (render_data, d.p);
    return;
  }
  if (type == "cached_glue_widget") {
    vue_cached_glue_widget d= open_box<vue_cached_glue_widget> (data);
    int pw= d.pic->get_width ();
    int ph= d.pic->get_height ();
    int nw= pw, nh= ph;
    current_window->get_viewport_size (render_data, nw, nh);
    if ((nw != pw) || (nh != ph)) {
      // the size of the widget has changed, regenerate the picture
      d.pic= print_glue (nw, nh, d.col);
      data= close_box (d);
    }
    current_window->draw_picture (render_data, d.pic);
    return;
  }
  cout << "WARNING: Empty rendering of widget of type " << type << LF;
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
* input_text_widget
******************************************************************************/

//VUE_WIDGET(input_text_widget, command, call_back, string, type, array<string>, def,
//        int, style, string, width);
  // a textual input widget for input of a given type and a list of suggested
  // default inputs (the first one should be displayed, if there is one)
  // an optional width may be specified for the input field
  // the width is specified in TeXmacs length format with units em, px or w


class vue_input_text_widget_rep : public vue_widget_rep {
public:
  string  s;           // the string being entered
  string  draw_s;      // the string being displayed
  SI      text_h;      // text height
  string  type;        // expected type of string
  string  name;        // optional name of the input field
  string  serial;      // optional serial number of the input field
  array<string> def;   // default possible input values
  command call_back;   // routine called on <return> or <escape>
  int     style;       // style of widget
  bool    greyed;      // greyed input
  string  width;       // width of input field
  bool    persistent;  // don't complete after loss of focus
  bool    ok;          // input not canceled
  bool    done;        // call back has been called
  int     def_cur;     // current choice between default possible values
  SI      dw, dh;      // border width and height
  int     pos;         // cursor position
  SI      scroll;      // how much scrolled to the left
  bool    got_focus;   // got keyboard focus
  bool    hilit;       // hilit on keyboard focus
  array<string> tabs;  // tab completions
  int     tab_nr;      // currently visible tab-completion
  int     tab_pos;     // cursor position where tab was pressed

  string buffer; // cache
  
  vue_input_text_widget_rep (command _call_back, string _type, array<string> _def,
                             int _style, string _width);
  void do_layout ();
//  void render (void *data);
  bool process_key (string);
};

vue_input_text_widget_rep::vue_input_text_widget_rep (command _call_back,
          string _type, array<string> _def, int _style, string _width)
  : call_back (_call_back), type (_type),
    def (_def), style (_style), width (_width),
    greyed ((style & WIDGET_STYLE_INERT) != 0),
    pos (0),
    vue_widget_rep ("input_text_widget")
{
  if (N(def) > 0) {
    s= copy (def[0]);
  }
}

#ifdef OS_WIN32
#define URL_CONCATER  '\\'
#else
#define URL_CONCATER  '/'
#endif

bool
vue_input_text_widget_rep::process_key (string key) {
  bool continuous=
  starts (type, "search") ||
  starts (type, "replace-") ||
  starts (type, "spell") ||
  starts (serial, "form-");
  
  while ((N(key) >= 5) && (key(0,3) == "Mod") && (key[4] == '-') &&
         (key[3] >= '1') && (key[3] <= '5')) key= key (5, N(key));
  if (key == "space") key= " ";
  if (key == "<") key= "<less>";
  if (key == ">") key= "<gtr>";
  
  /* tab-completion */
  if (continuous);
  else if ((key == "tab" || key == "S-tab") && N(tabs) != 0) {
    int d=  (key == "tab"? 1: N(tabs)-1);
    tab_nr= (tab_nr + d) % N(tabs);
    s=      s (0, tab_pos) * tabs[tab_nr];
    pos=    N(s);
    return true;
  }
  else if (key == "tab" || key == "S-tab") {
    if (pos != N(s)) return;
    tabs= copy (def);
    if (ends (type, "file") || type == "directory") {
      url search= url_here ();
      url dir= (ends (s, string (URL_CONCATER))? url (s): head (url (s)));
      if (type == "smart-file") search= url ("$TEXMACS_FILE_PATH");
      if (is_rooted (dir)) search= url_here ();
      if (is_none (dir)) dir= url_here ();
      tabs= file_completions (search, dir);
    }
    tabs= strip_completions (tabs, s);
    tabs= close_completions (tabs);
    if (N (tabs) == 0);
    else if (N (tabs) == 1) {
      s=    s * tabs[0];
      pos=  N(s);
      tabs= array<string> (0);
    }
    else {
      tab_nr=  0;
      tab_pos= N(s);
      s=       s * tabs[0];
      pos=     N(s);
      beep ();
    }
    return true;
  }
  else {
    tabs=    array<string> (0);
    tab_nr=  0;
    tab_pos= 0;
  }
  
  /* other actions */
  if (continuous &&
      (key == "return" || key == "S-return" ||
       key == "home"   || key == "end" ||
       key == "up"     || key == "down" ||
       key == "pageup" || key == "pagedown" ||
       key == "tab"    || key == "S-tab" ||
       key == "escape" ||
       (starts (type, "spell") && string ("1") <= key && key <= string ("9")) ||
       (starts (type, "spell") && key == "+")));
  else if (key == "return") {
    // commit
    if (!continuous) {
      ok= true;
      done= true;
      command cmd= tm_new<applied_command_rep>(call_back,
                    list_object (object (s)));
      cmd_list= list(cmd, cmd_list);
      return true;
    }
  }
  else if ((key == "escape") || (key == "C-c") ||
           (key == "C-g")) {
    // cancel
    ok= false;
    done= true;
    call_back (list_object (object (false)));
    command cmd= tm_new<applied_command_rep>(call_back,
                  list_object (object (false)));
    cmd_list= list(cmd, cmd_list);
    return true;
  }
  else if ((key == "left") || (key == "C-b")) {
    if (pos>0) tm_char_backwards (s, pos); }
  else if ((key == "right") || (key == "C-f")) {
    if (pos<N(s)) tm_char_forwards (s, pos); }
  else if ((key == "home") || (key == "C-a")) pos=0;
  else if ((key == "end") || (key == "C-e")) pos=N(s);
  else if ((key == "up") || (key == "C-p")) {
    if (N(def) > 0) {
      def_cur= (def_cur+1) % N(def);
      s=       copy (def[def_cur]);
      pos=     N(s);
    }
  }
  else if ((key == "down") || (key == "C-n")) {
    if (N(def) > 0) {
      def_cur= (def_cur+N(def)-1) % N(def);
      s=       copy (def[def_cur]);
      pos=     N(s);
    }
  }
  else if (key == "C-k") s= s (0, pos);
  else if ((key == "C-d") || (key == "delete")) {
    if ((pos<N(s)) && (N(s)>0)) {
      int end= pos;
      tm_char_forwards (s, end);
      s= s (0, pos) * s (end, N(s));
    }
  }
  else if (key == "backspace" || key == "S-backspace") {
    if (pos>0) {
      int end= pos;
      tm_char_backwards (s, pos);
      s= s (0, pos) * s (end, N(s));
    }
  }
  else if (key == "C-backspace") {
    s= "";
    pos= 0;
  }
  else {
    if (starts (key, "<#"));
    else if (key == "<less>" || key == "<gtr>");
    else {
      if (N(key)!=1) return false;
      int i (key[0]);
      if ((i>=0) && (i<32)) return false;
    }
    s= s (0, pos) * key * s(pos, N(s));
    pos += N(key);
  }
  if (continuous) {
    command cmd= tm_new<applied_command_rep>(call_back,
                      list_object (list_object (object (s), object (key))));
    cmd_list= list(cmd, cmd_list);
  }
  return true;
}

void
vue_input_text_widget_rep::do_layout () {
  bool is_focused= current_window->kbd_focus == this;
  SI w= decode_length (width, current_window, style);
  Clay_Color bg;
  if (is_focused) {
    buffer= copy ( s (0, pos) * "<#007c>" * s(pos, N(s)));
    bg=  { 228, 228, 220, 255 };
  }
  else {
    buffer= copy (s);
    bg= { 208, 208, 210, 255 };
  }
  CLAY({
    .backgroundColor= bg,
    .layout= {
      .sizing= {
        .width=  CLAY_SIZING_FIXED((float) 2*w/PIXEL),
        .height= CLAY_SIZING_FIT() },
      .padding= { 8, 8, 4, 4 } }})
  {
    layout_text (buffer, 0, black);
    if ((N(key_event) > 0) && (is_focused)) {
      //FIXME: handle focus correctly!!
      process_key (key_event);
      key_event= "";
    }
    if (Clay_Hovered () && (mouse_action == "press-left")) {
      mouse_action= "";
      current_window->kbd_focus= this;
    }
  }
}

widget
input_text_widget (command call_back, string type, array<string> def,
                          int style, string width) {
  return abstract (tm_new<vue_input_text_widget_rep> (call_back, type,
                                                      def, style, width));
}

/******************************************************************************
* plain windows
******************************************************************************/

string type_vue_plain_window_widget ("vue_plain_window_widget");

class vue_plain_window_widget_rep : public vue_widget_rep {
public:
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
  bool post_layout ();
}; // class vue_plain_window_widget_rep

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
        bool flag= check_open<bool> (val, s);
        // true= get grab, false= release grab
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
        bool flag= check_open<bool> (val, s);
        if (win) {
          win->set_modified (flag);
        }
      }
      break;
    case SLOT_REFRESH:
      {
        string kind= check_open<string> (val, s);
        win->next_refresh_kinds << kind;
      }
      break;
    case SLOT_DESTROY:
    {
      ASSERT (is_nil (val), "type mismatch");
      if (!is_nil (quit)) cmd_list= list (quit, cmd_list);
      //wid->send (s, val); // forward to the content (seems unnecessary)
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
      SI x, y;
      check_type_id<coord2> (type_id, s);
      if (win) win->get_position (x, y);
      return close_box<coord2> (coord2 (x, y));
    }
    case SLOT_SIZE:
    {
      SI w, h;
      check_type_id<coord2> (type_id, s);
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
  CLAY({
    .id= CLAY_ID("plain_window_widget"),
    .backgroundColor= color_background,
    .layout= {
      .layoutDirection= CLAY_TOP_TO_BOTTOM,
      .sizing= layoutFit }})
  {
     concrete (wid)->do_layout ();
  }
}

bool
vue_plain_window_widget_rep::post_layout () {
  Clay_ElementData el= Clay_GetElementData (CLAY_ID("plain_window_widget"));
  SI w,h;
  win->get_size (w, h);
  SI cw= el.boundingBox.width * PIXEL / 2,
     ch= el.boundingBox.height * PIXEL / 2;
  if (false && !win->clay_debug && ((w != cw) || (h != ch))) {
    //cout << w << "," << h << " " << cw << "," << ch << LF;
    win->set_size (cw, ch);
    return true;
  }
  return false; // do not relayout
}

//******************************************************************************
// vue_texmacs_widget

//VUE_WIDGET(texmacs_widget, int, mask, command, quit);

class vue_texmacs_widget_rep : public vue_widget_rep {
  int mask;
  command quit;
  vue_widget main_widget;
  string left_footer, right_footer;
  vue_window win; // weak ref
  
  bool visibility [10];
  
  vue_widget main_menu;
  vue_widget main_icons;
  vue_widget mode_icons;
  vue_widget focus_icons;
  vue_widget user_icons;
  vue_widget side_tools;
  vue_widget left_tools;
  vue_widget bottom_tools;
  vue_widget extra_tools;
  
  vue_widget interactive_prompt;
  vue_widget interactive_input;

public:
  vue_texmacs_widget_rep (int _mask, command _quit);
  
  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
  void notify (slot s, blackbox new_val);
  
  void do_layout ();
}; // class vue_plain_window_widget_rep

widget texmacs_widget (int mask, command quit) {
  return abstract (tm_new<vue_texmacs_widget_rep> (mask, quit));
}
  
vue_texmacs_widget_rep::vue_texmacs_widget_rep (int _mask, command _quit)
  : mask (_mask), quit (_quit), win (NULL), vue_widget_rep ("vue_texmacs_widget_rep")
{
  // decode mask
  visibility[0]= (mask & 1)   == 1;   // header
  visibility[1]= (mask & 2)   == 2;   // main
  visibility[2]= (mask & 4)   == 4;   // mode
  visibility[3]= (mask & 8)   == 8;   // focus
  visibility[4]= (mask & 16)  == 16;  // user
  visibility[5]= (mask & 32)  == 32;  // footer
  visibility[6]= (mask & 64)  == 64;  // right side tools
  visibility[7]= (mask & 128) == 128; // left side tools
  visibility[8]= (mask & 256) == 256; // bottom tools
  visibility[9]= (mask & 512) == 512; // extra bottom tools
  
  left_footer= translate ("Welcome to TeXmacs");
  right_footer= translate ("Booting");
};


void
vue_texmacs_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_texmacs_widget_rep::send " << slot_name (s) << LF;
  
  switch (s) {
    case SLOT_INVALIDATE:
    case SLOT_INVALIDATE_ALL:
    case SLOT_EXTENTS:
    case SLOT_SCROLL_POSITION:
    case SLOT_ZOOM_FACTOR:
    case SLOT_MOUSE_GRAB:
      main_widget->send(s, val);
      return;
      
    case SLOT_LEFT_FOOTER:
      left_footer= check_open<string> (val, s);
      break;
      
    case SLOT_RIGHT_FOOTER:
      right_footer= check_open<string> (val, s);
      break;
      
    case SLOT_SCROLLBARS_VISIBILITY:
        // ignore this: qt handles scrollbars independently
        //                send_int (THIS, "scrollbars", val);
      break;
      
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
      {
        int index= ((s - SLOT_HEADER_VISIBILITY) >>1 ) % 10;
        visibility [index]= check_open<bool> (val, s);
        // update_visibility();
      }
      break;
      
    case SLOT_DESTROY:
      ASSERT (is_nil (val), "type mismatch");
      if (!is_nil (quit)) quit ();
 //     the_gui->need_update ();
      break;
      
    case SLOT_MODIFIED:
      if (win) win->content->send (s, val);
//      cout << "MODIFIED!" << LF;
      break;
      
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
  switch (s) {
    case SLOT_SCROLLABLE:
      check_type_void (index, s);
      main_widget= concrete (w);
      if (win) win->kbd_focus= main_widget;
      break;
      
    case SLOT_MAIN_MENU:
      check_type_void (index, s);
      main_menu= concrete (w);
      break;

    case SLOT_MAIN_ICONS:
      check_type_void (index, s);
      main_icons= concrete (w);
      break;

    case SLOT_MODE_ICONS:
      check_type_void (index, s);
      mode_icons= concrete (w);
      break;
 
    case SLOT_FOCUS_ICONS:
      check_type_void (index, s);
      focus_icons= concrete (w);
      break;
 
    case SLOT_USER_ICONS:
      check_type_void (index, s);
      user_icons= concrete (w);
      break;
 
    case SLOT_SIDE_TOOLS:
      check_type_void (index, s);
      side_tools= concrete (w);
      break;
 
    case SLOT_LEFT_TOOLS:
      check_type_void (index, s);
      left_tools= concrete (w);
      break;
 
    case SLOT_BOTTOM_TOOLS:
      check_type_void (index, s);
      bottom_tools= concrete (w);
      break;
 
    case SLOT_EXTRA_TOOLS:
      check_type_void (index, s);
      extra_tools= concrete (w);
      break;
 
    case SLOT_INTERACTIVE_PROMPT:
      check_type_void (index, s);
      interactive_prompt= concrete (w);
      break;
 
    case SLOT_INTERACTIVE_INPUT:
      check_type_void (index, s);
      interactive_input= concrete (w);
      break;

    default:
      cout << "vue_texmacs_widget_rep::write(), unhandled " << slot_name (s)
           << " for widget of type: " << type << LF;
      break;
  }
}

blackbox
vue_texmacs_widget_rep::query (slot s, int type_id) {
    // Some slots are too noisy
  if (DEBUG_VUE_WIDGETS && (s != SLOT_IDENTIFIER))
    debug_widgets << "vue_texmacs_widget_rep: queried " << slot_name(s)
                  << "\t\tto widget\t" << type << LF;
  
  switch (s) {
    case SLOT_SCROLL_POSITION:
    case SLOT_EXTENTS:
    case SLOT_VISIBLE_PART:
      return main_widget->query (s, type_id);
      
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
      {
        int index= ((s - SLOT_HEADER_VISIBILITY) >>1 ) % 10;
        check_type_id<bool> (type_id, s);
        return close_box<bool> (visibility [index]);
      }
      break;
      
    default:
      return vue_widget_rep::query(s, type_id);
  }
}


void vue_texmacs_widget_rep::do_layout () {
  win= current_window; // save the info
  // grow to the size of the window
  SI w= 300, h= 300;
  if (win) win->get_size (w, h);
  if (win->kbd_focus == NULL) {
    win->kbd_focus= main_widget;
  }
  CLAY({
    .id= CLAY_IDI("texmacs_widget", id),
    .backgroundColor= color_background,
    .layout= {
      .layoutDirection= CLAY_TOP_TO_BOTTOM,
      .sizing= {
        .width=  CLAY_SIZING_FIT ((float) 2*w/PIXEL),
        .height= CLAY_SIZING_FIT ((float) 2*h/PIXEL)  },
      .padding= { 0, 0, 16, 16 },
      .childGap= 16  }})
  {
    CLAY({
      .id= CLAY_ID_LOCAL("MainMenuBar"),
      .layout= {
        .padding= { 8, 8, 0, 0 },
        .sizing= {
          .width=  CLAY_SIZING_GROW(0),
          .height= CLAY_SIZING_FIT(.min= 20) }}})
    {
      if (!is_nil (main_menu)) {
        main_menu->do_layout ();
      }
    }
    CLAY({
      .id= CLAY_ID_LOCAL("MainToolbar"),
      .layout= {
         .padding= { 8, 8, 0, 0 },
         .sizing= {
           .width=  CLAY_SIZING_GROW(0),
           .height= CLAY_SIZING_FIT(.min= 20) }}})
    {
      if (!is_nil (main_icons)) {
        main_icons->do_layout ();
      }
    }
    CLAY({
      .id= CLAY_ID_LOCAL("ModeToolbar"),
      .layout= {
         .padding= { 8, 8, 0, 0 },
         .sizing= {
           .width=  CLAY_SIZING_GROW(0),
           .height= CLAY_SIZING_FIT(.min= 20) }}})
    {
      if (!is_nil (mode_icons)) {
        mode_icons->do_layout ();
      }
    }
    CLAY({
      .id= CLAY_ID_LOCAL("FocusToolbar"),
      .layout= {
         .padding= { 8, 8, 0, 0 },
         .sizing= {
            .width=  CLAY_SIZING_GROW(0),
            .height= CLAY_SIZING_FIT(.min= 20) }}})
    {
      if (!is_nil (focus_icons)) {
        focus_icons->do_layout ();
      }
    }
    if (!is_nil (main_widget)) main_widget->do_layout ();
    CLAY({
      .id= CLAY_ID_LOCAL("Footer"),
      .layout= {
        .padding= { 8, 8, 0, 0 },
        .sizing= {
          .width=  CLAY_SIZING_GROW(0),
          .height= CLAY_SIZING_FIXED(40) }}})
    {
      layout_text (left_footer, 0, black);
      CLAY({
        .layout= {
           .sizing= {
             .width=  CLAY_SIZING_GROW(0),
             .height= CLAY_SIZING_FIXED(0) }} }) {} // spacer
      layout_text (left_footer, 0, black);

    }
  }
}


//*****************************************************************************
// vue_simple_widget

// Besides the widget constructors, any GUI implementation should also provide
// a simple_widget_rep class with the following virtual methods:
//
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

list<vue_simple_widget_rep*> paint_list;

vue_simple_widget_rep::vue_simple_widget_rep ()
: vue_widget_rep (vue_type_simple_widget),
  win (NULL), ren (NULL),
  size (coord2 (0, 0)),
  extents (0,0,0,0),
  mouse_cursor (coord2 (0, 0)),
  backing_pos (coord2(0, 0)),
  scroll_pos (coord2 (0, 0)),
  scroll_momentum (coord2 (0, 0)),
  absolute_scroll (false),
  backing_valid (false)
{
  // note that size is set to an arbitrary value to init the backing_store
  // create a backing store and the renderer
  backing_store= native_picture (size.x1, size.x2, 0, 0);
  ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
  paint_list= list<vue_simple_widget_rep*>(this, paint_list);
};

vue_simple_widget_rep::~vue_simple_widget_rep () {
  paint_list= remove (paint_list, this);
}

// Message handling
// ----------------
// When implementing simple_widget we have to respond to certain messages (via
// send, query, read, write, notify). They are detailed in `message.hpp` but
// more concretely one can check tm_frame.cpp to see an intermediate interface
// that the editor is using, e.g. to manage the properties of the editor's UI
// 
// /* canvas */
// void set_scrollbars (int sb);
// void get_visible (SI& x1, SI& y1, SI& x2, SI& y2);
// void scroll_where (SI& x, SI& y);
// void scroll_to (SI x, SI y);
// void set_extents (SI x1, SI y1, SI x2, SI y2);
// void get_extents (SI& x1, SI& y1, SI& x2, SI& y2);
// void full_screen_mode (bool on, bool edit);
//
// a detail on SLOT_SCROLL_POSITION which is given the coordinates of the cursor
// or of the center of the screen and it actually means to scroll in such a way
// to make this part visible. SLOT_SCROLL_POSITION is also send while doing
// mouse scrolling in edit_interface_rep::mouse_scroll, in which case the
// position is relative to a query to SLOT_SCROLL_POSITION.
//
// the editor queries SLOT_IDENTIFIER used to check if the widget is
// attached to a window (via is_attached in `message.hpp`)
// and sends to SLOT_INVALIDATE, SLOT_INVALIDATE_ALL to request repaints
//
// we will also receive calls to get_position, get_scroll_position used to
// position popup menus in edit_interface_rep::mouse_adjust.
//
// get_size, get_position refers to the window geometry
//
// send_keyboard_focus, send_mouse_grab

void
vue_simple_widget_rep::send (slot s, blackbox val) {
  //save_send_slot (s, val);
  switch (s) {
    case SLOT_INVALIDATE:
      {
        coord4 r= check_open<coord4> (val, s);
        SI x1= r.x1, y1= r.x2, x2= r.x3, y2= r.x4;
        invalidate_rect (x1, y1, x2, y2);
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
        coord4 r= check_open<coord4> (val, s);
        extents= rectangle (r.x1, r.x2, r.x3, r.x4);
        cout << "extents " << extents << LF;
      }
      break;
    case SLOT_SCROLL_POSITION:
      {
        coord2 pt= check_open<coord2> (val, s);
        scroll_pos= pt;
        absolute_scroll= true;
      }
      break;
    case SLOT_ZOOM_FACTOR:
      {
        double new_zoom= check_open<double> (val, s);
        if (DEBUG_EVENTS) debug_events << "New zoom factor :" << new_zoom << LF;
        handle_set_zoom_factor (new_zoom);
        invalidate_all ();
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
        T contents= check_open<T> (val, s); // x1= name, x2= mask.
        //NOT_IMPLEMENTED("qt_simple_widget::SLOT_MOUSE_POINTER");
      }
      break;
    case SLOT_CURSOR:
      {
        mouse_cursor= check_open <coord2> (val, s);
      }
      break;
    default:
      cout << "WARNING: simple_widget is not handling this " << slot_name (s) << LF;
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
      if (win && !is_nil (win->content))
        return win->content->query(s, type_id);
      else
        return close_box<int>(0);
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
      //FIXME: is ok?
      return close_box<coord2> (size);
    }
    case SLOT_SCROLL_POSITION:
    {
      cout << "scroll_where " << backing_pos << LF;
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> ( coord2 (backing_pos.x1, backing_pos.x2) );
    }
    case SLOT_EXTENTS:
    {
      check_type_id<coord4> (type_id, s);
      return close_box<coord4> (coord4 (extents->x1, extents->y1,
                                        extents->x2, extents->y2));
    }
    case SLOT_VISIBLE_PART:
    {
      check_type_id<coord4> (type_id, s);
      rectangle r (0, size.x2, size.x1, 0);
      ren->set_origin (-backing_pos.x1, -backing_pos.x2);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
//      cout << "visible part " << r << LF;
      return close_box<coord4> (coord4 (r->x1, r->y1, r->x2, r->y2));
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
      return win ? abstract (win->content) : abstract(NULL);
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

void
vue_simple_widget_rep::do_layout () {
  win= current_window; // save the info
  SI w= 0, h= 0;
  Clay_Sizing s= layoutExpand;
  if (is_embedded_widget ()) {
    handle_get_size_hint (w, h);
    s= {
      .width=  CLAY_SIZING_FIT(.min= (float)w/ren->pixel),
      .height= CLAY_SIZING_FIT(.min= (float)h/ren->pixel) };
  }
  Clay_ElementId clay_id= CLAY_IDI("simple_widget", id);
  CLAY({
    .id= clay_id,
    .layout= { .sizing= s },
    .custom= { .customData= vue_render_widget },
    .userData= this })
  {
    Clay_ElementData canvas_layout= Clay_GetElementData (clay_id);
    Clay_Vector2 scrollPosition = {
      .x= (float)backing_pos.x1 / ren->pixel,
      .y= (float)backing_pos.x2 / ren->pixel };
    Clay_ScrollContainerData scrollData= {
      .scrollPosition= &scrollPosition,
      .scrollContainerDimensions= {
        .width=  canvas_layout.boundingBox.width,
        .height= canvas_layout.boundingBox.height },
      .contentDimensions= {
        .width=  ((float)extents->x2 - extents->x1)/ren->pixel,
        .height= ((float)extents->y2 - extents->y1)/ren->pixel, },
      .config= {
        .horizontal= true,
        .vertical= true,
        .childOffset= scrollPosition },
      .found= true
    };
    if (scrollData.found && canvas_layout.found) {
      scroll_bar (id, scrollData, clay_id, canvas_layout);
      scroll_pos.x1= scrollPosition.x * ren->pixel;
      scroll_pos.x2= scrollPosition.y * ren->pixel;
      absolute_scroll= false;
    }
    if (1) { // debug view
      CLAY({
        .backgroundColor= { 80, 80, 80, 80 },
        .layout= { .padding= { 18, 18, 18, 18 } },
        .floating= {
          .attachTo= CLAY_ATTACH_TO_PARENT,
          .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH }})
      {
        debug_text= "";
        tm_ostream out= string_ostream (debug_text);
        out << " extents:  " << extents << LF;
        out << " viewport: " << rectangle(backing_pos.x1,
                                          backing_pos.x2 - canvas_layout.boundingBox.height * ren->pixel,
                                          backing_pos.x1 + canvas_layout.boundingBox.width * ren->pixel,
                                          backing_pos.x2);
        out.flush ();
        CLAY_TEXT(CLAY_TM_STRING(debug_text),
                  CLAY_TEXT_CONFIG({ .fontSize= 30, .textColor= { 0, 0, 200, 255} }));
      }
    }
    if (Clay_Hovered () && (mouse_action != "")) {
      Clay_ElementData d= Clay_GetElementData (clay_id);
      SI x= mouse_x - d.boundingBox.x;
      SI y= mouse_y - d.boundingBox.y;
      ren->set_origin (-backing_pos.x1, -backing_pos.x2);
      ren->encode (x,y);
      if (N(mouse_data) == 2) {
        mouse_data[0] *= ren->pixel * size.x1 * 0.01;
        mouse_data[1] *= ren->pixel * size.x2 * 0.01;
      }
      if (mouse_action != "move") {
        cout << "handling " << mouse_action << " at " << mouse_time << " (" << x << "," << y << ")";
        if (N(mouse_data) == 2) {
          cout << " [" << mouse_data[0] << "," << mouse_data[1] << "]";
        }
        cout << LF;
      }
      if (mouse_action == "wheel") {
        scroll_momentum.x1 += mouse_data[0];
        scroll_momentum.x2 += mouse_data[1];
      } else {
        if (starts (mouse_action, "press-")) {
          if (current_window->kbd_focus != this) {
            current_window->kbd_focus= this;
          }
        }
        handle_mouse (mouse_action, x, y, mouse_state, mouse_time, mouse_data);
      }
      // reset
      mouse_action="";
      if (N(mouse_data) > 0) mouse_data= array<double>();
    }
  }
  if (scroll_momentum.x1 != 0 || scroll_momentum.x2 != 0) {
    cout << "momentum " << scroll_momentum;
    time_t lapse= (texmacs_time () - momentum_time);
    momentum_time = texmacs_time();
    absolute_scroll= false;
    scroll_pos= backing_pos;
    scroll_pos.x1 += scroll_momentum.x1;
    scroll_pos.x2 += scroll_momentum.x2;
    while (lapse > 0) {
      scroll_momentum.x1= 0.97f * scroll_momentum.x1;
      scroll_momentum.x2= 0.97f * scroll_momentum.x2;
      lapse -= 1;
    }
    cout << "-> " << scroll_momentum << LF;
  }
  if ((current_window->kbd_focus == this) && N(key_event)>0) {
    handle_keypress (key_event, key_time);
    key_event= "";
  }
}

/******************************************************************************
 * Backing store management
 ******************************************************************************/

void
vue_simple_widget_rep::invalidate_rect (int x1, int y1, int x2, int y2) {
  int padding= 16;
  rectangle r= rectangle (x1-padding, y1-padding, x2+padding, y2+padding);
  // cout << r << LF;
  invalid_regions= invalid_regions | rectangles (r);
}

void
vue_simple_widget_rep::invalidate_viewport_rect (int x1, int y1, int x2, int y2) {
  ren->set_origin (-backing_pos.x1, -backing_pos.x2);
  ren->encode (x1, y1);
  ren->encode (x2, y2);
  invalidate_rect (x1, y2, x2, y1);
}

void
vue_simple_widget_rep::invalidate_all () {
  //cout << "invalidate all " << LF;
  invalid_regions= rectangles();
  invalidate_viewport_rect (0, 0, size.x1, size.x2);
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
  invalid_intern= ::translate (invalid_intern, dx, dy) & region;
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

  vue_plain_window_widget_rep *w=
      win ? dynamic_cast<vue_plain_window_widget_rep*>(win->content.rep)
          : NULL;

  if (!w) return; // we are not in a layout yet
  
  // retrieve current geometry
  Clay_ElementId clay_id= CLAY_IDI("simple_widget", id);
  Clay_SetCurrentContext (w->win->clay_ctx);
  Clay_ElementData d= Clay_GetElementData (clay_id);
  if (d.found) {
    // cache the current viewport size
    size.x1= d.boundingBox.width; // * retina_factor;
    size.x2= d.boundingBox.height; // * retina_factor;
  } else {
    cout << "clay_id not found!" << LF;
  }
  
  // current backing_store size
  int bs_w= backing_store->get_width ();
  int bs_h= backing_store->get_height ();

  // Update the scroll position

  // viewport size (in TeXmacs units)
  coord2 sz (size.x1 * ren->pixel, size.x2 * ren->pixel);

  if (backing_pos != scroll_pos) {
    // preprocess scroll_pos
    if (absolute_scroll) {
      coord2 pt= scroll_pos;
      scroll_pos= backing_pos;
      cout << "extents " << extents << LF;
      cout << "scroll_to (initial) " << pt << " current " << scroll_pos << " size " << sz << LF;
      if (pt.x1 < scroll_pos.x1) scroll_pos.x1= pt.x1-sz.x1/2;
      else if (pt.x1 > scroll_pos.x1 + sz.x1) scroll_pos.x1= pt.x1-sz.x1/2;
      if (pt.x2 > scroll_pos.x2) scroll_pos.x2= pt.x2+sz.x2/2;
      else if (pt.x2 < scroll_pos.x2 - sz.x2) scroll_pos.x2= pt.x2+sz.x2/2;
      cout << "scroll_pos (corrected) " << scroll_pos << LF;
      absolute_scroll=false;
    }
    
    // clamp the new position
    if (scroll_pos.x1 < extents->x1) scroll_pos.x1= extents->x1;
    else if (scroll_pos.x1 + sz.x1 > extents->x2) scroll_pos.x1= max (extents->x2 - sz.x1, 0);
    if (scroll_pos.x2 - sz.x2 < extents->y1) scroll_pos.x2= min (extents->y1 + sz.x2, 0);
    else if (scroll_pos.x2 > extents->y2) scroll_pos.x2= extents->y2;
  }
  
  // check if the scroll position has changed. backing_pos is the old position,
  // while scroll_pos is the new one. Instead of repainting the whole backing store,
  // we move the contents of the backing store, and invalidate the regions that
  // are not covered by the moved contents.
  
  if (backing_pos != scroll_pos) {
    int dx=  retina_factor * (scroll_pos.x1 - backing_pos.x1);
    int dy=  retina_factor * (scroll_pos.x2 - backing_pos.x2);

    backing_pos= scroll_pos;
    cout << "SCROLL CONTENTS BY " << dx << " " << dy << LF;
        
#if 0
    //FIXME: complete this part
    if (backing_valid) {
      translate_backing_store (0, 0, bs_w, bs_h, -dx, -dy);
      if (dy<0) invalidate_viewport_rect (0, 0, bs_w, min (bs_h,-dy));
      else if (dy>0) invalidate_viewport_rect (0, max (0,bs_h-dy), bs_w, bs_h);
      if (dx<0) invalidate_viewport_rect (0, 0, min (-dx, bs_w), bs_h);
      else if (dx>0) invalidate_viewport_rect (max (0, bs_w-dx), 0, bs_w, bs_h);
    } else {
      invalidate_all ();
    }
#else
    invalidate_all ();
#endif
  }
  
  // check if the window has been resized. If so, we need to resize the backing
  // store as well. During the resize, the origin remain the same. So we can just
  // crop the backing store if the window is smaller, or fill the new regions with
  // the background color if the window is bigger.

  int new_bs_w= size.x1;
  int new_bs_h= size.x2;

  if ((new_bs_w != bs_w)   || (new_bs_h != bs_h)) {
    // the viewport size changed, reset the backing store
    cout << "viewport changed (" << bs_w << "," << bs_h << ") (" << new_bs_w << "," << new_bs_h << ")" << LF;
    // create a new backing store with updated viewport and the renderer
    picture new_backing_store= native_picture (new_bs_w, new_bs_h, 0, 0);
    renderer ren2= picture_renderer (new_backing_store, std_shrinkf * retina_factor);
    
    // copy the old backingstore
    SI x1=0, y1=0, x2=bs_w, y2=bs_h;
    ren->set_origin (0,0); // we just want to copy bitmaps
    ren->encode (x1, y1);
    ren->encode (x2, y2);
    ren2->fetch (x1, y2, x2, y1, ren, x1, y2);
    
    // compute new invalid regions
    // add new exposed regions due to resize
    if (new_bs_w > bs_w) {
      rectangle r= rectangle (bs_w, new_bs_h, new_bs_w, 0);
      ren->set_origin (-backing_pos.x1, -backing_pos.x2);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      invalid_regions= invalid_regions | rectangles (r);
    }
    if (new_bs_h > bs_h) {
      rectangle r= rectangle (0, new_bs_h, new_bs_w, bs_h);
      ren->set_origin (-backing_pos.x1, -backing_pos.x2);
      ren->encode (r->x1, r->y1);
      ren->encode (r->x2, r->y2);
      invalid_regions= invalid_regions | rectangles (r);
    }
    
    // update the state
    bs_w= new_bs_w;
    bs_h= new_bs_h;
    backing_store= new_backing_store;
    delete_renderer (ren);
    ren= ren2;
  }
  
  // repaint invalid rectangles if needed
  if (!is_nil (invalid_regions)) {
    rectangles new_regions;
    
    // simplify
    rectangle lub= least_upper_bound (invalid_regions);
    if (area (lub) < 1.2 * area (invalid_regions))
      invalid_regions= rectangles (lub);
    
    while (!is_nil (invalid_regions)) {
      rectangle r= copy (invalid_regions->item);
      // cout << "repaint " << r << LF;
      r= thicken (r, 1, 1);
      ren->set_origin (-backing_pos.x1, -backing_pos.x2);
      ren->set_clipping (r->x1, r->y1, r->x2, r->y2);
      handle_repaint (ren, r->x1, r->y1, r->x2, r->y2);
      ren->set_clipping (r->x1, r->y1, r->x2, r->y2, true);
      if (gui_interrupted ())
        new_regions= rectangles (invalid_regions->item, new_regions);
      invalid_regions= invalid_regions->next;
    }
    invalid_regions= new_regions;
  } // if (!is_nil (invalid_regions))
  backing_valid= true;
}

void
vue_simple_widget_rep::repaint_all () {
  list<vue_simple_widget_rep*> l= paint_list;
  while (!is_nil(l)) {
    l->item->repaint_invalid_regions ();
    l= l->next;
  }
}

void
vue_simple_widget_rep::repaint_all_in_window (vue_window win) {
  list<vue_simple_widget_rep*> l= paint_list;
  while (!is_nil(l)) {
    if (l->item->win == win) l->item->repaint_invalid_regions ();
    l= l->next;
  }
}


void
vue_simple_widget_rep::render (void *data) {
  current_window->draw_picture (data, backing_store);
}

//-----------------------------------------------------------------------------
//vue_chooser_widget


/*!
  \param _cmd  Scheme closure to execute after the dialog is closed.
  \param _type What kind of dialog to show. Can be one of "image", "directory",
               or any of the supported file formats: "texmacs", "tmml",
               "postscript", etc. See perform_dialog()
 */
vue_chooser_widget_rep::vue_chooser_widget_rep (command _cmd, string _type, string _prompt)
 : vue_widget_rep ("file_chooser"), cmd (_cmd), prompt (_prompt),
   position (coord2 (0, 0)), size (coord2 (100, 100)), file ("")
{
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_chooser_widget_rep::vue_chooser_widget_rep type=\""
                  << type << "\" prompt=\"" << prompt << "\"" << LF;
  if (N(_type) > 0)
    type= _type;
  else type= "generic";
}

void
vue_chooser_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_VISIBILITY:
    {
      bool flag= check_open<bool> (val, s);
      (void) flag;
      FAILED("vue_chooser_widget::SLOT_VISIBILITY not implemented");
    }
      break;
    case SLOT_SIZE:
      size= check_open<coord2> (val,s );
      break;
    case SLOT_POSITION:
      position= check_open<coord2> (val, s);
      break;
    case SLOT_KEYBOARD_FOCUS:
      {
        check_type<bool>(val, s);
        tm_window win= concrete_window ();
        vue_window platform_win= 0;
        if (win) {
          vue_plain_window_widget_rep *vw= dynamic_cast<vue_plain_window_widget_rep*> (win->win.rep);
          if (vw) platform_win= vw->win;
        }
        perform_dialog (platform_win);
      }
      break;
    case SLOT_STRING_INPUT:
      check_type<string>(val, s);
      if (DEBUG_QT_WIDGETS)
        debug_widgets << "\tString input: " << open_box<string> (val) << LF;
      FAILED ("vue_chooser_widget::SLOT_STRING_INPUT not implemented");
      break;
    case SLOT_INPUT_TYPE:
      type= check_open<string> (val, s);
      break;
    case SLOT_FILE:
        //send_string (THIS, "file", val);
      file= check_open<string> (val, s);
      if (DEBUG_QT_WIDGETS)
        debug_widgets << "\tFile: " << file << LF;
      break;
    case SLOT_DIRECTORY:
      directory= check_open<string> (val, s);
      directory= as_string (url_pwd () * url_system (directory));
      break;
      
    default:
      vue_widget_rep::send (s, val);
  }
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_chooser_widget_rep: sent " << slot_name (s)
                  << "\t\tto widget\t"      << type << LF;
}

blackbox
vue_chooser_widget_rep::query (slot s, int type_id) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_chooser_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (position);
    }
    case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (size);
    }
    case SLOT_STRING_INPUT:
    {
      check_type_id<string> (type_id, s);
      if (DEBUG_VUE_WIDGETS) debug_widgets << "\tString: " << file << LF;
      return close_box<string> (file);
    }
    default:
      return vue_widget_rep::query (s, type_id);
  }
}

widget
vue_chooser_widget_rep::read (slot s, blackbox index) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_chooser_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
    case SLOT_WINDOW:
    case SLOT_FORM_FIELD:
    case SLOT_FILE:
    case SLOT_DIRECTORY:
      check_type_void (index, s);
      return this;
    default:
      return vue_widget_rep::read (s,index);
  }
}

void
vue_chooser_widget_rep::callback (char* res) {
  if (!res) {
    file= "#f";
  } else {
    string name (res, strlen (res));
    file= "(system->url " * scm_quote (name) * ")";
    if (type == "image") {
      url u= url_system (name);
      string w, h;
      //qt_pretty_image_size (u, w, h);
      string params;
      params << "\"" << w << "\" "
      << "\"" << h << "\" "
      << "\"" << "" << "\" "  // xps ??
      << "\"" << "" << "\"";   // yps ??
      file= "(list " * file * " " * params * ")";
    }
    cmd ();
    if (!is_nil (quit)) quit ();
  }
}


widget
file_chooser_widget (command cmd, string type, string prompt) {
  return abstract (tm_new<vue_chooser_widget_rep> (cmd, type, prompt));
}




//-----------------------------------------------------------------------------
// vue_field_widget_rep



//-----------------------------------------------------------------------------
// vue_inputs_list_widget

/*! A dialog with a list of inputs and ok and cancel buttons.
 
 In the general case each input is a vue_field_widget_rep which we lay out in a
 vertical table. However, for simple yes/no/cancel questions we try to use a
 system default dialog
 
 TODO?
 We try to use OS dialogs whenever possible, but this still needs improvement.
 We should also use a custom Qt widget and then bundle it in a modal window if
 required, so as to eventually be able to return something embeddable in
 as_qwidget(), in case we want to reuse this.
 */
class vue_inputs_list_widget_rep: public vue_widget_rep {
public:
  command cmd;
  coord2 size, position;
  string win_title;
  int style;
  array<vue_widget> fields;

  vue_inputs_list_widget_rep (command, array<string>);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
  void perform_dialog();
//  vue_field_widget_rep* field (int i);
};




/*! Each of the fields in a vue_inputs_list_widget_rep.
 
 Each field is composed of a prompt (a label) and an input (a QTMComboBox).
 */

class vue_field_widget_rep: public vue_widget_rep {
  string           prompt;
  string            input;
  string             type;
  array<string> proposals;
  vue_inputs_list_widget_rep* parent;

public:
  vue_field_widget_rep (vue_inputs_list_widget_rep* _parent, string _prompt);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);

  friend class vue_inputs_list_widget_rep;
};


vue_field_widget_rep::vue_field_widget_rep (vue_inputs_list_widget_rep* _parent,
                                          string _prompt)
  : vue_widget_rep ("field_widget"),
    prompt (_prompt), input (""), proposals (), parent (_parent)
{ }

void
vue_field_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_field_widget_rep::send " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    input= scm_quote (check_open<string> (val, s));
    break;
  case SLOT_INPUT_TYPE:
    type= check_open<string> (val, s);
    break;
  case SLOT_INPUT_PROPOSAL:
    proposals << check_open<string> (val, s);
    break;
  case SLOT_KEYBOARD_FOCUS:
    parent->send (s, val);
    break;
  default:
    vue_widget_rep::send (s, val);
  }
}

blackbox
vue_field_widget_rep::query (slot s, int type_id) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_field_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_STRING_INPUT:
    check_type_id<string> (type_id, s);
    return close_box<string> (input);
  default:
    return vue_widget_rep::query (s, type_id);
  }
}

vue_inputs_list_widget_rep::vue_inputs_list_widget_rep (command _cmd,
                                                      array<string> _prompts)
: vue_widget_rep ("inputs_list_widget"),
  cmd (_cmd), size (coord2 (100, 100)),
  position (coord2 (0, 0)),
  win_title (""), style (0)
{
  for (int i= 0; i < N(_prompts); i++)
    fields << concrete (tm_new<vue_field_widget_rep> ((vue_inputs_list_widget_rep*)this, _prompts[i]));
}

void
vue_inputs_list_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "vue_inputs_list_widget_rep::send " << slot_name(s) << LF;

  switch (s) {
  case SLOT_VISIBILITY:
    {
      bool flag= check_open<bool> (val, s);
      (void) flag;
      cout << "vue_inputs_list_widget::SLOT_VISIBILITY not implemented" << LF;
    }
    break;
  case SLOT_SIZE:
    size= check_open<coord2> (val, s);
    break;
  case SLOT_POSITION:
    position= check_open<coord2> (val, s);
    break;
  case SLOT_KEYBOARD_FOCUS:
    if (check_open<bool> (val, s)) perform_dialog ();
    break;
  default:
    vue_widget_rep::send (s, val);
  }
}

blackbox
vue_inputs_list_widget_rep::query (slot s, int type_id) {
  if (DEBUG_QT_WIDGETS)
    debug_widgets << "vue_inputs_list_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
  case SLOT_POSITION:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (position);
    }
  case SLOT_SIZE:
    {
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (size);
    }
  case SLOT_STRING_INPUT:
    if (N(fields) > 0) return fields[0]->query (s, type_id);
      
  default:
    return vue_widget_rep::query (s, type_id);
  }
}

widget
vue_inputs_list_widget_rep::read (slot s, blackbox val) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_inputs_list_widget_rep::read " << slot_name(s) << LF;
  switch (s) {
  case SLOT_WINDOW:
    check_type_void (val, s);
    return this;
  case SLOT_FORM_FIELD:
  {
    int index= check_open<int> (val, s);
    if (N(fields) > index)
      return static_cast<widget_rep*> (fields[index].rep);
  }
  default:
    return vue_widget_rep::read (s, val);
  }
}

void
vue_inputs_list_widget_rep::perform_dialog () {
  //FIXME: implement correct commands
  //maybe just create the window ahead and pass focus
  array<widget> lhs, rhs;
  for (int i=0; i< N(fields); i++) {
    vue_field_widget_rep* f= dynamic_cast<vue_field_widget_rep*> (fields[i].rep);
    if (f) {
      lhs << text_widget (f->prompt, 0, black);
      rhs << input_text_widget (command (), f->input, f->proposals, 0, "1w");
    }
  }
  widget w= vertical_list (array (
    aligned_widget (lhs, rhs),
    horizontal_list (array (
      menu_button (text_widget ("Cancel", 0, black), command ()),
      menu_button (text_widget ("Ok", 0, black), command ())))));
  plain_window_widget (w, win_title, command ());
}

//VUE_WIDGET(inputs_list_widget, command, call_back, array<string>, prompts);


widget
inputs_list_widget (command call_back, array<string> prompts) {
  return abstract (tm_new<vue_inputs_list_widget_rep> (call_back, prompts));
}


//-----------------------------------------------------------------------------

// toplevel window constructor

widget plain_window_widget (widget wid, string s, command quit) {
  if (concrete (wid)->type == "chooser_widget") {
    vue_chooser_widget_rep* cw= dynamic_cast<vue_chooser_widget_rep*> (wid.rep);
    cw->win_title= s;
    cw->quit= quit;
    return wid;
  } else if (concrete (wid)->type == "inputs_list_widget") {
    vue_inputs_list_widget_rep* cw= dynamic_cast<vue_inputs_list_widget_rep*> (wid.rep);
    cw->win_title= s;
//    cw->quit= quit;  // we already have a command
    return wid;
  } else {
    SI root_w, root_h;
    gui_root_extents (root_w, root_h);
    SI min_w= 0, min_h= 0, def_w= root_w, def_h= root_h,
    max_w= root_w, max_h= root_h;
    
    vue_plain_window_widget_rep *wwid= tm_new<vue_plain_window_widget_rep> (wid, s, quit);
    //wwid->win=
    plain_window (wwid, s);
    //  plain_window (wwid, s, min_w, min_h, def_w, def_h, max_w, max_h);
    return abstract (wwid);
  }
}
  
void destroy_window_widget (widget w) {
  vue_widget vw= concrete(w);
  cout << "destroy_window_widget on " << vw->type << LF;
  vue_plain_window_widget_rep *ww= dynamic_cast<vue_plain_window_widget_rep*> (vw.rep);
  if (ww){
    tm_delete (ww->win);
  } else {
    cout << "not a window widget!" << LF;
  }
}
// destroys a window as created by the above routines

