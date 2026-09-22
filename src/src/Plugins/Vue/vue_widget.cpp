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
#include "sys_utils.hpp" // for system (printer widget)
#include "analyze.hpp"   // for occurs (filtered choice)
#include "poly_line.hpp" // for ink widget

#if MUPDF_RENDERER
#include "../MuPDF/mupdf_picture.hpp"
#else
#include "../MuPDF/fitz_picture.hpp"
#endif

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

#include <SDL3/SDL.h> // SDL_GetSystemTheme (the themes)
#include "clay.h"
#include "clay_grid.h"

Clay_Sizing layoutExpand= {
  .width=  CLAY_SIZING_GROW(0),
  .height= CLAY_SIZING_GROW(0) };

Clay_Sizing layoutFit= {
  .width=  CLAY_SIZING_FIT(),
  .height= CLAY_SIZING_FIT() };

Clay_Sizing layoutFull= {
  .width=  CLAY_SIZING_PERCENT(1.0f),
  .height= CLAY_SIZING_PERCENT(1.0f) };

// Clay keeps the pointers to the strings of the element ids (its hash map
// items, the debug view of F1) beyond the layout pass: the strings must
// outlive the TeXmacs strings they come from (the debug view crashed on
// freed memory). They are interned once and never freed (type names, a few
// dozen); ids derived from a label and numbers use probe_id instead
static Clay_String
clay_tm_string (string s) {
  static hashmap<string,int> index (-1);
  static array<char*> store;
  int i= index[s];
  if (i < 0) {
    char* c= tm_new_array<char> (N(s) + 1);
    for (int k= 0; k < N(s); k++) c[k]= s[k];
    c[N(s)]= 0;
    i= N(store);
    store << c;
    index (s)= i;
  }
  return CLAY__INIT(Clay_String) { .isStaticallyAllocated= true, .length= N(s), .chars= store[i] };
}
#define CLAY_TM_STRING(s) clay_tm_string (s)

// the id of the k-th probe element of the widget 'id' (an element laid out
// only to be measured): a static label, the numbers go in the offset
static inline Clay_ElementId
probe_id (const char* label, unsigned int id, unsigned int k) {
  Clay_String cs= CLAY__INIT(Clay_String) { .isStaticallyAllocated= true, .length= (int32_t) strlen (label), .chars= label };
  return Clay__HashString (cs, id * 4096u + k);
}

/******************************************************************************
* Themes
*
* Every colour of the interface is a field of vue_theme; the globals below
* are its fields, so that the widgets can go on naming them. A theme is
* chosen with the "gui theme" preference: "light", "dark", or "default",
* which follows the appearance of the system (SDL_GetSystemTheme, and the
* SYSTEM_THEME_CHANGED event). Adding a theme means adding a constant of
* this type and a line in vue_theme_named; a widget which needs a colour
* which is not here should get a new field rather than a literal, or the
* theme will not cover it.
******************************************************************************/

static const vue_theme vue_theme_light= {
  .shade= { {160, 160, 160, 255}, {192, 192, 192, 255},
            {224, 224, 224, 255}, {240, 240, 240, 255} },
  .background= {192, 192, 192, 255},
  .highlight= {240, 240, 240, 255},
  .text= {0, 0, 0, 255},
  .text_grey= {112, 112, 112, 255},
  .border= {150, 150, 150, 255},
  .field= {250, 250, 250, 255},
  .field_focused= {238, 238, 228, 255},
  .selection= {100, 100, 255, 255},
  .selection_text= {255, 255, 255, 255},
  .selection_soft= {180, 196, 232, 255},
  .button= {236, 236, 236, 255},
  .button_hover= {248, 248, 248, 255},
  .button_down= {205, 205, 205, 255},
  .pressed= {200, 200, 200, 255},
  .scrollbar= {120, 120, 160, 150},
  .scrollbar_hover= {100, 100, 140, 150},
  .bar_line= {176, 176, 176, 255},
  .bar_mode= {212, 212, 212, 255},
  .bar_focus= {232, 232, 232, 255},
  .tab_inactive= {176, 176, 176, 255},
  .canvas= {160, 160, 160, 255},
  // a sheet of note paper rather than a warning sign
  .balloon= {252, 250, 232, 255},
  .balloon_border= {186, 180, 148, 255},
  .pre_edit= {252, 250, 232, 255},
  .pre_edit_line= {120, 120, 180, 255},
  .cursor= {224, 0, 0, 255}
};

static const vue_theme vue_theme_dark= {
  .shade= { {36, 36, 38, 255}, {52, 52, 55, 255},
            {68, 68, 72, 255}, {86, 86, 90, 255} },
  .background= {52, 52, 55, 255},
  .highlight= {86, 86, 90, 255},
  .text= {228, 228, 230, 255},
  .text_grey= {140, 140, 146, 255},
  .border= {92, 92, 98, 255},
  .field= {38, 38, 40, 255},
  .field_focused= {46, 46, 42, 255},
  .selection= {66, 96, 180, 255},
  .selection_text= {255, 255, 255, 255},
  .selection_soft= {64, 78, 120, 255},
  .button= {70, 70, 74, 255},
  .button_hover= {88, 88, 94, 255},
  .button_down= {44, 44, 47, 255},
  .pressed= {44, 44, 47, 255},
  .scrollbar= {130, 130, 170, 150},
  .scrollbar_hover= {154, 154, 194, 150},
  .bar_line= {34, 34, 36, 255},
  .bar_mode= {60, 60, 64, 255},
  .bar_focus= {68, 68, 72, 255},
  .tab_inactive= {44, 44, 47, 255},
  .canvas= {30, 30, 32, 255},
  .balloon= {62, 60, 44, 255},
  .balloon_border= {120, 116, 86, 255},
  .pre_edit= {62, 60, 44, 255},
  .pre_edit_line= {150, 150, 210, 255},
  .cursor= {255, 96, 96, 255}
};

vue_theme the_theme= vue_theme_light;

// the TeXmacs colour of a colour of the theme (for the text routines)
color
theme_color (Clay_Color c) {
  return rgb_color ((int) c.r, (int) c.g, (int) c.b, (int) c.a);
}

// the colours the widgets use; they are the fields of the current theme
Clay_Color palette[4];
Clay_Color color_background, color_highlight, color_text, color_border;
Clay_Color color_field, color_button, color_button_hover, color_button_down;
Clay_Color color_pressed;

static void
vue_apply_theme () {
  for (int i= 0; i < 4; i++) palette[i]= the_theme.shade[i];
  color_background= the_theme.background;
  color_highlight= the_theme.highlight;
  color_text= the_theme.text;
  color_border= the_theme.border;
  color_field= the_theme.field;
  color_button= the_theme.button;
  color_button_hover= the_theme.button_hover;
  color_button_down= the_theme.button_down;
  color_pressed= the_theme.pressed;
}

// Counts the changes of the icon theme: a picture widget which holds an
// icon of an older generation loads it again (see icon_picture).
static int icon_generation= 0;

// "light", "dark", or anything else (the "default" of the preference) to
// follow the appearance of the system
void
set_vue_theme (string name) {
  // TEXMACS_VUE_THEME overrides the preference (for the tests, and to try
  // a theme without changing the settings)
  string forced= get_env ("TEXMACS_VUE_THEME");
  if (N(forced) > 0) name= forced;
  bool dark;
  if (name == "dark") dark= true;
  else if (name == "light") dark= false;
  else dark= (SDL_GetSystemTheme () == SDL_SYSTEM_THEME_DARK);
  the_theme= dark ? vue_theme_dark : vue_theme_light;
  vue_apply_theme ();
  // the vector icons come in a light and a dark set: the widgets which were
  // built with the other one load theirs again (see icon_picture)
  string icons= dark ? string ("dark") : string ("light");
  if (icons != mupdf_get_icon_theme ()) {
    mupdf_set_icon_theme (icons);
    icon_generation++;
  }
  // the surround of the pages is a colour of TeXmacs, not of the widgets
  tm_background= rgb_color (the_theme.canvas.r, the_theme.canvas.g,
                            the_theme.canvas.b);
}

/*****************************************************************************/
// UI layout context (maybe refactor in a structure)

// keyboard events
string key_event;
string last_key;
time_t key_time;

// pointer info
string mouse_action;
time_t mouse_time;
int mouse_x; // signed: see vue_input_state in vue_gui.hpp
int mouse_y;
int mouse_ticket= 0; // the payload of a "drop" action
unsigned int mouse_state= 0;
array<double> mouse_data;

bool current_popup; // is there an active popup?
bool cancel_popup;  // should we cancel popups?
uint32_t open_pull_id= 0; // the pull button whose menu is open (0: none)
time_t away_time;   // tolerance for mouse motion

// some more context during layout
Clay_ElementId last_id;
bool debug_clay=false;

// ask the buttons to fit all horizontal space (items of vertical menus)
bool button_grow= false;
// the vertical menu being laid out has items with check marks: all its items
// reserve the column of the marks so that the labels align
bool menu_has_marks= false;

uint32_t current_balloon;
time_t balloon_time;

// list of commands
list<command> cmd_list;

vue_window current_window; // used during layout to propagate information
bool window_autosizing= false; // the window is being sized to its contents
int context_style= 0; // style flags (bold, grey) added by the enclosing divisions
bool in_title_bar= false; // laying out the title bar of a tool (its "x" is a close button)
// laying out the buttons of a "sections" or "section-tabs" bar of a tool:
// 0 not in a bar, 1 in a bar of buttons, 2 in a bar of tabs; the selected
// entry is wrapped in an "active-section"/"section-active-tab" division
int  section_bar= 0;
bool section_active= false;
bool layout_again= false; // see vue_widget.hpp
bool gui_needs_relayout= false; // see vue_widget.hpp

// signalling

typedef struct ui_signal {
  int  clicked; // a click completed on the element: none, left, middle, right
  int  pressed; // a button went down on the element in this pass (same values)
  bool held;    // the element is active: a button went down on it and is
                // still held, wherever the pointer is now (capture)
} ui_signal;

uint32_t active_id;
int active_button; // none, left, middle, right
uint32_t hot_id;

ScrollbarData scrollbarData= { 0, 0, true };

// The globals above describe the window currently being laid out; they are
// loaded from and stored back to the vue_input_state of that window so that
// every window only sees its own events.

static void
load_input_state (vue_window win) {
  vue_input_state& in= win->input;
  key_event= in.key_event;
  last_key= in.last_key;
  key_time= in.key_time;
  mouse_action= in.mouse_action;
  mouse_time= in.mouse_time;
  mouse_x= in.mouse_x;
  mouse_y= in.mouse_y;
  mouse_ticket= in.mouse_ticket;
  mouse_data= in.mouse_data;
  current_popup= in.current_popup;
  cancel_popup= in.cancel_popup;
  away_time= in.away_time;
  current_balloon= in.current_balloon;
  balloon_time= in.balloon_time;
  hot_id= in.hot_id;
  active_id= in.active_id;
  active_button= in.active_button;
  last_id= in.last_id;
  scrollbarData= in.scrollbar;
}

static void
store_input_state (vue_window win) {
  vue_input_state& in= win->input;
  in.key_event= key_event;
  in.last_key= last_key;
  in.key_time= key_time;
  in.mouse_action= mouse_action;
  in.mouse_time= mouse_time;
  in.mouse_x= mouse_x;
  in.mouse_y= mouse_y;
  in.mouse_ticket= mouse_ticket;
  in.mouse_data= mouse_data;
  in.current_popup= current_popup;
  in.cancel_popup= cancel_popup;
  in.away_time= away_time;
  in.current_balloon= current_balloon;
  in.balloon_time= balloon_time;
  in.hot_id= hot_id;
  in.active_id= active_id;
  in.active_button= active_button;
  in.last_id= last_id;
  in.scrollbar= scrollbarData;
}

void
set_kbd_focus (vue_window win, vue_widget w) {
  if (win == NULL || win->kbd_focus == w) return;
  time_t t= texmacs_time ();
  vue_simple_widget_rep* old= dynamic_cast<vue_simple_widget_rep*> (win->kbd_focus.rep);
  win->kbd_focus= w;
  if (old != NULL) old->handle_keyboard_focus (false, t);
  vue_simple_widget_rep* cur= dynamic_cast<vue_simple_widget_rep*> (w.rep);
  if (cur != NULL) cur->handle_keyboard_focus (true, t);
}

void
notify_window_focus (vue_window win, bool has_focus) {
  if (win == NULL) return;
  vue_simple_widget_rep* cur= dynamic_cast<vue_simple_widget_rep*> (win->kbd_focus.rep);
  if (cur != NULL) cur->handle_keyboard_focus (has_focus, texmacs_time ());
}

void
gui_init_context() {
  load_input_state (current_window);
  hot_id= 0;
  // a release which never reached us (outside the window) ends the capture;
  // the release event itself comes with the buttons already up and must
  // still find the active element (it is the click)
  if (active_id != 0 && (mouse_state & 7) == 0 &&
      !starts (mouse_action, "press-") && !starts (mouse_action, "release-")) {
    active_id= 0;
    active_button= 0;
  }
  
  // popup state initialization
  current_popup= false;
  cancel_popup= false;
  
  // make refresh messages available to widgets during layout
  current_window->refresh_kinds= current_window->next_refresh_kinds;
  current_window->next_refresh_kinds= hashset<string>();
}

void
gui_finalize_context() {
  if (starts (mouse_action, "release-")) {
    // deactivate elements, probably we released a button away from the active element
    active_button= 0;
    active_id= 0;
  }
  // events live for exactly one layout pass of their window
  mouse_action= "";
  key_event= "";
  mouse_data= array<double> ();
  store_input_state (current_window);
}


// The common mouse protocol of the elements: the element under the pointer
// is "hot" (hovered) unless another one is active; a press over an element
// makes it active until the button is released (the element keeps the
// pointer: it can be dragged outside, as a scroll bar thumb), a release over
// the active element is a click, a release elsewhere just deactivates it
// (gui_finalize_context).
ui_signal
button_logic (Clay_ElementId id) {
  static char const* p[4]= {"press-none",   "press-left",   "press-middle",   "press-right"};
  static char const* r[4]= {"release-none", "release-left", "release-middle", "release-right"};
  ui_signal res { .clicked= 0, .pressed= 0, .held= (active_id == id.id) };
  if (Clay_PointerOver (id)) {
    if (active_id == 0) {
      hot_id= id.id;
    }
    for (int i=0; i<4; i++) {
      if (mouse_action == p[i]) {
        active_id= id.id;
        active_button= i;
        res.pressed= i;
        res.held= true;
        break;
      }
      if ((mouse_action == r[i]) && (active_id == id.id) && (active_button == i)) {
        res.clicked= i;
        mouse_action= "";
        active_id= 0;
        active_button= 0;
        break;
      }
    }
  }
  return res;
}

/*****************************************************************************/

// DEBUG_VUE, DEBUG_VUE_WIDGETS and DEBUG_VUE_EVENTS: see vue_widget.hpp

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
  gui_needs_relayout= true;
  delete ptr;
}
#else
template<> void
tm_delete<vue_widget_rep> (vue_widget_rep* ptr) {
  if (ptr == NULL) return;
  gui_needs_relayout= true;
  void *mem= ptr->derived_this ();
  ptr -> ~vue_widget_rep ();
  fast_delete (mem);
}
#endif

unsigned int vue_widget_rep::serial_id= 0;

// The widgets named by the render commands of the last layout, see
// vue_widget.hpp. Holding them here is what makes those commands safe to
// draw: a widget which has left the widget tree is destroyed when the
// commands which name it are dropped, not before.
static array<widget> layout_widgets;

void*
vue_widget_rep::render_ref () {
  layout_widgets << widget (this);
  return (void*) this;
}

void
release_layout_widgets () { layout_widgets= array<widget> (); }

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
// VUE_WIDGET(popup_window_widget, widget, w, string, s);
// creates an undecorated popup window with name s and contents w
// VUE_WIDGET(tooltip_window_widget, widget, w, string, s);
// creates an undecorated tooltip window with name s and contents w
// (both are implemented at the end of this file using vue_plain_window_widget_rep)

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
VUE_WIDGET(choice_widget, command, cb, array<string>, vals, array<string>, chosen, bool, flag, int, style);
  // select one value (flag false) or several (flag true) from a list
widget choice_widget (command cmd, array<string> vals, array<string> chosen, int style) {
  return choice_widget (cmd, vals, chosen, true, style);
}
  // select a value from a long list of possible values
widget choice_widget (command cmd, array<string> vals, string cur, int style) {
  array<string> chosen (1);
  chosen[0]= cur;
  return choice_widget (cmd, vals, chosen, false, style);
}
  // select multiple values from a long list
VUE_WIDGET(filtered_choice_widget, command, cb, array<string>, vals, string, val, string, filter);
widget choice_widget (command cmd, array<string> vals, string cur, string filter) {
  return filtered_choice_widget(cmd, vals, cur, filter);
}
  // select a value from a long list with scrollbars and an input to filter
// VUE_WIDGET(tree_view_widget, command, cmd, tree, data, tree, data_roles);
  // A widget with a tree view which observes the data and updates automatically
  // (see vue_tree_view_widget_rep below)

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

// The "setting" widgets of the preference tools (a control with its
// description) and the responsive tabs are composed from simpler widgets
widget setting_toggle_widget (command cmd, string text, bool on, int style) {
  array<widget> a;
  a << toggle_widget (cmd, on, style)
    << glue_widget (false, false, 6*PIXEL, 0)
    << text_widget (text, style, black, false);
  return horizontal_list (a);
}
  // a check box followed by its description
widget setting_enum_widget (command cb, string text, array<string> vals,
                            string val, int st, string w) {
  array<widget> a;
  a << text_widget (text, st, black, false)
    << glue_widget (true, false, 6*PIXEL, 0)
    << enum_widget (cb, vals, val, st, w);
  return horizontal_list (a);
}
  // a description followed by a drop-down list
widget setting_group_widget (string text, array<widget> vals, int style) {
  array<widget> a;
  a << division_widget ("subtitle", text_widget (text, style, black, false))
    << vertical_list (vals);
  return vertical_list (a);
}
  // a titled group of settings
widget responsive_tabs_widget (array<widget> tabs, array<widget> bodies) {
  return tabs_widget (tabs, bodies);
}
widget responsive_icon_tabs_widget (array<url> us, array<widget> ss, array<widget> bs) {
  return icon_tabs_widget (us, ss, bs);
}
  // tabs which adapt to the available space: plain tabs here
  // an input toggle
VUE_WIDGET(wait_widget, SI, width, SI, height, string, message);
  // a widget of a specified width and height, displaying a wait message
  // this widget is only needed when using the X11 plugin
// VUE_WIDGET(ink_widget, command, cb);
  // widget for inking a sketch. The input may later be passed to
  // an external program for handwriting recognition,
  // using the callback routine (see vue_ink_widget_rep below)
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
// file_name is the icon the picture was loaded from (none for a picture
// which has no file), and stamp the icon theme and the resolution it was
// loaded for: see icon_picture
VUE_WIDGET_DATA(picture_widget, picture, p, url, file_name, int, stamp);
// The icon a picture widget shows depends on the theme (the light or the
// dark vector set) and on the resolution it is drawn at, both of which may
// change while the widget is alive: it is loaded again when they do.
static int
icon_stamp () { return 8 * icon_generation + retina_factor; }

static picture
icon_picture (blackbox& data) {
  vue_picture_widget d= open_box<vue_picture_widget> (data);
  if (d.stamp != icon_stamp () && !is_none (d.file_name)) {
    d.p= load_xpm (d.file_name);
    d.stamp= icon_stamp ();
    data= close_box (d);
  }
  return d.p;
}

VUE_WIDGET_DATA(cached_pull_button, widget, w, promise<widget>, pw, widget, cw,
                bool, placed, bool, flip, float, shift_x, float, shift_y);
// placed: the position of the open menu has been decided (see layout_pull_button)
// flip: the menu is opened on the other side of the button to stay in the window
// shift_x, shift_y: shift of the menu to keep it inside the window
// data for a button w with a lazy pulldown menu pw and a cached value
VUE_WIDGET_DATA(cached_glue_widget, picture, pic, tree, col, bool, hx, bool, vx, SI, w, SI, h);

VUE_WIDGET_DATA(tabs_widget_star, array<widget>, tabs, array<widget>, icons, array<widget>, bodies, int, current);

VUE_WIDGET_DATA(refreshable_widget_star, object, prom, string, kind, widget, current, object, curobj);

VUE_WIDGET_DATA(refresh_widget_star, string, tmwid, string, kind, widget, current, object, curobj);

VUE_WIDGET_DATA(split_widget_star, widget, a, widget, b, float, pos, bool, dragging);
// hsplit/vsplit widgets with the position of the divider (in pixels, <0 if unset)

VUE_WIDGET_DATA(enum_widget_star, command, cb, array<string>, vals, string, val, int, st, string, w, bool, open);
// an enum widget with the state of its dropdown list

VUE_WIDGET_DATA(filtered_choice_widget_star, command, cb, array<string>, vals, string, val, widget, input);
// a filtered choice widget with the input field used for the filter

VUE_WIDGET_DATA(printer_widget_star, command, cmd, url, ps_pdf_file, widget, content, widget, printer, widget, copies, widget, pages, string, options_for);
// a printer widget with the prebuilt dialog contents

VUE_WIDGET_DATA(color_picker_widget_star, command, cmd, bool, bg, array<tree>, proposals, widget, content);
// a color picker with the prebuilt dialog contents

/******************************************************************************
* Helper commands
******************************************************************************/

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

inline command
applied_command (command cmd, object arg) {
  return tm_new<applied_command_rep> (cmd, arg);
}

class noop_command_rep: public command_rep {
public:
  noop_command_rep () {}
  void apply () {}
  void apply (object arg) { (void) arg; }
  tm_ostream& print (tm_ostream& out) { return out << "<noop_command>"; }
};

inline command
noop_command () {
  return tm_new<noop_command_rep> ();
}

string input_text_widget_string (widget w); // defined below
bool focus_on_named_input (vue_window win, string field); // defined below
string enum_widget_value (widget w);         // defined below

// an option of a printer as listed by lpoptions -l: "Key/Label: v1 *v2 v3"
// (the default is starred); the dialog shows it as an enum
struct printer_option {
  string key, label, def;
  array<string> values;
  widget choice; // the enum_widget of the dialog
};

// the options of a printer worth a choice: paper size, two-sided, color
static array<printer_option>
printer_options (string printer) {
  // lpoptions spawns a process: the result is cached per printer, so that
  // switching back and forth in the dialog does not block the layout again
  static hashmap<string,int> cached (-1);
  static array<array<printer_option> > store;
  int idx= cached[printer];
  if (idx >= 0) return store[idx];
  array<printer_option> opts;
  string cmd= "lpoptions -l 2>/dev/null";
  if (N(printer) > 0) cmd= "lpoptions -p " * escape_sh (printer) * " -l 2>/dev/null";
  array<string> lines= tokenize (var_eval_system (cmd), "\n");
  for (int i= 0; i < N(lines); i++) {
    int c= search_forwards (": ", lines[i]);
    int sl= search_forwards ("/", lines[i]);
    if (c < 0 || sl < 0 || sl > c) continue;
    printer_option o;
    o.key= lines[i] (0, sl);
    o.label= lines[i] (sl+1, c);
    if (o.key != "PageSize" && o.key != "Duplex" && o.key != "ColorModel") continue;
    array<string> vs= tokenize (trim_spaces (lines[i] (c+2, N(lines[i]))), " ");
    for (int j= 0; j < N(vs); j++) {
      string v= vs[j];
      if (N(v) == 0) continue;
      bool def= (v[0] == '*');
      if (def) v= v (1, N(v));
      // the paper sizes: the named ones only (not 209.9x329.49mm, Custom...)
      if (o.key == "PageSize" && (occurs (".", v) || occurs ("x", v) || v == "Custom")) continue;
      o.values << v;
      if (def) o.def= v;
    }
    if (N(o.values) > 1) opts << o;
  }
  cached (printer)= N(store);
  store << opts;
  return opts;
}

// print a file with the system spooler (lpr) with the settings chosen in
// the printer dialog, then run 'after'
class print_command_rep: public command_rep {
  url file;
  command after;
  widget printer, copies, pages;
  array<printer_option> options;
public:
  print_command_rep (url _file, command _after, widget _printer, widget _copies, widget _pages,
                     array<printer_option> _options):
    file (_file), after (_after), printer (_printer), copies (_copies), pages (_pages),
    options (_options) {}
  void apply () {
    string cmd= "lpr";
    string pr= enum_widget_value (printer);
    if (N(pr) > 0 && pr != translate ("Default printer")) cmd << " -P " << escape_sh (pr);
    int n= as_int (input_text_widget_string (copies));
    if (n > 1) cmd << " -# " << as_string (min (n, 99));
    string rg= input_text_widget_string (pages);
    string clean;
    for (int i= 0; i < N(rg); i++) // digits, commas and dashes only
      if ((rg[i] >= '0' && rg[i] <= '9') || rg[i] == ',' || rg[i] == '-') clean << rg[i];
    if (N(clean) > 0) cmd << " -o page-ranges=" << clean;
    for (int i= 0; i < N(options); i++) {
      string v= enum_widget_value (options[i].choice);
      if (N(v) > 0 && v != options[i].def) cmd << " -o " << options[i].key << "=" << escape_sh (v);
    }
    cmd << " " << escape_sh (concretize (file));
    if (DEBUG_VUE_WIDGETS) debug_widgets << "Running print command: " << cmd << LF;
    if (DEBUG_VUE_WIDGETS) debug_widgets << "print: " << cmd << LF;
    system (cmd);
    if (!is_nil (after)) after ();
  }
  tm_ostream& print (tm_ostream& out) { return out << "<print_command " << file << ">"; }
};

widget input_text_widget (command call_back, string type, array<string> def,
                          int style, string width);

// the printers known to the spooler (lpstat), after the default choice
static array<string>
system_printers () {
  array<string> ps;
  ps << translate ("Default printer");
  string out= var_eval_system ("lpstat -a 2>/dev/null");
  array<string> lines= tokenize (out, "\n");
  for (int i= 0; i < N(lines); i++) {
    array<string> a= tokenize (trim_spaces (lines[i]), " ");
    if (N(a) > 0 && N(a[0]) > 0 && a[0] != "lpstat:") ps << a[0];
  }
  return ps;
}

// the persistent inputs of the printer dialog (kept across the rebuilds of
// its contents when another printer is chosen)
static void
make_printer_inputs (widget& printer, widget& copies, widget& pages) {
  array<string> printers= system_printers ();
  printer= enum_widget (noop_command (), printers, printers[0], 0, "14em");
  array<string> one; one << string ("1");
  array<string> none; none << string ("");
  copies= input_text_widget (noop_command (), "copies", one, 0, "3em");
  pages= input_text_widget (noop_command (), "pages", none, 0, "8em");
}

// contents of the dialog shown by printer_widget: the printer, the number
// of copies, the pages (as lpr's page-ranges: "1-3,7"), the options of the
// chosen printer (lpoptions), Cancel/Print
static widget
make_printer_dialog (command cmd, url ps_pdf_file, widget printer, widget copies, widget pages) {
  string pr= enum_widget_value (printer);
  if (pr == translate ("Default printer")) pr= "";
  array<printer_option> options= printer_options (pr);
  array<widget> lhs, rhs;
  lhs << text_widget (translate ("Printer") * ":", 0, black)
      << text_widget (translate ("Copies") * ":", 0, black)
      << text_widget (translate ("Pages") * ":", 0, black);
  rhs << printer << copies
      << horizontal_list (array<widget> (pages, glue_widget (false, false, 8*PIXEL, 0),
                                         text_widget (translate ("all, or e.g. 1-3,7"), WIDGET_STYLE_GREY, black)));
  for (int i= 0; i < N(options); i++) {
    options[i].choice= enum_widget (noop_command (), options[i].values, options[i].def, 0, "14em");
    lhs << text_widget (translate (options[i].label) * ":", 0, black);
    rhs << options[i].choice;
  }
  array<widget> buttons;
  buttons << menu_button (text_widget (translate ("Cancel"), 0, black), cmd, "", "", WIDGET_STYLE_BUTTON)
          << glue_widget (false, false, 8*PIXEL, 0)
          << menu_button (text_widget (translate ("Print"), 0, black),
                          tm_new<print_command_rep> (ps_pdf_file, cmd, printer, copies, pages, options),
                          "", "", WIDGET_STYLE_BUTTON);
  array<widget> rows;
  rows << text_widget (translate ("Print document") * ": " * as_string (tail (ps_pdf_file)), 0, black)
       << glue_widget (false, false, 0, 8*PIXEL)
       << aligned_widget (lhs, rhs, 3*PIXEL, 3*PIXEL, 0, 0)
       << glue_widget (false, false, 0, 10*PIXEL)
       << horizontal_list (buttons);
  return vertical_list (rows);
}

// contents of the dialog shown by color_picker_widget
static widget
make_color_picker_dialog (command cmd, bool bg, array<tree> proposals) {
  (void) bg;
  static const char* standard[]= {
    "black", "dark grey", "grey", "light grey", "white",
    "red", "green", "blue", "yellow", "cyan", "magenta",
    "dark red", "dark green", "dark blue", "dark yellow", "dark cyan", "dark magenta",
    "orange", "brown", "pink", "pastel red", "pastel green", "pastel blue",
    "pastel yellow", "pastel cyan", "pastel magenta", "pastel orange", "pastel brown",
    NULL };
  array<widget> rows;
  if (N(proposals) > 0) {
    array<widget> swatches;
    for (int i=0; i<N(proposals); i++)
      swatches << menu_button (glue_widget (proposals[i], false, false, 12*PIXEL, 12*PIXEL),
                               applied_command (cmd, list_object (object (proposals[i]))),
                               "", "", 0);
    rows << text_widget (translate ("Recent colors"), 0, black)
         << tile_menu (swatches, 8);
  }
  array<widget> swatches;
  for (int i=0; standard[i] != NULL; i++) {
    tree col (standard[i]);
    swatches << menu_button (glue_widget (col, false, false, 12*PIXEL, 12*PIXEL),
                             applied_command (cmd, list_object (object (col))),
                             "", "", 0);
  }
  rows << text_widget (translate ("Colors"), 0, black)
       << tile_menu (swatches, 8)
       << glue_widget (false, false, 0, 10*PIXEL)
       << menu_button (text_widget (translate ("Cancel"), 0, black),
                       applied_command (cmd, list_object (object (false))), "", "", WIDGET_STYLE_BUTTON);
  return vertical_list (rows);
}


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
      vue_picture_widget pd { .p= load_xpm (d.us[i]), .file_name= d.us[i],
                              .stamp= icon_stamp () };
      icons << vue_create<vue_picture_widget> ("picture_widget", pd);
    }
    vue_tabs_widget_star dd { .tabs= d.ss, .icons= icons, .bodies= d.bs, .current= 0 };
    data= close_box (dd);
    return;
  }
  if (type == "pulldown_button") {
    // add more space in the struct for caching the widget
    vue_pulldown_button d= open_box<vue_pulldown_button> (data);
    widget cw;
    vue_cached_pull_button cd { d.w, d.pw, cw, false, false, 0, 0 };
    data= close_box (cd);
    return;
  }
  if (type == "pullright_button") {
    // add more space in the struct for caching the widget
    vue_pullright_button d= open_box<vue_pullright_button> (data);
    widget cw;
    vue_cached_pull_button cd { d.w, d.pw, cw, false, false, 0, 0 };
    data= close_box(cd);
    return;
  }
  if (type == "xpm_widget") {
    //VUE_WIDGET(xpm_widget, url, file_name);
    vue_xpm_widget d= open_box<vue_xpm_widget> (data);
    vue_picture_widget dd { .p= load_xpm (d.file_name), .file_name= d.file_name,
                            .stamp= icon_stamp () };
    data= close_box(dd);
    type= "picture_widget";
    return;
  }
  if (type == "colored_glue_widget") {
    vue_colored_glue_widget d= open_box<vue_colored_glue_widget> (data);
    picture p= native_picture (0,0, 0, 0); // empty cache
    type= "cached_glue_widget";
    data= close_box (vue_cached_glue_widget { .pic= p, .col= d.col, .hx= d.hx, .vx= d.vx, .w= d.w, .h= d.h });
    return;
  }
  if (type == "enum_widget") {
    vue_enum_widget d= open_box<vue_enum_widget> (data);
    vue_enum_widget_star dd { .cb= d.cb, .vals= d.vals, .val= d.val, .st= d.st, .w= d.w, .open= false };
    data= close_box (dd);
    return;
  }
  if (type == "hsplit_widget") {
    vue_hsplit_widget d= open_box<vue_hsplit_widget> (data);
    data= close_box (vue_split_widget_star { .a= d.l, .b= d.r, .pos= -1, .dragging= false });
    return;
  }
  if (type == "vsplit_widget") {
    vue_vsplit_widget d= open_box<vue_vsplit_widget> (data);
    data= close_box (vue_split_widget_star { .a= d.t, .b= d.b, .pos= -1, .dragging= false });
    return;
  }
  if (type == "filtered_choice_widget") {
    vue_filtered_choice_widget d= open_box<vue_filtered_choice_widget> (data);
    // the filter is edited in a text input, we read its contents directly during layout
    array<string> def (1);
    def[0]= d.filter;
    widget input= input_text_widget (noop_command (), "search-filter", def, 0, "24em");
    vue_filtered_choice_widget_star dd { .cb= d.cb, .vals= d.vals, .val= d.val, .input= input };
    data= close_box (dd);
    return;
  }
  if (type == "printer_widget") {
    vue_printer_widget d= open_box<vue_printer_widget> (data);
    widget printer, copies, pages;
    make_printer_inputs (printer, copies, pages);
    vue_printer_widget_star dd { .cmd= d.cmd, .ps_pdf_file= d.ps_pdf_file,
                                 .content= make_printer_dialog (d.cmd, d.ps_pdf_file, printer, copies, pages),
                                 .printer= printer, .copies= copies, .pages= pages,
                                 .options_for= enum_widget_value (printer) };
    data= close_box (dd);
    return;
  }
  if (type == "color_picker_widget") {
    vue_color_picker_widget d= open_box<vue_color_picker_widget> (data);
    vue_color_picker_widget_star dd { .cmd= d.cmd, .bg= d.bg, .proposals= d.proposals,
                                      .content= make_color_picker_dialog (d.cmd, d.bg, d.proposals) };
    data= close_box (dd);
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
  }
  if (type == "popup_widget") {
    //VUE_WIDGET(popup_widget, widget, w);
    vue_popup_widget d= open_box<vue_popup_widget> (data);
    if (s == SLOT_MOUSE_GRAB) {
      // the grab is implicit: our window is dismissed when the pointer leaves
      // (see SDL_EVENT_WINDOW_MOUSE_LEAVE in vue_gui.cpp)
      return;
    }
    d.w->send (s, val);
    return;
  }
  if (type == "printer_widget" || type == "color_picker_widget") {
    // these dialogs are windows created by the scheme side, nothing to do
    if (s == SLOT_VISIBILITY || s == SLOT_KEYBOARD_FOCUS) return;
  }
  vue_widget_rep::send (s, val);
}

void scroll_bar (Clay_ElementId &my_id, Clay_ScrollContainerData &scrollData, int16_t z= 1); // below

void
layout_pull_button (vue_ui_rep *w) {
  vue_cached_pull_button d= open_box<vue_cached_pull_button> (w->data);
  bool down= w->type == "pulldown_button";
  Clay_ElementId button_id= CLAY_SIDI(CLAY_TM_STRING(w->type), w->id);
  Clay_ElementId float_id=  CLAY_IDI("pull_button_float", w->id);
  Clay_Sizing s= layoutExpand;
  if (down) s= { CLAY_SIZING_FIT(.min=20) };
  ui_signal sig= button_logic (button_id);
  CLAY(button_id, {
    .layout= {
      .padding= CLAY_PADDING_ALL(5),
      .childGap= 4,
      .sizing= s,
      .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
    // flat: the bar or menu behind shows through unless hovered (the bars
    // of the main window have different greys)
    .backgroundColor= hot_id == button_id.id ? color_highlight : (Clay_Color) { 0, 0, 0, 0 } })
  {
    // items of vertical menus with check marks reserve their column
    if (!down && menu_has_marks)
      CLAY_AUTO_ID({ .layout= { .sizing= { CLAY_SIZING_FIXED(22), CLAY_SIZING_FIXED(22) }}}) {}
    concrete(d.w)->do_layout ();
    if (!down) {
      CLAY_AUTO_ID({ .layout= { .sizing= layoutExpand }}){};
      layout_text("<#25B8>", 0, black); // right arrow
    }
    if (sig.clicked == 1) {
      if (is_nil (d.cw)) {
        // we clicked an inactive button, we evaluate the promise
        d.cw= d.pw->eval ();
        d.placed= false;
        d.flip= false;
        d.shift_x= d.shift_y= 0;
        current_popup= true;
        away_time= 0;
        // only the buttons of a bar are mutually exclusive: a submenu
        // (pullright) belongs to the chain of the menu it is in, and
        // claiming the slot here would close its own parent
        if (down) open_pull_id= button_id.id;
      } else {
        // we clicked an active button, we go back to an inactive state
        d.cw= NULL;
        current_popup= false;
        if (down && open_pull_id == button_id.id) open_pull_id= 0;
      }
    } else if (current_popup) {
      // some other popup is active, we should be inactive
      d.cw= NULL;
    }
    else if (down && !is_nil (d.cw) &&
             open_pull_id != 0 && open_pull_id != button_id.id) {
      // another button of the bar opened its menu (it may have been laid
      // out after us, where neither cancel_popup nor current_popup reaches
      // us); our own submenus close with us
      d.cw= NULL;
    }
    // if we are active then we draw the float window
    if (!is_nil (d.cw)) {
      // when the menu (as laid out in the previous pass) sticks out of the
      // window, open it on the other side of the button if there is more
      // room there, otherwise shift it back inside; the decision is kept
      // until the menu closes to avoid flickering
      Clay_Dimensions dims= { current_window->layout_w, current_window->layout_h };
      Clay_ElementData fd= Clay_GetElementData (float_id);
      Clay_ElementData bd= Clay_GetElementData (button_id);
      if (fd.found && bd.found && !d.placed) {
        d.placed= true;
        Clay_BoundingBox f= fd.boundingBox, b= bd.boundingBox;
        float over_x= f.x + f.width  - dims.width;
        float over_y= f.y + f.height - dims.height;
        if (down) {
          if (over_y > 0) {
            if (b.y > dims.height - (b.y + b.height)) d.flip= true;
            else d.shift_y= -min (over_y, f.y);
          }
          if (over_x > 0) d.shift_x= -min (over_x, f.x);
        } else {
          if (over_x > 0) {
            if (b.x > dims.width - (b.x + b.width)) d.flip= true;
            else d.shift_x= -min (over_x, f.x);
          }
          if (over_y > 0) d.shift_y= -min (over_y, f.y);
        }
      }
      Clay_FloatingAttachPoints attach;
      if (down) attach= d.flip
        ? (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_BOTTOM, .parent= CLAY_ATTACH_POINT_LEFT_TOP }
        : (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_TOP, .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM };
      else attach= d.flip
        ? (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_RIGHT_TOP, .parent= CLAY_ATTACH_POINT_LEFT_TOP }
        : (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_TOP, .parent= CLAY_ATTACH_POINT_RIGHT_TOP };
      Clay_Vector2 offset= { d.shift_x, d.shift_y };
      // the menu is at most as tall as the window and scrolls (wheel or
      // scroll bar) when its contents are taller; it is drawn above the
      // scroll bars of the editors (zIndex 1)
      CLAY(float_id, {
        .floating= {
          .offset= offset,
          .zIndex= 5,
          .attachTo= CLAY_ATTACH_TO_PARENT,
          .attachPoints= attach },
        .layout= {
          .padding= { 8, 8, 8, 8 },
          .sizing= { .width= CLAY_SIZING_FIT(.min= 120),
                     .height= CLAY_SIZING_FIT(.max= dims.height) }},
        .backgroundColor= color_background,
        .clip= { .vertical= true, .childOffset= Clay_GetScrollOffset () },
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
  if (!is_nil (d.cw)) {
    Clay_ScrollContainerData sd= Clay_GetScrollContainerData (float_id);
    if (sd.found) scroll_bar (float_id, sd, 6);
  }
  // store back changes
  w->data= close_box (d);
}

static bool widget_grows (widget w, bool horizontal); // below

void
layout_menu (unsigned int id, array<widget> a, bool vert, uint16_t gap= 10) {
  // a menu fits its items and grows along an axis only when one of its items
  // does (a horizontal menu bar fills the height of its row); the items of a
  // vertical menu fill the width of the menu
  bool grows_main= false, grows_cross= false, marks= false;
  for (int i=0; i<N(a); i++) {
    grows_main=  grows_main  || widget_grows (a[i], !vert);
    grows_cross= grows_cross || widget_grows (a[i], vert);
    vue_ui_rep* u= dynamic_cast<vue_ui_rep*> (concrete (a[i]).rep);
    if (u != NULL && u->type == "menu_button" &&
        N(open_box<vue_menu_button> (u->data).pre) > 0) marks= true;
  }
  Clay_Sizing s= layoutFit;
  if (vert) {
    if (grows_cross) s.width=  CLAY_SIZING_GROW(0);
    if (grows_main)  s.height= CLAY_SIZING_GROW(0);
  } else {
    if (grows_main)  s.width=  CLAY_SIZING_GROW(0);
    s.height= CLAY_SIZING_GROW(0);
  }
  CLAY(vert ? CLAY_IDI("vertical_menu", id) : CLAY_IDI("horizontal_menu", id), {
    .layout= {
      .layoutDirection= vert ? CLAY_TOP_TO_BOTTOM : CLAY_LEFT_TO_RIGHT,
      .sizing= s,
      .childGap= gap,
      // a horizontal menu fills the height of its bar: its items (icons of
      // several sizes, texts, separators) are centered in it
      .childAlignment= { .y= vert ? CLAY_ALIGN_Y_TOP : CLAY_ALIGN_Y_CENTER } }})
  {
    bool save_grow= button_grow, save_marks= menu_has_marks;
    button_grow= vert;
    menu_has_marks= vert && marks;
    for (int i=0, n=N(a); i< n; i++) {
      concrete (a[i])->do_layout ();
    }
    button_grow= save_grow;
    menu_has_marks= save_marks;
  }
}

// Size policy: does the widget want the extra space along an axis?
// Containers grow along an axis only when one of their children does,
// so that lists of labels keep their size while lists with a resizable
// widget follow the window.
static bool
widget_grows (widget w, bool horizontal) {
  vue_widget_rep* r= concrete (w).rep;
  if (r == NULL) return false;
  string t= r->type;
  if (t == "simple_widget" || t == "user_canvas_widget" ||
      t == "hsplit_widget" || t == "vsplit_widget" ||
      t == "tabs_widget" || t == "icon_tabs_widget" ||
      t == "vue_texmacs_widget_rep") return true; // an embedded editor
  vue_ui_rep* u= dynamic_cast<vue_ui_rep*> (r);
  if (u == NULL) return false;
  array<widget> children;
  if (t == "resize_widget") {
    if (window_autosizing) return false; // fixed to the default size
    vue_resize_widget d= open_box<vue_resize_widget> (u->data);
    return horizontal ? d.w1 != d.w3 : d.h1 != d.h3;
  }
  else if (t == "glue_widget") {
    vue_glue_widget d= open_box<vue_glue_widget> (u->data);
    return horizontal ? d.hx : d.vx;
  }
  else if (t == "cached_glue_widget") {
    vue_cached_glue_widget d= open_box<vue_cached_glue_widget> (u->data);
    return horizontal ? d.hx : d.vx;
  }
  else if (t == "vertical_list")   children= open_box<vue_vertical_list> (u->data).a;
  else if (t == "horizontal_list") children= open_box<vue_horizontal_list> (u->data).a;
  else if (t == "vertical_menu")   children= open_box<vue_vertical_menu> (u->data).a;
  else if (t == "horizontal_menu") children= open_box<vue_horizontal_menu> (u->data).a;
  else if (t == "wrapped_widget")  children << open_box<vue_wrapped_widget> (u->data).w;
  else if (t == "division_widget") children << open_box<vue_division_widget> (u->data).w;
  else if (t == "extend_widget")   children << open_box<vue_extend_widget> (u->data).w;
  else if (t == "refreshable_widget")
    children << open_box<vue_refreshable_widget_star> (u->data).current;
  else if (t == "refresh_widget")
    children << open_box<vue_refresh_widget_star> (u->data).current;
  else return false;
  for (int i=0; i<N(children); i++)
    if (!is_nil (children[i]) && widget_grows (children[i], horizontal)) return true;
  return false;
}

void
layout_list (unsigned int id, array<widget> a, bool vert) {
  // a vertical list fills the width of its parent; otherwise a list grows
  // along an axis only when one of its items does
  bool grows_main= false, grows_cross= false;
  for (int i=0; i<N(a); i++) {
    grows_main=  grows_main  || widget_grows (a[i], !vert);
    grows_cross= grows_cross || widget_grows (a[i], vert);
  }
  Clay_Sizing s= layoutFit;
  if (vert) {
    s.width=  CLAY_SIZING_GROW(0);
    if (grows_main) s.height= CLAY_SIZING_GROW(0);
  } else {
    if (grows_main)  s.width=  CLAY_SIZING_GROW(0);
    if (grows_cross) s.height= CLAY_SIZING_GROW(0);
  }
  CLAY(vert ? CLAY_IDI("vertical_list", id) : CLAY_IDI("horizontal_list", id), {
     .layout= {
       .layoutDirection= vert ? CLAY_TOP_TO_BOTTOM : CLAY_LEFT_TO_RIGHT,
       .sizing= s,
       .childAlignment= { .y= vert ? CLAY_ALIGN_Y_TOP : CLAY_ALIGN_Y_CENTER } }})
  {
    for (int i=0, n=N(a); i< n; i++) {
      concrete (a[i])->do_layout ();
    }
  }
}

// see ScrollbarData in vue_gui.hpp and scrollbarData above

void
scroll_bar (Clay_ElementId &my_id, Clay_ScrollContainerData &scrollData, int16_t z) {
  // z: the bars are drawn above their container (which may itself float)
  Clay_Vector2 ratio= (Clay_Vector2) {
    scrollData.contentDimensions.width / scrollData.scrollContainerDimensions.width,
    scrollData.contentDimensions.height / scrollData.scrollContainerDimensions.height,
  };
  // the thumbs follow the common mouse protocol (button_logic): pressed
  // over a thumb, the pointer drags it until the button is released, even
  // outside the bar
  // vertical scroll bar
  if (scrollData.scrollContainerDimensions.height < scrollData.contentDimensions.height) {
    Clay_ElementId vsb_id= CLAY_IDI("ScrollBarV", my_id.id);
    CLAY(vsb_id, {
      .floating= {
        .attachTo= CLAY_ATTACH_TO_ELEMENT_WITH_ID,
        .offset= { .y= -(scrollData.scrollPosition->y / ratio.y) },
        .zIndex= z,
        .parentId= my_id.id,
        .attachPoints= {
          .element= CLAY_ATTACH_POINT_RIGHT_TOP,
          .parent=  CLAY_ATTACH_POINT_RIGHT_TOP }},
        .layout= {
          .sizing= {
            CLAY_SIZING_FIXED(24),
            CLAY_SIZING_FIXED(scrollData.scrollContainerDimensions.height / ratio.y) }},
        .backgroundColor= Clay_PointerOver (vsb_id)
          ? the_theme.scrollbar_hover : the_theme.scrollbar,
      .cornerRadius= CLAY_CORNER_RADIUS(12) }){};
    ui_signal vsig= button_logic (vsb_id);
    if (vsig.pressed == 1) {
      mouse_action= ""; // the press is ours, not the container's
      scrollbarData.vertical= true;
      scrollbarData.clickOrigin= (float) mouse_y;
      scrollbarData.positionOrigin= scrollData.scrollPosition->y;
    } else if (vsig.held && scrollbarData.vertical) {
      scrollData.scrollPosition->y= scrollbarData.positionOrigin + (scrollbarData.clickOrigin - mouse_y) * ratio.y;
      scrollData.scrollPosition->y= min ( max (scrollData.scrollPosition->y, -(max(scrollData.contentDimensions.height - scrollData.scrollContainerDimensions.height, 0.0f))), 0.0f);
    }
  }
  
  // horizontal scroll bar
  if (scrollData.scrollContainerDimensions.width < scrollData.contentDimensions.width) {
    Clay_ElementId hsb_id= CLAY_IDI("ScrollBarH", my_id.id);
    CLAY(hsb_id, {
      .floating= {
        .attachTo= CLAY_ATTACH_TO_ELEMENT_WITH_ID,
        .offset= { .x= -(scrollData.scrollPosition->x / ratio.x) },
        .zIndex= z,
        .parentId= my_id.id,
        .attachPoints= {
          .element= CLAY_ATTACH_POINT_LEFT_BOTTOM,
          .parent=  CLAY_ATTACH_POINT_LEFT_BOTTOM }},
        .layout= {
          .sizing= {
            CLAY_SIZING_FIXED(scrollData.scrollContainerDimensions.width / ratio.x),
            CLAY_SIZING_FIXED(24) }},
        .backgroundColor= Clay_PointerOver (hsb_id)
          ? the_theme.scrollbar_hover : the_theme.scrollbar,
      .cornerRadius= CLAY_CORNER_RADIUS(12) }){};
    ui_signal hsig= button_logic (hsb_id);
    if (hsig.pressed == 1) {
      mouse_action= "";
      scrollbarData.vertical= false;
      scrollbarData.clickOrigin= (float) mouse_x;
      scrollbarData.positionOrigin= scrollData.scrollPosition->x;
    } else if (hsig.held && !scrollbarData.vertical) {
      scrollData.scrollPosition->x= scrollbarData.positionOrigin + (scrollbarData.clickOrigin - mouse_x) * ratio.x;
      scrollData.scrollPosition->x= min ( max (scrollData.scrollPosition->x, -(max(scrollData.contentDimensions.width - scrollData.scrollContainerDimensions.width, 0.0f))), 0.0f);
    }
  }
}

string input_text_widget_string (widget w); // defined below

// the cross of the close buttons of the tools
static void
render_close_mark_fn (renderer ren, void* data, rectangle r) {
  (void) data;
  SI px= ren->pixel;
  SI w= r->x2 - r->x1, h= r->y2 - r->y1;
  SI x1= r->x1 + (SI) (0.32*w), x2= r->x1 + (SI) (0.68*w);
  SI y1= r->y1 + (SI) (0.32*h), y2= r->y1 + (SI) (0.68*h);
  ren->set_pencil (pencil (theme_color (the_theme.text), 2*px, cap_round));
  ren->line (x1, y1, x2, y2);
  ren->line (x1, y2, x2, y1);
}

// the mark in front of a menu item, from the 'pre' of menu_button:
// 1= check ("v"), 2= bullet ("*"), 3= circle ("o")
static void
render_menu_mark_fn (renderer ren, void* data, rectangle r) {
  int kind= (int) (intptr_t) data;
  SI px= ren->pixel;
  SI w= r->x2 - r->x1, h= r->y2 - r->y1;
  SI cx= (r->x1 + r->x2) / 2, cy= (r->y1 + r->y2) / 2;
  if (kind == 1) {
    array<SI> xs (3), ys (3);
    xs[0]= r->x1 + (SI) (0.22*w); ys[0]= r->y1 + (SI) (0.50*h);
    xs[1]= r->x1 + (SI) (0.42*w); ys[1]= r->y1 + (SI) (0.28*h);
    xs[2]= r->x1 + (SI) (0.78*w); ys[2]= r->y1 + (SI) (0.74*h);
    ren->set_pencil (pencil (black, 2*px, cap_round));
    ren->lines (xs, ys);
  }
  else {
    SI rad= min (w, h) / 5;
    ren->set_pencil (pencil (black, px));
    if (kind == 2) ren->fill_arc (cx-rad, cy-rad, cx+rad, cy+rad, 0, 360*64);
    else ren->arc (cx-rad, cy-rad, cx+rad, cy+rad, 0, 360*64);
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
    //VUE_WIDGET(division_widget, string, name, widget, w);
    // the CSS class names used by the scheme code (see the Qt themes in
    // misc/themes): title and subtitle bars of the tools, discrete texts,
    // section bars; "plain" and unknown names are transparent containers
    vue_division_widget d= open_box<vue_division_widget> (data);
    Clay_ElementId div_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    int saved_style= context_style;
    bool saved_title= in_title_bar;
    if (d.name == "title" || d.name == "title-bar") {
      // the header of a tool: a bold title on a framed bar, rounded on top
      context_style |= WIDGET_STYLE_BOLD;
      in_title_bar= true;
      CLAY(div_id, {
        .backgroundColor= { 208, 208, 208, 255 },
        .cornerRadius= { 6, 6, 0, 0 },
        .layout= {
          .padding= { 12, 8, 8, 8 },
          .childGap= 8,
          .sizing= { .width= CLAY_SIZING_GROW(0) },
          .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
        .border= { .width= { 1, 1, 1, 1 }, .color= color_border }})
      {
        concrete (d.w)->do_layout ();
      }
    }
    else if (d.name == "subtitle") {
      context_style |= WIDGET_STYLE_BOLD;
      CLAY(div_id, {
        .layout= {
          .padding= { 8, 8, 8, 4 },
          .sizing= { .width= CLAY_SIZING_GROW(0) }},
        .border= { .width= { .bottom= 1 }, .color= color_border }})
      {
        concrete (d.w)->do_layout ();
      }
    }
    else if (d.name == "discrete") {
      context_style |= WIDGET_STYLE_GREY;
      CLAY(div_id, { .layout= { .padding= { 4, 4, 2, 2 } }})
      {
        concrete (d.w)->do_layout ();
      }
    }
    else if (d.name == "sections" || d.name == "section-tabs") {
      // horizontal bars of section buttons: "sections" is a segmented bar of
      // buttons, "section-tabs" a row of tabs sitting on a line; the buttons
      // draw themselves according to section_bar (see menu_button)
      bool tabs= (d.name == "section-tabs");
      int saved_bar= section_bar;
      bool saved_active= section_active;
      section_bar= tabs ? 2 : 1;
      section_active= false;
      if (tabs) {
        context_style |= WIDGET_STYLE_GREY; // inactive tabs are dimmed
        CLAY(div_id, {
          .layout= {
            .padding= { 8, 8, 4, 0 },
            .sizing= { .width= CLAY_SIZING_GROW(0) },
            .childAlignment= { .y= CLAY_ALIGN_Y_BOTTOM }},
          .border= { .width= { .bottom= 1 }, .color= color_border }})
        {
          concrete (d.w)->do_layout ();
        }
      }
      else {
        CLAY(div_id, {
          .backgroundColor= { 204, 204, 204, 255 },
          .cornerRadius= CLAY_CORNER_RADIUS(7),
          .layout= {
            .padding= CLAY_PADDING_ALL(2),
            .sizing= { .width= CLAY_SIZING_FIT(0) },
            .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
          .border= { .width= { 1, 1, 1, 1 }, .color= color_border }})
        {
          concrete (d.w)->do_layout ();
        }
      }
      section_bar= saved_bar;
      section_active= saved_active;
    }
    else if (d.name == "active-section" || d.name == "section-active-tab") {
      // the selected entry of the bars above: a transparent wrapper, the
      // button inside draws itself as active
      bool saved_active= section_active;
      section_active= true;
      context_style &= ~WIDGET_STYLE_GREY;
      CLAY(div_id, {}) {
        concrete (d.w)->do_layout ();
      }
      section_active= saved_active;
    }
    else concrete (d.w)->do_layout (); // "plain" and others
    context_style= saved_style;
    in_title_bar= saved_title;
    return;
  }
  if (type == "aligned_widget") {
    //VUE_WIDGET(aligned_widget, array<widget>, lhs, array<widget>, rhs,
    //            SI, hsep, SI, vsep,
    //            SI, lpad, SI, rpad);
    vue_aligned_widget d= open_box<vue_aligned_widget> (data);
    // the two columns are independent Clay elements; to keep the rows aligned
    // each cell gets as minimal height the height of both cells of its row,
    // as measured in the previous layout pass
    int n= min (N(d.lhs), N(d.rhs));
    array<float> row_h (n);
    for (int i=0; i<n; i++) {
      Clay_ElementData l= Clay_GetElementData (probe_id ("aligned_widget_cell", id, 2*i));
      Clay_ElementData r= Clay_GetElementData (probe_id ("aligned_widget_cell", id, 2*i+1));
      row_h[i]= max (l.found ? l.boundingBox.height : 0.0f,
                     r.found ? r.boundingBox.height : 0.0f);
      // a new widget: this pass is not aligned yet, ask for another one
      if (!l.found || !r.found) layout_again= true;
    }
    CLAY(CLAY_IDI("aligned_widget", id), {
      .layout= {
        .padding= { (uint16_t) (retina_factor*d.lpad / PIXEL), (uint16_t) (retina_factor*d.rpad / PIXEL), 0, 0 },
        .layoutDirection= CLAY_LEFT_TO_RIGHT,
        .childGap= (uint16_t) (retina_factor*d.hsep / PIXEL),
        .sizing= { CLAY_SIZING_FIT(0), CLAY_SIZING_FIT(0) }}})
    {
      for (int col=0; col<2; col++) {
        array<widget>& cells= (col == 0) ? d.lhs : d.rhs;
        CLAY_AUTO_ID({
          .layout= {
            .layoutDirection= CLAY_TOP_TO_BOTTOM,
            .childGap= (uint16_t) (retina_factor*d.vsep / PIXEL),
            .childAlignment= { .x= (col == 0) ? CLAY_ALIGN_X_RIGHT : CLAY_ALIGN_X_LEFT }}})
        {
          for (int i=0; i<n; i++) {
            CLAY(probe_id ("aligned_widget_cell", id, 2*i + col), {
              .layout= {
                .sizing= { .height= CLAY_SIZING_FIT (.min= row_h[i]) },
                .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }}})
            {
              concrete (cells[i])->do_layout ();
            }
          }
        }
      }
    }
    return;
  }
  if (type == "tabs_widget" || type == "icon_tabs_widget") {
    //VUE_WIDGET(tabs_widget, array<widget>, tabs, array<widget>, bodies);
    //VUE_WIDGET(icon_tabs_widget, array<url>, us, array<widget>, ss, array<widget>, bs);
    // A tab bar above the page of the current tab. The widget fills the space
    // given by its container and its page area is never smaller than the
    // largest page, so that switching tabs does not change the layout (as in
    // the Widkit version): the hidden pages are laid out off-screen to be
    // measured, the current one contributes its natural size.
    vue_tabs_widget_star d= open_box<vue_tabs_widget_star> (data);
    int n= min (N(d.tabs), N(d.bodies));
    if (n == 0) return;
    if (d.current < 0 || d.current >= n) d.current= 0;
    int next= d.current;
    Clay_ElementId clay_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    const float pad= 14; // around the page
    float page_w= 0, page_h= 0;
    for (int i=0; i<n; i++) {
      Clay_ElementData ed= Clay_GetElementData (probe_id ("tabs_widget_page", id, i));
      if (ed.found) {
        page_w= max (page_w, ed.boundingBox.width);
        page_h= max (page_h, ed.boundingBox.height);
      }
    }
    // the icons of the tabs come in several sizes (20 and 32 pixels in the
    // preferences): they are centered in boxes of the largest size, so that
    // all the tabs have the same height
    float icon_w= 0, icon_h= 0;
    for (int i= 0; i < N(d.icons); i++) {
      vue_ui_rep* ir= dynamic_cast<vue_ui_rep*> (concrete (d.icons[i]).rep);
      if (ir == NULL || ir->type != "picture_widget") continue;
      picture ip= icon_picture (ir->data);
      icon_w= max (icon_w, (float) ip->get_width ());
      icon_h= max (icon_h, (float) ip->get_height ());
    }
    CLAY(clay_id, {
      .layout= {
        .layoutDirection= CLAY_TOP_TO_BOTTOM,
        .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_GROW(0) }}})
    {
      // the tab bar; the current tab is drawn connected to the page frame
      CLAY(CLAY_ID_LOCAL("tab_bar"), {
        .layout= {
          .layoutDirection= CLAY_LEFT_TO_RIGHT,
          .padding= { 10, 10, 6, 0 },
          .childGap= 4,
          .childAlignment= { .y= CLAY_ALIGN_Y_BOTTOM },
          .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_FIT(0) }}})
      {
        for (int i= 0; i< n; i++) {
          Clay_ElementId tab_id= CLAY_IDI_LOCAL("tab", i);
          if (button_logic (tab_id).clicked == 1) next= i;
          bool cur= (d.current == i);
          // the current tab is open at the bottom and merges with the page,
          // the other ones are framed and slightly lower
          Clay_Color bg= cur ? color_background
                       : ((hot_id == tab_id.id) ? color_highlight : the_theme.tab_inactive);
          Clay_ElementData td= Clay_GetElementData (tab_id);
          CLAY(tab_id, {
            .backgroundColor= bg,
            .cornerRadius= { 10, 10, 0, 0 },
            .layout= {
              .padding= { 20, 20, (uint16_t) (cur ? 10 : 8), (uint16_t) (cur ? 10 : 7) },
              .childGap= 10,
              .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
            .border= { .width= { 1, 1, 1, (uint16_t) (cur ? 0 : 1) }, .color= color_border }})
          {
            if (i < N(d.icons)) {
              CLAY_AUTO_ID({
                .layout= {
                  .sizing= { CLAY_SIZING_FIXED (icon_w), CLAY_SIZING_FIXED (icon_h) },
                  .childAlignment= { .x= CLAY_ALIGN_X_CENTER, .y= CLAY_ALIGN_Y_CENTER }}})
              {
                concrete (d.icons[i])->do_layout ();
              }
            }
            concrete (d.tabs[i])->do_layout ();
            if (cur && td.found) {
              // cover the top border of the page under the current tab
              CLAY_AUTO_ID({
                .backgroundColor= color_background,
                .layout= { .sizing= { CLAY_SIZING_FIXED (td.boundingBox.width - 2),
                                      CLAY_SIZING_FIXED (2) }},
                .floating= {
                  .offset= { 1, -1 },
                  .zIndex= 1,
                  .attachTo= CLAY_ATTACH_TO_PARENT,
                  .attachPoints= { .element= CLAY_ATTACH_POINT_LEFT_TOP,
                                   .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM },
                  .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH }}) {}
            }
          }
        }
      }
      // the page of the current tab
      CLAY(CLAY_ID_LOCAL("tab_area"), {
        .backgroundColor= color_background,
        .cornerRadius= { 0, 8, 8, 8 },
        .layout= {
          .padding= CLAY_PADDING_ALL((uint16_t) pad),
          .sizing= { .width=  CLAY_SIZING_GROW(.min= page_w + 2*pad),
                     .height= CLAY_SIZING_GROW(.min= page_h + 2*pad) }},
        .border= { .width= { 1, 1, 1, 1 }, .color= color_border }})
      {
        CLAY_AUTO_ID({
          .layout= { .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_GROW(0) }}})
        {
          concrete (d.bodies[d.current])->do_layout ();
        }
      }
      // the hidden pages, laid out off-screen only to be measured
      CLAY_AUTO_ID({
        .layout= { .layoutDirection= CLAY_TOP_TO_BOTTOM },
        .floating= {
          .offset= { -100000, -100000 },
          .attachTo= CLAY_ATTACH_TO_ROOT,
          .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH }})
      {
        for (int i=0; i<n; i++) {
          if (i == d.current) continue;
          CLAY(probe_id ("tabs_widget_page", id, i), {
            .layout= { .sizing= layoutFit }})
          {
            concrete (d.bodies[i])->do_layout ();
          }
        }
      }
    }
    d.current= next;
    data= close_box (d);
    return;
  }
  if (type == "menu_button") {
    //VUE_WIDGET(menu_button, widget, w, command, cmd, string, pre, string, ks, int, style);
    vue_menu_button d= open_box<vue_menu_button> (data);
    bool inert= (d.style & WIDGET_STYLE_INERT) != 0;
    Clay_ElementId button_id= CLAY_IDI ("menu_button", id);
    ui_signal sig { .clicked= 0 };
    if (!inert) sig= button_logic (button_id);
    // push buttons (dialogs) are framed, the flat buttons of menus and tool
    // bars are only highlighted when hovered or pressed
    bool push= (d.style & WIDGET_STYLE_BUTTON) != 0;
    // The colour and pattern palettes are tiles of explicit buttons whose
    // whole content is a coloured rectangle: the tile is the button, and a
    // push frame around it takes more room and more attention than the
    // colour it presents. They are drawn flat and close together, with just
    // enough room around one for the highlight to show when it is hovered.
    bool swatch= false;
    {
      vue_ui_rep* in= dynamic_cast<vue_ui_rep*> (concrete (d.w).rep);
      swatch= (in != NULL && (in->type == "cached_glue_widget" ||
                              in->type == "colored_glue_widget"));
      if (push && swatch) push= false;
    }
    bool pressed= (d.style & WIDGET_STYLE_PRESSED) != 0;
    bool hot= !inert && (hot_id == button_id.id);
    bool down= !inert && (active_id == button_id.id);
    if (in_title_bar) {
      // the "x" of the title bar of a tool: a round close button
      vue_ui_rep* lab= dynamic_cast<vue_ui_rep*> (concrete (d.w).rep);
      if (lab != NULL && lab->type == "text_widget" &&
          open_box<vue_text_widget> (lab->data).s == "x") {
        Clay_Color cbg= down ? color_button_down
                     : (hot ? color_button_hover : the_theme.shade[1]);
        // Clay emits the background rectangle of an element after its custom
        // command: the mark must be a child of the round button
        CLAY(button_id, {
          .backgroundColor= cbg,
          .cornerRadius= CLAY_CORNER_RADIUS(13),
          .layout= { .sizing= { CLAY_SIZING_FIXED(26), CLAY_SIZING_FIXED(26) }},
          .border= { .width= { 1, 1, 1, 1 }, .color= color_border }}) {
          CLAY_AUTO_ID({
            .layout= { .sizing= layoutExpand },
            .custom= { .customData= (void*) &render_close_mark_fn },
            .userData= NULL }) {}
        }
        if (sig.clicked == 1) {
          cancel_popup= true;
          cmd_list= list (d.cmd, cmd_list);
        }
        return;
      }
    }
    Clay_Sizing sz= { CLAY_SIZING_GROW(0), CLAY_SIZING_FIT(0) }; // items of vertical menus
    if (!button_grow) sz= { CLAY_SIZING_FIT (.min= push ? 70.0f : 20.0f) };
    Clay_Color bg= { 0, 0, 0, 0 }; // flat buttons show their container
    Clay_Padding padding= swatch ? CLAY_PADDING_ALL(2) : CLAY_PADDING_ALL(5);
    Clay_CornerRadius radius= CLAY_CORNER_RADIUS(4);
    Clay_BorderElementConfig border= {};
    bool tab_strip= false;
    if (push) {
      bg= down ? color_button_down : (hot ? color_button_hover : color_button);
      padding= { 14, 14, 6, 6 };
      radius= CLAY_CORNER_RADIUS(6);
      border= { .width= { 1, 1, 1, 1 }, .color= color_border };
    }
    else if (section_bar == 2) {
      // a tab of a "section-tabs" bar: the active one is framed and merges
      // with the area below (the line of the bar is covered by a strip)
      padding= { 12, 12, 6, 6 };
      radius= { 6, 6, 0, 0 };
      if (section_active) {
        bg= palette[3];
        border= { .width= { 1, 1, 1, 0 }, .color= color_border };
        tab_strip= true;
      }
      else if (down) bg= color_pressed;
      else if (hot) bg= { 236, 236, 236, 255 };
    }
    else if (section_bar == 1) {
      // a segment of a "sections" bar
      padding= { 12, 12, 4, 4 };
      radius= CLAY_CORNER_RADIUS(5);
      if (section_active) {
        bg= palette[3];
        border= { .width= { 1, 1, 1, 1 }, .color= color_border };
      }
      else if (down) bg= color_pressed;
      else if (hot) bg= { 220, 220, 220, 255 };
    }
    else if (down || pressed) bg= color_pressed;
    else if (hot) bg= color_highlight;
    Clay_ElementData bd= Clay_GetElementData (button_id);
    CLAY(button_id, {
      .layout= {
        .padding= padding,
        .childGap= 4,
        .sizing= sz,
        // the label of a menu item is aligned with the labels above and
        // below it, a push button and a colour cell are centered (the cells
        // of a tile are stretched to the width of the menu the tile is in)
        .childAlignment= { .x= (push || swatch) ? CLAY_ALIGN_X_CENTER
                                                : CLAY_ALIGN_X_LEFT,
                           .y= CLAY_ALIGN_Y_CENTER }},
      .backgroundColor= bg,
      .cornerRadius= radius,
      .border= border,
      // the highlight fades in and out (Clay animates the color change)
      .transition= { .handler= Clay_EaseOut, .duration= 0.12f,
                     .properties= CLAY_TRANSITION_PROPERTY_BACKGROUND_COLOR }
    }) {
      last_id= button_id;
      if ((menu_has_marks && !swatch) || N(d.pre) > 0) {
        // the column for the mark of the item: "v" (check), "*" or "o";
        // all items of a menu with marks reserve it so that labels align,
        // but the cells of a colour tile are not labels: reserving it in
        // each of them spread the palette by the width of a mark per column
        int kind= (d.pre == "v") ? 1 : (d.pre == "*") ? 2 : (d.pre == "o") ? 3 : 0;
        CLAY_AUTO_ID({
          .layout= { .sizing= { CLAY_SIZING_FIXED(22), CLAY_SIZING_FIXED(22) }},
          .custom= { .customData= (kind != 0) ? (void*) &render_menu_mark_fn : NULL },
          .userData= (void*) (intptr_t) kind }) {}
      }
      concrete(d.w)->do_layout ();
      if (N(d.ks) > 0) {
        // add shortcut
        CLAY_AUTO_ID({ .layout= { .sizing= layoutExpand }}) {}
        layout_text (d.ks, d.style, black);
      }
      if (tab_strip && bd.found) {
        // cover the bottom line of the bar under the active tab (the border
        // of the bar is drawn after its children, hence the z index)
        CLAY_AUTO_ID({
          .backgroundColor= bg,
          .layout= { .sizing= { CLAY_SIZING_FIXED (bd.boundingBox.width - 2),
                                CLAY_SIZING_FIXED (1) }},
          .floating= {
            .offset= { 1, -1 },
            .zIndex= 1,
            .attachTo= CLAY_ATTACH_TO_PARENT,
            .attachPoints= { .element= CLAY_ATTACH_POINT_LEFT_TOP,
                             .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM },
            .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH }}) {}
      }
    }
    if (sig.clicked == 1) {
      // close any active popup chain (see pull_widget)
      cancel_popup= true;
      if (DEBUG_VUE_WIDGETS) debug_widgets << "Click!! " << id << LF;
      cmd_list= list(d.cmd, cmd_list);
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
    layout_text (d.s, d.style, d.col); // grey/inert handled by layout_text
    if (debug_clay) cout << "text_widget " << id <<  "  [" << d.s << "] last_id: " << last_id.id << LF;
    return;
  }
  if (type == "menu_separator") {
    //VUE_WIDGET(menu_separator, bool, vertical);
    vue_menu_separator d= open_box<vue_menu_separator> (data);
    if (d.vertical) {
      CLAY(CLAY_IDI("menu_separator (v)", id), {
        .layout= {
          .sizing= { .height= CLAY_SIZING_GROW(0) },
          .padding= {5,5,5,5} },
        .border= {
          .width= { .left= 2 },
          .color=  { 150, 150, 150, 255 } } });
    } else {
      CLAY(CLAY_IDI("menu_separator (h)", id), {
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
    layout_text (d.name, d.style, dark_grey);
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
        CLAY_AUTO_ID({
          .backgroundColor= the_theme.balloon,
          .layout= { .padding= { 10, 10, 10, 10 } },
          .cornerRadius= CLAY_CORNER_RADIUS(4),
          .border= {
            .width= { 1, 1, 1, 1 },
            .color= the_theme.balloon_border },
          .floating= {
            .offset= { 0, 4 },
            .zIndex= 10,
            .parentId= target_id.id,
            .attachPoints= {
              .element= CLAY_ATTACH_POINT_LEFT_TOP,
              .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM },
            .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH,
            .attachTo= CLAY_ATTACH_TO_ELEMENT_WITH_ID }})
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
    picture p= icon_picture (data);
    SI w= p->get_width ();
    SI h= p->get_height ();
    // no background: Clay draws it after the custom command (the picture)
    CLAY_AUTO_ID({
      .layout= {
        .sizing= { CLAY_SIZING_FIXED( (float)w), CLAY_SIZING_FIXED( (float)h) } },
      .custom= { .customData=  vue_render_widget },
      .userData= render_ref () }) {};
    return;
  }
  if (type == "glue_widget") {
    //VUE_WIDGET(glue_widget, bool, hx, bool, vx, SI, w, SI, h);
    vue_glue_widget d= open_box<vue_glue_widget> (data);
    CLAY(CLAY_IDI("glue_widget", id), {
      .layout= {
        .sizing= {
          .width=  d.hx ? CLAY_SIZING_GROW( .min= (float) retina_factor*d.w/PIXEL)
                        : CLAY_SIZING_FIXED((float) retina_factor*d.w/PIXEL),
          .height= d.vx ? CLAY_SIZING_GROW( .min= (float) retina_factor*d.h/PIXEL)
                        : CLAY_SIZING_FIXED((float) retina_factor*d.h/PIXEL) }}}) {};
    return;
  }
  if (type == "cached_glue_widget") {
    //VUE_WIDGET(colored_glue_widget, tree, col, bool, hx, bool, vx, SI, w, SI, h);
    vue_cached_glue_widget d= open_box<vue_cached_glue_widget> (data);
    CLAY_AUTO_ID({
      //.id= CLAY_IDI("colored_glue_widget", id),
      .custom= { .customData=  vue_render_widget },
      .userData= render_ref (),
      .layout= {
        .sizing= {
          .width= d.hx  ? CLAY_SIZING_GROW( .min= (float) retina_factor*d.w/PIXEL)
                        : CLAY_SIZING_FIT( .min= (float) retina_factor*d.w/PIXEL),
          .height= d.vx ? CLAY_SIZING_GROW( .min= (float) retina_factor*d.h/PIXEL)
                        : CLAY_SIZING_FIT( .min= (float) retina_factor*d.h/PIXEL) }}}) {};
    return;
  }
  if (type == "tile_menu") {
    //VUE_WIDGET(tile_menu, array<widget>, a, int, cols);
    // a menu rendered as a table of cols columns wide & made up of widgets in a
    vue_tile_menu d= open_box<vue_tile_menu> (data);
    int c=0, n= N(d.a);
    // the cells of a tile keep their size: they are not the items of the
    // vertical menu the tile sits in, which stretch to its width (a palette
    // in a menu wider than itself would spread its colours apart)
    bool save_grow= button_grow;
    button_grow= false;
    CLAY_AUTO_ID({ .layout= {
      .layoutDirection= CLAY_TOP_TO_BOTTOM,
      .childGap= 2 }})
    {
      while (c < n) {
        CLAY_AUTO_ID({ .layout= {
          .layoutDirection= CLAY_LEFT_TO_RIGHT,
          .childGap= 2 }})
        {
          for (int i=0; i< d.cols; i++) {
            if (c == n) break;
            concrete (d.a[c])-> do_layout ();
            c++;
          }
        }
      }
    }
    button_grow= save_grow;
    return;
  }
  if (type == "toggle_widget") {
    // VUE_WIDGET(toggle_widget, command, cmd, bool, on, int, style);
    vue_toggle_widget d= open_box<vue_toggle_widget> (data);
    Clay_ElementId toggle_id= CLAY_IDI ("toggle_widget", id);
    bool inert= d.style & WIDGET_STYLE_INERT;
    if (!inert && (button_logic (toggle_id).clicked == 1)) {
      if (DEBUG_VUE_WIDGETS) debug_widgets << "Click toggle! [" << (d.on ? "X" : " ") << "]" << LF;
      d.on= !d.on;
      data= close_box (d);
      command c (tm_new<applied_command_rep> (d.cmd, list_object (object (d.on))));
      cmd_list= list (c, cmd_list);
    }
    // a check box, drawn by vue_ui_rep::render (smaller in the mini style)
    float box= (d.style & WIDGET_STYLE_MINI) ? 24 : 30;
    CLAY(toggle_id, {
      .layout= { .sizing= { CLAY_SIZING_FIXED(box), CLAY_SIZING_FIXED(box) }},
      .custom= { .customData= vue_render_widget },
      .userData= render_ref () }) {}
    return;
  }
  if (type == "enum_widget") {
    //VUE_WIDGET(enum_widget, command, cb, array<string>, vals, string, val, int, st, string, w);
    // a button showing the current value, with a dropdown list of the choices
    vue_enum_widget_star d= open_box<vue_enum_widget_star> (data);
    bool inert= (d.st & WIDGET_STYLE_INERT) != 0;
    Clay_ElementId enum_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    Clay_ElementId list_id= CLAY_IDI ("enum_widget_list", id);
    ui_signal sig { .clicked= 0 };
    if (!inert) sig= button_logic (enum_id);
    bool changed= false;
    if (sig.clicked == 1) { d.open= !d.open; changed= true; }
    Clay_Sizing sz= { CLAY_SIZING_FIT (.min= 40), CLAY_SIZING_FIT (0) };
    if (N(d.w) > 0) {
      SI w= decode_length (d.w, current_window, d.st);
      sz.width= CLAY_SIZING_FIXED ((float) retina_factor*w/PIXEL);
    }
    Clay_ElementData ed= Clay_GetElementData (enum_id);
    CLAY(enum_id, {
      .layout= { .sizing= sz, .padding= { 8, 8, 4, 4 }, .childGap= 4 },
      .backgroundColor= (!inert && hot_id == enum_id.id) ? color_highlight
                                                          : the_theme.shade[2],
      .border= { .width= { 1, 1, 1, 1 }, .color= palette[0] }})
    {
      layout_text (d.val, d.st, inert ? dark_grey : black);
      CLAY_AUTO_ID({ .layout= { .sizing= { .width= CLAY_SIZING_GROW(0) }}}) {}
      layout_text ("<#25BE>", 0, inert ? dark_grey : black); // down arrow
      if (d.open) {
        CLAY(list_id, {
          .floating= {
            .zIndex= 10,
            .attachTo= CLAY_ATTACH_TO_PARENT,
            .attachPoints= { .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM }},
          .layout= {
            .layoutDirection= CLAY_TOP_TO_BOTTOM,
            .padding= CLAY_PADDING_ALL(4),
            .sizing= { .width= CLAY_SIZING_FIT (.min= ed.found ? ed.boundingBox.width : 0) }},
          .backgroundColor= color_background,
          .border= { .width= { 1, 1, 1, 1 }, .color= { 150, 150, 150, 255 }}})
        {
          for (int i=0; i<N(d.vals); i++) {
            Clay_ElementId item_id= CLAY_IDI_LOCAL ("item", i);
            ui_signal isig= button_logic (item_id);
            bool active= (d.vals[i] == d.val);
            CLAY(item_id, {
              .layout= { .padding= { 8, 8, 4, 4 }, .sizing= { .width= CLAY_SIZING_GROW(0) }},
              .backgroundColor= (hot_id == item_id.id) ? color_highlight
                                : (active ? palette[2] : color_background) })
            {
              layout_text (d.vals[i], d.st, black);
            }
            if (isig.clicked == 1) {
              d.val= d.vals[i];
              d.open= false;
              changed= true;
              cmd_list= list (applied_command (d.cb, list_object (object (d.val))), cmd_list);
            }
          }
        }
        // dismiss the list when clicking somewhere else
        if (starts (mouse_action, "press-") &&
            !Clay_PointerOver (list_id) && !Clay_PointerOver (enum_id)) {
          d.open= false;
          changed= true;
        }
      }
    }
    if (changed) data= close_box (d);
    return;
  }
  if (type == "resize_widget") {
    //VUE_WIDGET(resize_widget, widget, w, int, style, string, w1, string, h1,
    //string, w2, string, h2, string, w3, string, h3,
    //string, hpos, string, vpos);
    //FIXME: implement
    vue_resize_widget d= open_box<vue_resize_widget> (data);
    SI minw, minh, defw, defh, maxw, maxh;
    minw= decode_length (d.w1, current_window, d.style);
    minh= decode_length (d.h1, current_window, d.style);
    defw= decode_length (d.w2, current_window, d.style);
    defh= decode_length (d.h2, current_window, d.style);
    maxw= decode_length (d.w3, current_window, d.style);
    maxh= decode_length (d.h3, current_window, d.style);
    // the default size is used while the window is sized to its contents,
    // afterwards the widget follows the size of the window within its limits
    // (the limits of the window itself are set in vue_plain_window_widget_rep)
    Clay_Sizing sizing;
    if (window_autosizing) {
      sizing.width=  CLAY_SIZING_FIXED ((float) retina_factor*defw/PIXEL);
      sizing.height= CLAY_SIZING_FIXED ((float) retina_factor*defh/PIXEL);
    } else {
      sizing.width=  CLAY_SIZING_GROW (.min= (float) retina_factor*minw/PIXEL, .max= (float) retina_factor*maxw/PIXEL);
      sizing.height= CLAY_SIZING_GROW (.min= (float) retina_factor*minh/PIXEL, .max= (float) retina_factor*maxh/PIXEL);
    }
    CLAY(CLAY_SIDI(CLAY_TM_STRING(type), id), {
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
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
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
      string s= "'(vertical (link " * d.tmwid * "))";
      eval ("(lazy-initialize-force)");
      object xwid_expanded= eval (s); // evaluated once, was evaluated twice
      object xwid= call ("menu-expand", xwid_expanded);
      static hashmap<object, widget> cache;
      if (d.curobj == xwid); // unchanged: keep the widget we already have
      else if (cache->contains (xwid)) {
        d.curobj= xwid;
        d.current= cache [xwid];
        data= close_box (d);
      }
      else {
        d.curobj= xwid;
        d.current= make_menu_widget (xwid_expanded);
        if (menu_caching) cache (xwid)= d.current;
        data= close_box (d);
      }
    }
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
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
    CLAY(my_id, {
      .layout= { .sizing= layoutExpand },
      .backgroundColor= color_field,
      .border= {
        .width= { 1, 1, 1, 1 },
        .color= color_border },
      .clip= {
        .horizontal= true, .vertical= true,
        .childOffset= Clay_GetScrollOffset () }})
    {
      concrete (d.wid)->do_layout ();
    }
    Clay_ScrollContainerData scrollData= Clay_GetScrollContainerData (my_id);
    //Clay_ElementData canvas_layout= Clay_GetElementData (my_id);
    if (scrollData.found) {
      scroll_bar (my_id, scrollData);
    }
    return;
  }
  if (type == "hsplit_widget" || type == "vsplit_widget") {
    //VUE_WIDGET(hsplit_widget, widget, l, widget, r);
    //VUE_WIDGET(vsplit_widget, widget, t, widget, b);
    // two panes with a draggable divider; until the divider has been moved
    // both panes share the space equally
    vue_split_widget_star d= open_box<vue_split_widget_star> (data);
    bool horiz= (type == "hsplit_widget");
    const float bar= 8;
    Clay_ElementId my_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    Clay_ElementId bar_id= CLAY_IDI ("splitter", id);
    Clay_ElementData ed= Clay_GetElementData (my_id);
    bool changed= false;
    if (!d.dragging && mouse_action == "press-left" && Clay_PointerOver (bar_id)) {
      d.dragging= true;
      mouse_action= "";
      changed= true;
    }
    if (d.dragging) {
      if (!(mouse_state & 1)) d.dragging= false;
      else if (ed.found) {
        float total= horiz ? ed.boundingBox.width : ed.boundingBox.height;
        float p= horiz ? mouse_x - ed.boundingBox.x : mouse_y - ed.boundingBox.y;
        d.pos= max (bar, min (p - bar/2, total - 2*bar));
      }
      changed= true;
    }
    Clay_Sizing first= layoutExpand, second= layoutExpand;
    if (d.pos >= 0) {
      if (horiz) first.width=  CLAY_SIZING_FIXED (d.pos);
      else       first.height= CLAY_SIZING_FIXED (d.pos);
    }
    CLAY(my_id, {
      .layout= {
        .sizing= layoutExpand,
        .layoutDirection= horiz ? CLAY_LEFT_TO_RIGHT : CLAY_TOP_TO_BOTTOM }})
    {
      CLAY_AUTO_ID({ .layout= { .sizing= first }}) {
        concrete (d.a)->do_layout ();
      }
      CLAY(bar_id, {
        .backgroundColor= (d.dragging || Clay_PointerOver (bar_id)) ? palette[0] : palette[3],
        .layout= {
          .sizing= {
            .width=  horiz ? CLAY_SIZING_FIXED (bar) : CLAY_SIZING_GROW(0),
            .height= horiz ? CLAY_SIZING_GROW(0) : CLAY_SIZING_FIXED (bar) }}}) {};
      CLAY_AUTO_ID({ .layout= { .sizing= second }}) {
        concrete (d.b)->do_layout ();
      }
    }
    if (changed) data= close_box (d);
    return;
  }
  if (type == "choice_widget") {
    //VUE_WIDGET(choice_widget, command, cb, array<string>, vals, array<string>, chosen, bool, flag);
    // flag is true when multiple selections are allowed
    vue_choice_widget d= open_box<vue_choice_widget> (data);
    bool changed= false;
    // an inert or greyed list is shown but does not react (as the Qt one);
    // the labels follow the mini, monospaced and bold flags
    bool inert= (d.style & (WIDGET_STYLE_INERT | WIDGET_STYLE_GREY)) != 0;
    int  lab_style= d.style & (WIDGET_STYLE_MINI | WIDGET_STYLE_MONOSPACED |
                               WIDGET_STYLE_BOLD | WIDGET_STYLE_CENTERED);
    CLAY(CLAY_SIDI(CLAY_TM_STRING(type), id), {
      .backgroundColor= inert ? color_background : color_field,
      .layout= {
        .layoutDirection=  CLAY_TOP_TO_BOTTOM,
        .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_FIT(0) },
        .childGap= 2 }
    }) {
      for (int i=0; i<N(d.vals); i++) {
        int j, n= N(d.chosen);
        for (j=0; j<n; j++)
          if (d.chosen[j] == d.vals[i]) break;
        bool active= (j < n);
        Clay_ElementId item_id= CLAY_IDI_LOCAL ("item", i);
        if (!inert && button_logic (item_id).clicked == 1) {
          if (d.flag) {
            // toggle the selection of this item
            if (active) d.chosen= append (range (d.chosen, 0, j), range (d.chosen, j+1, n));
            else d.chosen << d.vals[i];
          } else {
            d.chosen= array<string> (1);
            d.chosen[0]= d.vals[i];
          }
          active= !active || !d.flag;
          changed= true;
        }
        Clay_Color bg= inert ? color_background : color_field;
        if (active) bg= inert ? the_theme.selection_soft : the_theme.selection;
        else if (!inert && hot_id == item_id.id) bg= color_highlight;
        // the items of a mini list are tighter, as in the mini bars
        uint16_t pad_x= (d.style & WIDGET_STYLE_MINI) ? 4 : 8;
        uint16_t pad_y= (d.style & WIDGET_STYLE_MINI) ? 1 : 2;
        CLAY(item_id, {
          .layout= { .padding= { pad_x, pad_x, pad_y, pad_y },
                     .sizing= { .width= CLAY_SIZING_GROW(0) }},
          .backgroundColor= bg })
        {
          color col= active ? theme_color (the_theme.selection_text)
                            : theme_color (the_theme.text);
          if (inert && !active) col= theme_color (the_theme.text_grey);
          layout_text (d.vals [i], lab_style, col);
        }
      }
    }
    if (changed) {
      data= close_box (d);
      object l;
      if (d.flag) {
        l= null_object ();
        for (int i= N(d.chosen)-1; i>=0; i--) l= cons (object (d.chosen[i]), l);
      }
      else l= object (N(d.chosen) > 0 ? d.chosen[0] : string (""));
      cmd_list= list (applied_command (d.cb, list_object (l)), cmd_list);
    }
    return;
  }
  if (type == "filtered_choice_widget") {
    //VUE_WIDGET(filtered_choice_widget, command, cb, array<string>, vals, string, val, string, filter);
    // a text input for the filter above a scrollable list of the matching values
    vue_filtered_choice_widget_star d= open_box<vue_filtered_choice_widget_star> (data);
    string filter= input_text_widget_string (d.input);
    Clay_ElementId my_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    Clay_ElementId list_id= CLAY_IDI ("filtered_choice_list", id);
    bool changed= false;
    CLAY(my_id, {
      .layout= {
        .layoutDirection= CLAY_TOP_TO_BOTTOM,
        .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_GROW(0) },
        .childGap= 4 }})
    {
      // the input has a fixed width (24em): it is clipped to the width of
      // the list, which is that of the container (a resize box usually)
      CLAY_AUTO_ID({
        .layout= { .sizing= { .width= CLAY_SIZING_GROW(0) }},
        .clip= { .horizontal= true }})
      {
        concrete (d.input)->do_layout ();
      }
      // long values are clipped too, instead of widening the list
      CLAY(list_id, {
        .layout= {
          .layoutDirection= CLAY_TOP_TO_BOTTOM,
          .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_GROW(.min= 100) }},
        .backgroundColor= color_field,
        .border= { .width= { 1, 1, 1, 1 }, .color= color_border },
        .clip= { .horizontal= true, .vertical= true, .childOffset= Clay_GetScrollOffset () }})
      {
        for (int i=0; i<N(d.vals); i++) {
          if (N(filter) > 0 && !occurs (filter, d.vals[i])) continue;
          Clay_ElementId item_id= CLAY_IDI_LOCAL ("item", i);
          bool active= (d.vals[i] == d.val);
          if (button_logic (item_id).clicked == 1) {
            d.val= d.vals[i];
            active= true;
            changed= true;
          }
          Clay_Color bg= color_field;
          if (active) bg= the_theme.selection;
          else if (hot_id == item_id.id) bg= color_highlight;
          CLAY(item_id, {
            .layout= { .padding= { 8, 8, 2, 2 }, .sizing= { .width= CLAY_SIZING_GROW(0) }},
            .backgroundColor= bg })
          {
            layout_text (d.vals[i], 0, active ? theme_color (the_theme.selection_text)
                                              : theme_color (the_theme.text));
          }
        }
      }
    }
    Clay_ScrollContainerData scrollData= Clay_GetScrollContainerData (list_id);
    if (scrollData.found) scroll_bar (list_id, scrollData);
    if (changed) {
      data= close_box (d);
      cmd_list= list (applied_command (d.cb, list_object (object (d.val), object (filter))),
                      cmd_list);
    }
    return;
  }
  if (type == "popup_widget") {
    //VUE_WIDGET(popup_widget, widget, w);
    // the unmapping is handled by the popup window containing us
    vue_popup_widget d= open_box<vue_popup_widget> (data);
    concrete (d.w)->do_layout ();
    return;
  }
  if (type == "minibar_menu") {
    //VUE_WIDGET(minibar_menu, array<widget>, a);
    vue_minibar_menu d= open_box<vue_minibar_menu> (data);
    layout_menu (id, d.a, false, 2);
    return;
  }
  if (type == "empty_widget") {
    //VUE_WIDGET(empty_widget);
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= { .sizing= { CLAY_SIZING_FIXED(0), CLAY_SIZING_FIXED(0) }}}) {};
    return;
  }
  if (type == "extend_widget") {
    //VUE_WIDGET(extend_widget, widget, w, array<widget>, a);
    // the widgets in a are laid out off-screen (so that Clay culls them) and
    // their sizes, as measured in the previous layout pass, are used as
    // minimal size for w
    vue_extend_widget d= open_box<vue_extend_widget> (data);
    Clay_ElementId my_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    float min_w= 0, min_h= 0;
    for (int i=0; i<N(d.a); i++) {
      Clay_ElementData ed= Clay_GetElementData (probe_id ("extend_widget_probe", id, i));
      if (ed.found) {
        min_w= max (min_w, ed.boundingBox.width);
        min_h= max (min_h, ed.boundingBox.height);
      }
    }
    CLAY(my_id, {
      .layout= { .sizing= { CLAY_SIZING_FIT (.min= min_w), CLAY_SIZING_FIT (.min= min_h) }}})
    {
      concrete (d.w)->do_layout ();
      CLAY_AUTO_ID({
        .layout= { .layoutDirection= CLAY_TOP_TO_BOTTOM },
        .floating= {
          .offset= { -100000, -100000 },
          .attachTo= CLAY_ATTACH_TO_ROOT,
          .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH }})
      {
        for (int i=0; i<N(d.a); i++) {
          CLAY(probe_id ("extend_widget_probe", id, i), {
            .layout= { .sizing= layoutFit }})
          {
            concrete (d.a[i])->do_layout ();
          }
        }
      }
    }
    return;
  }
  if (type == "wait_widget") {
    //VUE_WIDGET(wait_widget, SI, width, SI, height, string, message);
    vue_wait_widget d= open_box<vue_wait_widget> (data);
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= {
        .sizing= { CLAY_SIZING_FIXED ((float) retina_factor*d.width/PIXEL),
                   CLAY_SIZING_FIXED ((float) retina_factor*d.height/PIXEL) },
        .layoutDirection= CLAY_TOP_TO_BOTTOM,
        .childGap= 8,
        .childAlignment= { CLAY_ALIGN_X_CENTER, CLAY_ALIGN_Y_CENTER }},
      .backgroundColor= { 255, 255, 160, 255 },
      .border= { .width= { 1, 1, 1, 1 }, .color= { 0, 0, 0, 255 }}})
    {
      layout_text (upcase_all (translate ("please wait")), WIDGET_STYLE_BOLD, black);
      if (N(d.message) > 0) layout_text (d.message, 0, black);
    }
    return;
  }
  if (type == "printer_widget") {
    //VUE_WIDGET(printer_widget, command, cmd, url, ps_pdf_file);
    vue_printer_widget_star d= open_box<vue_printer_widget_star> (data);
    if (enum_widget_value (d.printer) != d.options_for) {
      // another printer: its options replace the previous ones (the inputs
      // for the printer, the copies and the pages are kept)
      d.options_for= enum_widget_value (d.printer);
      d.content= make_printer_dialog (d.cmd, d.ps_pdf_file, d.printer, d.copies, d.pages);
      data= close_box (d);
      layout_again= true; // the window is sized to the new contents
    }
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= { .padding= CLAY_PADDING_ALL(16), .sizing= layoutFit }})
    {
      concrete (d.content)->do_layout ();
    }
    return;
  }
  if (type == "color_picker_widget") {
    //VUE_WIDGET(color_picker_widget, command, cmd, bool, bg, array<tree>, proposals);
    vue_color_picker_widget_star d= open_box<vue_color_picker_widget_star> (data);
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= { .padding= CLAY_PADDING_ALL(16), .sizing= layoutFit }})
    {
      concrete (d.content)->do_layout ();
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
  if (type == "toggle_widget") {
    // a rounded box, filled with the accent color and a check mark when on
    vue_toggle_widget d= open_box<vue_toggle_widget> (data);
    vue_render_ren_data* rd= (vue_render_ren_data*) render_data;
    renderer ren= rd->ren;
    rectangle r= rd->r;
    bool inert= (d.style & WIDGET_STYLE_INERT) != 0;
    bool hot= (hot_id == CLAY_IDI ("toggle_widget", id).id);
    SI px= ren->pixel, m= 4*px, rad= 4*px; // 22px box in a 30px cell
    SI x1= r->x1 + m, y1= r->y1 + m, x2= r->x2 - m, y2= r->y2 - m;
    color fill= d.on ? (inert ? rgb_color (160, 170, 200) : rgb_color (70, 110, 220))
                     : (hot ? rgb_color (255, 255, 255) : rgb_color (248, 248, 248));
    color edge= d.on ? fill : rgb_color (inert ? 190 : 150, inert ? 190 : 150, inert ? 190 : 150);
    ren->set_pencil (pencil (fill, px));
    ren->rounded_rectangle (x1, y1, x2, y2, rad, rad, rad, rad, true);
    ren->set_pencil (pencil (edge, px));
    ren->rounded_rectangle (x1, y1, x2, y2, rad, rad, rad, rad, false);
    if (d.on) {
      SI w= x2 - x1, h= y2 - y1;
      array<SI> xs (3), ys (3);
      xs[0]= x1 + (SI) (0.22*w); ys[0]= y1 + (SI) (0.50*h);
      xs[1]= x1 + (SI) (0.42*w); ys[1]= y1 + (SI) (0.27*h);
      xs[2]= x1 + (SI) (0.78*w); ys[2]= y1 + (SI) (0.74*h);
      ren->set_pencil (pencil (white, 3*px, cap_round));
      ren->lines (xs, ys);
    }
    return;
  }
  if (type == "picture_widget") {
    current_window->draw_picture (render_data, icon_picture (data));
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
  if (DEBUG_VUE_WIDGETS) debug_widgets << "vue_widget_rep::send(), unhandled " << slot_name (s)
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
    if (DEBUG_VUE_WIDGETS) debug_widgets << "vue_widget_rep::query(), unhandled " << slot_name (s)
    << " for widget of type: " << type << LF;
  }
  return fake_query (s, type_id);
}

widget
vue_widget_rep::read (slot s, blackbox index)  {
  (void) index;
  if (DEBUG_VUE_WIDGETS) debug_widgets << "vue_widget_rep::read(), unhandled " << slot_name (s)
       << " for widget of type: " << type << LF;
  return empty_widget ();
}

void
vue_widget_rep::write (slot s, blackbox index, widget w)  {
  (void) index; (void) w;
  if (DEBUG_VUE_WIDGETS) debug_widgets << "vue_widget_rep::write(), unhandled " << slot_name (s)
       << " for widget of type: " << type << LF;
}

void
vue_widget_rep::notify (slot s, blackbox new_val) {
  if (DEBUG_VUE_WIDGETS) debug_widgets << "vue_widget_rep::notify(), unhandled " << slot_name (s)
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


// The input fields which exist at the moment. SLOT_KEYBOARD_FOCUS_ON names
// one of them ("search", "replace-what", "spell"...) and there is no way to
// walk the widget tree of a window down to it, so they register themselves;
// Qt looks the widget up by its object name in the same spirit.
static hashset<pointer> live_inputs;

class vue_input_text_widget_rep : public vue_widget_rep {
public:
  string  s;           // the string being entered
  string  type;        // expected type of string
  string  name;        // optional name of the input field
  string  serial;      // optional serial number of the input field
  array<string> def;   // default possible input values
  command call_back;   // routine called on <return> or <escape>
  int     style;       // style of widget
  bool    greyed;      // greyed input
  string  width;       // width of input field
  bool    ok;          // input not canceled
  bool    done;        // call back has been called
  int     def_cur;     // current choice between default possible values
  int     pos;         // cursor position (index in s)
  int     sel;         // anchor of the selection (index in s), -1 for none
  SI      scroll;      // how much the text is scrolled to the left (SI)
  string  pre_edit;     // the text an input method is composing, shown at
  int     pre_edit_pos; // the cursor inside it (a byte offset)
  array<string> tabs;  // tab completions
  int     tab_nr;      // currently visible tab-completion
  int     tab_pos;     // cursor position where tab was pressed

  command tab_cb; // called with #t/#f on tab/shift-tab (moves the focus in dialogs)
  
  vue_input_text_widget_rep (command _call_back, string _type, array<string> _def,
                             int _style, string _width);
  ~vue_input_text_widget_rep ();
  void set_type (string t);
  bool continuous ();
  string display ();
  font  get_font ();
  SI    prefix_width (string ds, int n);
  int   position_at (SI x);
  bool  selection (int& b, int& e);
  void  delete_selection ();
  void  insert (string ins);
  void  copy_selection (bool cut);
  void  paste ();
  void  word_left ();
  void  word_right ();
  void do_layout ();
  void render (void *data);
  bool process_key (string);
};

// The input field of the dialogs (as the Widkit one): a lowered box, pastel
// when it has the focus, with the text scrolled so that the red cursor stays
// visible, a selection (shift+arrows, the mouse), the clipboard (M-c/M-x/M-v
// or C-y), word moves (A-/C- arrows, A-backspace), the history of the
// proposals (up/down), tab completion, return commits and escape cancels.
// It is drawn by render (a Clay custom element).

static const int input_pad_x= 6, input_pad_y= 3; // in device pixels

vue_input_text_widget_rep::vue_input_text_widget_rep (command _call_back,
          string _type, array<string> _def, int _style, string _width)
  : vue_widget_rep ("input_text_widget"),
    type ("default"), name ("default"), serial ("default"),
    def (_def), call_back (_call_back), style (_style),
    greyed ((_style & WIDGET_STYLE_INERT) != 0), width (_width),
    ok (true), done (false), def_cur (0), pos (0), sel (-1), scroll (0),
    pre_edit (""), pre_edit_pos (0), tab_nr (0), tab_pos (0)
{
  set_type (_type);
  if (N(def) > 0) {
    s= copy (def[0]);
    pos= N(s); // the cursor starts at the end of the default input
  }
  live_inputs->insert ((pointer) this);
}

vue_input_text_widget_rep::~vue_input_text_widget_rep () {
  live_inputs->remove ((pointer) this);
}

// "name#serial:type" as the Widkit and Qt inputs understand it
void
vue_input_text_widget_rep::set_type (string t) {
  int i= search_forwards (":", 0, t);
  if (i >= 0) {
    type= t (i+1, N(t));
    name= t (0, i);
    int j= search_forwards ("#", 0, name);
    if (j >= 0) {
      serial= name (j+1, N(name));
      name  = name (0, j);
    }
  }
  else type= t;
}

bool
vue_input_text_widget_rep::continuous () {
  return
    starts (type, "search") ||
    starts (type, "replace-") ||
    starts (type, "spell") ||
    starts (serial, "form-");
}

// the string as displayed (passwords are hidden)
string
vue_input_text_widget_rep::display () {
  if (type != "password") return s;
  string ds= copy (s);
  for (int i=0; i<N(ds); i++) ds[i]= '*';
  return ds;
}

font
vue_input_text_widget_rep::get_font () {
  return get_default_styled_font (style & (WIDGET_STYLE_MINI | WIDGET_STYLE_MONOSPACED));
}

// the width (SI of the window renderer) of the first n bytes of ds; the
// fonts are measured at three times the resolution (see layout_text_box)
SI
vue_input_text_widget_rep::prefix_width (string ds, int n) {
  if (n <= 0) return 0;
  metric ex;
  get_font ()->var_get_extents (ds (0, n), ex);
  return (ex->x2 - ex->x1) / 3;
}

// the position in s of the character boundary nearest to x (SI, relative
// to the start of the text, scroll included)
int
vue_input_text_widget_rep::position_at (SI x) {
  string ds= display ();
  int p= 0, prev= 0;
  SI old= 0;
  while (p < N(ds)) {
    prev= p;
    tm_char_forwards (ds, p);
    SI w= prefix_width (ds, p);
    if ((old + w) / 2 > x) return prev;
    old= w;
  }
  return N(ds);
}

bool
vue_input_text_widget_rep::selection (int& b, int& e) {
  if (sel < 0 || sel == pos || sel > N(s)) return false;
  b= min (sel, pos); e= max (sel, pos);
  return true;
}

void
vue_input_text_widget_rep::delete_selection () {
  int b, e;
  if (!selection (b, e)) { sel= -1; return; }
  s= s (0, b) * s (e, N(s));
  pos= b;
  sel= -1;
}

void
vue_input_text_widget_rep::insert (string ins) {
  delete_selection ();
  s= s (0, pos) * ins * s (pos, N(s));
  pos += N(ins);
}

void
vue_input_text_widget_rep::copy_selection (bool cut) {
  int b, e;
  if (!selection (b, e)) return;
  string t= tm_decode (s (b, e));
  // "primary" is the system clipboard of the Vue GUI (see vue_gui.cpp)
  set_selection ("primary", tuple ("extern", t), t, t, "", "verbatim");
  if (cut) delete_selection ();
}

void
vue_input_text_widget_rep::paste () {
  tree t; string str;
  (void) get_selection ("primary", t, str, "verbatim");
  string ins;
  if (is_tuple (t, "extern", 1)) ins= tm_encode (as_string (t[1]));
  else if (N(str) > 0) ins= tm_encode (str);
  if (N(ins) == 0) return;
  // a single line: the newlines of the clipboard become spaces
  ins= replace (ins, "\n", " ");
  insert (ins);
}

static bool
is_word_char (string s, int i) {
  if (i < 0 || i >= N(s)) return false;
  unsigned char c= (unsigned char) s[i];
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
         c == '_' || c >= 128 || c == '<' || c == '>';
}

void
vue_input_text_widget_rep::word_left () {
  while (pos > 0 && !is_word_char (s, pos-1)) tm_char_backwards (s, pos);
  while (pos > 0 && is_word_char (s, pos-1)) tm_char_backwards (s, pos);
}

void
vue_input_text_widget_rep::word_right () {
  while (pos < N(s) && !is_word_char (s, pos)) tm_char_forwards (s, pos);
  while (pos < N(s) && is_word_char (s, pos)) tm_char_forwards (s, pos);
}

#ifdef OS_WIN32
#define URL_CONCATER  '\\'
#else
#define URL_CONCATER  '/'
#endif

bool
vue_input_text_widget_rep::process_key (string key) {
  if (greyed) return false;
  if (starts (key, "pre-edit:")) {
    // An input method is composing (a dead key, a CJK method): the text is
    // shown at the cursor until it is committed, when it arrives as an
    // ordinary text event. The key is "pre-edit:<cursor>:<text>", the
    // cursor counted in characters, and an empty text ends the composition
    // (the same format the editor receives, see edit_keyboard.cpp).
    string k= key (9, N(key));
    int i= 0, n= N(k);
    while (i < n && k[i] != ':') i++;
    pre_edit= (i < n) ? k (i+1, n) : string ("");
    int chars= (i < n && is_int (k (0, i))) ? as_int (k (0, i)) : 0;
    pre_edit_pos= 0;
    for (int j= 0; j < chars && pre_edit_pos < N(pre_edit); j++)
      tm_char_forwards (pre_edit, pre_edit_pos);
    // what is composed replaces the selection once it is committed
    if (N(pre_edit) > 0) sel= -1;
    return true;
  }
  pre_edit= ""; pre_edit_pos= 0; // any other key ends a composition

  while ((N(key) >= 5) && (key(0,3) == "Mod") && (key[4] == '-') &&
         (key[3] >= '1') && (key[3] <= '5')) key= key (5, N(key));
  if (key == "space") key= " ";
  if (key == "<") key= "<less>";
  if (key == ">") key= "<gtr>";

  // the modifiers of the movement keys: S- extends the selection, A-/C-
  // move by words (the prefixes come in the order M- A- C- S-, see lookup_key)
  bool shift= false, word= false, cmd= false;
  string base= key;
  while (true) {
    if (starts (base, "M-")) { cmd= true; base= base (2, N(base)); }
    else if (starts (base, "A-")) { word= true; base= base (2, N(base)); }
    else if (starts (base, "C-") && (ends (base, "left") || ends (base, "right"))) { word= true; base= base (2, N(base)); }
    else if (starts (base, "S-")) { shift= true; base= base (2, N(base)); }
    else break;
  }
  bool movement= (base == "left" || base == "right" || base == "home" || base == "end");
  if (movement && !cmd) {
    if (shift) { if (sel < 0) sel= pos; } else sel= -1;
    if (base == "left")       { if (word) word_left ();  else if (pos > 0) tm_char_backwards (s, pos); }
    else if (base == "right") { if (word) word_right (); else if (pos < N(s)) tm_char_forwards (s, pos); }
    else if (base == "home")  pos= 0;
    else                      pos= N(s);
    tabs= array<string> (0);
    return true;
  }
  // any key but tab ends a tab completion (the stored tab_pos and the
  // proposals belong to the string as it was when tab was pressed)
  if (key != "tab" && key != "S-tab") {
    tabs= array<string> (0);
    tab_nr= 0;
    tab_pos= 0;
  }
  // the clipboard and the selection
  if (key == "M-a") { sel= 0; pos= N(s); return true; }
  if (key == "M-c") { copy_selection (false); return true; }
  if (key == "M-x") { copy_selection (true); goto changed; }
  if (key == "M-v" || key == "C-y") { paste (); goto changed; }
  if (key == "A-backspace" || key == "C-w") {
    int b, e;
    if (selection (b, e)) delete_selection ();
    else { int end= pos; word_left (); s= s (0, pos) * s (end, N(s)); }
    goto changed;
  }
  
  /* tab order (in dialogs) or tab-completion */
  if ((key == "tab" || key == "S-tab") && !is_nil (tab_cb)) {
    cmd_list= list (applied_command (tab_cb, list_object (object (key == "tab"))), cmd_list);
    return true;
  }
  if (continuous ());
  else if ((key == "tab" || key == "S-tab") && N(tabs) != 0) {
    int d=  (key == "tab"? 1: N(tabs)-1);
    tab_nr= (tab_nr + d) % N(tabs);
    s=      s (0, tab_pos) * tabs[tab_nr];
    pos=    N(s);
    sel= -1;
    return true;
  }
  else if (key == "tab" || key == "S-tab") {
    if (pos != N(s)) return false;
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
    sel= -1;
    return true;
  }
  else {
    tabs=    array<string> (0);
    tab_nr=  0;
    tab_pos= 0;
  }
  
  /* other actions */
  if (continuous () &&
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
    if (!continuous ()) {
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
    command cmd= tm_new<applied_command_rep>(call_back,
                  list_object (object (false)));
    cmd_list= list(cmd, cmd_list);
    return true;
  }
  else if ((key == "C-b")) { sel= -1; if (pos>0) tm_char_backwards (s, pos); }
  else if ((key == "C-f")) { sel= -1; if (pos<N(s)) tm_char_forwards (s, pos); }
  else if ((key == "C-a")) { sel= -1; pos=0; }
  else if ((key == "C-e")) { sel= -1; pos=N(s); }
  else if ((key == "up") || (key == "C-p")) {
    if (N(def) > 0) {
      def_cur= (def_cur+1) % N(def);
      s=       copy (def[def_cur]);
      pos=     N(s);
      sel= -1;
    }
  }
  else if ((key == "down") || (key == "C-n")) {
    if (N(def) > 0) {
      def_cur= (def_cur+N(def)-1) % N(def);
      s=       copy (def[def_cur]);
      pos=     N(s);
      sel= -1;
    }
  }
  else if (key == "C-k") { sel= -1; s= s (0, pos); }
  else if ((key == "C-d") || (key == "delete")) {
    int b, e;
    if (selection (b, e)) delete_selection ();
    else if ((pos<N(s)) && (N(s)>0)) {
      int end= pos;
      tm_char_forwards (s, end);
      s= s (0, pos) * s (end, N(s));
    }
  }
  else if (key == "backspace" || key == "S-backspace") {
    int b, e;
    if (selection (b, e)) delete_selection ();
    else if (pos>0) {
      int end= pos;
      tm_char_backwards (s, pos);
      s= s (0, pos) * s (end, N(s));
    }
  }
  else if (key == "C-backspace") {
    s= "";
    pos= 0;
    sel= -1;
  }
  else {
    if (starts (key, "<#"));
    else if (key == "<less>" || key == "<gtr>");
    else {
      if (N(key)!=1) return false;
      int i (key[0]);
      if ((i>=0) && (i<32)) return false;
    }
    insert (key);
  }
changed:
  if (continuous ()) {
    command cmd= tm_new<applied_command_rep>(call_back,
                      list_object (list_object (object (s), object (key))));
    cmd_list= list(cmd, cmd_list);
  }
  return true;
}

void
vue_input_text_widget_rep::render (void *data) {
  vue_render_ren_data* d= (vue_render_ren_data*) data;
  renderer ren= d->ren;
  rectangle r= d->r;
  SI px= ren->pixel;
  bool focused= (current_window != NULL && current_window->kbd_focus == this);
  // the box: pastel when focused, a lowered border
  color bg= greyed ? theme_color (the_theme.shade[1])
          : focused ? theme_color (the_theme.field_focused)
                    : theme_color (the_theme.shade[2]);
  ren->set_pencil (pencil (bg));
  ren->fill (r->x1, r->y1, r->x2, r->y2);
  // the lowered border: darker above and to the left, lighter below
  ren->set_pencil (pencil (theme_color (the_theme.border)));
  ren->fill (r->x1, r->y2 - px, r->x2, r->y2);
  ren->fill (r->x1, r->y1, r->x1 + px, r->y2);
  ren->set_pencil (pencil (theme_color (the_theme.shade[3])));
  ren->fill (r->x1, r->y1, r->x2, r->y1 + px);
  ren->fill (r->x2 - px, r->y1, r->x2, r->y2);
  // the text, scrolled so that the cursor stays visible (with a margin);
  // what an input method is composing is shown at the cursor, so it is
  // spliced into the string which is drawn and measured
  font fn= get_font ();
  string ds= display ();
  int pre_n= N(pre_edit);
  if (pre_n > 0) ds= ds (0, pos) * pre_edit * ds (pos, N(ds));
  metric ex;
  fn->var_get_extents (ds, ex);
  SI x0= r->x1 + input_pad_x * px, x1= r->x2 - input_pad_x * px;
  SI inner= x1 - x0;
  SI text_w= (ex->x2 - ex->x1) / 3;
  SI cur= prefix_width (ds, pos + (pre_n > 0 ? pre_edit_pos : 0));
  SI marge= inner / 4;
  if (cur - scroll > inner - marge) scroll= cur + marge - inner;
  if (cur - scroll < marge) scroll= cur - marge;
  if (scroll > text_w - inner) scroll= text_w - inner;
  if (scroll < 0) scroll= 0;
  SI h_text= (fn->y2 - fn->y1) / 3;
  SI bottom= r->y1 + ((r->y2 - r->y1) - h_text) / 2, top= bottom + h_text;
  ren->clip (x0, r->y1, x1, r->y2);
  if (pre_n > 0) {
    // the composition: a pale box with an underline, as the editor's
    // pre-edit ornament
    SI xb= x0 + prefix_width (ds, pos) - scroll;
    SI xe= x0 + prefix_width (ds, pos + pre_n) - scroll;
    ren->set_pencil (pencil (theme_color (the_theme.pre_edit)));
    ren->fill (xb, bottom, xe, top);
    ren->set_pencil (pencil (theme_color (the_theme.pre_edit_line)));
    ren->fill (xb, bottom, xe, bottom + px);
  }
  int b, e;
  if (pre_n == 0 && focused && selection (b, e)) {
    SI xb= x0 + prefix_width (ds, b) - scroll, xe= x0 + prefix_width (ds, e) - scroll;
    ren->set_pencil (pencil (theme_color (the_theme.selection_soft)));
    ren->fill (xb, bottom, xe, top);
  }
  ren->set_shrinking_factor (3);
  ren->set_pencil (pencil (theme_color (greyed ? the_theme.text_grey
                                               : the_theme.text)));
  fn->var_draw (ren, ds, 3 * (x0 - scroll) - ex->x1, 3 * bottom - fn->y1);
  ren->set_shrinking_factor (1);
  if (focused && !greyed) {
    SI cx= x0 + cur - scroll;
    ren->set_pencil (pencil (theme_color (the_theme.cursor), px));
    ren->line (cx, bottom, cx, top);
    ren->line (cx - px, bottom, cx + px, bottom);
    ren->line (cx - px, top, cx + px, top);
  }
  ren->unclip ();
}

void
vue_input_text_widget_rep::do_layout () {
  bool is_focused= current_window->kbd_focus == this;
  SI w= decode_length (width, current_window, style);
  font fn= get_font ();
  SI h_text= (fn->y2 - fn->y1 + 2) / 3;
  // the layout is in device pixels, retina_factor of them per point
  float w_px= (float) retina_factor*w/PIXEL + 2*input_pad_x;
  float h_px= (float) retina_factor*h_text/PIXEL + 2*input_pad_y;
  Clay_ElementId cid= CLAY_IDI ("input_text_widget", id);
  ui_signal sig { .clicked= 0 };
  if (!greyed) sig= button_logic (cid);
  Clay_ElementData ed= Clay_GetElementData (cid);
  CLAY(cid, {
    .layout= { .sizing= { CLAY_SIZING_FIXED (w_px), CLAY_SIZING_FIXED (h_px) } },
    .custom= { .customData= vue_render_widget },
    .userData= render_ref () }) {}
  if (ed.found && (sig.pressed == 1 || (sig.held && (mouse_state & 1)))) {
    // the mouse places the cursor and, dragged, selects
    SI x= (SI) ((mouse_x - ed.boundingBox.x - input_pad_x)
                * (PIXEL / retina_factor)) + scroll;
    int p= position_at (x);
    if (sig.pressed == 1) {
      mouse_action= "";
      set_kbd_focus (current_window, this);
      pos= p; sel= p;
    }
    else pos= p;
  }
  if (sig.clicked == 1 && sel == pos) sel= -1;
  if ((N(key_event) > 0) && (is_focused)) {
    process_key (key_event);
    key_event= "";
  }
}

widget
input_text_widget (command call_back, string type, array<string> def,
                          int style, string width) {
  return abstract (tm_new<vue_input_text_widget_rep> (call_back, type,
                                                      def, style, width));
}

// Give the keyboard focus to the input field with this name, as
// SLOT_KEYBOARD_FOCUS_ON asks ("search", "replace-what", "spell"...).
// False when no such field exists at the moment.
bool
focus_on_named_input (vue_window win, string field) {
  // The field is identified by the string the widget was built with:
  // "name#serial:type", which we split, or a bare word which lands in the
  // type ("search", "replace-what"...). Qt matches the same string, which
  // it keeps whole as the object name, so both halves are tried here.
  for (int pass= 0; pass < 2; pass++) {
    iterator<pointer> it= iterate (live_inputs);
    while (it->busy ()) {
      vue_input_text_widget_rep* in= (vue_input_text_widget_rep*) it->next ();
      if (pass == 0 ? (in->name == field) : (in->type == field)) {
        set_kbd_focus (win, in);
        return true;
      }
    }
  }
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "no input field named " << field << LF;
  return false;
}

// the string currently entered in an input_text_widget
string
input_text_widget_string (widget w) {
  vue_input_text_widget_rep* in= dynamic_cast<vue_input_text_widget_rep*> (w.rep);
  return (in != NULL) ? in->s : string ("");
}

// the value currently selected in an enum_widget
string
enum_widget_value (widget w) {
  vue_ui_rep* u= dynamic_cast<vue_ui_rep*> (w.rep);
  if (u == NULL || u->type != "enum_widget") return "";
  return open_box<vue_enum_widget_star> (u->data).val;
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
  bool popup;    // undecorated popup or tooltip window, sized to its contents
  bool autosize; // size the window to its contents at the next layout pass
  bool quit_sent; // the quit command has been queued
  SI last_cw, last_ch; // contents size measured in the previous layout pass
  string title;
  string refresh_kind;
  
public:
  vue_plain_window_widget_rep (widget _wid, string _name, command _quit,
                               bool _popup= false);
  ~vue_plain_window_widget_rep() {}
  
  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
  void notify (slot s, blackbox new_val);
  
  void do_layout ();
  bool post_layout ();
}; // class vue_plain_window_widget_rep

vue_plain_window_widget_rep::vue_plain_window_widget_rep (widget _wid, string _name,
                                                          command _quit, bool _popup)
: vue_widget_rep (type_vue_plain_window_widget), wid(_wid), name(_name), quit(_quit),
  win (NULL), visible (false), popup (_popup) {
  if (DEBUG_VUE) debug_widgets << "Creating vue_plain_window_widget" << (popup ? " (popup)" : "") << LF;
  // dialogs get their initial size from their contents, the main TeXmacs
  // window and popups are handled differently (see do_layout/post_layout)
  autosize= !popup && concrete (wid)->type != "vue_texmacs_widget_rep";
  last_cw= last_ch= -1;
  quit_sent= false;
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
        autosize= false; // an explicit size wins over the contents
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
        check_type<bool> (val, s); // true= get grab, false= release grab
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
        if (win) win->next_refresh_kinds << kind;
      }
      break;
    case SLOT_DESTROY:
    {
      ASSERT (is_nil (val), "type mismatch");
      // the quit command usually deletes the window, which sends us
      // SLOT_DESTROY again: run it only once. The main TeXmacs window has
      // no quit command of its own: the request goes to its contents (the
      // texmacs widget, whose command kills the window or quits), as the
      // X11 port does
      if (!quit_sent) {
        quit_sent= true;
        if (!is_nil (quit)) cmd_list= list (quit, cmd_list);
        else if (!is_nil (wid)) wid->send (s, val);
      }
    }
      break;
    default:
      vue_widget_rep::send(s, val);
  }
}

blackbox
vue_plain_window_widget_rep::query (slot s, int type_id) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_plain_window_widget_rep::query " << slot_name(s) << LF;
  switch (s) {
    case SLOT_IDENTIFIER:
    {
      check_type_id<int> (type_id, s);
      return close_box<int> (win ? win->id : 0);
    }
    case SLOT_POSITION:
    {
      SI x= 0, y= 0; // the window may already be destroyed
      check_type_id<coord2> (type_id, s);
      if (win) win->get_position (x, y);
      return close_box<coord2> (coord2 (x, y));
    }
    case SLOT_SIZE:
    {
      SI w= 0, h= 0;
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
  if (DEBUG_VUE_WIDGETS)
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
  if (DEBUG_VUE_WIDGETS)
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
  Clay_BorderElementConfig border= {};
  if (popup) border= { .width= { 1, 1, 1, 1 }, .color= { 150, 150, 150, 255 } };
  // no background: process_redraw clears the window with the same colour
  // before replaying the commands, and painting it again here cost a fill
  // of the whole window per frame (see "Rendering details" in
  // docs/vue-graphics-stack.md)
  CLAY(CLAY_ID("plain_window_widget"), {
    .layout= {
      .layoutDirection= CLAY_TOP_TO_BOTTOM,
      // while sizing to the contents the root must not be bound by the window
      .sizing= (popup || autosize) ? layoutFit : layoutFull },
    .border= border })
  {
     window_autosizing= autosize;
     concrete (wid)->do_layout ();
     window_autosizing= false;
  }
  // popup menus are dismissed once one of their buttons has been activated
  if (popup && cancel_popup && win) win->set_visibility (false);
}

// the resize_widget which determines the size limits of a window, if any:
// the first one found below the pass-through containers of the contents
static vue_ui_rep*
find_resize_widget (widget w) {
  vue_ui_rep* u= dynamic_cast<vue_ui_rep*> (w.rep);
  if (u == NULL) return NULL;
  if (u->type == "resize_widget") return u;
  array<widget> children;
  if (u->type == "vertical_list")
    children= open_box<vue_vertical_list> (u->data).a;
  else if (u->type == "horizontal_list")
    children= open_box<vue_horizontal_list> (u->data).a;
  else if (u->type == "vertical_menu")
    children= open_box<vue_vertical_menu> (u->data).a;
  else if (u->type == "horizontal_menu")
    children= open_box<vue_horizontal_menu> (u->data).a;
  else if (u->type == "wrapped_widget")
    children << open_box<vue_wrapped_widget> (u->data).w;
  else if (u->type == "division_widget")
    children << open_box<vue_division_widget> (u->data).w;
  else if (u->type == "refreshable_widget")
    children << open_box<vue_refreshable_widget_star> (u->data).current;
  else if (u->type == "refresh_widget")
    children << open_box<vue_refresh_widget_star> (u->data).current;
  for (int i=0; i<N(children); i++) {
    if (is_nil (children[i])) continue;
    vue_ui_rep* r= find_resize_widget (children[i]);
    if (r != NULL) return r;
  }
  return NULL;
}

bool
vue_plain_window_widget_rep::post_layout () {
  if (win == NULL) return false;
  Clay_ElementData el= Clay_GetElementData (CLAY_ID("plain_window_widget"));
  if (!el.found) return false;
  SI w, h;
  win->get_size (w, h);
  SI cw= (SI) (el.boundingBox.width  * PIXEL / retina_factor),
     ch= (SI) (el.boundingBox.height * PIXEL / retina_factor);
  if (cw <= 0 || ch <= 0) return false;
  // the window may be shown once its contents fit in it (see vue_window_rep)
  if (!(popup || autosize)) {
    win->ready_to_show= true;
    return false;
  }
  if (abs (w - cw) <= PIXEL && abs (h - ch) <= PIXEL) win->ready_to_show= true;
  // popups always follow their contents, other windows only initially
  if (!popup) {
    // some widgets (tabs, aligned, extend) use measurements of the previous
    // layout pass: wait until the size of the contents is stable
    if (cw != last_cw || ch != last_ch) {
      last_cw= cw; last_ch= ch;
      return false;
    }
    // leave some room for the window decorations and the screen borders
    SI sw, sh;
    gui_root_extents (sw, sh);
    cw= min (cw, sw - 100*PIXEL);
    ch= min (ch, sh - 150*PIXEL);
    autosize= false;
    // the limits of a resize_widget in the contents become those of the
    // window, corrected by the space taken by everything around it
    vue_ui_rep* rw= find_resize_widget (wid);
    if (rw != NULL) {
      vue_resize_widget d= open_box<vue_resize_widget> (rw->data);
      Clay_ElementData re= Clay_GetElementData (CLAY_SIDI (CLAY_TM_STRING (rw->type), rw->id));
      if (re.found) {
        SI dw= cw - (SI) (re.boundingBox.width  * PIXEL / retina_factor);
        SI dh= ch - (SI) (re.boundingBox.height * PIXEL / retina_factor);
        SI minw= decode_length (d.w1, win, d.style), minh= decode_length (d.h1, win, d.style);
        SI maxw= decode_length (d.w3, win, d.style), maxh= decode_length (d.h3, win, d.style);
        win->set_size_limits (minw + dw, minh + dh, maxw + dw, maxh + dh);
      }
    }
  }
  if ((w != cw) || (h != ch)) {
    win->set_size (cw, ch);
    if (popup) {
      // keep the popup on the screen (set_position clamps with the new size)
      SI x, y;
      win->get_position (x, y);
      win->set_position (x, y);
    }
  }
  return false; // the new size is picked up by the next layout pass
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
  
  // the query line of the footer: a prompt and an input field, shown in
  // place of the footer while the editor waits for an answer
  vue_widget interactive_prompt;
  vue_widget interactive_input;
  bool interactive_mode;

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

// index in vue_texmacs_widget_rep::visibility of a visibility slot, in the
// order of the bits of the mask given to texmacs_widget (the slots are not
// declared in that order: the footer comes after the tools)
static int
visibility_index (slot s) {
  switch (s) {
    case SLOT_HEADER_VISIBILITY:       return 0;
    case SLOT_MAIN_ICONS_VISIBILITY:   return 1;
    case SLOT_MODE_ICONS_VISIBILITY:   return 2;
    case SLOT_FOCUS_ICONS_VISIBILITY:  return 3;
    case SLOT_USER_ICONS_VISIBILITY:   return 4;
    case SLOT_FOOTER_VISIBILITY:       return 5;
    case SLOT_SIDE_TOOLS_VISIBILITY:   return 6;
    case SLOT_LEFT_TOOLS_VISIBILITY:   return 7;
    case SLOT_BOTTOM_TOOLS_VISIBILITY: return 8;
    case SLOT_EXTRA_TOOLS_VISIBILITY:  return 9;
    default: return 0;
  }
}
  
vue_texmacs_widget_rep::vue_texmacs_widget_rep (int _mask, command _quit)
  : vue_widget_rep ("vue_texmacs_widget_rep"), mask (_mask), quit (_quit),
    win (NULL), interactive_mode (false)
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
      visibility [visibility_index (s)]= check_open<bool> (val, s);
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

    case SLOT_INTERACTIVE_MODE:
      // the footer becomes a query line; the input takes the keyboard
      interactive_mode= check_open<bool> (val, s);
      if (win) {
        if (interactive_mode && !is_nil (interactive_input))
          set_kbd_focus (win, interactive_input);
        else set_kbd_focus (win, main_widget);
      }
      break;

    case SLOT_FULL_SCREEN:
      if (win) win->set_full_screen (check_open<bool> (val, s));
      break;

    case SLOT_KEYBOARD_FOCUS_ON:
      if (win) focus_on_named_input (win, check_open<string> (val, s));
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
      if (win) set_kbd_focus (win, main_widget);
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
      if (DEBUG_VUE_WIDGETS) debug_widgets << "vue_texmacs_widget_rep::write(), unhandled " << slot_name (s)
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

    case SLOT_INTERACTIVE_MODE:
      check_type_id<bool> (type_id, s);
      return close_box<bool> (interactive_mode);

    case SLOT_INTERACTIVE_INPUT:
      // what was typed in the query line, quoted as the dialogs quote the
      // answers of their fields ("#f" when there is nothing to report)
      check_type_id<string> (type_id, s);
      if (!is_nil (interactive_input)) {
        widget iw= abstract (interactive_input);
        vue_input_text_widget_rep* in=
          dynamic_cast<vue_input_text_widget_rep*> (iw.rep);
        if (in != NULL && in->ok)
          return close_box<string> (scm_quote (in->s));
      }
      return close_box<string> (string ("#f"));

    case SLOT_SIZE:
    {
      // the size of the window (the editor sizes the "automatic" paper
      // from it, as with the Qt main window); for an embedded editor
      // (mask 0) the size of its canvas, as the Qt embedded widget
      check_type_id<coord2> (type_id, s);
      if (mask == 0 && !is_nil (main_widget))
        return main_widget->query (s, type_id);
      SI w= 0, h= 0;
      if (win) win->get_size (w, h);
      return close_box<coord2> (coord2 (w, h));
    }
      
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
      return close_box<bool> (visibility [visibility_index (s)]);
      
    default:
      return vue_widget_rep::query(s, type_id);
  }
}


// A tool area of the main window (side, left, bottom or extra tools): the
// the initial states of the slide-in transitions of the tool panels: from
// beyond the right, left or bottom edge of their final position
static Clay_TransitionData
slide_from_right (Clay_TransitionData target, Clay_TransitionProperty props) {
  (void) props; target.boundingBox.x += target.boundingBox.width; return target;
}
static Clay_TransitionData
slide_from_left (Clay_TransitionData target, Clay_TransitionProperty props) {
  (void) props; target.boundingBox.x -= target.boundingBox.width; return target;
}
static Clay_TransitionData
slide_from_bottom (Clay_TransitionData target, Clay_TransitionProperty props) {
  (void) props; target.boundingBox.y += target.boundingBox.height; return target;
}

// tools are stacked in a scrollable panel of their natural size, bounded by a
// fraction of the window so that the editor keeps most of the space; a panel
// which appears slides in from its edge (from: 0 right, 1 left, 2 bottom;
// position only, so that the sizes the tools measure are final at once)
static void
layout_tool_panel (Clay_ElementId id, vue_widget tools, bool side, float win_w, float win_h, int from= 0) {
  Clay_Sizing sizing;
  if (side) sizing= { CLAY_SIZING_FIT (.min= 150, .max= (float) max (150.0, 0.4 * win_w)),
                      CLAY_SIZING_GROW(0) };
  else      sizing= { CLAY_SIZING_GROW(0),
                      CLAY_SIZING_FIT (.min= 40, .max= (float) max (40.0, 0.4 * win_h)) };
  Clay_BorderWidth bw= side ? (Clay_BorderWidth) { 1, 1, 0, 0 } : (Clay_BorderWidth) { 0, 0, 1, 1 };
  CLAY(id, {
    .backgroundColor= palette[2],
    .layout= {
      .sizing= sizing,
      .padding= CLAY_PADDING_ALL(10),
      .childGap= 10,
      .layoutDirection= CLAY_TOP_TO_BOTTOM },
    .clip= { .horizontal= !side, .vertical= side, .childOffset= Clay_GetScrollOffset () },
    .border= { .width= bw, .color= color_border },
    .transition= {
      .handler= Clay_EaseOut, .duration= 0.15f,
      .properties= side ? CLAY_TRANSITION_PROPERTY_X : CLAY_TRANSITION_PROPERTY_Y,
      .enter= { .setInitialState= (from == 1) ? slide_from_left
                                : (from == 2) ? slide_from_bottom : slide_from_right,
                .trigger= CLAY_TRANSITION_ENTER_SKIP_ON_FIRST_PARENT_FRAME }}})
  {
    tools->do_layout ();
  }
  Clay_ScrollContainerData sd= Clay_GetScrollContainerData (id);
  if (sd.found) scroll_bar (id, sd);
}

// The bars of the main window look as in the Qt port: a menu bar and a
// main tool bar in the window grey, a lighter mode bar and a lighter still
// focus bar, separated by 2 px lines slightly darker than the window grey,
// no gaps; the footer in the window grey right below the canvas. Sizes in
// pixels (the bars are as tall as their contents, at least these heights).
static const uint16_t bar_hpad= 24;   // contents clear of the window edges
static const float bar_menu_h= 62, bar_main_h= 88, bar_mode_h= 72, bar_focus_h= 64,
                   bar_footer_h= 56;

void vue_texmacs_widget_rep::do_layout () {
  win= current_window; // save the info
  // grow to the size of the window
  SI w= 300, h= 300;
  if (win) win->get_size (w, h);
  if (win->kbd_focus == NULL) set_kbd_focus (win, main_widget);
  // the bars follow the mask given at creation and the visibility slots;
  // an embedded editor (texmacs-input in a dialog or a tool, mask 0) is
  // only its canvas, as the Qt embedded widget
  // no background either, for the same reason: with bars this widget fills
  // the window, which is already cleared with that colour, and an embedded
  // editor (mask 0) shows the container behind it
  CLAY(CLAY_IDI("texmacs_widget", id), {
    .layout= {
      .layoutDirection= CLAY_TOP_TO_BOTTOM,
      .sizing= layoutFull, // fills the window, or the box of an embedded editor
      .padding= { 0, 0, 0, 0 },
      .childGap= 0 }})
  {
    if (visibility[0]) CLAY(CLAY_ID_LOCAL("MainMenuBar"), {
      .layout= {
        .padding= { bar_hpad, bar_hpad, 0, 0 },
        .childAlignment= { .y= CLAY_ALIGN_Y_CENTER },
        .sizing= {
          .width=  CLAY_SIZING_GROW(0),
          .height= CLAY_SIZING_FIT(.min= bar_menu_h) }},
      .backgroundColor= color_background,
      .border= { .width= { .bottom= 2 }, .color= the_theme.bar_line }})
    {
      if (!is_nil (main_menu)) {
        main_menu->do_layout ();
      }
    }
    if (visibility[1]) CLAY(CLAY_ID_LOCAL("MainToolbar"), {
      .layout= {
         .padding= { bar_hpad, bar_hpad, 0, 0 },
         .childAlignment= { .y= CLAY_ALIGN_Y_CENTER },
         .sizing= {
           .width=  CLAY_SIZING_GROW(0),
           .height= CLAY_SIZING_FIT(.min= bar_main_h) }},
      .backgroundColor= color_background,
      .border= { .width= { .bottom= 2 }, .color= the_theme.bar_line }})
    {
      if (!is_nil (main_icons)) {
        main_icons->do_layout ();
      }
    }
    if (visibility[2]) CLAY(CLAY_ID_LOCAL("ModeToolbar"), {
      .layout= {
         .padding= { bar_hpad, bar_hpad, 0, 0 },
         .childAlignment= { .y= CLAY_ALIGN_Y_CENTER },
         .sizing= {
           .width=  CLAY_SIZING_GROW(0),
           .height= CLAY_SIZING_FIT(.min= bar_mode_h) }},
      .backgroundColor= the_theme.bar_mode,
      .border= { .width= { .bottom= 2 }, .color= the_theme.bar_line }})
    {
      if (!is_nil (mode_icons)) {
        mode_icons->do_layout ();
      }
    }
    if (visibility[3]) CLAY(CLAY_ID_LOCAL("FocusToolbar"), {
      .layout= {
         .padding= { bar_hpad, bar_hpad, 0, 0 },
         .childAlignment= { .y= CLAY_ALIGN_Y_CENTER },
         .sizing= {
            .width=  CLAY_SIZING_GROW(0),
            .height= CLAY_SIZING_FIT(.min= bar_focus_h) }},
      .backgroundColor= the_theme.bar_focus,
      .border= { .width= { .bottom= 2 }, .color= the_theme.bar_line }})
    {
      if (!is_nil (focus_icons)) {
        focus_icons->do_layout ();
      }
    }
    // the user icon bar, which a document may fill through its style
    // (the bar was received and stored, and never drawn)
    if (visibility[4] && !is_nil (user_icons))
      CLAY(CLAY_ID_LOCAL("UserToolbar"), {
        .layout= {
           .padding= { bar_hpad, bar_hpad, 0, 0 },
           .childAlignment= { .y= CLAY_ALIGN_Y_CENTER },
           .sizing= {
              .width=  CLAY_SIZING_GROW(0),
              .height= CLAY_SIZING_FIT(.min= bar_focus_h) }},
        .backgroundColor= the_theme.bar_focus,
        .border= { .width= { .bottom= 2 }, .color= the_theme.bar_line }})
      {
        user_icons->do_layout ();
      }
    // the middle row: left tools, the editor and the side tools
    CLAY(CLAY_ID_LOCAL("Middle"), {
      .layout= {
        .layoutDirection= CLAY_LEFT_TO_RIGHT,
        .sizing= layoutExpand }})
    {
      if (visibility[7] && !is_nil (left_tools))
        layout_tool_panel (CLAY_ID_LOCAL("LeftTools"), left_tools, true,
                           win->layout_w, win->layout_h, 1);
      if (!is_nil (main_widget)) main_widget->do_layout ();
      if (visibility[6] && !is_nil (side_tools))
        layout_tool_panel (CLAY_ID_LOCAL("SideTools"), side_tools, true,
                           win->layout_w, win->layout_h);
    }
    if (visibility[8] && !is_nil (bottom_tools))
      layout_tool_panel (CLAY_ID_LOCAL("BottomTools"), bottom_tools, false,
                         win->layout_w, win->layout_h, 2);
    if (visibility[9] && !is_nil (extra_tools))
      layout_tool_panel (CLAY_ID_LOCAL("ExtraTools"), extra_tools, false,
                         win->layout_w, win->layout_h, 2);
    if (visibility[5]) CLAY(CLAY_ID_LOCAL("Footer"), {
      .layout= {
        .padding= { bar_hpad, bar_hpad, 0, 0 },
        .childAlignment= { .y= CLAY_ALIGN_Y_CENTER },
        .sizing= {
          .width=  CLAY_SIZING_GROW(0),
          .height= CLAY_SIZING_FIXED(bar_footer_h) }},
      .backgroundColor= color_background })
    {
      if (interactive_mode && !is_nil (interactive_input)) {
        // the query line: the footer becomes a prompt and a field, as it
        // does under Qt when "interactive questions" is set to "footer"
        if (!is_nil (interactive_prompt)) interactive_prompt->do_layout ();
        CLAY_AUTO_ID({ .layout= { .sizing= { CLAY_SIZING_FIXED(8) }}}) {}
        CLAY_AUTO_ID({
          .layout= { .sizing= { .width= CLAY_SIZING_GROW(0) } },
          .clip= { .horizontal= true }})
        {
          interactive_input->do_layout ();
        }
      }
      else {
        // the left text takes the remaining space and is clipped
        CLAY_AUTO_ID({
          .layout= { .sizing= { .width= CLAY_SIZING_GROW(0) } },
          .clip= { .horizontal= true }})
        {
          layout_text (left_footer, 0, black);
        }
        layout_text (right_footer, 0, black);
      }
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
  win (NULL),
  size (coord2 (0, 0)),
  extents (0,0,0,0),
  scroll_pos (coord2 (0, 0)),
  cursor_pos (coord2 (0, 0)),
  mouse_grab (false),
  absolute_scroll (false),
  ren (NULL),
  backing_pos (coord2 (0, 0)), origin (coord2 (0, 0)),
  backing_valid (false),
  resize_pending (false),
  scroll_rest_x (0), scroll_rest_y (0)
{
  // note that size is set to an arbitrary value to init the backing_store
  // create a backing store and the renderer
  // opaque, so that blitting it into the window needs no test per pixel
  // (see native_opaque_picture)
  backing_store= native_opaque_picture (size.x1, size.x2, 0, 0);
  ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
  paint_list= list<vue_simple_widget_rep*>(this, paint_list);
};

vue_simple_widget_rep::~vue_simple_widget_rep () {
  paint_list= remove (paint_list, this);
  // the renderer holds the backing store pixmap, a draw device and a PDF
  // processor: they were leaked for every destroyed editor
  if (ren != NULL) { delete_renderer (ren); ren= NULL; }
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
        if (DEBUG_VUE_EVENTS) debug_events << "extents " << extents << LF;
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
        cursor_pos= check_open <coord2> (val, s);
      }
      break;
    default:
      if (DEBUG_VUE_WIDGETS) debug_widgets << "simple_widget does not handle " << slot_name (s) << LF;
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
      // the position of the canvas in its window, in TeXmacs coordinates
      // (PIXEL per point, y up): the editor adds it to the position of the
      // window and to a click to place its context menu (edit_mouse.cpp)
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (coord2 (origin.x1 * PIXEL / retina_factor,
                                        -origin.x2 * PIXEL / retina_factor));
    }
    case SLOT_SIZE:
    {
      // the size of the viewport in TeXmacs units (size is in pixels of the
      // backing store); the editor derives the width of the paper from it
      check_type_id<coord2> (type_id, s);
      return close_box<coord2> (coord2 (size.x1 * ren->pixel, size.x2 * ren->pixel));
    }
    case SLOT_SCROLL_POSITION:
    {
      if (DEBUG_VUE_EVENTS) debug_events << "scroll_where " << backing_pos << LF;
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

/******************************************************************************
* layout
******************************************************************************/

void
vue_simple_widget_rep::do_layout () {
  win= current_window; // save the info
  SI w= 0, h= 0;
  Clay_Sizing s= layoutExpand;
  if (is_embedded_widget () && !is_editor_widget ()) {
    // typeset boxes (texmacs-output) have their natural size; editors,
    // embedded or not, fill their container (their size hint is the screen)
    handle_get_size_hint (w, h);
    s= {
      .width=  CLAY_SIZING_FIT(.min= (float)w/ren->pixel),
      .height= CLAY_SIZING_FIT(.min= (float)h/ren->pixel) };
  }
  Clay_ElementId clay_id= CLAY_IDI("simple_widget", id);
  Clay_ElementData d= Clay_GetElementData (clay_id);
  CLAY(clay_id, {
    .layout= { .sizing= s },
    .custom= { .customData= vue_render_widget },
    .userData= render_ref () })
  {
    if (0) { // debug view
      CLAY_AUTO_ID({
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
                                          backing_pos.x2 - d.boundingBox.height * ren->pixel,
                                          backing_pos.x1 + d.boundingBox.width * ren->pixel,
                                          backing_pos.x2);
        out.flush ();
        CLAY_TEXT(CLAY_TM_STRING(debug_text),
                  CLAY_TEXT_CONFIG({ .fontSize= 30, .textColor= { 0, 0, 200, 255} }));
      }
    }
  }
  if (d.found) {
    Clay_Vector2 scrollPosition = {
      .x= -(float)backing_pos.x1 / ren->pixel,
      .y= (float)backing_pos.x2 / ren->pixel };
    Clay_ScrollContainerData scrollData= {
      .scrollPosition= &scrollPosition,
      .scrollContainerDimensions= {
        .width=  d.boundingBox.width,
        .height= d.boundingBox.height },
      .contentDimensions= {
        .width=  ((float)extents->x2 - extents->x1)/ren->pixel,
        .height= ((float)extents->y2 - extents->y1)/ren->pixel, },
      .config= {
        .horizontal= true,
        .vertical= true,
        .childOffset= scrollPosition },
      .found= true
    };
    Clay_Vector2 before= scrollPosition;
    scroll_bar (clay_id, scrollData);
    // only a dragged thumb changes the position here; assigning it back
    // unconditionally dropped the scroll requests of the editor (scroll to
    // the cursor) whenever a second layout pass followed in the same
    // iteration of the loop
    if (scrollPosition.x != before.x || scrollPosition.y != before.y) {
      scroll_pos.x1= -((SI) floor (scrollPosition.x + 0.5)) * ren->pixel;
      scroll_pos.x2=  ((SI) floor (scrollPosition.y + 0.5)) * ren->pixel;
      absolute_scroll= false;
    }
  }
  // note: our CLAY block is closed here, Clay_Hovered () would test the parent
  if (Clay_PointerOver (clay_id) && (mouse_action != "")) {
    SI x= mouse_x - d.boundingBox.x;
    SI y= mouse_y - d.boundingBox.y;
    ren->set_origin (-backing_pos.x1, -backing_pos.x2);
    ren->encode (x,y);
    if (N(mouse_data) == 2) {
      // the wheel deltas come in device pixels (see push_wheel in
      // vue_gui.cpp): the same displacement as a dragged scroll bar gives
      mouse_data[0] *= ren->pixel;
      mouse_data[1] *= ren->pixel;
    }
    if (DEBUG_VUE_EVENTS && mouse_action != "move") {
      debug_events << "handling " << mouse_action << " at " << mouse_time
                   << " (" << x << "," << y << ")";
      if (N(mouse_data) == 2)
        debug_events << " [" << mouse_data[0] << "," << mouse_data[1] << "]";
      debug_events << LF;
    }
    if (mouse_action == "wheel") {
      // the deltas come in small steps (kinetic scrolling, see vue_gui.cpp):
      // the fractions of SI are carried over to the next step
      absolute_scroll= false;
      scroll_pos= backing_pos;
      scroll_rest_x += mouse_data[0];
      scroll_rest_y += mouse_data[1];
      // whole pixels only (the backing store is shifted, see
      // repaint_invalid_regions), the remainder waits for the next step
      SI px= ren->pixel;
      SI dx= ((SI) floor (scroll_rest_x / px)) * px;
      SI dy= ((SI) floor (scroll_rest_y / px)) * px;
      scroll_rest_x -= dx; scroll_rest_y -= dy;
      scroll_pos.x1 += dx;
      scroll_pos.x2 += dy;
    } else {
      if (starts (mouse_action, "press-")) {
        set_kbd_focus (current_window, this);
      }
      // a drop passes the key of its payload where the modifiers usually
      // are (call_drop_event in edit_mouse.cpp reads it back)
      int mods= (mouse_action == "drop") ? mouse_ticket : (int) mouse_state;
      handle_mouse (mouse_action, x, y, mods, mouse_time, mouse_data);
    }
    // reset
    mouse_action="";
    if (N(mouse_data) > 0) mouse_data= array<double>();
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
  SI X1= x1, Y1= y1, X2= x2, Y2= y2;
  ren->set_origin (-backing_pos.x1, -backing_pos.x2);
  ren->encode (X1, Y1);
  ren->encode (X2, Y2);
  invalidate_rect (X1, Y2, X2, Y1);
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

// shift the pixels of the backing store by (dpx, dpy) (pixmap pixels, y
// down): the content moved by the scroll, the exposed strips are
// invalidated by the caller
void
vue_simple_widget_rep::translate_backing_store (int dpx, int dpy) {
#if MUPDF_RENDERER
  fz_pixmap *pix=  ((mupdf_picture_rep*)backing_store->get_handle())->pix;
#else
  fz_pixmap *pix=  ((fitz_picture_rep*)backing_store->get_handle())->pix;
#endif
  if (pix == NULL || pix->samples == NULL) return;
  int w= pix->w, h= pix->h, n= pix->n;
  ptrdiff_t stride= pix->stride;
  if (dpx >= w || -dpx >= w || dpy >= h || -dpy >= h) return; // nothing left
  int x0= max (0, dpx), x1= min (w, w + dpx); // destination columns
  int y0= max (0, dpy), y1= min (h, h + dpy); // destination rows
  size_t len= (size_t) (x1 - x0) * n;
  if (dpy > 0) // moving down: from the last row up, so as not to overwrite sources
    for (int y= y1 - 1; y >= y0; y--)
      memmove (pix->samples + y * stride + x0 * n,
               pix->samples + (y - dpy) * stride + (x0 - dpx) * n, len);
  else
    for (int y= y0; y < y1; y++)
      memmove (pix->samples + y * stride + x0 * n,
               pix->samples + (y - dpy) * stride + (x0 - dpx) * n, len);
}

// a scroll position on the pixel grid of the backing store (floor)
static SI
grid_floor (SI v, SI px) {
  return (v >= 0) ? (v / px) * px : -(((-v) + px - 1) / px) * px;
}

void
vue_simple_widget_rep::invalidate_all_editors () {
  list<vue_simple_widget_rep*> l= paint_list;
  while (!is_nil (l)) { l->item->invalidate_all (); l= l->next; }
}

void
vue_simple_widget_rep::repaint_invalid_regions () {

  vue_plain_window_widget_rep *w=
      win ? dynamic_cast<vue_plain_window_widget_rep*>(win->content.rep)
          : NULL;

  if (!w) return; // we are not in a layout yet
  
  // retrieve current geometry
  Clay_ElementId clay_id= CLAY_IDI("simple_widget", id);
  {
    with_window frame (w->win);
    Clay_ElementData d= Clay_GetElementData (clay_id);
    if (d.found) {
      // cache the current viewport size
      size.x1= d.boundingBox.width; // * retina_factor;
      size.x2= d.boundingBox.height; // * retina_factor;
      origin.x1= (SI) d.boundingBox.x;
      origin.x2= (SI) d.boundingBox.y;
    } else {
      if (DEBUG_VUE_WIDGETS) debug_widgets << "clay_id of a simple widget not found" << LF;
    }
  }

  // current backing_store size
  int bs_w= backing_store->get_width ();
  int bs_h= backing_store->get_height ();

  // Update the scroll position

  // viewport size (in TeXmacs units)
  coord2 sz (size.x1 * ren->pixel, size.x2 * ren->pixel);

  // the extents may have changed since the last repaint (e.g. the paper is
  // centered in a wider viewport): the position is clamped in any case
  {
    // preprocess scroll_pos
    if (absolute_scroll) {
      coord2 pt= scroll_pos;
      scroll_pos= backing_pos;
      // cout << "extents " << extents << LF;
      // cout << "scroll_to (initial) " << pt << " current " << scroll_pos << " size " << sz << LF;
      if (pt.x1 < scroll_pos.x1) scroll_pos.x1= pt.x1-sz.x1/2;
      else if (pt.x1 > scroll_pos.x1 + sz.x1) scroll_pos.x1= pt.x1-sz.x1/2;
      if (pt.x2 > scroll_pos.x2) scroll_pos.x2= pt.x2+sz.x2/2;
      else if (pt.x2 < scroll_pos.x2 - sz.x2) scroll_pos.x2= pt.x2+sz.x2/2;
      // cout << "scroll_pos (corrected) " << scroll_pos << LF;
      absolute_scroll=false;
    }
    
    // clamp the new position
    if (scroll_pos.x1 < extents->x1) scroll_pos.x1= extents->x1;
    else if (scroll_pos.x1 + sz.x1 > extents->x2)
      scroll_pos.x1= max (extents->x2 - sz.x1, extents->x1);
    if (scroll_pos.x2 - sz.x2 < extents->y1)
      scroll_pos.x2= min (extents->y1 + sz.x2, extents->y2);
    else if (scroll_pos.x2 > extents->y2) scroll_pos.x2= extents->y2;
    // and keep it on the pixel grid, so that the content of the backing
    // store can be reused after a scroll (a shift by whole pixels)
    scroll_pos.x1= grid_floor (scroll_pos.x1, ren->pixel);
    scroll_pos.x2= grid_floor (scroll_pos.x2, ren->pixel);
  }
  
  // the scroll position changed: instead of repainting the whole backing
  // store, shift its content and repaint only the exposed strips (the
  // pending invalid regions are in document coordinates and stay valid).
  // The window renderer maps document point P to pixel
  // ((P.x - pos.x1)/pixel, (pos.x2 - P.y)/pixel), so the content moves by
  // (-ddx, +ddy) pixels when the position moves by (ddx, ddy)
  if (backing_pos != scroll_pos) {
    SI ddx= scroll_pos.x1 - backing_pos.x1;
    SI ddy= scroll_pos.x2 - backing_pos.x2;
    backing_pos= scroll_pos;
    if (backing_valid && ddx % ren->pixel == 0 && ddy % ren->pixel == 0 &&
        (int) size.x1 == bs_w && (int) size.x2 == bs_h) {
      int dpx= (int) (-ddx / ren->pixel), dpy= (int) (ddy / ren->pixel);
      translate_backing_store (dpx, dpy);
      if (dpy > 0) invalidate_viewport_rect (0, 0, bs_w, min (bs_h, dpy));
      else if (dpy < 0) invalidate_viewport_rect (0, max (0, bs_h + dpy), bs_w, bs_h);
      if (dpx > 0) invalidate_viewport_rect (0, 0, min (bs_w, dpx), bs_h);
      else if (dpx < 0) invalidate_viewport_rect (max (0, bs_w + dpx), 0, bs_w, bs_h);
    }
    else invalidate_all ();
  }
  
  // check if the window has been resized. If so, we need to resize the backing
  // store as well. During the resize, the origin remain the same. So we can just
  // crop the backing store if the window is smaller, or fill the new regions with
  // the background color if the window is bigger.

  int new_bs_w= size.x1;
  int new_bs_h= size.x2;

  if ((new_bs_w != bs_w)   || (new_bs_h != bs_h)) {
    // the viewport size changed, reset the backing store
    // cout << "viewport changed (" << bs_w << "," << bs_h << ") (" << new_bs_w << "," << new_bs_h << ")" << LF;
    // create a new backing store with updated viewport and the renderer
    picture new_backing_store= native_opaque_picture (new_bs_w, new_bs_h, 0, 0);
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
    // the editor must be told (see notify_resizes), but not while we are
    // repainting: it would re-typeset in the middle of a repaint
    resize_pending= true;
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
vue_simple_widget_rep::notify_resizes () {
  // the editor re-typesets for a new viewport (e.g. the width of the paper
  // in papyrus mode follows the window); this must happen before its
  // pending changes are applied by the interpose handler
  list<vue_simple_widget_rep*> l= paint_list;
  while (!is_nil(l)) {
    vue_simple_widget_rep* w= l->item;
    if (w->resize_pending) {
      w->resize_pending= false;
      w->handle_notify_resize (w->size.x1 * w->ren->pixel, w->size.x2 * w->ren->pixel);
    }
    l= l->next;
  }
}

void
vue_simple_widget_rep::forget_window (vue_window win) {
  // widgets may outlive their window (e.g. the contents of a dialog which
  // are still referenced from scheme): drop the dangling reference
  list<vue_simple_widget_rep*> l= paint_list;
  while (!is_nil(l)) {
    if (l->item->win == win) l->item->win= NULL;
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
   shown (false), position (coord2 (0, 0)), size (coord2 (100, 100)), file ("")
{
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_chooser_widget_rep::vue_chooser_widget_rep file_type=\""
                  << file_type << "\" prompt=\"" << prompt << "\"" << LF;
  if (N(_type) > 0)
    file_type= _type;
  else file_type= "generic";
}

// the window the dialog belongs to (the current TeXmacs window)
static vue_window
parent_platform_window () {
  tm_window win= concrete_window ();
  if (win == NULL) return NULL;
  vue_plain_window_widget_rep *vw= dynamic_cast<vue_plain_window_widget_rep*> (win->win.rep);
  return (vw != NULL) ? vw->win : NULL;
}

void
vue_chooser_widget_rep::send (slot s, blackbox val) {
  switch (s) {
    case SLOT_VISIBILITY:
    {
      // the chooser is the system file dialog: shown once, by the first of
      // SLOT_VISIBILITY and SLOT_KEYBOARD_FOCUS (dialogue_start sends both);
      // this used to be a FAILED, whose error console was the second
      // "dialog" opening next to the file panel
      bool flag= check_open<bool> (val, s);
      if (flag && !shown) {
        shown= true;
        perform_dialog (parent_platform_window ());
      }
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
        if (!shown) {
          shown= true;
          perform_dialog (parent_platform_window ());
        }
      }
      break;
    case SLOT_STRING_INPUT:
      // the file name typed in a TeXmacs chooser: the system dialog has its own
      check_type<string>(val, s);
      break;
    case SLOT_INPUT_TYPE:
      file_type= check_open<string> (val, s);
      break;
    case SLOT_FILE:
        //send_string (THIS, "file", val);
      file= check_open<string> (val, s);
      if (DEBUG_VUE_WIDGETS)
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
                  << "\t\tto widget\t"      << file_type << LF;
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
    // cancelled: the dialogue command gets #f and the dialogue ends (as
    // with the Qt chooser), otherwise the next dialog could not open
    file= "#f";
    cmd ();
    if (!is_nil (quit)) quit ();
  } else {
    string name (res, strlen (res));
    file= "(system->url " * scm_quote (name) * ")";
    if (file_type == "image") {
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
  bool done;                //!< the answers have been reported to cmd
  array<vue_widget> fields;
  array<widget> inputs;     //!< the text inputs of the dialog, one per field
  widget win_widget;        //!< the plain window showing the dialog

  vue_inputs_list_widget_rep (command, array<string>);

  virtual void      send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget    read (slot s, blackbox index);
  
  void perform_dialog ();
  void focus_first_input ();
  void focus_input (int i);       //!< give the keyboard focus to the i-th input
  void finish (bool ok);          //!< store the answers ("#f" if canceled) and call cmd
  void answer (string s);         //!< question dialogs: one of the proposals was chosen
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
  win_title (""), style (0), done (false)
{
  for (int i= 0; i < N(_prompts); i++)
    fields << concrete (tm_new<vue_field_widget_rep> ((vue_inputs_list_widget_rep*)this, _prompts[i]));
}

void
vue_inputs_list_widget_rep::send (slot s, blackbox val) {
  if (DEBUG_VUE_WIDGETS)
    debug_widgets << "vue_inputs_list_widget_rep::send " << slot_name(s) << LF;

  switch (s) {
  case SLOT_VISIBILITY:
    {
      bool flag= check_open<bool> (val, s);
      if (flag && is_nil (win_widget)) perform_dialog ();
      else if (!is_nil (win_widget)) set_visibility (win_widget, flag);
    }
    break;
  case SLOT_SIZE:
    size= check_open<coord2> (val, s);
    break;
  case SLOT_POSITION:
    position= check_open<coord2> (val, s);
    break;
  case SLOT_KEYBOARD_FOCUS:
    if (check_open<bool> (val, s)) {
      if (is_nil (win_widget)) perform_dialog ();
      focus_first_input ();
    }
    break;
  default:
    vue_widget_rep::send (s, val);
  }
}

blackbox
vue_inputs_list_widget_rep::query (slot s, int type_id) {
  if (DEBUG_VUE_WIDGETS)
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

// Ok/Cancel of an inputs list dialog. Also used as callback of its text
// inputs, which call us with (#f) on escape and (string) on return.
class inputs_list_command_rep: public command_rep {
  vue_inputs_list_widget_rep* dlg;
  bool ok;
public:
  inputs_list_command_rep (vue_inputs_list_widget_rep* _dlg, bool _ok)
    : dlg (_dlg), ok (_ok) {}
  void apply () { dlg->finish (ok); }
  void apply (object args) {
    bool canceled= is_list (args) && !is_null (args) &&
                   is_bool (car (args)) && !as_bool (car (args));
    dlg->finish (ok && !canceled);
  }
  tm_ostream& print (tm_ostream& out) {
    return out << "<inputs_list_command " << (ok ? "ok" : "cancel") << ">"; }
};

// tab/shift-tab in the i-th input of a dialog moves the focus
class inputs_list_tab_rep: public command_rep {
  vue_inputs_list_widget_rep* dlg;
  int i;
public:
  inputs_list_tab_rep (vue_inputs_list_widget_rep* _dlg, int _i): dlg (_dlg), i (_i) {}
  void apply () { dlg->focus_input (i+1); }
  void apply (object args) {
    bool forward= !(is_list (args) && !is_null (args) &&
                    is_bool (car (args)) && !as_bool (car (args)));
    dlg->focus_input (forward ? i+1 : i-1);
  }
  tm_ostream& print (tm_ostream& out) { return out << "<inputs_list_tab " << i << ">"; }
};

// one of the proposals of a question dialog
class inputs_list_answer_rep: public command_rep {
  vue_inputs_list_widget_rep* dlg;
  string s;
public:
  inputs_list_answer_rep (vue_inputs_list_widget_rep* _dlg, string _s)
    : dlg (_dlg), s (_s) {}
  void apply () { dlg->answer (s); }
  tm_ostream& print (tm_ostream& out) {
    return out << "<inputs_list_answer " << s << ">"; }
};

void
vue_inputs_list_widget_rep::perform_dialog () {
  if (!is_nil (win_widget)) return;
  command ok_cmd= tm_new<inputs_list_command_rep> (this, true);
  command cancel_cmd= tm_new<inputs_list_command_rep> (this, false);
  vue_field_widget_rep* f0=
    (N(fields) > 0) ? dynamic_cast<vue_field_widget_rep*> (fields[0].rep) : NULL;
  array<widget> rows;
  array<widget> buttons;
  if (N(fields) == 1 && f0 != NULL && f0->type == "question") {
    // a question: one button per proposed answer
    rows << text_widget (f0->prompt, 0, black);
    for (int i=0; i<N(f0->proposals); i++)
      buttons << menu_button (text_widget (upcase_first (f0->proposals[i]), 0, black),
                              tm_new<inputs_list_answer_rep> (this, f0->proposals[i]),
                              "", "", WIDGET_STYLE_BUTTON);
  }
  else {
    // the usual layout: prompts with their inputs, then Ok and Cancel
    array<widget> lhs, rhs;
    inputs= array<widget> ();
    for (int i=0; i< N(fields); i++) {
      vue_field_widget_rep* f= dynamic_cast<vue_field_widget_rep*> (fields[i].rep);
      if (f == NULL) continue;
      widget in= input_text_widget (ok_cmd, f->type, f->proposals, 0, "20em");
      vue_input_text_widget_rep* ir= dynamic_cast<vue_input_text_widget_rep*> (in.rep);
      if (ir != NULL) ir->tab_cb= tm_new<inputs_list_tab_rep> (this, N(inputs));
      inputs << in;
      lhs << text_widget (f->prompt, 0, black);
      rhs << in;
    }
    rows << aligned_widget (lhs, rhs, 6*PIXEL, 6*PIXEL, 0, 0);
    buttons << menu_button (text_widget (translate ("Ok"), 0, black), ok_cmd, "", "", WIDGET_STYLE_BUTTON);
  }
  buttons << menu_button (text_widget (translate ("Cancel"), 0, black), cancel_cmd, "", "", WIDGET_STYLE_BUTTON);
  rows << glue_widget (false, false, 0, 8*PIXEL)
       << horizontal_list (buttons);
  // some padding around the contents
  array<widget> padded;
  padded << glue_widget (false, false, 8*PIXEL, 0)
         << vertical_list (array<widget> (glue_widget (false, false, 0, 8*PIXEL),
                                          vertical_list (rows),
                                          glue_widget (false, false, 0, 8*PIXEL)))
         << glue_widget (false, false, 8*PIXEL, 0);
  widget content= horizontal_list (padded);
  // closing the window from its title bar cancels the dialog
  win_widget= plain_window_widget (content, win_title, cancel_cmd);
  set_position (win_widget, position.x1, position.x2);
  set_visibility (win_widget, true);
  focus_first_input ();
}

void
vue_inputs_list_widget_rep::focus_input (int i) {
  vue_plain_window_widget_rep* ww=
    dynamic_cast<vue_plain_window_widget_rep*> (win_widget.rep);
  int n= N(inputs);
  if (ww == NULL || ww->win == NULL || n == 0) return;
  i= ((i % n) + n) % n; // cyclic
  set_kbd_focus (ww->win, concrete (inputs[i]));
}

void
vue_inputs_list_widget_rep::focus_first_input () {
  focus_input (0);
}

void
vue_inputs_list_widget_rep::finish (bool ok) {
  if (done) return;
  done= true;
  for (int i=0; i< N(fields); i++) {
    vue_field_widget_rep* f= dynamic_cast<vue_field_widget_rep*> (fields[i].rep);
    if (f == NULL) continue;
    if (ok && i < N(inputs)) f->input= scm_quote (input_text_widget_string (inputs[i]));
    else f->input= "#f";
  }
  // the command may end the dialogue, which destroys this widget and drops
  // its reference to the command: keep our own reference while it runs
  command c= cmd;
  if (!is_nil (c)) c ();
}

void
vue_inputs_list_widget_rep::answer (string s) {
  if (done || N(fields) == 0) return;
  done= true;
  vue_field_widget_rep* f= dynamic_cast<vue_field_widget_rep*> (fields[0].rep);
  if (f != NULL) f->input= scm_quote (s);
  command c= cmd; // see finish
  if (!is_nil (c)) c ();
}

//VUE_WIDGET(inputs_list_widget, command, call_back, array<string>, prompts);


widget
inputs_list_widget (command call_back, array<string> prompts) {
  return abstract (tm_new<vue_inputs_list_widget_rep> (call_back, prompts));
}


/******************************************************************************
* ink_widget
******************************************************************************/

class vue_ink_widget_rep: public vue_widget_rep {
  command  cb;
  contours shs;      // the strokes, in pixels from the top-left corner (y downwards)
  bool     dragging; // are we in the middle of a stroke?
  int      w, h;     // size in pixels

public:
  vue_ink_widget_rep (command _cb)
    : vue_widget_rep ("ink_widget"), cb (_cb), shs (), dragging (false),
      w (600), h (400) {}
  void do_layout ();
  void render (void *data);
  void commit ();
};

void
vue_ink_widget_rep::commit () {
  // the strokes are passed to the callback as a list of lists of points
  // (in pixels, y axis upwards, as in the X11 implementation)
  object l= null_object ();
  for (int k= N(shs)-1; k>=0; k--) {
    poly_line sh= shs[k];
    object obj= null_object ();
    for (int i= N(sh)-1; i>=0; i--) {
      object p= list_object (object (sh[i][0]), object (-sh[i][1]));
      obj= cons (p, obj);
    }
    l= cons (obj, l);
  }
  cmd_list= list (applied_command (cb, list_object (l)), cmd_list);
}

void
vue_ink_widget_rep::do_layout () {
  Clay_ElementId my_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
  Clay_ElementData ed= Clay_GetElementData (my_id);
  CLAY(my_id, {
    .layout= { .sizing= { CLAY_SIZING_FIXED ((float) w), CLAY_SIZING_FIXED ((float) h) }},
    .border= { .width= { 1, 1, 1, 1 }, .color= palette[0] },
    .custom= { .customData= vue_render_widget },
    .userData= render_ref () }) {};

  if (!ed.found || mouse_action == "") return;
  bool over= Clay_PointerOver (my_id);
  if (!over && !dragging) return;
  double x= mouse_x - ed.boundingBox.x;
  double y= mouse_y - ed.boundingBox.y;
  point p (x, y);
  if (mouse_action == "press-right" && over) {
    // erase the strokes near the pointer
    int n= N(shs);
    contours nshs;
    for (int i=0; i<n; i++)
      if (!nearby (p, shs[i])) nshs << shs[i];
    shs= nshs;
    if (N(shs) != n) commit ();
    mouse_action= "";
  }
  else if (mouse_action == "press-left" && over) {
    poly_line sh (0);
    sh << p;
    shs << sh;
    dragging= true;
    mouse_action= "";
  }
  else if (dragging && (mouse_action == "move" || mouse_action == "release-left")) {
    poly_line& sh= shs [N(shs)-1];
    point q= sh [N(sh)-1];
    if (q[0] != x || q[1] != y) sh << p;
    if (mouse_action == "release-left") {
      dragging= false;
      commit ();
    }
    mouse_action= "";
  }
}

void
vue_ink_widget_rep::render (void *data) {
  vue_render_ren_data* d= (vue_render_ren_data*) data;
  renderer ren= d->ren;
  rectangle r= d->r;
  ren->set_background (rgb_color (255, 255, 240));
  ren->clear (r->x1, r->y1, r->x2, r->y2);
  ren->set_pencil (pencil (black, 2*ren->pixel));
  for (int i=0; i<N(shs); i++) {
    poly_line sh= shs[i];
    int n= N(sh);
    if (n == 0) continue;
    array<SI> x (max (n, 2)), y (max (n, 2));
    for (int j=0; j<n; j++) {
      x[j]= r->x1 + (SI) (sh[j][0] * ren->pixel);
      y[j]= r->y2 - (SI) (sh[j][1] * ren->pixel);
    }
    if (n == 1) { x[1]= x[0]; y[1]= y[0]; } // a single point is drawn as a dot
    ren->lines (x, y);
  }
}

widget
ink_widget (command cb) {
  return abstract (tm_new<vue_ink_widget_rep> (cb));
}

/******************************************************************************
* tree_view_widget
******************************************************************************/

// The data tree is displayed with one row per node, the children of the root
// being the top-level rows. The roles tree describes, for each tree label,
// which of the first children of a node hold the display string, the command
// string and the user data (same format as QTMTreeModel):
//   (tuple (label "DisplayRole" "CommandRole" "UserRole:1" ...) ...)
// the children of a node start after these role arguments.

class vue_tree_view_widget_rep: public vue_widget_rep {
  command cmd;
  tree    data;
  hashmap<int,int> nargs;             // number of role arguments per label
  hashmap<int,int> display_pos;       // position of the display string per label
  hashmap<int,int> command_pos;       // position of the command string per label
  hashmap<int,array<int> > user_pos;  // positions of the user data per label
  hashset<pointer> expanded;          // the expanded nodes

public:
  vue_tree_view_widget_rep (command _cmd, tree _data, tree roles);
  void do_layout ();

private:
  int    row_offset (tree t);
  bool   has_children (tree t);
  string node_label (tree t);
  void   layout_node (tree t, int depth);
  void   activate (tree t, int buttons);
};

vue_tree_view_widget_rep::vue_tree_view_widget_rep (command _cmd, tree _data, tree roles)
  : vue_widget_rep ("tree_view_widget"), cmd (_cmd), data (_data),
    nargs (0), display_pos (-1), command_pos (-1), user_pos (array<int> ())
{
  if (is_compound (roles))
    for (int i=0; i<N(roles); i++) {
      if (!is_compound (roles[i])) continue;
      int tag= (int) L(roles[i]);
      nargs (tag)= N(roles[i]);
      for (int j=0; j<N(roles[i]); j++) {
        if (!is_atomic (roles[i][j])) continue;
        string role= roles[i][j]->label;
        if (role == "DisplayRole") display_pos (tag)= j;
        else if (role == "CommandRole") command_pos (tag)= j;
        else if (starts (role, "UserRole:")) {
          int num= max (0, min (9, as_int (role (9, N(role))) - 1));
          array<int> a= user_pos [tag];
          while (N(a) <= num) a << -1;
          a[num]= j;
          user_pos (tag)= a;
        }
      }
    }
  expanded->insert ((pointer) data.operator-> ());
}

int
vue_tree_view_widget_rep::row_offset (tree t) {
  return is_compound (t) ? nargs [(int) L(t)] : 0;
}

bool
vue_tree_view_widget_rep::has_children (tree t) {
  return is_compound (t) && N(t) > row_offset (t);
}

string
vue_tree_view_widget_rep::node_label (tree t) {
  if (is_atomic (t)) return t->label;
  int pos= display_pos [(int) L(t)];
  if (pos >= 0 && pos < N(t) && is_atomic (t[pos])) return t[pos]->label;
  return as_string (L(t));
}

void
vue_tree_view_widget_rep::activate (tree t, int buttons) {
  // same arguments as the Qt implementation:
  // (user-data-n ... user-data-1 command-or-subtree mouse-buttons)
  object args= list_object (object (buttons));
  int pos= is_compound (t) ? command_pos [(int) L(t)] : -1;
  if (pos >= 0 && pos < N(t) && is_atomic (t[pos]))
    args= cons (object (t[pos]->label), args);
  else
    args= cons (object (t), args);
  if (is_compound (t)) {
    array<int> a= user_pos [(int) L(t)];
    for (int i=0; i<N(a); i++)
      if (a[i] >= 0 && a[i] < N(t) && is_atomic (t[a[i]]))
        args= cons (object (t[a[i]]->label), args);
  }
  cmd_list= list (applied_command (cmd, args), cmd_list);
}

void
vue_tree_view_widget_rep::layout_node (tree t, int depth) {
  pointer key= (pointer) t.operator-> ();
  uint32_t hash= (uint32_t) (((uintptr_t) key) >> 4);
  bool kids= has_children (t);
  bool open= expanded->contains (key);
  Clay_ElementId toggle_id= CLAY_IDI ("tree_view_toggle", hash);
  Clay_ElementId label_id=  CLAY_IDI ("tree_view_label", hash);
  ui_signal tsig { .clicked= 0 };
  if (kids) tsig= button_logic (toggle_id);
  ui_signal lsig= button_logic (label_id);
  CLAY_AUTO_ID({
    .layout= {
      .layoutDirection= CLAY_LEFT_TO_RIGHT,
      .padding= { (uint16_t) (16*depth), 0, 0, 0 },
      .sizing= { .width= CLAY_SIZING_GROW(0) },
      .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }}})
  {
    CLAY(toggle_id, {
      .layout= {
        .sizing= { CLAY_SIZING_FIXED(24), CLAY_SIZING_FIT(0) },
        .padding= { 4, 4, 2, 2 }}})
    {
      if (kids) layout_text (open ? "<#25BE>" : "<#25B8>", 0, dark_grey);
    }
    CLAY(label_id, {
      .layout= { .padding= { 4, 8, 2, 2 }, .sizing= { .width= CLAY_SIZING_GROW(0) }},
      .backgroundColor= (hot_id == label_id.id) ? color_highlight : color_field })
    {
      layout_text (node_label (t), 0, black);
    }
  }
  if (tsig.clicked == 1) {
    if (open) expanded->remove (key);
    else expanded->insert (key);
    open= !open;
  }
  if (lsig.clicked != 0) {
    static const int buttons[4]= { 0, 1, 4, 2 }; // none, left, middle, right (Qt encoding)
    activate (t, buttons[lsig.clicked]);
  }
  if (kids && open)
    for (int i= row_offset (t); i<N(t); i++)
      layout_node (t[i], depth+1);
}

void
vue_tree_view_widget_rep::do_layout () {
  CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
    .backgroundColor= color_field,
    .layout= {
      .layoutDirection= CLAY_TOP_TO_BOTTOM,
      .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_FIT(0) }}})
  {
    // the root itself is not displayed
    for (int i= row_offset (data); i<N(data); i++)
      layout_node (data[i], 0);
  }
}

widget
tree_view_widget (command cmd, tree data, tree data_roles) {
  return abstract (tm_new<vue_tree_view_widget_rep> (cmd, data, data_roles));
}

//-----------------------------------------------------------------------------

// toplevel window constructor

widget plain_window_widget (widget wid, string s, command quit) {
  // the file chooser is the system dialog: no window of ours around it
  // (the type name is the one of its constructor; a mismatch here opened
  // an empty TeXmacs window next to the file panel)
  if (dynamic_cast<vue_chooser_widget_rep*> (wid.rep) != NULL) {
    vue_chooser_widget_rep* cw= dynamic_cast<vue_chooser_widget_rep*> (wid.rep);
    cw->win_title= s;
    cw->quit= quit;
    return wid;
  } else if (dynamic_cast<vue_inputs_list_widget_rep*> (wid.rep) != NULL) {
    vue_inputs_list_widget_rep* cw= dynamic_cast<vue_inputs_list_widget_rep*> (wid.rep);
    cw->win_title= s;
//    cw->quit= quit;  // we already have a command
    return wid;
  } else {
    // the size is chosen by the window itself: dialogs follow their
    // contents, the main window and the popups have their own rules
    // (see vue_plain_window_widget_rep::post_layout)
    vue_plain_window_widget_rep *wwid= tm_new<vue_plain_window_widget_rep> (wid, s, quit);
    plain_window (wwid, s);
    return abstract (wwid);
  }
}
  
// undecorated windows for popup menus and tooltips; they are sized to their
// contents (see vue_plain_window_widget_rep::post_layout) and dismissed when
// the pointer leaves them (see SDL_EVENT_WINDOW_MOUSE_LEAVE in vue_gui.cpp)
widget
popup_window_widget (widget w, string s) {
  vue_plain_window_widget_rep *wwid=
    tm_new<vue_plain_window_widget_rep> (w, s, command (), true);
  plain_window (wwid, s, true);
  return abstract (wwid);
}

widget
tooltip_window_widget (widget w, string s) {
  return popup_window_widget (w, s);
}

void destroy_window_widget (widget w) {
  vue_widget vw= concrete(w);
  if (DEBUG_VUE) debug_widgets << "destroy_window_widget on " << vw->type << LF;
  vue_plain_window_widget_rep *ww= dynamic_cast<vue_plain_window_widget_rep*> (vw.rep);
  vue_inputs_list_widget_rep *il= dynamic_cast<vue_inputs_list_widget_rep*> (vw.rep);
  if (ww) {
    tm_delete (ww->win);
    ww->win= NULL; // the widget may outlive its window (Scheme holds it)
  } else if (il) {
    // the dialog is shown in a window of its own
    if (!is_nil (il->win_widget)) destroy_window_widget (il->win_widget);
    il->win_widget= widget ();
  } else {
    cout << "not a window widget!" << LF;
  }
}
// destroys a window as created by the above routines

