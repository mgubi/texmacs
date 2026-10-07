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
#include "boot.hpp"      // for get_user_preference (the icon bars)
#include "poly_line.hpp" // for ink widget

#include "../MuPDF/mupdf_picture.hpp"
#include "vue_gpu.hpp"
extern bool vue_profile_on; // TEXMACS_VUE_PROFILE (vue_gui.cpp)
#include "../MuPDF/mupdf_renderer.hpp" // draw_picture_scaled (the smooth zoom)

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
#ifdef __EMSCRIPTEN__
#include <emscripten.h> // EM_ASM (the input area of the page)
#endif
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
// only to be measured): a static label, the numbers go in the offset. The
// widget and the index are hashed one after the other, as Clay does for
// the children of an element (CLAY_IDI_LOCAL): packed into one number, as
// id * 4096 + k, they wrapped around past a million widgets, and a 4097th
// probe landed on the first probe of the next widget
static inline Clay_ElementId
probe_id (const char* label, unsigned int id, unsigned int k) {
  Clay_String cs= CLAY__INIT(Clay_String) { .isStaticallyAllocated= true, .length= (int32_t) strlen (label), .chars= label };
  Clay_ElementId base= Clay__HashStringWithOffset (cs, id, 0);
  return Clay__HashStringWithOffset (cs, k, base.id);
}


// The sizes of the interface written as numbers in this file (paddings,
// gaps, corner radii, the heights of the bars...) are those of a 2x display
// in device pixels, where they were tuned; the layout is in device pixels.
// ui_px scales them to the density of the window being laid out
// (retina_factor, 1 or 2, set for each window: vue_gui.hpp), so that they
// are the same in points at 1x: raw, they made the bars and the buttons
// twice too large on a 1x display (and in a browser at a ratio of 1).
static inline float
ui_pxf (float v) {
  return v * retina_factor / 2.0f;
}

static inline uint16_t
ui_px (float v) {
  if (v <= 0) return 0;
  float r= v * retina_factor / 2.0f;
  return (uint16_t) (r < 1.0f ? 1.0f : floorf (r + 0.5f));
}

// the rounded corners of the fields, lists and menus (the theme's radius,
// by a factor k for the elements inside them, which follow their curve)
static inline Clay_CornerRadius
ui_corners (float k= 1.0f) {
  return CLAY_CORNER_RADIUS (ui_pxf (the_theme.radius * k));
}

// the pull-down menus are rounder than the fields
static const float menu_round= 1.5f;
// the room between the border of a menu and its items (2x)
static const float menu_inset= 10;

// The corners of an element which lies inset (2x) in a container rounded
// as ui_corners (k): concentric with those of the container, their radius
// is the container's less the inset
static inline Clay_CornerRadius
ui_inner_corners (float k, float inset) {
  float r= ui_pxf (the_theme.radius * k) - (float) ui_px (inset);
  return CLAY_CORNER_RADIUS (r > 0 ? r : 0);
}

// the footer is being laid out: its buttons are flatter (menu_button,
// layout_pull_button)
static bool in_footer= false;

// the tool bars (main, mode, focus, user) are being laid out: their buttons
// and the gaps between them are tighter than elsewhere (2x values: ui_px)
static bool in_tool_bar= false;
#define tool_button_pad 4
#define tool_button_gap 4

// The main and mode icon bars in columns at the left of the editor, rather
// than in rows above it: the preference "icon bars" (left or top, in the
// General tab of the preferences), read at each layout, so that a change
// shows at once; TEXMACS_VUE_BARS=top or left (?bars=top in the browser)
// overrides it. While they are laid out, the rows of a bar (its horizontal
// menus and lists) go from top to bottom, its separators are horizontal and
// its pull-down menus open to the right
static bool in_side_bar= false;
static bool
bars_on_side () {
  static string forced= get_env ("TEXMACS_VUE_BARS");
  if (forced == "top") return false;
  if (forced == "left") return true;
  // (above the document by default on the desktop, as before the columns)
#ifdef __EMSCRIPTEN__
  return get_user_preference ("icon bars", "left") != "top";
#else
  return get_user_preference ("icon bars", "top") == "left";
#endif
}

// The context menu of the editor (texmacs-popup-menu, the Focus menu) at a
// point of the screen (SI, y upwards as for set_position): a right click
// on a tag of the interactive footer, once the tag is selected (the
// command of the tag runs first). One at a time: a new one replaces the
// last, as the popup of the editor (edit_interface_rep::mouse_adjust)
static widget footer_popup_wid;

// a context menu at a point of the screen (the editor's, that of an input
// field): it replaces the one shown before, if any
static void
show_context_menu (widget menu, SI x, SI y) {
  if (!is_nil (footer_popup_wid)) {
    set_visibility (footer_popup_wid, false);
    destroy_window_widget (footer_popup_wid);
    footer_popup_wid= widget ();
  }
  footer_popup_wid= popup_window_widget (popup_widget (menu), "Popup menu");
  set_position (footer_popup_wid, x, y);
  set_visibility (footer_popup_wid, true);
}

class footer_popup_command_rep : public command_rep {
  SI x, y;
public:
  footer_popup_command_rep (SI x2, SI y2) : x (x2), y (y2) {}
  void apply () {
    show_context_menu (make_menu_widget (eval ("'(vertical (link texmacs-popup-menu))")), x, y);
  }
  void apply (object arg) { (void) arg; apply (); }
  tm_ostream& print (tm_ostream& out) { return out << "<footer_popup_command>"; }
};

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

// (the colours of before; rounder corners)
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
  .cursor= {224, 0, 0, 255},
  .radius= 12
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
  .cursor= {255, 96, 96, 255},
  .radius= 12
};

vue_theme the_theme= vue_theme_light;

// the TeXmacs colour of a colour of the theme (for the text routines)
color
theme_color (Clay_Color c) {
  return rgb_color ((int) c.r, (int) c.g, (int) c.b, (int) c.a);
}

// the colours the widgets use; they are the fields of the current theme
Clay_Color palette[4];
Clay_Color color_background, color_text, color_border;
Clay_Color color_field, color_button, color_button_hover, color_button_down;
// The colour behind the buttons being laid out. The bars of the main
// window have different greys, and the highlight must show on each of them
// (see highlight_on); with_behind sets it for the contents of a bar.
Clay_Color color_behind;
struct with_behind {
  Clay_Color saved;
  with_behind (Clay_Color c) : saved (color_behind) { color_behind= c; }
  ~with_behind () { color_behind= saved; }
};
Clay_Color color_pressed;

static void
vue_apply_theme () {
  for (int i= 0; i < 4; i++) palette[i]= the_theme.shade[i];
  color_background= the_theme.background;
  color_behind= the_theme.background;
  color_text= the_theme.text;
  color_border= the_theme.border;
  color_field= the_theme.field;
  color_button= the_theme.button;
  color_button_hover= the_theme.button_hover;
  color_button_down= the_theme.button_down;
  color_pressed= the_theme.pressed;
}

static float
clamp_channel (float v) { return v < 0 ? 0 : (v > 255 ? 255 : v); }

// The highlight of a flat element, over the colour it rests on. The theme
// gives one colour, a step away from its own background -- the light theme
// lightens 192 to 240 -- and that colour is used wherever it shows, which
// is anything half a step or more away from it. The focus bar, at 232, is
// not: there the step is taken from the bar itself instead of from the
// background of the theme, and only half of it, since a highlight which is
// merely lighter than a light bar needs no more; that keeps the direction
// of the theme, so hovering lightens everywhere it can. Only where there
// is no room left for it, a field which is already almost white, does the
// step go the other way.
static float
channel_gap (Clay_Color a, Clay_Color b) {
  float g= fabsf (a.r - b.r);
  if (fabsf (a.g - b.g) > g) g= fabsf (a.g - b.g);
  if (fabsf (a.b - b.b) > g) g= fabsf (a.b - b.b);
  return g;
}

static Clay_Color
highlight_on (Clay_Color bg) {
  Clay_Color h= the_theme.highlight, b= the_theme.background;
  float dr= h.r - b.r, dg= h.g - b.g, db= h.b - b.b;
  float step= channel_gap (h, b);
  if (step <= 0) return h;
  float sep= step / 2;                   // enough of a difference to see
  if (channel_gap (h, bg) >= sep) return h;
  float k= sep / step;                   // half a step, in the same direction
  Clay_Color up= { clamp_channel (bg.r + k * dr), clamp_channel (bg.g + k * dg),
                   clamp_channel (bg.b + k * db), h.a };
  if (channel_gap (up, bg) >= 0.75f * sep) return up;
  return (Clay_Color) { clamp_channel (bg.r - k * dr), clamp_channel (bg.g - k * dg),
                        clamp_channel (bg.b - k * db), h.a };
}

// The background which a flat element shows when it is not highlighted.
// Clay interpolates the four channels of a colour, so fading a highlight
// out to a transparent *black* takes it through a dark grey: what is drawn
// while the animation runs is a shadow of the highlight over the bar, which
// reads as a flicker. Keeping the colour and dropping only the alpha makes
// the fade a plain blend from the highlight into the bar behind it.
static Clay_Color
faded (Clay_Color c) { return (Clay_Color) { c.r, c.g, c.b, 0 }; }

// The menus and lists appear with a short animation: they drop
// into place from a few pixels above, and their contents fade in. The
// renderers have no opacity for a group, so the fade is a veil: a box in
// the colour of the menu, over its contents, which Clay takes from opaque
// to transparent (menu_veil).
static Clay_TransitionData
drop_in (Clay_TransitionData target, Clay_TransitionProperty props) {
  (void) props; target.boundingBox.y -= ui_pxf (10); return target;
}
static Clay_TransitionData
veil_opaque (Clay_TransitionData target, Clay_TransitionProperty props) {
  (void) props; target.backgroundColor.a= 255; return target;
}
static const Clay_TransitionElementConfig drop_in_transition= {
  .handler= Clay_EaseOut, .duration= 0.14f,
  .properties= CLAY_TRANSITION_PROPERTY_Y,
  .enter= { .setInitialState= drop_in,
            .trigger= CLAY_TRANSITION_ENTER_TRIGGER_ON_FIRST_PARENT_FRAME }};

// the veil over the contents of a floating element which has just appeared
// (it is new: Clay animates it from veil_opaque); it lets the pointer through
static void
menu_veil (Clay_ElementId parent, Clay_Color bg, Clay_CornerRadius r, int16_t z) {
  CLAY(CLAY_IDI ("menu_veil", parent.id), {
    .layout= { .sizing= { CLAY_SIZING_GROW (0), CLAY_SIZING_GROW (0) }},
    .backgroundColor= { bg.r, bg.g, bg.b, 0 },
    .cornerRadius= r,
    .floating= {
      .zIndex= z,
      .attachTo= CLAY_ATTACH_TO_PARENT,
      .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH },
    .transition= {
      .handler= Clay_EaseOut, .duration= 0.18f,
      .properties= CLAY_TRANSITION_PROPERTY_BACKGROUND_COLOR,
      .enter= { .setInitialState= veil_opaque,
                .trigger= CLAY_TRANSITION_ENTER_TRIGGER_ON_FIRST_PARENT_FRAME }}}) {}
}

// a colour a fraction t of the way from a to b (a line between two colours
// of the theme, a frame which is to be fainter than the border)
static Clay_Color
mix_colors (Clay_Color a, Clay_Color b, float t) {
  return (Clay_Color) { a.r + t * (b.r - a.r), a.g + t * (b.g - a.g),
                        a.b + t * (b.b - a.b), a.a + t * (b.a - a.a) };
}

// Counts the changes of the icon theme: a picture widget which holds an
// icon of an older generation loads it again (see icon_picture).
static int icon_generation= 0;

#ifdef __EMSCRIPTEN__
// the class tm-dark of the page: the dark colours of its frame (the tabs,
// the menu of TeXmacs Vue, its dialogs); tm-theme-set tells the frame that
// TeXmacs chose (before, the frame follows the system)
EM_JS (void, vue_web_frame_theme, (int dark), {
  if (typeof document === 'undefined') return; // the node build: no page
  var c = document.documentElement.classList;
  c.add ('tm-theme-set');
  if (dark) c.add ('tm-dark'); else c.remove ('tm-dark');
});
#endif

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
  // TEXMACS_VUE_RADIUS overrides the rounding of the corners (0: square)
  string radius= get_env ("TEXMACS_VUE_RADIUS");
  if (N(radius) > 0 && is_double (radius))
    the_theme.radius= max (0.0, as_double (radius));
  vue_apply_theme ();
  // the vector icons come in a light and a dark set: the widgets which were
  // built with the other one load theirs again (see icon_picture)
  string icons= dark ? string ("dark") : string ("light");
  if (icons != mupdf_get_icon_theme ()) {
    mupdf_set_icon_theme (icons);
    icon_generation++;
  }
#ifdef __EMSCRIPTEN__
  // the frame of the page (misc/wasm/frame.js) in the same theme
  vue_web_frame_theme (dark ? 1 : 0);
#endif
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
int mouse_clicks= 1; // the count of the clicks of a press (2: double click)
unsigned int mouse_state= 0;
unsigned int mouse_presses= 0; // the presses of a button so far
array<double> mouse_data;

bool current_popup; // is there an active popup?
bool cancel_popup;  // should we cancel popups?
uint32_t open_pull_id= 0; // the pull button whose menu is open (0: none)
uint32_t open_pull_bar= 0; // the bar of that button
uint32_t current_bar= 0;  // the horizontal menu being laid out
uint32_t current_menu= 0; // the open menu being laid out (its float)
bool menu_press= false;   // the pass has a press, maybe taken by the menus
bool menu_hovered= false; // an item of an open menu is under the pointer
array<uint32_t> menu_zones_now; // the open menus and their buttons
// the pointer is over the open list of an enum (it floats above the menu
// which has the enum, where Clay does not see the pointer over the menu)
static bool pointer_on_enum_list= false;
// A title of a bar opens its menu as it is pressed, and the button may be
// held and dragged down to an item, which is chosen by the release (see
// layout_pull_button): while such a drag lasts, the title gives up the
// capture of the pointer, so that the items are hovered, and a release on
// an item of an open menu is a click even though the press was elsewhere
static bool menu_drag= false;
static bool menu_drag_ends= false; // the pass has the release ending it
static void* menu_drag_window= NULL; // the window of the drag

// The refresh messages (SLOT_REFRESH, refresh-now) reach a window, which
// lays out its widgets once afterwards; a refresh or refreshable widget
// which is not laid out in that pass (a hidden tool panel, a menu which is
// closed) must still see the message the next time it is. Every message
// gets a number, and each kind remembers the number of the last message
// of that kind; a widget remembers the number at its last refresh, and it
// is stale when a message of its kind (or "any") came later. Qt sends the
// message to every widget alive (tmSlotRefresh); this is the same, lazily.
static int refresh_serial= 0;
static hashmap<string,int> refresh_stamps (0);

static bool
refresh_stale (string kind, int stamp) {
  if (kind == "any") return refresh_serial > stamp; // any message at all
  return refresh_stamps["any"] > stamp || refresh_stamps[kind] > stamp;
}

// some more context during layout
Clay_ElementId last_id;
bool debug_clay=false;

// ask the buttons to fit all horizontal space (items of vertical menus)
bool button_grow= false;
// a text input fills the width it is given rather than taking its own (the
// input of an editable enum, whose width includes the arrow)
static bool input_fill= false;
// the type of the last widget which began to lay itself out: the only clue
// the Clay error handler has about where an error came from, since Clay
// says nothing about the element it was working on (see HandleClayErrors)
string layout_who;
// set while laying out what a resize widget contains: a widget which would
// otherwise take the size of its contents fills the box instead
bool fill_parent= false;

uint32_t current_balloon;
time_t balloon_time;
static unsigned int balloon_presses= 0; // mouse_presses when it was hovered

// list of commands
list<command> cmd_list;

vue_window current_window; // used during layout to propagate information
bool window_autosizing= false; // the window is being sized to its contents
// the contents of the window changed size (another tab): a window which was
// sized to its contents is sized to them again, as QTMTabWidget::resizeOthers
static bool refit_window= false;
int context_style= 0; // style flags (bold, grey) added by the enclosing divisions
// the flags of a container which its texts inherit (user_canvas_widget,
// resize_widget), as the style sheet of a container does in Qt
static const int inherited_styles= WIDGET_STYLE_MINI | WIDGET_STYLE_MONOSPACED |
  WIDGET_STYLE_GREY | WIDGET_STYLE_INERT | WIDGET_STYLE_BOLD;
// the resize widgets whose initial scrolling position has been set (by id)
static hashset<int> resize_positioned;
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
  mouse_clicks= in.mouse_clicks;
  mouse_data= in.mouse_data;
  current_popup= in.current_popup;
  cancel_popup= in.cancel_popup;
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
  in.mouse_clicks= mouse_clicks;
  in.mouse_data= mouse_data;
  in.current_popup= current_popup;
  in.cancel_popup= cancel_popup;
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

static bool wheel_pending= false; // the pass was given the wheel

void
gui_init_context() {
  load_input_state (current_window);
  wheel_pending= (mouse_action == "wheel");
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

  // the menus (see layout_pull_button): a press outside the open ones
  // closes them, and neither it nor the moves and the release which follow
  // it go to what is under the pointer, as on the Mac; Escape closes them
  vue_input_state& in= current_window->input;
  if (in.swallow_mouse) {
    if (starts (mouse_action, "release-")) {
      mouse_action= "";
      in.swallow_mouse= false;
    }
    else if (mouse_action == "move") mouse_action= "";
  }
  menu_press= starts (mouse_action, "press-");
  // a drag from a title of a bar ends with the release of the button, or
  // when the buttons are found up (the release went to another window)
  // (only in the passes of the window of the drag: the buttons are up in
  // the passes of the other windows which come between the release and
  // the pass of that window which takes it)
  if (menu_drag && menu_drag_window == (void*) current_window) {
    menu_drag_ends= starts (mouse_action, "release-");
    if (!menu_drag_ends && (mouse_state & 7) == 0 &&
        !starts (mouse_action, "press-"))
      menu_drag= false;
  }
  else menu_drag_ends= false;
  if (N(in.menu_zones) > 0) {
    if (menu_press) {
      bool inside= false;
      for (int i=0; i<N(in.menu_zones); i++)
        if (Clay_PointerOver ((Clay_ElementId) { .id= in.menu_zones[i] }))
          inside= true;
      if (!inside) {
        mouse_action= "";
        in.swallow_mouse= true;
      }
    }
    if (key_event == "escape") {
      cancel_popup= true;
      key_event= "";
    }
  }
  menu_zones_now= array<uint32_t> ();
  pointer_on_enum_list= false;
  menu_hovered= false;
  current_bar= current_menu= 0;
  
  // the refresh messages which came since the last pass: they are numbered
  // (refresh_stale), so that a widget which is not laid out now sees them
  // when it next is
  current_window->refresh_kinds= current_window->next_refresh_kinds;
  current_window->next_refresh_kinds= hashset<string>();
  iterator<string> it= iterate (current_window->refresh_kinds);
  while (it->busy ()) refresh_stamps (it->next ())= ++refresh_serial;
}

void
gui_finalize_context() {
  if (starts (mouse_action, "release-")) {
    // deactivate elements, probably we released a button away from the active element
    active_button= 0;
    active_id= 0;
  }
  if (menu_drag_ends) menu_drag= menu_drag_ends= false;
  // the wheel was used by a widget (see clay_wheel_flush in vue_gui.cpp)
  if (wheel_pending && mouse_action != "wheel")
    current_window->input.wheel_taken= true;
  wheel_pending= false;
  // the menus open in this pass; the pointer left their items
  current_window->input.menu_zones= menu_zones_now;
  if (!menu_hovered) current_window->input.hover_item= 0;
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
      // the release which ends a drag from a title of a bar chooses the
      // item of the open menu it is over (see menu_drag)
      bool dragged= menu_drag && i == 1 && active_id == 0 && current_menu != 0;
      if ((mouse_action == r[i]) &&
          (dragged || ((active_id == id.id) && (active_button == i)))) {
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
// Tabs whose presentation is the preference "gui:responsive tab mode", read
// when they are made (as the look and feel, a change shows in the next
// ones): "top" plain tabs, "side" a column of tabs on the left of the page
// (the default, as in the Qt port), "mobile" the list of the tabs, a page
// replacing it with a button back to the list, "grid" all the pages at
// once, in two columns (three in a wide window). Qt has the four modes in
// QTMResponsiveTabWidget but always shows the side one (the mobile one on
// Android). TEXMACS_VUE_TAB_MODE overrides the preference (the tests)
static string
responsive_tab_mode () {
  string mode= get_env ("TEXMACS_VUE_TAB_MODE");
  if (mode == "") mode= get_preference ("gui:responsive tab mode", "side");
  if (mode != "top" && mode != "mobile" && mode != "grid") mode= "side";
  return mode;
}
VUE_WIDGET(responsive_tabs_widget, array<url>, us, array<widget>, tabs, array<widget>, bodies,
                                   string, mode);
VUE_WIDGET_DATA(responsive_tabs_widget_star, array<widget>, tabs, array<widget>, icons,
                array<widget>, bodies, int, current, string, mode, bool, viewing);
widget responsive_tabs_widget (array<widget> tabs, array<widget> bodies) {
  string mode= responsive_tab_mode ();
  if (mode == "top") return tabs_widget (tabs, bodies);
  vue_responsive_tabs_widget d { .tabs= tabs, .bodies= bodies, .mode= mode };
  return vue_create<vue_responsive_tabs_widget> ("responsive_tabs_widget", d);
}
widget responsive_icon_tabs_widget (array<url> us, array<widget> ss, array<widget> bs) {
  string mode= responsive_tab_mode ();
  if (mode == "top") return icon_tabs_widget (us, ss, bs);
  vue_responsive_tabs_widget d { .us= us, .tabs= ss, .bodies= bs, .mode= mode };
  return vue_create<vue_responsive_tabs_widget> ("responsive_tabs_widget", d);
}
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

// The icon set of the preferences, followed at once rather than at the next
// start of TeXmacs: when it changes, it is put on the path of the icons
// (apply_icon_set) and the widgets load their icons again (icon_picture);
// the icons loaded are kept per set (load_xpm), a change back costs nothing.
// The loop calls it at each iteration: a lookup in the preferences
void
vue_follow_icon_set () {
  static string current;
  string now= get_user_preference ("icon set", "lucide");
  if (N(current) == 0) { current= now; return; }
  if (now == current) return;
  current= now;
  apply_icon_set ();
  icon_generation++;
  gui_needs_relayout= true;
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
                bool, placed, bool, flip, float, shift_x, float, shift_y,
                float, win_w, float, win_h, float, menu_w, float, menu_h);
// placed: the position of the open menu has been decided (see layout_pull_button)
// flip: the menu is opened on the other side of the button to stay in the window
// shift_x, shift_y: shift of the menu to keep it inside the window
// win_w, win_h, menu_w, menu_h: the window and the menu the decision was
// taken for (it is taken again when the window is resized, or when the
// menu changes size, while the menu is open: a part of it refreshed by a
// choice made in it, the typographic palette of the colour menus)
// data for a button w with a lazy pulldown menu pw and a cached value
VUE_WIDGET_DATA(cached_glue_widget, picture, pic, tree, col, bool, hx, bool, vx, SI, w, SI, h);

VUE_WIDGET_DATA(tabs_widget_star, array<widget>, tabs, array<widget>, icons, array<widget>, bodies, int, current);

VUE_WIDGET_DATA(refreshable_widget_star, object, prom, string, kind, widget, current, object, curobj, int, stamp);

VUE_WIDGET_DATA(refresh_widget_star, string, tmwid, string, kind, widget, current, object, curobj, int, stamp,
                array<object>, cache_keys, array<widget>, cache_widgets);
// stamp: the number of the last refresh message the widget has seen (see
// refresh_stale); cache_keys, cache_widgets: the menus the widget has
// built, by their expansion (its own cache, as Qt's QTMRefreshWidget has:
// one cache for all of them gave a widget to several parents at once)

VUE_WIDGET_DATA(split_widget_star, widget, a, widget, b, float, pos, bool, dragging);
// hsplit/vsplit widgets with the position of the divider (in pixels, <0 if unset)

VUE_WIDGET_DATA(enum_widget_star, command, cb, array<string>, vals, string, val, int, st, string, w, bool, open,
                bool, editable, widget, input);
// an enum widget with the state of its dropdown list; an editable one (as
// Qt, when the value or the last of the values is empty) has a text input
// in place of the value

VUE_WIDGET_DATA(filtered_choice_widget_star, command, cb, array<string>, vals, string, val, widget, input);
// a filtered choice widget with the input field used for the filter

VUE_WIDGET_DATA(printer_widget_star, command, cmd, url, ps_pdf_file, widget, content, widget, printer, widget, copies, widget, pages, widget, orientation, string, options_for);
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

// Markers at the ends of a clipped container whose contents do not fit: a
// strip of the colour behind, opaque at the very edge and fading over the
// contents, with a chevron in it. A scroll bar would take room a bar which
// is already too narrow does not have, and would lie over the items of a
// menu; the markers only say that there is more that way, and a click on
// one brings it into view. z: drawn above the container, which may itself
// float (a menu).
static const float marker_fade[]= { 255, 200, 140, 80, 30 };
static const float marker_cell[]= { 22, 5, 5, 5, 5 };

// the chevron of a marker, pointing the way the hidden contents are: 0
// left, 1 right, 2 up, 3 down. It is drawn rather than written because the
// interface font has no left-pointing triangle (Lucida Grande has U+25B4,
// U+25B8 and U+25BE but not U+25C2), and a drawn one is sharp at every
// resolution; y grows upwards here (see the rectangle of a render command)
static void
render_marker_fn (renderer ren, void* data, rectangle r) {
  int dir= (int) (intptr_t) data;
  SI px= ren->pixel;
  SI cx= (r->x1 + r->x2) / 2, cy= (r->y1 + r->y2) / 2;
  SI a= max (2*px, min (r->x2 - r->x1, r->y2 - r->y1) / 5);
  ren->set_pencil (pencil (theme_color (the_theme.text_grey), 2*px, cap_round));
  switch (dir) {
    case 0:
      ren->line (cx + a, cy + a, cx - a, cy);
      ren->line (cx - a, cy, cx + a, cy - a); break;
    case 1:
      ren->line (cx - a, cy + a, cx + a, cy);
      ren->line (cx + a, cy, cx - a, cy - a); break;
    case 2:
      ren->line (cx - a, cy - a, cx, cy + a);
      ren->line (cx, cy + a, cx + a, cy - a); break;
    default:
      ren->line (cx - a, cy + a, cx, cy - a);
      ren->line (cx, cy - a, cx + a, cy + a); break;
  }
}

// a solid triangle, as the characters U+25B8, U+25BE... of the arrows of the
// widgets: data is dir as for render_marker_fn, plus 4 when it is grey
static void
render_triangle_fn (renderer ren, void* data, rectangle r) {
  int code= (int) (intptr_t) data, dir= code & 3;
  SI cx= (r->x1 + r->x2) / 2, cy= (r->y1 + r->y2) / 2;
  SI a= (min (r->x2 - r->x1, r->y2 - r->y1) * 5) / 16; // half the long side
  SI b= (a * 7) / 8;                             // half the height
  array<SI> x (3), y (3);
  switch (dir) {
    case 0:  x[0]= cx + b; y[0]= cy + a; x[1]= cx + b; y[1]= cy - a;
             x[2]= cx - b; y[2]= cy; break;
    case 1:  x[0]= cx - b; y[0]= cy + a; x[1]= cx - b; y[1]= cy - a;
             x[2]= cx + b; y[2]= cy; break;
    case 2:  x[0]= cx - a; y[0]= cy - b; x[1]= cx + a; y[1]= cy - b;
             x[2]= cx; y[2]= cy + b; break;
    default: x[0]= cx - a; y[0]= cy + b; x[1]= cx + a; y[1]= cy + b;
             x[2]= cx; y[2]= cy - b; break;
  }
  Clay_Color c= (code & 4) ? the_theme.text_grey : the_theme.text;
  ren->set_brush (theme_color (c));
  ren->polygon (x, y, true);
}

// An arrow of the widgets (dir as for render_marker_fn): the character of
// the interface font when it has one, a drawn solid triangle otherwise
// (Fira, the font of the browser, has no U+25B8 and no U+25BE)
static void
layout_arrow (string glyph, int dir, color c) {
  font fn= get_default_styled_font (0);
  if (fn->supports (glyph)) { layout_text (glyph, 0, c); return; }
  float s= (float) retina_factor * ((fn->y2 - fn->y1) / 3) / PIXEL;
  int grey= (c == black) ? 0 : 4;
  CLAY_AUTO_ID({
    .layout= { .sizing= { CLAY_SIZING_FIXED(s), CLAY_SIZING_FIXED(s) }},
    .custom= { .customData= (void*) &render_triangle_fn },
    .userData= (void*) (intptr_t) (dir + grey) }) {}
}

static void
scroll_markers (Clay_ElementId id, Clay_ScrollContainerData& sd,
                Clay_Color bg, bool horizontal, int16_t z= 1,
                float radius= 0) {
  // radius: the corners of a rounded container, which the opaque cell of a
  // marker, at its edge, follows
  // Clay clamps the position of a scroll container only while it handles a
  // wheel event: a bar or a menu which fits again, because the window was
  // made larger, would stay where it had been scrolled to, with its first
  // items out of reach and no marker left to say where they went. Clamping
  // it at every layout is what brings them back.
  float over_x= max (0.0f, sd.contentDimensions.width -
                           sd.scrollContainerDimensions.width);
  float over_y= max (0.0f, sd.contentDimensions.height -
                           sd.scrollContainerDimensions.height);
  sd.scrollPosition->x= min (max (sd.scrollPosition->x, -over_x), 0.0f);
  sd.scrollPosition->y= min (max (sd.scrollPosition->y, -over_y), 0.0f);
  float view=    horizontal ? sd.scrollContainerDimensions.width
                            : sd.scrollContainerDimensions.height;
  float content= horizontal ? sd.contentDimensions.width
                            : sd.contentDimensions.height;
  float cross=   horizontal ? sd.scrollContainerDimensions.height
                            : sd.scrollContainerDimensions.width;
  float* pos=    horizontal ? &sd.scrollPosition->x : &sd.scrollPosition->y;
  if (content <= view + 1 || view <= 0) return;
  float total= 0;
  for (int i=0; i<5; i++) total += ui_pxf (marker_cell[i]);
  for (int end= 0; end < 2; end++) {
    // how much is out of view before the first item, and after the last
    float hidden= (end == 0) ? -*pos : content - view + *pos;
    if (hidden <= 1) continue;
    // one id per side and per axis: a menu has markers on both
    Clay_ElementId m_id=
      horizontal ? ((end == 0) ? CLAY_IDI ("scroll_marker_left", id.id)
                               : CLAY_IDI ("scroll_marker_right", id.id))
                 : ((end == 0) ? CLAY_IDI ("scroll_marker_top", id.id)
                               : CLAY_IDI ("scroll_marker_bottom", id.id));
    Clay_FloatingAttachPoints att;
    if (end == 0)
      att= (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_TOP,
                                         .parent=  CLAY_ATTACH_POINT_LEFT_TOP };
    else if (horizontal)
      att= (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_RIGHT_TOP,
                                         .parent=  CLAY_ATTACH_POINT_RIGHT_TOP };
    else
      att= (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_BOTTOM,
                                         .parent=  CLAY_ATTACH_POINT_LEFT_BOTTOM };
    CLAY(m_id, {
      .floating= {
        .zIndex= z,
        .parentId= id.id,
        .attachPoints= att,
        .attachTo= CLAY_ATTACH_TO_ELEMENT_WITH_ID },
      .layout= {
        .sizing= horizontal
          ? (Clay_Sizing) { CLAY_SIZING_FIXED(total), CLAY_SIZING_FIXED(cross) }
          : (Clay_Sizing) { CLAY_SIZING_FIXED(cross), CLAY_SIZING_FIXED(total) },
        .layoutDirection= horizontal ? CLAY_LEFT_TO_RIGHT : CLAY_TOP_TO_BOTTOM }})
    {
      for (int i= 0; i < 5; i++) {
        int k= (end == 0) ? i : 4 - i; // the opaque cell sits at the edge
        CLAY_AUTO_ID({
          .layout= {
            .sizing= horizontal
              ? (Clay_Sizing) { CLAY_SIZING_FIXED(ui_pxf (marker_cell[k])), CLAY_SIZING_GROW(0) }
              : (Clay_Sizing) { CLAY_SIZING_GROW(0), CLAY_SIZING_FIXED(ui_pxf (marker_cell[k])) },
            .childAlignment= { CLAY_ALIGN_X_CENTER, CLAY_ALIGN_Y_CENTER }},
          .backgroundColor= { bg.r, bg.g, bg.b, marker_fade[k] },
          .cornerRadius= (k != 0 || radius <= 0) ? (Clay_CornerRadius) { 0, 0, 0, 0 }
            : horizontal
              ? (end == 0 ? (Clay_CornerRadius) { radius, 0, radius, 0 }
                          : (Clay_CornerRadius) { 0, radius, 0, radius })
              : (end == 0 ? (Clay_CornerRadius) { radius, radius, 0, 0 }
                          : (Clay_CornerRadius) { 0, 0, radius, radius }) })
        {
          if (k == 0) {
            int dir= horizontal ? (end == 0 ? 0 : 1) : (end == 0 ? 2 : 3);
            CLAY_AUTO_ID({
              .layout= { .sizing= { CLAY_SIZING_FIXED(ui_pxf (marker_cell[0])),
                                    CLAY_SIZING_FIXED(ui_pxf (marker_cell[0])) }},
              .custom= { .customData= (void*) &render_marker_fn },
              .userData= (void*) (intptr_t) dir }) {}
          }
        }
      }
    }
    // a click on a marker brings the side it points to into view
    if (button_logic (m_id).clicked == 1) {
      float step= min (hidden, 0.8f * view);
      *pos += (end == 0) ? step : -step;
      mouse_action= ""; // the click is ours, not the container's
    }
  }
}

// A wheel turned over a container which only scrolls sideways moves it
// sideways: a mouse has no horizontal wheel, and Clay gives each axis a
// delta of its own. Called from push_wheel once the pointer state is set,
// on the deltas which go to Clay (the editors keep theirs as they are).
void
vue_wheel_axes (double& dx, double& dy) {
  if (dy == 0) return;
  Clay_ElementIdArray ids= Clay_GetPointerOverIds ();
  // the array is filled as Clay descends the tree: the innermost container
  // under the pointer is the last one, and it is the one which scrolls
  for (int32_t i= ids.length - 1; i >= 0; i--) {
    Clay_ScrollContainerData sd= Clay_GetScrollContainerData (ids.internalArray[i]);
    if (!sd.found) continue;
    if (sd.config.horizontal && !sd.config.vertical) { dx += dy; dy= 0; }
    return;
  }
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
  widget printer, copies, pages, orientation;
  array<printer_option> options;
public:
  print_command_rep (url _file, command _after, widget _printer, widget _copies, widget _pages,
                     widget _orientation, array<printer_option> _options):
    file (_file), after (_after), printer (_printer), copies (_copies), pages (_pages),
    orientation (_orientation), options (_options) {}
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
    // the spooler turns the pages (IPP orientation-requested, 4 is
    // landscape), as the Qt port asks it (QTMPrinterSettings)
    if (enum_widget_value (orientation) == translate ("Landscape"))
      cmd << " -o orientation-requested=4";
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
make_printer_inputs (widget& printer, widget& copies, widget& pages, widget& orientation) {
  array<string> printers= system_printers ();
  printer= enum_widget (noop_command (), printers, printers[0], 0, "14em");
  array<string> one; one << string ("1");
  array<string> none; none << string ("");
  copies= input_text_widget (noop_command (), "copies", one, 0, "3em");
  pages= input_text_widget (noop_command (), "pages", none, 0, "8em");
  array<string> turns;
  turns << translate ("Portrait") << translate ("Landscape");
  orientation= enum_widget (noop_command (), turns, turns[0], 0, "14em");
}

// contents of the dialog shown by printer_widget: the printer, the number
// of copies, the pages (as lpr's page-ranges: "1-3,7"), the orientation,
// the options of the chosen printer (lpoptions), Cancel/Print
static widget
make_printer_dialog (command cmd, url ps_pdf_file, widget printer, widget copies, widget pages,
                     widget orientation) {
  string pr= enum_widget_value (printer);
  if (pr == translate ("Default printer")) pr= "";
  array<printer_option> options= printer_options (pr);
  array<widget> lhs, rhs;
  lhs << text_widget (translate ("Printer") * ":", 0, black)
      << text_widget (translate ("Copies") * ":", 0, black)
      << text_widget (translate ("Pages") * ":", 0, black)
      << text_widget (translate ("Orientation") * ":", 0, black);
  rhs << printer << copies
      << horizontal_list (array<widget> (pages, glue_widget (false, false, 8*PIXEL, 0),
                                         text_widget (translate ("all, or e.g. 1-3,7"), WIDGET_STYLE_GREY, black)))
      << orientation;
  for (int i= 0; i < N(options); i++) {
    options[i].choice= enum_widget (noop_command (), options[i].values, options[i].def, 0, "14em");
    lhs << text_widget (translate (options[i].label) * ":", 0, black);
    rhs << options[i].choice;
  }
  array<widget> buttons;
  buttons << menu_button (text_widget (translate ("Cancel"), 0, black), cmd, "", "", WIDGET_STYLE_BUTTON)
          << glue_widget (false, false, 8*PIXEL, 0)
          << menu_button (text_widget (translate ("Print"), 0, black),
                          tm_new<print_command_rep> (ps_pdf_file, cmd, printer, copies, pages,
                                                             orientation, options),
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

// the width of a label in the font of a style, in points times PIXEL (as
// layout_text_box measures it)
static SI
label_width (string s, int style) {
  font fn= get_default_styled_font (style);
  metric ex;
  fn->var_get_extents (s, ex);
  return (ex->x2 - ex->x1 + 2) / 3;
}

// the widest of the values of an enum (Qt sizes its combo boxes so, in
// QTMComboBox::addItemsAndResize), in device pixels
static float
enum_values_width (array<string> vals, string val, int style) {
  SI w= label_width (val, style);
  for (int i= 0; i < N(vals); i++) w= max (w, label_width (vals[i], style));
  return (float) retina_factor * w / PIXEL;
}

// The call back of the text input of an editable enum: the value is
// committed by return, as the line edit of an editable QComboBox; escape
// (the input calls back with #f) leaves it as it was
class enum_commit_command_rep: public command_rep {
  command cb;
public:
  enum_commit_command_rep (command _cb): cb (_cb) {}
  void apply () {}
  void apply (object args) {
    if (is_list (args) && !is_null (args) && is_string (car (args))) cb (args);
  }
  tm_ostream& print (tm_ostream& out) { return out << "<enum_commit_command>"; }
};

// the text input of an editable enum, holding val; as wide as the widest of
// the values unless the enum was given a width
static widget
make_enum_input (command cb, array<string> vals, string val, int st, string w) {
  if (N(w) == 0) {
    SI wd= label_width (val, st);
    for (int i= 0; i < N(vals); i++) wd= max (wd, label_width (vals[i], st));
    w= as_string (max (wd / PIXEL, 40) + 4) * "px";
  }
  array<string> def (1);
  def[0]= val;
  return input_text_widget (tm_new<enum_commit_command_rep> (cb), "string",
                            def, st, w);
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
  if (type == "responsive_tabs_widget") {
    vue_responsive_tabs_widget d= open_box<vue_responsive_tabs_widget> (data);
    array<widget> icons;
    for (int i=0; i< N(d.us); i++) {
      vue_picture_widget pd { .p= load_xpm (d.us[i]), .file_name= d.us[i],
                              .stamp= icon_stamp () };
      icons << vue_create<vue_picture_widget> ("picture_widget", pd);
    }
    vue_responsive_tabs_widget_star dd { .tabs= d.tabs, .icons= icons, .bodies= d.bodies,
                                         .current= 0, .mode= d.mode, .viewing= false };
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
    // the convention of Qt (qt_ui_element.cpp): the enum can be edited when
    // the value is empty or the last value is, and that empty value is not
    // one of the choices
    array<string> vals= d.vals;
    bool editable= (N(vals) == 0 || d.val == "" || vals[N(vals)-1] == "");
    if (N(vals) > 0 && vals[N(vals)-1] == "") vals= range (vals, 0, N(vals)-1);
    widget input;
    if (editable) input= make_enum_input (d.cb, vals, d.val, d.st, d.w);
    vue_enum_widget_star dd { .cb= d.cb, .vals= vals, .val= d.val, .st= d.st, .w= d.w, .open= false,
                              .editable= editable, .input= input };
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
    widget printer, copies, pages, orientation;
    make_printer_inputs (printer, copies, pages, orientation);
    vue_printer_widget_star dd { .cmd= d.cmd, .ps_pdf_file= d.ps_pdf_file,
                                 .content= make_printer_dialog (d.cmd, d.ps_pdf_file, printer, copies,
                                                                pages, orientation),
                                 .printer= printer, .copies= copies, .pages= pages,
                                 .orientation= orientation,
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
extern "C" Clay_Dimensions vue_clay_min_dimensions (Clay_ElementId id); // clay.c

// The menus behave as those of the Mac: a click on a title of a bar opens
// its menu, and while it is open the pointer opens the menu of any other
// title of that bar it goes over; a submenu opens when the pointer rests
// on its item (MENU_DELAY) and closes when it rests on another item of the
// same menu; everything stays open when the pointer leaves the menus, until
// an item is chosen, the pointer is pressed outside them (the press is not
// passed on: gui_init_context), or Escape.
#define MENU_DELAY 150

// the padding of the items of vertical menus: on the left, the column of
// the check marks, which every item has, marked or not, so that the labels
// of a menu align and a menu is as wide with marks as without them
static Clay_Padding
menu_item_padding () {
  return { 0, ui_px (16), ui_px (6), ui_px (6) };
}

static void render_menu_mark_fn (renderer ren, void* data, rectangle r); // below

static void
layout_mark_column (int kind= 0) {
  // kind: 0 none, 1 "v" (check), 2 "*", 3 "o" (see render_menu_mark_fn)
  CLAY_AUTO_ID({
    .layout= { .sizing= { CLAY_SIZING_FIXED(ui_pxf (22)), CLAY_SIZING_FIXED(ui_pxf (22)) }},
    .custom= { .customData= (kind != 0) ? (void*) &render_menu_mark_fn : NULL },
    .userData= (void*) (intptr_t) kind }) {}
}

// the item of an open menu under the pointer, and since when
static void
note_menu_hover (Clay_ElementId id) {
  if (current_menu == 0 || !Clay_PointerOver (id)) return;
  vue_input_state& in= current_window->input;
  menu_hovered= true;
  if (in.hover_item != id.id || in.hover_menu != current_menu) {
    in.hover_item= id.id;
    in.hover_menu= current_menu;
    in.hover_since= texmacs_time ();
  }
}

// the pointer has rested MENU_DELAY on an item of the menu (0: on none)
static bool
menu_rested (uint32_t menu, uint32_t item) {
  vue_input_state& in= current_window->input;
  if (in.hover_menu != menu || in.hover_item == 0) return false;
  if (item != 0 && in.hover_item != item) return false;
  if (texmacs_time () - in.hover_since >= MENU_DELAY) return true;
  needs_update (); // a pass when the delay is over, the pointer resting
  return false;
}

// The buttons of the tool bars which show an icon are square, whatever
// the proportions of the icon: the side is the larger one of the icon, and
// in the column at the left of the editor that of the icons of the main bar
// (24 points) at least, so that the buttons of its two bars match.
// Returns false when the button shows something else (a text).
static bool
icon_size (widget w, float& iw, float& ih) {
  vue_ui_rep* u= dynamic_cast<vue_ui_rep*> (concrete (w).rep);
  if (u == NULL) return false;
  if (u->type == "picture_widget") {
    picture p= icon_picture (u->data);
    iw= (float) p->get_width (); ih= (float) p->get_height ();
    return true;
  }
  if (u->type == "balloon_widget")
    return icon_size (open_box<vue_balloon_widget> (u->data).w, iw, ih);
  return false;
}

static bool
square_tool_button (widget content, Clay_Sizing& s, Clay_Padding& padding) {
  float iw, ih;
  if (!in_tool_bar || !icon_size (content, iw, ih)) return false;
  float side= max (iw, ih);
  if (in_side_bar) side= max (side, ui_pxf (48));
  uint16_t pad= ui_px (tool_button_pad);
  padding= CLAY_PADDING_ALL (pad);
  s= { CLAY_SIZING_FIXED (side + 2 * pad), CLAY_SIZING_FIXED (side + 2 * pad) };
  return true;
}

void
layout_pull_button (vue_ui_rep *w) {
  vue_cached_pull_button d= open_box<vue_cached_pull_button> (w->data);
  bool down= w->type == "pulldown_button";
  // where the menu opens: below the button, or at its right in the column
  // of the bars at the left of the editor
  bool opens_down= down && !in_side_bar;
  Clay_ElementId button_id= CLAY_SIDI(CLAY_TM_STRING(w->type), w->id);
  Clay_ElementId float_id=  CLAY_IDI("pull_button_float", w->id);
  Clay_Sizing s= layoutExpand;
  if (down) s= { CLAY_SIZING_FIT(.min=ui_pxf (20)) };
  ui_signal sig= button_logic (button_id);
  uint32_t parent_menu= current_menu;
  if (!down) note_menu_hover (button_id);
  // the items of vertical menus have the padding of menu_button, but the
  // arrow of a submenu lies in the padding on the right, near the border
  // the titles of a menu bar: roomy, with a round highlight; more padding
  // above than below, as the text keeps room for the descenders below its
  // baseline, so that the letters look centered in the highlight
  Clay_Padding padding= { ui_px (10), ui_px (10), ui_px (8), ui_px (4) };
  if (in_footer) padding= { ui_px (10), ui_px (10), ui_px (4), ui_px (2) };
  // the highlight is rounded as the items of the menus, in a menu and on
  // the bars alike
  float rad= ui_inner_corners (menu_round, menu_inset).topLeft;
  if (!down && button_grow) {
    padding= menu_item_padding ();
    padding.right= 0;
  }
  bool square= down && !in_footer && square_tool_button (d.w, s, padding);
  CLAY(button_id, {
    .layout= {
      .padding= padding,
      .childGap= ui_px (4),
      .sizing= s,
      .childAlignment= { .x= square ? CLAY_ALIGN_X_CENTER : CLAY_ALIGN_X_LEFT,
                         .y= CLAY_ALIGN_Y_CENTER }},
    // flat: the bar or menu behind shows through unless hovered (the bars
    // of the main window have different greys, hence highlight_on)
    .backgroundColor= (hot_id == button_id.id || !is_nil (d.cw))
                      ? highlight_on (color_behind)
                      : (Clay_Color) { 0, 0, 0, 0 },
    .cornerRadius= CLAY_CORNER_RADIUS(rad) })
  {
    // items of vertical menus have the column of the check marks
    if (!down && button_grow) layout_mark_column ();
    concrete(d.w)->do_layout ();
    // the menus of the interactive footer say that they are menus
    if (down && in_footer) layout_arrow ("<#25BE>", 3, dark_grey);
    if (!down) {
      CLAY_AUTO_ID({ .layout= { .sizing= { CLAY_SIZING_GROW(ui_pxf (32)), CLAY_SIZING_GROW(0) }}}){};
      layout_arrow ("<#25B8>", 1, black); // right arrow
    }
    auto open_menu= [&] () {
      // evaluate the promise
      d.cw= d.pw->eval ();
      d.placed= false;
      d.flip= false;
      d.shift_x= d.shift_y= 0;
      d.win_w= d.win_h= 0;
      d.menu_w= d.menu_h= 0;
      current_popup= true;
      // only the buttons of a bar are mutually exclusive: a submenu
      // (pullright) belongs to the chain of the menu it is in, and
      // claiming the slot here would close its own parent
      if (down) {
        open_pull_id= button_id.id;
        open_pull_bar= current_bar;
      }
    };
    auto close_menu= [&] () {
      d.cw= NULL;
      if (down && open_pull_id == button_id.id) open_pull_id= 0;
    };
    bool is_open= !is_nil (d.cw);
    if (down && sig.pressed == 1) {
      // a title of a bar acts as it is pressed, as on the Mac: it opens
      // its menu, or closes it when it is open. The button may then be
      // dragged to an item and released there to choose it: the title
      // gives up the pointer (which it took with the press), so that the
      // items are hovered, and the release is theirs (menu_drag)
      if (!is_open) {
        open_menu ();
        menu_drag= true;
        menu_drag_window= (void*) current_window;
      }
      else {
        close_menu ();
        current_popup= false;
      }
      active_id= 0;
      active_button= 0;
    }
    else if (!down && sig.clicked == 1 && !is_open) open_menu ();
    else if (!is_open && down && open_pull_id != 0 &&
             open_pull_id != button_id.id && open_pull_bar == current_bar &&
             active_id == 0 && Clay_PointerOver (button_id)) {
      // another menu of the bar is open: the pointer takes it here
      open_menu ();
      layout_again= true;
    }
    else if (!is_open && !down && !current_popup &&
             menu_rested (parent_menu, button_id.id))
      open_menu (); // the pointer rests on the item of a submenu
    else if (current_popup && is_open && sig.clicked != 1) {
      // some other popup is active, we should be inactive
      close_menu ();
    }
    else if (down && is_open &&
             open_pull_id != 0 && open_pull_id != button_id.id) {
      // another button of the bar opened its menu (it may have been laid
      // out after us, where neither cancel_popup nor current_popup reaches
      // us); our own submenus close with us
      close_menu ();
    }
    else if (!down && is_open && !Clay_PointerOver (button_id) &&
             menu_rested (parent_menu, 0) &&
             current_window->input.hover_item != button_id.id)
      close_menu (); // the pointer rests on another item of our menu
    // if we are active then we draw the float window
    if (!is_nil (d.cw)) {
      // when the menu (as laid out in the previous pass) sticks out of the
      // window, open it on the other side of the button if there is more
      // room there, otherwise shift it back inside; the decision is kept
      // until the menu closes, or the window is resized, to avoid flickering
      Clay_Dimensions dims= { current_window->layout_w, current_window->layout_h };
      Clay_ElementData fd= Clay_GetElementData (float_id);
      Clay_ElementData bd= Clay_GetElementData (button_id);
      if (d.placed && (d.win_w != dims.width || d.win_h != dims.height))
        d.placed= false; // the window was resized under the open menu
      if (d.placed && fd.found && (d.menu_w != fd.boundingBox.width ||
                                   d.menu_h != fd.boundingBox.height))
        d.placed= false; // the menu changed size (refreshed in place)
      if (!fd.found)
        // the menu has never been laid out: there is nothing to place it
        // by yet, and drawing this pass would show it over the edge of the
        // window for a frame. Another pass, and it is placed before it is
        // seen (the loop lays out again while layout_again is set)
        layout_again= true;
      if (fd.found && bd.found && !d.placed) {
        d.placed= true;
        d.win_w= dims.width; d.win_h= dims.height;
        d.menu_w= fd.boundingBox.width; d.menu_h= fd.boundingBox.height;
        Clay_BoundingBox f= fd.boundingBox, b= bd.boundingBox;
        // the box was measured with the shift of the last decision in it:
        // take it off, or a decision taken twice would not be the same one
        f.x -= d.shift_x; f.y -= d.shift_y;
        d.shift_x= d.shift_y= 0;
        float over_x= f.x + f.width  - dims.width;
        float over_y= f.y + f.height - dims.height;
        if (opens_down) {
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
      if (opens_down) attach= d.flip
        ? (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_BOTTOM, .parent= CLAY_ATTACH_POINT_LEFT_TOP }
        : (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_TOP, .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM };
      else attach= d.flip
        ? (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_RIGHT_TOP, .parent= CLAY_ATTACH_POINT_LEFT_TOP }
        : (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_TOP, .parent= CLAY_ATTACH_POINT_RIGHT_TOP };
      Clay_Vector2 offset= { d.shift_x, d.shift_y };
      // the menu is at most as large as the window and scrolls (wheel or
      // a click on a marker) when its contents do not fit; it is drawn
      // above the scroll bars of the editors (zIndex 1)
      CLAY(float_id, {
        .floating= {
          .offset= offset,
          .zIndex= 5,
          .attachTo= CLAY_ATTACH_TO_PARENT,
          .attachPoints= attach },
        .layout= {
          .padding= CLAY_PADDING_ALL (ui_px (menu_inset)),
          .sizing= { .width= CLAY_SIZING_FIT(.min= ui_pxf (120), .max= dims.width),
                     .height= CLAY_SIZING_FIT(.max= dims.height) }},
        .backgroundColor= color_background,
        .cornerRadius= ui_corners (menu_round),
        .clip= { .horizontal= true, .vertical= true,
                 .childOffset= Clay_GetScrollOffset () },
        .border= {
          .width= { 1, 1, 1, 1 },
          .color= color_border },
        .transition= drop_in_transition })
      {
        current_popup= false;
        uint32_t save_menu= current_menu, save_bar= current_bar;
        current_menu= float_id.id;
        current_bar= 0;
        concrete (d.cw)->do_layout ();
        current_menu= save_menu;
        current_bar= save_bar;
        menu_veil (float_id, color_background, ui_corners (menu_round), 5);
        // a press outside the chain: its last menu closes first, then the
        // ones it hangs from, down to the one the press is in
        bool outside= menu_press && !Clay_PointerOver (float_id) &&
                      !Clay_PointerOver (button_id) && !pointer_on_enum_list;
        if (cancel_popup || (!current_popup && outside)) {
          // an item was chosen, or we are the last popup of the chain and
          // the pointer was pressed outside: then we need to deactivate
          close_menu ();
          current_popup= false;
        } else {
          // ok, we are the current popup now in this layout cycle
          current_popup= true;
          menu_zones_now << float_id.id << button_id.id;
        }
      }
    }
  }
  if (!is_nil (d.cw)) {
    // markers rather than a scroll bar: a bar would lie over the labels of
    // the items and over the arrows of the submenus
    Clay_ScrollContainerData sd= Clay_GetScrollContainerData (float_id);
    if (sd.found) {
      scroll_markers (float_id, sd, color_background, false, 6, ui_corners (menu_round).topLeft);
      scroll_markers (float_id, sd, color_background, true, 6, ui_corners (menu_round).topLeft);
    }
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
  bool grows_main= false, grows_cross= false;
  for (int i=0; i<N(a); i++) {
    grows_main=  grows_main  || widget_grows (a[i], !vert);
    grows_cross= grows_cross || widget_grows (a[i], vert);
  }
  Clay_Sizing s= layoutFit;
  // a row of a bar in the column at the left of the editor: top to bottom,
  // its items centered in the width of the column
  bool column= in_side_bar && !vert;
  if (column) s.width= CLAY_SIZING_GROW(0);
  else if (vert) {
    if (grows_cross) s.width=  CLAY_SIZING_GROW(0);
    if (grows_main)  s.height= CLAY_SIZING_GROW(0);
  } else {
    if (grows_main)  s.width=  CLAY_SIZING_GROW(0);
    s.height= CLAY_SIZING_GROW(0);
  }
  CLAY(vert ? CLAY_IDI("vertical_menu", id) : CLAY_IDI("horizontal_menu", id), {
    .layout= {
      .layoutDirection= (vert || column) ? CLAY_TOP_TO_BOTTOM : CLAY_LEFT_TO_RIGHT,
      .sizing= s,
      // a 2x value, as the numbers of this file; tighter in the tool bars
      .childGap= ui_px ((in_tool_bar && !vert) ? tool_button_gap : gap),
      // a horizontal menu fills the height of its bar: its items (icons of
      // several sizes, texts, separators) are centered in it
      .childAlignment= column ? (Clay_ChildAlignment) { .x= CLAY_ALIGN_X_CENTER, .y= CLAY_ALIGN_Y_TOP }
                              : (Clay_ChildAlignment) { .y= vert ? CLAY_ALIGN_Y_TOP : CLAY_ALIGN_Y_CENTER } }})
  {
    bool save_grow= button_grow;
    uint32_t save_bar= current_bar, save_menu= current_menu;
    button_grow= vert;
    // the titles of a bar (see layout_pull_button); a vertical menu which
    // is not in an open menu is one too (a popup menu in a window of its
    // own, a menu of a dialog): its submenus open under the pointer
    if (!vert) current_bar= id;
    else if (current_menu == 0) current_menu= CLAY_IDI("vertical_menu", id).id;
    for (int i=0, n=N(a); i< n; i++) {
      string t= concrete (a[i])->type;
      if (vert && (t == "menu_group" || t == "text_widget")) {
        // a label of the menu (the greyed title of a group): where the
        // labels of the items are, with their padding and mark column.
        // The pointer resting on it rests on the menu as on an item, and
        // closes the submenu of another item (see layout_pull_button)
        Clay_ElementId label_id= CLAY_IDI ("menu_label", concrete (a[i])->id);
        note_menu_hover (label_id);
        CLAY(label_id, {
          .layout= {
            .padding= menu_item_padding (),
            .sizing= { CLAY_SIZING_GROW(0), CLAY_SIZING_FIT(0) },
            .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }}}) {
          layout_mark_column ();
          concrete (a[i])->do_layout ();
        }
      }
      else concrete (a[i])->do_layout ();
    }
    button_grow= save_grow;
    current_bar= save_bar;
    current_menu= save_menu;
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
      t == "responsive_tabs_widget" ||
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
  bool column= in_side_bar && !vert; // see layout_menu
  if (column) s= { CLAY_SIZING_GROW(0), CLAY_SIZING_FIT(0) };
  CLAY(vert ? CLAY_IDI("vertical_list", id) : CLAY_IDI("horizontal_list", id), {
     .layout= {
       .layoutDirection= (vert || column) ? CLAY_TOP_TO_BOTTOM : CLAY_LEFT_TO_RIGHT,
       .sizing= s,
       .childAlignment= column ? (Clay_ChildAlignment) { .x= CLAY_ALIGN_X_CENTER, .y= CLAY_ALIGN_Y_TOP }
                               : (Clay_ChildAlignment) { .y= vert ? CLAY_ALIGN_Y_TOP : CLAY_ALIGN_Y_CENTER } }})
  {
    // the buttons of a row keep their size (the >>> glue of a row of
    // buttons pushes them to one side), even in a dialog, which is a
    // vertical menu whose items stretch to its width
    bool save_grow= button_grow;
    if (!vert) button_grow= false;
    for (int i=0, n=N(a); i< n; i++) {
      concrete (a[i])->do_layout ();
    }
    button_grow= save_grow;
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
            CLAY_SIZING_FIXED(ui_pxf (24)),
            CLAY_SIZING_FIXED(scrollData.scrollContainerDimensions.height / ratio.y) }},
        .backgroundColor= Clay_PointerOver (vsb_id)
          ? the_theme.scrollbar_hover : the_theme.scrollbar,
      .cornerRadius= CLAY_CORNER_RADIUS(ui_pxf (12)) }){};
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
            CLAY_SIZING_FIXED(ui_pxf (24)) }},
        .backgroundColor= Clay_PointerOver (hsb_id)
          ? the_theme.scrollbar_hover : the_theme.scrollbar,
      .cornerRadius= CLAY_CORNER_RADIUS(ui_pxf (12)) }){};
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
    ren->set_pencil (pencil (theme_color (the_theme.text), 2*px, cap_round));
    ren->lines (xs, ys);
  }
  else {
    SI rad= min (w, h) / 5;
    ren->set_pencil (pencil (theme_color (the_theme.text), px));
    if (kind == 2) ren->fill_arc (cx-rad, cy-rad, cx+rad, cy+rad, 0, 360*64);
    else ren->arc (cx-rad, cy-rad, cx+rad, cy+rad, 0, 360*64);
  }
}

void
vue_ui_rep::do_layout () {
  layout_who= type;
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
      // the grey of the mode bar (212 in the light theme), a step lighter
      // than the window; the buttons in it are highlighted over it
      with_behind wb (the_theme.bar_mode);
      CLAY(div_id, {
        .backgroundColor= the_theme.bar_mode,
        .cornerRadius= { ui_pxf (6), ui_pxf (6), 0, 0 },
        .layout= {
          .padding= { ui_px (12), ui_px (8), ui_px (8), ui_px (8) },
          .childGap= ui_px (8),
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
          .padding= { ui_px (8), ui_px (8), ui_px (8), ui_px (4) },
          .sizing= { .width= CLAY_SIZING_GROW(0) }},
        .border= { .width= { .bottom= 1 }, .color= color_border }})
      {
        concrete (d.w)->do_layout ();
      }
    }
    else if (d.name == "wait-panel") {
      // the wait indicator (show_wait_indicator in vue_gui.cpp): a framed
      // panel, its content centred vertically. Rounded when the windows are
      // drawn in one; a window of its own is square, framed by the popup
      bool round= vue_single_window ();
      Clay_BorderElementConfig frame= {};
      if (round) frame= { .width= { 1, 1, 1, 1 }, .color= color_border };
      CLAY(div_id, {
        .backgroundColor= color_field,
        .cornerRadius= round ? ui_corners (menu_round) : CLAY_CORNER_RADIUS (0),
        .layout= {
          .padding= { ui_px (18), ui_px (24), ui_px (14), ui_px (14) },
          .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
        .border= frame })
      {
        concrete (d.w)->do_layout ();
      }
    }
    else if (d.name == "discrete") {
      context_style |= WIDGET_STYLE_GREY;
      CLAY(div_id, { .layout= { .padding= { ui_px (4), ui_px (4), ui_px (2), ui_px (2) } }})
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
            .padding= { ui_px (8), ui_px (8), ui_px (4), 0 },
            .sizing= { .width= CLAY_SIZING_GROW(0) },
            .childAlignment= { .y= CLAY_ALIGN_Y_BOTTOM }},
          .border= { .width= { .bottom= 1 }, .color= color_border }})
        {
          concrete (d.w)->do_layout ();
        }
      }
      else {
        // a segmented bar in the grey of the mode bar (the segments are
        // highlighted over it: see menu_button)
        with_behind wb (the_theme.bar_mode);
        CLAY(div_id, {
          .backgroundColor= the_theme.bar_mode,
          .cornerRadius= CLAY_CORNER_RADIUS(ui_pxf (7)),
          .layout= {
            .padding= CLAY_PADDING_ALL(ui_px (2)),
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
    // given by its container, and its page is the size of the current tab:
    // as in Qt (QTMTabWidget::resizeOthers), a dialog sized to its contents
    // is sized again to the new tab when another one is chosen
    // (refit_window), where Widkit kept the size of the largest page.
    vue_tabs_widget_star d= open_box<vue_tabs_widget_star> (data);
    int n= min (N(d.tabs), N(d.bodies));
    if (n == 0) return;
    if (d.current < 0 || d.current >= n) d.current= 0;
    int next= d.current;
    Clay_ElementId clay_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    const float pad= ui_pxf (14); // around the page
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
          .padding= { ui_px (10), ui_px (10), ui_px (6), 0 },
          .childGap= ui_px (4),
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
                       : ((hot_id == tab_id.id) ? highlight_on (the_theme.tab_inactive)
                                                : the_theme.tab_inactive);
          Clay_ElementData td= Clay_GetElementData (tab_id);
          CLAY(tab_id, {
            .backgroundColor= bg,
            .cornerRadius= { ui_pxf (16), ui_pxf (16), 0, 0 },
            .layout= {
              .padding= { ui_px (26), ui_px (26), ui_px (cur ? 13 : 11), ui_px (cur ? 13 : 10) },
              .childGap= ui_px (10),
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
                                      CLAY_SIZING_FIXED (ui_pxf (2)) }},
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
        .cornerRadius= { 0, ui_pxf (8), ui_pxf (8), ui_pxf (8) },
        .layout= {
          .padding= CLAY_PADDING_ALL((uint16_t) pad),
          .sizing= { .width=  CLAY_SIZING_GROW(0),
                     .height= CLAY_SIZING_GROW(0) }},
        .border= { .width= { 1, 1, 1, 1 }, .color= color_border }})
      {
        CLAY_AUTO_ID({
          .layout= { .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_GROW(0) }}})
        {
          concrete (d.bodies[d.current])->do_layout ();
        }
      }
    }
    if (next != d.current) refit_window= true;
    d.current= next;
    data= close_box (d);
    return;
  }
  if (type == "responsive_tabs_widget") {
    // the modes other than "top" (see responsive_tab_mode)
    vue_responsive_tabs_widget_star d= open_box<vue_responsive_tabs_widget_star> (data);
    int n= min (N(d.tabs), N(d.bodies));
    if (n == 0) return;
    if (d.current < 0 || d.current >= n) d.current= 0;
    int  next= d.current;
    bool viewing= d.viewing;
    Clay_ElementId clay_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    const float pad= ui_pxf (14); // around a page
    Clay_Sizing grow= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_GROW(0) };
    // the label of a tab: its icon, if any, and its text
    auto label= [&] (int i) {
      if (i < N(d.icons)) concrete (d.icons[i])->do_layout ();
      concrete (d.tabs[i])->do_layout ();
    };
    // a page, framed (rounded corners: which ones as a mask, 1 for the top
    // left, 2 top right, 4 bottom right, 8 bottom left)
    auto page= [&] (Clay_ElementId pid, int i, int corners) {
      float r= ui_pxf (8);
      CLAY(pid, {
        .backgroundColor= color_background,
        .cornerRadius= { (corners & 1) ? r : 0, (corners & 2) ? r : 0,
                         (corners & 8) ? r : 0, (corners & 4) ? r : 0 },
        .layout= { .padding= CLAY_PADDING_ALL((uint16_t) pad), .sizing= grow },
        .border= { .width= { 1, 1, 1, 1 }, .color= color_border }})
      {
        CLAY_AUTO_ID({ .layout= { .sizing= grow }}) {
          concrete (d.bodies[i])->do_layout ();
        }
      }
    };
    if (d.mode == "side") {
      // the tabs in a column, the current one merging with the page
      CLAY(clay_id, {
        .layout= { .layoutDirection= CLAY_LEFT_TO_RIGHT, .sizing= grow }})
      {
        CLAY(CLAY_ID_LOCAL("tab_column"), {
          .layout= {
            .layoutDirection= CLAY_TOP_TO_BOTTOM,
            .padding= { 0, 0, ui_px (10), ui_px (10) },
            .childGap= ui_px (4),
            .sizing= { .width= CLAY_SIZING_FIT(0), .height= CLAY_SIZING_FIT(0) }}})
        {
          for (int i= 0; i < n; i++) {
            Clay_ElementId tab_id= CLAY_IDI_LOCAL("tab", i);
            if (button_logic (tab_id).clicked == 1) next= i;
            bool cur= (d.current == i);
            Clay_Color bg= cur ? color_background
                         : ((hot_id == tab_id.id) ? highlight_on (the_theme.tab_inactive)
                                                  : the_theme.tab_inactive);
            Clay_ElementData td= Clay_GetElementData (tab_id);
            CLAY(tab_id, {
              .backgroundColor= bg,
              .cornerRadius= { ui_pxf (12), 0, ui_pxf (12), 0 },
              .layout= {
                .padding= { ui_px (18), ui_px (cur ? 20 : 18), ui_px (8), ui_px (8) },
                .childGap= ui_px (10),
                .sizing= { .width= CLAY_SIZING_GROW(0) },
                .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
              .border= { .width= { 1, (uint16_t) (cur ? 0 : 1), 1, 1 }, .color= color_border }})
            {
              label (i);
              if (cur && td.found) {
                // cover the left border of the page beside the current tab
                CLAY_AUTO_ID({
                  .backgroundColor= color_background,
                  .layout= { .sizing= { CLAY_SIZING_FIXED (ui_pxf (2)),
                                        CLAY_SIZING_FIXED (td.boundingBox.height - 2) }},
                  .floating= {
                    .offset= { -1, 1 },
                    .zIndex= 1,
                    .attachTo= CLAY_ATTACH_TO_PARENT,
                    .attachPoints= { .element= CLAY_ATTACH_POINT_LEFT_TOP,
                                     .parent= CLAY_ATTACH_POINT_RIGHT_TOP },
                    .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH }}) {}
              }
            }
          }
        }
        page (CLAY_ID_LOCAL("tab_area"), d.current, 2 | 4 | 8);
      }
    }
    else if (d.mode == "mobile") {
      // the list of the tabs; a page replaces it, under a bar with a button
      // back to the list and the name of the page
      CLAY(clay_id, {
        .layout= { .layoutDirection= CLAY_TOP_TO_BOTTOM, .childGap= ui_px (6), .sizing= grow }})
      {
        if (!d.viewing) {
          for (int i= 0; i < n; i++) {
            Clay_ElementId row_id= CLAY_IDI_LOCAL("tab_row", i);
            if (button_logic (row_id).clicked == 1) { next= i; viewing= true; }
            CLAY(row_id, {
              .backgroundColor= (hot_id == row_id.id) ? highlight_on (color_field) : color_field,
              .cornerRadius= CLAY_CORNER_RADIUS (ui_pxf (6)),
              .layout= {
                .padding= { ui_px (14), ui_px (10), ui_px (10), ui_px (10) },
                .childGap= ui_px (10),
                .sizing= { .width= CLAY_SIZING_GROW(0) },
                .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
              .border= { .width= { 1, 1, 1, 1 }, .color= color_border }})
            {
              label (i);
              CLAY_AUTO_ID({ .layout= { .sizing= { .width= CLAY_SIZING_GROW(0) }}}) {}
              layout_arrow ("<#25B8>", 1, dark_grey);
            }
          }
        }
        else {
          CLAY(CLAY_ID_LOCAL("tab_bar"), {
            .layout= {
              .childGap= ui_px (12),
              .sizing= { .width= CLAY_SIZING_GROW(0) },
              .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }}})
          {
            Clay_ElementId back_id= CLAY_ID_LOCAL("tab_back");
            if (button_logic (back_id).clicked == 1) viewing= false;
            CLAY(back_id, {
              .backgroundColor= (hot_id == back_id.id) ? highlight_on (the_theme.tab_inactive)
                                                       : the_theme.tab_inactive,
              .cornerRadius= CLAY_CORNER_RADIUS (ui_pxf (6)),
              .layout= {
                .padding= { ui_px (8), ui_px (12), ui_px (4), ui_px (4) },
                .childGap= ui_px (6),
                .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
              .border= { .width= { 1, 1, 1, 1 }, .color= color_border }})
            {
              layout_arrow ("<#25C2>", 0, dark_grey);
              layout_text (translate ("Back"), 0, black);
            }
            label (d.current);
          }
          page (CLAY_ID_LOCAL("tab_area"), d.current, 1 | 2 | 4 | 8);
        }
      }
    }
    else {
      // "grid": every page under its name, in two columns, three in a wide
      // window (above 1700 pixels, as in Qt), from the width of the last
      // layout
      Clay_ElementData gd= Clay_GetElementData (clay_id);
      int cols= (gd.found && gd.boundingBox.width > ui_pxf (1700)) ? 3 : 2;
      cols= min (cols, n);
      CLAY(clay_id, {
        .layout= { .layoutDirection= CLAY_TOP_TO_BOTTOM, .childGap= ui_px (10), .sizing= grow }})
      {
        for (int r= 0; r*cols < n; r++)
          CLAY(CLAY_IDI_LOCAL("tab_grid_row", r), {
            .layout= { .childGap= ui_px (10), .sizing= grow }})
          {
            for (int c= 0; c < cols; c++) {
              int i= r*cols + c;
              CLAY(CLAY_IDI_LOCAL("tab_cell", i), {
                .layout= { .layoutDirection= CLAY_TOP_TO_BOTTOM, .childGap= ui_px (4),
                           .sizing= grow }})
              {
                if (i < n) {
                  CLAY(CLAY_IDI_LOCAL("tab_title", i), {
                    .layout= { .padding= { ui_px (4), 0, 0, 0 }, .childGap= ui_px (8),
                               .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }}})
                  {
                    int save= context_style;
                    context_style |= WIDGET_STYLE_BOLD;
                    label (i);
                    context_style= save;
                  }
                  page (CLAY_IDI_LOCAL("tab_page", i), i, 1 | 2 | 4 | 8);
                }
              }
            }
          }
      }
    }
    if (next != d.current || viewing != d.viewing) refit_window= true;
    d.current= next;
    d.viewing= viewing;
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
          .cornerRadius= CLAY_CORNER_RADIUS(ui_pxf (13)),
          .layout= { .sizing= { CLAY_SIZING_FIXED(ui_pxf (26)), CLAY_SIZING_FIXED(ui_pxf (26)) }},
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
    if (!button_grow) sz= { CLAY_SIZING_FIT (.min= ui_pxf (push ? 70 : 20)) };
    Clay_Color hl= highlight_on (color_behind); // shows on the bar behind
    Clay_Color bg= faded (hl); // flat buttons show their container
    Clay_Padding padding= swatch ? CLAY_PADDING_ALL(ui_px (2)) : CLAY_PADDING_ALL(ui_px (5));
    // an item of a vertical menu: the column of the marks, then its label
    bool item= !swatch && !push && section_bar == 0 && button_grow;
    if (item) padding= menu_item_padding ();
    // a disabled item is an item all the same: the pointer resting on it
    // closes the submenu of another item (see layout_pull_button)
    note_menu_hover (button_id);
    // the highlight of an item follows the corners of the menu (and of the
    // items which open submenus, see layout_pull_button)
    Clay_CornerRadius radius= item ? ui_inner_corners (menu_round, menu_inset)
                                   : CLAY_CORNER_RADIUS(ui_pxf (4));
    Clay_BorderElementConfig border= {};
    bool tab_strip= false;
    if (push) {
      bg= down ? color_button_down : (hot ? color_button_hover : color_button);
      padding= { ui_px (14), ui_px (14), ui_px (6), ui_px (6) };
      radius= CLAY_CORNER_RADIUS(ui_pxf (6));
      border= { .width= { 1, 1, 1, 1 }, .color= color_border };
    }
    else if (section_bar == 2) {
      // a tab of a "section-tabs" bar: the active one is framed and merges
      // with the area below (the line of the bar is covered by a strip)
      padding= { ui_px (16), ui_px (16), ui_px (8), ui_px (8) };
      radius= { ui_pxf (12), ui_pxf (12), 0, 0 };
      if (section_active) {
        bg= palette[3];
        border= { .width= { 1, 1, 1, 0 }, .color= color_border };
        tab_strip= true;
      }
      else if (down) bg= color_pressed;
      else if (hot) bg= highlight_on (color_behind);
    }
    else if (section_bar == 1) {
      // a segment of a "sections" bar
      padding= { ui_px (12), ui_px (12), ui_px (4), ui_px (4) };
      radius= CLAY_CORNER_RADIUS(ui_pxf (5));
      if (section_active) {
        bg= palette[3];
        border= { .width= { 1, 1, 1, 1 }, .color= color_border };
      }
      else if (down) bg= color_pressed;
      else if (hot) bg= highlight_on (color_behind);
    }
    else {
      if (!item && !swatch) {
        // a button of a tool bar: roomier, with a rounder highlight (flatter
        // in the footer, which is lower than the tool bars)
        padding= in_footer ? (Clay_Padding) { ui_px (10), ui_px (10), ui_px (3), ui_px (3) }
                 : in_tool_bar ? CLAY_PADDING_ALL(ui_px (tool_button_pad))
                 : CLAY_PADDING_ALL(ui_px (7));
        radius= ui_inner_corners (menu_round, menu_inset); // as the menu items
      }
      if (down || pressed) bg= color_pressed;
      else if (hot) bg= hl;
    }
    // an icon of a tool bar: a square button (see square_tool_button)
    bool square= !item && !push && !swatch && section_bar == 0 && !in_footer &&
                 square_tool_button (d.w, sz, padding);
    Clay_ElementData bd= Clay_GetElementData (button_id);
    CLAY(button_id, {
      .layout= {
        .padding= padding,
        .childGap= ui_px (4),
        .sizing= sz,
        // the label of a menu item is aligned with the labels above and
        // below it, a push button and a colour cell are centered (the cells
        // of a tile are stretched to the width of the menu the tile is in)
        .childAlignment= { .x= (push || swatch || square) ? CLAY_ALIGN_X_CENTER
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
      if (item || N(d.pre) > 0) {
        // the column for the mark of the item: "v" (check), "*" or "o";
        // every item of a vertical menu has it, but the cells of a colour
        // tile are not labels: reserving it in each of them spread the
        // palette by the width of a mark per column
        int kind= (d.pre == "v") ? 1 : (d.pre == "*") ? 2 : (d.pre == "o") ? 3 : 0;
        layout_mark_column (kind);
      }
      concrete(d.w)->do_layout ();
      if (N(d.ks) > 0) {
        // add shortcut, well apart from the label
        CLAY_AUTO_ID({ .layout= { .sizing= { CLAY_SIZING_GROW(ui_pxf (32)), CLAY_SIZING_GROW(0) }}}) {}
        layout_keys (d.ks, d.style, black);
      }
      if (tab_strip && bd.found) {
        // cover the bottom line of the bar under the active tab (the border
        // of the bar is drawn after its children, hence the z index)
        CLAY_AUTO_ID({
          .backgroundColor= bg,
          .layout= { .sizing= { CLAY_SIZING_FIXED (bd.boundingBox.width - 2),
                                CLAY_SIZING_FIXED (ui_pxf (1)) }},
          .floating= {
            .offset= { 1, -1 },
            .zIndex= 1,
            .attachTo= CLAY_ATTACH_TO_PARENT,
            .attachPoints= { .element= CLAY_ATTACH_POINT_LEFT_TOP,
                             .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM },
            .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH }}) {}
      }
    }
    // a right click on a tag of the interactive footer, or a click with
    // Control or Option held: the right click of a Mac trackpad or mouse
    // with one button, which the browser (and SDL) report as a left click
    bool context_click= in_footer && current_window != NULL &&
      (sig.clicked == 3 ||
       (sig.clicked == 1 && (SDL_GetModState () & (SDL_KMOD_CTRL | SDL_KMOD_ALT)) != 0));
    if (sig.clicked == 1 && !context_click) {
      // close any active popup chain (see pull_widget)
      cancel_popup= true;
      if (DEBUG_VUE_WIDGETS) debug_widgets << "Click!! " << id << LF;
      cmd_list= list(d.cmd, cmd_list);
    }
    else if (context_click) {
      // a tag of the interactive footer: selected, then its context menu
      // at the pointer (the position of the window and the pointer in
      // points; mouse_x, mouse_y are layout pixels)
      cancel_popup= true;
      SI wx, wy;
      current_window->get_position (wx, wy);
      SI x= wx + (SI) (mouse_x * PIXEL / retina_factor);
      SI y= wy - (SI) (mouse_y * PIXEL / retina_factor);
      cmd_list= list (d.cmd, cmd_list);
      cmd_list= list (command (tm_new<footer_popup_command_rep> (x, y)), cmd_list);
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
    if (d.vertical && in_side_bar) {
      // the separator of the groups of a bar in the column: a rule across
      CLAY(CLAY_IDI("menu_separator (v)", id), {
        .layout= {
          .sizing= { .width= CLAY_SIZING_GROW(0) },
          .padding= { ui_px (5), ui_px (5), ui_px (5), ui_px (5) } },
        .border= {
          .width= { .top= ui_px (2) },
          .color= color_border } });
    }
    else if (d.vertical) {
      CLAY(CLAY_IDI("menu_separator (v)", id), {
        .layout= {
          .sizing= { .height= CLAY_SIZING_GROW(0) },
          .padding= { ui_px (5), ui_px (5), ui_px (5), ui_px (5) } },
        .border= {
          .width= { .left= ui_px (2) },
          .color= color_border } });
    } else {
      // the pointer resting on the rule of a menu rests on the menu as on
      // an item: it closes the submenu of another item
      Clay_ElementId sep_id= CLAY_IDI("menu_separator (h)", id);
      note_menu_hover (sep_id);
      CLAY(sep_id, {
        .layout= {
          .sizing= { .width= CLAY_SIZING_GROW(0) },
          .padding= { ui_px (5), ui_px (5), ui_px (5), ui_px (5) } },
        .border= {
          .width= { .top= ui_px (2) } ,
          .color= the_theme.shade[2] } });
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
        balloon_presses= mouse_presses;
      }
      // a click puts it away (or keeps it from coming) until the pointer
      // leaves the widget, as a tooltip does; the press and its release
      // may both come between two layouts, hence the count
      if (mouse_presses != balloon_presses) {
        balloon_presses= mouse_presses;
        balloon_time= texmacs_time () - 5000;
      }
      // nor does it come while a menu is open (a menu of a bar, a submenu):
      // it would hide the menu; its delay starts when the menus close
      else if ((current_window != NULL &&
                N(current_window->input.menu_zones) > 0) ||
               N(menu_zones_now) > 0)
        balloon_time= texmacs_time ();
      time_t elapsed= texmacs_time () - balloon_time;
      if ((elapsed > 1000) && (elapsed < 5000)) {
        // The balloon sits near the pointer and floats over the whole
        // window, as the tooltips of the Qt port do (QToolTip::showText at
        // the cursor). Attached under its own widget it landed on top of
        // the next item of a menu and was clipped to the menu's width, so
        // walking down a menu replaced one item after another with an
        // opaque box: it read as the menu flickering.
        Clay_ElementId balloon_id= CLAY_IDI ("balloon_widget", id);
        Clay_ElementData bd= Clay_GetElementData (balloon_id);
        float bx= (float) mouse_x + 14, by= (float) mouse_y + 22;
        if (bd.found && current_window != NULL) {
          // keep it inside the window, and above the pointer when there is
          // no room below (the size is the one it had in the last pass)
          float bw= bd.boundingBox.width, bh= bd.boundingBox.height;
          if (bx + bw > current_window->layout_w)
            bx= max (0.0f, current_window->layout_w - bw);
          if (by + bh > current_window->layout_h)
            by= max (0.0f, (float) mouse_y - bh - 8);
        }
        CLAY(balloon_id, {
          .backgroundColor= the_theme.balloon,
          .layout= { .padding= { ui_px (10), ui_px (10), ui_px (10), ui_px (10) } },
          .cornerRadius= CLAY_CORNER_RADIUS(ui_pxf (8)),
          .border= {
            .width= { 1, 1, 1, 1 },
            .color= the_theme.balloon_border },
          .floating= {
            .offset= { bx, by },
            .zIndex= 10,
            .attachPoints= {
              .element= CLAY_ATTACH_POINT_LEFT_TOP,
              .parent= CLAY_ATTACH_POINT_LEFT_TOP },
            .pointerCaptureMode= CLAY_POINTER_CAPTURE_MODE_PASSTHROUGH,
            .attachTo= CLAY_ATTACH_TO_ROOT }})
        {
          // (no veil here, unlike the menus: the box of a balloon appears
          // at once, and its text a moment later read as a delay)
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
      .childGap= ui_px (2) }})
    {
      while (c < n) {
        CLAY_AUTO_ID({ .layout= {
          .layoutDirection= CLAY_LEFT_TO_RIGHT,
          .childGap= ui_px (2) }})
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
    float box= ui_pxf ((d.style & WIDGET_STYLE_MINI) ? 24 : 30);
    CLAY(toggle_id, {
      .layout= { .sizing= { CLAY_SIZING_FIXED(box), CLAY_SIZING_FIXED(box) }},
      .custom= { .customData= vue_render_widget },
      .userData= render_ref () }) {}
    return;
  }
  if (type == "enum_widget") {
    //VUE_WIDGET(enum_widget, command, cb, array<string>, vals, string, val, int, st, string, w);
    // a button showing the current value, with a dropdown list of the
    // choices; an editable enum has a text input in place of the value and
    // the button is only the arrow
    vue_enum_widget_star d= open_box<vue_enum_widget_star> (data);
    bool inert= (d.st & WIDGET_STYLE_INERT) != 0;
    Clay_ElementId enum_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    Clay_ElementId arrow_id= CLAY_IDI ("enum_widget_arrow", id);
    Clay_ElementId list_id= CLAY_IDI ("enum_widget_list", id);
    Clay_ElementId button_id= d.editable ? arrow_id : enum_id;
    bool changed= false;
    if (d.editable) {
      // what is typed is the value (enum_widget_value), committed by return
      string typed= input_text_widget_string (d.input);
      if (typed != d.val) { d.val= typed; changed= true; }
    }
    ui_signal sig { .clicked= 0 };
    if (!inert) sig= button_logic (button_id);
    if (sig.clicked == 1) { d.open= !d.open; changed= true; }
    // as wide as the widest of the values, as the combo boxes of Qt, so
    // that it does not change size with the value, unless a width is given
    Clay_Sizing sz= { CLAY_SIZING_FIT (.min= ui_pxf (40)), CLAY_SIZING_FIT (0) };
    Clay_Sizing val_sz= { CLAY_SIZING_FIXED (enum_values_width (d.vals, d.val, d.st | context_style)),
                          CLAY_SIZING_FIT (0) };
    if (N(d.w) > 0) {
      SI w= decode_length (d.w, current_window, d.st);
      // the width given is that of the whole enum, arrow included
      sz.width= CLAY_SIZING_FIXED ((float) retina_factor*w/PIXEL);
      val_sz.width= CLAY_SIZING_GROW (0);
    }
    Clay_Color face= (!inert && hot_id == button_id.id)
                     ? highlight_on (the_theme.shade[2]) : the_theme.shade[2];
    Clay_ElementData ed= Clay_GetElementData (enum_id);
    // The list opens below the enum, or above it when it does not fit
    // below and there is more room above, and it is at most as tall as the
    // room on its side; it scrolls (wheel, markers) when it is taller, as
    // the menus do (layout_pull_button). The height it needs is that of its
    // contents in the last pass; the first time it is laid out once more
    // before it is drawn
    bool flip= false;
    float max_h= current_window->layout_h;
    if (d.open) {
      Clay_ScrollContainerData ld= Clay_GetScrollContainerData (list_id);
      if (!ld.found || !ed.found) layout_again= true;
      else {
        float margin= ui_pxf (4);
        float need= ld.contentDimensions.height + 2;
        float above= ed.boundingBox.y - margin;
        float below= current_window->layout_h
                     - (ed.boundingBox.y + ed.boundingBox.height) - margin;
        flip= (need > below && above > below);
        max_h= max (ui_pxf (40), flip ? above : below);
      }
    }
    CLAY(enum_id, {
      .layout= { .sizing= sz,
                 .padding= d.editable ? (Clay_Padding) { 0, 0, 0, 0 }
                                      : (Clay_Padding) { ui_px (8), ui_px (8), ui_px (4), ui_px (4) },
                 .childGap= ui_px (4),
                 .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
      .backgroundColor= d.editable ? (Clay_Color) { 0, 0, 0, 0 } : face,
      .cornerRadius= ui_corners (),
      .border= { .width= { 1, 1, 1, 1 },
                 .color= d.editable ? (Clay_Color) { 0, 0, 0, 0 } : palette[0] }})
    {
      if (d.editable) {
        bool save_fill= input_fill;
        input_fill= (N(d.w) > 0);
        concrete (d.input)->do_layout ();
        input_fill= save_fill;
        CLAY(arrow_id, {
          .layout= { .sizing= { CLAY_SIZING_FIT (0), CLAY_SIZING_GROW (0) },
                     .padding= { ui_px (6), ui_px (6), ui_px (4), ui_px (4) },
                     .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
          .backgroundColor= face,
          .cornerRadius= ui_corners (),
          .border= { .width= { 1, 1, 1, 1 }, .color= palette[0] }})
        {
          layout_arrow ("<#25BE>", 3, inert ? dark_grey : black); // down arrow
        }
      }
      else {
        CLAY_AUTO_ID({ .layout= { .sizing= val_sz }}) {
          layout_text (d.val, d.st, inert ? dark_grey : black);
        }
        layout_arrow ("<#25BE>", 3, inert ? dark_grey : black); // down arrow
      }
      if (d.open) {
        CLAY(list_id, {
          .floating= {
            .zIndex= 10,
            .attachTo= CLAY_ATTACH_TO_PARENT,
            .attachPoints= flip
              ? (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_BOTTOM,
                                              .parent= CLAY_ATTACH_POINT_LEFT_TOP }
              : (Clay_FloatingAttachPoints) { .element= CLAY_ATTACH_POINT_LEFT_TOP,
                                              .parent= CLAY_ATTACH_POINT_LEFT_BOTTOM }},
          .layout= {
            .layoutDirection= CLAY_TOP_TO_BOTTOM,
            .padding= CLAY_PADDING_ALL(ui_px (4)),
            .sizing= { .width= CLAY_SIZING_FIT (.min= ed.found ? ed.boundingBox.width : 0),
                       .height= CLAY_SIZING_FIT (.max= max_h) }},
          .backgroundColor= color_background,
          .cornerRadius= ui_corners (),
          .clip= { .vertical= true, .childOffset= Clay_GetScrollOffset () },
          .border= { .width= { 1, 1, 1, 1 }, .color= color_border },
          .transition= drop_in_transition })
        {
          menu_veil (list_id, color_background, ui_corners (), 10);
          for (int i=0; i<N(d.vals); i++) {
            Clay_ElementId item_id= CLAY_IDI_LOCAL ("item", i);
            ui_signal isig= button_logic (item_id);
            bool active= (d.vals[i] == d.val);
            CLAY(item_id, {
              .layout= { .padding= { ui_px (8), ui_px (8), ui_px (4), ui_px (4) }, .sizing= { .width= CLAY_SIZING_GROW(0) }},
              .backgroundColor= (hot_id == item_id.id) ? highlight_on (color_background)
                                : (active ? palette[2] : color_background),
              .cornerRadius= ui_inner_corners (1, 4) })
            {
              layout_text (d.vals[i], d.st, black);
            }
            if (isig.clicked == 1) {
              d.val= d.vals[i];
              d.open= false;
              changed= true;
              // the input shows the value chosen (it has no setter: it is
              // made again, with the value as its default)
              if (d.editable)
                d.input= make_enum_input (d.cb, d.vals, d.val, d.st, d.w);
              cmd_list= list (applied_command (d.cb, list_object (object (d.val))), cmd_list);
            }
          }
        }
        // a press on the list is its own: the elements under it, laid out
        // after it, must not take it (a swatch of the colour menu under the
        // list of the sets of the typographic palette: its release chose
        // the colour and closed the menu)
        if (starts (mouse_action, "press-") && Clay_PointerOver (list_id))
          mouse_action= "";
        // in a menu, the list is part of it (a press on it is not outside)
        if (Clay_PointerOver (list_id)) pointer_on_enum_list= true;
        if (current_menu != 0) menu_zones_now << list_id.id;
        // dismiss the list when clicking somewhere else
        if (starts (mouse_action, "press-") &&
            !Clay_PointerOver (list_id) && !Clay_PointerOver (button_id)) {
          d.open= false;
          changed= true;
        }
      }
    }
    if (d.open) {
      Clay_ScrollContainerData ld= Clay_GetScrollContainerData (list_id);
      if (ld.found) scroll_markers (list_id, ld, color_background, false, 11, ui_corners ().topLeft);
    }
    if (changed) data= close_box (d);
    return;
  }
  if (type == "resize_widget") {
    //VUE_WIDGET(resize_widget, widget, w, int, style, string, w1, string, h1,
    //string, w2, string, h2, string, w3, string, h3,
    //string, hpos, string, vpos);
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
    // an initial scrolling position other than the top left (hpos "right"
    // or "center", vpos "bottom" or "center") makes the box a scroll
    // container of its contents, which start at that position, as in
    // Widkit (resize_widget in canvas_widget.cpp): a log which shows its
    // last lines. Otherwise the contents fill the box
    bool hscroll= (d.hpos == "right" || d.hpos == "center");
    bool vscroll= (d.vpos == "bottom" || d.vpos == "center");
    Clay_ElementId my_id= CLAY_SIDI (CLAY_TM_STRING (type), id);
    int save_style= context_style;
    context_style |= d.style & inherited_styles;
    if (!hscroll && !vscroll) {
      CLAY(my_id, {
        .layout= { .sizing= sizing }})
      {
        // what a resize contains is meant to fill it: a typeset box would
        // otherwise keep the size of its contents inside a pane which asked
        // for a larger one (the documentation pane of the macro editors)
        bool save_fill= fill_parent;
        fill_parent= true;
        concrete(d.w)->do_layout ();
        fill_parent= save_fill;
      }
      context_style= save_style;
      return;
    }
    CLAY(my_id, {
      .layout= { .sizing= sizing },
      .clip= {
        .horizontal= hscroll, .vertical= vscroll,
        .childOffset= Clay_GetScrollOffset () }})
    {
      concrete(d.w)->do_layout ();
    }
    context_style= save_style;
    Clay_ScrollContainerData sd= Clay_GetScrollContainerData (my_id);
    if (sd.found) {
      // the position is set once, when the contents have been measured
      // (Clay knows them from the previous layout); then it is the user's
      if (!resize_positioned->contains ((int) id) &&
          sd.contentDimensions.width > 0 && sd.contentDimensions.height > 0) {
        resize_positioned << (int) id;
        float over_x= max (sd.contentDimensions.width - sd.scrollContainerDimensions.width, 0.0f);
        float over_y= max (sd.contentDimensions.height - sd.scrollContainerDimensions.height, 0.0f);
        if (hscroll) sd.scrollPosition->x= -(d.hpos == "center" ? floor (over_x / 2) : over_x);
        if (vscroll) sd.scrollPosition->y= -(d.vpos == "center" ? floor (over_y / 2) : over_y);
        gui_needs_relayout= true; // shown at once, by another layout
      }
      scroll_bar (my_id, sd);
    }
    return;
  }
  if (type == "refreshable_widget") {
    //VUE_WIDGET(refreshable_widget, object, prom, string, kind);
    vue_refreshable_widget_star d= open_box<vue_refreshable_widget_star> (data);
    if (is_nil (d.current) || refresh_stale (d.kind, d.stamp)) {
      // (re)initialize the widget: it is new, or a message of its kind came
      // since it last was, maybe while it was not laid out (refresh_stale)
      d.stamp= refresh_serial;
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
    // a pass-through container: it grows along an axis when its contents
    // do (a column ending with a vertical glue fills the height of its row)
    Clay_Sizing rs= layoutFit;
    if (!is_nil (d.current)) {
      if (widget_grows (d.current, true))  rs.width=  CLAY_SIZING_GROW(0);
      if (widget_grows (d.current, false)) rs.height= CLAY_SIZING_GROW(0);
    }
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= { .sizing= rs }})
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
    if (is_nil (d.current) || refresh_stale (d.kind, d.stamp)) {
      // (re)initialize the widget (see refreshable_widget)
      d.stamp= refresh_serial;
      string s= "'(vertical (link " * d.tmwid * "))";
      eval ("(lazy-initialize-force)");
      object xwid_expanded= eval (s); // evaluated once, was evaluated twice
      object xwid= call ("menu-expand", xwid_expanded);
      // the widgets this one has built, by expansion: its own cache, as in
      // Qt (QTMRefreshWidget::cache); a cache shared by all of them handed
      // the same widget to two refresh widgets showing the same menu
      int k= -1;
      for (int i= 0; i < N(d.cache_keys); i++)
        if (d.cache_keys[i] == xwid) { k= i; break; }
      if (d.curobj == xwid); // unchanged: keep the widget we already have
      else if (k >= 0) {
        d.curobj= xwid;
        d.current= d.cache_widgets[k];
      }
      else {
        d.curobj= xwid;
        d.current= make_menu_widget (xwid_expanded);
        if (menu_caching) {
          d.cache_keys << xwid;
          d.cache_widgets << d.current;
        }
      }
      data= close_box (d);
    }
    // a pass-through container: it grows along an axis when its contents
    // do (a column ending with a vertical glue fills the height of its row)
    Clay_Sizing rs= layoutFit;
    if (!is_nil (d.current)) {
      if (widget_grows (d.current, true))  rs.width=  CLAY_SIZING_GROW(0);
      if (widget_grows (d.current, false)) rs.height= CLAY_SIZING_GROW(0);
    }
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= { .sizing= rs }})
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
    // its style reaches the contents, as a style sheet does in Qt
    // (qt_apply_tm_style): mini, monospaced, bold or greyed texts
    int save_style= context_style;
    context_style |= d.style & inherited_styles;
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
    context_style= save_style;
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
    const float bar= ui_pxf (8);
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
    // The list is as tall as its items, unless it is in a resize (the
    // choice lists of the dialogs are: fill_parent), which may give it less
    // room: then it fills it, clipped, and scrolls, with a scroll bar, as
    // the list view of Qt (QTMListView, made with scroll= true). Anywhere
    // else a clipped list would be shrunk to nothing when the window is
    // sized to its contents: a clip container asks for no room
    bool bounded= fill_parent;
    Clay_ElementId my_id= CLAY_SIDI(CLAY_TM_STRING(type), id);
    CLAY(my_id, {
      .backgroundColor= inert ? color_background : color_field,
      .layout= {
        .layoutDirection=  CLAY_TOP_TO_BOTTOM,
        .sizing= { .width= CLAY_SIZING_GROW(0),
                   .height= bounded ? CLAY_SIZING_GROW(0) : CLAY_SIZING_FIT(0) },
        // the items keep off the rounded corners
        .padding= CLAY_PADDING_ALL (the_theme.radius > 0 ? ui_px (3) : (uint16_t) 0),
        .childGap= ui_px (2) },
      .cornerRadius= ui_corners (),
      .clip= { .horizontal= bounded, .vertical= bounded,
               .childOffset= bounded ? Clay_GetScrollOffset () : (Clay_Vector2) { 0, 0 } }
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
        else if (!inert && hot_id == item_id.id) bg= highlight_on (color_field);
        // the items of a mini list are tighter, as in the mini bars
        uint16_t pad_x= ui_px ((d.style & WIDGET_STYLE_MINI) ? 4 : 8);
        uint16_t pad_y= ui_px ((d.style & WIDGET_STYLE_MINI) ? 1 : 2);
        CLAY(item_id, {
          .layout= { .padding= { pad_x, pad_x, pad_y, pad_y },
                     .sizing= { .width= CLAY_SIZING_GROW(0) }},
          .backgroundColor= bg,
          .cornerRadius= the_theme.radius > 0 ? ui_inner_corners (1, 3) : ui_corners (0.5f) })
        {
          color col= active ? theme_color (the_theme.selection_text)
                            : theme_color (the_theme.text);
          if (inert && !active) col= theme_color (the_theme.text_grey);
          layout_text (d.vals [i], lab_style, col);
        }
      }
    }
    if (bounded) {
      Clay_ScrollContainerData scrollData= Clay_GetScrollContainerData (my_id);
      if (scrollData.found) scroll_bar (my_id, scrollData);
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
        .childGap= ui_px (4) }})
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
          .sizing= { .width= CLAY_SIZING_GROW(0), .height= CLAY_SIZING_GROW(.min= ui_pxf (100)) }},
        .backgroundColor= color_field,
        .cornerRadius= ui_corners (),
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
          else if (hot_id == item_id.id) bg= highlight_on (color_field);
          CLAY(item_id, {
            .layout= { .padding= { ui_px (8), ui_px (8), ui_px (2), ui_px (2) }, .sizing= { .width= CLAY_SIZING_GROW(0) }},
            .backgroundColor= bg,
            .cornerRadius= ui_corners (0.5f) })
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
        .childGap= ui_px (8),
        .childAlignment= { CLAY_ALIGN_X_CENTER, CLAY_ALIGN_Y_CENTER }},
      .backgroundColor= the_theme.balloon,
      .border= { .width= { 1, 1, 1, 1 }, .color= the_theme.balloon_border }})
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
      // for the printer, the copies, the pages and the orientation are kept)
      d.options_for= enum_widget_value (d.printer);
      d.content= make_printer_dialog (d.cmd, d.ps_pdf_file, d.printer, d.copies, d.pages,
                                      d.orientation);
      data= close_box (d);
      layout_again= true; // the window is sized to the new contents
    }
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= { .padding= CLAY_PADDING_ALL(ui_px (16)), .sizing= layoutFit }})
    {
      concrete (d.content)->do_layout ();
    }
    return;
  }
  if (type == "color_picker_widget") {
    //VUE_WIDGET(color_picker_widget, command, cmd, bool, bg, array<tree>, proposals);
    vue_color_picker_widget_star d= open_box<vue_color_picker_widget_star> (data);
    CLAY(CLAY_SIDI (CLAY_TM_STRING (type), id), {
      .layout= { .padding= CLAY_PADDING_ALL(ui_px (16)), .sizing= layoutFit }})
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
    // a 22px box in a 30px cell at 2x (ui_px)
    SI px= ren->pixel, m= ui_px (4) * px, rad= ui_px (4) * px;
    SI x1= r->x1 + m, y1= r->y1 + m, x2= r->x2 - m, y2= r->y2 - m;
    // the colours of the theme: the accent of the selections when on (the
    // soft one when inert), a field when off, lighter or darker when hot
    // as the fields are; an inert box has a fainter frame
    Clay_Color f= d.on ? (inert ? the_theme.selection_soft : the_theme.selection)
                       : (hot ? highlight_on (the_theme.field) : the_theme.field);
    Clay_Color e= d.on ? f : (inert ? mix_colors (the_theme.border, the_theme.background, 0.5f)
                                    : the_theme.border);
    color fill= theme_color (f), edge= theme_color (e);
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
      ren->set_pencil (pencil (theme_color (the_theme.selection_text),
                               max (1, (int) ui_px (3)) * px, cap_round));
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
  bool    word_drag;   // the selection was made by a double or triple
                       // click: the drag which follows does not undo it
  SI      scroll;      // how much the text is scrolled to the left (SI)
  string  pre_edit;     // the text an input method is composing, shown at
  int     pre_edit_pos; // the cursor inside it (a byte offset)
  array<string> tabs;  // tab completions
  int     tab_nr;      // currently visible tab-completion
  int     tab_pos;     // cursor position where tab was pressed

  command tab_cb; // called with #t/#f on tab/shift-tab (moves the focus in dialogs)
  vue_window win; // the window it was last laid out in (weak, only compared)
  
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
  void  select_word (int p);
  widget input_context_menu ();
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

// in device pixels, at 2x (scaled to the density: ui_px)
#define input_pad_x ((int) ui_px (6))
#define input_pad_y ((int) ui_px (3))

vue_input_text_widget_rep::vue_input_text_widget_rep (command _call_back,
          string _type, array<string> _def, int _style, string _width)
  : vue_widget_rep ("input_text_widget"),
    type ("default"), name ("default"), serial ("default"),
    def (_def), call_back (_call_back), style (_style),
    greyed ((_style & WIDGET_STYLE_INERT) != 0), width (_width),
    ok (true), done (false), def_cur (0), pos (0), sel (-1), word_drag (false), scroll (0),
    pre_edit (""), pre_edit_pos (0), tab_nr (0), tab_pos (0), win (NULL)
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

// the word at p (index in s), selected: a run of word characters, or else
// of the other ones (a double click between two words selects the spaces)
void
vue_input_text_widget_rep::select_word (int p) {
  p= max (0, min (p, N(s)));
  // the character after p, unless p is at the end of a word
  int c= p;
  if (c == N(s) || (c > 0 && is_word_char (s, c-1) && !is_word_char (s, c)))
    if (c > 0) tm_char_backwards (s, c);
  bool w= is_word_char (s, c);
  int b= c, e= c;
  while (b > 0) {
    int q= b; tm_char_backwards (s, q);
    if (is_word_char (s, q) != w) break;
    b= q;
  }
  while (e < N(s) && is_word_char (s, e) == w) tm_char_forwards (s, e);
  sel= b; pos= e;
  tabs= array<string> (0);
}

// an item of the context menu of an input field: the key it stands for,
// processed as if typed (so that the field reports its changes the same
// way). It holds the field, which may be gone by the time it is chosen
class input_menu_command_rep : public command_rep {
  widget field;
  string key;
public:
  input_menu_command_rep (widget f, string k) : field (f), key (k) {}
  void apply () {
    vue_input_text_widget_rep* in= dynamic_cast<vue_input_text_widget_rep*> (field.rep);
    if (in != NULL) in->process_key (key);
    gui_needs_relayout= true;
  }
  tm_ostream& print (tm_ostream& out) { return out << "<input_menu " << key << ">"; }
};

widget
vue_input_text_widget_rep::input_context_menu () {
  int b, e;
  bool some= selection (b, e) && b < e;
  bool editable= !greyed;
  widget me (this);
  auto item= [&] (string label, string key, bool active) {
    int st= active ? 0 : WIDGET_STYLE_INERT;
    // the shortcut as the menus of Scheme show it (kbd-system, menu-widget.scm)
    string ks= translate (as_tree (call ("kbd-system-rewrite", key)));
    return menu_button (text_widget (translate (label), st, black),
                        tm_new<input_menu_command_rep> (me, key), "", ks, st);
  };
  array<widget> items;
  items << item ("Cut", "M-x", some && editable)
        << item ("Copy", "M-c", some)
        << item ("Paste", "M-v", editable)
        << menu_separator (false)
        << item ("Select all", "M-a", N(s) > 0);
  return vertical_menu (items);
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
  SI rad= (SI) (ui_pxf (the_theme.radius) * px);
  if (rad > 0) {
    // rounded: the box and a thin frame of the colour of the borders
    ren->set_pencil (pencil (bg));
    ren->rounded_rectangle (r->x1, r->y1, r->x2, r->y2, rad, rad, rad, rad, true);
    ren->set_pencil (pencil (theme_color (the_theme.border), px));
    SI h= px / 2; // the line on the pixels inside the box
    ren->rounded_rectangle (r->x1 + h, r->y1 + h, r->x2 - h, r->y2 - h,
                            rad, rad, rad, rad, false);
  }
  else {
    ren->set_pencil (pencil (bg));
    ren->fill (r->x1, r->y1, r->x2, r->y2);
    // the lowered border: darker above and to the left, lighter below
    ren->set_pencil (pencil (theme_color (the_theme.border)));
    ren->fill (r->x1, r->y2 - px, r->x2, r->y2);
    ren->fill (r->x1, r->y1, r->x1 + px, r->y2);
    ren->set_pencil (pencil (theme_color (the_theme.shade[3])));
    ren->fill (r->x1, r->y1, r->x2, r->y1 + px);
    ren->fill (r->x2 - px, r->y1, r->x2, r->y2);
  }
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
  win= current_window; // see focus_on_named_input
  // a window without keyboard focus gives it to its first field, as a Qt
  // dialog does (an editor embedded in the dialog does not take it from
  // the field, see vue_texmacs_widget_rep::do_layout)
  if (current_window->kbd_focus == NULL && !greyed &&
      is_nil (current_window->default_focus))
    current_window->default_focus= this;
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
  Clay_SizingAxis sw= input_fill ? CLAY_SIZING_GROW (0) : CLAY_SIZING_FIXED (w_px);
  // a width in "w" is a multiple of the default width of an input, as in
  // Qt (qt_decode_length: of its size hint), not of the window: "10w", as
  // the passphrase of the wallet asks, was ten windows wide and pushed the
  // buttons of its dialog out of sight. It fills the room it is given, up
  // to that width.
  if (!input_fill) {
    double w_len; string w_unit;
    parse_length (width, w_len, w_unit);
    if (w_unit == "w")
      sw= CLAY_SIZING_GROW (.min= ui_pxf (60),
                            .max= (float) (w_len * ui_pxf (150)));
  }
  CLAY(cid, {
    .layout= { .sizing= { sw, CLAY_SIZING_FIXED (h_px) } },
    .custom= { .customData= vue_render_widget },
    .userData= render_ref () }) {}
  bool by_words= false; // a double click selected a word
  if (ed.found && (sig.pressed == 1 || (sig.held && (mouse_state & 1) && !word_drag))) {
    // the mouse places the cursor and, dragged, selects; a double click
    // selects the word under the pointer, a triple one the whole field
    SI x= (SI) ((mouse_x - ed.boundingBox.x - input_pad_x)
                * (PIXEL / retina_factor)) + scroll;
    int p= position_at (x);
    if (sig.pressed == 1) {
      mouse_action= "";
      set_kbd_focus (current_window, this);
      word_drag= false;
      if (mouse_clicks >= 3) { sel= 0; pos= N(s); word_drag= true; }
      else if (mouse_clicks == 2) { select_word (p); word_drag= by_words= true; }
      else { pos= p; sel= p; }
    }
    else pos= p;
  }
  if (sig.clicked == 1 && sel == pos && !word_drag) sel= -1;
  if (sig.clicked == 1 && !by_words) word_drag= false;
  if (ed.found && sig.pressed == 3) {
    // the context menu of the field, at the pointer (as the line edits of
    // Qt): the clipboard and the selection
    mouse_action= "";
    set_kbd_focus (current_window, this);
    SI wx, wy;
    current_window->get_position (wx, wy);
    SI x= wx + (SI) (mouse_x * PIXEL / retina_factor);
    SI y= wy - (SI) (mouse_y * PIXEL / retina_factor);
    show_context_menu (input_context_menu (), x, y);
  }
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
  // Only the fields of this window count (the search bar of another
  // window has the same name): those laid out in it, else those not laid
  // out yet, as a bar which has just been built and asks for the keyboard
  // at once (toolbar-search-start); Qt searches the children of the window.
  for (int pass= 0; pass < 4; pass++) {
    iterator<pointer> it= iterate (live_inputs);
    while (it->busy ()) {
      vue_input_text_widget_rep* in= (vue_input_text_widget_rep*) it->next ();
      if (in->win != ((pass < 2) ? win : (vue_window) NULL)) continue;
      if ((pass & 1) == 0 ? (in->name == field) : (in->type == field)) {
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
  bool fits_contents; // it was sized to its contents (a dialog), and is
                      // again when they change size (refit_window)
  bool quit_sent; // the quit command has been queued
  SI last_cw, last_ch; // contents size measured in the previous layout pass
  // the point on which the window is centred once it is sized to its
  // contents (a dialog placed before its size was known, see centre_on)
  bool centre_pending;
  SI centre_x, centre_y;
  float min_cw, min_ch; // smallest size of the contents of a dialog (pixels)
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
  void centre_on (SI x, SI y) { centre_pending= true; centre_x= x; centre_y= y; }
}; // class vue_plain_window_widget_rep

// the texmacs widget learns its window from the window around it (defined
// with vue_texmacs_widget_rep below)
static void texmacs_widget_set_window (vue_widget w, vue_window win);

vue_plain_window_widget_rep::vue_plain_window_widget_rep (widget _wid, string _name,
                                                          command _quit, bool _popup)
: vue_widget_rep (type_vue_plain_window_widget), wid(_wid), name(_name), quit(_quit),
  win (NULL), visible (false), popup (_popup) {
  if (DEBUG_VUE) debug_widgets << "Creating vue_plain_window_widget" << (popup ? " (popup)" : "") << LF;
  // dialogs get their initial size from their contents, the main TeXmacs
  // window and popups are handled differently (see do_layout/post_layout)
  autosize= !popup && concrete (wid)->type != "vue_texmacs_widget_rep";
  fits_contents= autosize;
  last_cw= last_ch= -1;
  min_cw= min_ch= 0;
  quit_sent= false;
  centre_pending= false;
  centre_x= centre_y= 0;
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
        // the editor of the window is in it from now on (SLOT_IDENTIFIER of
        // the texmacs widget), not only once it is laid out: TeXmacs
        // attaches its view at once (attach_view checks is_attached)
        if (!is_nil (wid)) texmacs_widget_set_window (concrete (wid), win);
      }
      break;
    case SLOT_SIZE:
      {
        coord2 p= check_open<coord2> (val, s);
        autosize= false; // an explicit size wins over the contents
        fits_contents= false;
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
    case SLOT_ON_TOP:
      {
        bool flag= check_open<bool> (val, s);
        if (win) win->set_on_top (flag);
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
        // numbered at once, and every window laid out again: the widgets
        // to refresh may be in another window, which the message does not
        // reach (a menu or a popup whose refreshable part depends on a
        // choice made in it, the typographic palette of the colour menus)
        // (not the periodic "auto" one, which each window takes in turn)
        if (kind != "auto") {
          refresh_stamps (kind)= ++refresh_serial;
          gui_needs_relayout= true;
        }
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
    case SLOT_FULL_SCREEN:
      // TeXmacs sends it to the window (tm_frame_rep::full_screen_mode);
      // the texmacs widget inside hides its bars and makes the window full
      // screen. It was dropped here, so presentation mode stayed in the
      // window
      if (!is_nil (wid)) wid->send (s, val);
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
  // (not around the wait indicator drawn in a window with the others, a
  // panel with its own rounded frame: the square line showed at its corners)
  if (popup && !(name == "Wait" && vue_single_window ()))
    border= { .width= { 1, 1, 1, 1 }, .color= { 150, 150, 150, 255 } };
  // no background: process_redraw clears the window with the same colour
  // before replaying the commands, and painting it again here cost a fill
  // of the whole window per frame (see "Rendering details" in
  // docs/vue-graphics-stack.md)
  // A dialog, once it has its size, keeps its contents inside. They take
  // the size of the window, and shrink with it as far as they can: down to
  // the smallest size Clay finds for them (min_cw, min_ch, measured on
  // plain_window_probe, which is not bound, see post_layout). In a window
  // smaller than that they keep that size and scroll (the children of a
  // container which clips are not compressed, see clay.h), as after a
  // resize or on a small page.
  bool scrolled= !popup && !autosize &&
                 concrete (wid)->type != "vue_texmacs_widget_rep";
  refit_window= false; // set by a tabs widget which changed tab
  if (scrolled) {
    Clay_ElementId my_id= CLAY_ID("plain_window_widget");
    float ww= (win != NULL) ? win->layout_w : 0, wh= (win != NULL) ? win->layout_h : 0;
    CLAY(my_id, {
      .layout= { .sizing= layoutFull },
      .border= border,
      .clip= { .horizontal= true, .vertical= true,
               .childOffset= Clay_GetScrollOffset () }})
    {
      CLAY(CLAY_ID("plain_window_contents"), {
        .layout= {
          .sizing= {
            .width=  CLAY_SIZING_GROW(.min= min_cw, .max= (float) max (min_cw, ww)),
            .height= CLAY_SIZING_GROW(.min= min_ch, .max= (float) max (min_ch, wh)) }}})
      {
        CLAY(CLAY_ID("plain_window_probe"), {
          .layout= {
            .layoutDirection= CLAY_TOP_TO_BOTTOM,
            .sizing= layoutExpand }})
        {
          concrete (wid)->do_layout ();
        }
      }
    }
    // the size of the window, made from the size of the contents, may be a
    // pixel short of it (roundings): no scrolling for so little
    Clay_ScrollContainerData sd= Clay_GetScrollContainerData (my_id);
    if (sd.found) {
      const float slack= 2.0f;
      bool sx= sd.contentDimensions.width  > sd.scrollContainerDimensions.width  + slack;
      bool sy= sd.contentDimensions.height > sd.scrollContainerDimensions.height + slack;
      if (!sx) sd.scrollPosition->x= 0;
      if (!sy) sd.scrollPosition->y= 0;
      if (sx || sy) {
        Clay_ScrollContainerData bars= sd;
        if (!sx) bars.contentDimensions.width = bars.scrollContainerDimensions.width;
        if (!sy) bars.contentDimensions.height= bars.scrollContainerDimensions.height;
        scroll_bar (my_id, bars);
      }
    }
  }
  else
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
  if (refit_window && fits_contents && !autosize) {
    autosize= true; // from the next pass, see post_layout
    last_cw= last_ch= -1;
    layout_again= true;
  }
  refit_window= false;
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
  // the smallest size of the contents of a dialog (see do_layout): laid out
  // again when it changed
  if (!(popup || autosize)) {
    Clay_Dimensions m= vue_clay_min_dimensions (CLAY_ID("plain_window_probe"));
    if (m.width > 0 && m.height > 0 &&
        (fabs (m.width - min_cw) > 0.5f || fabs (m.height - min_ch) > 0.5f)) {
      min_cw= m.width; min_ch= m.height;
      win->ready_to_show= true;
      return true;
    }
  }
  // the window may be shown once its contents fit in it (see vue_window_rep)
  if (!(popup || autosize)) {
    win->ready_to_show= true;
    return false;
  }
  // (not before a window to be centred has been moved to its place)
  if (abs (w - cw) <= PIXEL && abs (h - ch) <= PIXEL && !centre_pending)
    win->ready_to_show= true;
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
  if (!popup && centre_pending) {
    // the size is final: the window goes where it was meant to be centred
    // (y upwards, the position is the top left corner)
    centre_pending= false;
    win->set_position (centre_x - cw / 2, centre_y + ch / 2);
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

  // the interactive footer (the preference "interactive footer"): the
  // properties of the text at the cursor and the tags around it as menus,
  // see (texmacs menus footer-menu), in place of the texts of the footer
  // while the editor shows those (footer_menus) and not a message; the
  // menus are rebuilt when their expansions change, as the tool bars
  // (tm_window_rep::get_menu_widget)
  bool footer_menus;
  object footer_env_menu, footer_path_menu;
  vue_widget footer_env, footer_path;
  float footer_room; // the width of the tags in the last layout, points
  void update_footer_menus ();

  // the title of the window and the marker of a document with unsaved
  // changes. TeXmacs sends them to this widget, which only learns which
  // window it is in when it is first laid out: what arrives before that
  // (the name of the document always does) is kept here and given to the
  // window then, or the title would stay the one given at creation
  string win_title;
  bool win_title_set;
  bool win_modified, win_modified_set;

public:
  vue_texmacs_widget_rep (int _mask, command _quit);
  
  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
  void notify (slot s, blackbox new_val);
  
  void do_layout ();
  friend void texmacs_widget_set_window (vue_widget w, vue_window win);
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
    win (NULL), interactive_mode (false), footer_menus (false), footer_room (0),
    win_title_set (false), win_modified (false), win_modified_set (false)
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
      // the editor sends the left footer first (edit_interface_rep::
      // set_footer), having said what it shows
      footer_menus= get_preference ("interactive footer") == "on" &&
                    as_bool (call ("footer-environment?"));
      break;
      
    case SLOT_RIGHT_FOOTER:
      right_footer= check_open<string> (val, s);
      if (footer_menus) update_footer_menus ();
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
      
    case SLOT_NAME:
      // the title of the window: TeXmacs sends it to the widget of the
      // editor (tm_window_rep::wid), which is this one, and the window
      // below is what shows it. It was not forwarded at all, so the title
      // never named the document
      win_title= check_open<string> (val, s);
      win_title_set= true;
      if (win) { win->set_name (win_title); win_title_set= false; }
      break;

    case SLOT_MODIFIED:
      win_modified= check_open<bool> (val, s);
      win_modified_set= true;
      if (win) { win->set_modified (win_modified); win_modified_set= false; }
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
      {
        bool flag= check_open<bool> (val, s);
        // no scroll bars on the slides, as in Qt
        vue_simple_widget_rep* canvas=
          dynamic_cast<vue_simple_widget_rep*> (main_widget.rep);
        if (canvas) canvas->scrollbars_hidden= flag;
        if (win) win->set_full_screen (flag);
      }
      break;

    case SLOT_KEYBOARD_FOCUS_ON:
      if (win) focus_on_named_input (win, check_open<string> (val, s));
      break;

    case SLOT_KEYBOARD_FOCUS:
      // (keyboard-focus-on "canvas"), e.g. when the search bar closes: the
      // keyboard goes back to the editor (qt_tm_widget_rep gives it to the
      // canvas); it was dropped, and the typing went on into the bar
      if (check_open<bool> (val, s) && win && !is_nil (main_widget))
        set_kbd_focus (win, main_widget);
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
      {
        check_type_void (index, s);
        // the editor which is switched out is no longer in a window (its
        // SLOT_IDENTIFIER answers 0, as in Qt) and the new one is in ours
        // at once, before it is laid out
        vue_simple_widget_rep* old=
          dynamic_cast<vue_simple_widget_rep*> (main_widget.rep);
        if (old != NULL) old->win= NULL;
        main_widget= concrete (w);
        vue_simple_widget_rep* cur=
          dynamic_cast<vue_simple_widget_rep*> (main_widget.rep);
        if (cur != NULL && win != NULL) cur->win= win;
        if (win) set_kbd_focus (win, main_widget);
      }
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
    case SLOT_IDENTIFIER:
      // the window it is in, as the Qt widgets answer through their
      // window (0: in none, see is_attached). This was a counter which
      // counted up at every query and never said 0
      check_type_id<int> (type_id, s);
      return close_box<int> (win ? win->id : 0);

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
  if (side) sizing= { CLAY_SIZING_FIT (.min= ui_pxf (150), .max= (float) max (150.0, 0.4 * win_w)),
                      CLAY_SIZING_GROW(0) };
  else      sizing= { CLAY_SIZING_GROW(0),
                      CLAY_SIZING_FIT (.min= ui_pxf (40), .max= (float) max (40.0, 0.4 * win_h)) };
  Clay_BorderWidth bw= side ? (Clay_BorderWidth) { 1, 1, 0, 0 } : (Clay_BorderWidth) { 0, 0, 1, 1 };
  CLAY(id, {
    .backgroundColor= palette[2],
    .layout= {
      .sizing= sizing,
      .padding= CLAY_PADDING_ALL(ui_px (10)),
      .childGap= ui_px (10),
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
    with_behind b (palette[2]);
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
// (2x values, scaled to the density: ui_px)
#define bar_hpad     ui_px (24)   // contents clear of the window edges
#define bar_menu_h   ui_pxf (62)
#define bar_main_h   ui_pxf (72)
#define bar_mode_h   ui_pxf (62)
#define bar_focus_h  ui_pxf (56)
#define bar_footer_h ui_pxf (56)

static void
footer_menu (object& current, vue_widget& w, string menu) {
  object m= eval ("'" * menu);
  object x= call ("menu-expand", m);
  if (!is_nil (w) && x == current) return;
  current= x;
  w= concrete (make_menu_widget (m));
}

void
vue_texmacs_widget_rep::update_footer_menus () {
  // the room of the tags, in characters (their width in the last pass,
  // at some 7 points a character): the outer tags which do not fit are
  // folded into a menu, see (texmacs menus footer-menu)
  if (footer_room > 0)
    call ("footer-set-budget", object (max ((int) (footer_room / 7.0f), 12)));
  footer_menu (footer_env_menu, footer_env,
               "(horizontal (link texmacs-footer-environment))");
  footer_menu (footer_path_menu, footer_path,
               "(horizontal (link texmacs-footer-path))");
}

// The contents of a bar of the main window, clipped to its width: when
// they do not fit, the markers at the ends say so and a click on one
// brings the rest into view. A scroll bar is not an option here: a bar
// which is too narrow for its buttons has no room to spare for one, and it
// would cover them. The key is unique per bar and per editor widget.
static void
layout_bar_content (int key, vue_widget content, Clay_Color bg) {
  Clay_ElementId clip_id= CLAY_IDI ("bar_clip", key);
  CLAY(clip_id, {
    .layout= {
      .sizing= { CLAY_SIZING_GROW(0), CLAY_SIZING_FIT(0) },
      .childAlignment= { .y= CLAY_ALIGN_Y_CENTER }},
    .clip= { .horizontal= true, .childOffset= Clay_GetScrollOffset () }})
  {
    with_behind b (bg);
    content->do_layout ();
  }
  Clay_ScrollContainerData sd= Clay_GetScrollContainerData (clip_id);
  if (sd.found) scroll_markers (clip_id, sd, bg, true, 2);
}

// a bar as a column at the left of the editor (see in_side_bar): the
// height of the editor, which it scrolls when its icons do not fit
static void
layout_side_bar_content (int key, vue_widget content, Clay_Color bg) {
  Clay_ElementId clip_id= CLAY_IDI ("side_bar_clip", key);
  CLAY(clip_id, {
    .layout= {
      .padding= { ui_px (4), ui_px (4), ui_px (6), ui_px (6) },
      .sizing= { CLAY_SIZING_FIT(0), CLAY_SIZING_GROW(0) },
      .childAlignment= { .x= CLAY_ALIGN_X_CENTER }},
    .backgroundColor= bg,
    .border= { .width= { .right= 2 }, .color= the_theme.bar_line },
    .clip= { .vertical= true, .childOffset= Clay_GetScrollOffset () }})
  {
    with_behind b (bg);
    in_tool_bar= true; in_side_bar= true;
    content->do_layout ();
    in_tool_bar= false; in_side_bar= false;
  }
  Clay_ScrollContainerData sd= Clay_GetScrollContainerData (clip_id);
  if (sd.found) scroll_markers (clip_id, sd, bg, false, 2);
}

static void
texmacs_widget_set_window (vue_widget w, vue_window win) {
  vue_texmacs_widget_rep* tw= dynamic_cast<vue_texmacs_widget_rep*> (w.rep);
  if (tw == NULL) return;
  tw->win= win;
  vue_simple_widget_rep* canvas=
    dynamic_cast<vue_simple_widget_rep*> (tw->main_widget.rep);
  if (canvas != NULL) canvas->win= win;
}

void vue_texmacs_widget_rep::do_layout () {
  win= current_window; // save the info
  // what TeXmacs sent before we knew our window (see win_title)
  if (win != NULL) {
    if (win_title_set) { win->set_name (win_title); win_title_set= false; }
    if (win_modified_set) {
      win->set_modified (win_modified); win_modified_set= false;
    }
  }
  // grow to the size of the window
  SI w= 300, h= 300;
  if (win) win->get_size (w, h);
  // the editor gets the focus of a new window, but not from here: a focus
  // is a change of the editor (freeze, focus, decorations), which the
  // interpose handler applies, and a window first laid out after it -- one
  // opened by a command, the second file on the command line -- would be
  // repainted with the change pending ("Invalid situation" in
  // edit_interface_rep::handle_repaint). The loop gives it just before
  // the interpose handler (apply_default_focus in vue_gui.cpp)
  // an embedded editor (mask 0: texmacs-input in a dialog or a tool) leaves
  // it to a field of the window laid out before it, as Qt gives the
  // keyboard of a dialog to its first field
  if (win->kbd_focus == NULL && (mask != 0 || is_nil (win->default_focus)))
    win->default_focus= main_widget;
  // the bars follow the mask given at creation and the visibility slots
  // (the icon bars go with the header, as in Qt: presentation mode hides
  // the header only);
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
      if (!is_nil (main_menu))
        layout_bar_content (8*id + 0, main_menu, color_background);
    }
    bool side= bars_on_side ();
    if (visibility[0] && visibility[1] && !side) CLAY(CLAY_ID_LOCAL("MainToolbar"), {
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
        in_tool_bar= true;
        layout_bar_content (8*id + 1, main_icons, color_background);
        in_tool_bar= false;
      }
    }
    if (visibility[0] && visibility[2] && !side) CLAY(CLAY_ID_LOCAL("ModeToolbar"), {
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
        in_tool_bar= true;
        layout_bar_content (8*id + 2, mode_icons, the_theme.bar_mode);
        in_tool_bar= false;
      }
    }
    // the main and mode bars as two columns at the left, side by side (see
    // in_side_bar), from the menu bar down to the footer; the focus bar,
    // the user bar and the editor with its tools at their right
    bool main_side= side && visibility[0] && visibility[1] && !is_nil (main_icons);
    bool mode_side= side && visibility[0] && visibility[2] && !is_nil (mode_icons);
    auto body= [&] () {
      if (visibility[0] && visibility[3]) CLAY(CLAY_ID_LOCAL("FocusToolbar"), {
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
          in_tool_bar= true;
          layout_bar_content (8*id + 3, focus_icons, the_theme.bar_focus);
          in_tool_bar= false;
        }
      }
      // the user icon bar, which a document may fill through its style
      // (the bar was received and stored, and never drawn)
      if (visibility[0] && visibility[4] && !is_nil (user_icons))
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
          in_tool_bar= true;
          layout_bar_content (8*id + 4, user_icons, the_theme.bar_focus);
          in_tool_bar= false;
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
    };
    if (main_side || mode_side)
      CLAY(CLAY_ID_LOCAL("Body"), {
        .layout= {
          .layoutDirection= CLAY_LEFT_TO_RIGHT,
          .sizing= layoutExpand }})
      {
        if (main_side) layout_side_bar_content (8*id + 1, main_icons, color_background);
        if (mode_side) layout_side_bar_content (8*id + 2, mode_icons, the_theme.bar_mode);
        CLAY(CLAY_ID_LOCAL("BodyRight"), {
          .layout= {
            .layoutDirection= CLAY_TOP_TO_BOTTOM,
            .sizing= layoutExpand }})
        {
          body ();
        }
      }
    else body ();
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
        CLAY_AUTO_ID({ .layout= { .sizing= { CLAY_SIZING_FIXED(ui_pxf (8)) }}}) {}
        CLAY_AUTO_ID({
          .layout= { .sizing= { .width= CLAY_SIZING_GROW(0) } },
          .clip= { .horizontal= true }})
        {
          interactive_input->do_layout ();
        }
      }
      else if (footer_menus && !is_nil (footer_env) && !is_nil (footer_path)) {
        // the interactive footer: the properties on the left, the tags on
        // the right, which give way to the properties when space is short
        // (clipped on their left: the innermost tags stay in view)
        in_footer= true;
        footer_env->do_layout ();
        // when the tags are wider than their room, they are shifted left by
        // the difference (as measured in the last pass): the end of the
        // path, the innermost tags and the character, stays in view
        Clay_ElementId path_id= CLAY_IDI ("footer_path", id);
        Clay_ScrollContainerData pd= Clay_GetScrollContainerData (path_id);
        float shift= 0;
        if (pd.found && pd.contentDimensions.width > pd.scrollContainerDimensions.width)
          shift= pd.scrollContainerDimensions.width - pd.contentDimensions.width;
        if (pd.found) footer_room= pd.scrollContainerDimensions.width / retina_factor;
        CLAY(path_id, {
          .layout= {
            .sizing= { .width= CLAY_SIZING_GROW(0) },
            .childAlignment= { .x= CLAY_ALIGN_X_RIGHT, .y= CLAY_ALIGN_Y_CENTER }},
          .clip= { .horizontal= true, .childOffset= { shift, 0 } }})
        {
          footer_path->do_layout ();
        }
        in_footer= false;
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

// The backing store of an editor and its renderer: a texture when the
// windows are drawn by the GPU (vue_gpu.cpp), else an opaque MuPDF pixmap,
// so that blitting it into the window needs no test per pixel (see
// native_opaque_picture)
static picture
backing_picture (int w, int h) {
  if (vue_gpu_windows ()) return gpu_backing_picture (w, h);
  return native_opaque_picture (w, h, 0, 0);
}

static renderer
backing_renderer (picture p) {
  if (is_gpu_picture (p)) return gpu_picture_renderer (p, std_shrinkf * retina_factor);
  return picture_renderer (p, std_shrinkf * retina_factor);
}

vue_simple_widget_rep::vue_simple_widget_rep ()
: vue_widget_rep (vue_type_simple_widget),
  win (NULL),
  size (coord2 (0, 0)),
  extents (0,0,0,0),
  scroll_pos (coord2 (0, 0)),
  cursor_pos (coord2 (0, 0)),
  mouse_grab (false),
  absolute_scroll (false),
  scroll_pending (false),
  scrollbars_hidden (false),
  pointer_captured (false),
  ren (NULL),
  backing_pos (coord2 (0, 0)), origin (coord2 (0, 0)),
  backing_valid (false),
  resize_pending (false),
  cursor_moved (false), ime_x (-1), ime_y (-1),
  scroll_rest_x (0), scroll_rest_y (0),
  zoom_ratio (1.0), zoom_now (0.0), zoom_pos (coord2 (0, 0)), zoom_start (0)
{
  // note that size is set to an arbitrary value to init the backing_store
  // create a backing store and the renderer
  backing_store= backing_picture (size.x1, size.x2);
  ren= backing_renderer (backing_store);
  ren_retina= retina_factor;
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
        start_zoom_transition (new_zoom);
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
        // the input area of the window follows it once the scroll which
        // may come with it is applied (update_text_input_area)
        cursor_pos= check_open <coord2> (val, s);
        cursor_moved= true;
      }
      break;
    case SLOT_KEYBOARD_FOCUS:
      // the editor asks for the keyboard (send_keyboard_focus: a new view,
      // a click, the end of an interactive command), as qt_widget_rep
      // gives it to its widget
      if (check_open<bool> (val, s) && win) set_kbd_focus (win, this);
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
      // (origin is in device pixels of the window, at its own density)
      check_type_id<coord2> (type_id, s);
      int rf= win ? win->retina : retina_factor;
      return close_box<coord2> (coord2 (origin.x1 * PIXEL / rf,
                                        -origin.x2 * PIXEL / rf));
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
  layout_who= is_editor_widget () ? string ("editor") : string ("typeset box");
  win= current_window; // save the info
  // the zoom the editor starts with (the first one is not sent to the
  // widget): the smooth zoom needs the old zoom to scale from
  if (zoom_now == 0 && is_editor_widget ())
    zoom_now= as_double (call ("get-window-zoom-factor"));
  SI w= 0, h= 0;
  Clay_Sizing s= layoutExpand;
  if (is_embedded_widget () && !is_editor_widget ()) {
    // typeset boxes (texmacs-output) have their natural size; editors,
    // embedded or not, fill their container (their size hint is the screen)
    handle_get_size_hint (w, h);
    // a typeset box has its natural size, unless it was given one: a
    // "resize" around it is a pane of that size, which it has to fill
    // (its own background is painted over the whole of its rectangle)
    if (fill_parent)
      s= { .width=  CLAY_SIZING_GROW(.min= (float)w/ren->pixel),
           .height= CLAY_SIZING_GROW(.min= (float)h/ren->pixel) };
    else
      s= { .width=  CLAY_SIZING_FIT(.min= (float)w/ren->pixel),
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
        .layout= { .padding= { ui_px (18), ui_px (18), ui_px (18), ui_px (18) } },
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
    if (!scrollbars_hidden) scroll_bar (clay_id, scrollData);
    // only a dragged thumb changes the position here; assigning it back
    // unconditionally dropped the scroll requests of the editor (scroll to
    // the cursor) whenever a second layout pass followed in the same
    // iteration of the loop
    if (scrollPosition.x != before.x || scrollPosition.y != before.y) {
      scroll_pos.x1= -((SI) floor (scrollPosition.x + 0.5)) * ren->pixel;
      scroll_pos.x2=  ((SI) floor (scrollPosition.y + 0.5)) * ren->pixel;
      absolute_scroll= false;
      scroll_pending= true;
    }
  }
  // note: our CLAY block is closed here, Clay_Hovered () would test the parent
  bool over= Clay_PointerOver (clay_id);
  // A press on the canvas captures the pointer until the buttons are
  // released: the moves and the release reach the editor wherever they
  // happen, beyond the viewport and outside the window (SDL keeps sending
  // them while a button is held), so that a drag selection extends past
  // the visible part and the editor scrolls to follow it, as with Qt. A
  // release which never reached us (the buttons are up) ends it too
  if (pointer_captured && (mouse_state & 7) == 0 &&
      !starts (mouse_action, "release-"))
    pointer_captured= false;
  bool captured= pointer_captured && d.found &&
                 (mouse_action == "move" || starts (mouse_action, "release-"));
  if (over && starts (mouse_action, "press-")) pointer_captured= true;
  if (starts (mouse_action, "release-")) pointer_captured= false;
  if ((over || captured) && (mouse_action != "")) {
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
    if (mouse_action == "wheel" && is_editor_widget () &&
        as_bool (call ("wheel-capture?"))) {
      // the editor wants the wheel (in graphics mode, see QTMWidget::
      // wheelEvent): it gets it as a "wheel" event, deltas in points
      // (the displacement of a trackpad, as Qt's pixelDelta), instead of
      // the view being scrolled
      // (mouse_data is in SI here, ren->pixel per device pixel)
      array<double> data;
      if (N(mouse_data) == 2) {
        double f= (double) ren->pixel * (win ? win->density : 1.0f);
        data << mouse_data[0] / f << mouse_data[1] / f;
      }
      handle_mouse ("wheel", x, y, (int) mouse_state, mouse_time, data);
    }
    else if (mouse_action == "wheel") {
      // the deltas come in small steps (see "Scrolling with the wheel" in
      // vue_gui.cpp): the fractions of SI are carried over to the next step
      // and the deltas add up on top of a position which is still pending.
      // Starting from backing_pos each time lost every delta but the last
      // whenever the repaint was skipped, which is exactly what a fast
      // swipe does: its events never stop coming, so the repaint, which is
      // what moves backing_pos, never got its turn and the page stood still
      if (!scroll_pending || absolute_scroll) scroll_pos= backing_pos;
      absolute_scroll= false;
      scroll_pending= true;
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
  if (is_gpu_picture (backing_store)) {
    gpu_translate_picture (backing_store, dpx, dpy);
    return;
  }
  fz_pixmap *pix=  ((mupdf_picture_rep*)backing_store->get_handle())->pix;
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

  // Everything below happens at the density of our window: with_window
  // makes its factor the one of the renderers (retina_factor), so that the
  // backing store is made and painted at the resolution of the display the
  // window is on, not at that of whichever window happened to be current
  // (the loop repaints all the editors outside any window)
  with_window frame (w->win);

  // retrieve current geometry
  Clay_ElementId clay_id= CLAY_IDI("simple_widget", id);
  {
    Clay_ElementData d= Clay_GetElementData (clay_id);
    if (d.found) {
      // cache the current viewport size
      size.x1= d.boundingBox.width; // * retina_factor;
      size.x2= d.boundingBox.height; // * retina_factor;
      origin.x1= (SI) d.boundingBox.x;
      origin.x2= (SI) d.boundingBox.y;
    } else {
      // Not in the layout, so not on the screen: nothing to paint, and the
      // regions stay invalid until it is laid out again. An editor whose
      // view was taken out of its window is such a widget -- the "no name"
      // one, replaced by a file named on the command line before it was
      // ever painted -- and repainting it made its detached view current
      // (SERVER in edit_interface_rep::update_visible), where TeXmacs
      // stopped: "no window attached to view".
      if (DEBUG_VUE_WIDGETS) debug_widgets << "clay_id of a simple widget not found" << LF;
      return;
    }
  }

  // current backing_store size
  int bs_w= backing_store->get_width ();
  int bs_h= backing_store->get_height ();
  // A renderer made at another density (the window moved to a display of
  // another density, or the widget to another window) is replaced by one at
  // the density of our window, and everything is painted again: nothing of
  // the old pixels can be reused, and the scroll below must already count
  // with the new size of a pixel
  if (ren_retina != retina_factor) {
    SI old_w= bs_w * ren->pixel, old_h= bs_h * ren->pixel;
    bs_w= size.x1; bs_h= size.x2;
    backing_store= backing_picture (bs_w, bs_h);
    delete_renderer (ren);
    ren= backing_renderer (backing_store);
    ren_retina= retina_factor;
    backing_valid= false;
    invalidate_all ();
    if (old_w != bs_w * ren->pixel || old_h != bs_h * ren->pixel)
      resize_pending= true; // see below
  }

  // Update the scroll position

  // viewport size (in TeXmacs units)
  coord2 sz (size.x1 * ren->pixel, size.x2 * ren->pixel);

  // the extents may have changed since the last repaint (e.g. the paper is
  // centered in a wider viewport): the position is clamped in any case
  {
    // preprocess scroll_pos
    if (absolute_scroll) {
      // SLOT_SCROLL_POSITION names the point which is to be at the centre
      // of the view, as in Qt (qt_simple_widget_rep: origin = p - size/2),
      // whether it is visible already or not: the editor relies on it to
      // move the view by a little (selection_visible during a drag, which
      // asks for a centre slightly off the current one) and to centre a
      // page ("snap to pages"). It used to move only when the point was
      // outside the view, so a drag selection never scrolled
      coord2 pt= scroll_pos;
      scroll_pos.x1= pt.x1 - sz.x1/2;
      scroll_pos.x2= pt.x2 + sz.x2/2; // y upwards: the top of the view
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
    scroll_pending= false; // the position below is the one asked for
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
    picture new_backing_store= backing_picture (new_bs_w, new_bs_h);
    renderer ren2= backing_renderer (new_backing_store);
    
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
    ren_retina= retina_factor;
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
    // the profile counts the time the GPU took to draw it
    if (vue_profile_on && is_gpu_picture (backing_store)) gpu_finish ();
  } // if (!is_nil (invalid_regions))
  backing_valid= true;
  update_text_input_area ();
}

// The input methods show their candidates next to the cursor of the editor
// which has the keyboard: SDL is told where it is (SDL_SetTextInputArea, in
// points of the window), as Qt answers ImCursorRectangle from the position
// of SLOT_CURSOR. Done after the scroll, which moves the cursor in the
// window, and only when it changed. A virtual window (a tab, a dialog of
// single-window mode) is shown in its host, the area is set there. In the
// browser, where SDL has no input method, the hidden text area of the page
// in which they compose goes there (tmIme.caret, misc/wasm/ime.js).
void
vue_simple_widget_rep::update_text_input_area () {
  if (win == NULL || win->kbd_focus != this) return;
  float dx, dy;
  SDL_Window* sw= (SDL_Window*) vue_shown_in (win, dx, dy);
  if (sw == NULL) return;
  SI x= cursor_pos.x1, y= cursor_pos.x2;
  ren->set_origin (-backing_pos.x1, -backing_pos.x2);
  ren->decode (x, y); // pixels of the backing store, from its top left
  float d= (win->density > 0.0f) ? win->density : 1.0f;
  int px= (int) (dx + (origin.x1 + x) / d), py= (int) (dy + (origin.x2 + y) / d);
  if (!cursor_moved && px == ime_x && py == ime_y) return;
  cursor_moved= false;
  ime_x= px; ime_y= py;
  // a thin box on the baseline, the candidates go below it
  SDL_Rect r= { px, py - 12, 2, 16 };
  SDL_SetTextInputArea (sw, &r, 0);
#ifdef __EMSCRIPTEN__
  EM_ASM ({ if (typeof tmIme !== 'undefined' && tmIme) tmIme.caret ($0, $1, $2); },
          r.x, r.y, r.h);
#endif
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
  if (!is_nil (zoom_snap) && render_zoom (data)) return;
  current_window->draw_picture (data, backing_store);
}

// the backing store is opaque (native_opaque_picture) and drawn at the
// corner of the box: it covers the box when it is as large, which it is
// but while the box is being resized (and during the smooth zoom, which
// draws something else)
bool
vue_simple_widget_rep::renders_opaque (int w, int h) {
  if (!is_nil (zoom_snap) || is_nil (backing_store)) return false;
  if (is_gpu_picture (backing_store)) // a texture, opaque
    return backing_store->get_width () >= w && backing_store->get_height () >= h;
  mupdf_picture_rep* p= (mupdf_picture_rep*) backing_store->get_handle ();
  return p != NULL && p->opaque && p->w >= w && p->h >= h;
}

/******************************************************************************
* The smooth zoom
*
* When the zoom of an editor changes, the page does not jump from the old
* size to the new: for a moment (zoom_duration) the old picture grows or
* shrinks to the new size and fades out, over the new one, which grows or
* shrinks from the old size to its own. Both are scaled about the point of
* the view which the zoom leaves in place, as the editor scrolled after it.
* The old picture is a copy of the backing store taken when the new zoom
* arrives (it has not been repainted yet); the new one is the backing store,
* repainted meanwhile. The preference "smooth zoom" (default on) turns it
* off; zooms by less than 3% (a pinch, a step of the wheel) are immediate.
******************************************************************************/

extern time_t vue_animation_until; // vue_gui.cpp: frames until then
static const double zoom_duration= 150.0; // ms

void
vue_simple_widget_rep::start_zoom_transition (double new_zoom) {
  double old_zoom= zoom_now;
  zoom_now= new_zoom;
  if (!is_editor_widget () || old_zoom <= 0 || new_zoom <= 0) return;
  double r= new_zoom / old_zoom;
  if (fabs (log (r)) < 0.03 || !backing_valid || is_nil (backing_store)) return;
  if (get_preference ("smooth zoom", "on") == "off") return;
  if (is_gpu_picture (backing_store)) {
    zoom_snap= gpu_copy_picture (backing_store);
    if (is_nil (zoom_snap)) return;
    zoom_ratio= r;
    zoom_pos= backing_pos;
    zoom_start= 0;
    vue_animation_until= max (vue_animation_until, texmacs_time () + 1000);
    return;
  }
  mupdf_picture_rep* pict=
    (mupdf_picture_rep*) as_mupdf_picture (backing_store)->get_handle ();
  if (pict == NULL || pict->pix == NULL) return;
  fz_pixmap* copy= NULL;
  fz_try (mupdf_context ()) { copy= fz_clone_pixmap (mupdf_context (), pict->pix); }
  fz_catch (mupdf_context ()) { copy= NULL; }
  if (copy == NULL) return;
  mupdf_picture_rep* snap= tm_new<mupdf_picture_rep> (copy, pict->ox, pict->oy);
  snap->opaque= pict->opaque; // the fast path of draw_picture_scaled
  zoom_snap= picture (snap);
  fz_drop_pixmap (mupdf_context (), copy);
  zoom_ratio= r;
  zoom_pos= backing_pos;
  // the transition starts with its first frame (render_zoom): the repaint
  // of the editor at the new zoom, which comes first, may take longer than
  // the whole transition
  zoom_start= 0;
  vue_animation_until= max (vue_animation_until, texmacs_time () + 1000);
}

bool
vue_simple_widget_rep::render_zoom (void *data) {
  vue_render_ren_data* d= (vue_render_ren_data*) data;
  mupdf_renderer_rep* mr= dynamic_cast<mupdf_renderer_rep*> (d->ren);
  bool gpu= is_gpu_renderer (d->ren);
  time_t now= texmacs_time ();
  if (zoom_start == 0) {
    zoom_start= now;
    vue_animation_until= now + (time_t) zoom_duration + 20;
  }
  double t= (now - zoom_start) / zoom_duration;
  if ((mr == NULL && !gpu) || t >= 1.0 || t < 0.0) { zoom_snap= picture (); return false; }
  double u= 1.0 - (1.0 - t) * (1.0 - t) * (1.0 - t); // ease out
  // a point of the view (pixels from its top left) moves by the zoom from
  // x to r x + T, as the editor scrolled (backing_pos: the top left of the
  // view in the zoomed document, y upwards); the point which stays is
  // c = T / (1 - r), and at the time u the old picture is scaled by r^u
  // about c, the new one by r^u / r
  double r= zoom_ratio;
  double pe= (double) ren->pixel;
  double tx= (r * zoom_pos.x1 - backing_pos.x1) / pe;
  double ty= (backing_pos.x2 - r * zoom_pos.x2) / pe;
  double cx= tx / (1.0 - r), cy= ty / (1.0 - r);
  double s= pow (r, u);
  rectangle rr= d->r;
  renderer R= d->ren;
  SI P= R->pixel;
  R->clip (rr->x1, rr->y1, rr->x2, rr->y2);
  // the room which the pictures leave, in the colour of the canvas
  R->set_pencil (backing_store->get_pixel (0, 0));
  R->fill (rr->x1, rr->y1, rr->x2, rr->y2);
  auto place= [&] (picture p, double sc, int alpha) {
    double left= cx * (1.0 - sc), top= cy * (1.0 - sc);
    SI x= rr->x1 + (SI) (left * P);
    SI y= rr->y2 - (SI) ((top + p->get_height () * sc) * P);
    if (gpu) gpu_draw_picture_scaled (R, p, x, y, sc, alpha);
    else mr->draw_picture_scaled (p, x, y, sc, alpha);
  };
  uint64_t TMPT0= SDL_GetTicksNS ();
  place (backing_store, s / r, 255);
  place (zoom_snap, s, (int) (255.0 * (1.0 - u)));
  R->unclip ();
  cout << "TMPF t=" << (int) (t*1000) << " draw " << (int) ((SDL_GetTicksNS () - TMPT0)/1000000) << "ms" << LF;
  return true;
}

//-----------------------------------------------------------------------------
//vue_chooser_widget

#include "editor.hpp"   // get_current_editor ()->as_length (image sizes)
#include "new_view.hpp"

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

// the width and height of an image as pretty TeXmacs lengths, the policy
// of qt_pretty_image_size (and of vue_pretty_image_size for the drops): the
// size in points, or the width of the line for a wider image; nothing for
// the formats the box sizes itself
static void
chooser_pretty_image_size (url image, string& w, string& h) {
  w= ""; h= "";
  string ext= locase_all (suffix (image));
  if (ext == "pdf" || ext == "ps" || ext == "eps") return;
  picture pic= load_picture (image, -1, -1, tree (""), PIXEL);
  if (is_nil (pic)) return;
  int ww= pic->get_width (), hh= pic->get_height ();
  SI pt= get_current_editor () -> as_length ("1pt");
  SI par= get_current_editor () -> as_length ("1par");
  if (ww <= 0 || hh <= 0 || ww * pt > par) { w= "1par"; h= ""; }
  else { w= as_string (ww) * "pt"; h= as_string (hh) * "pt"; }
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
      chooser_pretty_image_size (u, w, h);
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
  cmd (_cmd), size (coord2 (0, 0)),
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
  // dialogue_start centres the dialog on its window with the size we
  // report, which is not known before the dialog is laid out: the window
  // is centred on that point once it has been sized to its contents
  vue_plain_window_widget_rep* ww=
    dynamic_cast<vue_plain_window_widget_rep*> (win_widget.rep);
  if (ww != NULL)
    ww->centre_on (position.x1 + size.x1 / 2, position.x2 - size.x2 / 2);
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
        .sizing= { CLAY_SIZING_FIXED(ui_pxf (24)), CLAY_SIZING_FIT(0) },
        .padding= { ui_px (4), ui_px (4), ui_px (2), ui_px (2) }}})
    {
      if (kids) layout_arrow (open ? "<#25BE>" : "<#25B8>", open ? 3 : 1, dark_grey);
    }
    CLAY(label_id, {
      .layout= { .padding= { ui_px (4), ui_px (8), ui_px (2), ui_px (2) }, .sizing= { .width= CLAY_SIZING_GROW(0) }},
      .backgroundColor= (hot_id == label_id.id) ? highlight_on (color_field)
                                                 : color_field })
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
    plain_window (wwid, s, false,
                  concrete (wid)->type == "vue_texmacs_widget_rep");
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
  } else if (dynamic_cast<vue_chooser_widget_rep*> (vw.rep) != NULL) {
    // the file chooser is the system dialog, which closes itself: there
    // is no window of ours to destroy (see plain_window_widget)
  } else {
    cout << "not a window widget!" << LF;
  }
}
// destroys a window as created by the above routines

