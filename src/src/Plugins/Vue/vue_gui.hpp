/******************************************************************************
* MODULE     : vue_gui.hpp
* DESCRIPTION: Vue GUI
* COPYRIGHT  : (C) 2025  Masssimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef VUE_GUI_H
#define VUE_GUI_H

#include "gui.hpp"
#include "font.hpp"
#include "vue_widget.hpp"
#include "clay.h"

// drag state of a scroll bar
typedef struct {
  float clickOrigin;
  float positionOrigin;
  bool vertical; // which thumb the active element (button_logic) is
} ScrollbarData;

// Pending input events and interaction state of a window. The widgets read
// and update this state through the globals of vue_widget.cpp, which are
// loaded from (and stored back to) the window being laid out, see
// gui_init_context / gui_finalize_context.
struct vue_input_state {
  // keyboard events (window relative, consumed by the focused widget)
  string key_event;
  string last_key;
  time_t key_time;
  uint64_t key_stamp;      // SDL timestamp of the last key delivered as a key
                           // (a text event right after it is the same keystroke)
  // pointer events (coordinates are relative to the window)
  string mouse_action;
  time_t mouse_time;
  // signed: a drag may continue outside the window, where SDL reports
  // negative coordinates, and a pointer which left has none at all
  int    mouse_x, mouse_y;
  array<double> mouse_data;
  // kinetic scrolling (see wheel_inertia_step in vue_gui.cpp): the speed
  // of the wheel estimated from its events, the velocity of the glide after
  // they stop (wheel units per ms), the times of the last step and event
  double wheel_est_x, wheel_est_y;
  double wheel_vx, wheel_vy;
  time_t wheel_time;
  time_t wheel_event_time;
  bool   wheel_precise;    // the stream has fractional deltas (a trackpad)
  // popups and balloons
  bool current_popup;      // is there an active popup?
  bool cancel_popup;       // should we cancel popups?
  time_t away_time;        // tolerance for mouse motion
  uint32_t current_balloon;
  time_t balloon_time;
  // hot and active elements
  uint32_t hot_id;
  uint32_t active_id;
  int active_button;       // none, left, middle, right
  Clay_ElementId last_id;
  ScrollbarData scrollbar;

  vue_input_state ()
    : key_time (0), key_stamp (0), mouse_time (0), mouse_x (0), mouse_y (0),
      wheel_est_x (0), wheel_est_y (0), wheel_vx (0), wheel_vy (0),
      wheel_time (0), wheel_event_time (0), wheel_precise (false),
      current_popup (false), cancel_popup (false), away_time (0),
      current_balloon (0), balloon_time (0),
      hot_id (0), active_id (0), active_button (0), last_id {},
      scrollbar { 0, 0, true } {}
};

class vue_window_rep {
public:
  static int serial;
  int id;
  string name;
  
  string the_name;
  string mod_name;
  string orig_name;

  vue_widget content;
  vue_widget kbd_focus;
  hashset<string> refresh_kinds; // refresh cycle
  hashset<string> next_refresh_kinds; // refresh cycle

  //picture backing_store;
  //renderer ren;
  Clay_Context *clay_ctx;
  Clay_Arena clay_arena;
  Clay_RenderCommandArray render_commands;
  
  bool clay_debug;
  bool popup; // undecorated popup/tooltip window
  vue_input_state input; // pending events and interaction state
  float layout_w, layout_h; // size of the layout area (pixels)
  // windows are shown only once their size matches their contents, to avoid
  // flickering while a new window is sized (see set_visibility/process_layout)
  bool visible_requested; // set_visibility (true) has been called
  bool shown;             // the platform window is currently shown
  bool ready_to_show;     // the contents fit the window (set by post_layout)
  int  layout_passes;     // passes since creation (bounds the waiting)
  time_t last_layout_time; // for the frame time of the Clay transitions
  bool transitions_active; // Clay reported running transitions (keep drawing)
  
  vue_window_rep (vue_widget w, string _name, bool _popup= false)
  : id (serial++), name (_name), orig_name (_name), content (w),
    clay_debug (false), popup (_popup), layout_w (0), layout_h (0),
    visible_requested (false), shown (false), ready_to_show (false),
    layout_passes (0), last_layout_time (0), transitions_active (false)
  { render_commands.length= 0; }
  virtual ~vue_window_rep () {};
  
  virtual void *platform_window () = 0;
  
  virtual void   destroy_event () = 0;
  virtual void   set_name (string name) = 0;
  virtual string get_name () = 0;
  virtual void   set_modified (bool flag) = 0;
  virtual void   set_visibility (bool flag) = 0;
  virtual void   set_size (SI w, SI h) = 0;
  virtual void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) = 0;
  virtual void   get_size (SI& w, SI& h) = 0;
  virtual void   get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h) = 0;
  virtual void   set_position (SI x, SI y) = 0;
  virtual void   get_position (SI& x, SI& y) = 0;
  
  virtual void process_layout () = 0;
  virtual void process_redraw () = 0;
  virtual void draw_picture (void *data, picture pic) = 0;
  virtual void get_viewport_size (void *data, int& w, int& h) = 0;
};

typedef vue_window_rep* vue_window;

extern hashmap<int, pointer> id_to_window;
void draw_picture (void *data, picture pic);
void get_viewport_size (void *data, int& w, int& h);
vue_window plain_window (vue_widget wwid, string name, bool popup= false);

typedef void (*render_fn) (renderer ren, void *data, rectangle rect);

// data passed to vue_widget_rep::render when drawing with a TeXmacs renderer
struct vue_render_ren_data {
  renderer ren;
  rectangle r;
};

struct styled_string_rep : public concrete_struct {
  string s;
  color c;
  font fn;
  styled_string_rep (string _s, font _fn, color _c)
  : s (_s), c (_c), fn (_fn) {};
};

class styled_string {
public:
  ABSTRACT_NULL(styled_string);

  inline bool operator == (styled_string w) { return rep == w.rep; }
  inline bool operator != (styled_string w) { return rep != w.rep; }
};
ABSTRACT_NULL_CODE(styled_string);


extern void* vue_render_widget;
extern void* vue_render_text;

void layout_text (string s, int style, color c);

extern vue_window current_window;
class with_window {
public:
  vue_window saved_win;
  with_window (vue_window _win)
  : saved_win (current_window) { if (_win) Clay_SetCurrentContext (_win->clay_ctx); current_window= _win; }
  // the context is restored unconditionally: leaving it on a window whose
  // arena is freed later left Clay pointing into freed memory
  ~with_window () {
    Clay_SetCurrentContext (saved_win ? saved_win->clay_ctx : NULL);
    current_window= saved_win; }
};

#endif


