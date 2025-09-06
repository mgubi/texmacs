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
#include "vue_widget.hpp"
#include "clay.h"

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

  picture backing_store;
  renderer ren;
  Clay_Context *clay_ctx;
  Clay_Arena clay_arena;
  Clay_RenderCommandArray render_commands;
  
  bool relayout;
  bool clay_debug;
  
  vue_window_rep (vue_widget w, string _name)
  : content (w), name (_name), id (serial++), orig_name (_name) {}
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
vue_window plain_window (vue_widget wwid, string name);

#endif


