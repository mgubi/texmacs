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

#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>

class vue_window_rep {
public:
  static int serial;
  
  int id;
  SDL_Window *sdl_win;
  SDL_Renderer *sdl_ren;
  TTF_TextEngine *text_engine;
  string name;
  
  string the_name;
  string mod_name;
  string orig_name;

  vue_widget content;
  vue_widget kbd_focus;

  picture backing_store;
  renderer ren;
  Clay_Context *clay_ctx;
  Clay_Arena clay_arena;
  Clay_RenderCommandArray render_commands;
  
  bool relayout;
  bool clay_debug;
  
  vue_window_rep (vue_widget w, string name);
  ~vue_window_rep ();
  void destroy_event ();
  
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
  void process_redraw ();
};

typedef vue_window_rep* vue_window;

extern hashmap<int, pointer> id_to_window;
void draw_picture (vue_render_data *data, picture pic);
void get_viewport_size (vue_render_data *data, int& w, int& h);
#endif


