
/******************************************************************************
* MODULE     : sdl_window.hpp
* DESCRIPTION: Windows under SDL
* COPYRIGHT  : (C) 2022  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SDL_WINDOW_H
#define SDL_WINDOW_H
#include "window.hpp"
#include "rectangles.hpp"
#include "array.hpp"

#include "widget.hpp"
#include "sdl_gui.hpp"

#include <SDL3/SDL.h>

/******************************************************************************
* The sdl_window class
*
* The widgets draw with a MuPDF renderer into the backing store, an opaque
* pixmap of the size of the window in device pixels; what changed is copied
* to the surface of the window when the repaint is done (present). The
* rectangles below are in device pixels, with y going down.
******************************************************************************/

class sdl_window_rep: public window_rep {
public:
  int              id;
  widget           w;
  sdl_gui          gui;
  string           orig_name;
  bool             popup;
  string           the_name;
  string           mod_name;

  SDL_Window*   sdl_win;

  picture       backing_store;
  renderer      ren;
  float         density;         // device pixels per point

  rectangles    invalid_regions; // to be repainted by the widgets
  rectangles    dirty;           // repainted, not yet on the screen
  SI            Min_w, Min_h;
  SI            Def_w, Def_h;
  SI            Max_w, Max_h;
  int           win_x, win_y;    // points
  int           win_w, win_h;    // points

  widget_rep*   kbd_focus;
  bool          has_focus;

  bool          full_screen_flag;

public:

  /******************** specific routines for sdl_window *********************/

  sdl_window_rep (widget w, sdl_gui gui, string name, bool popup,
                  SI min_w, SI min_h, SI def_w, SI def_h, SI max_w, SI max_h);
  ~sdl_window_rep ();
  widget get_widget ();

  void initialize ();

  void move_event (int x, int y);
  void resize_event (int w, int h);
  void destroy_event ();
  void invalidate_event (int x1, int y1, int x2, int y2);
  void key_event (string key);
  void mouse_event (string ev, float x, float y, time_t t);
  void focus_in_event ();
  void focus_out_event ();
  void repaint_invalid_regions ();

  void invalidate_all ();
  void expose ();                // the whole backing store to the screen
  void present ();               // the dirty rectangles to the screen
  bool sync_backing_store ();    // follow the size of the window
  void shift_pixels (int x1, int y1, int x2, int y2, int dx, int dy);
  void pointer_position (float x, float y, SI& px, SI& py);
  SI   to_si (int pixels);

  /********************* routines from window.hpp ****************************/

  void   set_name (string name);
  string get_name ();
  void   set_modified (bool flag);
  void   set_visibility (bool flag);
  void   set_full_screen (bool flag);
  void   set_size (SI w, SI h);
  void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h);
  void   get_size (SI& w, SI& h);
  void   get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h);
  void   set_position (SI x, SI y);
  void   get_position (SI& x, SI& y);
  void   set_keyboard_focus (widget wid, bool get_focus);
  bool   get_keyboard_focus (widget wid);
  void   set_mouse_grab (widget wid, bool get_grab);
  bool   get_mouse_grab (widget w);
  void   set_mouse_pointer (widget wid, string name, string mask);
  void   delayed_message (widget wid, string s, time_t delay);
  void   invalidate (SI x1, SI y1, SI x2, SI y2);
  bool   is_invalid ();
  void   translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy);

  /****************************** friends ************************************/

  friend class sdl_gui_rep;
  friend int get_identifier (window w);
};

typedef sdl_window_rep* sdl_window;
SDL_Window* get_Window (widget w);
sdl_window get_sdl_window (widget w);
sdl_window get_window_from_ID (Uint32 ID);

#endif // defined SDL_WINDOW_H
