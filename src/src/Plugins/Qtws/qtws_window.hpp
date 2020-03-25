
/******************************************************************************
* MODULE     : qtws_window.hpp
* DESCRIPTION: QT WebSockets window class
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTWS_WINDOW_H
#define QTWS_WINDOW_H
#include "widget.hpp"
#include "window.hpp"
#include "Qt/qt_renderer.hpp"
#include "qtws_gui.hpp"
#include "rectangles.hpp"
#include "array.hpp"

/******************************************************************************
* The qtws_window class
******************************************************************************/

typedef int Window; // window id

class qtws_window_rep: public window_rep {
public:
  widget           w;
  qtws_gui         gui;
  string           orig_name;
  char*            name;
  string           the_name;
  string           mod_name;
  Window           win;
//  x_drawable_rep*  ren;

  rectangles    invalid_regions;
  SI            Min_w, Min_h;
  SI            Def_w, Def_h;
  SI            Max_w, Max_h;
  int           win_x, win_y;
  int           win_w, win_h;

  widget_rep*   kbd_focus;
  bool          has_focus;

  bool          full_screen_flag;
  Window        save_win;
  int           save_x, save_y;
  int           save_w, save_h;

public:

  /********************* specific routines for x_window **********************/


  qtws_window_rep (widget w, qtws_gui gui, char* name,
		SI min_w, SI min_h, SI def_w, SI def_h, SI max_w, SI max_h);
  ~qtws_window_rep ();
  widget get_widget ();
  void get_extents (int& w, int& h);

  void set_hints (int min_w, int min_h, int max_w, int max_h);
  void initialize ();

  void move_event (int x, int y);
  void resize_event (int w, int h);
  void destroy_event ();
  void invalidate_event (int x1, int y1, int x2, int y2);
  void key_event (string key);
  void mouse_event (string ev, int x, int y, time_t t);
  void focus_in_event ();
  void focus_out_event ();
  void repaint_invalid_regions ();

  /********************* routines from window.hpp **************************/

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

  friend class qtws_gui_rep;
  friend int get_identifier (window w);
};

typedef qtws_window_rep* qtws_window;
Window get_Window (widget w);
qtws_window get_qtws_window (widget w);
extern int alt_mask;
extern int meta_mask;

#endif // defined X_WINDOW_H
