
/******************************************************************************
* MODULE     : sdl_gui.hpp
* DESCRIPTION: Graphical user interface for SDL
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SDL_GUI_H
#define SDL_GUI_H
#include "tm_timer.hpp"
#include "gui.hpp"
#include "widget.hpp"
#include "array.hpp"
#include "hashmap.hpp"
#include "colors.hpp"

#include <SDL3/SDL.h>

class sdl_gui_rep;
class sdl_drawable_rep;
class sdl_window_rep;
typedef sdl_gui_rep* sdl_gui;
typedef sdl_window_rep* sdl_window;
extern sdl_gui the_gui;


/******************************************************************************
* Delayed messages
******************************************************************************/

struct message_rep: concrete_struct {
  widget wid;
  string s;
  time_t t;
  message_rep (widget wid, string s, time_t t);
  friend class message;
};

class message {
  CONCRETE(message);
  message (widget wid, string s, time_t t);
};
CONCRETE_CODE(message);

tm_ostream& operator << (tm_ostream& out, message m);

/******************************************************************************
* The sdl_gui class
******************************************************************************/

class sdl_gui_rep {
public:

  int screen_width, screen_height;

  unsigned int    mouse_state;  // buttons and modifiers, as TeXmacs wants them
  SDL_MouseButtonFlags buttons; // the buttons which are down (from the events)
  list<widget>    grab_ptr;
  list<widget>    grab_kbd;
  list<message>   messages;
  widget          balloon_wid;
  window          balloon_win;
  SI              balloon_x;
  SI              balloon_y;
  time_t          balloon_time;
  bool            interrupted;
  time_t          interrupt_time;
  bool            update_requested; // needs_update: do not sleep

  // the wheel: the deltas accumulate into steps of the editor (see
  // SDL_EVENT_MOUSE_WHEEL in sdl_gui.cpp)
  double          wheel_acc;
  bool            wheel_precise;
  Uint64          wheel_stamp;

  // the keystroke which was delivered as a key: its text event is ignored
  Uint64          key_stamp;

  list<SDL_Window*>            windows_l;

public:
  sdl_gui_rep (int& argc, char** argv);
  ~sdl_gui_rep ();

  void update_mouse_state (); // from the buttons and the modifiers

  /********************* extents, grabbing, selections ***********************/
  void   get_extents (SI& width, SI& height);
  void   get_max_size (SI& width, SI& height);
  void   emulate_leave_enter (widget old_widget, widget new_widget);
  void   obtain_mouse_grab (widget wid);
  void   release_mouse_grab ();
  bool   has_mouse_grab (widget w);

  /*********************** interclient communication *************************/
  void   created_window (SDL_Window* win);
  void   deleted_window (SDL_Window* win);
  void   focussed_window (SDL_Window* win);

  /**************************** miscellaneous ********************************/
  void   show_help_balloon (widget wid, SI x, SI y);
  void   map_balloon ();
  void   unmap_balloon ();
  void   set_mouse_pointer (widget w, string name);
  void   set_mouse_pointer (widget w, string curs_name, string mask_name);
  void   show_wait_indicator (widget w, string message, string arg);
  void   external_event (string s, time_t t);
  bool   check_event (int type);
  void   set_default_font (string name);
  font   default_font_sub (bool tt, bool mini, bool bold);
  font   default_font (bool tt, bool mini, bool bold);

  /************************** Event processing *******************************/
  void process_event (SDL_Event* event);
  void wheel_event (sdl_window win, SDL_MouseWheelEvent* ev);
  void process_messages ();
  int  next_message_delay (int delay);
  void repaint_windows ();
  void event_loop ();
};

#endif // defined SDL_GUI_H
