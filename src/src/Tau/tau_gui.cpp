
/******************************************************************************
* MODULE     : tau_gui.cpp
* DESCRIPTION: What the core of Tau has in the place of a GUI
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* Tau has no GUI in the core (docs/tau-design.md). This file answers what
* the core still asks of one:
* - the services of gui.hpp: the loop, the clipboard, the events;
* - the window of a view (texmacs_widget), which only remembers its view;
* - the constructors of widgets (widget.hpp), which make nothing. They are
*   here until their callers are gone: kernel/gui/menu-widget.scm, replaced
*   by the serialiser of the interface, and Texmacs/Window.
******************************************************************************/

#include "Tau/tau_widget.hpp"
#include "gui.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "promise.hpp"
#include "command.hpp"
#include "url.hpp"
#include "hashmap.hpp"
#include "scheme.hpp"
#include <unistd.h>
#include <stdlib.h>

/******************************************************************************
* Globals which the core reads
******************************************************************************/

int  nr_windows= 0;             // the windows of the old organisation
bool char_clip= true;
hashmap<int,tree> payloads;     // what was dropped on a view, by ticket

/******************************************************************************
* A widget which is nothing, and the window of a view
******************************************************************************/

class no_widget_rep: public widget_rep {
public:
  no_widget_rep () {}
  void send (slot s, blackbox val) { (void) s; (void) val; }
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w) {
    (void) s; (void) index; (void) w; }
  void notify (slot s, blackbox new_val) { (void) s; (void) new_val; }
};

static widget
no_widget () {
  return widget (tm_new<no_widget_rep> ());
}

blackbox
no_widget_rep::query (slot s, int type_id) {
  // an answer of the type which is asked, with nothing in it
  (void) s;
  if (type_id == type_helper<bool>::id) return close_box<bool> (false);
  if (type_id == type_helper<int>::id) return close_box<int> (0);
  if (type_id == type_helper<double>::id) return close_box<double> (1.0);
  if (type_id == type_helper<string>::id) return close_box<string> ("#f");
  if (type_id == type_helper<coord2>::id)
    return close_box<coord2> (coord2 (0, 0));
  if (type_id == type_helper<coord4>::id)
    return close_box<coord4> (coord4 (0, 0, 0, 0));
  return blackbox ();
}

widget
no_widget_rep::read (slot s, blackbox index) {
  (void) s; (void) index;
  return no_widget ();
}

// The window of a view: the core still makes a window for each view
// (Texmacs/Data/new_window.cpp), attaches the view to it and asks it for
// the canvas, of which the editor wants the size and the scroll position.
// The canvas is the view itself (tau_widget.hpp).

class view_window_rep: public no_widget_rep {
  widget  view;
  command quit;
public:
  view_window_rep (command quit2): quit (quit2) {}
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
};

blackbox
view_window_rep::query (slot s, int type_id) {
  // "attached to a window" is a non zero identifier (is_attached)
  if (s == SLOT_IDENTIFIER) return close_box<int> (1);
  return no_widget_rep::query (s, type_id);
}

widget
view_window_rep::read (slot s, blackbox index) {
  switch (s) {
  case SLOT_CANVAS:
  case SLOT_SCROLLABLE:
    if (!is_nil (view)) return view;
    return no_widget ();
  case SLOT_WINDOW:
    return widget (this);
  default:
    return no_widget_rep::read (s, index);
  }
}

void
view_window_rep::write (slot s, blackbox index, widget w) {
  (void) index;
  if (s == SLOT_SCROLLABLE || s == SLOT_CANVAS) view= w;
}

widget
texmacs_widget (int mask, command quit) {
  (void) mask;
  return widget (tm_new<view_window_rep> (quit));
}

void
destroy_window_widget (widget w) {
  (void) w;
}

/******************************************************************************
* The loop
******************************************************************************/

static void (*the_interpose_handler) (void) = NULL;

void
gui_open (int& argc, char** argv) {
  (void) argc; (void) argv;
}

void
gui_close () {
}

void
gui_interpose (void (*r) (void)) {
  the_interpose_handler= r;
}

void
gui_start_loop () {
  // without a page there are no events: the delayed commands are run until
  // one of them quits (tm_server_rep::quit leaves the program)
  while (true) {
    if (the_interpose_handler != NULL) the_interpose_handler ();
    usleep (1000);
  }
}

void
gui_root_extents (SI& width, SI& height) {
  width = 1920 * PIXEL;
  height= 1080 * PIXEL;
}

void
gui_refresh () {
}

string
gui_version () {
  return "tau";
}

void
needs_update () {
}

bool
check_event (int type) {
  // the core does not see the events of the page while it computes
  (void) type;
  return false;
}

void
beep () {
}

void
set_default_font (string name) {
  (void) name;
}

void
show_help_balloon (widget balloon, SI x, SI y) {
  (void) balloon; (void) x; (void) y;
}

void
show_wait_indicator (widget base, string message, string argument) {
  (void) base; (void) message; (void) argument;
}

/******************************************************************************
* The clipboard: kept here, until the page has it
******************************************************************************/

static hashmap<string,tree>   selection_trees ("none");
static hashmap<string,string> selection_strings ("");

bool
set_selection (string cb, tree t,
               string s, string sv, string sh, string format) {
  (void) sv; (void) sh; (void) format;
  selection_trees (cb)= copy (t);
  selection_strings (cb)= s;
  return true;
}

bool
get_selection (string cb, tree& t, string& s, string format) {
  (void) format;
  if (!selection_trees->contains (cb)) return false;
  t= copy (selection_trees [cb]);
  s= selection_strings [cb];
  return true;
}

void
clear_selection (string cb) {
  selection_trees->reset (cb);
  selection_strings->reset (cb);
}

/******************************************************************************
* The constructors of widgets: nothing is made
******************************************************************************/

widget aligned_widget (array<widget> a1, array<widget> a2, int a3, int a4, int a5, int a6) {
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5; (void) a6;
  return no_widget (); }

widget balloon_widget (widget a1, widget a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget choice_widget (command a1, array<string> a2, array<string> a3, int a4) {
  (void) a1; (void) a2; (void) a3; (void) a4;
  return no_widget (); }

widget choice_widget (command a1, array<string> a2, string a3, int a4) {
  (void) a1; (void) a2; (void) a3; (void) a4;
  return no_widget (); }

widget choice_widget (command a1, array<string> a2, string a3, string a4) {
  (void) a1; (void) a2; (void) a3; (void) a4;
  return no_widget (); }

widget color_picker_widget (command a1, bool a2, array<tree> a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget division_widget (string a1, widget a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget empty_widget () { return no_widget (); }

widget enum_widget (command a1, array<string> a2, string a3, int a4, string a5) {
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5;
  return no_widget (); }

widget extend_widget (widget a1, array<widget> a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget file_chooser_widget (command a1, string a2, string a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget glue_widget (bool a1, bool a2, int a3, int a4) {
  (void) a1; (void) a2; (void) a3; (void) a4;
  return no_widget (); }

widget glue_widget (tree a1, bool a2, bool a3, int a4, int a5) {
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5;
  return no_widget (); }

widget horizontal_list (array<widget> a1) {
  (void) a1;
  return no_widget (); }

widget horizontal_menu (array<widget> a1) {
  (void) a1;
  return no_widget (); }

widget hsplit_widget (widget a1, widget a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget icon_tabs_widget (array<url> a1, array<widget> a2, array<widget> a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget ink_widget (command a1) {
  (void) a1;
  return no_widget (); }

widget input_text_widget (command a1, string a2, array<string> a3, int a4, string a5) {
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5;
  return no_widget (); }

widget inputs_list_widget (command a1, array<string> a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget menu_button (widget a1, command a2, string a3, string a4, int a5) {
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5;
  return no_widget (); }

widget menu_group (string a1, int a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget menu_separator (bool a1) {
  (void) a1;
  return no_widget (); }

widget minibar_menu (array<widget> a1) {
  (void) a1;
  return no_widget (); }

widget plain_window_widget (widget a1, string a2, command a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget popup_widget (widget a1) {
  (void) a1;
  return no_widget (); }

widget popup_window_widget (widget a1, string a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget printer_widget (command a1, url a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget pulldown_button (widget a1, promise<widget> a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget pullright_button (widget a1, promise<widget> a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget refresh_widget (string a1, string a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget refreshable_widget (object a1, string a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget resize_widget (widget a1, int a2, string a3, string a4, string a5, string a6, string a7, string a8, string a9, string a10) {
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5; (void) a6; (void) a7; (void) a8; (void) a9; (void) a10;
  return no_widget (); }

widget responsive_icon_tabs_widget (array<url> a1, array<widget> a2, array<widget> a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget responsive_tabs_widget (array<widget> a1, array<widget> a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget setting_enum_widget (command a1, string a2, array<string> a3, string a4, int a5, string a6) {
  (void) a1; (void) a2; (void) a3; (void) a4; (void) a5; (void) a6;
  return no_widget (); }

widget setting_group_widget (string a1, array<widget> a2, int a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget setting_toggle_widget (command a1, string a2, bool a3, int a4) {
  (void) a1; (void) a2; (void) a3; (void) a4;
  return no_widget (); }

widget tabs_widget (array<widget> a1, array<widget> a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget text_widget (string a1, int a2, unsigned int a3, bool a4) {
  (void) a1; (void) a2; (void) a3; (void) a4;
  return no_widget (); }

widget tile_menu (array<widget> a1, int a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget toggle_widget (command a1, bool a2, int a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget tooltip_window_widget (widget a1, string a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget tree_view_widget (command a1, tree a2, tree a3) {
  (void) a1; (void) a2; (void) a3;
  return no_widget (); }

widget user_canvas_widget (widget a1, int a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget vertical_list (array<widget> a1) {
  (void) a1;
  return no_widget (); }

widget vertical_menu (array<widget> a1) {
  (void) a1;
  return no_widget (); }

widget vsplit_widget (widget a1, widget a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget wrapped_widget (widget a1, command a2) {
  (void) a1; (void) a2;
  return no_widget (); }

widget xpm_widget (url a1) {
  (void) a1;
  return no_widget (); }
