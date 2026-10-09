
/******************************************************************************
* MODULE     : tm_frame.cpp
* DESCRIPTION: Routines for main TeXmacs frames
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_frame.hpp"
#include "tm_window.hpp"
#include "message.hpp"
#include "drd_std.hpp"
#include "tm_data.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

tm_frame_rep::tm_frame_rep ():
  full_screen (false), full_screen_edit (false), dialogue_win () {}
tm_frame_rep::~tm_frame_rep () {}

/******************************************************************************
* Subroutines
******************************************************************************/

string
icon_bar_name (int which) {
  if (which == 0) return "main";
  else if (which == 1) return "mode";
  else if (which == 2) return "focus";
  else return "user";
}

/******************************************************************************
* Properties of the current place
*******************************************************************************
* The core has no windows (tm_window.hpp): what was asked of the window of
* the current view is asked of the view itself (its size, where it
* scrolls), kept for its place (the zoom factor, properties) or said to the
* page (the bars, the tools and the footer, of which the page has one set,
* for the view which has the keyboard: Tau/tau_gui.cpp).
******************************************************************************/

void tau_chrome (int which, object menu);
void tau_set_visible (string part, bool flag);
bool tau_get_visible (string part);
void tau_footer (string side, string text);

// (made when first asked: the trees and the objects of Scheme are not
// there yet when the program starts)
static hashmap<tree,tree>&
place_properties_table () {
  static hashmap<tree,tree>* t= NULL;
  if (t == NULL) t= tm_new<hashmap<tree,tree> > (tree (UNINIT));
  return *t;
}
#define place_properties (place_properties_table ())

static tree
property_key (scheme_tree what) {
  return tuple (as_string (current_place ()), what);
}

int
tm_frame_rep::get_window_serial () {
  // (a number which tells the places apart, for the history of the cursor)
  return max (0, current_place ());
}

void
tm_frame_rep::set_window_property (scheme_tree what, scheme_tree val) {
  if (!has_current_window ()) return;
  place_properties (property_key (what))= val;
}

void
tm_frame_rep::set_bool_window_property (string what, bool val) {
  set_window_property (what, val? string ("true"): string ("false"));
}

void
tm_frame_rep::set_int_window_property (string what, int val) {
  set_window_property (what, as_tree (val));
}

void
tm_frame_rep::set_string_window_property (string what, string val) {
  set_window_property (what, val);
}

scheme_tree
tm_frame_rep::get_window_property (scheme_tree what) {
  if (!has_current_window ()) return scheme_tree ();
  return place_properties [property_key (what)];
}

bool
tm_frame_rep::get_bool_window_property (string what) {
  if (!has_current_window ()) return false;
  return as_bool (get_window_property (what));
}

int
tm_frame_rep::get_int_window_property (string what) {
  if (!has_current_window ()) return 0;
  return as_int (get_window_property (what));
}

string
tm_frame_rep::get_string_window_property (string what) {
  if (!has_current_window ()) return "";
  return as_string (get_window_property (what));
}

/******************************************************************************
* The menus, the icon bars and the tools
*******************************************************************************
* A part is described to the page when it changed, and all of them when
* another place takes the bars: which is -1 for the menu bar, 0 to 3 for
* the icon bars, 10 and more for the side tools, 20 and more for the bottom
* tools.
******************************************************************************/

void
tm_frame_rep::menu_widget (string menu, widget& w) {
  (void) menu;
  w= glue_widget ();
}

drd_info use_current_drd (); // Data/new_view.cpp

static int bars_owner= 0;
static hashmap<int,object>&
described_parts_table () {
  static hashmap<int,object>* t= NULL;
  if (t == NULL) t= tm_new<hashmap<int,object> > (null_object ());
  return *t;
}
#define described_parts (described_parts_table ())

void
forget_described_parts () {
  bars_owner= 0;
}

static void
describe_part (int which, string menu) {
  int place= current_place ();
  if (place <= 0) return;
  eval ("(lazy-initialize-force)");
  drd_info old_drd= use_current_drd ();
  object xmenu= call ("menu-expand", eval ("'" * menu));
  if (bars_owner != place) {
    bars_owner= place;
    described_parts= hashmap<int,object> (null_object ());
  }
  else if (described_parts->contains (which) &&
           described_parts [which] == xmenu) {
    the_drd= old_drd;
    return;
  }
  described_parts (which)= xmenu;
  tau_chrome (which, eval ("'" * menu));
  the_drd= old_drd;
}

void
tm_frame_rep::menu_main (string menu) {
  describe_part (-1, menu);
}

void
tm_frame_rep::menu_icons (int which, string menu) {
  if ((which<0) || (which>3)) return;
  describe_part (which, menu);
}

void
tm_frame_rep::side_tools (int which, string tools) {
  if ((which<0) || (which>1)) return;
  describe_part (10 + which, tools);
}

void
tm_frame_rep::bottom_tools (int which, string tools) {
  if ((which<0) || (which>1)) return;
  describe_part (20 + which, tools);
}

static string
icon_bar_part (int which) {
  return "icons-" * as_string (which);
}

void
tm_frame_rep::show_header (bool flag) {
  tau_set_visible ("menu", flag);
}

void
tm_frame_rep::show_icon_bar (int which, bool flag) {
  if ((which<0) || (which>3)) return;
  tau_set_visible (icon_bar_part (which), flag);
}

void
tm_frame_rep::show_side_tools (int which, bool flag) {
  if ((which<0) || (which>1)) return;
  tau_set_visible ("side-" * as_string (which), flag);
}

void
tm_frame_rep::show_bottom_tools (int which, bool flag) {
  if ((which<0) || (which>1)) return;
  tau_set_visible ("bottom-" * as_string (which), flag);
}

void
tm_frame_rep::show_footer (bool flag) {
  tau_set_visible ("footer", flag);
}

bool
tm_frame_rep::visible_header () {
  return tau_get_visible ("menu");
}

bool
tm_frame_rep::visible_icon_bar (int which) {
  if ((which<0) || (which>3)) return false;
  return tau_get_visible (icon_bar_part (which));
}

bool
tm_frame_rep::visible_side_tools (int which) {
  if ((which<0) || (which>1)) return false;
  return tau_get_visible ("side-" * as_string (which));
}

bool
tm_frame_rep::visible_bottom_tools (int which) {
  if ((which<0) || (which>1)) return false;
  return tau_get_visible ("bottom-" * as_string (which));
}

bool
tm_frame_rep::visible_footer () {
  return tau_get_visible ("footer");
}

/******************************************************************************
* The view: its zoom factor, its size and where it scrolls
******************************************************************************/

void
tm_frame_rep::set_window_zoom_factor (double zoom) {
  // the zoom factor is of the place: it stays when another buffer is
  // shown there
  if (!has_current_window ()) return;
  if (zoom >= 25.0 ) zoom= 25.0;
  if (zoom <=  0.04) zoom=  0.04;
  zoom= normal_zoom (zoom);
  set_place_zoom (current_place (), retina_zoom * zoom);
  ::set_zoom_factor (get_current_editor (), retina_zoom * zoom);
}

double
tm_frame_rep::get_window_zoom_factor () {
  if (!has_current_window ()) return 1;
  return get_place_zoom (current_place ()) / retina_zoom;
}

void
tm_frame_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_visible_part (get_current_editor (), x1, y1, x2, y2);
}

void
tm_frame_rep::set_scrollbars (int sb) {
  // (the page scrolls a view as it wants)
  (void) sb;
}

void
tm_frame_rep::scroll_where (SI& x, SI& y) {
  get_scroll_position (get_current_editor (), x, y);
}

void
tm_frame_rep::scroll_to (SI x, SI y) {
  set_scroll_position (get_current_editor (), x, y);
}

void
tm_frame_rep::get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  ::get_extents (get_current_editor (), x1, y1, x2, y2);
}

void
tm_frame_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  ::set_extents (get_current_editor (), x1, y1, x2, y2);
}

void
tm_frame_rep::set_left_footer (string s) {
  if (current_place () > 0) tau_footer ("left", s);
}

void
tm_frame_rep::set_right_footer (string s) {
  if (current_place () > 0) tau_footer ("right", s);
}

void
tm_frame_rep::set_message (tree left, tree right, bool temp) {
  if (!has_current_window ()) return;
  get_current_editor() -> set_message (left, right, temp);
}

void
tm_frame_rep::recall_message () {
  if (!has_current_window ()) return;
  get_current_editor() -> recall_message ();
}

void
tm_frame_rep::full_screen_mode (bool on, bool edit) {
  if (!has_current_window ()) return;
  if (on && !edit) {
    show_header (false);
    show_footer (false);
  }
  else {
    show_header (true);
    show_footer (true);
  }
  get_current_editor () -> full_screen_mode (on && !edit);
  full_screen = on;
  full_screen_edit = on && edit;
}

bool
tm_frame_rep::in_full_screen_mode () {
  return full_screen && !full_screen_edit;
}

bool
tm_frame_rep::in_full_screen_edit_mode () {
  return full_screen && full_screen_edit;
}
