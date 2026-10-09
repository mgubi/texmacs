
/******************************************************************************
* MODULE     : tau_widget.cpp
* DESCRIPTION: The place of a view, as the core of Tau sees it
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tau/tau_widget.hpp"

hashset<pointer> simple_widget_rep::all_widgets;

simple_widget_rep::simple_widget_rep ():
  place_w (800 * PIXEL), place_h (600 * PIXEL), scroll_x (0), scroll_y (0),
  zoom (1.0), has_focus (false),
  extents (0, 0, 0, 0), invalid (), invalid_all (true),
  cursor_x (0), cursor_y (0), mouse_grab (false), pointer_name ("")
{
  all_widgets->insert ((pointer) this);
}

simple_widget_rep::~simple_widget_rep () {
  all_widgets->remove ((pointer) this);
}

/******************************************************************************
* The messages of the editor to its place
******************************************************************************/

void
simple_widget_rep::send (slot s, blackbox val) {
  switch (s) {
  case SLOT_INVALIDATE:
    {
      coord4 r= open_box<coord4> (val);
      invalid= rectangles (rectangle (r.x1, r.x2, r.x3, r.x4), invalid);
    }
    break;
  case SLOT_INVALIDATE_ALL:
    invalid_all= true;
    break;
  case SLOT_EXTENTS:
    {
      coord4 r= open_box<coord4> (val);
      extents= rectangle (r.x1, r.x2, r.x3, r.x4);
    }
    break;
  case SLOT_SCROLL_POSITION:
    {
      coord2 p= open_box<coord2> (val);
      scroll_x= p.x1; scroll_y= p.x2;
    }
    break;
  case SLOT_ZOOM_FACTOR:
    zoom= open_box<double> (val);
    handle_set_zoom_factor (zoom);
    invalid_all= true;
    break;
  case SLOT_MOUSE_GRAB:
    mouse_grab= open_box<bool> (val);
    break;
  case SLOT_MOUSE_POINTER:
    {
      typedef pair<string,string> T;
      pointer_name= open_box<T> (val).x1;
    }
    break;
  case SLOT_CURSOR:
    {
      coord2 p= open_box<coord2> (val);
      cursor_x= p.x1; cursor_y= p.x2;
    }
    break;
  case SLOT_KEYBOARD_FOCUS:
    has_focus= open_box<bool> (val);
    break;
  default:
    // the rest concerns windows and widgets, which the core has not
    break;
  }
}

blackbox
simple_widget_rep::query (slot s, int type_id) {
  (void) type_id;
  switch (s) {
  case SLOT_IDENTIFIER:
    // "attached to a window" is a non zero identifier (is_attached)
    return close_box<int> (1);
  case SLOT_INVALID:
    return close_box<bool> (invalid_all || !is_nil (invalid));
  case SLOT_POSITION:
    return close_box<coord2> (coord2 (0, 0));
  case SLOT_SIZE:
    return close_box<coord2> (coord2 (place_w, place_h));
  case SLOT_SCROLL_POSITION:
    return close_box<coord2> (coord2 (scroll_x, scroll_y));
  case SLOT_EXTENTS:
    return close_box<coord4> (coord4 (extents->x1, extents->y1,
                                      extents->x2, extents->y2));
  case SLOT_VISIBLE_PART:
    return close_box<coord4> (coord4 (scroll_x, scroll_y - place_h,
                                      scroll_x + place_w, scroll_y));
  case SLOT_ZOOM_FACTOR:
    return close_box<double> (zoom);
  default:
    return blackbox ();
  }
}

widget
simple_widget_rep::read (slot s, blackbox index) {
  (void) index;
  switch (s) {
  case SLOT_WINDOW:
    // a view is its own window, as far as the editor asks
    return widget (this);
  default:
    return widget ();
  }
}

void
simple_widget_rep::write (slot s, blackbox index, widget w) {
  (void) s; (void) index; (void) w;
}

void
simple_widget_rep::notify (slot s, blackbox new_val) {
  (void) s; (void) new_val;
}

/******************************************************************************
* The protocol of the editor: defaults
******************************************************************************/

bool simple_widget_rep::is_editor_widget () { return false; }
bool simple_widget_rep::is_embedded_widget () { return false; }

void
simple_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  w= place_w; h= place_h;
}

void
simple_widget_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
}

void
simple_widget_rep::handle_keypress (string key, time_t t) {
  (void) key; (void) t;
}

void
simple_widget_rep::handle_keyboard_focus (bool has_focus, time_t t) {
  (void) has_focus; (void) t;
}

void
simple_widget_rep::handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                                 array<double> data) {
  (void) kind; (void) x; (void) y; (void) mods; (void) t; (void) data;
}

void
simple_widget_rep::handle_set_zoom_factor (double zoom) {
  (void) zoom;
}

void
simple_widget_rep::handle_clear (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
simple_widget_rep::handle_repaint (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}
