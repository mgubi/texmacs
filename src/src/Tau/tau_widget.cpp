
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
#include "MuPDF/mupdf_picture.hpp"
#include "iterator.hpp"

hashset<pointer> simple_widget_rep::all_widgets;
static int next_view_id= 1;

simple_widget_rep::simple_widget_rep ():
  id (next_view_id++),
  px_w (0), px_h (0), density (1.0), place_counter (0),
  place_w (800 * PIXEL), place_h (600 * PIXEL), scroll_x (0), scroll_y (0),
  absolute_scroll (false), zoom (1.0), has_focus (false),
  resize_pending (false), shown (false), backing (), ren (NULL),
  backing_x (0), backing_y (0),
  drawn_x1 (0), drawn_y1 (0), drawn_x2 (0), drawn_y2 (0),
  extents (0, 0, 0, 0), invalid (), invalid_all (true),
  cursor_x (0), cursor_y (0), mouse_grab (false), pointer_name ("")
{
  all_widgets->insert ((pointer) this);
}

simple_widget_rep::~simple_widget_rep () {
  all_widgets->remove ((pointer) this);
  if (ren != NULL) delete_renderer (ren);
}

simple_widget_rep*
simple_widget_rep::find (int id) {
  iterator<pointer> it= iterate (all_widgets);
  while (it->busy ()) {
    simple_widget_rep* w= (simple_widget_rep*) it->next ();
    if (w->id == id) return w;
  }
  return NULL;
}

/******************************************************************************
* The place of the view and its pixels
******************************************************************************/

static SI
grid_floor (SI v, SI unit) {
  SI r= v % unit;
  if (r < 0) r += unit;
  return v - r;
}

void
simple_widget_rep::set_place (int w, int h, double density2, int counter) {
  // the page tells the size of the canvas and the density of the screen;
  // the counter comes back with what is drawn for this place
  place_counter= counter;
  if (w < 1) w= 1;
  if (h < 1) h= 1;
  if (density2 <= 0.0) density2= 1.0;
  if (w == px_w && h == px_h && density2 == density && ren != NULL) {
    // the same place told again: the page may have dropped what was drawn
    // for the place before, so all of it is sent for this one
    invalid_all= true;
    return;
  }
  px_w= w; px_h= h; density= density2;
  // the density of the screen, as the core knows it (a whole number)
  set_retina_factor (max (1, (int) (density + 0.5)));
  backing= native_opaque_picture (px_w, px_h, 0, 0);
  if (ren != NULL) delete_renderer (ren);
  ren= picture_renderer (backing, std_shrinkf * retina_factor);
  place_w= px_w * ren->pixel;
  place_h= px_h * ren->pixel;
  invalid_all= true;
  resize_pending= true;
}

void
simple_widget_rep::scroll_by (int dx, int dy) {
  if (ren == NULL) return;
  scroll_x= backing_x + dx * ren->pixel;
  scroll_y= backing_y - dy * ren->pixel;
  absolute_scroll= false;
}

void
simple_widget_rep::to_document (SI& x, SI& y) {
  if (ren == NULL) { x= y= 0; return; }
  ren->set_origin (-backing_x, -backing_y);
  ren->encode (x, y);
}

void
simple_widget_rep::notify_resize () {
  // the editor typesets again for a new size (the width of the paper may
  // follow it): before its pending changes are applied
  if (!resize_pending) return;
  resize_pending= false;
  handle_notify_resize (place_w, place_h);
}

bool
simple_widget_rep::repaint () {
  if (ren == NULL) return false;
  // the scroll position: where it is asked, within the extents
  if (absolute_scroll) {
    scroll_x -= place_w / 2;
    scroll_y += place_h / 2;
    absolute_scroll= false;
  }
  if (scroll_x < extents->x1) scroll_x= extents->x1;
  else if (scroll_x + place_w > extents->x2)
    scroll_x= max (extents->x2 - place_w, extents->x1);
  if (scroll_y - place_h < extents->y1)
    scroll_y= min (extents->y1 + place_h, extents->y2);
  else if (scroll_y > extents->y2) scroll_y= extents->y2;
  scroll_x= grid_floor (scroll_x, ren->pixel);
  scroll_y= grid_floor (scroll_y, ren->pixel);
  if (scroll_x != backing_x || scroll_y != backing_y) {
    backing_x= scroll_x; backing_y= scroll_y;
    invalid_all= true;
  }
  if (!invalid_all && is_nil (invalid)) return false;

  // the canvas, in the coordinates of the document
  SI X1= 0, Y1= 0, X2= px_w, Y2= px_h;
  ren->set_origin (-backing_x, -backing_y);
  ren->encode (X1, Y1);
  ren->encode (X2, Y2);
  rectangle canvas (X1, Y2, X2, Y1);
  rectangles todo;
  if (invalid_all) todo= rectangles (canvas);
  else {
    SI pad= 2 * ren->pixel;
    todo= invalid & rectangles (rectangle (X1 - pad, Y2 - pad,
                                           X2 + pad, Y1 + pad));
  }
  invalid= rectangles ();
  invalid_all= false;
  if (is_nil (todo)) return false;
  rectangle lub= least_upper_bound (todo);
  // the pixels which change: all that is sent to the page
  drawn_x1= max (0, (int) ((lub->x1 - backing_x) / ren->pixel) - 2);
  drawn_x2= min (px_w, (int) ((lub->x2 - backing_x) / ren->pixel) + 3);
  drawn_y1= max (0, (int) ((backing_y - lub->y2) / ren->pixel) - 2);
  drawn_y2= min (px_h, (int) ((backing_y - lub->y1) / ren->pixel) + 3);
  if (drawn_x2 <= drawn_x1 || drawn_y2 <= drawn_y1) {
    drawn_x1= 0; drawn_y1= 0; drawn_x2= px_w; drawn_y2= px_h; }
  if (area (lub) < 1.2 * area (todo)) todo= rectangles (lub);
  while (!is_nil (todo)) {
    rectangle r= thicken (copy (todo->item), 1, 1);
    ren->set_origin (-backing_x, -backing_y);
    ren->set_clipping (r->x1, r->y1, r->x2, r->y2);
    handle_repaint (ren, r->x1, r->y1, r->x2, r->y2);
    ren->set_clipping (r->x1, r->y1, r->x2, r->y2, true);
    todo= todo->next;
  }
  return true;
}

unsigned char*
simple_widget_rep::pixels () {
  if (is_nil (backing)) return NULL;
  mupdf_picture_rep* p= (mupdf_picture_rep*) backing->get_handle ();
  return fz_pixmap_samples (mupdf_context (), p->pix);
}

void
simple_widget_rep::extents_in_pixels (int& w, int& h, int& sx, int& sy) {
  SI px= (ren == NULL? PIXEL: ren->pixel);
  w = (int) ((extents->x2 - extents->x1) / px);
  h = (int) ((extents->y2 - extents->y1) / px);
  sx= (int) ((backing_x - extents->x1) / px);
  sy= (int) ((extents->y2 - backing_y) / px);
}

void
simple_widget_rep::cursor_in_pixels (int& x, int& y) {
  SI px= (ren == NULL? PIXEL: ren->pixel);
  x= (int) ((cursor_x - backing_x) / px);
  y= (int) ((backing_y - cursor_y) / px);
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
      // the editor asks for a point at the centre of the canvas
      coord2 p= open_box<coord2> (val);
      scroll_x= p.x1; scroll_y= p.x2;
      absolute_scroll= true;
      if (getenv ("TAU_DEBUG_SCROLL") != NULL)
        cout << "TAUDBG scroll_to " << id << " " << scroll_x/256 << "," << scroll_y/256
             << " extents " << extents->y1/256 << ".." << extents->y2/256
             << " place " << place_w/256 << "x" << place_h/256 << LF;
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
    // "attached to a window" is a non zero identifier (is_attached): an
    // editor which is shown at a place
    return close_box<int> (is_editor_widget () && !shown? 0: 1);
  case SLOT_INVALID:
    return close_box<bool> (invalid_all || !is_nil (invalid));
  case SLOT_POSITION:
    return close_box<coord2> (coord2 (0, 0));
  case SLOT_SIZE:
    return close_box<coord2> (coord2 (place_w, place_h));
  case SLOT_SCROLL_POSITION:
    // as it is set: the point at the centre of the canvas. (Told as the
    // top left corner, each position read and then set moved the view by
    // half its size, down and to the right, to the end of a long document)
    if (absolute_scroll) return close_box<coord2> (coord2 (scroll_x, scroll_y));
    return close_box<coord2> (coord2 (scroll_x + place_w / 2,
                                      scroll_y - place_h / 2));
  case SLOT_EXTENTS:
    return close_box<coord4> (coord4 (extents->x1, extents->y1,
                                      extents->x2, extents->y2));
  case SLOT_VISIBLE_PART:
    return close_box<coord4> (coord4 (backing_x, backing_y - place_h,
                                      backing_x + place_w, backing_y));
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
  case SLOT_CANVAS:
  case SLOT_SCROLLABLE:
    // a view is its own window and its own canvas, as far as the editor
    // asks
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
