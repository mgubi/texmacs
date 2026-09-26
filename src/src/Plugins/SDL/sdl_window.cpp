
/******************************************************************************
* MODULE     : sdl_window.cpp
* DESCRIPTION: Windows under SDL
* COPYRIGHT  : (C) 2022  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "sdl_window.hpp"

#include "message.hpp"
#include "boot.hpp"

// the SDL port draws with MuPDF: the backing store of a window is a MuPDF
// pixmap, drawn by mupdf_renderer
#if !MUPDF_RENDERER
#error "the SDL GUI needs MuPDF (MUPDF_RENDERER)"
#endif
#include "../MuPDF/mupdf_picture.hpp"

int nr_windows;

hashmap<SDL_Window*,pointer> Window_to_window (NULL);
hashmap<int, window> id_to_window (0);

/******************************************************************************
* Creation and deletion of an sdl_window
******************************************************************************/

static int serial= 1; // serial identifier for windows

void
sdl_window_rep::initialize () {
  SI min_w= Min_w / PIXEL, min_h= Min_h / PIXEL;
  SI max_w= Max_w / PIXEL, max_h= Max_h / PIXEL;

  full_screen_flag= false;

  if (win_x + win_w > gui->screen_width) win_x= gui->screen_width - win_w;
  if (win_x < 0) win_x= 0;
  if (win_y + win_h > gui->screen_height) win_y= gui->screen_height - win_h;
  if (win_y < 0) win_y= 0;

  // windows start hidden: TeXmacs shows them once they are set up
  // (set_visibility). Popups (menus, balloons) are undecorated, stay on top
  // and never take the keyboard focus from the window which opened them.
  SDL_WindowFlags flags= SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_HIDDEN;
  if (popup)
    flags |= SDL_WINDOW_BORDERLESS | SDL_WINDOW_ALWAYS_ON_TOP |
             SDL_WINDOW_NOT_FOCUSABLE;
  else flags |= SDL_WINDOW_RESIZABLE;

  c_string title (cork_to_utf8 (orig_name));
  sdl_win= SDL_CreateWindow (title, max (win_w, 1), max (win_h, 1), flags);
  if (sdl_win == NULL) {
    SDL_LogError (SDL_LOG_CATEGORY_APPLICATION,
                  "Couldn't create window: %s", SDL_GetError ());
    FAILED ("SDL: cannot create a window");
  }
  the_name= orig_name;
  mod_name= orig_name;

  SDL_SetWindowPosition (sdl_win, win_x, win_y);
  if (max_w > 0 && max_h > 0) SDL_SetWindowMaximumSize (sdl_win, max_w, max_h);
  SDL_SetWindowMinimumSize (sdl_win, min_w, min_h);
  if (!popup) SDL_StartTextInput (sdl_win); // text and input method events

  int pw= 1, ph= 1;
  SDL_GetWindowSizeInPixels (sdl_win, &pw, &ph);
  density= (win_w > 0) ? ((float) pw) / win_w : 1.0f;
  backing_store= native_opaque_picture (max (pw, 1), max (ph, 1), 0, 0);
  ren= picture_renderer (backing_store, std_shrinkf * retina_factor);

  nr_windows++;
  Window_to_window (sdl_win)= (void*) this;
  id= serial++;
  id_to_window (id)= this;

  // update widget state
  set_identifier (w, id);
  notify_position (w, 0, 0);
  notify_size (w, to_si (pw), to_si (ph));

  gui->created_window (sdl_win);
  if (DEBUG_EVENTS) debug_events << "create window " << id << LF;
}

sdl_window_rep::sdl_window_rep (widget w2, sdl_gui gui2, string n2, bool popup2,
                                SI min_w, SI min_h, SI def_w, SI def_h,
                                SI max_w, SI max_h):
  window_rep (), w (w2), gui (gui2), orig_name (n2), popup (popup2),
  sdl_win (NULL), ren (NULL), density (1.0f),
  Min_w (min_w), Min_h (min_h), Def_w (def_w), Def_h (def_h),
  Max_w (max_w), Max_h (max_h),
  win_x (0), win_y (0), win_w (def_w/PIXEL), win_h (def_h/PIXEL),
  kbd_focus (w.rep), has_focus (false), full_screen_flag (false)
{
  initialize ();
}

sdl_window_rep::~sdl_window_rep () {
  if (DEBUG_EVENTS) debug_events << "destroy window " << id << LF;
  id_to_window->reset (id);
  id= 0;
  set_identifier (w, 0); // FIXME: is this ok?
  Window_to_window->reset (sdl_win);
  nr_windows--;
  gui->deleted_window (sdl_win);
  if (!popup) SDL_StopTextInput (sdl_win);
  SDL_DestroyWindow (sdl_win);
  delete_renderer (ren);
}

widget
sdl_window_rep::get_widget () {
  return w;
}

SDL_Window*
get_Window (widget w) {
  int id= get_identifier (w);
  if (id == 0) {
    failed_error << "widget = " << w << "\n";
    FAILED ("widget is not attached to a window");
  }
  sdl_window w2= (sdl_window)id_to_window [id];
  return w2->sdl_win;
}

sdl_window
get_sdl_window (widget w) {
  int id= get_identifier (w);
  if (id == 0) return NULL;
  return (sdl_window)id_to_window[id];
}

sdl_window
get_window_from_ID (Uint32 ID) {
  SDL_Window *w= SDL_GetWindowFromID (ID);
  if (w == NULL) return NULL;
  return (sdl_window) Window_to_window [w];
}

int
get_identifier (window w) {
  if (w == NULL) return 0;
  else return (((sdl_window) w) -> id);
}

window
get_window (int id) {
  if (id == 0) return NULL;
  else return id_to_window [id];
}

// a length in device pixels in TeXmacs units: the widgets see the window
// at the scale of its renderer, whatever the density of its display
SI
sdl_window_rep::to_si (int pixels) {
  return ((SI) pixels) * ren->pixel;
}

/******************************************************************************
* Window appearance
******************************************************************************/

void
sdl_window_rep::get_position (SI& x, SI& y) {
  int xx, yy;
  SDL_GetWindowPosition (sdl_win, &xx, &yy);
  x=  xx*PIXEL;
  y= -yy*PIXEL;
}

void
sdl_window_rep::get_size (SI& ww, SI& hh) {
  ww= to_si (backing_store->get_width ());
  hh= to_si (backing_store->get_height ());
}

void
sdl_window_rep::get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h) {
  min_w= Min_w; min_h= Min_h; max_w= Max_w; max_h= Max_h;
}

void
sdl_window_rep::set_position (SI x, SI y) {
  x= x/PIXEL;
  y= -y/PIXEL;
  if ((x+ win_w) > gui->screen_width) x= gui->screen_width- win_w;
  if (x<0) x=0;
  if ((y+ win_h) > gui->screen_height) y= gui->screen_height- win_h;
  if (y<0) y=0;
  win_x= x;
  win_y= y;
  SDL_SetWindowPosition (sdl_win, win_x, win_y);
}

void
sdl_window_rep::set_size (SI w, SI h) {
  w= w/PIXEL; h= h/PIXEL;
  SDL_SetWindowSize (sdl_win, w, h);
}

void
sdl_window_rep::set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) {
  if (min_w == Min_w && min_h == Min_h && max_w == Max_w && max_h == Max_h)
    return;
  Min_w= min_w; Min_h= min_h; Max_w= max_w; Max_h= max_h;
  min_w= min_w/PIXEL; min_h= min_h/PIXEL;
  max_w= max_w/PIXEL; max_h= max_h/PIXEL;
  if (max_w > 0 && max_h > 0) SDL_SetWindowMaximumSize (sdl_win, max_w, max_h);
  SDL_SetWindowMinimumSize (sdl_win, min_w, min_h);
}

void
sdl_window_rep::set_name (string name) {
  if (the_name != name) {
    c_string s (cork_to_utf8 (name));
    SDL_SetWindowTitle (sdl_win, s);
    the_name= name;
    mod_name= name;
  }
}

string
sdl_window_rep::get_name () {
  return the_name;
}

void
sdl_window_rep::set_modified (bool flag) {
  string name= (flag? (the_name * " *"): the_name);
  if (mod_name != name) {
    c_string s (cork_to_utf8 (name));
    SDL_SetWindowTitle (sdl_win, s);
    mod_name= name;
  }
}

void
sdl_window_rep::set_visibility (bool flag) {
  if (flag) {
    SDL_ShowWindow (sdl_win);
    // a popup is shown where the pointer is: bring it above the window
    // which opened it, without taking the focus
    if (popup) SDL_RaiseWindow (sdl_win);
    // as after an Expose event of X11: the widgets paint the whole window
    invalidate_all ();
  }
  else SDL_HideWindow (sdl_win);
}

void
sdl_window_rep::set_full_screen (bool flag) {
  if (full_screen_flag == flag) return;
  // the system resizes the window, which is followed by the usual resize
  // events (resize_event)
  SDL_SetWindowFullscreen (sdl_win, flag);
  full_screen_flag= flag;
}

void
sdl_window_rep::move_event (int x, int y) {
  bool flag= (win_x!=x) || (win_y!=y);
  win_x= x; win_y= y;
  if (flag) {
    notify_position (w, win_x*PIXEL, win_y*PIXEL);
    notify_window_move (orig_name, x*PIXEL, -y*PIXEL);
  }
}

void
sdl_window_rep::resize_event (int ww, int hh) {
  bool flag= (win_w!=ww) || (win_h!=hh);
  win_w= ww; win_h= hh;
  if (flag) {
    // the backing store follows the window at once, so that the widgets
    // are laid out for the size which is on the screen
    sync_backing_store ();
    notify_size (w, to_si (backing_store->get_width ()),
                    to_si (backing_store->get_height ()));
    // the widgets were laid out again: they paint the whole window (X11
    // sends an Expose event after a resize, on which Widkit relies)
    invalidate_all ();
    notify_window_resize (orig_name, ww*PIXEL, hh*PIXEL);
  }
}

void
sdl_window_rep::destroy_event () {
  notify_window_destroy (orig_name);
  send_destroy (w);
}

/******************************************************************************
* Event handling
******************************************************************************/

void
sdl_window_rep::invalidate_event (int x1, int y1, int x2, int y2) {
  invalid_regions= invalid_regions | rectangles (rectangle (x1, y1, x2, y2));
}

void
sdl_window_rep::key_event (string key) {
  send_keyboard (kbd_focus, key);
}

void
sdl_window_rep::focus_in_event () {
  has_focus= true;
  notify_keyboard_focus (kbd_focus, true);
  gui->focussed_window (sdl_win);
}

void
sdl_window_rep::focus_out_event () {
  has_focus= false;
  notify_keyboard_focus (kbd_focus, false);
}

// a position of the pointer (points, relative to the window) in the
// coordinates of the renderer
void
sdl_window_rep::pointer_position (float x, float y, SI& px, SI& py) {
  px= (SI) (x * density);
  py= (SI) (y * density);
  ren->set_origin (0, 0);
  ren->encode (px, py);
}

void
sdl_window_rep::mouse_event (string ev, float x, float y, time_t t) {
  sdl_window target= this;
  widget     wid   = w;
  if (!is_nil (gui->grab_ptr) && get_sdl_window (gui->grab_ptr->item) != NULL) {
    // the events go to the widget which grabbed the pointer, in its
    // coordinates (a menu gets the events of the window it was opened from)
    target= get_sdl_window (gui->grab_ptr->item);
    wid   = gui->grab_ptr->item;
    if (target != this) {
      int gw_x, gw_y, w_x, w_y;
      SDL_GetWindowPosition (target->sdl_win, &gw_x, &gw_y);
      SDL_GetWindowPosition (sdl_win, &w_x, &w_y);
      x += (float) (w_x - gw_x);
      y += (float) (w_y - gw_y);
    }
  }
  SI px, py;
  target->pointer_position (x, y, px, py);
  send_mouse (wid, ev, px, py, gui->mouse_state, t);
}

/******************************************************************************
* The backing store
******************************************************************************/

// the size of the window in device pixels changed (a resize, or a move to a
// display of another density): a new backing store, which keeps what can be
// kept of the old one
bool
sdl_window_rep::sync_backing_store () {
  int pw= 1, ph= 1;
  SDL_GetWindowSizeInPixels (sdl_win, &pw, &ph);
  int ww= 1, wh= 1;
  SDL_GetWindowSize (sdl_win, &ww, &wh);
  if (ww > 0) density= ((float) pw) / ww;
  pw= max (pw, 1); ph= max (ph, 1);
  int bs_w= backing_store->get_width ();
  int bs_h= backing_store->get_height ();
  if (pw == bs_w && ph == bs_h) return false;

  picture new_store= native_opaque_picture (pw, ph, 0, 0);
  fz_pixmap* src= ((mupdf_picture_rep*) backing_store->get_handle ())->pix;
  fz_pixmap* dst= ((mupdf_picture_rep*) new_store->get_handle ())->pix;
  int cw= min (bs_w, pw), ch= min (bs_h, ph);
  if (src != NULL && dst != NULL && src->n == dst->n)
    for (int y= 0; y < ch; y++)
      memcpy (dst->samples + y * dst->stride, src->samples + y * src->stride,
              (size_t) cw * src->n);
  if (pw > bs_w) invalidate_event (bs_w, 0, pw, ph);
  if (ph > bs_h) invalidate_event (0, bs_h, pw, ph);
  invalid_regions= invalid_regions & rectangles (rectangle (0, 0, pw, ph));

  renderer ren2= picture_renderer (new_store, std_shrinkf * retina_factor);
  delete_renderer (ren);
  ren= ren2;
  backing_store= new_store;
  expose (); // the surface of the window was replaced as well
  return true;
}

void
sdl_window_rep::expose () {
  dirty= rectangles (rectangle (0, 0, backing_store->get_width (),
                                backing_store->get_height ()));
}

// copy what was repainted to the surface of the window, and show it
void
sdl_window_rep::present () {
  if (is_nil (dirty)) return;
  SDL_Surface* surf= SDL_GetWindowSurface (sdl_win);
  fz_pixmap* pix= ((mupdf_picture_rep*) backing_store->get_handle ())->pix;
  if (surf == NULL || pix == NULL || pix->n != 4) {
    // e.g. a window which is being destroyed or minimized
    dirty= rectangles ();
    return;
  }
  int W= min (pix->w, surf->w), H= min (pix->h, surf->h);
  rectangle lub= least_upper_bound (dirty);
  if (area (lub) < 1.2 * area (dirty)) dirty= rectangles (lub);
  bool locked= SDL_MUSTLOCK (surf) && SDL_LockSurface (surf);
  int bpp= SDL_BYTESPERPIXEL (surf->format);
  array<SDL_Rect> rects;
  for (rectangles l= dirty; !is_nil (l); l= l->next) {
    int x1= max ((int) l->item->x1, 0), y1= max ((int) l->item->y1, 0);
    int x2= min ((int) l->item->x2, W), y2= min ((int) l->item->y2, H);
    if (x1 >= x2 || y1 >= y2) continue;
    // the pixmap is RGBA in memory; SDL converts to the format of the
    // window (BGRA on most systems)
    SDL_ConvertPixels (x2 - x1, y2 - y1, SDL_PIXELFORMAT_RGBA32,
                       pix->samples + (ptrdiff_t) y1 * pix->stride + 4 * x1,
                       (int) pix->stride, surf->format,
                       ((unsigned char*) surf->pixels) +
                         (ptrdiff_t) y1 * surf->pitch + bpp * x1,
                       surf->pitch);
    SDL_Rect r= { x1, y1, x2 - x1, y2 - y1 };
    rects << r;
  }
  if (locked) SDL_UnlockSurface (surf);
  dirty= rectangles ();
  if (N(rects) > 0 && !SDL_UpdateWindowSurfaceRects (sdl_win, A(rects), N(rects))) {
    static int reported= 0;
    if (reported++ < 3)
      SDL_Log ("SDL_UpdateWindowSurfaceRects failed: %s", SDL_GetError ());
  }
}

void
sdl_window_rep::repaint_invalid_regions () {
  sync_backing_store ();

  // repaint invalid rectangles if needed
  if (!is_nil (invalid_regions)) {
    rectangles new_regions;

    // simplify
    rectangle lub= least_upper_bound (invalid_regions);
    if (area (lub) < 1.2 * area (invalid_regions))
      invalid_regions= rectangles (lub);

    while (!is_nil (invalid_regions)) {
      ren->set_origin (0, 0);
      rectangle r= thicken (invalid_regions->item, 1, 1);
      dirty= dirty | rectangles (r);
      SI x1= r->x1, y1= r->y1, x2= r->x2, y2= r->y2;
      ren->encode (x1, y1);
      ren->encode (x2, y2);
      ren->set_clipping (x1, y2, x2, y1);
      send_repaint (w, ren, x1, y2, x2, y1);
      ren->set_clipping (x1, y2, x2, y1, true);
      if (gui_interrupted ())
        new_regions= rectangles (invalid_regions->item, new_regions);
      invalid_regions= invalid_regions->next;
    }
    invalid_regions= new_regions;
  }
  present ();
}

// move the pixels of the rectangle (x1, y1)-(x2, y2) by (dx, dy), inside
// that rectangle (what moves out of it is lost)
void
sdl_window_rep::shift_pixels (int x1, int y1, int x2, int y2, int dx, int dy) {
  fz_pixmap* pix= ((mupdf_picture_rep*) backing_store->get_handle ())->pix;
  if (pix == NULL || pix->samples == NULL) return;
  x1= max (x1, 0); y1= max (y1, 0);
  x2= min (x2, pix->w); y2= min (y2, pix->h);
  // the destination: the rectangle moved, within the rectangle
  int X1= max (x1, x1 + dx), X2= min (x2, x2 + dx);
  int Y1= max (y1, y1 + dy), Y2= min (y2, y2 + dy);
  if (X1 >= X2 || Y1 >= Y2) return;
  int n= pix->n;
  ptrdiff_t stride= pix->stride;
  size_t len= (size_t) (X2 - X1) * n;
  if (dy > 0) // down: from the last row up, so as not to overwrite the sources
    for (int y= Y2 - 1; y >= Y1; y--)
      memmove (pix->samples + y * stride + X1 * n,
               pix->samples + (y - dy) * stride + (X1 - dx) * n, len);
  else
    for (int y= Y1; y < Y2; y++)
      memmove (pix->samples + y * stride + X1 * n,
               pix->samples + (y - dy) * stride + (X1 - dx) * n, len);
  dirty= dirty | rectangles (rectangle (X1, Y1, X2, Y2));
}

void
sdl_window_rep::translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) {
  ren->set_origin (0,0);
  SI X1= x1+ dx;
  SI Y2= y2+ dy;
  ren->decode (x1, y1);
  ren->decode (x2, y2);
  ren->decode (X1, Y2);
  dx= X1- x1;
  dy= Y2- y2;

  rectangles region (rectangle (x1, y2, x2, y1));
  rectangles invalid_intern= invalid_regions & region;
  rectangles invalid_extern= invalid_regions - invalid_intern;
  invalid_intern = ::translate (invalid_intern, dx, dy) & region;
  invalid_regions= invalid_extern | invalid_intern;

  rectangles extra= thicken (region - ::translate (region, dx, dy), 1, 1);
  invalid_regions= invalid_regions | extra;

  if (x1<x2 && y2<y1)
    shift_pixels ((int) x1, (int) y2, (int) x2, (int) y1, (int) dx, (int) dy);
}

void
sdl_window_rep::set_keyboard_focus (widget wid, bool get_focus) {
  ASSERT (get_focus, "explicit loss of keyboard focus not yet implemented");
  if (has_focus && (kbd_focus != wid.rep)) {
    notify_keyboard_focus (kbd_focus, false);
    notify_keyboard_focus (wid, true);
  }
  kbd_focus= wid.rep;
}

bool
sdl_window_rep::get_keyboard_focus (widget wid) {
  return has_focus && kbd_focus == wid.rep;
}

void
sdl_window_rep::set_mouse_grab (widget wid, bool get_grab) {
  if (get_grab) gui->obtain_mouse_grab (wid);
  else gui->release_mouse_grab ();
}

bool
sdl_window_rep::get_mouse_grab (widget w) {
  return gui->has_mouse_grab (w);
}

void
sdl_window_rep::set_mouse_pointer (widget wid, string name, string mask) {
  if (mask == "") gui->set_mouse_pointer (wid, name);
  else gui->set_mouse_pointer (wid, name, mask);
}

/******************************************************************************
* Delayed messages
******************************************************************************/

message_rep::message_rep (widget wid2, string s2, time_t t2):
  wid (wid2), s (s2), t (t2) {}
message::message (widget wid, string s, time_t t):
  rep (tm_new<message_rep> (wid, s, t)) {}

tm_ostream&
operator << (tm_ostream& out, message m) {
  return out << "message " << m->s << " to " << m->wid
	     << "at time " << m->t << "\n";
}

static list<message>
insert_message (list<message> l, widget wid, string s, time_t cur, time_t t) {
  if (is_nil (l)) return list<message> (message (wid, s, t));
  time_t ref= l->item->t;
  if ((t-cur) <= (ref-cur)) return list<message> (message (wid, s, t), l);
  return list<message> (l->item, insert_message (l->next, wid, s, cur, t));
}

void
sdl_window_rep::delayed_message (widget wid, string s, time_t delay) {
  time_t ct= texmacs_time ();
  the_gui->messages= insert_message (the_gui->messages, wid, s, ct, ct+ delay);
}

/******************************************************************************
* Routines concerning regions in a window
******************************************************************************/

void
sdl_window_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {
  ren->set_origin(0, 0);
  ren->outer_round (x1, y1, x2, y2);
  ren->decode (x1, y1);
  ren->decode (x2, y2);
  invalidate_event (x1, y2, x2, y1);
}

bool
sdl_window_rep::is_invalid () {
  return ! is_nil (invalid_regions);
}

void
sdl_window_rep::invalidate_all () {
  invalidate_event (0, 0, backing_store->get_width(), backing_store->get_height());
}

/******************************************************************************
* Interface
******************************************************************************/

window
popup_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  window win= tm_new<sdl_window_rep> (w, the_gui, name, true,
				      min_w, min_h, def_w, def_h, max_w, max_h);
  return win;
}

window
plain_window (widget w, string name, SI min_w, SI min_h,
	      SI def_w, SI def_h, SI max_w, SI max_h)
{
  window win= tm_new<sdl_window_rep> (w, the_gui, name, false,
				      min_w, min_h, def_w, def_h, max_w, max_h);
  return win;
}
