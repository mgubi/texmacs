
/******************************************************************************
* MODULE     : new_window.cpp
* DESCRIPTION: Global window management
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_data.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "web_files.hpp"
#include "tm_link.hpp"
#include "message.hpp"
#include "dictionary.hpp"
#include "new_document.hpp"
#include "hashmap.hpp"

/******************************************************************************
* Places
*******************************************************************************
* The core has no windows: a view is shown at a place, which is a number
* (tm_window.hpp). The places of the panes of the page are listed, for the
* commands which go through them; those of the views in dialogs are not
* (their numbers are negative). What is kept for a place is its zoom
* factor, which stays when another buffer is shown there.
******************************************************************************/

static int last_place= 1;
static int last_embedded_place= -1;
static array<int> all_places;
static hashmap<int,double> place_zoom (1.0);

void tau_place_made (int place);    // told to the page (Tau/tau_gui.cpp)
void tau_place_deleted (int place);

int
new_place (bool embedded) {
  int place= embedded? last_embedded_place--: last_place++;
  if (!embedded) all_places << place;
  place_zoom (place)= retina_zoom * get_server () -> get_default_zoom_factor ();
  return place;
}

void
delete_place (int place) {
  array<int> r;
  for (int i=0; i<N(all_places); i++)
    if (all_places[i] != place) r << all_places[i];
  all_places= r;
  place_zoom->reset (place);
  if (place > 0) tau_place_deleted (place);
}

bool
is_place (int place) {
  return place != 0 && place_zoom->contains (place);
}

url
place_url (int place) {
  if (place == 0) return url_none ();
  return url ("tmfs://place/" * as_string (place));
}

int
url_place (url u) {
  if (is_none (u)) return 0;
  string s= as_string (u);
  if (!starts (s, "tmfs://place/")) return 0;
  s= s (13, N(s));
  if (!is_int (s)) return 0;
  int place= as_int (s);
  return is_place (place)? place: 0;
}

tm_view
place_view (int place) {
  if (place == 0) return NULL;
  array<url> vs= get_all_views ();
  for (int i=0; i<N(vs); i++) {
    tm_view vw= concrete_view (vs[i]);
    if (vw != NULL && vw->place == place) return vw;
  }
  return NULL;
}

int
current_place () {
  tm_view vw= concrete_view (get_current_view_safe ());
  return vw == NULL? 0: vw->place;
}

double
get_place_zoom (int place) {
  return place_zoom [place];
}

void
set_place_zoom (int place, double zoom) {
  if (is_place (place)) place_zoom (place)= zoom;
}

/******************************************************************************
* The places as Scheme knows them
******************************************************************************/

array<url>
windows_list () {
  array<url> r;
  for (int i=0; i<N(all_places); i++) r << place_url (all_places[i]);
  return r;
}

int
get_nr_windows () {
  return N(all_places);
}

bool
has_current_window () {
  return current_place () != 0;
}

url
get_current_window () {
  if (!has_current_window ()) return url ("");
  return place_url (current_place ());
}

array<url>
buffer_to_windows (url name) {
  array<url> r, vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++) {
    url win= view_to_window (vs[i]);
    if (!is_none (win)) r << win;
  }
  return r;
}

url
window_to_buffer (url win) {
  return view_to_buffer (window_to_view (win));
}

url
window_to_view (url win) {
  tm_view vw= place_view (url_place (win));
  if (vw == NULL) return url_none ();
  return abstract_view (vw);
}

void
window_set_buffer (url win, url name) {
  url old= window_to_view (win);
  if (is_none (old) || view_to_buffer (old) == name) return;
  window_set_view (win, get_passive_view (name), false);
}

void
window_focus (url win) {
  if (win == get_current_window ()) return;
  url old= window_to_view (win);
  if (is_none (old)) return;
  set_current_view (old);
}

void
switch_to_window (url new_w) {
  url old_w= get_current_window ();
  if (new_w == old_w) return;
  url old_u= window_to_view (old_w);
  url new_u= window_to_view (new_w);
  if (!is_none (old_u) && !is_none (new_u)) {
    tm_view old_vw = concrete_view (old_u);
    if (old_vw != NULL) old_vw->ed->suspend ();
  }
  if (!is_none (new_u)) {
    tm_view new_vw = concrete_view (new_u);
    if (new_vw != NULL) {
      new_vw->ed->resume ();
      send_keyboard_focus (new_vw->ed);
    }
  }
}

/******************************************************************************
* Other subroutines
******************************************************************************/

void
create_buffer (url name, tree doc) {
  if (!is_nil (concrete_buffer (name))) return;
  set_buffer_tree (name, doc);
}

void
new_buffer_in_this_window (url name, tree doc) {
  if (is_nil (concrete_buffer (name)))
    create_buffer (name, doc);
  switch_to_buffer (name);
}

url
new_buffer_in_new_window (url name, tree doc, tree geom) {
  // a new place: the core makes it at once, so that the commands which
  // follow find the buffer there, and the page shows it as it wants
  (void) geom;
  if (is_nil (concrete_buffer (name)))
    create_buffer (name, doc);
  url win= place_url (new_place ());
  window_set_view (win, get_passive_view (name), true);
  return win;
}

/******************************************************************************
* Exported routines
******************************************************************************/

url
create_buffer () {
  url name= make_new_buffer ();
  switch_to_buffer (name);
  return name;
}

url
open_window (tree geom) {
  url name= make_new_buffer ();
  return new_buffer_in_new_window (name, tree (DOCUMENT), geom);
}

void
clone_window () {
  url win= place_url (new_place ());
  window_set_view (win, get_passive_view (get_current_buffer ()), true);
}

void
kill_buffer (url name) {
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    if (!is_none (vs[i])) {
      url prev= get_recent_view (name, false, true, false, true);
      if (is_none (prev)) {
        prev= get_recent_view (name, false, true, false, false);
        if (is_none (prev)) continue;
        prev= get_new_view (view_to_buffer (prev));
      }
      window_set_view (view_to_window (vs[i]), prev, false);
    }
  remove_buffer (name);
}

static void
delete_window (url win) {
  int place= url_place (win);
  if (place == 0) return;
  // (the view is kept: at least one is needed for buffer_modified)
  tm_view vw= place_view (place);
  if (vw != NULL) detach_view (abstract_view (vw));
  delete_place (place);
}

void
kill_window (url wname) {
  // the place is given up; another one takes the keyboard. The last place
  // stays: the page has nothing else to show
  array<url> vs= get_all_views ();
  for (int i=0; i<N(vs); i++) {
    url win= view_to_window (vs[i]);
    if (!is_none (win) && win != wname && url_place (win) > 0) {
      set_current_view (vs[i]);
      delete_window (wname);
      return;
    }
  }
}

void
kill_current_window_and_buffer () {
  url name= get_current_buffer ();
  array<url> vs= buffer_to_views (get_current_buffer ());
  url win= get_current_window ();
  bool kill= true;
  for (int i=0; i<N(vs); i++)
    if (view_to_window (vs[i]) != win)
      kill= false;
  if (get_nr_windows () <= 1) return;
  kill_window (win);
  if (kill) remove_buffer (name);
}
