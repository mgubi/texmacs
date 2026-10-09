
/******************************************************************************
* MODULE     : tm_window.hpp
* DESCRIPTION: TeXmacs main data structures (buffers, views and windows)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_WINDOW_H
#define TM_WINDOW_H
#include "server.hpp"
#include "tm_buffer.hpp"

/******************************************************************************
* Views and places
*******************************************************************************
* A view is an editor on a buffer. Where a view is shown is a place: a pane
* of the page, or a field of a dialog. The core knows a place by its number
* and nothing else (docs/tau-design.md): the windows of the other ports,
* with their widgets, their bars and their menus, are of the page. A view
* which is not shown has the place 0. Scheme names a place by the url which
* named a window (tmfs://window/N).
******************************************************************************/

class tm_view_rep {
public:
  tm_buffer buf;
  editor    ed;
  int       place;  // where the view is shown, or 0
  int       nr;
  tm_view_rep (tm_buffer buf2, editor ed2);
};

typedef tm_buffer_rep* tm_buffer;
typedef tm_view_rep*   tm_view;

int    new_place (bool embedded= false);  // a place for a pane or in a dialog
void   delete_place (int place);
bool   is_place (int place);
url    place_url (int place);             // url_none for no place
int    url_place (url u);                 // 0 when it is none
int    current_place ();                  // of the current view, or 0
tm_view place_view (int place);           // the view shown there, or NULL
double get_place_zoom (int place);
void   set_place_zoom (int place, double zoom);
double get_doc_zoom_factor (tree doc);

widget texmacs_output_widget (tree doc, tree style);
widget texmacs_input_widget (tree doc, tree style, url wname);
bool is_embedded_buffer (url name);
array<SI> get_texmacs_widget_size (widget wid);

int window_handle ();
void window_create (int win, widget wid, string name, command quit);
void window_create_plain (int win, widget wid, string name);
void window_create_popup (int win, widget wid, string name);
void window_create_tooltip  (int win, widget wid, string name);
void window_delete (int win);
void window_show (int win);
void window_hide (int win);
void window_set_on_top (int win, bool flag);
scheme_tree window_get_size (int win);
void window_set_size (int win, int w, int h);
scheme_tree window_get_position (int win);
void window_set_position (int win, int x, int y);
void windows_delayed_refresh (int ms);
void windows_refresh (string kind= "auto");
path window_search (url name);

#endif // defined TM_WINDOW_H
