
/******************************************************************************
* MODULE     : tm_window.cpp
* DESCRIPTION: The views in dialogs, and what is left of the windows
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_window.hpp"
#include "tm_data.hpp"
#include "message.hpp"
#include "dictionary.hpp"
#include "merge_sort.hpp"
#include "iterator.hpp"
#include "boot.hpp"
#include "drd_std.hpp"

int geometry_w= 800, geometry_h= 600;
int geometry_x= 0  , geometry_y= 0;

// The core has no windows (tm_window.hpp): a view is shown at a place, and
// the bars and the menus around it are of the page (tm_frame.cpp). What is
// here is the view in a dialog, which has a place of its own, and the
// functions of the windows which Scheme still calls.

double
get_doc_zoom_factor (tree doc) {
  if (is_compound (doc))
    for (int i=0; i<N(doc); i++) {
      if (is_compound (doc[i], "initial") || is_func (doc[i], COLLECTION))
        return get_doc_zoom_factor (doc[i]);
      else {
	if (is_func (doc[i], ASSOCIATE, 2) && doc[i][0] == ZOOM_FACTOR)
	  return as_double (doc[i][1]);
      }
    }
  return -1.0;
}

/******************************************************************************
* A view in a dialog: a document which is edited there (texmacs-input)
******************************************************************************/

static hashmap<tree,int>&
embedded_buffers_table () {
  static hashmap<tree,int>* t= NULL;
  if (t == NULL) t= tm_new<hashmap<tree,int> > (0);
  return *t;
}
#define embedded_buffers (embedded_buffers_table ())

class close_embedded_command_rep: public command_rep {
  tm_view vw;
  url name;
public:
  close_embedded_command_rep (tm_view vw2, url n2): vw (vw2), name (n2) {
    embedded_buffers (name->t)= embedded_buffers [name->t] + 1; }
  void apply ();
  tm_ostream& print (tm_ostream& out) {
    return out << "<command close_embedded>"; }
};

void
close_embedded_command_rep::apply () {
  // the dialog is gone: the keyboard goes back to the view which opened
  // it, or to any view which is shown, and the buffer is closed
  ASSERT (!is_nil (vw->ed), "embedded command acting on deleted editor");
  url foc= url_none ();
  if (vw->ed->mvw != NULL) foc= place_url (vw->ed->mvw->place);
  if (is_none (foc)) {
    array<url> a= windows_list ();
    if (N(a) != 0) foc= a[0];
  }
  if (!is_none (foc)) window_focus (foc);
  int place= vw->place;
  embedded_buffers (name->t)= embedded_buffers [name->t] - 1;
  if (embedded_buffers [name->t] <= 0) embedded_buffers->reset (name->t);
  detach_view (abstract_view (vw));
  remove_buffer (vw->buf->buf->name);
  delete_place (place);
}

path
window_search (url name) {
  // (for Scheme: not nil when the buffer is edited in a dialog)
  if (embedded_buffers->contains (name->t)) return path (1);
  return path ();
}

bool
is_embedded_buffer (url name) {
  return embedded_buffers->contains (name->t);
}

url
embedded_name (url name) {
  static int nr= 0;
  if (!is_none (name)) return name;
  nr++;
  return url (string ("tmfs://aux/TeXmacs-input-" * as_string (nr)));
}

tree
enrich_embedded_document (tree body, tree style) {
  tree orig= body;
  if (is_func (body, WITH)) body= body[N(body)-1];
  if (!is_func (body, DOCUMENT)) body= tree (DOCUMENT, body);
  hashmap<string,tree> initial (UNINIT);
  initial (PAGE_MEDIUM)= "automatic";
  initial (PAGE_SCREEN_LEFT)= "4px";
  initial (PAGE_SCREEN_RIGHT)= "4px";
  initial (PAGE_SCREEN_TOP)= "2px";
  initial (PAGE_SCREEN_BOT)= "2px";
  
  if (is_func (orig, WITH))
    for (int i=0; i+2<N(orig); i+=2)
      if (is_atomic (orig[i])) {
        //cout << "Set " << orig[i] << " = " << orig[i+1] << LF;
        initial (orig[i]->label)= orig[i+1];
      }
  //initial (DPI)= "720";
  //initial (ZOOM_FACTOR)= (retina_zoom==1? "1.2": "1.8");
  initial (DPI)= "600";
  initial (ZOOM_FACTOR)= (retina_zoom==2? "1.0": "1.2");
  // TODO: to be carefully checked for all operating systems
  initial ("no-zoom")= "true";
  tree doc (DOCUMENT);
  doc << compound ("TeXmacs", TEXMACS_VERSION);
  doc << style; //compound ("style", style);
  doc << compound ("body", body);
  doc << compound ("initial", make_collection (initial));
  if (initial->contains ("project"))
    doc << compound ("project", initial ["project"]);
  return doc;
}

widget
texmacs_input_widget (tree doc, tree style, url wname) {
  // the widget is the editor itself, which is a view as that of a pane
  // (Tau/tau_gui.cpp); what is done when the dialog goes is kept with it
  doc= enrich_embedded_document (doc, style);
  url       base = get_master_buffer (get_current_buffer ());
  tm_view   curvw= concrete_view (get_current_view ());
  url       name = embedded_name (wname);
  if (contains (name, get_all_buffers ())) set_buffer_tree (name, doc);
  else create_buffer (name, doc);
  url       u    = get_passive_view (name);
  tm_view   vw   = concrete_view (u);
  int       place= new_place (true);
  double    zoom = retina_zoom * get_doc_zoom_factor (doc);
  if (zoom > 0.0) set_place_zoom (place, zoom);
  set_master_buffer (name, base);
  url temp= get_current_view_safe ();
  attach_view (place_url (place), u);
  set_current_view (temp);
  vw->ed->mvw= curvw;
  command close_cmd= tm_new<close_embedded_command_rep> (vw, name);
  return wrapped_widget (vw->ed, close_cmd);
}

/******************************************************************************
* The windows of the other ports which Scheme may still ask for: the
* dialogs are described to the page (kernel/gui/menu-serial.scm)
******************************************************************************/

int
window_handle () {
  static int window_next= 1;
  return window_next++;
}

void window_create (int win, widget wid, string name, command quit) {
  (void) win; (void) wid; (void) name; (void) quit; }
void window_create_plain (int win, widget wid, string name) {
  (void) win; (void) wid; (void) name; }
void window_create_popup (int win, widget wid, string name) {
  (void) win; (void) wid; (void) name; }
void window_create_tooltip (int win, widget wid, string name) {
  (void) win; (void) wid; (void) name; }
void window_delete (int win) { (void) win; }
void window_show (int win) { (void) win; }
void window_hide (int win) { (void) win; }
void window_set_on_top (int win, bool flag) { (void) win; (void) flag; }
void window_set_size (int win, int w, int h) { (void) win; (void) w; (void) h; }
void window_set_position (int win, int x, int y) {
  (void) win; (void) x; (void) y; }

scheme_tree
window_get_size (int win) {
  (void) win;
  return tuple ("0", "0");
}

scheme_tree
window_get_position (int win) {
  (void) win;
  return tuple ("0", "0");
}

/******************************************************************************
* Refreshing
******************************************************************************/

static time_t refresh_time= 0;

void
windows_delayed_refresh (int ms) {
  refresh_time= texmacs_time () + ms;
}

void
windows_refresh (string kind) {
  // the parts of the interface of a given kind are described again
  // (refresh-now); "auto" is the turn of the menus, which the editors
  // look after themselves
  void tau_refresh (string kind);
  if (kind == "auto") {
    if (texmacs_time () < refresh_time) return;
    windows_delayed_refresh (1000000000);
  }
  else tau_refresh (kind);
}
