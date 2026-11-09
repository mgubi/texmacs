
/******************************************************************************
* MODULE     : tm_window.cpp
* DESCRIPTION: Main TeXmacs windows
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

widget texmacs_window_widget (widget wid, tree geom);
widget make_menu_widget (object menu);
widget make_menu_widget (object menu, int w, int h);
void refresh_size (widget wid, bool exact);

static int pending_window_handle= 0; // window under construction, 0 if none
static void end_tool_openings (string prefix, bool all);
static hashmap<int,widget> window_table (NULL);
static hashmap<tree,path> window_by_name;
static time_t refresh_time= 0;

/******************************************************************************
* User preference management concerning the geometry of windows
******************************************************************************/

hashmap<string,string> window_names ("");

string
unique_window_name (string name) {
  for (int i=1; true; i++) {
    string wname= name;
    if (i > 1) wname= wname * ":" * as_string (i);
    if (!window_names->contains (wname)) {
      window_names (wname)= name;
      return wname;
    }
  }
}

static bool
move_accept (int old_x, int old_y, int x, int y) {
  if (old_x == x && old_y == y) return false;
  return true;
}

void
notify_window_move (string name, SI xx, SI yy) {
  int x=  xx / PIXEL;
  int y= -yy / PIXEL;
  if (name != "popup") {
    //cout << "Move " << name << " to " << x << ", " << y << "\n";
    string old_x= get_user_preference ("abscissa " * name, "");
    string old_y= get_user_preference ("ordinate " * name, "");
    //cout << "Move " << name << ": " << old_x << ", " << old_y
    //<< " --> " << x << ", " << y << "\n";
    if (old_x == "" || old_y == "" ||
        move_accept (as_int (old_x), as_int (old_y), x, y)) {
      set_user_preference ("abscissa " * name, as_string (x));
      set_user_preference ("ordinate " * name, as_string (y));
    }
  }
}

static bool
resize_accept (int old_w, int old_h, int w, int h) {
  if (old_w == w && old_h == h) return false;
#ifdef QTTEXMACS
  if (old_w == w && old_h - 80 <= h && h <= old_h + 80) return false;
#endif
  return true;
}

void
notify_window_resize (string name, SI ww, SI hh) {
  int w= ww / PIXEL;
  int h= hh / PIXEL;
  if (name != "popup") {
    //cout << "Resize " << name << " to " << ww << ", " << hh << "\n";
    string old_w= get_user_preference ("width " * name, "");
    string old_h= get_user_preference ("height " * name, "");
    //cout << "Resize " << name << ": " << old_w << ", " << old_h
    //<< " --> " << w << ", " << h << "\n";
    if (old_w == "" || old_h == "" ||
        resize_accept (as_int (old_w), as_int (old_h), w, h)) {
      set_user_preference ("width " * name, as_string (w));
      set_user_preference ("height " * name, as_string (h));
    }
  }
}

void
notify_window_destroy (string name) {
  window_names->reset (name);
}

void
get_preferred_position (string name, SI& xx, SI& yy) {
  if (has_user_preference ("abscissa " * name)) {
    int x= as_int (get_user_preference ("abscissa " * name));
    int y= as_int (get_user_preference ("ordinate " * name));
    
    xx=  x * PIXEL;
    yy= -y * PIXEL;
  }
}

void
get_preferred_size (string name, SI& ww, SI& hh) {
  if (has_user_preference ("width " * name)) {
    int w= as_int (get_user_preference ("width " * name));
    int h= as_int (get_user_preference ("height " * name));
    ww= w * PIXEL;
    hh= h * PIXEL;
    //cout << "Size " << name << ": " << w << ", " << h << "\n";
  }
}

/******************************************************************************
* Meta editor constructor and destructor
******************************************************************************/

static int tm_window_serial= 1;

tm_window_rep::tm_window_rep (widget wid2, tree geom):
  win (texmacs_window_widget (wid2, geom)),
  wid (wid2), id (create_window_id ()),
  serial (tm_window_serial++),
  menu_current (object ()), menu_cache (widget ()),
  text_ptr (NULL), cur_url (url_none ())
{
  zoomf= retina_zoom * get_server () -> get_default_zoom_factor ();
}

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

tm_window_rep::tm_window_rep (tree doc, command quit):
  win (texmacs_widget (0, quit)),
  wid (win), id (url_none ()),
  serial (tm_window_serial++),
  menu_current (object ()), menu_cache (widget ()),
  text_ptr (NULL), cur_url (url_none ())
{
  zoomf= retina_zoom * get_doc_zoom_factor (doc);
  if (zoomf < 0.0)
    zoomf= retina_zoom * get_server () -> get_default_zoom_factor ();
}

tm_window_rep::~tm_window_rep () {
  // the tools of a window end with it (#776)
  if (!is_none (id)) end_tool_openings (as_string (id) * "|", true);
  if (!is_none (id)) destroy_window_id (id);
}

/******************************************************************************
* Creation of TeXmacs window
******************************************************************************/

widget
texmacs_window_widget (widget wid, tree geom) {
  SI W, H;
  SI w= geometry_w, h= geometry_h;
  SI x= geometry_x, y= geometry_y;
  bool custom= is_tuple (geom) && N (geom) >= 2;
#ifndef QTTEXMACS
  if (use_side_tools) { w += 200; h += 100; }
  if (use_left_tools) { w += 200; h += 100; }
#endif
  if (custom) {
    w= as_int (geom[0]);
    h= as_int (geom[1]);
  }
  if (w == 800 && h == 600) {
    w *= retina_zoom;
    h *= retina_zoom;
  }
  gui_root_extents (W, H); W /= PIXEL; H /= PIXEL;
  if (x < 0) x= W + x + 1 - w;
  if (y < 0) y= H + y + 1 - h;
  string name= "TeXmacs";
  name= unique_window_name (name);
  widget win= plain_window_widget (wid, name);
  SI xx= x * PIXEL, yy= -y * PIXEL;
  SI ww= w * PIXEL, hh=  h * PIXEL;
  if (!custom) {
    get_preferred_position (name, xx, yy);
    get_preferred_size (name, ww, hh);
  }
  set_size (win, ww, hh);
  set_position (win, xx, yy);
  return win;
}

/******************************************************************************
* Openings of embedded TeXmacs widgets (#776)
*
* A named texmacs-input has one editor per opening of the dialog or the tool
* which shows it. An opening is a number, never reused, taken:
* - by a dialog when its window is made (window_handle): every refresh of
*   the dialog is in that opening;
* - by a tool of a tool slot of a window (side, left, bottom, extra) when it
*   is shown at a build of the slot while it was not before; a tool still
*   shown keeps its opening; a tool no longer shown ends it;
* - elsewhere (a menu, a widget made by hand), by the field itself: a field
*   whose name is in a live opening is made again in that opening, else in a
*   new one, which ends when its widget is let go.
* The refreshables keep the opening in which they were made. A field made
* again in its opening keeps its view, and so its editor and its history;
* a field in a new opening gets a new view. When an opening ends, the views
* of its fields are deleted, and the buffers of unnamed fields closed.
******************************************************************************/

static int current_opening= 0;  // the opening whose widgets are being made
static int opening_next= 1;
static hashmap<int,int> dialog_opening (0);  // alternative window -> opening
static hashmap<string,int> tool_opening (0); // "window|slot|tool" -> opening
static hashmap<int,array<url> > opening_fields; // the names of its fields
static hashmap<string,url> field_view (url_none ()); // "opening|name" ->
static hashmap<string,tree> field_body;   // the body at the last build
static hashmap<string,url> field_main (url_none ()); // window of the focus
static hashmap<string,bool> field_named (false);
static hashmap<string,int> field_owner (0);  // name -> its latest opening
static tm_window_rep* tool_build_win= NULL;  // tool slot being made
static int tool_build_which= -1;
static hashset<string> tool_build_seen;

static int
new_opening () {
  return opening_next++;
}

static string
field_key (int op, url name) {
  return as_string (op) * "|" * as_string (name);
}

int
embedded_opening () {
  return current_opening;
}

void
embedded_opening_set (int op) {
  current_opening= op;
}

int
embedded_tool_opening (string tool) {
  // the opening of a tool of the tool slot being made, which becomes the
  // current one; returns the former current opening
  int old= current_opening;
  if (tool_build_win == NULL) return old;
  string key= as_string (tool_build_win->id) * "|" *
              as_string (tool_build_which) * "|" * tool;
  if (!tool_opening->contains (key)) tool_opening (key)= new_opening ();
  tool_build_seen->insert (key);
  current_opening= tool_opening [key];
  return old;
}

static void
end_field (int op, url name, bool remove_named) {
  // the end of a field of an opening: its view is deleted, with its editor
  // and its history; the buffer of an unnamed field is closed, a named one
  // stays (the search toolbar after the search tool), unless remove_named
  // and it has no other view
  string key = field_key (op, name);
  tm_view v  = concrete_view (field_view [key]);
  url  mainw = field_main [key];
  bool named = field_named [key];
  field_view->reset (key);
  field_body->reset (key);
  field_main->reset (key);
  field_named->reset (key);
  if (field_owner [as_string (name)] == op)
    field_owner->reset (as_string (name));
  if (opening_fields->contains (op)) {
    array<url> a= opening_fields [op], b;
    for (int i=0; i<N(a); i++)
      if (a[i] != name) b << a[i];
    opening_fields (op)= b;
  }
  if (v == NULL) return;
  url u= abstract_view (v);
  v->win= NULL;
  v->ed->cvw= NULL;
  if (u == get_current_view_safe ()) {
    // the focus goes to the window of the field
    url nv= url_none ();
    if (!is_none (mainw)) nv= window_to_view (mainw);
    if (is_none (nv) || nv == u) {
      array<url> h= get_all_views ();
      for (int i=0; i<N(h) && is_none (nv); i++)
        if (h[i] != u && concrete_view (h[i]) != NULL) nv= h[i];
    }
    if (!is_none (nv)) set_current_view (nv);
  }
  if (N (buffer_to_views (name)) == 1 && (!named || remove_named))
    remove_buffer (name);
  else delete_view (u);
}

static void
end_opening (int op) {
  if (!opening_fields->contains (op)) return;
  array<url> a= copy (opening_fields [op]);
  for (int i=0; i<N(a); i++) end_field (op, a[i], false);
  opening_fields->reset (op);
}

static void
end_tool_openings (string prefix, bool all) {
  // the tools of a slot no longer shown at its build, or all the tools of
  // a window which is deleted
  array<string> gone;
  iterator<string> it= iterate (tool_opening);
  while (it->busy ()) {
    string key= it->next ();
    if (starts (key, prefix) && (all || !tool_build_seen->contains (key)))
      gone << key;
  }
  for (int i=0; i<N(gone); i++) {
    int op= tool_opening [gone[i]];
    tool_opening->reset (gone[i]);
    end_opening (op);
  }
}

/******************************************************************************
* Closing embedded TeXmacs widgets
******************************************************************************/

class close_embedded_command_rep: public command_rep {
  // The command keeps the name of the buffer and the window of the widget,
  // not the view: the buffer may have been closed before the widget (its
  // view is deleted then), the opening of the widget may have ended, or the
  // field be shown by a newer widget of its opening, which takes over its
  // view (#775, #776)
  // A widget has two such commands, of which at most one runs: one for its
  // destruction by its window, one for when it is let go without that
  // (dropped: side tools made again, #776), which keeps a named buffer.
  tm_window win;
  url name;
  int win_id;
  url main_win;  // the window of the focus when the field was made
  int op;        // the opening of the field
  bool dropped;
public:
  close_embedded_command_rep (tm_window w2, url n2, int win2, url m2,
                              int op2, bool d):
    win (w2), name (n2), win_id (win2), main_win (m2), op (op2),
    dropped (d) {
      if (win_id != 0 && !dropped)
        window_by_name(name->t)= path (win_id, window_by_name[name->t]); }
  void apply ();
  tm_ostream& print (tm_ostream& out) {
    return out << "<command close_embedded>"; }
};

static void
attach_embedded_view (tm_view vw, tm_window win) {
  vw->win= win;
  set_scrollable (win->wid, vw->ed);
  vw->ed->cvw= win->wid.rep;
}

void
close_embedded_command_rep::apply () {
  // the view of the buffer which is still shown by this widget, if any
  tm_view vw= NULL;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++) {
    tm_view v= concrete_view (vs[i]);
    if (v != NULL && v->win == win) vw= v;
  }
  // the focus goes back to the master window; when the widget was let go
  // (side tools made again, #776), only if the focus was in it
  tm_view cur= concrete_view (get_current_view_safe ());
  if (!dropped || cur == NULL || cur->win == win) {
    array<url> a= windows_list ();
    url foc= main_win;
    if (is_none (foc) || !contains (foc, a))
      foc= (N(a) == 0? url_none (): a[0]);
    if (!is_none (foc)) window_focus (foc);
  }
  // unregister the window only when it still shows the buffer, or the
  // buffer is gone: a newer widget of the same dialog (made again by a
  // refreshable) may have taken the buffer over without registering (#776)
  if (vw != NULL || !contains (name, get_all_buffers ()))
    window_by_name(name->t)= remove<int> (window_by_name[name->t], win_id);
  if (vw != NULL) {
    // the widget still shows its field: the field ends with it (its dialog
    // is closed, or it is let go otherwise than at the end of its opening);
    // a dialog which is closed closes a named buffer too, as before
    if (field_view [field_key (op, name)] == abstract_view (vw))
      end_field (op, name, !dropped);
    else {
      // not a field of this opening (should not happen)
      vw->win= NULL;
      vw->ed->cvw= NULL;
      if (N (vs) == 1 && !(dropped && field_named [field_key (op, name)]))
        remove_buffer (name);
      else delete_view (abstract_view (vw));
    }
  }
  tm_delete (win);
}

static command
close_embedded_command (tm_window win, url name, int id, url mainw,
                        int op, bool d) {
  return tm_new<close_embedded_command_rep> (win, name, id, mainw, op, d);
}

static path
filter_existing (path wins) {
  if (is_nil (wins)) return wins;
  if (window_table->contains (wins->item))
    return path (wins->item, filter_existing (wins->next));
  return filter_existing (wins->next);
}

path
window_search (url name) {
  return filter_existing (window_by_name[name->t]);
}

bool
is_embedded_buffer (url name) {
  return !is_nil (window_search (name));
}

/******************************************************************************
* Embedded TeXmacs widgets
******************************************************************************/

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
  doc= enrich_embedded_document (doc, style);
  url       base = get_master_buffer (get_current_buffer ());
  tm_view   curvw= concrete_view (get_current_view ());
  url       name = embedded_name (wname);
  // One editor per opening of a named texmacs-input (#776): made again in
  // its opening (a refresh, side tools made again), the field keeps its
  // view, with its editor and its undo history; in a new opening, it gets a
  // new view, whose editor starts with an empty history
  int op= current_opening;
  if (op == 0) {
    // made outside a dialog or a tool: in the live opening of its name if
    // any (a menu made again), else in a new one
    int own= field_owner [as_string (name)];
    if (own != 0 && concrete_view (field_view [field_key (own, name)]) != NULL)
      op= own;
    else op= new_opening ();
  }
  string  key   = field_key (op, name);
  tm_view vw    = concrete_view (field_view [key]);
  bool    exists= contains (name, get_all_buffers ());
  if (vw != NULL && exists) {
    // a new content is an edit which can be undone, unless nothing was
    // typed since the former one (a field which shows results): then the
    // history would only pile up the former contents
    tree before= copy (get_buffer_body (name));
    bool typed = (before != field_body [key]);
    set_buffer_tree (name, doc);
    if (!typed && get_buffer_body (name) != before)
      vw->ed->clear_undo_history ();
  }
  else {
    if (exists) set_buffer_tree (name, doc);
    else create_buffer (name, doc);
    vw= concrete_view (get_new_view (name));
    field_view  (key)= abstract_view (vw);
    field_named (key)= !is_none (wname);
    if (!contains (name, opening_fields [op])) opening_fields (op) << name;
  }
  field_owner (as_string (name))= op;
  field_body  (key)= copy (get_buffer_body (name));
  // the window to which the focus returns: the one of the focus when the
  // field was first made outside of it, else the window of its tool
  bool  real = (curvw != NULL && curvw->win != NULL &&
                !is_none (curvw->win->id));
  url   mainw= url_none ();
  if (real) mainw= abstract_window (curvw->win);
  else if (!is_none (field_main [key])) mainw= field_main [key];
  else if (tool_build_win != NULL) mainw= tool_build_win->id;
  field_main (key)= mainw;
  tm_window win  = tm_new<tm_window_rep> (doc, command ());
  set_master_buffer (name, base);
  attach_embedded_view (vw, win);
  vw->ed->mvw= (real? curvw: NULL);
  // NOTE: the widget is in an alternative window only when it is made while
  // that window is being built (between window_handle and window_create);
  // the widgets of the side tools of a main window are in none
  int     id       = pending_window_handle;
  command close_cmd= close_embedded_command (win, name, id, mainw, op, false);
  // when the widget is let go without being destroyed by its window, and
  // without the end of its opening, a buffer without a name is closed too:
  // nothing else can use it (#776)
  command drop_cmd = close_embedded_command (win, name, id, mainw, op, true);
  return wrapped_widget (win->wid, close_cmd, drop_cmd);
}

/******************************************************************************
* Meta mathods
******************************************************************************/

void
tm_window_rep::set_window_name (string s) {
  if (cur_title != s) {
    cur_title= s;
    set_name (wid, s);
  }
}

void
tm_window_rep::set_modified (bool flag) {
  ::set_modified (wid, flag);
}

void
tm_window_rep::set_window_url (url u) {
  if (!is_none (u)) {
    cur_url= u;
    set_file (wid, as_string (u));
  }
}

void
tm_window_rep::map () {
  set_visibility (win, true);
}

void
tm_window_rep::unmap () {
  set_visibility (win, false);
}

void
tm_window_rep::refresh () {
  menu_current= hashmap<int,object> (object ());
  menu_cache= hashmap<object,widget> (widget ());
}

/******************************************************************************
* Menus
******************************************************************************/

bool menu_caching= true;

bool
tm_window_rep::get_menu_widget (int which, string menu, widget& w) {
  drd_info old_drd= the_drd;
  if (!is_none (window_to_view (id))) {
    tm_view vw= concrete_view (window_to_view (id));
    if (vw != NULL) the_drd= vw->ed->drd;
  }
  //cout << "expand " << menu << "\n";
  object xmenu= call ("menu-expand", eval ("'" * menu));
  the_drd= old_drd;
  //if (which == 10) cout << "xmenu= " << xmenu << "\n";
  //cout << "xmenu= " << xmenu << "\n";
  if (menu_current[which] == xmenu) return false;
  if (menu_cache->contains (xmenu)) {
    //if (menu_current[which] == xmenu) cout << "Same " << menu << "\n";
    //if (which == 10) cout << which << " -> cached? " << (menu_current[which] == xmenu? "yes": "no") << LF;
    //cout << which << " -> cached? " << (menu_current[which] == xmenu) << LF;
    if (which < 10) {
      menu_current (which)= xmenu;
      //cout << "Cached " << menu << "\n";
      w= menu_cache [xmenu];
      return true;
    }
  }
  //if (which == 10) cout << which << " -> compute" << LF;
  //cout << which << " -> compute" << LF;
  menu_current (which)= xmenu;
  //cout << "Compute " << menu << "\n";
  object umenu= eval ("'" * menu);
  // the tools of a tool slot: a tool already shown keeps its opening, a new
  // one takes one, and the openings of the tools no longer shown end (#776)
  bool tools= (which >= 10);
  int  old_op= current_opening;
  if (tools) {
    tool_build_win  = this;
    tool_build_which= which;
    tool_build_seen = hashset<string> ();
    current_opening = 0;
  }
  if (which == 10 || which == 11) w= make_menu_widget (umenu, 400, 1000);
  else w= make_menu_widget (umenu);
  if (tools) {
    current_opening= old_op;
    end_tool_openings (as_string (id) * "|" * as_string (which) * "|", false);
    tool_build_win  = NULL;
    tool_build_which= -1;
    tool_build_seen = hashset<string> ();
  }
  // the cache is only read for the menus and the icon bars (which < 10):
  // the widgets of the side and bottom tools were kept for nothing, with
  // the buffers of their texmacs-input (#776)
  if (menu_caching && which < 10)
    if (as_bool (call ("cache-menu?", xmenu))) {
      //if (which == 10) cout << which << " -> cached" << LF;
      //cout << which << " -> cached" << LF;
      menu_cache (xmenu)= w;
    }
  return true;
}

void
tm_window_rep::menu_main (string menu) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (-1, menu, w))
    ::set_main_menu (wid, w);
}

void
tm_window_rep::menu_icons (int which, string menu) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (which, menu, w)) {
    if      (which == 0) set_main_icons (wid, w);
    else if (which == 1) set_mode_icons (wid, w);
    else if (which == 2) set_focus_icons (wid, w);
    else if (which == 3) set_user_icons (wid, w);
  }
}

void
tm_window_rep::side_tools (int which, string tools) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (10 + which, tools, w)) {
    if      (which == 0) set_side_tools (wid, w);
    else if (which == 1) set_left_tools (wid, w);
  }
}

void
tm_window_rep::bottom_tools (int which, string tools) {
  eval ("(lazy-initialize-force)");
  widget w;
  if (get_menu_widget (20 + which, tools, w)) {
    if      (which == 0) set_bottom_tools (wid, w);
    else if (which == 1) set_extra_tools (wid, w);
  }
}

void
tm_window_rep::set_header_flag (bool flag) {
  set_header_visibility (wid, flag);
}

void
tm_window_rep::set_icon_bar_flag (int which, bool flag) {
  if      (which == 0) set_main_icons_visibility (wid, flag);
  else if (which == 1) set_mode_icons_visibility (wid, flag);
  else if (which == 2) set_focus_icons_visibility (wid, flag);
  else if (which == 3) set_user_icons_visibility (wid, flag);
}

void
tm_window_rep::set_side_tools_flag (int which, bool flag) {
  if      (which == 0) set_side_tools_visibility (wid, flag);
  else if (which == 1) set_left_tools_visibility (wid, flag);
}

void
tm_window_rep::set_bottom_tools_flag (int which, bool flag) {
  if      (which == 0) set_bottom_tools_visibility (wid, flag);
  else if (which == 1) set_extra_tools_visibility (wid, flag);
}

bool
tm_window_rep::get_header_flag () {
  return get_header_visibility (wid);
}

bool
tm_window_rep::get_icon_bar_flag (int which) {
  if      (which == 0) return get_main_icons_visibility (wid);
  else if (which == 1) return get_mode_icons_visibility (wid);
  else if (which == 2) return get_focus_icons_visibility (wid);
  else if (which == 3) return get_user_icons_visibility (wid);
  else return false;
}

bool
tm_window_rep::get_side_tools_flag (int which) {
  if      (which == 0) return get_side_tools_visibility (wid);
  else if (which == 1) return get_left_tools_visibility (wid);
  else return false;
}

bool
tm_window_rep::get_bottom_tools_flag (int which) {
  if      (which == 0) return get_bottom_tools_visibility (wid);
  else if (which == 1) return get_extra_tools_visibility (wid);
  else return false;
}

/******************************************************************************
* The canvas
******************************************************************************/

void
tm_window_rep::set_window_zoom_factor (double zoom) {
  zoomf= retina_zoom * zoom;
  ::set_zoom_factor (wid, zoomf);
}

double
tm_window_rep::get_window_zoom_factor () {
  return zoomf / retina_zoom;
}

void
tm_window_rep::get_visible (SI& x1, SI& y1, SI& x2, SI& y2) {
  get_visible_part (wid, x1, y1, x2, y2);
}

void
tm_window_rep::get_extents (SI& x1, SI& y1, SI& x2, SI& y2) {
  ::get_extents (wid, x1, y1, x2, y2);
}

void
tm_window_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  ::set_extents (wid, x1, y1, x2, y2);
}

void
tm_window_rep::set_scrollbars (int i) {
  ::set_scrollbars_visibility (wid, i);
}

void
tm_window_rep::get_scroll_pos (SI& x, SI& y) {
  get_scroll_position (wid, x, y);
}

void
tm_window_rep::set_scroll_pos (SI x, SI y) {
  set_scroll_position (wid, x, y);
}

/******************************************************************************
* The footer as a status bar
******************************************************************************/

bool
tm_window_rep::get_footer_flag () {
  return get_footer_visibility (wid);
}

void
tm_window_rep::set_footer_flag (bool flag) {
  set_footer_visibility (wid, flag);
}

void
tm_window_rep::set_left_footer (string s) {
  ::set_left_footer (wid, s);
}

void
tm_window_rep::set_right_footer (string s) {
  ::set_right_footer (wid, s);
}

/******************************************************************************
* Interactive commands on the footer
******************************************************************************/

class ia_command_rep: public command_rep {
  tm_window_rep* win;
public:
  ia_command_rep (tm_window_rep* win2): win (win2) {}
  void apply () { win->interactive_return (); }
  tm_ostream& print (tm_ostream& out) { return out << "<command ia>"; }
};

bool
tm_window_rep::get_interactive_mode () {
  return ::get_interactive_mode (wid);
}

void
tm_window_rep::set_interactive_mode (bool flag) {
  ::set_interactive_mode (wid, flag);
}

void
tm_window_rep::interactive (string name, string type, array<string> def,
			    string& s, command cmd)
{
  if (get_interactive_mode ()) { s= "cancel"; return; }
  text_ptr = &s;
  call_back= cmd;
  widget tw = text_widget (translate (name), 0, black, false);
  widget inp= input_text_widget (tm_new<ia_command_rep> (this), type, def,
                                 WIDGET_STYLE_MINI);
  set_interactive_prompt (wid, tw);
  set_interactive_input (wid, inp);
  set_interactive_mode (true);
}

void
tm_window_rep::interactive_return () {
  if (text_ptr == NULL) return;
  *text_ptr= get_interactive_input (wid);
  text_ptr= NULL;
  set_interactive_mode (false);
  call_back ();
}

/******************************************************************************
* Other top level windows
******************************************************************************/

int
window_handle () {
  static int window_next= 1;
  pending_window_handle= window_next;
  // the widgets of the window are made in its opening (#776)
  dialog_opening (window_next)= new_opening ();
  current_opening= dialog_opening [window_next];
  return window_next++;
}

static void
window_register (int win, widget pww) {
  window_table (win)= pww;
  if (win == pending_window_handle) pending_window_handle= 0;
  if (current_opening != 0 && current_opening == dialog_opening [win])
    current_opening= 0;
}

void
window_create (int win, widget wid, string name, command quit) {
  widget pww;
  pww= plain_window_widget (wid, name, quit);
  window_register (win, pww);
}

void
window_create_plain (int win, widget wid, string name) {
  widget pww;
  pww= plain_window_widget (wid, name);
  window_register (win, pww);
}

void
window_create_popup (int win, widget wid, string name) {
  widget pww;
  pww= popup_window_widget (wid, name);
  window_register (win, pww);
}

void
window_create_tooltip (int win, widget wid, string name) {
  widget pww;
  pww= tooltip_window_widget (wid, name);
  window_register (win, pww);
}

/*
FIXME: this old implementation does not work in the presence
of texmacs_input widgets.  The current hack remedies this situation
by explicitly signalling the widget destruction slot before
the actual destruction of the widget.  This is still not sufficient
in the case of Qt though and also might cause the desruction slot
to be signalled twice.

void
window_delete (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  window_table->reset (win);
  destroy_window_widget (pww);
}
*/

void
window_delete (int win) {
  static hashmap<int,bool> busy (false);
  if (busy->contains (win)) return;
  busy (win)= true;
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  window_table->reset (win);
  send_destroy (pww);
  destroy_window_widget (pww);
  // the fields of the window end with it (#776)
  if (dialog_opening->contains (win)) {
    int op= dialog_opening [win];
    dialog_opening->reset (win);
    end_opening (op);
  }
  busy (win)= false;
}

void
window_show (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_visibility (pww, true);
}

void
window_hide (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_visibility (pww, false);
}

scheme_tree
window_get_size (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  SI w, h;
  get_size (pww, w, h);
  return tuple (as_string (w/PIXEL), as_string (h/PIXEL));
}

void
window_set_size (int win, int w, int h) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_size (pww, w*PIXEL, h*PIXEL);
}

scheme_tree
window_get_position (int win) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  SI x, y;
  get_position (pww, x, y);
  return tuple (as_string (x/PIXEL), as_string (y/PIXEL));
}

void
window_set_position (int win, int x, int y) {
  ASSERT (window_table->contains (win), "window does not exist");
  widget pww= window_table [win];
  set_position (pww, x*PIXEL, y*PIXEL);
}

void
windows_delayed_refresh (int ms) {
  refresh_time= texmacs_time () + ms;
}

void
windows_refresh (string kind) {
  if (kind == "auto" && texmacs_time () < refresh_time) return;
  iterator<int> it= iterate (window_table);
  while (it->busy ()) {
    int id= it->next ();
    send_refresh (window_table[id], kind);
#ifdef X11TEXMACS
    if (kind == "auto") refresh_size (window_table[id], false);
#endif
  }
  if (kind == "auto") windows_delayed_refresh (1000000000);
}
