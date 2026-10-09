
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
#include "converter.hpp"
#include "iterator.hpp"
#include "boot.hpp"
#include "convert.hpp"
#include "tm_window.hpp"
#include "tm_buffer.hpp"
#include "new_window.hpp"
#include "new_view.hpp"
#include "new_buffer.hpp"
#include <unistd.h>
#include <stdlib.h>
#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

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

static int next_window_id= 1;

class view_window_rep: public no_widget_rep {
public:
  int     id;    // the number of the window for the page
  widget  view;
  command quit;
  view_window_rep (command quit2): id (next_window_id++), quit (quit2) {
    nr_windows++; }
  ~view_window_rep () { nr_windows--; }
  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
};

static void tau_post_json (const char* kind, string part, string json);
static hashmap<int,bool> bar_visibility (true);

// the bars of a window, for the page
static string
visibility_part (slot s) {
  switch (s) {
  case SLOT_HEADER_VISIBILITY: return "menu";
  case SLOT_MAIN_ICONS_VISIBILITY: return "icons-0";
  case SLOT_MODE_ICONS_VISIBILITY: return "icons-1";
  case SLOT_FOCUS_ICONS_VISIBILITY: return "icons-2";
  case SLOT_USER_ICONS_VISIBILITY: return "icons-3";
  case SLOT_SIDE_TOOLS_VISIBILITY: return "side-0";
  case SLOT_BOTTOM_TOOLS_VISIBILITY: return "bottom-0";
  case SLOT_FOOTER_VISIBILITY: return "footer";
  default: return "";
  }
}

// what the core says of the scrolling to the window is for the view
static bool
is_canvas_slot (slot s) {
  return s == SLOT_EXTENTS || s == SLOT_SCROLL_POSITION ||
         s == SLOT_VISIBLE_PART || s == SLOT_ZOOM_FACTOR;
}

void
view_window_rep::send (slot s, blackbox val) {
  if (is_canvas_slot (s) && !is_nil (view)) view->send (s, val);
  else if (s == SLOT_LEFT_FOOTER || s == SLOT_RIGHT_FOOTER) {
    string text= cork_to_utf8 (open_box<string> (val));
    tau_post_json ("footer", s == SLOT_LEFT_FOOTER? "left": "right",
                   "{\"text\":" * scm_quote (text) * "}");
  }
  else if (visibility_part (s) != "") {
    bool flag= open_box<bool> (val);
    bar_visibility ((int) s)= flag;
    tau_post_json ("visible", visibility_part (s),
                   flag? string ("{\"visible\":true}")
                       : string ("{\"visible\":false}"));
  }
}

blackbox
view_window_rep::query (slot s, int type_id) {
  // "attached to a window" is a non zero identifier (is_attached)
  if (s == SLOT_IDENTIFIER) return close_box<int> (1);
  if (is_canvas_slot (s) && !is_nil (view)) return view->query (s, type_id);
  if (visibility_part (s) != "")
    return close_box<bool> (bar_visibility [(int) s]);
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

static void tau_post_view (int id, int window, const char* what);

void
view_window_rep::write (slot s, blackbox index, widget w) {
  (void) index;
  if (s == SLOT_SCROLLABLE || s == SLOT_CANVAS) {
    // the view which was here is detached, and the page is told of the
    // view which is shown now
    simple_widget_rep* old= dynamic_cast<simple_widget_rep*> (view.rep);
    if (old != NULL) old->shown= false;
    view= w;
    simple_widget_rep* v= dynamic_cast<simple_widget_rep*> (w.rep);
    if (v != NULL) {
      v->shown= true;
      if (v->is_editor_widget ()) tau_post_view (v->id, id, "shown");
    }
  }
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

/******************************************************************************
* The messages to the page (docs/tau-design.md, "The protocol")
*******************************************************************************
* The program is given a function tauPost (message, transferables) when it
* is started in a worker (misc/tau/web/tau-worker.js). Without it (under
* node) nothing is sent.
******************************************************************************/

#ifdef __EMSCRIPTEN__
EM_JS (void, tau_js_view, (int view, int window, const char* what), {
  if (Module.tauPost) Module.tauPost ({ t: "view", view: view, window: window,
                                        what: UTF8ToString (what) });
});

EM_JS (void, tau_js_paint, (int view, int counter, int w, int h,
                            const unsigned char* pixels,
                            int ew, int eh, int sx, int sy,
                            int cx, int cy), {
  if (!Module.tauPost) return;
  // a copy of the pixels, which is transferred to the page
  var data= HEAPU8.slice (pixels, pixels + 4 * w * h);
  Module.tauPost ({ t: "paint", view: view, place: counter,
                    width: w, height: h, pixels: data.buffer,
                    extents: { width: ew, height: eh },
                    scroll: { x: sx, y: sy },
                    caret: { x: cx, y: cy } }, [data.buffer]);
});
#else
static void tau_js_view (int view, int window, const char* what) {
  (void) view; (void) window; (void) what; }
static void tau_js_paint (int view, int counter, int w, int h,
                          const unsigned char* pixels,
                          int ew, int eh, int sx, int sy, int cx, int cy) {
  (void) view; (void) counter; (void) w; (void) h; (void) pixels;
  (void) ew; (void) eh; (void) sx; (void) sy; (void) cx; (void) cy; }
#endif

static void
tau_post_view (int id, int window, const char* what) {
  tau_js_view (id, window, what);
}

#ifdef __EMSCRIPTEN__
EM_JS (void, tau_js_json, (const char* kind, const char* part,
                           const char* json), {
  if (!Module.tauPost) return;
  var m= JSON.parse (UTF8ToString (json));
  m.t= UTF8ToString (kind);
  m.part= UTF8ToString (part);
  Module.tauPost (m);
});
#else
static void tau_js_json (const char* kind, const char* part,
                         const char* json) {
  (void) kind; (void) part; (void) json; }
#endif

static void
tau_post_json (const char* kind, string part, string json) {
  c_string _part (part);
  c_string _json (json);
  tau_js_json (kind, _part, _json);
}

// a menu, an icon bar or the tools of a window, described to the page:
// which is -1 for the menu bar, 0 to 3 for the icon bars, 10 and more for
// the side tools, 20 and more for the bottom tools (tm_window.cpp)
void
tau_chrome (int which, object menu) {
  string part;
  if (which < 0) part= "menu";
  else if (which < 10) part= "icons-" * as_string (which);
  else if (which < 20) part= "side-" * as_string (which - 10);
  else part= "bottom-" * as_string (which - 20);
  string json= as_string (call ("tau-serialize-part", object (part), menu));
  tau_post_json ("chrome", part, json);
}

/******************************************************************************
* The buffers and the windows, for the tabs and the panes of the page
*******************************************************************************
* The page shows the views where it wants; what it needs to know is which
* documents there are and which one each window of the core shows. This is
* said whenever it changes, whatever changed it.
******************************************************************************/

static string
json_quote (string s) {
  // s is in the encoding of TeXmacs
  string u= cork_to_utf8 (s), r= "\"";
  for (int i=0; i<N(u); i++) {
    unsigned char c= (unsigned char) u[i];
    if (c == '"' || c == '\\') r << '\\' << u[i];
    else if (c == '\n') r << "\\n";
    else if (c < 32) r << ' ';
    else r << u[i];
  }
  return r * "\"";
}

static view_window_rep*
window_widget (url win) {
  tm_window w= concrete_window (win);
  if (w == NULL) return NULL;
  return dynamic_cast<view_window_rep*> (w->wid.rep);
}

static url
window_of_number (int id) {
  array<url> l= windows_list ();
  for (int i=0; i<N(l); i++) {
    view_window_rep* w= window_widget (l[i]);
    if (w != NULL && w->id == id) return l[i];
  }
  return url_none ();
}

static string
tau_state () {
  string r= "{\"buffers\":[";
  array<url> bs= get_all_buffers ();
  bool first= true;
  for (int i=0; i<N(bs); i++) {
    if (!as_bool (call ("buffer-in-menu?", object (bs[i])))) continue;
    string title= get_title_buffer (bs[i]);
    if (title == "") title= as_string (tail (bs[i]));
    if (!first) r << ",";
    first= false;
    r << "{\"name\":" << json_quote (as_string (bs[i]))
      << ",\"title\":" << json_quote (title)
      << ",\"modified\":" << (buffer_modified (bs[i])? "true": "false")
      << "}";
  }
  r << "],\"windows\":[";
  array<url> ws= windows_list ();
  first= true;
  for (int i=0; i<N(ws); i++) {
    view_window_rep* w= window_widget (ws[i]);
    if (w == NULL) continue;
    simple_widget_rep* v= dynamic_cast<simple_widget_rep*> (w->view.rep);
    if (!first) r << ",";
    first= false;
    r << "{\"window\":" << as_string (w->id)
      << ",\"view\":" << as_string (v == NULL? 0: v->id)
      << ",\"buffer\":" << json_quote (as_string (window_to_buffer (ws[i])))
      << "}";
  }
  r << "]}";
  return r;
}

/******************************************************************************
* A turn of the core
*******************************************************************************
* The server of TeXmacs is the loop of the worker: a message of the page is
* handled, then the pending commands are run and the views which changed
* are drawn and sent, at most once each. The same happens at regular
* intervals without a message, for the delayed commands.
******************************************************************************/

static void
tau_turn () {
  array<simple_widget_rep*> views;
  iterator<pointer> it= iterate (simple_widget_rep::all_widgets);
  while (it->busy ()) views << (simple_widget_rep*) it->next ();
  // (a view which is not shown has no window: the editor cannot be asked)
  for (int i=0; i<N(views); i++)
    if (views[i]->shown) views[i]->notify_resize ();
  if (the_interpose_handler != NULL) the_interpose_handler ();
  for (int i=0; i<N(views); i++) {
    simple_widget_rep* v= views[i];
    if (!simple_widget_rep::all_widgets->contains ((pointer) v)) continue;
    if (!v->shown || !v->is_editor_widget () || v->ren == NULL) continue;
    if (!v->repaint ()) continue;
    int ew, eh, sx, sy, cx, cy;
    v->extents_in_pixels (ew, eh, sx, sy);
    v->cursor_in_pixels (cx, cy);
    tau_js_paint (v->id, v->place_counter, v->px_w, v->px_h, v->pixels (),
                  ew, eh, sx, sy, cx, cy);
  }
  // what Scheme has to say to the page (menu-serial.scm)
  string out= as_string (call ("tau-outbox"));
  if (N(out) != 0) tau_post_json ("batch", "", out);
  static string last_state;
  string state= tau_state ();
  if (state != last_state) {
    last_state= state;
    tau_post_json ("buffers", "", state);
  }
}

// the parts of the interface of a given kind are described again
void
tau_refresh (string kind) {
  call ("tau-refresh", object (kind));
}

void
gui_start_loop () {
  if (headless_mode) {
    // without a page there are no events: the delayed commands are run
    // until one of them quits (tm_server_rep::quit leaves the program)
    while (true) {
      if (the_interpose_handler != NULL) the_interpose_handler ();
      usleep (1000);
    }
  }
#ifdef __EMSCRIPTEN__
  // In a worker the program gives control back: the messages of the page
  // call the functions below, and a turn is made at regular intervals for
  // the delayed commands. Control does not come back here: the stack is
  // unwound (the server of TeXmacs_main is not on it).
  emscripten_set_main_loop (tau_turn, 20, true);
#endif
}

/******************************************************************************
* The messages of the page
******************************************************************************/

#ifndef EMSCRIPTEN_KEEPALIVE
#define EMSCRIPTEN_KEEPALIVE
#endif

extern "C" {

EMSCRIPTEN_KEEPALIVE
void
tau_place (int view, int w, int h, double density, int counter) {
  simple_widget_rep* v= simple_widget_rep::find (view);
  if (v == NULL) return;
  v->set_place (w, h, density, counter);
  tau_turn ();
}

EMSCRIPTEN_KEEPALIVE
void
tau_scroll_by (int view, int dx, int dy) {
  simple_widget_rep* v= simple_widget_rep::find (view);
  if (v == NULL) return;
  v->scroll_by (dx, dy);
  tau_turn ();
}

EMSCRIPTEN_KEEPALIVE
void
tau_focus (int view, int has_focus) {
  simple_widget_rep* v= simple_widget_rep::find (view);
  if (v == NULL) return;
  v->has_focus= (has_focus != 0);
  v->handle_keyboard_focus (has_focus != 0, texmacs_time ());
  tau_turn ();
}

EMSCRIPTEN_KEEPALIVE
void
tau_key (int view, const char* key) {
  // a key in the notation of TeXmacs ("a", "C-x", "S-left"), in UTF-8
  simple_widget_rep* v= simple_widget_rep::find (view);
  if (v == NULL) return;
  v->handle_keypress (utf8_to_cork (string (key)), texmacs_time ());
  tau_turn ();
}

EMSCRIPTEN_KEEPALIVE
void
tau_invoke (int n) {
  // the action of an entry of a menu, by its number
  call ("tau-invoke", object (n));
  tau_turn ();
}

// the value of an input: args are the arguments of its command, written
// in Scheme by the worker
EMSCRIPTEN_KEEPALIVE
void
tau_answer (int n, const char* args) {
  eval ("(tau-answer " * as_string (n) * " " * string (args) * ")");
  tau_turn ();
}

// the user closed a dialog
EMSCRIPTEN_KEEPALIVE
void
tau_closed (int id) {
  call ("tau-dialog-closed", object (id));
  tau_turn ();
}

EMSCRIPTEN_KEEPALIVE
void
tau_expand (int n) {
  // the contents of a submenu, by its number
  string json= as_string (call ("tau-expand", object (n)));
  tau_post_json ("contents", as_string (n), json);
}

// What the tabs and the panes ask: to show a buffer in a window ("switch"),
// to close a buffer ("close"), a new document in a window ("new"), to close
// a window ("close-window"). The buffer is named as in the state.
EMSCRIPTEN_KEEPALIVE
void
tau_buffer (const char* what_c, int window, const char* name_c) {
  string what (what_c);
  url win= window_of_number (window);
  url name= url_system (utf8_to_cork (string (name_c)));
  if (is_none (win)) return;
  if (what == "close-window") {
    view_window_rep* w= window_widget (win);
    if (w != NULL && !is_nil (w->quit)) w->quit ();
  }
  else {
    if (win != get_current_window ()) switch_to_window (win);
    if (what == "switch") switch_to_buffer (name);
    else if (what == "close") call ("tau-close-buffer", object (name));
    else if (what == "new") call ("new-document");
  }
  tau_turn ();
}

// The file which the page was asked for (tau-files.scm), or a file which
// was dropped on the page (ticket 0): it is in the file system now
EMSCRIPTEN_KEEPALIVE
void
tau_file (int ticket, const char* path) {
  call ("tau-file-chosen", object (ticket),
        object (url_system (utf8_to_cork (string (path)))));
  tau_turn ();
}

void tau_paste_text (string text, string html);

// What the user pastes in a view, from the clipboard of the browser
EMSCRIPTEN_KEEPALIVE
void
tau_paste (int view, const char* text, const char* html) {
  simple_widget_rep* v= simple_widget_rep::find (view);
  if (v == NULL) return;
  tau_paste_text (string (text), string (html));
  // (the view which pastes has the keyboard: it is the current one)
  eval ("(clipboard-paste \"primary\")");
  tau_turn ();
}

EMSCRIPTEN_KEEPALIVE
void
tau_mouse (int view, const char* kind, int x, int y, int mods) {
  // kind as in TeXmacs ("press-left", "move", "release-left"...); the
  // position in pixels of the canvas, from its top left corner
  simple_widget_rep* v= simple_widget_rep::find (view);
  if (v == NULL) return;
  SI dx= x, dy= y;
  v->to_document (dx, dy);
  v->handle_mouse (string (kind), dx, dy, mods, texmacs_time ());
  tau_turn ();
}

} // extern "C"

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
* The clipboard
*******************************************************************************
* What is copied is kept here as a tree, and its text is given to the page
* for the clipboard of the browser. The browser gives its clipboard only
* when the user pastes: the page sends the text then, and it replaces what
* is kept here unless it is the text which came from here.
******************************************************************************/

static hashmap<string,tree>   selection_trees ("none");
static hashmap<string,string> selection_strings ("");
static string                 copied_text;  // what the page was given

static string
without_returns (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (s[i] != '\r') r << s[i];
  return r;
}

bool
set_selection (string cb, tree t,
               string s, string sv, string sh, string format) {
  (void) sh; (void) format;
  selection_trees (cb)= copy (t);
  selection_strings (cb)= s;
  if (cb == "primary") {
    copied_text= without_returns (sv);
    string u= "\"";
    for (int i=0; i<N(sv); i++) {
      unsigned char c= (unsigned char) sv[i];
      if (c == '"' || c == '\\') u << '\\' << sv[i];
      else if (c == '\n') u << "\\n";
      else if (c == '\t') u << "\\t";
      else if (c < 32) continue;
      else u << sv[i];
    }
    tau_post_json ("clipboard", "", "{\"text\":" * u * "\"}");
  }
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

void
tau_paste_text (string text, string html) {
  // as the other interfaces read the clipboard of the system: HTML when
  // there is some, else text
  text= without_returns (text);
  if (text == copied_text && selection_trees->contains ("primary")) return;
  string s;
  if (N(html) != 0) {
    s= as_string (call ("convert", html, "html-snippet", "texmacs-snippet"));
    tree t= as_tree (call ("convert", s, "texmacs-snippet", "texmacs-tree"));
    t= default_with_simplify (t);
    s= as_string (call ("convert", t, "texmacs-tree", "texmacs-snippet"));
  }
  else
    s= as_string (call ("convert", text, "verbatim-snippet",
                        "texmacs-snippet"));
  selection_trees ("primary")= tuple ("extern", s);
  selection_strings ("primary")= s;
  copied_text= text;
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
