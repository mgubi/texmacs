
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
#include "analyze.hpp"
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

// The core has no windows (Texmacs/tm_window.hpp): a view is shown at a
// place, which is a number, and the bars around the views are of the page.
// What the core says of them goes straight to the page.

static void tau_post_json (const char* kind, string part, string json);
static void tau_post_view (int id, int window, const char* what);
static hashmap<string,bool> part_visibility (true);

// a bar or a tool is shown or hidden: the bars are there until they are
// hidden, the tools once they are shown
static bool
is_tool_part (string part) {
  return starts (part, "side-") || starts (part, "bottom-");
}

bool
tau_get_visible (string part) {
  if (!part_visibility->contains (part)) return !is_tool_part (part);
  return part_visibility [part];
}

void
tau_set_visible (string part, bool flag) {
  part_visibility (part)= flag;
  tau_post_json ("visible", part, flag? string ("{\"visible\":true}")
                                      : string ("{\"visible\":false}"));
}

void
tau_fullscreen (bool on) {
  tau_post_json ("fullscreen", "", on? string ("{\"on\":true}")
                                     : string ("{\"on\":false}"));
}

void
tau_footer (string side, string text) {
  tau_post_json ("footer", side,
                 "{\"text\":" * scm_quote (cork_to_utf8 (text)) * "}");
}

// A view is shown at a place, or not any more (attach_view and detach_view
// in Texmacs/Data/new_view.cpp). The page is told of the views of its
// panes; where a view in a dialog is, the description of the dialog says.
void
tau_view_shown (editor_rep* ed, int place, bool shown) {
  simple_widget_rep* v= (simple_widget_rep*) ed;
  v->shown= shown;
  if (shown && place > 0) tau_post_view (v->id, place, "shown");
}

void
tau_place_deleted (int place) {
  // (the page learns it from the state of the buffers and the places)
  (void) place;
}

widget
texmacs_widget (int mask, command quit) {
  (void) mask; (void) quit;
  return no_widget ();
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
                            int x1, int y1, int x2, int y2,
                            int ew, int eh, int sx, int sy,
                            int cx, int cy), {
  if (!Module.tauPost) return;
  // a copy of the pixels which changed (a rectangle of the canvas), which
  // is transferred to the page
  var dw= x2 - x1, dh= y2 - y1, data;
  if (dw == w && dh == h) data= HEAPU8.slice (pixels, pixels + 4 * w * h);
  else {
    data= new Uint8Array (4 * dw * dh);
    for (var row= 0; row < dh; row++) {
      var from= pixels + 4 * ((y1 + row) * w + x1);
      data.set (HEAPU8.subarray (from, from + 4 * dw), 4 * row * dw);
    }
  }
  Module.tauPost ({ t: "paint", view: view, place: counter,
                    width: w, height: h, pixels: data.buffer,
                    x: x1, y: y1, w: dw, h: dh,
                    extents: { width: ew, height: eh },
                    scroll: { x: sx, y: sy },
                    caret: { x: cx, y: cy } }, [data.buffer]);
});
#else
static void tau_js_view (int view, int window, const char* what) {
  (void) view; (void) window; (void) what; }
static void tau_js_paint (int view, int counter, int w, int h,
                          const unsigned char* pixels,
                          int x1, int y1, int x2, int y2,
                          int ew, int eh, int sx, int sy, int cx, int cy) {
  (void) view; (void) counter; (void) w; (void) h; (void) pixels;
  (void) x1; (void) y1; (void) x2; (void) y2;
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

// the context menu of a view: a part as the others, which the page shows
// under the pointer
void
tau_popup (string menu, int view) {
  object umenu= eval ("'(vertical (link " * menu * "))");
  string json= as_string (call ("tau-serialize-part", object ("popup"), umenu));
  tau_post_json ("popup", as_string (view), json);
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

static string
tau_state () {
  // (and where the main and the mode icon bars are, which is of the page)
  string r= "{\"bars\":" * json_quote (get_preference ("icon bars")) *
            ",\"buffers\":[";
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
    int place= url_place (ws[i]);
    tm_view vw= place_view (place);
    if (vw == NULL) continue;
    simple_widget_rep* v= (simple_widget_rep*) vw->ed.operator -> ();
    if (!first) r << ",";
    first= false;
    r << "{\"window\":" << as_string (place)
      << ",\"view\":" << as_string (v->id)
      << ",\"buffer\":" << json_quote (as_string (vw->buf->buf->name))
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
    if (!v->shown || v->ren == NULL) continue;
    if (!v->repaint ()) continue;
    int ew, eh, sx, sy, cx, cy;
    v->extents_in_pixels (ew, eh, sx, sy);
    v->cursor_in_pixels (cx, cy);
    tau_js_paint (v->id, v->place_counter, v->px_w, v->px_h, v->pixels (),
                  v->drawn_x1, v->drawn_y1, v->drawn_x2, v->drawn_y2,
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
  // (what is not an editor has no window which shows it: the page does)
  if (!v->is_editor_widget ()) v->shown= true;
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

// The text which the keyboard made (typed, composed with dead keys or by an
// input method), in UTF-8: one key for each character, by the names which
// TeXmacs has for them (as the other ports: cork_key of the Vue port)
EMSCRIPTEN_KEEPALIVE
void
tau_text (int view, const char* text) {
  simple_widget_rep* v= simple_widget_rep::find (view);
  if (v == NULL) return;
  string r= utf8_to_cork (string (text));
  int pos= 0;
  while (pos < N(r)) {
    int start= pos;
    tm_char_forwards (r, pos);
    if (pos <= start) pos= start + 1;
    string k= r (start, pos);
    int n= N(k);
    if (n >= 3 && k[0] == '<' && k[1] != '#' && k[n-1] == '>') k= k (1, n-1);
    if (k == "less") k= "<";
    else if (k == "gtr") k= ">";
    else if (k == " ") k= "space";
    else if (k == "\n" || k == "\r") k= "return";
    else if (k == "\t") k= "tab";
    if (!simple_widget_rep::all_widgets->contains ((pointer) v)) break;
    v->handle_keypress (k, texmacs_time ());
  }
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

// a Scheme command of the tests: the worker passes it on only when the
// page was opened with ?debug (tau-worker.js)
EMSCRIPTEN_KEEPALIVE
void
tau_scheme (const char* code) {
  exec_delayed (scheme_cmd (string (code)));
  tau_turn ();
}

// the browser left the full screen (the user pressed Escape): TeXmacs
// leaves its full screen mode too
EMSCRIPTEN_KEEPALIVE
void
tau_fullscreen_left () {
  eval ("(cond ((full-screen-edit?) (toggle-full-screen-edit-mode))"
        "      ((full-screen?) (toggle-full-screen-mode)))");
  tau_turn ();
}

// Scheme for the JavaScript of the worker (misc/wasm/javascript.js, the
// global TeXmacs of the JavaScript plugin), under the names which that
// script calls in the browser build of TeXmacs. The text of a command is
// in UTF-8: what is not ASCII goes to the encoding of TeXmacs, the rest
// stays as it is (utf8_to_cork would make symbols of "<" and ">").
static string
scheme_text (string s) {
  string r;
  int i= 0, n= N(s);
  while (i < n) {
    if (((unsigned char) s[i]) < 128) { r << s[i]; i++; continue; }
    int j= i + 1;
    while (j < n && (((unsigned char) s[j]) & 0xC0) == 0x80) j++;
    r << utf8_to_cork (s (i, j));
    i= j;
  }
  return r;
}

// a command which the loop runs, as the delayed commands
EMSCRIPTEN_KEEPALIVE
void
vue_web_scheme (const char* cmd) {
  exec_delayed (scheme_cmd (scheme_text (string (cmd))));
}

// an expression evaluated now: its value as text (a string as it is, the
// rest as object->string writes it, an error as (error ...)). Not from
// code which TeXmacs runs. The text stays until the next call.
EMSCRIPTEN_KEEPALIVE
const char*
vue_web_scheme_eval (const char* cmd) {
  static char* last= NULL;
  string expr= "(let ((r (catch #t (lambda () (begin " *
               scheme_text (string (cmd)) *
               "\n)) (lambda args (cons 'error args)))))"
               " (if (string? r) r (object->string r)))";
  string r;
  try {
    object o= eval (expr);
    r= is_string (o) ? as_string (o) : string ("");
  }
  catch (string msg) {
    r= "(error " * scm_quote (msg) * ")";
  }
  if (last != NULL) tm_delete_array (last);
  last= as_charp (cork_to_utf8 (r));
  return last;
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

// What the tabs and the panes ask, for a place (which the page calls a
// window): to show a buffer there ("switch"), to close a buffer ("close"),
// a new document there ("new"), to give the place up ("close-window"). The
// buffer is named as in the state.
EMSCRIPTEN_KEEPALIVE
void
tau_buffer (const char* what_c, int place, const char* name_c) {
  string what (what_c);
  url win= place_url (place);
  url name= url_system (utf8_to_cork (string (name_c)));
  if (!is_place (place) || place_view (place) == NULL) return;
  if (what == "close-window") {
    object cmd= list_object (symbol_object ("safely-kill-place"),
                             object (win));
    exec_delayed (scheme_cmd (cmd));
  }
  else {
    if (win != get_current_window ()) {
      switch_to_window (win);
      window_focus (win);
    }
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
  eval ("(kbd-paste)");
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
  // (nothing there is the tree "none", as in the other ports)
  t= "none"; s= "";
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
  // (nothing from the page: what is kept here is pasted)
  if (N(text) == 0 && N(html) == 0) return;
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

// a widget with what is done when nobody keeps it any more: the window of
// a view in a dialog, whose buffer is closed then
class wrapped_widget_rep: public no_widget_rep {
public:
  widget  w;
  command quit;
  wrapped_widget_rep (widget w2, command q): w (w2), quit (q) {}
  ~wrapped_widget_rep () { if (!is_nil (quit)) quit (); }
};

widget wrapped_widget (widget a1, command a2) {
  return widget (tm_new<wrapped_widget_rep> (a1, a2)); }

// The view which a widget made by Scheme is (texmacs-output) or shows
// (texmacs-input), with the size it wishes: for the description of a
// dialog (menu-serial.scm)
array<SI>
tau_widget_view (widget wid) {
  array<SI> ret;
  widget_rep* r= wid.rep;
  wrapped_widget_rep* ww= dynamic_cast<wrapped_widget_rep*> (r);
  if (ww != NULL) r= ww->w.rep;
  simple_widget_rep* v= dynamic_cast<simple_widget_rep*> (r);
  SI w= 0, h= 0;
  if (v != NULL && !v->is_editor_widget ()) v->handle_get_size_hint (w, h);
  ret << w << h << (v == NULL? 0: v->id);
  return ret;
}

widget xpm_widget (url a1) {
  (void) a1;
  return no_widget (); }
