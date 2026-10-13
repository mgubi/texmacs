
/******************************************************************************
* MODULE     : tau_shell.cpp
* DESCRIPTION: The desktop program of Tau: Vue for the interface, and the
*              core in a process of its own
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* What the page is in a browser (docs/tau-design.md, "On the desktop"): the
* windows, the menus, the bars, the keyboard and the pointer, with the core
* of Tau (tau_core, the editor and the typesetter) in another process which
* is told what the user does and says what to show (tau_native.cpp has the
* messages). The interface is Vue, the GUI of TeXmacs on SDL (Plugins/Vue),
* built with the rest of TeXmacs as usual (misc/tau/shell/Makefile, the
* switch TAU_SHELL), of which this program uses the kernel, the fonts, the
* renderer and the widgets, and neither the editor nor its Scheme: the
* interpreter is there for the values which the widgets pass to their
* commands, with a few definitions for what Vue asks of it.
*
* - A view of the core is a canvas of Vue (tau_view_rep) which draws the
*   pixels the core sends and sends it the keys and the pointer.
* - A menu or a bar is made from its description (JSON, as the page gets
*   it) with the widgets of TeXmacs (widget.hpp), which Vue implements: an
*   entry is a button whose command sends the number of its action, a
*   submenu a button whose contents are asked for when it opens.
******************************************************************************/

#ifdef TAU_SHELL

#include "hashset.hpp"
#include "hashmap.hpp"
#include "tree.hpp"
#include "Vue/vue_widget.hpp"
#include "Vue/vue_gui.hpp"
#include "MuPDF/mupdf_picture.hpp"
#include "widget.hpp"
#include "message.hpp"
#include "gui.hpp"
#include "scheme.hpp"
#include "promise.hpp"
#include "command.hpp"
#include "converter.hpp"
#include "sys_utils.hpp"
#include "renderer.hpp"
#include "url.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "iterator.hpp"
#include <SDL3/SDL.h>
#include <mupdf/fitz.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <string>
#include <vector>
#include <deque>
#include <mutex>
#include <condition_variable>
#include <thread>
#include <chrono>

/******************************************************************************
* What the rest of TeXmacs asks of the GUI of Tau and of its windows, which
* are not here
******************************************************************************/

class editor_rep;
void tau_chrome (int which, object menu) { (void) which; (void) menu; }
void tau_footer (string side, string text) { (void) side; (void) text; }
void tau_refresh (string kind) { (void) kind; }
void tau_fullscreen (bool on) { (void) on; }
void tau_view_shown (editor_rep* ed, int place, bool shown) {
  (void) ed; (void) place; (void) shown; }
bool tau_get_visible (string part) { (void) part; return true; }
void tau_set_visible (string part, bool flag) { (void) part; (void) flag; }
void tau_place_deleted (int place) { (void) place; }
widget make_menu_widget (object wid) { (void) wid; return empty_widget (); }
bool menu_caching= true;
void notify_window_move (string name, SI x, SI y) {
  (void) name; (void) x; (void) y; }
void notify_window_resize (string name, SI w, SI h) {
  (void) name; (void) w; (void) h; }
void notify_window_destroy (string name) { (void) name; }

/******************************************************************************
* The core: a process, what it is told and what it says
******************************************************************************/

struct core_frame {
  char tag[5];
  std::vector<unsigned char> body;
};

static int                     core_in= -1;     // what the core is told
static pid_t                   core_pid= 0;
static std::mutex              frames_lock;
static std::condition_variable frames_came;
static std::deque<core_frame>  frames;
static Uint32                  frame_event= 0;  // wakes the loop of Vue
static bool                    core_gone= false;

static bool
read_all (int fd, void* data, size_t n) {
  char* p= (char*) data;
  while (n > 0) {
    ssize_t r= read (fd, p, n);
    if (r <= 0) return false;
    p += r; n -= (size_t) r;
  }
  return true;
}

static void
core_reader (int fd) {
  while (true) {
    char head[8];
    if (!read_all (fd, head, 8)) break;
    uint32_t len;
    memcpy (&len, head + 4, 4);
    core_frame f;
    memcpy (f.tag, head, 4); f.tag[4]= 0;
    f.body.resize (len);
    if (len > 0 && !read_all (fd, f.body.data (), len)) break;
    {
      std::lock_guard<std::mutex> g (frames_lock);
      frames.push_back (std::move (f));
    }
    frames_came.notify_all ();
    SDL_Event e; SDL_zero (e); e.type= frame_event;
    SDL_PushEvent (&e);
  }
  {
    std::lock_guard<std::mutex> g (frames_lock);
    core_gone= true;
  }
  frames_came.notify_all ();
  SDL_Event e; SDL_zero (e); e.type= frame_event;
  SDL_PushEvent (&e);
}

// a line for the core (see tau_native.cpp)
static void
core_say (std::string line) {
  if (core_in < 0) return;
  static bool debug= (getenv ("TAU_SHELL_DEBUG") != NULL);
  if (debug && line.compare (0, 11, "mouse ") != 0 && line.find (" move ") == std::string::npos)
    fprintf (stderr, "tau_shell] > %s\n", line.c_str ());
  line.push_back ('\n');
  const char* p= line.data (); size_t n= line.size ();
  while (n > 0) {
    ssize_t r= write (core_in, p, n);
    if (r <= 0) return;
    p += r; n -= (size_t) r;
  }
}

static std::string
std_of (string s) {
  c_string c (s);
  return std::string ((char*) c);
}

static std::string
escaped (std::string s) {
  std::string r;
  for (char c : s)
    if (c == '\n') r += "\\n";
    else if (c == '\\') r += "\\\\";
    else r.push_back (c);
  return r;
}

static bool
core_start (url exe, int argc, char** argv) {
  int in_pipe[2], out_pipe[2];
  if (pipe (in_pipe) != 0 || pipe (out_pipe) != 0) return false;
  signal (SIGPIPE, SIG_IGN);
  std::string prog= std_of (as_string (exe));
  core_pid= fork ();
  if (core_pid == 0) {
    dup2 (in_pipe[0], 0);
    close (in_pipe[1]); close (out_pipe[0]);
    setenv ("TAU_FD", std::to_string (out_pipe[1]).c_str (), 1);
#ifdef __APPLE__
    setenv ("TEXMACS_WEB_PLATFORM", "macos", 0);
#endif
    std::vector<char*> args;
    args.push_back ((char*) prog.c_str ());
    for (int i= 1; i < argc; i++) args.push_back (argv[i]);
    args.push_back (NULL);
    execv (prog.c_str (), args.data ());
    perror (prog.c_str ());
    _exit (127);
  }
  close (in_pipe[0]); close (out_pipe[1]);
  core_in= in_pipe[1];
  frame_event= SDL_RegisterEvents (1);
  std::thread (core_reader, out_pipe[0]).detach ();
  return true;
}

/******************************************************************************
* The descriptions of the core: JSON
******************************************************************************/

struct json {
  enum kind_t { NUL, BOOL, NUM, STR, ARR, OBJ } kind= NUL;
  bool        b= false;
  double      n= 0;
  std::string s;
  std::vector<json> a;
  std::vector<std::pair<std::string,json> > o;

  const json& operator [] (const char* key) const {
    static json none;
    for (auto& p : o) if (p.first == key) return p.second;
    return none;
  }
  bool has (const char* key) const {
    for (auto& p : o) if (p.first == key) return true;
    return false;
  }
  // the text of a string, in the encoding of TeXmacs
  string text () const {
    return kind == STR? utf8_to_cork (string (s.c_str ())): string (""); }
  std::string str () const { return kind == STR? s: std::string (); }
  int num () const { return kind == NUM? (int) n: 0; }
  bool truth () const { return kind == BOOL? b: (kind != NUL); }
};

static void
json_space (const char*& p, const char* end) {
  while (p < end && (*p == ' ' || *p == '\n' || *p == '\t' || *p == '\r')) p++;
}

static void
json_utf8 (std::string& r, unsigned c) {
  if (c < 0x80) r.push_back ((char) c);
  else if (c < 0x800) {
    r.push_back ((char) (0xC0 | (c >> 6)));
    r.push_back ((char) (0x80 | (c & 0x3F))); }
  else {
    r.push_back ((char) (0xE0 | (c >> 12)));
    r.push_back ((char) (0x80 | ((c >> 6) & 0x3F)));
    r.push_back ((char) (0x80 | (c & 0x3F))); }
}

static std::string
json_string (const char*& p, const char* end) {
  std::string r;
  p++; // the quote
  while (p < end && *p != '"') {
    if (*p == '\\' && p + 1 < end) {
      p++;
      switch (*p) {
      case 'n': r.push_back ('\n'); break;
      case 't': r.push_back ('\t'); break;
      case 'r': r.push_back ('\r'); break;
      case 'b': r.push_back ('\b'); break;
      case 'f': r.push_back ('\f'); break;
      case 'u':
        if (p + 4 < end) {
          json_utf8 (r, (unsigned) strtoul (std::string (p + 1, 4).c_str (), NULL, 16));
          p += 4;
        }
        break;
      default: r.push_back (*p);
      }
      p++;
    }
    else r.push_back (*p++);
  }
  if (p < end) p++;
  return r;
}

static json
json_value (const char*& p, const char* end) {
  json v;
  json_space (p, end);
  if (p >= end) return v;
  if (*p == '{') {
    v.kind= json::OBJ; p++;
    json_space (p, end);
    while (p < end && *p != '}') {
      json_space (p, end);
      if (*p != '"') break;
      std::string key= json_string (p, end);
      json_space (p, end);
      if (p < end && *p == ':') p++;
      v.o.push_back (std::make_pair (key, json_value (p, end)));
      json_space (p, end);
      if (p < end && *p == ',') p++;
      json_space (p, end);
    }
    if (p < end) p++;
  }
  else if (*p == '[') {
    v.kind= json::ARR; p++;
    json_space (p, end);
    while (p < end && *p != ']') {
      v.a.push_back (json_value (p, end));
      json_space (p, end);
      if (p < end && *p == ',') p++;
      json_space (p, end);
    }
    if (p < end) p++;
  }
  else if (*p == '"') { v.kind= json::STR; v.s= json_string (p, end); }
  else if (!strncmp (p, "true", 4)) { v.kind= json::BOOL; v.b= true; p += 4; }
  else if (!strncmp (p, "false", 5)) { v.kind= json::BOOL; v.b= false; p += 5; }
  else if (!strncmp (p, "null", 4)) p += 4;
  else {
    char* q= NULL;
    v.kind= json::NUM; v.n= strtod (p, &q);
    if (q == p) { v.kind= json::NUL; p++; } else p= q;
  }
  return v;
}

static json
json_parse (const char* data, size_t n) {
  const char* p= data;
  return json_value (p, data + n);
}

/******************************************************************************
* A view of the core: a canvas which shows its pixels
******************************************************************************/

class tau_view_rep;
static hashmap<int,pointer> views (NULL); // number of a view -> its canvas
static std::string clipboard_text ();

class tau_view_rep: public vue_simple_widget_rep {
public:
  int     view;        // the number of the view in the core, or 0
  int     place;       // the number of its last place
  int     pw, ph;      // the size of the place, in pixels
  picture frame;       // the pixels which the core sent for this place
  int     fw, fh;
  bool    embedded;    // in a dialog, with a size of its own
  bool    input;       // it takes the keyboard
  SI      hint_w, hint_h;
  // Where the view is in its document. The core scrolls it and says where
  // it is with each paint (ax, ay: the top left corner of the frame, in
  // the coordinates of this canvas, whose extents are those of the
  // document); the canvas of Vue then shows its scroll bars. When they are
  // dragged, Vue moves the canvas and the core is asked to follow.
  SI      ax, ay;
  bool    asked;       // the core was asked to scroll, and has not yet
  SI      asked_x, asked_y;

  tau_view_rep (int v= 0, bool emb= false, bool inp= true, SI hw= 0, SI hh= 0):
    view (0), place (0), pw (0), ph (0), fw (0), fh (0),
    embedded (emb), input (inp), hint_w (hw), hint_h (hh),
    ax (0), ay (0), asked (false), asked_x (0), asked_y (0) { set_view (v); }
  ~tau_view_rep () {
    if (view && views [view] == (pointer) this) views->reset (view); }
  bool is_editor_widget () { return true; }
  bool is_embedded_widget () { return embedded; }
  void handle_get_size_hint (SI& w, SI& h) { w= hint_w; h= hint_h; }

  double density () { return win != NULL && win->density > 0? win->density: 1.0; }
  void say_place () {
    if (!view || pw <= 0 || ph <= 0) return;
    core_say ("place " + std::to_string (view) + " " + std::to_string (pw) + " " +
              std::to_string (ph) + " " + std::to_string (density ()) + " " +
              std::to_string (++place));
  }
  void set_view (int v) {
    if (v == view) return;
    if (view && views [view] == (pointer) this) views->reset (view);
    view= v;
    if (view) views (view)= (pointer) this;
    say_place ();
  }

  void handle_notify_resize (SI w, SI h) {
    if (ren == NULL) return;
    int nw= (int) (w / ren->pixel), nh= (int) (h / ren->pixel);
    if (nw == pw && nh == ph) return;
    pw= nw; ph= nh;
    say_place ();
  }
  void handle_keypress (string key, time_t t) {
    (void) t;
    if (!view || !input) return;
    if (key == "M-v" || key == "C-v") {
      // what the system has to paste, if it has a text: the core has its
      // own clipboard only
      std::string text= clipboard_text ();
      if (text != "") {
        core_say ("paste " + std::to_string (view) + " " + escaped (text));
        return;
      }
    }
    core_say ("key " + std::to_string (view) + " " + std_of (cork_to_utf8 (key)));
  }
  void handle_keyboard_focus (bool has_focus, time_t t) {
    (void) t;
    if (view) core_say ("focus " + std::to_string (view) + (has_focus? " 1": " 0"));
  }
  void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                     array<double> data) {
    (void) t;
    if (!view || ren == NULL) return;
    int px= (int) ((x - backing_pos.x1) / ren->pixel);
    int py= (int) ((backing_pos.x2 - y) / ren->pixel);
    if (kind == "drop") {
      // What was dropped on the view (the items which Vue made of it,
      // under a ticket): the images are inserted where they fell, the
      // other files are opened, a text is pasted
      extern hashmap<int,tree> payloads;
      tree doc= payloads [mods];
      payloads->reset (mods);
      std::string images, text;
      for (int i= 0; i < N(doc); i++)
        if (is_func (doc[i], IMAGE) && is_atomic (doc[i][0]))
          images += (images == ""? "": "\n") + std_of (cork_to_utf8 (doc[i][0]->label));
        else if (is_atomic (doc[i])) {
          url u= url_system (doc[i]->label);
          if (is_rooted (u) && exists (u))
            core_say ("chosen 0 " + std_of (cork_to_utf8 (doc[i]->label)));
          else text += std_of (cork_to_utf8 (doc[i]->label));
        }
      if (images != "")
        core_say ("drop " + std::to_string (view) + " " + std::to_string (px) + " " +
                  std::to_string (py) + " " + escaped (images));
      if (text != "") core_say ("paste " + std::to_string (view) + " " + escaped (text));
      return;
    }
    if (kind == "wheel") {
      // (the displacement in points, y up: for the core pixels, y down)
      if (N(data) != 2) return;
      double d= density ();
      core_say ("scroll " + std::to_string (view) + " " +
                std::to_string ((int) (-data[0] * d)) + " " +
                std::to_string ((int) (-data[1] * d)) + " " +
                std::to_string (px) + " " + std::to_string (py));
      return;
    }
    core_say ("mouse " + std::to_string (view) + " " + std_of (kind) + " " +
              std::to_string (px) + " " + std::to_string (py) + " " +
              std::to_string (mods) + " 0");
  }
  void handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2) {
    win->set_background (white);
    win->clear (x1, y1, x2, y2);
    if (is_nil (frame)) return;
    win->draw_picture (frame, ax, ay - fh * win->pixel);
    follow ();
  }
  // the canvas is not where the frame is (a scroll bar of Vue was
  // dragged): the core is asked for the difference
  void follow () {
    if (!view || ren == NULL || embedded) return;
    if (backing_pos.x1 == ax && backing_pos.x2 == ay) { asked= false; return; }
    if (asked && asked_x == backing_pos.x1 && asked_y == backing_pos.x2) return;
    asked= true; asked_x= backing_pos.x1; asked_y= backing_pos.x2;
    core_say ("scroll " + std::to_string (view) + " " +
              std::to_string ((int) ((backing_pos.x1 - ax) / ren->pixel)) + " " +
              std::to_string ((int) ((ay - backing_pos.x2) / ren->pixel)) + " 0 0");
  }

  // a paint of the core: the rectangle which changed
  void paint (const int32_t* n, const unsigned char* px) {
    if (n[0] != view || n[1] != place) return; // for another place
    int w= n[2], h= n[3], x= n[4], y= n[5], dw= n[6], dh= n[7];
    if (is_nil (frame) || w != fw || h != fh) {
      frame= native_opaque_picture (w, h, 0, 0);
      fw= w; fh= h;
    }
    mupdf_picture_rep* p= (mupdf_picture_rep*) frame->get_handle ();
    if (p == NULL || p->pix == NULL || x < 0 || y < 0 || x + dw > fw || y + dh > fh) return;
    fz_context* ctx= mupdf_context ();
    unsigned char* dst= fz_pixmap_samples (ctx, p->pix);
    ptrdiff_t stride= fz_pixmap_stride (ctx, p->pix);
    for (int row= 0; row < dh; row++)
      memcpy (dst + (ptrdiff_t) (y + row) * stride + 4 * x,
              px + (size_t) 4 * row * dw, (size_t) 4 * dw);
    // (the image which MuPDF made of the pixmap is not of these pixels)
    if (p->im != NULL) { fz_drop_image (ctx, p->im); p->im= NULL; }
    if (!embedded && ren != NULL) {
      // the extents of the document and where the view is in it
      SI px= ren->pixel;
      SI nx= (SI) n[10] * px, ny= -(SI) n[11] * px;
      bool moved= (nx != ax || ny != ay);
      extents= rectangle (0, -(SI) max (n[9], h) * px, (SI) max (n[8], w) * px, 0);
      ax= nx; ay= ny;
      if (asked && !moved) asked= false; // (it could not go there)
      if (!asked && (backing_pos.x1 != ax || backing_pos.x2 != ay)) {
        // the core scrolled: the canvas goes where the frame is
        scroll_pos= coord2 (ax, ay);
        absolute_scroll= false;
        scroll_pending= true;
      }
    }
    invalidate_all ();
    needs_update ();
  }
};

/******************************************************************************
* The interface, from its descriptions
******************************************************************************/

static widget        the_window;       // the window of the shell
static widget        the_frame;        // the widget of TeXmacs inside it
static tau_view_rep* the_view= NULL;

widget tau_shell_window () { return the_window; }

// the frames which came while something else was waited for
static std::deque<core_frame> put_aside;

// The contents of a submenu, asked from the core and waited for: Vue asks
// for the widget of a menu when it opens it, and wants it then
static json
expand (int number) {
  core_say ("expand " + std::to_string (number));
  std::string part= std::to_string (number);
  auto until= std::chrono::steady_clock::now () + std::chrono::seconds (3);
  while (true) {
    std::unique_lock<std::mutex> g (frames_lock);
    while (!frames.empty ()) {
      core_frame f= std::move (frames.front ());
      frames.pop_front ();
      if (!strcmp (f.tag, "JSON")) {
        const char* kind= (const char*) f.body.data ();
        const char* fpart= kind + strlen (kind) + 1;
        if (!strcmp (kind, "contents") && part == fpart) {
          const char* js= fpart + strlen (fpart) + 1;
          return json_parse (js, f.body.size () - (size_t) (js - kind));
        }
      }
      put_aside.push_back (std::move (f));
    }
    if (core_gone ||
        frames_came.wait_until (g, until) == std::cv_status::timeout)
      return json ();
  }
}

class invoke_command_rep: public command_rep {
  int number;
public:
  invoke_command_rep (int n): number (n) {}
  void apply () { core_say ("invoke " + std::to_string (number)); }
  void apply (object args) { (void) args; apply (); }
  tm_ostream& print (tm_ostream& out) { return out << "invoke " << number; }
};

// a value which a widget gives its command, as Scheme reads it in the core
static std::string
scheme_of (object x) {
  if (is_string (x)) return std_of (scm_quote (cork_to_utf8 (as_string (x))));
  if (is_bool (x)) return as_bool (x)? "#t": "#f";
  if (is_int (x)) return std::to_string (as_int (x));
  if (is_double (x)) return std::to_string (as_double (x));
  if (is_null (x)) return "(list)";
  if (is_list (x)) {
    std::string r= "(list";
    for (; !is_null (x); x= cdr (x)) r += " " + scheme_of (car (x));
    return r + ")";
  }
  return "#f";
}

// the command of an input: its value is the answer under a number
class answer_command_rep: public command_rep {
  int number;
public:
  answer_command_rep (int n): number (n) {}
  void apply () {}
  void apply (object args) {
    // (the arguments of the command; an input which is given up says #f)
    std::string r;
    if (is_list (args) && !is_null (args) && is_bool (car (args)) &&
        !as_bool (car (args)) && is_null (cdr (args))) return;
    if (is_list (args))
      for (; !is_null (args); args= cdr (args)) r += " " + scheme_of (car (args));
    else r= " " + scheme_of (args);
    core_say ("answer " + std::to_string (number) + escaped (r));
  }
  tm_ostream& print (tm_ostream& out) { return out << "answer " << number; }
};

// a toggle answers what it becomes
class toggle_command_rep: public command_rep {
  int number; bool on;
public:
  toggle_command_rep (int n, bool o): number (n), on (o) {}
  void apply () {
    on= !on;
    core_say ("answer " + std::to_string (number) + (on? " #t": " #f")); }
  void apply (object args) {
    if (is_list (args) && !is_null (args) && is_bool (car (args))) {
      on= as_bool (car (args));
      core_say ("answer " + std::to_string (number) + (on? " #t": " #f"));
    }
    else apply ();
  }
  tm_ostream& print (tm_ostream& out) { return out << "toggle " << number; }
};

// A part which the core sends again ("refresh"): a place for widgets
// which are replaced, in the layout of Vue, which is made anew at each pass
class tau_slot_rep: public vue_widget_rep {
public:
  widget content;
  tau_slot_rep (): vue_widget_rep ("tau-slot") {}
  void do_layout () { if (!is_nil (content)) concrete (content)->do_layout (); }
  bool post_layout () {
    return !is_nil (content) && concrete (content)->post_layout (); }
};
struct slot_info { tau_slot_rep* slot; bool dialog; bool row; };
static hashmap<int,pointer> slots (NULL); // number of a refreshable -> its slot

// where a node is: in a menu or a bar, or in a dialog (or a tool), where
// it is laid out; in a row or in a column
struct context { bool dialog; bool row; };

// the first input of what is being made: it takes the keyboard
static widget first_input;
static string first_input_type;

static widget build (const json& node, int style, context c);
static array<widget> build_items (const json& items, int style, context c);

static widget
pack (array<widget> a, context c) {
  if (N(a) == 1) return a[0];
  if (c.dialog) return c.row? horizontal_list (a): vertical_list (a);
  return c.row? horizontal_menu (a): vertical_menu (a);
}

class contents_promise_rep: public promise_rep<widget> {
  int number;
public:
  contents_promise_rep (int n): number (n) {}
  tm_ostream& print (tm_ostream& out) { return out << "contents " << number; }
  widget eval () {
    json c= expand (number);
    return vertical_menu (build_items (c["items"], 0, { false, false }));
  }
};

// what an entry or a submenu shows: its icon or its label
static widget
build_label (const json& node, int style) {
  if (node.has ("icon") && node["icon"].str () != "")
    // By its name, as the menus of TeXmacs name it: Vue finds the file
    // in the icon set and the theme, and gives it the size of an icon
    // (the file which the core found is drawn at its own size: a flag
    // filled the window)
    return xpm_widget (url (node["icon"].text ()));
  return text_widget (node["label"].text (), style, black, false);
}

static array<string>
strings_of (const json& a) {
  array<string> r;
  for (const json& v : a.a) r << v.text ();
  return r;
}

// an input with its label before it
static widget
labelled (const json& node, widget w) {
  if (node["label"].str () == "") return w;
  array<widget> a;
  a << text_widget (node["label"].text (), 0, black, false)
    << glue_widget (false, false, 6 * PIXEL, 0) << w;
  return horizontal_list (a);
}

static widget
build (const json& node, int style, context c) {
  std::string kind= node["kind"].str ();
  bool enabled= !node.has ("enabled") || node["enabled"].truth ();
  int st= style | (enabled? 0: WIDGET_STYLE_INERT | WIDGET_STYLE_GREY);
  context row= { c.dialog, true }, column= { c.dialog, false };
  string width= node["width"].text ();
  widget w;
  if (kind == "horizontal" || kind == "hlist" || kind == "class")
    w= c.dialog? horizontal_list (build_items (node["items"], style, row))
               : horizontal_menu (build_items (node["items"], style, row));
  else if (kind == "vertical" || kind == "vlist" || kind == "division")
    w= c.dialog? vertical_list (build_items (node["items"], style, column))
               : vertical_menu (build_items (node["items"], style, column));
  else if (kind == "minibar")
    w= minibar_menu (build_items (node["items"], style, row));
  else if (kind == "tile")
    w= tile_menu (build_items (node["items"], style, row),
                  node.has ("columns")? node["columns"].num (): 8);
  else if (kind == "separator")
    w= menu_separator (c.dialog? node["vertical"].truth (): c.row);
  else if (kind == "group")
    w= menu_group (node["label"].text (), style);
  else if (kind == "glue")
    w= glue_widget (node["hext"].truth (), node["vext"].truth (),
                    node["width"].num () * PIXEL, node["height"].num () * PIXEL);
  else if (kind == "text")
    w= text_widget (node["label"].text (), style, black, false);
  else if (kind == "entry") {
    command cmd= tm_new<invoke_command_rep> (node["action"].num ());
    if (node["button"].truth () && c.dialog) st |= WIDGET_STYLE_BUTTON;
    if (c.dialog && node["check"].str () != "") st |= WIDGET_STYLE_PRESSED;
    w= menu_button (build_label (node, st), cmd, node["check"].text (),
                    node["shortcut"].text (), st);
  }
  else if (kind == "submenu") {
    promise<widget> pw= tm_new<contents_promise_rep> (node["contents"].num ());
    if (node["pull"].str () == "right") w= pullright_button (build_label (node, st), pw);
    else w= pulldown_button (build_label (node, st), pw);
  }
  else if (kind == "refreshable") {
    tau_slot_rep* slot= tm_new<tau_slot_rep> ();
    slot->content= pack (build_items (node["items"], style, c), c);
    slot_info* info= new slot_info { slot, c.dialog, c.row };
    slots (node["number"].num ())= (pointer) info;
    w= widget (slot);
  }
  else if (kind == "hsplit" || kind == "vsplit") {
    array<widget> a= build_items (node["items"], style, column);
    if (N(a) >= 2) w= (kind == "hsplit"? hsplit_widget (a[0], a[1]): vsplit_widget (a[0], a[1]));
    else w= pack (a, column);
  }
  else if (kind == "scrollable")
    w= user_canvas_widget (pack (build_items (node["items"], style, column), column), 0);
  else if (kind == "resize") {
    widget inner= pack (build_items (node["items"], style, column), column);
    string h= node["height"].text ();
    w= resize_widget (inner, 0, width, h, width, h, width, h, "", "");
  }
  else if (kind == "box") {
    array<widget> a;
    if (node["label"].str () != "") a << menu_group (node["label"].text (), style);
    a << build_items (node["items"], style, column);
    w= vertical_list (a);
  }
  else if (kind == "aligned") {
    array<widget> lhs, rhs;
    for (const json& r : node["rows"].a) {
      lhs << pack (build_items (r["left"], style, row), row);
      rhs << pack (build_items (r["right"], style, row), row);
    }
    w= aligned_widget (lhs, rhs, 0, 0, 0, 0);
  }
  else if (kind == "tabs") {
    array<widget> tabs, bodies;
    for (const json& t : node["tabs"].a) {
      tabs << pack (build_items (t["label"], style, row), row);
      bodies << pack (build_items (t["items"], style, column), column);
    }
    w= tabs_widget (tabs, bodies);
  }
  else if (kind == "view") {
    w= widget (tm_new<tau_view_rep> (node["view"].num (), true,
                                     !node.has ("input") || node["input"].truth (),
                                     node["width"].num () * PIXEL,
                                     node["height"].num () * PIXEL));
  }
  else if (kind == "input") {
    command cmd= tm_new<answer_command_rep> (node["answer"].num ());
    array<string> def;
    def << node["value"].text ();
    for (const json& v : node["proposals"].a)
      if (v.str () != node["value"].str ()) def << v.text ();
    w= input_text_widget (cmd, node["type"].text (), def, st, width);
    if (is_nil (first_input) && enabled) {
      first_input= w;
      first_input_type= node["type"].text ();
    }
  }
  else if (kind == "enum") {
    command cmd= tm_new<answer_command_rep> (node["answer"].num ());
    array<string> vals= strings_of (node["values"]);
    if (node["editable"].truth ()) vals << string ("");
    w= labelled (node, enum_widget (cmd, vals, node["value"].text (), st, width));
  }
  else if (kind == "choice") {
    command cmd= tm_new<answer_command_rep> (node["answer"].num ());
    array<string> vals= strings_of (node["values"]), chosen= strings_of (node["chosen"]);
    if (node.has ("filter"))
      w= choice_widget (cmd, vals, N(chosen) > 0? chosen[0]: string (""),
                        node["filter"].text ());
    else if (node["multiple"].truth ()) w= choice_widget (cmd, vals, chosen, st);
    else w= choice_widget (cmd, vals, N(chosen) > 0? chosen[0]: string (""), st);
  }
  else if (kind == "toggle") {
    bool on= node["on"].truth ();
    command cmd= tm_new<toggle_command_rep> (node["answer"].num (), on);
    w= labelled (node, toggle_widget (cmd, on, st));
  }
  else {
    static hashmap<string,bool> told (false);
    string k (kind.c_str ());
    if (!told[k]) {
      told (k)= true;
      cout << "tau_shell] no widget yet for " << k << LF;
    }
    if (!c.dialog) return widget ();
    return text_widget ("[" * k * "]", WIDGET_STYLE_GREY, black, false);
  }
  if (node.has ("help") && node["help"].str () != "" && !is_nil (w) &&
      (kind == "entry" || kind == "submenu"))
    w= balloon_widget (w, text_widget (node["help"].text (), 0, black, false));
  return w;
}

static array<widget>
build_items (const json& items, int style, context c) {
  array<widget> a;
  for (const json& item : items.a) {
    widget w= build (item, style, c);
    if (!is_nil (w)) a << w;
  }
  return a;
}

// a part of the window: the menu bar, an icon bar, the tools at a side
static void
set_part (std::string part, const json& desc) {
  bool tool= part.compare (0, 5, "side-") == 0 || part.compare (0, 7, "bottom-") == 0;
  context c= { tool, !tool };
  first_input= widget ();
  string pending_focus;
  widget w= pack (build_items (desc["items"], 0, c), c);
  if (tool) {
    // a tool with a field (the search) takes the keyboard when it comes,
    // and the view has it back when the tool goes
    static hashmap<string,bool> had_input (false);
    string key (part.c_str ());
    if (!is_nil (first_input)) {
      // (by its name, as TeXmacs does: the field is not laid out yet)
      if (!had_input [key]) pending_focus= first_input_type;
      had_input (key)= true;
    }
    else if (had_input [key]) {
      had_input (key)= false;
      send_keyboard_focus (the_frame, true);
    }
  }
  first_input= widget ();
  if (part == "menu") set_main_menu (the_frame, w);
  else if (part == "icons-0") set_main_icons (the_frame, w);
  else if (part == "icons-1") set_mode_icons (the_frame, w);
  else if (part == "icons-2") set_focus_icons (the_frame, w);
  else if (part == "side-0") set_side_tools (the_frame, w);
  else if (part == "side-1") set_left_tools (the_frame, w);
  else if (part == "bottom-0") set_bottom_tools (the_frame, w);
  else if (part == "bottom-1") set_extra_tools (the_frame, w);
  if (pending_focus != "") send_keyboard_focus_on (the_frame, pending_focus);
}

static void
set_visible (std::string part, bool flag) {
  if (part == "menu") set_header_visibility (the_frame, flag);
  else if (part == "icons-0") set_main_icons_visibility (the_frame, flag);
  else if (part == "icons-1") set_mode_icons_visibility (the_frame, flag);
  else if (part == "icons-2") set_focus_icons_visibility (the_frame, flag);
  else if (part == "footer") set_footer_visibility (the_frame, flag);
  else if (part == "side-0") set_side_tools_visibility (the_frame, flag);
  else if (part == "side-1") set_left_tools_visibility (the_frame, flag);
  else if (part == "bottom-0") set_bottom_tools_visibility (the_frame, flag);
  else if (part == "bottom-1") set_extra_tools_visibility (the_frame, flag);
}

/******************************************************************************
* The dialogs, the menu of a view, the clipboard, the files
******************************************************************************/

static hashmap<int,widget> dialogs; // number of a dialog -> its window

class close_dialog_command_rep: public command_rep {
  int id;
public:
  close_dialog_command_rep (int i): id (i) {}
  void apply () { core_say ("close " + std::to_string (id)); }
  void apply (object args) { (void) args; apply (); }
  tm_ostream& print (tm_ostream& out) { return out << "close " << id; }
};

static void
remove_dialog (int id) {
  if (!dialogs->contains (id)) return;
  widget win= dialogs [id];
  dialogs->reset (id);
  set_visibility (win, false);
  destroy_window_widget (win);
}

static void
show_dialog (const json& m) {
  int id= m["id"].num ();
  remove_dialog (id); // (shown again: with its new contents)
  context c= { true, false };
  first_input= widget ();
  widget body= pack (build_items (m["items"], 0, c), c);
  command quit= tm_new<close_dialog_command_rep> (id);
  widget win= plain_window_widget (body, m["title"].text (), quit);
  dialogs (id)= win;
  set_visibility (win, true);
  first_input= widget ();
}

static void
refresh_part (const json& m) {
  slot_info* info= (slot_info*) slots [m["number"].num ()];
  if (info == NULL) return;
  context c= { info->dialog, info->row };
  info->slot->content= pack (build_items (m["items"], 0, c), c);
  gui_needs_relayout= true;
}

// the menu of a view, where the pointer is
static widget context_menu;
static void
show_context_menu (const json& m) {
  if (!is_nil (context_menu)) {
    set_visibility (context_menu, false);
    destroy_window_widget (context_menu);
    context_menu= widget ();
  }
  widget menu= vertical_menu (build_items (m["items"], 0, { false, false }));
  context_menu= popup_window_widget (popup_widget (menu), "Popup menu");
  float x= 0, y= 0;
  SDL_GetGlobalMouseState (&x, &y);
  set_position (context_menu, (SI) (x * PIXEL), (SI) (-y * PIXEL));
  set_visibility (context_menu, true);
}

// A tooltip of the core: a view in a small window over the main view
static hashmap<string,widget> tooltips;
static void
show_tooltip (const json& m) {
  string id= m["id"].text ();
  if (tooltips->contains (id)) {
    widget old= tooltips [id];
    tooltips->reset (id);
    set_visibility (old, false);
    destroy_window_widget (old);
  }
  if (m["view"].num () == 0 || the_view == NULL) return;
  widget v (tm_new<tau_view_rep> (m["view"].num (), true, false,
                                  m["width"].num () * PIXEL, m["height"].num () * PIXEL));
  widget win= tooltip_window_widget (v, "Tooltip");
  SI wx= 0, wy= 0, cx= 0, cy= 0;
  get_position (the_window, wx, wy);
  get_position (widget (the_view), cx, cy);
  set_position (win, wx + cx + m["x"].num () * PIXEL, wy + cy - m["y"].num () * PIXEL);
  set_visibility (win, true);
  tooltips (id)= win;
}

static std::string
clipboard_text () {
  if (!SDL_HasClipboardText ()) return "";
  char* t= SDL_GetClipboardText ();
  std::string r (t == NULL? "": t);
  SDL_free (t);
  return r;
}

// A file which the core asks for: the dialog of the system. (The page
// has a panel on the files which it keeps; here the files are those of the
// computer.)
class file_chosen_command_rep: public command_rep {
public:
  int ticket;
  widget chooser;
  file_chosen_command_rep (int t): ticket (t) {}
  void apply () {
    vue_chooser_widget_rep* ch= dynamic_cast<vue_chooser_widget_rep*> (chooser.rep);
    string file= (ch == NULL? string ("#f"): ch->file);
    // (the chooser gives the file as Scheme: (system->url "..."), or #f)
    string path;
    int i= search_forwards ("\"", 0, file);
    if (i >= 0) path= scm_unquote (file (i, search_backwards ("\"", N(file), file) + 1));
    core_say ("chosen " + std::to_string (ticket) + " " + std_of (path));
    chooser= widget (); // (the two refer to each other)
  }
  void apply (object args) { (void) args; apply (); }
  tm_ostream& print (tm_ostream& out) { return out << "file " << ticket; }
};

static void
choose_file (const json& m) {
  std::string mode= m["mode"].str ();
  if (mode == "browse") return; // (the files are those of the computer)
  file_chosen_command_rep* rep= tm_new<file_chosen_command_rep> (m["ticket"].num ());
  command cmd (rep);
  // (not "image": the chooser of Vue then asks the editor for sizes)
  widget ch= file_chooser_widget (cmd, "generic", mode == "save"? string ("Save"): string (""));
  rep->chooser= ch;
  send<string> (ch, SLOT_DIRECTORY, m["dir"].text ());
  if (mode == "save") send<string> (ch, SLOT_FILE, m["name"].text ());
  widget win= plain_window_widget (ch, m["title"].text ());
  set_visibility (win, true);
}

/******************************************************************************
* What the core says, at each turn of the loop of Vue
******************************************************************************/

// The other windows of the core (View -> New window): each is a window of
// its own here, with a view and nothing else; the bars of the main window
// are those of the view which has the keyboard, as the core says them.
struct pane { widget window; tau_view_rep* view; };
static hashmap<int,pointer> panes (NULL); // number of a place -> its pane
static int main_place= 0;

class close_pane_command_rep: public command_rep {
  int place;
public:
  close_pane_command_rep (int p): place (p) {}
  void apply () { core_say ("buffer close-window " + std::to_string (place) + " -"); }
  void apply (object args) { (void) args; apply (); }
  tm_ostream& print (tm_ostream& out) { return out << "close pane " << place; }
};

static tau_view_rep*
view_of_place (int place) {
  if (main_place == 0) main_place= place;
  if (place == main_place) return the_view;
  pane* p= (pane*) panes [place];
  if (p == NULL) {
    p= new pane ();
    p->view= tm_new<tau_view_rep> ();
    widget frame= texmacs_widget (0, command ());
    p->window= plain_window_widget (frame, "Tau", tm_new<close_pane_command_rep> (place));
    set_header_visibility (frame, false);
    set_footer_visibility (frame, false);
    set_size (p->window, 800 * PIXEL, 600 * PIXEL);
    set_scrollable (frame, widget (p->view));
    set_visibility (p->window, true);
    panes (place)= (pointer) p;
  }
  return p->view;
}

// the places which the core has no more
static void
drop_panes (const json& desc) {
  array<int> gone;
  for (iterator<int> it= iterate (panes); it->busy (); ) {
    int place= it->next ();
    bool found= false;
    for (const json& w : desc["windows"].a) found= found || w["window"].num () == place;
    if (!found) gone << place;
  }
  for (int i= 0; i < N(gone); i++) {
    pane* p= (pane*) panes [gone[i]];
    panes->reset (gone[i]);
    if (p == NULL) continue;
    set_visibility (p->window, false);
    destroy_window_widget (p->window);
    delete p;
  }
}

// The documents, as tabs over the view: the one which is shown is
// pressed, a click shows another one, the cross closes one (the core asks
// first when it is not saved). One document: no tabs. They are in the
// place of the fourth icon bar (the icons of the user), just above the
// view, which the core seldom uses: what it says of that bar is dropped.

class buffer_command_rep: public command_rep {
  std::string what, name; int place;
public:
  buffer_command_rep (std::string w, int p, std::string n):
    what (w), name (n), place (p) {}
  void apply () { core_say ("buffer " + what + " " + std::to_string (place) + " " + name); }
  void apply (object args) { (void) args; apply (); }
  tm_ostream& print (tm_ostream& out) { return out << "buffer"; }
};

static void
set_tabs (const json& desc) {
  if (desc["windows"].a.empty ()) return;
  const json* found= &desc["windows"].a.front ();
  for (const json& w : desc["windows"].a)
    if (w["window"].num () == main_place) found= &w;
  const json& win= *found;
  int place= win["window"].num ();
  array<widget> a;
  if (desc["buffers"].a.size () > 1)
    for (const json& b : desc["buffers"].a) {
      bool current= b["name"].str () == win["buffer"].str ();
      string title= b["title"].text () * (b["modified"].truth ()? string (" *"): string (""));
      int st= WIDGET_STYLE_BUTTON | (current? WIDGET_STYLE_PRESSED: 0);
      // (the one which is shown: in bold)
      a << menu_button (text_widget (title, st | (current? WIDGET_STYLE_BOLD: 0), black, false),
                        tm_new<buffer_command_rep> ("switch", place, b["name"].str ()),
                        "", "", st)
        << menu_button (text_widget ("<times>", WIDGET_STYLE_GREY, black, false),
                        tm_new<buffer_command_rep> ("close", place, b["name"].str ()),
                        "", "", 0)
        << glue_widget (false, false, 6 * PIXEL, 0);
    }
  if (N(a) > 0) {
    a << glue_widget (true, false, 0, 0);
    set_user_icons (the_frame, horizontal_menu (a));
  }
  set_user_icons_visibility (the_frame, N(a) > 0);
}

static void
handle_message (std::string kind, std::string part, const json& desc) {
  if (kind == "batch") {
    // what Scheme said in a turn: each with its kind and its part
    for (const json& m : desc["msgs"].a)
      handle_message (m["t"].str (), m["part"].str (), m);
  }
  else if (kind == "chrome") set_part (part, desc);
  else if (kind == "visible") set_visible (part, desc["visible"].truth ());
  else if (kind == "footer") {
    if (part == "left") set_left_footer (the_frame, desc["text"].text ());
    else set_right_footer (the_frame, desc["text"].text ());
  }
  else if (kind == "buffers") {
    // the title of the window: the document which is shown
    if (desc["windows"].a.empty ()) return;
    set_tabs (desc);
    drop_panes (desc);
    for (const json& w : desc["windows"].a) {
      int place= w["window"].num ();
      pane* p= (pane*) panes [place];
      if (place != main_place && p == NULL) continue;
      for (const json& b : desc["buffers"].a)
        if (b["name"].str () == w["buffer"].str ())
          set_name (place == main_place? the_window: p->window, b["title"].text () *
                    (b["modified"].truth ()? string (" * - Tau"): string (" - Tau")));
    }
  }
  else if (kind == "dialog") show_dialog (desc);
  else if (kind == "close") remove_dialog (desc["id"].num ());
  else if (kind == "refresh") refresh_part (desc);
  else if (kind == "popup") show_context_menu (desc);
  else if (kind == "tooltip") show_tooltip (desc);
  else if (kind == "clipboard") SDL_SetClipboardText (desc["text"].str ().c_str ());
  else if (kind == "paste-request") {
    if (the_view != NULL && the_view->view)
      core_say ("paste " + std::to_string (the_view->view) + " " + escaped (clipboard_text ()));
  }
  else if (kind == "files") choose_file (desc);
  else if (kind == "quit") exit (0);
}

static void
handle_frame (core_frame& f) {
  if (!strcmp (f.tag, "VIEW")) {
    int view= 0, window= 0;
    sscanf (std::string (f.body.begin (), f.body.end ()).c_str (), "%d %d", &view, &window);
    tau_view_rep* v= view_of_place (window);
    if (v != NULL) v->set_view (view);
  }
  else if (!strcmp (f.tag, "PANT") && f.body.size () >= 56) {
    int32_t n[14];
    memcpy (n, f.body.data (), 56);
    tau_view_rep* v= (tau_view_rep*) views [n[0]];
    static bool debug= (getenv ("TAU_SHELL_DEBUG") != NULL);
    if (debug)
      fprintf (stderr, "tau_shell] PANT view %d place %d size %dx%d rect %d,%d %dx%d scroll %d,%d of %dx%d%s\n",
               n[0], n[1], n[2], n[3], n[4], n[5], n[6], n[7], n[10], n[11], n[8], n[9],
               v == NULL? " (no canvas)": (n[1] != v->place? " (another place)": ""));
    if (v != NULL) v->paint (n, f.body.data () + 56);
    // TAU_SHELL_EVAL: a command of Scheme for the core, once it draws
    // (for the tests: a dialog to open)
    static bool evaluated= false;
    if (!evaluated) {
      evaluated= true;
      if (getenv ("TAU_SHELL_EVAL") != NULL)
        core_say ("scheme " + escaped (getenv ("TAU_SHELL_EVAL")));
    }
  }
  else if (!strcmp (f.tag, "JSON")) {
    const char* kind= (const char*) f.body.data ();
    const char* part= kind + strlen (kind) + 1;
    const char* js= part + strlen (part) + 1;
    json desc= json_parse (js, f.body.size () - (size_t) (js - kind));
    // TAU_SHELL_DEBUG: what the core says, as it comes
    static bool debug= (getenv ("TAU_SHELL_DEBUG") != NULL);
    if (debug)
      fprintf (stderr, "tau_shell] %s %s %.*s\n", kind, part,
               (int) std::min ((size_t) 1500, f.body.size () - (size_t) (js - kind)), js);
    handle_message (kind, part, desc);
  }
}

static void
shell_turn () {
  std::deque<core_frame> got;
  bool gone;
  {
    std::lock_guard<std::mutex> g (frames_lock);
    got.swap (put_aside);
    for (core_frame& f : frames) got.push_back (std::move (f));
    frames.clear ();
    gone= core_gone;
  }
  for (core_frame& f : got) handle_frame (f);
  if (!got.empty ()) needs_update ();
  if (gone && !is_nil (the_window)) {
    // the core is gone: so is this program
    cout << "tau_shell] the core stopped" << LF;
    exit (0);
  }
}

// The window is closed: the core is asked to quit, which it does after a
// question when documents are not saved; this program ends with it
class quit_command_rep: public command_rep {
public:
  void apply () {
    if (core_gone || core_in < 0) exit (0);
    core_say ("scheme (safely-quit-TeXmacs)");
  }
  void apply (object args) { (void) args; apply (); }
  tm_ostream& print (tm_ostream& out) { return out << "quit"; }
};

/******************************************************************************
* The program
******************************************************************************/

void
tau_shell_main (int argc, char** argv) {
  initialize_scheme ();
  // what Vue asks of Scheme, which is in the core here
  eval ("(begin"
        " (define (lazy-initialize-force) #t)"
        " (define (menu-expand x) x)"
        " (define (kbd-system-rewrite k) k)"
        " (define (footer-environment?) #f)"
        " (define (footer-set-budget n) #t)"
        " (define (get-window-zoom-factor) 1.0)"
        " (define (wheel-capture?) #t)"
        " (define (set-message . l) #t)"
        " (define (format-get-name x) x)"
        " (define (format-get-suffixes* x) (list))"
        " (define (safely-quit-TeXmacs) (quit))"
        " (define (open-window) #t))");
  gui_open (argc, argv);

  // the core, next to this program unless TAU_CORE says where
  string core= get_env ("TAU_CORE");
  if (core == "") {
    string self (argv[0]);
    int i= N(self) - 1;
    while (i >= 0 && self[i] != '/') i--;
    core= (i < 0? string ("."): self (0, i)) * "/tau_core";
  }

  command quit= tm_new<quit_command_rep> ();
  the_frame= texmacs_widget (0, quit);
  dialogs= hashmap<int,widget> ();
  tooltips= hashmap<string,widget> ();
  the_view= tm_new<tau_view_rep> ();
  widget view_widget (the_view);
  the_window= plain_window_widget (the_frame, "Tau", quit);
  set_size (the_window, 1000 * PIXEL, 700 * PIXEL);
  set_scrollable (the_frame, view_widget);
  set_visibility (the_window, true);

  if (!core_start (url_system (core), argc, argv)) {
    cerr << "tau_shell] cannot start " << core << LF;
    exit (1);
  }
  gui_interpose (shell_turn);
  gui_start_loop ();
  if (core_in >= 0) close (core_in);
  exit (0);
}

#endif // TAU_SHELL
