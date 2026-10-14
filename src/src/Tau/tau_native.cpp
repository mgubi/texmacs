
/******************************************************************************
* MODULE     : tau_native.cpp
* DESCRIPTION: The core of Tau as a native program: its messages over pipes
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* A trial (docs/tau-design.md, "On the desktop"). In the page the core is in
* a worker and its messages are those of the worker. Built for the machine
* (misc/tau/native/Makefile) it is a process of its own, started by the
* program which has the windows (src/Tau/tau_shell.cpp, built by
* misc/tau/shell/Makefile), and the same messages go over two pipes:
*
* - what the other side says comes on the standard input, a line for each
*   message: its name and its arguments, the last of which may hold spaces
*   ("key 3 C-x", "mouse 3 press-left 120 80 1 0", "place 3 800 600 2 1",
*   "answer 12 \"a value\" #t", "chosen 4 /home/me/a.tm", "bar 3 0 1200":
*   where a scroll bar was dragged to). A last
*   argument which is a key, a text, a path or a name has its new lines
*   written \n and its backslashes twice: a line is one message, whatever
*   a file is called. A message whose numbers are not numbers, or which has
*   no name the core knows, is said on the standard error and dropped;
* - what the core says goes to the descriptor which the variable TAU_FD
*   names (the standard output is for what TeXmacs prints), in frames: four
*   letters, the length of what follows in four bytes (in the order of the
*   machine: both programs are on it), and that. "HELO" comes first, with
*   the version of these messages (TAU_PROTOCOL); "VIEW" and "JSON" are
*   text, "PANT" the numbers of a paint (14 integers) and then its pixels,
*   "PONG" the answer to "ping".
*
* A ping is answered once the messages before it were handled, and the
* views they changed drawn and sent (each handler ends with a turn). What
* a handler only puts among the delayed commands (an action of a menu, an
* answer, a command of Scheme) may not be done yet.
*
* When the other side goes (its end of a pipe is closed) the documents
* which are not saved are saved aside, as TeXmacs does at intervals (the
* files "~" which it proposes at the next start), and the core ends. So it
* does when a signal asks it to end (SIGTERM, SIGINT, SIGHUP), with the
* status 3, which is not that of quitting (0).
* Without TAU_FD nothing is said, and the core only runs what it is given.
******************************************************************************/

#ifndef __EMSCRIPTEN__

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <string>
#include <unistd.h>
#include <poll.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>

// the version of the messages: the shell says nothing to a core which has
// another one (tau_shell.cpp)
#define TAU_PROTOCOL 1

// the messages of the other side, as the worker of the page calls them
// (tau_gui.cpp)
extern "C" {
  void tau_place (int view, int w, int h, double density, int counter);
  void tau_scroll_by (int view, int dx, int dy, int x, int y);
  void tau_bar (int view, int x, int y);
  void tau_focus (int view, int has_focus);
  void tau_key (int view, const char* key);
  void tau_text (int view, const char* text);
  void tau_mouse (int view, const char* kind, int x, int y, int mods, int age);
  void tau_invoke (int n);
  void tau_expand (int n);
  void tau_scheme (const char* code);
  void tau_closed (int id);
  void tau_context_menu (int view);
  void tau_answer (int n, const char* args);
  void tau_file (int ticket, const char* path);
  void tau_paste (int view, const char* text, const char* html);
  void tau_buffer (const char* what, int place, const char* name);
  void tau_drop (int view, int x, int y, const char* paths);
}

// the documents which are not saved, saved aside (tau_gui.cpp)
void tau_native_rescue ();

static int  out_fd= -2;
static bool peer_gone= false; // the other side no longer reads

static int
out () {
  if (out_fd == -2) {
    const char* s= getenv ("TAU_FD");
    out_fd= (s == NULL? -1: atoi (s));
    // (not for the programs which the core starts, the sessions of the
    // plugins: with it they kept the pipe open, and the other side did
    // not see the core go when it went before them)
    if (out_fd >= 0) fcntl (out_fd, F_SETFD, FD_CLOEXEC);
    unsetenv ("TAU_FD");
  }
  return peer_gone? -1: out_fd;
}

static void
write_all (const void* data, size_t n) {
  const char* p= (const char*) data;
  while (n > 0) {
    ssize_t r= write (out_fd, p, n);
    if (r < 0) {
      if (errno == EINTR) continue;
      // (the other side is gone: nothing more is said, and the loop ends
      // the core once the documents are saved aside)
      peer_gone= true;
      return;
    }
    p += r; n -= (size_t) r;
  }
}

static void frame (const char* tag, const void* a, size_t na,
                   const void* b= NULL, size_t nb= 0);

// what the core is, before all it says (the parts of the interface are
// said while TeXmacs starts, before the loop): the other side checks it
static void
hello () {
  static bool said= false;
  if (said) return;
  said= true;
  char text[32];
  int n= snprintf (text, sizeof (text), "%d", TAU_PROTOCOL);
  frame ("HELO", text, (size_t) n);
}

static void
frame (const char* tag, const void* a, size_t na, const void* b,
       size_t nb) {
  if (out () < 0) return;
  hello ();
  uint32_t len= (uint32_t) (na + nb);
  char head[8];
  memcpy (head, tag, 4);
  memcpy (head + 4, &len, 4);
  write_all (head, 8);
  if (na > 0 && !peer_gone) write_all (a, na);
  if (nb > 0 && !peer_gone) write_all (b, nb);
}

void
tau_native_view (int view, int window, const char* what) {
  char buf[256];
  int n= snprintf (buf, sizeof (buf), "%d %d %s", view, window, what);
  frame ("VIEW", buf, (size_t) n);
}

void
tau_native_json (const char* kind, const char* part, const char* json) {
  // the kind, the part and the description, each ended by a zero
  std::string s (kind);
  s.push_back ('\0'); s += part; s.push_back ('\0'); s += json;
  frame ("JSON", s.data (), s.size ());
}

void
tau_native_paint (int view, int counter, int w, int h,
                  const unsigned char* pixels,
                  int x1, int y1, int x2, int y2,
                  int ew, int eh, int sx, int sy, int cx, int cy) {
  if (out () < 0 || pixels == NULL) return;
  // the rectangle which changed, its rows one after the other
  int dw= x2 - x1, dh= y2 - y1;
  if (dw <= 0 || dh <= 0) return;
  int32_t nums[14]= { view, counter, w, h, x1, y1, dw, dh,
                      ew, eh, sx, sy, cx, cy };
  size_t bytes= (size_t) 4 * dw * dh;
  if (dw == w) {
    frame ("PANT", nums, sizeof (nums), pixels + (size_t) 4 * w * y1, bytes);
    return;
  }
  unsigned char* data= (unsigned char*) malloc (bytes);
  for (int row= 0; row < dh; row++)
    memcpy (data + (size_t) 4 * row * dw,
            pixels + (size_t) 4 * ((size_t) (y1 + row) * w + x1),
            (size_t) 4 * dw);
  frame ("PANT", nums, sizeof (nums), data, bytes);
  free (data);
}

/******************************************************************************
* What the other side says
******************************************************************************/

// the next word of a line (the rest of the line when last is true: after
// the one space which separates it, since a text may begin with spaces)
static std::string
word (const std::string& line, size_t& pos, bool last= false) {
  if (last) { if (pos < line.size () && line[pos] == ' ') pos++; }
  else while (pos < line.size () && line[pos] == ' ') pos++;
  size_t start= pos;
  if (last) pos= line.size ();
  else while (pos < line.size () && line[pos] != ' ') pos++;
  return line.substr (start, pos - start);
}

// a number; one which is not makes the message wrong (bad)
static int
num (const std::string& line, size_t& pos, bool& bad) {
  std::string w= word (line, pos);
  char* end= NULL;
  long v= strtol (w.c_str (), &end, 10);
  if (w.empty () || *end != 0 || v < -2147483647L || v > 2147483647L) bad= true;
  return (int) v;
}

static double
real (const std::string& line, size_t& pos, bool& bad) {
  std::string w= word (line, pos);
  char* end= NULL;
  double v= strtod (w.c_str (), &end);
  if (w.empty () || *end != 0 || v != v) bad= true;
  return v;
}

// (a new line in what is typed or run is written \n, a backslash twice)
static std::string
unescape (const std::string& s) {
  std::string r;
  for (size_t i= 0; i < s.size (); i++)
    if (s[i] == '\\' && i + 1 < s.size ()) {
      i++;
      r.push_back (s[i] == 'n'? '\n': s[i]);
    }
    else r.push_back (s[i]);
  return r;
}

static void
told (const char* what, const std::string& line) {
  fprintf (stderr, "tau_core] %s: %.200s\n", what, line.c_str ());
}

static void
dispatch (const std::string& line, void (*turn) (void)) {
  size_t pos= 0;
  bool bad= false;
  std::string name= word (line, pos);
#define NUM num (line, pos, bad)
#define LAST unescape (word (line, pos, true))
#define CHECKED(call) do { if (bad) told ("a wrong message", line); else call; } while (0)
  if (name == "place") {
    int view= NUM, w= NUM, h= NUM;
    double density= real (line, pos, bad);
    int counter= NUM;
    CHECKED (tau_place (view, w, h, density, counter));
  }
  else if (name == "mouse") {
    int view= NUM;
    std::string kind= word (line, pos);
    int x= NUM, y= NUM, mods= NUM, age= NUM;
    CHECKED (tau_mouse (view, kind.c_str (), x, y, mods, age));
  }
  else if (name == "scroll") {
    int view= NUM, dx= NUM, dy= NUM, x= NUM, y= NUM;
    CHECKED (tau_scroll_by (view, dx, dy, x, y));
  }
  else if (name == "bar") {
    // (where a scroll bar was dragged to, in pixels of the document)
    int view= NUM, x= NUM, y= NUM;
    CHECKED (tau_bar (view, x, y));
  }
  else if (name == "key") {
    int view= NUM;
    std::string key= LAST;
    CHECKED (tau_key (view, key.c_str ()));
  }
  else if (name == "text") {
    int view= NUM;
    std::string text= LAST;
    CHECKED (tau_text (view, text.c_str ()));
  }
  else if (name == "focus") {
    int view= NUM, has= NUM;
    CHECKED (tau_focus (view, has));
  }
  else if (name == "invoke") { int n= NUM; CHECKED (tau_invoke (n)); }
  else if (name == "expand") { int n= NUM; CHECKED (tau_expand (n)); }
  else if (name == "close") { int n= NUM; CHECKED (tau_closed (n)); }
  // (the context menu of a view, asked from outside it: a tag of the footer)
  else if (name == "popup") { int n= NUM; CHECKED (tau_context_menu (n)); }
  else if (name == "scheme") tau_scheme (LAST.c_str ());
  else if (name == "answer") {
    // (the arguments as Scheme reads them)
    int n= NUM;
    std::string args= LAST;
    CHECKED (tau_answer (n, args.c_str ()));
  }
  else if (name == "chosen") {
    // (a path may hold anything, a new line too: escaped as a text is)
    int ticket= NUM;
    std::string path= LAST;
    CHECKED (tau_file (ticket, path.c_str ()));
  }
  else if (name == "paste") {
    int view= NUM;
    std::string text= LAST;
    CHECKED (tau_paste (view, text.c_str (), ""));
  }
  else if (name == "drop") {
    // (images which were dropped: their paths, a line for each)
    int view= NUM, x= NUM, y= NUM;
    std::string paths= LAST;
    CHECKED (tau_drop (view, x, y, paths.c_str ()));
  }
  else if (name == "buffer") {
    std::string what= word (line, pos);
    int place= NUM;
    std::string bname= LAST;
    CHECKED (tau_buffer (what.c_str (), place, bname.c_str ()));
  }
  else if (name == "ping") {
    // (answered once all that came before it is done, as in the page)
    std::string id= word (line, pos, true);
    frame ("PONG", id.data (), id.size ());
  }
  else if (name == "turn") turn ();
  else if (name != "") told ("a message the core does not know", line);
#undef NUM
#undef LAST
#undef CHECKED
}

// A line longer than this is no message (the other side is wrong): what
// came of it is dropped, until its end
#define TAU_LINE_MAX ((size_t) 256 << 20)

// the end, when the other side is gone
static void
leave (int code= 0) {
  tau_native_rescue ();
  _exit (code);
}

// Asked to end by a signal (the session which ends, kill): the documents
// are saved aside first, by the loop, once what the core does is done; a
// second signal ends it at once. It ends with 3, which the shell tells
// from quitting (0).
static volatile sig_atomic_t stop_asked= 0;

static void
stop_on_signal (int sig) {
  (void) sig;
  if (stop_asked) _exit (3);
  stop_asked= 1;
}

void
tau_native_loop (void (*turn) (void)) {
  // A turn is made for each message (the functions above end with one)
  // and, when nothing comes, at regular intervals for the delayed commands
  // (and the animations, the pipes of the plugins...)
  std::string pending;
  bool skipping= false; // (the rest of a line which was too long)
  char buf[65536];
  struct pollfd pfd;
  pfd.fd= 0; pfd.events= POLLIN;
  if (out () >= 0) hello ();
  signal (SIGTERM, stop_on_signal);
  signal (SIGINT, stop_on_signal);
  signal (SIGHUP, stop_on_signal);
  while (true) {
    if (stop_asked) leave (3);
    if (peer_gone) leave ();
    int r= poll (&pfd, 1, 20);
    if (r < 0 && errno != EINTR) break;
    if (r <= 0) { turn (); continue; }
    ssize_t n= read (0, buf, sizeof (buf));
    if (n < 0) { if (errno == EINTR) continue; break; }
    if (n == 0) {
      // the other side closed: it is gone (a last message without its new
      // line is still one)
      if (!skipping && !pending.empty ()) dispatch (pending, turn);
      break;
    }
    pending.append (buf, (size_t) n);
    size_t start= 0, nl;
    while ((nl= pending.find ('\n', start)) != std::string::npos) {
      if (!skipping) dispatch (pending.substr (start, nl - start), turn);
      skipping= false;
      start= nl + 1;
      if (stop_asked) leave (3);
      if (peer_gone) leave ();
    }
    pending.erase (0, start);
    if (pending.size () > TAU_LINE_MAX) {
      fprintf (stderr, "tau_core] a line of more than %zu bytes: dropped\n",
               TAU_LINE_MAX);
      pending.clear ();
      skipping= true;
    }
  }
  leave ();
}

#endif // !__EMSCRIPTEN__
