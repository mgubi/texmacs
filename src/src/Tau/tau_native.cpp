
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
* program which has the windows (misc/tau/native/tau_shell.cpp), and the
* same messages go over two pipes:
*
* - what the other side says comes on the standard input, a line for each
*   message: its name and its arguments, the last of which may hold spaces
*   ("key 3 C-x", "mouse 3 press-left 120 80 1 0", "place 3 800 600 2 1",
*   "answer 12 \"a value\" #t", "chosen 4 /home/me/a.tm");
* - what the core says goes to the descriptor which the variable TAU_FD
*   names (the standard output is for what TeXmacs prints), in frames: four
*   letters, the length of what follows in four bytes, and that. "VIEW"
*   and "JSON" are text, "PANT" the numbers of a paint (14 integers) and
*   then its pixels, "PONG" the answer to "ping".
*
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

// the messages of the other side, as the worker of the page calls them
// (tau_gui.cpp)
extern "C" {
  void tau_place (int view, int w, int h, double density, int counter);
  void tau_scroll_by (int view, int dx, int dy, int x, int y);
  void tau_focus (int view, int has_focus);
  void tau_key (int view, const char* key);
  void tau_text (int view, const char* text);
  void tau_mouse (int view, const char* kind, int x, int y, int mods, int age);
  void tau_invoke (int n);
  void tau_expand (int n);
  void tau_scheme (const char* code);
  void tau_closed (int id);
  void tau_answer (int n, const char* args);
  void tau_file (int ticket, const char* path);
  void tau_paste (int view, const char* text, const char* html);
  void tau_buffer (const char* what, int place, const char* name);
  void tau_drop (int view, int x, int y, const char* paths);
}

static int out_fd= -2;

static int
out () {
  if (out_fd == -2) {
    const char* s= getenv ("TAU_FD");
    out_fd= (s == NULL? -1: atoi (s));
  }
  return out_fd;
}

static void
write_all (const void* data, size_t n) {
  const char* p= (const char*) data;
  while (n > 0) {
    ssize_t r= write (out_fd, p, n);
    if (r < 0) {
      if (errno == EINTR) continue;
      // (the other side is gone)
      _exit (0);
    }
    p += r; n -= (size_t) r;
  }
}

static void
frame (const char* tag, const void* a, size_t na, const void* b= NULL,
       size_t nb= 0) {
  if (out () < 0) return;
  uint32_t len= (uint32_t) (na + nb);
  char head[8];
  memcpy (head, tag, 4);
  memcpy (head + 4, &len, 4);
  write_all (head, 8);
  if (na > 0) write_all (a, na);
  if (nb > 0) write_all (b, nb);
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

// the next word of a line (the rest of the line when last is true)
static std::string
word (const std::string& line, size_t& pos, bool last= false) {
  while (pos < line.size () && line[pos] == ' ') pos++;
  size_t start= pos;
  if (last) pos= line.size ();
  else while (pos < line.size () && line[pos] != ' ') pos++;
  return line.substr (start, pos - start);
}

static int
num (const std::string& line, size_t& pos) {
  return atoi (word (line, pos).c_str ());
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
dispatch (const std::string& line, void (*turn) (void)) {
  size_t pos= 0;
  std::string name= word (line, pos);
  if (name == "place") {
    int view= num (line, pos), w= num (line, pos), h= num (line, pos);
    double density= atof (word (line, pos).c_str ());
    tau_place (view, w, h, density, num (line, pos));
  }
  else if (name == "mouse") {
    int view= num (line, pos);
    std::string kind= word (line, pos);
    int x= num (line, pos), y= num (line, pos), mods= num (line, pos);
    tau_mouse (view, kind.c_str (), x, y, mods, num (line, pos));
  }
  else if (name == "scroll") {
    int view= num (line, pos), dx= num (line, pos), dy= num (line, pos);
    int x= num (line, pos);
    tau_scroll_by (view, dx, dy, x, num (line, pos));
  }
  else if (name == "key") {
    int view= num (line, pos);
    tau_key (view, word (line, pos, true).c_str ());
  }
  else if (name == "text") {
    int view= num (line, pos);
    tau_text (view, unescape (word (line, pos, true)).c_str ());
  }
  else if (name == "focus") {
    int view= num (line, pos);
    tau_focus (view, num (line, pos));
  }
  else if (name == "invoke") tau_invoke (num (line, pos));
  else if (name == "expand") tau_expand (num (line, pos));
  else if (name == "close") tau_closed (num (line, pos));
  else if (name == "scheme")
    tau_scheme (unescape (word (line, pos, true)).c_str ());
  else if (name == "answer") {
    // (the arguments as Scheme reads them)
    int n= num (line, pos);
    tau_answer (n, unescape (word (line, pos, true)).c_str ());
  }
  else if (name == "chosen") {
    int ticket= num (line, pos);
    tau_file (ticket, word (line, pos, true).c_str ());
  }
  else if (name == "paste") {
    int view= num (line, pos);
    tau_paste (view, unescape (word (line, pos, true)).c_str (), "");
  }
  else if (name == "drop") {
    // (images which were dropped: their paths, a line for each)
    int view= num (line, pos), x= num (line, pos), y= num (line, pos);
    tau_drop (view, x, y, unescape (word (line, pos, true)).c_str ());
  }
  else if (name == "buffer") {
    std::string what= word (line, pos);
    int place= num (line, pos);
    tau_buffer (what.c_str (), place, word (line, pos, true).c_str ());
  }
  else if (name == "ping") {
    // (answered once all that came before it is done, as in the page)
    std::string id= word (line, pos, true);
    frame ("PONG", id.data (), id.size ());
  }
  else if (name == "turn") turn ();
}

void
tau_native_loop (void (*turn) (void)) {
  // A turn is made for each message (the functions above end with one)
  // and, when nothing comes, at regular intervals for the delayed commands
  std::string pending;
  char buf[65536];
  struct pollfd pfd;
  pfd.fd= 0; pfd.events= POLLIN;
  while (true) {
    int r= poll (&pfd, 1, 20);
    if (r < 0 && errno != EINTR) break;
    if (r <= 0) { turn (); continue; }
    ssize_t n= read (0, buf, sizeof (buf));
    if (n == 0) break; // the other side closed: it is gone
    if (n < 0) { if (errno == EINTR) continue; break; }
    pending.append (buf, (size_t) n);
    size_t start= 0, nl;
    while ((nl= pending.find ('\n', start)) != std::string::npos) {
      dispatch (pending.substr (start, nl - start), turn);
      start= nl + 1;
    }
    pending.erase (0, start);
  }
  _exit (0);
}

#endif // !__EMSCRIPTEN__
