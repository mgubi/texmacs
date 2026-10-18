
/******************************************************************************
* MODULE     : vue_stream.cpp
* DESCRIPTION: The display of the Vue GUI as a stream of pixels and events
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "vue_stream.hpp"
#include "sys_utils.hpp"
#include <vector>
#include <string>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <errno.h>
#ifdef USE_THORVG
#include "webgpu_wire.h" // the calls of WebGPU go in the stream
#endif

/******************************************************************************
* The protocol
*
* Messages both ways: a header of two little-endian 32-bit words, the type
* and the length of what follows, then that many bytes. Numbers are
* little-endian, 32 bits (u32, i32, f32) unless said otherwise.
*
* From TeXmacs (its standard output; what it writes there itself goes to
* the standard error):
*   1 HELLO    the bytes "TMWX", u32 version (1): the first message, before
*              anything else which TeXmacs writes (vue_stream_redirect)
*   2 RECT     u32 x, y, w, h, u32 encoding (0: raw), the pixels: h rows of
*              w pixels of 4 bytes, B G R and an unused byte
*   3 FRAME    u32 w, h: the rectangles since the last one make a frame of
*              that size, to be shown
*   4 CURSOR   u32 SDL_SystemCursor
*   5 TITLE    the title of the window (utf8)
*   6 GPU      calls of WebGPU, made by the host on its own WebGPU instead of
*              RECT and FRAME, when the host gave TEXMACS_VUE_GPU=webgpu
*              (misc/wasix/webgpu/webgpu_wire.cpp, gpu_backend_wg.cpp)
*
* From the host (the standard input of TeXmacs):
*   1 RESIZE   i32 w, h: the size of the display (points)
*   2 MOTION   f32 x, y, u32 buttons (SDL_BUTTON_MASK), u32 SDL_Keymod
*   3 BUTTON   f32 x, y, u32 button (SDL_BUTTON_*), down, clicks, SDL_Keymod
*   4 WHEEL    f32 x, y, f32 dx, dy, u32 SDL_Keymod
*   5 KEY      u32 SDL_Scancode, SDL_Keycode, SDL_Keymod, down, repeat
*   6 TEXT     utf8, the text typed
*   7 FOCUS    u32 gained
*   8 LEAVE    the pointer left the display
*   9 QUIT     the host asks TeXmacs to quit
* The end of the standard input ends TeXmacs.
******************************************************************************/

#ifdef __wasi__

enum { OUT_HELLO= 1, OUT_RECT, OUT_FRAME, OUT_CURSOR, OUT_TITLE, OUT_GPU };
enum { IN_RESIZE= 1, IN_MOTION, IN_BUTTON, IN_WHEEL, IN_KEY, IN_TEXT,
       IN_FOCUS, IN_LEAVE, IN_QUIT };

static int on= -1;
static int in_fd= 0, out_fd= 1;
static SDL_Window* streamed= NULL;     // the window shown by the host
static int want_w= 0, want_h= 0;       // the size the host gives it
static Uint32 buttons= 0;
static std::vector<unsigned char> shown; // the pixels the host has
static int shown_w= 0, shown_h= 0;
static std::vector<unsigned char> inbuf; // received, not yet handled
static std::vector<std::string> texts;  // of the text events in the queue
static int last_cursor= -1;
static std::string last_title;

bool
vue_stream_on () {
  if (on < 0) on= (get_env ("TEXMACS_DISPLAY") == "stream") ? 1 : 0;
  return on == 1;
}

/******************************************************************************
* Output
******************************************************************************/

static void
write_all (const void* data, size_t n) {
  const unsigned char* p= (const unsigned char*) data;
  while (n > 0) {
    ssize_t k= ::write (out_fd, p, n);
    if (k < 0) {
      if (errno == EINTR || errno == EAGAIN) continue;
      _exit (0); // the host has gone
    }
    p += k; n -= k;
  }
}

static void
put32 (std::vector<unsigned char>& b, uint32_t v) {
  b.push_back (v & 255); b.push_back ((v >> 8) & 255);
  b.push_back ((v >> 16) & 255); b.push_back ((v >> 24) & 255);
}

static void
send (uint32_t type, const std::vector<unsigned char>& payload) {
  std::vector<unsigned char> h;
  put32 (h, type); put32 (h, (uint32_t) payload.size ());
  write_all (h.data (), h.size ());
  if (!payload.empty ()) write_all (payload.data (), payload.size ());
}

void
vue_stream_redirect () {
  // the stream keeps the standard output: what TeXmacs prints goes to the
  // standard error, from the start (texmacs_entrypoint)
  static bool done= false;
  if (done || !vue_stream_on ()) return;
  done= true;
  out_fd= dup (1);
  dup2 (2, 1);
  in_fd= 0;
  std::vector<unsigned char> p= { 'T', 'M', 'W', 'X' };
  put32 (p, 1);
  send (OUT_HELLO, p);
}

#ifdef USE_THORVG
static void
gpu_sink (const void* data, size_t n) {
  unsigned char h[8];
  uint32_t t= OUT_GPU, len= (uint32_t) n;
  for (int i= 0; i < 4; i++) { h[i]= (t >> (8*i)) & 255; h[4+i]= (len >> (8*i)) & 255; }
  write_all (h, 8);
  write_all (data, n);
}
#endif

void
vue_stream_open () {
  if (!vue_stream_on ()) return;
  vue_stream_redirect ();
  SDL_SetHint (SDL_HINT_VIDEO_DRIVER, "dummy");
#ifdef USE_THORVG
  wgpuWireSetSink (gpu_sink);
#endif
}

// the window is cut in tiles; the tiles which changed since the last frame
// are sent, those next to each other in a row of tiles as one rectangle
#define TILE 64

static void
send_rect (const unsigned char* pix, int pitch, int x, int y, int w, int h) {
  std::vector<unsigned char> p;
  p.reserve (20 + 4 * w * h);
  put32 (p, x); put32 (p, y); put32 (p, w); put32 (p, h); put32 (p, 0);
  for (int j= y; j < y + h; j++) {
    const unsigned char* row= pix + j * pitch + 4 * x;
    p.insert (p.end (), row, row + 4 * w);
  }
  send (OUT_RECT, p);
}

bool
vue_stream_track (SDL_Window* win) {
  if (!vue_stream_on ()) return false;
  if (streamed == NULL) streamed= win;
  if (win != streamed) return false;
  // the host decides of the size: a size TeXmacs gave the window is undone
  if (want_w > 0 && want_h > 0) {
    int w, h;
    SDL_GetWindowSize (win, &w, &h);
    if (w != want_w || h != want_h) SDL_SetWindowSize (win, want_w, want_h);
  }
  return true;
}

void
vue_stream_present (SDL_Window* win, SDL_Surface* surf) {
  if (!vue_stream_on () || surf == NULL) return;
  if (!vue_stream_track (win)) return;
  if (!SDL_LockSurface (surf)) return;
  int w= surf->w, h= surf->h, pitch= surf->pitch;
  const unsigned char* pix= (const unsigned char*) surf->pixels;
  bool all= (w != shown_w || h != shown_h);
  if (all) {
    shown.assign ((size_t) 4 * w * h, 0);
    shown_w= w; shown_h= h;
  }
  bool sent= all;
  for (int ty= 0; ty < h; ty += TILE) {
    int th= min (TILE, h - ty);
    int run= -1; // the first tile of the run of changed tiles
    // (tx ends at w, past the last tile, which ends the last run)
    for (int tx= 0; ; tx= min (tx + TILE, w)) {
      bool changed= false;
      if (tx < w) {
        int tw= min (TILE, w - tx);
        if (all) changed= true;
        else
          for (int j= ty; j < ty + th && !changed; j++)
            changed= memcmp (pix + j * pitch + 4 * tx,
                             &shown[((size_t) j * w + tx) * 4], 4 * tw) != 0;
      }
      if (changed && run < 0) run= tx;
      if (!changed && run >= 0) {
        int rw= min (tx, w) - run;
        send_rect (pix, pitch, run, ty, rw, th);
        for (int j= ty; j < ty + th; j++)
          memcpy (&shown[((size_t) j * w + run) * 4], pix + j * pitch + 4 * run, 4 * rw);
        run= -1;
        sent= true;
      }
      if (tx >= w) break;
    }
  }
  SDL_UnlockSurface (surf);
  if (sent) {
    std::vector<unsigned char> p;
    put32 (p, w); put32 (p, h);
    send (OUT_FRAME, p);
  }
}

void
vue_stream_cursor (int c) {
  if (!vue_stream_on () || c == last_cursor) return;
  last_cursor= c;
  std::vector<unsigned char> p;
  put32 (p, c);
  send (OUT_CURSOR, p);
}

void
vue_stream_title (string title) {
  if (!vue_stream_on ()) return;
  std::string t (&title[0], N(title));
  if (t == last_title) return;
  last_title= t;
  send (OUT_TITLE, std::vector<unsigned char> (t.begin (), t.end ()));
}

/******************************************************************************
* Input
******************************************************************************/

static uint32_t
get32 (const unsigned char* p) {
  return (uint32_t) p[0] | ((uint32_t) p[1] << 8) |
         ((uint32_t) p[2] << 16) | ((uint32_t) p[3] << 24);
}

static float
getf (const unsigned char* p) {
  uint32_t u= get32 (p);
  float f;
  memcpy (&f, &u, 4);
  return f;
}

Uint32
vue_stream_buttons () {
  return buttons;
}

static SDL_WindowID
window_id () {
  return streamed != NULL ? SDL_GetWindowID (streamed) : 0;
}

static void
handle (uint32_t type, const unsigned char* p, uint32_t n) {
  SDL_Event ev;
  SDL_zero (ev);
  Uint64 now= SDL_GetTicksNS ();
  switch (type) {
  case IN_RESIZE:
    if (n < 8) return;
    want_w= (int) get32 (p); want_h= (int) get32 (p + 4);
    if (streamed != NULL && want_w > 0 && want_h > 0)
      SDL_SetWindowSize (streamed, want_w, want_h);
    return;
  case IN_MOTION:
    if (n < 16) return;
    buttons= get32 (p + 8);
    SDL_SetModState ((SDL_Keymod) get32 (p + 12));
    ev.type= SDL_EVENT_MOUSE_MOTION;
    ev.motion.timestamp= now;
    ev.motion.windowID= window_id ();
    ev.motion.state= buttons;
    ev.motion.x= getf (p); ev.motion.y= getf (p + 4);
    break;
  case IN_BUTTON:
    if (n < 24) return;
    {
      Uint8 b= (Uint8) get32 (p + 8);
      bool down= get32 (p + 12) != 0;
      SDL_SetModState ((SDL_Keymod) get32 (p + 20));
      if (down) buttons |= SDL_BUTTON_MASK (b);
      else buttons &= ~SDL_BUTTON_MASK (b);
      ev.type= down ? SDL_EVENT_MOUSE_BUTTON_DOWN : SDL_EVENT_MOUSE_BUTTON_UP;
      ev.button.timestamp= now;
      ev.button.windowID= window_id ();
      ev.button.button= b;
      ev.button.down= down;
      ev.button.clicks= (Uint8) max (1, (int) get32 (p + 16));
      ev.button.x= getf (p); ev.button.y= getf (p + 4);
    }
    break;
  case IN_WHEEL:
    if (n < 20) return;
    SDL_SetModState ((SDL_Keymod) get32 (p + 16));
    ev.type= SDL_EVENT_MOUSE_WHEEL;
    ev.wheel.timestamp= now;
    ev.wheel.windowID= window_id ();
    ev.wheel.mouse_x= getf (p); ev.wheel.mouse_y= getf (p + 4);
    ev.wheel.x= getf (p + 8); ev.wheel.y= getf (p + 12);
    ev.wheel.direction= SDL_MOUSEWHEEL_NORMAL;
    break;
  case IN_KEY:
    if (n < 20) return;
    SDL_SetModState ((SDL_Keymod) get32 (p + 8));
    ev.type= get32 (p + 12) ? SDL_EVENT_KEY_DOWN : SDL_EVENT_KEY_UP;
    ev.key.timestamp= now;
    ev.key.windowID= window_id ();
    ev.key.scancode= (SDL_Scancode) get32 (p);
    ev.key.key= (SDL_Keycode) get32 (p + 4);
    ev.key.mod= (SDL_Keymod) get32 (p + 8);
    ev.key.down= get32 (p + 12) != 0;
    ev.key.repeat= get32 (p + 16) != 0;
    break;
  case IN_TEXT:
    // the event points to the text: kept until the next pump, which comes
    // once the queue is empty (see vue_stream_pump)
    texts.push_back (std::string ((const char*) p, n));
    ev.type= SDL_EVENT_TEXT_INPUT;
    ev.text.timestamp= now;
    ev.text.windowID= window_id ();
    ev.text.text= texts.back ().c_str ();
    break;
  case IN_FOCUS:
    if (n < 4) return;
    ev.type= get32 (p) ? SDL_EVENT_WINDOW_FOCUS_GAINED : SDL_EVENT_WINDOW_FOCUS_LOST;
    ev.window.timestamp= now;
    ev.window.windowID= window_id ();
    break;
  case IN_LEAVE:
    ev.type= SDL_EVENT_WINDOW_MOUSE_LEAVE;
    ev.window.timestamp= now;
    ev.window.windowID= window_id ();
    break;
  case IN_QUIT:
    ev.type= SDL_EVENT_QUIT;
    ev.quit.timestamp= now;
    break;
  default:
    return;
  }
  SDL_PushEvent (&ev);
}

void
vue_stream_pump () {
  if (!vue_stream_on ()) return;
  // (std::string keeps short texts inside itself: the vector must not move
  // them while events point to them, hence the reserve)
  texts.clear ();
  texts.reserve (1024);
  unsigned char buf[65536];
  while (true) {
    struct pollfd pfd= { in_fd, POLLIN, 0 };
    if (poll (&pfd, 1, 0) <= 0 || (pfd.revents & (POLLIN | POLLHUP)) == 0) break;
    ssize_t k= ::read (in_fd, buf, sizeof (buf));
    if (k == 0) _exit (0); // the host has gone
    if (k < 0) { if (errno == EINTR || errno == EAGAIN) continue; break; }
    inbuf.insert (inbuf.end (), buf, buf + k);
  }
  size_t pos= 0;
  while (inbuf.size () - pos >= 8 && texts.size () < 1000) {
    uint32_t type= get32 (&inbuf[pos]), n= get32 (&inbuf[pos + 4]);
    if (inbuf.size () - pos - 8 < n) break;
    handle (type, &inbuf[pos + 8], n);
    pos += 8 + n;
  }
  inbuf.erase (inbuf.begin (), inbuf.begin () + pos);
}

void
vue_stream_wait (int ms) {
  struct pollfd pfd= { in_fd, POLLIN, 0 };
  poll (&pfd, 1, ms);
}

#endif // __wasi__
