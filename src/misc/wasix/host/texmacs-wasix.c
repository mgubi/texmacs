/******************************************************************************
* MODULE     : texmacs-wasix.c
* DESCRIPTION: The desktop host of the WASIX build: runs TeXmacs in wasmer
*              and shows its display (the stream of vue_stream.cpp) in a
*              window of SDL3
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
*   texmacs-wasix [-tm <TeXmacs dir>] [-wasm <texmacs.wasm>] [-home <dir>]
*                 [-wasmer <wasmer>] [-shot <file.bmp> <seconds>]
*                 [-- <arguments of TeXmacs>]
*
* -shot (for the tests): the window as drawn is saved after each frame, and
* the host quits after that many seconds.
*
* The TeXmacs tree is /texmacs for the program, <dir> (default
* ~/.TeXmacs-wasix) its home, the current directory /work, where it starts.
* Built by misc/wasix/host/Makefile (a native program, with SDL3).
******************************************************************************/

#include <SDL3/SDL.h>
#include <errno.h>
#include <signal.h>
#include <pthread.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

extern char** environ;

enum { OUT_HELLO= 1, OUT_RECT, OUT_FRAME, OUT_CURSOR, OUT_TITLE };
enum { IN_RESIZE= 1, IN_MOTION, IN_BUTTON, IN_WHEEL, IN_KEY, IN_TEXT,
       IN_FOCUS, IN_LEAVE, IN_QUIT };

static int to_tm= -1, from_tm= -1;
static pid_t child= -1;

/* the frame, written by the reader thread, uploaded by the main thread */
static SDL_Mutex* lock;
static unsigned char* fb= NULL;
static int fb_w= 0, fb_h= 0;
static int dirty_x1, dirty_y1, dirty_x2, dirty_y2; /* empty when x1 >= x2 */
static int shown_w= 0, shown_h= 0;                 /* of the last FRAME */
static int new_cursor= -1;
static char* new_title= NULL;
static Uint32 frame_event;

/******************************************************************************
* Messages to TeXmacs
******************************************************************************/

static void
put32 (unsigned char* p, Uint32 v) {
  p[0]= v & 255; p[1]= (v >> 8) & 255; p[2]= (v >> 16) & 255; p[3]= v >> 24;
}

static void
putf (unsigned char* p, float f) {
  Uint32 u;
  memcpy (&u, &f, 4);
  put32 (p, u);
}

static void
send_msg (Uint32 type, const void* payload, Uint32 n) {
  unsigned char h[8];
  put32 (h, type); put32 (h + 4, n);
  unsigned char* all= malloc (8 + n);
  memcpy (all, h, 8);
  if (n > 0) memcpy (all + 8, payload, n);
  size_t k= 0;
  while (k < 8 + n) {
    ssize_t w= write (to_tm, all + k, 8 + n - k);
    if (w < 0) { if (errno == EINTR) continue; break; }
    k += w;
  }
  free (all);
}

/******************************************************************************
* Messages from TeXmacs (the reader thread)
******************************************************************************/

static int
read_all (unsigned char* p, size_t n) {
  while (n > 0) {
    ssize_t k= read (from_tm, p, n);
    if (k == 0) return 0;
    if (k < 0) { if (errno == EINTR) continue; return 0; }
    p += k; n -= k;
  }
  return 1;
}

static Uint32
get32 (const unsigned char* p) {
  return (Uint32) p[0] | ((Uint32) p[1] << 8) | ((Uint32) p[2] << 16) |
         ((Uint32) p[3] << 24);
}

static void
reshape (int w, int h) {
  /* (with the lock) the frame keeps what it had where the sizes overlap */
  if (w == fb_w && h == fb_h) return;
  unsigned char* n= calloc ((size_t) w * h, 4);
  for (int j= 0; j < h && j < fb_h; j++)
    memcpy (n + (size_t) 4 * j * w, fb + (size_t) 4 * j * fb_w,
            4 * (w < fb_w ? w : fb_w));
  free (fb);
  fb= n; fb_w= w; fb_h= h;
  dirty_x1= 0; dirty_y1= 0; dirty_x2= w; dirty_y2= h;
}

static int
reader (void* data) {
  (void) data;
  /* the stream starts at the magic word of HELLO, after what the runtime
     itself may have printed */
  unsigned char c, magic[4]= { 0, 0, 0, 0 };
  while (memcmp (magic, "TMWX", 4) != 0) {
    if (!read_all (&c, 1)) goto end;
    memmove (magic, magic + 1, 3); magic[3]= c;
  }
  unsigned char version[4];
  if (!read_all (version, 4)) goto end;
  while (1) {
    unsigned char h[8];
    if (!read_all (h, 8)) break;
    Uint32 type= get32 (h), n= get32 (h + 4);
    unsigned char* p= malloc (n + 1);
    if (!read_all (p, n)) { free (p); break; }
    p[n]= 0;
    SDL_LockMutex (lock);
    if (type == OUT_RECT && n >= 20) {
      int x= get32 (p), y= get32 (p + 4), w= get32 (p + 8), hh= get32 (p + 12);
      if ((size_t) n >= 20 + (size_t) 4 * w * hh) {
        int W= x + w > fb_w ? x + w : fb_w, H= y + hh > fb_h ? y + hh : fb_h;
        reshape (W, H);
        for (int j= 0; j < hh; j++)
          memcpy (fb + ((size_t) (y + j) * fb_w + x) * 4, p + 20 + (size_t) 4 * j * w, 4 * w);
        if (dirty_x1 >= dirty_x2) {
          dirty_x1= x; dirty_y1= y; dirty_x2= x + w; dirty_y2= y + hh;
        }
        else {
          if (x < dirty_x1) dirty_x1= x;
          if (y < dirty_y1) dirty_y1= y;
          if (x + w > dirty_x2) dirty_x2= x + w;
          if (y + hh > dirty_y2) dirty_y2= y + hh;
        }
      }
    }
    else if (type == OUT_FRAME && n >= 8) {
      shown_w= get32 (p); shown_h= get32 (p + 4);
      reshape (shown_w, shown_h);
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= frame_event;
      SDL_PushEvent (&ev);
    }
    else if (type == OUT_CURSOR && n >= 4) new_cursor= (int) get32 (p);
    else if (type == OUT_TITLE) { free (new_title); new_title= strdup ((char*) p); }
    SDL_UnlockMutex (lock);
    free (p);
  }
 end:
  {
    SDL_Event ev;
    SDL_zero (ev);
    ev.type= SDL_EVENT_QUIT; /* TeXmacs has ended */
    ev.quit.timestamp= 1;    /* (to tell it from the window closed) */
    SDL_PushEvent (&ev);
  }
  return 0;
}

/******************************************************************************
* Starting TeXmacs in wasmer
******************************************************************************/

static char*
cat (const char* a, const char* b) {
  char* s= malloc (strlen (a) + strlen (b) + 1);
  strcpy (s, a); strcat (s, b);
  return s;
}

static int
start_texmacs (const char* wasmer, const char* tm, const char* wasm,
               const char* home, int argc, char** argv) {
  int in[2], out[2];
  if (pipe (in) != 0 || pipe (out) != 0) return 0;
  char cwd[4096];
  if (!getcwd (cwd, sizeof (cwd))) strcpy (cwd, ".");
  const char* args[64];
  int n= 0;
  args[n++]= wasmer; args[n++]= "run";
  args[n++]= "--volume"; args[n++]= cat (tm, ":/texmacs");
  args[n++]= "--volume"; args[n++]= cat (home, ":/tmhome");
  args[n++]= "--volume"; args[n++]= cat (cwd, ":/work");
  args[n++]= "--cwd"; args[n++]= "/work";
  args[n++]= "--env"; args[n++]= "TEXMACS_PATH=/texmacs";
  args[n++]= "--env"; args[n++]= "TEXMACS_HOME_PATH=/tmhome";
  args[n++]= "--env"; args[n++]= "HOME=/tmhome";
  args[n++]= "--env"; args[n++]= "TEXMACS_DISPLAY=stream";
  args[n++]= wasm;
  if (argc > 0) args[n++]= "--";
  for (int i= 0; i < argc && n < 62; i++) args[n++]= argv[i];
  args[n]= NULL;
  posix_spawn_file_actions_t fa;
  posix_spawn_file_actions_init (&fa);
  posix_spawn_file_actions_adddup2 (&fa, in[0], 0);
  posix_spawn_file_actions_adddup2 (&fa, out[1], 1);
  posix_spawn_file_actions_addclose (&fa, in[1]);
  posix_spawn_file_actions_addclose (&fa, out[0]);
  int r= posix_spawnp (&child, wasmer, &fa, NULL, (char**) args, environ);
  posix_spawn_file_actions_destroy (&fa);
  close (in[0]); close (out[1]);
  if (r != 0) {
    fprintf (stderr, "texmacs-wasix: cannot run %s: %s\n", wasmer, strerror (r));
    return 0;
  }
  to_tm= in[1]; from_tm= out[0];
  return 1;
}

/******************************************************************************
* The window
******************************************************************************/

static Uint32
keymod () {
  return (Uint32) SDL_GetModState ();
}

static void
send_size (SDL_Window* win) {
  int w, h;
  SDL_GetWindowSize (win, &w, &h);
  unsigned char p[8];
  put32 (p, w); put32 (p + 4, h);
  send_msg (IN_RESIZE, p, 8);
}

int
main (int argc, char** argv) {
  const char* tm= NULL;
  const char* wasm= NULL;
  const char* wasmer= "wasmer";
  const char* home= NULL;
  const char* shot= NULL;
  int shot_secs= 0;
  int i= 1;
  for (; i < argc; i++) {
    if (!strcmp (argv[i], "--")) { i++; break; }
    else if (!strcmp (argv[i], "-tm") && i + 1 < argc) tm= argv[++i];
    else if (!strcmp (argv[i], "-wasm") && i + 1 < argc) wasm= argv[++i];
    else if (!strcmp (argv[i], "-home") && i + 1 < argc) home= argv[++i];
    else if (!strcmp (argv[i], "-wasmer") && i + 1 < argc) wasmer= argv[++i];
    else if (!strcmp (argv[i], "-shot") && i + 2 < argc) {
      shot= argv[++i]; shot_secs= atoi (argv[++i]); }
    else {
      fprintf (stderr, "usage: texmacs-wasix [-tm dir] [-wasm file] "
                       "[-home dir] [-wasmer prog] [-- args]\n");
      return 2;
    }
  }
  if (tm == NULL || wasm == NULL) {
    fprintf (stderr, "texmacs-wasix: -tm and -wasm are needed\n");
    return 2;
  }
  if (home == NULL) {
    const char* h= getenv ("HOME");
    home= cat (h ? h : ".", "/.TeXmacs-wasix");
  }
  mkdir (home, 0755);
  if (!start_texmacs (wasmer, tm, wasm, home, argc - i, argv + i)) return 1;

  if (!SDL_Init (SDL_INIT_VIDEO)) {
    fprintf (stderr, "texmacs-wasix: %s\n", SDL_GetError ());
    return 1;
  }
  lock= SDL_CreateMutex ();
  frame_event= SDL_RegisterEvents (1);
  SDL_Window* win;
  SDL_Renderer* ren;
  if (!SDL_CreateWindowAndRenderer ("TeXmacs (WASIX)", 1100, 800,
                                    SDL_WINDOW_RESIZABLE, &win, &ren)) {
    fprintf (stderr, "texmacs-wasix: %s\n", SDL_GetError ());
    return 1;
  }
  SDL_StartTextInput (win);
  SDL_Texture* tex= NULL;
  int tex_w= 0, tex_h= 0;
  SDL_Cursor* cursors[SDL_SYSTEM_CURSOR_COUNT]= { NULL };
  SDL_DetachThread (SDL_CreateThread (reader, "reader", NULL));
  send_size (win);
  { unsigned char p[4]; put32 (p, 1); send_msg (IN_FOCUS, p, 4); }

  int running= 1;
  Uint64 start= SDL_GetTicks ();
  while (running) {
    SDL_Event ev;
    if (shot && SDL_GetTicks () - start > (Uint64) shot_secs * 1000) break;
    if (!SDL_WaitEventTimeout (&ev, 200)) continue;
    unsigned char p[32];
    switch (ev.type) {
    case SDL_EVENT_QUIT:
      if (ev.quit.timestamp == 1) running= 0; /* TeXmacs has ended */
      else send_msg (IN_QUIT, NULL, 0);       /* TeXmacs asks to save */
      break;
    case SDL_EVENT_WINDOW_RESIZED:
      send_size (win);
      break;
    case SDL_EVENT_WINDOW_FOCUS_GAINED:
    case SDL_EVENT_WINDOW_FOCUS_LOST:
      put32 (p, ev.type == SDL_EVENT_WINDOW_FOCUS_GAINED);
      send_msg (IN_FOCUS, p, 4);
      break;
    case SDL_EVENT_WINDOW_MOUSE_LEAVE:
      send_msg (IN_LEAVE, NULL, 0);
      break;
    case SDL_EVENT_MOUSE_MOTION:
      putf (p, ev.motion.x); putf (p + 4, ev.motion.y);
      put32 (p + 8, ev.motion.state); put32 (p + 12, keymod ());
      send_msg (IN_MOTION, p, 16);
      break;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
      putf (p, ev.button.x); putf (p + 4, ev.button.y);
      put32 (p + 8, ev.button.button); put32 (p + 12, ev.button.down);
      put32 (p + 16, ev.button.clicks); put32 (p + 20, keymod ());
      send_msg (IN_BUTTON, p, 24);
      break;
    case SDL_EVENT_MOUSE_WHEEL:
      putf (p, ev.wheel.mouse_x); putf (p + 4, ev.wheel.mouse_y);
      putf (p + 8, ev.wheel.x); putf (p + 12, ev.wheel.y);
      put32 (p + 16, keymod ());
      send_msg (IN_WHEEL, p, 20);
      break;
    case SDL_EVENT_KEY_DOWN:
    case SDL_EVENT_KEY_UP:
      put32 (p, ev.key.scancode); put32 (p + 4, ev.key.key);
      put32 (p + 8, ev.key.mod); put32 (p + 12, ev.key.down);
      put32 (p + 16, ev.key.repeat);
      send_msg (IN_KEY, p, 20);
      break;
    case SDL_EVENT_TEXT_INPUT:
      send_msg (IN_TEXT, ev.text.text, (Uint32) strlen (ev.text.text));
      break;
    default:
      if (ev.type == frame_event) {
        SDL_LockMutex (lock);
        if (tex == NULL || tex_w != fb_w || tex_h != fb_h) {
          if (tex) SDL_DestroyTexture (tex);
          tex= SDL_CreateTexture (ren, SDL_PIXELFORMAT_XRGB8888,
                                  SDL_TEXTUREACCESS_STREAMING, fb_w, fb_h);
          tex_w= fb_w; tex_h= fb_h;
          dirty_x1= 0; dirty_y1= 0; dirty_x2= fb_w; dirty_y2= fb_h;
        }
        if (tex && dirty_x1 < dirty_x2 && dirty_y1 < dirty_y2) {
          SDL_Rect r= { dirty_x1, dirty_y1, dirty_x2 - dirty_x1, dirty_y2 - dirty_y1 };
          SDL_UpdateTexture (tex, &r, fb + ((size_t) dirty_y1 * fb_w + dirty_x1) * 4,
                             4 * fb_w);
          dirty_x1= dirty_x2= 0;
        }
        if (new_cursor >= 0 && new_cursor < SDL_SYSTEM_CURSOR_COUNT) {
          if (!cursors[new_cursor])
            cursors[new_cursor]= SDL_CreateSystemCursor ((SDL_SystemCursor) new_cursor);
          if (cursors[new_cursor]) SDL_SetCursor (cursors[new_cursor]);
          new_cursor= -1;
        }
        if (new_title) {
          SDL_SetWindowTitle (win, new_title);
          free (new_title); new_title= NULL;
        }
        SDL_UnlockMutex (lock);
        SDL_SetRenderDrawColor (ren, 128, 128, 128, 255);
        SDL_RenderClear (ren);
        if (tex) {
          /* the frame at its size in points, at the top left */
          SDL_FRect d= { 0, 0, (float) tex_w, (float) tex_h };
          SDL_RenderTexture (ren, tex, NULL, &d);
        }
        if (shot) {
          SDL_Surface* s= SDL_RenderReadPixels (ren, NULL);
          if (s) { SDL_SaveBMP (s, shot); SDL_DestroySurface (s); }
        }
        SDL_RenderPresent (ren);
      }
    }
  }
  close (to_tm);
  if (shot) kill (child, SIGTERM);
  int status;
  waitpid (child, &status, 0);
  SDL_Quit ();
  return 0;
}
