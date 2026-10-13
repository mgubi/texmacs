
/******************************************************************************
* MODULE     : tau_shell.cpp
* DESCRIPTION: A window for the native core of Tau, in a process of its own
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* A trial (docs/tau-design.md, "On the desktop"): what the page of Tau is
* to the worker, this program is to the native core (src/Tau/tau_native.cpp
* has the messages). It starts the core, shows the view of its window in a
* window of SDL, and sends it the keys, the pointer and the wheel. Nothing
* of TeXmacs is linked here: the two processes share no memory, and the
* window stays alive whatever the core does.
*
* It has no menus, bars nor dialogs (their descriptions arrive, and are
* counted): it is there to try the transport and to measure it. The title
* of the window tells the times: from a key to the pixels which answer it,
* and the size of what came.
*
*   tau_shell [--core <program>] [--texmacs <TEXMACS_PATH>] [--home <dir>]
*             [--bench] [--scheme <code>] [--wait <ms>] [arguments of TeXmacs...]
*
* --bench types and scrolls by itself, prints the times and quits; --scheme
* runs a command of Scheme first (a document to open), and --wait is how
* long the bench leaves it before it starts.
******************************************************************************/

#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <string>
#include <vector>
#include <deque>
#include <mutex>
#include <thread>
#include <algorithm>

struct Frame { char tag[5]; std::vector<unsigned char> body; Uint64 at; };

static std::mutex frames_lock;
static std::deque<Frame> frames;
static Uint32 frame_event= 0;
static int to_core= -1;

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

// what the core says, read in a thread of its own and given to the loop
static void
reader (int fd) {
  while (true) {
    char head[8];
    if (!read_all (fd, head, 8)) break;
    uint32_t len;
    memcpy (&len, head + 4, 4);
    Frame f;
    memcpy (f.tag, head, 4); f.tag[4]= 0;
    f.body.resize (len);
    if (len > 0 && !read_all (fd, f.body.data (), len)) break;
    f.at= SDL_GetTicksNS ();
    { std::lock_guard<std::mutex> g (frames_lock); frames.push_back (std::move (f)); }
    SDL_Event e; SDL_zero (e); e.type= frame_event;
    SDL_PushEvent (&e);
  }
  SDL_Event e; SDL_zero (e); e.type= SDL_EVENT_QUIT;
  SDL_PushEvent (&e);
}

static void
say (const std::string& line) {
  std::string s= line + "\n";
  const char* p= s.data (); size_t n= s.size ();
  while (n > 0) {
    ssize_t r= write (to_core, p, n);
    if (r <= 0) return;
    p += r; n -= (size_t) r;
  }
}

static std::string
escape (const char* s) {
  std::string r;
  for (; *s; s++)
    if (*s == '\n') r += "\\n";
    else if (*s == '\\') r += "\\\\";
    else r.push_back (*s);
  return r;
}

// the name of a key for TeXmacs, or "" when the key is text (which comes
// by itself) or a modifier
static std::string
key_name (SDL_Keycode k, SDL_Keymod mod) {
  bool shift= (mod & SDL_KMOD_SHIFT) != 0, ctrl= (mod & SDL_KMOD_CTRL) != 0;
  bool alt= (mod & SDL_KMOD_ALT) != 0, meta= (mod & SDL_KMOD_GUI) != 0;
  std::string name;
  switch (k) {
  case SDLK_RETURN: case SDLK_KP_ENTER: name= "return"; break;
  case SDLK_BACKSPACE: name= "backspace"; break;
  case SDLK_DELETE: name= "delete"; break;
  case SDLK_TAB: name= "tab"; break;
  case SDLK_ESCAPE: name= "escape"; break;
  case SDLK_LEFT: name= "left"; break;
  case SDLK_RIGHT: name= "right"; break;
  case SDLK_UP: name= "up"; break;
  case SDLK_DOWN: name= "down"; break;
  case SDLK_HOME: name= "home"; break;
  case SDLK_END: name= "end"; break;
  case SDLK_PAGEUP: name= "pageup"; break;
  case SDLK_PAGEDOWN: name= "pagedown"; break;
  case SDLK_SPACE: if (!ctrl && !meta && !alt) return ""; name= "space"; break;
  default:
    if (k >= SDLK_F1 && k <= SDLK_F12) name= "F" + std::to_string (k - SDLK_F1 + 1);
    // (a letter with Control or Command is a shortcut, not text)
    else if ((ctrl || meta) && k >= 32 && k < 127) { name= std::string (1, (char) k); shift= false; }
    else return "";
  }
  if (shift) name= "S-" + name;
  if (ctrl) name= "C-" + name;
  if (alt) name= "A-" + name;
  if (meta) name= "M-" + name;
  return name;
}

static int
mods_of (SDL_MouseButtonFlags buttons) {
  SDL_Keymod m= SDL_GetModState ();
  return ((buttons & SDL_BUTTON_LMASK) ? 1 : 0) | ((buttons & SDL_BUTTON_MMASK) ? 2 : 0) |
         ((buttons & SDL_BUTTON_RMASK) ? 4 : 0) | ((m & SDL_KMOD_SHIFT) ? 256 : 0) |
         ((m & SDL_KMOD_CTRL) ? 1024 : 0) | ((m & SDL_KMOD_ALT) ? 2048 : 0) |
         ((m & SDL_KMOD_GUI) ? 4096 : 0);
}

struct Stats {
  std::vector<double> v;
  void add (double x) { v.push_back (x); }
  double mean () { double s= 0; for (double x : v) s += x; return v.empty () ? 0 : s / v.size (); }
  double high () { if (v.empty ()) return 0; std::vector<double> w= v; std::sort (w.begin (), w.end ()); return w[(w.size () * 95) / 100 >= w.size () ? w.size () - 1 : (w.size () * 95) / 100]; }
};

int
main (int argc, char** argv) {
  std::string self= argv[0];
  std::string dir= self.find ('/') == std::string::npos ? "." : self.substr (0, self.rfind ('/'));
  std::string core= dir + "/tau_core", texmacs= dir + "/../TeXmacs";
  std::string home= std::string (getenv ("HOME") ? getenv ("HOME") : ".") + "/.TauTeXmacs";
  bool bench= false;
  std::string scheme;
  int wait_ms= 1500;
  std::vector<std::string> rest;
  for (int i= 1; i < argc; i++) {
    std::string a= argv[i];
    if (a == "--core" && i + 1 < argc) core= argv[++i];
    else if (a == "--texmacs" && i + 1 < argc) texmacs= argv[++i];
    else if (a == "--home" && i + 1 < argc) home= argv[++i];
    else if (a == "--bench") bench= true;
    else if (a == "--scheme" && i + 1 < argc) scheme= argv[++i];
    else if (a == "--wait" && i + 1 < argc) wait_ms= atoi (argv[++i]);
    else rest.push_back (a);
  }

  // the core: a child, with a pipe for what it is told and one for what it says
  int in_pipe[2], out_pipe[2];
  if (pipe (in_pipe) != 0 || pipe (out_pipe) != 0) { perror ("pipe"); return 1; }
  signal (SIGPIPE, SIG_IGN);
  pid_t child= fork ();
  if (child == 0) {
    dup2 (in_pipe[0], 0);
    close (in_pipe[1]); close (out_pipe[0]);
    setenv ("TAU_FD", std::to_string (out_pipe[1]).c_str (), 1);
    setenv ("TEXMACS_PATH", texmacs.c_str (), 1);
    setenv ("TEXMACS_HOME_PATH", home.c_str (), 1);
#ifdef __APPLE__
    setenv ("TEXMACS_WEB_PLATFORM", "macos", 0);
#endif
    std::vector<char*> args;
    args.push_back ((char*) core.c_str ());
    for (auto& a : rest) args.push_back ((char*) a.c_str ());
    args.push_back (NULL);
    execv (core.c_str (), args.data ());
    perror (core.c_str ());
    _exit (127);
  }
  close (in_pipe[0]); close (out_pipe[1]);
  to_core= in_pipe[1];

  if (!SDL_Init (SDL_INIT_VIDEO)) { fprintf (stderr, "SDL: %s\n", SDL_GetError ()); return 1; }
  SDL_Window* window= SDL_CreateWindow ("Tau", 1000, 700, SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY);
  SDL_Renderer* ren= window ? SDL_CreateRenderer (window, NULL) : NULL;
  if (!ren) { fprintf (stderr, "SDL: %s\n", SDL_GetError ()); return 1; }
  SDL_StartTextInput (window);
  frame_event= SDL_RegisterEvents (1);
  std::thread (reader, out_pipe[0]).detach ();

  int view= 0, place= 0, tex_w= 0, tex_h= 0, pings= 0;
  SDL_Texture* tex= NULL;
  bool dirty= true, focused= true;
  Uint64 start= SDL_GetTicksNS (), first_paint= 0, key_at= 0, wheel_at= 0, ping_at= 0;
  Stats key_ms, wheel_ms, ping_ms, copy_ms;
  size_t json_bytes= 0, json_count= 0, paint_bytes= 0, paint_count= 0;
  std::string footer;

  auto pixels_of_window= [&] (int& w, int& h, float& density) {
    SDL_GetWindowSizeInPixels (window, &w, &h);
    density= SDL_GetWindowPixelDensity (window);
    if (density <= 0) density= 1;
  };
  auto send_place= [&] () {
    if (!view) return;
    int w, h; float d;
    pixels_of_window (w, h, d);
    say ("place " + std::to_string (view) + " " + std::to_string (w) + " " + std::to_string (h) + " " +
         std::to_string (d) + " " + std::to_string (++place));
  };
  auto ping= [&] () { ping_at= SDL_GetTicksNS (); say ("ping " + std::to_string (++pings)); };

  // --bench: what is typed and scrolled by itself, a step for each answer
  int bench_step= 0;
  Uint64 bench_at= 0; // when it starts
  const int BENCH_KEYS= 60, BENCH_WHEEL= 40;
  auto bench_next= [&] () {
    if (!bench || !view) return;
    if (bench_step < BENCH_KEYS) {
      key_at= SDL_GetTicksNS ();
      if (bench_step % 7 == 6) say ("key " + std::to_string (view) + " space");
      else say ("text " + std::to_string (view) + " " + std::string (1, "etaoin"[bench_step % 6]));
    }
    else if (bench_step < BENCH_KEYS + BENCH_WHEEL) {
      wheel_at= SDL_GetTicksNS ();
      say ("scroll " + std::to_string (view) + " 0 " + std::to_string (bench_step % 2 ? -120 : 120) + " 300 300");
    }
    else { SDL_Event e; SDL_zero (e); e.type= SDL_EVENT_QUIT; SDL_PushEvent (&e); return; }
    bench_step++;
    ping ();
  };

  bool running= true;
  while (running) {
    SDL_Event e;
    if (!SDL_WaitEventTimeout (&e, bench ? 50 : 500)) e.type= 0;
    do {
      float density= SDL_GetWindowPixelDensity (window);
      if (density <= 0) density= 1;
      if (e.type == SDL_EVENT_QUIT) running= false;
      else if (e.type == SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED) send_place ();
      else if (e.type == SDL_EVENT_WINDOW_FOCUS_GAINED || e.type == SDL_EVENT_WINDOW_FOCUS_LOST) {
        focused= e.type == SDL_EVENT_WINDOW_FOCUS_GAINED;
        if (view) say ("focus " + std::to_string (view) + (focused ? " 1" : " 0"));
      }
      else if (e.type == SDL_EVENT_KEY_DOWN && view) {
        std::string name= key_name (e.key.key, e.key.mod);
        if (name != "") { key_at= SDL_GetTicksNS (); say ("key " + std::to_string (view) + " " + name); }
      }
      else if (e.type == SDL_EVENT_TEXT_INPUT && view) {
        if (SDL_GetModState () & (SDL_KMOD_CTRL | SDL_KMOD_GUI)) continue;
        key_at= SDL_GetTicksNS ();
        if (strcmp (e.text.text, " ") == 0) say ("key " + std::to_string (view) + " space");
        else say ("text " + std::to_string (view) + " " + escape (e.text.text));
      }
      else if ((e.type == SDL_EVENT_MOUSE_BUTTON_DOWN || e.type == SDL_EVENT_MOUSE_BUTTON_UP) && view) {
        const char* b= e.button.button == SDL_BUTTON_LEFT ? "left" : e.button.button == SDL_BUTTON_MIDDLE ? "middle" : "right";
        bool down= e.type == SDL_EVENT_MOUSE_BUTTON_DOWN;
        SDL_MouseButtonFlags buttons= SDL_GetMouseState (NULL, NULL);
        say ("mouse " + std::to_string (view) + (down ? " press-" : " release-") + b + " " +
             std::to_string ((int) (e.button.x * density)) + " " + std::to_string ((int) (e.button.y * density)) + " " +
             std::to_string (mods_of (buttons)) + " 0");
      }
      else if (e.type == SDL_EVENT_MOUSE_MOTION && view) {
        say ("mouse " + std::to_string (view) + " move " + std::to_string ((int) (e.motion.x * density)) + " " +
             std::to_string ((int) (e.motion.y * density)) + " " + std::to_string (mods_of (e.motion.state)) + " 0");
      }
      else if (e.type == SDL_EVENT_MOUSE_WHEEL && view) {
        wheel_at= SDL_GetTicksNS ();
        say ("scroll " + std::to_string (view) + " " + std::to_string ((int) (-e.wheel.x * 30 * density)) + " " +
             std::to_string ((int) (-e.wheel.y * 30 * density)) + " " + std::to_string ((int) (e.wheel.mouse_x * density)) + " " +
             std::to_string ((int) (e.wheel.mouse_y * density)));
      }
    } while (SDL_PollEvent (&e) && e.type != frame_event);

    if (bench && bench_at && SDL_GetTicksNS () >= bench_at) {
      // (in the middle of the document, with the cursor there)
      bench_at= 0;
      int w, h; float d;
      pixels_of_window (w, h, d);
      say ("scroll " + std::to_string (view) + " 0 " + std::to_string (40 * h) + " 300 300");
      for (const char* kind : { "press-left", "release-left" })
        say ("mouse " + std::to_string (view) + " " + kind + " " + std::to_string (w / 2) + " " + std::to_string (h / 2) +
             (kind[0] == 'p' ? " 1 0" : " 0 0"));
      say ("ping start");
    }
    // what the core said
    std::deque<Frame> got;
    { std::lock_guard<std::mutex> g (frames_lock); got.swap (frames); }
    for (Frame& f : got) {
      if (!strcmp (f.tag, "VIEW")) {
        int v= 0, w= 0;
        sscanf ((const char*) std::string (f.body.begin (), f.body.end ()).c_str (), "%d %d", &v, &w);
        view= v;
        send_place ();
        say ("focus " + std::to_string (view) + (focused ? " 1" : " 0"));
      }
      else if (!strcmp (f.tag, "JSON")) {
        json_bytes += f.body.size (); json_count++;
        // (the footer, for the title: its text is in the description)
        const char* kind= (const char*) f.body.data ();
        if (!strcmp (kind, "footer")) footer= "";
      }
      else if (!strcmp (f.tag, "PANT") && f.body.size () >= 56) {
        int32_t n[14];
        memcpy (n, f.body.data (), 56);
        if (n[0] != view || n[1] != place) continue; // for another place
        int w= n[2], h= n[3];
        if (!tex || w != tex_w || h != tex_h) {
          if (tex) SDL_DestroyTexture (tex);
          tex= SDL_CreateTexture (ren, SDL_PIXELFORMAT_RGBA32, SDL_TEXTUREACCESS_STREAMING, w, h);
          SDL_SetTextureBlendMode (tex, SDL_BLENDMODE_NONE);
          tex_w= w; tex_h= h;
        }
        SDL_Rect r= { n[4], n[5], n[6], n[7] };
        Uint64 c0= SDL_GetTicksNS ();
        SDL_UpdateTexture (tex, &r, f.body.data () + 56, 4 * n[6]);
        copy_ms.add ((SDL_GetTicksNS () - c0) / 1e6);
        paint_bytes += f.body.size (); paint_count++;
        dirty= true;
        if (!first_paint) {
          first_paint= f.at;
          if (scheme != "") say ("scheme " + escape (scheme.c_str ()));
          bench_at= SDL_GetTicksNS () + (Uint64) wait_ms * 1000000;
        }
        // (a paint which was on its way before the key is not its answer)
        if (key_at && f.at >= key_at) { key_ms.add ((f.at - key_at) / 1e6); key_at= 0; }
        if (wheel_at && f.at >= wheel_at) { wheel_ms.add ((f.at - wheel_at) / 1e6); wheel_at= 0; }
      }
      else if (!strcmp (f.tag, "PONG")) {
        ping_ms.add ((f.at - ping_at) / 1e6);
        key_at= wheel_at= 0; // (a step which painted nothing)
        bench_next ();
      }
    }
    if (dirty) {
      SDL_SetRenderDrawColor (ren, 160, 160, 160, 255);
      SDL_RenderClear (ren);
      if (tex) SDL_RenderTexture (ren, tex, NULL, NULL);
      SDL_RenderPresent (ren);
      dirty= false;
      char title[256];
      snprintf (title, sizeof (title), "Tau (native core) — key to pixels %.1f ms, wheel %.1f ms, %zu paints, %zu descriptions",
                key_ms.v.empty () ? 0.0 : key_ms.v.back (), wheel_ms.v.empty () ? 0.0 : wheel_ms.v.back (), paint_count, json_count);
      SDL_SetWindowTitle (window, title);
    }
  }

  int w, h; float d;
  pixels_of_window (w, h, d);
  printf ("tau_shell: view of %d x %d pixels (density %.1f)\n", w, h, d);
  printf ("tau_shell: the core painted first after %.0f ms\n", first_paint ? (first_paint - start) / 1e6 : -1.0);
  printf ("tau_shell: key to pixels    mean %.2f ms, 95%% under %.2f (%zu)\n", key_ms.mean (), key_ms.high (), key_ms.v.size ());
  printf ("tau_shell: wheel to pixels  mean %.2f ms, 95%% under %.2f (%zu)\n", wheel_ms.mean (), wheel_ms.high (), wheel_ms.v.size ());
  printf ("tau_shell: pixels to the texture  mean %.2f ms, 95%% under %.2f (%zu)\n", copy_ms.mean (), copy_ms.high (), copy_ms.v.size ());
  printf ("tau_shell: %zu paints, %.1f MB; %zu descriptions, %.0f KB\n", paint_count, paint_bytes / 1048576.0, json_count, json_bytes / 1024.0);
  close (to_core);
  int status= 0;
  for (int i= 0; i < 20 && waitpid (child, &status, WNOHANG) == 0; i++) SDL_Delay (50);
  kill (child, SIGTERM);
  SDL_Quit ();
  return 0;
}
