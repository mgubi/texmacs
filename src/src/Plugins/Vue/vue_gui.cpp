/******************************************************************************
* MODULE     : vue_gui.cpp
* DESCRIPTION: Vue GUI
* COPYRIGHT  : (C) 2025  Masssimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "vue_gui.hpp"
#include "vue_widget.hpp"
#include "vue_gpu.hpp"

#include "array.hpp"
#include "hashmap.hpp"
#include "iterator.hpp"

#include "dictionary.hpp" // get_output_language

#include "message.hpp"
#include "window.hpp"
#include "font.hpp"

#include "analyze.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "scheme.hpp"
#include "dictionary.hpp"
#include "locale.hpp"
#include "editor.hpp"
#include "new_view.hpp"      // get_current_editor()
#include "image_files.hpp"
#include "boot.hpp"      // is_headless
#include "tm_window.hpp"
#ifdef OS_MACOS
#include "MacOS/mac_utilities.h" // mac_beep
#include <objc/runtime.h> // the NSWindow of a tool window, see set_on_top
#include <objc/message.h>
#endif
#include "sys_utils.hpp"     // get_env
#include "file.hpp"          // load_string (scripted events)
#include "socket_notifier.hpp" // notifiers_active (pause of the loop)

#include <SDL3/SDL.h>
// SDL_SetMainReady, without SDL replacing main (TeXmacs has its own)
#define SDL_MAIN_HANDLED
#include <SDL3/SDL_main.h>
#include <unistd.h> // usleep (the headless loop)
#ifdef __EMSCRIPTEN__
#include <emscripten.h> // emscripten_set_main_loop
#endif
// SDL3_ttf serves only the rendering through SDL's own renderer (see
// vue_sdl_window_rep), which the browser build leaves out
#ifndef __EMSCRIPTEN__
#define VUE_SDL_RENDERER 1
#endif
#ifdef VUE_SDL_RENDERER
#include <SDL3_ttf/SDL_ttf.h>
#endif

// The Vue GUI draws with MuPDF: its windows and pictures are MuPDF pixmaps,
// drawn by mupdf_renderer (the experimental fitz_renderer, the other one,
// has gone; see the log of Plugins/MuPDF)
#if !MUPDF_RENDERER
#error "the Vue GUI needs MuPDF (MUPDF_RENDERER)"
#endif
#include "../MuPDF/mupdf_picture.hpp"
#include "../MuPDF/mupdf_renderer.hpp" // mupdf_image_gc

#include "clay.h"
extern "C" bool vue_clay_transitions_active (void); // clay.c
extern "C" int  vue_clay_capacity_report (char* buf, int n); // clay.c



/*****************************************************************************/
// UI layout context (maybe refactor in a structure)

// pointer info (the per-window events are stored in vue_window_rep::input)
extern unsigned int mouse_state;

// scripted events (development aid, see TEXMACS_VUE_SCRIPT below)
static bool script_active= false;
static unsigned int script_buttons= 0;  // buttons held down by the script
static vue_window last_created_window= NULL;
static vue_window script_win= NULL;     // target window of the script
static vue_window snapshot_win= NULL;   // window whose next redraw is saved as
static string snapshot_name;            // <snapshot dir>/<snapshot_name>.png
static void script_init ();
static void script_step ();


extern bool debug_clay;

// list of commands
extern list<command> cmd_list;

extern vue_window current_window; // used during layout to propagate information

void gui_init_context();
void gui_finalize_context();


//******************************************************************************
// vue_window

int nr_windows= 0;
hashmap<SDL_Window*, pointer> Window_to_window;
hashmap<int, pointer> id_to_window;

static bool single_window_mode ();

class vue_sdl_base_window_rep : public vue_window_rep {
public:
  SDL_Window *sdl_win;
  SI Min_w, Min_h, Max_w, Max_h; // size limits, 0 if unset
  bool on_top;       // a tool window, above the other windows of TeXmacs
  bool level_raised; // ... and currently at the level of SDL's "on top"
  bool document= false; // the window of an editor (see plain_window)
  // the geometry last seen (see track_geometry), in points; unknown while
  // saved_w < 0
  int  saved_x, saved_y, saved_w, saved_h;

  // adopt: an SDL window taken over from a window being destroyed (the
  // host of single-window mode, see forget_host), instead of a new one
  vue_sdl_base_window_rep (vue_widget w, string name, bool popup= false,
                           SDL_Window* adopt= NULL);
  ~vue_sdl_base_window_rep ();

  void *platform_window () { return (void*)sdl_win; }

  void   destroy_event ();
  void   update_title ();  // the name and the marker of unsaved changes
  void   set_name (string name);
  string get_name ();
  void   set_modified (bool flag);
  void   set_visibility (bool flag);
  void   set_full_screen (bool flag);
  void   set_on_top (bool flag);
  void   follow_app_focus (); // an on-top window leaves its level with the app
  void   track_geometry ();   // the user moved or resized the window
  void   set_size (SI w, SI h);
  void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h);
  void   update_density (); // the pixel density of its display (override)
  void   get_size (SI& w, SI& h);
  void   get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h);
  void   set_position (SI x, SI y);
  void   get_position (SI& x, SI& y);
  
  void process_layout ();
  void layout_size (int& w, int& h);
};

int vue_window_rep::serial= 1; // serial identifier for windows

// single-window mode (see "Single-window mode" below)
static bool close_hosted_windows (vue_window w);
static void host_changed ();

#ifdef VUE_SDL_RENDERER
static inline Clay_Dimensions SDL_MeasureText(Clay_StringSlice text, Clay_TextElementConfig *config, void *userData)
{
  TTF_Font **fonts= (TTF_Font **)userData;
  TTF_Font *font= fonts[config->fontId];
  int width, height;

  TTF_SetFontSize(font, config->fontSize);
  if (!TTF_GetStringSize(font, text.chars, text.length, &width, &height)) {
      SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Failed to measure text: %s", SDL_GetError ());
  }

  return (Clay_Dimensions) { (float) width, (float) height };
}
#endif

// Clay reports its problems through this handler and goes on, skipping the
// offending element. Each kind is printed a few times and then suppressed,
// since a layout which goes wrong goes wrong on every frame.
//
// Two of them need more than the message Clay gives. An exhausted internal
// array and a genuine out of bounds read are both reported as
// CLAY_ERROR_TYPE_INTERNAL_ERROR with the same text, so the occupancy of
// the arrays is printed with it: one at its capacity says which to enlarge,
// none at capacity says the access itself was wrong and is worth reporting
// upstream. The window and the element being laid out are named too, since
// a context belongs to one window and an error says nothing about which.
void HandleClayErrors (Clay_ErrorData errorData) {
  static int reported[16];
  const int max_reports= 3;
  int kind= (int) errorData.errorType;
  if (kind < 0 || kind >= 16) kind= 15;
  int seen= reported[kind]++;
  if (seen >= max_reports) return;
  cout << "TeXmacs] Clay error (" << kind << "): "
       << string (errorData.errorText.chars, errorData.errorText.length) << LF;
  char report[512];
  int full= vue_clay_capacity_report (report, 512);
  cout << "TeXmacs]   " << string (report) << LF;
  cout << "TeXmacs]   window " << (current_window != NULL
                                   ? as_string (current_window->id)
                                   : string ("none"));
  if (current_window != NULL && N(current_window->name) > 0)
    cout << " (" << current_window->name << ")";
  cout << ", last widget laid out: "
       << (N(layout_who) > 0 ? layout_who : string ("?")) << LF;
  if (kind == (int) CLAY_ERROR_TYPE_INTERNAL_ERROR)
    cout << "TeXmacs]   " << (full > 0
          ? string ("an internal array is full: raise its capacity")
          : string ("no array is full: an out of bounds access, report it "
                    "to Clay with the element above")) << LF;
  if (seen + 1 == max_reports)
    cout << "TeXmacs]   (further errors of this kind are not reported)" << LF;
}

#ifdef VUE_SDL_RENDERER
static TTF_Font **ttf_fonts= NULL; // fonts cache
#endif

// The layout context of a window (SDL or virtual): its own Clay context and
// arena. The previous context is restored, since a window may be created in
// the middle of the layout of another one.
static void
init_window_clay (vue_window_rep* w, int win_w, int win_h) {
  Clay_Context *save_ctx= Clay_GetCurrentContext ();
  // the element hash map must hold the ids of the previous frame and of
  // the current one together (the stale ones go at the next layout), so
  // a window whose widgets are rebuilt (a tool with a long list) needs
  // twice its largest frame; the default 8192 was exceeded by the macros
  // editor
  Clay_SetMaxElementCount (32768);
  uint64_t totalMemorySize= Clay_MinMemorySize ();
  static bool reported= false;
  if (!reported) { cout << "Vue: Clay arena " << (totalMemorySize >> 20) << " MB per window" << LF; reported= true; }
  w->clay_arena= (Clay_Arena) {
      .memory=  (char*) SDL_malloc (totalMemorySize),
      .capacity= (size_t) totalMemorySize
  };
  if (w->clay_arena.memory == NULL) FAILED ("Vue: cannot allocate the layout arena");
  w->clay_ctx= Clay_Initialize (w->clay_arena, (Clay_Dimensions) { (float) win_w, (float) win_h }, (Clay_ErrorHandler) { HandleClayErrors });
  Clay_SetCurrentContext (save_ctx);
  w->clay_debug= false;
  w->last_layout_time= 0;
  w->transitions_active= false;
}

#ifndef __EMSCRIPTEN__
// The logo of TeXmacs Vue (misc/icons/vue-logo), as the icon of its windows
// (in the Dock of macOS, the task bar elsewhere): read once, with MuPDF, and
// with its alpha no longer premultiplied, as SDL wants it
static SDL_Surface*
vue_logo_surface () {
  static SDL_Surface* icon= NULL;
  static bool tried= false;
  if (tried) return icon;
  tried= true;
  url u= resolve (url ("$TEXMACS_PATH") * url ("misc/images/texmacs-vue-256.png"));
  if (is_none (u)) return NULL;
  fz_image* im= mupdf_load_image (u);
  fz_pixmap* pix= mupdf_pixmap_from_image (im);
  fz_context* ctx= mupdf_context ();
  if (im != NULL) fz_drop_image (ctx, im);
  if (pix == NULL) return NULL;
  int w= fz_pixmap_width (ctx, pix), h= fz_pixmap_height (ctx, pix);
  if (fz_pixmap_components (ctx, pix) == 4 && fz_pixmap_alpha (ctx, pix)) {
    icon= SDL_CreateSurface (w, h, SDL_PIXELFORMAT_RGBA32);
    if (icon != NULL) {
      unsigned char* src= fz_pixmap_samples (ctx, pix);
      int stride= fz_pixmap_stride (ctx, pix);
      for (int y= 0; y < h; y++) {
        unsigned char* p= src + y * stride;
        unsigned char* q= ((unsigned char*) icon->pixels) + y * icon->pitch;
        for (int x= 0; x < w; x++, p += 4, q += 4) {
          int a= p[3];
          for (int c= 0; c < 3; c++) q[c]= a == 0 ? 0 : (unsigned char) min (255, (p[c] * 255 + a / 2) / a);
          q[3]= (unsigned char) a;
        }
      }
    }
  }
  fz_drop_pixmap (ctx, pix);
  return icon;
}
#endif

vue_sdl_base_window_rep::vue_sdl_base_window_rep (vue_widget _content, string _name, bool _popup,
                                                  SDL_Window* adopt)
: vue_window_rep (_content, _name, _popup), Min_w (0), Min_h (0), Max_w (0), Max_h (0),
  on_top (false), level_raised (false), saved_x (0), saved_y (0), saved_w (-1), saved_h (-1)
{
  if (DEBUG_VUE) debug_widgets << "create vue_sdl_base_window_rep " << id << (popup ? " (popup)" : "") << LF;
  if (adopt != NULL) {
    // the window is already on the screen, with its title and its input
    the_name= name;
    mod_name= name;
    sdl_win= adopt;
    nr_windows++;
    Window_to_window (sdl_win)= (void*) this;
    id= serial++;
    id_to_window (id)= this;
    int win_w, win_h;
    SDL_GetWindowSize (sdl_win, &win_w, &win_h);
    set_identifier (abstract (content), id);
    notify_position (abstract (content), 0, 0);
    notify_size (abstract (content), win_w, win_h);
    init_window_clay (this, win_w, win_h);
    update_density ();
    visible_requested= ready_to_show= true;
    shown= !(SDL_GetWindowFlags (sdl_win) & SDL_WINDOW_HIDDEN);
    return;
  }
  // windows start hidden and are shown once laid out, see set_visibility
  SDL_WindowFlags flags= SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_RESIZABLE |
                         SDL_WINDOW_HIDDEN;
#ifdef __EMSCRIPTEN__
  // the one window of the browser (the others are virtual) takes the page,
  // and follows its size
  flags |= SDL_WINDOW_FILL_DOCUMENT;
#endif
  if (popup)
    // popups and tooltips are undecorated, start hidden and stay on top;
    // they are shown via SLOT_VISIBILITY once positioned
    flags= SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_BORDERLESS |
           SDL_WINDOW_ALWAYS_ON_TOP | SDL_WINDOW_HIDDEN | SDL_WINDOW_NOT_FOCUSABLE;
  // drawn by the GPU (vue_gpu.cpp): every window has a GL drawable, and the
  // first one makes the context which they all share
  bool gpu= vue_gpu_enabled ();
  if (gpu) { vue_gpu_prepare (); flags |= SDL_WINDOW_OPENGL; }
  int win_w= 200, win_h= 200;
  int win_x=30, win_y= 30;
  // the name a window is created with is its title until TeXmacs gives it
  // one (SLOT_NAME): the two must agree, or the first unsaved change would
  // replace the title by its marker alone
  the_name= name;
  mod_name= name;
  c_string buf (cork_to_utf8 (name));
  sdl_win= SDL_CreateWindow (buf, win_w, win_h, flags);
  if (!sdl_win) {
    // nothing sensible can be done without a window
    SDL_LogError (SDL_LOG_CATEGORY_APPLICATION, "Couldn't create window: %s", SDL_GetError ());
    FAILED ("Vue: cannot create a window");
  }
  // the context exists before anything is drawn: the editors make their
  // backing stores before their window is first drawn
  if (gpu) vue_gpu_attach (sdl_win);
  
  nr_windows++;
  last_created_window= this;
#ifndef __EMSCRIPTEN__
  if (!popup) {
    SDL_Surface* icon= vue_logo_surface ();
    if (icon != NULL) SDL_SetWindowIcon (sdl_win, icon);
  }
#endif
  SDL_SetWindowPosition (sdl_win, win_x, win_y);
  Window_to_window (sdl_win)= (void*) this;
  id= serial++;
  id_to_window (id)= this;
  
  SDL_StartTextInput (sdl_win);
  
  // update widget state
  set_identifier (abstract (content), id);
  notify_position (abstract (content), 0, 0);
  notify_size (abstract (content), win_w,  win_h);
  
  init_window_clay (this, win_w, win_h);
  update_density ();
}

// The pixel density of the display this window is on. The layout works in
// device pixels and the pointer comes in points, so the two are related by
// this factor; the renderers draw at 'retina' pixels per point.
void
vue_sdl_base_window_rep::update_density () {
  float d= SDL_GetWindowPixelDensity (sdl_win);
  if (d <= 0.0f) d= 1.0f;
  // TEXMACS_VUE_DENSITY overrides it: to draw at 1x on a HiDPI display,
  // and to exercise the other path while testing
  static string forced= get_env ("TEXMACS_VUE_DENSITY");
  if (N(forced) > 0 && is_double (forced)) d= (float) as_double (forced);
  int r= max (1, (int) (d + 0.5f));
  if (d == density && r == retina) return;
  density= d;
  retina= r;
  if (DEBUG_VUE)
    SDL_Log ("Window %d: pixel density %.2f, drawing at %dx", id, d, r);
}

vue_sdl_base_window_rep::~vue_sdl_base_window_rep () {
  if (DEBUG_VUE) debug_widgets << "destroy vue_sdl_base_window_rep " << id << LF;
  vue_simple_widget_rep::forget_window (this);
  // forget the weak references of the scripting aid
  if (last_created_window == this) last_created_window= NULL;
  if (script_win == this) script_win= NULL;
  if (snapshot_win == this) snapshot_win= NULL;
  id_to_window->reset (id);
  id= 0;
  set_identifier (abstract (content), 0); // FIXME: is this ok?
  nr_windows--;
  SDL_free (clay_arena.memory);
  // NULL: the SDL window went to another one (see forget_host)
  if (sdl_win == NULL) return;
  Window_to_window->reset (sdl_win);
  SDL_StopTextInput (sdl_win);
  SDL_DestroyWindow (sdl_win);
}

void
vue_sdl_base_window_rep::destroy_event () {
  // the host which outlived its own window closes the windows it holds
  if (close_hosted_windows (this)) return;
  notify_window_destroy (orig_name);
  send_destroy (abstract (content));
}


void
vue_sdl_base_window_rep::get_position (SI& x, SI& y) {
  int xx, yy;
  SDL_GetWindowPosition (sdl_win, &xx, &yy);
  x=  xx * PIXEL;
  y= -yy * PIXEL;
}

void
vue_sdl_base_window_rep::get_size (SI& ww, SI& hh) {
  int win_w, win_h;
  SDL_GetWindowSize (sdl_win, &win_w, &win_h);
  ww= win_w * PIXEL;
  hh= win_h * PIXEL;
}

void
vue_sdl_base_window_rep::get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h) {
  min_w= Min_w; min_h= Min_h; max_w= Max_w; max_h= Max_h;
}

// The window is kept on the display it is put on, the one which holds most
// of it (or the nearest), in the part of that display which is not taken
// by the menu bar and the dock. The displays are in one plane of points,
// where the ones on the left of or above the primary display have negative
// coordinates: those are allowed.
void
vue_sdl_base_window_rep::set_position (SI x, SI y) {
  int win_w, win_h;
  SDL_GetWindowSize (sdl_win, &win_w, &win_h);

  int win_x= x/PIXEL;
  int win_y= -y/PIXEL;
  SDL_Rect wr= { win_x, win_y, max (win_w, 1), max (win_h, 1) };
  SDL_DisplayID d= SDL_GetDisplayForRect (&wr);
  if (d == 0) d= SDL_GetPrimaryDisplay ();
  SDL_Rect r;
  if (d != 0 && (SDL_GetDisplayUsableBounds (d, &r) ||
                 SDL_GetDisplayBounds (d, &r))) {
    if (win_x + win_w > r.x + r.w) win_x= r.x + r.w - win_w;
    if (win_x < r.x) win_x= r.x;
    if (win_y + win_h > r.y + r.h) win_y= r.y + r.h - win_h;
    if (win_y < r.y) win_y= r.y;
  }
  if (DEBUG_VUE_EVENTS) SDL_Log ("Window %d set_position %d %d", id, win_x, win_y);
  SDL_SetWindowPosition (sdl_win, win_x, win_y);
}

void
vue_sdl_base_window_rep::set_size (SI w, SI h) {
  w= w/PIXEL; h= h/PIXEL;
  //h=-h; ren->decode (w, h);
  SDL_SetWindowSize (sdl_win, w, h);
}

void
vue_sdl_base_window_rep::set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) {
  if (min_w == Min_w && min_h == Min_h && max_w == Max_w && max_h == Max_h)
    return;
  Min_w= min_w; Min_h= min_h; Max_w= max_w; Max_h= max_h;
  // a limit of 0 means no limit for SDL
  SDL_SetWindowMinimumSize (sdl_win, max (min_w/PIXEL, 0), max (min_h/PIXEL, 0));
  SDL_SetWindowMaximumSize (sdl_win, max (max_w/PIXEL, 0), max (max_h/PIXEL, 0));
}

// The title of a window is the name TeXmacs gives it plus, for a document
// with unsaved changes, a marker (the Qt port draws the same through
// setWindowModified and the [*] of its title). The two halves are kept
// apart, so that a change to one does not undo the other: setting the name
// dropped the marker until the document was saved and changed again, and
// setting the marker on a window whose name had never arrived turned the
// whole title into " *".
void
vue_sdl_base_window_rep::update_title () {
  string name= modified ? the_name * " *" : the_name;
  if (DEBUG_VUE_WIDGETS) debug_widgets << "window title: " << name << LF;
  if (mod_name == name) return;
  mod_name= name;
  // SDL takes UTF-8, the names of TeXmacs are in its own encoding
  c_string s (cork_to_utf8 (name));
  SDL_SetWindowTitle (sdl_win, s);
}

void
vue_sdl_base_window_rep::set_name (string name) {
  if (the_name != name) { the_name= name; update_title (); }
}

string
vue_sdl_base_window_rep::get_name () {
  return the_name;
}

void
vue_sdl_base_window_rep::set_modified (bool flag) {
  if (modified != flag) { modified= flag; update_title (); }
}

void
vue_sdl_base_window_rep::set_full_screen (bool flag) {
  // presentation and full screen modes (SLOT_FULL_SCREEN)
  if (!SDL_SetWindowFullscreen (sdl_win, flag))
    SDL_Log ("SDL_SetWindowFullscreen failed: %s", SDL_GetError ());
  // the change is asynchronous (an animation on macOS); presentation mode
  // fits the slide to the window just after it, so wait for the new size
  else SDL_SyncWindow (sdl_win);
}

// A window on top (a tool, SLOT_ON_TOP) stays above the windows of TeXmacs,
// not above those of the other applications, as the Qt::Tool of the Qt port
// and the NS port. SDL's "always on top" alone floats above everything, so:
// on macOS the window hides with the application, as a panel of Cocoa does
// (setHidesOnDeactivate, on the NSWindow of SDL; SDL gives it the floating
// level); elsewhere it leaves that level while no window of TeXmacs has the
// keyboard, see follow_app_focus. A parent window (SDL_SetWindowParent)
// would do on some systems, but moves the tool with its parent on macOS and
// destroys it with it, which the windows of TeXmacs do not expect.
void
vue_sdl_base_window_rep::set_on_top (bool flag) {
  on_top= flag;
  level_raised= flag;
  SDL_SetWindowAlwaysOnTop (sdl_win, flag);
#ifdef OS_MACOS
  void* ns= SDL_GetPointerProperty (SDL_GetWindowProperties (sdl_win),
                                    SDL_PROP_WINDOW_COCOA_WINDOW_POINTER, NULL);
  if (ns != NULL)
    ((void (*) (objc_object*, SEL, BOOL)) objc_msgSend)
      ((objc_object*) ns, sel_registerName ("setHidesOnDeactivate:"), flag ? YES : NO);
#endif
}

void
vue_sdl_base_window_rep::follow_app_focus () {
  if (!on_top) return;
  bool active= (SDL_GetKeyboardFocus () != NULL);
  if (active == level_raised) return;
  level_raised= active;
  SDL_SetWindowAlwaysOnTop (sdl_win, active);
}

// The geometry of a window which the user changed is kept in the
// preferences, as the Qt port does (moveEvent and resizeEvent of
// QTMWindow), where texmacs_window_widget finds it for the next window of
// that name ("abscissa TeXmacs" and friends). The windows are compared to
// what they were at the previous frame: SDL reports the moves while the
// loop may be held (a move is modal on macOS), and one write per frame
// instead of one per event. The host of single-window mode takes the
// virtual windows along.
void
vue_sdl_base_window_rep::track_geometry () {
  int x, y, w, h;
  SDL_GetWindowPosition (sdl_win, &x, &y);
  SDL_GetWindowSize (sdl_win, &w, &h);
  bool moved= (x != saved_x || y != saved_y);
  bool resized= (w != saved_w || h != saved_h);
  bool first= (saved_w < 0);
  if (!moved && !resized) return;
  saved_x= x; saved_y= y; saved_w= w; saved_h= h;
  host_changed ();
  // the first geometry is the one TeXmacs gave; popups are not remembered
  // (nor by the other ports), nor is the host which outlived its window
  if (first || popup || N(orig_name) == 0) return;
  if (moved) notify_window_move (orig_name, x * PIXEL, -y * PIXEL);
  if (resized) notify_window_resize (orig_name, w * PIXEL, h * PIXEL);
}

void
vue_sdl_base_window_rep::set_visibility (bool flag) {
  visible_requested= flag;
  if (!flag) {
    if (shown) SDL_HideWindow (sdl_win);
    shown= false;
  }
  else if (ready_to_show && !shown) {
    SDL_ShowWindow (sdl_win);
    shown= true;
  }
  // the window of an editor already shown comes to the front with the
  // focus (switch_to_window maps the window of a buffer to switch to it:
  // the Go menu, switch-to-buffer*)
  else if (shown && document) SDL_RaiseWindow (sdl_win);
  // otherwise the window is shown by process_layout once it fits its contents
}
 
// The layout of a window (SDL or virtual) at the size it has in device
// pixels, with the passes its contents ask for
static void clay_wheel_flush (vue_window_rep* win);

static void
layout_window_passes (vue_window_rep* w) {
  bool relayout= false;
  int passes= 0;
  do {
    with_window frame (w);
    layout_again= false;
    // init the current GUI context
    int win_w, win_h;
    w->layout_size (win_w, win_h);
    Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) win_w, (float) win_h });
    w->layout_w= win_w; w->layout_h= win_h;
    gui_init_context ();

    // layout the top widget
    Clay_SetDebugModeEnabled (w->clay_debug);
    Clay__debugViewWidth= 600; // redefine to have more space
    Clay_BeginLayout ();
    w->content->do_layout ();
    // the frame time drives the transitions declared by the elements
    // (.transition, see the notes on animation in vue-graphics-stack.md)
    time_t now= texmacs_time ();
    float dt= (w->last_layout_time == 0) ? 0.0f : (float) (now - w->last_layout_time) / 1000.0f;
    w->last_layout_time= now;
    w->render_commands= Clay_EndLayout (min (dt, 0.1f));
    w->transitions_active= vue_clay_transitions_active (); // clay.c
    gui_finalize_context ();

    // post layout tweaking
    relayout= w->content->post_layout () || layout_again;
  } while (relayout && ++passes < 5);
  {
    with_window frame (w);
    clay_wheel_flush (w);
  }
}

void
vue_sdl_base_window_rep::layout_size (int& w, int& h) {
  SDL_GetWindowSizeInPixels (sdl_win, &w, &h);
}

void
vue_sdl_base_window_rep::process_layout () {
  layout_window_passes (this);

  // show the window once it fits its contents (or after a few passes, in
  // case the contents never settle)
  layout_passes++;
  if (visible_requested && !shown && (ready_to_show || layout_passes > 10)) {
    SDL_ShowWindow (sdl_win);
    shown= true;
  }
}

void snapshot_pixmap (fz_context* ctx, fz_pixmap *pix);

static void
save_pixmap_as_png (fz_context *ctx, fz_pixmap *pix, string path) {
  c_string cpath (path);
  fz_output *out= NULL;
  fz_pixmap *rgb_pix= NULL;
  fz_var (out);
  fz_var (rgb_pix);
  fz_try (ctx) {
    rgb_pix= fz_convert_pixmap (ctx, pix, fz_device_rgb (ctx),
                                NULL, NULL, fz_default_color_params, 1);
    out= fz_new_output_with_path (ctx, cpath, 0);
    fz_write_pixmap_as_png (ctx, out, rgb_pix);
  }
  fz_always (ctx) {
    fz_drop_pixmap (ctx, rgb_pix);
    fz_close_output (ctx, out);
    fz_drop_output (ctx, out);
  }
  fz_catch (ctx) {
    cout << "Fitz error in save_pixmap_as_png: " << fz_caught_message (ctx) << LF;
  }
}

#ifdef VUE_SDL_RENDERER
//******************************************************************************
// Rendering through SDL's own renderer and the example Clay renderer
// (clay_renderer_SDL3.c), as an alternative to the MuPDF path below.
// Unused: plain_window creates a vue_sdl_mupdf_window_rep. Kept as the
// starting point of a GPU path; the font is a hardcoded personal one.

class vue_sdl_window_rep : public vue_sdl_base_window_rep {
public:
  SDL_Renderer *sdl_ren;
  TTF_TextEngine *text_engine;

  vue_sdl_window_rep (vue_widget w, string name, bool popup= false);
  ~vue_sdl_window_rep ();
  
  void process_redraw ();
  void draw_picture (void *data, picture pic);
  void get_viewport_size (void *data, int& w, int& h);
};


typedef struct {
    SDL_Renderer *renderer;
    TTF_TextEngine *textEngine;
    TTF_Font **fonts;
} Clay_SDL3RendererData;

struct vue_render_data {
  SDL_Renderer *sdl_ren;
  SDL_FRect *rect;
};

extern "C"  {
void SDL_Clay_RenderClayCommands (Clay_SDL3RendererData *rendererData, Clay_RenderCommandArray *rcommands);
void
vue_render (SDL_Renderer *sdl_ren, void *data, SDL_FRect *rect) {
  vue_widget w ((vue_widget_rep*)data);
  vue_render_data args= { sdl_ren, rect };
  w->render (&args);
}
}


void
sdl_draw_picture (SDL_Renderer *sdl_ren, picture pic, SDL_FRect *dest) {
  // propagate immediately the changes to the screen
  fz_pixmap *pix=  ((mupdf_picture_rep*)pic->get_handle())->pix;
  fz_context *ctx= mupdf_context ();

  snapshot_pixmap (ctx, pix);
  unsigned char *pixels= fz_pixmap_samples (ctx, pix);
  int w= fz_pixmap_width (ctx, pix);
  int h= fz_pixmap_height (ctx, pix);
  SDL_Surface *surf= SDL_CreateSurfaceFrom (w, h, SDL_PIXELFORMAT_RGBA32, pixels, 4*w);
  // FIXME: premultiplied?
  SDL_Texture *tex= SDL_CreateTextureFromSurface (sdl_ren, surf);
  SDL_SetTextureBlendMode (tex, SDL_BLENDMODE_BLEND);
  //SDL_SetRenderDrawColor (sdl_ren, 0, 255, 0,  SDL_ALPHA_OPAQUE);
  SDL_FRect src= { 0, 0, (float)w, (float)h };
  //SDL_RenderFillRect (sdl_ren, dest);
  SDL_RenderTexture (sdl_ren, tex, &src, dest);
  SDL_DestroyTexture (tex);
  SDL_DestroySurface (surf);
}

void
vue_sdl_window_rep::draw_picture (void *data, picture pic) {
  sdl_draw_picture (((vue_render_data*)data)->sdl_ren, pic,
                    ((vue_render_data*)data)->rect);
}

void
vue_sdl_window_rep::get_viewport_size (void *data, int& w, int& h) {
  w= (int) ((vue_render_data*)data)->rect->w;
  h= (int) ((vue_render_data*)data)->rect->h;
}

vue_sdl_window_rep::vue_sdl_window_rep (vue_widget w, string name, bool popup)
  : vue_sdl_base_window_rep (w, name, popup), sdl_ren (NULL), text_engine (NULL)
{
  if (!sdl_ren) {
    sdl_ren= SDL_CreateRenderer (sdl_win, NULL);
    if (!sdl_ren) {
      SDL_LogError (SDL_LOG_CATEGORY_ERROR, "Failed to create renderer: %s", SDL_GetError ());
    }
  }
  
  if (!text_engine) {
    text_engine= TTF_CreateRendererTextEngine (sdl_ren);
    if (!text_engine) {
      SDL_LogError (SDL_LOG_CATEGORY_ERROR, "Failed to create text engine from renderer: %s", SDL_GetError ());
    }

    if (!ttf_fonts) {
      ttf_fonts= (TTF_Font **)SDL_calloc (1, sizeof(TTF_Font *));
      if (!ttf_fonts) {
        SDL_LogError (SDL_LOG_CATEGORY_ERROR, "Failed to allocate memory for the font array: %s", SDL_GetError ());
        return;
      }
      
      TTF_Font *font= TTF_OpenFont( //"/Users/mgubi/t/clay/examples/SDL3-simple-demo/resources/Roboto-Regular.ttf"
            "/Users/mgubi/.TeXmacs/fonts/unpacked/LucidaGrande.0.ttf",
          24);
      if (!font) {
        SDL_LogError (SDL_LOG_CATEGORY_ERROR, "Failed to load font: %s", SDL_GetError ());
        return;
      }
      ttf_fonts[0]= font;
    }
    {
      with_window frame (this);
      Clay_SetMeasureTextFunction (SDL_MeasureText, ttf_fonts);
    }
  }
}

vue_sdl_window_rep::~vue_sdl_window_rep () {
  TTF_DestroyRendererTextEngine (text_engine);
  SDL_DestroyRenderer (sdl_ren);
}

void
vue_sdl_window_rep::process_redraw () {
  // render!
  SDL_SetRenderDrawColor (sdl_ren, 0, 0, 0, 255);
  SDL_RenderClear (sdl_ren);

  Clay_SDL3RendererData rd { sdl_ren, text_engine, ttf_fonts };
  SDL_Clay_RenderClayCommands (&rd, &render_commands);

  SDL_RenderPresent (sdl_ren);
}
#endif // VUE_SDL_RENDERER

//******************************************************************************
// rendering via MuPDF renderer

array<styled_string> styled_strings;

// single-window mode (see "Single-window mode" below)
static void composite_virtual_windows (vue_window host, renderer ren);
static int32_t host_overlay_start (Clay_RenderCommandArray& a);
static bool is_host (vue_window w);
static void forget_host (vue_window w);

class vue_sdl_mupdf_window_rep : public vue_sdl_base_window_rep {
public:
  renderer ren;
  picture backing_store;

  vue_sdl_mupdf_window_rep (vue_widget w, string name, bool popup= false,
                            SDL_Window* adopt= NULL);
  // (no renderer: a window which was never shown was never drawn)
  ~vue_sdl_mupdf_window_rep () {
    forget_host (this); if (ren != NULL) delete_renderer (ren); }
  
  void process_redraw ();
  void process_layout ();
  
  void draw_picture (void *data, picture pic);
  void get_viewport_size (void *data, int& w, int& h);
};

void render_clay_commands (renderer ren, Clay_RenderCommandArray *rcommands);

Clay_Dimensions
ren_measure_text (Clay_StringSlice text, Clay_TextElementConfig *config, void *userData) {
  // the measured text is drawn by CLAY_TEXT elements (the debug view only:
  // the widgets draw their own text, see layout_text_box); every path must
  // return, a missing one was undefined behaviour
  (void) config; (void) userData;
  string s (text.chars, text.length);
  static font fn;
  if (is_nil (fn)) fn= get_default_styled_font (0);
  metric ex;
  fn->var_get_extents (s, ex);
  SI w= ((ex->x2 - ex->x1 + 2)/3);
  SI h= ((fn->y2 - fn->y1 + 2)/3);
  abs_round (w, h);
  return (Clay_Dimensions) { .width= (float) retina_factor*w / PIXEL,
                             .height= (float) retina_factor*h / PIXEL };
}

vue_sdl_mupdf_window_rep::vue_sdl_mupdf_window_rep (vue_widget w, string name, bool popup,
                                                    SDL_Window* adopt)
  : vue_sdl_base_window_rep (w, name, popup, adopt), ren (NULL)
{
  with_window frame (this);
  Clay_SetMeasureTextFunction (ren_measure_text, this);
};

void
vue_sdl_mupdf_window_rep::process_layout () {
  vue_sdl_base_window_rep::process_layout ();
}

void
sdl_draw_picture (SDL_Surface *dest_surf, picture pic, SDL_FRect *dest) {
  // propagate immediately the changes to the screen
  fz_pixmap *pix= ((mupdf_picture_rep*)pic->get_handle())->pix;
  fz_context *ctx= mupdf_context ();
  snapshot_pixmap (ctx, pix);
  unsigned char *pixels= fz_pixmap_samples (ctx, pix);
  int w= fz_pixmap_width (ctx, pix);
  int h= fz_pixmap_height (ctx, pix);
  if (dest_surf == NULL) return;
  SDL_Surface *surf= SDL_CreateSurfaceFrom (w, h, SDL_PIXELFORMAT_RGBA32, pixels, 4*w);
  if (surf == NULL) {
    SDL_Log ("SDL_CreateSurfaceFrom failed: %s", SDL_GetError ());
    return;
  }
  // FIXME: premultiplied?
  if (!SDL_BlitSurface (surf, 0, dest_surf, 0))
    SDL_Log ("SDL_BlitSurface failed: %s", SDL_GetError ());
  SDL_DestroySurface (surf);
}

picture
native_picture_from_SDL_Surface (SDL_Surface *surf) {
  fz_context *ctx= mupdf_context ();
  fz_pixmap *pix= NULL;
  // the window surface is wrapped, not copied; a 1x1 pixmap replaces it if
  // MuPDF refuses (nothing is then drawn in this frame)
  // SDL only promises the format which suits the window best: check that
  // it is four bytes per pixel and use its own pitch (the rows may be
  // padded, which sheared the image when 4*w was assumed)
  // and the order of its bytes: B, G, R, A for the formats of macOS
  // (ARGB8888), R, G, B, A for that of the browser (RGBA32, i.e. ABGR8888
  // on a little endian machine); taking one for the other exchanged the
  // red and the blue of everything
  fz_colorspace* cs= mupdf_screen_colorspace ();
  if (surf != NULL) {
    if (surf->format == SDL_PIXELFORMAT_ABGR8888 ||
        surf->format == SDL_PIXELFORMAT_XBGR8888) cs= fz_device_rgb (ctx);
    else if (surf->format == SDL_PIXELFORMAT_ARGB8888 ||
             surf->format == SDL_PIXELFORMAT_XRGB8888) cs= fz_device_bgr (ctx);
  }
  bool ok= (surf != NULL) && SDL_BYTESPERPIXEL (surf->format) == 4 &&
           mupdf_protected ("window surface", [&] () {
    pix= fz_new_pixmap_with_data (ctx, cs,
                                  surf->w, surf->h, NULL, 1, surf->pitch,
                                  (unsigned char*) surf->pixels);
  });
  if (surf != NULL && SDL_BYTESPERPIXEL (surf->format) != 4) {
    static bool reported= false;
    if (!reported) {
      reported= true;
      SDL_Log ("unsupported window surface format %s (%d bytes per pixel)",
               SDL_GetPixelFormatName (surf->format), SDL_BYTESPERPIXEL (surf->format));
    }
  }
  if (!ok) pix= mupdf_new_pixmap (1, 1);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (ctx, pix);
  return p;
}

// the two halves of the redraw, summed over the windows of one frame and
// read by the profiler in gui_start_loop
bool     vue_profile_on= false; // TEXMACS_VUE_PROFILE is set
uint64_t vue_clay_ns= 0, vue_upload_ns= 0;
uint64_t vue_fill_ns= 0;   // the full-window background fill
int      vue_commands= 0;  // render commands replayed
// time and count of the replay, by Clay command type (1 rectangle,
// 2 border, 3 text, 4 image, 5/6 scissor, 7/8 overlay, 9 custom)
uint64_t vue_cmd_ns[12];
int      vue_cmd_n[12];
// the custom commands, split: the texts, the editors' backing stores and
// the other widgets (icons, check boxes, colour cells...)
uint64_t vue_text_ns= 0, vue_editor_ns= 0, vue_other_ns= 0;
int      vue_text_n= 0,  vue_editor_n= 0,  vue_other_n= 0;


// The parts of a w x h window (device pixels, y down) which no render
// command paints with an opaque color: the background of the window is
// filled there only. Filling all of it at every frame, as before, cost two
// thirds of a frame of the browser build, where the editors and the bars
// paint nearly everything anyway. Counted as opaque: the rectangles of an
// opaque color without rounded corners, and the widgets which say so
// (renders_opaque: the editors), each within the clip it is drawn in and
// one pixel in from its edges (where the rounding of the renderer could
// leave a pixel unpainted; those pixels are filled, then painted over).
// A pixel which is filled or not ends the same: every pixel left out is
// painted opaque later in the frame, over whatever it held.
static rectangles
uncovered_area (Clay_RenderCommandArray* rc, int w, int h) {
  rectangle win (0, 0, w, h);
  rectangles covered;
  array<rectangle> clips;
  for (int32_t i= 0; i < rc->length; i++) {
    Clay_RenderCommand* cmd= Clay_RenderCommandArray_Get (rc, i);
    Clay_BoundingBox bb= cmd->boundingBox;
    rectangle box ((SI) ceil (bb.x) + 1, (SI) ceil (bb.y) + 1,
                   (SI) floor (bb.x + bb.width) - 1,
                   (SI) floor (bb.y + bb.height) - 1);
    rectangle clip= N(clips) == 0 ? win : clips[N(clips) - 1];
    bool opaque= false;
    switch (cmd->commandType) {
      case CLAY_RENDER_COMMAND_TYPE_SCISSOR_START: {
        rectangle c ((SI) floor (bb.x), (SI) floor (bb.y),
                     (SI) ceil (bb.x + bb.width), (SI) ceil (bb.y + bb.height));
        clips << rectangle (max (c->x1, clip->x1), max (c->y1, clip->y1),
                            min (c->x2, clip->x2), min (c->y2, clip->y2));
        break;
      }
      case CLAY_RENDER_COMMAND_TYPE_SCISSOR_END:
        if (N(clips) > 0) clips->resize (N(clips) - 1);
        break;
      case CLAY_RENDER_COMMAND_TYPE_RECTANGLE: {
        Clay_RectangleRenderData* d= &cmd->renderData.rectangle;
        opaque= d->backgroundColor.a >= 255 &&
                d->cornerRadius.topLeft <= 0 && d->cornerRadius.topRight <= 0 &&
                d->cornerRadius.bottomLeft <= 0 && d->cornerRadius.bottomRight <= 0;
        break;
      }
      case CLAY_RENDER_COMMAND_TYPE_CUSTOM:
        opaque= cmd->renderData.custom.customData == vue_render_widget &&
                cmd->userData != NULL &&
                ((vue_widget_rep*) cmd->userData)->renders_opaque
                  ((int) ceil (bb.width), (int) ceil (bb.height));
        break;
      default:
        break;
    }
    if (!opaque) continue;
    SI x1= max (box->x1, clip->x1), y1= max (box->y1, clip->y1);
    SI x2= min (box->x2, clip->x2), y2= min (box->y2, clip->y2);
    // small ones are not worth the fragments they cut the rest into
    if (x2 - x1 >= 32 && y2 - y1 >= 32)
      covered= rectangles (rectangle (x1, y1, x2, y2), covered);
  }
  return rectangles (win) - covered;
}

void
vue_sdl_mupdf_window_rep::process_redraw () {
  // a hidden or minimized window is neither drawn nor uploaded (the loop
  // redraws every window at every frame); it is drawn again when shown
  if (!shown || (SDL_GetWindowFlags (sdl_win) &
                 (SDL_WINDOW_HIDDEN | SDL_WINDOW_MINIMIZED))) return;
  track_geometry ();
#ifndef OS_MACOS
  follow_app_focus ();
#endif
  with_window frame (this);
  int win_w, win_h;

  SDL_Surface *surf= SDL_GetWindowSurface(sdl_win);
  if (surf == NULL) {
    // e.g. a window being destroyed: nothing to draw on (reported a few
    // times only, it may go on for every frame)
    static int reported= 0;
    if (reported++ < 3)
      SDL_Log ("SDL_GetWindowSurface failed: %s", SDL_GetError ());
    return;
  }
  backing_store= native_picture_from_SDL_Surface (surf);
  fz_pixmap *pix= ((mupdf_picture_rep*)backing_store->get_handle())->pix;
  fz_context *ctx= mupdf_context ();

  if (!ren) {
    ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
  } else {
    static_cast<mupdf_renderer_rep*>(ren)->begin (pix);
  }
  // NOTE: no clipping of its own at the start of a frame (the renderer
  // keeps the one of the size of its first frame, and the elements which
  // clip are intersected with it, see SCISSOR_START); the device clips to
  // the surface
  ren->cx1= ren->ox - (1 << 28); ren->cx2= ren->ox + (1 << 28);
  ren->cy1= ren->oy - (1 << 28); ren->cy2= ren->oy + (1 << 28);
  
  win_w = surf->w;
  win_h = surf->h;
    
  time_t t1, t2;
  t2= texmacs_time ();
  uint64_t t_ns= vue_profile_on ? SDL_GetTicksNS () : 0;
  // areas not covered by any element: red in the debug mode (F1) to spot them
  ren->set_pencil (clay_debug ? rgb_color (255, 0, 0)
                              : theme_color (the_theme.background));
  if (clay_debug)
    ren->fill (0, -win_h * ren->pixel, win_w * ren->pixel, 0);
  else
    for (rectangles l= uncovered_area (&render_commands, win_w, win_h);
         !is_nil (l); l= l->next) {
      rectangle u= l->item;
      ren->fill (u->x1 * ren->pixel, -u->y2 * ren->pixel,
                 u->x2 * ren->pixel, -u->y1 * ren->pixel);
    }
  if (vue_profile_on) {
    vue_fill_ns += SDL_GetTicksNS () - t_ns;
    vue_commands += render_commands.length;
  }
  if (is_host (this)) {
    // the virtual windows go between the contents of the host and its
    // floating elements (menus, lists, balloons), see host_overlay_start
    int32_t k= host_overlay_start (render_commands);
    Clay_RenderCommandArray below= render_commands, above= render_commands;
    below.length= k;
    above.internalArray += k; above.length -= k; above.capacity -= k;
    render_clay_commands (ren, &below);
    composite_virtual_windows (this, ren);
    render_clay_commands (ren, &above);
  }
  else render_clay_commands (ren, &render_commands);

    static_cast<mupdf_renderer_rep*>(ren)->end ();

  if (vue_profile_on) vue_clay_ns += SDL_GetTicksNS () - t_ns;
  t1= t2; t2= texmacs_time ();
  if (DEBUG_VUE && t2 - t1 > 30)
    debug_widgets << "render_clay_commands took " << t2 - t1 << "ms" << LF;
  
  // development aid: when TEXMACS_VUE_SNAPSHOT is set to a directory, the
  // rendering of every window is saved there as window-<id>.png at each redraw
  static string snapshot_dir= get_env ("TEXMACS_VUE_SNAPSHOT");
  if (N(snapshot_dir) > 0) {
    save_pixmap_as_png (ctx, pix, snapshot_dir * "/window-" * as_string (id) * ".png");
    // a virtual window is saved with the host which draws it
    bool target= (snapshot_win == this) ||
                 (snapshot_win != NULL && snapshot_win->platform_window () == NULL && is_host (this));
    if (target && N(snapshot_name) > 0) {
      // named snapshot requested by a script
      save_pixmap_as_png (ctx, pix, snapshot_dir * "/" * snapshot_name * ".png");
      snapshot_name= "";
    }
  }

  //SDL_SetRenderDrawColor (sdl_ren, 0, 0, 0, 255);
  //SDL_RenderClear (sdl_ren);
  t_ns= vue_profile_on ? SDL_GetTicksNS () : 0;
  if (!SDL_UpdateWindowSurface (sdl_win)) {
    static int reported= 0;
    if (reported++ < 3) SDL_Log ("SDL_UpdateWindowSurface failed: %s", SDL_GetError ());
  }
  if (vue_profile_on) vue_upload_ns += SDL_GetTicksNS () - t_ns;
  t1= t2; t2= texmacs_time ();
  if (DEBUG_VUE && t2 - t1 > 30)
    debug_widgets << "SDL_UpdateWindowSurface took " << t2 - t1 << "ms" << LF;
}


// see vue_gui.hpp for vue_render_ren_data

void
vue_render_widget_fn (renderer ren, void *w, rectangle r) {
  // the widget is alive as long as this command may be drawn: the layout
  // which produced the command holds a reference (see render_ref)
  vue_render_ren_data data { .ren= ren, .r= r };
  if (!vue_profile_on) { ((vue_widget_rep*)w)->render (&data); return; }
  uint64_t t= SDL_GetTicksNS ();
  bool editor= (((vue_widget_rep*)w)->type == "simple_widget");
  ((vue_widget_rep*)w)->render (&data);
  t= SDL_GetTicksNS () - t;
  if (editor) { vue_editor_ns += t; vue_editor_n++; }
  else { vue_other_ns += t; vue_other_n++; }
}
void *vue_render_widget= (void*)&vue_render_widget_fn;

void
vue_sdl_mupdf_window_rep::draw_picture (void *data, picture pic) {
  vue_render_ren_data* d= (vue_render_ren_data*)data;
  d->ren->draw_picture (pic, d->r->x1, d->r->y1);
}

void
vue_sdl_mupdf_window_rep::get_viewport_size (void *data, int& w, int& h) {
  vue_render_ren_data* d= (vue_render_ren_data*)data;
  w= (d->r->x2 - d->r->x1) / d->ren->pixel;
  h= (d->r->y2 - d->r->y1) / d->ren->pixel;
}

/******************************************************************************
* Windows drawn by the GPU (vue_gpu.cpp): the same commands, replayed on a
* renderer of the default framebuffer, every frame (the editors keep their
* backing stores as textures, which this draws as quads)
******************************************************************************/

class vue_sdl_gpu_window_rep : public vue_sdl_base_window_rep {
public:
  renderer ren;
  unsigned long long presented; // the hash of the frame last presented

  vue_sdl_gpu_window_rep (vue_widget w, string name, bool popup= false,
                          SDL_Window* adopt= NULL)
    : vue_sdl_base_window_rep (w, name, popup, adopt), ren (NULL), presented (0) {
    with_window frame (this);
    Clay_SetMeasureTextFunction (ren_measure_text, this);
  }
  ~vue_sdl_gpu_window_rep () {
    forget_host (this); if (ren != NULL) delete_renderer (ren); }

  void process_redraw ();
  void process_layout () { vue_sdl_base_window_rep::process_layout (); }
  void draw_picture (void *data, picture pic) {
    vue_render_ren_data* d= (vue_render_ren_data*) data;
    d->ren->draw_picture (pic, d->r->x1, d->r->y1); }
  void get_viewport_size (void *data, int& w, int& h) {
    vue_render_ren_data* d= (vue_render_ren_data*) data;
    w= (d->r->x2 - d->r->x1) / d->ren->pixel;
    h= (d->r->y2 - d->r->y1) / d->ren->pixel; }
};

void
vue_sdl_gpu_window_rep::process_redraw () {
  if (!shown || (SDL_GetWindowFlags (sdl_win) &
                 (SDL_WINDOW_HIDDEN | SDL_WINDOW_MINIMIZED))) {
    presented= 0; // shown again: presented again, whatever it draws
    return;
  }
  track_geometry ();
#ifndef OS_MACOS
  follow_app_focus ();
#endif
  with_window frame (this);
  if (!vue_gpu_attach (sdl_win)) return;
  int win_w= 0, win_h= 0;
  SDL_GetWindowSizeInPixels (sdl_win, &win_w, &win_h);
  if (win_w <= 0 || win_h <= 0) return;
  if (ren == NULL) ren= gpu_screen_renderer (std_shrinkf * retina_factor);
  gpu_begin_screen (ren, win_w, win_h);
  // as the MuPDF window: no clip of its own, the elements clip
  ren->cx1= ren->ox - (1 << 28); ren->cx2= ren->ox + (1 << 28);
  ren->cy1= ren->oy - (1 << 28); ren->cy2= ren->oy + (1 << 28);
  uint64_t t_ns= vue_profile_on ? SDL_GetTicksNS () : 0;
  // the background of the theme (red in the F1 debug mode): a clear of the
  // GPU, at no cost worth avoiding
  ren->set_pencil (clay_debug ? rgb_color (255, 0, 0)
                              : theme_color (the_theme.background));
  ren->fill (0, -win_h * ren->pixel, win_w * ren->pixel, 0);
  if (vue_profile_on) {
    vue_fill_ns += SDL_GetTicksNS () - t_ns;
    vue_commands += render_commands.length;
  }
  if (is_host (this)) {
    // the virtual windows go between the contents of the host and its
    // floating elements, as in the MuPDF window (host_overlay_start)
    int32_t k= host_overlay_start (render_commands);
    Clay_RenderCommandArray below= render_commands, above= render_commands;
    below.length= k;
    above.internalArray += k; above.length -= k; above.capacity -= k;
    render_clay_commands (ren, &below);
    composite_virtual_windows (this, ren);
    render_clay_commands (ren, &above);
  }
  else render_clay_commands (ren, &render_commands);
  if (vue_profile_on) gpu_finish (); // with the time the GPU took
  else gpu_flush ();
  if (vue_profile_on) vue_clay_ns += SDL_GetTicksNS () - t_ns;
  // a frame which draws what the last presented one drew is not presented:
  // on macOS SDL_GL_SwapWindow waits for the display (even with no swap
  // interval), and the loop draws every window at every iteration. What
  // is drawn has been drawn into the back buffer, which every frame draws
  // in full, so nothing stale can show
  unsigned long long h= gpu_frame_hash ();
  bool same= (h == presented);
  static string snapshot_dir= get_env ("TEXMACS_VUE_SNAPSHOT");
  if (N(snapshot_dir) > 0) {
    picture shot= gpu_read_screen (win_w, win_h);
    if (!is_nil (shot)) {
      fz_pixmap* pix= ((mupdf_picture_rep*) shot->get_handle ())->pix;
      save_pixmap_as_png (mupdf_context (), pix,
                          snapshot_dir * "/window-" * as_string (id) * ".png");
      bool target= (snapshot_win == this) ||
                   (snapshot_win != NULL && snapshot_win->platform_window () == NULL &&
                    is_host (this));
      if (target && N(snapshot_name) > 0) {
        save_pixmap_as_png (mupdf_context (), pix,
                            snapshot_dir * "/" * snapshot_name * ".png");
        snapshot_name= "";
      }
    }
  }
  if (same) return;
  presented= h;
  t_ns= vue_profile_on ? SDL_GetTicksNS () : 0;
  vue_gpu_present (sdl_win);
  if (vue_profile_on) vue_upload_ns += SDL_GetTicksNS () - t_ns;
}

void
render_clay_commands (renderer ren, Clay_RenderCommandArray *rcommands)
{
  // Clay culls the commands of elements outside the window (e.g. the widgets
  // laid out off-screen to be measured) but not always both ends of a clip:
  // keep track of the clip depth and ignore what is off-screen ourselves
  int clip_depth= 0;
  for (int32_t i = 0; i < rcommands->length; i++) {
    Clay_RenderCommand *rcmd = Clay_RenderCommandArray_Get (rcommands, i);
    const Clay_BoundingBox bounding_box = rcmd->boundingBox;
    static bool dump= N(get_env ("TEXMACS_VUE_DUMP")) > 0;
    if (dump) {
      cout << "DUMP " << (int) rcmd->commandType << " id " << rcmd->id << " box " << bounding_box.x << "," << bounding_box.y << " " << bounding_box.width << "x" << bounding_box.height;
      if (rcmd->commandType == CLAY_RENDER_COMMAND_TYPE_RECTANGLE) cout << " color " << (int) rcmd->renderData.rectangle.backgroundColor.r << "," << (int) rcmd->renderData.rectangle.backgroundColor.g << "," << (int) rcmd->renderData.rectangle.backgroundColor.b << "," << (int) rcmd->renderData.rectangle.backgroundColor.a << " radius " << rcmd->renderData.rectangle.cornerRadius.topLeft;
      if (rcmd->commandType == CLAY_RENDER_COMMAND_TYPE_BORDER) {
        cout << " border " << (int) rcmd->renderData.border.width.top << " color " << (int) rcmd->renderData.border.color.r;
        // identify the element: try the known id patterns
        const char* labels[]= { "menu_button", "division_widget", "enum_widget", "input_text_widget",
          "toggle_widget", "tabs_widget", "icon_tabs_widget", "filtered_choice_widget", "filtered_choice_list",
          "choice_widget", "tree_view_widget", "resize_widget", "simple_widget", "texmacs_widget",
          "pulldown_button", "pullright_button", "user_canvas_widget", "aligned_widget", "hsplit_widget", "vsplit_widget", NULL };
        for (int l= 0; labels[l] != NULL; l++) {
          string lab (labels[l]);
          for (unsigned int k= 0; k < 8000; k++) {
            Clay_String cs= { .isStaticallyAllocated= true, .length= (int32_t) N(lab), .chars= &(lab[0]) };
            Clay_ElementId cid= Clay__HashString (cs, k);
            if (cid.id == rcmd->id) { cout << " <" << lab << " " << k << ">"; break; }
          }
        }
      }
      cout << LF;
    }
    bool offscreen= (bounding_box.x + bounding_box.width < 0) ||
                    (bounding_box.y + bounding_box.height < 0);
    if (offscreen && rcmd->commandType != CLAY_RENDER_COMMAND_TYPE_SCISSOR_START &&
        rcmd->commandType != CLAY_RENDER_COMMAND_TYPE_SCISSOR_END) continue;
    rectangle r (bounding_box.x * ren->pixel,
                 -(bounding_box.y + bounding_box.height) * ren->pixel,
                 (bounding_box.x + bounding_box.width)  * ren->pixel,
                 -bounding_box.y * ren->pixel);
    uint64_t t_cmd= vue_profile_on ? SDL_GetTicksNS () : 0;
    switch (rcmd->commandType) {
      case CLAY_RENDER_COMMAND_TYPE_RECTANGLE: {
        Clay_RectangleRenderData *config = &rcmd->renderData.rectangle;
        color c= rgb_color (config->backgroundColor.r, config->backgroundColor.g,
                            config->backgroundColor.b, config->backgroundColor.a);
        ren->set_pencil (c);
        if (config->cornerRadius.topLeft > 0    || config->cornerRadius.topRight > 0 ||
            config->cornerRadius.bottomLeft > 0 || config->cornerRadius.bottomRight > 0) {
          // Draw rounded rectangle
          SI r_tl= (SI) (config->cornerRadius.topLeft * ren->pixel);
          SI r_tr= (SI) (config->cornerRadius.topRight * ren->pixel);
          SI r_br= (SI) (config->cornerRadius.bottomRight * ren->pixel);
          SI r_bl= (SI) (config->cornerRadius.bottomLeft * ren->pixel);
          ren->rounded_rectangle (r->x1, r->y1, r->x2, r->y2, r_tl, r_tr, r_br, r_bl, true);
        } else {
          ren->fill (r->x1, r->y1, r->x2, r->y2);
        }
      } break;
      case CLAY_RENDER_COMMAND_TYPE_TEXT: {
        Clay_TextRenderData *config = &rcmd->renderData.text;
        // config->fontSize
        // config->fontId
        // config->stringContents.chars
        // config->stringContents.length
        ren->set_pencil (rgb_color (config->textColor.r, config->textColor.g,
                                    config->textColor.b, config->textColor.a));
        //FIXME: consider the style of the text element
        static font fn; // the same for every command and frame
        if (is_nil (fn)) fn= get_default_styled_font (0);
        ren->set_shrinking_factor (3);
        string s (config->stringContents.chars,
                  config->stringContents.length);
        fn ->var_draw (ren, s, r->x1*3, r->y1*3- fn->y1);
        ren->set_shrinking_factor (1);
      } break;
      case CLAY_RENDER_COMMAND_TYPE_BORDER: {
        Clay_BorderRenderData *config = &rcmd->renderData.border;
        color c= rgb_color (config->color.r, config->color.g, config->color.b, config->color.a);
        Clay_BorderWidth bw= config->width;
        bool uniform= (bw.left == bw.right && bw.top == bw.bottom && bw.left == bw.top);
        if (!uniform && bw.bottom == 0 && bw.left > 0 &&
            bw.left == bw.top && bw.top == bw.right &&
            (config->cornerRadius.topLeft > 0 || config->cornerRadius.topRight > 0)) {
          // open at the bottom with rounded top corners (the current tab,
          // which merges with the page below): one line along the left
          // side, the top corners and the right side, on the pixels inside
          SI px= ren->pixel, w= bw.top * px, h= w / 2;
          SI x1= r->x1 + h, x2= r->x2 - h, y1= r->y1, y2= r->y2 - h;
          SI rmax= min (x2 - x1, y2 - y1) / 2;
          SI rl= min ((SI) (config->cornerRadius.topLeft  * px), rmax);
          SI rr= min ((SI) (config->cornerRadius.topRight * px), rmax);
          array<SI> xs, ys;
          xs << x1; ys << y1;
          const int steps= 8; // each corner as a polyline of 8 segments
          for (int i= 0; i <= steps; i++) {
            double t= (M_PI / 2) * i / steps; // from the left side to the top
            xs << (SI) (x1 + rl - rl * cos (t)); ys << (SI) (y2 - rl + rl * sin (t));
          }
          for (int i= 0; i <= steps; i++) {
            double t= (M_PI / 2) * i / steps; // from the top to the right side
            xs << (SI) (x2 - rr + rr * sin (t)); ys << (SI) (y2 - rr + rr * cos (t));
          }
          xs << x2; ys << y1;
          ren->set_pencil (pencil (c, w, cap_flat, join_round));
          ren->lines (xs, ys);
          break;
        }
        if (!uniform) {
          // some sides only (the line under a bar, a separator): each side
          // is a filled strip of its own width, no outline, no corners
          SI px= ren->pixel;
          ren->set_pencil (pencil (c));
          if (bw.left > 0)   ren->fill (r->x1, r->y1, r->x1 + bw.left * px, r->y2);
          if (bw.right > 0)  ren->fill (r->x2 - bw.right * px, r->y1, r->x2, r->y2);
          if (bw.top > 0)    ren->fill (r->x1, r->y2 - bw.top * px, r->x2, r->y2);
          if (bw.bottom > 0) ren->fill (r->x1, r->y1, r->x2, r->y1 + bw.bottom * px);
          break;
        }
        // we need a ticker pen, otherwise the corners look blurry (maybe we should use a different method?)
        pencil p= pencil (c, 2*ren->pixel+((config->width.top-1))*ren->pixel, cap_square);
        ren->set_pencil (p);
#ifdef USE_OLD_BORDER_RENDERING
        const float minRadius = min (bounding_box.width, bounding_box.height) / 2.0f;
        const Clay_CornerRadius clampedRadii = {
          .topLeft= (float) min (config->cornerRadius.topLeft, minRadius) * ren->pixel,
          .topRight= (float) min (config->cornerRadius.topRight, minRadius) * ren->pixel,
          .bottomLeft= (float) min (config->cornerRadius.bottomLeft, minRadius) * ren->pixel,
          .bottomRight= (float) min (config->cornerRadius.bottomRight, minRadius) * ren->pixel
        };
        //edges
        if (config->width.left > 0) {
          ren->fill (r->x1 - ren->pixel,
                     r->y1 + clampedRadii.topLeft - ren->pixel,
                     r->x1 + config->width.left * ren->pixel,
                     r->y2 - clampedRadii.bottomLeft + ren->pixel );
        }
        if (config->width.right > 0) {
          ren->fill (r->x2 - config->width.right * ren->pixel,
                     r->y1 + clampedRadii.topRight - ren->pixel,
                     r->x2 + ren->pixel,
                     r->y2 - clampedRadii.bottomRight + ren->pixel );
        }
        if (config->width.top > 0) {
          ren->fill (r->x1 + clampedRadii.topLeft - ren->pixel,
                     r->y2 - config->width.top * ren->pixel,
                     r->x2 - clampedRadii.topRight + ren->pixel,
                     r->y2 + ren->pixel);
        }
        if (config->width.bottom > 0) {
          ren->fill (r->x1 + clampedRadii.bottomLeft - ren->pixel,
                     r->y1 - ren->pixel,
                     r->x2 - clampedRadii.bottomRight + ren->pixel,
                     r->y1 + config->width.bottom * ren->pixel);
        }
        //corners
        if (config->cornerRadius.topLeft > 0) {
          ren->arc (r->x1, r->y2 - clampedRadii.topLeft - ren->pixel,
                    r->x1 + clampedRadii.topLeft, r->y2, 90*64, 90*64);
        }
        if (config->cornerRadius.topRight > 0) {
          ren->arc (r->x2 - clampedRadii.topRight, r->y2 - clampedRadii.topRight,
                    r->x2, r->y2, 0, 90*64);
        }
        if (config->cornerRadius.bottomLeft > 0) {
          ren->arc (r->x1, r->y1,
                    r->x1 + clampedRadii.bottomLeft, r->y1 + clampedRadii.bottomLeft,
                    180*64, 90*64);
        }
        if (config->cornerRadius.bottomRight > 0) {
          ren->arc (r->x2 - clampedRadii.bottomRight, r->y1,
                    r->x2, r->y1 + clampedRadii.bottomRight, 270*64, 90*64);
        }
#else
        // Use rounded_rectangle for borders
        SI r_tl= (SI) (config->cornerRadius.topLeft * ren->pixel);
        SI r_tr= (SI) (config->cornerRadius.topRight * ren->pixel);
        SI r_br= (SI) (config->cornerRadius.bottomRight * ren->pixel);
        SI r_bl= (SI) (config->cornerRadius.bottomLeft * ren->pixel);
        ren->rounded_rectangle (r->x1, r->y1, r->x2, r->y2, r_tl, r_tr, r_br, r_bl, false);
#endif
      } break;
      case CLAY_RENDER_COMMAND_TYPE_SCISSOR_START: {
        // within the clipping in force (renderer_rep::clip replaces it): an
        // element which clips may be larger than what shows it, as the
        // contents of a dialog made smaller, and was drawn outside of it
        clip_depth++;
        SI x1= rcmd->boundingBox.x * ren->pixel;
        SI y1= -(rcmd->boundingBox.y + rcmd->boundingBox.height) * ren->pixel;
        SI x2= (rcmd->boundingBox.x + rcmd->boundingBox.width) * ren->pixel;
        SI y2= -rcmd->boundingBox.y * ren->pixel;
        SI ox1, oy1, ox2, oy2;
        ren->get_clipping (ox1, oy1, ox2, oy2);
        x1= max (x1, ox1); y1= max (y1, oy1);
        x2= max (x1, min (x2, ox2)); y2= max (y1, min (y2, oy2));
        ren->clip (x1, y1, x2, y2);
          break;
      }
      case CLAY_RENDER_COMMAND_TYPE_SCISSOR_END: {
        if (clip_depth > 0) {
          clip_depth--;
          ren->unclip ();
        }
        break;
      }
      case CLAY_RENDER_COMMAND_TYPE_IMAGE: {
        { static bool reported= false; // the widgets draw their own images
          if (!reported) { reported= true; cout << "TeXmacs] Clay image commands are not supported" << LF; } }
          //SDL_Texture *texture = (SDL_Texture *)rcmd->renderData.image.imageData;
          break;
      }
      case CLAY_RENDER_COMMAND_TYPE_CUSTOM: {
        render_fn fn= (render_fn)rcmd->renderData.custom.customData;
        fn (ren, rcmd->userData, r);
        break;
      }
      default:
        SDL_Log ("Unknown render command type: %d", rcmd->commandType);
    }
    int ty= (int) rcmd->commandType;
    if (vue_profile_on && ty >= 0 && ty < 12) {
      vue_cmd_ns[ty] += SDL_GetTicksNS () - t_cmd;
      vue_cmd_n[ty]++;
    }
  }
}

void
vue_render_text_fn (renderer ren, void *w, rectangle r) {
  uint64_t t= vue_profile_on ? SDL_GetTicksNS () : 0;
  styled_string ss= (styled_string_rep *)w;
  ren->set_pencil (ss->c);
  ren->set_shrinking_factor (3);
  ss->fn->var_draw (ren, ss->s, r->x1*3, r->y1*3- ss->fn->y1);
  ren->set_shrinking_factor (1);
  if (vue_profile_on) { vue_text_ns += SDL_GetTicksNS () - t; vue_text_n++; }
}

void *vue_render_text= (void*)&vue_render_text_fn;

// The extents of the texts of the widgets, measured once per (font,
// string): a layout pass runs several times per frame and a menu bar holds
// many unchanging labels. A few fonts are in use at once (the plain, bold
// and small ones of the widgets, alternating in one layout), so each has a
// table of its own; with more fonts, the one used least recently makes
// room, and a full table starts again. Bounded: the texts of a UI are few.
struct text_extents {
  string font_name;
  hashmap<string,int> index;
  array<SI> w, h;
  int last_use;
  text_extents (): index (-1), last_use (-1) {}
};

static text_extents&
text_extents_of (font fn) {
  const int nr_fonts= 8;
  static text_extents tables[nr_fonts];
  static int use_clock= 0;
  int found= -1, oldest= 0;
  for (int i= 0; i < nr_fonts; i++) {
    if (tables[i].last_use >= 0 && tables[i].font_name == fn->res_name) {
      found= i; break; }
    if (tables[i].last_use < tables[oldest].last_use) oldest= i;
  }
  if (found < 0) {
    found= oldest;
    tables[found].font_name= fn->res_name;
    tables[found].index= hashmap<string,int> (-1);
    tables[found].w= array<SI> (); tables[found].h= array<SI> ();
  }
  tables[found].last_use= use_clock++;
  return tables[found];
}

static void
layout_text_box (string s, int style, color c) {
  font fn= get_default_styled_font (style);
  text_extents& cache= text_extents_of (fn);
  SI w, h;
  int idx= cache.index[s];
  if (idx >= 0) { w= cache.w[idx]; h= cache.h[idx]; }
  else {
    metric ex;
    fn->var_get_extents (s, ex);
    w= ((ex->x2- ex->x1+ 2)/3);
    h= ((fn->y2- fn->y1+ 2)/3);
    abs_round (w, h);
    if (N(cache.w) >= 4096) {
      cache.index= hashmap<string,int> (-1);
      cache.w= array<SI> (); cache.h= array<SI> ();
    }
    cache.index (s)= N(cache.w);
    cache.w << w; cache.h << h;
  }
  styled_string ss= tm_new<styled_string_rep> (s, fn, c);
  styled_strings << ss;
  CLAY_AUTO_ID({
    .layout= {
      .sizing= {
        CLAY_SIZING_FIXED((float) retina_factor*w/PIXEL),
        CLAY_SIZING_FIXED((float) retina_factor*h/PIXEL) }},
    .custom= { .customData= vue_render_text },
    .userData= ss.rep
  }) {};
}

extern int context_style; // style flags added by the enclosing divisions

void layout_text (string s, int style, color c) {
  style |= context_style;
  // grey and inert texts are greyed, whatever color was asked for
  if (style & (WIDGET_STYLE_GREY | WIDGET_STYLE_INERT)) c= dark_grey;
  // the widgets (and the core) ask for black and dark grey without knowing
  // about the theme: those two are the text colours of the theme, so that
  // a dark theme does not need every call site to be changed
  if (c == black) c= theme_color (the_theme.text);
  else if (c == dark_grey) c= theme_color (the_theme.text_grey);
  if (style & WIDGET_STYLE_CENTERED) {
    // centered in the space given by the container
    CLAY_AUTO_ID({
      .layout= {
        .sizing= { .width= CLAY_SIZING_GROW(0) },
        .childAlignment= { .x= CLAY_ALIGN_X_CENTER }}})
    {
      layout_text_box (s, style, c);
    }
  }
  else layout_text_box (s, style, c);
}

//******************************************************************************
// Single-window mode: virtual windows
//
// In a browser there is one canvas and no other window. The first window
// becomes the host, and the windows created after it (dialogs, tools,
// balloons, popups) are virtual: each has its layout context and its input
// state as an SDL window has, but no SDL window. The host draws them over
// its own contents (composite_virtual_windows), dialogs with a title bar to
// move and close them and a frame to resize them by its edges and corners,
// and hands them the pointer events which fall on them
// and the keys when one of them has the focus (route_pointer, route_keys).
// Their positions are screen points like those of SDL windows, so that the
// code which places a window relative to another does not change.
//
// Always on in the browser; on the desktop with TEXMACS_VUE_SINGLE_WINDOW.
//******************************************************************************

static bool
single_window_mode () {
#ifdef __EMSCRIPTEN__
  return true;
#else
  static int on= -1;
  if (on < 0) {
    string v= get_env ("TEXMACS_VUE_SINGLE_WINDOW");
    on= (N(v) > 0 && v != "0") ? 1 : 0;
  }
  return on == 1;
#endif
}

class vue_virtual_window_rep;
static vue_sdl_base_window_rep* the_host= NULL;       // holds the others
static bool host_is_bare= false; // the host outlived its own window, see forget_host
static array<vue_virtual_window_rep*> virtual_windows; // back to front
static vue_virtual_window_rep* focused_virtual= NULL;  // gets the keys
static const float title_bar_h= 24.0f;                 // points
static const float frame_w= 4.0f;     // the frame around a dialog, points
static const float frame_grab= 3.0f;  // and outside it, which also grabs it
static const float dialog_min_w= 120.0f, dialog_min_h= 48.0f; // resized

// the content area of the host, in screen points (the virtual windows are
// first brought up to date with a move or a resize of the host)
static void
host_geometry (float& x, float& y, float& w, float& h) {
  x= y= 0; w= h= 1;
  if (the_host == NULL) return;
  host_changed ();
  int ix, iy, iw, ih;
  SDL_GetWindowPosition (the_host->sdl_win, &ix, &iy);
  SDL_GetWindowSize (the_host->sdl_win, &iw, &ih);
  x= ix; y= iy; w= iw; h= ih;
}

class vue_virtual_window_rep : public vue_window_rep {
public:
  float x, y;  // top left corner of the contents, screen points
  float w, h;  // size of the contents, points
  bool  placed; // positioned by TeXmacs (else centered on the host)
  bool  on_top; // above the other virtual windows (see restack)
  // a window may fill the host instead of floating on it, as its contents:
  // in full screen mode (presentations), and the editor which takes the
  // place of the window of a closed host (see promote_editor); the
  // geometry it had is restored when it floats again
  bool  full, promoted;
  float saved_x, saved_y, saved_w, saved_h;
  SI Min_w, Min_h, Max_w, Max_h;

  vue_virtual_window_rep (vue_widget w, string name, bool popup);
  ~vue_virtual_window_rep ();

  void*  platform_window () { return NULL; }
  bool   fills () { return full || promoted; }
  bool   decorated () { return !popup && !fills (); }
  float  top () { return decorated () ? y - title_bar_h : y; }
  int    layer () { return fills () ? 0 : popup ? 3 : on_top ? 2 : 1; }
  void   destroy_event ();
  void   update_title ();
  void   set_name (string n) { if (the_name != n) { the_name= n; update_title (); } }
  string get_name () { return the_name; }
  void   set_modified (bool flag) {
    if (modified != flag) { modified= flag; update_title (); } }
  void   set_visibility (bool flag);
  void   set_full_screen (bool flag);
  void   set_on_top (bool flag);
  void   set_size (SI w, SI h);
  void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h);
  void   get_size (SI& w, SI& h);
  void   get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h);
  void   set_position (SI x, SI y);
  void   get_position (SI& x, SI& y);
  void   update_density ();
  void   layout_size (int& w, int& h);
  void   process_layout ();
  void   process_redraw () {} // drawn by the host, see composite_virtual_windows
  void   draw_picture (void *data, picture pic);
  void   get_viewport_size (void *data, int& w, int& h);

  void   show ();
  void   raise ();
  void   clamp ();
  void   fill ();
  void   set_fills (bool full, bool promoted);
  int    frame_edges (float sx, float sy);
  void   resize_from (int e, float x0, float y0, float w0, float h0,
                      float dx, float dy);
  bool   contains (float sx, float sy) {
    return sx >= x && sx < x + w && sy >= y && sy < y + h; }
  bool   in_title_bar (float sx, float sy) {
    return decorated () && sx >= x && sx < x + w && sy >= y - title_bar_h && sy < y; }
};

static void focus_virtual (vue_virtual_window_rep* v);
static void promote_editor ();
static vue_window pointer_hover= NULL;             // the window under the pointer
static vue_virtual_window_rep* pointer_capture= NULL; // a button is held in it
static vue_virtual_window_rep* drag_win= NULL;     // moved by its title bar
static float drag_dx= 0, drag_dy= 0;
static vue_virtual_window_rep* resize_win= NULL;   // resized by its frame
static int   resize_edges= 0;                      // see frame_edges
static float resize_px, resize_py;                 // where the drag started
static float resize_x0, resize_y0, resize_w0, resize_h0; // and the window then

vue_virtual_window_rep::vue_virtual_window_rep (vue_widget _content, string _name, bool _popup)
  : vue_window_rep (_content, _name, _popup), x (0), y (0), w (200), h (200),
    placed (false), on_top (false), full (false), promoted (false),
    saved_x (0), saved_y (0), saved_w (200), saved_h (200),
    Min_w (0), Min_h (0), Max_w (0), Max_h (0)
{
  if (DEBUG_VUE) debug_widgets << "create vue_virtual_window_rep " << id << (popup ? " (popup)" : "") << LF;
  the_name= name;
  mod_name= name;
  nr_windows++;
  last_created_window= this;
  id= serial++; // as the SDL windows do, so that the ids are the same
  id_to_window (id)= this;
  set_identifier (abstract (content), id);
  notify_position (abstract (content), 0, 0);
  notify_size (abstract (content), (SI) w, (SI) h);
  init_window_clay (this, (int) w, (int) h);
  update_density ();
  {
    with_window frame (this);
    Clay_SetMeasureTextFunction (ren_measure_text, this);
  }
  virtual_windows << this;
  raise (); // below the popups and the windows on top
}

// the windows which a host holds, but the popups (the menus and balloons,
// which go with the window they were opened from)
static int
nr_hosted_windows () {
  int n= 0;
  for (int i= 0; i < N(virtual_windows); i++)
    if (!virtual_windows[i]->popup) n++;
  return n;
}

vue_virtual_window_rep::~vue_virtual_window_rep () {
  if (DEBUG_VUE) debug_widgets << "destroy vue_virtual_window_rep " << id << LF;
  vue_simple_widget_rep::forget_window (this);
  if (last_created_window == this) last_created_window= NULL;
  if (script_win == this) script_win= NULL;
  if (snapshot_win == this) snapshot_win= NULL;
  if (pointer_hover == this) pointer_hover= NULL;
  if (pointer_capture == this) pointer_capture= NULL;
  if (drag_win == this) drag_win= NULL;
  if (focused_virtual == this) focus_virtual (NULL);
  if (resize_win == this) resize_win= NULL;
  array<vue_virtual_window_rep*> rest;
  for (int i= 0; i < N(virtual_windows); i++)
    if (virtual_windows[i] != this) rest << virtual_windows[i];
  virtual_windows= rest;
  id_to_window->reset (id);
  id= 0;
  set_identifier (abstract (content), 0);
  nr_windows--;
  SDL_free (clay_arena.memory);
  // the host which outlived its window: another editor takes the place of
  // this one, and the host goes with the last window it holds
  if (host_is_bare && the_host != NULL) {
    if (nr_hosted_windows () == 0) tm_delete (the_host);
    else if (promoted) promote_editor ();
  }
}

void
vue_virtual_window_rep::destroy_event () {
  notify_window_destroy (orig_name);
  send_destroy (abstract (content));
}

// the name and the marker of unsaved changes, as vue_sdl_base_window_rep
// shows them in its title; the title bar is drawn with mod_name, and the
// host shows the title of the editor which fills it in place of its own
void
vue_virtual_window_rep::update_title () {
  mod_name= modified ? the_name * " *" : the_name;
  if (promoted && the_host != NULL) {
    c_string s (cork_to_utf8 (mod_name));
    SDL_SetWindowTitle (the_host->sdl_win, s);
  }
}

void
vue_virtual_window_rep::update_density () {
  if (the_host == NULL) return;
  density= the_host->density;
  retina= the_host->retina;
}

void
vue_virtual_window_rep::layout_size (int& lw, int& lh) {
  lw= max (1, (int) (w * density + 0.5f));
  lh= max (1, (int) (h * density + 0.5f));
}

void
vue_virtual_window_rep::process_layout () {
  update_density ();
  layout_window_passes (this);
  layout_passes++;
  if (visible_requested && !shown && (ready_to_show || layout_passes > 10))
    show ();
}

// the whole content area of the host
void
vue_virtual_window_rep::fill () {
  float hx, hy, hw, hh;
  host_geometry (hx, hy, hw, hh);
  x= hx; y= hy; w= hw; h= hh;
}

// keep the window, title bar and frame included, on the host; a dialog
// larger than the host is made smaller (its contents scroll, see
// vue_plain_window_widget_rep::do_layout)
void
vue_virtual_window_rep::clamp () {
  if (fills ()) { fill (); return; }
  float hx, hy, hw, hh;
  host_geometry (hx, hy, hw, hh);
  float tb= decorated () ? title_bar_h : 0;
  float f= decorated () ? frame_w : 0;
  if (decorated ()) {
    w= min (w, max (hw - 2*f, 1.0f));
    h= min (h, max (hh - tb - 2*f, 1.0f));
  }
  if (x + w + f > hx + hw) x= hx + hw - w - f;
  if (x - f < hx) x= hx + f;
  if (y + h + f > hy + hh) y= hy + hh - h - f;
  if (y - tb - f < hy) y= hy + tb + f;
}

// the edges of the frame of a dialog under the point (sx, sy): 1 left,
// 2 right, 4 top, 8 bottom, two of them at a corner (which takes some
// length of the sides, to be easy to grab); 0 elsewhere
int
vue_virtual_window_rep::frame_edges (float sx, float sy) {
  if (!decorated () || !shown) return 0;
  float x1= x - frame_w, x2= x + w + frame_w;
  float y1= top () - frame_w, y2= y + h + frame_w;
  if (sx < x1 - frame_grab || sx >= x2 + frame_grab ||
      sy < y1 - frame_grab || sy >= y2 + frame_grab) return 0;
  int e= 0;
  if (sx < x) e |= 1; else if (sx >= x + w) e |= 2;
  if (sy < top ()) e |= 4; else if (sy >= y + h) e |= 8;
  const float corner= 14.0f;
  if (e & 3) { if (sy < y1 + corner) e |= 4; else if (sy >= y2 - corner) e |= 8; }
  if (e & 12) { if (sx < x1 + corner) e |= 1; else if (sx >= x2 - corner) e |= 2; }
  return e;
}

// the frame of a dialog was dragged by (dx, dy) from where the window was
// (x0, y0, w0, h0): its edges e follow, within the host and the limits
void
vue_virtual_window_rep::resize_from (int e, float x0, float y0, float w0,
                                     float h0, float dx, float dy) {
  float hx, hy, hw, hh;
  host_geometry (hx, hy, hw, hh);
  // at most up to the edges of the host, and to the limits of the contents
  float max_w= (e & 1) ? x0 + w0 - (hx + frame_w) : hx + hw - frame_w - x0;
  float max_h= (e & 4) ? y0 + h0 - (hy + title_bar_h + frame_w)
                       : hy + hh - frame_w - y0;
  if (Max_w > 0) max_w= min (max_w, (float) Max_w / PIXEL);
  if (Max_h > 0) max_h= min (max_h, (float) Max_h / PIXEL);
  float nw= w0, nh= h0;
  if (e & 1) nw= w0 - dx; else if (e & 2) nw= w0 + dx;
  if (e & 4) nh= h0 - dy; else if (e & 8) nh= h0 + dy;
  nw= max (min (nw, max_w), min (dialog_min_w, w0));
  nh= max (min (nh, max_h), min (dialog_min_h, h0));
  x= (e & 1) ? x0 + w0 - nw : x0;
  y= (e & 4) ? y0 + h0 - nh : y0;
  w= nw; h= nh;
  placed= true;
  gui_needs_relayout= true;
}

// the pointer of the host: a double arrow over the frame of a dialog
static void
set_frame_cursor (int e) {
  static int current= 0;
  static SDL_Cursor* cursors[16]= { NULL };
  if (e == current) return;
  current= e;
  SDL_SystemCursor c= SDL_SYSTEM_CURSOR_DEFAULT;
  switch (e) {
    case 1: case 2: c= SDL_SYSTEM_CURSOR_EW_RESIZE; break;
    case 4: case 8: c= SDL_SYSTEM_CURSOR_NS_RESIZE; break;
    case 5: case 10: c= SDL_SYSTEM_CURSOR_NWSE_RESIZE; break;
    case 6: case 9: c= SDL_SYSTEM_CURSOR_NESW_RESIZE; break;
    default: e= 0;
  }
  if (cursors[e] == NULL) cursors[e]= SDL_CreateSystemCursor (c);
  if (cursors[e] != NULL) SDL_SetCursor (cursors[e]);
}

void
vue_virtual_window_rep::show () {
  shown= true;
  if (!placed && !fills ()) {
    // centered on the host (a dialog nobody positioned)
    float hx, hy, hw, hh;
    host_geometry (hx, hy, hw, hh);
    x= hx + (hw - w) / 2;
    y= hy + (hh - h) / 2;
  }
  clamp ();
  raise ();
  if (!popup) focus_virtual (this);
}

// The order of the windows, back to front, by layer: those which fill the
// host (they are its contents), the others, the ones on top (tools), and
// the popups and balloons, which are above everything, as the menus and
// the tooltips of the desktop. The order within a layer is kept.
static void
restack () {
  array<vue_virtual_window_rep*> sorted;
  for (int l= 0; l <= 3; l++)
    for (int i= 0; i < N(virtual_windows); i++)
      if (virtual_windows[i]->layer () == l) sorted << virtual_windows[i];
  virtual_windows= sorted;
}

// to the front of its layer
void
vue_virtual_window_rep::raise () {
  array<vue_virtual_window_rep*> rest;
  for (int i= 0; i < N(virtual_windows); i++)
    if (virtual_windows[i] != this) rest << virtual_windows[i];
  virtual_windows= rest << this;
  restack ();
}

// Above the other windows, or back among them. Only the layer changes: the
// window is not raised (turning it off let it jump in front of the others),
// it ends up at the bottom of the windows on top, or at the top of the rest.
void
vue_virtual_window_rep::set_on_top (bool flag) {
  if (on_top == flag) return;
  on_top= flag;
  restack ();
}

// full: set_full_screen; promoted: see promote_editor
void
vue_virtual_window_rep::set_fills (bool _full, bool _promoted) {
  bool before= fills ();
  full= _full; promoted= _promoted;
  if (fills () == before) return;
  if (fills ()) {
    saved_x= x; saved_y= y; saved_w= w; saved_h= h;
    fill ();
  }
  else {
    x= saved_x; y= saved_y; w= saved_w; h= saved_h;
    clamp ();
  }
  restack ();
}

// presentation and full screen modes: the window takes the whole host (the
// host itself goes full screen only when it is asked to, as a window)
void
vue_virtual_window_rep::set_full_screen (bool flag) {
  set_fills (flag, promoted);
  if (flag && shown) focus_virtual (this);
}

void
vue_virtual_window_rep::set_visibility (bool flag) {
  visible_requested= flag;
  if (!flag) {
    shown= false;
    if (focused_virtual == this) focus_virtual (NULL);
    if (pointer_capture == this) pointer_capture= NULL;
    if (drag_win == this) drag_win= NULL;
    if (resize_win == this) resize_win= NULL;
  }
  else if (ready_to_show && !shown) show ();
  // otherwise it is shown by process_layout once it fits its contents
}

void
vue_virtual_window_rep::set_size (SI sw, SI sh) {
  float nw= max (1.0f, (float) sw / PIXEL);
  float nh= max (1.0f, (float) sh / PIXEL);
  if (Min_w > 0) nw= max (nw, (float) Min_w / PIXEL);
  if (Min_h > 0) nh= max (nh, (float) Min_h / PIXEL);
  if (Max_w > 0) nw= min (nw, (float) Max_w / PIXEL);
  if (Max_h > 0) nh= min (nh, (float) Max_h / PIXEL);
  // a window which fills the host gets that size when it floats again
  if (fills ()) { saved_w= nw; saved_h= nh; return; }
  w= nw; h= nh;
  if (shown) clamp ();
}

void
vue_virtual_window_rep::set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h) {
  Min_w= min_w; Min_h= min_h; Max_w= max_w; Max_h= max_h;
}

void
vue_virtual_window_rep::get_size (SI& sw, SI& sh) {
  sw= (SI) (w * PIXEL);
  sh= (SI) (h * PIXEL);
}

void
vue_virtual_window_rep::get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h) {
  min_w= Min_w; min_h= Min_h; max_w= Max_w; max_h= Max_h;
}

void
vue_virtual_window_rep::set_position (SI sx, SI sy) {
  host_changed (); // or a pending move of the host would move it too
  placed= true;
  if (fills ()) {
    saved_x= (float) sx / PIXEL;
    saved_y= (float) -sy / PIXEL;
    return;
  }
  x= (float) sx / PIXEL;
  y= (float) -sy / PIXEL;
  clamp ();
}

void
vue_virtual_window_rep::get_position (SI& sx, SI& sy) {
  host_changed ();
  sx= (SI) (x * PIXEL);
  sy= (SI) (-y * PIXEL);
}

void
vue_virtual_window_rep::draw_picture (void *data, picture pic) {
  vue_render_ren_data* d= (vue_render_ren_data*)data;
  d->ren->draw_picture (pic, d->r->x1, d->r->y1);
}

void
vue_virtual_window_rep::get_viewport_size (void *data, int& vw, int& vh) {
  vue_render_ren_data* d= (vue_render_ren_data*)data;
  vw= (d->r->x2 - d->r->x1) / d->ren->pixel;
  vh= (d->r->y2 - d->r->y1) / d->ren->pixel;
}

static bool
is_host (vue_window w) {
  return w != NULL && w == (vue_window) the_host;
}

// The geometry of the host last seen, in screen points (unknown while
// host_w < 0). The windows on it are screen positioned, as SDL windows are:
// when the host moves they move with it, and when it shrinks they are
// brought back on it. Checked whenever the geometry of the host is asked
// for (host_geometry) and at each redraw of the host (track_geometry),
// which is how a move of the host is noticed.
static int host_x= 0, host_y= 0, host_w= -1, host_h= -1;

static void
host_changed () {
  if (the_host == NULL) return;
  int x, y, w, h;
  SDL_GetWindowPosition (the_host->sdl_win, &x, &y);
  SDL_GetWindowSize (the_host->sdl_win, &w, &h);
  if (x == host_x && y == host_y && w == host_w && h == host_h) return;
  bool known= (host_w >= 0);
  float dx= (float) (x - host_x), dy= (float) (y - host_y);
  host_x= x; host_y= y; host_w= w; host_h= h; // before clamp, which asks
  if (!known) return;
  for (int i= 0; i < N(virtual_windows); i++) {
    vue_virtual_window_rep* v= virtual_windows[i];
    v->x += dx; v->y += dy;
    v->saved_x += dx; v->saved_y += dy;
    v->clamp ();
  }
}

// The editor which fills the host in place of its window: when the window
// of the host is closed, the windows it holds would be lost with it, so
// the host keeps the SDL window (forget_host) and the editor on top takes
// its place, as its contents. The editors are the windows TeXmacs names
// "TeXmacs", "TeXmacs:2"... (unique_window_name in tm_window.cpp); with
// none, the other windows stay where they are on an empty host.
static void
promote_editor () {
  if (the_host == NULL) return;
  vue_virtual_window_rep* best= NULL;
  for (int i= N(virtual_windows) - 1; i >= 0 && best == NULL; i--) {
    vue_virtual_window_rep* v= virtual_windows[i];
    if (!v->popup && v->visible_requested && starts (v->orig_name, "TeXmacs"))
      best= v;
  }
  if (best == NULL) return;
  best->set_fills (best->full, true);
  best->update_title ();
  if (best->shown) focus_virtual (best);
}

// The host goes away. The virtual windows it holds would go with it: the
// SDL window is handed over to a host with no contents of its own, which
// goes on holding them, and an editor among them is promoted in place of
// the one which was closed. That host goes with the last window it holds
// (see ~vue_virtual_window_rep).
static void
forget_host (vue_window w) {
  if (!is_host (w)) return;
  vue_sdl_base_window_rep* old= the_host;
  the_host= NULL;
  host_is_bare= false;
  host_w= host_h= -1;
  if (pointer_hover == w) pointer_hover= NULL;
  if (!single_window_mode () || is_headless () || nr_hosted_windows () == 0)
    return;
  SDL_Window* sw= old->sdl_win;
  Window_to_window->reset (sw);
  old->sdl_win= NULL; // not destroyed with the old window
  vue_widget empty (tm_new<vue_widget_rep> ("vue_host"));
  if (vue_gpu_windows ())
    the_host= tm_new<vue_sdl_gpu_window_rep> (empty, "", false, sw);
  else the_host= tm_new<vue_sdl_mupdf_window_rep> (empty, "", false, sw);
  host_is_bare= true;
  SDL_SetWindowTitle (sw, "TeXmacs");
  promote_editor ();
}

// Closing the host which outlived its window closes the windows it holds
// (they may refuse, e.g. a document with unsaved changes). Returns whether
// w was such a host.
static bool
close_hosted_windows (vue_window w) {
  if (!host_is_bare || !is_host (w)) return false;
  array<vue_virtual_window_rep*> vl= virtual_windows;
  for (int i= N(vl) - 1; i >= 0; i--)
    if (id_to_window->contains (vl[i]->id) && !vl[i]->popup)
      vl[i]->destroy_event ();
  return true;
}

// The editors are told of a change of focus when the loop may change them,
// before the interpose handler applies their changes (deliver_focus). A
// focus given in between, e.g. to a tab made by a command of the interpose
// handler, left an editor with changes not applied when it was repainted
// ("Invalid situation (514) in edit_interface_rep::handle_repaint"): the
// windows of SDL get their focus as events, at the start of the loop.
static array<int>  focus_ids;
static array<bool> focus_flags;

static void
queue_focus (vue_window w, bool flag) {
  if (w == NULL) return;
  focus_ids << w->id;
  focus_flags << flag;
}

static void
deliver_focus () {
  array<int> ids= focus_ids;
  array<bool> flags= focus_flags;
  focus_ids= array<int> ();
  focus_flags= array<bool> ();
  for (int i= 0; i < N(ids); i++)
    if (id_to_window->contains (ids[i]))
      notify_window_focus ((vue_window) id_to_window[ids[i]], flags[i]);
}

// the keys go to the focused virtual window, or to the host; the widget
// which has the focus in each is told whether its window has it
static void
focus_virtual (vue_virtual_window_rep* v) {
  if (focused_virtual == v) return;
  vue_window old= (focused_virtual != NULL) ? (vue_window) focused_virtual
                                            : (vue_window) the_host;
  focused_virtual= v;
  vue_window cur= (v != NULL) ? (vue_window) v : (vue_window) the_host;
  queue_focus (old, false);
  queue_focus (cur, true);
}

// a text drawn with the fonts of the widgets, vertically centered in the
// band [y1, y2] (renderer coordinates, see vue_render_text_fn)
static void
draw_band_text (renderer ren, string s, int style, color c, SI x, SI y1, SI y2) {
  font fn= get_default_styled_font (style);
  SI th= (fn->y2 - fn->y1) / 3;
  SI yb= y1 + ((y2 - y1) - th) / 2;
  ren->set_pencil (c);
  ren->set_shrinking_factor (3);
  fn->var_draw (ren, s, x*3, yb*3 - fn->y1);
  ren->set_shrinking_factor (1);
}

// The floating elements of the layout of the host with at least this
// depth, its pulldown menus (5), the lists of its choice widgets and its
// balloons (10), are above the virtual windows, as the menus and the
// tooltips of the desktop are above its windows; the scroll bars (1) are
// not. Clay sorts the commands by depth: those are at the end.
static const int16_t host_overlay_z= 5;

static int32_t
host_overlay_start (Clay_RenderCommandArray& a) {
  for (int32_t i= 0; i < a.length; i++)
    if (Clay_RenderCommandArray_Get (&a, i)->zIndex >= host_overlay_z) return i;
  return a.length;
}

// is there such an element of the host at (x, y) (points in the host)?
// Then the pointer is for the host, whatever is below
static bool
host_overlay_at (float x, float y) {
  if (the_host == NULL) return false;
  Clay_RenderCommandArray& a= the_host->render_commands;
  float d= the_host->density, px= x * d, py= y * d;
  for (int32_t i= host_overlay_start (a); i < a.length; i++) {
    Clay_BoundingBox b= Clay_RenderCommandArray_Get (&a, i)->boundingBox;
    if (px >= b.x && px < b.x + b.width && py >= b.y && py < b.y + b.height)
      return true;
  }
  return false;
}

// fill the rectangle x1..x2, y1..y2 (device pixels, y downwards) with its
// corners rounded by r
static void
fill_rounded (renderer ren, int x1, int y1, int x2, int y2, float r) {
  SI px= ren->pixel;
  r= min (r, (float) min (x2 - x1, y2 - y1) / 2.0f);
  if (r < 1.0f) { ren->fill (x1*px, -y2*px, x2*px, -y1*px); return; }
  int n= max (2, (int) (r / 1.5f));  // the segments of a quarter of circle
  float cx[4]= { x2 - r, x1 + r, x1 + r, x2 - r };
  float cy[4]= { y1 + r, y1 + r, y2 - r, y2 - r };
  array<SI> xs, ys;
  for (int c= 0; c < 4; c++)
    for (int k= 0; k <= n; k++) {
      double a= (c + (double) k / n) * M_PI / 2.0;
      xs << (SI) ((cx[c] + r * cos (a)) * px);
      ys << (SI) (-(cy[c] - r * sin (a)) * px);
    }
  ren->polygon (xs, ys);
}

// draw the visible virtual windows over the host, back to front
static void
composite_virtual_windows (vue_window host, renderer ren) {
  if (N(virtual_windows) == 0) return;
  float hx, hy, hw, hh;
  host_geometry (hx, hy, hw, hh);
  float d= host->density;
  SI px= ren->pixel;
  for (int i= 0; i < N(virtual_windows); i++) {
    vue_virtual_window_rep* v= virtual_windows[i];
    if (!v->shown) continue;
    int X= (int) ((v->x - hx) * d), Y= (int) ((v->y - hy) * d);
    int W, H;
    v->layout_size (W, H);
    ren->set_origin (0, 0);
    if (v->decorated ()) {
      int T= (int) (title_bar_h * d), B= max (1, (int) d);
      int F= (int) (frame_w * d + 0.5f);
      // the corners are rounded as those of the menus (vue_widget.cpp: the
      // theme's radius by 1.5, in 2x pixels); the contents stay square,
      // the frame is wide enough to hold the curve
      float R= the_theme.radius * 1.5f / 2.0f * d;
      // a shadow, then a frame around the title bar and the contents, with
      // a line on both of its sides
      for (int k= 3; k >= 1; k--) {
        int s= k * B;
        ren->set_pencil (rgb_color (0, 0, 0, 22));
        fill_rounded (ren, X-F-B-s, Y-T-F-B-s+B, X+W+F+B+s, Y+H+F+B+s+B, R + s);
      }
      ren->set_pencil (theme_color (the_theme.border));
      fill_rounded (ren, X-F-B, Y-T-F-B, X+W+F+B, Y+H+F+B, R);
      ren->set_pencil (theme_color (the_theme.shade[2]));
      fill_rounded (ren, X-F, Y-T-F, X+W+F, Y+H+F, max (0.0f, R - B));
      ren->set_pencil (theme_color (the_theme.border));
      ren->fill ((X-B)*px, -(Y+H+B)*px, (X+W+B)*px, -Y*px);
      color tc= theme_color (the_theme.text);
      draw_band_text (ren, v->mod_name, WIDGET_STYLE_BOLD, tc,
                      (X + (int) (8*d))*px, -Y*px, -(Y-T)*px);
      // the close box: a cross, whatever the fonts have
      int c= (int) (7*d), cx= X + W - T/2, cy= Y - T/2;
      ren->set_pencil (pencil (tc, max (1, (int) d) * px));
      ren->line ((cx-c/2)*px, -(cy-c/2)*px, (cx+c/2)*px, -(cy+c/2)*px);
      ren->line ((cx-c/2)*px, -(cy+c/2)*px, (cx+c/2)*px, -(cy-c/2)*px);
    }
    ren->set_origin (X*px, -Y*px);
    ren->clip (0, -H*px, W*px, 0);
    ren->set_pencil (theme_color (the_theme.background));
    ren->fill (0, -H*px, W*px, 0);
    {
      with_window frame (v);
      render_clay_commands (ren, &v->render_commands);
    }
    ren->unclip ();
    ren->set_origin (0, 0);
  }
}

// the pointer has left a window: no element of it is under the pointer any
// more (as for SDL_EVENT_WINDOW_MOUSE_LEAVE); a popup goes away
static void
pointer_left (vue_window w) {
  if (w == NULL) return;
  vue_virtual_window_rep* v= dynamic_cast<vue_virtual_window_rep*> (w);
  if (v != NULL && v->popup) { v->set_visibility (false); return; }
  with_window frame (w);
  Clay_SetPointerState ((Clay_Vector2) { -1, -1 }, false);
  w->input.mouse_action= "move";
  w->input.mouse_time= texmacs_time ();
}

// A pointer event of the host (x, y: points in the host). Returns the
// window it is for, with x and y made relative to it, or NULL when the
// event was used here (a title bar). kind: 0 motion, 1 press, 2 release,
// 3 an event which only needs the window under the pointer (wheel, drop).
static vue_window
route_pointer (vue_window win, float& x, float& y, int kind) {
  if (!single_window_mode () || win == NULL || win != (vue_window) the_host)
    return win;
  float hx, hy, hw, hh;
  host_geometry (hx, hy, hw, hh);
  float sx= hx + x, sy= hy + y;
  if (resize_win != NULL && kind != 3) {
    if (kind == 0)
      resize_win->resize_from (resize_edges, resize_x0, resize_y0,
                               resize_w0, resize_h0,
                               sx - resize_px, sy - resize_py);
    else if (kind == 2) { resize_win= NULL; set_frame_cursor (0); }
    return NULL;
  }
  if (drag_win != NULL && kind != 3) {
    if (kind == 0) {
      drag_win->x= sx - drag_dx;
      drag_win->y= sy - drag_dy;
      drag_win->placed= true;
      drag_win->clamp ();
    }
    else if (kind == 2) drag_win= NULL;
    return NULL;
  }
  vue_virtual_window_rep* target= NULL;
  bool title= false;
  int  edges= 0; // on the frame of target (then title is true too)
  if (pointer_capture != NULL && kind != 3) target= pointer_capture;
  else if (!host_overlay_at (x, y))
    for (int i= N(virtual_windows) - 1; i >= 0; i--) {
      vue_virtual_window_rep* v= virtual_windows[i];
      if (!v->shown) continue;
      if (v->contains (sx, sy)) { target= v; break; }
      if (v->in_title_bar (sx, sy)) { target= v; title= true; break; }
      if ((edges= v->frame_edges (sx, sy)) != 0) { target= v; title= true; break; }
    }
  if (kind == 1) {
    // a press outside the popups dismisses them, and reaches its target
    for (int i= N(virtual_windows) - 1; i >= 0; i--) {
      vue_virtual_window_rep* v= virtual_windows[i];
      if (v->shown && v->popup && v != target) v->set_visibility (false);
    }
    if (target != NULL) {
      target->raise ();
      if (target->decorated ()) focus_virtual (target);
    }
    else focus_virtual (NULL);
    if (edges != 0) {
      resize_win= target;
      resize_edges= edges;
      resize_px= sx; resize_py= sy;
      resize_x0= target->x; resize_y0= target->y;
      resize_w0= target->w; resize_h0= target->h;
      return NULL;
    }
    if (title) {
      if (sx >= target->x + target->w - title_bar_h) target->destroy_event ();
      else {
        drag_win= target;
        drag_dx= sx - target->x;
        drag_dy= sy - target->y;
      }
      return NULL;
    }
    pointer_capture= target;
  }
  else if (kind == 2) pointer_capture= NULL;
  if (kind != 3) {
    vue_window now= (target != NULL && !title) ? (vue_window) target : win;
    if (now != pointer_hover) {
      vue_window old= pointer_hover;
      pointer_hover= now;
      if (old != NULL) pointer_left (old);
    }
  }
  if (kind == 0) set_frame_cursor (edges);
  if (title) return NULL;
  if (target == NULL) return win;
  x= sx - target->x;
  y= sy - target->y;
  return target;
}

// the window which gets the keys of the host
static vue_window
route_keys (vue_window win) {
  if (single_window_mode () && win != NULL && win == (vue_window) the_host &&
      focused_virtual != NULL && focused_virtual->shown)
    return focused_virtual;
  return win;
}

// the windows are drawn by the GPU, and the editors keep their backing
// stores as textures
bool
vue_gpu_windows () {
  // NOTE: OpenGL is probed once, before the first window is made
  vue_gpu_prepare ();
  return vue_gpu_enabled ();
}

//******************************************************************************
// entrypoints for top-level windows

vue_window
plain_window (vue_widget wwid, string name, bool popup, bool document) {
  // headless: the windows are virtual as well, with no host to be drawn in
  if (is_headless () || (single_window_mode () && the_host != NULL))
    return tm_new<vue_virtual_window_rep> (wwid, name, popup);
  if (vue_gpu_windows ()) {
    vue_sdl_gpu_window_rep* g= tm_new<vue_sdl_gpu_window_rep> (wwid, name, popup);
    g->document= document && !popup;
    if (single_window_mode () && the_host == NULL && !popup) the_host= g;
    return g;
  }
  vue_sdl_mupdf_window_rep* w= tm_new<vue_sdl_mupdf_window_rep> (wwid, name, popup);
  w->document= document && !popup;
  if (single_window_mode () && the_host == NULL && !popup) the_host= w;
  return w;
}

//******************************************************************************
// vue_gui

/******************************************************************************
* Main routines
******************************************************************************/

bool char_clip= true;

void initialize_keyboard ();
extern Uint32 vue_dialog_event; // the results of the file dialogs (below)

void gui_open (int& argc, char** argv) {
  // start the gui
  
  // headless (-headless): no display is opened at all, which is what makes
  // the browser build testable under node (see docs/wasm/README.md)
  if (is_headless ()) {
    initialize_colors ();
    initialize_keyboard ();
    return;
  }
  // trackpads: macOS itself generates the momentum events of a gesture
  // (SDL drops them by default), see the kinetic scrolling notes below
  SDL_SetHint (SDL_HINT_MAC_SCROLL_MOMENTUM, "1");
  SDL_SetMainReady (); // TeXmacs has its own main (no SDL_main.h)
  if (!SDL_Init (SDL_INIT_VIDEO)) { // no audio backend is needed
    SDL_Log ("Unable to initialize SDL: %s", SDL_GetError ());
    exit (-1);
  }

#ifdef VUE_SDL_RENDERER
  if (!TTF_Init()) {
    exit (-1);
  }
#endif

  SDL_SetHint (SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, "1");
  vue_dialog_event= SDL_RegisterEvents (1); // results of the file dialogs
  
  // The layout works in device pixels (SDL_GetWindowSizeInPixels) while
  // the pointer comes in points: the factor between them is the pixel
  // density of the display. It was hardcoded to 2, so on a display without
  // HiDPI every pointer position was doubled and nothing could be hit.
  // The density is a property of each window (update_density, from the
  // display it is on), made current while that window is laid out or
  // drawn (with_window, in vue_gui.hpp); the factor set here, from the
  // primary display, is only the one in force outside of any window.
  {
    // the pixel density of the desktop mode, not its content scale, which
    // macOS reports as 1 while drawing at 2 pixels per point
    float density= 0.0f;
    const SDL_DisplayMode* mode= SDL_GetDesktopDisplayMode (SDL_GetPrimaryDisplay ());
    if (mode != NULL) density= mode->pixel_density;
    string forced= get_env ("TEXMACS_VUE_DENSITY"); // see update_density
    if (N(forced) > 0 && is_double (forced)) density= (float) as_double (forced);
    int factor= (density >= 1.5f) ? 2 : 1; // the renderer wants an integer
    if (density <= 0.0f) factor= 2; // unknown: the previous default
    set_retina_factor (factor);
    if (DEBUG_VUE || factor != 2)
      SDL_Log ("display pixel density %.2f: drawing at %dx", density, factor);
  }
  initialize_colors ();
  // the interface theme follows the "gui theme" preference, whose
  // "default" means the appearance of the system
  set_vue_theme (get_preference ("gui theme", "default"));
  initialize_keyboard ();
}

void gui_close () {
  // cleanly close the gui
  if (!is_headless ()) SDL_Quit();
}

void gui_root_extents (SI& width, SI& height)
{
  // in single-window mode the windows live on the host
  if (single_window_mode () && the_host != NULL) {
    int w, h;
    SDL_GetWindowSize (the_host->sdl_win, &w, &h);
    width= w * PIXEL;
    height= h * PIXEL;
    return;
  }
  // get the screen size
  SDL_Rect r;
  if (SDL_GetDisplayBounds (SDL_GetPrimaryDisplay (), &r)) {
    width= r.w * PIXEL;
    height= r.h * PIXEL;
    //cout << "SCREEN:" << screen_width << "," << screen_height << LF;
  } else {
    // headless or SDL trouble: pretend a common screen instead of leaving
    // the sizes undefined
    static bool reported= false;
    if (!reported) SDL_Log ("SDL_GetDisplayBounds failed: %s", SDL_GetError ());
    reported= true;
    width= 1440 * PIXEL;
    height= 900 * PIXEL;
  }
}

void gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  gui_root_extents (width, height);
}

void gui_refresh () {
  // update and redraw all windows (e.g. on a change of output language);
  // the theme is re-read here too, so that a preference which is applied
  // through this path takes effect without a restart
  set_vue_theme (get_preference ("gui theme", "default"));
  gui_needs_relayout= true;
  vue_simple_widget_rep::invalidate_all_editors ();
}

string gui_version () {
  // retrieve the type of GUI that is being used
  return "vue";
}

/******************************************************************************
* Hack for getting the remote time
******************************************************************************/

static bool   time_initialized= false;
static time_t time_difference= 0;

static void
synchronize_time (Uint32 t) {
  if (time_initialized && time_difference == 0) return;
  time_t d= texmacs_time () - ((time_t) t);
  if (time_initialized) {
    if (d < time_difference)
      time_difference= d;
  }
  else {
    time_initialized= true;
    time_difference= d;
  }
  if (-1000 <= time_difference && time_difference <= 1000)
    time_difference= 0;
}

static time_t
remote_time (Uint32 t) {
  return ((time_t) t) + time_difference;
}

/******************************************************************************
* Event loop
******************************************************************************/

static void (*the_interpose_handler) (void)= NULL;

///////// Gui state

static int  kbd_count= 0;
static bool request_partial_redraw= false;
static bool interrupted= false;
static time_t interrupt_time=0;
// the text which the last key delivered as a key types as well (a digit of
// the keypad, the space of shift+space): SDL sends it in a text event,
// which must not type it a second time (see SDL_EVENT_TEXT_INPUT)
static string   kbd_echo;
static uint64_t kbd_echo_stamp= 0;

// F1 toggles the Clay debug view of a window only in debug mode (-debug-qt)
// or with TEXMACS_VUE_CLAY_DEBUG set; otherwise it is a key of TeXmacs
static bool
clay_debug_key () {
  static int env= -1;
  if (env < 0) env= (N(get_env ("TEXMACS_VUE_CLAY_DEBUG")) > 0) ? 1 : 0;
  return env == 1 || DEBUG_VUE;
}

hashmap<int,string> lower_key;
hashmap<int,string> upper_key;

/////////

void gui_interpose (void (*f) (void)) {
  // specify an interpose routine for the main loop
  the_interpose_handler= f;
}

int number_of_servers (); // in texmacs_server.hpp

void sdl_log_event (const SDL_Event *event);
static string lookup_key (SDL_Scancode scancode, SDL_Keymod mod,
                          bool* produces_text= NULL, string* echo= NULL);
static string cork_key (string r);
static string print_modifiers (SDL_Keymod mod);
static string print_key_info ( SDL_KeyboardEvent *key );

void process_event (SDL_Event *event);
void close_help_balloon ();
void dismiss_wait_indicator ();

// The payloads of the drops, read back by call_drop_event (edit_mouse.cpp)
// through the ticket carried by the "drop" mouse action.
hashmap<int, tree> payloads;
static int  drop_serial= 0;
static tree drop_doc (CONCAT);

// the width and height of a dropped image as a pretty TeXmacs length
// (the policy of qt_pretty_image_size: a wide image fills the line)
static void
vue_pretty_image_size (int ww, int hh, string& w, string& h) {
  SI pt= get_current_editor () -> as_length ("1pt");
  SI par= get_current_editor () -> as_length ("1par");
  if (ww <= 0 || hh <= 0 || ww * pt > par) { w= "1par"; h= ""; }
  else { w= as_string (ww) * "pt"; h= as_string (hh) * "pt"; }
}

static void
vue_pretty_image_size (url image, string& w, string& h) {
  w= ""; h= "";
  string ext= locase_all (suffix (image));
  if (ext == "pdf" || ext == "ps" || ext == "eps") return; // sized by the box
  picture pic= load_picture (image, -1, -1, tree (""), PIXEL);
  if (is_nil (pic)) return;
  vue_pretty_image_size (pic->get_width (), pic->get_height (), w, h);
}
struct vue_dialog_result;
static void vue_dialog_finish (vue_dialog_result* res, char* file, bool chosen);
extern Uint32 vue_dialog_event;
void process_messages ();
void process_layout ();
void process_redraw ();

bool gui_wait=  false;

// ms: the editors are repainted at least this often while the events keep
// coming (see the main loop)
static const time_t vue_repaint_dt= 16;

/******************************************************************************
* Scrolling with the wheel
*
* A trackpad drags the view: its events carry the displacement of the
* fingers and are applied at once, so the page follows them exactly. A
* mouse wheel does not drag anything, it asks for a fixed distance: that
* distance is not jumped but travelled over the next few frames (decaying
* with the time constant wheel_smooth_tau), which is what the native
* applications do and what makes a notch read as a movement rather than as
* a cut. Several notches add up, so a wheel which is spun scrolls
* continuously and comes to rest shortly after the last notch.
*
* Telling the two apart: SDL does not report which device sent an event
* (hasPreciseScrollingDeltas is lost on the way), only the deltas, in
* "lines". A trackpad gives a tenth of the displacement of the fingers in
* points, hence wheel_precise_step, and its deltas are fractional; a notch
* is one whole unit. Whole deltas are therefore ambiguous, and the first
* event of a stream may be taken for a notch when it opens a fast swipe:
* the next event settles it, by its fraction or by following within
* wheel_burst_dt, and since a notch is travelled and not jumped the excess
* can still be taken back (wheel_over_x/y) before it has all been applied.
*
* A trackpad has no phase either, so we cannot tell fingers which pause
* from fingers which are lifted. On macOS the system computes the momentum
* itself and, with SDL_HINT_MAC_SCROLL_MOMENTUM, sends it as a stream of
* wheel events after the fingers are lifted: the view follows the fingers
* while they are down and the glide of the system after. Elsewhere the
* speed of the fingers is estimated from the events and, when they stop
* above wheel_launch_speed, the view goes on with that velocity, decaying
* exponentially with the time constant wheel_tau.
******************************************************************************/

static const double wheel_tau= 350.0;          // ms: the glide of a trackpad
static const double wheel_smooth_tau= 45.0;    // ms: the travel of a notch
static const double wheel_launch_speed= 1.0;   // device pixels per ms
static const double wheel_stop_speed= 0.02;    // device pixels per ms: a third
                                               // of a pixel per frame
static const time_t wheel_stream_dt= 30;       // ms: the events have stopped
static const time_t wheel_slow_dt= 200;        // ms: the wheel is turned slowly
static const time_t wheel_burst_dt= 16;        // ms: too soon for a second notch
// SDL reports the deltas in "lines": a trackpad (precise deltas) gives a
// tenth of the finger's displacement in points, so 10 points per unit make
// the page follow the finger exactly, as a dragged scroll bar follows the
// pointer; a notch of a mouse wheel is one unit and scrolls about six lines
static const double wheel_precise_step= 10.0;  // points per unit
static const double wheel_notch_step= 80.0;    // points per notch
#ifdef OS_MACOS
static const bool wheel_system_momentum= true; // the system glides for us
#else
static const bool wheel_system_momentum= false;
#endif

// deliver a wheel delta (device pixels) to the window: to the widgets
// (mouse_action) and to the Clay scroll container under the pointer
static void
push_wheel (vue_window win, double dx, double dy) {
  vue_input_state& in= win->input;
  if (in.mouse_action == "wheel" && N(in.mouse_data) == 2) {
    in.mouse_data[0] += dx; // several deltas in the same frame add up
    in.mouse_data[1] += dy;
  }
  else {
    in.mouse_action= "wheel";
    in.mouse_data= array<double> (dx, dy);
  }
  with_window frame (win);
  Clay_SetPointerState ((Clay_Vector2) { (float) in.mouse_x, (float) in.mouse_y }, false);
  // a bar which only scrolls sideways takes the wheel sideways
  double cx= dx, cy= dy;
  vue_wheel_axes (cx, cy);
  // Clay scrolls its containers by ten pixels per unit of delta. The deltas
  // are applied once, before the next layout (clay_wheel_flush): each call
  // of Clay_UpdateScrollContainers forgets the containers not laid out
  // since the previous one, so that a second call in a frame (a trackpad
  // sends several events per frame) reset their scrolling to the start
  in.clay_wheel_x += cx / 10;
  in.clay_wheel_y += cy / 10;
}

// the wheel of the frame to the Clay scroll containers of the window, as
// Clay wants it, once between two layouts (win is the current window).
// After the layout which gave the wheel to the widgets, and only if none
// of them used it: an editor in a dialog scrolls, not the dialog around
// it (Clay itself gives the wheel to the innermost of its containers)
static void
clay_wheel_flush (vue_window_rep* win) {
  vue_input_state& in= win->input;
  bool taken= in.wheel_taken;
  in.wheel_taken= false;
  if (in.clay_wheel_x == 0 && in.clay_wheel_y == 0) return;
  if (!taken) {
    Clay_SetPointerState ((Clay_Vector2) { (float) in.mouse_x, (float) in.mouse_y }, false);
    Clay_UpdateScrollContainers (true, (Clay_Vector2) { (float) in.clay_wheel_x,
                                                        (float) in.clay_wheel_y }, 0.01f);
    gui_needs_relayout= true; // shown at once, by another layout
  }
  in.clay_wheel_x= in.clay_wheel_y= 0;
}

// a wheel event: scroll and update the estimated speed of the wheel
// (x, y are the deltas as reported by SDL, in wheel units; stamp is the
// timestamp SDL gave the event, in ns: the events queued during a frame
// are all handled at the end of it, so the clock would report them as
// simultaneous and the intervals below would all be zero)
static void
wheel_event (vue_window win, double x, double y, time_t now, uint64_t stamp) {
  vue_input_state& in= win->input;
  in.wheel_vx= in.wheel_vy= 0; // the user took over from a glide
  time_t dt= (in.wheel_stamp == 0 || stamp <= in.wheel_stamp) ? wheel_slow_dt
             : (time_t) ((stamp - in.wheel_stamp) / 1000000);
  in.wheel_stamp= stamp;
  bool fresh= (dt >= wheel_slow_dt); // a new stream of events
  if (fresh) {
    in.wheel_precise= false;
    in.wheel_over_x= in.wheel_over_y= 0;
  }
  // fractional deltas are a trackpad; so is a second event which follows
  // the opening one too soon for a wheel to have turned twice (see above)
  bool precise= (x != floor (x) || y != floor (y)) ||
                (in.wheel_ambiguous && !fresh && dt <= wheel_burst_dt);
  in.wheel_ambiguous= fresh && !precise;
  if (precise && !in.wheel_precise) {
    in.wheel_precise= true;
    // the stream opened with whole deltas and was taken for a notch: take
    // back the excess, of which little has been applied so far
    in.wheel_pend_x -= in.wheel_over_x;
    in.wheel_pend_y -= in.wheel_over_y;
    in.wheel_over_x= in.wheel_over_y= 0;
  }
  double step= win->density * (in.wheel_precise ? wheel_precise_step : wheel_notch_step);
  double dx= x * step, dy= y * step; // device pixels
  dt= max ((time_t) 8, min (dt, wheel_slow_dt));
  in.wheel_est_x= 0.5 * (in.wheel_est_x + dx / dt);
  in.wheel_est_y= 0.5 * (in.wheel_est_y + dy / dt);
  in.wheel_event_time= now;
  if (in.wheel_precise) push_wheel (win, dx, dy); // follow the fingers
  else {
    // a notch is travelled over the next frames, not jumped (see above)
    if (in.wheel_pend_x == 0 && in.wheel_pend_y == 0) in.wheel_smooth_time= now;
    in.wheel_pend_x += dx; in.wheel_pend_y += dy;
    double over= win->density * (wheel_notch_step - wheel_precise_step);
    in.wheel_over_x= in.wheel_ambiguous ? x * over : 0.0;
    in.wheel_over_y= in.wheel_ambiguous ? y * over : 0.0;
  }
}

// advance the scrolling of all windows; returns true if the loop must come
// back soon (a notch is travelling, a view glides, or a glide may start)
static bool
wheel_step () {
  bool busy= false;
  time_t now= texmacs_time ();
  iterator<int> it= iterate (id_to_window);
  while (it->busy ()) {
    vue_window win= (vue_window) id_to_window[it->next ()];
    if (win == NULL) continue;
    vue_input_state& in= win->input;
    // a stream which opened with whole deltas and got no second event in
    // time comes from a wheel after all: the notch may travel in full
    if (in.wheel_ambiguous && now - in.wheel_event_time > wheel_burst_dt) {
      in.wheel_ambiguous= false;
      in.wheel_over_x= in.wheel_over_y= 0;
    }
    // the distance asked by the wheel notches, travelled over a few frames
    if (in.wheel_pend_x != 0 || in.wheel_pend_y != 0) {
      busy= true;
      time_t dt= now - in.wheel_smooth_time;
      if (dt > 0) {
        double part= 1.0 - exp (- (double) dt / wheel_smooth_tau);
        double dx= in.wheel_pend_x * part, dy= in.wheel_pend_y * part;
        // while the notch may still turn out to be a swipe, travel no
        // further than a swipe would have: the excess (wheel_over_x/y) is
        // then taken back before any of it has been applied, and the view
        // does not have to spring back
        if (in.wheel_ambiguous) {
          if (fabs (in.wheel_pend_x - dx) < fabs (in.wheel_over_x))
            dx= in.wheel_pend_x - in.wheel_over_x;
          if (fabs (in.wheel_pend_y - dy) < fabs (in.wheel_over_y))
            dy= in.wheel_pend_y - in.wheel_over_y;
        }
        in.wheel_pend_x -= dx; in.wheel_pend_y -= dy;
        // the last half pixel is not worth another frame
        if (!in.wheel_ambiguous) {
          if (fabs (in.wheel_pend_x) < 0.5) { dx += in.wheel_pend_x; in.wheel_pend_x= 0; }
          if (fabs (in.wheel_pend_y) < 0.5) { dy += in.wheel_pend_y; in.wheel_pend_y= 0; }
        }
        in.wheel_smooth_time= now;
        if (dx != 0 || dy != 0) push_wheel (win, dx, dy);
      }
    }
    if (in.wheel_vx == 0 && in.wheel_vy == 0) {
      // no glide: did the events just stop with a launched trackpad?
      if (in.wheel_est_x == 0 && in.wheel_est_y == 0) continue;
      busy= true;
      if (now - in.wheel_event_time < wheel_stream_dt) continue;
      // a notch travels by itself and the system glides for a trackpad
      // where it does the momentum: neither wants a glide of ours
      if (hypot (in.wheel_est_x, in.wheel_est_y) >= wheel_launch_speed &&
          in.wheel_precise && !wheel_system_momentum) {
        in.wheel_vx= in.wheel_est_x;
        in.wheel_vy= in.wheel_est_y;
        in.wheel_time= now;
      }
      in.wheel_est_x= in.wheel_est_y= 0;
      continue;
    }
    busy= true;
    time_t dt= now - in.wheel_time;
    if (dt <= 0) continue;
    double decay= exp (- (double) dt / wheel_tau);
    double dx= in.wheel_vx * wheel_tau * (1.0 - decay);
    double dy= in.wheel_vy * wheel_tau * (1.0 - decay);
    in.wheel_vx *= decay;
    in.wheel_vy *= decay;
    in.wheel_time= now;
    // the glide ends once it is too slow to be seen: an exponential decay
    // never reaches zero, and it kept the loop drawing frames of fractions
    // of a pixel for seconds
    if (hypot (in.wheel_vx, in.wheel_vy) < wheel_stop_speed)
      in.wheel_vx= in.wheel_vy= 0;
    if (dx != 0 || dy != 0) push_wheel (win, dx, dy);
  }
  return busy;
}
// an animation of ours runs until then (the smooth zoom of the editors,
// vue_widget.cpp): frames until then, as for the transitions of Clay
time_t vue_animation_until= 0;

// does a window have a Clay transition in progress? (see process_layout)
static bool
transitions_running () {
  if (texmacs_time () < vue_animation_until) return true;
  iterator<int> it= iterate (id_to_window);
  while (it->busy ()) {
    vue_window win= (vue_window) id_to_window[it->next ()];
    if (win != NULL && win->transitions_active) return true;
  }
  return false;
}

bool gui_needs_update= true;

bool event_filter (void *userdata, SDL_Event *event);

/******************************************************************************
* Frame profiler
*
* TEXMACS_VUE_PROFILE=<n> prints, every n frames of the event loop (300 by
* default), where the time of a frame went: one line per phase with the
* share of the total, the mean over the window, the worst frame, and how
* many frames the phase actually did something in. It measures wall time
* with SDL_GetTicksNS, so the "wait" phase is the time the loop spent
* asleep, which is what is left when nothing needs doing.
******************************************************************************/

enum { VP_WAIT= 0, VP_LAYOUT, VP_COMMANDS, VP_INTERPOSE, VP_REPAINT,
       VP_REDRAW, VP_CLAY, VP_FILL, VP_UPLOAD, VP_FRAME, VP_N };

static const char* vue_phase_name[VP_N]= {
  "wait", "layout", "commands", "interpose", "repaint",
  "redraw", "  of it: clay replay", "    of it: background fill",
  "  of it: surface upload", "FRAME" };

extern bool vue_profile_on;
extern uint64_t vue_clay_ns, vue_upload_ns, vue_fill_ns;
extern int vue_commands;
extern uint64_t vue_cmd_ns[12];
extern int vue_cmd_n[12];
extern uint64_t vue_text_ns, vue_editor_ns, vue_other_ns;
extern int vue_text_n, vue_editor_n, vue_other_n;

static uint64_t vue_phase_total[VP_N];
static uint64_t vue_phase_worst[VP_N];
static int      vue_phase_count[VP_N];
static int      vue_frames= 0;
static long     vue_total_commands= 0;
static int      vue_profile_every= -1; // -1: the variable has not been read

static bool
vue_profiling () {
  if (vue_profile_every < 0) {
    string s= get_env ("TEXMACS_VUE_PROFILE");
    if (N(s) == 0) vue_profile_every= 0;
    else if (is_int (s) && as_int (s) > 0) vue_profile_every= as_int (s);
    else vue_profile_every= 300;
    vue_profile_on= (vue_profile_every > 0);
  }
  return vue_profile_on;
}

static inline uint64_t
vue_now () { return vue_profiling () ? SDL_GetTicksNS () : 0; }

static void
vue_profile_add (int phase, uint64_t ns) {
  if (!vue_profiling ()) return;
  vue_phase_total[phase] += ns;
  if (ns > vue_phase_worst[phase]) vue_phase_worst[phase]= ns;
  if (ns > 0) vue_phase_count[phase]++;
}

static void
vue_profile_frame () {
  if (!vue_profiling ()) return;
  if (++vue_frames < vue_profile_every) return;
  double total= (double) vue_phase_total[VP_FRAME];
  cout << "\n--- Vue: " << vue_frames << " frames, "
       << (total / 1e6) << " ms, "
       << (total > 0 ? (vue_frames * 1e9 / total) : 0.0) << " frames/s, "
       << (vue_total_commands / vue_frames) << " render commands/frame\n";
  // the windows: their sizes and their state (hidden, minimized, occluded:
  // a window laid out at 0 x 0 has no render commands)
  iterator<SDL_Window*> wit= iterate (Window_to_window);
  while (wit->busy ()) {
    SDL_Window* sw= wit->next ();
    int ww= 0, wh= 0, pw= 0, ph= 0;
    SDL_GetWindowSize (sw, &ww, &wh);
    SDL_GetWindowSizeInPixels (sw, &pw, &ph);
    char flags[32];
    snprintf (flags, sizeof (flags), "%llx",
              (unsigned long long) SDL_GetWindowFlags (sw));
    cout << "  window " << (int) SDL_GetWindowID (sw) << ": " << ww << " x "
         << wh << " points, " << pw << " x " << ph << " pixels, flags 0x"
         << flags << "\n";
  }
  for (int i= 0; i < VP_N; i++) {
    double t= (double) vue_phase_total[i];
    cout << "  " << vue_phase_name[i] << "\t"
         << (total > 0 ? (100.0 * t / total) : 0.0) << "%\t mean "
         << (t / 1e6 / vue_frames) << " ms\t worst "
         << (vue_phase_worst[i] / 1e6) << " ms\t in "
         << vue_phase_count[i] << " frames\n";
  }
  static const char* ty[12]= { "?0", "rectangle", "border", "text", "image",
    "scissor start", "scissor end", "overlay", "overlay end", "custom",
    "?10", "?11" };
  cout << "  replay by command type:\n";
  for (int i= 0; i < 12; i++)
    if (vue_cmd_n[i] > 0)
      cout << "    " << ty[i] << "\t" << (vue_cmd_ns[i] / 1e6 / vue_frames)
           << " ms/frame\t" << (vue_cmd_n[i] / vue_frames) << " per frame\n";
  cout << "    custom: text\t" << (vue_text_ns / 1e6 / vue_frames)
       << " ms/frame\t" << (vue_text_n / vue_frames) << " per frame\n"
       << "    custom: editors\t" << (vue_editor_ns / 1e6 / vue_frames)
       << " ms/frame\t" << (vue_editor_n / vue_frames) << " per frame\n"
       << "    custom: other\t" << (vue_other_ns / 1e6 / vue_frames)
       << " ms/frame\t" << (vue_other_n / vue_frames) << " per frame\n";
  vue_text_ns= vue_editor_ns= vue_other_ns= 0;
  vue_text_n= vue_editor_n= vue_other_n= 0;
  for (int i= 0; i < 12; i++) { vue_cmd_ns[i]= 0; vue_cmd_n[i]= 0; }
  cout << LF;
  for (int i= 0; i < VP_N; i++) {
    vue_phase_total[i]= 0; vue_phase_worst[i]= 0; vue_phase_count[i]= 0;
  }
  vue_frames= 0;
  vue_total_commands= 0;
}

// The resize watch (event_filter) runs a whole frame, and SDL calls it
// from inside whatever SDL call pumps the events -- SDL_ShowWindow among
// them, which process_layout calls in the middle of a frame. It may do so
// only while the loop is waiting for events, which is also where a live
// resize (the window dragged) delivers them; anywhere else the event is
// left to the next frame, whose layout reads the size of the window anyway.
// Running a frame from inside SDL_ShowWindow repainted an editor whose view
// was not yet attached to its window: with a file on the command line,
// TeXmacs stopped at once ("no window attached to view").
static bool watch_may_run= false;

// The focus a window was given when first laid out (default_focus), set
// just before the interpose handler, so that it applies the change of the
// editor before anything is repainted (see vue_texmacs_widget_rep). A window
// which has closed in between is no longer in the table.
static void
apply_default_focus () {
  iterator<int> it= iterate (id_to_window);
  while (it->busy ()) {
    vue_window win= (vue_window) id_to_window [it->next ()];
    if (win == NULL || is_nil (win->default_focus)) continue;
    vue_widget w= win->default_focus;
    win->default_focus= vue_widget ();
    if (is_nil (win->kbd_focus)) set_kbd_focus (win, w);
  }
}

static bool
loop_poll (SDL_Event* event) {
  watch_may_run= true;
  bool r= SDL_PollEvent (event);
  watch_may_run= false;
  return r;
}

static void
loop_wait (int ms) {
#ifdef __EMSCRIPTEN__
  (void) ms; // the browser paces the frames (see gui_start_loop)
#else
  watch_may_run= true;
  SDL_WaitEventTimeout (NULL, ms);
  watch_may_run= false;
#endif
}

// headless: no window, no event; the interpose handler runs the commands of
// the command line and the delayed ones, until one of them quits (quitting
// ends the process, see quit_texmacs_internal)
static void
headless_loop () {
  while (true) {
    if (!is_nil (cmd_list)) {
      list<command> l= reverse (cmd_list);
      cmd_list= list<command> ();
      for (; !is_nil (l); l= l->next) l->item->apply ();
    }
    if (the_interpose_handler != NULL) the_interpose_handler ();
    usleep (10000);
  }
}

// The characters of a text committed at once (by an input method) are
// delivered as one key each, as the Qt port does (QTMWidget.cpp): the
// window takes one key per frame, so the rest waits here and goes before
// any later event
static array<string> pending_keys;
static int pending_keys_win= -1;

static bool
deliver_pending_key () {
  if (N(pending_keys) == 0) return false;
  string k= pending_keys[0];
  array<string> rest;
  for (int i= 1; i < N(pending_keys); i++) rest << pending_keys[i];
  pending_keys= rest;
  if (!id_to_window->contains (pending_keys_win)) {
    pending_keys= array<string> (); // the window closed meanwhile
    return false;
  }
  vue_window win= (vue_window) id_to_window [pending_keys_win];
  if (win == NULL) return false;
  win->input.key_event= k;
  win->input.key_time= texmacs_time ();
  win->input.last_key= k;
  win->input.key_stamp= 0;
  return true;
}

// Is a full frame needed although the iteration was woken without any
// event (the pause of the loop is over)? Only when something may have
// changed: a request of the editors, a widget replaced, an editor to
// repaint, a window waiting to be shown, a snapshot of the test driver.
// The loop wakes up every 40 ms while a socket is open, and laid out and
// redrew every window each time.
extern list<vue_simple_widget_rep*> paint_list; // vue_widget.cpp

static bool
frame_wanted () {
  if (gui_needs_update || gui_needs_relayout || request_partial_redraw ||
      !is_nil (cmd_list) || N(pending_keys) > 0 || N(snapshot_name) > 0)
    return true;
  iterator<int> it= iterate (id_to_window);
  while (it->busy ()) {
    vue_window win= (vue_window) id_to_window [it->next ()];
    if (win != NULL && win->visible_requested && !win->shown) return true;
  }
  // (through the widget interface: the backing stores are not ours)
  list<vue_simple_widget_rep*> l= paint_list;
  for (; !is_nil (l); l= l->next)
    if (open_box<bool> (l->item->query (SLOT_INVALID, type_helper<bool>::id)))
      return true;
  return false;
}

// a full frame at least this often, whatever frame_wanted says: a change
// which asks for nothing (a label rewritten by a delayed command) still
// shows up in time
static const time_t vue_idle_frame_dt= 250;

// One iteration of the main loop: the events, the layout, the commands, the
// interpose handler, the repaint of the editors and the redraw of the
// windows. The desktop calls it in a loop, the browser once per frame (it
// may not keep control: see gui_start_loop).
static int loop_delay= 10; // the pause of the loop, which grows when idle

static void
loop_iteration () {
  int& delay= loop_delay;
  time_t t1= 0, t2= 0;
  uint64_t t_frame= vue_now (); // the whole iteration, wait included
  static time_t last_frame= 0; // the last full frame (see frame_wanted)
  bool active= false; // an event, or something moving by itself

  // 1. process events
  script_step (); // may push synthetic events
  SDL_Event event;
  if (deliver_pending_key ()) active= true;
  else if (loop_poll (&event)) {
    active= true;
    bool batchable= (event.type == SDL_EVENT_MOUSE_WHEEL ||
                     event.type == SDL_EVENT_MOUSE_MOTION);
    process_event (&event);
    gui_needs_update= true;
    // A frame costs more than the interval between the events of a
    // trackpad or of a fast pointer: handle the wheel and motion events
    // which are already queued in this frame too (their deltas add up,
    // the last position wins), so that the view keeps up with the
    // fingers. Only when the first event was itself a motion or a wheel:
    // the motion handler overwrites mouse_action, so batching after a
    // press or a release would drop it before any widget sees it (a
    // click on a trackpad almost always comes with a small motion).
    while (batchable &&
           SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                           SDL_EVENT_FIRST, SDL_EVENT_LAST) == 1 &&
           (event.type == SDL_EVENT_MOUSE_WHEEL ||
            event.type == SDL_EVENT_MOUSE_MOTION) &&
           loop_poll (&event)) {
      process_event (&event);
    }
  }
  if (transitions_running ()) {
    // a transition animates: keep the frames coming (paced, woken by events)
    gui_needs_update= true;
    if (!loop_poll (NULL)) loop_wait (8);
  }
  if (wheel_step ()) {
    // keep the frames coming while the view moves by itself (or while a
    // stream of wheel events is being watched for a launch), paced at
    // 5 ms but woken up by any event: a plain sleep here added its
    // length to the latency of every wheel event
    gui_needs_update= true;
    if (!loop_poll (NULL)) loop_wait (5);
  }

  if (gui_needs_update) {
    active= true;
    delay= 10;
    gui_wait= false;
    gui_needs_update= false;
  }
      
  // 2. wait for events on all channels
  // (always without a window: nothing else paces the loop then, which spun
  // at full speed while a server kept it going)
  if (gui_wait || nr_windows == 0) {
    // sleep until an event arrives, or at most 'delay' (the interpose
    // handler and the delayed Scheme commands need periodic calls; the
    // pause grows while nothing happens). A plain SDL_Delay here made the
    // first event after a pause wait for the end of the pause: up to 1 s
    // before a scroll started to move
    // sockets and pipes (plugins, the TeXmacs client/server) are polled
    // by the interpose handler (perform_select): keep the pause short
    // while any is open, they have no event of their own to wake us
    int pause= notifiers_active () ? min (delay, 40) : delay;
    uint64_t t_wait= vue_now ();
    loop_wait (pause);
    vue_profile_add (VP_WAIT, vue_now () - t_wait);
    delay += (delay/5);
    if (delay > 1000) delay= 1000;
  }

  // an iteration woken by the end of its pause, with nothing to show, lays
  // out and draws nothing (see frame_wanted); the interpose handler still
  // runs, and whatever it changes gets a frame at once
  bool quiet= !active && !frame_wanted () &&
              texmacs_time () - last_frame < vue_idle_frame_dt;

  // 3. process layout and handle events
  if (!quiet) {
    t2= texmacs_time ();
    uint64_t t_ns= vue_now ();
    process_layout ();
    vue_profile_add (VP_LAYOUT, vue_now () - t_ns);
    t1= t2; t2= texmacs_time ();
    if (DEBUG_VUE && t2 - t1 >= 30) debug_widgets << "layout took " << t2 - t1 << "ms" << LF;
  }
  
  // 4. exec commands if present
  uint64_t t_cmd= vue_now ();
  if (!is_nil (cmd_list)) {
    list<command> l= reverse(cmd_list);
    cmd_list= list<command>();
    while (!is_nil(l)) {
      if (DEBUG_VUE_WIDGETS) debug_widgets << "run command " << l->item << LF;
      l->item->apply();
      l= l->next;
    }
  }
  vue_profile_add (VP_COMMANDS, vue_now () - t_cmd);
  
  // 5. interpose
  uint64_t t_int= vue_now ();
  t2= texmacs_time ();
  deliver_focus (); // the changes of focus of the virtual windows
  apply_default_focus ();
  vue_simple_widget_rep::notify_resizes ();
  if (the_interpose_handler != NULL) the_interpose_handler ();
  if (nr_windows == 0) { gui_wait= true; return; }
  vue_profile_add (VP_INTERPOSE, vue_now () - t_int);
  t1= t2; t2= texmacs_time ();
  if (DEBUG_VUE && t2 - t1 >= 30) debug_widgets << "interpose took " << t2-t1 << "ms" << LF;

  if (quiet && !frame_wanted ()) { gui_wait= true; return; }
  last_frame= texmacs_time ();

  // the commands and the interpose handler may have replaced widgets
  // (menus, tools, dialogs): the render commands of the last layout would
  // draw freed widgets, so lay the windows out again first (a quiet
  // iteration has not laid them out yet)
  if (gui_needs_relayout || quiet) process_layout ();

  // 6. repaint all the editors
  uint64_t t_rep= vue_now ();
  t2= texmacs_time ();
  // The editors are repainted when no event is waiting, so that a burst
  // of input is handled before the pixels are computed. A trackpad
  // delivers its events faster than a frame is drawn, though, and its
  // stream never runs dry: the repaint then never got its turn for as
  // long as a swipe lasted and the page stood still. It is therefore
  // done anyway once it is older than vue_repaint_dt.
  static time_t last_repaint= 0;
  time_t now_rep= texmacs_time ();
  int n_events= SDL_PollEvent (NULL);
  if (n_events == 0 || request_partial_redraw ||
      now_rep - last_repaint >= vue_repaint_dt) {
    last_repaint= now_rep;
    request_partial_redraw= false;

    interrupted= false;
    interrupt_time= texmacs_time () + (100 / (n_events + 1));

    vue_simple_widget_rep::repaint_all ();
    // note that repaint can be interrupted if events are present
    //FIXME: we should redraw the focused editor first, then the others

    request_partial_redraw= interrupted;
  }
  vue_profile_add (VP_REPAINT, vue_now () - t_rep);
  t1= t2; t2= texmacs_time ();
  if (DEBUG_VUE && t2 - t1 >= 30) debug_widgets << "repaint took " << t2 - t1 << "ms" << LF;

  // 7. redraw the UI
  // the repaint runs the typesetter, which may execute Scheme and replace
  // widgets (a tool rebuilt from a document change, say): lay the windows
  // out again, or the interface would be drawn as it was before. A layout
  // which frees a widget asks for another one, hence the loop, bounded in
  // case one never settles; drawing commands which are one layout old is
  // safe, they hold their widgets (see render_ref).
  for (int pass= 0; gui_needs_relayout && pass < 4; pass++) process_layout ();
  uint64_t t_draw= vue_now ();
  vue_clay_ns= 0; vue_upload_ns= 0; vue_fill_ns= 0; vue_commands= 0;
  process_redraw ();
  vue_profile_add (VP_REDRAW, vue_now () - t_draw);
  vue_profile_add (VP_CLAY, vue_clay_ns);
  vue_profile_add (VP_FILL, vue_fill_ns);
  vue_profile_add (VP_UPLOAD, vue_upload_ns);
  vue_total_commands += vue_commands;
  t1= t2; t2= texmacs_time ();
  if (DEBUG_VUE && t2 - t1 >= 50) debug_widgets << "redraw took " << t2 - t1 << "ms" << LF;
  vue_profile_add (VP_FRAME, vue_now () - t_frame);
  vue_profile_frame ();
  gui_wait= true;
}

void gui_start_loop () {
  // start the main loop
  if (is_headless ()) { headless_loop (); return; }
  request_partial_redraw= true;

  // FIXME: Don't typeset when resizing window

  // SDL_EVENT_QUIT asks TeXmacs to quit (see process_event), so SDL must
  // not send it when the last window is closed: that window may only be
  // hidden, and TeXmacs decides itself what closing it means
  SDL_SetHint (SDL_HINT_QUIT_ON_LAST_WINDOW_CLOSE, "0");
  SDL_AddEventWatch (&event_filter, NULL);
  script_init ();

#ifdef __EMSCRIPTEN__
  // The browser calls an iteration per animation frame and the loop may not
  // wait (loop_wait does nothing). Control does not come back: the stack is
  // unwound, which is why the server of TeXmacs_main is not on it there.
  emscripten_set_main_loop (loop_iteration, 0, true);
#else
  while (nr_windows > 0 || number_of_servers () > 0) loop_iteration ();
#endif
}

void process_layout () {
  // reset memory pools: the render commands of the previous layout are
  // about to be replaced, so what was held for them is released here (a
  // widget which left the widget tree meanwhile dies at this point)
  styled_strings= array<styled_string>();
  release_layout_widgets ();
  gui_needs_relayout= false; // widgets deleted while laying out are not drawn
  
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy()) { // and then the other windows
    vue_window_rep *win= (vue_window_rep*) Window_to_window [it->next()];
    win->process_layout ();
  }
  // the virtual windows (single-window mode); the array may change while
  // they are laid out
  array<vue_virtual_window_rep*> vl= virtual_windows;
  for (int i= 0; i < N(vl); i++)
    if (id_to_window->contains (vl[i]->id)) vl[i]->process_layout ();
}

void process_redraw () {
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy()) { // and then the other windows
    vue_window_rep *win= (vue_window_rep*) Window_to_window [it->next()];
    win->process_redraw ();
  }
}

static vue_window
get_window_from_ID (Uint32 ID) {
  SDL_Window *w= SDL_GetWindowFromID (ID);
  if (w == NULL) return NULL;
  vue_window win= (vue_window) Window_to_window [w];
  return win;
}

/******************************************************************************
* Scripted events (development aid)
*
* When TEXMACS_VUE_SCRIPT names a file, its lines are executed one by one
* (a line is executed only when no SDL event is pending). Coordinates are in
* window points, relative to the content area of the target window:
*
*   # comment
*   wait <ms>                       pause
*   window <substring of title>     select the target window (default: last created)
*   window #<id>                    select the target window by its id
*   move x y                        pointer motion
*   press x y [left|right|middle]   button down
*   release x y [left|right|middle] button up
*   click x y [left|right|middle] [n]  press followed by release; n: the
*                                   count of a double (2) or triple (3) click
*   wheel x y dx dy                 wheel event at (x, y)
*   key [S-][C-][A-][M-]<name>      key press, e.g. Return, Escape, Tab, Down,
*                                   with shift/control/option/command prefixes
*                                   ("_" for a space: Keypad_1)
*   key <key> <text>                the key with the text the system sends too
*   text <string>                   text input, one event per character
*   commit <string>                 text input, one event (an input method)
*   compose <text>                  composition of an input method (empty: end it)
*   focus                           pretend the target window got the keyboard focus
*   drop x y <path>|text:<text>     drag and drop of one item at that position
*   repaint                         invalidate every editor (repaint from scratch)
*   scheme <expression>             run a Scheme command, as -x does (at the
*                                   next turn of the loop): a zoom, a marker
*                                   which (display) prints in the log...
*   snapshot <name>                 save the target window as <TEXMACS_VUE_SNAPSHOT>/<name>.png
*   resize w h                      resize the target window (points)
*   close                           ask to close the target window
******************************************************************************/

static array<string> script_lines;
static int script_pos= 0;
static time_t script_next= 0;

static void
script_init () {
  string file= get_env ("TEXMACS_VUE_SCRIPT");
  if (N(file) == 0) return;
  string s;
  if (load_string (url_system (file), s, false)) {
    cout << "vue script: cannot read " << file << LF;
    return;
  }
  script_lines= tokenize (s, "\n");
  script_active= true;
  cout << "vue script: " << N(script_lines) << " lines" << LF;
}

static bool script_no_target= false; // the last "window" command matched nothing

static vue_window
script_target () {
  if (script_no_target) return NULL; // the commands are skipped until a "window" matches
  if (script_win != NULL && id_to_window->contains (script_win->id))
    return script_win;
  return last_created_window;
}

// the SDL window which receives the events meant for win, and the position
// (x, y) in it: a virtual window gets them through its host, with the
// coordinates shifted, so that the routing of single-window mode is tested
static Uint32
script_window_id (vue_window win, float& x, float& y) {
  if (win->platform_window () != NULL)
    return SDL_GetWindowID ((SDL_Window*) win->platform_window ());
  vue_virtual_window_rep* v= dynamic_cast<vue_virtual_window_rep*> (win);
  if (v == NULL || the_host == NULL) return 0;
  float hx, hy, hw, hh;
  host_geometry (hx, hy, hw, hh);
  x += v->x - hx;
  y += v->y - hy;
  return SDL_GetWindowID (the_host->sdl_win);
}

static Uint32
script_window_id (vue_window win) {
  float x= 0, y= 0;
  return script_window_id (win, x, y);
}

static Uint8
script_button (array<string> a, int i) {
  if (N(a) > i && a[i] == "right") return SDL_BUTTON_RIGHT;
  if (N(a) > i && a[i] == "middle") return SDL_BUTTON_MIDDLE;
  return SDL_BUTTON_LEFT;
}

static void
script_push_button (vue_window win, float x, float y, Uint8 button, bool down,
                    int clicks= 1) {
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= down ? SDL_EVENT_MOUSE_BUTTON_DOWN : SDL_EVENT_MOUSE_BUTTON_UP;
  ev.button.timestamp= SDL_GetTicksNS ();
  ev.button.windowID= script_window_id (win, x, y);
  ev.button.button= button;
  ev.button.down= down;
  ev.button.clicks= (Uint8) clicks;
  ev.button.x= x;
  ev.button.y= y;
  if (down) script_buttons |= SDL_BUTTON_MASK (button);
  else script_buttons &= ~SDL_BUTTON_MASK (button);
  SDL_PushEvent (&ev);
}

static void
script_push_motion (vue_window win, float x, float y) {
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= SDL_EVENT_MOUSE_MOTION;
  ev.motion.timestamp= SDL_GetTicksNS ();
  ev.motion.windowID= script_window_id (win, x, y);
  ev.motion.state= script_buttons;
  ev.motion.x= x;
  ev.motion.y= y;
  SDL_PushEvent (&ev);
}

static void
script_step () {
  if (!script_active) return;
  if (SDL_PollEvent (NULL)) return; // let pending events be processed first
  time_t now= texmacs_time ();
  if (now < script_next) return;
  while (script_pos < N(script_lines)) {
    string line= trim_spaces (script_lines[script_pos++]);
    if (N(line) == 0 || line[0] == '#') continue;
    array<string> a= tokenize (line, " ");
    string cmd= a[0];
    vue_window win= script_target ();
    cout << "vue script: " << line << LF;
    if (cmd == "wait" && N(a) > 1) {
      script_next= now + as_int (a[1]);
      return;
    }
    else if (cmd == "window" && N(a) > 1) {
      string title= line (N(cmd)+1, N(line));
      // no match: the following commands are skipped rather than sent to
      // the previous target (e.g. closing the main window by mistake)
      script_win= NULL;
      iterator<int> it= iterate (id_to_window);
      while (it->busy ()) {
        vue_window w= (vue_window) id_to_window [it->next ()];
        if (title == "#" * as_string (w->id) ||
            occurs (title, w->name) || occurs (title, w->get_name ())) script_win= w;
      }
      script_no_target= (script_win == NULL);
      if (script_win == NULL) cout << "vue script: no window matches " << title << LF;
      else {
        SI wx, wy, ww, wh;
        script_win->get_position (wx, wy);
        script_win->get_size (ww, wh);
        cout << "vue script: window at " << wx/PIXEL << "," << -wy/PIXEL
             << " size " << ww/PIXEL << "x" << wh/PIXEL
             << (script_win->platform_window () == NULL ? " (virtual)" : "") << LF;
      }
      continue;
    }
    if (win == NULL) continue;
    if (cmd == "move" && N(a) > 2)
      script_push_motion (win, as_double (a[1]), as_double (a[2]));
    else if (cmd == "press" && N(a) > 2)
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), true);
    else if (cmd == "release" && N(a) > 2)
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), false);
    else if (cmd == "click" && N(a) > 2) {
      // the count of a double or triple click, as SDL gives it (the
      // presses before it are the script's own clicks)
      int clicks= (N(a) > 4 && is_int (a[4])) ? max (1, as_int (a[4])) : 1;
      script_push_motion (win, as_double (a[1]), as_double (a[2]));
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), true, clicks);
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), false, clicks);
    }
    else if (cmd == "wheel" && N(a) > 4) {
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_MOUSE_WHEEL;
      ev.wheel.timestamp= SDL_GetTicksNS ();
      float wx= as_double (a[1]), wy= as_double (a[2]);
      ev.wheel.windowID= script_window_id (win, wx, wy);
      ev.wheel.mouse_x= wx;
      ev.wheel.mouse_y= wy;
      ev.wheel.x= as_double (a[3]);
      ev.wheel.y= as_double (a[4]);
      SDL_PushEvent (&ev);
    }
    else if (cmd == "key" && N(a) > 1) {
      // "key [S-][C-][A-][M-]<SDL key name>": the prefixes are the shift,
      // control, option and command modifiers
      SDL_Event ev;
      SDL_zero (ev);
      string kn= a[1];
      SDL_Keymod mod= SDL_KMOD_NONE;
      while (N(kn) > 2 && kn[1] == '-') {
        if (kn[0] == 'S') mod |= SDL_KMOD_LSHIFT;
        else if (kn[0] == 'C') mod |= SDL_KMOD_LCTRL;
        else if (kn[0] == 'A') mod |= SDL_KMOD_LALT;
        else if (kn[0] == 'M') mod |= SDL_KMOD_LGUI;
        else break;
        kn= kn (2, N(kn));
      }
      // the names with spaces ("Keypad 1") are written with underscores
      c_string name (replace (kn, "_", " "));
      ev.type= SDL_EVENT_KEY_DOWN;
      ev.key.timestamp= SDL_GetTicksNS ();
      ev.key.windowID= script_window_id (win);
      ev.key.scancode= SDL_GetScancodeFromName (name);
      ev.key.key= SDL_GetKeyFromScancode (ev.key.scancode, SDL_KMOD_NONE, false);
      ev.key.mod= mod;
      ev.key.down= true;
      SDL_PushEvent (&ev);
      if (N(a) > 2) {
        // "key <name> <text>": the text which the system sends with the
        // key, at once (a digit of the keypad)
        static c_string ktext ("");
        ktext= c_string (a[2]);
        SDL_zero (ev);
        ev.type= SDL_EVENT_TEXT_INPUT;
        ev.text.timestamp= SDL_GetTicksNS ();
        ev.text.windowID= script_window_id (win);
        ev.text.text= ktext;
        SDL_PushEvent (&ev);
      }
    }
    else if (cmd == "text" && N(a) > 1) {
      // one text input event per (utf8) character, as SDL does. The events
      // keep pointers to the text: the characters of the line are kept
      // until the next "text" command, which comes once these events have
      // all been handled (script_step waits for an empty queue); a ring of
      // 64 slots was overwritten by a longer line before it was read
      static array<c_string> buffers;
      buffers= array<c_string> ();
      string txt= line (N(cmd)+1, N(line));
      int i= 0;
      while (i < N(txt)) {
        int start= i;
        // the length of the utf8 sequence from its lead byte
        unsigned char c= (unsigned char) txt[i];
        int len= (c < 0x80) ? 1 : (c >= 0xF0) ? 4 : (c >= 0xE0) ? 3 : (c >= 0xC0) ? 2 : 1;
        i= min (N(txt), start + len);
        buffers << c_string (txt (start, i));
        SDL_Event ev;
        SDL_zero (ev);
        ev.type= SDL_EVENT_TEXT_INPUT;
        ev.text.timestamp= SDL_GetTicksNS ();
        ev.text.windowID= script_window_id (win);
        ev.text.text= buffers[N(buffers) - 1];
        SDL_PushEvent (&ev);
      }
    }
    else if (cmd == "snapshot" && N(a) > 1) {
      snapshot_win= win;
      snapshot_name= a[1];
    }
    else if (cmd == "resize" && N(a) > 2)
      win->set_size (as_int (a[1]) * PIXEL, as_int (a[2]) * PIXEL);
    else if (cmd == "repaint") // every editor from scratch (checks the incremental paths)
      vue_simple_widget_rep::invalidate_all_editors ();
    else if (cmd == "scheme" && N(line) > 7)
      exec_delayed (scheme_cmd (line (7, N(line))));
    else if (cmd == "focus") {
      // pretend the window got the keyboard focus (a test instance launched
      // while another application is in use never gets it)
      // (a virtual window: the host gets it, and that window the keys)
      vue_virtual_window_rep* v= dynamic_cast<vue_virtual_window_rep*> (win);
      if (v != NULL) { v->raise (); focus_virtual (v); }
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_WINDOW_FOCUS_GAINED;
      ev.window.timestamp= SDL_GetTicksNS ();
      ev.window.windowID= script_window_id (win);
      SDL_PushEvent (&ev);
    }
    else if (cmd == "commit" && N(a) > 1) {
      // the text committed by an input method: several characters in one
      // text input event
      static c_string ctext ("");
      ctext= c_string (line (N(cmd)+1, N(line)));
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_TEXT_INPUT;
      ev.text.timestamp= SDL_GetTicksNS ();
      ev.text.windowID= script_window_id (win);
      ev.text.text= ctext;
      SDL_PushEvent (&ev);
    }
    else if (cmd == "compose") {
      // the composition of an input method: "compose <text>" (no text ends it)
      static string composed; // SDL keeps the pointer: the string must live on
      composed= (N(a) > 1) ? line (N(cmd)+1, N(line)) : string ("");
      static c_string ctext ("");
      ctext= c_string (composed);
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_TEXT_EDITING;
      ev.edit.timestamp= SDL_GetTicksNS ();
      ev.edit.windowID= script_window_id (win);
      ev.edit.text= ctext;
      ev.edit.start= N(composed);
      ev.edit.length= 0;
      SDL_PushEvent (&ev);
    }
    else if (cmd == "drop" && N(a) > 3) {
      // "drop x y <path>" or "drop x y text:<text>": a synthetic drop of
      // one item, as the system sends it (begin, item, complete)
      static char buf[1024];
      string item= line (N(cmd) + N(a[1]) + N(a[2]) + 3, N(line));
      bool is_text= starts (item, "text:");
      if (is_text) item= item (5, N(item));
      c_string citem (item);
      int n= min ((int) strlen (citem), 1023);
      for (int i= 0; i < n; i++) buf[i]= ((char*) citem)[i];
      buf[n]= 0;
      float dx= as_double (a[1]), dy= as_double (a[2]);
      Uint32 wid= script_window_id (win, dx, dy);
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_DROP_BEGIN;
      ev.drop.timestamp= SDL_GetTicksNS ();
      ev.drop.windowID= wid;
      SDL_PushEvent (&ev);
      SDL_zero (ev);
      ev.type= is_text ? SDL_EVENT_DROP_TEXT : SDL_EVENT_DROP_FILE;
      ev.drop.timestamp= SDL_GetTicksNS ();
      ev.drop.windowID= wid;
      ev.drop.x= dx;
      ev.drop.y= dy;
      ev.drop.data= buf;
      SDL_PushEvent (&ev);
      SDL_zero (ev);
      ev.type= SDL_EVENT_DROP_COMPLETE;
      ev.drop.timestamp= SDL_GetTicksNS ();
      ev.drop.windowID= wid;
      ev.drop.x= dx;
      ev.drop.y= dy;
      SDL_PushEvent (&ev);
    }
    else if (cmd == "close" && win->platform_window () == NULL)
      win->destroy_event (); // a virtual window: as its close box does
    else if (cmd == "close") {
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_WINDOW_CLOSE_REQUESTED;
      ev.window.timestamp= SDL_GetTicksNS ();
      ev.window.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
      SDL_PushEvent (&ev);
    }
    else cout << "vue script: unknown command " << line << LF;
    return; // one command per loop iteration
  }
  cout << "vue script: done" << LF;
  script_active= false;
}

// the state of the buttons and of the modifiers, as TeXmacs encodes it
static unsigned int
mouse_bits (Uint32 buttons, SDL_Keymod mods) {
  unsigned int state= 0;
  if ((buttons & SDL_BUTTON_LMASK) != 0)  state += 1;
  if ((buttons & SDL_BUTTON_MMASK) != 0)  state += 2;
  if ((buttons & SDL_BUTTON_RMASK) != 0)  state += 4;
  if ((buttons & SDL_BUTTON_X1MASK) != 0) state += 8;
  if ((buttons & SDL_BUTTON_X2MASK) != 0) state += 16;
  if ((mods & SDL_KMOD_SHIFT) != 0) state += 256;
  if ((mods & SDL_KMOD_CTRL)  != 0) state += 1024;
  if ((mods & SDL_KMOD_ALT)   != 0) state += 2048;
  if ((mods & SDL_KMOD_GUI)   != 0) state += 4096;
#ifdef OS_MACOS
  // a one button mouse: control and option with the button make a right
  // and a middle click, as in the Qt port (QTMWidget.cpp), which passes
  // the modifiers too. Only with the button: the lost-release check of the
  // widgets (gui_init_context) takes a bare control for a held button
  if ((buttons & SDL_BUTTON_LMASK) != 0) {
    if ((mods & SDL_KMOD_CTRL) != 0) state |= 4;
    if ((mods & SDL_KMOD_ALT)  != 0) state |= 2;
  }
#endif
  return state;
}

static Uint32
mouse_buttons () {
  float x, y;
  Uint32 buttons= SDL_GetGlobalMouseState (&x, &y);
  if (script_active) buttons= script_buttons; // synthetic events
  return buttons;
}

static void update_mouse_state () {
  mouse_state= mouse_bits (mouse_buttons (), SDL_GetModState ());
}

static string
mouse_decode (unsigned int mstate) {
  // we check (mstate & 1) at last since it is usually set
  if (mstate & 2)       return "middle";
  else if (mstate & 4)  return "right";
  else if (mstate & 8)  return "up";
  else if (mstate & 16) return "down";
  else if (mstate & 1)  return "left";
  return "unknown";
}

SDL_Keycode 
postprocess_key_event (SDL_Scancode scancode, SDL_Keymod *current_mod, bool is_key_event) {
  SDL_Keycode out_key;

  // Test all combinations of shift and alt
  static SDL_Keymod combinations[4]= {
    SDL_KMOD_NONE,
    SDL_KMOD_SHIFT,
    SDL_KMOD_ALT,
    SDL_KMOD_SHIFT | SDL_KMOD_ALT
  };

  SDL_Keycode results[4];
  for (int i = 0; i < 4; i++) {
    results[i]= SDL_GetKeyFromScancode (scancode, combinations[i], is_key_event);
  }

  // Check if the base key (results[0]) is a modifier
  bool is_modifier = (results[0] == SDLK_LSHIFT   || results[0] == SDLK_RSHIFT ||
                      results[0] == SDLK_LCTRL    || results[0] == SDLK_RCTRL ||
                      results[0] == SDLK_LALT     || results[0] == SDLK_RALT ||
                      results[0] == SDLK_LGUI     || results[0] == SDLK_RGUI ||
                      results[0] == SDLK_LMETA    || results[0] == SDLK_RMETA ||
                      results[0] == SDLK_CAPSLOCK || results[0] == SDLK_NUMLOCKCLEAR ||
                      results[0] == SDLK_SCROLLLOCK);

  if (is_modifier) {
    return SDLK_UNKNOWN;
  }

  // Remove modifiers in current event are already used to compose key.
  // On macOS option composes a character, except in a shortcut: with
  // command or control it stays a modifier (M-A-x, A-C-x), as in the Qt
  // port; option+s with command was taken for the "ß" of M-ß
  bool fold_alt= true;
#ifdef OS_MACOS
  if ((*current_mod & (SDL_KMOD_CTRL | SDL_KMOD_GUI)) != 0) fold_alt= false;
#endif
  if (!fold_alt) {
    if ((*current_mod & SDL_KMOD_SHIFT) && (results[1] != results[0])) {
      *current_mod&= ~SDL_KMOD_SHIFT;
      out_key= results[1];
    }
    else out_key= results[0];
  }
  else if ( (*current_mod & SDL_KMOD_SHIFT) && (*current_mod & SDL_KMOD_ALT) && (results[3] != results[0])) {
    *current_mod&= ~(SDL_KMOD_SHIFT | SDL_KMOD_ALT);
    out_key= results[3];
  } else if ( (*current_mod & SDL_KMOD_ALT) && (results[2] != results[0])) {
    *current_mod&= ~SDL_KMOD_ALT;
    out_key= results[2];
  } else if ( (*current_mod & SDL_KMOD_SHIFT) && (results[1] != results[0])) {
    *current_mod&= ~SDL_KMOD_SHIFT;
    out_key= results[1];
  } else {
    out_key= results[0];
  }
  return out_key;
}

// While a popup window is visible it grabs the pointer: mouse events sent to
// other windows are redirected to the popup when the pointer is over it, and
// dropped otherwise (except for a button press, which dismisses the popup by
// reaching its target). This mimics the X11 grab which edit_mouse relies on.
static vue_window
visible_popup () {
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy ()) {
    SDL_Window* sw= it->next ();
    vue_window w= (vue_window) Window_to_window [sw];
    if (w->popup && !(SDL_GetWindowFlags (sw) & SDL_WINDOW_HIDDEN)) return w;
  }
  return NULL;
}

static bool
popup_grab (vue_window& win, float& x, float& y, bool press) {
  vue_window pop= visible_popup ();
  if (pop == NULL || win == NULL || win == pop) return true;
  int wx, wy, px, py, pw, ph;
  SDL_GetWindowPosition ((SDL_Window*) win->platform_window (), &wx, &wy);
  SDL_GetWindowPosition ((SDL_Window*) pop->platform_window (), &px, &py);
  SDL_GetWindowSize ((SDL_Window*) pop->platform_window (), &pw, &ph);
  float sx= wx + x, sy= wy + y; // screen coordinates
  if (sx >= px && sx < px + pw && sy >= py && sy < py + ph) {
    win= pop;
    x= sx - px;
    y= sy - py;
    return true;
  }
  if (!press) return false;
  // a press outside dismisses the popup and reaches its target
  pop->set_visibility (false);
  return true;
}

// The wheel with control (command on macOS) alone zooms the editor rather
// than scrolling it, as in the Qt port (QTMWidget::wheelEvent): by the
// sixteenth root of the displacement (in degrees for a notch of a wheel, in
// points for a trackpad) for each event. It is the editor with the keyboard
// focus in the window under the pointer, which is the current one when the
// window is. Returns true if the event was used; not when the editor
// captures the wheel.
static bool
wheel_zooms (vue_window win, double y) {
#ifdef OS_MACOS
  const SDL_Keymod zoom_mod= SDL_KMOD_GUI;
#else
  const SDL_Keymod zoom_mod= SDL_KMOD_CTRL;
#endif
  const SDL_Keymod all= SDL_KMOD_SHIFT | SDL_KMOD_CTRL | SDL_KMOD_ALT | SDL_KMOD_GUI;
  SDL_Keymod mods= SDL_GetModState () & all;
  if (mods == 0 || (mods & ~zoom_mod) != 0) return false;
  if (dynamic_cast<vue_simple_widget_rep*> (win->kbd_focus.rep) == NULL) return false;
  // as in Qt, the editor gets the wheel when it captures it (graphics)
  if (as_bool (call ("wheel-capture?"))) return false;
  if (y == 0) return true; // a sideways wheel does not scroll either
  double m= (y == floor (y)) ? 15.0 * fabs (y) : wheel_precise_step * fabs (y);
  double f= pow (max (m, 1.0), 1.0 / 16.0);
  if (f <= 1.0) return true;
  string cmd= (y > 0 ? "(zoom-in " : "(zoom-out ") * as_string (f) * ")";
  exec_delayed (scheme_cmd (cmd));
  return true;
}

void
process_event (SDL_Event *event) {
  // note: events are stored in the input state of their window and cleared
  // once that window has been laid out (see gui_finalize_context)
  vue_window win;
  if (DEBUG_VUE_EVENTS && event->type != SDL_EVENT_MOUSE_MOTION)
    sdl_log_event (event);
  if (vue_dialog_event != 0 && event->type == vue_dialog_event) {
    // the result of a file dialog, pushed by its callback (which may run
    // on another thread): the command runs here, on the main thread
    vue_dialog_finish ((vue_dialog_result*) event->user.data1,
                       (char*) event->user.data2, event->user.code != 0);
    return;
  }
  switch (event->type) {
    case SDL_EVENT_QUIT:
      // the Quit of the application menu or of the Dock, a signal: as the
      // Quit of TeXmacs, which asks about the unsaved documents (SDL does
      // not send it any more when the last window closes, gui_start_loop)
      exec_delayed (scheme_cmd ("(safely-quit-TeXmacs)"));
      break;
    case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->destroy_event();
      break;
    case SDL_EVENT_WINDOW_MOUSE_LEAVE:
      // popup menus are dismissed as soon as the pointer leaves them
      win= get_window_from_ID (event->window.windowID);
      if (win && win->popup) win->set_visibility (false);
      else if (win) {
        // no element is under the pointer any more (no hovered button left
        // highlighted behind); an element being dragged stays active and
        // keeps the last position inside the window, so that it freezes
        // there instead of jumping to an extreme
        with_window frame (win);
        Clay_SetPointerState ((Clay_Vector2) { -1, -1 }, false);
        win->input.mouse_action= "move";
        win->input.mouse_time= texmacs_time ();
        // nor of the virtual window it was over (single-window mode)
        if (is_host (win) && pointer_hover != win) pointer_left (pointer_hover);
        if (is_host (win)) pointer_hover= NULL;
      }
      break;
    case SDL_EVENT_SYSTEM_THEME_CHANGED:
    {
      // the system switched between its light and dark appearance: the
      // widgets follow it when the preference does not force a theme
      string pref= get_preference ("gui theme", "default");
      if (pref != "light" && pref != "dark") {
        set_vue_theme (pref);
        gui_needs_relayout= true;
        vue_simple_widget_rep::invalidate_all_editors ();
      }
      break;
    }
    case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
    case SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED:
      // the window moved to a display of another density: the layout and
      // the backing stores of its editors follow (their pixel size changes
      // with the density, which the repaint notices)
      win= get_window_from_ID (event->window.windowID);
      if (win) {
        int was= win->retina;
        win->update_density ();
        if (win->retina != was) {
          with_window frame (win);
          vue_simple_widget_rep::invalidate_all_editors ();
          gui_needs_relayout= true;
        }
      }
      break;
    case SDL_EVENT_WINDOW_FOCUS_GAINED:
    case SDL_EVENT_WINDOW_FOCUS_LOST:
      // tell the focused widget of the window (e.g. the editor, which hides
      // its cursor) whether the window has the keyboard focus
      win= get_window_from_ID (event->window.windowID);
      if (win) notify_window_focus (route_keys (win), event->type == SDL_EVENT_WINDOW_FOCUS_GAINED);
      break;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
    {
      update_mouse_state ();
      win= get_window_from_ID (event->button.windowID);
      float bx= event->button.x, by= event->button.y;
      bool down= (event->button.type == SDL_EVENT_MOUSE_BUTTON_DOWN);
      if (down) dismiss_wait_indicator ();
      win= route_pointer (win, bx, by, down ? 1 : 2);
      if (win && popup_grab (win, bx, by, down)) {
        // the button of the event, whatever the state says by now (a
        // release comes with the button already up), with the modifiers
        // which make it another button (see mouse_bits)
        unsigned int bits= mouse_bits (mouse_buttons () | SDL_BUTTON_MASK (event->button.button),
                                       SDL_GetModState ());
        string action= (down ? "press-" : "release-") * mouse_decode (bits);
        vue_input_state& in= win->input;
        in.mouse_action= action;
        if (down) in.mouse_clicks= max (1, (int) event->button.clicks);
        in.mouse_time= texmacs_time();
        in.mouse_x= (int) (bx * win->density);
        in.mouse_y= (int) (by * win->density);
        with_window frame (win);
        Clay_SetPointerState ((Clay_Vector2) { (float) in.mouse_x, (float) in.mouse_y },
                             (event->button.button == SDL_BUTTON_LEFT) &&
                             (event->button.type == SDL_EVENT_MOUSE_BUTTON_DOWN));
      }
      break;
    } // case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_WHEEL:
    {
      update_mouse_state ();
      if (DEBUG_VUE_EVENTS)
        SDL_Log ("Window %d got wheel event %f %f (queued for %d ms)",
                 event->wheel.windowID, event->wheel.x, event->wheel.y,
                 (int) ((SDL_GetTicksNS () - event->wheel.timestamp) / 1000000));
      win= get_window_from_ID (event->wheel.windowID);
      float wx= event->wheel.mouse_x, wy= event->wheel.mouse_y;
      win= route_pointer (win, wx, wy, 3);
      if (win && wheel_zooms (win, event->wheel.y)) break;
      if (win) {
        vue_input_state& in= win->input;
        in.mouse_time= texmacs_time();
        in.mouse_x= (int) (wx * win->density);
        in.mouse_y= (int) (wy * win->density);
        wheel_event (win, event->wheel.x, event->wheel.y, in.mouse_time,
                     event->wheel.timestamp); // see "Scrolling with the wheel"
      }
      break;
    } // case SDL_EVENT_MOUSE_WHEEL:
    case SDL_EVENT_MOUSE_MOTION:
    {
      close_help_balloon (); // it lives until the pointer or a key moves
      update_mouse_state ();
      win= get_window_from_ID (event->motion.windowID);
      float mx= event->motion.x, my= event->motion.y;
      win= route_pointer (win, mx, my, 0);
      if (win && popup_grab (win, mx, my, false)) {
        with_window frame (win);
        Clay_SetPointerState ((Clay_Vector2) { mx * win->density, my * win->density },
                             (event->motion.state & SDL_BUTTON_LMASK) != 0);
        vue_input_state& in= win->input;
        in.mouse_action= "move";
        in.mouse_time= texmacs_time();
        in.mouse_x= (int) (mx * win->density);
        in.mouse_y= (int) (my * win->density);
      }
      break;
    } // case SDL_EVENT_MOUSE_MOTION:
    case SDL_EVENT_KEY_DOWN:
    {
      close_help_balloon ();
      dismiss_wait_indicator ();
      if (DEBUG_VUE_EVENTS) {
        c_string buf (print_key_info (&(event->key)));
        SDL_Log ("Keydown: %s ", (char*) buf);
      }
      win= route_keys (get_window_from_ID (event->key.windowID));
      if (win) {
        if (event->key.scancode == SDL_SCANCODE_F1 && clay_debug_key () &&
            (event->key.mod & (SDL_KMOD_SHIFT | SDL_KMOD_CTRL |
                               SDL_KMOD_ALT | SDL_KMOD_GUI)) == 0) {
          // toggle the debug mode for the current window (a development
          // aid: otherwise F1 is a key of TeXmacs, the help)
          win->clay_debug = !win->clay_debug;
          if (DEBUG_VUE) debug_widgets << "Clay debug view " << (win->clay_debug ? "on" : "off") << LF;
          break;
        }

        bool produces_text= false;
        string echo;
        string key= lookup_key (event->key.scancode, event->key.mod, &produces_text, &echo);
        if (produces_text) {
          // the text event of this keystroke follows (or not: a dead key)
          if (N(key) > 0) request_partial_redraw= true;
          break;
        }
        
        if (N(key)>0) {
          //cout << "Press " << key << " at " << (time_t) ev->xkey.time
          //<< " (" << texmacs_time() << ")\n";
          kbd_count++;
          // SDL3 timestamps are nanoseconds; they were cast to 32 bits and
          // compared with milliseconds, which wrapped every 4.3 seconds and
          // set request_partial_redraw at random
          Uint32 stamp= (Uint32) (event->key.timestamp / 1000000ull);
          synchronize_time (stamp);
          if (texmacs_time () - remote_time (stamp) < 100 ||
              (kbd_count & 15) == 0)
            request_partial_redraw= true;
          //cout << "key   : " << key << "\n";
          //cout << "redraw: " << request_partial_redraw << "\n";
          //if (N(key)>0) win->key_event (key);

          win->input.key_event= key;
          win->input.key_time= texmacs_time();
          win->input.last_key= key;
          // only a keystroke with a modifier may still get a text event of
          // its own; without one, the text which follows is the next key's,
          // unless it is the very text this key types (see kbd_echo)
          bool with_mods= (event->key.mod & (SDL_KMOD_CTRL | SDL_KMOD_ALT | SDL_KMOD_GUI)) != 0;
          win->input.key_stamp= with_mods ? event->key.timestamp : 0;
          kbd_echo= echo;
          kbd_echo_stamp= event->key.timestamp;
        }
      }
      break;
    } // case SDL_EVENT_KEY_DOWN:
    case SDL_EVENT_TEXT_INPUT:
    {
      // the text typed by a keystroke (see SDL_EVENT_KEY_DOWN), as the key
      // names of TeXmacs for the characters which have one
      string txt= (event->text.text != NULL) ? string (event->text.text) : string ("");
      win= route_keys (get_window_from_ID (event->text.windowID));
      // the text of a key delivered as a key (a digit of the keypad,
      // shift+space), which SDL sends as well: it was typed twice
      bool echo= N(kbd_echo) > 0 && txt == kbd_echo &&
                 event->text.timestamp - kbd_echo_stamp < 30000000ull;
      kbd_echo= "";
      if (win && N(txt) > 0) {
        // a text event right after a key delivered as a key (a command
        // modifier, an unconsumed alt) belongs to that keystroke
        if (echo || (win->input.key_stamp != 0 &&
                     event->text.timestamp - win->input.key_stamp < 30000000ull)) {
          if (DEBUG_VUE_EVENTS) {
            c_string lk (win->input.last_key);
            SDL_Log ("Text input '%s' follows the key %s: ignored", event->text.text, (char*) lk);
          }
        } else {
          // one key per character: an input method commits a whole word
          // at once, and the editor takes a key for one character (the Qt
          // port splits it too, QTMWidget::inputMethodEvent); the others
          // wait in pending_keys, one per frame
          string r= utf8_to_cork (txt);
          array<string> keys;
          int pos= 0;
          while (pos < N(r)) {
            int start= pos;
            tm_char_forwards (r, pos);
            if (pos <= start) pos= start + 1;
            string k= cork_key (r (start, pos));
            if (k == " ") k= "space";
            keys << k;
          }
          if (N(keys) > 0) {
            win->input.key_event= keys[0];
            win->input.key_time= texmacs_time();
            win->input.last_key= keys[0];
            for (int i= 1; i < N(keys); i++) pending_keys << keys[i];
            if (N(keys) > 1) pending_keys_win= win->id;
          }
        }
      }
      break;
    } //case SDL_EVENT_TEXT_INPUT


    // Drag and drop: SDL sends DROP_BEGIN, then one DROP_FILE or DROP_TEXT
    // per item, then DROP_COMPLETE. The items are collected in a tree and
    // handed to the editor as a "drop" mouse action carrying a ticket; the
    // editor reads the payload back (call_drop_event in edit_mouse.cpp) and
    // calls mouse-drop-event.
    case SDL_EVENT_DROP_BEGIN:
      drop_doc= tree (CONCAT);
      break;
    case SDL_EVENT_DROP_FILE:
    case SDL_EVENT_DROP_TEXT:
    {
      if (event->drop.data == NULL) break;
      string item= utf8_to_cork (string (event->drop.data,
                                         (int) strlen (event->drop.data)));
      if (event->type == SDL_EVENT_DROP_TEXT) drop_doc << item;
      else {
        // a file: images are inserted as such, everything else by name
        url u= url_system (item);
        string ext= locase_all (suffix (u));
        if (ext == "png" || ext == "jpg" || ext == "jpeg" || ext == "gif" ||
            ext == "tif" || ext == "tiff" || ext == "bmp" || ext == "svg" ||
            ext == "pdf" || ext == "ps" || ext == "eps") {
          string iw, ih;
          vue_pretty_image_size (u, iw, ih);
          drop_doc << tree (IMAGE, as_string (u), iw, ih, "", "");
        }
        else drop_doc << as_string (u);
      }
      break;
    }
    case SDL_EVENT_DROP_COMPLETE:
    {
      win= get_window_from_ID (event->drop.windowID);
      float dx= event->drop.x, dy= event->drop.y;
      win= route_pointer (win, dx, dy, 3);
      if (win == NULL || N(drop_doc) == 0) { drop_doc= tree (CONCAT); break; }
      vue_input_state& in= win->input;
      in.mouse_action= "drop";
      in.mouse_time= texmacs_time ();
      in.mouse_x= (int) (dx * win->density);
      in.mouse_y= (int) (dy * win->density);
      in.mouse_ticket= ++drop_serial;
      payloads (in.mouse_ticket)= drop_doc;
      if (DEBUG_VUE_EVENTS)
        debug_events << "drop of " << N(drop_doc) << " item(s) at "
                     << in.mouse_x << "," << in.mouse_y << LF;
      drop_doc= tree (CONCAT);
      break;
    }
    case SDL_EVENT_TEXT_EDITING:
    {
      // the composition of an input method (dead keys, CJK...): the editor
      // shows it as a pre-edit ("pre-edit:<cursor>:<text>", an empty text
      // ends it), as the Qt port does; the committed text comes as a text
      // input event
      win= route_keys (get_window_from_ID (event->edit.windowID));
      if (win) {
        string t= (event->edit.text != NULL) ? utf8_to_cork (string (event->edit.text)) : string ("");
        string k= "pre-edit:";
        if (N(t) > 0) k << as_string (max (0, (int) event->edit.start)) << ":" << t;
        if (DEBUG_VUE_EVENTS) debug_events << "key press: " << k << LF;
        win->input.key_event= k;
        win->input.key_time= texmacs_time ();
        win->input.key_stamp= 0;
      }
      break;
    }
  } // switch (event->type)
}

bool event_filter (void *userdata, SDL_Event *event) {
  if (event->type == SDL_EVENT_WINDOW_RESIZED) {
    // A resize is handled here, inside SDL's event pump, so that the window
    // never shows stale content while it is dragged. The pump runs from
    // every SDL_PollEvent/SDL_WaitEventTimeout/SDL_PushEvent, hence also
    // from inside a frame: without this guard a whole frame (layout, the
    // interpose handler with its Scheme, repaint, redraw) could start in
    // the middle of another one.
    static bool busy= false;
    if (busy) return true;
    // only while the main loop waits for events (see watch_may_run): the
    // event stays in the queue, and the next frame lays the window out at
    // its new size
    if (!watch_may_run) return true;
    vue_window win= get_window_from_ID (event->window.windowID);
    if (win) {
      busy= true;
      with_window frame (win);
      Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) event->window.data1, (float) event->window.data2 });
      // one window only, so the pools are not released here: the commands
      // of the other windows still name their widgets and their texts. A
      // drag therefore accumulates one layout's worth of each per event,
      // until the loop runs process_layout again when the drag ends.
      // the interpose handler and the repaint run Scheme, which may close
      // the window: it is looked up again after them (by its id and its
      // SDL window, a new window may have been given its address)
      int vid= win->id;
      Uint32 sid= event->window.windowID;
      auto alive= [vid, sid, win] () {
        return id_to_window->contains (vid) && get_window_from_ID (sid) == win; };
      win->process_layout();
      vue_simple_widget_rep::notify_resizes ();
      if (the_interpose_handler != NULL) the_interpose_handler ();
      if (gui_needs_relayout) process_layout ();
      if (alive ()) vue_simple_widget_rep::repaint_all_in_window (win);
      // the repaint may have replaced widgets, see gui_start_loop
      for (int pass= 0; gui_needs_relayout && pass < 4; pass++) process_layout ();
      if (alive ()) win->process_redraw();
      busy= false;
      return true; // the return value of a watch is ignored by SDL anyway
    }
  }
  return true;
}


/******************************************************************************
* Font support
******************************************************************************/

void set_default_font (string name) {
  // set the name of the default font
  //FIXME: ignore?
}

font get_default_font (bool tt, bool mini, bool bold) {
  // get the default font, depending on desired characteristics:
  // tt for a monospaced font, mini for a smaller font and bold for a bold font
  string s= "";
  string series= (bold? string ("bold"): string ("medium"));
  if (s == "") s= "ecrm11@300";
  int i, j, n= N(s);
  for (j=0; j<n; j++) if (is_digit (s[j])) break;
  string fam= s (0, j);
  if (mini && fam == "ecrm") fam= "ecss";
  if (bold && fam == "ecrm") fam= "ecbx";
  if (bold && fam == "ecss") fam= "ecsx";
  for (i=j; j<n; j++) if (s[j] == '@') break;
  int sz= (j<n? as_int (s (i, j)): 10);
  if (j<n) j++;
  int dpi= (j<n? as_int (s (j, n)): 300);
  if (mini) { sz= (int) (0.6 * sz); dpi= (int) (1.3333333 * dpi); }
#ifdef __EMSCRIPTEN__
  // the browser: Fira, which comes with TeXmacs (the TeX fonts of the
  // fallback below have no glyph for the arrows and marks of the widgets)
  if (tt) return unicode_font (bold ? "FiraMono-Bold" : "FiraMono-Regular", sz, dpi);
  return unicode_font (bold ? "FiraSans-Bold" : "FiraSans-Regular",
                       sz, (int) (0.95 * dpi));
#endif
  if (tt) {
    // WIDGET_STYLE_MONOSPACED: a typewriter font, whatever the platform
    tree tt_fn= tuple ("modern", "tt", series, "right");
    tt_fn << as_string (sz) << as_string (dpi);
    return find_font (tt_fn);
  }
  if (use_macos_fonts ()) {
    // The family is named directly rather than through the "apple-lucida"
    // tuple: the rule which translates that name (fonts-truetype.scm) maps
    // it to the regular face whatever the series is asked for, so every
    // bold label came out in the regular weight. The Qt port does not go
    // through this at all, it asks Qt for a bold QFont.
    return find_font ("Lucida Grande", "ss", series, "right",
                      sz, (int) (0.95 * dpi));
  }
  if (N(fam) >= 2) {
    string ff= fam (0, 2);
    string out_lan= get_output_language ();
    if (((out_lan == "bulgarian") || (out_lan == "russian") ||
   (out_lan == "ukrainian")) &&
  ((ff == "cm") || (ff == "ec"))) {
      fam= "la" * fam (2, N(fam)); ff= "la"; if (sz<100) sz *= 100; }
    if (out_lan == "japanese" || out_lan == "korean") {
      tree modern_fn= tuple ("modern", "ss", series, "right");
      modern_fn << as_string (sz) << as_string (dpi);
      return find_font (modern_fn);
    }
    if (out_lan == "chinese" || out_lan == "taiwanese")
      return unicode_font ("fireflysung", sz, dpi);
    if (out_lan == "greek")
      return unicode_font ("Stix", sz, dpi);
    //if (out_lan == "japanese")
    //return unicode_font ("ipagui", sz, dpi);
    //if (out_lan == "korean")
    //return unicode_font ("UnDotum", sz, dpi);
    if (ff == "ec")
      return tex_ec_font (tt? ff * "tt": fam, sz, dpi);
    if (ff == "la")
      return tex_la_font (tt? ff * "tt": fam, sz, dpi, 1000);
    if (ff == "pu") tt= false;
    if ((ff == "cm") || (ff == "pn") || (ff == "pu"))
      return tex_cm_font (tt? ff * "tt": fam, sz, dpi);
  }
  return tex_font (fam, sz, dpi);
  // if (out_lan == "german") return tex_font ("ygoth", 14, 300, 0);
  // return tex_font ("rpagk", 10, 300, 0);
  // return tex_font ("rphvr", 10, 300, 0);
  // return ps_font ("b&h-lucidabright-medium-r-normal", 11, 300);
  return NULL;
}

void load_system_font (string family, int size, int dpi,
                       font_metric& fnm, font_glyphs& fng) {
  // load the metric and glyphs of a system font
  // you are not obliged to provide any system fonts
  FAILED("this should not be called");
}

/******************************************************************************
* Clipboard support
******************************************************************************/

// Internal storage for selections (for primary/mouse selections not supported by SDL)
static hashmap<string,tree> selection_t ("none");
static hashmap<string,string> selection_s ("");

// Structure to hold clipboard data for the callback
struct clipboard_data {
  string texmacs_data;    // TeXmacs native format
  string plain_text;      // Plain text (verbatim)
  string html_text;       // HTML format
  string format_type;     // Format type (default, verbatim, html, latex)

  // C string versions (owned by this structure, must persist until cleanup)
  c_string c_texmacs_data;
  c_string c_plain_text;
  c_string c_html_text;

  // the c_string members start empty (c_string (NULL) resolved to the
  // length constructor with a zero length by accident)
  clipboard_data (): format_type ("default") {}
};

// Clipboard data callback - called when the OS requests clipboard data
static const void* SDLCALL
clipboard_data_callback (void *userdata, const char *mime_type, size_t *size) {
  clipboard_data* data = static_cast<clipboard_data*>(userdata);
  if (!data || !mime_type) {
    *size = 0;
    return NULL;
  }

  string mime_str (mime_type);

  // TeXmacs native format
  if (mime_str == "application/x-texmacs-clipboard") {
    *size = N(data->texmacs_data);
    return (const void*) data->c_texmacs_data;
  }
  // HTML format
  else if (mime_str == "text/html") {
    if (N(data->html_text) > 0) {
      *size = N(data->html_text);
      return (const void*) data->c_html_text;
    }
  }
  // Plain text (UTF-8)
  else if (mime_str == "text/plain" || mime_str == "text/plain;charset=utf-8") {
    if (N(data->plain_text) > 0) {
      *size = N(data->plain_text);
      return (const void*) data->c_plain_text;
    } else {
      *size = N(data->texmacs_data);
      return (const void*) data->c_texmacs_data;
    }
  }

  // Default: return texmacs data
  *size = N(data->texmacs_data);
  return (const void*) data->c_texmacs_data;
}

// Clipboard cleanup callback - called when clipboard is cleared or replaced
static void SDLCALL
clipboard_cleanup_callback (void *userdata) {
  clipboard_data* data = static_cast<clipboard_data*>(userdata);
  if (data) {
    delete data;
  }
}

// a text in Latin-1 as UTF-8 (the clipboard of SDL is in UTF-8)
static string
latin1_to_utf8 (string s) {
  string r;
  for (int i= 0; i < N(s); i++) {
    unsigned char c= (unsigned char) s[i];
    if (c < 0x80) r << (char) c;
    else { r << (char) (0xC0 | (c >> 6)); r << (char) (0x80 | (c & 0x3F)); }
  }
  return r;
}

// the size in pixels of a PNG image, from its header; false if it is none
static bool
png_size (string s, int& w, int& h) {
  if (N(s) < 24 || s (0, 8) != string ("\x89PNG\r\n\x1a\n", 8) ||
      s (12, 16) != "IHDR") return false;
  w= 0; h= 0;
  for (int i= 16; i < 20; i++) w= (w << 8) | (unsigned char) s[i];
  for (int i= 20; i < 24; i++) h= (h << 8) | (unsigned char) s[i];
  return w > 0 && h > 0;
}

bool set_selection (string key, tree t,
                    string s, string sv, string sh, string format) {

  // Copy a selection 't' of a given 'format' to the clipboard 'cb',
  // where 's' contains the string serialization of t according to the format
  // and possibly the variants 'sv' and 'sh' for verbatim and html
  // Returns true on success

  // Store selection internally
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);

  // SDL3 only supports the system clipboard, not primary/mouse selections
  // So we only set the system clipboard for "primary" key
  if (key != "primary") return true;

  // Prepare clipboard data structure
  clipboard_data* clip_data = new clipboard_data();
  clip_data->texmacs_data = s;
  clip_data->format_type = format;

  // Handle encoding for plain text
  string plain_text = sv;
  if (format == "verbatim" || format == "default") {
    if (format == "default" && N(sv) > 0) {
      plain_text = sv;
    } else if (N(sv) == 0) {
      plain_text = s;
    }

    // the clipboard holds UTF-8: the verbatim text is in the encoding of
    // the preference, taken for Latin-1 unless it is UTF-8 (as the Qt port
    // does, qt_gui.cpp)
    string enc = get_preference ("texmacs->verbatim:encoding");
    if (enc == "auto")
      enc = get_locale_charset ();
    if (enc != "utf-8" && enc != "UTF-8")
      plain_text = latin1_to_utf8 (plain_text);
    clip_data->plain_text = plain_text;
  }
  else if (format == "html") {
    clip_data->html_text = s;
    clip_data->plain_text = s; // Also provide as plain text fallback
  }
  else if (format == "latex") {
    string enc = get_preference ("texmacs->latex:encoding");
    if (enc == "utf-8" || enc == "UTF-8" || enc == "cork")
      clip_data->plain_text= s;
    else clip_data->plain_text= latin1_to_utf8 (s);
  }
  else {
    clip_data->plain_text = s;
  }

  if (N(sh) > 0) {
    clip_data->html_text = sh;
  }

  // Initialize c_string versions (these will persist until cleanup callback)
  clip_data->c_texmacs_data = c_string (clip_data->texmacs_data);
  if (N(clip_data->plain_text) > 0) {
    clip_data->c_plain_text = c_string (clip_data->plain_text);
  }
  if (N(clip_data->html_text) > 0) {
    clip_data->c_html_text = c_string (clip_data->html_text);
  }

  // Build list of MIME types to offer
  const char* mime_types[4];
  size_t num_mime_types = 0;

  // Always offer TeXmacs native format
  mime_types[num_mime_types++] = "application/x-texmacs-clipboard";

  // Offer HTML if available
  if (N(clip_data->html_text) > 0) {
    mime_types[num_mime_types++] = "text/html";
  }

  // Always offer plain text (UTF-8)
  mime_types[num_mime_types++] = "text/plain;charset=utf-8";
  mime_types[num_mime_types++] = "text/plain";

  // Set clipboard data with callbacks
  if (!SDL_SetClipboardData (clipboard_data_callback,
                              clipboard_cleanup_callback,
                              clip_data,
                              mime_types,
                              num_mime_types)) {
    SDL_Log ("Failed to set clipboard data: %s", SDL_GetError ());
    delete clip_data;
    return false;
  }

  return true;
}

// an image on the clipboard, for the "Copy to > Image" of the edit menu
// (graphics_file_to_clipboard, edit_main.cpp): the contents of the file
// under the MIME type of its format, as qt_gui_rep::put_graphics_on_
// clipboard does; SDL gives the type to the system (public.png, ...)
struct image_clipboard {
  string mime, bytes;
};

static const void* SDLCALL
image_clipboard_callback (void *userdata, const char *mime_type, size_t *size) {
  image_clipboard* img= static_cast<image_clipboard*> (userdata);
  if (img == NULL || mime_type == NULL || img->mime != string (mime_type)) {
    *size= 0;
    return NULL;
  }
  *size= N(img->bytes);
  return (const void*) &(img->bytes[0]);
}

static void SDLCALL
image_clipboard_cleanup (void *userdata) {
  tm_delete (static_cast<image_clipboard*> (userdata));
}

bool
vue_put_graphics_on_clipboard (url file) {
  string ext= locase_all (suffix (file));
  string mime;
  if (ext == "png") mime= "image/png";
  else if (ext == "jpg" || ext == "jpeg") mime= "image/jpeg";
  else if (ext == "bmp") mime= "image/bmp";
  else if (ext == "tif" || ext == "tiff") mime= "image/tiff";
  else if (ext == "svg") mime= "image/svg+xml";
  else if (ext == "pdf") mime= "application/pdf";
  else if (ext == "eps" || ext == "ps") mime= "application/postscript";
  else return false;
  image_clipboard* img= tm_new<image_clipboard> ();
  img->mime= mime;
  if (load_string (file, img->bytes, false) || N(img->bytes) == 0) {
    tm_delete (img);
    return false;
  }
  c_string cmime (mime);
  const char* mime_types[1]= { (const char*) cmime };
  if (!SDL_SetClipboardData (image_clipboard_callback, image_clipboard_cleanup,
                             img, mime_types, 1)) {
    SDL_Log ("Failed to set clipboard data: %s", SDL_GetError ());
    tm_delete (img);
    return false;
  }
  return true;
}

bool get_selection (string key, tree& t, string& s, string format) {
  // Retrieve the selection 't' of a given 'format' from the clipboard 'cb',
  // where 's' is the string serialization of t according to the format
  // Returns true on success; sets t to (extern s) for external selections

  bool direct_selection = (key == "extern");
  if (direct_selection) key = "primary";

  s = "";
  t = "none";

  // The keys other than "primary" are the internal buffers of TeXmacs
  // ("secondary", "ternary", "temp", "wrapbuf", the registers): they live
  // in our own storage, which set_selection fills. This used to sit after
  // a "return false" for every such key, so nothing was ever read back.
  if (key != "primary") {
    if (!selection_t->contains (key)) return false;
    t = copy (selection_t [key]);
    s = copy (selection_s [key]);
    return true;
  }

  // Try to get clipboard data from SDL
  string input_format = "";
  size_t data_size = 0;
  void* data_ptr = NULL;

  // Try different formats based on what's requested and available
  if (format == "default") {
    // Try TeXmacs native format first
    if (SDL_HasClipboardData ("application/x-texmacs-clipboard")) {
      data_ptr = SDL_GetClipboardData ("application/x-texmacs-clipboard", &data_size);
      if (data_ptr) {
        s = string ((char*)data_ptr, data_size);
        SDL_free (data_ptr);
        input_format = "texmacs-snippet";
      }
    }
    // an image (a screenshot...), inserted as a PNG, as in the Qt port
    else if (SDL_HasClipboardData ("image/png")) {
      data_ptr = SDL_GetClipboardData ("image/png", &data_size);
      if (data_ptr) {
        s = string ((char*)data_ptr, data_size);
        SDL_free (data_ptr);
        input_format = "picture";
      }
    }
    // Try HTML format
    else if (SDL_HasClipboardData ("text/html")) {
      data_ptr = SDL_GetClipboardData ("text/html", &data_size);
      if (data_ptr) {
        s = string ((char*)data_ptr, data_size);
        SDL_free (data_ptr);
        input_format = "html-snippet";
      }
    }
    // Try UTF-8 plain text
    else if (SDL_HasClipboardData ("text/plain;charset=utf-8")) {
      data_ptr = SDL_GetClipboardData ("text/plain;charset=utf-8", &data_size);
      if (data_ptr) {
        s = string ((char*)data_ptr, data_size);
        SDL_free (data_ptr);
        input_format = "verbatim-snippet";
      }
    }
    // Fall back to plain text
    else if (SDL_HasClipboardData ("text/plain")) {
      data_ptr = SDL_GetClipboardData ("text/plain", &data_size);
      if (data_ptr) {
        s = string ((char*)data_ptr, data_size);
        SDL_free (data_ptr);
        input_format = "verbatim-snippet";
      }
    }
    // Last resort: use simple text
    else {
      char* text = SDL_GetClipboardText ();
      if (text) {
        s = string (text);
        SDL_free (text);
        input_format = "verbatim-snippet";
      }
    }
  }
  else if (format == "verbatim") {
    // For verbatim, always get plain text
    if (get_preference ("verbatim->texmacs:encoding") == "utf-8" ||
        get_preference ("verbatim->texmacs:encoding") == "auto") {
      char* text = SDL_GetClipboardText ();
      if (text) {
        s = string (text);
        SDL_free (text);
      }
    }
    else {
      // Try to get with specific encoding if needed
      data_ptr = SDL_GetClipboardData ("text/plain", &data_size);
      if (data_ptr) {
        s = string ((char*)data_ptr, data_size);
        SDL_free (data_ptr);
      }
    }
  }
  else {
    // For other formats, get plain text
    char* text = SDL_GetClipboardText ();
    if (text) {
      s = string (text);
      SDL_free (text);
    }
  }

  // If no data was retrieved, return false
  if (N(s) == 0) return false;

  // Apply buggy paste corrections if needed
  if (input_format == "html-snippet" && seems_buggy_html_paste (s))
    s = correct_buggy_html_paste (s);
  if (input_format != "picture" && seems_buggy_paste (s))
    s = correct_buggy_paste (s);

  // Convert to TeXmacs format if needed
  if (input_format != "" && input_format != "picture" && !direct_selection) {
    s = as_string (call ("convert", s, input_format, "texmacs-snippet"));
  }

  if (input_format == "picture") {
    tree im (IMAGE);
    int ww= 0, hh= 0;
    string w, h;
    if (png_size (s, ww, hh)) vue_pretty_image_size (ww, hh, w, h);
    im << tuple (tree (RAW_DATA, s), "png") << w << h << "" << "";
    s = as_string (call ("convert", im, "texmacs-tree", "texmacs-snippet"));
  }

  if (input_format == "html-snippet") {
    tree t_temp = as_tree (call ("convert", s, "texmacs-snippet", "texmacs-tree"));
    t_temp = default_with_simplify (t_temp);
    s = as_string (call ("convert", t_temp, "texmacs-tree", "texmacs-snippet"));
  }

  t = tuple ("extern", s);

  return true;
}

void clear_selection (string key) {
  // Clear the selection on clipboard 'cb'

  // Clear internal storage
  selection_t->reset (key);
  selection_s->reset (key);

  // SDL3 only supports system clipboard, not primary/mouse selections
  if (key != "primary") return;

  // Clear the SDL clipboard, if it holds what we put there: the contents
  // copied by another application stay (as in qt_gui.cpp)
  if (SDL_HasClipboardData ("application/x-texmacs-clipboard"))
    SDL_ClearClipboardData ();
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void beep () {
  // Issue a beep: the system alert sound on macOS, the console bell
  // elsewhere (SDL has no beep of its own)
#ifdef OS_MACOS
  mac_beep ();
#else
  cerr << "\a" << flush;
#endif
}

void needs_update () {
  // the editor asks for a frame: wake the loop and shorten its pause, as
  // an event would (clearing the flag here dropped the request)
  gui_needs_update= true;
}

bool check_event (int type) {
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
  bool status;
  switch (type) {
  case INTERRUPT_EVENT:
    if (interrupted) return true;
    else {
      int n=1; // n=XPending (dpy);
      time_t now= texmacs_time ();
      if (now - interrupt_time < 0) return false;
      else interrupt_time= now + (100 / (n + 1));
      // the queue only holds what the last pump fetched from the system,
      // which was before the repaint began: without a pump a key typed
      // during a long repaint never interrupted it (the checks are spaced
      // by interrupt_time; the resize watch does not run from here, see
      // watch_may_run)
      SDL_PumpEvents ();
      interrupted= (SDL_HasEvent (SDL_EVENT_KEY_DOWN) == true) ||
                   (SDL_HasEvent (SDL_EVENT_MOUSE_BUTTON_DOWN) == true);
      return interrupted;
    }
  case INTERRUPTED_EVENT:
    return interrupted;
  case ANY_EVENT:
    // SDL leaves a poll sentinel in the queue after each pump (it marks the
    // end of a poll): it is not an event of ours, and counting it made the
    // editor never idle (idle_time stayed 0: no delayed :idle commands, no
    // pre-edit of the input methods, which waits for 100 ms of idleness)
    return SDL_HasEvents (SDL_EVENT_FIRST, SDL_EVENT_POLL_SENTINEL - 1) ||
           SDL_HasEvents (SDL_EVENT_POLL_SENTINEL + 1, SDL_EVENT_USER - 1);
  case MOTION_EVENT:
    status= (SDL_HasEvent (SDL_EVENT_MOUSE_MOTION) == true);
    return status;
  case DRAG_EVENT:
    {
      status= false;
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_MOTION)) {
        if (event.motion.state) {
          status= true;
        }
      }
    }
    return status;
  case MENU_EVENT:
    status= (SDL_HasEvent (SDL_EVENT_MOUSE_BUTTON_UP) == true);
    if (!status) {
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_MOTION)) {
        status=  (event.motion.state != 0);
      }
    }
    return status;
  }
  return interrupted;
}

void image_gc (string name) {
  // Garbage collect images of a given name (may use wildcards): the
  // renderer caches the decoded images, the patterns and their images
  mupdf_image_gc (name);
}

// the balloon shown by show_help_balloon, and the wait indicator: both are
// popup windows of our own, dismissed from the main loop
static widget help_balloon_wid;
static widget wait_indicator_wid;
static list<string> wait_messages;

// called from the loop: the balloon goes away at the first key or motion
void
close_help_balloon () {
  if (is_nil (help_balloon_wid)) return;
  set_visibility (help_balloon_wid, false);
  destroy_window_widget (help_balloon_wid);
  help_balloon_wid= widget ();
}

void show_help_balloon (widget balloon, SI x, SI y) {
  // Display a help balloon at position (x, y), which is relative to the
  // window of the editor; it disappears as soon as the user presses a key
  // or moves the mouse (see process_event)
  close_help_balloon ();
  if (!has_current_window ()) return;
  SI wx= 0, wy= 0;
  get_position (get_window (concrete_window () -> win), wx, wy);
  help_balloon_wid= popup_window_widget (balloon, "Balloon");
  set_position (help_balloon_wid, x + wx, y + wy);
  set_visibility (help_balloon_wid, true);
}

static void
close_wait_window () {
  if (is_nil (wait_indicator_wid)) return;
  set_visibility (wait_indicator_wid, false);
  destroy_window_widget (wait_indicator_wid);
  wait_indicator_wid= widget ();
}

// A key or a click: the loop takes events again, so the operation which
// asked for the wait indicator is over, and the messages it did not take
// back go (the manuals push one per pass and never pop them; the last one,
// "Finishing manual", stayed on the screen)
void
dismiss_wait_indicator () {
  if (is_nil (wait_messages)) return;
  wait_messages= list<string> ();
  close_wait_window ();
}

void show_wait_indicator (widget base, string message, string argument) {
  // Display a wait indicator with a message and an optional argument, at
  // the centre of the window which triggered the lengthy operation; an
  // empty message pops the last one (the calls are nested). It is a panel
  // with the icon of TeXmacs, the outermost operation in bold and, when
  // operations are nested, the innermost one under it (as the Qt port
  // shows the first and the last message)
  (void) base;
  if (is_headless ()) return;
  if (N(message) > 0) {
    string msg= message;
    if (argument != "") msg= msg * " " * argument * "...";
    wait_messages= list<string> (msg, wait_messages);
  }
  else if (!is_nil (wait_messages)) wait_messages= wait_messages->next;

  close_wait_window ();
  if (is_nil (wait_messages) || !has_current_window ()) return;

  string outer= wait_messages->item, inner;
  for (list<string> l= wait_messages; !is_nil (l); l= l->next) outer= l->item;
  if (!is_nil (wait_messages->next)) inner= wait_messages->item;
  array<widget> lines;
  lines << text_widget (outer, WIDGET_STYLE_BOLD, black);
  if (N(inner) > 0)
    lines << glue_widget (false, false, 0, 3*PIXEL)
          << text_widget (inner, WIDGET_STYLE_GREY, black);
  else
    lines << glue_widget (false, false, 0, 3*PIXEL)
          << text_widget (translate ("Please wait"), WIDGET_STYLE_GREY, black);
  array<widget> row;
  row << xpm_widget (url_system ("$TEXMACS_PATH/misc/images/texmacs-vue-64.png"))
      << glue_widget (false, false, 14*PIXEL, 0)
      << vertical_list (lines);
  widget panel= division_widget ("wait-panel", horizontal_list (row));
  wait_indicator_wid= popup_window_widget (panel, "Wait");
  SI wx= 0, wy= 0, ww= 0, wh= 0;
  widget win= get_window (concrete_window () -> win);
  get_position (win, wx, wy);
  get_size (win, ww, wh);
  set_position (wait_indicator_wid, wx + ww/2, wy - wh/2);
  set_visibility (wait_indicator_wid, true);
  // the window must appear now: the operation which asked for it is about
  // to block the loop. It is centred once laid out, when its size is known
  process_layout ();
  SI pw= 0, ph= 0;
  get_size (wait_indicator_wid, pw, ph);
  if (pw > 0 && ph > 0) {
    set_position (wait_indicator_wid, wx + (ww - pw)/2, wy - (wh - ph)/2);
    process_layout ();
  }
  process_redraw ();
}

void external_event (string type, time_t t) {
  // External events, such as pushing a button of a remote infrared
  // commander: they reach the focused editor as a key
  if (current_window == NULL) return;
  vue_simple_widget_rep* ed=
    dynamic_cast<vue_simple_widget_rep*> (current_window->kbd_focus.rep);
  if (ed != NULL) ed->handle_keypress (type, t);
}

//*****************************************************************************
// chooser_widget platform dependent dialog code

// SDL may invoke the callback of a file dialog from another thread (it
// does with the portal and zenity backends), where neither Scheme nor the
// widget tree may be touched. The callback only copies the result and
// pushes an event; the main loop runs the command (process_event below).
// The result holds a reference to the chooser, so that a widget released
// while the panel is open stays alive until we are done with it.
struct vue_dialog_result {
  widget wid;
  string file;
  bool   chosen;
};

Uint32 vue_dialog_event= 0; // registered in gui_open

static void SDLCALL
file_dialog_callback (void* userdata, const char* const* filelist,
                     int filter_index)
{
  // No TeXmacs object is made or freed here (their allocator is not thread
  // safe): the name is copied with SDL's allocator, and the result is
  // filled and freed by vue_dialog_finish, on the main thread
  (void) filter_index;
  char* file= NULL;
  bool chosen= false;
  if (filelist == NULL)
    SDL_Log ("File dialog error: %s", SDL_GetError ());
  else if (*filelist != NULL) { // NULL: cancelled
    file= SDL_strdup (*filelist);
    chosen= (file != NULL);
  }
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= vue_dialog_event;
  ev.user.code= chosen ? 1 : 0;
  ev.user.data1= userdata;
  ev.user.data2= file;
  if (!SDL_PushEvent (&ev)) {
    // the result is lost (and leaks: it may not be freed here)
    SDL_Log ("cannot deliver the result of the file dialog: %s", SDL_GetError ());
    SDL_free (file);
  }
}

// called from the main loop when the event pushed above arrives
static void
vue_dialog_finish (vue_dialog_result* res, char* file, bool chosen) {
  if (res == NULL) { SDL_free (file); return; }
  res->chosen= chosen && file != NULL;
  if (res->chosen) res->file= string (file, (int) strlen (file));
  SDL_free (file);
  vue_chooser_widget_rep* w=
    dynamic_cast<vue_chooser_widget_rep*> (res->wid.rep);
  if (w != NULL) {
    if (res->chosen) {
      c_string name (res->file);
      w->callback ((char*) name);
    }
    else w->callback (NULL);
  }
  tm_delete (res);
}

#ifdef __EMSCRIPTEN__
/******************************************************************************
* The file dialogs of the browser
*
* SDL has none there. Open: the file input of the page; the file chosen is
* copied to /home/web/Uploads and its path handed over as SDL's callback
* would. Save: the name is asked for (a page cannot choose a place on the
* disk of the user); the file goes to /home/web/Documents, which is kept
* (see misc/wasm/web-pre.js), and is offered as a download once written:
* the command of the dialog writes it in a later frame, so the page waits
* until its size is stable. The input needs a recent gesture of the user,
* which the click on the menu item is.
******************************************************************************/

EM_JS_DEPS (vue_web_dialogs, "$withStackSave,$stringToUTF8OnStack,$UTF8ToString");

EM_JS (void, vue_web_open_dialog, (void* res, const char* accept), {
  function safe_name (n) {
    return n.split ('/').join ('_').split (String.fromCharCode (92)).join ('_');
  }
  var input = document.createElement ('input');
  input.type = 'file';
  var acc = UTF8ToString (accept);
  if (acc) input.accept = acc;
  var done = false;
  function finish (path) {
    if (done) return;
    done = true;
    withStackSave (function () {
      _vue_web_dialog_done (res, path ? stringToUTF8OnStack (path) : 0);
    });
  }
  input.addEventListener ('cancel', function () { finish (null); });
  input.onchange = function () {
    var f = input.files && input.files[0];
    if (!f) { finish (null); return; }
    f.arrayBuffer ().then (function (buf) {
      try { FS.mkdirTree ('/home/web/Uploads'); } catch (e) {}
      var path = '/home/web/Uploads/' + safe_name (f.name);
      FS.writeFile (path, new Uint8Array (buf));
      finish (path);
    });
  };
  input.click ();
});

EM_JS (void, vue_web_save_dialog, (void* res, const char* name), {
  function safe_name (n) {
    return n.split ('/').join ('_').split (String.fromCharCode (92)).join ('_');
  }
  var n = window.prompt ('Save as', UTF8ToString (name) || 'untitled.tm');
  if (!n) {
    withStackSave (function () { _vue_web_dialog_done (res, 0); });
    return;
  }
  n = safe_name (n);
  try { FS.mkdirTree ('/home/web/Documents'); } catch (e) {}
  var path = '/home/web/Documents/' + n;
  withStackSave (function () {
    _vue_web_dialog_done (res, stringToUTF8OnStack (path));
  });
  var last = -1, stable = 0, tries = 0;
  var timer = setInterval (function () {
    var size = -1;
    try { size = FS.stat (path).size; } catch (e) {}
    stable = (size >= 0 && size === last) ? stable + 1 : 0;
    last = size;
    if (stable < 2 && ++tries < 120) return;
    clearInterval (timer);
    if (size < 0) return;
    var a = document.createElement ('a');
    a.href = URL.createObjectURL (new Blob ([FS.readFile (path)]));
    a.download = n;
    document.body.appendChild (a);
    a.click ();
    a.remove ();
    setTimeout (function () { URL.revokeObjectURL (a.href); }, 10000);
  }, 500);
});

// the answer of a dialog of the page (path NULL: cancelled)
extern "C" EMSCRIPTEN_KEEPALIVE void
vue_web_dialog_done (void* res, const char* path) {
  const char* list[2]= { path, NULL };
  file_dialog_callback (res, list, 0);
}

// the suffixes of the files which may be chosen, for the file input
static string
web_accept (string file_type) {
  if (file_type == "image") return "image/*,.pdf,.eps,.ps,.svg";
  if (file_type == "directory" || file_type == "generic") return "";
  tree sufs= as_tree (call ("format-get-suffixes*", file_type));
  string acc;
  if (is_tuple (sufs))
    for (int i= 0; i < N(sufs); i++) {
      if (N(acc) > 0) acc << ",";
      acc << "." << as_string (sufs[i]);
    }
  return acc;
}
#endif

void
vue_chooser_widget_rep::perform_dialog (vue_window win) {
#ifdef __EMSCRIPTEN__
  (void) win;
  vue_dialog_result* wres= tm_new<vue_dialog_result> ();
  wres->wid= abstract (this);
  wres->chosen= false;
  if (file_type == "directory") vue_web_dialog_done ((void*) wres, NULL);
  else if (prompt != "") {
    c_string cname (file);
    vue_web_save_dialog ((void*) wres, cname);
  }
  else {
    c_string cacc (web_accept (file_type));
    vue_web_open_dialog ((void*) wres, cacc);
  }
  return;
#endif
 
  c_string caption (win_title);
  c_string tmp1 (directory);
  c_string tmp2 (file);

  string filter;
  // Define file filters
  static const SDL_DialogFileFilter all_filters[] = {
      { "PNG Images",  "png" },
      { "JPEG Images", "jpg;jpeg" },
      { "PDF Images", "pdf" },
      { "All Files",   "*" }
  };

  // The filters of the dialog. SDL keeps the pointer until its callback
  // runs, so what we build lives in static storage; one dialog is open at
  // a time. The strings are held by c_strings rather than freed at once.
  static SDL_DialogFileFilter type_filters[2];
  static c_string filter_name, filter_pattern;
  void *sdl_filters= NULL;
  int sdl_n_filters= 0;
  SDL_FileDialogType sdl_type;
  
  if (prompt != "")
    sdl_type= SDL_FILEDIALOG_SAVEFILE;
  else if (file_type == "directory")
    sdl_type= SDL_FILEDIALOG_OPENFOLDER;
  else
    sdl_type= SDL_FILEDIALOG_OPENFILE;

  if (file_type == "image") {
    sdl_n_filters= 4;
    sdl_filters= (void*) all_filters;
  } else if (file_type == "directory" || file_type == "generic") {
    sdl_n_filters= 0;
  } else {
    // the name of the format and the suffixes it is known by, e.g.
    // "TeXmacs document" and "tm;ts;tp", plus a catch-all
    filter= as_string (call ("format-get-name", file_type));
    tree sufs= as_tree (call ("format-get-suffixes*", file_type));
    string pat;
    if (is_tuple (sufs))
      for (int i= 0; i < N(sufs); i++) {
        if (N(pat) > 0) pat << ";";
        pat << as_string (sufs[i]);
      }
    if (N(pat) == 0) pat= "*";
    filter_name= c_string (filter);
    filter_pattern= c_string (pat);
    type_filters[0].name= filter_name;
    type_filters[0].pattern= filter_pattern;
    type_filters[1].name= "All Files";
    type_filters[1].pattern= "*";
    sdl_filters= (void*) type_filters;
    sdl_n_filters= 2;
  }
 
  // Create and set dialog properties
  SDL_PropertiesID props = SDL_CreateProperties();
  if (sdl_n_filters > 0) {
    SDL_SetPointerProperty(props, SDL_PROP_FILE_DIALOG_FILTERS_POINTER, sdl_filters);
    SDL_SetNumberProperty(props, SDL_PROP_FILE_DIALOG_NFILTERS_NUMBER, sdl_n_filters);
  }
  if (win) {
    SDL_SetPointerProperty(props, SDL_PROP_FILE_DIALOG_WINDOW_POINTER, win->platform_window ());
  }
  SDL_SetBooleanProperty(props, SDL_PROP_FILE_DIALOG_MANY_BOOLEAN, false); // single file
  SDL_SetStringProperty(props, SDL_PROP_FILE_DIALOG_TITLE_STRING, caption);
  SDL_SetStringProperty(props, SDL_PROP_FILE_DIALOG_ACCEPT_STRING, sdl_type == SDL_FILEDIALOG_SAVEFILE ? "Save" : "Open");
  SDL_SetStringProperty(props, SDL_PROP_FILE_DIALOG_CANCEL_STRING, "Cancel");
  // the folder (or file, for the save dialogs) the dialog starts at;
  // SDL_GetPrefPath was used here by mistake: it creates a preferences
  // folder named after its arguments under Application Support
  string location= directory;
  if (N(file) > 0 && sdl_type == SDL_FILEDIALOG_SAVEFILE)
    location= as_string (url_system (directory) * url_system (file));
  c_string tmp3 (location);
  if (N(location) > 0)
    SDL_SetStringProperty(props, SDL_PROP_FILE_DIALOG_LOCATION_STRING, tmp3);

  // Show the dialog (non-blocking); the result comes back through an event
  vue_dialog_result* res= tm_new<vue_dialog_result> ();
  res->wid= abstract (this);
  res->chosen= false;
  SDL_ShowFileDialogWithProperties (sdl_type,
                                    file_dialog_callback,
                                    (void*) res,
                                    props);
  SDL_DestroyProperties(props);
}


//*****************************************************************************
//*****************************************************************************
// Boring auxiliary functions

/******************************************************************************
* Set up keyboard
******************************************************************************/

void
map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= "S-" * s;
}

void
Map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= s;
}

void
MMap (int key, string s1, string s2) {
  lower_key (key)= s1;
  upper_key (key)= s2;
}

void
initialize_keyboard () {
  static bool initialized= false;
  if (initialized) return;
  initialized= true;
  
  // Latin characters
  MMap (SDLK_A, "a", "A");
  MMap (SDLK_B, "b", "B");
  MMap (SDLK_C, "c", "C");
  MMap (SDLK_D, "d", "D");
  MMap (SDLK_E, "e", "E");
  MMap (SDLK_F, "f", "F");
  MMap (SDLK_G, "g", "G");
  MMap (SDLK_H, "h", "H");
  MMap (SDLK_I, "i", "I");
  MMap (SDLK_J, "j", "J");
  MMap (SDLK_K, "k", "K");
  MMap (SDLK_L, "l", "L");
  MMap (SDLK_M, "m", "M");
  MMap (SDLK_N, "n", "N");
  MMap (SDLK_O, "o", "O");
  MMap (SDLK_P, "p", "P");
  MMap (SDLK_Q, "q", "Q");
  MMap (SDLK_R, "r", "R");
  MMap (SDLK_S, "s", "S");
  MMap (SDLK_T, "t", "T");
  MMap (SDLK_U, "u", "U");
  MMap (SDLK_V, "v", "V");
  MMap (SDLK_W, "w", "W");
  MMap (SDLK_X, "x", "X");
  MMap (SDLK_Y, "y", "Y");
  MMap (SDLK_Z, "z", "Z");

  Map (SDLK_0, "0");
  Map (SDLK_1, "1");
  Map (SDLK_2, "2");
  Map (SDLK_3, "3");
  Map (SDLK_4, "4");
  Map (SDLK_5, "5");
  Map (SDLK_6, "6");
  Map (SDLK_7, "7");
  Map (SDLK_8, "8");
  Map (SDLK_9, "9");

  // Standard ASCII Symbols
  Map (SDLK_EXCLAIM, "!");
  Map (SDLK_DBLAPOSTROPHE, "\x22");
  Map (SDLK_HASH, "#");
  Map (SDLK_DOLLAR, "$");
  Map (SDLK_PERCENT, "%");
  Map (SDLK_AMPERSAND, "&");
  Map (SDLK_APOSTROPHE, "'");
  Map (SDLK_LEFTPAREN, "(");
  Map (SDLK_RIGHTPAREN, ")");
  Map (SDLK_ASTERISK, "*");
  Map (SDLK_PLUS, "+");
  Map (SDLK_COMMA, ",");
  Map (SDLK_MINUS, "-");
  Map (SDLK_PERIOD, ".");
  Map (SDLK_SLASH, "/");
  Map (SDLK_COLON, ":");
  Map (SDLK_SEMICOLON, ";");
  Map (SDLK_LESS, "<");
  Map (SDLK_EQUALS, "=");
  Map (SDLK_GREATER, ">");
  Map (SDLK_QUESTION, "?");
  Map (SDLK_AT, "@");
  Map (SDLK_LEFTBRACKET, "[");
  Map (SDLK_BACKSLASH, "\\");
  Map (SDLK_RIGHTBRACKET, "]");
  Map (SDLK_CARET, "^");
  Map (SDLK_UNDERSCORE, "_");
  Map (SDLK_GRAVE, "`");
  // "{", "|" and "}" are typed with a modifier and come as text events;
  // mapping them here overwrote the "[", "]" entries of the same scancodes
  //Map (SDLK_TILDA, "~");

  // dead keys
  Map (0xFE50, "grave");
  Map (0xFE51, "acute");
  Map (0xFE52, "hat");
  Map (0xFE53, "tilde");
  Map (0xFE54, "macron");
  Map (0xFE55, "breve");
  Map (0xFE56, "abovedot");
  Map (0XFE57, "umlaut");
  Map (0xFE58, "abovering");
  Map (0xFE59, "doubleacute");
  Map (0xFE5A, "check");
  Map (0xFE5B, "cedilla");
  Map (0xFE5C, "ogonek");
  Map (0xFE5D, "iota");
  Map (0xFE5E, "voicedsound");
  Map (0xFE5F, "semivoicedsound");
  Map (0xFE60, "belowdot");
  
  // Special control keys
  Map (SDLK_PAGEUP, "pageup");
  Map (SDLK_PAGEDOWN, "pagedown");
  Map (SDLK_UNDO, "undo");
//  Map (SDLK_REDO, "redo");
  Map (SDLK_CANCEL, "cancel");

  // Control keys
  map (SDLK_SPACE, "space");
  map (SDLK_RETURN, "return");
  map (SDLK_BACKSPACE, "backspace");
  map (SDLK_DELETE, "delete");
  map (SDLK_INSERT, "insert");
  map (SDLK_TAB, "tab");
  // shift+tab: SDL folds the shift into the key, which is S-tab as in Qt
  Map (SDLK_LEFT_TAB, "S-tab");
  map (SDLK_ESCAPE, "escape");
  map (SDLK_LEFT, "left");
  map (SDLK_RIGHT, "right");
  map (SDLK_UP, "up");
  map (SDLK_DOWN, "down");
  map (SDLK_PAGEUP, "pageup");
  map (SDLK_PAGEDOWN, "pagedown");
  map (SDLK_HOME, "home");
  map (SDLK_END, "end");
  map (SDLK_F1, "F1");
  map (SDLK_F2, "F2");
  map (SDLK_F3, "F3");
  map (SDLK_F4, "F4");
  map (SDLK_F5, "F5");
  map (SDLK_F6, "F6");
  map (SDLK_F7, "F7");
  map (SDLK_F8, "F8");
  map (SDLK_F9, "F9");
  map (SDLK_F10, "F10");
  map (SDLK_F11, "F11");
  map (SDLK_F12, "F12");
  map (SDLK_F13, "F13");
  map (SDLK_F14, "F14");
  map (SDLK_F15, "F15");
  map (SDLK_F16, "F16");
  map (SDLK_F17, "F17");
  map (SDLK_F18, "F18");
  map (SDLK_F19, "F19");
  map (SDLK_F20, "F20");
  // map (SDLK_Mode_switch, "modeswitch");

  // Keypad keys
  Map (SDLK_KP_SPACE, "K-space");
  Map (SDLK_KP_ENTER, "K-return");
//  Map (SDLK_KP_DELETE, "K-delete");
//  Map (SDLK_KP_INSERT, "K-insert");
  Map (SDLK_KP_TAB, "K-tab");
//  Map (SDLK_KP_LEFT, "K-left");
//  Map (SDLK_KP_Right, "K-right");
//  Map (SDLK_KP_Up, "K-up");
//  Map (SDLK_KP_Down, "K-down");
//  Map (SDLK_KP_Page_Up, "K-pageup");
//  Map (SDLK_KP_Page_Down, "K-pagedown");
//  Map (SDLK_KP_Home, "K-home");
//  Map (SDLK_KP_Begin, "K-begin");
//  Map (SDLK_KP_End, "K-end");
//  Map (SDLK_KP_F1, "K-F1");
//  Map (SDLK_KP_F2, "K-F2");
//  Map (SDLK_KP_F3, "K-F3");
//  Map (SDLK_KP_F4, "K-F4");
  Map (SDLK_KP_EQUALS, "K-=");
  Map (SDLK_KP_MULTIPLY, "K-*");
  Map (SDLK_KP_PLUS, "K-+");
  Map (SDLK_KP_MINUS, "K--");
  Map (SDLK_KP_PERIOD, "K-.");
  Map (SDLK_KP_COMMA, "K-,");
  Map (SDLK_KP_DIVIDE, "K-/");
  Map (SDLK_KP_0, "K-0");
  Map (SDLK_KP_1, "K-1");
  Map (SDLK_KP_2, "K-2");
  Map (SDLK_KP_3, "K-3");
  Map (SDLK_KP_4, "K-4");
  Map (SDLK_KP_5, "K-5");
  Map (SDLK_KP_6, "K-6");
  Map (SDLK_KP_7, "K-7");
  Map (SDLK_KP_8, "K-8");
  Map (SDLK_KP_9, "K-9");

  // Miscellaneous
  Map (0x20ac, "euro");
}

/******************************************************************************
* SDL3 event logger
******************************************************************************/


// The name of a key for TeXmacs, from the text it types (in the Cork
// encoding of utf8_to_cork), as the Qt port names it (QTMKeyboardEvent.cpp):
// a symbol without its brackets, which the editor puts back ("<alpha>"
// gives "alpha"), and "<" and ">" as themselves. A key "<less>" was
// inserted as "<<less>>": a broken string in the document, whose cursor
// then went past its end ("bad path", a crash, after a Backspace).
static string
cork_key (string r) {
  int n= N(r);
  if (n >= 3 && r[0] == '<' && r[1] != '#' && r[n-1] == '>' &&
      search_forwards ("<", 1, r) < 0)
    r= r (1, n-1);
  if (r == "less") return "<";
  if (r == "gtr") return ">";
  return r;
}

// The key of the US layout at the place of a key, for a shortcut typed on
// a layout which is not Latin (Russian, Greek...): control+С is C-c, as in
// the Qt port and the native applications. SDL gives it with its keycode
// options (latin_letters, the default); the letters and the digits are
// taken from their place otherwise. SDLK_UNKNOWN if there is none.
static SDL_Keycode
latin_key (SDL_Scancode scancode) {
  SDL_Keycode k= SDL_GetKeyFromScancode (scancode, SDL_KMOD_NONE, true);
  if (k >= 0x20 && k < 0x7f) return k;
  if (scancode >= SDL_SCANCODE_A && scancode <= SDL_SCANCODE_Z)
    return (SDL_Keycode) ('a' + (scancode - SDL_SCANCODE_A));
  if (scancode >= SDL_SCANCODE_1 && scancode <= SDL_SCANCODE_9)
    return (SDL_Keycode) ('1' + (scancode - SDL_SCANCODE_1));
  if (scancode == SDL_SCANCODE_0) return SDLK_0;
  return SDLK_UNKNOWN;
}

static string
lookup_key (SDL_Scancode scancode, SDL_Keymod mod, bool* produces_text,
            string* echo) {
  SDL_Keymod orig_mod= mod;
  SDL_Keycode key= postprocess_key_event (scancode, &mod, false);
  if (key == SDLK_UNKNOWN) return ""; // it is only a modifier, we ignore it
  bool command= (mod & (SDL_KMOD_CTRL | SDL_KMOD_ALT | SDL_KMOD_GUI)) != 0;
  // shift+space is a key of its own (S-space), as in the Qt port; the text
  // event would have made it a plain space
  bool shift_space= (key == SDLK_SPACE && (mod & SDL_KMOD_SHIFT) != 0);
  // a character key without a command modifier types text: the system
  // sends the text (composed with the dead keys and the input method) in a
  // text event, which is delivered instead of the key
  if (produces_text != NULL)
    *produces_text= (key >= 0x20 && key != 0x7f && (key & SDLK_SCANCODE_MASK) == 0 &&
                     !command && !shift_space);
  // a shortcut on a key which is not Latin: its Latin equivalent with all
  // the modifiers (the character was returned without them, so that
  // control+С inserted a С)
  if (command && key >= 0x80 && (key & SDLK_SCANCODE_MASK) == 0) {
    SDL_Keycode latin= latin_key (scancode);
    if (latin != SDLK_UNKNOWN) {
      mod= orig_mod;
      key= latin;
      if (key >= 'a' && key <= 'z' && (mod & SDL_KMOD_SHIFT) != 0) {
        key= key - 'a' + 'A'; // C-A, as control+shift+a on a Latin layout
        mod&= ~SDL_KMOD_SHIFT;
      }
    }
  }

  if (DEBUG_VUE_EVENTS)
    debug_events << "postprocessed key: " << SDL_GetKeyName (key) << " " << print_modifiers (mod) << LF;

  const char* str= SDL_GetKeyName (key);
  string r (str, (int)strlen (str));
  r= cork_key (utf8_to_cork (r));
  // a character with no modifier left (the text event types it)
  if (contains_unicode_char (r) && !command) return r;
  string s=r;
  if ((key >= 'A') && (key <= 'Z')) s= upper_key[key - 'A' + 'a'];
  else if ((key >= 'a') && (key <= 'z')) s= lower_key[key];
  else if (lower_key->contains(key))  s= lower_key [key];
  if ((N(s)>=2) && (s[0]=='K') && (s[1]=='-')) s= s (2, N(s));
  // the text which SDL also sends for a key delivered as a key without a
  // command modifier: the digits and operators of the keypad, the space
  if (echo != NULL && !command) {
    if (s == "space") *echo= " ";
    else if (N(s) == 1) *echo= s;
  }

  if (mod & SDL_KMOD_SHIFT) s= "S-" * s;
  if (mod & SDL_KMOD_CTRL)  s= "C-" * s;
  if (mod & SDL_KMOD_ALT)   s= "A-" * s;
  if (mod & SDL_KMOD_GUI)   s= "M-" * s;
  if (DEBUG_VUE_EVENTS) debug_events << "key press: " << s << LF;
  return s;
}

// Print modifier info
static string
print_modifiers (SDL_Keymod mod) {
  string s;
  s << " Modifers: [" << as_string (mod) << " ";
  
  // If there are none then say so and return.
  if( mod == SDL_KMOD_NONE ){
    s << "None ]\n";
    return s;
  }
  
  // Check for the presence of each SDLMod value
  if( mod & SDL_KMOD_NUM )    s << "NUMLOCK ";
  if( mod & SDL_KMOD_CAPS )   s << "CAPSLOCK ";
  if( mod & SDL_KMOD_LCTRL )  s << "LCTRL ";
  if( mod & SDL_KMOD_RCTRL )  s << "RCTRL ";
  if( mod & SDL_KMOD_RSHIFT ) s << "RSHIFT ";
  if( mod & SDL_KMOD_LSHIFT ) s << "LSHIFT ";
  if( mod & SDL_KMOD_RALT )   s << "RALT ";
  if( mod & SDL_KMOD_LALT )   s << "LALT ";
  if( mod & SDL_KMOD_RGUI )   s << "RGUI ";
  if( mod & SDL_KMOD_LGUI )   s << "LGUI ";
  if( mod & SDL_KMOD_CTRL )   s << "CTRL ";
  if( mod & SDL_KMOD_SHIFT )  s << "SHIFT ";
  if( mod & SDL_KMOD_ALT )    s << "ALT ";
  if( mod & SDL_KMOD_GUI )    s << "GUI ";
  s << "]";
  return s;
}

// Print all information about a key event
static string
print_key_info ( SDL_KeyboardEvent *key ) {
  string s;
  // Is it a release or a press?
  s <<  (key->type == SDL_EVENT_KEY_UP ? "Release:- " : "Press:- ");
  // Print the hardware scancode first
  s << "Scancode: " << as_hexadecimal (key->scancode);
  // Print the name of the key
  s << ", Name: " << SDL_GetKeyName (key->key);
  // Print modifier info
  s << print_modifiers (key->mod);
  return s;
}



#define SDL_EVENT_TYPE_LIST \
  X(SDL_EVENT_FIRST) \
  X(SDL_EVENT_QUIT) \
  X(SDL_EVENT_TERMINATING) \
  X(SDL_EVENT_LOW_MEMORY) \
  X(SDL_EVENT_WILL_ENTER_BACKGROUND) \
  X(SDL_EVENT_DID_ENTER_BACKGROUND) \
  X(SDL_EVENT_WILL_ENTER_FOREGROUND) \
  X(SDL_EVENT_DID_ENTER_FOREGROUND) \
  X(SDL_EVENT_LOCALE_CHANGED) \
  X(SDL_EVENT_SYSTEM_THEME_CHANGED) \
  X(SDL_EVENT_DISPLAY_ORIENTATION) \
  X(SDL_EVENT_DISPLAY_ADDED) \
  X(SDL_EVENT_DISPLAY_REMOVED) \
  X(SDL_EVENT_DISPLAY_MOVED) \
  X(SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED) \
  X(SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED) \
  X(SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED) \
  X(SDL_EVENT_WINDOW_SHOWN) \
  X(SDL_EVENT_WINDOW_HIDDEN) \
  X(SDL_EVENT_WINDOW_EXPOSED) \
  X(SDL_EVENT_WINDOW_MOVED) \
  X(SDL_EVENT_WINDOW_RESIZED) \
  X(SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED) \
  X(SDL_EVENT_WINDOW_METAL_VIEW_RESIZED) \
  X(SDL_EVENT_WINDOW_MINIMIZED) \
  X(SDL_EVENT_WINDOW_MAXIMIZED) \
  X(SDL_EVENT_WINDOW_RESTORED) \
  X(SDL_EVENT_WINDOW_MOUSE_ENTER) \
  X(SDL_EVENT_WINDOW_MOUSE_LEAVE) \
  X(SDL_EVENT_WINDOW_FOCUS_GAINED) \
  X(SDL_EVENT_WINDOW_FOCUS_LOST) \
  X(SDL_EVENT_WINDOW_CLOSE_REQUESTED) \
  X(SDL_EVENT_WINDOW_HIT_TEST) \
  X(SDL_EVENT_WINDOW_ICCPROF_CHANGED) \
  X(SDL_EVENT_WINDOW_DISPLAY_CHANGED) \
  X(SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED) \
  X(SDL_EVENT_WINDOW_SAFE_AREA_CHANGED) \
  X(SDL_EVENT_WINDOW_OCCLUDED) \
  X(SDL_EVENT_WINDOW_ENTER_FULLSCREEN) \
  X(SDL_EVENT_WINDOW_LEAVE_FULLSCREEN) \
  X(SDL_EVENT_WINDOW_DESTROYED) \
  X(SDL_EVENT_WINDOW_HDR_STATE_CHANGED) \
  X(SDL_EVENT_KEY_DOWN) \
  X(SDL_EVENT_KEY_UP) \
  X(SDL_EVENT_TEXT_EDITING) \
  X(SDL_EVENT_TEXT_INPUT) \
  X(SDL_EVENT_KEYMAP_CHANGED) \
  X(SDL_EVENT_KEYBOARD_ADDED) \
  X(SDL_EVENT_KEYBOARD_REMOVED) \
  X(SDL_EVENT_TEXT_EDITING_CANDIDATES) \
  X(SDL_EVENT_MOUSE_MOTION) \
  X(SDL_EVENT_MOUSE_BUTTON_DOWN) \
  X(SDL_EVENT_MOUSE_BUTTON_UP) \
  X(SDL_EVENT_MOUSE_WHEEL) \
  X(SDL_EVENT_MOUSE_ADDED) \
  X(SDL_EVENT_MOUSE_REMOVED) \
  X(SDL_EVENT_JOYSTICK_AXIS_MOTION) \
  X(SDL_EVENT_JOYSTICK_BALL_MOTION) \
  X(SDL_EVENT_JOYSTICK_HAT_MOTION) \
  X(SDL_EVENT_JOYSTICK_BUTTON_DOWN) \
  X(SDL_EVENT_JOYSTICK_BUTTON_UP) \
  X(SDL_EVENT_JOYSTICK_ADDED) \
  X(SDL_EVENT_JOYSTICK_REMOVED) \
  X(SDL_EVENT_JOYSTICK_BATTERY_UPDATED) \
  X(SDL_EVENT_JOYSTICK_UPDATE_COMPLETE) \
  X(SDL_EVENT_GAMEPAD_AXIS_MOTION) \
  X(SDL_EVENT_GAMEPAD_BUTTON_DOWN) \
  X(SDL_EVENT_GAMEPAD_BUTTON_UP) \
  X(SDL_EVENT_GAMEPAD_ADDED) \
  X(SDL_EVENT_GAMEPAD_REMOVED) \
  X(SDL_EVENT_GAMEPAD_REMAPPED) \
  X(SDL_EVENT_GAMEPAD_TOUCHPAD_DOWN) \
  X(SDL_EVENT_GAMEPAD_TOUCHPAD_MOTION) \
  X(SDL_EVENT_GAMEPAD_TOUCHPAD_UP) \
  X(SDL_EVENT_GAMEPAD_SENSOR_UPDATE) \
  X(SDL_EVENT_GAMEPAD_UPDATE_COMPLETE) \
  X(SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED) \
  X(SDL_EVENT_FINGER_DOWN) \
  X(SDL_EVENT_FINGER_UP) \
  X(SDL_EVENT_FINGER_MOTION) \
  X(SDL_EVENT_FINGER_CANCELED) \
  X(SDL_EVENT_CLIPBOARD_UPDATE) \
  X(SDL_EVENT_DROP_FILE) \
  X(SDL_EVENT_DROP_TEXT) \
  X(SDL_EVENT_DROP_BEGIN) \
  X(SDL_EVENT_DROP_COMPLETE) \
  X(SDL_EVENT_DROP_POSITION) \
  X(SDL_EVENT_AUDIO_DEVICE_ADDED) \
  X(SDL_EVENT_AUDIO_DEVICE_REMOVED) \
  X(SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED) \
  X(SDL_EVENT_SENSOR_UPDATE) \
  X(SDL_EVENT_PEN_PROXIMITY_IN) \
  X(SDL_EVENT_PEN_PROXIMITY_OUT) \
  X(SDL_EVENT_PEN_DOWN) \
  X(SDL_EVENT_PEN_UP) \
  X(SDL_EVENT_PEN_BUTTON_DOWN) \
  X(SDL_EVENT_PEN_BUTTON_UP) \
  X(SDL_EVENT_PEN_MOTION) \
  X(SDL_EVENT_PEN_AXIS) \
  X(SDL_EVENT_CAMERA_DEVICE_ADDED) \
  X(SDL_EVENT_CAMERA_DEVICE_REMOVED) \
  X(SDL_EVENT_CAMERA_DEVICE_APPROVED) \
  X(SDL_EVENT_CAMERA_DEVICE_DENIED) \
  X(SDL_EVENT_RENDER_TARGETS_RESET) \
  X(SDL_EVENT_RENDER_DEVICE_RESET) \
  X(SDL_EVENT_RENDER_DEVICE_LOST) \
  X(SDL_EVENT_PRIVATE0) \
  X(SDL_EVENT_PRIVATE1) \
  X(SDL_EVENT_PRIVATE2) \
  X(SDL_EVENT_PRIVATE3) \
  X(SDL_EVENT_POLL_SENTINEL) \
  X(SDL_EVENT_USER) \
  X(SDL_EVENT_LAST) \
  X(SDL_EVENT_ENUM_PADDING)

const char*
SDL_EventTypeToString (Uint32 type) {
  switch (type) {
#define X(name) case name: return #name;
    SDL_EVENT_TYPE_LIST
#undef X
    default:
      return "SDL_EVENT_UNKNOWN";
  }
}

void
sdl_log_event (const SDL_Event *event) {
  if (!event) return;
  
  switch (event->type) {
      // Quit
    case SDL_EVENT_QUIT:
      SDL_Log ("Event: SDL_QUIT");
      break;
      
      // Keyboard
    case SDL_EVENT_KEY_DOWN:
    case SDL_EVENT_KEY_UP:
      SDL_Log ("Event: %s - Key: %s (Scancode: %d, Mod: 0x%x, Repeat: %d)",
               event->type == SDL_EVENT_KEY_DOWN ? "KEY_DOWN" : "KEY_UP",
               SDL_GetKeyName(event->key.key),
               event->key.scancode,
               event->key.mod,
               event->key.repeat);
      break;
      
      // Mouse motion
    case SDL_EVENT_MOUSE_MOTION:
      SDL_Log ("Event: MOUSE_MOTION - x: %f, y: %f, xrel: %f, yrel: %f",
               event->motion.x, event->motion.y,
               event->motion.xrel, event->motion.yrel);
      break;
      
      // Mouse buttons
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
      SDL_Log ("Event: %s - Button: %d, Clicks: %d, x: %f, y: %f",
               event->type == SDL_EVENT_MOUSE_BUTTON_DOWN ? "MOUSE_BUTTON_DOWN" : "MOUSE_BUTTON_UP",
               event->button.button, event->button.clicks,
               event->button.x, event->button.y);
      break;
      
      // Mouse wheel
    case SDL_EVENT_MOUSE_WHEEL:
      SDL_Log ("Event: MOUSE_WHEEL - x: %f, y: %f, direction: %d",
               event->wheel.x, event->wheel.y,
               event->wheel.direction);
      break;
      
      // Text input
    case SDL_EVENT_TEXT_INPUT:
      SDL_Log ("Event: TEXT_INPUT - Text: %s", event->text.text);
      break;
      
    case SDL_EVENT_TEXT_EDITING:
      SDL_Log ("Event: TEXT_EDITING - Text: %s, Start: %d, Length: %d",
               event->edit.text, event->edit.start, event->edit.length);
      break;
      
      // Window events
      
    case SDL_EVENT_WINDOW_SHOWN:
      SDL_Log ("Window %u shown", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_HIDDEN:
      SDL_Log ("Window %u hidden", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_EXPOSED:
      SDL_Log ("Window %u exposed", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_MOVED:
      SDL_Log ("Window %u moved to (%d, %d)",
               event->window.windowID,
               event->window.data1,
               event->window.data2);
      break;
      
    case SDL_EVENT_WINDOW_RESIZED:
      SDL_Log ("Window %u resized to %dx%d",
               event->window.windowID,
               event->window.data1,
               event->window.data2);
      break;
      
    case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
      SDL_Log ("Window %u pixel size changed to %dx%d",
               event->window.windowID,
               event->window.data1,
               event->window.data2);
      break;
      
    case SDL_EVENT_WINDOW_MINIMIZED:
      SDL_Log ("Window %u minimized", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_MAXIMIZED:
      SDL_Log ("Window %u maximized", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_RESTORED:
      SDL_Log ("Window %u restored", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_MOUSE_ENTER:
      SDL_Log ("Mouse entered window %u", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_MOUSE_LEAVE:
      SDL_Log ("Mouse left window %u", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_FOCUS_GAINED:
      SDL_Log ("Window %u gained keyboard focus", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_FOCUS_LOST:
      SDL_Log ("Window %u lost keyboard focus", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
      SDL_Log ("Window %u close requested", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_HIT_TEST:
      SDL_Log ("Window %u hit test event", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_ICCPROF_CHANGED:
      SDL_Log ("Window %u ICC profile changed", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_DISPLAY_CHANGED:
      SDL_Log ("Window %u moved to display %d", event->window.windowID, event->window.data1);
      break;
      
    case SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED:
      SDL_Log ("Window %u display scale changed to %d", event->window.windowID, event->window.data1);
      break;
      
    case SDL_EVENT_WINDOW_SAFE_AREA_CHANGED:
      SDL_Log ("Window %u safe area changed", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_OCCLUDED:
      SDL_Log ("Window %u occluded", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_ENTER_FULLSCREEN:
      SDL_Log ("Window %u entered fullscreen", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_LEAVE_FULLSCREEN:
      SDL_Log ("Window %u left fullscreen", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_DESTROYED:
      SDL_Log ("Window %u destroyed", event->window.windowID);
      break;
      
    case SDL_EVENT_WINDOW_HDR_STATE_CHANGED:
      SDL_Log ("Window %u HDR state changed", event->window.windowID);
      break;
      
      // Game controller (SDL_Gamepad)
    case SDL_EVENT_GAMEPAD_ADDED:
      SDL_Log ("Event: GAMEPAD_ADDED - Device Index: %d", event->gdevice.which);
      break;
      
    case SDL_EVENT_GAMEPAD_REMOVED:
      SDL_Log ("Event: GAMEPAD_REMOVED - Instance ID: %d", event->gdevice.which);
      break;
      
    case SDL_EVENT_GAMEPAD_BUTTON_DOWN:
    case SDL_EVENT_GAMEPAD_BUTTON_UP:
      SDL_Log ("Event: %s - Button: %d, Instance ID: %d",
               event->type == SDL_EVENT_GAMEPAD_BUTTON_DOWN ? "GAMEPAD_BUTTON_DOWN" : "GAMEPAD_BUTTON_UP",
               event->gbutton.button, event->gbutton.which);
      break;
      
    case SDL_EVENT_GAMEPAD_AXIS_MOTION:
      SDL_Log ("Event: GAMEPAD_AXIS_MOTION - Axis: %d, Value: %d, Instance ID: %d",
               event->gaxis.axis, event->gaxis.value, event->gaxis.which);
      break;
      
      // Touch input
    case SDL_EVENT_FINGER_DOWN:
    case SDL_EVENT_FINGER_UP:
    case SDL_EVENT_FINGER_MOTION:
      SDL_Log ("Event: %s - FingerID: %" SDL_PRIs64 ", x: %f, y: %f, dx: %f, dy: %f, pressure: %f",
               event->type == SDL_EVENT_FINGER_DOWN ? "FINGER_DOWN" :
               event->type == SDL_EVENT_FINGER_UP   ? "FINGER_UP"   : "FINGER_MOTION",
               event->tfinger.fingerID,
               event->tfinger.x, event->tfinger.y,
               event->tfinger.dx, event->tfinger.dy,
               event->tfinger.pressure);
      break;
      
    default:
      SDL_Log ("Event: %s", SDL_EventTypeToString (event->type));
      break;
  }
}

