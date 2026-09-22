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
#endif
#include "sys_utils.hpp"     // get_env
#include "file.hpp"          // load_string (scripted events)
#include "socket_notifier.hpp" // notifiers_active (pause of the loop)

#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>

#if MUPDF_RENDERER
#include "../MuPDF/mupdf_picture.hpp"
#include "../MuPDF/mupdf_renderer.hpp" // mupdf_image_gc
#else
#include "../MuPDF/fitz_picture.hpp"
#endif

#include "clay.h"
extern "C" bool vue_clay_transitions_active (void); // clay.c



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

class vue_sdl_base_window_rep : public vue_window_rep {
public:
  SDL_Window *sdl_win;
  SI Min_w, Min_h, Max_w, Max_h; // size limits, 0 if unset
  
  vue_sdl_base_window_rep (vue_widget w, string name, bool popup= false);
  ~vue_sdl_base_window_rep ();
  
  void *platform_window () { return (void*)sdl_win; }

  void   destroy_event ();
  void   set_name (string name);
  string get_name ();
  void   set_modified (bool flag);
  void   set_visibility (bool flag);
  void   set_size (SI w, SI h);
  void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h);
  void   update_density (); // the pixel density of its display (override)
  void   get_size (SI& w, SI& h);
  void   get_size_limits (SI& min_w, SI& min_h, SI& max_w, SI& max_h);
  void   set_position (SI x, SI y);
  void   get_position (SI& x, SI& y);
  
  void process_layout ();
};

int vue_window_rep::serial= 1; // serial identifier for windows

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

// Clay reports its problems through this handler and goes on (the offending
// element is skipped): they are logged, once per kind to avoid flooding the
// log during an animation. Capacity errors would need bigger arenas (see
// Clay_SetMaxElementCount before Clay_Initialize below).
void HandleClayErrors (Clay_ErrorData errorData) {
  static int reported[16];
  int kind= (int) errorData.errorType;
  if (kind < 0 || kind >= 16) kind= 15;
  if (reported[kind]++ > 0) return;
  cout << "TeXmacs] Clay error (" << kind << "): "
       << string (errorData.errorText.chars, errorData.errorText.length) << LF;
}

static TTF_Font **ttf_fonts= NULL; // fonts cache

vue_sdl_base_window_rep::vue_sdl_base_window_rep (vue_widget _content, string _name, bool _popup)
: vue_window_rep (_content, _name, _popup), Min_w (0), Min_h (0), Max_w (0), Max_h (0)
{
  if (DEBUG_VUE) debug_widgets << "create vue_sdl_base_window_rep " << id << (popup ? " (popup)" : "") << LF;
  // windows start hidden and are shown once laid out, see set_visibility
  SDL_WindowFlags flags= SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_RESIZABLE |
                         SDL_WINDOW_HIDDEN;
  if (popup)
    // popups and tooltips are undecorated, start hidden and stay on top;
    // they are shown via SLOT_VISIBILITY once positioned
    flags= SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_BORDERLESS |
           SDL_WINDOW_ALWAYS_ON_TOP | SDL_WINDOW_HIDDEN | SDL_WINDOW_NOT_FOCUSABLE;
  int win_w= 200, win_h= 200;
  int win_x=30, win_y= 30;
  c_string buf (name);
  
  sdl_win= SDL_CreateWindow (buf, win_w, win_h, flags);
  if (!sdl_win) {
    // nothing sensible can be done without a window
    SDL_LogError (SDL_LOG_CATEGORY_APPLICATION, "Couldn't create window: %s", SDL_GetError ());
    FAILED ("Vue: cannot create a window");
  }
  
  nr_windows++;
  last_created_window= this;
  SDL_SetWindowPosition (sdl_win, win_x, win_y);
  Window_to_window (sdl_win)= (void*) this;
  id= serial++;
  id_to_window (id)= this;
  
  SDL_StartTextInput (sdl_win);
  
  // update widget state
  set_identifier (abstract (content), id);
  notify_position (abstract (content), 0, 0);
  notify_size (abstract (content), win_w,  win_h);
  
  {
    // initialize clay context
    // note: we need to preserve previous context in case it was present
    // we may be in the middle of some layout operation for another window
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
    clay_arena= (Clay_Arena) {
        .memory=  (char*) SDL_malloc (totalMemorySize),
        .capacity= totalMemorySize
    };
    if (clay_arena.memory == NULL) FAILED ("Vue: cannot allocate the layout arena");
    clay_ctx= Clay_Initialize (clay_arena, (Clay_Dimensions) { (float) win_w, (float) win_h }, (Clay_ErrorHandler) { HandleClayErrors });
    Clay_SetCurrentContext (save_ctx);
  }
  
  clay_debug= false;
  last_layout_time= 0;
  transitions_active= false;
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
  Window_to_window->reset (sdl_win);
  nr_windows--;

  SDL_StopTextInput (sdl_win);

  SDL_free (clay_arena.memory);
  SDL_DestroyWindow (sdl_win);
}

void
vue_sdl_base_window_rep::destroy_event () {
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

void
vue_sdl_base_window_rep::set_position (SI x, SI y) {
  SI screen_w, screen_h;
  gui_root_extents (screen_w, screen_h);
  screen_w /= PIXEL; screen_h /= PIXEL;
  
  int win_w, win_h;
  SDL_GetWindowSize (sdl_win, &win_w, &win_h);

  x= x/PIXEL;
  y= -y/PIXEL;
  if ((x+ win_w) > screen_w) x= screen_w- win_w;
  if (x<0) x=0;
  if ((y+ win_h) > screen_h) y= screen_h- win_h;
  if (y<0) y=0;
  SI win_x= x, win_y= y;
  if (DEBUG_VUE_EVENTS) SDL_Log ("Window %d set_position %d %d", id, (int) win_x, (int) win_y);
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

void
vue_sdl_base_window_rep::set_name (string name) {
  if (the_name != name) {
    c_string s (name);
    SDL_SetWindowTitle (sdl_win, s);
    the_name= name;
    mod_name= name;
  }
}

string
vue_sdl_base_window_rep::get_name () {
  return the_name;
}

void
vue_sdl_base_window_rep::set_modified (bool flag) {
  string name= (flag? (the_name * " *"): the_name);
  if (mod_name != name) {
    c_string s (name);
    SDL_SetWindowTitle (sdl_win, s);
    mod_name= name;
  }
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
  // otherwise the window is shown by process_layout once it fits its contents
}
 
void
vue_sdl_base_window_rep::process_layout () {
  bool relayout= false;
  int passes= 0;
  do {
    with_window frame (this);
    layout_again= false;
    // init the current GUI context
    int win_x, win_y, win_w, win_h;
    SDL_GetWindowSizeInPixels (sdl_win, &win_w, &win_h);
    SDL_GetWindowPosition (sdl_win, &win_x, &win_y);

//    Clay_SetCurrentContext (clay_ctx);
    Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) win_w, (float) win_h });
    layout_w= win_w; layout_h= win_h;
    gui_init_context ();

    // layout the top widget
    Clay_SetDebugModeEnabled (clay_debug);
    Clay__debugViewWidth= 600; // redefine to have more space
    Clay_BeginLayout ();
    content->do_layout ();
    // the frame time drives the transitions declared by the elements
    // (.transition, see the notes on animation in vue-graphics-stack.md)
    time_t now= texmacs_time ();
    float dt= (last_layout_time == 0) ? 0.0f : (float) (now - last_layout_time) / 1000.0f;
    last_layout_time= now;
    render_commands= Clay_EndLayout (min (dt, 0.1f));
    transitions_active= vue_clay_transitions_active (); // clay.c
    gui_finalize_context ();

    // post layout tweaking
    relayout= content->post_layout () || layout_again;
  } while (relayout && ++passes < 5);

  // show the window once it fits its contents (or after a few passes, in
  // case the contents never settle)
  layout_passes++;
  if (visible_requested && !shown && (ready_to_show || layout_passes > 10)) {
    SDL_ShowWindow (sdl_win);
    shown= true;
  }
}

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

void
sdl_draw_picture (SDL_Renderer *sdl_ren, picture pic, SDL_FRect *dest) {
  // propagate immediately the changes to the screen
#if MUPDF_RENDERER
  fz_pixmap *pix=  ((mupdf_picture_rep*)pic->get_handle())->pix;
  fz_context *ctx= mupdf_context ();
#else
  fz_pixmap *pix=  ((fitz_picture_rep*)pic->get_handle())->pix;
  fz_context *ctx= get_fitz_context ();
#endif

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

//******************************************************************************
// rendering via MuPDF renderer

array<styled_string> styled_strings;

class vue_sdl_mupdf_window_rep : public vue_sdl_base_window_rep {
public:
  renderer ren;
  picture backing_store;

  vue_sdl_mupdf_window_rep (vue_widget w, string name, bool popup= false);
  ~vue_sdl_mupdf_window_rep () { delete_renderer (ren); }
  
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

vue_sdl_mupdf_window_rep::vue_sdl_mupdf_window_rep (vue_widget w, string name, bool popup)
  : vue_sdl_base_window_rep (w, name, popup), ren (NULL)
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
#if MUPDF_RENDERER
  fz_pixmap *pix= ((mupdf_picture_rep*)pic->get_handle())->pix;
  fz_context *ctx= mupdf_context ();
#else
  fz_pixmap *pix= ((fitz_picture_rep*)pic->get_handle())->pix;
  fz_context *ctx= get_fitz_context ();
#endif
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
#if MUPDF_RENDERER
  fz_context *ctx= mupdf_context ();
#else
  fz_context *ctx= get_fitz_context ();
#endif
  fz_pixmap *pix= NULL;
#if MUPDF_RENDERER
  // the window surface is wrapped, not copied; a 1x1 pixmap replaces it if
  // MuPDF refuses (nothing is then drawn in this frame)
  // SDL only promises the format which suits the window best: check that
  // it is four bytes per pixel and use its own pitch (the rows may be
  // padded, which sheared the image when 4*w was assumed)
  bool ok= (surf != NULL) && SDL_BYTESPERPIXEL (surf->format) == 4 &&
           mupdf_protected ("window surface", [&] () {
    pix= fz_new_pixmap_with_data (ctx, fz_device_bgr (ctx),
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
#else
  pix= fz_new_pixmap_with_data (ctx,
                      fz_device_bgr (ctx),
                      surf->w, surf->h, NULL, 1, 4*surf->w,
                      (unsigned char*)surf->pixels);
#endif
#if MUPDF_RENDERER
  picture p= mupdf_picture (pix, 0, 0);
#else
  picture p= fitz_picture (pix, 0, 0);
#endif
  fz_drop_pixmap (ctx, pix);
  return p;
}

void
vue_sdl_mupdf_window_rep::process_redraw () {
  with_window frame (this);
  int win_w, win_h;

  SDL_Surface *surf= SDL_GetWindowSurface(sdl_win);
  if (surf == NULL) {
    // e.g. a window being destroyed or minimized: nothing to draw on
    SDL_Log ("SDL_GetWindowSurface failed: %s", SDL_GetError ());
    return;
  }
  backing_store= native_picture_from_SDL_Surface (surf);
#if MUPDF_RENDERER
  fz_pixmap *pix= ((mupdf_picture_rep*)backing_store->get_handle())->pix;
  fz_context *ctx= mupdf_context ();
#else
  fz_pixmap *pix= ((fitz_picture_rep*)backing_store->get_handle())->pix;
  fz_context *ctx= get_fitz_context ();
#endif

  if (!ren) {
    ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
  } else {
#if MUPDF_RENDERER
    static_cast<mupdf_renderer_rep*>(ren)->begin (pix);
#else
    static_cast<fitz_renderer_rep*>(ren)->begin (pix);
#endif
  }
  
  win_w = surf->w;
  win_h = surf->h;
    
  time_t t1, t2;
  t2= texmacs_time ();
  // areas not covered by any element: red in the debug mode (F1) to spot them
  ren->set_pencil (clay_debug ? rgb_color (255, 0, 0) : rgb_color (192, 192, 192));
  ren->fill (0, -win_h * ren->pixel, win_w * ren->pixel, 0);
  render_clay_commands (ren, &render_commands);

#if MUPDF_RENDERER
    static_cast<mupdf_renderer_rep*>(ren)->end ();
#else
    static_cast<fitz_renderer_rep*>(ren)->end ();
#endif

  t1= t2; t2= texmacs_time ();
  if (DEBUG_VUE && t2 - t1 > 30)
    debug_widgets << "render_clay_commands took " << t2 - t1 << "ms" << LF;
  
  // development aid: when TEXMACS_VUE_SNAPSHOT is set to a directory, the
  // rendering of every window is saved there as window-<id>.png at each redraw
  static string snapshot_dir= get_env ("TEXMACS_VUE_SNAPSHOT");
  if (N(snapshot_dir) > 0) {
    save_pixmap_as_png (ctx, pix, snapshot_dir * "/window-" * as_string (id) * ".png");
    if (snapshot_win == this && N(snapshot_name) > 0) {
      // named snapshot requested by a script
      save_pixmap_as_png (ctx, pix, snapshot_dir * "/" * snapshot_name * ".png");
      snapshot_name= "";
    }
  }

  //SDL_SetRenderDrawColor (sdl_ren, 0, 0, 0, 255);
  //SDL_RenderClear (sdl_ren);
  if (!SDL_UpdateWindowSurface (sdl_win)) {
    static int reported= 0;
    if (reported++ < 3) SDL_Log ("SDL_UpdateWindowSurface failed: %s", SDL_GetError ());
  }
  t1= t2; t2= texmacs_time ();
  if (DEBUG_VUE && t2 - t1 > 30)
    debug_widgets << "SDL_UpdateWindowSurface took " << t2 - t1 << "ms" << LF;
}


// see vue_gui.hpp for vue_render_ren_data

void
vue_render_widget_fn (renderer ren, void *w, rectangle r) {
  vue_render_ren_data data { .ren= ren, .r= r };
  ((vue_widget_rep*)w)->render (&data);
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
        clip_depth++;
        ren->clip (rcmd->boundingBox.x * ren->pixel,
                   -(rcmd->boundingBox.y + rcmd->boundingBox.height) * ren->pixel,
                   (rcmd->boundingBox.x + rcmd->boundingBox.width) * ren->pixel,
                   -rcmd->boundingBox.y * ren->pixel);
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
  }
}

void
vue_render_text_fn (renderer ren, void *w, rectangle r) {
  styled_string ss= (styled_string_rep *)w;
  ren->set_pencil (ss->c);
  ren->set_shrinking_factor (3);
  ss->fn->var_draw (ren, ss->s, r->x1*3, r->y1*3- ss->fn->y1);
  ren->set_shrinking_factor (1);
}

void *vue_render_text= (void*)&vue_render_text_fn;

static void
layout_text_box (string s, int style, color c) {
  font fn= get_default_styled_font (style);
  // the extents are measured once per (font, string): a layout pass runs
  // several times per frame and a menu bar holds many unchanging labels
  static hashmap<string,int> extent_cache (-1);
  static array<SI> extent_w, extent_h;
  static string cache_font;
  if (cache_font != fn->res_name) {
    cache_font= fn->res_name;
    extent_cache= hashmap<string,int> (-1);
    extent_w= array<SI> (); extent_h= array<SI> ();
  }
  SI w, h;
  int idx= extent_cache[s];
  if (idx >= 0) { w= extent_w[idx]; h= extent_h[idx]; }
  else {
    metric ex;
    fn->var_get_extents (s, ex);
    w= ((ex->x2- ex->x1+ 2)/3);
    h= ((fn->y2- fn->y1+ 2)/3);
    abs_round (w, h);
    if (N(extent_w) < 4096) { // bounded: the texts of a UI are few
      extent_cache (s)= N(extent_w);
      extent_w << w; extent_h << h;
    }
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
// entrypoints for top-level windows

vue_window
plain_window (vue_widget wwid, string name, bool popup) {
 return tm_new<vue_sdl_mupdf_window_rep> (wwid, name, popup);
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
  
  // trackpads: macOS itself generates the momentum events of a gesture
  // (SDL drops them by default), see the kinetic scrolling notes below
  SDL_SetHint (SDL_HINT_MAC_SCROLL_MOMENTUM, "1");
  if (!SDL_Init (SDL_INIT_VIDEO)) { // no audio backend is needed
    SDL_Log ("Unable to initialize SDL: %s", SDL_GetError ());
    exit (-1);
  }

  if (!TTF_Init()) {
    exit (-1);
  }

  SDL_SetHint (SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, "1");
  vue_dialog_event= SDL_RegisterEvents (1); // results of the file dialogs
  
  // The layout works in device pixels (SDL_GetWindowSizeInPixels) while
  // the pointer comes in points: the factor between them is the pixel
  // density of the display. It was hardcoded to 2, so on a display without
  // HiDPI every pointer position was doubled and nothing could be hit.
  // TeXmacs keeps one global factor, so a mixed-density setup follows the
  // primary display.
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
  SDL_Quit();
}

void gui_root_extents (SI& width, SI& height)
{
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

hashmap<int,string> lower_key;
hashmap<int,string> upper_key;

/////////

void gui_interpose (void (*f) (void)) {
  // specify an interpose routine for the main loop
  the_interpose_handler= f;
}

int number_of_servers (); // in texmacs_server.hpp

void sdl_log_event (const SDL_Event *event);
static string lookup_key (SDL_Scancode scancode, SDL_Keymod mod, bool* produces_text= NULL);
static string print_modifiers (SDL_Keymod mod);
static string print_key_info ( SDL_KeyboardEvent *key );

void process_event (SDL_Event *event);
void close_help_balloon ();

// The payloads of the drops, read back by call_drop_event (edit_mouse.cpp)
// through the ticket carried by the "drop" mouse action.
hashmap<int, tree> payloads;
static int  drop_serial= 0;
static tree drop_doc (CONCAT);

// the width and height of a dropped image as a pretty TeXmacs length
// (the policy of qt_pretty_image_size: a wide image fills the line)
static void
vue_pretty_image_size (url image, string& w, string& h) {
  w= ""; h= "";
  string ext= locase_all (suffix (image));
  if (ext == "pdf" || ext == "ps" || ext == "eps") return; // sized by the box
  picture pic= load_picture (image, -1, -1, tree (""), PIXEL);
  if (is_nil (pic)) return;
  int ww= pic->get_width (), hh= pic->get_height ();
  SI pt= get_current_editor () -> as_length ("1pt");
  SI par= get_current_editor () -> as_length ("1par");
  if (ww <= 0 || hh <= 0 || ww * pt > par) { w= "1par"; h= ""; }
  else { w= as_string (ww) * "pt"; h= as_string (hh) * "pt"; }
}
struct vue_dialog_result;
static void vue_dialog_finish (vue_dialog_result* res);
extern Uint32 vue_dialog_event;
void process_messages ();
void process_layout ();
void process_redraw ();

bool gui_wait=  false;

/******************************************************************************
* Kinetic scrolling
*
* Wheel events scroll at once, so that a wheel which is turned slowly moves
* the view in sync. Meanwhile the speed of the wheel is estimated from the
* events; when they stop while the wheel was "launched" (speed above
* wheel_launch_speed), the view goes on with that velocity, decaying
* exponentially with time constant wheel_tau, through synthetic wheel deltas.
*
* Trackpads: SDL reports the gestures as wheel events with fractional
* ("precise") deltas and no phase, so we cannot tell fingers which pause
* from fingers which are lifted. On macOS the system computes the momentum
* itself and, with SDL_HINT_MAC_SCROLL_MOMENTUM, sends it as a stream of
* wheel events after the fingers are lifted: the view follows the fingers
* exactly while they are down and the system glide after; precise streams
* start no glide of ours there. Elsewhere the wheel model applies to them.
******************************************************************************/

static const double wheel_tau= 350.0;          // ms
static const double wheel_launch_speed= 1.0;   // device pixels per ms
static const time_t wheel_stream_dt= 30;       // ms: the events have stopped
static const time_t wheel_slow_dt= 200;        // ms: the wheel is turned slowly
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
  // Clay scrolls its containers by ten pixels per unit of delta
  Clay_UpdateScrollContainers (true, (Clay_Vector2) { (float) dx / 10, (float) dy / 10 }, 0.01f);
}

// a wheel event: scroll now and update the estimated speed of the wheel
// (x, y are the deltas as reported by SDL, in wheel units)
static void
wheel_event (vue_window win, double x, double y, time_t now) {
  vue_input_state& in= win->input;
  in.wheel_vx= in.wheel_vy= 0; // the user took over from a glide
  time_t dt= (in.wheel_event_time == 0) ? wheel_slow_dt : now - in.wheel_event_time;
  if (dt >= wheel_slow_dt) in.wheel_precise= false; // a new stream of events
  if (x != floor (x) || y != floor (y)) in.wheel_precise= true;
  double step= win->density * (in.wheel_precise ? wheel_precise_step : wheel_notch_step);
  double dx= x * step, dy= y * step; // device pixels
  dt= max ((time_t) 8, min (dt, wheel_slow_dt));
  in.wheel_est_x= 0.5 * (in.wheel_est_x + dx / dt);
  in.wheel_est_y= 0.5 * (in.wheel_est_y + dy / dt);
  in.wheel_event_time= now;
  push_wheel (win, dx, dy);
}

// advance the kinetic scrolling of all windows; returns true if the loop
// must come back soon (a view glides or a glide may start)
static bool
wheel_inertia_step () {
  bool busy= false;
  time_t now= texmacs_time ();
  iterator<int> it= iterate (id_to_window);
  while (it->busy ()) {
    vue_window win= (vue_window) id_to_window[it->next ()];
    if (win == NULL) continue;
    vue_input_state& in= win->input;
    if (in.wheel_vx == 0 && in.wheel_vy == 0) {
      // no glide: did the events just stop with a launched wheel?
      if (in.wheel_est_x == 0 && in.wheel_est_y == 0) continue;
      busy= true;
      if (now - in.wheel_event_time < wheel_stream_dt) continue;
      if (hypot (in.wheel_est_x, in.wheel_est_y) >= wheel_launch_speed &&
          !(in.wheel_precise && wheel_system_momentum)) {
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
    if (fabs (in.wheel_vx) < 1e-4) in.wheel_vx= 0;
    if (fabs (in.wheel_vy) < 1e-4) in.wheel_vy= 0;
    if (dx != 0 || dy != 0) push_wheel (win, dx, dy);
  }
  return busy;
}
// does a window have a Clay transition in progress? (see process_layout)
static bool
transitions_running () {
  iterator<int> it= iterate (id_to_window);
  while (it->busy ()) {
    vue_window win= (vue_window) id_to_window[it->next ()];
    if (win != NULL && win->transitions_active) return true;
  }
  return false;
}

bool gui_needs_update= true;

bool event_filter (void *userdata, SDL_Event *event);

void gui_start_loop () {
  // start the main loop
  int  delay= 10;
  request_partial_redraw= true;
  time_t t1, t2;

  // FIXME: Don't typeset when resizing window
  
  SDL_AddEventWatch (&event_filter, NULL);
  script_init ();

  while (nr_windows > 0 || number_of_servers () > 0) {
    
    // 1. process events
    script_step (); // may push synthetic events
    SDL_Event event;
    if (SDL_PollEvent (&event)) {
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
             SDL_PollEvent (&event)) {
        process_event (&event);
      }
    }
    if (transitions_running ()) {
      // a transition animates: keep the frames coming (paced, woken by events)
      gui_needs_update= true;
      if (!SDL_PollEvent (NULL)) SDL_WaitEventTimeout (NULL, 8);
    }
    if (wheel_inertia_step ()) {
      // keep the frames coming while the view glides (or while a stream of
      // wheel events is being watched for a launch), paced at 5 ms but
      // woken up by any event: a plain sleep here added its length to the
      // latency of every wheel event
      gui_needs_update= true;
      if (!SDL_PollEvent (NULL)) SDL_WaitEventTimeout (NULL, 5);
    }

    if (gui_needs_update) {
      delay= 10;
      gui_wait= false;
      gui_needs_update= false;
    }
        
    // 2. wait for events on all channels
    if (gui_wait) {
      // sleep until an event arrives, or at most 'delay' (the interpose
      // handler and the delayed Scheme commands need periodic calls; the
      // pause grows while nothing happens). A plain SDL_Delay here made the
      // first event after a pause wait for the end of the pause: up to 1 s
      // before a scroll started to move
      // sockets and pipes (plugins, the TeXmacs client/server) are polled
      // by the interpose handler (perform_select): keep the pause short
      // while any is open, they have no event of their own to wake us
      int pause= notifiers_active () ? min (delay, 40) : delay;
      SDL_WaitEventTimeout (NULL, pause);
      delay += (delay/5);
      if (delay > 1000) delay= 1000;
    }

    // 3. process layout and handle events
    {
      t2= texmacs_time ();
      process_layout ();
      t1= t2; t2= texmacs_time ();
      if (DEBUG_VUE && t2 - t1 >= 30) debug_widgets << "layout took " << t2 - t1 << "ms" << LF;
    }
    
    // 4. exec commands if present
    if (!is_nil (cmd_list)) {
      list<command> l= reverse(cmd_list);
      cmd_list= list<command>();
      while (!is_nil(l)) {
        if (DEBUG_VUE_WIDGETS) debug_widgets << "run command " << l->item << LF;
        l->item->apply();
        l= l->next;
      }
    }
    
    // 5. interpose
    t2= texmacs_time ();
    vue_simple_widget_rep::notify_resizes ();
    if (the_interpose_handler != NULL) the_interpose_handler ();
    if (nr_windows == 0) continue;
    t1= t2; t2= texmacs_time ();
    if (DEBUG_VUE && t2 - t1 >= 30) debug_widgets << "interpose took " << t2-t1 << "ms" << LF;

    if (nr_windows == 0) continue;

    // the commands and the interpose handler may have replaced widgets
    // (menus, tools, dialogs): the render commands of the last layout would
    // draw freed widgets, so lay the windows out again first
    if (gui_needs_relayout) process_layout ();

    // 6. repaint all the editors
    t2= texmacs_time ();
    int n_events= SDL_PollEvent (NULL);
    if (n_events == 0 || request_partial_redraw) {
      request_partial_redraw= false;

      interrupted= false;
      interrupt_time= texmacs_time () + (100 / (n_events + 1));

      vue_simple_widget_rep::repaint_all ();
      // note that repaint can be interrupted if events are present
      //FIXME: we should redraw the focused editor first, then the others

      request_partial_redraw= interrupted;
    }
    t1= t2; t2= texmacs_time ();
    if (DEBUG_VUE && t2 - t1 >= 30) debug_widgets << "repaint took " << t2 - t1 << "ms" << LF;

    // 7. redraw the UI
    process_redraw ();
    t1= t2; t2= texmacs_time ();
    if (DEBUG_VUE && t2 - t1 >= 50) debug_widgets << "redraw took " << t2 - t1 << "ms" << LF;
    gui_wait= true;
  }
}

void process_layout () {
  // reset memory pools
  styled_strings= array<styled_string>();
  gui_needs_relayout= false; // widgets deleted while laying out are not drawn
  
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy()) { // and then the other windows
    vue_window_rep *win= (vue_window_rep*) Window_to_window [it->next()];
    win->process_layout ();
  }
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
*   click x y [left|right|middle]   press followed by release
*   wheel x y dx dy                 wheel event at (x, y)
*   key <SDL key name>              key press, e.g. Return, Escape, Tab, Down
*   text <string>                   text input
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
  if (script_win != NULL && Window_to_window->contains ((SDL_Window*) script_win->platform_window ()))
    return script_win;
  return last_created_window;
}

static Uint8
script_button (array<string> a, int i) {
  if (N(a) > i && a[i] == "right") return SDL_BUTTON_RIGHT;
  if (N(a) > i && a[i] == "middle") return SDL_BUTTON_MIDDLE;
  return SDL_BUTTON_LEFT;
}

static void
script_push_button (vue_window win, float x, float y, Uint8 button, bool down) {
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= down ? SDL_EVENT_MOUSE_BUTTON_DOWN : SDL_EVENT_MOUSE_BUTTON_UP;
  ev.button.timestamp= SDL_GetTicksNS ();
  ev.button.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
  ev.button.button= button;
  ev.button.down= down;
  ev.button.clicks= 1;
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
  ev.motion.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
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
      iterator<SDL_Window*> it= iterate (Window_to_window);
      while (it->busy ()) {
        vue_window w= (vue_window) Window_to_window [it->next ()];
        if (title == "#" * as_string (w->id) ||
            occurs (title, w->name) || occurs (title, w->get_name ())) script_win= w;
      }
      script_no_target= (script_win == NULL);
      if (script_win == NULL) cout << "vue script: no window matches " << title << LF;
      else {
        int wx, wy, ww, wh;
        SDL_GetWindowPosition ((SDL_Window*) script_win->platform_window (), &wx, &wy);
        SDL_GetWindowSize ((SDL_Window*) script_win->platform_window (), &ww, &wh);
        cout << "vue script: window at " << wx << "," << wy << " size " << ww << "x" << wh << LF;
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
      script_push_motion (win, as_double (a[1]), as_double (a[2]));
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), true);
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), false);
    }
    else if (cmd == "wheel" && N(a) > 4) {
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_MOUSE_WHEEL;
      ev.wheel.timestamp= SDL_GetTicksNS ();
      ev.wheel.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
      ev.wheel.mouse_x= as_double (a[1]);
      ev.wheel.mouse_y= as_double (a[2]);
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
      c_string name (kn);
      ev.type= SDL_EVENT_KEY_DOWN;
      ev.key.timestamp= SDL_GetTicksNS ();
      ev.key.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
      ev.key.scancode= SDL_GetScancodeFromName (name);
      ev.key.key= SDL_GetKeyFromScancode (ev.key.scancode, SDL_KMOD_NONE, false);
      ev.key.mod= mod;
      ev.key.down= true;
      SDL_PushEvent (&ev);
    }
    else if (cmd == "text" && N(a) > 1) {
      // one text input event per (utf8) character, as SDL does
      static char buffers[64][8]; // the events keep pointers to the text
      static int next= 0;
      string txt= line (N(cmd)+1, N(line));
      int i= 0;
      while (i < N(txt)) {
        int start= i;
        // the length of the utf8 sequence from its lead byte
        unsigned char c= (unsigned char) txt[i];
        int len= (c < 0x80) ? 1 : (c >= 0xF0) ? 4 : (c >= 0xE0) ? 3 : (c >= 0xC0) ? 2 : 1;
        i= min (N(txt), start + len);
        char* buf= buffers[next++ % 64];
        int n= min (i - start, 7);
        for (int j=0; j<n; j++) buf[j]= txt[start+j];
        buf[n]= 0;
        SDL_Event ev;
        SDL_zero (ev);
        ev.type= SDL_EVENT_TEXT_INPUT;
        ev.text.timestamp= SDL_GetTicksNS ();
        ev.text.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
        ev.text.text= buf;
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
    else if (cmd == "focus") {
      // pretend the window got the keyboard focus (a test instance launched
      // while another application is in use never gets it)
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_WINDOW_FOCUS_GAINED;
      ev.window.timestamp= SDL_GetTicksNS ();
      ev.window.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
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
      ev.edit.windowID= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
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
      Uint32 wid= SDL_GetWindowID ((SDL_Window*) win->platform_window ());
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
      ev.drop.x= as_double (a[1]);
      ev.drop.y= as_double (a[2]);
      ev.drop.data= buf;
      SDL_PushEvent (&ev);
      SDL_zero (ev);
      ev.type= SDL_EVENT_DROP_COMPLETE;
      ev.drop.timestamp= SDL_GetTicksNS ();
      ev.drop.windowID= wid;
      ev.drop.x= as_double (a[1]);
      ev.drop.y= as_double (a[2]);
      SDL_PushEvent (&ev);
    }
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

static void update_mouse_state () {
  unsigned int state= 0;

  float x, y;

  Uint32 buttons= SDL_GetGlobalMouseState (&x, &y);
  if (script_active) buttons= script_buttons; // synthetic events
  SDL_Keymod mods= SDL_GetModState();

  // compute state
  if ((buttons & SDL_BUTTON_LMASK) != 0)  state += 1;
  if ((buttons & SDL_BUTTON_MMASK) != 0)  state += 2;
  if ((buttons & SDL_BUTTON_RMASK) != 0)  state += 4;
  if ((buttons & SDL_BUTTON_X1MASK) != 0) state += 8;
  if ((buttons & SDL_BUTTON_X2MASK) != 0) state += 16;
  if ((mods & SDL_KMOD_SHIFT) != 0) state += 256;
  if ((mods & SDL_KMOD_CTRL)  != 0) state += 1024 + 4;
  if ((mods & SDL_KMOD_ALT)  != 0)  state += 2048 + 2;
  if ((mods & SDL_KMOD_GUI)  != 0)  state += 4096;
//  if ((mods & SDL_KMOD_CAPS)  != 0) state += 1024;
  mouse_state= state;
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

  // Remove modifiers in current event are already used to compose key
  if ( (*current_mod & SDL_KMOD_SHIFT) && (*current_mod & SDL_KMOD_ALT) && (results[3] != results[0])) {
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
    vue_dialog_finish ((vue_dialog_result*) event->user.data1);
    return;
  }
  switch (event->type) {
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
      if (win) notify_window_focus (win, event->type == SDL_EVENT_WINDOW_FOCUS_GAINED);
      break;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
    {
      update_mouse_state ();
      win= get_window_from_ID (event->button.windowID);
      float bx= event->button.x, by= event->button.y;
      bool down= (event->button.type == SDL_EVENT_MOUSE_BUTTON_DOWN);
      if (win && popup_grab (win, bx, by, down)) {
        string action;
        if (down) {
          action= "press-" * mouse_decode (mouse_state | SDL_BUTTON_MASK (event->button.button));
        } else {
          action= "release-" * mouse_decode (mouse_state | SDL_BUTTON_MASK (event->button.button));
        }
        vue_input_state& in= win->input;
        in.mouse_action= action;
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
      if (win) {
        vue_input_state& in= win->input;
        in.mouse_time= texmacs_time();
        in.mouse_x= (int) (event->wheel.mouse_x * win->density);
        in.mouse_y= (int) (event->wheel.mouse_y * win->density);
        wheel_event (win, event->wheel.x, event->wheel.y, in.mouse_time); // kinetic scrolling, see above
      }
      break;
    } // case SDL_EVENT_MOUSE_WHEEL:
    case SDL_EVENT_MOUSE_MOTION:
    {
      close_help_balloon (); // it lives until the pointer or a key moves
      update_mouse_state ();
      win= get_window_from_ID (event->motion.windowID);
      float mx= event->motion.x, my= event->motion.y;
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
      if (DEBUG_VUE_EVENTS) {
        c_string buf (print_key_info (&(event->key)));
        SDL_Log ("Keydown: %s ", (char*) buf);
      }
      win= get_window_from_ID (event->key.windowID);
      if (win) {
        if (event->key.scancode == SDL_SCANCODE_F1) {
          // toggle the debug mode for the current window
          win->clay_debug = !win->clay_debug;
          if (DEBUG_VUE) debug_widgets << "Clay debug view " << (win->clay_debug ? "on" : "off") << LF;
          break;
        }

        bool produces_text= false;
        string key= lookup_key (event->key.scancode, event->key.mod, &produces_text);
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
          win->input.key_stamp= event->key.timestamp;
        }
      }
      break;
    } // case SDL_EVENT_KEY_DOWN:
    case SDL_EVENT_TEXT_INPUT:
    {
      // the text typed by a keystroke (see SDL_EVENT_KEY_DOWN): the key
      // names of TeXmacs for the characters which have one
      string r= utf8_to_cork (event->text.text);
      if (r == " ") r= "space";
      else if (r == "<") r= "<less>";
      else if (r == ">") r= "<gtr>";
      win= get_window_from_ID (event->text.windowID);
      if (win) {
        // a text event right after a key delivered as a key (a command
        // modifier, an unconsumed alt) belongs to that keystroke
        if (win->input.key_stamp != 0 &&
            event->text.timestamp - win->input.key_stamp < 30000000ull) {
          c_string lk (win->input.last_key);
          SDL_Log ("Text input '%s' follows the key %s: ignored", event->text.text, (char*) lk);
        } else {
          win->input.key_event= r;
          win->input.key_time= texmacs_time();
          win->input.last_key= r;
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
      if (win == NULL || N(drop_doc) == 0) { drop_doc= tree (CONCAT); break; }
      vue_input_state& in= win->input;
      in.mouse_action= "drop";
      in.mouse_time= texmacs_time ();
      in.mouse_x= (int) (event->drop.x * win->density);
      in.mouse_y= (int) (event->drop.y * win->density);
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
      win= get_window_from_ID (event->edit.windowID);
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
    vue_window win= get_window_from_ID (event->window.windowID);
    if (win) {
      busy= true;
      with_window frame (win);
      Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) event->window.data1, (float) event->window.data2 });
      win->process_layout();
      vue_simple_widget_rep::notify_resizes ();
      if (the_interpose_handler != NULL) the_interpose_handler ();
      if (gui_needs_relayout) process_layout ();
      vue_simple_widget_rep::repaint_all_in_window (win);
      win->process_redraw();
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
  if (tt) {
    // WIDGET_STYLE_MONOSPACED: a typewriter font, whatever the platform
    tree tt_fn= tuple ("modern", "tt", series, "right");
    tt_fn << as_string (sz) << as_string (dpi);
    return find_font (tt_fn);
  }
  if (use_macos_fonts ()) {
    tree lucida_fn= tuple ("apple-lucida", "ss", series, "right");
    lucida_fn << as_string (sz) << as_string ((int) (0.95 * dpi));
    return find_font (lucida_fn);
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

    // Handle encoding preferences
    string enc = get_preference ("texmacs->verbatim:encoding");
    if (enc == "auto")
      enc = get_locale_charset ();

    // SDL3 clipboard uses UTF-8, so ensure proper encoding
    // (assuming text is already UTF-8 compatible or needs conversion)
    clip_data->plain_text = plain_text;
  }
  else if (format == "html") {
    clip_data->html_text = s;
    clip_data->plain_text = s; // Also provide as plain text fallback
  }
  else if (format == "latex") {
    clip_data->plain_text= s; // SDL3 uses UTF-8
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

bool get_selection (string key, tree& t, string& s, string format) {
  // Retrieve the selection 't' of a given 'format' from the clipboard 'cb',
  // where 's' is the string serialization of t according to the format
  // Returns true on success; sets t to (extern s) for external selections

  bool direct_selection = (key == "extern");
  if (direct_selection) key = "primary";

  // SDL3 doesn't support mouse/selection clipboard, only primary
  if (key != "primary") return false;

  s = "";
  t = "none";

  // the keys other than "primary" (the internal buffers of TeXmacs, "temp",
  // "wrapbuf"...) live in our own storage; the condition also excluded
  // "primary" and could never hold, so nothing was ever read back
  bool owns= (key != "primary");

  if (owns && (selection_t->contains (key))) {
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
  if (input_format != "" && !direct_selection) {
    s = as_string (call ("convert", s, input_format, "texmacs-snippet"));
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

  // Clear the SDL clipboard
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
  // Inform the gui that the editor needs to update itself
  // before repainting can start
  gui_needs_update= false;
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
#if MUPDF_RENDERER
  mupdf_image_gc (name);
#else
  (void) name;
#endif
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

void show_wait_indicator (widget base, string message, string argument) {
  // Display a wait indicator with a message and an optional argument, at
  // the centre of the window which triggered the lengthy operation; an
  // empty message pops the last one (the calls are nested)
  (void) base;
  if (is_headless ()) return;
  if (N(message) > 0) {
    string msg= message;
    if (argument != "") msg= msg * " " * argument * "...";
    wait_messages= list<string> (msg, wait_messages);
  }
  else if (!is_nil (wait_messages)) wait_messages= wait_messages->next;

  if (!is_nil (wait_indicator_wid)) {
    set_visibility (wait_indicator_wid, false);
    destroy_window_widget (wait_indicator_wid);
    wait_indicator_wid= widget ();
  }
  if (is_nil (wait_messages) || !has_current_window ()) return;

  widget lab= text_widget (wait_messages->item, 0, black);
  wait_indicator_wid= popup_window_widget (lab, "Wait");
  SI wx= 0, wy= 0, ww= 0, wh= 0;
  widget win= get_window (concrete_window () -> win);
  get_position (win, wx, wy);
  get_size (win, ww, wh);
  set_position (wait_indicator_wid, wx + ww/2, wy - wh/2);
  set_visibility (wait_indicator_wid, true);
  // the window must appear now: the operation which asked for it is about
  // to block the loop
  process_layout ();
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
  (void) filter_index;
  vue_dialog_result* res= (vue_dialog_result*) userdata;
  if (filelist == NULL) {
    SDL_Log ("File dialog error: %s", SDL_GetError ());
    res->chosen= false;
  }
  else if (*filelist == NULL) res->chosen= false; // cancelled
  else {
    res->file= string (*filelist, (int) strlen (*filelist));
    res->chosen= true;
  }
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= vue_dialog_event;
  ev.user.data1= res;
  if (!SDL_PushEvent (&ev)) {
    SDL_Log ("cannot deliver the result of the file dialog: %s", SDL_GetError ());
    tm_delete (res);
  }
}

// called from the main loop when the event pushed above arrives
static void
vue_dialog_finish (vue_dialog_result* res) {
  if (res == NULL) return;
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

void
vue_chooser_widget_rep::perform_dialog (vue_window win) {
 
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

  void *sdl_filters;
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
    sdl_n_filters= 1;
    filter= as_string (call ("format-get-name", file_type));
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
  map (SDLK_LEFT_TAB, "tab");
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


static string
lookup_key (SDL_Scancode scancode, SDL_Keymod mod, bool* produces_text) {
  SDL_Keycode key= postprocess_key_event (scancode, &mod, false);
  if (key == SDLK_UNKNOWN) return ""; // it is only a modifier, we ignore it
  // a character key without a command modifier types text: the system
  // sends the text (composed with the dead keys and the input method) in a
  // text event, which is delivered instead of the key
  if (produces_text != NULL)
    *produces_text= (key >= 0x20 && key != 0x7f && (key & SDLK_SCANCODE_MASK) == 0 &&
                     (mod & (SDL_KMOD_CTRL | SDL_KMOD_ALT | SDL_KMOD_GUI)) == 0);

  if (DEBUG_VUE_EVENTS)
    debug_events << "postprocessed key: " << SDL_GetKeyName (key) << " " << print_modifiers (mod) << LF;

  const char* str= SDL_GetKeyName (key);
  string r (str, (int)strlen (str));
  r= utf8_to_cork (r);
  if (contains_unicode_char (r)) return r;
  string s=r;
  if ((key >= 'A') && (key <= 'Z')) s= upper_key[key - 'A' + 'a'];
  else if ((key >= 'a') && (key <= 'z')) s= lower_key[key];
  else if (lower_key->contains(key))  s= lower_key [key];
  if ((N(s)>=2) && (s[0]=='K') && (s[1]=='-')) s= s (2, N(s));

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

