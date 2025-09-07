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

#include "message.hpp"
#include "window.hpp"
#include "iterator.hpp"
#include "font.hpp"
#include "dictionary.hpp" // get_output_language


#include "analyze.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "scheme.hpp"
#include "dictionary.hpp"
#include "editor.hpp"
#include "new_view.hpp"      // get_current_editor()
#include "image_files.hpp"
#include "tm_window.hpp"

#include <SDL3/SDL.h>
#include <SDL3_ttf/SDL_ttf.h>
#include "../MuPDF/mupdf_picture.hpp"

#include "clay.h"


/*****************************************************************************/
// UI layout context (maybe refactor in a structure)

// keyboard events
extern string key_event;
extern time_t key_time;

// pointer info
extern string mouse_action;
extern time_t mouse_time;
extern unsigned int mouse_x;
extern unsigned int mouse_y;
extern unsigned int mouse_state;
extern array<double> mouse_data;


extern bool debug_clay;

// list of commands
extern list<command> cmd_list;

extern vue_window current_window; // used during layout to propagate information

void gui_init_context();


//******************************************************************************
// vue_window

int nr_windows= 0;
hashmap<SDL_Window*, pointer> Window_to_window;
hashmap<int, pointer> id_to_window;

class vue_sdl_base_window_rep : public vue_window_rep {
public:
  SDL_Window *sdl_win;
  
  vue_sdl_base_window_rep (vue_widget w, string name);
  ~vue_sdl_base_window_rep ();
  
  void *platform_window () { return (void*)sdl_win; }

  void   destroy_event ();
  void   set_name (string name);
  string get_name ();
  void   set_modified (bool flag);
  void   set_visibility (bool flag);
  void   set_size (SI w, SI h);
  void   set_size_limits (SI min_w, SI min_h, SI max_w, SI max_h);
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
      SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Failed to measure text: %s", SDL_GetError());
  }

  return (Clay_Dimensions) { (float) width, (float) height };
}

void HandleClayErrors (Clay_ErrorData errorData) {
  // See the Clay_ErrorData struct for more information
  printf ("%s", errorData.errorText.chars);
  // FIXME: properly handle Clay's errors
  FAILED ("Clay error");
}

static TTF_Font **ttf_fonts= NULL; // fonts cache

vue_sdl_base_window_rep::vue_sdl_base_window_rep (vue_widget _content, string _name)
: vue_window_rep (_content, _name)
{
  cout << "create vue_sdl_base_window_rep " << id << LF;
  SDL_WindowFlags flags= SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_RESIZABLE;
  int win_w= 200, win_h= 200;
  int win_x=30, win_y= 30;
  c_string buf (name);
  
  sdl_win= SDL_CreateWindow (buf, win_w, win_h, flags);
  if (!sdl_win) {
    SDL_LogError (SDL_LOG_CATEGORY_APPLICATION, "Couldn't create window: %s", SDL_GetError());
  }
  
  nr_windows++;
  SDL_SetWindowPosition (sdl_win, win_x, win_y);
  Window_to_window (sdl_win)= (void*) this;
  id= serial++;
  id_to_window (id)= this;
  
  // update widget state
  set_identifier (abstract (content), id);
  notify_position (abstract (content), 0, 0);
  notify_size (abstract (content), win_w,  win_h);
  
  // init Clay
  uint64_t totalMemorySize= Clay_MinMemorySize ();
  Clay_Arena clay_arena= (Clay_Arena) {
      .memory=  (char*) SDL_malloc (totalMemorySize),
      .capacity= totalMemorySize
  };

  clay_ctx= Clay_Initialize (clay_arena, (Clay_Dimensions) { (float) win_w, (float) win_h }, (Clay_ErrorHandler) { HandleClayErrors });
  relayout= true;
  clay_debug= false;
}

vue_sdl_base_window_rep::~vue_sdl_base_window_rep () {
  cout << "destroy vue_sdl_base_window_rep " << id << LF;
  id_to_window->reset (id);
  id= 0;
  set_identifier (abstract (content), 0); // FIXME: is this ok?
  Window_to_window->reset (sdl_win);
  nr_windows--;

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
  //min_w= Min_w; min_h= Min_h; max_w= Max_w; max_h= Max_h;
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
#if 0
  if (min_w == Min_w && min_h == Min_h && max_w == Max_w && max_h == Max_h)
    return;
  Min_w= min_w; Min_h= min_h; Max_w= max_w; Max_h= max_h;
  min_w= min_w/PIXEL; min_h= min_h/PIXEL;
  max_w= max_w/PIXEL; max_h= max_h/PIXEL;
  SDL_SetWindowMaximumSize (sdl_win, max_w, max_h);
  SDL_SetWindowMinimumSize (sdl_win, min_w, min_h);
#endif
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
  if (flag) SDL_ShowWindow (sdl_win);
  else SDL_HideWindow (sdl_win);
}
 
void
vue_sdl_base_window_rep::process_layout () {
  // init the current GUI context
  Clay_SetCurrentContext (clay_ctx);
  int win_x, win_y, win_w, win_h;
  SDL_GetWindowSizeInPixels (sdl_win, &win_w, &win_h);
  SDL_GetWindowPosition (sdl_win, &win_x, &win_y);
  Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) win_w, (float) win_h });
  current_window= this;
  gui_init_context ();

  // layout the top widget
  Clay_SetDebugModeEnabled (clay_debug);
  Clay_BeginLayout ();
  content->do_layout ();
  render_commands= Clay_EndLayout ();
  
  // post layout tweaking
  content->post_layout ();
  
  // reset for safety (should not be used outside layout)
  current_window= NULL;
}

//******************************************************************************
// rendering via SDL renderer

class vue_sdl_window_rep : public vue_sdl_base_window_rep {
public:
  SDL_Renderer *sdl_ren;
  TTF_TextEngine *text_engine;

  vue_sdl_window_rep (vue_widget w, string name);
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

void snapshot_pixmap (fz_pixmap *pix);

void
sdl_draw_picture (SDL_Renderer *sdl_ren, picture pic, SDL_FRect *dest) {
  // propagate immediately the changes to the screen
  fz_pixmap *pix= ((mupdf_picture_rep*)pic->get_handle())->pix;
  snapshot_pixmap (pix);
  unsigned char *pixels= fz_pixmap_samples (mupdf_context (), pix);
  int w= fz_pixmap_width (mupdf_context (), pix);
  int h= fz_pixmap_height (mupdf_context (), pix);
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

vue_sdl_window_rep::vue_sdl_window_rep (vue_widget w, string name)
  : vue_sdl_base_window_rep (w, name)
{
  if (!sdl_ren) {
    sdl_ren= SDL_CreateRenderer(sdl_win, NULL);
    if (!sdl_ren) {
        SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Failed to create renderer: %s", SDL_GetError());
    }
  }
  
  if (!text_engine) {
    text_engine= TTF_CreateRendererTextEngine (sdl_ren);
    if (!text_engine) {
        SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Failed to create text engine from renderer: %s", SDL_GetError());
    }

    if (!ttf_fonts) {
      ttf_fonts= (TTF_Font **)SDL_calloc (1, sizeof(TTF_Font *));
      if (!ttf_fonts) {
        SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Failed to allocate memory for the font array: %s", SDL_GetError());
        return;
      }
      
      TTF_Font *font= TTF_OpenFont( //"/Users/mgubi/t/clay/examples/SDL3-simple-demo/resources/Roboto-Regular.ttf"
            "/Users/mgubi/.TeXmacs/fonts/unpacked/LucidaGrande.0.ttf",
          24);
      if (!font) {
        SDL_LogError(SDL_LOG_CATEGORY_ERROR, "Failed to load font: %s", SDL_GetError());
        return;
      }
      ttf_fonts[0]= font;
    }
    Clay_SetCurrentContext (clay_ctx);
    Clay_SetMeasureTextFunction (SDL_MeasureText, ttf_fonts);
  }
}

vue_sdl_window_rep::~vue_sdl_window_rep () {
  TTF_DestroyRendererTextEngine (text_engine);
  SDL_DestroyRenderer (sdl_ren);
}

void
vue_sdl_window_rep::process_redraw () {
  // render!
  SDL_SetRenderDrawColor(sdl_ren, 0, 0, 0, 255);
  SDL_RenderClear(sdl_ren);

  Clay_SDL3RendererData rd{ sdl_ren, text_engine, ttf_fonts };
  SDL_Clay_RenderClayCommands (&rd, &render_commands);

  SDL_RenderPresent(sdl_ren);
}

//******************************************************************************
// rendering via MuPDF renderer

array<styled_string> styled_strings;

class vue_sdl_mupdf_window_rep : public vue_sdl_base_window_rep {
public:
  renderer ren;
  picture backing_store;

  vue_sdl_mupdf_window_rep (vue_widget w, string name);
  ~vue_sdl_mupdf_window_rep () { delete_renderer (ren); }
  
  void process_redraw ();
  void process_layout ();
  
  void draw_picture (void *data, picture pic);
  void get_viewport_size (void *data, int& w, int& h);
};

void render_clay_commands (renderer ren, Clay_RenderCommandArray *rcommands);

Clay_Dimensions
ren_measure_text (Clay_StringSlice text, Clay_TextElementConfig *config, void *userData) {
  vue_sdl_mupdf_window_rep *win= (vue_sdl_mupdf_window_rep*)userData;
  if (win) {
    string s(text.chars, text.length);
//    font fn= (font_rep*)config->userData;
    static font fn;
    if (is_nil (fn)) fn= get_default_styled_font (0);
    metric  ex;
    fn->var_get_extents (s, ex);
    SI w = ((ex->x2- ex->x1+ 2)/3);
    SI h = ((fn->y2- fn->y1+ 2)/3);
    abs_round (w, h);
    return (Clay_Dimensions){ .width= (float)2 *w / PIXEL, .height= (float)2*h  / PIXEL };
  }
}

vue_sdl_mupdf_window_rep::vue_sdl_mupdf_window_rep (vue_widget w, string name)
  : ren(NULL), vue_sdl_base_window_rep (w, name)
{
  Clay_SetCurrentContext (clay_ctx);
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
  snapshot_pixmap (pix);
  unsigned char *pixels= fz_pixmap_samples (mupdf_context (), pix);
  int w= fz_pixmap_width (mupdf_context (), pix);
  int h= fz_pixmap_height (mupdf_context (), pix);
  SDL_Surface *surf= SDL_CreateSurfaceFrom (w, h, SDL_PIXELFORMAT_RGBA32, pixels, 4*w);
  // FIXME: premultiplied?
  SDL_FRect src= { 0, 0, (float)w, (float)h };
  //SDL_RenderFillRect (sdl_ren, dest);
  SDL_BlitSurface (surf, 0, dest_surf, 0);
  SDL_DestroySurface (surf);
}

picture
native_picture_from_SDL_Surface (SDL_Surface *surf) {
  fz_pixmap *pix= fz_new_pixmap_with_data (mupdf_context (), fz_device_bgr (mupdf_context ()),
                                           surf->w, surf->h, NULL, 1, 4*surf->w, (unsigned char*)surf->pixels);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
}

void
vue_sdl_mupdf_window_rep::process_redraw () {
  int win_w, win_h;

  SDL_Surface *surf= SDL_GetWindowSurface(sdl_win);
  backing_store= native_picture_from_SDL_Surface (surf);
  if (ren) delete_renderer (ren);
  ren= picture_renderer (backing_store, std_shrinkf * retina_factor);
  win_w = surf->w;
  win_h = surf->h;
    
  time_t t1, t2;
  t2= texmacs_time ();
  ren->set_pencil (rgb_color (255,0,0));
  ren->fill (0, -win_h * ren->pixel, win_w * ren->pixel, 0);
  render_clay_commands (ren, &render_commands);
  t1= t2; t2= texmacs_time ();
  if (t2 - t1 > 30) cout << "render_clay_commands took " << t2 - t1 << "ms" << LF;
  
  //SDL_SetRenderDrawColor (sdl_ren, 0, 0, 0, 255);
  //SDL_RenderClear (sdl_ren);
  SDL_UpdateWindowSurface (sdl_win);
  t1= t2; t2= texmacs_time ();
  if (t2 - t1 > 30) cout << "SDL_UpdateWindowSurface took " << t2 - t1 << "ms" << LF;
}


typedef void (*render_fn) (renderer ren, void *data, rectangle rect);

struct vue_render_ren_data {
  renderer ren;
  rectangle r;
};

typedef void (*render_fn) (renderer ren, void *data, rectangle rect);

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
  for (int32_t i = 0; i < rcommands->length; i++) {
      Clay_RenderCommand *rcmd = Clay_RenderCommandArray_Get (rcommands, i);
      const Clay_BoundingBox bounding_box = rcmd->boundingBox;
      rectangle r (bounding_box.x * ren->pixel,
                   -(bounding_box.y + bounding_box.height) * ren->pixel,
                   (bounding_box.x + bounding_box.width)  * ren->pixel,
                   -bounding_box.y * ren->pixel);
      switch (rcmd->commandType) {
          case CLAY_RENDER_COMMAND_TYPE_RECTANGLE: {
            Clay_RectangleRenderData *config = &rcmd->renderData.rectangle;
//              SDL_SetRenderDrawBlendMode(rendererData->renderer, SDL_BLENDMODE_BLEND);
            color c= rgb_color (config->backgroundColor.r, config->backgroundColor.g, config->backgroundColor.b, config->backgroundColor.a);
            ren->set_pencil (c);
            if (config->cornerRadius.topLeft > 0) {
              ren->fill (r->x1, r->y1, r->x2, r->y2);
//              SDL_Clay_RenderFillRoundedRect(rendererData, rect, config->cornerRadius.topLeft, config->backgroundColor);
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
            ren->set_pencil (rgb_color (config->textColor.r, config->textColor.g, config->textColor.b, config->textColor.a));
            //font fn= get_default_styled_font (style);
            font fn= get_default_styled_font (0); //FIXME: consider style
            ren->set_shrinking_factor (3);
            string s (config->stringContents.chars,
                      config->stringContents.length);
            fn ->var_draw (ren, s, r->x1*3, r->y1*3- fn->y1);
            ren->set_shrinking_factor (1);
          } break;
          case CLAY_RENDER_COMMAND_TYPE_BORDER: {
              Clay_BorderRenderData *config = &rcmd->renderData.border;

              const float minRadius = min (bounding_box.width, bounding_box.height) / 2.0f;
              const Clay_CornerRadius clampedRadii = {
                  .topLeft= (float) min (config->cornerRadius.topLeft, minRadius) * ren->pixel,
                  .topRight= (float) min (config->cornerRadius.topRight, minRadius) * ren->pixel,
                  .bottomLeft= (float) min (config->cornerRadius.bottomLeft, minRadius) * ren->pixel,
                  .bottomRight= (float) min (config->cornerRadius.bottomRight, minRadius) * ren->pixel
              };
              //edges
              ren->set_pencil (rgb_color (config->color.r, config->color.g, config->color.b, config->color.a));

              if (config->width.left > 0) {
                ren->fill (r->x1 - ren->pixel,
                           r->y1 - clampedRadii.topLeft,
                           r->x1 + config->width.left * ren->pixel,
                           r->y2 + clampedRadii.bottomLeft );
              }
              if (config->width.right > 0) {
                ren->fill (r->x2 + ren->pixel - config->width.right * ren->pixel,
                           r->y1 - clampedRadii.topRight,
                           r->x2 + ren->pixel,
                           r->y2 + clampedRadii.bottomRight );
              }
              if (config->width.top > 0) {
                ren->fill (r->x1 + clampedRadii.topLeft,
                           r->y2 - config->width.top * ren->pixel,
                           r->x2 - clampedRadii.topRight,
                           r->y2 + ren->pixel);
              }
              if (config->width.bottom > 0) {
                ren->fill (r->x2 + clampedRadii.bottomLeft,
                           r->y1 - ren->pixel,
                           r->x2 - clampedRadii.bottomRight,
                           r->y1 + config->width.bottom * ren->pixel);
              }
              //corners
              if (config->cornerRadius.topLeft > 0) {
                ren->fill_arc (r->x1, r->y1, r->x1 + clampedRadii.topLeft, r->y1 - clampedRadii.topLeft, 90, 180);
              }
              if (config->cornerRadius.topRight > 0) {
                ren->fill_arc (r->x2, r->y1, r->x2 - clampedRadii.topRight, r->y1 - clampedRadii.topRight, 0, 90);
              }
              if (config->cornerRadius.bottomLeft > 0) {
                ren->fill_arc (r->x1, r->y2, r->x1 + clampedRadii.bottomLeft, r->y2 - clampedRadii.bottomLeft, 180, 270);
              }
              if (config->cornerRadius.bottomRight > 0) {
                ren->fill_arc (r->x2, r->y2, r->x2 - clampedRadii.bottomRight, r->y2 - clampedRadii.bottomRight, 270, 360);
              }

          } break;
          case CLAY_RENDER_COMMAND_TYPE_SCISSOR_START: {
            Clay_BoundingBox boundingBox = rcmd->boundingBox;
            ren->clip (rcmd->boundingBox.x * ren->pixel,
                       -(rcmd->boundingBox.y + rcmd->boundingBox.height)  * ren->pixel,
                       (rcmd->boundingBox.x + rcmd->boundingBox.width)  * ren->pixel,
                       -rcmd->boundingBox.y * ren->pixel);
              break;
          }
          case CLAY_RENDER_COMMAND_TYPE_SCISSOR_END: {
            ren->unclip ();
            break;
          }
          case CLAY_RENDER_COMMAND_TYPE_IMAGE: {
            cout << "CLAY_RENDER_COMMAND_TYPE_IMAGE unsupported" << LF;
              //SDL_Texture *texture = (SDL_Texture *)rcmd->renderData.image.imageData;
              break;
          }
          case CLAY_RENDER_COMMAND_TYPE_CUSTOM: {
            render_fn fn= (render_fn)rcmd->renderData.custom.customData;
            fn (ren, rcmd->userData, r);
            break;
          }
          default:
              SDL_Log("Unknown render command type: %d", rcmd->commandType);
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

void layout_text (string s, int style, color c) {
  font fn= get_default_styled_font (style);
  metric ex;
  fn->var_get_extents (s, ex);
  SI w = ((ex->x2- ex->x1+ 2)/3);
  SI h = ((fn->y2- fn->y1+ 2)/3);
  abs_round (w, h);
  styled_string ss= tm_new<styled_string_rep> (s, fn, c);
  styled_strings << ss;
  CLAY({
    .layout= {
      .sizing= {
        CLAY_SIZING_FIXED((float)2*w/PIXEL),
        CLAY_SIZING_FIXED((float)2*h/PIXEL) }},
    .custom= { .customData= vue_render_text },
    .userData= ss.rep
  }) {};
}

//******************************************************************************
// entrypoints for windows

vue_window
plain_window (vue_widget wwid, string name) {
 return tm_new<vue_sdl_mupdf_window_rep> (wwid, name);
}

//******************************************************************************
// vue_gui

/******************************************************************************
* Main routines
******************************************************************************/

bool char_clip= true;

void initialize_keyboard ();

void gui_open (int& argc, char** argv) {
  // start the gui
  
  if (!SDL_Init (SDL_INIT_VIDEO|SDL_INIT_AUDIO)) {
    SDL_Log ("Unable to initialize SDL: %s", SDL_GetError ());
    exit (-1);
  }

  if (!TTF_Init()) {
    exit (-1);
  }

  SDL_SetHint (SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, "1");
  
  set_retina_factor (2);
  initialize_colors ();
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
  if (SDL_GetDisplayBounds (1, &r)) {
    width= r.w * PIXEL;
    height= r.h * PIXEL;
    //cout << "SCREEN:" << screen_width << "," << screen_height << LF;
  } else {
    SDL_Log ("SDL_GetDisplayBounds failed: %s", SDL_GetError ());
  }
}

void gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  gui_root_extents (width, height);
}

void gui_refresh () {
  // update and redraw all windows (e.g. on change of output language)
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
void process_event (SDL_Event *event);
void process_messages ();
void process_layout ();
void process_redraw ();

bool gui_wait=  false;
bool gui_needs_update= true;

void gui_start_loop () {
  // start the main loop
  int  delay= 10;
  request_partial_redraw= true;
  time_t t1, t2;

  // FIXME: Don't typeset when resizing window

  while (nr_windows > 0 || number_of_servers () > 0) {
    
    // 1. process events
    SDL_Event event;
    if (SDL_PollEvent (&event)) {
      process_event (&event);
      gui_needs_update= true;
    }

    if (gui_needs_update) {
      delay= 10;
      gui_wait= false;
      gui_needs_update= false;
    }
        
    // 2. wait for events on all channels
    if (gui_wait) {
      SDL_Delay (delay);
      delay += (delay/5);
      if (delay > 500) delay= 500;
    }

    // 3. process layout and handle events
    {
      t2= texmacs_time ();
      process_layout ();
      t1= t2; t2= texmacs_time ();
      if (t2 - t1 >= 30) cout << "layout took " << t2 - t1 << "ms\n";
    }
    
    // 4. exec commands if present
    if (!is_nil (cmd_list)) {
      list<command> l= reverse(cmd_list);
      cmd_list= list<command>();
      while (!is_nil(l)) {
        cout << "run command " << l->item << LF;
        l->item->apply();
        l= l->next;
      }
    }
    
    // 5. interpose
    t2= texmacs_time ();
    if (the_interpose_handler != NULL) the_interpose_handler ();
    if (nr_windows == 0) continue;
    t1= t2; t2= texmacs_time ();
    if (t2 - t1 >= 30) cout << "interpose took " << t2-t1 << "ms\n";

    if (nr_windows == 0) continue;

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
    if (t2 - t1 >= 30) cout << "repaint took " << t2 - t1 << "ms\n";

    // 7. redraw the UI
    process_redraw ();
    t1= t2; t2= texmacs_time ();
    if (t2 - t1 >= 30) cout << "redraw took " << t2 - t1 << "ms\n";
    gui_wait= true;
  }
}

void process_layout () {
  // reset memory pools
  styled_strings= array<styled_string>();
  
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
    current_window= win;
    win->process_redraw ();
    current_window= NULL;
  }
}

static vue_window
get_window_from_ID (Uint32 ID) {
  SDL_Window *w= SDL_GetWindowFromID (ID);
  if (w == NULL) return NULL;
  vue_window win= (vue_window) Window_to_window [w];
  return win;
}

static void update_mouse_state () {
  unsigned int state= 0;

  float x, y;

  Uint32 buttons= SDL_GetGlobalMouseState (&x, &y);
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
lookup_mouse (Uint8 button) {
  if (button == SDL_BUTTON_LEFT)   return "left";
  if (button == SDL_BUTTON_MIDDLE) return "middle";
  if (button == SDL_BUTTON_RIGHT)  return "right";
  if (button == SDL_BUTTON_X1)     return "extra1";
  if (button == SDL_BUTTON_X2)     return "extra2";
  return "button-error";
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

static string
lookup_key (SDL_Keycode key, SDL_Keymod mod) {
  const char* str= SDL_GetKeyName (key);
  string r (str, (int)strlen (str));
  r= utf8_to_cork (r);
  if (contains_unicode_char (r)) return r;
//  string s=r;
  if ((key >= 'A') && (key <= 'Z')) key= key - 'A' + 'a';
  string s= ((mod & SDL_KMOD_SHIFT) ? upper_key [key] : lower_key [key]);
  if ((N(s)>=2) && (s[0]=='K') && (s[1]=='-')) s= s (2, N(s));

  if (mod & SDL_KMOD_CTRL) s= "C-" * s;
  if (mod & SDL_KMOD_ALT)  s= "A-" * s;
  if (mod & SDL_KMOD_GUI)  s= "M-" * s;
  cout << "key press: " << s << LF;
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

void
process_event (SDL_Event *event) {
  // reset events
  mouse_action="";
  key_event="";
  vue_window win;
  if (event->type != SDL_EVENT_MOUSE_MOTION) sdl_log_event (event);
  switch (event->type) {
    case SDL_EVENT_WINDOW_RESIZED:
      win= get_window_from_ID (event->window.windowID);
      if (win) {
        Clay_SetCurrentContext (win->clay_ctx);
        Clay_SetLayoutDimensions ((Clay_Dimensions) { (float) event->window.data1, (float) event->window.data2 });
        win->relayout= true;
      }
      break;
    case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->destroy_event();
      break;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
    {
      update_mouse_state ();
      win= get_window_from_ID (event->button.windowID);
      if (win) {
        string action;
        if (event->button.type == SDL_EVENT_MOUSE_BUTTON_DOWN) {
          action= "press-" * mouse_decode (mouse_state | SDL_BUTTON_MASK (event->button.button));
        } else {
          action= "release-" * mouse_decode (mouse_state | SDL_BUTTON_MASK (event->button.button));
        }
        mouse_action= action;
        mouse_time= texmacs_time();
        mouse_x= event->button.x * retina_factor;
        mouse_y= event->button.y * retina_factor;
        Clay_SetCurrentContext (win->clay_ctx);
        Clay_SetPointerState ((Clay_Vector2) { (float) mouse_x, (float) mouse_y },
                             (event->button.button == SDL_BUTTON_LEFT) &&
                             (event->button.type == SDL_EVENT_MOUSE_BUTTON_DOWN));
      }
      break;
    } // case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_WHEEL:
    {
      update_mouse_state ();
      SDL_Log("Window %d got wheel event event %f %f",
              event->wheel.windowID, event->wheel.x, event->wheel.y);
      win= get_window_from_ID (event->wheel.windowID);
      if (win) {
        mouse_action= "wheel";
        mouse_time= texmacs_time();
        mouse_x= event->wheel.mouse_x * retina_factor;
        mouse_y= event->wheel.mouse_y * retina_factor;;
        mouse_data= array<double> (event->wheel.x * retina_factor, event->wheel.y * retina_factor);
        Clay_SetCurrentContext (win->clay_ctx);
        Clay_UpdateScrollContainers (true, (Clay_Vector2){ event->wheel.x * retina_factor, event->wheel.y * retina_factor }, 0.01f);
      }
      break;
    } // case SDL_EVENT_MOUSE_WHEEL:
    case SDL_EVENT_MOUSE_MOTION:
    {
      update_mouse_state ();
      win= get_window_from_ID (event->motion.windowID);
      if (win) {
        Clay_SetCurrentContext (win->clay_ctx);
        Clay_SetPointerState ((Clay_Vector2) { event->motion.x * retina_factor, event->motion.y * retina_factor },
                             event->button.button & SDL_BUTTON_LMASK);
        mouse_action= "move";
        mouse_time= texmacs_time();
        mouse_x= event->motion.x * retina_factor;
        mouse_y= event->motion.y * retina_factor;
      }
      break;
    } // case SDL_EVENT_MOUSE_MOTION:
    case SDL_EVENT_KEY_DOWN:
    {
      {
        c_string buf (print_key_info (&(event->key)));
        SDL_Log("Keydown: %s ", (char*)buf);
      }
      win= get_window_from_ID (event->key.windowID);
      if (win) {
        if (event->key.scancode == SDL_SCANCODE_F1) {
          // toggle the debug mode for the current window
          win->clay_debug = !win->clay_debug;
          cout << "TOGGLE debug mode " << (win->clay_debug ? "true" : "false") << LF;
          break;
        }
        if (event->key.scancode == SDL_SCANCODE_LSHIFT) break;
        SDL_Keymod m= event->key.mod;
        SDL_Keycode keycode= SDL_GetKeyFromScancode (event->key.scancode, event->key.mod, false);
        string key= lookup_key (keycode, event->key.mod);
        //cout << "Press " << key << " at " << (time_t) ev->xkey.time
        //<< " (" << texmacs_time() << ")\n";
        kbd_count++;
        //FIXME: conversion below loses precision from UInt64 to UInt32
        synchronize_time ((Uint32)event->key.timestamp);
        if (texmacs_time () - remote_time ((Uint32)event->key.timestamp) < 100 ||
            (kbd_count & 15) == 0)
          request_partial_redraw= true;
        //cout << "key   : " << key << "\n";
        //cout << "redraw: " << request_partial_redraw << "\n";
        //if (N(key)>0) win->key_event (key);
        if (N(key)>0) {
          key_event= key;
          key_time= texmacs_time();
        }
      }
      break;
    } // case SDL_EVENT_KEY_DOWN:
  } // switch (event->type)
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

bool set_selection (string cb, tree t,
                    string s, string sv, string sh, string format) {
  
  // Copy a selection 't' of a given 'format' to the clipboard 'cb',
  // where 's' contains the string serialization of t according to the format
  // and possibly the variants 'sv' and 'sh' for verbatim and html
  // Returns true on success
  
  //FIXME: implement
  return true;
}

bool get_selection (string cb, tree& t, string& s, string format) {
  // Retrieve the selection 't' of a given 'format' from the clipboard 'cb',
  // where 's' is the string serialization of t according to the format
  // Returns true on success; sets t to (extern s) for external selections
  
  //FIXME: implement
  return true;
}

void clear_selection (string cb) {
  // Clear the selection on clipboard 'cb'
  //FIXME: implement
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void beep () {
  // Issue a beep
  //FIXME: implement
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
    return (SDL_HasEvents(SDL_EVENT_FIRST, SDL_EVENT_LAST) == true);
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
  // Garbage collect images of a given name (may use wildcards)
  // This routine only needs to be implemented if you use your own image cache
  //FIXME: implement
}

void show_help_balloon (widget balloon, SI x, SI y) {
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
  //FIXME: implement
}

void show_wait_indicator (widget base, string message, string argument) {
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty
  //FIXME: implement
}

void external_event (string type, time_t t) {
  // External events, such as pushing a button of a remote infrared commander
  //FIXME: implement
}

//*****************************************************************************
// chooser_widget platform dependent dialog code

// Callback invoked when dialog is closed
static void SDLCALL
file_dialog_callback (void* userdata, const char* const* filelist,
                     int filter_index)
{
  vue_chooser_widget_rep *w= (vue_chooser_widget_rep *)userdata;
  
  if (!filelist) {
    SDL_Log("Error: %s", SDL_GetError());
    return;
  } else if (!*filelist) {
    SDL_Log("Dialog canceled or no selection.");
    w->callback (NULL);
    return;
  }

  for (const char* const* ptr = filelist; *ptr; ++ptr) {
    SDL_Log ("Selected: %s", *ptr);
    w->callback ((char*)*ptr);
    return;
  }

  if (filter_index >= 0) {
    SDL_Log("Selected filter index: %d", filter_index);
  } else {
    SDL_Log("Filter not reported by platform.");
  }
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
  else if (type == "directory")
    sdl_type= SDL_FILEDIALOG_OPENFOLDER;
  else
    sdl_type= SDL_FILEDIALOG_OPENFILE;

  if (type == "image") {
    sdl_n_filters= 4;
    sdl_filters= (void*)all_filters;
  } else if (type == "directory") {
    sdl_n_filters= 0;
  } else if (type == "generic") {
    sdl_n_filters= 0;
  } else {
    sdl_n_filters= 1;
    filter= as_string (call ("format-get-name", type));
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
  SDL_SetStringProperty(props, SDL_PROP_FILE_DIALOG_LOCATION_STRING, SDL_GetPrefPath (tmp1, tmp2));

  // Show the dialog (non-blocking)
  SDL_ShowFileDialogWithProperties (sdl_type,
                                    file_dialog_callback,
                                    (void*)this,  // userdata
                                    props);
  SDL_DestroyProperties(props);
}


//*****************************************************************************
//*****************************************************************************
// Boring auxiliary functions

/******************************************************************************
* Set up keyboard
******************************************************************************/

#ifndef SDLK_ISO_Left_Tab
#define SDLK_ISO_Left_Tab 0xFE20
#endif

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
#if 0
  Map (SDLK_A, "A");
  Map (SDLK_B, "B");
  Map (SDLK_C, "C");
  Map (SDLK_D, "D");
  Map (SDLK_E, "E");
  Map (SDLK_F, "F");
  Map (SDLK_G, "G");
  Map (SDLK_H, "H");
  Map (SDLK_I, "I");
  Map (SDLK_J, "J");
  Map (SDLK_K, "K");
  Map (SDLK_L, "L");
  Map (SDLK_M, "M");
  Map (SDLK_N, "N");
  Map (SDLK_O, "O");
  Map (SDLK_P, "P");
  Map (SDLK_Q, "Q");
  Map (SDLK_R, "R");
  Map (SDLK_S, "S");
  Map (SDLK_T, "T");
  Map (SDLK_U, "U");
  Map (SDLK_V, "V");
  Map (SDLK_W, "W");
  Map (SDLK_X, "X");
  Map (SDLK_Y, "Y");
  Map (SDLK_Z, "Z");
#endif
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

#if 0
  // Cyrillic letters
  Map (SDLK_Cyrillic_a,   "\xe0");
  Map (SDLK_Cyrillic_be,  "\xe1");
  Map (SDLK_Cyrillic_ve,  "\xe2");
  Map (SDLK_Cyrillic_ghe, "\xe3");
  Map (SDLK_Cyrillic_de,  "\xe4");
  Map (SDLK_Cyrillic_ie,  "\xe5");
  Map (SDLK_Cyrillic_io,  "\xbc");
  Map (SDLK_Cyrillic_zhe, "\xe6");
  Map (SDLK_Cyrillic_ze,  "\xe7");
  Map (SDLK_Cyrillic_i,   "\xe8");
  Map (SDLK_Cyrillic_shorti,   "\xe9");
  Map (SDLK_Cyrillic_ka,  "\xea");
  Map (SDLK_Cyrillic_el,  "\xeb");
  Map (SDLK_Cyrillic_em,  "\xec");
  Map (SDLK_Cyrillic_en,  "\xed");
  Map (SDLK_Cyrillic_o,   "\xee");
  Map (SDLK_Cyrillic_pe,  "\xef");
  Map (SDLK_Cyrillic_er,  "\xf0");
  Map (SDLK_Cyrillic_es,  "\xf1");
  Map (SDLK_Cyrillic_te,  "\xf2");
  Map (SDLK_Cyrillic_u,   "\xf3");
  Map (SDLK_Cyrillic_ef,  "\xf4");
  Map (SDLK_Cyrillic_ha,  "\xf5");
  Map (SDLK_Cyrillic_tse, "\xf6");
  Map (SDLK_Cyrillic_che, "\xf7");
  Map (SDLK_Cyrillic_sha, "\xf8");
  Map (SDLK_Cyrillic_shcha,    "\xf9");
  Map (SDLK_Cyrillic_hardsign, "\xfa");
  Map (SDLK_Cyrillic_yeru,     "\xfb");
  Map (SDLK_Cyrillic_softsign, "\xfc");
  Map (SDLK_Cyrillic_e,   "\xfd");
  Map (SDLK_Cyrillic_yu,  "\xfe");
  Map (SDLK_Cyrillic_ya,  "\xff");
  Map (SDLK_Cyrillic_A,   "\xc0");
  Map (SDLK_Cyrillic_BE,  "\xc1");
  Map (SDLK_Cyrillic_VE,  "\xc2");
  Map (SDLK_Cyrillic_GHE, "\xc3");
  Map (SDLK_Cyrillic_DE,  "\xc4");
  Map (SDLK_Cyrillic_IE,  "\xc5");
  Map (SDLK_Cyrillic_IO,  "\x9c");
  Map (SDLK_Cyrillic_ZHE, "\xc6");
  Map (SDLK_Cyrillic_ZE,  "\xc7");
  Map (SDLK_Cyrillic_I,   "\xc8");
  Map (SDLK_Cyrillic_SHORTI,   "\xc9");
  Map (SDLK_Cyrillic_KA,  "\xca");
  Map (SDLK_Cyrillic_EL,  "\xcb");
  Map (SDLK_Cyrillic_EM,  "\xcc");
  Map (SDLK_Cyrillic_EN,  "\xcd");
  Map (SDLK_Cyrillic_O,   "\xce");
  Map (SDLK_Cyrillic_PE,  "\xcf");
  Map (SDLK_Cyrillic_ER,  "\xd0");
  Map (SDLK_Cyrillic_ES,  "\xd1");
  Map (SDLK_Cyrillic_TE,  "\xd2");
  Map (SDLK_Cyrillic_U,   "\xd3");
  Map (SDLK_Cyrillic_EF,  "\xd4");
  Map (SDLK_Cyrillic_HA,  "\xd5");
  Map (SDLK_Cyrillic_TSE, "\xd6");
  Map (SDLK_Cyrillic_CHE, "\xd7");
  Map (SDLK_Cyrillic_SHA, "\xd8");
  Map (SDLK_Cyrillic_SHCHA,    "\xd9");
  Map (SDLK_Cyrillic_HARDSIGN, "\xda");
  Map (SDLK_Cyrillic_YERU,     "\xdb");
  Map (SDLK_Cyrillic_SOFTSIGN, "\xdc");
  Map (SDLK_Cyrillic_E,   "\xdd");
  Map (SDLK_Cyrillic_YU,  "\xde");
  Map (SDLK_Cyrillic_YA,  "\xdf");

  //Ukrainian letters in T2A encoding
  Map (SDLK_Ukrainian_i,   "i"); // Fall back!
  Map (SDLK_Ukrainian_I,   "I"); // Fall back!
  Map (SDLK_Ukrainian_yi,   "\xa8");
  Map (SDLK_Ukrainian_YI,   "\x88");
  Map (SDLK_Ukrainian_ie,   "\xb9");
  Map (SDLK_Ukrainian_IE,   "\x99");
  // Map (SDLK_Ukrainian_ghe_with_upturn,   "\xa0");
  // Map (SDLK_Ukrainian_GHE_WITH_UPTURN,   "\x80");
  Map (0x6ad,   "\xa0");
  Map (0x6bd,   "\x80");
#endif
  
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
  Map (SDLK_LEFTBRACKET, "{");
  Map (SDLK_KP_VERTICALBAR, "|");
  Map (SDLK_RIGHTBRACKET, "}");
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

#if 0
  // Extended symbols and accented characters
  Map (SDLK_nobreakspace, "varspace");
  Map (SDLK_exclamdown, "exclamdown");
  Map (SDLK_cent, "cent");
  Map (SDLK_sterling, "sterling");
  Map (SDLK_currency, "currency");
  Map (SDLK_yen, "yen");
  Map (SDLK_brokenbar, "brokenbar");
  Map (SDLK_section, "section");
  Map (SDLK_diaeresis, "umlaut");
  Map (SDLK_copyright, "copyright");
  Map (SDLK_ordfeminine, "ordfeminine");
  Map (SDLK_guillemotleft, "guillemotleft");
  Map (SDLK_notsign, "notsign");
  Map (SDLK_hyphen, "hyphen");
  Map (SDLK_registered, "registered");
  Map (SDLK_macron, "macron");
  Map (SDLK_degree, "degree");
  Map (SDLK_plusminus, "plusminus");
  Map (SDLK_twosuperior, "twosuperior");
  Map (SDLK_threesuperior, "threesuperior");
  Map (SDLK_acute, "acute");
  Map (SDLK_mu, "mu");
  Map (SDLK_paragraph, "paragraph");
  Map (SDLK_periodcentered, "periodcentered");
  Map (SDLK_cedilla, "cedilla");
  Map (SDLK_onesuperior, "onesuperior");
  Map (SDLK_masculine, "masculine");
  Map (SDLK_guillemotright, "guillemotright");
  Map (SDLK_onequarter, "onequarter");
  Map (SDLK_onehalf, "onehalf");
  Map (SDLK_threequarters, "threequarters");
  Map (SDLK_questiondown, "questiondown");
  Map (SDLK_multiply, "times");
  Map (SDLK_division, "div");

  Map (SDLK_Agrave, "\xc0");
  Map (SDLK_Aacute, "\xc1");
  Map (SDLK_Acircumflex, "\xc2");
  Map (SDLK_Atilde, "\xc3");
  Map (SDLK_Adiaeresis, "\xc4");
  Map (SDLK_Aring, "\xc5");
  Map (SDLK_AE, "\xc6");
  Map (SDLK_Ccedilla, "\xc7");
  Map (SDLK_Egrave, "\xc8");
  Map (SDLK_Eacute, "\xc9");
  Map (SDLK_Ecircumflex, "\xca");
  Map (SDLK_Ediaeresis, "\xcb");
  Map (SDLK_Igrave, "\xcc");
  Map (SDLK_Iacute, "\xcd");
  Map (SDLK_Icircumflex, "\xce");
  Map (SDLK_Idiaeresis, "\xcf");
  Map (SDLK_ETH, "\xd0");
  Map (SDLK_Eth, "\xd0");
  Map (SDLK_Ntilde, "\xd1");
  Map (SDLK_Ograve, "\xd2");
  Map (SDLK_Oacute, "\xd3");
  Map (SDLK_Ocircumflex, "\xd4");
  Map (SDLK_Otilde, "\xd5");
  Map (SDLK_Odiaeresis, "\xd6");
  Map (SDLK_OE, "\xd7");
  Map (SDLK_Ooblique, "\xd8");
  Map (SDLK_Ugrave, "\xd9");
  Map (SDLK_Uacute, "\xda");
  Map (SDLK_Ucircumflex, "\xdb");
  Map (SDLK_Udiaeresis, "\xdc");
  Map (SDLK_Yacute, "\xdd");
  Map (SDLK_THORN, "\xde");
  Map (SDLK_Thorn, "\xde");
  Map (SDLK_ssharp, "sz");
  Map (SDLK_agrave, "\xe0");
  Map (SDLK_aacute, "\xe1");
  Map (SDLK_acircumflex, "\xe2");
  Map (SDLK_atilde, "\xe3");
  Map (SDLK_adiaeresis, "\xe4");
  Map (SDLK_aring, "\xe5");
  Map (SDLK_ae, "\xe6");
  Map (SDLK_ccedilla, "\xe7");
  Map (SDLK_egrave, "\xe8");
  Map (SDLK_eacute, "\xe9");
  Map (SDLK_ecircumflex, "\xea");
  Map (SDLK_ediaeresis, "\xeb");
  Map (SDLK_igrave, "\xec");
  Map (SDLK_iacute, "\xed");
  Map (SDLK_icircumflex, "\xee");
  Map (SDLK_idiaeresis, "\xef");
  Map (SDLK_eth, "\xf0");
  Map (SDLK_ntilde, "\xf1");
  Map (SDLK_ograve, "\xf2");
  Map (SDLK_oacute, "\xf3");
  Map (SDLK_ocircumflex, "\xf4");
  Map (SDLK_otilde, "\xf5");
  Map (SDLK_odiaeresis, "\xf6");
  Map (SDLK_oe, "\xf7");
  Map (SDLK_oslash, "\xf8");
  Map (SDLK_ugrave, "\xf9");
  Map (SDLK_uacute, "\xfa");
  Map (SDLK_ucircumflex, "\xfb");
  Map (SDLK_udiaeresis, "\xfc");
  Map (SDLK_yacute, "\xfd");
  Map (SDLK_thorn, "\xfe");
  Map (SDLK_ydiaeresis, "\xff");

  // Symbols from iso-latin-2
  Map (SDLK_Aogonek, "\x81");
  Map (SDLK_breve, "breve");
  Map (SDLK_Lstroke, "\x8a");
  Map (SDLK_Lcaron, "\x89");
  Map (SDLK_Sacute, "\x91");
  Map (SDLK_Scaron, "\x92");
  Map (SDLK_Scedilla, "\x93");
  Map (SDLK_Tcaron, "\x94");
  Map (SDLK_Zacute, "\x99");
  Map (SDLK_Zcaron, "\x9a");
  Map (SDLK_Zabovedot, "\x9b");
  Map (SDLK_aogonek, "\xa1");
  Map (SDLK_ogonek, "ogonek");
  Map (SDLK_lstroke, "\xaa");
  Map (SDLK_lcaron, "\xa9");
  Map (SDLK_sacute, "\xb1");
  Map (SDLK_caron, "caron");
  Map (SDLK_scaron, "\xb2");
  Map (SDLK_scedilla, "\xb3");
  Map (SDLK_tcaron, "\xb4");
  Map (SDLK_zacute, "\xb9");
  Map (SDLK_doubleacute, "doubleacute");
  Map (SDLK_zcaron, "\xba");
  Map (SDLK_zabovedot, "\xbb");
  Map (SDLK_Racute, "\x8f");
  Map (SDLK_Abreve, "\x80");
  Map (SDLK_Lacute, "\x88");
  Map (SDLK_Cacute, "\x82");
  Map (SDLK_Ccaron, "\x83");
  Map (SDLK_Eogonek, "\x86");
  Map (SDLK_Ecaron, "\x85");
  Map (SDLK_Dcaron, "\x84");
  Map (SDLK_Dstroke, "\xd0");
  Map (SDLK_Nacute, "\x8b");
  Map (SDLK_Ncaron, "\x8c");
  Map (SDLK_Odoubleacute, "\x8e");
  Map (SDLK_Rcaron, "\x90");
  Map (SDLK_Uring, "\x97");
  Map (SDLK_Udoubleacute, "\x96");
  Map (SDLK_Tcedilla, "\x95");
  Map (SDLK_racute, "\xaf");
  Map (SDLK_abreve, "\xa0");
  Map (SDLK_lacute, "\xa8");
  Map (SDLK_cacute, "\xa2");
  Map (SDLK_ccaron, "\xa3");
  Map (SDLK_eogonek, "\xa6");
  Map (SDLK_ecaron, "\xa5");
  Map (SDLK_dcaron, "\xa4");
  Map (SDLK_dstroke, "\x9e");
  Map (SDLK_nacute, "\xab");
  Map (SDLK_ncaron, "\xac");
  Map (SDLK_odoubleacute, "\xae");
  Map (SDLK_udoubleacute, "\xb6");
  Map (SDLK_rcaron, "\xb0");
  Map (SDLK_uring, "\xb7");
  Map (SDLK_tcedilla, "\xb5");
  Map (SDLK_abovedot, "abovedot");
#endif
  
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
  map (SDLK_ISO_Left_Tab, "tab");
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


// SDL3 event logger

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
            SDL_Log("Event: SDL_QUIT");
            break;

        // Keyboard
        case SDL_EVENT_KEY_DOWN:
        case SDL_EVENT_KEY_UP:
            SDL_Log("Event: %s - Key: %s (Scancode: %d, Mod: 0x%x, Repeat: %d)",
                    event->type == SDL_EVENT_KEY_DOWN ? "KEY_DOWN" : "KEY_UP",
                    SDL_GetKeyName(event->key.key),
                    event->key.scancode,
                    event->key.mod,
                    event->key.repeat);
            break;

        // Mouse motion
        case SDL_EVENT_MOUSE_MOTION:
            SDL_Log("Event: MOUSE_MOTION - x: %f, y: %f, xrel: %f, yrel: %f",
                    event->motion.x, event->motion.y,
                    event->motion.xrel, event->motion.yrel);
            break;

        // Mouse buttons
        case SDL_EVENT_MOUSE_BUTTON_DOWN:
        case SDL_EVENT_MOUSE_BUTTON_UP:
            SDL_Log("Event: %s - Button: %d, Clicks: %d, x: %f, y: %f",
                    event->type == SDL_EVENT_MOUSE_BUTTON_DOWN ? "MOUSE_BUTTON_DOWN" : "MOUSE_BUTTON_UP",
                    event->button.button, event->button.clicks,
                    event->button.x, event->button.y);
            break;

        // Mouse wheel
        case SDL_EVENT_MOUSE_WHEEL:
            SDL_Log("Event: MOUSE_WHEEL - x: %f, y: %f, direction: %d",
                    event->wheel.x, event->wheel.y,
                    event->wheel.direction);
            break;

        // Text input
        case SDL_EVENT_TEXT_INPUT:
            SDL_Log("Event: TEXT_INPUT - Text: %s", event->text.text);
            break;

        case SDL_EVENT_TEXT_EDITING:
            SDL_Log("Event: TEXT_EDITING - Text: %s, Start: %d, Length: %d",
                    event->edit.text, event->edit.start, event->edit.length);
            break;

        // Window events

        case SDL_EVENT_WINDOW_SHOWN:
            SDL_Log("Window %u shown", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_HIDDEN:
            SDL_Log("Window %u hidden", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_EXPOSED:
            SDL_Log("Window %u exposed", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_MOVED:
            SDL_Log("Window %u moved to (%d, %d)",
                    event->window.windowID,
                    event->window.data1,
                    event->window.data2);
            break;
        
        case SDL_EVENT_WINDOW_RESIZED:
            SDL_Log("Window %u resized to %dx%d",
                    event->window.windowID,
                    event->window.data1,
                    event->window.data2);
            break;
        
        case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
            SDL_Log("Window %u pixel size changed to %dx%d",
                    event->window.windowID,
                    event->window.data1,
                    event->window.data2);
            break;
        
        case SDL_EVENT_WINDOW_MINIMIZED:
            SDL_Log("Window %u minimized", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_MAXIMIZED:
            SDL_Log("Window %u maximized", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_RESTORED:
            SDL_Log("Window %u restored", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_MOUSE_ENTER:
            SDL_Log("Mouse entered window %u", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_MOUSE_LEAVE:
            SDL_Log("Mouse left window %u", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_FOCUS_GAINED:
            SDL_Log("Window %u gained keyboard focus", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_FOCUS_LOST:
            SDL_Log("Window %u lost keyboard focus", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
            SDL_Log("Window %u close requested", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_HIT_TEST:
            SDL_Log("Window %u hit test event", event->window.windowID);
            break;
        
        case SDL_EVENT_WINDOW_ICCPROF_CHANGED:
            SDL_Log("Window %u ICC profile changed", event->window.windowID);
            break;

        case SDL_EVENT_WINDOW_DISPLAY_CHANGED:
            SDL_Log("Window %u moved to display %d", event->window.windowID, event->window.data1);
            break;

        case SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED:
            SDL_Log("Window %u display scale changed to %d", event->window.windowID, event->window.data1);
            break;

        case SDL_EVENT_WINDOW_SAFE_AREA_CHANGED:
            SDL_Log("Window %u safe area changed", event->window.windowID);
            break;

        case SDL_EVENT_WINDOW_OCCLUDED:
            SDL_Log("Window %u occluded", event->window.windowID);
            break;

        case SDL_EVENT_WINDOW_ENTER_FULLSCREEN:
            SDL_Log("Window %u entered fullscreen", event->window.windowID);
            break;

        case SDL_EVENT_WINDOW_LEAVE_FULLSCREEN:
            SDL_Log("Window %u left fullscreen", event->window.windowID);
            break;

        case SDL_EVENT_WINDOW_DESTROYED:
            SDL_Log("Window %u destroyed", event->window.windowID);
            break;

        case SDL_EVENT_WINDOW_HDR_STATE_CHANGED:
            SDL_Log("Window %u HDR state changed", event->window.windowID);
            break;

        // Game controller (SDL_Gamepad)
        case SDL_EVENT_GAMEPAD_ADDED:
            SDL_Log("Event: GAMEPAD_ADDED - Device Index: %d", event->gdevice.which);
            break;

        case SDL_EVENT_GAMEPAD_REMOVED:
            SDL_Log("Event: GAMEPAD_REMOVED - Instance ID: %d", event->gdevice.which);
            break;

        case SDL_EVENT_GAMEPAD_BUTTON_DOWN:
        case SDL_EVENT_GAMEPAD_BUTTON_UP:
            SDL_Log("Event: %s - Button: %d, Instance ID: %d",
                    event->type == SDL_EVENT_GAMEPAD_BUTTON_DOWN ? "GAMEPAD_BUTTON_DOWN" : "GAMEPAD_BUTTON_UP",
                    event->gbutton.button, event->gbutton.which);
            break;

        case SDL_EVENT_GAMEPAD_AXIS_MOTION:
            SDL_Log("Event: GAMEPAD_AXIS_MOTION - Axis: %d, Value: %d, Instance ID: %d",
                    event->gaxis.axis, event->gaxis.value, event->gaxis.which);
            break;

        // Touch input
        case SDL_EVENT_FINGER_DOWN:
        case SDL_EVENT_FINGER_UP:
        case SDL_EVENT_FINGER_MOTION:
            SDL_Log("Event: %s - FingerID: %" SDL_PRIs64 ", x: %f, y: %f, dx: %f, dy: %f, pressure: %f",
                    event->type == SDL_EVENT_FINGER_DOWN ? "FINGER_DOWN" :
                    event->type == SDL_EVENT_FINGER_UP   ? "FINGER_UP"   : "FINGER_MOTION",
                    event->tfinger.fingerID,
                    event->tfinger.x, event->tfinger.y,
                    event->tfinger.dx, event->tfinger.dy,
                    event->tfinger.pressure);
            break;

        default:
            SDL_Log("Event: %s", SDL_EventTypeToString (event->type));
            break;
    }
}

