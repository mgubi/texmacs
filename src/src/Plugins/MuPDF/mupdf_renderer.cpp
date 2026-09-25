
/******************************************************************************
* MODULE     : mupdf_renderer.cpp
* DESCRIPTION: Raster device with MuPDF
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mupdf_renderer.hpp"
#include "analyze.hpp"
#include "iterator.hpp" // mupdf_image_gc
#include "image_files.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "scheme.hpp"
#include "frame.hpp"

#include "Freetype/tt_file.hpp"
#include "Freetype/free_type.hpp"


#include <mupdf/pdf.h>

#include "mupdf_picture.hpp"

// MuPDF errors and warnings go to the log (an error thrown outside a fz_try
// block still terminates the process, see mupdf_picture.cpp)
static void
mupdf_error_callback (void* user, const char* message) {
  (void) user;
  cout << "TeXmacs] MuPDF error: " << message << LF;
}

static void
mupdf_warning_callback (void* user, const char* message) {
  (void) user;
  cout << "TeXmacs] MuPDF warning: " << message << LF;
}

// manage a single global context for fitz
fz_context*
mupdf_context () {
  static fz_context *ctx= NULL;
  if (!ctx) {
    ctx= fz_new_context (NULL, NULL, FZ_STORE_UNLIMITED);
    fz_set_error_callback (ctx, mupdf_error_callback, NULL);
    fz_set_warning_callback (ctx, mupdf_warning_callback, NULL);
  }
  return ctx;
}

// load a font file, NULL if MuPDF cannot (fz_throw outside fz_try would
// terminate the process)
static fz_font*
mupdf_font_from_file (const char* path) {
  fz_context* ctx= mupdf_context ();
  fz_font* font= NULL;
  fz_var (font);
  fz_try (ctx) {
    font= fz_new_font_from_file (ctx, NULL, path, 0, 0);
  }
  fz_catch (ctx) {
    font= NULL;
    cout << "TeXmacs] MuPDF cannot load font " << path << ": "
         << fz_caught_message (ctx) << LF;
  }
  return font;
}

// global auxiliary document needed to invoke some functions
pdf_document*
mupdf_document () {
  static pdf_document *doc= NULL;
  if (!doc) {
    fz_context *ctx= mupdf_context ();
    fz_var (doc);
    fz_try (ctx) {
      doc= pdf_create_document (ctx);
    }
    fz_catch (ctx) {
      doc= NULL;
      cout << "TeXmacs] MuPDF cannot create the auxiliary document: "
           << fz_caught_message (ctx) << LF;
    }
  }
  return doc;
}


void
snapshot_pixmap (fz_context *ctx, fz_pixmap *pix) {
  static int i=0;
  string str = "/Users/mgubi/snapshot-";
  str << as_string (i) << ".png";
  i = (i+1) % 1000;
  c_string cstr (str);
  fz_output *out;
  fz_pixmap *rgb_pix;
  fz_var (out);
  fz_var (rgb_pix);
  fz_try (ctx) {
    rgb_pix = fz_convert_pixmap(ctx, pix, fz_device_rgb (ctx),
                                                NULL, NULL, fz_default_color_params, 1);
    out= fz_new_output_with_path (ctx, cstr, 0);
    fz_write_pixmap_as_png (ctx, out, rgb_pix);
  }
  fz_always (ctx) {
    fz_drop_pixmap (ctx, rgb_pix);
    fz_close_output (ctx, out);
    fz_drop_output (ctx, out);
  }
  fz_catch (ctx) {
    const char* error_msg = fz_caught_message(ctx);
    cout << "Fitz error in snapshot_pixmap: " << error_msg << LF;
  }
}

/******************************************************************************
* Fitz pixmaps
******************************************************************************/

struct mupdf_pixmap_rep: concrete_struct {
  fz_pixmap *img;
  SI xo,yo;
  int w,h;
  mupdf_pixmap_rep (fz_pixmap* img2, SI xo2, SI yo2, int w2, int h2)
    : img (img2), xo (xo2), yo (yo2), w (w2), h (h2) {
    fz_keep_pixmap (mupdf_context (), img); }
  ~mupdf_pixmap_rep() { fz_drop_pixmap (mupdf_context (), img); }
  friend class mupdf_pixmap;
};

class mupdf_pixmap {
  CONCRETE_NULL (mupdf_pixmap);
  mupdf_pixmap (fz_pixmap* img2, SI xo2, SI yo2, int w2, int h2):
    rep (tm_new<mupdf_pixmap_rep> (img2, xo2, yo2, w2, h2)) {}
};

CONCRETE_NULL_CODE (mupdf_pixmap);

/******************************************************************************
* Fitz images
******************************************************************************/

struct mupdf_image_rep: concrete_struct {
  int w, h, xo, yo;
  fz_image *img;
  mupdf_image_rep (fz_image* img2)
    : w (0), h (0), xo (0), yo (0), img (img2) {
    if (img == NULL) return; // an image which could not be loaded
    fz_keep_image (mupdf_context (), img);
    // get pixmap size
    fz_pixmap *pix= mupdf_pixmap_from_image (img);
    if (pix != NULL) {
      w= fz_pixmap_width (mupdf_context (), pix);
      h= fz_pixmap_height (mupdf_context (), pix);
      fz_drop_pixmap (mupdf_context (), pix);
    }
  }
  ~mupdf_image_rep() { fz_drop_image (mupdf_context (), img); }
  friend class mupdf_image;
};

class mupdf_image {
  CONCRETE_NULL (mupdf_image);
  mupdf_image (fz_image* img2):
    rep (tm_new<mupdf_image_rep> (img2)) {}
};

CONCRETE_NULL_CODE (mupdf_image);

/******************************************************************************
* pdf patterns
******************************************************************************/

struct mupdf_pattern_rep: concrete_struct {
  pdf_pattern *pat;
  mupdf_pattern_rep (pdf_pattern* _pat)
    : pat (_pat) {
    pdf_keep_pattern (mupdf_context (), pat);
  }
  ~mupdf_pattern_rep() { pdf_drop_pattern (mupdf_context (), pat); }
  friend class mupdf_pattern;
};

class mupdf_pattern {
  CONCRETE_NULL (mupdf_pattern);
  mupdf_pattern (pdf_pattern* _pat):
    rep (tm_new<mupdf_pattern_rep> (_pat)) {}
};

CONCRETE_NULL_CODE (mupdf_pattern);

/******************************************************************************
* pdf figures, drawn as drawing
******************************************************************************/

// A PDF figure as a form XObject. Each lives in the document it was read
// from -- read into memory, so that no file stays open -- and the form is
// made there, around the contents and the resources of the first page;
// dropping the form drops the document, and the memory with it (a form
// grafted into the one auxiliary document would stay there for good).
struct mupdf_form_rep: concrete_struct {
  pdf_document *doc;
  pdf_obj *xo;
  mupdf_form_rep (pdf_document* doc2, pdf_obj* xo2): doc (doc2), xo (xo2) {}
  ~mupdf_form_rep () {
    pdf_drop_obj (mupdf_context (), xo);
    pdf_drop_document (mupdf_context (), doc);
  }
  friend class mupdf_form;
};

class mupdf_form {
  CONCRETE_NULL (mupdf_form);
  mupdf_form (pdf_document* doc2, pdf_obj* xo2):
    rep (tm_new<mupdf_form_rep> (doc2, xo2)) {}
};

CONCRETE_NULL_CODE (mupdf_form);

/******************************************************************************
* pdf fonts
******************************************************************************/

struct mupdf_font_rep: concrete_struct {
  pdf_font_desc *fn;
  mupdf_font_rep (pdf_font_desc* _fn)
    : fn (_fn) {
    pdf_keep_font (mupdf_context (), fn);
  }
  ~mupdf_font_rep() { pdf_drop_font (mupdf_context (), fn); }
  friend class mupdf_font;
};

class mupdf_font {
  CONCRETE_NULL (mupdf_font);
  mupdf_font (pdf_font_desc* _fn):
    rep (tm_new<mupdf_font_rep> (_fn)) {}
};

CONCRETE_NULL_CODE (mupdf_font);

/******************************************************************************
* Global support variables for all mupdf_renderers
******************************************************************************/

// bitmaps of all characters
static hashmap<basic_character, mupdf_image> character_image;

// caches
static hashmap<unsigned long long int, mupdf_image> picture_pool;
static hashmap<tree, mupdf_image>  image_pool;
static hashmap<tree, mupdf_pattern> pattern_pool;
static hashmap<tree, mupdf_image> pattern_image_pool;
static hashmap<tree, mupdf_form> form_pool; // nil: MuPDF cannot read it
static hashmap<string, mupdf_font> native_fonts;

// Garbage collect the cached images whose name matches (image_gc in
// gui.hpp; the keys of the pools are tuples whose first element is the
// name of the file). "*" flushes everything.
void mupdf_image_gc (string name) {
  if (name == "*" || name == "") {
    image_pool= hashmap<tree, mupdf_image> ();
    pattern_pool= hashmap<tree, mupdf_pattern> ();
    pattern_image_pool= hashmap<tree, mupdf_image> ();
    form_pool= hashmap<tree, mupdf_form> ();
    return;
  }
  array<tree> gone;
  iterator<tree> it= iterate (image_pool);
  while (it->busy ()) {
    tree key= it->next ();
    if (N(key) > 0 && is_atomic (key[0]) && occurs (name, key[0]->label))
      gone << key;
  }
  for (int i= 0; i < N(gone); i++) image_pool->reset (gone[i]);
  gone= array<tree> ();
  it= iterate (pattern_image_pool);
  while (it->busy ()) {
    tree key= it->next ();
    if (N(key) > 0 && is_atomic (key[0]) && occurs (name, key[0]->label))
      gone << key;
  }
  for (int i= 0; i < N(gone); i++) pattern_image_pool->reset (gone[i]);
  gone= array<tree> ();
  it= iterate (form_pool);
  while (it->busy ()) {
    tree key= it->next ();
    if (N(key) > 0 && is_atomic (key[0]) && occurs (name, key[0]->label))
      gone << key;
  }
  for (int i= 0; i < N(gone); i++) form_pool->reset (gone[i]);
}

// flush caches
void del_obj_mupdf_renderer (void)  {
  character_image= hashmap<basic_character, mupdf_image> ();
  image_pool=  hashmap<tree, mupdf_image> ();
  picture_pool= hashmap<unsigned long long int, mupdf_image> ();
  pattern_pool= hashmap<tree, mupdf_pattern> ();
  pattern_image_pool= hashmap<tree, mupdf_image> ();
  form_pool= hashmap<tree, mupdf_form> ();
  native_fonts= hashmap<string, mupdf_font> ();
}

/******************************************************************************
* mupdf_renderer
******************************************************************************/

mupdf_renderer_rep::mupdf_renderer_rep (int w2, int h2)
  : basic_renderer_rep (true, 1.0, w2, h2),
    pixmap (NULL), dev (NULL), proc (NULL),
    fg (-1), bg (-1),
    lw (-1), clip_level (0), fill_is_pattern (false),
    in_text (false), cfn ("")
{
  reset_zoom_factor();
}

mupdf_renderer_rep::~mupdf_renderer_rep () {
  end ();
}

void*
mupdf_renderer_rep::get_handle () {
  return (void*) this;
}

void
mupdf_renderer_rep::get_extents (SI& w2, SI& h2) {
  if (pixmap) {
    w2= fz_pixmap_width (mupdf_context (), pixmap);
    h2= fz_pixmap_height (mupdf_context (), pixmap);
  } else {
    w2 = w; h2 = h;
  }
}

void
mupdf_renderer_rep::set_zoom_factor (double zoom, bool safe) {
  // the retina factor is applied here, not through pixel_ratio: the
  // consistency check of the base class does not apply
  (void) safe;
  renderer_rep::set_zoom_factor (retina_factor * zoom, false);
  retina_pixel= pixel * retina_factor;
}

void
mupdf_renderer_rep::begin (void* handle) {
  fz_pixmap *_pixmap= static_cast<fz_pixmap*>(handle);
  if (_pixmap) {
    fz_context *ctx= mupdf_context ();
    if (dev) end ();
    pixmap= _pixmap;
    fz_keep_pixmap (ctx, pixmap);
    w= fz_pixmap_width (ctx, pixmap);
    h= fz_pixmap_height (ctx, pixmap);
    dev= NULL; proc= NULL;
    fz_matrix ctm= fz_make_matrix(1, 0, 0, -1, 0, 0);
    bool ok= mupdf_protected ("mupdf_renderer_rep::begin", [&] () {
      dev= fz_new_draw_device (ctx, fz_identity, pixmap);
      proc= pdf_new_run_processor (ctx, mupdf_document (), dev, ctm, -1, "View", NULL, NULL, NULL, NULL, NULL);
    });
    if (!ok) {
      // the drawing operators need a processor: draw into a 1x1 dummy
      // pixmap instead (if even this fails we are out of memory)
      if (dev != NULL) { fz_drop_device (ctx, dev); dev= NULL; }
      fz_drop_pixmap (ctx, pixmap);
      pixmap= mupdf_new_pixmap (1, 1);
      w= h= 1;
      dev= fz_new_draw_device (ctx, fz_identity, pixmap);
      proc= pdf_new_run_processor (ctx, mupdf_document (), dev, ctm, -1, "View", NULL, NULL, NULL, NULL, NULL);
    }
    
    fg  = -1;
    bg  = -1;
    lw  = -1;
    current_width = -1.0;
    cfn= "";
    in_text = false;
    clip_level = 0;
    fill_is_pattern = false;
    
    // outmost save of the graphics state
    proc->op_q (mupdf_context (), proc);
    // set scaling suitable for dpi (pdf default is 72)
    proc->op_cm (mupdf_context (), proc, 1, 0, 0, 1, 0, 0);

    //set_origin(0, -500);
    //set_origin (0, h*pixel);
    //set_clipping (0, (int) (-h*pixel), (int) (w*pixel), 0);
  } else {
    debug_std << "mupdf_renderer_rep::begin : invalid pixmap" << LF;
  }
}

void
mupdf_renderer_rep::end () {
  end_text ();

  if (proc) {
    // reset set_clipping calls in order to have well formed PDF.
    while (clip_level--)
      proc->op_Q (mupdf_context (), proc);
    // outmost restore for the graphics state (see begin_page)
    proc->op_Q (mupdf_context (), proc);

    pdf_close_processor (mupdf_context (), proc);
    pdf_drop_processor (mupdf_context (), proc);
    proc= NULL;
  }
  if (dev) {
    fz_close_device (mupdf_context (), dev);
    fz_drop_device (mupdf_context (), dev);
    dev= NULL;
  }
  if (pixmap) {
    fz_drop_pixmap (mupdf_context (), pixmap);
    pixmap= NULL;
  }
}

void
mupdf_renderer_rep::begin_text () {
  if (!in_text) {
    in_text= true;
    prev_text_x= to_x(0);
    prev_text_y= to_y(0);
    proc->op_BT (mupdf_context (), proc);
    proc->op_Tm (mupdf_context (), proc, 1, 0, 0, 1, prev_text_x, prev_text_y);
  }
}

void
mupdf_renderer_rep::end_text () {
  if (in_text) {
    in_text= false;
    proc->op_ET (mupdf_context (), proc);
  }
}

/******************************************************************************
* Transformations
******************************************************************************/

void
mupdf_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");

  end_text ();

  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);

  frame cv= scaling (point (pixel, -pixel), point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  //cout << "Set transformation " << o << ", " << ux << ", " << uy << "\n";

  proc->op_q (mupdf_context (), proc);
  proc->op_cm (mupdf_context (), proc, ux[0], ux[1], uy[0], uy[1], o[0], o[1]);

  rectangle nclip= fr [oclip];
  clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
mupdf_renderer_rep::reset_transformation () {
  unclip ();
  proc->op_Q (mupdf_context (), proc);
}

/******************************************************************************
* Clipping
******************************************************************************/

void
mupdf_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);

  end_text();
  
  outer_round (x1, y1, x2, y2);
  if (restore) {
    // debug_convert << "restore clipping\n";
    if (clip_level > 0) {
      proc->op_Q (mupdf_context (), proc);
      clip_level--;
    }
    cfn= "";
  }
  else {
    // debug_convert << "set clipping\n";
    proc->op_q (mupdf_context (), proc);
    clip_level++;
    float xx1= to_x (min (x1, x2));
    float yy1= to_y (min (y1, y2));
    float xx2= to_x (max (x1, x2));
    float yy2= to_y (max (y1, y2));
    proc->op_re (mupdf_context (), proc, xx1, yy1, xx2-xx1, yy2-yy1);
    proc->op_W (mupdf_context (), proc);
    proc->op_n (mupdf_context (), proc);
  }
}

/******************************************************************************
 * Graphic state management
 ******************************************************************************/

void
mupdf_renderer_rep::select_alpha (int alpha) {
  float da = ((float) alpha)/1000.0;
  proc->op_gs_ca (mupdf_context (), proc, da);
  proc->op_gs_CA (mupdf_context (), proc, da);
}

void
mupdf_renderer_rep::select_stroke_color (color c) {;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r= ((r*1000)/255);
  g= ((g*1000)/255);
  b= ((b*1000)/255);
  a= ((a*1000)/255);
  float dr= ((float) r) / 1000.0;
  float dg= ((float) g) / 1000.0;
  float db= ((float) b) / 1000.0;
  proc->op_RG (mupdf_context (), proc, dr, dg, db); // stroke color
  select_alpha (a);
}

void
mupdf_renderer_rep::select_fill_color (color c) {;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r= ((r*1000)/255);
  g= ((g*1000)/255);
  b= ((b*1000)/255);
  a= ((a*1000)/255);
  float dr= ((float) r) / 1000.0;
  float dg= ((float) g) / 1000.0;
  float db= ((float) b) / 1000.0;
  proc->op_rg (mupdf_context (), proc, dr, dg, db); // non-stroking color
  select_alpha (a);
  fill_is_pattern= false;
}

static mupdf_image
get_image (url u, int w, int h, tree eff, SI pixel) {
  mupdf_image mpim= mupdf_image ();
  fz_pixmap *pix= mupdf_load_pixmap (u, w, h, eff, pixel);
  if (pix) {
    fz_image *im= mupdf_image_from_pixmap (pix);
    fz_drop_pixmap (mupdf_context (), pix);
    if (im != NULL) mpim= mupdf_image (im);
  }
  return mpim;
}

// A pdf_pattern of ours is reference counted like one MuPDF loads, and
// dropped the same way (pdf_drop_pattern_imp is not exported)
static void
drop_pattern_imp (fz_context *ctx, fz_storable *s) {
  pdf_pattern *pat= (pdf_pattern *) s;
  pdf_drop_obj (ctx, pat->resources);
  pdf_drop_obj (ctx, pat->contents);
  fz_free (ctx, pat);
}

static pdf_pattern*
new_pattern (fz_context *ctx, pdf_document *doc, float w, float h,
             pdf_obj *resources, pdf_obj *contents, fz_matrix m) {
  pdf_pattern *pat= fz_malloc_struct (ctx, pdf_pattern);
  FZ_INIT_STORABLE (pat, 1, drop_pattern_imp);
  pat->document= doc;
  pat->id= 0; // no id: no cached tiles, which would not know the phase
  pat->ismask= 0;
  pat->xstep= w;
  pat->ystep= h;
  pat->bbox= fz_make_rect (0, 0, w, h);
  pat->matrix= m;
  pat->resources= pdf_keep_obj (ctx, resources);
  pat->contents= pdf_keep_obj (ctx, contents);
  return pat;
}

// The pattern p placed for this renderer: its tiles have a corner at the
// origin of the document, as the Qt port places them (decode (0, 0)), so
// that the pattern moves with what it fills when the view scrolls -- and
// a strip repainted after a scroll shift meets the part which was moved
// without a seam. A pattern lives in the default space of the processor
// (the matrix of begin), which is the space of to_x and to_y. The one in
// the pool is the unplaced original; the placed one is made for each use
// and belongs to the caller.
static pdf_pattern*
placed_pattern (fz_context *ctx, pdf_pattern *t, double ox, double oy) {
  return new_pattern (ctx, t->document, t->xstep, t->ystep,
                      t->resources, t->contents,
                      fz_make_matrix (1, 0, 0, 1, (float) ox, (float) oy));
}

void
mupdf_renderer_rep::register_pattern (brush br, SI pixel) {
  // debug_convert << "register_pattern_image\n";
  if (is_nil (br) || br->get_type () != brush_pattern) {
    convert_warning << "mupdf_renderer_rep::register_pattern_image: "
                    << "brush with pattern expected\n";
    return;
  }
  tree p= br->get_pattern ();
  // debug_convert << p << "\n";
  if (pattern_pool->contains(p)) return;

  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  tree key= tuple (u->t, as_string (w), as_string (h), eff);
  
  mupdf_image image_pdf;
  if (pattern_image_pool->contains (key))
    image_pdf= pattern_image_pool [key];
  else {
    // debug_convert << "Insert pattern image\n";
    image_pdf= get_image (u, w, h, eff, pixel);
    if (is_nil (image_pdf)) {
      convert_error << "mupdf_renderer_rep::register_pattern : Cannot read image file '" << u << "'"
        << " with get_image" << LF;
      return;
    }
    if (w != image_pdf->w || h != image_pdf->h) {
      convert_error << "mupdf_renderer_rep::register_pattern : Invalid image size '" << u << "'"
        << " after get_image" << LF;
      return;
    }
    pattern_image_pool(key) = image_pdf;
  }

  fz_context *ctx= mupdf_context ();
  pdf_document *doc= mupdf_document ();
  pdf_obj *subres= NULL;
  pdf_obj *contents= NULL;
  fz_buffer *buf= NULL;
  bool ok= mupdf_protected ("mupdf_renderer_rep::register_pattern", [&] () {
    // the resources of the pattern: /Resources << /XObject << /pattern-image ref >> >>
    subres= pdf_new_dict (ctx, doc, 1);
    pdf_obj *xobjs= pdf_new_dict (ctx, doc, 1);
    pdf_obj *ref= pdf_add_image (ctx, doc, image_pdf->img);
    pdf_dict_puts (ctx, xobjs, "pattern-image", ref);
    pdf_dict_puts (ctx, subres, "XObject", xobjs);
    pdf_drop_obj (ctx, ref);
    pdf_drop_obj (ctx, xobjs);
    buf= fz_new_buffer (ctx, 0);
    pdf_processor *pout= pdf_new_buffer_processor (ctx, buf, 0, 0);
    pout->op_q (ctx, pout);
    pout->op_cm (ctx, pout, w, 0, 0, h, 0, 0);
    pout->op_Do_image (ctx, pout, "pattern-image", NULL);
    pout->op_Q (ctx, pout);
    pdf_close_processor (ctx, pout);
    pdf_drop_processor (ctx, pout);
    contents= pdf_add_stream (ctx, doc, buf, NULL /* dict */, 0 /* compress */);
  });
  if (buf != NULL) fz_drop_buffer (ctx, buf);
  if (!ok) {
    if (subres != NULL) pdf_drop_obj (ctx, subres);
    if (contents != NULL) pdf_drop_obj (ctx, contents);
    return; // the pattern stays unregistered: the callers fall back
  }
  {
    pdf_pattern *pat= NULL;
    mupdf_protected ("mupdf_renderer_rep::register_pattern", [&] () {
      pat= new_pattern (ctx, doc, w, h, subres, contents, fz_identity);
    });
    pdf_drop_obj (ctx, subres);
    pdf_drop_obj (ctx, contents);
    if (pat == NULL) return;
    mupdf_pattern p_pdf (pat);
    pdf_drop_pattern (ctx, pat);
    pattern_pool (p) = p_pdf;
  }
}

void
mupdf_renderer_rep::select_stroke_pattern (brush br) {
  if (is_nil(br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern (br, brushpx == -1 ? pixel : brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "mupdf_renderer_rep::select_stroke_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  mupdf_pattern p= pattern_pool [p_tree];
  fz_context *ctx= mupdf_context ();
  pdf_pattern *pat= NULL;
  mupdf_protected ("select_stroke_pattern", [&] () {
    pat= placed_pattern (ctx, p->pat, to_x (0), to_y (0));
    proc->op_CS (ctx, proc, "Pattern", fz_device_rgb (ctx));
    proc->op_SC_pattern (ctx, proc, "*stroke-pattern*", pat, 0, NULL);
  });
  pdf_drop_pattern (ctx, pat); // the processor keeps its own
}

void
mupdf_renderer_rep::select_fill_pattern (brush br) {
  if (is_nil(br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern (br, brushpx==-1? pixel: brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "mupdf_renderer_rep::select_fill_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  mupdf_pattern p= pattern_pool [p_tree];
  fill_is_pattern= true;
  fz_context *ctx= mupdf_context ();
  pdf_pattern *pat= NULL;
  mupdf_protected ("select_fill_pattern", [&] () {
    pat= placed_pattern (ctx, p->pat, to_x (0), to_y (0));
    proc->op_CS (ctx, proc, "Pattern", fz_device_rgb (ctx));
    proc->op_sc_pattern (ctx, proc, "*fill-pattern*", pat, 0, NULL);
  });
  pdf_drop_pattern (ctx, pat); // the processor keeps its own
  select_alpha ((1000*br->get_alpha ())/255);
}

void
mupdf_renderer_rep::select_line_width (SI w) {
  float pw = w /pixel;
  //if (pw < 1) pw= 1;
  if (pw != current_width) {
    proc->op_w (mupdf_context (), proc, pw);
    current_width = pw;
  }
}

void
mupdf_renderer_rep::set_pencil (pencil pen2) {
  // debug_convert << "set_pencil\n";
  pen= pen2;
  lw= pen->get_width ();
  select_line_width (lw);
  color c= pen->get_color ();
  fg= c;
  select_fill_color (c);
  select_stroke_color (c);
  if (pen->get_type () == pencil_brush) {
    // debug_convert << "pencil has brush type" << LF;
    brush br= pen->get_brush ();
    fg_brush= br;
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  if (pen->get_cap () == cap_round)
    proc->op_J (mupdf_context (), proc, 1); // round cap
  else
    proc->op_J (mupdf_context (), proc, 2); // square cap
  proc->op_j (mupdf_context (), proc, 1); // round join
}

void
mupdf_renderer_rep::set_brush (brush br) {
  // debug_convert << "set_brush\n";
  fg_brush= br;
  pen= pencil (br);
  set_pencil (pen);  // FIXME ???
  if (is_nil (br)) return;
  if (br->get_type () == brush_none) {
    pen = pencil ();
    fg_brush = brush ();
  }
  else {
    select_fill_color (pen->get_color ());
    select_stroke_color (pen->get_color ());
  }
  if (br->get_type () == brush_pattern) {
    tree p_tree= br->get_pattern ();
    register_pattern (br, brushpx == -1 ? pixel : brushpx);
    if (!pattern_pool->contains (p_tree)) {
      convert_error << "mupdf_renderer_rep::set_brush: "
        << "cannot find registered pattern\n";
      return;
    }
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  //select_alpha (br->get_alpha ());
}
void
mupdf_renderer_rep::clear_device (SI x1, SI y1, SI x2, SI y2) {
  // the neutral pattern around the pages, as in the Qt port: white, then
  // the pattern image tiled at its natural size
  static brush neutral;
  static bool resolved= false;
  if (!resolved) {
    resolved= true;
    url u= resolve_pattern (url ("neutral-pattern.png"));
    if (!is_none (u))
      neutral= brush (compound ("pattern", as_string (u), "", ""), 255);
  }
  end_text ();
  float xx1= to_x (min (x1, x2));
  float yy1= to_y (min (y1, y2));
  float xx2= to_x (max (x1, x2));
  float yy2= to_y (max (y1, y2));
  bool cleared= fill_direct (x1, y1, x2, y2, white);
  proc->op_q (mupdf_context (), proc);
  if (!cleared) {
    select_fill_color (white);
    proc->op_re (mupdf_context (), proc, xx1, yy1, xx2-xx1, yy2-yy1);
    proc->op_f (mupdf_context (), proc);
  }
  if (!is_nil (neutral)) {
    select_fill_pattern (neutral);
    proc->op_re (mupdf_context (), proc, xx1, yy1, xx2-xx1, yy2-yy1);
    proc->op_f (mupdf_context (), proc);
  }
  select_fill_color (fg);
  select_fill_pattern (fg_brush);
  proc->op_Q (mupdf_context (), proc);
}

void
mupdf_renderer_rep::set_background (brush b) {
  // debug_convert << "set_background\n";
  bg_brush= b;
  bg= b->get_color ();
}

/******************************************************************************
 * Graphics primitives
 ******************************************************************************/

void
mupdf_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "line\n";
  end_text ();
  proc->op_m (mupdf_context (), proc, to_x (x1), to_y (y1));
  proc->op_l (mupdf_context (), proc, to_x (x2), to_y (y2));
  proc->op_S (mupdf_context (), proc);
}

void
mupdf_renderer_rep::lines (array<SI> x, array<SI> y) {
  // debug_convert << "lines\n";
  end_text ();
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  end_text ();
  proc->op_q (mupdf_context (), proc);
  if (pen->get_cap () == cap_round ||
      (x[N(x)-1] == x[0] && y[N(y)-1] == y[0]))
    proc->op_J (mupdf_context (), proc, 1); // round cap
  else
    proc->op_J (mupdf_context (), proc, 2); // square cap
  proc->op_j (mupdf_context (), proc, 1); // round join
  proc->op_m (mupdf_context (), proc, to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++) {
    proc->op_l (mupdf_context (), proc, to_x (x[i]), to_y (y[i]));
  }
  proc->op_S (mupdf_context (), proc);
  proc->op_Q (mupdf_context (), proc);
}

void
mupdf_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  if ((is_nil (bg_brush) || bg_brush->get_type () != brush_pattern) &&
      fill_direct (x1, y1, x2, y2, bg)) return; // a plain background color
  end_text ();
  float xx1= to_x (min (x1, x2));
  float yy1= to_y (min (y1, y2));
  float xx2= to_x (max (x1, x2));
  float yy2= to_y (max (y1, y2));
  // debug_convert << "clear" << xx1 << " " << yy1 << " " << xx2 << " " << yy2 << LF;
  proc->op_q (mupdf_context (), proc);
  select_fill_color (bg);
  select_fill_pattern (bg_brush);
  proc->op_re (mupdf_context (), proc, xx1, yy1, xx2-xx1, yy2-yy1);
  proc->op_f (mupdf_context (), proc);
  select_fill_color (fg);
  select_fill_pattern (fg_brush);
  proc->op_Q (mupdf_context (), proc);
}

/******************************************************************************
 * Direct pixel access
 *
 * The filled rectangles of the GUI and the blits of the backing stores of
 * the editors make up most of a frame of the Vue GUI. Drawn through the PDF
 * processor they are rasterized as paths and painted as images (with a
 * colorspace conversion, the window surface being BGR), which took most of
 * the frame time while scrolling. Axis-aligned boxes land on integer device
 * pixels (to_x/to_y divide SI by the pixel size), so they are written into
 * the pixmap directly, with the same result: solid or translucent colors
 * (source-over, premultiplied alpha as in MuPDF's pixmaps) within the
 * current clip, as long as the fill is not a pattern. Everything else
 * (rounded corners, arcs, text, patterns) still goes through MuPDF.
 ******************************************************************************/

// the device pixels [px1, px2) x [py1, py2) (y down) covered by the SI box,
// intersected with the current clip and with the pixmap; false if empty
bool
mupdf_renderer_rep::device_box (SI x1, SI y1, SI x2, SI y2,
                                int& px1, int& py1, int& px2, int& py2) {
  if (pixmap == NULL || pixmap->samples == NULL) return false;
  if (x1 > x2) { SI t= x1; x1= x2; x2= t; }
  if (y1 > y2) { SI t= y1; y1= y2; y2= t; }
  // to_x/to_y map SI (y up) to PDF points at integer positions, the device
  // is upside down (see the ctm in begin)
  px1= (int) to_x (x1); px2= (int) to_x (x2);
  py1= (int) -to_y (y2); py2= (int) -to_y (y1);
  if (clip_level > 0) {
    // the clip of the PDF state was set from the same SI coordinates
    SI ax1, ay1, ax2, ay2;
    get_clipping (ax1, ay1, ax2, ay2);
    px1= max (px1, (int) to_x (ax1)); px2= min (px2, (int) to_x (ax2));
    py1= max (py1, (int) -to_y (ay2)); py2= min (py2, (int) -to_y (ay1));
  }
  px1= max (px1, 0); py1= max (py1, 0);
  px2= min (px2, pixmap->w); py2= min (py2, pixmap->h);
  return px1 < px2 && py1 < py2;
}

// fill the box with a plain color; false if MuPDF must do it
bool
mupdf_renderer_rep::fill_direct (SI x1, SI y1, SI x2, SI y2, color c) {
  if (pixmap == NULL) return false;
  if (pixmap->n != 4 || pixmap->s != 0 || !pixmap->alpha) return false;
  fz_context* ctx= mupdf_context ();
  bool bgr= (pixmap->colorspace == fz_device_bgr (ctx));
  if (!bgr && pixmap->colorspace != fz_device_rgb (ctx)) return false;
  int px1, py1, px2, py2;
  end_text ();
  if (!device_box (x1, y1, x2, y2, px1, py1, px2, py2)) return true; // clipped away
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  if (bgr) { int t= r; r= b; b= t; }
  if (a <= 0) return true;
  unsigned char* row= pixmap->samples + (ptrdiff_t) py1 * pixmap->stride + 4 * px1;
  int n= px2 - px1;
  if (a >= 255) {
    unsigned char c[4]= { (unsigned char) r, (unsigned char) g, (unsigned char) b, 255 };
    for (int py= py1; py < py2; py++, row += pixmap->stride) {
      unsigned char* d= row;
      for (int i= 0; i < n; i++, d += 4) { d[0]= c[0]; d[1]= c[1]; d[2]= c[2]; d[3]= c[3]; }
    }
  }
  else {
    // source-over with a premultiplied source color
    int sr= r*a/255, sg= g*a/255, sb= b*a/255, ia= 255 - a;
    for (int py= py1; py < py2; py++, row += pixmap->stride) {
      unsigned char* d= row;
      for (int i= 0; i < n; i++, d += 4) {
        d[0]= (unsigned char) (sr + (d[0]*ia)/255);
        d[1]= (unsigned char) (sg + (d[1]*ia)/255);
        d[2]= (unsigned char) (sb + (d[2]*ia)/255);
        d[3]= (unsigned char) (a  + (d[3]*ia)/255);
      }
    }
  }
  return true;
}

// blit the pixmap with its bottom left corner at (x, y) (SI), 1 device
// pixel per pixel of the source, composed with the given alpha; false if
// MuPDF must do it (other formats). opaque says that every pixel of the
// source has an alpha of 255, which the caller knows and we cannot afford
// to check: reading the source to find out costs as much as the blit.
bool
mupdf_renderer_rep::draw_pixmap_direct (fz_pixmap* src, SI x, SI y, int alpha,
                                       bool opaque) {
  if (src == NULL || src->samples == NULL || pixmap == NULL) return false;
  if (pixmap->n != 4 || pixmap->s != 0 || !pixmap->alpha) return false;
  if (src->s != 0 || (!(src->n == 4 && src->alpha) && !(src->n == 3 && !src->alpha)))
    return false;
  fz_context* ctx= mupdf_context ();
  bool dst_bgr= (pixmap->colorspace == fz_device_bgr (ctx));
  bool src_bgr= (src->colorspace == fz_device_bgr (ctx));
  if (!dst_bgr && pixmap->colorspace != fz_device_rgb (ctx)) return false;
  if (!src_bgr && src->colorspace != fz_device_rgb (ctx)) return false;
  if (alpha <= 0) return true;
  end_text ();
  // the box of the image in device pixels, then the visible part of it
  int ix1= (int) to_x (x), iy2= (int) -to_y (y);
  int ix2= ix1 + src->w, iy1= iy2 - src->h;
  int px1, py1, px2, py2;
  if (!device_box (x, y, x + src->w * pixel, y + src->h * pixel, px1, py1, px2, py2)) return true;
  px1= max (px1, ix1); px2= min (px2, ix2);
  py1= max (py1, iy1); py2= min (py2, iy2);
  if (px1 >= px2 || py1 >= py2) return true;
  bool swap_rb= (dst_bgr != src_bgr);
  int sn= src->n, n= px2 - px1;
  const unsigned char* srow= src->samples + (ptrdiff_t) (py1 - iy1) * src->stride + sn * (px1 - ix1);
  unsigned char* drow= pixmap->samples + (ptrdiff_t) py1 * pixmap->stride + 4 * px1;
  // Nothing to compose and nothing to decide per pixel: the rows are copied
  // as they are, or reordered with a loop the compiler can vectorise. This
  // is the blit of an editor's backing store, which is opaque throughout
  // (see native_opaque_picture); it is an order of magnitude faster than
  // the general loop below, whose cost is the test on the alpha rather than
  // the reordering.
  if (opaque && alpha >= 255 && sn == 4) {
    for (int py= py1; py < py2; py++, srow += src->stride, drow += pixmap->stride) {
      if (!swap_rb) memcpy (drow, srow, (size_t) n * 4);
      else {
        const unsigned char* sp= srow;
        unsigned char* d= drow;
        for (int i= 0; i < n; i++, sp += 4, d += 4) {
          d[0]= sp[2]; d[1]= sp[1]; d[2]= sp[0]; d[3]= 255;
        }
      }
    }
    return true;
  }
  for (int py= py1; py < py2; py++, srow += src->stride, drow += pixmap->stride) {
    const unsigned char* sp= srow;
    unsigned char* d= drow;
    for (int i= 0; i < n; i++, sp += sn, d += 4) {
      int sr= sp[0], sg= sp[1], sb= sp[2], sa= (sn == 4) ? sp[3] : 255;
      if (swap_rb) { int t= sr; sr= sb; sb= t; }
      if (alpha < 255) { sr= sr*alpha/255; sg= sg*alpha/255; sb= sb*alpha/255; sa= sa*alpha/255; }
      if (sa >= 255) { d[0]= sr; d[1]= sg; d[2]= sb; d[3]= 255; }
      else if (sa > 0) {
        int ia= 255 - sa;
        d[0]= (unsigned char) (sr + (d[0]*ia)/255);
        d[1]= (unsigned char) (sg + (d[1]*ia)/255);
        d[2]= (unsigned char) (sb + (d[2]*ia)/255);
        d[3]= (unsigned char) (sa + (d[3]*ia)/255);
      }
    }
  }
  return true;
}

void
mupdf_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if ((x1<x2) && (y1<y2))
  {
    if (!fill_is_pattern && fill_direct (x1, y1, x2, y2, fg)) return;
    end_text ();
    float xx1= to_x (min (x1, x2));
    float yy1= to_y (min (y1, y2));
    float xx2= to_x (max (x1, x2));
    float yy2= to_y (max (y1, y2));
    proc->op_re (mupdf_context (), proc, xx1, yy1, xx2-xx1, yy2-yy1);
    proc->op_f (mupdf_context (), proc); // FIXME Winding
  }
}

void
mupdf_renderer_rep::bezier_arc (SI x1, SI y1, SI x2, SI y2,
                                int alpha, int delta, bool filled)
{
  // PDF can describe only cubic bezier paths, so we have to make up the arc
  // with them. Since this is not mathematically exact, we minimize errors by
  // drawing beziers sub-arcs of at most 90??
  end_text ();
  proc->op_q (mupdf_context (), proc); // save graphics state

  float xx1 = to_x(x1), yy1 = to_y(y1), xx2 = to_x(x2), yy2 = to_y(y2);
  float cx = (xx1+xx2)/2, cy = (yy1+yy2)/2;
  float rx = (xx2-xx1)/2, ry = (yy2-yy1)/2;
  proc->op_cm (mupdf_context (), proc, 1, 0, 0, 1, cx, cy); // centering
  //we can't apply scale here because in pdf the pen is scaled too

  int i=1+abs(delta)/(90*64); //number of sub-arcs needed
  if ((abs(delta)%(90*64))==0) i-- ; //correction needed if exact multiple of 90??
  float phi= 2.0*M_PI*(delta)/(i*360.0*64.0); //angular span of each sub-arc
  float a = 2.0*M_PI*(alpha)/(360.0*64.0); //start angle in radians

  // Control points for an arc of radius 1, centered on the x-axis and
  // spanning phi degrees. From: http://www.tinaja.com/glib/bezcirc2.pdf
  float sphi = sin(phi/2),  cphi = cos(phi/2);
  float bx0 = cphi,      by0 = -sphi;
  float bx1 = (4.0-bx0)/3.0,  by1 = (1.0-bx0)*(3.0-bx0)/(3.0*by0);
  float bx2 = bx1,      by2 = -by1;
  float bx3 = bx0,      by3 = -by0;
  
  // repeatedly draw rotated and scaled sub-arc
  // cannot use user-space transformations with cm util path is painted
  // (otherwise path is lost) so we perform explicit rotation+scaling
  // calculations
  int k;
  for (k=0; k<i;k++) {
    sphi = sin(phi*(k+0.5)+a);
    cphi = cos(phi*(k+0.5)+a);
    if (k==0) {
      //start point
      proc->op_m (mupdf_context (), proc,
                  rx*(bx0*cphi-by0*sphi),ry* (+bx0*sphi+by0*cphi));
    }
    proc->op_c (mupdf_context (), proc,
      rx*(bx1*cphi-by1*sphi), ry*(+bx1*sphi+by1*cphi),
      rx*(bx2*cphi-by2*sphi), ry*(+bx2*sphi+by2*cphi),
      rx*(bx3*cphi-by3*sphi), ry*(+bx3*sphi+by3*cphi));
  }
  
  // paint
  if (filled) {
    // proc->op_l (mupdf_context (), proc, 0.0, 0.0); // for a filled "pie"
    // with vertex at the center
    proc->op_f (mupdf_context (), proc);
  } else {
    // here we close the path if it's a full circle
     if (abs(delta) == 360*64)
       proc->op_s (mupdf_context (), proc);
     else
       proc->op_S (mupdf_context (), proc);
  }
  // restore the graphics state (undoes centering only)
  proc->op_Q (mupdf_context (), proc);
}

void
mupdf_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  // debug_convert << "arc\n";
  end_text ();
  bezier_arc(x1, y1, x2, y2, alpha, delta, false);
}

void
mupdf_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2,
                              int alpha, int delta) {
  // debug_convert << "fill_arc\n";
  end_text ();
  bezier_arc(x1, y1, x2, y2, alpha, delta, true);
}

void
mupdf_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  // debug_convert << "polygon\n";
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  end_text ();

  proc->op_m (mupdf_context (), proc, to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++)
    proc->op_l (mupdf_context (), proc, to_x (x[i]), to_y (y[i]));
  proc->op_h (mupdf_context (), proc);
  // as the PDF renderer (and X11): nonzero winding for convex polygons,
  // even-odd for the others (the Qt port uses the winding rule there)
  if (convex)
    proc->op_f (mupdf_context (), proc); // nonzero winding
  else
    proc->op_fstar (mupdf_context (), proc); // even-odd
}

void
mupdf_renderer_rep::rounded_rectangle (SI x1, SI y1, SI x2, SI y2,
                                       SI r_tl, SI r_tr, SI r_br, SI r_bl,
                                       bool filled) {
  // Draw a rectangle with rounded corners using cubic Bézier curves
  // r_tl, r_tr, r_br, r_bl are the radii for top-left, top-right,
  // bottom-right, and bottom-left corners respectively
  end_text ();

  // Ensure coordinates are ordered correctly
  float xx1 = to_x (min (x1, x2));
  float yy1 = to_y (min (y1, y2));
  float xx2 = to_x (max (x1, x2));
  float yy2 = to_y (max (y1, y2));

  // Convert radii to PDF coordinates
  float rtl = (float) r_tl / pixel;
  float rtr = (float) r_tr / pixel;
  float rbr = (float) r_br / pixel;
  float rbl = (float) r_bl / pixel;

  // Clamp radii to half the rectangle dimensions
  float max_rx = (xx2 - xx1) / 2.0;
  float max_ry = (yy2 - yy1) / 2.0;
  float max_r = (max_rx < max_ry) ? max_rx : max_ry;
  if (rtl > max_r) rtl = max_r;
  if (rtr > max_r) rtr = max_r;
  if (rbr > max_r) rbr = max_r;
  if (rbl > max_r) rbl = max_r;

  // Bézier control point distance for circular arc approximation
  // For a 90° arc, the magic number is 4/3 * tan(π/8) ≈ 0.5522847498
  float kappa = 0.5522847498;

  // Note: In PDF coordinates y increases downward, so yy1 is visually bottom, yy2 is top
  // Therefore: r_bl -> yy1 left, r_br -> yy1 right, r_tl -> yy2 left, r_tr -> yy2 right

  // Start at bottom-left corner (moving right from the rounded corner)
  proc->op_m (mupdf_context (), proc, xx1 + rbl, yy1);

  // Bottom edge
  proc->op_l (mupdf_context (), proc, xx2 - rbr, yy1);

  // Bottom-right corner
  if (rbr > 0) {
    float cx = rbr * kappa;
    proc->op_c (mupdf_context (), proc,
                xx2 - rbr + cx, yy1,
                xx2, yy1 + rbr - cx,
                xx2, yy1 + rbr);
  }

  // Right edge
  proc->op_l (mupdf_context (), proc, xx2, yy2 - rtr);

  // Top-right corner
  if (rtr > 0) {
    float cx = rtr * kappa;
    proc->op_c (mupdf_context (), proc,
                xx2, yy2 - rtr + cx,
                xx2 - rtr + cx, yy2,
                xx2 - rtr, yy2);
  }

  // Top edge
  proc->op_l (mupdf_context (), proc, xx1 + rtl, yy2);

  // Top-left corner
  if (rtl > 0) {
    float cx = rtl * kappa;
    proc->op_c (mupdf_context (), proc,
                xx1 + rtl - cx, yy2,
                xx1, yy2 - rtl + cx,
                xx1, yy2 - rtl);
  }

  // Left edge
  proc->op_l (mupdf_context (), proc, xx1, yy1 + rbl);

  // Bottom-left corner (closing the path)
  if (rbl > 0) {
    float cx = rbl * kappa;
    proc->op_c (mupdf_context (), proc,
                xx1, yy1 + rbl - cx,
                xx1 + rbl - cx, yy1,
                xx1 + rbl, yy1);
  }

  // Close and paint the path
  proc->op_h (mupdf_context (), proc);
  if (filled)
    proc->op_f (mupdf_context (), proc);
  else
    proc->op_S (mupdf_context (), proc);
}

/******************************************************************************
* Image rendering
******************************************************************************/

static void
set_default_gstate (fz_context *ctx, pdf_processor *proc) {
//  buf << "<< /Type /ExtGState\r\n";
//  buf << "/LW 1.0\r\n";
  proc->op_w (ctx, proc, 1.0);
//  buf << "/LC 0\r\n";
  proc->op_J (ctx, proc, 0);
//  buf << "/LJ 0\r\n";
  proc->op_j (ctx, proc, 0);
//  buf << "/ML 10.0\r\n";
  proc->op_M (ctx, proc, 10.0);
//       //buf << "/D [[] 0]\r\n"; // useless
//  buf << "/RI /RelativeColorimetric\r\n";
  proc->op_ri (ctx, proc, "RelativeColorimetric");
//  buf << "/OP false\r\n";
  proc->op_gs_OP (ctx, proc, 0);
//  buf << "/op false\r\n";
  proc->op_gs_op (ctx, proc, 0);
//  buf << "/FL 1.0\r\n";
  proc->op_i (ctx, proc, 1.0);
//  buf << "/SA false\r\n"; // Automatic Stroke Adjustement
  // not available in mupdf apparently
//  buf << "/BM /Normal\r\n";
  proc->op_gs_BM (ctx, proc, "Normal");
//  buf << "/SMask /None\r\n";
  proc->op_gs_SMask (ctx, proc, NULL, NULL, NULL, 0, NULL);
//  buf << "/CA 1.0\r\n";
  proc->op_gs_CA (ctx, proc, 1.0);
//  buf << "/ca 1.0\r\n";
  proc->op_gs_ca (ctx, proc, 1.0);
//  buf << "/AIS false\r\n"; // Alpha is shape
  // not available in mupdf apparently
//  buf << "/TK true\r\n"; // text knockout flag
  // not available in mupdf apparently
}

static void
image (fz_context *ctx, pdf_processor *proc, mupdf_image im, int alpha,
       float a, float b, float c, float d, float e, float f) {
  // debug_convert << "mupdf_renderer_rep::image " << u << ", " << w << " x " << h
  //    << " + (" << x << ", " << y << ")" << LF;
  if (is_nil (im) || im->img == NULL) return; // nothing to draw
  proc->op_q (ctx, proc);
  set_default_gstate (ctx, proc);
  proc->op_cm (ctx, proc, a, b, c, d, e, f);
  float da = ((float) alpha)/255.0;
  proc->op_gs_ca (ctx, proc, da);
  proc->op_gs_CA (ctx, proc, da);
  proc->op_Do_image (ctx, proc, "Image", im->img);
 // proc->op_re (ctx, proc, 0, 0, 1, 1);
 // proc->op_S (ctx, proc);
  proc->op_Q (ctx, proc);
}

// the content streams of a page, which may be one stream or a list of them
static fz_buffer*
page_contents (fz_context* ctx, pdf_obj* contents) {
  if (!pdf_is_array (ctx, contents)) return pdf_load_stream (ctx, contents);
  fz_buffer* all= fz_new_buffer (ctx, 4096);
  int n= pdf_array_len (ctx, contents);
  for (int i=0; i<n; i++) {
    fz_buffer* b= NULL;
    fz_try (ctx) { b= pdf_load_stream (ctx, pdf_array_get (ctx, contents, i)); }
    fz_catch (ctx) { b= NULL; }
    if (b == NULL) continue;
    unsigned char* d= NULL;
    size_t len= fz_buffer_storage (ctx, b, &d);
    fz_append_data (ctx, all, d, len);
    fz_append_byte (ctx, all, '\n');
    fz_drop_buffer (ctx, b);
  }
  return all;
}

// The first page of a PDF file as a form, nil if MuPDF cannot read it.
static mupdf_form
load_pdf_form (url u) {
  fz_context* ctx= mupdf_context ();
  fz_buffer* data= NULL;
  fz_stream* in= NULL;
  fz_buffer* buf= NULL;
  pdf_document* doc= NULL;
  pdf_obj* xo= NULL;
  fz_var (data); fz_var (in); fz_var (buf); fz_var (doc); fz_var (xo);
  fz_try (ctx) {
    c_string path (concretize (u));
    data= fz_read_file (ctx, path);
    in= fz_open_buffer (ctx, data);
    doc= pdf_open_document_with_stream (ctx, in);
    pdf_obj* page= pdf_lookup_page_obj (ctx, doc, 0);
    fz_rect box; fz_matrix m;
    pdf_page_obj_transform (ctx, page, &box, &m);
    // pdf_page_obj_transform gives fitz's transform of the page, which
    // turns PDF space (y up) into fitz space (y down) as well as undoing
    // /Rotate; a form's /Matrix lives in PDF space, so the turn upside
    // down is taken back out, or every figure comes out upside down
    m= fz_concat (m, fz_scale (1, -1));
    buf= page_contents (ctx, pdf_dict_get (ctx, page, PDF_NAME(Contents)));
    pdf_obj* res= pdf_dict_get_inheritable (ctx, page, PDF_NAME(Resources));
    xo= pdf_new_xobject (ctx, doc, box, m, res, buf);
  }
  fz_always (ctx) {
    fz_drop_buffer (ctx, buf);
    fz_drop_stream (ctx, in);
    fz_drop_buffer (ctx, data);
  }
  fz_catch (ctx) {
    cout << "TeXmacs] MuPDF cannot read " << u << ": "
         << fz_caught_message (ctx) << LF;
    pdf_drop_obj (ctx, xo);
    pdf_drop_document (ctx, doc);
    return mupdf_form ();
  }
  return mupdf_form (doc, xo);
}

// Draw a form into the box of size w by h (device pixels) whose lower left
// corner is at (x, y). A form is drawn in its /BBox as its own /Matrix
// places it -- a page turned by /Rotate comes with a matrix which turns it
// back -- so it is that placed box which is mapped onto the one asked for,
// as in the PDF renderer.
static void
draw_form (fz_context *ctx, pdf_processor *proc, mupdf_form fm, int alpha,
           double w, double h, double x, double y) {
  if (is_nil (fm) || fm->xo == NULL) return;
  fz_rect b= pdf_dict_get_rect (ctx, fm->xo, PDF_NAME(BBox));
  b= fz_transform_rect (b, pdf_dict_get_matrix (ctx, fm->xo, PDF_NAME(Matrix)));
  double bw= b.x1 - b.x0, bh= b.y1 - b.y0;
  if (bw <= 0 || bh <= 0) return;
  double sx= w / bw, sy= h / bh;
  // q and Q stay outside: a figure which throws halfway must not leave
  // the graphics state one level deeper
  proc->op_q (ctx, proc);
  mupdf_protected ("draw_form", [&] () {
    set_default_gstate (ctx, proc);
    proc->op_cm (ctx, proc, sx, 0, 0, sy, x - b.x0 * sx, y - b.y0 * sy);
    float da= ((float) alpha) / 255.0;
    proc->op_gs_ca (ctx, proc, da);
    proc->op_gs_CA (ctx, proc, da);
    // a form is run against the resources of the stream it occurs in,
    // and the operators here come from no stream: give it an empty stack
    // (the form has resources of its own), the frame which
    // pdf_process_contents would have pushed
    pdf_processor_push_resources (ctx, proc, NULL);
    fz_try (ctx) { proc->op_Do_form (ctx, proc, "Fm", fm->xo); }
    fz_always (ctx) { pdf_drop_obj (ctx, pdf_processor_pop_resources (ctx, proc)); }
    fz_catch (ctx) { fz_rethrow (ctx); }
  });
  proc->op_Q (ctx, proc);
}

void
mupdf_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  p= as_mupdf_picture (p);
  mupdf_picture_rep* pict= (mupdf_picture_rep*) p->get_handle ();
  if (draw_pixmap_direct (pict->pix, x - p->get_origin_x () * pixel,
                          y - p->get_origin_y () * pixel, alpha, pict->opaque))
    return;
  if (!pict->im) {
    // let's cache the image representation of the pixmap
    // it will be dropped by the object
    pict->im= mupdf_image_from_pixmap (pict->pix);
    if (pict->im == NULL) return;
  }
  int w= p->get_width (), h= p->get_height ();
  int ox= p->get_origin_x (), oy= p->get_origin_y ();
  end_text ();
  image (mupdf_context (), proc, pict->im, alpha,
         w, 0, 0, h,
         to_x (x - ox*pixel), to_y (y - oy*pixel));
}

void
mupdf_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  // debug_convert << "pdf renderer, draw_scalable "
  //   << im->get_name () << " at " << x << ", " << y
  //   << " (" << alpha << ")" << LF;
  if (im->get_type () != scalable_image ||
      (im->get_type () == scalable_image && im->get_effect () != tree ("")))
    renderer_rep::draw_scalable (im, x, y, alpha);
  else {
    url u= im->get_name ();
    tree lookup= tuple (u->t);
    if (locase_all (suffix (u)) == "pdf") {
      // a PDF is drawn as what it is, a drawing, at any zoom
      if (!form_pool->contains (lookup))
        form_pool (lookup)= load_pdf_form (u);
      mupdf_form fm= form_pool [lookup];
      if (!is_nil (fm)) {
        rectangle r= im->get_logical_extents ();
        SI w= r->x2 - r->x1, h= r->y2 - r->y1;
        end_text ();
        draw_form (mupdf_context (), proc, fm, alpha,
                   ((double) w)/pixel, ((double) h)/pixel,
                   to_x (x - r->x1), to_y (y - r->y1));
        return;
      }
      // MuPDF cannot read it: the converters may
    }
    mupdf_image im2;
    if (image_pool->contains (lookup))
      im2= image_pool [lookup];
    else {
      fz_image* fzim= mupdf_load_image (u);
      if (fzim == NULL) {
        // not loadable by MuPDF: the generic path converts the file
        renderer_rep::draw_scalable (im, x, y, alpha);
        return;
      }
      im2= mupdf_image (fzim);
      fz_drop_image (mupdf_context (), fzim);
      image_pool (lookup)= im2;
    }
    if (is_nil (im2) || im2->img == NULL) return;
    rectangle r= im->get_logical_extents ();
    SI w= r->x2 - r->x1, h= r->y2 - r->y1;
    int ox= r->x1, oy= r->y1;
    end_text ();
    image (mupdf_context (), proc, im2, alpha,
           ((double)w)/pixel, 0,
           0, ((double)h)/pixel ,
           to_x (x - ox), to_y (y - oy));

  }
}

/******************************************************************************
* Glyph rendering
******************************************************************************/

// Glyphs filled with the pattern of a brush pencil: the glyph mask
// modulates the pattern image, sampled where the glyph lands on the device
// (as in the Qt port), and the result is drawn as an image.

// the pattern images decoded as RGB pixmaps, by pattern data
static hashmap<tree,pointer> pattern_pixmap_pool (NULL);

static fz_pixmap*
get_pattern_pixmap (brush br, SI pixel) {
  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  tree key= tuple (u->t, as_string (w), as_string (h), eff);
  if (pattern_pixmap_pool->contains (key))
    return (fz_pixmap*) pattern_pixmap_pool [key];
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= mupdf_load_pixmap (u, w, h, eff, pixel);
  fz_pixmap* rgb= NULL;
  if (pix != NULL) {
    // RGB with alpha, whatever the file provides
    mupdf_protected ("pattern pixmap", [&] () {
      rgb= fz_convert_pixmap (ctx, pix, fz_device_rgb (ctx), NULL, NULL,
                              fz_default_color_params, 1);
    });
    fz_drop_pixmap (ctx, pix);
  }
  pattern_pixmap_pool (key)= (pointer) rgb; // NULL too: do not retry
  return rgb;
}

void
mupdf_renderer_rep::draw_bis (int c, font_glyphs fng, SI x, SI y) {
  fz_context* ctx= mupdf_context ();
  SI xo, yo;
  glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
  glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo, 1.0);
  int w= gl->width, h= gl->height;
  if (w <= 0 || h <= 0) return;
  brush br= pen->get_brush ();
  fz_pixmap* pat= get_pattern_pixmap (br, brushpx == -1? pixel: brushpx);
  if (pat == NULL) { // no pattern: plain glyph in the color of the pencil
    pencil saved= pen;
    pen= pencil (pen->get_color (), pen->get_width ());
    draw (c, fng, x, y);
    pen= saved;
    return;
  }
  int pattern_alpha= br->get_alpha ();
  int pw= fz_pixmap_width (ctx, pat), ph= fz_pixmap_height (ctx, pat);
  int pn= fz_pixmap_components (ctx, pat), stride= fz_pixmap_stride (ctx, pat);
  unsigned char* ps= fz_pixmap_samples (ctx, pat);
  // device position of the top left pixel of the glyph (the device y axis
  // points downwards, see the matrix of begin)
  int tx= (int) floor (to_x (x - xo*std_shrinkf));
  int ty= (int) floor (- to_y (y + yo*std_shrinkf));
  int nr_cols= std_shrinkf*std_shrinkf;
  if (nr_cols >= 64) nr_cols= 64;
  unsigned char *samples= (unsigned char *)
    Memento_label (fz_malloc (ctx, h*w*4), "pattern_glyph_data");
  unsigned char *d= samples;
  // the pattern is anchored at the origin of the document, as the fills
  // are (placed_pattern): its device position is (to_x (0), -to_y (0))
  int ax= tx - (int) floor (to_x (0)), ay= ty + (int) floor (to_y (0));
  for (int j=0; j<h; j++) {
    int py= ((ay + j) % ph + ph) % ph;
    for (int i=0; i<w; i++) {
      int px= ((ax + i) % pw + pw) % pw;
      unsigned char* s= ps + py*stride + px*pn;
      int r= s[0], g= (pn >= 3? s[1]: s[0]), b= (pn >= 3? s[2]: s[0]);
      int a= (pn == 4 || pn == 2)? s[pn-1]: 255;
      if (get_reverse_colors ()) reverse (r, g, b);
      int cov= (gl->get_x (i, j) * pattern_alpha) / nr_cols; // 0..255
      // fz pixmaps with alpha are premultiplied, and so is the result
      d[0]= (r*cov)/255;
      d[1]= (g*cov)/255;
      d[2]= (b*cov)/255;
      d[3]= (a*cov)/255;
      d+= 4;
    }
  }
  fz_pixmap* pix= NULL;
  fz_image* im= NULL;
  mupdf_protected ("pattern glyph image", [&] () {
    pix= fz_new_pixmap_with_data (ctx, fz_device_rgb (ctx),
                                  w, h, NULL, 1, w*4, samples);
    im= fz_new_image_from_pixmap (ctx, pix, NULL);
  });
  if (im == NULL) {
    if (pix != NULL) fz_drop_pixmap (ctx, pix);
    else fz_free (ctx, samples);
    return;
  }
  mupdf_image mi (im);
  fz_drop_pixmap (ctx, pix);
  fz_drop_image (ctx, im);
  end_text ();
  image (ctx, proc, mi, 255, w, 0.0, 0.0, h,
         to_x (x - xo*std_shrinkf), to_y (y + yo*std_shrinkf - h*pixel));
}

static
pdf_font_desc *load_pdf_font (string fontname) {
  int pos= search_forwards (":", fontname);
  string fname= (pos==-1? fontname: fontname (0, pos));
  // compound and other virtual fonts ("compound-(math ...)") have no file:
  // do not ask kpsewhich about them (the shell chokes on the name)
  if (occurs (" ", fname) || occurs ("(", fname) || occurs ("[", fname))
    return NULL;
  url u = url_none ();
  {
    //debug_convert << " try freetype " << LF;
    u = tt_font_find (fname);
    //debug_convert << fname << " " << u << LF;
  }
  if (!is_none (u)) {
    int pos= search_forwards (".", fontname);
    string rname= (pos==-1? fontname: fontname (0, pos));
    pdf_font_desc* fontdesc= NULL;
    {
      //debug_convert << "fz_new_font_from_file "  << u  << LF;
      c_string path (concretize (u));
      fz_font *font= mupdf_font_from_file (path);
      if (font) {
        fontdesc= pdf_new_font_desc (mupdf_context ());
        fontdesc->font= font;
        fontdesc->encoding=
            pdf_load_system_cmap (mupdf_context (), "Identity-H");
        // FIXME: do we need to care about all the other fields? (seems not)
        // fix the encoding for FreeType
        // see tt_face_rep::tt_face_rep
        mupdf_select_custom_charmap (fontdesc->font);
      }
    }
    if (fontdesc != NULL) {
      return fontdesc;
    }
    else {
//      convert_warning << "mupdf_renderer, font: " << fname
//          << " in file " << u << " cannot be loaded. "
//          << "Will be rendered as pixmaps" << LF;
    }
  }
  return NULL;
}

// FIXME: cannot we handle more easily font size? (also in pdf_hummus)
static float
font_size (string name) {
  int pos= search_backwards (".", name);
  int szpos= pos-1;
  while ((szpos>0) && is_numeric (name[szpos-1])) szpos--;
  double size= as_double (name (szpos, pos));
  if (size == 0) size= 10;
  int end= pos+1;
  while (end < N(name) && is_numeric (name[end])) end++;
  double dpi= as_double (name (pos+1, end));
  double mag= (size) * (dpi/72.0);
  return mag;
}

// FreeType is reached only with MuPDF's lock held. The lock is what
// serializes FreeType between the threads MuPDF may run, and it points
// FreeType's allocator at the calling context: a face which allocates
// on first use -- an OpenType one asked for a glyph name loads its table
// of names then -- crashed the PDF renderer when it was asked directly.
// Selecting a charmap and looking up an index do not allocate, but the
// discipline is kept everywhere so that it does not depend on that.
void
mupdf_select_custom_charmap (fz_font* font) {
  fz_context* ctx= mupdf_context ();
  FT_Face face= (font == NULL) ? NULL : (FT_Face) fz_font_ft_face (ctx, font);
  if (face == NULL) return;
  fz_ft_lock (ctx);
  ft_select_charmap (face, ft_encoding_adobe_custom);
  fz_ft_unlock (ctx);
}

unsigned int
mupdf_glyph_index (fz_font* font, int i) {
  if (i >= 0xc000000) return i - 0xc000000;
  fz_context* ctx= mupdf_context ();
  FT_Face face= (font == NULL) ? NULL : (FT_Face) fz_font_ft_face (ctx, font);
  if (face == NULL) return 0;
  fz_ft_lock (ctx);
  FT_UInt g= ft_get_char_index (face, i);
  fz_ft_unlock (ctx);
  return g;
}

void
mupdf_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  if (pen->get_type () == pencil_brush &&
      !is_nil (pen->get_brush ()) &&
      pen->get_brush ()->get_type () == brush_pattern) {
    draw_bis (c, fng, x, y); // glyphs filled with a pattern
    return;
  }
  string fontname = fng->res_name;
  pdf_font_desc* fontdesc= NULL;

  begin_text ();

  if (cfn != fontname) {
    // change font
    cfn= fontname;
    // try to find a native font
    if (!native_fonts->contains (fontname)) {
      fontdesc= load_pdf_font (fontname);
      if (fontdesc) {
        native_fonts (fontname)= mupdf_font (fontdesc);
        pdf_drop_font (mupdf_context (), fontdesc);
      } else {
        native_fonts (fontname)= mupdf_font (NULL);
        // this means use bitmap glyphs
      }
    } else {
      fontdesc= native_fonts (fontname)->fn;
    }
    if (fontdesc) {
      // we have a native font
      fsize = font_size (fontname);
      proc->op_Tf (mupdf_context (), proc, "draw", fontdesc, fsize/std_shrinkf);
    }
  } else {
    fontdesc= native_fonts (fontname)->fn;
  }
  // draw glyph
  if (fontdesc) {
    proc->op_Td (mupdf_context (), proc,
                 to_x (x) - prev_text_x, to_y (y) - prev_text_y);
    prev_text_x= to_x (x);
    prev_text_y= to_y (y);
    glyph gl= fng->get (c);
    if (is_nil (gl)) return;
    unsigned int gl_index; // = gl->index;
    {
      // apriori we already have the glyph index in gl->index
      // however we cannot trust this value since it is manipulated
      // in tt_face.cpp to go aroung a problem with glyph mapping in
      // Type1 fonts (for example) for pdf_hummus
      //
      // MuPDF seems to like the glyph value returned by
      // ft_get_char_index on the FT_Face it will use.
      gl_index= mupdf_glyph_index (fontdesc->font, c);
    }
    char glyphs[2] = { (char)(gl_index >> 8), (char)(gl_index) };
    proc->op_Tj (mupdf_context (), proc, glyphs, 2);
    return;
  }
  // we do not have a native font, draw a bitmap
  // we use an "immediate" approach, without trying to build a Type3 font
  // this is appropriate for raster rendering, but we need to change it
  // if we want to render to a PDF file
  // get the pixmap
  color fgc= pen->get_color ();
  basic_character xc (c, fng, std_shrinkf, fgc, 0);
  mupdf_image mi= character_image [xc];
  if (is_nil (mi)) {
    int r, g, b, a;
    get_rgb (fgc, r, g, b, a);
    if (get_reverse_colors ()) reverse (r, g, b);
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo, 1.0);
    int w= gl->width, h= gl->height;

    unsigned char *samples = (unsigned char *)
         Memento_label (fz_malloc (mupdf_context (), h*w*4),
                        "glyph_pixmap_data");
    int nr_cols= std_shrinkf*std_shrinkf;
    if (nr_cols >= 64) nr_cols= 64;
    unsigned char *d= samples;
    for (int y=0; y <h; y++) {
      for (int x=0; x <w; x++) {
        int col = gl->get_x (x, y);
        // we need to store premultiplied values for fz_pixmap
        int alpha= ((a*col)/nr_cols) & 0xFF;
        d[0] = (r*alpha)/255;
        d[1] = (g*alpha)/255;
        d[2] = (b*alpha)/255;
        d[3] = alpha;
        d+= 4;
      }
    }
    fz_pixmap* pix= NULL;
    fz_image* im= NULL;
    mupdf_protected ("glyph image", [&] () {
      pix= fz_new_pixmap_with_data (mupdf_context (),
                                    fz_device_rgb (mupdf_context ()),
                                    w, h, NULL, 1, w*4, samples);
      im= fz_new_image_from_pixmap (mupdf_context (), pix, NULL);
    });
    if (im == NULL) { // the glyph is not drawn
      if (pix != NULL) fz_drop_pixmap (mupdf_context (), pix);
      else fz_free (mupdf_context (), samples);
      return;
    }
    mi= mupdf_image (im);
    mi->xo= xo; mi->yo= yo;
    character_image (xc)= mi;
    fz_drop_pixmap (mupdf_context (), pix);
    fz_drop_image (mupdf_context (), im);
  }
  // draw the character
  image (mupdf_context (), proc, mi, 255,
         mi->w, 0.0, 0.0, mi->h,
         to_x (x- mi->xo*std_shrinkf), to_y (y+ mi->yo*std_shrinkf-mi->h*pixel));
}

/******************************************************************************
 * Main renderer
 ******************************************************************************/

mupdf_renderer_rep*
the_mupdf_renderer () {
  static mupdf_renderer_rep* the_renderer= NULL;
  if (!the_renderer) {
    the_renderer= tm_new<mupdf_renderer_rep> ();
  }
  return the_renderer;
}

/******************************************************************************
* Copying regions
******************************************************************************/


// fitz does not have a function for low lever copy of pixmaps
// we have to do it ourselves

static void
translate_pixmap (fz_pixmap *dest_pix, fz_irect dest_rect,
                  fz_pixmap *src_pix, int dx, int dy) {
  fz_context *ctx= mupdf_context ();
  fz_irect src_rect = fz_translate_irect (dest_rect,  -dx, -dy);
  src_rect = fz_intersect_irect (src_rect, fz_pixmap_bbox (ctx, src_pix));
  src_rect = fz_intersect_irect (src_rect,
                fz_translate_irect (fz_pixmap_bbox (ctx, dest_pix), -dx, -dy));
  dest_rect = fz_translate_irect (src_rect,  dx, dy);
  if (fz_is_empty_irect(dest_rect))  return;
  // all set up, let's do the work now
  {
    unsigned char *srcp;
    unsigned char *destp;
    unsigned int y, w;
    size_t destspan, srcspan;
    w = (unsigned int)(dest_rect.x1 - dest_rect.x0);
    y = (unsigned int)(dest_rect.y1 - dest_rect.y0);

    srcspan = src_pix->stride;
    srcp = src_pix->samples + srcspan * (src_rect.y0 - src_pix->y) +
           (src_rect.x0 - src_pix->x) * (size_t)src_pix->n;
    destspan = dest_pix->stride;
    destp = dest_pix->samples + destspan * (dest_rect.y0 - dest_pix->y) +
            (dest_rect.x0 - dest_pix->x) * (size_t)dest_pix->n;

    if (src_pix->n == dest_pix->n)
    {
      w *= src_pix->n;
      do
      {
        memcpy (destp, srcp, w);
        srcp += srcspan;
        destp += destspan;
      }
      while (--y);
    }
    else
    {
      cout << "mupdf_renderer_rep / translate_pixmap : non compatible pixmaps"
           << LF;
    }
  }
}


void
mupdf_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  ASSERT (ren != NULL, "invalid situation");
  if (ren->is_printer ()) return;
  mupdf_renderer_rep* src= (mupdf_renderer_rep*) ren->get_handle ();
  if (src->pixmap == pixmap && x1 == x && y1 == y) return;
  outer_round (x1, y1, x2, y2);
  SI X1= x1, Y1= y1;
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (X1, Y1);
  decode (x1, y1);
  decode (x2, y2);
  src->decode (x, y);
  x += x1 - X1;
  y += y1 - Y1;
  if (x1<x2 && y2<y1) {
    //  XCopyArea (dpy, src->win, win, gc, x, y, x2-x1, y1-y2, x1, y2);
    translate_pixmap (pixmap, fz_make_irect (x1, y2, x2, y1),
                      src->pixmap, x1-x, y1-y);
  }
}

/******************************************************************************
 * Shadow management methods 
 ******************************************************************************/

void
mupdf_renderer_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw != mw || sh != mh) {
      delete_shadow (ren);
      ren= NULL;
    }
  }
  if (ren == NULL)  {
    ren= (renderer) tm_new<mupdf_renderer_rep> (mw, mh);
    fz_pixmap *pix= mupdf_new_pixmap (mw, mh);
    static_cast<mupdf_renderer_rep*>(ren)->begin(pix);
    fz_drop_pixmap (mupdf_context (), pix);
  }
}

void 
mupdf_renderer_rep::delete_shadow (renderer& ren)  {
  if (ren != NULL) {
    static_cast<mupdf_renderer_rep*>(ren)->end();
    tm_delete (ren);
    ren= NULL;
  }
}

extern "C" {
// not exported from fitz/pixmap-imp.h
  void fz_copy_pixmap_rect(fz_context *ctx, fz_pixmap *dest, fz_pixmap *src, fz_irect r, const fz_default_colorspaces *default_cs);
}

void 
mupdf_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  mupdf_renderer_rep* shadow= static_cast<mupdf_renderer_rep*>(ren);
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  shadow->ox= ox;
  shadow->oy= oy;
  shadow->master= this;
  shadow->cx1= x1+ ox;
  shadow->cy1= y1+ oy;
  shadow->cx2= x2+ ox;
  shadow->cy2= y2+ oy;
  
  decode (x1, y1);
  decode (x2, y2);
  if (x1<x2 && y2<y1) {
    fz_irect rect= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (mupdf_context(), shadow->pixmap, pixmap, rect, NULL);
  }
}

void
mupdf_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // FIXME: we should use the routine fetch later
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  mupdf_renderer_rep* shadow= static_cast<mupdf_renderer_rep*>(ren);
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  decode (x1, y1);
  decode (x2, y2);
  if (x1<x2 && y2<y1) {
    fz_irect rect= fz_make_irect (x1, y2, x2, y1);
    fz_copy_pixmap_rect (mupdf_context(), pixmap, shadow->pixmap, rect, NULL);
  }
}

void 
mupdf_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2)  {
  if (master == NULL) return;
  if (pixmap == static_cast<mupdf_renderer_rep*>(master)->pixmap) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  static_cast<mupdf_renderer_rep*>(master)->encode (x1, y1);
  static_cast<mupdf_renderer_rep*>(master)->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}
