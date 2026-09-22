
/******************************************************************************
* MODULE     : mupdf_picture.cpp
* DESCRIPTION: Picture objects for MuPDF
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mupdf_picture.hpp"

#include "file.hpp"
#include "image_files.hpp"
#include "effect.hpp"

/******************************************************************************
* Protected MuPDF calls (see mupdf_picture.hpp)
******************************************************************************/

bool
mupdf_protected_call (const char* what, void (*f) (void*), void* data) {
  fz_context* ctx= mupdf_context ();
  int ok= 1;
  fz_var (ok);
  fz_try (ctx) {
    f (data);
  }
  fz_catch (ctx) {
    ok= 0;
    cout << "TeXmacs] MuPDF error in " << what << ": "
         << fz_caught_message (ctx) << LF;
  }
  return ok != 0;
}

fz_image*
mupdf_image_from_file (const char* path) {
  fz_context* ctx= mupdf_context ();
  fz_image* im= NULL;
  fz_var (im);
  fz_try (ctx) {
    im= fz_new_image_from_file (ctx, path);
  }
  fz_catch (ctx) {
    im= NULL;
    cout << "TeXmacs] MuPDF cannot load image " << path << ": "
         << fz_caught_message (ctx) << LF;
  }
  return im;
}

fz_image*
mupdf_image_from_pixmap (fz_pixmap* pix) {
  if (pix == NULL) return NULL;
  fz_context* ctx= mupdf_context ();
  fz_image* im= NULL;
  fz_var (im);
  fz_try (ctx) {
    im= fz_new_image_from_pixmap (ctx, pix, NULL);
  }
  fz_catch (ctx) {
    im= NULL;
    cout << "TeXmacs] MuPDF cannot make an image: "
         << fz_caught_message (ctx) << LF;
  }
  return im;
}

fz_pixmap*
mupdf_pixmap_from_image (fz_image* im) {
  if (im == NULL) return NULL;
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= NULL;
  fz_var (pix);
  fz_try (ctx) {
    pix= fz_get_pixmap_from_image (ctx, im, NULL, NULL, NULL, NULL);
  }
  fz_catch (ctx) {
    pix= NULL;
    cout << "TeXmacs] MuPDF cannot decode an image: "
         << fz_caught_message (ctx) << LF;
  }
  return pix;
}

// The channel order of the window surfaces. SDL gives the format which
// suits the window best; that is BGRA on the platforms supported here, and
// native_picture_from_SDL_Surface wraps the surface with this colorspace.
// A pixmap which is going to be blitted into a window is allocated with it
// too, so that the blit is a copy per row rather than a conversion per
// pixel (see mupdf_renderer_rep::draw_pixmap_direct).
fz_colorspace*
mupdf_screen_colorspace () {
  return fz_device_bgr (mupdf_context ());
}

fz_pixmap*
mupdf_new_pixmap (int w, int h) {
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= NULL;
  fz_var (pix);
  fz_try (ctx) {
    pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), w, h, NULL, 1);
  }
  fz_catch (ctx) {
    pix= NULL;
    cout << "TeXmacs] MuPDF cannot allocate a " << w << "x" << h
         << " pixmap: " << fz_caught_message (ctx) << LF;
  }
  // a 1x1 pixmap stands for what could not be allocated (out of memory
  // for a 1x1 pixmap would terminate the process, nothing to do then)
  if (pix == NULL) pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), 1, 1, NULL, 1);
  fz_clear_pixmap (ctx, pix);
  return pix;
}

/******************************************************************************
* Abstract mupdf pictures
******************************************************************************/

mupdf_picture_rep::mupdf_picture_rep (fz_pixmap *_pix, int ox2, int oy2):
  pix (_pix), im (NULL),
  w (fz_pixmap_width (mupdf_context (), pix)),
  h (fz_pixmap_height (mupdf_context (), pix)),
  ox (ox2), oy (oy2), opaque (false) {
  fz_keep_pixmap (mupdf_context (), pix);
}

mupdf_picture_rep::~mupdf_picture_rep () {
  fz_drop_pixmap (mupdf_context (), pix);
  fz_drop_image (mupdf_context (), im);
}

picture_kind mupdf_picture_rep::get_type () { return picture_native; }
void* mupdf_picture_rep::get_handle () { return (void*) this; }

int mupdf_picture_rep::get_width () { return w; }
int mupdf_picture_rep::get_height () { return h; }
int mupdf_picture_rep::get_origin_x () { return ox; }
int mupdf_picture_rep::get_origin_y () { return oy; }
void mupdf_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
mupdf_picture_rep::internal_get_pixel (int x, int y) {
  unsigned char *samples= fz_pixmap_samples (mupdf_context (), pix);
  return  rgbap_to_argb (((color*)samples)[x+w*(h-1-y)]);
}

void
mupdf_picture_rep::internal_set_pixel (int x, int y, color c) {
  unsigned char *samples= fz_pixmap_samples (mupdf_context (), pix);
  ((color*)samples)[x+w*(h-1-y)]= argb_to_rgbap (c);
}

picture
mupdf_picture (fz_pixmap *_pix, int ox, int oy) {
  return (picture)tm_new<mupdf_picture_rep, fz_pixmap*,int,int> (_pix, ox, oy);
}

picture
as_mupdf_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  fz_pixmap *pix= mupdf_new_pixmap (pic->get_width (), pic->get_height ());
  picture ret= mupdf_picture (pix, pic->get_origin_x (), pic->get_origin_y ());
  fz_drop_pixmap (mupdf_context (), pix);
  ret->copy_from (pic); // FIXME: is this inefficient???
  return ret;
}

#ifdef MUPDF_RENDERER
picture
as_native_picture (picture pict) {
  return as_mupdf_picture (pict);
}

picture
native_picture (int w, int h, int ox, int oy) {
  fz_pixmap *pix= mupdf_new_pixmap (w, h);
  picture p= mupdf_picture (pix, ox, oy);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
}

// A picture which is opaque from the start and stays so: source-over on an
// opaque destination leaves the alpha at 255, whatever is drawn. Blitting
// such a picture needs no test per pixel, which is what the backing store
// of an editor is for: with the test the blit of a full window costs about
// 4.4 ms, without it 0.4 ms, the speed of a memcpy (the reordering of the
// channels into the window, which MuPDF does not do for us, is free once
// the loop has no branch in it).
picture
native_opaque_picture (int w, int h, int ox, int oy) {
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= mupdf_new_pixmap (w, h);
  fz_clear_pixmap_with_value (ctx, pix, 0xff); // white, and opaque
  picture p= mupdf_picture (pix, ox, oy);
  ((mupdf_picture_rep*) p->get_handle ())->opaque= true;
  fz_drop_pixmap (ctx, pix);
  return p;
}
#endif

/******************************************************************************
* Rendering on images
******************************************************************************/

class mupdf_image_renderer_rep: public mupdf_renderer_rep {
public:
  picture pict;
  
public:
  mupdf_image_renderer_rep (picture pict, double zoom);
  void* get_data_handle ();
};

mupdf_image_renderer_rep::mupdf_image_renderer_rep (picture p, double zoom)
  : mupdf_renderer_rep (), pict (p)
{
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI)  tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;

  int pw = p->get_width ();
  int ph = p->get_height ();
  int pox= p->get_origin_x ();
  int poy= p->get_origin_y ();

  ox = pox * pixel;
  oy = poy * pixel;
  /*
  cx1= 0;
  cy1= 0;
  cx2= pw * pixel;
  cy2= ph * pixel;
  */
  cx1= 0;
  cy1= -ph * pixel;
  cx2= pw * pixel;
  cy2= 0;

  mupdf_picture_rep* handle= (mupdf_picture_rep*) pict->get_handle ();
  begin (handle->pix);
}

void*
mupdf_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

#ifdef MUPDF_RENDERER
renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<mupdf_image_renderer_rep> (p, zoomf);
}
#endif

/******************************************************************************
* Loading pictures
******************************************************************************/

picture raw_load_xpm (url file_name);

/******************************************************************************
* Vector pictures
*
* MuPDF draws SVG itself (its source/svg), so the icons of TeXmacs are
* rendered from their vector originals instead of from the rasters, sharp at
* every resolution and available in a light and a dark variant. Its parser
* reads the presentation attributes and the inline style attribute only, not
* a <style> element with class selectors: an icon written that way comes out
* black, so the files of misc/pixmaps carry their styles inline.
******************************************************************************/

// Draw u in a box of w x h points, at scale device pixels per point. A side
// given as zero is taken from the size the file declares; the drawing keeps
// its proportions and is centered in the box. NULL when the file cannot be
// read or parsed.
fz_pixmap*
mupdf_render_svg (url u, int w, int h, int scale) {
  fz_context* ctx= mupdf_context ();
  c_string path (concretize (u));
  fz_buffer* buf= NULL;
  fz_display_list* list= NULL;
  fz_device* dev= NULL;
  fz_pixmap* pix= NULL;
  float dw= 0.0f, dh= 0.0f;
  if (scale < 1) scale= 1;
  fz_var (buf);
  fz_var (list);
  fz_var (dev);
  fz_var (pix);
  fz_try (ctx) {
    buf= fz_read_file (ctx, path);
    list= fz_new_display_list_from_svg (ctx, buf, NULL, NULL, &dw, &dh);
    if (dw <= 0.0f || dh <= 0.0f) fz_throw (ctx, FZ_ERROR_GENERIC, "empty svg");
    // the box: what was asked for, completed with what the file declares
    if (w <= 0 && h <= 0) { w= (int) (dw + 0.5f); h= (int) (dh + 0.5f); }
    else if (w <= 0) w= (int) ((dw * h) / dh + 0.5f);
    else if (h <= 0) h= (int) ((dh * w) / dw + 0.5f);
    if (w < 1) w= 1;
    if (h < 1) h= 1;
    float f= ((float) w) / dw;
    if (((float) h) / dh < f) f= ((float) h) / dh;
    pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), w * scale, h * scale, NULL, 1);
    fz_clear_pixmap (ctx, pix); // transparent
    fz_matrix m= fz_concat (fz_scale (f * scale, f * scale),
                            fz_translate (0.5f * (w - f * dw) * scale,
                                          0.5f * (h - f * dh) * scale));
    dev= fz_new_draw_device (ctx, m, pix);
    fz_run_display_list (ctx, list, dev, fz_identity, fz_infinite_rect, NULL);
    fz_close_device (ctx, dev);
  }
  fz_always (ctx) {
    fz_drop_device (ctx, dev);
    fz_drop_display_list (ctx, list);
    fz_drop_buffer (ctx, buf);
  }
  fz_catch (ctx) {
    fz_drop_pixmap (ctx, pix);
    pix= NULL;
    cout << "TeXmacs] MuPDF cannot render " << path << ": "
         << fz_caught_message (ctx) << LF;
  }
  return pix;
}

picture
mupdf_load_svg (url u, int w, int h) {
  fz_pixmap* pix= mupdf_render_svg (u, w, h, retina_factor);
  if (pix == NULL) return picture ();
  picture pic= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return pic;
}

fz_image *
mupdf_load_image (url u) {
  //cout << "mupdf_load_image " << u << LF;
  fz_image *im = NULL;
  string suf= suffix (u);
  if (suf == "svg") {
      // at the size the file declares, and at the resolution we draw at:
      // mupdf_load_pixmap scales the result when a size is asked for
      fz_pixmap* pix= mupdf_render_svg (u, 0, 0, retina_factor);
      if (pix != NULL) {
        im= mupdf_image_from_pixmap (pix);
        fz_drop_pixmap (mupdf_context (), pix);
      }
    } else if ((suf == "jpg") || (suf == "png")) {
      // FIXME: add more supported formats
      c_string path (concretize (u));
      //cout << "path :" << path << LF;
      im= mupdf_image_from_file (path);
    } else if (suf == "xpm") {
      // try to load higher definition png equivalent if available
      url png_equiv= glue (unglue (u, 4), "_x4.png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      png_equiv= glue (unglue (u, 4), "_x2.png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      png_equiv= glue (unglue (u, 4), ".png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      // ok, try to load the xpm finally
      picture xp= as_mupdf_picture (raw_load_xpm (u));
      fz_pixmap *pix= ((mupdf_picture_rep*)xp->get_handle())->pix;
      im= mupdf_image_from_pixmap (pix);
    }
  return im;
}

fz_pixmap*
mupdf_load_pixmap (url u, int w, int h, tree eff, SI pixel) {
  fz_image *im = mupdf_load_image (u);

  if (im == NULL) {
    // attempt to convert to png
    url temp= url_temp (".png");
    image_to_png (u, temp, w, h);
    c_string path (as_string (temp));
    im= mupdf_image_from_file (path);
    remove (temp);
  }
  
  // Error Handling
  if (im == NULL) {
      cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
      return NULL;
  }

  // Scaling
  fz_pixmap *pix= mupdf_pixmap_from_image (im);
  fz_drop_image (mupdf_context (), im); // we do not need it anymore
  if (pix == NULL) {
    cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
    return NULL;
  }

  // Scaling to the requested size (patterns are given with a size)
  fz_context* ctx= mupdf_context ();
  if (w > 0 && h > 0 &&
      (fz_pixmap_width (ctx, pix) != w || fz_pixmap_height (ctx, pix) != h)) {
    fz_pixmap* scaled= NULL;
    mupdf_protected ("image scaling", [&] () {
      scaled= fz_scale_pixmap (ctx, pix, 0, 0, w, h, NULL);
    });
    if (scaled != NULL) {
      fz_drop_pixmap (ctx, pix);
      pix= scaled;
    }
    else cout << "TeXmacs] warning: cannot scale " << concretize (u) << "\n";
  }

  // Build effect
  if (eff != "") {
    effect e= build_effect (eff);
    picture src= mupdf_picture (pix, 0, 0);
    array<picture> a;
    a << src;
    picture pic= e->apply (a, pixel);
    picture dest= as_mupdf_picture (pic);
    mupdf_picture_rep* rep= (mupdf_picture_rep*) dest->get_handle ();
    fz_pixmap* tpix= rep->pix;
    fz_drop_pixmap (mupdf_context (), pix);
    pix= tpix;
  }
  return pix;
}

picture 
mupdf_load_picture (url file_name) {
  fz_image* fzim= mupdf_load_image (file_name);  
  fz_pixmap *pix= mupdf_pixmap_from_image (fzim);
  if (fzim != NULL) fz_drop_image (mupdf_context (), fzim); // not needed anymore
  if (pix == NULL) {
    cout << "TeXmacs] warning: cannot load picture " << file_name << "\n";
    pix= mupdf_new_pixmap (1, 1);
  }

  picture pic= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return pic;
}

// The theme in which the vector icons are looked up: misc/pixmaps/light and
// misc/pixmaps/dark hold one recoloured copy of every icon each, and both
// are on $TEXMACS_PIXMAP_PATH. The GUI sets it from its own theme.
static string mupdf_icon_theme= "light";

void
mupdf_set_icon_theme (string theme) {
  if (theme != "dark") theme= "light";
  mupdf_icon_theme= theme;
}

string
mupdf_get_icon_theme () { return mupdf_icon_theme; }

// The size in points at which an icon set is drawn is the name of the
// directory it sits in (modern/24x24/main, traditional/--x17, where a dash
// means that the side is free). The sizes the SVG files declare cannot be
// used instead: a few dozen of them, the flags in particular, declare the
// size of the drawing they were made from rather than that of the icon.
static void
icon_box_size (url dir, int& w, int& h) {
  w= h= 0;
  for (int i= 0; i < 2 && !is_none (dir) && !is_root (dir); i++) {
    string s= as_string (tail (dir));
    int k= search_forwards ("x", 0, s);
    if (k > 0) {
      string sw= s (0, k), sh= s (k+1, N(s));
      bool free_w= (sw == "--"), free_h= (sh == "--");
      if ((free_w || is_int (sw)) && (free_h || is_int (sh))) {
        w= free_w ? 0 : as_int (sw);
        h= free_h ? 0 : as_int (sh);
        return;
      }
    }
    dir= head (dir);
  }
}

// The icons ship in several variants: name.svg (vector, in a light and a
// dark version), name.xpm (the legacy 1x format), name.png (1x),
// name_x2.png (2x) and name_x4.png (4x), all of the same size in points.
// Draw the vector one when there is one, and otherwise pick the raster which
// matches the resolution we draw at (retina_factor device pixels per point),
// falling back on the smaller ones, then on the file which was asked for.
picture 
mupdf_load_xpm (url file_name) {
  if (suffix (file_name) != "xpm") return mupdf_load_picture (file_name);
  url base= unglue (file_name, 4); // without ".xpm"
  url svg= resolve (url ("$TEXMACS_PIXMAP_PATH") * url (mupdf_icon_theme) *
                    glue (tail (base), ".svg") |
                    glue (base, ".svg"));
  if (!is_none (svg)) {
    int w= 0, h= 0;
    icon_box_size (head (file_name), w, h);
    picture pic= mupdf_load_svg (svg, w, h);
    if (!is_nil (pic)) return pic;
  }
  array<string> tried;
  if (retina_factor >= 4) tried << string ("_x4.png");
  if (retina_factor >= 2) tried << string ("_x2.png");
  tried << string (".png");
  for (int i= 0; i < N(tried); i++) {
    url variant= glue (base, tried[i]);
    if (exists (resolve ("$TEXMACS_PIXMAP_PATH" * variant)))
      return mupdf_load_picture (variant);
  }
  return mupdf_load_picture (file_name); // the xpm itself
}  

#ifdef MUPDF_RENDERER
picture
load_picture (url u, int w, int h, tree eff, int pixel) {
  fz_pixmap* pix= mupdf_load_pixmap (u, w, h, eff, pixel);
  if (pix == NULL) return error_picture (w, h);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context(), pix);
  return p;
}

void
save_picture (url dest, picture p) {
  if (suffix(dest) != "png") {
    cout << "TeXmacs] warning: cannot save " << concretize (dest)
         << ", format not supported\n";
    return;
  }
  picture q= as_mupdf_picture (p);
  mupdf_picture_rep* pict= (mupdf_picture_rep*) q->get_handle ();
  if (exists (dest)) remove (dest);
  c_string path= concretize (dest);
  fz_output *out= fz_new_output_with_path (mupdf_context (), path, 0);
  fz_write_pixmap_as_png (mupdf_context (), out, pict->pix);
  fz_close_output (mupdf_context (), out);
  fz_drop_output (mupdf_context (), out);
}
#endif
