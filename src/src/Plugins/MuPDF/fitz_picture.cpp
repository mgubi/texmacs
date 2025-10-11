/******************************************************************************
* MODULE     : fitz_picture.cpp
* DESCRIPTION: Picture objects for Fitz renderer
* COPYRIGHT  : (C) 2025 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "fitz_picture.hpp"
#include "fitz_renderer.hpp"

#include "file.hpp"
#include "image_files.hpp"
#include "effect.hpp"

/******************************************************************************
* Fitz picture implementation
******************************************************************************/

fitz_picture_rep::fitz_picture_rep (fz_pixmap *_pix, int ox2, int oy2):
  pix (_pix), im (NULL),
  w (fz_pixmap_width (get_fitz_context (), pix)),
  h (fz_pixmap_height (get_fitz_context (), pix)),
  ox (ox2), oy (oy2) {
  fz_keep_pixmap (get_fitz_context (), pix);
}

fitz_picture_rep::~fitz_picture_rep () {
  fz_context *ctx = get_fitz_context ();
  if (ctx) {
    if (pix) fz_drop_pixmap (ctx, pix);
    if (im) fz_drop_image (ctx, im);
  }
}

picture_kind fitz_picture_rep::get_type () { return picture_native; }
void* fitz_picture_rep::get_handle () { return (void*) this; }

int fitz_picture_rep::get_width () { return w; }
int fitz_picture_rep::get_height () { return h; }
int fitz_picture_rep::get_origin_x () { return ox; }
int fitz_picture_rep::get_origin_y () { return oy; }
void fitz_picture_rep::set_origin (int ox2, int oy2) { ox = ox2; oy = oy2; }

color
fitz_picture_rep::internal_get_pixel (int x, int y) {
  if (!pix) return rgb_color (0, 0, 0);
  unsigned char *samples = fz_pixmap_samples (get_fitz_context (), pix);
  return rgbap_to_argb (((color*)samples)[x + w * (h - 1 - y)]);
}

void
fitz_picture_rep::internal_set_pixel (int x, int y, color c) {
  if (!pix) return;
  unsigned char *samples = fz_pixmap_samples (get_fitz_context (), pix);
  ((color*)samples)[x + w * (h - 1 - y)] = argb_to_rgbap (c);
}

/******************************************************************************
* Factory functions
******************************************************************************/

picture
fitz_picture (fz_pixmap *_pix, int ox, int oy) {
  return (picture) tm_new<fitz_picture_rep> (_pix, ox, oy);
}

picture
as_fitz_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;

  fz_context *ctx = get_fitz_context ();
  if (!ctx) return pic;

  fz_pixmap *pix = fz_new_pixmap (ctx, fz_device_rgb (ctx),
                                  pic->get_width (), pic->get_height (),
                                  NULL, 1);
  if (!pix) return pic;

  picture ret = fitz_picture (pix, pic->get_origin_x (), pic->get_origin_y ());
  fz_drop_pixmap (ctx, pix);
  ret->copy_from (pic);
  return ret;
}

/******************************************************************************
* Image loading functions
******************************************************************************/

picture raw_load_xpm (url file_name); // Defined elsewhere in TeXmacs

fz_image *
fitz_load_image (url u) {
  fz_context *ctx = get_fitz_context ();
  if (!ctx) return NULL;

  fz_image *im = NULL;
  string suf = suffix (u);

  fz_try (ctx) {
    if (suf == "svg") {
      // SVG support could be added in the future
      // For now, fall through to conversion approach
    } else if ((suf == "jpg") || (suf == "jpeg") || (suf == "png") ||
               (suf == "gif") || (suf == "bmp") || (suf == "tiff")) {
      // Load supported image formats directly
      c_string path (concretize (u));
      im = fz_new_image_from_file (ctx, path);
    } else if (suf == "xpm") {
      // Try to load higher definition equivalents first
      url png_equiv = glue (unglue (u, 4), "_x4.png");
      if (exists (png_equiv)) {
        return fitz_load_image (png_equiv);
      }
      png_equiv = glue (unglue (u, 4), "_x2.png");
      if (exists (png_equiv)) {
        return fitz_load_image (png_equiv);
      }
      png_equiv = glue (unglue (u, 4), ".png");
      if (exists (png_equiv)) {
        return fitz_load_image (png_equiv);
      }

      // Load XPM using TeXmacs loader and convert to Fitz image
      picture xp = as_fitz_picture (raw_load_xpm (u));
      if (!is_nil (xp)) {
        fitz_picture_rep* rep = (fitz_picture_rep*) xp->get_handle ();
        if (rep && rep->pix) {
          im = fz_new_image_from_pixmap (ctx, rep->pix, NULL);
        }
      }
    }
  }
  fz_catch (ctx) {
    convert_warning << "fitz_picture: Failed to load image " << u << LF;
    im = NULL;
  }

  return im;
}

fz_pixmap*
fitz_load_pixmap (url u, int w, int h, tree eff, SI pixel) {
  fz_context *ctx = get_fitz_context ();
  if (!ctx) return NULL;

  fz_image *im = fitz_load_image (u);
  fz_pixmap *pix = NULL;

  if (im == NULL) {
    // Attempt to convert to PNG using TeXmacs image conversion
    fz_try (ctx) {
      url temp = url_temp (".png");
      image_to_png (u, temp, w, h);
      c_string path (as_string (temp));
      im = fz_new_image_from_file (ctx, path);
      remove (temp);
    }
    fz_catch (ctx) {
      convert_warning << "fitz_picture: Image conversion failed for " << u << LF;
    }
  }

  // Error handling
  if (im == NULL) {
    convert_warning << "fitz_picture: Cannot load " << concretize (u) << LF;
    return NULL;
  }

  fz_try (ctx) {
    // Get pixmap from image
    pix = fz_get_pixmap_from_image (ctx, im, NULL, NULL, NULL, NULL);

    // Check if scaling is needed
    int img_w = fz_pixmap_width (ctx, pix);
    int img_h = fz_pixmap_height (ctx, pix);

    if (w > 0 && h > 0 && (img_w != w || img_h != h)) {
      // Use Fitz scaling if needed
      fz_matrix scale_matrix = fz_scale ((float)w / img_w, (float)h / img_h);
      fz_pixmap *scaled_pix = fz_new_pixmap_from_pixmap (ctx, pix, NULL);
      // Note: Full scaling implementation would require more complex Fitz operations
      convert_warning << "fitz_picture: Image scaling not fully implemented for "
                      << u << LF;
    }

    // Apply effects if specified
    if (eff != "") {
      effect e = build_effect (eff);
      picture src = fitz_picture (pix, 0, 0);
      array<picture> a;
      a << src;
      picture pic = e->apply (a, pixel);
      picture dest = as_fitz_picture (pic);
      fitz_picture_rep* rep = (fitz_picture_rep*) dest->get_handle ();
      fz_pixmap* effect_pix = rep->pix;
      fz_keep_pixmap (ctx, effect_pix);
      fz_drop_pixmap (ctx, pix);
      pix = effect_pix;
    }
  }
  fz_always (ctx) {
    if (im) fz_drop_image (ctx, im);
  }
  fz_catch (ctx) {
    convert_error << "fitz_picture: Failed to process pixmap for " << u << LF;
    if (pix) {
      fz_drop_pixmap (ctx, pix);
      pix = NULL;
    }
  }

  return pix;
}

picture
fitz_load_picture (url file_name) {
  fz_context *ctx = get_fitz_context ();
  if (!ctx) return picture ();

  fz_image* fzim = fitz_load_image (file_name);
  if (!fzim) return picture ();

  fz_pixmap *pix = NULL;
  picture pic;

  fz_try (ctx) {
    pix = fz_get_pixmap_from_image (ctx, fzim, NULL, NULL, NULL, NULL);
    pic = fitz_picture (pix, 0, 0);
  }
  fz_always (ctx) {
    if (fzim) fz_drop_image (ctx, fzim);
    if (pix) fz_drop_pixmap (ctx, pix);
  }
  fz_catch (ctx) {
    convert_error << "fitz_picture: Failed to load picture " << file_name << LF;
    pic = picture ();
  }

  return pic;
}

/******************************************************************************
* Rendering on Fitz pictures
******************************************************************************/

class fitz_picture_renderer_rep: public fitz_renderer_rep {
public:
  picture pict;

public:
  fitz_picture_renderer_rep (picture pict, double zoom);
  void* get_data_handle ();
};

fitz_picture_renderer_rep::fitz_picture_renderer_rep (picture p, double zoom)
  : fitz_renderer_rep (), pict (p)
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

  // Set clipping coordinates (same as MuPDF renderer)
  cx1= 0;
  cy1= -ph * pixel;
  cx2= pw * pixel;
  cy2= 0;

  // Convert picture to Fitz format and begin rendering
  picture fitz_pict = as_fitz_picture (pict);
  fitz_picture_rep* handle = (fitz_picture_rep*) fitz_pict->get_handle ();
  begin (handle->pix);
}

void*
fitz_picture_renderer_rep::get_data_handle () {
  return (void*) this;
}

renderer
fitz_picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<fitz_picture_renderer_rep> (p, zoomf);
}
