/******************************************************************************
* MODULE     : fitz_picture.hpp
* DESCRIPTION: Picture objects for Fitz renderer
* COPYRIGHT  : (C) 2025
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FITZ_PICTURE_HPP
#define FITZ_PICTURE_HPP

#include "picture.hpp"
#include "url.hpp"
#include "basic_renderer.hpp"
#include "fitz_renderer.hpp"

#include <mupdf/fitz.h>

/******************************************************************************
* Color conversion utilities (same as MuPDF)
******************************************************************************/

// Convert from ARGB (TeXmacs color) to RGBA premultiplied (Fitz format)
inline color argb_to_rgbap (color c) {
  int r, g, b, a;
  get_rgb_color(c, r, g, b, a);
  r = ((r * a) / 255) & 0xFF;
  g = ((g * a) / 255) & 0xFF;
  b = ((b * a) / 255) & 0xFF;
  return (a << 24) + (b << 16) + (g << 8) + r;
}

// Convert from RGBA premultiplied (Fitz format) to ARGB (TeXmacs color)
inline color rgbap_to_argb (color c) {
  int r, g, b, a;
  a = (c >> 24) & 0xFF;
  b = (c >> 16) & 0xFF;
  g = (c >>  8) & 0xFF;
  r = (c >>  0) & 0xFF;
  if (a) {
    r = ((r * 255) / a) & 0xFF;
    g = ((g * 255) / a) & 0xFF;
    b = ((b * 255) / a) & 0xFF;
  } else {
    r = g = b = 0;
  }
  return rgb_color(r, g, b, a);
}

/******************************************************************************
* Fitz picture implementation
******************************************************************************/

class fitz_picture_rep: public picture_rep {
public:
  fz_pixmap *pix;
  fz_image  *im;
  int w, h;
  int ox, oy;

protected:
  color internal_get_pixel (int x, int y);
  void internal_set_pixel (int x, int y, color c);

public:
  fitz_picture_rep (fz_pixmap *_pix, int ox2, int oy2);
  ~fitz_picture_rep ();

  picture_kind get_type ();
  void* get_handle ();
  int get_width ();
  int get_height ();
  int get_origin_x ();
  int get_origin_y ();
  void set_origin (int ox2, int oy2);
};

/******************************************************************************
* Factory functions
******************************************************************************/

picture fitz_picture (fz_pixmap *pix, int ox, int oy);
picture as_fitz_picture (picture pic);

/******************************************************************************
* Image loading functions
******************************************************************************/

fz_image  *fitz_load_image (url u);
fz_pixmap *fitz_load_pixmap (url u, int w, int h, tree eff, SI pixel);
picture   fitz_load_picture (url file_name);

/******************************************************************************
* Picture renderer
******************************************************************************/

renderer fitz_picture_renderer (picture p, double zoomf);

#endif // defined FITZ_PICTURE_HPP