
/******************************************************************************
* MODULE     : ns_picture.hpp
* DESCRIPTION: NS pictures
* COPYRIGHT  : (C) 2013 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NS_PICTURE_HPP
#define NS_PICTURE_HPP

#include "ns_renderer.h"

class ns_picture_rep: public picture_rep {
public:
  NSBitmapImageRep *pict;
  int w, h;
  int ox, oy;

protected:
  color internal_get_pixel (int x, int y);
  void internal_set_pixel (int x, int y, color c);

public:
  ns_picture_rep (NSBitmapImageRep *im, int ox2, int oy2);
  ~ns_picture_rep ();
  picture_kind get_type ();
  void* get_handle ();
  int get_width ();
  int get_height ();
  int get_origin_x ();
  int get_origin_y ();
  void set_origin (int ox2, int oy2);
};

picture ns_picture (NSBitmapImageRep *im, int ox, int oy);
NSBitmapImageRep* xpm_image (url file_name);

class ns_image_renderer_rep: public ns_renderer_rep {
public:
  picture pict;
  int x1, y1, x2, y2;
  
public:
  ns_image_renderer_rep (picture pict, double zoom);
  ~ns_image_renderer_rep ();
  void set_zoom_factor (double zoom);
  void* get_data_handle ();
};

#endif // defined NS_PICTURE_HPP
