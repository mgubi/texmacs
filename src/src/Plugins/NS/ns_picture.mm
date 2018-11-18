
/******************************************************************************
* MODULE     : ns_picture.cpp
* DESCRIPTION: NS pictures
* COPYRIGHT  : (C) 2013 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
#include "ns_picture.h"
#include "analyze.hpp"
#include "image_files.hpp"
//#include "qt_utilities.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "scheme.hpp"
#include "frame.hpp"
#include "effect.hpp"

#include "ns_utilities.h"

/******************************************************************************
* Abstract Qt pictures
******************************************************************************/

ns_picture_rep::ns_picture_rep (NSImage *im, int ox2, int oy2):
  pict (im), w ([im size].width), h ([im size].height), ox (ox2), oy (oy2) {}

picture_kind ns_picture_rep::get_type () { return picture_native; }
void* ns_picture_rep::get_handle () { return (void*) this; }

int ns_picture_rep::get_width () { return w; }
int ns_picture_rep::get_height () { return h; }
int ns_picture_rep::get_origin_x () { return ox; }
int ns_picture_rep::get_origin_y () { return oy; }
void ns_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
ns_picture_rep::internal_get_pixel (int x, int y) {
    [pict ]
  return (color) pict.pixel (x, h - 1 - y);
}

void
ns_picture_rep::internal_set_pixel (int x, int y, color c) {
  pict.setPixel (x, h - 1 - y, c);
}

picture
ns_picture (const QImage& im, int ox, int oy) {
  return (picture) tm_new<ns_picture_rep,QImage,int,int> (im, ox, oy);
}

picture
as_ns_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  picture ret= ns_picture (QImage (pic->get_width (), pic->get_height (),
                                   QImage::Format_ARGB32),
                           pic->get_origin_x (), pic->get_origin_y ());
  ret->copy_from (pic);
  return ret;
}

picture
as_native_picture (picture pict) {
  return as_ns_picture (pict);
}

QImage*
xpm_image (url file_name) {
  picture p= load_xpm (file_name);
  ns_picture_rep* rep= (ns_picture_rep*) p->get_handle ();
  return &(rep->pict);
}

picture
native_picture (int w, int h, int ox, int oy) {
  return ns_picture (QImage (w, h, QImage::Format_ARGB32), ox, oy);
}

void
qt_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  p= as_ns_picture (p);
  ns_picture_rep* pict= (ns_picture_rep*) p->get_handle ();
  int x0= pict->ox, y0= pict->h - 1 - pict->oy;
  decode (x, y);
  qreal old_opacity= painter->opacity ();
  painter->setOpacity (qreal (alpha) / qreal (255));
  painter->drawImage (x - x0, y - y0, pict->pict);
  painter->setOpacity (old_opacity);
}

/******************************************************************************
* Rendering on images
******************************************************************************/

qt_image_renderer_rep::qt_image_renderer_rep (picture p, double zoom):
  qt_renderer_rep (new QPainter ()), pict (p)
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
  cx1= 0;
  cy1= 0;
  cx2= pw * pixel;
  cy2= ph * pixel;

  ns_picture_rep* handle= (ns_picture_rep*) pict->get_handle ();
  QImage& im (handle->pict);
#if (QT_VERSION >= 0x040800)
  im.fill (QColor (0, 0, 0, 0));
#else
  im.fill ((uint) 0);
#endif
  painter->begin (&im);
}

qt_image_renderer_rep::~qt_image_renderer_rep () {
  painter->end();
  delete painter;
  painter = NULL;
}

void
qt_image_renderer_rep::set_zoom_factor (double zoom) {
  renderer_rep::set_zoom_factor (zoom);
}

void*
qt_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<qt_image_renderer_rep> (p, zoomf);
}

/******************************************************************************
* Loading pictures
******************************************************************************/

QImage*
get_image (url u, int w, int h) {
  QImage *pm = NULL;
  if (qt_supports (u))
    pm= new QImage (os8bits_to_qstring (concretize (u)));
  else {
    url temp= url_temp (".png");
    image_to_png (u, temp, w, h);
    pm= new QImage (os8bits_to_qstring (as_string (temp)));
    remove (temp);
  }
  if (pm == NULL || pm->isNull ()) {
      if (pm != NULL) delete pm;
      cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
      return NULL;
  }
  if (pm->width () != w || pm->height () != h)
    (*pm)= pm->scaled (w, h);
  return pm;
}

picture
load_picture (url u, int w, int h) {
  QImage* im= get_image (u, w, h);
  if (im == NULL) return error_picture (w, h);
  return ns_picture (*im, 0, 0);
}

picture
qt_load_xpm (url file_name) {
  string sss;
  if (retina_icons > 1 && suffix (file_name) == "xpm") {
    url png_equiv= glue (unglue (file_name, 4), "_x2.png");
    load_string ("$TEXMACS_PIXMAP_PATH" * png_equiv, sss, false);
  }
  if (sss == "" && suffix (file_name) == "xpm") {
    url png_equiv= glue (unglue (file_name, 3), "png");
    load_string ("$TEXMACS_PIXMAP_PATH" * png_equiv, sss, false);
  }
  if (sss == "")
    load_string ("$TEXMACS_PIXMAP_PATH" * file_name, sss, false);
  if (sss == "")
    load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", sss, true);
  c_string buf (sss);
  QImage pm;
  pm.loadFromData ((uchar*) (char*) buf, N(sss));
  return ns_picture (pm, 0, 0);
}

/******************************************************************************
* Applying effects to existing pictures
******************************************************************************/

void
qt_apply_effect (tree eff, array<url> src, url dest, int w, int h) {
  array<picture> a;
  for (int i=0; i<N(src); i++)
    a << load_picture (src[i], w, h);
  effect  e= build_effect (eff);
  picture t= e->apply (a, PIXEL);
  picture q= as_ns_picture (t);
  ns_picture_rep* pict= (ns_picture_rep*) q->get_handle ();
  pict->pict.save (os8bits_to_qstring (concretize (dest)));
}
