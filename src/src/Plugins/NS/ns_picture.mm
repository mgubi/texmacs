
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

ns_picture_rep::ns_picture_rep (NSBitmapImageRep *im, int ox2, int oy2) :
  pict (im), w ([im size].width), h ([im size].height), ox (ox2), oy (oy2) { [pict retain]; }

ns_picture_rep::~ns_picture_rep () { [pict release]; }

picture_kind ns_picture_rep::get_type () { return picture_native; }
void* ns_picture_rep::get_handle () { return (void*) this; }

int ns_picture_rep::get_width () { return w; }
int ns_picture_rep::get_height () { return h; }
int ns_picture_rep::get_origin_x () { return ox; }
int ns_picture_rep::get_origin_y () { return oy; }
void ns_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
ns_picture_rep::internal_get_pixel (int x, int y) {
  NSUInteger col;
  [pict getPixel:&col atX:x y:h - 1 - y];
  return (color) col;
}

void
ns_picture_rep::internal_set_pixel (int x, int y, color c) {
  NSUInteger col = c;
  [pict setPixel:&col atX:x y:h - 1 - y];
}

picture
ns_picture (NSBitmapImageRep* im, int ox, int oy) {
  return (picture) tm_new<ns_picture_rep,NSBitmapImageRep*,int,int> (im, ox, oy);
}

picture
as_ns_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  picture ret = native_picture(pic->get_width (), pic->get_height (),
                               pic->get_origin_x (), pic->get_origin_y ());
  ret->copy_from (pic);
  return ret;
}

picture
as_native_picture (picture pict) {
  return as_ns_picture (pict);
}

NSBitmapImageRep*
xpm_image (url file_name) {
  picture p= load_xpm (file_name);
  ns_picture_rep* rep= (ns_picture_rep*) p->get_handle ();
  return rep->pict;
}

picture
native_picture (int w, int h, int ox, int oy) {
  NSInteger pixelsWide = w;
  NSInteger pixelsHigh = h;
  // FIXME: maybe the following is not correct, I'm not sure about handling of alpha
  // (premultipiled in this configuration)
  NSBitmapImageRep* im =
    [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                            pixelsWide: pixelsWide
                                            pixelsHigh: pixelsHigh
                                         bitsPerSample: 8
                                       samplesPerPixel: 4
                                              hasAlpha: YES
                                              isPlanar: NO
                                        colorSpaceName: NSDeviceRGBColorSpace
                                           bytesPerRow: 4 * pixelsWide
                                          bitsPerPixel: 32];
  picture ret =  ns_picture (im, ox, oy);
  [im release];
  return ret;
}

void
ns_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  p= as_ns_picture (p);
  ns_picture_rep* pict= (ns_picture_rep*) p->get_handle ();
  int x0= pict->ox, y0= pict->h - 1 - pict->oy;
  decode (x, y);
  [pict->pict drawInRect: NSMakeRect(x - x0,  y - y0, pict->w, pict->h)
                fromRect: NSZeroRect
               operation: NSCompositingOperationSourceAtop
                fraction: (alpha/255.0)
          respectFlipped: YES hints: NULL];
}

/******************************************************************************
* Rendering on images
******************************************************************************/

ns_image_renderer_rep::ns_image_renderer_rep (picture p, double zoom) :
  ns_renderer_rep (), pict (p)
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
  NSBitmapImageRep* im = handle->pict;
  [NSGraphicsContext saveGraphicsState];
  [NSGraphicsContext setCurrentContext:[NSGraphicsContext graphicsContextWithBitmapImageRep:im]];

  [[NSColor colorWithWhite:0.0 alpha:1.0] drawSwatchInRect: NSMakeRect(0, 0, pw, ph)];
  //painter->begin (&im);
}

ns_image_renderer_rep::~ns_image_renderer_rep () {
  [NSGraphicsContext restoreGraphicsState];
}

void
ns_image_renderer_rep::set_zoom_factor (double zoom) {
  renderer_rep::set_zoom_factor (zoom);
}

void*
ns_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<ns_image_renderer_rep> (p, zoomf);
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
