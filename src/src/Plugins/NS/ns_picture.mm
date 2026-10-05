
/******************************************************************************
* MODULE     : ns_picture.cpp
* DESCRIPTION: NS pictures
* COPYRIGHT  : (C) 2013 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_cocoa.h"
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

// NOTE: the rows of the bitmaps go from the top to the bottom, with
// premultiplied RGBA pixels (see native_picture); the y coordinate of TeXmacs
// goes up

color
ns_picture_rep::internal_get_pixel (int x, int y) {
  unsigned char* p= [pict bitmapData] + (h - 1 - y) * [pict bytesPerRow] + 4 * x;
  int a= p[3];
  if (a == 0) return rgb_color (0, 0, 0, 0);
  return rgb_color ((255 * p[0] + a/2) / a, (255 * p[1] + a/2) / a,
                    (255 * p[2] + a/2) / a, a);
}

void
ns_picture_rep::internal_set_pixel (int x, int y, color c) {
  unsigned char* p= [pict bitmapData] + (h - 1 - y) * [pict bytesPerRow] + 4 * x;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  p[0]= (r * a + 127) / 255;
  p[1]= (g * a + 127) / 255;
  p[2]= (b * a + 127) / 255;
  p[3]= a;
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

static NSImage*
svg_icon (url file_name) {
  // The vector version of an icon, looked up as by the Qt and Vue interfaces:
  // name.svg in the light variant of the icon sets on $TEXMACS_PIXMAP_PATH.
  // The set chosen in the preferences (neo-classical by default) comes first
  // on the path, and its icons only exist as SVG files.
  if (suffix (file_name) != "xpm") return nil;
  url base= unglue (file_name, 4);
  url svg= resolve (url ("$TEXMACS_PIXMAP_PATH") * url ("light") *
                    glue (tail (base), ".svg") |
                    url ("$TEXMACS_PIXMAP_PATH") * glue (base, ".svg"));
  string sss;
  if (is_none (svg) || load_string (svg, sss, false) || sss == "") return nil;
  c_string buf (sss);
  NSData* data= [NSData dataWithBytes: (char*) buf length: N(sss)];
  return [[NSImage alloc] initWithData: data];
}

static NSBitmapImageRep*
render_icon (NSImage* im, int w, int h) {
  // The icon drawn at w x h pixels (retained)
  picture p= native_picture (w, h, 0, 0);
  NSBitmapImageRep* rep= ((ns_picture_rep*) p->get_handle ())->pict;
  [NSGraphicsContext saveGraphicsState];
  [NSGraphicsContext
   setCurrentContext: [NSGraphicsContext graphicsContextWithBitmapImageRep: rep]];
  [im drawInRect: NSMakeRect (0, 0, w, h)];
  [NSGraphicsContext restoreGraphicsState];
  return [rep retain];
}

NSBitmapImageRep*
xpm_image (url file_name) {
  // As in qt_load_xpm, the SVG version of the icon is drawn when there is
  // one, and otherwise its PNG equivalent (at double resolution on retina
  // screens); the size of the image is in points
  static hashmap<string,pointer> cache (NULL);
  string key= as_string (file_name);
  if (cache->contains (key)) return (NSBitmapImageRep*) cache[key];
  string sss;
  double f= 1.0;
  if (retina_icons > 1 && suffix (file_name) == "xpm") {
    url png_equiv= glue (unglue (file_name, 4), "_x2.png");
    load_string ("$TEXMACS_PIXMAP_PATH" * png_equiv, sss, false);
    if (sss != "") f= 2.0;
  }
  if (sss == "" && suffix (file_name) == "xpm") {
    url png_equiv= glue (unglue (file_name, 3), "png");
    load_string ("$TEXMACS_PIXMAP_PATH" * png_equiv, sss, false);
  }
  NSBitmapImageRep* im= nil;
  if (sss != "") {
    c_string buf (sss);
    NSData* data= [NSData dataWithBytes: (char*) buf length: N(sss)];
    im= [[NSBitmapImageRep alloc] initWithData: data];
    if (im) [im setSize: NSMakeSize ([im pixelsWide] / f, [im pixelsHigh] / f)];
  }
  if (NSImage* svg= svg_icon (file_name)) {
    // the size of the raster icon when there is one, since a few SVG files
    // declare the size of the drawing they were made from (the flags)
    NSSize sz= im ? [im size] : [svg size];
    int s= max (retina_icons, 1);
    NSBitmapImageRep* rep= render_icon (svg, (int) (s * sz.width + 0.5),
                                        (int) (s * sz.height + 0.5));
    [rep setSize: sz];
    [svg release];
    [im release];
    im= rep;
  }
  if (!im) {
    // FIXME: the conversion of the XPM pictures loses the transparency
    picture p= load_xpm (file_name);
    ns_picture_rep* rep= (ns_picture_rep*) p->get_handle ();
    im= [rep->pict retain];
  }
  cache (key)= (pointer) im;
  return im;
}

picture
native_picture (int w, int h, int ox, int oy) {
  // A transparent bitmap with premultiplied RGBA pixels
  NSInteger pixelsWide = max (w, 1);
  NSInteger pixelsHigh = max (h, 1);
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
  memset ([im bitmapData], 0, 4 * pixelsWide * pixelsHigh);
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
  // NOTE: y goes down in the device coordinates, and the pictures are stored
  // from the top to the bottom, so that they are drawn upside down
  CGContextRef ctx= [context CGContext];
  CGContextSaveGState (ctx);
  CGContextTranslateCTM (ctx, x - x0, y - y0 + pict->h);
  CGContextScaleCTM (ctx, 1.0, -1.0);
  [pict->pict drawInRect: NSMakeRect (0, 0, pict->w, pict->h)
                fromRect: NSZeroRect
               operation: NSCompositingOperationSourceOver
                fraction: (alpha/255.0)
          respectFlipped: NO hints: NULL];
  CGContextRestoreGState (ctx);
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
  cy1= -ph * pixel;
  cx2= pw * pixel;
  cy2= 0;

  // NOTE: as in the Qt interface, the picture is cleared first
  ns_picture_rep* handle = (ns_picture_rep*) pict->get_handle ();
  NSBitmapImageRep* im = handle->pict;
  memset ([im bitmapData], 0, [im bytesPerRow] * [im pixelsHigh]);
  begin ([NSGraphicsContext graphicsContextWithBitmapImageRep: im]);
}

ns_image_renderer_rep::~ns_image_renderer_rep () {
  // The renderer draws with y going down in a context where it goes up, so
  // that the rows are reversed at the end (see draw_picture)
  end ();
  ns_picture_rep* handle = (ns_picture_rep*) pict->get_handle ();
  NSBitmapImageRep* im = handle->pict;
  NSInteger bpr= [im bytesPerRow], n= [im pixelsHigh];
  unsigned char* data= [im bitmapData];
  STACK_NEW_ARRAY (row, unsigned char, bpr);
  for (NSInteger i=0; i < n/2; i++) {
    memcpy (row, data + i*bpr, bpr);
    memcpy (data + i*bpr, data + (n-1-i)*bpr, bpr);
    memcpy (data + (n-1-i)*bpr, row, bpr);
  }
  STACK_DELETE_ARRAY (row);
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

static NSImage*
load_image (url u, int w, int h) {
  // The image of the file (+1 reference), converted when NSImage cannot
  // read it
  NSString *fname = to_nsstring (concretize (u));
  NSImage *pm = [[NSImage alloc] initWithContentsOfFile: fname];
  if (!pm) {
    url temp= url_temp (".png");
    image_to_png (u, temp, w, h);
    fname = to_nsstring (concretize (temp));
    pm = [[NSImage alloc] initWithContentsOfFile: fname];
    remove (temp);
  }
  if (pm == nil)
    cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
  return pm;
}

static picture
load_picture_bis (url u, int w, int h, tree eff, int pixel) {
  // As get_image_for_real in the Qt interface: the image at the given size,
  // with the effect (a null picture when the file cannot be read)
  NSImage* im = load_image (u, w, h);
  if (im == nil) return picture ();
  if (w <= 0 || h <= 0) {
    NSSize sz= [im size];
    if (w <= 0) w= (int) ceil (sz.width);
    if (h <= 0) h= (int) ceil (sz.height);
  }
  picture p = native_picture (w, h, 0, 0);
  ns_picture_rep* handle= (ns_picture_rep*) p->get_handle ();
  NSBitmapImageRep* rep = handle->pict;
  [NSGraphicsContext saveGraphicsState];
  [NSGraphicsContext
   setCurrentContext: [NSGraphicsContext graphicsContextWithBitmapImageRep: rep]];
  [im drawInRect:NSMakeRect (0, 0, w, h)];
  [NSGraphicsContext restoreGraphicsState];
  [im release];
  if (eff != "") {
    effect e= build_effect (eff);
    array<picture> a;
    a << p;
    p= as_ns_picture (e->apply (a, pixel));
  }
  return p;
}

picture
load_picture (url u, int w, int h, tree eff, int pixel) {
  picture p= load_picture_bis (u, w, h, eff, pixel);
  if (is_nil (p)) return error_picture (w, h);
  return p;
}

static hashmap<tree,pointer> ns_pic_cache (NULL);

NSImage*
get_image (url u, int w, int h, tree eff, SI pixel) {
  // The tile of a pattern: the image at the size w x h (in pixels of the
  // device), with its effect; as in the Qt interface, the images are cached
  // (the cache owns them, the callers do not release them)
  tree key= tuple (as_tree (u), as_tree (w), as_tree (h));
  if (eff != "") key << eff << as_tree (pixel);
  if (ns_pic_cache->contains (key)) return (NSImage*) ns_pic_cache [key];
  NSImage* im= nil;
  picture p= load_picture_bis (u, w, h, eff, pixel);
  if (!is_nil (p)) {
    // NOTE: a single bitmap whose size (in points) is its number of pixels,
    // since the default space of the contexts of the renderer is in pixels
    ns_picture_rep* handle= (ns_picture_rep*) p->get_handle ();
    NSBitmapImageRep* rep= handle->pict;
    [rep setSize: NSMakeSize ([rep pixelsWide], [rep pixelsHigh])];
    im= [[NSImage alloc] initWithSize: [rep size]];
    [im addRepresentation: rep];
  }
  ns_pic_cache (key)= (pointer) im;
  return im;
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
  NSImage* svg= svg_icon (file_name);
  if (svg) {
    // drawn at the size in pixels of the raster icon when there is one
    // (see xpm_image)
    int s= max (retina_icons, 1);
    NSSize sz= NSMakeSize (s * [svg size].width, s * [svg size].height);
    if (sss != "") {
      c_string buf (sss);
      NSBitmapImageRep* r= [NSBitmapImageRep imageRepWithData:
                             [NSData dataWithBytes: (char*) buf length: N(sss)]];
      if (r) sz= NSMakeSize ([r pixelsWide], [r pixelsHigh]);
    }
    NSBitmapImageRep* rep= render_icon (svg, (int) (sz.width + 0.5),
                                        (int) (sz.height + 0.5));
    [svg release];
    picture p= ns_picture (rep, 0, 0);
    [rep release];
    return p;
  }
  if (sss == "")
    load_string ("$TEXMACS_PIXMAP_PATH" * file_name, sss, false);
  if (sss == "")
    load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", sss, true);
  c_string buf (sss);
  NSImage *im = [[NSImage alloc]
                 initWithData:[NSData dataWithBytes:(char*) buf length:N(sss)]];
  if (im) {
    NSSize sz = [im size];
    picture p = native_picture(sz.width, sz.height, 0, 0);
    ns_picture_rep* handle= (ns_picture_rep*) p->get_handle ();
    NSBitmapImageRep* rep = handle->pict;
    [NSGraphicsContext saveGraphicsState];
    [NSGraphicsContext
     setCurrentContext:[NSGraphicsContext graphicsContextWithBitmapImageRep:rep]];
    [im drawInRect:NSMakeRect (0, 0, sz.width, sz.height)];
    [NSGraphicsContext restoreGraphicsState];
    [im release];
    return p;
  }
  else
    return error_picture (10, 10);
}

/******************************************************************************
* Applying effects to existing pictures
******************************************************************************/

void
ns_apply_effect (tree eff, array<url> src, url dest, int w, int h) {
  array<picture> a;
  for (int i=0; i<N(src); i++)
    a << load_picture (src[i], w, h, tree (""), PIXEL);
  effect  e= build_effect (eff);
  picture t= e->apply (a, PIXEL);
  picture q= as_ns_picture (t);
  ns_picture_rep* pict= (ns_picture_rep*) q->get_handle ();
  
  NSBitmapImageFileType format = NSBitmapImageFileTypeTIFF;
  bool known= true;
  {
    string suf = suffix (dest);
    if (suf == "png") format = NSBitmapImageFileTypePNG;
    else if (suf == "gif") format = NSBitmapImageFileTypeGIF;
    else if (suf == "bmp") format = NSBitmapImageFileTypeBMP;
    else if (suf == "jpg") format = NSBitmapImageFileTypeJPEG;
    else if ((suf == "tif") || (suf == "tiff")) format = NSBitmapImageFileTypeTIFF;
    else known= false;
  }
  if (known) {
    NSData *png_data = [pict->pict representationUsingType: format properties: [NSDictionary dictionary]];
    [png_data writeToFile: to_nsstring_utf8 ( concretize (dest))
              atomically: NO];
  } else
    cout << "TeXmacs] warning: cannot save " << concretize (dest) << "\n";
}

void
save_picture (url dest, picture p) {
  picture q= as_ns_picture (p);
  ns_picture_rep* pict= (ns_picture_rep*) q->get_handle ();
  if (exists (dest)) remove (dest);
  string suf= suffix (dest);
  NSBitmapImageFileType format= NSBitmapImageFileTypePNG;
  if (suf == "jpg" || suf == "jpeg") format= NSBitmapImageFileTypeJPEG;
  else if (suf == "tif" || suf == "tiff") format= NSBitmapImageFileTypeTIFF;
  else if (suf == "bmp") format= NSBitmapImageFileTypeBMP;
  else if (suf == "gif") format= NSBitmapImageFileTypeGIF;
  NSData* data= [pict->pict representationUsingType: format
                                         properties: [NSDictionary dictionary]];
  [data writeToFile: to_nsstring_utf8 (concretize (dest)) atomically: NO];
}
