
/******************************************************************************
* MODULE     : ns_renderer.mm
* DESCRIPTION: Cocoa drawing interface class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_cocoa.h"
#include "ns_renderer.h"
#include "analyze.hpp"
#include "image_files.hpp"
#include "file.hpp"
#include "frame.hpp"


#include "ns_utilities.h"
#include "MacOS/mac_images.h"


/******************************************************************************
 * Cocoa images
 ******************************************************************************/

struct ns_image_rep: concrete_struct {
  NSImage* img;
  SI xo,yo;
  int w,h;
  ns_image_rep (NSImage* img2, SI xo2, SI yo2, int w2, int h2)
    : img (img2), xo (xo2), yo (yo2), w (w2), h (h2) { [img retain]; };
  ~ns_image_rep()  {  [img release]; };
  friend class ns_image;
};

class ns_image {
  CONCRETE_NULL(ns_image);
  ns_image (NSImage* img2, SI xo2, SI yo2, int w2, int h2)
    : rep (tm_new <ns_image_rep> (img2, xo2, yo2, w2, h2)) {};
};

CONCRETE_NULL_CODE(ns_image);

/******************************************************************************
 * CG images
 ******************************************************************************/

struct cg_image_rep: concrete_struct {
	CGImageRef img;
	SI xo,yo;
	int w,h;
	cg_image_rep (CGImageRef img2, SI xo2, SI yo2, int w2, int h2) :
    img (img2), xo (xo2), yo (yo2), w (w2), h (h2) { CGImageRetain (img); };
	~cg_image_rep()  {  CGImageRelease (img); };
	friend class cg_image;
};

class cg_image {
	CONCRETE_NULL(cg_image);
	cg_image (CGImageRef img2, SI xo2, SI yo2, int w2, int h2):
    rep (tm_new <cg_image_rep> (img2, xo2, yo2, w2, h2)) {}	
};

CONCRETE_NULL_CODE(cg_image);


/******************************************************************************
 * Global support variables for all ns_renderers
 ******************************************************************************/

// bitmaps of all characters
static hashmap<basic_character,cg_image> character_image;
// image cache
static hashmap<string,ns_image> images; 

/******************************************************************************
 * ns_renderer
 ******************************************************************************/

ns_renderer_rep::ns_renderer_rep (int w2, int h2) :
  basic_renderer_rep (true, 1.0, w2, h2), context (NULL)
{
  reset_zoom_factor();
}

ns_renderer_rep::~ns_renderer_rep () {
  // NOTE: a shadow uses the context of its master, without begin (and the
  // editor deletes its shadows with tm_delete, not delete_shadow)
  if (context && master == NULL) end();
} ;

void 
ns_renderer_rep::begin (void * c) { 
  context = (NSGraphicsContext*)c;
  [NSGraphicsContext saveGraphicsState];
  [NSGraphicsContext setCurrentContext: context];
  [context retain];
//  CGContextBeginPage(context, NULL);
}

void 
ns_renderer_rep::end () { 
  if (!context) return;
  {
    CGContextRef ctx = [context CGContext];
    if (clip_pushed (ctx)) CGContextRestoreGState (ctx);
    clip_pushed_table->reset ((pointer) ctx);
  }
//  CGContextEndPage(context);
  [NSGraphicsContext restoreGraphicsState];
  [context release];
//  CGContextRelease(context); 
  context = NULL;  
}

void
ns_renderer_rep::set_zoom_factor (double zoom, bool safe) {
    renderer_rep::set_zoom_factor (retina_factor * zoom, safe);
    retina_pixel= pixel * retina_factor;
}

/******************************************************************************
 * Transformations
 ******************************************************************************/

void
ns_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");
  
  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);
  
  frame cv= scaling (point (pixel, -pixel), point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  //cout << "Set transformation " << o << ", " << ux << ", " << uy << "\n";
  {
    CGContextRef ctx = [context CGContext];
    CGAffineTransform tr = CGAffineTransformMake (ux[0], ux[1], uy[0], uy[1], o[0], o[1]);
    // NOTE: the clipping inside the transformation starts from this state
    clip_stack_pushed << clip_pushed (ctx);
    clip_pushed (ctx)= false;
    CGContextSaveGState (ctx);
    CGContextConcatCTM (ctx, tr);
  }
  rectangle nclip= fr [oclip];
  clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
ns_renderer_rep::reset_transformation () {
  unclip ();
  {
    CGContextRef ctx = [context CGContext];
    if (clip_pushed (ctx)) CGContextRestoreGState (ctx);
    CGContextRestoreGState (ctx);
    int n= N(clip_stack_pushed);
    if (n > 0) {
      clip_pushed (ctx)= clip_stack_pushed[n-1];
      clip_stack_pushed->resize (n-1);
    }
    else clip_pushed (ctx)= false;
    reapply_state ();
  }
}

/******************************************************************************
 * Clipping
 ******************************************************************************/

void
ns_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore)
{
  // NOTE: as QPainter::setClipRect, the new clipping replaces the previous
  // one; since CoreGraphics can only reduce the clipping region, the state
  // before the clipping is saved, and restored at the next clipping
  (void) restore;
  basic_renderer_rep::set_clipping (x1, y1, x2, y2);
  if (!context) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  CGContextRef ctx = [context CGContext];
  if (clip_pushed (ctx)) CGContextRestoreGState (ctx);
  CGContextSaveGState (ctx);
  clip_pushed (ctx)= true;
  if ((x1<x2) && (y2<y1))
    CGContextClipToRect (ctx, CGRectMake (x1,y2,x2-x1,y1-y2));
  else CGContextClipToRect (ctx, CGRectZero);
  reapply_state ();
}

hashmap<pointer,bool> ns_renderer_rep::clip_pushed_table (false);

bool&
ns_renderer_rep::clip_pushed (CGContextRef ctx) {
  // Whether the state before the clipping was saved (the shadows share the
  // context of their master, and thus this information)
  return clip_pushed_table ((pointer) ctx);
}

void
ns_renderer_rep::reapply_state () {
  // The pen and the brush after restoring the graphics state
  if (!is_nil (pen)) {
    pencil p= pen;
    brush b= fg_brush;
    if (!is_nil (b) && b->get_type () == brush_pattern) set_brush (b);
    set_pencil (p);
  }
}

/******************************************************************************
 * Drawing
 ******************************************************************************/
bool is_percentage (tree t, string s= "%");
double as_percentage (tree t);

static NSImage*
get_pattern_image (brush br, SI pixel) {
  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  NSImage* pm= get_image (u, w, h, eff, pixel);
  return pm;
}

NSColor *
to_nscolor (color col) {
  int r, g, b, a;
  get_rgb_color (col, r, g, b, a);
  if (get_reverse_colors ()) reverse (r, g, b);
  return [NSColor colorWithDeviceRed: r/255.0 green:g/255.0 blue:b/255.0 alpha:a/255.0];
}

color
to_color (const NSColor* c) {
  CGFloat r, g, b, a;
  [c getRed:&r green:&g blue:&b  alpha:&a];
  if (get_reverse_colors ()) {
    int ir, ig, ib;
    ir = (int)(r*255.0); ig = (int)(g*255.0); ib = (int)(b*255.0);
    reverse (ir, ig, ib);
    r = (ir/255.0); g = (ig/255.0); b = (ib/255.0);
  }
  return rgb_color (r, g, b, a);
}

static void
drawColoredPatternCallback (void *info, CGContextRef myContext) {
  NSImage *im = (NSImage*)info;
  if (im) {
    NSSize is = [im size];
    NSRect r = NSMakeRect (0,0,is.width,is.height);
    // NOTE: the tile is drawn in the context of the pattern
    CGImageRef cg= [im CGImageForProposedRect: &r context: nil hints: nil];
    if (cg) CGContextDrawImage (myContext, NSRectToCGRect (r), cg);
  }
}

static void
patternReleaseInfoCallback(void *info) {
  NSImage *im = (NSImage*)info;
  if (im) [im release];
}

static void
set_pattern (CGContextRef ctx, NSImage *pm, CGFloat pattern_alpha, double pox, double poy, bool fill) {
  NSSize pms = [pm size];
  //painter->setOpacity (qreal (pattern_alpha) / qreal (255));
  CGColorSpaceRef patternSpace = CGColorSpaceCreatePattern (NULL);
  if (fill) CGContextSetFillColorSpace (ctx, patternSpace);
  else CGContextSetStrokeColorSpace (ctx, patternSpace);
  CGColorSpaceRelease (patternSpace);
  struct CGPatternCallbacks callbacks = {
    0,
    drawColoredPatternCallback,
    patternReleaseInfoCallback
  };
  [pm retain];
  CGPatternRef pattern = CGPatternCreate (pm,
                                          CGRectMake (0, 0, pms.width, pms.height),
                                          // NOTE: y goes down (see draw_picture)
                                          CGAffineTransformMake (1, 0, 0, -1, pox, poy),
                                          pms.width, pms.height,
                                          kCGPatternTilingConstantSpacing,
                                          true, &callbacks);
  if (fill)
    CGContextSetFillPattern (ctx, pattern, &pattern_alpha);
  else
    CGContextSetStrokePattern (ctx, pattern, &pattern_alpha);
  CGPatternRelease (pattern);
}
  
void
ns_renderer_rep::set_pencil (pencil np) {
  // NOTE: as in the Qt interface, a pattern brush remains for the filling
  if (!context) { basic_renderer_rep::set_pencil (np); return; }
  CGContextSetAlpha ([context CGContext], 1.0);
  basic_renderer_rep::set_pencil (np);
  NSColor *c= to_nscolor (pen->get_color ());
  if (is_nil (fg_brush) || fg_brush->get_type () != brush_pattern) [c set];
  else [c setStroke];
  CGFloat pw= (CGFloat) (((double) pen->get_width ()) / ((double) pixel));
  [NSBezierPath setDefaultLineWidth:pw];
  if (np->get_type () == pencil_brush) {
    // following https://developer.apple.com/library/archive/documentation/GraphicsImaging/Conceptual/drawingwithquartz2d/dq_patterns/dq_patterns.html
    brush br= np->get_brush ();
    NSImage* pm= get_pattern_image (br, pixel);
    CGFloat pattern_alpha= br->get_alpha ()/255.0;
    if (pm != NULL) {
      CGContextRef ctx = [context CGContext];
      CGContextSetAlpha (ctx, pattern_alpha);
      double pox, poy;
      decode (0, 0, pox, poy);
      set_pattern (ctx, pm, 1.0, pox, poy, false);
    }
  }
  [NSBezierPath setDefaultLineCapStyle: (pen->get_cap () == cap_round? NSLineCapStyleRound : NSLineCapStyleButt)];
  [NSBezierPath setDefaultLineJoinStyle: NSLineJoinStyleRound];
}

void
ns_renderer_rep::set_brush (brush br) {
  if (!context) { basic_renderer_rep::set_brush (br); return; }
  CGContextRef ctx = [context CGContext];
  basic_renderer_rep::set_brush (br);
  if (br->get_type () == brush_none) {
    // FIXME: not sure what to do here
    //painter->setPen (QPen (Qt::NoPen));
    //painter->setBrush (QBrush (Qt::NoBrush));
  }
  else {
    CGColorRef col= [to_nscolor (pen->get_color ()) CGColor];
    CGContextSetStrokeColorWithColor (ctx, col);
    CGContextSetFillColorWithColor (ctx, col);
  }
  if (br->get_type () == brush_pattern) {
    NSImage* pm= get_pattern_image (br, pixel);
    int pattern_alpha= br->get_alpha ();
    CGContextSetAlpha (ctx, pattern_alpha / 255.0);
    if (pm != NULL) {
      double pox, poy;
      decode (0, 0, pox, poy);
      set_pattern (ctx, pm, 1.0, pox, poy, true);
    }
  }
}

void
ns_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  [NSBezierPath strokeLineFromPoint:NSMakePoint(rx1,ry1) toPoint:NSMakePoint(rx2,ry2)];
}

void
ns_renderer_rep::lines (array<SI> x, array<SI> y) {
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, NSPoint, n);
  for (i=0; i<n; i++) {
    decode (x[i], y[i], pnt[i].x, pnt[i].y);
  }
  NSBezierPath *path = [NSBezierPath bezierPath];
  [path appendBezierPathWithPoints:pnt count:n];
  [path setLineCapStyle:(pen->get_cap () == cap_round? NSLineCapStyleRound : NSLineCapStyleButt)];
  if (x[N(x)-1] == x[0] && y[N(y)-1] == y[0]) [path setLineCapStyle:NSLineCapStyleRound];
  [path setLineJoinStyle: NSLineJoinStyleRound];
  [path stroke];
  // XDrawLines (dpy, win, gc, pnt, n, CoordModeOrigin);
  STACK_DELETE_ARRAY (pnt);
  // NOTE: the path is autoreleased
}

void
ns_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  decode (x1, y1);
  decode (x2, y2);
  if ((x1>=x2) || (y1<=y2)) return;
	NSRect rect = NSMakeRect (x1,y2,x2-x1,y1-y2);
  [context saveGraphicsState];
  [to_nscolor (bg_brush->get_color ()) setFill];
  [NSBezierPath fillRect:rect];
  [context restoreGraphicsState];
}

void
ns_renderer_rep::clear_device (SI x1, SI y1, SI x2, SI y2) {
  // The neutral background, below transparent parts (as in Qt)
  static NSColor* neutral= nil;
  if (!neutral) {
    url u= resolve_pattern ("neutral-pattern.png");
    NSImage* im= nil;
    if (!is_none (u))
      im= [[[NSImage alloc] initWithContentsOfFile:
                              to_nsstring (concretize (u))] autorelease];
    if (im) neutral= [[NSColor colorWithPatternImage: im] retain];
    else neutral= [[NSColor lightGrayColor] retain];
  }
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  decode (x1, y1);
  decode (x2, y2);
  if ((x1>=x2) || (y1<=y2)) return;
  NSRect rect = NSMakeRect (x1,y2,x2-x1,y1-y2);
  [context saveGraphicsState];
  [[NSColor whiteColor] setFill];
  [NSBezierPath fillRect:rect];
  [neutral setFill];
  [NSBezierPath fillRect:rect];
  [context restoreGraphicsState];
}

void
ns_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if ((x2>x1) && ((x2-x1)<pixel)) {
    SI d= pixel-(x2-x1);
    x1 -= (d>>1);
    x2 += ((d+1)>>1);
  }
  if ((y2>y1) && ((y2-y1)<pixel)) {
    SI d= pixel-(y2-y1);
    y1 -= (d>>1);
    y2 += ((d+1)>>1);
  }
  
  x1= max (x1, cx1-ox); y1= max (y1, cy1-oy);
  x2= min (x2, cx2-ox); y2= min (y2, cy2-oy);
  // outer_round (x1, y1, x2, y2); might still be needed somewhere
  if ((x1>=x2) || (y1>=y2)) return;
  
  decode (x1, y1);
  decode (x2, y2);
  
  NSRect rect = NSMakeRect (x1,y2,x2-x1,y1-y2);
  [context saveGraphicsState];
  [to_nscolor (pen->get_color ()) setFill];
  [NSBezierPath fillRect:rect];
  [context restoreGraphicsState];
}

static NSBezierPath*
arc_path (double x1, double y1, double x2, double y2, int alpha, int delta) {
  // The arc of the ellipse inscribed in the rectangle, from alpha to
  // alpha+delta (in 64th of degrees, counterclockwise on the screen)
  NSBezierPath* p= [NSBezierPath bezierPath];
  [p appendBezierPathWithArcWithCenter: NSZeroPoint radius: 1.0
                            startAngle: alpha / 64.0
                              endAngle: (alpha + delta) / 64.0
                             clockwise: delta < 0];
  // NOTE: y goes down in the device coordinates
  NSAffineTransform* t= [NSAffineTransform transform];
  [t translateXBy: (x1 + x2) / 2 yBy: (y1 + y2) / 2];
  [t scaleXBy: (x2 - x1) / 2 yBy: - (y1 - y2) / 2];
  [p transformUsingAffineTransform: t];
  return p;
}

void
ns_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if ((x1>=x2) || (y1>=y2)) return;
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  NSBezierPath* p= arc_path (rx1, ry1, rx2, ry2, alpha, delta);
  [p setLineWidth: [NSBezierPath defaultLineWidth]];
  [p stroke];
}

void
ns_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  // NOTE: as in the Qt interface, the arc is closed by its chord
  if ((x1>=x2) || (y1>=y2)) return;
  double rx1, ry1, rx2, ry2;
  decode (x1, y1, rx1, ry1);
  decode (x2, y2, rx2, ry2);
  NSBezierPath* p= arc_path (rx1, ry1, rx2, ry2, alpha, delta);
  [p closePath];
  [p setWindingRule: NSWindingRuleNonZero];
  [context saveGraphicsState];
  if (is_nil (fg_brush) || fg_brush->get_type () != brush_pattern)
    [to_nscolor (pen->get_color ()) setFill];
  [p fill];
  [context restoreGraphicsState];
}

void
ns_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {  
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  STACK_NEW_ARRAY (pnt, NSPoint, n);
  for (i=0; i<n; i++) {
    decode (x[i], y[i], pnt[i].x, pnt[i].y);
  }
  
  NSBezierPath *path = [NSBezierPath bezierPath];
  [path appendBezierPathWithPoints: pnt count: n];
  [path closePath];
  [path setWindingRule: (convex? NSWindingRuleEvenOdd : NSWindingRuleNonZero)];
  [context saveGraphicsState];
  if (is_nil (fg_brush) || fg_brush->get_type () != brush_pattern)
    [to_nscolor (pen->get_color ()) setFill];
  [path fill];
  [context restoreGraphicsState];
  
  STACK_DELETE_ARRAY (pnt);
  // NOTE: the path is autoreleased
}

void
ns_renderer_rep::draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3) {
  array<SI> x (3), y (3);
  x[0]= x1; y[0]= y1;
  x[1]= x2; y[1]= y2;
  x[2]= x3; y[2]= y3;
  polygon (x, y, true);
}

/******************************************************************************
 * Image rendering
 ******************************************************************************/
#if 0 //old code
struct ns_cache_image_rep: cache_image_element_rep {
	ns_cache_image_rep (int w2, int h2, time_t time2, NSImage *ptr2) :
  cache_image_element_rep(w2,h2,time2,ptr2) {  [(NSImage*)ptr retain]; };
	virtual ~ns_cache_image_rep() {   [(NSImage*)ptr release]; };
};


void
ns_renderer_rep::image (url u, SI w, SI h, SI x, SI y,
                          double cx1, double cy1, double cx2, double cy2,
                          int alpha)
{
  // Given an image of original size (W, H),
  // we display the part (cx1 * W, xy1 * H, cx2 * W, cy2 * H)
  // at position (x, y) in a rectangle of size (w, h)
  
  // if (DEBUG_EVENTS) cout << "cg_renderer_rep::image " << as_string(u) << LF;
  (void) alpha; // FIXME
  
  w= w/pixel; h= h/pixel;
  decode (x, y);
  
  //painter.setRenderHints (0);
  //painter.drawRect (QRect (x, y-h, w, h));
  
  NSImage *pm = NULL;
  tree lookup= tuple (u->t);
  lookup << as_string (w ) << as_string (h )
  << as_string (cx1) << as_string (cy1)
  << as_string (cx2) << as_string (cy2) << "cg-image" ;
  cache_image_element ci = get_image_cache(lookup);
  if (!is_nil(ci)) {
    pm = static_cast<NSImage*> (ci->ptr);
  } else {
	  if (suffix (u) == "png") {
      // rendering
      string suu = as_string (u);
      // cout << suu << LF;
      pm = [[NSImage alloc] initWithContentsOfFile:to_nsstring(suu)];
	  } else if (suffix (u) == "ps" ||
               suffix (u) == "eps" ||
               suffix (u) == "pdf") {
      url temp= url_temp (".png");
      mac_image_to_png (u, temp, w, h);
//      system ("convert", u, temp);
      string suu = as_string (temp);
      pm = [[NSImage alloc] initWithContentsOfFile:to_nsstring(suu)];
      remove (temp);
    }
    
    if (pm == NULL ) {
      cout << "TeXmacs] warning: cannot render " << as_string (u) << "\n";
      return;
    }
    // caching
    ci = tm_new <ns_cache_image_rep> (w,h, texmacs_time(), pm);
    set_image_cache(lookup, ci);
    (ci->nr)++;
  }
  
  NSSize isz = [pm size];
  [pm setFlipped:YES];
//  [pm drawAtPoint:NSMakePoint(x,y) fromRect:NSMakeRect(0,0,w,h) operation:NSCompositeSourceAtop fraction:1.0];
  [pm drawInRect:NSMakeRect(x,y-h,w,h) fromRect:NSMakeRect(0,0,isz.width,isz.height) operation:NSCompositeSourceAtop fraction:1.0];
}
#endif


#if 0 // not used
void
ns_renderer_rep::draw_clipped (NSImage *im, int w, int h, SI x, SI y) {
  (void) w; (void) h;
  SI x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
  [im drawAtPoint:NSMakePoint (x,y) fromRect:NSMakeRect (0,0,w,h) operation:NSCompositingOperationSourceAtop fraction:1.0];
}  
#endif

void
ns_renderer_rep::draw_clipped (CGImageRef im, int w, int h, SI x, SI y) {
  (void) w; (void) h;
  SI x1=cx1-ox, y1=cy2-oy, x2= cx2-ox, y2= cy1-oy;
  decode (x , y );
  decode (x1, y1);
  decode (x2, y2);
  y--; // top-left origin to bottom-left origin conversion
  CGContextRef cgc = [[NSGraphicsContext currentContext] CGContext];
  CGRect r = CGRectMake (x,y,w,h);
//  CGContextSetShouldAntialias (cgc, true);
//  CGContextSaveGState (cgc);
  CGContextDrawImage (cgc, r, im);
//  CGContextRestoreGState (cgc);
}


static CGContextRef
MyCreateBitmapContext (int pixelsWide, int pixelsHigh) {
  // A transparent bitmap with premultiplied RGBA pixels, in the color space
  // of the backing store (the memory belongs to the context)
  if (pixelsWide < 1 || pixelsHigh < 1) return NULL;
  CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB ();
  CGContextRef context = CGBitmapContextCreate (NULL, pixelsWide, pixelsHigh, 8,
                                                0, colorSpace,
                                                kCGImageAlphaPremultipliedLast);
  CGColorSpaceRelease (colorSpace);
  if (context == NULL) return NULL;
  memset (CGBitmapContextGetData (context), 0,
          CGBitmapContextGetBytesPerRow (context) * pixelsHigh);
  return context;
}

// NOTE: the rows of the glyphs go down, and so does y in the device
// coordinates, so that the row j of a glyph is the row j from the bottom of
// its bitmap (the last row of its memory; see draw_clipped)

static inline unsigned char*
glyph_pixel (CGContextRef ic, int i, int j) {
  size_t h= CGBitmapContextGetHeight (ic);
  return ((unsigned char*) CGBitmapContextGetData (ic))
    + (h - 1 - j) * CGBitmapContextGetBytesPerRow (ic) + 4 * i;
}

void
ns_renderer_rep::draw_bis (int c, font_glyphs fng, SI x, SI y) {
  // draw with a pattern (as in the Qt interface)
  SI xo, yo;
  glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
  glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo, pixel_ratio);
  int w= gl->width, h= gl->height;
  CGContextRef ic = MyCreateBitmapContext (w, h);
  if (ic == NULL) return;
  SI tx= x- xo*std_shrinkf, ty= y+ yo*std_shrinkf;
  {
    // the pattern, at its place in the device coordinates
    brush br= pen->get_brush ();
    NSImage* pm= get_pattern_image (br, brushpx==-1? pixel: brushpx);
    if (pm != NULL) {
      double pox, poy;
      decode (0, 0, pox, poy);
      SI dx= tx, dy= ty;
      decode (dx, dy); dy--;
      set_pattern (ic, pm, br->get_alpha () / 255.0, pox - dx, poy - dy, true);
      CGContextFillRect (ic, CGRectMake (0, 0, w, h));
    }
  }
  {
    // the glyph as the alpha channel of the pattern
    int nr_cols= std_shrinkf*std_shrinkf;
    if (nr_cols >= 64) nr_cols= 64;
    bool rev= get_reverse_colors ();
    for (int j=0; j<h; j++)
      for (int i=0; i<w; i++) {
        unsigned char* p= glyph_pixel (ic, i, j);
        int col= gl->get_x (i, j);
        if (rev && p[3] != 0) {
          int a= p[3];
          int r= (255 * p[0] + a/2) / a;
          int g= (255 * p[1] + a/2) / a;
          int b= (255 * p[2] + a/2) / a;
          reverse (r, g, b);
          p[0]= (r * a + 127) / 255;
          p[1]= (g * a + 127) / 255;
          p[2]= (b * a + 127) / 255;
        }
        for (int k=0; k<4; k++) p[k]= (p[k] * col) / nr_cols;
      }
  }
  CGImageRef im = CGBitmapContextCreateImage (ic);
  CGContextRelease (ic);
  if (im == NULL) return;
  draw_clipped (im, w, h, tx, ty);
  CGImageRelease (im);
}

void
ns_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  if (pen->get_type () == pencil_brush) {
    draw_bis (c, fng, x, y);
    return;
  }
  
  // get the pixmap
  color fgc= pen->get_color ();
  basic_character xc (c, fng, std_shrinkf, fgc, 0);
  cg_image mi = character_image [xc];
  if (is_nil(mi)) {
    int r, g, b, a;
    get_rgb (fgc, r, g, b, a);
    if (get_reverse_colors ()) reverse (r, g, b);
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo, pixel_ratio);
    int i, j, w= gl->width, h= gl->height;
    CGImageRef im = NULL;
    CGContextRef ic = MyCreateBitmapContext (w, h);
    if (ic != NULL) {
      // the color of the pen, with the glyph as alpha (premultiplied)
      int nr_cols= std_shrinkf*std_shrinkf;
      if (nr_cols >= 64) nr_cols= 64;
      for (j=0; j<h; j++)
        for (i=0; i<w; i++) {
          int col = gl->get_x (i, j);
          int ca= (a*col)/nr_cols;
          unsigned char* p= glyph_pixel (ic, i, j);
          p[0]= (r * ca + 127) / 255;
          p[1]= (g * ca + 127) / 255;
          p[2]= (b * ca + 127) / 255;
          p[3]= ca;
        }
      im = CGBitmapContextCreateImage (ic);
      CGContextRelease (ic);
    }
    cg_image mi2 (im, xo, yo, w, h);
    mi = mi2;
    if (im) CGImageRelease (im); // cg_image retains im
    character_image (xc)= mi;
  }
  
  // draw the character
  if (mi->img)
    draw_clipped (mi->img, mi->w, mi->h, x- mi->xo*std_shrinkf, y+ mi->yo*std_shrinkf);
#if 0 // old code
  {
    CGContextRef cgc = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];

		(void) w; (void) h;
		int x1= x- mi->xo*std_shrinkf;
		int y1=  y+ mi->yo*std_shrinkf;
		decode (x1, y1);
		y1--; // top-left origin to bottom-left origin conversion
		CGRect r = CGRectMake(x1,y1,mi->w,mi->h);
		CGContextSetShouldAntialias (cgc, true);
		CGContextSaveGState (cgc);
		//  cg_set_color (context, cur_fg);
		CGContextClipToMask (cgc, r, mi->img); 
		CGContextFillRect (cgc, r);
		CGContextRestoreGState (cgc);
	}
#endif
}

#if 0 // old code
void ns_renderer_rep::draw (int c, font_glyphs fng, SI x, SI y) {
  // get the pixmap
  basic_character xc (c, fng, std_shrinkf, 0, 0);
  cg_image mi = character_image [xc];
  if (is_nil(mi)) {
    // cout << "CACHING:" << c << "\n" ;
    SI xo, yo;
    glyph pre_gl= fng->get (c); if (is_nil (pre_gl)) return;
    glyph gl= shrink (pre_gl, std_shrinkf, std_shrinkf, xo, yo);
    int i, j, w= gl->width, h= gl->height;
    NSImage *im = [[NSImage alloc] initWithSize:NSMakeSize(w,h)];
    int nr_cols= std_shrinkf*std_shrinkf;
    if (nr_cols >= 64) nr_cols= 64;

    [im lockFocus];
    for (j=0; j<h; j++)
      for (i=0; i<w; i++) {
        int col = gl->get_x (i, j);
        [[NSColor colorWithDeviceRed:0.0 green:0.0 blue:0.0 alpha: ((255*col)/(nr_cols+1))/255.0] set]; 
        [NSBezierPath fillRect:NSMakeRect(i,j,1,1)];
      }
    [im unlockFocus];
    
    ns_image mi2(im, xo, yo, w, h );
	mi = mi2;
    [im release]; // ns_image retains im
    character_image (xc)= mi;
    // FIXME: we must release the image at some point (this should be ok now, see ns_image)
  }
  
  // draw the character
  {
    CGContextRef cgc = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
    (void) w; (void) h;
    int x1= x- mi->xo*std_shrinkf;
    int y1=  y+ mi->yo*std_shrinkf;
    decode (x1, y1);
    y1--; // top-left origin to bottom-left origin conversion
    CGRect r = CGRectMake(x1,y1,mi->w,mi->h);
    CGContextSetShouldAntialias (cgc, true);
    CGContextSaveGState (cgc);
    //  ns_set_color (context, cur_fg);
    CGContextClipToMask (cgc, r, (CGImage*)(mi->img)); 
    CGContextFillRect (cgc, r);
    CGContextRestoreGState (cgc);
  }  

  
  // draw the character
//  draw_clipped (mi->img, mi->w, mi->h,
 //               x- mi->xo*std_shrinkf, y+ mi->yo*std_shrinkf);
}
#endif

#if 0 // old code
/******************************************************************************
* Setting up and displaying xpm pixmaps
******************************************************************************/

NSImage* xpm_init(url file_name)
{
  tree t= xpm_load (file_name);
  
  // get main info
  int ok, i=0, j, k, w, h, c, b, x, y;
  string s= as_string (t[0]);
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  if ((!ok) || (N(t)<(c+1)) || (c<=0)) {
    cerr << "File name= " << file_name << "\n";
    FAILED ("invalid xpm");
  }
  
  // setup colors
  string first_name;
  hashmap<string,color> pmcs;
  for (k=0; k<c; k++) {
    string s   = as_string (t[k+1]);
    string name= "";
    string def = "none";
    if (N(s)<b) i=N(s);
    else { name= s(0,b); i=b; }
    if (k==0) first_name= name;
    
    skip_spaces (s, i);
    if ((i<N(s)) && (s[i]=='s')) {
      i++;
      skip_spaces (s, i);
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      skip_spaces (s, i);
    }
    if ((i<N(s)) && (s[i]=='c')) {
      i++;
      skip_spaces (s, i);
      j=i;
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      def= locase_all (s (j, i));
    }

		pmcs(name)= xpm_to_color(def);
  }
  NSImage *im = [[NSImage alloc] initWithSize:NSMakeSize(w,h)];
	[im setFlipped:YES];
  [im lockFocus];
  [[NSGraphicsContext currentContext] setCompositingOperation: NSCompositeCopy];

  // setup pixmap
  for (y=0; y<h; y++) {
    if (N(t)< (y+c+1)) s= "";
    else s= as_string (t[y+c+1]);
    for (x=0; x<w; x++) {
      string name;
      if (N(s)<(b*(x+1))) name= first_name;
      else name= s (b*x, b*(x+1));
      if ((name == first_name) || !(pmcs->contains (name)))
        [[NSColor colorWithDeviceWhite:1.0 alpha:0.0] set] ;      
      else {
      color col = pmcs[name];
      ns_set_color (col);
        }
	  [NSBezierPath fillRect:NSMakeRect(x,y,1,1)];
    }
  }
  [im unlockFocus];
  return im;
}


extern int char_clip;

NSImage *
ns_renderer_rep::xpm_image(url file_name)
{ 
	NSImage *image = nil;
  ns_image mi = images [as_string(file_name)];
  if (is_nil(mi)) {    
		image = xpm_init(file_name);
    int w, h;
    NSSize imgSize = [image size];
    w = imgSize.width; h = imgSize.height;
		ns_image mi2(image,0,0,w,h);
		mi = mi2;
		images(as_string(file_name)) = mi2; 	
    [image release];
  }  
  else image = mi->img;
	return image;
}

void ns_renderer_rep::xpm (url file_name, SI x, SI y) {
  y -= pixel; // counter balance shift in draw_clipped
  
 // c_string chstr (as_string (file_name));
//  NSString *name = [NSString stringWithCString:chstr];
//  name = [[name stringByDeletingPathExtension] stringByAppendingPathExtension:@"png"];
  ///name = [name stringByDeletingPathExtension];
  NSImage *image = xpm_image(file_name);
  
  ASSERT (pixel == PIXEL, "pixel and PIXEL should coincide");
  int w, h;
  NSSize imgSize = [image size];
  w = imgSize.width; h = imgSize.height;

//  [(NSImageRep*)[[image representations] objectAtIndex:0]  drawAtPoint:NSMakePoint(x,y)];
  
  int old_clip= char_clip;
  char_clip= true;
  draw_clipped (image, w, h, x, y);
  char_clip=old_clip;
}
#endif

/******************************************************************************
 * main cocoa renderer
 ******************************************************************************/


ns_renderer_rep*
the_ns_renderer () {
  static ns_renderer_rep* the_renderer = NULL;
	if (!the_renderer) the_renderer = tm_new <ns_renderer_rep> ();
	return the_renderer;
}



/******************************************************************************
* Shadows
******************************************************************************/

// NOTE: as the proxy renderers of the Qt interface, the shadows draw directly
// in the graphics context of their master, which is buffered by the simple
// widget; the shadow borrows the context without retaining it

void
ns_renderer_rep::new_shadow (renderer& ren) {
  SI mw, mh, sw, sh;
  get_extents (mw, mh);
  if (ren != NULL) {
    ren->get_extents (sw, sh);
    if (sw != mw || sh != mh) {
      delete_shadow (ren);
      ren= NULL;
    }
  }
  if (ren == NULL) ren= (renderer) tm_new<ns_renderer_rep> (w, h);
}

void
ns_renderer_rep::delete_shadow (renderer& ren) {
  if (ren != NULL) {
    static_cast<ns_renderer_rep*> (ren)->context= NULL;
    tm_delete (ren);
    ren= NULL;
  }
}

void
ns_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  ns_renderer_rep* shadow= static_cast<ns_renderer_rep*> (ren);
  shadow->master = this;
  shadow->context= context;
  outer_round (x1, y1, x2, y2);
  x1= max (x1, cx1- ox);
  y1= max (y1, cy1- oy);
  x2= min (x2, cx2- ox);
  y2= min (y2, cy2- oy);
  shadow->ox= ox;
  shadow->oy= oy;
  if (x1<x2 && y1<y2) shadow->set_clipping (x1, y1, x2, y2);
}

void
ns_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  ASSERT (ren != NULL, "invalid renderer");
  if (ren->is_printer ()) return;
  // NOTE: nothing to copy, since the shadow draws in our own context
  if (context == static_cast<ns_renderer_rep*> (ren)->context) return;
  // FIXME: copy the region from shadows with their own context
  (void) x1; (void) y1; (void) x2; (void) y2;
}

void
ns_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  if (master == NULL) return;
  if (context == static_cast<ns_renderer_rep*> (master)->context) return;
  outer_round (x1, y1, x2, y2);
  decode (x1, y1);
  decode (x2, y2);
  static_cast<ns_renderer_rep*> (master)->encode (x1, y1);
  static_cast<ns_renderer_rep*> (master)->encode (x2, y2);
  master->put_shadow (this, x1, y1, x2, y2);
}
