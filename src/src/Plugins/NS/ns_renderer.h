
/******************************************************************************
* MODULE     : ns_renderer.hpp
* DESCRIPTION: Cocoa drawing interface class
* COPYRIGHT  : (C) 2006,2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NS_RENDERER_H
#define NS_RENDERER_H

#include "basic_renderer.hpp"

class ns_renderer_rep:  public basic_renderer_rep {
public:
  NSGraphicsContext *context;
  
  ns_renderer_rep (int w = 0, int h = 0);
  ~ns_renderer_rep ();
    
  void set_zoom_factor (double zoom);
    
  void begin (void* handle);
  void end ();


  void set_transformation (frame fr);
  void reset_transformation ();
   
  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore = false);

  void  draw_bis (int char_code, font_glyphs fn, SI x, SI y);
  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  //void  draw (const QFont& qfn, const QString& s, SI x, SI y, double zoom);
  void  set_pencil (pencil p);
  void  set_brush (brush b);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  void  draw_triangle (SI x1, SI y1, SI x2, SI y2, SI x3, SI y3);
    
  void draw_clipped (NSImage * im, int w, int h, SI x, SI y);
    
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
    
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);
    
  void draw_picture (picture pict, SI x, SI y, int alpha);
};

ns_renderer_rep *the_ns_renderer();

NSImage* get_image (url u, int w, int h);


#endif // defined NS_RENDERER_H
