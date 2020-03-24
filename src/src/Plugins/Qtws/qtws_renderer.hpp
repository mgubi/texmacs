
/******************************************************************************
* MODULE     : qtws_renderer.hpp
* DESCRIPTION: QT WebSockets drawing interface class
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef QTWS_RENDERER_HPP
#define QTWS_RENDERER_HPP

#include "basic_renderer.hpp"
#include <QPainter>
#include <QPixmap>
#include <QImage>
#include <QtGlobal>
#include <QWidget>

// if QTMPIXMAPS is defined we use QPixmap for characters
// otherwise we use QImage (which support alpha also under X11)

#ifdef Q_OS_MAC
#define QTMPIXMAPS
#else
#undef QTMPIXMAPS
#endif

#ifdef QTMPIXMAPS
#define QTMImage QPixmap
#else
#define QTMImage QImage
#endif


class qtws_renderer_rep:  public basic_renderer_rep {
public:
  QPainter *painter; // FIXME: painter needs begin/end

public:
  qtws_renderer_rep (QPainter *_painter, int w = 0, int h = 0);
  ~qtws_renderer_rep ();
  void* get_handle ();

  void set_zoom_factor (double zoom);

  void begin (void* handle);
  void end ();

  //void set_extent (int _w, int _h) { w = _w; h = _h; }
  void get_extents (int& w, int& h);

  void set_transformation (frame fr);
  void reset_transformation ();

  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore = false);

  void  draw_bis (int char_code, font_glyphs fn, SI x, SI y);
  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  draw (const QFont& qfn, const QString& s, SI x, SI y, double zoom);
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

  void draw_clipped (QImage * im, int w, int h, SI x, SI y);
  void draw_clipped (QPixmap * im, int w, int h, SI x, SI y);
  
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);

  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  void draw_picture (picture pict, SI x, SI y, int alpha);
};

qtws_renderer_rep* the_qtws_renderer();
QImage* get_image (url u, int w, int h, tree eff, SI pixel);

class qt_shadow_renderer_rep: public qtws_renderer_rep {
public:
  QPixmap px;   
  qtws_renderer_rep *master;
  
public:
  qtws_shadow_renderer_rep (QPixmap _px = QPixmap());
  ~qtws_shadow_renderer_rep ();
  
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};

class qtws_proxy_renderer_rep: public qtws_renderer_rep {
public:
  qtws_renderer_rep *base;
  
public:
  qtws_proxy_renderer_rep (qtws_renderer_rep *_base)
  : qt_renderer_rep(_base->painter), base(_base) {}
  ~qtws_proxy_renderer_rep () {};
  
  void new_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
};

#endif // defined QTWS_RENDERER_HPP
