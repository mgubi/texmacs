
/******************************************************************************
* MODULE     : tau_widget.hpp
* DESCRIPTION: The place of a view, as the core of Tau sees it
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
* In Tau the editor runs without a GUI (docs/tau-design.md). The editor is
* still written as a widget of the GUI: simple_widget_rep is what it derives
* from. Here it is no widget of anything: it keeps the state of the place
* where the page shows the view (size, scroll position, zoom) and what the
* editor asked of it (extents, invalid regions, cursor), and answers the
* questions of the editor from that state.
******************************************************************************/

#ifndef TAU_WIDGET_H
#define TAU_WIDGET_H

#include "widget.hpp"
#include "message.hpp"
#include "renderer.hpp"
#include "rectangles.hpp"
#include "hashset.hpp"
#include "ntuple.hpp"

typedef quartet<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

class simple_widget_rep: public widget_rep {
public:
  int    id;                   // the number of the view, for the page
  // the place, as the page told it
  int    px_w, px_h;           // size of the canvas, in pixels of the screen
  double density;              // pixels of the screen per pixel of TeXmacs
  int    place_counter;        // the number of this place (see set_place)
  SI     place_w, place_h;     // size of the canvas, in TeXmacs units
  SI     scroll_x, scroll_y;   // scroll position asked for: the top left
                               // corner of the canvas in the document (y up)
  bool   absolute_scroll;      // ... or its centre, when the editor asked
  double zoom;                 // zoom factor
  bool   has_focus;            // the keyboard is here
  bool   resize_pending;       // the editor is not told of the size yet
  bool   shown;                // attached to a window of the core
  // the pixels of the canvas
  picture  backing;
  renderer ren;
  SI       backing_x, backing_y; // the scroll position they are drawn for
  int      drawn_x1, drawn_y1, drawn_x2, drawn_y2; // the pixels which the
                                   // last repaint changed
  // what the editor asked
  rectangle  extents;          // of the document
  rectangles invalid;          // regions to draw again
  bool       invalid_all;
  SI         cursor_x, cursor_y;
  bool       mouse_grab;
  string     pointer_name;     // shape of the pointer over the view

  simple_widget_rep ();
  ~simple_widget_rep ();

  void     send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  widget   read (slot s, blackbox index);
  void     write (slot s, blackbox index, widget w);
  void     notify (slot s, blackbox new_val);

  // the protocol of the editor
  virtual bool is_editor_widget ();
  virtual bool is_embedded_widget ();
  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                             array<double> data= array<double> ());
  virtual void handle_set_zoom_factor (double zoom);
  virtual void handle_clear (renderer ren, SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (renderer ren, SI x1, SI y1, SI x2, SI y2);

  // the place of the view (docs/tau-design.md, "place")
  void set_place (int w, int h, double density, int counter);
  void scroll_by (int dx, int dy);       // in pixels of the screen, y down
  void to_document (SI& x, SI& y);       // from pixels of the canvas
  void notify_resize ();                 // tell the editor, if needed
  bool repaint ();                       // draw what is invalid; true if
                                         // the pixels changed
  unsigned char* pixels ();              // RGBA, px_w * px_h * 4 bytes
  void extents_in_pixels (int& w, int& h, int& sx, int& sy);
  void cursor_in_pixels (int& x, int& y);

  // all the views
  static hashset<pointer> all_widgets;
  static simple_widget_rep* find (int id);
};

#endif // defined TAU_WIDGET_H
