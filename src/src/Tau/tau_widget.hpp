
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
  // the place, as the page told it (TeXmacs units, PIXEL per pixel)
  SI     place_w, place_h;     // size of the canvas
  SI     scroll_x, scroll_y;   // scroll position
  double zoom;                 // zoom factor
  bool   has_focus;            // the keyboard is here
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

  // all the views
  static hashset<pointer> all_widgets;
};

#endif // defined TAU_WIDGET_H
