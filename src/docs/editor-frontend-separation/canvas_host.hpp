
/******************************************************************************
* MODULE     : canvas_host.hpp
* DESCRIPTION: What the contents of a drawing area ask of the GUI
*              (DRAFT, see docs/editor-frontend-separation.md; not compiled)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// Intended place: Graphics/Gui/canvas_host.hpp
//
// Implemented by the widget which canvas_widget (client) returns, in each
// GUI. It gathers what the editor does today through widget messages to
// itself (send_invalidate (this, ...) in edit_interface.cpp), through the
// canvas of its window (::get_canvas (widget (cvw))), and the few places
// where Edit/ tests which GUI it is compiled for.

#ifndef CANVAS_HOST_H
#define CANVAS_HOST_H
#include "rectangles.hpp"

class canvas_host_rep {
public:
  virtual ~canvas_host_rep () {}

  // Repainting (today send_invalidate, send_invalidate_all, query_invalid).
  // Coordinates are relative to the top left corner of the canvas.
  virtual void invalidate (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void invalidate_all () = 0;
  virtual bool has_invalid_regions () = 0;

  // Keyboard and pointer (today send_keyboard_focus, send_keyboard_focus_on,
  // send_mouse_grab, send_mouse_pointer, send_cursor).
  virtual void request_keyboard_focus (bool get_focus) = 0;
  virtual void request_keyboard_focus_on (string field) = 0;
  virtual void set_mouse_grab (bool grab) = 0;
  virtual void set_mouse_pointer (string name, string mask_name) = 0;
  virtual void set_cursor (SI x, SI y) = 0; // for input methods

  // Geometry (today ::get_size / ::get_position of get_canvas (cvw), and
  // ::get_position of get_window (this)).
  virtual void get_canvas_size (SI& w, SI& h) = 0;
  virtual void get_canvas_position (SI& x, SI& y) = 0;
  virtual void get_window_position (SI& x, SI& y) = 0;

  // Scrolling. Today these go through the server and the current view
  // (SERVER (set_extents (...)) -> tm_frame_rep -> tm_window_rep ->
  // ::set_extents (wid, ...)), although they only concern the canvas.
  virtual void set_extents (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void get_extents (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void get_visible (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void scroll_to (SI x, SI y) = 0;
  virtual void scroll_where (SI& x, SI& y) = 0;
  virtual void set_scrollbars (int policy) = 0;

  // Properties of the GUI which Edit/ reads through #ifdefs today
  // (edit_interface.cpp, edit_main.cpp). One question each, answered at run
  // time, so that the editor is compiled once for every GUI.

  // Width of a scroll bar, in SI. Qt asks its style (PM_ScrollBarExtent);
  // the others use 20 * PIXEL.
  virtual SI scrollbar_width () = 0;
  // The canvas draws a frame of 1 pixel around the document, and centers
  // documents narrower than itself. True for X11, Widkit/Qtwk, SDL and Vue;
  // false for Qt, whose scroll area does both itself.
  virtual bool frames_and_centers_document () = 0;
  // Selections are painted as filled rectangles, so their invalidated
  // regions need not be thinned to an outline. True for Qt, SDL and Vue.
  virtual bool fills_selections () = 0;
  // The GUI can rasterize a document to PNG, JPEG or TIFF when printing to
  // a bitmap file. True for Qt and Vue.
  virtual bool can_print_bitmaps () = 0;
};

#endif // defined CANVAS_HOST_H
