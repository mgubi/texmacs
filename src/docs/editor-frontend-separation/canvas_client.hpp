
/******************************************************************************
* MODULE     : canvas_client.hpp
* DESCRIPTION: What a GUI calls on the contents of a drawing area
*              (DRAFT, see docs/editor-frontend-separation.md; not compiled)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// Intended place: Graphics/Gui/canvas_client.hpp
//
// Today the editor (Edit/editor.hpp) and the box widgets of
// Texmacs/Window/tm_button.cpp inherit from simple_widget_rep, a typedef to
// a class of the GUI chosen at compile time (qt_simple_widget_rep,
// vue_simple_widget_rep, Widkit's simple_widget_rep). The virtual methods
// below are exactly the ones they override; they become an interface of the
// core, which the GUI calls through a pointer instead of by inheritance.
//
// The names keep the handle_ prefix so that the implementations in
// Edit/Interface/edit_interface.cpp and tm_button.cpp move without edits.

#ifndef CANVAS_CLIENT_H
#define CANVAS_CLIENT_H
#include "renderer.hpp"
#include "array.hpp"
#include "widget.hpp"

class canvas_host_rep;

class canvas_client_rep {
public:
  virtual ~canvas_client_rep () {}

  // The GUI gives the client its drawing area when the widget is created,
  // and NULL before destroying it. The client does not own the host.
  virtual void attach_host (canvas_host_rep* host) = 0;

  // Kind of contents, used by the GUI for layout decisions
  // (qt_simple_widget.cpp: scrollarea()->editor_flag; vue_widget.cpp:
  // sizing of embedded editors from their container).
  virtual bool is_editor_widget () { return false; }
  virtual bool is_embedded_widget () { return false; }

  // Size negotiation, in screen units (SI)
  virtual void handle_get_size_hint (SI& w, SI& h) = 0;
  virtual void handle_notify_resize (SI w, SI h) = 0;

  // Input. Keys are TeXmacs key strings ("C-x", "A-e", "space"...), already
  // composed by the GUI: dead keys and the macOS compose map are resolved
  // before handle_keypress is called (see the design notes, "Keyboard").
  virtual void handle_keypress (string key, time_t t) = 0;
  virtual void handle_keyboard_focus (bool has_focus, time_t t) = 0;
  // kind: "press-left", "release-right", "move", "dragging-left", "enter",
  // "leave", "drop"...; data: pressure and tilt, or a drop ticket.
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                             array<double> data) = 0;

  virtual void handle_set_zoom_factor (double zoom) = 0;

  // Drawing, on a renderer the GUI has already bound to its surface and
  // clipped to the rectangle (x1, y1)-(x2, y2) in canvas coordinates.
  virtual void handle_clear (renderer ren, SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void handle_repaint (renderer ren, SI x1, SI y1, SI x2, SI y2) = 0;
};

// Every GUI provides this factory (declared next to the others in
// Graphics/Gui/widget.hpp). The widget wraps the client: it forwards the
// handle_ calls to it and implements canvas_host_rep for it. It does not
// own the client; whoever created the client destroys it after the widget.
widget canvas_widget (canvas_client_rep* client);

#endif // defined CANVAS_CLIENT_H
