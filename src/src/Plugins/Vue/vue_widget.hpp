/******************************************************************************
* MODULE     : vue_widget.hpp
* DESCRIPTION: Definition of Vue widgets
* COPYRIGHT  : (C) 2025  Masssimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef VUE_WIDGET_H
#define VUE_WIDGET_H

#include "widget.hpp"
#include "blackbox.hpp"

typedef quartet<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

class vue_window_rep;
typedef vue_window_rep *vue_window;

class vue_widget_rep : public widget_rep {
public:
  string type;
  unsigned int id;
  static unsigned int serial_id;
  
public:
  vue_widget_rep (string _type) : type (_type), id (serial_id++) {};
  virtual ~vue_widget_rep () {};
  
  // widget messages with TeXmacs
  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);
  widget read (slot s, blackbox index);
  void write (slot s, blackbox index, widget w);
  void notify (slot s, blackbox new_val);
  
  // layout and rendering
  virtual void do_layout () {};     // layout the widget
  virtual bool post_layout () {};   // postprocessing, returns true if relayout is needed
  virtual void render (void *data); // rendering callback
};

template<> void tm_delete<vue_widget_rep>(vue_widget_rep *);

class vue_widget {
public:
  ABSTRACT_NULL(vue_widget);

  inline bool operator == (vue_widget w) { return rep == w.rep; }
  inline bool operator != (vue_widget w) { return rep != w.rep; }
};
ABSTRACT_NULL_CODE(vue_widget);

inline widget abstract (vue_widget w) {
  return widget (w.rep);
}

inline vue_widget concrete (widget w) {
  return vue_widget (static_cast<vue_widget_rep*> (w.rep));
}

// comparison of widgets is needed by blackbox
inline bool operator==(const widget &lhs, const widget &rhs) {
  return lhs.rep == rhs.rep;
}

class vue_simple_widget_rep : public vue_widget_rep {
public:
  vue_window win; // the window, weak ref

  // properties set via messages
  coord2 size;
  rectangle extents;
  coord2 scroll_pos;
  coord2 scroll_momentum;
  time_t momentum_time;
  coord2 cursor_pos;
  bool mouse_grab;
  bool absolute_scroll;
  
  string debug_text; // debug view

  vue_simple_widget_rep ();
  ~vue_simple_widget_rep ();
  
  
  void send (slot s, blackbox val);
  blackbox query (slot s, int type_id);  
  widget read (slot s, blackbox index);
  
  void do_layout ();
  void render (void *data);

  // protocol for simple widgets to be used by the editor
  virtual bool is_editor_widget ();
  virtual bool is_embedded_widget ();
  virtual void handle_get_size_hint (SI& w, SI& h);
  virtual void handle_notify_resize (SI w, SI h);
  virtual void handle_keypress (string key, time_t t);
  virtual void handle_keyboard_focus (bool has_focus, time_t t);
  virtual void handle_mouse (string kind, SI x, SI y, int mods, time_t t,
                             array<double> data= array<double> ());
  virtual void handle_set_zoom_factor (double zoom);
  virtual void handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2);
  virtual void handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2);
  
  
  // backing store management
  
  static void repaint_all (); // called in the event loop
  static void repaint_all_in_window (vue_window win);
  
protected:
  static hashset<pointer> all_widgets;
  
  renderer     ren;
  rectangles   invalid_regions;
  picture      backing_store;
  coord2       backing_pos;
  bool         backing_valid;
  
  void invalidate_rect (int x1, int y1, int x2, int y2);
  void invalidate_viewport_rect (int x1, int y1, int x2, int y2);
  void invalidate_all ();
  bool is_invalid ();
  void repaint_invalid_regions ();
  void translate_backing_store (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy);
};

typedef vue_simple_widget_rep simple_widget_rep;

/*!
  A file/directory chooser dialog, using native dialogs where available.
  See @link widget.cpp @endlink for an explanation of send(), query(),
  read(), etc.
 */
class vue_chooser_widget_rep: public vue_widget_rep {
public:
  command cmd;           //!< Scheme closure to execute when the file is chosen
  command quit;          //!< Execute when the dialog closes.
  string type;           //!< File types to filter in the dialog
  string prompt;         //!< Is this a "Save" dialog?
  string win_title;      //!< Set by plain_window_widget()
  
  string directory; //!< Set this property sending SLOT_DIRECTORY to this widget
  coord2 position;  //!< Set this property sending SLOT_POSITION to this widget
  coord2 size;      //!< Set this property sending SLOT_SIZE to this widget
  string file;      //!< Set this property sending SLOT_FILE to this widget

public:
  vue_chooser_widget_rep (command, string, string);
  
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  
  void perform_dialog (vue_window win);
  void callback (char *res);
};

#endif

