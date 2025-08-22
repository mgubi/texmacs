/******************************************************************************
* MODULE     : vuw_widget.hpp
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

class vue_widget_rep : public widget_rep {
protected:
  blackbox data;
  string type;
  
public:
  vue_widget_rep (string _type, blackbox _data= NULL)
    : type (_type), data (_data) {};
  virtual ~vue_widget_rep () {};
  
  virtual void send (slot s, blackbox val);
  virtual blackbox query (slot s, int type_id);
  virtual widget read (slot s, blackbox index);
  virtual void write (slot s, blackbox index, widget w);
  virtual void notify (slot s, blackbox new_val);
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
  vue_simple_widget_rep ();
  ~vue_simple_widget_rep () {};
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
};

typedef vue_simple_widget_rep simple_widget_rep;
#endif

