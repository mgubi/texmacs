
/******************************************************************************
* MODULE     : ns_widget.h
* DESCRIPTION: Aqua widget class
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NS_WIDGET_H
#define NS_WIDGET_H

#include "widget.hpp"

#ifndef MAC_COCOA_H
typedef struct TeXmacs_NSView {
  void * isa;
}  NSView ; // hack to allow inclusion in pure C++ sources
//#else
//typedef NSView * NSViewPtr ; 
typedef void *TMMenuItem;
#else
@class TMMenuItem;
#endif


class ns_widget_rep : public widget_rep {
protected:
  array<widget> children;
  ns_widget_rep  *parent;
  
public:
  long                id;
  
  /*! A list of all supported widget types.
   FIXME: This enum breaks the basic inheritance rules, since we have to
   update the base class each time we implement a new subclass. It's also some
   sort of bastardic and lame RTTI, which might be proof of bad design...
   But it comes handy in a few places right now ;)
   NOTE: please modify ns_widget_type_strings[] in type_as_string() accordingly!
   */
  enum types {
    none = 0,
    input_widget,    file_chooser,       window_widget,      view_widget,
    horizontal_menu, vertical_menu,      horizontal_list,    vertical_list,
    tile_menu,       minibar_menu,       menu_separator,     menu_group,
    pulldown_button, pullright_button,   menu_button,        balloon_widget,
    text_widget,     xpm_widget,         toggle_widget,      enum_widget,
    choice_widget,   scrollable_widget,  hsplit_widget,      vsplit_widget,
    aligned_widget,  tabs_widget,        icon_tabs_widget,   wrapped_widget,
    refresh_widget,  refreshable_widget, glue_widget,        resize_widget,
    texmacs_widget,  simple_widget,      embedded_tm_widget, popup_widget,
    field_widget,    filtered_choice_widget, tree_view_widget
  } ;
  
  types type;
  
  ns_widget_rep (types _type=none);
  virtual ~ns_widget_rep ();
  virtual inline string get_nickname () { return "popup"; }
  
  virtual widget plain_window_widget (string name, command quit);
  virtual widget make_popup_widget ();
  virtual widget popup_window_widget (string s);
  
  void add_child (widget a);
  void add_children (array<widget> a);
  
  ////////////////////// NS semantics of abstract texmacs widgets
  
  virtual NSView*  as_nsview ();
  
  //virtual QAction*         as_qaction ();
  //virtual QWidget*         as_qwidget ();
  //virtual QLayoutItem*     as_qlayoutitem ();
  //virtual QList<QAction*>* get_qactionlist();
  
  
  ////////////////////// Debugging
  
  string type_as_string() {
    static const char* ns_widget_type_strings[] = {
      "none",
      "input_widget",       "file_chooser",       "window_widget",
      "view_widget",        "horizontal_menu",    "vertical_menu",
      "horizontal_list",    "vertical_list",      "tile_menu",
      "minibar_menu",       "menu_separator",     "menu_group",
      "pulldown_button",    "pullright_button",   "menu_button",
      "balloon_widget",     "text_widget",        "xpm_widget",
      "toggle_widget",      "enum_widget",        "choice_widget",
      "scrollable_widget",  "hsplit_widget",      "vsplit_widget",
      "aligned_widget",     "tabs_widget",        "icon_tabs_widget",
      "wrapped_widget",     "refresh_widget",     "refreshable_widget",
      "glue_widget",        "resize_widget",      "texmacs_widget",
      "simple_widget",      "embedded_tm_widget", "popup_widget",
      "field_widget",       "filtered_choice_widget", "tree_view_widget"
    };
    return string (ns_widget_type_strings[type]) * "\t id: " * as_string (id);
  }
  
  ////////////////////// Handling of TeXmacs' messages
  
  /// See widkit_wrapper.cpp for the reference list of slots. Based on the
  /// handlers invoked by wk_widget_rep::send(), query() etc. we can decide
  /// what slots must implement each ns_widget.
  virtual void send (slot s, blackbox val);
  
  virtual blackbox query (slot s, int type_id) {
    (void) type_id;
    if (DEBUG_QT)
      debug_qt << "ns_widget_rep::query(), unhandled " << slot_name (s)
      << " for widget of type: " << type_as_string() << LF;
    return blackbox ();
  }
  
  virtual widget read (slot s, blackbox index) {
    (void) index;
    if (DEBUG_QT)
      debug_qt << "ns_widget_rep::read(), unhandled " << slot_name (s)
      << " for widget of type: " << type_as_string() << LF;
    return widget ();
  }
  
  virtual void write (slot s, blackbox index, widget w) {
    (void) index; (void) w;
    if (DEBUG_QT)
      debug_qt << "ns_widget_rep::write(), unhandled " << slot_name (s)
      << " for widget of type: " << type_as_string() << LF;
  }
  
  virtual void notify (slot s, blackbox new_val) {
    (void) new_val;
    if (DEBUG_QT)
      debug_qt << "ns_widget_rep::notify(), unhandled " << slot_name (s)
      << " for widget of type: " << type_as_string() << LF;
  }
};


/*! Reference counting mechanism.
 
 Like elsewhere in TeXmacs, this is a wrapper around its corresponding
 ns_widget_rep which implements reference counting.
 See src/Kernel/Abstractions/basic.hpp
 */
class ns_widget {
public:
  ABSTRACT_NULL(ns_widget); // Automagically declared constructor, methods, etc.
  
  inline bool operator == (ns_widget w) { return rep == w.rep; }
  inline bool operator != (ns_widget w) { return rep != w.rep; }
};

// Automagically create definitions for the stuff declared inside ns_widget with
// the macro ABSTRACT_NULL(). See src/Kernel/Abstractions/basic.hpp
ABSTRACT_NULL_CODE(ns_widget);

// Needed for the ntuples (see ntuple.h)
tm_ostream& operator << (tm_ostream& out, ns_widget w);

/*! Casting form ns_widget to widget */
inline widget abstract (ns_widget w) { return widget (w.rep); }

/*! Casting from widget to ns_widget */
inline ns_widget concrete (widget w) {
  return ns_widget (static_cast<ns_widget_rep*> (w.rep));
}


class ns_view_widget_rep: public ns_widget_rep {
public:	
	NSView *view;

public:
  ns_view_widget_rep (NSView *v);
  ~ns_view_widget_rep ();

  virtual void send (slot s, blackbox val);
    // send a message val to the slot s
  virtual blackbox query (slot s, int type_id);
    // obtain information of a given type from the slot s
  virtual widget read (slot s, blackbox index);
    // abstract read access (of type s) of a subwidget at position index
  virtual void write (slot s, blackbox index, widget w);
    // abstract write access (of type s) of a subwidget at position index
  virtual void notify (slot s, blackbox new_val);
    // notification of a change on a slot s which contains a state variable
//  virtual void connect (slot s, widget w2, slot s2);
    // connect a state slot s to another slot s2 of another widget w2
//  virtual void deconnect (slot s, widget w2, slot s2);
    // deconnect a state slot s from another slot s2 of another widget w2

	virtual widget plain_window_widget (string s); 
};

#endif // defined NS_WIDGET_H
