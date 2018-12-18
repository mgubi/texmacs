
/******************************************************************************
 * MODULE     : ns_ui_element.h
 * DESCRIPTION: User interface proxies
 * COPYRIGHT  : (C) 2018  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef NS_UI_ELEMENT_H
#define NS_UI_ELEMENT_H

#include "ntuple.hpp"
#include "promise.hpp"
#include "url.hpp"
#include "scheme.hpp"
#include "hashmap.hpp"

#include "ns_widget.h"

class ns_ui_element_rep: public ns_widget_rep {
  
  // NOTE: automatic deletion of the blackbox upon destruction will trigger
  // deletion of all the nested widgets within.
  blackbox                  load;
  //QPointer<QAction> cachedAction;
  //QList<QAction*>* cachedActionList;
  
public:
  ns_ui_element_rep (types _type, blackbox _load);
  virtual ~ns_ui_element_rep();
  
  virtual widget make_popup_widget ();
  
  
  //virtual QAction*         as_qaction ();
  //virtual QWidget*         as_qwidget ();
  //virtual QLayoutItem*     as_qlayoutitem ();
  //virtual QList<QAction*>* get_qactionlist();
  
  operator tree ();
  
  template<class X1> static ns_widget create (types _type, X1 x1) {
    return tm_new <ns_ui_element_rep> (_type, close_box<X1>(x1));
  }
  
  template <class X1, class X2>
  static ns_widget create (types _type, X1 x1, X2 x2) {
    typedef pair<X1,X2> T;
    return tm_new <ns_ui_element_rep> (_type, close_box<T> (T (x1,x2)));
  }
  
  template <class X1, class X2, class X3>
  static ns_widget create (types _type, X1 x1, X2 x2, X3 x3) {
    typedef triple<X1,X2,X3> T;
    return tm_new <ns_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3)));
  }
  
  template <class X1, class X2, class X3, class X4>
  static ns_widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4) {
    typedef quartet<X1,X2,X3,X4> T;
    return tm_new <ns_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3,x4)));
  }
  
  template <class X1, class X2, class X3, class X4, class X5>
  static ns_widget create (types _type, X1 x1, X2 x2, X3 x3, X4 x4, X5 x5) {
    typedef quintuple<X1,X2,X3,X4,X5> T;
    return tm_new <ns_ui_element_rep> (_type, close_box<T> (T (x1,x2,x3,x4,x5)));
  }
  
protected:
  static blackbox get_payload (ns_widget qtw, types check_type = none);
};


/*! A rectangular separator widget with a colored background. */
class ns_glue_widget_rep: public ns_widget_rep {
public:
  tree col;
  bool hx, vx;
  SI w,h;
  
  ns_glue_widget_rep (tree _col, bool _hx, bool _vx, SI _w, SI _h)
  : col (_col), hx (_hx), vx (_vx), w (_w), h (_h) { }
  
  ns_glue_widget_rep () { }
  
  //QPixmap render ();
  
  //virtual QAction* as_qaction ();
  //virtual QWidget* as_qwidget ();
};

/*! A wrapper widget executing a quit command upon SLOT_DESTROY. */
class ns_wrapped_widget_rep : public ns_widget_rep {
  widget tmwid;
  command quit;
  
public:
  ns_wrapped_widget_rep (widget _tmwid, command _quit)
  : tmwid (_tmwid), quit (_quit) {
    add_child (tmwid);
  }
  
  //QWidget* as_qwidget () { return concrete(tmwid)->as_qwidget(); }
  
  void send (slot s, blackbox val) {
    switch (s) {
      case SLOT_DESTROY:
        if (! is_nil (quit)) quit ();
        quit = command();
        break;
      default:
        return;
    }
    ns_widget_rep::send (s, val);
    if (DEBUG_QT_WIDGETS)
      cout << "ns_wrapped_widget_rep: sent " << slot_name (s)
      << "\t\tto widget\t" << type_as_string() << LF;
  }
  
};

#if 0
/*! Ad-hoc command to be used with choice widgets.
 
 The command associated with a ns_ui_element::choice_widget has one parameter
 (a list of selected items).
 For the reason to be of this class, see \sa ns_toggle_command_rep.
 \sa ns_ui_element, , ns_ui_element_rep::as_qwidget, ns_ui_element_rep::choice_widget
 */
class ns_choice_command_rep : public command_rep {
  //QPointer<QTMListView> qwid;
  command                cmd;
  bool              multiple;  //<! Are multiple choices allowed in the widget?
  bool              filtered;
  
public:
  ns_choice_command_rep (QTMListView* w, command c, bool m, bool f=false)
  : qwid(w), cmd(c), multiple(m), filtered(f) {}
  
  virtual void apply () {
    if (qwid) {
      QStringList selected;
      foreach (QModelIndex item, qwid->selectionModel()->selectedIndexes())
      selected << qwid->model()->data(item).toString();
      
      object l= null_object ();
      if (multiple)
        for (int i = selected.size() - 1; i >= 0; --i)
          l = cons (from_qstring (selected[i]), l);
      else if (selected.size() > 0)
        l = from_qstring (selected[0]);
      else
        l = "";
      
      if (filtered)
        cmd (list_object (l, from_qstring (qwid->filter()->filterRegExp().pattern())));
      else
        cmd (list_object (l));
    }
  }
  
  tm_ostream& print (tm_ostream& out) { return out << "Choice"; }
};
#endif

#endif // defined NS_UI_ELEMENT_H
