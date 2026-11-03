
/******************************************************************************
* MODULE     : qt_test.cpp
* DESCRIPTION: Driving the Qt interface from scripts, for testing purposes
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_utilities.hpp"
#include "qt_gui.hpp"
#include "sys_utils.hpp"
#include "gui_test.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "url.hpp"

#include <QApplication>
#include <QWidget>
#include <QAbstractButton>
#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QKeyEvent>
#include <QInputMethodEvent>
#include <QMainWindow>
#include <QPixmap>
#include <QToolButton>
#include <QToolBar>
#include <QTimer>
#include <QTabBar>
#include <QStatusBar>
#include <QLabel>
#include <QPushButton>
#include <QWheelEvent>
#include <QClipboard>
#include <QGuiApplication>
#include <QPointer>
#include <QKeySequence>
#include <QComboBox>
#include <QLineEdit>
#include <QAbstractItemView>
#include <QTreeView>

/******************************************************************************
* Snapshots of the windows
******************************************************************************/

int
gui_test_snapshot (string dir) {
  // Save the visible top level widgets (windows, dialogs, menus)
  // as dir/window-<i>.png; returns their number
  int n= 0;
  QApplication::processEvents ();
  foreach (QWidget* w, QApplication::topLevelWidgets())
    if (w->isVisible ()) {
      w->repaint ();
      QString name= to_qstring (dir * "/window-" * as_string (n++) * ".png");
      w->grab ().save (name);
    }
  return n;
}

/******************************************************************************
* Buttons
******************************************************************************/

static string
normalize (QString s) {
  // Labels as written in the menus and widgets of TeXmacs
  s.remove ('&');
  // (the menus View and Help are written with a Cyrillic e on macOS, so
  // that the system does not add its items to them)
  return replace (replace (from_qstring (s.trimmed ()), "<ldots>", "..."),
                  "<#435>", "e");
}

static string
button_label (QAbstractButton* b) {
  QString s= b->text ();
  if (s.isEmpty ()) s= b->toolTip ();
  if (s.isEmpty ()) s= b->statusTip ();
  return normalize (s);
}

array<string>
gui_test_buttons () {
  // The labels of the visible and enabled buttons
  array<string> r;
  foreach (QWidget* top, QApplication::topLevelWidgets())
    if (top->isVisible ())
      foreach (QAbstractButton* b, top->findChildren<QAbstractButton*> ())
        if (b->isVisible () && b->isEnabled ()) {
          string s= button_label (b);
          if (N(s) > 0) r << s;
        }
  return r;
}

static QList<QWidget*>
visible_windows () {
  // The visible top level widgets, starting with the active one (dialogs)
  QList<QWidget*> r;
  QWidget* active= QApplication::activeModalWidget ();
  if (active == NULL) active= QApplication::activeWindow ();
  if (active != NULL && active->isVisible ()) r << active;
  foreach (QWidget* top, QApplication::topLevelWidgets())
    if (top->isVisible () && top != active) r << top;
  return r;
}

bool
gui_test_click (string label) {
  // Click the first visible and enabled button (or tab) with the given
  // label, looking first in the active window
  foreach (QWidget* top, visible_windows ())
    foreach (QAbstractButton* b, top->findChildren<QAbstractButton*> ())
      if (b->isVisible () && b->isEnabled () &&
          locase_all (button_label (b)) == locase_all (label)) {
        b->click ();
        return true;
      }
  foreach (QWidget* top, visible_windows ())
    foreach (QTabBar* t, top->findChildren<QTabBar*> ())
      if (t->isVisible ())
        for (int i=0; i<t->count (); i++)
          if (locase_all (normalize (t->tabText (i))) ==
              locase_all (label)) {
            t->setCurrentIndex (i);
            return true;
          }
  return false;
}

/******************************************************************************
* Menus
******************************************************************************/

static string
action_label (QAction* a) {
  QString s= a->text ();
  int tab= s.indexOf ('\t');
  if (tab >= 0) s= s.left (tab);
  return normalize (s);
}

static QAction*
find_action (QList<QAction*> l, string label) {
  foreach (QAction* a, l)
    if (action_label (a) == label) return a;
  foreach (QAction* a, l)
    if (starts (action_label (a), label)) return a;
  return NULL;
}

static QMenuBar*
find_menu_bar () {
  // NOTE: on macOS, the native menu bar is not attached to the window;
  // prefer the one of the active window, otherwise take the last one
  QMenuBar* found= NULL;
  foreach (QWidget* top, QApplication::topLevelWidgets()) {
    QMainWindow* mw= qobject_cast<QMainWindow*> (top);
    if (mw != NULL && mw->isVisible () && mw->menuBar () != NULL &&
        mw->menuBar ()->actions ().size () > 0)
      found= mw->menuBar ();
  }
  if (found != NULL) return found;
  foreach (QWidget* w, QApplication::allWidgets()) {
    QMenuBar* mb= qobject_cast<QMenuBar*> (w);
    if (mb != NULL && mb->actions ().size () > 0) found= mb;
  }
  return found;
}

static QList<QAction*>
top_menu_actions () {
  // The entries of the main menu bar: a native menu bar, or (by default)
  // a tool bar with menus inside the window
  // NOTE: QTMToolbar creates the tool buttons itself, so that the
  // actions are those of the buttons
  QList<QAction*> r;
  foreach (QWidget* top, QApplication::topLevelWidgets())
    if (top->isVisible ())
      foreach (QToolBar* tb, top->findChildren<QToolBar*> ())
        if (tb->isVisible ())
          foreach (QToolButton* b, tb->findChildren<QToolButton*> ())
            if (b->isVisible () && b->defaultAction () != NULL &&
                b->defaultAction ()->menu () != NULL)
              r << b->defaultAction ();
  if (r.size () > 0) return r;
  QMenuBar* bar= find_menu_bar ();
  if (bar != NULL) return bar->actions ();
  return r;
}

static void
populate (QMenu* m) {
  // NOTE: the menus of TeXmacs are built lazily, when shown; the menu is
  // also "hidden" again, since TeXmacs postpones the updates of the main
  // menu while one of its menus is shown (menu_count); the postponed
  // update is only installed later, from a timer
  QMetaObject::invokeMethod (m, "force");
  QMetaObject::invokeMethod (m, "aboutToShow");
  QMetaObject::invokeMethod (m, "aboutToHide");
}

bool
gui_test_menu (string path) {
  // Trigger the menu entry with the path "Menu|Submenu|Entry"; the labels
  // are compared without shortcuts, and may be prefixes of the real labels
  array<string> l= tokenize (path, "|");
  QList<QAction*> actions= top_menu_actions ();
  for (int i=0; i<N(l); i++) {
    QAction* a= find_action (actions, l[i]);
    if (a == NULL) return false;
    if (i == N(l) - 1) {
      a->trigger ();
      return true;
    }
    QMenu* m= a->menu ();
    if (m == NULL) return false;
    populate (m);
    actions= m->actions ();
  }
  return false;
}

array<string>
gui_test_menu_entries (string path) {
  // The labels of the entries of the menu with the given path
  array<string> r;
  array<string> l= tokenize (path, "|");
  QList<QAction*> actions= top_menu_actions ();
  for (int i=0; i<N(l); i++) {
    QAction* a= find_action (actions, l[i]);
    if (a == NULL || a->menu () == NULL) return r;
    populate (a->menu ());
    actions= a->menu ()->actions ();
  }
  foreach (QAction* a, actions)
    if (a->isVisible ())
      r << (a->isSeparator () ? string ("---") :
            action_label (a) * (a->isEnabled () ? string ("") : string (" (disabled)")));
  return r;
}

/******************************************************************************
* Typing text
******************************************************************************/

void
gui_test_type (string text) {
  // Send the characters of text (in utf8) to the widget with the focus
  QWidget* w= QApplication::focusWidget ();
  if (w == NULL) return;
  c_string _text (text);
  QString s= QString::fromUtf8 ((char*) _text);
  for (int i=0; i<s.size (); i++) {
    QKeyEvent press (QEvent::KeyPress, 0, Qt::NoModifier, QString (s[i]));
    QKeyEvent release (QEvent::KeyRelease, 0, Qt::NoModifier, QString (s[i]));
    QApplication::sendEvent (w, &press);
    QApplication::sendEvent (w, &release);
  }
}

/******************************************************************************
* Answering modal dialogs
******************************************************************************/

static void
click_when_present (string dir, string label, int tries) {
  // Save the windows in dir (unless empty), then click the button with the
  // given label, retrying for a while if it does not exist yet
  if (N(dir) > 0 && N(label) == 0) { gui_test_snapshot (dir); return; }
  bool found= false;
  foreach (QWidget* top, QApplication::topLevelWidgets())
    if (top->isVisible ())
      foreach (QAbstractButton* b, top->findChildren<QAbstractButton*> ())
        if (b->isVisible () && b->isEnabled () &&
            locase_all (button_label (b)) == locase_all (label))
          found= true;
  if (found) {
    if (N(dir) > 0) gui_test_snapshot (dir);
    gui_test_click (label);
  }
  else if (tries > 0)
    QTimer::singleShot (250, [dir, label, tries] () {
        click_when_present (dir, label, tries - 1); });
}

void
gui_test_click_later (int ms, string dir, string label) {
  // After ms milliseconds, save the windows in dir (unless empty) and click
  // the button with the given label (unless empty), as soon as it exists
  // (for at most 10 seconds).  Qt timers also fire inside the event loops
  // of modal dialogs, unlike the delayed commands of TeXmacs, so this
  // allows to answer them.
  QTimer::singleShot (ms, [dir, label] () {
      click_when_present (dir, label, 40); });
}

/******************************************************************************
* The hooks of the scripted tests (gui_test.hpp, docs/gui-scripting.md)
*
* The driver (check gui-driver) runs its steps from a timer (gui_test_pump),
* which Qt also fires in the loops of the menus and of the modal dialogs.
* The input is posted, not sent: a press which opens a menu runs the loop
* of the menu, and a sent event would only come back once the menu closed.
* Positions are in points from the top left of the window of the script;
* a press goes to the topmost widget at its place on the screen, which may
* be a menu (a window of its own in Qt).
******************************************************************************/

static QPointer<QWidget> script_win;     // the window of the script
static bool script_no_target= false;     // the last "window" matched nothing
static Qt::MouseButtons script_buttons= Qt::NoButton;
static QPoint script_pointer;            // the last position, global

string gui_test_port () { return "qt"; }

static bool
is_script_window (QWidget* w) {
  // the windows a script may choose: not the menus, balloons and such
  return w != NULL && w->isVisible () && w->isWindow () &&
         qobject_cast<QMenu*> (w) == NULL &&
         w->windowType () != Qt::ToolTip && w->windowType () != Qt::Popup;
}

static QWidget*
main_window () {
  // the editor window (a QMainWindow), the active one when there are several
  QWidget* active= QApplication::activeWindow ();
  if (qobject_cast<QMainWindow*> (active) != NULL) return active;
  foreach (QWidget* top, QApplication::topLevelWidgets ())
    if (qobject_cast<QMainWindow*> (top) != NULL && top->isVisible ())
      return top;
  return NULL;
}

static QWidget*
script_target () {
  if (script_no_target) return NULL;
  if (!script_win.isNull () && script_win->isVisible ()) return script_win;
  QWidget* w= QApplication::activeModalWidget ();
  if (w == NULL) w= QApplication::activeWindow ();
  if (!is_script_window (w)) w= main_window ();
  return w;
}

static QPoint
script_origin (QWidget* win) {
  return win->mapToGlobal (QPoint (0, 0));
}

/******************************************************************************
* Where the texts and icons are: the path of the bars and menus
******************************************************************************/

static string
icon_name (QObject* o) {
  // the name of the file of the icon of a widget or an action (qt_ui_element
  // sets it, see "tmIcon"), as "icon:<name>"
  if (o == NULL) return "";
  QVariant v= o->property ("tmIcon");
  if (!v.isValid ()) return "";
  return "icon:" * from_qstring (v.toString ());
}

static string
bar_name (QWidget* w) {
  // the name of a bar of the editor, for the scripts
  QToolBar* tb= qobject_cast<QToolBar*> (w);
  if (tb != NULL) {
    string t= from_qstring (tb->windowTitle ());
    if (t == "menu toolbar") return "menubar";
    if (t == "main toolbar") return "mainbar";
    if (t == "mode toolbar") return "modebar";
    if (t == "focus toolbar") return "focusbar";
    if (t == "user toolbar") return "userbar";
    return "";
  }
  if (qobject_cast<QStatusBar*> (w) != NULL) return "footer";
  if (qobject_cast<QMenuBar*> (w) != NULL) return "menubar";
  return "";
}

static string
named_of (QObject* o) {
  // the names of the (named ...) of the menus around a widget or an action:
  // the divisions put them in its property tmNamed (qt_ui_element.cpp), the
  // outer first, as "#outer > #inner"
  if (o == NULL) return "";
  QVariant v= o->property ("tmNamed");
  if (!v.isValid ()) return "";
  array<string> l= tokenize (from_qstring (v.toString ()), "\t");
  string r;
  for (int i= 0; i < N(l); i++)
    if (starts (l[i], "named:"))
      r= (N(r) == 0 ? r : r * " > ") * "#" * l[i] (6, N(l[i]));
  return r;
}

static string join_path (string a, string b) {
  if (N(a) == 0) return b;
  if (N(b) == 0) return a;
  return a * " > " * b;
}

static string menu_path (QMenu* m, int depth);

static string
widget_path (QWidget* w, int depth) {
  // the bars, menus and names a widget is in (its own name included)
  string r= named_of (w);
  for (QWidget* p= w->parentWidget (); p != NULL; p= p->parentWidget ()) {
    QMenu* m= qobject_cast<QMenu*> (p);
    if (m != NULL) return join_path (menu_path (m, depth), r);
    r= join_path (join_path (bar_name (p), named_of (p)), r);
    if (p->isWindow ()) break;
  }
  return r;
}

static string
menu_path (QMenu* m, int depth) {
  // the path of the items of a menu: that of what opened it, and its label
  if (depth > 12) return "";
  foreach (QWidget* w, QApplication::allWidgets ()) {
    if (!w->isVisible ()) continue;
    QToolButton* tb= qobject_cast<QToolButton*> (w);
    QPushButton* pb= qobject_cast<QPushButton*> (w);
    QMenu* om= qobject_cast<QMenu*> (w);
    if ((tb != NULL && (tb->menu () == m || (tb->defaultAction () != NULL &&
                                             tb->defaultAction ()->menu () == m))) ||
        (pb != NULL && pb->menu () == m)) {
      QAbstractButton* b= (QAbstractButton*) w;
      string label= button_label (b);
      if (N(label) == 0) label= icon_name (b);
      if (N(label) == 0 && tb != NULL) label= icon_name (tb->defaultAction ());
      return join_path (widget_path (w, depth + 1), label);
    }
    QMenuBar* mb= qobject_cast<QMenuBar*> (w);
    if (mb != NULL)
      foreach (QAction* a, mb->actions ())
        if (a->menu () == m) return join_path ("menubar", action_label (a));
    if (om != NULL && om != m)
      foreach (QAction* a, om->actions ())
        if (a->menu () == m) {
          string label= action_label (a);
          if (N(label) == 0) label= icon_name (a);
          return join_path (join_path (menu_path (om, depth + 1),
                                       named_of (a)), label);
        }
  }
  return "";
}

static void
add_text (array<string>& r, QPoint origin, QRect global, string path,
          string text) {
  if (N(text) == 0) return;
  QPoint p= global.topLeft () - origin;
  r << (as_string (p.x ()) * "\t" * as_string (p.y ()) * "\t" *
        as_string (global.width ()) * "\t" * as_string (global.height ()) *
        "\t" * path * "\t" * text);
}

static QRect
global_rect (QWidget* w) {
  return QRect (w->mapToGlobal (QPoint (0, 0)), w->size ());
}

static void
collect_rows (QAbstractItemView* v, QModelIndex parent, QPoint origin,
              string path, array<string>& r, int& n) {
  // the rows of a list or a tree which are in view (the children of the
  // expanded ones of a tree)
  QAbstractItemModel* m= v->model ();
  if (m == NULL) return;
  QTreeView* tv= qobject_cast<QTreeView*> (v);
  QRect view= v->viewport ()->rect ();
  for (int i= 0; i < m->rowCount (parent) && n < 1000; i++) {
    QModelIndex idx= m->index (i, 0, parent);
    QRect rect= v->visualRect (idx);
    if (!rect.isEmpty () && rect.intersects (view)) {
      n++;
      add_text (r, origin,
                QRect (v->viewport ()->mapToGlobal (rect.topLeft ()), rect.size ()),
                path, normalize (m->data (idx).toString ()));
    }
    if (tv != NULL && tv->isExpanded (idx))
      collect_rows (v, idx, origin, path, r, n);
  }
}

static void
collect_texts (QWidget* w, QPoint origin, array<string>& r) {
  if (!w->isVisible ()) return;
  QComboBox* cb= qobject_cast<QComboBox*> (w);
  QAbstractItemView* iv= qobject_cast<QAbstractItemView*> (w);
  if (cb != NULL)
    add_text (r, origin, global_rect (w), widget_path (w, 0),
              normalize (cb->currentText ()));
  if (iv != NULL) {
    int n= 0;
    collect_rows (iv, QModelIndex (), origin, widget_path (w, 0), r, n);
    return;
  }
  QAbstractButton* b= qobject_cast<QAbstractButton*> (w);
  QLabel* l= qobject_cast<QLabel*> (w);
  QTabBar* t= qobject_cast<QTabBar*> (w);
  if (b != NULL) {
    string label= normalize (b->text ());
    if (N(label) == 0) label= icon_name (b);
    QToolButton* tb= qobject_cast<QToolButton*> (w);
    if (N(label) == 0 && tb != NULL) label= icon_name (tb->defaultAction ());
    if (N(label) == 0) label= button_label (b);
    add_text (r, origin, global_rect (w), widget_path (w, 0), label);
  }
  else if (l != NULL) {
    string label= normalize (l->text ());
    if (N(label) == 0) label= icon_name (l);
    add_text (r, origin, global_rect (w), widget_path (w, 0), label);
  }
  else if (t != NULL)
    for (int i=0; i<t->count (); i++)
      add_text (r, origin,
                QRect (t->mapToGlobal (t->tabRect (i).topLeft ()),
                       t->tabRect (i).size ()),
                widget_path (w, 0), normalize (t->tabText (i)));
  foreach (QObject* c, w->children ()) {
    QWidget* cw= qobject_cast<QWidget*> (c);
    if (cw != NULL && !cw->isWindow ()) collect_texts (cw, origin, r);
  }
}

array<string>
gui_test_windows () {
  array<string> r;
  foreach (QWidget* top, QApplication::topLevelWidgets ())
    if (is_script_window (top)) r << from_qstring (top->windowTitle ());
  return r;
}

array<string>
gui_test_texts () {
  // the texts and icons of the window of the script, then those of the
  // open menus, the deepest last (the topmost)
  array<string> r;
  QWidget* win= script_target ();
  if (win == NULL) return r;
  QPoint origin= script_origin (win);
  collect_texts (win, origin, r);
  // the popups which are not menus (the list of a combo box)
  foreach (QWidget* top, QApplication::topLevelWidgets ())
    if (top->isVisible () && top->windowType () == Qt::Popup &&
        qobject_cast<QMenu*> (top) == NULL)
      collect_texts (top, origin, r);
  // the menus of the same depth in any order, the deeper ones after
  array<array<string> > by_depth;
  foreach (QWidget* top, QApplication::topLevelWidgets ()) {
    QMenu* m= qobject_cast<QMenu*> (top);
    if (m == NULL || !m->isVisible ()) continue;
    string path= menu_path (m, 0);
    int depth= min (N(tokenize (path, " > ")), 15);
    while (N(by_depth) <= depth) by_depth << array<string> ();
    foreach (QAction* a, m->actions ())
      if (a->isVisible () && !a->isSeparator ()) {
        string label= action_label (a);
        if (N(label) == 0) label= icon_name (a);
        QRect g= m->actionGeometry (a);
        add_text (by_depth[depth], origin,
                  QRect (m->mapToGlobal (g.topLeft ()), g.size ()),
                  join_path (path, named_of (a)), label);
      }
  }
  for (int i= 0; i < N(by_depth); i++) r << by_depth[i];
  return r;
}

/******************************************************************************
* The commands of the scripts
******************************************************************************/

static Qt::MouseButton
script_button (array<string> a, int i) {
  if (N(a) > i && a[i] == "right") return Qt::RightButton;
  if (N(a) > i && a[i] == "middle") return Qt::MiddleButton;
  return Qt::LeftButton;
}

// the widget which gets the pointer at the global position g: the topmost
// one, but not of a window which takes no input (the balloons of TeXmacs),
// as the window system does: the widget of the window of the script there
static QWidget*
pointer_widget (QPoint g) {
  QWidget* w= QApplication::widgetAt (g);
  if (w != NULL &&
      (w->window ()->windowFlags () & Qt::WindowTransparentForInput)) {
    QWidget* win= script_target ();
    w= (win == NULL? NULL: win->childAt (win->mapFromGlobal (g)));
    if (w == NULL) w= win;
  }
  return w;
}

static void
post_mouse (QEvent::Type type, QPoint g, Qt::MouseButton button) {
  // a mouse event at the global position g, to the topmost widget there
  QWidget* w= pointer_widget (g);
  if (w == NULL) w= script_target ();
  if (w == NULL) return;
  QPoint p= w->mapFromGlobal (g);
  script_pointer= g;
  if (type == QEvent::MouseButtonPress || type == QEvent::MouseButtonDblClick)
    script_buttons |= button;
  QMouseEvent* ev= new QMouseEvent (type, QPointF (p), QPointF (g), button,
                                    script_buttons, Qt::NoModifier);
  if (type == QEvent::MouseButtonRelease) script_buttons &= ~button;
  QApplication::postEvent (w, ev);
}

static QPoint
script_point (QWidget* win, array<string> a) {
  return script_origin (win) +
         QPoint ((int) as_double (a[1]), (int) as_double (a[2]));
}

static QWidget*
key_target (QWidget* win) {
  QWidget* f= QApplication::focusWidget ();
  if (f != NULL && f->window () == win->window ()) return f;
  if (win->focusWidget () != NULL) return win->focusWidget ();
  // no focus yet (a dialog gets it a moment after it shows): its first
  // input field, as the dialog will give it
  QLineEdit* le= win->findChild<QLineEdit*> ();
  if (le != NULL && le->isVisible ()) {
    le->setFocus ();
    return le;
  }
  return win;
}

static void
post_key (QWidget* win, string name, string text) {
  // "[S-][C-][A-][M-]<name>": shift, control, option, command (as Vue);
  // on macOS Qt calls the command key Control and the control key Meta
  Qt::KeyboardModifiers mods= Qt::NoModifier;
#ifdef Q_OS_MACOS
  const Qt::KeyboardModifier ctrl= Qt::MetaModifier, cmd= Qt::ControlModifier;
#else
  const Qt::KeyboardModifier ctrl= Qt::ControlModifier, cmd= Qt::MetaModifier;
#endif
  while (N(name) > 2 && name[1] == '-') {
    if (name[0] == 'S') mods |= Qt::ShiftModifier;
    else if (name[0] == 'C') mods |= ctrl;
    else if (name[0] == 'A') mods |= Qt::AltModifier;
    else if (name[0] == 'M') mods |= cmd;
    else break;
    name= name (2, N(name));
  }
  // the names of SDL, which the scripts of Vue use, where Qt has others
  if (name == "Escape") name= "Esc";
  else if (name == "Delete") name= "Del";
  else if (name == "PageUp") name= "PgUp";
  else if (name == "PageDown") name= "PgDown";
  else if (name == "Insert") name= "Ins";
  else if (starts (name, "Keypad_") && N(name) == 8) {
    // a key of the keypad: Keypad_1, Keypad_Plus is not known here
    mods |= Qt::KeypadModifier;
    name= name (7, 8);
  }
  QKeySequence seq (to_qstring (name));
#if QT_VERSION >= 0x060000
  int key= seq.count () == 0? 0: (int) seq[0].key ();
#else
  int key= seq.count () == 0? 0: (int) (seq[0] & ~Qt::KeyboardModifierMask);
#endif
  if (key == 0 || key == Qt::Key_unknown) {
    cout << "qt script: unknown key " << name << LF;
    return;
  }
  QString t;
  if (N(text) > 0) t= to_qstring (text);
  else if (N(name) == 1 &&
           (mods & ~(Qt::ShiftModifier | Qt::KeypadModifier)) == 0)
    t= (mods & Qt::ShiftModifier) ? to_qstring (name).toUpper ()
                                  : to_qstring (name).toLower ();
  else if (name == "Return") t= "\r";
  else if (name == "Tab") t= "\t";
  else if (name == "Space") t= " ";
  QWidget* w= key_target (win);
  QApplication::postEvent (w, new QKeyEvent (QEvent::KeyPress, key, mods, t));
  QApplication::postEvent (w, new QKeyEvent (QEvent::KeyRelease, key, mods, t));
}

static void
post_text (QWidget* win, string text) {
  QWidget* w= key_target (win);
  c_string _text (text);
  QString s= QString::fromUtf8 ((char*) _text);
  for (int i=0; i<s.size (); i++) {
    QString c (s[i]);
    QApplication::postEvent (w, new QKeyEvent (QEvent::KeyPress, 0,
                                               Qt::NoModifier, c));
    QApplication::postEvent (w, new QKeyEvent (QEvent::KeyRelease, 0,
                                               Qt::NoModifier, c));
  }
}

static string
snapshot_dir () {
  string d= get_env ("TEXMACS_TEST_SNAPSHOT");
  if (N(d) == 0) d= get_env ("TEXMACS_VUE_SNAPSHOT");
  return d;
}

int
gui_test_raw (string line) {
  line= trim_spaces (line);
  if (N(line) == 0) return 1;
  array<string> a= tokenize (line, " ");
  string cmd= a[0];
  string rest= N(line) > N(cmd) ? line (N(cmd) + 1, N(line)) : string ("");
  if (cmd == "window" && N(a) > 1) {
    // "window main" (the editor), or a part of the title of a window
    script_win= NULL;
    if (rest == "main") script_win= main_window ();
    else
      foreach (QWidget* top, QApplication::topLevelWidgets ())
        if (is_script_window (top) &&
            occurs (rest, from_qstring (top->windowTitle ())))
          script_win= top;
    script_no_target= script_win.isNull ();
    if (script_no_target) {
      cout << "qt script: no window matches " << rest << "; there are";
      foreach (QWidget* top, QApplication::topLevelWidgets ())
        if (top->isVisible ())
          cout << " [" << from_qstring (top->windowTitle ()) << "]"
               << (is_script_window (top) ? "" : "(not a window)");
      cout << LF;
    }
    else {
      QRect g= script_win->geometry ();
      cout << "qt script: window at " << g.x () << "," << g.y ()
           << " size " << g.width () << "x" << g.height () << LF;
    }
    return 1;
  }
  if (cmd == "clipboard") {
    // the clipboard of the system: offscreen, that of the process
    c_string _t (rest);
    QGuiApplication::clipboard ()->setText (QString::fromUtf8 ((char*) _t));
    return 1;
  }
  QWidget* win= script_target ();
  if (win == NULL) {
    // after a "window" which matched nothing: skipped, until one matches
    cout << "qt script: no window, skipped: " << line << LF;
    return 1;
  }
  if (cmd == "move" && N(a) > 2)
    post_mouse (QEvent::MouseMove, script_point (win, a), Qt::NoButton);
  else if (cmd == "press" && N(a) > 2)
    post_mouse (QEvent::MouseButtonPress, script_point (win, a),
                script_button (a, 3));
  else if (cmd == "release" && N(a) > 2)
    post_mouse (QEvent::MouseButtonRelease, script_point (win, a),
                script_button (a, 3));
  else if (cmd == "click" && N(a) > 2) {
    QPoint g= script_point (win, a);
    Qt::MouseButton b= script_button (a, 3);
    QMenu* m= qobject_cast<QMenu*> (QApplication::widgetAt (g));
    QAction* act= (m == NULL) ? NULL : m->actionAt (m->mapFromGlobal (g));
    if (act != NULL && b == Qt::LeftButton) {
      // an item of a menu: a menu triggers an item at the release of the
      // button only when the pointer moved onto it before, as the window
      // system tells it, which posted events do not: it is chosen as with
      // the keyboard instead (it opens its submenu, or it runs and the
      // menus close)
      m->setActiveAction (act);
      QApplication::postEvent (m, new QKeyEvent (QEvent::KeyPress,
                                                 Qt::Key_Return, Qt::NoModifier));
      QApplication::postEvent (m, new QKeyEvent (QEvent::KeyRelease,
                                                 Qt::Key_Return, Qt::NoModifier));
      return 0;
    }
    int clicks= (N(a) > 4 && is_int (a[4])) ? max (1, as_int (a[4])) : 1;
    post_mouse (QEvent::MouseMove, g, Qt::NoButton);
    post_mouse (clicks >= 2 ? QEvent::MouseButtonDblClick
                            : QEvent::MouseButtonPress, g, b);
    post_mouse (QEvent::MouseButtonRelease, g, b);
  }
  else if (cmd == "wheel" && N(a) > 4) {
    QPoint g= script_point (win, a);
    QWidget* w= pointer_widget (g);
    if (w == NULL) w= win;
    QPoint angle ((int) (120 * as_double (a[3])), (int) (120 * as_double (a[4])));
    QApplication::postEvent (w, new QWheelEvent (QPointF (w->mapFromGlobal (g)),
                                                QPointF (g), QPoint (), angle,
                                                script_buttons, Qt::NoModifier,
                                                Qt::NoScrollPhase, false));
  }
  else if (cmd == "key" && N(a) > 1)
    post_key (win, a[1], N(a) > 2 ? a[2] : string (""));
  else if (cmd == "text" && N(a) > 1)
    post_text (win, rest);
  else if (cmd == "commit" && N(a) > 1) {
    // an input method commits a text (as Vue's script command does)
    QWidget* w= key_target (win);
    c_string _text (rest);
    QInputMethodEvent ev;
    ev.setCommitString (QString::fromUtf8 ((char*) _text));
    QApplication::sendEvent (w, &ev);
  }
  else if (cmd == "focus")
    // the keys of a script go to the focus widget of its window, which
    // needs not be active; an activation by the script would not last
    // either (TeXmacs is not the active application of a test)
    win->raise ();
  else if (cmd == "snapshot" && N(a) > 1) {
    string dir= snapshot_dir ();
    // (grab paints the window itself: no events are handled within a step)
    if (N(dir) > 0) win->grab ().save (to_qstring (dir * "/" * a[1] * ".png"));
  }
  else if (cmd == "resize" && N(a) > 2)
    win->resize (as_int (a[1]), as_int (a[2]));
  else if (cmd == "repaint")
    win->update ();
  else if (cmd == "close")
    win->close ();
  else {
    cout << "qt script: unknown command " << line << LF;
    return 2;
  }
  return 0;
}

bool
gui_test_busy () {
  // the events of TeXmacs, its delayed commands which are due, an update
  // (the events posted by the script are handled before the timer of the
  // pump fires again)
  // while a dialog is modal, the delayed commands and the updates wait for
  // it to close, which the script does: they are not counted
  if (QApplication::activeModalWidget () != NULL) return false;
  return the_gui->test_busy ();
}

void
gui_test_pump (bool on) {
  static QTimer* timer= NULL;
  static bool running= false; // not again while a step runs (a nested loop)
  if (timer == NULL) {
    timer= new QTimer ();
    timer->setInterval (10);
    QObject::connect (timer, &QTimer::timeout, [] () {
        if (running) return;
        running= true;
        call ("gui-test-pump");
        running= false; });
  }
  if (on) timer->start ();
  else {
    timer->stop ();
    script_buttons= Qt::NoButton;
  }
}
