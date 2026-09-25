
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
#include "sys_utils.hpp"
#include "analyze.hpp"

#include <QApplication>
#include <QWidget>
#include <QAbstractButton>
#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QKeyEvent>
#include <QMainWindow>
#include <QPixmap>
#include <QToolButton>
#include <QToolBar>
#include <QTimer>
#include <QTabBar>

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
  return replace (from_qstring (s.trimmed ()), "<ldots>", "...");
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
  QString s= QString::fromUtf8 (as_charp (text));
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
