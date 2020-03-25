
/******************************************************************************
* MODULE     : QTWSGuiHelper.cpp
* DESCRIPTION: QT WebSockets Gui helper class.
*              Infrastructure for delayed menu installation 
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTWSGuiHelper.hpp"
#include "qtws_gui.hpp"
#include "scheme.hpp"
#include "iterator.hpp"
#include <QFileOpenEvent>

void
QTWSGuiHelper::doUpdate () {
BEGIN_SLOT
  // cout << "UPDATE " << texmacs_time () << LF;
  gui->update();
END_SLOT
}

void
QTWSGuiHelper::doRefresh () {
BEGIN_SLOT
  emit refresh();
END_SLOT
}

#if 0
bool
QTWSGuiHelper::eventFilter (QObject *obj, QEvent *event) {
  if (event->type() == QEvent::FileOpen) {
    static bool new_window_flag= false;
    QFileOpenEvent* openEvent = static_cast<QFileOpenEvent *>(event);
    const char* s = openEvent->file().toUtf8().constData();
      //qDebug ("File Open Event %s", s);
    const char *win= new_window_flag? ":new-window": ":current-window";
    call ("load-buffer", object (url_system (s)), eval (win));
    new_window_flag= true;
    return true;
  } else {
      // standard event processing
    return QObject::eventFilter (obj, event);
  }
}
#endif

void 
QTWSGuiHelper::doPopWaitingWidgets() {
BEGIN_SLOT
  if (!is_nil (waiting_widgets)) {
    if (DEBUG_QT)
      debug_qt << "Installing postponed menu" << LF;
    waiting_widgets->item->install_main_menu();
    waiting_widgets = waiting_widgets->next;
  }
END_SLOT
}

void
QTWSGuiHelper::emitTmSlotRefresh (string kind) {
BEGIN_SLOT
  emit tmSlotRefresh (kind);
END_SLOT
}
