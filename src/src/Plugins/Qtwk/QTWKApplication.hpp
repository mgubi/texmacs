
/******************************************************************************
 * MODULE     : QTWKApplication.hpp
 * DESCRIPTION:
 * COPYRIGHT  :
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTWKAPPLICATION_HPP
#define QTWKAPPLICATION_HPP

#include <QApplication>
#include "string.hpp"
#include "sys_utils.hpp"
#include "url.hpp"
#include "boot.hpp"
#include "gui.hpp"

#if defined(Q_OS_MAC) && QT_VERSION < 0x060000
#include "QTMMacPasteboardMimePDF.hpp"
#endif

class QTWKApplication: public QApplication {
  Q_OBJECT

#if defined(Q_OS_MAC) && QT_VERSION < 0x060000 
  QMacPasteboardMimePDF mac_pasteboard_mime_pdf;
#endif
  
public:
  QTWKApplication (int& argc, char** argv);
  void set_window_icon (string icon_path);
  void load() {};
  virtual bool notify (QObject* receiver, QEvent* event);
};

inline QTWKApplication *tmapp() {
  ASSERT (!headless_mode, "invalid call of tmapp() in headless mode");
  return dynamic_cast<QTWKApplication *>(qApp);
}

class QTWKCoreApplication: public QCoreApplication {
  Q_OBJECT
  
public:
  QTWKCoreApplication (int& argc, char** argv) :
    QCoreApplication (argc, argv) {}

  void set_window_icon (string icon_path) {
    (void) icon_path;
  }

  virtual bool notify (QObject* receiver, QEvent* event)
  {
    try {
      return QCoreApplication::notify (receiver, event);
    }
    catch (string s) {
      qt_error << "Thrown " << s << LF;
      the_exception= s;
    }
    return false;
  }
};

#endif   // QTWKAPPLICATION_HPP
