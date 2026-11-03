/******************************************************************************
* MODULE     : qt_http_test.cpp
* DESCRIPTION: the requests of the request links with Qt 6, to a local mock
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"

// (a test of the Qt 6 builds only: the others have no Qt)
#if defined(QTTEXMACS) && AC_QT_MAJOR_VERSION >= 6

#include "tree.hpp"
#include "analyze.hpp"
#include "web_files.hpp"

#include <QCoreApplication>
#include <QElapsedTimer>
#include <QPointer>
#include <QThread>
#include <QTcpServer>
#include <QTcpSocket>
#include <QTimer>
#include <QJsonDocument>
#include <QJsonObject>
#include <functional>

// the events of Qt handled until ok holds, at most msecs (QTRY_VERIFY)
static bool
wait_until (std::function<bool ()> ok, int msecs) {
  QElapsedTimer t; t.start ();
  while (t.elapsed () < msecs) {
    if (ok ()) return true;
    QCoreApplication::processEvents (QEventLoop::AllEvents, 20);
    QThread::msleep (5);
  }
  return ok ();
}

// A server on 127.0.0.1 (a port which the system chooses): POST /fast is
// answered "NEW" at once; POST /slow is answered "OLD1", then "OLD2" a
// little later (an answer which streams, as those of the AI engines). The
// body of the last request is kept.
class MockServer: public QObject {
public:
  QTcpServer server;
  QByteArray last_body;
  MockServer () {
    connect (&server, &QTcpServer::newConnection, this,
             [this] () { accept (); });
    server.listen (QHostAddress::LocalHost, 0);
  }
  QString url (QString path) {
    return QString ("http://127.0.0.1:%1%2").arg (server.serverPort ()).arg (path);
  }
private:
  void accept () {
    while (server.hasPendingConnections ()) {
      QTcpSocket* s= server.nextPendingConnection ();
      s->setProperty ("data", QByteArray ());
      connect (s, &QTcpSocket::readyRead, this, [this, s] () { read (s); });
      connect (s, &QTcpSocket::disconnected, s, &QObject::deleteLater);
    }
  }
  void read (QTcpSocket* s) {
    QByteArray d= s->property ("data").toByteArray () + s->readAll ();
    s->setProperty ("data", d);
    int h= d.indexOf ("\r\n\r\n");
    if (h < 0) return;
    int len= 0;
    for (const QByteArray& l: d.left (h).split ('\n'))
      if (l.toLower ().startsWith ("content-length:"))
        len= l.mid (15).trimmed ().toInt ();
    if (d.size () < h + 4 + len) return;
    s->setProperty ("data", QByteArray ("done"));
    last_body= d.mid (h + 4, len);
    bool slow= d.startsWith ("POST /slow");
    s->write ("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n"
              "Connection: close\r\n\r\n");
    if (!slow) { s->write ("NEW"); s->disconnectFromHost (); return; }
    s->write ("OLD1");
    s->flush ();
    QPointer<QTcpSocket> p (s);
    QTimer::singleShot (600, this, [p] () {
      if (p.isNull ()) return;
      p->write ("OLD2");
      p->disconnectFromHost ();
    });
  }
};


static array<string>
json_headers () {
  array<string> h;
  h << string ("Content-Type") << string ("application/json");
  return h;
}

static string
as_tm (QString s) {
  QByteArray b= s.toUtf8 ();
  return string (b.constData (), b.size ());
}

// the strings of a request stay strings ("42", "true"); the numbers and the
// booleans of the request are marked (json-number, json-boolean)
static void
test_json_strings () {
#if defined(QTTEXMACS) && AC_QT_MAJOR_VERSION >= 6
  MockServer m;
  CHECK (m.server.isListening ());
  tree data (ATTR);
  data << tree ("content") << tree ("42")
       << tree ("again") << tree ("true")
       << tree ("max_tokens") << compound ("json-number", "64")
       << tree ("stream") << compound ("json-boolean", "true");
  int status= 1;
  string outbuf, errbuf;
  bool kill= false;
  CHECK (!qt_async_http_post (as_tm (m.url ("/fast")), json_headers (),
                                data, status, outbuf, errbuf, kill));
  CHECK (wait_until ([&] () { return status == 0; }, 3000));
  QJsonObject o= QJsonDocument::fromJson (m.last_body).object ();
  CHECK_MSG (o["content"].isString (), m.last_body.constData ());
  CHECK (o["content"].toString () == QString ("42"));
  CHECK_MSG (o["again"].isString (), m.last_body.constData ());
  CHECK (o["again"].toString () == QString ("true"));
  CHECK_MSG (o["max_tokens"].isDouble (), m.last_body.constData ());
  CHECK_EQ (o["max_tokens"].toInt (), 64);
  CHECK_MSG (o["stream"].isBool (), m.last_body.constData ());
  CHECK (o["stream"].toBool ());
#else
  SKIP ("the requests are made by Qt with Qt 6 only");
#endif
}

// an interrupted answer (request_link_rep::interrupt: kill, then
// http_async_cancel), then the next question in the same buffers
// (request_link_rep::write): nothing of the first reaches the second
static void
test_interrupted_answer () {
#if defined(QTTEXMACS) && AC_QT_MAJOR_VERSION >= 6
  MockServer m;
  CHECK (m.server.isListening ());
  tree data (ATTR);
  data << tree ("q") << tree ("x");
  int status= 1;
  string outbuf, errbuf;
  bool kill= false;
  CHECK (!qt_async_http_post (as_tm (m.url ("/slow")), json_headers (),
                                data, status, outbuf, errbuf, kill));
  CHECK (wait_until ([&] () { return occurs ("OLD1", outbuf); }, 3000));
  kill= true;
  http_async_cancel (&outbuf);
  status= 1; outbuf= ""; errbuf= ""; kill= false;
  CHECK (!qt_async_http_post (as_tm (m.url ("/fast")), json_headers (),
                                data, status, outbuf, errbuf, kill));
  CHECK (wait_until ([&] () { return status == 0; }, 3000));
  CHECK_MSG (outbuf == "NEW", outbuf);
  // the time for the first answer to go on (OLD2), had it not been stopped
  (void) wait_until ([] () { return false; }, 1200);
  CHECK_MSG (outbuf == "NEW", outbuf);
  CHECK_EQ (status, 0);
#else
  SKIP ("the requests are made by Qt with Qt 6 only");
#endif
}

int
main (int argc, char** argv) {
  QCoreApplication app (argc, argv);
  RUN (test_json_strings);
  RUN (test_interrupted_answer);
  return test_report ();
}

#else

static void
test_qt6 () {
  SKIP ("the requests are made by Qt with Qt 6 only");
}

int
main () {
  RUN (test_qt6);
  return test_report ();
}

#endif
