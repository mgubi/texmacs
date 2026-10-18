/******************************************************************************
* MODULE     : qt_http_test.cpp
* DESCRIPTION: the requests of the request links with Qt 6, to a local mock
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "tree.hpp"
#include "analyze.hpp"
#include "web_files.hpp"

#include <QTcpServer>
#include <QTcpSocket>
#include <QTimer>
#include <QJsonDocument>
#include <QJsonObject>

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

class TestQtHttp: public QObject {
  Q_OBJECT

private slots:
  void test_json_strings ();
  void test_interrupted_answer ();
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
void
TestQtHttp::test_json_strings () {
#if defined(QTTEXMACS) && AC_QT_MAJOR_VERSION >= 6
  MockServer m;
  QVERIFY (m.server.isListening ());
  tree data (ATTR);
  data << tree ("content") << tree ("42")
       << tree ("again") << tree ("true")
       << tree ("max_tokens") << compound ("json-number", "64")
       << tree ("stream") << compound ("json-boolean", "true");
  int status= 1;
  string outbuf, errbuf;
  bool kill= false;
  QVERIFY (!qt_async_http_post (as_tm (m.url ("/fast")), json_headers (),
                                data, status, outbuf, errbuf, kill));
  QTRY_VERIFY_WITH_TIMEOUT (status == 0, 3000);
  QJsonObject o= QJsonDocument::fromJson (m.last_body).object ();
  QVERIFY2 (o["content"].isString (), m.last_body.constData ());
  QCOMPARE (o["content"].toString (), QString ("42"));
  QVERIFY2 (o["again"].isString (), m.last_body.constData ());
  QCOMPARE (o["again"].toString (), QString ("true"));
  QVERIFY2 (o["max_tokens"].isDouble (), m.last_body.constData ());
  QCOMPARE (o["max_tokens"].toInt (), 64);
  QVERIFY2 (o["stream"].isBool (), m.last_body.constData ());
  QVERIFY (o["stream"].toBool ());
#else
  QSKIP ("the requests are made by Qt with Qt 6 only");
#endif
}

// an interrupted answer (request_link_rep::interrupt: kill, then
// http_async_cancel), then the next question in the same buffers
// (request_link_rep::write): nothing of the first reaches the second
void
TestQtHttp::test_interrupted_answer () {
#if defined(QTTEXMACS) && AC_QT_MAJOR_VERSION >= 6
  MockServer m;
  QVERIFY (m.server.isListening ());
  tree data (ATTR);
  data << tree ("q") << tree ("x");
  int status= 1;
  string outbuf, errbuf;
  bool kill= false;
  QVERIFY (!qt_async_http_post (as_tm (m.url ("/slow")), json_headers (),
                                data, status, outbuf, errbuf, kill));
  QTRY_VERIFY_WITH_TIMEOUT (occurs ("OLD1", outbuf), 3000);
  kill= true;
  http_async_cancel (&outbuf);
  status= 1; outbuf= ""; errbuf= ""; kill= false;
  QVERIFY (!qt_async_http_post (as_tm (m.url ("/fast")), json_headers (),
                                data, status, outbuf, errbuf, kill));
  QTRY_VERIFY_WITH_TIMEOUT (status == 0, 3000);
  QVERIFY2 (outbuf == "NEW", as_charp (outbuf));
  // the time for the first answer to go on (OLD2), had it not been stopped
  QTest::qWait (1200);
  QVERIFY2 (outbuf == "NEW", as_charp (outbuf));
  QCOMPARE (status, 0);
#else
  QSKIP ("the requests are made by Qt with Qt 6 only");
#endif
}

QTEST_MAIN(TestQtHttp)
#include "qt_http_test.moc"
