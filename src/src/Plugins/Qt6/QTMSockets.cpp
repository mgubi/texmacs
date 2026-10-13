
/******************************************************************************
* MODULE     : QTMsockets.cpp
* DESCRIPTION: QT TeXmacs sockets manager
* COPYRIGHT  : (C) 2015 Denis RAUX
*                  2022 Gregoire LECERF
*                  2025 Robin WILS
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "QTMSockets.hpp"
#include "scheme.hpp"
#include "iterator.hpp"
#include "analyze.hpp"
#include "boot.hpp"
#include "server_log.hpp"
#include "gnutls.hpp"
#include "tm_timer.hpp"
#include "file.hpp"
#include <cctype>
#ifdef QT_WEBSOCKETS_LIB
#include <QFile>
#include <QTimer>
#include <QHostAddress>
#ifndef QT_NO_SSL
#include <QSslCertificate>
#include <QSslConfiguration>
#include <QSslKey>
#endif
#endif

#if defined(OS_MACOS)
  #include "MacOS/mac_utilities.h"
#endif

#ifndef OS_MINGW

#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdint.h>
#include <fcntl.h>
#include <arpa/inet.h>

#define CONNECT ::connect
#define CLOSE(a) close(a)
#define WRITE(a, b, c) ::write(a, b, c)
#define ERRNO errno
#define ERRSOC(a) a 
#define GETADDRINFO getaddrinfo
#define FREEADDRINFO freeaddrinfo
#define ADDRINFO addrinfo
#define SOCKET socket
#define GAI_STRERROR gai_strerror
#define BIND bind
#define LISTEN listen
#define ACCEPT accept
#define INET_NTOP inet_ntop
#define TM_FD_SET(fd, set) FD_SET(fd, set)
#define GETSOCKOPT getsockopt
#define RECV(a, b, c, d) ::recv(a, b, c, d)

#else

#define CONNECT wsoc::connect
#define CLOSE(a) wsoc::closesocket(a)
#define WRITE(a, b, c) wsoc::send(a, b, c, 0) 
#define ERRNO wsoc::WSAGetLastError()
#define ERRSOC(a) WSA##a 
#define GETADDRINFO wsoc::getaddrinfo 
#define FREEADDRINFO wsoc::freeaddrinfo
#define ADDRINFO wsoc::addrinfo
#define SOCKET wsoc::socket
#define GAI_STRERROR wsoc::gai_strerrorA
#define BIND wsoc::bind
#define LISTEN wsoc::listen
#define ACCEPT wsoc::accept
#define INET_NTOP wsoc::inet_ntop
#undef FD_ISSET
#define FD_ISSET __WSAFDIsSet
#define TM_FD_SET(fd, set) FD_SET((u_int) fd, set)
#define GETSOCKOPT wsoc::getsockopt
#define RECV(a, b, c, d) wsoc::recv(a, b, c, d)

#endif

/******************************************************************************
* Utilities
******************************************************************************/

static string
string_from_socket_address (SOCKADDR_STORAGE* sock) {
  static char tmp[128];
  if (sock->ss_family == AF_INET) {
#ifdef OS_MINGW
    return wsoc::inet_ntoa (((SOCKADDR_IN*) sock)->sin_addr);
#else
    if (inet_ntop (AF_INET, &(((sockaddr_in*) sock)->sin_addr),
        tmp, sizeof(tmp)) == NULL)
      return "";
    return tmp;
#endif
  }
  if (sock->ss_family == AF_INET6) {
#if !defined (OS_MINGW) || (_WIN32_WINNT >= 0x0600)
    if (INET_NTOP (AF_INET6, &(((SOCKADDR_IN6*) sock)->sin6_addr),
        tmp, sizeof(tmp)) == NULL)
      return "";
#else
    return "";
#endif
    return string ("[") * tmp * string ("]");
  }
  return "";
}

string
socket_debug_io_string (string s, int max) {
  int i, n= N(s);
  string r;
  for (i=0; i<n && i<max; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else if (std::isprint(c) || std::isspace(c)) r << s[i];
    else r << "\\x" << as_hexadecimal(c);
  }
  if (n > max) {
    r << "... (" << as_string (n) << " bytes truncated to "
      << as_string (max) << ")";
  }
  return r;
}

unsigned long long int qtmsocket_debug_counter= 0;

static int
safe_server_close (int fd) {
  int ret= CLOSE (fd);
  if (ret != 0) {
    SERRNO_LOGE ("server socket close");
    SLOGE ("cannot close server socket. Please exit TeXmacs in order to stop "
        "the server completely");
  }
  return ret;
}

static int
set_socket_noblock (int socket) {
#ifndef OS_MINGW
    if (fcntl (socket, F_SETFL, O_NONBLOCK) == -1)
      return -1;
#else
    {
      using namespace wsoc;
      u_long flags = -1;
      if (ioctlsocket (socket, FIONBIO, &flags) == SOCKET_ERROR)
        return -1;
    }
#endif
    return 0;
}

/******************************************************************************
* Socket initialization
******************************************************************************/

#ifndef OS_MINGW

bool
socket_present () {
  return true;
}

#else

static bool __socket_present= false;
static wsoc::WSADATA wsadata;

struct __WSA_initializer {
  __WSA_initializer () {
	using namespace wsoc;
    if (__socket_present) return;
    int e= WSAStartup (MAKEWORD (2,0), &wsadata);
    if (e == 0)
      __socket_present= true;
    else {
      SERRNO_LOGE ("WSAStartup failed, disabling sockets");
    }
  }
  ~__WSA_initializer () {
    wsoc::WSACleanup ();
    __socket_present= false;
  }
};

bool
socket_present () {
  static __WSA_initializer __dummy;
  (void) __dummy;
  return __socket_present;
}

#endif

int try_connect (const char* host, const char* port, int timeout,
                 char* errbuf, size_t errlen) {
#define MAX_SOCKS 16
  struct ADDRINFO hints;
  struct ADDRINFO *result, *rp;
  memset (&hints, 0, sizeof (hints));
  hints.ai_family= AF_UNSPEC;
  hints.ai_socktype= SOCK_STREAM;
  hints.ai_protocol= 0;
  hints.ai_canonname= NULL;
  hints.ai_addr= NULL;
  hints.ai_next= NULL;

  if (errbuf && errlen > 0) errbuf[0]= '\0';

  int x= GETADDRINFO (host, port, &hints, &result);
  if (x != 0) {
    if (errbuf)
      snprintf (errbuf, errlen, "getaddrinfo: %s", GAI_STRERROR (x));
    return -1;
  }

  int socks[MAX_SOCKS];
  struct tm_pollfd pfds[MAX_SOCKS];
  int n= 0;

  for (rp= result; rp != NULL && n < MAX_SOCKS; rp= rp->ai_next) {
    int s= SOCKET (rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (s < 0) continue;
    if (set_socket_noblock (s) == -1) {
      CLOSE (s);
      continue;
    }

    int ret= CONNECT (s, rp->ai_addr, rp->ai_addrlen);
    if (ret == 0) {
      // immediate success
      for (int j= 0; j < n; j++) CLOSE (socks[j]);
      FREEADDRINFO (result);
      return s;
    }

#if defined(OS_MINGW)
    int err= ERRNO;
    if (err != WSAEWOULDBLOCK) {
      CLOSE (s);
      continue;
    }
#else
    if (errno != EINPROGRESS) {
      CLOSE (s);
      continue;
    }
#endif
    socks[n]= s;
    pfds[n].fd= s;
    pfds[n].events= TM_POLL_WRITE;
    pfds[n].revents= 0;
    n++;
  }
  FREEADDRINFO (result);

  if (n == 0) return -1;

  int ret= tm_poll (pfds, n, timeout);
  if (ret <= 0) {
    for (int i= 0; i < n; i++) CLOSE (socks[i]);
    return -1;
  }

  int winner= -1;
  for (int i= 0; i < n; i++) {
    if (pfds[i].revents & TM_POLL_WRITE) {
      int err= 0;
      socklen_t elen= sizeof (err);
      GETSOCKOPT (socks[i], SOL_SOCKET, SO_ERROR, (char*) &err, &elen);
      if (err == 0) {
        winner= socks[i];
        break;
      }
    }
  }

  for (int i= 0; i < n; i++)
    if (socks[i] != winner) CLOSE (socks[i]);

  return winner;
}

/******************************************************************************
* Socket link
******************************************************************************/

static hashset<pointer> all_connections;

static inline bool
exists (socket_link_rep* s) {
  return all_connections->contains ((pointer) s);
}

static inline void
checkin (socket_link_rep* s) {
  all_connections->insert ((pointer) s);
}

static inline void
checkout (socket_link_rep* s) {
  all_connections->remove ((pointer) s);
}

// to be created by the server
socket_link_rep::socket_link_rep (int fd, SOCKADDR_STORAGE* addr,
    tm_contact contact2):
  host (""), port (0), socket_id (fd), contact (contact2),
    read_notifier_ptr (NULL), write_notifier_ptr (NULL) {
  alive= false;
  memcpy (&address, addr, sizeof(address));
  host= string_from_socket_address (&address);
}

// to be created by clients
socket_link_rep::socket_link_rep (string host2, unsigned short port2,
    tm_contact contact2):
  host (host2), port (port2), socket_id (-1), contact (contact2),
    read_notifier_ptr (NULL), write_notifier_ptr (NULL) {
  alive= false;
}

socket_link_rep::~socket_link_rep () {
  DEBUG_SOCKET("'~socket_link_rep' is closing socket " << socket_id);
  if (socket_id >= 0)
    CLOSE(socket_id);
  input_buffer= "";
  output_buffer= "";
}

string
socket_link_rep::start () {
  if (!socket_present ())
    return "cannot use sockets";
  if (socket_id >= 0) { // used by the server
    if (!is_alive (contact)) {
      string msg= string ("unexpected closed contact for socket ") *
        as_string (socket_id);
      checkin (this);
      SLOGE (msg);
      return msg;
    }
  }
  else { // used for clients
    if (used_by_server ())
      return "internal error, inconsistent 'socket_link_rep'";

    c_string _host (host);
    c_string _port (as_string (port));
    char errbuf[256];

    socket_id= try_connect (_host, _port, 5000, errbuf, sizeof (errbuf));
    if (socket_id < 0) {
      if (errbuf[0] != '\0')
        return string ("'getaddrinfo' error: ") * string (errbuf);
      return "cannot connect to hostname '" * host
        * "' at port " * as_string (port);
    }

    DEBUG_SOCKET("'socket_link_rep::start' created socket with id "
      << socket_id);
    ::start (contact, socket_id);
    if (!is_alive (contact)) {
      CLOSE(socket_id);
      socket_id= -1;
      return "contact has not started";
    }

    SLOG ("contact started for socket " * as_string (socket_id));
    call ("client-add", object (socket_id));
  }
  checkin (this);
  read_notifier_ptr= new QSocketNotifier (socket_id, QSocketNotifier::Read);
  write_notifier_ptr= new QSocketNotifier (socket_id, QSocketNotifier::Write);
  if (!read_notifier_ptr || !write_notifier_ptr)
    return "cannot create socket notifiers";

  if (is_active (contact)) {
    connect_data_notifiers ();
  } else {
    connect_handshake_notifiers ();
  }
  read_notifier_ptr->setEnabled (true);
  write_notifier_ptr->setEnabled (true);
  alive= true;
  return "";
}

void socket_link_rep::connect_data_notifiers () {
  ASSERT (read_notifier_ptr && write_notifier_ptr,
	  "socket notifiers are NULL");
  ASSERT (is_active(contact), "inactive contact");
  SLOG ("connecting data notifiers for socket " * as_string (socket_id));
  read_notifier_ptr->disconnect();
  write_notifier_ptr->disconnect();

#if QT_VERSION < 0x060000
  QObject::connect (read_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (data_set_ready(int)));
  QObject::connect (write_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (ready_to_send(int)));
#else
  QObject::connect (read_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::data_set_ready);
  QObject::connect (write_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::ready_to_send);
#endif
}

void socket_link_rep::connect_handshake_notifiers () {
  ASSERT (read_notifier_ptr && write_notifier_ptr,
	  "socket notifiers are NULL");
  SLOG ("connecting handshake resume notifier for socket "
	* as_string (socket_id));
#if QT_VERSION < 0x060000
  QObject::connect (read_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (resume_start(int)));
  QObject::connect (write_notifier_ptr, SIGNAL (activated(int)),
    this, SLOT (resume_start(int)));
#else
  QObject::connect (read_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::resume_start);
  QObject::connect (write_notifier_ptr, &QSocketNotifier::activated,
    this, &socket_link_rep::resume_start);
#endif
}

void
socket_link_rep::stop () {
  DEBUG_SOCKET("'socket_link_rep::stop' is closing socket " << socket_id);
  if (!alive) {
    checkout (this);
    return;
  }
  if (read_notifier_ptr) {
    read_notifier_ptr->setEnabled (false);
    read_notifier_ptr->disconnect ();
    read_notifier_ptr->deleteLater ();
  }
  if (write_notifier_ptr) {
    write_notifier_ptr->setEnabled (false);
    write_notifier_ptr->disconnect ();
    write_notifier_ptr->deleteLater ();
  }
  if (used_by_server ()) { // socket created by the server
    call ("server-logout-client", object (socket_id));
    call ("server-remove", object (socket_id));
  }
  else { // socket created by a client
    call ("client-remove", object (socket_id));
  }
  alive= false;
  close_socket ();
  if (used_by_server ()) // socket created by the server
    emit disconnection (this);
  else
    socket_id= -1;
  checkout (this);
}

void
socket_link_rep::close_socket () {
  ::stop (contact);
  if (socket_id >= 0) {
    if (CLOSE(socket_id) && used_by_server ())
      SLOGE ("cannot close socket " * as_string (socket_id));
  }
}

string&
socket_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT)
    return input_buffer;
  return empty_string;
}

string
socket_link_rep::read (int channel) {
  DEBUG_SOCKET("'socket_link_rep::read' received size " << N(input_buffer));
  if (channel == LINK_OUT && N(input_buffer)) {
    string r= input_buffer;
    input_buffer= "";
    return r;
  }
  else return "";
}

void
socket_link_rep::write (string s, int channel) {
  DEBUG_SOCKET("'socket_link_rep::write' received size " << N(s));
  if ((!alive) || (channel != LINK_IN) || !N(s))
    return;
  output_buffer << s;
  write_notifier_ptr->setEnabled (true);
}

inline bool
socket_link_rep::retry (int err) {
#ifdef USE_GNUTLS
  return err < 0 && err != EAGAIN && err != GNUTLS_E_AGAIN &&
    err != GNUTLS_E_INTERRUPTED;
#else
  return err < 0 && err != EAGAIN;
#endif
}

void
socket_link_rep::resume_start (int s) {
  if (!exists (this)) return;
  if (!is_alive (contact)) {
    DEBUG_SOCKET ("contact is dead for socket " * as_string (s));
    stop ();
    // on cert error an interactive trust-certificate widget is spawned
    if (!is_headless ()
        && contact->last_error() != ""
        && contact->last_error() != "certificate verify interactive")
      call ("client-open-error", contact->last_error());
    return;
  }
  // Get the notifier that sent the signal
  QSocketNotifier* notifier = qobject_cast<QSocketNotifier*> (sender ());
  if (!is_active (contact)) {
    notifier->setEnabled (false);
    ::start (contact, s);
    if (!is_alive (contact)) {
      stop ();
      if (!is_headless ()
          && contact->last_error() != ""
          && contact->last_error() != "certificate verify interactive")
        call ("client-open-error", contact->last_error());
      return;
    }
  }
  if (is_active (contact))
    connect_data_notifiers();
  notifier->setEnabled (true);
}

void
socket_link_rep::data_set_ready (int s) {
  if (!exists (this)) return;
  read_notifier_ptr->setEnabled (false);
  if (!alive)
    return;
  if (!is_active (contact)) {
    stop ();
    return;
  }
  DEBUG_SOCKET("'socket_link_rep::data_set_ready', socket "
    << socket_id << ", s= " << s);
  char data[16384];
  int n= receive (contact, (void*) data, 16384);
  DEBUG_SOCKET("'socket_link_rep::data_set_ready', socket "
    << socket_id << " 'receive' returned " << n);
  if (n == 0) {
    DEBUG_SOCKET("'socket_link_rep::data_set_ready', socket "
      << socket_id << " hung up");
    if (!used_by_server ())
      io_error << "connection to server '" << host << "' hung up" << LF;
    stop ();
  }
  else if (n < 0) {
    if (is_alive (contact))
      read_notifier_ptr->setEnabled (true);
    else {
      DEBUG_SOCKET("'socket_link_rep::data_set_ready', 'receive' failed: "
        << last_error (contact));
      if (used_by_server ())
	io_error << "connection to client " << s << " aborted" << LF;
      else
        io_error << "connection to server '" << host << "' aborted" << LF;
      stop ();
    }
  }
  else {
    input_buffer << string (data, n);
    if (DEBUG_IO) {
      string s (data, n);
      bool ok= true;
      for (int i= 0; i < N(s); i++)
        if (((int) (unsigned char) s[i]) >= 128 ||
            (((int) (unsigned char) s[i]) < 32 &&
             s[i] != '\n' && s[i] != '\t'))
          ok= false;
      if (ok) {
        DEBUG_SOCKET_DATA(
            "'socket_link_rep::data_set_ready', received data: ", s);
      }
      else {
        DEBUG_SOCKET("'socket_link_rep::data_set_ready', received size "
          << N(s));
      }
    }
    if (!is_nil (feed_cmd))
      feed_cmd->apply ();
    read_notifier_ptr->setEnabled (true);
  }
}

void
socket_link_rep::ready_to_send (int s) {
  if (!exists (this)) return;
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  write_notifier_ptr->setEnabled (false);
  if (!alive)
    return;
  if (!is_active (contact)) {
    stop ();
    return;
  }
  int n= N(output_buffer);
  if (n > 0) {
    c_string buf (output_buffer);
    int ret= send (contact, buf, n);
    DEBUG_SOCKET("'socket_link_rep::ready_to_send', 'send' returned "
      << ret);
    if (ret > 0) {
      if (ret == n)
        output_buffer= "";
      else
        output_buffer= output_buffer (ret, n);
      n -= ret;
      if (n > 0) write_notifier_ptr->setEnabled (true);
    }
    else if (retry (ret)) {
      DEBUG_SOCKET("'socket_link_rep::ready_to_send', error: "
        << last_error (contact));
      if (is_active (contact)) {
        if (used_by_server ())
          io_warning << "retrying connection to client "
            << as_string (s) << ": " << last_error (contact);
        else
          io_warning << "retrying connection to server '"
            << host << "': " << last_error (contact);
        write_notifier_ptr->setEnabled (true);
      }
      else {
        if (used_by_server ())
          io_error << "connection to client "
            << as_string (s) << " aborted: " << last_error (contact);
        else
          io_error << "connection to server '"
            << host << "' aborted: " << last_error (contact);
        stop ();
      }
    }
    else write_notifier_ptr->setEnabled (true);
  }
}

void
socket_link_rep::listen (int msecs) {
  if (!alive) return;
  time_t start = texmacs_time();
  int initial_len = N(input_buffer);
  while (alive && N(input_buffer) == initial_len) {
    qApp->processEvents(QEventLoop::ExcludeUserInputEvents);
    if (msecs > 0 && (texmacs_time() - start >= msecs)) break;
  }
}

/******************************************************************************
* Links of WebSocket clients
*
* A client in a browser can only open WebSockets. Such a link carries the
* usual packets of a TeXmacs client in the binary messages of a QWebSocket,
* which owns the socket (and its TLS session for wss).
******************************************************************************/

#ifdef QT_WEBSOCKETS_LIB

websocket_link_rep::websocket_link_rep (int fd, SOCKADDR_STORAGE* addr,
    QWebSocket* ws2):
  socket_link_rep (fd, addr, tm_contact ()), ws (ws2) {}

websocket_link_rep::~websocket_link_rep () {
  socket_id= -1; // closed by the QWebSocket
}

string
websocket_link_rep::start () {
  checkin (this);
  if (!ws || ws->state () != QAbstractSocket::ConnectedState) {
    string msg= string ("unexpected closed WebSocket for socket ") *
      as_string (socket_id);
    SLOGE (msg);
    if (ws) {
      ws->abort ();
      ws->deleteLater ();
    }
    return msg;
  }
  // as much as fits in a string, and far less than Qt would take
  ws->setMaxAllowedIncomingFrameSize (1 << 28);
  ws->setMaxAllowedIncomingMessageSize (1 << 28);
  QObject::connect (ws, SIGNAL (binaryMessageReceived (const QByteArray&)),
    this, SLOT (binary_received (const QByteArray&)));
  QObject::connect (ws, SIGNAL (textMessageReceived (const QString&)),
    this, SLOT (text_received (const QString&)));
  QObject::connect (ws, SIGNAL (disconnected ()), this, SLOT (closed ()));
  alive= true;
  return "";
}

void
websocket_link_rep::close_socket () {
  if (!ws) return;
  ws->disconnect (this);
  if (ws->state () == QAbstractSocket::UnconnectedState) ws->deleteLater ();
  else {
    // the closing frame is sent before the socket is closed; the socket
    // of a client which does not take it is closed all the same
    QObject::connect (ws, SIGNAL (disconnected ()), ws, SLOT (deleteLater ()));
    QPointer<QWebSocket> ws_ptr (ws);
    QTimer::singleShot (5000, ws, [ws_ptr] () {
      if (!ws_ptr) return;
      ws_ptr->abort ();
      ws_ptr->deleteLater ();
    });
    ws->close ();
  }
}

void
websocket_link_rep::write (string s, int channel) {
  DEBUG_SOCKET("'websocket_link_rep::write' received size " << N(s));
  if ((!alive) || (channel != LINK_IN) || !N(s) || !ws)
    return;
  c_string buf (s);
  ws->sendBinaryMessage (QByteArray ((const char*) buf, N(s)));
}

void
websocket_link_rep::binary_received (const QByteArray& data) {
  if (!exists (this) || !alive) return;
  DEBUG_SOCKET("'websocket_link_rep::binary_received', socket "
    << socket_id << " received size " << (int) data.size ());
  input_buffer << string (data.constData (), (int) data.size ());
  if (!is_nil (feed_cmd))
    feed_cmd->apply ();
}

void
websocket_link_rep::text_received (const QString& data) {
  binary_received (data.toUtf8 ());
}

void
websocket_link_rep::closed () {
  if (!exists (this)) return;
  DEBUG_SOCKET("'websocket_link_rep::closed', socket "
    << socket_id << " hung up");
  stop ();
}

#endif // QT_WEBSOCKETS_LIB

/******************************************************************************
* Server
******************************************************************************/

static hashset<pointer> all_servers;

static inline bool
exists (socket_server_rep* s) {
  return all_servers->contains ((pointer) s);
}

static inline void
checkin (socket_server_rep* s) {
  all_servers->insert ((pointer) s);
}

static inline void
checkout (socket_server_rep* s) {
  all_servers->remove ((pointer) s);
}

/******************************************************************************
* WebSocket clients of the server
*
* The first bytes of a client say what it is, and they are only looked at,
* not read, so that whoever serves the client gets all of them:
*   - "GET " opens the handshake of a WebSocket (ws, TeXmacs in a page served
*     over http, or on this machine), which a QWebSocketServer performs;
*   - a TLS record (0x16, a ClientHello) is a client over TLS. A browser
*     always sends the ALPN extension there and the TeXmacs client never
*     does: the former is a WebSocket over TLS (wss, TeXmacs in a page served
*     over https, which may open no other), whose TLS session is one of Qt
*     (a WebSocket client over TLS which is not a browser has to send that
*     extension too, or it is taken for a TeXmacs client);
*   - any other client is a TeXmacs client, served through a contact.
* A TeXmacs client speaks first (its login, or the handshake of TLS) and the
* server never does, so that waiting for those bytes blocks nobody.
*
* The TLS session of a wss client uses the certificate of the server
* ($TEXMACS_SERVER_CERT_DIR/cert.pem and key.pem, as in gnutls.cpp), which
* the browser must trust: a certificate of an authority for the name of the
* host (Let's Encrypt...), not the self-signed one of the tests.
******************************************************************************/

#ifdef QT_WEBSOCKETS_LIB

#define SNIFF_MORE   0  // not enough bytes yet
#define SNIFF_NATIVE 1  // a TeXmacs client
#define SNIFF_WS     2  // a WebSocket
#define SNIFF_WSS    3  // a WebSocket over TLS

static string
websocket_mode () {
  // who may come as a WebSocket: "local" (this machine only), "on", "off"
#if QT_VERSION < QT_VERSION_CHECK(6,4,0)
  // a browser gives up when the subprotocol it asked for is not confirmed,
  // and the sockets of Emscripten ask for one: this needs
  // QWebSocketServer::setSupportedSubprotocols
  return "off";
#endif
  string mode= get_preference ("server websocket", "local");
  if (mode == "default") mode= "local";
  return mode;
}

// Whether the ClientHello at the start of a TLS connection has the ALPN
// extension (RFC 7301): 1 or 0, and -1 when more bytes are needed
static int
client_hello_has_alpn (const unsigned char* b, int n) {
  if (n < 5) return -1;
  int len= (b[3] << 8) | b[4];
  if (len > 16384) return 0;
  if (n < 5 + len) return -1;
  int p= 5, end= 5 + len;
  if (end - p < 4 || b[p] != 1) return 0; // not a ClientHello
  p += 4;      // type and length of the message
  p += 2 + 32; // version and random bytes
  if (p + 1 > end) return 0;
  p += 1 + b[p]; // session
  if (p + 2 > end) return 0;
  p += 2 + ((b[p] << 8) | b[p+1]); // cipher suites
  if (p + 1 > end) return 0;
  p += 1 + b[p]; // compression methods
  if (p + 2 > end) return 0;
  int ext_end= p + 2 + ((b[p] << 8) | b[p+1]);
  if (ext_end > end) ext_end= end;
  p += 2;
  while (p + 4 <= ext_end) {
    int type= (b[p] << 8) | b[p+1];
    if (type == 16) return 1;
    p += 4 + ((b[p+2] << 8) | b[p+3]);
  }
  return 0;
}

static int
sniff_client (const unsigned char* b, int n) {
  if (b[0] == 'G') {
    for (int i= 0; i < 4 && i < n; i++)
      if (b[i] != (unsigned char) "GET "[i]) return SNIFF_NATIVE;
    return n < 4 ? SNIFF_MORE : SNIFF_WS; // "GET " may come in pieces
  }
  if (b[0] == 0x16) {
    int alpn= client_hello_has_alpn (b, n);
    if (alpn < 0) return SNIFF_MORE;
    return alpn ? SNIFF_WSS : SNIFF_NATIVE;
  }
  return SNIFF_NATIVE;
}

socket_sniffer_rep::socket_sniffer_rep (socket_server_rep* server2, int fd,
    SOCKADDR_STORAGE* addr):
  QObject (server2), server (server2), socket_id (fd), owner (true),
  dead (false), partial_since (0), poll_delay (20) {
  memcpy (&address, addr, sizeof (address));
  notifier_ptr= new QSocketNotifier (socket_id, QSocketNotifier::Read, this);
#if QT_VERSION < 0x060000
  QObject::connect (notifier_ptr, SIGNAL (activated (int)),
    this, SLOT (look ()));
#else
  QObject::connect (notifier_ptr, &QSocketNotifier::activated,
    this, &socket_sniffer_rep::look);
#endif
}

socket_sniffer_rep::~socket_sniffer_rep () {
  if (tls_ptr) delete tls_ptr; // closes the socket
  else if (owner) CLOSE (socket_id);
}

bool
socket_sniffer_rep::allowed (bool tls) {
  // whether a WebSocket client, over TLS or not, is served
  string from= string_from_socket_address (&address);
  bool local= starts (from, "127.") || from == "[::1]" ||
              starts (from, "[::ffff:127.");
  if (websocket_mode () != "on" && !local) {
    SLOGE ("WebSocket client from " * from * " at socket "
        * as_string (socket_id)
        * " is not served (preference \"server websocket\")");
    return false;
  }
  // a server which asks its clients for TLS does not take passwords in
  // clear from other machines either: those have to use wss
  if (!tls && !local && get_preference ("tls-server") == string ("on")) {
    SLOGE ("WebSocket client without TLS from " * from * " at socket "
        * as_string (socket_id)
        * " is not served (preference \"tls-server\")");
    return false;
  }
  return true;
}

void
socket_sniffer_rep::failed () {
  if (dead) return;
  dead= true;
  SLOGE ("connection failed from " * string_from_socket_address (&address)
      * " at socket " * as_string (socket_id));
  server->address_from_id->reset (socket_id);
  if (notifier_ptr) notifier_ptr->setEnabled (false);
  deleteLater ();
}

void
socket_sniffer_rep::look () {
  if (dead || !owner) return;
  static unsigned char buf[5 + 16384]; // a whole TLS record
  int n= RECV (socket_id, (char*) buf, sizeof (buf), MSG_PEEK);
  if (n < 0) {
    int err= ERRNO;
    if (err == ERRSOC(EWOULDBLOCK) || err == ERRSOC(EINTR)) return;
#ifndef OS_MINGW
    if (err == EAGAIN) return;
#endif
  }
  if (n <= 0) { failed (); return; }
  int kind= sniff_client (buf, n);
  // the socket stays readable: the next look is on time, not on its notifier
  notifier_ptr->setEnabled (false);
  if (kind == SNIFF_MORE) {
    if (partial_since == 0) partial_since= texmacs_time ();
    if (texmacs_time () - partial_since < 5000) {
      QTimer::singleShot (poll_delay, this, SLOT (look ()));
      if (poll_delay < 640) poll_delay *= 2;
      return;
    }
    kind= SNIFF_NATIVE; // too slow for a browser
  }
  if (kind != SNIFF_NATIVE && !allowed (kind == SNIFF_WSS)) {
    failed ();
    return;
  }
  if (kind == SNIFF_NATIVE && buf[0] == 0x16 &&
      get_preference ("tls-server") != string ("on")) {
    // it would wait for an answer forever
    SLOGE ("TeXmacs client over TLS from "
        * string_from_socket_address (&address) * " at socket "
        * as_string (socket_id)
        * " is not served (preference \"tls-server\")");
    failed ();
    return;
  }
  if (kind == SNIFF_WSS) { start_tls (); return; }
  owner= false;
  if (kind == SNIFF_NATIVE) server->serve (socket_id, address);
  else {
    QTcpSocket* sock= new QTcpSocket ();
    if (sock->setSocketDescriptor (socket_id))
      server->serve_websocket (sock, socket_id, &address);
    else {
      delete sock;
      owner= true;
      failed ();
      return;
    }
  }
  dead= true;
  deleteLater ();
}

#ifndef QT_NO_SSL

static QByteArray
read_server_file (string name) {
  c_string path (concretize (url ("$TEXMACS_SERVER_CERT_DIR/" * name)));
  QFile file (QString::fromUtf8 ((const char*) path));
  if (!file.open (QIODevice::ReadOnly)) return QByteArray ();
  return file.readAll ();
}

void
socket_sniffer_rep::start_tls () {
  string from= string_from_socket_address (&address);
  QList<QSslCertificate> chain=
    QSslCertificate::fromData (read_server_file ("cert.pem"), QSsl::Pem);
  QByteArray pem= read_server_file ("key.pem");
  QSslKey key (pem, QSsl::Rsa, QSsl::Pem);
  if (key.isNull ()) key= QSslKey (pem, QSsl::Ec, QSsl::Pem);
  if (!QSslSocket::supportsSsl () || chain.isEmpty () || key.isNull ()) {
    SLOGE ("no TLS for the WebSocket client from " * from * " at socket "
        * as_string (socket_id) * (QSslSocket::supportsSsl () ?
          ": no certificate with an RSA or EC key in $TEXMACS_SERVER_CERT_DIR" :
          ": Qt has no support for TLS"));
    failed ();
    return;
  }
  QSslSocket* tls= new QSslSocket ();
  if (!tls->setSocketDescriptor (socket_id)) {
    delete tls;
    failed ();
    return;
  }
  owner= false;
  tls_ptr= tls;
  QSslConfiguration conf= tls->sslConfiguration ();
  conf.setLocalCertificateChain (chain);
  conf.setPrivateKey (key);
  conf.setPeerVerifyMode (QSslSocket::VerifyNone);
  conf.setAllowedNextProtocols (QList<QByteArray> ()
    << QByteArray (QSslConfiguration::NextProtocolHttp1_1));
  tls->setSslConfiguration (conf);
  QObject::connect (tls, SIGNAL (encrypted ()), this, SLOT (encrypted ()));
  QObject::connect (tls, SIGNAL (disconnected ()), this, SLOT (failed ()));
  int timeout= as_int (get_preference ("server contact timeout", "10000"));
  QTimer::singleShot (timeout > 0 ? timeout : 10000, this, SLOT (failed ()));
  SLOGI ("TLS for the WebSocket client from " * from
      * " at socket " * as_string (socket_id));
  tls->startServerEncryption ();
}

#else

void
socket_sniffer_rep::start_tls () {
  SLOGE ("no TLS for the WebSocket client from "
      * string_from_socket_address (&address) * " at socket "
      * as_string (socket_id) * ": Qt has no support for TLS");
  failed ();
}

#endif

void
socket_sniffer_rep::encrypted () {
  if (dead || !tls_ptr) return;
  QTcpSocket* tls= tls_ptr;
  tls_ptr= NULL;
  tls->disconnect (this);
  server->serve_websocket (tls, socket_id, &address);
  dead= true;
  deleteLater ();
}

static QString
websocket_peer (QHostAddress peer, int peer_port,
                QHostAddress local, int local_port) {
  // the two ends of a connection, which no other connection has
  return peer.toString () + QString ("#") + QString::number (peer_port) +
    QString ("#") + local.toString () + QString ("#") +
    QString::number (local_port);
}

void
socket_server_rep::serve_websocket (QTcpSocket* sock, int client,
    SOCKADDR_STORAGE* addr) {
  // hand a client which opens a WebSocket (over TLS or not) to Qt, and
  // remember which socket it is until its handshake is over
  if (!websocket_server_ptr) {
    websocket_server_ptr= new QWebSocketServer (QString ("TeXmacs"),
      QWebSocketServer::NonSecureMode);
#if QT_VERSION >= QT_VERSION_CHECK(6,4,0)
    // the subprotocol of the sockets of Emscripten, which ask for it
    websocket_server_ptr->setSupportedSubprotocols (
      QStringList () << QString ("binary"));
#endif
    connect (websocket_server_ptr, SIGNAL (newConnection ()),
             this, SLOT (websocket_connection ()));
  }
  sock->setParent (websocket_server_ptr);
  QString peer= websocket_peer (sock->peerAddress (), sock->peerPort (),
                                sock->localAddress (), sock->localPort ());
  websocket_pending pending;
  pending.socket_id= client;
  memcpy (&pending.address, addr, sizeof (pending.address));
  websocket_pendings.insert (peer, pending);
  connect (sock, &QAbstractSocket::disconnected, this, [this, peer, client] () {
    // closed before the end of the handshake
    if (!websocket_pendings.contains (peer) ||
        websocket_pendings[peer].socket_id != client) return;
    SLOGE ("WebSocket handshake failed from "
        * string_from_socket_address (&websocket_pendings[peer].address)
        * " at socket " * as_string (client));
    websocket_pendings.remove (peer);
    address_from_id->reset (client);
  });
#if QT_VERSION >= QT_VERSION_CHECK(6,4,0)
  websocket_server_ptr->handleConnection (sock);
#endif
  // a client which does not finish its request is not kept
  int timeout= as_int (get_preference ("server contact timeout", "10000"));
  QPointer<QTcpSocket> sock_ptr (sock);
  QTimer::singleShot (timeout > 0 ? timeout : 10000, this,
                      [this, sock_ptr, peer, client] () {
    if (sock_ptr && websocket_pendings.contains (peer) &&
        websocket_pendings[peer].socket_id == client)
      sock_ptr->abort ();
  });
  // what Qt has read already (with the end of the handshake of TLS) makes
  // no further signal
  if (sock->bytesAvailable () > 0)
    QMetaObject::invokeMethod (sock, "readyRead", Qt::QueuedConnection);
}

void
socket_server_rep::websocket_connection () {
  if (!exists (this) || !websocket_server_ptr) return;
  while (websocket_server_ptr->hasPendingConnections ()) {
    QWebSocket* ws= websocket_server_ptr->nextPendingConnection ();
    QString peer= websocket_peer (ws->peerAddress (), ws->peerPort (),
                                  ws->localAddress (), ws->localPort ());
    if (!websocket_pendings.contains (peer)) {
      SLOGE ("unexpected WebSocket client");
      ws->abort ();
      ws->deleteLater ();
      continue;
    }
    websocket_pending pending= websocket_pendings.take (peer);
    string from= string_from_socket_address (&pending.address);
    SLOGI ("WebSocket opened from " * from
        * " at socket " * as_string (pending.socket_id));
    add_link (new websocket_link_rep (pending.socket_id, &pending.address, ws),
              from);
  }
}

#endif // QT_WEBSOCKETS_LIB

socket_server_rep::socket_server_rep (string host2, unsigned short port2):
  host (host2), port (port2), socket_id (-1), notifier_ptr (NULL),
  socket_ptr_from_id ((pointer) NULL) {}

socket_server_rep::~socket_server_rep () {}

string
socket_server_rep::start () {
#if defined(OS_MACOS)
  mac_begin_server ();
#endif
  socket_id = -1;
  if (!socket_present ()) {
    SLOGE ("cannot use sockets");
    return "cannot use sockets";
  }
  c_string _port (as_string (port));
  c_string _host (host);
  struct ADDRINFO hints;
  struct ADDRINFO *result, *rp;
  memset (&hints, 0, sizeof(struct ADDRINFO));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE;
  hints.ai_protocol = 0;
  hints.ai_canonname = NULL;
  hints.ai_addr = NULL;
  hints.ai_next = NULL;
  int x = GETADDRINFO(host == "" ? (char*) NULL : (char*) _host,
    (char*) _port, &hints, &result);
  if (x != 0)  {
    SLOGE ("'getaddrinfo' failed for " * host * " via port " * as_string (port)
        * ": "  * as_string (GAI_STRERROR(x)));
    return "'getaddrinfo' failed";
  }
  for (rp = result; rp != NULL; rp = rp->ai_next) {
    hostname= string_from_socket_address ((SOCKADDR_STORAGE*) rp->ai_addr);
    SLOGI ("trying to serve at " * hostname * ":" * as_string (port));
    socket_id= SOCKET(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
    if (socket_id < 0) {
      SERRNO_LOGE ("server socket creation");
      socket_id = -1;
      continue;
    }

    if (set_socket_noblock (socket_id) == -1) {
      SERRNO_LOGE ("cannot set socket as non blocking");
      safe_server_close (socket_id);
      socket_id = -1;
      continue;
    }

    if (BIND(socket_id, rp->ai_addr, rp->ai_addrlen) == 0)
      break;
    else if (errno == EADDRINUSE) {
      SERRNO_LOGE ("bind");
      safe_server_close (socket_id);
      socket_id = -1;
      break;
    }
    SERRNO_LOGE ("bind");
    safe_server_close (socket_id);
    socket_id = -1;
  }
  FREEADDRINFO(result); 
  if (socket_id < 0) {
    SLOGE ("cannot start server at " * host * " via " * as_string (port));
    return "cannot create server socket";
  }
  if (LISTEN(socket_id, 10) != 0) {
    SERRNO_LOGE ("listen on " * host * ":" * as_string (port));
    return "'listen' failed";
  }
  checkin (this);
  notifier_ptr= new QSocketNotifier (socket_id, QSocketNotifier::Read);
#if QT_VERSION < 0x060000
  QObject::connect (notifier_ptr, SIGNAL(activated(int)),
    this, SLOT(connection(int)));
#else
  QObject::connect (notifier_ptr, &QSocketNotifier::activated,
    this, &socket_server_rep::connection);
#endif
  SLOGI ("waiting for connections at " * host * ":" * as_string (port));
  call ("server-create-default-admin-account");
  return "";
}

void
socket_server_rep::stop () {
  SLOGI ("stopping server at " * host * ":" * as_string (port));
  iterator<pointer> it= iterate (connections);
  while (it->busy ()) {
    socket_link_rep* c= (socket_link_rep*) it->next ();
    c->stop ();
    disconnection (c);
  }
  if (notifier_ptr) {
    notifier_ptr->disconnect (SIGNAL(activated(int)));
    delete notifier_ptr;
    notifier_ptr= NULL;
  }
#ifdef QT_WEBSOCKETS_LIB
  // the clients which were not yet recognized or are still opening
  QList<socket_sniffer_rep*> sniffers= findChildren<socket_sniffer_rep*> ();
  for (int i= 0; i < sniffers.size (); i++) delete sniffers[i];
  websocket_pendings.clear ();
  if (websocket_server_ptr) {
    // with its clients, once their signals are over
    websocket_server_ptr->disconnect (this);
    websocket_server_ptr->deleteLater ();
    websocket_server_ptr= NULL;
  }
#endif
  safe_server_close (socket_id);
  socket_id= -1;
#if defined(OS_MACOS)
  mac_end_server ();
#endif
  checkout (this);
  SLOGI ("server stopped at " * host * ":" * as_string (port));
  server_log_stop ();
}

void
socket_server_rep::connection (int s) {
  if (!exists (this)) return;
  SLOGI ("new connection received from " * as_string (s));
  int client;
  SOCKADDR_STORAGE cltadd;
  socklen_t sz= sizeof (cltadd);
  if (!notifier_ptr->isEnabled ()) return;
  client= ACCEPT(s, (SOCKADDR*) &cltadd, &sz);
  if (client <= 0) {
    switch (ERRNO) {
    case ERRSOC(EWOULDBLOCK):
    case ERRSOC(ECONNABORTED): break;
    default: {
      notifier_ptr->setEnabled (false);
      SLOGE ("server socket aborted");
      stop (); }
    }
    SLOGE ("connection failed from " * string_from_socket_address (&cltadd));
    return;
  }
  string address= string_from_socket_address (&cltadd);
  address_from_id (client)= address;
  SLOGI ("connection accepted from " * address
      * " at socket " * as_string (client));

#ifndef OS_MINGW
  if (fcntl (client, F_SETFL, O_NONBLOCK) == -1) {
    SERRNO_LOGE ("cannot set socket as non blocking");
    return;
  }
#else
  {
    using namespace wsoc;
    u_long flags = 1;
    if (ioctlsocket (client, FIONBIO, &flags) == SOCKET_ERROR) {
      SERRNO_LOGE ("cannot set socket as non blocking");
      return;
    }
  }
#endif

#ifdef QT_WEBSOCKETS_LIB
  // the clients in a browser come as WebSockets: the first bytes of a
  // client say what it is
  if (websocket_mode () != "off") {
    (void) new socket_sniffer_rep (this, client, &cltadd);
    return;
  }
#endif
  serve (client, cltadd);
}

void
socket_server_rep::serve (int client, SOCKADDR_STORAGE cltadd) {
  // a TeXmacs client, through a contact
  array<array<string> > authentications;
  array<string> _anonymous; _anonymous << string ("anonymous");
  if (get_preference ("tls-server") == string ("on")) {
    if (get_preference ("tls-server authentication anonymous")
      == string ("on"))
      authentications << _anonymous;
  }
  else
    authentications << _anonymous;

  bool is_tls_server = get_preference ("tls-server") == string ("on");
  tm_contact contact= is_tls_server ?
    make_tls_server_contact (authentications) :
    make_socket_server_contact (authentications);

  if (!contact.rep) {
    SLOGE ("contact creation failed from " * string_from_socket_address (&cltadd)
        * " at socket " * as_string (client));
    CLOSE(client);
    if (!gnutls_present() && is_tls_server) {
    SLOGW ("tls-server preference is on but GnuTLS is missing, either disable"
        " tls-server preference or use a TeXmacs version with GnuTLS");
    }
    return;
  }

  ::start (contact, client);

  if (!is_alive (contact)) {
    SLOGE ("contact failed from " * string_from_socket_address (&cltadd)
      * " at socket " * as_string (client));
    ::stop (contact);
    CLOSE(client);
    return;
  } 

  SLOGI ("contact started from " * string_from_socket_address (&cltadd)
    * " at socket " * as_string (client));

  add_link (new socket_link_rep (client, &cltadd, contact),
            string_from_socket_address (&cltadd));
}

void
socket_server_rep::add_link (socket_link_rep* clt, string from) {
  int client= clt->get_socket_id ();
  string st= clt->start ();
  if (st != "") {
    SLOGE ("'socket_link_rep' failed from " * from
      * " at socket " * as_string (client) * ": " * st);
    clt->stop ();
    clt->deleteLater ();
    address_from_id->reset (client);
    return;
  }
  socket_ptr_from_id (clt->get_socket_id ())= (pointer) clt;
  connections->insert ((pointer) clt);
  call ("server-add", object (clt->get_socket_id ()));
  connect (clt, SIGNAL (disconnection(socket_link_rep*)), this,
	   SLOT (disconnection (socket_link_rep*)));
  SLOGI ("'socket_link_rep' started from " * from
    * " at socket " * as_string (client));
}

void
socket_server_rep::disconnection (class socket_link_rep* clt) {
  if (!exists (this)) return;
  int io= clt->get_socket_id ();
  SLOGI ("disconnection of " * clt->get_host_name ()
      * " from socket " * as_string (io));
  connections->remove ((pointer) clt);
  socket_ptr_from_id->reset (io);
  address_from_id->reset (io);
  clt->stop ();
  clt->deleteLater();
}

string
socket_server_rep::read (int id) {
  socket_link_rep* clt= find_connection_ptr (id);
  if (!clt)
    return "";
  if (!clt->complete_packet (LINK_OUT)) return "";
  bool success;
  string ret= clt->read_packet (LINK_OUT, 0, success);
  return ret;
}

void
socket_server_rep::write (int id, string s) {
  socket_link_rep* clt= find_connection_ptr (id);
  if (clt)
    clt->write_packet (s, LINK_IN);
}

socket_link_rep*
socket_server_rep::find_connection_ptr (int id) {
  pointer ptr= socket_ptr_from_id[id];
  if (ptr)
    return (socket_link_rep*) ptr;
  SLOGE ("'socket_server_rep::find_connection_ptr', cannot find socket "
      * as_string (id));
  return NULL;
}

void
socket_server_rep::listen_connections (int msecs) {
  (void) msecs;
  FAILED ("not implemented");
}
