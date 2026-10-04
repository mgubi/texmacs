
/******************************************************************************
* MODULE     : tm_sockets.hpp
* DESCRIPTION: TeXmacs sockets manager (GUI independent, from QTMSockets) - Header
* COPYRIGHT  : (C) 2015 Denis RAUX
*                  2022 Gregoire LECERF
*                  2025 Robin WILS
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_SOCKETS_HPP
#define TM_SOCKETS_HPP

#include <stdint.h>

#ifndef OS_MINGW

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define SOCKADDR_IN sockaddr_in
#define SOCKADDR_IN6 sockaddr_in6
#define SOCKADDR sockaddr
#define SOCKADDR_STORAGE sockaddr_storage

#else

namespace wsoc {
#include <winsock2.h>
#include <ws2tcpip.h>
}
typedef uint32_t in_addr_t;
typedef int socklen_t;
#define SOCKADDR_IN wsoc::sockaddr_in
#define SOCKADDR_IN6 wsoc::sockaddr_in6
#define SOCKADDR wsoc::sockaddr
#define SOCKADDR_STORAGE wsoc::sockaddr_storage
#undef FAILED
#endif

#include "hashset.hpp"
#include "hashmap.hpp"
#include "string.hpp"
#include "tm_link.hpp"
#include "socket_contact.hpp"
#include "socket_notifier.hpp"


/******************************************************************************
* Socket utilities
******************************************************************************/

extern unsigned long long int tmsocket_debug_counter;

string socket_debug_io_string (string s, int max);

#define DEBUG_SOCKET(a) \
  if (DEBUG_SOCKETS) debug_io << "TeXmacs " \
	<< tmsocket_debug_counter++ << "] " << a << "\n"

#define DEBUG_SOCKET_DATA(a,s) \
   if (N(s)) DEBUG_SOCKET (a << socket_debug_io_string (s, 2048))

/******************************************************************************
* Socket initialization
******************************************************************************/

int try_connect (const char* host, const char* port, int timeout_ms,
                 char* errbuf, size_t errlen);

/******************************************************************************
* Socket link
******************************************************************************/

class socket_server_rep;

// A connection (a client of a remote server, or one client of our server),
// GUI independent: the readiness of its socket is watched through the
// socket notifiers of System/Link, which the GUI loops poll
// (perform_select in the interpose handler).
class socket_link_rep: public tm_link_rep {
public:
  socket_link_rep (int io, SOCKADDR_STORAGE* addr, tm_contact contact,
                   socket_server_rep* server);
  socket_link_rep (string host2, unsigned short port2, tm_contact contact);
  ~socket_link_rep ();
  string  start ();
  void    write (string s, int channel=LINK_OUT);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt () {}
  void    stop ();
  int     get_socket_id () { return socket_id; }
  string  get_host_name () { return host; }
  bool    retry (int err);

  void data_set_ready (int);
  void ready_to_send (int);
  void resume_start (int);
private:
  string host;
  unsigned short port;
  int socket_id;
  tm_contact contact;
  string input_buffer;
  string output_buffer;
  socket_server_rep* server; // the server this connection belongs to (or NULL)
  bool handshake;            // the notifiers resume the handshake of the contact
  bool read_enabled, write_enabled;
  socket_notifier read_notifier, write_notifier;
  SOCKADDR_STORAGE address;
  inline bool used_by_server () { return port == 0; }

  void enable_read (bool on);
  void enable_write (bool on);
  void connect_data_notifiers ();
  void connect_handshake_notifiers ();
  static void read_callback (void* obj, void* info);
  static void write_callback (void* obj, void* info);
};

/******************************************************************************
* Server
******************************************************************************/

class socket_server_rep {
public:
  string host, hostname;
  unsigned short port;
  hashmap<int,string> address_from_id;
  socket_server_rep (string host2, unsigned short port2= 6561);
  ~socket_server_rep ();
  string start ();
  void stop ();
  string read (int client);
  void write (int client, string s);
  int number_of_connections () {
    return N(connections); }
  void listen_connections (int msecs);
  void connection (int client);
  void disconnection (socket_link_rep* client_ptr);
private:
  int socket_id;
  hashset<pointer> connections;
  bool listening;            // the notifier of the server socket is registered
  socket_notifier notifier;
  hashmap<int,pointer> socket_ptr_from_id;
  socket_link_rep* find_connection_ptr (int id);
  static void accept_callback (void* obj, void* info);
};

// the links which were stopped and wait to be freed (a link cannot free
// itself from its own callbacks): freed at the next poll
void collect_stopped_links ();

#endif // TM_SOCKETS_HPP
