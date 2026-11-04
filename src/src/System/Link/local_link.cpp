/******************************************************************************
* MODULE     : local_link.cpp
* DESCRIPTION: Loopback links between TeXmacs processes of one user
*              (shared files, see src/docs/shared-files.md)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "local_link.hpp"
#include "analyze.hpp"
#include "array.hpp"
#include "config.h"
#include "tm_timer.hpp"

#ifdef __EMSCRIPTEN__
// The browser has no processes of the user to share files with: the
// functions do nothing (as on Windows, where the feature is off).
tm_link make_local_link (int) { return tm_link (); }
int    local_listen () { return 0; }
void   local_unlisten () {}
int    local_accept () { return -1; }
int    local_connect (int) { return -1; }
bool   local_send (int, string) { return false; }
string local_receive (int) { return ""; }
bool   local_alive (int) { return false; }
void   local_set_limit (int, int) {}
bool   local_send_raw (int, string) { return false; }
void   local_close (int) {}
int    shared_process_id () { return 0; }
bool   shared_pid_alive (int) { return false; }
int    shared_port_probe (int) { return -1; }
string shared_real_path (string) { return ""; }
string shared_user_dir () { return ""; }
tree   shared_list_dir (string) { return tuple (); }
bool   shared_set_private (string) { return false; }
bool   shared_create_file (string, string) { return false; }
bool   shared_create_document (string, string) { return false; }
bool   shared_lexists (string) { return false; }
bool   shared_replace_file (string, string) { return false; }
string shared_random_token () { return "00000000000000000000000000000000"; }
int    shared_lock_acquire (string) { return -2; }
void   shared_lock_release (int) {}
bool   shared_lock_valid (int, string) { return false; }
int    shared_lock_renew (int, string) { return -1; }
void   shared_touch (string) {}
#else
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#ifndef OS_MINGW
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#define L_CLOSE(a)    ::close (a)
#define L_SEND(a,b,c) ::send (a, b, c, L_NOSIGNAL)
#define L_RECV(a,b,c) ::recv (a, b, c, 0)
#define L_BIND        ::bind
#define L_LISTEN      ::listen
#define L_ACCEPT      ::accept
#define L_CONNECT     ::connect
#define L_SOCKET      ::socket
#define L_ERRNO       errno
#define L_WOULDBLOCK(e) ((e) == EAGAIN || (e) == EWOULDBLOCK || (e) == EINTR)
#define L_SOCK_T int
#ifdef MSG_NOSIGNAL
#define L_NOSIGNAL MSG_NOSIGNAL
#else
#define L_NOSIGNAL 0
#endif
#else
// Windows (not tested in the first version of the shared files): winsock in
// a namespace, as in socket_contact.cpp, to keep its names out of TeXmacs
#include <process.h>
#include <direct.h>
namespace wsoc {
#include <winsock2.h>
#include <ws2tcpip.h>
}
using namespace wsoc;
#define L_CLOSE(a)    wsoc::closesocket (a)
#define L_SEND(a,b,c) wsoc::send (a, b, c, 0)
#define L_RECV(a,b,c) wsoc::recv (a, b, c, 0)
#define L_BIND        wsoc::bind
#define L_LISTEN      wsoc::listen
#define L_ACCEPT      wsoc::accept
#define L_CONNECT     wsoc::connect
#define L_SOCKET      wsoc::socket
#define L_ERRNO       WSAGetLastError ()
#define L_WOULDBLOCK(e) ((e) == WSAEWOULDBLOCK || (e) == WSAEINTR)
#define L_SOCK_T wsoc::SOCKET
#endif

/******************************************************************************
* The link
******************************************************************************/

static bool
local_sockets_ready () {
#ifdef OS_MINGW
  static bool init= false, ok= false;
  if (!init) {
    init= true;
    WSADATA wsadata;
    ok= (WSAStartup (MAKEWORD (2,0), &wsadata) == 0);
  }
  return ok;
#else
  static bool init= false;
  if (!init) { init= true; signal (SIGPIPE, SIG_IGN); }
  return true;
#endif
}

static void
local_nonblocking (int fd) {
#ifndef OS_MINGW
  int fl= fcntl (fd, F_GETFL, 0);
  if (fl >= 0) fcntl (fd, F_SETFL, fl | O_NONBLOCK);
  // not given to the programs that are started: they would keep the
  // connection open after this process has died
  int fd_fl= fcntl (fd, F_GETFD, 0);
  if (fd_fl >= 0) fcntl (fd, F_SETFD, fd_fl | FD_CLOEXEC);
#ifdef SO_NOSIGPIPE
  int one= 1;
  setsockopt (fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof (one));
#endif
#else
  u_long on= 1;
  ioctlsocket ((L_SOCK_T) fd, FIONBIO, &on);
#endif
}

struct local_link_rep: tm_link_rep {
  int    fd;
  string in;      // bytes received, not yet a whole packet
  string out;     // bytes not yet taken by the peer
  int    limit;   // the longest packet (and the buffer before a whole one)
public:
  local_link_rep (int fd2): fd (fd2), limit (65536) { alive= (fd >= 0); }
  ~local_link_rep () { stop (); }

  string  start () { return alive? "ok": "dead"; }
  void    write (string s, int channel);
  string& watch (int channel) { (void) channel; fill (); return in; }
  string  read (int channel);
  void    listen (int msecs) { (void) msecs; fill (); flush (); }
  void    interrupt () {}
  void    stop ();
  bool    pending_write () { return N(out) > 0; }

  bool    valid ();   // is in the beginning of a packet within the limit?
  void    fill ();    // what has come, into in
  void    flush ();   // what can be written of out
};

// What a peer may send before it is trusted: packets are <length>\n<payload>
// with the length in decimal digits (not the "!" of the encrypted links of
// tm_link, nor a sign), at most 11 of them, and at most `limit' bytes. A link
// which does not follow this is closed: the memory is bounded.
bool
local_link_rep::valid () {
  int n= N(in);
  if (n == 0) return true;
  if ((long long) n > 2LL * limit + 16) return false;
  int i= 0;
  while (i < n && i < 12 && in[i] >= '0' && in[i] <= '9') i++;
  if (i == 0 || i > 11) return false;
  if (i == n) return true;                    // the digits are not ended yet
  if (in[i] != '\n') return false;
  long long len= 0;
  for (int j=0; j<i; j++) len= 10 * len + (in[j] - '0');
  return len <= (long long) limit;
}

void
local_link_rep::fill () {
  if (!alive) return;
  char buf[65536];
  while (true) {
    int r= (int) L_RECV ((L_SOCK_T) fd, buf, sizeof (buf));
    if (r > 0) in << string (buf, r);
    else if (r == 0) { stop (); return; }
    else {
      if (L_WOULDBLOCK (L_ERRNO)) break;
      stop (); return;
    }
    if (!valid ()) break;
    if (r < (int) sizeof (buf)) break;
  }
  if (!valid ()) { in= ""; stop (); }
}

void
local_link_rep::flush () {
  if (!alive) return;
  while (N(out) > 0) {
    c_string p (out);
    int r= (int) L_SEND ((L_SOCK_T) fd, (char*) p, N(out));
    if (r > 0) out= out (r, N(out));
    else if (r < 0 && L_WOULDBLOCK (L_ERRNO)) return;
    else { stop (); return; }
  }
}

void
local_link_rep::write (string s, int channel) {
  if (!alive || channel != LINK_IN) return;
  out << s;
  flush ();
}

string
local_link_rep::read (int channel) {
  (void) channel;
  fill ();
  string r= in;
  in= "";
  return r;
}

void
local_link_rep::stop () {
  if (fd >= 0) {
    L_CLOSE ((L_SOCK_T) fd);
    fd= -1;
  }
  alive= false;
}

tm_link
make_local_link (int fd) {
  local_sockets_ready ();
  local_nonblocking (fd);
  return tm_new<local_link_rep> (fd);
}

/******************************************************************************
* The links of the Scheme programs
******************************************************************************/

static array<tm_link> local_links;
static int local_server_fd= -1;
// links closed by their owner which still have output to write
static array<tm_link> local_closing;
static array<int>     local_closing_polls;  // polls left before it is dropped

static void
local_flush_closing () {
  for (int i= N(local_closing) - 1; i >= 0; i--) {
    local_link_rep* ln= (local_link_rep*) local_closing[i].rep;
    ln->flush ();
    if (!ln->alive || N(ln->out) == 0 || --local_closing_polls[i] <= 0) {
      ln->stop ();
      for (int j=i; j<N(local_closing)-1; j++) {
        local_closing[j]= local_closing[j+1];
        local_closing_polls[j]= local_closing_polls[j+1];
      }
      local_closing->resize (N(local_closing)-1);
      local_closing_polls->resize (N(local_closing_polls)-1);
    }
  }
}

static int
local_register (tm_link ln) {
  for (int i=0; i<N(local_links); i++)
    if (is_nil (local_links[i])) { local_links[i]= ln; return i; }
  local_links << ln;
  return N(local_links) - 1;
}

static local_link_rep*
local_get (int id) {
  if (id < 0 || id >= N(local_links) || is_nil (local_links[id])) return NULL;
  return (local_link_rep*) local_links[id].rep;
}

int
local_listen () {
  if (!local_sockets_ready ()) return 0;
  if (local_server_fd < 0) {
    int fd= (int) L_SOCKET (AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return 0;
    struct sockaddr_in addr;
    memset (&addr, 0, sizeof (addr));
    addr.sin_family     = AF_INET;
    addr.sin_addr.s_addr= htonl (INADDR_LOOPBACK);   // 127.0.0.1 only
    addr.sin_port       = 0;
    if (L_BIND ((L_SOCK_T) fd, (struct sockaddr*) &addr, sizeof (addr)) != 0 ||
        L_LISTEN ((L_SOCK_T) fd, 16) != 0) {
      L_CLOSE ((L_SOCK_T) fd);
      return 0;
    }
    local_nonblocking (fd);
    local_server_fd= fd;
  }
  struct sockaddr_in addr;
#ifndef OS_MINGW
  socklen_t len= sizeof (addr);
#else
  int len= sizeof (addr);
#endif
  if (getsockname ((L_SOCK_T) local_server_fd, (struct sockaddr*) &addr, &len) != 0)
    return 0;
  return (int) ntohs (addr.sin_port);
}

void
local_unlisten () {
  if (local_server_fd >= 0) L_CLOSE ((L_SOCK_T) local_server_fd);
  local_server_fd= -1;
}

int
local_accept () {
  local_flush_closing ();
  if (local_server_fd < 0) return -1;
  struct sockaddr_in addr;
#ifndef OS_MINGW
  socklen_t len= sizeof (addr);
#else
  int len= sizeof (addr);
#endif
  int fd= (int) L_ACCEPT ((L_SOCK_T) local_server_fd,
                          (struct sockaddr*) &addr, &len);
  if (fd < 0) return -1;
  return local_register (make_local_link (fd));
}

// The link, -2 when the connection is refused (nothing listens there), -1 for
// any other error (too many files, not allowed...: nothing is known about the
// process that may be there)
int
local_connect (int port) {
  if (!local_sockets_ready ()) return -1;
  struct sockaddr_in addr;
  memset (&addr, 0, sizeof (addr));
  addr.sin_family     = AF_INET;
  addr.sin_addr.s_addr= htonl (INADDR_LOOPBACK);
  addr.sin_port       = htons ((unsigned short) port);
  // (a connection to the loopback is made or refused at once; after an
  // interrupted connect the state of the socket is not defined: try again
  // with a new one)
  int fd= -1, err= 0;
  for (int tries= 0; tries < 3; tries++) {
    fd= (int) L_SOCKET (AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    if (L_CONNECT ((L_SOCK_T) fd, (struct sockaddr*) &addr, sizeof (addr)) == 0)
      break;
    err= L_ERRNO;
    L_CLOSE ((L_SOCK_T) fd);
    fd= -1;
#ifndef OS_MINGW
    if (err != EINTR) break;
#else
    break;
#endif
  }
  if (fd < 0) {
#ifndef OS_MINGW
    return err == ECONNREFUSED? -2: -1;
#else
    return err == WSAECONNREFUSED? -2: -1;
#endif
  }
  tm_link ln= make_local_link (fd);
  ((local_link_rep*) ln.rep)->limit= 64 * 1024 * 1024;
  return local_register (ln);
}

bool
local_send (int id, string s) {
  local_link_rep* ln= local_get (id);
  if (ln == NULL) return false;
  ln->write_packet (s, LINK_IN);
  return ln->alive;
}

string
local_receive (int id) {
  local_link_rep* ln= local_get (id);
  if (ln == NULL) return "";
  ln->fill ();
  ln->flush ();
  bool success;
  string r= ln->read_packet (LINK_OUT, 0, success);
  return success? r: string ("");
}

bool
local_alive (int id) {
  local_link_rep* ln= local_get (id);
  if (ln == NULL) return false;
  ln->fill ();
  ln->flush ();
  // (a link which is closed by the peer may still hold a whole packet)
  return ln->alive || ln->complete_packet (LINK_OUT);
}

void
local_close (int id) {
  local_link_rep* ln= local_get (id);
  if (ln == NULL) return;
  ln->flush ();
  if (ln->alive && N(ln->out) > 0) {
    // (the rest is written by the next polls: local_accept)
    local_closing << local_links[id];
    local_closing_polls << 250;
  }
  else ln->stop ();
  local_links[id]= tm_link ();
}

void
local_set_limit (int id, int bytes) {
  local_link_rep* ln= local_get (id);
  if (ln != NULL && bytes > 0) ln->limit= bytes;
}

bool
local_send_raw (int id, string s) {
  local_link_rep* ln= local_get (id);
  if (ln == NULL) return false;
  ln->write (s, LINK_IN);
  return ln->alive;
}

/******************************************************************************
* Helpers of the shared files
******************************************************************************/

int
shared_port_probe (int port) {
#ifndef OS_MINGW
  if (port <= 0 || port > 65535) return 0;
  int fd= (int) socket (AF_INET, SOCK_STREAM, 0);
  if (fd < 0) return -1;
  int fl= fcntl (fd, F_GETFL, 0);
  fcntl (fd, F_SETFL, fl | O_NONBLOCK);
  struct sockaddr_in addr;
  memset (&addr, 0, sizeof (addr));
  addr.sin_family     = AF_INET;
  addr.sin_addr.s_addr= htonl (INADDR_LOOPBACK);
  addr.sin_port       = htons ((unsigned short) port);
  int r= connect (fd, (struct sockaddr*) &addr, sizeof (addr));
  int res= -1;
  if (r == 0) res= 1;
  else if (errno == ECONNREFUSED) res= 0;
  else if (errno == EINPROGRESS) {
    struct pollfd p;
    p.fd= fd; p.events= POLLOUT; p.revents= 0;
    if (poll (&p, 1, 100) > 0) {
      int err= 0; socklen_t len= sizeof (err);
      getsockopt (fd, SOL_SOCKET, SO_ERROR, &err, &len);
      res= (err == 0)? 1: (err == ECONNREFUSED? 0: -1);
    }
  }
  close (fd);
  return res;
#else
  (void) port;
  return -1;
#endif
}

bool
shared_pid_alive (int pid) {
#ifndef OS_MINGW
  if (pid <= 0) return false;
  // (a stopped process exists: this does not touch it, nor its sockets)
  return kill ((pid_t) pid, 0) == 0 || errno == EPERM;
#else
  (void) pid;
  return true;
#endif
}

int
shared_process_id () {
#ifndef OS_MINGW
  return (int) getpid ();
#else
  return (int) _getpid ();
#endif
}

string
shared_real_path (string path) {
  c_string p (path);
#ifndef OS_MINGW
  char* r= realpath ((char*) p, NULL);
  if (r == NULL) return "";
  string s (r);
  free (r);
#ifdef F_GETPATH
  // macOS: on a volume that is not case sensitive realpath keeps the case
  // it was given; the system knows the stored one (so that one file has one
  // key whatever the case of its name)
  {
    c_string q (s);
    int fd= open ((char*) q, O_RDONLY | O_NONBLOCK);
    if (fd >= 0) {
      char buf[1024];
      if (fcntl (fd, F_GETPATH, buf) != -1) s= string (buf);
      ::close (fd);
    }
  }
#endif
  return s;
#else
  char* r= _fullpath (NULL, (char*) p, 0);
  if (r == NULL) return "";
  string s (r);
  free (r);
  return s;
#endif
}

bool
shared_set_private (string path) {
#ifndef OS_MINGW
  c_string p (path);
  return chmod ((char*) p, 0600) == 0;
#else
  (void) path;
  return true;
#endif
}

static string
shared_env (const char* name) {
  const char* v= getenv (name);
  return v == NULL? string (""): string (v);
}

string
shared_user_dir () {
  // The same folder for every process of the user, whatever its environment:
  // /tmp/texmacs-<uid>/shared, with /tmp as the system resolves it (on macOS
  // /private/tmp). (TEXMACS_SHARED_DIR replaces /tmp: for the tests.) It must
  // be a folder of the user which nobody else can enter (mode 0700), else
  // there is none. Files in /tmp may be cleaned by age: the hosts touch theirs.
#ifndef OS_MINGW
  string base= shared_env ("TEXMACS_SHARED_DIR");
  if (base == "") base= "/tmp";
  base= shared_real_path (base);
  if (base == "") return "";
  while (N(base) > 1 && base[N(base)-1] == '/') base= base (0, N(base)-1);
  string top = base * "/texmacs-" * as_string ((int) getuid ());
  string dir = top * "/shared";
  string dirs[2]; dirs[0]= top; dirs[1]= dir;
  for (int i=0; i<2; i++) {
    c_string p (dirs[i]);
    mkdir ((char*) p, 0700);
    struct stat st;
    if (lstat ((char*) p, &st) != 0) return "";
    if (!S_ISDIR (st.st_mode) || st.st_uid != getuid ()) return "";
    if ((st.st_mode & 077) != 0 && chmod ((char*) p, 0700) != 0) return "";
  }
  return dir;
#else
  // Not on Windows (yet): it needs a system source of random numbers
  // (BCryptGenRandom) and a file lock (LockFileEx), which were not written;
  // the preference has no effect there.
  return "";
#endif
}

tree
shared_list_dir (string dir) {
  tree r (TUPLE);
#ifndef OS_MINGW
  c_string p (dir);
  DIR* d= opendir ((char*) p);
  if (d == NULL) return r;
  struct dirent* e;
  while ((e= readdir (d)) != NULL) {
    string name (e->d_name);
    if (name != "." && name != "..") r << tree (name);
  }
  closedir (d);
#endif
  return r;
}

// A file is made with its whole content, or not at all: the entries of the
// shared files are read by other processes at any time
bool
shared_lexists (string path) {
#ifndef OS_MINGW
  c_string p (path);
  struct stat st;
  return lstat ((char*) p, &st) == 0;
#else
  c_string p (path);
  FILE* f= fopen ((char*) p, "rb");
  if (f != NULL) { fclose (f); return true; }
  return false;
#endif
}

bool
shared_create_document (string path, string s) {
  // (never follows a link: O_EXCL fails on any existing path, a dangling
  // symbolic link included)
  c_string p (path);
#ifndef OS_MINGW
  int fd= open ((char*) p, O_WRONLY | O_CREAT | O_EXCL, 0666);
  if (fd < 0) return false;
  c_string c (s);
  int n= N(s), done= 0;
  while (done < n) {
    int r= (int) ::write (fd, ((char*) c) + done, n - done);
    if (r <= 0) { ::close (fd); unlink ((char*) p); return false; }
    done += r;
  }
  ::close (fd);
  return true;
#else
  return shared_create_file (path, s);
#endif
}

bool
shared_create_file (string path, string s) {
  c_string p (path);
#ifndef OS_MINGW
  int fd= open ((char*) p, O_WRONLY | O_CREAT | O_EXCL, 0600);
  if (fd < 0) return false;
  c_string c (s);
  int n= N(s), done= 0;
  while (done < n) {
    int r= (int) ::write (fd, ((char*) c) + done, n - done);
    if (r <= 0) { ::close (fd); unlink ((char*) p); return false; }
    done += r;
  }
  ::close (fd);
  return true;
#else
  FILE* f= fopen ((char*) p, "rb");
  if (f != NULL) { fclose (f); return false; }
  f= fopen ((char*) p, "wb");
  if (f == NULL) return false;
  c_string c (s);
  bool ok= (fwrite ((char*) c, 1, N(s), f) == (size_t) N(s));
  fclose (f);
  return ok;
#endif
}

bool
shared_replace_file (string path, string s) {
  string tmp= path * ".tmp" * as_string (shared_process_id ());
  c_string p (path), t (tmp);
#ifndef OS_MINGW
  unlink ((char*) t);
  if (!shared_create_file (tmp, s)) return false;
  if (rename ((char*) t, (char*) p) != 0) { unlink ((char*) t); return false; }
  return true;
#else
  remove ((char*) t);
  if (!shared_create_file (tmp, s)) return false;
  remove ((char*) p);
  return rename ((char*) t, (char*) p) == 0;
#endif
}

string
shared_random_token () {
  unsigned char buf[16];
  bool ok= false;
#ifndef OS_MINGW
  FILE* f= fopen ("/dev/urandom", "rb");
  if (f != NULL) {
    ok= (fread (buf, 1, 16, f) == 16);
    fclose (f);
  }
#endif
  if (!ok) {
    // (no system source: not secure; the entries are in a folder of mode 0700)
    unsigned long long x= (unsigned long long) texmacs_time ();
    x= x * 6364136223846793005ULL + (unsigned long long) shared_process_id ();
    for (int i=0; i<16; i++) {
      x= x * 6364136223846793005ULL + 1442695040888963407ULL;
      buf[i]= (unsigned char) (x >> 33);
    }
  }
  string r;
  const char* hex= "0123456789abcdef";
  for (int i=0; i<16; i++) {
    r << hex[buf[i] >> 4];
    r << hex[buf[i] & 15];
  }
  return r;
}

// The authority to host a file is an exclusive advisory lock (flock) on a
// file of the per-user folder: held exactly while the process lives and keeps
// it (the system frees it when the process dies, however it dies, and never
// while it only stalls). The descriptor is not given to programs that are
// started (close on exec).
static array<int> shared_lock_fds;

int
shared_lock_acquire (string path) {
#ifndef OS_MINGW
  c_string p (path);
  int fd= open ((char*) p, O_RDWR | O_CREAT, 0600);
  if (fd < 0) return -2;
  int fl= fcntl (fd, F_GETFD, 0);
  if (fl >= 0) fcntl (fd, F_SETFD, fl | FD_CLOEXEC);
  if (flock (fd, LOCK_EX | LOCK_NB) != 0) {
    int e= errno;
    ::close (fd);
    return e == EWOULDBLOCK? -1: -2;   // -1: someone holds it
  }
  for (int i=0; i<N(shared_lock_fds); i++)
    if (shared_lock_fds[i] < 0) { shared_lock_fds[i]= fd; return i; }
  shared_lock_fds << fd;
  return N(shared_lock_fds) - 1;
#else
  (void) path;
  return -2;
#endif
}

void
shared_lock_release (int id) {
#ifndef OS_MINGW
  if (id < 0 || id >= N(shared_lock_fds) || shared_lock_fds[id] < 0) return;
  ::close (shared_lock_fds[id]);   // (closing frees the lock)
  shared_lock_fds[id]= -1;
#else
  (void) id;
#endif
}

// Is the file which the lock @id holds still the file at @path? (If the file
// was removed, or replaced, another process could make and lock a new one.)
bool
shared_lock_valid (int id, string path) {
#ifndef OS_MINGW
  if (id < 0 || id >= N(shared_lock_fds) || shared_lock_fds[id] < 0) return false;
  struct stat a, b;
  c_string p (path);
  if (fstat (shared_lock_fds[id], &a) != 0) return false;
  if (stat ((char*) p, &b) != 0) return false;
  return a.st_dev == b.st_dev && a.st_ino == b.st_ino;
#else
  (void) id; (void) path;
  return false;
#endif
}

// Make the file again and lock it, in place of the lock @id: 1 if it worked
// (nobody had it), 0 if the lock is lost (another process holds a lock there,
// then that one is the host; or the file can not be made: no such folder, not
// allowed; or the file at the path is another one than the file of the lock),
// -1 for any other error (no descriptor left, no room: nothing is known; the
// caller tries again a few times, and then takes it for lost). The lock @id
// is still the old one when it is not 1.
int
shared_lock_renew (int id, string path) {
#ifndef OS_MINGW
  if (id < 0 || id >= N(shared_lock_fds) || shared_lock_fds[id] < 0) return 0;
  c_string p (path);
  // (when something other is wrong: is the file at the path another one?)
  struct stat a, b;
  bool other= (fstat (shared_lock_fds[id], &a) == 0 &&
               stat ((char*) p, &b) == 0 &&
               (a.st_dev != b.st_dev || a.st_ino != b.st_ino));
  int fd= open ((char*) p, O_RDWR | O_CREAT, 0600);
  if (fd < 0) {
    if (errno == ENOENT || errno == ENOTDIR || errno == EACCES) return 0;
    return other? 0: -1;
  }
  int fl= fcntl (fd, F_GETFD, 0);
  if (fl >= 0) fcntl (fd, F_SETFD, fl | FD_CLOEXEC);
  if (flock (fd, LOCK_EX | LOCK_NB) != 0) {
    int e= errno;
    ::close (fd);
    return (e == EWOULDBLOCK || other)? 0: -1;
  }
  ::close (shared_lock_fds[id]);
  shared_lock_fds[id]= fd;
  return 1;
#else
  (void) id; (void) path;
  return -1;
#endif
}

// (so that the cleaners of /tmp which go by age leave the files alone)
void
shared_touch (string path) {
#ifndef OS_MINGW
  c_string p (path);
  utimes ((char*) p, NULL);
#else
  (void) path;
#endif
}

#endif // __EMSCRIPTEN__
