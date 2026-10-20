
/******************************************************************************
* MODULE     : sys_utils.cpp
* DESCRIPTION: file handling
* COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven, Denis Raux
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// winsock's select takes at most FD_SETSIZE sockets, 64 unless the size is
// set before its first include
#if defined(_WIN32) && !defined(FD_SETSIZE)
#define FD_SETSIZE 1024
#endif

#include "sys_utils.hpp"
#include "file.hpp"
#include "tree.hpp"
#include "parse_string.hpp"
#include <cstring>

#ifndef OS_MINGW
#include <poll.h>
#include <stdlib.h>
#else
#include <winsock2.h>
#endif

int script_status = 1;

/******************************************************************************
* System functions
******************************************************************************/

int
system (string s, string& result, string& error) {
#if defined (OS_MINGW64)
  int r= windows_system (s, result, error);
#elif defined (OS_MINGW)
  int r= qt_system (s, result, error);
#elif defined (OS_ANDROID)
  int r= qt_system (s, result, error);
#else 
  int r= unix_system (s, result, error);
#endif
  return r;
}

int
system (string s, string& result) {
#if defined (OS_MINGW64)
  int r= windows_system (s, result); 
#elif defined (OS_MINGW)
  int r= qt_system (s, result);
#elif defined (OS_ANDROID)
  int r= qt_system (s, result);
#else
  int r= unix_system (s, result);
#endif
  return r;
}

int
system (string s) {
  if (DEBUG_STD) debug_shell << s << "\n";
  if (DEBUG_VERBOSE) {
    string result;
    int r= system (s, result);
    debug_shell << result;
    return r;
  }
  else {
#if defined (OS_MINGW64)
    return windows_system (s);
#elif defined (OS_MINGW)
    return qt_system (s);
#elif defined (OS_ANDROID)
    return qt_system (s);
#else
    return unix_system (s);
#endif
  }
}

string
eval_system (string s) {
  string result;
  (void) system (s, result);
  return result;
}

string
var_eval_system (string s) {
  string r= eval_system (s);
  while ((N(r)>0) && (r[N(r)-1]=='\n' || r[N(r)-1]=='\r')) r= r (0,N(r)-1);
  return r;
}

string
get_env (string var) {
  string ret;
  bool has_value = texmacs_getenv(var, ret);
  if (!has_value) {
    if (var == "PWD") return get_env ("HOME");
    return "";
  }
  return ret;
}

void
set_env (string var, string with) {
  texmacs_setenv(var, with);
}

url
get_texmacs_path () {
  string tmpath= get_env ("TEXMACS_PATH");
    //FIXME: Why is this?
  while ((N(tmpath)>0) && (tmpath [N(tmpath) - 1] == '/'))
    tmpath= tmpath (0, N(tmpath)-1);
  return url_system (tmpath);
}

url
get_texmacs_home_path () {
  url path= get_env ("TEXMACS_HOME_PATH");
  if (path == "")
    path= url_system ("$HOME/.TeXmacs_Upsilon");
  return path;
}

array<string>
evaluate_system (array<string> arg,
		 array<int> fd_in, array<string> in,
		 array<int> fd_out) {
  array<string> out (N(fd_out));
  array<string*> ptr (N(fd_out));
  for (int i= 0; i < N(fd_out); i++) ptr[i]= &(out[i]);
#ifdef OS_MINGW
  int ret= mingw_system (arg, fd_in, in, fd_out, ptr);
#elif defined (OS_ANDROID)
  int ret = -1;
  //int ret= qt_system (arg, fd_in, in, fd_out, ptr);
#else
  int ret= unix_system (arg, fd_in, in, fd_out, ptr);
#endif
  // the inputs sent to the command (passphrases of GnuPG, of the keychain)
  // are overwritten once sent, before they are released: they are the
  // copies made for this call by the glue (evaluate-system), which nobody
  // reads after it (the C++ callers give no inputs)
  for (int i= 0; i < N(in); i++) secure_wipe (in[i]);
  return append (as_string (ret), out);
}

void
secure_wipe (string s) {
  // (through a volatile pointer, so that the writes are not left out as
  // writes to memory which is released afterwards)
  volatile char* p= (volatile char*) &(s[0]);
  for (int i= 0; i < N(s); i++) p[i]= 0;
}


string 
get_printing_default () {
#if defined (OS_MINGW)
  url embedded ("$TEXMACS_PATH/bin/SumatraPDF.exe");
  if (exists (embedded))
    return sys_concretize (embedded) * " -print-dialog -exit-when-done";
  else return "";
#else
  return "lp";
#endif
}

class PrintCap {
private:
  string prt_cmd;
  bool blank;
public:	
  PrintCap (): blank (true) {};
  friend string get_printing_cmd ();
  friend void set_printing_cmd (string);
} print_cap;

string
get_printing_cmd () {
  if (print_cap.blank) {
    print_cap.prt_cmd= get_printing_default ();
    print_cap.blank= false;
  }
  return print_cap.prt_cmd;
}

void
set_printing_cmd (string cmd) {
  print_cap.prt_cmd= cmd;
  print_cap.blank= false;
}

bool
has_printing_cmd () {
  static bool has= get_printing_cmd () != "";
  return has;
}

int
tm_poll (struct tm_pollfd* fds, int nfds, int timeout_ms) {
#ifndef OS_MINGW
  // (on the stack for the usual few descriptors, else on the heap)
  struct pollfd few[64];
  struct pollfd* pfds= few;
  if (nfds > 64) {
    pfds= (struct pollfd*) malloc (nfds * sizeof (struct pollfd));
    if (pfds == NULL) { pfds= few; nfds= 64; }
  }
  for (int i= 0; i < nfds; i++) {
    pfds[i].fd= fds[i].fd;
    pfds[i].events= 0;
    if (fds[i].events & TM_POLL_READ)  pfds[i].events |= POLLIN;
    if (fds[i].events & TM_POLL_WRITE) pfds[i].events |= POLLOUT;
    pfds[i].revents= 0;
  }
  int ret= poll (pfds, nfds, timeout_ms);
  for (int i= 0; i < nfds; i++) {
    fds[i].revents= 0;
    if (pfds[i].revents & POLLIN)
      fds[i].revents |= TM_POLL_READ;
    if (pfds[i].revents & POLLOUT)
      fds[i].revents |= TM_POLL_WRITE;
    if (pfds[i].revents & (POLLERR | POLLHUP | POLLNVAL))
      fds[i].revents |= TM_POLL_ERROR;
  }
  if (pfds != few) free (pfds);
  return ret;
#else
  // Windows select() only supports sockets, not pipes or file handles
  fd_set rfds, wfds, efds;
  FD_ZERO (&rfds);
  FD_ZERO (&wfds);
  FD_ZERO (&efds);
  for (int i= 0; i < nfds; i++) {
    if (fds[i].events & TM_POLL_READ)  FD_SET (fds[i].fd, &rfds);
    if (fds[i].events & TM_POLL_WRITE) FD_SET (fds[i].fd, &wfds);
    FD_SET (fds[i].fd, &efds);
    fds[i].revents= 0;
  }
  struct timeval tv;
  struct timeval* tvp= NULL;
  if (timeout_ms >= 0) {
    tv.tv_sec= timeout_ms / 1000;
    tv.tv_usec= (timeout_ms % 1000) * 1000;
    tvp= &tv;
  }
  int ret= select (0, &rfds, &wfds, &efds, tvp);
  if (ret > 0) {
    int count= 0;
    for (int i= 0; i < nfds; i++) {
      if (FD_ISSET (fds[i].fd, &rfds)) fds[i].revents |= TM_POLL_READ;
      if (FD_ISSET (fds[i].fd, &wfds)) fds[i].revents |= TM_POLL_WRITE;
      if (FD_ISSET (fds[i].fd, &efds)) fds[i].revents |= TM_POLL_ERROR;
      if (fds[i].revents) count++;
    }
    return count;
  }
  return ret;
#endif
}

/******************************************************************************
* Asynchroneous execution of commands
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#if defined (OS_MINGW) || defined (OS_WIN)
#include <io.h>
#else
#include <unistd.h>
#endif
#include "scheme.hpp"

struct async_handle {
  FILE*  fp;
  bool   done;
  char*  buf;
  int    len;
  int    cap;
  pthread_mutex_t lock; // buf, len and the pointers below, which the
                        // reading thread uses
  bool   cancelled; // no longer wanted (async_eval_cancel): it writes nothing
  // either
  object* call_back; // (a pointer: a handle of a request link makes no
                     // object, and needs no Scheme)
  // or
  int*    status;
  string* outbuf;
  string* errbuf;
  bool*   kill;
  async_handle (FILE* fp2, object call_back2):
    fp (fp2), done (false),
    buf ((char*) malloc (4096)), len (0), cap (4096),
    cancelled (false), call_back (tm_new<object> (call_back2)),
    status (NULL), outbuf (NULL), errbuf (NULL), kill (NULL) {
      pthread_mutex_init (&lock, NULL); }
  async_handle (FILE* fp2, int& st, string& out,
		string& err, bool& k):
    fp (fp2), done (false),
    buf ((char*) malloc (4096)), len (0), cap (4096), cancelled (false),
    call_back (NULL), status (&st), outbuf (&out), errbuf (&err), kill (&k) {
      pthread_mutex_init (&lock, NULL); }
  ~async_handle () {
    if (call_back != NULL) tm_delete<object> (call_back);
    pthread_mutex_destroy (&lock); }
};

array<async_handle*> async_busy;

// a handle which is no longer wanted: cancelled, or its request killed
// (called with its lock held: async_eval_cancel changes kill)
static bool
async_unwanted (async_handle* handle) {
  return handle->cancelled || (handle->kill != NULL && *(handle->kill));
}

void*
async_read_output (void* arg) {
  typedef FILE* FILEp;
  typedef char* charp;
  async_handle* handle= (async_handle*) arg;
  FILEp& fp  = handle->fp;
  bool&  done= handle->done;
  charp& buf = handle->buf;
  int&   len = handle->len;
  int&   cap = handle->cap;
  pthread_mutex_lock (&handle->lock);
  bool unwanted= async_unwanted (handle);
  pthread_mutex_unlock (&handle->lock);
  if (unwanted) {
    pclose (fp);
    fp  = NULL;
    done= true;
    return NULL;
  }
  // what is there is read as it comes (read, not fread, which waits for
  // a whole buffer): a request link shows the output so far (a streamed
  // answer, see async_eval_pending); a handle which is no longer wanted
  // stops reading
  while (true) {
    char buffer[4096];
    int bytes_read;
    bytes_read= read (fileno (fp), buffer, sizeof (buffer));
    if (bytes_read <= 0) break;
    pthread_mutex_lock (&handle->lock);
    if (async_unwanted (handle)) {
      pthread_mutex_unlock (&handle->lock);
      break;
    }
    while (bytes_read + len > cap) {
      char* buf2= (char*) malloc (2 * cap);
      for (int i=0; i<len; i++) buf2[i]= buf[i];
      free ((void*) buf);
      buf= buf2;
      cap= 2 * cap;
    }
    for (int i=0; i<bytes_read; i++)
      buf[len+i]= buffer[i];
    len += bytes_read;
    pthread_mutex_unlock (&handle->lock);
  }

  pclose (fp);
  fp  = NULL;
  done= true;
  return NULL;
}

bool
async_eval_system (string c, object call_back) {
  string cmd = c;
#if !defined (OS_MINGW)
  cmd = cmd * " 2> /dev/null";
#endif
  int i, n= N(cmd);
  char* cmd_= (char*) malloc (n+1);
  for (i=0; i<n; i++) cmd_[i]= cmd[i];
  cmd_[n]= '\0';

  FILE *fp = popen (cmd_, "r");
  if (!fp) return true;
  async_handle* handle= tm_new<async_handle> (fp, call_back);
  async_busy << handle;

  pthread_t thread;
  pthread_create (&thread, NULL, async_read_output, handle);
  pthread_detach (thread);

  free ((void*) cmd_);
  return false;
}

bool
async_eval_system (string c, int& status, string& outbuf,
		   string& errbuf, bool& kill) {
  string cmd = c;
#if !defined (OS_MINGW)
  cmd = cmd * " 2> /dev/null";
#endif
  int i, n= N(cmd);
  char* cmd_= (char*) malloc (n+1);
  for (i=0; i<n; i++) cmd_[i]= cmd[i];
  cmd_[n]= '\0';

  FILE *fp = popen (cmd_, "r");
  if (!fp) return true;
  async_handle* handle=
    tm_new<async_handle> (fp, status, outbuf, errbuf, kill);
  async_busy << handle;

  pthread_t thread;
  pthread_create (&thread, NULL, async_read_output, handle);
  pthread_detach (thread);

  free ((void*) cmd_);
  return false;
}

/******************************************************************************
* Asynchronous execution of commands without shell
******************************************************************************/

struct async_process {
  int    id;
  object call_back;
#if defined (OS_MINGW) || defined (OS_ANDROID)
  array<string> result;
#else
  unix_process_rep* rep;
#endif
};

static array<async_process*> async_processes;
static int async_process_counter= 0;

int
async_evaluate_system (array<string> arg, string in, object call_back) {
  // Run arg[0] with arguments arg[i], i >= 1, without shell, sending in
  // to its standard input.  When the command terminates, call_back is
  // called with the list (exit-code stdout stderr).
  // Returns an identifier for async_evaluate_cancel, or 0 on failure.
  async_process* p= tm_new<async_process> ();
  p->id= ++async_process_counter;
  p->call_back= call_back;
#if defined (OS_MINGW) || defined (OS_ANDROID)
  array<int> fd_in;
  array<string> str_in;
  if (N(in) > 0) { fd_in << 0; str_in << in; }
  array<int> fd_out;
  fd_out << 1 << 2;
  p->result= evaluate_system (arg, fd_in, str_in, fd_out);
#else
  p->rep= unix_system_start (arg, in);
  if (p->rep == NULL) {
    tm_delete<async_process> (p);
    return 0;
  }
#endif
  async_processes << p;
  return p->id;
}

void
async_evaluate_cancel (int id) {
  // Terminate the command with identifier id; its call back will be
  // called as usual, when it has terminated
#if defined (OS_MINGW) || defined (OS_ANDROID)
  (void) id;
#else
  for (int i=0; i<N(async_processes); i++)
    if (async_processes[i]->id == id)
      unix_system_kill (async_processes[i]->rep);
#endif
}

static void
async_evaluate_pending () {
  array<async_process*> done;
  array<async_process*> busy;
  array<object> results;
  for (int i=0; i<N(async_processes); i++) {
    async_process* p= async_processes[i];
#if defined (OS_MINGW) || defined (OS_ANDROID)
    int ret= as_int (p->result[0]);
    string out= p->result[1], err= p->result[2];
    done << p;
    results << list_object (object (ret), object (out), object (err));
#else
    int ret;
    string out, err;
    if (unix_system_finished (p->rep, ret, out, err)) {
      done << p;
      results << list_object (object (ret), object (out), object (err));
    }
    else busy << p;
#endif
  }
  // NOTE: the call backs might start new processes
  async_processes= busy;
  for (int i=0; i<N(done); i++) {
    call (done[i]->call_back, results[i]);
    tm_delete<async_process> (done[i]);
  }
}

#ifdef __EMSCRIPTEN__
void web_async_pending (); // the requests of the browser (web_files.cpp)
#endif

void
async_eval_pending () {
#ifdef __EMSCRIPTEN__
  web_async_pending ();
#endif
  if (N(async_processes) > 0) async_evaluate_pending ();
  for (int i=0; i<N(async_busy); )
    if (async_busy[i]->done) {
      async_handle* handle= async_busy[i];
      string out (handle->buf, handle->len);
      if (handle->cancelled);
      else if (handle->status == NULL)
	call (*(async_busy[i]->call_back), out);
      else {
	*(handle->status)= 0;
	*(handle->outbuf)= out;
	*(handle->errbuf)= "";
      }
      free (handle->buf);
      tm_delete<async_handle> (handle);
      async_busy= append (range (async_busy, 0, i),
                          range (async_busy, i + 1, N(async_busy)));
    }
    else {
      // the output so far, for a request link (its partial answer)
      async_handle* handle= async_busy[i];
      if (handle->outbuf != NULL && !handle->cancelled &&
          (handle->kill == NULL || !*(handle->kill))) {
        pthread_mutex_lock (&handle->lock);
        if (handle->len > N(*(handle->outbuf)))
          *(handle->outbuf)= string (handle->buf, handle->len);
        pthread_mutex_unlock (&handle->lock);
      }
      i++;
    }
}

/******************************************************************************
* User information
******************************************************************************/

string get_user_login () {
#ifdef OS_MINGW
  return get_env ("USERNAME");
#else
  return unix_get_login ();
#endif
}

string get_user_name () {
#ifdef OS_MINGW
  // the Windows entry point stores the display name (GetUserNameExW) in
  // TEXMACS_DISPLAYNAME (see Plugins/Windows64/windows64_entrypoint.cpp)
  string name= get_env ("TEXMACS_DISPLAYNAME");
  return name == ""? get_env ("USERNAME"): name;
#else // Linux and macOS
  return unix_get_username ();
#endif
}

// The commands of a request link (the curl program, see web_files.cpp)
// which write into outbuf are no longer wanted: an interrupted answer, or
// a link which asks again or is destroyed. They write nothing more into the
// link (its status, its output, its errors), and their output is dropped:
// nothing of an interrupted answer reaches the next one.
void
async_eval_cancel (string* outbuf) {
  for (int i=0; i<N(async_busy); i++) {
    async_handle* handle= async_busy[i];
    if (handle->outbuf != outbuf || handle->cancelled) continue;
    pthread_mutex_lock (&handle->lock);
    handle->cancelled= true;
    handle->status= NULL;
    handle->outbuf= NULL;
    handle->errbuf= NULL;
    handle->kill= NULL;
    pthread_mutex_unlock (&handle->lock);
  }
}
