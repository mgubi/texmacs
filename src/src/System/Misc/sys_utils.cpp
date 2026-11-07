
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
  // a path of the system, as for get_texmacs_path: with the standard syntax
  // the ':' of a Windows drive (C:\...) separates two paths
  string home= get_env ("TEXMACS_HOME_PATH");
  if (home == "") return url_system ("$HOME/.TeXmacs_Upsilon");
  return url_system (home);
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

// The command runs in the shell; a thread reads its output (and, on Unix,
// its errors) as it comes and waits for its end, and the main loop
// (async_eval_pending) hands over the result once the thread is done.
// The thread and the main loop share the buffers, the done flag and the
// process under the lock of the handle; the pointers given by the caller
// (status, outbuf, errbuf, kill) are only used by the main loop.
// On Unix, the command is the leader of its own process group, so that a
// kill reaches the commands which the shell runs (curl...); on Windows, it
// is run by popen: its errors are not read, and a kill does not stop it
// (its result is only dropped). The output is read as it comes: a request
// link shows the output so far (a streamed answer, see async_eval_pending).
// A command of a request link which is no longer wanted (async_eval_cancel)
// is stopped like a killed one, and writes nothing more into the link.

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include "scheme.hpp"

// (popen on Windows, and where processes are not made with posix_spawn)
#if defined (OS_MINGW) || defined (OS_WIN) || defined (OS_ANDROID) || \
    defined (__EMSCRIPTEN__)
#define ASYNC_POPEN
#if defined (OS_MINGW) || defined (OS_WIN)
#include <io.h>
#else
#include <unistd.h>
#include <sys/wait.h>
#endif
#else
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <unistd.h>
#include <sys/wait.h>
extern char **environ;
#endif

#define ASYNC_OUTPUT  0 // call_back (output)
#define ASYNC_FULL    1 // call_back (status, output, errors)
#define ASYNC_POINTER 2 // status, outbuf, errbuf (a request link)

struct async_buffer {
  char* a;
  int   n;
  int   cap;
  async_buffer (): a ((char*) malloc (4096)), n (0), cap (4096) {}
  ~async_buffer () { free ((void*) a); }
  void append (const char* s, int k) {
    while (n + k > cap) {
      cap= 2 * cap;
      a= (char*) realloc ((void*) a, cap);
    }
    memcpy (a + n, s, k);
    n += k; }
};

struct async_handle {
  pthread_mutex_t lock;   // the fields below which the thread changes
  bool         done;      // the thread is done: the result can be taken
  int          exit_code; // of the command (see async_exit_code)
  async_buffer out;       // its standard output
  async_buffer err;       // its standard error (Unix)
#ifdef ASYNC_POPEN
  FILE*        fp;
#else
  pid_t        pid;       // the process (and its group), -1 once waited for
  int          fd_out;
  int          fd_err;
#endif
  bool         killed;    // *kill was seen: the process was stopped
  bool         cancelled; // no longer wanted (async_eval_cancel): the
                          // pointers below are NULL, and it writes nothing
  int          mode;
  // either
  object*      call_back; // (a pointer: the variant of a request link makes
                          // no object, and needs no Scheme)
  // or
  int*         status;
  string*      outbuf;
  string*      errbuf;
  bool*        kill;
  async_handle (int mode2, object call_back2):
    done (false), exit_code (-1), killed (false), cancelled (false),
    mode (mode2),
    call_back (tm_new<object> (call_back2)),
    status (NULL), outbuf (NULL), errbuf (NULL), kill (NULL) {
      pthread_mutex_init (&lock, NULL); }
  async_handle (int* st, string* out, string* err, bool* k):
    done (false), exit_code (-1), killed (false), cancelled (false),
    mode (ASYNC_POINTER),
    call_back (NULL), status (st), outbuf (out), errbuf (err), kill (k) {
      pthread_mutex_init (&lock, NULL); }
  ~async_handle () {
    if (call_back != NULL) tm_delete<object> (call_back);
    pthread_mutex_destroy (&lock); }
};

array<async_handle*> async_busy;

static void
async_append (async_handle* handle, bool to_err, const char* s, int k) {
  pthread_mutex_lock (&handle->lock);
  if (to_err) handle->err.append (s, k);
  else handle->out.append (s, k);
  pthread_mutex_unlock (&handle->lock);
}

static void
async_finish (async_handle* handle, int code) {
  // the last access of the thread to the handle: the main loop may free it
  // as soon as the lock is released
  pthread_mutex_lock (&handle->lock);
  handle->exit_code= code;
  handle->done= true;
  pthread_mutex_unlock (&handle->lock);
}

#ifdef ASYNC_POPEN

void*
async_read_output (void* arg) {
  // (read, not fread, which waits for a whole buffer; a command which is
  // no longer wanted is no longer read)
  async_handle* handle= (async_handle*) arg;
  while (true) {
    char buffer[4096];
    int bytes_read= read (fileno (handle->fp), buffer, sizeof (buffer));
    if (bytes_read <= 0) break;
    pthread_mutex_lock (&handle->lock);
    bool unwanted= handle->killed || handle->cancelled;
    pthread_mutex_unlock (&handle->lock);
    if (unwanted) break;
    async_append (handle, false, buffer, bytes_read);
  }
  int ret= pclose (handle->fp);
  handle->fp= NULL;
#if defined (OS_MINGW) || defined (OS_WIN)
  int code= ret;   // (the exit code of the command)
#else
  int code= -1;
  if (ret != -1 && WIFEXITED (ret)) code= WEXITSTATUS (ret);
  else if (ret != -1 && WIFSIGNALED (ret)) code= 128 + WTERMSIG (ret);
#endif
  async_finish (handle, code);
  return NULL;
}

static bool
async_launch (string c, async_handle* handle) {
  string cmd = c;
#if !defined (OS_MINGW) && !defined (OS_WIN)
  cmd = cmd * " 2> /dev/null";
#endif
  c_string cmd_ (cmd);
  handle->fp= popen (cmd_, "r");
  return handle->fp == NULL;
}

static void
async_stop (async_handle* handle) {
  (void) handle;
}

#else

void*
async_read_output (void* arg) {
  // reads both outputs until their ends, then waits for the process
  async_handle* handle= (async_handle*) arg;
  struct pollfd fds[2];
  fds[0].fd= handle->fd_out; fds[0].events= POLLIN;
  fds[1].fd= handle->fd_err; fds[1].events= POLLIN;
  int open_fds= 2;
  while (open_fds > 0) {
    for (int i=0; i<2; i++) fds[i].revents= 0;
    int r= poll (fds, 2, -1);
    if (r < 0) {
      if (errno == EINTR) continue;
      break;
    }
    for (int i=0; i<2; i++) {
      if (fds[i].fd < 0 || fds[i].revents == 0) continue;
      char buffer[4096];
      ssize_t bytes_read= read (fds[i].fd, buffer, sizeof (buffer));
      if (bytes_read < 0 && errno == EINTR) continue;
      if (bytes_read <= 0) {
        close (fds[i].fd);
        fds[i].fd= -1;   // (poll ignores it)
        open_fds--;
      }
      else async_append (handle, i == 1, buffer, (int) bytes_read);
    }
  }
  for (int i=0; i<2; i++)
    if (fds[i].fd >= 0) close (fds[i].fd);

  // the process is waited for in two steps, so that the main loop never
  // kills the group of a process which was waited for (its number may be
  // given to another process): first its end, then, with the lock, it is
  // waited for and marked as such
  pid_t pid= handle->pid;
  siginfo_t info;
  while (waitid (P_PID, pid, &info, WEXITED | WNOWAIT) < 0 && errno == EINTR);
  int st= 0, ret;
  pthread_mutex_lock (&handle->lock);
  while ((ret= waitpid (pid, &st, 0)) < 0 && errno == EINTR);
  handle->pid= -1;
  pthread_mutex_unlock (&handle->lock);
  int code= -1;
  if (ret == pid && WIFEXITED (st)) code= WEXITSTATUS (st);
  else if (ret == pid && WIFSIGNALED (st)) code= 128 + WTERMSIG (st);
  async_finish (handle, code);
  return NULL;
}

static bool
async_launch (string cmd, async_handle* handle) {
  // runs 'sh -c cmd' in a new process group; true if it was not launched
  int p_out[2], p_err[2];
  if (pipe (p_out) != 0) return true;
  if (pipe (p_err) != 0) { close (p_out[0]); close (p_out[1]); return true; }
  // (the ends kept here are not inherited by other commands)
  fcntl (p_out[0], F_SETFD, FD_CLOEXEC);
  fcntl (p_err[0], F_SETFD, FD_CLOEXEC);
  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attr;
  posix_spawn_file_actions_init (&actions);
  posix_spawnattr_init (&attr);
  posix_spawn_file_actions_adddup2 (&actions, p_out[1], 1);
  posix_spawn_file_actions_adddup2 (&actions, p_err[1], 2);
  posix_spawn_file_actions_addclose (&actions, p_out[1]);
  posix_spawn_file_actions_addclose (&actions, p_err[1]);
  posix_spawnattr_setflags (&attr, POSIX_SPAWN_SETPGROUP);
  posix_spawnattr_setpgroup (&attr, 0);
  c_string cmd_ (cmd);
  char sh[]= "/bin/sh", dash_c[]= "-c";
  char* argv[]= { sh, dash_c, (char*) cmd_, NULL };
  pid_t pid;
  int r= posix_spawn (&pid, "/bin/sh", &actions, &attr, argv, environ);
  posix_spawn_file_actions_destroy (&actions);
  posix_spawnattr_destroy (&attr);
  close (p_out[1]);
  close (p_err[1]);
  if (r != 0) {
    close (p_out[0]);
    close (p_err[0]);
    return true;
  }
  handle->pid   = pid;
  handle->fd_out= p_out[0];
  handle->fd_err= p_err[0];
  return false;
}

static void
async_stop (async_handle* handle) {
  // (called with the lock: the process was not yet waited for)
  if (handle->pid > 0) kill (-handle->pid, SIGTERM);
}

#endif

static bool
async_start (string cmd, async_handle* handle) {
  // true if the command could not be launched
  if (async_launch (cmd, handle)) {
    tm_delete<async_handle> (handle);
    return true;
  }
  async_busy << handle;
  pthread_t thread;
  if (pthread_create (&thread, NULL, async_read_output, handle) != 0) {
    // (without a thread, it is read here: the main loop waits for it)
    async_read_output (handle);
    return false;
  }
  pthread_detach (thread);
  return false;
}

bool
async_eval_system (string cmd, object call_back) {
  // call_back gets the output of the command (its errors are dropped)
  return async_start (cmd, tm_new<async_handle> (ASYNC_OUTPUT, call_back));
}

bool
async_eval_system_full (string cmd, object call_back) {
  // call_back gets the exit code of the command (128 + the signal number if
  // a signal stopped it, -1 if it could not be waited for), its output and
  // its errors (empty on Windows)
  return async_start (cmd, tm_new<async_handle> (ASYNC_FULL, call_back));
}

bool
async_eval_system (string cmd, int& status, string& outbuf,
		   string& errbuf, bool& kill) {
  // At the end of the command, status is 0 if it succeeded, and negative
  // otherwise (minus its exit code); outbuf and errbuf get its output and
  // its errors. While kill is set, the command is stopped, and once it
  // was stopped, nothing more is written into status, outbuf and errbuf.
  return async_start (cmd, tm_new<async_handle> (&status, &outbuf,
                                                  &errbuf, &kill));
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
  for (int i=0; i<N(async_busy); ) {
    async_handle* handle= async_busy[i];
    pthread_mutex_lock (&handle->lock);
    bool done= handle->done;
    if (!done && !handle->killed && handle->kill != NULL && *(handle->kill)) {
      handle->killed= true;
      async_stop (handle);
    }
    // the output so far, for a request link (its partial answer)
    if (!done && handle->mode == ASYNC_POINTER &&
        !handle->killed && !handle->cancelled &&
        handle->out.n > N(*(handle->outbuf)))
      *(handle->outbuf)= string (handle->out.a, handle->out.n);
    pthread_mutex_unlock (&handle->lock);
    if (!done) { i++; continue; }
    // the thread is done with the handle; it is removed before the call
    // back, which may launch other commands
    async_busy= append (range (async_busy, 0, i),
                        range (async_busy, i + 1, N(async_busy)));
    int    code= handle->exit_code;
    string out (handle->out.a, handle->out.n);
    string err (handle->err.a, handle->err.n);
    if (handle->mode == ASYNC_OUTPUT)
      call (*(handle->call_back), out);
    else if (handle->mode == ASYNC_FULL)
      call (*(handle->call_back), object (code), object (out), object (err));
    else if (handle->killed || handle->cancelled);
      // (the link no longer waits for it)
    else {
      *(handle->status)= (code == 0? 0: (code > 0? -code: -1));
      *(handle->outbuf)= out;
      *(handle->errbuf)= err;
    }
    tm_delete<async_handle> (handle);
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
// a link which asks again or is destroyed. They are stopped (on Unix), they
// write nothing more into the link (its status, its output, its errors),
// and their output is dropped: nothing of an interrupted answer reaches the
// next one.
void
async_eval_cancel (string* outbuf) {
  for (int i=0; i<N(async_busy); i++) {
    async_handle* handle= async_busy[i];
    if (handle->mode != ASYNC_POINTER || handle->outbuf != outbuf) continue;
    pthread_mutex_lock (&handle->lock);
    if (!handle->done && !handle->killed && !handle->cancelled)
      async_stop (handle);
    handle->cancelled= true;
    handle->status= NULL;
    handle->outbuf= NULL;
    handle->errbuf= NULL;
    handle->kill= NULL;
    pthread_mutex_unlock (&handle->lock);
  }
}
