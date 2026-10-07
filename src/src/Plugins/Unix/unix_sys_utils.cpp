
/******************************************************************************
* MODULE     : unix_sys_utils.cpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2009  David MICHEL, 2015  Gregoire LECERF
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "unix_sys_utils.hpp"
#include "file.hpp"
#include "tm_timer.hpp"
#include <stdlib.h>
#include <fcntl.h>
#include <spawn.h>
#include <unistd.h>
#include <string.h>
#include <sys/wait.h>
#include <pthread.h>
#include <pwd.h>
#include <signal.h>
#include <errno.h>
#include <atomic>

int
unix_system (string s) {
  c_string _s (s * " > /dev/null 2>&1");
  int ret= system (_s);
  return ret;
}

int
unix_system (string cmd, string& result) {
  result= "";
  if (cmd == "") return 0;
  url temp= url_temp ();
  string temp_s= escape_sh (concretize (temp));
  // group the command, so that its own redirections take precedence
  c_string _cmd ("{ " * cmd * "\n} > " * temp_s * " 2>&1");
  int ret= system (_cmd);
  bool flag= load_string (temp, result, false);
  remove (temp);
  if (flag) result= "";
  return ret;
}

int
unix_system (string cmd, string& result, string& error) {
  result= ""; error= "";
  if (cmd == "") return 0;
  url temps= url_temp ();
  url tempe= url_temp ();
  string temp_s= escape_sh (concretize (temps));
  string temp_e= escape_sh (concretize (tempe));
  c_string _cmd ("{ " * cmd * "\n} > " * temp_s * " 2> " * temp_e);
  int ret= system (_cmd);
  bool flag= load_string (temps, result, false);
  remove (temps);
  if (flag) result= "";
  flag= load_string (tempe, error, false);
  remove (tempe);
  if (flag) error= "";
  return ret;
}

/******************************************************************************
* Evaluation via specified file descriptors
******************************************************************************/

#if !defined(OS_MINGW) && !defined(X11TEXMACS) && !defined(OS_ANDROID)

extern char **environ;

// exception safe mutex
struct _mutex {
  pthread_mutex_t rep;
  inline _mutex () { pthread_mutex_init (&rep, NULL); }
  inline ~_mutex () { pthread_mutex_destroy (&rep); }
};

struct _mutex_lock {
  pthread_mutex_t* rep;
  inline _mutex_lock (_mutex& m): rep (&(m.rep)) {
    pthread_mutex_lock (rep); }
  inline ~_mutex_lock () {
    pthread_mutex_unlock (rep); }
};

// thread safe malloc and free
static _mutex _ts_memory_lock;

void*
_ts_malloc (int n) {
  _mutex_lock lock (_ts_memory_lock);
  return malloc (n);
}

void
_ts_free (void* a) {
  _mutex_lock lock (_ts_memory_lock);
  free (a);
}

// thread safe strings
struct _ts_string {
  int n, l;
  char* a;

  inline _ts_string (): n (0), l (0), a (NULL) {}
  inline ~_ts_string () { if (l != 0) _ts_free ((void*) a); }

  void resize (int m) {
    if (m <= n) return;
    int new_l= max (2 * n, m);
    char* new_a= (char*) _ts_malloc (new_l * sizeof (char));
    memcpy (new_a, a, n);
    _ts_free ((void*) a);
    a= new_a;
    l= new_l; }

  void append (char* b, int m) {
    resize (m + n);
    memcpy (a + n, b, m);
    n += m; }

  void copy (char* b, int m) {
    resize (m);
    memcpy (a, b, m);
    n= m; }
};

// pipe
struct _pipe_t {
  int rep[2];
  int st;
  inline _pipe_t () {
    st= pipe (rep);
    int fl= fcntl (rep[0], F_GETFL);
    fl = fl & (~(int) O_NONBLOCK);
    fcntl (rep[0], F_SETFL, fl);
    fl= fcntl (rep[1], F_GETFL);
    fl= fl & (~(int) O_NONBLOCK);
    fcntl (rep[1], F_SETFL, fl); }
  inline ~_pipe_t () {
    if (rep[0] >= 0) close (rep[0]);
    if (rep[1] >= 0) close (rep[1]); }
  // the end i was closed elsewhere and should not be closed again
  inline void release (int i) { rep[i]= -1; }
  inline int in () const { return rep[0]; }
  inline int out () const { return rep[1]; }
  inline int status () const { return st; }
};

// asynchronous channel between spawn process
struct _channel {
  int fd;
  _ts_string data;
  _mutex data_lock;  // for reading the data before the thread has finished
  int buffer_size;
  array<char> buffer;
  int status;
  std::atomic<bool> finished;
  std::atomic<bool> closed;
  _channel () : status (0), finished (false), closed (false) {}
  string get_data () {
    _mutex_lock lock (data_lock);
    return string (data.a, data.n); }
  void _init_in (int fd2, string data2, int chunk_size) {
    fd= fd2;
    data.copy (&data2[0], N(data2));
    buffer_size= chunk_size; }
  void _init_out (int fd2, int buffer_size2) {
    fd= fd2;
    buffer_size= buffer_size2;
    buffer= array<char> (buffer_size2); }
};

// data read from spawn process
static void*
_background_read_task (void* channel_as_void_ptr) {
  _channel* c= (_channel*) channel_as_void_ptr;
  int fd= c->fd;
  int n= c->buffer_size;
  char* b= A (c->buffer);
  int m;
  do {
    m= read (fd, b, n);
    // cout << "read " << m << " bytes from " << fd << "\n";
    if (m < 0 && errno == EINTR) { m= 1; continue; }
    if (m > 0) { _mutex_lock lock (c->data_lock); c->data.append (b, m); }
    if (m == 0) { if (close (fd) != 0) c->status= -1; }
  } while (m > 0);
  if (m < 0) close (fd);
  c->closed= true;
  c->finished= true;
  return (void*) NULL;
}

// data written to spawn process
static void*
_background_write_task (void* channel_as_void_ptr) {
  _channel* c= (_channel*) channel_as_void_ptr;
  int fd= c->fd;
  const char* d= c->data.a;
  int n= c->buffer_size;
  int t= (c->data).n, k= 0, o= 0;
  if (t == 0) {
    // NOTE: the process should see the end of its (empty) input
    if (close (fd) != 0) c->status= -1;
    c->closed= true;
    c->finished= true;
    return (void*) NULL; }
  if (n == 0) { c->status= -1; c->finished= true; return (void*) NULL; }
  do {
    int m= min (n, t - k);
    // cout << "writting " << m << " bytes / " << t-k << "\n";
    o= write (fd, (void*) (d + k), m);
    // cout << "written " << o << " bytes to " << fd << "\n";
    if (o < 0 && errno == EINTR) { o= 1; continue; }
    if (o > 0) k += o;
    if (o < 0) { close (fd); c->status= -1; c->closed= true; }
    if (k == t) { if (close (fd) != 0) c->status= -1; c->closed= true; }
  } while (o > 0 && k < t);
  c->finished= true;
  return (void*) NULL;
}

// exception safe file actions
struct _file_actions_t {
  posix_spawn_file_actions_t rep;
  int st;
  inline _file_actions_t () { 
    st= posix_spawn_file_actions_init (&rep); }
  inline ~_file_actions_t () {
    posix_spawn_file_actions_destroy (&rep); }
  inline int status () const { return st; }
};

// exception safe spawn attributes
struct _spawnattr_t {
  posix_spawnattr_t rep;
  int st;
  inline _spawnattr_t () { st= posix_spawnattr_init (&rep); }
  inline ~_spawnattr_t () { posix_spawnattr_destroy (&rep); }
  inline int status () const { return st; }
};

static bool
_new_session (_spawnattr_t& attr) {
  // run the command in a new session (or at least process group), so that
  // it can be killed together with the processes that it starts; without
  // controlling terminal, it cannot be stopped by prompts (SIGTTIN)
  if (attr.status () != 0) return false;
#ifdef POSIX_SPAWN_SETSID
  return posix_spawnattr_setflags (&attr.rep, POSIX_SPAWN_SETSID) == 0;
#else
  return posix_spawnattr_setflags (&attr.rep, POSIX_SPAWN_SETPGROUP) == 0 &&
         posix_spawnattr_setpgroup (&attr.rep, 0) == 0;
#endif
}

// time during which the output of a terminated command is still collected
// (processes which it started in the background may hold its pipes)
#define _LINGER_TIME 2000

static bool
_wait_finished (_channel* c, time_t start) {
  while (!c->finished && texmacs_time () - start < _LINGER_TIME)
    usleep (1000);
  return c->finished;
}

// Texmacs warning for long spawn commands
static void
_unix_system_warn (pid_t pid, string which, string msg) {
  (void) which;
  io_warning << "unix_system, pid " << pid
	     << ", warning: " << msg << "\n";
}

int
unix_system (array<string> arg,
	     array<int> fd_in, array<string> str_in,
	     array<int> fd_out, array<string*> str_out) {
  // Run command arg[0] with arguments arg[i], i >= 1.
  // str_in[i] is sent to the file descriptor fd_in[i].
  // str_out[i] is filled from the file descriptor fd_out[i].
  // If str_in[i] is -1 then $$i automatically replaced by a valid
  // file descriptor in arg.
  if (N(arg) == 0) return 0;
  string which= recompose (arg, " ");
  int n_in= N(fd_in), n_out= N(fd_out);
  ASSERT(N(str_in)  == n_in, "size mismatch");
  ASSERT(N(str_out) == n_out, "size mismatch");
  array<_pipe_t> pp_in (n_in), pp_out (n_out);
  _file_actions_t file_actions;
  for (int i= 0; i < n_in; i++) {
    if (posix_spawn_file_actions_addclose
	(&file_actions.rep, pp_in[i].out ()) != 0) return -1;
    if (fd_in[i] >= 0) {
      if (posix_spawn_file_actions_adddup2
	  (&file_actions.rep, pp_in[i].in (), fd_in[i]) != 0) return -1;
      if (posix_spawn_file_actions_addclose
	  (&file_actions.rep, pp_in[i].in ()) != 0) return -1; } }
  for (int i= 0; i < n_out; i++) {
    if (posix_spawn_file_actions_addclose
	(&file_actions.rep, pp_out[i].in ()) != 0) return -1;
    if (posix_spawn_file_actions_adddup2
	(&file_actions.rep, pp_out[i].out (), fd_out[i]) != 0) return -1;
    if (posix_spawn_file_actions_addclose
	(&file_actions.rep, pp_out[i].out ()) != 0) return -1; }
  array<string> arg_= arg;
  for (int j= 0; j < N(arg); j++)
    for (int i= 0; i < n_in; i++)
      if (fd_in[i] < 0)
        arg_[j]= replace (arg_[j], "$$" * as_string (i),
	  	          as_string (pp_in[i].in ()));
  if (DEBUG_IO)
    debug_io << "unix_system, launching: " << arg_ << "\n"; 
  array<char*> _arg;
  for (int j= 0; j < N(arg_); j++)
    _arg << as_charp (arg_[j]);
  _arg << (char*) NULL;
  _spawnattr_t attr;
  if (!_new_session (attr)) {
    for (int j= 0; j < N(arg_); j++) tm_delete_array (_arg[j]);
    return -1;
  }
  pid_t pid;
#ifdef __EMSCRIPTEN__
  // no processes in the browser: the command fails as if not found
  int status= -1; pid= 0;
#else
  int status= posix_spawnp (&pid, _arg[0], &file_actions.rep, &attr.rep,
			    A(_arg), environ);
#endif
  for (int j= 0; j < N(arg_); j++)
    tm_delete_array (_arg[j]);
  if (status != 0) {
    if (DEBUG_IO) debug_io << "unix_system, failed" << "\n";
    return -1;
  }
  if (DEBUG_IO)
    debug_io << "unix_system, succeeded to create pid "
	     << pid << "\n";

  // close useless ports
  for (int i= 0; i < n_in ; i++) { close (pp_in[i].in ()); pp_in[i].release (0); }
  for (int i= 0; i < n_out; i++) { close (pp_out[i].out ()); pp_out[i].release (1); }

  // NOTE: the channels are allocated on the heap, since a thread may
  // outlive this function (see below)

  // write to spawn process
  array<_channel*> channels_in (n_in);
  array<pthread_t> threads_write (n_in);
  for (int i= 0; i < n_in; i++) {
    channels_in[i]= tm_new<_channel> ();
    channels_in[i]->_init_in (pp_in[i].out (), str_in[i], 1 << 12);
    if (pthread_create (&threads_write[i], NULL /* &attr */,
			_background_write_task,
			(void *) channels_in[i]))
      return -1;
  }

  // read from spawn process
  array<_channel*> channels_out (n_out);
  array<pthread_t> threads_read (n_out);
  for (int i= 0; i < n_out; i++) {
    channels_out[i]= tm_new<_channel> ();
    channels_out[i]->_init_out (pp_out[i].in (), 1 << 12);
    if (pthread_create (&threads_read[i], NULL /* &attr */,
			_background_read_task,
			(void *) channels_out[i]))
      return -1;
  }

  int wret;
  time_t last_wait_time= texmacs_time ();
  while ((wret= waitpid (pid, &status, WNOHANG)) == 0) {
    usleep (100);
    if (texmacs_time () - last_wait_time > 5000) {
      last_wait_time= texmacs_time ();
      _unix_system_warn (pid, which, "waiting spawn process");
    }
  }
  if (DEBUG_IO)
    debug_io << "unix_system, pid " << pid << " terminated" << "\n"; 

  // wait for terminating threads
  // NOTE: processes started in the background by the command (e.g. by a
  // hook of git) may keep its pipes open; after a while, we stop waiting
  // for them, and the threads finish on their own, with their channels
  // and file descriptors (which are therefore neither freed nor closed)
  void* exit_status;
  int thread_status= 0;
  time_t exit_time= texmacs_time ();
  for (int i= 0; i < n_in; i++) {
    _channel* c= channels_in[i];
    if (!_wait_finished (c, exit_time)) {
      pthread_detach (threads_write[i]);
      pp_in[i].release (1);
      continue;
    }
    pthread_join (threads_write[i], &exit_status);
    if (c->closed) pp_in[i].release (1);
    if (c->status < 0) thread_status= -1;
    tm_delete<_channel> (c);
  }
  for (int i= 0; i < n_out; i++) {
    _channel* c= channels_out[i];
    if (!_wait_finished (c, exit_time)) {
      if (DEBUG_IO)
        debug_io << "unix_system, pid " << pid
                 << ": output still open, not waiting any longer\n";
      *(str_out[i])= c->get_data ();
      pthread_detach (threads_read[i]);
      pp_out[i].release (0);
      continue;
    }
    pthread_join (threads_read[i], &exit_status);
    if (c->closed) pp_out[i].release (0);
    *(str_out[i])= c->get_data ();
    if (c->status < 0) thread_status= -1;
    tm_delete<_channel> (c);
  }

  if (thread_status < 0) return thread_status;
  if (wret < 0 || WIFEXITED(status) == 0) return -1;
  return WEXITSTATUS(status);
}

/******************************************************************************
* Asynchronous evaluation via standard input, output and error
******************************************************************************/

struct unix_process_rep {
  pid_t     pid;
  int       killed;
  bool      exited;
  int       status;
  bool      has_in, has_out, has_err;
  _channel  in, out, err;
  pthread_t th_in, th_out, th_err;
};

static void
_close_fds (int* fd, int n) {
  for (int i= 0; i < n; i++) close (fd[i]);
}

unix_process_rep*
unix_system_start (array<string> arg, string input) {
  // Start the command arg[0] with arguments arg[i], i >= 1, send input
  // to its standard input and collect its standard output and error.
  // Returns NULL on failure; use unix_system_finished to wait for the end.
  if (N(arg) == 0) return NULL;
  int fd[6];
  if (pipe (fd) != 0) return NULL;
  if (pipe (fd + 2) != 0) { _close_fds (fd, 2); return NULL; }
  if (pipe (fd + 4) != 0) { _close_fds (fd, 4); return NULL; }
  // the ends of the parent should not leak into other child processes
  fcntl (fd[1], F_SETFD, FD_CLOEXEC);
  fcntl (fd[2], F_SETFD, FD_CLOEXEC);
  fcntl (fd[4], F_SETFD, FD_CLOEXEC);
  _file_actions_t file_actions;
  bool ok= file_actions.status () == 0;
  ok= ok && posix_spawn_file_actions_adddup2 (&file_actions.rep, fd[0], 0) == 0;
  ok= ok && posix_spawn_file_actions_adddup2 (&file_actions.rep, fd[3], 1) == 0;
  ok= ok && posix_spawn_file_actions_adddup2 (&file_actions.rep, fd[5], 2) == 0;
  for (int i= 0; i < 6; i++)
    ok= ok && posix_spawn_file_actions_addclose (&file_actions.rep, fd[i]) == 0;
  _spawnattr_t attr;
  ok= ok && _new_session (attr);
#if defined(POSIX_SPAWN_CLOEXEC_DEFAULT)
  // only the standard file descriptors are inherited (macOS)
  short flags= 0;
  ok= ok && posix_spawnattr_getflags (&attr.rep, &flags) == 0;
  ok= ok && posix_spawnattr_setflags (&attr.rep,
                                      flags | POSIX_SPAWN_CLOEXEC_DEFAULT) == 0;
#elif defined(__GLIBC__) && (__GLIBC__ > 2 || __GLIBC_MINOR__ >= 34)
  // only the standard file descriptors are inherited (glibc 2.34)
  ok= ok && posix_spawn_file_actions_addclosefrom_np (&file_actions.rep, 3) == 0;
#endif
  if (!ok) { _close_fds (fd, 6); return NULL; }

  array<char*> _arg;
  for (int j= 0; j < N(arg); j++)
    _arg << as_charp (arg[j]);
  _arg << (char*) NULL;
  pid_t pid;
#ifdef __EMSCRIPTEN__
  // no processes in the browser: the command fails as if not found
  int status= -1; pid= 0;
#else
  int status= posix_spawnp (&pid, _arg[0], &file_actions.rep, &attr.rep,
			    A(_arg), environ);
#endif
  for (int j= 0; j < N(arg); j++)
    tm_delete_array (_arg[j]);
  if (status != 0) {
    if (DEBUG_IO) debug_io << "unix_system_start, failed " << arg << "\n";
    _close_fds (fd, 6);
    return NULL;
  }
  if (DEBUG_IO)
    debug_io << "unix_system_start, pid " << pid << ": " << arg << "\n";
  close (fd[0]); close (fd[3]); close (fd[5]);

  // the threads close the remaining file descriptors when they are done
  unix_process_rep* rep= tm_new<unix_process_rep> ();
  rep->pid= pid;
  rep->killed= 0;
  rep->exited= false;
  rep->status= 0;
  rep->has_in= N(input) > 0;
  rep->out._init_out (fd[2], 1 << 12);
  rep->err._init_out (fd[4], 1 << 12);
  if (rep->has_in) {
    rep->in._init_in (fd[1], input, 1 << 12);
    if (pthread_create (&rep->th_in, NULL, _background_write_task,
			(void*) &(rep->in)))
      { close (fd[1]); rep->has_in= false; }
  }
  else close (fd[1]);
  // NOTE: the output threads are always created
  rep->has_out= pthread_create (&rep->th_out, NULL, _background_read_task,
				(void*) &(rep->out)) == 0;
  if (!rep->has_out) { close (fd[2]); rep->out.finished= true; }
  rep->has_err= pthread_create (&rep->th_err, NULL, _background_read_task,
				(void*) &(rep->err)) == 0;
  if (!rep->has_err) { close (fd[4]); rep->err.finished= true; }
  return rep;
}

bool
unix_system_finished (unix_process_rep* rep,
		      int& ret, string& out, string& err) {
  // Check whether the process has terminated and all its output has been
  // read; if so, then retrieve its exit code and output, and release rep.
  // NOTE: processes started by the command may still hold the output
  // pipes, so we do not wait for the threads before they are finished.
  // NOTE: the process is only reaped at the end, so that its process
  // group still exists, and can be killed (see unix_system_kill)
  if (!rep->exited) {
    siginfo_t info;
    info.si_pid= 0;
    int wret= waitid (P_PID, rep->pid, &info, WEXITED | WNOHANG | WNOWAIT);
    if (wret == 0 && info.si_pid == 0) return false;
    rep->exited= true;
    if (wret != 0 || info.si_code != CLD_EXITED) rep->status= -1;
    else rep->status= info.si_status;
  }
  if ((rep->has_in && !rep->in.finished) ||
      !rep->out.finished || !rep->err.finished)
    return false;
  int status;
  waitpid (rep->pid, &status, 0);
  void* exit_status;
  if (rep->has_in) pthread_join (rep->th_in, &exit_status);
  if (rep->has_out) pthread_join (rep->th_out, &exit_status);
  if (rep->has_err) pthread_join (rep->th_err, &exit_status);
  out= string (rep->out.data.a, rep->out.data.n);
  err= string (rep->err.data.a, rep->err.data.n);
  ret= rep->status;
  if (DEBUG_IO)
    debug_io << "unix_system_finished, pid " << rep->pid
	     << " exited with " << ret << "\n";
  tm_delete<unix_process_rep> (rep);
  return true;
}

void
unix_system_kill (unix_process_rep* rep) {
  // Terminate the process and the processes which it started; they are
  // woken up in case they were stopped, and killed at the second attempt.
  // NOTE: this also works once the process has exited, while processes
  // that it started still hold its pipes: it is only reaped once they
  // are closed, so that its process group cannot have been reused
  rep->killed++;
  kill (-rep->pid, rep->killed > 1 ? SIGKILL : SIGTERM);
  kill (-rep->pid, SIGCONT);
}

#else

int
unix_system (array<string> arg,
	     array<int> fd_in, array<string> str_in,
	     array<int> fd_out, array<string*> str_out) {
  (void) arg; (void) fd_in; (void) str_in; (void) fd_out; (void) str_out;
  FAILED ("unsupported system call");
}

unix_process_rep*
unix_system_start (array<string> arg, string input) {
  (void) arg; (void) input;
  return NULL;
}

bool
unix_system_finished (unix_process_rep* rep,
		      int& ret, string& out, string& err) {
  (void) rep; ret= -1; out= ""; err= "";
  return true;
}

void
unix_system_kill (unix_process_rep* rep) {
  (void) rep;
}

#endif

// getpwuid finds no entry for the user where there is no user database (a
// container, or the browser): the login then comes from USER, if any
string unix_get_login () {
  uid_t uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  if (pwd != NULL && pwd->pw_name != NULL) return string (pwd->pw_name);
  const char* user= getenv ("USER");
  return user != NULL ? string (user) : string ("");
}

string unix_get_username () {
  uid_t uid= getuid ();
  struct passwd* pwd= getpwuid (uid);
  if (pwd == NULL || pwd->pw_gecos == NULL) return unix_get_login ();
  array<string> a= tokenize (string (pwd->pw_gecos), string (","));
  return N(a) > 0 ? a[0] : string ("");
}
