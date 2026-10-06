
/******************************************************************************
* MODULE     : pipe_link.cpp
* DESCRIPTION: TeXmacs links by pipes
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "basic.hpp"

#if !(defined (QTTEXMACS) && (defined (OS_MINGW) || defined (QTPIPES)))

#include "tm_link.hpp"
#include "socket_notifier.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"
#include "analyze.hpp"
#include <stdio.h>
#include <string.h>
#ifndef OS_MINGW
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <errno.h>
#endif
#if !defined(__APPLE__) && !defined(__FreeBSD__)
#include <malloc.h>
#endif

hashset<pointer> pipe_link_set;
void pipe_callback (void *obj, void *info);
extern char **environ;
void close_all_cmdlines ();
void close_all_requests ();
void process_all_cmdlines ();
void process_all_requests ();

#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define IN 0
#define OUT 1
#define TERMCHAR '\1'

/******************************************************************************
* The pipe_link class
******************************************************************************/

struct pipe_link_rep: tm_link_rep {
  string cmd;           // command for launching the pipe
  int    pid;           // process identifier of the child
  int    pp_in [2];     // for data going to the child
  int    pp_out[2];     // for data coming from the child
  int    pp_err[2];     // for error messages coming from the child
  int    in;            // file descriptor for data going to the child
  int    out;           // file descriptor for data coming from the child
  int    err;           // file descriptor for errors coming from the child

  string outbuf;        // pending output from plugin
  string errbuf;        // pending errors from plugin

  socket_notifier snout, snerr;
  
public:
  pipe_link_rep (string cmd);
  ~pipe_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    feed (int channel);
  void    close_fds ();
};

pipe_link_rep::pipe_link_rep (string cmd2): cmd (cmd2) {
  pipe_link_set->insert ((pointer) this);
  in     = pp_in [0]= pp_in [1]= -1;
  out    = pp_out[0]= pp_out[1]= -1;
  err    = pp_err[0]= pp_err[1]= -1;
  outbuf = "";
  errbuf = "";
  alive  = false;
  pid    = -1;  // no process yet: terminate_child does nothing
}

pipe_link_rep::~pipe_link_rep () {
  stop ();
  pipe_link_set->remove ((pointer) this);
}

tm_link
make_pipe_link (string cmd) {
  return tm_new<pipe_link_rep> (cmd);
}

#ifndef OS_MINGW
static void
terminate_child (int pid) {
  // no process (fork failed, or none was made): killpg (-1, ...) fails, and
  // kill (-1, SIGKILL) below would kill every process of the user
  if (pid <= 0) return;
  // Ask the process group of the child to terminate, give it a short
  // time to do so, kill it otherwise, and reap the child
  if (-1 != killpg (pid, SIGTERM)) {
    for (int i=0; i<50; i++) {
      if (waitpid (pid, NULL, WNOHANG) != 0) {
        killpg (pid, SIGKILL);
        return;
      }
      usleep (10000);
    }
    killpg (pid, SIGKILL);
  }
  else kill (pid, SIGKILL);
  waitpid (pid, NULL, 0);
}
#endif

void
pipe_link_rep::close_fds () {
#ifndef OS_MINGW
  if (in  != -1) { close (in ); in = -1; }
  if (out != -1) { close (out); out= -1; }
  if (err != -1) { close (err); err= -1; }
#endif
}

void
close_all_pipes () {
#ifndef OS_MINGW
  iterator<pointer> it= iterate (pipe_link_set);
  while (it->busy()) {
    pipe_link_rep* con= (pipe_link_rep*) it->next();
    if (con->alive) {
      terminate_child (con->pid);
      con->alive= false;
      remove_notifier (con->snout);
      remove_notifier (con->snerr);
      con->close_fds ();
    }
  }
#endif
  close_all_cmdlines ();
  close_all_requests ();
}

void
process_all_pipes () {
  iterator<pointer> it= iterate (pipe_link_set);
  while (it->busy()) {
    pipe_link_rep* con= (pipe_link_rep*) it->next();
    if (con->alive) con->apply_command ();
  }
  process_all_cmdlines ();
  process_all_requests ();
}

/******************************************************************************
* Routines for pipe_links
******************************************************************************/

#ifndef OS_MINGW
// NOTE: the same as in cmdline_link.cpp, which is compiled with it without Qt
static void
execute_shell (string s) {
  c_string _s (s);
  char *argv[4];
  argv[0] = const_cast<char*> ("sh");
  argv[1] = const_cast<char*> ("-c");
  argv[2] = _s;
  argv[3] = NULL;
  execve ("/bin/sh", argv, environ);
}
#endif

#ifndef OS_MINGW
static bool
program_found (string cmd) {
  // Can the program which @cmd runs be started? As for Qt pipes, which
  // start the program themselves, a program which is not found is an error;
  // a command which is not a plain call of a program is left to the shell
  int i= 0, n= N(cmd);
  while (i < n && (cmd[i] == ' ' || cmd[i] == '\t')) i++;
  int start= i;
  while (i < n && cmd[i] != ' ' && cmd[i] != '\t') i++;
  string prog= cmd (start, i);
  if (prog == "") return true;
  for (int j= 0; j < N(prog); j++)
    if (!is_alpha (prog[j]) && !is_digit (prog[j]) &&
        prog[j] != '.' && prog[j] != '_' && prog[j] != '-' &&
        prog[j] != '+' && prog[j] != '/')
      return true;
  if (prog == "exec" || prog == "cd" || prog == "eval" || prog == "." ||
      prog == "command" || prog == "set" || prog == "export" ||
      prog == "trap" || prog == "ulimit" || prog == "umask")
    return true;
  if (search_forwards ("/", prog) >= 0) {
    c_string p (prog);
    return access (p, X_OK) == 0;
  }
  string path= get_env ("PATH");
  int k= 0;
  while (k <= N(path)) {
    int e= search_forwards (":", k, path);
    if (e < 0) e= N(path);
    string dir= path (k, e);
    c_string p ((dir == ""? string ("."): dir) * "/" * prog);
    if (access (p, X_OK) == 0) return true;
    k= e + 1;
  }
  return false;
}
#endif

string
pipe_link_rep::start () {
#ifndef OS_MINGW
  if (alive) return "busy";
  if (DEBUG_AUTO) debug_io << "Launching '" << cmd << "'\n";
#ifdef __EMSCRIPTEN__
  // a page has no processes: fork fails, and the pipes, taken for those of
  // a live program, were read again and again (the page froze)
  return "Error: the programs of the plugins do not run in the browser";
#endif
  if (!program_found (cmd)) {
    if (DEBUG_IO) debug_io << "Error: cannot start '" << cmd << "'\n";
    return "Error: cannot start application";
  }

  int e1= pipe (pp_in ); (void) e1;
  int e2= pipe (pp_out); (void) e2;
  int e3= pipe (pp_err); (void) e3;
  pid= fork ();
  if (pid < 0) { // no process: not a live program
    int* fds[3]= { pp_in, pp_out, pp_err };
    for (int i= 0; i < 3; i++)
      for (int j= 0; j < 2; j++)
        if (fds[i][j] >= 0) { close (fds[i][j]); fds[i][j]= -1; }
    return "Error: cannot start '" * cmd * "'";
  }
  if (pid==0) { // the child
    setsid();
    close (pp_in  [OUT]);
    close (pp_out [IN ]);
    close (pp_err [IN ]);
    dup2  (pp_in  [IN ], STDIN );
    close (pp_in  [IN ]);
    dup2  (pp_out [OUT], STDOUT);
    close (pp_out [OUT]);
    dup2  (pp_err [OUT], STDERR);
    close (pp_err [OUT]);

    execute_shell (cmd);
    exit (127);
    // exit (system (cmd) != 0);
  }
  else { // the main process
    in = pp_in  [OUT];
    close (pp_in [IN]);
    out= pp_out [IN ];
    close (pp_out [OUT]);
    err= pp_err [IN ];
    close (pp_err [OUT]);

    alive= true;
    snout = socket_notifier (out, &pipe_callback, this, NULL);
    snerr = socket_notifier (err, &pipe_callback, this, NULL);
    add_notifier (snout);
    add_notifier (snerr);
    
    if (/* !banner */ true) return "ok";
    else {
      int r;
      char outbuf[1024];
      r= ::read (out, outbuf, 1024);
      if (r == 1 && outbuf[0] == TERMCHAR) return "ok";
      alive= false;
      terminate_child (pid);
      remove_notifier (snout);
      remove_notifier (snerr);
      close_fds ();
      if (r == -1) return "Error: the application does not reply";
      else
        return "Error: the application did not send its usual startup banner";
    }
  }
#else
  return "Error: pipes not implemented";
#endif
}

#ifndef OS_MINGW
static string
debug_io_string (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c == DATA_BEGIN) r << "[BEGIN]";
    else if (c == DATA_END) r << "[END]";
    else if (c == DATA_ABORT) r << "[ABORT]";
    else if (c == DATA_COMMAND) r << "[COMMAND]";
    else if (c == DATA_ESCAPE) r << "[ESCAPE]";
    else r << s[i];
  }
  return r;
}
#endif

void
pipe_link_rep::write (string s, int channel) {
#ifndef OS_MINGW
  if ((!alive) || (channel != LINK_IN)) return;
  if (DEBUG_IO) debug_io << "[INPUT]" << debug_io_string (s);
  c_string _s (s);
  int err= ::write (in, _s, N(s));
  (void) err;
#endif
}

void
pipe_link_rep::feed (int channel) {
#ifndef OS_MINGW
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;
  int r;
  char tempout[1024];
  if (channel == LINK_OUT) r = ::read (out, tempout, 1024);
  else r = ::read (err, tempout, 1024);
  // NOTE: an interrupted read is tried again later; after any other
  // failure, the pipe is closed as at its end, since it would otherwise
  // be reported readable again and again
  if (r == -1 && (errno == EINTR || errno == EAGAIN)) return;
  if (r == -1) io_error << "Read failed for '" << cmd << "'\n";
  if (r <= 0) {
    terminate_child (pid);
    alive= false;
    remove_notifier (snout);      
    remove_notifier (snerr);      
    close_fds ();
  }
  else {
    if (DEBUG_IO) debug_io << debug_io_string (string (tempout, r));
    if (channel == LINK_OUT) outbuf << string (tempout, r);
    else errbuf << string (tempout, r);
  }
#endif
}

string&
pipe_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
pipe_link_rep::read (int channel) {
  // NOTE: as for Qt pipes, what is pending is read first, so that the
  // output arrives without the event loop (synchronous evaluations,
  // interruptions)
  if (alive && outbuf == "" && errbuf == "") listen (0);
  if (channel == LINK_OUT) {
    string r= outbuf;
    outbuf= "";
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= errbuf;
    errbuf= "";
    return r;
  }
  else return string("");
}

void
pipe_link_rep::listen (int msecs) {
#ifdef OS_MINGW
  using namespace wsoc;
#endif
  if (!alive) return;
  time_t wait_until= texmacs_time () + msecs;
  while (alive && (outbuf == "") && (errbuf == "")) {
    fd_set rfds;
    FD_ZERO (&rfds);
    FD_SET (out, &rfds);
    FD_SET (err, &rfds);
    // NOTE: only the time which is left is waited for, and listen (0)
    // checks the pipes once (it is called by each read)
    time_t left= max ((time_t) 0, wait_until - texmacs_time ());
    struct timeval tv;
    tv.tv_sec  = left / 1000;
    tv.tv_usec = 1000 * (left % 1000);
    int nr= select (max (out, err) + 1, &rfds, NULL, NULL, &tv);
    if (nr > 0 && FD_ISSET (out, &rfds)) feed (LINK_OUT);
    if (alive && nr > 0 && FD_ISSET (err, &rfds)) feed (LINK_ERR);
    if (msecs == 0 || texmacs_time () - wait_until >= 0) break;
  }
}

void
pipe_link_rep::interrupt () {
#ifndef OS_MINGW
  if (!alive) return;
  killpg (pid, SIGINT);
#endif
}

void
pipe_link_rep::stop () {
#ifndef OS_MINGW
  if (!alive) return;
  terminate_child (pid);
  alive= false;

  remove_notifier (snout);
  remove_notifier (snerr);
  close_fds ();
#endif
}

/******************************************************************************
* Call back for new information on pipe
******************************************************************************/

void pipe_callback (void *obj, void *info) {
#ifndef OS_MINGW
  (void) info;
  pipe_link_rep* con= (pipe_link_rep*) obj;  
  bool busy= true;
  bool news= false;
  while (busy && con->alive) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= max (con->err, con->out) + 1;
    FD_SET (con->out, &rfds);
    FD_SET (con->err, &rfds);
  
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    select (max_fd, &rfds, NULL, NULL, &tv);

    busy= false;
    if (con->alive && FD_ISSET (con->out, &rfds)) {
      //cout << "pipe_callback OUT" << LF;
      con->feed (LINK_OUT);
      busy= news= true;
    }
    if (con->alive && FD_ISSET (con->err, &rfds)) {
      //cout << "pipe_callback ERR" << LF;
      con->feed (LINK_ERR);
      busy= news= true;
    }
  }
  /* FIXME: find out the appropriate place to call the callback
     Currently, the callback is called in tm_server_rep::interpose_handler */
  if (!is_nil (con->feed_cmd) && news) {
    //cout << "pipe_callback APPLY" << LF;
    if (!is_nil (con->feed_cmd))
      con->feed_cmd->apply (); // call the data processor
  }
#endif
}

#endif // !(defined (QTTEXMACS) && defined (OS_MINGW))
