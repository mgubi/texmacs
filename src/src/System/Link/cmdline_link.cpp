
/******************************************************************************
* MODULE     : cmdline_link.cpp
* DESCRIPTION: TeXmacs links by pipes, evaluation of single commands
* COPYRIGHT  : (C) 2025  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "basic.hpp"

// NOTE: with Qt pipes, the command line links are those of
// Plugins/Qt/qt_cmdline_link.cpp (QProcess), as the pipe links; on
// Windows, this file is for the builds without Qt (windows_process)
#if !(defined (QTTEXMACS) && (defined (OS_MINGW) || defined (QTPIPES)))

#include "tm_link.hpp"
#include "socket_notifier.hpp"
#include "sys_utils.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include <stdio.h>
#include <string.h>

#ifndef OS_MINGW
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <errno.h>
#define TM_FD_SET(fd, set) FD_SET(fd, set)
#else
#include "windows_process.hpp"
#endif // OS_MINGW

#if !defined(__APPLE__) && !defined(__FreeBSD__)
#include <malloc.h>
#endif

hashset<pointer> cmdline_link_set;
void cmdline_callback (void *obj, void *info);

#ifndef OS_MINGW
extern char **environ;
#endif

#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define CMDIN 0
#define CMDOUT 1
#define TERMCHAR '\1'

/******************************************************************************
* The cmdline_link class
******************************************************************************/

struct cmdline_link_rep: tm_link_rep {
  string name;          // name of the plugin
  string session;       // name of the session
  int    pid;           // process identifier of the child
  int    pp_in [2];     // for data going to the child
  int    pp_out[2];     // for data coming from the child
  int    pp_err[2];     // for error messages coming from the child
  int    in;            // file descriptor for data going to the child
  int    out;           // file descriptor for data coming from the child
  int    err;           // file descriptor for errors coming from the child
#ifdef OS_MINGW
  windows_process proc; // the child, with its pipes (instead of the above)
#endif

  string outbuf;        // pending output from plugin
  string errbuf;        // pending errors from plugin

  socket_notifier snout, snerr;
  
public:
  cmdline_link_rep (string name, string session);
  ~cmdline_link_rep ();

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

cmdline_link_rep::cmdline_link_rep (string name2, string session2):
  name (name2), session (session2)
{
  cmdline_link_set->insert ((pointer) this);
  in     = pp_in [0]= pp_in [1]= -1;
  out    = pp_out[0]= pp_out[1]= -1;
  err    = pp_err[0]= pp_err[1]= -1;
  outbuf = "";
  errbuf = "";
  alive  = false;
  pid    = -1;  // no process yet: terminate_child does nothing
}

cmdline_link_rep::~cmdline_link_rep () {
  stop ();
  cmdline_link_set->remove ((pointer) this);
}

tm_link
make_cmdline_link (string name, string session) {
  return tm_new<cmdline_link_rep> (name, session);
}

#ifndef OS_MINGW
// NOTE: the same as in pipe_link.cpp, which is not compiled with Qt pipes
static void
terminate_child (int pid) {
  // no process (fork failed, or none was made): killpg (-1, ...) fails, and
  // kill (-1, SIGKILL) below would kill every process of the user
  if (pid <= 0) return;
  // Ask the process group of the child to terminate, give it up to 2 s to
  // do so, kill what is left of it otherwise, and reap the child. The whole
  // group is waited for, not only the child: a wrapper script often runs
  // the program as a child of its own (without exec), and stops at once on
  // SIGTERM while the program still cleans up
  if (-1 != killpg (pid, SIGTERM)) {
    bool reaped= false;
    for (int i=0; i<200; i++) {
      if (!reaped && waitpid (pid, NULL, WNOHANG) != 0) reaped= true;
      if (reaped && killpg (pid, 0) == -1) return;  // the group is gone
      usleep (10000);
    }
    // (the group may have ended during the last pause: its number may then
    // be another group's; EPERM: a member which we may not signal)
    if (killpg (pid, 0) == 0 || errno == EPERM) killpg (pid, SIGKILL);
    if (!reaped) waitpid (pid, NULL, 0);
  }
  else {
    kill (pid, SIGKILL);
    waitpid (pid, NULL, 0);
  }
}
#endif

void
cmdline_link_rep::close_fds () {
#ifndef OS_MINGW
  if (in  != -1) { close (in ); in = -1; }
  if (out != -1) { close (out); out= -1; }
  if (err != -1) { close (err); err= -1; }
#endif
}

void
close_all_cmdlines () {
  iterator<pointer> it= iterate (cmdline_link_set);
  while (it->busy()) {
    cmdline_link_rep* con= (cmdline_link_rep*) it->next();
#ifdef OS_MINGW
    if (con->alive) {
      con->proc.terminate (0);
      con->alive= false;
    }
#else
    if (con->alive) {
      terminate_child (con->pid);
      con->alive= false;
      remove_notifier (con->snout);
      remove_notifier (con->snerr);
      con->close_fds ();
      //cout << "Alive = false, close all\n";
    }
#endif
  }
}

void
process_all_cmdlines () {
  iterator<pointer> it= iterate (cmdline_link_set);
  while (it->busy()) {
    cmdline_link_rep* con= (cmdline_link_rep*) it->next();
    //cout << con->name << " ~> " << (con->alive? "true": "false") << "\n";
    if (con->alive)
      con->apply_command ();
  }
}

/******************************************************************************
* Routines for cmdline_links
******************************************************************************/
// NOTE: the copy in pipe_link.cpp is local to it
#ifndef OS_MINGW
void
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

string
cmdline_link_rep::start () {
  outbuf= "";
  errbuf= "";
  return "cmdline";
}

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

void
cmdline_link_rep::write (string s, int channel) {
  while (ends (s, "\n")) s= s (0, N(s)-1);
  //cout << "Write[" << name << "] " << s << "\n";
  if (alive || (channel != LINK_IN)) return;
  string cmd= as_string (call ("connection-cmdline", name, session, s));
  if (cmd == "") return;
#ifdef OS_MINGW
  // The command is a line of the command interpreter (%ComSpec% /d /s /c,
  // as the shell runs it on Unix, and as _wsystem does), its errors going
  // to NUL (as 2> /dev/null, for all the commands of the line). Nothing is
  // written to its input, which is closed at once: a program which reads
  // it ends instead of waiting (the input is in the command).
  if (DEBUG_IO) debug_io << "Launching '" << cmd << "'\n";
  if (!proc.start (std::string (&cmd[0], N(cmd)), true, false)) {
    errbuf << "Error: cannot start '" << cmd << "'\n";
    if (DEBUG_IO) debug_io << string (proc.error ().c_str ()) << "\n";
    return;
  }
  proc.close_input ();
  alive= true;
#else
  cmd= cmd * " 2> /dev/null";
  //cout << "Command[" << name << "," << s << "] = " << cmd << "\n";
  if (DEBUG_IO) debug_io << "Launching '" << cmd << "'\n";
  int e1= pipe (pp_in ); (void) e1;
  int e2= pipe (pp_out); (void) e2;
  int e3= pipe (pp_err); (void) e3;
  pid= fork ();
  if (pid < 0) { // no process (a page has none): not a live program
    int* fds[3]= { pp_in, pp_out, pp_err };
    for (int i= 0; i < 3; i++)
      for (int j= 0; j < 2; j++)
        if (fds[i][j] >= 0) { close (fds[i][j]); fds[i][j]= -1; }
    errbuf << "Error: cannot start '" << cmd << "'\n";
    return;
  }
  if (pid==0) { // the child
    setsid();
    // an interrupt is not ignored there (a TeXmacs started with
    // SIGINT ignored, as by `sh ... &', passes that on)
    signal (SIGINT, SIG_DFL);
    signal (SIGQUIT, SIG_DFL);
    close (pp_in  [CMDOUT]);
    close (pp_out [CMDIN ]);
    close (pp_err [CMDIN ]);
    dup2  (pp_in  [CMDIN ], STDIN );
    close (pp_in  [CMDIN ]);
    dup2  (pp_out [CMDOUT], STDOUT);
    close (pp_out [CMDOUT]);
    dup2  (pp_err [CMDOUT], STDERR);
    close (pp_err [CMDOUT]);

    execute_shell (cmd);
    exit (127);
    // exit (system (cmd) != 0);
  }
  else { // the main process
    in = pp_in  [CMDOUT];
    close (pp_in [CMDIN]);
    out= pp_out [CMDIN ];
    close (pp_out [CMDOUT]);
    err= pp_err [CMDIN ];
    close (pp_err [CMDOUT]);
    // (handed over: the numbers are not kept, so never closed again)
    pp_in[0]= pp_in[1]= pp_out[0]= pp_out[1]= pp_err[0]= pp_err[1]= -1;

    alive= true;
    //cout << "Alive = true\n";
    snout = socket_notifier (out, &cmdline_callback, this, NULL);
    snerr = socket_notifier (err, &cmdline_callback, this, NULL);
    add_notifier (snout);
    add_notifier (snerr);
  }
#endif
}

void
cmdline_link_rep::feed (int channel) {
  //cout << "Feed " << channel << "\n";
#ifdef OS_MINGW
  // what the threads of proc have read, from both pipes; the end is that
  // of the output, as on Unix (the process then ends: it is given 2 s, as
  // after SIGTERM in terminate_child, and what is left of it is ended)
  (void) channel;
  if (!alive) return;
  std::string o, e;
  bool end= proc.take (o, e);
  if (o.size () > 0) {
    string s (o.data (), (int) o.size ());
    if (DEBUG_IO) debug_io << debug_io_string (s);
    outbuf << s;
  }
  if (e.size () > 0) errbuf << string (e.data (), (int) e.size ());
  if (end) {
    proc.terminate (2000);
    alive= false;
  }
#else
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;
  int r;
  char tempout[1024];
  if (channel == LINK_OUT) r = ::read (out, tempout, 1024);
  else r = ::read (err, tempout, 1024);
  // NOTE: an interrupted read is tried again later; after any other
  // failure, the pipe is closed as at its end, since it would otherwise
  // be reported readable again and again
  if (r == -1 && (errno == EINTR || errno == EAGAIN)) return;
  if (r == -1) io_error << "Read failed for '" << name << "'\n";
  if (r <= 0) {
    if (r == 0 && channel != LINK_OUT) return;
    terminate_child (pid);
    alive= false;
    //cout << "Alive = false, feed\n";
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
cmdline_link_rep::watch (int channel) {
  //cout << "Watch " << channel << "\n";
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
cmdline_link_rep::read (int channel) {
  //cout << "Read " << channel << "\n";
  if (channel == LINK_OUT) {
#ifdef OS_MINGW
    // NOTE: no notifiers here (on Unix, perform_select feeds the link):
    // what waits for the answer by reading it (connection_eval) takes the
    // news itself, waiting for them at most 1 ms
    if (alive && proc.wait (1)) feed (LINK_OUT);
#endif
    if (alive) {
      //cout << "\n--- partial output ---\n" << outbuf << "\n";
      return "";
    }
    string r= outbuf;
    outbuf= "";
#ifdef OS_MINGW
    // the ends of lines of the programs of Windows (as QIODevice::Text)
    r= replace (r, "\r\n", "\n");
#endif
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= errbuf;
    errbuf= "";
#ifdef OS_MINGW
    r= replace (r, "\r\n", "\n");
#endif
    return r;
  }
  else return string("");
}

void
cmdline_link_rep::listen (int msecs) {
  //cout << "Listen " << msecs << "\n";
  if (!alive) return;
  time_t wait_until= texmacs_time () + msecs;
#ifdef OS_MINGW
  // as below, the pipes being read by the threads of proc: wait for news
  // of them, take what they read, until nothing more comes within msecs
  while (alive) {
    int nout= N(outbuf), nerr= N(errbuf);
    if (proc.wait (msecs)) feed (LINK_OUT);
    if (texmacs_time () - wait_until > 0) break;
    if (N(outbuf) == nout && N(errbuf) == nerr) break;
  }
#else
  // NOTE: the end of the output closes the descriptors (feed): they are
  // only used while the link is alive
  while (alive) {
    int nout= N(outbuf), nerr= N(errbuf);
    fd_set rfds;
    FD_ZERO (&rfds);
    TM_FD_SET (out, &rfds);
    TM_FD_SET (err, &rfds);
    struct timeval tv;
    tv.tv_sec  = msecs / 1000;
    tv.tv_usec = 1000 * (msecs % 1000);
    int nr= select (max (out, err) + 1, &rfds, NULL, NULL, &tv);
    if (nr > 0 && FD_ISSET (out, &rfds)) feed (LINK_OUT);
    if (alive && nr > 0 && FD_ISSET (err, &rfds)) feed (LINK_ERR);
    if (texmacs_time () - wait_until > 0) break;
    if (N(outbuf) == nout && N(errbuf) == nerr) break;
  }
#endif
}

void
cmdline_link_rep::interrupt () {
  //cout << "Interrupt\n";
  if (!alive) return;
#ifdef OS_MINGW
  // no SIGINT for the processes without console: the process and all it
  // started are ended, as by Ctrl+C (STATUS_CONTROL_C_EXIT); the end of
  // their output then ends the link, as on Unix
  proc.kill (0xC000013A);
#else
  killpg (pid, SIGINT);
#endif
}

void
cmdline_link_rep::stop () {
  //cout << "Stop\n";
  if (!alive) return;
#ifdef OS_MINGW
  // Windows cannot ask a program of the console to end (QProcess::terminate
  // posts WM_CLOSE, which they ignore): the process and all it started
  // are ended at once
  proc.terminate (0);
  alive= false;
#else
  terminate_child (pid);
  alive= false;    
  //cout << "Alive = false, stop\n";

  remove_notifier (snout);
  remove_notifier (snerr);
  close_fds ();
#endif
}

/******************************************************************************
* Call back for new information on pipe
******************************************************************************/

void cmdline_callback (void *obj, void *info) {
  //cout << "Cmd_line_callback\n";
#ifdef OS_MINGW
  (void) obj;
  (void) info;
#else
  (void) info;
  cmdline_link_rep* con= (cmdline_link_rep*) obj;  
  bool busy= true;
  bool news= false;
  while (busy && con->alive) {
    fd_set rfds;
    FD_ZERO (&rfds);
    int max_fd= max (con->err, con->out) + 1;
    TM_FD_SET (con->out, &rfds);
    TM_FD_SET (con->err, &rfds);
  
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    int nr= select (max_fd, &rfds, NULL, NULL, &tv);
    if (nr < 0 && errno == EINTR) continue;
    // (after an error, the set is unspecified: nothing is read)
    if (nr <= 0) break;

    busy= false;
    if (con->alive && FD_ISSET (con->out, &rfds)) {
      //cout << "cmdline_callback OUT" << LF;
      con->feed (LINK_OUT);
      busy= news= true;
    }
    if (con->alive && FD_ISSET (con->err, &rfds)) {
      //cout << "cmdline_callback ERR" << LF;
      con->feed (LINK_ERR);
      busy= news= true;
    }
  }
  /* FIXME: find out the appropriate place to call the callback
     Currently, the callback is called in tm_server_rep::interpose_handler */
  if (!is_nil (con->feed_cmd) && news) {
    //cout << "cmdline_callback APPLY" << LF;
    if (!is_nil (con->feed_cmd))
      con->feed_cmd->apply (); // call the data processor
  }
#endif
}

#endif // !(defined (QTTEXMACS) && (defined (OS_MINGW) || defined (QTPIPES)))
