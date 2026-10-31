
/******************************************************************************
* MODULE     : qt_pipe_link.cpp
* DESCRIPTION: TeXmacs links by pipes
* COPYRIGHT  : (C) 2000  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "basic.hpp"

#if defined (QTTEXMACS) && (defined (OS_MINGW) || defined (QTPIPES))

#include "tm_link.hpp"
#include "QTMPipeLink.hpp"
#include "hashset.hpp"
#include "array.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"

#include <sys/types.h>
#include <signal.h>
#ifdef OS_MINGW
#else
#  include <sys/wait.h>
#  include <unistd.h>
#endif
#include <errno.h>
#include <QThread>

void close_all_cmdlines ();
void process_all_cmdlines ();

hashset<pointer> pipe_link_set;

/******************************************************************************
* Definition of qt_pipe_link_rep class
******************************************************************************/

struct qt_pipe_link_rep: tm_link_rep {
  QTMPipeLink* PipeLink;

public:
  qt_pipe_link_rep (string cmd);
  ~qt_pipe_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  bool    is_readable (int channel);
  void    interrupt ();
  void    stop ();
  void    feed (int channel);
  void    end_process ();
};

qt_pipe_link_rep::qt_pipe_link_rep (string cmd) {
  PipeLink = new QTMPipeLink (cmd);
  PipeLink->feed_cmd = &feed_cmd;
  pipe_link_set->insert ((pointer) this);
  alive = false;
}

qt_pipe_link_rep::~qt_pipe_link_rep () {
  stop ();
  pipe_link_set->remove ((pointer) this);
  delete PipeLink;
}

/******************************************************************************
* Ending the processes
******************************************************************************/

#ifdef QTM_PIPE_GROUPS
// The processes which were asked to terminate and have not ended yet (with
// their groups), and the times when they are killed: their QProcess is
// handed over to here, and the link gets a new one
static array<pointer> ending_link;
static array<time_t>  ending_kill_at;
static pid_t ending_owner= 0;  // the process of TeXmacs (not a fork of it)

static void
kill_ending_groups () {
  // at an exit which skips close_all_pipes (exit from Scheme): the groups
  // which are left are killed at once (Qt may be gone: only the signals;
  // a group which is gone may have the number of another process)
  if (getpid () != ending_owner) return;
  for (int i=0; i<N(ending_link); i++) {
    qint64 group= ((QTMPipeLink*) ending_link[i])->group;
    if (QTMPipeLink::groupAlive (group)) ::killpg ((pid_t) group, SIGKILL);
  }
}

static void
reap_ending_links (bool wait) {
  // the processes of end_process which have ended are reaped, those whose
  // time is up are killed; @wait: wait for them all (at the exit)
  array<pointer> links;
  array<time_t>  kill_ats;
  for (int i=0; i<N(ending_link); i++) {
    QTMPipeLink* p= (QTMPipeLink*) ending_link[i];
    if (wait)
      while (!p->hasEnded () && texmacs_time () - ending_kill_at[i] < 0)
        usleep (10000);
    if (p->hasEnded ()) delete p;
    else if (wait || texmacs_time () - ending_kill_at[i] >= 0) {
      p->killGroup ();
      delete p;
    }
    else {
      links    << ending_link[i];
      kill_ats << ending_kill_at[i];
    }
  }
  ending_link   = links;
  ending_kill_at= kill_ats;
}
#endif

void
qt_pipe_link_rep::end_process () {
  // End the process and its group. On Unix, the group is asked to terminate
  // and is given up to 2 s to do so, after which what is left of it is
  // killed, as in pipe_link.cpp: only the first 50 ms are waited for here,
  // the rest by reap_ending_links, so that the interface is not blocked
#ifndef QTM_PIPE_GROUPS
  PipeLink->killProcess (0);
#else
  if (PipeLink->state () == QProcess::NotRunning && PipeLink->group <= 0)
    return;
  if (PipeLink->askToEnd ()) return;
  if (ending_owner == 0) {
    ending_owner= getpid ();
    atexit (kill_ending_groups);
  }
  ending_link    << (pointer) PipeLink;
  ending_kill_at << (texmacs_time () + 2000 - 50);
  QTMPipeLink* p= new QTMPipeLink (PipeLink->cmd);
  p->feed_cmd= &feed_cmd;
  p->setOutbuf (PipeLink->getOutbuf ());
  p->setErrbuf (PipeLink->getErrbuf ());
  PipeLink= p;
#endif
}

/******************************************************************************
* Routines for qt_pipe_links
******************************************************************************/

string
qt_pipe_link_rep::start () {
#ifdef QTM_PIPE_GROUPS
  reap_ending_links (false);
#endif
  if (alive) return "busy";
#ifdef QTM_PIPE_GROUPS
  // (what is left of the group of a process which exited by itself)
  end_process ();
#endif
  if (DEBUG_AUTO)
    debug_shell << "Launching '" << PipeLink->cmd << "'\n";
  if (! PipeLink->launchCmd ()) {
    if (DEBUG_AUTO)
      debug_shell << "TeXmacs] Error: cannot start '" << PipeLink->cmd << "'\n";
    return "Error: cannot start application";
  }
  alive= true;
  return "ok";
}

void
qt_pipe_link_rep::write (string s, int channel) {
  if ((!alive) || (channel != LINK_IN)) return;
  if (-1 == PipeLink->writeStdin (s)) {
    io_error << "Error: cannot write to '" << PipeLink->cmd << "'\n";
    stop();
  }
}

void
qt_pipe_link_rep::feed (int channel) {
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR))) return;
  if (channel == LINK_OUT) PipeLink->feedBuf (QProcess::StandardOutput);
  else PipeLink->feedBuf (QProcess::StandardError);
}

string&
qt_pipe_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return PipeLink->getOutbuf ();
  else if (channel == LINK_ERR) return PipeLink->getErrbuf ();
  else  return empty_string;
}

string
qt_pipe_link_rep::read (int channel) {
  listen (0);
  if (alive && PipeLink->state () == QProcess::NotRunning) {
    // the process has exited: what it wrote is read, and the link is dead
    // once the other channel has been read too
    PipeLink->feedBuf (QProcess::StandardOutput);
    PipeLink->feedBuf (QProcess::StandardError);
    string& other= (channel == LINK_OUT? PipeLink->getErrbuf ():
                                         PipeLink->getOutbuf ());
    if (other == "") {
      alive= false;
#ifdef QTM_PIPE_GROUPS
      // (what is left of its group, as in pipe_link.cpp)
      end_process ();
#endif
    }
  }
  if (channel == LINK_OUT) {
    string r= PipeLink->getOutbuf ();
    PipeLink->setOutbuf ("");
    return r;
  }
  else if (channel == LINK_ERR) {
    string r= PipeLink->getErrbuf ();
    PipeLink->setErrbuf ("");
    return r;
  }
  else return string("");
}

void
qt_pipe_link_rep::listen (int msecs) {
  if (!alive) return;
  time_t wait_until= texmacs_time () + msecs;
  while ((PipeLink->getOutbuf() == "") && (PipeLink->getErrbuf() == "")) {
    PipeLink->listenChannel (QProcess::StandardOutput, 0);
    PipeLink->listenChannel (QProcess::StandardError, 0);
    // (both channels are polled: wait a little between two polls rather
    // than keep a processor busy, but not once the time is over, so that
    // listen (0) only polls)
    if (texmacs_time () - wait_until >= 0) break;
    if (PipeLink->getOutbuf() == "" && PipeLink->getErrbuf() == "")
      QThread::msleep (1);
  }
}

bool
qt_pipe_link_rep::is_readable (int channel) {
  if ((!alive) || ((channel != LINK_OUT) && (channel != LINK_ERR)))
    return false;
  if (channel == LINK_OUT)
    return PipeLink->listenChannel (QProcess::StandardOutput, 0);
  else
    return PipeLink->listenChannel (QProcess::StandardError, 0);
}

void
qt_pipe_link_rep::interrupt () {
  if (!alive) return;
#ifdef OS_MINGW
  // Not implemented
  qt_error << "SIGINT not implemented on Windows\n";
#else
  // the whole group, as in pipe_link.cpp (killpg did not work here before
  // the process had a group of its own)
  PipeLink->interruptGroup ();
#endif
}

void
qt_pipe_link_rep::stop () {
#ifdef QTM_PIPE_GROUPS
  reap_ending_links (false);
#endif
  end_process ();
  alive= false;
}

/******************************************************************************
* Main builder function for qt_pipe_links
******************************************************************************/

tm_link
make_pipe_link (string cmd) {
  return tm_new<qt_pipe_link_rep> (cmd);
}

/******************************************************************************
* Emergency exit for all pipes
******************************************************************************/

void
close_all_pipes () {
  iterator<pointer> it= iterate (pipe_link_set);
  while (it->busy()) {
    qt_pipe_link_rep* con= (qt_pipe_link_rep*) it->next();
#ifdef QTM_PIPE_GROUPS
    // (all are asked to terminate first, then waited for together: at most
    // about 2 s in all, not 2 s for each; also what is left of the group
    // of a plugin which exited by itself)
    con->end_process ();
    con->alive= false;
#else
    if (con->alive) con->stop ();
#endif
  }
#ifdef QTM_PIPE_GROUPS
  reap_ending_links (true);
#endif
  close_all_cmdlines ();
}

void
process_all_pipes () {
#ifdef QTM_PIPE_GROUPS
  if (N(ending_link) > 0) reap_ending_links (false);
#endif
  iterator<pointer> it= iterate (pipe_link_set);
  while (it->busy()) {
    qt_pipe_link_rep* con= (qt_pipe_link_rep*) it->next();
#ifdef QTM_PIPE_GROUPS
    // (a program which exited by itself, and was not read since)
    con->PipeLink->forgetEndedGroup ();
#endif
    if (con->alive) con->apply_command ();
  }
  process_all_cmdlines ();
}

#endif // defined (QTTEXMACS) && defined (OS_MINGW)
