/******************************************************************************
* MODULE     : qt_cmdline_link.cpp
* DESCRIPTION: TeXmacs links by pipes, evaluation of single commands (Qt)
* COPYRIGHT  : (C) 2025  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "basic.hpp"

#if defined (QTTEXMACS) && (defined (OS_MINGW) || defined (QTPIPES))

#include "tm_link.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "tm_timer.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "qt_utilities.hpp"
#include <QProcess>

#ifdef OS_MINGW
#include <windows.h>
#include <tlhelp32.h>
#else
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#endif

// NOTE: the command line links of cmdline_link.cpp, with QProcess as the
// pipe links of qt_pipe_link.cpp. As there, the command and all it starts
// form a group, which is stopped or interrupted as a whole: a process group
// of its own on Unix (setsid in the child, then killpg), a job on Windows
// (the process is started suspended, put in the job, then resumed).
// Differences with cmdline_link.cpp: the input of the command is empty
// rather than a pipe left open, and the end of the command is that of the
// shell rather than that of its output (what is left of the group is then
// stopped as at the end of the output there, and what it would still write
// is lost).

hashset<pointer> cmdline_link_set;

/******************************************************************************
* The process of a command, and its group
******************************************************************************/

class qt_cmdline_process: public QProcess {
public:
  qt_cmdline_process () {
#ifndef OS_MINGW
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    setChildProcessModifier ([] () { ::setsid (); });
#endif
#endif
  }
#if !defined (OS_MINGW) && QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
protected:
  void setupChildProcess () override { ::setsid (); }
#endif
};

#ifdef OS_MINGW
static void
resume_process (DWORD pid) {
  // the threads of a process started suspended (QProcess keeps the handle
  // of its thread to itself): there is only one, the first
  HANDLE snap= CreateToolhelp32Snapshot (TH32CS_SNAPTHREAD, 0);
  if (snap == INVALID_HANDLE_VALUE) return;
  THREADENTRY32 te;
  te.dwSize= sizeof (te);
  if (Thread32First (snap, &te))
    do {
      if (te.th32OwnerProcessID != pid) continue;
      HANDLE th= OpenThread (THREAD_SUSPEND_RESUME, FALSE, te.th32ThreadID);
      if (th != NULL) { ResumeThread (th); CloseHandle (th); }
    } while (Thread32Next (snap, &te));
  CloseHandle (snap);
}

static HANDLE
make_job (DWORD pid) {
  // a job which ends all its processes once closed, with the process
  HANDLE job= CreateJobObjectW (NULL, NULL);
  if (job == NULL) return NULL;
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION li;
  memset (&li, 0, sizeof (li));
  li.BasicLimitInformation.LimitFlags= JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
  HANDLE ph= OpenProcess (PROCESS_SET_QUOTA | PROCESS_TERMINATE, FALSE, pid);
  if (ph == NULL ||
      !SetInformationJobObject (job, JobObjectExtendedLimitInformation,
                                &li, sizeof (li)) ||
      !AssignProcessToJobObject (job, ph)) {
    if (ph != NULL) CloseHandle (ph);
    CloseHandle (job);
    return NULL;
  }
  CloseHandle (ph);
  return job;
}
#endif

/******************************************************************************
* The qt_cmdline_link class
******************************************************************************/

struct qt_cmdline_link_rep: tm_link_rep {
  string name;              // name of the plugin
  string session;           // name of the session
  qt_cmdline_process* proc; // the process of the command, if any
  qint64 pid;               // its identifier (also that of its group)
#ifdef OS_MINGW
  HANDLE job;               // the job of the command and all it started
#endif
  string outbuf;            // pending output from plugin
  string errbuf;            // pending errors from plugin

public:
  qt_cmdline_link_rep (string name, string session);
  ~qt_cmdline_link_rep ();

  string  start ();
  void    write (string s, int channel);
  string& watch (int channel);
  string  read (int channel);
  void    listen (int msecs);
  void    interrupt ();
  void    stop ();

  void    feed (int channel);
  void    terminate (bool ask);
};

qt_cmdline_link_rep::qt_cmdline_link_rep (string name2, string session2):
  name (name2), session (session2), proc (NULL), pid (0)
{
  cmdline_link_set->insert ((pointer) this);
#ifdef OS_MINGW
  job   = NULL;
#endif
  outbuf= "";
  errbuf= "";
  alive = false;
}

qt_cmdline_link_rep::~qt_cmdline_link_rep () {
  stop ();
  cmdline_link_set->remove ((pointer) this);
}

tm_link
make_cmdline_link (string name, string session) {
  return tm_new<qt_cmdline_link_rep> (name, session);
}

void
qt_cmdline_link_rep::terminate (bool ask) {
  // End the group of the command, and the process itself. With @ask (Unix
  // only), the group is asked to terminate (SIGTERM), and given up to 2 s
  // to do so, as by terminate_child in cmdline_link.cpp
  if (proc == NULL) return;
#ifdef OS_MINGW
  (void) ask;
  // Windows cannot ask a program of the console to end (QProcess::terminate
  // posts WM_CLOSE, which they ignore): the job is ended at once
  if (job != NULL) {
    TerminateJobObject (job, 1);
    CloseHandle (job);
    job= NULL;
  }
  if (proc->state () != QProcess::NotRunning) proc->kill ();
#else
  // NOTE: the process itself is reaped by QProcess (waitForFinished), never
  // by waitpid here; while it is not, its group exists
  if (pid > 0 && ::killpg ((pid_t) pid, ask? SIGTERM: SIGKILL) != -1) {
    for (int i=0; ask && i<200; i++) {
      bool done= proc->state () == QProcess::NotRunning ||
                 proc->waitForFinished (10);
      if (done && ::killpg ((pid_t) pid, 0) == -1) break;  // the group is gone
      if (done) usleep (10000);
    }
    // (EPERM: a member which we may not signal)
    if (::killpg ((pid_t) pid, 0) == 0 || errno == EPERM)
      ::killpg ((pid_t) pid, SIGKILL);
  }
  else if (proc->state () != QProcess::NotRunning) proc->kill ();
#endif
  if (proc->state () != QProcess::NotRunning) proc->waitForFinished (-1);
  delete proc;
  proc= NULL;
  pid = 0;
}

void
close_all_cmdlines () {
  iterator<pointer> it= iterate (cmdline_link_set);
  while (it->busy()) {
    qt_cmdline_link_rep* con= (qt_cmdline_link_rep*) it->next();
    if (con->alive) con->stop ();
  }
}

void
process_all_cmdlines () {
  iterator<pointer> it= iterate (cmdline_link_set);
  while (it->busy()) {
    qt_cmdline_link_rep* con= (qt_cmdline_link_rep*) it->next();
    if (con->alive) con->apply_command ();
  }
}

/******************************************************************************
* Routines for qt_cmdline_links
******************************************************************************/

string
qt_cmdline_link_rep::start () {
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
qt_cmdline_link_rep::write (string s, int channel) {
  while (ends (s, "\n")) s= s (0, N(s)-1);
  if (alive || (channel != LINK_IN)) return;
  string cmd= as_string (call ("connection-cmdline", name, session, s));
  if (cmd == "") return;
  terminate (false);  // (the process of the previous command)
  proc= new qt_cmdline_process ();
  proc->setStandardInputFile (QProcess::nullDevice ());
  proc->setReadChannel (QProcess::StandardOutput);
#ifdef OS_MINGW
  // The command is a line of the command interpreter, as the shell runs
  // it on Unix (and as _wsystem does), its errors going to NUL
  QString shell= qEnvironmentVariable ("ComSpec");
  if (shell.isEmpty ()) shell= "cmd.exe";
  proc->setProgram (shell);
  proc->setNativeArguments (QString ("/d /s /c \"") + utf8_to_qstring (cmd) +
                            QString ("\""));
  proc->setStandardErrorFile (QProcess::nullDevice ());
  proc->setCreateProcessArgumentsModifier (
    [] (QProcess::CreateProcessArguments* args) {
      args->flags |= CREATE_SUSPENDED; });
#else
  cmd= cmd * " 2> /dev/null";
  proc->setProgram ("/bin/sh");
  proc->setArguments (QStringList () << "-c" << utf8_to_qstring (cmd));
#endif
  if (DEBUG_IO) debug_io << "Launching '" << cmd << "'\n";
  proc->start (QIODevice::ReadWrite);
  if (!proc->waitForStarted ()) {
    errbuf << "Error: cannot start '" << cmd << "'\n";
    delete proc;
    proc= NULL;
    return;
  }
  pid= proc->processId ();
#ifdef OS_MINGW
  // (if no job can be made, only the process itself can be ended)
  job= make_job ((DWORD) pid);
  resume_process ((DWORD) pid);
#endif
  alive= true;
}

void
qt_cmdline_link_rep::feed (int channel) {
  // what QProcess has read, without waiting; the end is that of the command
  (void) channel;
  if (!alive) return;
  bool end= proc->state () == QProcess::NotRunning ||
            proc->waitForFinished (0);
  QByteArray o= proc->readAllStandardOutput ();
  QByteArray e= proc->readAllStandardError ();
  if (o.size () > 0) {
    string s (o.constData (), o.size ());
    if (DEBUG_IO) debug_io << debug_io_string (s);
    outbuf << s;
  }
  if (e.size () > 0) errbuf << string (e.constData (), e.size ());
  if (end) {
    terminate (true);
    alive= false;
  }
}

string&
qt_cmdline_link_rep::watch (int channel) {
  static string empty_string= "";
  if (channel == LINK_OUT) return outbuf;
  else if (channel == LINK_ERR) return errbuf;
  else return empty_string;
}

string
qt_cmdline_link_rep::read (int channel) {
  if (channel == LINK_OUT) {
    // NOTE: there is no perform_select with Qt pipes: what waits for the
    // answer by reading it (connection_retrieve) takes the news itself,
    // waiting for them at most 1 ms
    if (alive) {
      proc->waitForReadyRead (1);
      feed (LINK_OUT);
    }
    if (alive) return "";
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
qt_cmdline_link_rep::listen (int msecs) {
  if (!alive) return;
  time_t wait_until= texmacs_time () + msecs;
  while (alive) {
    int nout= N(outbuf), nerr= N(errbuf);
    proc->waitForReadyRead (msecs);
    feed (LINK_OUT);
    if (texmacs_time () - wait_until > 0) break;
    if (N(outbuf) == nout && N(errbuf) == nerr) break;
  }
}

void
qt_cmdline_link_rep::interrupt () {
  if (!alive) return;
#ifdef OS_MINGW
  // no SIGINT for the processes without console: the process and all it
  // started are ended, as by Ctrl+C (STATUS_CONTROL_C_EXIT); the end of
  // the command then ends the link, as on Unix
  if (job != NULL) TerminateJobObject (job, 0xC000013A);
  else proc->kill ();
#else
  if (::killpg ((pid_t) pid, SIGINT) == -1) ::kill ((pid_t) pid, SIGINT);
#endif
}

void
qt_cmdline_link_rep::stop () {
  if (!alive) return;
  terminate (true);
  alive= false;
}

#endif // defined (QTTEXMACS) && (defined (OS_MINGW) || defined (QTPIPES))
