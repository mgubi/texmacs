
/******************************************************************************
* MODULE     : qt_pipe_link.cpp
* DESCRIPTION: QT TeXmacs links
* COPYRIGHT  : (C) 2009 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_link.hpp"
#include "qt_utilities.hpp"
#include "qt_gui.hpp"
#include "QTMPipeLink.hpp"
#include <QByteArray>

#ifdef OS_MINGW
#include <windows.h>
#elif !defined(OS_ANDROID)
#include <wordexp.h>
#endif
#ifndef OS_MINGW
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#endif
#ifdef QTM_PIPE_GROUPS
#include <stdio.h>
#include <string.h>
#ifdef __linux__
#include <dirent.h>
#endif
#endif

// NOTE: on Unix, the program is started in a process group of its own
// (setsid in the child), which is stopped or interrupted as a whole, as in
// pipe_link.cpp: a plugin started by a script which runs its program as a
// child (without exec) leaves nothing behind. Windows has no process groups
// for QProcess (nor Android here): there, only the process itself is
// stopped.

static string
debug_io_string (QByteArray s) {
  int i, n= s.size ();
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
QTMPipeLink::readErrOut () {
BEGIN_SLOT
  feedBuf (QProcess::StandardError);
  feedBuf (QProcess::StandardOutput);
END_SLOT
}

#ifdef QTM_PIPE_GROUPS
// in the child: a group of its own, where an interrupt is not ignored (a
// TeXmacs started with SIGINT ignored, as by `sh ... &', passes that on)
static void
new_group () {
  ::setsid ();
  ::signal (SIGINT, SIG_DFL);
  ::signal (SIGQUIT, SIG_DFL);
}
#endif

QTMPipeLink::QTMPipeLink (string cmd2):
  cmd (cmd2), outbuf (""), errbuf (""), feed_cmd (NULL), group (0)
{
#ifdef QTM_PIPE_GROUPS
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  setChildProcessModifier ([] () { new_group (); });
#endif
#endif
}

#if defined (QTM_PIPE_GROUPS) && QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
void
QTMPipeLink::setupChildProcess () {
  new_group ();
}
#endif

QTMPipeLink::~QTMPipeLink () {
#ifdef QTM_PIPE_GROUPS
  killGroup ();
#else
  killProcess (1000);
#endif
}

bool
QTMPipeLink::launchCmd () {
#ifdef QTM_PIPE_GROUPS
  if (state () != QProcess::NotRunning || group > 0) killGroup ();
#else
  if (state () != QProcess::NotRunning) killProcess (1000);
#endif

  QString raw = utf8_to_qstring(cmd);
  QString program;
  QStringList args;

#if defined(Q_OS_WIN)
  int argc = 0;
  LPWSTR *argv = CommandLineToArgvW((LPCWSTR)raw.utf16(), &argc);

  if (!argv) {
    return false;
  }

  if (argc > 0) {
    program = QString::fromWCharArray(argv[0]);
    for (int i = 1; i < argc; ++i)
        args << QString::fromWCharArray(argv[i]);
  }

  LocalFree(argv);

#elif !defined(OS_ANDROID)
  wordexp_t exp;
  memset(&exp, 0, sizeof(exp));

  int status = wordexp(raw.toUtf8().constData(), &exp, 0);

  if (status != 0) {
    wordfree(&exp);
    return false;
  }

  if (exp.we_wordc > 0) {
    program = QString::fromUtf8(exp.we_wordv[0]);
    for (size_t i = 1; i < exp.we_wordc; ++i)
    args << QString::fromUtf8(exp.we_wordv[i]);
  }

  wordfree(&exp);
#else
  QStringList list = QProcess::splitCommand(raw);
  if (!list.isEmpty()) {
    program = list.takeFirst();
  }
  args = list;
#endif

  this->start(program, args);

  bool ok = waitForStarted();
  if (ok) {
#ifdef QTM_PIPE_GROUPS
    // (the process leads its group: the group has its number)
#if QT_VERSION < QT_VERSION_CHECK(5, 3, 0)
    group= (qint64) pid ();
#else
    group= processId ();
#endif
#endif
    // NOTE: unique, since a process which exited by itself is started
    // again without disconnecting (see killProcess)
    connect(this, SIGNAL(readyReadStandardOutput()), SLOT(readErrOut()),
            Qt::UniqueConnection);
    connect(this, SIGNAL(readyReadStandardError()), SLOT(readErrOut()),
            Qt::UniqueConnection);
  }
  return ok;
}

int
QTMPipeLink::writeStdin (string s) {
  c_string _s (s);
  if (DEBUG_IO) debug_io << "[INPUT]" << debug_io_string ((char*)_s);
  int err= QIODevice::write (_s, N(s));
  return err;
}

void
QTMPipeLink::feedBuf (ProcessChannel channel) {
  setReadChannel (channel);
  QByteArray tempout = QIODevice::readAll ();
  string s (tempout.constData (), tempout.size ());
  if (channel == QProcess::StandardOutput) outbuf << s;
  else errbuf << s;
  if (DEBUG_IO)
    debug_io << "[OUTPUT " << channel << "]" << debug_io_string (tempout) << "\n";
}

bool
QTMPipeLink::listenChannel (ProcessChannel channel, int msecs) {
  setReadChannel (channel);
  return waitForReadyRead (msecs);
}

void
QTMPipeLink::killProcess (int msecs) {
  disconnect (SIGNAL(readyReadStandardOutput ()), this, SLOT(readErrOut ()));
  disconnect (SIGNAL(readyReadStandardError ()), this, SLOT(readErrOut ()));
#ifdef OS_MINGW
  (void) msecs;
  close ();
#else
  terminate ();
  if (! waitForFinished (msecs)) {
    kill ();
    // the process is reaped here: QProcess reaps it only when it notices
    // its end, in the event loop or in a wait; without one (a script run
    // with -x, the test suites) a stopped plugin stayed a zombie where
    // QProcess is told of the end by a pidfd (Linux), not by SIGCHLD (macOS)
    waitForFinished (1000);
  }
#endif
}

#ifdef QTM_PIPE_GROUPS
// NOTE: the group is waited for before any wait of Qt: once Qt sees that the
// program has ended (in a wait, or in the event loop), it closes its ends of
// the pipes, and a member of the group which still cleans up then dies of
// SIGPIPE when it writes there. Qt reaps the program at once on macOS (by
// the SIGCHLD handler of forkfd), but only in a wait or in the event loop on
// Linux (where it learns of the end by a pidfd): a group whose program is a
// zombie is ended. Once a group is gone, its number may be that of a new
// process: it is checked before each signal, and forgotten (group 0) once it
// is gone

bool
QTMPipeLink::groupAlive (qint64 pg) {
  // whether a process of the group pg is left, not counting zombies (EPERM:
  // a member which we may not signal)
  if (pg <= 0) return false;
#ifdef __linux__
  DIR* dir= opendir ("/proc");
  if (dir != NULL) {
    bool found= false;
    while (!found) {
      struct dirent* e= readdir (dir);
      if (e == NULL) break;
      if (e->d_name[0] < '0' || e->d_name[0] > '9') continue;
      char path[64], buf[512];
      snprintf (path, sizeof (path), "/proc/%s/stat", e->d_name);
      FILE* f= fopen (path, "r");
      if (f == NULL) continue;
      size_t n= fread (buf, 1, sizeof (buf) - 1, f);
      fclose (f);
      buf[n]= '\0';
      // pid (command) state ppid pgrp ...: the command may hold parentheses
      char* q= strrchr (buf, ')');
      char st;
      int ppid, pgrp;
      if (q != NULL && sscanf (q + 1, " %c %d %d", &st, &ppid, &pgrp) == 3)
        found= (pgrp == (int) pg && st != 'Z' && st != 'X');
    }
    closedir (dir);
    return found;
  }
#endif
  return ::killpg ((pid_t) pg, 0) == 0 || errno == EPERM;
}

bool
QTMPipeLink::hasEnded () {
  // whether the group is gone and the process reaped
  if (group > 0) {
    if (groupAlive (group)) return false;
    group= 0;
    // (the program has ended: the wait only reaps it)
    if (state () != QProcess::NotRunning) waitForFinished (1000);
  }
  return state () == QProcess::NotRunning || waitForFinished (0);
}

bool
QTMPipeLink::askToEnd () {
  // Ask the group to terminate (SIGTERM), and wait for it 50 ms at most:
  // whether it has ended. A program which takes longer (or ignores SIGTERM)
  // is killed later by killGroup, see reap_ending_links in qt_pipe_link.cpp
  disconnect (SIGNAL(readyReadStandardOutput ()), this, SLOT(readErrOut ()));
  disconnect (SIGNAL(readyReadStandardError ()), this, SLOT(readErrOut ()));
  if (!groupAlive (group) || ::killpg ((pid_t) group, SIGTERM) == -1) {
    // no group (any more): only the process, if it still runs
    group= 0;
    if (state () != QProcess::NotRunning) {
      kill ();
      waitForFinished (1000);
    }
    return true;
  }
  for (int i=0; i<5 && groupAlive (group); i++) usleep (10000);
  return hasEnded ();
}

void
QTMPipeLink::killGroup () {
  // kill the group at once, and reap the process
  disconnect (SIGNAL(readyReadStandardOutput ()), this, SLOT(readErrOut ()));
  disconnect (SIGNAL(readyReadStandardError ()), this, SLOT(readErrOut ()));
  if (groupAlive (group)) ::killpg ((pid_t) group, SIGKILL);
  group= 0;
  if (state () != QProcess::NotRunning) {
    kill ();
    // (QProcess reaps the process only when it notices its end, in the
    // event loop or in a wait, see killProcess)
    waitForFinished (1000);
  }
}
#endif

#ifndef OS_MINGW
void
QTMPipeLink::interruptGroup () {
  if (group > 0 && ::killpg ((pid_t) group, SIGINT) != -1) return;
#if QT_VERSION < QT_VERSION_CHECK(5, 3, 0)
  qint64 pid= (qint64) this->pid ();
#else
  qint64 pid= processId ();
#endif
  if (pid <= 0 || ::kill ((pid_t) pid, SIGINT) == 0) return;
  // (ESRCH: the process has just ended, and is not reaped yet)
  int err= errno;
  if (err != ESRCH)
    qt_error << "Interrupt not successful, pid: " << pid
             << " return code: " << err << "\n";
}
#endif

