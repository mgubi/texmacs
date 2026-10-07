
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
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#endif

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

QTMPipeLink::QTMPipeLink (string cmd2) : cmd (cmd2), outbuf (""), errbuf ("") {}

QTMPipeLink::~QTMPipeLink () {
  killProcess (1000);
}

bool
QTMPipeLink::launchCmd () {
  if (state () != QProcess::NotRunning) killProcess (1000);

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
  // as the pipes without Qt (System/Link/pipe_link.cpp): sh runs the
  // command, so that it may start with a word of the shell (if, readarray),
  // in a process group of its own, which stop terminates as a whole; a
  // program which is not found is still an error at once
  if (!pipe_program_found (cmd)) return false;
  program = "/bin/sh";
  args << "-c" << raw;
#if QT_VERSION >= 0x060000
  setChildProcessModifier ([] () { ::setsid (); });
#endif
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
  int written= QIODevice::write (_s, N(s));
  if (written == -1 || !waitForBytesWritten (-1)) return -1;
  return written;
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
#elif defined(OS_ANDROID)
  terminate ();
  if (! waitForFinished (msecs)) kill ();
#else
  // Ask the process group of the program to terminate and wait for all of
  // it (up to 2 s, or msecs), as the pipes without Qt do: a wrapper script
  // often runs the program as a child of its own, which still cleans up
  // when the wrapper is gone; then kill what is left
  int limit= (msecs > 0? msecs: 2000);
  qint64 pid= processId ();
  if (pid > 0 && ::killpg ((pid_t) pid, SIGTERM) == 0) {
    waitForFinished (limit);
    for (int waited= 0; waited < limit; waited += 10) {
      if (::killpg ((pid_t) pid, 0) == -1 && errno != EPERM) break;
      ::usleep (10000);
    }
    if (::killpg ((pid_t) pid, 0) == 0 || errno == EPERM)
      ::killpg ((pid_t) pid, SIGKILL);
  }
  if (state () != QProcess::NotRunning) {
    kill ();
    waitForFinished (1000);
  }
#endif
}

#if !defined (OS_MINGW) && !defined (OS_ANDROID) && QT_VERSION < 0x060000
void
QTMPipeLink::setupChildProcess () {
  ::setsid ();
}
#endif

