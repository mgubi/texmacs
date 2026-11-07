/******************************************************************************
* MODULE     : windows_process.cpp
* DESCRIPTION: child processes with pipes on Windows, for cmdline_link
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// What QProcess does on Windows, without Qt (the core of TeXmacs, and the
// Vue build for Windows, have no Qt):
// - the process is started with CreateProcessW, from a command in UTF-8,
//   its standard input, output and error being anonymous pipes; only the
//   ends of the child are inheritable, and only they are inherited (a
//   list of handles: a child does not keep the pipes of another one open,
//   which would delay the end of their output), without a console window;
// - one thread for each pipe from the child reads it into a buffer, under
//   a lock; TeXmacs takes what was read without ever blocking, and may
//   wait for news on an event;
// - the process is put in a job of its own (it is started suspended until
//   then, so that its children are in the job too), which ends it and all
//   it started at once: the equivalent of the process group of the Unix
//   code (killpg); the job is closed with the kill of what is left in it.
//   Windows has no SIGTERM: QProcess::terminate posts WM_CLOSE, which the
//   programs of the console ignore, and then kills; here the process is
//   given a grace period to end by itself (after the end of its output),
//   then what is left is ended.

#include "config.h"

// (the Qt builds use QProcess: Plugins/Qt/qt_cmdline_link.cpp)
#if defined (OS_MINGW) && !defined (QTTEXMACS)

#include "windows_process.hpp"
#include <windows.h>
#include <process.h>
#include <vector>

/******************************************************************************
* Conversions
******************************************************************************/

static std::wstring
utf8_to_wide (const std::string& s) {
  if (s.empty ()) return std::wstring ();
  int n= MultiByteToWideChar (CP_UTF8, 0, s.data (), (int) s.size (), NULL, 0);
  std::wstring r (n, 0);
  MultiByteToWideChar (CP_UTF8, 0, s.data (), (int) s.size (), &r[0], n);
  return r;
}

static std::string
wide_to_utf8 (const std::wstring& s) {
  if (s.empty ()) return std::string ();
  int n= WideCharToMultiByte (CP_UTF8, 0, s.data (), (int) s.size (),
                              NULL, 0, NULL, NULL);
  std::string r (n, 0);
  WideCharToMultiByte (CP_UTF8, 0, s.data (), (int) s.size (),
                       &r[0], n, NULL, NULL);
  return r;
}

static std::string
error_message (const char* what, DWORD e) {
  std::string r= what;
  wchar_t* buf= NULL;
  DWORD n= FormatMessageW (FORMAT_MESSAGE_ALLOCATE_BUFFER |
                           FORMAT_MESSAGE_FROM_SYSTEM |
                           FORMAT_MESSAGE_IGNORE_INSERTS,
                           NULL, e, 0, (LPWSTR) &buf, 0, NULL);
  if (n > 0 && buf != NULL) {
    std::wstring w (buf, n);
    while (!w.empty () && (w.back () == L'\n' || w.back () == L'\r'))
      w.pop_back ();
    r += ": " + wide_to_utf8 (w);
  }
  if (buf != NULL) LocalFree (buf);
  r += " (error " + std::to_string ((unsigned long) e) + ")";
  return r;
}

// the command interpreter, as _wsystem finds it
static std::wstring
command_interpreter () {
  wchar_t buf[MAX_PATH];
  DWORD n= GetEnvironmentVariableW (L"ComSpec", buf, MAX_PATH);
  if (n > 0 && n < MAX_PATH) return std::wstring (buf, n);
  n= GetSystemDirectoryW (buf, MAX_PATH);
  if (n > 0 && n < MAX_PATH) return std::wstring (buf, n) + L"\\cmd.exe";
  return L"cmd.exe";
}

/******************************************************************************
* The pipes from the child, read in the background
******************************************************************************/

// NOTE: a channel only uses what it owns (its lock, a copy of the handle of
// the event), so that a thread which could not be joined does no harm: the
// channel is then left to it, never freed

struct windows_channel {
  CRITICAL_SECTION lock;
  HANDLE      pipe;     // our end of the pipe
  HANDLE      news;     // signaled when something was read, or at the end
  HANDLE      thread;
  std::string buf;      // read, not yet taken
  bool        eof;      // the pipe was closed by all who wrote to it
};

static unsigned __stdcall
read_channel (void* arg) {
  windows_channel* ch= (windows_channel*) arg;
  char buf[4096];
  DWORD n;
  // NOTE: ReadFile fails at the end (ERROR_BROKEN_PIPE: all the ends for
  // writing were closed) or when cancelled (CancelSynchronousIo); it reads
  // nothing, successfully, when the child wrote nothing
  while (ReadFile (ch->pipe, buf, sizeof (buf), &n, NULL)) {
    if (n == 0) continue;
    EnterCriticalSection (&ch->lock);
    ch->buf.append (buf, n);
    LeaveCriticalSection (&ch->lock);
    SetEvent (ch->news);
  }
  EnterCriticalSection (&ch->lock);
  ch->eof= true;
  LeaveCriticalSection (&ch->lock);
  SetEvent (ch->news);
  return 0;
}

static windows_channel*
open_channel (HANDLE pipe, HANDLE news) {
  windows_channel* ch= new windows_channel;
  InitializeCriticalSection (&ch->lock);
  ch->pipe  = pipe;
  ch->news  = NULL;
  ch->eof   = false;
  DuplicateHandle (GetCurrentProcess (), news, GetCurrentProcess (),
                   &ch->news, 0, FALSE, DUPLICATE_SAME_ACCESS);
  ch->thread= (HANDLE) _beginthreadex (NULL, 0, read_channel, ch, 0, NULL);
  if (ch->thread == NULL) ch->eof= true;  // (never read: nothing comes)
  return ch;
}

static void
close_channel (windows_channel* ch) {
  if (ch == NULL) return;
  if (ch->thread != NULL) {
    // the thread ends with the output, which ends with the processes which
    // could write to it; one which escaped the job may keep it open
    if (WaitForSingleObject (ch->thread, 1000) != WAIT_OBJECT_0) {
      CancelSynchronousIo (ch->thread);
      if (WaitForSingleObject (ch->thread, 1000) != WAIT_OBJECT_0)
        return;  // (left to the thread, see above)
    }
    CloseHandle (ch->thread);
  }
  CloseHandle (ch->pipe);
  if (ch->news != NULL) CloseHandle (ch->news);
  DeleteCriticalSection (&ch->lock);
  delete ch;
}

/******************************************************************************
* The process
******************************************************************************/

struct windows_process_rep {
  HANDLE process;       // the child, NULL when none
  HANDLE job;           // its job, NULL if it could not be put in one
  HANDLE in;            // our end of its input, NULL once closed
  HANDLE news;          // event signaled by the channels
  DWORD  id;
  windows_channel* out;
  windows_channel* err; // NULL when the errors go to NUL
  std::string error;
  int    code;          // exit code of the last process, -1 if unknown
};

windows_process::windows_process () {
  rep= new windows_process_rep;
  rep->process= rep->job= rep->in= NULL;
  rep->news   = CreateEventW (NULL, FALSE, FALSE, NULL);
  rep->id     = 0;
  rep->out    = rep->err= NULL;
  rep->code   = -1;
}

windows_process::~windows_process () {
  terminate (0);
  if (rep->news != NULL) CloseHandle (rep->news);
  delete rep;
}

static void
close_handle (HANDLE& h) {
  if (h != NULL && h != INVALID_HANDLE_VALUE) CloseHandle (h);
  h= NULL;
}

bool
windows_process::start (const std::string& cmd, bool shell, bool keep_err) {
  terminate (0);
  rep->error= "";
  rep->code = -1;

  // the command line: the command interpreter, with /d (no AutoRun
  // commands of the registry) and /s /c "cmd" (the outer quotes removed,
  // what is between them left as it is)
  std::wstring app, line;
  if (shell) {
    app = command_interpreter ();
    line= L"\"" + app + L"\" /d /s /c \"" + utf8_to_wide (cmd) + L"\"";
  }
  else line= utf8_to_wide (cmd);
  std::vector<wchar_t> buf (line.begin (), line.end ());
  buf.push_back (0);

  SECURITY_ATTRIBUTES sa;
  sa.nLength= sizeof (sa);
  sa.lpSecurityDescriptor= NULL;
  sa.bInheritHandle= TRUE;
  HANDLE in_r= NULL, in_w= NULL, out_r= NULL, out_w= NULL;
  HANDLE err_r= NULL, err_w= NULL;
  bool ok= CreatePipe (&in_r, &in_w, &sa, 0) &&
           CreatePipe (&out_r, &out_w, &sa, 0);
  if (ok && keep_err) ok= CreatePipe (&err_r, &err_w, &sa, 0);
  else if (ok) {
    err_w= CreateFileW (L"NUL", GENERIC_WRITE, FILE_SHARE_READ |
                        FILE_SHARE_WRITE, &sa, OPEN_EXISTING, 0, NULL);
    ok= err_w != INVALID_HANDLE_VALUE;
    if (!ok) err_w= NULL;
  }
  // our ends are not inherited
  if (ok) ok= SetHandleInformation (in_w , HANDLE_FLAG_INHERIT, 0) &&
              SetHandleInformation (out_r, HANDLE_FLAG_INHERIT, 0) &&
              (err_r == NULL ||
               SetHandleInformation (err_r, HANDLE_FLAG_INHERIT, 0));
  if (!ok) {
    rep->error= error_message ("cannot make the pipes", GetLastError ());
    close_handle (in_r); close_handle (in_w); close_handle (out_r);
    close_handle (out_w); close_handle (err_r); close_handle (err_w);
    return false;
  }

  // only the ends of the child are inherited
  HANDLE inherited[3]= { in_r, out_w, err_w };
  SIZE_T size= 0;
  InitializeProcThreadAttributeList (NULL, 1, 0, &size);
  std::vector<char> attr_buf (size);
  LPPROC_THREAD_ATTRIBUTE_LIST attrs=
    (LPPROC_THREAD_ATTRIBUTE_LIST) attr_buf.data ();
  bool attrs_ok= InitializeProcThreadAttributeList (attrs, 1, 0, &size) &&
    UpdateProcThreadAttribute (attrs, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
                               inherited, sizeof (inherited), NULL, NULL);

  STARTUPINFOEXW si;
  ZeroMemory (&si, sizeof (si));
  si.StartupInfo.cb= sizeof (si);
  si.StartupInfo.dwFlags= STARTF_USESTDHANDLES;
  si.StartupInfo.hStdInput = in_r;
  si.StartupInfo.hStdOutput= out_w;
  si.StartupInfo.hStdError = err_w;
  DWORD flags= CREATE_NO_WINDOW | CREATE_SUSPENDED;
  if (attrs_ok) {
    si.lpAttributeList= attrs;
    flags |= EXTENDED_STARTUPINFO_PRESENT;
  }
  else si.StartupInfo.cb= sizeof (STARTUPINFOW);
  PROCESS_INFORMATION pi;
  ZeroMemory (&pi, sizeof (pi));
  BOOL created= CreateProcessW (app.empty ()? NULL: app.c_str (), buf.data (),
                                NULL, NULL, TRUE, flags, NULL, NULL,
                                &si.StartupInfo, &pi);
  DWORD e= GetLastError ();
  if (attrs_ok) DeleteProcThreadAttributeList (attrs);
  // the ends of the child are its own now
  close_handle (in_r); close_handle (out_w); close_handle (err_w);
  if (!created) {
    rep->error= error_message ("cannot start the process", e);
    close_handle (in_w); close_handle (out_r); close_handle (err_r);
    return false;
  }

  // the job, which ends the process and all it starts together
  rep->job= CreateJobObjectW (NULL, NULL);
  if (rep->job != NULL) {
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION li;
    ZeroMemory (&li, sizeof (li));
    li.BasicLimitInformation.LimitFlags= JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject (rep->job, JobObjectExtendedLimitInformation,
                                  &li, sizeof (li)) ||
        !AssignProcessToJobObject (rep->job, pi.hProcess))
      close_handle (rep->job);  // (the process alone is ended then)
  }
  ResumeThread (pi.hThread);
  CloseHandle (pi.hThread);

  rep->process= pi.hProcess;
  rep->id     = pi.dwProcessId;
  rep->in     = in_w;
  ResetEvent (rep->news);
  rep->out= open_channel (out_r, rep->news);
  rep->err= err_r == NULL? NULL: open_channel (err_r, rep->news);
  return true;
}

std::string
windows_process::error () {
  return rep->error;
}

bool
windows_process::running () {
  return rep->process != NULL &&
         WaitForSingleObject (rep->process, 0) == WAIT_TIMEOUT;
}

unsigned long
windows_process::pid () {
  return rep->process == NULL? 0: (unsigned long) rep->id;
}

int
windows_process::exit_code () {
  if (rep->process != NULL && !running ()) {
    DWORD code;
    if (GetExitCodeProcess (rep->process, &code)) rep->code= (int) code;
  }
  return rep->code;
}

/******************************************************************************
* Input and output
******************************************************************************/

bool
windows_process::write (const std::string& s) {
  // NOTE: blocks while the pipe is full (until the process reads)
  if (rep->in == NULL) return false;
  size_t done= 0;
  while (done < s.size ()) {
    DWORD n= 0;
    if (!WriteFile (rep->in, s.data () + done, (DWORD) (s.size () - done),
                    &n, NULL))
      return false;
    done += n;
  }
  return true;
}

void
windows_process::close_input () {
  close_handle (rep->in);
}

static bool
channel_take (windows_channel* ch, std::string& r) {
  // appends what was read, returns whether the pipe has ended
  if (ch == NULL) return true;
  EnterCriticalSection (&ch->lock);
  r.append (ch->buf);
  ch->buf.clear ();
  bool eof= ch->eof;
  LeaveCriticalSection (&ch->lock);
  return eof;
}

static bool
channel_ended (windows_channel* ch, bool& pending) {
  if (ch == NULL) return true;
  EnterCriticalSection (&ch->lock);
  if (!ch->buf.empty ()) pending= true;
  bool eof= ch->eof;
  LeaveCriticalSection (&ch->lock);
  return eof;
}

bool
windows_process::take (std::string& out, std::string& err) {
  // NOTE: the end is seen before what is taken: what a thread read before
  // the end is in its buffer by then
  bool end_out= channel_take (rep->out, out);
  bool end_err= channel_take (rep->err, err);
  return end_out && end_err;
}

bool
windows_process::ended () {
  bool pending= false;
  bool e1= channel_ended (rep->out, pending);
  bool e2= channel_ended (rep->err, pending);
  return e1 && e2;
}

bool
windows_process::wait (int msecs) {
  DWORD start= GetTickCount ();
  while (true) {
    bool pending= false;
    bool e1= channel_ended (rep->out, pending);
    bool e2= channel_ended (rep->err, pending);
    if (pending || (e1 && e2)) return true;
    DWORD spent= GetTickCount () - start;
    if (msecs >= 0 && spent >= (DWORD) msecs) return false;
    WaitForSingleObject (rep->news, msecs < 0? INFINITE: msecs - spent);
  }
}

/******************************************************************************
* The end
******************************************************************************/

void
windows_process::kill (unsigned code) {
  if (rep->process == NULL) return;
  if (rep->job != NULL) TerminateJobObject (rep->job, code);
  else if (running ()) TerminateProcess (rep->process, code);
}

void
windows_process::terminate (int grace) {
  close_input ();
  if (rep->process != NULL) {
    if (grace > 0) WaitForSingleObject (rep->process, grace);
    // what is left of the process and of all it started
    // (TerminateJobObject does nothing to those which have ended)
    if (rep->job != NULL) TerminateJobObject (rep->job, 1);
    else if (running ()) TerminateProcess (rep->process, 1);
    WaitForSingleObject (rep->process, 5000);
    (void) exit_code ();
    close_handle (rep->process);
    rep->id= 0;
  }
  close_channel (rep->out); rep->out= NULL;
  close_channel (rep->err); rep->err= NULL;
  close_handle (rep->job);
}

#endif // defined (OS_MINGW) && !defined (QTTEXMACS)
