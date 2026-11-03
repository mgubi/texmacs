/******************************************************************************
* MODULE     : windows_process.hpp
* DESCRIPTION: child processes with pipes on Windows, for cmdline_link
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WINDOWS_PROCESS_H
#define WINDOWS_PROCESS_H

// NOTE: no TeXmacs types and no <windows.h> here: the module is used by
// cmdline_link.cpp (where <windows.h> would clash with the names of
// TeXmacs) and can be tested on its own. Strings are in UTF-8.

#include <string>

struct windows_process_rep;

class windows_process {
  windows_process_rep* rep;
  windows_process (const windows_process&);            // not copied
  windows_process& operator= (const windows_process&); // not copied
public:
  windows_process ();
  ~windows_process ();  // terminate (0)

  // Starts the command: with shell, as a line of the command interpreter
  // (%ComSpec% /d /s /c "cmd"), else as a program and its arguments in
  // the conventions of Windows. Its standard input, output and error are
  // pipes; keep_err false gives it NUL as its error (as "2> /dev/null").
  // The process and all those it starts are in a job of their own, ended
  // by terminate and kill. Returns false if nothing could be started (see
  // error); a running process is terminated first.
  bool start (const std::string& cmd, bool shell, bool keep_err= true);
  std::string error ();     // why the last start failed

  bool running ();          // the process (not its children) runs
  unsigned long pid ();     // its number, 0 when none
  int  exit_code ();        // once ended, else -1

  // the input of the process: what is written goes to it, until it is
  // closed (the process then reads the end of its input)
  bool write (const std::string& s);
  void close_input ();

  // the output and errors of the process, read by threads in the
  // background, never blocking TeXmacs: take appends what was read since
  // the last time to out and err, and returns true once all of it was
  // taken (the process, and all which inherited them, closed its output
  // and error); wait returns true as soon as something is there to take
  // or the output has ended, false after msecs (-1: no limit); ended says
  // whether the output has ended (taken or not)
  bool take (std::string& out, std::string& err);
  bool wait (int msecs);
  bool ended ();

  // kill ends the process and all it started at once (the exit code being
  // code), as the user would by Ctrl+C; their output then ends.
  // terminate gives the process up to grace ms to end by itself (it has
  // closed its output), then ends what is left of it and of its children,
  // and frees all: pipes, threads, handles. The object can start again.
  void kill (unsigned code);
  void terminate (int grace);
};

#endif // WINDOWS_PROCESS_H
