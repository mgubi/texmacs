/******************************************************************************
* MODULE     : mcp_stdio.cpp
* DESCRIPTION: Standard input and output of `texmacs --mcp`
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mcp_stdio.hpp"
#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifndef OS_MINGW
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <sys/stat.h>
#endif

static int  mcp_in = -1;        // the protocol input (the original stdin)
static int  mcp_out= -1;        // the protocol output (the original stdout)
static bool mcp_eof= false;

void
mcp_stdio_setup (string log_file) {
#ifndef OS_MINGW
  if (mcp_out < 0) {
    mcp_in = dup (0);
    mcp_out= dup (1);
    if (mcp_in < 0 || mcp_out < 0) { mcp_eof= true; return; }
    fcntl (mcp_in , F_SETFD, FD_CLOEXEC);
    fcntl (mcp_out, F_SETFD, FD_CLOEXEC);
    // (a program started by TeXmacs must not read the protocol, nor write in it)
    int null= open ("/dev/null", O_RDONLY);
    if (null >= 0) { dup2 (null, 0); close (null); }
  }
  int log= -1;
  if (N(log_file) != 0) {
    char* tmp= as_charp (log_file);
    // (a log of more than 4 MB is kept as mcp.log.1, replacing the older one)
    struct stat st;
    if (stat (tmp, &st) == 0 && st.st_size > 4 * 1024 * 1024) {
      char* old= as_charp (log_file * ".1");
      rename (tmp, old);
      tm_delete_array (old);
    }
    log= open (tmp, O_WRONLY | O_CREAT | O_APPEND, 0600);
    tm_delete_array (tmp);
  }
  if (log < 0) log= open ("/dev/null", O_WRONLY);
  if (log >= 0) { dup2 (log, 1); dup2 (log, 2); close (log); }
  signal (SIGPIPE, SIG_IGN);
#else
  (void) log_file;
  mcp_eof= true;
#endif
}

bool
mcp_requested (int argc, char** argv) {
  for (int i= 1; i < argc; i++)
    if (strcmp (argv[i], "--mcp") == 0 || strcmp (argv[i], "-mcp") == 0)
      return true;
  return false;
}

bool
mcp_stdio_active () {
  return mcp_out >= 0;
}

string
mcp_stdin_read () {
#ifndef OS_MINGW
  if (mcp_in < 0 || mcp_eof) return "";
  struct pollfd p;
  p.fd= mcp_in; p.events= POLLIN; p.revents= 0;
  int r= poll (&p, 1, 0);
  if (r <= 0) return "";
  char buf[65536];
  ssize_t n= read (mcp_in, buf, sizeof (buf));
  if (n < 0) {
    if (errno == EINTR || errno == EAGAIN) return "";
    mcp_eof= true;
    return "";
  }
  if (n == 0) { mcp_eof= true; return ""; }
  return string (buf, (int) n);
#else
  return "";
#endif
}

bool
mcp_stdin_eof () {
  return mcp_eof;
}

bool
mcp_stdout_write (string s) {
#ifndef OS_MINGW
  if (mcp_out < 0) return false;
  int i= 0, n= N(s);
  while (i < n) {
    ssize_t w= write (mcp_out, &(s[i]), n - i);
    if (w < 0) {
      if (errno == EINTR) continue;
      if (errno == EAGAIN) {
        struct pollfd p;
        p.fd= mcp_out; p.events= POLLOUT; p.revents= 0;
        poll (&p, 1, 100);
        continue;
      }
      return false;       // the reader is gone
    }
    i += (int) w;
  }
  return true;
#else
  (void) s;
  return false;
#endif
}
