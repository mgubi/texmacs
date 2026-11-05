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
#include <ctype.h>
#include <errno.h>

#if !defined(OS_MINGW) && !defined(__EMSCRIPTEN__)
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <sys/stat.h>
#endif

static int  mcp_in = -1;        // the protocol input (the original stdin)
static int  mcp_out= -1;        // the protocol output (the original stdout)
static bool mcp_eof= false;
static string mcp_log_path;                 // the log file ("" if none)
static int  mcp_ticks= 0;
static off_t MCP_LOG_CAP= 4 * 1024 * 1024;   // (TEXMACS_MCP_LOG_CAP: the tests)

void
mcp_stdio_setup (string log_file) {
#if !defined(OS_MINGW) && !defined(__EMSCRIPTEN__)
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
    if (stat (tmp, &st) == 0 && st.st_size > MCP_LOG_CAP) {
      char* old= as_charp (log_file * ".1");
      rename (tmp, old);
      tm_delete_array (old);
    }
    log= open (tmp, O_WRONLY | O_CREAT | O_APPEND, 0600);
    tm_delete_array (tmp);
  }
  mcp_log_path= log_file;
  if (getenv ("TEXMACS_MCP_LOG_CAP") != NULL && atoi (getenv ("TEXMACS_MCP_LOG_CAP")) > 0)
    MCP_LOG_CAP= (off_t) atoi (getenv ("TEXMACS_MCP_LOG_CAP"));
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

#if !defined(OS_MINGW) && !defined(__EMSCRIPTEN__)
// A log which grew over the cap during the run is kept as mcp.log.1 (the
// older one is replaced) and a new one is started.
static bool mcp_rotate_failed= false;

static void
mcp_rotate_log () {
  if (N(mcp_log_path) == 0 || mcp_rotate_failed) return;
  struct stat st, sp;
  if (fstat (1, &st) != 0 || !S_ISREG (st.st_mode) || st.st_size <= MCP_LOG_CAP)
    return;
  char* cur= as_charp (mcp_log_path);
  char* old= as_charp (mcp_log_path * ".1");
  // Only the process whose descriptor is the file mcp.log renames it: if
  // another server (of the same home) has rotated it already, this one just
  // follows the new file.
  bool same= stat (cur, &sp) == 0 && sp.st_ino == st.st_ino && sp.st_dev == st.st_dev;
  if (same) {
    if (rename (cur, old) != 0) mcp_rotate_failed= true;
    else chmod (old, 0600);
  }
  if (!mcp_rotate_failed) {
    int log= open (cur, O_WRONLY | O_CREAT | O_APPEND, 0600);
    if (log >= 0) { dup2 (log, 1); dup2 (log, 2); close (log); }
  }
  tm_delete_array (cur);
  tm_delete_array (old);
}
#endif

static string
mcp_stdin_poll (int ms) {
#if !defined(OS_MINGW) && !defined(__EMSCRIPTEN__)
  if (mcp_in < 0 || mcp_eof) return "";
  struct pollfd p;
  p.fd= mcp_in; p.events= POLLIN; p.revents= 0;
  int r= poll (&p, 1, ms);
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
  (void) ms;
  return "";
#endif
}

string
mcp_stdin_read () {
#if !defined(OS_MINGW) && !defined(__EMSCRIPTEN__)
  if (++mcp_ticks % 64 == 0) mcp_rotate_log ();
#endif
  return mcp_stdin_poll (0);
}

string
mcp_stdin_wait (int ms) {
  // (waits in the system, no process, at most 200 ms at a time)
  if (ms < 0) ms= 0;
  if (ms > 200) ms= 200;
  return mcp_stdin_poll (ms);
}

bool
mcp_stdin_eof () {
  return mcp_eof;
}

bool
mcp_stdout_write (string s) {
#if !defined(OS_MINGW) && !defined(__EMSCRIPTEN__)
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

/******************************************************************************
* A strict JSON check
******************************************************************************/

#include <math.h>

namespace {
  struct json_check {
    const char* s;
    int n, i;
    json_check (const char* s2, int n2): s (s2), n (n2), i (0) {}
    void blank () {
      while (i < n && (s[i]==' ' || s[i]=='\t' || s[i]=='\n' || s[i]=='\r')) i++;
    }
    bool lit (const char* w) {
      int k= (int) strlen (w);
      if (i + k > n || strncmp (s + i, w, k) != 0) return false;
      i += k; return true;
    }
    bool str () {                         // at the opening quote
      i++;
      while (i < n) {
        unsigned char c= s[i++];
        if (c == '"') return true;
        if (c < 0x20) return false;          // (a raw control character)
        if (c == '\\') {
          if (i >= n) return false;
          char e= s[i++];
          if (e == 'u') {
            for (int k= 0; k < 4; k++, i++)
              if (i >= n || !isxdigit ((unsigned char) s[i])) return false;
          }
          else if (e == 0 || !strchr ("\"\\/bfnrt", e)) return false;
        }
      }
      return false;
    }
    bool number () {
      int b= i;
      if (i < n && s[i] == '-') i++;
      if (i >= n) return false;
      if (s[i] == '0') i++;
      else if (s[i] >= '1' && s[i] <= '9') while (i < n && isdigit ((unsigned char) s[i])) i++;
      else return false;
      if (i < n && s[i] == '.') {
        i++;
        if (i >= n || !isdigit ((unsigned char) s[i])) return false;
        while (i < n && isdigit ((unsigned char) s[i])) i++;
      }
      if (i < n && (s[i] == 'e' || s[i] == 'E')) {
        i++;
        if (i < n && (s[i] == '+' || s[i] == '-')) i++;
        if (i >= n || !isdigit ((unsigned char) s[i])) return false;
        while (i < n && isdigit ((unsigned char) s[i])) i++;
      }
      int len= i - b;
      char* buf= (char*) malloc (len + 1);
      if (buf == NULL) return false;
      memcpy (buf, s + b, len); buf[len]= '\0';
      bool ok= isfinite (strtod (buf, NULL));
      free (buf);
      return ok;
    }
    bool value (int depth) {
      blank ();
      if (i >= n) return false;
      char c= s[i];
      if (c == '"') return str ();
      if (c == 't') return lit ("true");
      if (c == 'f') return lit ("false");
      if (c == 'n') return lit ("null");
      if (c == '{' || c == '[') {
        if (depth >= 512) return false;
        char close= (c == '{')? '}': ']';
        i++; blank ();
        if (i < n && s[i] == close) { i++; return true; }
        while (true) {
          blank ();
          if (c == '{') {
            if (i >= n || s[i] != '"' || !str ()) return false;
            blank ();
            if (i >= n || s[i] != ':') return false;
            i++;
          }
          if (!value (depth + 1)) return false;
          blank ();
          if (i >= n) return false;
          if (s[i] == ',') { i++; continue; }
          if (s[i] == close) { i++; return true; }
          return false;
        }
      }
      return number ();
    }
  };
}

static bool
valid_utf8 (const unsigned char* s, int n) {
  int i= 0;
  while (i < n) {
    unsigned char c= s[i];
    int k= 0;
    if (c < 0x80) { i++; continue; }
    else if (c >= 0xC2 && c <= 0xDF) k= 1;
    else if (c >= 0xE0 && c <= 0xEF) k= 2;
    else if (c >= 0xF0 && c <= 0xF4) k= 3;
    else return false;
    if (i + k >= n) return false;
    for (int j= 1; j <= k; j++)
      if ((s[i+j] & 0xC0) != 0x80) return false;
    if (c == 0xE0 && s[i+1] < 0xA0) return false;      // overlong
    if (c == 0xED && s[i+1] >= 0xA0) return false;      // surrogates
    if (c == 0xF0 && s[i+1] < 0x90) return false;
    if (c == 0xF4 && s[i+1] >= 0x90) return false;
    i += k + 1;
  }
  return true;
}

bool
mcp_json_valid (string t) {
  if (N(t) == 0) return false;
  if (!valid_utf8 ((const unsigned char*) &(t[0]), N(t))) return false;
  json_check c (&(t[0]), N(t));
  c.blank ();
  if (c.i >= c.n || (c.s[c.i] != '{' && c.s[c.i] != '[')) return false;
  if (!c.value (0)) return false;
  c.blank ();
  return c.i == c.n;
}
