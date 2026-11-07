
/******************************************************************************
* MODULE     : async_eval_test.cpp
* DESCRIPTION: tests of the asynchronous commands (async_eval_system); the
*              commands of a request link (the curl program) which are
*              no longer wanted write nothing into the next answer
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "sys_utils.hpp"
#include "tm_timer.hpp"
#include "file.hpp"
#include "analyze.hpp"
#ifndef OS_MINGW
#include <unistd.h>
#endif

// the variant of a request link: status, output and errors are written by
// async_eval_pending once the command is done (it is called here as the
// main loop does); false if the command did not end within msecs
static bool
wait_command (int& status, int msecs) {
  time_t start= texmacs_time ();
  while (status > 0) {
    async_eval_pending ();
    if (status <= 0) break;
    if (texmacs_time () - start > msecs) return false;
#ifndef OS_MINGW
    usleep (10000);
#endif
  }
  return true;
}

// the exit code and the errors of the command reach the caller
void
test_status_and_errors () {
#ifdef OS_MINGW
  SKIP ("no sh on Windows");
#else
  int status= 1; string out, err; bool kill= false;
  CHECK (!async_eval_system ("echo out; echo err >&2; exit 3",
                             status, out, err, kill));
  CHECK (wait_command (status, 10000));
  CHECK_EQ (status, -3);
  CHECK_EQ (out, string ("out\n"));
  CHECK_EQ (err, string ("err\n"));
  status= 1;
  CHECK (!async_eval_system ("printf abc", status, out, err, kill));
  CHECK (wait_command (status, 10000));
  CHECK_EQ (status, 0);
  CHECK_EQ (out, string ("abc"));
  CHECK_EQ (err, string (""));
#endif
}

// a long output, written while the main loop looks at the handle
void
test_long_output () {
#ifdef OS_MINGW
  SKIP ("no sh on Windows");
#else
  int status= 1; string out, err; bool kill= false;
  CHECK (!async_eval_system ("i=0; while [ $i -lt 2000 ]; do "
                             "echo 0123456789012345678901234567890123456789; "
                             "echo e >&2; i=$((i+1)); done",
                             status, out, err, kill));
  CHECK (wait_command (status, 20000));
  CHECK_EQ (status, 0);
  CHECK_EQ (N(out), 2000 * 41);
  CHECK_EQ (N(err), 2000 * 2);
#endif
}

// a kill stops the command while it runs (with the commands of its shell),
// and nothing is written into the caller's buffers afterwards
void
test_kill () {
#ifdef OS_MINGW
  SKIP ("no kill on Windows");
#else
  // (the command would leave a mark after 2 s: once killed, it does not)
  url mark= url_temp (".mark");
  string cmd= "sleep 2; touch '" * as_string (mark) * "'; echo late";
  int status= 1; string out= "untouched", err; bool kill= false;
  time_t start= texmacs_time ();
  CHECK (!async_eval_system (cmd, status, out, err, kill));
  async_eval_pending ();
  kill= true;
  for (int i= 0; i < 300; i++) {
    async_eval_pending ();
    usleep (10000);
  }
  CHECK (!exists (mark));
  // status and output stay as they were
  CHECK_EQ (status, 1);
  CHECK_EQ (out, string ("untouched"));
  // a command started after it is not disturbed
  kill= false; status= 1;
  CHECK (!async_eval_system ("echo next", status, out, err, kill));
  CHECK (wait_command (status, 10000));
  CHECK_EQ (status, 0);
  CHECK_EQ (out, string ("next\n"));
  CHECK (texmacs_time () - start < 20000);
#endif
}

// A request link without libcurl and without Qt 6 asks the curl program,
// whose output async_eval_system reads in a thread into the buffers of the
// link. An interrupted answer (request_link_rep::interrupt) is cancelled,
// then the next question uses the same buffers (request_link_rep::write):
// the slow command below stands for the interrupted curl, which goes on
// writing for a while; nothing of it may reach the next answer.

#ifndef OS_MINGW
static bool
pump_until (bool (*ok) (int&, string&), int& status, string& out, int msecs) {
  time_t t0= texmacs_time ();
  while ((texmacs_time () - t0) < msecs) {
    async_eval_pending ();
    if (ok (status, out)) return true;
    usleep (20000);
  }
  return false;
}

static bool
started (int& status, string& out) {
  (void) status; return occurs ("OLD1", out); }

static bool
ended (int& status, string& out) {
  (void) out; return status == 0; }
#endif

static void
test_interrupted_answer () {
#ifdef OS_MINGW
  SKIP ("no sh on Windows");
#else
  int status= 1;
  string outbuf, errbuf;
  bool kill= false;
  CHECK (!async_eval_system ("sh -c 'printf OLD1; sleep 1; printf OLD2'",
                               status, outbuf, errbuf, kill));
  CHECK (pump_until (started, status, outbuf, 3000));
  // interrupted, as request_link_rep::interrupt does it
  kill= true;
  async_eval_cancel (&outbuf);
  // the next question, in the same buffers, as request_link_rep::write
  status= 1; outbuf= ""; errbuf= ""; kill= false;
  CHECK (!async_eval_system ("printf NEW", status, outbuf, errbuf, kill));
  CHECK (pump_until (ended, status, outbuf, 3000));
  CHECK_MSG (outbuf == "NEW", outbuf);
  // the interrupted command ends meanwhile: its output is dropped
  time_t t0= texmacs_time ();
  while ((texmacs_time () - t0) < 1500) { async_eval_pending (); usleep (20000); }
  CHECK_MSG (outbuf == "NEW", outbuf);
  CHECK_EQ (status, 0);
#endif
}

int
main () {
  RUN (test_status_and_errors);
  RUN (test_long_output);
  RUN (test_kill);
  RUN (test_interrupted_answer);
  return test_report ();
}
