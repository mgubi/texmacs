
/******************************************************************************
* MODULE     : async_eval_test.cpp
* DESCRIPTION: tests of the asynchronous commands (async_eval_system)
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

int
main () {
  RUN (test_status_and_errors);
  RUN (test_long_output);
  RUN (test_kill);
  return test_report ();
}
