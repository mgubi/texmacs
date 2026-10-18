/******************************************************************************
* MODULE     : async_eval_test.cpp
* DESCRIPTION: the commands of a request link (the curl program) which are
*              no longer wanted write nothing into the next answer
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include <unistd.h>
#include "tm_timer.hpp"
#include "sys_utils.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"

// A request link without libcurl and without Qt 6 asks the curl program,
// whose output async_eval_system reads in a thread into the buffers of the
// link. An interrupted answer (request_link_rep::interrupt) is cancelled,
// then the next question uses the same buffers (request_link_rep::write):
// the slow command below stands for the interrupted curl, which goes on
// writing for a while; nothing of it may reach the next answer.


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

static void
test_interrupted_answer () {
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
}

int
main () {
  RUN (test_interrupted_answer);
  return test_report ();
}
