
/******************************************************************************
* MODULE     : tm_poll_test.cpp
* DESCRIPTION: tests of tm_poll
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "sys_utils.hpp"
#ifndef OS_MINGW
#include <unistd.h>
#endif

// tm_poll reports the descriptors which are ready, however many are polled:
// of 70 pipes, only the last one has something to read
void
test_many_descriptors () {
#ifdef OS_MINGW
  SKIP ("no pipes for select on Windows");
#else
  const int n= 70;
  int ends[n][2];
  struct tm_pollfd fds[n];
  for (int i= 0; i < n; i++) {
    CHECK (pipe (ends[i]) == 0);
    fds[i].fd= ends[i][0];
    fds[i].events= TM_POLL_READ;
    fds[i].revents= -1;
  }
  CHECK (write (ends[n-1][1], "x", 1) == 1);
  CHECK_EQ (tm_poll (fds, n, 0), 1);
  CHECK_EQ (fds[n-1].revents, TM_POLL_READ);
  CHECK_EQ (fds[0].revents, 0);
  CHECK_EQ (fds[n-2].revents, 0);
  for (int i= 0; i < n; i++) {
    close (ends[i][0]);
    close (ends[i][1]);
  }
#endif
}

// the usual few descriptors
void
test_few_descriptors () {
#ifdef OS_MINGW
  SKIP ("no pipes for select on Windows");
#else
  int ends[2];
  CHECK (pipe (ends) == 0);
  struct tm_pollfd fds[1];
  fds[0].fd= ends[0];
  fds[0].events= TM_POLL_READ;
  CHECK_EQ (tm_poll (fds, 1, 0), 0);
  CHECK_EQ (fds[0].revents, 0);
  CHECK (write (ends[1], "x", 1) == 1);
  CHECK_EQ (tm_poll (fds, 1, 0), 1);
  CHECK_EQ (fds[0].revents, TM_POLL_READ);
  close (ends[0]);
  close (ends[1]);
#endif
}

int
main () {
  RUN (test_many_descriptors);
  RUN (test_few_descriptors);
  return test_report ();
}
