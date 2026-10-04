
/******************************************************************************
* MODULE     : socket_notifier.cpp
* DESCRIPTION: Notifiers for socket activity
* COPYRIGHT  : (C) 2009 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// NOTE: commented out after creation of cmdline_link.cpp
// #ifndef QTTEXMACS

#include "socket_notifier.hpp"
#include "list.hpp"
#include "array.hpp"
#include "iterator.hpp"
#include "config.h"

#ifndef OS_MINGW
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif
#include <errno.h>

static hashset<socket_notifier> notifiers;

void
socket_notifier_rep::notify () {
  if (!is_nil (cmd)) cmd->apply ();
}

void
add_notifier (socket_notifier sn)  {
  //cout << "enable notifier " << LF;
  notifiers->insert (sn);
} 

void
remove_notifier (socket_notifier sn)  {
  //cout << "disable notifier " << LF;
  notifiers->remove (sn);
}

bool
notifiers_active () {
  return N(notifiers) > 0;
}

// Call the notifiers whose socket is ready (readable, or writable for the
// write notifiers), until nothing is ready or after a bounded number of
// rounds (a writable socket is ready as long as its notifier is kept: the
// socket links remove theirs once their output is flushed). The notifiers
// may add or remove notifiers while being called: iterate over a copy.
void 
perform_select () {
#ifndef OS_MINGW
  for (int rounds= 0; rounds < 64; rounds++) {
    fd_set rfds, wfds;
    FD_ZERO (&rfds);
    FD_ZERO (&wfds);
    int max_fd= 0;
    array<socket_notifier> current;
    iterator<socket_notifier> it = iterate (notifiers);
    while (it->busy ()) {
      socket_notifier sn= it->next ();
      if (sn->fd < 0 || sn->fd >= FD_SETSIZE) continue;
      current << sn;
      if (sn->write) FD_SET (sn->fd, &wfds);
      else FD_SET (sn->fd, &rfds);
      if (sn->fd >= max_fd) max_fd= sn->fd+1;
    }
    if (max_fd == 0) break;
    struct timeval tv;
    tv.tv_sec  = 0;
    tv.tv_usec = 0;
    int nr = select (max_fd, &rfds, &wfds, NULL, &tv);
    if (nr <= 0) break;
    for (int i= 0; i < N(current); i++) {
      socket_notifier sn= current[i];
      if (!notifiers->contains (sn)) continue; // removed by a previous notifier
      if (sn->write ? FD_ISSET (sn->fd, &wfds) : FD_ISSET (sn->fd, &rfds))
        sn->notify ();
    }
  }
#else
  io_error << "perform_select is not implemented";
#endif  
}

// NOTE: commented out after creation of cmdline_link.cpp
// #endif
