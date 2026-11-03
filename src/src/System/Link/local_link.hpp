/******************************************************************************
* MODULE     : local_link.hpp
* DESCRIPTION: Loopback links between TeXmacs processes of one user
*              (shared files, see src/docs/shared-files.md)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LOCAL_LINK_H
#define LOCAL_LINK_H
#include "tm_link.hpp"

// A tm_link over a connected socket of 127.0.0.1: messages are tm_link
// packets (<length>\n<payload>). Nothing blocks: what the peer does not take
// yet is kept and written when the link is used again. The sockets are
// polled by the caller (Scheme: progs/utils/relate/shared-files.scm), which
// is cheap (a recv on each link), so that the TeXmacs server, its notifiers
// and its backoff polling are not involved.
tm_link make_local_link (int fd);

// Clients of the Scheme programs. A link is an integer, as for the language
// servers. local_listen binds the loopback only, on a port chosen by the
// system, and gives it (0 on failure).
int    local_listen ();
void   local_unlisten ();
int    local_accept ();                // the link of a new connection, or -1
int    local_connect (int port);       // the link, or -1
bool   local_send (int id, string s);  // false if the link is dead
string local_receive (int id);         // the next packet, or "" (none yet)
bool   local_alive (int id);
void   local_close (int id);

// Helpers of the shared files: the process, the per-user folder, paths
int    shared_process_id ();
bool   shared_process_alive (int pid);
string shared_real_path (string path);  // canonical path, "" if none
string shared_user_dir ();              // per-user folder (0700), "" if none
tree   shared_list_dir (string dir);    // the names in a folder (a tuple)
bool   shared_set_private (string path);// chmod 0600 on a file (not Windows)
bool   shared_create_file (string path, string s); // fails if it exists
bool   shared_replace_file (string path, string s);// atomically (tmp + rename)
string shared_random_token ();          // 32 hex digits from the system

#endif // LOCAL_LINK_H
