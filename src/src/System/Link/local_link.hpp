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
void   local_set_limit (int id, int bytes); // the longest packet of the link
bool   local_send_raw (int id, string s);   // bytes as they are (tests)
void   local_close (int id);

// Helpers of the shared files: the process, the per-user folder, paths
int    shared_process_id ();
int    shared_port_probe (int port);        // 1 accepts, 0 refused, -1 unknown (100 ms at most)
bool   shared_pid_alive (int pid);          // does a process with that id exist?
string shared_real_path (string path);  // canonical path, "" if none
string shared_user_dir ();              // per-user folder (0700), "" if none
tree   shared_list_dir (string dir);    // the names in a folder (a tuple)
bool   shared_set_private (string path);// chmod 0600 on a file (not Windows)
bool   shared_create_document (string path, string s); // like shared_create_file, mode 0666 & ~umask
bool   shared_lexists (string path);       // does the path exist in any form (a dangling link too)?
bool   shared_create_file (string path, string s); // fails if it exists
bool   shared_replace_file (string path, string s);// atomically (tmp + rename)
string shared_random_token ();
int    shared_lock_acquire (string path); // a handle; -1 if held, -2 on error
void   shared_lock_release (int id);
bool   shared_lock_valid (int id, string path);  // still the file at path?
int    shared_lock_renew (int id, string path);  // 1 made and locked, 0 held by another, -1 error
void   shared_touch (string path);          // 32 hex digits from the system

#endif // LOCAL_LINK_H
