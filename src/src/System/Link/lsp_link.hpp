/******************************************************************************
* MODULE     : lsp_link.hpp
* DESCRIPTION: Links to language servers (Language Server Protocol)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LSP_LINK_H
#define LSP_LINK_H
#include "tm_link.hpp"

class object;

// The messages of a language server, framed by their headers, over any link
// (a pipe for now): write sends one message, read (LINK_OUT) gives the next
// complete one ("" if none), read (LINK_ERR) what the server wrote on stderr
tm_link make_lsp_link (tm_link transport);
string  lsp_frame (string msg);

// The clients of the Scheme programs. The call back of a client is called
// with a kind and a string: "message" and the JSON text of a message,
// "stderr" and what the server wrote there, "exit" and "" at its end
object lsp_start (string cmd, object call_back); // the id, or an error
bool   lsp_send (int id, string msg);
void   lsp_listen (int id, int msecs);
bool   lsp_alive (int id);
void   lsp_stop (int id);
void   process_all_lsp ();

#endif // LSP_LINK_H
