/******************************************************************************
* MODULE     : mcp_stdio.hpp
* DESCRIPTION: Standard input and output of `texmacs --mcp` (see
*              src/docs/mcp-server.md)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MCP_STDIO_H
#define MCP_STDIO_H
#include "string.hpp"

// In `texmacs --mcp` the standard output carries the protocol and nothing
// else: mcp_stdio_setup keeps the original descriptors 0 and 1 for the
// protocol and sends descriptors 1 and 2 (so everything that TeXmacs and the
// libraries print) to the log file. It is called before anything is printed
// (with no file at first, then, when the home is known, with the log file).
// The rest is polled from Scheme (progs/agents/mcp-server.scm) on the main
// thread; nothing blocks.
void   mcp_stdio_setup (string log_file);
bool   mcp_stdio_active ();
bool   mcp_requested (int argc, char** argv); // is --mcp on the command line?

string mcp_stdin_read ();       // the bytes available now, or ""
string mcp_stdin_wait (int ms); // the same, waiting at most ms milliseconds for them
bool   mcp_stdin_eof ();        // the end of the input was reached
bool   mcp_stdout_write (string s); // all of s on the protocol output

// Is s one JSON object or array, strictly (literals, numbers that fit a
// double, strings and their escapes, brackets), nested at most 512 deep,
// with nothing after it? The reader of the protocol (json->typed-tree) is
// lenient and crashes on a nesting of hundreds of thousands of levels.
bool   mcp_json_valid (string s);

#endif // MCP_STDIO_H
