/******************************************************************************
* MODULE     : tree_sitter_glue.hpp
* DESCRIPTION: tree-sitter languages generated in Scheme, run by the C runtime
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_SITTER_GLUE_H
#define TREE_SITTER_GLUE_H
#include "string.hpp"
#include "array.hpp"
#include "url.hpp"
#include "scheme.hpp"

int    tree_sitter_language_new (string data);
int    tree_sitter_language_load (url file);
string tree_sitter_parse_sexp (int language, string text);
int    tree_sitter_query_new (int language, string source);
array<string> tree_sitter_query_capture_names (int query);
array<int>    tree_sitter_query_captures (int query, string text);
bool tree_sitter_set_scanner (int language, object scanner);
int  tree_sitter_lexer_lookahead ();
void tree_sitter_lexer_advance (bool skip);
void tree_sitter_lexer_mark_end ();
void tree_sitter_lexer_set_result (int symbol);
int  tree_sitter_lexer_column ();
bool tree_sitter_lexer_eof ();

#endif // TREE_SITTER_GLUE_H
