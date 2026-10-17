/******************************************************************************
* MODULE     : tree_sitter_glue.cpp
* DESCRIPTION: tree-sitter languages generated in Scheme, run by the C runtime
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* The tables written by ts-export-language (progs/treesit/ts-export.scm)
   are given to ts_dynamic.c, which makes a TSLanguage of them; the parsing
   is done by the vendored runtime of tree-sitter. A language is known to
   Scheme by its index in ts_languages. */

#include "tree_sitter_glue.hpp"
#include "array.hpp"
#include "tm_ostream.hpp"
#include "ts_dynamic.h"
#include "tree-sitter/include/tree_sitter/api.h"
#include <stdlib.h>

static array<const TSLanguage*> ts_languages;

int
tree_sitter_language_new (string data) {
  c_string s (data);
  const char* error= NULL;
  const TSLanguage* l= tsdyn_language_new (s, N(data), &error);
  if (l == NULL) {
    if (error != NULL) std_error << "tree-sitter: " << error << "\n";
    return -1;
  }
  ts_languages << l;
  return N(ts_languages) - 1;
}

string
tree_sitter_parse_sexp (int language, string text) {
  // the S-expression of the tree of text (ts_node_string)
  if (language < 0 || language >= N(ts_languages)) return "";
  TSParser* p= ts_parser_new ();
  if (!ts_parser_set_language (p, ts_languages[language])) {
    ts_parser_delete (p);
    return "";
  }
  c_string s (text);
  TSTree* t= ts_parser_parse_string (p, NULL, s, (uint32_t) N(text));
  char* r= ts_node_string (ts_tree_root_node (t));
  string result (r);
  free (r);
  ts_tree_delete (t);
  ts_parser_delete (p);
  return result;
}
