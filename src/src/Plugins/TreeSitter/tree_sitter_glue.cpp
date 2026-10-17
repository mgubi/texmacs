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
#include <string.h>
#include <regex>
#include <string>
#include <vector>

static array<const TSLanguage*> ts_languages;

struct ts_query_entry {
  int language;
  TSQuery* query;
  std::vector<std::regex*> regexes;   // one per #match? step, in order
};
static array<ts_query_entry*> ts_queries;

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

/******************************************************************************
* Queries
******************************************************************************/

int
tree_sitter_query_new (int language, string source) {
  // the index of the query, or -1 (with the position of the error)
  if (language < 0 || language >= N(ts_languages)) return -1;
  c_string s (source);
  uint32_t offset;
  TSQueryError error;
  TSQuery* q= ts_query_new (ts_languages[language], s, (uint32_t) N(source),
                            &offset, &error);
  if (q == NULL) {
    std_error << "tree-sitter: error " << (int) error << " in the query at "
              << (int) offset << "\n";
    return -1;
  }
  ts_query_entry* e= new ts_query_entry;
  e->language= language;
  e->query= q;
  // compile the regular expressions of the predicates once
  for (uint32_t p= 0; p < ts_query_pattern_count (q); p++) {
    uint32_t n;
    const TSQueryPredicateStep* steps= ts_query_predicates_for_pattern (q, p, &n);
    for (uint32_t i= 0; i < n; i++) {
      if (steps[i].type != TSQueryPredicateStepTypeString || i + 2 >= n) continue;
      uint32_t len;
      const char* op= ts_query_string_value_for_id (q, steps[i].value_id, &len);
      std::string name (op, len);
      if ((name == "match?" || name == "not-match?") &&
          steps[i+2].type == TSQueryPredicateStepTypeString) {
        const char* re= ts_query_string_value_for_id (q, steps[i+2].value_id, &len);
        std::regex* r= NULL;
        try { r= new std::regex (std::string (re, len)); }
        catch (...) { r= NULL; }
        e->regexes.push_back (r);
      }
    }
  }
  ts_queries << e;
  return N(ts_queries) - 1;
}

array<string>
tree_sitter_query_capture_names (int query) {
  array<string> r;
  if (query < 0 || query >= N(ts_queries)) return r;
  TSQuery* q= ts_queries[query]->query;
  for (uint32_t i= 0; i < ts_query_capture_count (q); i++) {
    uint32_t len;
    const char* name= ts_query_capture_name_for_id (q, i, &len);
    r << string (name, (int) len);
  }
  return r;
}

static std::string
capture_text (const TSQueryMatch& m, uint32_t capture, const char* text) {
  for (uint16_t i= 0; i < m.capture_count; i++)
    if (m.captures[i].index == capture) {
      uint32_t a= ts_node_start_byte (m.captures[i].node);
      uint32_t b= ts_node_end_byte (m.captures[i].node);
      return std::string (text + a, b - a);
    }
  return std::string ();
}

static bool
satisfies_predicates (ts_query_entry* e, const TSQueryMatch& m, const char* text,
                      const std::vector<int>& regex_base) {
  // the text predicates #eq? #match? #any-of? and their negations; the other
  // predicates (#set!, #is?...) are directives which do not filter
  TSQuery* q= e->query;
  uint32_t n;
  const TSQueryPredicateStep* steps= ts_query_predicates_for_pattern (q, m.pattern_index, &n);
  int regex= regex_base[m.pattern_index];
  uint32_t i= 0;
  while (i < n) {
    uint32_t j= i;
    while (j < n && steps[j].type != TSQueryPredicateStepTypeDone) j++;
    if (j > i && steps[i].type == TSQueryPredicateStepTypeString) {
      uint32_t len;
      const char* op= ts_query_string_value_for_id (q, steps[i].value_id, &len);
      std::string name (op, len);
      bool negate= name.compare (0, 4, "not-") == 0;
      std::string base= negate? name.substr (4): name;
      if (j - i >= 3 && steps[i+1].type == TSQueryPredicateStepTypeCapture) {
        std::string left= capture_text (m, steps[i+1].value_id, text);
        bool ok= true;
        if (base == "eq?") {
          std::string right;
          if (steps[i+2].type == TSQueryPredicateStepTypeCapture)
            right= capture_text (m, steps[i+2].value_id, text);
          else {
            const char* v= ts_query_string_value_for_id (q, steps[i+2].value_id, &len);
            right= std::string (v, len);
          }
          ok= (left == right);
        }
        else if (base == "match?") {
          std::regex* r= e->regexes[regex++];
          ok= (r != NULL) && std::regex_search (left, *r);
        }
        else if (base == "any-of?") {
          ok= false;
          for (uint32_t k= i + 2; k < j; k++) {
            const char* v= ts_query_string_value_for_id (q, steps[k].value_id, &len);
            if (left == std::string (v, len)) ok= true;
          }
        }
        else { i= j + 1; continue; }
        if (ok == negate) return false;
      }
    }
    i= j + 1;
  }
  return true;
}

array<int>
tree_sitter_query_captures (int query, string text) {
  // start, end (in bytes) and capture index of each capture of the query in
  // the tree of text, in the order of the text, after the predicates
  array<int> r;
  if (query < 0 || query >= N(ts_queries)) return r;
  ts_query_entry* e= ts_queries[query];
  TSQuery* q= e->query;
  // the index of the first regular expression of each pattern
  std::vector<int> regex_base;
  int count= 0;
  for (uint32_t p= 0; p < ts_query_pattern_count (q); p++) {
    regex_base.push_back (count);
    uint32_t n;
    const TSQueryPredicateStep* steps= ts_query_predicates_for_pattern (q, p, &n);
    for (uint32_t i= 0; i + 2 < n; i++) {
      if (steps[i].type != TSQueryPredicateStepTypeString) continue;
      uint32_t len;
      const char* op= ts_query_string_value_for_id (q, steps[i].value_id, &len);
      std::string name (op, len);
      if ((name == "match?" || name == "not-match?") &&
          steps[i+2].type == TSQueryPredicateStepTypeString) count++;
    }
  }
  TSParser* p= ts_parser_new ();
  ts_parser_set_language (p, ts_languages[e->language]);
  c_string s (text);
  TSTree* t= ts_parser_parse_string (p, NULL, s, (uint32_t) N(text));
  TSQueryCursor* c= ts_query_cursor_new ();
  ts_query_cursor_exec (c, q, ts_tree_root_node (t));
  TSQueryMatch m;
  uint32_t index;
  while (ts_query_cursor_next_capture (c, &m, &index)) {
    if (!satisfies_predicates (e, m, s, regex_base)) {
      ts_query_cursor_remove_match (c, m.id);
      continue;
    }
    TSQueryCapture cap= m.captures[index];
    r << (int) ts_node_start_byte (cap.node) << (int) ts_node_end_byte (cap.node)
      << (int) cap.index;
  }
  ts_query_cursor_delete (c);
  ts_tree_delete (t);
  ts_parser_delete (p);
  return r;
}
