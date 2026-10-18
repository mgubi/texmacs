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
#include "hashmap.hpp"
#include "tm_ostream.hpp"
#include "file.hpp"
#include "ts_dynamic.h"
#include "scheme.hpp"
#include "tree-sitter/include/tree_sitter/api.h"
#include "tree-sitter/src/parser.h"
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

const TSLanguage*
tree_sitter_get_language (int language) {
  // the language of an index of tree-sitter-language-new, or NULL
  if (language < 0 || language >= N(ts_languages)) return NULL;
  return ts_languages[language];
}

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

int
tree_sitter_language_load (url file) {
  // the language of a file written by ts-export-language, or -1
  string data;
  if (load_string (file, data, false)) return -1;
  return tree_sitter_language_new (data);
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

static std::vector<int>
regex_bases (TSQuery* q) {
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
  return regex_base;
}

void
tree_sitter_query_run (int query, TSNode root, const char* text, array<int>& r,
                       int from= 0, int to= -1) {
  // append start, end (in bytes) and capture index of each capture of the
  // query under root, in the order of the text, after the predicates; only
  // the captures of the matches which meet the bytes from..to when to >= 0
  if (query < 0 || query >= N(ts_queries)) return;
  ts_query_entry* e= ts_queries[query];
  TSQuery* q= e->query;
  std::vector<int> regex_base= regex_bases (q);
  TSQueryCursor* c= ts_query_cursor_new ();
  if (to >= 0) ts_query_cursor_set_byte_range (c, (uint32_t) from, (uint32_t) to);
  ts_query_cursor_exec (c, q, root);
  TSQueryMatch m;
  uint32_t index;
  while (ts_query_cursor_next_capture (c, &m, &index)) {
    if (!satisfies_predicates (e, m, text, regex_base)) {
      ts_query_cursor_remove_match (c, m.id);
      continue;
    }
    TSQueryCapture cap= m.captures[index];
    r << (int) ts_node_start_byte (cap.node) << (int) ts_node_end_byte (cap.node)
      << (int) cap.index;
  }
  ts_query_cursor_delete (c);
}

static std::string
injection_property (TSQuery* q, uint32_t pattern) {
  // the language which #set! injection.language gives to a pattern, or ""
  uint32_t n, len;
  const TSQueryPredicateStep* steps= ts_query_predicates_for_pattern (q, pattern, &n);
  for (uint32_t i= 0; i + 2 < n; i++) {
    if (steps[i].type != TSQueryPredicateStepTypeString ||
        steps[i+1].type != TSQueryPredicateStepTypeString ||
        steps[i+2].type != TSQueryPredicateStepTypeString) continue;
    const char* op= ts_query_string_value_for_id (q, steps[i].value_id, &len);
    if (std::string (op, len) != "set!") continue;
    const char* key= ts_query_string_value_for_id (q, steps[i+1].value_id, &len);
    if (std::string (key, len) != "injection.language") continue;
    const char* v= ts_query_string_value_for_id (q, steps[i+2].value_id, &len);
    return std::string (v, len);
  }
  return std::string ();
}

void
tree_sitter_query_injections (int query, TSNode root, const char* text,
                              array<int>& ranges, array<string>& languages,
                              int from, int to) {
  // the injections of a query (injections.scm) under root, after the
  // predicates: the start and end (in bytes) of each @injection.content,
  // and its language, the text of @injection.language or the value of
  // #set! injection.language; only those which meet from..to when to >= 0
  if (query < 0 || query >= N(ts_queries)) return;
  ts_query_entry* e= ts_queries[query];
  TSQuery* q= e->query;
  int content= -1, language= -1;
  for (uint32_t i= 0; i < ts_query_capture_count (q); i++) {
    uint32_t len;
    const char* name= ts_query_capture_name_for_id (q, i, &len);
    std::string s (name, len);
    if (s == "injection.content") content= (int) i;
    if (s == "injection.language") language= (int) i;
  }
  if (content < 0) return;
  std::vector<int> regex_base= regex_bases (q);
  TSQueryCursor* c= ts_query_cursor_new ();
  if (to >= 0) ts_query_cursor_set_byte_range (c, (uint32_t) from, (uint32_t) to);
  ts_query_cursor_exec (c, q, root);
  TSQueryMatch m;
  while (ts_query_cursor_next_match (c, &m)) {
    if (!satisfies_predicates (e, m, text, regex_base)) continue;
    std::string lan= language < 0? std::string ():
                     capture_text (m, (uint32_t) language, text);
    if (lan.empty ()) lan= injection_property (q, m.pattern_index);
    if (lan.empty ()) continue;
    for (uint16_t i= 0; i < m.capture_count; i++)
      if ((int) m.captures[i].index == content) {
        ranges << (int) ts_node_start_byte (m.captures[i].node)
               << (int) ts_node_end_byte (m.captures[i].node);
        languages << string (lan.c_str (), (int) lan.size ());
      }
  }
  ts_query_cursor_delete (c);
}

array<int>
tree_sitter_query_captures (int query, string text) {
  // the captures of the query in the tree of text (see tree_sitter_query_run)
  array<int> r;
  if (query < 0 || query >= N(ts_queries)) return r;
  TSParser* p= ts_parser_new ();
  ts_parser_set_language (p, ts_languages[ts_queries[query]->language]);
  c_string s (text);
  TSTree* t= ts_parser_parse_string (p, NULL, s, (uint32_t) N(text));
  tree_sitter_query_run (query, ts_tree_root_node (t), s, r);
  ts_tree_delete (t);
  ts_parser_delete (p);
  return r;
}

int
tree_sitter_query_language (int query) {
  if (query < 0 || query >= N(ts_queries)) return -1;
  return ts_queries[query]->language;
}

/******************************************************************************
* External scanners written in Scheme
******************************************************************************/

/* The scanner of a language is a Scheme vector #(create scan deserialize
   serialize), as for progs/treesit/ts-parse.scm. The runtime calls the
   functions below, which call Scheme; while the Scheme scan runs, the
   lexer functions of ts-parse.scm act on the current TSLexer through the
   glue tree-sitter-lexer-... The state of the scanner is stored in the
   buffer of tree-sitter as the text of the Scheme value. As create has no
   argument, each language with a scanner has its own create function.

   A scan is called for each token where the parser accepts an external
   one, so the trampolines avoid the evaluation of Scheme strings: the
   vectors of the valid tokens are made once for each row of the table of
   the external states of the language (valid points into it), and the
   texts of the states are mapped back to the values which were serialized
   (states are values which are not modified afterwards), so that the
   reader is only needed for texts from elsewhere. */

struct ts_scanner_entry {
  object create, scan, deserialize, serialize;
  int external_count;
  const bool* states;          // the table of the external states
  array<object> valid;         // the vectors of its rows, made when needed
  hashmap<string,object> read; // the texts of the serialized states
};

static object
scanner_procedure (const char* name) {
  static hashmap<string,object> procs;
  if (!procs->contains (name)) procs (name)= eval (name);
  return procs [name];
}
static array<ts_scanner_entry*> ts_scanners;

struct ts_scanner_payload {
  ts_scanner_entry* scanner;
  object state;
  // the text of the state which the payload holds, when known: restoring
  // that text again (tree-sitter does it before each scan, usually with
  // the state just serialized) needs no call of Scheme
  bool known;
  string text;
};

static TSLexer* current_lexer= NULL;

static void*
scanner_create (int slot) {
  ts_scanner_payload* p= new ts_scanner_payload;
  p->scanner= ts_scanners[slot];
  p->state= call (p->scanner->create);
  p->known= false;
  return p;
}

#define SCANNER_SLOT(n) \
  static void* scanner_create_##n () { return scanner_create (n); }
SCANNER_SLOT(0) SCANNER_SLOT(1) SCANNER_SLOT(2) SCANNER_SLOT(3)
SCANNER_SLOT(4) SCANNER_SLOT(5) SCANNER_SLOT(6) SCANNER_SLOT(7)
SCANNER_SLOT(8) SCANNER_SLOT(9) SCANNER_SLOT(10) SCANNER_SLOT(11)
SCANNER_SLOT(12) SCANNER_SLOT(13) SCANNER_SLOT(14) SCANNER_SLOT(15)
SCANNER_SLOT(16) SCANNER_SLOT(17) SCANNER_SLOT(18) SCANNER_SLOT(19)
SCANNER_SLOT(20) SCANNER_SLOT(21) SCANNER_SLOT(22) SCANNER_SLOT(23)
SCANNER_SLOT(24) SCANNER_SLOT(25) SCANNER_SLOT(26) SCANNER_SLOT(27)
SCANNER_SLOT(28) SCANNER_SLOT(29) SCANNER_SLOT(30) SCANNER_SLOT(31)
#define SCANNER_SLOTS 32
static void* (*scanner_creates[SCANNER_SLOTS]) (void)= {
  scanner_create_0, scanner_create_1, scanner_create_2, scanner_create_3,
  scanner_create_4, scanner_create_5, scanner_create_6, scanner_create_7,
  scanner_create_8, scanner_create_9, scanner_create_10, scanner_create_11,
  scanner_create_12, scanner_create_13, scanner_create_14, scanner_create_15,
  scanner_create_16, scanner_create_17, scanner_create_18, scanner_create_19,
  scanner_create_20, scanner_create_21, scanner_create_22, scanner_create_23,
  scanner_create_24, scanner_create_25, scanner_create_26, scanner_create_27,
  scanner_create_28, scanner_create_29, scanner_create_30, scanner_create_31 };
// the slot of each language with a scanner, by its index
static hashmap<int,int> scanner_slots (-1);

static void
scanner_destroy (void* payload) {
  delete (ts_scanner_payload*) payload;
}

static bool
scanner_scan (void* payload, TSLexer* lexer, const bool* valid) {
  ts_scanner_payload* p= (ts_scanner_payload*) payload;
  static object c_lexer= symbol_object ("c-lexer");
  ts_scanner_entry* e= p->scanner;
  int n= e->external_count;
  int row= (valid == NULL || n == 0)? 0: (int) ((valid - e->states) / n);
  if (row >= N(e->valid) || is_null (e->valid[row])) {
    array<object> v;
    for (int i= 0; i < n; i++) v << object (valid != NULL && valid[i]);
    while (N(e->valid) <= row) e->valid << object ();
    e->valid[row]= call (scanner_procedure ("list->vector"), as_list_object (v));
  }
  TSLexer* old= current_lexer;
  current_lexer= lexer;
  p->known= false;   // the scan may change the state, even when it fails
  object r= call (e->scan, p->state, c_lexer, e->valid[row]);
  current_lexer= old;
  return as_bool (r);
}

static unsigned
scanner_serialize (void* payload, char* buffer) {
  ts_scanner_payload* p= (ts_scanner_payload*) payload;
  object v= call (p->scanner->serialize, p->state);
  if (is_bool (v) && !as_bool (v)) {
    p->known= true; p->text= "";
    return 0;
  }
  string s= as_string (call (scanner_procedure ("object->string"), v));
  if (N(s) > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) { p->known= false; return 0; }
  hashmap<string,object>& read= p->scanner->read;
  if (!read->contains (s)) {
    if (N(read) >= 4096) read= hashmap<string,object> ();
    read (s)= v;
  }
  for (int i= 0; i < N(s); i++) buffer[i]= s[i];
  p->known= true; p->text= s;
  return (unsigned) N(s);
}

static void
scanner_deserialize (void* payload, const char* buffer, unsigned length) {
  ts_scanner_payload* p= (ts_scanner_payload*) payload;
  if (p->known && N(p->text) == (int) length &&
      (length == 0 || memcmp (&(p->text[0]), buffer, length) == 0))
    return;
  object v= object (false);
  if (length != 0) {
    string s (buffer, (int) length);
    hashmap<string,object>& read= p->scanner->read;
    if (read->contains (s)) v= read [s];
    else v= call (scanner_procedure ("string->object"), s);
  }
  call (p->scanner->deserialize, p->state, v);
  p->known= true; p->text= string (buffer, (int) length);
}

bool
tree_sitter_unpack (url from, url to) {
  // write to the content of the xz file from (the tables shipped with
  // TeXmacs, see progs/treesit/ts-registry.scm)
  string s;
  if (load_string (from, s, false)) return false;
  c_string cs (s);
  unsigned char* out= NULL;
  long n= ts_xz_decompress ((const unsigned char*) (const char*) cs, (size_t) N(s), &out);
  if (n < 0) return false;
  string r ((char*) out, (int) n);
  free (out);
  return !save_string (to, r, false);
}

bool
tree_sitter_set_scanner (int language, object scanner) {
  // give a Scheme scanner to a language (before its first parse)
  if (language < 0 || language >= N(ts_languages)) return false;
  // a language which is given a scanner again keeps its slot
  int slot= scanner_slots[language];
  if (slot < 0 && N(ts_scanners) >= SCANNER_SLOTS) return false;
  TSLanguage* l= (TSLanguage*) ts_languages[language];
  ts_scanner_entry* e= new ts_scanner_entry;
  e->create= call ("vector-ref", scanner, object (0));
  e->scan= call ("vector-ref", scanner, object (1));
  e->deserialize= call ("vector-ref", scanner, object (2));
  e->serialize= call ("vector-ref", scanner, object (3));
  e->external_count= (int) l->external_token_count;
  e->states= l->external_scanner.states;
  if (slot < 0) {
    slot= N(ts_scanners);
    ts_scanners << e;
    scanner_slots (language)= slot;
  }
  else ts_scanners[slot]= e;
  l->external_scanner.create= scanner_creates[slot];
  l->external_scanner.destroy= scanner_destroy;
  l->external_scanner.scan= scanner_scan;
  l->external_scanner.serialize= scanner_serialize;
  l->external_scanner.deserialize= scanner_deserialize;
  return true;
}

int
tree_sitter_lexer_lookahead () {
  return current_lexer == NULL? 0: (int) current_lexer->lookahead;
}

void
tree_sitter_lexer_advance (bool skip) {
  if (current_lexer != NULL) current_lexer->advance (current_lexer, skip);
}

void
tree_sitter_lexer_mark_end () {
  if (current_lexer != NULL) current_lexer->mark_end (current_lexer);
}

void
tree_sitter_lexer_set_result (int symbol) {
  if (current_lexer != NULL) current_lexer->result_symbol= (TSSymbol) symbol;
}

int
tree_sitter_lexer_column () {
  return current_lexer == NULL? 0: (int) current_lexer->get_column (current_lexer);
}

bool
tree_sitter_lexer_eof () {
  return current_lexer == NULL || current_lexer->eof (current_lexer);
}

/* The loops over characters of the scanners (progs/treesit/ts-scan.scm):
   each advances over characters while they are in (or not in) a set, and
   gives their number. The sets are ASCII characters; the end of the text
   (lookahead 0) stops them. */

static int
lexer_advance_over (string chars, bool in, bool skip) {
  if (current_lexer == NULL) return 0;
  bool set[128];
  for (int i= 0; i < 128; i++) set[i]= false;
  for (int i= 0; i < N(chars); i++)
    if (((unsigned char) chars[i]) < 128) set[(int) chars[i]]= true;
  int n= 0;
  while (true) {
    int32_t c= current_lexer->lookahead;
    if (c == 0 || current_lexer->eof (current_lexer)) break;
    if ((c < 128 && set[c]) != in) break;
    current_lexer->advance (current_lexer, skip);
    n++;
  }
  return n;
}

int
tree_sitter_lexer_advance_while (string chars, bool skip) {
  return lexer_advance_over (chars, true, skip);
}

int
tree_sitter_lexer_advance_until (string chars, bool skip) {
  return lexer_advance_over (chars, false, skip);
}

static bool
lexer_space (int32_t c) {
  // as ts-wspace? in progs/treesit/ts-chars.scm
  return c == 32 || (c >= 9 && c <= 13) || c == 0x85 || c == 0xa0 ||
         c == 0x1680 || (c >= 0x2000 && c <= 0x200a) || c == 0x2028 ||
         c == 0x2029 || c == 0x202f || c == 0x205f || c == 0x3000;
}

int
tree_sitter_lexer_skip_spaces (bool skip, bool newlines) {
  // advances over white space, and over newlines if newlines
  if (current_lexer == NULL) return 0;
  int n= 0;
  while (true) {
    int32_t c= current_lexer->lookahead;
    if (!lexer_space (c) || (c == 10 && !newlines)) break;
    if (current_lexer->eof (current_lexer)) break;
    current_lexer->advance (current_lexer, skip);
    n++;
  }
  return n;
}
