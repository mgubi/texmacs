/******************************************************************************
* MODULE     : syntax_region.cpp
* DESCRIPTION: the tree-sitter syntax trees of the code regions of documents
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "syntax_region.hpp"
#include "converter.hpp"
#include "hashmap.hpp"
#include "tree-sitter/include/tree_sitter/api.h"
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <vector>

const TSLanguage* tree_sitter_get_language (int language);
void tree_sitter_query_run (int query, TSNode root, const char* text,
                            array<int>& r);

/******************************************************************************
* Decoding the TeXmacs encoding
******************************************************************************/

/* The parser reads the lines as they are: Cork bytes and entities <name>.
   The decode function gives one code point for one Cork byte or for one
   entity, so that the byte offsets of tree-sitter are positions in the
   TeXmacs strings. */

static int32_t cork_table[256];
static bool cork_table_ready= false;
static hashmap<string,int> entity_table (-1);

static void
init_cork_table () {
  for (int c= 0; c < 256; c++) {
    if (c < 128) { cork_table[c]= c; continue; }
    string u= cork_to_utf8 (string ((char) c));
    int i= 0;
    cork_table[c]= (N(u) > 0)? (int32_t) decode_from_utf8 (u, i): 0xFFFD;
  }
  cork_table_ready= true;
}

static int32_t
entity_code_point (string name) {
  // name is <...>
  if (entity_table->contains (name)) return entity_table[name];
  int32_t r= 0xFFFD;
  if (N(name) > 3 && name[1] == '#') {
    r= 0;
    for (int i= 2; i < N(name) - 1; i++) {
      char c= name[i];
      int d= (c >= '0' && c <= '9')? c - '0':
             (c >= 'a' && c <= 'f')? c - 'a' + 10:
             (c >= 'A' && c <= 'F')? c - 'A' + 10: -1;
      if (d < 0) { r= 0xFFFD; break; }
      r= 16 * r + d;
    }
  }
  else {
    string u= cork_to_utf8 (name);
    if (N(u) > 0 && u != name) {
      int i= 0;
      r= (int32_t) decode_from_utf8 (u, i);
    }
  }
  entity_table (name)= r;
  return r;
}

static uint32_t
texmacs_decode (const uint8_t* s, uint32_t length, int32_t* code_point) {
  if (length == 0) { *code_point= -1; return 0; }
  if (s[0] == '<') {
    uint32_t j= 1;
    while (j < length && j < 64 && s[j] != '>' && s[j] != '<' && s[j] != '\n') j++;
    if (j < length && s[j] == '>' && j > 1) {
      *code_point= entity_code_point (string ((const char*) s, (int) j + 1));
      return j + 1;
    }
  }
  *code_point= cork_table[s[0]];
  return 1;
}

/******************************************************************************
* Regions
******************************************************************************/

struct syntax_region_rep {
  tree root;
  int language;
  array<string> lines;
  array<int> offsets;   // start of each line, and the total length + 1
  TSParser* parser;
  TSTree* tree;
  int full_parses, incremental_parses;
  int version;                 // changes at each parse
  int hl_query, hl_version;    // the colours below are for these
  array<array<int> > hl_cols;  // colour per position, per line
};

static array<syntax_region_rep*> regions;
static const int max_regions= 32;

static void
flatten (tree t, string& r) {
  if (is_atomic (t)) r << t->label;
  else for (int i= 0; i < N(t); i++) flatten (t[i], r);
}

static array<string>
region_lines (tree t) {
  array<string> r;
  if (is_atomic (t)) r << t->label;
  else if (is_document (t))
    for (int i= 0; i < N(t); i++) {
      if (is_atomic (t[i])) r << t[i]->label;
      else { string s; flatten (t[i], s); r << s; }
    }
  else { string s; flatten (t, s); r << s; }
  return r;
}

static array<int>
line_offsets (array<string> lines) {
  array<int> r;
  int pos= 0;
  for (int i= 0; i < N(lines); i++) { r << pos; pos += N(lines[i]) + 1; }
  r << pos;
  return r;
}

static const char newline[]= "\n";

static const char*
region_read (void* payload, uint32_t byte, TSPoint p, uint32_t* bytes_read) {
  (void) p;
  syntax_region_rep* r= (syntax_region_rep*) payload;
  int n= N(r->lines);
  if (n == 0 || (int) byte >= r->offsets[n] - 1) { *bytes_read= 0; return ""; }
  // the line of the byte
  int lo= 0, hi= n - 1;
  while (lo < hi) {
    int mid= (lo + hi + 1) / 2;
    if (r->offsets[mid] <= (int) byte) lo= mid; else hi= mid - 1;
  }
  int col= (int) byte - r->offsets[lo];
  string& s= r->lines[lo];
  if (col < N(s)) {
    *bytes_read= (uint32_t) (N(s) - col);
    return &(s[col]);
  }
  *bytes_read= 1;
  return newline;
}

static TSPoint
point_of (array<int>& offsets, int byte) {
  int n= N(offsets) - 1;
  int lo= 0, hi= n - 1;
  if (n <= 0) return TSPoint { 0, 0 };
  while (lo < hi) {
    int mid= (lo + hi + 1) / 2;
    if (offsets[mid] <= byte) lo= mid; else hi= mid - 1;
  }
  return TSPoint { (uint32_t) lo, (uint32_t) (byte - offsets[lo]) };
}

static void
region_parse (syntax_region_rep* r, TSTree* old) {
  TSInput input= { r, region_read, TSInputEncodingCustom, texmacs_decode };
  TSTree* t= ts_parser_parse (r->parser, old, input);
  if (old != NULL) ts_tree_delete (old);
  r->tree= t;
  r->version++;
}

static string
segment (array<string>& lines, int a, int b) {
  // the lines a..b-1, each followed by a newline
  string s;
  for (int i= a; i < b; i++) s << lines[i] << "\n";
  return s;
}

static void
region_update (syntax_region_rep* r) {
  array<string> cur= region_lines (r->root);
  array<string>& old= r->lines;
  int n0= N(old), n1= N(cur);
  int a= 0;
  while (a < n0 && a < n1 && old[a] == cur[a]) a++;
  if (a == n0 && a == n1) return;
  int b= 0;
  while (b < n0 - a && b < n1 - a && old[n0-1-b] == cur[n1-1-b]) b++;
  // the changed lines, with the newlines which follow them (a virtual
  // newline after the last line, the same in both texts)
  string os= segment (old, a, n0 - b), ns= segment (cur, a, n1 - b);
  int p= 0;
  while (p < N(os) && p < N(ns) && os[p] == ns[p]) p++;
  int s= 0;
  while (s < N(os) - p && s < N(ns) - p && os[N(os)-1-s] == ns[N(ns)-1-s]) s++;
  array<int> new_offsets= line_offsets (cur);
  int base= r->offsets[a];
  int start= base + p, old_end= base + N(os) - s, new_end= base + N(ns) - s;
  TSInputEdit e;
  e.start_byte= (uint32_t) start;
  e.old_end_byte= (uint32_t) old_end;
  e.new_end_byte= (uint32_t) new_end;
  e.start_point= point_of (r->offsets, start);
  e.old_end_point= point_of (r->offsets, old_end);
  e.new_end_point= point_of (new_offsets, new_end);
  ts_tree_edit (r->tree, &e);
  r->lines= cur;
  r->offsets= new_offsets;
  region_parse (r, r->tree);
  r->incremental_parses++;
}

static void
region_delete (syntax_region_rep* r) {
  if (r->tree != NULL) ts_tree_delete (r->tree);
  ts_parser_delete (r->parser);
  tm_delete (r);
}

static syntax_region_rep*
get_region (tree root, int language) {
  // the region of root, parsed and up to date, or NULL
  const TSLanguage* lang= tree_sitter_get_language (language);
  if (lang == NULL) return NULL;
  if (!cork_table_ready) init_cork_table ();
  for (int i= 0; i < N(regions); i++)
    if (strong_equal (regions[i]->root, root)) {
      syntax_region_rep* r= regions[i];
      // most recently used first
      for (int j= i; j > 0; j--) regions[j]= regions[j-1];
      regions[0]= r;
      if (r->language != language) {
        ts_parser_set_language (r->parser, lang);
        ts_tree_delete (r->tree);
        r->language= language;
        r->lines= region_lines (root);
        r->offsets= line_offsets (r->lines);
        r->tree= NULL;
        region_parse (r, NULL);
        r->full_parses++;
      }
      else region_update (r);
      return r;
    }
  syntax_region_rep* r= tm_new<syntax_region_rep> ();
  r->root= root;
  r->language= language;
  r->lines= region_lines (root);
  r->offsets= line_offsets (r->lines);
  r->parser= ts_parser_new ();
  r->tree= NULL;
  r->full_parses= 1;
  r->incremental_parses= 0;
  r->version= 0;
  r->hl_query= -1;
  r->hl_version= -1;
  ts_parser_set_language (r->parser, lang);
  region_parse (r, NULL);
  if (N(regions) >= max_regions) {
    region_delete (regions[N(regions) - 1]);
    regions->resize (N(regions) - 1);
  }
  regions << r;
  for (int j= N(regions) - 1; j > 0; j--) regions[j]= regions[j-1];
  regions[0]= r;
  return r;
}

void
syntax_region_forget (tree root) {
  for (int i= 0; i < N(regions); i++)
    if (strong_equal (regions[i]->root, root)) {
      region_delete (regions[i]);
      for (int j= i; j + 1 < N(regions); j++) regions[j]= regions[j+1];
      regions->resize (N(regions) - 1);
      return;
    }
}

/******************************************************************************
* Requests
******************************************************************************/

string
syntax_region_sexp (tree root, int language) {
  syntax_region_rep* r= get_region (root, language);
  if (r == NULL) return "";
  char* s= ts_node_string (ts_tree_root_node (r->tree));
  string result (s);
  free (s);
  return result;
}

static object
point_object (TSPoint p) {
  return list_object (object ((int) p.row), object ((int) p.column));
}

static object
node_object (TSNode n, const char* field) {
  // (type named? (row column) (row column) field)
  array<object> l;
  l << object (string (ts_node_type (n)));
  l << object (ts_node_is_named (n));
  l << point_object (ts_node_start_point (n));
  l << point_object (ts_node_end_point (n));
  l << (field == NULL? object (false): object (string (field)));
  return as_list_object (l);
}

object
syntax_region_node_at (tree root, int language, int row, int column) {
  // the nodes around a position, innermost first
  syntax_region_rep* r= get_region (root, language);
  array<object> l;
  if (r == NULL) return as_list_object (l);
  TSPoint p= { (uint32_t) row, (uint32_t) column };
  TSNode top= ts_tree_root_node (r->tree);
  TSNode n= ts_node_descendant_for_point_range (top, p, p);
  while (!ts_node_is_null (n)) {
    TSNode parent= ts_node_parent (n);
    const char* field= NULL;
    if (!ts_node_is_null (parent)) {
      for (uint32_t i= 0; i < ts_node_child_count (parent); i++)
        if (ts_node_eq (ts_node_child (parent, i), n)) {
          field= ts_node_field_name_for_child (parent, i);
          break;
        }
    }
    l << node_object (n, field);
    n= parent;
  }
  return as_list_object (l);
}

static void
collect_errors (TSNode n, array<object>& l) {
  if (ts_node_is_error (n) || ts_node_is_missing (n)) {
    l << list_object (object (string (ts_node_is_missing (n)? "missing": "error")),
                      point_object (ts_node_start_point (n)),
                      point_object (ts_node_end_point (n)));
    if (ts_node_is_missing (n)) return;
  }
  if (!ts_node_has_error (n)) return;
  for (uint32_t i= 0; i < ts_node_child_count (n); i++)
    collect_errors (ts_node_child (n, i), l);
}

object
syntax_region_errors (tree root, int language) {
  // (kind (row column) (row column)) for each error and missing node
  syntax_region_rep* r= get_region (root, language);
  array<object> l;
  if (r != NULL) collect_errors (ts_tree_root_node (r->tree), l);
  return as_list_object (l);
}

array<int>
syntax_region_statistics (tree root, int language) {
  // the number of full and of incremental parses of the region
  array<int> a;
  syntax_region_rep* r= get_region (root, language);
  if (r != NULL) a << r->full_parses << r->incremental_parses;
  return a;
}

/******************************************************************************
* Colours of the lines
******************************************************************************/

struct ts_capture { int start, end, color; };

static bool
capture_less (const ts_capture& a, const ts_capture& b) {
  // in the order of the text, and the outer node first
  if (a.start != b.start) return a.start < b.start;
  return a.end > b.end;
}

array<array<int> >
syntax_region_highlight (tree root, int language, int query, array<int> colors) {
  // the colour codes of each position of each line, for the query with the
  // colour code of each capture (0: no colour). As in tree-sitter-highlight,
  // the last pattern which captures a node gives its colour (none when its
  // capture has no colour), and an inner node is painted over an outer one.
  // Nodes are told apart by their ranges.
  syntax_region_rep* r= get_region (root, language);
  if (r == NULL) return array<array<int> > ();
  if (r->hl_query == query && r->hl_version == r->version) return r->hl_cols;
  string text;
  for (int i= 0; i < N(r->lines); i++) {
    if (i > 0) text << "\n";
    text << r->lines[i];
  }
  array<int> caps;
  c_string ctext (text);
  tree_sitter_query_run (query, ts_tree_root_node (r->tree), ctext, caps);
  std::vector<ts_capture> v;
  for (int i= 0; i + 2 < N(caps); i += 3) {
    int c= caps[i+2];
    int col= (c >= 0 && c < N(colors))? colors[c]: 0;
    v.push_back (ts_capture { caps[i], caps[i+1], col });
  }
  std::stable_sort (v.begin (), v.end (), capture_less);
  array<int> paint (N(text));
  for (int i= 0; i < N(text); i++) paint[i]= 0;
  for (size_t k= 0; k < v.size (); k++) {
    if (k + 1 < v.size () && v[k].start == v[k+1].start && v[k].end == v[k+1].end)
      continue;
    if (v[k].color == 0) continue;
    int a= max (v[k].start, 0), b= min (v[k].end, N(text));
    for (int i= a; i < b; i++) paint[i]= v[k].color;
  }
  array<array<int> > cols;
  for (int i= 0; i < N(r->lines); i++) {
    int n= N(r->lines[i]), o= r->offsets[i];
    array<int> c (n);
    for (int j= 0; j < n; j++) c[j]= paint[o + j];
    cols << c;
  }
  r->hl_query= query;
  r->hl_version= r->version;
  r->hl_cols= cols;
  return cols;
}
