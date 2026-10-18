/******************************************************************************
* MODULE     : syntax_region.cpp
* DESCRIPTION: the tree-sitter syntax trees of the code regions of documents
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "code_lines.hpp"
#include "syntax_region.hpp"
#include "ts_captures.hpp"
#include "language.hpp"
#include "converter.hpp"
#include "hashmap.hpp"
#include "tree-sitter/include/tree_sitter/api.h"
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <vector>

const TSLanguage* tree_sitter_get_language (int language);
void tree_sitter_query_run (int query, TSNode root, const char* text,
                            array<int>& r, int from= 0, int to= -1);
void tree_sitter_query_injections (int query, TSNode root, const char* text,
                                   array<int>& ranges, array<string>& languages,
                                   int from= 0, int to= -1);
void tree_sitter_query_node_captures (int query, TSNode root, const char* text,
                                      ts_node_captures& r);

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
  int hl_query, hl_injections, hl_epoch;  // the colours below are for these
  array<array<int> > hl_cols;  // colour per position, per line (or empty)
  array<bool> hl_dirty;        // the lines whose colours are out of date
  int ind_query, ind_version;  // the captures below are for these
  ts_node_captures ind_caps;   // of the query of indentation
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
  TSTree* old_tree= ts_tree_copy (r->tree);
  region_parse (r, r->tree);
  r->incremental_parses++;
  // the colours: those of the unchanged lines are kept, the changed lines
  // and those where the syntax changed (a comment opened above them...)
  // are out of date
  if (N(r->hl_cols) == n0) {
    array<array<int> > cols;
    array<bool> dirty;
    for (int i= 0; i < a; i++) { cols << r->hl_cols[i]; dirty << r->hl_dirty[i]; }
    for (int i= a; i < n1 - b; i++) { cols << array<int> (); dirty << true; }
    for (int i= n0 - b; i < n0; i++) { cols << r->hl_cols[i]; dirty << r->hl_dirty[i]; }
    uint32_t count;
    TSRange* ranges= ts_tree_get_changed_ranges (old_tree, r->tree, &count);
    for (uint32_t k= 0; k < count; k++) {
      int r1= (int) ranges[k].start_point.row;
      int r2= min ((int) ranges[k].end_point.row, n1 - 1);
      for (int i= max (r1, 0); i <= r2; i++) dirty[i]= true;
    }
    free (ranges);
    r->hl_cols= cols;
    r->hl_dirty= dirty;
  }
  ts_tree_delete (old_tree);
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
        r->hl_cols= array<array<int> > ();
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
  r->ind_query= -1;
  r->ind_version= -1;
  r->hl_injections= -1;
  r->hl_epoch= -1;
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
  return syntax_region_node_range (root, language, row, column, row, column);
}

object
syntax_region_node_range (tree root, int language,
                          int row1, int col1, int row2, int col2) {
  // the nodes which contain a range, innermost first
  syntax_region_rep* r= get_region (root, language);
  array<object> l;
  if (r == NULL) return as_list_object (l);
  TSPoint p1= { (uint32_t) row1, (uint32_t) col1 };
  TSPoint p2= { (uint32_t) row2, (uint32_t) col2 };
  TSNode top= ts_tree_root_node (r->tree);
  TSNode n= ts_node_descendant_for_point_range (top, p1, p2);
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

object
syntax_region_last_nodes (tree root, int language) {
  // the root, its last child, the last child of that one..., outermost
  // first: where the text ends (an empty block of Python is found there)
  syntax_region_rep* r= get_region (root, language);
  array<object> l;
  if (r == NULL) return as_list_object (l);
  TSNode n= ts_tree_root_node (r->tree);
  while (!ts_node_is_null (n)) {
    l << node_object (n, NULL);
    uint32_t k= ts_node_child_count (n);
    if (k == 0) break;
    n= ts_node_child (n, k - 1);
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

object
syntax_region_captures (tree root, int language, int query) {
  // the captures of a query in the region, in the order of the text, after
  // the predicates: (capture (row column) (row column))
  syntax_region_rep* r= get_region (root, language);
  array<object> l;
  if (r == NULL) return as_list_object (l);
  string text;
  for (int i= 0; i < N(r->lines); i++) {
    if (i > 0) text << "\n";
    text << r->lines[i];
  }
  array<int> caps;
  c_string ctext (text);
  tree_sitter_query_run (query, ts_tree_root_node (r->tree), ctext, caps);
  for (int i= 0; i + 2 < N(caps); i += 3)
    l << list_object (object (caps[i+2]),
                      point_object (point_of (r->offsets, caps[i])),
                      point_object (point_of (r->offsets, caps[i+1])));
  return as_list_object (l);
}

static void
paint_captures (array<int>& caps, array<int>& colors, array<int>& paint,
                int offset, int end) {
  // paint the captures (start, end, capture index) of a query, shifted by
  // offset, between max (offset, 0) and end (paint has end positions). As in tree-sitter-highlight, the last pattern which
  // captures a node gives its colour (none when its capture has no colour),
  // and an inner node is painted over an outer one. Nodes are told apart
  // by their ranges.
  std::vector<ts_capture> v;
  for (int i= 0; i + 2 < N(caps); i += 3) {
    int c= caps[i+2];
    int col= (c >= 0 && c < N(colors))? colors[c]: 0;
    v.push_back (ts_capture { caps[i] + offset, caps[i+1] + offset, col });
  }
  std::stable_sort (v.begin (), v.end (), capture_less);
  for (size_t k= 0; k < v.size (); k++) {
    if (k + 1 < v.size () && v[k].start == v[k+1].start && v[k].end == v[k+1].end)
      continue;
    if (v[k].color == 0) continue;
    int a= max (v[k].start, max (offset, 0)), b= min (v[k].end, end);
    for (int i= a; i < b; i++) paint[i]= v[k].color;
  }
}

struct text_input { const char* s; uint32_t n; };

static const char*
text_read (void* payload, uint32_t byte, TSPoint p, uint32_t* bytes_read) {
  (void) p;
  text_input* t= (text_input*) payload;
  if (byte >= t->n) { *bytes_read= 0; return ""; }
  *bytes_read= t->n - byte;
  return t->s + byte;
}

static void
paint_injection (const char* text, int start, int end, int language,
                 int query, array<int>& colors, array<int>& paint,
                 int from, int to) {
  // the text start..end parsed in another language and painted with its
  // highlight query, where it meets from..to (paint holds these positions)
  const TSLanguage* lang= tree_sitter_get_language (language);
  if (lang == NULL || start >= end) return;
  TSParser* p= ts_parser_new ();
  if (!ts_parser_set_language (p, lang)) { ts_parser_delete (p); return; }
  text_input in= { text + start, (uint32_t) (end - start) };
  TSInput input= { &in, text_read, TSInputEncodingCustom, texmacs_decode };
  TSTree* t= ts_parser_parse (p, NULL, input);
  if (t != NULL) {
    // the predicates read the text of the injection, from its start
    string sub (text + start, end - start);
    c_string csub (sub);
    array<int> caps;
    tree_sitter_query_run (query, ts_tree_root_node (t), csub, caps,
                           max (from - start, 0), min (to, end) - start);
    // (an injection which starts before from has a negative offset)
    paint_captures (caps, colors, paint, start - from, min (end, to) - from);
    ts_tree_delete (t);
  }
  ts_parser_delete (p);
}

static bool
bracket_type (const char* t) {
  return t[0] != '\0' && t[1] == '\0' && strchr ("()[]{}", t[0]) != NULL;
}

static char
opening_bracket (char c) {
  return c == ')'? '(': c == ']'? '[': c == '}'? '{': '\0';
}

static void
paint_node (TSNode n, array<int>& paint, int from, int to, int code) {
  int a= max ((int) ts_node_start_byte (n), from);
  int b= min ((int) ts_node_end_byte (n), to);
  for (int i= a; i < b; i++) paint[i - from]= code;
}

static void
paint_unmatched (TSNode n, array<int>& paint, int from, int to, int code) {
  // paint in the colour code the brackets which error recovery left
  // unmatched: those directly in an ERROR node, and the opening bracket of
  // a node where a closing one is MISSING (only the nodes with errors are
  // visited)
  if (!ts_node_has_error (n)) return;
  if ((int) ts_node_end_byte (n) < from || (int) ts_node_start_byte (n) > to) return;
  bool err= ts_node_is_error (n);
  uint32_t k= ts_node_child_count (n);
  for (uint32_t i= 0; i < k; i++) {
    TSNode c= ts_node_child (n, i);
    const char* type= ts_node_type (c);
    if (ts_node_is_missing (c) && bracket_type (type)) {
      char open= opening_bracket (type[0]);
      for (uint32_t j= 0; j < i && open != '\0'; j++) {
        TSNode o= ts_node_child (n, j);
        const char* t= ts_node_type (o);
        if (!ts_node_is_named (o) && t[0] == open && t[1] == '\0') {
          paint_node (o, paint, from, to, code);
          break;
        }
      }
    }
    else if (err && !ts_node_is_named (c) && bracket_type (type))
      paint_node (c, paint, from, to, code);
    else paint_unmatched (c, paint, from, to, code);
  }
}

array<array<int> >
syntax_region_highlight (tree root, int language, int query, array<int> colors,
                         int injections, ts_injection_resolver resolve,
                         int epoch, array<int>* recoloured) {
  // the colour codes of each position of each line, for the query with the
  // colour code of each capture (0: no colour; see paint_captures); then
  // the injections (a query of injections.scm, or -1): the ranges which it
  // gives in other languages, coloured by the highlighters of these
  // languages (resolve, which epoch changes) over the colours of the region.
  // After an edit, only the lines whose colours are out of date are
  // coloured again (the query runs on their bytes); their rows are appended
  // to recoloured, when given.
  syntax_region_rep* r= get_region (root, language);
  if (r == NULL) return array<array<int> > ();
  int n= N(r->lines);
  if (r->hl_query != query || r->hl_injections != injections ||
      r->hl_epoch != epoch || N(r->hl_cols) != n) {
    r->hl_cols= array<array<int> > (n);
    r->hl_dirty= array<bool> (n);
    for (int i= 0; i < n; i++) r->hl_dirty[i]= true;
    r->hl_query= query;
    r->hl_injections= injections;
    r->hl_epoch= epoch;
  }
  int first= 0, last= n - 1;
  while (first < n && !r->hl_dirty[first]) first++;
  while (last >= first && !r->hl_dirty[last]) last--;
  if (first > last) return r->hl_cols;
  string text;
  for (int i= 0; i < n; i++) {
    if (i > 0) text << "\n";
    text << r->lines[i];
  }
  int from= r->offsets[first];
  int to= r->offsets[last] + N(r->lines[last]);
  array<int> caps;
  c_string ctext (text);
  TSNode top= ts_tree_root_node (r->tree);
  tree_sitter_query_run (query, top, ctext, caps, from, to);
  array<int> paint (to - from);
  for (int i= 0; i < to - from; i++) paint[i]= 0;
  paint_captures (caps, colors, paint, -from, to - from);
  if (injections >= 0 && resolve != NULL) {
    array<int> ranges;
    array<string> names;
    tree_sitter_query_injections (injections, top, ctext, ranges, names, from, to);
    for (int k= 0; k < N(names); k++) {
      int lan2, query2;
      array<int> colors2;
      if (resolve (names[k], lan2, query2, colors2))
        paint_injection (ctext, ranges[2*k], min (ranges[2*k+1], N(text)),
                         lan2, query2, colors2, paint, from, to);
    }
  }
  paint_unmatched (top, paint, from, to, encode_color ("error"));
  for (int i= first; i <= last; i++) {
    int m= N(r->lines[i]), o= r->offsets[i] - from;
    array<int> c (m);
    for (int j= 0; j < m; j++) c[j]= paint[o + j];
    r->hl_cols[i]= c;
    r->hl_dirty[i]= false;
    if (recoloured != NULL) (*recoloured) << i;
  }
  return r->hl_cols;
}

/******************************************************************************
* Errors after an insertion
******************************************************************************/

static int
count_errors (TSNode n) {
  if (!ts_node_has_error (n)) return 0;
  int r= (ts_node_is_error (n) || ts_node_is_missing (n))? 1: 0;
  uint32_t k= ts_node_child_count (n);
  for (uint32_t i= 0; i < k; i++) r += count_errors (ts_node_child (n, i));
  return r;
}

int
syntax_region_errors_with (tree root, int language, int row, int col, string ins) {
  // the number of ERROR and MISSING nodes of the region if ins were
  // inserted at the point (the tree of the region is reparsed in a copy,
  // incrementally); -1 without a tree
  syntax_region_rep* r= get_region (root, language);
  if (r == NULL || row < 0 || row >= N(r->lines)) return -1;
  if (N(ins) == 0) return count_errors (ts_tree_root_node (r->tree));
  int pos= r->offsets[row] + min (max (col, 0), N(r->lines[row]));
  string text;
  for (int i= 0; i < N(r->lines); i++) {
    if (i > 0) text << "\n";
    text << r->lines[i];
  }
  text= text (0, pos) * ins * text (pos, N(text));
  array<string> new_lines;
  int last= 0;
  for (int i= 0; i <= N(text); i++)
    if (i == N(text) || text[i] == '\n') { new_lines << text (last, i); last= i + 1; }
  array<int> new_offsets= line_offsets (new_lines);
  TSTree* t= ts_tree_copy (r->tree);
  TSInputEdit e;
  e.start_byte= e.old_end_byte= (uint32_t) pos;
  e.new_end_byte= (uint32_t) (pos + N(ins));
  e.start_point= e.old_end_point= point_of (r->offsets, pos);
  e.new_end_point= point_of (new_offsets, pos + N(ins));
  ts_tree_edit (t, &e);
  TSParser* p= ts_parser_new ();
  ts_parser_set_language (p, tree_sitter_get_language (language));
  c_string ctext (text);
  text_input in= { (const char*) ctext, (uint32_t) N(text) };
  TSInput input= { &in, text_read, TSInputEncodingCustom, texmacs_decode };
  TSTree* t2= ts_parser_parse (p, t, input);
  int n= t2 == NULL? -1: count_errors (ts_tree_root_node (t2));
  if (t2 != NULL) ts_tree_delete (t2);
  ts_tree_delete (t);
  ts_parser_delete (p);
  return n;
}

/******************************************************************************
* Moves between the nodes
******************************************************************************/

static bool
point_before (TSPoint a, TSPoint b) {
  return a.row < b.row || (a.row == b.row && a.column < b.column);
}

static bool
point_equal (TSPoint a, TSPoint b) {
  return a.row == b.row && a.column == b.column;
}

array<int>
syntax_region_node_move (tree root, int language, int row, int col, int dir) {
  // the start (row column) of the node where a structured move goes from
  // the point: the next (dir 1) or previous (-1) named sibling of the
  // largest named node which starts at the point (or of an ancestor, when
  // there is none; the previous of a point inside a node is its start), the
  // first ancestor which starts before the point (2), the first named
  // descendant which starts after it (-2); empty when there is none
  array<int> r;
  syntax_region_rep* reg= get_region (root, language);
  if (reg == NULL) return r;
  TSNode top= ts_tree_root_node (reg->tree);
  TSPoint p= { (uint32_t) row, (uint32_t) col };
  TSNode node= ts_node_named_descendant_for_point_range (top, p, p);
  if (ts_node_is_null (node)) return r;
  TSNode target= { { 0, 0, 0, 0 }, NULL, NULL };
  if (dir == 1 || dir == -1) {
    if (point_equal (ts_node_start_point (node), p))
      while (true) {
        TSNode up= ts_node_parent (node);
        if (ts_node_is_null (up) || ts_node_is_null (ts_node_parent (up)) ||
            !point_equal (ts_node_start_point (up), p)) break;
        node= up;
      }
    if (dir == -1 && point_before (ts_node_start_point (node), p))
      target= node;
    else
      for (TSNode n= node; !ts_node_is_null (n); n= ts_node_parent (n)) {
        TSNode s= dir == 1? ts_node_next_named_sibling (n):
                            ts_node_prev_named_sibling (n);
        if (!ts_node_is_null (s)) { target= s; break; }
      }
  }
  else if (dir == 2) {
    for (TSNode n= ts_node_parent (node); !ts_node_is_null (n); n= ts_node_parent (n))
      if (point_before (ts_node_start_point (n), p)) { target= n; break; }
  }
  else if (dir == -2) {
    // the first named node after the point, inside the node at the point
    TSNode n= node;
    while (!ts_node_is_null (n) && ts_node_is_null (target)) {
      uint32_t k= ts_node_named_child_count (n);
      TSNode next= { { 0, 0, 0, 0 }, NULL, NULL };
      for (uint32_t i= 0; i < k; i++) {
        TSNode c= ts_node_named_child (n, i);
        if (point_before (p, ts_node_start_point (c))) { target= c; break; }
        if (!point_before (ts_node_end_point (c), p) &&
            point_before (p, ts_node_end_point (c))) { next= c; break; }
      }
      n= next;
    }
  }
  if (ts_node_is_null (target)) return r;
  TSPoint s= ts_node_start_point (target);
  r << (int) s.row << (int) s.column;
  return r;
}

static void
push_range (array<int>& r, TSNode n) {
  // the start and end (row column row column) of n, or -1 four times
  if (ts_node_is_null (n)) { r << -1 << -1 << -1 << -1; return; }
  TSPoint a= ts_node_start_point (n), b= ts_node_end_point (n);
  r << (int) a.row << (int) a.column << (int) b.row << (int) b.column;
}

static bool
encloses_with_brackets (TSNode n) {
  // does n start with an opening bracket and end with the matching one?
  uint32_t k= ts_node_child_count (n);
  if (k < 2) return false;
  TSNode a= ts_node_child (n, 0), b= ts_node_child (n, k - 1);
  const char* ta= ts_node_type (a);
  const char* tb= ts_node_type (b);
  return !ts_node_is_named (a) && !ts_node_is_named (b) && !ts_node_is_missing (b) &&
         bracket_type (ta) && bracket_type (tb) && opening_bracket (tb[0]) == ta[0];
}

array<int>
syntax_region_node_info (tree root, int language, int row, int col) {
  // the ranges around the point, for the structural edits: the node at the
  // point (the largest named node which starts there, or else the named
  // leaf which contains it), its next and previous named siblings, its
  // named parent (not the root), the innermost node around the point which
  // is a pair of brackets, and the largest named node which ends at the
  // point; empty without a tree
  array<int> r;
  syntax_region_rep* reg= get_region (root, language);
  if (reg == NULL) return r;
  TSNode top= ts_tree_root_node (reg->tree);
  TSPoint p= { (uint32_t) row, (uint32_t) col };
  TSNode none= { { 0, 0, 0, 0 }, NULL, NULL };
  TSNode node= ts_node_named_descendant_for_point_range (top, p, p);
  if (!ts_node_is_null (node) && point_equal (ts_node_start_point (node), p))
    while (true) {
      TSNode up= ts_node_parent (node);
      if (ts_node_is_null (up) || ts_node_is_null (ts_node_parent (up)) ||
          !point_equal (ts_node_start_point (up), p)) break;
      node= up;
    }
  else if (!ts_node_is_null (node) && ts_node_named_child_count (node) != 0)
    node= none;   // inside a node with parts, but at none of them
  bool real= !ts_node_is_null (node) && !ts_node_is_null (ts_node_parent (node));
  push_range (r, real? node: none);
  push_range (r, real? ts_node_next_named_sibling (node): none);
  push_range (r, real? ts_node_prev_named_sibling (node): none);
  TSNode parent= real? ts_node_parent (node): none;
  while (!ts_node_is_null (parent) && !ts_node_is_named (parent))
    parent= ts_node_parent (parent);
  if (!ts_node_is_null (parent) && ts_node_is_null (ts_node_parent (parent)))
    parent= none;
  push_range (r, parent);
  TSNode pair= none;
  for (TSNode n= ts_node_descendant_for_point_range (top, p, p);
       !ts_node_is_null (n); n= ts_node_parent (n))
    if (encloses_with_brackets (n) && point_before (ts_node_start_point (n), p) &&
        point_before (p, ts_node_end_point (n))) { pair= n; break; }
  push_range (r, pair);
  TSNode before= none;
  if (col > 0) {
    TSPoint q= { (uint32_t) row, (uint32_t) (col - 1) };
    TSNode n= ts_node_named_descendant_for_point_range (top, q, q);
    if (!ts_node_is_null (n) && point_equal (ts_node_end_point (n), p)) {
      while (true) {
        TSNode up= ts_node_parent (n);
        if (ts_node_is_null (up) || ts_node_is_null (ts_node_parent (up)) ||
            !point_equal (ts_node_end_point (up), p)) break;
        n= up;
      }
      if (!ts_node_is_null (ts_node_parent (n))) before= n;
    }
  }
  push_range (r, before);
  return r;
}

/******************************************************************************
* Indentation by the queries of Neovim (indents.scm)
******************************************************************************/

/* A port of get_indent of nvim-treesitter (lua/nvim-treesitter/indent.lua):
   the captures @indent.begin, .end, .branch, .dedent, .align, .auto,
   .ignore and .zero of the query, with the properties indent.immediate,
   indent.start_at_same_line, indent.open_delimiter, indent.close_delimiter,
   indent.increment and indent.avoid_last_matching_next. Rows and columns
   are those of the lines of the region (positions in the TeXmacs strings). */

// (the positions and columns are those of the core, System/Language/
// code_lines.cpp: a tab of a source file is the character <#9>, which goes
// to the next stop of every indent_tab columns, the step of the indentation
// asked for, which is the width of a tab where the lines are indented by
// tabs, see program-indent-step)
static int indent_tab= 8;

static int
indent_columns (string s) {
  // the offset of the first character of @s which is not blank
  return code_indent_end (s);
}

static int
indent_width (string s) {
  // the column of the first character of @s which is not blank
  return code_column (s, code_indent_end (s), indent_tab);
}

static string
trimmed (string s) {
  int a= code_indent_end (s), b= N(s);
  while (b > a && (s[b-1] == ' ' || s[b-1] == '\t' ||
                   (b - 4 >= a && is_source_tab (s, b-4))))
    b -= (s[b-1] == '>'? 4: 1);
  return s (a, b);
}

static int
char_column (string line, int col) {
  // the column of a position of a TeXmacs string in characters: an entity
  // <name> counts as one (as get-offset of prog/scheme-edit.scm), a tab
  // goes to its stop
  return code_column (line, col, indent_tab);
}

static bool
point_less (TSPoint a, TSPoint b) {
  return a.row < b.row || (a.row == b.row && a.column < b.column);
}

static ts_properties* captured (ts_node_captures& q, const char* name, TSNode n);

static int
lisp_indent (array<string>& lines, TSNode top, ts_node_captures& q, int row) {
  // the indentation of Lisp of prog/scheme-edit.scm on the tree: in the
  // innermost list (@indent.lisp) around the line, with the elements which
  // come before it, the first being the operator of arity i (indent.arity,
  // from the keywords of TeXmacs) or none: aligned with the reference
  // argument (the first element on the line of the last one, but not the
  // operator unless it is alone), or, for a keyword, the column of the
  // operator plus 3 for its first i arguments and plus 1 for the next one
  TSPoint p= { (uint32_t) row, 0 };
  TSNode n= ts_node_descendant_for_point_range (top, p, p);
  ts_properties* lisp= NULL;
  while (!ts_node_is_null (n)) {
    lisp= captured (q, "indent.lisp", n);
    if (lisp != NULL && point_less (ts_node_start_point (n), p)) break;
    lisp= NULL;
    n= ts_node_parent (n);
  }
  if (lisp == NULL) return 0;
  array<TSNode> elems;
  uint32_t k= ts_node_named_child_count (n);
  for (uint32_t i= 0; i < k; i++) {
    TSNode c= ts_node_named_child (n, i);
    if (std::string (ts_node_type (c)).find ("comment") != std::string::npos) continue;
    if ((int) ts_node_start_point (c).row >= row) break;
    elems << c;
  }
  int nr= N(elems);
  if (nr == 0) return 0;
  // the reference argument
  int j= nr - 1;
  while (j > 1 && ts_node_start_point (elems[j-1]).row == ts_node_start_point (elems[j]).row)
    j--;
  TSNode a= elems[j];
  auto offset= [&] (TSNode e) {
    TSPoint s= ts_node_start_point (e);
    return char_column (lines[s.row], (int) s.column);
  };
  if (!lisp->count ("indent.arity")) return offset (a);
  int arity= atoi ((*lisp)["indent.arity"].c_str ());
  if (nr <= arity) return offset (elems[0]) + 3;
  if (nr == arity + 1) return offset (elems[0]) + 1;
  return offset (a);
}

static TSNode
node_at (TSNode root, int row, int col) {
  TSPoint a= { (uint32_t) row, (uint32_t) max (col, 0) };
  TSPoint b= { (uint32_t) row, (uint32_t) max (col, 0) + 1 };
  return ts_node_descendant_for_point_range (root, a, b);
}

static ts_properties*
captured (ts_node_captures& q, const char* name, TSNode n) {
  // the properties of n as a node of the capture name, or NULL
  if (ts_node_is_null (n)) return NULL;
  auto it= q.find (name);
  if (it == q.end ()) return NULL;
  auto jt= it->second.find (n.id);
  return jt == it->second.end ()? NULL: &(jt->second);
}

static bool
find_delimiter (array<string>& lines, TSNode node, const std::string& delim,
                TSNode& child, bool& is_end) {
  // the child of type delim, and whether only spaces and delim follow it on
  // its line
  uint32_t n= ts_node_child_count (node);
  for (uint32_t i= 0; i < n; i++) {
    TSNode c= ts_node_child (node, i);
    if (std::string (ts_node_type (c)) != delim) continue;
    child= c;
    TSPoint e= ts_node_end_point (c);
    int row= (int) ts_node_start_point (c).row;
    string line= row < N(lines)? lines[row]: string ("");
    is_end= true;
    for (int k= (int) e.column; k < N(line); k++)
      if (is_source_tab (line, k)) k += 3;
      else if (line[k] != ' ' && line[k] != '\t' &&
               delim.find (line[k]) == std::string::npos)
        is_end= false;
    return true;
  }
  return false;
}

int
syntax_region_indent (tree root, int language, int query, int row, int tab) {
  indent_tab= max (1, tab);
  // the indentation of the line row by the query (indents.scm); -1 when
  // the line keeps the indentation it would have without the query
  // (@indent.auto), -2 without a syntax tree
  syntax_region_rep* r= get_region (root, language);
  if (r == NULL) return -2;
  array<string>& lines= r->lines;
  if (row < 0 || row >= N(lines)) return -2;
  TSNode top= ts_tree_root_node (r->tree);
  if (r->ind_query != query || r->ind_version != r->version) {
    string text;
    for (int i= 0; i < N(lines); i++) {
      if (i > 0) text << "\n";
      text << lines[i];
    }
    c_string ctext (text);
    r->ind_caps.clear ();
    tree_sitter_query_node_captures (query, top, ctext, r->ind_caps);
    r->ind_query= query;
    r->ind_version= r->version;
  }
  ts_node_captures& q= r->ind_caps;
  if (q.count ("indent.lisp")) return lisp_indent (lines, top, q, row);
  TSNode node= { { 0, 0, 0, 0 }, NULL, NULL };
  bool found= false;
  if (indent_columns (lines[row]) == N(lines[row])) {
    // a blank line: the node at the end of the previous line
    int prev= row - 1;
    while (prev >= 0 && indent_columns (lines[prev]) == N(lines[prev])) prev--;
    if (prev >= 0) {
      int ic= indent_columns (lines[prev]);
      string pl= trimmed (lines[prev]);
      node= node_at (top, prev, ic + N(pl) - 1);
      found= true;
      if (std::string (ts_node_type (node)).find ("comment") != std::string::npos) {
        // a comment at the end of the line does not count
        TSNode first= node_at (top, prev, ic);
        if (first.id != node.id) {
          int scol= (int) ts_node_start_point (node).column;
          pl= trimmed (lines[prev] (ic, max (scol, ic)));
          node= node_at (top, prev, ic + N(pl) - 1);
        }
      }
      if (captured (q, "indent.end", node) != NULL)
        node= node_at (top, row, indent_columns (lines[row]));
    }
  }
  else {
    node= node_at (top, row, indent_columns (lines[row]));
    found= true;
  }
  if (!found || ts_node_is_null (node)) return 0;
  if (captured (q, "indent.zero", node) != NULL) return 0;
  int indent= 0;
  std::unordered_map<int,bool> processed;
  while (!ts_node_is_null (node)) {
    ts_properties* begin= captured (q, "indent.begin", node);
    ts_properties* align= captured (q, "indent.align", node);
    int srow= (int) ts_node_start_point (node).row;
    int erow= (int) ts_node_end_point (node).row;
    if (begin == NULL && align == NULL && captured (q, "indent.auto", node) != NULL &&
        srow < row && row <= erow)
      return -1;
    if (begin == NULL && captured (q, "indent.ignore", node) != NULL &&
        srow < row && row <= erow)
      return 0;
    bool is_processed= false;
    bool done_row= processed[srow];
    if (!done_row &&
        ((captured (q, "indent.branch", node) != NULL && srow == row) ||
         (captured (q, "indent.dedent", node) != NULL && srow != row))) {
      indent -= tab;
      is_processed= true;
    }
    bool should= !done_row;
    bool in_err= false;
    if (should) {
      TSNode parent= ts_node_parent (node);
      in_err= !ts_node_is_null (parent) && ts_node_has_error (parent);
    }
    if (should && begin != NULL &&
        (srow != erow || in_err || begin->count ("indent.immediate")) &&
        (srow != row || begin->count ("indent.start_at_same_line"))) {
      indent += tab;
      is_processed= true;
    }
    if (in_err && align == NULL) {
      // the aligned indent of the first child, for an error node
      uint32_t n= ts_node_child_count (node);
      for (uint32_t i= 0; i < n; i++) {
        ts_properties* a= captured (q, "indent.align", ts_node_child (node, i));
        if (a != NULL) {
          q["indent.align"][node.id]= *a;
          align= captured (q, "indent.align", node);
          break;
        }
      }
    }
    if (should && align != NULL && (srow != erow || in_err) && srow != row) {
      ts_properties meta= *align;
      TSNode o_delim= node, c_delim= node;
      bool o_found= true, c_found= true, o_last= false, c_last= false;
      if (meta.count ("indent.open_delimiter"))
        o_found= find_delimiter (lines, node, meta["indent.open_delimiter"], o_delim, o_last);
      if (meta.count ("indent.close_delimiter"))
        c_found= find_delimiter (lines, node, meta["indent.close_delimiter"], c_delim, c_last);
      if (o_found) {
        int o_srow= (int) ts_node_start_point (o_delim).row;
        int o_scol= (int) ts_node_start_point (o_delim).column;
        int c_srow= c_found? (int) ts_node_start_point (c_delim).row: -1;
        bool absolute= false;
        if (o_last) {
          // a hanging indent (the line ends with the opening delimiter)
          indent += tab;
          if (c_last && c_srow >= 0 && c_srow < row) indent= max (indent - tab, 0);
        }
        else if (c_last && c_srow >= 0 && o_srow != c_srow && c_srow < row)
          indent= max (indent - tab, 0);
        else {
          int increment= 1;
          if (meta.count ("indent.increment"))
            increment= atoi (meta["indent.increment"].c_str ());
          indent= char_column (lines[o_srow], o_scol) + increment;
          absolute= true;
        }
        if (c_srow >= 0 && c_srow != o_srow && c_srow == row &&
            meta.count ("indent.avoid_last_matching_next") &&
            indent <= indent_width (lines[o_srow]) + tab)
          indent += tab;
        is_processed= true;
        if (absolute) return indent;
      }
    }
    processed[srow]= done_row || is_processed;
    node= ts_node_parent (node);
  }
  return indent;
}
