/******************************************************************************
* MODULE     : ts_tables.cpp
* DESCRIPTION: the parse states of tree-sitter grammars, built in C++
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* A port of the LR(1) construction of progs/treesit/ts-tables.scm and of the
   stages of build-tables! (ts-build.scm) which act on its states: the
   coincident tokens, the error state, the used symbols, the merging of
   compatible states, the unit reductions, the unused states and their
   order. It makes the same tables, byte for byte, as the Scheme code, which
   stays the reference: every order which shows in the result is the one of
   the Scheme code (numbering of the item keys, the item kept among equal
   ones, insertion order of the entries, stable sorts, order of the splits).

   tree_sitter_tables_start reads the syntax grammar written by
   progs/treesit/ts-native.scm and builds the states; tree_sitter_tables_finish
   is given the token conflicts and the keywords, computed in Scheme, and
   gives the final states. Results are texts for the Scheme reader. */

#include "tree_sitter_glue.hpp"
#include <algorithm>
#include <chrono>
#include <climits>
#include <sys/resource.h>
#ifdef __APPLE__
#include <malloc/malloc.h>
#endif
#include <functional>
#include <iterator>
#include <map>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

namespace {

typedef std::vector<int> ivec;
typedef long long ll;

const int KB= 1048576;
const int K_EXTERNAL= 0, K_END= 1, K_EONT= 2, K_TERMINAL= 3, K_NONTERMINAL= 4;
const int SYM_END= K_END * KB, SYM_EONT= K_EONT * KB;
inline int sym_kind (int s) { return s / KB; }
inline int sym_index (int s) { return s % KB; }
inline int terminal (int i) { return K_TERMINAL * KB + i; }
inline int non_terminal (int i) { return K_NONTERMINAL * KB + i; }

// under TS_PROFILE, the time since the previous lap (what = NULL starts)
void
lap (const char* what) {
  static bool profile= getenv ("TS_PROFILE") != NULL;
  static std::chrono::steady_clock::time_point last;
  std::chrono::steady_clock::time_point now= std::chrono::steady_clock::now ();
  if (profile && what != NULL) {
    struct rusage u;
    getrusage (RUSAGE_SELF, &u);
    // the peak resident memory so far, in MB (ru_maxrss: bytes on macOS)
#ifdef __APPLE__
    long mb= (long) (u.ru_maxrss / (1024 * 1024));
    malloc_statistics_t st;
    malloc_zone_statistics (NULL, &st);
    long used= (long) (st.size_in_use / (1024 * 1024));
#else
    long mb= (long) (u.ru_maxrss / 1024);
    long used= -1;
#endif
    fprintf (stderr, "ts-tables: %s %d ms, peak %ld MB, in use %ld MB\n", what, (int)
             std::chrono::duration_cast<std::chrono::milliseconds> (now - last).count (),
             mb, used);
  }
  last= now;
}
inline int external (int i) { return K_EXTERNAL * KB + i; }
inline bool is_terminal (int s) { return sym_kind (s) == K_TERMINAL; }
inline bool is_non_terminal (int s) { return sym_kind (s) == K_NONTERMINAL; }
inline bool is_external (int s) { return sym_kind (s) == K_EXTERNAL; }

const int VT_HIDDEN= 0, VT_AUXILIARY= 1, VT_ANONYMOUS= 2, VT_NAMED= 3;
inline bool vt_visible (int k) { return k >= VT_ANONYMOUS; }

struct ts_fail { std::string message; ts_fail (std::string m): message (m) {} };

/******************************************************************************
* Sorted sets of integers (as ts-insert, ts-union...)
******************************************************************************/

ivec
set_insert (int s, const ivec& set) {
  ivec r= set;
  ivec::iterator it= std::lower_bound (r.begin (), r.end (), s);
  if (it == r.end () || *it != s) r.insert (it, s);
  return r;
}

ivec
set_remove (int s, const ivec& set) {
  ivec r= set;
  ivec::iterator it= std::lower_bound (r.begin (), r.end (), s);
  if (it != r.end () && *it == s) r.erase (it);
  return r;
}

ivec
set_union (const ivec& a, const ivec& b) {
  ivec r;
  std::set_union (a.begin (), a.end (), b.begin (), b.end (), std::back_inserter (r));
  return r;
}

inline bool
set_contains (const ivec& set, int s) {
  return std::binary_search (set.begin (), set.end (), s);
}

ivec
terminals_of (const ivec& set) {
  ivec r;
  for (size_t i= 0; i < set.size (); i++) if (is_terminal (set[i])) r.push_back (set[i]);
  return r;
}

struct vec_hash {
  size_t operator () (const std::vector<ll>& v) const {
    size_t h= v.size ();
    for (size_t i= 0; i < v.size (); i++) h= h * 1000003u ^ (size_t) v[i];
    return h;
  }
  size_t operator () (const ivec& v) const {
    size_t h= v.size ();
    for (size_t i= 0; i < v.size (); i++) h= h * 1000003u ^ (size_t) v[i];
    return h;
  }
};

/******************************************************************************
* The syntax grammar
******************************************************************************/

struct step_t {
  int symbol;
  int prec_kind;   // 0 none, 1 integer, 2 name
  int prec;        // the integer, or the id of the name
  int assoc;       // 0 none, 1 left, 2 right
  int alias;       // id of the string, or -1
  int alias_named;
  int field;       // id of the string, or -1
  int reserved;
};

struct prod_t {
  std::vector<step_t> steps;
  int dynamic;
  bool eof;
};

struct grammar_t {
  int n_terminals, n_externals, n_vars, word;
  std::vector<std::string> strings;
  std::map<std::string,int> string_ids;
  std::vector<std::string> var_names, terminal_names, external_names;
  ivec var_kinds, terminal_kinds, external_kinds, external_internal;
  std::vector<prod_t> prods;
  std::vector<std::pair<int,int> > var_prods;
  std::map<std::pair<int,int>, ivec> inlines;
  ivec inline_syms, extras;
  std::vector<ivec> conflicts;
  std::vector<std::vector<std::pair<int,int> > > precedences;   // (0 name / 1 symbol, string id)
  std::vector<ivec> reserved_sets;
  std::vector<std::vector<int> > var_fields;                   // string ids
  ivec default_aliased, supertypes;
  prod_t start;
  // the NFA of the tokens: kind 0 advance (chars next prec sep), 1 split
  // (a b), 2 accept (token prec)
  struct nfa_node { int kind; std::vector<std::pair<int,int> > chars; int a, b, prec; bool sep; };
  std::vector<nfa_node> nfa;
  ivec implicit_prec, start_state;

  int intern (const std::string& s) {
    std::map<std::string,int>::iterator it= string_ids.find (s);
    if (it != string_ids.end ()) return it->second;
    int id= (int) strings.size ();
    strings.push_back (s);
    string_ids[s]= id;
    return id;
  }

  std::string symbol_name (int s) {
    if (s == SYM_END || s == SYM_EONT) return "EOF";
    if (is_external (s)) return external_names[sym_index (s)];
    if (is_non_terminal (s)) return var_names[sym_index (s)];
    int i= sym_index (s);
    if (terminal_kinds[i] == VT_NAMED) return terminal_names[i];
    return "'" + terminal_names[i] + "'";
  }
};

struct reader_t {
  const char* p;
  const char* e;
  reader_t (const char* p2, const char* e2): p (p2), e (e2) {}
  void skip () { while (p < e && (*p == ' ' || *p == '\n')) p++; }
  ll num () {
    skip ();
    bool neg= false;
    if (p < e && *p == '-') { neg= true; p++; }
    if (p >= e || *p < '0' || *p > '9') throw ts_fail ("tables: a number was expected");
    ll r= 0;
    while (p < e && *p >= '0' && *p <= '9') r= 10 * r + (*p++ - '0');
    return neg? -r: r;
  }
  std::string str () {
    ll n= num ();
    if (p >= e || *p != ':' || n < 0 || p + 1 + n > e) throw ts_fail ("tables: bad string");
    p++;
    std::string s (p, (size_t) n);
    p += n;
    return s;
  }
  ivec syms () {
    ll n= num ();
    ivec r;
    for (ll i= 0; i < n; i++) r.push_back ((int) num ());
    return r;
  }
  void word (const char* w) {
    skip ();
    size_t n= strlen (w);
    if ((size_t) (e - p) < n || strncmp (p, w, n) != 0)
      throw ts_fail (std::string ("tables: expected ") + w);
    p += n;
  }
};

void
read_grammar (grammar_t& g, reader_t& r) {
  r.word ("tstab1");
  g.n_terminals= (int) r.num ();
  g.n_externals= (int) r.num ();
  g.n_vars= (int) r.num ();
  g.word= (int) r.num ();
  for (int i= 0; i < g.n_vars; i++) {
    g.var_names.push_back (r.str ());
    g.var_kinds.push_back ((int) r.num ());
  }
  for (int i= 0; i < g.n_terminals; i++) {
    g.terminal_names.push_back (r.str ());
    g.terminal_kinds.push_back ((int) r.num ());
  }
  for (int i= 0; i < g.n_externals; i++) {
    g.external_names.push_back (r.str ());
    g.external_kinds.push_back ((int) r.num ());
    g.external_internal.push_back ((int) r.num ());
  }
  int np= (int) r.num ();
  for (int i= 0; i < np; i++) {
    prod_t p;
    p.dynamic= (int) r.num ();
    p.eof= r.num () != 0;
    int ns= (int) r.num ();
    for (int k= 0; k < ns; k++) {
      step_t s;
      s.symbol= (int) r.num ();
      s.prec_kind= (int) r.num ();
      s.prec= 0;
      if (s.prec_kind == 1) s.prec= (int) r.num ();
      else if (s.prec_kind == 2) s.prec= g.intern (r.str ());
      s.assoc= (int) r.num ();
      s.alias= -1; s.alias_named= 0;
      if (r.num ()) { s.alias= g.intern (r.str ()); s.alias_named= (int) r.num (); }
      s.field= -1;
      if (r.num ()) s.field= g.intern (r.str ());
      s.reserved= (int) r.num ();
      p.steps.push_back (s);
    }
    g.prods.push_back (p);
  }
  for (int i= 0; i < g.n_vars; i++) {
    int a= (int) r.num (), b= (int) r.num ();
    g.var_prods.push_back (std::make_pair (a, b));
  }
  int ni= (int) r.num ();
  for (int i= 0; i < ni; i++) {
    int pid= (int) r.num (), step= (int) r.num ();
    g.inlines[std::make_pair (pid, step)]= r.syms ();
  }
  g.inline_syms= r.syms ();
  g.extras= r.syms ();
  int nc= (int) r.num ();
  for (int i= 0; i < nc; i++) g.conflicts.push_back (r.syms ());
  int nl= (int) r.num ();
  for (int i= 0; i < nl; i++) {
    std::vector<std::pair<int,int> > l;
    int n= (int) r.num ();
    for (int k= 0; k < n; k++) {
      int kind= (int) r.num ();
      l.push_back (std::make_pair (kind, g.intern (r.str ())));
    }
    g.precedences.push_back (l);
  }
  int nr= (int) r.num ();
  for (int i= 0; i < nr; i++) g.reserved_sets.push_back (r.syms ());
  for (int i= 0; i < g.n_vars; i++) {
    int n= (int) r.num ();
    std::vector<int> fs;
    for (int k= 0; k < n; k++) fs.push_back (g.intern (r.str ()));
    g.var_fields.push_back (fs);
  }
  g.default_aliased= r.syms ();
  g.supertypes= r.syms ();
  int nn= (int) r.num ();
  for (int i= 0; i < nn; i++) {
    grammar_t::nfa_node x;
    x.kind= (int) r.num ();
    x.a= x.b= x.prec= 0; x.sep= false;
    if (x.kind == 0) {
      int nc= (int) r.num ();
      for (int k= 0; k < nc; k++) {
        int a= (int) r.num (), b= (int) r.num ();
        x.chars.push_back (std::make_pair (a, b));
      }
      x.a= (int) r.num ();
      x.prec= (int) r.num ();
      x.sep= r.num () != 0;
    }
    else { x.a= (int) r.num (); x.b= (int) r.num (); if (x.kind == 2) x.prec= x.b; }
    g.nfa.push_back (x);
  }
  for (int i= 0; i < g.n_terminals; i++) {
    g.implicit_prec.push_back ((int) r.num ());
    g.start_state.push_back ((int) r.num ());
  }
  r.word ("end");
  // the augmented start production
  step_t s0= { non_terminal (0), 0, 0, 0, -1, 0, -1, 65535 };
  g.start.steps.push_back (s0);
  g.start.dynamic= 0;
  g.start.eof= false;
}

/******************************************************************************
* Items
******************************************************************************/

struct item_t {
  int var, prod, step;
  bool inherited;
  int key;
  const prod_t* p;
  const step_t* at;     // the step at the dot, or NULL
  const step_t* prev;   // the step before the dot, or NULL
  ll sort_key;
};

const ll ITEM_KEY_LIMIT= 2097152;

struct items_t {
  grammar_t& g;
  std::unordered_map<std::vector<ll>, int, vec_hash> contents;
  std::unordered_map<ll, int> key_memo;
  std::unordered_map<ll, item_t*> item_memo;
  std::vector<item_t*> all;
  items_t (grammar_t& g2): g (g2) {}
  ~items_t () { clear (); }
  void clear () {
    for (size_t i= 0; i < all.size (); i++) delete all[i];
    std::vector<item_t*> ().swap (all);
    std::unordered_map<std::vector<ll>, int, vec_hash> ().swap (contents);
    std::unordered_map<ll, int> ().swap (key_memo);
    std::unordered_map<ll, item_t*> ().swap (item_memo);
  }

  const prod_t& production (int prod) { return prod < 0? g.start: g.prods[prod]; }

  static void push_step (std::vector<ll>& c, const step_t& s) {
    c.push_back (s.symbol); c.push_back (s.prec_kind); c.push_back (s.prec);
    c.push_back (s.assoc); c.push_back (s.alias); c.push_back (s.alias_named);
    c.push_back (s.field); c.push_back (s.reserved);
  }

  int key_of (int var, int prod, int step, bool inherited) {
    // item-key-of and content-key
    ll k= ((((ll) (prod + 1) * 1024 + step) * 2 + (inherited? 1: 0)) * 1048576) + (var + 1);
    std::unordered_map<ll,int>::iterator it= key_memo.find (k);
    if (it != key_memo.end ()) return it->second;
    const prod_t& p= production (prod);
    std::vector<ll> c;
    c.push_back (var); c.push_back (step); c.push_back (inherited? 1: 0);
    c.push_back (p.dynamic); c.push_back (p.eof? 1: 0); c.push_back ((ll) p.steps.size ());
    if (step > 0) {
      const step_t& prev= p.steps[step-1];
      c.push_back (prev.prec_kind); c.push_back (prev.prec); c.push_back (prev.assoc);
    }
    else { c.push_back (0); c.push_back (0); c.push_back (0); }
    c.push_back (step);
    for (int i= 0; i < step; i++) {
      const step_t& s= p.steps[i];
      c.push_back (s.alias); c.push_back (s.alias_named); c.push_back (s.field);
      c.push_back (inherited? s.symbol: -1);
    }
    c.push_back (-7);
    for (size_t i= step; i < p.steps.size (); i++) push_step (c, p.steps[i]);
    std::unordered_map<std::vector<ll>,int,vec_hash>::iterator jt= contents.find (c);
    int id;
    if (jt != contents.end ()) id= jt->second;
    else { id= (int) contents.size (); contents[c]= id; }
    key_memo[k]= id;
    return id;
  }

  item_t* make (int var, int prod, int step, bool inherited) {
    ll k= ((((ll) (prod + 1) * 1024 + step) * 2 + (inherited? 1: 0)) * 1048576) + (var + 1);
    std::unordered_map<ll,item_t*>::iterator it= item_memo.find (k);
    if (it != item_memo.end ()) return it->second;
    int key= key_of (var, prod, step, inherited);
    item_t* item= new item_t;
    item->var= var; item->prod= prod; item->step= step; item->inherited= inherited;
    item->key= key;
    item->p= &production (prod);
    int n= (int) item->p->steps.size ();
    item->at= step < n? &item->p->steps[step]: NULL;
    item->prev= step > 0? &item->p->steps[step-1]: NULL;
    if (key >= ITEM_KEY_LIMIT || var + 1 >= ITEM_KEY_LIMIT)
      throw ts_fail ("too many items for the sort keys");
    item->sort_key= ((ll) step * ITEM_KEY_LIMIT + var + 1) * ITEM_KEY_LIMIT + key;
    item_memo[k]= item;
    all.push_back (item);
    return item;
  }

  item_t* successor (item_t* i) { return make (i->var, i->prod, i->step + 1, i->inherited); }
  item_t* substitute (item_t* i, int prod) { return make (i->var, prod, i->step, i->inherited); }
  item_t* with_inherited (item_t* i) { return make (i->var, i->prod, i->step, true); }
};

inline int item_symbol (const item_t* i) { return i->at? i->at->symbol: -1; }
inline bool item_augmented (const item_t* i) { return i->var < 0; }

/******************************************************************************
* Lookahead sets
******************************************************************************/

struct la_pool_t {
  std::vector<ivec> sets;
  std::unordered_map<ivec, int, vec_hash> ids;
  std::unordered_map<ll, int> unions;
  la_pool_t () { intern (ivec ()); }
  int intern (const ivec& s) {
    std::unordered_map<ivec,int,vec_hash>::iterator it= ids.find (s);
    if (it != ids.end ()) return it->second;
    int id= (int) sets.size ();
    sets.push_back (s);
    ids[s]= id;
    return id;
  }
  const ivec& get (int id) { return sets[id]; }
  int unite (int a, int b) {
    if (a == b || b == 0) return a;
    if (a == 0) return b;
    ll key= a < b? (ll) a * 16777216 + b: (ll) b * 16777216 + a;
    std::unordered_map<ll,int>::iterator it= unions.find (key);
    if (it != unions.end ()) return it->second;
    int id= intern (set_union (sets[a], sets[b]));
    unions[key]= id;
    return id;
  }
  int insert (int a, int s) { return intern (set_insert (s, sets[a])); }
};

/******************************************************************************
* Item sets
******************************************************************************/

struct entry_t {
  item_t* item;
  int las;
  int reserved;
};

struct item_set_t {
  // an item set while it is built: its entries by key, in insertion order
  // (the index pos of the keys is made once the set is no longer small, and
  // covers the first pos.size () entries)
  std::unordered_map<int, int> pos;
  std::vector<entry_t> entries;
  entry_t& insert (item_t* item) {
    int key= item->key;
    if (entries.size () <= 16) {
      for (size_t k= 0; k < entries.size (); k++)
        if (entries[k].item->key == key) return entries[k];
    }
    else {
      for (size_t k= pos.size (); k < entries.size (); k++)
        pos[entries[k].item->key]= (int) k;
      std::unordered_map<int,int>::iterator it= pos.find (key);
      if (it != pos.end ()) return entries[it->second];
    }
    entry_t e= { item, 0, 0 };
    entries.push_back (e);
    return entries.back ();
  }
  std::vector<entry_t> sorted () const {
    std::vector<entry_t> r= entries;
    std::stable_sort (r.begin (), r.end (),
                      [] (const entry_t& a, const entry_t& b) {
                        return a.item->sort_key < b.item->sort_key; });
    return r;
  }
};

/******************************************************************************
* Parse states
******************************************************************************/

enum { A_SHIFT, A_REDUCE, A_ACCEPT, A_SHIFT_EXTRA, A_RECOVER, A_GOTO };

struct action_t {
  int kind;
  int a, b, c, d;   // shift: state, repetition; reduce: symbol count dynamic production
  bool operator == (const action_t& o) const {
    return kind == o.kind && a == o.a && b == o.b && c == o.c && d == o.d; }
};

inline action_t make_action (int kind, int a= 0, int b= 0, int c= 0, int d= 0) {
  action_t x= { kind, a, b, c, d }; return x; }

struct action_list_t {
  // the actions of an entry of a parse state: nearly always one, kept in
  // place; the others, if any, in rest (tens of millions of entries for a
  // large grammar, so no allocation for the usual case)
  action_t first;
  int n;
  std::vector<action_t>* rest;
  action_list_t (): n (0), rest (NULL) {}
  action_list_t (const action_list_t& o): first (o.first), n (o.n),
    rest (o.rest == NULL? NULL: new std::vector<action_t> (*o.rest)) {}
  action_list_t& operator = (const action_list_t& o) {
    if (this == &o) return *this;
    first= o.first; n= o.n;
    delete rest;
    rest= (o.rest == NULL? NULL: new std::vector<action_t> (*o.rest));
    return *this;
  }
  action_list_t (action_list_t&& o) noexcept: first (o.first), n (o.n), rest (o.rest) {
    o.rest= NULL; o.n= 0; }
  action_list_t& operator = (action_list_t&& o) noexcept {
    if (this == &o) return *this;
    delete rest;
    first= o.first; n= o.n; rest= o.rest;
    o.rest= NULL; o.n= 0;
    return *this;
  }
  ~action_list_t () { delete rest; }
  size_t size () const { return (size_t) n; }
  bool empty () const { return n == 0; }
  action_t& operator [] (size_t i) { return i == 0? first: (*rest)[i - 1]; }
  const action_t& operator [] (size_t i) const { return i == 0? first: (*rest)[i - 1]; }
  action_t& back () { return (*this)[n - 1]; }
  const action_t& back () const { return (*this)[n - 1]; }
  void push_back (const action_t& a) {
    if (n == 0) first= a;
    else {
      if (rest == NULL) rest= new std::vector<action_t> ();
      rest->push_back (a);
    }
    n++;
  }
  void pop_back () { if (n > 1) rest->pop_back (); n--; }
  void clear () { delete rest; rest= NULL; n= 0; }
  bool contains (const action_t& a) const {
    for (int i= 0; i < n; i++) if ((*this)[i] == a) return true;
    return false;
  }
  bool operator == (const action_list_t& o) const {
    if (n != o.n) return false;
    for (int i= 0; i < n; i++) if (!((*this)[i] == o[i])) return false;
    return true;
  }
};

struct tentry_t {
  action_list_t actions;
  bool reusable;
};

template<class T> struct omap_t {
  // a map from symbols which remembers the order of insertion; its index of
  // the keys is a hash table while the map is built, then (freeze) a sorted
  // vector, much smaller: the parse states of a large grammar are hundreds
  // of thousands before their merging
  ivec keys;
  std::vector<T> vals;
  std::unordered_map<int,int> pos;
  std::vector<std::pair<int,int> > index;   // (key, position), once frozen
  bool frozen= false;
  int find (int k) const {
    if (!frozen) {
      std::unordered_map<int,int>::const_iterator it= pos.find (k);
      return it == pos.end ()? -1: it->second;
    }
    std::vector<std::pair<int,int> >::const_iterator it=
      std::lower_bound (index.begin (), index.end (), std::make_pair (k, INT_MIN));
    return (it != index.end () && it->first == k)? it->second: -1;
  }
  bool has (int k) const { return find (k) >= 0; }
  T* ref (int k) { int i= find (k); return i < 0? NULL: &vals[i]; }
  void set (int k, const T& v) {
    int i= find (k);
    if (i >= 0) { vals[i]= v; return; }
    int p= (int) keys.size ();
    keys.push_back (k);
    vals.push_back (v);
    if (!frozen) pos[k]= p;
    else index.insert (std::lower_bound (index.begin (), index.end (),
                                         std::make_pair (k, INT_MIN)),
                       std::make_pair (k, p));
  }
  void freeze () {
    if (frozen) return;
    index.clear ();
    for (size_t p= 0; p < keys.size (); p++)
      index.push_back (std::make_pair (keys[p], (int) p));
    std::sort (index.begin (), index.end ());
    std::unordered_map<int,int> ().swap (pos);
    keys.shrink_to_fit ();
    vals.shrink_to_fit ();
    frozen= true;
  }
  int size () const { return (int) keys.size (); }
};

struct state_t {
  int id;
  omap_t<tentry_t> terms;
  omap_t<action_t> nonterms;
  ivec reserved;
  int core;
  bool eof_gated;
};

struct aux_info_t {
  int symbol;
  ivec parents;
  bool operator == (const aux_info_t& o) const {
    return symbol == o.symbol && parents == o.parents; }
};

struct red_info_t {
  int prec_kind, prec;   // the precedence (kind 0: none)
  ivec symbols;
  bool left, right, none;
};

struct prod_info_t {
  std::vector<std::pair<int,int> > aliases;   // (string id or -1, named)
  std::vector<std::pair<int, std::vector<std::pair<int,bool> > > > fields;
  bool operator == (const prod_info_t& o) const {
    return aliases == o.aliases && fields == o.fields; }
};

/******************************************************************************
* The builder
******************************************************************************/

struct addition_t {
  item_t* item;
  int las, reserved;
  bool propagates, has_word;
  bool operator == (const addition_t& o) const {
    return item == o.item && las == o.las && reserved == o.reserved &&
           propagates == o.propagates && has_word == o.has_word; }
};

struct builder_t {
  grammar_t& g;
  items_t items;
  la_pool_t pool;
  std::unordered_map<int, ivec> first, last;
  std::unordered_map<int, int> reserved_first;
  std::unordered_map<int, int> first_ids;
  std::vector<std::vector<addition_t> > additions;

  // the parse table
  std::vector<state_t*> states;
  std::vector<std::vector<entry_t> > kernels;
  // the preceding symbols of the states, as shared lists: a state has a
  // node, a node is (symbol . parent node), node 0 the empty list
  ivec preceding;
  std::vector<std::pair<int,int> > preceding_nodes;
  std::unordered_map<std::vector<ll>, int, vec_hash> state_ids;
  std::unordered_map<std::vector<ll>, int, vec_hash> core_ids;
  std::vector<std::pair<int, std::vector<aux_info_t> > > queue;
  size_t queue_head;
  std::vector<std::pair<int,int> > non_terminal_extra_states;
  std::vector<prod_info_t> production_infos;
  std::unordered_map<int,int> production_info_ids;
  int max_aliased_length;
  std::vector<ivec> used_conflicts;   // reversed: the Scheme list conses

  builder_t (grammar_t& g2): g (g2), items (g2), queue_head (0), max_aliased_length (1),
    preceding_nodes (1, std::make_pair (-1, -1)) {}
  ~builder_t () { for (size_t i= 0; i < states.size (); i++) delete states[i]; }

  const ivec& first_set (int s) { return first[s]; }
  void release_construction ();
  int first_set_id (int s) {
    std::unordered_map<int,int>::iterator it= first_ids.find (s);
    if (it != first_ids.end ()) return it->second;
    int id= pool.intern (first[s]);
    first_ids[s]= id;
    return id;
  }

  void make_builder ();
  std::vector<entry_t> closure (const std::vector<entry_t>& kernel);
  int add_state (int pre, const std::vector<aux_info_t>& aux,
                 const std::vector<entry_t>& entries);
  int cons_preceding (int symbol, int pre) {
    preceding_nodes.push_back (std::make_pair (symbol, pre));
    return (int) preceding_nodes.size () - 1; }
  int production_id (item_t* item);
  int compare_precedence (int lk, int lv, const ivec& lsyms, int rk, int rv, const ivec& rsyms);
  void handle_conflict (state_t* state, const std::vector<entry_t>& entries,
                        int pre, const std::vector<aux_info_t>& aux,
                        int lookahead, red_info_t& ri);
  void add_actions (int state_id, const std::vector<aux_info_t>& aux,
                    const std::vector<entry_t>& entries);
  void build ();
};

void
builder_t::make_builder () {
  for (int i= 0; i < g.n_terminals; i++) {
    first[terminal (i)]= ivec (1, terminal (i));
    last[terminal (i)]= ivec (1, terminal (i));
    reserved_first[terminal (i)]= 0;
  }
  for (int i= 0; i < g.n_externals; i++) {
    first[external (i)]= ivec (1, external (i));
    last[external (i)]= ivec (1, external (i));
    reserved_first[external (i)]= 0;
  }
  for (int i= 0; i < g.n_vars; i++) {
    ivec fs, ls;
    int rf= 0;
    {
      std::unordered_map<int,bool> processed;
      ivec stack (1, i);
      while (!stack.empty ()) {
        // (car stack) is the back
        int top= stack.back (); stack.pop_back ();
        ivec pushed;
        for (int pid= g.var_prods[top].first; pid < g.var_prods[top].second; pid++) {
          const prod_t& p= g.prods[pid];
          if (p.steps.empty ()) continue;
          const step_t& step= p.steps[0];
          int s= step.symbol;
          if (is_terminal (s) || is_external (s)) fs= set_insert (s, fs);
          else if (!processed[s]) { processed[s]= true; pushed.push_back (sym_index (s)); }
          rf= std::max (rf, step.reserved);
        }
        for (size_t k= 0; k < pushed.size (); k++) stack.push_back (pushed[k]);
      }
    }
    {
      std::unordered_map<int,bool> processed;
      ivec stack (1, i);
      while (!stack.empty ()) {
        int top= stack.back (); stack.pop_back ();
        ivec pushed;
        for (int pid= g.var_prods[top].first; pid < g.var_prods[top].second; pid++) {
          const prod_t& p= g.prods[pid];
          if (p.steps.empty ()) continue;
          int s= p.steps.back ().symbol;
          if (is_terminal (s) || is_external (s)) ls= set_insert (s, ls);
          else if (!processed[s]) { processed[s]= true; pushed.push_back (sym_index (s)); }
        }
        for (size_t k= 0; k < pushed.size (); k++) stack.push_back (pushed[k]);
      }
    }
    first[non_terminal (i)]= fs;
    reserved_first[non_terminal (i)]= rf;
    last[non_terminal (i)]= ls;
  }
  // the additions of each non-terminal
  additions.resize (g.n_vars);
  for (int i= 0; i < g.n_vars; i++) {
    struct info_t { ivec las; int rid; bool prop; };
    ivec order;
    std::unordered_map<int, info_t> infos;
    struct frame_t { int v; ivec las; int rid; bool prop; };
    std::vector<frame_t> stack;
    frame_t f0= { i, ivec (), 0, true };
    stack.push_back (f0);
    while (!stack.empty ()) {
      frame_t top= stack.back (); stack.pop_back ();
      if (infos.find (top.v) == infos.end ()) {
        info_t x= { ivec (), 0, false };
        infos[top.v]= x;
        order.push_back (top.v);
      }
      info_t& info= infos[top.v];
      ivec new_las= set_union (info.las, top.las);
      bool did= new_las != info.las || top.rid > info.rid || (top.prop && !info.prop);
      info.las= new_las;
      if (top.rid > info.rid) info.rid= top.rid;
      if (top.prop) info.prop= true;
      if (!did) continue;
      // the pushes come out last first, as with cons onto rest
      for (int pid= g.var_prods[top.v].first; pid < g.var_prods[top.v].second; pid++) {
        const prod_t& p= g.prods[pid];
        if (!p.steps.empty () && is_non_terminal (p.steps[0].symbol)) {
          int s= sym_index (p.steps[0].symbol);
          frame_t f;
          f.v= s;
          if (p.steps.size () > 1) {
            int next= p.steps[1].symbol;
            f.las= first[next]; f.rid= reserved_first[next]; f.prop= false;
          }
          else if (p.eof) { f.las= ivec (1, SYM_END); f.rid= 0; f.prop= false; }
          else { f.las= top.las; f.rid= top.rid; f.prop= top.prop; }
          stack.push_back (f);
        }
      }
    }
    std::vector<addition_t> result;   // in reverse order, as the Scheme list
    for (size_t k= 0; k < order.size (); k++) {
      int v= order[k];
      if (std::find (g.inline_syms.begin (), g.inline_syms.end (), non_terminal (v)) !=
          g.inline_syms.end ()) continue;
      info_t& info= infos[v];
      int base_las= pool.intern (info.las);
      bool base_word= g.word >= 0 && set_contains (info.las, g.word);
      int eof_las= pool.intern (ivec (1, SYM_END));
      for (int pid= g.var_prods[v].first; pid < g.var_prods[v].second; pid++) {
        std::map<std::pair<int,int>,ivec>::iterator it= g.inlines.find (std::make_pair (pid, 0));
        ivec ids= it == g.inlines.end ()? ivec (1, pid): it->second;
        for (size_t j= 0; j < ids.size (); j++) {
          int id= ids[j];
          addition_t a;
          a.item= items.make (v, id, 0, false);
          if (g.prods[id].eof) {
            a.las= eof_las; a.reserved= 0; a.propagates= false; a.has_word= false; }
          else {
            a.las= base_las; a.reserved= info.rid; a.propagates= info.prop;
            a.has_word= base_word; }
          if (std::find (result.begin (), result.end (), a) == result.end ())
            result.push_back (a);
        }
      }
    }
    additions[i]= result;   // (reverse result) of the list made by cons
  }
}

std::vector<entry_t>
builder_t::closure (const std::vector<entry_t>& kernel) {
  item_set_t set;
  int word= g.word;
  for (size_t k= 0; k < kernel.size (); k++) {
    const entry_t& entry= kernel[k];
    item_t* item= entry.item;
    ivec ids;
    bool has_ids= false;
    if (item->prod >= 0) {
      std::map<std::pair<int,int>,ivec>::iterator it=
        g.inlines.find (std::make_pair (item->prod, item->step));
      if (it != g.inlines.end ()) { ids= it->second; has_ids= true; }
    }
    std::vector<item_t*> todo;
    if (has_ids) for (size_t j= 0; j < ids.size (); j++) todo.push_back (items.substitute (item, ids[j]));
    else todo.push_back (item);
    for (size_t j= 0; j < todo.size (); j++) {
      item_t* it= todo[j];
      int las= entry.las, reserved= entry.reserved;
      int s= item_symbol (it);
      if (s >= 0 && is_non_terminal (s)) {
        int i= it->step + 1;
        int next= i < (int) it->p->steps.size ()? it->p->steps[i].symbol: -1;
        int following= next >= 0? first_set_id (next): las;
        int following_reserved= next >= 0? reserved_first[next]: reserved;
        const std::vector<addition_t>& adds= additions[sym_index (s)];
        for (size_t a= 0; a < adds.size (); a++) {
          const addition_t& info= adds[a];
          entry_t& e= set.insert (info.item);
          e.las= pool.unite (e.las, info.las);
          if (info.has_word) e.reserved= std::max (e.reserved, info.reserved);
          if (info.propagates) {
            e.las= pool.unite (e.las, following);
            if (word >= 0 && set_contains (pool.get (following), word))
              e.reserved= std::max (e.reserved, following_reserved);
          }
        }
      }
      entry_t& e= set.insert (it);
      e.las= pool.unite (e.las, las);
      e.reserved= std::max (e.reserved, reserved);
    }
  }
  return set.sorted ();
}

int
builder_t::add_state (int pre, const std::vector<aux_info_t>& aux,
                      const std::vector<entry_t>& entries) {
  std::vector<ll> sig, core;
  for (size_t i= 0; i < entries.size (); i++) {
    sig.push_back (entries[i].item->key);
    sig.push_back (entries[i].las);
    sig.push_back (entries[i].reserved);
    core.push_back (entries[i].item->key);
  }
  std::unordered_map<std::vector<ll>,int,vec_hash>::iterator it= state_ids.find (sig);
  if (it != state_ids.end ()) return it->second;
  int core_id;
  std::unordered_map<std::vector<ll>,int,vec_hash>::iterator ct= core_ids.find (core);
  if (ct != core_ids.end ()) core_id= ct->second;
  else { core_id= (int) core_ids.size (); core_ids[core]= core_id; }
  int id= (int) states.size ();
  state_t* s= new state_t;
  s->id= id; s->core= core_id; s->eof_gated= false;
  states.push_back (s);
  kernels.push_back (entries);
  preceding.push_back (pre);
  state_ids[sig]= id;
  queue.push_back (std::make_pair (id, aux));
  return id;
}

int
builder_t::production_id (item_t* item) {
  std::unordered_map<int,int>::iterator it= production_info_ids.find (item->prod);
  if (it != production_info_ids.end ()) return it->second;
  const prod_t& p= *item->p;
  std::vector<std::pair<int,int> > aliases;   // forward order
  prod_info_t info;
  auto add_field= [&info] (int f, std::pair<int,bool> loc) {
    for (size_t k= 0; k < info.fields.size (); k++)
      if (info.fields[k].first == f) { info.fields[k].second.push_back (loc); return; }
    info.fields.push_back (std::make_pair (f, std::vector<std::pair<int,bool> > (1, loc)));
  };
  for (size_t i= 0; i < p.steps.size (); i++) {
    const step_t& step= p.steps[i];
    aliases.push_back (std::make_pair (step.alias, step.alias_named));
    if (step.field >= 0) add_field (step.field, std::make_pair ((int) i, false));
    if (is_non_terminal (step.symbol) && !vt_visible (g.var_kinds[sym_index (step.symbol)])) {
      const std::vector<int>& fs= g.var_fields[sym_index (step.symbol)];
      for (size_t k= 0; k < fs.size (); k++) add_field (fs[k], std::make_pair ((int) i, true));
    }
  }
  while (!aliases.empty () && aliases.back ().first < 0) aliases.pop_back ();
  info.aliases= aliases;
  grammar_t& gg= g;
  std::stable_sort (info.fields.begin (), info.fields.end (),
                    [&gg] (const std::pair<int, std::vector<std::pair<int,bool> > >& a,
                           const std::pair<int, std::vector<std::pair<int,bool> > >& b) {
                      return gg.strings[a.first] < gg.strings[b.first]; });
  int id= -1;
  for (size_t k= 0; k < production_infos.size (); k++)
    if (production_infos[k] == info) { id= (int) k; break; }
  if (id < 0) { id= (int) production_infos.size (); production_infos.push_back (info); }
  if ((int) p.steps.size () > max_aliased_length) max_aliased_length= (int) p.steps.size ();
  production_info_ids[item->prod]= id;
  return id;
}

inline int cmp (int a, int b) { return a < b? -1: (a > b? 1: 0); }

int
builder_t::compare_precedence (int lk, int lv, const ivec& lsyms, int rk, int rv, const ivec& rsyms) {
  // the precedences are (kind value): 0 none, 1 integer, 2 name
  if (lk == 1 && rk == 1 && (lv != 0 || rv != 0)) return cmp (lv, rv);
  if (lk == 1 && rk == 0 && lv != 0) return cmp (lv, 0);
  if (lk == 0 && rk == 1 && rv != 0) return cmp (0, rv);
  auto matches= [this] (const std::pair<int,int>& entry, int k, int v, const ivec& syms) {
    if (entry.first == 0) return k == 2 && v == entry.second;
    for (size_t i= 0; i < syms.size (); i++)
      if (g.var_names[sym_index (syms[i])] == g.strings[entry.second]) return true;
    return false;
  };
  for (size_t l= 0; l < g.precedences.size (); l++) {
    bool saw_left= false, saw_right= false;
    const std::vector<std::pair<int,int> >& list= g.precedences[l];
    for (size_t k= 0; k < list.size (); k++) {
      if (matches (list[k], lk, lv, lsyms)) {
        if (saw_right) return -1;
        saw_left= true;
      }
      else if (matches (list[k], rk, rv, rsyms)) {
        if (saw_left) return 1;
        saw_right= true;
      }
    }
  }
  return 0;
}

static bool
item_done (const item_t* i) { return i->at == NULL; }

void
builder_t::handle_conflict (state_t* state, const std::vector<entry_t>& entries,
                            int pre, const std::vector<aux_info_t>& aux,
                            int lookahead, red_info_t& ri) {
  tentry_t* entry= state->terms.ref (lookahead);
  std::vector<std::pair<std::pair<int,int>, int> > shift_precs;   // ((kind value) . symbol)
  std::vector<item_t*> conflicting;
  auto adjoin= [&conflicting] (item_t* i) {
    if (std::find (conflicting.begin (), conflicting.end (), i) == conflicting.end ())
      conflicting.push_back (i); };
  for (size_t k= 0; k < entries.size (); k++) {
    item_t* item= entries[k].item;
    const step_t* step= item->at;
    if (step) {
      if (item->step > 0 && set_contains (first_set (step->symbol), lookahead)) {
        if (!item_augmented (item)) adjoin (item);
        int pk= item->prev? item->prev->prec_kind: 0;
        int pv= item->prev? item->prev->prec: 0;
        std::pair<std::pair<int,int>,int> pp (std::make_pair (pk, pk? pv: 0), non_terminal (item->var));
        if (std::find (shift_precs.begin (), shift_precs.end (), pp) == shift_precs.end ())
          shift_precs.push_back (pp);
      }
    }
    else if (set_contains (pool.get (entries[k].las), lookahead) && !item_augmented (item))
      adjoin (item);
  }
  action_list_t& actions= entry->actions;
  action_t last= actions.back ();
  bool repetition= false;
  if (last.kind == A_SHIFT && !conflicting.empty ()) {
    int v= conflicting[0]->var;
    if (g.var_kinds[v] == VT_AUXILIARY) {
      repetition= true;
      for (size_t k= 0; k < conflicting.size (); k++)
        if (conflicting[k]->var != v) repetition= false;
    }
  }
  if (repetition) {
    actions.pop_back ();
    actions.push_back (make_action (A_SHIFT, last.a, 1));
    return;
  }
  if (last.kind == A_SHIFT) {
    bool less= false, equal= false, more= false;
    for (size_t k= 0; k < shift_precs.size (); k++) {
      int c= compare_precedence (shift_precs[k].first.first, shift_precs[k].first.second,
                                 ivec (1, shift_precs[k].second),
                                 ri.prec_kind, ri.prec, ri.symbols);
      if (c == 1) more= true; else if (c == -1) less= true; else equal= true;
    }
    bool right_only= !ri.left && !ri.none && ri.right;
    auto keep_last= [&actions] () { action_t l= actions.back (); actions.clear (); actions.push_back (l); };
    auto pop= [&actions, &conflicting] () {
      actions.pop_back ();
      std::vector<item_t*> r;
      for (size_t k= 0; k < conflicting.size (); k++)
        if (item_done (conflicting[k])) r.push_back (conflicting[k]);
      conflicting= r;
    };
    if (more && !less) keep_last ();
    else if (less && !more) { if (equal && right_only) keep_last (); else pop (); }
    else if (!less && !more) {
      if (ri.left && !ri.none && !ri.right) pop ();
      else if (right_only) keep_last ();
    }
  }
  if (actions.size () != 1) {
    ivec actual;
    for (size_t k= 0; k < conflicting.size (); k++) {
      item_t* item= conflicting[k];
      int s= non_terminal (item->var);
      if (g.var_kinds[item->var] == VT_AUXILIARY) {
        for (int j= (int) aux.size () - 1; j >= 0; j--)
          if (aux[j].symbol == s) {
            actual.insert (actual.end (), aux[j].parents.begin (), aux[j].parents.end ());
            break;
          }
      }
      else actual.push_back (s);
    }
    std::stable_sort (actual.begin (), actual.end ());
    actual.erase (std::unique (actual.begin (), actual.end ()), actual.end ());
    if (std::find (g.conflicts.begin (), g.conflicts.end (), actual) != g.conflicts.end ())
      used_conflicts.push_back (actual);
    else {
      std::string m= "unresolved conflict for symbol sequence: ";
      ivec preceding_symbols;   // reversed
      for (int x= pre; x != 0; x= preceding_nodes[x].second)
        preceding_symbols.push_back (preceding_nodes[x].first);
      for (int k= (int) preceding_symbols.size () - 1; k >= 0; k--)
        m += g.symbol_name (preceding_symbols[k]) + " ";
      m += "* " + g.symbol_name (lookahead) + "; add a conflict for these rules: ";
      for (size_t k= 0; k < actual.size (); k++) m += g.symbol_name (actual[k]) + " ";
      throw ts_fail (m);
    }
  }
}

static aux_info_t
auxiliary_node_info (const std::vector<entry_t>& entries, int symbol, grammar_t& g) {
  aux_info_t r;
  r.symbol= symbol;
  for (size_t k= 0; k < entries.size (); k++) {
    item_t* item= entries[k].item;
    if (!item_augmented (item) && item_symbol (item) == symbol &&
        g.var_kinds[item->var] != VT_AUXILIARY)
      r.parents.push_back (non_terminal (item->var));
  }
  return r;
}

void
builder_t::add_actions (int state_id, const std::vector<aux_info_t>& aux_in,
                        const std::vector<entry_t>& entries) {
  state_t* state= states[state_id];
  int pre= preceding[state_id];
  std::map<int, item_set_t> terminal_successors, non_terminal_successors;
  ivec conflicted;
  std::unordered_map<int, red_info_t> reduction_infos;
  std::unordered_map<int, aux_info_t> aux_info;
  std::vector<aux_info_t> aux= aux_in;   // forward order: aux-in, then the new ones
  for (size_t k= 0; k < entries.size (); k++) {
    const entry_t& e= entries[k];
    item_t* item= e.item;
    int next= item_symbol (item);
    if (next >= 0) {
      item_t* successor= items.successor (item);
      std::map<int,item_set_t>* table= &terminal_successors;
      if (is_non_terminal (next)) {
        table= &non_terminal_successors;
        if (g.var_kinds[sym_index (next)] == VT_AUXILIARY) {
          std::unordered_map<int,aux_info_t>::iterator it= aux_info.find (next);
          if (it == aux_info.end ())
            it= aux_info.insert (std::make_pair (next, auxiliary_node_info (entries, next, g))).first;
          aux.push_back (it->second);
        }
        if (!vt_visible (g.var_kinds[sym_index (next)]) &&
            !g.var_fields[sym_index (next)].empty ())
          successor= items.with_inherited (successor);
      }
      entry_t& se= (*table)[next].insert (successor);
      se.las= pool.unite (se.las, e.las);
      se.reserved= std::max (se.reserved, e.reserved);
    }
    else {
      int symbol= non_terminal (std::max (0, item->var));
      const prod_t& p= *item->p;
      action_t action= item_augmented (item)?
        make_action (A_ACCEPT):
        make_action (A_REDUCE, symbol, item->step, p.dynamic, production_id (item));
      int pk= item->prev? item->prev->prec_kind: 0;
      int pv= item->prev? item->prev->prec: 0;
      int assoc= item->prev? item->prev->assoc: 0;
      if (p.eof) state->eof_gated= true;
      const ivec las= pool.get (e.las);
      for (size_t j= 0; j < las.size (); j++) {
        int la= las[j];
        if (p.eof && la != SYM_END) continue;
        if (!state->terms.has (la)) {
          tentry_t te; te.reusable= true;
          state->terms.set (la, te);
        }
        tentry_t* te= state->terms.ref (la);
        if (reduction_infos.find (la) == reduction_infos.end ()) {
          red_info_t x= { 0, 0, ivec (), false, false, false };
          reduction_infos[la]= x;
        }
        red_info_t& ri= reduction_infos[la];
        bool go= true;
        if (te->actions.empty ()) te->actions.push_back (action);
        else {
          int c= compare_precedence (pk, pv, ivec (1, symbol), ri.prec_kind, ri.prec, ri.symbols);
          if (c == 1) {
            te->actions.clear (); te->actions.push_back (action);
            conflicted= set_remove (la, conflicted);
            ri.prec_kind= 0; ri.prec= 0; ri.symbols.clear ();
            ri.left= ri.right= ri.none= false;
          }
          else if (c == 0) {
            if (!te->actions.contains (action)) {
              te->actions.push_back (action);
              conflicted= set_insert (la, conflicted);
            }
          }
          else go= false;
        }
        if (go) {
          ri.prec_kind= pk; ri.prec= pk? pv: 0;
          ri.symbols= set_insert (symbol, ri.symbols);
          if (assoc == 1) ri.left= true;
          else if (assoc == 2) ri.right= true;
          else ri.none= true;
        }
      }
    }
  }
  // consecutive duplicates removed
  {
    std::vector<aux_info_t> r;
    for (size_t k= 0; k < aux.size (); k++)
      if (r.empty () || !(r.back () == aux[k])) r.push_back (aux[k]);
    aux= r;
  }
  for (std::map<int,item_set_t>::iterator it= terminal_successors.begin ();
       it != terminal_successors.end (); ++it) {
    int symbol= it->first;
    int next_id= add_state (cons_preceding (symbol, pre), aux, it->second.sorted ());
    if (!state->terms.has (symbol)) {
      tentry_t te; te.reusable= true;
      state->terms.set (symbol, te);
    }
    tentry_t* te= state->terms.ref (symbol);
    if (!te->actions.empty ()) conflicted= set_insert (symbol, conflicted);
    te->actions.push_back (make_action (A_SHIFT, next_id, 0));
  }
  for (std::map<int,item_set_t>::iterator it= non_terminal_successors.begin ();
       it != non_terminal_successors.end (); ++it) {
    int next_id= add_state (cons_preceding (it->first, pre), aux, it->second.sorted ());
    state->nonterms.set (it->first, make_action (A_GOTO, next_id));
  }
  for (size_t k= 0; k < conflicted.size (); k++)
    handle_conflict (state, entries, pre, aux, conflicted[k],
                     reduction_infos[conflicted[k]]);
  if (state->terms.has (SYM_EONT)) {
    if (state->terms.size () > 1)
      throw ts_fail ("extra rules must have unambiguous endings");
  }
  else {
    for (size_t k= 0; k < non_terminal_extra_states.size (); k++) {
      int sym= non_terminal_extra_states[k].first;
      if (!state->terms.has (sym)) {
        tentry_t te; te.reusable= true;
        te.actions.push_back (make_action (A_SHIFT, non_terminal_extra_states[k].second, 0));
        state->terms.set (sym, te);
      }
    }
    for (size_t k= 0; k < g.extras.size (); k++) {
      int extra= g.extras[k];
      if (is_non_terminal (extra))
        state->nonterms.set (extra, make_action (A_SHIFT_EXTRA));
      else if (!state->terms.has (extra)) {
        tentry_t te; te.reusable= true;
        te.actions.push_back (make_action (A_SHIFT_EXTRA));
        state->terms.set (extra, te);
      }
    }
  }
  if (g.word >= 0) {
    int best= -1;
    for (size_t k= 0; k < entries.size (); k++) {
      const entry_t& e= entries[k];
      const step_t* step= e.item->at;
      int v= -1;
      if (step) { if (step->symbol == g.word) v= step->reserved; }
      else if (set_contains (pool.get (e.las), g.word)) v= e.reserved;
      if (v >= 0 && (best < 0 || v > best)) best= v;
    }
    if (best >= 0) state->reserved= g.reserved_sets[best];
  }
}

void
builder_t::build () {
  make_builder ();
  prod_info_t empty;
  production_infos.push_back (empty);
  add_state (0, std::vector<aux_info_t> (), std::vector<entry_t> ());
  {
    std::vector<entry_t> k;
    entry_t e= { items.make (-1, -1, 0, false), pool.intern (ivec (1, SYM_END)), 0 };
    k.push_back (e);
    add_state (0, std::vector<aux_info_t> (), k);
  }
  // the states of the non-terminal extras
  {
    std::map<int, item_set_t> by_first;
    for (size_t k= 0; k < g.extras.size (); k++) {
      int extra= g.extras[k];
      if (!is_non_terminal (extra)) continue;
      int v= sym_index (extra);
      for (int pid= g.var_prods[v].first; pid < g.var_prods[v].second; pid++) {
        int first_sym= g.prods[pid].steps[0].symbol;
        entry_t& e= by_first[first_sym].insert (items.make (v, pid, 1, false));
        e.las= pool.insert (e.las, SYM_EONT);
      }
    }
    for (std::map<int,item_set_t>::iterator it= by_first.begin (); it != by_first.end (); ++it) {
      if (is_non_terminal (it->first))
        throw ts_fail ("non-terminal used in a non-terminal extra rule " + g.symbol_name (it->first));
      int id= add_state (0, std::vector<aux_info_t> (), it->second.sorted ());
      non_terminal_extra_states.push_back (std::make_pair (it->first, id));
    }
  }
  while (queue_head < queue.size ()) {
    std::pair<int, std::vector<aux_info_t> > e= queue[queue_head];
    queue[queue_head].second.clear ();
    queue_head++;
    std::vector<entry_t> entries= closure (kernels[e.first]);
    kernels[e.first].clear ();
    kernels[e.first].shrink_to_fit ();
    add_actions (e.first, e.second, entries);
    // the state is complete
    states[e.first]->terms.freeze ();
    states[e.first]->nonterms.freeze ();
  }
  release_construction ();
}

template<class T> static void
release (T& x) {
  // free the memory of a container
  T ().swap (x);
}

void
builder_t::release_construction () {
  // what only the construction of the states needs (the item sets, the
  // lookahead sets, the signatures of the states): the next stages use the
  // states, the first and last sets and the production infos
  items.clear ();
  release (pool.sets); release (pool.ids); release (pool.unions);
  release (additions); release (first_ids);
  release (kernels); release (preceding); release (preceding_nodes);
  release (state_ids); release (core_ids); release (queue);
}


/******************************************************************************
* Character sets (sorted lists of half-open ranges, as in ts-base.scm)
******************************************************************************/

typedef std::vector<std::pair<int,int> > cset;
const int CS_END= 0x110000;

cset
cs_add_range (const cset& cs, int start, int end) {
  cset r;
  size_t i= 0;
  while (i < cs.size ()) {
    if (cs[i].first > end) break;
    if (cs[i].second >= start) {
      int s= std::min (start, cs[i].first), e= std::max (end, cs[i].second);
      i++;
      while (i < cs.size () && cs[i].first <= e) { e= std::max (e, cs[i].second); i++; }
      r.push_back (std::make_pair (s, e));
      r.insert (r.end (), cs.begin () + i, cs.end ());
      return r;
    }
    r.push_back (cs[i]);
    i++;
  }
  r.push_back (std::make_pair (start, end));
  r.insert (r.end (), cs.begin () + i, cs.end ());
  return r;
}

cset
cs_add (const cset& a, const cset& b) {
  cset r= a;
  for (size_t i= 0; i < b.size (); i++) r= cs_add_range (r, b[i].first, b[i].second);
  return r;
}

cset
cs_negate (const cset& cs) {
  cset r;
  int prev= 0;
  for (size_t i= 0; i < cs.size (); i++) {
    if (prev < cs[i].first) r.push_back (std::make_pair (prev, cs[i].first));
    prev= cs[i].second;
  }
  if (prev < CS_END) r.push_back (std::make_pair (prev, CS_END));
  return r;
}

cset
cs_intersect (const cset& a, const cset& b) {
  cset r;
  size_t i= 0, j= 0;
  while (i < a.size () && j < b.size ()) {
    if (a[i].second <= b[j].first) i++;
    else if (b[j].second <= a[i].first) j++;
    else {
      r.push_back (std::make_pair (std::max (a[i].first, b[j].first),
                                   std::min (a[i].second, b[j].second)));
      if (a[i].second < b[j].second) i++; else j++;
    }
  }
  return r;
}

inline cset cs_difference (const cset& a, const cset& b) { return cs_intersect (a, cs_negate (b)); }

ll
cs_count (const cset& cs) {
  ll n= 0;
  for (size_t i= 0; i < cs.size (); i++) n += cs[i].second - cs[i].first;
  return n;
}

int
cs_compare (const cset& a, const cset& b) {
  int c= cmp ((int) 0, 0);
  ll na= cs_count (a), nb= cs_count (b);
  if (na != nb) return na < nb? -1: 1;
  for (size_t i= 0; i < a.size () && i < b.size (); i++) {
    int la= a[i].second - a[i].first, lb= b[i].second - b[i].first;
    c= cmp (la, lb);
    if (c != 0) return c;
    if (a[i].first != b[i].first) return cmp (a[i].first, b[i].first);
  }
  return 0;
}

/******************************************************************************
* NFA cursors (ts-nfa.scm)
******************************************************************************/

struct transition_t {
  cset chars;
  bool sep;
  int prec;
  ivec states;
};

struct lexer_t {
  grammar_t& g;
  lexer_t (grammar_t& g2): g (g2) {}

  ivec closure (const ivec& ids) {
    std::vector<bool> seen (g.nfa.size (), false);
    ivec todo (ids.rbegin (), ids.rend ()), acc;
    while (!todo.empty ()) {
      int x= todo.back (); todo.pop_back ();
      if (seen[x]) continue;
      seen[x]= true;
      const grammar_t::nfa_node& s= g.nfa[x];
      if (s.kind == 1) { todo.push_back (s.b); todo.push_back (s.a); }
      else acc.push_back (x);
    }
    std::sort (acc.begin (), acc.end ());
    return acc;
  }

  std::vector<std::pair<int,int> > completions (const ivec& ids) {
    // (token . precedence)
    std::vector<std::pair<int,int> > r;
    for (size_t i= 0; i < ids.size (); i++)
      if (g.nfa[ids[i]].kind == 2) r.push_back (std::make_pair (g.nfa[ids[i]].a, g.nfa[ids[i]].prec));
    return r;
  }

  bool any_separator (const ivec& ids) {
    for (size_t i= 0; i < ids.size (); i++)
      if (g.nfa[ids[i]].kind == 0 && g.nfa[ids[i]].sep) return true;
    return false;
  }

  std::vector<transition_t> transitions (const ivec& ids) {
    std::vector<transition_t> result;
    for (size_t k= 0; k < ids.size (); k++) {
      const grammar_t::nfa_node& raw= g.nfa[ids[k]];
      if (raw.kind != 0) continue;
      cset chars= raw.chars;
      bool sep= raw.sep;
      int prec= raw.prec, state= raw.a;
      for (size_t i= 0; i < result.size () && !chars.empty (); i++) {
        cset inter= cs_intersect (result[i].chars, chars);
        if (inter.empty ()) continue;
        cset rest= cs_difference (result[i].chars, inter);
        transition_t nw;
        nw.chars= inter;
        nw.sep= result[i].sep && sep;
        nw.prec= std::max (result[i].prec, prec);
        nw.states= set_insert (state, result[i].states);
        chars= cs_difference (chars, inter);
        if (rest.empty ()) result[i]= nw;
        else {
          result[i].chars= rest;
          result.push_back (nw);
        }
      }
      if (!chars.empty ()) {
        transition_t t;
        t.chars= chars; t.sep= sep; t.prec= prec; t.states= ivec (1, state);
        result.push_back (t);
      }
    }
    std::vector<transition_t> acc;
    for (size_t k= 0; k < result.size (); k++) {
      const transition_t& t= result[k];
      int same= -1;
      for (size_t j= 0; j < acc.size (); j++)
        if (acc[j].states == t.states && acc[j].sep == t.sep && acc[j].prec == t.prec) { same= (int) j; break; }
      if (same >= 0) acc[same].chars= cs_add (acc[same].chars, t.chars);
      else acc.push_back (t);
    }
    std::stable_sort (acc.begin (), acc.end (), [] (const transition_t& a, const transition_t& b) {
      return cs_compare (a.chars, b.chars) < 0; });
    return acc;
  }

  int variable_index (int state) {
    int lo= 0, hi= g.n_terminals;
    while (lo < hi) {
      int mid= (lo + hi) / 2;
      if (g.start_state[mid] < state) lo= mid + 1; else hi= mid;
    }
    return lo;
  }

  bool prefer_token (int lprec, int ltok, int rprec, int rtok) {
    if (lprec < rprec) return false;
    if (lprec > rprec) return true;
    int l= g.implicit_prec[ltok], r= g.implicit_prec[rtok];
    if (l < r) return false;
    if (l > r) return true;
    return ltok < rtok;
  }

  bool prefer_transition (const transition_t& t, int completed_id, int completed_prec, bool has_sep) {
    if (t.prec < completed_prec) return false;
    if (t.prec == completed_prec) {
      if (t.sep) return false;
      if (!has_sep) return true;
      for (size_t i= 0; i < t.states.size (); i++)
        if (variable_index (t.states[i]) == completed_id) return true;
      return false;
    }
    return true;
  }
};

/******************************************************************************
* Lex tables (build_lex_table.rs, as ts-build.scm)
******************************************************************************/

struct lex_adv_t { cset chars; int state; bool main; };

struct lex_state_t {
  int accept;              // symbol, or -1
  int eof, eof_main;       // eof action: state or -1
  std::vector<lex_adv_t> adv;
};

struct lex_builder_t {
  lexer_t lx;
  std::vector<lex_state_t> table;
  std::map<std::pair<bool, ivec>, int> ids;
  std::vector<std::pair<int, std::pair<ivec,bool> > > pending;
  size_t pending_head;
  lex_builder_t (grammar_t& g): lx (g), pending_head (0) {}

  std::pair<int,bool> add_state (const ivec& nfa_states, bool eof_valid) {
    ivec c= lx.closure (nfa_states);
    std::pair<bool,ivec> key (eof_valid, c);
    std::map<std::pair<bool,ivec>,int>::iterator it= ids.find (key);
    if (it != ids.end ()) return std::make_pair (it->second, false);
    int id= (int) table.size ();
    lex_state_t s; s.accept= -1; s.eof= -1; s.eof_main= 0;
    table.push_back (s);
    ids[key]= id;
    pending.push_back (std::make_pair (id, std::make_pair (c, eof_valid)));
    return std::make_pair (id, true);
  }

  void populate (int id, const ivec& c, bool eof_valid) {
    bool has_completion= false;
    std::pair<int,int> completion (0, 0);   // (token . precedence)
    std::vector<std::pair<int,int> > cs= lx.completions (c);
    for (size_t k= 0; k < cs.size (); k++)
      if (!(has_completion && lx.prefer_token (completion.second, completion.first,
                                               cs[k].second, cs[k].first))) {
        completion= cs[k]; has_completion= true; }
    std::vector<transition_t> ts= lx.transitions (c);
    bool has_sep= lx.any_separator (c);
    if (eof_valid) {
      int next= add_state (ivec (), false).first;
      table[id].eof= next; table[id].eof_main= 1;
    }
    for (size_t k= 0; k < ts.size (); k++) {
      const transition_t& t= ts[k];
      if (has_completion && !lx.prefer_transition (t, completion.first, completion.second, has_sep))
        continue;
      int next= add_state (t.states, eof_valid && t.sep).first;
      lex_adv_t a; a.chars= t.chars; a.state= next; a.main= !t.sep;
      table[id].adv.push_back (a);
    }
    if (has_completion) table[id].accept= terminal (completion.first);
    else if (c.empty ()) table[id].accept= SYM_END;
  }

  int add_state_for_tokens (const ivec& tokens) {
    bool eof_valid= false;
    ivec nfa_states;
    for (size_t k= 0; k < tokens.size (); k++)
      if (is_terminal (tokens[k])) nfa_states.push_back (lx.g.start_state[sym_index (tokens[k])]);
      else eof_valid= true;
    std::pair<int,bool> r= add_state (nfa_states, eof_valid);
    while (pending_head < pending.size ()) {
      std::pair<int, std::pair<ivec,bool> > e= pending[pending_head++];
      populate (e.first, e.second.first, e.second.second);
    }
    return r.first;
  }
};

static int
opt_cmp_int (int x, int y) {
  if (x < 0 && y < 0) return 0;
  if (x < 0) return -1;
  if (y < 0) return 1;
  return cmp (x, y);
}

static bool
lex_state_less (const lex_state_t& a, const lex_state_t& b) {
  int c= opt_cmp_int (a.accept, b.accept);
  if (c != 0) return c < 0;
  if (a.eof < 0 && b.eof >= 0) return true;
  if (a.eof >= 0 && b.eof < 0) return false;
  if (a.eof >= 0) {
    c= cmp (a.eof, b.eof);
    if (c == 0) c= cmp (a.eof_main, b.eof_main);
    if (c != 0) return c < 0;
  }
  for (size_t i= 0; i < a.adv.size () && i < b.adv.size (); i++) {
    c= cs_compare (a.adv[i].chars, b.adv[i].chars);
    if (c == 0) c= cmp (a.adv[i].state, b.adv[i].state);
    if (c == 0) c= cmp (a.adv[i].main? 1: 0, b.adv[i].main? 1: 0);
    if (c != 0) return c < 0;
  }
  return a.adv.size () < b.adv.size ();
}

static std::vector<lex_state_t>
renumber_lex (const std::vector<lex_state_t>& table, const ivec& order, const ivec& new_of) {
  std::vector<lex_state_t> r;
  for (size_t k= 0; k < order.size (); k++) {
    lex_state_t s= table[order[k]];
    if (s.eof >= 0) s.eof= new_of[s.eof];
    for (size_t j= 0; j < s.adv.size (); j++) s.adv[j].state= new_of[s.adv[j].state];
    r.push_back (s);
  }
  return r;
}

/******************************************************************************
* The stages after the construction
******************************************************************************/

const int DOES_MATCH_VALID_CONTINUATION= 4, DOES_MATCH_SEPARATORS= 8, MATCHES_SAME_STRING= 16;

struct tables_t {
  grammar_t g;
  builder_t* b;
  int n;                        // terminals
  std::vector<bool> contains, without_word;   // the coincident tokens
  ivec tc;                      // the token conflicts, n * n statuses
  ivec keywords;
  ivec symbols;
  tables_t (): b (NULL) {}
  ~tables_t () { delete b; }

  int status (int i, int j) { return tc[n * i + j]; }
  bool does_conflict (int i, int j) {
    return (status (i, j) & (DOES_MATCH_VALID_CONTINUATION | DOES_MATCH_SEPARATORS |
                             MATCHES_SAME_STRING)) != 0; }
  bool does_match_shorter_or_longer (int i, int j) {
    return (status (i, j) & (DOES_MATCH_VALID_CONTINUATION | DOES_MATCH_SEPARATORS)) != 0 &&
           (status (j, i) & DOES_MATCH_SEPARATORS) == 0; }
  bool coincident (int a, int b) { return contains[n * a + b]; }

  void coincident_index ();
  void token_conflicts ();
  ivec following_tokens (int k);
  std::vector<ivec> following;
  void compute_following ();
  void populate_error_state ();
  void used_symbols ();
  std::vector<state_t*> merge (std::vector<state_t*> states);
  void remove_unit_reductions (std::vector<state_t*>& states);
  std::vector<state_t*> remove_unused (std::vector<state_t*> states);
  std::vector<state_t*> reorder (std::vector<state_t*> states);
  // the lex tables
  bool does_match_prefix (int i, int j) { return (status (i, j) & 1) != 0; }
  bool does_overlap (int i, int j) {
    return (status (i, j) & (DOES_MATCH_SEPARATORS | 1 | MATCHES_SAME_STRING | 2)) != 0; }
  ivec lex_of;                  // the lex state of each parse state
  std::vector<lex_state_t> main_table, keyword_table;
  void build_lex_tables (std::vector<state_t*>& states);
  // the language for the C runtime
  void export_language (reader_t& r, std::string& out);
};

void
tables_t::compute_following () {
  grammar_t& g= this->g;
  following.assign (n, ivec ());
  ivec all;
  for (int i= 0; i < n; i++) all.push_back (terminal (i));
  for (size_t pi= 0; pi < g.prods.size (); pi++) {
    const prod_t& p= g.prods[pi];
    for (size_t i= 1; i < p.steps.size (); i++) {
      const ivec& left= b->last[p.steps[i-1].symbol];
      ivec right= terminals_of (b->first[p.steps[i].symbol]);
      ivec reserved= terminals_of (g.reserved_sets[b->reserved_first[p.steps[i].symbol]]);
      for (size_t k= 0; k < left.size (); k++)
        if (is_terminal (left[k])) {
          int x= sym_index (left[k]);
          following[x]= set_union (following[x], right);
          following[x]= set_union (following[x], reserved);
        }
    }
  }
  for (size_t k= 0; k < g.extras.size (); k++) {
    int extra= g.extras[k];
    if (!is_terminal (extra)) continue;
    for (int j= 0; j < n; j++) following[j]= set_insert (extra, following[j]);
    following[sym_index (extra)]= all;
  }
}

void
tables_t::coincident_index () {
  contains.assign ((size_t) n * n, false);
  without_word.assign ((size_t) n * n, false);
  int word= g.word;
  for (size_t si= 0; si < b->states.size (); si++) {
    state_t* s= b->states[si];
    ivec ts;
    for (int k= 0; k < s->terms.size (); k++)
      if (is_terminal (s->terms.keys[k])) ts.push_back (sym_index (s->terms.keys[k]));
    bool has_word= word >= 0 && s->terms.has (word);
    for (size_t x= 0; x < ts.size (); x++)
      for (size_t y= x; y < ts.size (); y++) {
        int a= ts[x], c= ts[y];
        contains[(size_t) a * n + c]= true;
        contains[(size_t) c * n + a]= true;
        if (!has_word) {
          without_word[(size_t) a * n + c]= true;
          without_word[(size_t) c * n + a]= true;
        }
      }
  }
}

/* The token conflicts (token_conflicts.rs, make-token-conflict-map of
   ts-build.scm): for each pair of tokens i and j, the NFAs of both are run
   on the same characters, and the statuses say how one can get in the way of
   the other: tc[n * i + j] for i, tc[n * j + i] for j. */

const int MATCHES_PREFIX= 1, DOES_MATCH_CONTINUATION= 2, MATCHES_DIFFERENT_STRING= 32;

void
tables_t::token_conflicts () {
  lexer_t lx (g);
  int n_nfa= (int) g.nfa.size ();
  ivec var_of (n_nfa, 0);
  for (int s= 0; s < n_nfa; s++) var_of[s]= lx.variable_index (s);
  std::vector<cset> starting (n), following_chars (n);
  for (int k= 0; k < n; k++) {
    ivec ids= lx.closure (ivec (1, g.start_state[k]));
    for (size_t x= 0; x < ids.size (); x++)
      if (g.nfa[ids[x]].kind == 0) starting[k]= cs_add (starting[k], g.nfa[ids[x]].chars);
  }
  for (int k= 0; k < n; k++)
    for (size_t x= 0; x < following[k].size (); x++)
      if (is_terminal (following[k][x]))
        following_chars[k]= cs_add (following_chars[k], starting[sym_index (following[k][x])]);
  tc.assign ((size_t) n * n, 0);
  for (int i= 0; i < n; i++)
    for (int j= 0; j < i; j++) {
      int r0= 0, r1= 0;
      auto mark= [&] (int who, int bit) { if (who == i) r0 |= bit; else r1 |= bit; };
      std::unordered_set<ivec, vec_hash> visited;
      std::vector<ivec> queue;
      ivec first;
      first.push_back (g.start_state[i]);
      first.push_back (g.start_state[j]);
      queue.push_back (first);
      while (!queue.empty ()) {
        ivec state_set= queue.back ();
        queue.pop_back ();
        int first_live= var_of[state_set[0]];
        bool all_same= true;
        for (size_t x= 0; x < state_set.size () && all_same; x++)
          if (var_of[state_set[x]] != first_live) all_same= false;
        if (all_same) { mark (first_live == i? i: j, MATCHES_DIFFERENT_STRING); continue; }
        ivec ids= lx.closure (state_set);
        int within_sep= -1;   // unknown
        bool has_completion= false;
        int c_id= 0, c_prec= 0;
        std::vector<std::pair<int,int> > cs= lx.completions (ids);
        for (size_t x= 0; x < cs.size (); x++) {
          int id= cs[x].first, prec= cs[x].second;
          if (within_sep < 0) within_sep= lx.any_separator (ids)? 1: 0;
          if (within_sep == 1) mark (id == i? i: j, DOES_MATCH_SEPARATORS);
          if (has_completion) {
            if (id != c_id) {
              int preferred;
              if (lx.prefer_token (c_prec, c_id, prec, id)) preferred= c_id;
              else { c_id= id; c_prec= prec; preferred= id; }
              mark (preferred == i? i: j, MATCHES_SAME_STRING);
            }
          }
          else { has_completion= true; c_id= id; c_prec= prec; }
        }
        std::vector<transition_t> ts= lx.transitions (ids);
        for (size_t x= 0; x < ts.size (); x++) {
          const transition_t& t= ts[x];
          if (has_completion) {
            int prev= -1, advanced= -1;
            bool found= false;
            for (size_t k= 0; k < t.states.size () && !found; k++) {
              int v= var_of[t.states[k]];
              if (v == prev) continue;
              if (v == c_id) found= true;
              else { prev= v; advanced= v; }
            }
            if (advanced >= 0 && !found) {
              if (lx.prefer_transition (t, c_id, c_prec, within_sep == 1)) {
                int other= advanced == i? j: i;
                mark (advanced == i? i: j, DOES_MATCH_CONTINUATION);
                if (!cs_intersect (t.chars, following_chars[other]).empty ())
                  mark (advanced == i? i: j, DOES_MATCH_VALID_CONTINUATION);
              }
              else mark (c_id == i? i: j, MATCHES_PREFIX);
            }
          }
          if (visited.insert (t.states).second) queue.push_back (t.states);
        }
      }
      tc[(size_t) n * i + j]= r0;
      tc[(size_t) n * j + i]= r1;
    }
}

void
tables_t::populate_error_state () {
  state_t* state= b->states[0];
  int word= g.word;
  std::vector<bool> conflict_free (n, false);
  ivec free_list;
  for (int i= 0; i < n; i++) {
    bool bad= false;
    for (int j= 0; j < n && !bad; j++)
      if (j != i && !coincident (i, j) && does_match_shorter_or_longer (i, j)) bad= true;
    if (!bad) { conflict_free[i]= true; free_list.push_back (i); }
  }
  tentry_t recover;
  recover.reusable= false;
  recover.actions.push_back (make_action (A_RECOVER));
  for (int i= 0; i < n; i++) {
    int s= terminal (i);
    bool skip= false;
    if (!conflict_free[i] &&
        std::find (keywords.begin (), keywords.end (), s) == keywords.end () &&
        word != s) {
      for (size_t k= 0; k < free_list.size () && !skip; k++)
        if (!coincident (i, free_list[k]) && does_conflict (i, free_list[k])) skip= true;
    }
    if (!skip && !state->terms.has (s)) state->terms.set (s, recover);
  }
  for (int i= 0; i < g.n_externals; i++)
    if (g.external_internal[i] < 0 && !state->terms.has (external (i)))
      state->terms.set (external (i), recover);
  state->terms.set (SYM_END, recover);
}

void
tables_t::used_symbols () {
  std::unordered_map<int,bool> used;
  for (size_t si= 0; si < b->states.size (); si++) {
    state_t* s= b->states[si];
    for (int k= 0; k < s->terms.size (); k++) used[s->terms.keys[k]]= true;
    for (int k= 0; k < s->nonterms.size (); k++) used[s->nonterms.keys[k]]= true;
  }
  ivec terms;
  for (int i= 0; i < n; i++) if (used.count (terminal (i))) terms.push_back (terminal (i));
  int word= g.word;
  if (word >= 0 && std::find (terms.begin (), terms.end (), word) != terms.end ()) {
    terms.erase (std::find (terms.begin (), terms.end (), word));
    terms.insert (terms.begin (), word);
  }
  symbols.clear ();
  symbols.push_back (SYM_END);
  symbols.insert (symbols.end (), terms.begin (), terms.end ());
  for (int i= 0; i < g.n_externals; i++) if (used.count (external (i))) symbols.push_back (external (i));
  for (int i= 0; i < g.n_vars; i++) if (used.count (non_terminal (i))) symbols.push_back (non_terminal (i));
}

static int
last_shift_target (const action_list_t& actions) {
  if (actions.empty () || actions.back ().kind != A_SHIFT) return -1;
  return actions.back ().a;
}

template<typename F> static bool
split_groups (std::vector<ivec>& groups, ivec& group_of, size_t start,
              const F& should_split,
              std::vector<bool>* dirty, const std::vector<ivec>* preds) {
  bool result= false;
  std::vector<bool> is_split (group_of.size (), false);
  for (size_t gid= start; gid < groups.size (); gid++) {
    if (dirty && !(*dirty)[gid]) continue;
    if (dirty) (*dirty)[gid]= false;
    ivec ids= groups[gid];
    ivec split;
    for (size_t x= 0; x < ids.size (); x++) {
      int l= ids[x];
      if (is_split[l]) continue;
      for (size_t y= x + 1; y < ids.size (); y++) {
        int r= ids[y];
        if (!is_split[r] && should_split (l, r)) {
          split.push_back (r);
          is_split[r]= true;
        }
      }
    }
    if (!split.empty ()) {
      result= true;
      ivec rest;
      for (size_t x= 0; x < ids.size (); x++) if (!is_split[ids[x]]) rest.push_back (ids[x]);
      groups[gid]= rest;
      int nw= (int) groups.size ();
      for (size_t x= 0; x < split.size (); x++) { group_of[split[x]]= nw; is_split[split[x]]= false; }
      groups.push_back (split);
      if (dirty) {
        dirty->push_back (true);
        for (size_t x= 0; x < split.size (); x++) {
          const ivec& ps= (*preds)[split[x]];
          for (size_t k= 0; k < ps.size (); k++) (*dirty)[group_of[ps[k]]]= true;
        }
      }
    }
  }
  return result;
}

static void
remap_states (std::vector<state_t*>& states, const ivec& fun) {
  for (size_t si= 0; si < states.size (); si++) {
    state_t* s= states[si];
    for (int k= 0; k < s->terms.size (); k++) {
      action_list_t& as= s->terms.vals[k].actions;
      for (size_t j= 0; j < as.size (); j++)
        if (as[j].kind == A_SHIFT) as[j].a= fun[as[j].a];
    }
    for (int k= 0; k < s->nonterms.size (); k++)
      if (s->nonterms.vals[k].kind == A_GOTO) s->nonterms.vals[k].a= fun[s->nonterms.vals[k].a];
  }
}

std::vector<state_t*>
tables_t::merge (std::vector<state_t*> states) {
  int ns= (int) states.size ();
  int core_count= 0;
  for (int i= 0; i < ns; i++) core_count= std::max (core_count, states[i]->core);
  core_count++;
  std::vector<ivec> groups (core_count);
  ivec group_of (ns, 0);
  for (int i= 0; i < ns; i++) { groups[states[i]->core].push_back (i); group_of[i]= states[i]->core; }
  int word= g.word;
  ivec internal_external;
  for (int i= 0; i < g.n_externals; i++)
    if (g.external_internal[i] >= 0) internal_external.push_back (g.external_internal[i]);
  std::vector<std::vector<bool> > terminal_sets (ns, std::vector<bool> (n, false));
  for (int i= 0; i < ns; i++)
    for (int k= 0; k < states[i]->terms.size (); k++)
      if (is_terminal (states[i]->terms.keys[k]))
        terminal_sets[i][sym_index (states[i]->terms.keys[k])]= true;
  std::vector<ivec> conflict_rows (n);
  for (int i= 0; i < n; i++)
    for (int j= 0; j < n; j++) if (does_conflict (i, j)) conflict_rows[i].push_back (j);
  std::vector<bool> keyword_set (n, false);
  for (size_t k= 0; k < keywords.size (); k++) keyword_set[sym_index (keywords[k])]= true;
  auto token_conflicts= [&] (int right, int new_token) -> bool {
    if (new_token == SYM_EONT) return true;
    if (is_external (new_token)) return true;
    if (set_contains (states[right]->reserved, new_token)) return false;
    if (is_terminal (new_token) &&
        std::find (internal_external.begin (), internal_external.end (), new_token) !=
        internal_external.end ()) return true;
    bool is_word= word == new_token;
    bool is_keyword= word >= 0 && is_terminal (new_token) && keyword_set[sym_index (new_token)];
    const std::vector<bool>& set= terminal_sets[right];
    const ivec& row= conflict_rows[sym_index (new_token)];
    for (size_t k= 0; k < row.size (); k++) {
      int j= row[k];
      if (set[j] && !(is_keyword && terminal (j) == word) && !(is_word && keyword_set[j]))
        return true;
    }
    return false;
  };
  // for the terminals, 0 unknown, 1 no conflict, 2 conflict, in two bits
  // (states times terminals: a gigabyte in bytes for a large grammar)
  std::vector<unsigned char> token_memo (((size_t) ns * n + 3) / 4, 0);
  auto token_conflicts_memo= [&] (int right, int new_token) -> bool {
    if (!is_terminal (new_token)) return token_conflicts (right, new_token);
    size_t i= (size_t) right * n + sym_index (new_token);
    unsigned char& byte= token_memo[i / 4];
    int shift= (int) (i % 4) * 2;
    int m= (byte >> shift) & 3;
    if (m == 0) {
      m= token_conflicts (right, new_token)? 2: 1;
      byte= (unsigned char) (byte | (m << shift));
    }
    return m == 2;
  };
  auto entries_conflict= [&] (const tentry_t& e1, const tentry_t& e2) -> bool {
    const action_list_t& a1= e1.actions;
    const action_list_t& a2= e2.actions;
    if (a1 == a2) return false;
    if (a1.size () != a2.size ()) return true;
    for (size_t k= 0; k < a1.size (); k++) {
      const action_t& x= a1[k];
      const action_t& y= a2[k];
      if (x.kind == A_SHIFT && y.kind == A_SHIFT) {
        if (!(group_of[x.a] == group_of[y.a] && x.b == y.b)) return true;
      }
      else if (!(x == y)) return true;
    }
    return false;
  };
  // the sorted (token . index of the entry) of each state
  std::vector<std::vector<std::pair<int,int> > > sorted_entries (ns);
  std::vector<std::vector<std::pair<int,int> > > shift_targets (ns), gotos (ns);
  for (int i= 0; i < ns; i++) {
    state_t* s= states[i];
    for (int k= 0; k < s->terms.size (); k++) {
      sorted_entries[i].push_back (std::make_pair (s->terms.keys[k], k));
      int t= last_shift_target (s->terms.vals[k].actions);
      if (t >= 0) shift_targets[i].push_back (std::make_pair (s->terms.keys[k], t));
    }
    for (int k= 0; k < s->nonterms.size (); k++)
      gotos[i].push_back (std::make_pair (s->nonterms.keys[k], k));
    std::stable_sort (sorted_entries[i].begin (), sorted_entries[i].end (),
                      [] (const std::pair<int,int>& a, const std::pair<int,int>& b) { return a.first < b.first; });
    std::stable_sort (shift_targets[i].begin (), shift_targets[i].end (),
                      [] (const std::pair<int,int>& a, const std::pair<int,int>& b) { return a.first < b.first; });
    std::stable_sort (gotos[i].begin (), gotos[i].end (),
                      [] (const std::pair<int,int>& a, const std::pair<int,int>& b) { return a.first < b.first; });
  }
  auto states_conflict= [&] (int s1, int s2) -> bool {
    const std::vector<std::pair<int,int> >& v1= sorted_entries[s1];
    const std::vector<std::pair<int,int> >& v2= sorted_entries[s2];
    size_t i= 0, j= 0, n1= v1.size (), n2= v2.size ();
    while (i < n1 || j < n2) {
      if (j == n2) { if (token_conflicts_memo (s2, v1[i].first)) return true; i++; }
      else if (i == n1) { if (token_conflicts_memo (s1, v2[j].first)) return true; j++; }
      else if (v1[i].first < v2[j].first) { if (token_conflicts_memo (s2, v1[i].first)) return true; i++; }
      else if (v1[i].first > v2[j].first) { if (token_conflicts_memo (s1, v2[j].first)) return true; j++; }
      else {
        if (entries_conflict (states[s1]->terms.vals[v1[i].second],
                              states[s2]->terms.vals[v2[j].second])) return true;
        i++; j++;
      }
    }
    return false;
  };
  auto successors_differ= [&] (int s1, int s2) -> bool {
    {
      const std::vector<std::pair<int,int> >& v1= shift_targets[s1];
      const std::vector<std::pair<int,int> >& v2= shift_targets[s2];
      size_t i= 0, j= 0;
      while (i < v1.size () && j < v2.size ()) {
        if (v1[i].first < v2[j].first) i++;
        else if (v1[i].first > v2[j].first) j++;
        else {
          if (group_of[v1[i].second] != group_of[v2[j].second]) return true;
          i++; j++;
        }
      }
    }
    const std::vector<std::pair<int,int> >& v1= gotos[s1];
    const std::vector<std::pair<int,int> >& v2= gotos[s2];
    size_t i= 0, j= 0;
    while (i < v1.size () && j < v2.size ()) {
      if (v1[i].first < v2[j].first) i++;
      else if (v1[i].first > v2[j].first) j++;
      else {
        const action_t& a= states[s1]->nonterms.vals[v1[i].second];
        const action_t& b= states[s2]->nonterms.vals[v2[j].second];
        bool differ;
        if (a.kind == A_SHIFT_EXTRA && b.kind == A_SHIFT_EXTRA) differ= false;
        else if (a.kind == A_GOTO && b.kind == A_GOTO) differ= group_of[a.a] != group_of[b.a];
        else differ= true;
        if (differ) return true;
        i++; j++;
      }
    }
    return false;
  };
  std::vector<ivec> preds (ns);
  for (int i= 0; i < ns; i++) {
    for (size_t k= 0; k < shift_targets[i].size (); k++) preds[shift_targets[i][k].second].push_back (i);
    for (size_t k= 0; k < gotos[i].size (); k++) {
      const action_t& a= states[i]->nonterms.vals[gotos[i][k].second];
      if (a.kind == A_GOTO) preds[a.a].push_back (i);
    }
  }
  // (the Scheme code conses the predecessors: the order does not matter)
  lap ("merge: setup");
  // the split by conflicts, as split_groups with states_conflict, but two
  // states with the same terminal entries (the shift targets up to their
  // group) have the same fate: only the first of each class is compared
  auto same_entries= [&] (int s1, int s2) -> bool {
    if (states[s1]->reserved != states[s2]->reserved) return false;
    const std::vector<std::pair<int,int> >& v1= sorted_entries[s1];
    const std::vector<std::pair<int,int> >& v2= sorted_entries[s2];
    if (v1.size () != v2.size ()) return false;
    for (size_t k= 0; k < v1.size (); k++) {
      if (v1[k].first != v2[k].first) return false;
      const action_list_t& a1= states[s1]->terms.vals[v1[k].second].actions;
      const action_list_t& a2= states[s2]->terms.vals[v2[k].second].actions;
      if (a1.size () != a2.size ()) return false;
      for (size_t j= 0; j < a1.size (); j++) {
        const action_t& x= a1[j];
        const action_t& y= a2[j];
        if (x.kind != y.kind || x.b != y.b || x.c != y.c || x.d != y.d) return false;
        if (x.kind == A_SHIFT? group_of[x.a] != group_of[y.a]: x.a != y.a) return false;
      }
    }
    return true;
  };
  auto entries_hash= [&] (int s) -> size_t {
    size_t h= states[s]->reserved.size ();
    const std::vector<std::pair<int,int> >& v= sorted_entries[s];
    for (size_t k= 0; k < v.size (); k++) {
      h= h * 1000003u ^ (size_t) v[k].first;
      const action_list_t& as= states[s]->terms.vals[v[k].second].actions;
      for (size_t j= 0; j < as.size (); j++)
        h= h * 1000003u ^ (size_t) (as[j].kind == A_SHIFT? group_of[as[j].a]: as[j].a);
    }
    return h;
  };
  for (size_t gid= 0; gid < groups.size (); gid++) {
    const ivec ids= groups[gid];
    // per position: the position of the state which splits it, or -1
    ivec splitter (ids.size (), -1);
    ivec staying;   // positions of the first staying state of each class
    std::unordered_map<size_t, ivec> classes;   // hash -> positions of firsts
    for (size_t x= 0; x < ids.size (); x++) {
      ivec& firsts= classes[entries_hash (ids[x])];
      int first= -1;
      for (size_t k= 0; k < firsts.size () && first < 0; k++)
        if (same_entries (ids[firsts[k]], ids[x])) first= firsts[k];
      if (first >= 0) { splitter[x]= splitter[first]; continue; }
      firsts.push_back ((int) x);
      for (size_t k= 0; k < staying.size (); k++)
        if (states_conflict (ids[staying[k]], ids[x])) { splitter[x]= staying[k]; break; }
      if (splitter[x] < 0) staying.push_back ((int) x);
    }
    // the split states in the order in which split_groups finds them
    std::vector<std::pair<int,int> > split;
    ivec rest;
    for (size_t x= 0; x < ids.size (); x++)
      if (splitter[x] < 0) rest.push_back (ids[x]);
      else split.push_back (std::make_pair (splitter[x], (int) x));
    if (split.empty ()) continue;
    std::stable_sort (split.begin (), split.end ());
    groups[gid]= rest;
    int nw= (int) groups.size ();
    ivec moved;
    for (size_t k= 0; k < split.size (); k++) {
      moved.push_back (ids[split[k].second]);
      group_of[ids[split[k].second]]= nw;
    }
    groups.push_back (moved);
  }
  lap ("merge: conflicts");
  std::vector<bool> dirty (groups.size (), true);
  while (split_groups (groups, group_of, 0, successors_differ, &dirty, &preds)) {}
  lap ("merge: successors");
  // the groups of the error state and of the start state come first
  for (int k= 0; k < 2; k++) {
    size_t pos= 0;
    for (size_t x= 0; x < groups.size (); x++)
      if (std::find (groups[x].begin (), groups[x].end (), k) != groups[x].end ()) { pos= x; break; }
    std::swap (groups[k], groups[pos]);
  }
  for (size_t k= 0; k < groups.size (); k++)
    for (size_t x= 0; x < groups[k].size (); x++) group_of[groups[k][x]]= (int) k;
  std::vector<state_t*> merged;
  std::vector<bool> kept (ns, false);
  for (size_t k= 0; k < groups.size (); k++) {
    const ivec& ids= groups[k];
    state_t* s= states[ids[0]];
    kept[ids[0]]= true;
    for (size_t x= 1; x < ids.size (); x++) {
      state_t* o= states[ids[x]];
      if (o->eof_gated) s->eof_gated= true;
      for (int j= 0; j < o->terms.size (); j++) s->terms.set (o->terms.keys[j], o->terms.vals[j]);
      for (int j= 0; j < o->nonterms.size (); j++) s->nonterms.set (o->nonterms.keys[j], o->nonterms.vals[j]);
      ivec r= set_union (s->reserved, o->reserved);
      ivec r2;
      for (size_t j= 0; j < r.size (); j++) if (!s->terms.has (r[j])) r2.push_back (r[j]);
      s->reserved= r2;
    }
    merged.push_back (s);
  }
  for (int i= 0; i < ns; i++) if (!kept[i]) delete states[i];
  remap_states (merged, group_of);
  return merged;
}

void
tables_t::remove_unit_reductions (std::vector<state_t*>& states) {
  std::unordered_map<int,bool> aliased, default_aliased, supertype, extra;
  for (size_t pi= 0; pi < g.prods.size (); pi++)
    for (size_t k= 0; k < g.prods[pi].steps.size (); k++)
      if (g.prods[pi].steps[k].alias >= 0) aliased[g.prods[pi].steps[k].symbol]= true;
  for (size_t k= 0; k < g.default_aliased.size (); k++) default_aliased[g.default_aliased[k]]= true;
  for (size_t k= 0; k < g.supertypes.size (); k++) supertype[g.supertypes[k]]= true;
  for (size_t k= 0; k < g.extras.size (); k++) extra[g.extras[k]]= true;
  std::unordered_map<int,int> unit;
  for (size_t i= 0; i < states.size (); i++) {
    state_t* s= states[i];
    if (s->eof_gated) continue;
    int symbol= -1;
    bool only= true;
    for (int k= 0; k < s->terms.size () && only; k++) {
      const action_list_t& as= s->terms.vals[k].actions;
      for (size_t j= 0; j < as.size () && only; j++) {
        const action_t& a= as[j];
        if (a.kind == A_SHIFT_EXTRA) continue;
        int sym= a.b;
        if (a.kind == A_REDUCE && a.b == 1 && a.d == 0 &&
            (sym= a.a, !default_aliased.count (sym) && !supertype.count (sym) &&
             !extra.count (sym) && !aliased.count (sym) &&
             g.var_kinds[sym_index (sym)] != VT_NAMED && (symbol < 0 || symbol == sym)))
          symbol= sym;
        else only= false;
      }
    }
    if (symbol >= 0 && only) unit[(int) i]= symbol;
  }
  if (unit.empty ()) return;
  for (size_t i= 0; i < states.size (); i++) {
    state_t* s= states[i];
    while (true) {
      bool done= true;
      for (int k= 0; k < s->nonterms.size (); k++) {
        action_t a= s->nonterms.vals[k];
        if (a.kind != A_GOTO) continue;
        std::unordered_map<int,int>::iterator it= unit.find (a.a);
        if (it == unit.end ()) continue;
        done= false;
        action_t* g2= s->nonterms.ref (it->second);
        if (g2 && g2->kind == A_GOTO) s->nonterms.vals[k]= *g2;
      }
      for (int k= 0; k < s->terms.size (); k++) {
        action_list_t& as= s->terms.vals[k].actions;
        for (size_t j= 0; j < as.size (); j++) {
          if (as[j].kind != A_SHIFT) continue;
          std::unordered_map<int,int>::iterator it= unit.find (as[j].a);
          action_t* g2= it == unit.end ()? NULL: s->nonterms.ref (it->second);
          if (g2 && g2->kind == A_GOTO && g2->a != as[j].a) {
            done= false;
            as[j]= make_action (A_SHIFT, g2->a, as[j].b);
          }
        }
      }
      if (done) break;
    }
  }
}

std::vector<state_t*>
tables_t::remove_unused (std::vector<state_t*> states) {
  int ns= (int) states.size ();
  std::vector<bool> used (ns, false);
  ivec replacement (ns, 0);
  used[0]= true;
  if (ns > 1) used[1]= true;
  for (int i= 0; i < ns; i++) {
    state_t* s= states[i];
    for (int k= 0; k < s->terms.size (); k++) {
      const action_list_t& as= s->terms.vals[k].actions;
      for (size_t j= 0; j < as.size (); j++) if (as[j].kind == A_SHIFT) used[as[j].a]= true;
    }
    for (int k= 0; k < s->nonterms.size (); k++)
      if (s->nonterms.vals[k].kind == A_GOTO) used[s->nonterms.vals[k].a]= true;
  }
  int removed= 0;
  for (int i= 0; i < ns; i++) { replacement[i]= i - removed; if (!used[i]) removed++; }
  std::vector<state_t*> r;
  for (int i= 0; i < ns; i++) {
    if (used[i]) r.push_back (states[i]); else delete states[i];
  }
  remap_states (r, replacement);
  return r;
}

std::vector<state_t*>
tables_t::reorder (std::vector<state_t*> states) {
  int ns= (int) states.size ();
  ivec order;
  for (int i= 0; i < ns; i++) order.push_back (i);
  auto key= [&states] (int i) -> ll {
    if (i <= 1) return (ll) i - 1000000;
    return - (ll) (states[i]->terms.size () + states[i]->nonterms.size ());
  };
  std::stable_sort (order.begin (), order.end (), [&key] (int a, int b) { return key (a) < key (b); });
  ivec new_of (ns, 0);
  for (int k= 0; k < ns; k++) new_of[order[k]]= k;
  std::vector<state_t*> r;
  for (int k= 0; k < ns; k++) r.push_back (states[order[k]]);
  remap_states (r, new_of);
  return r;
}


void
tables_t::build_lex_tables (std::vector<state_t*>& states) {
  int word= g.word;
  if (word >= 0) {
    lex_builder_t lb (g);
    lb.add_state_for_tokens (keywords);
    keyword_table= lb.table;
  }
  std::vector<bool> is_keyword (n, false);
  for (size_t k= 0; k < keywords.size (); k++) is_keyword[sym_index (keywords[k])]= true;
  auto check_conflicts= [this] (int i, const ivec& set) -> bool {
    for (size_t k= 0; k < set.size (); k++) {
      if (!is_terminal (set[k])) continue;
      int j= sym_index (set[k]);
      if (does_conflict (i, j) || does_match_prefix (i, j) ||
          ((does_overlap (i, j) || does_overlap (j, i)) && !coincident (i, j)))
        return true;
    }
    return false;
  };
  std::vector<std::pair<ivec, ivec> > by_token_set;
  for (size_t si= 0; si < states.size (); si++) {
    state_t* s= states[si];
    ivec tokens;
    auto add= [&] (int t) {
      if (is_terminal (t)) tokens= set_insert (is_keyword[sym_index (t)]? word: t, tokens);
      else if (t == SYM_END) tokens= set_insert (t, tokens);
    };
    for (int k= 0; k < s->terms.size (); k++) add (s->terms.keys[k]);
    for (size_t k= 0; k < s->reserved.size (); k++) add (s->reserved[k]);
    bool merged= false;
    for (size_t e= 0; e < by_token_set.size () && !merged; e++) {
      const ivec& a= by_token_set[e].first;
      bool bad= false;
      for (size_t k= 0; k < a.size () && !bad; k++)
        if (is_terminal (a[k]) && !set_contains (tokens, a[k]) &&
            check_conflicts (sym_index (a[k]), tokens)) bad= true;
      for (size_t k= 0; k < tokens.size () && !bad; k++)
        if (is_terminal (tokens[k]) && !set_contains (a, tokens[k]) &&
            check_conflicts (sym_index (tokens[k]), a)) bad= true;
      if (!bad) {
        by_token_set[e].first= set_union (a, tokens);
        by_token_set[e].second.push_back ((int) si);
        merged= true;
      }
    }
    if (!merged) by_token_set.push_back (std::make_pair (tokens, ivec (1, (int) si)));
  }
  lex_of.assign (states.size (), 0);
  lex_builder_t lb (g);
  for (size_t e= 0; e < by_token_set.size (); e++) {
    int id= lb.add_state_for_tokens (by_token_set[e].first);
    for (size_t k= 0; k < by_token_set[e].second.size (); k++) lex_of[by_token_set[e].second[k]]= id;
  }
  // minimize
  std::vector<lex_state_t>& table= lb.table;
  int nt= (int) table.size ();
  std::map<std::vector<ll>, int> sig_ids;
  std::vector<ivec> groups0;
  for (int i= 0; i < nt; i++) {
    std::vector<ll> sig;
    sig.push_back (i == 0); sig.push_back (table[i].accept); sig.push_back (table[i].eof >= 0);
    for (size_t k= 0; k < table[i].adv.size (); k++) {
      sig.push_back (-1);
      for (size_t j= 0; j < table[i].adv[k].chars.size (); j++) {
        sig.push_back (table[i].adv[k].chars[j].first); sig.push_back (table[i].adv[k].chars[j].second); }
      sig.push_back (-2); sig.push_back (table[i].adv[k].main);
    }
    std::map<std::vector<ll>,int>::iterator it= sig_ids.find (sig);
    if (it == sig_ids.end ()) { sig_ids[sig]= (int) groups0.size (); groups0.push_back (ivec (1, i)); }
    else groups0[it->second].push_back (i);
  }
  std::stable_sort (groups0.begin (), groups0.end (),
                    [] (const ivec& a, const ivec& b) {
                      return std::lexicographical_compare (a.begin (), a.end (), b.begin (), b.end ()); });
  std::vector<ivec> groups= groups0;
  for (size_t x= 0; x < groups.size (); x++)
    if (std::find (groups[x].begin (), groups[x].end (), 0) != groups[x].end ()) {
      std::swap (groups[0], groups[x]); break; }
  ivec group_of (nt, 0);
  for (size_t k= 0; k < groups.size (); k++)
    for (size_t x= 0; x < groups[k].size (); x++) group_of[groups[k][x]]= (int) k;
  auto differ= [&] (int l, int r) -> bool {
    const std::vector<lex_adv_t>& a= table[l].adv;
    const std::vector<lex_adv_t>& b= table[r].adv;
    for (size_t k= 0; k < a.size () && k < b.size (); k++)
      if (group_of[a[k].state] != group_of[b[k].state]) return true;
    return false;
  };
  while (split_groups (groups, group_of, 1, differ, NULL, NULL)) {}
  std::vector<lex_state_t> mini;
  for (size_t k= 0; k < groups.size (); k++) {
    lex_state_t s= table[groups[k][0]];
    if (s.eof >= 0) s.eof= group_of[s.eof];
    for (size_t j= 0; j < s.adv.size (); j++) s.adv[j].state= group_of[s.adv[j].state];
    mini.push_back (s);
  }
  for (size_t i= 0; i < lex_of.size (); i++) lex_of[i]= group_of[lex_of[i]];
  // sort
  int nm= (int) mini.size ();
  ivec order;
  for (int i= 1; i < nm; i++) order.push_back (i);
  std::stable_sort (order.begin (), order.end (),
                    [&mini] (int a, int b) { return lex_state_less (mini[a], mini[b]); });
  order.insert (order.begin (), 0);
  ivec new_of (nm, 0);
  for (int k= 0; k < nm; k++) new_of[order[k]]= k;
  main_table= renumber_lex (mini, order, new_of);
  for (size_t i= 0; i < lex_of.size (); i++) lex_of[i]= new_of[lex_of[i]];
}

/******************************************************************************
* Writing the results for the Scheme reader
******************************************************************************/

void
write_string (std::string& out, const std::string& s) {
  out += '"';
  for (size_t i= 0; i < s.size (); i++) {
    char c= s[i];
    if (c == '"' || c == '\\') out += '\\';
    out += c;
  }
  out += '"';
}

inline void w_int (std::string& out, ll x) { out += std::to_string (x); out += ' '; }

void
write_action (std::string& out, const action_t& a) {
  switch (a.kind) {
  case A_SHIFT:
    out += "(shift "; w_int (out, a.a); out += a.b? "#t)": "#f)"; break;
  case A_REDUCE:
    out += "(reduce "; w_int (out, a.a); w_int (out, a.b); w_int (out, a.c); w_int (out, a.d);
    out += ")"; break;
  case A_ACCEPT: out += "(accept)"; break;
  case A_SHIFT_EXTRA: out += "(shift-extra)"; break;
  case A_RECOVER: out += "(recover)"; break;
  case A_GOTO: out += "(goto "; w_int (out, a.a); out += ")"; break;
  }
}

void
write_syms (std::string& out, const ivec& l) {
  out += "(";
  for (size_t i= 0; i < l.size (); i++) w_int (out, l[i]);
  out += ")";
}

void
write_lex_table (std::string& out, const std::vector<lex_state_t>& table) {
  out += "(";
  for (size_t i= 0; i < table.size (); i++) {
    const lex_state_t& s= table[i];
    out += "(";
    if (s.accept < 0) out += "#f "; else w_int (out, s.accept);
    if (s.eof < 0) out += "#f ";
    else { out += "("; out += std::to_string (s.eof); out += s.eof_main? " . #t) ": " . #f) "; }
    out += "(";
    for (size_t k= 0; k < s.adv.size (); k++) {
      out += "((";
      for (size_t j= 0; j < s.adv[k].chars.size (); j++) {
        out += "("; out += std::to_string (s.adv[k].chars[j].first); out += " . ";
        out += std::to_string (s.adv[k].chars[j].second); out += ")";
      }
      out += ") "; out += std::to_string (s.adv[k].state);
      out += s.adv[k].main? " . #t)": " . #f)";
    }
    out += "))\n";
  }
  out += ")";
}

/******************************************************************************
* The language for the C runtime (ts-language.scm and ts-export.scm)
******************************************************************************/

typedef std::vector<std::vector<int> > action_list;   // kind and 4 values

struct table_value_t {
  bool entry;           // the actions of a token, else the state of a goto
  action_list actions;
  bool reusable;
  int state;
};

static void
export_lex_table (std::string& out, const std::vector<lex_state_t>& table,
                  const std::unordered_map<int,int>& ids) {
  w_int (out, (ll) table.size ());
  for (size_t i= 0; i < table.size (); i++) {
    const lex_state_t& s= table[i];
    std::unordered_map<int,int>::const_iterator it= ids.find (s.accept);
    w_int (out, s.accept < 0 || it == ids.end ()? -1: it->second);
    w_int (out, s.eof);
    w_int (out, (ll) s.adv.size ());
    for (size_t k= 0; k < s.adv.size (); k++) {
      w_int (out, s.adv[k].state);
      w_int (out, s.adv[k].main? 0: 1);
      w_int (out, (ll) s.adv[k].chars.size ());
      for (size_t j= 0; j < s.adv[k].chars.size (); j++) {
        w_int (out, s.adv[k].chars[j].first);
        w_int (out, s.adv[k].chars[j].second);
      }
    }
  }
}

static void
copy_ints (reader_t& r, std::string& out) {
  // a count and as many integers
  ll n= r.num ();
  for (ll i= 0; i < n; i++) w_int (out, r.num ());
}

static void
copy_strings (reader_t& r, std::string& out) {
  ll n= r.num ();
  for (ll i= 0; i < n; i++) {
    std::string s= r.str ();
    w_int (out, (ll) s.size ());
    out.back ()= ':';
    out += s;
    out += ' ';
  }
}

static void
copy_symbol_lists (reader_t& r, std::string& out, bool with_count) {
  // a count, then symbol count symbol...
  ll n= r.num ();
  if (with_count) w_int (out, n);
  for (ll i= 0; i < n; i++) { w_int (out, r.num ()); ll k= r.num (); w_int (out, k);
    for (ll j= 0; j < k; j++) w_int (out, r.num ()); }
}

void
tables_t::export_language (reader_t& r, std::string& out) {
  std::vector<state_t*>& states= b->states;
  int n_states= (int) states.size ();
  int n_symbols= (int) symbols.size ();
  std::unordered_map<int,int> ids;
  for (int i= 0; i < n_symbols; i++) ids[symbols[i]]= i;
  ids[SYM_EONT]= 0;
  auto id_of= [&ids] (int s) -> int {
    std::unordered_map<int,int>::iterator it= ids.find (s);
    if (it == ids.end ()) throw ts_fail ("tables: a symbol without id");
    return it->second;
  };
  // the external lex states (populate-external-lex-states!)
  std::unordered_map<int,int> by_internal;
  for (int i= 0; i < g.n_externals; i++)
    if (g.external_internal[i] >= 0) by_internal[g.external_internal[i]]= i;
  std::vector<ivec> ext_states (1);
  std::map<ivec,int> ext_ids;
  ext_ids[ivec ()]= 0;
  ivec ext_of (n_states, 0);
  for (int si= 0; si < n_states; si++) {
    state_t* s= states[si];
    ivec tokens;
    for (int k= 0; k < s->terms.size (); k++) {
      int t= s->terms.keys[k];
      if (is_external (t)) tokens= set_insert (t, tokens);
      else if (is_terminal (t) && by_internal.count (t))
        tokens= set_insert (external (by_internal[t]), tokens);
    }
    std::map<ivec,int>::iterator it= ext_ids.find (tokens);
    if (it != ext_ids.end ()) ext_of[si]= it->second;
    else {
      ext_of[si]= ext_ids[tokens]= (int) ext_states.size ();
      ext_states.push_back (tokens);
    }
  }
  // the fragile tokens (mark-fragile-tokens!)
  for (int si= 0; si < n_states; si++) {
    state_t* s= states[si];
    ivec valid;
    for (int k= 0; k < s->terms.size (); k++)
      if (is_terminal (s->terms.keys[k])) valid.push_back (sym_index (s->terms.keys[k]));
    for (int k= 0; k < s->terms.size (); k++) {
      int t= s->terms.keys[k];
      if (!is_terminal (t)) continue;
      for (size_t x= 0; x < valid.size (); x++)
        if (does_overlap (valid[x], sym_index (t))) { s->terms.vals[k].reusable= false; break; }
    }
  }
  // the parse table, by state and symbol id (tables->language)
  std::vector<std::map<int, table_value_t> > table (n_states);
  for (int si= 0; si < n_states; si++) {
    state_t* s= states[si];
    for (int k= 0; k < s->terms.size (); k++) {
      table_value_t v;
      v.entry= true; v.reusable= s->terms.vals[k].reusable; v.state= 0;
      const action_list_t& as= s->terms.vals[k].actions;
      for (size_t j= 0; j < as.size (); j++) {
        const action_t& a= as[j];
        ivec c;
        switch (a.kind) {
        case A_SHIFT: c= { 0, a.a, 0, a.b? 1: 0 }; break;
        case A_SHIFT_EXTRA: c= { 0, 0, 1, 0 }; break;
        case A_REDUCE: c= { 1, id_of (a.a), a.b, a.c, a.d }; break;
        case A_ACCEPT: c= { 2 }; break;
        case A_RECOVER: c= { 3 }; break;
        default: throw ts_fail ("tables: unknown action");
        }
        v.actions.push_back (c);
      }
      table[si][id_of (s->terms.keys[k])]= v;
    }
    for (int k= 0; k < s->nonterms.size (); k++) {
      table_value_t v;
      v.entry= false; v.reusable= false;
      const action_t& a= s->nonterms.vals[k];
      v.state= a.kind == A_GOTO? a.a: si;
      table[si][id_of (s->nonterms.keys[k])]= v;
    }
  }
  // the symbol part made by Scheme
  std::string name= r.str ();
  ll alias_count= r.num ();
  std::string names, metadata, field_names;
  copy_strings (r, names);
  copy_ints (r, metadata);
  ll n_fields= r.num ();
  for (ll i= 0; i < n_fields; i++) {
    std::string f= r.str ();
    field_names += std::to_string (f.size ()) + ":" + f + " ";
  }
  ll n_pids= r.num ();
  std::vector<ivec> alias_seqs (n_pids);
  for (ll i= 0; i < n_pids; i++) alias_seqs[i]= r.syms ();
  std::string field_maps;
  if (r.num () != n_pids) throw ts_fail ("tables: field maps");
  for (ll i= 0; i < n_pids; i++) {
    ll k= r.num ();
    w_int (field_maps, k);
    for (ll j= 0; j < 3 * k; j++) w_int (field_maps, r.num ());
  }
  std::string external_symbols, public_symbols, alias_map, supertype_map;
  copy_ints (r, external_symbols);
  copy_ints (r, public_symbols);
  copy_symbol_lists (r, alias_map, false);
  copy_symbol_lists (r, supertype_map, true);
  // the action lists, numbered in the order of the table
  std::map<std::pair<action_list,bool>, int> list_ids;
  std::vector<const table_value_t*> list_order;
  table_value_t empty;
  empty.entry= true; empty.reusable= false; empty.state= 0;
  list_ids[std::make_pair (action_list (), false)]= 0;
  list_order.push_back (&empty);
  std::vector<std::vector<std::pair<int,int> > > entries (n_states);
  ivec children (n_pids, 0);
  for (int si= 0; si < n_states; si++)
    for (std::map<int,table_value_t>::iterator it= table[si].begin (); it != table[si].end (); ++it) {
      const table_value_t& v= it->second;
      int x= v.state;
      if (v.entry) {
        std::pair<action_list,bool> key (v.actions, v.reusable);
        std::map<std::pair<action_list,bool>, int>::iterator f= list_ids.find (key);
        if (f != list_ids.end ()) x= f->second;
        else { x= list_ids[key]= (int) list_order.size (); list_order.push_back (&v); }
        for (size_t j= 0; j < v.actions.size (); j++)
          if (v.actions[j][0] == 1 && v.actions[j][4] < n_pids)
            children[v.actions[j][4]]= std::max (children[v.actions[j][4]], v.actions[j][2]);
      }
      entries[si].push_back (std::make_pair (it->first, x));
    }
  int max_alias= 1;
  for (ll i= 0; i < n_pids; i++)
    max_alias= std::max (max_alias, std::max ((int) alias_seqs[i].size (), children[i]));
  // the reserved words of the states
  std::map<ivec,int> reserved_ids;
  std::vector<ivec> reserved_order;
  reserved_ids[ivec ()]= 0;
  reserved_order.push_back (ivec ());
  ivec reserved_of (n_states, 0);
  for (int si= 0; si < n_states; si++) {
    ivec set;
    for (size_t k= 0; k < states[si]->reserved.size (); k++)
      if (ids.count (states[si]->reserved[k])) set.push_back (ids[states[si]->reserved[k]]);
    std::map<ivec,int>::iterator f= reserved_ids.find (set);
    if (f != reserved_ids.end ()) reserved_of[si]= f->second;
    else {
      reserved_of[si]= reserved_ids[set]= (int) reserved_order.size ();
      reserved_order.push_back (set);
    }
  }
  int token_count= 0;
  for (int i= 0; i < n_symbols; i++) if (!is_non_terminal (symbols[i])) token_count++;
  int keyword_capture= g.word >= 0 && ids.count (g.word)? ids[g.word]: 0;
  // writing
  out += "tsdyn1 ";
  out += std::to_string (name.size ()) + ":" + name + " ";
  w_int (out, n_symbols); w_int (out, alias_count); w_int (out, token_count);
  w_int (out, g.n_externals); w_int (out, n_states); w_int (out, n_fields);
  w_int (out, n_pids); w_int (out, max_alias); w_int (out, keyword_capture);
  out += names; out += metadata; out += field_names;
  w_int (out, (ll) list_order.size ());
  for (size_t i= 0; i < list_order.size (); i++) {
    const table_value_t& v= *list_order[i];
    w_int (out, v.reusable? 1: 0);
    w_int (out, (ll) v.actions.size ());
    for (size_t j= 0; j < v.actions.size (); j++)
      for (size_t k= 0; k < v.actions[j].size (); k++) w_int (out, v.actions[j][k]);
  }
  for (int si= 0; si < n_states; si++) {
    w_int (out, (ll) entries[si].size ());
    for (size_t k= 0; k < entries[si].size (); k++) {
      w_int (out, entries[si][k].first); w_int (out, entries[si][k].second); }
  }
  for (int si= 0; si < n_states; si++) {
    w_int (out, states[si]->terms.has (SYM_EONT)? 65535: lex_of[si]);
    w_int (out, ext_of[si]);
    w_int (out, reserved_of[si]);
  }
  size_t reserved_size= 0;
  for (size_t i= 0; i < reserved_order.size (); i++)
    reserved_size= std::max (reserved_size, reserved_order[i].size ());
  w_int (out, (ll) reserved_order.size ());
  w_int (out, (ll) reserved_size);
  for (size_t i= 0; i < reserved_order.size (); i++)
    for (size_t k= 0; k < reserved_size; k++)
      w_int (out, k < reserved_order[i].size ()? reserved_order[i][k]: 0);
  for (ll i= 0; i < n_pids; i++)
    for (int k= 0; k < max_alias; k++)
      w_int (out, k < (int) alias_seqs[i].size ()? alias_seqs[i][k]: 0);
  out += field_maps;
  out += external_symbols;
  w_int (out, (ll) ext_states.size ());
  for (size_t i= 0; i < ext_states.size (); i++)
    for (int k= 0; k < g.n_externals; k++)
      w_int (out, set_contains (ext_states[i], external (k))? 1: 0);
  export_lex_table (out, main_table, ids);
  export_lex_table (out, keyword_table, ids);
  out += public_symbols;
  out += alias_map;
  w_int (out, 0);
  std::unordered_map<int,int> first_of_core;
  for (int si= 0; si < n_states; si++) {
    std::unordered_map<int,int>::iterator f= first_of_core.find (states[si]->core);
    if (f == first_of_core.end ()) { first_of_core[states[si]->core]= si; w_int (out, si); }
    else w_int (out, f->second);
  }
  out += supertype_map;
  out += '\n';
}

std::map<int, tables_t*> all_tables;
int tables_counter= 0;

} // namespace

/******************************************************************************
* Interface
******************************************************************************/

string
tree_sitter_tables_start (string data) {
  // (ok id following contains without-word token-conflicts), or (error message)
  tables_t* t= new tables_t;
  std::string out;
  try {
    c_string cs (data);
    reader_t r (cs, ((const char*) cs) + N(data));
    lap (NULL);
    read_grammar (t->g, r);
    t->n= t->g.n_terminals;
    lap ("read grammar");
    t->b= new builder_t (t->g);
    t->b->build ();
    lap ("lr");
    t->compute_following ();
    t->coincident_index ();
    lap ("following, coincident");
    t->token_conflicts ();
    lap ("token conflicts");
    int id= ++tables_counter;
    all_tables[id]= t;
    out += "(ok "; w_int (out, id);
    out += "(";
    for (int i= 0; i < t->n; i++) write_syms (out, t->following[i]);
    out += ") \"";
    for (size_t i= 0; i < t->contains.size (); i++) out += t->contains[i]? '1': '0';
    out += "\" \"";
    for (size_t i= 0; i < t->without_word.size (); i++) out += t->without_word[i]? '1': '0';
    out += "\" #(";
    for (size_t i= 0; i < t->tc.size (); i++) w_int (out, t->tc[i]);
    out += "))";
  }
  catch (ts_fail& f) {
    delete t;
    out= "(error ";
    write_string (out, f.message);
    out += ")";
  }
  return string (out.c_str (), (int) out.size ());
}

string
tree_sitter_tables_finish (int id, string data) {
  // (ok states symbols production-infos max-aliased used-conflicts
  // main-lex-table keyword-lex-table), or (error message); the tables of id
  // are freed, unless data ends with the flag 1: then they are kept for
  // tree_sitter_tables_export, and the states and the lex tables are #f
  std::map<int,tables_t*>::iterator it= all_tables.find (id);
  if (it == all_tables.end ()) return "(error \"tree-sitter tables: unknown id\")";
  tables_t* t= it->second;
  all_tables.erase (it);
  std::string out;
  bool keep= false;
  try {
    c_string cs (data);
    reader_t r (cs, ((const char*) cs) + N(data));
    t->keywords= r.syms ();
    r.skip ();
    keep= r.p < r.e && r.num () == 1;
    lap (NULL);
    t->populate_error_state ();
    t->used_symbols ();
    std::vector<state_t*> states= t->b->states;
    t->b->states.clear ();
    bool profile= getenv ("TS_PROFILE") != NULL;
    if (profile) fprintf (stderr, "ts-tables: %d LR states\n", (int) states.size ());
    lap ("error state");
    states= t->merge (states);
    if (profile) fprintf (stderr, "ts-tables: %d after merging\n", (int) states.size ());
    lap ("merge");
    t->remove_unit_reductions (states);
    states= t->remove_unused (states);
    if (profile) fprintf (stderr, "ts-tables: %d used\n", (int) states.size ());
    states= t->reorder (states);
    lap ("unit, unused, reorder");
    t->build_lex_tables (states);
    lap ("lex");
    t->b->states= states;
    grammar_t& g= t->g;
    out += keep? "(ok #f ": "(ok (";
    for (size_t si= 0; si < states.size () && !keep; si++) {
      state_t* s= states[si];
      out += "("; w_int (out, s->id); w_int (out, s->core);
      out += s->eof_gated? "#t ": "#f ";
      write_syms (out, s->reserved);
      out += " (";
      for (int k= 0; k < s->terms.size (); k++) {
        out += "("; w_int (out, s->terms.keys[k]);
        out += s->terms.vals[k].reusable? "#t": "#f";
        const action_list_t& as= s->terms.vals[k].actions;
        for (size_t j= 0; j < as.size (); j++) { out += ' '; write_action (out, as[j]); }
        out += ")";
      }
      out += ") (";
      for (int k= 0; k < s->nonterms.size (); k++) {
        out += "("; w_int (out, s->nonterms.keys[k]);
        const action_t& a= s->nonterms.vals[k];
        if (a.kind == A_GOTO) { out += "goto "; w_int (out, a.a); }
        else out += "shift-extra";
        out += ")";
      }
      out += ") ";
      w_int (out, t->lex_of[si]);
      out += ")\n";
    }
    if (!keep) out += ") ";
    write_syms (out, t->symbols);
    out += " (";
    const std::vector<prod_info_t>& infos= t->b->production_infos;
    for (size_t k= 0; k < infos.size (); k++) {
      out += "((";
      for (size_t j= 0; j < infos[k].aliases.size (); j++) {
        const std::pair<int,int>& a= infos[k].aliases[j];
        if (a.first < 0) out += "#f ";
        else { out += "("; write_string (out, g.strings[a.first]); out += a.second? " . #t) ": " . #f) "; }
      }
      out += ")";
      for (size_t j= 0; j < infos[k].fields.size (); j++) {
        out += " (";
        write_string (out, g.strings[infos[k].fields[j].first]);
        const std::vector<std::pair<int,bool> >& locs= infos[k].fields[j].second;
        for (size_t x= 0; x < locs.size (); x++) {
          out += " ("; out += std::to_string (locs[x].first);
          out += locs[x].second? " . #t)": " . #f)";
        }
        out += ")";
      }
      out += ")";
    }
    out += ") ";
    w_int (out, t->b->max_aliased_length);
    out += "(";
    for (size_t k= t->b->used_conflicts.size (); k > 0; k--) write_syms (out, t->b->used_conflicts[k-1]);
    out += ") ";
    if (keep) out += "#f #f";
    else {
      write_lex_table (out, t->main_table);
      out += " ";
      write_lex_table (out, t->keyword_table);
    }
    out += ")";
  }
  catch (ts_fail& f) {
    out= "(error ";
    write_string (out, f.message);
    out += ")";
    keep= false;
  }
  if (keep) all_tables[id]= t;
  else delete t;
  return string (out.c_str (), (int) out.size ());
}

string
tree_sitter_tables_export (int id, string data) {
  // the tables of id for ts_dynamic.c, as ts-export.scm writes them, or
  // (error message); data has what depends on the symbols only (see
  // ts-native.scm); the tables of id are freed
  std::map<int,tables_t*>::iterator it= all_tables.find (id);
  if (it == all_tables.end ()) return "(error \"tree-sitter tables: unknown id\")";
  tables_t* t= it->second;
  all_tables.erase (it);
  std::string out;
  try {
    c_string cs (data);
    reader_t r (cs, ((const char*) cs) + N(data));
    t->export_language (r, out);
  }
  catch (ts_fail& f) {
    out= "(error ";
    write_string (out, f.message);
    out += ")";
  }
  delete t;
  return string (out.c_str (), (int) out.size ());
}
