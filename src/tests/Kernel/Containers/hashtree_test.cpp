/******************************************************************************
* MODULE     : hashtree_test.cpp
* DESCRIPTION: tests of hashtrees and of pairs, triples and quartets
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "hashtree.hpp"
#include "ntuple.hpp"

/******************************************************************************
* Hashtrees
******************************************************************************/

// A hashtree<char,string> is the trie which the converters use to map
// strings to strings. Note that, contrary to the comments in hashtree.hpp,
// ht (key) is the read-write access which creates a missing child, and
// ht [key] is the read-only access which fails on a missing child.
// A label given as a string literal must be wrapped in string (...):
// otherwise it converts to bool and selects the private null constructor.

typedef hashtree<char,string> trie;

static void
trie_add (trie t, string key, string val) {
  for (int i=0; i<N(key); i++) t= t (key[i]);
  t->set_label (val);
}

// longest prefix lookup without creating nodes, as in converter_rep::match
static string
trie_get (trie t, string key) {
  for (int i=0; i<N(key); i++) {
    if (!t->contains (key[i])) return "<none>";
    t= t[key[i]];
  }
  return t->get_label ();
}

static void
test_empty_node () {
  trie t;
  CHECK (!is_nil (t));
  CHECK_EQ (N(t), 0);
  CHECK_EQ (t->get_label (), string (""));
  CHECK (!t->contains ('a'));
  trie u (string ("value"));
  CHECK_EQ (u->label, string ("value"));
  CHECK_EQ (N(u), 0);
}

// the English-German dictionary of the comments in hashtree.hpp
static void
test_dictionary () {
  const char* table[][2]= {
    { "he", "er" }, { "head", "Kopf" }, { "heaven", "Himmel" },
    { "hell", "Hoelle" }, { "hello", "hallo" } };
  int n= 5;
  trie t;
  for (int i=0; i<n; i++) trie_add (t, table[i][0], table[i][1]);
  for (int i=0; i<n; i++)
    CHECK_EQ (trie_get (t, table[i][0]), string (table[i][1]));
  // inner nodes without a value, and missing words
  CHECK_EQ (trie_get (t, "h"), string (""));
  CHECK_EQ (trie_get (t, "hea"), string (""));
  CHECK_EQ (trie_get (t, "hex"), string ("<none>"));
  CHECK_EQ (trie_get (t, "helloo"), string ("<none>"));
  // shape of the tree
  CHECK_EQ (N(t), 1);
  CHECK_EQ (N(t['h']), 1);
  CHECK_EQ (N(t['h']['e']), 2);
  CHECK_EQ (N(t['h']['e']['a']), 2);
}

// reading with () creates nodes, but does not change the dictionary
static void
test_rw_access_creates () {
  trie t;
  trie_add (t, "ab", "x");
  CHECK (!t->contains ('z'));
  trie z= t ('z');
  CHECK (t->contains ('z'));
  CHECK_EQ (z->get_label (), string (""));
  CHECK_EQ (trie_get (t, "ab"), string ("x"));
  CHECK_EQ (N(t), 2);
}

// hashtrees have reference semantics: a child obtained from its parent
// is the node stored in the parent
static void
test_sharing () {
  trie t;
  trie a= t ('a');
  a->set_label ("A");
  CHECK_EQ (t['a']->label, string ("A"));
  trie copy_of_t= t;
  copy_of_t ('b')->set_label ("B");
  CHECK_EQ (trie_get (t, "b"), string ("B"));
  trie other;
  other= t;
  CHECK_EQ (N(other), 2);
}

static void
test_add_child () {
  trie t, c (string ("child"));
  c ('x')->set_label ("grandchild");
  t->add_child ('c', c);
  CHECK_EQ (trie_get (t, "c"), string ("child"));
  CHECK_EQ (trie_get (t, "cx"), string ("grandchild"));
  t->add_new_child ('d');
  CHECK (t->contains ('d'));
  CHECK_EQ (trie_get (t, "d"), string (""));
  // replacing a child
  trie r (string ("replaced"));
  t->add_child ('c', r);
  CHECK_EQ (trie_get (t, "c"), string ("replaced"));
  CHECK_EQ (trie_get (t, "cx"), string ("<none>"));
}

// a trie with integer keys and values
static void
test_int_keys () {
  hashtree<int,int> t;
  for (int i=0; i<100; i++) t (i % 10) (i / 10)->set_label (i);
  CHECK_EQ (N(t), 10);
  bool ok= true;
  for (int i=0; i<100; i++)
    if (t[i % 10][i / 10]->label != i) ok= false;
  CHECK (ok);
}

/******************************************************************************
* Pairs, triples and quartets
******************************************************************************/

static void
test_pair () {
  pair<int,string> p (1, "one"), q (1, "one"), r (1, "two"), s (2, "one");
  CHECK (p == q);
  CHECK (!(p != q));
  CHECK (p != r);
  CHECK (p != s);
  CHECK (!(p == r));
  CHECK_EQ (hash (p), hash (q));
  pair<int,string> t= r;
  CHECK (t == r);
  t= s;
  CHECK (t == s);
  CHECK_EQ (t.x1, 2);
  CHECK_EQ (t.x2, string ("one"));
}

// each component takes part in the comparison
static void
test_triple_quartet () {
  triple<int,int,int> a (1, 2, 3);
  int other[][3]= { {1, 2, 3}, {0, 2, 3}, {1, 0, 3}, {1, 2, 0} };
  for (int i=0; i<4; i++) {
    triple<int,int,int> b (other[i][0], other[i][1], other[i][2]);
    CHECK_EQ (a == b, i == 0);
    CHECK_EQ (a != b, i != 0);
  }
  quartet<int,int,int,int> q (1, 2, 3, 4);
  int qo[][4]= { {1, 2, 3, 4}, {9, 2, 3, 4}, {1, 9, 3, 4},
                 {1, 2, 9, 4}, {1, 2, 3, 9} };
  for (int i=0; i<5; i++) {
    quartet<int,int,int,int> b (qo[i][0], qo[i][1], qo[i][2], qo[i][3]);
    CHECK_EQ (q == b, i == 0);
    CHECK_EQ (q != b, i != 0);
  }
  triple<int,int,int> c (1, 2, 3);
  CHECK_EQ (hash (a), hash (c));
  quartet<int,int,int,int> d (1, 2, 3, 4);
  CHECK_EQ (hash (q), hash (d));
}

// the hash of a pair depends on the order of the components
static void
test_pair_hash_order () {
  pair<int,int> p (1, 2), q (2, 1);
  CHECK (hash (p) != hash (q));
}

int
main () {
  RUN (test_empty_node);
  RUN (test_dictionary);
  RUN (test_rw_access_creates);
  RUN (test_sharing);
  RUN (test_add_child);
  RUN (test_int_keys);
  RUN (test_pair);
  RUN (test_triple_quartet);
  RUN (test_pair_hash_order);
  return test_report ();
}
