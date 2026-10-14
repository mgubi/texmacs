/******************************************************************************
* MODULE     : rel_hashmap_test.cpp
* DESCRIPTION: tests of relative hashmaps
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "rel_hashmap.hpp"

// A relative hashmap is a stack of hashmaps: a lookup returns the value of
// the topmost layer that defines the key. The editor uses it for local
// changes which are later confirmed (merge) or discarded (shorten).

typedef rel_hashmap<string,int> rmap;

// a base layer a=1, b=2, c=3 with a layer on top of it
static rmap
layered () {
  rmap r (0);
  r ("a")= 1;
  r ("b")= 2;
  r ("c")= 3;
  r->extend ();
  return r;
}

static void
test_single_layer () {
  rmap r (-1);
  CHECK (is_nil (r->next));
  CHECK (!r->contains ("x"));
  CHECK_EQ (r["x"], -1);
  r ("x")= 5;
  CHECK (r->contains ("x"));
  CHECK_EQ (r["x"], 5);
  // reading an absent key does not define it
  CHECK (!r->contains ("y"));
}

static void
test_from_hashmap () {
  hashmap<string,int> h (7);
  h ("k")= 1;
  rmap r (h);
  CHECK_EQ (r["k"], 1);
  CHECK_EQ (r["other"], 7);
  rmap top (hashmap<string,int> (7), r);
  CHECK_EQ (top["k"], 1);
  top ("k")= 2;
  CHECK_EQ (top["k"], 2);
  CHECK_EQ (r["k"], 1);
}

// lookups fall through to the lower layer; writes shadow it
static void
test_fall_through () {
  rmap r= layered ();
  CHECK (!is_nil (r->next));
  CHECK_EQ (N(r->item), 0);
  CHECK_EQ (r["a"], 1);
  CHECK_EQ (r["c"], 3);
  CHECK_EQ (r["zz"], 0);
  CHECK (r->contains ("b"));
  CHECK (!r->contains ("zz"));
  r ("b")= 20;
  r ("d")= 4;
  CHECK_EQ (r["b"], 20);
  CHECK_EQ (r["d"], 4);
  CHECK_EQ (r->next["b"], 2);
  CHECK (!r->next->contains ("d"));
}

// read-write access copies the value of the lower layer first,
// so that an in-place update starts from the visible value
static void
test_rw_copies_down () {
  rmap r= layered ();
  r ("c") += 10;
  CHECK_EQ (r["c"], 13);
  CHECK_EQ (r->next["c"], 3);
  CHECK (r->item->contains ("c"));
  // an absent key starts from the default value
  r ("new") += 1;
  CHECK_EQ (r["new"], 1);
}

static void
test_shorten_discards () {
  rmap r= layered ();
  r ("a")= 100;
  r ("e")= 5;
  r->shorten ();
  CHECK (is_nil (r->next));
  CHECK_EQ (r["a"], 1);
  CHECK (!r->contains ("e"));
}

static void
test_merge_confirms () {
  rmap r= layered ();
  r ("a")= 100;
  r ("e")= 5;
  r->merge ();
  CHECK (is_nil (r->next));
  CHECK_EQ (r["a"], 100);
  CHECK_EQ (r["b"], 2);
  CHECK_EQ (r["e"], 5);
  CHECK_EQ (N(r->item), 4);
}

// three layers: merging the top one leaves the bottom one intact
static void
test_three_layers () {
  rmap r= layered ();
  r ("a")= 10;
  r->extend ();
  r ("a")= 100;
  r ("b")= 200;
  CHECK_EQ (r["a"], 100);
  CHECK_EQ (r->next["a"], 10);
  CHECK_EQ (r->next->next["a"], 1);
  r->merge ();
  CHECK_EQ (r["a"], 100);
  CHECK_EQ (r["b"], 200);
  CHECK_EQ (r->next["a"], 1);
  CHECK_EQ (r->next["b"], 2);
  r->shorten ();
  CHECK_EQ (r["a"], 1);
  CHECK_EQ (r["b"], 2);
}

static void
test_change () {
  rmap r= layered ();
  hashmap<string,int> ch (0);
  ch ("a")= 11;
  ch ("z")= 26;
  r->change (ch);
  CHECK_EQ (r["a"], 11);
  CHECK_EQ (r["z"], 26);
  CHECK_EQ (r["b"], 2);
  CHECK_EQ (r->next["a"], 1);
}

// find_changes keeps only the entries of a patch that would change
// the visible values
static void
test_find_changes () {
  rmap r= layered ();
  r ("b")= 20;
  hashmap<string,int> ch (0);
  ch ("a")= 1;    // same as below: no change
  ch ("b")= 20;   // same as the local value: no change
  ch ("c")= 30;   // a change
  ch ("d")= 4;    // a new key
  r->find_changes (ch);
  CHECK_EQ (N(ch), 2);
  CHECK (!ch->contains ("a"));
  CHECK (!ch->contains ("b"));
  CHECK_EQ (ch["c"], 30);
  CHECK_EQ (ch["d"], 4);
}

// find_differences completes a patch with the values of the lower layer
// for the keys changed locally: applied to the lower layer, the result
// undoes the local changes
static void
test_find_differences () {
  rmap r= layered ();
  r ("a")= 100;   // changed
  r ("b")= 2;     // written but unchanged
  r ("e")= 5;     // new
  hashmap<string,int> ch (0);
  r->find_differences (ch);
  CHECK_EQ (N(ch), 2);
  CHECK_EQ (ch["a"], 1);
  CHECK (ch->contains ("e"));
  CHECK_EQ (ch["e"], 0);
  CHECK (!ch->contains ("b"));
  // applying the local changes to the lower layer and then the
  // differences undoes everything for the keys that existed before
  rmap s= layered ();
  s ("a")= 100;
  s ("e")= 5;
  s->merge ();
  s->change (ch);
  CHECK_EQ (s["a"], 1);
  CHECK_EQ (s["b"], 2);
  CHECK_EQ (s["e"], 0);
}

int
main () {
  RUN (test_single_layer);
  RUN (test_from_hashmap);
  RUN (test_fall_through);
  RUN (test_rw_copies_down);
  RUN (test_shorten_discards);
  RUN (test_merge_confirms);
  RUN (test_three_layers);
  RUN (test_change);
  RUN (test_find_changes);
  RUN (test_find_differences);
  return test_report ();
}
