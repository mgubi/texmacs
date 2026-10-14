/******************************************************************************
* MODULE     : iterator_test.cpp
* DESCRIPTION: tests of iterators over the containers of the kernel
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "iterator.hpp"
#include "tree.hpp"

// Iterators exist for hashmaps (over the keys) and hashsets. The tests
// check that each element comes out exactly once, whatever the number of
// buckets, and that an iterator over an empty container is not busy.

// counts how often each key of [0, n) is produced by an iterator
static array<int>
tally (iterator<int> it, int n, int& outside) {
  array<int> seen (n);
  for (int i=0; i<n; i++) seen[i]= 0;
  outside= 0;
  while (it->busy ()) {
    int k= it->next ();
    if (k >= 0 && k < n) seen[k]++;
    else outside++;
  }
  return seen;
}

static bool
all_once (array<int> seen) {
  for (int i=0; i<N(seen); i++)
    if (seen[i] != 1) return false;
  return true;
}

static void
test_empty () {
  hashmap<int,int> h (0);
  iterator<int> it= iterate (h);
  CHECK (!it->busy ());
  CHECK_EQ (it->remains (), 0);
  hashset<string> s;
  iterator<string> jt= iterate (s);
  CHECK (!jt->busy ());
  // an empty map with many buckets: spool must run off the end cleanly
  hashmap<int,int> big (0, 64);
  CHECK (!iterate (big)->busy ());
}

// tables of sizes and initial bucket counts, so that both a single long
// chain and a sparse array of buckets get visited
static void
test_hashmap_keys () {
  int sizes[]  = { 1, 2, 7, 100, 1000 };
  int buckets[]= { 1, 4, 256 };
  for (int s=0; s<5; s++)
    for (int b=0; b<3; b++) {
      int n= sizes[s];
      hashmap<int,int> h (-1, buckets[b]);
      for (int i=0; i<n; i++) h (i)= 10 * i;
      int outside;
      array<int> seen= tally (iterate (h), n, outside);
      CHECK_MSG (all_once (seen) && outside == 0,
                 "every key once for n= " * as_string (n) *
                 ", buckets= " * as_string (buckets[b]));
    }
}

// resetting keys removes them from the iteration
static void
test_hashmap_after_reset () {
  hashmap<int,int> h (0);
  for (int i=0; i<50; i++) h (i)= i;
  for (int i=0; i<50; i+=2) h->reset (i);
  int count= 0, odd= 0;
  iterator<int> it= iterate (h);
  while (it->busy ()) {
    int k= it->next ();
    count++;
    if (k % 2 == 1) odd++;
  }
  CHECK_EQ (count, 25);
  CHECK_EQ (odd, 25);
}

// busy may be called any number of times without advancing
static void
test_busy_idempotent () {
  hashmap<int,int> h (0);
  h (3)= 1;
  h (5)= 2;
  iterator<int> it= iterate (h);
  CHECK (it->busy ());
  CHECK (it->busy ());
  CHECK_EQ (it->remains (), -1);
  int a= it->next ();
  CHECK (it->busy ());
  int b= it->next ();
  CHECK (!it->busy ());
  CHECK (!it->busy ());
  CHECK ((a == 3 && b == 5) || (a == 5 && b == 3));
}

static void
test_hashset_strings () {
  const char* words[]= { "alpha", "beta", "gamma", "delta", "epsilon",
                         "zeta", "eta", "theta", "" };
  int n= 9;
  hashset<string> s;
  for (int i=0; i<n; i++) s->insert (words[i]);
  s->insert ("beta");  // a duplicate is not iterated twice
  CHECK_EQ (N(s), n);
  hashmap<string,int> seen (0);
  int count= 0;
  iterator<string> it= iterate (s);
  while (it->busy ()) {
    seen (it->next ()) ++;
    count++;
  }
  CHECK_EQ (count, n);
  for (int i=0; i<n; i++)
    CHECK_MSG (seen[words[i]] == 1, "seen once: " * string (words[i]));
}

static void
test_hashset_ints () {
  hashset<int> s;
  for (int i=0; i<300; i++) s->insert (i);
  for (int i=0; i<300; i+=3) s->remove (i);
  int outside;
  array<int> seen= tally (iterate (s), 300, outside);
  bool ok= outside == 0;
  for (int i=0; i<300; i++)
    if (seen[i] != (i % 3 == 0? 0: 1)) ok= false;
  CHECK (ok);
}

// two iterators over the same map are independent
static void
test_independent () {
  hashmap<int,int> h (0);
  for (int i=0; i<20; i++) h (i)= i;
  iterator<int> it1= iterate (h), it2= iterate (h);
  int n1= 0, n2= 0;
  while (it1->busy ()) { it1->next (); n1++; }
  while (it2->busy ()) { it2->next (); n2++; }
  CHECK_EQ (n1, 20);
  CHECK_EQ (n2, 20);
}

// conversion to a tuple consumes the iterator
static void
test_as_tree () {
  hashmap<string,int> h (0);
  h ("a")= 1;
  h ("b")= 2;
  h ("c")= 3;
  iterator<string> it= iterate (h);
  tree t= (tree) it;
  CHECK (is_tuple (t));
  CHECK_EQ (N(t), 3);
  CHECK (!it->busy ());
  hashset<string> keys;
  for (int i=0; i<N(t); i++) keys->insert (as_string (t[i]));
  CHECK (N(keys) == 3 && keys->contains ("a") &&
         keys->contains ("b") && keys->contains ("c"));
}

int
main () {
  RUN (test_empty);
  RUN (test_hashmap_keys);
  RUN (test_hashmap_after_reset);
  RUN (test_busy_idempotent);
  RUN (test_hashset_strings);
  RUN (test_hashset_ints);
  RUN (test_independent);
  RUN (test_as_tree);
  return test_report ();
}
