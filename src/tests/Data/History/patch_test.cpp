/******************************************************************************
* MODULE     : patch_test.cpp
* DESCRIPTION: tests of patches, their inverses and their commutation
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "patch.hpp"
#include "archiver.hpp"
#include "observer.hpp"
#include "new_document.hpp"

extern tree the_et;

static string
show (tree t) {
  return print_to_string (t);
}

static string
show (modification m) {
  return print_to_string (m);
}

static string
show (patch p) {
  return print_to_string (p);
}

// <document|hello|<concat|ab|cd|<frac|x|y>>|<concat|u|v>|world>
static tree
sample () {
  return tree (DOCUMENT, "hello",
               tree (CONCAT, "ab", "cd", tree (FRAC, "x", "y")),
               tree (CONCAT, "u", "v"),
               "world");
}

// one modification of each kind, all applicable to sample ()
static const int nr_samples= 18;

static modification
sample_modification (int i) {
  switch (i) {
  case  0: return mod_assign (path (), "x");
  case  1: return mod_assign (path (1, 2), tree (SQRT, "z"));
  case  2: return mod_insert (path (0), 5, "!");
  case  3: return mod_insert (path (), 1, tree (DOCUMENT, "new", "lines"));
  case  4: return mod_remove (path (0), 1, 3);
  case  5: return mod_remove (path (), 0, 3);
  case  6: return mod_split (path (), 0, 2);
  case  7: return mod_split (path (1), 2, 1);
  case  8: return mod_join (path (1), 0);
  case  9: return mod_join (path (), 1);
  case 10: return mod_assign_node (path (1, 2), TUPLE);
  case 11: return mod_insert_node (path (1, 2), 0, tree (SQRT));
  case 12: return mod_insert_node (path (3), 1, tree (TUPLE, "a", "b"));
  case 13: return mod_remove_node (path (1, 2), 1);
  case 14: return mod_remove_node (path (), 3);
  case 15: return mod_set_cursor (path (0), 2, "");
  case 16: return mod_insert (path (1), 3, tree (CONCAT, "e"));
  case 17: return mod_remove (path (1, 1), 0, 2);
  default: return mod_assign (path (), "");
  }
}

/******************************************************************************
* Inversion
******************************************************************************/

// undo relies on invert (m, t): applying m and then its inverse gives back
// t, and inverting the inverse on the modified tree gives back m
static void
test_invert_modification () {
  tree t= sample ();
  for (int i=0; i<nr_samples; i++) {
    modification m= sample_modification (i);
    CHECK_MSG (is_applicable (t, m), show (m));
    tree t2= clean_apply (t, m);
    modification m2= invert (m, t);
    CHECK_MSG (is_applicable (t2, m2), show (m2));
    CHECK_EQ (show (clean_apply (t2, m2)), show (t));
    CHECK_EQ (show (invert (m2, t2)), show (m));
  }
}

// a modification patch just stores both directions; inverting it swaps them
static void
test_invert_patch () {
  tree t= sample ();
  for (int i=0; i<nr_samples; i++) {
    modification m= sample_modification (i);
    patch p (m, invert (m, t));
    CHECK (is_modification (p));
    CHECK (is_applicable (p, t));
    tree t2= clean_apply (p, t);
    CHECK_EQ (show (t2), show (clean_apply (t, m)));
    patch q= invert (p, t);
    CHECK (get_modification (q) == get_inverse (p));
    CHECK (get_inverse (q) == get_modification (p));
    CHECK_EQ (show (clean_apply (q, t2)), show (t));
    CHECK (invert (q, t2) == p);
    CHECK (copy (p) == p);
  }
}

/******************************************************************************
* Compound patches
******************************************************************************/

// builds the patch of a sequence of modifications, each with its inverse
// computed on the tree it applies to
static patch
make_patch (tree t, modification* ms, int n) {
  array<patch> a;
  for (int i=0; i<n; i++) {
    a << patch (ms[i], invert (ms[i], t));
    t= clean_apply (t, ms[i]);
  }
  return patch (a);
}

static void
test_compound () {
  tree t= tree (DOCUMENT, "hello", "world");
  modification ms[]= {
    mod_insert (path (0), 5, ","),
    mod_join (path (), 0),
    mod_insert (path (0), 6, " "),
    mod_insert_node (path (0), 0, tree (CONCAT)),
    mod_insert (path (0), 0, tree (CONCAT, tree (FRAC, "1", "2"))),
    mod_assign (path (0, 0, 0), "one"),
    mod_set_cursor (path (0, 1), 3, "") };
  patch p= make_patch (t, ms, 7);
  CHECK (is_compound (p));
  CHECK_EQ (N(p), 7);
  CHECK_EQ (nr_children (p), 7);
  CHECK_EQ (N(children (p, 2, 5)), 3);
  CHECK (child (p, 1) == p[1]);
  CHECK (does_modify (p));
  CHECK_MSG (is_applicable (p, t), show (p));
  // the second step joins two strings, which a lone tree cannot do
  CHECK (!is_applicable (p, tree (DOCUMENT, "hello", tree (CONCAT))));

  tree expected= tree (DOCUMENT,
                       tree (CONCAT, tree (FRAC, "one", "2"), "hello, world"));
  tree t2= clean_apply (p, t);
  CHECK_EQ (show (t2), show (expected));
  tree u= copy (t);
  apply (p, u);
  CHECK_EQ (show (u), show (expected));

  // the inverse of a compound patch undoes its steps in reverse order
  patch q= invert (p, t);
  CHECK (is_compound (q));
  CHECK_EQ (N(q), 7);
  CHECK (get_modification (q[0]) == get_inverse (p[6]));
  CHECK (get_modification (q[6]) == get_inverse (p[0]));
  CHECK (is_applicable (q, t2));
  CHECK_EQ (show (clean_apply (q, t2)), show (t));
  CHECK (invert (q, t2) == p);

  // the undo of a compound followed by its redo is the identity
  patch both (p, q);
  CHECK_EQ (show (clean_apply (both, t)), show (t));
}

// compactify flattens nested compounds and cancels a patch followed by its
// inverse; this is how the archiver keeps its history small
static void
test_compactify () {
  tree t= tree (DOCUMENT, "abc");
  modification m1= mod_insert (path (0), 3, "d");
  modification m2= mod_remove (path (0), 0, 1);
  patch p1 (m1, invert (m1, t));
  patch p2 (m2, invert (m2, clean_apply (t, m1)));
  patch nested (p1, patch (p2, patch (array<patch> ())));
  patch c= compactify (nested);
  CHECK (is_compound (c));
  CHECK_EQ (N(c), 2);
  CHECK (c[0] == p1 && c[1] == p2);
  CHECK_EQ (show (clean_apply (c, t)), show (clean_apply (nested, t)));

  // a single child is returned as such
  CHECK (compactify (patch (p1, patch (array<patch> ()))) == p1);

  // a patch followed by its inverse cancels out
  patch undo= invert (p1, t);
  patch cancelled= compactify (patch (p1, undo));
  CHECK (is_compound (cancelled));
  CHECK_EQ (N(cancelled), 0);
  CHECK (compactify (patch (p1, patch (undo, p2))) == p2);

  // compounds of patches by one author become one patch of that author
  double a= new_author ();
  patch pa (a, p1);
  patch pb (a, p2);
  patch ca= compactify (patch (pa, pb));
  CHECK (is_author (ca));
  CHECK (get_author (ca) == a);
  CHECK_EQ (N(ca[0]), 2);
  CHECK_EQ (show (clean_apply (ca, t)), show (tree (DOCUMENT, "bcd")));
}

/******************************************************************************
* Commutation
******************************************************************************/

// a tree with enough structure for the modifications below: three levels of
// tuples of six children, with strings as leaves
static tree
commute_tree (int& counter, int depth) {
  if (depth == 0) return tree ("s" * as_string (counter++));
  tree t (TUPLE, 6);
  for (int i=0; i<6; i++) t[i]= commute_tree (counter, depth - 1);
  return t;
}

static tree
commute_tree () {
  int counter= 10;
  return commute_tree (counter, 3);
}

// modifications at the root, under children of the root and in strings;
// adapted from the disabled test routine at the end of commute.cpp
static modification
commute_modification (int i) {
  switch (i) {
  case  0: return mod_assign (path (), "Hi");
  case  1: return mod_insert (path (), 0, tree (TUPLE, "a", "b"));
  case  2: return mod_remove (path (), 0, 2);
  case  3: return mod_split (path (), 0, 1);
  case  4: return mod_join (path (), 0);
  case  5: return mod_assign_node (path (), CONCAT);
  case  6: return mod_insert_node (path (), 1, tree (TUPLE, "a", "b"));
  case  7: return mod_remove_node (path (), 0);
  case  8: return mod_insert (path (), 1, tree (TUPLE, "a", "b"));
  case  9: return mod_insert (path (), 2, tree (TUPLE, "a", "b"));
  case 10: return mod_remove (path (), 1, 2);
  case 11: return mod_remove (path (), 2, 2);
  case 12: return mod_split (path (), 1, 2);
  case 13: return mod_split (path (), 2, 1);
  case 14: return mod_join (path (), 1);
  case 15: return mod_join (path (), 2);
  case 16: return mod_remove_node (path (), 1);
  case 17: return mod_remove_node (path (), 2);
  case 18: case 19: case 20: case 21: case 22: case 23: case 24: case 25:
    return path (0) * commute_modification (i - 18);
  case 26: case 27: case 28: case 29: case 30: case 31: case 32: case 33:
    return path (1) * commute_modification (i - 26);
  case 34: case 35: case 36: case 37: case 38: case 39: case 40: case 41:
    return path (2) * commute_modification (i - 34);
  case 42: return mod_insert (path (1, 1, 0), 1, "xy");
  case 43: return mod_insert (path (1, 1, 0), 0, "z");
  case 44: return mod_remove (path (1, 1, 0), 0, 2);
  case 45: return mod_split (path (1, 1), 0, 1);
  case 46: return mod_join (path (1, 1), 0);
  case 47: return mod_set_cursor (path (1, 1, 0), 1, "");
  case 48: return mod_set_cursor (path (), 0, "");
  default: return mod_assign (path (), "");
  }
}

static const int nr_commute= 49;

// is_applicable crashes on a join below a string (can_join indexes the
// string as a tree), so such joins are filtered out first
static bool
applicable (tree t, modification m) {
  if (m->k == MOD_JOIN &&
      (!has_subtree (t, root (m)) || is_atomic (subtree (t, root (m)))))
    return false;
  return is_applicable (t, m);
}

// whenever swap says that m1;m2 can be reordered into m2*;m1*, both orders
// must give the same tree, and swapping back must be consistent
static void
test_commute_modifications () {
  tree t= commute_tree ();
  int nr_swapped= 0, nr_refused= 0;
  for (int i=0; i<nr_commute; i++)
    for (int j=0; j<nr_commute; j++) {
      modification m1= commute_modification (i);
      modification m2= commute_modification (j);
      if (!applicable (t, m1)) continue;
      tree t1= clean_apply (t, m1);
      if (!applicable (t1, m2)) continue;
      tree goal= clean_apply (t1, m2);
      modification s1= m1, s2= m2;
      bool r= swap (s1, s2);
      CHECK_EQ (commute (m1, m2), r);
      if (!r) { nr_refused++; continue; }
      nr_swapped++;
      string what= show (m1) * " ; " * show (m2);
      if (!applicable (t, s1)) {
        CHECK_MSG (false, "swapped first step not applicable: " * what);
        continue;
      }
      tree u1= clean_apply (t, s1);
      if (!applicable (u1, s2)) {
        CHECK_MSG (false, "swapped second step not applicable: " * what);
        continue;
      }
      CHECK_MSG (clean_apply (u1, s2) == goal, "different result: " * what);

      // swap is allowed to refuse the way back (it does for a cursor move
      // at the root), but when it does swap back the result must agree
      modification b1= s1, b2= s2;
      if (swap (b1, b2) && (b1 != m1 || b2 != m2)) {
        modification c1= b1, c2= b2;
        CHECK_MSG (swap (c1, c2) && c1 == s1 && c2 == s2,
                   "inconsistent swap: " * what);
      }

      // pull and co_pull are swap in the other direction
      CHECK (can_pull (m2, m1));
      CHECK (pull (m2, m1) == s1);
      CHECK (co_pull (m2, m1) == s2);
    }
  // make sure the table exercises both outcomes
  CHECK (nr_swapped > 500);
  CHECK (nr_refused > 100);
}

// swapping patches must also swap their inverses, so that the reordered
// history can still be undone
static void
test_commute_patches () {
  tree t= commute_tree ();
  int nr_swapped= 0;
  for (int i=0; i<nr_commute; i++)
    for (int j=0; j<nr_commute; j++) {
      modification m1= commute_modification (i);
      modification m2= commute_modification (j);
      if (!applicable (t, m1)) continue;
      tree t1= clean_apply (t, m1);
      if (!applicable (t1, m2)) continue;
      tree t2= clean_apply (t1, m2);
      patch p1 (m1, invert (m1, t));
      patch p2 (m2, invert (m2, t1));
      patch s1= p1, s2= p2;
      if (!swap (s1, s2)) continue;
      nr_swapped++;
      string what= show (m1) * " ; " * show (m2);
      if (!applicable (t, get_modification (s1)) ||
          !applicable (clean_apply (s1, t), get_modification (s2))) {
        CHECK_MSG (false, "swapped patches not applicable: " * what);
        continue;
      }
      tree u1= clean_apply (s1, t);
      CHECK_MSG (clean_apply (s2, u1) == t2, "different result: " * what);
      // undo in the new order: the inverse of s2, then that of s1; only
      // the whole sequence is checked, since for an insertion at the place
      // of a removal the swapped inverses are paired with the wrong steps
      modification i2= get_inverse (s2), i1= get_inverse (s1);
      if (!applicable (t2, i2) || !applicable (clean_apply (t2, i2), i1)) {
        CHECK_MSG (false, "swapped inverses not applicable: " * what);
        continue;
      }
      CHECK_MSG (clean_apply (clean_apply (t2, i2), i1) == t,
                 "undo after swap: " * what);
    }
  CHECK (nr_swapped > 500);
}

// a few cases worked out by hand
static void
test_commute_examples () {
  // edits in different subtrees are independent
  CHECK (commute (mod_assign (path (0), "x"), mod_assign (path (1), "y")));
  // an insertion before a position shifts it
  modification m1= mod_insert (path (), 0, tree (TUPLE, "a", "b"));
  modification m2= mod_assign (path (3), "y");
  CHECK (swap (m1, m2));
  CHECK (m1 == mod_assign (path (1), "y"));
  CHECK (m2 == mod_insert (path (), 0, tree (TUPLE, "a", "b")));
  // a removal and an edit inside what was removed do not commute
  CHECK (!commute (mod_assign (path (2), "y"), mod_remove (path (), 1, 3)));
  // a split followed by the join of its two halves does not commute
  CHECK (!commute (mod_split (path (), 0, 2), mod_join (path (), 0)));
  // an assignment of the whole tree only commutes with itself
  CHECK (commute (mod_assign (path (), "a"), mod_assign (path (), "a")));
  CHECK (!commute (mod_assign (path (), "a"), mod_assign (path (), "b")));

  // compound and author patches commute child by child
  tree t= tree (TUPLE, "a", "b", "c");
  modification ms[]= {
    mod_insert (path (0), 1, "x"), mod_insert (path (0), 0, "y") };
  patch p= make_patch (t, ms, 2);
  patch q (mod_assign (path (2), "z"), mod_assign (path (2), "c"));
  tree goal= clean_apply (q, clean_apply (p, t));
  patch s1= patch (new_author (), p), s2= q;
  CHECK (swap (s1, s2));
  CHECK_EQ (show (clean_apply (s2, clean_apply (s1, t))), show (goal));
  CHECK (can_pull (q, p));
  CHECK_EQ (show (clean_apply (co_pull (q, p), clean_apply (pull (q, p), t))),
            show (goal));
}

/******************************************************************************
* Joining patches
******************************************************************************/

// consecutive typing in a string is merged into one undoable patch
static void
test_join () {
  tree t= tree (DOCUMENT, "ab");
  modification m1= mod_insert (path (0), 2, "c");
  tree t1= clean_apply (t, m1);
  modification m2= mod_insert (path (0), 3, "d");
  tree t2= clean_apply (t1, m2);
  patch p1 (m1, invert (m1, t));
  patch p2 (m2, invert (m2, t1));
  CHECK (join (p1, p2, t));
  CHECK (is_modification (p1));
  CHECK (get_modification (p1) == mod_insert (path (0), 2, "cd"));
  CHECK (get_inverse (p1) == mod_remove (path (0), 2, 2));
  CHECK_EQ (show (clean_apply (p1, t)), show (t2));
  CHECK_EQ (show (clean_apply (invert (p1, t), t2)), show (t));

  // two backspaces
  modification r1= mod_remove (path (0), 3, 1);
  tree v1= clean_apply (t2, r1);
  modification r2= mod_remove (path (0), 2, 1);
  patch q1 (r1, invert (r1, t2));
  patch q2 (r2, invert (r2, v1));
  CHECK (join (q1, q2, t2));
  CHECK (get_modification (q1) == mod_remove (path (0), 2, 2));
  CHECK_EQ (show (clean_apply (q1, t2)), show (t));
  CHECK_EQ (show (clean_apply (invert (q1, t2), t)), show (t2));

  // a cursor move in the first patch is kept in front
  patch c (mod_set_cursor (path (0), 0, ""), mod_set_cursor (path (0), 0, ""));
  patch pc (c, patch (m1, invert (m1, t)));
  CHECK (join (pc, p2, t));
  CHECK_EQ (N(pc), 2);
  CHECK (pc[0] == c);
  CHECK_EQ (show (clean_apply (pc, t)), show (t2));

  // insertions in different strings are not joined
  tree w= tree (DOCUMENT, "ab", "cd");
  modification n1= mod_insert (path (0), 0, "x");
  modification n2= mod_insert (path (1), 0, "y");
  patch o1 (n1, invert (n1, w));
  patch o2 (n2, invert (n2, clean_apply (w, n1)));
  patch o1_saved= o1;
  CHECK (!join (o1, o2, w));
  CHECK (o1 == o1_saved);
}

/******************************************************************************
* Other patches
******************************************************************************/

static void
test_cursor_birth_author () {
  tree t= tree (DOCUMENT, "ab");
  modification cur= mod_set_cursor (path (0), 1, "");
  CHECK (invert (cur, t) == cur);
  patch pc (cur, cur);
  CHECK (!does_modify (pc));
  CHECK_EQ (show (clean_apply (pc, t)), show (t));

  modification m= mod_insert (path (0), 0, "x");
  patch pm (m, invert (m, t));
  patch mixed (array<patch> (pc, pm, pc));
  CHECK (does_modify (mixed));
  CHECK (remove_set_cursor (mixed) == pm);
  CHECK_EQ (nr_children (remove_set_cursor (patch (pc, pc))), 0);

  // birth patches do not touch the tree and invert to deaths
  double a= new_author ();
  patch b (a, true);
  CHECK (is_birth (b) && get_birth (b));
  CHECK (get_author (b) == a);
  CHECK (is_applicable (b, t));
  CHECK_EQ (show (clean_apply (b, t)), show (t));
  CHECK (!get_birth (invert (b, t)));
  CHECK (!does_modify (b));

  // author patches wrap a patch and keep their author when inverted
  patch pa (a, pm);
  CHECK (is_author (pa));
  CHECK_EQ (N(pa), 1);
  CHECK_EQ (show (clean_apply (pa, t)), show (clean_apply (t, m)));
  patch ia= invert (pa, t);
  CHECK (is_author (ia) && get_author (ia) == a);
  CHECK_EQ (show (clean_apply (ia, clean_apply (pa, t))), show (t));
  CHECK (copy (pa) == pa);

  // a branch patch with a single branch applies like that branch
  array<patch> one;
  one << pm;
  patch br (true, one);
  CHECK (is_branch (br));
  CHECK_EQ (nr_branches (br), 1);
  CHECK (branch (br, 0) == pm);
  CHECK_EQ (show (clean_apply (br, t)), show (clean_apply (t, m)));
  CHECK (compactify (br) == pm);
  CHECK_EQ (nr_branches (pm), 1);
  CHECK (branch (pm, 0) == pm);

  // new authors and markers never coincide
  double a2= new_author ();
  double mk= new_marker ();
  CHECK (a2 != a && mk != a && mk != a2);
  double old= get_author ();
  set_author (a2);
  CHECK (get_author () == a2);
  set_author (old);
}

/******************************************************************************
* The archiver
******************************************************************************/

// the undo history of a document in the global edit tree, set up the way
// the editor does it
static void
test_archiver () {
  the_et     = tuple ();
  the_et->obs= ip_observer (path ());
  path rp= new_document ();
  double author= new_author ();
  set_author (author);
  archiver arch (author, rp);
  tree original= subtree (the_et, rp);
  CHECK_EQ (show (original), show (tree (DOCUMENT, "")));
  CHECK_EQ (arch->undo_possibilities (), 0);

  // three separate edits, each confirmed as a history item
  apply (subtree (the_et, rp), mod_insert (path (0), 0, "hello"));
  CHECK (arch->active ());
  global_confirm ();
  CHECK (!arch->active ());
  apply (subtree (the_et, rp), mod_split (path (), 0, 2));
  global_confirm ();
  apply (subtree (the_et, rp), mod_insert_node (path (1), 0, tree (CONCAT)));
  global_confirm ();
  tree edited= tree (DOCUMENT, "he", tree (CONCAT, "llo"));
  CHECK_EQ (show (subtree (the_et, rp)), show (edited));
  CHECK_EQ (arch->undo_possibilities (), 1);
  CHECK_EQ (arch->redo_possibilities (), 0);

  arch->undo (0);
  CHECK_EQ (show (subtree (the_et, rp)),
            show (tree (DOCUMENT, "he", "llo")));
  CHECK_EQ (arch->redo_possibilities (), 1);
  arch->undo (0);
  CHECK_EQ (show (subtree (the_et, rp)), show (tree (DOCUMENT, "hello")));
  arch->undo (0);
  CHECK_EQ (show (subtree (the_et, rp)), show (tree (DOCUMENT, "")));
  CHECK_EQ (arch->undo_possibilities (), 0);

  arch->redo (0);
  arch->redo (0);
  CHECK_EQ (show (subtree (the_et, rp)),
            show (tree (DOCUMENT, "he", "llo")));
  arch->redo (0);
  CHECK_EQ (show (subtree (the_et, rp)), show (edited));
  CHECK_EQ (arch->redo_possibilities (), 0);

  // a new edit after an undo starts a new branch of the future
  arch->undo (0);
  apply (subtree (the_et, rp), mod_assign (path (1), "y"));
  global_confirm ();
  CHECK_EQ (show (subtree (the_et, rp)), show (tree (DOCUMENT, "he", "y")));
  arch->undo (0);
  CHECK_EQ (arch->redo_possibilities (), 2);
  arch->redo (1);
  CHECK_EQ (show (subtree (the_et, rp)), show (edited));

  // cancelling throws away the unconfirmed modifications
  apply (subtree (the_et, rp), mod_assign (path (0), "xx"));
  CHECK (arch->active ());
  global_cancel ();
  CHECK (!arch->active ());
  CHECK_EQ (show (subtree (the_et, rp)), show (edited));
}

// consecutive typing in one string is merged by simplify into one item,
// but never across the point where the document was saved
static void
test_archiver_simplify () {
  the_et     = tuple ();
  the_et->obs= ip_observer (path ());
  path rp= new_document ();
  double author= new_author ();
  set_author (author);
  archiver arch (author, rp);
  const char* keys[]= { "a", "b", "c", "d", "e", "f" };
  for (int i=0; i<6; i++) {
    if (i == 4) arch->notify_save ();
    apply (subtree (the_et, rp), mod_insert (path (0), i, keys[i]));
    global_confirm ();
  }
  CHECK_EQ (show (subtree (the_et, rp)), show (tree (DOCUMENT, "abcdef")));
  CHECK (!arch->conform_save ());
  arch->undo (0);
  CHECK_EQ (show (subtree (the_et, rp)), show (tree (DOCUMENT, "abcd")));
  CHECK (arch->conform_save ());
  arch->undo (0);
  CHECK_EQ (show (subtree (the_et, rp)), show (tree (DOCUMENT, "")));
  CHECK_EQ (arch->undo_possibilities (), 0);
  arch->redo (0);
  CHECK_EQ (show (subtree (the_et, rp)), show (tree (DOCUMENT, "abcd")));
}

int
main () {
  RUN (test_invert_modification);
  RUN (test_invert_patch);
  RUN (test_compound);
  RUN (test_compactify);
  RUN (test_commute_modifications);
  RUN (test_commute_patches);
  RUN (test_commute_examples);
  RUN (test_join);
  RUN (test_cursor_birth_author);
  RUN (test_archiver);
  RUN (test_archiver_simplify);
  return test_report ();
}
