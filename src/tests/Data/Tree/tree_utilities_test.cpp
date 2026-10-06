/******************************************************************************
* MODULE     : tree_utilities_test.cpp
* DESCRIPTION: tests of the tree utilities: correction, analysis,
*              traversal and search
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// Only the routines which are functions of trees and of the standard DRD
// are tested here. The symbol types and priorities and concat_tokenize
// need the math grammar, which is defined in Scheme, and the search for
// sections asks Scheme for the list of section tags, so they are left out.

#include "tm_test.hpp"
#include "tree_modify.hpp"
#include "tree_analyze.hpp"
#include "tree_traverse.hpp"
#include "tree_search.hpp"
#include "drd_std.hpp"
#include "convert.hpp"

static path
P (const char* s) {
  return as_path (string (s));
}

static string
S (path p) {
  return as_string (p);
}

static string
S (tree t) {
  return tree_to_scheme (t);
}

/******************************************************************************
* Correction of trees
******************************************************************************/

static tree
corrected (tree t) {
  tree u= copy (t);
  correct_downwards (u);
  return u;
}

// correct_node joins adjacent strings, removes empty strings and flattens
// nested concatenations, but keeps a concat with a single child
static void
test_correct_concat () {
  tree f= tree (FRAC, "x", "y");
  struct { tree in; tree out; } cases[]= {
    { concat ("a", "b"), tree (CONCAT, "ab") },
    { concat ("a", "", "b"), tree (CONCAT, "ab") },
    { concat ("", f, ""), tree (CONCAT, f) },
    { concat ("a", f, "b"), concat ("a", f, "b") },
    { concat ("a", concat ("b", f), "c"), concat ("ab", f, "c") },
    { concat (concat ("a", "b"), concat ("c", "d")), tree (CONCAT, "abcd") },
    { tree (CONCAT), "" },
    { "abc", "abc" }
  };
  for (auto c: cases) {
    tree u= copy (c.in);
    correct_node (u);
    CHECK_EQ (S (u), S (c.out));
    // correcting twice changes nothing
    tree v= copy (u);
    correct_node (v);
    CHECK_EQ (S (v), S (u));
  }
}

// nodes with an arity which the DRD does not allow are replaced by ""
static void
test_correct_arity () {
  tree u= tree (FRAC, "a");
  correct_node (u);
  CHECK_EQ (S (u), "\"\"");
  u= tree (FRAC, "a", "b", "c");
  correct_node (u);
  CHECK_EQ (S (u), "\"\"");
  u= tree (FRAC, "a", "b");
  correct_node (u);
  CHECK_EQ (S (u), S (tree (FRAC, "a", "b")));
  // unknown tags are left alone
  u= tree (make_tree_label ("my-unknown-macro"), "a", "b", "c");
  correct_node (u);
  CHECK_EQ (S (u), "(my-unknown-macro \"a\" \"b\" \"c\")");
}

// correct_downwards also corrects the descendants, and the empty strings
// left by wrong arities are removed from the concatenations around them
static void
test_correct_downwards () {
  tree t= tree (DOCUMENT,
                concat ("a", tree (FRAC, "b"), "c"),
                tree (FRAC, concat ("x", "", "y"), "z"));
  tree r= tree (DOCUMENT,
                tree (CONCAT, "ac"),
                tree (FRAC, tree (CONCAT, "xy"), "z"));
  CHECK_EQ (S (corrected (t)), S (r));
  // correct_node alone only looks at the root
  tree u= copy (t);
  correct_node (u);
  CHECK_EQ (S (u), S (t));
  // a detached tree has no ancestors to correct
  tree v= concat ("a", "b");
  correct_upwards (v);
  CHECK_EQ (S (v), S (tree (CONCAT, "ab")));
}

/******************************************************************************
* Decomposition of concatenations and with-like tags
******************************************************************************/

static void
test_concat_decompose () {
  tree f= tree (FRAC, "x", "y");
  array<tree> a= concat_decompose (concat ("ab", concat ("c", f), ""));
  CHECK_EQ (N (a), 3);
  CHECK (a[0] == "ab" && a[1] == "c" && a[2] == f);
  CHECK_EQ (N (concat_decompose ("")), 0);
  CHECK_EQ (N (concat_decompose ("x")), 1);
  CHECK_EQ (N (concat_decompose (f)), 1);
  CHECK_EQ (N (concat_decompose (tree (CONCAT))), 0);
  // recomposition joins the strings and drops the concat when possible
  CHECK_EQ (S (concat_recompose (a)), S (concat ("abc", f)));
  CHECK_EQ (S (concat_recompose (array<tree> ())), "\"\"");
  array<tree> b;
  b << tree ("a") << tree ("b");
  CHECK_EQ (S (concat_recompose (b)), "\"ab\"");
  array<tree> c;
  c << f;
  CHECK_EQ (S (concat_recompose (c)), S (f));
  // recompose after decompose is a normal form
  tree ts[]= { concat ("a", "b"), concat ("a", f, "b"),
               concat (f, concat (f, "x")), tree ("z"), f };
  for (tree t: ts) {
    tree n= concat_recompose (concat_decompose (t));
    CHECK_EQ (S (concat_recompose (concat_decompose (n))), S (n));
  }
}

static void
test_with () {
  tree w= tree (WITH, "color", "red", "body");
  tree w2= tree (WITH, "color", "red", "other");
  tree w3= tree (WITH, "color", "blue", "other");
  tree w4= tree (WITH, "font-shape", "italic", "body");
  CHECK (is_with_like (w));
  CHECK (is_with_like (tree (WITH_PACKAGE, "pkg", "body")));
  CHECK (!is_with_like (tree (FRAC, "a", "b")));
  CHECK (!is_with_like (tree ("with")));
  CHECK (!is_with_like (tree (WITH)));
  CHECK (with_body (w) == "body");
  CHECK (with_same_type (w, w2));
  CHECK (!with_same_type (w, w3));
  CHECK (with_similar_type (w, w3));     // same variables
  CHECK (!with_similar_type (w, w4));
  CHECK (!with_similar_type (w, tree (WITH, "color", "red", "a", "b", "c")));
  // nested withs of the same type are flattened by with_decompose
  tree t= concat ("a", tree (WITH, "color", "red", concat ("b", "c")),
                  tree (WITH, "color", "blue", "d"));
  array<tree> a= with_decompose (w, t);
  CHECK_EQ (N (a), 4);
  CHECK (a[0] == "a" && a[1] == "b" && a[2] == "c");
  CHECK (a[3] == tree (WITH, "color", "blue", "d"));
  tree r= with_recompose (w, a);
  CHECK_EQ (S (r), S (tree (WITH, "color", "red",
                            concat ("abc", tree (WITH, "color", "blue", "d")))));
  // with_recompose does not modify the model
  CHECK (w == tree (WITH, "color", "red", "body"));
}

static void
test_correctable_child () {
  tree f= tree (FRAC, "a", "b");
  CHECK (is_correctable_child (f, 0));
  CHECK (is_correctable_child (f, 1));
  tree c= concat ("a", f, tree (LEFT, "("));
  CHECK (!is_correctable_child (c, 0));
  CHECK (is_correctable_child (c, 1));
  CHECK (!is_correctable_child (c, 2));
  tree d= concat ("x", tree (AROUND, "(", "y", ")"));
  CHECK (is_correctable_child (d, 1));
  CHECK (!is_correctable_child (d, 1, true));
  tree w= tree (WITH, "color", "red", "body");
  CHECK (!is_correctable_child (w, 0));
  CHECK (is_correctable_child (w, 2));
}

/******************************************************************************
* Properties of tags in the standard DRD
******************************************************************************/

static void
test_arity () {
  struct { tree_label l; int n; bool ok; } cases[]= {
    { FRAC, 2, true },
    { FRAC, 1, false },
    { FRAC, 3, false },
    { LSUB, 1, true },
    { LSUB, 0, false },
    { WITH, 1, true },
    { WITH, 3, true },
    { WITH, 2, false },
    { WITH, 5, true },
    { CONCAT, 0, false },     // an empty concat is corrected into ""
    { CONCAT, 1, true },
    { CONCAT, 7, true },
    { DOCUMENT, 3, true }
  };
  for (auto c: cases) {
    CHECK_MSG (correct_arity (c.l, c.n) == c.ok,
               as_string (c.l) * " with " * as_string (c.n) * " children");
    tree t (c.l, c.n);
    CHECK (correct_arity (t, c.n) == c.ok);
    if (c.ok)
      CHECK (minimal_arity (c.l) <= c.n && c.n <= maximal_arity (c.l));
  }
  CHECK_EQ (minimal_arity (FRAC), 2);
  CHECK_EQ (maximal_arity (FRAC), 2);
  CHECK_EQ (minimal_arity (tree (FRAC, "a", "b")), 2);
  CHECK_EQ (maximal_arity (tree (FRAC, "a", "b")), 2);
  CHECK_EQ (minimal_arity (WITH), 1);
}

static void
test_accessibility () {
  tree f= tree (FRAC, "a", "b");
  tree w= tree (WITH, "color", "red", "body");
  tree v= tree (VALUE, "x");
  CHECK (is_accessible_child (f, 0));
  CHECK (is_accessible_child (f, 1));
  CHECK (!is_accessible_child (w, 0));
  CHECK (!is_accessible_child (w, 1));
  CHECK (is_accessible_child (w, 2));
  CHECK (!is_accessible_child (v, 0));
  array<tree> a= accessible_children (w);
  CHECK_EQ (N (a), 1);
  CHECK (N (a) == 1 && a[0] == "body");
  CHECK_EQ (N (accessible_children (f)), 2);
  CHECK_EQ (N (accessible_children (v)), 0);
  CHECK (all_accessible (f));
  CHECK (all_accessible (tree (CONCAT, "a", "b")));
  CHECK (!all_accessible (w));
  CHECK (none_accessible (v));
  CHECK (!none_accessible (w));
  CHECK (!all_accessible (tree ("a")));
  CHECK (!none_accessible (tree ("a")));
  CHECK (exists_accessible_inside (f));
  CHECK (exists_accessible_inside (tree ("a")));
}

static void
test_names () {
  tree f= tree (FRAC, "a", "b");
  CHECK_EQ (get_name (f), "fraction");
  CHECK_EQ (get_name (tree (LSUB, "a")), "left subscript");
  CHECK_EQ (get_name (tree (CONCAT, "a", "b")), "concat");
  CHECK_EQ (get_name (tree (DOCUMENT, "a")), "document");
}

/******************************************************************************
* Traversal
******************************************************************************/

// walking a tree from its start to its end with next_valid visits valid
// cursor positions in increasing order, and previous_valid walks back
static void
check_walk (tree t) {
  path p= start (t), last= end (t);
  array<path> visited;
  visited << p;
  for (int i=0; i<1000 && p != last; i++) {
    path q= next_valid (t, p);
    CHECK_MSG (path_less (p, q), "next_valid from " * S (p) * " to " * S (q));
    if (!path_less (p, q)) return;
    CHECK_MSG (valid_cursor (t, q), "invalid cursor " * S (q));
    p= q;
    visited << p;
  }
  CHECK (p == last);
  CHECK (next_valid (t, last) == last);
  CHECK (previous_valid (t, start (t)) == start (t));
  for (int i=N(visited)-1; i>0; i--)
    CHECK_MSG (previous_valid (t, visited[i]) == visited[i-1],
               "previous_valid from " * S (visited[i]));
}

static void
test_next_valid () {
  tree t= tree (DOCUMENT, "abc", "de");
  CHECK_EQ (S (start (t)), "0.0");
  CHECK_EQ (S (end (t)), "1.2");
  CHECK_EQ (S (next_valid (t, P ("0.1"))), "0.2");
  CHECK_EQ (S (next_valid (t, P ("0.3"))), "1.0");
  CHECK_EQ (S (previous_valid (t, P ("1.0"))), "0.3");
  CHECK_EQ (S (previous_valid (t, P ("0.2"))), "0.1");
  check_walk (t);
  check_walk (tree (DOCUMENT, concat ("ab", tree (FRAC, "x", "yz"), "c")));
  check_walk (tree (DOCUMENT, concat ("a", tree (WITH, "color", "red", "bc"))));
  check_walk (tree (DOCUMENT, "", "a"));
}

static void
test_next_word () {
  tree t= tree (DOCUMENT, "hello world foo");
  CHECK_EQ (S (next_word (t, P ("0.0"))), "0.5");
  CHECK_EQ (S (next_word (t, P ("0.5"))), "0.11");
  CHECK_EQ (S (next_word (t, P ("0.11"))), "0.15");
  CHECK_EQ (S (previous_word (t, P ("0.15"))), "0.12");
  CHECK_EQ (S (previous_word (t, P ("0.12"))), "0.6");
  CHECK_EQ (S (previous_word (t, P ("0.3"))), "0.0");
  CHECK_EQ (S (next_word (t, P ("0.15"))), "0.15");
}

static void
test_next_argument () {
  tree t= tree (DOCUMENT, tree (FRAC, "ab", "cd"));
  CHECK_EQ (S (next_argument (t, P ("0.0"))), "0.1.0");
  CHECK_EQ (S (previous_argument (t, P ("0.1"))), "0.0.2");
  CHECK_EQ (S (next_argument (t, P ("0.1"))), "");
  CHECK_EQ (S (previous_argument (t, P ("0.0"))), "");
  // the inaccessible arguments of a with are skipped
  tree w= tree (DOCUMENT, tree (WITH, "color", "red", "ab"));
  CHECK_EQ (S (previous_argument (w, P ("0.2"))), "");
}

static void
test_inside_same () {
  tree t= tree (DOCUMENT, "abc", "de");
  CHECK (inside_same (t, P ("0.1"), P ("0.2"), DOCUMENT));
  CHECK (!inside_same (t, P ("0.1"), P ("1.1"), DOCUMENT));
  CHECK (inside_contiguous_document (t, P ("0.1"), P ("0.3")));
  tree u= tree (DOCUMENT, concat ("a", tree (FRAC, "x", "y")));
  CHECK (inside_same (u, P ("0.0.0"), P ("0.1.0.1"), DOCUMENT));
}

/******************************************************************************
* Search
******************************************************************************/

// every hit of a string search is a range inside a single string which
// spells the searched string
static void
check_string_hits (tree t, range_set sel, string what) {
  CHECK_EQ (N (sel) % 2, 0);
  for (int i=0; i+1<N(sel); i+=2) {
    path p1= sel[i], p2= sel[i+1];
    CHECK (path_up (p1) == path_up (p2));
    CHECK (has_subtree (t, path_up (p1)));
    tree st= subtree (t, path_up (p1));
    CHECK (is_atomic (st));
    if (!is_atomic (st)) continue;
    CHECK_EQ (st->label (last_item (p1), last_item (p2)), what);
    if (i >= 2) CHECK (path_less_eq (sel[i-1], p1));
  }
}

static void
test_search_string () {
  tree t= tree (DOCUMENT,
                concat ("abcab", tree (FRAC, "ab", "x")),
                "cab");
  range_set sel= search (t, "ab", path ());
  CHECK_EQ (N (sel), 8);
  check_string_hits (t, sel, "ab");
  if (N (sel) == 8) {
    CHECK_EQ (S (sel[0]), "0.0.0");
    CHECK_EQ (S (sel[1]), "0.0.2");
    CHECK_EQ (S (sel[2]), "0.0.3");
    CHECK_EQ (S (sel[3]), "0.0.5");
    CHECK_EQ (S (sel[4]), "0.1.0.0");
    CHECK_EQ (S (sel[5]), "0.1.0.2");
    CHECK_EQ (S (sel[6]), "1.1");
    CHECK_EQ (S (sel[7]), "1.3");
  }
  // the path argument is prefixed to the results
  range_set sub= search (t[1], "ab", P ("1"));
  CHECK_EQ (N (sub), 2);
  if (N (sub) == 2) CHECK (sub[0] == sel[6] && sub[1] == sel[7]);
  CHECK_EQ (N (search (t, "zzz", path ())), 0);
  CHECK_EQ (N (search (t, "c", path ())), 4);
  check_string_hits (t, search (t, "c", path ()), "c");
  // overlapping occurrences are counted once
  range_set ov= search (tree ("aaaa"), "aa", path ());
  CHECK_EQ (N (ov), 4);
  check_string_hits (tree ("aaaa"), ov, "aa");
}

static void
test_search_tree () {
  tree f= tree (FRAC, "ab", "x");
  tree t= tree (DOCUMENT, concat ("abcab", f), "cab", copy (f));
  range_set sel= search (t, f, path ());
  CHECK_EQ (N (sel), 4);
  if (N (sel) == 4) {
    CHECK (sel[0] == P ("0.1") * start (f));
    CHECK (sel[1] == P ("0.1") * end (f));
    CHECK (sel[2] == P ("2") * start (f));
    CHECK (sel[3] == P ("2") * end (f));
    CHECK (subtree (t, common (sel[0], sel[1])) == f);
  }
  CHECK_EQ (N (search (t, tree (FRAC, "ab", "y"), path ())), 0);
  // inaccessible children are not searched
  tree w= tree (WITH, "color", "red", "red");
  range_set ws= search (w, "red", path ());
  CHECK_EQ (N (ws), 2);
  if (N (ws) == 2) CHECK_EQ (S (ws[0]), "2.0");
}

static void
test_search_hits () {
  range_set sels;
  sels << P ("0.1") << P ("0.3") << P ("0.5") << P ("0.7") << P ("1.0")
       << P ("1.2");
  // the next hit at or after a position
  range_set n= next_search_hit (sels, P ("0.0"), false);
  CHECK (N (n) == 2 && n[0] == P ("0.1"));
  n= next_search_hit (sels, P ("0.4"), false);
  CHECK (N (n) == 2 && n[0] == P ("0.5"));
  n= next_search_hit (sels, P ("0.2"), false);
  CHECK (N (n) == 2 && n[0] == P ("0.1"));
  n= next_search_hit (sels, P ("0.2"), true);
  CHECK (N (n) == 2 && n[0] == P ("0.5"));
  CHECK_EQ (N (next_search_hit (sels, P ("1.5"), false)), 0);
  CHECK_EQ (N (next_search_hit (range_set (), P ("0.0"), false)), 0);
  // the previous hit
  range_set p= previous_search_hit (sels, P ("0.6"), false);
  CHECK (N (p) == 2 && p[0] == P ("0.5"));
  p= previous_search_hit (sels, P ("1.5"), false);
  CHECK (N (p) == 2 && p[0] == P ("1.0"));
  p= previous_search_hit (sels, P ("0.6"), true);
  CHECK (N (p) == 2 && p[0] == P ("0.1"));
  CHECK_EQ (N (previous_search_hit (sels, P ("0.0"), false)), 0);
  CHECK_EQ (N (previous_search_hit (range_set (), P ("0.0"), false)), 0);
}

int
main () {
  init_std_drd ();
  RUN (test_correct_concat);
  RUN (test_correct_arity);
  RUN (test_correct_downwards);
  RUN (test_concat_decompose);
  RUN (test_with);
  RUN (test_correctable_child);
  RUN (test_arity);
  RUN (test_accessibility);
  RUN (test_names);
  RUN (test_next_valid);
  RUN (test_next_word);
  RUN (test_next_argument);
  RUN (test_inside_same);
  RUN (test_search_string);
  RUN (test_search_tree);
  RUN (test_search_hits);
  return test_report ();
}
