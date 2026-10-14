/******************************************************************************
* MODULE     : path_test.cpp
* DESCRIPTION: tests of paths in trees
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "path.hpp"

// paths are written as in as_string, "1.2.3", and the empty string
// stands for the empty path
static path
P (const char* s) {
  return as_path (string (s));
}

static string
S (path p) {
  return as_string (p);
}

// a small document: <document|<concat|ab|<frac|x|y>>|cd>
static tree
sample () {
  return tree (DOCUMENT,
               tree (CONCAT, "ab", tree (FRAC, "x", "y")),
               "cd");
}

/******************************************************************************
* Conversions
******************************************************************************/

static void
test_as_string () {
  CHECK_EQ (S (path ()), "");
  CHECK_EQ (S (path (7)), "7");
  CHECK_EQ (S (path (1, path (2, path (3)))), "1.2.3");
  CHECK_EQ (S (path (-1, path (0))), "-1.0");
}

// as_path reads the maximal runs of digits and skips everything else,
// so that it also parses version numbers and signs are lost
static void
test_as_path () {
  struct { const char* in; const char* out; } cases[]= {
    { "", "" },
    { "abc", "" },
    { "0", "0" },
    { "1.2.3", "1.2.3" },
    { "12.345", "12.345" },
    { "x12y3", "12.3" },
    { "1.0-beta2", "1.0.2" },
    { "-4", "4" },
    { "007", "7" }
  };
  for (auto c: cases)
    CHECK_EQ (S (P (c.in)), c.out);
  // as_string and as_path are inverse on paths of non negative integers
  path ps[]= { path (), path (0), P ("3.1.4.1.5"), P ("10.0.200") };
  for (path p: ps)
    CHECK (as_path (as_string (p)) == p);
}

static void
test_zero_path () {
  CHECK (zero_path (path ()));
  CHECK (zero_path (P ("0")));
  CHECK (zero_path (P ("0.0.0")));
  CHECK (!zero_path (P ("1")));
  CHECK (!zero_path (P ("0.0.1")));
  CHECK (!zero_path (P ("1.0")));
}

static void
test_hash () {
  CHECK_EQ (hash (path ()), 0);
  CHECK_EQ (hash (P ("5")), 5);
  CHECK_EQ (hash (P ("1.2.3")), hash (P ("1.2.3")));
  CHECK_EQ (hash (copy (P ("4.5"))), hash (P ("4.5")));
  CHECK (hash (P ("1.2")) != hash (P ("2.1")));
}

static void
test_versions () {
  struct { const char* v1; const char* v2; bool inf_eq; bool inf; } cases[]= {
    { "1.0", "1.0", true, false },
    { "1.0", "1.1", true, true },
    { "1.1", "1.0", false, false },
    { "1.0.2", "1.0.10", true, true },
    { "1.0.10", "1.0.2", false, false },
    { "1.0", "1.0.1", true, true },     // a prefix comes first
    { "1.99.1", "2.0", true, true },
    { "2.0", "1.99.1", false, false }
  };
  for (auto c: cases) {
    CHECK_MSG (version_inf_eq (c.v1, c.v2) == c.inf_eq,
               string ("version_inf_eq ") * c.v1 * " " * c.v2);
    CHECK_MSG (version_inf (c.v1, c.v2) == c.inf,
               string ("version_inf ") * c.v1 * " " * c.v2);
  }
}

/******************************************************************************
* List operations on paths
******************************************************************************/

static void
test_list_operations () {
  path p= P ("1.2.3");
  CHECK_EQ (N (p), 3);
  CHECK_EQ (N (path ()), 0);
  CHECK_EQ (S (p * 4), "1.2.3.4");
  CHECK_EQ (S (p * P ("4.5")), "1.2.3.4.5");
  CHECK_EQ (S (path () * p), "1.2.3");
  CHECK_EQ (S (p * path ()), "1.2.3");
  CHECK_EQ (S (head (p, 2)), "1.2");
  CHECK_EQ (S (head (p, 0)), "");
  CHECK_EQ (S (tail (p, 1)), "2.3");
  CHECK_EQ (S (tail (p, 3)), "");
  CHECK_EQ (last_item (p), 3);
  CHECK_EQ (S (reverse (p)), "3.2.1");
  CHECK (head (p, 2) * tail (p, 2) == p);
  CHECK (p == P ("1.2.3"));
  CHECK (p != P ("1.2"));
  CHECK (p != P ("1.2.4"));
}

/******************************************************************************
* Operations on paths
******************************************************************************/

static void
test_path_up () {
  CHECK_EQ (S (path_up (P ("1.2.3"))), "1.2");
  CHECK_EQ (S (path_up (P ("4"))), "");
  CHECK_EQ (S (path_up (P ("1.2.3"), 0)), "1.2.3");
  CHECK_EQ (S (path_up (P ("1.2.3"), 2)), "1");
  CHECK_EQ (S (path_up (P ("1.2.3"), 3)), "");
  // path_up undoes the addition of a last item
  path ps[]= { path (), P ("0"), P ("5.6.7") };
  for (path p: ps)
    CHECK (path_up (p * 9) == p);
}

static void
test_path_add () {
  CHECK_EQ (S (path_add (P ("1.2.3"), 1)), "1.2.4");
  CHECK_EQ (S (path_add (P ("1.2.3"), -3)), "1.2.0");
  CHECK_EQ (S (path_inc (P ("5"))), "6");
  CHECK_EQ (S (path_dec (P ("0.5"))), "0.4");
  CHECK_EQ (S (path_add (P ("1.2.3"), 10, 0)), "11.2.3");
  CHECK_EQ (S (path_add (P ("1.2.3"), -2, 1)), "1.0.3");
  // the positional version works on a copy
  path p= P ("1.2.3");
  path q= path_add (p, 5, 1);
  CHECK_EQ (S (p), "1.2.3");
  CHECK_EQ (S (q), "1.7.3");
}

static void
test_strip_and_common () {
  CHECK_EQ (S (P ("1.2.3") / P ("1.2")), "3");
  CHECK_EQ (S (P ("1.2.3") / path ()), "1.2.3");
  CHECK_EQ (S (P ("1.2.3") / P ("1.2.3")), "");
  CHECK_EQ (S (strip (P ("4.5.6"), P ("4"))), "5.6");
  struct { const char* p; const char* q; const char* c; } cases[]= {
    { "1.2.3", "1.2.4", "1.2" },
    { "1.2.3", "1.2", "1.2" },
    { "1.2.3", "2.2.3", "" },
    { "", "1.2", "" },
    { "0.0", "0.0", "0.0" }
  };
  for (auto c: cases) {
    path p= P (c.p), q= P (c.q), r= common (p, q);
    CHECK_EQ (S (r), c.c);
    CHECK (common (q, p) == r);
    // the common part is a prefix of both paths
    CHECK (head (p, N(r)) == r && head (q, N(r)) == r);
    CHECK (r * (p / r) == p);
  }
}

// path_inf and path_inf_eq are the lexicographic order where an empty
// path is not comparable to anything but itself (for path_inf_eq)
static void
test_path_inf () {
  struct { const char* p; const char* q; bool inf; bool inf_eq; } cases[]= {
    { "1", "2", true, true },
    { "2", "1", false, false },
    { "1.5", "2.0", true, true },
    { "1.2", "1.3", true, true },
    { "1.2", "1.2", false, true },
    { "1", "1.0", false, false },
    { "1.0", "1", false, false },
    { "", "", false, true },
    { "", "1", false, false },
    { "1", "", false, false }
  };
  for (auto c: cases) {
    string what= string (c.p) * " vs " * c.q;
    CHECK_MSG (path_inf (P (c.p), P (c.q)) == c.inf, "path_inf " * what);
    CHECK_MSG (path_inf_eq (P (c.p), P (c.q)) == c.inf_eq,
               "path_inf_eq " * what);
  }
}

// path_less compares cursor positions: a position 0 at the end of a
// path is before everything in the node, and 1 after everything
static void
test_path_less () {
  struct { const char* p; const char* q; bool le; } cases[]= {
    { "0.3", "0.5", true },
    { "0.5", "0.3", false },
    { "0.5", "0.5", true },
    { "0.5", "1.0", true },
    { "1.0", "0.5", false },
    { "0", "2.3", true },        // before the node
    { "2.3", "1", true },        // after the node
    { "1", "2.3", false },
    { "2.3", "0", false },
    { "0", "1", true },
    { "1", "0", false },
    { "3", "4", true },
    { "", "", true },
    { "", "1.2", false }
  };
  for (auto c: cases) {
    path p= P (c.p), q= P (c.q);
    string what= string (c.p) * " vs " * c.q;
    CHECK_MSG (path_less_eq (p, q) == c.le, "path_less_eq " * what);
    CHECK_MSG (path_less (p, q) == (c.le && p != q), "path_less " * what);
  }
  // irreflexive and asymmetric
  path ps[]= { P ("0"), P ("1"), P ("0.2"), P ("3.1.4") };
  for (path p: ps) {
    CHECK (!path_less (p, p));
    CHECK (path_less_eq (p, p));
    for (path q: ps)
      CHECK (!(path_less (p, q) && path_less (q, p)));
  }
}

/******************************************************************************
* Subtrees
******************************************************************************/

static void
test_has_subtree () {
  tree t= sample ();
  struct { const char* p; bool ok; } cases[]= {
    { "", true },
    { "0", true },
    { "1", true },
    { "2", false },
    { "0.1", true },
    { "0.1.1", true },
    { "0.1.2", false },
    { "0.0.0", false },    // no children below a string
    { "1.0", false }
  };
  for (auto c: cases)
    CHECK_MSG (has_subtree (t, P (c.p)) == c.ok, string ("has_subtree ") * c.p);
  CHECK (!has_subtree (t, path (-1)));
  CHECK (!has_subtree (tree ("abc"), P ("0")));
  CHECK (has_subtree (tree ("abc"), path ()));
}

static void
test_subtree () {
  tree t= sample ();
  CHECK (subtree (t, path ()) == t);
  CHECK (subtree (t, P ("1")) == tree ("cd"));
  CHECK (subtree (t, P ("0.0")) == tree ("ab"));
  CHECK (subtree (t, P ("0.1")) == tree (FRAC, "x", "y"));
  CHECK (subtree (t, P ("0.1.1")) == tree ("y"));
  // subtree returns a reference into the tree
  subtree (t, P ("0.1.0"))= "z";
  CHECK (t[0][1][0] == "z");
  // composition: walking p then q is walking p * q
  tree u= sample ();
  CHECK (subtree (subtree (u, P ("0")), P ("1.1")) == subtree (u, P ("0.1.1")));
  // parent_subtree stops one level above the end of the path
  CHECK (parent_subtree (u, P ("0.1.1")) == tree (FRAC, "x", "y"));
  CHECK (parent_subtree (u, P ("1")) == u);
  CHECK (parent_subtree (u, P ("0.1.1")) == subtree (u, P ("0.1")));
}

int
main () {
  RUN (test_as_string);
  RUN (test_as_path);
  RUN (test_zero_path);
  RUN (test_hash);
  RUN (test_versions);
  RUN (test_list_operations);
  RUN (test_path_up);
  RUN (test_path_add);
  RUN (test_strip_and_common);
  RUN (test_path_inf);
  RUN (test_path_less);
  RUN (test_has_subtree);
  RUN (test_subtree);
  return test_report ();
}
