/******************************************************************************
* MODULE     : modification_test.cpp
* DESCRIPTION: tests of elementary tree modifications
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "modification.hpp"

static string
show (tree t) {
  return print_to_string (t);
}

static string
show (modification m) {
  return print_to_string (m);
}

// <document|hello|<concat|ab|cd|<frac|x|y>>|<concat|u|v>|world>
static tree
sample () {
  return tree (DOCUMENT, "hello",
               tree (CONCAT, "ab", "cd", tree (FRAC, "x", "y")),
               tree (CONCAT, "u", "v"),
               "world");
}

static tree
sample_with (int i, tree u) {
  tree t= sample ();
  t[i]= u;
  return t;
}

/******************************************************************************
* Accessors
******************************************************************************/

// the constructors pack position and argument into the path; root, index
// and argument must unpack them again
static void
test_accessors () {
  modification ins= mod_insert (path (1, 2), 3, "x");
  CHECK (root (ins) == path (1, 2));
  CHECK_EQ (index (ins), 3);
  CHECK (get_path (ins) == path (1, 2, 3));
  CHECK (get_tree (ins) == tree ("x"));

  modification rem= mod_remove (path (4), 5, 6);
  CHECK (root (rem) == path (4));
  CHECK_EQ (index (rem), 5);
  CHECK_EQ (argument (rem), 6);

  modification spl= mod_split (path (), 1, 2);
  CHECK (is_nil (root (spl)));
  CHECK_EQ (index (spl), 1);
  CHECK_EQ (argument (spl), 2);

  modification joi= mod_join (path (7), 0);
  CHECK (root (joi) == path (7));
  CHECK_EQ (index (joi), 0);

  modification asn= mod_assign_node (path (0, 1), FRAC);
  CHECK (root (asn) == path (0, 1));
  CHECK (L (asn) == FRAC);

  modification inn= mod_insert_node (path (2), 1, tree (TUPLE, "a"));
  CHECK (root (inn) == path (2));
  CHECK_EQ (argument (inn), 1);

  modification rmn= mod_remove_node (path (3), 0);
  CHECK (root (rmn) == path (3));
  CHECK_EQ (index (rmn), 0);

  modification cur= mod_set_cursor (path (0), 4, "data");
  CHECK (root (cur) == path (0));
  CHECK_EQ (index (cur), 4);
  CHECK (get_tree (cur) == tree ("data"));
}

// get_type and make_modification are the scheme interface: they must be
// mutually inverse for every kind of modification
static void
test_scheme_interface () {
  modification a[]= {
    mod_assign (path (1), "a"),
    mod_insert (path (0), 1, "b"),
    mod_remove (path (0), 1, 2),
    mod_split (path (), 0, 2),
    mod_join (path (), 1),
    mod_assign_node (path (1), TUPLE),
    mod_insert_node (path (1), 0, tree (TUPLE)),
    mod_remove_node (path (1), 0),
    mod_set_cursor (path (0), 2, "") };
  const char* names[]= {
    "assign", "insert", "remove", "split", "join",
    "assign-node", "insert-node", "remove-node", "set-cursor" };
  for (int i=0; i<9; i++) {
    CHECK_EQ (get_type (a[i]), names[i]);
    modification m= make_modification (get_type (a[i]),
                                       get_path (a[i]), get_tree (a[i]));
    CHECK_MSG (m == a[i], "make_modification inverts " * get_type (a[i]));
  }
}

static void
test_equality_and_paths () {
  modification m= mod_insert (path (1), 0, "x");
  CHECK (m == mod_insert (path (1), 0, "x"));
  CHECK (!(m != mod_insert (path (1), 0, "x")));
  CHECK (m != mod_insert (path (1), 0, "y"));
  CHECK (m != mod_insert (path (1), 1, "x"));
  CHECK (m != mod_remove (path (1), 0, 1));
  CHECK (copy (m) == m);

  // prefixing and stripping a path moves the modification in the tree
  CHECK (2 * m == mod_insert (path (2, 1), 0, "x"));
  CHECK (path (3, 4) * m == mod_insert (path (3, 4, 1), 0, "x"));
  CHECK ((path (3, 4) * m) / path (3, 4) == m);
  CHECK (mod_assign (path (1), "z") * 2 == mod_assign (path (1, 2), "z"));

  CHECK_EQ (show (mod_remove (path (1), 2, 3)), "remove ([ 1 ], 2, 3)");
  CHECK_EQ (show (mod_join (path (), 0)), "join ([ ], 0)");
  CHECK_EQ (show (mod_assign (path (0), "a")), "assign ([ 0 ], a)");
}

/******************************************************************************
* Applicability
******************************************************************************/

struct applicability_case {
  modification mod;
  bool ok;
};

// is_applicable guards every apply: it must accept the valid modifications
// and reject those with a bad path, a bad position or a bad kind of tree
static void
test_is_applicable () {
  tree t= sample ();
  applicability_case cases[]= {
    { mod_assign (path (), "x"), true },
    { mod_assign (path (1, 2, 0), "x"), true },
    { mod_assign (path (9), "x"), false },
    { mod_assign (path (0, 0), "x"), false },        // below a string
    { mod_insert (path (0), 5, "!"), true },
    { mod_insert (path (0), 0, "!"), true },
    { mod_insert (path (0), 6, "!"), false },        // past the end
    { mod_insert (path (0), -1, "!"), false },
    { mod_insert (path (0), 0, tree (TUPLE)), false },  // tree in a string
    { mod_insert (path (), 4, tree (DOCUMENT, "a")), true },
    { mod_insert (path (), 5, tree (DOCUMENT, "a")), false },
    { mod_insert (path (), 0, "a"), false },          // string in a tree
    { mod_insert (path (7), 0, "a"), false },
    { mod_remove (path (0), 1, 4), true },
    { mod_remove (path (0), 3, 3), false },
    { mod_remove (path (0), -1, 1), false },
    { mod_remove (path (), 0, 4), true },
    { mod_remove (path (), 3, 2), false },
    { mod_remove (path (5), 0, 0), false },
    { mod_split (path (), 0, 5), true },
    { mod_split (path (), 0, 6), false },
    { mod_split (path (), 1, 3), true },
    { mod_split (path (), 1, 4), false },
    { mod_split (path (), 4, 0), false },
    { mod_join (path (), 1), true },                  // two concats
    { mod_join (path (1), 0), true },                 // two strings
    { mod_join (path (), 0), false },                 // string and concat
    { mod_join (path (), 3), false },                 // no right neighbour
    { mod_join (path (), -1), false },
    { mod_join (path (0), 0), false },                // below a string
    { mod_join (path (0), 2), false },
    { mod_assign_node (path (1, 2), TUPLE), true },
    { mod_assign_node (path (0), TUPLE), false },     // a string has no node
    { mod_assign_node (path (8), TUPLE), false },
    { mod_insert_node (path (3), 0, tree (TUPLE, "a")), true },
    { mod_insert_node (path (3), 1, tree (TUPLE, "a")), true },
    { mod_insert_node (path (3), 2, tree (TUPLE, "a")), false },
    { mod_insert_node (path (3), 0, "a"), false },    // no node to insert
    { mod_remove_node (path (1), 2), true },
    { mod_remove_node (path (1), 3), false },
    { mod_remove_node (path (0), 0), false },
    { mod_set_cursor (path (0), 5, ""), true },
    { mod_set_cursor (path (0), 6, ""), false },
    { mod_set_cursor (path (6), 0, ""), false } };
  int n= sizeof (cases) / sizeof (cases[0]);
  for (int i=0; i<n; i++)
    CHECK_MSG (is_applicable (t, cases[i].mod) == cases[i].ok,
               show (cases[i].mod) * " should " *
               (cases[i].ok? string (""): string ("not ")) *
               "be applicable");
}

/******************************************************************************
* Application
******************************************************************************/

struct application_case {
  modification mod;
  tree result;
};

// clean_apply is functional: it returns the modified tree and leaves its
// argument alone; apply modifies a detached tree in place and must agree
static void
check_application (bool in_place) {
  tree frac_xy= tree (FRAC, "x", "y");
  tree con1= tree (CONCAT, "ab", "cd", frac_xy);
  application_case cases[]= {
    { mod_assign (path (), "x"), tree ("x") },
    { mod_assign (path (1, 2), "z"),
      sample_with (1, tree (CONCAT, "ab", "cd", "z")) },
    { mod_insert (path (0), 5, "!"), sample_with (0, "hello!") },
    { mod_insert (path (0), 0, "oh "), sample_with (0, "oh hello") },
    { mod_insert (path (), 1, tree (DOCUMENT, "new", "lines")),
      tree (DOCUMENT, "hello", "new", "lines", con1,
            tree (CONCAT, "u", "v"), "world") },
    { mod_remove (path (0), 1, 3), sample_with (0, "ho") },
    { mod_remove (path (), 0, 3), tree (DOCUMENT, "world") },
    { mod_remove (path (), 2, 0), sample () },
    { mod_split (path (), 0, 2),
      tree (DOCUMENT, "he", "llo", con1, tree (CONCAT, "u", "v"), "world") },
    { mod_split (path (1), 2, 1),
      sample_with (1, tree (CONCAT, "ab", "cd", tree (FRAC, "x"),
                           tree (FRAC, "y"))) },
    { mod_split (path (), 1, 0),
      tree (DOCUMENT, "hello", tree (CONCAT), con1,
            tree (CONCAT, "u", "v"), "world") },
    { mod_join (path (1), 0),
      sample_with (1, tree (CONCAT, "abcd", frac_xy)) },
    { mod_join (path (), 1),
      tree (DOCUMENT, "hello",
            tree (CONCAT, "ab", "cd", frac_xy, "u", "v"), "world") },
    { mod_assign_node (path (1, 2), TUPLE),
      sample_with (1, tree (CONCAT, "ab", "cd", tree (TUPLE, "x", "y"))) },
    { mod_insert_node (path (1, 2), 0, tree (SQRT)),
      sample_with (1, tree (CONCAT, "ab", "cd", tree (SQRT, frac_xy))) },
    { mod_insert_node (path (3), 1, tree (TUPLE, "a", "b")),
      sample_with (3, tree (TUPLE, "a", "world", "b")) },
    { mod_remove_node (path (1, 2), 1),
      sample_with (1, tree (CONCAT, "ab", "cd", "y")) },
    { mod_remove_node (path (), 3), tree ("world") },
    { mod_set_cursor (path (0), 2, ""), sample () } };
  int n= sizeof (cases) / sizeof (cases[0]);
  for (int i=0; i<n; i++) {
    tree t= sample ();
    CHECK_MSG (is_applicable (t, cases[i].mod), show (cases[i].mod));
    if (in_place) {
      apply (t, cases[i].mod);
      CHECK_EQ (show (t), show (cases[i].result));
    }
    else {
      CHECK_EQ (show (clean_apply (t, cases[i].mod)), show (cases[i].result));
      CHECK_EQ (show (t), show (sample ()));
    }
  }
}

static void
test_clean_apply () {
  check_application (false);
}

static void
test_apply_in_place () {
  check_application (true);
}

// a sequence of modifications, each on the result of the previous one
static void
test_sequence () {
  tree t= tree (DOCUMENT, "");
  apply (t, mod_insert (path (0), 0, "helo"));
  apply (t, mod_insert (path (0), 3, "l"));
  apply (t, mod_split (path (), 0, 3));
  apply (t, mod_insert_node (path (1), 0, tree (SQRT)));
  apply (t, mod_assign_node (path (1), TUPLE));
  CHECK_EQ (show (t), show (tree (DOCUMENT, "hel", tree (TUPLE, "lo"))));
  apply (t, mod_remove_node (path (1), 0));
  apply (t, mod_join (path (), 0));
  CHECK_EQ (show (t), show (tree (DOCUMENT, "hello")));
}

int
main () {
  RUN (test_accessors);
  RUN (test_scheme_interface);
  RUN (test_equality_and_paths);
  RUN (test_is_applicable);
  RUN (test_clean_apply);
  RUN (test_apply_in_place);
  RUN (test_sequence);
  return test_report ();
}
