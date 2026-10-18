
/******************************************************************************
* MODULE     : tree_test.cpp
* DESCRIPTION: Tests on tree
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "tree.hpp"

static void
test_is_atomic () {
  CHECK (is_atomic (tree ()));
}

static void
test_is_tuple () {
  CHECK (is_tuple (tuple ()));
  CHECK (is_tuple (tuple (tree ())));
  CHECK (is_tuple (tuple (tree (), tree ())));
  CHECK (is_tuple (tuple (tree (), tree (), tree ())));
  CHECK (is_tuple (tuple (tree (), tree (), tree (), tree ())));
  CHECK (is_tuple (tuple (tree (), tree (), tree (), tree (), tree ())));
}

static void
test_is_concat () {
  CHECK (is_concat (concat ()));
  CHECK (is_concat (concat (tree ())));
  CHECK (is_concat (concat (tree (), tree ())));
  CHECK (is_concat (concat (tree (), tree (), tree ())));
  CHECK (is_concat (concat (tree (), tree (), tree (), tree ())));
  CHECK (is_concat (concat (tree (), tree (), tree (), tree (), tree ())));
}

int
main () {
  RUN (test_is_atomic);
  RUN (test_is_tuple);
  RUN (test_is_concat);
  return test_report ();
}
