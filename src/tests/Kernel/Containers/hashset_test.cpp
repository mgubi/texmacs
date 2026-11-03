
/******************************************************************************
* MODULE     : hashset_test.cpp
* DESCRIPTION: test on hashset 
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "hashset.hpp"
#include "string.hpp"

/******************************************************************************
* tests on contains
******************************************************************************/

static void
test_contains () {
  auto set= hashset<string>();
  set->insert("Hello");
  CHECK_EQ (set->contains("Hello"), true);
  CHECK_EQ (set->contains("hello"), false);
}

static void
test_init () {
  auto set= hashset<string>();
  set << string("hello") << string("world");
  CHECK_EQ (set->contains("hello"), true);
  CHECK_EQ (set->contains("world"), true);
  CHECK_EQ (N(set), 2);
}

int
main () {
  RUN (test_contains);
  RUN (test_init);
  return test_report ();
}
