
/******************************************************************************
* MODULE     : list_test.cpp
* DESCRIPTION: test on linked lists with reference counting
* COPYRIGHT  : (C) 2018 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "list.hpp"

// a failed assertion of the kernel throws a string
#define CHECK_THROWS(expr) {                                   \
    bool thrown= false;                                         \
    try { (void) (expr); } catch (string) { thrown= true; }     \
    CHECK_MSG (thrown, #expr " throws"); }

static list<long> gen(int64_t n) {
  auto normal = list<long>();
  for (long i=0; i<n; i++)
    normal << i;
  return normal;
}

auto the_nil_list = list<string>();
auto the_atom_list = list<long>(1);
auto normal = list<long>(1, 2, 3, list<long>());

static void
test_is_nil () {
  CHECK_EQ (is_nil (the_nil_list), true);
  CHECK_EQ (is_nil (the_atom_list), false);
}

static void
test_is_atom () {
  CHECK_EQ (is_atom (the_nil_list), false);
  CHECK_EQ (is_atom (the_atom_list), true);
}

/******************************************************************************
* tests on output and convertion
******************************************************************************/

static void
test_access () {
  CHECK_THROWS (the_nil_list[0]);

  CHECK_EQ (the_atom_list[0], 1L);

  CHECK_EQ (normal[0], 1);
  CHECK_EQ (normal[1], 2);
  CHECK_EQ (normal[2], 3);
}

/******************************************************************************
* tests on insertion and suppression
******************************************************************************/

static void
test_operate_on_the_last () {
  CHECK_THROWS (access_last(the_nil_list));
  CHECK_THROWS (suppress_last(the_nil_list));
  CHECK_THROWS (last_item(the_nil_list));

  auto the_atom_list_copy = copy(the_atom_list);
  access_last(the_atom_list_copy) = 2L;
  CHECK_EQ (the_atom_list_copy, list<long>(2L));
  suppress_last (the_atom_list_copy);
  CHECK_EQ (the_atom_list_copy, list<long>());
  CHECK_EQ (last_item(the_atom_list), 1);

  auto normal_copy = copy(normal);
  access_last (normal_copy) = 4;
  CHECK_EQ (normal_copy, list<long>(1, 2, 4, list<long>()));
  suppress_last (normal_copy);
  CHECK_EQ (normal_copy, list<long>(1, 2, list<long>()));
  CHECK_EQ (last_item(normal), 3);
}

/******************************************************************************
* tests on computations with list<T> structures
******************************************************************************/

static void
test_size () {
  CHECK_EQ (N (the_nil_list), 0);
  CHECK_EQ (N (the_atom_list), 1);
  CHECK_EQ (N (normal), 3);
  for (auto i=4; i<=100; i++) {
    auto list = gen (i);
    CHECK_EQ (N (list), i);
  }
}

static void
test_copy () {
  CHECK_EQ (copy(the_nil_list), the_nil_list);
  CHECK_EQ (copy(the_atom_list), the_atom_list);
  CHECK_EQ (copy(normal), normal);
}

static void
test_append () {
  auto appended = the_nil_list * string("a");
  CHECK_EQ (appended, list<string>(string("a")));

  auto to_append = list<string>("a");
  CHECK_EQ (the_nil_list * to_append, to_append);

  auto nil_to_append = list<string>();
  CHECK_EQ (the_nil_list * nil_to_append, nil_to_append);

  CHECK_EQ (the_atom_list * list<long>(), the_atom_list);

  CHECK_EQ (list<long>(1L, 2L, list<long>()) * list<long>(3L), normal);
  CHECK_EQ (list<long>(1L) * list<long>(2L, 3L, list<long>()), normal);
}

static void
test_head_and_tail () {
  CHECK_THROWS (head(the_nil_list));
  CHECK_THROWS (tail(the_nil_list));

  CHECK_EQ (head(the_atom_list), the_atom_list);
  CHECK_EQ (tail(the_atom_list), list<long>());

  CHECK_EQ (head(normal), list<long>(1));
  CHECK_EQ (tail(normal), list<long>(2, 3, list<long>()));
}

static void
test_reverse () {
  CHECK_EQ (reverse (the_nil_list), the_nil_list);
  CHECK_EQ (reverse (the_atom_list), the_atom_list);
  CHECK_EQ (reverse (normal), list<long>(3, 2, 1, list<long>()));
}

static void
test_remove () {
  CHECK_EQ (remove (the_nil_list, string("a")), the_nil_list);
  CHECK_EQ (remove (the_atom_list, 1L), list<long>());
  CHECK_EQ (remove (normal, 2L), list<long>(1, 3, list<long>()));
}

static void
test_contains () {
  CHECK_EQ (contains (the_nil_list, string("a")), false);
  CHECK_EQ (contains (the_atom_list, 1L), true);
  CHECK_EQ (contains (normal, 1L), true);

  CHECK_EQ (contains (normal, 2L), true);
  CHECK_EQ (contains (normal, 3L), true);
}

int
main () {
  RUN (test_is_nil);
  RUN (test_is_atom);
  RUN (test_access);
  RUN (test_operate_on_the_last);
  RUN (test_size);
  RUN (test_copy);
  RUN (test_append);
  RUN (test_head_and_tail);
  RUN (test_reverse);
  RUN (test_remove);
  RUN (test_contains);
  return test_report ();
}
