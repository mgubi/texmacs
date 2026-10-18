/******************************************************************************
* MODULE     : converter_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "converter.hpp"

static void
test_utf8_to_cork () {
  // (as strings: CHECK_EQ on two char* would compare the pointers)
  CHECK_EQ (utf8_to_cork ("中"), string ("<#4E2D>"));
  CHECK_EQ (utf8_to_cork ("“"), string ("\x10"));
  CHECK_EQ (utf8_to_cork ("”"), string ("\x11"));
}

int
main () {
  RUN (test_utf8_to_cork);
  return test_report ();
}
