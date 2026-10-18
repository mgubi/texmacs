
/******************************************************************************
* MODULE     : env_length_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "env.hpp"

static void
test_is_length () {
  edit_env env= edit_env ();
  const char* valid_lengths[]= {
    "1cm", "1mm", "1in", "1pt", "1bp", "1dd", "1pc", "1cc",
    "1fs", "1fbs", "1ln", "1sep", "1yfrac", "1ex", "1emunit",
    "1fn", "1fns", "1bls", "1spc", "1xpsc",
    "1w", "1h", "1l", "1r", "1b", "1t",
    "1par", "1pag", "1px", "1tmpt" };
  for (const char* l: valid_lengths)
    CHECK_MSG (env->is_length (string (l)), string (l) * " is a length");
}

int
main () {
  RUN (test_is_length);
  return test_report ();
}
