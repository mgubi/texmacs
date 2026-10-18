
/******************************************************************************
* MODULE     : analyze_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/


#include "tm_test.hpp"
#include "analyze.hpp"

static void
test_is_alpha () {
  for (unsigned char c=0; c<255; c++) {
    if ((c>=65 && c<=90) || (c>=97 && c<=122)) {
      CHECK (is_alpha (c));
    } else {
      CHECK (!is_alpha (c));
    }
  }
}

static void
test_locase_all () {
  CHECK (locase_all (string ("true")) == string ("true"));
  CHECK (locase_all (string ("TRue")) == string ("true"));
  CHECK (locase_all (string ("TRUE")) == string ("true"));
  CHECK (locase_all (string ("123TRUE")) == string ("123true"));
}

static void
test_upcase_all () {
  CHECK (upcase_all (string ("true")) == string ("TRUE"));
  CHECK (upcase_all (string ("TRue")) == string ("TRUE"));
  CHECK (upcase_all (string ("TRUE")) == string ("TRUE"));
  CHECK (upcase_all (string ("123true")) == string ("123TRUE"));
}

static void
test_string_minus () {
  CHECK (string_minus ("Hello World", "eo") == string ("Hll Wrld"));
  CHECK (string_minus ("", "abc") == string (""));
  CHECK (string_minus ("abc", "") == string ("abc"));
}

static void
test_string_union () {
  CHECK (string_union ("abc", "") == string("abc"));
  CHECK (string_union ("", "abc") == string("abc"));
  CHECK (string_union ("Hello World", "eo") == string ("Hll Wrldeo"));
}

static void
test_scm_quote () {
  CHECK_EQ (scm_quote ("a"), string ("\"a\""));
  CHECK_EQ (scm_quote (""), string ("\"\""));
  CHECK_EQ (scm_quote ("\\"), string ("\"\\\\\""));
}

static void
test_scm_unquote () {
  CHECK_EQ (scm_unquote("\"\""), string (""));
  CHECK_EQ (scm_unquote("\"abc\""), string ("abc"));
  CHECK_EQ (scm_unquote("abc"), string ("abc"));
  CHECK_EQ (scm_unquote(""), string (""));
  CHECK_EQ (scm_unquote("\"\\\\\""), string ("\\"));
}

static void
test_raw_quote () {
  CHECK_EQ (raw_quote ("a"), string ("\"a\""));
  CHECK_EQ (raw_quote (""), string ("\"\""));
}

static void
test_raw_unquote () {
  CHECK_EQ (raw_unquote ("\"a\""), string ("a"));
  CHECK_EQ (raw_unquote ("\"a"), string ("\"a"));
  CHECK_EQ (raw_unquote ("a\""), string ("a\""));
  CHECK_EQ (raw_unquote (""), string (""));
  CHECK_EQ (raw_unquote ("a"), string ("a"));
}

static void
test_unescape_guile () {
  // unescape_guile turns \\xHH into the byte HH and leaves everything else
  // alone; the expectation of four backslashes here was never true
  CHECK_EQ (unescape_guile ("\\\\"), string ("\\\\"));
  CHECK_EQ (unescape_guile ("a\\x41b"), string ("aAb"));
  CHECK_EQ (unescape_guile ("\\xzz"), string ("\\xzz"));
  CHECK_EQ (unescape_guile ("abc"), string ("abc"));
}

static void
test_starts () {
  CHECK (starts ("abc_def", "abc"));
  CHECK (!starts ("abc_def", "def"));
  CHECK (starts ("abc", ""));
  CHECK (starts ("", ""));
}

static void
test_ends () {
  CHECK (ends ("abc_def", "def"));
  CHECK (ends ("abc_def", ""));
  CHECK (!ends ("abc_def", "de"));
}

static void
test_read_word () {
  string word;
  int i=0;
  CHECK (read_word ("hello123", i, word));
  CHECK_EQ (word, string ("hello"));
  CHECK_EQ (i, 5);

  i=0;
  word= "";
  CHECK (!read_word ("123", i, word));
  CHECK (is_empty (word));
  CHECK_EQ (i, 0);
}

static void
test_as_hexadecimal () {
  CHECK_EQ (as_hexadecimal (0), string ("0"));
  CHECK_EQ (as_hexadecimal (255), string ("FF"));
  CHECK_EQ (as_hexadecimal (-255), string ("-FF"));
  CHECK_EQ (as_hexadecimal (2147483647), string ("7FFFFFFF"));
  // the negation of the smallest int overflows, which used to make the
  // conversion recurse forever
  CHECK_EQ (as_hexadecimal ((int) -2147483647 - 1), string ("-80000000"));
  CHECK_EQ (from_hexadecimal (as_hexadecimal ((int) -2147483647 - 1)),
            (int) -2147483647 - 1);
  CHECK_EQ (as_hexadecimal (0x1234, 6), string ("001234"));
}

int
main () {
  RUN (test_is_alpha);
  RUN (test_locase_all);
  RUN (test_upcase_all);
  RUN (test_string_minus);
  RUN (test_string_union);
  RUN (test_scm_quote);
  RUN (test_scm_unquote);
  RUN (test_raw_quote);
  RUN (test_raw_unquote);
  RUN (test_unescape_guile);
  RUN (test_starts);
  RUN (test_ends);
  RUN (test_read_word);
  RUN (test_as_hexadecimal);
  return test_report ();
}
