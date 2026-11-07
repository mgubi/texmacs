
/******************************************************************************
* MODULE     : string_test.cpp
* DESCRIPTION: Fixed size strings with reference counting and
*              pointer copying. Zero-characters are allowed in strings.
* COPYRIGHT  : (C) 2018-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "string.hpp"

/******************************************************************************
* Tests on Common routines for strings
******************************************************************************/

static void
test_equality () {
  CHECK_EQ (string("abc") == "abc", true);
  CHECK_EQ (string("abc") == "", false);

  CHECK_EQ (string("abc") != "abc", false);
  CHECK_EQ (string("abc") != "", true);

  CHECK_EQ (string("abc") == string("abc"), true);
  CHECK_EQ (string("abc") == string(), false);
  CHECK_EQ (string("abc") != string("abc"), false);
  CHECK_EQ (string("abc") != string(), true);

  CHECK_EQ (string() == string(), true);
}

static void
test_compare () {
  CHECK (string("ab") < string("b"));
  CHECK (string() < string("0"));

  CHECK (string("a") <= string("a"));
  CHECK (string("ab") <= string("b"));
  CHECK (string() <= string());
  CHECK (string() <= string("0"));
}

static void
test_slice () {
  CHECK (string("abcde")(0, 0) == string());
  CHECK (string("abcde")(0, 1) == string("a"));
  CHECK (string("abcde")(0, 10) == string("abcde"));
  CHECK (string("abcde")(-1, 1) == string("a"));
  CHECK (string("abcde")(3, 2) == string());
  CHECK (string("abcde")(3, -2) == string());
  CHECK (string("abcde")(10, 11) == string());
  CHECK (string("abcde")(-3, -2) == string());
}

static void
test_concat () {
  CHECK (string("abc") * "de" == string("abcde"));
  CHECK (string("abc") * string("de") == string("abcde"));
  CHECK ("abc" * string("de") == string("abcde"));
}

/******************************************************************************
* Modifications
******************************************************************************/
static void
test_append () {
  auto str = string();
  str << 'x';
  CHECK (str == string("x"));
  str << string("yz");
  CHECK (str == string("xyz"));
}

/******************************************************************************
* Conversions
******************************************************************************/
static void
test_as_bool () {
  CHECK_EQ (as_bool(string("true")), true);
  CHECK_EQ (as_bool(string("#t")), true);
  CHECK_EQ (as_bool(string("false")), false);

  // implicit conversion from char*
  CHECK (as_bool("true"));
  CHECK (as_bool("#t"));
  CHECK (!as_bool("false"));
}

static void
test_as_string_bool () {
  CHECK (as_string_bool(true) == string("true"));
  CHECK (as_string_bool(false) == string("false"));
}

/******************************************************************************
* Predicates
******************************************************************************/
static void
test_is_empty () {
  CHECK (is_empty (""));
  CHECK (!is_empty (" "));
  CHECK (!is_empty ("nonempty"));
}

static void
test_is_bool () {
  CHECK (is_bool ("true"));
  CHECK (is_bool ("false"));
  CHECK (is_bool (string ("true")));
  CHECK (is_bool (string ("false")));

  CHECK (!is_bool ("100"));
  CHECK (!is_bool ("nil"));
}

static void
test_is_int () {
  // Empty string is not an int
  CHECK (!is_int (""));

  // Only 0-9 in chars are int
  for (auto i= 0; i<256; i++) {
    char iter= (char) i;
    if (iter >= '0' && iter <= '9')
      CHECK (is_int (iter));
    else
      CHECK (!is_int (iter));
  }

  // Random tests
  CHECK (is_int ("-100"));
  CHECK (is_int ("+100"));
  CHECK (is_int ("100"));

  CHECK (!is_int(".0"));
  CHECK (!is_int("0x09"));
}

static void
test_is_quoted () {
  CHECK (is_quoted ("\"\""));
  CHECK (is_quoted ("\"Hello TeXmacs\""));
  // is_quoted only checks if a string starts with a double quote
  // and ends with another double quote, regardless the validity
  // of the raw string
  CHECK (is_quoted ("\"Hello\"TeXmacs\""));

  CHECK (!is_quoted ("\""));
  CHECK (!is_quoted ("A"));
  CHECK (!is_quoted ("9"));
  CHECK (!is_quoted ("Hello TeXmacs"));
  CHECK (!is_quoted ("\"Hello TeXmac\"s"));
  CHECK (!is_quoted ("H\"ello TeXmacs\""));
}

int
main () {
  RUN (test_equality);
  RUN (test_compare);
  RUN (test_slice);
  RUN (test_concat);
  RUN (test_append);
  RUN (test_as_bool);
  RUN (test_as_string_bool);
  RUN (test_is_empty);
  RUN (test_is_bool);
  RUN (test_is_int);
  RUN (test_is_quoted);
  return test_report ();
}
