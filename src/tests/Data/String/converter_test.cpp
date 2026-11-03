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

static void
test_iconv_replacement () {
  // a character which cannot be converted is replaced by U+FFFD, or by ?
  // when the target encoding has no U+FFFD, and the conversion goes on
  CHECK_EQ (convert ("caf\xe9 x", "UTF-8", "ISO-8859-1"), string ("caf? x"));
  CHECK_EQ (convert ("a\xe2\x82\xac" "b", "UTF-8", "ISO-8859-1"),
            string ("a?b"));
  CHECK_EQ (convert ("ab\xc3", "UTF-8", "ISO-8859-1"), string ("ab?"));
  CHECK_EQ (convert ("a\xe9" "b", "ASCII", "UTF-8"),
            string ("a\xef\xbf\xbd" "b"));
  CHECK_EQ (convert ("a\xe9" "b", "ASCII", "Cork"), string ("a<#FFFD>b"));
  CHECK_EQ (check_encoding ("a\xe9" "b", "UTF-8"), false);
  CHECK_EQ (check_encoding ("caf\xc3\xa9", "UTF-8"), true);
}

int
main () {
  RUN (test_utf8_to_cork);
  RUN (test_iconv_replacement);
  return test_report ();
}
