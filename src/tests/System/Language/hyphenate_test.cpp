/******************************************************************************
* MODULE     : hyphenate_test.cpp
* DESCRIPTION: tests of the hyphenation of words by patterns
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "hyphenate.hpp"

// The tables are those which load_hyphen_tables makes from a file: the
// patterns by their letters, with "?" for the strings which are no pattern,
// and the exceptions by the word without its hyphens. The penalties of
// get_hyphens are those after each byte of a word in the Cork encoding, in
// which a character outside the encoding takes several bytes, such as
// <#3B1>.

static hashmap<string,string>
test_patterns () {
  hashmap<string,string> patterns ("?");
  patterns ("ab")= "a1b";
  patterns ("<#3B1>b")= "<#3B1>1b";
  // a pattern which ends with the end of the word
  patterns ("cdxxx.")= "c1dxxx.";
  return patterns;
}

// the positions after which the word may be hyphenated
static string
breaks (string s, hashmap<string,string> hyphenations) {
  array<int> a= get_hyphens (s, test_patterns (), hyphenations, false);
  string r;
  if (N(a) != N(s) - 1) return "wrong length " * as_string (N(a));
  for (int i=0; i<N(a); i++)
    if (a[i] == HYPH_STD) {
      if (N(r) != 0) r << ",";
      r << as_string (i);
    }
  return r;
}

static string
breaks (string s) {
  return breaks (s, hashmap<string,string> ());
}

static void
test_bytes () {
  // xxa-bxxx
  CHECK_EQ (breaks ("xxabxxx"), "2");
  CHECK_EQ (breaks ("xxxxxxx"), "");
}

static void
test_characters () {
  // x<#3B1>a-bxxx: after the a, which is the byte 7
  CHECK_EQ (breaks ("x<#3B1>abxxx"), "7");
  CHECK_EQ (breaks ("X<#391>ABXXX"), "7");
  // xxa-b<#3B1>xx: a character after the hyphen moves nothing
  CHECK_EQ (breaks ("xxab<#3B1>xx"), "2");
  // xx<#3B1>-bxxx: a pattern with such a character
  CHECK_EQ (breaks ("xx<#3B1>bxxx"), "7");
  // never inside a character, nor next to the ends of the word
  CHECK_EQ (breaks ("<#3B1>abxxx"), "");
  CHECK_EQ (breaks ("xxx<#3B1><#3B1>xxx"), "");
  CHECK_EQ (breaks ("<#3B1>"), "");
}

static void
test_word_end () {
  // xxc-dxxx: the pattern ends with the final dot of the word
  CHECK_EQ (breaks ("xxcdxxx"), "2");
  // ... and only matches at the end
  CHECK_EQ (breaks ("xxcdxxxx"), "");
  // the same in a word with a character <...>
  CHECK_EQ (breaks ("x<#3B1>cdxxx"), "7");
  CHECK_EQ (breaks ("x<#3B1>cdxxxx"), "");
}

static void
test_english () {
  // the American English patterns of $TEXMACS_PATH, where the breaks of
  // these words come from the patterns 3side., 5lene. and 5stat.
  hashmap<string,string> patterns ("?"), hyphenations ("?");
  load_hyphen_tables ("us", patterns, hyphenations, true);
  array<int> a= get_hyphens ("alongside", patterns, hyphenations, false);
  CHECK_EQ (a[4] == HYPH_STD, true);  // along-side
  a= get_hyphens ("acetylene", patterns, hyphenations, false);
  CHECK_EQ (a[4] == HYPH_STD, true);  // acety-lene
  a= get_hyphens ("aerostat", patterns, hyphenations, false);
  CHECK_EQ (a[3] == HYPH_STD, true);  // aero-stat
}

static void
test_exceptions () {
  hashmap<string,string> hyphenations;
  hyphenations ("xabx")= "xa-bx";
  hyphenations ("x<#3B1>bx")= "x<#3B1>-bx";
  CHECK_EQ (breaks ("xabx", hyphenations), "1");
  CHECK_EQ (breaks ("x<#3B1>bx", hyphenations), "6");
}

int
main () {
  RUN (test_bytes);
  RUN (test_characters);
  RUN (test_word_end);
  RUN (test_english);
  RUN (test_exceptions);
  return test_report ();
}
