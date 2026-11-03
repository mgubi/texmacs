
/******************************************************************************
* MODULE     : font_test.cpp
* DESCRIPTION: tests on font
* COPYRIGHT  : (C) 2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "font.hpp"

static void
test_default_chinese_font_name () {
#ifdef OS_MACOS
  CHECK_EQ (default_chinese_font_name (), string ("Songti SC"));
#else
  SKIP ("the default Chinese font is checked on macOS only");
#endif
}

extern hashmap<string,tree> font_substitutions;

static array<string>
strings (string a, string b= "") {
  array<string> r;
  r << a;
  if (b != "") r << b;
  return r;
}

static tree
rule (string m1, string p1, string m2) {
  // the substitution ((m1 p1) (m2)) of font-substitutions.scm
  return tuple (tuple (m1, p1), tuple (m2));
}

static void
test_apply_substitutions () {
  // the rules are those of font-substitutions.scm which apply: the test adds
  // its own, for masters which no font has
  font_database_load ();
  font_substitutions ("TestRoman")= tuple (rule ("TestRoman", "cjk", "TestX"));
  // the glyphs asked for are kept: (roman cjk) became (SimSun), the next
  // fallback fonts were then those closest to SimSun, without CJK (#536)
  CHECK_EQ (apply_substitutions (strings ("TestRoman", "cjk")),
            strings ("TestX", "cjk"));
  // the other properties of the rule are replaced as before
  font_substitutions ("TestSerif")=
    tuple (rule ("TestSerif", "sansserif", "TestSans"));
  CHECK_EQ (apply_substitutions (strings ("TestSerif", "sansserif")),
            strings ("TestSans"));
  // rules in a cycle end: the kept glyphs could make (A cjk) -> (B cjk) ->
  // (A cjk) ..., and a rule which gives back its property loops by itself
  font_substitutions ("TestA")= tuple (rule ("TestA", "cjk", "TestB"));
  font_substitutions ("TestB")= tuple (rule ("TestB", "cjk", "TestA"));
  array<string> r= apply_substitutions (strings ("TestA", "cjk"));
  CHECK (N(r) == 2 && (r[0] == "TestA" || r[0] == "TestB"));
  CHECK_EQ (r[1], string ("cjk"));
  font_substitutions ("TestC")=
    tuple (tuple (tuple ("TestC", "bold"), tuple ("TestC", "bold")));
  r= apply_substitutions (strings ("TestC", "bold"));
  CHECK_EQ (r[0], string ("TestC"));
  font_substitutions->reset ("TestRoman");
  font_substitutions->reset ("TestSerif");
  font_substitutions->reset ("TestA");
  font_substitutions->reset ("TestB");
  font_substitutions->reset ("TestC");
}

int
main () {
  RUN (test_default_chinese_font_name);
  RUN (test_apply_substitutions);
  return test_report ();
}
