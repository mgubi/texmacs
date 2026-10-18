
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

int
main () {
  RUN (test_default_chinese_font_name);
  return test_report ();
}
