
/******************************************************************************
* MODULE     : qt_utilities_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "url.hpp"
#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#endif

static void
test_qt_supports () {
#ifdef QTTEXMACS
  CHECK (qt_supports (url ("x.svg")));
  CHECK (qt_supports (url ("x.png")));
#else
  SKIP ("no Qt in this build");
#endif
}

int
main () {
  RUN (test_qt_supports);
  return test_report ();
}
