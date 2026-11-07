
/******************************************************************************
* MODULE     : preferences_test.cpp
* DESCRIPTION: tests of the reading of the file of the user preferences
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "boot.hpp"
#include "file.hpp"
#include "sys_utils.hpp"

// The tests write the file of the preferences of the home, so they only run
// in a home which has none: the scratch home of tests/Makefile, never the
// home of a user.
static bool scratch_home= false;

static url
preferences_file () {
  return url ("$TEXMACS_HOME_PATH/system") * url ("preferences.scm");
}

#define NEEDS_SCRATCH_HOME \
  if (!scratch_home) SKIP ("TEXMACS_HOME_PATH is not a home without preferences")

// the file of the preferences gets the contents s, which are then read
static void
load_preferences (string s) {
  url dir= "$TEXMACS_HOME_PATH/system";
  if (!exists (dir)) mkdir (dir);
  save_string (preferences_file (), s);
  load_user_preferences ();
}

static void
test_several_preferences () {
  NEEDS_SCRATCH_HOME;
  load_preferences ("(\"test:a\" \"1\")\n(\"test:b\" \"2\")\n");
  CHECK_EQ (get_user_preference ("test:a", "none"), "1");
  CHECK_EQ (get_user_preference ("test:b", "none"), "2");
}

static void
test_single_preference () {
  NEEDS_SCRATCH_HOME;
  load_preferences ("(\"test:c\" \"3\")\n");
  CHECK_EQ (get_user_preference ("test:c", "none"), "3");
}

// the older format: the pairs in a list of their own
static void
test_wrapped_preferences () {
  NEEDS_SCRATCH_HOME;
  load_preferences ("((\"test:d\" \"4\") (\"test:e\" \"5\"))\n");
  CHECK_EQ (get_user_preference ("test:d", "none"), "4");
  CHECK_EQ (get_user_preference ("test:e", "none"), "5");
  load_preferences ("((\"test:f\" \"6\"))\n");
  CHECK_EQ (get_user_preference ("test:f", "none"), "6");
}

static void
test_no_preferences () {
  NEEDS_SCRATCH_HOME;
  load_preferences ("");
  CHECK (!has_user_preference ("test:g"));
  load_preferences ("(\"test:g\")\n");
  CHECK (!has_user_preference ("test:g"));
}

int
main () {
  scratch_home= get_env ("TEXMACS_HOME_PATH") != "" &&
                !exists (preferences_file ());
  RUN (test_several_preferences);
  RUN (test_single_preference);
  RUN (test_wrapped_preferences);
  RUN (test_no_preferences);
  if (scratch_home) remove (preferences_file ());
  return test_report ();
}
