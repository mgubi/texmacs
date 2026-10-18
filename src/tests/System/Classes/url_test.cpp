
/******************************************************************************
* MODULE     : url_test.cpp
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "file.hpp"

static url tmfs_1= url_system ("tmfs://git/help");
static url http_1= url_system ("http://texmacs.org");
static url https_1= url_system ("https://ustc.edu.cn");
static url root_tmp= url ("/tmp");
static url root_no_such_tmp= url ("/no_such_tmp");

static void
test_exists () {
  // two cases: root directory
  // TODO: Windows compatibility
  CHECK (exists (root_tmp));
  CHECK (!exists (root_no_such_tmp));
}

static void
test_suffix () {
  // empty suffix should work
  url no_suffix= url ("/a/b/c/d/no_suffix");
  CHECK_EQ (suffix (no_suffix), string (""));
  url no_suffix2= url ("/a/b.c/d/no_suffix");
  CHECK_EQ (suffix (no_suffix2), string (""));

  // normal suffix should work
  url png= url ("/a/b/c/d.png");
  CHECK_EQ (suffix (png), string ("png"));
  url png2= url ("/a/b.c/d.png");
  CHECK_EQ (suffix (png2), string ("png"));
}

static void
test_is_rooted_tmfs () {
  CHECK (is_rooted_tmfs (tmfs_1));
  CHECK (!is_rooted_tmfs (http_1));
  CHECK (!is_rooted_tmfs (https_1));
  CHECK (!is_rooted_tmfs (root_tmp));
}

static void
test_is_rooted_web () {
  CHECK (is_rooted_web (http_1));
  CHECK (is_rooted_web (https_1));
  CHECK (!is_rooted_web (root_tmp));
  CHECK (!is_rooted_web (tmfs_1));
}

static void
test_descends () {
  CHECK (descends (url_system ("/tmp/a.txt"), root_tmp));
  CHECK (descends (url_system ("$TEXMACS_PATH/doc/main/man-manual.en.tm"),
                   url_system ("$TEXMACS_PATH")));
  CHECK (!descends (root_no_such_tmp, root_tmp));
}

int
main () {
  RUN (test_exists);
  RUN (test_suffix);
  RUN (test_is_rooted_tmfs);
  RUN (test_is_rooted_web);
  RUN (test_descends);
  return test_report ();
}
