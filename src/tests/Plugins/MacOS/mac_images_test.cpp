
/******************************************************************************
* MODULE     : mac_images_test.cpp
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "url.hpp"
#include "MacOS/mac_images.h"

static url jpg_file ("$TEXMACS_PATH/misc/patterns/wall/solid.jpg");
static const int jpg_file_width = 500;
static const int jpg_file_height= 500;

static void
test_mac_supports () {
#ifdef MACOSX_EXTENSIONS
  // a file which is no image (the SVG of the first version of this test is
  // read by the ImageIO of recent versions of macOS)
  CHECK (!mac_supports (url ("$TEXMACS_PATH/doc/main/links/man-bibliography.en.tm")));
  CHECK (mac_supports (url ("$TEXMACS_PATH/misc/images/fancy-c.png")));
  CHECK (mac_supports (jpg_file));
#else
  SKIP ("no macOS extensions in this build");
#endif
}

static void
test_mac_image_size () {
#ifdef MACOSX_EXTENSIONS
  int w= 0, h= 0;
  mac_image_size (jpg_file, w, h);
  CHECK_EQ (w, jpg_file_width);
  CHECK_EQ (h, jpg_file_height);
#else
  SKIP ("no macOS extensions in this build");
#endif
}

int
main () {
  RUN (test_mac_supports);
  RUN (test_mac_image_size);
  return test_report ();
}
