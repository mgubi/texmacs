
/******************************************************************************
* MODULE     : qt_picture_test.cpp
* DESCRIPTION: tests of the cache of the pictures loaded by Qt
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "picture.hpp"
#include "colors.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include <QImage>
#include <QColor>
#include <utime.h>
#include <time.h>

// a png of 4 x 2 pixels of one color, with the given modification time
static void
save_png (url u, QColor c, time_t mtime) {
  QImage im (4, 2, QImage::Format_ARGB32);
  im.fill (c);
  c_string name (concretize (u));
  im.save (QString ((char*) name), "PNG");
  struct utimbuf t;
  t.actime= mtime;
  t.modtime= mtime;
  utime ((char*) name, &t);
}

static int
red_of (picture p) {
  int r, g, b, a;
  get_rgb_color (p->get_pixel (1, 1), r, g, b, a);
  return r;
}

// a picture whose file changed is loaded again
static void
test_file_change () {
  if (get_env ("TEXMACS_HOME_PATH") == "")
    SKIP ("TEXMACS_HOME_PATH is not set");
  url u= url ("$TEXMACS_HOME_PATH") * url ("qt_picture_test.png");
  time_t now= time (NULL);
  save_png (u, QColor (255, 0, 0), now - 100);
  picture p1= load_picture (u, 4, 2, "", 1);
  CHECK_EQ (red_of (p1), 255);
  save_png (u, QColor (0, 0, 255), now);
  picture p2= load_picture (u, 4, 2, "", 1);
  CHECK_EQ (red_of (p2), 0);
  // unchanged: the picture of the cache
  picture p3= load_picture (u, 4, 2, "", 1);
  CHECK_EQ (red_of (p3), 0);
  remove (u);
}

int
main () {
  RUN (test_file_change);
  return test_report ();
}
