/******************************************************************************
* MODULE     : page_box_test.cpp
* DESCRIPTION: tests of the drawing of a page: decorations below the text,
*              a page background with transparency (on a screen renderer)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "Boxes/construct.hpp"
#include "renderer.hpp"
#include "picture.hpp"
#include "colors.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include <QImage>
#include <QColor>

// the renderers of pictures are those of the screen (is_screen), painting
// on an image which keeps what was painted before, like the window does
static const int PX= 5 * PIXEL;
static const int W = 40;   // the page: W x W pixels

static picture
make_screen (renderer& ren, color previous) {
  picture pic= native_picture (W, W, 0, 0);
  ren= picture_renderer (pic, 1.0);
  ren->set_background (brush (previous));
  ren->clear (0, -W*PX, W*PX, 0);
  return pic;
}

static box
rect_box (int x1, int y1, int x2, int y2, color c) {
  array<SI> x, y;
  x << (SI) (x1*PX) << (SI) (x2*PX) << (SI) (x2*PX) << (SI) (x1*PX);
  y << (SI) (-y1*PX) << (SI) (-y1*PX) << (SI) (-y2*PX) << (SI) (-y2*PX);
  return polygon_box (path (), x, y, brush (c));
}

static box
make_page (brush bgc, box body, box dec) {
  array<box> bs (1); bs[0]= body;
  array<SI>  bx (1); bx[0]= 0;
  array<SI>  by (1); by[0]= 0;
  array<box> ds; array<SI> dx, dy;
  if (!is_nil (dec)) { ds << dec; dx << (SI) 0; dy << (SI) 0; }
  return page_box (path (), tree ("page"), 1, bgc, W*PX, W*PX,
                   bs, bx, by, ds, dx, dy);
}

// the color of the pixel in row y counted from the top of the picture
static void
rgb_of (picture pic, int x, int y, int& r, int& g, int& b) {
  int a;
  get_rgb_color (pic->get_pixel (x, W-1-y), r, g, b, a);
}

// the text (here a black rectangle) is drawn over the header and footer
// (here a yellow rectangle over the same place): watermarks and background
// images of the decorations must not hide the text
static void
test_decorations_below_text () {
  color yellow= rgb_color (255, 213, 79);
  color black = rgb_color (0, 0, 0);
  box body= rect_box (5, 5, 25, 15, black);
  box dec = rect_box (15, 10, 35, 30, yellow);
  box page= make_page (brush (false), body, dec);
  renderer ren;
  picture pic= make_screen (ren, white);
  rectangles rs;
  page->redraw (ren, path (), rs);
  int r, g, b;
  rgb_of (pic, 20, 12, r, g, b);   // text and decoration
  CHECK_EQ (r + g + b, 0);
  rgb_of (pic, 30, 25, r, g, b);   // decoration only
  CHECK_EQ (r, 255);
  CHECK_EQ (b, 79);
  rgb_of (pic, 10, 8, r, g, b);    // text only
  CHECK_EQ (r + g + b, 0);
  tm_delete (ren);
}

// a png of W x W pixels, transparent except for the right half, which is
// blue at 25% opacity
static url
save_pattern () {
  url u= url ("$TEXMACS_HOME_PATH") * url ("page_box_test.png");
  QImage im (W, W, QImage::Format_ARGB32);
  im.fill (QColor (0, 0, 0, 0));
  for (int y= 0; y < W; y++)
    for (int x= W/2; x < W; x++)
      im.setPixel (x, y, qRgba (30, 90, 168, 64));
  c_string name (concretize (u));
  im.save (QString ((char*) name), "PNG");
  return u;
}

// the page background is a pattern with transparency: it is painted over
// the screen of the previous paint (the window is repainted by rectangles,
// more than once), and must not show it: the same result whatever was there
static void
test_pattern_background () {
  if (get_env ("TEXMACS_HOME_PATH") == "")
    SKIP ("TEXMACS_HOME_PATH is not set");
  url u= save_pattern ();
  tree pat (_PATTERN, as_string (u), "100%", "100%");
  brush bgc (pat, 255);
  box page= make_page (bgc, empty_box (path (), 0, 0, 0, 0), box ());
  color prevs[2]= { rgb_color (255, 0, 0), rgb_color (0, 128, 0) };
  int r[2][2], g[2][2], b[2][2];
  for (int i= 0; i < 2; i++) {
    renderer ren;
    picture pic= make_screen (ren, prevs[i]);
    rectangles rs;
    page->redraw (ren, path (), rs);
    if (i == 0) {   // painted twice: the translucent part is not darker
      int r0, g0, b0;
      rgb_of (pic, 30, 20, r0, g0, b0);
      page->redraw (ren, path (), rs);
      rgb_of (pic, 30, 20, r[i][1], g[i][1], b[i][1]);
      CHECK_EQ (r0, r[i][1]);
      CHECK_EQ (b0, b[i][1]);
    }
    rgb_of (pic, 10, 20, r[i][0], g[i][0], b[i][0]);   // transparent part
    rgb_of (pic, 30, 20, r[i][1], g[i][1], b[i][1]);   // translucent part
    tm_delete (ren);
  }
  // transparent: the white of the pattern; translucent: blue over white
  for (int i= 0; i < 2; i++) {
    CHECK (r[i][0] >= 253 && g[i][0] >= 253 && b[i][0] >= 253);
    CHECK (r[i][1] >= 195 && r[i][1] <= 203);
    CHECK (b[i][1] >= 230 && b[i][1] <= 236);
  }
  CHECK_EQ (r[0][1], r[1][1]);
  CHECK_EQ (g[0][1], g[1][1]);
  CHECK_EQ (b[0][1], b[1][1]);
  remove (u);
}

int
main () {
  RUN (test_decorations_below_text);
  RUN (test_pattern_background);
  return test_report ();
}
