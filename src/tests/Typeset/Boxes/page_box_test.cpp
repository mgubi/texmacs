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
#include "vars.hpp"
#include "hashmap.hpp"
#include <QImage>
#include <QColor>

// the renderers of pictures are those of the screen (is_screen), painting
// on an image which keeps what was painted before, like the window does
static const int PX= 5 * PIXEL;
static const int W = 40;   // the page: W x W pixels

// the background of the renderer is that of the document (bg-color)
static picture
make_screen (renderer& ren, color previous, brush doc_bg= brush (white)) {
  picture pic= native_picture (W, W, 0, 0);
  ren= picture_renderer (pic, 1.0);
  ren->set_background (brush (previous));
  ren->clear (0, -W*PX, W*PX, 0);
  ren->set_background (doc_bg);
  return pic;
}

// on a screen renderer, every 16th box painted asks the window whether the
// painting is interrupted: there is none here
extern int nr_painted;
static void
paint (box page, renderer ren, rectangles& rs) {
  nr_painted= 0;
  page->redraw (ren, path (), rs);
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
  paint (page, ren, rs);
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

extern hashmap<string,tree> default_env;
void initialize_default_env ();

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
    paint (page, ren, rs);
    if (i == 0) {   // painted twice: the translucent part is not darker
      int r0, g0, b0;
      rgb_of (pic, 30, 20, r0, g0, b0);
      paint (page, ren, rs);
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

static tree
make_pattern (url u, tree eff, string base= "") {
  tree p (_PATTERN, as_string (u), "100%", "100%");
  if (eff != tree ("") || base != "") p << eff;
  if (base != "") p << tree (base);
  return p;
}

static void
paint_page (picture& pic, renderer& ren, tree pat, int alpha, color previous,
            brush doc_bg, bool screen= true) {
  box page= make_page (brush (pat, alpha),
                       empty_box (path (), 0, 0, 0, 0), box ());
  pic= make_screen (ren, previous, doc_bg);
  ren->is_screen= screen;
  rectangles rs;
  paint (page, ren, rs);
  paint (page, ren, rs);
}

static bool
is_rgb (picture pic, int x, int y, int r0, int g0, int b0, int tol= 2) {
  int r, g, b;
  rgb_of (pic, x, y, r, g, b);
  return abs (r - r0) <= tol && abs (g - g0) <= tol && abs (b - b0) <= tol;
}

// the base color (5th argument) is painted under the image, opaque, whatever
// its alpha or the opacity of the brush, whatever was there, also twice
static void
test_pattern_base_color () {
  if (get_env ("TEXMACS_HOME_PATH") == "")
    SKIP ("TEXMACS_HOME_PATH is not set");
  url u= save_pattern ();
  picture pic; renderer ren;
  color red= rgb_color (255, 0, 0);
  brush doc (white);
  paint_page (pic, ren, make_pattern (u, "", "#00ff0080"), 255, red, doc);
  CHECK (is_rgb (pic, 10, 20, 0, 255, 0));
  tm_delete (ren);
  paint_page (pic, ren, make_pattern (u, "", "blue"), 128, red, doc);
  CHECK (is_rgb (pic, 10, 20, 0, 0, 255));
  tm_delete (ren);
  // in a print too (the paper is then white and painted by the printer)
  paint_page (pic, ren, make_pattern (u, "", "blue"), 255, red, doc, false);
  CHECK (is_rgb (pic, 10, 20, 0, 0, 255));
  tm_delete (ren);
  // an effect and a base color together: the image is recolored red (the
  // translucent part is red at 25% over the base color)
  tree eff (EFF_RECOLOR, "0", "red");
  paint_page (pic, ren, make_pattern (u, eff, "blue"), 255, red, doc);
  CHECK (is_rgb (pic, 10, 20, 0, 0, 255));
  CHECK (is_rgb (pic, 30, 20, 64, 0, 191, 4));
  tm_delete (ren);
  // not a color: none, empty (no base color)
  paint_page (pic, ren, make_pattern (u, "", "none"), 255, red, doc);
  CHECK (is_rgb (pic, 10, 20, 255, 255, 255));
  tm_delete (ren);
  remove (u);
}

// no base color: on a screen the page starts from the paper, which is the
// background color of the document if that is a color, and else the default
// of bg-color in the environment (a setting), never from the earlier paint
static void
test_pattern_paper () {
  if (get_env ("TEXMACS_HOME_PATH") == "")
    SKIP ("TEXMACS_HOME_PATH is not set");
  url u= save_pattern ();
  picture pic; renderer ren;
  color red= rgb_color (255, 0, 0);
  tree pat= make_pattern (u, "");
  paint_page (pic, ren, pat, 255, red, brush (rgb_color (0, 128, 0)));
  CHECK (is_rgb (pic, 10, 20, 0, 128, 0));
  tm_delete (ren);
  initialize_default_env ();
  tree old= default_env [BG_COLOR];
  default_env (BG_COLOR)= tree ("#ff8000");
  paint_page (pic, ren, pat, 255, red, brush (false));
  CHECK (is_rgb (pic, 10, 20, 255, 128, 0));
  tm_delete (ren);
  default_env (BG_COLOR)= old;
  // an old pattern with a color as 4th argument: it is no base color
  tree pat4 (_PATTERN, as_string (u), "100%", "100%", "blue");
  paint_page (pic, ren, pat4, 255, red, brush (white));
  CHECK (is_rgb (pic, 10, 20, 255, 255, 255));
  tm_delete (ren);
  remove (u);
}

// a pattern nested in the page (a cell, with|bg-color): transparent when
// no base color was chosen, over the base color when there is one
static void
test_pattern_nested () {
  if (get_env ("TEXMACS_HOME_PATH") == "")
    SKIP ("TEXMACS_HOME_PATH is not set");
  url u= save_pattern ();
  renderer ren;
  picture pic= make_screen (ren, rgb_color (255, 0, 0));
  ren->set_background (brush (make_pattern (u, ""), 255));
  ren->clear_pattern (0, -W*PX, W*PX, 0);
  CHECK (is_rgb (pic, 10, 20, 255, 0, 0));   // still the earlier paint
  ren->set_background (brush (make_pattern (u, "", "blue"), 255));
  ren->clear_pattern (0, -W*PX, W*PX, 0);
  CHECK (is_rgb (pic, 10, 20, 0, 0, 255));
  tm_delete (ren);
  remove (u);
}

int
main () {
  RUN (test_decorations_below_text);
  RUN (test_pattern_background);
  RUN (test_pattern_base_color);
  RUN (test_pattern_paper);
  RUN (test_pattern_nested);
  return test_report ();
}
