/******************************************************************************
* MODULE     : glyph_test.cpp
* DESCRIPTION: tests of the glyphs of the bitmap fonts: their sizes
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "bitmap_font.hpp"

glyph join (glyph gl1, glyph gl2);

// a glyph wider than a short: the wide braces of the virtual fonts at a
// large zoom, joined from pieces (#659: the width wrapped to a negative
// number and the raster could not be allocated, which stopped TeXmacs)
static void
test_wide () {
  glyph a (20119, 330, 24, 119);
  glyph b (21007, 541, -20076, 330);
  a->set_x (0, 0, 1);
  b->set_x (21006, 540, 1);
  glyph j= join (a, b);
  CHECK_EQ (j->width, 41107);
  CHECK_EQ (j->height, 541);
  CHECK_EQ (j->xoff, 24);
  CHECK_EQ (j->get_x (0, 211), 1);
  CHECK_EQ (j->get_x (41106, 540), 1);
}

// a glyph too large for its raster is empty rather than an abort
static void
test_too_large () {
  glyph g (100000, 100000, 0, 0);
  CHECK_EQ (g->width, 0);
  CHECK_EQ (g->height, 0);
  glyph n (-5, 10, 0, 0);
  CHECK_EQ (n->width, 0);
}

int
main () {
  RUN (test_wide);
  RUN (test_too_large);
  return test_report ();
}
