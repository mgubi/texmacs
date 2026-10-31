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
glyph hor_extend (glyph gl, int pos, int by);
glyph hor_take (glyph gl, int pos, int nr);
#include <unistd.h>

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

// the operations whose result is too large give an empty glyph, without
// writing into it (a FAILED "bad x-index" left the window blank)
static void
test_ops_too_large () {
  glyph h (20000, 1, 0, 1);
  glyph v (1, 20000, 0, 20000);
  h->set_x (0, 0, 1);
  v->set_x (0, 0, 1);
  bool ok= true;
  glyph j;
  try { j= join (h, v); } catch (...) { ok= false; }
  CHECK_MSG (ok, "join of 20000x1 and 1x20000 failed");
  if (ok) {
    CHECK_EQ (j->width, 0);
    CHECK_EQ (j->get_x (0, 0), 0);
  }
  glyph t (10, 30000, 0, 30000);
  t->set_x (5, 5, 1);
  glyph e;
  ok= true;
  try { e= hor_extend (t, 5, 10000); } catch (...) { ok= false; }
  CHECK_MSG (ok, "hor_extend of 10x30000 by 10000 failed");
  if (ok) CHECK_EQ (e->width, 0);
  // and the raw accessors of an empty glyph do nothing
  glyph z (100000, 100000, 0, 0);
  z->set_1 (5, 5, 1);
  CHECK_EQ (z->get_1 (5, 5), 0);
}

// hor_extend and hor_take of an empty glyph, or of a blank glyph 1 pixel
// wide, return (their search of a column with ink went on forever: the
// alarm ends the test then)
static void
test_extend_empty () {
  alarm (20);
  glyph z (100000, 100000, 0, 0);
  glyph e= hor_extend (z, -1, 100);
  CHECK_EQ (e->height, 0);
  glyph t= hor_take (z, -1, 10);
  CHECK_EQ (t->height, 0);
  glyph b (1, 5, 0, 5);
  glyph be= hor_extend (b, 0, 3);
  CHECK_EQ (be->width, 4);
  glyph bt= hor_take (b, 0, 3);
  CHECK (bt->width <= 3);
  alarm (0);
}

int
main () {
  RUN (test_wide);
  RUN (test_too_large);
  RUN (test_ops_too_large);
  RUN (test_extend_empty);
  return test_report ();
}
