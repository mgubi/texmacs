
/******************************************************************************
* MODULE     : italic_correct.h
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ITALIC_CORRECT_H
#define ITALIC_CORRECT_H
#include "boxes.hpp"

inline void
italic_correct (box b, SI& lc, SI& rc) {
  // the corrections are kept for italic_restore, which undoes exactly them:
  // asked again after the widening, they can differ, since some depend on
  // the extents of the box, which this changes. Those of a concatenation
  // come from its first and last boxes within its extents (get_first and
  // get_last in concat_boxes.cpp): once widened, boxes of width zero or
  // overhanging the edges (the markers around a macro argument, labels,
  // specifics, locus markers) can become them, with other corrections;
  // those of a side box (side_box_rep::left_correction and
  // right_correction in script_boxes.cpp) are measured from its extents.
  lc= b->left_correction ();
  rc= b->right_correction ();
  b->x1 -= lc;
  b->x2 += rc;
}

inline void
italic_restore (box b, SI lc, SI rc) {
  b->x1 += lc;
  b->x2 -= rc;
}

#endif // defined ITALIC_CORRECT_H
