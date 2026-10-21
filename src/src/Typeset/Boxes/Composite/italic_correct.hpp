
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
  // the corrections are kept for italic_restore, which undoes them: those
  // of a concatenation depend on its extents, which this changes (the
  // markers around a macro argument, of width zero, would then count as its
  // first and last boxes, without corrections)
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
