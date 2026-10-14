/******************************************************************************
* MODULE     : rectangles_test.cpp
* DESCRIPTION: tests of rectangles and lists of rectangles
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "rectangles.hpp"

static string
show (rectangle r) {
  return "(" * as_string (r->x1) * ", " * as_string (r->y1) * ", " *
         as_string (r->x2) * ", " * as_string (r->y2) * ")";
}

#define CHECK_RECT(r, x1, y1, x2, y2) \
  CHECK_EQ (show (r), show (rectangle (x1, y1, x2, y2)))

static rectangles
L (rectangle r1) {
  return rectangles (r1);
}

static rectangles
L (rectangle r1, rectangle r2) {
  return rectangles (r1, r2, rectangles ());
}

static rectangles
L (rectangle r1, rectangle r2, rectangle r3) {
  return rectangles (r1, rectangles (r2, rectangles (r3)));
}

// no two rectangles of the list overlap
static bool
pairwise_disjoint (rectangles l) {
  for (rectangles a= l; !is_nil (a); a= a->next)
    for (rectangles b= a->next; !is_nil (b); b= b->next)
      if (intersect (a->item, b->item)) return false;
  return true;
}

// every rectangle of the list lies inside r
static bool
all_inside (rectangles l, rectangle r) {
  for (; !is_nil (l); l= l->next)
    if (!(l->item <= r)) return false;
  return true;
}

// a few pairs of rectangles in various relative positions
static rectangle pairs[][2]= {
  { rectangle (0, 0, 10, 10), rectangle (5, 5, 15, 15) },     // corners
  { rectangle (0, 0, 10, 10), rectangle (2, 2, 8, 8) },       // nested
  { rectangle (0, 0, 10, 10), rectangle (10, 0, 20, 10) },    // touching
  { rectangle (0, 0, 10, 10), rectangle (20, 20, 30, 30) },   // apart
  { rectangle (0, 0, 10, 10), rectangle (3, -5, 6, 20) },     // a cross
  { rectangle (-10, -10, 0, 0), rectangle (-5, -20, 5, -5) },
  { rectangle (0, 0, 10, 10), rectangle (0, 0, 10, 10) }      // equal
};

/******************************************************************************
* Single rectangles
******************************************************************************/

static void
test_construction () {
  rectangle r;
  CHECK_RECT (r, 0, 0, 0, 0);
  rectangle s (1, 2, 3, 4);
  CHECK_EQ (s->x1, 1);
  CHECK_EQ (s->y1, 2);
  CHECK_EQ (s->x2, 3);
  CHECK_EQ (s->y2, 4);
  CHECK (s == rectangle (1, 2, 3, 4));
  CHECK (s != rectangle (1, 2, 3, 5));
  CHECK (!(s != rectangle (1, 2, 3, 4)));
  // a copy is independent, an assignment shares
  rectangle c= copy (s), a= s;
  c->x1= 100;
  CHECK_EQ (s->x1, 1);
  a->x1= 100;
  CHECK_EQ (s->x1, 100);
  // conversion to a tree
  tree t= (tree) rectangle (1, -2, 30, 4);
  CHECK (t == tree (TUPLE, "1", "-2", "30", "4"));
}

static void
test_area () {
  CHECK_EQ (area (rectangle (0, 0, 10, 20)), 200.0);
  CHECK_EQ (area (rectangle (-5, -5, 5, 5)), 100.0);
  CHECK_EQ (area (rectangle (0, 0, 0, 10)), 0.0);
  // a rectangle with reversed corners counts as empty
  CHECK_EQ (area (rectangle (10, 0, 0, 10)), 0.0);
  CHECK_EQ (area (rectangle (0, 10, 10, 0)), 0.0);
  // large coordinates do not overflow
  CHECK_EQ (area (rectangle (0, 0, 1 << 20, 1 << 20)), 1099511627776.0);
}

// intersect is strict: rectangles touching along an edge do not intersect
static void
test_intersect () {
  bool expected[]= { true, true, false, false, true, true, true };
  for (int i=0; i<7; i++) {
    rectangle r1= pairs[i][0], r2= pairs[i][1];
    CHECK_MSG (intersect (r1, r2) == expected[i],
               "intersect " * show (r1) * " " * show (r2));
    CHECK (intersect (r1, r2) == intersect (r2, r1));
  }
  // only the strict inequalities on the coordinates are tested: a
  // degenerate rectangle inside another one counts as intersecting, and
  // the empty pieces this produces are removed later by correct
  CHECK (intersect (rectangle (5, 5, 5, 5), rectangle (0, 0, 10, 10)));
  CHECK (!intersect (rectangle (5, 5, 5, 5), rectangle (5, 5, 5, 5)));
  rectangles d= rectangles (rectangle (5, 5, 5, 5)) &
                rectangles (rectangle (0, 0, 10, 10));
  CHECK_EQ (N (d), 1);
  CHECK (is_nil (correct (d)));
}

static void
test_inclusion () {
  rectangle big (0, 0, 10, 10);
  CHECK (rectangle (2, 2, 8, 8) <= big);
  CHECK (big <= big);
  CHECK (rectangle (0, 0, 10, 5) <= big);
  CHECK (!(big <= rectangle (2, 2, 8, 8)));
  CHECK (!(rectangle (5, 5, 15, 15) <= big));
  CHECK (!(rectangle (-1, 0, 10, 10) <= big));
}

static void
test_transformations () {
  rectangle r (1, 2, 3, 4);
  CHECK_RECT (translate (r, 10, -10), 11, -8, 13, -6);
  CHECK_RECT (translate (r, 0, 0), 1, 2, 3, 4);
  CHECK_RECT (thicken (r, 1, 2), 0, 0, 4, 6);
  CHECK_RECT (thicken (thicken (r, 1, 2), -1, -2), 1, 2, 3, 4);
  CHECK_RECT (r * 3, 3, 6, 9, 12);
  CHECK_RECT (rectangle (3, 6, 9, 12) / 3, 1, 2, 3, 4);
  // integer division truncates towards zero
  CHECK_RECT (rectangle (-3, -1, 3, 5) / 2, -1, 0, 1, 2);
  // with a double factor the result is rounded outwards
  CHECK_RECT (r * 0.5, 0, 1, 2, 2);
  CHECK_RECT (rectangle (-3, -1, 3, 5) / 2.0, -2, -1, 2, 3);
  CHECK_RECT (r * 1.0, 1, 2, 3, 4);
  for (int i=0; i<7; i++) {
    rectangle s= pairs[i][0] * 0.3;
    CHECK (pairs[i][0] * 0.3 <= s);
    CHECK (area (pairs[i][1] / 0.7) >= area (pairs[i][1]));
  }
  // translation keeps the area, thickening adds a frame
  CHECK_EQ (area (translate (r, 7, 9)), area (r));
  CHECK_EQ (area (thicken (r, 1, 1)), 16.0);
  // the source is never modified
  CHECK_RECT (r, 1, 2, 3, 4);
}

static void
test_least_upper_bound () {
  CHECK_RECT (least_upper_bound (rectangle (0, 0, 1, 1), rectangle (5, 6, 7, 8)),
              0, 0, 7, 8);
  for (int i=0; i<7; i++) {
    rectangle r1= pairs[i][0], r2= pairs[i][1];
    rectangle u= least_upper_bound (r1, r2);
    CHECK (r1 <= u && r2 <= u);
    CHECK (u == least_upper_bound (r2, r1));
    CHECK (u == least_upper_bound (L (r1, r2)));
  }
  CHECK_RECT (least_upper_bound (L (rectangle (1, 2, 3, 4))), 1, 2, 3, 4);
  CHECK_RECT (least_upper_bound (L (rectangle (0, 5, 1, 6),
                                    rectangle (-2, 0, 0, 1),
                                    rectangle (4, 4, 5, 5))),
              -2, 0, 5, 6);
  // the bound of a list is a fresh rectangle
  rectangles l= L (rectangle (1, 2, 3, 4));
  rectangle b= least_upper_bound (l);
  b->x1= 0;
  CHECK_RECT (l->item, 1, 2, 3, 4);
}

/******************************************************************************
* Lists of rectangles
******************************************************************************/

static void
test_list_intersection () {
  CHECK (is_nil (rectangles () & L (rectangle (0, 0, 1, 1))));
  CHECK (is_nil (L (rectangle (0, 0, 1, 1)) & rectangles ()));
  rectangles l= L (rectangle (0, 0, 10, 10)) & L (rectangle (5, 5, 15, 15));
  CHECK_EQ (N (l), 1);
  CHECK_RECT (l->item, 5, 5, 10, 10);
  for (int i=0; i<7; i++) {
    rectangle r1= pairs[i][0], r2= pairs[i][1];
    rectangles i12= L (r1) & L (r2), i21= L (r2) & L (r1);
    CHECK_EQ (N (i12), intersect (r1, r2)? 1: 0);
    CHECK (i12 == i21);
    CHECK (all_inside (i12, r1) && all_inside (i12, r2));
  }
  // with several rectangles on each side the pieces are pairwise intersections
  rectangles a= L (rectangle (0, 0, 10, 10), rectangle (20, 0, 30, 10));
  rectangles b= L (rectangle (5, 5, 25, 15));
  rectangles ab= a & b;
  CHECK_EQ (N (ab), 2);
  CHECK_EQ (area (ab), 50.0);
}

// l1 - l2 is a list of disjoint pieces covering l1 outside of l2
static void
test_difference () {
  CHECK (is_nil (rectangles () - L (rectangle (0, 0, 1, 1))));
  rectangles a= L (rectangle (0, 0, 10, 10));
  CHECK (a - rectangles () == a);
  CHECK (is_nil (a - a));
  CHECK (is_nil (a - L (rectangle (-1, -1, 11, 11))));
  // removing a disjoint rectangle keeps the original
  CHECK (a - L (rectangle (20, 20, 30, 30)) == a);
  // a hole in the middle leaves a frame of four pieces
  rectangles frame= a - L (rectangle (2, 2, 8, 8));
  CHECK_EQ (N (frame), 4);
  CHECK_EQ (area (frame), 64.0);
  CHECK (pairwise_disjoint (frame));
  for (int i=0; i<7; i++) {
    rectangle r1= pairs[i][0], r2= pairs[i][1];
    rectangles d= L (r1) - L (r2);
    CHECK (pairwise_disjoint (d));
    CHECK (all_inside (d, r1));
    CHECK (is_nil (d & L (r2)));
    CHECK_EQ (area (d), area (r1) - area (L (r1) & L (r2)));
  }
  // subtracting several rectangles one after the other
  rectangles two= L (rectangle (0, 0, 5, 10), rectangle (5, 0, 10, 10));
  CHECK (is_nil (a - two));
}

// l1 | l2 covers both lists; adjacent pieces of equal height or width
// are merged
static void
test_union () {
  for (int i=0; i<7; i++) {
    rectangle r1= pairs[i][0], r2= pairs[i][1];
    rectangles u= L (r1) | L (r2);
    CHECK (pairwise_disjoint (u));
    CHECK (all_inside (u, least_upper_bound (r1, r2)));
    CHECK_EQ (area (u), area (r1) + area (r2) - area (L (r1) & L (r2)));
    CHECK_EQ (area (L (r2) | L (r1)), area (u));
  }
  // two halves side by side make a single rectangle
  rectangles h= L (rectangle (0, 0, 5, 10)) | L (rectangle (5, 0, 10, 10));
  CHECK_EQ (N (h), 1);
  CHECK_RECT (h->item, 0, 0, 10, 10);
  rectangles v= L (rectangle (0, 0, 10, 4)) | L (rectangle (0, 4, 10, 10));
  CHECK_EQ (N (v), 1);
  CHECK_RECT (v->item, 0, 0, 10, 10);
  // touching along part of an edge only: no merge
  rectangles p= L (rectangle (0, 0, 5, 10)) | L (rectangle (5, 0, 10, 5));
  CHECK_EQ (N (p), 2);
  // union with an included rectangle gives the larger one
  rectangles n= L (rectangle (2, 2, 8, 8)) | L (rectangle (0, 0, 10, 10));
  CHECK_EQ (N (n), 1);
  CHECK_RECT (n->item, 0, 0, 10, 10);
  CHECK (is_nil (rectangles () | rectangles ()));
}

static void
test_correct_and_simplify () {
  rectangles l= L (rectangle (0, 0, 10, 10),
                   rectangle (5, 5, 5, 8),        // zero width
                   rectangle (0, 4, 3, 2));       // reversed
  rectangles c= correct (l);
  CHECK_EQ (N (c), 1);
  CHECK_RECT (c->item, 0, 0, 10, 10);
  CHECK (is_nil (correct (rectangles ())));
  CHECK (correct (c) == c);
  // three adjacent strips collapse into one rectangle
  rectangles strips= L (rectangle (0, 0, 10, 1),
                        rectangle (0, 1, 10, 2),
                        rectangle (0, 2, 10, 3));
  rectangles s= simplify (strips);
  CHECK_EQ (N (s), 1);
  CHECK_RECT (s->item, 0, 0, 10, 3);
  // simplification keeps the covered area of disjoint rectangles
  rectangles d= L (rectangle (0, 0, 2, 2), rectangle (5, 5, 7, 7));
  CHECK_EQ (area (simplify (d)), area (d));
  CHECK (is_nil (simplify (rectangles ())));
}

static void
test_list_transformations () {
  rectangles l= L (rectangle (0, 0, 1, 1), rectangle (2, 2, 4, 4));
  rectangles t= translate (l, 1, 2);
  CHECK_EQ (N (t), 2);
  CHECK_RECT (t->item, 1, 2, 2, 3);
  CHECK_RECT (t->next->item, 3, 4, 5, 6);
  rectangles k= thicken (l, 1, 0);
  CHECK_RECT (k->item, -1, 0, 2, 1);
  CHECK_RECT (k->next->item, 1, 2, 5, 4);
  rectangles m= l * 3;
  CHECK_RECT (m->next->item, 6, 6, 12, 12);
  CHECK (m / 3 == l);
  CHECK_EQ (area (l), 5.0);
  CHECK_EQ (area (m), 45.0);
  CHECK_EQ (area (rectangles ()), 0.0);
  CHECK (is_nil (translate (rectangles (), 1, 1)));
  CHECK (is_nil (thicken (rectangles (), 1, 1)));
  // the original list is not modified
  CHECK_RECT (l->item, 0, 0, 1, 1);
}

// outlines draws a frame of width pixel on the left and right and of
// height pixel above and below, just outside the rectangle thickened by
// 2 pixel vertically
static void
test_outlines () {
  rectangle r (0, 0, 10, 10);
  rectangles o= outlines (L (r), 1);
  CHECK (pairwise_disjoint (o));
  CHECK_EQ (area (o), 52.0);
  CHECK (is_nil (o & L (thicken (r, 0, 2))));
  CHECK (all_inside (o, thicken (r, 1, 3)));
  CHECK (is_nil (outlines (rectangles (), 1)));
}

int
main () {
  RUN (test_construction);
  RUN (test_area);
  RUN (test_intersect);
  RUN (test_inclusion);
  RUN (test_transformations);
  RUN (test_least_upper_bound);
  RUN (test_list_intersection);
  RUN (test_difference);
  RUN (test_union);
  RUN (test_correct_and_simplify);
  RUN (test_list_transformations);
  RUN (test_outlines);
  return test_report ();
}
