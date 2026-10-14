/******************************************************************************
* MODULE     : point_test.cpp
* DESCRIPTION: tests of points and of elementary planar geometry
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "point.hpp"
#include "math_util.hpp"

static bool
near_eq (double x, double y, double eps= 1.0e-9) {
  return fabs (x - y) <= eps;
}

static bool
near_eq (point p, point q, double eps= 1.0e-9) {
  if (N(p) != N(q)) return false;
  for (int i=0; i<N(p); i++)
    if (!near_eq (p[i], q[i], eps)) return false;
  return true;
}

static string
show (point p) {
  string s= "(";
  for (int i=0; i<N(p); i++) {
    if (i > 0) s << ", ";
    s << as_string (p[i]);
  }
  return s * ")";
}

#define CHECK_NEAR(x, y) \
  CHECK_MSG (near_eq (x, y), #x " is " * test_show (x) * ", not " #y)
#define CHECK_POINT(p, q) \
  CHECK_MSG (near_eq (p, q), #p " is " * show (p) * ", not " * show (q))

/******************************************************************************
* Arithmetic
******************************************************************************/

static void
test_arithmetic () {
  point p (1.0, 2.0, 3.0), q (4.0, -1.0, 0.5);
  CHECK_POINT (p + q, point (5.0, 1.0, 3.5));
  CHECK_POINT (p - q, point (-3.0, 3.0, 2.5));
  CHECK_POINT (-p, point (-1.0, -2.0, -3.0));
  CHECK_POINT (2.0 * p, point (2.0, 4.0, 6.0));
  CHECK_POINT (p * q, point (4.0, -2.0, 1.5));
  CHECK_POINT (p / 2.0, point (0.5, 1.0, 1.5));
  CHECK_POINT (p / q, point (0.25, -2.0, 6.0));
  CHECK_POINT (abs (q), point (4.0, 1.0, 0.5));
  CHECK_NEAR (min (q), -1.0);
  CHECK_NEAR (max (q), 4.0);
  CHECK_NEAR (min (as_point (7.0)), 7.0);
}

// binary operations truncate to the shorter of the two points
static void
test_mixed_dimensions () {
  point p (1.0, 2.0, 3.0), q (10.0, 20.0);
  CHECK_POINT (p + q, point (11.0, 22.0));
  CHECK_POINT (q - p, point (9.0, 18.0));
  CHECK_NEAR (inner (p, q), 50.0);
  CHECK_EQ (N(p + point ()), 0);
}

// equality is up to 1e-6 and requires equal dimensions
static void
test_equality () {
  point p (1.0, 2.0);
  CHECK (p == point (1.0, 2.0));
  CHECK (p == point (1.0 + 1.0e-8, 2.0 - 1.0e-8));
  CHECK (!(p == point (1.0, 2.001)));
  CHECK (!(p == point (1.0, 2.0, 0.0)));
  CHECK (point () == point ());
}

static void
test_norm_inner () {
  struct { double x, y, n; } cases[]= {
    { 0.0, 0.0, 0.0 }, { 3.0, 4.0, 5.0 }, { -5.0, 12.0, 13.0 },
    { 1.0, 1.0, sqrt (2.0) } };
  for (int i=0; i<4; i++) {
    point p (cases[i].x, cases[i].y);
    CHECK_NEAR (norm (p), cases[i].n);
    CHECK_NEAR (inner (p, p), cases[i].n * cases[i].n);
  }
  CHECK_NEAR (inner (point (1.0, 0.0), point (0.0, 1.0)), 0.0);
  CHECK_NEAR (inner (point (1.0, 2.0, 3.0), point (4.0, 5.0, 6.0)), 32.0);
}

// arg is the angle in [0, 2 pi)
static void
test_arg () {
  struct { double x, y, a; } cases[]= {
    { 1.0, 0.0, 0.0 }, { 0.0, 1.0, tm_PI / 2 }, { -1.0, 0.0, tm_PI },
    { 0.0, -1.0, 3 * tm_PI / 2 }, { 2.0, 2.0, tm_PI / 4 },
    { 1.0, -1.0, 7 * tm_PI / 4 } };
  for (int i=0; i<6; i++)
    CHECK_MSG (near_eq (arg (point (cases[i].x, cases[i].y)), cases[i].a),
               "arg of case " * as_string (i));
}

static void
test_rotate_slant () {
  point o (0.0, 0.0), c (1.0, 1.0);
  CHECK_POINT (rotate_2D (point (1.0, 0.0), o, tm_PI / 2), point (0.0, 1.0));
  CHECK_POINT (rotate_2D (point (1.0, 0.0), o, tm_PI), point (-1.0, 0.0));
  CHECK_POINT (rotate_2D (point (2.0, 1.0), c, tm_PI / 2), point (1.0, 2.0));
  CHECK_POINT (rotate_2D (point (2.0, 1.0), c, 0.0), point (2.0, 1.0));
  // a rotation preserves the distance to the center
  point r= rotate_2D (point (3.0, -2.0), c, 0.7);
  CHECK_NEAR (norm (r - c), norm (point (3.0, -2.0) - c));
  CHECK_POINT (slanted (point (1.0, 2.0), 0.5), point (2.0, 2.0));
  CHECK_POINT (slanted (point (1.0, 0.0), 0.5), point (1.0, 0.0));
}

/******************************************************************************
* Linear dependency and orthogonalization
******************************************************************************/

static void
test_collinear () {
  CHECK (collinear (point (1.0, 2.0), point (2.0, 4.0)));
  CHECK (collinear (point (1.0, 2.0), point (-3.0, -6.0)));
  CHECK (!collinear (point (1.0, 0.0), point (0.0, 1.0)));
  CHECK (!collinear (point (1.0, 0.0), point (1.0, 0.1)));
  point a (0.0, 0.0), b (1.0, 1.0), c (2.0, 2.0), d (2.0, 0.0);
  CHECK (linearly_dependent (a, b, c));
  CHECK (linearly_dependent (a, a, d));
  CHECK (!linearly_dependent (a, b, d));
}

static void
test_orthogonalize () {
  point i, j;
  CHECK (!orthogonalize (i, j, point (0.0, 0.0), point (1.0, 1.0),
                         point (3.0, 3.0)));
  CHECK (orthogonalize (i, j, point (1.0, 1.0), point (3.0, 1.0),
                        point (2.0, 5.0)));
  CHECK_POINT (i, point (1.0, 0.0));
  CHECK_POINT (j, point (0.0, 1.0));
  // in space: an orthonormal basis of the plane through the three points
  point p1 (0.0, 0.0, 0.0), p2 (1.0, 1.0, 0.0), p3 (0.0, 1.0, 1.0);
  CHECK (orthogonalize (i, j, p1, p2, p3));
  CHECK_NEAR (norm (i), 1.0);
  CHECK_NEAR (norm (j), 1.0);
  CHECK_NEAR (inner (i, j), 0.0);
  point v= p3 - p1;
  CHECK_POINT (inner (v, i) * i + inner (v, j) * j, v);
}

/******************************************************************************
* Projections, distances and intersections
******************************************************************************/

static axis
make_axis (point p0, point p1) {
  axis a;
  a.p0= p0;
  a.p1= p1;
  return a;
}

static void
test_projection () {
  axis ax= make_axis (point (0.0, 0.0), point (2.0, 0.0));
  CHECK_POINT (proj (ax, point (1.0, 5.0)), point (1.0, 0.0));
  CHECK_POINT (proj (ax, point (-3.0, -1.0)), point (-3.0, 0.0));
  CHECK_NEAR (dist (ax, point (1.0, 5.0)), 5.0);
  CHECK_NEAR (dist (ax, point (7.0, -2.0)), 2.0);
  axis diag= make_axis (point (1.0, 1.0), point (2.0, 2.0));
  CHECK_POINT (proj (diag, point (0.0, 2.0)), point (1.0, 1.0));
  CHECK_NEAR (dist (diag, point (0.0, 2.0)), sqrt (2.0));
  // a degenerate axis projects everything on its point
  axis pt= make_axis (point (3.0, 4.0), point (3.0, 4.0));
  CHECK_POINT (proj (pt, point (0.0, 0.0)), point (3.0, 4.0));
}

// the distance to a segment is the distance to the line inside the
// segment and the distance to the nearest end outside of it
static void
test_segment_distance () {
  point a (0.0, 0.0), b (4.0, 0.0);
  struct { double x, y, d; } cases[]= {
    { 2.0, 3.0, 3.0 }, { 2.0, -1.0, 1.0 }, { -3.0, 4.0, 5.0 },
    { 7.0, 4.0, 5.0 }, { 4.0, 0.0, 0.0 }, { 1.0, 0.0, 0.0 } };
  for (int i=0; i<6; i++)
    CHECK_MSG (near_eq (seg_dist (a, b, point (cases[i].x, cases[i].y)),
                     cases[i].d),
               "seg_dist of case " * as_string (i));
  CHECK_NEAR (seg_dist (make_axis (a, b), point (2.0, 3.0)), 3.0);
}

static void
test_intersection () {
  axis h= make_axis (point (0.0, 1.0), point (1.0, 1.0));
  axis v= make_axis (point (3.0, 0.0), point (3.0, 1.0));
  CHECK_POINT (intersection (h, v), point (3.0, 1.0));
  axis d1= make_axis (point (0.0, 0.0), point (1.0, 1.0));
  axis d2= make_axis (point (0.0, 4.0), point (1.0, 3.0));
  CHECK_POINT (intersection (d1, d2), point (2.0, 2.0));
  CHECK_POINT (intersection (d2, d1), point (2.0, 2.0));
  // parallel lines do not meet: the result is the empty point
  axis h2= make_axis (point (0.0, 2.0), point (5.0, 2.0));
  CHECK_EQ (N(intersection (h, h2)), 0);
}

// the perpendicular bisectors of a triangle meet in the circumcenter
static void
test_midperp () {
  point p1 (0.0, 0.0), p2 (4.0, 0.0), p3 (0.0, 2.0);
  axis m= midperp (p1, p2, p3);
  CHECK_POINT (m.p0, point (2.0, 0.0));
  CHECK_NEAR (inner (m.p1 - m.p0, p2 - p1), 0.0);
  point c= intersection (midperp (p1, p2, p3), midperp (p2, p3, p1));
  CHECK_POINT (c, point (2.0, 1.0));
  CHECK_NEAR (norm (c - p1), norm (c - p2));
  CHECK_NEAR (norm (c - p1), norm (c - p3));
  // a degenerate triangle has no bisector: an empty axis
  axis z= midperp (p1, p2, point (8.0, 0.0));
  CHECK_EQ (N(z.p0), 0);
}

static void
test_inside_rectangle () {
  point lo (0.0, 0.0), hi (2.0, 1.0);
  CHECK (inside_rectangle (point (1.0, 0.5), lo, hi));
  CHECK (inside_rectangle (point (0.0, 0.0), lo, hi));
  CHECK (inside_rectangle (point (2.0, 1.0), lo, hi));
  CHECK (!inside_rectangle (point (2.1, 0.5), lo, hi));
  CHECK (!inside_rectangle (point (1.0, -0.1), lo, hi));
}

/******************************************************************************
* Conversion to and from trees
******************************************************************************/

static void
test_trees () {
  point p (1.5, -2.0, 0.25);
  tree t= as_tree (p);
  CHECK (is_point (t));
  CHECK_EQ (N(t), 3);
  CHECK_POINT (as_point (t), p);
  CHECK_POINT (as_point (tuple ("1", "2")), point (1.0, 2.0));
  CHECK_EQ (N(as_point (tree ("1"))), 0);
  CHECK_EQ (N(as_point (3.0)), 1);
  CHECK_NEAR (as_point (3.0)[0], 3.0);
}

int
main () {
  RUN (test_arithmetic);
  RUN (test_mixed_dimensions);
  RUN (test_equality);
  RUN (test_norm_inner);
  RUN (test_arg);
  RUN (test_rotate_slant);
  RUN (test_collinear);
  RUN (test_orthogonalize);
  RUN (test_projection);
  RUN (test_segment_distance);
  RUN (test_intersection);
  RUN (test_midperp);
  RUN (test_inside_rectangle);
  RUN (test_trees);
  return test_report ();
}
