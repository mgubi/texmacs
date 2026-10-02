/******************************************************************************
* MODULE     : frame_test.cpp
* DESCRIPTION: tests of coordinate frames and of elementary curves
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "frame.hpp"
#include "curve.hpp"
#include "matrix.hpp"
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

// sample points on which identities are checked
static array<point>
samples () {
  array<point> a;
  a << point (0.0, 0.0) << point (1.0, 0.0) << point (0.0, 1.0)
    << point (-2.5, 3.75) << point (100.0, -7.0) << point (0.1, 0.2);
  return a;
}

// the invertible frames of frame.hpp, with a name for the messages
static array<frame>
invertible_frames (array<string>& names) {
  array<frame> fs;
  fs << shift_2D (point (3.0, -1.0));            names << string ("shift");
  fs << scaling (2.5, point (1.0, 2.0));         names << string ("scaling");
  fs << scaling (point (2.0, -0.5), point (0.0, 1.0));
  names << string ("anisotropic scaling");
  fs << rotation_2D (point (1.0, 1.0), 0.6);     names << string ("rotation");
  fs << slanting (point (0.5, -1.0), 0.25);      names << string ("slanting");
  fs << linear_2D (matrix_2D<double> (2.0, 1.0, -1.0, 3.0));
  names << string ("linear");
  return fs;
}

/******************************************************************************
* Frames
******************************************************************************/

static void
test_known_values () {
  CHECK_POINT (shift_2D (point (1.0, 2.0)) (point (3.0, 4.0)),
               point (4.0, 6.0));
  CHECK_POINT (scaling (2.0, point (1.0, 1.0)) (point (3.0, -1.0)),
               point (7.0, -1.0));
  CHECK_POINT (scaling (point (2.0, 3.0), point (0.0, 0.0)) (point (1.0, 1.0)),
               point (2.0, 3.0));
  CHECK_POINT (rotation_2D (point (0.0, 0.0), tm_PI / 2) (point (1.0, 0.0)),
               point (0.0, 1.0));
  CHECK_POINT (slanting (point (0.0, 0.0), 0.5) (point (0.0, 2.0)),
               point (1.0, 2.0));
  CHECK_POINT (linear_2D (matrix_2D<double> (1.0, 2.0, 3.0, 4.0))
                 (point (1.0, 1.0)), point (3.0, 7.0));
  // affine: the last column of the 3x3 matrix is the translation
  matrix<double> m (1.0, 3, 3);
  m (0, 0)= 2.0; m (0, 2)= 5.0; m (1, 2)= -1.0;
  CHECK_POINT (affine_2D (m) (point (1.0, 1.0)), point (7.0, 0.0));
}

// f [f (p)] = p and f (f [p]) = p for every invertible frame
static void
test_inverse_transform () {
  array<string> names;
  array<frame> fs= invertible_frames (names);
  array<point> ps= samples ();
  for (int i=0; i<N(fs); i++)
    for (int j=0; j<N(ps); j++) {
      frame f= fs[i];
      CHECK_MSG (near_eq (f [f (ps[j])], ps[j]),
                 names[i] * ": inverse of direct at " * show (ps[j]));
      CHECK_MSG (near_eq (f (f [ps[j]]), ps[j]),
                 names[i] * ": direct of inverse at " * show (ps[j]));
    }
}

// composition and the invert operation of frames
static void
test_compose_invert () {
  array<string> names;
  array<frame> fs= invertible_frames (names);
  array<point> ps= samples ();
  for (int i=0; i<N(fs); i++)
    for (int k=0; k<N(fs); k++) {
      frame f= fs[i], g= fs[k];
      frame fg= f * g;
      frame id= f * invert (f);
      CHECK (fg->linear);
      for (int j=0; j<N(ps); j++) {
        point p= ps[j];
        string where= names[i] * " and " * names[k] * " at " * show (p);
        CHECK_MSG (near_eq (fg (p), f (g (p))), "composition: " * where);
        CHECK_MSG (near_eq (fg [p], g [f [p]]), "inverse of composition: "
                   * where);
        CHECK_MSG (near_eq (id (p), p), "f * invert (f): " * where);
        CHECK_MSG (near_eq (invert (f) (p), f [p]), "invert: " * where);
        CHECK_MSG (near_eq (invert (f) [p], f (p)), "invert back: " * where);
      }
    }
}

// for linear frames the jacobian is the linear part of the map
static void
test_jacobian () {
  array<string> names;
  array<frame> fs= invertible_frames (names);
  array<point> ps= samples ();
  point vs[]= { point (1.0, 0.0), point (0.0, 1.0), point (2.0, -3.0) };
  for (int i=0; i<N(fs); i++)
    for (int j=0; j<N(ps); j++)
      for (int k=0; k<3; k++) {
        frame f= fs[i];
        point p= ps[j], v= vs[k];
        bool error= true;
        point jv= f->jacobian (p, v, error);
        CHECK (!error);
        CHECK_MSG (near_eq (jv, f (p + v) - f (p), 1.0e-8),
                   names[i] * ": jacobian at " * show (p) * " on " * show (v));
      }
  // the jacobian of a composition is the product of the jacobians
  frame f= rotation_2D (point (1.0, 0.0), 0.3) * scaling (2.0, point (0.0, 1.0));
  bool error= true;
  point jv= f->jacobian (point (1.0, 1.0), point (1.0, 0.0), error);
  CHECK (!error);
  CHECK_POINT (jv, rotate_2D (point (2.0, 0.0), point (0.0, 0.0), 0.3));
}

// jacobian_of_inverse is the jacobian of the inverse map, checked here
// for all invertible frames but slanting, whose jacobian_of_inverse
// applies the direct slant (frame.cpp)
static void
test_jacobian_of_inverse () {
  frame fs[]= { shift_2D (point (3.0, -1.0)),
                scaling (2.5, point (1.0, 2.0)),
                scaling (point (2.0, -0.5), point (0.0, 1.0)),
                rotation_2D (point (1.0, 1.0), 0.6),
                linear_2D (matrix_2D<double> (2.0, 1.0, -1.0, 3.0)) };
  array<point> ps= samples ();
  point v (2.0, -3.0);
  for (int i=0; i<5; i++)
    for (int j=0; j<N(ps); j++) {
      point p= ps[j];
      bool error= true;
      point w= fs[i]->jacobian_of_inverse (p, v, error);
      CHECK (!error);
      CHECK_MSG (near_eq (w, fs[i] [p + v] - fs[i] [p], 1.0e-8),
                 "jacobian_of_inverse of frame " * as_string (i) *
                 " at " * show (p));
    }
}

// direct_bound (p, eps) bounds the displacement of the image
static void
test_bounds () {
  frame s= scaling (4.0, point (0.0, 0.0));
  CHECK_NEAR (s->direct_bound (point (0.0, 0.0), 1.0), 0.25);
  CHECK_NEAR (s->inverse_bound (point (0.0, 0.0), 1.0), 4.0);
  CHECK_NEAR (s->direct_scalar (3.0), 12.0);
  CHECK_NEAR (s->inverse_scalar (12.0), 3.0);
  frame r= rotation_2D (point (5.0, 5.0), 1.0);
  CHECK_NEAR (r->direct_scalar (2.0), 2.0);
  frame inv= invert (s);
  CHECK_NEAR (inv->direct_bound (point (0.0, 0.0), 1.0), 4.0);
}

// the bounding box of the image of a rectangle
static void
test_rectangles () {
  rectangle r (0, 0, 10, 20);
  rectangle s= scaling (2.0, point (1.0, 0.0)) (r);
  CHECK_EQ ((int) s->x1, 1);
  CHECK_EQ ((int) s->y1, 0);
  CHECK_EQ ((int) s->x2, 21);
  CHECK_EQ ((int) s->y2, 40);
  rectangle t= rotation_2D (point (0.0, 0.0), tm_PI / 2) (r);
  CHECK_EQ ((int) t->x1, -20);
  CHECK_EQ ((int) t->y1, 0);
  CHECK_EQ ((int) t->x2, 0);
  CHECK_EQ ((int) t->y2, 10);
  rectangle u= shift_2D (point (5.0, 5.0)) [r];
  CHECK_EQ ((int) u->x1, -5);
  CHECK_EQ ((int) u->y2, 15);
}

/******************************************************************************
* Curves
******************************************************************************/

static void
test_segment () {
  point a (1.0, 2.0), b (5.0, -2.0);
  curve c= segment (a, b);
  CHECK_EQ (c->nr_components (), 1);
  CHECK_POINT (c (0.0), a);
  CHECK_POINT (c (1.0), b);
  CHECK_POINT (c (0.25), point (2.0, 1.0));
  bool error= true;
  CHECK_POINT (c->grad (0.5, error), b - a);
  CHECK (!error);
  array<point> r= c->rectify (0.1);
  CHECK_EQ (N(r), 2);
  CHECK (N(r) == 2 && near_eq (r[0], a) && near_eq (r[1], b));
  array<double> abs;
  array<point> pts;
  array<path> cip;
  CHECK_EQ (c->get_control_points (abs, pts, cip), 2);
  CHECK (near_eq (pts[0], a) && near_eq (pts[1], b));
}

// a polyline is parameterized uniformly by its segments
static void
test_poly_segment () {
  array<point> a;
  a << point (0.0, 0.0) << point (2.0, 0.0) << point (2.0, 2.0);
  curve c= poly_segment (a, array<path> ());
  CHECK_EQ (c->nr_components (), 2);
  struct { double t, x, y; } cases[]= {
    { 0.0, 0.0, 0.0 }, { 0.25, 1.0, 0.0 }, { 0.5, 2.0, 0.0 },
    { 0.75, 2.0, 1.0 }, { 1.0, 2.0, 2.0 } };
  for (int i=0; i<5; i++)
    CHECK_MSG (near_eq (c (cases[i].t), point (cases[i].x, cases[i].y)),
               "poly_segment at " * as_string (cases[i].t));
  bool error;
  CHECK_POINT (c->grad (0.75, error), point (0.0, 4.0));
  array<point> r= c->rectify (0.01);
  CHECK_EQ (N(r), 3);
}

// an arc through three points of the unit circle
static void
test_arc () {
  array<point> a;
  a << point (1.0, 0.0) << point (0.0, 1.0) << point (-1.0, 0.0);
  curve c= arc (a, array<path> ());
  CHECK_POINT (c (0.0), point (1.0, 0.0));
  CHECK_POINT (c (0.5), point (0.0, 1.0));
  CHECK_POINT (c (1.0), point (-1.0, 0.0));
  bool ok= true;
  for (int i=0; i<=10; i++)
    if (!near_eq (norm (c (i / 10.0)), 1.0)) ok= false;
  CHECK_MSG (ok, "the arc stays on the circle");
  // the arc goes through the middle point, not around the other side
  for (int i=1; i<10; i++)
    if (c (i / 10.0) [1] < 0.0) ok= false;
  CHECK_MSG (ok, "the arc stays in the upper half plane");
  // a closed arc is the full circle
  curve full= arc (a, array<path> (), true);
  CHECK_POINT (full (1.0), point (1.0, 0.0));
  CHECK_POINT (full (0.75), point (0.0, -1.0));
  // collinear points give a degenerate arc
  array<point> b;
  b << point (0.0, 0.0) << point (1.0, 1.0) << point (2.0, 2.0);
  array<double> abs;
  array<point> pts;
  array<path> cip;
  arc (b, array<path> ())->get_control_points (abs, pts, cip);
  CHECK_EQ (N(pts), 1);
}

// a cubic Bezier curve given by its four control points
static void
test_bezier () {
  array<point> a;
  a << point (0.0, 0.0) << point (0.0, 1.0) << point (1.0, 1.0)
    << point (1.0, 0.0);
  curve c= bezier (a);
  CHECK_POINT (c (0.0), point (0.0, 0.0));
  CHECK_POINT (c (1.0), point (1.0, 0.0));
  CHECK_POINT (c (0.5), point (0.5, 0.75));
  bool error;
  CHECK_POINT (c->grad (1.0, error), point (0.0, -3.0));
  // a rectification stays within eps of the curve
  array<point> r= c->rectify (0.01);
  CHECK (N(r) > 2);
  CHECK (near_eq (r[0], c (0.0)) && near_eq (r[N(r)-1], c (1.0)));
}

static void
test_curve_operations () {
  point a (0.0, 0.0), b (4.0, 0.0), d (4.0, 4.0);
  curve s1= segment (a, b), s2= segment (b, d);
  curve inv= invert (s1);
  CHECK_POINT (inv (0.0), b);
  CHECK_POINT (inv (0.25), point (3.0, 0.0));
  curve p= part (s1, 0.25, 0.75);
  CHECK_POINT (p (0.0), point (1.0, 0.0));
  CHECK_POINT (p (1.0), point (3.0, 0.0));
  array<curve> cs;
  cs << s1 << s2;
  curve c= compound (cs);
  CHECK_EQ (c->nr_components (), 2);
  CHECK_POINT (c (0.25), point (2.0, 0.0));
  CHECK_POINT (c (0.75), point (4.0, 2.0));
  CHECK_POINT (c (1.0), d);
  // a frame maps a curve pointwise
  frame f= rotation_2D (point (0.0, 0.0), tm_PI / 2);
  curve fc= f (s1);
  CHECK_POINT (fc (0.5), point (0.0, 2.0));
  CHECK_POINT (f [fc] (0.5), point (2.0, 0.0));
}

// rectify_cumul adds the points of a curve except its starting point
static void
test_rectify_starting_point () {
  point a (0.0, 0.0), b (4.0, 0.0);
  CHECK_EQ (N(segment (a, b)->rectify (0.1)), 2);
  CHECK_EQ (N(part (segment (a, b), 0.0, 1.0)->rectify (0.1)), 2);
}

int
main () {
  RUN (test_known_values);
  RUN (test_inverse_transform);
  RUN (test_compose_invert);
  RUN (test_jacobian);
  RUN (test_jacobian_of_inverse);
  RUN (test_bounds);
  RUN (test_rectangles);
  RUN (test_segment);
  RUN (test_poly_segment);
  RUN (test_arc);
  RUN (test_bezier);
  RUN (test_curve_operations);
  RUN (test_rectify_starting_point);
  return test_report ();
}
