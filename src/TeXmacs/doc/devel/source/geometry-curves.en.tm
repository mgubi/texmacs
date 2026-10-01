<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Points, frames, curves and grids>

  This page describes the classes of <verbatim|Graphics/Types/>. Their use
  by the typesetter of <markup|graphics> (the frame
  <cpp|edit_env_rep::fr>, the grids <src-var|gr-grid> and
  <src-var|gr-edit-grid>, the <cpp|typeset_*> routines for curves) is
  explained in <hlink|the graphics editor:
  typesetting|graphics-editor-typeset.en.tm>, and is not repeated here.

  <section|Points and axes>

  A <cpp|point> is an <cpp|array\<less\>double\<gtr\>>
  (<verbatim|Graphics/Types/point.hpp>). Points usually have two
  coordinates, three for the <hlink|three dimensional
  objects|geometry-algebra.en.tm>; the empty array serves as an
  <em|invalid> point, which is what <cpp|as_point> returns for a tree that
  is neither a <markup|point> nor a <markup|tuple>.

  <paragraph|Arithmetic.>The operators <cpp|+>, <cpp|->, <cpp|*> and
  <cpp|/> between points work componentwise on the <em|common> length of
  their operands: adding a two dimensional and a three dimensional point
  silently gives a two dimensional result, and any operation with an
  invalid point gives an invalid point. Scalar multiplication and division,
  <cpp|abs>, <cpp|min> and <cpp|max> (the last two assert a non empty
  point) are also provided. Equality <cpp|==> compares with a tolerance of
  <math|10<rsup|-6>>.

  <paragraph|Geometry.><cpp|inner>, <cpp|norm>, <cpp|arg> (the angle in
  <math|[0,2\<pi\>)>), <cpp|rotate_2D (p, o, angle)>, <cpp|slanted (p,
  slant)>, <cpp|collinear>, <cpp|linearly_dependent> (three points),
  <cpp|orthogonalize> (an orthonormal basis of the plane through three
  points, which works in any dimension) and <cpp|inside_rectangle>. An
  <cpp|axis> is a pair of points describing a line; <cpp|proj>,
  <cpp|dist>, <cpp|seg_dist> (distance to the segment), <cpp|midperp>
  (perpendicular bisector of the first two points, in the plane of the
  three points) and <cpp|intersection (axis, axis)> operate on them.
  Degenerate cases return invalid points rather than failing.

  <verbatim|Graphics/Types/math_util.hpp> defines the constants
  <cpp|tm_infinity> (the largest <cpp|float>), <cpp|tm_PI> and <cpp|tm_E>,
  and helpers <cpp|square>, <cpp|norm (double)>, <cpp|nearest>,
  <cpp|sign>, <cpp|fnull (x, eps)> and an integer power <cpp|pow (double,
  int)>.

  <section|Frames>

  <subsection|The abstract interface>

  A <cpp|frame> (<verbatim|Graphics/Types/frame.hpp>) is an invertible map
  from <em|local> coordinates to the coordinates of a <em|parent> system.
  <cpp|f (p)> applies the direct map and <cpp|f [p]> the inverse one; both
  are also defined on <cpp|rectangle>s (<cpp|enclose> transforms the four
  edges, sampled at 20 points each unless the frame is linear, and returns
  the bounding box) and on curves (<cpp|f (c)> is a transformed curve, see
  below). Besides <cpp|direct_transform> and <cpp|inverse_transform>, a
  <cpp|frame_rep> implements

  <\description>
    <item*|<cpp|jacobian (p, v, error)>,
    <cpp|jacobian_of_inverse>>The derivative at <cpp|p> applied to the
    vector <cpp|v>, used to compute tangents of transformed curves.

    <item*|<cpp|direct_bound (p, eps)>, <cpp|inverse_bound>>A <math|\<delta\>>
    such that points within <math|\<delta\>> of <cpp|p> are mapped within
    <cpp|eps> of the image of <cpp|p>; used to choose the precision with
    which a curve must be rectified before it is transformed.

    <item*|<cpp|direct_scalar (x)>, <cpp|inverse_scalar>>The length of the
    image of a horizontal vector of length <math|x>. The typesetter uses
    <cpp|inverse_scalar> to convert pixel sizes and pen widths into
    graphical units (<verbatim|Typeset/Concat/concat_graphics.cpp>,
    <verbatim|Edit/Interface/edit_graphics.cpp>). The header itself notes
    that this is \Perror-prone\Q: it is only meaningful for conformal
    maps.

    <item*|<cpp|linear>>A flag which is set when the frame maps segments to
    segments (this includes the affine frames). Only for such frames can a
    transformed curve be rectified, and <cpp|enclose> uses the corners
    alone.

    <item*|<cpp|operator tree>>A description for debugging, such as
    <verbatim|(tuple "scale" ...)>.
  </description>

  <subsection|Concrete frames>

  <\description>
    <item*|<cpp|shift_2D (d)>>Translation.

    <item*|<cpp|scaling (m, shift)>>The map <math|p\<mapsto\>shift+m p>,
    with a scalar or a point (one factor per axis) as <math|m>. This is the
    frame of every picture: <cpp|update_frame> builds it from
    <src-var|gr-frame>.

    <item*|<cpp|rotation_2D (center, angle)>, <cpp|slanting (center,
    slant)>>Rotation (angle in radians) and horizontal slanting; the latter
    is used by the poor man's italic fonts
    (<verbatim|Graphics/Fonts/poor_italic.cpp>).

    <item*|<cpp|linear_2D (m)>>A linear map given by a <math|2\<times\>2>
    matrix, inverted once at construction. The transformations of the form
    <verbatim|(tuple "linear" a b c d)> given to <markup|gr-transform>
    (<cpp|get_transformation> in <verbatim|concat_graphics.cpp>) and the
    glyph transformations of <verbatim|Graphics/Bitmap_fonts/glyph_transforms.cpp>
    produce such frames.

    <item*|<cpp|affine_2D (m)>>An affine map given by a <math|3\<times\>3>
    matrix in homogeneous coordinates. Only the direct map is implemented.

    <item*|<cpp|bend_frame (fun, ...)>>A vertical bending
    <math|(x,y)\<mapsto\>(x,y+fun(x))>, optionally conjugated by a
    rectangle. It is not linear and has no Jacobian; nothing uses it.

    <item*|<cpp|f1 * f2>, <cpp|invert (f)>>Composition (apply <cpp|f2>
    first) and inversion; both are lazy wrappers.
  </description>

  <section|Curves>

  <subsection|The abstract interface>

  A <cpp|curve> (<verbatim|Graphics/Types/curve.hpp>) is a map from
  <math|[0,1]> to points: <cpp|c (t)> evaluates it. The virtual methods of
  <cpp|curve_rep> are

  <\description>
    <item*|<cpp|nr_components ()>>The number of pieces, used to choose
    step sizes and to parameterize concatenations.

    <item*|<cpp|rectify_cumul (a, eps)>>Appends to <cpp|a> a polyline
    approximating the curve within <cpp|eps> (without the starting point);
    <cpp|rectify (eps)> returns the full polyline. This is how curves are
    drawn: <cpp|curve_box> rectifies its curve with a precision of a quarter
    of <cpp|PIXEL> (<verbatim|Typeset/Boxes/Graphics/graphics_boxes.cpp>).

    <item*|<cpp|bound (t, eps)>>A parameter distance within which the curve
    moves by less than <cpp|eps>. The default implementation divides
    <cpp|eps> by the norm of the gradient and halves the result until the
    neighbouring points are close enough.

    <item*|<cpp|grad (t, error)>>The derivative.

    <item*|<cpp|curvature (t1, t2)>>A bound on the curvature between two
    parameters (several implementations return <cpp|tm_infinity>).

    <item*|<cpp|get_control_points (abs, pts, cip)>>The parameters, points
    and source paths of the control points. The paths connect the curve to
    the <markup|point> trees of the document and are what allows the
    graphics editor to select and drag control points.

    <item*|<cpp|find_closest_points (t1, t2, p, eps)>,
    <cpp|find_closest_point>>Local minima of the distance to <cpp|p>,
    found by walking along the curve with steps given by <cpp|bound>, and
    sorted by distance.
  </description>

  <subsection|Concrete curves>

  <\description>
    <item*|<cpp|segment (p1, p2)>, <cpp|poly_segment (a, cip)>>A segment
    and a polyline; the polyline has <math|N(a)-1> components with equal
    parameter intervals.

    <item*|<cpp|spline (a, cip, close, interpol)>>A quadratic B-spline.
    With <cpp|interpol> (the default) the control points are first computed
    by solving a tridiagonal (or, for closed splines, cyclic tridiagonal)
    system so that the curve passes through the given points; the solvers
    are in <verbatim|Graphics/Types/equations.cpp> (<cpp|tridiag_solve>,
    <cpp|xtridiag_solve>, <cpp|quasitridiag_solve>). The
    <markup|spline> and <markup|cspline> tags produce such curves.

    <item*|<cpp|bezier (a)>, <cpp|poly_bezier (a, cip, simple,
    closed)>>A cubic Bezier curve given by exactly four points, and a
    sequence of them. In <em|simple> mode (the <markup|smooth> and
    <markup|csmooth> tags), the intermediate control points are computed
    from the given points; otherwise (<markup|bezier>, <markup|cbezier>)
    every third point is on the curve and the others are control points.

    <item*|<cpp|arc (a, cip, close)>>The circular arc through three points
    (a full circle if <cpp|close>); degenerate input gives a single point.

    <item*|<cpp|compound (cs)>>Concatenation (continuity at the junctions
    is not checked).

    <item*|<cpp|invert (c)>, <cpp|part (c, t0, t1)>, <cpp|truncate (c,
    portion, eps)>>Reversed parameterization, a portion of a curve, and the initial
    part of a curve covering a given portion of its length. <cpp|part> is used to cut
    curves at the white zones around arrow heads and dots
    (<cpp|adjust_extremities> in <verbatim|concat_graphics.cpp>).

    <item*|<cpp|f (c)>>The image of a curve by a frame. Its
    <cpp|rectify_cumul> rectifies the original curve with precision
    <cpp|f-\<gtr\>direct_bound (c(0), eps)> and maps the points; this only
    works for linear frames.

    <item*|<cpp|recontrol (c, a, cip)>>The same curve with other control
    points; used for calligraphic strokes, whose shape is computed but
    whose control points are those of the source markup.
  </description>

  <subsection|Closest points and intersections>

  <cpp|closest (c, p)> refines <cpp|find_closest_point> up to ten times and
  returns the closest point on the curve; the graphics editor uses it to
  snap to curves (<verbatim|Edit/Interface/edit_graphics.cpp>,
  <verbatim|Typeset/Boxes/Modifier/change_boxes.cpp>).
  <cpp|intersection (f, g, p0, eps)> looks for an intersection of two
  (two dimensional) curves near <cpp|p0>: it starts from the closest points
  of both curves to <cpp|p0> and runs a Newton iteration on the parameters,
  stopping when the distance no longer decreases by at least ten percent.
  For <cpp|f == g> it looks for a self intersection between the two closest
  points. The editor uses it to snap to the intersections of the curves found
  near the mouse (<cpp|snap_to_guide>).

  <subsection|Polyline helpers>

  <verbatim|Graphics/Types/curve_extras.cpp> contains routines on arrays of
  points, all used by the <markup|calligraphy> tag
  (<cpp|typeset_calligraphy> in <verbatim|concat_graphics.cpp>):
  <cpp|refine> and <cpp|smoothen> (subdivision and moving average),
  <cpp|bezier_fit> (a piecewise cubic approximation within a tolerance),
  <cpp|rectify_bezier>, <cpp|oval_profile> (the outline of an elliptic pen)
  and <cpp|calligraphy> (the outline swept by such a pen along a
  polyline), and <cpp|simplify_polyline> (removal of points within a
  tolerance). Two least squares fitting methods, <cpp|std_bezier_fit> and
  <cpp|alt_bezier_fit>, are present but unused.

  <section|Grids>

  A <cpp|grid> (<verbatim|Graphics/Types/grid.hpp>) produces the curves to
  be drawn within given limits (<cpp|get_curves>), the curves near a point
  (<cpp|get_curves_around>) and the closest grid point
  (<cpp|find_closest_point>, <cpp|find_point_around>). The concrete grids
  are <cpp|empty_grid>, <cpp|cartesian>, <cpp|polar> and
  <cpp|logarithmic>; <cpp|as_grid> and <cpp|as_tree> convert from and to
  the tuples stored in <src-var|gr-grid>. Their role in drawing and snapping
  is described in <hlink|the graphics editor:
  typesetting|graphics-editor-typeset.en.tm>.

  <section|Pitfalls>

  <\itemize>
    <item>The Jacobian of the inverse of a slanting uses <cpp|+slant>
    instead of <cpp|-slant> (<verbatim|Graphics/Types/frame.cpp:172>), so
    tangents of curves mapped by an inverted slanting are wrong.

    <item>The bounds of <cpp|shift_2D>, <cpp|rotation_2D>,
    <cpp|linear_2D> and <cpp|affine_2D> return <cpp|eps> unchanged. This is
    right for isometries, but for a <cpp|linear_2D> or <cpp|affine_2D> map
    which enlarges distances, a transformed curve is rectified too coarsely
    by the factor of enlargement. The bound of <cpp|scaling> with a
    negative factor is negative.

    <item>Several operations abort with <verbatim|"not yet implemented">:
    the inverse of an <cpp|affine_2D> frame and the Jacobians of
    <cpp|bend_frame>, of the inverse of an affine frame and of the inverse
    of a composed frame (<verbatim|frame.cpp:227-235, 259-265, 302-304>),
    and the rectification of a curve transformed by a non linear frame and
    the curvature of any transformed curve (<verbatim|curve.cpp:1165,
    1180>).

    <item>The Jacobian of <cpp|affine_2D> multiplies the <math|3\<times\>3>
    matrix with a two dimensional vector, which fails the dimension check
    of the matrix product (<verbatim|frame.cpp:216, 231>). No code calls
    <cpp|affine_2D> at present.

    <item><cpp|bezier (a)> reads four points without checking that
    <cpp|a> has them (<verbatim|curve.cpp:691-696>).

    <item>A closed <cpp|spline> appends two points to the array it was
    given, which is shared with the caller (<verbatim|curve.cpp:392>).

    <item><cpp|arg (p)> divides by the norm and is undefined for the zero
    vector; <cpp|pow (double, int)> returns 1 for negative exponents.
  </itemize>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
