<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The algebra library and three dimensional objects>

  <section|The algebra library>

  <source-link|Graphics/Mathematics/|src/Graphics/Mathematics> is a small header-only library of
  generic mathematical containers. Only matrices and polynomials are used by
  the rest of <TeXmacs>; the other classes are an experiment in generic
  numerical programming, exercised only by the self test
  <cpp|test_math> (<source-link|test_math.cpp|src/Graphics/Mathematics/test_math.cpp>), which is called from
  <cpp|texmacs_entrypoint> only when <verbatim|ENABLE_TESTS> is defined in
  <source-link|Texmacs/Texmacs/texmacs.cpp|src/Texmacs/Texmacs/texmacs.cpp> (it is commented out).

  <subsection|Matrices>

  <cpp|matrix\<less\>T\<gtr\>> (<source-link|matrix.hpp|src/Graphics/Mathematics/matrix.hpp>) is a concrete
  reference counted structure holding <cpp|NR (m)> rows and <cpp|NC (m)>
  columns in row major order; <cpp|m (i, j)> accesses an entry. The
  constructor <cpp|matrix\<less\>T\<gtr\> (c, rows, cols)> creates a
  matrix with <cpp|c> on the diagonal and zeros elsewhere, so
  <cpp|matrix\<less\>double\<gtr\> (1.0, 3, 3)> is the identity. The
  operations are:

  <\itemize>
    <item>componentwise <cpp|+>, <cpp|-> and unary operations
    (<cpp|unary> and <cpp|binary> with an operator class from
    <source-link|operators.hpp|src/Graphics/Mathematics/operators.hpp>);

    <item>the matrix product, and the product of a matrix with a
    <cpp|vector\<less\>T\<gtr\>> or an <cpp|array\<less\>T\<gtr\>> (both
    assert matching dimensions);

    <item><cpp|transpose>, <cpp|matrix_2D (a, b, c, d)>, and
    <cpp|invert>, which uses the closed formula for <math|2\<times\>2>
    matrices and Gauss-Jordan elimination with partial pivoting otherwise
    (asserting that the matrix is invertible);

    <item><cpp|projective_apply (m, v)>, which applies an
    <math|(n+1)\<times\>(n+1)> matrix to an <math|n>-dimensional point in
    homogeneous coordinates (or to an array of points);

    <item>conversion to and from trees: <cpp|as_tree> produces a tuple of
    row tuples, and <cpp|as_matrix\<less\>T\<gtr\>> parses one (asserting
    that it is a non empty tuple of tuples).
  </itemize>

  Matrices are used by the <cpp|linear_2D> and <cpp|affine_2D> frames
  (see <hlink|points, frames, curves and grids|geometry-curves.en.tm>), by
  the glyph transformations of <source-link|Graphics/Bitmap_fonts/glyph_transforms.cpp|src/Graphics/Bitmap_fonts/glyph_transforms.cpp>,
  by the <verbatim|(tuple "linear" ...)> transformations of
  <markup|gr-transform> and the three dimensional graphics in
  <source-link|Typeset/Concat/concat_graphics.cpp|src/Typeset/Concat/concat_graphics.cpp>, and by the (unused) least
  squares Bezier fitting of <source-link|Graphics/Types/curve_extras.cpp|src/Graphics/Types/curve_extras.cpp>.

  <subsection|Polynomials and vectors>

  <cpp|polynomial\<less\>T\<gtr\>> (<source-link|polynomial.hpp|src/Graphics/Mathematics/polynomial.hpp>) stores its
  coefficients by increasing degree. <cpp|p (x)> evaluates it with
  Horner's scheme and <cpp|p (x, k)> evaluates its <math|k>-th derivative;
  there are <cpp|+>, <cpp|->, <cpp|*>, <cpp|derive> and products with
  scalars and arrays. The spline curves of <source-link|Graphics/Types/curve.cpp|src/Graphics/Types/curve.cpp>
  represent each basis function piece as a <cpp|polynomial\<less\>double\<gtr\>>
  (<cpp|dpol>), and each piece of the curve as a vector of such
  polynomials (<cpp|dpols>), one per coordinate.

  <cpp|vector\<less\>T\<gtr\>> (<source-link|vector.hpp|src/Graphics/Mathematics/vector.hpp>) is a fixed size
  vector with componentwise arithmetic, elementary functions, scalar
  operations, <cpp|square_norm>, <cpp|norm> and <cpp|derive>. Note that the
  geometric <cpp|point> type is <em|not> a <cpp|vector> but an
  <cpp|array\<less\>double\<gtr\>>.

  <subsection|Experimental classes>

  <\description>
    <item*|<cpp|ball\<less\>C\<gtr\>>>(<source-link|ball.hpp|src/Graphics/Mathematics/ball.hpp>) Ball (interval)
    arithmetic: a center and a radius, with arithmetic and elementary
    functions which enclose the exact result.

    <item*|<cpp|function\<less\>F,T\<gtr\>>>(<source-link|function.hpp|src/Graphics/Mathematics/function.hpp>,
    <source-link|function_extra.hpp|src/Graphics/Mathematics/function_extra.hpp>) Abstract functions which can be evaluated
    at a point or on a ball, built from constants, coordinates, arithmetic
    and elementary functions, piecewise definitions
    (<cpp|pw_function>), vectors of functions and polynomials.

    <item*|Operators and properties>(<source-link|operators.hpp|src/Graphics/Mathematics/operators.hpp>,
    <source-link|properties.hpp|src/Graphics/Mathematics/properties.hpp>) Operator classes such as <cpp|add_op> or
    <cpp|neg_op>, with an <cpp|op> method and a <cpp|diff> method for the
    derivative, and traits classes giving the scalar, norm and product
    types of a type. The operator classes are also used by the raster
    pictures (<source-link|Graphics/Pictures/raster_operators.hpp|src/Graphics/Pictures/raster_operators.hpp>).

    <item*|Symbolic trees>(<source-link|math_tree.hpp|src/Graphics/Mathematics/math_tree.hpp>,
    <source-link|math_tree.cpp|src/Graphics/Mathematics/math_tree.cpp>) Arithmetic on trees (<cpp|add>, <cpp|mul>,
    <cpp|sqrt>, ...), which lets the generic code run on symbolic values,
    and <cpp|as_math_string> to print them.
  </description>

  <section|Three dimensional objects>

  <subsection|Markup>

  The experimental tags <markup|object-3d>, <markup|triangle-3d>,
  <markup|transform-3d> and <markup|light-3d> describe a scene made of
  colored triangles, which is drawn inside a <markup|graphics>. They are
  listed among the <hlink|graphical primitives|../format/regular/prim-graphics.en.tm>
  and declared in <source-link|Data/Drd/drd_std.cpp|src/Data/Drd/drd_std.cpp>:

  <\description>
    <item*|<markup|triangle-3d>>Three <markup|point>s with three
    coordinates and a color.

    <item*|<markup|object-3d>>A list of <markup|triangle-3d>s.

    <item*|<markup|transform-3d>>An object and a <math|4\<times\>4> matrix
    (a tuple of row tuples) acting on homogeneous coordinates.

    <item*|<markup|light-3d>>An object and a light, which is either
    <verbatim|(light-diffuse <em|point> <em|shadow-color> <em|light-color>)>
    or <verbatim|(light-specular <em|point> <em|point> <em|color>)>. A
    light position with four coordinates is taken in homogeneous
    coordinates.
  </description>

  <cpp|concater_rep::typeset_graphics_3d> (<source-link|concat_graphics.cpp|src/Typeset/Concat/concat_graphics.cpp>)
  evaluates the tag, converts it with <cpp|as_spacial> and composes the
  result with a <math|4\<times\>4> matrix built from the frame
  <cpp|env-\<gtr\>fr> of the picture and the variable
  <src-var|gr-transformation>; the <math|z> coordinate is kept for depth
  sorting. Invalid input gives the error <verbatim|"bad spacial object">.
  The result is a <cpp|spacial_box> (<source-link|Typeset/Boxes/Graphics/graphics_boxes.cpp|src/Typeset/Boxes/Graphics/graphics_boxes.cpp>),
  which draws itself by <cpp|renderer_rep::draw_spacial>.

  <subsection|Spacial objects>

  A <cpp|spacial> (<source-link|Graphics/Spacial/spacial.hpp|src/Graphics/Spacial/spacial.hpp>) is an abstract
  handle with the methods <cpp|get_extents>, <cpp|draw (renderer)>,
  <cpp|transform (matrix)> and <cpp|enlighten (light)>. There are three
  implementations:

  <\description-paragraphs>
    <item*|<cpp|triangulated (ts, cs)>>(<source-link|triangulated.cpp|src/Graphics/Spacial/triangulated.cpp>) An
    array of triangles with one color each. Before drawing, the triangles
    are sorted by increasing mean <math|z> coordinate and drawn in that
    order (a painter's algorithm, without splitting of intersecting
    triangles); each is drawn with
    <cpp|renderer_rep::draw_triangle>. <cpp|transform> applies
    <cpp|projective_apply> to all vertices; <cpp|enlighten> recomputes the
    colors: a diffuse light mixes the shadow and light colors according to
    the angle between the normal of the triangle and the direction of the
    light, a specular light adds a highlight with an eighth power falloff;
    the computations use <cpp|true_color> and <cpp|source_over> (see
    <hlink|colors|geometry-colors.en.tm>).

    <item*|<cpp|transformed (obj, m)>, <cpp|enlightened (obj,
    light)>>(<source-link|transformed.cpp|src/Graphics/Spacial/transformed.cpp>, <source-link|enlightened.cpp|src/Graphics/Spacial/enlightened.cpp>) Lazy
    wrappers which apply the transformation or the light on first use.
  </description-paragraphs>

  The renderers do not know about three dimensional objects: the generic
  <cpp|renderer_rep::draw_spacial> just calls <cpp|obj-\<gtr\>draw>, which
  ends up in ordinary filled triangles.

  <section|Pitfalls>

  <\itemize>
    <item>The products of a matrix with a <cpp|vector> or an <cpp|array>
    initialize the result with a loop over the number of <em|columns>
    instead of rows (<verbatim|Graphics/Mathematics/matrix.hpp:200-201,
    213-214>). For a non square matrix with more rows than columns, the
    extra entries start from uninitialized values; with fewer rows, the
    loop writes past the end of the result. All current callers use square
    matrices.

    <item>The diagonal constructor sets the entries whose linear index is a
    multiple of <math|cols+1> (<source-link|matrix.hpp:54|src/Graphics/Mathematics/matrix.hpp:54>), which also hits
    off-diagonal entries when a matrix has more than <math|cols+1> rows.

    <item><cpp|invert> asserts invertibility for large matrices but divides
    by a zero determinant without warning for <math|2\<times\>2> matrices
    (<source-link|matrix.hpp:249|src/Graphics/Mathematics/matrix.hpp:249>); a singular <verbatim|(tuple "linear" ...)>
    transformation therefore produces infinite coordinates.

    <item>The depth sorting of <cpp|triangulated> does not resolve
    intersecting or cyclically overlapping triangles,. Lights are evaluated in the coordinates of the
    <markup|light-3d> tag, before the transformation into the picture.
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
