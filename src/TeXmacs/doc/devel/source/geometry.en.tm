<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Colors and geometric helpers>

  <section|Introduction>

  Several low level libraries in <source-link|src/src/Graphics/|src/Graphics> are shared by
  the typesetter, the renderers, the picture effects and the graphics
  editor:

  <\itemize>
    <item>the <em|color> library (<source-link|Graphics/Colors/|src/Graphics/Colors>), which packs
    colors into 32 bit words, resolves color names from several
    dictionaries, implements the reverse (dark) display mode and provides
    floating point colors for image processing;

    <item>the <em|geometric types> (<verbatim|Graphics/Types/>): points,
    frames (coordinate transformations), curves and grids, together with
    the numerical routines for splines, closest points and intersections;

    <item>a small generic <em|algebra> library
    (<source-link|Graphics/Mathematics/|src/Graphics/Mathematics>) with matrices, vectors, polynomials
    and a few experimental classes;

    <item>the experimental <em|three dimensional> objects
    (<source-link|Graphics/Spacial/|src/Graphics/Spacial>) behind the <markup|object-3d> family of
    tags.
  </itemize>

  This chapter describes the data structures and their numerical
  conventions. How the graphics editor and the typesetter of
  <markup|graphics> use frames and grids is explained in <hlink|the graphics
  editor: typesetting|graphics-editor-typeset.en.tm>; how colors end up on
  the screen or in a <abbr|PDF> file is explained in <hlink|the renderer
  interface|renderer.en.tm>; and the picture effects, which use the floating
  point colors of this chapter, are described in <hlink|raster pictures and
  effects|images-pictures.en.tm>.

  <section|Overview>

  <\verbatim-code>
    markup \ \ \ \ \ \ \ \ \ \ \ converted by \ \ \ \ result \ \ \ \ \ used by

    "red", "#ff000080" named_color \ \ \ \ color \ \ \ \ \ pencils, renderers

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (implicit) \ \ \ \ \ true_color \ effects, 3D light

    (point "1" "2") \ \ as_point \ \ \ \ \ \ \ point \ \ \ \ \ curves, frames

    gr-frame \ \ \ \ \ \ \ \ \ update_frame \ \ \ frame \ \ \ \ \ curve images

    (spline ...) \ \ \ \ \ typeset_spline \ curve \ \ \ \ \ curve_box, snapping

    gr-grid \ \ \ \ \ \ \ \ \ \ as_grid \ \ \ \ \ \ \ \ grid \ \ \ \ \ \ grid_box, snapping

    (object-3d ...) \ \ as_spacial \ \ \ \ \ spacial \ \ \ spacial_box
  </verbatim-code>

  All these types follow the usual <TeXmacs> conventions (see <hlink|basic
  data types|types.en.tm>): <cpp|point> is a plain
  <cpp|array\<less\>double\<gtr\>>, <cpp|matrix> and <cpp|polynomial> are
  concrete reference counted structures, and <cpp|frame>, <cpp|curve>,
  <cpp|grid> and <cpp|spacial> are abstract handles whose concrete classes
  are hidden in the <verbatim|.cpp> files and created by constructor
  functions such as <cpp|scaling>, <cpp|spline> or <cpp|cartesian>.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Graphics/Colors/colors.hpp|src/Graphics/Colors/colors.hpp>, <source-link|colors.cpp|src/Graphics/Colors/colors.cpp>>The
    <cpp|color> word: <cpp|rgb_color>, <cpp|get_rgb_color>,
    <cpp|cmyk_color>, <cpp|xpm_color>, <cpp|named_color>,
    <cpp|get_hex_color>, <cpp|blend_colors>, <cpp|reverse>, the global
    flags <cpp|true_colors> and <cpp|reverse_colors>.

    <item*|<source-link|tm_colors.hpp|src/Graphics/Colors/tm_colors.hpp>, <source-link|x11_colors.hpp|src/Graphics/Colors/x11_colors.hpp>,
    <source-link|svg_colors.hpp|src/Graphics/Colors/svg_colors.hpp>, <source-link|xc_colors.hpp|src/Graphics/Colors/xc_colors.hpp>,
    <source-link|dvips_colors.hpp|src/Graphics/Colors/dvips_colors.hpp>>The five color dictionaries, as static
    tables included only by <source-link|colors.cpp|src/Graphics/Colors/colors.cpp>.

    <item*|<source-link|Graphics/Colors/true_color.hpp|src/Graphics/Colors/true_color.hpp>,
    <source-link|true_color.cpp|src/Graphics/Colors/true_color.cpp>>Floating point <abbr|RGBA> colors with
    arithmetic, alpha composition and color transformations.

    <item*|<source-link|Graphics/Types/point.hpp|src/Graphics/Types/point.hpp>, <source-link|point.cpp|src/Graphics/Types/point.cpp>>Points,
    axes and elementary plane geometry.

    <item*|<source-link|Graphics/Types/frame.hpp|src/Graphics/Types/frame.hpp>,
    <source-link|frame.cpp|src/Graphics/Types/frame.cpp>>Frames: invertible coordinate transformations.

    <item*|<source-link|Graphics/Types/curve.hpp|src/Graphics/Types/curve.hpp>, <source-link|curve.cpp|src/Graphics/Types/curve.cpp>,
    <source-link|curve_extras.cpp|src/Graphics/Types/curve_extras.cpp>>Parameterized curves, rectification,
    closest points and intersections; polyline simplification, Bezier
    fitting and calligraphic strokes.

    <item*|<source-link|Graphics/Types/equations.hpp|src/Graphics/Types/equations.hpp>,
    <source-link|equations.cpp|src/Graphics/Types/equations.cpp>>Tridiagonal solvers used for interpolating
    splines.

    <item*|<source-link|Graphics/Types/grid.hpp|src/Graphics/Types/grid.hpp>, <source-link|grid.cpp|src/Graphics/Types/grid.cpp>>Grids
    (cartesian, polar, logarithmic).

    <item*|<source-link|Graphics/Types/math_util.hpp|src/Graphics/Types/math_util.hpp>>Numerical constants and
    helpers (<cpp|tm_infinity>, <cpp|tm_PI>, <cpp|square>, <cpp|fnull>,
    ...).

    <item*|<source-link|Graphics/Mathematics/|src/Graphics/Mathematics>>Generic templates:
    <source-link|matrix.hpp|src/Graphics/Mathematics/matrix.hpp>, <source-link|vector.hpp|src/Graphics/Mathematics/vector.hpp>,
    <source-link|polynomial.hpp|src/Graphics/Mathematics/polynomial.hpp>, <source-link|ball.hpp|src/Graphics/Mathematics/ball.hpp>,
    <source-link|function.hpp|src/Graphics/Mathematics/function.hpp>, <source-link|function_extra.hpp|src/Graphics/Mathematics/function_extra.hpp>, the operator
    and property traits <source-link|operators.hpp|src/Graphics/Mathematics/operators.hpp> and
    <source-link|properties.hpp|src/Graphics/Mathematics/properties.hpp>, symbolic trees <source-link|math_tree.hpp|src/Graphics/Mathematics/math_tree.hpp>,
    and the self test <source-link|test_math.cpp|src/Graphics/Mathematics/test_math.cpp>.

    <item*|<source-link|Graphics/Spacial/|src/Graphics/Spacial>>Triangulated three dimensional
    objects, their transformations and lighting.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Colors|geometry-colors.en.tm>

    <branch|Points, frames, curves and grids|geometry-curves.en.tm>

    <branch|The algebra library and three dimensional
    objects|geometry-algebra.en.tm>
  </traverse>

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
