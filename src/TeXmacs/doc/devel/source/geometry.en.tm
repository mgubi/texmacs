<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Colors and geometric helpers>

  <section|Introduction>

  Several low level libraries in <verbatim|src/src/Graphics/> are shared by
  the typesetter, the renderers, the picture effects and the graphics
  editor:

  <\itemize>
    <item>the <em|color> library (<verbatim|Graphics/Colors/>), which packs
    colors into 32 bit words, resolves color names from several
    dictionaries, implements the reverse (dark) display mode and provides
    floating point colors for image processing;

    <item>the <em|geometric types> (<verbatim|Graphics/Types/>): points,
    frames (coordinate transformations), curves and grids, together with
    the numerical routines for splines, closest points and intersections;

    <item>a small generic <em|algebra> library
    (<verbatim|Graphics/Mathematics/>) with matrices, vectors, polynomials
    and a few experimental classes;

    <item>the experimental <em|three dimensional> objects
    (<verbatim|Graphics/Spacial/>) behind the <markup|object-3d> family of
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
    <item*|<verbatim|Graphics/Colors/colors.hpp>, <verbatim|colors.cpp>>The
    <cpp|color> word: <cpp|rgb_color>, <cpp|get_rgb_color>,
    <cpp|cmyk_color>, <cpp|xpm_color>, <cpp|named_color>,
    <cpp|get_hex_color>, <cpp|blend_colors>, <cpp|reverse>, the global
    flags <cpp|true_colors> and <cpp|reverse_colors>.

    <item*|<verbatim|tm_colors.hpp>, <verbatim|x11_colors.hpp>,
    <verbatim|svg_colors.hpp>, <verbatim|xc_colors.hpp>,
    <verbatim|dvips_colors.hpp>>The five color dictionaries, as static
    tables included only by <verbatim|colors.cpp>.

    <item*|<verbatim|Graphics/Colors/true_color.hpp>,
    <verbatim|true_color.cpp>>Floating point <abbr|RGBA> colors with
    arithmetic, alpha composition and color transformations.

    <item*|<verbatim|Graphics/Types/point.hpp>, <verbatim|point.cpp>>Points,
    axes and elementary plane geometry.

    <item*|<verbatim|Graphics/Types/frame.hpp>,
    <verbatim|frame.cpp>>Frames: invertible coordinate transformations.

    <item*|<verbatim|Graphics/Types/curve.hpp>, <verbatim|curve.cpp>,
    <verbatim|curve_extras.cpp>>Parameterized curves, rectification,
    closest points and intersections; polyline simplification, Bezier
    fitting and calligraphic strokes.

    <item*|<verbatim|Graphics/Types/equations.hpp>,
    <verbatim|equations.cpp>>Tridiagonal solvers used for interpolating
    splines.

    <item*|<verbatim|Graphics/Types/grid.hpp>, <verbatim|grid.cpp>>Grids
    (cartesian, polar, logarithmic).

    <item*|<verbatim|Graphics/Types/math_util.hpp>>Numerical constants and
    helpers (<cpp|tm_infinity>, <cpp|tm_PI>, <cpp|square>, <cpp|fnull>,
    ...).

    <item*|<verbatim|Graphics/Mathematics/>>Generic templates:
    <verbatim|matrix.hpp>, <verbatim|vector.hpp>,
    <verbatim|polynomial.hpp>, <verbatim|ball.hpp>,
    <verbatim|function.hpp>, <verbatim|function_extra.hpp>, the operator
    and property traits <verbatim|operators.hpp> and
    <verbatim|properties.hpp>, symbolic trees <verbatim|math_tree.hpp>,
    and the self test <verbatim|test_math.cpp>.

    <item*|<verbatim|Graphics/Spacial/>>Triangulated three dimensional
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
