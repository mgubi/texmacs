<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Graphics interface between C++ and <scheme>>

  <\warning>
    Most of this page describes the interface as it was designed in 2005.
    The routines <scm|editor-\<gtr\>graphics>, <scm|graphics-\<gtr\>editor>,
    <scm|grid-project>, <scm|grid-point-pertinence\<less\>?>,
    <scm|graphics-find-disk> and <scm|graphics-find-rectangle> do not exist
    in the current implementation. The actual interface is summarized in the
    last section of this page.
  </warning>

  <paragraph*|Rationale>

  <TeXmacs> both implements a low-level part of the graphics in C++ and the
  high-level user interface in <scheme>. This API describes how both parts
  interact.

  The low-level C++ mainly takes care of transforming the graphical markup in
  a typeset box. It also provides routines for translating between physical
  coordinates (relative to the window) into logical coordinates (the local
  coordinate system of the graphics) and routines for interacting with the
  typeset boxes (finding the closest objects to a given point or region or
  projecting a point on a grid).

  <paragraph*|Definitions>

  <\description>
    <item*|Editor coordinates>The coordinates of the outermost typeset box.
    Mouse events are typically passed in these coordinates. The corresponding
    data type is <verbatim|SI>.

    <item*|Graphics coordinates>The coordinates of the innermost graphics
    corresponding to the current cursor position.

    <item*|Grid>The current grid relative to the graphics for editing objects
    (this grid may theoretically be different from the grid which is
    displayed). The current grid consists both of a mathematical type of grid
    (no grid, cartesian grid, polar grid, etc.), together with special points
    which correspond either to control points, intersections of curves with
    the grid, intersections of curves, or self-intersections of curves.

    <item*|Grid point>A point on the grid is a triple <scm|(<scm-arg|p>
    <scm-arg|distance> <scm-arg|type>)>, where <scm-arg|p> is a point in
    graphics coordinates, <scm-arg|distance> its distance to the point which
    was projected on the grid (see <verbatim|grid-project> below) and
    <scm-arg|type> the type of grid point with a potential origin. For
    instance, <scm-arg|type> can be <verbatim|plain> or something like
    <verbatim|(control t)> for a control point corresponding to the tree
    <scm|t> in the document.
  </description>

  <paragraph*|Coordinate transformations>

  <\explain>
    <scm|(editor-\<gtr\>graphics <scm-arg|p>)><explain-synopsis|get graphics
    coordinates>
  <|explain>
    Transform a point <scm-arg|p> of the form <scm|(<scm-arg|x> <scm-arg|y>)>
    from the editor coordinates into the graphics coordinates.
  </explain>

  <\explain>
    <scm|(graphics-\<gtr\>editor p)><explain-synopsis|get editor coordinates>
  <|explain>
    Transform a point <scm-arg|p> of the form <scm|(<scm-arg|x> <scm-arg|y>)>
    from the graphics coordinates into the editor coordinates.
  </explain>

  <paragraph*|Grid routines>

  <\explain>
    <scm|(grid-project <scm-arg|p>)><explain-synopsis|project point on grid>
  <|explain>
    Given a point <scm-arg|p> (in graphics coordinates), find its projection
    on \ the current grid, the <scm-arg|distance> part of the projection
    being the distance between <scm-arg|p> and its projection.

    Note: the routine grid-project can also be used in order to find editable
    shapes and groups close to the current pointer position. Indeed, the
    corresponding control points are understood to lie on the grid in our
    sense.
  </explain>

  <\explain>
    <scm|(grid-point-pertinence\<less\>? <scm-arg|p> <scm-arg|q>)>

    <scm|(grid-point-pertinence\<less\>=? <scm-arg|p>
    <scm-arg|q>)><explain-synopsis|order by pertinence>
  <|explain>
    Grid points are ordered by pertinence as a function of type and distance.
    For instance, control points have higher pertinence than plain grid
    points and closer grid points are considered better than farther ones.
  </explain>

  <paragraph*|Selection of shapes>

  <\explain>
    <scm|(graphics-find-disk <scm-arg|p> <scm-arg|r>)><explain-synopsis|search
    shapes in disk>
  <|explain>
    Return the list of all trees in the graphics which intersect a disk with
    center <scm-arg|p> and radius <scm-arg|r> (in graphics coordinates).
  </explain>

  <\explain>
    <scm|(graphics-find-rectangle <scm-arg|p>
    <scm-arg|q>)><explain-synopsis|search shapes in rectangle>
  <|explain>
    Return the list of all trees in the graphics which intersect a rectangle
    with corners <scm-arg|p> and <scm-arg|q> (in graphics coordinates).
  </explain>

  <paragraph*|Computations with shapes>

  <\explain>
    <scm|(box-info <scm-arg|t> <scm-arg|what>)><explain-synopsis|get
    bounding box for a shape>
  <|explain>
    Get a bounding box (and other information) about a shape <scm-arg|t>.
    <scm-arg|t> can be a tree or a scheme tree, and <scm-arg|what> is a
    string of letters which specify the requested coordinates, like
    <scm|"lbrt"> (left, bottom, right and top of the logical box) or
    <scm|"LBRT"> (the same for the ink box). The routine is implemented in
    <verbatim|graphics-utils.scm> by typesetting the markup <markup|box-info>
    with <scm|texmacs-exec*>; the analogous routines <scm|frame-direct> and
    <scm|frame-inverse> transform points between the coordinates of the
    graphics and the coordinates of the typeset document.
  </explain>

  <\remark>
    This section might be extended, since a lot of the graphical intelligence
    is implemented in the C++ code. For instance, we might want to compute
    the intersections of two curves inside the Scheme code. Also, when we
    will allow for user macros, we might want routines which return the
    graphical expansion of the macro (the constituent elementary shapes, i.e.
    polylines, splines, etc.).
  </remark>

  <paragraph*|The current interface>

  In the current implementation, the <c++> editor
  (<verbatim|src/Edit/Interface/edit_graphics.cpp>) handles mouse events
  inside graphics as follows: it transforms the mouse position into
  graphics coordinates, projects it on the current grid (taking into account
  the control points of nearby objects), and then calls one of the
  <scheme> routines <scm|graphics-move>, <scm|graphics-release-left>,
  <scm|graphics-start-drag-left>, <scm|graphics-dragging-left>,
  <scm|graphics-end-drag-left>, <scm|graphics-release-right>, <abbr|etc.>
  with the resulting coordinates as strings. These routines are defined in
  <verbatim|progs/graphics/graphics-edit.scm> and dispatch on the current
  graphical mode. The main glued routines which may be used by these
  handlers are:

  <\explain>
    <scm|(get-graphical-x)>

    <scm|(get-graphical-y)>

    <scm|(get-graphical-pixel)><explain-synopsis|current position>
  <|explain>
    The coordinates of the last (adjusted) mouse position in graphics
    coordinates, and the size of a pixel in the same unit.
  </explain>

  <\explain>
    <scm|(graphical-select <scm-arg|x> <scm-arg|y>)>

    <scm|(graphical-select-area <scm-arg|x1> <scm-arg|y1> <scm-arg|x2>
    <scm-arg|y2>)><explain-synopsis|find objects>
  <|explain>
    Return the list of the objects close to a point <abbr|resp.> inside a
    rectangle, given in graphics coordinates, as a tree of lists of paths,
    ordered by pertinence.
  </explain>

  <\explain>
    <scm|(set-graphical-object <scm-arg|t>)>

    <scm|(get-graphical-object)><explain-synopsis|the temporary object>
  <|explain>
    Set <abbr|resp.> get the markup (typically the object under
    construction together with its control points) which is displayed on
    top of the graphics without being part of the document.
  </explain>

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>