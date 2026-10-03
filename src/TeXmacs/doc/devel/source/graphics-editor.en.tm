<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The graphics editor>

  <section|Introduction>

  <TeXmacs> pictures are ordinary document trees. A picture is a
  <markup|graphics> tag whose children are graphical objects such as
  <markup|point>, <markup|line>, <markup|spline>, <markup|arc> or
  <markup|text-at>, usually wrapped in <markup|with> tags which set their
  color, line width and other attributes. The graphics editor is the part
  of <TeXmacs> which lets the user draw and modify such trees with the
  mouse.

  The implementation is split over three layers, which communicate through
  a small number of glued functions:

  <\enumerate>
    <item><em|Typesetting.> The typesetter turns the graphical markup into
    boxes (<cpp|graphics_box>, <cpp|curve_box>, <cpp|point_box>,
    <cpp|grid_box>, <cpp|text_at_box>, ...). These boxes know their
    coordinate frame and grid, and they implement
    <cpp|graphical_select>, which finds the control points, curve points and
    text borders near a given position. This is the only place where the
    geometry of the objects is computed.

    <item><em|C++ editor hooks.> The class <cpp|edit_graphics_rep>
    (<verbatim|Edit/Interface/edit_graphics.cpp>) is a component of the
    editor. It decides whether the cursor and the mouse are inside a
    picture, converts mouse positions from screen coordinates to graphical
    coordinates, snaps them to points, curves and grids, forwards the mouse
    events to <scheme>, and draws the temporary <em|graphical object>: an
    overlay which shows control points, selections and the object being
    created.

    <item><em|The <scheme> state machine.> All actual editing decisions are
    taken in <scheme>, in the modules under
    <verbatim|src/TeXmacs/progs/graphics/>. These implement the editing
    modes (point mode, group mode, hand drawing), a state machine which
    remembers what the user is doing (creating a curve, dragging a point,
    moving a group of objects), the <em|sketch> which holds the objects
    being edited, the menus, toolbars and keyboard shortcuts, and the
    management of the default properties of new objects.
  </enumerate>

  The user level description of the graphics editor is in the manual
  chapter <hlink|creating technical pictures|../../main/graphics/man-graphics.en.tm>.
  The graphical markup is documented in <hlink|graphics
  primitives|../format/regular/prim-graphics.en.tm> and the graphical
  environment variables in <hlink|graphics environment
  variables|../format/environment/env-graphics.en.tm>. The pages under
  <hlink|Scheme interface for the graphical
  mode|../scheme/graphics/scheme-graphics.en.tm> describe a design of the
  <scheme> interface from 2005; they are only of historical interest, since
  most of the routines described there do not exist anymore. The present
  chapter describes the implementation as it is in the source code.

  All C++ file names below are relative to <verbatim|src/src/>, and all
  <scheme> file names are relative to <verbatim|src/TeXmacs/progs/>.

  <section|Source map>

  <\description-paragraphs>
    <item*|<verbatim|Graphics/Types/>>Geometric types:
    <cpp|point> (an <cpp|array\<less\>double\<gtr\>>, in
    <verbatim|point.hpp>), coordinate transformations <cpp|frame>
    (<verbatim|frame.hpp>), curves <cpp|curve> (<verbatim|curve.hpp>,
    <verbatim|curve.cpp>, <verbatim|curve_extras.cpp> for the hand drawing
    algorithms), and grids <cpp|grid> (<verbatim|grid.hpp>).

    <item*|<verbatim|Graphics/Spacial/>>Experimental three dimensional
    objects (<cpp|spacial>): triangulated surfaces, their transformations
    and their lighting.

    <item*|<verbatim|Typeset/Concat/concat_graphics.cpp>>Typesetting of all
    graphical primitives (<cpp|concater_rep::typeset_graphics>,
    <cpp|typeset_line>, <cpp|typeset_text_at>, ...), and the support for
    graphical constraints (<cpp|set_graphical_value> and friends).

    <item*|<verbatim|Typeset/Boxes/Graphics/>>The graphics specific boxes:
    <verbatim|graphics_boxes.cpp> (<cpp|graphics_box>,
    <cpp|graphics_group_box>, <cpp|point_box>, <cpp|curve_box>,
    <cpp|spacial_box>) and <verbatim|grid_boxes.cpp> (<cpp|grid_box>).
    The <cpp|text_at_box> lives in
    <verbatim|Typeset/Boxes/Modifier/change_boxes.cpp>. The constructors are
    declared in <verbatim|Typeset/Boxes/graphics.hpp>.

    <item*|<verbatim|Typeset/boxes.hpp>, <verbatim|Typeset/Boxes/Basic/boxes.cpp>>The
    graphical selection type <cpp|gr_selection>, and the default
    implementations of <cpp|box_rep::find_frame>, <cpp|find_grid>,
    <cpp|find_limits> and <cpp|graphical_select>.

    <item*|<verbatim|Typeset/Env/env_semantics.cpp>>Computation of the
    current frame <cpp|edit_env_rep::fr> and clipping limits from the
    variables <src-var|gr-frame> and <src-var|gr-geometry>
    (<cpp|update_frame>, <cpp|update_geometry>) and of the cached graphical
    attributes (point style, arrows, text alignment, ...).

    <item*|<verbatim|Edit/Interface/edit_graphics.hpp>,
    <verbatim|edit_graphics.cpp>>The editor component
    <cpp|edit_graphics_rep>.

    <item*|<verbatim|Edit/Interface/edit_mouse.cpp>,
    <verbatim|edit_repaint.cpp>, <verbatim|edit_interface.cpp>>The places
    where the generic editor calls the graphics component: mouse dispatch,
    drawing of the overlay and of the graphical cursor, and the transfer of
    the snapping parameters.

    <item*|<verbatim|graphics/graphics-drd.scm>>Groups of graphical tags,
    the table of graphical attributes and their defaults.

    <item*|<verbatim|graphics/graphics-utils.scm>>Access to the innermost
    <markup|graphics>, its properties and the objects inside it; insertion of
    new objects; <scm|make-graphics>.

    <item*|<verbatim|graphics/graphics-env.scm>>The state
    <scm|graphics-state> of the editor, the state stack, the filtering of
    graphical selections and <scm|graphics-reset-context>.

    <item*|<verbatim|graphics/graphics-object.scm>>Construction of the
    graphical object (the overlay) and management of the sketch.

    <item*|<verbatim|graphics/graphics-edit.scm>>Entry points for the mouse
    events coming from C++, and z-ordering.

    <item*|<verbatim|graphics/graphics-single.scm>>Point mode: creation
    and modification of single objects, and hand drawing.

    <item*|<verbatim|graphics/graphics-group.scm>>Group mode: selection,
    moving, resizing, rotating, grouping, properties and the clipboard.

    <item*|<verbatim|graphics/graphics-main.scm>>Global properties of a
    picture: extents, frame, zoom, grids, editing mode, default properties
    of new objects and snapping options.

    <item*|<verbatim|graphics/graphics-markup.scm>>User defined graphical
    macros (<scm|define-graphics>).

    <item*|<verbatim|graphics/graphics-animate.scm>>Editing of animated
    pictures.

    <item*|<verbatim|graphics/graphics-kbd.scm>,
    <verbatim|graphics/graphics-menu.scm>>Keyboard shortcuts, menus and
    toolbars.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Graphical markup and its typesetting|graphics-editor-typeset.en.tm>

    <branch|The C++ side of the graphics editor|graphics-editor-kernel.en.tm>

    <branch|The <scheme> state machine|graphics-editor-scheme.en.tm>

    <branch|Extending the graphics editor and
    pitfalls|graphics-editor-extend.en.tm>
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
