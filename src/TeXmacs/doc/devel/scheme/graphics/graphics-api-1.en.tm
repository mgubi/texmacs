<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Low level graphics manipulation>

  <\warning>
    This page describes the design of the low-level graphics library as it
    was planned in 2005. Only part of it has been implemented, sometimes
    under different names; the entries below indicate the current status.
    The actual implementation of the graphical editor can be found in
    <verbatim|progs/graphics/>, mainly in <source-link|graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm>
    (enhanced trees), <source-link|graphics-object.scm|TeXmacs/progs/graphics/graphics-object.scm> (the sketch and the
    current object) and <source-link|graphics-edit.scm|TeXmacs/progs/graphics/graphics-edit.scm> (mouse handlers).
  </warning>

  <paragraph*|Rationale>

  <TeXmacs> provides a small low-level library for the manipulation of
  graphics on top of the usual tree interface. One particularity of graphics
  operations is that they usually concern a large number of continuous
  changes (as a function of mouse movement) to one or more objects (under
  construction or being edited). On the one hand, this means that not all
  movements have to be undoable. On the other hand, this implies that some
  optimizations may be necessary to obtain a reasonable speed.

  For these reasons, the library allows the programmer to focus attention on
  one or several objects in a graphics and to quickly perform operations on
  these objects. Focus is mostly understood to be temporary: typically, the
  focus is released as soon as an operation has been completed, i.e. the
  construction of a polyline.

  From the implementation point of view, the selected objects may either be
  removed from the document (current implementation), or kept in the document
  (future implementation), while displaying them on top of the other objects
  (if necessary).

  <paragraph*|Definitions>

  <\description>
    <item*|Tree>As in the main tree API. There are three main types of trees
    with graphical markup: graphics, shapes and groups.

    <item*|Enhanced tree>Trees with graphical markup can be enhanced to
    provide additional properties for the markup by means of with tags. For
    instance, an "enhanced shape" (see below) might be a polyline together
    with a particular color and line width.

    <item*|Radical and properties>In the case of an enhanced tree of the form
    <scm|(with <scm-args|props> <scm-arg|object>)>, <scm-arg|object> is
    called the radical of the enhanced tree and <scm-args|props> the
    properties of the enhanced tree. Notice that an enhanced tree is allowed
    to be reduced to its radical, in which case it has no properties.

    <item*|Graphics>This term corresponds to the main graphics, which is an
    ordered list of enhanced shapes or groups. Enhancements for the main
    graphics can be divided in two categories:

    <\itemize>
      <item>Global properties for the graphics itself, e.g. rendering
      properties, or a background grid.

      <item>Editing properties, which control the current editing behaviour
      of the graphics (polyline mode, current pen colour, etc.).
    </itemize>

    <item*|Shape>A shape is an atomic graphical markup primitive, such as a
    polyline. Typical enhancements for shapes are pen color, fill color, line
    width, arrow mode, etc.

    <item*|Group>A group is an ordered list of enhanced shapes or groups. The
    possible enhancements for groups are the same as the ones for shapes
    (and, in this respect, groups therefore differ from graphics).

    <item*|Sketch> The current sketch corresponds to a single or ordered list
    of enhanced shapes or groups on which the graphical editor is currently
    operating. There are two main modes for the sketch:

    <\description>
      <item*|SELECTING>the sketch corresponds to a selection of enhanced
      shapes or groups in the main document.

      <item*|MODIFYING>the sketch corresponds to a single or ordered list of
      enhanced shapes or groups which are being constructed or modified. The
      trees in the sketch can be new trees or trees which correspond to
      marked (invisible) trees in the main document.
    </description>

    The current sketch is usually displayed on top of all other graphics,
    together with several control points.
  </description>

  <paragraph*|Manipulation of enhanced trees>

  <\explain>
    <scm|(enhanced-tree-\<gtr\>radical <scm-arg|t>)><explain-synopsis|get
    radical>
  <|explain>
    \ Given an enhanced tree <scm-arg|t>, return its radical. In the current
    implementation, the enhancements are <markup|with> and
    <markup|anim-edit> tags (see <scm|enhanced-tree?> in
    <source-link|graphics-utils.scm|TeXmacs/progs/graphics/graphics-utils.scm>); <scm|stree-radical> does the same for
    <scheme> trees.
  </explain>

  <\explain>
    <scm|(radical-\<gtr\>enhanced-tree <scm-arg|t>)><explain-synopsis|get
    enhanced tree>
  <|explain>
    Given a radical <scm-arg|t>, find its parent which corresponds to its
    largest enhancement. If <scm-arg|t> is not enhanced, then it is
    returned unchanged. The tree <scm-arg|t> must belong to a <TeXmacs>
    document.
  </explain>

  <\explain>
    <scm|(enhanced-tree-set! <scm-arg|t> <scm-args|p> <scm-arg|u>)>

    <scm|(enhanced-tree-ref <scm-arg|t> <scm-args|p>)>

    <scm|(enhanced-tree-arity <scm-arg|t>
    <scm-args|p>)><explain-synopsis|analogue of basic tree API>
  <|explain>
    These routines are similar to <scm|tree-set!>, <scm|tree-ref>, etc.
    except that they operate on the radical of the enhanced tree. <em|Not
    implemented.>
  </explain>

  <\explain>
    <scm|(enhanced-tree-properties-set! <scm-arg|t>
    <scm-arg|l>)><explain-synopsis|set properties>
  <|explain>
    Given an enhanced tree <scm-arg|t>, override its properties with the
    elements in the association list <scm-arg|l>. <em|Not implemented.>
  </explain>

  <\explain>
    <scm|(enhanced-tree-properties-ref <scm-arg|t>)><explain-synopsis|get
    properties>
  <|explain>
    Obtain an association list with all properties of the enhanced tree
    <scm-arg|t>. <em|Not implemented.>
  </explain>

  <\explain>
    <scm|(enhanced-tree-property-set! <scm-arg|t> <scm-arg|var>
    <scm-arg|val>)><explain-synopsis|set enhanced property>
  <|explain>
    Set the property <scm-arg|var> of an enhanced tree <scm-arg|t> to
    <scm-arg|val>. <em|Not implemented.>
  </explain>

  <\explain>
    <scm|(enhanced-tree-property-ref <scm-arg|t>
    <scm-arg|var>)><explain-synopsis|get enhanced property>
  <|explain>
    Obtain the property <scm-arg|var> of an enhanced tree <scm-arg|t>.
    <em|Not implemented.>
  </explain>

  <paragraph*|Sketch manipulation>

  <\explain>
    <scm|(sketch-get)><explain-synopsis|get current sketch>
  <|explain>
    Return the current sketch. In the current implementation, the sketch is
    a list of (enhanced) trees, rather than a single tree; the originally
    planned name was <scm|sketch-tree>. <scm|(sketch-in? <scm-arg|t>)> tests
    whether <scm-arg|t> belongs to the sketch.
  </explain>

  <\explain>
    <scm|(sketch-new <scm-arg|t>)><explain-synopsis|start sketch>
  <|explain>
    Put a new tree in the sketch, which is not part of the document. This
    routine is typically called when starting the construction of a new
    enhanced shape. <em|Not implemented>; use <scm|sketch-set!>.
  </explain>

  <\explain>
    <scm|(sketch-set! <scm-arg|l>)><explain-synopsis|set sketch>
  <|explain>
    Assign the sketch with the list <scm-arg|l> of trees; trees which are
    part of the document are replaced by their largest enhancement (and the
    correspondence with the document is maintained). This routine is
    typically called when editing an enhanced shape. (The original design
    called it <scm|sketch-set> and took a single tree.)
  </explain>

  <\explain>
    <scm|(sketch-reset)><explain-synopsis|reset sketch tree>
  <|explain>
    Assign the sketch with an empty list of objects. This routine is
    typically called before starting the selection of a group of objects.
  </explain>

  <\explain>
    <scm|(sketch-toggle <scm-arg|t>)><explain-synopsis|toggle a tree in the
    sketch>
  <|explain>
    This routine toggles whether a tree <scm-arg|t> in the document (or
    rather its largest enhancement) belongs to the sketch. This routine is
    typically called when selecting a group of objects.
  </explain>

  <\explain>
    <scm|(sketch-checkout)><explain-synopsis|checkout the sketch>
  <|explain>
    Enter MODIFYING mode and potentially disable the counterparts of the
    trees in the sketch in the main document.
  </explain>

  <\explain>
    <scm|(sketch-commit)><explain-synopsis|commit the sketch>
  <|explain>
    Commit changes made to the sketch in MODIFYING mode and return to
    SELECTING mode.
  </explain>

  <\explain>
    <scm|(sketch-cancel)><explain-synopsis|cancel the sketch>
  <|explain>
    Cancel any changes made to the sketch in MODIFYING mode and return to the
    state of the document before the call of <scm|sketch-checkout>. <em|In
    the current implementation, this routine does nothing.>
  </explain>

  <paragraph*|Miscellaneous>

  <\explain>
    <scm|(sketch-controls-set <scm-arg|l>)><explain-synopsis|set controls>
  <|explain>
    Assign a list of markup objects with control ornaments to the current
    sketch. The ornaments are rendered on top of the sketch as a visual aid
    for the user. Typically, when editing a polyline, <scm-arg|l> consists of
    a list of control points. <em|Not implemented as such:> the current
    object and its control points are computed by
    <scm|graphics-decorations-update> and passed to the <c++> editor with
    <scm|(set-graphical-object <scm-arg|t>)>.
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