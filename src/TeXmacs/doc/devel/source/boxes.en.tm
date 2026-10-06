<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The boxes produced by the typesetter>

  <section|Introduction>

  The <TeXmacs> typesetter essentially translates a document represented by a
  tree into a graphical box, which can either be displayed on the screen or
  on a printer. Contrary to a system like <LaTeX>, the graphical box actually
  contains much more information than is necessary for a graphical rendering.
  Roughly speaking, this information can be subdivided into the following
  categories:

  <\itemize>
    <item>Logical and physical bounding boxes.

    <item>A method for graphical rendering.

    <item>Miscellaneous typesetting information.

    <item>Keeping track of the source subtree which led to the box.

    <item>Computing the positions of cursors and selections.

    <item>Event handlers for dynamic content.
  </itemize>

  The abstract class <cpp|box_rep> and the class <cpp|box> are declared in
  <source-link|Typeset/boxes.hpp|src/Typeset/boxes.hpp>. The concrete box classes are implemented in
  the subdirectories of <verbatim|Typeset/Boxes>: <verbatim|Basic> (text
  boxes, rubber boxes such as large delimiters, empty boxes, <abbr|etc.>),
  <verbatim|Composite> (concatenations, stacks, fractions, roots, scripts,
  superpositions, <abbr|etc.>), <verbatim|Modifier> (boxes which modify the
  rendering of a single child, such as color changes, clipping or
  highlighting), <verbatim|Graphics> (graphics and grids) and
  <verbatim|Animate> (animations). The ways in which boxes are drawn on the
  screen are described in the chapter on <hlink|renderers|renderer.en.tm>.

  The logical bounding box (fields <cpp|x1>, <cpp|y1>, <cpp|x2>,
  <cpp|y2>) is used by the typesetter to position the box with
  respect to other boxes. A certain amount of other information, such as the
  slant of the box, is also stored for the typesetter or can be computed
  by virtual methods. The physical (or ink) bounding box (fields <cpp|x3>,
  <cpp|y3>, <cpp|x4>, <cpp|y4>) encloses the graphical representation of
  the box. This knowledge is
  needed when partially redrawing a box in an efficient way.

  In order to position the cursor or when making a selection, it is necessary
  to have a correspondence between logical positions in the source tree and
  physical positions in the typeset boxes. More precisely, boxes and their
  subboxes are logically organized as a tree. Boxes provide routines to
  translate between paths in the box tree and the source tree and to find the
  path which is associated to a graphical point.

  <section|The correspondence between a box and its source>

  <subsection|Discussion of the problems being encountered>

  In order to implement the correspondence between paths in the source tree
  and the box tree, one has to face several simultaneous difficulties:

  <\enumerate>
    <item>Due to line breaking, footnotes and macro expansions, the
    correspondence may be non straightforward.

    <item>The correspondence has to be reasonably time and space efficient.

    <item>Some boxes, such header and footers, or certain results of macro
    expansions, may not be \Paccessible\Q. Although one should be able to
    find a reasonable cursor position when clicking on them, the contents of
    this box can not be edited directly.

    <item>The correspondence has to be reasonably complete (see the next
    section).
  </enumerate>

  The first difficulty forces us to store a path in the source tree along
  with any box. In order to save storage, this path is stored in a reversed
  manner, so that common heads can be shared. This common head sharing is
  also necessary to quickly change the source locations when modifying the
  source tree, for instance by inserting a new paragraph.

  Inverse paths do not only occur in boxes: each subtree of the global edit
  tree knows its own inverse path through an observer (see
  <source-link|Data/Observers/ip_observer.cpp|src/Data/Observers/ip_observer.cpp> and the function
  <cpp|obtain_ip>), which is updated whenever the tree is modified.

  In order to cope with the third difficulty, the inverse path may start with
  a negative number, which indicates that the box can not directly be edited
  (we also say that the box is a decoration). In this case, the tail of the
  inverse path corresponds to a location in the source tree, where the cursor
  should be positioned when clicking on the box. The negative number
  influences the way in which this is done.

  <subsection|The three kinds of paths>

  More precisely, we have to deal with three kinds of paths:

  <\description>
    <item*|Tree paths>These paths correspond to paths in the source tree.
    Actually, the path minus its last item points to a subtree of the source
    tree. The last item gives a position in this subtree: if the subtree is a
    leaf, i.e. a string, it is a position in this string. Otherwise a zero
    indicates a position before the subtree and a one a position after the
    subtree.

    <item*|Inverse paths>These are just reverted tree paths (with shared
    tails), with an optional negative head. A negative head indicates that
    the tree path is not accessible, i.e. the corresponding subtree does not
    correspond to editable content. The possible negative values are defined
    in <source-link|Typeset/boxes.hpp|src/Typeset/boxes.hpp>. For <cpp|DECORATION> (<math|-1>), the
    tail of the inverse path already includes a position. For
    <cpp|DECORATION_LEFT>, <cpp|DECORATION_MIDDLE> and
    <cpp|DECORATION_RIGHT> (<math|-2>, <math|-3> and <hgroup|<math|-4>>), a
    zero or one has to be put behind the tree path: always zero,
    <abbr|resp.> depending on the cursor position, <abbr|resp.> always one
    (see the function <cpp|descend_decode>). Such inverse paths are
    constructed using <cpp|decorate>, <cpp|decorate_left>,
    <cpp|decorate_middle> and <cpp|decorate_right>. Finally, the value
    <cpp|DETACHED> (<math|-5>) is used for trees which are not attached to
    any document.

    <item*|Box paths>These paths correspond to logical paths in the box tree.
    Again, the path minus its last item points to a subbox of the main box,
    and the last item gives a position in this subtree: if the subbox
    corresponds to a text box it is a position in this text. Otherwise a zero
    indicates a position before the subbox and a one a position after it. In
    the case of side boxes, a two and a three may also indicate the position
    after the left script <abbr|resp.> before the right script.
  </description>

  <subsection|The conversion routines>

  In order to implement the conversion between the three kinds of paths,
  every box comes with a reference inverse path <cpp|ip> in the source
  tree. Composite boxes also come with a left and a right inverse path
  <cpp|lip> <abbr|resp.> <cpp|rip>, which correspond to the left-most and
  right-most accessible paths in its subboxes (if there are such subboxes).
  These are returned by the virtual methods <cpp|find_lip> and
  <cpp|find_rip>.

  The routine:

  <\cpp-code>
    virtual path box_rep::find_tree_path (path bp);
  </cpp-code>

  transforms a box path into a tree path. This routine (which only uses
  <cpp|ip>) is fast and has a linear time complexity as a function of
  the lengths of the paths. The routine:\ 

  <\cpp-code>
    virtual path box_rep::find_box_path (path p, bool& found);
  </cpp-code>

  does the inverse conversion; the flag <cpp|found> is set to
  <cpp|false> if no exact match could be found. Unfortunately, in the worst
  case, it may be necessary to search for the matching tree path in all
  subboxes. Nevertheless, in the best case, a dichotomic algorithm (which
  uses <cpp|lip> and <cpp|rip>), finds the right branch how to descend in a
  logarithmic time. This algorithm also has a quadratic time complexity
  as a function of the lengths of the paths, because we frequently need to
  revert paths.

  <section|The cursor and selections>

  In order to fulfill the requirement of being a \Pstructured editor\Q,
  <TeXmacs> needs to provide a (reasonably) complete correspondence between
  logical tree paths and physical cursor positions. This yields an additional
  difficulty in the case of \Penvironment changes\Q, such as a change in font
  or color. Indeed, when you are on the border of such a change, it is not
  clear <with|font-shape|italic|a priori> which environment you are in.

  In <TeXmacs>, the cursor position therefore contains an <math|x> and a
  <math|y> coordinate, as well as an additional infinitesimal
  <math|x>-coordinate, called <math|\<delta\>>. A change in environment is
  then represented by a box with an infinitesimal width. Although the
  <math|\<delta\>>-position of the cursor is always zero when you select
  using the mouse, it may be non zero when moving around using the cursor
  keys. The routine

  <\cpp-code>
    virtual path box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  </cpp-code>

  which is linear time as a function of the length of the path, searches
  the box path which corresponds to a cursor position. The non virtual
  method <cpp|find_tree_path (SI x, SI y, SI delta)> combines this routine
  with the conversion into a tree path. Inversely, the routine

  <\cpp-code>
    virtual cursor box_rep::find_cursor (path bp);
  </cpp-code>

  yields a graphical representation for the cursor at a certain box path.
  The cursor (see the class <cpp|cursor_rep> in <source-link|Typeset/boxes.hpp|src/Typeset/boxes.hpp>)
  is given by the coordinates <math|x>, <math|y> and <math|\<delta\>> of its
  origin (the fields <cpp|ox>, <cpp|oy> and <cpp|delta>), and a line segment
  relative to this origin, which is determined by its vertical extremities
  <math|y<rsub|1>> and <math|y<rsub|2>> and its <cpp|slope>.

  In a similar way, the routine

  <\cpp-code>
    virtual selection box_rep::find_selection (path lbp, path rbp);
  </cpp-code>

  computes the selection between two given box paths. This selection
  comprises two delimiting tree paths (the fields <cpp|start> and
  <cpp|end>) and a graphical representation in the form of a list of
  rectangles (the field <cpp|rs>).

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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