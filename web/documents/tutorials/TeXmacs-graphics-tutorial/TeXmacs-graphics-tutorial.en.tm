<TeXmacs|2.1.5>

<style|<tuple|article|doc>>

<\body>
  <doc-data|<doc-title|A <TeXmacs> graphics tutorial>|<doc-running-title|A
  <TeXmacs> graphics tutorial>|<doc-author|<author-data|<author-name|Henri
  Lesourd>|<author-email|texmacs@free.fr>>>|<doc-date|June, September
  2005>|<doc-misc|Updated for <TeXmacs> 2.1.5 in 2026.>>

  <section|Introduction>

  This document describes the graphical tool of <TeXmacs>, which you can use
  to make drawings, illustrations, diagrams, etc. In
  section<nbsp><reference|sec-example>, we introduce this graphical tool
  using a simple example, for which we show in detail the operations that a
  user must perform to build it step by step. In
  sections<nbsp><reference|sec-grids> to<nbsp><reference|sec-misc>, we
  describe the available features of the graphical tool, and
  section<nbsp><reference|sec-since> lists what changed since the first
  version of this tutorial. Finally, appendices<nbsp><reference|app-markup>
  and<nbsp><reference|app-scheme> summarize the graphical markup of <TeXmacs>
  and the use of the <name|Scheme> scripting language to generate calculated
  drawings.

  The pictures of this tutorial were made with <TeXmacs> 2.1.5 (the <TeXmacs>
  Fixes application for <name|macOS>); on other systems and with other looks,
  the toolbars look slightly different, and the keyboard shortcuts are those
  shown by the menus of your <TeXmacs>.

  <subsection|Overview><label|sec-intro>

  The graphical tool of <TeXmacs> is made of several complementary tools:

  <\enumerate-roman>
    <item>A <em|grid>, which helps you to position the objects precisely. You
    can change the parameters of the grid to make it finer or coarser, and
    change the colors of its different kinds of lines (axes, units,
    subunits). The available grids are <em|Cartesian>, <em|polar>,
    <em|logarithmic> and <em|notebook> grids (see
    section<nbsp><reference|sec-grids>). Points are <em|snapped> to the grid,
    to the points and intersections of the existing curves and to the borders
    of the text boxes; what is snapped to is set in <menu|Focus|Snap>.

    <item>Tools for drawing <em|curves>: <em|points>, <em|lines>,
    <em|polygons>, <em|splines>, <em|smooth curves> and <em|Bezier curves>
    with their closed variants, <em|arcs> and <em|circles> (see
    section<nbsp><reference|sec-curves>).

    <item><em|Text boxes>, <em|mathematical formulas> and <em|long text>
    boxes can be placed anywhere in the picture, with any <TeXmacs> markup
    inside; you can choose their horizontal and vertical alignment with
    respect to the anchor point which places them in the picture (see
    section<nbsp><reference|sec-text>). Hand-drawn curves can be added with
    the <em|penscript> and <em|calligraphy> tools.

    <item>The <em|properties> of the objects: the <em|color>, <em|fill
    color>, <em|opacity>, <em|line width>, <em|dash style>, <em|arrows>,
    <em|point style> and the <em|alignment> of text boxes (see
    section<nbsp><reference|sec-props>).

    <item>You can <em|select> one or more objects, and <em|copy>, <em|cut>
    and <em|paste> them (see section<nbsp><reference|sec-select>).

    <item>Operations on a set of selected objects: <em|grouping and
    ungrouping>, <em|moving>, <em|resizing> and <em|rotating> (see
    section<nbsp><reference|sec-transform>).

    <item>Miscellaneous: changing the size of the picture, cropping it,
    moving the origin of its frame and zooming the whole picture (see
    section<nbsp><reference|sec-misc>).
  </enumerate-roman>

  <subsection|The user interface>

  When the cursor is inside a picture, the <menu|Insert> menu becomes the
  menu of the graphical tool (on the left below), and the <menu|Focus> menu
  gives the properties of the objects which will be drawn, for the current
  mode (on the right, for circles):

  <\center>
    <image|images/menu-insert-graphics.png|0.25par|||><space|3em><image|images/menu-focus-graphics.png|0.24par|||>
  </center>

  The mode dependent toolbar becomes the <em|graphics toolbar>:

  <\center>
    <image|images/toolbar-graphics.png|0.95par|||>
  </center>

  Its first row is made of four groups of icons:

  <\itemize>
    <item>The <em|global icons> give the <menu|Insert|Geometry> menu (size,
    crop, alignment, unit, origin and zoom of the picture), the
    <menu|Insert|Grid> menu, the overlays of the picture, and the icon to
    leave graphics mode:

    <image|images/icons-global.png|0.16par|||>

    <item>The <em|insertion modes> for curves: point, line, polygon, spline,
    closed spline, arc and circle (in the <menu|Insert> menu: <menu|Point>,
    <menu|Line>, <menu|Polygon>, <menu|Curve>, <menu|Closed curve>,
    <menu|Arc>, <menu|Circle>):

    <image|images/icons-modes.png|0.27par|||>

    <item>The insertion modes for <menu|Text>, <menu|Mathematics>, <menu|Long
    text> and hand-drawn curves:

    <image|images/icons-text.png|0.16par|||>

    <item>The <em|group operations>: <menu|Set properties>, <menu|Move
    objects>, <menu|Resize objects>, <menu|Rotate objects>,
    <menu|Group/ungroup> and <menu|Animate objects>:

    <image|images/icons-group.png|0.23par|||>
  </itemize>

  The second row, the <em|focus bar>, shows the current mode and the current
  values of the properties of the objects which will be drawn in this mode
  (color, fill color, opacity, line width and dashes, arrows...); click on a
  value to change it. For the group operations, it gives the snapping
  options.

  <subsection|An introductory example><label|sec-example>

  So that you can try the graphical tool of <TeXmacs> right away, even if you
  know nothing about it yet, we now show you step by step how to build the
  following drawing:

  <\center>
    <image|images/example-final.png|0.75par|||>
  </center>

  <subsubsection|Insert a new picture and set its grid>

  First, insert a new picture in the document, using <menu|Insert|Image|Draw
  image>. The cursor is now inside the picture, and the menus and toolbars
  are those of the graphical tool. Add a polar grid to the picture, using
  <menu|Insert|Grid|Type|Polar>. Next, set the parameters of the grid: select
  <menu|Insert|Grid|Unit length|2>, then <menu|Insert|Grid|Number of polar
  steps|6>, then <menu|Insert|Grid|Number of subunit steps|2> (this entry
  appears when <menu|Insert|Grid|Show subunits> is on, as it is by default).
  To see the whole picture at once, you can also reduce the zoom of the
  window (<menu|View|Zoom>). You obtain the following grid:

  <\center>
    <image|images/step1-polar-grid.png|0.75par|||>
  </center>

  <subsubsection|Insert a first circle>

  Now we insert a circle into the picture. Choose <menu|Insert|Circle> (or
  the icon <icon|tm_carc_mode.xpm>), move the mouse over the picture and
  click with the <em|left mouse button>. As soon as you move the mouse, you
  see that you are moving the end of a segment:

  <\center>
    <image|images/step2-drawing-circle.png|0.75par|||>
  </center>

  When you click again, the second point of the circle is added, and the
  segment becomes a moving circle through the two points and the mouse. Click
  a third time and then once more at the same place (a double click) to fix
  the last point: the whole circle is inserted in the picture, and you are
  ready to insert another object. The status bar always tells you what the
  mouse buttons do; for instance, while you are inserting the points,
  <em|Right click: undo> removes the last one.

  <\center>
    <image|images/step2-first-circle.png|0.75par|||>
  </center>

  We now move the points of the circle to their intended positions. When the
  mouse is near the circle, its three <em|control points> are shown, and when
  the mouse is near one of them, this control point becomes the current one,
  and it is highlighted:

  <\center>
    <image|images/step2-hover-point.png|0.75par|||>
  </center>

  All curves (lines, splines, arcs...) are edited this way in <TeXmacs>:

  <\itemize>
    <item>to <em|move> a control point, drag and drop it with the <em|left
    mouse button>;

    <item>when the mouse is on an <em|edge> of a curve with an arbitrary
    number of points (lines, polygons, splines), a drag and drop with the
    left mouse button <em|inserts> a new point there;

    <item>the <em|middle mouse button> removes the control point under the
    mouse, and <key|S-> with the middle mouse button removes the whole
    object;

    <item>when several control points or objects are at the same place, the
    <key|tab> key cycles through them.
  </itemize>

  Thanks to the snapping, the points fall exactly on the points of the grid.
  Move the three points of the circle to the origin and to the intersections
  of the first subunit circle with the rays at <math|\<pm\>60> degrees. You
  obtain:

  <\center>
    <image|images/step3-circle-moved.png|0.75par|||>
  </center>

  <subsubsection|Set the properties of this first circle>

  We now turn our circle into a thicker blue one. Select the <menu|Insert|Set
  properties> mode (or the icon <icon|tm_edit_props.xpm>), and click on the
  circle with the left mouse button: the circle is selected (with the right
  mouse button, or by dragging a rectangle with it, you can select several
  objects), and the focus bar and the <menu|Focus> menu now show its
  properties. Choose the color in <menu|Focus|Color|Blue> and the width in
  <menu|Focus|Line width|2 ln> (or in the focus bar): the change is applied
  at once to the selected circle, which becomes thicker and blue.

  <\center>
    <image|images/step4-blue-circle.png|0.75par|||>
  </center>

  In the insertion modes, the <menu|Focus> menu and the focus bar give the
  properties of the objects which will be drawn: if you choose a color and a
  width <em|before> drawing, the next objects are drawn with them. In any
  mode, <shortcut|(graphics-apply-props-at-mouse)> applies these current
  properties to the object under the mouse.

  <subsubsection|Make three copies of this circle>

  Now we see how to copy and paste graphical objects. Select the
  <menu|Insert|Move objects> mode (icon <icon|tm_group_move.xpm>), move the
  mouse over the circle and click with the <em|right mouse button>: the
  circle is selected. Then copy it with <menu|Edit|Copy>
  (<shortcut|(kbd-copy)>) and paste it with <menu|Edit|Paste>
  (<shortcut|(kbd-paste)>) three times: copy and paste work in the group
  modes, and the three copies lie on top of the original circle.

  <\center>
    <image|images/step5-copy-selected.png|0.75par|||>
  </center>

  <subsubsection|Set the position and the color of each of the circles>

  Now move each copy to its own position (the circles all go through the
  origin, and their centers are on the first subunit circle of the grid, to
  the north, the south and the west). In <menu|Insert|Move objects> mode, a
  left click on a copy picks it up, and it follows the mouse until a second
  left click drops it (you can also drag and drop it). Then go back to the
  <menu|Insert|Set properties> mode, click on a circle to select it and
  choose its color in <menu|Focus|Color>: green for the north, red for the
  south and brown for the west. You obtain:

  <\center>
    <image|images/step5-four-circles.png|0.75par|||>
  </center>

  <subsubsection|Add a text box>

  We now add a text box with the text \PEast\Q, in bold face and in size 24.
  Choose <menu|Insert|Text> (icon <icon|tm_textat_mode.xpm>), move the mouse
  into the picture and click with the left mouse button: a new text box is
  inserted, with the text cursor inside it. Choose the bold face and the size
  24 (<menu|Format|Series|Bold>, <menu|Format|Size|24>, or the font toolbar),
  and type \PEast\Q. You should obtain something like:

  <\center>
    <image|images/step6-text-east.png|0.75par|||>
  </center>

  The toolbars are now those of the text inside the box, and the focus bar
  gives the alignment of the box. To leave the text box, click in the picture
  outside of it.

  <subsubsection|Make three copies of this text box>

  Now make three copies of the text box you just inserted, as you did for the
  circles. Move each box to a different position, then click into each copy
  to change its text. You get something like:

  <\center>
    <image|images/step7-four-texts.png|0.75par|||>
  </center>

  <subsubsection|Change the alignment of the text boxes>

  The position of a text box is given by an anchor point; its <em|horizontal
  alignment> (left, center or right) and its <em|vertical alignment> (bottom,
  base, axis, center or top) say how the box is placed with respect to this
  point. We give \PWest\Q a right alignment, \PEast\Q a left alignment, both
  centered vertically, and center \PNorth\Q and \PSouth\Q horizontally,
  \PNorth\Q at the bottom and \PSouth\Q at the top. To do this, choose the
  <menu|Insert|Set properties> mode, click on a text box with the left mouse
  button to select it, and choose its alignment in <menu|Focus|Horizontal
  alignment> and <menu|Focus|Vertical alignment> (or in the focus bar). The
  boxes move with respect to their anchor points:

  <\center>
    <image|images/step8-alignment.png|0.75par|||>
  </center>

  <subsubsection|Move the text boxes to their intended positions>

  Now, in <menu|Insert|Text> mode, drag and drop each text box (with the left
  mouse button) to its position: the anchor points snap to the points of the
  grid. Thanks to the alignments, each text sits next to its circle, outside
  of it:

  <\center>
    <image|images/step9-positions.png|0.75par|||>
  </center>

  To place the boxes more precisely, you can zoom in, with <menu|View|Zoom>
  or with the keys of section<nbsp><reference|sec-misc>, and come back to the
  usual zoom afterwards.

  <subsubsection|Select all, group it>

  To do this, select the <menu|Insert|Group/ungroup> mode (icon
  <icon|tm_group_group.xpm>). You can then select all the objects at once:
  drag the <em|right mouse button> from one corner of a rectangle around the
  objects to the opposite corner; all the objects inside the rectangle are
  selected. You can also select the objects one after the other with right
  clicks (a right click on a selected object unselects it). Once the
  selection is done, click with the <em|left mouse button> to group the
  selected objects into one object:

  <\center>
    <image|images/step10-select-all.png|0.75par|||>
  </center>

  A left click on a group, in this mode, ungroups it again.

  <subsubsection|Resize and rotate the whole>

  Finally, to make the whole drawing smaller, select the <menu|Insert|Resize
  objects> mode (icon <icon|tm_group_zoom.xpm>), select the group with a
  right click if it is not selected yet, click with the left mouse button to
  start, move the mouse, and click again with the left mouse button when the
  size is right. We get for example:

  <\center>
    <image|images/step11-zoomed.png|0.75par|||>
  </center>

  To rotate the drawing, select the <menu|Insert|Rotate objects> mode (icon
  <icon|tm_group_rotate.xpm>) and proceed in the same way. The text boxes
  move with the rotation, but their text stays horizontal:

  <\center>
    <image|images/step11-rotated.png|0.75par|||>
  </center>

  <subsubsection|Change the geometry of the picture>

  You can change the size of the picture with <menu|Insert|Geometry|Size> and
  <menu|Insert|Geometry|Resize>, or with the keys
  <shortcut|(graphics-decrease-hsize)>, <shortcut|(graphics-increase-hsize)>,
  <shortcut|(graphics-decrease-vsize)> and
  <shortcut|(graphics-increase-vsize)>; with
  <menu|Insert|Geometry|Crop|Crop>, the picture is cropped automatically to
  the size of its contents (plus a padding). <menu|Insert|Geometry|Alignment>
  sets the vertical alignment of the picture with respect to the text. You
  can also zoom in and out of the whole picture with
  <shortcut|(graphics-zoom-in)> and <shortcut|(graphics-zoom-out)>, and shift
  the drawing with the arrow keys (when the picture is not cropped
  automatically).

  <section|Grids><label|sec-grids>

  The grid is set in the <menu|Insert|Grid> menu (or with the icon
  <icon|tm_graphics_grid.xpm>).

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|<menu|Insert|Grid|Show
  grid>>|<cell|Show or hide the grid (also
  <shortcut|(graphics-toggle-grid)>)>>|<row|<cell|<menu|Insert|Grid|Type>>|<cell|Set
  the type of grid: no grid, Cartesian, polar, logarithmic or
  notebook>>|<row|<cell|<menu|Insert|Grid|Center>>|<cell|Set the center of
  the grid>>|<row|<cell|<menu|Insert|Grid|Unit length>>|<cell|Set the unit
  length>>|<row|<cell|<menu|Insert|Grid|Number of polar steps>>|<cell|Set the
  number of rays (polar grids only)>>|<row|<cell|<menu|Insert|Grid|Logarithmic
  base>>|<cell|Set the base (logarithmic grids
  only)>>|<row|<cell|<menu|Insert|Grid|Color of the axes>>|<cell|Set the
  color of the axes>>|<row|<cell|<menu|Insert|Grid|Color of the
  units>>|<cell|Set the color of the unit
  lines>>|<row|<cell|<menu|Insert|Grid|Show subunits>>|<cell|Show or hide the
  subunit lines>>|<row|<cell|<menu|Insert|Grid|Color of the
  subunits>>|<cell|Set the color of the subunit
  lines>>|<row|<cell|<menu|Insert|Grid|Number of subunit steps>>|<cell|Set
  the number of subunits per unit (also <shortcut|(graphics-set-grid-aspect
  'detailed 2 #t)>, <shortcut|(graphics-set-grid-aspect 'detailed 3
  #t)>...)>>|<row|<cell|<menu|Focus|Snap>>|<cell|Choose what the points snap
  to: grid points, grid lines, curve points, intersections, text corners and
  borders; the snap distance>>>>>>
    The grid.
  </big-table>

  Older versions of <TeXmacs> had a separate, invisible <em|edit grid>; it is
  replaced by the snapping options of <menu|Focus|Snap>.

  <section|Curves><label|sec-curves>

  <subsection|Insertion modes>

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|<menu|Insert|Point>
  <icon|tm_point_mode.xpm>>|<cell|Insert points>>|<row|<cell|<menu|Insert|Line>
  <icon|tm_line_mode.xpm>>|<cell|Insert broken
  lines>>|<row|<cell|<menu|Insert|Polygon>
  <icon|tm_cline_mode.xpm>>|<cell|Insert polygons>>|<row|<cell|<menu|Insert|Curve>
  <icon|tm_spline_mode.xpm>>|<cell|Insert splines, smooth curves or Bezier
  curves>>|<row|<cell|<menu|Insert|Closed curve>
  <icon|tm_cspline_mode.xpm>>|<cell|Insert closed splines, smooth curves or
  Bezier curves>>|<row|<cell|<menu|Insert|Arc>
  <icon|tm_arc_mode.xpm>>|<cell|Insert arcs (through three
  points)>>|<row|<cell|<menu|Insert|Circle>
  <icon|tm_carc_mode.xpm>>|<cell|Insert circles (through three
  points)>>|<row|<cell|<menu|Insert|Penscript>,
  <menu|Insert|Calligraphy>>|<cell|Draw by hand>>>>>>
    The insertion modes for curves.
  </big-table>

  <subsection|Operations>

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|Left
  click>|<cell|Start a new curve, or add a point to the curve being
  drawn>>|<row|<cell|Double left click>|<cell|Add the last point and finish
  the curve>>|<row|<cell|Right click>|<cell|Remove the last point of the
  curve being drawn; otherwise, remove the control point under the mouse,
  like the middle button (with <key|S->: the whole object)>>|<row|<cell|Left
  drag and drop>|<cell|Move the control point under the mouse, or insert a
  new point on the edge under the mouse>>|<row|<cell|Middle
  click>|<cell|Remove the control point under the mouse>>|<row|<cell|<key|S->
  and middle click>|<cell|Remove the object under the
  mouse>>|<row|<cell|<key|tab>>|<cell|Make the next control point or object
  under the mouse the current one>>|<row|<cell|<key|backspace>,
  <key|delete>>|<cell|Remove the control point under the mouse, like the
  middle button>>|<row|<cell|<shortcut|(graphics-zmove 'closer)>,
  <shortcut|(graphics-zmove 'farther)>>|<cell|Move the current object closer
  to or farther from the eye>>|<row|<cell|<shortcut|(undo 0)>>|<cell|Undo (as
  everywhere in <TeXmacs>)>>>>>>
    Editing curves.
  </big-table>

  <section|Text boxes><label|sec-text>

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|<menu|Insert|Text>
  <icon|tm_textat_mode.xpm>>|<cell|Insert text
  boxes>>|<row|<cell|<menu|Insert|Mathematics>
  <icon|tm_math.xpm>>|<cell|Insert mathematical
  formulas>>|<row|<cell|<menu|Insert|Long text>
  <icon|tm_document_at.xpm>>|<cell|Insert boxes with several
  paragraphs>>|<row|<cell|Left click outside the boxes>|<cell|Insert a new
  box and put the text cursor into it>>|<row|<cell|Left click on a
  box>|<cell|Put the text cursor into the box>>|<row|<cell|Left drag and
  drop>|<cell|Move the box under the mouse>>|<row|<cell|Middle click, right
  click>|<cell|Remove the box under the mouse>>>>>>
    Text boxes.
  </big-table>

  <section|Properties of the objects><label|sec-props>

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|<menu|Insert|Set
  properties> <icon|tm_edit_props.xpm>>|<cell|Enter the mode for changing the
  properties of existing objects; in this mode, the values chosen below apply
  at once to the selected objects>>|<row|<cell|<menu|Focus|Color>>|<cell|Set
  the current color>>|<row|<cell|<menu|Focus|Fill color>>|<cell|Set the
  current fill color>>|<row|<cell|<menu|Focus|Opacity>>|<cell|Set the current
  opacity>>|<row|<cell|<menu|Focus|Point style>, <menu|Focus|Point
  size>>|<cell|Set the style and the size of
  points>>|<row|<cell|<menu|Focus|Line width>>|<cell|Set the current line
  width>>|<row|<cell|<menu|Focus|Line dashes>>|<cell|Set the current dash
  pattern>>|<row|<cell|<menu|Focus|Line arrows>>|<cell|Set the current
  arrows>>|<row|<cell|<menu|Focus|Horizontal alignment>>|<cell|Set the
  current horizontal alignment of text boxes>>|<row|<cell|<menu|Focus|Vertical
  alignment>>|<cell|Set the current vertical alignment of text
  boxes>>|<row|<cell|Left click (in <menu|Set properties> mode)>|<cell|Select
  the object under the mouse (a right click adds it to the
  selection)>>|<row|<cell|<shortcut|(graphics-apply-props-at-mouse)>>|<cell|Apply
  the current properties to the object under the
  mouse>>|<row|<cell|<shortcut|(graphics-get-props-at-mouse)>>|<cell|Make the
  properties of the object under the mouse the current ones>>>>>>
    Properties.
  </big-table>

  The menus show only the properties which make sense in the current mode:
  for instance, the alignments are in the <menu|Focus> menu of the text
  modes.

  <section|Selecting and grouping the objects><label|sec-select>

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|<menu|Insert|Group/ungroup>
  <icon|tm_group_group.xpm>>|<cell|Enter the group/ungroup mode (the
  selection actions below are also available in the other group
  modes)>>|<row|<cell|Right click>|<cell|Add the object under the mouse to
  the selection, or remove it>>|<row|<cell|Right drag and drop>|<cell|Select
  all the objects inside a rectangle>>|<row|<cell|Right click outside the
  objects>, middle click|<cell|Forget the current selection>>|<row|<cell|Left
  click>|<cell|Group the selected objects, or ungroup the selected
  group>>|<row|<cell|<menu|Edit|Copy>, <menu|Edit|Cut>>|<cell|Copy or cut the
  selected objects>>|<row|<cell|<menu|Edit|Paste>>|<cell|Paste the objects,
  at the place of the copied ones; they are selected>>|<row|<cell|<key|S->
  and middle click>|<cell|Remove the selected objects (if nothing is
  selected, the object under the mouse)>>>>>>
    Selecting and grouping.
  </big-table>

  <section|Transforming the objects><label|sec-transform>

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|<menu|Insert|Move
  objects> <icon|tm_group_move.xpm>>|<cell|Enter the move
  mode>>|<row|<cell|<menu|Insert|Resize objects>
  <icon|tm_group_zoom.xpm>>|<cell|Enter the resize
  mode>>|<row|<cell|<menu|Insert|Rotate objects>
  <icon|tm_group_rotate.xpm>>|<cell|Enter the rotate mode>>|<row|<cell|Left
  click>|<cell|Start the operation on the selected objects, or on the object
  under the mouse if nothing is selected; a second left click finishes
  it>>|<row|<cell|Left drag and drop>|<cell|Perform the operation in one
  move>>>>>>
    Transforming objects.
  </big-table>

  <section|Miscellaneous><label|sec-misc>

  The following keys act on the whole picture when the cursor is inside it.

  <\big-table|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|1|1|-1|font-shape|italic>|<cwith|1|-1|2|2|cell-hyphen|t>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|Action>|<cell|Behaviour>>|<row|<cell|<shortcut|(graphics-zoom-in)>>|<cell|Zoom
  in>>|<row|<cell|<shortcut|(graphics-zoom-out)>>|<cell|Zoom
  out>>|<row|<cell|<shortcut|(graphics-set-zoom 1.0)>,
  <shortcut|(graphics-set-zoom 2.0)>...>|<cell|Zoom by a factor 1,
  2...>>|<row|<cell|<shortcut|(graphics-move-origin-left)>,
  <shortcut|(graphics-move-origin-right)>>|<cell|Shift the drawing to the
  right, to the left>>|<row|<cell|<shortcut|(graphics-move-origin-down)>,
  <shortcut|(graphics-move-origin-up)>>|<cell|Shift the drawing up,
  down>>|<row|<cell|<shortcut|(graphics-set-origin "0.5gw"
  "0.5gh")>>|<cell|Put the origin at the center>>|<row|<cell|<shortcut|(graphics-decrease-hsize)>,
  <shortcut|(graphics-increase-hsize)>>|<cell|Decrease, increase the width of
  the picture>>|<row|<cell|<shortcut|(graphics-increase-vsize)>,
  <shortcut|(graphics-decrease-vsize)>>|<cell|Increase, decrease the height
  of the picture>>|<row|<cell|<shortcut|(graphics-toggle-grid)>>|<cell|Show
  or hide the grid>>>>>>
    Keys acting on the whole picture.
  </big-table>

  <section|Since 2005><label|sec-since>

  The plans of the first version of this tutorial were \Pother kinds of
  curves (i.e.<nbsp>conics)\Q and \Pzooming and rotating the text boxes\Q.
  Meanwhile <TeXmacs> gained smooth and Bezier curves, the opacity and fill
  of the objects, hand-drawn curves, long text boxes, arrows with text,
  overlays and animations of pictures, and the snapping of the points to the
  grid and to the existing objects.

  <\with|font-size|2>
    <space|2em><em|Long live Free Software!>
  </with>

  <appendix|Overview of the graphical markup><label|app-markup>

  <subsection|Graphical objects>

  <\itemize>
    <item><markup|point>: <inactive*|<point|x|y>>;

    <item><markup|line>: <inactive*|<line|<point|x<rsub|1>|y<rsub|1>>|...|<point|x<rsub|n>|y<rsub|n>>>>;
    <markup|cline> (polygons), <markup|spline>, <markup|cspline>,
    <markup|smooth>, <markup|csmooth>, <markup|bezier>, <markup|cbezier> take
    their points in the same way;

    <item><markup|arc>: <inactive*|<arc|<point|x<rsub|1>|y<rsub|1>>|<point|x<rsub|2>|y<rsub|2>>|<point|x<rsub|3>|y<rsub|3>>>>,
    and <markup|carc> for circles;

    <item><markup|gr-group>: <inactive*|<gr-group|object<rsub|1>|...|object<rsub|n>>>;

    <item><markup|text-at>: <inactive*|<text-at|markup|<point|x|y>>>, and
    <markup|math-at> and <markup|document-at> for formulas and long texts.
  </itemize>

  A picture is a <markup|graphics> tag containing these objects, usually
  inside a <markup|with> which gives the frame (<src-var|gr-frame>), the size
  (<src-var|gr-geometry>), the grid (<src-var|gr-grid>) and the current
  properties of the editor (<src-var|gr-color>, <src-var|gr-mode>...).

  <subsection|Graphical properties>

  The properties are set with <markup|with> around the objects:

  <\itemize>
    <item><src-var|color>, <src-var|fill-color>: red, blue, green, pastel
    red, #4040ff...;

    <item><src-var|opacity>: 0% to 100%;

    <item><src-var|point-style>: disk, round, square...;
    <src-var|point-size>: 2.5ln...;

    <item><src-var|line-width>: 1ln, 2ln, 0.01cm...;

    <item><src-var|dash-style>: a pattern of 0 and 1, like 11100;
    <src-var|dash-style-unit>: 5ln...;

    <item><src-var|arrow-begin>, <src-var|arrow-end>: \<less\>, \<gtr\>,
    \|\<gtr\>, \<gtr\>\<gtr\>, o...;

    <item><src-var|text-at-halign>: left, center, right;

    <item><src-var|text-at-valign>: bottom, base, axis, center, top.
  </itemize>

  For instance, the blue circle of our example is

  <\tm-fragment>
    <inactive*|<with|color|blue|line-width|2ln|<carc|<point|0|0>|<point|0.5|-0.866>|<point|0.5|0.866>>>>
  </tm-fragment>

  <appendix|Graphical output using <name|Scheme>><label|app-scheme>

  In this appendix, we show a <name|Scheme> function which draws the lines
  linking together a set of points on a circle, each of the points being
  linked with all the others. Its parameters are the radius of the circle,
  the number of points, the color and the line width of the lines. The
  function returns a picture, as <TeXmacs> markup in <name|Scheme> form.

  <\scm-code>
    (tm-define (rose r n col lw)

    \ \ (:secure #t)

    \ \ (let* ((n (string-\<gtr\>number (tree-\<gtr\>string n)))

    \ \ \ \ \ \ \ \ \ (rad (/ (length-decode (tree-\<gtr\>string r))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (length-decode "1cm")))

    \ \ \ \ \ \ \ \ \ (pi (acos -1))

    \ \ \ \ \ \ \ \ \ (pts (map (lambda (i)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (let ((t (/ (* 2 pi i) n)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ `(point
    ,(number-\<gtr\>string (* rad (cos t)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ,(number-\<gtr\>string
    (* rad (sin t))))))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (iota n)))

    \ \ \ \ \ \ \ \ \ (lines (append-map (lambda (p)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (map (lambda
    (q) `(line ,p ,q))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (cdr
    (member p pts))))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ pts))

    \ \ \ \ \ \ \ \ \ (size (string-append (number-\<gtr\>string (* 2 (+ rad
    0.2))) "cm")))

    \ \ \ \ `(with "gr-geometry" (tuple "geometry" ,size ,size "center")

    \ \ \ \ \ \ \ \ \ \ \ "color" ,(tree-\<gtr\>string col)

    \ \ \ \ \ \ \ \ \ \ \ "line-width" ,(tree-\<gtr\>string lw)

    \ \ \ \ \ \ \ \ \ \ \ "fill-color" ,(tree-\<gtr\>string col)

    \ \ \ \ \ \ \ (graphics "" ,@lines ,@pts))))
  </scm-code>

  Put this code in your file <verbatim|my-init-texmacs.scm> (in the folder
  <verbatim|progs> of your <TeXmacs> home folder, <verbatim|~/.TeXmacs> on
  <name|Linux> and <name|macOS>, <verbatim|%APPDATA%\\TeXmacs> on
  <name|Windows>), or evaluate it in a <name|Scheme> session
  (<menu|Insert|Session|Scheme>). The <markup|extern> primitive embeds the
  function in a macro:

  <\tm-fragment>
    <inactive*|<assign|rose|<macro|r|st|col|lw|<extern|rose|<arg|r>|<arg|st>|<arg|col>|<arg|lw>>>>>
  </tm-fragment>

  Using an instance of this macro, for example
  <inactive*|<rose|3.5cm|15|red|2ln>>, we get the following drawing:

  <\center>
    <with|gr-geometry|<tuple|geometry|7.40003307315783cm|7.40003307315783cm|center>|color|red|line-width|2ln|fill-color|red|<graphics||<line|<point|3.50001653657891|0.0>|<point|3.19742420866565|1.4235849767979>>|<line|<point|3.50001653657891|0.0>|<point|2.34196818738708|2.60101917824393>>|<line|<point|3.50001653657891|0.0>|<point|1.08156459039623|3.32871353425417>>|<line|<point|3.50001653657891|0.0>|<point|-0.365851349979968|3.48084307977876>>|<line|<point|3.50001653657891|0.0>|<point|-1.75000826828946|3.03110323434297>>|<line|<point|3.50001653657891|0.0>|<point|-2.83157285868568|2.05725810298086>>|<line|<point|3.50001653657891|0.0>|<point|-3.42353277778331|0.72769435601024>>|<line|<point|3.50001653657891|0.0>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|3.50001653657891|0.0>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|3.50001653657891|0.0>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|3.50001653657891|0.0>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|3.50001653657891|0.0>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|3.50001653657891|0.0>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|3.50001653657891|0.0>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|3.19742420866565|1.4235849767979>|<point|2.34196818738708|2.60101917824393>>|<line|<point|3.19742420866565|1.4235849767979>|<point|1.08156459039623|3.32871353425417>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-0.365851349979968|3.48084307977876>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-1.75000826828946|3.03110323434297>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-2.83157285868568|2.05725810298086>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-3.42353277778331|0.72769435601024>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|3.19742420866565|1.4235849767979>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|3.19742420866565|1.4235849767979>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|3.19742420866565|1.4235849767979>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|3.19742420866565|1.4235849767979>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|2.34196818738708|2.60101917824393>|<point|1.08156459039623|3.32871353425417>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-0.365851349979968|3.48084307977876>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-1.75000826828946|3.03110323434297>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-2.83157285868568|2.05725810298086>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-3.42353277778331|0.72769435601024>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|2.34196818738708|2.60101917824393>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|2.34196818738708|2.60101917824393>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|2.34196818738708|2.60101917824393>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|2.34196818738708|2.60101917824393>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-0.365851349979968|3.48084307977876>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-1.75000826828946|3.03110323434297>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-2.83157285868568|2.05725810298086>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-3.42353277778331|0.72769435601024>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|1.08156459039623|3.32871353425417>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|1.08156459039623|3.32871353425417>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|1.08156459039623|3.32871353425417>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|1.08156459039623|3.32871353425417>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|-1.75000826828946|3.03110323434297>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|-2.83157285868568|2.05725810298086>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|-3.42353277778331|0.72769435601024>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-0.365851349979968|3.48084307977876>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|-2.83157285868568|2.05725810298086>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|-3.42353277778331|0.72769435601024>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-1.75000826828946|3.03110323434297>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|-3.42353277778331|0.72769435601024>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-2.83157285868568|2.05725810298086>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-3.42353277778331|0.72769435601024>|<point|-3.42353277778331|-0.727694356010239>>|<line|<point|-3.42353277778331|0.72769435601024>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|-3.42353277778331|0.72769435601024>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|-3.42353277778331|0.72769435601024>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|-3.42353277778331|0.72769435601024>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-3.42353277778331|0.72769435601024>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-3.42353277778331|0.72769435601024>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-3.42353277778331|-0.727694356010239>|<point|-2.83157285868569|-2.05725810298086>>|<line|<point|-3.42353277778331|-0.727694356010239>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|-3.42353277778331|-0.727694356010239>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|-3.42353277778331|-0.727694356010239>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-3.42353277778331|-0.727694356010239>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-3.42353277778331|-0.727694356010239>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-2.83157285868569|-2.05725810298086>|<point|-1.75000826828946|-3.03110323434296>>|<line|<point|-2.83157285868569|-2.05725810298086>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|-2.83157285868569|-2.05725810298086>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-2.83157285868569|-2.05725810298086>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-2.83157285868569|-2.05725810298086>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-1.75000826828946|-3.03110323434296>|<point|-0.365851349979971|-3.48084307977876>>|<line|<point|-1.75000826828946|-3.03110323434296>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-1.75000826828946|-3.03110323434296>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-1.75000826828946|-3.03110323434296>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|-0.365851349979971|-3.48084307977876>|<point|1.08156459039623|-3.32871353425417>>|<line|<point|-0.365851349979971|-3.48084307977876>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|-0.365851349979971|-3.48084307977876>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|1.08156459039623|-3.32871353425417>|<point|2.34196818738708|-2.60101917824393>>|<line|<point|1.08156459039623|-3.32871353425417>|<point|3.19742420866565|-1.4235849767979>>|<line|<point|2.34196818738708|-2.60101917824393>|<point|3.19742420866565|-1.4235849767979>>|<point|3.50001653657891|0.0>|<point|3.19742420866565|1.4235849767979>|<point|2.34196818738708|2.60101917824393>|<point|1.08156459039623|3.32871353425417>|<point|-0.365851349979968|3.48084307977876>|<point|-1.75000826828946|3.03110323434297>|<point|-2.83157285868568|2.05725810298086>|<point|-3.42353277778331|0.72769435601024>|<point|-3.42353277778331|-0.727694356010239>|<point|-2.83157285868569|-2.05725810298086>|<point|-1.75000826828946|-3.03110323434296>|<point|-0.365851349979971|-3.48084307977876>|<point|1.08156459039623|-3.32871353425417>|<point|2.34196818738708|-2.60101917824393>|<point|3.19742420866565|-1.4235849767979>>>
  </center>

  <\with|font-size|2>
    <space|2em><em|That's all folks!>
  </with>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|page-type|a4>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|app-markup|<tuple|A|15>>
    <associate|app-scheme|<tuple|B|15>>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-100|<tuple|2|12>>
    <associate|auto-101|<tuple|3.2|13>>
    <associate|auto-102|<tuple|3|13>>
    <associate|auto-103|<tuple|4|13>>
    <associate|auto-104|<tuple|4|13>>
    <associate|auto-105|<tuple|4|13>>
    <associate|auto-106|<tuple|4|13>>
    <associate|auto-107|<tuple|4|13>>
    <associate|auto-108|<tuple|5|13>>
    <associate|auto-109|<tuple|5|13>>
    <associate|auto-11|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-110|<tuple|5|13>>
    <associate|auto-111|<tuple|5|13>>
    <associate|auto-112|<tuple|5|13>>
    <associate|auto-113|<tuple|5|13>>
    <associate|auto-114|<tuple|5|13>>
    <associate|auto-115|<tuple|5|13>>
    <associate|auto-116|<tuple|5|13>>
    <associate|auto-117|<tuple|5|13>>
    <associate|auto-118|<tuple|5|13>>
    <associate|auto-119|<tuple|5|13>>
    <associate|auto-12|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-120|<tuple|5|13>>
    <associate|auto-121|<tuple|5|13>>
    <associate|auto-122|<tuple|5|13>>
    <associate|auto-123|<tuple|6|14>>
    <associate|auto-124|<tuple|6|14>>
    <associate|auto-125|<tuple|6|14>>
    <associate|auto-126|<tuple|6|14>>
    <associate|auto-127|<tuple|6|14>>
    <associate|auto-128|<tuple|6|14>>
    <associate|auto-129|<tuple|7|14>>
    <associate|auto-13|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-130|<tuple|7|14>>
    <associate|auto-131|<tuple|7|14>>
    <associate|auto-132|<tuple|7|14>>
    <associate|auto-133|<tuple|7|14>>
    <associate|auto-134|<tuple|8|14>>
    <associate|auto-135|<tuple|8|14>>
    <associate|auto-136|<tuple|9|14>>
    <associate|auto-137|<tuple|A|15>>
    <associate|auto-138|<tuple|A.1|15>>
    <associate|auto-139|<tuple|A.2|15>>
    <associate|auto-14|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-140|<tuple|B|15>>
    <associate|auto-141|<tuple|B|16>>
    <associate|auto-15|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-16|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-17|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-18|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-19|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-2|<tuple|1.1|1>>
    <associate|auto-20|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-21|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-22|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-23|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-24|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-25|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-26|<tuple|1.3|3>>
    <associate|auto-27|<tuple|1.3.1|3>>
    <associate|auto-28|<tuple|1.3.1|3>>
    <associate|auto-29|<tuple|1.3.1|3>>
    <associate|auto-3|<tuple|i|1>>
    <associate|auto-30|<tuple|1.3.1|3>>
    <associate|auto-31|<tuple|1.3.1|3>>
    <associate|auto-32|<tuple|1.3.1|3>>
    <associate|auto-33|<tuple|1.3.1|3>>
    <associate|auto-34|<tuple|1.3.1|3>>
    <associate|auto-35|<tuple|1.3.2|4>>
    <associate|auto-36|<tuple|1.3.2|4>>
    <associate|auto-37|<tuple|1.3.3|6>>
    <associate|auto-38|<tuple|1.3.3|6>>
    <associate|auto-39|<tuple|1.3.3|6>>
    <associate|auto-4|<tuple|1.2|2>>
    <associate|auto-40|<tuple|1.3.3|6>>
    <associate|auto-41|<tuple|1.3.3|6>>
    <associate|auto-42|<tuple|1.3.3|7>>
    <associate|auto-43|<tuple|1.3.4|7>>
    <associate|auto-44|<tuple|1.3.4|7>>
    <associate|auto-45|<tuple|1.3.4|7>>
    <associate|auto-46|<tuple|1.3.4|7>>
    <associate|auto-47|<tuple|1.3.5|7>>
    <associate|auto-48|<tuple|1.3.5|7>>
    <associate|auto-49|<tuple|1.3.5|7>>
    <associate|auto-5|<tuple|1.2|2>>
    <associate|auto-50|<tuple|1.3.5|7>>
    <associate|auto-51|<tuple|1.3.6|8>>
    <associate|auto-52|<tuple|1.3.6|8>>
    <associate|auto-53|<tuple|1.3.6|8>>
    <associate|auto-54|<tuple|1.3.6|8>>
    <associate|auto-55|<tuple|1.3.7|8>>
    <associate|auto-56|<tuple|1.3.8|9>>
    <associate|auto-57|<tuple|1.3.8|9>>
    <associate|auto-58|<tuple|1.3.8|9>>
    <associate|auto-59|<tuple|1.3.8|9>>
    <associate|auto-6|<tuple|1.2|2>>
    <associate|auto-60|<tuple|1.3.9|9>>
    <associate|auto-61|<tuple|1.3.9|9>>
    <associate|auto-62|<tuple|1.3.9|10>>
    <associate|auto-63|<tuple|1.3.10|10>>
    <associate|auto-64|<tuple|1.3.10|10>>
    <associate|auto-65|<tuple|1.3.11|11>>
    <associate|auto-66|<tuple|1.3.11|11>>
    <associate|auto-67|<tuple|1.3.11|11>>
    <associate|auto-68|<tuple|1.3.12|11>>
    <associate|auto-69|<tuple|1.3.12|11>>
    <associate|auto-7|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-70|<tuple|1.3.12|11>>
    <associate|auto-71|<tuple|1.3.12|11>>
    <associate|auto-72|<tuple|1.3.12|11>>
    <associate|auto-73|<tuple|2|12>>
    <associate|auto-74|<tuple|2|12>>
    <associate|auto-75|<tuple|1|12>>
    <associate|auto-76|<tuple|1|12>>
    <associate|auto-77|<tuple|1|12>>
    <associate|auto-78|<tuple|1|12>>
    <associate|auto-79|<tuple|1|12>>
    <associate|auto-8|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-80|<tuple|1|12>>
    <associate|auto-81|<tuple|1|12>>
    <associate|auto-82|<tuple|1|12>>
    <associate|auto-83|<tuple|1|12>>
    <associate|auto-84|<tuple|1|12>>
    <associate|auto-85|<tuple|1|12>>
    <associate|auto-86|<tuple|1|12>>
    <associate|auto-87|<tuple|1|12>>
    <associate|auto-88|<tuple|1|12>>
    <associate|auto-89|<tuple|3|12>>
    <associate|auto-9|<tuple|<with|mode|<quote|math>|\<bullet\>>|2>>
    <associate|auto-90|<tuple|3.1|12>>
    <associate|auto-91|<tuple|2|12>>
    <associate|auto-92|<tuple|2|12>>
    <associate|auto-93|<tuple|2|12>>
    <associate|auto-94|<tuple|2|12>>
    <associate|auto-95|<tuple|2|12>>
    <associate|auto-96|<tuple|2|12>>
    <associate|auto-97|<tuple|2|12>>
    <associate|auto-98|<tuple|2|12>>
    <associate|auto-99|<tuple|2|12>>
    <associate|sec-curves|<tuple|3|12>>
    <associate|sec-example|<tuple|1.3|3>>
    <associate|sec-grids|<tuple|2|12>>
    <associate|sec-intro|<tuple|1.1|1>>
    <associate|sec-misc|<tuple|8|14>>
    <associate|sec-props|<tuple|5|13>>
    <associate|sec-select|<tuple|6|14>>
    <associate|sec-since|<tuple|9|14>>
    <associate|sec-text|<tuple|4|13>>
    <associate|sec-transform|<tuple|7|14>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Snap>>|<pageref|auto-3>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>>|<pageref|auto-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>>|<pageref|auto-6>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Geometry>>|<pageref|auto-7>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>>|<pageref|auto-8>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>>|<pageref|auto-9>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Point>>|<pageref|auto-10>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Line>>|<pageref|auto-11>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Polygon>>|<pageref|auto-12>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Curve>>|<pageref|auto-13>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Closed
      curve>>|<pageref|auto-14>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Arc>>|<pageref|auto-15>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Circle>>|<pageref|auto-16>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Text>>|<pageref|auto-17>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Mathematics>>|<pageref|auto-18>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Long
      text>>|<pageref|auto-19>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Set
      properties>>|<pageref|auto-20>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Move
      objects>>|<pageref|auto-21>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Resize
      objects>>|<pageref|auto-22>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Rotate
      objects>>|<pageref|auto-23>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Group/ungroup>>|<pageref|auto-24>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Animate
      objects>>|<pageref|auto-25>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Image>|<with|font-family|<quote|ss>|Draw
      image>>|<pageref|auto-28>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Type>|<with|font-family|<quote|ss>|Polar>>|<pageref|auto-29>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Unit
      length>|<with|font-family|<quote|ss>|2>>|<pageref|auto-30>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Number
      of polar steps>|<with|font-family|<quote|ss>|6>>|<pageref|auto-31>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Number
      of subunit steps>|<with|font-family|<quote|ss>|2>>|<pageref|auto-32>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Show
      subunits>>|<pageref|auto-33>>

      <tuple|<tuple|<with|font-family|<quote|ss>|View>|<with|font-family|<quote|ss>|Zoom>>|<pageref|auto-34>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Circle>>|<pageref|auto-36>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Set
      properties>>|<pageref|auto-38>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>>|<pageref|auto-39>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Color>|<with|font-family|<quote|ss>|Blue>>|<pageref|auto-40>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Line
      width>|<with|font-family|<quote|ss>|2 ln>>|<pageref|auto-41>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>>|<pageref|auto-42>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Move
      objects>>|<pageref|auto-44>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Edit>|<with|font-family|<quote|ss>|Copy>>|<pageref|auto-45>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Edit>|<with|font-family|<quote|ss>|Paste>>|<pageref|auto-46>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Move
      objects>>|<pageref|auto-48>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Set
      properties>>|<pageref|auto-49>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Color>>|<pageref|auto-50>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Text>>|<pageref|auto-52>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Format>|<with|font-family|<quote|ss>|Series>|<with|font-family|<quote|ss>|Bold>>|<pageref|auto-53>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Format>|<with|font-family|<quote|ss>|Size>|<with|font-family|<quote|ss>|24>>|<pageref|auto-54>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Set
      properties>>|<pageref|auto-57>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Horizontal
      alignment>>|<pageref|auto-58>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Vertical
      alignment>>|<pageref|auto-59>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Text>>|<pageref|auto-61>>

      <tuple|<tuple|<with|font-family|<quote|ss>|View>|<with|font-family|<quote|ss>|Zoom>>|<pageref|auto-62>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Group/ungroup>>|<pageref|auto-64>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Resize
      objects>>|<pageref|auto-66>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Rotate
      objects>>|<pageref|auto-67>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Geometry>|<with|font-family|<quote|ss>|Size>>|<pageref|auto-69>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Geometry>|<with|font-family|<quote|ss>|Resize>>|<pageref|auto-70>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Geometry>|<with|font-family|<quote|ss>|Crop>|<with|font-family|<quote|ss>|Crop>>|<pageref|auto-71>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Geometry>|<with|font-family|<quote|ss>|Alignment>>|<pageref|auto-72>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>>|<pageref|auto-74>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Show
      grid>>|<pageref|auto-75>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Type>>|<pageref|auto-76>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Center>>|<pageref|auto-77>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Unit
      length>>|<pageref|auto-78>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Number
      of polar steps>>|<pageref|auto-79>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Logarithmic
      base>>|<pageref|auto-80>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Color
      of the axes>>|<pageref|auto-81>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Color
      of the units>>|<pageref|auto-82>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Show
      subunits>>|<pageref|auto-83>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Color
      of the subunits>>|<pageref|auto-84>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Grid>|<with|font-family|<quote|ss>|Number
      of subunit steps>>|<pageref|auto-85>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Snap>>|<pageref|auto-86>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Snap>>|<pageref|auto-88>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Point>>|<pageref|auto-91>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Line>>|<pageref|auto-92>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Polygon>>|<pageref|auto-93>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Curve>>|<pageref|auto-94>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Closed
      curve>>|<pageref|auto-95>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Arc>>|<pageref|auto-96>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Circle>>|<pageref|auto-97>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Penscript>>|<pageref|auto-98>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Calligraphy>>|<pageref|auto-99>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Text>>|<pageref|auto-104>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Mathematics>>|<pageref|auto-105>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Long
      text>>|<pageref|auto-106>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Set
      properties>>|<pageref|auto-109>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Color>>|<pageref|auto-110>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Fill
      color>>|<pageref|auto-111>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Opacity>>|<pageref|auto-112>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Point
      style>>|<pageref|auto-113>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Point
      size>>|<pageref|auto-114>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Line
      width>>|<pageref|auto-115>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Line
      dashes>>|<pageref|auto-116>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Line
      arrows>>|<pageref|auto-117>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Horizontal
      alignment>>|<pageref|auto-118>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>|<with|font-family|<quote|ss>|Vertical
      alignment>>|<pageref|auto-119>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Set
      properties>>|<pageref|auto-120>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Focus>>|<pageref|auto-122>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Group/ungroup>>|<pageref|auto-124>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Edit>|<with|font-family|<quote|ss>|Copy>>|<pageref|auto-125>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Edit>|<with|font-family|<quote|ss>|Cut>>|<pageref|auto-126>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Edit>|<with|font-family|<quote|ss>|Paste>>|<pageref|auto-127>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Move
      objects>>|<pageref|auto-130>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Resize
      objects>>|<pageref|auto-131>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Rotate
      objects>>|<pageref|auto-132>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Session>|<with|font-family|<quote|ss>|Scheme>>|<pageref|auto-141>>
    </associate>
    <\associate|table>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        The grid.
      </surround>|<pageref|auto-87>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|2>|>
        The insertion modes for curves.
      </surround>|<pageref|auto-100>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|3>|>
        Editing curves.
      </surround>|<pageref|auto-102>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|4>|>
        Text boxes.
      </surround>|<pageref|auto-107>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|5>|>
        Properties.
      </surround>|<pageref|auto-121>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|6>|>
        Selecting and grouping.
      </surround>|<pageref|auto-128>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|7>|>
        Transforming objects.
      </surround>|<pageref|auto-133>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|8>|>
        Keys acting on the whole picture.
      </surround>|<pageref|auto-135>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Introduction>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|1tab>|1.1<space|2spc>Overview
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2>>

      <with|par-left|<quote|1tab>|1.2<space|2spc>The user interface
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <with|par-left|<quote|1tab>|1.3<space|2spc>An introductory example
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-26>>

      <with|par-left|<quote|2tab>|1.3.1<space|2spc>Insert a new picture and
      set its grid <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-27>>

      <with|par-left|<quote|2tab>|1.3.2<space|2spc>Insert a first circle
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-35>>

      <with|par-left|<quote|2tab>|1.3.3<space|2spc>Set the properties of this
      first circle <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-37>>

      <with|par-left|<quote|2tab>|1.3.4<space|2spc>Make three copies of this
      circle <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-43>>

      <with|par-left|<quote|2tab>|1.3.5<space|2spc>Set the position and the
      color of each of the circles <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-47>>

      <with|par-left|<quote|2tab>|1.3.6<space|2spc>Add a text box
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-51>>

      <with|par-left|<quote|2tab>|1.3.7<space|2spc>Make three copies of this
      text box <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-55>>

      <with|par-left|<quote|2tab>|1.3.8<space|2spc>Change the alignment of
      the text boxes <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-56>>

      <with|par-left|<quote|2tab>|1.3.9<space|2spc>Move the text boxes to
      their intended positions <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-60>>

      <with|par-left|<quote|2tab>|1.3.10<space|2spc>Select all, group it
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-63>>

      <with|par-left|<quote|2tab>|1.3.11<space|2spc>Resize and rotate the
      whole <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-65>>

      <with|par-left|<quote|2tab>|1.3.12<space|2spc>Change the geometry of
      the picture <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-68>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Grids>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-73><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Curves>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-89><vspace|0.5fn>

      <with|par-left|<quote|1tab>|3.1<space|2spc>Insertion modes
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-90>>

      <with|par-left|<quote|1tab>|3.2<space|2spc>Operations
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-101>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>Text
      boxes> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-103><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|5<space|2spc>Properties
      of the objects> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-108><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|6<space|2spc>Selecting
      and grouping the objects> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-123><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|7<space|2spc>Transforming
      the objects> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-129><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|8<space|2spc>Miscellaneous>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-134><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|9<space|2spc>Since
      2005> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-136><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Appendix
      A<space|2spc>Overview of the graphical markup>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-137><vspace|0.5fn>

      <with|par-left|<quote|1tab>|A.1<space|2spc>Graphical objects
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-138>>

      <with|par-left|<quote|1tab>|A.2<space|2spc>Graphical properties
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-139>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Appendix
      B<space|2spc>Graphical output using
      <with|font-shape|<quote|small-caps>|Scheme>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-140><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>