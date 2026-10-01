<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Graphical markup and its typesetting>

  <section|The data model>

  <subsection|Graphical markup>

  A picture is a <markup|graphics> tag. Its children are drawn in order, so
  that later children cover earlier ones (as in <abbr|SVG>; see the comment
  at the top of <verbatim|Typeset/Concat/concat_graphics.cpp>). A freshly
  inserted picture, as created by <scm|make-graphics> in
  <verbatim|graphics/graphics-utils.scm>, looks like this:

  <\scm-code>
    (with "gr-mode" "point"

    \ \ \ \ \ \ "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))

    \ \ \ \ \ \ "gr-geometry" (tuple "geometry" "1par" "0.6par")

    \ \ (graphics ""))
  </scm-code>

  After drawing a red line and a label it might become

  <\scm-code>
    (with "gr-mode" (tuple "edit" "line") "gr-color" "red" ...

    \ \ (graphics ""

    \ \ \ \ (with "color" "red"

    \ \ \ \ \ \ (line (point "-2" "0") (point "1.5" "0.5")))

    \ \ \ \ (with "text-at-halign" "center"

    \ \ \ \ \ \ (text-at "A" (point "0" "1")))))
  </scm-code>

  The children of a <markup|graphics> are the following kinds of trees.
  Their <abbr|DRD> properties are declared in
  <verbatim|Data/Drd/drd_std.cpp>, their semantics is described in
  <hlink|graphics primitives|../format/regular/prim-graphics.en.tm>.

  <\description>
    <item*|Points>A <markup|point> with two (or three) coordinates. The
    coordinates are numbers in the units of the current frame, but lengths
    such as <verbatim|1cm> are also accepted: in that case
    <cpp|edit_env_rep::as_point> (<verbatim|Typeset/Env/env_length.cpp>)
    converts them through the inverse frame.

    <item*|Curves><markup|line>, <markup|cline>, <markup|spline>,
    <markup|cspline>, <markup|bezier>, <markup|cbezier>, <markup|smooth>,
    <markup|csmooth>, <markup|arc> and <markup|carc>. Their arguments are
    points; the <verbatim|c> variants are closed. <markup|arc> and
    <markup|carc> have exactly three points.

    <item*|Text><markup|text-at>, <markup|math-at> and
    <markup|document-at>, whose first argument is the (editable) content and
    whose second argument is the anchor point.

    <item*|Hand drawings><markup|penscript> and <markup|calligraphy>, with
    four arguments: start point, end point, an <markup|ink-meta> tag
    (identifier and pixel size at creation time) and a <markup|tuple> of
    samples <verbatim|(tuple x y time pressure)>.

    <item*|Grouping and transformations><markup|gr-group> (a group of
    objects which behaves as a single object), <markup|superpose>,
    <markup|gr-transform> and <markup|gr-effect>.

    <item*|Attributes><markup|with> tags around an object, setting
    variables like <src-var|color>, <src-var|line-width>,
    <src-var|fill-color>, <src-var|point-style>, <src-var|arrow-end>,
    <src-var|text-at-halign> or <src-var|magnify>.

    <item*|Constraints and identifiers>The variable <src-var|gid> gives an
    object a graphical identifier; constraint tags like <markup|is-equal>
    relate identified objects (see below).

    <item*|Macros>Any user macro which expands to graphics, for instance
    <markup|rectangle>, <markup|circle> or <markup|arrow-with-text> from
    <verbatim|packages/standard/std-graphics.ts>.
  </description>

  An empty string child (the <scm|""> in the examples above) is ignored by
  the typesetter (<cpp|typeset_graphical> skips atomic children); it gives
  the cursor a place to stand in an empty picture.

  <subsection|Two families of environment variables>

  There are two sets of graphical variables, with a different purpose. This
  is the single most important point to understand about the data model.

  <\itemize>
    <item>The <em|typesetting> variables, like <src-var|color>,
    <src-var|line-width>, <src-var|point-style>, <src-var|fill-color> or
    <src-var|text-at-halign>, are read by the typesetter when it draws an
    object. They are set on individual objects by <markup|with> tags. Their
    built-in defaults are in <cpp|initialize_default_env>
    (<verbatim|Typeset/Env/env_default.cpp>).

    <item>The <verbatim|gr-> prefixed variables, like <src-var|gr-color>,
    <src-var|gr-line-width> or <src-var|gr-point-style>, are <em|not>
    used by the typesetter. They store the properties which the editor
    gives to <em|new> objects, and they live in a <markup|with> around the
    <markup|graphics> tag. Their default value is <verbatim|default>. The
    menus and toolbars modify them through <scm|graphics-set-property>,
    and the editor copies them onto new objects with <scm|graphics-enrich>.
  </itemize>

  A few <verbatim|gr-> variables do concern the picture as a whole and are
  read by the typesetter: <src-var|gr-geometry> (size and vertical
  alignment), <src-var|gr-frame> (coordinate system),
  <src-var|gr-grid>, <src-var|gr-grid-aspect>, <src-var|gr-edit-grid>,
  <src-var|gr-edit-grid-aspect>, <src-var|gr-auto-crop>,
  <src-var|gr-crop-padding> and <src-var|gr-transformation> (the 3D
  view). Other ones only matter to the editor: <src-var|gr-mode> (the
  current editing mode), <src-var|gr-snap-distance> and <src-var|gr-snap>
  (snapping parameters; the latter is not a built-in variable and is only
  known to <scheme>).

  On the <scheme> side, the list of per-object attributes, their defaults
  and the attributes which make sense for each tag are in
  <verbatim|graphics/graphics-drd.scm>:

  <\explain>
    <scm|(graphics-all-attributes)><explain-synopsis|all attribute names>
  <|explain>
    The keys of <scm|attribute-default-table>: <verbatim|gid>,
    <verbatim|anim-id>, <verbatim|proviso>, <verbatim|magnify>,
    <verbatim|color>, <verbatim|opacity>, the point, line, dash, arrow, fill,
    <verbatim|text-at-*>, <verbatim|doc-at-*> and pen attributes, without
    <verbatim|gr-> prefix.
  </explain>

  <\explain>
    <scm|(graphics-attributes <scm-arg|tag>)><explain-synopsis|attributes
    relevant for a tag>
  <|explain>
    Overloaded with <scm|:require> clauses: points get the point attributes,
    curves (and user defined graphical macros) the line, dash, arrow and
    fill attributes, text tags the alignment attributes, and so on. Only
    these attributes are copied to a new object of this kind.
  </explain>

  <\explain>
    <scm|(graphics-attribute-default <scm-arg|attr>)><explain-synopsis|default
    value of an attribute>
  <|explain>
    Looks up <scm-arg|attr> (with or without <verbatim|gr-> prefix) in
    <scm|attribute-default-table>. <scm|graphics-set-property> removes a
    <verbatim|gr-> property instead of setting it when the new value equals
    this default.
  </explain>

  The tag groups (<scm|define-group>) in the same file are used throughout
  the editor: <scm|graphical-curve-tag> (with
  <scm|graphical-open-curve-tag> and <scm|graphical-closed-curve-tag>),
  <scm|graphical-text-tag> (<markup|text-at>, <markup|math-at>,
  <markup|document-at>), <scm|graphical-pen-tag>,
  <scm|graphical-group-tag> (<markup|gr-group>) and
  <scm|graphical-over-under-tag> (<markup|draw-over>,
  <markup|draw-under>). The lists <scm|gr-tags-all>, <scm|gr-tags-curves>,
  <scm|gr-tags-noncurves> and <scm|gr-tags-user> are derived from them; the
  last one is filled by <scm|define-graphics> (see the <hlink|chapter on
  extensions|graphics-editor-extend.en.tm>).

  <section|Coordinates>

  <subsection|Frames>

  A <cpp|point> is just an <cpp|array\<less\>double\<gtr\>>
  (<verbatim|Graphics/Types/point.hpp>); the empty array is used as an
  invalid point, so many routines test <cpp|N(p) == 0>. A <cpp|frame>
  (<verbatim|Graphics/Types/frame.hpp>) is an abstract invertible map
  between two coordinate systems. Its operators are

  <\cpp-code>
    point frame::operator () (point p); \ // direct: local -\<gtr\> parent

    point frame::operator [] (point p); \ // inverse: parent -\<gtr\> local
  </cpp-code>

  and the same operators are defined on rectangles and on curves (the
  transformation of a curve is again a curve). Concrete frames are
  constructed by <cpp|scaling>, <cpp|shift_2D>, <cpp|rotation_2D>,
  <cpp|slanting>, <cpp|linear_2D> and <cpp|affine_2D>; they can be composed
  with <cpp|operator *> and inverted with <cpp|invert>. A frame also knows
  its Jacobian and bounds which are used when computing intersections of
  transformed curves.

  When the typesetter enters a <markup|graphics>, the environment field
  <cpp|edit_env_rep::fr> holds the frame which maps <em|graphical
  coordinates> (the numbers stored in <markup|point> tags) to typesetter
  coordinates (<cpp|SI> units relative to the origin of the box being
  built). It is recomputed by <cpp|edit_env_rep::update_frame>
  (<verbatim|Typeset/Env/env_semantics.cpp>) whenever <src-var|gr-frame> or
  <src-var|gr-geometry> changes:

  <\itemize>
    <item><src-var|gr-geometry> has the form
    <verbatim|(tuple "geometry" w h [valign])>; <cpp|update_geometry> stores
    the width and height in <cpp|gw> and <cpp|gh> (these define the length
    units <verbatim|gw> and <verbatim|gh>) and the alignment in
    <cpp|gvalign>.

    <item><src-var|gr-frame> has the form
    <verbatim|(tuple "scale" unit (tuple x y))>, for instance
    <verbatim|(tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))>: one graphical
    unit is one centimeter, and the origin is at the center of the picture.
    The resulting frame is <cpp|scaling (magn, point (x, y + yinc))>, where
    <cpp|yinc> depends on the vertical alignment. Without a valid
    <src-var|gr-frame>, a default of one centimeter with origin at
    <verbatim|(0.5par, 1yfrac)> is used.

    <item>The visible rectangle of the picture, in graphical coordinates, is
    stored in <cpp|clip_lim1> and <cpp|clip_lim2>.
  </itemize>

  Because the unit and the origin are lengths, zooming a picture
  (<scm|graphics-zoom> in <verbatim|graphics/graphics-main.scm>) is
  implemented by rewriting <src-var|gr-frame> and the
  <src-var|magnify> property of the picture, and scrolling
  (<scm|graphics-move-origin>) by rewriting the origin. The objects
  themselves are unchanged.

  The environment exposes the frame to markup through the primitives
  <markup|frame-direct> and <markup|frame-inverse>
  (<cpp|exec_frame_direct> and <cpp|exec_frame_inverse> in
  <verbatim|Typeset/Env/env_exec.cpp>); the <scheme> functions
  <scm|frame-direct> and <scm|frame-inverse> in
  <verbatim|graphics/graphics-utils.scm> evaluate these primitives at the
  cursor, and are used by the editor to compute distances in physical
  units, for instance in <scm|points-dist\<less\>>.

  <subsection|Grids>

  A <cpp|grid> (<verbatim|Graphics/Types/grid.hpp>) is an abstract
  object which produces the lines to be drawn
  (<cpp|grid_rep::get_curves>) and which can find the nearest grid point
  (<cpp|find_point_around>) and nearby grid lines
  (<cpp|get_curves_around>). The concrete grids are <cpp|empty_grid>,
  <cpp|cartesian>, <cpp|polar> and <cpp|logarithmic>. The function
  <cpp|as_grid> parses trees like <verbatim|(tuple "cartesian" center
  step)> or <verbatim|(tuple "polar" center step astep)>, and
  <cpp|grid_rep::set_aspect> applies the subdivisions and colors from a
  <verbatim|*-grid-aspect> variable.

  Each picture has two grids. The <em|visual grid> <src-var|gr-grid> is
  drawn (as the first child of the <cpp|graphics_box>) but plays no role in
  editing. The <em|edit grid> <src-var|gr-edit-grid> is invisible and is
  stored in the <cpp|graphics_box>; it is the grid used for snapping. By
  default the <scheme> code keeps the edit grid synchronized with the visual
  one (<scm|egrid-as-vgrid?>, <scm|update-edit-grid> and
  <scm|graphics-set-edit-grid> in <verbatim|graphics/graphics-main.scm>).

  <section|Typesetting>

  <subsection|The <markup|graphics> tag>

  The concater dispatches each graphical tag to a method in
  <verbatim|Typeset/Concat/concat_graphics.cpp> (see the big switch in
  <verbatim|concater.cpp>). For <markup|graphics> this is

  <\cpp-code>
    void

    concater_rep::typeset_graphics (tree t, path ip) {

    BEGIN_MAGNIFY

    \ \ env-\<gtr\>update_color ();

    \ \ env-\<gtr\>update_dash_style_unit ();

    \ \ grid gr= as_grid (env-\<gtr\>read (GR_GRID));

    \ \ array\<less\>box\<gtr\> bs;

    \ \ gr-\<gtr\>set_aspect (env-\<gtr\>read (GR_GRID_ASPECT));

    \ \ bs \<less\>\<less\> grid_box (ip, gr, env-\<gtr\>fr, env-\<gtr\>as_length ("2ln"),

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ env-\<gtr\>clip_lim1, env-\<gtr\>clip_lim2);

    \ \ ...

    \ \ typeset_graphical (bs, t, ip);

    \ \ ...

    \ \ gr= as_grid (env-\<gtr\>read (GR_EDIT_GRID));

    \ \ gr-\<gtr\>set_aspect (env-\<gtr\>read (GR_EDIT_GRID_ASPECT));

    \ \ box b= graphics_box (ip, bs, env-\<gtr\>fr, gr, lim1, lim2);

    \ \ ...

    \ \ print (b);

    \ \ notify_graphics_extents (t, lim1, lim2);

    END_MAGNIFY

    }
  </cpp-code>

  The omitted parts handle <src-var|gr-auto-crop>: when it is
  <verbatim|true>, the limits are computed from the extents of the
  typeset children, enlarged by <src-var|gr-crop-padding>, instead of
  <cpp|clip_lim1>, <cpp|clip_lim2>. The macros <cpp|BEGIN_MAGNIFY> and
  <cpp|END_MAGNIFY> multiply <src-var|magnification> by the current
  <src-var|magnify> and reset <src-var|magnify> to 1; they surround all
  graphical primitives, which is how a <src-var|magnify> attribute on an
  object or on a group scales line widths, point sizes and text.
  <cpp|notify_graphics_extents> calls the <scheme> hook
  <scm|graphics-notify-extents> (defined in
  <verbatim|dynamic/scripts-edit.scm>) for identified pictures, which is
  used by some plug-ins.

  <cpp|typeset_graphical> typesets the children:

  <\enumerate>
    <item>It first records the subtrees of all objects carrying a
    <src-var|gid> (<cpp|set_graphical_values>) and applies the constraints
    among the children (only <markup|is-equal> is implemented; the other
    constraint tags print a message).

    <item>Every non-atomic, non-constraint child is then typeset by
    <cpp|typeset_gr_item>. For a <markup|with>, this function evaluates the
    variables, returns an empty box when <src-var|proviso> is
    <verbatim|false>, and otherwise typesets the body with the variables
    temporarily set. Other children are typeset with
    <cpp|typeset_as_atomic>.
  </enumerate>

  The result of all this is a <cpp|graphics_box> whose first child is the
  <cpp|grid_box> of the visual grid.

  <subsection|Objects>

  <\description>
    <item*|<cpp|typeset_point>>Evaluates the coordinates, maps them through
    <cpp|env-\<gtr\>fr> and produces a <cpp|point_box> with the current
    <src-var|point-style>, <src-var|point-size>, pen and fill brush.

    <item*|<cpp|typeset_line>, <cpp|typeset_spline>, <cpp|typeset_arc>,
    <cpp|typeset_bezier>>Evaluate the points, build a <cpp|curve> with
    <cpp|poly_segment>, <cpp|spline>, <cpp|arc> or <cpp|poly_bezier>
    (<verbatim|Graphics/Types/curve.cpp>), transform it with
    <cpp|env-\<gtr\>fr> and produce a <cpp|curve_box>. Each curve is built
    with the array of inverse paths of its points (<cpp|cip>), which allows
    the curve box to report which source point corresponds to which control
    point. A one point curve is doubled, an arc through three collinear
    points falls back to a line, and a spline or Bezier curve with less than
    three points falls back to a polygon. Before the box is made,
    <cpp|adjust_extremities> cuts the curve where it enters the
    <cpp|white_zones>: rectangles around <markup|text-at> boxes with a
    non-negative <src-var|text-at-repulse>, so that lines do not cross
    labels. Arrow heads are obtained by typesetting the macros named by
    <src-var|arrow-begin> and <src-var|arrow-end>
    (<cpp|typeset_line_arrows>).

    <item*|<cpp|typeset_text_at>, <cpp|typeset_math_at>,
    <cpp|typeset_document_at>>Typeset the content (respectively as text, as
    <markup|math> and inside a <markup|paragraph-box>), position it with
    respect to the anchor according to <src-var|text-at-halign> and
    <src-var|text-at-valign> (or <src-var|doc-at-valign>), produce a
    <cpp|text_at_box>, and register a white zone if requested.

    <item*|<cpp|typeset_calligraphy>>Used for <markup|penscript> and
    <markup|calligraphy>. The samples are first adapted to the possibly
    modified end points, then smoothed according to <src-var|pen-enhance>
    (<cpp|refine> and <cpp|smoothen>, or <cpp|bezier_fit> and
    <cpp|rectify_bezier>, in <verbatim|Graphics/Types/curve_extras.cpp>).
    For <markup|calligraphy>, the curve is replaced by the outline of an
    oval pen (<cpp|oval_profile>, <cpp|calligraphy>), which is filled.

    <item*|<cpp|typeset_gr_group>>Typesets the children with
    <cpp|typeset_graphical> into a <cpp|graphics_group_box>.

    <item*|<cpp|typeset_gr_transform>, <cpp|typeset_gr_effect>>Wrap the
    typeset body in a <cpp|transformed_box> (the transformation is one of
    the tuples recognized by <cpp|is_transformation>: rotation, scaling,
    slanting or a linear map) or an <cpp|effect_box>.

    <item*|<cpp|typeset_graphics_3d>>Used for the experimental tags
    <markup|object-3d>, <markup|triangle-3d>, <markup|transform-3d> and
    <markup|light-3d>. The tree is converted into a <cpp|spacial>
    (<verbatim|Graphics/Spacial/spacial.hpp>), transformed by the
    composition of the current frame and the 4<math|\<times\>>4 matrix in
    <src-var|gr-transformation>, and drawn by a <cpp|spacial_box>. The
    keyboard shortcuts <key|C-left>, <key|C-right>, <key|C-up> and
    <key|C-down> rotate the view by modifying <src-var|gr-transformation>
    (<scm|graphics-rotate-xz>, <scm|graphics-rotate-yz>).
  </description>

  The tags <markup|spline*> (<cpp|typeset_var_spline>) and <markup|fill>
  (<cpp|typeset_fill>) are placeholders which only print a
  <cpp|test_box>.

  <subsection|Graphical constraints>

  The variable <src-var|gid> and the global table
  <cpp|graphical_values> implement a rudimentary constraint system. While
  typesetting a picture, every subtree <verbatim|(with "gid" id body)>
  registers <verbatim|body> under <verbatim|id>, and
  <verbatim|(is-equal id1 id2)> copies the value of <verbatim|id2> to
  <verbatim|id1>. When <cpp|edit_env_rep::as_point> evaluates
  <verbatim|(with "gid" id (point ...))> and the registered value differs,
  it calls <cpp|graphics_require_update> and still returns the <em|old>
  point. After the editor has committed an object, the <scheme> function
  <scm|graphics-update-constraints> (in
  <verbatim|graphics/graphics-single.scm>) asks
  <scm|graphics-needs-update?>, and rewrites the outdated points in the
  document using <scm|graphics-ref> and <scm|graphics-notify-update>. The
  corresponding glue functions are <scm|graphics-set>, <scm|graphics-has?>,
  <scm|graphics-ref>, <scm|graphics-needs-update?> and
  <scm|graphics-notify-update>.

  <section|Graphics boxes>

  <subsection|The box classes>

  <\description>
    <item*|<cpp|graphics_box_rep>>A composite box which stores the frame
    <cpp|f>, the edit grid <cpp|g> and the limits <cpp|lim1>, <cpp|lim2>.
    Its logical extents are the image of the limits; its ink extents are
    clipped to them, and <cpp|pre_display> installs a clipping rectangle,
    so that objects outside the picture are not drawn. It overrides
    <cpp|get_frame>, <cpp|get_grid> and <cpp|get_limits>, and its
    <cpp|find_child> prefers <markup|text-at> children so that clicking on a
    label puts the cursor into the label.

    <item*|<cpp|graphics_group_box_rep>>The box of a <markup|gr-group>. It
    is not accessible, so that the cursor cannot enter a group, and its
    <cpp|graphical_select> reports the whole group as a single selection of
    type <verbatim|group>.

    <item*|<cpp|point_box_rep>>Draws a point. The polygonal styles
    (<verbatim|square>, <verbatim|diamond>, <verbatim|triangle>,
    <verbatim|star>, <verbatim|plus>, <verbatim|cross>) are produced by
    <cpp|get_contour>; any other style is drawn as a circle, filled for
    <verbatim|disk>; the style <verbatim|none> draws nothing.

    <item*|<cpp|curve_box_rep>>Draws a rectified curve with a pencil, dash
    style and motif, an optional fill brush and arrow boxes. Its tree
    representation is the string <verbatim|curve> and that of a point box
    is <verbatim|point>; <cpp|draw_graphical_object> relies on this.

    <item*|<cpp|grid_box_rep>>Draws a grid. The grid lines are computed
    lazily at the first display (and again when the pixel size changes).
    Grid boxes are never selected.

    <item*|<cpp|text_at_box_rep>>A <cpp|move_box_rep> which remembers the
    position of its anchor (<cpp|hx>, <cpp|hy>), its axis and its snapping
    padding.
  </description>

  <subsection|Frames, grids and limits along a box path>

  The editor needs the frame of the picture which contains the cursor.
  Since boxes are positioned relative to their parent, the frame is
  obtained by walking down the box tree and composing the translations
  with the innermost frame found (<verbatim|Typeset/Boxes/Basic/boxes.cpp>):

  <\cpp-code>
    frame

    box_rep::find_frame (path bp, bool last) {

    \ \ SI \ \ \ x= 0;

    \ \ SI \ \ \ y= 0;

    \ \ box \ \ b= this;

    \ \ frame f= get_frame ();

    \ \ while (!is_nil (bp)) {

    \ \ \ \ x += b-\<gtr\>sx (bp-\<gtr\>item);

    \ \ \ \ y += b-\<gtr\>sy (bp-\<gtr\>item);

    \ \ \ \ b \ = b-\<gtr\>subbox (bp-\<gtr\>item);

    \ \ \ \ bp = bp-\<gtr\>next;

    \ \ \ \ frame g= b-\<gtr\>get_frame ();

    \ \ \ \ if (!is_nil (g)) {

    \ \ \ \ \ \ if (last)

    \ \ \ \ \ \ \ \ f= g;

    \ \ \ \ \ \ else

    \ \ \ \ \ \ \ \ f= scaling (1.0, point (x, y)) * g;

    \ \ \ \ }

    \ \ }

    \ \ return f;

    }
  </cpp-code>

  With <cpp|last= false> the result maps graphical coordinates to absolute
  document coordinates (those of the mouse); with <cpp|last= true> it maps
  them to the local coordinates of the graphics box, which are the
  coordinates of the points returned by <cpp|graphical_select>.
  <cpp|find_grid> and <cpp|find_limits> similarly return the innermost grid
  and limits.

  <subsection|Graphical selection>

  The virtual method

  <\cpp-code>
    virtual gr_selections box_rep::graphical_select (SI x, SI y, SI dist);
  </cpp-code>

  returns all the parts of a box within distance <cpp|dist> of
  <verbatim|(x, y)>, and the variant with a rectangle returns the parts
  inside the rectangle. A <cpp|gr_selection> (<verbatim|Typeset/boxes.hpp>)
  has the fields

  <\description>
    <item*|<cpp|type>>A string describing what was found (see the table
    below).

    <item*|<cpp|cp>>One or two paths in the source tree. For a control
    point, this is the path of the <markup|point> subtree; for a point on a
    curve segment, the paths of the two control points which bound the
    segment.

    <item*|<cpp|pts>>The corresponding control points.

    <item*|<cpp|p>, <cpp|dist>>The point found and its distance to the
    mouse.

    <item*|<cpp|c>>For selections lying on a curve, the curve itself; it is
    used to compute intersections when snapping.
  </description>

  <descriptive-table|<tformat|<table|<row|<cell|Type>|<cell|Produced
  by>|<cell|Meaning>>|<row|<cell|<verbatim|point>>|<cell|<cpp|point_box_rep>>|<cell|a
  point object>>|<row|<cell|<verbatim|curve-handle>>|<cell|<cpp|curve_box_rep>>|<cell|a
  control point of a curve>>|<row|<cell|<verbatim|curve-point>>|<cell|<cpp|curve_box_rep>>|<cell|a
  point on a curve segment (two paths in
  <cpp|cp>)>>|<row|<cell|<verbatim|curve>>|<cell|<cpp|curve_box_rep>>|<cell|a
  curve inside a rectangle>>|<row|<cell|<verbatim|text>>|<cell|<cpp|text_at_box_rep>>|<cell|the
  inside of a text box>>|<row|<cell|<verbatim|text-handle>>|<cell|<cpp|text_at_box_rep>>|<cell|the
  anchor point of a text box>>|<row|<cell|<verbatim|text-border>,
  <verbatim|text-border-point>>|<cell|<cpp|text_at_box_rep>>|<cell|the
  border of a text box (enlarged by <src-var|text-at-snapping>) and special
  points on it>>|<row|<cell|<verbatim|group>>|<cell|<cpp|graphics_group_box_rep>>|<cell|a
  whole group>>|<row|<cell|<verbatim|box>>|<cell|<cpp|box_rep>>|<cell|any
  other box (default implementation)>>|<row|<cell|<verbatim|grid-point>,
  <verbatim|grid-curve-point>>|<cell|<cpp|edit_graphics_rep::adjust>>|<cell|added
  by the editor when snapping to the edit grid>>>>>

  A curve box first looks for control points within the distance and only
  returns curve points if none is found. The composite
  <cpp|graphics_box_rep::graphical_select> simply collects the selections
  of all children, from the last one to the first one. Finally
  <cpp|as_tree (gr_selections)> sorts the selections by distance and
  converts them into a tuple of tuples of paths, which is what <scheme>
  receives.

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
